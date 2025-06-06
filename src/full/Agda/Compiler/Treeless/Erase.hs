{-# OPTIONS_GHC -Wunused-imports #-}

{-# LANGUAGE PatternSynonyms #-}

module Agda.Compiler.Treeless.Erase
       ( eraseTerms
       , computeErasedConstructorArgs
       , isErasable
       ) where

import Control.Arrow       ( first, second )
import Control.Monad.State ( StateT, evalStateT )

import Data.Map (Map)
import qualified Data.Map as Map

import Agda.Syntax.Common
import Agda.Syntax.Internal as I
import Agda.Syntax.Treeless
import Agda.Syntax.Literal

import Agda.TypeChecking.Substitute
import Agda.TypeChecking.Monad as I
import Agda.TypeChecking.Telescope
import Agda.TypeChecking.Datatypes
import Agda.TypeChecking.Pretty
import Agda.TypeChecking.Primitive

import Agda.Compiler.Treeless.Subst
import Agda.Compiler.Treeless.Unused

import Agda.Utils.Functor
import Agda.Utils.Lens
import Agda.Utils.Maybe
import Agda.Utils.Memo
import Agda.Utils.Monad
import Agda.Syntax.Common.Pretty (prettyShow)
import Agda.Utils.IntSet.Infinite (IntSet)
import qualified Agda.Utils.IntSet.Infinite as IntSet

import Agda.Utils.Impossible

-- | State of the eraser.
data ESt = ESt
  { _funMap  :: Map QName FunInfo
      -- ^ Memoize computed `FunInfo` for functions/constructors/... `QName`.
  , _typeMap :: Map QName TypeInfo
      -- ^ Memoize computed `TypeInfo` for data/record types `QName`.
  }

funMap :: Lens' ESt (Map QName FunInfo)
funMap f r = f (_funMap r) <&> \ a -> r { _funMap = a }

typeMap :: Lens' ESt (Map QName TypeInfo)
typeMap f r = f (_typeMap r) <&> \ a -> r { _typeMap = a }

-- | Eraser monad.
type E = StateT ESt TCM

runE :: E a -> TCM a
runE m = evalStateT m (ESt Map.empty Map.empty)

-- | Takes the name of the data/record type.
computeErasedConstructorArgs :: QName -> TCM ()
computeErasedConstructorArgs d = do
  cs <- getNotErasedConstructors d
  runE $ mapM_ getFunInfo cs

eraseTerms :: QName -> EvaluationStrategy -> TTerm -> TCM TTerm
eraseTerms q eval t = usedArguments q t *> runE (eraseTop q t)
  where
    eraseTop q t = do
      (_, h) <- getFunInfo q
      case h of
        Erasable -> pure TErased
        Empty    -> pure TErased
        _        -> erase t

    erase t = case tAppView t of

      (TCon c, vs) -> do
        (rs, h) <- getFunInfo c
        when (length rs < length vs) __IMPOSSIBLE__
        case h of
          Erasable -> pure TErased
          Empty    -> pure TErased
          _        -> tApp (TCon c) <$> zipWithM eraseRel rs vs

      (TDef f, vs) -> do
        (rs, h) <- getFunInfo f
        case h of
          Erasable -> pure TErased
          Empty    -> pure TErased
          _        -> tApp (TDef f) <$> zipWithM eraseRel (rs ++ repeat NotErasable) vs

      _ -> case t of
        TVar{}         -> pure t
        TDef{}         -> pure t
        TPrim{}        -> pure t
        TLit{}         -> pure t
        TCon{}         -> pure t
        TApp f es      -> tApp <$> erase f <*> mapM erase es
        TLam b         -> tLam <$> erase b
        TLet e b       -> do
          e <- erase e
          if isErased e
            then case b of
                   TCase 0 _ _ _ -> tLet TErased <$> erase b
                   _             -> erase $ subst 0 TErased b
            else tLet e <$> erase b
        TCase x t d bs -> do
          (d, bs) <- pruneUnreachable x (caseErased t) (caseType t) d bs
          d       <- erase d
          bs      <- mapM eraseAlt bs
          tCase x t d bs

        TUnit          -> pure t
        TSort          -> pure t
        TErased        -> pure t
        TError{}       -> pure t
        TCoerce e      -> TCoerce <$> erase e

    -- #3380: this is not safe for strict backends
    tLam TErased | eval == LazyEvaluation = TErased
    tLam t                                = TLam t

    tLet e b
      | freeIn 0 b = TLet e b
      | otherwise  = strengthen impossible b

    tApp f []                  = f
    tApp TErased _             = TErased
    tApp f _ | isUnreachable f = tUnreachable
    tApp f es                  = mkTApp f es

    tCase x t d bs
      | isErased d && all (isErased . aBody) bs = pure TErased
      | otherwise = case bs of
        [b@(TACon c _ _)] -> do
          h <- snd <$> getFunInfo c
          case h of
            NotErasable -> fallback
            Empty       -> pure TErased
            Erasable    -> erasedBody b
        _ -> fallback
      where
        noerase = pure $ TCase x t d bs

        erasedBody = \case
          TACon _ arity body ->
            (if arity == 0 then pure else erase) $
               -- might enable more erasure
            applySubst (replicate arity TErased ++# idS) body
          TALit _ body   -> pure body
          TAGuard _ body -> pure body

        fallback = case (caseErased t, bs) of
          (Erased{}, [b]) ->
            -- The case variable is erased, and there is exactly one
            -- case: use the case's body.
            erasedBody b
          (Erased{}, [])  ->
            -- The case variable is erased, and there is no case: use
            -- the default.
            pure $ if isErased d then TErased else d
          (Erased{}, _ : _ : _) ->
            -- The case variable is erased, and there are at least two
            -- cases: crash.
            __IMPOSSIBLE__
          _ ->
            -- The case variable is not erased: do not erase anything.
            noerase

    isErased t = t == TErased || isUnreachable t

    eraseRel r t | erasable r = pure TErased
                 | otherwise  = erase t

    eraseAlt = \case
      TALit l b   -> TALit l   <$> erase b
      TACon c a b -> do
        rs <- map erasable . fst <$> getFunInfo c
        let sub = foldr (\ e -> if e then (TErased :#) . wkS 1 else liftS 1) idS $ reverse rs
        TACon c a <$> erase (applySubst sub b)
      TAGuard g b -> TAGuard   <$> erase g <*> erase b

pruneUnreachable ::
  Int -> Erased -> CaseType -> TTerm -> [TAlt] -> E (TTerm, [TAlt])
pruneUnreachable x erased t d bs = case erased of
  NotErased{} -> pruneUnreachable' x erased t d bs
  Erased{}    ->
    -- If the match is on an erased argument, then the first branch
    -- should match.
    case bs of
      []    -> pruneUnreachable' x erased t d            []
      b : _ -> pruneUnreachable' x erased t tUnreachable [b]

-- | Doesn't have any type information (other than the name of the data type),
--   so we can't do better than checking if all constructors are present.
pruneUnreachable' ::
  Int -> Erased -> CaseType -> TTerm -> [TAlt] -> E (TTerm, [TAlt])
pruneUnreachable' _ erased (CTData q) d bs' = do
  -- Erased constructors are pruned iff the match is made on a
  -- non-erased argument.
  cs <- lift $
        if isErased erased
        then getConstructors q
        else getNotErasedConstructors q
  let bs | isErased erased = bs'
         | otherwise       =
           flip filter bs' $ \case
             a@TACon{} -> (aCon a) `elem` cs
             TAGuard{} -> True
             TALit{}   -> True
  let -- In the case of a match on an erased argument the value d is
      -- equal to tUnreachable, except perhaps if bs is empty. In the
      -- latter case complete is True exactly when the type has zero
      -- constructors (erased or not), in which case it makes sense to
      -- replace d with tUnreachable.
      complete = length cs == length [ b | b@TACon{} <- bs ]
      d' | complete  = tUnreachable
         | otherwise = d
  return (d', bs)
pruneUnreachable' x _ CTNat d bs =
  return $ pruneIntCase x d bs (IntSet.below 0)
pruneUnreachable' x _ CTInt d bs =
  return $ pruneIntCase x d bs IntSet.empty
pruneUnreachable' _ _ _ d bs =
  pure (d, bs)

-- These are the guards we generate for Int/Nat pattern matching
pattern Below :: Int -> Integer -> TTerm
pattern Below x n = TApp (TPrim PLt)  [TVar x, TLit (LitNat n)]

pattern Above :: Int -> Integer -> TTerm
pattern Above x n = TApp (TPrim PGeq) [TVar x, TLit (LitNat n)]

-- | Strip unreachable clauses (replace by tUnreachable for the default).
--   Fourth argument is the set of ints covered so far.
pruneIntCase :: Int -> TTerm -> [TAlt] -> IntSet -> (TTerm, [TAlt])
pruneIntCase x d bs cover = go bs cover
  where
    go [] cover
      | cover == IntSet.full = (tUnreachable, [])
      | otherwise            = (d, [])
    go (b : bs) cover =
      case b of
        TAGuard (Below y n) _ | x == y -> rec (IntSet.below n)
        TAGuard (Above y n) _ | x == y -> rec (IntSet.above n)
        TALit (LitNat n) _             -> rec (IntSet.singleton n)
        _                                -> second (b :) $ go bs cover
      where
        rec this = second addAlt $ go bs cover'
          where
            this'  = IntSet.difference this cover
            cover' = this' <> cover
            addAlt = case IntSet.toFiniteList this' of
                       Just []  -> id                                     -- unreachable case
                       Just [n] -> (TALit (LitNat n) (aBody b) :) -- possibly refined case
                       _        -> (b :)                                  -- unchanged case

data TypeInfo = Empty | Erasable | NotErasable
  deriving (Eq, Show)

sumTypeInfo :: [TypeInfo] -> TypeInfo
sumTypeInfo is = foldr plus Empty is
  where
    plus Empty       r           = r
    plus r           Empty       = r
    plus Erasable    r           = r
    plus r           Erasable    = r
    plus NotErasable NotErasable = NotErasable

erasable :: TypeInfo -> Bool
erasable Erasable    = True
erasable Empty       = True
erasable NotErasable = False

type FunInfo = ([TypeInfo], TypeInfo)

getFunInfo :: QName -> E FunInfo
getFunInfo q = memo (funMap . key q) $ getInfo q
  where
    getInfo :: QName -> E FunInfo
    getInfo q = do
      (rs, t) <- do
        (tel, t) <- lift $ typeWithoutParams q
        is     <- mapM (getTypeInfo . snd . dget) tel
        used   <- lift $ (++ repeat ArgUsed) . fromMaybe [] <$> getCompiledArgUse q
        forced <- lift $ (++ repeat NotForced) <$> getForcedArgs q
        return (zipWith3 (uncurry . mkR . getModality) tel (zip forced used) is, t)
      h <- if isAbsurdLambdaName q then pure Erasable else getTypeInfo t
      lift $ reportSLn "treeless.opt.erase.info" 50 $ "type info for " ++ prettyShow q ++ ": " ++ show rs ++ " -> " ++ show h
      lift $ setErasedConArgs q $ map erasable rs
      return (rs, h)

    -- Treat empty, erasable, or unused arguments as Erasable
    mkR :: Modality -> IsForced -> ArgUsage -> TypeInfo -> TypeInfo
    mkR m f u i
      | not (usableModality m) = Erasable
      | ArgUnused <- u         = Erasable
      | Forced <- f            = Erasable
      | otherwise              = i

isErasable :: QName -> TCM Bool
isErasable qn =
  erasable . snd <$> runE (getFunInfo qn)

telListView :: Type -> TCM (ListTel, Type)
telListView t = do
  TelV tel t <- telViewPath t
  return (telToList tel, t)

typeWithoutParams :: QName -> TCM (ListTel, Type)
typeWithoutParams q = do
  def <- getConstInfo q
  let d = case I.theDef def of
        Function{ funProjection = Right Projection{ projIndex = i } } -> i - 1
        Constructor{ conPars = n } -> n
        _                          -> 0
  first (drop d) <$> telListView (defType def)

getTypeInfo :: Type -> E TypeInfo
getTypeInfo t0 = do
  (tel, t) <- lift $ telListView t0
  et <- case I.unEl t of
    I.Def d _ -> do
      -- #2916: Only update the memo table for d. Results for other types are
      -- under the assumption that d is erasable!
      oldMap <- use typeMap
      dInfo <- typeInfo d
      typeMap .= Map.insert d dInfo oldMap
      return dInfo
    Sort{}    -> return Erasable
    _         -> return NotErasable
  is <- mapM (getTypeInfo . snd . dget) tel
  let e | Empty `elem` is = Erasable
        | null is         = et        -- TODO: guard should really be "all inhabited is"
        | et == Empty     = Erasable
        | otherwise       = et
  lift $ reportSDoc "treeless.opt.erase.type" 50 $ prettyTCM t0 <+> text ("is " ++ show e)
  return e
  where
  typeInfo :: QName -> E TypeInfo
  typeInfo q = ifM (erasureForbidden q) (return NotErasable) $ {-else-} do
    memoRec (typeMap . key q) Erasable $ do  -- assume recursive occurrences are erasable
      msizes <- lift $ mapM getBuiltinName
                         [builtinSize, builtinSizeLt]
      def    <- lift $ getConstInfo q
      let mcs = case I.theDef def of
                  I.Datatype{ dataCons = cs } -> Just cs
                  I.Record{ recConHead = c }  -> Just [conName c]
                  _                           -> Nothing
      case mcs of
        _ | Just q `elem` msizes -> return Erasable
        Just [c] -> do
          (ts, _) <- lift $ typeWithoutParams c
          let rs = map getModality ts
          is <- mapM (getTypeInfo . snd . dget) ts
          let er = and [ erasable i || not (usableModality r) | (i, r) <- zip is rs ]
          return $ if er then Erasable else NotErasable
        Just []      -> return Empty
        Just (_:_:_) -> return NotErasable
        Nothing ->
          case I.theDef def of
            I.Function{ funClauses = cs } ->
              sumTypeInfo <$> mapM (maybe (return Empty) (getTypeInfo . El __DUMMY_SORT__) . clauseBody) cs
            _ -> return NotErasable
  -- The backend also has a say whether a type is eraseable or not.
  erasureForbidden :: QName -> E Bool
  erasureForbidden q = lift $ not <$> activeBackendMayEraseType q
