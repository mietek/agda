module Agda.Compiler.JS.Substitution where

import Prelude hiding ( map, lookup )
import Data.Map ( empty, unionWith, singleton, findWithDefault )
import qualified Data.Map as Map
import Data.List ( genericIndex )
import qualified Data.List as List

import Agda.Syntax.Common ( Nat )
import Agda.Compiler.JS.Syntax
  ( Exp(Self,Undefined,Local,Lambda,Object,Array,Apply,Lookup,If,BinOp,PreOp),
    MemberId, LocalId(LocalId) )
import Agda.Utils.Function ( iterate' )

-- Map for expressions

map :: Nat -> (Nat -> LocalId -> Exp) -> Exp -> Exp
map m f (Local i)       = f m i
map m f (Lambda i e)    = Lambda i (map (m + i) f e)
map m f (Object o)      = Object (Map.map (map m f) o)
map m f (Array es)      = Array (List.map (\(c, e) -> (c, map m f e)) es)
map m f (Apply e es)    = Apply (map m f e) (List.map (map m f) es)
map m f (Lookup e l)    = Lookup (map m f e) l
map m f (If e e' e'')   = If (map m f e) (map m f e') (map m f e'')
map m f (PreOp op e)    = PreOp op (map m f e)
map m f (BinOp e op e') = BinOp (map m f e) op (map m f e')
map m f e               = e

-- Shifting

shift :: Nat -> Exp -> Exp
shift = shiftFrom 0

shiftFrom :: Nat -> Nat -> Exp -> Exp
shiftFrom m 0 e = e
shiftFrom m n e = map m (shifter n) e

shifter :: Nat -> Nat -> LocalId -> Exp
shifter n m (LocalId i) | i < m     = Local (LocalId i)
shifter n m (LocalId i) | otherwise = Local (LocalId (i + n))

-- Substitution

subst :: Nat -> [Exp] -> Exp -> Exp
subst 0 es e = e
subst n es e = map 0 (substituter n es) e

substituter :: Nat -> [Exp] -> Nat -> LocalId -> Exp
substituter n es m (LocalId i) | i < m       = Local (LocalId i)
substituter n es m (LocalId i) | (i - m) < n = shift m (genericIndex (es ++ repeat Undefined) (n - (i + 1 - m)))
substituter n es m (LocalId i) | otherwise   = Local (LocalId (i - n))

-- A variant on substitution which performs beta-reduction

map' :: Nat -> (Nat -> LocalId -> Exp) -> Exp -> Exp
map' m f (Local i)       = f m i
map' m f (Lambda i e)    = Lambda i (map' (m + i) f e)
map' m f (Object o)      = Object (Map.map (map' m f) o)
map' m f (Array es)      = Array (List.map (\(c, e) -> (c, map' m f e)) es)
map' m f (Apply e es)    = apply (map' m f e) (List.map (map' m f) es)
map' m f (Lookup e l)    = lookup (map' m f e) l
map' m f (If e e' e'')   = If (map' m f e) (map' m f e') (map' m f e'')
map' m f (PreOp op e)    = PreOp op (map' m f e)
map' m f (BinOp e op e') = BinOp (map' m f e) op (map' m f e')
map' m f e               = e

subst' :: Nat -> [Exp] -> Exp -> Exp
subst' 0 es e = e
subst' n es e = map' 0 (substituter n es) e

-- Beta-reducing application and field access

apply :: Exp -> [Exp] -> Exp
apply (Lambda i e) es = subst' i es e
apply e            es = Apply e es

lookup :: Exp -> MemberId -> Exp
lookup (Object o) l = findWithDefault Undefined l o
lookup e          l = Lookup e l

-- Some helper functions

curriedApply :: Exp -> [Exp] -> Exp
curriedApply = foldl (\ f e -> apply f [e])

curriedLambda :: Nat -> Exp -> Exp
curriedLambda n = iterate' n (Lambda 1)

emp :: Exp
emp = Object (empty)
