+++
title = "Proof/Haskell"
description = ""
date = 2010-02-18T20:55:28Z
aliases = []
[extra]
id = 6085
[taxonomies]
categories = []
tags = []
+++

We implement a minimal theorem-verifying library with just the features we need for this proof. By carefully choosing what to export, we ensure that any proof the user constructs is valid, so long as it's accepted by the compiler's type-checker and our <code>verify</code> function. (Exception: there is no protection against the user assuming contradictory hypotheses.)


```haskell
module Proof

   (Proof, verify,
    IntExpr(Sum, Product, Var, Constant),
    Divides, Equals, TStatement(..), OStatement,
    (.==), (.|), (.+), flipEq,
    transitiveEquality, equalTerms, containsFactor, multDiv, 
    factorFrom, substDividend)

where

import Data.List
import Control.Monad
import Text.Printf

-- Some convenience functions.

failf :: (Monad m, Show a) => String -> a -> m z
failf s a = fail $ printf s (show a)

failf2 :: (Monad m, Show a, Show b) => String -> a -> b -> m z
failf2 s a b = fail $ printf s (show a) (show b)

{- The Proof monad is like Maybe except that its equivalent of
Nothing (namely Bogus) retains the error string given to 'fail'. -}

data Proof a = Valid a | Bogus String

instance Monad Proof where
    Valid a >>= f = f a
    Bogus s >>= _ = Bogus s
    return = Valid
    fail = Bogus

data IntExpr =
    Sum [IntExpr] | Product [IntExpr] | Quotient IntExpr IntExpr |
    Var String | Constant Int deriving (Eq, Show)
{- All these constructors are exported except Quotient,
since the quotient of two integers isn't necessarily an integer. -}

data Equals = IntExpr :== IntExpr deriving (Show, Eq)
{- a :== b represents the fact that a is numerically equal to
b, although it probably isn't the case that a == b according
to IntExpr's instance of Eq. -}

data Divides = IntExpr :| IntExpr deriving (Show, Eq)
{- a :| b represents the fact that a divides b. -}

{- (:==) and (:|) aren't exported. If they were, the user
could make unwarranted assumptions in the middle of a proof.
Instead, the user expresses claims and hypotheses as OStatements,
"opaque statements", which can be used to set up a proof for
digestion by 'verify' but can't be used directly. 'verify' turns
OStatements into TStatements, "transparent statements", whose
constructors are exported. -}
 
data OStatement = OS_Equals Equals | OS_Divides Divides

data TStatement = TS_Equals Equals | TS_Divides Divides deriving (Show, Eq)

(.==) :: IntExpr -> IntExpr -> OStatement
a .== b = OS_Equals $ a :== b

(.|) :: IntExpr -> IntExpr -> OStatement
a .| b = OS_Divides $ a :| b
 
clarify :: OStatement -> TStatement
clarify (OS_Equals  e) = TS_Equals  e
clarify (OS_Divides d) = TS_Divides d

verify :: ([TStatement] -> Proof TStatement)
       -> [OStatement] -> OStatement -> Maybe String
{- Returns Nothing if the proof works,
Just "description of the error" otherwise. -}
verify proof hypotheses claim =
    case proof $ map clarify hypotheses of
        Bogus err        -> Just err
        Valid conclusion -> if conclusion == clarify claim
            then Nothing
            else Just $ printf "verify: Proof concluded %s, not %s" (show conclusion) (show $ clarify claim)

(.+) :: IntExpr -> IntExpr -> IntExpr
{- Some syntactic sugar for the user. -}
a .+ b = Sum [a, b]

subst :: IntExpr -> Equals -> Proof IntExpr
{- A convenience function used internally. -}
subst a (a' :== b) | a == a'   = return b
                   | otherwise = failf2 "subst: %s not the same as %s" a a'

{- All the remaining functions are rules of inference, which allow
the user to construct proofs. Rules are in the Proof monad if
their legality depends on the values of their arguments. -}

flipEq :: Equals -> Equals
{- Equality is reflexive. -}
flipEq (a :== b) = b :== a

transitiveEquality :: [Equals] -> Proof Equals
{- Equality is transitive. -}
transitiveEquality []               = fail "transitiveEquality: empty list"
transitiveEquality ((a :== b) : es) = liftM (a :==) $ foldM subst b es

equalTerms :: IntExpr -> [Equals] -> Proof Equals
{- If   a = A, b = B, ..., z = Z,
   then a + b + ... + z = A + B + ... + Z. -}
equalTerms sum@(Sum l) eqs
    | length l == length eqs = zipWithM subst l eqs >>= return . (sum :==) . Sum
    | otherwise              = failf2 "equalTerms: %s and %s have different lengths" l eqs

containsFactor :: IntExpr -> IntExpr -> Proof Divides
{- If   p == a*b,
   then a | p. -}
expr@(Product l) `containsFactor` x
    | x `elem` l = return $ x :| expr
    | otherwise  = failf2 "containsFactor: %s not found in %s" x expr
y                `containsFactor` _ = failf "containsFactor: %s not a product" y

multDiv :: Divides -> (IntExpr, Equals)
{- If   d | x,
   then x == d*(x/d), where x/d is an integer. -}
multDiv (d :| x) = (new, x :== new)
  where new = Product [d, Quotient x d]

factorFrom :: IntExpr -> IntExpr -> Proof (IntExpr, Equals)
{- If   s is a sum of products and every product contains a factor x
   then s may be expressed as x times a sum. -} 
x `factorFrom` sum@(Sum l) = do
      l2 <- mapM f l
      let p = Product [x, Sum l2]
      return (p, sum :== p)
  where f (Product l) = case x `elemIndex` l of
            Just i  -> let (a, b) = splitAt i l
                       in  return $ Product $ a ++ tail b
            Nothing -> failf2 "factorFrom: %s not found in %s" x l
        f x          = failf "factorFrom: %s not a product" x
_ `factorFrom` y           = failf "factorFrom: %s not a sum" y

substDividend :: Divides -> Equals -> Proof Divides
{- If   a | b and b == c,
   then a | c. -}
substDividend (a :| b) eq = liftM (a :|) $ subst b eq
```


Now for the proof. We deviate from the task description by thinking of even numbers as integers that have a certain property (namely, divisibility by 2) rather than as a subset of the natural numbers. So what we actually prove is that if <math>a</math> and <math>b</math> are each divisible by 2, so is <math>a + b</math>.


```haskell
import Control.Monad
import Proof

evensSumToEven :: String
evensSumToEven = case verify proof hypotheses claim of
    Just s  -> "Error: " ++ s
    Nothing -> "Valid"

  where hypotheses = [c2 .| n, c2 .| m]
        claim = c2 .| (n .+ m)

        proof [TS_Divides nd2, TS_Divides md2] = do
            let (n', nEQn') = multDiv nd2
                (m', mEQm') = multDiv md2
                sum' = n' .+ m'
            sum'EQsum <- equalTerms sum' [flipEq nEQn', flipEq mEQm']
            (product, sum'EQproduct) <- c2 `factorFrom` sum'
            productDIVc2 <- product `containsFactor` c2
            productEQsum <- transitiveEquality
               [flipEq sum'EQproduct, sum'EQsum]
            liftM TS_Divides $ substDividend productDIVc2 productEQsum

        (n, m) = (Var "n", Var "m")
        c2 = Constant 2

main = putStrLn evensSumToEven
```

