{-# LANGUAGE RankNTypes #-}
module PHOAS where

----------------------------------------------------------------

import Control.Monad.Reader hiding (fix)

----------------------------------------------------------------

-- | PHOAS-encoded Lambda Calculus
--
-- The identity function can be defined as
--
-- >>> idLambda = HideLambda (PLam (\x -> PVar x))
--
-- >>> t2 = HideLambda (PLam (\x -> PLam (\y -> PApp (PLam (\x -> PVar x)) (PAdd (PVar x) (PLit 3)))))
-- >>> show t2
-- "\\a. \\b. (\\c. c) (a + 3)"
-- @
data PLambda a
  = PVar a
  | PLit Int
  | PBool Bool
  | PIf (PLambda a) (PLambda a) (PLambda a)
  | PAdd (PLambda a) (PLambda a)
  | PMult (PLambda a) (PLambda a)
  | PEq (PLambda a) (PLambda a)
  | PLam (a -> PLambda a)
  | PApp (PLambda a) (PLambda a)

newtype Lambda = HideLambda {revealLambda :: forall a. PLambda a}

instance Show Lambda where
  show = showLambda

showLambda :: Lambda -> String
showLambda e =
  runReader (showPLambda (revealLambda e)) 'a'
  where
    showPLambda :: PLambda Char -> Reader Char String
    showPLambda (PVar v) = return [v]
    showPLambda (PLam f) =
      do
        x <- fresh
        s <- local succ $ do showPLambda (f x)
        return ("\\" ++ [x] ++ ". " ++ s)
    showPLambda (PApp e1 e2) =
      do
        s1 <- showPLambda e1
        s2 <- showPLambda e2
        return ("(" ++ s1 ++ ") (" ++ s2 ++ ")")
    showPLambda (PLit n) = return (show n)
    showPLambda (PBool b) = return (show b)
    showPLambda (PIf e1 e2 e3) =
      do
        s1 <- showPLambda e1
        s2 <- showPLambda e2
        s3 <- showPLambda e3
        return ("if (" ++ s1 ++ ") then (" ++ s2 ++ ") else (" ++ s3 ++ ")")
    showPLambda (PAdd e1 e2) = do s1 <- showPLambda e1; s2 <- showPLambda e2; return (s1 ++ " + " ++ s2)
    showPLambda (PMult e1 e2) = do s1 <- showPLambda e1; s2 <- showPLambda e2; return (s1 ++ " * " ++ s2)
    showPLambda (PEq e1 e2) = do s1 <- showPLambda e1; s2 <- showPLambda e2; return (s1 ++ " == " ++ s2)

    fresh :: MonadReader r m => m r
    fresh = ask

data Value
  = VLit Int
  | VBool Bool
  | Val (Value -> Value)

instance Show Value where
  show (VLit x) = show x
  show (VBool x) = show x
  show (Val f) = "<<function>>"

-- |  Evaluator for PHOAS-encoded Lambda Calculus
--
-- >>> t1 = HideLambda (PApp (PLam (\x -> PAdd (PLit 3) (PVar x))) (PLit 4))
-- >>> show (eval t1)
-- 7
eval :: Lambda -> Value
eval e = evalPLambda (revealLambda e)
  where
    evalPLambda :: PLambda Value -> Value
    evalPLambda (PVar v) = v
    evalPLambda (PLit n) = VLit n
    evalPLambda (PBool b) = VBool b
    evalPLambda (PIf e1 e2 e3) = case evalPLambda e1 of
      VBool b -> if b then evalPLambda e2 else evalPLambda e3
    evalPLambda (PAdd e1 e2) = case (evalPLambda e1, evalPLambda e2) of
      (VLit x, VLit y) -> VLit (x + y)
    evalPLambda (PMult e1 e2) = case (evalPLambda e1, evalPLambda e2) of
      (VLit x, VLit y) -> VLit (x * y)
    evalPLambda (PEq e1 e2) = case (evalPLambda e1, evalPLambda e2) of
      (VLit x, VLit y) -> VBool (x == y)
    evalPLambda (PLam f) = Val (evalPLambda . f)
    evalPLambda (PApp e1 e2) = case evalPLambda e1 of
      Val f -> f (evalPLambda e2)

-- | Classic HOAS-encoding of Lambda Calculus
data Exp
  = L (Exp -> Exp)
  | A Exp Exp

-- Missing parts
evalExp :: Exp -> Value
evalExp (L f) =
  Val (evalExp . f . reify)
evalExp (A e1 e2) =
  case evalExp e1 of
    Val f -> f (evalExp e2)
    _ -> error "bad!"

reify :: Value -> Exp
reify (Val f) =
  L (reify . f . evalExp)
