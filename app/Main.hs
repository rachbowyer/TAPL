{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Text.Parsers.Frisby as Fb

-- Define AST

data Term = TmTrue 
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
   deriving (Eq, Show)


-- Haskell version of the OCaml arithmetic interpreter in TAPL
-- Note Haskell does not allow throwing exceptions in pure
-- functions - so ported to using Maybe monad.

isNumericalVal :: Term -> Bool
isNumericalVal TmZero = True
isNumericalVal (TmSucc t) = isNumericalVal t
isNumericalVal _ = False 

isVal :: Term -> Bool
isVal TmTrue = True
isVal TmFalse = True
isVal t = isNumericalVal t

eval1 :: Term -> Maybe Term
eval1 (TmIf TmTrue t _) = Just t
eval1 (TmIf TmFalse _ t) = Just t
eval1 (TmIf t1 t2 t3) = fmap (\t -> TmIf t t2 t3) $ eval1 t1
eval1 (TmSucc t) = fmap (\t' -> TmSucc t') $ eval1 t
eval1 (TmPred TmZero) = Just TmZero 
eval1 (TmPred (TmSucc t)) | isNumericalVal t = Just t
eval1 (TmPred t) = fmap (\t' -> TmPred t') $ eval1 t
eval1 (TmIsZero TmZero) = Just TmTrue
eval1 (TmIsZero (TmSucc t)) | isNumericalVal t = Just TmFalse
eval1 (TmIsZero t) = fmap (\t' -> TmIsZero t') $ eval1 t
eval1 _ = Nothing 

eval :: Term -> Term
eval t = case eval1 t of 
  Just t' -> eval t'
  Nothing -> t


-- Simple parser. A PEG packrack parser using Frisby library
-- Parenthesis are optional, but if used must balance

parser :: forall s . PM s (P s Term)
parser = mdo 
  whiteSpace <- Fb.newRule $ many (char ' ')
  embeddedRulesNoParen <- Fb.newRule $ whiteSpace ->> rules <<- whiteSpace
  embeddedRulesParen <- Fb.newRule $ whiteSpace ->> char '(' ->> whiteSpace 
                        ->> rules <<- whiteSpace <<- char ')' <<- whiteSpace
  embeddedRules <- Fb.newRule $ embeddedRulesParen // embeddedRulesNoParen

  zeroRule <- Fb.newRule $ char '0' ## \_ -> TmZero
  falseRule <- Fb.newRule $ text "false" ## \_ -> TmFalse
  trueRule <- Fb.newRule $ text "true" ## \_ -> TmTrue
  isZeroRule <- Fb.newRule $ text "iszero" ->> embeddedRules ## TmIsZero 
  predRule <- Fb.newRule $ text "pred" ->> embeddedRules ## TmPred
  succRule <- Fb.newRule $ text "succ" ->> embeddedRules ## TmSucc 
  ifRule <- Fb.newRule $ text "if" ->> embeddedRules <<- text "then" 
            Fb.<> embeddedRules <<- text "else" Fb.<> embeddedRules 
            ## \((a, b), c) -> TmIf a b c

  rules <- Fb.newRule $ ifRule // succRule // predRule // isZeroRule 
                               // trueRule // falseRule // zeroRule

  return rules


main :: IO ()
main = do
    c <- getContents
    print $ eval $ runPeg parser c


-- Example
-- echo "if (iszero (pred (pred (succ 0)))) then succ 0 else 0"  | cabal run tapl
