{-
  A basic calculator for arithmetic expressions
  Based on the example in Chapter 8 of "Programming in Haskell"
  by Graham Hutton.

  Pedro Vasconcelos, 2025
-}
module Main where

import Parsing
import Data.Char

--
-- a data type for expressions
-- made up from integer numbers, + and *
--
data Expr = Num Integer
          | Var Name -- Part 2 variable reference
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Mod Expr Expr
          deriving Show
-- Part 2 type and data create
type Name = String
type Env = [(Name, Integer)]

data Command = Assign Name Expr -- x = expr
              | EvalCmd Expr -- just an expression command
              deriving Show

-- a recursive evaluator for expressions
--
eval :: Env -> Expr -> Integer
eval env (Num n) = n
eval env (Var x) = case lookup x env of -- part2
                  Just v -> v -- part2
                  Nothing -> error ("undefined variable: " ++ x) -- part2
eval env (Add e1 e2) = eval env e1 + eval env e2 -- add env to everything 
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2
eval env (Mod e1 e2) = eval env e1 `mod` eval env e2

-- | a parser for expressions
-- Grammar rules:
--
-- expr ::= term exprCont
-- exprCont ::= '+' term exprCont | epsilon

-- term ::= factor termCont
-- termCont ::= '*' factor termCont | epsilon

-- factor ::= natural | '(' expr ')'

expr :: Parser Expr
expr = do t <- term
          exprCont t

exprCont :: Expr -> Parser Expr
exprCont acc = do 
                  char '+'
                  t <- term
                  exprCont (Add acc t)
               <|> (do 
                      char '-'
                      t <- term
                      exprCont (Sub acc t))
               <|> return acc
              
term :: Parser Expr
term = do f <- factor
          termCont f

termCont :: Expr -> Parser Expr
termCont acc =  do 
                   char '*'
                   f <- factor  
                   termCont (Mul acc f)
                 <|> (do
                        char '/'
                        f <- factor
                        termCont (Div acc f))
                 <|> (do
                        char '%'
                        f <- factor
                        termCont (Mod acc f))
                 <|> return acc

factor :: Parser Expr
factor =  (do 
            x <- variable
            return (Var x)) --Part 2 add variable to factor
          <|>
          (do 
            n <- natural
            return (Num n))
          <|>
          do char '('
             e <- expr
             char ')'
             return e
             

natural :: Parser Integer
natural = do xs <- many1 (satisfy isDigit)
             return (read xs)

variable :: Parser Name --Part 2 create variable function
variable = do
             xs <- many1 (satisfy isLetter)
             return xs

command :: Parser Command --Part 2 create command function
command = (do
             x <- variable
             char '='
             e <- expr
             return (Assign x e))
          <|> (do
                 e <- expr
                 return (EvalCmd e))

update :: Name -> Integer -> Env -> Env --Part 2 create update function
update x v env = (x, v) : filter (\(y,_) -> y /= x) env

runCommand :: Env -> Command -> (Integer, Env) --Part 2 create runCommand func
runCommand env (EvalCmd e) =
  let v = eval env e
  in (v, env)
runCommand env (Assign x e) =
  let v = eval env e
      env' = update x v env
  in (v, env')
----------------------------------------------------------------             
  
main :: IO ()
main
  = do 
      txt <- getContents
      calculator [] (lines txt) --Part 2 add []

-- | read-eval-print loop
calculator :: Env -> [String] -> IO () --Part 2 add Env -> before [String]
calculator _ []  = return () --Part 2 add _ before []
calculator env (l:ls) = do --Part 2 add env and redo func
                          let (out, env') = process env l
                          putStrLn out
                          calculator env' ls  

-- | evaluate a single expression
{-evaluate :: String -> String --Part 2 replace this evaluate func for process below
evaluate txt
  = case parse expr txt of
      [ (tree, "") ] ->  show (eval tree)
      _ -> "parse error; try again"-}  

process :: Env -> String -> (String, Env) --Part 2 create process func -
process env txt =
  case parse command txt of
    [(cmd, "")] ->
      let (v, env') = runCommand env cmd
      in  (show v, env')
    _ ->
      ("parse error; try again", env)