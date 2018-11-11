module Parse
    (
        parseString
    ) where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import DataStructure
import Semantic

data Stm' = Seq [Stm']
            | Assignment' String AExp
            | If' BExp Stm' Stm'
            | While' BExp Stm'
            | Repeat' Stm' BExp
            | For' Var AExp AExp Stm'
            | Skip'
            deriving (Show)

languageDef =
    emptyDef { Token.commentStart = "/*"
             , Token.commentEnd = "*/"
             , Token.commentLine = "//"
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.reservedNames = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "repeat"
                                     , "until"
                                     , "for"
                                     , "to"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
             , Token.reservedOpNames = [ "+", "-", "*", ":="
                                       , "<", ">", "==", "and", "or", "not"
                                       ]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
braces = Token.braces lexer
integer = Token.integer lexer
semi = Token.semi lexer
whiteSpace = Token.whiteSpace lexer

whileParser :: Parser Stm'
whileParser = whiteSpace >> statement

stm'2Stm :: Stm' -> Stm
stm'2Stm Skip' = Skip
stm'2Stm (Assignment' x a) = Assignment x a
stm'2Stm (If' c s0 s1) = If c (stm'2Stm s0) (stm'2Stm s1)
stm'2Stm (While' b s) = While b $ stm'2Stm s
stm'2Stm (Repeat' s b) = Repeat (stm'2Stm s) b
stm'2Stm (For' x a0 a1 s) = For x a0 a1 $ stm'2Stm s
stm'2Stm (Seq x) = seq2Comp x

seq2Comp [] = Skip -- ??
seq2Comp (x:[]) = stm'2Stm x
seq2Comp (x:xs) = Composition (stm'2Stm x) (seq2Comp xs)

statement :: Parser Stm'
statement = braces statement
           <|> sequenceOfStm

-- sequenceOfStm =
--     do list <- (sepBy1 statment' semi)
--        return $ if length list == 1 then head list else composeSeq list


-- composeSeq (x1:x2:[]) = Composition x1 x2
-- composeSeq (x:xs) = Composition x $osition composeSeq xs

sequenceOfStm =
    do list <- (sepBy1 statment' semi)
       -- If there's only one statement return it without using Seq.
       return $ if length list == 1 then head list else Seq list

statment' :: Parser Stm'
statment' = ifStm
            <|> whileStm
            <|> repeatStm
            <|> forStm
            <|> skipStm
            <|> assignStm

ifStm :: Parser Stm'
ifStm =
    do reserved "if"
       cond <- bExpression
       reserved "then"
       stm1 <- statement
       reserved "else"
       stm2 <- statement
       return $ If' cond stm1 stm2

whileStm :: Parser Stm'
whileStm =
    do reserved "while"
       cond <- bExpression
       reserved "do"
       stm <- statement
       return $ While' cond stm

repeatStm :: Parser Stm'
repeatStm =
    do reserved "repeat"
       stm <- statement
       reserved "until"
       cond <- bExpression
       return $ Repeat' stm cond

forStm :: Parser Stm'
forStm =
    do reserved "for"
       var <- identifier
       reservedOp ":="
       expr0 <- aExpression
       reserved "to"
       expr1 <- aExpression
       reserved "do"
       stm <- statement
       return $ For' var expr0 expr1 stm

assignStm :: Parser Stm'
assignStm =
    do var <- identifier
       reservedOp ":="
       expr <- aExpression
       return $ Assignment' var expr

skipStm :: Parser Stm'
skipStm = reserved "skip" >> return Skip'

aExpression :: Parser AExp
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExp
bExpression = buildExpressionParser bOperators bTerm


aOperators = [ [Infix (reservedOp "*" >> return (AExp Mul )) AssocLeft]
             , [Infix (reservedOp "+" >> return (AExp Sum )) AssocLeft]
             , [Infix (reservedOp "-" >> return (AExp Sub )) AssocLeft]
             ]

bOperators = [ [Prefix (reservedOp "not" >> return (Not  )) ]
             , [Infix (reservedOp "and" >> return (BExp And )) AssocLeft]
             , [Infix (reservedOp "or" >> return (BExp Or )) AssocLeft]
             ]

aTerm = parens aExpression
        <|> liftM Variable identifier
        <|> liftM Numeral integer

bTerm = parens bExpression
        <|> (reserved "true" >> return (Boolean True))
        <|> (reserved "false" >> return (Boolean False))
        <|> rExpression

rExpression =
    do a1 <- aExpression
       op <- relation
       a2 <- aExpression
       return $ ABExp op a1 a2

relation = (reservedOp ">" >> return Greater)
           <|> (reservedOp "<" >> return Smaller)
           <|> (reservedOp "==" >> return Equal)

parseString :: String -> Stm
parseString str =
    case parse whileParser "" str of
      Left e -> error $ show e
      Right r -> (stm'2Stm r)

main = do
  program <- getContents
  let ast = parseString program
  putStrLn $ show ast