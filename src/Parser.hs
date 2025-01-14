module Parser where

import Lang
import Text.Parsec hiding (runP,parse)
--import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
-- import qualified Text.Parsec.Expr as Ex

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------

-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser langDef

-- Analizador de Tokens
langDef :: LanguageDef u
langDef = emptyDef
    { commentStart    = "{-"
    , commentEnd      = "-}"
    , commentLine     = "--"
    , reservedNames   = ["def", "false"]
    , reservedOpNames = [ "&&", "||", "!", "->"]
    }

--  

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

--------------------------
-- Parser
--------------------------

-- | Parser de Prop

pnot :: P (SProp -> SProp)
pnot = do
  reservedOp "!"
  return Not

pand :: P SProp 
pand = do
  p1 <- sprop3
  reservedOp "&&"
  And p1 <$> sprop3

por :: P SProp
por = do
  p1 <- sprop3
  reservedOp "||"
  Or p1 <$> sprop3

implies :: P SProp 
implies = do
  p1 <- sprop2
  reservedOp "->"
  Implies p1 <$> sprop1

atom :: P SProp
atom = Atom <$> identifier

bottom :: P SProp
bottom = do
  reserved "false"
  return Bottom

def :: P SProp
def = do
  reserved "def"
  Def <$> identifier

sprop :: P SProp
sprop = sprop1

sprop1 :: P SProp
--sprop1 = try implies <|> sprop2
sprop1 = chainr1 sprop2 (reservedOp "->" >> return Implies)
sprop2 :: P SProp
sprop2 = try por <|> try pand <|> sprop3

sprop3 :: P SProp
sprop3 = try (pnot <*> sprop3) <|> sprop4

sprop4 :: P SProp
sprop4 = atom <|> bottom <|> def <|> parens sprop

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SProp
parse s = case runP sprop s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s ++ "\n" ++ show e)



