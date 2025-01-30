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
    , reservedNames   = ["hip","def", "context", "proof", "by", "0", "||e", "||i1", "||i2", "&&e1", "&&e2", "&&i", "->e", "->i", "!!e", "!i", "0e", "0i", "t"]
    , reservedOpNames = [ "&&", "||", "!", "->"]
    }

--  

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

braces :: P a -> P a
braces = Tok.braces lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

nat :: P Integer
nat = Tok.natural lexer
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
  reserved "0"
  return Bottom

def :: P SProp
def = do
  reserved "def"
  Def <$> identifier

sprop :: P SProp
sprop = sprop1

sprop1 :: P SProp
sprop1 = chainr1 sprop2 (reservedOp "->" >> return Implies)

sprop2 :: P SProp
sprop2 = chainr1 sprop3 (reservedOp "||" >> return Or)

sprop3 :: P SProp
sprop3 = chainl1 sprop4 (reservedOp "&&" >> return And)

sprop4 :: P SProp
sprop4 = pnot <*> sprop4 <|> atom <|> bottom <|> def <|> parens sprop

-- | Parser de NaTree

natree :: P NaTree
natree = do
  p <- sprop
  rule <- rule0 <|> rule1 <|> rule2 <|> rule3
  return $ rule p

rule0 :: P (SProp -> NaTree)
rule0 = hip

hip :: P (SProp -> NaTree)
hip = do
  reserved "hip"
  Hip <$> nat

-- subtree1 
rule1 :: P (SProp -> NaTree)
rule1 = 
  andElim1 <|> andElim2 <|>
  notInt <|> dobNotElim <|>
  impliesInt <|> orInt1 <|>
  orInt2 <|> trivial

andElim1 :: P (SProp -> NaTree)
andElim1 =  do
  reserved "&&e1"
  AndElim First <$> subtree1

andElim2 :: P (SProp -> NaTree)
andElim2 =  do
  reserved "&&e2"
  AndElim Second <$> subtree1

notInt :: P (SProp -> NaTree)
notInt = do
  reserved "!i"
  NotInt <$> subtree1

dobNotElim :: P (SProp -> NaTree)
dobNotElim = do
  reserved "!!e"
  DobNotElim <$> subtree1

impliesInt :: P (SProp -> NaTree)
impliesInt = do
  reserved "->i"
  n <- nat
  ImpliesInt n <$> subtree1

orInt1 :: P (SProp -> NaTree)
orInt1 = do
  reserved "||i1"
  OrInt First <$> subtree1

orInt2 :: P (SProp -> NaTree)
orInt2 = do
  reserved "||i2"
  OrInt Second <$> subtree1


trivial :: P (SProp -> NaTree)
trivial = do
  reserved "t"
  Trivial <$> subtree1

subtree1 :: P NaTree
subtree1 = braces natree
--subtrees2

rule2 :: P (SProp -> NaTree)
rule2 = andInt <|> bottomInt <|> impliesElim

andInt :: P (SProp -> NaTree)
andInt = do
  reserved "&&i"
  (t1,t2) <- subtree2
  return $ AndInt t1 t2

bottomInt :: P (SProp -> NaTree)
bottomInt = do
  reserved "0i"
  (t1,t2) <- subtree2
  return $ BottomInt t1 t2

impliesElim :: P (SProp -> NaTree)
impliesElim = do
  reserved "->e"
  (t1,t2) <- subtree2
  return $ ImpliesElim t1 t2

subtree2 :: P (NaTree, NaTree)
subtree2 = do
  t1 <- braces natree
  t2 <- braces natree
  return (t1, t2)

--subtrees3
rule3:: P (SProp -> NaTree)
rule3 = orElim

orElim :: P (SProp -> NaTree)
orElim = do
  reserved "||e"
  n1 <- nat
  n2 <- nat
  (t, t1, t2) <- subtree3
  return $ OrElim t n1 t1 n2 t2

subtree3 :: P (NaTree, NaTree, NaTree)
subtree3 = do
  t1 <- braces natree
  t2 <- braces natree
  t3 <- braces natree
  return (t1, t2, t3)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SProp
parse s = case runP sprop s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s ++ "\n" ++ show e)