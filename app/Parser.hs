{-# LANGUAGE OverloadedStrings #-}


module TuringParser where


import Control.Applicative hiding (many)
import Control.Monad
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Void
import qualified Data.Text.IO as TI
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import Main(HeadMove(..),Content(..))

data ReactionConfig = ReactionConfig Int HeadMove (Either Int String) deriving (Eq,Show)
type BodyLine = (Maybe ReactionConfig,Maybe ReactionConfig)

data RawContent = RawMark Content |
                  RawNum  Int     |
                  RawIndicator    
                  deriving (Eq, Show)

data ProgramExpr = PTable [BodyLine]
                  | PVar String
                  | PCombine ProgramExpr ProgramExpr
                  | PRepeat ProgramExpr String deriving (Eq,Show)
data TapeExpr = TVar String
                | TArray [RawContent] deriving (Eq,Show)

data Declaration = ProgramDeclaration {programIdentity::String,
                                       programBody::ProgramExpr}
                 | TapeDeclaration    {tapeIdentity::String,
                                       tapeBody::TapeExpr}
                 | RunDeclaration     {programToRun :: ProgramExpr,
                                       tapeToRun :: TapeExpr}
                   deriving (Eq,Show)

type Parser = Parsec Void Text

-- eat spaces
-- the following code is copied from Mark Karpov's Megaparsec tutorial
-- see https://markkarpov.com/tutorial/megaparsec.html
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer ::(Num a) => Parser Integer
integer = lexeme L.decimal

pVariable :: Parser String
pVariable = lexeme ( (:) <$> letterChar <*> many alphaNumChar <?> "variable")

pStateNum :: Parser (Either Int Text)
pStateNum = Left <$> (lexeme L.decimal)


pStateText :: Parser (Either Int Text)
pStateText = Right <$> pVariable

pNextState :: Parser (Either Int Text)
pNextState = pStateNum <|> pStateText


pHeadMove :: Parser HeadMove
pHeadMove = choice
  [ MLeft <$ string "L",
    MRight <$ string "R",
    MStop <$ string "O"]

pReactionConfig :: Parser ReactionConfig
pReactionConfig = do
  ow <- integer
  mv <- pHeadMove
  ns <- pNextState
  return ReactionConfig ow mv ns

pBodyLine :: Parser BodyLine
pBodyLine = do
  r1 <- (optional . try $ lexeme pReactionConfig) <* (string ",")
  r2 <- (optional . try $ lexeme pReactionConfig) <* (string ";")
  return (r1,r2)

pProgramTable :: Parser ProgramExpr
pProgramTable = PTable <$> between (char '{') (char '}') $ many $ lexeme pBodyLine
  
pProgramVariable :: Parser ProgramExpr
pProgramVariable = PVar <$> pVariable

pProgramParens :: Parser a -> Parser a
pProgramParens :: between (symbol "(") (symbol ")")

pProgramRepeat :: Parser ProgramExpr
pRrogramRepeat = do
  p <- pRogramExpr
  void $ lexeme $ string "!"
  s <- pVariable
  return $ PRepeat p s
  

pTerm :: Parser ProgramExpr
pTerm = choice [
  pProgramTable,
  pProgramParens pProgramExpr,
  try pProgramRepeat,
  pProgramVariable
 ]

operatorTable :: [[Operator Parser ProgramExpr]]
operatorTable =
  [[
      binary "|=>" PCombine
   ]]
  
binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary name f = InfixL (f <$ symbol name)

pProgramExpr :: Parser ProgramExpr
pProgramExpr = makeExprParser pTerm operatorTable

pRawNum :: Parser RawContent
pRawNum = between (char '(') (char ')') $ RawNum <$> integer

pRawContent :: Parser RawContent
pRawContent = choice
  [ RawMark Mark <$ char '1',
    RawMark Blank <$ char '0',
    RawIndicator  <$ char '|',
    pRawNum]
  
pTapeArray :: Parser TapeExpr
pTapeArray = TArray <$> between (char '[') (char ']') $ many pRawContent

pTape :: Parser TapeExpr
pTape = (TVar <$> pVariable ) <|> pTapeArray

pProgramDeclaration :: Parser Declaration
pProgramDeclaration = do
  n <- pVariable
  void $ lexeme $ char '='
  p <- lexeme pProgramExpr
  return ProgramDeclaration{programIdentity=n,programBody=p}

pTapeDeclaration = do
  n <- pVariable
  void $ lexeme $ char '='
  p <- lexeme pTape
  return TapeDeclaration{tapeIdentity=n,tapeBody=p}

pRunDeclaration  = do
  p <- pProgramExpr
  void $ lexeme $ symbol "|>"
  t <- lexeme pTape
  return RunDeclaration{programToRun=p,tapeToRun=t}

pProgram :: Parser [Declaration]
pProgram = many pRunDeclaration

