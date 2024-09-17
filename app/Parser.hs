{-# LANGUAGE OverloadedStrings #-}


module Parser(ReactionConfig(..),
               RawContent(..),
               ProgramExpr(..),
               BodyLine,
               Declaration(..),
               Content(..),
               HeadMove(..),
               pProgram) where


import Control.Applicative hiding (many)
import Control.Monad
-- import qualified Data.Vector as V
import Data.Text (Text)
import Data.Void
-- import qualified Data.Text.IO as TI
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L

data ReactionConfig = ReactionConfig Int HeadMove (Either Int String) deriving (Eq,Show)
type BodyLine = (Maybe ReactionConfig,Maybe ReactionConfig)

data HeadMove = MLeft | MRight | MStop deriving (Eq, Show)
data Content = Blank | Mark   deriving (Eq, Show)

data RawContent = RawMark Content |
                  RawNum  Int     |
                  RawIndicator    
                  deriving (Eq, Show)

data ProgramExpr = PTable [BodyLine]
                  | PVar String
                  | PCombine ProgramExpr ProgramExpr
                  | PRepeat ProgramExpr String
                  | PTapeArray [RawContent]
                  | PApp ProgramExpr ProgramExpr
                 deriving (Eq,Show)

-- data TapeExpr = TVar String
--                 | TArray [RawContent]
--                 | TApp TapeExpr TapeExpr
--                 | TPWild ProgramExpr
--               deriving (Eq,Show)

data Declaration = BindingDeclaration {decIdentity::String,
                                      decBody :: ProgramExpr}
                   | RawExpr            {rawBody :: ProgramExpr}
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


symbol :: Text -> Parser Text
symbol = L.symbol sc

integer ::(Num a) => Parser a
integer = lexeme L.decimal

pVariable :: Parser String
pVariable = lexeme ( (:) <$> letterChar <*> many alphaNumChar <?> "variable")

pStateNum :: Parser (Either Int String)
pStateNum = Left <$> (lexeme L.decimal)


pStateText :: Parser (Either Int String)
pStateText = Right <$> pVariable

pNextState :: Parser (Either Int String)
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
  return $ ReactionConfig ow mv ns

pBodyLine :: Parser BodyLine
pBodyLine = do
  r1 <- (optional . try $ lexeme pReactionConfig) <* (string ",")
  r2 <- (optional . try $ lexeme pReactionConfig) <* (string ";")
  return (r1,r2)

pProgramTable :: Parser ProgramExpr
pProgramTable = PTable <$> (between (lexeme $ char '{') (lexeme $ char '}') $ many $ lexeme pBodyLine)
  
pProgramVariable :: Parser ProgramExpr
pProgramVariable = PVar <$> (lexeme ( (:) <$> letterChar <*> many alphaNumChar <?> "variable"))

pProgramParens :: Parser a -> Parser a
pProgramParens = between (lexeme $ symbol "(") (lexeme $ symbol ")")

pProgramRepeat :: Parser ProgramExpr
pProgramRepeat = do
  p <- between (lexeme $ symbol "<") (lexeme $ symbol ">") $ pProgramExpr
  s <- pVariable
  return $ PRepeat p s  

pRawNum :: Parser RawContent
pRawNum = between (char '(') (char ')') $ RawNum <$> integer

pRawContent :: Parser RawContent
pRawContent = choice
  [ RawMark Mark <$ char '1',
    RawMark Blank <$ char '0',
    RawIndicator  <$ char '|',
    pRawNum]
  
pTapeArray :: Parser ProgramExpr
pTapeArray = PTapeArray <$> (between (char '[') (char ']') $ many pRawContent)

pTerm :: Parser ProgramExpr
pTerm = choice [
  pProgramTable,
  pTapeArray,
  pProgramParens pProgramExpr,
  pProgramRepeat,
  pProgramVariable
 ]

operatorTable :: [[Operator Parser ProgramExpr]]
operatorTable =
  [ [
      binary "|=>" PCombine
    ],
    [ binaryr "|>" PApp]
  ]
  
binary :: Text -> (ProgramExpr -> ProgramExpr -> ProgramExpr) -> Operator Parser ProgramExpr
binary name f = InfixL (f <$ symbol name)

binaryr :: Text -> (ProgramExpr -> ProgramExpr -> ProgramExpr) -> Operator Parser ProgramExpr
binaryr name f = InfixR (f <$ symbol name)

pProgramExpr :: Parser ProgramExpr
pProgramExpr =  makeExprParser pTerm operatorTable

-- program: declarations
-- binding declaration:

pBindingDeclaration :: Parser Declaration
pBindingDeclaration =  do
  n <- pVariable
  void $ lexeme $ char '='
  p <- lexeme pProgramExpr
  return BindingDeclaration{decIdentity=n,decBody=p}


pSimpDeclaration :: Parser Declaration
pSimpDeclaration =RawExpr <$> ((symbol "!") *> pProgramExpr)

pProgram :: Parser [Declaration]
pProgram = many $ (choice [try pSimpDeclaration, pBindingDeclaration])



