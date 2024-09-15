{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards#-}

module TuringParser where


import Control.Applicative
import Control.Monad
import qualified Data.Vector as V
import Data.Text (Text)
import Data.Void
import qualified Data.Text.IO as TI
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Main (Content,HeadMove,Reaction,State,StateIndex,TapeIndex)

data ReactionConfig = ReactionConfig Int HeadMove (Either Int Text)
type BodyLine = (Maybe ReactionConfig,Maybe ReactionConfig)


data Declaration = ProgramDeclaration {programIdentity::Text,
                                       programBody::[BodyLine]}
                 | TapeDeclaration    {tapeIdentity::Text,
                                       tapeBody::[Content],
                                       tapeIndex::Int}
                 | ProgramCombine     {combinationIdentity::Text,
                                       combinationBody::[Text]}
                 | ProgramRepeat      {repeatIdentity :: Text,
                                       repeatBody     :: (Either Text, [BodyLine]),
                                       repeatState    :: Text}
                 | ProgramRun         {ProgramIdentity :: Text,
                                       programBody :: Text}
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

integer :: Parser Integer
integer = lexeme L.decimal

pVariable :: Parser Text
pVariable = lexeme $ (:) <$> letterChar <*> many alphaNumber <?> "variable"

pStateNum :: Parser (Either Int Text)
pStateNum =
  do num <- lexeme L.decimal
     return (Left num)

pStateText :: Parser (Either Int Text)
pStateText =
  do et <- 

pNextState :: Parser (Either Int Text)

pReactionConfig :: Parser ReactionConfig
pReactionConfig = do

pDeclaration :: Parser Declaration

pProgram :: Parser [Declaration]
pProgram = many


