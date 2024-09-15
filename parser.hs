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

type Parser = Parsec Void Text

-- eat spaces
-- the following code is copied from Mark Karpov's Megaparsec tutorial
-- see https://markkarpov.com/tutorial/megaparsec.html
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

integer :: Parser Integer
integer = lexeme L.decimal


