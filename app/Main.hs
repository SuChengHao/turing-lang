module Main (
  main) where 

import Parser(pProgram,Declaration)
import Interpreter (simpInterpreter)
import Compiler(tyckDeclaration)
import Text.Megaparsec(runParser)
import Text.Megaparsec.Error(errorBundlePretty)
import qualified Data.Text.IO as TI
-- import Control.Exception (catch, throwIO, Exception)
import System.Environment
import System.Exit
usage :: IO ()
usage = putStrLn "turm file.turm"
exit :: IO ()
exit    = exitWith ExitSuccess

parseProgramFromFile :: String -> IO (Either String [Declaration])
parseProgramFromFile src =
  do f <- TI.readFile src
     case (runParser pProgram src f) of
       Right u -> return $ Right u
       Left err -> return $ Left $ errorBundlePretty err

tyckAndRun :: [Declaration] -> IO ()
tyckAndRun p =
  case res of
    Left msg -> putStrLn msg
    Right _  -> simpInterpreter [] p
  where
    res = tyckDeclaration [] p
  

interpProgramFromFile :: String -> IO ()
interpProgramFromFile src =
  do msg <- parseProgramFromFile src
     case msg of
       Left err -> putStr err
       Right u -> tyckAndRun u

manipulateArgs :: [String] -> IO ()
manipulateArgs ("-h":_) = usage >> exit
manipulateArgs (src:_) = interpProgramFromFile src >> exit
manipulateArgs []      = putStrLn "turm, please provide at least one source file"

main :: IO ()
main = do
  args <- getArgs
  manipulateArgs args

