module Main (
  main) where 

import Parser(pProgram,Declaration)
import Interpreter (simpInterpreter)
import Compiler(tyckDeclaration)
import Text.Megaparsec(runParser)
import Text.Megaparsec.Error(errorBundlePretty)
import qualified Data.Text.IO as TI
import Control.Monad (foldM_)
import System.Environment
import System.Console.GetOpt
import System.Exit
import System.IO

data Flag =
  Version
  | Help
  deriving (Eq,Ord,Enum,Show)

flags :: [OptDescr Flag]
flags = [Option ['v'] [] (NoArg Version) "show version information",
         Option ['h'] ["help"] (NoArg Help) "show help message"]


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
  do
    putStrLn $ "[" ++ src ++ "]"
    msg <- parseProgramFromFile src
    case msg of
      Left err -> putStr err
      Right u -> tyckAndRun u


main :: IO ()
main = do
  argv <- getArgs
  case getOpt Permute flags argv of
    (args,fs,[]) -> 
      if Help `elem` args
      then do hPutStrLn stderr (usageInfo header flags)
              exitWith ExitSuccess
      else if Version `elem` args
           then  do hPutStrLn stderr "turm version 0.1.0.0"
                    exitWith ExitSuccess
           else foldM_ (\_ -> interpProgramFromFile) () fs
    (_,_,errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo header flags)
      exitWith (ExitFailure 1)
    where header = "Usage turm [-hv] [file.turm ...]"

