{-# LANGUAGE OverloadedStrings #-}

module TuringCompiler where

import TuringParser(RawContent(..))
import Main(Content(..),TapeIndex,Tape,Program,Reaction(..),HeadMove(..))
import Control.Monad(foldM)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

type PStore = [(String,[BodyLine])]
type TStore = [(String,[Tape])]

data ExprType = TProg | TTape deriving (Eq,Show)
type Env = [(String, ExprType)]




reactionMap :: (ReactionConfig -> a) -> [BodyLine] -> [(Maybe a,Maybe a)]
reactionMap f t = map statemap t
  where statemap (r1,r2) = (f <$> r1,f <$> r2)

checkState :: BodyLine -> (Either String ExprType)
checkState (rb1,rb2) = checkReaction rb1 *> checkReaction rb2 
  where checkReaction Nothing = Right TProg
        checkReaction Just react@(ReactionConfig i mv _) =
          if i == 0 || i == 1 then Right TProg
          else Left $ "illegal indicator:\n " ++ show react
checkTable :: [BodyLine] -> (Either String ExprType)
checkTable = foldM f (Right TProg)
  where f b a =
          do t <- a
             return t
  
tyckExpr :: Env -> ProgramExpr -> (Either String ExprType)
tyckExpr e (PTable tabl) = checkTable tabl
tyckExpr e (PVar u) = case (lookup u) of
                        Just t -> Right t
                        Nothing -> Left $ "symbol " ++ u ++ " is not defined"
tyckExpr e (PCombine p1 p2) =
  do t1 <- p1
     t2 <- p2
     case (t1,t2) of
       (TTape,_) -> Left $ "the left part of combine is not a Program:\n " ++ show p1 ++ "\n"
       (_,TTape) -> Left $ "the right part of combine is not a Program:\n " ++ show p2 ++ "\n"
       (_,_) -> Right TProg
tyckExpr e (PRepeat p) u =
  do t <- p
     if t != TProg
       then Left $ "the inner part of repeat is not a program:\n" ++ show p ++ "\n"
       else Right TProg
tyckExpr e (PTapeArray _) = Right TTape
tyckExpr e (PApp p1 p2) =
  do t1 <- p1
     t2 <- p2
     case (t1, t2) of
       (TTape,_) -> Left $ "the left part of application is not a program:\n" ++ show p1 ++ "\n"
       (_,TProg) -> Left $ "the right part of application is not a tape:\n" ++ show p2 ++ "\n"
       (_,_) -> Right TTape




fromRawNumToContent :: Int -> [Content]
fromRawNumToContent i = [1 | x <- [1..i+1]]

fromRawContentToContent :: [RawContent] -> [Content]
fromRawContentToContent [] = []
fromRawContentToContent x:xs =
  case x of
    RawMark c -> c : (fromRawContentToContent xs)
    RawNum  i -> (fromRawNumToContent i) ++  (fromRawContentToContent xs)
    RawIndicator -> fromRawContentToContent xs

findIndicator :: Int -> [RawContent] -> Maybe Int
findIndicator i [] = Nothing
findIndicator i x:xs = case x of
                         RawIndicator -> Just i
                         RawMark _    -> findIndicator (i+1) xs
                         RawNum acc   -> findIndicator (i+acc+1) xs


compileTapeToVector :: [RawContent] -> (V.Vector Content,Int)
compileTapeToVector t =
  (V.fromList $ fromRawContentToContent t, idx)
  where idx = 
          case (findIndicator 0) of
            Just i -> todo
            Nothing -> error "the tape " ++ show t ++ " does not have an indicator"

-- create a list, and transform the list into mutable vector
compileTape :: (MV.PrimMonad m) => [RawContent] -> Tape (MV.PrimState m)
compileTape t =
  (V.thaw v,idx)
  where tape = compileTapeToVector t
        v    = fst tape
        idx  = snd tape

intToOverWrite :: Int -> Content
intToOverWrite 1 = Mark
intToOverWrite 0 = Blank
intToOverWrite i = error "overwrite content " ++ i ++ " is not legal"

  
        
exitReactionMap :: Int -> ReactionConfig -> Reaction
exitReactionMap i (ReactionConfig ow m (Left ns)) = Reaction{movement=m,overwrite = intToOverWrite ow,nextstate = ns}
exitReactionMap i (ReactionConfig ow m (Right _)) = Reaction{movement=m,overwrite = intToOverWrite ow,nextstate = i}


compileProgramTable :: [BodyLine] -> Program
-- All exiting states are set to the length
compileProgramTable =
  V.fromList $ reactionMap (exitReactionMap i)
  where i = length t

normalizeReaction :: Int -> Int -> Maybe ReactionConfig -> Maybe ReactionConfig
normalizeReaction exit c ori@(Just (ReactionConfig ov mv (Left u))) =
  if u >= exit then Just (ReactionConfig ov mv (Left exit))
  else ori
normalizeReaction exit c (Just (ReactionConfig ov mv (Right _))) =
  Just (ReactionConfig ov mv (Left exit))
normalizeReaction exit c Nothing = Just (ReactionConfig c MStop (Left exit))  
                                                                                      
normalizeState :: Int -> BodyLine -> BodyLine
normalizeState exit (r1,r2) = (normalizeReaction exit 0 r1, normalizeReaction exit 1 r2)

normalizeProgram :: [BodyLine] -> [BodyLine]
normalizeProgram p =
  map (normalizeState exit) p
  where exit = length p

shiftReactionMap :: Int -> ReactionConfig -> ReactionConfig
shiftReactionMap i (ReactionConfig ov mv (Left ns)) =
  ReactionConfig ov mv (Left (ns + i))
shiftReactionMap _ ori = ori

shiftProgram :: Int -> [BodyLine] -> [BodyLine]
shiftProgram i = reactionMap (shiftReactionMap i)

compileCombine :: [BodyLine] -> [BodyLine]
compileCombine p1 p2 =
  normalizeProgram p1 ++ shiftProgram exit p2
  where exit = length p1

repeatReactionMap :: String -> ReactionConfig -> ReactionConfig
repeatReactionMap idd ori@(ReactionConfig ov mv (Right u)) =
  if idd == u then (ReactionConfig ov mv (Left 0))
  else ori
repeatReactionMap _ ori = ori

compileRepeat :: [BodyLine] -> String -> [BodyLine]
compileRepeat source exit = reactionMap (repeatReactionMap exit) source


  
