{-# LANGUAGE OverloadedStrings #-}

module Compiler(compileProgramTable,
                compileCombine,
                compileRepeat,
                compileTapeToVector,
                ExprType(..),
                Env,
                StateIndex,
                Reaction(..),
                State,
                Program,
                readReaction,
                numOfStates,
                tyckExpr,
                tyckDeclaration
               ) where

import Parser

import Control.Monad(foldM)
import qualified Data.Vector as V
-- import qualified Data.Vector.Mutable as MV


data ExprType = TProg | TTape deriving (Eq,Show)
type Env = [(String, ExprType)]

type StateIndex = Int
-- a state consists of some necessary information for a machine to proceed
-- the movement
data Reaction = Reaction 
  {
    movement :: HeadMove,
    overwrite :: Content,
    nextstate :: StateIndex
  } deriving (Eq,Show)



-- now a machine consists of finitely many states. A state determines
-- the corresponding reaction for each content that the head has read

-- type State = Content -> Reaction

-- However, this representation of states is far from efficient.
-- For example, we often want to combine several programs where
-- modification is needed.
-- Therefore, we use a simpler but harder to extend form.

-- then, a program is a vector of states.
type State = (Maybe Reaction, Maybe Reaction)
type Program = V.Vector State

-- type State = (Reaction,Reaction)
-- since we have two kinds of content, we need
-- two kinds of reaction

readReaction :: Program -> StateIndex -> Content -> Maybe Reaction
readReaction p idx c =
  do line <- p V.!? idx
     case c of
       Blank -> fst line
       Mark  -> snd line


-- During initialization, the count of states is often needed.
-- return the number of states
numOfStates :: Program -> Int
numOfStates = V.length



reactionMap :: (ReactionConfig -> a) -> [BodyLine] -> [(Maybe a,Maybe a)]
reactionMap f t = map statemap t
  where statemap (r1,r2) = (f <$> r1,f <$> r2)

checkState :: BodyLine -> (Either String ExprType)
checkState (rb1,rb2) = checkReaction rb1 *> checkReaction rb2 
  where checkReaction Nothing = Right TProg
        checkReaction (Just react@(ReactionConfig i _ _)) =
          if i == 0 || i == 1 then Right TProg
          else Left $ "illegal indicator:\n " ++ show react
checkTable :: [BodyLine] -> (Either String ExprType)
checkTable = foldM f TProg
  where f _ a =
          do t <- checkState a
             return t
  
tyckExpr :: Env -> ProgramExpr -> (Either String ExprType)
tyckExpr _ (PTable tabl) = checkTable tabl
tyckExpr e (PVar u) = case (lookup u e) of
                        Just t -> Right t
                        Nothing -> Left $ "symbol " ++ u ++ " is not defined"
tyckExpr e (PCombine p1 p2) =
  do t1 <- tyckExpr e p1
     t2 <- tyckExpr e p2
     case (t1,t2) of
       (TTape,_) -> Left $ "the left part of combine is not a Program:\n " ++ show p1 ++ "\n"
       (_,TTape) -> Left $ "the right part of combine is not a Program:\n " ++ show p2 ++ "\n"
       (_,_) -> Right TProg
tyckExpr e (PRepeat p _) =
  do t <- tyckExpr e p
     if t /= TProg
       then Left $ "the inner part of repeat is not a program:\n" ++ show p ++ "\n"
       else Right TProg
tyckExpr _ (PTapeArray _) = Right TTape
tyckExpr e (PApp p1 p2) =
  do t1 <- tyckExpr e p1
     t2 <- tyckExpr e p2
     case (t1, t2) of
       (TTape,_) -> Left $ "the left part of application is not a program:\n" ++ show p1 ++ "\n"
       (_,TProg) -> Left $ "the right part of application is not a tape:\n" ++ show p2 ++ "\n"
       (_,_) -> Right TTape

tyckDeclaration :: Env -> [Declaration] -> (Either String ())
tyckDeclaration _ [] = Right ()
tyckDeclaration e ((BindingDeclaration {decIdentity=idd, decBody=body}) :ds) =
  do ty <- tyckExpr e body
     tyckDeclaration ((idd,ty):e) ds
tyckDeclaration e (RawExpr{rawBody=body}:ds) =
  do ty <- tyckExpr e body
     if ty == TTape
       then tyckDeclaration e ds
       else Left $ "the raw expr is not a tape " ++ show body ++ "\n"
       




fromRawNumToContent :: Int -> [Content]
fromRawNumToContent i = [Mark | _ <- [1..i+1]]

fromRawContentToContent :: [RawContent] -> [Content]
fromRawContentToContent [] = []
fromRawContentToContent (x:xs) =
  case x of
    RawMark c -> c : (fromRawContentToContent xs)
    RawNum  i -> (fromRawNumToContent i) ++  (fromRawContentToContent xs)
    RawIndicator -> fromRawContentToContent xs

findIndicator :: Int -> [RawContent] -> Maybe Int
findIndicator _ [] = Nothing
findIndicator i (x:xs) = case x of
                         RawIndicator -> Just i
                         RawMark _    -> findIndicator (i+1) xs
                         RawNum acc   -> findIndicator (i+acc+1) xs


compileTapeToVector :: [RawContent] -> (V.Vector Content,Int)
compileTapeToVector t =
  (V.snoc (V.fromList $ fromRawContentToContent t) Blank, idx)
  where idx = 
          case (findIndicator 0 t) of
            Just i -> i
            Nothing -> error $ "the tape " ++ show t ++ " does not have an indicator"

-- create a list, and transform the list into mutable vector
-- compileTape :: (MV.PrimMonad m) => [RawContent] -> Tape (MV.PrimState m)
-- compileTape t =
--   (V.thaw v,idx)
--   where tape = compileTapeToVector t
--         v    = fst tape
--         idx  = snd tape

intToOverWrite :: Int -> Content
intToOverWrite 1 = Mark
intToOverWrite 0 = Blank
intToOverWrite i = error $ "overwrite content " ++ show i ++ " is not legal"

  
        
exitReactionMap :: Int -> ReactionConfig -> Reaction
exitReactionMap _ (ReactionConfig ow m (Left ns)) = Reaction{movement=m,overwrite = intToOverWrite ow,nextstate = ns- 1}
exitReactionMap i (ReactionConfig ow m (Right _)) = Reaction{movement=m,overwrite = intToOverWrite ow,nextstate = i}


compileProgramTable :: [BodyLine] -> Program
-- All exiting states are set to the length
compileProgramTable t =
  V.fromList $ reactionMap (exitReactionMap i) t
  where i = length t

normalizeReaction :: Int -> Int -> Maybe ReactionConfig -> Maybe ReactionConfig
normalizeReaction exit _ ori@(Just (ReactionConfig ov mv (Left u))) =
  if u >= exit then Just (ReactionConfig ov mv (Left exit))
  else ori
normalizeReaction exit _ (Just (ReactionConfig ov mv (Right _))) =
  Just (ReactionConfig ov mv (Left exit))
normalizeReaction exit c Nothing = Just (ReactionConfig c MStop (Left exit))  
                                                                                      
normalizeState :: Int -> BodyLine -> BodyLine
normalizeState exit (r1,r2) = (normalizeReaction exit 0 r1, normalizeReaction exit 1 r2)

normalizeProgram :: [BodyLine] -> [BodyLine]
normalizeProgram p =
  map (normalizeState exit) p
  where exit = length p + 1

shiftReactionMap :: Int -> ReactionConfig -> ReactionConfig
shiftReactionMap i (ReactionConfig ov mv (Left ns)) =
  ReactionConfig ov mv (Left (ns + i))
shiftReactionMap _ ori = ori

shiftProgram :: Int -> [BodyLine] -> [BodyLine]
shiftProgram i = reactionMap (shiftReactionMap i)

compileCombine :: [BodyLine] -> [BodyLine] -> [BodyLine]
compileCombine p1 p2 =
  normalizeProgram p1 ++ shiftProgram exit p2
  where exit = length p1

repeatReactionMap :: String -> ReactionConfig -> ReactionConfig
repeatReactionMap idd ori@(ReactionConfig ov mv (Right u)) =
  if idd == u then (ReactionConfig ov mv (Left 1))
  else ori
repeatReactionMap _ ori = ori

compileRepeat :: [BodyLine] -> String -> [BodyLine]
compileRepeat source exit = reactionMap (repeatReactionMap exit) source


  
