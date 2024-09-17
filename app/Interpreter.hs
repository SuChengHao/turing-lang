module Interpreter(
  Content(Blank,Mark),
  HeadMove(MLeft,MRight,MStop),
  Reaction(..),
  State,
  StateIndex,
  Program,
  TapeIndex,
  Tape,
  simpInterpreter
  ) where

import Parser
import Compiler
import Data.List(groupBy)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


import Control.Monad (forM_)
import Control.Monad.ST (runST)

data Value = VProgram [BodyLine]
           | VTape    (V.Vector Content,Int)
           deriving (Eq,Show)

type VStore = [(String,Value)]





-- A program runs on a paper tape.
-- So a tape has infinitely many cells that are stored one by one.
-- Since the content of cells are often updated during evaluation,
-- we implement it as a mutable vector


type TapeIndex = Int
type Tape s = (MV.MVector s Content, TapeIndex)

nextIndex :: TapeIndex -> HeadMove -> Maybe TapeIndex
nextIndex i MLeft = if i > 0 then Just (i - 1) else Nothing
nextIndex i MRight = Just (i + 1)
nextIndex i MStop = Just i

adjustTape :: (MV.PrimMonad m) => Tape (MV.PrimState m) -> Int  -> m (Tape (MV.PrimState m))
adjustTape (tape, idx) growlen  =
  do
    let len = MV.length tape
    ntape <- MV.grow tape growlen
    forM_ [len..(len + growlen - 1)] $ (\i -> MV.write ntape i Blank)
    return (ntape,idx)

data Halting = UndefinedState | CrossTape Reaction | Skip Reaction deriving (Eq,Show)
isHalt ::  (MV.PrimMonad m) => Program -> StateIndex -> Content -> Tape (MV.PrimState m) -> m Halting
isHalt p idx c (_,tidx) =
  case (readReaction p idx c) of
    Nothing -> return UndefinedState
    Just react@Reaction{movement=mv} -> case (nextIndex tidx mv) of
                                          Just _ -> return (Skip react)
                                          Nothing -> return $ CrossTape react
      
reactWriteTape :: (MV.PrimMonad m) => Reaction -> Tape (MV.PrimState m) -> m ( )
reactWriteTape Reaction{overwrite=ov} (tape,tidx) =
    -- first, try to adjustTape if the idx is quite larger
  MV.write  tape tidx ov
reactMoveHead :: (MV.PrimMonad m) => Reaction -> Tape (MV.PrimState m) -> m (Tape(MV.PrimState m))
reactMoveHead Reaction{movement=mv} conf@(tape,tidx) = 
  let len = MV.length tape in
      case (nextIndex tidx mv) of
        Just i -> do { ntape <- if i < len then return conf else adjustTape conf len;
                       return (fst ntape, i) }
        Nothing -> error "access negative-indexed content but the machine has not halted.\n"
        
reduceProgram :: (MV.PrimMonad m) => Program -> StateIndex -> Tape (MV.PrimState m) -> m (Tape(MV.PrimState m))
reduceProgram p pidx conf@(tape,tidx) =
  do
    currentContent <- MV.read tape tidx
    halt <- isHalt p pidx currentContent (tape,tidx)
    case halt of
      UndefinedState -> return conf
      CrossTape react-> do  reactWriteTape react conf
                            return conf
      Skip react          -> do reactWriteTape react conf
                                ntape <- reactMoveHead react conf
                                reduceProgram p (nextstate react) ntape
      
 
charifyContent :: Content -> Char
charifyContent Blank = 'o'
charifyContent Mark  = '✓'

-- naiveTapeToString :: (MV.PrimMonad m) => Tape (MV.PrimState m) -> m String
-- naiveTapeToString (tape, ind) =
--   do
--     let process acc i ele =
--           if i == ind
--           then '|':(charifyContent ele):'|':acc
--           else (charifyContent ele):acc
--     str <- MV.ifoldl process "" tape
--     return $ reverse str
    
-- formatTape :: (V.Vector Content , TapeIndex) -> String
-- formatTape (tape, ind) =
--   reverse $ V.ifoldl process "" tape
--   where process acc i ele =
--           if i == ind
--           then '|':(charifyContent ele):'|':acc
--           else (charifyContent ele):acc

compressCharify :: [Content] -> String
compressCharify [] = ""
compressCharify l@(x:_) =
  case x of
    Blank -> take n (repeat 'o')
    Mark  -> "("++show (n - 1)++")"
    where n = length l
    
  
                       
compressListTapeAux :: [[Content]] -> TapeIndex -> String
compressListTapeAux [] _ = ""
compressListTapeAux (x:xs) ti =
  if length x < ti || ti < 0 then
    compressCharify x ++ compressListTapeAux xs (ti - length x)
  else
    f ti ++ compressListTapeAux xs (-1)
    where
      sym = charifyContent $ head x
      f ind = (take ind (repeat sym)) ++ "|" ++ (take (length x - ind) (repeat sym))
    
  

compressListTape :: [Content] -> TapeIndex -> String
compressListTape tl ti = compressListTapeAux inter ti
  where inter = groupBy (==) tl
        
compressFormatTape :: (V.Vector Content, TapeIndex) -> String
compressFormatTape (tv,ti) =
  compressListTape tlist ti
  where tlist = V.toList tv
        
simpFormatProgram :: [BodyLine] -> String
simpFormatProgram = show

simpFormatValue :: Value -> String
simpFormatValue (VProgram bd) = simpFormatProgram bd
simpFormatValue (VTape tbl) = compressFormatTape tbl

-- freezeTape :: (MV.PrimMonad m) => Tape (MV.PrimState m) -> m ((V.Vector Content),TapeIndex)
-- freezeTape (tape,ind) = do ntape <- V.freeze tape
--                            return (ntape,ind)


-- printTape ::(MV.PrimMonad m) => Tape (MV.PrimState m) -> m String
-- printTape t = do str <- naiveTapeToString t
--                  return $ reverse str

-- tapen :: (MV.PrimMonad m) => Int -> m (Tape (MV.PrimState m))
-- tapen n = do
--   tape <- MV.new (n+3)
--   forM_ [0 .. n+2] $ \i -> MV.write tape i Blank
--   forM_ [1..n+1] $ \i -> MV.write tape i Mark
--   return (tape,1)

-- programSucc::Program
-- programSucc = V.create (do {
--                          v <- MV.new 2;
--                          MV.write v 0 (Just Reaction{movement=MLeft,overwrite=Mark,nextstate=1},
--                                  Just Reaction{movement=MRight,overwrite=Mark,nextstate=0});
--                          MV.write v 1 (Just Reaction{movement=MRight,overwrite=Blank,nextstate=2},
--                                    Just Reaction{movement=MLeft,overwrite=Mark,nextstate=1});
--                          return v
--                       })
-- isExitReaction :: Int -> Reaction -> Bool
-- isExitReaction u Reaction{nextstate=ns} | ns >= u = True
-- isExitReaction u _ = False

-- isExitState :: Int -> State -> Maybe Bool
-- isExitState i (r1,r2) =
--   do
--     let ier r = fmap (isExitReaction i) r
--     b1 <- ier r1
--     b2 <- ier r2
--     return $ b1 || b2

-- stopStates :: Program -> [StateIndex]
-- stopStates p = V.ifoldl (\acc i ele -> case isExitState u ele of
--                                          Just True    -> i:acc
--                                          _            -> acc)
--                         [] p
--   where u = numOfStates p
-- reactionShift :: Int -> Reaction -> Reaction
-- reactionShift i Reaction{movement=mv,overwrite=ow,nextstate = ns} =
--   Reaction{movement=mv,overwrite=ow,nextstate = ns + i}
  

-- stateShift :: Int -> Program -> Program
-- stateShift i = V.map (\x -> case x of
--                               (r1,r2) ->( (maybeShift r1), (maybeShift r2)))
--                where maybeShift = fmap (reactionShift i)



-- modifyExitReaction :: (Reaction -> Bool) -> Reaction ->  StateIndex -> Reaction
-- modifyExitReaction pred react@Reaction{movement=mv,overwrite=ov,nextstate=_} ns =
--   if (pred react) then Reaction{movement=mv,overwrite=ov,nextstate=ns}
--   else react

-- modifyExitState :: (Reaction -> Bool) -> StateIndex -> State -> State
-- modifyExitState pred u (rb1,rb2) = (mk rb1 Blank,mk rb2 Mark)
--   where mk react c = Just (case react of
--                              Just r -> modifyExitReaction pred r u
--                              Nothing -> Reaction{movement=MStop,overwrite=c,nextstate=u})
                       

-- programRepeat :: Program-> Program
-- programRepeat p =
--   p V.// (map createExitState (stopStates p))
--   where u = numOfStates p
--         process = modifyExitState (\rb -> case rb of Reaction{nextstate=ns} -> ns >=u) 0
--         createExitState i = (i, process $ p V.! i)

-- combine two programs
-- programCombine :: Program -> Program -> Program
-- programCombine p1 p2 =
--   -- collecting all stop reaction, and substitute a jump reaction for it.
--   p1 V.// (map createExitState (stopStates p1)) V.++ (stateShift u p2)
--   where u = numOfStates p1
--         process = modifyExitState (\rb -> case rb of Reaction{nextstate=ns} -> ns >=u) u
--         createExitState i = (i, process $ p1 V.! i)
          


extractProgram :: Value -> [BodyLine]
extractProgram (VProgram body) = body
extractProgram _             = error "unexpected error, the result is not a program"

extractTape :: Value -> (V.Vector Content,Int)
extractTape (VTape body) = body
extractTape _          = error "unexpected error, the result is not a tape"


runExpr :: VStore -> ProgramExpr -> Value
runExpr s (PVar u) = case (lookup u s) of
                          Just v -> v
                          Nothing -> error "unexpected error, undefined reference"
runExpr _ (PTable bodys)     = VProgram bodys
runExpr _ (PTapeArray tbl)   = VTape $ compileTapeToVector tbl
runExpr s (PCombine p1 p2)   =
  VProgram $ compileCombine v1 v2
  where v1 = extractProgram $ runExpr s p1
        v2 = extractProgram $ runExpr s p2
runExpr s (PRepeat p u)      =
  VProgram $ compileRepeat v u
  where v = extractProgram $ runExpr s p
runExpr s (PApp p t)         =
  VTape $ runApp pv tv
  where pv = extractProgram $ runExpr s p
        tv = extractTape $ runExpr s t

runApp :: [BodyLine] -> (V.Vector Content,Int) -> (V.Vector Content, Int)
runApp ptbl (tv,ti) = runST $
  do
    let p = compileProgramTable ptbl
    mt <- V.thaw tv
    res <- reduceProgram p 0 (mt,ti)
    afterMT <- V.freeze $ fst res
    let afterIdx = snd res
    
    return (afterMT, afterIdx)



simpInterpreter :: VStore -> [Declaration] -> IO ()
simpInterpreter _ [] = return ()
simpInterpreter s ((BindingDeclaration {decIdentity=i,decBody = body}):ds) =
  let v = runExpr s body in
    simpInterpreter ((i,v):s) ds
    
simpInterpreter s (RawExpr{rawBody=body}:ds) =
  -- do let v = extractTape $ runExpr s body
  --    putStrLn $ compressFormatTape v
  --    simpInterpreter s ds
  do let v = runExpr s body
     putStrLn $ simpFormatValue v
     simpInterpreter s ds
