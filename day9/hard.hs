{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State.Lazy (State, runState, sequence, state)
import qualified Data.Set as S

data Context =
  Context { ih  :: Int
          , jh  :: Int
          , tailCoords :: [(Int, Int)]
          , seenPos :: S.Set (Int, Int)
          }

data Dir = L | U | R | D
  deriving Read

data Move =
    Move { dir :: Dir
         , amt :: Int
         }

rec :: [(Int, Int)] -> [(Int, Int)]
rec [] = []
rec [x] = [x]
rec (p1:p2:t) =
  let p2' = updateTail p1 p2
   in p1 : rec (p2':t)

updateHead :: Dir -> (Int, Int) -> (Int, Int)
updateHead dir (ih, jh) =
  case dir of
    L -> (ih, jh - 1)
    U -> (ih + 1, jh)
    R -> (ih, jh + 1)
    D -> (ih - 1, jh)

updateTail :: (Int, Int) -> (Int, Int) -> (Int, Int)
updateTail (i', j') (it, jt) 
  | abs (it - i') <= 1 && abs (jt - j') <= 1 = (it, jt)
  | otherwise =
    let it' = if abs (it - i') > 1 then (it + i') `div` 2 else i'
        jt' = if abs (jt - j') > 1 then (jt + j') `div` 2 else j'
     in (it', jt')

runMove :: Move -> State Context ()
runMove Move{..} 
  | amt == 0 = pure ()
  | otherwise = state $ \Context{..} ->
  let (ih', jh') = updateHead dir (ih, jh)
      (h:t) = tailCoords
      t1 = updateTail (ih', jh') h
      tailCoords' = rec (t1 : t)
      seenPos' = S.insert (last tailCoords') seenPos
   in runState (runMove (Move dir (amt - 1))) (Context ih' jh' tailCoords' seenPos')

runMoves :: [Move] -> State Context ()
runMoves = sequence_ . map runMove

-- solve :: [Move] -> Int
solve ms =
  let tailCoord = [(0,0) | _ <- [1..9]]
      initialState = Context 0 0 tailCoord (S.singleton (0,0))
   in S.size $ seenPos $ snd $ runState (runMoves ms) initialState

parseInput :: [String] -> [Move]
parseInput =
  map $ \s -> let [dir, amt] = words s
               in Move (read dir) (read amt)

main :: IO ()
main = interact (report . solve . parseInput . lines)
  where report i = show i <> "\n"
