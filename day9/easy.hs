{-# LANGUAGE RecordWildCards #-}

import Control.Monad.State.Lazy (State, runState, sequence, state)
import qualified Data.Set as S

data Context =
  Context { ih :: Int
          , jh :: Int
          , it :: Int
          , jt :: Int
          , seenPos :: S.Set (Int, Int)
          }

data Dir = L | U | R | D
  deriving Read

data Move =
    Move { dir :: Dir
         , amt :: Int
         }

runMove :: Move -> State Context ()
runMove Move{..} 
  | amt == 0 = pure ()
  | otherwise = state $ \Context{..} ->
  let (it', jt') = case dir of
            L -> (it, jt - 1)
            U -> (it + 1, jt)
            R -> (it, jt + 1)
            D -> (it - 1, jt)
      (ih', jh') =
        if abs (ih - it') <= 1 && abs (jh - jt') <= 1 then (ih, jh)
        else if abs (ih - it') > 1 then ((ih + it') `div` 2, jt')
             else (it', (jh + jt') `div` 2)
      seenPos' = S.insert (ih', jh') seenPos
   in runState (runMove (Move dir (amt - 1))) (Context ih' jh' it' jt' seenPos')

runMoves :: [Move] -> State Context ()
runMoves = sequence_ . map runMove

solve :: [Move] -> Int
solve ms =
  S.size $ seenPos $ snd $ runState (runMoves ms) (Context 0 0 0 0 (S.singleton (0,0)))

parseInput :: [String] -> [Move]
parseInput =
  map $ \s -> let [dir, amt] = words s
               in Move (read dir) (read amt)

main :: IO ()
main = interact (report . solve . parseInput . lines)
  where report i = show i <> "\n"
