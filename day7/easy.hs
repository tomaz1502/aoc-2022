import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Path = [String]

processList :: [String] -> (Int, [String], [String])
processList [] = (0, [], [])
processList (l:ls) =
  let (restSize, restChildren, restInp) = processList ls
   in case words l of
        "$":_ -> (0, [], l:ls)
        "dir":dirName:[] -> (restSize, dirName : restChildren, restInp)
        size:_ -> ((read size) + restSize, restChildren, restInp)
        i -> error (show i)

dfs :: M.Map Path (Int, [String]) -> Path -> [Int]
dfs m cwd =
  let (currSize, children) = fromJust $ M.lookup cwd m
      rec = map (dfs m) (map (:cwd) children)
      mySize = foldr (\x xs -> (head x) + xs) currSize rec
   in mySize : concat rec

solve :: [String] -> Int
solve inp =
  let info = go M.empty [] inp
      folderSizes = dfs info []
   in foldr (\x xs -> if x <= 100000 then x + xs else xs) 0 folderSizes
  where go :: M.Map Path (Int, [String]) -> Path -> [String] -> M.Map Path (Int, [String])
        go m _ [] = m
        go m cwd (l:ls) =
          case words l of
            "$":"cd":["/"]     -> go m [] ls
            "$":"cd":[".."]    -> go m (drop 1 cwd) ls
            "$":"cd":[dirName] -> go m (dirName : cwd) ls
            "$":["ls"] ->
              let (size, children, restInp) = processList ls
               in go (M.insert cwd (size, children) m) cwd restInp
            _ -> error "unexpected command"

main :: IO ()
main = interact (report . solve . lines)
  where report i = show i <> "\n"
