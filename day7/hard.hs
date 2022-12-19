import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Path = [String]

diskSize, requiredSpace :: Int
diskSize = 70000000
requiredSpace = 30000000

processList :: [String] -> (Int, [String], [String])
processList [] = (0, [], [])
processList (l:ls) =
  let (restSize, restChildren, restInp) = processList ls
   in case words l of
        "$":_ -> (0, [], l:ls)
        "dir":dirName:[] -> (restSize, dirName : restChildren, restInp)
        size:_ -> ((read size) + restSize, restChildren, restInp)
        i -> error (show i)


-- take the map only containing the sum of sizes of files
-- directly contained in each directory and returns the
-- full size of the subtree
dfs :: M.Map Path (Int, [String]) -> Path -> M.Map Path Int
dfs m cwd =
  let (currSize, children) = fromJust $ M.lookup cwd m
      children' = map (:cwd) children
      rec' = map (dfs m) children'
      rec = foldr M.union M.empty rec'
      fullSize = foldr (\x xs -> fromJust (M.lookup x rec) + xs) currSize children'
   in M.insert cwd fullSize rec

solve :: [String] -> Int
solve inp =
  let info = go M.empty [] inp
      folderSizes = dfs info []
      rootSize = fromJust (M.lookup [] folderSizes)
      freeSpace = diskSize - rootSize
      requiredRemoval = requiredSpace - freeSpace
      answer = foldr (\x xs -> if x >= requiredRemoval && x <= xs then x else xs) rootSize folderSizes
   in answer
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
