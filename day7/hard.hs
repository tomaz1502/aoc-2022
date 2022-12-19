import qualified Data.Map as M
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Path = [String]
type FileSystem = M.Map Path (Int, [String])

diskSize, requiredSpace :: Int
diskSize = 70000000
requiredSpace = 30000000

processList :: [String] -> (Int, [String], [String])
processList [] = (0, [], [])
processList list@(item : items) =
  let (restSize, restChildren, restInp) = processList items
   in case words item of
        "$" : _              -> (0, [], list)
        "dir" : dirName : [] -> (restSize, dirName : restChildren, restInp)
        size : _             -> ((read size) + restSize, restChildren, restInp)
        _                    -> error "[processList]: unexpected string"

-- Build the file system represented as a map from a path
-- to it's size and the name of it's chilren. Only computes
-- the size of the files that are directly contained in that
-- path
buildPartialTree :: [String] -> FileSystem
buildPartialTree = go M.empty []
  where go :: FileSystem -> Path -> [String] -> FileSystem
        go acc _ [] = acc
        go acc cwd (cmd : cmds) =
          case words cmd of
            "$" : "cd" : ["/"]     -> go acc [] cmds
            "$" : "cd" : [".."]    -> go acc (drop 1 cwd) cmds
            "$" : "cd" : [dirName] -> go acc (dirName : cwd) cmds
            "$" : ["ls"] ->
              let (size, children, restInp) = processList cmds
               in go (M.insert cwd (size, children) acc) cwd restInp
            _ -> error "[buildPartialTree]: unexpected command"

-- Take the map only containing the sum of sizes of files
-- directly contained in each directory and returns map
-- containing the full size of the subtree
dfs :: FileSystem -> Path -> M.Map Path Int
dfs m cwd =
  let (currSize, children) = fromJust $ M.lookup cwd m
      children' = map (:cwd) children
      rec' = map (dfs m) children'
      rec = M.unions rec'
      getChildSize = \child -> fromJust (M.lookup child rec)
      fullSize = foldr (\ch acc -> getChildSize ch + acc) currSize children'
   in M.insert cwd fullSize rec

solve :: [String] -> Int
solve inp =
  let info = buildPartialTree inp
      folderSizes = dfs info []
      rootSize = fromJust (M.lookup [] folderSizes)
      freeSpace = diskSize - rootSize
      requiredRemoval = requiredSpace - freeSpace
      checkFolder = \fSize currBest ->
        if fSize >= requiredRemoval && fSize <= currBest
        then fSize else currBest
   in foldr checkFolder rootSize folderSizes

main :: IO ()
main = interact (report . solve . lines)
  where report i = show i <> "\n"
