import Data.Char (ord)

type Stack a = [a]

-- OK
-- @return (original value, distance to next >=)
buildSufDistances :: [Int] -> [Int]
buildSufDistances [] = []
buildSufDistances arr@(a:as) =
  let n = length arr
      (a':as') = reverse arr
   in reverse $ 0 : go n (n - 2) [(a', n - 1)] as'
  where go :: Int -> Int -> Stack (Int, Int) -> [Int] -> [Int]
        go _ _ stk [] = []
        go n i [] (a:as) = (n - i - 1) : go n (i - 1) [(a, i)] as
        go n i stk@((topVal, topIdx):stk') (a:as)
          | a > topVal = go n i stk' (a:as)
          | otherwise = (topIdx - i) : go n (i - 1) ((a, i):stk) as

buildPrefDistances :: [Int] -> [Int]
buildPrefDistances = reverse . buildSufDistances . reverse

-- assumes every list have the same length
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
  | null (head xs) = []
  | otherwise =
      let top = map head xs
          bot = map tail xs
       in top : transpose bot

go :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int
go [] [] [] [] = 0
go ([]:m1) ([]:m2) ([]:m3) ([]:m4) = go m1 m2 m3 m4
go ((a:as):as') ((b:bs):bs') ((c:cs):cs') ((d:ds):ds') =
  max (a * b * c * d) (go (as:as') (bs:bs') (cs:cs') (ds:ds'))
go _ _ _ _ = error "[go]: invalid input"

solve :: [[Int]] -> Int
solve mat =
  let m1   = map buildPrefDistances mat
      m2   = map buildSufDistances mat
      mat' = transpose mat
      m3'  = map buildPrefDistances mat'
      m4'  = map buildSufDistances mat'
      m3   = transpose m3'
      m4   = transpose m4'
   in go m1 m2 m3 m4

parseInput :: [String] -> [[Int]]
parseInput [] = []
parseInput (l:ls) =
  map (\x -> ord x - ord '0') l : parseInput ls

main :: IO ()
main = interact (report . solve . parseInput . lines)
  where report i = show i <> "\n"
