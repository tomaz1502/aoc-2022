import Data.Char (ord)

-- posicao i = maximum [i + 1 .. ]
buildSuffixMax :: (Num a, Ord a) => [a] -> [a]
buildSuffixMax [] = []
buildSuffixMax [x] = [-1]
buildSuffixMax (_:x2:xs) =
  case buildSuffixMax (x2:xs) of
    []   -> error "unreacheable"
    y:ys -> (max x2 y) : y : ys

buildPreffixMax :: (Num a, Ord a) => [a] -> [a]
buildPreffixMax = reverse . buildSuffixMax . reverse

-- assumes every list have the same length
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose xs
  | null (head xs) = []
  | otherwise =
      let top = map head xs
          bot = map tail xs
       in top : transpose bot

go :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int
go [] [] [] [] [] = 0
go ([]:mat) ([]:m1) ([]:m2) ([]:m3) ([]:m4) = go mat m1 m2 m3 m4
go ((x:xs):xs') ((a:as):as') ((b:bs):bs') ((c:cs):cs') ((d:ds):ds') =
  let cellVal =
        if x > minimum [a,b,c,d] then 1 else 0
   in cellVal + go (xs:xs') (as:as') (bs:bs') (cs:cs') (ds:ds')
go _ _ _ _ _ = error "[go]: invalid input"

-- solve :: [[Int]] -> Int
solve mat =
  let m1   = map buildPreffixMax mat
      m2   = map buildSuffixMax mat
      mat' = transpose mat
      m3'  = map buildPreffixMax mat'
      m4'  = map buildSuffixMax mat'
      m3   = transpose m3'
      m4   = transpose m4'
   in go mat m1 m2 m3 m4

parseInput :: [String] -> [[Int]]
parseInput [] = []
parseInput (l:ls) =
  map (\x -> ord x - ord '0') l : parseInput ls

main :: IO ()
main = interact (report . solve . parseInput . lines)
  where report i = show i <> "\n"
