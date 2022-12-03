import Data.Char (ord, isLower)
import Data.List (splitAt, find)
import qualified Data.Set as S

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = take i xs : chunksOf i (drop i xs)

val :: Char -> Int
val c =
  if isLower c then
    ord c - ord 'a' + 1
  else
    ord c - ord 'A' + 27

getRepeated :: (Eq a, Ord a) => [[a]] -> a
getRepeated xs@[r1,r2,r3] =
  let [s1,s2,s3] = map S.fromList xs
   in S.elemAt 0 $ s1 `S.intersection` s2 `S.intersection` s3

main :: IO ()
main = interact $ report . sum . map (val . getRepeated) . chunksOf 3 . lines
  where report i = show i <> "\n"
