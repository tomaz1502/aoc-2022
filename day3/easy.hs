import Data.Char (ord, isLower)
import Data.List (splitAt, find)

val :: Char -> Int
val c =
  if isLower c then
    ord c - ord 'a' + 1
  else
    ord c - ord 'A' + 27

getRepeated :: (Eq a) => [a] -> a
getRepeated as =
  let l             = length as
      (left, right) = splitAt (l `div` 2) as
      Just a        = find (`elem` right) left
   in a

main :: IO ()
main = interact $ report . sum . map (val . getRepeated) . lines
  where report i = show i <> "\n"
