readElf :: [String] -> ([Int], [String])
readElf [] = ([], [])
readElf (l:ls) =
  case l of
    "" -> ([], ls)
    n' ->
      let (xs, ls') = readElf ls
       in ((read n') : xs, ls')

readElves :: [String] -> [[Int]]
readElves [] = []
readElves inp =
  let (xs, ls) = readElf inp
      rest     = readElves ls
   in xs : rest

getThreeMax :: [Int] -> (Int, Int, Int)
getThreeMax = go 0 0 0
  where go :: Int -> Int -> Int -> [Int] -> (Int, Int, Int)
        go a b c [] = (a,b,c)
        go a b c (x:xs) =
          if x >= a then
            go x a b xs
          else if x >= b then
            go a x b xs
          else if x >= c then
            go a b x xs
          else
            go a b c xs

main :: IO ()
main = interact $ report . sumThree . getThreeMax . map sum . readElves . lines
  where report i = show i <> "\n"
        sumThree (a,b,c) = a + b + c

