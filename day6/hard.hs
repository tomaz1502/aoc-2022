import qualified Data.Set as S

allDistinct :: (Eq a, Ord a) => [a] -> Bool
allDistinct xs =
    S.size (S.fromList xs) == length xs

solve :: String -> Int
solve = go 0
  where go :: Int -> String -> Int
        go i l@(x:xs) =
          if allDistinct (take 14 l) then
            i + 14
          else
            go (i + 1) xs
        go _ _ = error "not found"

main :: IO ()
main = interact $ report . solve
  where report i = show i <> "\n"
