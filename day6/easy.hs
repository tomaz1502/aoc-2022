import qualified Data.Set as S

allDistinct :: (Eq a, Ord a) => a -> a -> a -> a -> Bool
allDistinct a b c d =
    S.size (S.fromList [a,b,c,d]) == 4

solve :: String -> Int
solve = go 0
  where go :: Int -> String -> Int
        go i (a:b:c:d:r) =
          if allDistinct a b c d then
            i + 4
          else
            go (i + 1) (b:c:d:r)
        go _ _ = error "not found"

main :: IO ()
main = interact $ report . solve
  where report i = show i <> "\n"
