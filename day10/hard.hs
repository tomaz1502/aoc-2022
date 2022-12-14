chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf i xs = take i xs : chunksOf i (drop i xs)

getPos :: [String] -> [Int]
getPos ss = 1 : go 1 ss
  where go _ [] = []
        go val (inst:insts) =
          case words inst of
            ["noop"] -> val : go val insts
            ["addx", amt] ->
                 val : val + read amt : go (val + read amt) insts
            _ -> error "unknown instruction"

solve :: [Int] -> String
solve = go 0
  where go _ [] = ""
        go i (p:ps)
          | abs (i - p) <= 1 = '#' : go ((i + 1) `mod` 40) ps
          | otherwise = '.' : go ((i + 1) `mod` 40) ps

main :: IO ()
main = interact (unlines . chunksOf 40 . solve . getPos . lines)
  where report i = show i <> "\n"
