getPos :: [String] -> [Int]
getPos ss = 1 : go 1 ss
  where go _ [] = []
        go val (inst:insts) =
          case words inst of
            ["noop"] -> val : go val insts
            ["addx", amt] ->
                 val : val + read amt : go (val + read amt) insts
            _ -> error "unknown instruction"

solve :: [Int] -> Int
solve pos =
  let indexed = zip [1..] pos
      interesting = filter (\(p, _) -> p `mod` 40 == 20) indexed
   in foldr (\(p, v) acc -> p * v + acc) 0 interesting

main :: IO ()
main = interact (report . solve . getPos . lines)
  where report i = show i <> "\n"
