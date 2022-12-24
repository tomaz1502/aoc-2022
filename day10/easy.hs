solve :: [String] -> Int
solve = go 1 1
  where go _ _ [] = 0
        go val tick (inst:insts) =
         let add = if tick `mod` 40 == 20 then val * tick else 0
          in case words inst of
            ["noop"] -> add + go val (tick + 1) insts
            ["addx", amt] ->
                let corner = if tick `mod` 40 == 19 then val * (tick + 1) else 0
                 in corner + add + go (val + read amt) (tick + 2) insts

main :: IO ()
main = interact (report . solve . lines)
  where report i = show i <> "\n"
