replace :: a -> Int -> [a] -> [a]
replace a 0 (x:xs) = a : xs
replace a i (x:xs) = x : replace a (i - 1) xs

data Inst =
  Inst { amt  :: Int
       , from :: Int
       , to   :: Int
       }
  deriving Show

readInst :: String -> Inst
readInst s =
    let [_, amt, _, from, _, to] = map read $ words s
     in Inst amt (from - 1) (to - 1)

parsePileLine :: String -> [Char]
parsePileLine [] = []
parsePileLine s =
  let box  = take 4 s
      rest = parsePileLine $ drop 4 s
   in box !! 1 : rest

buildPiles :: [String] -> [[Char]]
buildPiles inp =
  let l = length inp
      emptyPiles = [[] | _ <- [0..l]]
   in foldr pileUp emptyPiles (map parsePileLine inp)
  where pileUp [] [] = []
        pileUp (c:cs) (p:ps) =
          let newPile = if c /= ' ' then c : p
                        else p
           in newPile : pileUp cs ps

run :: [[Char]] -> [Inst] -> [[Char]]
run piles [] = piles
run piles (i:is) =
  let moved = take (amt i) (piles !! from i)
      added = replace (moved ++ (piles !! to i)) (to i) piles
      removed = replace (drop (amt i) (piles !! from i)) (from i) added
   in run removed is

main :: IO ()
main = interact $ showFirst . (\(top, bot) -> run (buildPiles top) (map readInst bot)) . separate . lines
  where separate :: [String] -> ([String], [String])
        separate inp =
          ( init $ takeWhile (not . null) inp
          , tail $ dropWhile (not . null) inp
          )
        showFirst :: [[Char]] -> String
        showFirst = map head
