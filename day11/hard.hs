{-# LANGUAGE RecordWildCards #-}

import Text.Read (readMaybe)

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ a = a
applyN i f a = applyN (i - 1) f (f a)

getTwoLargest :: (Ord a, Bounded a) => [a] -> (a, a)
getTwoLargest = go minBound minBound
  where go :: (Ord a, Bounded a) => a -> a -> [a] -> (a, a)
        go m1 m2 [] = (m1, m2)
        go m1 m2 (x:xs) =
          case (compare x m1, compare x m2) of
            (GT, _) -> go x m1 xs
            (_, GT) -> go m1 x xs
            (_, _)  -> go m1 m2 xs

data Monkey =
  Monkey { items       :: [Int]
         , op          :: Int -> Int
         , testMod     :: Int -- keep just the number to take the mod
         , trueBranch  :: Int
         , falseBranch :: Int
         , score       :: Int
         }

showMonkey :: Monkey -> String
showMonkey Monkey{..} =
  unlines [ show items
          , show (op 10)
          , show testMod
          , show trueBranch
          , show falseBranch
          ]

readItems :: String -> [Int]
readItems s =
  let list = drop 2 $ words s
      list' = map init list
   in map read (init list' ++ [last list])

readOperation :: String -> Int -> Int
readOperation s old =
  let [x, op, y] = drop 3 (words s)
   in case op of
        "+" -> get x old + get y old
        "*" -> get x old * get y old
  where get s def =
          case readMaybe s of
            Nothing -> def
            Just i  -> i

readTest :: String -> Int
readTest s = read (last (words s))

readBranch :: String -> Int
readBranch s = read (last (words s))

readMonkey :: [String] -> (Monkey, [String])
readMonkey (_ : itS : opS : testS : trS : faS : r) =
    let op          = readOperation opS
        items       = readItems itS
        test        = readTest testS
        trueBranch  = readBranch trS
        falseBranch = readBranch faS
        monkey      = Monkey items op test trueBranch falseBranch 0
   in (monkey, r)
readMonkey _ = error "unexpected input"

readMonkeys :: [String] -> [Monkey]
readMonkeys []  = []
readMonkeys inp =
  let (m, inp') = readMonkey inp
      rest      = readMonkeys inp'
   in m : rest

chooseTargets :: Int -> Int -> Monkey -> [(Int, Int, Int)]
chooseTargets prod idx Monkey{..} = map run items
  where run w =
          let w' = op w `mod` prod
           in if w' `mod` testMod == 0
              then (trueBranch, idx, w')
              else (falseBranch, idx, w')

updateSource :: [Monkey] -> (Int, Int, Int) -> [Monkey]
updateSource ms (_, source, _) =
  let pref     = take source ms
      suf      = drop (source + 1) ms
      mSource  = ms !! source
      score'   = score mSource + 1
      items'   = items mSource
      mSource' = mSource { score = score'
                         , items = tail items'
                         }
   in pref ++ [mSource'] ++ suf

updateTarget :: [Monkey] -> (Int, Int, Int) -> [Monkey]
updateTarget ms (target, _, w) =
  let pref     = take target ms
      suf      = drop (target + 1) ms
      mTarget  = ms !! target
      items'   = items mTarget
      mTarget' = mTarget { items = items' ++ [w] }
   in pref ++ [mTarget'] ++ suf

updateMonkeys :: [Monkey] -> (Int, Int, Int) -> [Monkey]
updateMonkeys ms info = updateSource (updateTarget ms info) info

runRound :: Int -> [Monkey] -> [Monkey]
runRound prod = go 0
  where go i ms
          | i < length ms =
            let m = ms !! i
                ms' = foldl updateMonkeys ms (chooseTargets prod i m)
             in go (i + 1) ms'
          | otherwise = ms

solve :: [String] -> Integer
solve inp =
  let ms = readMonkeys inp
      prod = product (map testMod ms)
      ms' = applyN 10000 (runRound prod) ms
      scores = map score ms'
      (a, b) = getTwoLargest scores
   in (toInteger a) * (toInteger b)

main :: IO ()
main = interact (report . solve . filter (not . null) . lines)
  where report i = show i <> "\n"
