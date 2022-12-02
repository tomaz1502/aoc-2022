import Data.Char (ord)

data Move = Rock | Paper | Scissors
  deriving Eq

toMove :: Char -> Move
toMove 'A' = Rock 
toMove 'B' = Paper 
toMove 'C' = Scissors 
toMove _   = error "invalid input"

val :: Move -> Int
val Rock     = 1
val Paper    = 2
val Scissors = 3

winsFrom :: Move -> Move
winsFrom Rock     = Paper
winsFrom Paper    = Scissors
winsFrom Scissors = Rock

losesFrom :: Move -> Move
losesFrom Rock     = Scissors
losesFrom Paper    = Rock
losesFrom Scissors = Paper

-- 0 if first loses, 1 if it's a tie, 2 if first wins
result :: Move -> Move -> Int
result a x
 | a == x           = 1
 | winsFrom a == x  = 0
 | otherwise        = 2

score :: Move -> Move -> Int
score a x = 3 * (result x a) + val x

getMoves :: String -> (Move, Move)
getMoves [a, ' ', x] =
  let f = case x of
            'X' -> losesFrom
            'Y' -> id
            'Z' -> winsFrom
      m = toMove a
   in (m, f m)
getMoves _ = error "invalid input"

main :: IO ()
main = interact $ report . sum . map (uncurry score . getMoves) . lines
  where report i = show i <> "\n"
