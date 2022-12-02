import Data.Char (ord)

data Move = Rock | Paper | Scissors
  deriving Eq

toMove :: Char -> Move
toMove 'A' = Rock 
toMove 'B' = Paper 
toMove 'C' = Scissors 
toMove 'X' = Rock 
toMove 'Y' = Paper 
toMove 'Z' = Scissors 
toMove _   = error "invalid input"

val :: Move -> Int
val Rock     = 1
val Paper    = 2
val Scissors = 3

-- 0 if first loses, 1 if it's a tie, 2 if first wins
result :: Move -> Move -> Int
result a x =
  if a == x then
    1
  else
    case (a, x) of
      (Rock, Paper)     -> 0
      (Rock, Scissors)  -> 2
      (Paper, Rock)     -> 2
      (Paper, Scissors) -> 0
      (Scissors, Rock)  -> 0
      (Scissors, Paper) -> 2

score :: Move -> Move -> Int
score a x = 3 * (result x a) + val x

getMoves :: String -> (Move, Move)
getMoves [a, ' ', x] = (toMove a, toMove x)
getMoves _ = error "invalid input"

main :: IO ()
main = interact $ report . sum . map (uncurry score . getMoves) . lines
  where report i = show i <> "\n"
