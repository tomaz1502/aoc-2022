data Range = MkRange Int Int

readRange :: String -> Range
readRange s =
  let (s1, s2') = span (/= '-') s
      s2 = tail s2'
   in MkRange (read s1) (read s2)

readPair :: String -> (Range, Range)
readPair s =
  let (s1, s2') = span (/= ',') s
      s2 = tail s2'
   in (readRange s1, readRange s2)

containedIn :: Range -> Range -> Bool
containedIn (MkRange i1 j1) (MkRange i2 j2) =
  i1 <= i2 && j1 >= j2

badPair :: Range -> Range -> Bool
badPair r1 r2 = r1 `containedIn` r2 || r2 `containedIn` r1

main :: IO ()
main = interact $
  report . length . filter (uncurry badPair) . map readPair . lines
  where report i = show i <> "\n"




