data Range =
    Range { low  :: Int
          , high :: Int
          }

readRange :: String -> Range
readRange s =
  let (s1, s2') = span (/= '-') s
      s2 = tail s2'
   in Range (read s1) (read s2)

readPair :: String -> (Range, Range)
readPair s =
  let (s1, s2') = span (/= ',') s
      s2 = tail s2'
   in (readRange s1, readRange s2)

-- first range starts first and ends after the second one starts
intersect :: Range -> Range -> Bool
intersect r1 r2 =
  low r1 <= low r2 && high r1 >= low r2

badPair :: Range -> Range -> Bool
badPair r1 r2 = r1 `intersect` r2 || r2 `intersect` r1

main :: IO ()
main = interact $
  report . length . filter (uncurry badPair) . map readPair . lines
  where report i = show i <> "\n"





