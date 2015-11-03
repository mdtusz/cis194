module Golf where

-- Q1
skipBy :: Integral a => a -> [b] -> [b]
skipBy n = map snd . filter (\x -> mod (fst x) n == 0 ) . zip [1..]

skips :: [a] -> [[a]]
skips xs = zipWith skipBy [1..] $ replicate (length xs) xs 


-- Q2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
  | y > x && y > z = y : localMaxima (y:z:xs)
  | otherwise = localMaxima (y:z:xs)
localMaxima _ = []


-- Q3
legend = "==========\n0123456789\n"

distribution xs = map (\n -> length $ filter (== n) xs) [0..9]

histogram xs = unlines (map (line c) [m + 1, m..1]) ++ legend
  where c = distribution xs
        m = maximum c

line xs n = [if i >= n then '*' else ' ' | i <- xs]

