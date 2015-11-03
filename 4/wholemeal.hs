fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

--fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate f
  where f n = if even n then n `div` 2 else 3 * n + 1

data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode x Leaf = Node 0 Leaf x Leaf
addNode x (Node h lt n rt)
  | treeHeight lt > treeHeight rt = Node nh lt n (addNode x rt)
  | otherwise = Node nh (addNode x lt) n rt
  where nh = 1 + max (treeHeight lt) (treeHeight rt)

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = odd . length . filter (\x -> x)

xor' :: [Bool] -> Bool
xor' = foldr (\x y -> x /= y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

-- sieveSundaram :: Integer -> [Integer]
sieveSundaram n = 2:[2*x+1 | x <- [1..n], not (x `elem` sieve), 2*x+1 <= n]
  where sieve = [i+j+2*i*j | i <- [1..n], j <- [i..n]]