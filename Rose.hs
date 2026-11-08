-- Like C #include or else it would be default Main which we don't want
module Rose where

data Rose a = Node a [Rose a] deriving Show

example :: Rose Int
example = Node 1 [ Node 2 [Node 5 [], Node 6 []], Node 3 [], Node 4 [Node 7 [Node 8 []]]]

size :: Rose a -> Int
size (Node _ xs) = 1 + sum [size x | x <- xs]

height :: Rose a -> Int
height (Node _ []) = 0
height (Node _ xs) = maximum [1 + height x | x <- xs]

leaves :: Rose a -> [a]
leaves (Node val []) = [val]
leaves (Node _ xs) = concat [leaves x | x <- xs]

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node val _) = [val]
elemsOnDepth d (Node _ xs) = concat [elemsOnDepth (d - 1) x | x <- xs]

instance Functor Rose where
    fmap :: (a -> b) -> Rose a -> Rose b
    fmap f (Node val xs) = Node (f val) [fmap f x | x <- xs]

foldRose :: (b -> a -> b) -> b -> Rose a -> b
foldRose f acc (Node val xs) = foldl g (f acc val) xs
                             where
                                g = foldRose f
