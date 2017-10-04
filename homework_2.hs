_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
_zipWith zipFunc (x:xs) (y:ys)
 = zipFunc x y : _zipWith zipFunc xs ys
_zipWith _ _ [] = []
_zipWith _ [] _ = []

_flip :: (a -> a -> b) -> a -> a -> b
_flip flipFunc arg1 arg2 = flipFunc arg2 arg1

_map :: (a -> b) -> [a] -> [b]
_map mapFunc (x:xs) = mapFunc x : _map mapFunc xs
_map _ [] = []

_filter :: (a -> Bool) -> [a] -> [a]
_filter filterFunc (x:xs)
                        | filterFunc x = x : _filter filterFunc xs 
                        | otherwise = _filter filterFunc xs
_filter _ [] = []