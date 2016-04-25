g :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
g _ p [] = return p
g h p (x:xs) =  (h p x) >>= (\a2 -> g h a2 xs)
