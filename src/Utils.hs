module Utils where
--------------------------------------------------------------------------------
-- |
-- simple utils
--------------------------------------------------------------------------------


-- |
--
-- >>> listToEither "empty" [1..10]
-- Right 1
--
-- >>> listToEither "empty" []
-- Left "empty"
listToEither :: a -> [b] -> Either a b
listToEither a []    = Left a
listToEither _ (x:_) = Right x




-- |
--
-- >>> maybeToEither "none" (Just "one")
-- Right "one"
--
-- >>> maybeToEither "none" Nothing
-- Left "none"
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither a Nothing  = Left a
maybeToEither _ (Just b) = Right b



-- |
--
--
-- >>> diff [2,4] [1,2,3,4,5]
-- [1,3,5]
diff :: Eq a => [a] -> [a] -> [a]
diff toRemove = filter (not . flip elem toRemove)


-- |
--
-- >>> diffBy (\a b -> a == show b) ["2","4"] [1..10]
-- [1,3,5,6,7,8,9,10]
diffBy :: Eq b => (a -> b -> Bool) -> [a] -> [b] -> [b]
diffBy f toRemove = filter (\b -> not $ any (flip f b) toRemove)

