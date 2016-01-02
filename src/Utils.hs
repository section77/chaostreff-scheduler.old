{-# OPTIONS_HADDOCK ignore-exports #-}
--------------------------------------------------------------------------------
-- |
-- simple utils
--------------------------------------------------------------------------------
module Utils where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT (..), throwE)
import           Types

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



-- | lift an (Either AppError a) into (App a)
--
liftEither :: Either AppError a -> ExceptT AppError IO a
liftEither (Left e) = throwE e
liftEither (Right a) = lift $ return a
