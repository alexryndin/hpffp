-- Write bind in terms of fmap and join.
-- Fear is the mind-killer, friend. You can do it.
import Control.Monad (join)

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
