module ToDo where

import qualified Control.Monad.Par.IO    as P
import qualified Control.Monad.Par.Class as P
import           Control.Monad.IO.Class (liftIO)

-- Chunk list and perform chunks in parallel
{-# INLINE toDoList #-}
toDoList :: Int -> [IO a] -> IO [a]
toDoList chunkSize = fmap concat . P.runParIO . inPar . fmap liftIO 
    where
    inPar [] = return []
    inPar xs = do
        let (chunk,rest) = splitAt chunkSize xs
        var <- P.spawn_ (sequence chunk)
        ys  <- inPar rest
        y   <- P.get var
        return (y:ys)