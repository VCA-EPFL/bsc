module IOMutVar(MutableVar, newVar, readVar, writeVar) where

-- import Data.IORef
import Control.Concurrent.Extra(newVar, readVar, writeVar, Var)

type MutableVar a = Var a

-- newVar :: a -> IO (IORef a)
-- newVar = new

-- readVar :: IORef a -> IO a
-- readVar = readIORef

-- writeVar :: IORef a -> a -> IO ()
-- writeVar = writeIORef
