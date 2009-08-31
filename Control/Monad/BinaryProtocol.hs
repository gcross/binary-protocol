-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.BinaryProtocol
-- Copyright   :  (c) Gregory Crosswhite
-- License     :  BSD-style
-- 
-- Maintainer  :  gcrosswhite@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Monad to ease writing a binary network protocol.
--
-----------------------------------------------------------------------------

module Control.Monad.BinaryProtocol where

import Control.Monad.State
import Data.Binary (Binary)
import qualified Data.Binary as B
import Data.Binary.Get (runGetState)
import qualified Data.ByteString.Lazy as L
import System.IO

type BinaryProtocol = StateT (Handle,Handle,L.ByteString) IO

-- | Take a BinaryProtocol monad and run it on the given file handle.
-- | PLEASE NOTE: The handle is essentially consumed by this function, so it should not be used afterward.
runProtocol :: BinaryProtocol a -> Handle -> Handle -> IO a
runProtocol protocol read_handle write_handle = do
    input <- L.hGetContents read_handle
    result <- evalStateT protocol (read_handle,write_handle,input)
-- Note that we deliberately do NOT close the read_handle since result is lazy and hence might
-- need to read more data from the read_handle at a later point.  It will be closed automatically
-- on this side anyway once all of the data has been read.
    if (read_handle /= write_handle)
      then hClose write_handle
      else hFlush write_handle
    return result

-- | Read in a value of type 'a' from the connection;  'a' must be an instance of the @Binary@ class.
receive :: Binary a => BinaryProtocol a
receive = do
    (read_handle,write_handle,input) <- get
    let (value,remaining_input,_) = runGetState B.get input 0
    put (read_handle,write_handle,remaining_input)
    return value

-- | Send a value of type 'a' down the connection;  'a' must be an instance of the @Binary@ class.
send :: Binary a => a -> BinaryProtocol ()
send value = do
    (_,write_handle,_) <- get
    liftIO $ L.hPut write_handle (B.encode value)

-- | Flush the connection.
flush :: BinaryProtocol ()
flush = do
    (_,write_handle,_) <- get
    liftIO . hFlush $ write_handle
