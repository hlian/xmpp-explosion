module Network.Protocol.Explosion where

import Control.Concurrent (forkIO, killThread)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, with)
import Data.Conduit
import Data.Conduit.Binary (sinkHandle, sourceHandle)
import Network (connectTo, PortID)
import System.IO

telnet :: String -> PortID -> IO ()
telnet host port =
    runResourceT $ do
      (releaseSocket, socket) <-
          with (connectTo host $ port) hClose
      liftIO $ mapM_ (`hSetBuffering` LineBuffering) [stdin, stdout, socket]
      (releaseThread, _) <-
          with (forkIO $ runResourceT $ sourceHandle stdin $$ sinkHandle socket) killThread
      sourceHandle socket $$ sinkHandle stdout
