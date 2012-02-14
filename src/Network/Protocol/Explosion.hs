module Network.Protocol.Explosion where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (try)
import Control.Exception.Base (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT, with)
import qualified Crypto.Random.AESCtr as AESCtr
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.Binary (sinkHandle, sourceHandle)
import Network (PortID (..))
import qualified Network
import qualified Network.TLS as TLS
import Network.TLS.Extra (ciphersuite_all)
import System.IO

tlsParams = TLS.defaultParams { TLS.pCiphers = ciphersuite_all }

tee :: Monad m => m a -> (a -> m ()) -> m a
tee obj f = do
  x <- obj
  f x
  return x

lineBuffer = flip hSetBuffering $ LineBuffering

connectTo host port =
    Network.connectTo host (PortNumber $ fromIntegral port) `tee` lineBuffer

source :: IO ByteString -> Source IO ByteString
source recv = Source pull close
    where
      pull = do
        bs <- liftIO $ try recv
        case bs of
          Left TLS.Error_EOF -> return Closed
          Left ex -> liftIO $ throwIO ex
          Right bs | B.null bs -> return Closed
                   | otherwise -> return $ Open(source recv) bs
      close = return ()

sink :: (ByteString -> IO ()) -> Sink ByteString IO ()
sink send = SinkData push close
    where
      push bs = (liftIO $ send bs) >> (return $ Processing push close)
      close = return ()

securePlumbing host port = do
  prg <- liftIO $ AESCtr.makeSystem
  conn <- connectTo host port
  client <- handshake prg conn
  return (source $ TLS.recvData client,
          sink $ TLS.sendData client . L.fromChunks . (:[]))
    where
      handshake prg conn = TLS.client tlsParams prg conn `tee` TLS.handshake

plumbing host port =  do
  conn <- connectTo host port
  return (source $ B.hGet conn 4096, sink $ B.hPutStr conn)

telnet host port =
    runResourceT $ do
      (source, sink) <- liftIO $ (if port == 443 then securePlumbing else plumbing) host port
      liftIO $ mapM_ lineBuffer [stdin, stdout]
      (releaseThread, _) <-
          with (forkIO $ runResourceT $ sourceHandle stdin $$ sink) killThread
      source $$ sinkHandle stdout
