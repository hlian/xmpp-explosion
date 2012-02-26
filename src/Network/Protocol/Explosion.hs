module Network.Protocol.Explosion where

import Control.Concurrent (forkIO, killThread)
import Control.Exception (try)
import Control.Exception.Base (throwIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (release, runResourceT, with)
import qualified Crypto.Random.AESCtr as AESCtr
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Data.Conduit
import Data.Conduit.Binary (sinkHandle, sourceHandle)
import Debug.Trace
import Network (PortID (..))
import qualified Network
import qualified Network.TLS as TLS
import Network.TLS.Extra (ciphersuite_all)
import System.IO

data Connection = Connection {
  connHandle :: Handle,
  connSource :: Source IO ByteString,
  connSink :: Sink ByteString IO ()
  }

isClosed conn = hIsEOF $ connHandle conn

tlsParams = TLS.defaultParams { TLS.pCiphers = ciphersuite_all }

tee :: Monad m => m a -> (a -> m ()) -> m a
tee obj f = do
  x <- obj
  f x
  return x

noBuffer = flip hSetBuffering $ NoBuffering

connectTo host port =
    Network.connectTo host (PortNumber $ fromIntegral port) `tee` noBuffer

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

securePlumbing :: String -> Int -> ResourceT IO Connection
securePlumbing host port = do
  prg <- liftIO $ AESCtr.makeSystem
  (releaseHandle, handle) <- with (connectTo host port) hClose
  (releaseContext, context) <- with (TLS.client tlsParams prg handle) TLS.bye
  TLS.handshake context
  return $ Connection handle
    (source $ TLS.recvData context)
    (sink $ TLS.sendData context . L.fromChunks . (:[]))

plumbing :: String -> Int -> ResourceT IO Connection
plumbing host port =  do
  (releaseHandle, handle) <- with (connectTo host port) hClose
  return $ Connection handle
    (source $ B.hGet handle 4096)
    (sink $ B.hPutStr handle)

readLine handle = liftIO $ do
  eof <- hIsEOF handle
  if eof then return IOClosed else fmap IOOpen $ hGetLine handle

stdinSource = Source pull close
    where
      pull = liftIO $ do
        line <- hGetLine stdin
        if line == "send\n" then return Closed else return $ Open stdinSource (BC.pack line)
      close = return ()

telnet' :: Connection -> ResourceT IO ()
telnet' conn  = do
  trace "read" $ return ()
  stdinSource $$ connSink conn
  trace "write" $ return ()
  connSource conn $$ sinkHandle stdout
  telnet' conn

telnet host port =
    runResourceT $ do
      conn <- (if port == 443 then securePlumbing else plumbing) host port
      liftIO $ mapM_ noBuffer [stdin, stdout]
      telnet' conn
