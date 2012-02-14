import Network (PortID (PortNumber))
import Network.Protocol.Explosion (telnet)
import System.Environment (getArgs, getProgName)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, port] -> telnet host $ (read port :: Int)
    _ -> putStrLn $ "Usage: [prog] host port"
