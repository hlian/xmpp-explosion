import Network (PortID (PortNumber))
import Network.Protocol.Explosion (telnet)
import System.Environment (getArgs, getProgName)
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [host, port] -> telnet host $ PortNumber $ fromInteger $ (read port :: Integer)
    _ -> usageExit
  where
    usageExit = do
         name <- getProgName
         putStrLn $ "Usage : " ++ name ++ " host port"
