module RemoteSend where


import Control.Exception
import Network
import System.IO

main = putStrLn "Hello World"

data SendData a = Data a
                | Err String

data RemoteServ = RS Handle

class Sendable a where
     send :: Hostname -> a -> SendData a
     receive :: RemoteServ -> SendData a

instance (Binary a) => Sendable a where
    send x = 


dataNum :: Word8
dataNum = 218

instance (Binary a) => Binary (Sendable a) where
    put (Data a) = do
        putWord8 dataNum
        put a
    put (Err str) = do
        putWord8 (-1)
        put str
    get = do
        n <- getWord8
        case n of
            dataNum -> liftM Data get
            (-1) -> liftM Err get
            _ -> return $ Err "Error in received data"
        
sndMsg :: Sendable a => a -> Hostname -> IO ()
