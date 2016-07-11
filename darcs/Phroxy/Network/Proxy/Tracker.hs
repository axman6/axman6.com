module Network.Proxy.Tracker where

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Monad
import Data.Binary
import Data.Int
import Data.Map.TernaryMap
import Data.Maybe
import Data.Time.Clock
import Network.Curl
import Network.HTTP
import Network.Stream
import Network.URI
import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Prelude (lookup)
import System.Locale
import System.Posix.Time
import System.Posix.Files
import System.Time
import System.Time.Parse
import Text.Printf



type ObjID = Int64

data ObjInfo = Obj {
        date :: String,
        -- date :: CalendarTime,
        objpath :: FilePath,
        idTag :: ObjID
        }
        | Pending String
        deriving Show

defaultObj = Obj "<Date>" "<FilePath>" 0

data Tracker = Tr (Chan Comm)

type PRLookup = TernaryMap Char ObjInfo
type BSResponse = CurlResponse_ [(String, String)] L8.ByteString

data SaveState = Save PRLookup ObjID

instance Binary ObjInfo where
    put (Obj da pa idt) = put da >> put pa >> put idt
    get = do
        da <- get
        pa <- get
        idt <- get
        return (Obj da pa idt)

instance Binary SaveState where
    put (Save l o) = put l >> put o
    get = liftM2 Save get get

data Comm = Get String (MVar (Maybe ObjInfo))
          | Elem String (MVar Bool)
          | Insert String ObjInfo
          | Fetch String (MVar (Maybe L8.ByteString))
          | ShutDown

objTracker :: Chan Comm -> PRLookup -> ObjID -> IO ()
objTracker reqs tree oid = do
    req <- readChan reqs
    case req of
        Get str var -> putMVar var (tree ! str) >> objTracker reqs tree oid
        Elem str var -> putMVar var (member str tree)
                        >> objTracker reqs (insert str (Pending "blah") tree) oid
        Insert str ele -> objTracker reqs (insert str ele tree) oid
        Fetch str var -> if member str tree
                            then do
                                putStrLn $ "* * * * * * * FOUND OBJECT: " ++ str
                                contents <- L8.readFile . objpath . fromJust . lookup str $ tree
                                putMVar var (Just contents)
                                objTracker reqs tree oid
                            else do
                                forkIO $ fetch var (Tr reqs) str oid
                                objTracker reqs tree (oid+1)
        ShutDown -> do
            encodeFile savefile $ Save tree oid

savefile :: FilePath
savefile = "Phroxysave.dat"

fetch :: MVar (Maybe L8.ByteString) -> Tracker -> String -> ObjID -> IO ()
fetch var tr url oid = do
    resp <- simpleHTTP $ getRequest url -- :: IO (Network.Stream.Result (Response L8.ByteString))
    print resp
    writeFile (show oid) . rspBody . (\(Right x)-> x) $ resp
    putMVar var . Just . L8.pack . rspBody . (\(Right x)-> x) $ resp
    putObj url defaultObj{objpath = show oid} tr
    
-- fetch var tr url oid = withCurlDo $ do
--         case parseURI url of
--                 (Just url') -> do
--                     print url'
--                     (CurlResponse ccode stat statln hdrs bdy info)
--                         <- curlGetResponse_ (uriPath url') [] :: IO BSResponse   
--                     printf "Path: %s stat: %d statln: %s headers: %s\n"
--                         url stat statln . show $ (hdrs::[(String,String)])
--                     case ccode of
--                         CurlOK -> do
--                             -- now <- getCurrentTime
--                             -- let Just now' = parseCalendarTime defaultTimeLocale rfc822DateFormat now
--                             putMVar var (Just bdy)
--                             L8.writeFile (show oid) bdy
--                             case Prelude.lookup "Date" hdrs of
--                                 Just d -> putObj url defaultObj{date = d} tr
--                                         -- {date = parseCalendarTime defaultTimeLocale rfc822DateFormat . tail $ d}
--                                 Nothing -> putObj url defaultObj tr
--                         _ -> putMVar var Nothing >> return ()
--                 _ -> putMVar var Nothing >> return ()
            
            

httpDate :: IO String
httpDate = do t <- toCalendarTime . flip TOD 0
                      . fromIntegral . fromEnum =<< epochTime
              return (formatCalendarTime defaultTimeLocale
                                "%a, %d %b %Y %H:%M:%S GMT" t)



newTracker :: IO Tracker
newTracker = do
    chan <- newChan
    prev <- fileExist savefile
    if prev
        then do
            (Save t i) <- decodeFile savefile
            forkIO $ objTracker chan t i
        else
            forkIO $ objTracker chan empty 0
    return $ Tr chan

getObj :: String -> Tracker -> IO (Maybe ObjInfo)
getObj str (Tr chan) = do
    var <- newEmptyMVar
    writeChan chan (Get str var)
    readMVar var

putObj :: String -> ObjInfo -> Tracker -> IO ()
putObj str obj (Tr chan) = do
    writeChan chan (Insert str obj)

checkObj :: String -> Tracker -> IO Bool
checkObj str (Tr chan) = do
    var <- newEmptyMVar
    writeChan chan (Elem str var)
    readMVar var

fetchUrl :: String -> Tracker -> IO (Maybe L8.ByteString)
fetchUrl str (Tr chan) = do
    var <- newEmptyMVar
    writeChan chan (Fetch str var)
    readMVar var
    
shutdown :: Tracker -> IO ()
shutdown (Tr chan) = writeChan chan ShutDown



    