module Client where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception (IOException, catch, finally)
import Control.Monad (forever)
import Data.Maybe (isNothing)

import Network

import System.Console.GetOpt
import qualified System.Console.Readline as RL
import System.Environment (getProgName)
import System.Exit
import System.IO

import Helpers (mainHelper, unwordsToMaybe)
import Protocol
import Room (validateRoomName)

data Opts = Opts {
    host :: Maybe String,
    port :: PortID,
    nick :: Maybe String
} deriving Show

defaultOpts :: Opts
defaultOpts = Opts { host = Nothing, port = PortNumber 55555, nick = Nothing }

optDescrs :: [OptDescr (Opts -> IO Opts)]
optDescrs = [ Option ['n'] ["nick"]
                  (ReqArg (\arg opts -> return opts { nick = Just arg }) "NICK")
                  "required: nickname to use"
            , Option ['a'] ["server"]
                  (ReqArg (\arg opts -> return opts { host = Just arg }) "SERVER")
                  "required: host/IP to connect to"
            , Option ['p'] ["port"]
                  (ReqArg (\arg opts -> return opts { port = PortNumber $ read arg }) "55555")
                  "port on SERVER to connect to, default 55555"
            , Option ['h'] ["help"]
                  (NoArg (\_ -> do
                      progName <- getProgName
                      let header = "Usage: " ++ progName ++ " [OPTIONS]"
                      putStrLn $ usageInfo header optDescrs
                      exitSuccess))
                  "show this help message"
            ]

main :: IO ()
main = mainHelper optDescrs defaultOpts $ \hcf opts ->
        case host opts of
            Nothing -> hcf "--server is required"
            Just host ->
                case (nick opts) of
                    Nothing -> hcf "--nick is required"
                    Just nick -> do
                        rlLock  <- newTVarIO False
                        didQuit <- newTVarIO False
                        client rlLock didQuit host (port opts) nick hcf

-- same as putStrLn, except it prints it above the readline
-- adapted from http://stackoverflow.com/a/15541914/1763937
rlPutStrLn :: TVar Bool -> String -> IO ()
rlPutStrLn needsHack text = (=<<) id $ atomically $ do
    isReading <- readTVar needsHack
    if isReading then return $ do
        savedPoint <- RL.getPoint
        savedLine  <- RL.getLineBuffer
        RL.savePrompt
        RL.setLineBuffer ""
        RL.redisplay
        putStrLn text
        RL.restorePrompt
        RL.setLineBuffer savedLine
        RL.onNewLine
        RL.setPoint savedPoint
        RL.redisplay
    else return $ putStrLn text

client :: TVar Bool -> TVar Bool -> String -> PortID -> String -> (String -> IO ()) -> IO ()
client rlLock didQuit host port nick hcf = client' where
    client' = withSocketsDo $ do
        hSetBuffering stdout NoBuffering -- otherwise it trips up when readline and rlPutStrLn is sep. threads
        handle <- connectTo host port
        hSetNewlineMode handle (NewlineMode CRLF CRLF)
        hSetBuffering handle LineBuffering

        loginSuccess <- tryLogin handle
        case loginSuccess of
            Left error -> hClose handle >> hcf error
            Right _ -> do
                forkIO $ (handleIncoming handle) `catch` markQuit `finally` hClose handle
                (readInput handle Nothing) `catch` markQuit `finally` hClose handle

    markQuit :: IOException -> IO ()
    markQuit _ = atomically $ writeTVar didQuit True


    tryLogin :: Handle -> IO (Either String ())
    tryLogin handle = do
        writeMessage handle (Name nick)
        resp <- readMessage handle
        case resp of
            Right Ok          -> return . Right $ ()
            Right (Err error) -> return . Left  $ "Server responded with an error: " ++ maybe "<no reason>" id error
            Right other       -> return . Left  $ "Got an unexpected response from the server: " ++ show other
            Left protoError   -> return . Left  $ "Got an unexpected response from the server: " ++ protoError

    handleIncoming handle = forever $ do
        read <- readMessage handle
        rlPutStrLn rlLock $ case read of
            Left error -> "Error while processing incoming message: " ++ error
            Right message -> prettifyIncomingMessage message

    readInput handle currentChannel = do
        didDisconnect <- atomically $ readTVar didQuit -- hIsClosed handle causes weird deadlocks????
        if didDisconnect then do
            putStrLn $ "Lost connection. Quitting."
            return ()
        else do
            atomically $ writeTVar rlLock True
            line <- RL.readline $ maybe "[no channel]> " (\c -> "-> "++c++" > ") currentChannel
            atomically $ writeTVar rlLock False
            case line of
                Nothing   -> readInput handle currentChannel
                Just ""   -> readInput handle currentChannel
                Just ('/':command) -> do
                    RL.addHistory $ '/':command
                    newChannel <- handleCommand command handle currentChannel
                    readInput handle newChannel
                Just line -> do
                    RL.addHistory line
                    case currentChannel of
                        Nothing -> noChannelHint
                        Just target -> writeMessage handle (Say target line)
                    readInput handle currentChannel

    noChannelHint = do
        putStrLn $ "You are not currently in a channel or messaging someone."
        putStrLn $ "Hint: /st (user|#channel), /join #channel, OR /(msg|say) USER message"

    joinHelper handle newRoom oldRoom =
        if validateRoomName newRoom then do
          writeMessage handle $ Join newRoom
          return $ Just newRoom
        else do
          rlPutStrLn rlLock $ "`" ++ newRoom ++ "` is not a valid room name. Room names start with a '#'"
          return oldRoom

    quitHelper handle message = do
        atomically $ writeTVar didQuit True
        writeMessage handle $ Quit (unwordsToMaybe message)
        return Nothing

    sayOrActHelper handle target message constructor = do
        writeMessage handle $ constructor target (unwords message)
        return $ Just target

    handleCommand command handle channel = case words command of
        (('/':fakeSlash) : xs) -> -- "//whatever" should be the same as saying "/whatever" to currentChannel
            case channel of
                Nothing -> noChannelHint >> return channel
                Just target -> sayOrActHelper handle target (('/':fakeSlash):xs) Say
        ("st" : target : _) -> return $ Just target
        ("part" : message) -> do
            case channel of
                Nothing -> noChannelHint >> return Nothing
                Just channel -> do
                    writeMessage handle $ Part channel (unwordsToMaybe message)
                    return Nothing
        ("q"    : message)  -> quitHelper handle message
        ("quit" : message)  -> quitHelper handle message
        ("j"    : room : _) -> joinHelper handle room channel
        ("join" : room : _) -> joinHelper handle room channel
        ("nick" : name : _) -> (writeMessage handle $ Name name) >> return channel
        ("say" : target : message) -> sayOrActHelper handle target message Say
        ("msg" : target : message) -> sayOrActHelper handle target message Say
        ("act" : message) -> maybe (noChannelHint >> return channel) (\c -> sayOrActHelper handle c message Act) channel
        ("me"  : message) -> maybe (noChannelHint >> return channel) (\c -> sayOrActHelper handle c message Act) channel
        fallback -> do
            rlPutStrLn rlLock $ "Not sure what to do with command: /" ++ unwords fallback
            return channel

    -- todo should probably add some padding esp. for Say' and Act'
    prettifyIncomingMessage (Ok                   ) = ">>> SERVER: OK"
    prettifyIncomingMessage (Err msg              ) = ">>> SERVER: ERROR: " ++ maybe "<no message>" id msg
    prettifyIncomingMessage (Join' room actor     ) = actor ++ " has joined " ++ room
    prettifyIncomingMessage (Part' room actor msg ) = actor ++ " has left " ++ room ++ maybe "" (\m -> " ("++m++")") msg
    prettifyIncomingMessage (Quit' actor msg      ) = actor ++ " has left the server" ++ maybe "" (\m -> " ("++m++")") msg
    prettifyIncomingMessage (Say' origin actor msg) = origin ++ " :: <" ++ actor ++ "> " ++ msg
    prettifyIncomingMessage (Act' origin actor msg) = origin ++ " :: * " ++ actor ++ " " ++ msg
    prettifyIncomingMessage (msg                  ) = ">>> SERVER: " ++ show msg
