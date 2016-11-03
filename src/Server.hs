module Server where

import Data.Map.Strict as Map

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forever, when, unless)

import Network

import System.Console.GetOpt
import System.Environment (getProgName)
import System.Exit
import System.IO

import Helpers
import Protocol
import Room
import User

data Opts = Opts { port :: PortID } deriving Show

defaultOpts :: Opts
defaultOpts = Opts { port = PortNumber 55555 }

optDescrs :: [OptDescr (Opts -> IO Opts)]
optDescrs = [ Option ['p'] ["port"]
                  (ReqArg (\arg opts -> return opts { port = PortNumber $ read arg }) "PORT")
                  "port to bind to"
            , Option ['h'] ["help"]
                  (NoArg (\_ -> do
                      progName <- getProgName
                      let header = "Usage: " ++ progName ++ " [OPTIONS]"
                      putStrLn $ usageInfo header optDescrs
                      exitSuccess))
                  "show this help message"
            ]

main :: IO ()
main = mainHelper optDescrs defaultOpts $ const server

initStores :: STM (UserMap, RoomMap)
initStores = do
    userMap <- newTVar Map.empty
    roomMap <- newTVar Map.empty
    return (userMap, roomMap)

server :: Opts -> IO ()
server opts = withSocketsDo $ do
    putStrLn $ "Listening on " ++ (show . port) opts
    listenSock <- (listenOn . port) opts
    (userMap, roomMap) <- atomically initStores
    forever $ do
        (handle, hostname, port) <- accept listenSock
        hSetNewlineMode handle (NewlineMode CRLF CRLF)
        hSetBuffering handle LineBuffering
        forkIO $ do
            putStrLn $ "Accepted connection from " ++ hostname ++ ":" ++ show port
            loginHandler userMap roomMap handle


loginHandler :: UserMap -> RoomMap -> Handle -> IO ()
loginHandler userMap roomMap handle = do
    messageE <- readMessage handle
    case messageE of
        Left err -> closeWithErrorMessage handle $ Just "fix your client pls kthxbai"
        Right message -> do
            case message of
                Name nick -> do
                    additionOp <- atomically $ addUser nick handle userMap
                    case additionOp of
                        Left err -> closeWithErrorMessage handle $ Just err
                        Right userT -> do
                            writeMessage handle Ok
                            sessionHandler userMap roomMap userT
                _ -> closeWithErrorMessage handle $ Just "Expecting a NAME message right about now"

sessionHandler :: UserMap -> RoomMap -> TVar User -> IO ()
sessionHandler userMap roomMap userT = helper where
    helper = do
        user <- atomically $ readTVar userT
        handle <- return . connection $ user
        connectionClosed <- hIsClosed handle
        unless connectionClosed $ do
            message <- (readMessage handle) `catch` mkExceptionString
            case message of
                Left err -> do
                    putStrLn $ "Error with `" ++ (nick user) ++ "`: " ++ err
                    closeWithErrorMessage handle $ Just err
                    clearUserFromServer user
                    broadcastToAllUsersInServer (Quit' (nick user) (Just "Connection lost")) userMap
                    return ()
                Right msg -> do
                    shouldContinue <- processMessage user msg
                    case shouldContinue of
                        True -> sessionHandler userMap roomMap userT
                        False -> return ()

    mkExceptionString :: IOException -> IO (Either String Message)
    mkExceptionString = return . Left . ("Exception reading from handle: "++) . show

    clearUserFromServer user = atomically $ do
        removeUserFromAllRooms userT roomMap
        removeUser (nick user) userMap
        return ()

    -- Bool is whether or not we should poll again for messages from this client
    processMessage :: User -> Message -> IO Bool
    processMessage user message = case message of
        Name newName -> do
            renameOp <- atomically $ renameUser (nick user) newName userMap
            writeToUser $ case renameOp of
                Left error -> (Err $ Just error)
                Right _    -> Ok
            return True

        Quit reason -> do
            clearUserFromServer user
            closeWithErrorMessage (connection user) $ Just "kthxbai"
            broadcastToAllUsersInServer (Quit' (nick user) reason) userMap
            return False

        Part room reason -> let msg = (Part' room (nick user) reason) in do
                if validateRoomName room then do
                    didRemove <- atomically $ do
                        isInRoom <- userIsInRoom room userT roomMap
                        when isInRoom $ removeUserFromRoom room userT roomMap
                        return isInRoom
                    if didRemove
                        then broadcastToRoom room msg roomMap >> writeToUser msg
                        else writeToUser $ Err $ Just $ "You already appear to not be in " ++ room
                else writeToUser $ Err $ Just $ room ++ " is not a valid room identifier"
                return True

        Join room -> do
            if validateRoomName room then do
                wasInRoom <- atomically $ do
                    wasInRoom <- userIsInRoom room userT roomMap
                    unless wasInRoom $ addUserToRoom room userT roomMap >> return ()
                    return wasInRoom
                if wasInRoom
                    then writeToUser $ Err $ Just $ "You are already in room " ++ room
                    else broadcastToRoom room (Join' room (nick user)) roomMap
            else writeToUser (Err . Just $ "`" ++ room ++ "` is not a valid room identifier")
            return True

        Say dest message -> validateDestAndDispatch dest (Say' dest (nick user) message) >> (return True)
        Act dest action  -> validateDestAndDispatch dest (Act' dest (nick user) action)  >> (return True)

        x -> do
            clearUserFromServer user
            closeWithErrorMessage (connection user) $ Just ("Unexpected message: " ++ encodeMessage x)
            processMessage user (Quit $ Just "tried to break the server") -- to de-dupe the remove-from-server logic

        where
            writeToUser msg = writeMessage (connection user) msg

            quitToPart room reason = Part' room (nick user) $ Just $ maybe "No reason given" ("Quit: "++) reason

            validateDestAndDispatch target msg =
                if validateRoomName target -- todo check if a room exists/user is a member of it before sending?
                then broadcastToRoom target msg roomMap
                else do
                    trySend <- atomically $ sendMessageToUser target msg userMap
                    case trySend of
                        Left error -> writeToUser . Err . Just $ error
                        Right sendAction -> sendAction

closeWithErrorMessage :: Handle -> Maybe String -> IO ()
closeWithErrorMessage handle message = do
    writeMessage handle $ Err message
    hClose handle
