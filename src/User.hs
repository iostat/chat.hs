module User ( User(..)
            , UserMap
            , addUser
            , renameUser
            , removeUser
            , userExists
            , sendMessageToUser
            , broadcastToAllUsersInServer
            ) where

import Control.Concurrent.STM
import Control.Monad (liftM, (=<<))

import Data.Map.Strict as Map hiding (foldl)
import Data.Maybe (isJust)

import System.IO

import Protocol

data User = User {
    nick :: String,
    connection :: Handle,
    rooms :: [String]
}

type UserMap = TVar (Map String (TVar User))

addUser :: String -> Handle -> UserMap -> STM (Either String (TVar User))
addUser nick connection userMap =
    if not (validateUserNick nick)
        then return . Left $ "`" ++ nick ++ "` is not a valid nickname"
        else do
            map <- readTVar userMap
            case Map.lookup nick map of
                Nothing -> do
                    user <- newTVar $ User { nick = nick, connection = connection, rooms = [] }
                    writeTVar userMap $ Map.insert nick user map
                    return . Right $ user
                Just _ -> return . Left $ "User with nick `" ++ nick ++ "` already exists in the server"

renameUser :: String -> String -> UserMap -> STM (Either String (TVar User))
renameUser oldNick newNick userMap =
    if not (validateUserNick newNick)
        then return . Left $ "`" ++ newNick ++ "` is not a valid nickname"
        else do
            map <- readTVar userMap
            case Map.lookup oldNick map of
                Nothing -> return . Left $ "User with nick `" ++ oldNick ++ "` does not exist"
                Just tvarUser -> do
                    case Map.lookup newNick map of
                        Just _ -> return . Left $ "Nick `" ++ newNick ++ "` has already been claimed"
                        Nothing -> do
                            modifyTVar' tvarUser $ \u -> u { nick = newNick }
                            clearedOldNick <- return $ Map.delete oldNick map
                            writeTVar userMap $ Map.insert newNick tvarUser clearedOldNick
                            return . Right $ tvarUser

sendMessageToUser :: String -> Message -> UserMap -> STM (Either String (IO ()))
sendMessageToUser nick message userMap = do
    map <- readTVar userMap
    case Map.lookup nick map of
        Nothing -> return . Left $ "User named `" ++ nick ++ "` is not on this server"
        Just userT -> do
            user <- readTVar userT
            return . Right $ (writeMessage (connection user) message)

broadcastToAllUsersInServer :: Message -> UserMap -> IO ()
broadcastToAllUsersInServer message userMap = do
    actions <- atomically $ do
        users <- readTVar userMap
        atomicFmapUsersInServer ((flip writeMessage) message . connection) users
    sequence_ actions

atomicFmapUsersInServer :: (User -> a) -> Map String (TVar User) -> STM [a]
atomicFmapUsersInServer f map = sequence $ (liftM f <$> readTVar) <$> Map.elems map

userExists :: String -> UserMap -> STM Bool
userExists nick userMap = do
    map <- readTVar userMap
    return $ isJust $ Map.lookup nick map

validateUserNick :: String -> Bool
validateUserNick ('#':_) = False
validateUserNick _       = True

removeUser :: String -> UserMap -> STM (Either String (TVar User))
removeUser nick userMap = do
    map <- readTVar userMap
    case Map.lookup nick map of
        Just user -> do
            writeTVar userMap $ Map.delete nick map
            return . Right $ user
        Nothing -> return . Left $ "User with nick `" ++ nick ++ "` does not exist"

