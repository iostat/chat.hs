module Room ( Room(..)
            , RoomMap
            , addUserToRoom
            , removeUserFromRoom
            , removeUserFromAllRooms
            , broadcastToRoom
            , broadcastToRoomsWithUser
            , validateRoomName
            , userIsInRoom
            ) where

import Control.Monad (liftM, sequence_)
import Control.Concurrent.STM
import Data.Map.Strict as Map hiding (foldl, filter)

import Protocol
import User

data Room = Room {
    name :: String,
    users :: [TVar User]
}

type RoomMap = TVar (Map String Room)

getOrCreateRoom :: String -> RoomMap -> STM Room
getOrCreateRoom roomName roomMap = do
    rooms <- readTVar roomMap
    case Map.lookup roomName rooms of
        Just room -> return room
        Nothing   -> do
            theNewRoom <- return $ newRoom roomName
            writeTVar roomMap $ Map.insert roomName theNewRoom rooms
            return theNewRoom

addUserToRoom :: String -> TVar User -> RoomMap -> STM Room
addUserToRoom roomName theUser roomMap = do
    targetRoom <- getOrCreateRoom roomName roomMap
    if (theUser `elem` (users targetRoom))
        then return targetRoom
        else do
            roomWithUser <- return $ targetRoom { users = theUser : (users targetRoom) }
            modifyTVar' roomMap $ Map.insert roomName roomWithUser
            modifyTVar' theUser $ \u -> u { rooms = roomName : (rooms u) }
            return roomWithUser

removeUserFromRoom :: String -> TVar User -> RoomMap -> STM ()
removeUserFromRoom roomName theUser roomMap = do
    allRooms <- readTVar roomMap
    case Map.lookup roomName allRooms of
        Nothing -> return ()
        Just room -> do
            roomWithoutUser <- return $ room { users = filter (/= theUser) (users room) }
            modifyTVar theUser $ \u -> u { rooms = filter (/= roomName) (rooms u) }
            writeTVar roomMap $ case (users roomWithoutUser) of
                [] -> Map.delete roomName allRooms
                _  -> Map.insert roomName roomWithoutUser allRooms

removeUserFromAllRooms :: TVar User -> RoomMap -> STM ()
removeUserFromAllRooms userT roomMap = do
    user <- readTVar userT
    foldl (>>) (return ()) $ (\room -> removeUserFromRoom room userT roomMap) <$> (rooms user)

-- atomically read a list of users in a room
-- build up a list of IO actions that send the message
-- and fold those actions
broadcastToRoom :: String -> Message -> RoomMap -> IO ()
broadcastToRoom roomName message roomMap = do
    actions <- atomically $ do
        rooms <- readTVar roomMap
        case Map.lookup roomName rooms of
            Nothing   -> return []
            Just room -> atomicFmapUsers ((flip writeMessage) message . connection) room
    sequence_ actions

broadcastToRoomsWithUser :: TVar User -> Message -> RoomMap -> IO ()
broadcastToRoomsWithUser userT message roomMap = do
    rooms <- atomically $ readTVar userT >>= (return . rooms)
    foldl (>>) (return ()) $ (\roomName -> broadcastToRoom roomName message roomMap) <$> rooms

validateRoomName :: String -> Bool
validateRoomName ('#':xs) = True
validateRoomName _        = False

userIsInRoom :: String -> TVar User -> RoomMap -> STM Bool
userIsInRoom roomName userT roomMap = do
    user     <- readTVar userT
    allRooms <- readTVar roomMap
    case Map.lookup roomName allRooms of
        Nothing -> return False
        Just room -> return $ (roomName `elem` (rooms user)) && (userT `elem` (users room))

-- apply a function over the actual users in a room, within the STM monad
-- this guarantees that you'll get a consistent list of users in a room
-- as you would use it within the same `atomically` transaction
atomicFmapUsers :: (User -> a) -> Room -> STM [a]
atomicFmapUsers f room = sequence $ (liftM f <$> readTVar) <$> users room

newRoom :: String -> Room
newRoom name = Room { name = name, users = [] }
