module Protocol where

import System.IO (Handle, hGetLine, hPutStrLn)
import Text.Parsec hiding (Ok)

data Message = Name String
             | Join  String -- as a command (join channel)
             | Join' String String -- as a notif
             | Part  String (Maybe String) -- as a command (leave channel with an optional parting msg)
             | Part' String String (Maybe String) -- as a notification
             | Quit  (Maybe String) -- as a command (quitting client sends (Quit (Maybe message))
             | Quit' String (Maybe String) -- as a notif (e.g. everyone who's in a channel with the client)
             | Say   String String -- as a command e.g., client sends (Say `target` `message`)
             | Act   String String -- when it wants to send `message` to `target` ; ditto for Act
             | Say'  String String String -- as a notification, e.g. server says (Say' `origin` `actor` `message`)
             | Act'  String String String -- when `actor`'s client sends (Say `origin` `message`)
             | Ok
             | Err   (Maybe String)
             deriving (Read, Show)

encodeMessage :: Message -> String
encodeMessage Ok                            = "OK"
encodeMessage (Name nick)                   = "NAME "  ++ nick
encodeMessage (Join room)                   = "JOIN "  ++ room
encodeMessage (Join' room actor)            = "JOIN' " ++ room ++ " " ++ actor
encodeMessage (Part room message)           = "PART "  ++ room ++ maybe "" (" " ++) message
encodeMessage (Part' room actor message)    = "PART' " ++ room ++ " " ++ actor ++ maybe "" (" " ++) message
encodeMessage (Quit reason)                 = "QUIT"   ++ maybe "" (" " ++) reason
encodeMessage (Quit' user reason)           = "QUIT' " ++ user ++ maybe "" (" " ++) reason
encodeMessage (Say target message)          = "SAY "   ++ target ++ " " ++ message
encodeMessage (Act target action)           = "ACT "   ++ target ++ " " ++ action
encodeMessage (Say' origin actor message)   = "SAY' "  ++ origin ++ " " ++ actor ++ " " ++ message
encodeMessage (Act' origin actor action)    = "ACT' "  ++ origin ++ " " ++ actor ++ " " ++ action
encodeMessage (Err message)                 = "ERR"    ++ maybe "" (" " ++) message

decodeMessage :: String -> Either String Message
decodeMessage s =
    case parse parser "net-line" s of
        Left err -> Left $ show err
        Right ret -> Right ret
    where -- todo rearrange these w/ most common parses first !
        parser =  try parseOK
              <|> try (parseMaybe "ERR" Err)
              <|> try (parseArgMaybeArg "QUIT'" Quit') -- must go before QUIT or else (Quit Nothing) could match
              <|> try (parseMaybe "QUIT" Quit)
              <|> try (parseSingleArg "NAME" Name)
              <|> try (parseSingleArg "JOIN" Join)
              <|> try (parseTwoArgs "JOIN'" Join')
              <|> try (parseArgMaybeArg "PART" Part)
              <|> try parsePart'
              <|> try (parseTwoArgs "SAY" Say)
              <|> try (parseTwoArgs "ACT" Act)
              <|> try (parseThreeArgs "SAY'" Say')
              <|> try (parseThreeArgs "ACT'" Act')
              <|> fail ""

        parseOK = do
            string "OK"
            eof
            return Ok

        parseMaybe prefix constructor = do
            string prefix
            arg <- optionMaybe readSingleArg'
            return $ constructor arg

        parseSingleArg prefix constructor = do
            string prefix
            arg <- readSingleArg'
            return $ constructor arg

        parseTwoArgs prefix constructor = do
            string prefix
            (arg1, arg2) <- readTwoArgs
            return $ constructor arg1 arg2

        parseThreeArgs prefix constructor = do
            string prefix
            arg1 <- readSingleArg
            (arg2, arg3) <- readTwoArgs
            return $ constructor arg1 arg2 arg3

        parseArgMaybeArg prefix constructor = do
            string prefix
            arg <- readSingleArg
            maybeArg <- optionMaybe readSingleArg'
            return $ constructor arg maybeArg

        parsePart' = do
            string "PART'"
            room  <- readSingleArg
            actor <- readSingleArg
            msg   <- optionMaybe readSingleArg'
            return $ Part' room actor msg

        readSingleArg = do
            char ' '
            many1 (noneOf " ")

        readSingleArg' = do
            char ' '
            many1 anyChar

        readTwoArgs = do
            arg1 <- readSingleArg
            arg2 <- readSingleArg'
            return (arg1, arg2)

readMessage :: Handle -> IO (Either String Message)
readMessage handle = hGetLine handle >>= return . decodeMessage

writeMessage :: Handle -> Message -> IO ()
writeMessage handle message = hPutStrLn handle $ encodeMessage message
