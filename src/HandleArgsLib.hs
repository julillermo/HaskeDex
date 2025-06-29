module HandleArgsLib
  ( handleArgsDoBlock,
    withErrorHandling,
    determineHaskeDexFeature,
    Among3 (..),
  )
where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import qualified System.IO.Error as IOError (userError)

data Among3 a b c = First a | Second b | Third c

-- handleArgsDoBlock :: IO (Either String FilePath)
-- handleArgsDoBlock = do
--   parseArgs <$> Env.getArgs
--   where
--     parseArgs argumentList =
--       case argumentList of
--         [fname] -> checkString fname
--         [] -> Left "no filename provided"
--         _default -> Left "multiple files not supported"
--     checkString string =
--       case string of
--         [] -> Left "provided an empty string"
--         validString -> Right validString

handleArgsDoBlock :: IO (Among3 FilePath FilePath String)
handleArgsDoBlock = do
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> checkString fname
        [fname, action] -> validateAction fname action
        [] -> Third "no filename provided"
        _default -> Third "multiple files not supported"
    checkString string =
      case string of
        [] -> Third "provided an empty string"
        validString -> First validString
    validateAction fName action =
      case action of
        "info" -> Second fName
        _default -> Third "action is unrecognized"

withErrorHandling :: IO () -> IO ()
withErrorHandling ioAction = Exception.catch ioAction handleError

handleError :: IOError -> IO ()
handleError e =
  TextIO.putStrLn (Text.pack "I ran into an error:") >> print e

determineHaskeDexFeature :: (Show c) => Among3 FilePath FilePath c -> IO (Either FilePath FilePath)
determineHaskeDexFeature uInput =
  case uInput of
    (First fPathLess) -> return $ Left fPathLess
    (Second fPathInfo) -> return $ Right fPathInfo
    (Third c) -> Exception.throwIO . IOError.userError $ show c

-- eitherToError :: (Show a) => Either a b -> IO b
-- eitherToError eToE =
--   case eToE of
--     (Right a) -> return a
--     (Left e) -> Exception.throwIO . IOError.userError $ show e