module HaskeDexDoBlockLib where

import qualified Control.Exception as Exception
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified System.Environment as Env
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
import qualified System.IO.Error as IOError (userError)
import qualified System.Info as SystemInfo
import qualified System.Process as Process (readProcess)

data ScreenDimensions = ScreenDimensions
  { screenRows :: Int,
    screenColumns :: Int
  }
  deriving (Show)

data ContinueCancel
  = Continue
  | Cancel
  deriving (Eq, Show)

getTerminalSizeDoBlock :: IO ScreenDimensions
getTerminalSizeDoBlock =
  case SystemInfo.os of
    "darwin" -> tputScreenDimensions
    "linux" -> tputScreenDimensions
    _default -> pure $ ScreenDimensions {screenRows = 25, screenColumns = 80}
  where
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
      lines_ <- Process.readProcess "tput" ["lines"] ""
      cols_ <- Process.readProcess "tput" ["cols"] ""
      return $
        ScreenDimensions
          { screenRows = read (init lines_),
            screenColumns = read $ init cols_
          }

handleArgsDoBlock :: IO (Either String FilePath)
handleArgsDoBlock = do
  parseArgs <$> Env.getArgs
  where
    parseArgs argumentList =
      case argumentList of
        [fname] -> checkString fname
        [] -> Left "no filename provided"
        _default -> Left "multiple files not supported"
    checkString string =
      case string of
        [] -> Left "provided an empty string"
        validString -> Right validString

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
      let (candidate, nextLines) = Text.splitAt lineLength lineText
          (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
       in firstLine : wordWrap lineLength (overflow <> nextLines)
  where
    softWrap hardwrappedText textIndex
      | textIndex <= 0 = (hardwrappedText, Text.empty)
      | Text.index hardwrappedText textIndex == ' ' -- if it's an 'space' character
        =
          let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
           in (wrappedLine, Text.tail rest)
      | otherwise = softWrap hardwrappedText (textIndex - 1)

groupsOf :: Int -> [a] -> [[a]]
groupsOf n elems =
  let (hd, tl) = splitAt n elems -- this declares variables instead of functions
   in case tl of
        [] -> [hd]
        tailValue -> hd : groupsOf n tailValue

paginate :: ScreenDimensions -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines = concatMap (wordWrap cols) unwrappedLines
      pageLines = groupsOf rows wrappedLines
   in map Text.unlines pageLines

showPagesDoBlock :: [Text.Text] -> IO ()
showPagesDoBlock pages =
  case pages of
    [] -> return ()
    (page : _pages) -> do
      clearScreenDoBlock
      TextIO.putStrLn page
      input <- getContinueDoBlock
      case input of
        Continue -> showPagesDoBlock pages
        Cancel -> return ()

clearScreenDoBlock :: IO ()
clearScreenDoBlock = do
  putStr "\ESC[2J\ESC[H"

getContinueDoBlock :: IO ContinueCancel
getContinueDoBlock = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  input <- getChar
  case input of
    ' ' -> return Continue
    'q' -> return Cancel
    _default -> getContinueDoBlock

runHaskeDexDoBlock :: IO ()
runHaskeDexDoBlock = do
  args <- handleArgsDoBlock
  targetFilePath <- eitherToError args
  contents <- TextIO.readFile targetFilePath
  termSize <- getTerminalSizeDoBlock

  let pages = paginate termSize contents

  withErrorHandling $ showPagesDoBlock pages
  where
    withErrorHandling :: IO () -> IO ()
    withErrorHandling ioAction = Exception.catch ioAction handleError
    handleError :: IOError -> IO ()
    handleError e =
      TextIO.putStrLn (Text.pack "I ran into an error:") >> print e
    eitherToError :: (Show a) => Either a b -> IO b
    eitherToError eToE =
      case eToE of
        (Right a) -> return a
        (Left e) -> Exception.throwIO . IOError.userError $ show e