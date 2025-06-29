module HaskeDexDoBlockLib where

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import FileInfoLib (printFileInfo)
import HandleArgsLib
  ( determineHaskeDexFeature,
    handleArgsDoBlock,
    withErrorHandling,
  )
import System.IO (BufferMode (NoBuffering), hSetBuffering, hSetEcho, stdin)
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

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText =
  if Text.length lineText <= lineLength
    then [lineText]
    else
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
  targetFilePathWithFeature <- determineHaskeDexFeature args
  case targetFilePathWithFeature of
    (Left fPathLess) -> do
      contents <- TextIO.readFile fPathLess
      termSize <- getTerminalSizeDoBlock
      let pages = paginate termSize contents
      withErrorHandling $ showPagesDoBlock pages
    (Right fPathInfo) -> do
      printFileInfo fPathInfo