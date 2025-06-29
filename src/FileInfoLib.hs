module FileInfoLib (fileInfoDo, printFileInfo) where

import qualified Data.Time.Clock as Clock
import qualified System.Directory as Directory

data FileInfo = FileInfo
  { filePath :: FilePath,
    fileSize :: Int,
    fileMTime :: Clock.UTCTime,
    fileReadable :: Bool,
    fileWriteable :: Bool,
    fileExecutable :: Bool
  }
  deriving (Show)

fileInfoDo :: FilePath -> IO FileInfo
fileInfoDo filePath_ = do
  perms <- Directory.getPermissions filePath_
  mtime <- Directory.getModificationTime filePath_
  contents <- readFile filePath_

  let size = length contents
  return
    FileInfo
      { filePath = filePath_,
        fileSize = size,
        fileMTime = mtime,
        fileReadable = Directory.readable perms,
        fileWriteable = Directory.writable perms,
        fileExecutable = Directory.executable perms
      }

printFileInfo :: String -> IO ()
printFileInfo filePath_ = do
  fileInfoToPrint <- fileInfoDo filePath_
  print fileInfoToPrint