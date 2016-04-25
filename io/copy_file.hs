import System.IO

-- copy file
main = do
  fileReadName <-getLine
  fileWriteName <- getLine

  fileReadHandle <- openFile fileReadName ReadMode
  fileWriteHandle <- openFile fileWriteName WriteMode

  copyFile fileReadHandle fileWriteHandle

  hClose fileReadHandle
  hClose fileWriteHandle

copyFile :: Handle -> Handle -> IO()
copyFile readHandle writeHandle = do
  ineof <- hIsEOF readHandle
  if ineof then return ()
  else do
    fileLine <- hGetLine readHandle
    hPutStrLn writeHandle fileLine
    copyFile readHandle writeHandle
