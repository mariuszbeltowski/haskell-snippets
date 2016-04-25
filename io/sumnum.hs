import System.IO

-- write to file output.txt sum and count of integer from file int.txt
main = do
  content <- readLines "ints.txt"
  let arr = makeInteger content

  let arrSum = sum arr
  let arrCount = length arr

  fileWriteHandle <- openFile "output.txt" WriteMode
  hPutStrLn fileWriteHandle (show arrSum)
  hPutStrLn fileWriteHandle (show arrCount)

  hClose fileWriteHandle

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

makeInteger :: [String] -> [Int]
makeInteger = map read
