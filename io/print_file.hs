import System.IO

main = do
  content <- readLines "text1.txt"
  let arr =  content
  let pairs = zip [1..] arr
  let mapped = map (\(x,y) -> concat [(show x), ": ", y] ) pairs

  putStr ( unlines mapped )


readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile
