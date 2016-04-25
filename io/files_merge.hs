import System.IO

-- function must be "main" if args are required
-- merge 2 files
main = do
  file1 <-getLine
  file2 <-getLine
  f1 <- openFile file1 ReadMode
  f2 <- openFile file2 ReadMode
  contents1 <- hGetContents f1
  contents2 <- hGetContents f2
  let allLines1 = lines contents1
  let allLines2 = lines contents2
  let r1 = map (appendNewLine) allLines1
  let r2 = map (appendNewLine) allLines2
  let result = concat (zipWith (++) r1 r2)
  writeFile "result.txt" result


appendNewLine x = if((last x)=='.') then x ++ ['\n'] else x++[' ']
