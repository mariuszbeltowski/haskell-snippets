import System.IO;
import System.Environment;

--allSeq n = all words of length n, alphabet {a,b}
allSeq :: Int -> [String]
allSeq 1 = ["a","b"]
allSeq n = ['a':x | x<-allSeq (n-1)] ++['b':x | x<-allSeq (n-1)]

-- palinList n generate all palindroms of length n nad, alphabet {a,b}
-- simplest version?
palinList :: Int -> [String]
palinList n = [x | x<-allSeq n, x==reverse x]


-- save list to file
listToFile :: Handle -> [String] -> IO ()
listToFile fhandle [] = return ()
listToFile fhandle (x:xs) = do
				hPutStrLn fhandle x
				listToFile fhandle xs

palin :: Handle -> Int -> IO ()
palin fhandle n = listToFile fhandle (palinList n)

-- Howto: ghc palin.hs -o palin
-- palin ola.txt 10
-- Or from interpreter: :set args ola.txt 10
-- main.

main = do
		(fname:num:rest) <-getArgs
		fhandle <-openFile fname WriteMode
		palin fhandle ((read num)::Int)
		hClose fhandle
