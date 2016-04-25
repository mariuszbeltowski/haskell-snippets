import Data.Maybe

pierw2 :: Double -> Maybe Double
pierw2 x
    | x >= 0  = Just $ sqrt x
    | otherwise = Nothing

pierw4 :: Double -> Maybe Double
pierw4 x = Just x >>= pierw2 >>= pierw2


inv3 :: Double -> Maybe Double
inv3 n
    | n < 0 = Nothing
    | otherwise = return n >>= \x->return ((x) ** (1/3))

-- >>= \y -> return (1/y)

f :: Double -> Maybe Double
f x
    | x == 0 = Nothing
    | otherwise = Just (1/x)

g :: Double -> Maybe Double
g x
    | x>0 = Just (x ** (1/3))
    | otherwise = Nothing

-- inv3 x = return x >>= f >>= g

-- >> similar to >>= but without passing the value

-- ex from exam
-- isInt x = x == fromInteger (round x)

is3 :: Int -> Maybe Int
is3 x
  --  | length [ x | x<-[(1.0) .. xdo3], x == pie] >= 1 = Just ( round (pie) :: Int )
    | elem pie [(1.0) .. xdo3] = Just ( round (pie) :: Int )
    | otherwise = Nothing
      where
        xdo3 = fromIntegral (x*x*x)
        pie =  (  (fromIntegral x)  **(1/3))

c2 :: Maybe Int -> Int
c2 Nothing = 0
c2 (Just x) = x*2+1


c :: Maybe Int -> Int
c x
    | isJust x = fromJust x * 2 + 1
    | isNothing x = 0

fun :: Int -> Int
fun x = c $ (Just x) >>= is3

-- Pesel, Nazwisko
type PeselDB = [(String, String)]
-- Nazwisko, Adres
type AdressDB = [(String, String)]

lookupPesel :: PeselDB -> String -> Maybe String
lookupPesel [] _ = Nothing
lookupPesel (x:xs) nazwisko =
  if snd x == nazwisko then
    return (fst x)
  else
    lookupPesel xs nazwisko

getNameByPesel :: PeselDB -> String -> Maybe String
getNameByPesel [] _ = Nothing
getNameByPesel (x:xs) pesel =
  if fst x == pesel then
    return (snd x)
  else
    getNameByPesel xs pesel

lookupName :: AdressDB -> String -> Maybe String
lookupName [] _ = Nothing
lookupName (x:xs) address =
  if snd x == address then
    return (fst x)
  else
    lookupName xs address


test = do
  let x = 4
  print "dupa"
  return 4
