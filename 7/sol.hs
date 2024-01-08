import Data.List
import Data.Maybe
import Text.Parsec
import Text.Parsec.String
import Control.Arrow ((&&&))

data Hand = Hand [Char] Int deriving(Show)

parseHand :: Parser Hand
parseHand = Hand <$> many1 card <* space <*> number 

-- I feel like there has to be a better way to do this in Parsec
card :: Parser Char 
card = char '2' <|>
	char '3' <|>
	char '4' <|>
	char '5' <|>
	char '6' <|>
	char '7' <|>
	char '8' <|>
	char '9' <|>
	char 'T' <|>
	char 'J' <|>
	char 'Q' <|>
	char 'K' <|>
	char 'A'

number :: Parser Int
number = read <$> many1 digit

compareHand :: Hand -> Hand -> Ordering
compareHand (Hand c1 _) (Hand c2 _) = case compare (getRank c1) (getRank c2) of 
	GT -> GT 
	LT -> LT
	EQ -> secondScoring c1 c2 getCardValue1

compareHand2 :: Hand -> Hand -> Ordering
compareHand2 (Hand c1 _) (Hand c2 _) = case compare (getRank $ replaceJokers c1) (getRank $ replaceJokers c2) of 
	GT -> GT 
	LT -> LT
	EQ -> secondScoring c1 c2 getCardValue2

getRank :: [Char] -> Int
getRank c =
  case sort $ map length $ group $ sort c of
    [5]         -> 6 
    [1,4]       -> 5 
    [2,3]       -> 4 
    [1,1,3]     -> 3 
    [1,2,2]     -> 2 
    [1,1,1,2]   -> 1 
    [1,1,1,1,1] -> 0 
    _           -> error "oops"

getCardValue1 :: Char -> Int
getCardValue1 c = fromJust $ elemIndex c "23456789TJQKA"

getCardValue2 :: Char -> Int
getCardValue2 c = fromJust $ elemIndex c "J23456789TQKA"

secondScoring :: [Char] -> [Char] -> (Char -> Int) -> Ordering 
secondScoring c1 c2 f = case compare (f $ head c1) (f $ head c2) of 
	GT -> GT 
	LT -> LT
	EQ -> secondScoring (tail c1) (tail c2) f

getNum :: [Hand] -> Int -> Int 
getNum ((Hand c b):cs) i = b * i + getNum cs (i + 1) 
getNum [] _ = 0

replaceJokers :: [Char] -> [Char]
replaceJokers c = case c of 
	"JJJJJ" -> "AAAAA"
	_ -> map (\x -> if x == 'J' then mostCommon $ filter (/= 'J') c else x) c

mostCommon :: [Char] -> Char
mostCommon = snd . maximum . map (length &&& head) . group . sort

main = do
	input <- readFile "day7.txt"
	let m = map (\x -> case parse parseHand "throwaway.txt" x of 
		Left e -> error "oops"
		Right c -> c) (lines input)
	print $ getNum (sortBy compareHand m) 1
	print $ getNum (sortBy compareHand2 m) 1
