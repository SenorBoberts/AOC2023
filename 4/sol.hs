import Text.Parsec 
import Text.Parsec.String 
import Control.Monad


data Card = Card Int [Int] [Int] deriving (Show)

parseCard :: Parser Card 
parseCard = Card <$> 
	(string "Card" >> spaces >> number <* char ':') <*> (spaces >> number `endBy` spaces <* char '|') <*>
	(spaces >> number `sepBy` spaces)

-- Helpers
number :: Parser Int
number = read <$> many1 digit

--PART1 

calcScore :: Card -> Int
calcScore (Card _ w l) = foldr (\x a -> if x `elem` w then if a == 0 then 1 else a*2 else a) 0 l

--PART2 THIS IS SO SLOW HOLY FUCK

numCardsWon :: Card -> Int
numCardsWon (Card _ w l) = foldr (\x a -> if x `elem` w then a + 1 else a) 0 l

processCard :: Card -> [Card] -> Int
processCard (Card i w l) cards = do 
	let s = numCardsWon (Card i w l)
	let m = map (cards!!) [i..i+s-1]
	1 + (sum $ map (\x -> processCard x cards) m)

calcScoreManyCards :: [Card] -> Int
calcScoreManyCards c = sum $ map (\x -> processCard x c) c 

main = do 
	input <- readFile "day4.txt"
	let cards = map (parse parseCard "input.txt") (lines input)
	let unwrapped = map (\card -> case card of 
		Left e -> error "no parse" 
		Right c -> c) cards
	let s = sum $ map calcScore unwrapped 
	print s

	let s2 = calcScoreManyCards unwrapped
	print s2
