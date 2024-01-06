import Text.Parsec
import Text.Parsec.String

-- PARSER

data Game = Game Int [[Round]] deriving (Show)
data Round = Round Int String deriving (Show)

parseGame :: Parser Game
parseGame = Game <$> (string "Game" >> space >> (read <$> many1 digit) <* char ':') <*> (parseRounds `sepBy` char ';')

parseRounds :: Parser [Round]
parseRounds = parseRound `sepBy` char ','

parseRound :: Parser Round
parseRound = Round <$> (space >> number <* space) <*> many1 letter

number :: Parser Int 
number = read <$> many1 digit

-- PART 1 Processing 

checkIllegalGame :: Game -> Int
checkIllegalGame (Game i rounds) = if foldr (\r b -> if checkIllegalRound r then b else False) True rounds then i else 0

checkIllegalRound :: [Round] -> Bool
checkIllegalRound = foldr (\(Round n s) b -> 
	case s of 
		"blue" -> if n > 14 then False else b 
		"red" -> if n > 12 then False else b 
		"green" -> if n > 13 then False else b
	) True

-- PART 2 Processing

gameMin :: Game -> Int 
gameMin (Game i rounds) = case getMin (0, 0, 0) $ concat rounds of (x, y, z) -> x * y * z

getMin :: (Int, Int, Int) -> [Round] -> (Int, Int, Int)
getMin = foldr (\(Round i s) (r, g, b) -> 
	case s of 
		"blue" -> if b < i then (r, g, i) else (r, g, b)
		"red" -> if r < i then (i, g, b) else (r, g, b)
		"green" -> if g < i then (r, i, b) else (r, g, b)
		)  


main = do 
	input <- readFile "day2.txt"
	let games = map (parse parseGame "input.txt") (lines input)
	let unwraped = map (\game -> case game of 
		Left e -> error "parse no parse right"
		Right c -> c) games
	--PART1
	let s = sum $ map checkIllegalGame unwraped
	print s
	--PART2
	let s2 = sum $ map gameMin unwraped
	print s2
