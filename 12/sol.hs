import Text.Parsec 
import Text.Parsec.String

data Report = Report String [Int]

parseReport :: Parser Report
parseReport = Report <$> parselist <*> parseGroups

parselist = anyChar `endBy` space

parseGroups :: Parser [Int]
parseGroups = number `sepBy` char ','

number :: Parser Int 
number = read <$> many1 digit

main = do 
	input <- readFile "day12test.txt"
	print input
