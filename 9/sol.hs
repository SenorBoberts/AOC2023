import Text.Parsec 
import Text.Parsec.String

parseInput :: Parser [Int]
parseInput = number `sepEndBy` space

number :: Parser Int 
number = read <$> many1 (digit <|> char '-')

-- Part 1 

predictStep :: [Int] -> Int
predictStep l = let diff = getDiff l in 
	if allZeros diff then last l else last l + predictStep diff
	

getDiff :: [Int] -> [Int]
getDiff [] = []
getDiff [x] = []
getDiff (x:y:z) = (y - x) : getDiff (y:z)

allZeros :: [Int] -> Bool
allZeros [] = True
allZeros l = foldr (\x a -> if x /= 0 then False else a) True l

main = do
	input <- readFile "day9.txt"
	let histories = map (\x -> case parse parseInput "throwaway" x of 
		Left e -> error "error" 
		Right c -> c) (lines input)
	print $ sum $ map predictStep histories
	print $ sum $ map (predictStep . reverse) histories

