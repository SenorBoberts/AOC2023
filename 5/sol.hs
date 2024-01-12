import Text.Parsec
import Text.Parsec.String
import Data.List

data Almanac = Almanac [Int] [[Mapping]] deriving Show -- Almanac [Seeds] [Mappings] 
data Mapping = Mapping Int Int Int deriving Show  -- Mapping Start Source Size

parseAlmanac :: Parser Almanac
parseAlmanac = Almanac <$> parseSeeds <*> manyTill parseMap (try eof)

parseSeeds :: Parser [Int]
parseSeeds = string "seeds:" >> space >> number `sepEndBy` space <* newline

parseMap :: Parser [Mapping]
parseMap = skipName >> newline >> parseMapping `sepEndBy` newline

parseMapping :: Parser Mapping
parseMapping = Mapping <$> number <* space <*> number <* space <*> number 

skipName :: Parser String 
skipName = manyTill anyChar (try (char ':'))

number :: Parser Int 
number = read <$> many1 digit

-- Part 1 

solve :: Int -> [Mapping] -> Int
solve s = foldr 
	(\(Mapping source start size) a -> 
		if s >= start && s < start + size 
			then s - start + source 
			else a) s 

solveSteps :: Int -> [[Mapping]] -> Int
solveSteps s [] = s
solveSteps s (map:mappings) = solveSteps (solve s map) mappings 

-- Part 2 
-- Lets use a million memory 
processSeeds :: [Int] -> [Int]
processSeeds [] = []
processSeeds (x:y:l) = [x..x + y] ++ processSeeds l

rmdups :: (Ord a) => [a] -> [a]
rmdups = map head . group . sort

main = do
	input <- readFile "day5.txt"
	let (Almanac seeds mappings) = case parse parseAlmanac "throwaway" input of 
		Left e -> error "error"
		Right c -> c
	print $ minimum $ map (`solveSteps` mappings) seeds

	-- Part 2 
	--print $ length $ processSeeds seeds
	--print $ length $ rmdups $ processSeeds seeds
	print $ minimum $ map (`solveSteps` mappings) $ processSeeds seeds
