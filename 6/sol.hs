import Text.Parsec 
import Text.Parsec.String

data Races = Races [Int] [Int] deriving (Show)

-- Parser 1 
parseInput :: Parser Races
parseInput = Races <$> (string "Time:" >> spaces >> many1 (number <* spaces)) <*> (string "Distance:" >> spaces >> many1 (number <* spaces))

number :: Parser Int 
number = read <$> many1 digit

-- Part 1

getDistance :: Int -> Int -> Int -> Bool
getDistance hold time record = (time - hold) * hold > record

getRace :: Int -> Int -> Int
getRace time record = foldr (\x a -> if getDistance x time record then a + 1 else a) 0 [0..time] 

getRaces :: [Int] -> [Int] -> Int
getRaces (x:xs) (y:ys) = getRace x y * getRaces xs ys
getRaces [] [] = 1

-- Part 2 
-- I have no idea why it makes me do this
getBigRace :: [Int] -> String
getBigRace = concatMap show

getInt :: [Int] -> Int
getInt i = read (getBigRace i)

getPart2 :: [Int] -> [Int] -> Int
getPart2 l1 l2 = getRace2 (getInt l1) (getInt l2)


getRaceBot hold time record = if getDistance hold time record then hold else getRaceBot (hold + 1) time record 
getRaceTop hold time record = if getDistance hold time record then hold else getRaceTop (hold - 1) time record 
getRace2 time record = getRaceTop time time record - getRaceBot 0 time record + 1 

main = do 
	input <- readFile "day5.txt"
	let p = case parse parseInput "throwaway.txt" input of 
		Left e -> error "e"
		Right (Races l1 l2) -> getRaces l1 l2 
	print p
	let p2 = case parse parseInput "throwaway.txt" input of 
		Left e -> error "e"
		Right (Races l1 l2) -> getPart2 l1 l2 
	print p2

	
