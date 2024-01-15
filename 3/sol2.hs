import Data.List (groupBy, sortBy)
data Cord = Cord Int Int deriving (Show, Eq)

isDigit :: Char -> Bool 
isDigit d = d `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

number :: String -> Int 
number = read 

extractNumber :: [Char] -> [Char]
extractNumber [] = []
extractNumber (d:line) = if isDigit d then d : extractNumber line else []

dropN :: Int -> [Char] -> [Char]
dropN _ [] = []
dropN 0 xs = xs
dropN i (x:xs) = dropN (i-1) xs   

-- Part 2
-- Looks like I kept track of the wrong things for part 2 to be easy

findGearsRow :: [Char] -> Int -> Int -> [Cord]
findGearsRow [] _ _ = []
findGearsRow (l:line) row col = if l == '*' then Cord row col : findGearsRow line row (col + 1) else findGearsRow line row (col + 1) 

findGears :: [[Char]] -> Int -> [Cord]
findGears [] _ = []
findGears (x:xs) i = findGearsRow x i 0 ++ findGears xs (i+1)

checkPos'sGear :: [Cord] -> Int -> Int -> Int -> (Bool, Cord)
checkPos'sGear pos i row col = if i == -1 then checkPosGear pos row (col - 1) else 
	case checkPosGear pos row (col + i) of
		(True, c) -> (True, c)
		(False, _) -> checkPos'sGear pos (i-1) row col

checkPosGear :: [Cord] -> Int -> Int -> (Bool, Cord)
checkPosGear pos row col  
	| Cord row col `elem` pos = (True, Cord row col)
	| Cord (row - 1)  col `elem` pos = (True, Cord (row - 1) col)
	| Cord (row + 1) col `elem` pos = (True, Cord (row + 1) col)
	| otherwise = (False, Cord (-1) (-1))

findGearNumberRow :: [Cord] -> [Char] -> Int -> Int -> [(Cord, Int)] 
findGearNumberRow _ [] _ _ = [];
findGearNumberRow pos line row col = let i = extractNumber line in let p = checkPos'sGear pos (length i) row col in 
	if i /= [] && fst p then 
		(snd p, number i) : findGearNumberRow pos (dropN (length i) line) row (col + length i)
	else 
		findGearNumberRow pos (dropN 1 line) row (col + 1)
	
findGearNumbers :: [Cord] -> [[Char]] -> Int -> [(Cord, Int)]
findGearNumbers _ [] _ = []
findGearNumbers pos (g:grid) i = findGearNumberRow pos g i 0 ++ findGearNumbers pos grid (i+1)

compareCord (Cord a b) (Cord x y) 
	| a > x = GT 
	| a < x = LT 
	| a == x = compare b y

groupGears :: [(Cord, Int)] -> [[(Cord, Int)]]
groupGears l = groupBy (\(c1, _) (c2, _) -> c1 == c2) $ sortBy (\(c1, _) (c2, _) -> compareCord c1 c2) l

getGearRatio :: [(Cord, Int)] -> Int 
getGearRatio ((Cord _ _, i):(Cord _ _, j):[]) = i * j
getGearRatio _ = 0

calcGearRatios :: [[(Cord, Int)]] -> Int
calcGearRatios = foldr (\x a -> getGearRatio x + a) 0

main = do 
	input <- readFile "day3.txt"
	let grid = lines input
	-- part 2
	let gears = findGears grid 0
	print $ calcGearRatios $ groupGears $ findGearNumbers gears grid 0
