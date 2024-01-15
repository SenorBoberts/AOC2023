import Data.List (groupBy, sortBy)
data Cord = Cord Int Int deriving (Show, Eq)

-- PART 1

isSymbol :: Char -> Bool
isSymbol c = c `elem` ['!', '@', '#', '$', '%', '^', '&', '*', '-', '+', '=', '/']

isDigit :: Char -> Bool 
isDigit d = d `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9']

number :: String -> Int 
number = read 

findSymbolsRow :: [Char] -> Int -> Int -> [Cord]
findSymbolsRow [] _ _ = []
findSymbolsRow (l:line) row col = if isSymbol l then Cord row col : findSymbolsRow line row (col + 1) else findSymbolsRow line row (col + 1) 

findSymbols :: [[Char]] -> Int -> [Cord]
findSymbols [] _ = []
findSymbols (x:xs) i = findSymbolsRow x i 0 ++ findSymbols xs (i+1)

findNumberRow :: [Cord] -> [Char] -> Int -> Int -> Int 
findNumberRow _ [] _ _ = 0;
findNumberRow pos line row col = let i = extractNumber line in 
	if i /= [] && checkPos's pos (length i) row col then 
		number i + findNumberRow pos (dropN (length i) line) row (col + length i)
	else 
		findNumberRow pos (dropN 1 line) row (col + 1)
	
findNumbers :: [Cord] -> [[Char]] -> Int -> Int
findNumbers _ [] _ = 0
findNumbers pos (g:grid) i = findNumberRow pos g i 0 + findNumbers pos grid (i+1)

extractNumber :: [Char] -> [Char]
extractNumber [] = []
extractNumber (d:line) = if isDigit d then d : extractNumber line else []

checkPos's :: [Cord] -> Int -> Int -> Int -> Bool
checkPos's pos i row col = if i == -1 then checkPos pos row (col - 1) else checkPos pos row (col + i) || checkPos's pos (i-1) row col 

checkPos :: [Cord] -> Int -> Int -> Bool
checkPos pos row col = Cord row col `elem` pos || Cord (row - 1)  col `elem` pos || Cord (row + 1) col `elem` pos 

dropN :: Int -> [Char] -> [Char]
dropN _ [] = []
dropN 0 xs = xs
dropN i (x:xs) = dropN (i-1) xs   

main = do 
	input <- readFile "day3.txt"
	let grid = lines input
	let syms = findSymbols grid 0 
	-- part 1
	print $ findNumbers syms grid 0
