{-# LANGUAGE MultiWayIf #-}

getValLine :: String -> Int
getValLine line = read [checkFrontString line, checkBackString line 'A'] 

isNum :: Char -> Bool 
isNum x = x `elem` ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'] 

--part1
getFirstDigit :: String -> Char
getFirstDigit (x:xs) = if isNum x then x else getFirstDigit xs
getFirstDigit [] = '0'

--part2
--
checkForWord :: String -> Char
checkForWord line = if 
	| take 4 line == "zero" -> '0'
	| take 3 line == "one" -> '1'
	| take 3 line == "two" -> '2'
	| take 5 line == "three" -> '3'
	| take 4 line == "four" -> '4'
	| take 4 line == "five" -> '5'
	| take 3 line == "six" -> '6'
	| take 5 line == "seven" -> '7'
	| take 5 line == "eight" -> '8'
	| take 4 line == "nine" -> '9'
	| otherwise -> 'A'

checkFrontString :: String -> Char
checkFrontString line = if 
	| isNum $ head line -> head line
	| checkForWord line /= 'A' -> checkForWord line 
	| otherwise -> checkFrontString $ tail line

checkBackString :: String -> Char -> Char
checkBackString line acc = if 
	| null line -> acc
	| isNum $ head line -> checkBackString (tail line) (head line)
	| checkForWord line /= 'A' -> checkBackString (tail line) (checkForWord line)
	| otherwise -> checkBackString (tail line) acc 
	
main = do 
	input <- readFile "input.txt"
	let x = sum $ map getValLine (lines input)
	print x
