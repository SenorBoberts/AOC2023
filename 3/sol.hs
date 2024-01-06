
data Cord = Cord Int Int

-- findSymbols :: [[Char]] -> [Cord]
findSymbols = 

main = do 
	input <- readFile "day3.txt"
	let grid = lines input
	print grid
