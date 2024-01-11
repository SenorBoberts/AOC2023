import Text.Parsec 
import Text.Parsec.String
import qualified Data.Map as Map

data Node = Node String Edge deriving (Show)
data Edge = Edge String String deriving (Show)

parseNode :: Parser Node 
parseNode = Node <$> count 3 anyChar <* string " = " <*> parseEdge

parseEdge :: Parser Edge 
parseEdge = Edge <$> (char '(' >> count 3 anyChar <* char ',') <*> (space >> count 3 anyChar <* char ')') 

findNext :: Map.Map String Edge -> String -> Char -> String
findNext m s d = do 
	let Edge l r = case Map.lookup s m of 
		Just x -> x 
		Nothing -> error "Error"
	if d == 'L' then l else r

findSteps :: Map.Map String Edge -> String -> [Char] -> [Char] -> Int -> Int 
findSteps m s (x:xs) fullinstr count = do 
	let loc = findNext m s x 
	if loc == "ZZZ" then count else findSteps m loc xs fullinstr (count + 1)
findSteps m s [] fullinstr count = findSteps m s fullinstr fullinstr count

--- PART 2 

-- General Solution that is too slow :(

getAllAs :: [Node] -> [String]
getAllAs = foldr (\(Node s _) a -> if last s == 'A' then s:a else a) []

checkAllZs :: [String] -> Bool 
checkAllZs = foldr (\s a -> if last s /= 'Z' then False else a) True

findSteps2 :: Map.Map String Edge -> [String] -> [Char] -> [Char] -> Int -> Int
findSteps2 m ss (x:xs) fullinstr count = do 
	let locs = map (\s -> findNext m s x) ss 
	if checkAllZs locs then count else findSteps2 m locs xs fullinstr (count + 1)
findSteps2 m ss [] fullinstr count = findSteps2 m ss fullinstr fullinstr count

-- LCM solution that only works for specially crafted input :annoying:

findStepsLCM :: Map.Map String Edge -> [String] -> [Char] -> [Int]
findStepsLCM m ss xs = map (\s -> findStepLCM m s xs xs 1) ss 

findStepLCM :: Map.Map String Edge -> String -> [Char] -> [Char] -> Int -> Int 
findStepLCM m s (x:xs) fullinstr count = do 
	let loc = findNext m s x 
	if endsInZ loc then count else findStepLCM m loc xs fullinstr (count + 1)
findStepLCM m s [] fullinstr count = findStepLCM m s fullinstr fullinstr count

endsInZ :: String -> Bool
endsInZ s = last s == 'Z'

lcmm :: Integral a => [a] -> a
lcmm = foldr lcm 1 

main = do 
	input <- readFile "day8.txt"
	let nodes = map (\x -> case parse parseNode "throwaway" x of 
		Left e -> error "error"
		Right c -> c) (lines input)
	let m = foldr (\(Node s e) m -> Map.insert s e m) Map.empty nodes
	input2 <- readFile "day8instr.txt"
	let i = filter (/= '\n') input2
	print $ findSteps m "AAA" i i 1 
	-- Part 2 
	let starts = getAllAs nodes
	print $ lcmm $ findStepsLCM m starts i  
