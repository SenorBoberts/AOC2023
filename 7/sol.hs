import Text.Parsec
import Text.Parsec.String

data Hand = Hand [Char] Int

parseHand :: Parser Hand
parseHand = Hand <$> many1 anyChar <* space <*> number 

number :: Parser Int
number = read <$> many1 digit


compareHand :: Hand -> Hand -> Ordering
compareHand (Hand c1 _) (Hand c2 _) = GT;


--getOccMap (Hand c _) = foldr (\x a -> ) [] c

main = do
	input <- readFile "day7test.txt"
	print "hi"
