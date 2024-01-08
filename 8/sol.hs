import Text.Parsec 
import Text.Parsec.String


data Node = Leaf String | 

parseNodes :: Parser [Node]
parseNodes = many1 parseNode

parseNode :: Parser Node 
parseNode = Node hello None None




main = do 
	print "ln"
