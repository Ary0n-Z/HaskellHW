import Data.List

----------------------------------------------------------
------------------- Cool calculator -------------------
-- coolCalculator supports brackets
-- example: > coolCalculator "(3 + 6 / (34.2 - (-34)) / 2) - (-3.354 + 8 * 3)"
type ExprStartIndex = Int
type ExprEndIndex = Int
type LastExpr = (ExprStartIndex,ExprEndIndex)

coolCalculator :: String -> Double
coolCalculator expr = calculator.coolCalculate.wordsWhen (==' ') $ (coolBracketsSugar expr)

coolCalculate :: [String] -> [String]
coolCalculate expr = if startIndex == endIndex then expr
                     else coolCalculate updatedExpr
              where 
                  calculatedExpr = calculator (getLastExpr expr (startIndex,endIndex))
                  (startIndex,endIndex) = getLastExprRange expr
                  updatedExpr = (take (startIndex) expr) ++ [(show calculatedExpr)] ++ (drop (endIndex+1) expr) 

getLastExpr :: [String] -> LastExpr -> [String]
getLastExpr expr (startIndex,endIndex) = take (endIndex-startIndex-1) (drop (startIndex+1) expr)
                
getLastExprRange :: [String] -> LastExpr
getLastExprRange expr = case elemIndex ")" expr of
                  (Just endIndex) -> (getStartIndex expr endIndex,endIndex)
                  Nothing -> (-1,-1)
                  
getStartIndex :: [String] -> Int -> Int
getStartIndex _ 0 = 0
getStartIndex expr endIndex
                    | expr !! endIndex == "(" = endIndex
                    | otherwise = getStartIndex expr (endIndex-1)

-- Cut String to [String] if p at char returns true. Found in the Net. 
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
                            
-- Syntax sugar for brackets.
coolBracketsSugar :: String -> String
coolBracketsSugar [] = []
coolBracketsSugar (x:xs)
                 | x == '(' = x : ' ' : coolBracketsSugar xs
                 | x == ')' = ' ' : x : coolBracketsSugar xs
                 | otherwise = x : coolBracketsSugar xs
----------------------------------------------------------
------------------- Default calculator -------------------
-- example: > calculator ["4", "-", "5", "*", "2"]

type Expression = String

data MathOperation a = MathOperation String a
data Tree = Leaf Double 
         | Node (MathOperation (Double->Double->Double)) Tree Tree
         
defaultOperations :: [MathOperation (Double -> Double -> Double)]
defaultOperations =  [MathOperation "+" (+),MathOperation "-" (-), MathOperation "*" (*), MathOperation "/" (/)]

calculator :: [Expression] -> Double
calculator expr = calculate.createTree $ expr

calculate:: Tree -> Double
calculate tr = case tr of 
                    Leaf value ->  value
                    Node (MathOperation _ op) left right -> ((op $ (calculate left)) (calculate right))

createTree :: [Expression] -> Tree
createTree expr = case lastOperationIndex expr defaultOperations of
                (Just (index,op)) -> (Node 
                                   (MathOperation (expr !! index) op)
                                   (createTree (take index expr))
                                   (createTree (drop (index + 1) expr)))
                Nothing -> Leaf (read.head $ expr :: Double)

lastOperationIndex :: [Expression] -> [MathOperation (Double -> Double -> Double)] -> Maybe (Int,(Double -> Double -> Double))
lastOperationIndex _ [] = Nothing
lastOperationIndex expr ((MathOperation opName op):xs)= case elemIndex opName expr of
                                (Just index) -> (Just (index,op))
                                Nothing -> lastOperationIndex expr xs
