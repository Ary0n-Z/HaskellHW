import Data.List
-------------------CoolPart:
type ExprStartIndex = Int
type ExprEndIndex = Int
type LastExpr = (ExprStartIndex,ExprEndIndex)

coolCalculator :: String -> Double
coolCalculator expr = calculator.coolCalculate.wordsWhen (==' ') $ (coolBracketsSugar expr)

coolBracketsSugar :: String -> String
coolBracketsSugar expr = reverse (coolRightBracketsSugar (reverse (coolLeftBracketsSugar expr)))

coolLeftBracketsSugar :: String -> String
coolLeftBracketsSugar [] = []
coolLeftBracketsSugar (x:xs)
                | x == '(' = x : ' ' : coolLeftBracketsSugar xs
                | otherwise = x : coolLeftBracketsSugar xs

coolRightBracketsSugar :: String -> String
coolRightBracketsSugar [] = []
coolRightBracketsSugar (x:xs)
                | x == ')' = x : ' ' : coolRightBracketsSugar xs
                | otherwise = x : coolRightBracketsSugar xs

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

--found in the net:
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
-------------------
-------------------
data Op = Plus | Minus | Devision | Multiplication
data Tree = Leaf Double 
         | Node Op Tree Tree

calculator :: [String] -> Double
calculator expr = calculate.createTree $ expr

calculate:: Tree -> Double
calculate tr = case tr of 
                    Leaf value ->  value
                    Node op left right -> (((getOpFunc op) $ (calculate left)) (calculate right))

createTree :: [String] -> Tree
createTree expr = case lastOperationIndex expr of
                (Just index) -> (Node 
                                   (getOp (expr !! index))
                                   (createTree (take index expr))
                                   (createTree (drop (index + 1) expr)))
                Nothing -> Leaf (read.head $ expr :: Double)

getOpFunc :: Fractional a => Op -> (a -> a -> a)
getOpFunc op = case op of
                Plus -> (+)
                Minus -> (-)
                Devision -> (/)
                Multiplication -> (*)

getOp :: String -> Op
getOp "+" = Plus
getOp "-" = Minus
getOp "*" = Multiplication
getOp "/" = Devision

lastOperationIndex :: [String] -> Maybe Int                 
lastOperationIndex arr
                   | plusIndex /= Nothing = plusIndex
                   | minusIndex /= Nothing = minusIndex
                   | multIndex /= Nothing = multIndex
                   | devIndex /= Nothing = devIndex
                   | otherwise = Nothing
                          where 
                          plusIndex = Data.List.elemIndex "+" arr
                          minusIndex = Data.List.elemIndex "-" arr
                          multIndex = Data.List.elemIndex "*" arr
                          devIndex = Data.List.elemIndex "/" arr
