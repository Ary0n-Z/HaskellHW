import Data.List

data Op = Plus | Minus | Devision | Multiplication
data Tree = Leaf Double 
         | Node Op Tree Tree

calculator :: String -> Double
calculator expr = calculate.createTree $ (wordsWhen (==' ') expr)

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
                Nothing -> clearLeaf expr

clearLeaf :: [String] -> Tree
clearLeaf expr = Leaf (read.head.removeBrackets $ expr :: Double)

removeBrackets :: [String] -> [String]
removeBrackets [] = []
removeBrackets (x:xs)
              | x == "(" = removeBrackets xs
              | x == ")" = removeBrackets xs
              | otherwise = x : removeBrackets xs
              
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
                   | isFullZero zeroArr = lastOperationIndex.zeroOutBrackets $ arr
                   | otherwise = Nothing
                          where 
                          zeroArr = zeroBrackets arr 0
                          plusIndex = Data.List.elemIndex "+" zeroArr
                          minusIndex = Data.List.elemIndex "-" zeroArr
                          multIndex = Data.List.elemIndex "*" zeroArr
                          devIndex = Data.List.elemIndex "/" zeroArr

zeroOutBrackets :: [String] -> [String]
zeroOutBrackets (x:xs)
             | x == "a" = zeroOutBrackets xs
             | (h == "(") && (l == ")") = zeroFirst.zeroLast $ arr
             | h == "(" = zeroFirst arr
             | l == ")" = zeroLast arr
             | otherwise = arr
                where
                arr = (x:xs)
                h = x
                l = last xs
 
zeroFirst (x:xs) = "a" : xs 
zeroLast arr = (init arr) ++ ["a"]
                
zeroBrackets :: [String] -> Int -> [String]
zeroBrackets [] _ = []
zeroBrackets (x:xs) deapth
            | x == "(" = "a" : zeroBrackets xs (deapth + 1)
            | x == ")" = "a" : zeroBrackets xs (deapth - 1)
            | deapth == 0 = x : zeroBrackets xs deapth
            | otherwise = "a" : zeroBrackets xs deapth


isFullZero :: [String] -> Bool
isFullZero [] = True
isFullZero (x:xs)
          | x == "a" = isFullZero xs
          | otherwise = False

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'