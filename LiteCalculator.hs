import Data.List

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

--type CheckOp = True
--type CheckValue = False

--checkInput :: [String] -> Bool -> Bool
--checkInput (x:xs) checkType
--                | checkType == CheckValue = if correctValue x 
--				                            then checkInput xs CheckOp
--											else False
--			    | checkType == CheckOp = if correctOp x
--				                         then checkInput xs CheckValue
--										 else False