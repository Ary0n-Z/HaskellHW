nonParity :: [Integer] -> [Integer]
nonParity arr
        | null arr = []
        | head arr `mod` 2 == 0 = nonParity (tail arr)
		| otherwise = head arr : nonParity (tail arr)