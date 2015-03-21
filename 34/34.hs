digs :: Integral x => x -> [x]
digs 0 = []
digs x = digs (x `div` 10) ++ [x `mod` 10]

maxim = 40585 -- computed with maximum of the list

fact x = product [1 .. x]

factVal x = sum $ map (\i -> fact i) (digs x)
	

answer = sum $ map (\i -> if (i == factVal i) then i else 0) [3 .. maxim]
