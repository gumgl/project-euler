--makeList (a:as) (b:bs) = a^b : (makeList [a] b:bs) ++ (makeList a:as [b]) ++ (makeList as bs)
import Data.List

as = [2..100]
bs = [2..100]

combos = [ (a,b) | a<-as, b<-bs ]

l = map (\(a,b) -> a^b) combos

-- counts unique elements of an ordered list (not counting duplicates)
countUnique [] _ = 0
countUnique (x:xs) prev =
	if (x == prev) then countUnique xs prev
	else 1 + countUnique xs x

answer = countUnique (sort l) 0