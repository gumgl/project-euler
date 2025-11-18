p 0 _ = 1
p 1 _ = 1
p _ 1 = 1
p total maxN
    | maxN > total = p total (maxN - 1) -- We have used up all maxN, skip to next maxN
    | otherwise = (p (total - maxN) maxN) -- Use one maxN of potentially many
                + (p total (maxN - 1)) -- Do not use maxN, total remains unchanged

main = print (p 100 99)