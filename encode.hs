rldecode :: [Int] -> [Int]
rldecode [] = [] 
rldecode (0:as) = toZero (head as) ++ rldecode (tail as)
rldecode (a:as) = a : rldecode as

toZero:: Int -> [Int]
toZero 0 = []
toZero n = 0 : toZero (n-1)   


teste :: [Int]
teste = rldecode [17, 0, 3, 23, 0, 4, 9]

rlencode0 :: [Int] -> [Int]
rlencode0 [] = [] 
rlencode0 (0:nums) = 0 : ((count0 nums) + 1) : rlencode0 (drop0 nums) 
rlencode0 (n:nums) = n : rlencode0 nums

count0 :: [Int] -> Int
count0 (0:resto) = 1 + count0 resto
count0 _ = 0

drop0 :: [Int] -> [Int]
drop0 [] = []
drop0 (0:as) = drop0 as
drop0 as = as

teste2 :: [Int]
teste2 = rlencode0 [17, 0, 0, 0, 23, 0, 0, 0, 0, 9]
