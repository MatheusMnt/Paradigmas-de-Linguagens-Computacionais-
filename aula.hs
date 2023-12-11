

primes :: [Integer] 
primes = sieve [2..]


sieve :: [Integer] -> [Integer]
sieve [] = []
sieve (primo:lista) = primo : sieve [a | a <- lista,  a `mod` primo /= 0] 
