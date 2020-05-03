{-- We take a natural number. If that number is even, we divide it by two. 
If it's odd, we multiply it by 3 and then add 1 to that. We take the resulting 
number and apply the 
same thing to it, which produces a new number and so on. In essence--}
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
    | even n = n: collatz (n `div` 2)
    | otherwise = n: collatz (3*n + 1)



zipWith' :: (a->b->c) -> [a]->[b]->[c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

elemof10 :: Int -> Bool
elemof10 = (`elem` [1..10])

elem'' :: Int -> Bool
elem'' x = elem x [1..10]

fun :: (a->a) -> (a->a)
fun f = f

qs :: (Ord a) => [a] -> [a]
qs [] = []
qs [x] = [x]
qs (x:xs) =
    let big = qs [a | a <- xs, a >= x]
        small = qs [a | a <- xs, a < x]
    in small ++ [x] ++ big

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
	| x == y = True
	| otherwise = elem' x ys

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys


max' :: (Ord a) => [a] -> a
max' [] = error "no elements in list"
max'  [x] = x
max' (x:xs)
	| x > tailMax = x
	| otherwise = tailMax
	where tailMax = max' xs
f :: (Num a, Ord a) => [a] -> [a]
f (x:xs)
	| x < 5 = xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

headthing :: (Num a) => [a] -> (a,[a])
headthing [] = error "oops"
headthing (x:y:z:zs) = (x * y * z,zs)
headthing (x:a) = (x,a)

doubleMe x = x + x

doubleUs x y = 2*x + y*2

is_small x = if x < 10 
	then 1 
	else 0

isSeven :: (Integral a) => a -> String

isSeven 7 = "It's seven!"
isSeven x = "Not seven, sorry"
	
factorial :: (Integral a) => a -> a

factorial 0 = 1 
factorial n = n * factorial (n - 1)

addV :: (Num a) => (a,a)->(a,a)->(a,a)
addV (a,b) (x,y) = (a+x, b+y)


