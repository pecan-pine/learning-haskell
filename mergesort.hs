import Debug.Trace

debug = flip trace

ci arr =  print $ show $  st (arr, 0)

--n is number of swaps
st :: (Ord a, Show a) => ([a], Integer) -> ([a], Integer)
st ([], n) = ([], n)
st ([a], n) = ([a],n)
st ([a,b], n) = if a <= b then ([a,b],n) else ([b,a],n+1) `debug` show ("pair to swap:",[a,b], "adding this to n:", 1)
st ((a:b:c:cs), n) = merge (st ([a,b],n)) (st ((c:cs), 0))

merge:: (Ord a, Show a) => ([a], Integer) -> ([a], Integer) -> ([a], Integer)
merge (xs, n) ([], m) = (xs, n + m)
merge ([],n) (ys, m) = (ys, n+m)
merge ((x:xs),n) ((y:ys), m) = if x <= y
    then (x : fst result0, snd result0)
    else (y : fst result1, snd result1)
    where 
        result0 = merge (xs,n) ((y:ys), m)
        result1 = merge ((x:xs),n) (ys, m+(fromIntegral ( length (x:xs)))) --`debug` show ("swapping lists:", (x:xs), (y:ys), "adding this to m:", length((x:xs)) )