-- pex6main.hs 
-- unKnot Haskell

-- name: Tyler Clifton

{- DOCUMENTATION: I sat down with Gavin Smith to go over the problem conceptually. 
He didn't actually show me any code, only explained how he thought through recursively
checking for wrap-around Type-II knots. I implemented some of that logic in my code.

I also referred to the following documentation for writing my code:
https://www.haskell.org/tutorial/patterns.html
https://livebook.manning.com/book/get-programming-with-haskell/chapter-7
https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-Maybe.html
https://zvon.org/other/haskell/Outputprelude/mod_f.html
-}

-- pex6main.hs

unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

-- Type-I: remove adjacent pairs with the same crossing
typeI :: [(Char,Char)] -> [(Char,Char)]
typeI [] = []
typeI [x] = [x]
typeI ((x1,t1):(x2,t2):rest)
    | x1 == x2  = typeI rest
    | otherwise = (x1,t1) : typeI ((x2,t2):rest)

-- Remove the first adjacent pair matching a given order and where types are equal
-- and differ from the head type. Skip any adjacent same-label pair.
removeFirstAdjPair :: [(Char,Char)] -> (Char,Char) -> Char -> ([(Char,Char)], Bool)
removeFirstAdjPair [] _ _ = ([], False)
removeFirstAdjPair [x] _ _ = ([x], False)
removeFirstAdjPair ((x1,t1):(x2,t2):rest) ord tHead
    | x1 == x2 =
        removeFirstAdjPair rest ord tHead
    | x1 == fst ord && x2 == snd ord && t1 == t2 && t1 /= tHead =
        (rest, True)
removeFirstAdjPair ((x1,t1):(x2,t2):rest) ord tHead =
    letRes (removeFirstAdjPair ((x2,t2):rest) ord tHead) (x1,t1)

-- matches the pair result
letRes :: ([(Char,Char)], Bool) -> (Char,Char) -> ([(Char,Char)], Bool)
letRes (restRes, found) p = (p:restRes, found)

-- Extractors for the pair returned by removeFirstAdjPair
removeFirstAdjPairBool :: ([(Char,Char)], Bool) -> Bool
removeFirstAdjPairBool (_,b) = b

removeFirstAdjPairOut :: ([(Char,Char)], Bool) -> [(Char,Char)]
removeFirstAdjPairOut (l,_) = l

findTypeIIAtHead :: [(Char,Char)] -> Maybe [(Char,Char)]
findTypeIIAtHead ((x1,t1):(x2,t2):rest)
    | t1 /= t2 = Nothing
    | removeFirstAdjPairBool (removeFirstAdjPair (reduce rest) (x1,x2) t1) =
        Just (removeFirstAdjPairOut (removeFirstAdjPair (reduce rest) (x1,x2) t1))
    | removeFirstAdjPairBool (removeFirstAdjPair (reduce rest) (x2,x1) t1) =
        Just (removeFirstAdjPairOut (removeFirstAdjPair (reduce rest) (x2,x1) t1))
    | otherwise = Nothing
findTypeIIAtHead _ = Nothing

-- Rotate list left by one
rotate :: [a] -> [a]
rotate [] = []
rotate (h:t) = t ++ [h]

-- Length of a list
listLen :: [a] -> Int
listLen [] = 0
listLen (_:t) = 1 + listLen t

-- Rotate left n times
rotateTimes :: [a] -> Int -> [a]
rotateTimes xs 0 = xs
rotateTimes xs n = rotateTimes (rotate xs) (n - 1)

-- Compute back rotations based on length and rotation count
backRotationsInt :: Int -> Int -> Int
backRotationsInt m k =
    if m == 0 then 0 else (m - (k `mod` m)) `mod` m

-- Rotate back to original orientation after working in rotated frame
rotateBack :: [(Char,Char)] -> Int -> [(Char,Char)]
rotateBack xs k =
    if listLen xs == 0
    then xs
    else rotateTimes xs (backRotationsInt (listLen xs) k)

-- Fully reduce the list obtained at rotated frame, then rotate back
fullyReduceAndRotateBack :: [(Char,Char)] -> Int -> [(Char,Char)]
fullyReduceAndRotateBack reducedAtRotatedFrame k =
    rotateBack (reduce reducedAtRotatedFrame) k

fullyReduceAndRotateBackFromMaybe :: Maybe [(Char,Char)] -> Int -> [(Char,Char)]
fullyReduceAndRotateBackFromMaybe (Just r) k = fullyReduceAndRotateBack r k
fullyReduceAndRotateBackFromMaybe Nothing _ = []

-- Try to find a Type-II anywhere by rotation
findTypeII :: [(Char,Char)] -> Maybe [(Char,Char)]
findTypeII trip = tryRotations trip trip 0

tryRotations :: [(Char,Char)] -> [(Char,Char)] -> Int -> Maybe [(Char,Char)]
tryRotations orig current k
    | current == [] = Nothing
    | k /= 0 && current == orig = Nothing
    | findTypeIIAtHead current == Nothing =
        tryRotations orig (rotate current) (k + 1)
    | otherwise =
        Just (fullyReduceAndRotateBackFromMaybe (findTypeIIAtHead current) k)

-- Apply Type-I then Type-II
reduceOnce :: [(Char,Char)] -> [(Char,Char)]
reduceOnce trip =
    if findTypeII (typeI trip) == Nothing
    then typeI trip
    else reduceOnceFound (findTypeII (typeI trip))

reduceOnceFound :: Maybe [(Char,Char)] -> [(Char,Char)]
reduceOnceFound (Just r) = r
reduceOnceFound Nothing  = typeI []

-- Fully reduce by repeatedly applying reductions until no change
reduce :: [(Char,Char)] -> [(Char,Char)]
reduce trip =
    if reduceOnce trip == trip
    then trip
    else reduce (reduceOnce trip)

-- performs reduction and returns the final string from unKnot
unKnotReduce :: [(Char,Char)] -> String
unKnotReduce trip = unKnot (reduce trip)

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnotReduce t01)

   let t02 = [('a','o'),('b','o'),('c','u'),('a','u'),('b','u'),('c','o')]
   print("   test case t02 - tripcode: " )
   print(t02)
   print("   result:" ++ unKnotReduce t02)

   let t03 = [('a','u'),('b','u'),('a','o'),('b','o')]
   print("   test case t03 - tripcode: " )
   print(t03)
   print("   result:" ++ unKnotReduce t03)

   let t04 = [('a','o'),('b','u'),('a','u'),('b','o')]
   print("   test case t04 - tripcode: " )
   print(t04)
   print("   result:" ++ unKnotReduce t04)

   let t05 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t05 - tripcode: " )
   print(t05)
   print("   result:" ++ unKnotReduce t05)

   let t06 = [('a','o'),('q','u'),('a','u')]
   print("   test case t06 - tripcode: " )
   print(t06)
   print("   result:" ++ unKnotReduce t06)

   let t07 = [('a','o'),('a','u'),('q','u')]
   print("   test case t07 - tripcode: " )
   print(t07)
   print("   result:" ++ unKnotReduce t07)

   let t08 = [('a','o'),('b','o'),('a','u'),('b','u'),('q','u')]
   print("   test case t08 - tripcode: " )
   print(t08)
   print("   result:" ++ unKnotReduce t08)

   let t09 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u')]
   print("   test case t09 - tripcode: " )
   print(t09)
   print("   result:" ++ unKnotReduce t09)

   let t10 = [('a','u'),('b','o'),('a','o'),('b','u'),('q','u'),('c','o'),('c','u')]
   print("   test case t10 - tripcode: " )
   print(t10)
   print("   result:" ++ unKnotReduce t10)

   let t11 = [('a','u'),('b','o'),('a','o'),('q','u'),('b','u'),('c','o'),('c','u')]
   print("   test case t11 - tripcode: " )
   print(t11)
   print("   result:" ++ unKnotReduce t11)

   let t12 = [('a','o'),('b','u'),('c','u'),('d','o'),('d','u'),('q','u'),('a','u'),('b','o'),('e','u'),('f','o'),('g','o'),('h','u'),('f','u'),('g','u'),('h','o'),('e','o'),('c','o')]
   print("   test case t12 - tripcode: " )
   print(t12)
   print("   result:" ++ unKnotReduce t12)

