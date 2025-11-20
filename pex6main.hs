-- pex6main.hs 
-- unKnot Haskell

-- name: Tyler Clifton

{- DOCUMENTATION: I sat down with Gavin Smith to go over the problem conceptually. 
He didn't actually show me any code, only explained how he thought through recursively
checking for wrap-around Type-II knots. I implemented some of that logic in my code.
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
typeI (p1@(x1,t1) : p2@(x2,t2) : rest)
    | x1 == x2  = typeI rest
    | otherwise = p1 : typeI (p2 : rest)

-- safely get the last element and the list without it
safeLastInit :: [a] -> Maybe (a, [a])
safeLastInit [] = Nothing
safeLastInit xs = Just (last xs, init xs)

-- Remove the first adjacent pair matching a given order and where types are equal
-- and differ from the head type. Skip any adjacent same-label pair, regardless of type.
removeFirstAdjPair :: [(Char,Char)] -> (Char,Char) -> Char -> ([(Char,Char)], Bool)
removeFirstAdjPair [] _ _ = ([], False)
removeFirstAdjPair [x] _ _ = ([x], False)
removeFirstAdjPair (p1@(x1,t1') : p2@(x2,t2') : rest) (o1,o2) tHead
    -- Skip local Type-I blockers (same crossing adjacent), regardless of type
    | x1 == x2 =
        removeFirstAdjPair rest (o1,o2) tHead

    -- Target adjacent pair must be in the specified order (SAME order),
    -- same type as each other, and that type must differ from the head type.
    | x1 == o1 && x2 == o2 && t1' == t2' && t1' /= tHead =
        (rest, True)

    -- Continue scanning
    | otherwise =
        let (restRes, found) = removeFirstAdjPair (p2 : rest) (o1,o2) tHead
        in (p1 : restRes, found)

-- Try to find a Type-II starting at head of list
-- If found, remove head two and the matching adjacent end pair
findTypeIIAtHead :: [(Char,Char)] -> Maybe [(Char,Char)]
findTypeIIAtHead (p1@(x1,t1) : p2@(x2,t2) : rest)
    | t1 /= t2 = Nothing
    | otherwise =
        let tailReduced = reduce rest
            orderSame = (x1, x2)
            orderRev  = (x2, x1)
            (afterRemSame, removedSame) = removeFirstAdjPair(tailReduced) orderSame t1
            (afterRemRev,  removedRev)  = removeFirstAdjPair(tailReduced) orderRev  t1
        in if removedSame then Just afterRemSame
           else if removedRev then Just afterRemRev
           else Nothing
findTypeIIAtHead _ = Nothing

-- rotates list
rotate :: [(a)] -> [(a)]
rotate [] = []
rotate (h:t) = t ++ [h]

-- find length
listLen :: [a] -> Int
listLen []     = 0
listLen (_:t)  = 1 + listLen t

-- rotate left n times
rotateTimes :: [a] -> Int -> [a]
rotateTimes xs 0 = xs
rotateTimes xs n = rotateTimes (rotate xs) (n - 1)

-- Try to find a Type-II anywhere by rotating.
-- When a match is found at rotation k
-- get a reduced list in that rotated frame and rotate it back to restore original orientation
findTypeII :: [(Char,Char)] -> Maybe [(Char,Char)]
findTypeII trip = tryRotations trip trip 0
  where
    tryRotations :: [(Char,Char)] -> [(Char,Char)] -> Int -> Maybe [(Char,Char)]
    tryRotations orig current k
        | null current = Nothing
        | k /= 0 && current == orig = Nothing
        | otherwise =
            case findTypeIIAtHead current of
                Just reducedAtRotatedFrame ->
                    let fullyReducedRotated = reduce reducedAtRotatedFrame
                        m = listLen fullyReducedRotated
                        backRotations = if m == 0 then 0 else (m - (k `mod` m)) `mod` m
                        reducedInOrig = rotateTimes fullyReducedRotated backRotations
                    in Just reducedInOrig
                Nothing ->
                    tryRotations orig (rotate current) (k + 1)

-- apply Type-I then Type-II
reduceOnce :: [(Char,Char)] -> [(Char,Char)]
reduceOnce trip =
    let t1 = typeI trip
    in case findTypeII t1 of
         Just r -> r
         Nothing -> t1

-- Fully reduce by repeatedly applying reductions until no change
reduce :: [(Char,Char)] -> [(Char,Char)]
reduce trip =
    let next = reduceOnce trip
    in if next == trip then trip else reduce next

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

   -- extra test cases used earlier
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

