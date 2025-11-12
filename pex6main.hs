-- pex6.hs 
-- unKnot Haskell

-- name: 

{- DOCUMENTATION:
-}
unKnot :: [(Char, Char)] -> String
unKnot tripCode
   | null tripCode = "unknot"
   | otherwise = "tangle - resulting trip code: " ++ (show tripCode)

-- typeIknot :: [(Char, Char)] -> String
-- typeIknot tripCode

main :: IO ()
main = do
   let t01 = [('a','o'),('a','u')]
   print("   test case t01 - tripcode: " )
   print(t01)
   print("   result:" ++ unKnot t01)

