module String 
(separateString
,concatString
,extractArgs
,validArguments
)where

add el []     = [el]
add el (x:xs) = x:(add el xs)

--"/folder1/folder2/file1" -> ["/","folder1","folder2","file1"]
separateString :: String -> [String]
separateString []          = []
separateString path@(x:xs) = separateString' path (if x =='/' then ['/'] else [])
  where 
    separateString' [] res 
        | res == "" = []
        | otherwise = [res]
    separateString' (x:xs) result 
        | x == '/'  =  result : (separateString' xs [])
        | otherwise =  separateString' xs (add x result)

--["/","folder1","folder2","file1"] -> "/folder1/folder2/file1"
concatString :: [String] -> String
concatString []     = []
concatString (x:[]) = x
concatString (x:xs)
    | x /= "/"  =  x++ "/" ++ (concatString xs) 
    | otherwise = "/" ++ concatString xs

-- "ls /folder1/file1/" -> [["ls"],["/","folder1","file1"]]
extractArgs :: String -> [[String]]
extractArgs []  = []
extractArgs str = extractArgs' str []
  where
    extractArgs'  [] res 
        | res == "" = []
        | otherwise = (separateString res:[])
    extractArgs'(x:xs) res
        | x == ' ' && not(null res)
                    = (separateString res):(extractArgs' xs [])
        | x /= ' '  = extractArgs' xs (add x res)
        | otherwise = extractArgs' xs res

cntArguments :: [[String]] -> Int
cntArguments []     = 0
cntArguments (x:xs) = 1 + cntArguments xs

validArguments :: [[String]] -> Bool
validArguments [] = False
validArguments (x:xs)
    | (x == ["pwd"]) && (cntArguments xs == 0) 
                     = True
    | (x == ["cd"])  && (cntArguments xs == 1) 
                     = True
    | (x == ["ls"])  && (cntArguments xs == 0 || cntArguments xs == 1) 
                     = True
    | (x == ["rm"] ) && (cntArguments xs /= 0) 
                     = True
    | (x == ["cat"]) = True  
    | otherwise      = False