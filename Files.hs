module Files
(cat
)where

import System
import String
import Commands

cat :: [[String]] -> Files -> IO ()
cat [] system = print00 
	-- the cat is called with zero args
cat args@(x:xs) system
	| not(null (checkOutFile args)) && checkInFile args     = do
		let outFile = checkOutFile args
		let pathOutFile = (checkTypeOfPath outFile system)
		let (exist,what,_)= ckeckFileExist pathOutFile system
		if(exist && what=="file")
			then do 
				cat11 args pathOutFile system 
				-- ^ output and input file are available
			else do 
				putStrLn "Incorrect output file!"
	| not(null(checkOutFile args)) && not (checkInFile args) = do
		let outFile = checkOutFile args 
		let pathOutFile = checkTypeOfPath outFile system
		let (exist,what,_)= ckeckFileExist pathOutFile system
		if(exist && what=="file") 
			then do
				print01 pathOutFile 
				-- ^ not intput file,but output file is available
			else do 
				putStrLn "Incorrect output file!"
	| otherwise = cat10 args system 
	-- ^ intput is available,but not output
 
--there are output and input file
--read content of every input file and write it to the output file
cat11 :: [[String]] -> [String] -> Files -> IO ()
cat11 [] _  _    = return ()
cat11 _  [] _    = return ()
cat11 (x:xs) outFile system
	| x == [">"] = return ()
	| otherwise  = do 
		let (cond,what,_) = ckeckFileExist (checkTypeOfPath x system) system
		if(cond && what=="file")
			then do 
				print11 (checkTypeOfPath x system) outFile
				cat11 xs outFile system
			else do 
				putStrLn $ "Can't open" ++ concatString (checkTypeOfPath x system)
				cat11 xs outFile system

print11 :: [String] -> [String]  ->IO ()
print11 [] _             = return ()
print11 _ []             = return ()
print11 readFrom writeTo = do
	content <- readFile (concatStringFile readFrom)
	appendFile (concatStringFile writeTo) (content ++ "\n")
						   
--there isn't output file,but the inputs are available
--read content of every input file and write it to the terminal						   
cat10 :: [[String]] -> Files -> IO ()
cat10 []       _    = return ()
cat10 (x:xs) system = do 
	let (cond,what,_) = ckeckFileExist (checkTypeOfPath x system) system
	if(cond && what=="file")
		then do
			print10 (checkTypeOfPath x system) 
			cat10 xs system
		else do 
			putStrLn $ "Can't open" ++ concatString (checkTypeOfPath x system)
			cat10 xs system
			
print10 :: [String] -> IO ()
print10 []   = return ()
print10 path = do 
    content <- readFile (concatStringFile path)
    putStrLn content
							   
--there aren't output and input files
--read line from terminal and print it back,
--until "." isn't entered					  
print00 :: IO ()							 
print00  = do
    content <- getLine
    if content == "."
        then do 
            return ()
        else do
            putStrLn content
            print00
		 
--there isn't input file,but output file is available
--read line from terminal and write it to output file the,
--until "." isn't entered
print01 :: [String] -> IO ()
print01 path = print01'(concatStringFile path)
  where 
  print01' :: String -> IO ()
  print01' []   = return ()
  print01' path = do
      content <- getLine
      if content == "."
          then do
              return ()
          else do
              appendFile path (content ++ "\n")
              print01' path

--check there is passed output file										 
checkOutFile :: [[String]] -> [String]
checkOutFile [] = []
checkOutFile(x:xs)
	| x == [">"] && not(null xs) && (length xs == 1) 
	            = head xs
	| otherwise = checkOutFile xs
 
 --check there are passed input files	
checkInFile :: [[String]] -> Bool
checkInFile []   = False
checkInFile (x:xs)
	| x /= [">"] = True
	| otherwise  = False

--the name of every file out of the tree is his full path from "/"-> 
--"-folder1-file" instead "/folder1/file1"
--from list of paths transform the name of file
concatStringFile :: [String] -> String
concatStringFile []     = []
concatStringFile (x:[]) = x
concatStringFile (x:xs)
    |x /= "/"  =  x  ++ "-" ++ (concatStringFile xs) 
    |otherwise = "-" ++ concatStringFile xs