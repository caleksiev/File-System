module Commands 
(pwd
,cd
,ls
,rm
,checkTypeOfPath
)where

import System
import Data.IORef
import String
import System.IO

pwd  ::Files -> String
pwd None            = ""
pwd (Files _ dir _) = dir

cd  ::[[String]] -> IORef Files -> IO ()
cd [] _  = return ()
cd path@(x:y:xs) refSystem = do
	system <- readIORef refSystem
	let path = checkTypeOfPath y system
	let (exist,what,_) = ckeckFileExist path system
	if exist && what == "dir"
		then do
			let newCd= changeCD (concatString path) system
			if isEmpty newCd
				then do
					putStrLn "The directory is a file!"
				else do
					writeIORef refSystem newCd
		else do 
			putStrLn "The directory dosn't exist!"
			
changeCD ::String -> Files -> Files
changeCD _ None                    = None
changeCD [] _                      = None
changeCD newCD (Files name _ list) = Files name newCD list

ls::[[String]]->Files->IO()
ls [] _            = return ()
ls _ None          = return ()
ls (x:[])   system = do 
	let (_,_,list) = ckeckFileExist (separateString (pwd system)) system
	showContOfDir list -- ^ ls with 0 args
ls (x:y:[]) system = do
	let (exist,what,list) = ckeckFileExist (checkTypeOfPath y system) system -- ^ls with 1 arg
	if (exist && what=="dir")
		then do 
			showContOfDir list
		else do
			putStrLn "The dir dosnt't exist or is a file.To see the content of file use cat command!"
							 
showContOfDir :: [Files] -> IO ()
showContOfDir []      = return()
showContOfDir [None]  = return ()
showContOfDir ((Files name _ _):[])
	| name ==".."     = putStrLn ""
	| otherwise       = putStrLn name
showContOfDir ((Files name _ _ ):rest)
	| name == ".."    = showContOfDir rest
	| otherwise       = do
		putStr $ name ++ " "
		showContOfDir rest

rm :: [[String]] -> IORef Files -> IO ()
rm [] _         = return ()
rm(x:xs) system = do
	currFileSystem <- readIORef system
	let path = checkTypeOfPath x currFileSystem
	let newFileSystem = removeFile path currFileSystem
	if  newFileSystem == currFileSystem -- ^ if there isn't change -> the file dosnt'exist or is a dir
		then do
			putStrLn $ "Can't remove " ++ concatString path
			rm xs system
		else do
			writeIORef system newFileSystem
			rm xs system 

removeFile :: [String] -> Files -> Files 
removeFile [] _        = None
removeFile _  None     = None
removeFile path system = ckeckAndRemove path system

--if the file path is relative transform it to full
checkTypeOfPath :: [String] -> Files -> [String]
checkTypeOfPath [] _ = []
checkTypeOfPath list@(x:xs) fileS
	| x == "/"       = list
	| otherwise      = separateString (pwd fileS) ++ list