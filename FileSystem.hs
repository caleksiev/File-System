module FileSystem
(main
)where

import Commands
import Data.IORef
import System
import String
import Files

main :: IO ()
main = do 
    system <- newIORef fileSystem -- ^ Init the file system
    menu system
    putStrLn "Quit the program!"

menu :: IORef Files  ->IO ()
menu system = do 
    putStrLn "Enter command:"
    command <- getLine
    if null command
        then do
            return ()
        else do
            let x = extractArgs command
            if validArguments x
                then do
                    runCommand x system
                else do 
                    putStrLn "Wrong command!"
            menu system
 
runCommand :: [[String]] -> IORef Files -> IO()
runCommand [] _ = return ()
runCommand commands@(x:xs) system 
    |x == ["pwd"] = do 
        currFileSystem <- readIORef system
        putStrLn $ pwd  currFileSystem
    |x == ["cd"]  = do
        currFileSystem <- readIORef system
        cd commands system 
    |x == ["ls"]  = do
        currFileSystem <- readIORef system
        ls commands currFileSystem
    |x == ["rm"]  = do
        rm xs system
    |x == ["cat"] = do
        currFileSystem <- readIORef system
        cat xs currFileSystem
    |otherwise = return ()