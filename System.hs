module System
(Files (..)
 ,fileSystem
 ,ckeckFileExist
 ,ckeckAndRemove
 ,isEmpty
 )where 
import String

data Files 
          = None
          | Files String String [Files] 
		  deriving (Eq, Ord, Read, Show)

fileSystem =
    Files "/" "/" [ Files ".." "/" [None]
	               ,Files "folder1" "dir" [ Files "file1.txt" "" [None]
				                           ,Files "file2.txt" "" [None]
										   ,Files ".." "/" [None]
										   ,Files "folder1" "dir" [ Files"file1.txt" "" [None]
										                           ,Files ".." "/folder1" [None]
																  ]
										   ,Files "folder2" "dir" [ Files "file1.txt" "" [None]
										                           ,Files ".." "/folder1"[None]
																   ,Files "folder1" "dir"[ Files "file1.txt" "" [None]
																                          ,Files "file2.txt" "" [None]
																						 ]
																  ]
										   ]
					,Files "folder2" "dir" [ Files "file1.txt" "" [None]
					                        ,Files ".." "/" [None]
					                        ,Files "folder1" "dir" [ Files "file1.txt" "" [None]
					                                                ,Files ".." "/folder2" [None]
					                                                ,Files "file2.txt" "" [None]
							                                        ,Files "file3.txt" "" [None]
																   ]
										   ]
                     ,Files "file1.txt" "" [None]
					 ,Files "file2.txt" "" [None]
				  ]
 
isEmpty :: Files -> Bool
isEmpty None = True
isEmpty _    = False

--search file path  in the form ["/","folder1","file1.txt"] in system and return tuple -> (fileExist?,type-dir or file,list of ad)
ckeckFileExist :: [String] -> Files -> (Bool,String,[Files])
ckeckFileExist  path system  = ckeckFileExist' path "" [] system                                                         														  
  where
    ckeckFileExist' :: [String] -> String -> [Files] -> Files -> (Bool,String,[Files])
    ckeckFileExist' []  what listOfAdj  _    = (True,what,listOfAdj)
    ckeckFileExist' _    _    _  None    = (False,"",[])
    ckeckFileExist' path@(x:xs) what listOfAdj files@(Files _ _ list) 
        | x == "/" && null xs = (True,"dir",list)
        | x == "/"  = ckeckFileExist' xs what listOfAdj files 
        | otherwise = ckeckFileExistInList path what listOfAdj list  
          where
            ckeckFileExistInList :: [String] -> String -> [Files] -> [Files] -> (Bool,String,[Files])
            ckeckFileExistInList []   _  _  _     = (False,"",[])
            ckeckFileExistInList  _   _  _  []    = (False,"",[])
            ckeckFileExistInList  _   _  _ [None] = (False,"",[])
            ckeckFileExistInList path@(x:xs) what listOfAdj (files@(Files name specInfo list ): rest)
                | name == x = if specInfo=="dir"
                                  then ckeckFileExist' xs "dir" list  files 
				                  else ckeckFileExist' xs "file"list  files 
                | otherwise = ckeckFileExistInList path what listOfAdj rest

--search file path  in the form ["/","folder1","file1.txt"] in system and remove it if exist and isn't a dir
--return the newFileSystem without the file or the same system if the file ins't exist or  is a dir
ckeckAndRemove :: [String] -> Files -> Files
ckeckAndRemove [] _ = None
ckeckAndRemove _ None = None
ckeckAndRemove path@(x:xs) files@(Files name specInfo list) 
    | x == "/" && null xs = files
    | x == "/" = ckeckAndRemove xs files
    |otherwise = (Files  name specInfo (ckeckAndRemoveDepth path list))
  where
    ckeckAndRemoveDepth::[String]->[Files]->[Files]
    ckeckAndRemoveDepth [] _ = [None]
    ckeckAndRemoveDepth _ [] = []
    ckeckAndRemoveDepth _ [None] = [None]
    ckeckAndRemoveDepth path@(x:xs) currList@(currFile@(Files name specInfo list):rest) 
        | x == name && null xs && specInfo == "" = rest
        | x == name && null xs && specInfo /= "" = currList
        | x == name = (ckeckAndRemove xs currFile):rest
        | otherwise = currFile : (ckeckAndRemoveDepth path rest)