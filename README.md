# File-System in Haskell

The project represent simulation of the file system in the Unix-based system, supporting basic command procedures such as ls, rm, cat, cd, pwd.

## Repository organization:

# FileSystem -> console interface, menu and basic function
# Commands -> commands realization - pwd, ls, cd, rm
# Files -> the realization of the "cat" command(it's created in a separate module, becaus works directly with files, unlike other commands
# System -> the whole system structure and the main funtions, that works with it
# String -> work and transform the input commands, for easy parsing
