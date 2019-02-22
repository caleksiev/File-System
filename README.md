# File-System in Haskell

The project represent simulation of the file system in the Unix-based system, supporting basic command procedures such as ls, rm, cat, cd, pwd.

## Repository organization:

### FileSystem
Console interface, menu and basic function.
### Commands
Commands realization - pwd, ls, cd, rm.
### Files
The realization of the "cat" command (it's created in a separate module, because works directly with files, unlike other commands)
### System
The whole system structure and the main funtions, that works with it.
### String
Work and transform the input commands, for easy parsing.
