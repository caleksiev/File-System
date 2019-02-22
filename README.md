# File-System in Haskell

File systems are designed to organize data in blocks of namespaces called files. The files are organized in a hierarchical structure by using directories. In Unix-based systems, the root of the file system is indicated by "/", the same symbol being used for a directory separator.

### The project represent simulation of the file system in the Unix-based system, supporting basic command procedures such as ls, rm, cat, cd, pwd.

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
