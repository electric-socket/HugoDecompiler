I wanted to learn how the Hugo compiler creates binaries for use of its run-time system. Hugo is a domain-specific programming language designed for creating interactive fiction. 

Hugp "programs" are created by specifying a set of values:
First, the command sequences, the list of verbs and "syntactic sugar" or additional words to allow more natural language. 
The following may come in any order:
* Specification of attributes, a set of flags used to describe certain conditions in the game.
* Specification of properties, specic values held by an object used to change gameplay.
* Specification of objects, the entities that are manipulated as part of the game.
* Global varables such as the player's score, how many moves they've made, and other information.
* Routines, pieces of code that are activated when the player types in a command.

Objects in the game include the player, NPCs, if any, items that can be moved, picked up, looked at, or used. They also include the various locations in the game.

All this information is then translated into tokens which can be interpreted by the game engine.

The program that this repo holds reads a compiled game into memory, and using the information from the reference book, displays the information about the game in a readable format.

To compile the source, you need the QB64 Phoenix Edition, which can be found [here](https://github.com/QB64-Phoenix-Edition/QB64pe).
