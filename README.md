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

A compiled executable is in this repo; for version 0.1.16, the checksums are:


Algorithm       Hash                                                                   Path                                                   
---------       ----                                                                   ----                                                   
SHA256          A73D372F523333C8C704E58C1DD7DCFC7EC7AFA09AA5614679231CDBC3308AD3       colors.bas              
SHA256          F581F1207CDF52A22ED8EAC956593E80E7B8AF7BFED3982BC0373CE912C8464A       Common Dialog.bi        
SHA256          EBD5A3E879629C605DDD5D1A89F3D54CDFF77BD65FE1D7C5372C3BFC84306D42       Common_Dialog_Prefix.bi 
SHA256          B32F0631FAD7B87D42247EB4E54C894EBD773565C3C890E097B24C1E30FC15EA       Common_Dialog_Suffix.bi 
SHA256          0BAE2C588C748F86036BD92B696C2622B92521C3685EB362850F300077F38D2C       hugoconstants.bas       
SHA256          D3E8DD7619AF7A86E62A982952696F5FFA73263F373E106500AF273425078CB3       hugodecomp.bas          
SHA256          7944BF94D0859C7D626F612CBDCC0DBD74935B2C1068467F129E726E0FAEA6EA       hugodecomp.exe          
SHA256          8177F97513213526DF2CF6184D8FF986C675AFB514D4E68A404010521B880643       LICENSE                 
SHA256          649F09F6F8EE61BBA92DADF348A4F7EA65AD0E2AFEA9A97F73ECDBC5703940F8       PrintW.bas              
                    
