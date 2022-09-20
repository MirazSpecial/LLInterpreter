# LLInterpreter
Interpreter of LittleLanguage written in Haskell as part of "Programming languages and paradigms" course at University of Warsaw

## Language
LittleLanguage is a very basic functional language. Its syntax is defined in ```grammer.cf``` file. LittleLanguage supports among others: lambda functions, higher order functions, partial functions, any type lists (recursive lists, function lists).

## Running
After compiling using ```make``` interpreter takes code file name as only argument, calculates all expressions, and prints the result on standard output.

## BNFC
BNFC was used to generate parser, changing language syntax in ```grammer.cf``` requires running ```bnfc --functor``` again on changed grammar definition.

## Examples
Working and incorrect examples are attached in adequate directories.
