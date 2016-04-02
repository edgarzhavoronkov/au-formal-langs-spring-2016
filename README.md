### Parser example

#### Description:
This is a parser for tiny language written as my home assignment for Formal Languages course @ SPbAU

#### Language:
Language L consists of variables(strings), integers, binary arithmetic and logic operations.

##### Grammar:  
* Expr -> Var | Int | Expr Op Expr  
* Op -> + | - | * | / | % | == | != | < | > | <= | >= | && | ||
* Stmt -> Skip | Var := Expr | Stmt ; Stmt | write Expr | read Expr | while Expr do Stmt | if Expr then Stmt else Stmt

#### Tools used:
Parser is written using Alex and Happy - lexer and parser generator for Haskell

#### Running:
Make sure you have Glorious Haskell Compiler, Cabal(package manager), Alex and Happy. You can get everything from standard repos in Ubuntu running `sudo apt-get install haskell-platform` in your favorite terminal emulator.

After you check out this repo and change into directory type `cabal configure` and `cabal build`. It should build executables stored in `./dist/build/${TARGET_NAME}`

There are three targets.  
1. main. To run it type `./dist/build/main/main`. Main program takes string - your program and shows AST, if it was successfully built.
2. testLexer. There are two hardcoded tests for lexer in `tests` folder and `./dist/build/testLexer/testLexer` simply runs them.
3. testParser. There are also two hardcoded tests for parser in `tests` folder and `./dist/build/testParser/testParser` simply runs them.
