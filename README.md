# Final Project

Final project for programming paradigms.



## Issue tracker
[ ] - comments
[+] - complicated arithmetic
[ ] - fork/join
[ ] - lock/unlock
[ ] - arrays
[?] - and/or

## Language features (goal)
 
### Data types
 - integers
 - booleans
 - arrays
 - strings
 
### Heap management

Finding space on the heap to allocate to the process but no deallocation
or garbage collection. 

### Expressions and variables
 - typed variables
 - nested scopes
 - compile-time check for being initialized
 - operations:
    - +, -, *, ==, <=, >=

### Statements
 - assignments (for arrays copy the contents)
 - if
 - while
 
### Concurrency
 - creating threads ([fork-join](https://en.wikipedia.org/wiki/Fork%E2%80%93join_model))
 - locking
 - nested forking but not forking in nested scopes
 - access to both local and global variables (annotate in language)
 - Sprockell multithreading, since the architecture does not support dynamic threading creation
 - Fork/join model in the architecture, forked thread could be forked again
 - Look through : WriteInstr, ReadInstr/Receive, and TestAndSet

### Procedures
 - functions only
 
 
### Code generation
Add some self made instructions that generate some parts of Sprockell to
make code generation easier.

## Grammar

```antlrv4
grammar README;

program : definition *;

definition : functionDef
           | variableDef SEMI
           | globalDef SEMI;

// Variables
variableDef : type identifier ASS expression;

array : LSQBRACKET (item ',') * item RSQBRACKET;

type: BOOLTYPE | INTTYPE | ARRTYPE;

globalDef: GLOBAL (identifier ',')* identifier;  

// Functions
functionDef : FUNCTION retType identifier LPAR params RPAR LBRACK statement+ RBRACK;

retType: type | VOID;

params: ((type identifier ',')* type identifier)*;


// Language
statement : defintion
          | if
          | while
          | RETURN identifier
          | expression;

if: IF LPAR bool RPAR LBRACK statement * RBRACK (ELSE LBRACK statement * RBRACK )*;

while: WHILE LPAR bool RPAR LBRACK statement * RBRACK;

bool: expression ordering expression | identifier;

ordering: LT | LE | EQ | NE | GT | GE;


//TODO more work on making this not left-recursive and correctly associative
expression : term
     | term ( '+' | '-' ) expression;

term : factor
     | factor '*' term;

factor : INTEGER | BOOLEAN | array
       | identifier LPAR ((arg ',')* arg) * RPAR
       | identifier
       | '(' expr ')';

```


## Testing





## Sample programs
 - [Peterson's algorithm](https://en.wikipedia.org/wiki/Peterson%27s_algorithm)
 - ```An elementary banking system, consisting of several processes trying to transfer money simultaneously. Your implementation should ensure that concurrent transfers do not mess up the state of the bank account.```
 