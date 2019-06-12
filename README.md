# Final Project

Final project for programming paradigms.

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

### Procedures
 - functions only
 
 
### Code generation
Add some self made instructions that generate some parts of Sprockell to
make code generation easier.

## Testing





## Sample programs
 - [Peterson's algorithm](https://en.wikipedia.org/wiki/Peterson%27s_algorithm)
 - ```An elementary banking system, consisting of several processes trying to transfer money simultan- eously. Your implementation should ensure that concurrent transfers do not mess up the state of the bank account.```
 