# Final Project

Final project for programming paradigms by: 

Jakub Orlinski - s18988956

Patrick Ignatius Patrick - s1969048

# Usage
To prepare the project for execution one should run the following

```
cd sprockell/
cabal install
cd ..
ghci Main.hs
```

Then inside Main.hs uncomment the lines as indicated on line 40,
depending on your architecture. This is needed on Unix systems
(as the file needs execution permissions) but the rest is just to 
have less clutter in `output/`. 

Then a couple of functions are at your disposal to use the language.
`compileToFile <insert path to .mc file>` - this lets you automatically compile the file to an executable and run it.
`parseFile <insert path to .mc file>` - this generates the IR or the program according to the microC language. 
`testSuite` - runs all the tests defined in samplesWithExp and returns a single boolean that indicates if all tests passed

#Explanation
The reinstallation of sprockell is needed as the sample programs
rely on more local data and shared memory than given by default.
Therefore the only changes in that directory are in BasicFunction.hs
where the constants are increased.

The output of the compilation can be found in `output/`,
the samples programs in `samples/` and the `tests/` directory 
holds some of the files needed to run tests - they contain the
expected output of the sample files.