# EDSL example in Haskell

## The Stream language

This is a small domain specific language, embedded in Haskell, for education purposes. The language can be used to describe stream computations. For example, the stream
````
(input :: Stream Int) >>> group 3 0 (+) >>> foreach (==10) >>> output
````
inputs a stream of integers (e.g. 2, 4, 4, 0, 5, -3, ...), then forms groups of 3 numbers and sums each group (starting with the initial value 0): 2+4+4=10, 0+5+(-3)=2, ... The sums are each compared to 10 (10\==10: true, 2\==10: false) and the resulting boolean values form the output stream (true, false, ...).

See the `Example.hs` file for more example streams.

## Shallow and deep embedding

The project contains two implementations of this streaming language. In case of shallow embedding, executing the streams as Haskell expressions will directly execute the streams. When deep embedding is used, executing the streams as Haskell expressions will build up the abstract syntax tree (AST) of the stream. The AST can be used both for compilation and interpretation. Streams of this project are compiled to C++.

## Usage

The project has been tested with GHC version 8.0.1.

Using the shallow implementation:
````
$ ghci -ishallow Examples.hs 
*Main> f6
2
4
4
True
0
5
-3
False
...
````

Using the deep implementation (execution, compilation and compilation to file):
````
$ ghci -ideep Examples.hs 
*Main> execute f6
2
4
4
True
0
5
-3
False
...
*Main> compile f6
#include <iostream>

int main() {
  while(true) {
...
}
*Main> compileToFile f6 "f6.cc"
````
