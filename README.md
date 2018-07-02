# A didactical interpreter builded with OCAML

This is an interpreter for a didactical language called "giaco" builded for the "Languages and Compilers" course at University of Verona

## Getting started

This project is aimed at extending the OCAML’s interpreter (denotational or operational or iterative) of the teaching imperative and functional language, including blocks, procedures and functions (therefore without the objects’ part).  

1. Extend the teaching language with the string type (of chars) with string operations like: length len(s), concatenation @, sub-string substr(s,i,j) that returns the sub-string of s composed by chars from the i-th to the j-th if i<=y otherwise the empty string, and string constant ’s’ where s is a chars’ sequence. Extend therefore the commands of the language with the command reflect(s) which takes in input a string s and calls the same interpreter on the string seen as commands’ sequence.  

2. Develop an analysis of information-flow (in jargon: Taint Analysis) as dynamic typing of the teaching extended language as in step one. You must determinate the tainted streams (untrusted) during the computation of a program P. Imagine partition the input store and environment (boot program P) between trusted data (untainted) and untrusted (tainted). During the calculation, the computation of an operation (expression, command or statement) that composes tainted data with untainted data returns an untainted data if the result is independent from the tainted input. Conversely, if the output of the operation depends on a tainted data, the result is tainted. In the case of reflect(s), if the string s is tainted, block the computation because it might have a potential code injection attack. The analysis must provide a store and an environment with labelled output values (tainted or untainted) and must dynamically control the execution so as avoid code injection.


##
