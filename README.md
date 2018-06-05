# Minimizator for Boolean algebra expressions
## Overview
Given the input string of a term in the language of boolean algebras, 
program parses it and produces as the output all minimal forms of the
particular term.

"B-term minimizator" uses Quine–McCluskey (or method of prime implications) 
algorithm to minimze given terms.

-- TODO more detailed descriptions.

## How to use
Program is written in Scala. Using it is  pretty straightforward; first make sure
you have java and jdk installed, then download [SBT](https://www.scala-sbt.org/`) and
install it.

Clone this repository.

Navigate to the directory of cloned repo, and in command line type `sbt` to
run sbt, which will download required dependencies and generate project files.
Once sbt console is running, type `compile` and if that goes without any errors,
type in `run` to execute the program. 

(NOTE: You have to change source file in order to give a different formula,
command/file input is not yet implemented, working on it!)

