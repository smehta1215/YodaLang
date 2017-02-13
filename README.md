# YodaLang: an internal Scala DSL

This DSL was submitted as the final project for Rellermeyer's CS 345 Programming Languages course in Fall 2016. 

Created by Sachin Mehta.

## What is YodaLang?

YodaLang is an internal Scala DSL that uses English-like syntax and semantics (with a Star Wars Yoda twist) and 
allows for Turing Complete programming. The language is type safe and contains basic type inferencing elements. An
additional feature of this language extends into physics, and allows the programmer to perform simple "force" based 
calculations on created objects such as planets and light-sabers. 

## Language Specification

YodaLang utilizes the constructs and conventions of Scala to make programming similar to generating the instructions 
in English; there are minimal requirements for parenthesis, periods, or semi-colons, however this does cause certain 
language restrictions. YodaLang works by traversing through the instructions first and storing them in a map that is
ordered to work as a stack, and then executes corresponding Scala code in the appropriate order.

Example program layout:

<pre><code>
import yodalang.YodaLang
<br />
object Program_Name extends YodaLang {
  def main(args: Array[string]) = {
    <br />
    Begin we will
    <br />
    ...YodaLang code...
    <br />
    Finish we will
    <br />
  }
}
</pre></code>

### YodaLang Basics

- Begin we will 

Indicates the start of every YodaLang program.

- Show me ___ you will

Displays the text in the blank to the console. The blank can be composed of strings, variables, or expressions.

- Force push "var_name" as ___

Stores the value, string, or expression in the blank as the var_name.

- Finish we will

Signifies the end of a YodaLang program.

### Simple Operations 

- Fear leads "var" anger ___

Adds a number, expression, or variable provided in the blank to the variable var assuming it is a number.

- Anger leads "var" hate ___

Subtracts a number, expression, or variable provided in the blank to the variable var assuming it is a number.

- Hate leads "var" suffering ___

Multiplies a number, expression, or variable provided in the blank to the variable var assuming it is a number.

- Suffering leads "var" darkness ___

Divides a number, expression, or variable provided in the blank to the variable var assuming it is a number.

### Conditionals and Loops

- Jedi path (___)

Tests if the expression in the blank is true; the "if" statement.

- Take Sith path

The else path if the expression is false.

- Restore the balance you will

Ends the if-else conditional. 

- What you learned

Start of the while loop.

- You must unlearn

End of the while loop that sends the program back to the first while loop instruction.

- Patience young padawan

Breaks the program out of the while loop.

### Functions

- Start training "function_name" you will

Creates a function with the specificed function_name.

- Give back "var" you will

Returns a variable var from the function.

- Stop your training you will

Indicates the end of a function.

- Force pull "function_name"  or  Force pull ("function_name", "return_var")

Calls a function, the second style calls a function and passes in a variable to hold the return value.

## Sources Used in the Creation of YodaLang

BAYSICK - https://github.com/fogus/baysick

basic.scala - https://gist.github.com/jrk/87146

gimme - https://github.com/l-hoang/gimme

scala-chef - https://github.com/l-hoang/scala-chef


