package yodalang

import scala.annotation.migration
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
// ^^Import lines above^^

class YodaLang {

  // Basic instruction line in YodaLang
  abstract sealed class YodaLine;

  // These handle displaying to the console
  case class PrintString(num: Int, s: String) extends YodaLine;
  case class PrintNumber(num: Int, s: Any) extends YodaLine;
  case class PrintFunction(num: Int, s: Function0[Any]) extends YodaLine;

  // Variable assignment
  case class Assign(num: Int, fn: Function0[Any]) extends YodaLine;

  // Handles end of program call
  case class End(num: Int) extends YodaLine;

  // Holds current instruction line number for execution
  var lineNumber: Int = 1;

  // Stack holds loops line numbers
  var loopStack = Stack[Int]();
  var afterLoopStack = Stack[Int]();

  // Structures that hold line information
  var lines = new HashMap[Int, YodaLine];
  val variables = new Variables;

  // Evaluates all instructions (runtime) with call Go
  private def gotoLine(line: Int) {
    lines(line) match {
      case PrintString(_, s: String) => {
        println(parseForVar(s));
        gotoLine(line + 1);
      }

      case PrintNumber(_, s: Int) => {
        println(s);
        gotoLine(line + 1);
      }

      case PrintNumber(_, s: Double) => {
        println(s);
        gotoLine(line + 1);
      }

      case PrintFunction(_, s: Function0[Any]) => {
        println(s());
        gotoLine(line + 1);
      }

      case Assign(_, fn: Function0[Any]) =>
        {
          fn();
          gotoLine(line + 1);
        }

      case End(_) => {};
    }
  }

  // Returns the value of a variable from the HashMap
  def extractVal(s: String): Any = {
    return variables.variablesScope(variables.getDepth())(s);
  }

  // Parses display string for presence of variable 
  // with designator 'v|'
  def parseForVar(s: String): String = {
    var parsed: Array[String] = s.split(" ");

    var index: Int = 0;
    var newString: String = "";
    while (index < parsed.length) {
      if (parsed(index).charAt(0).equals('v') && parsed(index).charAt(1).equals('|')) {
        var varName: String = parsed(index).substring(2);

        // Print error message if variable does not exist on the HashMap level
        if (variables.variablesScope(variables.getDepth()).contains(varName)) {
          newString += extractVal(varName) + " ";
        }
      } else {
        newString += parsed(index) + " ";
      }
      index += 1;
    }

    return newString;
  }

  // Assignment of variables
  case class Assignment(s: String) {
    def as(i: Int) = {
      lines(lineNumber) = Assign(lineNumber, (() => variables.set(s, i)));
      lineNumber += 1;
      LineTermination;
    }
    def as(z: String) = {
      lines(lineNumber) = Assign(lineNumber, (() => variables.set(s, z)));
      lineNumber += 1;
      LineTermination;
    }
    def as(d: Double) = {
      lines(lineNumber) = Assign(lineNumber, (() => variables.set(s, d)));
      lineNumber += 1;
      LineTermination;
    }
    def as(f: Function0[Any]) = {
      lines(lineNumber) = Assign(lineNumber, (() => variables.set(s, f)));
      lineNumber += 1;
      LineTermination;
    }
  }

  // Line terminating word "will"
  abstract sealed class WillWord;
  object will extends WillWord;

  // Starts YodaLang program
  object Begin {
    def we(w: WillWord) = {
      lines = new HashMap[Int, YodaLine];
      variables.setDepth(variables.getDepth() + 1);
      variables.createScope();
    }
  }

  // Ends YodaLang program
  object Finish {
    def we(w: WillWord) = {
      lines(lineNumber) = End(lineNumber);
      gotoLine(lines.keys.toList.sorted.head);
    }
  }

  object LineTermination {
    def you(w: WillWord) = {
      // Potentially change next line features
    }
  }

  // Starts off variable assignment, Create is the word that starts 
  // a new variable
  object Force {
    def push(s: String) = Assignment(s);
  }

  // Prints to console
  object Show {
    def me(s: String) = {
      lines(lineNumber) = PrintString(lineNumber, s);
      lineNumber += 1;
      LineTermination;
    }

    def me(s: Int) = {
      lines(lineNumber) = PrintNumber(lineNumber, s);
      lineNumber += 1;
      LineTermination;
    }
    def me(s: Double) = {
      lines(lineNumber) = PrintNumber(lineNumber, s);
      lineNumber += 1;
      LineTermination;
    }
    def me(s: Function0[Any]) = {
      lines(lineNumber) = PrintFunction(lineNumber, s);
      lineNumber += 1;
      LineTermination;
    }
  }

}