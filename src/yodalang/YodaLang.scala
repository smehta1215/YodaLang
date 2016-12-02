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

  // These handle operations on variables using Yoda quote
  case class AddOperation(num: Int, s: String, v: Any) extends YodaLine;
  case class SubOperation(num: Int, s: String, v: Any) extends YodaLine;
  case class MultOperation(num: Int, s: String, v: Any) extends YodaLine;
  case class DivOperation(num: Int, s: String, v: Any) extends YodaLine;

  // These handle conditionals
  case class IfExpression(num: Int, fn: Function0[Boolean]) extends YodaLine;
  case class ElseMarker(num: Int, b: Boolean = false) extends YodaLine;
  case class CloseIfExpression(num: Int) extends YodaLine;

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

      case AddOperation(_, s: String, v: Int) => {
        // Below is checking to make sure the stored value is an Int 
        // and not a String
        var nv: Any = 0;
        if (extractVal(s).isInstanceOf[Int]) {
          nv = extractVal(s).asInstanceOf[Int] + v;
        }
        if (extractVal(s).isInstanceOf[Double]) {
          nv = extractVal(s).asInstanceOf[Double] + v;
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case AddOperation(_, s: String, v: Double) => {
        var nv: Double = 0;
        if (extractVal(s).isInstanceOf[Double] || extractVal(s).isInstanceOf[Int]) {
          if (extractVal(s).isInstanceOf[Double]) {
            nv = (extractVal(s).asInstanceOf[Double] + v);
          } else {
            nv = extractVal(s).asInstanceOf[Int] + v;
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case AddOperation(_, s: String, v: String) => {
        var nv: Any = 0;

        if (extractVal(s).isInstanceOf[Int]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Int] + extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Int] + extractVal(v).asInstanceOf[Double];
          }
        }
        if (extractVal(s).isInstanceOf[Double]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Double] + extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Double] + extractVal(v).asInstanceOf[Double];
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case SubOperation(_, s: String, v: Int) => {
        var nv: Any = 0;
        if (extractVal(s).isInstanceOf[Int]) {
          nv = extractVal(s).asInstanceOf[Int] - v;
        }
        if (extractVal(s).isInstanceOf[Double]) {
          nv = extractVal(s).asInstanceOf[Double] - v;
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case SubOperation(_, s: String, v: Double) => {
        // Below is checking to make sure the stored value is an Int 
        // and not a String
        var nv: Double = 0;
        if (extractVal(s).isInstanceOf[Double] || extractVal(s).isInstanceOf[Int]) {
          if (extractVal(s).isInstanceOf[Double]) {
            nv = (extractVal(s).asInstanceOf[Double] - v);
          } else {
            nv = extractVal(s).asInstanceOf[Int] - v;
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case SubOperation(_, s: String, v: String) => {
        var nv: Any = 0;

        if (extractVal(s).isInstanceOf[Int]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Int] - extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Int] - extractVal(v).asInstanceOf[Double];
          }
        }
        if (extractVal(s).isInstanceOf[Double]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Double] - extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Double] - extractVal(v).asInstanceOf[Double];
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case MultOperation(_, s: String, v: Int) => {
        var nv: Any = 1;
        if (extractVal(s).isInstanceOf[Int]) {
          nv = extractVal(s).asInstanceOf[Int] * v;
        }
        if (extractVal(s).isInstanceOf[Double]) {
          nv = extractVal(s).asInstanceOf[Double] * v;
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case MultOperation(_, s: String, v: Double) => {
        var nv: Double = 1;
        if (extractVal(s).isInstanceOf[Double] || extractVal(s).isInstanceOf[Int]) {
          if (extractVal(s).isInstanceOf[Double]) {
            nv = (extractVal(s).asInstanceOf[Double] * v);
          } else {
            nv = extractVal(s).asInstanceOf[Int] * v;
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case MultOperation(_, s: String, v: String) => {
        var nv: Any = 1;

        if (extractVal(s).isInstanceOf[Int]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Int] * extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Int] * extractVal(v).asInstanceOf[Double];
          }
        }
        if (extractVal(s).isInstanceOf[Double]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Double] * extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Double] * extractVal(v).asInstanceOf[Double];
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case DivOperation(_, s: String, v: Int) => {
        var nv: Any = 1;
        if (extractVal(s).isInstanceOf[Int]) {
          nv = extractVal(s).asInstanceOf[Int] / v;
        }
        if (extractVal(s).isInstanceOf[Double]) {
          nv = extractVal(s).asInstanceOf[Double] / v;
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case DivOperation(_, s: String, v: Double) => {
        var nv: Double = 1;
        if (extractVal(s).isInstanceOf[Double] || extractVal(s).isInstanceOf[Int]) {
          if (extractVal(s).isInstanceOf[Double]) {
            nv = (extractVal(s).asInstanceOf[Double] / v);
          } else {
            nv = extractVal(s).asInstanceOf[Int] / v;
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case DivOperation(_, s: String, v: String) => {
        var nv: Any = 1;

        if (extractVal(s).isInstanceOf[Int]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Int] / extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Int] / extractVal(v).asInstanceOf[Double];
          }
        }
        if (extractVal(s).isInstanceOf[Double]) {
          if (extractVal(v).isInstanceOf[Int]) {
            nv = extractVal(s).asInstanceOf[Double] / extractVal(v).asInstanceOf[Int];
          } else if (extractVal(v).isInstanceOf[Double]) {
            nv = extractVal(s).asInstanceOf[Double] / extractVal(v).asInstanceOf[Double];
          }
        }

        variables.set(s, nv);

        gotoLine(line + 1);
      }

      case IfExpression(_, fn: Function0[Boolean]) => {
        var jumpToElse: Boolean = false;
        var jumpToClose: Boolean = false;
        if (fn()) {
          gotoLine(line + 1);
        } else {
          var corrElse: Int = 1;
          while ((!lines(line + corrElse).isInstanceOf[ElseMarker] || jumpToElse) && (!lines(line + corrElse).isInstanceOf[CloseIfExpression] || jumpToClose)) {
            if (lines(line + corrElse).isInstanceOf[IfExpression]) {
              jumpToElse = true;
              jumpToClose = true;
              corrElse += 1;
            } else if (lines(line + corrElse).isInstanceOf[ElseMarker]) {
              jumpToElse = false;
              corrElse += 1;
            } else if (lines(line + corrElse).isInstanceOf[CloseIfExpression]) {
              jumpToClose = false;
              corrElse += 1;
            } else {
              corrElse += 1;
            }
          }
          if (lines(line + corrElse).isInstanceOf[ElseMarker]) {
            lines(line + corrElse) = ElseMarker(line + corrElse, true);
            gotoLine(line + corrElse);
          } else {
            gotoLine(line + corrElse);
          }
        }
      }

      case ElseMarker(_, b: Boolean) => {
        var jumpToClose: Boolean = false;
        if (b) {
          gotoLine(line + 1);
        } // Else go to next close statement, account for other If 
        // expressions
        else {
          var corrClose: Int = 1;
          while (!lines(line + corrClose).isInstanceOf[CloseIfExpression] || jumpToClose) {
            if (lines(line + corrClose).isInstanceOf[IfExpression]) {
              jumpToClose = true;
              corrClose += 1;
            } else if (lines(line + corrClose).isInstanceOf[CloseIfExpression]) {
              jumpToClose = false;
              corrClose += 1;
            } else {
              corrClose += 1;
            }
          }
          gotoLine(line + corrClose);
        }
      }

      case CloseIfExpression(_) => {
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

  // Else statement terminator "path"
  abstract sealed class PathWord;
  object path extends PathWord;

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

  // Start of if statement with keyword "Consider"
  object Jedi {
    def path(s: Function0[Boolean]) = {
      lines(lineNumber) = IfExpression(lineNumber, s);
      lineNumber += 1;
    }
    // Case where it is just a boolean variable
    def apply() = {

    }
  }

  def Close = {
    lines(lineNumber) = CloseIfExpression(lineNumber);
    lineNumber += 1;
    LineTermination;
  }

  def Sith(p: PathWord) {
    lines(lineNumber) = ElseMarker(lineNumber);
    lineNumber += 1;
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

  // Handles basic add/sub/mul/div operations
  // and changes specific variable
  case class BasicOperations(v: String) {
    def hate(s: Int) = {
      lines(lineNumber) = SubOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def hate(s: String) = {
      lines(lineNumber) = SubOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def hate(s: Double) = {
      lines(lineNumber) = SubOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def anger(s: Int) = {
      lines(lineNumber) = AddOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def anger(s: String) = {
      lines(lineNumber) = AddOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def anger(s: Double) = {
      lines(lineNumber) = AddOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def darkness(s: Int) = {
      lines(lineNumber) = DivOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def darkness(s: String) = {
      lines(lineNumber) = DivOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def darkness(s: Double) = {
      lines(lineNumber) = DivOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def suffering(s: Int) = {
      lines(lineNumber) = MultOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def suffering(s: String) = {
      lines(lineNumber) = MultOperation(lineNumber, v, s);
      lineNumber += 1;
    }
    def suffering(s: Double) = {
      lines(lineNumber) = MultOperation(lineNumber, v, s);
      lineNumber += 1;
    }
  }

  // Yoda quotes on variables for basic change operations
  object Fear {
    def leads(s: String) = BasicOperations(s);
  }

  object Anger {
    def leads(s: String) = BasicOperations(s);
  }

  object Hate {
    def leads(s: String) = BasicOperations(s);
  }

  object Suffering {
    def leads(s: String) = BasicOperations(s);
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

  // Expression operations 
  implicit def operations(a: Any) = new {
    def plus(b: Any): Function0[Any] = {
      () =>
        {
          val base_a = a match {
            // Make sure String is in variable format and that it 
            // holds an int or decimal value
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }
          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n + m;
                case m: Double => n + m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n + m;
                case m: Double => n + m;
              }
            }
          }
        }
    }

    def minus(b: Any): Function0[Any] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }
          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n - m;
                case m: Double => n - m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n - m;
                case m: Double => n - m;
              }
            }
          }
        }
    }

    def times(b: Any): Function0[Any] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }
          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n * m;
                case m: Double => n * m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n * m;
                case m: Double => n * m;
              }
            }
          }
        }
    }

    def over(b: Any): Function0[Any] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }
          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n / m;
                case m: Double => n / m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n / m;
                case m: Double => n / m;
              }
            }
          }
        }
    }

    def greater_than(b: Any): Function0[Boolean] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }

          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n > m;
                case m: Double => n > m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n > m;
                case m: Double => n > m;
              }
            }
          }
        }
    }

    def lesser_than(b: Any): Function0[Boolean] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }

          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n < m;
                case m: Double => n < m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n < m;
                case m: Double => n < m;
              }
            }
          }
        }
    }

    def equal_to(b: Any): Function0[Boolean] = {
      () =>
        {
          val base_a = a match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => a;
          }

          val base_b = b match {
            case n: String         => extractVal(n);

            case n: Function0[Any] => n();
            case _                 => b;
          }

          base_a match {
            case n: Int => {
              base_b match {
                case m: Int    => n == m;
                case m: Double => n == m;
              }
            }
            case n: Double => {
              base_b match {
                case m: Int    => n == m;
                case m: Double => n == m;
              }
            }
          }
        }
    }
  }
}