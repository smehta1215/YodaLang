package yodalang

import scala.collection.mutable.HashMap

class Variables {
  // indicates scoped variables, the global level is 0
  var depth: Int = -1;

  val variablesScope = HashMap[Int, HashMap[String, Any]]();
  //    val vars = HashMap[String, Any]();

  def getDepth(): Int = {
    return depth;
  }

  def setDepth(d: Int) = {
    depth = d;
  }

  // Creates a new scope of variables for functions
  // Make sure new scope level doesn't already exist
  def createScope() = {
    variablesScope(getDepth()) = new HashMap[String, Any];
  }

  // Removes selected scope to represent local variable removal
  def removeScope() = {
  }

  // Returns correct scope; starts from the topmost level and goes to 
  // Global level (level zero) and checks if variable exists, if it 
  // does, then that level becomes the scope, otherwise the topmost 
  // level is the scope returned
  def getScope(s: String): HashMap[String, Any] = {
    var retScope = HashMap[String, Any]();
    retScope = variablesScope(getDepth());

    var tempDepth: Int = getDepth();
    while (tempDepth > -1) {
      if (variablesScope(tempDepth).contains(s)) {
        retScope = variablesScope(tempDepth);
      }
      tempDepth -= 1;
    }

    return retScope;
  }

  // Add a new variable in the format of a String name corresponding
  // to a value 
  def set(s: String, a: Any): Unit = {
    if (a.isInstanceOf[Function0[Any]]) {
      getScope(s)(s) = a.asInstanceOf[Function0[Any]]()
    } else {
      getScope(s)(s) = a;
    }
  }
}