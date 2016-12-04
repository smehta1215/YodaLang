import yodalang.YodaLang

object Test05 extends YodaLang {
  def main(args: Array[String]) = {
    Begin we will

    Show me "Function testing" you will
    
    // Account for variable shadowing
    Force push "y" as 5 you will

    Start training "exercise_1" you will
      Show me "Inside function call" you will
      Force push "x" as 2 you will
      Show me "v|x here" you will
      Give back "v|x" you will
    Stop your training you will

    Show me "This is before the function call" you will
    Force pull ( "exercise_1", "t" )
    Show me "This is after the function call" you will
    
    Show me "v|t" you will
    
    Finish we will
  }
}