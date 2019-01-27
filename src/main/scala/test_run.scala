object testrun{
  import formula.Utilty_formula._
  import Layer._
  import formula.protcol._
  import Utilty.ML._
  import Activation._
  import Utilty.RichArray._
  import Utilty.Stack
  import word2vec._

  def  main(args: Array[String]): Unit = {
    args(0) match {
      case "CBOW" =>{
        CBOW(args)
      }
      case "SkipGram" =>{
        SkipGram(args)
      }
    }
  }

  def CBOW(a: Array[String])={
    val Cbow = new CBOW("cbow_vec",89,2)
    if(a.size > 2){
      Cbow.load_Enbedding(a(2))
    }

    var finalfn = Cbow.train("collection3.txt",a(1).toInt)

  }

  def SkipGram(a: Array[String])={
    val Skip = new SkipGram("skip_vec",89,2)
    if(a.size > 2){
      Skip.load_Enbedding(a(2))
    }
    var finalfn = Skip.train("collection3.txt",a(1).toInt)

  }


}
