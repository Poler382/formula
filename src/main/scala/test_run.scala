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
        CBOW(args(1).toInt,args(2).toInt,args(3).toInt)
      }
      case "SkipGram" =>{
        SkipGram(args(1).toInt,args(2).toInt,args(3).toInt)
      }
      case "full" =>{

        CBOW(3000,2,2)
        CBOW(3000,4,2)
        CBOW(3000,8,2)
        CBOW(3000,16,2)

        SkipGram(3000,2,2)
        SkipGram(3000,4,2)
        SkipGram(3000,8,2)
        SkipGram(3000,16,2)
      }
      case "formula" =>{


      }
    }
  }

  def CBOW(epoch:Int,out:Int,Windowsize:Int)={
    val Cbow = new CBOW("cbow_vec",22,out,Windowsize:Int)
    var finalfn = Cbow.train_one("collection2.txt",epoch)

  }

  def SkipGram(epoch:Int,out:Int,Windowsize:Int)={
    val Skip = new SkipGram("skip_vec",22,out,Windowsize:Int)
    var finalfn = Skip.train_one("collection2.txt",epoch)
  }

  def FormulaVecter()={


  }


}
