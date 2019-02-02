package formula

object FormulaVecter{
  import formula.protcol._
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Activation._
  import Utilty.Stack
  val timer= new Utilty.timer()
  import formula.Utilty_formula._
  import word2vec._

  def learn(num:Int,Embedding_path:String)={
    val in = 10
    val hidden = 100
    val out = 64
    val path = "collection3.txt"
    val listCBOW_formula_vecter = new Stack[String]()

    val CBOWModel = new CBOW("formula",89,in)
    val Encoder = new Seq2Seq.Encoder(in,hidden,hidden,1)
    val Decoder = new Seq2Seq.Decoder(in,hidden,89,1)

    val words = CBOWModel.load_word_count(path).toList
    val Exercises = question_load_all2(path,words)

    CBOWModel.load_Enbedding(Embedding_path)

    for(epoch <- 0 until num){
      println(s"epoch ${epoch}")
      timer.timestart
      for(e <- 0 until Exercises.size){
        Exercises(e).foreach{a => print(a +" ")}
        println()






      }


    }


  }

  def main(args: Array[String]): Unit = {

  }



}
