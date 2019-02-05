object EmbeddingAnalyze {
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._

  def load_Enbedding(pathName:String)={
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    f(0).split(",").map(_.toFloat).toArray
  }


  def main(args: Array[String]): Unit = {
    val path = args(0)
    val in   = args(1).toInt
    val out  = args(2).toInt
    val label = Array("f","{","}","(",")","x","=","+","-",".","0","1","2","3","4","5","6","7","8","9","<EOS>","<UNK>")

    val EmbeddingVecter = load_Enbedding(path).grouped(out).toArray

    //類似度トップ５
    for(i <- 0  until label.size){
      var inner = Map[String,Float]()
      println(s"input -> ${label(i)}")
      for(j <- 0 until label.size){
        val a = math.sqrt((EmbeddingVecter(j) dot EmbeddingVecter(j)))
        val b = math.sqrt((EmbeddingVecter(i) dot EmbeddingVecter(i)))


        val cos = (EmbeddingVecter(j) dot EmbeddingVecter(i)) / (a*b)
        inner += (label(j) -> cos.toFloat)

      }
      val line = inner.toSeq.sortBy(_._2).reverse
      for(k <- 0 until 5){
        println(s"${line(k)._1} > : ${line(k)._2} ")
      }
      println("worst")

      val line2 = inner.toSeq.sortBy(_._2)
      for(k <- 0 until 5){
        println(s"${line2(k)._1} > : ${line2(k)._2} ")
      }
      println()
    }



    val zero  = EmbeddingVecter(label.indexOf("0"))
    val one   = EmbeddingVecter(label.indexOf("1"))
    val two   = EmbeddingVecter(label.indexOf("2"))
    val three = EmbeddingVecter(label.indexOf("3"))
    val fore  = EmbeddingVecter(label.indexOf("4"))
    val five  = EmbeddingVecter(label.indexOf("5"))
    val six   = EmbeddingVecter(label.indexOf("6"))
    val seven = EmbeddingVecter(label.indexOf("7"))
    val eight = EmbeddingVecter(label.indexOf("8"))
    val nine  = EmbeddingVecter(label.indexOf("9"))

    print("( ) の距離 ")
    val a   = EmbeddingVecter(label.indexOf("("))
    val b   = EmbeddingVecter(label.indexOf(")"))
    val ans1 = math.sqrt((a dot a) * (b dot b) - 2 * (a dot b))
    println(ans1)


    print("{ } の距離  " )
    val a2   = EmbeddingVecter(label.indexOf("("))
    val b2   = EmbeddingVecter(label.indexOf(")"))
    val ans2 = math.sqrt((a2 dot a2) * (b2 dot b2) - 2 * (a2 dot b2))

    println(ans2)


    print("8-6 = ")
    val ans3  = math.sqrt((eight dot eight) * (six dot six) - 2 * (eight dot six))
    println(ans3)

    print("5-3 = ")
    val ans4  = math.sqrt((five dot five) * (three dot three) - 2 * (five dot three))
    println(ans4)

    print("4-2 = ")
    val ans5  = math.sqrt((fore dot fore) * (two dot two) - 2 * (fore dot two))
    println(ans5)

    print("7-5 = ")
    val ans6  = math.sqrt((seven dot seven) * (five dot five) - 2 * (seven dot five))
    println(ans6)

    print("6-5 = ")
    val ans7  = math.sqrt((six dot six) * (five dot five) - 2 * (six dot five))
    println(ans7)

    println(ans3,ans4,ans5,ans6)
    println(ans7)


    print("8+6  =  ")
    val plas3  = math.sqrt((eight dot eight) * (six dot six) + 2 * (eight dot six))
    println(plas3)

    print("5+3  =  ")
    val plas4  = math.sqrt((five dot five) * (three dot three) + 2 * (five dot three))
    println(plas4)

    print("4+2  =  ")
    val plas5  = math.sqrt((fore dot fore) * (two dot two) + 2 * (fore dot two))
    println(plas5)

    print("7+5  =  ")
    val plas6  = math.sqrt((seven dot seven) * (five dot five) + 2 * (seven dot five))
    println(plas6)

    print("6+5  =  ")
    val plas7  = math.sqrt((six dot six) * (five dot five) + 2 * (six dot five))
    println(plas7)

    println(plas3,plas4,plas5,plas6)
    println(plas7)
  }




}
