import scala.sys._
import java.util.Date
import java.io.File
import scala.io.Source
object runner{
  val condition1 = List(
    "testrun CBOW 50 2 2 ",
    "testrun CBOW 50 4 2 ",
    "testrun CBOW 50 8 2 ",
    "testrun CBOW 50 16 2 ",
    "testrun CBOW 50 32 2 ",
    "testrun CBOW 50 2 4 ",
    "testrun CBOW 50 4 4 ",
    "testrun CBOW 50 8 4 ",
    "testrun CBOW 50 16 4 ",
    "testrun CBOW 50 32 4 ",
    "testrun CBOW 50 2 8 ",
    "testrun CBOW 50 4 8 ",
    "testrun CBOW 50 8 8 ",
    "testrun CBOW 50 16 8 ",
    "testrun CBOW 50 32 8 ",
  )
  val condition1_1 = condition1.take(7)
  val condition1_2 = condition1.drop(7)

  val condition2 = List(
    "testrun CBOW 5000 2 2 ",
    "testrun CBOW 5000 4 2 ",
    "testrun CBOW 5000 8 2 ",
    "testrun CBOW 5000 16 2 ",
    "testrun CBOW 5000 32 2 ",
    "testrun CBOW 5000 2 4 ",
    "testrun CBOW 5000 4 4 ",
    "testrun CBOW 5000 8 4 ",
    "testrun CBOW 5000 16 4 ",
    "testrun CBOW 5000 32 4 ",
    "testrun CBOW 5000 2 8 ",
    "testrun CBOW 5000 4 8 ",
    "testrun CBOW 5000 8 8 ",
    "testrun CBOW 5000 16 8 ",
    "testrun CBOW 5000 32 8 ",
  )
  val condition2_1 = condition2.take(7)
  val condition2_2 = condition2.drop(7)


  val condition3 = List(
    "testrun SkipGram 5000 2 2",
    "testrun SkipGram 5000 4 2",
    "testrun SkipGram 5000 8 2",
    "testrun SkipGram 5000 16 2",
    "testrun SkipGram 5000 32 2",
    "testrun SkipGram 5000 2 4",
    "testrun SkipGram 5000 4 4",
    "testrun SkipGram 5000 8 4",
    "testrun SkipGram 5000 16 4",
    "testrun SkipGram 5000 32 4",
    "testrun SkipGram 5000 2 8",
    "testrun SkipGram 5000 4 8",
    "testrun SkipGram 5000 8 8",
    "testrun SkipGram 5000 16 8",
    "testrun SkipGram 5000 32 8"
  )

  val condition3_1 = condition3.take(7)
  val condition3_2 = condition3.drop(7)

  val condition4 = List(
    "testrun CBOW 50000 2 2",
    "testrun CBOW 50000 4 2",
    "testrun CBOW 50000 8 2",
    "testrun CBOW 50000 16 2",
    "testrun CBOW 50000 32 2",
    "testrun CBOW 50000 2 4",
    "testrun CBOW 50000 4 4",
    "testrun CBOW 50000 8 4",
    "testrun CBOW 50000 16 4",
    "testrun CBOW 50000 32 4",
    "testrun CBOW 50000 2 8",
    "testrun CBOW 50000 4 8",
    "testrun CBOW 50000 8 8",
    "testrun CBOW 50000 16 8",
    "testrun CBOW 50000 32 8",
  )
  val condition4_1 = condition4.take(7)
  val condition4_2 = condition4.drop(7)


  val condition5 = List(
    "testrun SkipGram 50000 2 2",
    "testrun SkipGram 50000 4 2",
    "testrun SkipGram 5000 8 2",
    "testrun SkipGram 50000 16 2",
    "testrun SkipGram 50000 32 2",
    "testrun SkipGram 50000 2 4",
    "testrun SkipGram 50000 4 4",
    "testrun SkipGram 50000 8 4",
    "testrun SkipGram 50000 16 4",
    "testrun SkipGram 50000 32 4",
    "testrun SkipGram 50000 2 8",
    "testrun SkipGram 50000 4 8",
    "testrun SkipGram 50000 8 8",
    "testrun SkipGram 50000 16 8",
    "testrun SkipGram 50000 32 8"
  )

  val condition5_1 = condition5.take(7)
  val condition5_2 = condition5.drop(7)

  val condition6 = List(
    "testrun CBOW 5000 30 2",
    "testrun CBOW 5000 200 2",
    "testrun CBOW 5000 400 2",
    "testrun CBOW 5000 800 2",
    "testrun CBOW 5000 30 4",
    "testrun CBOW 5000 200 4",
    "testrun CBOW 5000 400 4",
    "testrun CBOW 5000 800 4",
    "testrun CBOW 5000 30 8",
    "testrun CBOW 5000 200 8",
    "testrun CBOW 5000 400 8",
    "testrun CBOW 5000 800 8",
  )
  val condition6_1 = condition6.take(2)
  val condition6_2 = condition6.drop(2).take(2)
  val condition6_3 = condition6.drop(4).take(2)
  val condition6_4 = condition6.drop(6).take(2)
  val condition6_5 = condition6.drop(8)

  val condition_formula = List(
    "formula.FormulaVecter 50 22 200 1 normal",
    "formula.FormulaVecter 50 22 200 4 normal",
    "formula.FormulaVecter 30 22 200 8 normal",
    "formula.FormulaVecter 30 22 500 1 normal" ,
    "formula.FormulaVecter 30 22 500 4 normal",
    "formula.FormulaVecter 30 22 500 8 normal" ,
    "formula.FormulaVecter 30 22 1000 1 normal" ,
    "formula.FormulaVecter 30 22 1000 4 normal" ,
    "formula.FormulaVecter 30 22 1000 8 normal" ,
    "formula.FormulaVecter 30 22 200 1 Skip" ,
    "formula.FormulaVecter 30 22 200 4 Skip" ,
    "formula.FormulaVecter 30 22 200 8 Skip" ,
    "formula.FormulaVecter 30 22 500 1 Skip" ,
    "formula.FormulaVecter 30 22 500 4 Skip" ,
    "formula.FormulaVecter 30 22 500 8 Skip" ,
    "formula.FormulaVecter 30 22 1000 1 Skip" ,
    "formula.FormulaVecter 30 22 1000 4 Skip" ,
    "formula.FormulaVecter 30 22 1000 8 Skip" ,
    "formula.FormulaVecter 30 22 200 1 Bi" ,
    "formula.FormulaVecter 30 22 500 1 Bi" ,
    "formula.FormulaVecter 30 22 1000 1 Bi"
  )




  def main(args:Array[String]){
    val jarfile = args(0)
    val result_path = "result/"

    // /    val objectname  = args(0)

    //args(1).toInt
    //val n = args(2).toInt
    val condition = List(condition_formula.take(8),condition_formula.drop(8))
    val l30 = condition(0)(0).split(" ")(2).toInt /10
    println(100)

    for(l <- condition){
      var resultlist = List[String]()
      var flist = List[String]()
      for(c <- l){
        val c2 = c.replaceAll(" ","_")
        val data = "-%tm%<td-%<tHh" format new Date
        val repath = result_path+c2+"-"+data+"/"
        println(repath)
        process.Process("mkdir "+repath).run
        Thread.sleep(100)
        //実行だけ
        //  process.Process("java -cp "+assembla+" "+objectname+" "+c).run

        //リダイレクト
        (
          process.Process("java -cp "+jarfile+" "+c) #> new File(repath+c2+".txt")
        ).run

        resultlist :+= repath+c2+".txt"
        flist :+= c2+".txt"
        Thread.sleep(100)

      }


      Thread.sleep(3000)
      var counter = new Array[Int](resultlist.size)
      var sum = 0
      while(sum < 30 * flist.size){
        sum = 0

        Thread.sleep(1000)
        for(i <- 0 until  resultlist.size){
          var text = process.Process("cat "+resultlist(i)).lineStream.toArray
          counter(i) += text.filter(_.contains("ep")).size
          //println(counter(i))
        }


        Thread.sleep(1000)
        println("now...")
        for(i <- 0 until counter.size){
          val persent = counter(i).toDouble/l30 * 30 //%
          val paint = (persent.toDouble / 4).toInt

          print(String.format("%35s",flist(i)))
          print(" : ")
          for(j <- 0 to 25){
            if(j < paint) print("#")
            else print(" ")
          }
          println("| "+persent.toString+"%")
          sum += persent.toInt
        }
        println()

        counter = new Array[Int](resultlist.size)
      }
    }

    Thread.sleep(200)
    // process.Process("pkill -KILL java").run
  }
}
