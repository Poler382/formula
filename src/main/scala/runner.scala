import scala.sys._
import java.util.Date
import java.io.File
import scala.io.Source
object runner{

  val condition1 = List(
    "testrun  10000 100 lab",
    "Batch 10000 100 lab",
    "LRver 10000 100 lab",
    "Dropout 10000 100 lab"
  )
  val condition2 = List(
    "testrun CBOW 5000 2 2 ",
    "testrun CBOW 5000 4 2 ",
    "testrun CBOW 5000 8 2 ",
    "testrun CBOW 5000 16 2 ",
    "testrun CBOW 5000 32 2  ",
    "testrun SkipGram 5000 2 2 ",
    "testrun SkipGram 5000 4 2 ",
    "testrun SkipGram 5000 8 2 ",
    "testrun SkipGram 5000 16 2 ",
    "testrun SkipGram 5000 16 2 ",
    "testrun CBOW 5000 2 4 ",
    "testrun CBOW 5000 4 4 ",
    "testrun CBOW 5000 8 4 ",
    "testrun CBOW 5000 16 4 ",
    "testrun CBOW 5000 32 4  ",
    "testrun SkipGram 5000 2 4 ",
    "testrun SkipGram 5000 4 4 ",
    "testrun SkipGram 5000 8 4 ",
    "testrun SkipGram 5000 16 4 ",
    "testrun SkipGram 5000 16 4 ",
    "testrun CBOW 5000 2 8 ",
    "testrun CBOW 5000 4 8 ",
    "testrun CBOW 5000 8 8 ",
    "testrun CBOW 5000 16 8 ",
    "testrun CBOW 5000 32 8  ",
    "testrun SkipGram 5000 2 8 ",
    "testrun SkipGram 5000 8 8 ",
    "testrun SkipGram 5000 8 8 ",
    "testrun SkipGram 5000 16 8 ",
    "testrun SkipGram 5000 16 8 "
  )


  def main(args:Array[String])={
    val jarfile = args(0)
    val result_path = "result/"

    //    val objectname  = args(0)
    val lnum = 5000
    //args(1).toInt
    //val n = args(2).toInt
    val condition = List(condition2)

    var resultlist = List[String]()
    var flist = List[String]()
    for(c <- condition2){
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
      //println("java -cp "+assembla+" "+objectname+" "+c+" & 2> "+repath+c2+".txt\n")
    }


    Thread.sleep(3000)
    var counter = new Array[Int](resultlist.size)
    var sum = 0
    while(sum < 100 * flist.size){
      sum = 0
      for(i <- 0 until  resultlist.size){
        var text = process.Process("cat "+resultlist(i)).lineStream.toArray
        counter(i) += text.size
      }


      Thread.sleep(200)
      println("now...")
      for(i <- 0 until counter.size){
        val persent = counter(i).toDouble/lnum * 100 //%
        val paint = (persent.toDouble / 4).toInt

        print(String.format("%45s",flist(i)))
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
}
