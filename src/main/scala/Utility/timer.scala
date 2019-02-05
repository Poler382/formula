package Utilty
class timer{
  import java.util.Date
  val timestack = new Stack[Double]()
  var s = 0d
  def timestart()={
    s = System.currentTimeMillis()
  }
  def timefinish()={
    val tm = (System.currentTimeMillis() - s)/1000d
    s = 0d
    timestack.push(tm)
    tm
  }
  def nowtime()={
    "-%tm%<td-%<tHh"format new Date
  }
  def date()={
    val n = new Date()
    n.toString.replace(" ","_")
  }
}

object time_say {
  val timer = new timer()
  def main(args: Array[String]): Unit = {
    var h = 20
    while(true){
      sys.process.Process("say 残り"+h+" 時間").run
      h -= 1
      Thread.sleep(1000 * 60 * 60)

    }


  }

}
