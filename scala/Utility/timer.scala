package Utilty
class timer{
  import java.util.Date
  val timestack = new  Stack[Double]()
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
