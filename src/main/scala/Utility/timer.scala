package Utilty
object timer{
  val timestack = new  Stack[Double]()
  var s = 0d
  def start()={
    s = System.currentTimeMillis()
  }
  def finish()={
    val tm = (System.currentTimeMillis() - s)
    s = 0d
    timestack.push(tm)
    println("time: "+tm / 1000d+" s")
  }
}
