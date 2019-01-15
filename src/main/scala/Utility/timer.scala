package Utilty
object timer{
  val timestack = new  Stack[Double]()
  var s = 0d
  def start()={
    s = System.currentTimeMillis()
  }
  def finish()={
    val tm = (System.currentTimeMillis() - s)/1000d
    s = 0d
    timestack.push(tm)
    println("time: "+tm  +" s")
    tm
  }
}
