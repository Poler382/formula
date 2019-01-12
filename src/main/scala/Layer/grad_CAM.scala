package Layer
import java.util.Date

class GradCAM (val IC:Int,val IW:Int,val IH:Int) extends Layer{
  import Utilty._
  import math._

  val Ist = new Stack[Array[T]]()
  var c = 0
  def forward(x: Array[T]): Array[T] = {
    Ist.push(x)
    x
  }

  def backward(d: Array[T]): Array[T]={
    c+=1
    if(is_CAM){
      println("grad start")
      val x = Ist.pop().grouped(IW*IH).toArray//chanelごとに配列化
      val alpha = d.grouped(IW*IH).toArray.map( _.sum / x.size )

      var img = Array.ofDim[T](IW*IH)

      for(ic <- 0 until IC;i <- 0 until IW;j <- 0 until IH){
        img(i*IW+j) += max(0f,alpha(ic)*x(ic)(i*IW+j))
      }

      val date = "grad-%tm%<td-%<tHh" format new Date
      val fn = "/home/pll20/sbt/t1/"+date+c.toString
      val pw = new java.io.PrintWriter(fn)
      pw.write(img.toList.mkString(","))
      pw.write("\n")
      pw.close
      is_CAM = false
    }
    d//backwardの邪魔はしない
  }

  //batch は対応しておりません
  override def forward(xs: Array[Array[T]]): Array[Array[T]] = xs
  override def backward(ds: Array[Array[T]]): Array[Array[T]] = ds

  def update(){}
  def reset(){}
  override def save(fn: String){}
  override def load(fn: String){}

}
