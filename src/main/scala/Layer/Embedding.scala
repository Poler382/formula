package word2vec
import Layer._
class Embedding(val xn: Int, val yn: Int,
  val eps:Float= 0.001f, val rho1:Float = 0.9f, val rho2:Float= 0.999f) extends Layer {
    import math._
    import Utilty.RichArray._
    import Utilty.ML._
    import Utilty.Stack

    val rand = new scala.util.Random(0)
    var W  = Array.ofDim[Float](xn*yn).map(_ => rand.nextGaussian.toFloat / math.sqrt(xn).toFloat).toArray
    var dW = Array.ofDim[Float](xn*yn)
    var count = 0d
    val stack = new Stack[Int]()//forwardのときどこを抜き出したかを保存しておく

    def forward(x:Array[Float]) = {
      val index = x(0).toInt
      stack.push(index)
      var tempW = W.grouped(yn).toArray
      tempW(index)
    }
    def backward(d:Array[Float]) = {
      var index = stack.pop
      for(i <- 0 until d.size){
        dW(index*yn +i) += d(i)
      }
      dW
    }

    override def forward(xs:Array[Array[Float]]):Array[Array[Float]] = xs.map(forward(_))
    override def backward(ds:Array[Array[Float]]):Array[Array[Float]] = ds.map(backward(_))

    var adam_W = new Adam(W.size, eps, rho1, rho2)
    def update_adam() {
      adam_W.update(W, dW)
    }

    var lr = 0.001f
    def update_sgd() {
      W = W.zip(dW).map{case (a,b) => a - ( b * lr ) }
    }
    def update() {
      dW = dW / count.toFloat
      update_adam()
      reset()
    }

    def reset() {
      dW = dW.map(a => 0f)
      count  = 0
    }

    override def save(fn: String) {
      val pw = new java.io.PrintWriter(fn)
      for (i <- 0 until W.size) {
        pw.write(W(i).toString)
        if (i != W.size - 1) {
          pw.write(",")
        }
      }
      pw.write("\n")
      pw.close()
    }

    override def load(fn: String) {
      val f = scala.io.Source.fromFile(fn).getLines.toArray
      W = f(0).split(",").map(_.toFloat).toArray
    }





  }
