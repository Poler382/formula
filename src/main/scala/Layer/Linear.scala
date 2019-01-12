package Layer
import math._
import kata._
import Utilty.RichArray._
class Linear(val xn: Int, val yn: Int,
  val eps:T= 0.001f, val rho1:T = 0.9f, val rho2:T= 0.999f) extends Layer {
    val rand = new scala.util.Random(0)
    var W = Array.ofDim[Float](xn*yn)
    W = (0 until W.size).map(_ => rand.nextGaussian.toFloat / math.sqrt(xn).toFloat ).toArray
    var dW = Array.ofDim[Float](xn * yn)
    var xs = List[Array[Float]]()
    var count=0f
    def push(x:Array[Float]) = { xs ::= x; x }
    def pop() = { val x = xs.head; xs = xs.tail; x }
    def transpose(x:Array[Float],row:Int,col:Int)={
      //////row:colはtransposeした後の行:列///////////
      var xt = Array.ofDim[Float](row*col)
      for(i <- 0 until col;j <- 0 until row){
        xt(i*row+j) = x(i+col*j)
      }
      xt
    }

    def forward(x:Array[Float]) = forward(Array(x)).flatten
    def backward(d:Array[Float]) = backward(Array(d)).flatten

    override def forward(xs:Array[Array[Float]]):Array[Array[Float]] = {
      //  W.take(10).foreach(a => print(a +" "))
      //  println()
      xs.map(push)
      var C = Array.ofDim[Float](yn*xs.size)
      var y = Array.ofDim[Float](xs.size*yn)
      var index = 0
      val mxs = transpose(xs.flatten,xs.size,xn)
      blas.BLAS.matmul(W,mxs,C,yn,xs.size,xn)
      for(i <- 0 until yn;j <- 0 until xs.size){
        y(i*xs.size+j) = C(i*xs.size+j)
      }
      val ys = transpose(y,yn,xs.size)
      ys.grouped(yn).toArray
    }

    override def backward(ds:Array[Array[Float]]):Array[Array[Float]] ={
      count += ds.size
      var xs = Array.ofDim[Float](ds.size,xn)
      for(i <- ds.size-1 to 0 by -1){
        xs(i) = pop()
      }
      var C2 = Array.ofDim[Float](xn*ds.size)
      val mds = transpose(ds.flatten,ds.size,yn)
      blas.BLAS.matmul(mds,xs.flatten,dW,yn,xn,ds.size)

      val Wt = transpose(W,yn,xn)
      blas.BLAS.matmul(Wt,mds,C2,xn,ds.size,yn)
      val dxs = transpose(C2,xn,ds.size)
      dxs.grouped(xn).toArray
    }

    var adam_W = new Adam(W.size, eps, rho1, rho2)

    def update_adam() {
      adam_W.update(W, dW)
    }

    var lr = 0.001f
    def update_sgd() {
      W = W.zip(dW).map{case (a,b) => a - ( b * lr ) }
    }
    def update() {
      dW = dW.map(_ / count)
      update_adam()
      reset()
    }

    def reset() {
      dW = dW.map(a => 0f)
      xs = List[Array[T]]()
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
