package Layer
class Bias2D(
  val in_height:Int,
  val in_width:Int,
  val in_channels:Int
)extends Layer{
  var bias = Array.ofDim[Float](in_channels)
  var grad = Array.ofDim[Float](in_channels)
  var step = 0f


  def forward(x:Array[Float])={
    var xs = x.grouped(in_height*in_width).toArray
    for(j <- 0 until bias.size;i <- 0 until xs(0).size){

      xs(j)(i) += bias(j)
    }
    xs.flatten

  }
  def backward(d:Array[Float])={
    d
  }
  override def forward(d:Array[Array[Float]])={
    d
  }
  override def backward(d:Array[Array[Float]])={
    d
  }
  def update(){}
  def reset(){}
  override def save(fn:String)={
    val pw = new java.io.PrintWriter(fn)
    for(i <- 0 until bias.size ) {
      pw.write(bias(i).toString)
      if(i != bias.size-1) {
        pw.write(",")
      }
    }
    pw.write("\n")
    pw.close()

  }
  override def load(fn:String){
    val f =scala.io.Source.fromFile(fn).getLines.map(_.split(",").map(_.toFloat).toArray).toArray
    for(i <- 0 until bias.size){
      bias(i)=f(0)(i)
    }
  }

}
