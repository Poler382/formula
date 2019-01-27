package Layer
class SoftMax() extends Layer {
  import math._
  import Utilty.RichArray._
  import Utilty.Stack
  val Y = new Stack[Array[T]]()

  def forward(xs: Array[T]) = {

    val ex = xs.map(math.exp(_).toFloat)
    val sum = ex.sum
    val y = ex.map(_ / sum)
    Y.push(y)
    y
  }

  def backward(ds: Array[T]) = {
    val y = Y.pop()
    var x = Array.ofDim[T](ds.size)
    val inner_product = ds dot y //内積

    for (i <- 0 until ds.size) {
      x(i) = y(i)*(ds(i)-inner_product)
    }
    x

    y * (ds - (y dot ds))
  }

  def update() {
    reset()
  }

  def reset() {
    Y.reset()
  }

  override def save(fn: String) {}
  override def load(fn: String) {}
}
