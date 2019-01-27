package Layer
class Softplus() extends Layer {
  var ys = List[Array[T]]()

  def push(y: Array[T]) = {
    ys ::= y;
    y
  }

  def pop() = {
    val y = ys.head;
    ys = ys.tail;
    y
  }

  def softplus(x:T) = x + math.log(1f + math.exp(-x)).toFloat

  def sigmoid(x:T) = 1 / (1f + math.exp(-x)).toFloat

  def forward(x: Array[T]) = {
    push(x)
    x.map(softplus)
  }

  def backward(d: Array[T]) = {
    val x = pop()
    (0 until d.size).map(i => d(i) * sigmoid(x(i))).toArray
  }

  def update() {
    reset()
  }

  def reset() {
    ys = List[Array[T]]()
  }

}
