package Layer
import math._
import kata._
import Utilty._

class Dropout(var dr:T) extends Layer {
  var masks = List[Array[T]]()

  def push(mask: Array[T]) = {
    masks ::= mask;
    mask
  }

  def pop() = {
    val mask = masks.head;
    masks = masks.tail;
    mask
  }

  val rand = new util.Random(0)

  def forward(x: Array[T]) = {
    if (is_test) {
      x.map(_ * (1 - dr))
    } else {
      val mask = push(Array.ofDim[T](x.size))
      for (i <- 0 until x.size) {
        if (rand.nextDouble> dr) {
          mask(i) = 1f
        }
      }
      x.zip(mask).map { case (a, b) => a * b }.toArray
    }
  }

  def backward(d: Array[T]) = {
    val mask = pop()
      (0 until d.size).map(i => if (mask(i) > 0) d(i) else 0f).toArray
  }

  def update() {
    reset()
  }

  def reset() {
    masks = List[Array[T]]()
  }
}
