package Activation
import math._
import Layer._
class Sigmoid() extends Layer {
  var ys = List[Array[Float]]()

  def push(y: Array[Float]) = {
    ys ::= y;
    y
  }

  def pop() = {
    val y = ys.head;
    ys = ys.tail;
    y
  }

  def sigmoid(x:Float) = 1 / (1 + math.exp(-x)).toFloat

  def forward(x: Array[Float]) = {
    push(x.map(sigmoid))
  }

  def backward(d: Array[Float]) = {
    val y = pop()
    (0 until d.size).map(i => d(i) * y(i) * (1f - y(i))).toArray
  }

  def update(){ reset()}

  def reset(){ ys = List[Array[Float]]()}

  override def save(fn: String) {}

  override def load(fn: String) {}
}

class Tanh() extends Layer {
  var ys = List[Array[Float]]()

  def push(y: Array[Float]) = {
    ys ::= y;
    y
  }

  def pop() = {
    val y = ys.head;
    ys = ys.tail;
    y
  }

  def forward(x: Array[Float]) = {
    push(x.map(math.tanh(_).toFloat))
  }

  def backward(d: Array[Float]) = {
    val y = pop()
    (0 until d.size).map(i => d(i) * (1f - y(i) * y(i))).toArray
  }

  def update() {
    reset()
  }

  def reset() {
    ys = List[Array[Float]]()
  }
}


class ReLU() extends Layer {
  var ys = List[Array[Float]]()

  def push(y: Array[Float]) = {
    ys ::= y;
    y
  }

  def pop() = {
    val y = ys.head;
    ys = ys.tail;
    y
  }

  def forward(x: Array[Float]) = {
    push(x.map(a => math.max(a, 0)))
  }

  def backward(d: Array[Float]) = {
    val y = pop()
    (0 until d.size).map(i => if (y(i) > 0) d(i) else 0f).toArray
  }

  def update() {
    reset()
  }

  def reset() {
    ys = List[Array[Float]]()
  }

}

class LeakyReLU(val alpha:Float) extends Layer {
  var ys = List[Array[Float]]()

  def push(y: Array[Float]) = {
    ys ::= y;
    y
  }

  def pop() = {
    val y = ys.head;
    ys = ys.tail;
    y
  }

  def forward(x: Array[Float]) = {
    push(x.map(a => if (a > 0) a else alpha * a))
  }

  def backward(d: Array[Float]) = {
    val y = pop()
    (0 until d.size).map(i => if (y(i) > 0) d(i) else alpha * d(i)).toArray
  }

  def update() {
    reset()
  }

  def reset() {
    ys = List[Array[Float]]()
  }
}
