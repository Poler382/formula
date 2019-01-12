package Layer
import math._
import Utilty.RichArray._

class StridedPadding(val IC: Int, val IH: Int, val IW: Int, val S: Int) extends Layer {
  val OH = S * IH + S - 1
  val OW = S * IW + S - 1
  val OC = IC

  def iindex(i: Int, j: Int, k: Int) = i * IH * IW + j * IW + k

  def oindex(i: Int, j: Int, k: Int) = i * OH * OW + j * OW + k

  def forward(X: Array[T]) = {
    val Z = Array.ofDim[T](OC * OH * OW)
    for (i <- 0 until IC; j <- 0 until IH; k <- 0 until IW) {
      Z(oindex(i, S - 1 + j * S, S - 1 + k * S)) = X(iindex(i, j, k))
    }
    Z
  }

  def backward(d: Array[T]) = {
    val D = Array.ofDim[T](IC * IH * IW)
    for (i <- 0 until IC; j <- 0 until IH; k <- 0 until IW) {
      D(iindex(i, j, k)) = d(oindex(i, S - 1 + j * S, S - 1 + k * S))
    }
    D
  }

  def update() {}

  def reset() {}
}


class ZeroPadding(val IC: Int, val IH: Int, val IW: Int, val P: Int) extends Layer {
  val OH = IH + 2 * P
  val OW = IW + 2 * P
  val OC = IC

  def iindex(i: Int, j: Int, k: Int) = i * IH * IW + j * IW + k

  def oindex(i: Int, j: Int, k: Int) = i * OH * OW + j * OW + k

  def forward(x: Array[T]) = {
    val y = new Array[T](OC * OH * OW)
    for (c <- 0 until IC; i <- 0 until IH; j <- 0 until IW) {
      y(oindex(c, i + P, j + P)) = x(iindex(c, i, j))
    }
    y
  }

  def backward(d: Array[T]) = {
    val d1 = new Array[T](IC * IH * IW)
    for (c <- 0 until IC; i <- 0 until IH; j <- 0 until IW) {
      d1(iindex(c, i, j)) = d(oindex(c, i + P, j + P))
    }
    d1
  }

  def update() {}

  def reset() {}
}
