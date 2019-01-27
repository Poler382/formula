package Layer
import math._
import Utilty.RichArray._

class Upsampling(val IC: Int, val IH: Int, val IW: Int, val BH: Int, val BW: Int) extends Layer {
  val OH = IH * BH
  val OW = IW * BW
  val OC = IC

  def iindex(i: Int, j: Int, k: Int) = i * IH * IW + j * IW + k

  def oindex(i: Int, j: Int, k: Int) = i * OH * OW + j * OW + k

  def forward(X: Array[T]) = {
    val Z = Array.ofDim[T](OC * OH * OW)
    for (i <- 0 until OC; j <- 0 until OH; k <- 0 until OW) {
      Z(oindex(i, j, k)) = X(iindex(i, j / BH, k / BW))
    }
    Z
  }

  def backward(d: Array[T]) = {
    val D = Array.ofDim[T](IC * IH * IW)
    for (i <- 0 until OC; j <- 0 until OH; k <- 0 until OW) {
      D(iindex(i, j / BH, k / BW)) += d(oindex(i, j, k))
    }
    D
  }

  def update() {}

  def reset() {}
}


class Subsampling(val IC: Int, val IH: Int, val IW: Int, val BH: Int, val BW: Int) extends Layer {
  val OH = IH / BH
  val OW = IW / BW
  val OC = IC

  def iindex(i: Int, j: Int, k: Int) = i * IH * IW + j * IW + k

  def oindex(i: Int, j: Int, k: Int) = i * OH * OW + j * OW + k

  def forward(X: Array[T]) = {
    val Z = Array.ofDim[T](OC * OH * OW)
    for (i <- 0 until OC; j <- 0 until OH; k <- 0 until OW) {
      Z(oindex(i, j, k)) = X(iindex(i, j * BH, k * BW))
    }
    Z
  }

  def backward(d: Array[T]) = {
    val D = Array.ofDim[T](IC * IH * IW)
    for (i <- 0 until OC; j <- 0 until OH; k <- 0 until OW) {
      D(iindex(i, j * BH, k * BW)) += d(oindex(i, j, k))
    }
    D
  }

  def update() {}

  def reset() {}
}
