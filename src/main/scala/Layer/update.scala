package Layer
import math._
import kata._
import Utilty._

class Adam(val n: Int,
  val eps:Float= 0.0002f,
  val rho1:Float= 0.5f,
  val rho2:Float= 0.999f){

  val delta = 1e-5
  var rho1t = 1:Float
  var rho2t = 1:Float
  var s = Array.ofDim[Float](n)
  var r = Array.ofDim[Float](n)

  def update(K: Array[Float], dK: Array[Float]) = {
    var nK = Array.ofDim[Float](K.size)
    rho1t *= rho1
    rho2t *= rho2
    val rho1tr = 1 / (1 - rho1t)
    val rho2tr = 1 / (1 - rho2t)
    for (i <- 0 until K.size) {
      s(i) = rho1 * s(i) + (1 - rho1) * dK(i)
      r(i) = rho2 * r(i) + (1 - rho2) * dK(i) * dK(i)
      val d = (s(i) * rho1tr) / (math.sqrt(r(i) * rho2tr) + delta)
      K(i) = (K(i) - eps * d).toFloat
    }
  }
}
