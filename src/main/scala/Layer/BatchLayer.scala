package Layer
import kata._

class BNa(
  val xn: Int,
  val eps:T= 0.001f,  val rho1:T = 0.9f, val rho2:T= 0.999f) extends Layer {
  var gamma = new Array[T](xn).map(_ => 1:T)
  var beta = new Array[T](xn)
  var dgamma = new Array[T](gamma.size)
  var dbeta = new Array[T](beta.size)
  val adam_gamma = new Adam(gamma.size, eps, rho1, rho2)
  val adam_beta = new Adam(beta.size, eps, rho1, rho2)
  var xmu = Array.ofDim[T](1, xn) // rhs is just a placeholder value
  var sigma = new Array[T](xn)
  val delta = 1e-8f
  var mmu = new Array[T](xn)
  var msigma = new Array[T](xn)
  val decay = 0.999f

  def forward(x: Array[T]): Array[T] = {
    val y = new Array[T](xn)
    for (i <- 0 until xn) {
      val xh = (x(i) - mmu(i)) / (msigma(i) + delta)
      y(i) = xh * gamma(i) + beta(i)
    }
    y
  }

  def backward(d: Array[T]): Array[T] = {
    d
  }

  override def forward(xs: Array[Array[T]]): Array[Array[T]] = {
    val m = xs.size
    xmu = Array.ofDim[T](m, xn)
    for (j <- 0 until xn) {
      var mu = 0:T
      for (i <- 0 until m) {
        mu += xs(i)(j)
      }
      mu /= m
      mmu(j) = decay * mmu(j) + (1 - decay) * mu
      for (i <- 0 until m) {
        xmu(i)(j) = xs(i)(j) - mu
        sigma(j) += (xmu(i)(j) * xmu(i)(j)).toFloat
      }
      sigma(j) = (math.sqrt(sigma(j) / m + delta)).toFloat
      msigma(j) = (decay * msigma(j) + (1 - decay) * sigma(j)):T
    }

    var ys = Array.ofDim[T](m, xn)
    for (j <- 0 until xn) {
      for (i <- 0 until m) {
        ys(i)(j) = gamma(j) * xmu(i)(j) / sigma(j) + beta(j)
      }
    }
    ys
  }

  override def backward(ds: Array[Array[T]]): Array[Array[T]] = {
    val m = ds.size
    var dx = Array.ofDim[T](m, xn)
    for (j <- 0 until xn) {
      for (i <- 0 until m) {
        dbeta(j) += ds(i)(j)
        dgamma(j) += ds(i)(j) * xmu(i)(j) / sigma(j)
      }

      var d1 = new Array[T](m)
      var d2 = 0f
      for (i <- 0 until m) {
        d1(i) = gamma(j) * ds(i)(j)
        d2 += xmu(i)(j) * d1(i)
      }

      val d3 = -d2 / (sigma(j) * sigma(j))
      val d4 = d3 / (2 * sigma(j))
      val d5 = d4 / m

      var d8 = 0:T
      var d10 = new Array[T](m)
      for (i <- 0 until m) {

        val d6 = 2 * xmu(i)(j) * d5
        val d7 = d1(i) / sigma(j)
        d10(i) = (d6 + d7).toFloat
        d8 -= d10(i)
      }
      val d9 = d8 / m

      for (i <- 0 until m) {
        dx(i)(j) = d9 + d10(i)
      }
    }
    dx
  }

  def update() {
    adam_beta.update(beta, dbeta)
    adam_gamma.update(gamma, dgamma)
    reset()
  }

  def reset() {
    dgamma = new Array[T](gamma.size)
    dbeta = new Array[T](beta.size)
  }
}

class GNa(
  val xn:Int,
  val W:Int,
  val H:Int,
  val gs:Int,//チャンネルを何分割するか
  val IC:Int = 1,//input chanel
  val eps:T  = 0.001f,
  val rho1:T = 0.9f,
  val rho2:T = 0.999f
)extends Layer{
  var gamma = Array.ofDim[T](xn*IC*gs).map(_=> 1f)
  var beta  = Array.ofDim[T](xn*IC*gs)
  val delta = 1e-8f
  val adam_gamma = new Adam(gamma.size,eps,rho1,rho2)
  val adam_beta  = new Adam(beta.size,eps,rho1,rho2)
  var dgamma = Array.ofDim[T](xn*IC*gs)
  var dbeta  = Array.ofDim[T](xn*IC*gs)


  def forward(d:Array[T]) : Array[T] = {
    println("nowonownow")
    d
  }

  def backward(d:Array[T]) : Array[T] = {
    d
  }

  var t_myu   = Array.ofDim[T](xn,IC,gs)
  var t_sigma = Array.ofDim[T](xn,IC,gs)
  var t_xhat  = Array.ofDim[T](xn,W*H*IC)

  override def forward(xs:Array[Array[T]]):Array[Array[T]]={
    var y       = Array.ofDim[T](xs.size,xs(0).size)

    for(k <- 0 until xs.size){
      val x = xs(k).clone
      for(ch<- 0 until IC){
        val head = ch * W * H
        for(j<-0 until gs){
          val ghead = W*H/gs * j
          var sum =0f
          for(i <- 0 until W*H/gs){
            sum += x(head+ghead+i)
          }
          t_myu(k)(ch)(j) = sum / W*H/gs

          sum = 0f
          for(i <- 0 until W*H/gs){
            sum += (x(head+ghead+i)-t_myu(k)(ch)(j))*(x(head+ghead+i)-t_myu(k)(ch)(j))
          }

          t_sigma(k)(ch)(j) = math.sqrt(sum/(W*H/gs)+eps).toFloat

          for(i <- 0 until W*H/gs){
            t_xhat(k)(head+ghead+i)=(x(head+ghead+i)-t_myu(k)(ch)(j))/t_sigma(k)(ch)(j)

          }
        }
      }

      for(ch <- 0 until IC;j <- 0 until gs;i <- 0 until W*H/gs){
        val date = x.size
        val head = ch * W * H
        val ghead = W*H/gs * j
        val soeji = date*k+head+ghead+i
        y(k)(head+ghead+i) = gamma(soeji)*t_xhat(k)(head+ghead+i)+beta(soeji)
      }
    }
    y.toArray
  }

  override def backward(ds:Array[Array[T]]):Array[Array[T]]={
    var dx = Array.ofDim[T](ds.size,ds(0).size)

    val date = IC*W*H
    for(i<- 0 until ds.size;ch <- 0 until IC;j<-0 until gs){
      var d2 = Array.ofDim[T](xn,IC,gs)
      val head = ch * W * H
      val ghead = W*H/gs * j
      for(k<- 0 until W*H/gs){
        val soeji = date*k+head+ghead+k
        dbeta(soeji)  += ds(i)(head+ghead+k)
        dgamma(soeji) += ds(i)(head+ghead+k)* t_xhat(k)(head+ghead+k)
        d2(i)(ch)(j)  += gamma(soeji)    * ds(i)(head+ghead+k)
      }

      val d3 = d2(i)(ch)(j) / t_sigma(i)(ch)(j)
      val d4 = t_myu(i)(ch)(j) * d2(i)(ch)(j)

      val d5 = -d4/(t_sigma(i)(ch)(j)*t_sigma(i)(ch)(j))
      val d6 = d5 /(2*t_sigma(i)(ch)(j))

      val d7 = d6 / (W*H/gs)
      var d11 = Array.ofDim[T](IC,gs)

      for(k<- 0 until W*H/gs){

        val xmyu = t_xhat(i)(head+ghead+k)*t_sigma(i)(ch)(j)
        val d8   = 2*xmyu*d7
        d11(ch)(j) += -1*(d3+d8)
      }

      for(k<- 0 until W*H/gs){

        val xmyu = t_xhat(i)(head+ghead+k)*t_sigma(i)(ch)(j)
        val d9   = d3+2*xmyu*d7
        dx(i)(head+ghead+k)=d9+d11(ch)(j)/ (W*H/gs)
      }
    }
    dx
  }
  def update() {
    adam_beta.update(beta, dbeta)
    adam_gamma.update(gamma, dgamma)
    reset()
  }

  def reset() {
    dgamma = Array.ofDim[T](xn*IC*gs)
    dbeta  = Array.ofDim[T](xn*IC*gs)
  }

}

object test{
  def trial()={
 /*   val xn:Int,
  val W:Int,
  val H:Int,
  val gs:Int,//チャンネルを何分割するか
  */
    val xs = Array(
      Array(1f,2f,3f,4f),
      Array(2f,3f,4f,5f),
      Array(3f,4f,5f,6f),
      Array(5f,6f,7f,8f)
    )

    val gn = new GNa(4,2,2,2)

    gn.backward(xs)
  }

}
