package Layer
import math._
import kata._
import Utilty._

class Convolution_Matver(
  val FS:Int,//カーネルの高さ
  val IC:Int, //入力チャネル数
  val IH:Int, //入力の高さ
  val IW:Int, //入力の幅
  val OC:Int, //出力チャネル数
  val eps:Float=0.001f,
  val rho1:Float=0.5f,
  val rho2:Float = 0.999f

) extends Layer {
  var X = Array[T]()
  val rand = new scala.util.Random(0)
  var F = Array.ofDim[T](OC*IC*FS*FS).map(a => rand.nextGaussian.toFloat*0.01f)
  val OW = IW-FS+1
  val OH = IH-FS+1
  var dF=Array.ofDim[T](IC*OC*FS*FS)


  def transpose(x:Array[T],row:Int,col:Int)={
    var xt = Array.ofDim[T](row*col)
    for(i <- 0 until col;j <- 0 until row){
      xt(i*row+j) = x(i+col*j)
    }
    xt
  }
  def reshape_x(x:Array[T],bs:Int=1)={
    def index2(n:Int,i:Int,j:Int,k:Int,l:Int,m:Int)=
      (i*FS*FS+l*FS+m)*OW*OH*bs+n*OW*OH+j*OW+k
    def ondex2(n:Int,i:Int,j:Int,k:Int,l:Int,m:Int) =
      n*IW*IH*IC +  i*IH*IW + j*IW+k + l*IW+m

    var xd = Array.ofDim[T](IC*FS*FS*OW*OH*bs)
    for(n <- 0 until bs;i <- 0 until IC;j <- 0 until OH;k <- 0 until OW){
      for(l <- 0 until FS;m <- 0 until FS){
        xd(index2(n,i,j,k,l,m)) = x(ondex2(n,i,j,k,l,m))
      }
    }
    xd
  }
  def reshape_y(yd:Array[T],bs:Int)={
    def index3(i:Int,j:Int,k:Int,l:Int) = i*OW*OH + j*OW*OH*bs + k*OW+l
    var y = Array.ofDim[T](OW*OH*OC*bs)
    var n = 0
    for(i <- 0 until bs; j <- 0 until OC){
      for(k <- 0 until OH;l <- 0 until OW){
        y(n) = yd(index3(i,j,k,l))
        n += 1
      }
    }
    y
  }
  def forward(x:Array[T])={
    val xd = reshape_x(x)
    X = xd
    val y = Array.ofDim[T](OW*OH*OC)
    blas.BLAS.matmul(F,xd,y,OC,OW*OH,FS*FS*IC)
    y
  }

  override def forward(x:Array[Array[T]]):Array[Array[T]]={
    val bs = x.size
    val xd = reshape_x(x.flatten,bs)
    X = xd
    val yd = Array.ofDim[T](OW*OH*OC*bs)
    blas.BLAS.matmul(F,xd,yd,OC,OW*OH*bs,FS*FS*IC)
    val y = reshape_y(yd,bs)
    y.grouped(OW*OH*OC).toArray
  }
  def reshape_d(d:Array[T],bs:Int=1)={
    var dd = Array.ofDim[T](OC*OW*OH*bs)
    var index = 0
    for(i <- 0 until OC;j <- 0 until bs;k <- 0 until OW*OH){
      dd(index) = d(i*OW*OH+j*OW*OH*OC+k)
      index += 1
    }
    dd
  }
  def sum_dx(dx:Array[T])={
    var index = 0
    var d_x = Array.ofDim[T](IC*IH*IW)
    for(i <- 0 until IC;j <- 0 until OH;k <- 0 until OW;l <- 0 until FS;m <- 0 until FS){
      d_x(i*IH*IW+j*IW+k+l*IW+m) += dx(index)
      index+=1
    }
    d_x
  }
  def reshape_dx(dx:Array[T],bs:Int)={
    var d_x =Array.ofDim[T](bs,FS*FS*IC*OW*OH)
    for(i <- 0 until bs){
      var index = 0
      for(j <- 0 until FS*FS*IC;k <- 0 until OW*OH){
        d_x(i)(index) = dx(i*OW*OH+j*OW*OH*bs+k)
        index += 1
        // println((i*OW*OH+j*OW*OH*bs+k))
      }

    }
    d_x
  }
  def backward(d:Array[T])={
    var dx = Array.ofDim[T](FS*FS*IC*OW*OH)
    var d_x= Array.ofDim[T](IC*IH*IW)
    blas.BLAS.matmul(d,transpose(X,FS*FS*IC,OW*OH),dF,OC,FS*FS*IC,OW*OH)
    blas.BLAS.matmul(transpose(F,OC,FS*FS*IC),d,dx,FS*FS*IC,OW*OH,OC)
    d_x = sum_dx(dx)
    d_x
  }
  override def backward(d:Array[Array[T]]):Array[Array[T]]={
    val bs = d.size
    var dx = Array.ofDim[T](bs*FS*FS*IC*OW*OH)//for cal
    var d_x= Array.ofDim[T](bs,IC*IH*IW)//for return
    val dd = reshape_d(d.flatten,bs)
    blas.BLAS.matmul(dd,transpose(X,FS*FS*IC,OW*OH*bs),dF,OC,FS*FS*IC,OW*OH*bs)
    blas.BLAS.matmul(transpose(F,OC,FS*FS*IC),dd,dx,FS*FS*IC,OW*OH*bs,OC)
     d_x = reshape_dx(dx,bs).map(sum_dx(_))
    /*for(i <- 0 until bs){
     d_x(i) = sum_dx(tmp(i))
     }*/
    d_x
  }


  var adam_K = new Adam(F.size, eps, rho1, rho2)
  def update(){
    //  println(F(0).toString+" "+F(10).toString+" "+F(20).toString+" "+F(30).toString+" "+F(40).toString)
    update_adam()
    reset()
  }
  def update_adam() {
    adam_K.update(F, dF)
  }

  def reset() {
    dF = Array.ofDim[T](OC*IC*FS*FS)
    X = Array[T]()
  }
  override def save(fn:String) {
    val pw = new java.io.PrintWriter(fn)
    for(i <- 0 until F.size ) {
      pw.write(F(i).toString)
      if(i != F.size-1) {
        pw.write(",")
      }
    }
    pw.write("\n")
    pw.close()

  }

  override def load(fn:String) {
    val f = scala.io.Source.fromFile(fn).getLines.toArray
    val tmp = f(0).split(",").map(_.toFloat).toArray
 //   println("\nFsize> "+F.size+" tmpsize> "+tmp.size)
    for(i <- 0 until tmp.size) {
      F(i) = tmp(i)
    }
  }
}








class Convolution3D(
  val KW:Int,val IH:Int,val IW:Int,val IC:Int,val OC:Int,
  val ss:Int=1,
  val e:T = 0.01f,
  val p1:T = 0.9f)extends Layer {
  val OH = 1 + (IH-KW)/ss //IH - KW + 1
  val OW = 1 + (IW-KW)/ss // w - kw + 1
  val rand = new scala.util.Random(0)
  var t=0
  var p1_t=1f
  var p2_t=1f
  var K = Array.ofDim[T](OC,IC,KW*KW).map(_.map(_.map(a => rand.nextGaussian.toFloat*0.01f)))
  var V_D = List[Array[T]]()
  var D_K = Array.ofDim[T](OC,IC,KW*KW)
  var s = Array.ofDim[T](OC,IC,KW*KW)
  var r = Array.ofDim[T](OC,IC,KW*KW)
  var n = 0f

  def iindex(i:Int, j:Int, k:Int) = i * IH*IW + j * IW + k
  def oindex(i:Int, j:Int, k:Int) = i * OH*OW + j * OW + k

  def push(V:Array[T]) = { V_D ::= V; V }
  def pop() = { val V = V_D.head; V_D = V_D.tail; V }

  def forward(V:Array[T]) = {
    push(V)
    val Z = Array.ofDim[T](OC*OH*OW)//1 + (IW-KW)/ss)

    for(i<-0 until OC ; j<-0 until OH ; k<-0 until OW){
      var s = 0f
      for(l<-0 until IC ; m<-0 until KW ; n<-0 until KW){
        s +=  V(iindex(l,j*ss+m,k*ss+n)) * K(i)(l)(m*KW+n)
      }
      Z(oindex(i,j,k)) = s
    }
    Z
  }

  def backward(G:Array[T]) = {

    val x = pop()
    n += 1f

    for(i<-0 until OC ;  j<-0 until IC ; k<-0 until KW ; l<-0 until KW){
      var s = 0f
      for(m<-0 until  OH ; n<-0 until OW){
        s += G(oindex(i,m,n)) * x(iindex(j,m*ss+k,n*ss+l))
      }

      D_K(i)(j)(k*KW+l) = s
    }

    val dV = Array.ofDim[T](IC * IH * IW)

    for(i<-0 until IC ; j<-0 until IH ; k<-0 until IW){
      var s1=0f
      for(l <- math.max((j-KW)/ss,0) until math.min(j/ss+1,OH) ; m <- 0 until KW){
        if(j==l*ss+m){
          for(n <- math.max((k-KW)/ss,0) until math.min(k/ss+1,OW) ; p<-0 until KW){
            if(k==n*ss+p)
              for(q<-0 until OC)
                s1 += K(q)(i)(m*KW+p)*G(oindex(q,l,n))
          }
        }
      }
      dV(iindex(i,j,k)) = s1
    }
    dV
  }

  def update() {

    for(i<-0 until OC ; j<-0 until IC ; k<-0 until KW*KW){
      D_K(i)(j)(k) = D_K(i)(j)(k)/n
    }
    val p2 = 0.999f
    val delta = 0.00000001f

    var s_h = Array.ofDim[T](OC,IC,KW*KW)
    var r_h = Array.ofDim[T](OC,IC,KW*KW)

    t += 1

    p1_t = p1 * p1_t
    p2_t = p2 * p2_t

    for(i<-0 until OC ; j<-0 until IC ; k<-0 until KW*KW){
      var D_s = 0f//ΔΘ

      s(i)(j)(k) = p1*s(i)(j)(k)+(1-p1)*D_K(i)(j)(k)
      r(i)(j)(k) = p2*r(i)(j)(k)+(1-p2)*D_K(i)(j)(k)*D_K(i)(j)(k)

      s_h(i)(j)(k) = s(i)(j)(k)/(1-p1_t)
      r_h(i)(j)(k) = r(i)(j)(k)/(1-p2_t)

      D_s = -1*e*s_h(i)(j)(k)/(math.sqrt(r_h(i)(j)(k))+delta).toFloat
      K(i)(j)(k) = K(i)(j)(k) + D_s

    }
    n=0f
    reset()
  }

  def update2(){
    val lr = 0.9f
    for(i<-0 until OC ; j<-0 until IC ; k<-0 until KW*KW)
      K(i)(j)(k) -= lr * D_K(i)(j)(k)
    reset()
  }

  def reset() {

    D_K = Array.ofDim[T](OC,IC,KW*KW)
  }

  override def save(fn:String) {
    val pw = new java.io.PrintWriter(fn)
    for(i <- 0 until OC ; j <- 0 until IC ; k <- 0 until KW*KW) {
      pw.write(K(i)(j)(k).toString)
      if(i != OC - 1 || j != IC-1 || k != KW*KW-1 ) {
        pw.write(",")
      }
    }
    pw.write("\n")
    pw.close()
  }

  override def load(fn:String) {
    val f = scala.io.Source.fromFile(fn).getLines.toArray
    val tmp = f(0).split(",").map(_.toFloat).toArray
    for(i <- 0 until OC ; j <- 0 until IC ; k <- 0 until KW*KW) {
      K(i)(j)(k) = tmp(i*IC*KW*KW+j*KW*KW+k)
    }
  }
}
