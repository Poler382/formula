package Layer
object Checker {
  type T = Float

  val rand = new util.Random(0)

  def ms(a:Long) = a / 1000000f

  def diff(x:Array[Float], t:Array[Float]) = sub(x,t)

  def add(x:Array[T], y:Array[T]) : Array[T] =
    (0 until x.size).map(i => x(i) + y(i)).toArray

  def sub(x:Array[Float], y:Array[Float]) : Array[Float] =
    (0 until x.size).map(i => x(i) - y(i)).toArray

  def add(x:Array[Array[T]], y:Array[Array[T]]) : Array[Array[T]] =
    (0 until x.size).map(i => add(x(i),y(i))).toArray

  def sub(x:Array[Array[T]], y:Array[Array[T]]) : Array[Array[T]] =
    (0 until x.size).map(i => sub(x(i),y(i))).toArray

  def neg(x:Array[Array[T]]) : Array[Array[T]] =
    x.map(_.map(a => -a))

  def random_uniform(n:Int, sigma:Float = 0.05f) = {
    (0 until n).map(i => sigma * (rand.nextFloat * 2 - 1)).toArray
  }

  def random_normal(n:Int, sigma:Float = 0.05f) = {
    (0 until n).map(i => sigma * rand.nextGaussian.toFloat).toArray
  }

  def numeric_diff(layer:Layer, xs:Array[Array[T]], p:Int = 0, q:Int = 0) = {
    val epsilon = 0.00001f
    val ds = Array.ofDim[T](xs.size, xs(0).size)
    for(i <- 0 until xs.size; j <- 0 until xs(0).size) {
      val xs1 = xs.map(_.clone)
      xs1(i)(j) -= epsilon
      val xs2 = xs.map(_.clone)
      xs2(i)(j) += epsilon

      layer.reset()
      val y1 = layer.forward(xs1)
      layer.reset()
      val y2 = layer.forward(xs2)
      val d = sub(y2,y1).map(_.map(_ / (2 * epsilon)))
      ds(i)(j) = d(p)(q)
    }
    layer.reset()
    ds
  }

  def test_layer(layer:Layer, dn:Int, xn:Int, yn:Int) = {
    var dxs_diffs = List[Array[Array[T]]]()
    for(i <- 0 until dn; j <- 0 until yn) {
   //   println(s"check:$i,$j")
      val xs = (0 until dn).map(_ => random_normal(xn)).toArray
      val ds = Array.ofDim[T](dn,yn); ds(i)(j) = 1f
      layer.reset()
      val y = layer.forward(xs)
      val dxs_bp = layer.backward(ds)
      val dxs_nd = numeric_diff(layer,xs,i,j)
      val dxs_diff = sub(dxs_bp, dxs_nd)
      dxs_diffs ::= dxs_diff
   /*   println(s"dxs_bp($i,$j):\n${dxs_bp.map(_.mkString(",")).mkString("\n")}")
      println(s"dxs_nd($i,$j):\n${dxs_nd.map(_.mkString(",")).mkString("\n")}")
      println(s"dxs_diff($i,$j):\n${dxs_diff.map(_.mkString(",")).mkString("\n")}")*/
    }
    val dxs_diffs1 = dxs_diffs.reverse.toArray
    var is_ok = true
    var diff_max = 0f
    for(i <- 0 until dn; j <- 0 until yn) {
    //  println(s"dxs_diff($i,$j):${dxs_diffs1(i*yn+j).flatten.mkString(",")}")
      diff_max = math.max(diff_max,dxs_diffs1(i*yn+j).flatten.map(math.abs _).max)
      if(dxs_diffs1(i*yn+j).flatten.exists(_ > 1e-2)) {
        is_ok = false
      }
    }
    println(s"check:$is_ok diff_max:$diff_max")
    is_ok
  }

    def Affine_w_diff(layer:Affine,xs:Array[Array[T]])={
    val epsilon = 0.0001f
    val Ws = layer.W.grouped(layer.xn).toArray
    val ds = Array.ofDim[T](layer.yn,layer.xn)

    for(i <- 0 until layer.yn;j <- 0 until layer.xn){
      val ws1 = Ws.map(_.clone)
      ws1(i)(j) -= epsilon
      val ws2 = Ws.map(_.clone)
      ws2(i)(j) += epsilon

      layer.reset()
      layer.W = ws1.flatten
      val y1 = layer.forward(xs)
      layer.reset()
      layer.W = ws2.flatten
      val y2 = layer.forward(xs)
      val d = sub(y2,y1).map(_.map(_ / (2 * epsilon)))
      for(p <- 0 until d.size;q <- 0 until d(0).size){
        ds(i)(j) += d(p)(q)
      }
    }
    layer.reset()
    ds
  }

  def Affine_b_diff(layer:Affine,xs:Array[Array[T]])={
    val epsilon = 0.0001f
    val bs = layer.b//.grouped(layer.xn).toArray
    val ds = Array.ofDim[T](layer.yn)

    for(i <- 0 until layer.yn){
      val bs1 = bs.clone
      bs1(i) -= epsilon
      val bs2 = bs.clone
      bs2(i) += epsilon

      layer.reset()
      layer.b = bs1
      val y1 = layer.forward(xs)
      layer.reset()
      layer.b = bs2
      val y2 = layer.forward(xs)
      val d = sub(y2,y1).map(_.map(_ / (2 * epsilon)))
      for(p <- 0 until d.size){
        ds(i) += d(p)(i)
      }
    }
    layer.reset()
    ds
  }


  def test_w_layer(layer:Affine,dn:Int,xn:Int,yn:Int)={
    var dws_diffs = List[Array[Array[T]]]()
    for(i <- 0 until dn;j <- 0 until yn){
 //     println(s"check:$i,$j")
      val xs = (0 until dn).map(_ => random_normal(xn)).toArray
      val ds = Array.ofDim[T](dn,yn);
      for(p <- 0 until dn;q <- 0 until yn ){
        ds(p)(q) = 1f
      }
      layer.reset()
      val y = layer.forward(xs)
      layer.backward(ds)
      val dws_bp = layer.dW.grouped(layer.xn).toArray
      val dws_nd = Affine_w_diff(layer,xs)
      val dws_diff = sub(dws_bp, dws_nd)
      dws_diffs ::= dws_diff
 //     println(s"dws_bp($i,$j):\n${dws_bp.map(_.mkString(",")).mkString("\n")}")
  //   println(s"dws_nd($i,$j):\n${dws_nd.map(_.mkString(",")).mkString("\n")}")
   // println(s"dws_diff($i,$j):\n${dws_diff.map(_.mkString(",")).mkString("\n")}")
    }
    val dws_diffs1 = dws_diffs.reverse.toArray
    var is_ok = true
    var diff_max = 0f
    for(i <- 0 until dn; j <- 0 until yn) {
    //  println(s"dws_diff($i,$j):${dws_diffs1(i*yn+j).flatten.mkString(",")}")
      diff_max = math.max(diff_max,dws_diffs1(i*yn+j).flatten.map(math.abs _).max)
      if(dws_diffs1(i*yn+j).flatten.exists(_ > 1e-3)) {
        is_ok = false
      }
    }
    println(s"check:$is_ok diff_max:$diff_max")
    is_ok
  }
  def test_b_layer(layer:Affine,dn:Int,xn:Int,yn:Int)={
    var dbs_diffs = List[Array[T]]()
    for(i <- 0 until dn; j <- 0 until yn) {
   //   println(s"check:$i,$j")
      val xs = (0 until dn).map(_ => random_normal(xn)).toArray
      val ds = Array.ofDim[T](dn,yn)
      for(p <- 0 until ds.size;q <- 0 until ds(0).size){
        ds(p)(q) = 1f
      }
      layer.reset()
      val y = layer.forward(xs)
      layer.backward(ds)
      val dbs_bp = layer.db.clone
      val dbs_nd = Affine_b_diff(layer,xs)
      val dbs_diff = sub(dbs_bp, dbs_nd)
      dbs_diffs ::= dbs_diff
/*      println(s"dbs_bp($i,$j):\n${dbs_bp.mkString(",")}")

      println(s"dbs_nd($i,$j):\n${dbs_nd.mkString(",")}")
      println(s"dbs_diff($i,$j):\n${dbs_diff.mkString(",")}")*/
    }
    val dbs_diffs1 = dbs_diffs.reverse.toArray
    var is_ok = true
    var diff_max = 0f
    for(i <- 0 until dn; j <- 0 until yn) {
     // println(s"dbs_diff($i,$j):${dbs_diffs1(i*yn+j).mkString(",")}")
      diff_max = math.max(diff_max,dbs_diffs1(i*yn+j).map(math.abs _).max)
      if(dbs_diffs1(i*yn+j).exists(_ > 1e-3)) {
        is_ok = false
      }
    }
    println(s"check:$is_ok diff_max:$diff_max")
    is_ok


  }

  def test() = {
    val a = new Affine(28*28,100)
    val rs = (0 until 100).map(_ => test_layer(a,3,a.xn,a.yn)).toArray
    rs
  }

}
