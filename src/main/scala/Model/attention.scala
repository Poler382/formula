package Layer
class Attention(){
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._
  val sf = new SoftMax()
  var hs_stack = new Stack[Array[Float]]()
  var ht_stack = new Stack[Array[Float]]()
  var hs_backwardloss = new Stack[Array[Float]]()
  var score_stack = new Stack[Array[Float]]()

  def get_hs_backwardloss()={
    hs_backwardloss.full.reverse.toArray
  }

  def Encoder_forward(x:Array[Float])={
    hs_stack.push(x)
    x
  }
  def repeat(x:Array[Float],size:Int)={
    var xs = Array.ofDim[Float](size,x.size)
    Range(0,size-1).map{i => xs(i) = x}
    xs
  }

  def sum(x:Array[Float],size:Int)={
    val xs = x.grouped(size).toArray //sizeは何個ずつまとめたいか
    Range(1,xs.size-1).map{i =>xs(0) += xs(i)}
    xs(0)
  }

  def backward(d:Array[Float])={
    val ds = repeat(d,hs_stack.len)
    val encoder_hslist = hs_stack.full.reverse.toArray.flatten

    val d2 = sf.backward(sum(ds.flatten * encoder_hslist,d.size))

    Range(0,hs_stack.len).map{i => hs_backwardloss.push( d * score_stack.head + d2 * ht_stack.head) }
    score_stack.pop//reset

    sum(encoder_hslist * repeat(d2,hs_stack.len).flatten,d.size)
  }


  def cal_score(encoder_hslist:Array[Array[Float]],h:Array[Float])={
    var InnerProduct = new Array[Float](encoder_hslist(0).size)
    print(s"   encoder size ${encoder_hslist.size}   ")
    Range(0,encoder_hslist.size-1).map{ i =>
      InnerProduct(i) = encoder_hslist(i) dot h
    }

    sf.forward(InnerProduct)
  }

  def Decoder_forward(ht:Array[Float])={//htはデコーダーの隠れ層の出力
    ht_stack.push(ht)
    val encoder_hslist = hs_stack.full.reverse.toArray
    val score = cal_score(encoder_hslist,ht)
    println(s"score.size ${score.size}")
    score_stack.push(score)
    var ContextVector = Array.ofDim[Float](encoder_hslist(0).size)
    for(i <- 0 until hs_stack.len){
      ContextVector += encoder_hslist(i) * score
    }
    ContextVector
  }

  def reset()={
    hs_stack.reset
    ht_stack.reset
    hs_backwardloss.reset
    score_stack.reset
  }


}
