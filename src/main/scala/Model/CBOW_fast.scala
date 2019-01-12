package word2vec
class CBOWfast(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import formula.Utilty_formula._
  import Activation._
  val eps : Float = 0.001f
  val rho1: Float = 0.9f
  val rho2: Float = 0.999f

  val rand = new scala.util.Random(0)
  var Win =new Embedding(xn,hidden)
  val Wout= new Embedding(hidden,xn)
  val sig  = new Sigmoid()
  val stack = new Stack[List[Float]]()
  def load_word_count(words:Array[String])={
    var word = List[String]()
    for(w <- words){
      if(!word.contains(w)){
        word ::= w
      }
    }
    word
  }
  def train(){

  }
  def forward(Contexts:Array[Int])={//数字のリストを受け取る(targetの周辺単語)

  }

  def backward(ds:Array[Float])={
    val da = Wout.backward(ds) * 0.5f

    ds
  }

  def update(){
    Win.update
    Wout.update
    reset()
  }

  def reset(){
    Win.reset
    Wout.reset

  }
  def load_Enbedding()={
    val pathName = "Emvedding/CBOWfast_"+title+"_"+xn+"x"+hidden+".txt"
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    Win.W = f(0).split(",").map(_.toFloat).toArray
  }
  def save_Distributed_Representation()={
    val pathName = "Emvedding/CBOWfast_"+title+"_"+xn+"x"+hidden+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    val ys1 = Win.W.mkString(",")

    writer.write(ys1)
    writer.close()
    println("success "+pathName)

  }

  def onehot(a:Int,size:Int) = {
    var  R = Array.ofDim[Float](size)
    R(a) = 1f
    R
  }
  def Context_num(in:Array[Int],Windowsize:Int)={
    val R = new Stack[Array[Int]]()
    val size= in.size
    for(i <- 0 until size){
      val x = new Stack[Int]()
      for(j <- -Windowsize to Windowsize){
        if(i + j >= 0 && i+j <= size -1 && j != 0){
          x.push(in(i+j))
        }
      }
      R.push(x.full.toArray.reverse)
    }
    R.full.toArray.reverse.init.tail
  }
  def crossEntropy(a:Array[Float],b:Array[Float])={
    var loss = 0d
    for(i <- 0 until a.size){
      loss += a(i) * math.log(b(i)+0.0001f)
    }
    loss.toFloat / a.size
  }

}
