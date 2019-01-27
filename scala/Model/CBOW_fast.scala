package word2vec
class CBOWfast(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import formula.Utilty_formula._
  import Activation._
  import formula.protcol._
  val eps : Float = 0.001f
  val rho1: Float = 0.9f
  val rho2: Float = 0.999f

  val rand  = new scala.util.Random(0)
  var Win   = new Embedding(xn,hidden)
  val Wout  = new Embedding(hidden,xn)
  val sig   = new Sigmoid()

  def load_word_count(words:Array[String])={
    //自然言語のコーパスで使う
    //同じ単語が何回出てきたかを数えることができる
    var word = List[String]()
    for(w <- words){
      if(!word.contains(w)){
        word ::= w
      }
    }
    word
  }
  def get_Probability_Distribution(st:String)={
    var ProbabilityDistribution = Array.ofDim[Float](vec.size)
    for(i <- 0 until st.size){
      ProbabilityDistribution(vec.indexOf(st(i).toString)) +=1f
    }
    val newP = ProbabilityDistribution.map(_/ProbabilityDistribution.sum).map(math.pow(_,0.75).toFloat)
    newP.map(_/newP.sum)
  }
  val PositiveStack = new Stack[List[Float]]()
  val NegativeStack = new Stack[List[Float]]()
  def train_formula(path:String,epoch:Int){
    val ProbabilityDistribution = question_load3(path).flatten.mkString("")
    val numbers =  question_load3(path).map(string2vecnumber(_))

    for(i <- 0 until numbers.size){
      val Contexts = Context_num(numbers(i),Windowsize)
      val targets = numbers(i).init.tail.map(_.toFloat) //先頭と最後を落とす
      var loss = 0f
      for(j <- 0 until Contexts.size){
        //positive
        val posi = forward(Contexts(j),targets(j).toInt)
        //loss = 1f - posi
        //Negative
        val samplelist = vec.toList.filter(_ != targets(j))
        val negaSample = Range(0, 2).map(i => samplelist(rand.nextInt(samplelist.size))).toArray
        for(k <- 0 until negaSample.size){
        //  val nega = forward(Contexts(j),negaSample(k))
    //      loss = 0f - nega
        }
      }

      //backward



    }

  }
  def forward(Contexts:Array[Int],target:Int)={
    //targetは正例の時は真ん中の対象単語
    //負例の時は確率分布からサンプリングした間違ったもの
    var W1 = Array.ofDim[Float](hidden)
    for(i <- 0 until Contexts.size){
      W1 = W1 + Win.forward(Array(Contexts(i).toFloat))
    }
    val W2 = Wout.forward(Array(target.toFloat))
    sig.forward(Array(W1 dot W2))
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
