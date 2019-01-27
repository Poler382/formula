package word2vec
class SkipGram(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  val Win = new Linear(xn,hidden)
  val Wout =new Linear(hidden,xn)
  val sf  = new SoftMax()

  def load_word_count(words:Array[String])={
    var word = List[String]()
    for(w <- words){
      if(!word.contains(w)){
        word ::= w
      }
    }
    word
  }

  def trains(onehotss:Array[Array[Array[Float]]],epoch:Int)={
    var lossList = List[String]()

    for(i <- 0 until epoch){
      for (onehots<- onehotss){
        val number = onehots.map{a => a.indexOf(a.max)}.toArray
        //  val CBOWModel = new CBOW(onehots(0).size,hidden,Windowsize*2,Windowsize)
        val Contexts = Context_num(number,Windowsize)

        var Loss = 0f; var cm = 0f; var all = 0f
        var time = System.currentTimeMillis()

        for(j <- 0 until onehots.size){

          val y = forward(onehots(j),Contexts(j))
          val c = Contexts(j).map(onehot(_,onehots(0).size))

          val ys = y.map(a => a.indexOf(a.max))
          val cs = Contexts(j)

          for(k <- 0 until ys.size){
            if(ys(k) == cs(k)){
              cm +=  1f
            }
            all += 1f
          }
          Loss += crossEntropy(c.flatten,y.flatten)

          backward(y - c)
        }

        update()
        if(i % 500 == 0){
          save_Distributed_Representation()
        }
        if(i % 10 == 0){
          println("epoch : "+i," count: "+cm/all," Loss : "+ math.exp(-Loss),"crossEntropy : "+ -Loss," time : "+(System.currentTimeMillis()-time))
        }
        lossList ::= (math.exp(-Loss/all)).toString
      }
    }
    savetxt_String(lossList,"perplexity_skip","/Users/yuya/Programing/sbt/formula")
  }

  def train(onehots:Array[Array[Float]],epoch:Int)={
    val number = onehots.map{a => a.indexOf(a.max)}.toArray
    val Contexts = Context_num(number,Windowsize)
    var lossList = List[String]()
    for(i <- 0 until epoch){
      var Loss = 0d;var cm = 0f;var all = 0f
      var time = System.currentTimeMillis()

      for(j <- 0 until onehots.size){

        val y = forward(onehots(j),Contexts(j))
        val c = Contexts(j).map(onehot(_,onehots(0).size))

        val ys = y.map(a => a.indexOf(a.max))
        val cs = Contexts(j)

        for(k <- 0 until ys.size){
          if(ys(k) == cs(k)){
            cm +=  1f
          }
          all += 1f
        }
        Loss += crossEntropy(c.flatten,y.flatten)

        backward(y - c)
      }



      update()
      if(i % 100 == 0){
        save_Distributed_Representation()
      }
      if(i % 10 == 0){
        println("epoch : "+i," count: "+cm/all," Loss : "+ math.exp(-Loss),"crossEntropy : "+ -Loss," time : "+(System.currentTimeMillis()-time))
      }
      lossList ::= (math.exp(-Loss/all)).toString
    }
    savetxt_String(lossList,"perplexity_skip","/Users/yuya/Programing/sbt/formula")
  }

  def forward(target:Array[Float],Contexts:Array[Int])={
    val y = Array.ofDim[Float](Contexts.size,xn)
    for(i <- 0 until Contexts.size){
      y(i) = sf.forward(Wout.forward(Win.forward(target)))
    }
    y
  }

  def load_Enbedding()={
    val pathName = "Emvedding/SkipGram_"+title+"_"+xn+"x"+hidden+".txt"
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    Win.W = f(0).split(",").map(_.toFloat).toArray
  }

  def backward(ds: Array[Array[Float]])={ //誤差の配列を送る
    var da = Array.ofDim[Float](hidden)
    for(i <- 0 until ds.size){
      da = da + Wout.backward(ds(i))
    }
    Win.backward(da)
  }

  def update(){
    Wout.update
    Win.update
    reset()
  }

  def reset(){
    Wout.reset
    Win.reset
  }
  def onehot(a:Int,size:Int) = {
    var  R = Array.ofDim[Float](size)
    R(a) = 1f
    R
  }
  def save_Distributed_Representation()={
    val pathName = "Emvedding/SkipGram_"+title+"_"+xn+"x"+hidden+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    val ys1 = Win.W.mkString(",") + "\n"
    writer.write(ys1)
    writer.close()
    println("success "+pathName)
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
    R.full.toArray.reverse
  }
  def crossEntropy(target:Array[Float],probability:Array[Float])={
    var loss =0d
    for(i <- 0 until target.size){
      loss += target(i) * math.log(probability(i)+0.0001f)
    }
    loss.toFloat / target.size
  }
}
