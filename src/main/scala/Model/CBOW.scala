package word2vec
class CBOW(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import Utilty.timer._
  //val Win = new Embedding(xn,hidden)
  val Win = new Linear(xn,hidden)
  val Wout= new Linear(hidden,xn)
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

  def forward(Contexts:Array[Int])={//数字のリストを受け取る(targetの周辺単語)

    var win = Array.ofDim[Float](hidden)
    for(i <- 0 until Contexts.size){
      win = win + Win.forward(onehot(Contexts(i),xn)) // 数字をonehotに変えてバッチとして流す
    }
    val y = Wout.forward(win / Contexts.size)
    sf.forward(y)
  }

  def backward(ds:Array[Float])={
    val da = Wout.backward(ds) * 0.5f
    for(i <- 0 until Layer_num){
      Win.backward(da)
    }
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
    val pathName = "Emvedding/CBOW_"+title+"_"+xn+"x"+hidden+".txt"
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    Win.W = f(0).split(",").map(_.toFloat).toArray
  }
  
  def save_Distributed_Representation()={
    val pathName = "Emvedding/CBOW_"+title+"_"+xn+"x"+hidden+".txt"
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

  def trains(onehotss:Array[Array[Array[Float]]],epoch:Int)={
    var lossList = List[String]()
    for(i <- 0 until epoch){
      start()
      var Loss = 0d; var cm = 0f; var all = 0f
      for(onehots <- onehotss){
        val number = onehots.map{a => a.indexOf(a.max)}.toArray
        //  val CBOWModel = new CBOW(onehots(0).size,hidden,Windowsize*2,Windowsize)
        val Contexts = Context_num(number,Windowsize)



        for(j <- 0 until Contexts.size){
          val y = forward(Contexts(j))
          if(onehots(j+1).indexOf(onehots(j+1).max) ==y.indexOf(y.max)){
            cm+=1
          }
          Loss += crossEntropy(onehots(j+1),y)
          all+=1
          backward(y-onehots(j+1))

        }
        update()
      }
      if(i % 500 == 0){
        save_Distributed_Representation()
      }
      if(i % 10 == 0){
        println("epoch : "+i," count: "+cm/all," Loss : "+ math.exp(-Loss),"crossEntropy : "+ -Loss," time : "+finish())
      }
      lossList ::= (math.exp(-Loss/all)).toString

    }
    savetxt_String(lossList,"perplexity","Embedding")
  }


  def train(onehots:Array[Array[Float]],epoch:Int)={
    val number = onehots.map{a => a.indexOf(a.max)}.toArray
    //  val CBOWModel = new CBOW(onehots(0).size,hidden,Windowsize*2,Windowsize)
    val Contexts = Context_num(number,Windowsize)
    var lossList = List[String]()
    for(i <- 0 until epoch){
      var Loss = 0d;var cm = 0f;var all =0f
      start()
      for(j <- 0 until Contexts.size){
        val y = forward(Contexts(j))
        if(onehots(j+1).indexOf(onehots(j+1).max) ==y.indexOf(y.max)){
          cm+=1
        }
        Loss += crossEntropy(onehots(j+1),y)
        all+=1
        backward(y-onehots(j+1))

      }
      update()
      if(i % 100 == 0){
        save_Distributed_Representation()
      }
      if(i % 10 == 0){
        println("epoch : "+i," count: "+cm/all," Loss : "+ math.exp(-Loss),"crossEntropy : "+ -Loss," time : "+finish())
      }
      lossList ::= (math.exp(-Loss/all)).toString
    }
    savetxt_String(lossList,"perplexity","/Users/yuya/Programing/sbt/formula")
  }
}
