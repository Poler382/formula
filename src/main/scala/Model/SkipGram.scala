package word2vec
class SkipGram(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import formula.protcol._
  import Utilty.call_py._
  import formula.Utilty_formula._
  import scala.sys._
  import java.util.Date
  import java.io.File
  import scala.io.Source
  val timer = new Utilty.timer()
  val Win = new Linear(xn,hidden)
  val Wout =new Linear(hidden,xn)
  val sf  = new SoftMax()

  def train(path:String,epoch:Int)={
    var finalpathname = ""
    val words = load_word_count(path).toList
    val q = question_load_all2(path,words)
    var lossList = List[String]()
    var Loss = 0d;var cm = 0f;var all =0f
    Wout.load("biasdata/SkipGram.wout.txt")

    for (ep <- 0 until epoch){
      timer.timestart()
      for(i <- 0 until q.size){
        //  println("q(i) => "+q(i).mkString(" "))

        val Context = Context_num(q(i),2)

        for(target <- 1 until Context.size){
          val y = forward(onehot(target,xn),Context(target-1))
          val t = Context(target-1).map(onehot(_,xn))//予測したであろう単語との誤差を取っていく

          backward(y-t)

          all += t.size
          Range(0,t.size-1).par.map{ r =>
            if(t(r).indexOf(t(r).max) == y(r).indexOf(y(r).max)){
              cm+=1
            }
            Loss += crossEntropy(t(r),y(r))
          }

        }

      }
      if((ep % 200 == 0 && ep != 0) || (ep == epoch -1 )){
        val p = save_Distributed_Representation()
        finalpathname = p
      }
      update()
      System.gc()
      lossList ::= (math.exp(-Loss/all)).toString

      if(ep % 10 == 0){
        //        println("epoch : "+ep," count: "+cm/all*1000," Loss : "+ math.exp(-Loss),"crossEntropy : "+ -Loss," time : "+finish())
        var str_ep =String.format("ep:%5s ",ep.toString);
        var str_count = String.format(" count: %-10s",(cm/all*100).toString)
        var str_CE = String.format(" crossEntropy: %18s ",(-Loss).toString)
        var str_perplexity = String.format(" perplexity: %18s ",(lossList.head.toString))

        var str_time = String.format(" time: %6s",(timer.timefinish).toString)

        println(str_ep+str_count+str_CE+str_perplexity+str_time)
        Loss = 0d
        cm = 0f
        all =0f
      }

      if(ep % 100 == 0 ){
        savetxt_String(lossList,"perplexity_skipGram_"+timer.date(),"txt")
        Win.save("biasdata/SkipGram.win.txt")
        Wout.save("biasdata/SkipGram.wout.txt")
      }

    }

    savetxt_String(lossList,"perplexity_skipGram_"+timer.date(),"txt")
    finalpathname
  }

  def load_word_count(path:String)={
    //単語をスペース区切りで出てきた単語を保存していく
    val words = scala.io.Source.fromFile(path).getLines.map(_.split(" ")).toArray.flatten

    var word = List[String]()
    for(w <- words){
      if(!word.contains(w)){
        word ::= w
      }
    }
    word.sorted
  }

  def forward(target:Array[Float],Contexts:Array[Int])={
    val y = Array.ofDim[Float](Contexts.size,xn)
    for(i <- 0 until Contexts.size){
      y(i) = sf.forward(Wout.forward(Win.forward(target)))
    }
    y
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
  def load_Enbedding(pathName:String="Emvedding/SkipGram_"+title+"_"+xn+"x"+hidden+".txt"
  )={
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    Win.W = f(0).split(",").map(_.toFloat).toArray
  }

  def save_Distributed_Representation()={
    val pathName = "Emvedding/SkipGram_"+title+"_"+xn+"x"+hidden+"_"+timer.date+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    val ys1 = Win.W.mkString(",") + "\n"
    writer.write(ys1)
    writer.close()
    println("success "+pathName)
    pathName
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
