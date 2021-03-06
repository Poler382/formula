package word2vec
class CBOW(title:String,xn:Int,hidden:Int,Layer_num:Int=2,Windowsize:Int=1){
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
  //  val Win = new Embedding(xn,hidden)
  val Win = new Linear(xn,hidden)
  val Wout= new Linear(hidden,xn)
  val sf  = new SoftMax()

  def load_word_count(path:String)={
    //単語をスペース区切りで出てきた単語を保存していく
    val words = scala.io.Source.fromFile(path).getLines.map(_.split(" ")).toArray.flatten
    var word = List[String]()
    for(w <- words){
      if(!word.contains(w)){
        word ::= w
      }
    }
    word.sorted:+ "<EOS>" :+ "<UNK>"
  }

  def forward(Contexts:Array[Int])={//数字のリストを受け取る(targetの周辺単語)

    var win = Array.ofDim[Float](hidden)
    for(i <- 0 until Contexts.size){
      win = win + Win.forward(onehot(Contexts(i),xn)) // 数字をonehotに変えてバッチとして流す
    }
    val y = Wout.forward(win * 0.5f)
    sf.forward(y)
  }

  def backward(ds:Array[Float])={
    val da = Wout.backward(ds) * 0.5f
    for(i <- 0 until 2){
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
    sf.reset
  }

  def load_Enbedding(pathName:String = "Emvedding/CBOW_"+title+"_"+xn+"x"+hidden+".txt") = {
    //  val pathName = "Emvedding/CBOW_"+title+"_"+xn+"x"+hidden+".txt"
    val f = scala.io.Source.fromFile(pathName).getLines.toArray
    Win.W = f(0).split(",").map(_.toFloat).toArray
  }

  def save_Distributed_Representation(Acc:String="")={
    val pathName = "Emvedding/CBOW_"+title+"_"+xn+"x"+hidden+"_W"+Windowsize+"_"+timer.date+"_Acc_"+Acc+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    val ys1 = Win.W.mkString(",")

    writer.write(ys1)
    writer.close()
    println("success "+pathName)
    pathName
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
  def train(path:String,epoch:Int)={
    var finalpathname=""
    val words = load_word_count(path).toList
    val q = question_load_all2(path,words)
    var lossList = List[String]()
    var Loss = 0d;var cm = 0f;var all = 0f
    //  Wout.load("biasdata/cbow.wout.txt")
    for (ep <- 0 until epoch){
      timer.timestart()
      for(i <- 0 until q.size){
        //  println("q(i) => "+q(i).mkString(" "))

        val Context = Context_num(q(i),2)
        for(j <- 0 until Context.size){

          val y = forward(Context(j))
          val t = onehot(q(i)(j+1),xn)
          backward(y-t)
          Loss += crossEntropy(t,y)
          all+=1
          if(t.indexOf(t.max) == y.indexOf(y.max)){
            cm+=1
          }
        }

      }
      if((ep % 200 == 0 && ep != 0) || (ep == epoch -1 )){
        val p =save_Distributed_Representation(cm.toString)
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

      if(ep % 500 == 0 && ep == epoch -1 ){
        savetxt_String(lossList,"perplexity"+timer.date(),"txt")
        Win.save("biasdata/cbow.win.txt")
        Wout.save("biasdata/cbow.wout.txt")

      }

    }

    savetxt_String(lossList,"perplexity"+timer.date(),"txt")
    finalpathname
  }

  def train_all(path:String,epoch:Int)={
    val words = all_mk().toList
    val q = question_load_all2(path,words)
    var lossList = List[String]()
    var Loss = 0d;var cm = 0f;var all =0f

    for (ep <- 0 until epoch){
      timer.timestart()
      for(i <- 0 until q.size){
        val Context = Context_num(q(i),Windowsize)
        for(j <- 0 until Context.size){

          val y = forward(Context(j))
          val t = onehot(q(i)(j+1),xn)
          backward(y-t)
          Loss += crossEntropy(t,y)
          all+=1
          if(t.indexOf(t.max) ==y.indexOf(y.max)){
            cm+=1
          }
        }

      }
      if((ep % 200 == 0 && ep != 0) || (ep == epoch -1 )){
        val p =save_Distributed_Representation()
        Utilty.call_py.TsneAndPcaPy(p,xn.toString,hidden.toString,title+"_"+xn+"x"+hidden+"_"+ep)
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
        savetxt_String(lossList,"perplexity"+timer.date(),"txt")
      }

    }

    savetxt_String(lossList,"perplexity"+timer.date(),"txt")

  }

  def train_one(path:String,epoch:Int)={
    var finalpathname=""
    val words = vecnormal.toList
    val q = question_load_all2_2(path,words)
    var lossList = List[String]()
    var Loss = 0d;var cm = 0f;var all = 0f
    //  Wout.load("biasdata/cbow.wout.txt")
    for (ep <- 0 until epoch){
      timer.timestart()
      for(i <- 0 until q.size){
        //  println("q(i) => "+q(i).mkString(" "))

        val Context = Context_num(q(i),Windowsize)
        for(j <- 0 until Context.size){

          val y = forward(Context(j))
          val t = onehot(q(i)(j+1),xn)
          backward(y-t)
          Loss += crossEntropy(t,y)
          all+=1
          if(t.indexOf(t.max) == y.indexOf(y.max)){
            cm+=1
          }
        }

      }
      if((ep % 200 == 0 && ep != 0) || (ep == epoch -1 )){
        val p =save_Distributed_Representation(cm.toString)
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

      if(ep % 500 == 0 && ep == epoch -1 ){
        savetxt_String(lossList,"perplexity"+timer.date(),"txt")
        Win.save("biasdata/cbow.win.txt")
        Wout.save("biasdata/cbow.wout.txt")

      }

    }

    savetxt_String(lossList,"perplexity"+timer.date(),"txt")
    finalpathname
  }


}
