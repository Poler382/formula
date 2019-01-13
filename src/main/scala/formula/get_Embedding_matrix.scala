package formula
object Word2vec{
  import Layer._
  import formula.protcol._
  import Utilty.ML._
  import Utilty.call_py._
  import Utilty.RichArray._
  import formula.Utilty_formula._
  import word2vec._
  val eps = 1e-5d

  def main(args: Array[String]): Unit = {
    val q = question_load3(collection_path).map(string2onehot(_))
    val SkipGramModel = new SkipGram("formula",q(0)(0).size,10)
    SkipGramModel.trains(q,2000)

    val CBOWModel = new CBOW("formula",q(0)(0).size,10)
    CBOWModel.trains(q,2000)

  }

  def t_SNE_vector()={
    val q = question_load3(collection_path).map(string2onehot(_))
    val SkipGramModel = new SkipGram("formula",q(0)(0).size,10)
    val CBOWModel = new CBOW("formula",q(0)(0).size,10)

    SkipGramModel.load_Enbedding()
    CBOWModel.load_Enbedding()

    val pathName1 = "SkipGram_.txt"
    val writer1 =  new java.io.PrintWriter(pathName1)

    val pathName2 = "CBOW_.txt"
    val writer2 =  new java.io.PrintWriter(pathName2)


    for(formula <- q ){
      var resultSkip = List[Float]()
      var resultCBOW = List[Float]()

      for(i <- 0 until formula.size){
        resultSkip = resultSkip ++ SkipGramModel.Win.forward(formula(i)).toList
        resultCBOW = resultCBOW ++ CBOWModel.Win.forward(formula(i)).toList
      }
      writer1.write(resultSkip.mkString(",")+"\n")
      writer2.write(resultCBOW.mkString(",")+"\n")
    }
    writer1.close
    writer2.close


  }







  def CBOWlearn(epoch:Int,Windowsize:Int = 1)={
    val sf = new SoftMax()
    val target = Array(vec2(0),vec2(1),vec2(2),vec2(3),vec2(4),vec2(1),vec2(5))//you say goodbye and I say hello
    val number = Array(0,1,2,3,4,1,5)
    val CBOWModel = new CBOW("yousay",vec2(0).size,5,Windowsize*2,Windowsize)
    val Contexts = CBOWModel.Context_num(number,Windowsize)
    var lossList = List[String]()
    for(i <- 0 until epoch){
      var Loss = 0d
      var cm = 0f
      var all =0f
      var time = System.currentTimeMillis()
      for(j <- 0 until Contexts.size){

        val y = CBOWModel.forward(Contexts(j))
        if(target(j+1).indexOf(target(j+1).max) ==y.indexOf(y.max)){
          cm+=1
        }
        Loss += CBOWModel.crossEntropy(target(j+1),y)
        all+=1
        CBOWModel.backward(y-target(j+1))
        CBOWModel.update()
      }

      println(
        "epoch : "+i,
        " count: "+cm/all,
        " Loss : "+ math.exp(-Loss),
        "crossEntropy : "+ -Loss,
        " time : "+(System.currentTimeMillis()-time)
      )
      lossList ::= (math.exp(-Loss/all)).toString
    }

  }

  def skiplearn(epoch:Int,Windowsize:Int = 1)={
    val sf = new SoftMax()
    val target = Array(vec2(0),vec2(1),vec2(2),vec2(3),vec2(4),vec2(1),vec2(5))//you say goodbye and I say hello
    val number = Array(0,1,2,3,4,1,5)
    val SkipGramModel = new SkipGram("yousay",vec2(0).size,5,2,1)
    val Contexts = SkipGramModel.Context_num(number,1)
    var lossList = List[String]()
    for(i <- 0 until epoch){
      var Loss = 0d
      var cm = 0f
      var all =0f
      var time = System.currentTimeMillis()
      for(j <- 0 until target.size){

        val y = SkipGramModel.forward(target(j),Contexts(j))
        val c = Contexts(j).map(SkipGramModel.onehot(_,vec2(0).size))

        val ys = y.map(a => a.indexOf(a.max))
        val cs = Contexts(j)

        for(k <- 0 until ys.size){
          if(ys(k) == cs(k)){
            cm +=  1f
          }
          all += 1f
        }
        val loss = SkipGramModel.crossEntropy(c.flatten,y.flatten)
        Loss += loss
        SkipGramModel.backward(y - c)

        SkipGramModel.update()

      }

      println(
        "epoch : "+i,
        " count: "+cm/all,
        " Loss : "+ math.exp(-Loss),
        "crossEntropy : "+ -Loss,
        " time : "+(System.currentTimeMillis()-time)
      )
      lossList ::= (math.exp(-Loss/all)).toString

    }
    savetxt_String(lossList,"parprexcy","/Users/yuya/Programing/sbt/formula")

    plotPy("/Users/yuya/Programing/sbt/formula/parprexcy.txt")
  }

}
