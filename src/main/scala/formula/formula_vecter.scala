package formula

object FormulaVecter{
  import formula.protcol._
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Activation._
  import Utilty.Stack
  import Utilty.timer
  import Utilty_formula._
  import word2vec._
  def learn(num:Int)={
    val in = 10
    val out = 64
    val q = question_load3(collection_path).map(string2onehot(_))
    val vecsize = q(0)(0).size
    val SkipGramModel = new SkipGram("formula",q(0)(0).size,in)
    val CBOWModel = new CBOW("formula",q(0)(0).size,in)

    SkipGramModel.load_Enbedding()
    CBOWModel.load_Enbedding()

    val lstm = new LSTM2(in,out)
    val af = new Affine(out,vecsize)
    val sf = new SoftMax()

    val lstm2 = new LSTM2(in,out)
    val af2 = new Affine(out,vecsize)
    val sf2 = new Sigmoid()

    val Encoder = List(lstm)
    val Decoder = List(lstm2,af2,sf2)

    for(epoch <- 0 until num){
      println(s"epoch ${epoch}")
      timer.start
      var loss = 0f
      for (m <- 0 until q.size/10){
        val formula = q(m)
        var resultSkip = List[Float]()
        var resultCBOW = List[Float]()

        for(i <- 0 until formula.size){
          val Embedding = SkipGramModel.Win.forward(formula(i))
          // CBOWModel.Win.forward(formula(i)).toList
          val output_Encoder = forwards(Encoder,Embedding)
          lstm2.SETpre_h(output_Encoder)//最終出力のこの値を式ベクトルとして使う？
        }

        var input = Array.ofDim[Float](vecsize)
        var Output = ""
        var target = ""
        val Stack = new Stack[Array[Float]]()
        for (i <- 0 until formula.size){
          val Embedding = SkipGramModel.Win.forward(input)
          input = forwards(Decoder,Embedding)
          Stack.push(input)
          target = target ++ vec(formula(i).indexOf(formula(i).max))
          Output = Output ++ vec(input.indexOf(input.max))
        }
        if(m%80==0){
          println("target -> "+target)
          println("Output -> "+Output)
        }
        val Y_decoder = Stack.full.reverse.toArray
        var dh = Array.ofDim[Float](out)

        for(i <- formula.size-1 until 0 by -1){
          backwards(Decoder,formula(i)-Y_decoder(i))
          loss += CBOWModel.crossEntropy(formula(i),Y_decoder(i))

        }

        lstm.BP_d.pop()
        lstm.BP_d.push(lstm2.BP_d.head)

        for(i <- formula.size-1 until 0 by -1){
          backwards(Encoder,dh)
        }

      }
      updates(Encoder)
      updates(Decoder)
      timer.finish
      println(s"loss ${loss}")
    }
  }

  def main(args: Array[String]): Unit = {
    learn(args(0).toInt)
  }



}
