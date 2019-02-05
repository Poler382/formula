package formula

object FormulaVecter{
  import formula.protcol._
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Activation._
  import Utilty.Stack
  val timer= new Utilty.timer()
  import formula.Utilty_formula._
  import word2vec._

  def main(args: Array[String]): Unit = {
    val num = args(0).toInt
    //  val Embedding_path = args(1)
    val in = args(1).toInt
    val hidden = args(2).toInt
    //    normallearn(num,in,hidden)
    //    skiplearn(num,in,hidden)
    Bi_directional_slearn(num,in,hidden)
  }

  def get_in_hidden(s:String)={
    val xx = s.split("_").filter(_.contains("89x"))
    val y = xx(0).split("x").map(_.toInt)
    (y(0)+2,y(1))
  }
  def index(i:Int,max:Int)={
    if(i < 0)  max else i
  }

  def normallearn(num:Int,in:Int,hidden:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)
    val Encoder = new Seq2Seq.Encoder(in,hidden,hidden,3)
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,3)
    //
    //出てきた物をベクトル化  var words = CBOWModel.load_word_count(path)
    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(3)

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    //  println(s"words ${words.size}")
    var XentropyList = List[Float]()
    var AccList = List[Float]()

    for(epoch <- 0 until num){
      timer.timestart
      var acc = 0f
      var partacc = 0f
      var clossentropy = 0f

      for(e <- 0 until Exercises.size){
        //println("\nencoder forward")
        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse
        var h = Array.ofDim[Float](hidden)

        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }

        formula_vecter.push(h.mkString(",")+"\n")

        //println("\ndecoder forward")
        //式ベクトルを渡す
        Decoder.Set_pre_h(h)

        //出力貯めるよう
        val ys = Array.ofDim[Float](Exercises(e).size,in)
        //初めの入力
        ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

        for(i <- 1 until ys.size){
          ys(i) = Decoder.forward(onehots(i))
        }

        for(i <- ys.size-1 to 0 by -1){
          Decoder.backward(ys(i)-onehots(i))
          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        for(i <- onehots.size-1 to 0 by -1){
          Encoder.backward(new Array[Float](hidden))
        }

        Encoder.update
        Decoder.update

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }


        if(inputExercises== outputExercises){
          acc +=1f
          sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }

      }


      if(epoch % 100 == 0){
        savetxt_Float(XentropyList,"E2D_crossEntropy_"+in+"x"+hidden+"_"+timer.date(),"txt")
        savetxt_Float(AccList,"E2D_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
        Encoder.save("E2D_"+epoch+"_"+in+"x"+hidden)
        Decoder.save("E2D_"+epoch+"_"+in+"x"+hidden)
        savetxt_String(formula_vecter.full,"E2D_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
      }
      formula_vecter.reset
      println(
        s"epoch ${epoch} Xentropy ${clossentropy} time:${timer.timefinish()} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises.size.toFloat} part accrate: ${partacc/Exercises.flatten.size.toFloat}"
      )

      XentropyList ::= clossentropy
      AccList ::= acc/Exercises.size.toFloat
      //println{s"time:${timer.timefinish()}"}

    }
    savetxt_Float(XentropyList,"E2D_crossEntropy_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    savetxt_Float(AccList,"E2D_AccRate_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    Encoder.save("E2D_final_"+in+"x"+hidden)
    Decoder.save("E2D_final_"+in+"x"+hidden)

  }

  def skiplearn(num:Int,in:Int,hidden:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)
    val Encoder = new Seq2Seq.SkipConnectionEncoder(in,hidden,hidden,5)
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)
    //
    //出てきた物をベクトル化  var words = CBOWModel.load_word_count(path)
    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(3)

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    //  println(s"words ${words.size}")
    var XentropyList = List[Float]()
    var AccList = List[Float]()

    for(epoch <- 0 until num){
      timer.timestart
      var acc = 0f
      var partacc = 0f
      var clossentropy = 0f

      for(e <- 0 until Exercises.size){
        //println("\nencoder forward")
        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse
        var h = Array.ofDim[Float](hidden)

        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }

        formula_vecter.push(h.mkString(",")+"\n")

        //println("\ndecoder forward")
        //式ベクトルを渡す
        Decoder.Set_pre_h(h)

        //出力貯めるよう
        val ys = Array.ofDim[Float](Exercises(e).size,in)
        //初めの入力
        ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

        for(i <- 1 until ys.size){
          ys(i) = Decoder.forward(onehots(i))
        }

        for(i <- ys.size-1 to 0 by -1){
          Decoder.backward(ys(i)-onehots(i))
          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        for(i <- onehots.size-1 to 0 by -1){
          Encoder.backward(new Array[Float](hidden))
        }

        Encoder.update
        Decoder.update

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }


        if(inputExercises== outputExercises){
          acc +=1f
          sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }

      }


      if(epoch % 100 == 0){
        savetxt_Float(XentropyList,"E2D_crossEntropy_"+in+"x"+hidden+"_"+timer.date(),"txt")
        savetxt_Float(AccList,"E2D_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
        Encoder.save("E2D_"+epoch+"_"+in+"x"+hidden)
        Decoder.save("E2D_"+epoch+"_"+in+"x"+hidden)
        savetxt_String(formula_vecter.full,"E2D_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
      }
      formula_vecter.reset
      println(
        s"epoch ${epoch} Xentropy ${clossentropy} time:${timer.timefinish()} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises.size.toFloat} part accrate: ${partacc/Exercises.flatten.size.toFloat}"
      )

      XentropyList ::= clossentropy
      AccList ::= acc/Exercises.size.toFloat
      //println{s"time:${timer.timefinish()}"}

    }
    savetxt_Float(XentropyList,"E2D_crossEntropy_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    savetxt_Float(AccList,"E2D_AccRate_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    Encoder.save("E2D_final_"+in+"x"+hidden)
    Decoder.save("E2D_final_"+in+"x"+hidden)

  }

  def Bi_directional_slearn(num:Int,in:Int,hidden:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)
    val Encoder = new Seq2Seq.BiDirectionalEncoder(in,hidden,hidden,1)
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)
    //
    //出てきた物をベクトル化  var words = CBOWModel.load_word_count(path)
    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(3)

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    //  println(s"words ${words.size}")
    var XentropyList = List[Float]()
    var AccList = List[Float]()

    for(epoch <- 0 until num){
      timer.timestart
      var acc = 0f
      var partacc = 0f
      var clossentropy = 0f

      for(e <- 0 until Exercises.size){
        //println("\nencoder forward")
        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse
        var hs = Encoder.forward(onehots)

        formula_vecter.push(hs.tail.mkString(",")+"\n")
        //println("\ndecoder forward")
        //式ベクトルを渡す
        Decoder.Set_pre_h(hs)
        //出力貯めるよう
        val ys = Array.ofDim[Float](Exercises(e).size,in)
        //初めの入力

        ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

        for(i <- 1 until ys.size){
          ys(i) = Decoder.forward(onehots(i))
        }

        for(i <- ys.size-1 to 0 by -1){
          Decoder.backward(ys(i)-onehots(i))
          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        Encoder.backward(Array.ofDim[Float](Exercises(e).size,hidden))

        Encoder.update
        Decoder.update

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }


        if(inputExercises== outputExercises){
          acc +=1f
          sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }

      }


      if(epoch % 100 == 0){
        savetxt_Float(XentropyList,"BiDirection_crossEntropy_"+in+"x"+hidden+"_"+timer.date(),"txt")
        savetxt_Float(AccList,"BiDirection_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
        Encoder.save("BiDirection_"+epoch+"_"+in+"x"+hidden)
        Decoder.save("BiDirection_"+epoch+"_"+in+"x"+hidden)
        savetxt_String(formula_vecter.full,"BiDirection_AccRate_"+in+"x"+hidden+"_"+timer.date(),"txt")
      }
      formula_vecter.reset
      println(
        s"epoch ${epoch} Xentropy ${clossentropy} time:${timer.timefinish()} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises.size.toFloat} part accrate: ${partacc/Exercises.flatten.size.toFloat}"
      )

      XentropyList ::= clossentropy
      AccList ::= acc/Exercises.size.toFloat
      //println{s"time:${timer.timefinish()}"}

    }
    savetxt_Float(XentropyList,"BiDirection_crossEntropy_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    savetxt_Float(AccList,"BiDirection_AccRate_final_"+in+"x"+hidden+"_"+timer.date(),"txt")
    Encoder.save("BiDirection_final_"+in+"x"+hidden)
    Decoder.save("BiDirection_final_"+in+"x"+hidden)

  }


  def crossEntropy(a:Array[Float],b:Array[Float])={
    var loss = 0d
    for(i <- 0 until a.size){
      loss += a(i) * math.log(b(i)+0.0001f)
    }
    loss.toFloat / a.size
  }


}
