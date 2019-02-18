package formula
class vercter_management(num: Int){//何このベクトルを保存したいか
  var vecter = Array.ofDim[(Float,String)](num) //(acc,ListをmkStringにしたもの)

  //必ず宣言したら呼ぶ
  def init()={
    for(i <- 0 until num){
      vecter(i) = (0f,"")
    }
  }

  def setVecter(index:Int,acc:Float,v:Array[Float])={
    if(vecter(index)._1 <= acc){
      //  sys.process.Process("say Stock").run
      vecter(index) = (acc,v.mkString(",")+"\n")
    }
  }

  def getVec() = vecter.map(_._2).toList

}


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
    val layernum = args(3).toInt
    val select = args(4)
    val take = 100
    if(select == "normal"){
      normallearnver2(num,in,hidden,layernum,take)
    }else if(select == "Skip"){
      skiplearn(num,in,hidden,layernum,take)
    }
    else if(select == "Bi"){
      Bi_directional_slearn(num,in,hidden,layernum,take)
    }else if(select == "all"){
      normallearn2(num,22,200,1,take)
      normallearn2(num,22,200,4,take)
      normallearn2(num,22,400,1,take)
      normallearn2(num,22,400,4,take)

      skiplearn(num,22,200,1,take)
      skiplearn(num,22,200,4,take)
      skiplearn(num,22,400,1,take)
      skiplearn(num,22,400,4,take)



      Bi_directional_slearn(num,in,hidden,layernum,take)
    }else{
      println("input nom patern")
    }


  }
  var pre_acc = 0f
  def Stack_entry(preh:String,h:String,acc:Float)={
    if(acc >= pre_acc){
      h
    }else{
      preh
    }
  }

  def index(i:Int,max:Int)={
    if(i < 0)  max else i
  }

  def normallearnver2(num:Int,in:Int,hidden:Int,layernum:Int,take:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)

    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(take)
    val test = question_load_all2_2(path,words).drop(take).take(take)
    val VM = new vercter_management(Exercises.size)
    VM.init()

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    var XentropyList = List[Float]()
    var AccList = List[Float]()
    var saveVecter = ""
    var testVecter = ""


    for(e <- 0 until Exercises.size){
      timer.timestart
      val Encoder = new Seq2Seq.Encoder(in,hidden,hidden,layernum)
      val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)

      var h = Array.ofDim[Float](hidden)
      for(epoch <- 0 until num){
        var acc = 0f
        var partacc = 0f
        var clossentropy = 0f

        //println("\nencoder forward")

        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse


        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }

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
          //  Decoder.backward(ys(i)-onehots(i))
          Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})
          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        for(i <- onehots.size-1 to 0 by -1){
          Encoder.backward(new Array[Float](hidden))
        }

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}



        if(inputExercises== outputExercises){
          acc +=1f
          //  sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }
        VM.setVecter(e,partacc,h)
        Encoder.update
        Decoder.update
        println(
          s"epoch ${epoch} Xentropy ${clossentropy} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises(e).size.toFloat} part accrate: ${partacc/Exercises(e).size.toFloat}"
        )
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }
      }
      saveVecter += h.mkString(",")+","

      if(e <= test.size){
        val onehots = test(e).map(a => onehot(a,words.size)).reverse

        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }
        testVecter += h.mkString(",")+","
        Encoder.reset
      }

    }

    val pathName = "txt/new/"+"E2D_EV_final_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    writer.write(saveVecter)
    writer.close()
    println("success ")


    val pathName2 = "txt/new/"+"E2D_EV_final_test_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer2 =  new java.io.PrintWriter(pathName2)
    writer2.write(testVecter)
    writer2.close()
    println("success ")

  }


  def normallearn(num:Int,in:Int,hidden:Int,layernum:Int,take:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)

    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(take)
    val test = question_load_all2_2(path,words).drop(take).take(10)
    val VM = new vercter_management(Exercises.size)
    VM.init()

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    var XentropyList = List[Float]()
    var AccList = List[Float]()
    var saveVecter = ""
    var testVecter = ""
    for(e <- 0 until Exercises.size){
      timer.timestart
      val Encoder = new Seq2Seq.Encoder(in,hidden,hidden,layernum)
      val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)

      var h = Array.ofDim[Float](hidden)
      for(epoch <- 0 until num){
        var acc = 0f
        var partacc = 0f
        var clossentropy = 0f

        //println("\nencoder forward")

        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse


        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }

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

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}



        if(inputExercises== outputExercises){
          acc +=1f
          //  sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }
        VM.setVecter(e,partacc,h)
        Encoder.update
        Decoder.update
        println(
          s"epoch ${epoch} Xentropy ${clossentropy} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises(e).size.toFloat} part accrate: ${partacc/Exercises(e).size.toFloat}"
        )
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }

      }

      saveVecter += h.mkString(",")+","

      //test
      val onehots = test(e).map(a => onehot(a,words.size)).reverse

      for(i <- 0 until onehots.size){
        h = Encoder.forward(onehots(i))
      }
      testVecter += h.mkString(",")+","
      Encoder.reset


    }

    val pathName = "txt/new/"+"E2D_EV_final_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    writer.write(saveVecter)
    writer.close()
    println("success ")


    val pathName2 = "txt/new/"+"E2D_EV_final_test_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer2 =  new java.io.PrintWriter(pathName2)
    writer2.write(testVecter)
    writer2.close()
    println("success ")

  }

  def skiplearn(num:Int,in:Int,hidden:Int,layernum:Int,take:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)
    //
    //出てきた物をベクトル化  var words = CBOWModel.load_word_count(path)
    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(take)
    val test = question_load_all2_2(path,words).drop(take).take(50)
    val VM = new vercter_management(Exercises.size)
    VM.init()
    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    var XentropyList = List[Float]()
    var AccList = List[Float]()
    var saveVecter = ""
    var testVecter = ""

    for(e <- 0 until Exercises.size){
      val Encoder = new Seq2Seq.SkipConnectionEncoder(in,hidden,hidden,layernum)
      val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)
      var h = Array.ofDim[Float](hidden)

      for(epoch <- 0 until num){
        timer.timestart
        var acc = 0f
        var partacc = 0f
        var clossentropy = 0f

        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse

        for(i <- 0 until onehots.size){
          h = Encoder.forward(onehots(i))
        }

        formula_vecter.push(h.mkString(",")+"\n")
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
          //Decoder.backward(ys(i)-onehots(i))
          Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})

          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        for(i <- onehots.size-1 to 0 by -1){
          Encoder.backward(new Array[Float](hidden))
        }


        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}

        if(inputExercises== outputExercises){
          acc +=1f
          //    sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }
        VM.setVecter(e,partacc,h)

        Encoder.update
        Decoder.update
        println(
          s"epoch ${epoch} Xentropy ${clossentropy} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises(e).size.toFloat} part accrate: ${partacc/Exercises(e).size.toFloat}"
        )
      }
      saveVecter += h.mkString(",") +","


      //test
      var h2 = Array.ofDim[Float](hidden)
      val onehots = test(e).map(a => onehot(a,words.size)).reverse

      for(i <- 0 until onehots.size){
        h2 = Encoder.forward(onehots(i))
      }
      testVecter += h2.mkString(",") +","


    }

    val pathName = "txt/new/"+"SkipConnect_Skip_final_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    writer.write(saveVecter)
    writer.close()
    println("success ")

    val pathName2 = "txt/new/"+"SkipConnect_Skip_test_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer2 =  new java.io.PrintWriter(pathName2)
    writer2.write(testVecter)
    writer.close()
    println("success ")

  }

  def Bi_directional_slearn(num:Int,in:Int,hidden:Int,layernum:Int,take:Int)={
    //val (in,hidden) = get_in_hidden(Embedding_path)
    println(in,hidden)
    val path = "collection2.txt"
    val formula_vecter = new Stack[String]()
    val CBOWModel = new word2vec.CBOW("formula",in,hidden)
    //    val Encoder = new Seq2Seq.BiDirectionalEncoder(in,hidden,hidden,layernum )
    //    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)
    //
    //出てきた物をベクトル化  var words = CBOWModel.load_word_count(path)

    var words = vecnormal.toList
    val Exercises = question_load_all2_2(path,words).take(take)
    val test = question_load_all2_2(path,words).drop(take).take(50)
    val VM = new vercter_management(Exercises.size)
    VM.init()

    //  Encoder.Embedding_load(Embedding_path)
    //  Decoder.Embedding_load(Embedding_path)

    //  println(s"words ${words.size}")
    var XentropyList = List[Float]()
    var AccList = List[Float]()
    var saveVecter = ""
    var testVecter = ""

    for(e <- 0 until Exercises.size){
      val Encoder = new Seq2Seq.BiDirectionalEncoder(in,hidden,hidden,layernum )
      val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)

      var hs = Array[Float](hidden)
      for(epoch <- 0 until num){
        timer.timestart
        var acc = 0f
        var partacc = 0f
        var clossentropy = 0f


        //println("\nencoder forward")
        val onehots = Exercises(e).map(a => onehot(a,words.size)).reverse
        hs = Encoder.forward(onehots)
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
        //  Decoder.backward(ys(i)-onehots(i))
          Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})

          clossentropy += crossEntropy(onehots(i),ys(i))
        }

        Encoder.setBP_d(Decoder.Get_BP_d())

        Encoder.backward(Array.ofDim[Float](Exercises(e).size,hidden))

        var inputExercises = ""
        Exercises(e).map{a => inputExercises += words(index(a,words.size-1))}
        var outputExercises = ""
        ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}

        if(inputExercises== outputExercises){
          acc +=1f
          //    sys.process.Process("say Hit").run
        }

        Range(0,inputExercises.size).map{i => if(inputExercises(i) == outputExercises(i)) partacc +=1f }

        VM.setVecter(e,partacc,hs)
        Encoder.update
        Decoder.update
        println(
          s"epoch ${epoch} Xentropy ${clossentropy} perprexy: ${math.exp(clossentropy)} accrate: ${acc/Exercises(e).size.toFloat} part accrate: ${partacc/Exercises(e).size.toFloat}"
        )
        if(epoch % 5  == 0){
          println("  input: " + inputExercises)
          println("predict: " + outputExercises)
        }
      }

      saveVecter += hs.mkString(",") +","

      //test
      var hs2 = Array[Float](hidden)

      val onehots = test(e).map(a => onehot(a,words.size)).reverse
      hs2 = Encoder.forward(onehots)
      testVecter += hs2.mkString(",") +","

    }

    val pathName = "txt/new/"+"BiDirection_Bi_final_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer =  new java.io.PrintWriter(pathName)
    writer.write(saveVecter)
    writer.close()
    println("success ")

    val pathName2 = "txt/new/"+"BiDirection_Bi_test_"+in+"x"+hidden+"_"+layernum+"_"+timer.date()+".txt"
    val writer2 =  new java.io.PrintWriter(pathName2)
    writer2.write(testVecter)
    writer2.close()
    println("success ")


  }


  def crossEntropy(a:Array[Float],b:Array[Float])={
    var loss = 0d
    for(i <- 0 until a.size){
      loss += a(i) * math.log(b(i)+0.0001f)
    }
    loss.toFloat / a.size
  }


}
