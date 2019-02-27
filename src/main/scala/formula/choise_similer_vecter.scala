package formula

object  SimilerVecter {
  import formula.protcol._
  import Layer._
  import Utilty.RichArray._
  import Utilty.ML._
  import Activation._
  import Utilty.Stack
  val timer= new Utilty.timer()
  import formula.Utilty_formula._
  import word2vec._
  var words = vecnormal.toList
  def main(args: Array[String]): Unit = {
    val load_vecter_pathlist = sys.process.Process("ls txt/lastrun/").lineStream.toArray.map(a => "txt/lastrun/"+a).toArray
    for(load_vecter_path <- load_vecter_pathlist){
      println(load_vecter_path)
      val size = load_vecter_path.split("_").toArray.filter(_.contains("22x")).toArray
      //  val input = args(1).replace("""\frac""","f").replace(" ","")
      val lll = load_vecter_path.split("/")
      val mode = lll.last.split("_").head
      val llll=  lll.last.split("_")
      val layernum = llll(llll.map(_.contains("22x")).indexOf(true)+1).toInt
      val vecters = load(load_vecter_path)
      val fff = List("x-3=7","f{4}{3}x-9=-10","0.1x=0.5x-1.6","2(x+5)=6","-4(x+5)=6" )
      for( input <- fff){
        val inputvecter = input.map(a => vecnormal.indexOf(a.toString)).toArray
        println(s"mode: ${mode} inputvectersize: ${size(0)} layer: ${layernum}")
        val h = mode match {
          case "E2D"         =>    normal(inputvecter,50,22,size(0).drop(3).toInt,layernum)
          case "SkipConnect" => skiplearn(inputvecter,50,22,size(0).drop(3).toInt,layernum)
          case "BiDirection" =>   bilearn(inputvecter,50,22,size(0).drop(3).toInt,layernum)
        }

        val line = cal_similer(h,vecters)

        val Exercises_string =  scala.io.Source.fromFile("collection2.txt").getLines.toArray
        println("input -> "+input)
        for(k <- 0 until 5){
          println(s"${Exercises_string(line(k)._1)} > ${line(k)._2} ")
        }
        println("______")
        for(k <- line.size-10 until line.size){
          println(s"${Exercises_string(line(k)._1)} > ${line(k)._2} ")
        }

      }
    }
  }


  def  entry(path : String)={
    val load_vecter_path = path
    val size = load_vecter_path.split("_").toArray.filter(_.contains("22x")).toArray
    val inputvecter = "5x+12=3"
    val mode = load_vecter_path.split("_")(0).split("/").last
    val layernum = 4
    val vecters = load(load_vecter_path)

    val input = inputvecter.map(a => vecnormal.indexOf(a.toString)).toArray

    val h = mode match {
      case "E2D"         =>    normal(input,50,22,size(0).drop(3).toInt,layernum)
      case "SkipConnect" => skiplearn(input,50,22,size(0).drop(3).toInt,layernum)
      case "BiDirection" =>   bilearn(input,50,22,size(0).drop(3).toInt,layernum)
    }

    cal_similer(h,vecters)


  }

  def load(path: String)={
    val size = path.split("_").toArray.filter(_.contains("22x")).toArray
    val raw = scala.io.Source.fromFile(path).getLines.toArray
    raw(0).split(",").map(_.toFloat).grouped(size(0).drop(3).toInt).toArray
  }

  def cal_similer(in : Array[Float],list: Array[Array[Float]])={
    //類似度トップ５
    val a = math.sqrt(in dot in)
    var inner = Map[Int,Float]()
    for(i <- 0 until list.size){
      val b = math.sqrt( list(i) dot list(i) )

      val cos = (list(i) dot in) / (a*b)
      inner += (i -> cos.toFloat)

    }
    val line = inner.toSeq.sortBy(_._2).reverse

    line
  }


  def normal(Exercises:Array[Int],num: Int,in:  Int,hidden: Int,layernum: Int)={
    val Encoder = new Seq2Seq.Encoder(in,hidden,hidden,layernum)
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)

    var h = Array.ofDim[Float](hidden)
    for(epoch <- 0 until num){
      print(".")
      val onehots = Exercises.map(a => onehot(a,words.size)).reverse
      for(i <- 0 until onehots.size){
        h = Encoder.forward(onehots(i))
      }
      //式ベクトルを渡す
      Decoder.Set_pre_h(h)
      //出力貯めるよう
      val ys = Array.ofDim[Float](Exercises.size,in)
      //初めの入力
      ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

      for(i <- 1 until ys.size){
        ys(i) = Decoder.forward(onehots(i))
      }

      for(i <- ys.size-1 to 0 by -1){
        Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})
      }

      Encoder.setBP_d(Decoder.Get_BP_d())

      for(i <- onehots.size-1 to 0 by -1){
        Encoder.backward(new Array[Float](hidden))
      }

      var inputExercises = ""
      Exercises.map{a => inputExercises += words(index(a,words.size-1))}
      var outputExercises = ""
      ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}

      Encoder.update
      Decoder.update
    //  var str_input  = String.format(s" input: %${inputExercises.size}s",inputExercises)
    //  var str_output = String.format(s" predict: %${outputExercises.size}s",outputExercises)
  //    println(str_input+str_output)

    }
    h
  }

  def skiplearn(Exercises:Array[Int],num: Int,in:  Int,hidden: Int,layernum: Int)={


    val Encoder = new Seq2Seq.SkipConnectionEncoder(in,hidden,hidden,layernum)
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)
    var h = Array.ofDim[Float](hidden)

    for(epoch <- 0 until num){
      print(".")

      val onehots = Exercises.map(a => onehot(a,words.size)).reverse

      for(i <- 0 until onehots.size){
        h = Encoder.forward(onehots(i))
      }

      //式ベクトルを渡す
      Decoder.Set_pre_h(h)
      val ys = Array.ofDim[Float](Exercises.size,in)
      //初めの入力
      ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

      for(i <- 1 until ys.size){
        ys(i) = Decoder.forward(onehots(i))
      }

      for(i <- ys.size-1 to 0 by -1){
        Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})
      }

      Encoder.setBP_d(Decoder.Get_BP_d())

      for(i <- onehots.size-1 to 0 by -1){
        Encoder.backward(new Array[Float](hidden))
      }


      var inputExercises = ""
      Exercises.map{a => inputExercises += words(index(a,words.size-1))}
      var outputExercises = ""
      ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}

      Encoder.update
      Decoder.update
    //  var str_input  = String.format(s" input: %${inputExercises.size}s",inputExercises)
//      var str_output = String.format(s" predict: %${outputExercises.size}s",outputExercises)

    }
    h
  }

  def bilearn(Exercises:Array[Int],num: Int,in:  Int,hidden: Int,layernum: Int)={


    val Encoder = new Seq2Seq.BiDirectionalEncoder1(in,hidden,hidden,layernum )
    val Decoder = new Seq2Seq.Decoder(in,hidden,in,1)

    var hs = Array[Float](hidden)
    for(epoch <- 0 until num){
      //println("\nencoder forward")
      print(".")

      val onehots = Exercises.map(a => onehot(a,words.size)).reverse
      hs = Encoder.forward(onehots)
      Decoder.Set_pre_h(hs)
      //出力貯めるよう
      val ys = Array.ofDim[Float](Exercises.size,in)
      //初めの入力
      ys(0) = Decoder.forward(onehot(words.indexOf("<EOS>"),words.size))

      for(i <- 1 until ys.size){
        ys(i) = Decoder.forward(onehots(i))
      }

      for(i <- ys.size-1 to 0 by -1){
        Decoder.backward(onehots(i)*ys(i).map{a => math.log(a+0.0001f).toFloat})
      }

      Encoder.setBP_d(Decoder.Get_BP_d())
      Encoder.backward(Array.ofDim[Float](Exercises.size,hidden))

      var inputExercises = ""
      Exercises.map{a => inputExercises += words(index(a,words.size-1))}
      var outputExercises = ""
      ys.reverse.map{a => outputExercises += words(index(a.indexOf(a.max),words.size-1) )}
      Encoder.update
      Decoder.update
//      var str_input  = String.format(s" input: %${inputExercises.size}s",inputExercises)
//      var str_output = String.format(s" predict: %${outputExercises.size}s",outputExercises)

  //    println(str_input+str_output)


    }
    hs

  }



}
