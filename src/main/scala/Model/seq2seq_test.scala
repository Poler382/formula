

object seq2seq_test{
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._
  val Line = "0123456789+_ "
  type T = Float

  def load()={
    val f = scala.io.Source.fromFile("addition.txt").getLines.toArray
    var siki = Array.ofDim[T](f.size,f(0).size,Line.size)
    for(i <- 0 until f.size;j <- 0 until f(0).size){
      siki(i)(j) = conv(f(i)(j))
    }
    siki
  }
  def conv(c:Char)={
    val x=new Array[T](Line.size)
    x(Line.indexOf(c))=1f
    x
  }
  var inputList   = new Stack[String]()
  var outputList  = new Stack[String]()
  var predictList = new Stack[String]()

  def recode(out:String,in:String,predict:String)={
    inputList.push(in)
    predictList.push(predict)
    outputList.push(out)
  }

  def reset()={
    outputList.reset
    inputList.reset
    predictList.reset
  }

  def fetch(point:Int=0)={
    (inputList.refer(point),predictList.refer(point),outputList.refer(point))
  }

  def accRate()={
    var rate = 0f
    var size = outputList.size
    Range(0,size-1).map{i =>
      if(predictList.refer(i) == outputList.refer(i)) rate+=1f
    }
    reset
    rate/size
  }
  def main(args:Array[String]){
    val epoch = args(0).toInt

    val EncoderUnit = new Seq2Seq.Encoder(13,100,100,1)
    val DecoderUnit = new Seq2Seq.Attention_Decoder(13,100,13,1)

    val fn = "addition.txt"
    val eqations = load()
    //load("/home/share/addition.txt")
    for(ep <- 0 until epoch){
      //println(s"epoch ${ep}")

      for(eq <- eqations.take(1)){
        var output = ""
        var input  = ""
        var predict = ""
        val xs = eq.take(7).reverse
        val ys = eq.drop(7).reverse
        var y = Array.ofDim[Float](100)

        for(i <- 0 until xs.size){
          input += Line(xs(i).indexOf(xs(i).max))
          y = EncoderUnit.forward(xs(i))
          DecoderUnit.Attention_Unit.Encoder_forward(y)
        }
        DecoderUnit.Set_pre_h(y)

        var zs = Array.ofDim[Float](ys.size,Line.size)
        var temp = ys(0)
        for(i <- 0 until ys.size){
          temp =  DecoderUnit.forward(temp)
          zs(i) = temp
          predict += Line(ys(i).indexOf(ys(i).max))
          output += Line(zs(i).indexOf(zs(i).max))
        }

        for(i <- ys.size-1 until 0 by -1){
          println(s"i ${i}")
          DecoderUnit.backward(zs(i)-ys(i) )
        }

        EncoderUnit.setBP_d(DecoderUnit.Get_BP_d())

        for(i <- xs.size-1 to 0 by -1){
          EncoderUnit.backward(new Array[Float](100))
        }


        recode(output,input,predict)
        EncoderUnit.update()
        DecoderUnit.update()
      }
      if(ep % 10 == 0){
        val (input,predict,output) = fetch()
        println(s"epoch ${ep} accrate ${accRate()}")
        println(s"Q  -> |${input.reverse}|  |${input.size}")
        println(s"A  -> |${predict.reverse}|    |${predict.size}")
        println(s"P  -> |${output.reverse}|    |${output.size}")

      }
    }

  }

  def main_peeky(args:Array[String]){
    val epoch = args(0).toInt

    val EncoderUnit       = new Seq2Seq.Encoder(13,100,100,2)
    val Peeky_DecoderUnit = new Seq2Seq.Peeky_Decoder(13,100,100,13,2)

    val fn = "addition.txt"
    val eqations = load()
    //load("/home/share/addition.txt")
    for(ep <- 0 until epoch){

      //println(s"epoch ${ep}")
      for(eq <- eqations.take(1)){
        val xs = eq.take(7).reverse
        val ys = eq.drop(7).reverse
        var y = Array.ofDim[Float](100)
        var output = ""
        var input  = ""
        var predict = ""
        for(i <- 0 until xs.size){
          input += Line(xs(i).indexOf(xs(i).max))
          y = EncoderUnit.forward(xs(i))
        }
        Peeky_DecoderUnit.Set_pre_h(y)

        var zs = Array.ofDim[Float](ys.size,Line.size)
        var temp = ys(0)
        for(i <- 0 until ys.size){
          //print(i+" ")
          temp =  Peeky_DecoderUnit.forward(temp)
          zs(i) = temp
          predict += Line(ys(i).indexOf(ys(i).max))
          output += Line(zs(i).indexOf(zs(i).max))
        }
        //  println("&")
        for(i <- ys.size-1 until 0 by -1){
          //     print(s" j -> ${i}")
          Peeky_DecoderUnit.backward( zs(i)-ys(i) )
        }

        EncoderUnit.setBP_d(Peeky_DecoderUnit.Get_BP_d())

        for(i <- xs.size-1 to 0 by -1){
          EncoderUnit.backward(Peeky_DecoderUnit.Get_peeky_encoder_h)
        }

        recode(output,input,predict)
        EncoderUnit.update()
        Peeky_DecoderUnit.update()
      }
      if(ep % 10 == 0){
        val (input,predict,output) = fetch()
        println(s"epoch ${ep} accrate ${accRate()}")
        println(s"Q  -> |${input.reverse}|  |${input.size}")
        println(s"A  -> |${predict.reverse}|    |${predict.size}")
        println(s"P  -> |${output.reverse}|    |${output.size}")
      }
    }

  }

  def main_encoder_decoder(args:Array[String]){
    val epoch = args(0).toInt

    val EncoderUnit = new Seq2Seq.Encoder(13,100,100,3)

    val da1   = new Linear(13,100)
    val drop3 = new Dropout(0.5f)
    val dl1   = new LSTM2(100,100)
    val drop4 = new Dropout(0.5f)
    val da2   = new Linear(100,13)
    val ds1   = new SoftMax()
    val dec   = List(da1,drop3,dl1,drop4,da2,ds1)

    val fn = "addition.txt"
    val eqations = load()
    //load("/home/share/addition.txt")
    for(ep <- 0 until epoch){
      //println(s"epoch ${ep}")

      for(eq <- eqations){
        var output = ""
        var input  = ""
        var predict = ""
        val xs = eq.take(7).reverse
        val ys = eq.drop(7).reverse
        var y = Array.ofDim[Float](100)

        for(i <- 0 until xs.size){
          input += Line(xs(i).indexOf(xs(i).max))

          //  xs(i).foreach{a => print(a+" ")}
          y = EncoderUnit.forward(xs(i))

          //y.foreach{a => print(a+" ")}
          //println("¥")
        }
        dl1.SETpre_h(y)

        var zs = Array.ofDim[Float](ys.size,Line.size)
        var temp = ys(0)
        for(i <- 0 until ys.size){
          //  print(i+" ")
          temp =  forwards(dec,ys(i))
          zs(i) = temp
          predict += Line(ys(i).indexOf(ys(i).max))
          //    print(Line(zs(i).indexOf(zs(i).max)))
          //  zs(i).foreach{a => print(a+" ")}
          output += Line(zs(i).indexOf(zs(i).max))

        }

        for(i <- ys.size-1 until 0 by -1){
          backwards(dec,zs(i)-ys(i) )
        }
        EncoderUnit.setBP_d(dl1.BP_d.head)

        for(i <- xs.size-1 to 0 by -1){

          EncoderUnit.backward(new Array[Float](100))
        }


        recode(output,input,predict)
        EncoderUnit.update()
        updates(dec)
      }
      if(ep % 10 == 0){
        val (input,predict,output) = fetch()
        println(s"epoch ${ep} accrate ${accRate()}")
        println(s"Q  -> |${input.reverse}|  |${input.size}")
        println(s"A  -> |${predict.reverse}|    |${predict.size}")
        println(s"P  -> |${output.reverse}|    |${output.size}")

      }
    }

  }


  def main_encoderver(args:Array[String]){
    val epoch = args(0).toInt

    val ea1   = new Linear(13,100)
    val drop1 = new Dropout(0.5f)
    val el1   = new LSTM2(100,100)
    val drop2 = new Dropout(0.5f)
    val enc   = List(ea1,drop1,el1,drop2)

    val da1   = new Linear(13,100)
    val drop3 = new Dropout(0.5f)
    val dl1   = new LSTM2(100,100)
    val drop4 = new Dropout(0.5f)
    val da2   = new Linear(100,13)
    val ds1   = new SoftMax()
    val dec   = List(da1,drop3,dl1,drop4,da2,ds1)

    val fn = "addition.txt"
    val eqations = load()
    //load("/home/share/addition.txt")
    for(ep <- 0 until epoch){
      //println(s"epoch ${ep}")
      for(eq <- eqations.take(1)){
        val xs = eq.take(7).reverse
        val ys = eq.drop(7).reverse
        var y = Array.ofDim[Float](100)
        var output = ""
        var input  = ""
        var predict = ""

        for(i <- 0 until xs.size){
          input += Line(xs(i).indexOf(xs(i).max))
          y = forwards(enc,xs(i))
        }
        dl1.SETpre_h(y)

        var zs = Array.ofDim[Float](ys.size,Line.size)
        var temp = ys(0)
        for(i <- 0 until ys.size){

          temp =  forwards(dec,ys(i))
          zs(i) = temp
          predict += Line(ys(i).indexOf(ys(i).max))
          //    print(Line(zs(i).indexOf(zs(i).max)))
          output += Line(zs(i).indexOf(zs(i).max))

        }


        for(i <- ys.size-1 until 0 by -1){
          //  println(i)
          //  println(s"zs ${Line(zs(i).indexOf(zs(i).max))}")
          //  println(s"ys ${Line(ys(i).indexOf(ys(i).max))}")
          backwards(dec,zs(i)-ys(i) )
        }
        el1.BP_d.pop()
        el1.BP_d.push(dl1.BP_d.head)

        for(i <- xs.size-1 to 0 by -1){
          backwards(enc,new Array[Float](100))
        }


        recode(output,input,predict)
        updates(enc)
        updates(dec)
      }
      if(ep % 10 == 0){
        val (input,predict,output) = fetch()
        println(s"epoch ${ep} accrate ${accRate()}")
        println(s"Q  -> |${input.reverse}|  |${input.size}")
        println(s"A  -> |${predict.reverse}|    |${predict.size}")
        println(s"P  -> |${output.reverse}|    |${output.size}")

      }
    }

  }

}
