package formula

object  OX_Discriminater {
  import formula.protcol._
  import Layer._
  import Activation._
  import Utilty.RichCal._
  import Utilty.RichArray._
  import formula.Utilty_formula._
  import Utilty.Image
  import Utilty.ML._
  val timer= new Utilty.timer()
  val Image = new Image()


  def main(args: Array[String]): Unit = {

    val mode = args(0)
    if(mode=="train"){
      // mode epoch networkmode
      train(args)
    }else if(mode=="test"){
      // mode inputfile networkmode
      val testresult = test(args)

    }else {
      println("other input ")
    }
  }

  def mkblack={
    val fn =scala.sys.process.Process("ls testdate").lineStream.toArray
    for(filename <- fn){
      val fnsplit = filename.dropRight(4).split("_")
      val name = fnsplit(0)
      val printnumber = fnsplit(1).toInt
      Image.write("black/"+name+"_"+printnumber+"black.png",formula.AnsCutter.make_black_paper("testdate/"+filename))
    }

  }
  def select_Net(mode:String = "OX_defo",H:Int=90,W:Int=240) = {
    var h = H
    var w = W
    var Fs = 2
    val g = mode match {
      case "OX_defo" =>{
        println(h,w)

        val a = new Convolution_Matver(Fs,3,h,w,16)
        val b = new Pooling(2,16,h-Fs+1,w-Fs+1)
        val c = new ReLU()

        h = (h-Fs+1)/2
        w = (w-Fs+1)/2
        println(h,w)

        val a1 = new Convolution_Matver(Fs,16,h,w,32)
        val b1 = new Pooling(2,32,h-Fs+1,w-Fs+1)
        val c1 = new ReLU()

        h = (h-Fs+1)/2
        w = (w-Fs+1)/2
        println(h,w)
        val a2 = new Convolution_Matver(Fs,32,h,w,64)
        val b2 = new Pooling(2,64,h-Fs+1,w-Fs+1)
        val c2 = new ReLU()

        h = (h-Fs+1)/2
        w = (w-Fs+1)/2
        println(h,w)
        val a3 = new Convolution_Matver(Fs,64,h,w,128)
        val b3 = new Pooling(2,128,h-Fs+1,w-Fs+1)
        val c3 = new ReLU()

        h = (h-Fs+1)/2
        w = (w-Fs+1)/2
        println(h,w)
        val af1 = new Affine(h*w*128,100)
        val ac  = new ReLU()
        val af2 = new Affine(100,2)
        val ac2  = new SoftMax()

        List(a,b,c,a1,b1,c1,a2,b2,c2,a3,b3,c3,af1,ac,af2,ac2)
      }


    }

    g
  }

  def savecsv_String(list:List[String],name:String,pagenum:Int)={
    val pathName = "formula_data.csv"

    var oldcsv = scala.io.Source.fromFile("formula_data.csv").getLines.toArray
    val record_name = oldcsv.map(_.split(",")).drop(2).filter(0 != _.size).map(_.head).filter(_.size != 0).toArray
    val ans = list.mkString

    if(record_name.contains(name)){
      println(s"${name} ${pagenum} 追加登録")//記録済み
      var writting_csv = scala.io.Source.fromFile("formula_data.csv").getLines.map(_.split(",")).toArray
      //  Range(0,ws.size-1).map(i => if(i < lines.size){ws(i) = lines(i)}else{ws(i) = " "})
      var linenum = record_name.indexOf(name)
      writting_csv(linenum+2)(pagenum) =  ans
      val w = writting_csv.map(_.mkString(",")).mkString("\n")
      val writer =  new java.io.PrintWriter(pathName)
      writer.write(w)
      writer.close()
      println("success csv")

    }else{
      //新規登録
      println(s"${name} ${pagenum} 新規登録")
      var ws = Array.ofDim[String](50)
      for(i <- 0 until ws.size){ws(i) = " "}
      ws(0) = name
      ws(pagenum) =  ans
      var writting_csv = scala.io.Source.fromFile("formula_data.csv").getLines.take(record_name.size+2).map(_.split(",")).toArray
      val w = writting_csv.map(_.mkString(",")).mkString("\n")
      val writer =  new java.io.PrintWriter(pathName)
      writer.write(w+"\n")
      writer.write(ws.mkString(",")+"\n")
      writer.close()
      println("success csv")
    }


    true
  }

  def test(args:Array[String])={

    val epoch = args(1).toInt
    val fn =scala.util.Random.shuffle(scala.sys.process.Process("ls png/test").lineStream.toList)
    val tmp = Image.read("png/test/"+fn(0)).toArray
    val h = tmp.size
    val w = tmp(0).size

  }

  def train(args: Array[String]): Unit = {
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)

    //args(0) is mode.  args(1) is epoch
    val networkmode = args(2)

    val epoch = args(1).toInt
    val fn =scala.util.Random.shuffle(scala.sys.process.Process("ls png/learning").lineStream.toList)
    val tmp = Image.read("png/learning/"+fn(0)).toArray
    val h = tmp.size
    val w = tmp(0).size

    val csv = scala.io.Source.fromFile("formula_data.csv").getLines.toArray
    val blackimg2 = fn.map(a => Image.RGBto3DArray(flat_and_float(Image.read("png/learning/"+a)),h,w).flatten).toArray

    val fn_test = scala.util.Random.shuffle(scala.sys.process.Process("ls png/test").lineStream.toList)
    val test_blackimg = fn_test.map(a => Image.RGBto3DArray(flat_and_float(Image.read("png/test/"+a)),h,w).flatten).toArray

    var train_LOSSLIST = new Utilty.Stack[Float]()
    var train_ACCLIST  = new Utilty.Stack[Float]()
    var test_LOSSLIST  = new Utilty.Stack[Float]()
    var test_ACCLIST   = new Utilty.Stack[Float]()

    val network = select_Net(networkmode,h,w)
    //loads(network,"OX_defo")

    for(ep<- 0 until epoch){
      timer.timestart
      //train
      var Collect_ans = Array.ofDim[Float](fn.size)
      Range(0,fn.size-1).par.map{ i =>
        val filename = fn(i)
        val fnsplit = filename.dropRight(4).split("_")
        val ans = fnsplit(3).toFloat
        Collect_ans(i) = ans
      }
      val (loss,count) = learning(network,Collect_ans,blackimg2,h,w)
      val train_time =timer.timefinish
      //  println(s"train => epoch: ${ep} loss: ${loss} count: ${count} / ${fn.size} Acc: ${count/fn.size} time: ${train_time}")
      var str_ep =String.format("ep:%3s ",ep.toString)
      var str_loss = String.format(" loss: %-10s",loss.toString)
      var str_acc = String.format(" acc: %-10s",(count/fn.size).toString)
      var str_time  = String.format(s" time: %5s",train_time.toString)

      println(s"train => "+str_ep + str_acc + str_loss + str_time)



      //test
      timer.timestart
      var TCollect_ans = Array.ofDim[Float](fn_test.size)
      Range(0,fn_test.size-1).par.map{ i =>
        val filename = fn_test(i)
        val fnsplit = filename.dropRight(4).split("_")
        val ans = fnsplit(3).toFloat
        TCollect_ans(i) = ans
      }
      var (testLOSS,testCOUNT) = forwarder(network,TCollect_ans,test_blackimg,h,w)
      System.gc()
      val test_time =timer.timefinish

    //  println(s"test  => epoch: ${ep} loss: ${testLOSS} count: ${testCOUNT} / ${fn_test.size} Acc: ${testCOUNT/fn_test.size} time: ${test_time}")
      var str_tep =String.format("ep:%3s ",ep.toString)
      var str_tloss = String.format(" loss: %-10s",testLOSS.toString)
      var str_tacc = String.format(" acc: %-10s",(testCOUNT/ fn_test.size).toString)
      var str_ttime  = String.format(s" time: %5s",test_time.toString)

      println(s"test => "+str_tep + str_tacc + str_tloss +str_ttime)



      if(ep % 50 == 0 || ep == epoch-1){
        saves(network,networkmode)
        println("save bias")
        savetxt_Float(train_LOSSLIST.full,"train_OX_LOSS_mid","txt")
        savetxt_Float(train_ACCLIST.full,"train_OX_ACC_mid","txt")
        savetxt_Float(test_LOSSLIST.full,"test_OX_LOSS_mid","txt")
        savetxt_Float(test_ACCLIST.full,"test_OX_ACC_mid","txt")
      }

      train_LOSSLIST.push(loss)
      train_ACCLIST.push(count/fn.size.toFloat)
      test_LOSSLIST.push(testLOSS)
      test_ACCLIST.push(testCOUNT/fn_test.size.toFloat)
    }
    savetxt_Float(train_LOSSLIST.full,"train_OX_LOSS_"+timer.date,"txt")
    savetxt_Float(train_ACCLIST.full,"train_OX_ACC_"+timer.date,"txt")
    savetxt_Float(test_LOSSLIST.full,"test_OX_LOSS_"+timer.date,"txt")
    savetxt_Float(test_ACCLIST.full,"test_OX_ACC_"+timer.date,"txt")

  }



  def learning(Net:List[Layer],target:Array[Float],imgSet:Array[Array[Float]],h:Int,w:Int):(Float,Float)={
    var loss = 0f

    val yList = List[String]()
    var count = 0f

    val y = forwards(Net,imgSet)
    val t = target.map(a => onehot(a.toInt,2))
    //print(target(i),y.indexOf(y.max))
    Range(0 ,y.size-1).par.map{i =>
      if(y(i).indexOf(y(i).max) == t(i).indexOf(t(i).max)) count += 1f
      loss += (y(i)*y(i) - t(i)*t(i)).sum
    }

    backwards(Net.init,(y*y-t*t))

    updates(Net)

    (loss,count)
  }
  //test時に実行用
  def forwarder(Net:List[Layer],target:Array[Float],imgSet:Array[Array[Float]],h:Int,w:Int):(Float,Float)={
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)
    var loss = 0f
    val yList = List[String]()
    var count = 0f

    val y = forwards(Net,imgSet)
    val t = target.map(a => onehot(a.toInt,2))

    Range(0 ,y.size-1).par.map{i =>
      if(y(i).indexOf(y(i).max) == t(i).indexOf(t(i).max)) count += 1f
      loss += (y(i) - t(i)).sum
    }

    resets(Net)
    (loss,count)
  }

  def tester(Net:List[Layer],imgSet:Array[Array[Array[Array[Int]]]])={
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)
    var loss = 0f
    val h = imgSet(0).size
    val w = imgSet(0)(0).size
    var yList = Array.ofDim[String](10)
    var count = 0

    Range(0 ,imgSet.size).par.map{ i =>
      println("tester => "+i)
      val img = Image.RGBto3DArray(flat_and_float(imgSet(i)),h,w).flatten

      var y = forwards(Net,img)
      if(y.indexOf(y.max) == 0) yList(i) = "x"
      else yList(i)= "o"
    }
    yList.reverse.toList
  }

  def tuner(network:List[Layer]){
    //起動時により良い性能を出すために　
    print("　tune start　")
    val epoch = 1
    val fn =scala.util.Random.shuffle(scala.sys.process.Process("ls black").lineStream.toList).take(5)
    val csv = scala.io.Source.fromFile("formula_data.csv").getLines.toArray
    val blackimg2 = fn.map(a => Image.read("black/"+a)).toArray
    for(ep <- 0 until epoch){
      timer.timestart
      var LOSS  = 0f
      var COUNT = 0f
      Range( 0 ,fn.size).map{ i =>
        val filename = fn(i)
        val fnsplit = filename.dropRight(9).split("_")
        val name = fnsplit(0)
        val printnumber = fnsplit(1).toInt - 1
        //    print(s"epoch: ${ep} name: ${name} number: ${printnumber}  ")
        val lines = csv(Range(0,csv.size).map(i => csv(i).contains(name)).toArray.indexOf(true)).split(",").tail
        val Collect_ans = lines(printnumber).toArray.map{a => if(a == 'o') 1f else 0f }.toArray
        val Cutting_ans =formula.AnsCutter.cut(blackimg2(i)).map(formula.AnsCutter.getbox(blackimg2(i),_)).toArray
        //      var (loss,count) = learning(network,Collect_ans,Cutting_ans)
        //    LOSS += loss
        //    COUNT += count
      }
      updates(network)
      println(s"tuner -> epoch: ${ep} loss: ${LOSS} count: ${COUNT} / ${10f*fn.size} Acc: ${COUNT/10f*fn.size} time: ${timer.timefinish}")
    }

  }



}
