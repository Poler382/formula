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
  import Utilty.timer
  val Image = new Image()

  def para()={
    timer.start
    (1 to 10).map { i =>  //parでparallel collection に変換
      println(i)
      Thread.sleep(500) //1回の処理に500ミリ秒かかるとする
    }
    timer.finish
  }


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
  def play()={
    val fn = scala.sys.process.Process("ls testdate").lineStream.toArray

    Range(0,fn.size).map{i=>
      test(Array("test",fn(i),"OX_defo"))
    }
  }
  def select_Net(mode:String = "OX_defo",H:Int=90,W:Int=240) = {
    var h = H
    var w = W
    val g = mode match {
      case "OX_defo" =>{
        val a = new Convolution_Matver(3,3,h,w,10)
        val b = new Pooling(2,10,h-3+1,w-3+1)
        val c = new ReLU()

        h = (h-3+1)/2
        w = (w-3+1)/2

        val a1 = new Convolution_Matver(3,10,h,w,10)
        val b1 = new Pooling(2,10,h-3+1,w-3+1)
        val c1 = new ReLU()

        h = (h-3+1)/2
        w = (w-3+1)/2

        val a2 = new Convolution_Matver(3,10,h,w,10)
        val b2 = new Pooling(2,10,h-3+1,w-3+1)
        val c2 = new ReLU()

        h = (h-3+1)/2
        w = (w-3+1)/2

        val a3 = new Convolution_Matver(3,10,h,w,10)
        val b3 = new Pooling(2,10,h-3+1,w-3+1)
        val c3 = new ReLU()

        h = (h-3+1)/2
        w = (w-3+1)/2

        val af1 = new Affine(h*w*10,100)
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

    val fn = args(1)
    val networkmode = args(2)
    val network = select_Net(networkmode)
    print("　test start　"+fn)
    //    val blackimg2 =  Image.read("testdate/"+fn)
    val blkpaper =  formula.AnsCutter.make_black_paper("testdate/"+fn)
    loads(network,networkmode)
    //tuner(network)//いい性能を出すためのチューニング
    val fnsplit = fn.dropRight(4).split("_")
    val name = fnsplit(0)
    val printnumber = fnsplit(1).toInt
    println(s"print:${fn} name : ${name} printnumber ${printnumber}")
    val Cutting_ans = formula.AnsCutter.cut(blkpaper).map(formula.AnsCutter.getbox(blkpaper,_)).toArray

    var yList = tester(network,Cutting_ans)
    resets(network)

    savecsv_String(yList,name,printnumber)

    yList
  }

  def train(args: Array[String]): Unit = {
    //args(0) is mode.  args(1) is epoch
    val networkmode = args(2)
    val network = select_Net(networkmode)
    val epoch = args(1).toInt
    val fn =scala.util.Random.shuffle(scala.sys.process.Process("ls black").lineStream.toList)

    val csv = scala.io.Source.fromFile("formula_data.csv").getLines.toArray
    val blackimg2 = fn.map(a => Image.read("black/"+a)).toArray

    val fn_test =scala.util.Random.shuffle(scala.sys.process.Process("ls testdate").lineStream.toList)
    val test_blackimg = fn_test.map(a => Image.read("testdate/"+a)).toArray

    var train_LOSSLIST = new Utilty.Stack[Float]()
    var train_ACCLIST  = new Utilty.Stack[Float]()
    var test_LOSSLIST  = new Utilty.Stack[Float]()
    var test_ACCLIST   = new Utilty.Stack[Float]()

    //loads(network,"OX_defo")

    for(ep<- 0 until epoch){
      timer.start
      var LOSS  = 0f
      var COUNT = 0f
      //train
      for(i <- 0 until fn.size){

        val filename = fn(i)
        val fnsplit = filename.dropRight(4).split("_")
        val name = fnsplit(0)
        val printnumber = fnsplit(1).toInt - 1
        //    print(s"epoch: ${ep} name: ${name} number: ${printnumber}  ")
        val lines = csv(Range(0,csv.size).par.map(i => csv(i).contains(name)).toArray.indexOf(true)).split(",").tail
        val Collect_ans = lines(printnumber).toArray.map{a => if(a == 'o') 1f else 0f }.toArray
        val Cutting_ans =formula.AnsCutter.cut(blackimg2(i)).map(formula.AnsCutter.getbox(blackimg2(i),_)).toArray
        var (loss,count) = learning(network,Collect_ans,Cutting_ans)
        LOSS += loss
        COUNT += count
        System.gc()
      }
      val train_time =timer.finish
      println(s"train => epoch: ${ep} loss: ${LOSS} count: ${COUNT} / ${10f*fn.size} Acc: ${COUNT/10f*fn.size} time: ${train_time}")

      //test
      var testLOSS  = 0f
      var testCOUNT = 0f
      timer.start
      for(i <- 0 until fn_test.size){

        val filename = fn_test(i)
        val fnsplit = filename.dropRight(4).split("_")
        val name = fnsplit(0)
        val printnumber = fnsplit(1).toInt - 1
        //    print(s"epoch: ${ep} name: ${name} number: ${printnumber}  ")
        val lines = csv(Range(0,csv.size).map(i => csv(i).contains(name)).toArray.indexOf(true)).split(",").tail
        val Collect_ans = lines(printnumber).toArray.map{a => if(a == 'o') 1f else 0f }.toArray
        val Cutting_ans = formula.AnsCutter.cut(test_blackimg(i)).map(formula.AnsCutter.getbox(test_blackimg(i),_)).toArray
        var (loss,count) = forwarder(network,Collect_ans,Cutting_ans)
        testLOSS += loss
        testCOUNT += count
        System.gc()
      }
      val test_time =timer.finish


      println(s"test  => epoch: ${ep} loss: ${testLOSS} count: ${testCOUNT} / ${10f*fn.size} Acc: ${testCOUNT/10f*fn.size} time: ${test_time}")


      if(ep % 50 == 0 || ep == epoch-1){
        saves(network,networkmode)
        println("save bias")
      }

      train_LOSSLIST.push(LOSS)
      train_ACCLIST.push(COUNT/10f*fn.size)
    }
    savetxt_Float(train_LOSSLIST.full,"OX_LOSS_"+timer.date,"txt")
    savetxt_Float(train_ACCLIST.full,"OX_ACC_"+timer.date,"txt")
  }



  def learning(Net:List[Layer],target:Array[Float],imgSet:Array[Array[Array[Array[Int]]]])={
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)
    var loss = 0f
    val h = imgSet(0).size
    val w = imgSet(0)(0).size
    //  val targetlist = List[String]()
    val yList = List[String]()
    var count = 0

    Range(0 ,imgSet.size).map{ i =>
      val img = Image.RGBto3DArray(flat_and_float(imgSet(i)),h,w).flatten

      val y = forwards(Net,img)
      val t = onehot(target(i).toInt,2)
      //print(target(i),y.indexOf(y.max))
      if(y.indexOf(y.max) == t.indexOf(t.max)) count += 1
      //  if(y.indexOf(y.max) == t.indexOf(t.max)) print(" o ") else print(" n ")
      loss += (t-y).sum
      backwards(Net.init,(y-t))
    }
    //  print(s"acc score ->  ${count / 10f}")
    updates(Net)
    (loss,count)
  }
  //test時に実行用
  def forwarder(Net:List[Layer],target:Array[Float],imgSet:Array[Array[Array[Array[Int]]]])={
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)
    var loss = 0f
    val h = imgSet(0).size
    val w = imgSet(0)(0).size
    //  val targetlist = List[String]()
    val yList = List[String]()
    var count = 0

    for(i <- 0 until imgSet.size){

      val img = Image.RGBto3DArray(flat_and_float(imgSet(i)),h,w).flatten

      val y = forwards(Net,img)
      val t = onehot(target(i).toInt,2)
      //print(target(i),y.indexOf(y.max))
      if(y.indexOf(y.max) == t.indexOf(t.max)) count += 1
      //  if(y.indexOf(y.max) == t.indexOf(t.max)) print(" o ") else print(" n ")
      loss += (t-y).sum
    }
    //  print(s"acc score ->  ${count / 10f}")
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
      timer.start
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
        var (loss,count) = learning(network,Collect_ans,Cutting_ans)
        LOSS += loss
        COUNT += count
      }
      updates(network)
      println(s"tuner -> epoch: ${ep} loss: ${LOSS} count: ${COUNT} / ${10f*fn.size} Acc: ${COUNT/10f*fn.size} time: ${timer.finish}")
    }

  }



}
