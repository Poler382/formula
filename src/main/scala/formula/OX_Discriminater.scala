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
  def mkblack={
    val fn =scala.sys.process.Process("ls marutuke_set").lineStream.toArray
    for(filename <- fn){
      val fnsplit = filename.dropRight(4).split("_")
      val name = fnsplit(0)
      val printnumber = fnsplit(1).toInt
      Image.write("black/"+name+"_"+printnumber+"black.png",formula.AnsCutter.make_black_paper("marutuke_set/"+filename))
    }

  }

  def main(args: Array[String]): Unit = {
    //args(0) is mode.  args(1) is epoch
    val network = select_Net()
    val epoch = args(1).toInt
    val fn =scala.sys.process.Process("ls black").lineStream.toArray
    val csv = scala.io.Source.fromFile("formula_data.csv").getLines.toArray
    val blackimg2 = fn.map(a => Image.read("black/"+a)).toArray
    var LOSSLIST = new Utilty.Stack[Float]()
    var ACCLIST = new Utilty.Stack[Float]()

    for(ep<- 0 until epoch){
      timer.start
      var LOSS  = 0f
      var COUNT = 0f
      for(i <- 0 until fn.size){

        val filename = fn(i)
        val fnsplit = filename.dropRight(9).split("_")
        val name = fnsplit(0)
        val printnumber = fnsplit(1).toInt - 1
        //print(s"epoch: ${ep} name: ${name} number: ${printnumber}  ")
        val lines = csv(Range(0,csv.size).map(i => csv(i).contains(name)).toArray.indexOf(true)).split(",").tail
        val Collect_ans = lines(printnumber).toArray.map{a => if(a == 'o') 1f else 0f }.toArray
        val Cutting_ans =formula.AnsCutter.cut(blackimg2(i)).map(formula.AnsCutter.getbox(blackimg2(i),_)).toArray
        var (loss,count) = learning(network,Collect_ans,Cutting_ans)
        LOSS += loss
        COUNT += count
      }
      println(s"epoch: ${ep} loss: ${LOSS}  Acc: ${COUNT/10f*fn.size} time: ${timer.finish}")
      saves(network,"OX_defo")
      LOSSLIST.push(LOSS)
      ACCLIST.push(COUNT/10f*fn.size)
    }
    savetxt_Float(LOSSLIST.full,"OX_LOSS_"+timer.date,"txt")
    savetxt_Float(ACCLIST.full,"OX_ACC_"+timer.date,"txt")
  }

  def select_Net(mode:String = "defo",H:Int=90,W:Int=240) = {
    var h = H
    var w = W
    val g = mode match {
      case "defo" =>{
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
        val ac  = new Sigmoid()
        val af2 = new Affine(100,1)
        val ac2  = new Sigmoid()

        List(a,b,c,a1,b1,c1,a2,b2,c2,a3,b3,c3,af1,ac,af2,ac2)
      }

    }

    g
  }

  def learning(Net:List[Layer],target:Array[Float],imgSet:Array[Array[Array[Array[Int]]]])={
    def flat_and_float(im: Array[Array[Array[Int]]]) = im.flatten.flatten.map(_.toFloat)
    var loss = 0f
    val h = imgSet(0).size
    val w = imgSet(0)(0).size
    val targetlist = List[String]()
    val yList = List[String]()
    var count = 0
    
    for(i <- 0 until imgSet.size){

      val img = Image.RGBto3DArray(flat_and_float(imgSet(i)),h,w).flatten

      val y = forwards(Net,img)
      var t1 = ""
      var t2 = ""
      if(target(i) > 0.5) t1 = "o" else t1 = "x"
      if(y(0) > 0.5f) t2 = "o" else t2 = "x"
      //    print(target(i),y(0))
      if(t1 == t2) count += 1
      loss += (target(i)-y(0))
      backwards(Net,Array(target(i)-y(0)))
    }
  //  print(s"acc score ->  ${count / 10f}")
    updates(Net)
    (loss,count)
  }





}
