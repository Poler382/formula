package formula
object Denominator{
  import Layer._
  import Utilty.ML._
  import Utilty.RichArray._
  import Utilty.Image
  import Activation._
  import protcol._

  val Image = new Image()
  var H = 105
  var W = 224
  def f (tf:String)= Image.RGBto3DArray(Image.read(tf).flatten.flatten.map(_.toFloat),H,W).flatten.map{a =>(255f - a)/255f}
  def g (tf:String)= tf.dropRight(4).split("_")(4).toInt
  def h (a :Int)={
    val ar = Array.ofDim[Float](200)
    ar(a)=1f
    ar
  }

  val C1 = new Convolution_Matver(4,3,H,W,8)
  val R1 = new ReLU()
  val P1 = new Pooling(2,8,102,221)
  val C2 = new Convolution_Matver(4,8,51,110,16)
  val R2 = new ReLU()
  val P2 = new Pooling(2,16,48,107)

  val C3 = new Convolution_Matver(4,16,24,53,32)
  val R3 = new ReLU()
  val P3 = new Pooling(2,32,21,50)
  val C4 = new Convolution_Matver(4,32,10,25,64)
  val R4 = new ReLU()

  val C5 = new Convolution_Matver(4,64,7,22,128)
  val R5 = new ReLU()
  val C6 = new Convolution_Matver(4,128,4,19,128)
  val R6 = new ReLU()

  val af1= new Affine(1*16*128,1000)
  val re1= new ReLU()
  val af2= new Affine(1000,500)
  val re3= new ReLU()
  val af3= new Affine(500,200)
  val sf = new SoftMax()

  val NN = List(C1,R1,P1,C2,R2,P2,C3,R3,P3,C4,R4,C5,R5,C6,R6,af1,re1,af2,re3,af3,sf)


  def forward(im:Array[Float]) = forwards(NN,im)
  def backward(im:Array[Float],label:Array[Float]) = backwards(NN,im-label)
  def update() = updates(NN)
  def reset() = resets(NN)
  def load()=loads(NN,"Denominator")

  def main(args:Array[String])={
    val num = args(0).toInt
    val files = sys.process.Process("ls fraction/").lineStream.toArray.map{a => "fraction/"+a}
    val trainfile = files.take((files.size*0.7).toInt)
    val testfile = files.drop((files.size*0.7).toInt)
    val train_img = trainfile.map(f(_))
    val test_img  = testfile.map(f(_))
    val train_label = trainfile.map(g(_)).map(h(_))
    val test_label = testfile.map(g(_)).map(h(_))


    //loads(NN,"Denominator")

    for (epoch <- 0 until num){
      var start = System.currentTimeMillis()
      var count = 0f
      var count2= 0f
      val y1 = forwards(NN,train_img)
      backwards(NN,y1-train_label)
      updates(NN)
      val y2 =forwards(NN,test_img)
      resets(NN)

      for(i <- 0 until y1.size){
        if(y1(i).indexOf(y1(i).max) == train_label(i).indexOf(train_label(i).max)){
          count+=1f
        }
      }
      for(i <- 0 until y2.size){
        if(y2(i).indexOf(y2(i).max) == test_label(i).indexOf(test_label(i).max)){
          count2+=1f
        }
      }
      var time = System.currentTimeMillis() - start
      println("*******epoch:"+epoch+"/ time:"+time+"s********")
      println("train_ACCrate "+count/trainfile.size)
      println("test_ACCrate  "+count2/testfile.size)
      println("*************************************")

      if(epoch % 50 == 0 || epoch -1 == num){
        println("save....")
        saves(NN,"Denominator")
      }


    }
  }
}
