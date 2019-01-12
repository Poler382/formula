package formula

object  Discriminator{
  import Layer._
  import Utilty.ML._
  import protcol._
  import Utilty.Image
  val Image = new Image()
  var H = 105
  var W = 224
  def f (tf:String)= Image.RGBto3DArray(Image.read(tf).flatten.flatten.map(_.toFloat),H,W).flatten.map{a =>(255f - a)/255f}
  def g (tf:String)= tf.dropRight(4).split("_")(2).toInt
  def h (a :Int)={
    val ar = Array.ofDim[Float](2)
    ar(a)=1f
    ar
  }

  def main(args: Array[String]): Unit = {

    val num = args(0).toInt
    val files = sys.process.Process("ls img/").lineStream.toArray.map{a => "img/"+a}
    val trainfile = files.take((files.size*0.7).toInt)
    val testfile = files.drop((files.size*0.7).toInt)
    val train_img = trainfile.map(f(_))
    val test_img  = testfile.map(f(_))
    val train_label = trainfile.map(g(_)).map(h(_))
    val test_label = testfile.map(g(_)).map(h(_))

  }

}
