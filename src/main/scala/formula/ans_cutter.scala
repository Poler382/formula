package formula

object AnsCutter{
  import protcol._
  import Layer._
  import Utilty.Image
  import Utilty.RichCal._
  import Utilty_formula._
  val Image = new Image()
  def main(args: Array[String]): Unit = {

    val filename = sys.process.Process("ls "+uchinagirei_d).lineStream.toArray.map{a => uchinagirei_d+a}
    var start_Coordinate = (350,1202)
    var ansbox = (105,224)
    var nextbox = (74,0)
    var Coordinate = (0,0)
    var img = Image.read(filename(0))
    var ansimg = Array.ofDim[Float](filename.size,10,ansbox._1,ansbox._2,3)

    for(f <- 0 until filename.size ){
      var img = Image.read(filename(f))
      Coordinate = start_Coordinate
      for(num <- 0 until 10){
        for(height <- 0 until ansbox._1; width <- 0 until ansbox._2){
          for(c <- 0 until 3){
            if (
              img(Coordinate._1+height)(Coordinate._2+width)(R) > 180 &&
              img(Coordinate._1+height)(Coordinate._2+width)(G) < 235 &&
              img(Coordinate._1+height)(Coordinate._2+width)(B) < 245
            ){
              ansimg(f)(num)(height)(width)(c) = 255
            }else{
              ansimg(f)(num)(height)(width)(c) = img(Coordinate._1+height)(Coordinate._2+width)(c)

            }
          }
        }
        Coordinate += nextbox+(ansbox._1,0)
      }
    }



    for(i <- 0 until filename.size){
      for(j <- 0 until 10){
        Image.write("img/"+str(i)+str(j)+".png",Image.allInt(ansimg(i)(j)))
      }

    }
  }


}
