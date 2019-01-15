package formula

object AnsCutter{
  import formula.protcol._
  import Layer._
  import Utilty.Image
  import Utilty.RichCal._
  import Utilty_formula._
  val Image = new Image()
  //答えのボックスの位置座標
  var start_Coordinate = (350,1202)//start
  var ansbox = (105,224) // kaitou kuurann
  var nextbox = (74,0) // next box
  var Coordinate = (0,0) // shokika
  var ans_start = (0,0)

  def cut(filename:String)={
    var img = Image.read(filename)
    val H = img.size
    val W = img(0).size
    var ansimg = Array.ofDim[Int](10,ansbox._1,ansbox._2,3)
    var flagcounter = 0
    var start_CoordinateList = List[(Int,Int)]()
    Coordinate = start_Coordinate
    for(i <- 0 until H; j <- 0 until W ){

      if(img(i)(j).sum / 3f  <  50){ //平均から黒か判断する
        if(flagcounter == 0) Coordinate = (i,j)
        flagcounter  +=1
      }else{
        flagcounter = 0
      }

      if(flagcounter > 200){
        start_CoordinateList ::= Coordinate

        flagcounter = 0
      }
    }
    ansimg
  }
  def main(args: Array[String]): Unit = {

    val filename = sys.process.Process("ls marutuke/").lineStream.toArray.map{a => uchinagirei_d+a}
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
