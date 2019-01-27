package formula

object AnsCutter{
  import formula.protcol._
  import Layer._
  import Utilty.RichCal._
  import Utilty_formula._
  import Utilty.Image
  val Image = new Image()
  //答えのボックスの位置座標
  var start_Coordinate = (350,1202)//start
  var ansbox = (105,224) // kaitou kuurann
  var nextbox = (74,0) // next box
  var Coordinate = (0,0) // shokika
  var ans_start = (0,0)
  val averageCoordnate = Array((1165,383), (1165,529), (1165,675), (1165,823), (1165,972), (1165,1120), (1165,1270), (1165,1416), (1165,1563),(1165,1709))
  def isBlack(img: Array[Int],lim : Int = 90) ={
    if(img(0) < lim && img(1) < lim && img(2) < lim ){
      true
    }else{
      false
    }
  }

  def lookRGB(img: Array[Int])={
    (img(0),img(1),img(2))
  }

  def getbox(img:Array[Array[Array[Int]]],start:(Int,Int))={
    val w = 240
    val h = 90
    var ansbox = Array.ofDim[Int](h,w,3)
    for(i <- 0 until h;j <- 0 until w; c <- 0 until 3){
      ansbox(i)(j)(c) = img(start._1+2+i)(start._2+2+j)(c)
    }
    ansbox
  }

  def get_rowband_black(img:Array[Array[Array[Int]]],start:(Int,Int),h:Int= 2, w:Int = 230)={
    var sum = 0

    for(i <- 0 until h;j <- 0 until w;c <- 0 until 3){
      sum += img(start._1 + i)(start._2 + j)(c)
    }
    sum / (h*w*3).toFloat
  }
  def get_colband_black(img:Array[Array[Array[Int]]],start:(Int,Int),h:Int= 70, w:Int = 2)={
    var sum = 0
    for(i <- 0 until h;j <- 0 until w;c <- 0 until 3){
      sum += img(start._1 + i)(start._2 + j)(c)
    }
    sum / (h*w*3).toFloat
  }
  def main(args: Array[String]): Unit = {
    try_it()
  }

  def try_it(){
    val fn0 =scala.sys.process.Process("ls marutuke_set").lineStream.toArray
    val ar = Array(fn0)
    for(fn <- fn0){
      Image.write("aftercut/"+fn+"_black.png",make_black_paper("marutuke_set/"+fn))
      val m =formula.AnsCutter.cut2("aftercut/"+fn+"_black.png")
      Image.write("aftercut/"+fn+"_check.png",m)
      //sys.process.Process("rm aftercut/*black*.png").run
    }
  }
  def make_black_paper(filename:String)={
    var img = Image.read(filename)
    val H = img.size
    val W = img(0).size
    for(i <- 0 until H; j <- 0 until W ){
      if(img(i)(j)(0) < 240 && img(i)(j)(1) < 240 && img(i)(j)(2) < 240  ){
        for(c <- 0 until 3){
          img(i)(j)(c) = 0//color(flagcounter)(c)
        }
      }
    }
    img
  }
  def cut(img:Array[Array[Array[Int]]])={
    val H = img.size;val W = img(0).size
    var start_CoordinateList = List[(Int,Int)]()
    Coordinate = start_Coordinate
    for((j,i) <- averageCoordnate  ){
      val range = 50
      //col2_250row70_2_120_120_last3
      var min = ((Float.PositiveInfinity,Float.PositiveInfinity),(0,0))//黒の値，その時の座標

      for(k <- -range/2 until range; l<- -range until range){
        val (row,col) = min._1
        if(row > get_rowband_black(img,(i+k,j+l),1,250) && col > get_colband_black(img,(i+k,j+l),80,1)){
          min = ((get_rowband_black(img,(i+k,j+l),1,250),(get_colband_black(img,(i+k,j+l),80,1))),(i+k,j+l))
        }
      }
      Coordinate = min._2
      start_CoordinateList ::= Coordinate
    }

    start_CoordinateList.reverse
  }


  def cut2(filename:String)={
    var color = Array(Array(152,52,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0))
    var img = Image.read(filename)
    var img2 = Image.read(filename)

    val H = img.size
    val W = img(0).size
    var ansimg = Array.ofDim[Int](10,ansbox._1,ansbox._2,3)
    var flagcounter = 0
    var isok = false
    var Window = 20
    var start_CoordinateList = List[(Int,Int)]()
    Coordinate = start_Coordinate
    val file = rand.nextInt(1000)
    for((j,i) <- averageCoordnate  ){
      println(j,i)
      val range = 50
      //col2_250row70_2_120_120_last3
      var row = (Float.PositiveInfinity,(0,0))//黒の値，その時の座標
      var col = (Float.PositiveInfinity,(0,0))
      for(k <- -range/2 until range; l<- -range until range){
        if(row._1 > get_rowband_black(img,(i+k,j+l),1,250) && col._1 > get_colband_black(img,(i+k,j+l),80,1)){
          row = (get_rowband_black(img,(i+k,j+l),1,250),(i+k,j+l))
          col = (get_colband_black(img,(i+k,j+l),80,1),(i+k,j+l))
        }
      }



      Coordinate = row._2
      print(filename,flagcounter)
      print(i,j)//," color -> ",lookRGB(color(flagcounter)))
      println( "  => " +lookRGB(img(i)(j))+", row : "+row+", col: "+col)

      start_CoordinateList ::= Coordinate

      val mm = getbox(img,Coordinate)
      //  Image.write(filename+"_"+flagcounter+".png",mm)
      flagcounter +=1



      //  色変えて角を見てた
      for(l <- 0 until Window;m <- 0 until Window){
        for(c <- 0 until 3){
          img2(Coordinate._1+l)(Coordinate._2+m)(c) = color(0)(c)
        }
      }
      for(l <- 0 until 5;m <- 0 until 5){
        for(c <- 0 until 3){
          img(Coordinate._1+l)(Coordinate._2+m)(c) = 255
        }
      }

      img(Coordinate._1)(Coordinate._2) =Array(255,0,0)

    }


    //}
    img2
  }



}
