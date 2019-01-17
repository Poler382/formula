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

  def bandtest(fn:String = "marutuke_set/oono_011.jpg")={
    val ono011 = Array((1169,387),(1169,533),(1169,678),(1169,825),(1169,974),(1169,1120),(1169,1269),(1169,1417),(1169,1566),(1169,1713))
    val ono012 = Array((1167,387),(1167,532),(1167,679),(1167,827),(1167,976),(1167,1125),(1167,1275),(1167,1420),(1167,1567),(1167,1713))
    val otuki12= Array((1161,376),(1161,522),(1161,668),(1161,818),(1161,967),(1161,1117),(1161,1266),(1161,1411),(1161,1557),(1161,1703))
    val CL=Array(ono011,ono012,otuki12)
    val fL = Array("marutuke_set/oono_011.jpg","marutuke_set/oono_012.jpg","marutuke_set/ootuki_012.jpg")
    //  val fn = "marutuke_set/oono_011.jpg"
    var img3 = Image.read(fn)

    var Window = 2
    for(i <- 0 until CL.size){
      val Image = new Image()
      var img = Image.read(fL(i))
      var img2 = Image.read(fL(i))

      for(cood <- CL(i)){
        for(p <- -Window to Window; q <-  -Window to Window){

          print("number:"+i,cood,p,q)
          println( "  => " +lookRGB(img(cood._2+p)(cood._1+q))+","+get_rowband_black(img,(cood._2+p,cood._1+q))+","+get_colband_black(img,(cood._2+p,cood._1+q)))
          val Coordinate = (cood._2,cood._1)
          img2(Coordinate._1+p)(Coordinate._2+q)(0) = 0
          img2(Coordinate._1+p)(Coordinate._2+q)(1) = 0
          img2(Coordinate._1+p)(Coordinate._2+q)(2) = 255

        }
      }
      val file = rand.nextInt(100)
      println(file)
      Image.write(i+"_"+file+".png",img2)

      println()
    }


    img3
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

  def cut(filename:String)={
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
    for(i <- 0 until H; j <- 0 until W ){
      if(i > 300 && j < 1300 && j > W/3 * 2 && i < H -100){
        //if(isBlack(img(i)(j),145) ){ //黒か判断する
        //  println(s"ave ${img(i)(j).sum / 3f}")
        //col2_250row70_2_120_120_last3
        val row = Range(-2,5).map(l => get_rowband_black(img,(i+l,j),2,250)).toArray.sorted.take(2).sum / 2f
        val col = Range(-2,5).map(l => get_colband_black(img,(i,j+l),50,2)).toArray.sorted.take(2).sum / 2f
        //  val col_last = Range(-2,5).map(l => get_colband_black(img,(i,j+245+l),30,3)).toArray.sorted.take(3).sum / 3f
        if( row < 160f && col  < 160f ){

          if(flagcounter == 0 ){
            Coordinate = (i,j)
            print(filename,flagcounter)
            print(i,j)//," color -> ",lookRGB(color(flagcounter)))
            println( "  => " +lookRGB(img(i)(j))+", row : "+row+", col: "+col)

            start_CoordinateList ::= Coordinate

            val mm = getbox(img,Coordinate)
            //  Image.write(filename+"_"+flagcounter+".png",mm)
            flagcounter +=1

          }else{
            if( (Coordinate - (i,j)).abs.sum > 150){
              print(filename,flagcounter)
              Coordinate = (i,j)
              var Window = 8

              print(i,j)//," color -> ",lookRGB(color(flagcounter)))
              println( "  => " +lookRGB(img(i)(j))+", row : "+row+", col: "+col)

              start_CoordinateList ::= Coordinate
              val mm = getbox(img,Coordinate)

              //  Image.write(filename+"_"+flagcounter+".png",mm)
              flagcounter +=1
            }
          }

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


        }
      }

      //}
    }

    img2
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
