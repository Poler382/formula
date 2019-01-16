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
  def get_colband_black(img:Array[Array[Array[Int]]],start:(Int,Int),h:Int= 50, w:Int = 2)={
    var sum = 0

    for(i <- 0 until h;j <- 0 until w;c <- 0 until 3){
      sum += img(start._1 + i)(start._2 + j)(c)
      /*  img(start._1 + i)(start._2 + j)(0) = 0
      img(start._1 + i)(start._2 + j)(1) = 0
      img(start._1 + i)(start._2 + j)(2) = 255
      */
    }
    sum / (h*w*3).toFloat
  }

  def try_it(){
    val fn0 = "marutuke_set/oono_011.jpg"
    val fn1 = "marutuke_set/oono_017.jpg"
    val fn2 = "marutuke_set/oono_017.jpg"
    val ar = Array(fn0,fn1,fn2)
    for(fn <- ar){

      val m =formula.AnsCutter.cut(fn)
      val Image = new Image()
      val file = rand.nextInt(1000)
      println(file)
      Image.write(file+".png",m)

    }
  }
  def make_black_paper(filename:String)={
    var img = Image.read(filename)
    val H = img.size
    val W = img(0).size
    for(i <- 0 until H; j <- 0 until W ){
      if(img(i)(j)(0) < 240 && img(i)(j)(1) < 240 && img(i)(j)(2) < 240  ){
        for(c <- 0 until 3){
          img(i)(j)(c) = 0//ßcolor(flagcounter)(c)
        }
      }
    }
    img
  }

  def cut(filename:String)={
    var color = Array(Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0),Array(0,0,255),Array(0,255,0),Array(255,0,0))
    var img = Image.read(filename)
    val H = img.size
    val W = img(0).size
    var ansimg = Array.ofDim[Int](10,ansbox._1,ansbox._2,3)
    var flagcounter = 0
    var isok = false
    var start_CoordinateList = List[(Int,Int)]()
    Coordinate = start_Coordinate
    val file = rand.nextInt(1000)
    for(i <- 0 until H; j <- 0 until W ){
      if(img(i)(j)(0) < 240 && img(i)(j)(1) < 240 && img(i)(j)(2) < 240  ){
        for(c <- 0 until 3){
          img(i)(j)(c) = 0//ßcolor(flagcounter)(c)
        }
      }

      if(i > 300){
        if(isBlack(img(i)(j),145) ){ //黒か判断する
          //  println(s"ave ${img(i)(j).sum / 3f}")
          if(get_rowband_black(img,(i,j),2,20) < 165f && get_colband_black(img,(i,j),20,2) < 155f){
            Coordinate = (i,j)
            var Window = 3
            print(flagcounter)
            print(i,j," color -> ",lookRGB(color(flagcounter)))
            println( "  => " +lookRGB(img(i)(j))+", row : "+get_rowband_black(img,Coordinate)+", col: "+get_colband_black(img,Coordinate))

            start_CoordinateList ::= Coordinate

            val mm = getbox(img,Coordinate)
            val Image = new Image()
            Image.write(filename+"_"+flagcounter+".png",mm)

            for(l <- -Window until Window;m <- -Window until Window){
              for(c <- 0 until 3){
                img(Coordinate._1+l)(Coordinate._2+m)(c) = color(flagcounter)(c)
              }
              /*  img(Coordinate._1+l)(Coordinate._2+m)(0) = 0
              img(Coordinate._1+l)(Coordinate._2+m)(1) = 0
              img(Coordinate._1+l)(Coordinate._2+m)(2) = 255*/
            }
            flagcounter +=1
          }
        }

      }
    }

    img
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
