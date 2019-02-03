object Search_img{
  val Image = new Utilty.Image()
  var tmp = Array.ofDim[Float](255*255*255,3)

  def load_img(fn:String)={
    println("read now")
    Image.read(fn).map( _.map( _.map(_.toFloat) ) ).toArray
  }
  def compear(a:Array[Float],b:Array[Float])={
    if(a(0)-b(0)==0 && a(1)-b(1)==0 && a(2)-b(2)==0 ){
      true
    }else{
      false
    }
  }

  //被ってない色を探す！，縦横を渡すことを忘れずに
  def non_RGB_search(namelist:Array[String],H:Int,W:Int)={
    val list = namelist.map(load_img(_))
    println("let's search")
    val ch = Array.ofDim[Float](tmp.size)
    for(n <- 0 until list.size){//一つの画像
      val img = list(n)//一つの画像
      print(s"${namelist(n)}>")
      for(i <- 0 until img.size;j <- 0 until img(0).size){
        print(".")
        //早くするために並列処理
        Range(0 ,tmp.size-1).par.map{t =>
          if(compear(tmp(t),img(i)(j))){
            ch(t) += 1
            println("\n!\n")
          }
        }
      }
    }

    println("\n\n search Result")

    for(i <- 0 until ch.size){
      if(ch(i) == 0){
        tmp(i).foreach{a => print(a + " ")}
      }
    }



  }

  def main(args: Array[String]): Unit = {
    for(r <- 0 until 255;g <- 0 until 255;b <- 0 until 255){
      tmp(r * 255 + g * 255 + b) = Array(r,g,b)
    }
    //ディレクトリだけ間違えないように！
    val imglist = sys.process.Process("ls RSdata/move/test01/").lineStream.toArray.map(a => "RSdata/move/test01/"+a )
    non_RGB_search(imglist,300,300)
  }


}
