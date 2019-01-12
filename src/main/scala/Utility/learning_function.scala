package Utilty
object ML{
  import Layer._
  val rand = new scala.util.Random(0)

  def print_result(
    num:Int,
    time:Double,
    namelist:List[String],
    outlist:List[Double],
    sp:Int)={
      var printdata = "result:"+num.toString+" - time:"+(time/1000d).toString+"\n"

      for(i <- 0 until outlist.size){
        printdata += namelist(i)+":"+outlist(i).toString+"/"
        if((i+1) % sp == 0 && i!= 0  ){printdata += "\n"}
      }

      println(printdata)

      printdata
    }

    def savetxt_Float(list:List[Float],fn:String,path:String)={
      val pathName = path+"/"+fn+".txt"
      val writer =  new java.io.PrintWriter(pathName)
      val ys1 = list.reverse.mkString(",") + "\n"
      writer.write(ys1)
      writer.close()
      println("success "+fn)
      true
    }

    def savetxt_String(list:List[String],fn:String,path:String)={
      val pathName = path+"/"+fn+".txt"
      val writer =  new java.io.PrintWriter(pathName)
      val ys1 = list.reverse.mkString(",") + "\n"
      writer.write(ys1)
      writer.close()
      println("success "+fn)
      true
    }

    def forwards(layers:List[Layer],x:Array[Float])={
      var temp = x
      for(lay <- layers){
        temp =lay.forward(temp)
      }
      temp
    }

    def backwards(layers:List[Layer],x:Array[Float])={
      var d = x
      for(lay <- layers.reverse){d = lay.backward(d)}
      d
    }

    def forwards(layers:List[Layer],x:Array[Array[Float]]): Array[Array[Float]]={
      var temp = x
      for(lay <- layers){
        temp =lay.forward(temp)
      }
      temp
    }

    def backwards(layers:List[Layer],x:Array[Array[Float]]): Array[Array[Float]]={
      var d = x
      for(lay <- layers.reverse){
        d = lay.backward(d)
      }
      d
    }

    def updates(layers:List[Layer])={
      for(lay <- layers){lay.update()}
    }

    def resets(layers:List[Layer]){
      for(lay <- layers){lay.reset()}
    }


    def saves(layers:List[Layer],fn:String){

      for(i <- 0 until layers.size){
        layers(i).save("biasdata/"+fn+"_"+i.toString)
      }

    }

    def loads(layers:List[Layer],fn:String){
      for(i <- 0 until layers.size ){
        layers(i).load("biasdata/"+fn+"_"+i.toString)
      }
    }

  }
