object d2p{


  def main(args: Array[String]): Unit = {
    val fnlist = sys.process.Process("ls Emvedding/").lineStream.toArray
    for(fn <- fnlist){
      println(fn)
      val num = fn.split("_").filter(_.contains("89x"))(0).split("x")
      val title = fn.split("_").tail.take(8).mkString("_")
      val t = fn.split("_").tail.take(3).mkString("_")

      if(num(0) == "89"){
        Utilty.call_py.TsneAndPcaPy0("Emvedding/"+fn,num(0),num(1),title,"labels89.txt",t)
        Thread.sleep(15000)
      }else if(num(0) == "91"){
        Utilty.call_py.TsneAndPcaPy0("Emvedding/"+fn,num(0),num(1),title,"labels91.txt",t)
        Thread.sleep(15000)
      }else{
        sys.process.Process("say No").run
        Thread.sleep(1000)
      }


    }





  }

}
