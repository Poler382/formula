object d2p{


  def main(args: Array[String]): Unit = {
    val fnlist = sys.process.Process("ls txt/new/").lineStream.toArray
    for(fn <- fnlist){
      println(fn)
      val num = fn.split("_").filter(_.contains("22x"))(0).split("x")
      val title = fn.split("_").tail.take(8).mkString("_")
      val t = fn.split("_").tail.take(3).mkString("_")
      println(num(0),num(1),title,t)

      Utilty.call_py.TsneAndPcaPy0("txt/new/"+fn,"20",num(1),"num_"+title,"Ex200.txt",t)
      Thread.sleep(1500)
      Utilty.call_py.TsneAndPcaPy0("txt/new/"+fn,"20",num(1),"formula_"+title,"Ex20.txt",t)
      Thread.sleep(1500)

    }



  }

}
