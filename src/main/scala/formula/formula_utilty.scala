package formula

object Utilty_formula{
  import formula.protcol._
  def str(a:Int) = a.toString
  def onehot(a:Int,size:Int) = {
    var  R = Array.ofDim[Float](size)
    R(a) = 1f
    R
  }
  def load()={
    val raw = scala.io.Source.fromFile(data_path).getLines.toArray.drop(2).map(_.split(","))
    val name = raw.map(_(0).trim())
    val result = Array.ofDim[Array[String]](raw.size)
    for(i <- 0 until raw.size){
      result(i)= raw(i)(1).split("#")
    }
    (name,result)
  }


  def texload(path:String)={
    //  val raw = scala.io.Source.fromFile(path).getLines.toArray.filter{_.contains("""large""")}.toArray.split("""\item""").flatten.grouped(10).toArray
    val raw = scala.io.Source.fromFile(path).getLines.toArray.filter{_.contains("""large""")}.toArray

    raw.map(_.replace("""\item"""," ")).grouped(10).toArray
  }

  def question_load(path:String)={
    scala.io.Source.fromFile(path).getLines.toArray.map(_.replace(" ","")).toArray
  }

  def question_load2(path:String)={
    val st = scala.io.Source.fromFile(path).getLines.toArray.map(_.replace(" ","")).toArray
    var x = st.map(_.replace("""\frac""","f"))
    for(i <- 0 until x.size){

      x(i) =x(i) + " " * (50 - x(i).size)
    }
    x
  }
  def question_load3(path:String)={
    val st = scala.io.Source.fromFile(path).getLines.toArray.map(_.replace(" ","")).toArray
    var x = st.map(_.replace("""\frac""","f"))
    for(i <- 0 until x.size){

      x(i) =x(i)
    }
    x
  }

  def string2vecnumber(st:String)={
    // string2vecnumber("5x+2=1") => Array[Int] = Array(15, 5, 7, 12, 6, 11)
    val t = st.replace("""\frac""","f")
    var return_st = Array.ofDim[Int](t.size)
    for(i <- 0 until t.size){
      println(t(i),vec.indexOf(t(i).toString))
      return_st(i) = vec.indexOf(t(i).toString)
    }
    return_st
  }


  def string2onehot(st:String)={

    val t = st.replace("""\frac""","f")
    //  println(t)
    var return_st = Array.ofDim[Float](t.size,vec.size)
    for(i <- 0 until t.size){
      //println(t(i))
      return_st(i)(vec.indexOf(t(i).toString)) = 1f

    }
    return_st
  }

  def onehot2string(ar:Array[Array[Float]])={

    var reSt = ""
    for(i <- 0 until ar.size){
      reSt += vec(ar(i).indexOf(ar(i).max) )
    }
    reSt.replace("f","""\frac""")
  }

}
