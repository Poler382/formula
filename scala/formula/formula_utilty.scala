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

    x
  }

  def all_mk()={
    val vec_trans = Array("f","{","}","(",")","x","=","+","-")
    val realnum = Range(0,99).map(i => i.toString).toArray
    val minnum  = Range(1,10000).map(i => (i / 1000d).toString ).toArray
    vec_trans++realnum++minnum
  }
  def question_load_all2(path:String,v:List[String])={
    def f (s :String) = v.indexOf(s)
    val st = scala.io.Source.fromFile(path).getLines.toArray

    st.map(_.split(" ").map(f(_)))//文字をベムトルに対応する数字列で返す
  }
  def question_load_nonnum(path:String)={
    val st = scala.io.Source.fromFile(path).getLines.toArray.map(_.replace(" ","")).toArray
    var xf = st.map(_.replace("""\frac""","f"))
    def strI(a : Int) = a.toString
    def strD(a : Double) = a.toString
    for(k <- 1000 until 0 by -1){
      xf = xf.map(_.replace(strD(k/100d),"s"))
    }

    for(k <- 99 to 10 by -1){
      xf = xf.map(_.replace(strI(k),"n"))
    }

    for(k <- 0 to 9){
      xf = xf.map(_.replace(strI(k),"n"))
    }

    xf
  }

  def string2vecnumber(st:String)={
    // string2vecnumber("5x+2=1") => Array[Int] = Array(15, 5, 7, 12, 6, 11)
    val t = st.replace("""\frac""","f")
    var return_st = Array.ofDim[Int](t.size)
    for(i <- 0 until t.size){
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

  def string2vectorInt(ar:String)={

    var re = Array.ofDim[Int](ar.size)
    for(i <- 0 until ar.size){
      re(i) = vec.indexOf(ar(i).toString)
    }
    re
  }

}
