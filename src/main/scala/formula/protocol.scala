package formula

object protcol{
  import Utilty_formula._
  import Layer._
  import Utilty.ML._
  import Activation._
  import Utilty.RichArray._
  import Utilty.Stack
  import Utilty.timer
  import word2vec._
  val rand = new scala.util.Random(2)
  val work_directry_path = "/Users/yuya/Desktop/中１方程式/"
  val data_path = "/Users/yuya/Desktop/中１方程式/中１方程式プリント実施具合.csv"
  val q1_10_path = "/Users/yuya/Programing/tex/中１方程式/question1-10.tex"
  val collection_path = "collection.txt"
  val question_path =  "/Users/yuya/Programing/tex/中１方程式/QList.txt"
  val question2_path =  "/Users/yuya/Programing/tex/中１方程式/QList2.txt"

  val uchinagirei_d = "uchireinagi/"

  val tex_folder = "tex/"
  val vec = Array("f","{","}","(",")","x","=","+","-",".","0","1","2","3","4","5","6","7","8","9")
  val vecw = Array("you","say","goodbye","and","I","hello")
  val vec2= Array(
    Array[Float](1,0,0,0,0,0,0,0),//you
    Array[Float](0,1,0,0,0,0,0,0),//say
    Array[Float](0,0,1,0,0,0,0,0),//goodbye
    Array[Float](0,0,0,1,0,0,0,0),//and
    Array[Float](0,0,0,0,1,0,0,0),//I
    Array[Float](0,0,0,0,0,1,0,0), //hello
  )

  val R = 0
  val G = 1
  val B = 2
}

object Utilty_formula{
  import protcol._
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
