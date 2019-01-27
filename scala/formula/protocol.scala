package formula

object protcol{
  import formula.Utilty_formula._
  import Layer._
  import formula.protcol._
  import Utilty.ML._
  import Activation._
  import Utilty.RichArray._
  import Utilty.Stack
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

  val vec_trans = Array("f","{","}","(",")","x","=","+","-","n","s")//".","0","1","2","3","4","5","6","7","8","9"
  val vecnormal = Array("f","{","}","(",")","x","=","+","-",".","0","1","2","3","4","5","6","7","8","9")
  val vecw = Array("you","say","goodbye","and","I","hello")
  val vec_all = all_mk()
  val vec = vecnormal
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

  def all_mk()={
    val vec_trans = Array("f","{","}","(",")","x","=","+","-")
    val realnum = Range(0,99).map(i => i.toString).toArray
    val minnum  = Range(1,10000).map(i => (i / 1000d).toString ).toArray
    vec_trans++realnum++minnum
  }


}
