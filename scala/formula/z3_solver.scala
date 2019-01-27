package formula

object solver_formula{
  val hedder = List[String]("from z3 import *","import sys")
  var outputfile1 = List[String](
    "",
    """x = Real("x")""",
    "s = Solver()"
  )
  var outputfile_fst = List[String](
    "s.check()",
    "m = s.model()",
    "print(m[x].as_fraction())",
    "path = '/Users/yuya/Programing/sbt/formula/anstemp.txt'",
    "",
    "with open(path,mode='w') as f:",
    " f.write(str(m[x].as_fraction()))",
    """ f.write("\n")"""
  )
  var outputfile2 = List[String](
    "s.add(x / 10 < 3)",
    "s.check()",
    "m = s.model()",
    "print(m[x].as_fraction())",
    "path = '/Users/yuya/Programing/sbt/formula/anstemp.txt'",
    "",
    "with open(path,mode='a') as f:",
    " f.write(str(m[x].as_fraction()))",
    """ f.write("\n")"""
  )
  def make_z3program() = {
    val pathName = "/Users/yuya/Programing/python/z3_out_anstext.py"
    val writer =  new java.io.PrintWriter(pathName)
    writer.write(hedder.mkString("\n"))
    val f = scala.io.Source.fromFile("z3py.txt").getLines.toArray.map(_.split(","))
    for(i <- 0 until f.size){
      println(i,f(i)(0))

      if(i == 0){
        val outfile =outputfile1 ++List[String]("s.add(" + f(i)(1) + ")#"+i)++outputfile_fst
        val ys1 = outfile.mkString("\n") + "\n"
        writer.write(ys1)
      }else{
        val outfile =outputfile1 ++List[String]("s.add(" + f(i)(1) + ")#"+i)++outputfile2
        val ys1 = outfile.mkString("\n") + "\n"
        writer.write(ys1)
      }

    }
    writer.close()

  }

  def m()={
    val f1 = scala.io.Source.fromFile("z3py.txt").getLines.toArray.map(_.split(","))
    val f2 = scala.io.Source.fromFile("anstemp.txt").getLines.toArray
    val pathName = "QandA.txt"
    val writer =  new java.io.PrintWriter(pathName)

    for(i <- 0 until f1.size){
      val n = f2(i).split("/").map(_.toDouble)
      var out = 0d
      if(n.size == 2){
        out = n(0) / n(1)
      }else{
        out = n(0)
      }
      writer.write(i+": "+f1(i)(0)+ " ,"+out)
      println(i+": "+f1(i)(0)+ " ,"+out)
    }
    writer.close
  }

}
