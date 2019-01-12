package formula


object formula_analy{
  import protcol._
  import Layer._
  import Utilty_formula._
  import Utilty.RichArray._
  def main(args: Array[String]): Unit = {
    analysis()
  }

  def analysis()={
    val (name,result) = load()
    val QUESTION = texload(q1_10_path)
    var MissCorrection = new Array[String](name.size).map(_ => " ")
    val misscounnt = Array.ofDim[Int](20,10)
    for(n <- 0 until  name.size){
      print(name(n)+":")
      for(q <- 0 until result(n).size) {
        for(ans <- 0 until result(n)(q).size){
        //  print(result(n)(q)(ans))
          if(result(n)(q)(ans) == 'x'){

            print(q,ans)
            misscounnt(q)(ans) +=1
            MissCorrection(n) += "("+str(q+1)+","+(ans+1)+") "+QUESTION(q)(ans)+"\n\n"
          }
        }
      }
      println()

    }
    mkprint(MissCorrection,name)
    misscounnt.take(7)

  }



  def mkprint(fn:Array[String],name:Array[String])={

    val pw = new java.io.PrintWriter("tex/misscollection.tex")
    pw.write("""
      \documentclass[a4paper,fleqn,papersize,15pt]{jsarticle}
      \begin{document}
      """)

      for (i <- 0 until fn.size) {
        pw.write(name(i)+"\n\n")

        pw.write(fn(i).toString)
        pw.write("""\clearpage""")
        pw.write("\n")

      }
      pw.write(
        """
        \end{document}
        \end{document}
        """
      )
      pw.close()

    }
    println("finish print")


  }
