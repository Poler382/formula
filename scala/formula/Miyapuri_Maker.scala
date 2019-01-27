package formula

object miyapuri_maker{
  import formula.protcol._
  import Utilty.ML._
  import Layer._
  def load(dir:String) =  scala.io.Source.fromFile(dir).getLines.toArray.mkString("\n")

  def main2(args: Array[String]): Unit = {
    val QuestionList =  List[String]()

    val formula = load_file(question_path)
    val context = load_file(question2_path).map(_.split(":").toArray).toArray

    val formulasize = formula.size
    val contextsize = context.size

    val path = "tex/"
    val pathName = "tex/question"+args(0)+".tex"
    val writer = new java.io.PrintWriter(pathName)

    var q_paperList = List[String]()

    val cheader = load(path+"common_header.tex")
    val pheader = load(path+"page_header.tex")
    val cfooter = load(path+"common_footer.tex")
    val pfooter = load(path+"page_footer.tex")
    q_paperList ::= cheader

    val rand1 = new scala.util.Random(5)
    val rand2 = new scala.util.Random(5)

    var formula_index =  rand1.shuffle(Range(0,formulasize).toList).toList
    var context_index =  rand1.shuffle(Range(0,contextsize).toList).toList

    for(one <- 0 until 10){
      q_paperList ::= pheader
      var ys = List[String]()
      val formula_index_t =  formula_index.take(7)
      val context_index_t =  context_index.take(3)
      formula_index = formula_index.drop(7)
      context_index = context_index.drop(3)


      for(i <-  0 until 7 ){
        ys ::= """\item $""" +formula(formula_index_t(i)) + """$ \begin{flushright}\textcolor{red}{\framebox[8em]{\rule{0pt}{6ex}}\end{flushright}} %"""+ formula_index_t(i)
      }

      for(i <- 0 until 3){
        val c = context(context_index_t(i))
        ys ::= """\item """ + c(0)+ """ \vfill \begin{flushright}\textcolor{red}{\framebox[8em]{\rule{0pt}{6ex}}\end{flushright}} %"""+ context_index_t(i)
        if(c.size != 1 ){
          ys ::= """\begin{enumerate}"""
        }

        for(j <- 1 until c.size){
          ys ::= """\item """ + c(j)+ """ \vfill \begin{flushright}\textcolor{red}{\framebox[8em]{\rule{0pt}{6ex}}\end{flushright}} %"""+ context_index_t(i)
        }
        if(c.size != 1){
          ys ::= """\end{enumerate}"""
        }

      }


      q_paperList ::= ys.reverse.mkString("\n")
      q_paperList ::= pfooter


    }
    q_paperList ::= cfooter



    writer.write(q_paperList.reverse.mkString("\n")+"\n")
    writer.write(cfooter)
    writer.close()

  }



    def main(args: Array[String]): Unit = {
      val QuestionList =  List[String]()

      val formula = load_file(question_path)
      val context = load_file(question2_path).map(_.split(":").toArray).toArray

      val formulasize = formula.size
      val contextsize = context.size

      val path = "tex/"
      val pathName = "tex/question"+args(0)+".tex"
      val writer = new java.io.PrintWriter(pathName)

      var q_paperList = List[String]()

      val cheader = load(path+"common_header.tex")
      val pheader = load(path+"page_header.tex")
      val cfooter = load(path+"common_footer.tex")
      val pfooter = load(path+"page_footer.tex")
      q_paperList ::= cheader

      val rand1 = new scala.util.Random(5)
      val rand2 = new scala.util.Random(5)

      var formula_index =  rand1.shuffle(Range(0,formulasize).toList).toList
      var context_index =  rand1.shuffle(Range(0,contextsize).toList).toList

      for(one <- 0 until 10){
        q_paperList ::= pheader
        var ys = List[String]()
        val formula_index_t =  formula_index.take(10)
        val context_index_t =  context_index.take(3)
        formula_index = formula_index.drop(10)
        context_index = context_index.drop(3)


        for(i <-  0 until 10 ){
          ys ::= """\item $""" +formula(formula_index_t(i)) + """$ \begin{flushright}\textcolor{red}{\framebox[8em]{\rule{0pt}{6ex}}\end{flushright}} \vfill %"""+ formula_index_t(i)
        }

        q_paperList ::= ys.reverse.mkString("\n")
        q_paperList ::= pfooter


      }
      q_paperList ::= cfooter



      writer.write(q_paperList.reverse.mkString("\n")+"\n")
      writer.write(cfooter)
      writer.close()

    }
  def load_file(path:String)={
    scala.io.Source.fromFile(path).getLines.toArray
  }


}
