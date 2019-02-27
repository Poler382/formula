object d2p{
  def d()={
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 1 normal > normal22x200l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 4 normal > normal22x200l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 8 normal > normal22x200l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 1 normal > normal22x500l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 4 normal > normal22x500l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 8 normal > normal22x500l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 1 normal > normal22x1000l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 4 normal > normal22x1000l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 8 normal > normal22x1000l8.log &").run

    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 1 Skip > Skip22x200l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 4 Skip > Skip22x200l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 8 Skip > Skip22x200l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 1 Skip > Skip22x500l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 4 Skip > Skip22x500l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 8 Skip > Skip22x500l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 1 Skip > Skip22x1000l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 4 Skip > Skip22x1000l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 8 Skip > Skip22x1000l8.log &").run

    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 1 Bi > Bi22x200l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 4 Bi > Bi22x200l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 200 8 Bi > Bi22x200l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 1 Bi > Bi22x500l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 4 Bi > Bi22x500l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 500 8 Bi > Bi22x500l8.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 1 Bi > Bi22x1000l1.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 4 Bi > Bi22x1000l4.log &").run
    sys.process.Process("java -cp target/scala-2.12/formula-assembly-1.0.0.jar formula.FormulaVecter 50 22 1000 8 Bi > Bi22x1000l8.log &").run

  }

  def main(args: Array[String]): Unit = {
    val fnlist = sys.process.Process("ls txt/new/").lineStream.toArray
    for(fn <- fnlist){
      println(fn)
      if(fn.contains(".txt")){//txt file is true

        val num = fn.split("_").filter(_.contains("22x"))(0).split("x")
        val title = fn.split("_").tail.take(8).mkString("_")
        val t = fn.split("_").tail.take(3).mkString("_")
        println(num(0),num(1),title,t)

        Utilty.call_py.TsneAndPcaPy0("txt/new/"+fn,"20",num(1),"num_"+title,"Ex200.txt",t)
        Thread.sleep(1000)
        Utilty.call_py.TsneAndPcaPy0("txt/new/"+fn,"20",num(1),"formula_"+title,"Ex20.txt",t)
        Thread.sleep(1000)


      }
    }



  }

}
