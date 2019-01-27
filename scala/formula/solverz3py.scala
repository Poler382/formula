package formula

object z3py{
  import formula.protcol._
  import Layer._
  import formula.Utilty_formula._
  def getkakko(i:Int,p:String)={
    val x1 = p.tail
    val end = x1.indexOf(")")
    val q = x1.take(end)
    var j = 0
    var output = ""
    var pre = ""
    var keisu = ""

    while(j < q.size){

      if( Decision_num(q(j))){//二桁以上の数に対応　
        output += q(j)
      }
      if( Decision_num(pre) && q(j) == 'x'){//一個前が数字で今が文字の時
        output += "x * "+keisu
      }

      if( "+-*/".contains(q(j))){
        //println("five")
        output += " "+q(j)+" "
      }
      pre = q(j).toString
      j+=1
    }
    (i+end+1,output)
  }
  def Decision_num(a:Char)={
    if(a >= '0' && a <= '9') true
    else false
  }
  def Decision_num(a:String )={
    if(a >= "0" && a <= "9") true
    else false
  }

  def OutofIndex(i : Int)={
    if(i == -1) 100
    else i
  }

  def main(args: Array[String]): Unit = {

    val Q = question_load3(collection_path).map(_.replace("f",""))
    var L = List[String]()
    for(i <- 0 until Q.size){
      var keisu = ""
      var fugou = ""
      var small = ""
      val q = Q(i)
      var output = ""
      var pre = ""
      val str =  System.currentTimeMillis()
      var time = 0d
      var j = 0 // pointer
      println(i)
      println("in  -> "+q)
      while(j < q.size && time < 3000d){
        //  println(j)
        if( pre == "" && Decision_num(q(j)) && q(j+1) !='.'  ){
          keisu += q(j)+ " "
          //    println("one/ num->"+ j)
        }else if( (pre == "" || pre =="}" || pre == ")")&& q(j)== 'x' ){
          output += q(j)+ " "
          //    println("one_dash/ num->"+ j)

        }else if(Decision_num(pre) && Decision_num(q(j)) ){
          keisu += q(j)
          //    println("two/ num->"+ j)
        }else if( ( Decision_num(pre) || "+-".contains(pre)) && q(j) == 'x'){
          output += keisu + " * x "
          keisu = ""
          //  println("three/ num->"+ j)
        }else if(Decision_num(pre) && q(j) == '.' && Decision_num(q(j+1)) ) {
          keisu = pre +"."+q(j+1).toString
          j += 1// 小数点の後ろに行く
          //      println("fore/ num->"+ j)

        }else if( "+-".contains(q(j)) ){
          fugou = ""
          fugou = q(j).toString
          //    println("five/ num->"+ j)

        }else if( "+-".contains(pre) && Decision_num(q(j)) && q.size -1 != j){
          keisu = fugou + q(j)
          fugou = ""
          //    println("six/ num->"+ j)

        }else if( "+-".contains(pre) && Decision_num(q(j)) && q.size -1 == j){
          output += fugou + q(j)
          fugou = ""
          //          println("final_six/ num->"+ j)

        }else if( q(j) == '{' && Decision_num(pre) ){

          val (out, pointer) = getfraction(j,q.drop(j))
          output += fugou + pre + "* ( " + out + " ) "
          j = pointer
          fugou = ""
          //    println("seven/ num->"+ j)

        }else if( q(j) == '{' && !Decision_num(pre)){

          val (out, pointer) = getfraction(j,q.drop(j))
          output += fugou + " ( " + out + " ) "
          j = pointer
          fugou = ""
          //    println("eight/ num->"+ j)

        }else if (q(j) == '(' && Decision_num(pre)) {

          val (pointer , out) = getkakko(j,q.drop(j))
          j = pointer
          output +=  keisu + " * ("+out+")"
          keisu = ""
          //    println("nine/ num->"+ j)

        }else if (q(j) == '(' && !Decision_num(pre)) {

          val (pointer , out) = getkakko(j,q.drop(j))
          j = pointer
          output +=  fugou + " * ("+out+")"
          keisu = ""
          fugou = ""
          //          println("ten / num->"+ j)

        }else if(q(j) == '=' && Decision_num(pre)){
          output += keisu + " == "
          keisu = ""
          //  println("eleven/ num->"+ j)

        }else if(j == q.size -1 && keisu == ""){
          output += q(j)
          //  println("twelve/ num->"+ j)
        }else if (j == q.size -1 && keisu != ""){
          output += keisu+q(j)
          //  println("theerteen/ num->"+ j)
        }else{
          //  println(q(j) +"is not found / last num->"+ j)
        }


        j = OutofIndex(j)
        if(j <= q.size -1){
          pre = q(j).toString
        }
        time =  System.currentTimeMillis() - str
        j+=1
      }
      if(output.size < 50){
        println("out -> "+output.replace("  "," "))
        L ::= q+","+output
      }else{
        println("out -> "+output.replace("  "," "))
        L ::= q+","
      }

    }
    val pathName = "z3py.txt"
    val writer =  new java.io.PrintWriter(pathName)
    val ys1 = L.reverse.mkString("\n") + "\n"
    writer.write(ys1)
    writer.close()
    println("success ")
  }
  def getfraction(i:Int,q:String) = {
    val x = q.drop(i)
    val son_start = x.indexOf('{')
      val son_end = x.indexOf('}')
      var mother = x.drop(son_end+1)
      var mother_start = son_end + mother.indexOf("{")+1
        var mother_end = son_end + mother.indexOf("}")+1

        var son = x.take(son_end).drop(son_start+1)
        var mot = x.take(mother_end).drop(mother_start+1)
        var fraction = son.toString + " / " + mot.toString
        (fraction,i+mother_end)
      }

      def tray(q:String)={



        var keisu = ""
        var fugou = ""
        var small = ""
        var output = ""
        var pre = ""

        var j = 0 // pointer
        println("in  -> "+q)
        while(j < q.size){
          println(j)
          if( pre == "" && Decision_num(q(j)) && q(j+1) !='.'  ){
            keisu += q(j)+ " "
            //    println("one/ num->"+ j)
          }else if( (pre == "" || pre =="}" || pre == ")")&& q(j)== 'x' ){
            output += q(j)+ " "
            //    println("one_dash/ num->"+ j)

          }else if(Decision_num(pre) && Decision_num(q(j)) ){
            keisu += q(j)
            //    println("two/ num->"+ j)
          }else if( ( Decision_num(pre) || "+-".contains(pre)) && q(j) == 'x'){
            output += keisu + " * x "
            keisu = ""
            //  println("three/ num->"+ j)
          }else if(Decision_num(pre) && q(j) == '.' && Decision_num(q(j+1)) ) {
            keisu = pre +"."+q(j+1).toString
            j += 1// 小数点の後ろに行く
            //      println("fore/ num->"+ j)

          }else if( "+-".contains(q(j)) ){
            fugou = ""
            fugou = q(j).toString
            //    println("five/ num->"+ j)

          }else if( "+-".contains(pre) && Decision_num(q(j)) && q.size -1 != j){
            keisu = fugou + q(j)
            fugou = ""
            //    println("six/ num->"+ j)

          }else if( "+-".contains(pre) && Decision_num(q(j)) && q.size -1 == j){
            output += fugou + q(j)
            fugou = ""
            //          println("final_six/ num->"+ j)

          }else if( q(j) == '{' && Decision_num(pre) ){

            val (out, pointer) = getfraction(j,q.drop(j))
            output += fugou + pre + "* ( " + out + " ) "
            j = pointer
            fugou = ""
            //    println("seven/ num->"+ j)

          }else if( q(j) == '{' && !Decision_num(pre)){

            val (out, pointer) = getfraction(j,q.drop(j))
            output += fugou + " ( " + out + " ) "
            j = pointer
            fugou = ""
            //    println("eight/ num->"+ j)

          }else if (q(j) == '(' && Decision_num(pre)) {

            val (pointer , out) = getkakko(j,q.drop(j))
            j = pointer
            output +=  keisu + " * ("+out+")"
            keisu = ""
            //    println("nine/ num->"+ j)

          }else if (q(j) == '(' && !Decision_num(pre)) {

            val (pointer , out) = getkakko(j,q.drop(j))
            j = pointer
            output +=  fugou + " * ("+out+")"
            keisu = ""
            fugou = ""
            //          println("ten / num->"+ j)

          }else if(q(j) == '=' && Decision_num(pre)){
            output += keisu + " == "
            keisu = ""
            //  println("eleven/ num->"+ j)

          }else if(j == q.size -1 && keisu == ""){
            output += q(j)
            //  println("twelve/ num->"+ j)
          }else if (j == q.size -1 && keisu != ""){
            output += keisu+q(j)
            //  println("theerteen/ num->"+ j)
          }else{
            //  println(q(j) +"is not found / last num->"+ j)
          }


          j = OutofIndex(j)
          if(j <= q.size -1){
            pre = q(j).toString
          }

          j+=1

        }
        println("out -> "+output.replace("  "," "))

      }







    }
