package Utilty
object RichArray {
  implicit class RichArray(val x:Array[Float]) extends AnyVal {

    def + (y:Float) = Range(0, x.size).map(i => x(i) + y).toArray
    def - (y:Float) = Range(0, x.size).map(i => x(i) - y).toArray
    def * (y:Float) = Range(0, x.size).map(i => x(i) * y).toArray
    def / (y:Float) = Range(0, x.size).map(i => x(i) / y).toArray
    def + (y:Array[Float]) = Range(0, x.size).map(i => x(i) + y(i)).toArray
    def - (y:Array[Float]) = Range(0, x.size).map(i => x(i) - y(i)).toArray
    def * (y:Array[Float]) = Range(0, x.size).map(i => x(i) * y(i)).toArray
    def / (y:Array[Float]) = Range(0, x.size).map(i => x(i) / y(i)).toArray
    def dot(y:Array[Float]) = (x * y).sum

    def transpose(height:Int, width:Int) = {
      val output = Array.ofDim[Float](height*width)
      for(i <- 0 until width)
      for(j <- 0 until height)
      output(i*height+j) = x(j*width+i)
      output
    }


  }

  implicit class RichArray2D(val xs:Array[Array[Float]]) extends AnyVal{

    def + (ys:Array[Array[Float]]) =
      xs.zip(ys).map{
        case (a, b) => a + b
      }

      def - (ys:Array[Array[Float]]) =
        xs.zip(ys).map{
          case (a, b) => a - b
        }

        def * (ys:Array[Array[Float]]) =
          xs.zip(ys).map{
            case (a, b) => a - b
          }

          def / (ys:Array[Array[Float]]) =
            xs.zip(ys).map{
              case (a, b) => a / b
            }

            def transpose = {
              val output = Array.ofDim[Float](xs(0).size*xs.size)
              for(i <- 0 until xs(0).size)
              for(j <- 0 until xs.size)
              output(i*xs.size+j) = xs(j)(i)
              output
            }

            def separate(size:Int)={
              //Array(Array(x1,x2,..),(y1,y2,..)) => Array(Array(x1,y1,..),Array(x2,y2,...))
              var ReAr = Array.ofDim[Float](size,xs.size)
              for(i <- 0 until size;j<- 0 until xs.size){
                ReAr(i)(j) = xs(j)(i)
              }
            }
          }
        }

        object RichCal{
          implicit class RichIntTaple(val x:(Int,Int) )extends AnyVal {
            def + (y:(Int,Int)) = (x._1 + y._1, x._2 + y._2)
            def - (y:(Int,Int)) = (x._1 - y._1, x._2 - y._2)
            def * (y:(Int,Int)) = (x._1 * y._1, x._2 * y._2)
            def / (y:(Int,Int)) = (x._1 / y._1, x._2 / y._2)
            def getone () = x._1
            def gettwo () = x._2

          }
          implicit class RichFloatTaple(val x:(Float,Float) )extends AnyVal {
            def + (y:(Float,Float)) = (x._1 + y._1, x._2 + y._2)
            def - (y:(Float,Float)) = (x._1 - y._1, x._2 - y._2)
            def * (y:(Float,Float)) = (x._1 * y._1, x._2 * y._2)
            def / (y:(Float,Float)) = (x._1 / y._1, x._2 / y._2)
            def getone () = x._1
            def gettwo () = x._2

          }
          implicit class RichDoubleTaple(val x:(Double,Double) )extends AnyVal {
            def + (y:(Double,Double)) = (x._1 + y._1, x._2 + y._2)
            def - (y:(Double,Double)) = (x._1 - y._1, x._2 - y._2)
            def * (y:(Double,Double)) = (x._1 * y._1, x._2 * y._2)
            def / (y:(Double,Double)) = (x._1 / y._1, x._2 / y._2)
            def getone () = x._1
            def gettwo () = x._2

          }
        }
