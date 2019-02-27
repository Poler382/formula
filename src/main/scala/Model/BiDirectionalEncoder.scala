package Seq2Seq
class Bi(in: Int,out: Int,droplate: Float = 0.2f){
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._

  val feedforwardLSTM  = new LSTM2(in,out)
  val feedforwardDrop = new Dropout(droplate)
  val backforwardLSTM = new LSTM2(in,out)
  val backforwardDrop = new Dropout(droplate)
  var pre_h = Array.ofDim[Float](out)

  def GET_Last_h()={
    pre_h
  }

  def setBP_d(d:Array[Float])={
    feedforwardLSTM.BP_d.pop
    feedforwardLSTM.BP_d.push(d)

    backforwardLSTM.BP_d.pop
    backforwardLSTM.BP_d.push(d)
  }

  def forward(X: Array[Array[Float]])={
    val feedStack = new Stack[Array[Float]]()
    val backStack = new Stack[Array[Float]]()

    for(i <- 0 until X.size){
      val feedX = X(i).take(X(i).size/2)
      val backX = X(i).drop(X(i).size/2)
      feedStack.push(feedforwardDrop.forward(feedforwardLSTM.forward(feedX)))
      backStack.push(backforwardDrop.forward(backforwardLSTM.forward(backX)))
    }

    val feed = feedStack.full.reverse
    val back = backStack.full
    var lstm = new Array[Array[Float]](X.size)
    Range(0,X.size).map{ k =>
      lstm(k) = feed(k) ++ back(k)
    }
    lstm
  }
  def backward(D: Array[Array[Float]])={
    val feedStack = new Stack[Array[Float]]()
    val backStack = new Stack[Array[Float]]()

    for(i <- 0 until D.size){
      val feedD = D(i).take(D(i).size/2)
      val backD = D(i).drop(D(i).size/2)
      feedStack.push(feedforwardDrop.backward(feedforwardLSTM.backward(feedD)))
      backStack.push(backforwardDrop.backward(backforwardLSTM.backward(backD)))
    }

    val feed = feedStack.full.reverse
    val back = backStack.full
    var lstm = new Array[Array[Float]](D.size)
    Range(0,D.size).map{ k =>
      lstm(k) = feed(k) ++ back(k)
    }
    lstm
  }

  def update()={
    feedforwardLSTM.update()
    feedforwardDrop.update()
    backforwardLSTM.update()
    backforwardDrop.update()
  }

  def save(fn : String)={
    feedforwardLSTM.save("biasdata/feed"+fn)
    feedforwardDrop.save("biasdata/feed"+fn)
    backforwardLSTM.save("biasdata/back"+fn)
    backforwardDrop.save("biasdata/back"+fn)


  }

  def load(fn : String)={
    feedforwardLSTM.load("biasdata/feed"+fn)
    feedforwardDrop.load("biasdata/feed"+fn)
    backforwardLSTM.load("biasdata/back"+fn)
    backforwardDrop.load("biasdata/back"+fn)
  }
}

class BiDirectionalEncoder(in: Int, hidden: Int, out: Int, layernum: Int=1){
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._

  def make_Unit(layernum:Int)={
    var Bi_Unit = new Stack[Bi]()
    Range(0,layernum).map{i =>
      Bi_Unit.push(new Bi(hidden/2,hidden/2,0.2f))
    }
    Bi_Unit.full.reverse
  }

  val Embedding = new Affine(in,hidden)
  var Bi_Unit = make_Unit(layernum)

  var pre_h = Array.ofDim[Float](out)
  def GET_Last_h()={
    pre_h
  }

  //入力を一気に受け取る(batchのように)

  def forward(x: Array[Array[Float]])={
    var EM = Embedding.forward(x)
    var lstm = EM
    for(i <- 0 until Bi_Unit.size){
      lstm = Bi_Unit(i).forward(lstm)
    }

    pre_h = lstm.last
    pre_h
  }
  def setBP_d(d: Array[Float])={
    Bi_Unit(layernum-1).setBP_d(d)
  }


  def backward(d: Array[Array[Float]])={
    var lstm_back = d

    for(i <- layernum-1 to 0 by -1){
      lstm_back = Bi_Unit(i).backward(lstm_back)
    }

    Embedding.backward(lstm_back)
  }
  def update()={
    Embedding.update()
    Bi_Unit.map(_.update())

  }
  def reset(){
    Embedding.reset()
    Bi_Unit.map(_.update())
  }
  def save(fn: String){
    Embedding.save("biasdata/Embedding_"+fn+"_"+hidden+"x"+hidden+".txt")
    Range(0,Bi_Unit.size-1).map{i => Bi_Unit(i).save("LSTM_Unit"+hidden+"x"+hidden+"_"+i+".txt")}

  }
  def load(fn: String){
    Embedding.load("biasdata/Embedding_"+fn+"_"+hidden+"x"+hidden+".txt")
    Range(0,Bi_Unit.size-1).map{i => Bi_Unit(i).load("LSTM_Unit"+hidden+"x"+hidden+"_"+i+".txt")}

  }

  def Embedding_load(path:String)={
    Embedding.load(path)
  }
}
