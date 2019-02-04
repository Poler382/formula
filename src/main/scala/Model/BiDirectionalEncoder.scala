package Seq2Seq
class Encoder(in: Int,hidden: Int,out: Int,layernum: Int=1){
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._

  def make_Unit(layernum:Int)={
    var feedforwardLSTM_Unit = new Stack[LSTM2]()
    var backforwardLSTM_Unit = new Stack[LSTM2]()

    var feedforwardDrop_Unit = new Stack[Dropout]()
    var backforwardDrop_Unit = new Stack[Dropout]()

    //  DLDLD こんな感じ
    Drop_Unit.push(new Dropout(0.2f))
    Range(0,layernum).map{i =>
      feedforwardLSTM_Unit.push(new LSTM2(hidden,hidden/2))
      feedforwardDrop_Unit.push(new Dropout(0.2f))
      backforwardLSTM_Unit.push(new LSTM2(hidden,hidden/2))
      backforwardDrop_Unit.push(new Dropout(0.2f))
    }

    (feedforwardLSTM_Unit.full.reverse,feedforwardDrop_Unit.full.reverse,backforwardLSTM_Unit.full.reverse,backforwardDrop_Unit.full.reverse)
  }

  val Embedding = new Affine(in,hidden)
  var Unit = make_Unit(layernum)
  var feedforwardLSTM_Unit = Unit._1
  var feedforwardDrop_Unit = Unit._2
  var backforwardLSTM_Unit = Unit._3
  var backforwardDrop_Unit = Unit._4

  var pre_h = Array.ofDim[Float](out)
  def GET_Last_h()={
    pre_h
  }
  //入力を一気に受け取る(batchのように)

  def forward(x: Array[Array[Float]])={
    var EM = Embedding.forward(x)
    var lstm = EM
    for(i <- 0 until layernum){
      val feedStack = new Stack[Array[Float]]()
      val backStack = new Stack[Array[Float]]()

      for(j <- 0 until x.size){
        feedStack.push(feedforwardDrop_Unit(i).forward(feedforwardLSTM_Unit(i).forward(lstm(j))))
        backStack.push(backforwardDrop_Unit(i).forward(backforwardLSTM_Unit(i).forward(lstm(x.size-1 -j))))
      }
      val fs = feedStack.reverse.full.toArray
      val bs = backStack.full.toArray
      Range(0,lstm.size).map{ i=>
         lstm(i) = fs(i) ++ bs(i)
      }
      feedStack.reset
      backStack.reset

    }

    pre_h = lstm
    pre_h
  }
  def setBP_d(d: Array[Float])={
    feedforwardLSTM_Unit(layernum-1).BP_d.pop
    backforwardLSTM_Unit(layernum-1).BP_d.pop
    feedforwardLSTM_Unit(layernum-1).BP_d.push(d)
    backforwardLSTM_Unit(layernum-1).BP_d.push(d)

  }
  def backward(d: Array[Float])={
    var lstm_back = d
    val feedStack = new Stack[Array[Float]]()
    val backStack = new Stack[Array[Float]]()

    for(i <- layernum-1 to 0 by -1){
      lstm_back = LSTM_Unit(i).backward(Drop_Unit(i+1).backward(lstm_back))
    }
    Embedding.backward(Drop_Unit(0).backward(lstm_back))
  }
  def update()={
    Embedding.update()
    LSTM_Unit.map(_.update())
    Drop_Unit.map(_.update())
  }
  def reset(){

    Embedding.reset()
    LSTM_Unit.map(_.reset())
    Drop_Unit.map(_.reset())
  }
  def save(fn: String){
    Embedding.save("biasdata/Embedding_"+fn+"_"+hidden+"x"+hidden+".txt")
    Range(0,LSTM_Unit.size-1).map{i => LSTM_Unit(i).save("biasdata/LSTM_Unit"+hidden+"x"+hidden+"_"+i+".txt")}

  }
  def load(fn: String){
    Embedding.load("biasdata/Embedding_"+fn+"_"+hidden+"x"+hidden+".txt")
    Range(0,LSTM_Unit.size-1).map{i => LSTM_Unit(i).load("biasdata/LSTM_Unit"+hidden+"x"+hidden+"_"+i+".txt")}

  }

  def Embedding_load(path:String)={
    Embedding.load(path)
  }
}
