package Seq2Seq
class Attention_Decoder(in: Int, hidden: Int, out: Int, layernum: Int= 1){
  import Utilty.RichArray._
  import Utilty.ML._
  import Utilty.Stack
  import java.util.Date
  import java.io.File
  import scala.io.Source
  import Layer._


  def make_Unit(layernum:Int)={
    var LSTM_Unit = new Stack[LSTM2]()
    var Drop_Unit = new Stack[Dropout]()

    //  DLDLD こんな感じ
    Drop_Unit.push(new Dropout(0.5f))
    Range(0,layernum).map{i =>
      LSTM_Unit.push(new LSTM2(hidden,hidden))
      Drop_Unit.push(new Dropout(0.5f))
    }

    (LSTM_Unit.full.reverse,Drop_Unit.full.reverse)
  }


  val Embedding = new Linear(in,hidden)
  var Unit = make_Unit(layernum)
  var LSTM_Unit = Unit._1
  var Drop_Unit = Unit._2
  val outputlayer = List[Layer](new Affine(hidden,out),new SoftMax())
  var Attention_Unit= new Attention()
  var pre_h = Array.ofDim[Float](out)

  def Set_pre_h(x:Array[Float]) ={
    LSTM_Unit(0).SETpre_h(x)
  }

  def Get_BP_d(layer_number:Int=layernum-1)={
    LSTM_Unit(layer_number).BP_d.head
  }

  def forward(x:Array[Float])={
    val EM = Embedding.forward(x)
    var lstm = EM
    for(i <- 0 until layernum){
      lstm = LSTM_Unit(i).forward(Drop_Unit(i).forward(lstm))
    }
    pre_h = lstm
    forwards(outputlayer,Attention_Unit.Decoder_forward(Drop_Unit(layernum).forward(lstm)))
  }

  def backward(d:Array[Float])={
    var d1 = backwards(outputlayer,d)
    var d2 = Attention_Unit.backward(d1)
    for(i <- layernum-1 to 0 by -1){
      d2 = LSTM_Unit(i).backward(Drop_Unit(i+1).backward(d2))
    }
    Embedding.backward(Drop_Unit(0).backward(d2))

  }
  def update()={
    Embedding.update()
    LSTM_Unit.map(_.update())
    Drop_Unit.map(_.update())
    updates(outputlayer)

  }
  def reset(){
    Embedding.reset()
    LSTM_Unit.map(_.reset())
    Drop_Unit.map(_.reset())
    resets(outputlayer)
  }


  def save(fn: String){

  }

  def load(fn: String){

  }

}
