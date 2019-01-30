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

  var pre_h = Array.ofDim[Float](out)
  def GET_Last_h()={
    pre_h
  }

  def forward(x: Array[Float])={
    val EM = Embedding.forward(x)
    var lstm = EM
    for(i <- 0 until layernum){
      lstm = LSTM_Unit(i).forward(Drop_Unit(i).forward(lstm))
    }
    pre_h = lstm
    Drop_Unit(layernum).forward(lstm)
  }
  def setBP_d(d: Array[Float])={
    LSTM_Unit(layernum-1).BP_d.pop
    LSTM_Unit(layernum-1).BP_d.push(d)
  }
  def backward(d: Array[Float])={
    var lstm_back = d
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
  }
  def load(fn: String){
  }
}

class Decoder(in: Int, hidden: Int, out: Int, layernum: Int= 1){
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
    Drop_Unit.push(new Dropout(0.2f))
    Range(0,layernum).map{i =>
      LSTM_Unit.push(new LSTM2(hidden,hidden))
      Drop_Unit.push(new Dropout(0.2f))
    }

    (LSTM_Unit.full.reverse,Drop_Unit.full.reverse)
  }


  val Embedding = new Linear(in,hidden)
  var Unit = make_Unit(layernum)
  var LSTM_Unit = Unit._1
  var Drop_Unit = Unit._2
  val outputlayer = List[Layer](new Affine(hidden,out),new SoftMax())

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
    forwards(outputlayer,Drop_Unit(layernum).forward(lstm))
  }

  def backward(d:Array[Float])={
    var d2 = backwards(outputlayer,d)

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

class Peeky_Decoder(in: Int,encoder_hidden: Int, hidden: Int, out: Int, layernum: Int= 1){
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
      LSTM_Unit.push(new LSTM2(hidden+encoder_hidden,hidden))
      Drop_Unit.push(new Dropout(0.5f))
    }

    (LSTM_Unit.full.reverse,Drop_Unit.full.reverse)
  }


  val Embedding = new Linear(in,hidden)
  var Unit = make_Unit(layernum)
  var LSTM_Unit = Unit._1
  var Drop_Unit = Unit._2
  val outputlayer = List[Layer](new Affine(hidden + encoder_hidden,out),new SoftMax())

  var pre_h = Array.ofDim[Float](encoder_hidden)
  var Peeky_encoder_h = Array.ofDim[Float](encoder_hidden)

  def Set_pre_h(x:Array[Float]) ={
    LSTM_Unit(0).SETpre_h(x)
    Peeky_encoder_h= x
  }
  def Get_peeky_encoder_h() = Peeky_encoder_h

  def Get_BP_d(layer_number:Int=layernum-1)={
    LSTM_Unit(layer_number).BP_d.head
  }

  def forward(x:Array[Float])={
    val EM = Embedding.forward(x)
    var lstm = EM
    for(i <- 0 until layernum){
      lstm = LSTM_Unit(i).forward(
        Drop_Unit(i).forward(lstm++Peeky_encoder_h)
      )
    }
    pre_h = lstm

    forwards(
      outputlayer,
      Drop_Unit(layernum).forward(lstm++Peeky_encoder_h)
    )
  }
  var encoder_backward_hs = Array.ofDim[Float](encoder_hidden)
  def backward(d:Array[Float])={
    var d2   = backwards(outputlayer,d)
    var d2_1 = d2.take(d2.size/2)
    encoder_backward_hs += d2.drop(d2.size/2)

    for(i <- layernum-1 to 0 by -1){
      var l2 = LSTM_Unit(i).backward(Drop_Unit(i+1).backward(d2_1))
      d2_1 = l2.take(l2.size/2)
      encoder_backward_hs += l2.drop(l2.size/2)
    }

    Embedding.backward(Drop_Unit(0).backward(d2_1))

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
