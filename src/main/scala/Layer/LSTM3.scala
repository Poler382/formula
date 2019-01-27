package Layer
class LSTM3(
  val Inputsize:Int,
  val Outputsize:Int
)extends Layer{
  import Utilty.RichArray._
  import Activation._
  import Utilty.ML._
  import Utilty.Stack


  val hiddenAfiine = new Affine(Inputsize,Outputsize)
  val hiddenLinear = new Linear(Outputsize,Outputsize)
  val inputAfiine = new Affine(Inputsize,Outputsize)
  val inputLinear = new Linear(Outputsize,Outputsize)
  val outputAfiine = new Affine(Inputsize,Outputsize)
  val outputLinear = new Linear(Outputsize,Outputsize)
  val forgetAfiine = new Affine(Inputsize,Outputsize)
  val forgetLinear = new Linear(Outputsize,Outputsize)
  val hiddenActivation = new Tanh()
  val inputActivation = new Sigmoid()
  val forgetActivation = new Sigmoid()
  val outputActivation = new Sigmoid()
  val CActivation = new Tanh()



  //saves
  var pre_hidden = new Stack[Array[Float]]()
  var pre_input  = new Stack[Array[Float]]()
  var pre_output = new Stack[Array[Float]]()
  var pre_forget = new Stack[Array[Float]]()
  var pre_C      = new Stack[Array[Float]]()
  pre_C.push(new Array[Float](Outputsize))
  var pre_C_tanh = new Stack[Array[Float]]()
  pre_C_tanh.push(new Array[Float](Outputsize))

  var pre_Y      = new Stack[Array[Float]]() // 一個前の出力
  pre_Y.push(new Array[Float](Outputsize))



  def GETpre_Y() = pre_Y.head
  def SETpre_Y(set:Array[Float]){
    pre_Y.pop
    pre_Y.push(set)
  }
  def hidden_forward(x:Array[Float],h:Array[Float]) ={
    hiddenActivation.forward(hiddenAfiine.forward(x) + hiddenLinear.forward(h))
  }
  def input_forward(x:Array[Float],h:Array[Float])={
    inputActivation.forward(inputAfiine.forward(x) + inputLinear.forward(h))
  }
  def output_forward(x:Array[Float],h:Array[Float])={
    outputActivation.forward(outputAfiine.forward(x) + outputLinear.forward(h))
  }
  def forget_forward(x:Array[Float],h:Array[Float])={
    forgetActivation.forward(forgetAfiine.forward(x) + forgetLinear.forward(h))
  }
  def hidden_backward(d:Array[Float]) ={
    val a =hiddenActivation.backward(d)
    (hiddenAfiine.backward(a),hiddenLinear.backward(a))
  }
  def input_backward(d:Array[Float])={
    val a = inputActivation.backward(d)
    (inputAfiine.backward(a),inputLinear.backward(a))
  }
  def output_backward(d:Array[Float])={
    val a = outputActivation.backward(d)
    (outputAfiine.backward(a),outputLinear.backward(a))
  }
  def forget_backward(d:Array[Float])={
    val a = forgetActivation.backward(d)
    (forgetAfiine.backward(a),forgetLinear.backward(a))
  }

  def forward(x: Array[Float]): Array[Float]={
    val hidden = hidden_forward(x,pre_Y.head)
    val input = input_forward(x,pre_Y.head)
    val output = output_forward(x,pre_Y.head)
    val forget = forget_forward(x,pre_Y.pop)

    val left = hidden * input
    val right = pre_C.head * forget
    val C = left + right
    val C_tanh = CActivation.forward(C)
    val y = C_tanh * output

    pre_C_tanh.push(C_tanh)
    pre_hidden.push(hidden)
    pre_input.push(input)
    pre_output.push(output)
    pre_forget.push(forget)
    pre_C.push(C)
    pre_Y.push(y) // 一個前の出力

    y
  }

  val backPropagetion_dh =  new Stack[Array[Float]]()
  backPropagetion_dh.push(new Array[Float](Outputsize))

  val backPropagetion_C =  new Stack[Array[Float]]()
  backPropagetion_C.push(new Array[Float](Outputsize))


  def backward(d: Array[Float]): Array[Float]={
    val ds = d + backPropagetion_dh.pop

    val backPropagetion_output = pre_C_tanh.pop * ds

    val dc = CActivation.backward(ds * pre_output.pop) + backPropagetion_C.pop
    val left = dc.clone
    val right = dc.clone

    backPropagetion_C.push(right * pre_forget.pop)
    val backPropagetion_forget = right * pre_C.pop
    val backPropagetion_input = pre_hidden.pop * left
    val backPropagetion_hidden = pre_input.pop * left

    val (hidden_h,hidden_dx) = hidden_backward(backPropagetion_hidden)
    val (input_h,input_dx) = input_backward(backPropagetion_input)
    val (output_h,output_dx) = output_backward(backPropagetion_output)
    val (forget_h,forget_dx) = forget_backward(backPropagetion_forget)

    backPropagetion_dh.push( hidden_h + input_h +  output_h + forget_h )

    hidden_dx + input_dx +  output_dx + forget_dx

  }
  override def forward(xs: Array[Array[Float]]): Array[Array[Float]] = {
    xs.map(forward(_))
  }
  override def backward(ds: Array[Array[Float]]): Array[Array[Float]] = {
    ds.map(backward(_))
  }
  def update(){
    hiddenAfiine.update()
    hiddenLinear.update()
    inputAfiine.update()
    inputLinear.update()
    outputAfiine.update()
    outputLinear.update()
    forgetAfiine.update()
    forgetLinear.update()
  }
  def reset(){
    hiddenAfiine.reset()
    hiddenLinear.reset()
    inputAfiine.reset()
    inputLinear.reset()
    outputAfiine.reset()
    outputLinear.reset()
    forgetAfiine.reset()
    forgetLinear.reset()
  }
  override def save(fn: String){


  }
  override def load(fn: String){


  }


}
