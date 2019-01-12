package Layer
class LSTM2(
  val Inputsize:Int,
  val Outputsize:Int
)extends Layer{
  import Utilty.RichArray._
  import Activation._
  import Utilty.ML._
  import Utilty.Stack

  var pre_h = Array.ofDim[Float](Outputsize)
  val hiddenGate = new Tanh()
  val inputGate  = new Sigmoid()
  val forgetGate = new Sigmoid()
  val outputGate = new Sigmoid()
  val CommonLayer = new Affine(Inputsize+Outputsize,Outputsize*4)
  val tanh = new Tanh()


  var Clist = new Stack[Array[Float]]()
  Clist.push(new Array[Float](Outputsize))
  var Ctanh = new Stack[Array[Float]]()

  val h_t =  new Stack[Array[Float]]()
  val i_t =  new Stack[Array[Float]]()
  val f_t =  new Stack[Array[Float]]()
  val o_t =  new Stack[Array[Float]]()
  val fc = new Stack[Array[Float]]()
  val hi = new Stack[Array[Float]]()

  def GETpre_h() = pre_h
  def SETpre_h(set:Array[Float]){
    pre_h = set
  }

  def forward(x: Array[Float]): Array[Float]={
    val common = CommonLayer.forward(x++pre_h).grouped(Outputsize).toArray
    h_t.push(hiddenGate.forward(common(0)))
    i_t.push(inputGate.forward(common(1)))
    f_t.push(forgetGate.forward(common(2)))
    o_t.push(outputGate.forward(common(3)))
    fc.push(Clist.head * f_t.head)
    hi.push(h_t.head * i_t.head)
    val c = (fc.head) + (hi.head)
    Clist.push(c)
    Ctanh.push(tanh.forward(c))
    val h =  Ctanh.head * o_t.head
    pre_h = h

    h
  }

  val BP_C = new Stack[Array[Float]]()
  val BP_d = new Stack[Array[Float]]()
  BP_d.push(new Array[Float](Outputsize))
  BP_C.push(new Array[Float](Outputsize))

  def backward(d: Array[Float]): Array[Float]={
    val a = BP_d.pop
    val b = Ctanh.pop
    val d4 = b * (d + a)
    val dc = tanh.backward(d * o_t.pop) * BP_C.pop

    val dfc = dc * hi.pop
    val dhi = dc * fc.head

    val d3 = forgetGate.backward(fc.pop * Clist.pop)

    val d1 = hiddenGate.backward(dhi * i_t.pop)
    val d2 = inputGate.backward(dhi * h_t.pop)

    BP_C.push(dfc * f_t.pop)

    val dx = CommonLayer.backward(d1++d2++d3++d4)
    BP_d.push(dx.drop(Inputsize))
    dx.take(Inputsize)
  }
  override def forward(xs: Array[Array[Float]]): Array[Array[Float]] = {
    xs
  }
  override def backward(ds: Array[Array[Float]]): Array[Array[Float]] = {
    ds
  }
  def update(){
    CommonLayer.update()
  }
  def reset(){
    CommonLayer.reset()
  }
  override def save(fn: String){
    CommonLayer.save(fn)

  }
  override def load(fn: String){
    CommonLayer.load(fn)

  }


}
