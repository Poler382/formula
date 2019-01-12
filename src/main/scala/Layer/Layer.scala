package Layer
abstract class Layer {
  type T = Float
  var is_test = false
  var is_CAM  = false
  def forward(x: Array[T]): Array[T]
  def backward(x: Array[T]): Array[T]
  def forward(xs: Array[Array[T]]): Array[Array[T]] = {
    xs.map(forward)
  }
  def backward(ds: Array[Array[T]]): Array[Array[T]] = {
    ds.reverse.map(backward).reverse
  }
  def update():Unit
  def reset():Unit
  def save(fn: String){}
  def load(fn: String){}
}

object kata{
 type T = Float
  //すべてのファイルにimportする
}
