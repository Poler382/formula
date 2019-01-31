package Utilty

class Stack[T]() {
  var x = List[T]()

  def push(a: T) {
    x = a :: x
  }

  def pop() = {
    var t = x.head
    x = x.tail
    t
  }
  def full()={
    x
  }

  def len() = x.size
  def size() = x.size
  def head() = x.head

  def p(i: Int) = x(i)
  def refer(i: Int) = x(i)

  def reset() {
    x = List[T]()
  }

  def reverse() {x=x.reverse}

}
