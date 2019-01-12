package Utilty
object call_py{
  import scala.sys.process.Process

  def plotPy(path:String)={
    Process("ipython src/main/python/myPlot.py "+path).run
  }

  def zplotPy(path:String)={
    Process("ipython src/main/python/zplot.py "+path).run
  }

  def convert2dPy(path:String)={
    Process("ipython src/main/python/convert2.py "+path).run
  }

  def convert3dPy(path:String)={
    Process("ipython src/main/python/convert3.py "+path).run
  }

}
