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

  def TsneAndPcaPy(path:String,in:String,out:String,outfn:String)={
    val hikisuu = String.format("%1s %1s %1s %1s",path,in,out,outfn)
    Process("ipython src/main/python/tsne.py "+hikisuu).run
  }

  def TsneAndPcaPy0(path:String,in:String,out:String,outfn:String,ol:String,title:String)={
    val hikisuu = String.format("%1s %1s %1s %1s %1s %1s",path,in,out,outfn,ol,title)
    Process("ipython src/main/python/tsne0.py "+hikisuu).run
  }

}
