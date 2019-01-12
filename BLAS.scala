package blas

object BLAS{
  def matmul2(X:Array[Float],Y:Array[Float],Z:Array[Float],m:Int,n:Int,k:Int){
    pllblas.BLAS.gemm(X,Y,Z,m,n,k,0,0)
  }

  def matmul(X: Array[Float] ,Y: Array[Float],Z: Array[Float],M: Int,N: Int,K: Int){
    for(i <- 0 until M;j <- 0 until N;k <- 0 until K){
        Z(i*N+j) += X(i*K+k) * Y(N*k+j)
    }
  }

}
