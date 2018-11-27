#' Calculates the error associated to all possible segments x[s:t] where 1<=s<=t<=N and N is the length of x
#' @param x a series
#' @return a ff matrix M such that M[s,t] corresponds to the error associated with segment x[s,t]
#' @export
#' @examples
#' x=c(rnorm(100,3,2),rnorm(100,10,2),rnorm(100,2,2),rnorm(100,-4,2),rnorm(100,6,2),rnorm(100,2,2),rnorm(100,8,2))
#' calculate_dst(x)
calculate_dst=function(x){
  N=length(x)
  v1=1:N
  v2=cumsum(x)
  v3=cumsum(x^2)
  vmoy=v2/v1
  dst=ff(0,dim=c(N+1,N))
  dst[N+1,]=rep(NA,N)
  dst[1,]=v3-2*vmoy*v2+vmoy^2*v1
  for (i in 2:N){
    new_v1=v1-i+1
    new_v2=c(rep(NA,i-1),v2[i:N]-v2[i-1])
    new_v3=c(rep(NA,i-1),v3[i:N]-v3[i-1])
    new_vmoy=new_v2/new_v1
    new_dst=new_v3-2*new_vmoy*new_v2+new_vmoy^2*new_v1
    dst[i,]=new_dst
  }
  return(dst)
}
