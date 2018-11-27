#' This function tests the differences of consecutive segments
#' @param x a series
#' @param v_r the cuts locations in the series
#' @return a test
#' @export
#' @examples
#' x=c(rnorm(100,3,2),rnorm(100,10,2),rnorm(100,2,2),rnorm(100,-4,2),rnorm(100,6,2),rnorm(100,2,2),rnorm(100,8,2))
#' Scheffe_test(x,v_r=c(1,51,101,201,351,401,501,601,701))
Scheffe_test=function(x,v_r,k=length(v_r-1), alpha=0.05){
  myfactor=segment_signal(v_r)
  length_segments=v_r[2:length(v_r)]-v_r[1:(length(v_r)-1)]
  n1=length_segments[1]
  n2=length_segments[2]
  means=tapply(x,myfactor, "mean")
  diffmeans=abs(means[[1]]-means[[2]])
  dfw=Dstat(x,v_r)/length(x)
  Sobs=diffmeans/(sqrt(dfw*(1/n1+1/n2)))
  df2=n1+n2-k
  Scritical=sqrt((k-1)*qf(1-alpha,k-1,df2))
  test=(Sobs>=Scritical)
  return(list(dfw=dfw,test=test))
}
