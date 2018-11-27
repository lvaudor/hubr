#' Tests whether all consecutive segments are significantly different
#' @param x a series
#' @param v_r the cuts locations in the series
#' @return a total sum of squares
#' @export
#' @examples
#' x=c(rnorm(100,3,2),rnorm(100,10,2),rnorm(100,2,2),rnorm(100,-4,2),rnorm(100,6,2),rnorm(100,2,2),rnorm(100,8,2))
#' test_segmentation(x, v_r=c(1,301,501,701))
test_segmentation=function(x,v_r,alpha=0.05){
  test=TRUE
  Nelts=length(v_r)
  test2by2=rep(NA,Nelts-2)
  if(Nelts==2){test=FALSE}
  if(Nelts>2){
    for (i in 2:(Nelts-1)){
      x_tmp=x[(v_r[i-1]):(v_r[i+1]-1)]
      v_r_tmp=c(v_r[i-1],v_r[i],v_r[i+1])
      test2by2[i-1]=Scheffe_test(x_tmp,v_r_tmp,Nelts-1, alpha)$test
    }
    if (any(test2by2 ==FALSE|is.na(test2by2))){test=FALSE}
  }
  Dstat_obs=Dstat(x,v_r)
  return(list(test=test,test2by2=test2by2, Dstat=Dstat_obs))
}

