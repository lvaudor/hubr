#' Total sum of squares
#' @param x a series
#' @param x a series
#' @return a total sum of squares
#' @export
#' @examples
#' x=c(rnorm(100,3,2),rnorm(100,10,2),rnorm(100,2,2),rnorm(100,-4,2),rnorm(100,6,2),rnorm(100,2,2),rnorm(100,8,2))
Dstat=function(x,seg){
 return(sum((x-model_signal(x,seg))^2))
}
