#' Calculates the fitted values of the segmentation model
#' @param x a series of quantitative values
#' @param seg a series of segment identifiers
#' @return a series where fitted values correspond to mean per segment
#' @export
#' @examples
#' x=c(rnorm(100,3,2),rnorm(100,10,2),rnorm(100,2,2),rnorm(100,-4,2),rnorm(100,6,2),rnorm(100,2,2),rnorm(100,8,2))
#' y=model_signal(x,seg=c(1,300,701))
#' plot(x)
#' lines(y, col="red")
model_signal=function(x,seg){
  length_segments=seg[2:length(seg)]-seg[1:(length(seg)-1)]
  myfactor=segment_signal(seg)
  means=as.vector(tapply(x,myfactor,"mean"))
  xbis=rep(means,length_segments)
  return(xbis)
}
