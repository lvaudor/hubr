#' Produces a series of segment identifiers based on a vector listing the boundaries of the segments
#' @param seg the boundaries of segments c(i1,i2,....,N) where i1, i2, ... correspond to first elements of each segment and N is the index of the last element
#' @return a series corresponding to segment identifiers
#' @export
#' @examples
#' segment_signal(c(1,40,68,100))
segment_signal=function(seg){
  length_segments=seg[2:length(seg)]-seg[1:(length(seg)-1)]
  segment_signal=rep(1:length(length_segments),length_segments)
  return(segment_signal)
}
