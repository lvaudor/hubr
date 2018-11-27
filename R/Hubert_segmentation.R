#' Finds the optimal Hubert segmentation of a series x
#' @param x a series
#' @param alpha the nominal type I error for the segmentation
#' @param Kmax maximum order of the segmentation (defaults to +Inf: segmentation goes on as long as some cuts are significant at the alpha level)
#' @return a list providing cuts location, test results and total error associated to the segmentation of the series
#' @export
#' @examples
#' x=c(rnorm(130,3,2),
#'     rnorm(220,10,2),
#'     rnorm(500,2,2),
#'     rnorm(50,-4,2),
#'     rnorm(30,2,2),
#'     rnorm(120,8,2))
#' seg_obj=Hubert_segmentation(x)
#' plot(x)
#' lines(model_signal(x,seg_obj$locations), col="red",lwd=3)
Hubert_segmentation=function(x,alpha=0.05,Kmax=+Inf){
  dst=calculate_dst(x)
  N=length(x)
  c_1=dst[1,]        #vecteurs c_...= vecteurs des couts minimaux
  z_1=rep(0,N)       #vecteurs z_...= vecteurs des elements pour lesquels rupture a un cout minimal
  K=2
  mytest=list(test=TRUE)
  while (K<=Kmax & mytest$test==TRUE){
    c_Km1=get(paste("c_",K-1,sep=""))              #on recupere les couts minimaux a l'ordre precedent
    c_K=c(0)
    z_K=c(0)
    for (s in 2:N){
      ets=c(NA,c_Km1)+dst[,s]
      c_K=c(c_K,min(ets[2:(s-1)],na.rm=T))    #pour chaque s-ieme element, quel est le cout minimal d'une rupture precedente?
      elem=which(ets[2:(s-1)]==min(ets[2:(s-1)],na.rm=T))
      elem=elem[length(elem)]
      z_K=c(z_K,elem)         #pour chaque s-ieme elment, quel est l'element precedent pour lequel le cout d'une rupture est minimal?
    }
    #print(K)
    assign(paste("c_",K,sep=""),c_K)               #on stocke le nouveau vecteur des couts minimaux a l'ordre K
    assign(paste("z_",K,sep=""),z_K)               #on stocke le nouveau vecteur des elements pour lesquels le cout d'une rupture est minimal ? l'ordre K
    if(K==2){tKm1=c(0,N)};if(K>2){tKm1=tK}
    #a l'ordre 1 la segmentation est c(0,N); pour K>=2, on garde en memoire la segmentation a l'ordre K-1
    tK=N
    for (k in K:1){
      z_k=get(paste("z_",k,sep=""))
      tK=c(z_k[tK[1]],tK)
    }
    mytest=test_segmentation(x,tK+1,alpha)         #on sort de la boucle quand test==F
    K=K+1
  }
  mynewtest=test_segmentation(x,tKm1+1,alpha)           #on s'interesse a la segmentation a l'ordre precedent
  return(c(list(locations=tKm1+1),Dstat=mynewtest$Dstat))
}
