% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hubert_segmentation.R
\name{Hubert_segmentation}
\alias{Hubert_segmentation}
\title{Finds the optimal Hubert segmentation of a series x}
\usage{
Hubert_segmentation(x, alpha = 0.05, Kmax = +Inf)
}
\arguments{
\item{x}{a series}

\item{alpha}{the nominal type I error for the segmentation}

\item{Kmax}{maximum order of the segmentation (defaults to +Inf: segmentation goes on as long as some cuts are significant at the alpha level)}
}
\value{
a list providing cuts location, test results and total error associated to the segmentation of the series
}
\description{
Finds the optimal Hubert segmentation of a series x
}
\examples{
x=c(rnorm(130,3,2),
    rnorm(220,10,2),
    rnorm(500,2,2),
    rnorm(50,-4,2),
    rnorm(30,2,2),
    rnorm(120,8,2))
seg_obj=Hubert_segmentation(x)
plot(x)
lines(model_signal(x,seg_obj$locations), col="red",lwd=3)
}
