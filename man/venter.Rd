\name{venter}
\alias{venter}
\alias{Venter}
\alias{shorth}
%\alias{mlv.dalenius}
%\alias{Dalenius}
%\alias{mlv.lms}
\alias{lms}

\title{ The Venter / Dalenius / LMS Mode Estimator }
\description{
This function computes the Venter mode estimator, also called the Dalenius, or LMS (Least Median Square) mode estimator.
}
\usage{
  venter(x, 
         bw = NULL, 
         k, 
         iter = 1, 
         type = 1, 
         tie.action = "mean", 
         tie.limit = 0.05)
  
  shorth(x, 
         ...)
}
\arguments{
  \item{x}{ numeric. Vector of observations. }
  \item{bw}{ numeric. The bandwidth to be used. Should belong to (0, 1]. See 'Details'. }
  \item{k}{ numeric. See 'Details'. }
  \item{iter}{ numeric. Number of iterations. }
  \item{type}{ numeric or character. The type of Venter estimate to be computed. See 'Details'. }
  \item{tie.action}{ character. The action to take if a tie is encountered. }
  \item{tie.limit}{ numeric. A limit deciding whether or not a warning is given when a tie is encountered. }
  \item{...}{ Further arguments. }
}
\details{
The modal interval, i.e. the shortest interval among intervals containing \code{k+1} observations, is first computed.
The user should either give the bandwidth \code{bw} or the argument \code{k}, 
\code{k} being taken equal to \code{ceiling(bw*n) - 1} if missing. \cr
If \code{type = 1}, the midpoint of the modal interval is returned.
If \code{type = 2}, the \code{floor((k+1)/2)}th element of the modal interval is returned.
If \code{type = 3} or \code{type = "dalenius"}, the median of the modal interval is returned.
If \code{type = 4} or \code{type = "shorth"}, the mean of the modal interval is returned.
If \code{type = 5} or \code{type = "ekblom"}, Ekblom's \eqn{L_{-\infty}}{L_{-infinity}} estimate is returned, see Ekblom (1972).
If \code{type = 6} or \code{type = "hsm"}, the half sample mode (hsm) is computed, see \code{\link{hsm}}.
}
\value{
A numeric value is returned, the mode estimate.
}
\note{
The user should preferentially call \code{venter} through 
 \code{mlv(x, method = "venter", ...)}.
This returns an object of class \code{\link{mlv}}.
}
\references{
  \itemize{
    \item Dalenius T. (1965).
    The Mode - A Negleted Statistical Parameter.
    \emph{J. Royal Statist. Soc. A}, \emph{128}:110-117.
    
    \item Venter J.H. (1967).
    On estimation of the mode.
    \emph{Ann. Math. Statist.}, \bold{38}(5):1446-1455.
    
    \item Ekblom H. (1972).
    A Monte Carlo investigation of mode estimators in small samples.
    \emph{Applied Statistics}, \bold{21}:177-184.
    %
    %\item Rousseeuw and Leroy, 1987  #(ou bien Andrews ?)
    
    \item Leclerc J. (1997).
    Comportement limite fort de deux estimateurs du mode : le shorth et l'estimateur naif.
    \emph{C. R. Acad. Sci. Paris, Serie I}, \bold{325}(11):1207-1210.
    %
    %\item Bickel ??
  }
}
\author{ P. Poncet }
\seealso{ \code{\link{mlv}} for general mode estimation, \code{\link{hsm}} for the half sample mode }
\examples{
library(evd)

# Unimodal distribution
x <- rgev(1000, loc = 23, scale = 1.5, shape = 0)

## True mode
gevMode(loc = 23, scale = 1.5, shape = 0)

## Estimate of the mode
venter(x, bw = 1/3)
M <- mlv(x, method = "venter", bw = 1/3)
print(M)
plot(M, xlim = c(20, 30))
}
\keyword{ univar }
