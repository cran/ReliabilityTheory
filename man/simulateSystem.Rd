\name{simulateSystem}
\alias{simulateSystem}
\title{Simulate Masked Lifetime Data for a System}
\description{
This function enables easy simulation of iid masked lifetime observations from a system or network.
}
\usage{
simulateSystem(system, n, rdens, ...)
}
\arguments{
  \item{system}{
    may be: a \code{\link{system}} object (made with \code{\link{createSystem}} representing the system; the collection of cutsets of the system; or the system signature.
}
  \item{n}{
    how many simulations to produce.
}
  \item{rdens}{
    a user defined function which generates random realisations of the component lifetimes.
}
  \item{...}{
    parameters passed to the user defined function \code{rdens}.
}
}
\details{
The system or network is specified by means of a \code{\link{system}} object, whereby each end of the system is denoted by nodes named \code{s} and \code{t} which are taken to be perfectly reliable.  It is easy to construct the appropriate reliability block diagram representation using the function \code{\link{createSystem}}.  Note that each physically distinct component should be separately numbered when constructing this object.

This function then generates iid realisations of masked lifetimes.
}
\value{
  a numeric vector of length \code{n} containing the masked lifetime data.
}
%\references{
%Samaniego, F. J. (2007), \emph{System Signatures and Their Applications in Engineering Reliability}, Springer.
%}
\author{
Louis J.M. Aslett \email{louis.aslett@durham.ac.uk} (\url{https://www.louisaslett.com/})
}
\note{
Please feel free to email \email{louis.aslett@durham.ac.uk} with any queries or if you encounter errors when running this function.
}

%\seealso{
%\code{\link{computeNetworkSignature}}
%}
\examples{
# Simulate 20 masked lifetimes of a two component series system with Exponential(2)
# component lifetimes
# Using igraph object ...
simulateSystem(createSystem(s -- 1 -- 2 -- t), 20, rexp, rate=2)

# ... and using signature
simulateSystem(c(1,0), 20, rexp, rate=2)
}
\keyword{ simulate masked lifetime data }
\keyword{ system }
\keyword{ signature }% __ONLY ONE__ keyword per line
