% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mutator.explosion.R
\name{doExplosionMutation}
\alias{doExplosionMutation}
\title{Explosion mutation}
\usage{
doExplosionMutation(coords, min.eps = 0.1, max.eps = 0.4, ...)
}
\arguments{
\item{coords}{[\code{matrix}]\cr
An n times 2 matrix of point coordinates in \eqn{[0, 1]^2}.}

\item{min.eps}{[\code{numeric(1)}]\cr
Minimum value for sampled explosion radius.
Default is 0.1.}

\item{max.eps}{[\code{numeric(1)}]\cr
Maximum value for sampled explosion radius.
Default is 0.3.}

\item{...}{[any]\cr
Currently not used.}
}
\value{
[\code{matrix}] Mutated coordinates.
}
\description{
This operator is closely related to the implosion mutation (see
\code{\link{doImplosionMutation}}). However, instead of dragging points towards
a center those points are instead moved away from it (similar to particles
affected by an explosion). Again, a center of explosion \eqn{c \in R^2} and
an explosion radius \eqn{\epsilon = U[min.eps, max.eps]} are sampled at random.
All points within the ball with radius \eqn{\epsilon} around \eqn{c} are moved
away from the center following the direction vector between center and
corresponding point. The internals ensure that moved points have distance
at least \eqn{\epsilon} to \eqn{c} after mutation plus an additive random number
sampled from an exponential distribution with rate parameter \eqn{\lambda = 10}.
}
\seealso{
\code{\link{build}}

Other mutation operators: \code{\link{doAxisProjectionMutation}},
  \code{\link{doClusterMutation}},
  \code{\link{doCompressionMutation}},
  \code{\link{doExpansionMutation}},
  \code{\link{doGridMutation}},
  \code{\link{doImplosionMutation}},
  \code{\link{doLinearProjectionMutation}},
  \code{\link{doNormalMutation}},
  \code{\link{doRotationMutation}},
  \code{\link{doUniformMutation}}
}
\concept{mutation operators}
