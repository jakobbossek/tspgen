#' @title
#' Implosion mutation
#'
#' @description Simple mutation operators, e.g., uniform mutation (see \code{\link{doUniformMutation}})
#' or normal mutation (see \code{\link[doNormalMutation}}) will most likely generate
#' instances that look uniformly distributed. However, often one is interested structures
#' that are different from random uniform instances. The implosion mutation is
#' tailored towards cluster generation. This is achieved by simulating an implosion:
#' a center of implosion \eqn{c \in R^2} is sampled alongside its implosion
#' radius \eqn{\epsilon \in [min.eps, max.eps]}. Next, all points \eqn{Q \subseteq P} within
#' the \eqn{\epsilon}-ball around the center of implosion are dragged towards the center
#' forming small or medium-sized cluster structures (depending on the randomly sampled
#' implosion radius).
#'
#' @template arg_coords
#' @param min.eps [\code{numeric(1)}]\cr
#'   Minimum value for sampled implosion radius.
#'   Defaults to 0.1
#' @param max.eps [\code{numeric(1)}]\cr
#'   Maximum value for sampled implosion radius.
#'   Defaults to 0.3.
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
doImplosionMutation = function(coords, min.eps = 0.1, max.eps = 0.3, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(min.eps, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(max.eps, lower = 0.05, upper = 0.5)
  if (min.eps > max.eps)
    BBmisc::stopf("[doImplosionMutation] min.eps must not be greater than max.eps.")

  # get implosion center
  blackhole = stats::runif(2)
  #blackhole = c(0.5, 0.5)

  # singularity radius
  eps = stats::runif(1L, min = min.eps, max = max.eps)
  #eps = 0.5

  dists = getDistancesToCenter(coords, blackhole)
  to.mutate = which(dists < eps)

  # do nothing if the number of selected points is below a fixed threshold
  if (length(to.mutate) < 2)
    return(coords)

  mutants = t(apply(coords[to.mutate, ], 1L, function(point) {
    dir.vec = getNormalizedDirectionVector(blackhole, point)
    dist = sqrt(sum((point - blackhole)^2))
    point + dir.vec * dist * min(abs(stats::rnorm(1L)), eps)#  (stats::runif(1L))
  }))
  coords[to.mutate, ] = mutants

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
