#' @title
#' Explosion mutation
#'
#' @description This operator is closely related to the implosion mutation (see
#' \code{\link{doImplosionMutation}}). However, instead of dragging points towards
#' a center those points are instead moved away from it (similar to particles
#' affected by an explosion). Again, a center of explosion \eqn{c \in R^2} and
#' an explosion radius \eqn{\epsilon = U[min.eps, max.eps]} are sampled at random.
#' All points within the ball with radius \eqn{\epsilon} around \eqn{c} are moved
#' away from the center following the direction vector between center and
#' corresponding point. The internals ensure that moved points have distance
#' at least \eqn{\epsilon} to \eqn{c} after mutation plus an additive random number
#' sampled from an exponential distribution with rate parameter \eqn{\lambda = 10}.
#'
#' @template arg_coords
#' @param min.eps [\code{numeric(1)}]\cr
#'   Minimum value for sampled explosion radius.
#'   Default is 0.1.
#' @param max.eps [\code{numeric(1)}]\cr
#'   Maximum value for sampled explosion radius.
#'   Default is 0.3.
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
doExplosionMutation = function(coords, min.eps = 0.1, max.eps = 0.4, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(min.eps, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(max.eps, lower = 0.05, upper = 0.5)
  if (min.eps > max.eps)
    BBmisc::stopf("[doExplosionMutation] min.eps must not be greater than max.eps.")

  # determine center of explosion
  center = stats::runif(2L)
  #center = c(0.5, 0.5)

  # radius of explosion
  eps = stats::runif(1L, min = min.eps, max = max.eps)
  #eps = 0.5

  # now compute all Euclidean distances to center
  # FIXME: do this faster, e.g. t(coords) - center?
  dists = getDistancesToCenter(coords, center)

  to.mutate = which(dists < eps)

  # do nothing if the number of selected points is below a fixed threshold
  if (length(to.mutate) < 2)
    return(coords)

  # now for all mutants compute direction vector
  mutants = t(apply(coords[to.mutate, ], 1L, function(point) {
    # normalized direction vector
    dir.vec = getNormalizedDirectionVector(center, point)
    # now shift point by at least eps into the dir.vec direction
    center + (dir.vec * (eps + stats::rexp(1L, rate = 10)))
  }))

  # debug
  # dd = as.data.frame(coords)
  # colnames(dd) = c("x1", "x2")
  # dd$mutated = ifelse(dists < eps, "YES", "NO")
  # pl = ggplot(dd, aes(x = x1, y = x2, color = mutated)) + geom_point()
  # print(pl)
  # BBmisc::pause()

  #coords[to.mutate, ] = forceToBounds(mutants)
  coords[to.mutate, ] = mutants

  # debug
  # dd[to.mutate, c("x1", "x2")] = coords[to.mutate, ]
  # pl = ggplot(dd, aes(x = x1, y = x2, color = mutated)) + geom_point()
  # print(pl)
  # BBmisc::pause()

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
