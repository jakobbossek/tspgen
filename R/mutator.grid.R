#' @title
#' Grid mutation
#'
#' @description This mutation operator is designed to introduce a partial grid structure
#' into to point of a given Euclidean problem instance. This is achieved by generating
#' a random box of width and height \eqn{w, h \in [box.min, box.max]} and placing it
#' at random within the boundaries \eqn{[0, 1]^2}. All points \eqn{Q \subseteq P} inside
#' the box are affected by the mutation. Point set \eqn{Q} is replaced by a quadratic
#' grid of points \eqn{Q'} with \eqn{g = \lfloor \sqrt{|Q|} \rfloor} rows and columns respectively.
#' Note that if \eqn{g^2 < |Q|} we ignore \eqn{|Q| - g} random points of \eqn{Q}, i.e., these
#' points are not touched.
#' Subsequent, optional steps involve rotation and noise addition: with probability \eqn{p.rot}
#' \eqn{Q'} is rotated by a random angle \eqn{alpha \in [0, \pi/2]} and with probability
#' \code{p.jitter} all points in \eqn{Q} are perturbed by additive Gaussian noise with
#' mean \eqn{(0,0)} and standard deviation \code{jitter.sd} in each dimension.
#'
#' @template arg_coords
#' @param box.min [\code{numeric(1)}]\cr
#'   Minimum for sampled box width and height respectively.
#'   Default is 0.1.
#' @param box.max [\code{numeric(1)}]\cr
#'   Maximum for sampled box width and height respectively.
#'   Default is 0.3.
#' @template arg_prot
#' @template arg_pjitter
#' @template arg_jittersd
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
doGridMutation = function(coords, box.min = 0.1, box.max = 0.3, p.rot = 0, p.jitter = 0, jitter.sd = 0, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(box.min, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(box.max, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(p.rot, lower = 0, upper = 1, null.ok = FALSE)
  checkmate::assertNumber(p.jitter, lower = 0, upper = 1, null.ok = FALSE)
  checkmate::assertNumber(jitter.sd, lower = 0, na.ok = FALSE, null.ok = FALSE)

  if (box.min > box.max)
    BBmisc::stopf("[doGridMutation] box.min must not be greater than box.max.")

  box.width  = stats::runif(1L, min = box.min, max = box.max)
  box.height = stats::runif(1L, min = box.min, max = box.max)

  # where to place the box inside [0, 1] x [0, 1]
  anchor = stats::runif(2L, 0, c(1 - box.width, 1 - box.height))

  # which points should be subject to mutation
  to.mutate = which(
    (coords[, 1L] > anchor[1L]) &
    (coords[, 1L] <= anchor[1L] + box.width) &
    (coords[, 2L] > anchor[2L]) &
    (coords[, 2L] <= anchor[2L] + box.height))

  n.mutants = length(to.mutate)
  if (n.mutants < 2L)
    return(coords)

  # get number of points in grid per dimension
  k.dim = floor(sqrt(n.mutants))

  # check if we need to ignore some mutants in subsequent steps
  k.dim.sq = k.dim^2
  if (k.dim.sq < n.mutants) {
    # drop some mutants
    to.mutate = sample(to.mutate, size = k.dim.sq, replace = FALSE)
  }

  grid = expand.grid(
    seq(anchor[1L], anchor[1L] + box.width, length.out = k.dim),
    seq(anchor[2L], anchor[2L] + box.height, length.out = k.dim))
  grid = unname(as.matrix(grid))

  # rotate grid with probability p.rot
  if (stats::runif(1L) < p.rot) {
    angle = stats::runif(1L, min = 0, max = 90)
    rot.mat = getRotationMatrix(angle)
    # perform rotation around grid center
    grid.mean = colMeans(grid)
    grid = t(rot.mat %*% (t(grid) - grid.mean) + grid.mean)
  }

  # jitter grid points
  if (stats::runif(1L) < p.jitter & jitter.sd > 0) {
    grid = grid + stats::rnorm(2 * k.dim.sq, sd = jitter.sd)
  }

  coords[to.mutate, ] = grid

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
