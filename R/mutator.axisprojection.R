#' @title
#' Axis projection mutation
#'
#' @description A subset \eqn{Q \subseteq P} (each point selected with independent
#' probability \code{pm}) is selected. Next, an axis \eqn{a \in \{1, 2\}} and a constant
#' \eqn{c \in [0, 1]} are sampled. Finally, the mutator sets \eqn{p_a = c} for all \eqn{p \in Q},
#' i.e., a projection on the constant vertical or horizontal line is performed.
#' Optionally, with probability \code{p.jitter} all projected points are subject to
#' Gaussian noise with standard deviation \code{jitter.sd} with respect to the \eqn{a}-th
#' coordinate: \eqn{p_a = p_a + N(0, jitter.sd)}.
#'
#' @template arg_coords
#' @template arg_pm
#' @template arg_pjitter
#' @template arg_jittersd
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
doAxisProjectionMutation = function(coords, pm = 0.1, p.jitter = 0, jitter.sd = 0.1, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)
  checkmate::assertNumber(p.jitter, lower = 0, upper = 1)
  checkmate::assertNumber(jitter.sd, lower = 0.001, na.ok = FALSE, null.ok = FALSE)

  to.mutate = sampleRows(coords, p = pm)

  if (length(to.mutate) < 2L)
    return(coords)

  # sample axis
  axis = sample(c(1L, 2L), size = 1L)

  # get bounds of selected points
  rng = range(coords[to.mutate, axis])

  # sample "constant axis" within range
  line = stats::runif(1L, min = rng[1L], max = rng[2L])

  # jitter points around projected axis
  if (runif(1) < p.jitter) {
    line = line + stats::rnorm(length(to.mutate), sd = jitter.sd)
  }

  coords[to.mutate, axis] = line

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
