#' @title
#' Linear projection mutation
#'
#' @description This is a generalization of the \code{\link{doAxisProjectionMutation}}.
#' A subset \eqn{Q \subseteq P} (each point selected with independent
#' probability \code{pm}) is selected. Next, a random intercept \eqn{a \in [0, 1]}
#' is sampled. In a subsequent step a slope \eqn{m} is sampled uniformly at random from
#' \eqn{[0, 3]} (if \eqn{a < 0.5}) and \eqn{[-3, 0]} (if \eqn{a \geq 0.5}). This heuristic
#' distinction of cases ensures that with high probability the resulting linear function
#' \eqn{a + mx} runs inside the bounding-box \eqn{[0, 1]^2} at least partially. Finally, all
#' points \eqn{p \in Q} are subject to an orthogonal projection on \eqn{a + mx}.
#' Additionally, with probability \code{p.jitter}, Gaussian noise is added to the
#' second coordinate of the projected points.
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
doLinearProjectionMutation = function(coords, pm = 0.1, p.jitter = 0, jitter.sd = 0, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)
  checkmate::assertNumber(p.jitter, lower = 0, upper = 1)
  checkmate::assertNumber(jitter.sd, lower = 0, na.ok = FALSE, null.ok = FALSE)

  to.mutate = sampleRows(coords, p = pm)
  n.mutants = length(to.mutate)

  if (n.mutants < 2L)
    return(coords)

  # sample linear function
  intercept = runif(1L)

  # we do not want, e.g., a positive slope if intercept is close to 1
  slope = if (intercept < 0.5)
    stats::runif(1L, min = 0, max = 3)
  else
    stats::runif(1L, min = -3, max = 0)

  # helper function
  linFun = function(x) {
    intercept + slope * x
  }

  coords[to.mutate, 2L] = linFun(coords[to.mutate, 1L])

  # jitter points vertically around projected line
  if (stats::runif(1L) < p.jitter & jitter.sd > 0) {
    coords[to.mutate, 2L] = coords[to.mutate, 2L] + stats::rnorm(n.mutants, sd = jitter.sd)
  }

  #coords[to.mutate, ] = forceToBounds(coords[to.mutate, ])
  return(coords)
}
