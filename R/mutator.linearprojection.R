#' @title
#' Linear projection mutation
#'
#' @description This is a generalization of the \code{\link{doAxisProjectionMutation}}.
#' A subset \eqn{Q \subseteq P} (each point selected with independent
#' probability \code{pm}) is selected. Next, a random intercept \eqn{\beta_0 \in [0, 1]}
#' is sampled. In a subsequent step a slope \eqn{\beta_1} is sampled uniformly at random from
#' \eqn{[0, 3]} (if \eqn{\beta_0 < 0.5}) and \eqn{[-3, 0]} (if \eqn{\beta_0 \geq 0.5}). This heuristic
#' distinction of cases ensures that with high probability the resulting linear function
#' \eqn{\beta_0 + \beta_1x} runs inside the bounding-box \eqn{[0, 1]^2} at least partially. Finally, all
#' points \eqn{p \in Q} are modified by setting \eqn{p_2 = \beta_0 + \beta_1p_1}.
#' Additionally, with probability \code{p.jitter}, Gaussian noise with mean zero
#' and standard deviation \code{jitter.sd} is added to the second coordinate.
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

  linear = getRandomLinearFunction()

  coords[to.mutate, 2L] = linear$linFun(coords[to.mutate, 1L])

  # jitter points vertically around projected line
  if (stats::runif(1L) < p.jitter & jitter.sd > 0) {
    coords[to.mutate, 2L] = coords[to.mutate, 2L] + stats::rnorm(n.mutants, sd = jitter.sd)
  }

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}

