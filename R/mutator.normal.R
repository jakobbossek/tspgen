#' @title Normal mutation
#'
#' @description Each point is subject to additive Gaussian noise with probability \code{pm}.
#'
#' @references
#' Mersmann, O., Bischl, B., Bossek, J., Trautmann, H., Wagner, M., & Neumann, F. (2012).
#' Local search and the traveling salesman problem: A feature-based characterization of
#' problem hardness. Lecture Notes in Computer Science (Including Subseries Lecture Notes
#' in Artificial Intelligence and Lecture Notes in Bioinformatics), 7219 LNCS, 115-129.
#
#' @template arg_coords
#' @template arg_pm
#' @param sigma [\code{numeric(1)}]\cr
#'   Standard deviation for normal random numbers generator.
#'   Default is \eqn{0.0025}.
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @family mutation operators
#' @seealso \code{\link{build}}
#' @export
doNormalMutation = function(coords, pm = 0.1, sigma = 0.0025, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)
  checkmate::assertNumber(sigma, lower = 0.00001, upper = 1)

  to.mutate = sampleRows(coords, pm)
  if (length(to.mutate) > 0L) {
    delta = matrix(stats::rnorm(2 * length(to.mutate), sd = sigma), ncol = 2L)
    coords[to.mutate,] = coords[to.mutate,] + delta
  }

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
