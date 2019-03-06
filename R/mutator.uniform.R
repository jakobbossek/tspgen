#' @title Uniform mutation
#'
#' @description Each point is replaced (with probability \code{pm}) by a point placed
#' uniformly at random in the Euclidean sub-space \eqn{[0,1]^2}.
#'
#' @references
#' Mersmann, O., Bischl, B., Bossek, J., Trautmann, H., Wagner, M., & Neumann, F. (2012).
#' Local search and the traveling salesman problem: A feature-based characterization of
#' problem hardness. Lecture Notes in Computer Science (Including Subseries Lecture Notes
#' in Artificial Intelligence and Lecture Notes in Bioinformatics), 7219 LNCS, 115-129.
#
#' @template arg_coords
#' @template arg_pm
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @family mutation operators
#' @seealso \code{\link{build}}
#' @export
doUniformMutation = function(coords, pm = 0.1, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)
  coords[to.mutate, ] = getUniformMatrix(length(to.mutate))

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}
