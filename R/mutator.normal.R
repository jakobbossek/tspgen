# Local mutation operator of EA.
# Global mutation operator for EA.
#
# @param coords [\code{matrix}]\cr
#   Numeric matrix of city coordinates,
#   rows denote cities.
# @param mut_op [\code{numeric(1)}]\cr
#   Mutation probability from [0,1].
# @param sigma [\code{numeric(1)}]\cr
#   Standard deviation of normal noise.
# @return [\code{matrix}]
#   Numeric matrix of globally mutated city coordinates.
doNormalMutation = function(coords, pm = 0.1, sigma = 0.0025) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)
  checkmate::assertNumber(sigma, lower = 0.00001, upper = 1)

  to.mutate = sampleRows(coords, pm)
  ## pmin(pmax(...)) used to ensure we stay in bounds:
  if (length(to.mutate) > 0L) {
    delta = matrix(rnorm(2 * length(to.mutate), sd = sigma), ncol = 2L)
    coords[to.mutate,] = coords[to.mutate,] + delta
    #coords[to.mutate,] = forceToBounds(coords[to.mutate,] + delta)
  }
  return(coords)
}
