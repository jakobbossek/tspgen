# Global mutation operator for EA.
#
# @param coords [\code{matrix}]\cr
#   Numeric matrix of city coordinates,
#   rows denote cities.
# @param mutOp [\code{numeric(1)}]\cr
#   Mutation probability from [0,1].
#
# @return [\code{matrix}]
#   Numeric matrix of globally mutated city coordinates.
doUniformMutation = function(coords, pm = 0.1, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)
  coords[to.mutate, ] = getUniformMatrix(length(to.mutate))
  coords
}
