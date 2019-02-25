context("mutators")

test_that("Mutators produce valid output", {
  mutators = list(
    doExpansionMutation,
    doCompressionMutation,
    doRotationMutation,
    doImplosionMutation,
    doExplosionMutation,
    doAxisProjectionMutation,
    doLinearProjectionMutation,
    doGridMutation,
    doClusterMutation,
    doUniformMutation
  )
  n.mutators = length(mutators)
  n.nodes = 50L
  coords = getUniformMatrix(n.nodes)
  for (i in seq_len(n.mutators)) {
    coords = replicate2(coords, mutators[[i]], n = 25L)
    checkmate::expect_matrix(coords, ncols = 2L, nrows = n.nodes, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
    coords = forceToBounds(coords)
    expect_true(all(coords >= 0 & coords <= 1))
  }
})
