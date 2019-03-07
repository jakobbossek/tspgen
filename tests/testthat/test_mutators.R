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

test_that("Build function", {
  presets = c("simple", "sophisticated", "all")
  n.nodes = 50L
  for (preset in presets) {
    x = build(n = n.nodes, iters = 25L, collection = init(preset))
    checkmate::expect_class(x, classes = "Network")
    checkmate::expect_matrix(x$coordinates, ncols = 2L, nrows = n.nodes, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  }
})
