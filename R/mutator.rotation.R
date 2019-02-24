doRotationMutation = function(coords, pm = 0.1, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)
  angle = runif(1L, min = 0, max = 360)
  rot.mat = getRotationMatrix(angle)

  if (length(to.mutate) < 2L)
    return(coords)

  # NOTE: rotation is centered around origin. Hence, we shift the rotated point cloud by a random number
  mutants = t(rot.mat %*% t(coords[to.mutate, ]) + runif(2L))
  #coords[to.mutate, ] = forceToBounds(mutants)
  coords[to.mutate, ] = mutants
  return(coords)
}
