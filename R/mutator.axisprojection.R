doAxisProjectionMutation = function(coords, pm, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)

  if (length(to.mutate) < 2L)
    return(coords)

  # sample axis
  axis = sample(c(1L, 2L), size = 1L)

  # get bounds of selected points
  rng = range(coords[to.mutate, axis])

  # sample "constant axis" within range
  line = runif(1L, min = rng[1L], max = rng[2L])

  coords[to.mutate, axis] = line
  return(coords)
}
