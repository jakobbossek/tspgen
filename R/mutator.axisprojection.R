doAxisProjectionMutation = function(coords, pm = 0.1, p.jitter = 0, jitter.sd = 0, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)
  checkmate::assertNumber(p.jitter, lower = 0, upper = 1)
  checkmate::assertNumber(jitter.sd, lower = 0, na.ok = FALSE, null.ok = FALSE)

  to.mutate = sampleRows(coords, p = pm)

  if (length(to.mutate) < 2L)
    return(coords)

  # sample axis
  axis = sample(c(1L, 2L), size = 1L)

  # get bounds of selected points
  rng = range(coords[to.mutate, axis])

  # sample "constant axis" within range
  line = stats::runif(1L, min = rng[1L], max = rng[2L])

  # jitter points around projected axis
  if (runif(1) < p.jitter & jitter.sd > 0) {
    line = line + stats::rnorm(length(to.mutate), sd = jitter.sd)
  }

  coords[to.mutate, axis] = line
  return(coords)
}
