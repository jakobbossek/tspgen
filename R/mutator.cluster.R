doClusterMutation = function(coords, pm = 0.1, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)
  n.mutants = length(to.mutate)
  if (n.mutants <= 1)
    return(coords)
  # generate cluster
  cl.center = stats::runif(2L)
  # mut_sd samplen
  sdev = stats::runif(1L, min = 0.001, max = 0.3)
  new.coords = matrix(stats::rnorm(2 * n.mutants, mean = 0, sd = sdev), ncol = 2L)
  new.coords = t(t(new.coords) + cl.center)
  new.coords = pmin(pmax(new.coords, 0), 1)
  coords[to.mutate, ] = new.coords
  return(coords)
}

