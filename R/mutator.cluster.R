doClusterMutation = function(coords, pm, ...) {
  to.mutate = sampleRows(coords, p = pm)
  n.mutants = length(to.mutate)
  if (n.mutants <= 1)
    return(coords)
  # generate cluster
  cl.center = runif(2L)
  # mut_sd samplen
  sdev = runif(1L, min = 0.001, max = 0.3)
  new.coords = matrix(rnorm(2 * n.mutants, mean = 0, sd = sdev), ncol = 2L)
  new.coords = t(t(new.coords) + cl.center)
  new.coords = pmin(pmax(new.coords, 0), 1)
  coords[to.mutate, ] = new.coords
  return(coords)
}

