doExplosionMutation = function(coords, min.eps = 0.1, max.eps = 0.4, ...) {
  # determine center of explosion
  center = runif(2L)
  #center = c(0.5, 0.5)

  # radius of explosion
  eps = runif(1L, min = min.eps, max = max.eps)
  #eps = 0.5

  # now compute all Euclidean distances to center
  # FIXME: do this faster, e.g. t(coords) - center?
  dists = getDistancesToCenter(coords, center)

  to.mutate = which(dists < eps)

  # do nothing if the number of selected points is below a fixed threshold
  if (length(to.mutate) < 2)
    return(coords)

  # now for all mutants compute direction vector
  mutants = t(apply(coords[to.mutate, ], 1L, function(point) {
    # normalized direction vector
    dir.vec = getNormalizedDirectionVector(center, point)

    # now shift point by at least eps into the dir.vec direction
    center + (dir.vec * (eps + rexp(1L, rate = 10)))
  }))
  coords[to.mutate, ] = forceToBounds(mutants)
  return(coords)
}
