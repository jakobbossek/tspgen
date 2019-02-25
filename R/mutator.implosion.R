doImplosionMutation = function(coords, min.eps = 0.1, max.eps = 0.3, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(min.eps, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(max.eps, lower = 0.05, upper = 0.5)
  if (min.eps > max.eps)
    BBmisc::stopf("[doImplosionMutation] min.eps must not be greater than max.eps.")

  # get implosion center
  blackhole = stats::runif(2)
  #blackhole = c(0.5, 0.5)

  # singularity radius
  eps = stats::runif(1L, min = min.eps, max = max.eps)
  #eps = 0.5

  dists = getDistancesToCenter(coords, blackhole)
  to.mutate = which(dists < eps)

  # do nothing if the number of selected points is below a fixed threshold
  if (length(to.mutate) < 2)
    return(coords)

  mutants = t(apply(coords[to.mutate, ], 1L, function(point) {
    dir.vec = getNormalizedDirectionVector(blackhole, point)
    dist = sqrt(sum((point - blackhole)^2))
    point + dir.vec * dist * min(abs(stats::rnorm(1L)), eps)#  (stats::runif(1L))
  }))
  coords[to.mutate, ] = mutants
  # no bounding necessary
  return(coords)
}
