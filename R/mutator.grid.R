doGridMutation = function(coords, box.min = 0.1, box.max = 0.3, ...) {
  box.width  = runif(1L, min = box.min, max = box.max)
  box.height = runif(1L, min = box.min, max = box.max)

  anchor = runif(2L, 0, c(1 - box.width, 1 - box.height))

  to.mutate = which(
    (coords[, 1L] > anchor[1L]) &
    (coords[, 1L] <= anchor[1L] + box.width) &
    (coords[, 2L] > anchor[2L]) &
    (coords[, 2L] <= anchor[2L] + box.height))

  n.mutants = length(to.mutate)
  if (n.mutants < 2L)
    return(coords)

  # get number of points in grid per dimension
  k.dim = floor(sqrt(n.mutants))

  # check if we need to ignore some mutants in subsequent steps
  k.dim.sq = k.dim^2
  if (k.dim.sq < n.mutants) {
    # drop some mutants
    to.mutate = sample(to.mutate, size = k.dim.sq, replace = FALSE)
  }

  grid = expand.grid(
    seq(anchor[1L], anchor[1L] + box.width, length.out = k.dim),
    seq(anchor[2L], anchor[2L] + box.height, length.out = k.dim))
  grid = unname(as.matrix(grid))

  coords[to.mutate, ] = grid
  return(coords)
}
