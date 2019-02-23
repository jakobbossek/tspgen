doGridMutation = function(coords, box.min = 0.1, box.max = 0.3, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(box.min, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(box.max, lower = 0.05, upper = 0.5)
  if (box.min > box.max)
    BBmisc::stopf("[doGridMutation] box.min must not be greater than box.max.")

  box.width  = runif(1L, min = box.min, max = box.max)
  box.height = runif(1L, min = box.min, max = box.max)

  # where to place the box inside [0, 1] x [0, 1]
  anchor = runif(2L, 0, c(1 - box.width, 1 - box.height))

  # which points should be subject to mutation
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
