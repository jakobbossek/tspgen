doLinearProjectionMutation = function(coords, pm, ...) {
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(pm, lower = 0, upper = 1)

  to.mutate = sampleRows(coords, p = pm)

  if (length(to.mutate) < 2L)
    return(coords)

  # sample linear function
  intercept = runif(1L)

  # we do not want, e.g., a positive slope if intercept is close to 1
  slope = if (intercept < 0.5)
    runif(1L, min = 0, max = 3)
  else
    runif(1L, min = -3, max = 0)

  # helper function
  linFun = function(x) {
    intercept + slope * x
  }

  coords[to.mutate, 2L] = linFun(coords[to.mutate, 1L])
  coords[to.mutate, ] = forceToBounds(coords[to.mutate, ])
  return(coords)
}
