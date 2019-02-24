sampleRows = function(x, p, ...) {
  if (!(checkmate::testMatrix(x) | checkmate::testDataFrame(x)))
    BBmisc::stopf("[sampleRows] You need to pass a matrix or a data frame.")
  which(runif(nrow(x)) < p)
}

replicate2 = function(x, fun, n, ...) {
  for (i in seq_len(n)) {
    x = fun(x, ...)
  }
  return(x)
}

degreeToRadiant = function(alpha) {
  alpha * (pi / 180)
}

getRotationMatrix = function(alpha) {
  alpha = degreeToRadiant(alpha)
  matrix(c(
    cos(alpha), -sin(alpha),
    sin(alpha), cos(alpha)),
    byrow = TRUE, ncol = 2L)
}

# FIXME: lower and upper: should we drop it or pass down to getUniformMatrix?
forceToBounds = function(x, lower = 0, upper = 1, method = "boundary") {
  methods = c("boundary", "uniform")
  if (is.null(method))
    method = sample(methods, size = 1L)
  checkmate::assertChoice(method, choices = methods)#, "epsboundary"))
  switch(method,
    "boundary" = pmin(pmax(x, lower), upper),
    "uniform" = {
      out.of.bounds = (x < lower | x > upper)
      out.of.bounds = rowSums(out.of.bounds) > 0
      n.oob = sum(out.of.bounds)
      x[out.of.bounds, ] = getUniformMatrix(n.oob) * upper
      x
    })
}

getDistancesToCenter = function(x, center) {
  apply(x, 1L, function(point) {
    sqrt(sum((point - center)^2))
  })
}

getUniformMatrix = function(n, d = 2L) {
  matrix(runif(d * n), ncol = d)
}

getNormalizedDirectionVector = function(x, y) {
  dir.vec = x - y
  dir.vec / (sqrt(sum(dir.vec^2)))
}

getRandomLinearFunction = function() {
  # sample linear function
  intercept = runif(1L)

  # we do not want, e.g., a positive slope if intercept is close to 1
  slope = if (intercept < 0.5)
    runif(1L, min = 0, max = 3)
  else
    runif(1L, min = -3, max = 0)

  force(slope)
  force(intercept)
  # helper function
  linFun = function(x) {
    intercept + slope * x
  }
  return(list(intercept = intercept, slope = slope, linFun = linFun))
}
