#' @title
#' Expansion mutation
#'
#' @description This is a generalization of the linear projection mutation operator
#' (see \code{\link{doLinearProjectionMutation}}). The parameters \eqn{a} (intercept)
#' and \eqn{m} (slope) of a linear function \eqn{a + mx} are sampled (see documentation
#' of \code{\link{doLinearProjectionMutation}} for details. All points whose orthogonal distance
#' to the linear function is below the sampled tube with \eqn{\epsilon = U[min.eps, max.eps]}
#' are subject to mutation. This is achieved by moving the points away from their orthogonal
#' projections to distance \eqn{\epsilon + Exp(\lambda = 10)}.
#'
#' @template arg_coords
#' @param min.eps [\code{numeric(1)}]\cr
#'   Minimum value for sampled tube width.
#'   Default is 0.1
#' @param max.eps [\code{numeric(1)}]\cr
#'   Maximum value for the sampled tube with.
#'   Default is 0.3.
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
doExpansionMutation = function(coords, min.eps = 0.1, max.eps = 0.3, ...) {
  doTubeMutation(coords, min.eps, max.eps, type = "Expansion", ...)
}

doTubeMutation = function(coords, min.eps = 0.1, max.eps = 0.3, type, ...) {
  checkmate::assertChoice(type, choices = c("Expansion", "Compression"))
  checkmate::assertMatrix(coords, ncols = 2L, mode = "numeric", any.missing = FALSE, all.missing = FALSE)
  checkmate::assertNumber(min.eps, lower = 0.05, upper = 0.5)
  checkmate::assertNumber(max.eps, lower = 0.05, upper = 0.5)
  if (min.eps > max.eps) {
    BBmisc::stopf("[do%sMutation] min.eps must not be greater than max.eps.", type)
  }

  # sample "tube-width"
  eps = stats::runif(1L, min = min.eps, max = max.eps)

  # get random linear function
  linear = getRandomLinearFunction()
  bb = c(0, linear$intercept)
  cc = c(1, linear$intercept + linear$slope) #linear$linFun(1))
  # direction vector of linear function
  uu = cc - bb

  # orthogonal projections on linear function
  projs = t(apply(coords, 1L, function(point) {
    (sum((point - bb) * uu) / (sum(uu^2))) * uu + bb
  }))

  # distance of points to their projections
  dists = sqrt(rowSums((projs - coords)^2))

  # which points are inside the eps-tube?
  to.mutate = which(dists < eps)

  # nothing to do if only few points are selected
  if (length(to.mutate) < 2L)
    return(coords)

  # get normalized direction vectors between points and projections
  norm.dir.vecs = t(apply(coords[to.mutate, ] - projs[to.mutate, ], 1L, function(vec) {
    vec / sqrt(sum(vec^2))
  }))

  # at last do the nice mutation
  mutants = if (type == "Expansion")
    projs[to.mutate, ] + norm.dir.vecs * (eps + stats::rexp(length(to.mutate), rate = 10))
  else
    coords[to.mutate, ] - norm.dir.vecs * dists[to.mutate] * pmin(abs(stats::rnorm(length(to.mutate))), 1)

  #coords[to.mutate, ] = forceToBounds(mutants)
  coords[to.mutate, ] = mutants

  # # debug
  # instance = makeNetwork(coords)
  # pl = autoplot(instance)
  # pl = pl + geom_abline(intercept = linear$intercept, slope = linear$slope, size = 2L, colour = "blue")
  # pl = pl + geom_abline(intercept = linear$intercept + eps, slope = linear$slope, size = 1L, colour = "blue", linetype = "dashed")
  # pl = pl + geom_abline(intercept = linear$intercept - eps, slope = linear$slope, size = 1L, colour = "blue", linetype = "dashed")
  # df = as.data.frame(coords)
  # colnames(df) = c("x1", "x2")
  # df$mutated = "NO"
  # df$mutated[to.mutate] = "YES"
  # df$mutated = as.factor(df$mutated)
  # pl = pl + geom_point(data = df, aes(colour = mutated))

  # dfp = as.data.frame(projs)
  # colnames(dfp) = c("x1", "x2")
  # pl = pl + geom_point(data = dfp, colour = "green", size = 1.5)
  # pl = pl + xlim(c(-3, 3)) + ylim(c(-3, 3))
  # print(pl)
  # stop()

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}

