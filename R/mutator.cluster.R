#' @title
#' Cluster mutation
#'
#' @description A subset \eqn{Q \subseteq P} (each point selected with independent
#' probability \code{pm}) is replaced by points placed according to a random sampleof size \eqn{|Q|} of
#' a bi-variate Gaussian distribution with uniformly sampled center and standard
#' deviation up to 0.3.
#'
#' @template arg_coords
#' @template arg_pm
#' @template arg_dots
#' @return [\code{matrix}] Mutated coordinates.
#' @seealso \code{\link{build}}
#' @family mutation operators
#' @export
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

  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, to.mutate)

  return(coords)
}

