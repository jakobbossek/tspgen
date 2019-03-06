#' @title Instance generation working horse.
#'
#' @description This function expects as primary parameters the desired instance size \eqn{n},
#' a number of iteration \code{iters} to perform and a collection of mutation operators
#' \code{collection}. The generation process is sequential:
#' \itemize{
#'   \item{Place \eqn{n} points uniformly at random in \eqn{[0,1]^2}. We shall denote this
#' set as \eqn{P} in the following.}.
#'   \item{For the desired number of iterations \code{iters} repeat the following
#' process: select a mutation operator \eqn{m} from the collection at random (according
#' to the probability distribution stored in \code{collection} and set \eqn{P = m(P)}.}
#' }
#'
#' @note If \code{\link{setProbabilities}} was not called on the \code{collection}
#' the algorithm falls back to the uniform distribution, i.e., each mutation operator
#' is selected with equal probability for application in each iteration.
#'
#' @param n [\code{integer(1)}]\cr
#'   Desired instance size, i.e., the number of nodes.
#' @param iters [\code{integer(1)}]\cr
#'   Number of iterations to perform.
#'   Default is 10.
#' @template arg_collection
#' @param return.all [\code{logical(1)}]\cr
#'   Shall all interim instances be stored and returned?
#'   Default is \code{FALSE}.
#' @param upper [\code{numeric(1)}]\cr
#'   Instance generation takes place in \eqn{[0,1]^2}. Use \code{upper} to
#'   upscale the boundaries, i.e., place nodes in \eqn{[0, upper]^2}.
#'   Default is 1.
#' @param bound.handling [\code{character(1)}]\cr
#'   Occasionally during instance generation points are moved outside the boundaries
#'   of the point space. The parameter \code{bound.handling} determines how to deal
#'   with these points. Option \dQuote{uniform} places outliers uniform at random within
#'.  the boundaries while option \dQuote{boundary} places them on the corresponding
#'   violates boundary/boundaries.
#'   Default is \dQuote{uniform}.
#' @return Either a netgen \code{Network} if \code{return.all = FALSE}, otherwise a
#' list of netgen networks of length \code{iters + 1}.
#' @examples
#' # set up a set of mutation operators
#' collection = init()
#' collection = addMutator(collection, "doUniformMutation", pm = 0.3)
#' collection = addMutator(collection, "doExplosionMutation", min.eps = 0.2, max.eps = 0.4)
#' collection = addMutator(collection, "doImplosionMutation", min.eps = 0.2, max.eps = 0.4)
#' collection = addMutator(collection, "doAxisProjectionMutation")
#'
#' # specify probability distribution.
#' collection = setProbabilities(collection, probs = c(0.1, 0.6, 0.2, 0.1))
#'
#' x = build(n = 50, iters = 10, collection = collection)
#' x = build(n = 100, iters = 50, collection = collection, return.all = TRUE, bound.handling = "boundary")
#' @export
build = function(n, iters = 10L, collection, return.all = FALSE, upper = 1, bound.handling = "uniform") {
  checkmate::asCount(n)
  checkmate::asCount(iters)
  checkmate::assertClass(collection, "tspgen_collection")
  checkmate::assertFlag(return.all)
  checkmate::assertNumber(upper, lower = 1, finite = TRUE)
  checkmate::assertChoice(bound.handling, choices = c("uniform", "boundary"))

  mutators = collection$mutators
  n.mutators = length(mutators)
  names = names(mutators)

  probs = if (is.null(collection$probs)) rep(1 / n.mutators, n.mutators) else collection$probs

  # baseline is a RUE instance
  coords = getUniformMatrix(n = n)

  if (return.all) {
    coords.list = vector(mode = "list", length = iters + 1)
    coords.list[[1L]] = coords * upper
  }

  for (i in seq_len(iters)) {
    idx = sample(seq_len(n.mutators), size = 1L, prob = probs)
    mutator.fun  = names[idx]
    mutator.pars = mutators[[mutator.fun]]
    mutator.pars = BBmisc::insert(mutator.pars, list(coords = coords))
    # apply mutation
    coords = do.call(mutator.fun, mutator.pars)
    attr(coords, "df") = NULL
    # repair points outside the bounding box
    coords = forceToBounds(coords, bound.handling = bound.handling)
    # avoid duplicate nodes
    coords = relocateDuplicates(coords)
    if (return.all) {
      coords.list[[i + 1L]] = coords * upper
    }
  }

  #print(coords.list)

  if (return.all)
    return(lapply(coords.list, netgen::makeNetwork, lower = 0, upper = upper))
  return(netgen::makeNetwork(coords * upper, lower = 0, upper = upper))
}
