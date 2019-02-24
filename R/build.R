#' @title Instance generation working horse.
#'
#' @description This function expects as parameters the desired instance size \eqn{n},
#' a number of iteration to perform and a collection of mutation operators. The
#' generation process is sequential:
#' \itemize{
#'  \item{Place \eqn{n} points uniformly at random in \eqn{[0,1]^2}. We shall denote this
#' set as \eqn{P} in the following.}.
#'  \item{For the desired number of iterations \code{iters} repeat the following
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
#' @examples
#' collection = init()
#' collection = addMutator(collection, "doUniformMutation", pm = 0.3)
#' collection = addMutator(collection, "doExplosionMutation", min.eps = 0.2, max.eps = 0.4)
#' collection = addMutator(collection, "doImplosionMutation", min.eps = 0.2, max.eps = 0.4)
#' collection = addMutator(collection, "doAxisProjectionMutation")
#' collection = setProbabilities(collection, probs = c(0.1, 0.6, 0.2, 0.1))
#'
#' x = build(n = 50, iters = 10, collection = collection)
#' @export
build = function(n, iters = 10L, collection) {
  mutators = collection$mutators
  n.mutators = length(mutators)
  names = names(mutators)

  probs = if (is.null(collection$probs)) rep(1 / n.mutators, n.mutators) else collection$probs

  # baseline is a RUE instance
  coords = getUniformMatrix(n = n)

  for (i in seq_len(iters)) {
    idx = sample(seq_len(n.mutators), size = 1L, prob = probs)
    mutator.fun  = names[idx]
    mutator.pars = mutators[[mutator.fun]]
    mutator.pars = BBmisc::insert(mutator.pars, list(coords = coords))
    coords = do.call(mutator.fun, mutator.pars)
  }

  return(netgen::makeNetwork(coords, lower = 0, upper = 1))
}

#' Initialize a bare mutation operator collection.
#'
#' @return [\code{tspgen_collection}] Collection of mutation operators.
#' @export
init = function() {
  BBmisc::makeS3Obj("tspgen_collection", mutators = list())
}

#' Adds a mutation operator to a collection.
#'
#' @template arg_collection
#' @param fun [\code{character(1)}]\cr
#'   Function name of mutation operator which should be added to collection
#'   provided as a string.
#' @param ... [any]\cr
#'   Parameters for \code{fun}. See the documentation of the corresponding mutation
#'   operator for details.
#' @return [\code{tspgen_collection}] Augmented mutation operator collection.
#' @export
addMutator = function(collection, fun, ...) {
  checkmate::assertClass(collection, "tspgen_collection")
  checkmate::assertString(fun)
  pars = list(...)
  if (!is.null(collection$probs))
    BBmisc::stopf("[addMutator] Probabilities already set. Add all mutators prior
      to setting the selection probabilities.")
  if (fun %in% names(collection$mutators))
    BBmisc::stopf("Mutation '%s' already added to collection.", fun)
  collection$mutators[[fun]] = pars
  return(collection)
}

#' @title Set mutation operator probabilities.
#'
#' @description This function defines the mutation operator probabilities. I.e.,
#' a vector of probabilities with the i-th component being the probability of
#' applying the i-th mutation operator in a single step of the instance generation
#' process.
#'
#' @template arg_collection
#' @param probs [\code{numeric}]\cr
#'   Numeric vector of probabilities. Needs to have the length of the mutation operator
#'   collection, i.e., the number of mutation operators in the collection.
#' @return [\code{tspgen_collection}] Augmented mutation operator collection.
#' @export
setProbabilities = function(collection, probs) {
  checkmate::assertClass(collection, "tspgen_collection")
  n.mutators = length(collection$mutators)
  if (length(probs) != n.mutators)
    BBmisc::stopf("[setProbabilities] There are %i mutators in the collection,
      but you tried to set %i probabilities.", n.mutators, length(probs))
  if (abs(sum(probs) - 1) > 0.00001)
    BBmisc::stopf("[setProbabilities] Probabilities need to sum up to 1.")
  collection$probs = probs
  return(collection)
}