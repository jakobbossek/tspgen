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

#' @export
print.tspgen_collection = function(x, ...) {
  n.mutators = length(x$mutators)

  if (n.mutators == 0L) {
    BBmisc::catf("Empty collection.")
  } else {
    probs = if (!is.null(x$probs)) x$probs else rep(1, n.mutators) / n.mutators
    mutator.names = names(x$mutators)
    mutator.names = gsub("^do", "", mutator.names)
    mutator.names = gsub("Mutation$", "", mutator.names)

    BBmisc::catf("Prob.       | Mutator")
    BBmisc::catf("---------------------")
    for (i in seq_len(n.mutators)) {
      BBmisc::catf("P(m) = %.2f | %s %s",
        probs[i],
        mutator.names[i],
        listToString(x$mutators[[i]], wrap = c("(", ")"))
      )
    }
  }
}

listToString = function(x, wrap = NULL) {
  if (length(x) == 0)
    return("")
  ns = names(x)
  s = sapply(ns, function(n) {
    paste0(n, " = ", x[[n]])
  })
  s = BBmisc::collapse(s, sep = ", ")
  if (!is.null(wrap))
    s = sprintf("%s%s%s", wrap[1L], s, wrap[2L])
  return(s)
}
