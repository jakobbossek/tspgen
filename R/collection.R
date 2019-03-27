#' Initialize a bare mutation operator collection.
#'
#' @param preset [\code{character(1)}]\cr
#'   Shall the collection be initialized with a preset of mutation operators?
#'   Option \dQuote{simple} adds \link[=doUniformMutation]{uniform} and
#'   \link[=doNormalMutation]{normal} mutation only.
#'   Option \dQuote{sophisticated} adds all available operators except the
#'   simple ones, i.e., uniform and normal.
#'   Option \dQuote{all} adds all available mutation operators.
#'   Note that each preset sets the uniform operator selection probability
#'   distribution, i.e., each operator is equally likely to be selected.
#'   Default is \code{NULL}, i.e., no preset at all.
#' @return [\code{tspgen_collection}] Collection of mutation operators.
#' @export
init = function(preset = NULL) {
  collection = BBmisc::makeS3Obj("tspgen_collection", mutators = list())
  if (is.null(preset))
    return(collection)

  checkmate::assertChoice(preset, choices = c("simple", "sophisticated", "all"), null.ok = FALSE)
  preset.funs = list(
    simple = addPresetSimple,
    sophisticated = addPresetSophisticated,
    all = addPresetAll)

  do.call(preset.funs[[preset]], list(collection = collection))
}

addPresetSimple = function(collection) {
  pm = 0.1
  collection = addMutator(collection, "doUniformMutation", pm = pm)
  collection = addMutator(collection, "doNormalMutation", pm = pm)
  return(collection)
}

addPresetSophisticated = function(collection) {
  pm = 0.1
  p.rot = 0.5
  jitter.sd = 0.05
  p.jitter = 0.5
  collection = addMutator(collection, "doExplosionMutation")
  collection = addMutator(collection, "doImplosionMutation")
  collection = addMutator(collection, "doRotationMutation", pm = pm)
  collection = addMutator(collection, "doClusterMutation", pm = pm)
  collection = addMutator(collection, "doExpansionMutation")
  collection = addMutator(collection, "doCompressionMutation")
  collection = addMutator(collection, "doAxisProjectionMutation", pm = pm, p.jitter = p.jitter, jitter.sd = jitter.sd)
  collection = addMutator(collection, "doLinearProjectionMutation", pm = pm, p.jitter = p.jitter, jitter.sd = jitter.sd)
  collection = addMutator(collection, "doGridMutation", pm = pm, p.jitter = p.jitter, jitter.sd = jitter.sd, p.rot = p.rot)
  return(collection)
}

addPresetAll = function(collection) {
  collection = addPresetSophisticated(addPresetSimple(collection))
  return(collection)
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

applyRandomMutation = function(collection, coords) {
  checkmate::assertClass(collection, "tspgen_collection")
  checkmate::assertMatrix(coords, mode = "numeric", min.rows = 2L, ncols = 2L, any.missing = FALSE, all.missing = FALSE)

  mutators = collection$mutators
  n.mutators = length(mutators)
  mutator.names = names(mutators)
  probs = if (!is.null(collection$probs)) collection$probs else rep(1, n.mutators) / n.mutators

  idx.mutator = sample(seq_len(n.mutators), size = 1L, prob = probs)
  mutator.fun  = mutator.names[idx.mutator]
  mutator.pars = mutators[[mutator.fun]]
  mutator.pars = BBmisc::insert(mutator.pars, list(coords = coords))

  # apply mutation
  coords = do.call(mutator.fun, mutator.pars)
  return(coords)
}
