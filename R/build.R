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

init = function() {
  BBmisc::makeS3Obj("tspgen_collection", mutators = list())
}

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
