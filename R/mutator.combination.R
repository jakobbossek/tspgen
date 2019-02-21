doMutation = function(coords, pm, ...) {
  mutators = list(
    doExpansionMutation,
    doCompressionMutation,
    doRotationMutation,
    doImplosionMutation,
    doExplosionMutation,
    doAxisProjectionMutation, doLinearProjectionMutation,
    doGridMutation,
    doClusterMutation,
    doUniformMutation
    )
  n.mutators = length(mutators)
  idx = sample(seq_len(n.mutators), size = 1L)

  mutators[[idx]](coords, pm = pm, ...)
}






















