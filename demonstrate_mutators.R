library(ggplot2)
library(gridExtra)
devtools::load_all(".")

n = 1000L
pm = 0.4
min.eps = 0.2
max.eps = 0.5
jitter.sd = 0.05
p.rot = 0
p.jitter = 0
iters = 1000L

options(tspgen.debug = TRUE)

doInitialSolutionMutation = function(coords, ...) {
  if (!is.null(getOption("tspgen.debug")))
    attr(coords, "df") = mutationAsDataframe(coords, integer(0L))
  return(coords)
}

x = generateRandomNetwork(1000L, lower = 0, upper = 1)
x.coords = x$coordinates
mutators = c(
  "doInitialSolutionMutation",
  "doNormalMutation", "doUniformMutation",
  "doExplosionMutation", "doImplosionMutation",
  "doClusterMutation",
  "doExpansionMutation", "doCompressionMutation",
  "doLinearProjectionMutation",
  "doGridMutation")

res = lapply(mutators, function(mutator) {
  set.seed(1234)
  y = do.call(mutator, list(coords = x.coords, pm = pm, min.eps = min.eps, max.eps = max.eps, box.min = min.eps, box.max = max.eps, p.jitter = p.jitter, jitter.sd = jitter.sd))
  y = attr(y, "df")
  y$Operator = substr(mutator, 3L, nchar(mutator) - 8L)
  return(y)
})
res = do.call(rbind, res)
res$Operator = factor(res$Operator, levels = sapply(mutators, function(x)
  substr(x, 3L, nchar(x) - 8L)), ordered = TRUE)
res$Mutated = ifelse(res$mutated, "yes", "no")


plotMutatorEffects = function(res) {
  pl = ggplot(res)
  pl = pl + geom_point(aes(x = x1, y = x2, color = Mutated, shape = Mutated))
  pl = pl + theme_bw()
  pl = pl + theme(legend.position = "top")
  pl = pl + scale_color_manual(breaks = c("yes", "no"), values = c("gray40", "black"))
  pl = pl + scale_shape_manual(breaks = c("yes", "no"), values = c(3, 18))
  pl = pl + xlim(c(0, 1)) + ylim(c(0, 1))
  pl = pl + labs(
    x = expression(x[1]), y = expression(x[2L]),
    shape = "Mutation applied",
    colour = "Mutation applied")
  pl = pl + facet_wrap(.~Operator, ncol = 5L, labeller = label_both)
  return(pl)
}


pl = plotMutatorEffects(res)

# Only old mutators
pl = plotMutatorEffects(res[res$Operator %in% c("InitialSolution", "Uniform", "Normal"), , drop = FALSE])
ggsave("images/operator-show-run-old.pdf", plot = pl, width = 7, height = 3.5, device = cairo_pdf)


pl = plotMutatorEffects(res[res$Operator %in% c("InitialSolution", "Explosion", "Implosion"), , drop = FALSE])
ggsave("images/operator-show-run-new-1.pdf", plot = pl, width = 7, height = 3.5, device = cairo_pdf)

pl = plotMutatorEffects(res[res$Operator %in% c("InitialSolution", "Cluster", "Grid"), , drop = FALSE])
ggsave("images/operator-show-run-new-2.pdf", plot = pl, width = 7, height = 3.5, device = cairo_pdf)

pl = plotMutatorEffects(res[res$Operator %in% c("InitialSolution", "Expansion", "Compression"), , drop = FALSE])
ggsave("images/operator-show-run-new-3.pdf", plot = pl, width = 7, height = 3.5, device = cairo_pdf)

