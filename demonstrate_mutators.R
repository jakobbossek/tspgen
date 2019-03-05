library(ggplot2)
library(gridExtra)
devtools::load_all(".")

n = 1000L
pm = 0.4
min.eps = 0.3
max.eps = 0.3
jitter.sd = 0.05
p.rot = 0
p.jitter = 0
iters = 1000L

x = generateRandomNetwork(1000L, lower = 0, upper = 1)
x.coords = x$coordinates
mutators = c("doExplosionMutation", "doImplosionMutation", "doRotationMutation",
  "doClusterMutation", "doExpansionMutation", "doCompressionMutation",
  "doAxisProjectionMutation", "doLinearProjectionMutation", "doGridMutation")
#set.seed(123)
for (mutator in mutators) {
  set.seed(1)
  y = do.call(mutator, list(coords = x.coords, pm = pm, min.eps = min.eps, max.eps = max.eps, box.min = min.eps, box.max = max.eps, p.jitter = p.jitter, jitter.sd = jitter.sd))
  grid.arrange(autoplot(x), autoplot(makeNetwork(y)) + xlim(c(0, 1)) + ylim(c(0, 1)))
  BBmisc::pause()
}
