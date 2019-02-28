#' @title
#' Compression mutation
#'
#' @description This operator is very similar to the expansion mutation (see
#' \code{\link{doExpansionMutation}}). Hence we refer the reader to the corresponding
#' documentation for details. The main difference is that instead of moving points
#' away from their orthogonal projection (expansion) points are attracted by their
#' orthogonal projections on the linear function.
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
doCompressionMutation = function(coords, min.eps = 0.1, max.eps = 0.3, ...) {
  doTubeMutation(coords, min.eps, max.eps, type = "Compression", ...)
}
