#' @param bound.handling [\code{character(1)}]\cr
#'   Occasionally during instance generation points are moved outside the boundaries
#'   of the point space. The parameter \code{bound.handling} determines how to deal
#'   with these points. Option \dQuote{uniform} places outliers uniform at random within
#'.  the boundaries while option \dQuote{boundary} places them on the corresponding
#'   violates boundary/boundaries.
#'   Default is \dQuote{uniform}.
