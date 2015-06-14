#' Cache the result of a given expression.
#'
#' Evaluates an expression and caches its result. If a cache exists, the
#' expression is re-evaluated only if one of the dependency files is more
#' recent than the cache.
#'
#' The given expression \code{expr} is evaluated only if one of the
#' following condition is true:
#'
#' \itemize{
#'  - the file identified in \code{path} does *not* exist; or
#'  - \code{force} is \code{TRUE}; or
#'  - the latest modified time of the files in \code{depends_on} is later than
#'    the modified time of the file in \code{path}.
#' }
#'
#' @param path the file caching the result of \code{expr}
#' @param expr the expression to be evaluated if triggered
#' @param force whether to force the evaluation of the expression \code{expr}
#'        and the update of its cache, defaults to \code{TRUE}
#' @param depends_on a character vector of files on which the evaluation of the
#'        expression \code{expr} depends on.
#' @param io a list with two named function \code{read} and \code{write}.
#'        \code{read} takes \code{path} and \code{args} as arguments,
#'        and \code{write} takes in addition \code{data}, \code{path} and
#'        \code{args}. \code{args} are a list of additional arguments possibly
#'        to the underlying reading or writing functions. Implementations for
#'        \code{\link{csv_io}}, \code{\link{rdata_io}} and \code{\link{rds_io}}
#'        are provided. The default value depends on the extension of the
#'        file described by the \code{path} parameter: \code{.csv},
#'        \code{.RData} and \code{.rds} corresponding to the 3 \code{io}
#'        implementations, defaulting to \code{rds_io}.
#' @param write_args a list of additional parameters to the \code{io$write} function.
#' @param read_args a list of additional parameters to the \code{io$read} function.
#' @param ... additinal parameters to the \code{io$read} function, concatenated
#'        after the list in \code{read_args}.
#'
#' @return the result of the evaluation of the expression \code{expr} if
#'         triggered, or its cached value stored in \code{path} otherwise
#' @import readr
#' @export
datanode <- function(path,
                     expr,
                     force = FALSE,
                     depends_on = character(0),
                     io = if (grepl("(?i).*\\.csv$", path)) csv_io
                     else if (grepl("(?i).*\\.RData$", path)) rdata_io
                     else rds_io,
                     write_args = NULL,
                     read_args = NULL,
                     ...) {
  triggered <-
    force ||
    !file.exists(path) ||
    (!is.null(depends_on) && length(depends_on) != 0 &&
       file_time_trigger(path, depends_on))

  if(triggered) {
    result <- expr
    io$write(data = result, path = path, args = write_args)
    result
  } else {
    io$read(path = path, args = read_args)
  }
}

file_modif_time <- function(path) file.info(path)$mtime

file_time_trigger <- function(path, depends_on) {
  !is.null(depends_on) &&
    file_modif_time(path) < max(sapply(depends_on, file_modif_time))
}

