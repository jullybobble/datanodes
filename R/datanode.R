#' Cache the result of a given expression.
#'
#' Evaluates an expression and caches its result. If a cache exists, the
#' expression is re-evaluated only if one of the dependency files is more
#' recent than the cache.
#'
#' The given expression \code{expr} is evaluated only if one of the
#' following condition is true:
#'
#' \itemize {
#'  - the file identified in \code{path} does *not* exist; or
#'  - \code{force} is \code{TRUE}; or
#'  - the latest modified time of the files in \code{depends_on} is later than
#'    the modified time of the file in \code{path}; or
#'  - \code{ask} is \code{TRUE} and the user answers \code{y} at the console
#'    (only considered in interactive mode).
#' }
#'
#' @param path the file caching the result of \code{expr}
#' @param expr the expression to be evaluated if triggered
#' @param force whether to force the evaluation of the expression \code{expr}
#'        and the update of its cache, defaults to \code{TRUE}
#' @param depends_on a character vector of files on which the evaluation of the
#'        expression \code{expr} depends on.
#' @param ask if evaluation was not triggered and R in is interactive mode, the
#'        user will be promted to evaluate the expression or to use the cache
#'
#' @return the result of the evaluation of the expression \code{expr} if
#'         triggered, or its cached value stored in \code{path} otherwise
#' @import readr
#' @export
datanode <- function(path,
                     expr,
                     force = FALSE,
                     depends_on = character(0),
                     ask = F) {
  triggered <-
    force ||
    !file.exists(path) ||
    (!is.null(depends_on) && length(depends_on) != 0 &&
       file_time_trigger(path, depends_on)) ||
    (interactive() ||
       (ask &&
          ask_trigger(path)))


  if(triggered) {
    result <- expr
    dn_save(result, path)
    result
  } else {
    dn_load(path)
  }
}

file_modif_time <- function(path) file.info(path)$mtime

file_time_trigger <- function(path, depends_on) {
  !is.null(depends_on) &&
    file_modif_time(path) < max(sapply(depends_on, file_modif_time))
}

ask_trigger <- function(path) {
  if(!interactive()) {
    TRUE
  }
  answer <- readline(sprintf("Recompute data in '%s'? [y|N] ", path))
  if (answer == "" || trimws(tolower(answer)) == "n") {
    FALSE
  } else if (trimws(tolower(answer)) == "y") {
    TRUE
  } else {
    message("can not parse answer: ", answer)
    ask_trigger(path)
  }
}

dn_load <- function(path) {
  new_env <- new.env()
  load(path, envir = new_env)
  new_env$data
}

dn_save <- function(data, path) {
  save(data, file = path)
  data
}

