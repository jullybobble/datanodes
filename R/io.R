read_rdata <- function(path, args = NULL) {
  new_env <- new.env()
  args <- c(list(path, envir = new_env), args)
  do.call(load, args = args)
  new_env$data
}

write_rdata <- function(data, path, args = NULL) {
  args <- c(list("data", file = path), args)
  do.call(save, args)
  data
}

#' Read and write \code{.RData} files.
#'
#' @seealso \code{\link{datanode}}
#'
#' @export
rdata_io <- list(read = read_rdata,
                 write = write_rdata)

#' @import readr
read_csv <- function(path, args = NULL) {
  args <- c(list(file = path), args)
  do.call(readr::read_csv, args)
}

#' @import readr
write_csv <- function(data, path, args = NULL) {
  args <- c(list(x = data, path = path), args)
  do.call(readr::write_csv, args)
  data
}

#' Read and write \code{.csv} files.
#'
#' @seealso \code{\link{datanode}}
#'
#' @export
csv_io <- list(read = read_csv,
               write = write_csv)

read_rds <- function(path, args = NULL) {
  args <- c(list(file = path), args)
  do.call(readRDS, args)
}

write_rds <- function(data, path, args = NULL) {
  args <- c(list(object = data, file = path), args)
  do.call(saveRDS, args)
  data
}

#' Read and write \code{.rds} files.
#'
#' @seealso \code{\link{datanode}}
#'
#' @export
rds_io <- list(read = read_rds,
               write = write_rds)
