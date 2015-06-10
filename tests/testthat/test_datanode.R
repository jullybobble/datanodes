test_data <-
  data.frame(ID = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10"),
             Age = c(26L, 25L, 23L, 32L, 24L, 20L, 29L, 24L, 21L, 33L),
             Sex = factor(c("Male", "Female", "Male", "Female", "Male", "Male",
                          "Female", "Female", "Female", "Female")),
             Height = c(68, 70, 71, 73, 68, 66, 70, 71, 74, 70),
             Died = c(TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE),
             Date = as.Date(c("2014-07-10", "2014-10-10", "2014-11-24", "2014-11-10", "2014-12-10",
                              "2014-12-10", "2015-01-10", "2015-03-10", "2015-05-10", "2015-05-14")),
             stringsAsFactors = F)

csv_read_args <- list(col_types = list(ID = col_character(),
                                       Height = col_double(),
                                       Sex = col_factor(c("Female", "Male"))))

test_that("dn create file if it doesn't exist", {
  file <- tempfile()
  data <- test_data
  expect_false(file.exists(file))
  returned <- datanode(file, data)
  expect_true(file.exists(file))

  # clean file
  file.remove(file)
})

test_that("dn returns identical object when missed", {
  file <- tempfile()
  data <- test_data
  expected <- rbind(data, data)
  returned <- datanode(file, rbind(data, data))
  expect_equal(returned, expected)

  # clean file
  file.remove(file)
})

test_that("dn returns identical object when hit", {
  file <- tempfile()
  data <- test_data
  expected <- rbind(data, data)
  datanode(file, rbind(data, data))
  expect_true(file.exists(file))
  returned <- datanode(file, rbind(data, data))
  expect_equal(returned, expected)

  # clean file
  file.remove(file)
})

test_that("dn doesn't evaluate expression when hit", {
  file <- tempfile()
  expected <- datanode(file, test_data)
  file_time <- file.info(file)$mtime

  returned <- datanode(file, stop())
  expect_equal(returned, expected)
  expect_identical(file.info(file)$mtime,
                   file_time)

  # clean file
  file.remove(file)
})

test_that("dn respects `force` parameter", {
  file <- tempfile()
  data <- test_data
  expected <- data
  datanode(file, data)
  expect_true(file.exists(file))
  evaluated = F
  datanode(file, force = F, {
    evaluated <- T
  })
  expect_false(evaluated)
  datanode(file, force = T, {
    evaluated <- T
  })
  expect_true(evaluated)

  # clean file
  file.remove(file)
})

test_that("a data node gets triggered by `file_time` with file dependency", {

  # dependency
  dep_file <- tempfile()
  dep <- datanode(dep_file, test_data)

  # tested node
  file <- tempfile()
  dep_fun <- function(data) rbind(data, data)
  node <- datanode(file,
                   depends_on = dep_file, {
    dep_fun(dep)
  })
  file_time_1 <- file.info(file)$mtime

  Sys.sleep(1) # makes sure file system time resolution is
                 # sufficient (1s should be enough everywhere)

  # test miss
  dep_2 <- datanode(dep_file, test_data, force = T)
  node <- datanode(file,
                   depends_on = dep_file, {
    dep_fun(dep_2)
  })
  expect_less_than(file_time_1,
                   file.info(file)$mtime)
  expect_equivalent(dep_fun(dep_2),
                    node)

  # clean temp files
  file.remove(dep_file)
  file.remove(file)
})

test_that("rdata io reads what was written", {
  f <- tempfile()
  on.exit(file.remove(f))

  data <- test_data

  datanode(f, io = rdata_io, data)

  expect_equivalent(datanode(f, io = rdata_io, data),
                    data)

})

test_that("csv io reads what was written", {
  f <- tempfile()
  on.exit(file.remove(f))

  data <- test_data

  datanode(f, io = csv_io, data)

  expect_equivalent(datanode(f,
                             io = csv_io,
                             read_args = csv_read_args,
                             NULL),
                    data)

})


test_that("csv io is selected if file ends in '.csv'", {
  f <- tempfile(fileext = '.cSv')
  on.exit(file.remove(f))

  data <- test_data

  datanode(f, data)

  expect_equivalent(csv_io$read(f,
                                list(col_types = csv_read_args$col_types)),
                    data)
})


test_that("RData io is selected if file ends in '.RData'", {
  f <- tempfile(fileext = '.rDaTa')
  on.exit(file.remove(f))

  data <- test_data

  datanode(f, data)

  expect_equivalent(rdata_io$read(f),
                    data)
})

test_that("RDS io is selected if file ends in 'rds'", {
  f <- tempfile(fileext = '.rdS')
  on.exit(file.remove(f))

  data <- test_data

  datanode(f, data)

  expect_equivalent(rds_io$read(f),
                    data)
})




