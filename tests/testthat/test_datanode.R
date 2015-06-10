require(wakefield, warn.conflicts = F)
require(dplyr, warn.conflicts = F)

random_data <- function() r_data_frame(n = 10, id,  age, sex, height, died, date_stamp)
csv_read_args <- list(col_types = list(ID = col_character(),
                                       Height = col_double(),
                                       Sex = col_factor(c("Female", "Male"))))

test_that("dn create file if it doesn't exist", {
  file <- tempfile()
  data <- random_data()
  file.exists(file) %>% expect_false()
  returned <- datanode(file, data)
  file.exists(file) %>% expect_true()

  # clean file
  file.remove(file)
})

test_that("dn returns identical object when missed", {
  file <- tempfile()
  data <- random_data()
  expected <- data %>% bind_rows(data)
  returned <- datanode(file, data %>% bind_rows(data))
  expect_equal(returned, expected)

  # clean file
  file.remove(file)
})

test_that("dn returns identical object when hit", {
  file <- tempfile()
  data <- random_data()
  expected <- data %>% bind_rows(data)
  datanode(file, data %>% bind_rows(data))
  file.exists(file) %>% expect_true()
  returned <- datanode(file, data %>% bind_rows(data))
  expect_equal(returned, expected)

  # clean file
  file.remove(file)
})

test_that("dn doesn't evaluate expression when hit", {
  file <- tempfile()
  expected <- datanode(file, random_data())
  file_time <- file.info(file) %>% with(mtime)

  returned <- datanode(file, stop())
  expect_equal(returned, expected)
  expect_identical(file.info(file) %>% with(mtime),
                   file_time)

  # clean file
  file.remove(file)
})

test_that("dn respects `force` parameter", {
  file <- tempfile()
  data <- random_data()
  expected <- data
  datanode(file, data)
  file.exists(file) %>% expect_true()
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
  dep <- datanode(dep_file, random_data())

  # tested node
  file <- tempfile()
  dep_fun <- function(data) bind_rows(data, data)
  node <- datanode(file,
                   depends_on = dep_file, {
    dep_fun(dep)
  })
  file_time_1 <- file.info(file) %>% with(mtime)

  Sys.sleep(1) # makes sure file system time resolution is
                 # sufficient (1s should be enough everywhere)

  # test miss
  dep_2 <- datanode(dep_file, random_data(), force = T)
  node <- datanode(file,
                   depends_on = dep_file, {
    dep_fun(dep_2)
  })
  expect_less_than(file_time_1,
                   file.info(file) %>% with(mtime))
  expect_equivalent(dep_fun(dep_2),
                    node)

  # clean temp files
  file.remove(dep_file)
  file.remove(file)
})

test_that("rdata io reads what was written", {
  f <- tempfile()
  on.exit(file.remove(f))

  data <- random_data()

  datanode(f, io = rdata_io, data)

  expect_equivalent(datanode(f, io = rdata_io, data),
                    data)

})

test_that("csv io reads what was written", {
  f <- tempfile()
  on.exit(file.remove(f))

  data <- random_data() %>%
    as.data.frame() # all.equal.tbl_df has problems with Date after csv write-read

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

  data <- random_data() %>%
    as.data.frame() # all.equal.tbl_df has problems with Date after csv write-read

  datanode(f, data)

  expect_equivalent(csv_io$read(f,
                                list(col_types = csv_read_args$col_types)),
                    data)
})


test_that("RData io is selected if file ends in '.RData'", {
  f <- tempfile(fileext = '.rDaTa')
  on.exit(file.remove(f))

  data <- random_data() %>%
    as.data.frame() # all.equal.tbl_df has problems with Date after csv write-read

  datanode(f, data)

  expect_equivalent(rdata_io$read(f),
                    data)
})

test_that("RDS io is selected if file ends in 'rds'", {
  f <- tempfile(fileext = '.rdS')
  on.exit(file.remove(f))

  data <- random_data() %>%
    as.data.frame() # all.equal.tbl_df has problems with Date after csv write-read

  datanode(f, data)

  expect_equivalent(rds_io$read(f),
                    data)
})




