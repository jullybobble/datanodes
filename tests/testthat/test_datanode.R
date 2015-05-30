require(wakefield)

random_data <- function() r_data_frame(n = 10, id,  age, sex, height, died, date_stamp)

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

