test_that("list_params is a list of one parameter per element ", {
  expect_true({
    par_list <- param_list(
      gamma = 1,
      lambda_0 = 2,
      theta = 3,
      eta = 1,
      mu = 1,
      s = 5
    )
    t1 <- is.list(par_list)
    t2 <- length(par_list) == length(unlist(par_list))
    t1 & t2
  })

})

test_that("simiulation output is a list", {
  expect_true({
par_list <- param_list(
  gamma = 1,
  lambda_0 = 2,
  theta = 3,
  eta = 1,
  mu = 1,
  s = 5
)
res <- full_simulation(n = 100,params = par_list,m = 1e4)
is.list(res)
})
})
