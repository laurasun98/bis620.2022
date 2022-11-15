test_that(
  "The accel_plot() returns a ggplot object.",
  {
    data(ukb_accel)
    p <-  accel_plot(ukb_accel[1:100, ])
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The accel_plot() errors when no time or freq column.",
  {
    data(iris)
    expect_error(accel_plot(iris))
  }
)

test_that(
  "The accel_plot() is correct for time-series data.",
  {
    data(ukb_accel)
    p <- accel_plot(ukb_accel[1:100, ])
    expect_doppelganger("first-100-samples", p)
  }
)

test_that(
  "we get a data frame for spectral_signature.",
  {
    data(ukb_accel)
    p <- spectral_signature(ukb_accel[1:100, ])
    expect_true(inherits(p, "data.frame"))
  }

)

test_that(
  "we get a data frame for spectral_signature, case when take_log=True.",
  {
    data(ukb_accel)
    p <- spectral_signature(ukb_accel[1:100, ], take_log = TRUE)
    expect_true(inherits(p, "data.frame"))
  }

)
