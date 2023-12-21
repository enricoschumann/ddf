if (requireNamespace("tinytest", quietly = TRUE))
    tinytest.results <- tinytest::test_package("ddf",
                                               color = interactive(),
                                               verbose = 1)
