old <- data.frame(key = 1:3, labels = c("a", "b", "c"))
new <- data.frame(key = 3:4, labels = c("d", "e"))
d <- ddf(new, old, by = "key")

expect_equal(nrow(d$added), 1)
expect_equal(nrow(d$removed), 2)

expect_equal(ddf(new, old, by = 1),
             ddf(new, old, by = "key"))
