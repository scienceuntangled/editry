context("general tests")

test_that("internal utilities work as expected", {
    chk <- editry:::nested_merge(list(a = 1, b = list(x = 2)), list(b = list(x = 3, y = 4), c = 5))
    expect_identical(chk, list(a = 1, b = list(x = 2, y = 4), c = 5))
    chk <- editry:::nested_merge(list(a = 1, b = 2), list(b = list(x = 3, y = 4), c = 5))
    expect_identical(chk, list(a = 1, b = 2, c = 5))
})
