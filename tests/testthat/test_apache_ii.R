context("Testing APACHE II score generating functions")

# First arg = description of function
# Subsequent args = function for testing e.g. expect_equal



test_that("Check neuro score generator apache ii component.", {
        dd <- data.table(
                gcs   = c(10, 12, 13, 15, 3, 5, 5, 7, 15, 15),
                time = c(0,30,0, 24,0,0,0, 22, 26, 70),
                id  = c(1,1,2, 3, 4, 5, 6, 7,7, 8) )
        # print(dd)
        gen_neuro(dd, "apache ii", gcs_ = gcs, hours_=time, id_=id)
        expect_equivalent(dd$apache_n, c(5, 5, 2, 0, 12, 10, 10, 8, 8, NA))
        expect_error(suppressWarnings(gen_neuro(dd,"apache iii")))
})