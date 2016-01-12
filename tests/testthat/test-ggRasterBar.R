################################################################################
# Use testthat to test phyloseq use of mapproj projections
################################################################################
library("ggRasterBar")
library("testthat")
context("Check basic Plots")

test_that("ggRasterBar makes a ggplot", {
    df =  data.frame(names= c("A","B","C","D"), values=c(100,80,40,20))
    plt = rasterbar(df, x="names", y="values", image="BoA", reorder=TRUE)
    expect_is(plt, "ggplot")
})
