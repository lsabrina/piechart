pkgname <- "piechart"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('piechart')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("piechart")
### * piechart

flush(stderr()); flush(stdout())

### Name: piechart
### Title: Draw a 2-dimensional pie chart
### Aliases: piechart

### ** Examples

piechart(rep(1, 24), col = rainbow(24), radius = 0.9)

pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
names(pie.sales) <- c("Blueberry", "Cherry", "Apple", 
"Boston Cream", "Other", "Vanilla Cream")
piechart(pie.sales) # default colours
piechart(pie.sales, col = c("purple", "violetred1", "green3", 
"cornsilk", "cyan", "white"))



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
