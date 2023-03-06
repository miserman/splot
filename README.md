# splot
An R package to ease data visualization.

The aim of this package is to make visualization an early part of the data analysis process by automating a few common plotting tasks.

In terms of design, it has three general principles:

* **Flexibility**: splot prefers to try to make a reasonable plot from any input, rather than erroring out.
* **Minimal specification**: You should be able to make most plots with just a formula input, and the same formula should generally be compatible with multiple plot types, regardless of variable types.
* **Tweakability**: Though splot is focused on quick, automated plotting, you should be able to adjust any aspect of the display with additional arguments, if you find a plot you want to display elsewhere.

## features
By entering a formula as the first argument in the `splot` function (e.g., `splot(y ~ x)`), you can make

* Density distributions (overlaid on histograms when there is no `by` variable)
* Scatter plots with prediction lines
* Bar or line graphs with error bars

For each type, multiple `y` variables or data at levels of a `by` variable are shown in the same plot frame,<br />
and data at levels of one or two `between` variables are shown in separate plot frames, organized in a grid.

## resources
* [Introduction](https://miserman.github.io/splot/articles/intro.html)
* [Documentation](https://miserman.github.io/splot/reference/splot.html)  
* [Style Guide](https://miserman.github.io/splot/articles/style.html)  
* [Gallery](https://miserman.github.io/splot/articles/gallery.html)
* Applied examples: [Exploring Data](https://miserman.github.io/splot/articles/explore.html) |
[Refining a Result](https://miserman.github.io/splot/articles/refine.html)

## installation
Download R from [r-project.org](https://www.r-project.org/).

Release ([version 0.5.3](https://CRAN.R-project.org/package=splot))
```R
install.packages('splot')
```
Development (version 0.5.4)
```R
# install.packages('remotes')
remotes::install_github('miserman/splot')
```
Then load the package:
```R
library(splot)
```
## examples
Make some data: random group and x variables, and a y variable related to x:
```R
group = rep(c('group 1', 'group 2'), 50)
x = rnorm(100)
y = x * .5 + rnorm(100)
```
The distribution of y:
```R
splot(y)
```
A scatter plot between y and x:
```R
splot(y ~ x)
```
Same data with a quadratic model:
```R
splot(y ~ x + x^2 + x^3)
```
Same data separated by group:
```R
splot(y ~ x * group)
```
Could also separate by median or standard deviations of x:
```R
splot(y ~ x * x)
splot(y ~ x * x, split='sd')
```
Summarize with a bar plot:
```R
splot(y ~ x * group, type='bar')
```
Two-level y variable with a probability prediction line:
```R
# make some new data for this example:
# a discrete y variable and related x variable:
y_bin = rep(c(1, 5), 50)
x_con = y_bin * .4 + rnorm(100)

# lines = 'prob' for a prediction line from a logistic model:
splot(y_bin ~ x_con, lines = 'prob')
```
