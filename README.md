# splot
An R package aimed at automating a few common visualization tasks in order to ease data exploration.

## features
You can make any of these with standard R packages, but they can sometimes be a hassle to set up:

* Density distributions (overlaid on histograms when there is no `by` variable)
* Scatter plots with prediction lines
* Bar or line graphs with error bars


For each type...

Multiple `y` variables or data at levels of a `by` variable are shown in the same plot frame,<br />
and data at levels of one or two `between` variable are shown in separate plot frames, organized in a grid.

## resources
* [Introduction](https://miserman.github.io/splot/)
* [Documentation](https://miserman.github.io/splot/#main_function)  
* [Style Guide](https://miserman.github.io/splot/#style)  
* [Gallery](https://miserman.github.io/splot/#gallery)
* Applied examples: [Exploring Data](https://miserman.github.io/splot/#explore) | [Refining a Result](https://miserman.github.io/splot/#refine)

## installation
Download R from [r-project.org](https://www.r-project.org/).

Release ([version 0.4.1](https://CRAN.R-project.org/package=splot))
```R
install.packages('splot')
```
Development (version 0.4.2)
```R
install.packages('devtools')
devtools::install_github('miserman/splot')
```
Then load the package:
```R
library(splot)
```
## examples
Make some data: random group and x variables, and a y variable related to x:
```R
group = rep(c('group 1','group 2'), 50)
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
y_bin = rep(c(1,5),50)
x_con = y_bin * .4 + rnorm(100)
splot(y_bin ~ x_con, lines='probability')
```