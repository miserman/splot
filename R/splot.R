#' Split Plot
#'
#' A plotting function aimed at automating some common visualization tasks in order to ease data exploration.
#' @param y a formula (see note), or the primary variable(s) to be shown on the y axis (unless \code{x} is not specified).
#'   When not a formula, this can be one or more variables as objects, or names in \code{data}.
#' @param data a \code{data.frame} to pull variables from. If variables aren't found in \code{data}, they will be looked
#'   for in the environment.
#' @param su a subset to all variables, applied after they are all retrieved from \code{data} or the environment.
#' @param type determines the type of plot to make, between \code{"bar"}, \code{"line"}, \code{"density"}, or
#'   \code{"scatter"}. If \code{"density"}, \code{x} is ignored. Anything including the first letter of each is accepted
#'   (e.g., \code{type='l'}).
#' @param split how to split any continuous variables (those with more than \code{lim} levels as factors). Default is
#'   \code{"median"}, with \code{"mean"}, \code{"standard deviation"}, \code{"quantile"}, or numbers as options. If
#'   numbers, the variable is either cut at each value in a vector, or broken into roughly equal chunks. Entering an
#'   integer (e.g., \code{split = 3L}) that is greater than 1 will force splitting into segments. Otherwise variables will
#'   be split by value if you enter a single value for split and there are at least two data less than or equal to and
#'   greater than the split, or if you enter more than 1 value for split. If a numeric split is not compatible with
#'   splitting by value or segment, splitting will default to the median.
#' @param levels a list with entries corresponding to variable names, used to rename and/or reorder factor levels. To
#'   reorder a factor, enter a vector of either numbers or existing level names in the new order (e.g.,
#'   \code{levels =} \code{list(var =} \code{c(3,2,1))}). To rename levels of a factor, enter a character vector the same
#'   length as the number of levels. To rename and reorder, enter a list, with names as the first entry, and order as the
#'   second entry (e.g., \code{levels =} \code{list(var =} \code{list(c('a','b','c'),} \code{c(3,2,1)))}). This happens
#'   after variables are split, so names and orders should correspond to the new split levels of split variables. For
#'   example, if a continuous variable is median split, it now has two levels ('Under Median' and 'Over Median'), which are
#'   the levels reordering or renaming would apply to. Multiple variables entered as \code{y} can be renamed and sorted
#'   with an entry titled 'mv'.
#' @param sort specified the order of character or factor \code{x} levels. By default, character or factor \code{x} levels
#'   are sorted alphabetically. \code{FALSE} will prevent this (preserving entered order). \code{TRUE} or \code{'d'} will
#'   sort by levels of \code{y} in decreasing order, and anything else will sort in increasing order.
#' @param error string; sets the type of error bars to show in bar or line plots, or turns them off. If \code{FALSE}, no
#'   error bars will be shown. Otherwise, the default is \code{"standard error"} (\code{'^s'}), with \code{"confidence
#'   intervals"} (anything else) as an option.
#' @param error.color color of the error bars. Default is \code{'#585858'}.
#' @param error.lwd line weight of error bars. Default is 2.
#' @param lim numeric; checked against the number of factor levels of each variable. Used to decide which variables should
#'   be split, which colors to use, and when to turn off the legend. Default is \code{9}. If set over \code{20}, \code{lim}
#'   is treated as infinite (set to \code{Inf}).
#' @param lines logical or a string specifying the type of lines to be drawn in scatter plots. By default (and whenever
#'   \code{cov} is not missing, or if \code{lines} matches \code{'^li|^lm|^st'}), a prediction line is fitted with
#'   \code{\link[stats]{lm}}. For (potentially) bendy lines, \code{'loess'} (matching \code{'^loe|^po|^cu'}) will use
#'   \code{\link[stats]{loess}}, and \code{'spline'} (\code{'^sm|^sp|^in'}) will use \code{\link[stats]{smooth.spline}}.
#'   If \code{y} is not numeric and has only 2 levels, \code{'probability'} (\code{'^pr|^log'}) will draw probabilities
#'   estimated by a logistic regression (\code{glm(y ~} \code{x, binomial)}). \code{'connected'} (\code{'^e|^co|^d'}) will
#'   draw lines connecting all points, and \code{FALSE} will not draw any lines.
#' @param colors sets a color theme or manually specifies colors. Default theme is \code{"pastel"}, with \code{"dark"} and
#'   \code{"bright"} as options; these are passed to \code{\link{splot.color}}. If set to \code{"grey"}, or if \code{by}
#'   has more than 9 levels, a grey scale is calculated using \code{\link[grDevices]{grey}}. See the \code{col} parameter
#'   in \code{\link[graphics]{par}} for acceptable manual inputs. To set text and axis colors, \code{col} sets outside
#'   texts (title, sud, labx, laby, and note), \code{col.sub} or \code{col.main} sets the frame titles, and \code{col.axis}
#'   sets the axis text and line colors. To set the color of error bars, use \code{error.color}. For histograms, a vector of
#'   two colors would apply to the density line and bars separately (e.g., for \code{color =} \code{c('red','green')}, the
#'   density line would be red and the histogram bars would be green). See the \code{color.lock} and \code{color.offset}
#'   arguments for more color controls.
#' @param ... passes additional arguments to \code{\link[graphics]{par}} or \code{\link[graphics]{legend}}. Arguments before
#'   this can be named partially; those after must by fully named.
#' @param colorby a variable or list of arguments used to set colors and the legend, alternatively to \code{by}. If
#'   \code{by} is not missing, \code{colorby} will be reduced to only the unique combinations of \code{by} and \code{colorby}.
#'   For example, if \code{by} is a participant ID with multiple observations per participant, and \code{by} is a condition
#'   ID which is the same for all observations from a given participant, \code{colorby} would assign a single color to each
#'   participant based on their condition. A list will be treated as a call to \code{link{splot.color}}, so arguments can be
#'   entered positionally or by name. Data entered directly into splot can be accessed by position name preceded by a
#'   period. For example, \code{splot(rnorm(100),} \code{colorby=.y)} would draw a histogram, with bars colored by the value
#'   of \code{y} (\code{rnorm(100)} in this case).
#' @param colorby.leg logical; if \code{FALSE}, a legend for \code{colorby} is never drawn. Otherwise, a legend for
#'   \code{colorby} will be drawn if there is no specified \code{by}, or for non-scatter plots (overwriting the usual legend).
#' @param color.lock logical; if \code{FALSE}, colors will not be adjusted to offset lines from points or histogram bars.
#' @param color.offset how much points or histogram bars should be offset from the initial color used for lines. Default is
#'   1.1; values greater than 1 lighten, and less than 1 darken.
#' @param color.summary specifies the function used to collapse multiple colors for a single display. Either a string
#'   matching one of \code{'mean'} (which uses \code{\link{splot.colormean}} to average RGB values), \code{'median'} (
#'   which treats codes as ordered, and selects that at the rounded median), or \code{'mode'} (which selects the most
#'   common code), or a function which takes color codes in its first argument, and outputs a single color code as a
#'   character.
#' @param opacity a number between 0 and 1; sets the opacity of points, lines, and bars. Semi-opaque lines will sometimes
#'   not be displayed in the plot window, but will show up when the plot is written to a file.
#' @param x secondary variable, to be shown in on the x axis. If not specified, \code{type} will be set to \code{'density'}.
#'   If \code{x} is a factor or vector of characters, or has fewer than \code{lim} levels when treated as a factor,
#'   \code{type} will be set to \code{'line'} unless specified.
#' @param by the 'splitting' variable within each plot, by which the plotted values of \code{x} and \code{y} will be
#'   grouped.
#' @param between a single object or name, or two in a vector (e.g., \code{c(b1, b2)}), the levels of which will determine
#'   the number of plot windows to be shown at once (the cells in a matrix of plots; levels of the first variable as rows,
#'   and levels of the second as columns).
#' @param cov additional variables used for adjustment. Bar and line plots include all \code{cov} variables in their
#'   regression models (via \code{\link[stats]{lm}}, e.g., \code{lm(y ~ 0 + x + cov1 + cov2)}) as covariates. Scatter plots
#'   with lines include all \code{cov} variables in the regression model to adjust the prediction line (e.g.,
#'   \code{lm(y ~ x + x^2)}).
#'   \code{\link[graphics]{par}} options \code{col}, \code{mfrow}, \code{oma}, \code{mar}, \code{mgp}, \code{font.main},
#'   \code{cex.main}, \code{font.lab}, \code{tcl}, \code{pch}, \code{lwd}, and \code{xpd} are all set within the function,
#'   but will be overwritten if they are included in the call. For example, \code{col} sets font colors in this case
#'   (as opposed to \code{colors} which sets line and point colors). The default is \code{'#303030'} for a nice dark grey,
#'   but maybe you want to lighten that up: \code{col='#606060'}. After arguments have been applied to
#'   \code{\link[graphics]{par}}, if any have not been used and match a \code{\link[graphics]{legend}} argument, these will
#'   be applied to \code{\link[graphics]{legend}}.
#' @param line.type a character setting the style of line (e.g., with points at joints) to be drawn in line plots. Default
#'   is \code{'b'} if \code{error} is \code{FALSE}, and \code{'l'} otherwise. See the \code{line} argument of
#'   \code{\link[graphics]{plot.default}} for options. \code{line.type='c'} can look nice when there aren't a lot of
#'   overlapping error bars.
#' @param mv.scale determines whether to center and scale multiple \code{y} variables. Does not center or scale by default.
#'   Anything other than \code{'none'} will mean center each numeric \code{y} variable. Anything matching \code{'^t|z|sc'}
#'   will also scale.
#' @param mv.as.x logical; if \code{TRUE}, variable names are displayed on the x axis, and \code{x} is treated as \code{by}.
#' @param save logical; if \code{TRUE}, an image of the plot is saved to the current working directory.
#' @param format the type of file to save plots as. default is \code{\link[grDevices]{cairo_pdf}}. See
#'   \code{\link[grDevices]{Devices}} for options.
#' @param dims a vector of 2 values (\code{c(width, height)}) specifying the dimensions of a plot to save in inches or
#'   pixels depending on \code{format}. Defaults to the dimensions of the plot window.
#' @param file.name a string with the name of the file to be save (excluding the extension, as this is added depending on
#'   \code{format}).
#' @param myl sets the range of the y axis (\code{ylim} of \code{\link[graphics]{plot}} or \code{\link[graphics]{barplot}}).
#'   If not specified, this will be calculated from the data.
#' @param mxl sets the range of the x axis (\code{xlim} of \code{\link[graphics]{plot}}). If not specified, this will be
#'   calculated from the data.
#' @param autori logical; if \code{FALSE}, the origin of plotted bars will be set to 0. Otherwise, bars are adjusted such
#'   that they extend to the bottom of the y axis.
#' @param xlas,ylas numeric; sets the orientation of the x- and y-axis labels. See \code{\link[graphics]{par}}.
#' @param xaxis,yaxis logical; if \code{FALSE}, the axis will not be drawn.
#' @param breaks determines the width of histogram bars. See \code{\link[graphics]{hist}}.
#' @param density.fill logical; \code{FALSE} will turn off polygon fills when they are displayed, \code{TRUE} will replace
#'   histograms with polygons.
#' @param density.opacity opacity of the density polygons, between 0 and 1.
#' @param density.args list of arguments to be passed to \code{\link[stats]{density}}.
#' @param leg sets the legend inside or outside the plot frames (when a character matching \code{'^i'}, or a character
#'   matching \code{'^o'} or a number respectively), or turns it off (when \code{FALSE}). When inside, a legend is drawn in
#'   each plot frame. When outside, a single legend is drawn either to the right of all plot frames, or within an empty
#'   plot frame. By default, this will be determined automatically, tending to set legends outside when there are multiple
#'   levels of \code{between}. A number will try and set the legend in an empty frame within the grid of plot frames. If
#'   there are no empty frames, the legend will just go to the side as if \code{leg='outside'}.
#' @param lpos sets the position of the legend within its frame (whether inside or outside of the plot frames) based on
#'   keywords (see \code{\link[graphics]{legend}}. By default, when the legend is outside, \code{lpos} is either
#'   \code{'right'} when the legend is in a right-hand column, or \code{'center'} when in an empty plot frame. When the
#'   legend is inside and \code{lpos} is not specified, the legend will be placed automatically based on the data. Set to
#'   \code{'place'} to manually place the legend; clicking the plot frame will set the top left corner of the legend.
#' @param lvn level variable name. Logical: if \code{FALSE}, the names of by and between variables will not be shown
#'   before their level (e.g., for a sex variable with a "female" level, "sex: female" would become "female" above each
#'   plot window).
#' @param leg.title sets the title of the legend (which is the by variable name by default), or turns it off with
#'   \code{FALSE}.
#' @param leg.args a list passing arguments to the \code{\link[graphics]{legend}} call.
#' @param title logical or a character: if \code{FALSE}, the main title is turned off. If a character, this will be shown
#'   as the main title.
#' @param labx,laby logical or a character: if \code{FALSE}, the label on the x axis is turned off. If a character, this
#'   will be shown as the axis label.
#' @param lty logical or a vector: if \code{FALSE}, lines are always solid. If a vector, changes line type based on each
#'   value. Otherwise loops through available line types, see \code{\link[graphics]{par}}.
#' @param lwd numeric; sets the weight of lines in line, density, and scatter plots. Default is 2. See
#'   \code{\link[graphics]{par}}.
#' @param sub affects the small title above each plot showing \code{between} levels; text replaces it, and \code{FALSE}
#'   turns it off.
#' @param note logical; if \code{FALSE}, the note at the bottom about splits and/or lines or error bars is turned off.
#' @param font named numeric vector: \code{c(title,sud,leg,leg.title,note)}. Sets the font of the title, su display, legend
#'   levels and title, and note. In addition, \code{font.lab} sets the x and y label font, \code{font.sub} sets the font of
#'   the little title in each panel, \code{font.axis} sets the axis label font, and \code{font.main} sets the between level/n
#'   heading font; these are passed to \code{\link[graphics]{par}}. See the input section.
#' @param cex named numeric vector: \code{c(title,sud,leg,note,points)}. Sets the font size of the title, su display, legend,
#'   note, and points. In addition, \code{cex.lab} sets the x and y label size, \code{cex.sub} sets the size of the little
#'   title in each panel, \code{cex.axis} sets the axis label size, and \code{cex.main} sets the between level/n heading size;
#'   these are passed to \code{\link[graphics]{par}}. See the input section.
#' @param sud affects the heading for subset and covariates/line adjustments (su display); text replaces it, and
#'   \code{FALSE} turns it off.
#' @param ndisp logical; if \code{FALSE}, n per level is no longer displayed in the subheadings.
#' @param labels logical; if \code{FALSE}, sets all settable text surrounding the plot to \code{FALSE} (just so you don't
#'   have to set all of them if you want a clean frame).
#' @param labels.filter a regular expression string to be replaced in label texts with a blank space. Default is
#'   \code{'_'}, so underscores appearing in the text of labels are replace with blank spaces. Set to
#'   \code{FALSE} to prevent all filtering.
#' @param labels.trim numeric or logical; the maximum length of label texts (in number of characters). Default is 20, with
#'   any longer labels being trimmed. Set to \code{FALSE} to prevent any trimming.
#' @param points logical; if \code{FALSE}, the points in a scatter plot are no longer drawn.
#' @param points.first logical; if \code{FALSE}, points are plotted after lines are drawn in a scatter plot, placing lines
#'   behind points. This does not apply to points or lines added in \code{add}, as that is always evaluated after the main
#'   points and lines are drawn.
#' @param byx logical; if \code{TRUE} (default) and \code{by} is specified, regressions for bar or line plots compare
#'   levels of \code{by} for each level of \code{x}. This makes for more intuitive error bars when comparing levels of
#'   \code{by} within a level of \code{x}; otherwise, the model is comparing the difference between the first level of
#'   \code{x} and each of its other levels.
#' @param drop named logical vector: \code{c(x,by,bet)}. Specifies how levels with no data should be treated. All are
#'   \code{TRUE} by default, meaning only levels with data will be presented, and the layout of \code{between} levels
#'   will be minimized. \code{x} only applies to bar or line plots. \code{by} relates to levels presented in the legend.
#'   If \code{bet} is \code{FALSE}, the layout of \code{between} variables will be strict, with levels of \code{between[1]}
#'   as rows, and levels of \code{between[2]} as columns -- if there are no data at an intersection of levels, the
#'   corresponding panel will be blank. See the input section.
#' @param prat panel ratio, referring to the ratio between plot frames and the legend frame when the legend is out. A
#'   single number will make all panels of equal width. A vector of two numbers will adjust the ratio between plot panels
#'   and the legend panel. For example, \code{prat=c(3,1)} makes all plot panels a relative width of 3, and the legend frame
#'   a relative width of 1.
#' @param check.height logical; if \code{FALSE}, the height of the plot frame will not be checked before plotting is
#'   attempted. The check tries to avoid later errors, but may prevent plotting when a plot is possible.
#' @param model logical; if \code{TRUE}, the summary of an interaction model will be printed. This model won't always align
#'   with what is plotted since variables may be treated differently, particularly in the case of interactions.
#' @param options a list with named arguments, useful for setting temporary defaults if you plan on using some of the same
#'   options for multiple plots (e.g., \code{opt = list(}\code{type = 'bar',} \code{colors = 'grey',}
#'   \code{bg = '#999999');} \code{splot(x ~ y,} \code{options = opt)}).
#'   use \code{\link[base]{quote}} to include options that are to be evaluated within the function (e.g.,
#'   \code{opt =} \code{list(su =} \code{quote(y > 0))}).
#' @param add evaluated within the function, so you can refer to the objects that are returned, to variable names (those
#'   from an entered data frame or entered as arguments), or entered data by their position, preceded by '.' (e.g.,
#'   \code{mod =} \code{lm(.y~.x)}). Useful for adding things like lines to a plot while the parameters are still
#'   those set by the function (e.g., \code{add =} \code{abline(v =} \code{mean(x),} \code{xpd = FALSE)} for a vertical
#'   line at the mean of x).
#'
#' @return A list containing data and settings is invisibly returned, which might be useful to check for errors.
#' Each of these objects can also be pulled from within \code{add}:
#' \tabular{ll}{
#'   \code{dat} \tab a \code{data.frame} of processed, unsegmented data.\cr
#'   \code{cdat} \tab a \code{list} of \code{list}s of \code{data.frame}s of processed, segmented data.\cr
#'   \code{txt} \tab a \code{list} of variable names. used mostly to pull variables from \code{data} or the environment.\cr
#'   \code{ptxt} \tab a \code{list} of processed variable and level names. Used mostly for labeling.\cr
#'   \code{seg} \tab a \code{list} containing segmentation information (such as levels) for each variable.\cr
#'   \code{ck} \tab a \code{list} of settings.\cr
#'   \code{lega} \tab a \code{list} of arguments that were or would have been passed to \code{\link[graphics]{legend}}.\cr
#'   \code{fmod} \tab an \code{lm} object if \code{model} is \code{TRUE}, and the model succeeded.
#' }
#'
#' @section Input:
#' \strong{formulas}
#'
#' When \code{y} is a formula (has a \code{~}), other variables will be pulled from it:
#'
#' \code{y ~ x * by * between[1] * between[2] + cov[1] + cov[2] + cov[n]}
#'
#' If \code{y} has multiple variables, \code{by} is used to identify the variable (it becomes a factor with variable names
#' as levels), so anything entered as \code{by} is treated as \code{between[1]}, \code{between[1]} is moved to
#' \code{between[2]}, and \code{between[2]} is discarded with a message.
#'
#' \strong{named vectors}
#'
#' Named vector arguments like \code{font}, \code{cex}, and \code{drop} can be set with a single value, positionally, or
#' with names. If a single value is entered (e.g., \code{drop = FALSE}), this will be applied to each level (i.e.,
#' \code{c(x = FALSE, by = FALSE, bet = FALSE)}). If more than one value is entered, these will be treated positionally
#' (e.g., \code{cex =} \code{c(2, 1.2)} would be read as \code{c(title = 2, sud = 1.2, leg = .9, note = .7, points = 1)}).
#' If values are named, only named values will be set, with other defaults retained (e.g., \code{cex =} \code{c(note = 1.2)}
#' would be read as \code{c(title = 1.5, sud = .9, leg = .9, note = 1.2, points = 1)}).
#'
#' @note
#' \strong{x-axis levels text}
#'
#' If the text of x-axis levels (those corresponding to the levels of \code{x}) are too long, they are hidden before
#' overlapping. To try and avoid this, by default longer texts are trimmed (dictated by \code{labels.trim}), and at some
#' point the orientation of level text is changed (settable with \code{xlas}), but you may still see level text missing.
#' To make these visible, you can reduce \code{labels.trim} from the default of 20 (or rename the levels of that variable),
#' make the level text vertical (\code{xlas = 3}), or expand your plot window if possible.
#'
#' \strong{missing levels, lines, and/or error bars}
#'
#' By default (if \code{drop = TRUE}), levels of \code{x} with no data are dropped, so you may not see every level of your
#' variable, at all or at a level of \code{by} or \code{between}. Sometimes error bars cannot be estimated (if, say, there
#' is only one observation at the given level), but lines are still drawn in these cases, so you may sometimes see levels
#' without error bars even when error bars are turned on. Sometimes (particularly when \code{drop['x']} is \code{FALSE}),
#' you might see floating error bars with no lines drawn to them, or what appear to be completely empty levels. This
#' happens when there is a missing level of \code{x} between two non-missing levels, potentially making an orphaned level
#' (if a non-missing level is surrounded by missing levels). If there are no error bars for this orphaned level, by default
#' nothing will be drawn to indicate it. If you set \code{line.type} to \code{'b'} (or any other type with points), a point
#' will be drawn at such error-bar-less, orphaned levels.
#'
#' \strong{unexpected failures}
#'
#' splot tries to clean up after itself in the case of an error, but you may still run into errors that break things before
#' this can happen. If after a failed plot you find that you're unable to make any new plots, or new plots are drawn over
#' old ones, you might try entering \code{dev.off()} into the console. If new plots look off (splot's
#' \code{\link[graphics]{par}} settings didn't get reset), you may have to close the plot window to reset
#' \code{\link[graphics]{par}} (if you're using RStudio, Plots > "Remove Plot..." or "Clear All..."), or restart R.
#'
#' @examples
#' # simulating data
#' n=2000
#' dat=data.frame(sapply(c('by','bet1','bet2'),function(c)sample(0:1,n,TRUE)))
#' dat$x=with(dat,
#'   rnorm(n)+by*-.4+by*bet1*-.3+by*bet2*.3+bet1*bet2*.9-.8+rnorm(n,0,by)
#' )
#' dat$y=with(dat,
#'   x*.2+by*.3+bet2*-.6+bet1*bet2*.8+x*by*bet1*-.5+x*by*bet1*bet2*-.5
#'   +rnorm(n,5)+rnorm(n,-1,.1*x^2)
#' )
#'
#' # looking at the distribution of y between bets split by by
#' splot(y, by=by, between=c(bet1, bet2), data=dat)
#'
#' # looking at quantile splits of y in y by x
#' splot(y~x*y, dat, split='quantile')
#'
#' # looking at y by x between bets
#' splot(y~x, dat, between=c(bet1, bet2))
#'
#' # sequentially adding levels of split
#' splot(y~x*by, dat)
#' splot(y~x*by*bet1, dat)
#' splot(y~x*by*bet1*bet2, dat)
#'
#' # same as the last but entered by name
#' splot(y, x=x, by=by, between=c(bet1, bet2), data=dat)
#'
#' # zooming in on one of the windows
#' splot(y~x*by, dat, bet1==1&bet2==0)
#'
#' # comparing an adjusted lm prediction line with a loess line
#' # this could also be entered as y ~ poly(x,3)
#' splot(y~x+x^2+x^3, dat, bet1==1&bet2==0&by==1, add={
#'   lines(x[order(x)], loess(y~x)$fitted[order(x)], lty=2)
#'   legend('topright', c('lm', 'loess'), lty=c(1, 2), lwd=c(2, 1), bty='n')
#' })
#'
#' # looking at different versions of x added to y
#' splot(cbind(
#'   Raw=y+x,
#'   Sine=y+sin(x),
#'   Cosine=y+cos(x),
#'   Tangent=y+tan(x)
#' )~x, dat, myl=c(-10,15), lines='loess', laby='y + versions of x')
#'
#' @export
#' @importFrom grDevices grey dev.copy dev.size dev.off cairo_pdf adjustcolor colors
#' @importFrom graphics axis axTicks hist legend lines text mtext plot barplot par points arrows strwidth layout plot.new
#' locator strheight polygon
#' @importFrom stats density median quantile sd lm glm confint update loess smooth.spline formula as.formula predict
#' var binomial

splot=function(y,data=NULL,su=NULL,type='',split='median',levels=list(),sort=NULL,error='standard',error.color='#585858',
  error.lwd=2,lim=9,lines=TRUE,colors='pastel',...,colorby=NULL,colorby.leg=TRUE,color.lock=FALSE,color.offset=1.1,
  color.summary='mean',opacity=1,x=NULL,by=NULL,between=NULL,cov=NULL,line.type='l',mv.scale='none',mv.as.x=FALSE,
  save=FALSE,format=cairo_pdf,dims=dev.size(),file.name='splot',myl=NULL,mxl=NULL,autori=TRUE,xlas=0,ylas=1,xaxis=TRUE,
  yaxis=TRUE,breaks='sturges',density.fill=TRUE,density.opacity=.4,density.args=list(),leg='outside',lpos='auto',lvn=TRUE,
  leg.title=TRUE,leg.args=list(),title=TRUE,labx=TRUE,laby=TRUE,lty=TRUE,lwd=2,sub=TRUE,ndisp=TRUE,note=TRUE,
  font=c(title=2,sud=1,leg=1,leg.title=2,note=3),cex=c(title=1.5,sud=.9,leg=.9,note=.7,points=1),sud=TRUE,labels=TRUE,
  labels.filter='_',labels.trim=20,points=TRUE,points.first=TRUE,byx=TRUE,drop=c(x=TRUE,by=TRUE,bet=TRUE),prat=c(1,1),
  check.height=TRUE, model=FALSE,options=NULL,add=NULL){
  #parsing input and preparing data
  if(check.height && dev.size()[2]<1.7)
    stop('the plot window seems too short; increase the height of the plot window, or set check.height to FALSE',
      call.=FALSE)
  if(!missing(options) && is.list(options) && length(options)!=0){
    a=as.list(match.call())[-1]
    options=tryCatch(options,error=function(e)NULL)
    if(is.null(options)) stop('could not find options')
    return(do.call(splot,c(a[names(a)!='options'],options[!names(options)%in%names(a)])))
  }
  if(!labels) title=sud=sub=labx=laby=note=FALSE
  if(options()$stringsAsFactors){
    on.exit(options(stringsAsFactors=TRUE))
    options(stringsAsFactors=FALSE)
  }
  ck=list(
    ff=list(bet=FALSE,cov=FALSE),
    t=if(grepl('^b|^l',type,TRUE)) 1 else if(grepl('^d',type,TRUE)) 2 else 3,
    b=grepl('^b',type,TRUE),
    tt=!missing(type) && !grepl('^b|^l',type,TRUE),
    d=!missing(data) && !is.null(data),
    su=!missing(su),
    c=!missing(cov),
    co=missing(colors),
    cb=!missing(colorby),
    cblegm=missing(colorby.leg),
    cbleg=is.logical(colorby.leg) && colorby.leg,
    poly=missing(density.fill) || (!is.logical(density.fill) || density.fill),
    polyo=!missing(density.fill) || !missing(density.opacity),
    e=grepl('^s',error,TRUE),
    el=!(is.logical(error) && !error),
    sp=if(!is.character(split)) 4 else if(grepl('^mea|^av',split,TRUE)) 1 else if(grepl('^q',split,TRUE)) 2 else
      ifelse(grepl('^s',split,TRUE),3,4),
    ly=!(is.logical(laby) && !laby) || is.character(laby),
    lys=is.character(laby),
    lx=!(is.logical(labx) && !labx) || is.character(labx),
    line=substitute(lines),
    lty=is.logical(lty),
    ltym=missing(lty),
    ltm=missing(line.type),
    leg=if(is.logical(leg) && !leg) 0 else if(!is.character(leg) || grepl('^o',leg,TRUE)) 1 else 2,
    legm=missing(leg),
    legt=!(is.logical(leg.title) && !leg.title),
    lp=is.character(lpos) && grepl('^a',lpos,TRUE),
    lpm=is.character(lpos) && grepl('^p|^m',lpos,TRUE),
    mod=!missing(x) && model,
    note=!is.character(note),
    mv=FALSE,
    mlvn=missing(lvn),
    opacity=!missing(opacity) && opacity<=1 && opacity>0
  )
  if(ck$lpm) lpos='center'
  if(ck$d && !is.data.frame(data)) data=as.data.frame(data)
  ck$ltck=(is.logical(ck$line) && ck$line) || !grepl('^F',ck$line)
  if(!ck$ltck && ck$note) note=FALSE
  ck$ltco=if(ck$ltck) if(is.logical(ck$line) || ck$c || grepl('^li|^lm|^st',ck$line,TRUE)) 'li' else
    if(grepl('^loe|^po|^cu',ck$line,TRUE)) 'lo' else if(grepl('^sm|^sp|^in',ck$line,TRUE)) 'sm' else
      if(grepl('^e|^co|^d',ck$line,TRUE)) 'e' else if(grepl('^pr|^log',ck$line,TRUE)) 'pr' else 'li' else 'li'
  if(any(!missing(font),!missing(cex),!missing(drop))){
    dop=formals(splot)[c('font','cex','drop')]
    oco=function(s,d){
      od=d=eval(d)
      if(length(s)!=length(d)){
        n=NULL
        if(!is.null(n<-names(s)) || length(s)!=1) if(!is.null(n)) d[n]=s[n] else d[seq_along(s)]=s else d[]=s
        s=d
      }
      s=s[names(od)]
      names(s)=names(od)
      if(any(n<-is.na(s))) s[n]=od[n]
      s
    }
    if(!missing(font)) font=oco(font,dop$font)
    if(!missing(cex)) cex=oco(cex,dop$cex)
    if(!missing(drop)) drop=oco(drop,dop$drop)
  }
  dn=if(ck$d) names(data) else ''
  if(any(grepl('~',c(substitute(y),if(paste(deparse(substitute(y)),collapse='')%in%ls(envir=globalenv())) y),fixed=TRUE))){
    f=as.character(as.formula(y))[-1]
    y = as.formula(y)[[2]]
    bl=function(x){
      cs=strsplit(x,'')[[1]]
      rs=lapply(c('(',')','[',']'),grep,cs,fixed=TRUE)
      l=vapply(rs,length,0)
      cr=TRUE
      if(any(l!=0)){
        if(l[1]!=l[2] || l[3]!=l[4]) stop('invalid parentheses or brackets in ',x)
        cr=!seq_along(cs)%in%c(
          unlist(lapply(seq_len(l[1]),function(r)do.call(seq,lapply(rs[1:2],'[[',r)))),
          unlist(lapply(seq_len(l[3]),function(r)do.call(seq,lapply(rs[3:4],'[[',r))))
        )
      }
      cs[cr]=sub('*','_VAR_',sub('+','_COV_',cs[cr],fixed=TRUE),fixed=TRUE)
      paste(cs,collapse='')
    }
    f=strsplit(bl(f[-1]),' _COV_ ',fixed=TRUE)[[1]]
    if(any(grepl(' _VAR_ ',f,fixed=TRUE))){
      r=strsplit(f[1],' _VAR_ ',fixed=TRUE)[[1]]
      if(length(r)) x=r[1]
      if(length(r)>1) by=r[2]
      if(length(r)>2){
        ck$ff$bet=TRUE
        between=r[3]
      }
      if(length(r)>3) between=c(r[3],r[4])
      f=f[!grepl(' _VAR_ ',f,fixed=TRUE)]
    }else{
      x=f[1]
      f=f[-1]
    }
    if(length(f)){
      cov=f
      ck$c=ck$ff$cov=TRUE
    }
  }
  txt=list(
    split='none',
    y=substitute(y),
    x=substitute(x),
    by=substitute(by),
    bet=as.list(substitute(between)),
    cov=as.list(substitute(cov)),
    su=deparse(substitute(su))
  )
  txt[c('bet','cov')]=lapply(c('bet','cov'),function(l){
    paste(if(!ck$ff[[l]] && length(txt[[l]])>1) txt[[l]][-1] else txt[[l]])
  })
  txt=lapply(txt,function(e)if(is.call(e)) paste(deparse(e), collapse = '\n') else e)
  if(length(txt$bet)>2) txt$bet=txt$bet[1:2]
  tdc=function(x,l=NULL){
    if(!is.call(x)) if((is.null(l) && length(x)!=1) || (!is.null(l) && length(x)==l)) return(x)
    if(is.character(x)) x=parse(text=x)
    tx=tryCatch(eval(x,data,parent.frame(2)),error=function(e)NULL)
    if(is.character(tx) && length(tx)<2){
      x=parse(text=tx)
      tx=tryCatch(eval(x,data,parent.frame(2)),error=function(e)NULL)
    }else if(is.null(tx)) tx=tryCatch(eval(x,data,parent.frame(3)),error=function(e)NULL)
    if(is.null(tx) || class(tx)%in%c('name','call','expression','function')) stop('could not find ',x,call.=FALSE)
    if(!is.null(l) && is.null(ncol(tx))) if(length(tx)!=l){
      tx=rep_len(tx,l)
      if(is.call(x)) x=deparse(x)
      warning(x,' is not the same length as y',call.=FALSE)
    }
    tx
  }
  if(!missing(data) && !class(data)%in%c('matrix','data.frame'))
    data=if(is.character(data)) eval(parse(text=data)) else eval(data,globalenv())
  dat = data.frame(y = tdc(txt$y), check.names = FALSE)
  if(ncol(dat)==1) names(dat)='y'
  nr=nrow(dat)
  lvs=function(x,s=FALSE) if(is.factor(x)) base::levels(x) else if(s) sort(unique(x[!is.na(x)])) else unique(x[!is.na(x)])
  for(n in names(txt)[-c(1, 2, 7)]){
    l=length(txt[[n]])
    if(l==0) next
    if(l==nr){
      dat[,n]=txt[[n]]
      txt[[n]]=n
    }else if(l==1) dat[,n]=tdc(txt[[n]],nr) else for(i in seq_along(txt[[n]])) dat[,paste0(n,'.',i)]=tdc(txt[[n]][[i]],nr)
  }
  if(length(txt$y)==nr) txt$y='y'
  if(missing(x) && !is.null(dat$y) && !is.numeric(dat$y)){
    dat$x = dat$y
    sl = grepl('^(y|by|bet[.12]{,2})$', colnames(dat))
    dat$y = if(sum(sl) == 1) dat[, sl] else do.call(paste, dat[, sl])
    dat$y = table(dat$y)[dat$y]
    if(sum(sl) != 1) dat = dat[, c('y', 'x', colnames(dat)[!colnames(dat) %in% c('y', 'x')])]
    if(ck$t!=2) txt[c('y','x')]=c('count',txt$y)
    ck$el=FALSE
    if(missing(type)){
      ck$b=TRUE
      ck$t=1
      ck[c('b','t','tt')]=list(TRUE,1,FALSE)
    }
    if(missing(autori)) autori=FALSE
  }
  if(NCOL(dat$x)>1){
    ck$c=TRUE
    txt$cov=c(txt$x,txt$cov)
    dat$cov=cbind(dat$cov,dat$x[,-1])
    dat$x=dat$x[,1]
  }
  ck$orn = nr
  su = substitute(su)
  if(ck$su && length(su) != nr){
    tsu = tryCatch(eval(su, if(ck$d) data), error = function(e) NULL)
    if(is.null(tsu) || length(tsu) != nr){
      odat = dat
      colnames(odat) = sub('^y\\.', '', colnames(dat))
      tsu = tryCatch(eval(su, odat), error = function(e) NULL)
    }
    if(!is.null(tsu)){
      tsu[is.na(tsu)] = FALSE
      su = tsu
    }
    if(is.logical(tsu) && sum(tsu) == 0 || length(tsu) == 0){
      ck$su = FALSE
      warning('su excludes all rows, so it was ignored.', .call = FALSE)
    }
  }
  tsu=vapply(dat,is.numeric,TRUE)
  ck$omitted=list(
    na=apply(dat,1,function(r)any(is.na(r))),
    inf=apply(dat[,tsu,drop=FALSE],1,function(r)any(is.infinite(r)))
  )
  if(ck$su) ck$omitted$su = !su
  ck$omitted$all=!Reduce('|',ck$omitted)
  if(any(!ck$omitted$all)){
    if(any(ck$omitted$all)){
      odat=dat[ck$omitted$all,,drop=FALSE]
      dat=odat
      dn=colnames(dat)
      if('x'%in%dn && length(unique(dat$x))==1){
        ck$t=2
        dat$x=NULL
        warning('after omitting, x only had 1 level, so it was dropped')
      }
      if('by'%in%dn && length(unique(dat$by))==1){
        txt$by=dat$by=NULL
        warning('after omitting, by only had 1 level, so it was dropped')
      }
      if(ck$d) data=data[ck$omitted$all,,drop=FALSE]
    }else stop('this combination of variables/splits has no complete cases')
  }
  dn=colnames(dat)
  nr=nrow(dat)
  if(sum(grepl('^y',dn))>1){
    #setting up multiple y variables
    dn=grep('^y\\.',dn)
    ck$mvn=colnames(dat)[dn]
    ck$mvnl = length(ck$mvn)
    if(any(tcn <- grepl('(V\\d+$|c\\(|y\\.(\\d+$|.*\\.))', ck$mvn))){
      ncn = substitute(y)
      if(length(ncn) > 1 && length(ncn <- as.character(ncn[-1])) == length(dn))
        ck$mvn[tcn] = paste0('y.', ncn[tcn])
    }
    ck$mv=TRUE
    if(ck$mlvn) lvn=FALSE
    if(!missing(by)){
      txt$bet=c(txt$by,txt$bet)
      if(length(txt$bet)>2){
        message('multiple y variables moves by to between, so the second level of between was dropped')
        txt$bet=txt$bet[1:2]
        dat=dat[-grep('bet',colnames(dat))[2]]
      }
      if(length(txt$bet)>1){
        dat$bet.1 = if(is.factor(dat$by)) dat$by else as.character(dat$by)
        dat$bet.2 = if(is.factor(dat$bet)) dat$bet else as.character(dat$bet)
        dat$bet=NULL
      }else dat$bet = if(is.factor(dat$by)) dat$by else as.character(dat$by)
    }
    td=dat
    if(any(ckn<-duplicated(ck$mvn))) ck$mvn[ckn]=paste0(ck$mvn[ckn],'_',seq_len(sum(ckn)))
    by=sub('^y\\.','',ck$mvn)
    if(any(by == '')) by[by == ''] = seq_len(sum(by == ''))
    by=factor(rep(by,each=nr),levels=by)
    cncls = vapply(dat[, dn], class, '')
    if(any(cnslsn <- cncls %in% c('numeric', 'integer')) && any(!cnslsn)) for(cnc in which(!cnslsn))
      dat[, cnc] = as.numeric(factor(dat[, cnc], lvs(dat[, cnc])))
    dat=data.frame(y=unlist(dat[,dn],use.names=FALSE))
    if(ncol(td)>length(dn)) dat=cbind(dat,do.call(rbind,lapply(seq_along(dn),function(i)td[,-dn,drop=FALSE])))
    if(mv.as.x){
      txt$by=txt$x
      txt$x=if(missing(labx)) 'variable' else if(labx==txt$by) paste0(labx,'.1') else labx
      dat$by=dat$x
      dat$x=by
    }else{
      txt$by='variable'
      dat$by=by
    }
    if(missing(leg.title) && !mv.as.x) ck$legt=FALSE
    if(!missing(levels) && 'mv'%in%names(levels)) names(levels)[names(levels)=='mv']=txt[[if(mv.as.x)'x'else'by']]
    dn=colnames(dat)
    if(!missing(mv.scale) && mv.scale != 'none'){
      tv = if(mv.as.x) dat$x else dat$by
      for(g in levels(as.factor(tv))){
        svar = tv == g
        cvar = scale(dat[svar, 1], scale = grepl('^t|z|sc', mv.scale, TRUE))
        if(any(is.na(cvar))) cvar = dat[svar, 1] - mean(dat[svar, 1], na.rm = TRUE)
        dat[svar, 1] = cvar
      }
    }
    nr=nrow(dat)
  }else ck$mv = FALSE
  if(!'x'%in%dn){
    ck$t=2
    if(!missing(type) && !grepl('^d',type,TRUE)) message('x must be included to show other types of splots')
  }
  if(!ck$cb && !'by'%in%dn) ck$leg=0
  if(lim>20 || (is.logical(lim) && !lim)){
    lim=Inf
    if(ck$legm && !ck$cb) ck$leg=0
    if(missing(error)) ck$el=FALSE
  }
  if(ck$ltm && !ck$el) line.type='b'
  if(ck$ltym && is.logical(lines) && !lines){ck$lty=FALSE; lty=1}
  if(!is.numeric(dat$y)){
    txt$yax = lvs(dat$y)
    if(!is.factor(dat$y)) dat$y = factor(dat$y, lvs(dat$y))
    dat$y = as.numeric(dat$y)
  }
  if('by'%in%dn && is.character(dat$by) && all(!grepl('[^0-9]',dat$by)))
    dat$by=gsub(' ','0',base::format(dat$by,justify='right'),fixed=TRUE)
  odat=dat
  #splitting and parsing variables
  splt=function(x, s){
    if(s == 1){
      txt$split <<- 'mean'
      factor(x >= mean(x, na.rm = TRUE) * 1, labels = c('Below Average', 'Above Average'))
    }else if(s == 3){
      txt$split <<- 'standard deviation'
      m = mean(x, na.rm = TRUE)
      s = sd(x, TRUE)
      cut(x, c(-Inf, m - s, m + s, Inf), labels = c('-1 SD', 'Mean', '+1 SD'))
    }else if(s == 2){
      txt$split <<- 'quantile'
      cut(x, c(-Inf, quantile(x, na.rm = TRUE)[c(2, 4)], Inf),
        labels = c('2nd Quantile', 'Median', '4th Quantile'))
    }else if(s == 4 && is.double(split) && (length(split) != 1 || all(c(sum(split >= x, na.rm = TRUE),
      sum(split <= x, na.rm = TRUE)) > 1))){
      txt$split <<- paste(split, collapse = ', ')
      cut(x, c(-Inf, split, Inf), paste0('<=', c(split, 'Inf')), ordered_result = TRUE)
    }else if(s == 4 && is.numeric(split) && split > 1){
      n = length(x)
      split = min(n, round(split), na.rm = TRUE)
      txt$split <<- paste0('segments (', split, ')')
      factor(paste('seg', rep(seq_len(split), each = round(n / split + .49))[order(order(x))]))
    }else{
      txt$split <<- 'median'
      factor(x >= median(x, TRUE) * 1, labels = c('Under Median', 'Over Median'))
    }
  }
  seg=list(
    x=list(e=!missing(x),s=FALSE,i=2),
    f1=list(e=FALSE,s=FALSE,l='',ll=1),
    f2=list(e=FALSE,s=FALSE,l='',ll=1),
    by=list(e=FALSE,s=FALSE,l='',ll=1)
  )
  if(seg$x$e && ck$t!=2) if((ck$t==1 || is.character(dat$x) || is.factor(dat$x)
    || (missing(type) && length(unique(dat$x))<lim))){
    dat$x=if(!is.character(dat$x) && !is.factor(dat$x) && length(unique(dat$x))>lim){
      seg$x$s=TRUE
      if(missing(type)) ck$t=1
      splt(dat$x,ck$sp)
    }else{
      if(missing(type)) ck$t=1
      as.factor(dat$x)
    }
  }
  if(ck$t==1 || (is.character(dat$x) || is.factor(dat$x))){
    seg$x$l=lvs(dat$x)
    if(length(seg$x$l)==1) ck$t=3
  }
  svar=NULL
  cvar=if(any(grepl('^c',dn))) which(grepl('^c',dn)) else NULL
  if(any(grepl('^b',dn))){
    svar=which(grepl('^b',dn))
    for(i in svar){
      e=if(grepl('bet',dn[i])) if(!seg$f1$e) 'f1' else 'f2' else 'by'
      seg[[e]]$e=TRUE
      seg[[e]]$i=i
      seg[[e]]$l=lvs(dat[,i])
      seg[[e]]$ll=length(seg[[e]]$l)
      if(seg[[e]]$ll>lim && !(is.character(dat[,i]) || is.factor(dat[,i]))){
        dat[,i]=splt(dat[,i],ck$sp)
        seg[[e]]$s=TRUE
        seg[[e]]$l=lvs(dat[,i])
        seg[[e]]$ll=length(seg[[e]]$l)
      }
      if(!is.factor(dat[, i])) dat[, i] = if(is.character(dat[, i]))
        factor(dat[, i], lvs(dat[, i])) else as.factor(dat[, i])
    }
  }
  if(seg$by$l[1]=='') seg$by$l='NA'
  fmod=NULL
  vs=c(y=txt$y,x=txt$x,by=txt$by,bet=txt$bet,cov=txt$cov)
  colnames(odat)=vs
  if(ck$t!=2 && model) tryCatch({
    mod=formula(paste(vs['y'],'~',vs['x'],
      if(seg$by$e) paste0('*',vs['by']),
      if(seg$f1$e) paste0('*',vs[grep('^bet',names(vs))[1]]),
      if(seg$f2$e) paste0('*',vs['bet2']),
      if(length(cvar)) paste0('+',paste0(vs['cov'],collapse='+'))
    ))
    fmod=lm(mod,odat)
    if(model){
      s=summary(fmod)
      s$call=mod
      print(s)
    }
  },error=function(e)warning(paste('summary model failed:',e$message),call.=FALSE))
  if(!missing(levels)) tryCatch({
    lc=c('y','x','by','f1','f2')
    ns=c(txt$y,txt$x,txt$by,txt$bet,lc)
    lc=c(lc[seq_len(length(ns)-length(lc))],lc)
    for(n in names(levels)){
      if(any(cns<-ns%in%n)){
        sl = lc[cns<-which(cns)[1]]
        if(sl == 'y'){
          sl = list(i = 1)
          vfac = txt$yax
          dat$y = factor(dat$y, labels = vfac)
        }else{
          sl = seg[[sl]]
          vfac = lvs(dat[,sl$i])
        }
        vl=length(vfac)
        ln=levels[[n]]
        lo=NULL
        if(is.list(ln)){
          if(length(ln)>1) lo=levels[[n]][[2]]
          ln=ln[[1]]
        }
        if(is.numeric(ln)) ln=vfac[ln]
        if(vl==length(ln)){
          vl=list(dat[,sl$i])
          if(all(ln%in%vfac)) vl$levels=ln else{
            if(!is.null(lo)){
              vl$labels=ln[lo]
              vl$levels=vfac[lo]
            }else vl$labels=ln
          }
          dat[,sl$i]=do.call(factor,vl)
          if('l'%in%names(sl)) seg[[lc[cns]]]$l=levels(dat[,sl$i])
        }else warning(n,' has ',vl,' levels but you provided ',length(ln),call.=FALSE)
        if(sl$i == 1){
          txt$yax = lvs(dat$y)
          dat$y = as.numeric(dat$y)
        }
      }
    }
  },error=function(e)warning('setting levels failed: ',e$message,call.=FALSE))
  dsf = list(c1 = '', sep = rep.int('^^', nr), c2 = '')
  if(seg$f1$e) dsf$c1 = dat[, seg$f1$i]
  if(seg$f2$e) dsf$c2 = dat[, seg$f2$i]
  cdat=split(dat,dsf)
  if(seg$by$e){
    cdat=lapply(cdat,function(s)if(length(unique(s$by))>1) split(s, factor(as.character(s$by), lvs(s$by))) else{
      s=lapply(seg$by$l,function(l) if(sum(s$by==l)) s else NULL)
      names(s)=seg$by$l
      s
    })
    if(all((seg$n<-vapply(cdat,length,0))==seg$by$ll)){
      seg$n=vapply(cdat,function(s)vapply(s,NROW,0),numeric(seg$by$ll))
    }else drop['by']=FALSE
  }else seg$n=vapply(cdat,nrow,0)
  if(seg$by$e && ck$t != 3 && drop['by']){
    seg$by$l = if(is.null(rownames(seg$n))) structure(seg$n > 1, names = seg$by$l) else
      vapply(rownames(seg$n), function(r) any(seg$n[r,] > 1), TRUE)
    if(!any(seg$by$l)){
      if(ck$t==2) stop('no level of by has more than 1 observation')
      warning('no level of by has more than 1 observation so it was treated as colorby',call.=FALSE)
      seg$by$e=FALSE
      seg$by$l=''
      seg$by$ll=1
      if(ck$cb){
        colorby = substitute(colorby)
        colorby[[2]] = dat$by
      }else colorby=dat$by
      ck$cb=TRUE
      dat=dat[,-seg$by$i]
      cdat=split(dat,dsf)
      seg$n=vapply(cdat,nrow,0)
    }else{
      seg$by$l=names(seg$by$l[seg$by$l])
      seg$by$ll=length(seg$by$l)
    }
  }
  if(!is.null(nrow(seg$n))){
    cdat=cdat[apply(seg$n,2,function(r)any(r>1))]
    if(nrow(seg$n)>1) seg$n=colSums(seg$n[,names(cdat),drop=FALSE])
  }
  if(ck$mv) seg$n=seg$n/length(ck$mvn)
  seg$ll=length(seg$n)
  if(ck$mlvn && seg$by$e && (seg$by$s || !any(grepl('^[0-9]',seg$by$l)))) lvn=FALSE
  ptxt=c(txt[-c(1,7)],l=lapply(seg[1:4],'[[','l'))
  if(missing(labels.trim) && seg$ll==1 && length(ptxt$l.x)<2 && (seg$by$ll==1 || ck$mv)) labels.trim=40
  if(is.numeric(labels.trim) || is.character(labels.filter)){
    vs=c('y','x','by','bet','cov','l.x','l.f1','l.f2','l.by')
    ptxt=lapply(vs,function(n){
      n=as.character(ptxt[[n]])
      if(length(n) != 0 && all(n != 'NULL' & n != '')){
        names(n) = n
        if(is.character(labels.filter)) n=gsub(labels.filter,' ',n,perl=TRUE)
        if(is.numeric(labels.trim)) if(any(ln<-nchar(n)>(labels.trim+3))) n[ln]=sub('$','...',strtrim(n[ln],labels.trim))
      }
      n
    })
    names(ptxt)=vs
  }
  if(is.character(labx)) ptxt$x=labx else if(ck$t == 2) ptxt$x = ptxt$y
  if(is.character(laby)) ptxt$y=laby else if(ck$t == 2) ptxt$y = 'Density'
  ck$ileg=seg$by$e && ck$leg>1
  ptxt$leg = ptxt$l.by
  fdat=dat
  names(fdat)=paste0('.',names(dat))
  fdat=if(!is.null(data)) if(nrow(data)==nr) cbind(data,fdat,odat) else data else cbind(fdat,odat)
  # figuring out colors
  csf = if(is.function(color.summary)) color.summary else if(grepl('^av|mea', color.summary, TRUE))
    splot.colormean else if(grepl('^mode', color.summary, TRUE)) function(x) names(which.max(table(x))) else
      function(x) lvs(x)[round(median(as.numeric(factor(x, lvs(x)))))]
  colors=substitute(colors)
  seg$cols=if(ck$co) colors else if(any(paste(colors)%in%names(fdat))) NULL else tryCatch(tdc(colors),error=function(e)NULL)
  if(is.null(seg$cols)) seg$cols=eval(colors,fdat)
  ptxt$cbo=substitute(colorby)
  if(length(ptxt$cbo)>1 && ptxt$cbo[[1]]=='list') ptxt$cbo=ptxt$cbo[[2]]
  if(!is.character(ptxt$cbo)) ptxt$cbo=deparse(ptxt$cbo)
  if(length(seg$cols)==1){
    if(grepl('^bri|^dar|^pas',seg$cols,TRUE) && (ck$cb || (seg$by$ll>1 && seg$by$ll<10))){
      seg$cols=splot.color(seed=seg$cols)
    }else if(ck$co || grepl('^gra|^grey',seg$cols,TRUE)) seg$cols=splot.color(seg$by$ll,seed='grey')
  }
  cl=length(seg$cols)
  seg$lcols=seg$cols
  ck[c('cbn', 'cbb')] = tg = FALSE
  chl = if(ck$cblegm) FALSE else ck$cbleg
  if(ck$cb){
    sca=names(formals(splot.color))
    colorby=substitute(colorby)
    cba=if(any(paste(colorby)%in%names(fdat))) NULL else tryCatch(tdc(colorby),error=function(e)NULL)
    if(is.null(cba)) cba=eval(substitute(colorby),fdat)
    if(is.null(cba) || (is.character(cba) && length(cba)==1)) cba=tdc(colorby)
    if(!is.list(cba) || is.data.frame(cba)) cba=list(x=cba) else if(is.null(names(cba)))
      names(cba)=names(formals(splot.color))[seq_along(cba)] else
      if(any(names(cba)=='')){tn=names(cba)=='';names(cba)[tn]=sca[seq_len(sum(tn))]}
    if(!is.null(ncol(cba$x)) && ncol(cba$x)>1){if(!'by'%in%names(cba)) cba$by=cba$x[,2];cba$x=cba$x[,1]}
    cba$flat=TRUE
    cn=names(cba)
    ck$cbb='by'%in%cn
    if(ck$mv && length(cba$x) * ck$mvnl == nr){
      cba$x = rep(cba$x, ck$mvnl)
      if(ck$cbb) cba$by = rep(cba$by, ck$mvnl)
    }
    if(ck$cbb){
      cba$by = if(is.numeric(cba$by) && length(unique(cba$by)) > lim){
        ptxt$cbos = if(missing(leg.title)) colorby else leg.title
        ptxt$cbos = if(is.call(ptxt$cbos))
          deparse(ptxt$cbos[[if(cn[2] == 'by' && length(ptxt$cbos) > 2) 3 else 2]]) else deparse(ptxt$cbos)
        splt(cba$by, ck$sp)
      }else factor(cba$by, lvs(cba$by))
      if(seg$by$e && seg$by$ll <= lim && length(cba$by) == nr && !identical(as.character(dat$by), as.character(cba$by))){
        cba$by = dat$by:cba$by
        cbbl = sub(':.*', '', lvs(cba$by))
        colorby[[3]] = as.name(paste0(ptxt$by, ':', colorby[[3]]))
        seg$lcols = seg$cols = splot.color(cbbl, seed = seg$cols)
        if(!ck$b && ck$line){
          if(length(lty) < seg$by$ll) lty = seq_len(seg$by$ll)
          ck[c('lty', 'ltym')] = FALSE
          lty = rep(lty, table(cbbl))
          seg$lty = unique(lty)
        }
      }else{
        lby=length(lvs(cba$by))
        if(!color.lock && cl<lby) seg$cols=splot.color(as.list(rep.int(round(lby/cl+.49),cl)),seed=seg$cols)
      }
    }
    if(length(cba$x)==ck$orn) cba$x=cba$x[ck$omitted$all]
    if(ck$cbb && length(cba$by)==ck$orn) cba$by=cba$by[ck$omitted$all]
    if(seg$by$e || !'seed' %in% cn){
      cba$seed = seg$cols
      if('seed' %in% cn)
        warning("colorby's seed is ignored because by is specified -- use colors to set seeds", call. = FALSE)
    }
    cn=names(cba)
    ckn=cken=is.numeric(cba$x)
    if((ck$t == 1 || any(seg$by$e, seg$f1$e)) && length(cba$x) == nr){
      seg$cbxls = lvs(cba$x)
      if(ck$t != 3 && (!seg$by$e || seg$by$ll > lim)){
        cba$x = vapply(split(cba$x, if(seg$by$e) dat$by else dat$x), function(x) if(ckn) mean(x, na.rm = TRUE) else
          names(which.max(table(x))), if(ckn) 0 else '')
        if(!ckn || length(seg$cbxls) <= lim){
          cba$x = if(ckn){
            cba$x = round(cba$x, 3)
            factor(cba$x, sort(unique(cba$x)))
          }else factor(cba$x, seg$cbxls)
          ckn = FALSE
        }
        if(ck$cbb && length(cba$by)==nr){
          cba$by = factor(vapply(split(cba$by, if(seg$by$e) dat$by else dat$x), function(x)
            names(which.max(table(x))), ''), lvs(cba$by))
          if(length(cba$x)!=length(cba$by)){
            cba$by=NULL
            ck$cbb=FALSE
            warning("colorby's by was dropped as it was not the same length as x after being aligned with the formula's x",
              call.=FALSE)
          }
        }
        if(ckn && !ck$b && ck$t == 1 && length(cba$x) == 2){
          cba$x = c(mean(cba$x), cba$x)
          if(ck$cbb) cba$by = factor(c(lvs(cba$by)[which.max(tabulate(cba$by))], as.character(cba$by)), lvs(cba$by))
        }
      }else if(!ck$cbb){
        if(ck$t == 3){
          cba$by = dat$by
        }else{
          cba$x = data.frame(cba$x, dat$by)
          if(ck$b && seg$ll != 1) cba$x$x = dat$x
          cba$x = unlist(lapply(split(cba$x, dsf), function(x) lapply(
            split(x[, 1], x[, -1]), function(x) if(!length(x)) NA else
              if(ckn) mean(x, na.rm = TRUE) else names(which.max(table(x))))
          ), TRUE, FALSE)
          if(length(cba$x) == seg$by$ll) names(cba$x) = seg$by$l else seg$ill = names(cba$x)
          if(!ckn) cba$x = factor(cba$x, seg$cbxls)
          cba$by = factor(rep_len(seg$by$l, length(cba$x)), seg$by$l)
          if(ck$cblegm) ck$cbleg = FALSE
        }
      }
    }
    if(ck$cbb){
      if(length(cba$by)==nr && length(cba$x)==seg$by$ll){
        tn=lapply(split(cba$by,dat$by),unique)
        if(all(vapply(tn,length,0)==1)) cba$by=unlist(tn,use.names=FALSE) else{
          cba$by=NULL
          warning("colorby's by was dropped as its levels within levels of by are not unique",call.=FALSE)
        }
      }
      if(ck$cbleg){
        chl=TRUE
        if(missing(leg.title)){
          leg.title=substitute(colorby)
          leg.title=if(is.call(leg.title) && length(leg.title)>2)
            deparse(leg.title[[if(cn[2] == 'by') 3 else 2]]) else deparse(leg.title)
        }
        ptxt$leg = lvs(cba$by)
      }
    }else{
      if(ck$cbleg && (ck$t==1 || !seg$by$e)){
        chl=TRUE
        tg=ckn
        ll=all(ck$t!=1 || (length(seg$x$l)>2 || seg$by$ll>2),length(unique(cba$x))>2)
        if(missing(leg.title) && length(ptxt$cbo) == 1) leg.title=ptxt$cbo
        ptxt$leg=if(ckn) formatC(c(min(cba$x),if(ll) mean(cba$x),max(cba$x)),2,format='f') else lvs(cba$x)
      }else if(!seg$by$e) ck$leg=0
    }
    if(!ckn && length(cba$x) > lim && !'shuffle' %in% cn) cba$shuffle = TRUE
    sca=cn%in%sca
    if(any(!sca)) warning(paste0('unused colorby arguments: ',paste(cn[!sca], collapse=', ')), call.=FALSE)
    seg$cols=do.call(splot.color,cba[sca])
    if(!is.null(names(cba$x))) names(seg$cols) = names(cba$x)
    if(!chl || ck$cbb){
      ck$cbn=TRUE
      ptxt$cbn=paste0('Colored by ',if(ckn || cken) 'value of ' else 'levels of ',ptxt$cbo,'. ')
    }
    if(seg$by$e && !ck$cbb){
      if(length(seg$cols) == length(ptxt$leg)){
        seg$lcols = seg$cols
      }else if(ckn && ck$cbb){
        seg$lcols = seg$cols[c(which.min(cba$x), which.max(cba$x))]
      }
    }
    if(chl){
      if(ck$legm && !ck$leg) ck$leg=1+seg$ll>1
      if((ck$ltym || length(lty) == length(seg$cbxls)) && (!seg$by$e || seg$by$ll > length(ptxt$leg))){
        ck[c('lty', 'ltym')] = FALSE
        if(!is.numeric(lty)) lty = 1
        seg$lty = rep_len(lty, seg$by$ll)
        if(!ck$ltym) lty = seq_along(seg$cbxls)
        if(ck$ltym && seg$by$e && !ckn){
          cbl = cba[[if(ck$cbb) 'by' else 'x']]
          for(g in seq_along(seg$cbxls)) seg$lty[cbl == seg$cbxls[[g]]] = lty[[g]]
        }
        lty = unique(seg$lty)
      }
      if(tg){
        l=length(seg$cols)
        seg$lcols=seg$cols[order(cba$x)[c(1,if(ll) round(mean(seq_len(l))),l)]]
      }else if(seg$by$e && length(seg$cols)==seg$by$ll && length(ptxt$leg)==seg$by$ll) seg$lcols=seg$cols
    }else{
      ptxt$leg = if(length(seg$cols) == seg$by$ll && !is.null(names(seg$cols))) names(seg$cols) else seg$by$l
      if(length(ptxt$leg) == length(seg$cols)) seg$lcols = seg$cols else
        if(all(ptxt$leg %in% names(seg$cols))) seg$lcols = seg$cols[ptxt$leg] else
          if(seg$by$e && length(seg$cols) == nr) seg$lcols = vapply(split(seg$cols, dat$by), csf, '')
    }
  }else{
    if(!color.lock && cl<seg$by$ll) seg$cols =
        splot.color(as.list(rep.int(round(seg$by$ll / cl + .49), cl)), seed = seg$cols)
    if(ck$t != 2 && !any(length(seg$cols) == c(nr, seg$by$ll)) && (!ck$b || seg$by$e))
      seg$cols = rep_len(seg$cols, seg$by$ll)
  }
  if(seg$by$e && !all(seg$by$l%in%names(seg$cols))){
    if(length(seg$cols)==seg$by$ll){
      names(seg$cols)=seg$by$l
      if(!ck$cbb && !chl) seg$lcols=seg$cols
    }else if(length(seg$lcols)==seg$by$ll) names(seg$lcols)=seg$by$l else if(length(ptxt$leg)==seg$by$ll){
      if(length(seg$lcols) == nr) seg$lcols = split(seg$lcols, dat$by) else{
        seg$lcols = rep_len(seg$lcols, seg$by$ll)
        if(any(grepl(names(cdat)[1], names(seg$lcols), fixed = TRUE))){
          for(g in names(cdat)) names(seg$lcols) = sub(paste0(g, '.'), '', names(seg$lcols), fixed = TRUE)
          if(all(seg$by$l %in% names(seg$lcols))) seg$lcols = seg$lcols[seg$by$l]
        }else names(seg$lcols) = seg$by$l
      }
    }
    if(ck$b && length(seg$cols) == nr) seg$cols = unlist(lapply(split(data.frame(seg$cols, dat$by), dat$x),
      function(d) vapply(split(d[, 1], d[, 2], drop = TRUE), csf, '')), use.names = FALSE)
  }
  if(ck$opacity && (ck$t!=3 || !points)) if(is.list(seg$cols)) lapply(seg$cols,adjustcolor,opacity) else
    seg$cols[]=adjustcolor(seg$cols,opacity)
  if(lvn && length(ptxt$by)) ptxt$l.by[] = paste0(paste0(ptxt$by, ': '), ptxt$l.by)
  if(length(seg$cols) == nr){
    if(any(seg$by$e && !ck$b, seg$f1$e, seg$f2$e)){
      seg$scols=split(if(seg$by$e && !ck$b) data.frame(seg$cols,dat$by) else seg$cols,dsf)
      if(!ck$b){
        if(seg$by$e) seg$scols = lapply(seg$scols, function(d) split(d[, 1], d[, 2]))
        if(ck$t == 1) seg$scols = lapply(seg$scols, function(bl) vapply(bl, function(bll)
          if(length(bll)) csf(bll) else '', ''))
      }
    }
  }else if(seg$ll != 1 && 'ill' %in% names(seg)){
    seg$scols = lapply(names(cdat), function(n) seg$cols[grepl(n, names(seg$cols), fixed = TRUE)])
    names(seg$scols) = names(cdat)
  }
  if(ck$t==2 && seg$by$ll>1 && !all(seg$by$l%in%names(seg$cols))){
    seg$cols=if(length(seg$lcols)==seg$by$ll) seg$lcols else if(length(seg$cols)==1)
      splot.color(seq_len(seg$by$ll),seed=seg$cols) else rep_len(seg$cols,seg$by$ll)
    if(is.null(names(seg$cols))) names(seg$cols)=seg$by$l
  }
  #figuring out parts of the plot
  ylab=if(ck$ly) ptxt$y else ''
  xlab=if(ck$lx && length(ptxt$x)) ptxt$x else ''
  main=if(is.logical(title) && title) paste0(if(ck$t==2)paste('Density of',ptxt$x) else paste(ptxt$y,
    'by',ptxt$x),if(seg$by$e && !ck$mv) paste(' at levels of',ptxt$by), if(length(ptxt$bet)!=0) paste(' between',
      paste(ptxt$bet,collapse=' & '))) else if (is.character(title)) title else ''
  if(!is.character(note)) if(!is.logical(note) || note){
    ck$er=ck$t==1 && ck$el
    ck$spm=txt$split!='none'
    if(ck$er && all(vapply(cdat,function(d){
      if(!is.data.frame(d)) all(vapply(d,function(dd) !anyDuplicated(dd$x),TRUE)) else !anyDuplicated(d$x)
    },TRUE))) ck[c('el','er')]=FALSE
    if(any(ck$cbn, ck$spm, ck$er, ck$t == 3 && ck$ltck)){
      if(ck$spm){
        tv = unique(c(
          if(seg$x$s) ptxt$x,
          if(seg$by$s) ptxt$by,
          if(seg$f1$s) ptxt$bet[1],
          if(seg$f2$s) ptxt$bet[2],
          if('cbos' %in% names(ptxt)) ptxt$cbos
        ))
        tv = sub(', (?=[A-z0-9]+$)', if(length(tv) > 2) ', & ' else ' & ', paste(tv, collapse = ', '), perl = TRUE)
      }
      note=paste0(
        if(ck$spm) paste0(tv,' split by ',txt$split,'. '),
        if(ck$er) paste('Error bars show',ifelse(ck$e,'standard error. ','95% confidence intervals. ')),
        if(ck$cbn) ptxt$cbn,
        if(ck$t == 3 && ck$ltck) paste0('Line type: ', switch(ck$ltco,
          li = 'lm', lo = 'loess', sm = 'spline', e = 'connected', pr = 'probability'), '.')
      )
    }
  }else note=''
  ck$sud=(!is.logical(sud) || sud) && (is.character(sud) || ck$su || ck$c)
  ck$sub=(!is.logical(sub) || sub) && (is.character(sub) || seg$ll>1 || ndisp)
  pdo=list(...)
  l2m=function(l){tl=round(l^.5); c(tl+all(l>c(tl^2,tl*(tl-1))),tl)}
  seg$dim=if(any(ckl<-c('mfrow','mfcol')%in%names(pdo))) pdo[[if(ckl[1]) 'mfrow' else 'mfcol']] else
    if(!seg$f1$e) c(1,1) else if(!seg$f2$e){
      if(seg$f1$ll>2) l2m(seg$f1$ll) else c(2,1)
    }else c(seg$f1$ll,seg$f2$ll)
  seg$l=t(data.frame(strsplit(names(cdat),'.^^.',fixed=TRUE)))
  if(seg$f1$e){
    rownames(seg$l)=match(seg$l[,1],seg$f1$l)
    seg[c('f1','f2')]=lapply(c('f1','f2'),function(n){
      nl=seg[[n]]
      if(nl$e) nl$l=unique(seg$l[,if(n=='f1') 1 else 2])
      if(nl$e) nl$ll=length(nl$l)
      nl
    })
  }
  nc=seg$dim[1]*seg$dim[2]
  if(length(ptxt$leg) == 1 && ptxt$leg == 'NA') ck$leg=0
  if(ck$leg==1 && ck$legm && (dev.size(units='in')[1]<2
    || (all(seg$dim==1) && (ck$t!=1 || seg$by$ll<9)))) ck$leg=2
  if(ck$leg==1) if(is.logical(leg) || is.character(leg)) leg=nc+1
  dop=par(no.readonly=TRUE)
  if(drop['bet'] && !any(ckl) && any(nc-seg$ll>=seg$dim)){
    seg$dim=l2m(seg$ll)
    nc=seg$dim[1]*seg$dim[2]
  }
  seg$dmat=matrix(seq_len(nc),seg$dim[2],seg$dim[1])
  if(!drop['bet'] && seg$f2$e){
    seg$lc=vapply(seg$f1$l,function(l)seg$f2$l%in%seg$l[seg$l[,1]==l,2],logical(seg$f2$ll))
  }else{
    seg$lc=seg$dmat==0
    seg$lc[seq_len(seg$ll)]=TRUE
  }
  if(nc>seg$ll){
    if(any(ckl)){
      tm=lapply(dim(seg$lc),seq_len)
      mm=matrix(FALSE,seg$dim[2],seg$dim[1])
      mm[tm[[1]],tm[[2]]]=seg$lc
      seg$lc=mm
    }
    if(!drop['bet']){
      seg$dmat[seg$lc]=seq_len(seg$ll)
      seg$dmat[!seg$lc]=seq_len(sum(!seg$lc))+seg$ll
    }
  }
  ck$legcol=FALSE
  if(lpos=='auto') 'topright'
  lega=list(x=lpos,col=seg$lcols,cex=cex['leg'],text.font=font['leg'],bty='n',x.intersp=.5,xjust=.5,legend=ptxt$leg)
  if(ck$legt && (is.character(leg.title) && length(leg.title) == 1 || length(ptxt$by) == 1))
    lega$title=if(is.character(leg.title)) leg.title else ptxt$by
  l=length(lega$legend)
  seg$lwd=rep_len(if(is.numeric(lwd)) lwd else 2,seg$by$ll)
  if(!'lty' %in% names(seg)) seg$lty=rep_len(if(!ck$ltym && !ck$lty) lty else
    if(ck$cbleg && ck$cbb && seg$by$ll==length(cba$by)) as.numeric(cba$by) else
      if(ck$lty && lty) seq_len(6) else 1,seg$by$ll)
  if(length(seg$cols)==length(seg$lcols)) names(seg$lcols)=names(seg$cols)
  if(seg$by$e || ck$cbb) names(seg$lwd)=names(seg$lty)=if(length(seg$lcols)==seg$by$ll) names(seg$lcols) else
    if(all(c(length(seg$lwd), length(seg$lty)) == length(seg$cols))) names(seg$cols) else seg$by$l
  lega$lwd=if(seg$by$ll==l) seg$lwd else rep_len(if(is.numeric(lwd)) lwd else 2,l)
  lega$lty=if(seg$by$ll==l) seg$lty else rep_len(if(!ck$ltym && !ck$lty) lty else if(ck$lty && lty) seq_len(6) else 1,l)
  if(!missing(leg.args)) lega[names(leg.args)]=leg.args
  if(any(tck<-!names(lega)%in%names(formals(legend)))){
    warning('dropped items from leg.args: ',paste(names(lega)[tck],collapse=', '),call.=FALSE)
    lega=lega[!tck]
  }
  if((ck$legm || !ck$leg) && missing(leg.args) && (sum(strheight(lega$legend,'i'))*cex['leg']*1.5/if('ncol'%in%names(pdo))
    pdo$ncol else 1)>dev.size()[2]){
      ck$leg=0
      if(ck$ltym) seg$lty[]=1
    }
  if(ck$leg==1){
    if(ck$legm && nc>seg$ll) leg=which(!seg$lc)[1]
    if(nc>seg$ll && leg<=nc){
      if(seg$lc[leg] && !drop['bet']){
        mm=which(!seg$lc)
        leg=mm[which.min(abs(mm-leg))]
      }
      if(seg$lc[leg]){
        seg$lc[]=TRUE
        seg$lc[leg]=FALSE
        seg$lc[seg$lc][bsq<-seq_len(nc-seg$ll-1)+seg$ll]=FALSE
        seg$dmat[seg$lc]=seq_len(seg$ll)
        seg$dmat[leg]=seg$ll+1
        seg$lc[leg]=TRUE
        seg$dmat[!seg$lc]=bsq+1
      }else{
        seg$lc[leg]=TRUE
        seg$dmat[leg]=seg$ll+1
        seg$dmat[!seg$lc]=seq_len(sum(!seg$lc))+seg$ll+1
      }
      if(ck$lp) lega$x='center'
    }else if(ck$lp) lega$x='right'
    if(nc==seg$ll || leg>nc){
      seg$dmat[seg$dmat==seg$ll+1]=nc+1
      seg$dmat=rbind(seg$dmat,rep.int(seg$ll+1,seg$dim[1]))
      ck$legcol=TRUE
    }
  }
  seg[c('dmat','lc')]=lapply(seg[c('dmat','lc')],t)
  seg$prat = if(missing(prat) && ck$legcol){
    lw = max(.4, if(ck$legt) strwidth(lega$title, 'i'), strwidth(ptxt$leg, 'i') / if(seg$ll > 1) 1.3 else 1.7) +
      if(all(seg$dim == 1)) .5 else .2
    fw = (dev.size(units = 'in')[1] - lw) / seg$dim[2]
    c(fw, max(fw / 10, lw))
  }else prat
  op=list(
    oma=c(
      sum(is.character(note) && note != '', ck$lx) + .15, ck$ly * .9,
      max(sum((main != '') * 1.8 + if(sum(seg$dim) > 2) .5 else 0, ck$sud), 1), .5
    ),
    mar=c(
      if(ck$lx) 2 else 1.5, if(ck$ly) 2.8 else 2.4, (ck$sud && (ck$su || ck$c)) *
      ifelse(seg$ll > 1, 2, 0) + (ck$sub && sum(seg$dim) > 2) * 1.3, 0
    ),
    mgp=c(3,.3,0),
    font.main=1,
    font.lab=2,
    cex.main=1,
    cex.lab=1,
    cex.axis=1,
    tcl=-.2,
    pch=19,
    col='#303030',
    xpd=NA
  )
  if(length(pdo)!=0){
    if(any(cpdo<-(npdo<-names(pdo))%in%names(dop))){
      op[npdo[cpdo]]=pdo[cpdo]
      if('font.sub'%in%names(op)) op$font.main=op$font.sub
      if('cex.sub'%in%names(op)) op$cex.main=op$cex.sub
      if('col.sub'%in%names(op)) op$col.main=op$col.sub
    }
    pdo=pdo[!cpdo]
  }
  if(!'horiz'%in%names(pdo) && !'ncol'%in%names(leg.args)) lega$ncol=1
  if(length(pdo)!=0){
    if(any(cpdo<-(npdo%in%names(formals(legend)) & !npdo%in%names(leg.args)))) lega[npdo[cpdo]]=pdo[cpdo]
    if(any(!cpdo)) warning('unused argument', if(sum(!cpdo) == 1) ': ' else 's: ' ,
      paste(names(pdo)[!cpdo], collapse=', '), call.=FALSE)
  }
  par(op)
  on.exit(par(dop))
  layout(seg$dmat,c(rep.int(seg$prat[1],seg$dim[2]),if(ck$legcol) seg$prat[if(length(seg$prat)>1) 2 else 1]))
  success=FALSE
  ck$scol='scols'%in%names(seg)
  for(i in names(cdat)){tryCatch({
    #plotting
    cl=(if(class(cdat[[i]])=='list') vapply(cdat[[i]],NROW,0) else nrow(cdat[[i]])) > 0
    if(any(!cl)){
      cdat[[i]]=cdat[[i]][cl]
      if(length(cdat[[i]])==0) next
    }
    if(ck$scol) seg$cols = seg$lcols = seg$scols[[i]]
    cl=strsplit(i,'.^^.',fixed=TRUE)[[1]]
    ptxt$sub=if(is.character(sub)) sub else if(ck$sub) if(seg$ll>1 || (!missing(ndisp) && ndisp)) paste0(
      if(seg$f1$e) paste0(
        if(lvn || (ck$mlvn && grepl('^[0-9]',cl[1]))) paste0(ptxt$bet[1],': '),cl[1],
        if(seg$f2$e) paste0(', ',if(lvn || (ck$mlvn && grepl('^[0-9]',cl[2]))) paste0(ptxt$bet[2],': '),cl[2])
      ),if((length(names(cdat))>1 || !missing(ndisp)) && ndisp) paste(', n =',seg$n[i])
    ) else ''
    if(!is.null(sort) && ck$t!=2 && class(if(seg$by$e) cdat[[i]][[1]][,'x'] else cdat[[i]][,'x']) %in%
        c('factor','character')){
      nsl = grepl('^[Ff]', as.character(sort))
      sdir = grepl('^[DdTt]',as.character(sort))
      td=if(seg$by$e) do.call(rbind,cdat[[i]]) else cdat[[i]]
      td[, 'x'] = as.character(td[, 'x'])
      cdat[[i]] = do.call(rbind, lapply(if(nsl) lvs(td[, 'x']) else
        names(sort(vapply(split(td[, 'y'], td[, 'x']), mean, 0, na.rm = TRUE), sdir)),
        function(l) td[td[, 'x'] == l,, drop = FALSE]
      ))
      seg$x$l=ptxt$l.x=lvs(cdat[[i]][,'x'])
      cdat[[i]][,'x']=factor(cdat[[i]][,'x'],seg$x$l)
      if(seg$by$e) cdat[[i]]=split(cdat[[i]],cdat[[i]][,'by'])
    }
    if(ck$t==1){
      #bar and line
      flipped=FALSE
      if(missing(byx) && ck$mv && any(vapply(cdat[[i]], function(d) any(vapply(split(d$y, as.character(d$x)),
        function(dl) if(length(dl) == 1) 0 else var(dl), 0) == 0), TRUE))) byx = FALSE
      if(byx && lim<Inf && seg$by$e && (is.list(cdat[[i]]) && length(cdat[[i]])>1)){
        flipped=TRUE
        cdat[[i]]=do.call(rbind,cdat[[i]])
        cdat[[i]][c('x','by')]=cdat[[i]][c('by','x')]
        if(is.numeric(cdat[[i]]$x)) cdat[[i]]$x = as.character(cdat[[i]]$x)
        cdat[[i]]=split(cdat[[i]],cdat[[i]]$by)[lvs(cdat[[i]]$by)]
      }
      dl=if(cl<-class(cdat[[1]])=='list') length(cdat[[i]]) else 1
      mot=paste0('y~0+',paste(names(if(cl) cdat[[i]][[1]] else cdat[[i]])[c(2,cvar)],collapse='+'))
      m=pe=ne=matrix(NA,seg$by$ll,max(c(1,length(seg$x$l))),dimnames=list(seg$by$l,seg$x$l))
      if(flipped) m=pe=ne=t(m)
      rn=if(nrow(m)==1) 1 else rownames(m)
      cn=if(seg$by$e && flipped) seg$by$l else colnames(m)
      for(l in seq_len(dl)){
        ri=rn[l]
        td=if(cl) cdat[[i]][[ri]] else cdat[[i]]
        if(is.null(td)) next
        if(nrow(td)>1 && length(unique(td$x))>1){
          mo=lm(mot,data=td)
          ccn = sub('^x', '', names(mo$coef))
          sus = which(ccn %in% cn)
          su = ccn[sus]
          m[ri, su] = mo$coef[sus]
          if(nrow(td) > 2 && anyDuplicated(td$x)){
            if(ck$e){
              e=suppressWarnings(summary(update(mo,~.-0))$coef[sus,2])
              e=e[c(2,seq_along(e)[-1])]
              pe[ri,su]=m[l,su]+e
              ne[ri,su]=m[l,su]-e
            }else{
              e=confint(mo)[sus,]
              pe[ri,su]=e[,2]
              ne[ri,su]=e[,1]
            }
          }
        }else{
          if(nrow(td)==0) next
          mo=lapply(split(td,td['x']),function(s)if(nrow(s)==0) NA else mean(s[is.finite(s[,'y']),'y']))
          m[ri,]=unlist(mo[colnames(m)])
        }
      }
      re=if(flipped) list(m=t(m),ne=t(ne),pe=t(pe)) else list(m=m,ne=ne,pe=pe)
      if(ck$ltm && all(apply(is.na(re$m),2,any))){
        drop['x']=FALSE
        line.type='b'
      }
      dx=!apply(is.na(re$m),2,all)
      if(drop['x']) re=lapply(re,function(s)s[,dx,drop=FALSE])
      m=re$m
      ne=re$ne
      pe=re$pe
      if(all(mna<-is.na(m))) next
      re=lapply(re,function(s){
        na=is.na(s)
        s[na]=m[na]
        s[!mna]
      })
      if(ck$el) ck$el = all(round(re$m - re$ne, 8) != 0)
      lb = min(re$m) - if(!ck$el) round((max(re$m) - min(re$m)) / 10) else max(abs(re$m - re$ne)) * 1.2
      if(ck$b && !ck$el) lb = lb - (max(re$m) - min(re$m)) * .1
      dm = dim(m)
      ylim = if(missing(myl)) c(lb, max(re$m) + if(ck$el) max(abs(re$m - re$pe)) else 0) else myl
      if(ck$leg==2 && ck$lp){
        if(!seg$by$e && ncol(m)==2) lega$x='top' else{
          lega$x=apply(m,2,function(r){na=!is.na(r);if(any(na)) max(r[na]) else -Inf})
          stw=ncol(m)
          oyl=if(stw%%2) 3 else 2
          lega$x=c('topleft','top','topright')[if(oyl==2) -2 else 1:3][which.min(vapply(split(lega$x,rep(seq_len(oyl),
            each=stw/oyl)[seq_len(stw)]),mean,0,na.rm=TRUE))]
          if(is.na(lega$x)) lega$x='topright'
        }
      }
      if(any(is.na(ylim))) next
      oyl = axTicks(2, c(ylim[1], ylim[2], par('yaxp')[3]))
      rn = if(nrow(m) == 1) colnames(m) else rownames(m)
      colnames(m)=if(drop['x'] && sum(dx)==ncol(m)) ptxt$l.x[dx] else ptxt$l.x
      stw=strwidth(colnames(m),'i')
      if((missing(xlas) || xlas>1) && sum(stw)>par('fin')[1]-sum(par('omi')[c(2,4)])-dm[2]*.1 && par('fin')[1]>2.5){
        xlas=3
        if(missing(mxl)) mxl=c(1,dm[2])
        mh=c(par('fin')[2]/2,max(stw))
        par(mai=c(min(mh)+.1,par('mai')[-1]))
        if(mh[1]<mh[2] && missing(labels.trim)){
          mh=round(mh[1]/.1)
          n=colnames(m)
          ln=nchar(n)>mh
          colnames(m)[ln]=sub('$','...',strtrim(n[ln],mh))
        }
      }
      if(min(re$ne,na.rm=TRUE)>=0) autori=FALSE
      rck=!is.list(seg$cols) && all(rn%in%names(seg$cols))
      if(rck && length(seg$cols)<dm[1]) seg$cols=rep_len(seg$cols,dm[1])
      if(!rck && ck$ltm && !ck$el) line.type='b'
      if(ck$b){
        if(autori){
          a = if(missing(myl)) lb else myl[1]
          a = a * -1
          m = m + a
          ne = ne + a
          pe = pe + a
          ayl = oyl + a
          aj = lapply(re, '+', a)
          ylim = if(missing(myl)) if(!ck$el) ylim + a else c(
            min(aj$m) - max(abs(aj$m - aj$ne)) * 1.2,
            max(aj$m) + max(abs(aj$m - aj$pe)) * if(ck$leg == 2 && seg$by$ll > 1) seg$by$ll ^ .3 + .7 else 1.2
          ) else myl + a
        }
        if(dm[1] != 1) rownames(m) = ptxt$l.by[rn]
        lega[c('lwd','lty')]=NULL
        lega[c('pch','pt.cex','x.intersp','y.intersp','adj')]=list(15,2,1,1.2,c(0,.35))
        p=barplot(m,beside=TRUE,col=if(rck) seg$cols[rn] else seg$cols,axes=FALSE,axisnames=FALSE,
          border=NA,ylab=NA,xlab=NA,ylim=ylim,main=if(ck$sub) ptxt$sub else NA,
          xpd=if('xpd'%in%names(pdo)) pdo$xpd else if(autori) NA else FALSE)
      }else{
        p=matrix(rep.int(seq_len(dm[2]),dm[1]),nrow=dm[1],byrow=TRUE)
        plot(NA,ylim=ylim,xlim=if(missing(mxl)) c(1-stw[1]/3,dm[2]+stw[length(stw)]/3) else mxl,ylab=NA,xlab=NA,
          main=if(ck$sub) ptxt$sub else NA,axes=FALSE)
        for(a in if(dm[1] == 1) 1 else if(all(rn%in%names(seg$cols))) rn else seq_len(dm[1]))
          graphics::lines(m[a,],col=seg$cols[[a]],lty=seg$lty[[a]],lwd=seg$lwd[[a]],type=line.type)
      }
      if(ck$ileg) lega$legend=rn
      if(xaxis) axis(1,colMeans(p),colnames(m),FALSE,las=xlas,cex=par('cex.axis'),fg=par('col.axis'))
      a2a=list(2,las=ylas,cex=par('cex.axis'),fg=par('col.axis'))
      if(ck$b && autori){
        a2a$at=ayl
        a2a$labels=formatC(oyl,2,format='f')
      }
      if(yaxis) do.call(axis,a2a)
      if(ck$el){
        te=round(Reduce('-',list(ne,pe)),8)
        te[is.na(te)]=0
        te=te==0
        if(any(te)) ne[te]=pe[te]=NA
        arrows(p,ne,p,pe,lwd=error.lwd,col=error.color,angle=90,code=3,length=.05)
      }
    }else if(ck$t==2){
      #density
      if(!is.list(density.args)) density.args = list()
      fdan = names(formals(stats::density.default))
      dan = names(density.args)
      if(any(mdan <- !dan %in% fdan)){
        warning(paste('unused density argument(s):', paste(dan[mdan], collapse = ', ')), call. = FALSE)
        density.args = density.args[!mdan]
      }
      density.args$give.Rkern = FALSE
      if(!missing(mxl)){
        if(!'from' %in% dan) density.args$from = mxl[1]
        if(!'to' %in% dan) density.args$to = mxl[2]
      }
      if(!'n' %in% dan) density.args$n = 512
      n = density.args$n
      m=list()
      dl=if(cl<-class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      rnl=logical(dl)
      rn=if(is.data.frame(cdat[[i]])) names(ptxt$l.by) else names(cdat[[i]])
      dx = dy = numeric(n * seg$by$ll)
      for(l in seq_len(dl)) tryCatch({
        density.args$x = (if(cl) cdat[[i]][[l]] else cdat[[i]])[,'y']
        m[[l]] = do.call(stats::density, density.args)
        dx[seq_len(n) + n * (l - 1)] = m[[l]]$x
        dy[seq_len(n) + n * (l - 1)] = m[[l]]$y
        rnl[l]=TRUE
      },error=function(e)NULL)
      names(m)=rn=rn[rnl]
      if(seg$by$ll>1 || (ck$polyo && ck$poly)){
        plot(NA,xlim=if(missing(mxl)) range(c(dx, dx)) else mxl,ylim=if(missing(myl)) c(0,max(dy)) else myl,
          main=if(ck$sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,xpd=if('xpd'%in%names(pdo)) pdo$xpd else FALSE)
        for(l in if(seg$by$ll > 1 && all(rn%in%names(seg$cols))) rn else seq_along(m)){
          if(ck$poly) polygon(m[[l]], col = adjustcolor(seg$cols[[l]], density.opacity), border = NA)
          if(!is.logical(lines) || lines) graphics::lines(m[[l]],col=seg$cols[[l]],lwd=seg$lwd[[l]],lty=seg$lty[[l]])
        }
        if(ck$ileg) lega$legend=rn
      }else{
        col=if(length(seg$lcols)>2) '#555555' else seg$lcols[1]
        if(ck$leg){
          lega[c('lwd','lty')]=NULL
          lega[c('pch','pt.cex','x.intersp','y.intersp','adj')]=list(15,2,1,1.2,c(0,.35))
        }
        y=(if(cl) cdat[[i]][[1]] else cdat[[i]])[,'y']
        hp = hist(y, breaks, plot = FALSE)
        if(ck$cb && length(seg$cols)==nr){
          nb = length(hp$counts)
          seg$cols = vapply(split(seg$cols[order(y)], sort(rep_len(seq_len(nb), nr))), csf, '')
          if(!ckn) seg$cols=adjustcolor(seg$cols,1,color.offset,color.offset,color.offset)
        }else if(!color.lock && (ck$co || length(seg$cols)==1))
          seg$cols[2]=adjustcolor(seg$cols[1],1,color.offset,color.offset,color.offset)
        hist(
          y,breaks,FALSE,border=if('border'%in%names(pdo)) pdo$border else par('bg'),main=if(ck$sub) ptxt$sub else NA,
          ylab=NA,xlab=NA,axes=FALSE,col=if(length(seg$cols)==2) seg$cols[2] else seg$cols,
          xlim = if(missing(mxl)) range(hp$breaks) else mxl,
          ylim = if(missing(myl)) c(0, max(c(dy, hp$density))) else myl
        )
        if(!is.logical(lines) || lines) graphics::lines(m[[1]],col=col,lwd=lwd,xpd=if('xpd'%in%names(pdo))
          pdo$xpd else FALSE)
      }
      if(ck$lp && ck$leg==2) lega$x=if(mean(dx)>mean(range(dx))) 'topleft' else 'topright'
      if(xaxis) axis(1,las=xlas,cex=par('cex.axis'),fg=par('col.axis'))
      if(yaxis) axis(2,las=ylas,cex=par('cex.axis'),fg=par('col.axis'))
    }else{
      #scatter
      dl=if(cl<-class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      rn=if(is.data.frame(cdat[[i]])) seg$by$l else names(cdat[[i]])
      td=if(cl) do.call(rbind,cdat[[i]]) else cdat[[i]]
      cx=td[,'x']
      cy=td[,'y']
      xch=if(is.numeric(cx) || is.logical(cx)) cx else as.numeric(factor(cx))
      a2a=list(cex=par('cex.axis'),fg=par('col.axis'))
      if(length(ptxt$l.x)!=0){
        a2a$tick=FALSE
        a2a$at=seq_along(ptxt$l.x)
        a2a$labels=ptxt$l.x
        if(missing(xlas) || xlas>1){
          xlas=3
          par(mai=c(min(c(par('fin')[2]/2,max(strwidth(ptxt$l.x,'i'))))+.1,par('mai')[-1]))
        }
      }
      plot(
        NA, xlim = if(missing(mxl)) range(xch,na.rm=TRUE) else mxl,
        ylim = if(missing(myl)) c(min(cy, na.rm = TRUE), max(cy, na.rm = TRUE) + max(cy, na.rm = TRUE) *
            if(ck$leg == 1 && seg$by$ll < lim) seg$by$ll / 20 else 0) else myl,
        main = if(ck$sub) ptxt$sub else NA, ylab = NA, xlab = NA, axes = FALSE
      )
      if(yaxis) do.call(axis,c(list(2,las=ylas),c(a2a[c('cex','fg')],
        if('yax'%in%names(txt))list(at=seq_along(txt$yax),labels=txt$yax,tick=FALSE))))
      if(xaxis) do.call(axis,c(list(1,las=xlas),a2a))
      if(ck$leg>1){
        up = xch[cy >= quantile(cy, na.rm = TRUE)[4]]
        mr = quantile(xch, na.rm = TRUE)
        if(ck$lp) lega$x=if(sum(up<mr[2])>sum(up>mr[4])) 'topright' else 'topleft'
        if(ck$ileg) lega$legend=rn
      }
      padj=if(color.lock || ck$cb || (missing(color.offset) && !ck$ltck)) 1 else color.offset
      ckcn=all(rn%in%names(seg$cols))
      ckln=all(rn%in%names(seg$lcols))
      if(!ckln) if(ckcn) seg$lcols = seg$cols else seg$lcols[] = if(opacity != 1)
        adjustcolor('#555555', opacity) else '#555555'
      lwd=rep_len(if(is.numeric(lwd)) lwd else 2,dl)
      for(l in if(ckcn) rn else seq_len(dl)){
        td=if(cl) cdat[[i]][[l]] else cdat[[i]]
        if(is.null(td)) next
        x=td[,'x']
        y=td[,'y']
        col=if(ckcn) seg$cols[[l]] else seg$cols
        if(opacity!=1 || padj!=1) col=adjustcolor(col,opacity,padj,padj,padj)
        if(points && points.first) points(x,y,col=col,cex=cex['points'])
        if(ck$ltck){
          lt=if(ck$ltco=='pr' && length(unique(y))!=2) 'li' else ck$ltco
          fit = if(lt == 'e') y else tryCatch({
            if(ck$c) lm(y~x+as.matrix(td[,cvar,drop=FALSE]))$fitted else
              if(lt=='pr'){
                yr=range(y)
                y=factor(y,labels=c(0,1))
                fit=predict(glm(y~x,binomial))
                fit=exp(fit)/(1+exp(fit))
                if(!all(yr==c(0,1))) fit=(fit-mean(fit))*(yr[2]-yr[1])+mean(yr)
                if(max(fit)>yr[2]) fit-(max(fit)-yr[2]) else fit
              }else predict(switch(lt,li=lm,lo=loess,sm=smooth.spline)(y~x))
          },error=function(e){warning('error estimating line: ',e$message,call.=FALSE);NULL})
          if(!is.null(fit)){
            if(lt == 'e') xo = x else if(lt == 'sm'){
              xo = fit$x
              fit = fit$y
            }else{
              or = order(x)
              xo = x[or]
              fit = fit[or]
            }
            graphics::lines(xo, fit, col = seg$lcols[[l]], lty = seg$lty[[l]], lwd = seg$lwd[[l]])
          }
        }
        if(points && !points.first) points(x,y,col=col,cex=cex['points'])
      }
    }
    if(ck$leg==2){
      if(ck$lpm){
        message('click to place the legend')
        lega[c('x','y')]=locator(1)
        if(is.null(lega$x)){
          warning('placing the legend with locator(1) failed')
          lega$y=NULL
          lega$x=if(seg$ll>1) 'topright' else 'right'
        }
      }
      tf=par('font')
      par(font=font['leg.title'])
      do.call(legend,lega)
      par(font=tf)
    }
    success=TRUE
    if(!missing(add)) tryCatch(eval(substitute(add),fdat)
      ,error=function(e)warning('error from add: ',e$message,call.=FALSE))
  },error=function(e){dev.off();stop(e)})}
  if(!success) stop("failed to make any plots with the current input",call.=FALSE)
  if(ck$leg==1){
    if(all(par('mfg')[1:2]!=0)){
      plot.new()
      if(ck$b) lega[c('pch','pt.cex','x.intersp','y.intersp','adj')]=list(15,2,1,1.2,c(0,.35))
      if(ck$lpm){
        message('click to place the legend')
        lega[c('x','y')]=locator(1)
        if(is.null(lega$x)){
          warning('placing the legend with locator(1) failed')
          lega$y=NULL
          lega$x=if(seg$ll>1) 'topright' else 'right'
        }
      }
      tf=par('font')
      par(font=font['leg.title'])
      do.call(legend,lega)
      par(font=tf)
    }else warning('legend positioning failed',call.=FALSE)
  }
  if(ck$sud && any(ck$su,ck$c,is.character(sud))) mtext(if(is.character(sud)) sud else
    gsub(', (?=[A-z0-9 ]+$)',ifelse(length(ptxt$cov)>2,', & ',' & '), gsub('^ | $','',paste0(if(ck$su)
      paste('Subset:',paste0(txt$su[1],if(length(txt$su)!=1)'...')),if(ck$su && ck$c)', ',if(ck$c)
        paste(if(ck$t==1)'Covariates:' else 'Line adjustment:',paste(ptxt$cov,collapse=', ')))),TRUE,TRUE)
    ,3,0,TRUE,cex=cex['sud'],font=font['sud'])
  mtext(main,3,if(ck$sud) 1.5 else .5,TRUE,cex=cex['title'],font=font['title'])
  mtext(ylab,2,-.2,TRUE,cex=par('cex.lab'),font=par('font.lab'))
  mtext(xlab,1,0,TRUE,cex=par('cex.lab'),font=par('font.lab'))
  if(is.character(note)) mtext(note,1,ck$lx,TRUE,adj=if(ck$ly) 0 else .01,font=font['note'],cex=cex['note'])
  if(save || (missing(save) && any(!missing(format),!missing(file.name),!missing(dims)))) tryCatch({
    if(is.character(format) || is.name(format)){
      t=as.character(format)
      format=eval(parse(text=t))
    }else t=deparse(substitute(format))
    if(is.function(format)) t=sub('^[^:]*::','',t)
    tt=if(any(grepl('cairo',t,TRUE))){
      paste0('.',tolower(strsplit(t,'_|Cairo')[[1]][2]))
    }else if(t=='postscript') '.ps' else paste0('.',t)
    if(missing(dims) && grepl('jpeg|png|tiff|bmp|bit',t,TRUE)) dims=dev.size(units='px')
    fn=paste0(if(main=='' || !missing(file.name)) file.name else gsub(' ','_',gsub('^ +| +$|  ','',main),fixed=TRUE),tt)
    dev.copy(format,fn,width=dims[1],height=dims[2])
    dev.off()
    message('image saved: ',getwd(),'/',fn)
  },error=function(e)warning('unable to save image: ',e$message,call.=FALSE))
  invisible(list(dat=dat,cdat=cdat,txt=txt,ptxt=ptxt,seg=seg,ck=ck,lega=lega,fmod=fmod))
}