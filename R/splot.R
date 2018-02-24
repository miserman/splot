#' Split Plot
#'
#' A plotting function aimed at automating some common visualization tasks in order to ease data exploration.
#' @param y A formula (see note), or the primary variable(s) to be shown on the y axis unless (\code{x} is not specified).
#'   When not a formula, this can be one or more variables as objects or names in \code{data}.
#' @param data a \code{data.frame} to pull variables from. If variables aren't found in \code{data}, they will be looked for
#'   in the environment.
#' @param su a subset to all variables, applied after they are all retrieved from \code{data} or the environment.
#' @param type determines the type of plot to make, between \code{"bar"}, \code{"line"}, \code{"density"}, or
#'   \code{"scatter"}. If \code{"density"}, \code{x} is ignored. Anything including the first letter of each is accepted
#'   (e.g., \code{type='l'}).
#' @param split how to split any continuous variables (those with more than \code{lim} levels as factors). Default is
#'   \code{"median"}, with \code{"mean"}, \code{"standard deviation"}, \code{"quantile"}, or a number as options. If a number,
#'   the variable is broken into roughly equal chunks.
#' @param levels a list with entries corresponding to variable names, used to rename and/or reorder factor levels. To
#'   reorder a factor, enter a vector of either numbers or existing level names in the new order (e.g.,
#'   \code{levels =}\code{list(var =} \code{c(3,2,1))}). To rename levels of a factor, enter a character vector the same
#'   length as the number of levels. To rename and reorder, enter a list, with names as the first entry, and order as the
#'   second entry (e.g., \code{levels =} \code{list(var =} \code{list(c('a','b','c'),} \code{c(3,2,1)))}). This happens after
#'   variables are split, so names and orders should correspond to the new split levels of split variables. For example, if
#'   a continuous variable is median split, it now has two levels ('Under Median' and 'Over Median'), which are the levels
#'   reordering or renaming would apply to. Multiple variables entered as \code{y} can be renamed and sorted with an entry
#'   titled 'mv'.
#' @param sort string: if \code{x} is a character or factor, specifies how it should be sorted in terms of the level's \code{y}
#'   value. Unspecified or \code{NULL} won't do any additional sorting. Anything starting with 'd' or 't' will sort highest to
#'   lowest.
#' @param error string: sets the type of error bars to show in bar or line plots, or turns them off. If \code{FALSE}, no error
#'   bars will be shown. Otherwise, the default is \code{"standard error"} (\code{'^s'}), with \code{"confidence intervals"}
#'   (anything else) as an option.
#' @param error.color color of the error bars. Default is \code{'#585858'}.
#' @param error.lwd line weight of error bars. Default is 2.
#' @param lim numeric. Checked against the number of factor levels of each variable. Used to decide which variables should
#'   be split, which colors to use, and when to turn off the legend. Default is \code{9}. If set over \code{20}, \code{lim}
#'   is treated as infinite (set to \code{Inf}).
#' @param lines logical or a string specifying the type of lines to be drawn in scatter plots. By default (and whenever
#'   \code{cov} is not missing), a prediction line is fitted with \code{\link[stats]{lm}}. For (potentially) bendy lines,
#'   \code{'loess'} (matching \code{'^lo|^p|^cu'}) will use \code{\link[stats]{loess}}, and \code{'spline'}
#'   (\code{'^sm|^sp|^in'}) will use \code{\link[stats]{smooth.spline}}. \code{'connected'} (\code{'^e|^co|^d'}) will draw
#'   lines connecting all points, and \code{FALSE} will not draw any lines.
#' @param ... passes additional arguments to \code{\link[graphics]{par}} or \code{\link[graphics]{legend}}. The
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
#' @param mv.as.x logical: if \code{TRUE}, variable names are displayed on the x axis, and \code{x} is treated as \code{by}.
#' @param save logical: if \code{TRUE}, an image of the plot is saved to the current working directory.
#' @param format the type of file to save plots as. default is \code{\link[grDevices]{cairo_pdf}}. See
#'   \code{\link[grDevices]{Devices}} for options.
#' @param dims a vector of 2 values (\code{c(width, height)}) specifying the dimensions of a plot to save in inches or
#'   pixels depending on \code{format}. Defaults to the dimensions of the plot window.
#' @param file.name a string with the name of the file to be save (excluding the extension, as this is added depending on
#'   \code{format}).
#' @param colors sets a color theme or manually specifies colors. Default theme is \code{"pastel"}, with \code{"dark"} and
#'   \code{"bright"} as options; these are passed to \code{\link{splot.color}}. If set to \code{"grey"}, or if \code{by} has
#'   more than 9 levels, a grey scale is calculated using \code{\link[grDevices]{grey}}. See the \code{col} parameter in
#'   \code{\link[graphics]{par}} for acceptable manual inputs. To set text and axis colors, \code{col} sets outside texts
#'   (title, sud, labx, laby, and note), \code{col.sub} or \code{col.main} sets the frame titles, and \code{col.axis} sets
#'   the axis text and line colors.
#' @param colorby a variable used to set colors and the legend, alternatively to \code{by}. If \code{by} is not missing,
#'   \code{colorby} will be reduced to only the unique combinations of \code{by} and \code{colorby}. For example, if \code{by}
#'   is a participant ID with multiple observations per participant, and \code{by} is a condition ID which is the same for all
#'   observations from a given participant, \code{colorby} would assign a single color to each participant based on their
#'   condition. If \code{by} is missing, \code{colorby} will only be applied if its levels are unique, in which case a color
#'   will be assigned to each level. The variable entered here is passed to \code{\link{splot.color}}, with \code{colors} as
#'   its \code{set} argument, and \code{by} as its \code{by} argument.
#' @param myl sets the range of the y axis (\code{ylim} of \code{\link[graphics]{plot}} or \code{\link[graphics]{barplot}}).
#'   If not specified, this will be calculated from the data.
#' @param mxl sets the range of the x axis (\code{xlim} of \code{\link[graphics]{plot}}). If not specified, this will be
#'   calculated from the data.
#' @param autori logical: if \code{FALSE}, the origin of plotted bars will be set to 0. Otherwise, bars are adjusted such
#'   that they extend to the bottom of the y axis.
#' @param xlas,ylas numeric: sets the orientation of the x- and y-axis labels. See \code{\link[graphics]{par}}.
#' @param bw sets the smoothing bandwidth when plotting densities. Default is \code{'nrd0'}. See
#'   \code{\link[stats]{density}}.
#' @param adj adjusts the smoothing of densities (\code{adj * bw}). See \code{\link[stats]{density}}.
#' @param leg sets the legend inside or outside the plot frames (when a character matching \code{'^i'}, or a character
#'   matching \code{'^o'} or a number respectively), or turns it off (when \code{FALSE}). When inside, a legend is drawn in
#'   each plot frame. When outside, a single legend is drawn either to the right of all plot frames, or within an empty plot
#'   frame. By default, this will be determined automatically, tending to set legends outside when there are multiple levels
#'   of \code{between}. A number will try and set the legend in an empty frame within the grid of plot frames. If there are
#'   no empty frames, the legend will just go to the side as if \code{leg='outside'}.
#' @param lpos sets the position of the legend within its frame (whether inside or outside of the plot frames) based on
#'   keywords (see \code{\link[graphics]{legend}}. By default, when the legend is outside, \code{lpos} is either
#'   \code{'right'} when the legend is in a right-hand column, or \code{'center'} when in an empty plot frame. When the
#'   legend is inside and \code{lpos} is not specified, the legend will be placed automatically based on the data.
#' @param lvn legend variable name. Logical: if \code{FALSE}, the names of by and between variables will not be shown before
#'   their level (e.g., for a sex variable with a "female" level, "sex: female" would become "female" in the legend or above
#'   each plot window).
#' @param title logical or a character: if \code{FALSE}, the main title is turned off. If a character, this will be shown as
#'   the main title.
#' @param labx logical or a character: if \code{FALSE}, the label on the x axis is turned off. If a character, this will be
#'   shown as the x axis label.
#' @param laby logical or a character: if \code{FALSE}, the label on the y axis is turned off. If a character, this will be
#'   shown as the y axis label.
#' @param lty logical or a vector: if \code{FALSE}, lines are always solid. If a vector, changes line type based on each
#'   value. Otherwise loops through available line types, see \code{\link[graphics]{par}}.
#' @param lwd numeric: sets the weight of lines in line, density, and scatter plots. Default is 2. See
#'   \code{\link[graphics]{par}}.
#' @param sub logical: if \code{FALSE}, the small title above each plot showing \code{between} levels is turned off.
#' @param note logical: if \code{FALSE}, the note at the bottom about splits and/or lines or error bars is turned off.
#' @param font named numeric vector: \code{c(title,sud,leg,note)}. Sets the font of the title, su display, legend, and note.
#'   In addition, \code{font.lab} sets the x and y label font, \code{font.sub} sets the font of the little title in each panel,
#'   \code{font.axis} sets the axis label font, and \code{font.main} sets the between level/n heading font; these are passed to
#'   \code{\link[graphics]{par}}. See the input section.
#' @param cex named numeric vector: \code{c(title,sud,leg,note)}. Sets the font size of the title, su display, legend, and note.
#'   In addition, \code{cex.lab} sets the x and y label size, \code{cex.sub} sets the size of the little title in each panel,
#'   \code{cex.axis} sets the axis label size, and \code{cex.main} sets the between level/n heading size; these are passed to
#'   \code{\link[graphics]{par}}. See the input section.
#' @param sud logical: if \code{FALSE}, the heading for subset and covariates/line adjustments (su display) is turned off.
#' @param ndisp logical: if \code{FALSE}, n per level is no longer displayed in the subheadings.
#' @param labels logical: if \code{FALSE}, sets all settable text surrounding the plot to \code{FALSE} (just so you don't
#'   have to set all of them if you want a clean frame).
#' @param labels.filter a regular expression string to be replaced in label texts with a blank space. Default is
#'   \code{'_|\\\\.'}; underscores and periods appearing in the text of labels are replace with blank spaces. Set to
#'   \code{FALSE} to prevent all filtering.
#' @param labels.trim numeric or logical: the maximum length of label texts (in number of characters). Default is 20, with
#'   any longer labels being trimmed. Set to \code{FALSE} to prevent any trimming.
#' @param points logical: if \code{FALSE}, the points in a scatter plot are no longer drawn.
#' @param points.first logical: if \code{FALSE}, points are plotted after lines are drawn in a scatter plot, placing lines
#'   behind points. This does not apply to points or lines added in \code{add}, as that is always evaluated after the main
#'   points and lines are drawn.
#' @param byx logical: if \code{TRUE} (default) and \code{by} is specified, regressions for bar or line plots compare levels
#'   of \code{by} for each level of \code{x}. This makes for more intuitive error bars when comparing levels of \code{by}
#'   within a level of \code{x}.
#' @param drop named logical vector: \code{c(x,by,bet)}. Specifies how levels with no data should be treated. All are
#'   \code{TRUE} by default, meaning only levels with data will be presented, and the layout of \code{between} levels
#'   will be minimized. \code{x} only applies to bar or line plots. \code{by} relates to levels presented in the legend.
#'   If \code{bet} is \code{FALSE}, the layout of \code{between} variables will be strict, with levels of \code{between[1]}
#'   as rows, and levels of \code{between[2]} as columns -- if there are no data at an intersection of levels, the
#'   corresponding panel will be blank. See the input section.
#' @param prat panel ratio, referring to the ratio between plot frames and the legend frame when the legend is out. A single
#'   number will make all panels of equal width. A vector of two numbers will adjust the ratio between plot panels and the
#'   legend panel (e.g., \code{prat=c(3,1)} makes all plot panels a relative width of 3, and the legend frame a relative
#'   width of 1).
#' @param model logical: if \code{TRUE}, the summary of an interaction model will be printed.
#' @param options a list with named arguments, useful for setting temporary defaults if you plan on using some of the same
#'   options for multiple plots (e.g., \code{opt =} \code{list(type = 'bar',} \code{colors = 'grey',} \code{bg = '#999999');}
#'   \code{splot(x~y,} \code{options = opt)}).
#'   use \code{\link[base]{quote}} to include options that are to be evaluated within the function (e.g.,
#'   \code{opt =} \code{list(su =} \code{quote(y>0))}).
#' @param add evaluated within the function. Useful for adding things like lines to a plot while the parameters are still
#'   those set by the function (e.g., \code{add =} \code{abline(v =} \code{mean(x),} \code{xpd = FALSE)} for a vertical line
#'   at the mean of x).
#'
#' @return A list containing data and settings is invisibly returned, which might be useful to check for errors.
#' Each of these objects can also be pulled from within \code{add}:
#' \tabular{ll}{
#'   \code{data} \tab a \code{data.frame} of processed, unsegmented data.\cr
#'   \code{cdat} \tab a \code{list} of \code{list}s of \code{data.frame}s of processed, segmented data.\cr
#'   \code{txt} \tab a \code{list} of variable names. used mostly to pull variables from \code{data} or the environment.\cr
#'   \code{ptxt} \tab a \code{list} of processed variable and level names. Used mostly for labeling.\cr
#'   \code{seg} \tab a \code{list} containing segmentation information (such as levels) for each variable.\cr
#'   \code{ck} \tab a \code{list} of settings.\cr
#'   \code{model} \tab an \code{lm} object if \code{model} is \code{TRUE}, and the model succeeded.
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
#' with names. If a single value is entered (e.g., \code{drop=FALSE}), this will be applied to each level (i.e.,
#' \code{c(x=FALSE,by=FALSE,bet=FALSE)}). If more than one value is entered, these will be treated positionally (e.g.,
#' \code{cex=c(2,1.2)} would be read as \code{c(title=2,leg=1.2,note=.7)}). If values are named, only named values will be
#' set, with other defaults retained (e.g., \code{cex=c(note=1.2)} would be read as \code{c(title=1.5,leg=1,note=1.2)}).
#'
#' @note
#' \strong{x-axis levels text}
#'
#' If the text of x-axis levels (those corresponding to the levels of \code{x}) are too long, they are hidden before
#' overlapping. To try and avoid this, by default longer texts are trimmed (dictated by \code{labels.trim}), and at some
#' point the orientation of level text is changed (settable with \code{xlas}), but you may still see level text missing.
#' To make these visible, you can reduce \code{labels.trim} from the default of 20 (or rename the levels of that variable),
#' make the level text vertical (\code{xlas=3}), or expand your plot window if possible.
#'
#' \strong{missing levels, lines, and/or error bars}
#'
#' By default (if \code{drop=TRUE}), levels of \code{x} with no data are dropped, so you may not see every level of your
#' variable, at all or at a level of \code{by} or \code{between}. Sometimes error bars cannot be estimated (if, say, there
#' is only one observation at the given level), but lines are still drawn in these cases, so you may sometimes see levels
#' without error bars even when error bars are turned on. Sometimes (particularly when \code{drop['x']} is \code{FALSE}),
#' you might see floating error bars with no lines drawn to them, or what appear to be completely empty levels. This happens,
#' when there is a missing level of \code{x} between two non-missing levels, potentially making an orphaned level (if a
#' non-missing level is surrounded by missing levels). If there are no error bars for this orphaned level, by default
#' nothing will be drawn to indicate it. If you set \code{line.type} to \code{'b'} (or any other type with points), a point
#' will be drawn at such error-bar-less orphaned levels.
#'
#' \strong{unexpected failures}
#'
#' splot tries to clean up after itself in the case of an error, but you may still run into errors that break things before
#' this can happen. If after a failed plot you find that you're unable to make any new plots, or new plots are drawn over old
#' ones, you might try entering \code{dev.off()} into the console. If new plots look off (splot's \code{\link[graphics]{par}}
#' settings didn't get reset), you may have to close the plot window to reset \code{\link[graphics]{par}} (if you're using
#' RStudio, Plots > "Remove Plot..." or "Clear All..."), or restart R.
#'
#' @examples
#' #simulating data
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
#' #looking at the distribution of y between bets split by by
#' splot(y, by=by, between=c(bet1, bet2), data=dat)
#'
#' #looking at quantile splits of y in y by x
#' splot(y~x*y, dat, split='quantile')
#'
#' #looking at y by x between bets
#' splot(y~x, dat, between=c(bet1, bet2))
#'
#' #sequentially adding levels of split
#' splot(y~x*by, dat)
#' splot(y~x*by*bet1, dat)
#' splot(y~x*by*bet1*bet2, dat)
#'
#' #same as the last but entered by name
#' splot(y, x=x, by=by, between=c(bet1, bet2), data=dat)
#'
#' #zooming in on one of the windows
#' splot(y~x*by, dat, bet1==1&bet2==0)
#'
#' #comparing an adjusted lm prediction line with a loess line
#' #this could also be entered as y ~ poly(x,3)
#' splot(y~x+x^2+x^3, dat, bet1==1&bet2==0&by==1, add={
#'   lines(x[order(x)], loess(y~x)$fitted[order(x)], lty=2)
#'   legend('topright', c('lm', 'loess'), lty=c(1, 2), lwd=c(2, 1), bty='n')
#' })
#'
#' #looking at different versions of x added to y
#' splot(cbind(
#'   Raw=y+x,
#'   Sine=y+sin(x),
#'   Cosine=y+cos(x),
#'   Tangent=y+tan(x)
#' )~x, dat, myl=c(-10,15), lines='loess', laby='y + versions of x')
#'
#' @export
#' @importFrom grDevices grey dev.copy dev.size dev.off cairo_pdf
#' @importFrom graphics axis axTicks hist legend lines mtext plot barplot par points arrows strwidth layout plot.new
#' @importFrom stats density median quantile sd lm confint update loess smooth.spline na.omit formula as.formula predict var

splot=function(y,data=NULL,su=NULL,type='',split='median',levels=list(),sort=NULL,error='standard',error.color='#585858',
  error.lwd=2,lim=9,lines=TRUE,...,x=NULL,by=NULL,between=NULL,cov=NULL,line.type='l',mv.scale='none',mv.as.x=FALSE,
  save=FALSE,format=cairo_pdf,dims=dev.size(),file.name='splot',colors='pastel',colorby=NULL,myl=NULL,mxl=NULL,autori=TRUE,
  xlas=0,ylas=1,bw='nrd0',adj=2,leg='outside',lpos='auto',lvn=TRUE,title=TRUE,labx=TRUE,laby=TRUE,lty=TRUE,lwd=2,sub=TRUE,
  ndisp=TRUE,note=TRUE,font=c(title=2,sud=1,leg=1,note=3),cex=c(title=1.5,sud=.9,leg=1,note=.7),sud=TRUE,labels=TRUE,
  labels.filter='_|\\.',labels.trim=20,points=TRUE,points.first=TRUE,byx=TRUE,drop=c(x=TRUE,by=TRUE,bet=TRUE),
  prat=c(1,1),model=FALSE,options=NULL,add=NULL){
  #parsing input and preparing data
  if(!missing(options) && is.list(options) && length(options)!=0){
    a=as.list(match.call())[-1]
    options=tryCatch(options,error=function(e)NULL)
    if(is.null(options)) stop('could not find options')
    return(do.call(splot,c(a[names(a)!='options'],options[!names(options)%in%names(a)])))
  }
  if(!labels) title=sud=sub=labx=laby=note=FALSE
  ck=list(
    ff=list(bet=FALSE,cov=FALSE),
    t=if(grepl('^b|^l',type,TRUE)) 1 else if(grepl('^d',type,TRUE)) 2 else 3,
    b=grepl('^b',type,TRUE),
    tt=!missing(type) && if(grepl('^b|^l',type,TRUE)) FALSE else TRUE,
    d=!missing(data) && !is.null(data),
    su=!missing(su),
    c=!missing(cov),
    co=missing(colors),
    cb=!missing(colorby),
    e=grepl('^s',error,TRUE),
    el=!(is.logical(error) && !error),
    sp=if(!is.character(split)) 4 else if(grepl('^mea|^av',split,TRUE)) 1 else if(grepl('^q',split,TRUE)) 2 else
      ifelse(grepl('^s',split,TRUE),3,4),
    ly=!(is.logical(laby) && !laby) || is.character(laby),
    lx=!(is.logical(labx) && !labx) || is.character(labx),
    lty=is.logical(lty),
    ltm=missing(line.type),
    leg=if(is.logical(leg) && !leg) 0 else if(!is.character(leg) || grepl('^o',leg,TRUE)) 1 else 2,
    legm=missing(leg),
    lp=is.character(lpos) && grepl('^a',lpos,TRUE),
    mod=!missing(x) && model,
    mv=FALSE,
    mlvn=missing(lvn)
  )
  if(any(!missing(font),!missing(cex),!missing(drop))){
    dop=as.list(args(splot))[c('font','cex','drop')]
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
  if(any(grepl('~',substitute(y),fixed=TRUE))){
    f=as.character(as.formula(y))[-1]
    y=f[1]
    bl=function(x){
      cs=strsplit(x,'')[[1]]
      rs=lapply(c('(',')','[',']'),grep,cs,fixed=TRUE)
      l=vapply(rs,length,0)
      cr=TRUE
      if(any(l!=0)){
        if(l[1]!=l[2] || l[3]!=l[4]) stop('invalid parentheses or brackets in ',x)
        rs[c(2,4)]=lapply(rs[c(2,4)],rev)
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
      if(length(r)>0) x=r[1]
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
    if(length(f)>0){
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
  txt=lapply(txt,function(e)if(is.call(e)) deparse(e) else e)
  if(length(txt$bet)>2) txt$bet=txt$bet[1:2]
  tdc=function(x,l=NULL){
    if(!is.null(l) && length(x)==l) return(x)
    if(is.character(x)) x=parse(text=x)
    tx=tryCatch(eval(x,data,parent.frame(2)),error=function(e)NULL)
    if(is.character(tx) && length(tx)<2){
      x=parse(text=tx)
      tx=tryCatch(eval(x,data,parent.frame(2)),error=function(e)NULL)
    }else if(is.null(tx)) tx=tryCatch(eval(x,data,parent.frame(3)),error=function(e)NULL)
    if(is.null(tx) || class(tx)%in%c('name','call','expression','function')) stop('could not find ',x,call.=FALSE)
    if(!is.null(l) && is.null(ncol(tx))) if(length(tx)!=l)
      warning(x,' is not the same length as y',call.=FALSE)
    tx
  }
  if(!missing(data) && !class(data)%in%c('matrix','data.frame'))
    data=if(is.character(data)) eval(parse(text=data)) else eval(data,globalenv())
  dat=data.frame(y=tdc(txt$y))
  if(ncol(dat)==1) names(dat)='y'
  rn=nrow(dat)
  if(length(txt$y)==rn) txt$y='y'
  for(n in names(txt)[-c(1,2,7)]){
    l=length(txt[[n]])
    if(l==0) next
    if(l==rn){
      dat[,n]=txt[[n]]
      txt[[n]]=n
    }else if(l==1) dat[,n]=tdc(txt[[n]],rn) else for(i in seq_along(txt[[n]])) dat[,paste0(n,'.',i)]=tdc(txt[[n]][[i]],rn)
  }
  if(NCOL(dat$x)>1){
    ck$c=TRUE
    txt$cov=c(txt$x,txt$cov)
    dat$cov=cbind(dat$cov,dat$x[,-1])
    dat$x=dat$x[,1]
  }
  if(!missing(colorby)){
    colorby=substitute(colorby)
    dat$cb=tdc(colorby,rn)
    if(ck$su) dat=if(ck$d) dat[eval(substitute(su),data),,drop=FALSE] else dat[su,,drop=FALSE]
    dat=na.omit(dat)
    colorby=dat$cb
    dat=dat[-ncol(dat)]
  }else{
    if(ck$su) dat=if(ck$d) dat[eval(substitute(su),data),,drop=FALSE] else dat[su,,drop=FALSE]
    dat=na.omit(dat)
  }
  if(nrow(dat)==0) stop('this combination of variables/splits has no complete cases')
  dn=colnames(dat)
  if(sum(grepl('^y',dn))>1){
    #setting up multiple y variables
    if(!ck$d){
      tcn=if(is.null(names(tcn<-tdc(txt$y)))) colnames(tcn) else names(tcn)
      if(!is.null(tcn) && length(tcn)==ncol(dat)) colnames(dat)=tcn
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
      dat$bet=cbind(as.character(dat$by),as.character(dat$bet))
    }
    dn=grep('^y\\.',dn)
    ck$mvn=colnames(dat)[dn]
    td=dat
    if(any(ckn<-duplicated(ck$mvn))) ck$mvn[ckn]=paste0(ck$mvn[ckn],'_',seq_len(sum(ckn)))
    by=sub('^y\\.','',ck$mvn)
    by=factor(rep(by,each=nrow(dat)),levels=by)
    dat=data.frame(y=unlist(dat[,dn],use.names=FALSE))
    if(ncol(td)>length(dn)) dat=cbind(dat,apply(td[,-dn,drop=FALSE],2,function(c)rep.int(c,length(dn))))
    if(mv.as.x){
      txt$by=txt$x
      txt$x=if(missing(labx)) 'variable' else if(labx==txt$by) paste0(labx,'.1') else labx
      dat$by=dat$x
      dat$x=by
    }else{
      txt$by='variable'
      dat$by=by
    }
    if(!missing(levels) && 'mv'%in%names(levels)) names(levels)[names(levels)=='mv']=txt[[if(mv.as.x)'x'else'by']]
    dn=colnames(dat)
    if(!missing(mv.scale) && mv.scale!='none'){
      tv=if(mv.as.x) dat$x else dat$by
      for(g in levels(as.factor(tv))) dat[tv==g,1]=scale(dat[tv==g,1],scale=grepl('^t|z|sc',mv.scale,TRUE))
    }
  }
  if(!'x'%in%dn){
    ck$t=2
    if(!missing(type) && !grepl('^d',type,TRUE)) message('x must be included to show other types of splots')
  }
  if(!ck$cb && !'by'%in%dn) ck$leg=0
  if(lim>20 || (is.logical(lim) && !lim)){
    lim=Inf
    if(ck$legm) ck$leg=0
    if(missing(error)) ck$el=FALSE
  }
  if(ck$ltm && !ck$el) line.type='b'
  if(missing(lty) && is.logical(lines) && !lines){ck$lty=FALSE; lty=1}
  odat=dat
  #splitting and parsing variables
  splt=function(x,s){
    if(s==1){
      txt$split<<-'mean'
      factor(ifelse(x<=mean(x),0,1),labels=c('Below Average','Above Average'))
    }else if(s==3){
      txt$split<<-'standard deviation'
      m=mean(x)
      s=sd(x)
      v=c(m-s,m+s)
      factor(ifelse(x<=v[1],0,ifelse(x>=v[2],2,1)),labels=c('-1 SD','Mean','+1 SD'))
    }else if(s==2){
      txt$split<<-'quantile'
      v=quantile(x)
      factor(ifelse(x<=v[2],0,ifelse(x>=v[4],2,1)),labels=c('2nd Quantile','Median','4th Quantile'))
    }else if(s==4 && is.numeric(split) && split>1){
      n=length(x)
      split=min(n,split)
      txt$split<<-paste0('segments (',split,')')
      factor(paste('seg',rep(seq_len(split),each=round(n/split+.5))[order(order(x))]))
    }else{
      txt$split<<-'median'
      factor(ifelse(x<=median(x),0,1),labels=c('Under Median','Over Median'))
    }
  }
  seg=list(
    x=list(s=FALSE,i=2),
    f1=list(e=FALSE,s=FALSE,l='',ll=1),
    f2=list(e=FALSE,s=FALSE,l='',ll=1),
    by=list(e=FALSE,s=FALSE,l='',ll=1)
  )
  if(!missing(x) && ck$t!=2) if((ck$t==1 || is.character(dat$x) || is.factor(dat$x)
    || (missing(type) && nlevels(factor(dat$x))<lim))){
    dat$x=if(!is.character(dat$x) && !is.factor(dat$x) && nlevels(factor(dat$x))>lim){
      seg$x$s=TRUE
      if(missing(type)) ck$t=1
      splt(dat$x,ck$sp)
    }else if(ck$t!=1 && is.factor(dat$x) && !any(grepl('[a-df-z]',dat$x,TRUE))){
      as.numeric(dat$x)
    }else{
      if(missing(type)) ck$t=1
      factor(dat$x)
    }
  }
  if(ck$t==1 || (is.character(dat$x) || is.factor(dat$x))){
    seg$x$l=(if(is.factor(dat$x)) base::levels else unique)(dat$x)
    if(length(seg$x$l)==1) stop('x has only 1 level within the complete cases of this set')
  }
  svar=NULL
  cvar=if(any(grepl('^c',dn))) which(grepl('^c',dn)) else NULL
  if(any(grepl('^b',dn))){
    svar=which(grepl('^b',dn))
    for(i in svar){
      e=if(grepl('bet',dn[i])) if(!seg$f1$e) 'f1' else 'f2' else 'by'
      seg[[e]]$e=TRUE
      seg[[e]]$i=i
      seg[[e]]$l=(if(is.factor(dat[,i])) base::levels else unique)(dat[,i])
      seg[[e]]$ll=length(seg[[e]]$l)
      if(seg[[e]]$ll>lim && !(is.character(dat[,i]) || is.factor(dat[,i]))){
        dat[,i]=splt(dat[,i],ck$sp)
        seg[[e]]$s=TRUE
        seg[[e]]$l=levels(dat[,i])
        seg[[e]]$ll=length(seg[[e]]$l)
      }
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
      if(length(cvar)>0) paste0('+',paste0(vs['cov'],collapse='+'))
    ))
    fmod=lm(mod,data=odat)
    if(model){
      s=summary(fmod)
      s$call=mod
      print(s)
    }
  },error=function(e)warning(paste('summary model failed:',e$message),call.=FALSE))
  if(!missing(levels)) tryCatch({
    lc=c('x','by','f1','f2')
    ns=c(txt$x,txt$by,txt$bet,lc)
    lc=c(lc[seq_len(length(ns)-length(lc))],lc)
    for(n in names(levels)){
      if(any(cns<-ns%in%n)){
        sl=seg[[lc[cns<-which(cns)[1]]]]
        vfac=(if(is.factor(dat[,sl$i])) base::levels else unique)(dat[,sl$i])
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
      }
    }
  },error=function(e)warning('setting levels failed: ',e$message,call.=FALSE))
  dsf=list(c1='',sep=rep('^^',nrow(dat)),c2='')
  if(seg$f1$e) dsf$c1=dat[,seg$f1$i]
  if(seg$f2$e) dsf$c2=dat[,seg$f2$i]
  cdat=split(dat,dsf)
  if(seg$by$e){
    cdat=lapply(cdat,function(s)if(length(unique(s$by))>1) split(s,s$by) else{
      s=list(s)[seg$by$l];names(s)=seg$by$l;s[vapply(s,is.null,TRUE)]=0;s
    })
    if(all((seg$n<-vapply(cdat,length,0))==seg$by$ll)){
      seg$n=vapply(cdat,function(s)vapply(s,NROW,0),numeric(seg$by$ll))
    }else drop['by']=FALSE
  }else seg$n=vapply(cdat,nrow,0)
  if(seg$by$e && drop['by']){
    seg$by$l=apply(seg$n,1,function(c)any(c>1))
    if(!any(seg$by$l)){
      if(ck$t==2) stop('no level of by has more than 1 observation')
      warning('no level of by has more than 1 observation so it was treated as colorby')
      seg$by$e=FALSE
      seg$by$l=''
      seg$by$ll=1
      colorby=dat$by
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
      if(length(n)!=0 && n!='NULL' && n!=''){
        if(is.character(labels.filter)) n=gsub(labels.filter,' ',n,perl=TRUE)
        if(is.numeric(labels.trim)) if(any(ln<-nchar(n)>(labels.trim+3))) n[ln]=sub('$','...',strtrim(n[ln],labels.trim))
      }
      n
    })
    names(ptxt)=vs
  }
  if(is.character(labx)) ptxt$x=labx
  if(is.character(laby)) ptxt$y=laby
  #figuring out parts of the plot
  if(!missing(colors)) colors=eval(substitute(colors),data)
  if(length(colors)==1){
    if(grepl('^bri|^dar|^pas',colors,TRUE) && (seg$by$ll>1 && (ck$cb || seg$by$ll<9))){
      colors=splot.color(colors)
    }else if(ck$co || grepl('^gra|^grey',colors,TRUE)) colors=splot.color('grey',ns=seg$by$ll)
  }
  if(ck$cb) if('by'%in%names(dat)){
    colorby=cbind(dat$by,as.character(colorby))
    colorby=colorby[!duplicated(colorby),]
    if(nrow(colorby)!=seg$by$ll){
      ck$cb=FALSE
      warning('colorby was ignored as it did not line up with by')
    }
  }else{
    if(ck$t!=3 && any(duplicated(colorby))){
      ck$cb=FALSE
      if(ck$co) colors='#666666'
      warning("colorby was ignored as by is missing and colorby's levels are not unique")
    }else{
      colorby=cbind(dat$x,as.character(colorby))
      if(is.factor(dat$x)) colorby=colorby[order(dat$x),]
      ptxt$l.by=unique(colorby[,2])
    }
  }
  if(ck$cb){
    ptxt$cb=unique(colorby[,2])
    cs=if(ck$co && length(colors)==1) splot.color('grey',ns=length(ptxt$cb)) else rep_len(colors,length(ptxt$cb))
    colors=if(length(cs)==nrow(colorby)) cs else splot.color(cs,by=colorby[,2])
  }else{
    cs=colors
    colors=rep_len(colors,seg$by$ll)
  }
  if(lvn && length(ptxt$by)!=0) ptxt$l.by=paste0(paste0(ptxt$by,': '),ptxt$l.by)
  if(length(colors)==length(ptxt$l.by)) names(colors)=names(ptxt$l.by)=if(seg$by$l[1]=='NA') ptxt$l.by else seg$by$l
  if(ck$t==3 && length(colors)==1) colors[2]=if(!ck$co && length(cs)>1) cs[2] else if(ck$t==3) '#999999' else '#adadad'
  ylab=if(ck$ly) ptxt$y else ''
  xlab=if(ck$lx) ptxt$x else ''
  main=if(is.logical(title) && title) paste0(if(ck$t==2)paste('Density of',ptxt$y) else paste(ptxt$y,
    'by',ptxt$x),if(seg$by$e && !ck$mv) paste(' at levels of',ptxt$by), if(length(ptxt$bet)!=0) paste(' between',
      paste(ptxt$bet,collapse=' & '))) else if (is.character(title)) title else ''
  if(!is.character(note)) if(!is.logical(note) || note){
    if(txt$split!='none' || (ck$t==1 && ck$el)){
      tv=c(if(seg$x$s) ptxt$x else '',if(seg$by$s) ptxt$by else '',if(seg$f1$s) ptxt$bet[1] else '',
        if(seg$f2$s) ptxt$bet[2] else '')
      tv=tv[tv!='']
      tv=gsub(', (?=[a-z0-9]+$)',ifelse(length(tv)>2,', & ',' & '),paste(tv,collapse=', '),TRUE,TRUE)
      note=paste0(
        if(txt$split!='none') paste0(tv,' split by ',txt$split,'. '),
        if(ck$t==1 && ck$el) paste('Error bars show',ifelse(ck$e,'standard error.','95% confidence intervals.'))
      )
    }
  }else note=''
  ck$sud=sud && (ck$su || ck$c)
  ck$sub=sub && (seg$ll>1 || ndisp)
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
  if(!ck$cb && seg$by$l[1]=='NA') ck$leg=0
  if(!ck$cb && ck$leg==1 && ck$legm && (dev.size(units='in')[1]<2
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
    }
    if(nc==seg$ll || leg>nc){
      seg$dmat[seg$dmat==seg$ll+1]=nc+1
      seg$dmat=rbind(seg$dmat,rep(seg$ll+1,seg$dim[1]))
      ck$legcol=TRUE
    }
    if(ck$lp) lpos=if(ck$legcol) 'right' else 'center'
  }
  seg[c('dmat','lc')]=lapply(seg[c('dmat','lc')],t)
  seg$prat=if(missing(prat) && ck$legcol){
    lw=max(.4,strwidth(ptxt$l.by,'i'))+if(all(seg$dim==1)) .5 else .2
    fw=(dev.size(units='in')[1]-lw)/seg$dim[2]
    c(fw,max(fw/10,lw))
  }else prat
  op=list(
    oma=c(sum(note!='',ck$lx)+.1,ck$ly,max(sum((main!='')*2+if(sum(seg$dim)>2) .5 else 0,ck$sud),1),0),
    mar=c(if(ck$lx) 2.5 else 1.5,if(ck$ly) 3 else 2,(ck$sud && (ck$su || ck$c))*ifelse(seg$ll>1,2,.5)+
        (ck$sub && sum(seg$dim)>2)+.5,!ck$legcol),
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
  lega=list(col=if(ck$cb) cs else colors[names(ptxt$l.by)],lty=if(ck$lty && lty) seq_len(seg$by$ll) else if(!missing(lty)
    && !ck$lty) lty else 1,lwd=lwd,cex=cex['leg'],text.font=font['leg'],bty='n',x.intersp=.5)
  if(!'horiz'%in%names(pdo)) lega$ncol=1
  if(length(pdo)!=0){
    if(any(cpdo<-npdo%in%names(as.list(args(legend))))) lega[npdo[cpdo]]=pdo[cpdo]
    if(any(!cpdo)) warning('unused argument',if(sum(!cpdo)==1) ': ' else 's: ' ,
      paste(names(pdo)[!cpdo],collapse=','),call.=FALSE)
  }
  par(op)
  on.exit(par(dop))
  layout(seg$dmat,c(rep(seg$prat[1],seg$dim[2]),if(ck$legcol) seg$prat[if(length(seg$prat)>1) 2 else 1]))
  success=FALSE
  for(i in names(cdat)){tryCatch({
    #plotting
    cl=(if(class(cdat[[i]])=='list') vapply(cdat[[i]],NROW,0) else nrow(cdat[[i]]))>1
    if(any(!cl)){
      cdat[[i]]=cdat[[i]][cl]
      if(length(cdat[[i]])==0) next
    }
    cl=strsplit(i,'.^^.',fixed=TRUE)[[1]]
    ptxt$sub=if(sub) if(seg$ll>1 || (!missing(ndisp) && ndisp)) paste0(
      if(seg$f1$e) paste0(
        if(lvn || (ck$mlvn && grepl('^[0-9]',cl[1]))) paste0(ptxt$bet[1],': '),cl[1],
        if(seg$f2$e) paste0(', ',if(lvn || (ck$mlvn && grepl('^[0-9]',cl[2]))) paste0(ptxt$bet[2],': '),cl[2])
      ),if((length(names(cdat))>1 || !missing(ndisp)) && ndisp) paste(', n =',seg$n[i])
    ) else if(is.character(sub)) sub else ''
    if(!is.null(sort) && ck$t!=2 && class(if(seg$by$e) cdat[[i]][[1]][,'x'] else cdat[[i]][,'x'])%in%c('factor','character')){
      sdir=grepl('^d|^t',as.character(sort),TRUE)
      td=if(seg$by$e) do.call(rbind,cdat[[i]]) else cdat[[i]]
      cdat[[i]]=do.call(rbind,lapply(names(sort(vapply(split(td[,'y'],as.character(td[,'x'])),mean,0,na.rm=TRUE),sdir)),
        function(l) td[td[,'x']==l,,drop=FALSE]
      ))
      seg$x$l=ptxt$l.x=unique(cdat[[i]][,'x'])
      cdat[[i]][,'x']=factor(cdat[[i]][,'x'],seg$x$l)
      if(seg$by$e) cdat[[i]]=split(cdat[[i]],as.character(cdat[[i]][,'by']))
    }
    if(ck$t==1){
      #bar and line
      flipped=FALSE
      if(missing(byx) && ck$mv && any(vapply(cdat[[i]],function(d)any(vapply(split(d$y,as.character(d$x)),
        function(dl) if(length(dl)==1) 0 else var(dl),0)==0),TRUE)))
        byx=FALSE
      if(byx && lim<Inf && seg$by$e && (is.list(cdat[[i]]) && length(cdat[[i]])>1)){
        flipped=TRUE
        cdat[[i]]=do.call(rbind,cdat[[i]])
        cdat[[i]][c('x','by')]=cdat[[i]][c('by','x')]
        cdat[[i]]$x=as.factor(cdat[[i]]$x)
        cdat[[i]]=split(cdat[[i]],cdat[[i]]$by)
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
          su=which(cn%in%sub('x','',names(mo$coef)))
          sus=seq_along(su)
          m[ri,su]=mo$coef[sus]
          if(nrow(td)>2){
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
          mo=lapply(split(td,td['x']),function(s)if(nrow(s)==0) NA else mean(s[!is.na(s[,'y']),'y']))
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
      lb=min(re$m)-max(abs(re$m-re$ne))*1.2
      dm=dim(m)
      ylim=if(missing(myl))
        c(lb,max(re$m)+max(abs(re$m-re$pe))*if(ck$leg==2 && seg$by$ll>1) seg$by$ll+1 else 1) else myl
      if(ck$leg==2 && ck$lp) lpos=ifelse(any(!is.na(m[1,])) && any(!is.na(m[dm[1],])),
        ifelse(max(m[1,!is.na(m[1,])])>max(m[dm[1],!is.na(m[dm[1],])]),'topleft','topright'),'topright')
      if(any(is.na(ylim))) next
      oyl=axTicks(2,axp=c(ylim[1],ylim[2],par('yaxp')[3]))
      rn=rownames(m)
      colnames(m)=if(drop['x'] && sum(dx)==ncol(m)) ptxt$l.x[dx] else ptxt$l.x
      stw=strwidth(colnames(m),'i')
      if((missing(xlas) || xlas>1) && sum(stw)>
          par('fin')[1]-sum(par('omi')[c(2,4)])-dm[2]*.1 && par('fin')[1]>2.5){
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
      rck=nrow(m)>1
      if(!rck && ck$ltm && identical(ne,pe)) line.type='b'
      if(ck$b){
        if(autori){
          a=if(missing(myl)) lb else myl[1]
          a=a*-1
          m=m+a
          ne=ne+a
          pe=pe+a
          ayl=oyl+a
          aj=lapply(re,function(r)r+a)
          ylim=if(missing(myl)) c(
            min(aj$m)-max(abs(aj$m-aj$ne))*1.2,
            max(aj$m)+max(abs(aj$m-aj$pe))*if(ck$leg==2 && seg$by$ll>1) seg$by$ll else 2.2
          ) else myl+a
        }
        rownames(m)=ptxt$l.by[rn]
        if(ck$leg==2) lega$x=lpos
        lega$xpd=NA
        lega=lega[!names(lega)%in%c('lty','lwd')]
        p=barplot(m,beside=TRUE,legend.text=if(ck$leg==2) if(ck$cb) ptxt$cb else rownames(m) else FALSE,
          col=if(rck) colors[rn] else colors,axes=FALSE,axisnames=FALSE,border=NA,ylab=NA,xlab=NA,ylim=ylim,
          main=if(sub) ptxt$sub else NA,args.legend=lega,xpd=if('xpd'%in%names(pdo)) pdo$xpd else if(autori) NA else FALSE)
      }else{
        p=matrix(rep(seq_len(dm[2]),dm[1]),nrow=dm[1],byrow=TRUE)
        plot(NA,ylim=ylim,xlim=if(missing(mxl)) c(1-stw[1]/3,dm[2]+stw[length(stw)]/3) else mxl,ylab=NA,xlab=NA,
          main=if(sub) ptxt$sub else NA,axes=FALSE)
        if(is.numeric(lty)) lty=rep(lty,dm[1]) else if(is.logical(lty) && lty) lty=seq_len(dm[1])
        for(a in seq_len(dm[1])) lines(m[a,],col=if(rck) colors[rn[a]] else colors,
          lty=if(is.numeric(lty)) lty[a] else 1,lwd=lwd,type=line.type)
        if(ck$leg==2) do.call(legend,c(list(lpos,legend=if(ck$cb) ptxt$cb else ptxt$l.by[rn]),lega))
      }
      axis(1,apply(p,2,mean),colnames(m),FALSE,las=xlas,cex=par('cex.axis'),fg=par('col.axis'))
      a2a=list(2,las=ylas,cex=par('cex.axis'),fg=par('col.axis'))
      if(ck$b && autori){
        a2a$at=ayl
        a2a$labels=round(oyl,2)
      }
      do.call(axis,a2a)
      if(ck$el && !identical(ne,pe)){
        te=round(Reduce('-',list(ne,pe)),8)
        te[is.na(te)]=0
        te=te==0
        if(any(te)) ne[te]=pe[te]=NA
        arrows(p,ne,p,pe,lwd=error.lwd,col=error.color,angle=90,code=3,length=.05)
      }
      success=TRUE
    }else if(ck$t==2){
      #density
      m=list()
      dl=if(cl<-class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      rn=if(is.data.frame(cdat[[i]])) ptxt$l.by else names(cdat[[i]])
      dx=dy=numeric(512*seg$by$ll)
      for(l in seq_len(dl)) tryCatch({
        m[[l]]=density((if(cl) cdat[[i]][[l]] else cdat[[i]])[,'y'],bw,adj)
        dx[1:512+512*(l-1)]=m[[l]]$x
        dy[1:512+512*(l-1)]=m[[l]]$y
      },error=function(e)NULL)
      if(seg$by$ll>1){
        lty=if(is.numeric(lty)) rep(lty,length(m)) else if(is.logical(lty) && lty) seq_along(m) else rep(1,length(m))
        plot(NA,xlim=if(missing(mxl)) c(min(dx),max(dx)) else mxl,ylim=if(missing(myl)) c(0,max(c(dy))*1.2) else myl,
          main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,xpd=if('xpd'%in%names(pdo)) pdo$xpd else FALSE)
        for(l in seq_along(m)) lines(m[[l]],col=colors[rn[l]],lwd=lwd,lty=lty[l])
        if(ck$leg==2) do.call(legend,c(list(if(ck$lp) ifelse(median(dx)<mean(range(dx)),'topleft','topright') else lpos,
          legend=ptxt$l.by[rn]),lega))
      }else{
        hist((if(cl) cdat[[i]][[1]] else cdat[[i]])[,'y'],border=if('border'%in%names(pdo)) pdo$border else par('bg'),
          freq=FALSE,main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,
          col=if(length(colors)>1) colors[2] else colors)
        if(!is.logical(lines) || lines) lines(m[[1]],col=colors[1],lwd=lwd,xpd=if('xpd'%in%names(pdo)) pdo$xpd else FALSE)
      }
      axis(1,las=xlas,cex=par('cex.axis'),fg=par('col.axis'))
      axis(2,las=ylas,cex=par('cex.axis'),fg=par('col.axis'))
      success=TRUE
    }else{
      #scatter
      dl=if(cl<-class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      rn=if(is.data.frame(cdat[[i]])) seg$by$l else names(cdat[[i]])
      td=if(cl) do.call(rbind,cdat[[i]]) else cdat[[i]]
      cx=td[,'x']
      cy=td[,'y']
      xch=if(is.numeric(cx)) cx else as.numeric(factor(cx))
      a2a=list(cex=par('cex.axis'),fg=par('col.axis'))
      if(length(ptxt$l.x)!=0){
        a2a$at=seq_along(ptxt$l.x)
        a2a$labels=ptxt$l.x
        if(missing(xlas) || xlas>1){
          xlas=3
          par(mai=c(min(c(par('fin')[2]/2,max(strwidth(ptxt$l.x,'i'))))+.1,par('mai')[-1]))
        }
      }
      plot(NA,xlim=if(missing(mxl)) range(xch,na.rm=TRUE) else mxl,ylim=if(missing(myl))
        c(min(cy),max(cy)+max(cy)*ifelse(ck$leg==1 && seg$by$ll<lim,seg$by$ll/20,0)) else myl,
        main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE)
      do.call(axis,c(list(2,las=ylas),a2a[c('cex','fg')]))
      do.call(axis,c(list(1,las=xlas),a2a))
      if(ck$leg>1){
        up=xch[cy>=quantile(cy)[4]]
        mr=quantile(xch)
        do.call(legend,c(list(if(ck$lp) ifelse(sum(up<mr[2])>sum(up>mr[4]),'topright','topleft') else lpos
          ,legend=if(ck$cb) ptxt$cb else ptxt$l.by[rn]),lega))
      }
      for(l in seq_len(dl)){
        td=if(cl) cdat[[i]][[rn[l]]] else cdat[[i]]
        if(is.null(td)) next
        x=td[,'x']
        y=td[,'y']
        col=if((seg$by$ll==1 && ck$cb) || is.null(names(colors))) colors else colors[rn[l]]
        coll=if(length(col)>1 && seg$by$ll==1) '#666666' else col
        if(points && points.first) points(x,y,col=col)
        lines=substitute(lines)
        if(if(is.logical(lines)) lines else !grepl('^F',lines)){
          lines=if(is.logical(lines) || ck$c || grepl('^li|^lm|^st',lines,TRUE)) 'li' else
            if(grepl('^lo|^p|^cu',lines,TRUE)) 'lo' else if(grepl('^sm|^sp|^in',lines,TRUE)) 'sm' else
              if(grepl('^e|^co|^d',lines,TRUE)) 'e' else 'li'
          fit=tryCatch({
            if(ck$c) lm(y~x+as.matrix(td[,cvar,drop=FALSE]))$fitted else
              if(lines=='e') y else predict(switch(lines,li=lm,lo=loess,sm=smooth.spline)(y~x))
          },error=function(e){warning('error estimating line: ',e$message,call.=FALSE);NULL})
          if(!is.null(fit)){
            if(lines=='sm') {xo=fit$x; fit=fit$y} else {or=order(x); xo=x[or]; fit=fit[or]}
            lines(xo,fit,col=coll,lty=if(ck$lty && lty) l else if(!missing(lty) && !ck$lty)
              ifelse(length(lty)>1,lty[l],l) else 1,lwd=lwd)
          }
        }
        if(points && !points.first) points(x,y,col=col)
      }
      success=TRUE
    }
    if(!missing(add)) tryCatch(eval(substitute(add),envir=cbind(dat,odat))
      ,error=function(e)warning('error from add: ',e$message,call.=FALSE))
  },error=function(e){dev.off();stop(e)})}
  if(!success) stop("failed to make any plots with the current input",call.=FALSE)
  if(ck$t==3 && !is.character(note) && is.character(lines) && (!is.logical(note) || note))
    note=paste0(if(is.logical(note)) '' else note, paste0('Line type: ',switch(lines,li='lm',
      lo='loess',sm='spline',e='connected'),'.'))
  if(ck$leg==1){
    if(all(par('mfg')[1:2]!=0)){
      plot.new()
      if(ck$b) lega[c('pch','pt.cex','x.intersp','y.intersp','adj')]=list(15,2,1,1.2,c(0,.35))
      do.call(legend,c(list(lpos,legend=if(ck$cb) ptxt$cb else ptxt$l.by),lega))
    }else warning('legend positioning failed',call.=FALSE)
  }
  if(sud) mtext(if(ck$sud) gsub(', (?=[A-z0-9 ]+$)',ifelse(length(ptxt$cov)>2,', & ',' & '),
    gsub('^ | $','',paste0(if(ck$su) paste('Subset:',paste0(txt$su[1],if(length(txt$su)!=1)'...')),if(ck$su && ck$c)', ',
      if(ck$c) paste(if(ck$t==1)'Covariates:' else 'Line adjustment:',paste(ptxt$cov,collapse=', ')))),TRUE,TRUE) else
        '',3,0,TRUE,cex=cex['sud'],font=font['sud'])
  mtext(main,3,if(ck$sud) 1.5 else .5,TRUE,cex=cex['title'],font=font['title'])
  mtext(if(ck$t==2) 'Density' else ylab,2,-.2,TRUE,cex=par('cex.lab'),font=par('font.lab'))
  mtext(if(ck$t==2) ylab else xlab,1,0,TRUE,cex=par('cex.lab'),font=par('font.lab'))
  if(is.character(note)) mtext(note,1,ck$lx,TRUE,adj=if(ck$ly) 0 else .01,font=font['note'],cex=cex['note'])
  par(dop)
  if(save || (missing(save) && any(!missing(format),!missing(file.name),!missing(dims)))) tryCatch({
    t=substitute(format)
    tt=if(any(grepl('cairo',t))){paste0('.',strsplit(deparse(t),'_')[[1]][2])
    }else if(t=='postscript') '.ps' else paste0('.',t)
    if(grepl('jpeg|png|tiff|bmp|bit',t) && missing(dims)) dims=dev.size(units='px')
    fn=paste0(if(main=='' || !missing(file.name)) file.name else gsub(' ','_',gsub('^ +| +$|  ','',main),fixed=TRUE),tt)
    dev.copy(format,fn,width=dims[1],height=dims[2])
    dev.off()
    message('image saved: ',getwd(),'/',fn)
  },error=function(e)warning('unable to save image: ',e$message,call.=FALSE))
  invisible(list(data=dat,cdat=cdat,txt=txt,ptxt=ptxt,seg=seg,ck=ck,model=fmod))
}