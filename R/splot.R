#' Split Plot
#'
#' A plotting function aimed at automating some common visualization tasks in order to ease data exploration.
#' @param y required. Primary variable, to be shown on the y axis unless \code{x} is not specified. Can be an object,
#'   name of a column in data, or a formula (see note bellow). Multiple variables can also be included as columns in a matrix or data frame.
#' @param x secondary variable, to be shown in on the x axis. If not specified, \code{type} will be set to \code{'density'}.
#'   If \code{x} is a factor or vector of characters, or has fewer than \code{lim} levels when treated as a factor, \code{type}
#'   will be set to \code{"line"} unless specified.
#' @param by the 'splitting' variable within each plot, by which the plotted values of \code{x} and \code{y} will be grouped.
#' @param between A single object or name, or two in a vector (e.g., \code{c(b1, b2)}), the levels of which will determine the
#'   number of plot windows to be shown at once (the cells in a matrix of plots; levels of the first variable as rows, and
#'   levels of the second as columns).
#' @param cov additional variables used for adjustment. If this is not missing, and \code{type} is not specified, \code{type} will
#'   default to \code{"line"}. Bar and line plots include all \code{cov} variables in their no-intercept regression models (via
#'   \code{\link[stats]{lm}}, e.g., \code{lm(y ~ 0 + x + cov1 + cov2)}) as covariates. If any \code{cov} variable matches \code{x},
#'   \code{line} is not \code{FALSE}, and \code{loess} is not \code{TRUE}, these will be included in the regression model to adjust the prediction
#'   line in a scatter plot (e.g., \code{lm(y ~ x + x^2)}).
#' @param type determines the type of plot to make, between \code{"bar"}, \code{"line"}, \code{"density"}, or \code{"scatter"}. If
#'   \code{"density"}, \code{x} is ignored. Anything including the first letter of each is accepted (e.g., \code{type='l'}).
#' @param split how to split any continuous variables (those with more than \code{lim} levels as factors). Default is \code{"median"},
#'   with \code{"mean"}, \code{"standard deviation"}, and \code{"quantile"} as options.
#' @param data a \code{data.frame} to pull variables from. If variables aren't found in \code{data}, they will be looked for in the environment.
#' @param su a subset to all variables, applied after they are all retrieved from \code{data} or the environment.
#' @param levels a list with entries corresponding to variable names, used to rename and/or reorder factor levels. To reorder a factor, enter a
#'   vector of either numbers or existing level names in the new order (e.g., \code{levels=list(var=c(3,2,1))}). To rename levels of a factor,
#'   enter a character vector the same length as the number of levels. To rename and reorder, enter a list, with names as the first entry, and
#'   order as the second entry (e.g., \code{levels=list(var=list(c('a','b','c'),c(3,2,1)))}). This happens after variables are split, so
#'   names and orders should correspond to the new split levels of split variables. For example, if a continuous variable is median split, it now
#'   has two levels ('Under Median' and 'Over Median'), which are the levels reordering or renaming would apply to.
#' @param error determines whether and which type of error bars to show in bar or line plots. If \code{FALSE}, no error bars will be shown.
#'   Otherwise, the default is \code{"standard error"}, with \code{"confidence intervals"} as an option.
#' @param error.color color of the error bars. Default is \code{'#585858'}.
#' @param model logical: if \code{TRUE}, the summary of an interaction model will be printed.
#' @param loess logical: if \code{TRUE}, \code{\link[stats]{loess}} lines are drawn instead of regression lines.
#' @param mv.scale determines whether to center and scale multiple \code{y} variables. Does not center or scale by default. Anything other than
#'   \code{'none'} will mean center each numeric \code{y} variable. Anything matching \code{'^t|z|sc'} will also scale.
#' @param mv.as.x logical: if \code{TRUE}, variable names are displayed on the x axis, and \code{x} is treated as \code{by}.
#' @param save logical: if \code{TRUE}, an image of the plot is saved to the current working directory.
#' @param format the type of file to save plots as. default is \code{\link[grDevices]{cairo_pdf}}. See \code{\link[grDevices]{Devices}} for options.
#' @param dims a vector of 2 values (\code{c(width, height)}) specifying the dimensions of a plot to save in inches or pixels depending on
#'   \code{format}. Defaults to the dimensions of the plot window.
#' @param file.name a string with the name of the file to be save (excluding the extension, as this is added depending on \code{format}).
#' @param lim numeric. Checked against the number of factor levels of each variable. Used to decide which variables should be split, which colors
#'   to use, and when to turn off the legend. Default is \code{9}. If set over \code{20}, \code{lim} is treated as infinite (set to \code{Inf}).
#' @param colors sets a color theme or manually specifies colors. Default theme is \code{"pastel"}, with \code{"dark"} and \code{"bright"} as
#'   options. If set to \code{"grey"}, or if \code{by} has more than 9 levels, a grey scale is calculated using \code{\link[grDevices]{grey}}.
#'   See the \code{col} parameter in \code{\link[graphics]{par}} for acceptable manual inputs.
#' @param myl sets the range of the y axis (\code{ylim} of \code{\link[graphics]{plot}} or \code{\link[graphics]{barplot}}). If not specified,
#'   this will be calculated from the data.
#' @param mxl sets the range of the x axis (\code{xlim} of \code{\link[graphics]{plot}}). If not specified, this will be calculated from the data.
#' @param autori logical: if \code{FALSE}, the origin of plotted bars will be set to 0. Otherwise, bars are adjusted such that they extend to the
#'   bottom of the y axis.
#' @param xlas sets the orientation of the x-axis labels. See \code{\link[graphics]{par}}.
#' @param ylas sets the orientation of the y-axis labels. See \code{\link[graphics]{par}}.
#' @param lwd sets the width of lines in scatter plots. Default is 2. See \code{\link[graphics]{par}}.
#' @param pch sets the type of dot to use when plotting points in a scatter plot. Default is 20. See \code{\link[graphics]{par}}.
#' @param bw sets the smoothing bandwidth when plotting densities. Default is \code{'nrd0'}. See \code{\link[stats]{density}}.
#' @param adj adjusts the smoothing of densities (\code{adj * bw}). See \code{\link[stats]{density}}.
#' @param lpos set the position of the legend. By default, it will try to choose between the top left and right areas. See \code{\link{legend}}.
#' @param lvn legend variable name. Logical: if \code{FALSE}, the names of by and between variables will not be shown before their level (e.g.,
#'   for a sex variable with a "female" level, "sex: female" would become "female" in the legend or above each plot window).
#' @param title logical or a character: if \code{FALSE}, the main title is turned off. If a character, this will be shown as the main title.
#' @param labx logical or a character: if \code{FALSE}, the label on the x axis is turned off. If a character, this will be shown as the
#'   x axis label.
#' @param laby logical or a character: if \code{FALSE}, the label on the y axis is turned off. If a character, this will be shown as the
#'   y axis label.
#' @param lty logical or a vector: if \code{FALSE}, lines are always solid. If a vector, changes line type based on each value. Otherwise loops
#'   through available line types, see \code{\link[graphics]{par}}.
#' @param lhz logical: if \code{FALSE}, the legend grows horizontally.
#' @param sub logical: if \code{FALSE}, the small title above each plot showing \code{between} levels is turned off.
#' @param leg logical: if \code{FALSE}, the legend is turned off.
#' @param note logical: if \code{FALSE}, the note at the bottom about splits and/or error bars is turned off.
#' @param sud logical: if \code{FALSE}, the heading for subset and covariates/line adjustments is turned off.
#' @param ndisp logical: if \code{FALSE}, n per level is no longer displayed in the subheadings.
#' @param labels logical: if \code{FALSE}, sets all settable text surrounding the plot to \code{FALSE} (just so you don't have to set all
#'   of them if you want a clean frame).
#' @param points logical: if \code{FALSE}, the points in a scatter plot are no longer drawn.
#' @param lines logical: if \code{FALSE}, the prediction lines in a scatter plot are no longer drawn.
#' @param byx logical: if \code{TRUE} (default) and \code{by} is specified, regressions for bar or line plots compare levels of \code{by} for each
#'   level of \code{x}. This makes for more intuitive error bars when comparing levels of \code{by} within a level of \code{x}.
#' @param drop logical: if \code{FALSE}, \code{x} levels with no data are still displayed in bar or line plots.
#' @param mar sets the margins of each plot window. Partially set automatically if not specified: \code{c(if(labx)2.5 else 0,if(laby)3 else 2,1,0)}.
#'   If \code{xlas} is not specified, or is greater than 2, and x-axis labels are overly long in bar or line plots,
#'   \code{mar[1]} is set by the x-axis text length (\code{strwidth(max(colnames(m)))*ifelse(labx,5.5,4.8)}). See \code{\link[graphics]{par}}.
#' @param add evaluated within the function. Useful for adding things like lines to a plot while the parameters are still those set by the
#'  function (e.g., \code{add=\{lines(1:10)\}}).
#' @param ... passes additional arguments to \code{\link[graphics]{plot}} or \code{\link[graphics]{barplot}}.
#'
#' @note
#' When \code{y} is a formula, other variables will be pulled from it:
#'
#' \code{y ~ x * by * between[1] * between[2] + cov[1] + cov[2] + cov[n]}
#'
#' @examples
#' #simulating data
#' n=2000
#' dat=data.frame(sapply(c('by','bet1','bet2'),function(c)sample(0:1,n,TRUE)))
#' dat$x=eval(quote(
#'   rnorm(n)+by*-.4+by*bet1*-.3+by*bet2*.3+bet1*bet2*.9-.8+rnorm(n,0,by)
#' ),envir=dat)
#' dat$y=eval(quote(
#'   x*.2+by*.3+bet2*-.6+bet1*bet2*.8+x*by*bet1*-.5+x*by*bet1*bet2*-.5
#'   +rnorm(n,5)+rnorm(n,-1,.1*x^2)
#' ),envir=dat)
#'
#' #looking at the distribution of y between bets split by by
#' splot(y, by=by, between=c(bet1, bet2), data=dat)
#'
#' #looking at quantile splits of y in y by x
#' splot(y~x*y, split='quantile', data=dat)
#'
#' #looking at y by x between bets
#' splot(y~x, between=c(bet1, bet2), data=dat)
#'
#' #sequentially adding levels of split
#' splot(y~x*by, data=dat)
#' splot(y~x*by*bet1, data=dat)
#' splot(y~x*by*bet1*bet2, data=dat)
#'
#' #same as the last but entered differently
#' splot(y, x, by, c(bet1, bet2), data=dat)
#'
#' #zooming in on one of the windows
#' splot(y~x*by, data=dat, su=bet1==1&bet2==0)
#'
#' #compairing an adjusted lm prediction line with a loess line
#' splot(y~x+x^2, data=dat, su=bet1==1&bet2==0&by==1, add={
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
#' )~x, data=dat, myl=c(-10,15), loess=TRUE, lpos='topright')
#'
#' @export
#' @importFrom grDevices grey dev.copy dev.size dev.off cairo_pdf
#' @importFrom graphics axis axTicks hist legend lines mtext plot barplot par points arrows strwidth
#' @importFrom stats density median quantile sd lm confint update loess na.omit as.formula
splot=function(y,x=NULL,by=NULL,between=NULL,cov=NULL,type='',split='median',data=NULL,su=NULL,levels=list(),error='standard',
  error.color='#585858',lim=9,model=FALSE,loess=FALSE,mv.scale='none',mv.as.x=FALSE,save=FALSE,format=cairo_pdf,dims=dev.size(),
  file.name='splot',colors=NULL,myl=NULL,mxl=NULL,autori=TRUE,xlas=0,ylas=1,lwd=2,pch=20,bw='nrd0',adj=2,lpos='auto',lvn=TRUE,
  title=TRUE,labx=TRUE,laby=TRUE,lty=TRUE,lhz=FALSE,sub=TRUE,ndisp=TRUE,leg=TRUE,note=TRUE,sud=TRUE,labels=TRUE,points=TRUE,
  lines=TRUE,byx=TRUE,drop=TRUE,mar='auto',add=NULL,...){
  #parsing input and preparing data
  if(!labels) title=sud=sub=labx=laby=note=FALSE
  ck=list(
    t=if(grepl('^b|^l',type,TRUE)) 1 else if(grepl('^d',type,TRUE)) 2 else 3,
    tt=!missing(type) && if(grepl('^b|^l',type,TRUE)) FALSE else TRUE,
    d=!missing(data) && !is.null(data),
    su=!missing(su),
    c=!missing(cov),
    e=grepl('^s',error,TRUE),
    el=!(is.logical(error) && !error),
    sp=if(grepl('^mea|^av',split,TRUE)) 1 else if(grepl('^q',split,TRUE)) 2 else ifelse(grepl('^s',split,TRUE),3,4),
    ly=!(is.logical(laby) && !laby) || is.character(laby),
    lx=!(is.logical(labx) && !labx) || is.character(labx),
    lty=is.logical(lty),
    mod=!missing(x) && model,
    mv=FALSE,
    mlvn=missing(lvn)
  )
  dn=if(ck$d) names(data) else ''
  if(tryCatch(any(grepl('~',y)),error=function(e)FALSE)){
    f=as.character(as.formula(y))[-1]
    y=f[1]
    f=strsplit(f[-1],' \\+ ')[[1]]
    if(any(grepl('\\*',f))){
      r=strsplit(f[1],' \\* ')[[1]]
      if(length(r)>0) x=r[1]
      if(length(r)>1) by=r[2]
      if(length(r)>2) between=r[3]
      if(length(r)>3) between=c(r[3],r[4])
      f=f[!grepl('\\*',f)]
    }else{
      x=f[1]
      f=f[-1]
    }
    if(length(f)>0){
      cov=f
      ck$c=TRUE
    }
  }
  txt=list(
    split='none',
    y=gsub('\\\"','',deparse(substitute(y))),
    x=gsub('\\\"','',deparse(substitute(x))),
    by=gsub('\\\"','',deparse(substitute(by))),
    between=as.list(substitute(between)),
    cov=as.list(substitute(cov)),
    su=deparse(substitute(su))
  )
  txt$between=paste(if(txt$between[1]%in%c('c','list')) txt$between[-1] else txt$between)
  txt$cov=paste(if(txt$cov[1]%in%c('c','list')) txt$cov[-1] else txt$cov)
  ptxt=lapply(txt,function(i) gsub('_|\\.',' ',i))
  if(is.character(labx)) ptxt$x=labx
  if(is.character(laby)) ptxt$y=laby
  if(ck$t!=1 && ck$c){
    if(!loess && lines && any(grepl(txt$x,txt$cov))){
      txt$cov=txt$cov[grepl(txt$x,txt$cov)]
    }else if(ck$tt){
      txt$cov='NULL'
      ck$c=FALSE
      message('covariates will only be included in bar or line plots')
    }else ck$t=1
  }
  tdc=function(x,data){
    s=TRUE
    if(!is.null(data)){
      o=tryCatch(eval(parse(text=x),envir=data),error=function(e){warning(e);s<<-FALSE})
      if(!s || length(o)<2){
        s=TRUE
        if(length(o)<2)
          o=tryCatch(eval(parse(text=eval(parse(text=x))),envir=data),error=function(e){warning(e);s<<-FALSE})
        if(!s || length(o)<2){
          s=TRUE
          o=tryCatch(eval(parse(text=x)),error=function(e){warning(e);s<<-FALSE})
        }
      }
    }else o=tryCatch(eval(parse(text=x),envir=globalenv()),error=function(e){warning(e);s<<-FALSE})
    if(!s || length(o)<2){stop(paste('Could not find',x),call.=FALSE)}
    o
  }
  if(length(txt$between)>2) txt$between=txt$between[1:2]
  dat=data.frame(y=tdc(txt$y,data))
  if(!missing(x)) dat$x=tdc(txt$x,data)
  if(!missing(by)) dat$by=tdc(txt$by,data)
  if(!missing(between)) for(i in txt$between) dat=cbind(dat,bet=tdc(i,data))
  if(ck$c) for(i in txt$cov) dat=cbind(dat,cov=tdc(i,data))
  if(ck$su) dat=if(ck$d) dat[eval(substitute(su),data),] else dat[su,]
  rm(data)
  dat=na.omit(dat)
  if(nrow(dat)==0) stop('this combination of variables/splits has no complete cases',call.=FALSE)
  dn=names(dat)
  if(sum(grepl('^y',dn))>1){
    #setting up multiple y variables
    ck$mv=TRUE
    if(ck$mlvn) lvn=FALSE
    if(!missing(by)) message('by is ignored when y has multiple variables')
    if(ck$t==2 && ck$lx && is.character(labx)) ptxt$y=labx
    dn=grep('y\\.',dn)
    ck$mvn=colnames(dat)[dn]
    td=dat
    by=rep(sub('^y\\.','',ck$mvn),each=nrow(dat))
    dat=data.frame(y=unlist(dat[,dn],use.names=FALSE))
    if(ncol(td)>length(dn)) dat=cbind(dat,apply(td[,-dn,drop=FALSE],2,function(c)rep.int(c,length(dn))))
    if(mv.as.x){
      txt$by=txt$x
      ptxt$by=ptxt$x
      if(missing(labx)) txt$x=ptxt$x='variable'
      dat$by=dat$x
      dat$x=by
    }else{
      txt$by=ptxt$by='variable'
      dat$by=by
    }
    dn=colnames(dat)
    for(c in seq_along(dn)) dat[,c]=ifelse(any(grepl('[A-z]',dat[,c])),as.factor,as.numeric)(matrix(dat[,c]))
    if(!missing(mv.scale) && mv.scale!='none'){
      tv=if(mv.as.x) dat$x else dat$by
      for(g in levels(as.factor(tv))) dat[tv==g,1]=scale(dat[tv==g,1],scale=grepl('^t|z|sc',mv.scale,TRUE))
    }
  }
  if(!'x'%in%dn){
    ck$t=2
    if(!missing(type) && !grepl('^d',type,TRUE)) message('x must be included to show other types of splots')
  }
  if(!'by'%in%dn) leg=FALSE
  if(lim>20 || (is.logical(lim) && !lim)){
    lim=Inf
    leg=FALSE
    if(missing(error)) ck$el=FALSE
  }
  odat=dat
  #splitting and parsing variables
  splt=function(x,s){
    d=numeric(length(x))
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
  if(!missing(x) && ck$t!=2) if((ck$t==1 || is.character(dat$x) || is.factor(dat$x)|| (missing(type)
    && nlevels(factor(dat$x))<lim)) && !(is.factor(dat$y) || is.character(dat$y) || nlevels(as.factor(dat$y))<9)){
    if(!is.character(dat$x) && !is.factor(dat$x) && nlevels(as.factor(dat$x))>lim){
      dat$x=splt(dat$x,ck$sp)
      seg$x$s=TRUE
    }else dat$x=as.factor(dat$x)
    if(missing(type)) ck$t=1
  }
  svar=NULL
  cvar=if(any(grepl('^c',dn))) which(grepl('^c',dn)) else NULL
  if(any(grepl('^b',dn))){
    svar=which(grepl('^b',dn))
    for(i in svar){
      e=if(dn[i]=='bet') if(!seg$f1$e) 'f1' else 'f2' else 'by'
      seg[[e]]$e=TRUE
      seg[[e]]$i=i
      seg[[e]]$l=sort(unique(dat[,i]))
      seg[[e]]$ll=length(seg[[e]]$l)
      if(seg[[e]]$ll>lim && !(is.character(dat[,i]) || is.factor(dat[,i]))){
        dat[,i]=splt(dat[,i],ck$sp)
        seg[[e]]$s=TRUE
        seg[[e]]$l=sort(unique(dat[,i]))
        seg[[e]]$ll=length(seg[[e]]$l)
      }
    }
  }
  fmod=NULL
  if(ck$t!=2 && model) tryCatch({
    mod=paste0(txt$y,'~',txt$x,
      if(seg$by$e) paste0('*',txt$by),
      if(seg$f1$e) paste0('*',txt$between[1]),
      if(seg$f2$e) paste0('*',txt$between[2]),
      if(length(cvar)>0) paste0('+',paste0(txt$cov,collapse='+'))
    )
    colnames(odat)=strsplit(mod,'~|\\*|\\+')[[1]]
    fmod=lm(mod,data=odat)
    if(model){
      s=summary(fmod)
      s$call=paste0('lm(',mod,')')
      print(s)
    }
  },error=function(e)warning(paste('summary model failed:',e$message),call.=FALSE))
  if(!missing(levels)) tryCatch({
    lc=c('x','by','f1','f2')
    ns=c(txt$x,txt$by,txt$between[1],txt$between[2])
    for(n in names(levels)){
      if(any(ns%in%n)){
        sl=seg[[lc[which(ns%in%n)]]]
        vfac=levels(as.factor(dat[,sl$i]))
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
          if('l'%in%names(sl)) seg[[lc[which(ns%in%n)]]]$l=vl$labels
        }else warning(n,' has ',vl,' levels and you only provided ',length(ln),call.=FALSE)
      }
    }
  },error=function(e)warning('setting levels failed: ',e$message,call.=FALSE))
  dsf=list(c1='',sep=rep('^^',nrow(dat)),c2='')
  if(seg$f1$e) dsf$c1=dat[,seg$f1$i]
  if(seg$f2$e) dsf$c2=dat[,seg$f2$i]
  dsf=do.call(paste0,dsf)
  cdat=split(dat,dsf)
  if(seg$by$e) cdat=lapply(cdat,function(s)split(s,s$by))
  seg[['l']]=list(by=levels(as.factor(dat$by)),l=unique(dsf))

  if(ck$mlvn && length(seg$l$by)>0 && (seg$by$s || !any(grepl('^[0-9]',seg$l$by)))) lvn=FALSE
  if(length(names(cdat))>1 && ndisp && seg$f1$e){
    sf=paste0(dat[,seg$f1$i],'^^',if(seg$f2$e)dat[,seg$f2$i])
    seg$n=sapply(split(dat,sf),nrow)
    if(ck$mv) seg$n=seg$n/length(ck$mvn)
  }
  #figuring out parts of the plot
  if(missing(colors) || (!missing(colors) && grepl('^gr|past|prim|bright|dark',colors[1],TRUE))){
    colors=if((missing(colors) && seg$by$ll>1 && seg$by$ll<9) || (!missing(colors) && !grepl('^gr',colors[1],TRUE))){
      if(!missing(colors) && grepl('prim|bright',colors[1],TRUE)){ c('#45ff00','#ba00ff','#000000','#ff0000','#fffd00','#003dff','#00f2f8','#999999','#ff891b')
      }else if(!missing(colors) && grepl('dark',colors[1],TRUE)){ c('#1b8621','#681686','#2a2a2a','#7c0d0d','#b5bc00','#241c80','#1a7e8b','#666666','#b06622')
      }else c('#82c473','#a378c0','#616161','#9f5c61','#d3d280','#6970b2','#78c4c2','#454744','#d98c82')
    }else grey(.2:seg$by$ll/(seg$by$ll+seg$by$ll*ifelse(seg$by$ll<10,.1,.3)))
  }
  ylab=if(ck$ly) ptxt$y else ''
  xlab=if(ck$lx) ptxt$x else ''
  main=if(is.logical(title) && title) paste(if(ck$t==2)paste('Density of',ptxt$y) else paste(ptxt$y,
    'by',ptxt$x),if(seg$by$e && !ck$mv) paste('at levels of',ptxt$by)) else if (is.character(title)) title else ''
  if(is.logical(note)) if(note){
    if(txt$split!='none' || (ck$t==1 && ck$el)){
      tv=c(if(seg$x$s) ptxt$x else '',if(seg$by$s) ptxt$by else '',if(seg$f1$s) ptxt$between[1] else '',if(seg$f2$s) ptxt$between[2] else '')
      tv=tv[tv!='']
      tv=gsub(', (?=[a-z0-9]+$)',ifelse(length(tv)>2,', & ',' & '),paste(tv,collapse=', '),TRUE,TRUE)
      note=paste0(
        if(txt$split!='none') paste0(tv,' split by ',txt$split,'. '),
        if(ck$t==1 && ck$el) paste('Error bars show',ifelse(ck$e,'standard error.','95% confidence intervals.'))
      )
    }
  }else note=''
  success=FALSE
  op=par(
    mfrow=c(seg$f1$ll,seg$f2$ll),
    oma=c(if(note=='')1 else 2,if(ck$ly)1 else 0,if(main=='') ifelse(ck$su || ck$c,2.5,0) else 4,0) ,
    mar=if(missing(mar)) c(if(ck$lx)2.5 else 1,if(ck$ly)3 else 2,1,0) else mar,
    mgp=c(3,.3,0),
    font.main=1,
    cex.main=1,
    font.lab=4,
    tcl=-.2,
    xpd=if(ck$t==2) FALSE else NA
  )
  for(i in names(cdat)){tryCatch({
    #plotting
    cl=(if(class(cdat[[i]])=='list') lapply(cdat[[i]],nrow) else nrow(cdat[[i]]))>1
    if(any(!cl)){
      cdat[[i]]=cdat[[i]][cl]
      if(length(cdat[[i]])==0) next
    }
    cl=strsplit(i,'^^',fixed=TRUE)[[1]]
    ptxt$sub=if(sub) if(length(seg$l$l)>1) paste0(
      if(seg$f1$e) paste0(
        if(lvn || (ck$mlvn && grepl('^[0-9]',cl[1]))) paste0(ptxt$between[1],': '),cl[1],
        if(seg$f2$e) paste0(', ',if(lvn || (ck$mlvn && grepl('^[0-9]',cl[2]))) paste0(ptxt$between[2],': '),cl[2])
      ),if(length(names(cdat))>1 && ndisp) paste(', n =',seg$n[[i]])
    ) else if(is.character(sub)) sub else ''
    if(ck$t==1){
      #bar and line
      flipped=FALSE
      if(byx && seg$by$e && lim<Inf){
        flipped=TRUE
        cdat[[i]]=do.call(rbind,cdat[[i]])
        cdat[[i]][c('x','by')]=cdat[[i]][c('by','x')]
        cdat[[i]]$x=as.factor(cdat[[i]]$x)
        cdat[[i]]=split(cdat[[i]],cdat[[i]]$by)
      }
      seg$x$l=levels(as.factor(dat$x))
      seg$x$ll=length(seg$x$l)
      dl=if(class(cdat[[1]])=='list') length(cdat[[i]]) else 1
      mot=paste0('y~0+',paste(names(if(dl==1) cdat[[i]] else cdat[[i]][[1]])[c(2,cvar)],collapse='+'))
      rn=gsub('_|\\.',' ',paste(if(lvn)paste0(ptxt$by,':'),seg$by$l))
      rn=gsub('MINUS',' - ',rn)
      m=pe=ne=matrix(NA,length(rn),max(c(1,seg$x$ll)),dimnames=list(rn,seg$x$l))
      if(flipped) m=pe=ne=t(m)
      rn=rownames(m)
      cn=if(seg$by$e && flipped) seg$by$l else colnames(m)
      for(l in seq_len(dl)){
        td=if(dl==1) cdat[[i]] else cdat[[i]][[l]]
        if(nrow(td)<2) next
        ri=rn[l]
        mo=lm(mot,data=td)
        su=which(cn%in%sub('x','',names(mo$coef)))
        sus=seq_along(su)
        m[ri,su]=mo$coef[sus]
        if(nrow(td)>2){
          if(ck$e){
            e=summary(update(mo,~.-0))$coef[sus,2]
            e=e[c(2,seq_along(e)[-1])]
            pe[ri,su]=m[l,su]+e
            ne[ri,su]=m[l,su]-e
          }else{
            e=confint(mo)[sus,]
            pe[ri,su]=e[,2]
            ne[ri,su]=e[,1]
          }
        }
      }
      re=if(flipped) list(m=t(m),ne=t(ne),pe=t(pe)) else list(m=m,ne=ne,pe=pe)
      if(drop){
        mm=apply(re$m,2,function(c)!all(is.na(c)))
        re=lapply(re,function(s)s[,mm,drop=FALSE])
      }
      m=re$m
      ne=re$ne
      pe=re$pe
      re=lapply(re,function(s)s[!is.na(m)])
      if(length(re$m)==0) next
      if(all(is.na(re$ne),is.na(re$pe))) re$pe=re$ne=re$m
      lb=min(re$m)-max(abs(re$m-re$ne))*1.2
      r=nrow(m)
      ylim=if(missing(myl))
        c(lb,max(re$m)+max(abs(re$m-re$pe))*if(leg && seg$by$ll>2)seg$by$ll else 2.2) else myl
      lpos=ifelse(lpos=='auto',ifelse(any(!is.na(m[1,])) && any(!is.na(m[r,])),
        ifelse(max(m[1,!is.na(m[1,])])>max(m[r,!is.na(m[r,])]),'topright','topleft'),'topright'),lpos)
      if(any(is.na(ylim))) next
      oyl=axTicks(2,axp=c(ylim[1],ylim[2],par('yaxp')[3]))
      stw=strwidth(colnames(m),units='inch')
      if(missing(mar) && (missing(xlas) || xlas>1) && sum(stw)>dev.size()[1]-1.5){
        xlas=3
        par(mar=c(max(stw)*ifelse(labx,5.5,4.8),par('mar')[-1]))
      }
      if(grepl('^b',type,TRUE)){
        if(autori && lb<0){
          a=abs(lb)
          m=m+a
          ne=ne+a
          pe=pe+a
          ayl=oyl+a
          aj=lapply(re,function(r)r+a)
          ylim=if(missing(myl)) c(
            min(aj$m)-max(abs(aj$m-aj$ne))*1.2,
            max(aj$m)+max(abs(aj$m-aj$pe))*if(leg && seg$by$ll>2) seg$by$ll else 2.2
          ) else myl+a
        }
        p=barplot(m,beside=TRUE,legend.text=leg,col=colors[seq_len(r)],axes=FALSE,axisnames=FALSE,border=NA,ylab=NA,xlab=NA,
          ylim=ylim,xpd=FALSE,main=if(sub) ptxt$sub else NA,args.legend=list(x=lpos,horiz=lhz,xpd=NA,bty='n'),...)
      }else{
        c=ncol(m)
        p=matrix(rep(seq_len(c),r),nrow=r,byrow=TRUE)
        plot(NA,ylim=ylim,xlim=if(missing(mxl)) c(1-c*.1,c+c*.1) else mxl,ylab=NA,xlab=NA,main=if(sub) ptxt$sub else NA,axes=FALSE,...)
        for(a in seq_len(r)) lines(m[a,],lwd=lwd,col=colors[a],lty=if(ck$lty && lty)a else if(!missing(lty) && !ck$lty) lty else 1)
        if(leg) legend(lpos,rownames(m),col=colors,lty=c(seq_len(r)),lwd=lwd,horiz=lhz,bty='n')
      }
      axis(1,apply(p,2,mean),colnames(m),FALSE,las=xlas)
      if(grepl('^b',type,TRUE) && autori && lb<0) axis(2,las=ylas,at=ayl,labels=round(oyl,2)) else axis(2,las=ylas)
      if(ck$el && !identical(ne,pe)) suppressWarnings(arrows(p,ne,p,pe,lwd=2,col=error.color,angle=90,code=3,length=.05))
      success=TRUE
    }else if(ck$t==2){
      #density
      m=list()
      dl=if(class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      dx=dy=numeric(512*seg$by$ll)
      for(l in seq_len(dl)) tryCatch({
          m[[l]]=density((if(dl==1) cdat[[i]] else cdat[[i]][[l]])[,'y'],bw,adj)
          dx[1:512+512*(l-1)]=m[[l]]$x
          dy[1:512+512*(l-1)]=m[[l]]$y
      },error=function(e)NULL)
      if(seg$by$ll>1){
        plot(NA,xlim=if(missing(mxl)) c(min(dx),max(dx)) else mxl,ylim=if(missing(myl)) c(0,max(c(dy))*1.2) else myl,
          main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,...)
        for(l in seq_along(m)) lines(m[[l]],col=colors[l],lwd=lwd,lty=l)
        if(leg) legend(ifelse(lpos=='auto',ifelse(median(dx)<mean(range(dx)),'topright','topleft'),lpos),
          sub('_|\\.',' ',paste(if(lvn)paste0(ptxt$by,':'),seg$l$by)),col=colors,lty=if(ck$lty && lty) seq_len(seg$by$ll) else if(!missing(lty)
            && !ck$lty) lty else 1,lwd=lwd,bty='n',horiz=lhz)
      }else{
        hist(cdat[[i]][,'y'],freq=FALSE,main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,col='#DEDEDE',...)
        lines(m[[1]],lwd=lwd,col='#303030')
      }
      axis(1,las=xlas)
      axis(2,las=ylas)
      success=TRUE
    }else{
      #scatter
      dl=if(class(cdat[[i]])=='list') length(cdat[[i]]) else 1
      td=if(dl==1) cdat[[i]] else do.call(rbind,cdat[[i]])
      cx=td[,'x']
      cy=td[,'y']
      xch=if(is.numeric(cx)) cx else as.numeric(factor(cx))
      plot(NA,xlim=if(missing(mxl)) range(xch,na.rm=TRUE) else mxl,ylim=if(missing(myl))
        c(min(cy),max(cy)+max(cy)*ifelse(leg && seg$by$ll<lim,seg$by$ll/20,0)) else myl,
        main=if(sub) ptxt$sub else NA,ylab=NA,xlab=NA,axes=FALSE,...)
      axis(1,las=xlas)
      axis(2,las=ylas)
      if(leg){
        up=xch[cy>=quantile(cy)[4]]
        mr=quantile(xch)
        legend(ifelse(lpos=='auto',ifelse(sum(up<mr[2])>sum(up>mr[4]),'topright','topleft'),lpos),
          sub('_|\\.',' ',paste(if(lvn)paste0(ptxt$by,':'),seg$l$by)),col=colors,lty=if(ck$lty && lty)seq_len(seg$by$ll) else if(!missing(lty)
            && !ck$lty) lty else 1,lwd=lwd,bty='n',horiz=lhz)
      }
      for(l in seq_len(dl)){
        td=if(dl==1) cdat[[i]] else cdat[[i]][[l]]
        x=td[,'x']
        y=td[,'y']
        if(points) points(x,y,pch=pch,col=if(seg$by$ll==1 && colors[1]=='#2E2E2E') '#999999' else colors[l])
        if(lines){
          fit=if(ck$c) lm(y~x+td[,cvar])$fitted else ifelse(loess,stats::loess,lm)(y~x)$fitted
          lines(x[order(x)],fit[order(x)],col=colors[l],lwd=lwd,lty=if(ck$lty && lty)
            l else if(!missing(lty) && !ck$lty)ifelse(length(lty)>1,lty[l],l) else 1)
        }
      }
      success=TRUE
    }
    if(!missing(add)) tryCatch(eval(substitute(add),envir=odat),error=function(e)warning('error from add: ',e$message,call.=FALSE))
  },error=function(e){par(op);stop(e)})}
  if(!success){par(op);stop("failed to make any plots with the current input",call.=FALSE)}
  mtext(main,3,2,TRUE,font=2,cex=1.5,col='#303030')
  mtext(if(sud && (ck$su || ck$c)) gsub(', (?=[A-z0-9 ]+$)',ifelse(length(ptxt$cov)>2,', & ',' & '),
    gsub('^ | $','',paste0(if(ck$su) paste('Subset:',txt$su),if(ck$su && ck$c)', ',
      if(ck$c) paste(if(ck$t==1)'Covariates:' else 'Line adjustment:',paste(ptxt$cov,collapse=', ')))),TRUE,TRUE) else '',3,.5,TRUE,cex=.9)
  mtext(if(ck$t==2) 'Density' else ylab,2,-.2,TRUE,font=2,col='#303030')
  mtext(if(ck$t==2) ylab else xlab,1,0,TRUE,font=2,col='#303030')
  if(!is.logical(note)) mtext(note,1,1,TRUE,adj=0,font=3,cex=.7)
  par(op)
  if(save || (missing(save) && any(!missing(format),!missing(dims)))) tryCatch({
    t=substitute(format)
    tt=if(grepl('cairo',t)){paste0('.',strsplit(deparse(t),'_')[[1]][2])
    }else if(t=='postscript') '.ps' else paste0('.',t)
    if(grepl('jpeg|png|tiff|bmp|bit',t) && missing(dims)) dims=dev.size(units='px')
    fn=paste0(if(main=='' || !missing(file.name)) file.name else gsub(' ','_',gsub('^ +| +$|  ','',main)),tt)
    dev.copy(format,fn,width=dims[1],height=dims[2])
    dev.off()
    message('image saved: ',getwd(),'/',fn)
  },error=function(e)warning('unable to save image: ',e$message,call.=FALSE))
  invisible(list(data=dat,cdat=cdat,txt=txt,ptxt=ptxt,seg=seg,ck=ck,model=fmod))
}
