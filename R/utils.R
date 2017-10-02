#' splot colors
#'
#' Get a prespecified set of 9 colors, a greyscale, or a set of random, grouped colors.
#' @param set the name of a set (matching \code{'bright'}, \code{'dark'}, \code{'pastel'}, or
#'   \code{'gray'} or \code{'grey'}), or a vector of color names or hex codes. See the details section.
#' @param by a factor-like variable; this will be used to set \code{ns} via \code{\link[base]{table}}.
#' @param ns a single value, or vector of values specifying the number of colors to generate.
#' @param maxiter the maximum number of iterations allowed when colors are being randomly sampled.
#' @param flat logical; if \code{FALSE}, if \code{by} is specified, a matrix is returned with two columns
#'   (group and color), and if \code{by} is missing, a list is returned with seed colors as names. Otherwise,
#'   a vector with all generated colors is returned, either in order of generation (if \code{by} is missing),
#'   or in the same order as \code{by}.
#' @details
#' If only \code{set} is specified, either a prespecified vector of color codes is returned (if
#' \code{set} is a single string, and matches one of \code{'bright'}, \code{'dark'}, or \code{'pastel'}),
#' or a version of the entered vector, with color names converted to hex codes (e.g.,
#' \code{splot.color(c('red','green','blue'))} would return \code{c('#ff0000','#00ff00','#0000ff')}).
#'
#' If \code{set} matches \code{'gray'} or \code{'grey'}, a single vector of colors codes is returned,
#' corresponding to either the length of \code{by} (if not missing) or the sum of \code{ns}.
#'
#' Otherwise, if \code{by} or \code{ns} is not missing, a set of colors is sampled for each value of
#' \code{ns} (where \code{ns=table(by)} if \code{by} is not missing). In this case, \code{set} determines the
#' seed color for each sample (e.g., if \code{set[1]='red'}, \code{ns[1]} colors similar to \code{'red'}
#' would be sampled).
#' @examples
#' x=rnorm(1000)
#' y=rnorm(1000)
#'
#' # get a quick feel for the range of colors
#' plot(y~x,pch=20,cex=10,col=splot.color(ns=c(20,20)))
#'
#' # with more extreme seed colors, differences are subtler
#' plot(y~x,pch=20,cex=10,col=splot.color(c('red','blue'),ns=c(20,20)))
#'
#' @export
#' @importFrom grDevices col2rgb rgb colors

splot.color=function(set='pastel',by=NULL,ns=1,maxiter=1000,flat=TRUE){
  sets=list(
    bright=c('#45ff00','#ba00ff','#000000','#ff0000','#fffd00','#003dff','#00f2f8','#999999','#ff891b'),
    dark=c('#1b8621','#681686','#2a2a2a','#7c0d0d','#b5bc00','#241c80','#1a7e8b','#666666','#b06622'),
    pastel=c('#82c473','#a378c0','#616161','#9f5c61','#d3d280','#6970b2','#78c4c2','#454744','#d98c82'),
    grey=function(n) if(n<2) '#666666' else grey(.2:n/(n+n*if(n<10) .1 else .3))
  )
  set=tolower(set)
  cm=TRUE
  if(length(set)==1 && grepl('^#|^bri|^dar|^pas|^grey|^gra',set,TRUE)){
    set=match.arg(set,names(sets))
    if(set=='grey') return(sets$grey(if(!missing(by)) length(by) else sum(ns))) else set=sets[[set]]
    cm=FALSE
  }
  if(cm) if(any(ck<-!grepl('^#',set)) && (ck<-set[ck]%in%colors())){
    set[ck]=apply(col2rgb(set[ck]),2,function(v)tolower(do.call(rgb,as.list(v/255))))
    if(any(ck<-!grepl('^#',set))){
      warning('not sure what ',paste(set[ck],collapse=' or '),' is supposed to be :L')
      set=set[!ck]
      if(length(set)==0) stop('no colors recognized in set')
    }
  }
  if(!missing(by) || !missing(ns)){
    if(!missing(by)) ns=table(by)
    if(length(ns)==0) return(set)
    set=rep_len(set,length(ns))
    hdc=c(0:9,letters[1:6])
    hdc=outer(hdc,hdc,paste0)
    ccord=function(cc){
      cc=strsplit(sub('^#','',cc),'')[[1]]
      cc=paste0(cc[c(TRUE,FALSE)],cc[c(FALSE,TRUE)])
      vapply(cc,function(c)which(hdc==c,TRUE),c(0,0))
    }
    ccode=function(m){
      s=seq_len(16)
      paste0('#',paste(apply(m,2,function(cc){
        hdc[which.min(abs(s-cc[1])),which.min(abs(s-cc[2]))]
      }),collapse=''))
    }
    csamp=function(code,n=1){
      ocs=code
      code=ccord(code)
      if(any(ck<-code>14)) code[ck]=code[ck]-(code[ck]-14)
      if(any(ck<-code<2)) code[ck]=code[ck]+(2-code[ck])
      i=1
      while(length(ocs)<=n && i<maxiter){
        s=sample(1:6,3)
        nc=code
        nc[s]=nc[s]+sample(-2:2,3,TRUE)
        nc=ccode(nc)
        if(!nc%in%ocs) ocs=c(ocs,nc)
        i=i+1
      }
      ocs
    }
    s=seq_along(set)
    o=lapply(s,function(i) if(ns[i]==1) set[i] else csamp(set[i],ns[i]))
    if(!missing(by)){
      gs=names(o)=names(ns)
      color=rep(NA,length(by))
      for(g in gs) color[by==g]=rep_len(o[[g]],ns[g])
      return(if(flat) color else cbind(group=as.character(by),color))
    }else{
      names(o)=set
      return(if(flat) unlist(o,use.names=FALSE) else o)
    }
  }
  set
}

#' splot benchmarker
#'
#' Time one or more expressions over several iteration, then plot the distributions of their times.
#' @param ... accepts any number of expressions to be timed. See examples.
#' @param runs the number of overall iterations. Increase to stabilize estimates.
#' @param runsize the number of times each expression is evaluated within each run. Increase to
#'   differentiate estimates (particularly for very fast operations).
#' @param cleanup logical; if \code{TRUE}, garbage collection will be performed before each run.
#'   Garbage collection greatly increases run time, but may result in more stable timings.
#' @param print.names logical; if \code{FALSE}, the entered expressions will be included in the plot
#'   as legend names. Otherwise, (and if the number of expressions is over 5 or the length of any
#'   expression is over 50 characters) expressions are replaced with numbers corresponding to their
#'   entered position.
#' @param options a list of options to pass on to splot.
#' @examples
#' # compare a few equivalent ways of looping through a vector
#' # though you'd probably need to increase the number of runs
#' # for a consistent determination
#' splot.bench(
#'   sapply(1:100,'*',10),
#'   mapply('*',1:100,10),
#'   vapply(1:100,'*',0,10),
#'   unlist(lapply(1:100,'*',10)),
#'   {a=numeric(100); for(i in 1:100) a[i]=i*10; a}
#' )
#' @export

splot.bench=function(...,runs=20,runsize=200,cleanup=FALSE,print.names=FALSE,options=list()){
  e=sapply(as.character(substitute(list(...)))[-1],function(t)parse(text=t))
  es=length(e)
  ne=names(e)
  seconds=matrix(NA,runs,es,dimnames=list(c(),ne))
  rs=seq_len(runsize)
  p=cleanup || (runs*runsize>999)
  tryCatch(for(t in e) eval(t),
    error=function(e)stop('one of your expressions breaks:\n',e,call.=FALSE))
  ost=proc.time()[3]
  cat('benchmarking',es,'expression(s) in chunks of',runsize,'per run... ')
  if(p) cat('\nrun 0 of',runs)
  for(r in seq_len(runs)){
    col=sample(ne)
    seconds[r,col]=vapply(col,function(t){
      if(cleanup) gc(FALSE)
      st=proc.time()[3]
      for(i in rs) eval(e[t])
      proc.time()[3]-st
    },0)
    if(p) cat('\rrun',r,'of',runs)
  }
  if(p) cat('\r')
  cat('finished in',round(proc.time()[3]-ost,2),'seconds                    \n\n')
  cat('expressions:\n\n')
  icn=seq_len(es)
  ne=gsub('\n','\n   ',ne,fixed=TRUE)
  for(i in icn) cat(i,'. ',ne[i],'\n',sep='')
  cat('\n')
  print(round(matrix(c(colSums(seconds),colMeans(seconds)),2,byrow=TRUE,
    dimnames=list(c('total time (seconds)','mean time per run'),icn)),4))
  if(print.names || es>5 || any(nchar(names(e))>50)) colnames(seconds)=icn
  title=paste('timing of',runs,'runs of',runsize,'calls each')
  splot(seconds,title=title,labels.filter=FALSE,labels.trim=FALSE,options=options)
}