#' splot colors
#'
#' Get a prespecified set of 9 colors, or a set of graded or random, potentially grouped colors.
#' @param x dictates the number and shade of colors. If a single value, returns that many samples of the
#'   first \code{seed} entry. If a vector, returns a color for each entry. If numeric, a single seed color
#'   is sampled in order of the vector. If a character or factor, a separate seed color is assigned to
#'   each level, then sampled within levels. Values or vectors in a list are each assigned a seed color.
#' @param by a vector to group \code{x} by; each level is assigned a seed color.
#' @param seed a vector of color names or codes to adjust from, lining up with levels of \code{x} or
#'   \code{by}, or the name of a palette, partially matching \code{'bright'}, \code{'dark'},
#'   \code{'pastel'}, or \code{'grey'}.
#' @param brightness adjusts the RGB values of the seed color, usually between -1 and 1.
#' @param luminance adjusts the white levels of the seed color, usually between -1 and 1.
#' @param opacity sets the opacity of the seed color, between 0 and 1.
#' @param extend if \code{method='scale'}, extends the range of the gradient beyond the sampled range,
#'   making for more similar colors (defaults is .5, with 0 sampling the full range). If
#'   \code{method='related'}, increases the amount any of the RGB values can be adjusted, making for
#'   potentially more different colors (default is 2).
#' @param lighten logical; if \code{TRUE}, scaled colors are lightened instead of darkened. Only
#'   applicable if \code{method='scale'}.
#' @param shuffle logical; if \code{TRUE}, scaled colors are shuffled. Only applicable if
#'   \code{method='scale'}.
#' @param flat logical; if \code{FALSE} and \code{x} is a character, factor, or list, or \code{by} is not
#'   missing, a list is returned.
#' @param method a character setting the sampling method: If \code{'related'} (\code{'^rel|^ran|^o'}),
#'   RGB values are freely adjusted, resulting in similar colors. If \code{'none'} (\code{'^no|^f|^bin'}),
#'   Seed colors are simply repeated in each level (sampling is off). Otherwise, RGB values are adjusted
#'   together, resulting in a gradient.
#' @param grade logical; if \code{TRUE}, seeds are adjusted on the scale of numeric \code{x}s.
#'   Otherwise, seeds are adjusted in even steps along numeric \code{x}s.
#' @param decreasing logical; if \code{FALSE}, assigns colors to numeric \code{x}s in increasing order.
#' @param nas value to replace missing values with.
#' @details
#' If \code{x} and \code{by} are not specified (or are characters with a length of 1, in which case they
#' are treated as \code{seed}), only the seed palette is returned.
#'
#' To expand on a palette, seed colors are assigned to groups, and variants of each seed are assigned to
#' values or levels within groups, or randomly or as a gradient if there are no values or level to assign to.
#'
#' Seed colors are assigned to groups. If \code{x} is a character or factor and no \code{by} has been
#' specified, groups are the unique levels of \code{x}. If \code{by} is specified and is a character or
#' factor, or has fewer than 10 unique levels, groups are levels of \code{by}. If \code{x} is a list,
#' groups are list entries.
#'
#' The number of variants for each seed color is determined either by a value (if the value has a length
#' of 1; e.g., \code{x=10}), the vector's length (if \code{x} is numeric), or the count of the given level
#' (if \code{x} is a factor or character vector).
#'
#' @examples
#' # including no arguments or just a palette name will only return the palette as a character vector
#' pastel_palette = splot.color()
#' dark_palette = splot.color('dark')
#'
#' # entering a number for x will generate that many variants of the first seed color
#' red_scale = splot.color(10, 'red')
#'
#' # entering a list of values as x will return that many variants of the associated seed
#' red_and_green_scales = splot.color(list(10,10), seed=c('red','green'))
#'
#' # this shows gradients of each color in the default palette
#' # a list entered as colorby is treated as arguments to splot.color
#' # periods before the position name refer to the internally assembled data
#' splot(
#'   rep(splot.color(),each=100)~rep.int(seq.int(.01,1,.01),9),colorby=list(.x,.y),
#'   lines=FALSE,mar=c(2,4,0,0),cex=c(points=3),leg=FALSE,pch=15,
#'   title="'pastel' palette",labx='value of x',laby='seed color'
#' )
#'
#' # colors graded by value, entered in a list
#' plot(1:30,numeric(30),pch=15,cex=10,col=splot.color(list(1:8,c(7:1,1:7),8:1)))
#'
#' # comparing sampling methods:
#' #   on top are 1000 similar colors, with different RGB ratios
#' #   on bottom are 268 colors with the same RGB ratio at different levels
#' splot(
#'   c(rnorm(1000),rnorm(1000,10))~rnorm(2000),lines=FALSE,
#'   colors=c(splot.color(1000),splot.color(1000,method='related'))
#' )
#'
#' @export

splot.color=function(x=NULL,by=NULL,seed='pastel',brightness=0,luminance=0,opacity=1,extend=.7,
  lighten=FALSE,shuffle=FALSE,flat=TRUE,method='scale',grade=FALSE,decreasing=TRUE,nas='#000000'){
  sets=list(
    bright=c('#45ff00','#ba00ff','#000000','#ff0000','#fffd00','#003dff','#00f2f8','#999999','#ff891b'),
    dark=c('#1b8621','#681686','#2a2a2a','#7c0d0d','#b5bc00','#241c80','#1a7e8b','#666666','#b06622'),
    pastel=c('#82c473','#a378c0','#616161','#9f5c61','#d3d280','#6970b2','#78c4c2','#454744','#d98c82'),
    grey=function(n) grey(.2:n/(n+n*if(n<10) .1 else .3))
  )
  if(missing(seed) && is.character(x) && (length(x) == 1 || all(tolower(x) %in% colors()))){
    seed = x
    x = numeric(length(seed)) + 1
  }
  if(missing(seed) && is.character(by) && length(by)==1){
    seed=by
    by=NULL
  }
  seed=tolower(seed)
  ox=NULL
  lvs=function(x) if(is.factor(x)) base::levels(x) else unique(x)
  if(!is.null(ncol(x)) && ncol(x)>1){
    if(is.null(by)) by=x[,2]
    x=x[,1]
  }else if(is.list(x) && length(x)==1) x=x[[1]]
  if(!is.null(by)){
    ol=length(x)
    if(is.null(x)){
      x=by
      by=NULL
    }else if(ol!=length(by)){
      if(is.numeric(by) && length(by)==1 && by<ol) by=rep_len(seq_len(by),ol) else{
        by=NULL
        warning('splot.color: by was dropped as it is not the same length as x')
      }
    }
  }
  if(!is.null(x) && (!(is.list(x) || is.numeric(x)) || (is.numeric(x) && !is.null(by)))){
    if(is.null(by)){
      ox=x
      x=as.list(table(x))[lvs(ox)]
    }else{
      if(is.numeric(by) && length(lvs(by))>9) warning('splot.color: only non-numeric bys are accepted') else{
        ox = if(is.factor(by)) by else as.character(by)
        x = split(x, by)[lvs(ox)]
      }
    }
  }
  ol=length(x)
  if(ol==1 && is.list(x)){
    x=x[[1]]
    ol=length(x)
    ox=NULL
  }
  ckd=!shuffle && ol!=1 && is.numeric(x) && any(duplicated(x))
  if(ckd){
    ox=x
    x=lvs(x)
    ol=length(x)
    if(ol==1){
      ckd=FALSE
      x=length(ox)
    }
  }
  n=if(ol==1) x else ol
  if(length(seed)==1 && grepl('^bri|^dar|^pas|^gr[ae]y',seed)){
    seed=match.arg(seed,names(sets))
    seed=if(seed=='grey') if(n==1) '#666666' else if(ol==1)
      return(sets$grey(n)) else sets$grey(n) else sets[[seed]]
    if(is.null(x) || (ol==1 && n<2)) return(seed)
  }
  sc=if(grepl('^rel|^ran|^o',method,TRUE)){
    r=if(missing(extend)) 2 else max(.001,extend)
    function(cc,n){
      if(length(n) != 1) n = length(n)
      cc=adjustcolor(cc)
      hdc=c(0:9,LETTERS[1:6])
      hdc=outer(hdc,hdc,paste0)
      ccord=function(cc){
        cc=strsplit(cc,'')[[1]][2:7]
        cc=paste0(cc[c(TRUE,FALSE)],cc[c(FALSE,TRUE)])
        vapply(cc,function(c)which(hdc==c,TRUE),c(0,0))
      }
      ccode=function(m){
        s=seq_len(16)
        paste0('#',paste(apply(m,2,function(cc){
          hdc[which.min(abs(s-cc[1])),which.min(abs(s-cc[2]))]
        }),collapse=''))
      }
      csamp=function(code,n){
        n=max(1,n-1)
        ocs=NULL
        code=ccord(code)
        if(any(ck<-code>14)) code[ck]=code[ck]-(code[ck]-14)
        if(any(ck<-code<2)) code[ck]=code[ck]+(2-code[ck])
        i=1
        while(length(ocs)<=n && i<9999){
          s=sample(1:6,3)
          nc=code
          nc[s]=nc[s]+sample(-r:r,3,TRUE)
          nc=ccode(nc)
          if(!nc%in%ocs) ocs=c(ocs,nc)
          i=i+1
        }
        if(any(opacity!=1,brightness!=0,luminance!=0)){
          adj=1+brightness
          ocs=adjustcolor(ocs,opacity,adj,adj,adj,c(rep(luminance,3),0))
        }
        if(length(ocs)!=n+1) ocs=rep_len(ocs,n+1)
        ocs
      }
      csamp(cc,n)
    }
  }else if(grepl('^no|^f|^bin',method,TRUE)) function(cc, n){
    if(length(n) != 1) n = length(n)
    if(any(opacity!=1,brightness!=0,luminance!=0)){
      adj=1+brightness
      cc=adjustcolor(cc,opacity,adj,adj,adj,c(rep(luminance,3),0))
    }
    rep(cc, n)
  }else function(cc, n){
    s = if(length(n) != 1){
      s = sort(n - mean(n, na.rm = TRUE), decreasing)
      s = s + abs(min(s, na.rm = TRUE))
      n = length(s)
      s / max(s, na.rm = TRUE) * (n - 1) + 1
    }else sort(seq_len(n), decreasing)
    r = max(n, n + n * extend)
    if(!lighten) s = s + r - max(s, na.rm = TRUE)
    vapply(s, function(i){
      adj = i / r + brightness
      if(lighten) adj = adj + 1
      adjustcolor(cc, opacity, adj, adj, adj, c(rep(luminance, 3), 0))
    }, '')
  }
  if(!is.list(x)){
    seed = sc(seed[1], if(grade) x else n)
    if(ckd){
      tx=ox
      oxf = is.finite(ox)
      for(i in seq_len(n)) ox[oxf & ox == x[i]] = seed[i]
      x=tx
      seed=ox
    }else if(shuffle) seed=sample(seed) else if(length(x)!=1) seed=seed[order(order(x))]
  }else{
    if(length(seed)<n) seed=rep_len(seed,n)
    seed=lapply(seq_len(n),function(g){
      l=length(x[[g]])
      if(l!=1 || as.integer(x[[g]])>0){
        cs=sc(seed[[g]],if(l==1 || (is.numeric(x[[g]]) && grade)) x[[g]] else l)
        if(shuffle) sample(cs) else if(l!=1) cs[order(order(x[[g]]))] else cs
      }else seed[[g]]
    })
    names(seed)=if(!is.null(names(x))) names(x) else vapply(seed,'[[','',1)
    if(flat) seed=if(!is.null(ox) && all(lvs(ox)%in%names(seed))){
      by=as.character(ox)
      for(g in lvs(ox)){
        su = !is.na(by) & !is.nan(by) & by == g
        ssu = sum(su)
        if(is.finite(ssu) && ssu) by[su] = rep_len(seed[[g]], ssu)
      }
      by
    }else unlist(seed)
  }
  if(!is.list(seed)) seed[is.na(seed) | is.nan(seed) | seed %in% c('NA', 'NaN', 'Inf', '-Inf')] = nas
  if(opacity == 1) seed = if(is.list(seed)) lapply(seed, function(s) sub('FF$', '', s)) else sub('FF$', '', seed)
  seed
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
#' # increase the number of runs for more stable estimates
#'
#' # compare ways of looping through a vector
#' splot.bench(
#'   sapply(1:100,'*',10),
#'   mapply('*',1:100,10),
#'   vapply(1:100,'*',0,10),
#'   unlist(lapply(1:100,'*',10)),
#'   {a=numeric(100); for(i in 1:100) a[i]=i*10; a},
#'   runs = 20, runsize = 200
#' )
#'
#' # compare ways of setting all but the maximum value of each row in a matrix to 0
#' \dontrun{
#' mat = matrix(c(rep(1, 4), rep(0, 8)), 4, 3)
#' splot.bench(
#'   t(vapply(seq_len(4), function(r){
#'     mat[r, mat[r,] < max(mat[r,])] = 0
#'     mat[r,]
#'   }, numeric(ncol(mat)))),
#'   do.call(rbind, lapply(seq_len(4), function(r){
#'     mat[r, mat[r,] < max(mat[r,])] = 0
#'     mat[r,]
#'   })),
#'   do.call(rbind, lapply(seq_len(4), function(r){
#'     nr = mat[r,]
#'     nr[nr < max(nr)] = 0
#'     nr
#'   })),
#'   {nm = mat; for(r in seq_len(4)){
#'     nr = nm[r,]
#'     nm[r, nr < max(nr)] = 0
#'   }; nm},
#'   {nm = mat; for(r in seq_len(4)) nm[r, nm[r,] < max(nm[r,])] = 0; nm},
#'   {nm = matrix(0, dim(mat)[1], dim(mat)[2]); for(r in seq_len(4)){
#'     m = which.max(mat[r,])
#'     nm[r, m] = mat[r, m]
#'   }; nm},
#'   {ck = do.call(rbind, lapply(seq_len(4), function(r){
#'     nr = mat[r,]
#'     nr < max(nr)
#'   })); nm = mat; nm[ck] = 0; nm},
#'   t(apply(mat, 1, function(r){
#'     r[r < max(r)] = 0
#'     r
#'   })),
#'   runs = 50, runsize = 200
#' )
#' }
#' @export

splot.bench=function(...,runs=20,runsize=200,cleanup=FALSE,print.names=FALSE,options=list()){
  e=sapply(as.character(substitute(list(...)))[-1],function(t)parse(text=t))
  e = e[!duplicated(names(e))]
  es=length(e)
  if(!es) stop('no expressions found', call. = FALSE)
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
  cat('finished',runs,'runs in',round(proc.time()[3]-ost,2),'seconds       \n\n')
  cat('expressions:\n\n')
  icn=seq_len(es)
  ne=gsub('\n','\n   ',ne,fixed=TRUE)
  for(i in icn) cat(i,'. ',ne[i],'\n',sep='')
  cat('\n')
  print(round(matrix(c(colSums(seconds),colMeans(seconds)),2,byrow=TRUE,
    dimnames=list(c('total time (seconds)','mean time per run'),icn)),4))
  if(!print.names) if(!missing(print.names) || es>5 || any(nchar(names(e))>50))
    colnames(seconds)=icn
  if(es == 1 && runs == 1) return(NULL)
  title=paste('timing of',runs,'runs of',runsize,'calls each')
  if(nrow(seconds) == 1){
    options$x = colnames(seconds)
    seconds = seconds[1,]
  }
  splot(seconds,title=title,labels.filter=FALSE,labels.trim=FALSE,options=options)
}