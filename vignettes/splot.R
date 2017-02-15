## ------------------------------------------------------------------------
library(splot)

## ------------------------------------------------------------------------
data = read.csv('https://files.osf.io/v1/resources/963gp/providers/osfstorage/58079eafb83f6901e4bcf194?direct=true&action=download')

## ------------------------------------------------------------------------
data[1:15, 1:8]

## ------------------------------------------------------------------------
attach(data)

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(LSM7, by=Study)

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(FemComp, by=Study)

## ---- fig.height=3.5, fig.width=5, fig.show='hold', dev='CairoSVG'-------
splot(LSM7~Topic, su=Study==4)
splot(LSM7~Style, su=Study==4)
splot(LSM7~Label, su=Study==4)

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(LSM7~Topic*Label, su=Study==4, levels=list(Topic=c('Life Course', 'Relational'), Label=c('Male', 'Female')), lpos='bottomright')

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(LSM7~FemComp*Topic*Label, su=Study==4, levels=list(Topic=c('Life Course', 'Relational'), Label=c('Male', 'Female')))

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(LSM7~FemComp*Topic*Label, su=Study==3, levels=list(Topic=c('Life Course', 'Relational'), Label=c('Male', 'Female')))

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
splot(LSM7~FemComp*Topic*Style, su=Study==3, levels=list(Topic=c('Life Course', 'Relational'), Label=c('Male', 'Female')))

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
#adjusting some setting makes things look nicer, but the only necessary extra one is lim.
#here, lim prevents ParticipantID from being split since it has so many levels,
#lty makes all lines solid, error turns off error bars, and mxl spreads the x labels apart more.
splot(LSM7~Prompt*ParticipantID, su=Study>2, lim=F, lty=F, error=F, mxl=c(1,2))

## ---- fig.height=6, fig.width=7, dev='CairoSVG'--------------------------
#reversing style coding, just cause
Style = ifelse(Style==0, 1, 0)
Study_and_Topic = paste0('S', Study, ' ', ifelse(Topic==1, 'Life', 'Relational'))

splot(ppSes~Study_and_Topic*Style, type='bar', colors='grey', levels=list(Style=c('Feminine', 'Masculine')))

