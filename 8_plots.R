# BATCHTOOLS
# 8: plots

library(latex2exp)

# load results
# load('results/results.RData')

# collect plot tables
# gaussian n tables
gauss_fn <- as.data.frame(plot_tables[[1]])
gauss_gn <- as.data.frame(plot_tables[[11]])
gauss_pn <- as.data.frame(plot_tables[[21]])
# gaussian m tables
gauss_fm <- as.data.frame(plot_tables[[2]])
gauss_gm <- as.data.frame(plot_tables[[12]])
gauss_pm <- as.data.frame(plot_tables[[22]])
# gaussian d1 tables (F and P the same)
gauss_pd1 <- as.data.frame(plot_tables[[23]])
# gaussian d2 tables (G and P the same)
gauss_pd2 <- as.data.frame(plot_tables[[24]])
# logistic n tables
log_fn <- as.data.frame(plot_tables[[5]])
log_gn <- as.data.frame(plot_tables[[15]])
log_pn <- as.data.frame(plot_tables[[25]])
# logistic m tables
log_fm <- as.data.frame(plot_tables[[6]])
log_gm <- as.data.frame(plot_tables[[16]])
log_pm <- as.data.frame(plot_tables[[26]])
# logistic d1 tables
log_pd1 <- as.data.frame(plot_tables[[27]])
# logistic d2 tables
log_pd2 <- as.data.frame(plot_tables[[28]])
# logistic dens tables
log_fdens <- as.data.frame(plot_tables[[9]])
log_gdens <- as.data.frame(plot_tables[[19]])
log_pdens <- as.data.frame(plot_tables[[29]])
# dependent dimensions tables
large_fm <- as.data.frame(plot_tables[[10]][-1,])
large_pm <- as.data.frame(plot_tables[[30]][-1,])

# black and white indicator
bw <- TRUE
# global plot options
par(mfrow=c(3,2),mar=c(.25,0,.25,.5),oma=c(4,5,4,3),cex.axis=.8,pty='m')

#### plot of gaussian results ####

# F error for Gaussian model
matplot(gauss_fn[,1],gauss_fn[,c(2,4,5)],type="b",
        lty=c(2,1,1),pch=c(1,0,17),
        #ylim=c(0,.18),
        ylim=c(0,.08),
        xaxt='n',col=ifelse(rep(bw,3),1,c('purple','blue','green')))
text(x=210,y=.14,labels=c('(A)'),pos=3)
matplot(gauss_fm[,1],gauss_fm[,c(2,4,5)],type='b',
        lty=c(2,1,1),pch=c(1,0,17),
        #ylim=c(0,.18),
        ylim=c(0,.08),
        yaxt="n",xaxt="n",col=ifelse(rep(bw,3),1,c('purple','blue','green')))
text(x=5,y=.14,labels=c('(D)'),pos=3)
if(!bw){
legend(x=21,
       #y=.165,
       y=.079,
       legend=c('Non-convex','MultiNeSS','MultiNeSS+','COSIE'),
       lty=c(2,1,1,3),
       pch=c(1,0,17,18),
       col=ifelse(rep(bw,4),1,c('purple','blue','green','orange')),
       bty='n',cex=.8)
}
# G error for Gaussian model
matplot(gauss_gn[,1],gauss_gn[,c(2,4,5)],type="b",
        lty=c(2,1,1),pch=c(1,0,17),ylim=c(0,.18),
        xaxt='n',col=ifelse(rep(bw,3),1,c('purple','blue','green')))
text(x=210,y=0,labels=c('(B)'),pos=3)

matplot(gauss_gm[,1],gauss_gm[,c(2,4,5)],type='b',
        lty=c(2,1,1),pch=c(1,0,17),ylim=c(0,.18),
        yaxt="n",xaxt="n",col=ifelse(rep(bw,3),1,c('purple','blue','green')))
text(x=5,y=0,labels=c('(E)'),pos=3)

# P error for Gaussian model
matplot(gauss_pn[,1],gauss_pn[,c(2,3,4,5)],type="b",
        lty=c(2,3,1,1),pch=c(1,18,0,17),ylim=c(0,.18),xlab="n",
        col=ifelse(rep(bw,4),1,c('purple','orange','blue','green')))
text(x=210,y=0,labels=c('(C)'),pos=3)

matplot(gauss_pm[,1],gauss_pm[,c(2,3,4,5)],type='b',
        lty=c(2,3,1,1),pch=c(1,18,0,17),ylim=c(0,.18),
        yaxt="n",xlab="m",
        col=ifelse(rep(bw,4),1,c('purple','orange','blue','green')))
text(x=5,y=0,labels=c('(F)'),pos=3)

mtext('n',side=1,line=2.5,outer=T,at=.25)
mtext('m',side=1,line=2.5,outer=T,at=.75)
mtext('Gaussian model',side=3,line=1.5,outer=T,at=.5)
mtext('m=8',side=3,line=.25,outer=T,at=.07,cex=.75)
mtext('n=400',side=3,line=.25,outer=T,at=.93,cex=.75)
mtext(TeX('Err_F'),side=2,line=2.5,outer=T,at=.82)
mtext(TeX('Err_G'),side=2,line=2.5,outer=T,at=.5)
mtext(TeX('Err_P'),side=2,line=2.5,outer=T,at=.15)

# export as 5 x 6 landscape pdf: gaussian[_bw].pdf

#### plot of logistic results ####
par(mfrow=c(3,2),mar=c(.25,0,.25,.5),oma=c(4,5,4,3),cex.axis=.8)

# F error for logistic model
matplot(log_fn[,1],log_fn[,c(3,4,5,6)],type="b",
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,.75),
        xaxt='n',col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=210,y=-.05,labels=c('(A)'),pos=3)
matplot(log_fm[,1],log_fm[,c(3,4,5,6)],type='b',
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,.75),
        yaxt="n",xaxt="n",col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=5,y=-.05,labels=c('(D)'),pos=3)

# G error for logistic model
matplot(log_gn[,1],log_gn[,c(3,4,5,6)],type="b",
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,.75),
        xaxt='n',col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=210,y=-.01,labels=c('(B)'),pos=3)

matplot(log_gm[,1],log_gm[,c(3,4,5,6)],type='b',
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,.75),
        yaxt="n",xaxt="n",col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=5,y=-.01,labels=c('(E)'),pos=3)

# P error for logistic model
matplot(log_pn[,1],log_pn[,c(2,3,4,5,6)],type="b",
        lty=c(3,2,4,1,1),pch=c(18,1,6,0,17),ylim=c(0,.42),xlab="n",
        col=ifelse(rep(bw,5),1,c('orange','purple','red','blue','green')))
text(x=210,y=-.02,labels=c('(C)'),pos=3)

matplot(log_pm[,1],log_pm[,c(2,3,4,5,6)],type='b',
        lty=c(3,2,4,1,1),pch=c(18,1,6,0,17),ylim=c(0,.42),
        yaxt="n",xlab="m",
        col=ifelse(rep(bw,5),1,c('orange','purple','red','blue','green')))
text(x=5,y=-.02,labels=c('(F)'),pos=3)
if(!bw){
legend(x=13,y=0.42,
       legend=c('Non-convex','MultiNeSS','MultiNeSS+','COSIE','M-GRAF'),
       lty=c(2,1,1,3,4),
       pch=c(1,0,17,18,6),
       col=ifelse(rep(bw,5),1,c('purple','blue','green','orange','red')),
       bty='n',cex=.8,ncol=2)
}

mtext('n',side=1,line=2.5,outer=T,at=.25)
mtext('m',side=1,line=2.5,outer=T,at=.75)
mtext('Logistic model',side=3,line=1.5,outer=T,at=.5)
mtext('m=8',side=3,line=.25,outer=T,at=.07,cex=.75)
mtext('n=400',side=3,line=.25,outer=T,at=.93,cex=.75)
mtext(TeX('Err_F'),side=2,line=2.5,outer=T,at=.82)
mtext(TeX('Err_G'),side=2,line=2.5,outer=T,at=.5)
mtext(TeX('Err_P'),side=2,line=2.5,outer=T,at=.15)

# export as 5 x 6 landscape pdf: logistic[_bw].pdf

#### plot of density results ####

par(mfrow=c(3,1))

# drop MGRAF for rho=6
log_fdens[7,4] <- log_gdens[7,4] <- log_pdens[7,4] <- NaN

# reversed indices
irev <- rev(1:7)

matplot(log_fdens[,1],log_fdens[irev,c(3,4,5,6)],type='b',
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,2),xaxt='n',
        col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=-.2,y=1.9,labels=c('(A)'),pos=4)
if(!bw){
legend(x=4.4,y=2.1,
       legend=c('Non-convex','MultiNeSS','MultiNeSS+','COSIE','M-GRAF'), # non-convex in paper
       lty=c(2,1,1,3,4),
       pch=c(1,0,17,18,6),
       col=ifelse(rep(bw,5),1,c('purple','blue','green','orange','red')),
       bty='n',cex=.8,ncol=2,text.width=.65)
}
matplot(log_gdens[,1],log_gdens[irev,c(3,4,5,6)],type='b',
        lty=c(2,4,1,1),pch=c(1,6,0,17),ylim=c(0,1),xaxt='n',
        col=ifelse(rep(bw,4),1,c('purple','red','blue','green')))
text(x=-.2,y=0.96,labels=c('(B)'),pos=4)
matplot(log_pdens[,1],log_pdens[irev,c(2,3,4,5,6)],type='b',
        lty=c(3,2,4,1,1),pch=c(18,1,6,0,17),ylim=c(0,1),xaxt='n',
        col=ifelse(rep(bw,5),1,c('orange','purple','red','blue','green')))
# manual axis to label with edge density
axis(side=1,at=0:6,labels=FALSE)
tixin <- .045
tixat <- seq(from=tixin,to=1-tixin,length.out=7)
mtext(rev(c('0.50','0.34','0.21','0.12','0.06','0.035','0.015')),
      at=tixat,side=1,outer=T,line=.5,
      cex=.5)
text(x=-.2,y=0.96,labels=c('(C)'),pos=4)

mtext('Edge density',side=1,line=2.5,outer=T,at=.5)
mtext('m=8',side=3,line=.25,outer=T,at=.07,cex=.75)
mtext('n=400',side=3,line=.25,outer=T,at=.93,cex=.75)
mtext('Logistic model',side=3,line=1.5,outer=T,at=.5)
mtext(TeX('Err_F'),side=2,line=2.5,outer=T,at=.82)
mtext(TeX('Err_G'),side=2,line=2.5,outer=T,at=.5)
mtext(TeX('Err_P'),side=2,line=2.5,outer=T,at=.15)

# export as 5x6 landscape pdf: logistic_density[_bw].pdf

#### plot of d1,d2 results ####

par(mfrow=c(2,2))

matplot(gauss_pd1[,1],gauss_pd1[,c(2,3,4,5)],type='b',
        lty=c(2,3,1,1),pch=c(1,18,0,17),ylim=c(0,.14),xaxt='n',
        col=ifelse(rep(bw,4),1,c('purple','orange','blue','green')))
text(x=12.3,y=.12,labels=c('(A)'),pos=2)
if(!bw){
legend(x=2,
       y=.15,
       legend=c('Non-convex','MultiNeSS','MultiNeSS+','COSIE'),
       lty=c(2,1,1,3),
       pch=c(1,0,17,18),
       col=ifelse(rep(bw,4),1,c('purple','blue','green','orange')),
       bty='n',cex=.8,ncol=1)
}
matplot(gauss_pd2[,1],gauss_pd2[,c(2,3,4,5)],type='b',
        lty=c(2,3,1,1),pch=c(1,18,0,17),ylim=c(0,.14),xaxt='n',yaxt='n',
        col=ifelse(rep(bw,4),1,c('purple','orange','blue','green')))
text(x=12.3,y=.02,labels=c('(C)'),pos=2)
matplot(log_pd1[,1],log_pd1[,c(2,3,4,5)],type='b',
        lty=c(3,2,1,1),pch=c(18,1,0,17),ylim=c(0,.3),
        col=ifelse(rep(bw,4),1,c('orange','purple','blue','green')))
text(x=12.3,y=.27,labels=c('(B)'),pos=2)
matplot(log_pd2[,1],log_pd2[,c(2,3,4,5)],type='b',
        lty=c(3,2,1,1),pch=c(18,1,0,17),ylim=c(0,.3),yaxt='n',
        col=ifelse(rep(bw,4),1,c('orange','purple','blue','green')))
text(x=12.3,y=.05,labels=c('(D)'),pos=2)
mtext(TeX('d_1'),side=1,line=2.1,outer=T,at=.25)
mtext(TeX('d_2'),side=1,line=2.1,outer=T,at=.75)
mtext(TeX('d_2=0'),side=3,line=.25,outer=T,at=.07,cex=.75)
mtext(TeX('d_1=0'),side=3,line=.25,outer=T,at=.93,cex=.75)
mtext(TeX('Err_P'),side=2,line=2.5,outer=T,at=.75)
mtext(TeX('Err_P'),side=2,line=2.5,outer=T,at=.25)
mtext('Gaussian model',side=4,line=.5,outer=T,at=.75)
mtext('Logistic model',side=4,line=.5,outer=T,at=.25)

# export as 5x6 landscape pdf: zerodim[_bw].pdf

#### plot of large m results ####

par(mfrow=c(2,1))

matplot(large_fm[,1],large_fm[,c(4,5,6,7)],type='b',
        lty=1,xaxt='n',ylim=c(0,100),
        pch=ifelse(rep(bw,4),c(2,5,3,4),2),
        col=ifelse(rep(bw,4),1,c('green','blue','red','orange')))
if(!bw){
legend(x=735,
       y=90,
       legend=c(0.2,0.4,0.6,0.8),
       lty=1,
       pch=ifelse(rep(bw,4),c(2,5,3,4),2),
       col=ifelse(rep(bw,4),1,c('green','blue','red','orange')),
       bty='n',cex=.8,ncol=2,
       title=TeX('$\\rho$'))
}
matplot(large_pm[,1],large_pm[,c(4,5,6,7)],type='b',
        lty=1,ylim=c(40,65),
        pch=ifelse(rep(bw,4),c(2,5,3,4),2),
        col=ifelse(rep(bw,4),1,c('green','blue','red','orange')))
mtext(TeX('m'),side=1,line=2.5,outer=T,at=.5)
mtext('Gaussian model',side=3,line=1.5,outer=T,at=.5)
mtext(TeX('Err*_F'),side=2,line=2.5,outer=T,at=.75)
mtext(TeX('Err*_P'),side=2,line=2.5,outer=T,at=.25)

# export as 5x6 landscape pdf: large[_bw].pdf
