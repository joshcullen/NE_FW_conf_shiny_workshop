rm(list=ls())

setwd('U:\\uf\\Shiny course\\2022 NE FWC\\2 BK tool')
dat=read.csv('calc all1.csv',as.is=T)

#subset the data
cond=dat$fever2wk==0 & dat$male==0 & dat$reg=='Sud-Ouest' & dat$urban==0
dat1=dat[cond,]

#draw graph
plot(pinf~age,data=dat1,ylim=c(0,1),pch=19)
points(pinf.rdt0~age,data=dat1,pch=19,col='blue')
points(pinf.rdt1~age,data=dat1,pch=19,col='red')
