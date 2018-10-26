# can be sourced, needs to be run on server
library(kyotil)
proj="performance"

settings=c("gridC","fastgrid","fastgrid2"); names(settings)=settings
reses=lapply (settings, function(sim.setting) get.sim.res("res_performance/gaussian_"%.%sim.setting%.%"_upperhinge/", verbose=F))

# table
tabs=sapply (reses, function(res) {
    apply(res, 1, function(x) formatDouble(mean(x),2) %.% " (" %.% formatDouble(sd(x),2) %.% ")" )
})
tabs

tab=cbind(c(tabs[[1]],NA), tabs[[2]], tabs[[3]])
colnames(tab)=names(tabs)# this sequence needs to match the above
rownames(tab)=names(tabs[[3]]) # this is needed so that 2000 is present
names(dimnames(tab))=c("$n$",NA); tab

if(!file.exists("tables")) dir.create("tables")
mytex(tab, file="tables/fastgrid2_performance")


## figure
#dat=sapply (reses, function(res) apply(res, 1, mean)); dat
#tab=cbind(c(dat[[2]],NA), dat[[1]], dat[[3]])
#colnames(tab)=names(dat)[c(2,1,3)]# this sequence needs to match the above
#rownames(tab)=names(dat[[1]]) # this is needed so that 2000 is present
#mypdf(file="figures/performance")
#    mymatplot(x=as.numeric(rownames(tab)), tab, legend.x=3, ylab="time (sec)", log="xy", col=c("darkgray","darkgray","black"), xlab=expression(italic(n)), lty=c(1,2,1), lwd=1.5, xaxt = "n", yaxt="n")    
#    labelsY=parse(text=paste(10, c("^-1","^0","^1","^2","^3"), sep=""))
#    labelsY[1:5]=c(0.1,1,10,100,1000) 
#    axis(1, labels=rownames(dat), at=as.numeric(rownames(dat)))
#    axis(2, 10**(-1:3), labels=labelsY, las=1)    
#dev.off()
