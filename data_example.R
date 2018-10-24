library(chngpt)
library(kyotil)
tabs=list()
for (i in 1:2) {    
    if(i==1) {
        dat=dat.mtct.2
    } else {
        set.seed(4) 
        n=50
        dat=dat.mtct.2[sample.int(nrow(dat.mtct.2),n),]
    }
    
    formula.1=V3_BioV3B~1; formula.2=~NAb_score
    
    fit.s=chngptm (formula.1, formula.2, dat, type="segmented", family="gaussian", est.method="fastgrid2", var.type="bootstrap", verbose=0, save.boot=T, lb.quantile=0.04)
    summary(fit.s, show.slope.post.threshold=T)    
    fit.u=chngptm (formula.1, formula.2, dat, type="upperhinge", family="gaussian", est.method="fastgrid2", var.type="bootstrap", verbose=0, save.boot=T, lb.quantile=0.04)
    summary(fit.u)
    
    tab.s=getFormattedSummary(list(fit.s), show.slope.post.threshold=T,  type=3, est.digits=c(1,1,1,2), se.digits=c(1,1,1,2), exp=T)     
    rownames(tab.s)[2:4]=c("before threshold","after threshold","threshold")    
    tab.u=getFormattedSummary(list(fit.u), type=3, est.digits=c(1,1,2), se.digits=c(1,1,2), exp=T) 
    rownames(tab.u)[2:3]=c("before threshold","threshold")    
    tab = cbinduneven(list("upper hinge"=tab.u, "segmented"=tab.s))   
    print(tab) 
    tabs[[i]]=tab
    
    myfigure(mfcol=c(2,2), height=5.4, width=8, mar=c(4.1, 4.1, 2.1, 2.1))
        adj=-0.08
        xlim=c(.4,.65)
        
        tmp=plot(fit.u, which=1, xlim=xlim)
        plot(fit.u, which=3, xlim=xlim, breaks=15)
            
        tmp=plot(fit.s, which=1, xlim=xlim)
        plot(fit.s, which=3, xlim=xlim, breaks=15)
}
tab=cbind(tabs[[1]], tabs[[2]])
tab
