# when adding a new est.method, e.g. sigmoid2b, needs to update this file, coef.0.ls.R, and runscript file. Also make sure sim.alphas has the right entry or alpha is provided in call to sim.chngpt
library(kyotil)
library(chngpt)

# process arguments
Args <- commandArgs(trailingOnly=TRUE) 
if (length(Args)==0) {
    # est.method: gridC  fastgrid  smoothapprox
    # model.setting: type of threshold effects, e.g. hinge_stratified
    # method: all/all0/all3/none/bootstrap/bootstrapmn. Difference between all/all0/all3 are that different models are used for fit.aux
    # proj: folder name for saving simulation results
    Args=c(batch.size="1",batch.number="1",est.method="fastgrid2",model.setting="upperhinge",proj="performance") 
}
myprint(Args)
i=0;
i=i+1; batch.size=as.numeric(Args[i])
i=i+1; batch=as.numeric(Args[i])
seeds=1:batch.size+batch.size*(batch-1); names(seeds)=seeds
myprint(batch, batch.size)
i=i+1; est.method=Args[i]
i=i+1; model.setting=Args[i]
tmp=strsplit(model.setting,"_")[[1]]
type=tmp[[1]]
stratified=any(tmp=="stratified")
i=i+1; proj=Args[i]
# extract information from "est.method"
# more params
verbose=ifelse(unix(),0,2)
seed=1 

ci.bootstrap.size=1e3
family="gaussian"
b.transition=Inf; verbose=0
if (est.method=="gridC" | est.method=="grid") nn=c(50,100,250,500) else nn=c(50,100,250,500,2000); names(nn)=nn
myprint(est.method)

begin=Sys.time()
res=
sapply(seeds, simplify="array", function (seed) {    myprint(seed)
sapply(nn, simplify="array", function (n) {          myprint(n)
    
    t.0=Sys.time()   
    
    dat=sim.chngpt(mean.model="thresholded", n=n, seed=seed, threshold.type=type, x.distr="norm", family=family, e.=4.5, b.transition=b.transition, beta=log(2.5), verbose=verbose, alpha=-0.700347)    
    
    formula.strat=NULL
    if(stratified) formula.strat=~I(z>0)
    
    out=system.time(chngptm (formula.1=y~z, formula.2=~x, data=dat, type=type, verbose=verbose, 
        formula.strat=formula.strat,
        family=family, search.bound=10, tol=1e-5, b.transition=b.transition, 
        ci.bootstrap.size=ci.bootstrap.size, 
        boot.test.inv.ci=FALSE, test.inv.ci=FALSE,
        est.method=est.method, var.type="bootstrap"
    ))["elapsed"]    
    if(verbose) print(out)
    
    #eval(eval(substitute(expression( res.len <<- length(res) ))))     # set res.len to be used outside the current environment
    #gc()# there are some memory problem, seem to quit automatically
    if(verbose) print("time used: "%.%format(Sys.time()-t.0))        
    unname(out)
                
})})
if (!is.null(dimnames(res))) names(dimnames(res))=c("n","seed")


# save results
foldername="res_"%.%proj%.%"/"; if(!file.exists(foldername)) dir.create(foldername)
foldername=foldername%.%family%.%"_"%.%est.method%.%"_"%.%model.setting %.%"/"; if(!file.exists(foldername)) dir.create(foldername)
save (res, file=foldername%.%"/batch"%.%formatInt(batch, 3)%.%".Rdata")
# note time passed
done = Sys.time()
body1=format(done-begin)
print(date())
print("time used: "%.%body1)
