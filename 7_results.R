# BATCHTOOLS
# 7: results

#### PART A: reduce results ####

# NULL to NA function for missing list entries
n2n <- function(x){ 
    ifelse(is.null(x),NA,x)
}

# summarize results
reduce <- function(res){list(Ferr=n2n(res$Ferr),
                             Gerr=n2n(res$Gerr),
                             Perr=n2n(res$Perr),
                             F_rank=n2n(res$F_rank),
                             G_rank=n2n(res$G_rank),
                             K=n2n(res$K))}
results <- unwrap(reduceResultsDataTable(ids=findDone(),fun = reduce))

# join job parameters
pars <- unwrap(getJobPars())
tab <- ijoin(pars, results)

#### PART B: full result tables ####

# summary tables
res_tables <- list()
length(res_tables) <- 10 # should match the number of job batches submitted
# identifiers for each batch of jobs 
# (constant columns in the problem and/or algorithm design)
templates <- list(data.table::data.table(m=8,rho=0,d1=2,d2=2,model="gaussian"),
                  data.table::data.table(n=400,rho=0,d1=2,d2=2,model="gaussian"),
                  data.table::data.table(n=400,m=8,rho=0,d2=0,model="gaussian"),
                  data.table::data.table(n=400,m=8,rho=0,d1=0,model="gaussian"),
                  data.table::data.table(m=8,rho=0,dens=0,d1=2,d2=2,model="logistic"),
                  data.table::data.table(n=400,rho=0,dens=0,d1=2,d2=2,model="logistic"),
                  data.table::data.table(n=400,m=8,rho=0,dens=0,d2=0,model="logistic"),
                  data.table::data.table(n=400,m=8,rho=0,dens=0,d1=0,model="logistic"),
                  data.table::data.table(n=400,m=8,rho=0,d1=2,d2=2,model="logistic"),
                  data.table::data.table(n=200,d1=2,d2=2,model="gaussian",algorithm="multiness"))
# varying columns in the problem and/or algorithm design
byvar <- list('n','m','d1','d2','n','m','d1','d2','dens',c('m','rho'))

for(ii in 1:length(res_tables)){
    # subset to each batch
    temp <- ijoin(templates[[ii]],tab)
    # summarize error metrics
    res_tables[[ii]] <- temp[,list(Ferr=mean(Ferr,na.rm=T),
                                   Gerr=mean(Gerr,na.rm=T),
                                   Perr=mean(Perr,na.rm=T),
                                   F_rank=mean(F_rank),
                                   G_rank=mean(G_rank/m),
                                   K_mean=mean(K),
                                   K_max=mean(as.integer(K==100+100*dens))),
                             by=c(byvar[[ii]],c("algorithm","refit"))]
    # additional information to identify result tables
    res_tables[[ii]]$model <- templates[[ii]]$model
}


#### PART C: plotting tables ####

# error metrics (independent variables)
yvars <- c("Ferr",
           "Gerr",
           "Perr")
# algorithm settings
algvars <- c("algorithm","refit")
plot_settings <- expand.grid(xvar=1:length(byvar),yvar=yvars)

# plotting tables (for matplot)
plot_tables <- list()
length(plot_tables) <- dim(plot_settings)[1]

for(ii in 1:length(plot_tables)){
    tt <- plot_settings[ii,1]
    if(length(byvar[[tt]])==1){
        plot_tables[[ii]] <- data.table::dcast(res_tables[[tt]],
                                               formula=paste0(byvar[[tt]],
                                                              "~",
                                                              paste(algvars,collapse="+")),
                                               value.var=as.character(plot_settings[ii,2]))
    }
    else{
        plot_tables[[ii]] <- data.table::dcast(res_tables[[tt]],
                                               formula=paste0(byvar[[tt]][1],
                                                              "~",
                                                              paste(c(byvar[[tt]][-1],algvars),collapse="+")),
                                               value.var=as.character(plot_settings[ii,2]))
    }
    # additional information to identify plot tables
    plot_tables[[ii]]$model <- templates[[tt]]$model
    plot_tables[[ii]]$yvar <- as.character(plot_settings[ii,2])
}

#### PART D: save ####

# save summary table to results
save(tab,res_tables,plot_tables,templates,file="results.RData")
