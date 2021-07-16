# BATCHTOOLS
# 4: make jobs

# gauss/log compare over n (5 settings)
pn_gaussian <- list(multiness_data = data.table::data.table(n=100*2:6,
                                              m=8,
                                              d1=2,d2=2,
                                              model='gaussian',
                                              dens=0,rho=0))
pn_logistic <- list(multiness_data = data.table::data.table(n=100*2:6,
                                              m=8,
                                              d1=2,d2=2,
                                              model='logistic',
                                              dens=0,rho=0))

# gauss/log compare over m (6 settings)
pm_gaussian <- list(multiness_data = data.table::data.table(n=400,
                                                m=c(4,8,12,15,20,30),
                                                d1=2,d2=2,
                                                model='gaussian',
                                                dens=0,rho=0))
pm_logistic <- list(multiness_data = data.table::data.table(n=400,
                                                m=c(4,8,12,15,20,30),
                                                d1=2,d2=2,
                                                model='logistic',
                                                dens=0,rho=0))

# gauss/log compare over d2 (d1=0) (6 settings)
pd2_gaussian <- list(multiness_data = data.table::data.table(n=400,
                                                m=8,
                                                d1=0,d2=2*1:6,
                                                model='gaussian',
                                                dens=0,rho=0))
pd2_logistic <- list(multiness_data = data.table::data.table(n=400,
                                                m=8,
                                                d1=0,d2=2*1:6,
                                                model='logistic',
                                                dens=0,rho=0))

# gauss/log compare over d1 (d2=0) (6 settings)
pd1_gaussian <- list(multiness_data = data.table::data.table(n=400,
                                                 m=8,
                                                 d1=2*1:6,d2=0,
                                                 model='gaussian',
                                                 dens=0,rho=0))
pd1_logistic <- list(multiness_data = data.table::data.table(n=400,
                                                 m=8,
                                                 d1=2*1:6,d2=0,
                                                 model='logistic',
                                                 dens=0,rho=0))

# gaussian compare over large m/rho (40 settings)
p_large <- list(multiness_data = data.table::CJ(n=200,
                                                 m=c(20,50,100,150,200,300,400,600,800,1000),
                                                 d1=2,d2=2,
                                                 model='gaussian',
                                                 dens=0,rho=c(0.2,0.4,0.6,0.8)))

# logistic compare over densities (7 settings)
p_dens <- list(multiness_data = data.table::data.table(n=400,
                                                       m=8,
                                                       d1=2,d2=2,
                                                       model='logistic',
                                                       dens=0:6,rho=0))

# algorithm designs

# algorithms for gaussian designs (4 methods)
a_gaussian <- list(alt_svd=data.table::data.table(),
                   cosie=data.table::data.table(),
                   multiness=data.table::data.table(refit=c(F,T)))

# algorithms for logistic designs (5 methods)
a_logistic <- list(grad_factor_mult=data.table::data.table(),
                   cosie=data.table::data.table(),
                   mgraf=data.table::data.table(),
                   multiness=data.table::data.table(refit=c(F,T)))

# algorithms for logistic designs, zero dimensions (4 methods)
a_logistic_zdim <- list(grad_factor_mult=data.table::data.table(),
                   cosie=data.table::data.table(),
                   multiness=data.table::data.table(refit=c(F,T)))


# algorithms for gaussian design, large m (1 method)
a_large <- list(multiness=data.table::data.table(refit=F))

# add experiments
# reps for small m experiments
nreps <- 100
# reps for large m experiments
nreps_large <- 20
# experiments - 22900 total
# normal experiments (9200 - 400 dups = 8800)
addExperiments(pn_gaussian,a_gaussian,repls=nreps) # 2000
addExperiments(pm_gaussian,a_gaussian,repls=nreps) # 2400 - 400 dups
addExperiments(pd1_gaussian,a_gaussian,repls=nreps) # 2400
addExperiments(pd2_gaussian,a_gaussian,repls=nreps) # 2400

# logit experiments (13800 - 500 dups = 13300)
addExperiments(pn_logistic,a_logistic,repls=nreps) # 2500
addExperiments(pm_logistic,a_logistic,repls=nreps) # 2500 - 500 dups
addExperiments(pd1_logistic,a_logistic_zdim,repls=nreps) # 2400
addExperiments(pd2_logistic,a_logistic_zdim,repls=nreps) # 2400
addExperiments(p_dens,a_logistic,repls=nreps) # 3500

# large experiments (800)
addExperiments(p_large,a_large,repls=nreps_large)
