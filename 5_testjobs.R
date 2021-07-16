# BATCHTOOLS
# 5: test jobs

# basic summary (by problem and algorithm)
summarizeExperiments()

# quick functions for job parameters/timed testing
ppar <- function(id){
    print(unwrap(getJobPars(id)))
}

ptest <- function(id){
    system.time(print(testJob(id)))
}

# timing/testing for each method/setting

# multiness (w/refit step)
# gaussian (4400)
# largest n
ppar(2000)
ptest(2000) # 15 seconds
# largest m
ppar(4000)
ptest(4000) # 15 seconds
# zero d2
ppar(6000)
ptest(6000) # 5 seconds
# zero d1
ppar(8000)
ptest(8000) # 5 seconds
# logistic (5800)
# largest n
ppar(11300)
ptest(11300) # 22 seconds
# largest m
ppar(13800)
ptest(13800) # 30 seconds
# zero d2
ppar(15800)
ptest(15800) # 12 seconds
# zero d1
ppar(17800)
ptest(17800)
# low density
ppar(22100)
ptest(22100) # 40 seconds
# low density no refit (check K > 100)
ppar(22000)
ptest(22000)

# baseline/svd
# gaussian (2200)
# largest n
ppar(500)
ptest(500) # 15 seconds
# largest m
ppar(2500)
ptest(2500) # 5 seconds
# zero d2
ppar(4500)
ptest(4500) # 1 second
# zero d1
ppar(6500)
ptest(6500) # 1 second
# logistic (2900)
# largest n
ppar(9300)
ptest(9300) # 30 seconds
# largest m
ppar(11800)
ptest(11800) # 30 seconds
# low density
ppar(19300)
ptest(19300) # 60 seconds

# cosie (2200 + 2900 = 5100)
# gaussian
# largest n
ppar(1000)
ptest(1000) # 1 second
# largest m
ppar(3000)
ptest(3000) # 3 seconds
# zero d1
ppar(5000)
ptest(5000)
# zero d2
ppar(7100)
ptest(7100)
# largest m (logistic)
ppar(12300)
ptest(12300)

# mgraf (1700)
# largest n
ppar(10300)
ptest(10300) # 45 seconds
# largest m
ppar(12800)
ptest(12800) # 65 seconds
# low density
# dens = 4
ppar(20500)
ptest(20500)
# dens = 5
ppar(20600)
ptest(20600)
# dens =6
ppar(20700)
ptest(20700)

# large scale (800)
# small m
ppar(22300)
ptest(22300)
# largest m
ppar(22860)
ptest(22860) # 180 seconds

# SUMMARY:
# mn gauss = 4400*15 = 19 hours
# mn log = 5800*40 = 65 hours
# svd gauss = 2200*15 = 10 hours
# gfm log = 2900*60 = 48 hours
# cosie = 5100*3 = 4 hours
# mgraf = 1700*65 = 31 hours
# large m = 800*180 = 40 hours

# total  217 hours / 20 chunks =  12 hours per chunk
# NOTE: nchunks should divide nreps so chunk timing is approx. equal

