# BATCHTOOLS
# 6: submit jobs

# set resources based on timing tests
resources <- list(walltime='12:00:00',ncpus=1,memory='20000m',measure.memory=T)
nchunks <- 20
# submit jobs
submitJobs(data.table::data.table(findJobs(),chunk=1:nchunks),
           resources=resources)
# check on jobs
getStatus()
