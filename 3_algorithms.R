# BATCHTOOLS
# 3: algorithms

# add algorithms
# algorithm function 'fun' defined in source files or packages
addAlgorithm(name = "alt_svd", fun = alt_svd_wrapper)
addAlgorithm(name = "grad_factor_mult", fun = grad_factor_mult_wrapper)
addAlgorithm(name = "multiness", fun = multiness_fit_wrapper)
addAlgorithm(name = "cosie", fun=cosie_wrapper)
addAlgorithm(name = "mgraf", fun=mgraf_wrapper)

# algorithms should return a list of univariate metrics
