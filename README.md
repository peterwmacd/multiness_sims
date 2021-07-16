# multiness_sims
Simulations for MULTIplex NEtworks with Shared Structure

All code evaluates low-rank and latent space methods for multiplex networks, based on synthetic data generated using the development R package "multiness". It will produce results and plots matching those in the pre-print "Latent space models for multiplex networks with shared structure" [arxiv](https://arxiv.org/abs/2012.14409).

The code is written to run on an HPC cluster using the R package "batchtools". Additional configuration may be required.

## How to run

R code files are to be run in sequence from 1 to 8. The subfolder "code_to_source" defines some additional wrapper functions for batchtools. "results.RData" and "plots" contain the final results and plots used in the pre-print. "0_reload.R" will reload the batchtools registry after restarting R.

- "1_makereg.R" creates the batchtools registry and installs required packages. By default it is placed in a folder "~/multiness_sims/".

- "2_problems.R" defines the synthetic data for each study based on wrapper functions in "code_to_source".

- "3_algorithms.R" defines the methods to be compared based on wrapper functions in "code_to_source".

- "4_makejobs.R" creates the jobs for each study based on combinations of problems and algorithms.

- "5_testjobs.R" tests the jobs to calibrate timing.

- "6_submitjobs.R" submits the jobs to the cluster in 20 chunks.

- "7_results.R" summarizes and saves results and summaries for each study.

- "8_plots.R" produces plots matching those in the pre-print.
