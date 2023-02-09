# Identifying predictors of sensorimotor adaption with 180,000 reaches


Hello! This repository contains the code used to analyze visuo-motor adaptation data collected on [testmybrain.org](https://testmybrain.org/). This code was written and is maintained by Hrach Asmerian (hrach.asmerian@berkeley.edu) and Jonathan Sanching Tsay (xiaotsay2015@gmail.com).


### Adaptation_reach_plots.Rmd

This file contains reach plots stratified by various demographic and behavioral variables as noted on in the paper. Figures are generated primarily using [ggplot2](https://ggplot2.tidyverse.org/), and subsequent plots are generated using the core plot p1. The document outline covers which plots are stratified by which variables.


### LASSO_regression_and_figures.Rmd

This file performs LASSO regression predicting adaptation metrics using demographic and behavioral measures using the cv.gglasso function in [gglasso](https://www.rdocumentation.org/packages/gglasso/versions/1.5/topics/cv.gglasso) (version 1.5).
This script also contains the "Shuffler" function which creates permutations of the supplied data set "X" number of times and plots actual model performance against models built from randomly permuted data. Beta coefficients from the models built on Early, Late and After-effect phases of adaptation are plotted, as well as adaptation vs. age figures.

### testmybrain_data_30Dec2022_part_#.csv

The original [testmybrain.org](https://testmybrain.org/) data set has been split to be able to store within this repository. These splits are bound back together within the aforementioned Rmd files.
