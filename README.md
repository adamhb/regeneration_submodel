# The Tree Recruitment Submodel

The Tree Recruitment Submodel is designed to predict environmentally sensitive tree recruitment rates within Vegetation Demographic Models (VDMs). 

The submodel is run within forest patches at Barro Colorado Island using output from ED2.

This repository contains:

1) scripts to prepare data to run the submodel ("clean_input")
2) the submodel and its functions ("model")
3) the submodel's parameters ("parameter_files")
4) scripts to run the submodel ("runs")
5) analyses used to formulate the submodel and parameterize it for BCI ("model_dev")
6) scripts used to derive empirical benchmarks and assign species to PFTs ("benchmarking")
7) scripts used to generate output figures
8) supporting functions and data ("temp" and "utils")

To run the submodel:

1) Download driver data and supporting data
2) Change input and output paths
3) Choose and scenario in the "run" folder and source the script

Model output will appear in the output path.

