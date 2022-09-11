#!/bin/bash

# set the number of nodes
#SBATCH --nodes=1

# set the number of CPU cores per node
#SBATCH --ntasks-per-node 2

# How much memory is needed (per node)
#SBATCH --mem=40GB

# set a partition
#SBATCH --partition normal

# set max wallclock time
#SBATCH --time=90:00:00

# set name of job
#SBATCH --job-name=cast_3000

# mail alert at start, end and abortion of execution
#SBATCH --mail-type=ALL

# set an output file
#SBATCH --output output.dat

# send mail to this address
#SBATCH --mail-user=jbahlmann@uni-muenster.de

# run the application
module add palma/2020b
module add foss R GDAL
R CMD BATCH --vanilla CV_nndm_3000.R
