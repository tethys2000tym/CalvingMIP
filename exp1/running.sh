#!/bin/bash

#SBATCH --job-name=exp1
#SBATCH --output=output_test1.txt
#SBATCH --error=errors_test1.txt
#SBATCH -N 1

#SBATCH -n 24
#SBATCH -p node

#SBATCH -A tianym

cd $SLURM_SUBMIT_DIR


#export LD_LIBRARY_PATH=/home/tianym/CALVINGMIP_TEST_CONFIG/PROG:$LD_LIBRARY_PATH
#export LD_LIBRARY_PATH= $LD_LIBRARY_PATH:/geo/apps/elmerice230913_gcc/lib
mpirun -np 24 ElmerSolver_mpi
#mpirun -np 24 --mca btl_openib_allow_ib true ElmerSolver_mpi
#ElmerSolver

