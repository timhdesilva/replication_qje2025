#!/bin/bash

#SBATCH --partition=sched_mit_sloan_batch
#SBATCH -o run.%j.out
#SBATCH --no-requeue
#SBATCH --time=4-00:00
#SBATCH --mem-per-cpu=4000MB
#SBATCH --constraint="centos7"
#SBATCH --exclusive

# Print parameters
echo "Number of nodes: $SLURM_NNODES"
echo "Number of cores: $SLURM_NTASKS"

# Load MPI
module purge
module load intel/2020-04
module load impi/2020-04

# Run
ulimit -s unlimited
mpirun -np $SLURM_NTASKS ./a.out

# Clean
./clean.sh

# Convert outputs to .dta
module purge
module load anaconda3/2023.07
if [ $mode -eq 2 ]; then
    python POST_plot_fit.py
elif [ $mode -eq 4 ]; then
    python ToPcl_ComparePolicies.py
    python POST_results_existing.py
elif [ $mode -eq 8 ]; then
    python ToPcl_OptimalPolicy.py
    python POST_results_optimal.py
fi
module purge
