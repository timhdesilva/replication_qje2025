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
if [ $mode -eq 0 ]; then
    python3 ToStata_GS.py
elif [ $mode -eq 3 ]; then
    python3 ToPcl_BunchDecomp.py
elif [ $mode -eq 4 ]; then
    python3 ToPcl_ComparePolicies.py
elif [ $mode -eq 5 ]; then
    python3 ToPcl_ComparePolicies_BB.py
elif [ $mode -eq 6 ]; then
    python3 ToPcl_ComputeMH.py
elif [ $mode -eq 7 ]; then
    python3 ToPcl_LafferCurves.py
elif [ $mode -eq 8 -o $mode -eq 10 ]; then
    python3 ToPcl_OptimalPolicy.py
elif [ $mode -eq 12 ]; then
    python3 ToStata_Restructure.py
elif [ $mode -eq 13 ]; then
    python3 ToPcl_OptimalPolicy_Redist.py
fi
module purge
