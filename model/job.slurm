#!/bin/bash

#SBATCH --partition=xeon-p8
#SBATCH -o run.%j.out
#SBATCH --no-requeue
#SBATCH --time=14-00:00
#SBATCH --mem-per-cpu=4000MB
#SBATCH --exclusive

# Print parameters
echo "Number of nodes: $SLURM_NNODES"
echo "Number of tasks: $SLURM_NTASKS"
echo "Number of CPUs per task: $SLURM_CPUS_PER_TASK"

# Double thread on each CPU
if [ $mode -eq 0 ] || [ $mode -eq 12 ]; then
    export OMP_NUM_THREADS=$(( 2 * SLURM_CPUS_PER_TASK ))
else
    export OMP_NUM_THREADS=2
fi

# Load MPI
module purge
module load intel-oneapi/2023.1

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
