#!/bin/bash

#SBATCH --partition=debug-cpu
#SBATCH -o run.%j.out
#SBATCH --no-requeue
#SBATCH --mem=128000MB
#SBATCH --exclusive
#SBATCH --ntasks=1

# Print parameters
echo "Number of nodes: $SLURM_NNODES"
echo "Number of tasks: $SLURM_NTASKS"
echo "Number of CPUs per task: $SLURM_CPUS_PER_TASK"

# Convert outputs to .dta
module purge
module load anaconda/2023a
python ToStata_GS.py

echo "Finished!"
