set -e

# Check arguments
if [ $# -ne 2 ]; then
    echo "You need to pass two arguments: <mode> <number of cores>"
    exit 1
fi

# Assign arguments and print
mode=$1
if [ $mode -eq 0 ]; then
    echo "Mode: Grid-Search"
elif [ $mode -eq 1 ]; then
    echo "Mode: SMM"
elif [ $mode -eq 2 ]; then
    echo "Mode: Standard Errors"
elif [ $mode -eq 4 ]; then
    echo "Mode: Compare Policies"
elif [ $mode -eq 8 ]; then
    echo "Mode: Optimal Policy"
fi
cores=$2
echo "Number of cores: $cores"

# Clean environment and files
./clean.sh
fprettify *.f90

# Load compiler
export TMPDIR=/state/partition1/user/$USER
module load intel-oneapi/2023.1
COMP="mpiifort"

# Compiler options
OPTIONS=" -cpp"
OPTIONS="${OPTIONS} -O3"
OPTIONS="${OPTIONS} -DMPI"
OPTIONS="${OPTIONS} -heap-arrays"
OPTIONS="${OPTIONS} -mcmodel=large"

# Compile
COMP="${COMP} ${OPTIONS}
Parameters.f90
types.f90
svd.f90
Procedures.f90
random.f90
EconFunctions.f90
Setup.f90
OptParameters.f90
NelderMead.f90
GoldenSection.f90
EVf.f90
OptimizeWork.f90
OptimizeRet.f90
ValueFunction.f90
Simulations.f90
SMMFunctions.f90
"
if [ $mode -eq 0 ]; then
    COMP="${COMP} Main_GS.f90"
elif [ $mode -eq 1 ]; then
    COMP="${COMP} amoeba.f90 TikTak.f90 Main_SMM.f90"
elif [ $mode -eq 2 ]; then
    COMP="${COMP} Main_SE.f90"
elif [ $mode -eq 4 ]; then
    COMP="${COMP} Main_ComparePolicies.f90"
elif [ $mode -eq 8 ]; then
    COMP="${COMP} amoeba.f90 TikTak.f90 Main_OptimalPolicy.f90"
fi
echo $COMP
$COMP

# Run
sbatch --export=mode=$mode --ntasks=$cores job.slurm
module purge
