set -e

# Check arguments
if [ $# -lt 2 ]; then
    echo "You need to pass at least two arguments!"
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
loc=$2
if [ $loc -eq 1 ]; then
    echo "Location: local"
elif [ $loc -eq 2 ]; then
    echo "Location: SuperCloud"
fi
cores=${3:-1}
echo "Number of cores: $cores"
debug=${4:-0}
if [ $debug -eq 0 ]; then
    echo "Debug flags: OFF"
else
    echo "Debug flags: ON"
fi
strictfp=${5:-0}
if [ $strictfp -eq 0 ]; then
    echo "FP Model: FAST"
else
    echo "FP Model: STRICT"
fi

# Clean environment and files
./clean.sh
fprettify *.f90

# Load compiler
if [ $loc -eq 1 ]; then
    COMP="gfortran"
elif [ $loc -eq 2 ]; then
    export TMPDIR=/state/partition1/user/$USER
    module load intel-oneapi/2023.1
    COMP="mpiifort"
fi

# Compiler options
OPTIONS=" -cpp"
if [ $debug -gt 0 ]; then
    if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
        OPTIONS="${OPTIONS} -C -g -traceback -check noarg_temp_created"
    else
        OPTIONS="${OPTIONS} -O0 -C -g -fbacktrace -fcheck=all -Wall -Wextra -finit-real=snan -fsignaling-nans -ffpe-trap=invalid,zero,overflow"
    fi
else
    if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
        OPTIONS="${OPTIONS} -Ofast"
    else
        OPTIONS="${OPTIONS} -O3"
    fi
fi
if [ $cores -gt 1 ] && [ $loc -eq 1 ]; then
    if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
        OPTIONS="${OPTIONS} -qopenmp"
    else
        OPTIONS="${OPTIONS} -fopenmp"
    fi
elif [ $loc -gt 1 ]; then
    OPTIONS="${OPTIONS} -DMPI"
fi

# Hybrid OpenMP and MPI on SuperCloud via double-threading
if [ $cores -gt 1 ] && [ $loc -eq 2 ]; then
    if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
        OPTIONS="${OPTIONS} -qopenmp"
    else
        OPTIONS="${OPTIONS} -fopenmp"
    fi
fi

# Additional ifort options that need to be adjusted (can in principle run without this)
if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
    OPTIONS="${OPTIONS} -heap-arrays"
    if [ $loc -gt 1 ]; then
        OPTIONS="${OPTIONS} -mcmodel=large"
    fi
fi

# Strict floating point model
if [ $strictfp -gt 0 ]; then
    if [ $COMP == "ifort" ] || [ $COMP == "mpiifort" ]; then
            OPTIONS="${OPTIONS} -fp-model=strict"
        fi
fi

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
if [ $loc -eq 1 ]; then
    export OMP_NUM_THREADS=$cores
    ./a.out
    if [ $mode -eq 0 ]; then
        python ToStata_GS.py
    elif [ $mode -eq 4 ]; then
        python ToPcl_ComparePolicies.py
    elif [ $mode -eq 8 ]; then
        python ToPcl_OptimalPolicy.py
    fi
elif [ $loc -eq 2 ]; then
    if [ $mode -eq 0 ]; then
        n_per_task=4 # number of CPUs needed when LCSimPanel=1 to avoid memory overflow
        n_tasks=$(( (cores + n_per_task - 1) / n_per_task ))
        job1=$(sbatch --export=mode=$mode --ntasks=$n_tasks --cpus-per-task=$n_per_task SuperCloud.slurm | awk '{print $4}')
        sbatch --dependency=afterok:$job1 Mode0_SuperCloud.slurm
    fi
    module purge
fi
