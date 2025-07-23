# Replication Code for QJE 2025 Student Debt Paper

This directory contains empirical analysis code for replicating the results in the 2025 Quarterly Journal of Economics paper on student debt.

## Directory Structure

```
empirics/
├── code/
│   ├── alife/                 # Analysis using Australian Longitudinal Individual File (ALife) data
│   ├── alife_fullsample/      # Full sample ALife analysis
│   ├── datalab/               # Data laboratory analysis
│   ├── hilda/                 # HILDA survey data analysis
│   └── miscellaneous/         # Additional analysis scripts
├── data/
│   └── mortality_tables/      # Mortality data for calibration
└── README.md
```

## Prerequisites

### Software Requirements
- Python 3.x with the following packages:
  - pandas
  - numpy
  - matplotlib
  - sklearn
  - statsmodels
  - patsy
  - cycler
  - dask (for datalab analysis)
- Stata (for some analysis files)
- R (may be required for some data files)

### Data Requirements
Before running any analysis, you must:

1. **Set up data directories**: Edit the `directories.py` file in each code subdirectory to point to your local data locations.

2. **Obtain required datasets**:
   - Australian Longitudinal Individual File (ALife) tax and superannuation data
   - HELP debt balance files
   - HILDA survey data
   - Additional auxiliary datasets as specified in individual scripts

## Setup Instructions

1. **Configure directories**: 
   - Edit `code/alife/directories.py` to set paths for ALife data files
   - Edit `code/alife_fullsample/directories.py` for full sample analysis
   - Edit `code/hilda/directories.py` for HILDA data paths
   - Edit other `directories.py` files as needed

2. **Install Python dependencies**:
   ```bash
   pip install -r requirements.txt
   ```

3. **Verify data file locations**: Ensure all data files are accessible at the paths specified in the directories.py files.

## Running the Analysis

Each code subdirectory contains a `main.sh` script that runs all Python files in the correct order:

```bash
# Run ALife analysis
cd code/alife
bash main.sh

# Run full sample analysis
cd code/alife_fullsample  
bash main.sh

# Run HILDA analysis
cd code/hilda
bash main.sh

# Run other analyses
cd code/datalab
bash main.sh

cd code/miscellaneous
bash main.sh
```

## File Descriptions

### ALife Analysis (`code/alife/`)
- `01_build.py`: Constructs analysis dataset from ALife tax and super files
- `01_moments_capital.py`: Calculates capital income moments
- `01_moments_wage.py`: Calculates wage income moments  
- `01_occupation.py`: Occupation-based analysis
- `02_moments_debt.py`: HELP debt moments analysis
- `02_moments_capital_yrfe.do`: Stata script for year fixed effects analysis

### HILDA Analysis (`code/hilda/`)
- `01_calibrate_A0.py`: Calibrates initial asset parameter
- `01_calibrate_borrowing_constraint.py`: Borrowing constraint calibration
- `01_calibrate_eqscale.py`: Equivalence scale calibration
- `01_compute_hours_moments.py`: Hours worked moments
- `01_make_ALife_inputs.py`: Creates inputs for ALife analysis

### Support Files
- `fxns_globals.py`: Global variables and utility functions
- `fxns_HELPpayment.py`: HELP payment calculation functions
- `fxns_bootstrap.py`: Bootstrap analysis functions
- `fxns_bunching.py`: Bunching analysis functions

## Notes

- All file paths in `directories.py` files must be set before running
- Some scripts require specific data file formats and variable names
- The analysis assumes Australian tax and education loan system parameters
- Results are saved to the directories specified in the `directories.py` configuration files

## Troubleshooting

If you encounter errors:
1. Verify all paths in `directories.py` files are correct
2. Check that all required data files exist
3. Ensure all Python dependencies are installed
4. Make sure you're running scripts from the correct directory