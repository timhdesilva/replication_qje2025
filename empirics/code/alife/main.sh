set -e

# Run files
for script in 01_build.py 01_moments_capital.py 01_moments_wage.py 01_occupation.py 02_moments_debt.py; do
    python "$script"
done
stata -b do 02_moments_capital_yrfe.do
