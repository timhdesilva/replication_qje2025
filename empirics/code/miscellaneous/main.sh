set -e

# Run files
for script in 01_mortality.py 01_rates_dollars.py 01_smoothed_tax.py; do
    python "$script"
done
