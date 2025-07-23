set -e

# Run files
for script in 01_build.py 02_plots.py; do
    python "$script"
done
