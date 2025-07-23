set -e

# Run files
for script in 01_build.py 02_bunching.py 02_bunching_heterogeneity.py 02_moments_bunching.py 02_moments_panel.py 02_moments_ratios.py 02_occupation.py 02_summarystats.py; do
    python "$script"
done
