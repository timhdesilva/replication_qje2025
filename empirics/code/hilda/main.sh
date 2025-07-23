set -e

# Run files
for script in 01_calibrate_A0.py 01_calibrate_borrowing_constraint.py 01_calibrate_eqscale.py 01_compute_hours_moments.py 01_make_ALife_inputs.py; do
    python "$script"
done
