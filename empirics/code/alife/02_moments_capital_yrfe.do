python:
import sys
sys.path.append(".")
from directories import datadir, momdir, sedir
end

python:
sfi.Macro.setLocal("datadir", datadir.replace("\\", "/"))
sfi.Macro.setLocal("momdir", momdir.replace("\\", "/"))
sfi.Macro.setLocal("sedir", sedir.replace("\\", "/"))
end

use "`datadir'/capitalpanel_1991_2019.dta", replace

replace age = 5 * floor(age / 5)
gen cohort = year - age + 22
keep if cohort >= 1963

xi: reghdfe capital_income i.age, a(year)

gen ae = _b[_cons]
gen ae_se = _se[_cons]
forvalues i = 25(5)60 {
	replace ae = ae + _b[_Iage_`i'] if age == `i'
	replace ae_se = sqrt(ae_se^2 + _se[_Iage_`i']^2) if age == `i'
}

collapse (first) ae ae_se, by(age)
sort age
export delimited ae using "`momdir'/ageprofile_yrfe_capital.txt", novar replace
export delimited ae_se using "`sedir'/ageprofile_yrfe_capital.txt", novar replace
