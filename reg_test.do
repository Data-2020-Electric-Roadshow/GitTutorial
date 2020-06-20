cd "E:\Data Plus\data\raw\From REA\Append Data"
use "stop_dist_merged_3843", clear
merge 1:1 system year using "singleyear_var_4143", keepusing(st_code state city county zip system sys_no op_kwh_p op_kwh_b allo_all re_opm op_ccn cons_aam)
drop if _m==2
drop _m
sort system year
** We propose two ways to measure kwh consumed per residential customer
* 1) For 1938-1940, use (original indicator in report * 12); For 1941-1943, use kwh_billed/consumer connected.
* 2) original indicator in report * 12 (op_kwh_pr*12)
* Note that in the report, kwh_billed is a "flow" variable, while consumer connected is a commulative variable
gen kwh_per_cc1= op_kwh_b/op_ccn
replace kwh_per_cc1= op_kwh_pr*12 if year<1941
replace kwh_per_cc1= op_kwh_pr*12 if year>=1941 & kwh_per_cc1==.
gen kwh_per_cc2= op_kwh_pr*12
* since there are some outliers, shrink upper way a little for the first indicator at the 98% threshold
winsor2 kwh_per_cc1, replace cut(0 97.5) trim
winsor2 kwh_per_cc2, replace cut(0 98.5) trim
* generate some log-scale indicator
gen ln_dist= ln(min_stop_dist)
gen ln_kwh1= ln(kwh_per_cc1)
gen ln_kwh2= ln(kwh_per_cc2)
* generate code indicating state, county or city
encode county, gen(county_code)
encode city, gen(city_code)
encode state, gen(state_code)
* regression experiment
set matsize 11000
** As a start, we use miles energized as the control variable
/* global control "" we should find a list of reasonable control variables*/
* only add county FE
qui: reghdfe kwh_per_cc1 min_stop_dist, absorb(year county)
est store a1
* add county + year FE
qui: reghdfe kwh_per_cc1 min_stop_dist i.state_code#c.year, absorb(year county)
est store a2
* cluster standard error on state level
qui: reghdfe kwh_per_cc1 min_stop_dist i.state_code#c.year, absorb(year county) cluster(state)
est store a3
* add control variable miles energized
qui: reghdfe kwh_per_cc1 min_stop_dist i.state_code#c.year op_me, absorb(year county) cluster(state)
est store a4
* remove the state in 1940 TVA territory, according to Kithchens and Fishback(2015)
gen TVA= 0
replace TVA =1 if state=="Alabama"|state=="Mississippi"|state=="Tennessee"
qui: reghdfe kwh_per_cc1 min_stop_dist i.state_code#c.year op_me if TVA==0, absorb(year county) cluster(state)
est store a5
* using proxy variable as a robust test
qui: reghdfe kwh_per_cc2 min_stop_dist i.state_code#c.year op_me, absorb(year county) cluster(state)
est store a6
* elasticity
qui: reghdfe ln_kwh1 ln_dist i.state_code#c.year, absorb(year county) cluster(state)
est store a7

esttab a1 a2 a3 a4 a5 a6 a7 using "reg test\Baseline.rtf" ,replace se ar2 nogap/*
  */ keep(min_stop_dist op_me ln_dist)/*
  */ order(min_stop_dist op_me ln_dist)/*
  */ scalars(N_clust) sfmt(%9.0g %9.3g %9.0g) star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) 

clear
