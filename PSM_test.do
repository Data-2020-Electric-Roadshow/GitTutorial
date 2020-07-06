****************************************************************************************
************* Step 0: Data Preproceesing and Merge with Census Data ********************
****************************************************************************************
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
gen kwh_per_cc2= op_kwh_pr*12
replace kwh_per_cc1= kwh_per_cc2 if year>=1941 & kwh_per_cc1==.
label variable kwh_per_cc1 "kwh_bill/number of customers (41-43) & kwh per customer*12 (38-40) "
label variable kwh_per_cc2 "kwh per customer*12 (38-43)"
* since there are some outliers, trim upper way a little for the first indicator at the 2.5% threshold
* generate some log-scale indicator
gen ln_dist= ln(min_stop_dist)
gen ln_kwh1= ln(kwh_per_cc1)
gen ln_kwh2= ln(kwh_per_cc2)
gen ln_mile= ln(op_me)
* generate code indicating state, county or city
encode county, gen(county_code)
encode city, gen(city_code)
encode state, gen(state_code)
egen system_code= group(state sys_no)
* merge with agriculture, demographic and political control dataset
gen id_master= _n
reclink state county using "E:\Data Plus\data\raw\ICPRS\demo_agri_3045", gen(simiscore) idm(id_master) idu(id_using)
drop simiscore id_using Ustate Ucounty _m
merge m:1 state using "E:\Data Plus\data\raw\ICPRS\Political Data\Strength Data_wideformat"
drop if _m==2
drop id_master id_using _m
* interpolate the census data, regarded as constant in 5-year period
gen ratio_croptolivestock_39= val_crop_39/val_livestock_39 
gen avg_farmsize= .
gen percent_white= .
gen farmer_pop= .
replace avg_farmsize= farmsize_35 if year < 1940
replace avg_farmsize= farmsize_40 if year >= 1940
replace percent_white= per_white_35 if year < 1940
replace percent_white= per_white_40 if year >= 1940
replace farmer_pop= farm_pop_35 if year < 1940
replace farmer_pop= farm_pop_40 if year >=1940
gen RCOMPB= .
gen DCOMPB= .
foreach var in RCOMPB DCOMPB{
replace `var'= `var'1938 if year>=1938 & year<=1939
replace `var'= `var'1940 if year>=1940 & year<=1941
replace `var'= `var'1942 if year>=1942 & year<=1943
}
foreach var in farmsize_35 farmsize_40 farm_pop_35 farm_pop_40 avg_farmsize farmer_pop electri_distri_line_40 hard_road_40{
gen ln_`var'= ln(`var')
}
* generate the lag term of covariates
gen percent_white_lag= .
gen ln_avg_farmsize_lag= .
gen ln_farmer_pop_lag= .
replace ln_avg_farmsize_lag= ln_farmsize_35 if year < 1941
replace ln_avg_farmsize_lag= ln_farmsize_40 if year >= 1941
replace percent_white_lag= per_white_35 if year < 1941
replace percent_white_lag= per_white_40 if year >= 1941
replace ln_farmer_pop_lag= ln_farm_pop_35 if year < 1941
replace ln_farmer_pop_lag= ln_farm_pop_40 if year >=1941
gen RCOMPB_lag= .
gen DCOMPB_lag= .
foreach var in RCOMPB DCOMPB{
replace `var'_lag= `var'1936 if year==1938
replace `var'_lag= `var'1938 if year>=1939 & year<=1940
replace `var'_lag= `var'1940 if year>=1941 & year<=1942
replace `var'_lag= `var'1942 if year==1943
}
* compute higher order terms
	foreach v2 of varlist dist_tolargecity ln_avg_farmsize_lag ln_farmer_pop_lag percent_white_lag ratio_croptolivestock_39 ln_electri_distri_line_40 ln_hard_road_40{
		egen tmp=mean(`v2')
		g `v2'_c=`v2'-tmp
		drop tmp
		g `v2'_sq=`v2'_c^2
	}
*
drop *_29 *_30 *_44 *_45 *46 *48 
duplicates drop system_code year, force
sort state sys_no year
compress
save "E:\Data Plus\data\PSM\Data\pre_data", replace


****************************************************************************
************* Step 1: Generate Treat and START Variable ********************
****************************************************************************
cd "E:\Data Plus\data\PSM"

// Threshold of treatment group (<=50 miles) Reason: to make sure each treat group can find sufficient control group, ///
// the distance of site to roadshow within 50 miles accounts for about 24%.
use "Data\pre_data", clear
xtset system_code year, yearly
gen after= 0
replace after= 1 if min_stop_dist <= 50 
label variable after "=1, if site i is located within 50 miles of roadshow stop in year t"
* some treatment sites are located within 50 miles of roadshow stop from the start of sample periods (i.e. year 1938), we generate a tag_label for these sites
bys system: egen min_after= min(after)
gen sys_always_treat= 1 if min_after== 1
drop min_after
label variable sys_always_treat "=1, if treatment sites are located within 50 miles of roadshow stop from the start of sample periods"
* drop the sites that only have 1-year observation (since these sites are "young", the data is not realible)
bys system: gen obs_nosite= _N
drop if obs_nosite==1
drop obs_nosite
* drop sites that have gaps on their data (year variable is not consecutive)
xtset system_code year
bysort system_code (year) : drop if _N < (year[_N] - year[1] + 1) 
* drop sites that don't have outcome variable (ln_kwh1) during the whole sample periods
bys system_code: egen maxkwh= max(ln_kwh1)
drop if  maxkwh==.
* generate leading variable (outcome variable)
xtset system_code year
foreach var of varlist kwh_per_cc1 ln_kwh1{
gen f`var'1= f1.`var'
gen f`var'2= f2.`var'
gen f`var'3= f3.`var'
}
* generate a variable which equals to 0 if the site is never located within 30 miles radius of a roadshow stop
bys system: egen max_after= max(after)
gen T= 1
replace T= 0 if max_after==0
drop max_after
label variable T "=0, site is never located within 50 miles radius of a roadshow stop during sample periods"
* generate start variable (START=1 only if site i starts being treated in that year)
xtset system_code year, yearly
sort system_code year
gen after_lag= l.after
gen after_diff= after-after_lag
gen START= 0
replace START= 1 if after_diff== 1 
// note: some sites are treated since the start year of sample
replace START= 1 if after_lag==.&after==1
* check if each site has only one start year
bys system_code: egen temp= total(START)
sum temp
drop temp
* ensure appropriate control group (su means summary)
gen temp2= year if START==1
bys system_code: egen temp3= min(temp2)
replace START= . if year>temp3
drop if START==. 
drop temp*
drop after_lag after_diff
label variable START "=1, site i starts being treated in that year"
save "Data\PSP_Sample_BeforeMatched_t0", replace

*****************************************************************************************************
************* Step 2: Use Logit Model to Estimate Propensity Score for each site ********************
*****************************************************************************************************
global syvar "ln_mile"
global svar "dist_tolargecity"
global cvar "ratio_croptolivestock_39 ratio_croptolivestock_39_sq ln_hard_road_40"
global cyvar "ln_avg_farmsize_lag ln_avg_farmsize_lag_sq percent_white_lag percent_white_lag_sq"
global pvar "DCOMPB_lag"

set more off
set matsize 11000

use "Data\PSP_Sample_BeforeMatched_t0", clear
* use logit model to estimate propensity score
drop if year>=1942 /* since stop information is not updated since 1941. Thus, there will be no new treatment group added */
logit START $syvar $svar $cvar $cyvar $pvar i.year
est store logit_model
esttab logit_model using "Logit Estimation.rtf" ,replace se pr2 nogap ///
   keep($syvar $svar $cvar $cyvar $pvar) ///
   scalars(N) sfmt(%9.0g %9.3g %9.0g) star(* 0.1 ** 0.05 *** 0.01) b(%6.4f) 
predict phat if e(sample), pr 
drop if phat==.

*****************************************************************************************************
************* Step 3: Match using kernel method year by year ****************************************
*****************************************************************************************************
* matching by year
egen idyr= group(year)
gen double phat_exact=phat+(idyr-1)*10 /* (idyr-1) part can reflect the difference of year */
su phat_exact phat

local caliper = 0.001
g caliper2=0.001

psmatch2 START, kernel caliper(`caliper') pscore(phat_exact) com outcome(kwh_per_cc1)
drop if _support==0
g esample=e(sample) /* e(sample) equals to 1 if the observation is in the estimation sample and 0 otherwise, thus we can know whether observation is in the matching sample */
pstest $syvar $svar $cvar $cyvar $pvar , both graph 
       
drop if _w==. /* drop unmatched sample */

save "Data\PSP_Sample_Matched_t0", replace

tab esample
tab _support
	
* store number of treated and caliper
g treated=1 if START==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'

xml_tab Stats , save("E:\Data Plus\data\PSM\PSM_Pstest_Table_0607.xml") replace sheet(Stats_t0) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $syvar $svar $cvar $cyvar $pvar  if START==1
est store treated
svy: mean $syvar $svar $cvar $cyvar $pvar  if START==0
est store control

xml_tab treated control, save("E:\Data Plus\data\PSM\PSM_Pstest_Table_0607.xml") append sheet(PST_t0)
est clear
