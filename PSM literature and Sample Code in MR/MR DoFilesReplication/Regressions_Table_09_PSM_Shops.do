


clear all
capture log close
set more off
set matsize 8000

* set path
global path ""
cd $path


set linesize 200
log using Log\Regressions_Table_09_PSM_Shops.log, replace

use Data\PSP_Sample.dta, clear

fvset base 2002 year 
fvset base 5211 ind4 


/*

Do file produces results presented in table 9 (lower panel) of the paper

1) identify import starters and non-importers

2) estimate probit model

3) ensure common support

4) implement matching algorithm,using psmatch2

5) save data sets

6) repeat exercise for different time horizons (p=-2 ... p=0, ... p=+3)

7) compute regression-adjusted att
*/


///////////////////////////////
///////////////////////////////

///			Period 0		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g todrop=l2.d_imp_neu15

drop if year<1999

* drop missings 
foreach v of varlist lnnbshops l1lnnbshops l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
	drop if `v'==.
}

drop if outlier_dlnnbshops==1
drop if outlier_lnlprod==1

* focus on first spell in case of multiple entries
g d_imp_start=d_imp_neu15_new_t0
bys cvrnr: egen temp=total(d_imp_start)
g temp2=year if d_imp_start==1
bys cvrnr: egen temp3=min(temp2)
* replace with missing after first import market entry
replace d_imp_start=. if year>temp3
replace d_imp_start=. if d_imp_start==0
drop temp*
* ensure appropriate control group
su d_imp_neu15_nope
order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
bys cvrnr: egen tmp=total(d_imp_start)
tab tmp
g tmp2=d_imp_start if tmp==1
tab tmp2, mi
replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
tab tmp2, mi

drop d_imp_start
ren tmp2 d_imp_start
drop if d_imp_start==.
drop if todrop==.
drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
tab year

* compute higher order terms
foreach v2 of varlist l1lnlprod {
	egen tmp=mean(`v2')
	g `v2'_c=`v2'-tmp
	drop tmp
	g `v2'_sq=`v2'_c^2
}

save Data\PSP_Sample_lnnbshops_BeforeMatched_t0.dta, replace

global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq l1lnnbshops

* probit by ind_id and year
probit d_imp_start $cvar i.year i.ind4
predict phat if e(sample), pr

local caliper = 0.001
g caliper2=0.001

* ensure support
egen idyr=group(year)
su idyr
local idyrmax=r(max)
forvalues jj=1/`idyrmax' {
	qui su phat if d_imp_start==0 & idyr==`jj'
	replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
}
drop if phat==.

* matching by year
g double phat_exact=phat+(idyr-1)*10
su phat_exact phat

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(lnnbshops)
g esample=e(sample)
pstest $cvar
est store pstest_lnnbshops

save Data\PSP_Sample_lnnbshops_Matched_t0.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_lnnbshops_t0) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_lnnbshops
svy: mean $cvar if  d_imp_start==0
est store control_lnnbshops

xml_tab treated_lnnbshops control_lnnbshops pstest_lnnbshops, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_lnnbshops_t0)
est clear
	

	
///////////////////////////////
///////////////////////////////

///			Period 1		///

///////////////////////////////
///////////////////////////////


use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g f1lnnbshops=f1.lnnbshops
g todrop=l2.d_imp_neu15
drop if f1lnnbshops==.

drop if year<1999

* drop missings 
foreach v of varlist lnnbshops l1lnnbshops l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
	drop if `v'==.
}

drop if outlier_dlnnbshops==1
drop if outlier_lnlprod==1

* focus on first spell in case of multiple entries
g d_imp_start=d_imp_neu15_new_t0
bys cvrnr: egen temp=total(d_imp_start)
g temp2=year if d_imp_start==1
bys cvrnr: egen temp3=min(temp2)
* replace with missing after first import market entry
replace d_imp_start=. if year>temp3
replace d_imp_start=. if d_imp_start==0
drop temp*
* ensure appropriate control group
su d_imp_neu15_nope
order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
bys cvrnr: egen tmp=total(d_imp_start)
tab tmp
g tmp2=d_imp_start if tmp==1
tab tmp2, mi
replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
tab tmp2, mi

drop d_imp_start
ren tmp2 d_imp_start
drop if d_imp_start==.
drop if todrop==.
drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
tab year

* compute higher order terms
foreach v2 of varlist l1lnlprod {
	egen tmp=mean(`v2')
	g `v2'_c=`v2'-tmp
	drop tmp
	g `v2'_sq=`v2'_c^2
}

save Data\PSP_Sample_lnnbshops_BeforeMatched_t1.dta, replace

global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq l1lnnbshops

* probit by ind_id and year
probit d_imp_start $cvar i.year i.ind4
predict phat if e(sample), pr

local caliper = 0.001
g caliper2=0.001

* ensure support
egen idyr=group(year)
su idyr
local idyrmax=r(max)
forvalues jj=1/`idyrmax' {
	qui su phat if d_imp_start==0 & idyr==`jj'
	replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
}
drop if phat==.

* matching by year
g double phat_exact=phat+(idyr-1)*10
su phat_exact phat

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f1lnnbshops)
g esample=e(sample)
pstest $cvar
est store pstest_lnnbshops

save Data\PSP_Sample_lnnbshops_Matched_t1.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_lnnbshops_t1) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_lnnbshops
svy: mean $cvar if  d_imp_start==0
est store control_lnnbshops

xml_tab treated_lnnbshops control_lnnbshops pstest_lnnbshops, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_lnnbshops_t1)
est clear
	


///////////////////////////////
///////////////////////////////

///			Period 2		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g f1lnnbshops=f1.lnnbshops
g f2lnnbshops=f2.lnnbshops
g todrop=l2.d_imp_neu15
drop if f1lnnbshops==.
drop if f2lnnbshops==.

drop if year<1999

* drop missings 
foreach v of varlist lnnbshops l1lnnbshops l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
	drop if `v'==.
}

drop if outlier_dlnnbshops==1
drop if outlier_lnlprod==1

* focus on first spell in case of multiple entries
g d_imp_start=d_imp_neu15_new_t0
bys cvrnr: egen temp=total(d_imp_start)
g temp2=year if d_imp_start==1
bys cvrnr: egen temp3=min(temp2)
* replace with missing after first import market entry
replace d_imp_start=. if year>temp3
replace d_imp_start=. if d_imp_start==0
drop temp*
* ensure appropriate control group
su d_imp_neu15_nope
order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
bys cvrnr: egen tmp=total(d_imp_start)
tab tmp
g tmp2=d_imp_start if tmp==1
tab tmp2, mi
replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
tab tmp2, mi

drop d_imp_start
ren tmp2 d_imp_start
drop if d_imp_start==.
drop if todrop==.
drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
tab year

* compute higher order terms
foreach v2 of varlist l1lnlprod {
	egen tmp=mean(`v2')
	g `v2'_c=`v2'-tmp
	drop tmp
	g `v2'_sq=`v2'_c^2
}

save Data\PSP_Sample_lnnbshops_BeforeMatched_t2.dta, replace

global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq l1lnnbshops

* probit by ind_id and year
probit d_imp_start $cvar i.year i.ind4
predict phat if e(sample), pr

local caliper = 0.001
g caliper2=0.001

* ensure support
egen idyr=group(year)
su idyr
local idyrmax=r(max)
forvalues jj=1/`idyrmax' {
	qui su phat if d_imp_start==0 & idyr==`jj'
	replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
}
drop if phat==.

* matching by year
g double phat_exact=phat+(idyr-1)*10
su phat_exact phat

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f2lnnbshops)
g esample=e(sample)
pstest $cvar
est store pstest_lnnbshops

save Data\PSP_Sample_lnnbshops_Matched_t2.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_lnnbshops_t2) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_lnnbshops
svy: mean $cvar if  d_imp_start==0
est store control_lnnbshops

xml_tab treated_lnnbshops control_lnnbshops pstest_lnnbshops, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_lnnbshops_t2)
est clear
	




///////////////////////////////
///////////////////////////////

///			Period 3		///

///////////////////////////////
///////////////////////////////


use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

g f1lnnbshops=f1.lnnbshops
g f2lnnbshops=f2.lnnbshops
g f3lnnbshops=f3.lnnbshops
g todrop=l2.d_imp_neu15
drop if f1lnnbshops==.
drop if f2lnnbshops==.
drop if f3lnnbshops==.

drop if year<1999

* drop missings 
foreach v of varlist lnnbshops l1lnnbshops l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
	drop if `v'==.
}

drop if outlier_dlnnbshops==1
drop if outlier_lnlprod==1

* focus on first spell in case of multiple entries
g d_imp_start=d_imp_neu15_new_t0
bys cvrnr: egen temp=total(d_imp_start)
g temp2=year if d_imp_start==1
bys cvrnr: egen temp3=min(temp2)
* replace with missing after first import market entry
replace d_imp_start=. if year>temp3
replace d_imp_start=. if d_imp_start==0
drop temp*
* ensure appropriate control group
su d_imp_neu15_nope
order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
bys cvrnr: egen tmp=total(d_imp_start)
tab tmp
g tmp2=d_imp_start if tmp==1
tab tmp2, mi
replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
tab tmp2, mi

drop d_imp_start
ren tmp2 d_imp_start
drop if d_imp_start==.
drop if todrop==.
drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
tab year

cap drop mypscore

* compute higher order terms
foreach v2 of varlist l1lnlprod {
	egen tmp=mean(`v2')
	g `v2'_c=`v2'-tmp
	drop tmp
	g `v2'_sq=`v2'_c^2
}

save Data\PSP_Sample_lnnbshops_BeforeMatched_t3.dta, replace

global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq l1lnnbshops

* probit by ind_id and year
probit d_imp_start $cvar i.year i.ind4
predict phat if e(sample), pr

local caliper = 0.001
g caliper2=0.001

* ensure support
egen idyr=group(year)
su idyr
local idyrmax=r(max)
forvalues jj=1/`idyrmax' {
	qui su phat if d_imp_start==0 & idyr==`jj'
	replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
}
drop if phat==.

* matching by year
g double phat_exact=phat+(idyr-1)*10
su phat_exact phat
psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(f3lnnbshops)
g esample=e(sample)
pstest $cvar
est store pstest_lnnbshops

save Data\PSP_Sample_lnnbshops_Matched_t3.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_lnnbshops_t3) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_lnnbshops
svy: mean $cvar if  d_imp_start==0
est store control_lnnbshops

xml_tab treated_lnnbshops control_lnnbshops pstest_lnnbshops, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_lnnbshops_t3)
est clear
	


///////////////////////////////
///////////////////////////////

///			Period t-2		///

///////////////////////////////
///////////////////////////////

use Data\PSP_Sample.dta, clear

* import starter
xtset cvrnr year, yearly
g d_imp_neu15_new_t0=1 if d_imp_neu15==1 & l.d_imp_neu15==0 & l2.d_imp_neu15==0
replace d_imp_neu15_new_t0=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

* control group should consist of non-importing firms
g d_imp_neu15_nope=0 if d_imp_neu15==0 & l.d_imp_neu15==0 & l2.d_imp_neu15==0

cap drop l1lnnbshops
cap drop l2lnnbshops
g l1lnnbshops=l1.lnnbshops
g l2lnnbshops=l2.lnnbshops
g todrop=l2.d_imp_neu15
drop if l1lnnbshops==.
drop if l2lnnbshops==.

drop if year<1999

* drop missings 
foreach v of varlist lnnbshops l1lnnbshops l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum {
	drop if `v'==.
}

drop if outlier_dlnnbshops==1
drop if outlier_lnlprod==1

* focus on first spell in case of multiple entries
g d_imp_start=d_imp_neu15_new_t0
bys cvrnr: egen temp=total(d_imp_start)
g temp2=year if d_imp_start==1
bys cvrnr: egen temp3=min(temp2)
* replace with missing after first import market entry
replace d_imp_start=. if year>temp3
replace d_imp_start=. if d_imp_start==0
drop temp*
* ensure appropriate control group
su d_imp_neu15_nope
order cvrnr year d_imp_neu15 d_imp_start d_imp_neu15_nope
bys cvrnr: egen tmp=total(d_imp_start)
tab tmp
g tmp2=d_imp_start if tmp==1
tab tmp2, mi
replace tmp2=d_imp_neu15_nope if d_imp_neu15_nope==0
tab tmp2, mi

drop d_imp_start
ren tmp2 d_imp_start
drop if d_imp_start==.
drop if todrop==.
drop todrop d_imp_neu15_nope d_imp_neu15_new_t0 tmp*
tab year

cap drop mypscore

* compute higher order terms
foreach v2 of varlist l1lnlprod {
	egen tmp=mean(`v2')
	g `v2'_c=`v2'-tmp
	drop tmp
	g `v2'_sq=`v2'_c^2
}

save Data\PSP_Sample_lnnbshops_BeforeMatched_t-2.dta, replace

global cvar l1lnlprod l1lnempl l1dlndomsales l1wshare l1lnbalance_sum l1lnlprod_sq l1lnnbshops

* probit by ind_id and year
probit d_imp_start $cvar i.year i.ind4
predict phat if e(sample), pr

local caliper = 0.001
g caliper2=0.001

* ensure support
egen idyr=group(year)
su idyr
local idyrmax=r(max)
forvalues jj=1/`idyrmax' {
	qui su phat if d_imp_start==0 & idyr==`jj'
	replace phat=. if d_imp_start==1 & idyr==`jj' & (phat>r(max) | phat<r(min))
}
drop if phat==.

* matching by year
g double phat_exact=phat+(idyr-1)*10
su phat_exact phat

psmatch2 d_imp_start, radius caliper(`caliper') pscore(phat_exact) com outcome(lnnbshops)
g esample=e(sample)
pstest $cvar
est store pstest_lnnbshops

save Data\PSP_Sample_lnnbshops_Matched_t-2.dta, replace

tab esample
tab _support

drop if _w==.

* store number of treated and caliper
g treated=1 if d_imp_start==1
g N=_N
tabstat treated N caliper2, s(N mean) c(s) save
tabstatmat all
matrix Stats=all'
xml_tab Stats , save(Log\PSM_Pstest_Table_09.xml) append sheet(Stats_lnnbshops_t-2) 

* store mean of treated and control group
svyset _n [pw=_w]
svy: mean $cvar if  d_imp_start==1
est store treated_lnnbshops
svy: mean $cvar if  d_imp_start==0
est store control_lnnbshops

xml_tab treated_lnnbshops control_lnnbshops pstest_lnnbshops, save(Log\PSM_Pstest_Table_09.xml) ///
 append sheet(PST_lnnbshops_t-2)
est clear
	

// OVERVIEW OF RESULTS


// probability of opening a new shops
* ATT - t-2 (placebo)
use Data\PSP_Sample_lnnbshops_Matched_t-2.dta, clear
su nbshops l1nbshops l2nbshops
cap drop dshops
g dshops=(l1lnnbshops>l2lnnbshops)
tab dshops
qui tab year, g(dyr)
reg dshops d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store placebo1_t0
* ATT - t0
use Data\PSP_Sample_lnnbshops_Matched_t0.dta, clear
su nbshops l1nbshops
su lnnbshops l1lnnbshops*
cap drop dshops
g dshops=(lnnbshops>l1lnnbshops)
tab dshops
qui tab year, g(dyr)
reg dshops d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store psps_t0
* ATT - t1
use Data\PSP_Sample_lnnbshops_Matched_t1.dta, clear
g dshops_cum=(f1lnnbshops>l1lnnbshops)
tab dshops
tab dshops_cum
qui tab year, g(dyr)
reg dshops_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store psps_cum_t1
* ATT - t2
use Data\PSP_Sample_lnnbshops_Matched_t2.dta, clear
g dshops_cum=(f2lnnbshops>l1lnnbshops)
tab dshops
tab dshops_cum
qui tab year, g(dyr)
reg dshops_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store psps_cum_t2
* ATT - t3
use Data\PSP_Sample_lnnbshops_Matched_t3.dta, clear
g dshops_cum=(f3lnnbshops>l1lnnbshops)
tab dshops
tab dshops_cum
qui tab year, g(dyr)
reg dshops_cum d_imp_start $cvar dyr* i.ind4 [pw=_weight], cluster(cvrnr)
est store psps_cum_t3

xml_tab placebo1_t0 psps_t* psps_c*, ///
 save(Log\Results_Table_09_PSM.xml) ///
 append sheet(dshops) sd below stats(N r2) ///
 keep(d_imp_start)

est clear



log close
exit, clear
