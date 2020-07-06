

clear all
capture log close

* set path
global path ""
cd $path



set linesize 200
log using Log\Regressions_Table_11_RegionLevel.log,replace

/*

Do file produces results presented in table 11 of the paper (and some results for the appendix)

1) collapse data to the shop-region level

2) merge data sets

3) run regressions - OLS

4) wild bootstrap

5) regressions for appendix

*/


global Reps 1000
global seed 987654321


// Collapse data set

use Data\Herf_Workplace_4.dta, clear
tab year

** distibute key variables across markets for multi-shop firms
** distribute key variables across shops
su oms domsales va balance_sum year

drop if oms<=0
drop if balance_sum<=0
drop if va<=0

* share based on total employment
bys cvrnr year: egen tot_antaar=total(antaar)
g share_lonaar=antaar/tot_antaar
order cvrnr year share_lonaar komny2 ind* antaar antnov lonaar longns
sum share* year
bys cvrnr year: egen tmp=total(share)
su tmp,d
count if share==0
drop tmp

replace imp_it=imp_it*share
replace imp_it_neu15=imp_it_neu15*share
replace oms=oms*share
replace balance_sum=balance_sum*share

replace va=va*share

g shops=1

cap drop N*
bys cvrnr year: g N=_N
su N,d
g chain=1 if N>1

* collapse data to cvrnr-komny2-year level (several shops in one market)
collapse (sum) oms imp_it_neu15 antnov antaar longns lonaar /// 
 va balance_sum shops (max) chain d_imp d_imp_neu15, by(cvrnr ind5 komny2 year)

g av_oms=oms
g sd_oms=oms
bys ind5 komny2 year: egen tmp=total(oms)
g ms=oms/tmp
g herf2 =ms^2
su herf2,d
g firms_tot=1
g firms_imp=d_imp
g firms_imp_neu15=d_imp_neu15
tab d_imp

* merge markups
merge m:1 cvrnr year using Data\TFP_MU_adj.dta, update
drop if _merge==2
drop _merge

* merge labor productivity
merge m:1 cvrnr year using Data\firm_exp_imp_v04.dta, update keepusing(lprod lnlprod outlier_lnlprod)
drop if _merge==2
drop _merge

* shares for mark-ups
cap drop tmp*
g tmp=oms
replace tmp=. if mu==.
replace mu=. if tmp==.
bys ind5 komny2 year: egen tmp2=total(tmp)
g s_mu=tmp/tmp2
drop tmp*
bys ind5 komny2 year: egen tmp=total(s_mu)
su tmp*,d

replace mu=mu*s_mu
drop tmp* s_mu

* shares for productivity
replace lprod=. if outlier_lnlprod==1
cap drop tmp*
g tmp=oms
replace tmp=. if lprod==.
replace lprod=. if tmp==.
bys ind5 komny2 year: egen tmp2=total(tmp)
g s_prod=tmp/tmp2
drop tmp*
bys ind5 komny2 year: egen tmp=total(s_prod)
su tmp*,d
replace lprod=lprod*s_prod
drop tmp* s_prod

su oms sd_oms
* collapse data to ind5-komny2-year level
collapse (sum) herf2 oms imp_it_neu15 firms_tot firms_imp* antnov antaar longns lonaar /// 
 va balance_sum shops chain mu lprod (sd) sd_oms (mean) av_oms, by(ind5 komny2 year)

replace mu=. if mu==0
replace lprod=. if lprod==0

su sd_oms av_oms firms_tot
su firms_tot if sd_oms==.
su firms_tot if sd_oms==0
g herf=((sd_oms^2/av_oms^2)+1)/firms_tot
su herf herf2
count if herf>1 & herf!=.
replace herf=1 if firms_tot==1
replace herf=1 if herf>1 & herf!=.
replace sd_oms=0 if sd_oms==.
su herf herf2

save Data\Herf_Workplace_5.dta, replace



// Prepare estimation sample

use Data\Herf_Workplace_5.dta, clear

* merge sector level retail import variables
merge m:1 ind5 year using Data\Imp_Sector_Retail_2.dta, update
tab year if _merge==1
keep if _merge==3
drop _merge

* merge sector level non-retail import variables
merge m:1 ind5 year using Data\Imp_Sector_NonRetail.dta, update
tab year if _merge==1
drop if _merge==2
drop _merge

egen idx=group(ind5 komny2)
xtset idx year, yearly

g lnherf=ln(herf)
g lnfirms=ln(firms_tot)
g lnav_oms=ln(av_oms)
g lnsd_oms=ln(sd_oms)
g lnmu=ln(mu)
g lnlprod=ln(lprod)

g lnsize=ln(balance_sum)
g l1lnsize=l.lnsize
g lnwage_idx=ln(lonaar/antaar)
g l1lnwage_idx=l.lnwage_idx

drop if lnimp_r==.
drop if lnlprod==.
drop if lnmu==.
drop if year<1999

qui tab year, g(dyr)

save Data\EstSampleRegionLevel.dta, replace


use Data\EstSampleRegionLevel.dta, clear
drop if ind5<5220 & year<2002

egen ind5_id=group(ind5)
su ind5_id

// Sector-level imports - direct imports

global xvar l1lnsize l1lnwage_idx
global xvar_dm l1lnsize_dm l1lnwage_idx_dm

** within transformation by hand to account for idx fixed effects

foreach v of varlist lnfirms lnav_oms lnsd_oms lnherf lnlprod lnmu { 
	
	use Data\EstSampleRegionLevel.dta, clear
	drop if ind5<5220 & year<2002
	foreach vvv of varlist `v' lnimp_r lnimp_w $xvar {
		drop if `vvv'==.
	}
	bys idx: g N=_N
	drop if N==1
	** within transformation
	foreach vvv of varlist `v' lnimp_r lnimp_w $xvar {
		bys idx: egen tmp=mean(`vvv')
		g `vvv'_dm=`vvv'-tmp
		drop tmp
	}
	
	bys ind5: egen tmp=total(oms)
	g weight=oms/tmp
	drop tmp*

	* unweighted
	reg `v'_dm lnimp_r_dm $xvar_dm dyr2-dyr10, cluster(ind5)
	est store s_`v'_r
	cgmwildboot `v'_dm lnimp_r_dm $xvar_dm dyr2-dyr10, cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(0 . . . . . . . . . . .)
	est store bs_`v'_r
	* weighted
	reg `v'_dm lnimp_r_dm $xvar_dm dyr2-dyr10 [aw=weight], cluster(ind5)
	est store ws_`v'_r
	cgmwildboot `v'_dm lnimp_r_dm $xvar_dm dyr2-dyr10 [aw=weight], cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(0 . . . . . . . . . . .)
	est store wbs_`v'_r

	drop `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm
}

xml_tab s_lnherf_r ws_lnherf_r, ///
 save(Log\Results_Table11_RegionLevel.xml) ///
 replace sheet(MarketLvl_Herf) sd below stats(N r2_a r2) ///
 keep(lnimp_r_dm $xvar_dm)

xml_tab s_*, ///
 save(Log\Results_Table11_RegionLevel.xml) ///
 append sheet(MarketLvl_App) sd below stats(N r2_a r2) ///
 keep(lnimp_r_dm $xvar_dm)

xml_tab ws_*, ///
 save(Log\Results_Table11_RegionLevel.xml) ///
 append sheet(MarketLvl_App_W) sd below stats(N r2_a r2) ///
 keep(lnimp_r_dm $xvar_dm)
est clear



// Sector-level imports - direct AND indirect imports

global xvar l1lnsize l1lnwage_idx
global xvar_dm l1lnsize_dm l1lnwage_idx_dm

** within transformation by hand to account for idx fixed effects

foreach v of varlist lnfirms lnav_oms lnsd_oms lnherf lnlprod lnmu { 
	
	use Data\EstSampleRegionLevel.dta, clear
	drop if ind5<5220 & year<2002
	foreach vvv of varlist `v' lnimp_r lnimp_w $xvar {
		drop if `vvv'==.
	}
	bys idx: g N=_N
	drop if N==1
	** within transformation
	foreach vvv of varlist `v' lnimp_r lnimp_w $xvar {
		bys idx: egen tmp=mean(`vvv')
		g `vvv'_dm=`vvv'-tmp
		drop tmp
	}
	
	bys ind5: egen tmp=total(oms)
	g weight=oms/tmp
	drop tmp*

	* unweighted
	reg `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10, cluster(ind5)
	est store s_`v'_r
	* H0 for direct imports
	cgmwildboot `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10, cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(0 . . . . . . . . . . . .)
	* H0 for direct inimports
	cgmwildboot `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10, cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(. 0 . . . . . . . . . . .)
	* weighted
	reg `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10 [aw=weight], cluster(ind5)
	est store ws_`v'_r
	* H0 for direct imports
	cgmwildboot `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10 [aw=weight], cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(0 . . . . . . . . . . . .)
	* H0 for direct inimports
	cgmwildboot `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm dyr2-dyr10 [aw=weight], cluster(ind5) bootcluster(ind5) reps($Reps) seed($seed) null(. 0 . . . . . . . . . . .)

	drop `v'_dm lnimp_r_dm lnimp_w_dm $xvar_dm
}




xml_tab s_*, ///
 save(Log\Results_Table11_RegionLevel.xml) ///
 append sheet(Market_Level_ImpSec_rw) sd below stats(N r2_a r2) ///
 keep(lnimp_r_dm lnimp_w_dm $xvar_dm)

xml_tab ws_*, ///
 save(Log\Results_Table11_RegionLevel.xml) ///
 append sheet(Market_Level_ImpSec_rw_W) sd below stats(N r2_a r2) ///
 keep(lnimp_r_dm lnimp_w_dm $xvar_dm)
est clear

 

log close
exit, clear
