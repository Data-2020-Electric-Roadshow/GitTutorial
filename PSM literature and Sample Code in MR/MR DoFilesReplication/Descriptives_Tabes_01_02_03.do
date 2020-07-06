

clear all
capture log close

set matsize 800
set more off


* set path
global path ""
cd $path

log using Log\Descriptives_Tables_01_02_03.log,replace


////////////////////////////////////////

//		 Table 1: Descriptives 		  //

////////////////////////////////////////


// Firms
use Data\firm_exp_imp_v03.dta, clear

tab ind_id
drop if ind_id==.
tab year
*drop if empl==0

g f_=1
g fi_=(imp_it!=0)
g fni_=(imp_it==0)
replace oms=domsales
drop if oms==. | oms<=0
g oms_i=oms if (imp_it!=0)
g oms_ni=oms if (imp_it==0)
replace imp_variety_hs6plus=. if imp_variety_hs6plus==0

collapse (sum) f_ fi_ fni_ imp_it oms oms_i oms_ni /// 
 (mean) imp_variety_hs6plus, by(year)

ren oms to_
ren oms_i to_i_
ren oms_ni to_ni_
*ren imp_it_neu15 i_
ren imp_it i_
ren imp_variety_hs6plus iv_
replace to_=to_/1000000
replace to_i_=to_i_/1000000
replace to_ni_=to_ni_/1000000
replace i_=i_/1000000

g sh_to_f=to_/f_
g sh_toi_fi=to_i/fi
g sh_toni_fni=to_ni/fni
g sh_toi_to=to_i/to_
g sh_i_fi =i_/fi_
g sh_fi_f =fi_/f_
list year sh_* if year>1998

drop if year<1999

tostring year, replace
export excel * using "Log\Descriptives_Tables_01_02_03.xls" , sheet(Firms_all) firstrow(variables) sheetreplace


// Trade

use Data\uhdi_9508_key_4_hs6.dta, clear

drop if cvrnr==.

tab good_type
keep if good_type==3

merge m:1 cvrnr year using Data\maj_firmastat_9308_2.dta, update keepusing(branche ind2 ind4) 
keep if _merge==3
drop _merge

*1) textiles --> NACE4: 5241-5243
gen ind_id=1 if ind4 >= 5241 & ind4 <=5243
*2) furniture --> NACE4: 5244
replace ind_id=2 if ind4 == 5244
*3) electrial household appliances --> NACE4: 5245
replace ind_id=3 if ind4 == 5245
*4) hardware --> NACE4: 5246
replace ind_id=4 if ind4 == 5246
*5) retail sale in non-specialized stores (department stores)
replace ind_id=5 if ind4 >= 5211 & ind4<=5212
*6) others retail sale in specialized stores
replace ind_id=6 if ind4 >= 5248 & ind4<=5249

tab ind_id, mi
replace ind_id=(ind_id!=.)
tab ind_id, mi

collapse (sum) imp, by(cvrnr ind_id year)
g firms=1
collapse (sum) firms imp, by(ind_id year)
ren firms f_
ren imp i_
drop if year<1999
reshape wide f_ i_ , i(year) j(ind)
g f_share=f_1/(f_0+f_1)
g i_share=i_1/(i_0+i_1)
list

export excel * using "Log\Descriptives_Tables_01_02_03.xls" , sheet(our_retail_cgoods_all) firstrow(variables) sheetreplace


// Herfindahl Index

// Save aggregated Herfindahl Index
* all markets
use Data\Herf_Workplace_5.dta, clear
*use Data\EstSampleRegionLevel.dta, clear
su ind5
su firms_tot,d
su firms_tot,d
drop if ind5<5220
bys year: egen tot_oms=total(oms)
g share=oms/tot_oms
g herf_2=herf*share
g herf2_2=herf2*share
g herf_p50=herf
g herf2_p50=herf2
g mu_p50=mu
replace oms=oms/1000000
collapse (mean) herf herf2 mu (sum) herf_2 herf2_2 firms_tot oms (p50) herf_p50 herf2_p50 mu_p50, by(year)
g av_oms=oms/firms_tot
list *
keep year herf herf_2 herf_p50
ren herf herf_av
ren herf_2 herf_wav
tostring year, replace
export excel * using "Log\Descriptives_Tables_01_02_03.xls" , sheet(Herf_all) firstrow(variables) sheetreplace



////////////////////////////////

//		 Table 2: CHAINS 	  //

////////////////////////////////


use Data\firm_exp_imp_v03.dta, clear

* number of shops
merge 1:1 cvrnr year using Data\NbShops2.dta, update
drop if _merge==1
drop _merge

drop if nbshops==.
su nbshops year

replace domsales=domsales/1000000
replace imp_it=imp_it/1000000

g chain_oms=domsales*chain
g chain_dimp=chain*d_imp
g chain_imp=chain*imp_it

drop if year<1999
g firm1=1
collapse (sum) nbshops firm domsales d_imp imp_it chain chain_*, by(year)

tostring year, replace
export excel * using "Log\Descriptives_Tables_01_02_03.xls" , sheet(Shops) firstrow(variables) sheetreplace



/////////////////////////////////

// Table 3: CHAINS vs. NOCHAIN //

/////////////////////////////////


use Data\firm_exp_imp_v04.dta, clear
tab year

* number of shops
merge 1:1 cvrnr year using Data\NbShops2.dta, update
drop if _merge==2
drop _merge

drop if year<1999
drop if outlier_lnlprod==1
drop if nbshops==.

su chain*
tab d_imp chain, mi
drop if lnlprod==.
drop if lnbalance_sum==.
drop if empl==0

cap drop idx
g idx="ChainImp" if chain==1 & d_imp==1
replace idx="ChainNoImp" if chain==1 & d_imp==0
replace idx="NoChainImp" if chain==0 & d_imp==1
replace idx="NoChainNoImp" if chain==0 & d_imp==0

tab idx, mi

foreach v of varlist domsales balance_sum imp_it {
	replace `v'=`v'/1000000
}

g nfirms=1
collapse (mean) domsales balance_sum imp_it empl (sum) nfirms d_imp, by(idx year)
sort year

keep if year==2005

order idx domsales empl nfirms d_imp imp_it
list
drop year
export excel * using "Log\Descriptives_Tables_01_02_03.xls" , sheet(ChainVsNoChain) firstrow(variables) sheetreplace




log close
exit, clear



