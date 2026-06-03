use  "study1_data.dta",clear

***************************************************************************
*Set data for survival Analaysis*******************************************
***************************************************************************
stset age, failure(died) enter(age_interview)

***************************************************************************
*Base Models***************************************************************
***************************************************************************

*Model 1
stcox ib1.rdo, shared(state) forceshared

*Model 2
stcox ib1.rdo female birthyear, shared(state) forceshared

*Model 3
stcox ib1.rdo##c.z_state_rep_fedelec, shared(state) forceshared

*Model 4
stcox ib1.rdo##c.z_state_rep_fedelec female  birthyear, shared(state) forceshared

***************************************************************************
*Predictions***************************************************************
***************************************************************************

***Overall
stpm2 ib1.rdo  female  birthyear , df(3) scale(hazard) eform
capture drop timevar
range timevar  20 100 81
capture drop h0_*
predict h0_rep, meansurv timevar(timevar) at(rdo 2) ci 
predict h0_dem, meansurv timevar(timevar) at(rdo 1) ci 
graph twoway rarea h0_rep_lci h0_rep_uci  timevar  || line h0_rep timevar || rarea h0_dem_lci h0_dem_uci timevar ||  line h0_dem timevar

***Across States
stpm2 ib1.rdo##c.z_state_rep_fedelec female birthyear, df(3) scale(hazard) eform
capture drop timevar
range timevar  20 100 81
capture drop h0_*
sum z_state_rep_fedelec
predict h0_repinrep, meansurv timevar(timevar) at(rdo 2 z_state_rep_fedelec `r(max)') ci 
sum z_state_rep_fedelec
predict h0_deminrep, meansurv timevar(timevar) at(rdo 1 z_state_rep_fedelec `r(max)') ci
sum z_state_rep_fedelec
predict h0_repindem, meansurv timevar(timevar) at(rdo 2 z_state_rep_fedelec `r(min)') ci
sum z_state_rep_fedelec
predict h0_demindem, meansurv timevar(timevar) at(rdo 1 z_state_rep_fedelec `r(min)') ci
graph twoway rarea h0_repinrep_lci h0_repinrep_uci  timevar  || line h0_repinrep timevar || rarea h0_deminrep_lci h0_deminrep_uci timevar ||  line h0_deminrep timevar
graph twoway rarea h0_repindem_lci h0_repindem_uci  timevar  || line h0_repindem timevar || rarea h0_demindem_lci h0_demindem_uci timevar ||  line h0_demindem timevar

***************************************************************************
*Proportional hazard assumptions*******************************************
***************************************************************************
*rdo = Strong Partisanship
*rdl = Not-strong Partisanship
*rde = Leaning Partisanship
*con = Political Ideology
*cos = Not-strong politcal ideology

stcox ib1.rdo z_state_rep_fedelec female  birthyear, shared(state) forceshared
stphtest,  plot(birthyear) 
stphtest,  plot(female) 
stphtest,  plot(z_state_rep_fedelec)
stphtest,  plot(2.rdo) 
stcox ib1.rdl z_state_rep_fedelec female  birthyear, shared(state) forceshared
stphtest,  plot(2.rdl)
stcox ib1.rde z_state_rep_fedelec female  birthyear, shared(state) forceshared
stphtest,  plot(2.rde)
stcox ib1.con z_state_rep_fedelec female  birthyear, shared(state) forceshared
stphtest,  plot(2.con)
stcox ib1.cos z_state_rep_fedelec female  birthyear, shared(state) forceshared
stphtest,  plot(2.cos)

***************************************************************************
*Robustness Tests**********************************************************
***************************************************************************

foreach x of varlist rdo rdl rde  {

*insert individual political variable
capture drop ind_pol 
gen ind_pol = `x'

*insert state-level political variable
capture drop z_state_pol 
gen z_state_pol  = z_state_pol_`x'

***************************************************************************
*Robustness - Young age and individual Confounds***************************
***************************************************************************

***excluding all below 50 years of age
stcox female  birthyear ib1.ind_pol##c.z_state_pol  if age >=50, shared(state) forceshared

preserve
***adjust social status variable
foreach y of varlist  race social_status rel_stat relig_ident relig_attend   {
replace `y' = . if race == . | social_status == .  | rel_stat == . | relig_ident == . | relig_attend == . 
}
capture drop z_social_status
egen z_social_status = std(social_status)

***Race
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.race##c.z_state_pol, shared(state) forceshared 

***Social Status
stcox female birthyear ib1.ind_pol##c.z_state_pol  c.z_social_status##c.z_state_pol, shared(state) forceshared 

***Relationship Status
stcox female birthyear  ib1.ind_pol##c.z_state_pol i.rel_stat##c.z_state_pol, shared(state) forceshared  

***Religiosity Attendance
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.relig_attend##c.z_state_pol , shared(state) forceshared 

***Religiosity Identitiy
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.relig_ident##c.z_state_pol, shared(state) forceshared 

***All
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.race##c.z_state_pol  c.z_social_status##c.z_state_pol i.rel_stat##c.z_state_pol i.relig_attend##c.z_state_pol i.relig_ident##c.z_state_pol, shared(state) forceshared

restore

***************************************************************************
*Robustness - State-leve Confounds*****************************************
***************************************************************************

***State-Level Race
stcox female birthyear ib1.ind_pol##c.z_state_pol    ib1.ind_pol##c.z_state_white_sh, shared(state) forceshared

***State-Level Income
stcox female birthyear  ib1.ind_pol##c.z_state_pol   ib1.ind_pol##c.z_state_income, shared(state) forceshared

***State-Level Inequality
stcox female birthyear   ib1.ind_pol##c.z_state_pol  ib1.ind_pol##c.z_state_gini, shared(state) forceshared

***State-Level Population Density
stcox female birthyear ib1.ind_pol##c.z_state_pol    ib1.ind_pol##c.z_state_popdens, shared(state) forceshared

***State-Level Religiosity
stcox female birthyear ib1.ind_pol##c.z_state_pol    ib1.ind_pol##c.z_state_relig, shared(state) forceshared

***All
stcox female birthyear ib1.ind_pol##c.z_state_pol	 ib1.ind_pol##c.z_state_white_sh ib1.ind_pol##c.z_state_income ib1.ind_pol##c.z_state_gini ib1.ind_pol##c.z_state_popdens  ib1.ind_pol##c.z_state_relig, shared(state) forceshared

***************************************************************************
*Robustness - Additional Robustness Tests**********************************
***************************************************************************

***Urban Rural
stcox female birthyear ib1.ind_pol##c.z_state_pol  	ib1.ind_pol##i.urbanrural, shared(state) forceshared

***Same State
stcox female  birthyear ib1.ind_pol##c.z_state_pol 	if diffstate16 != 1, shared(state) forceshared

***************************************************************************
*Robustness - Alternative State Political Measures*************************
***************************************************************************

***Presidential Elections
replace z_state_pol = z_state_rep_presid
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared

***Congress
replace z_state_pol = z_state_rep_congress
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared

***Legislature
replace z_state_pol = z_state_rep_legis
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared

***Republican Share
replace z_state_pol = z_state_gss_repsh_`x'
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared

***Combined 
replace z_state_pol = z_state_rep_all_`x'
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared
}

***Allowing Politcal Culture to vary across time
foreach x of varlist rdo rdl rde {
capture drop ind_pol 
gen ind_pol = `x'

preserve
keep  idu died  ind_pol  female birthyear age_stset state age_interview
sort idu
expand age_stset 
bysort idu: gen run = _n
gen year_run = birthyear + run
gen last = 1 if idu !=idu[_n+1]
gen died_run = 0
replace died_run = 1 if died == 1 & last ==1
stset run , id(idu) failure(died_run==1) enter(age_interview)
merge m:1 state year_run using "yearly_state.dta", generate(_merge_yearly)
keep if _merge_yearly ==3 
capture drop z_state_pol
gen z_state_pol = z_state_rep_fedelec_12ya
stcox female birthyear ib1.ind_pol##c.z_state_pol, shared (state) forceshared 
restore
}


***************************************************************************
*Political Ideology********************************************************
***************************************************************************

foreach x of varlist con cos  {

*insert individual political variable
capture drop ind_pol 
gen ind_pol = `x'

*insert state-level political variable
capture drop z_state_pol 
gen z_state_pol  = z_state_pol_`x'

***************************************************************************
*Robustness - Young age and individual Confounds***************************
***************************************************************************

***Main Model
stcox female  birthyear ib1.ind_pol##c.z_state_pol, shared(state) forceshared

***excluding all below 50 years of age
stcox female  birthyear ib1.ind_pol##c.z_state_pol  if age >=50, shared(state) forceshared

preserve
***adjust social status variable
foreach y of varlist  race social_status rel_stat relig_ident relig_attend   {
replace `y' = . if race == . | social_status == .  | rel_stat == . | relig_ident == . | relig_attend == . 
}
capture drop z_social_status
egen z_social_status = std(social_status)

***Race
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.race##c.z_state_pol, shared(state) forceshared 

***Social Status
stcox female birthyear ib1.ind_pol##c.z_state_pol  c.z_social_status##c.z_state_pol, shared(state) forceshared 

***Relationship Status
stcox female birthyear  ib1.ind_pol##c.z_state_pol i.rel_stat##c.z_state_pol, shared(state) forceshared  

***Religiosity Attendance
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.relig_attend##c.z_state_pol , shared(state) forceshared 

***Religiosity Identitiy
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.relig_ident##c.z_state_pol, shared(state) forceshared 

***All
stcox female birthyear ib1.ind_pol##c.z_state_pol  i.race##c.z_state_pol  c.z_social_status##c.z_state_pol i.rel_stat##c.z_state_pol i.relig_attend##c.z_state_pol i.relig_ident##c.z_state_pol, shared(state) forceshared

restore

***************************************************************************
*Robustness - State-leve Confounds*****************************************
***************************************************************************

***State-Level Race
stcox female birthyear ib1.ind_pol##c.z_state_pol   ib1.ind_pol##c.z_state_white_sh, shared(state) forceshared

***State-Level Income
stcox female birthyear  ib1.ind_pol##c.z_state_pol  ib1.ind_pol##c.z_state_income, shared(state) forceshared

***State-Level Inequality
stcox female birthyear   ib1.ind_pol##c.z_state_pol ib1.ind_pol##c.z_state_gini, shared(state) forceshared

***State-Level Population Density
stcox female birthyear ib1.ind_pol##c.z_state_pol   ib1.ind_pol##c.z_state_popdens, shared(state) forceshared

***State-Level Religiosity
stcox female birthyear ib1.ind_pol##c.z_state_pol   ib1.ind_pol##c.z_state_relig, shared(state) forceshared

***All
stcox female birthyear ib1.ind_pol##c.z_state_pol ib1.ind_pol##c.z_state_white_sh ib1.ind_pol##c.z_state_income ib1.ind_pol##c.z_state_gini ib1.ind_pol##c.z_state_popdens  ib1.ind_pol##c.z_state_relig, shared(state) forceshared

***************************************************************************
*Robustness - Additional Robustness Tests**********************************
***************************************************************************

***Urban Rural
stcox female birthyear ib1.ind_pol##c.z_state_pol   ib1.ind_pol##i.urbanrural, shared(state) forceshared

***Same State
stcox female  birthyear ib1.ind_pol##c.z_state_pol  if diffstate16 != 1, shared(state) forceshared
}

