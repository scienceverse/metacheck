*drop non-consent (scored as complete but no data collected)*
tabu consent
drop if consent==2

*drop incompletes*
tabu v10
drop if v10==0

*drop attention check failures*
tabu q44
keep if q44=1
keep if q44==1

*destring condition*
destring condition, replace

*generate dv*
gen diff = .
replace diff = (money_150100_1 - money_150100_rerate_1) if condition_money_first ==1
replace diff = (time_34_1 - time_34_rerate_1) if condition_money_first ==0

*first 750*
preserve
keep if first_750==1

ttest diff, by(condition)

restore

*second 750*
preserve
keep if first_750==0

ttest diff, by(condition)

restore

*full sample*
ttest diff, by(condition)
