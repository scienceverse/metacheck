egen dv = rowtotal(v1_scenario1 v1_scenario3 v1_scenario5 v1_scenario7 v1_scenario9 v1_scenario11 v2_scenario1 v2_scenario3 v2_scenario5 v2_scenario7 v2_scenario9 v2_scenario11)
replace dv = dv/6
summ dv
preserve
keep if wave1==1
ttest dv, by(version)
restore
preserve
keep if wave1==0
ttest dv, by(version)
restore
ttest dv, by(version)
