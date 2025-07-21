use "/Users/michaelodonnell/Dropbox/Research/UCSB Self-Control Fetzer/UCSB_SelfControl__Fetzer_to analyze_att_check.dta", clear
preserve
keep if first_750==1
probit guilty_total condition
restore
preserve
keep if first_750==0
probit guilty_total condition
restore
probit guilty_total condition
