tabu v10
gen diff = .
replace diff = (money_150100_1 - money_150100_rerate_1) if condition_money_first ==1
replace diff = (time_34_1 - time_34_rerate_1) if condition_money_first ==0
ttest diff, by(condition)
