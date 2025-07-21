preserve
keep if wave2==0
tabu v10
tabu condition ac
drop if condition ==0 & ac==1 | condition ==1 & ac==2
tabu condition ac
reg dummy condition_r female_missing female_missing age_eighteen age_25 age_45 age_35 age_55 age_65 age_miss hispanic_miss less_than_hs hs_grad some_college college_graduate educ_miss income_30k income_49k income_74k income_99k income_100k income_miss region_northeast region_midwest region_south region_west
restore
preserve
keep if wave2==1
tabu v10
drop if condition ==0 & ac==1 | condition ==1 & ac==2
tabu condition ac
reg dummy condition_r female_missing female_missing age_eighteen age_25 age_45 age_35 age_55 age_65 age_miss hispanic_miss less_than_hs hs_grad some_college college_graduate educ_miss income_30k income_49k income_74k income_99k income_100k income_miss region_northeast region_midwest region_south region_west
restore
preserve
tabu v10
drop if condition ==0 & ac==1 | condition ==1 & ac==2
tabu condition ac
reg dummy condition_r female_missing female_missing age_eighteen age_25 age_45 age_35 age_55 age_65 age_miss hispanic_miss less_than_hs hs_grad some_college college_graduate educ_miss income_30k income_49k income_74k income_99k income_100k income_miss region_northeast region_midwest region_south region_west
