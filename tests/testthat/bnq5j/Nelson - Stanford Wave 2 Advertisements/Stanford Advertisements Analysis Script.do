tabu v10
drop if v10==0

tabu consent
keep if consent==1

gen mcdonalds_first = .
replace mcdonalds_first=1 if mcdonaldsvideo==1

replace second750 =0 if missing(second750)
tabu second750

**replace zip+4 to standard zip to do region coding**
replace zip = "08016" in 1615
replace zip, destring
destring zip, replace

gen northeast=1 if region4==0
gen midwest=1 if region4==1
gen south=1 if region4==2
gen west=1 if region4==3
replace northeast=0 if missing(northeast)
replace midwest=0 if missing(midwest)
replace south=0 if missing(south)
replace west=0 if missing(west)

gen female=gender
recode female 1=0
recode female 2=1
gen female_miss = 1 if missing(female)
replace female_miss = 0 if missing(female_miss)

gen age_year  =2018 - birth_year

gen age_65_plus = 1 if age_year >65
gen age_miss = 1 if missing(age_year)
drop age_65_plus age_miss
gen age_18_to_24 = 1 if age_year >=18 & age_year <=24
gen age_25_to_34 = 1 if age_year >=25 & age_year <=34
gen age_35_to_44 = 1 if age_year >=35 & age_year <=44
gen age_45_to_54 = 1 if age_year >=45 & age_year <=54
gen age_55_to_64 = 1 if age_year >=55 & age_year <=64
gen age_65_plus = 1 if age_year >65
gen age_miss = 1 if missing(age_year)
replace age_18_to_24 = 0 if missing(age_18_to_24)
replace age_25_to_34 = 0 if missing(age_25_to_34)
replace age_35_to_44 = 0 if missing(age_35_to_44)
replace age_45_to_54 = 0 if missing(age_45_to_54)
replace age_55_to_64 = 0 if missing(age_55_to_64)
replace age_65_plus = 0 if missing(age_65_plus)
replace age_miss = 0 if missing(age_miss)

recode hispanic 2=0
gen hisp_miss = 1 if missing(hispanic)
replace hisp_miss =0 if missing(hisp_miss)

replace race_1=0 if missing(race_1)
replace race_2=0 if missing(race_2)
replace race_3=0 if missing(race_3)
replace race_4=0 if missing(race_4)
replace race_5=0 if missing(race_5)
replace race_6=0 if missing(race_6)

gen white=1 if race_1 ==1&race_2==0&race_3==0&race_4==0&race_5==0&race_6==0
replace white=0 if missing(white)

gen black=1 if race_1 ==0&race_2==1&race_3==0&race_4==0&race_5==0&race_6==0
replace black=0 if missing(black)

gen race_missing = 1 if race_1 ==0&race_2==0&race_3==0&race_4==0&race_5==0&race_6==0
replace race_missing = 0 if missing(race_missing)

gen race_other =1 if white==0&black==0&race_missing==0
replace race_other=0 if missing(race_other)

gen inc_less_30 = 1 if income_2a ==1 & income_1 ==1 | income_2a==2 & income_1 ==1 | income_2a==3 & income_1 ==1
gen inc_30_to_50 = 1 if  income_2a ==4 & income_1 ==1| income_2a==5 & income_1==1
gen inc_50_to_75 = 1 if  income_2b ==1 & income_1 ==2
gen inc_75_to_100 = 1 if  income_2b ==2 & income_1 ==2
gen inc_100_plus = 1 if  income_2b ==3 & income_1 ==2 |  income_2b ==4 & income_1 ==2
replace inc_less_30=0 if missing(inc_less_30)
replace inc_30_to_50 =0 if missing(inc_30_to_50)
replace inc_50_to_75 =0 if missing(inc_50_to_75)
replace inc_75_to_100 =0 if missing(inc_75_to_100)
replace inc_100_plus =0 if missing(inc_100_plus)
gen inc_missing =1 if inc_less_30 ==0 & inc_30_to_50==0 & inc_50_to_75==0 &inc_75_to_100==0 &inc_100_plus==0
replace inc_missing=0 if missing(inc_missing)

**first 750**
preserve
keep if second750==0

sem (Recommend -> rec_mcd@1 rec_mcd_fries rec_mcd_food) (IV -> mcdonalds_first@1) (Female -> female@1) (Age18 -> age_18@1) (Age25 -> age_25@1) (Age35 -> age_35@1) (Age45 -> age_45@1) (Age55 -> age_55@1) (Age65 -> age_65_plus@1) (Hispanic -> hispanic@1) (Black -> black@1) (Other_race -> race_other@1) (Hs_grad -> hs_grad@1) (Some_college -> some_college@1) (College_grad -> college_grad@1) (Income30 -> inc_30_to_50@1) (Income50 -> inc_50_to_75@1) (Income100 -> inc_100_plus@1) (Income75 -> inc_75_to_100@1) (Midwest -> midwest@1) (South -> south@1) (West -> west@1) (Recommend -> IV Female Age18 Age25 Age35 Age45 Age55 Age65 Hispanic Black Other_race Hs_grad Some_college College_grad Income30 Income50 Income75 Income100 Midwest South West), cov(e.mcdonalds_first@0 e.female@0 e.age_18@0 e.age_25@0 e.age_35@0 e.age_45@0 e.age_55@0 e.age_65_plus@0 e.hispanic@0 e.black@0 e.race_other@0 e.hs_grad@0 e.some_college@0 e.college_grad@0 e.inc_30_to_50@0 e.inc_50_to_75@0 e.inc_75_to_100@0 e.inc_100_plus@0 e.midwest@0 e.south@0 e.west@0)

restore

**second 750**
preserve
keep if second750==1

sem (Recommend -> rec_mcd@1 rec_mcd_fries rec_mcd_food) (IV -> mcdonalds_first@1) (Female -> female@1) (Age18 -> age_18@1) (Age25 -> age_25@1) (Age35 -> age_35@1) (Age45 -> age_45@1) (Age55 -> age_55@1) (Age65 -> age_65_plus@1) (Hispanic -> hispanic@1) (Black -> black@1) (Other_race -> race_other@1) (Hs_grad -> hs_grad@1) (Some_college -> some_college@1) (College_grad -> college_grad@1) (Income30 -> inc_30_to_50@1) (Income50 -> inc_50_to_75@1) (Income100 -> inc_100_plus@1) (Income75 -> inc_75_to_100@1) (Midwest -> midwest@1) (South -> south@1) (West -> west@1) (Recommend -> IV Female Age18 Age25 Age35 Age45 Age55 Age65 Hispanic Black Other_race Hs_grad Some_college College_grad Income30 Income50 Income75 Income100 Midwest South West), cov(e.mcdonalds_first@0 e.female@0 e.age_18@0 e.age_25@0 e.age_35@0 e.age_45@0 e.age_55@0 e.age_65_plus@0 e.hispanic@0 e.black@0 e.race_other@0 e.hs_grad@0 e.some_college@0 e.college_grad@0 e.inc_30_to_50@0 e.inc_50_to_75@0 e.inc_75_to_100@0 e.inc_100_plus@0 e.midwest@0 e.south@0 e.west@0)

restore

**total sample**
sem (Recommend -> rec_mcd@1 rec_mcd_fries rec_mcd_food) (IV -> mcdonalds_first@1) (Female -> female@1) (Age18 -> age_18@1) (Age25 -> age_25@1) (Age35 -> age_35@1) (Age45 -> age_45@1) (Age55 -> age_55@1) (Age65 -> age_65_plus@1) (Hispanic -> hispanic@1) (Black -> black@1) (Other_race -> race_other@1) (Hs_grad -> hs_grad@1) (Some_college -> some_college@1) (College_grad -> college_grad@1) (Income30 -> inc_30_to_50@1) (Income50 -> inc_50_to_75@1) (Income100 -> inc_100_plus@1) (Income75 -> inc_75_to_100@1) (Midwest -> midwest@1) (South -> south@1) (West -> west@1) (Recommend -> IV Female Age18 Age25 Age35 Age45 Age55 Age65 Hispanic Black Other_race Hs_grad Some_college College_grad Income30 Income50 Income75 Income100 Midwest South West), cov(e.mcdonalds_first@0 e.female@0 e.age_18@0 e.age_25@0 e.age_35@0 e.age_45@0 e.age_55@0 e.age_65_plus@0 e.hispanic@0 e.black@0 e.race_other@0 e.hs_grad@0 e.some_college@0 e.college_grad@0 e.inc_30_to_50@0 e.inc_50_to_75@0 e.inc_75_to_100@0 e.inc_100_plus@0 e.midwest@0 e.south@0 e.west@0)


