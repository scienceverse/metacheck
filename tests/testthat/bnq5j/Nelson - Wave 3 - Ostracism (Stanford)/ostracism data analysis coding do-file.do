destring condition, replace
keep if att_check==1 & condition_o ==0 | att_check ==2 & condition_o ==1
save "/Users/michaelodonnell/Dropbox/Research/Stanford - Ostracism/Stanford_Wave_3_Ostracism_incomplete&attention_drops.dta"

replace zipcode = "32401" in 757
**run region do-file**

tabu q11
ren q11 female

recode female 1=0
recode female 2=1
gen female_miss = 1 if missing(female)
replace female_miss = 0 if missing(female_miss)

ren q19 hispanic
recode hispanic 2=0
gen hisp_miss = 1 if missing(hispanic)
replace hisp_miss = 0 if missing(hisp_miss)

gen age = 2019 - q13
gen age_18_to_24 = 1 if age_years >=18 & age_years <=24
gen age_25_to_34 = 1 if age_years >=25 & age_years <=34
gen age_35_to_44 = 1 if age_years >=35 & age_years <=44
gen age_45_to_54 = 1 if age_years >=45 & age_years <=54
gen age_55_to_64 = 1 if age_years >=55 & age_years <=64
gen age_65_plus = 1 if age_years >65
gen age_miss = 1 if missing(age_years)

replace age_18_to_24 = 0 if missing(age_18_to_24)
replace age_25_to_34 = 0 if missing(age_25_to_34)
replace age_35_to_44 = 0 if missing(age_35_to_44)
replace age_45_to_54 = 0 if missing(age_45_to_54)
replace age_55_to_64 = 0 if missing(age_55_to_64)
replace age_65_plus = 0 if missing(age_65_plus)
replace age_miss = 0 if missing(age_miss)

gen black = 1 if q17_2 ==1
replace black=0 if missing(black)
gen other_race=1 if q17_3==1 | q17_4==1 | q17_5==1 | q17_6==1
replace other_race=0 if missing(other_race)

replace female_miss = 0 if missing(female_miss)

replace edu_miss = 0 if missing(edu_miss)
replace race_missing = 0 if missing(race_missing)

gen less_high_school = 1 if educ <= 8
gen hs_grad = 1 if educ==9
gen some_college = 1 if educ==10
gen college_grad =1 if educ >=11
replace less_high_school = 0 if missing(less_high_school)
replace hs_grad = 0 if missing(hs_grad)
replace some_college = 0 if missing(some_college)
replace college_grad = 0 if missing(college_grad)
gen edu_miss =1 if missing(educ)
replace edu_miss=0 if missing(edu_miss)

gen inc_less_30 = 1 if q26==1 | q26==2
gen inc_30_to_50 =1 if q26==4 | q26==5
gen inc_50_to_75 = 1 if q28==1
gen inc_75_to_100=1 if q28==2
gen inc_100_plus=1 if q28==3| q28==4
replace inc_less_30=0 if missing(inc_less_30)
replace inc_30_to_50 =0 if missing(inc_30_to_50)
replace inc_50_to_75 =0 if missing(inc_50_to_75)
replace inc_75_to_100 =0 if missing(inc_75_to_100)
replace inc_100_plus =0 if missing(inc_100_plus)

gen region_ne = 1 if region4==0
gen region_mw = 1 if region4==1
gen region_south = 1 if region4==2
gen region_west = 1 if region4==3
replace region_ne = 0 if missing(region_ne)
replace region_mw = 0 if missing(region_mw)
replace region_south = 0 if missing(region_south)
replace region_west = 0 if missing(region_west)
gen region_missing = 1 if region_ne==0 & region_mw ==0 & region_south ==0 & region_west ==0
replace region_missing = 0 if missing(region_missing)

recode gen_trust 2=.75
recode gen_trust 3=.5
recode gen_trust 4=.25
recode gen_trust 5=0

recode gen_fair 5=0
recode gen_fair 4=.25
recode gen_fair 3=.5
recode gen_fair 2=.75
recode gen_fair 1=1

recode gen_help 1=1
recode gen_help 2=.75
recode gen_help 3=.5
recode gen_help 4=.25
recode gen_help 5=0

gen dv = (gen_trust + gen_fair + gen_help)/3

preserve
keep if wave_2==0
reg dv condition_ost female female_miss hispanic hisp_miss black other_race race_missing hs_grad some_college college_grad edu_miss inc_30_to_50 inc_50_to_75 inc_75_to_100 inc_100_plus inc_missing region_mw region_south region_west region_missing
ttest dv, by(condition)
esize twosample dv, by(cond) all
restore

preserve
keep if wave_2==1
reg dv condition_ost female female_miss hispanic hisp_miss black other_race race_missing hs_grad some_college college_grad edu_miss inc_30_to_50 inc_50_to_75 inc_75_to_100 inc_100_plus inc_missing region_mw region_south region_west region_missing
ttest dv, by(condition)
esize twosample dv, by(cond) all
restore


reg dv condition_ost female female_miss hispanic hisp_miss black other_race race_missing hs_grad some_college college_grad edu_miss inc_30_to_50 inc_50_to_75 inc_75_to_100 inc_100_plus inc_missing region_mw region_south region_west region_missing
ttest dv, by(condition)
esize twosample dv, by(cond) all
