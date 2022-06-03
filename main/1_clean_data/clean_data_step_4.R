library(data.table)
library(stringr)

# CLEANING TO PREPARE FOR MULTIPLE IMPUTATION OR NOT?

imputation <- FALSE
# imputation <- TRUE

cargs <- commandArgs(trailingOnly = TRUE)
cargs <- str_subset(cargs, "imputation")
if(length(cargs) == 1) imputation <- str_detect(cargs, "TRUE")

if(imputation) {
    STEP_3_DATA = "main/1_clean_data/cleaned_data_step_3.Rdata"
} else {
    STEP_3_DATA = "main/1_clean_data/cleaned_data_step_3_no_imputation.Rdata"
}


if(!file.exists(STEP_3_DATA)) stop("Cannot perform step 4 of data cleaning without data from step 3.")

load(STEP_3_DATA)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Creating analysis variables...\n"))
# Create labern
d[ , labern := apply(d[ , list(incwage, incbus, incfarm)], 1, sum, na.rm=TRUE)]

# Create posern variable
d[ , posern := factor(ifelse(labern > 0, "Has positive earnings", "Has zero or negative earnings"))]


# Create partner variables (income, employment status (to create 0, 1, or 2 wage earners variables))
# What percent of households has a person with farm or self-employment income?
# table(d[ , any((relate %in% c("Head/householder", "Spouse") | cohabp1==1 | cohabp2==1) & 
#                    (incbus != 0 | incfarm != 0 | oincbus != 0 | oincfarm != 0)), by=key(d)][ , V1])
# length(which(is.na(d[ , any((relate %in% c("Head/householder", "Spouse") | cohabp1==1 | cohabp2==1) & 
#                                 (incbus != 0 | incfarm != 0 | oincbus != 0 | oincfarm != 0)), by=key(d)][ , V1])))
# In 101,442 households, the head or spouse or partner has some non-zero business
# or farm income. That's 11.2%.

# I think we are leaning toward keeping self-employment income as part of labor earnings.


# Create hd_or_pn
# DB May 2022 update: we only want this to flag heads and their partners
d[ , hd_or_pn := fam_head | relate == "Spouse" | cohabp2]

# Create non_earnings_income
nei_69_71 <- c("incss", "incwelfr", "incgov", "incidr", "incaloth")

nei_79_81 <- c("incss", "incwelfr", "incgov", "incaloth", "incretir", 
               "incssi", "incdrt", "incint")

nei_89_11 <- tolower(c("incss", "incwelfr", "INCRETI1", 
                       "INCRETI2", "incssi", "incint", "incunemp", "incwkcom", "incvet", "INCSURV1", 
                       "INCSURV2", "INCDISA1", "INCDISA2", "incdivid", "incrent", "inceduc", "incchild", 
                       "incalim", "incasist", "incother"))

d[year < 1979, non_earnings_income := apply(d[year < 1979, nei_69_71, with=FALSE], 1, sum, na.rm=TRUE)]
d[year %in% 1979:1981, non_earnings_income := apply(d[year %in% 1979:1981, nei_79_81, with=FALSE], 1, sum, na.rm=TRUE)]
d[year %in% 1989:2011, non_earnings_income := apply(d[year %in% 1989:2011, nei_89_11, with=FALSE], 1, sum, na.rm=TRUE)]

rm(nei_69_71, nei_79_81, nei_89_11)

# Create inc_sum
d[ , inc_sum := apply(d[ , list(labern, non_earnings_income)], 1, sum, na.rm=TRUE)]

# Create hhinc: the sum of income of all household members
setkey(d, year, serial)
d <- d[ , list(hhinc=sum(inc_sum, na.rm=TRUE)), by=key(d)][d]


# To create partner vars: Extract pnloc, posern, and labern, and rename 
# them to prepare for reintegration as partner variables.


# DB 5/9/2022, updating to not use pointer variables:
#####################################################
# Let's create a crude version of pnloc for heads <-> spouses and 
# cohabp1 <-> cohabp2
setkey(d, year, serial)
d[ , pernum_of_head := pernum[relate == "Head/householder"], by = key(d)]
d[ , pernum_of_spouse := pernum[relate == "Spouse"], by = key(d)]
d[ , pernum_of_cohabp1 := pernum[cohabp1], by = key(d)]
d[ , pernum_of_cohabp2 := pernum[cohabp2], by = key(d)]
d[ , pnloc := 0L]
d[relate == "Head/householder", pnloc := pernum_of_spouse]
d[relate == "Spouse", pnloc := pernum_of_head]
d[cohabp1 == TRUE, pnloc := pernum_of_cohabp2]
d[cohabp2 == TRUE, pnloc := pernum_of_cohabp1]
d[is.na(pnloc), pnloc := 0L]

d[ 
    , 
    c(
        "pernum_of_head", "pernum_of_spouse", "pernum_of_cohabp1", 
        "pernum_of_cohabp2"
    ) := NULL
]


pn_vars <- d[pnloc > 0, list(year=year, serial=serial, pernum=pnloc, pn_posern=posern, pn_labern=labern, pn_sex=sex)]
setkey(d, year, serial, pernum)
setkey(pn_vars, year, serial, pernum)
d <- pn_vars[d]
rm(pn_vars)


# Create num_earners
setkey(d, year, serial)
d[
    ,
    is_an_earner := (fam_head & posern == "Has positive earnings") |
        (relate == "Spouse" & posern == "Has positive earnings") |
        (cohabp2 & posern == "Has positive earnings")
]

d[ , num_earners := sum(is_an_earner), by = key(d)]

d[ , num_earners := as.character(num_earners)]
d[num_earners == "0", num_earners := "Zero earners"]
d[num_earners == "1", num_earners := "One earner"]
d[num_earners == "2", num_earners := "Two earners"]
d[ , num_earners := as.factor(num_earners)]


# Create group = sex:marr_cohab:num_earners
d[ , sex := as.factor(sex)]
d[ , group := sex:marr_cohab:num_earners]

# Create age_group
d[!is.na(age), age_group := factor(sapply(age, function(x) 
    if(x < 15) "Under 15" else 
        if(x < 20) "15-19" else
            if(x < 25) "20-24" else
                if(x < 30) "25-29" else
                    if(x < 35) "30-34" else
                        if(x < 40) "35-39" else 
                            if(x < 45) "40-44" else
                                if(x < 50) "45-49" else 
                                    if(x < 55) "50-54" else
                                        if(x < 60) "55-59" else
                                            if(x < 65) "60-64" else
                                                if(x < 70) "65-69" else
                                                    if(x < 75) "70-74" else
                                                        if(x < 80) "75-79" else
                                                            "80 plus"
))]

# Create age_group by sex interaction
d[ , age_group_by_sex := sex:age_group]

# Create sqrt_hh_size and sqrt_famsize
setkey(d, year, serial)
d[ , sqrt_hh_size := sqrt(.N), by=.(year, serial)]
# d[ , sqrt_famsize := sqrt(.N), by=.(year, serial, subfamid)]

# If sex is missing (because of qsex) and pnloc > 0, make sex equal the opposite of pn_sex
d[is.na(sex) & pnloc > 0, sex := ifelse(pn_sex=="Male", "Female", "Male")]

if(imputation) {
    save(d, file="main/1_clean_data/cleaned_data_step_4.Rdata")
} else {
    save(d, file="main/1_clean_data/cleaned_data_step_4_no_imputation.Rdata")
}

