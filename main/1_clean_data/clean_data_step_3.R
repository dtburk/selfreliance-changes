library(data.table)
library(stringr)
library(foreign)

# CLEANING TO PREPARE FOR MULTIPLE IMPUTATION OR NOT?

# imputation <- FALSE
imputation <- TRUE

cargs <- commandArgs(trailingOnly = TRUE)
cargs <- str_subset(cargs, "imputation")
if(length(cargs) == 1) imputation <- str_detect(cargs, "TRUE")

if(imputation) {
    STEP_2_DATA = "main/1_clean_data/cleaned_data_step_2.Rdata"
} else {
    STEP_2_DATA = "main/1_clean_data/cleaned_data_step_2_no_imputation.Rdata"
}

if(!file.exists(STEP_2_DATA)) stop("Cannot perform step 3 of data cleaning without data from step 2.")


source("functions/dss.R")
source("functions/do_string.R")

load(STEP_2_DATA)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Adjusting for inflation...\n"))
# Adjust for inflation (from Table 2.3.4 http://www.bea.gov/iTable/iTable.cfm?ReqID=9&step=1#reqid=9&step=3&isuri=1&910=x&911=0&903=64&904=1969&905=2011&906=a )
pce <- c("1969"=21.326, "1970"=22.325, "1971"=23.274, "1979"=39.714, "1980"=43.978, 
         "1981"=47.908, "1989"=64.641, "1990"=67.440, "1991"=69.652, "1999"=81.110, 
         "2000"=83.131, "2001"=84.736, "2009"=100.000, "2010"=101.653, "2011"=104.149)

pce <- data.table(year=as.integer(names(pce)), pce=pce)
setkey(pce, year)
setkey(d, year)
d <- pce[d]
rm(pce)

money_vars <- c("proptax", "stampval", "spmcaphous", "ffngcare", 
                "ffngcaid", "ftotval", "inctot", "incwage", "incbus",
                "incfarm", "incss", "incwelfr", "incgov", "incidr", 
                "incaloth", "incretir", "incssi", "incdrt", "incint", 
                "incunemp", "incwkcom", "incvet", "incdivid", "incrent", 
                "inceduc", "incchild", "incalim", "incasist", "incother", 
                "incdisa1", "incdisa2", "inclongj", "increti1", "increti2", 
                "incsurv1", "incsurv2", "oincbus", "oincfarm", "oincwage", 
                "adjginc", "capgain", "caploss", "eitcred", "fedtax", 
                "fedtaxac", "statetax", "stataxac", "taxinc")


for(varname in money_vars) {
    do_string(dss("d[ , %s := as.integer(100*%s/pce)]", c(varname, varname)))
}

rm(money_vars)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Identifying and labeling subfamilies...\n"))
# Unified approach to identifying related subfamilies and unrelated 
# subfamily heads.

# Flag married people with missing spouse (sploc==0)
setkey(d, year, serial)
# table(d[ , length(which(marst=="Married, spouse present" & sploc==0)), by=key(d)][ , V1])
# There are 132 pairs of people in the same hh who are married and have sploc==0,
# and 79 people who are the only one in the hh with sploc==0.
# I will just change marst to "Married, spouse absent" and marr_cohab to
# "Not married or cohabiting" for these individuals
d[marst=="Married, spouse present" & sploc==0, marst := "Married, spouse absent"]
d[marst=="Married, spouse absent" & cohabp1==FALSE & cohabp2==FALSE, marr_cohab := "Not married or cohabiting"]

# Load nchild
nchild <- data.table(read.dta("original_data/cps_nchild.dta"))
setkey(nchild, year, serial, pernum)
setkey(d, year, serial, pernum)
d <- nchild[d]
rm(nchild)

# 1. First, flag anyone who is married/cohabiting (marr_cohab) and flag
#    single parents (not married or cohabiting but nchild > 0).
d[ , marr_cohab := factor(ifelse(marst=="Married, spouse present" | cohabp1 | cohabp2, 
                                 "Married or cohabiting", "Not married or cohabiting"))]
d[ , single_parent := marr_cohab=="Not married or cohabiting" & nchild != "0 children present"]

# 2. Create child flags for anyone with (momloc > 0 | poploc > 0) & 
#    marr_cohab=="Not married or cohabiting" & nchild=="0 children present"
d[ , child_w_mom := momloc > 0 & poploc==0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]
d[ , child_w_pop := momloc==0 & poploc > 0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]
d[ , child_w_both := momloc > 0 & poploc > 0 & marr_cohab=="Not married or cohabiting" & nchild=="0 children present"]

# 3. Create a pnloc variable that equals sploc for married people and 
#    equals the pernum of cohabp2 for cohabp1 and vice versa.
d[ , pnloc := ifelse(marst=="Married, spouse present", sploc, 0L)]
setkey(d, year, serial)
d[ , cohabp1_pernum := pernum[cohabp1], by=key(d)]
d[ , cohabp2_pernum := pernum[cohabp2], by=key(d)]
d[cohabp1==TRUE, pnloc := cohabp2_pernum]
d[cohabp2==TRUE, pnloc := cohabp1_pernum]
d[ , cohabp1_pernum := NULL]
d[ , cohabp2_pernum := NULL]
# table(d[marr_cohab=="Married or cohabiting", pnloc])

# 4. Create a subfamid variable that is:
#    - the smaller of pnloc and pernum for marr_cohab people;
d[marr_cohab=="Married or cohabiting", subfamid := mapply(min, pnloc, pernum)]

#    - pernum for single parents;
d[single_parent==TRUE, subfamid := pernum]

#    - pernum for unmarried head/householders with no children present;
d[relate=="Head/householder" & marr_cohab=="Not married or cohabiting" & nchild=="0 children present", subfamid := pernum]

#    - subfamid of mom for children with only mom present, subfamid of pop for children with only pop present, and same subfamid of both parents if both are present;
get_subfamid <- function(subset, pernums) {
    do_string(dss("xx <- d[%s, list(year, serial, %s)]", c(subset, pernums)))
    setnames(xx, pernums, "pernum")
    setkey(xx, year, serial, pernum)
    setkey(d, year, serial, pernum)
    d[xx][ , subfamid]
}

d[child_w_mom==TRUE, subfamid := get_subfamid("child_w_mom==TRUE", "momloc")]
d[child_w_pop==TRUE, subfamid := get_subfamid("child_w_pop==TRUE", "poploc")]

d[child_w_both==TRUE, mom_subfamid := get_subfamid("child_w_both==TRUE", "momloc")]
d[child_w_both==TRUE, pop_subfamid := get_subfamid("child_w_both==TRUE", "poploc")]
# table(d[child_w_both==TRUE, mom_subfamid==pop_subfamid])
# The preceding table shows that the mom and dad of all children with both present
# have the same subfamid value; so we can just assign that of mom.
d[child_w_both==TRUE, subfamid := mom_subfamid]
d[ , mom_subfamid := NULL]
d[ , pop_subfamid := NULL]

rm(get_subfamid)

#    - subfamid of the head for relatives of the head who aren't marr_cohab, single parents, or children;
d[ , relative_or_foster_child_of_head := relate %in% c("Head/householder", "Spouse", "Child", 
                                                       "Stepchild", "Parent", "Sibling", 
                                                       "Grandchild", "Other relatives, n.s",
                                                       "Foster children")]
setkey(d, year, serial)
d[ , subfamid_of_head := subfamid[which(relate=="Head/householder")], by=key(d)]
d[relative_or_foster_child_of_head==TRUE & marr_cohab=="Not married or cohabiting" & 
      single_parent==FALSE & momloc==0 & poploc==0, subfamid := subfamid_of_head]

#    - and own pernum for those who aren't marr_cohab, single parents, children, or relatives of the head.
d[relative_or_foster_child_of_head==FALSE & marr_cohab=="Not married or cohabiting" &
      single_parent==FALSE & momloc==0 & poploc==0, subfamid := pernum]

d[ , n_subfam := length(unique(subfamid)), by=key(d)]

# Load ftype
ftype <- data.table(read.dta("original_data/cps_ftype.dta"))
setkey(ftype, year, serial, pernum)
setkey(d, year, serial, pernum)
d <- ftype[d]
rm(ftype)

# Create fam_head
d[ , fam_head := pernum==subfamid]
# table(d$fam_head)
setkey(d, year, serial, subfamid)
nrow(unique(d))
# Good: 977,359 family heads and the same number of subfamilies

if(imputation) {
    save(d, file="main/1_clean_data/cleaned_data_step_3.Rdata")
} else {
    save(d, file="main/1_clean_data/cleaned_data_step_3_no_imputation.Rdata")
}
