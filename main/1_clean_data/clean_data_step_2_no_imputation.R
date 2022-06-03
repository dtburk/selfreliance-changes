STEP_1_DATA = "main/1_clean_data/cleaned_data_step_1.Rdata"

if(!file.exists(STEP_1_DATA)) stop("Cannot perform step 2 of data cleaning without data from step 1.")

library(data.table)

source("functions/dss.R")
source("functions/do_string.R")

load(STEP_1_DATA)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Preparing to recode topcoded and allocated cases as NA...\n"))
# Here are the values indicating no imputation (allocation) (all other values indicate some kind of imputation, of income value or source):
no_alloc_values <- c("NIU/No allocation", "No change / not allocated", "No change or children or armed forces", "No change or children", "No allocation", "Not allocated", "No change")

# Combine qschcol1-3 and remove them
d[ , qschlco := "Not allocated"]
d[!(qschcol1=="No change" & qschcol2=="No change" & qschcol3=="No change"), qschlco := "Allocated"]

d[ , qschcol1 := NULL]
d[ , qschcol2 := NULL]
d[ , qschcol3 := NULL]

# Remove variables that are the sum of two components: incdisab, incsurv, incretir
# But first, assign zero values to components when the sum is zero
# Wait, incretir was its own variable (no components) before 1989, with its own
# topcoding, so treat it differently and don't remove
d[incdisab==0, incdisa1 := 0]
d[incdisab==0, incdisa2 := 0]
d[incsurv==0, incsurv1 := 0]
d[incsurv==0, incsurv2 := 0]
d[incretir==0 & year >= 1989, increti1 := 0]
d[incretir==0 & year >= 1989, increti2 := 0]
d[incretir > 0 & year >= 1989 & is.na(increti1), increti1 := incretir] # Fix two cases with positive incretir and NA increti1

d[ , incdisab := NULL]
d[ , incsurv := NULL]


# DB, 5/5/2022, updating code to not use pointers:
##################################################
# I don't think we need to do anything with the quality flags for the 
# no-imputation track, so I removed that stuff from this script

# Load topcodes and transform into matrix with rownames as varnames and colnames as years
topcodes <- data.table(read.csv("original_data/topcode_values.csv", stringsAsFactors=FALSE))
varnames <- topcodes$var
topcodes[ , var := NULL]
topcodes <- as.matrix(topcodes)
rownames(topcodes) <- tolower(varnames)
colnames(topcodes) <- gsub("^X", "", colnames(topcodes))
rm(varnames)

# Make dummies to flag topcoded cases
cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Making topcode flag variables...\n"))
for(v in rownames(topcodes)) {
    tc_var <- paste0("tc_", v)
    do_string(dss("d[ , %s := 0L]", tc_var))
    for(y in colnames(topcodes)) {
        if(is.na(topcodes[v, y])) next
        do_string(dss("d[year==%s & %s >= %s, %s := 1L]", c(y, v, topcodes[v, y], tc_var)))
    }
}

rm(topcodes, tc_var)

save(d, file="main/1_clean_data/cleaned_data_step_2_no_imputation.Rdata")