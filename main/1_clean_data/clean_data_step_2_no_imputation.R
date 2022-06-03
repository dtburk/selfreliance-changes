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

# Make data quality variable key
q_flags <- names(d)[grep("^q", names(d))]
get_var_name_from_flag_name <- function(x) {
    pattern <- paste0("^", ifelse(grepl("\\d$", x), substr(x, 2, 7), substr(x, 2, nchar(x))))
    names(d)[grep(pattern, names(d))]
}
names(q_flags) <- sapply(q_flags, get_var_name_from_flag_name)

rm(get_var_name_from_flag_name)

# Fix data quality flag names that don't match the pattern
change_name <- function(flag, new_name) names(q_flags)[which(q_flags==flag)] <<- new_name
change_name("qwkswork", "wkswork1")
q_flags["wkswork2"] <- "qwkswork"
change_name("qincassi", "incasist")
change_name("qincdis1", "incdisa1")
change_name("qincdis2", "incdisa2")
change_name("qincret1", "increti1")
change_name("qincret2", "increti2")
change_name("qincreti", "incretir")
change_name("qincss", "incss")
change_name("qincsur1", "incsurv1")
change_name("qincsur2", "incsurv2")

rm(change_name)

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