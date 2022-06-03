#############################################################################
# Create analysis variables and save datasets with only necessary variables #
#############################################################################
library(data.table)
library(ipumsr)

source("functions/make_analysis_dataset_no_subfams.R")

dss <- function(text, subs) {
    split = strsplit(text, "%s")[[1]]
    if(grepl("%s$", text)) split <- c(split, "")
    if (length(split) - 1 != length(subs)) {
        stop("Number of wildcard characters does not equal number of variables to substitute.")
    }
    return(paste0(split, c(subs, ""), collapse=""))
}

do_string <- function(x) {
    eval(parse(text=x), envir=parent.frame())
}

load("main/1_clean_data/cleaned_data_step_5_no_imputation.Rdata")

# Add region
ddi <- read_ipums_ddi("original_data/add_region.xml")
region <- read_ipums_micro(ddi, data_file = "original_data/add_region.dat.gz")
region <- data.table(region)
setnames(region, names(region), c("year", "serial", "region", "pernum"))
setkey(region, year, serial, pernum)

setkey(d, year, serial, pernum)
d <- region[d]
rm(region)

for(n in "filestat") {
    do_string(dss("d[ , %s := NULL]", n))
}


# Add FICA
ddi <- read_ipums_ddi("original_data/add_fica.xml")
fica <- read_ipums_micro(ddi, data_file = "original_data/add_fica.dat.gz")
setDT(fica)
setnames(fica, names(fica), tolower(names(fica)))
fica <- fica[ , .(year, serial, pernum, fica)]
setkey(fica, year, serial, pernum)

d <- fica[d]

# Rename copies of fedtax and fica to match what is expected by 
# `make_analysis_dataset()`:
d[ , fed_inc_tax_not_inflation_adj := fedtax]
d[ , fica_not_inflation_adj := fica]


d <- make_analysis_dataset(d)

save(
    d, 
    file="main/6b_make_analysis_dataset_non_imputed/1b_non_imputed_non_taxsim_analysis_vars.Rdata"
)
