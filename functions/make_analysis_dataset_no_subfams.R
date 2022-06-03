make_analysis_dataset <- function(data, imputed=FALSE, data_to_merge=NULL, 
                                  no_cohab = FALSE) {
    
    require(data.table)
    source("functions/limit_age_range.R")
    
    if(!is.null(data_to_merge)) {
        setkey(data, year, serial, pernum)
        setkey(data_to_merge, year, serial, pernum)
        data <- data_to_merge[data]
    }
    
    
    # Make sure all income vars are integers
    income_vars <- c("eitcred", "fedtax", "adjginc", "taxinc", 
                     "fedtaxac", "capgain", "caploss", "statetax", "stataxac", 
                     "incdivid", "incint", "incrent", "incother", "incalim", "incasist", 
                     "incss", "incwelfr", "incwkcom", "incvet", "incchild", "incunemp", 
                     "inceduc", "incssi", "incwage", "incbus", "incfarm", "incsurv1", 
                     "incsurv2", "incdisa1", "incdisa2", "increti1", "increti2", 
                     "proptax", "incdrt", "incgov", "incidr", "incaloth", "oincwage", 
                     "oincbus", "oincfarm", "inclongj")
    
    for(n in income_vars) {
        do_string(dss("data[ , %s := as.integer(round(%s))]", c(n, n)))
    }
    
    
    # data[year %in% 1979:1981, incwelfr := 100*.incwelfr/pce]
    
    
    data[year >= 1989, incwage := oincwage + inclongj*(srcearn=="Wage and salary")]
    data[year >= 1989, incbus := oincbus + inclongj*(srcearn=="Self employment")]
    data[year >= 1989, incfarm := oincfarm + inclongj*(srcearn=="Farm self employment")]
    
    data[ , incdisab := ifelse(year >=1989, incdisa1 + incdisa2, as.integer(NA))]
    data[ , incretir := ifelse(year >= 1989, increti1 + increti2, as.integer(NA))]
    data[ , incsurv := ifelse(year >=1989, incsurv1 + incsurv2, as.integer(NA))]
    
    
    if(imputed) {
        
        data[year %in% 1979:1981 & !is.na(incwelfr_alloc), incwelfr := incwelfr_alloc]
        
        # Set negative imputed values to zero for items that don't allow
        # negative values (I judged this based on whether there were any negative
        # values in the observed data).
        no_negatives <- c("capgain", "caploss", "incdivid", "incint", "incother",
                          "incalim", "incasist", "incss", "incwelfr", "incwkcom",
                          "incvet", "incchild", "incunemp", "inceduc", "incssi", 
                          "incwage", "incsurv", "incdisab", "incretir", "incdrt", 
                          "incgov", "incaloth", "wkswork1", "hrswork", "uhrswork")
        
        for(n in no_negatives) {
            do_string(dss("data[%s < 0, %s := 0L]", c(n, n)))
        }
    }
    
    
    .sum <- function(...) sum(..., na.rm=TRUE)
    
    data[ , labern := as.integer(.sum(incwage, incbus, incfarm)), by=.(year, serial, pernum)]
    
    data[labern < 0, labern := 0L] # labern must not be negative
    
    data[ , unearned_non_gov := as.integer(.sum(incidr, incaloth, incretir, 
                                                incdrt, incint, incdivid, 
                                                incrent, incchild, incalim, 
                                                incasist, incother)), 
          by=.(year, serial, pernum)]
    
    data[ , unearned_gov := as.integer(.sum(incss, incwelfr, incgov, incssi,
                                            incunemp, incwkcom, incvet, 
                                            inceduc, incdisab, incsurv)), 
          by=.(year, serial, pernum)]
    
    if("posern" %in% names(data)) data[ , posern := NULL]
    
    data[ , posern := labern > 0]
    
    data[ , inctot := as.integer(.sum(incwage, incbus, incfarm, incss, incwelfr, incgov, 
                                      incidr, incaloth, incretir, incssi, incdrt, incint,
                                      incunemp, incwkcom, incvet, incdivid, incrent, 
                                      inceduc, incchild, incalim, incasist, incother, 
                                      incdisab, incsurv)), 
          by=.(year, serial, pernum)]
    
    if(any(is.na(data$inctot))) stop(sprintf("inctot has NA values in decade %d", yr))
    
    data[ , tax := -as.integer(round((fed_inc_tax_not_inflation_adj+fica_not_inflation_adj)*100/pce))]
    
    partner <- data[pnloc > 0, .(year, serial, pernum=pnloc, pn_labern=labern, 
                                 pn_posern=posern, pn_age=age)]
    setkey(partner, year, serial, pernum)
    setkey(data, year, serial, pernum)
    data <- partner[data]
    
    data[ , fam_unearned_non_gov := .sum(unearned_non_gov), 
          by=.(year, serial)]
    data[ , fam_unearned_gov := .sum(unearned_gov), 
          by=.(year, serial)]
    data[ , fam_tax := .sum(tax), by=.(year, serial)]
    data[ , hd_1_labern := labern[fam_head], 
          by=.(year, serial)]
    data[ , hd_2_labern := labern[hd_or_pn & !fam_head], 
          by=.(year, serial)]
    data[ , oth_labern := .sum(labern[!hd_or_pn & !fam_head]), 
          by=.(year, serial)]
    
    data[ , other_inc := as.integer(.sum(fam_unearned_non_gov, 
                                         fam_unearned_gov, 
                                         fam_tax, 
                                         oth_labern)), by=.(year, serial, pernum)]
    
    data[ , fam_inc := .sum(hd_1_labern, hd_2_labern, other_inc), 
          by=.(year, serial, pernum)]
    
    # Count how many families have negative income, and remove them
    neg_fam_inc <- nrow(data[ , .(fam_inc=fam_inc[1]), by=.(year, serial)][fam_inc < 0,])
    cat("Removing", neg_fam_inc, "families with negative income.\n")
    data <- data[fam_inc >= 0, ]
    
    data[ , n_children_under_18 := sum(age < 18), 
          by=.(year, serial)]
    
    data[ , n_children_under_5 := sum(age < 5), 
          by=.(year, serial)]
    
    # Flag families with never-married children over age 25
    setkey(data, year, serial)
    
    data[ , child_25_or_over := relate == "Child" & age >= 25 & marst == "Never married/single"]
    data[ , fam_has_adult_child := any(child_25_or_over), by = key(data)]
    
    # Remove all but heads and partners
    data <- heads_and_partners_only(data)
    
    data[ , n_earners := .sum(posern, pn_posern), by=.(year, serial, pernum)]
    
    data[n_earners==0, num_earners := "Zero earners"] 
    data[n_earners==1, num_earners := "One earner"]
    data[n_earners==2, num_earners := "Two earners"]
    
    data[ , group := sex:marr_cohab:num_earners]
    
    data[ , group := as.factor(as.character(group))]
    
    data[pnloc==0, pn_labern := 0L]
    
    if(max(abs(data[ , fam_inc - labern - pn_labern - other_inc])) != 0) stop("Sum of components of family income do not equal family income.")
    if(max(abs(data[ , other_inc - fam_tax - fam_unearned_non_gov - fam_unearned_gov - oth_labern])) != 0) stop("Sum of components of other income do not equal other income.")
    
    # Create full-time, full-year variables
    # Hours variables: 
    # hrswork (hours worked last week)
    # uhrswork (hours usually worked per week last year; NA for 1970)
    # Weeks variables:
    # wkswork1 (integer; NA for 1970)
    # wkswork2 (ordered factor)
    # Full-time = hours >= 35
    data[year %in% 1969:1971, ft_hrs := hrswork >= 35]
    data[year > 1971, ft_hrs := uhrswork >= 35]
    # Full-year = weeks >= 50
    data[year %in% 1969:1971, fyr_wks := wkswork2=="50-52 weeks"]
    data[year > 1971, fyr_wks := wkswork1 >= 50]
    
    data[ , full_time_full_year := ft_hrs & fyr_wks]
    
    # Can't be full_time_full_year if incwage, incbus, and incfarm are all zero
    data[ , full_time_full_year := full_time_full_year & (incwage != 0 | incbus != 0 | incfarm != 0)]
    
    # Use cohabp1 and cohabp2 to create pn_age for cohabitors when cohabitation is
    # ignored, so that we can exclude the same people we would have based on partner
    # age
    if (no_cohab) {
        data[ , cohabp1_age := age[cohabp1], by = .(year, serial)]
        data[ , cohabp2_age := age[cohabp2], by = .(year, serial)]
        data[data$cohabp1, pn_age := cohabp2_age]
        data[data$cohabp2, pn_age := cohabp1_age]
    }
    
    data <- data[ , .(year, serial, pernum, pnloc, sex, marr_cohab, 
                      num_earners, group, sqrt_hh_size, n_children_under_18,
                      n_children_under_5, age, pn_age, educ, race, hispan, 
                      labern, pn_labern, other_inc, fam_unearned_non_gov, 
                      fam_unearned_gov, fam_tax, oth_labern, fam_inc, wtsupp, 
                      full_time_full_year, fam_has_adult_child, race, region)]
    
    # Limit to persons 25-54
    data <- limit_age_range(data)
    
    # Set wtsupp to 0 if it is negative (only applies to 56 cases in 1969-1971)
    data[wtsupp < 0, wtsupp := 0]
    
    return(data)
}

heads_and_partners_only <- function(data) {
    data[hd_or_pn == TRUE, ]
}