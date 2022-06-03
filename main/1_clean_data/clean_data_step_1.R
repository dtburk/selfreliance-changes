library(data.table)
library(foreign)

source("functions/fix_na_niu_values.R")

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Loading data...\n"))
load("original_data/cps_master.Rdata")

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Fixing NA and NIU values...\n"))
d <- fix_na_niu_values(d)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Misc cleaning...\n"))
# Make all names lowercase
setnames(d, names(d), tolower(names(d)))

# Remove spouse variables
d <- d[ , names(d)[!grepl("_sp$", names(d))], with=FALSE]

# Make age consistent over time
# Top codes:
# 1962-1987: 99+ coded as 99
# 1988-2001: 90+ coded as 90
# 2002-2003: 80+ coded as 80
# 2004-onward: 80-84 coded as 80, 85+ coded as 85
d[age=="Under 1 year", age := "0"]
d[age=="99+", age := "99"]
d[age=="90 (90+, 1988-2002)", age := "90"]
d[year >=2009 & age==80, age := "82"]
d[ , age := as.integer(as.character(age))]

# Load and attach group quarters variable
gq <- data.table(read.dta("original_data/cps_group_quarters.dta"))
setkey(gq, year, serial, pernum)
setkey(d, year, serial, pernum)
d <- gq[d]
rm(gq)

cat(paste0(format(Sys.time(), format="%H:%M:%S"), "    Identifying cohabitators...\n"))
# Identify cohabitators
# We define Adjusted POSSLQ households as those that
# meet the following criteria:  They contain (1) a reference  per-
#    son (householder); (2) one other adult (age 15+) of the oppo-
#    site sex who is not in a related subfamily,  not a secondary
# individual in group quarters, and not related to or a foster
# child of the reference  person; and (3) no other adults (age
# 15+) except foster children,  children or other relatives of the
# reference  person, or children of unrelated subfamilies.
# This definition  still excludes households in which a ref-
# erence person lives with a cohabiting partner and that
# partner's nonchild relatives.

d[ , cohabp1 := age > 14 & relate == "Head/householder" & marst != "Married, spouse present"]
d[ , cohabp2 := age > 14 & relate %in% c("Partner/roommate", "Unmarried partner", "Housemate/roomate",
                                         "Roomer/boarder/lodger", "Other nonrelatives") & marst != "Married, spouse present"]

# table(d$cohabp1)
# table(d$cohabp2)

setkey(d, year, serial)

d[ , hhcohabp1 := sum(cohabp1), by = key(d)]
d[ , hhcohabp2 := sum(cohabp2), by = key(d)]

# table(d$hhcohabp1)
# table(d$hhcohabp2)

# No households with more than one cohabp1

d[hhcohabp1==1, cohabp1_sex := sex[which(cohabp1)], by=key(d)]
d[ , cohabp2 := cohabp2 & sex != cohabp1_sex]

# table(d$cohabp2)

# This reduces potential cohabp2's from 73,942 to 40,438

# recalculate hhcohabp2
d[ , hhcohabp2 := NULL]
d[ , hhcohabp2 := sum(cohabp2), by=key(d)]

# table(d$hhcohabp2)

d[, hhcohabstep1 := hhcohabp1==1 & hhcohabp2 > 0]
# a = d[ , list(step1=hhcohabstep1[1], n_cohab2=hhcohabp2[1]), by = key(d)]
# table(a[ , list(step1, n_cohab2)])
# rm(a)

#           n_cohab2
#   step1         0      1      2      3      4      5      6      7
#      FALSE 864154      0      0      0      0      0      0      0
#      TRUE       0  36112   1731    221     34      8      3      1

# Do any cohabp2's have a mother or father in the household? That would suggest that
# the relate variable is not capturing subfamily relationships
# table(d[cohabp2==TRUE, momloc > 0 | poploc > 0])
# Yes, there are 571 cohabp2's with a mother or father in their household.
setorder(d, year, serial, pernum)
setkey(d, year, serial)
# table(d[ , list(n_cohabp2_moms=length(which(momloc[cohabp2==TRUE] > 0))), by=key(d)][ , n_cohabp2_moms])
# table(d[ , list(n_cohabp2_pops=length(which(poploc[cohabp2==TRUE] > 0))), by=key(d)][ , n_cohabp2_pops])
# There are 493 households with one cohabp2 with a parent in the household, but also
# some households with more than one such cohabp2.
# What I need to establish is whether the parent(s) of these cohabp2's are related 
# to the householder. If not, then these cohabp2's are children of unrelated
# families and thus cannot be a cohabp2, but they also can't disqualify the 
# household from being classified as cohabiting.
d[ , cohabp2_mom_pernum := paste0(momloc[which(momloc > 0 & cohabp2==TRUE)], collapse="&"), by=key(d)]
d[ , cohabp2_pop_pernum := paste0(poploc[which(poploc > 0 & cohabp2==TRUE)], collapse="&"), by=key(d)]

# table(d$cohabp2_mom_pernum)

#                  10       2     2&2   2&2&2       3     3&3       4     4&4       5     5&5       6 
#     2527424      12    1043     142      15     387      26     184      13     109       7      22 

# table(d$cohabp2_pop_pernum)

#                  11       2     2&2       3     3&3       4       5     5&5       6       7 
#     2528907      13     139       4     103       6     139      44       7      14       8 

# In each case of multiple cohabp2's with a parent in the household, the parent is shared
# (as evidenced by the repeated pernum). So I can change the mom and pop pernum vars:
d[ , cohabp2_mom_pernum := NULL]
d[ , cohabp2_pop_pernum := NULL]
d[ , cohabp2_mom_pernum := momloc[which(momloc > 0 & cohabp2==TRUE)][1], by=key(d)]
d[ , cohabp2_pop_pernum := poploc[which(poploc > 0 & cohabp2==TRUE)][1], by=key(d)]

d[ , cohabp2_mom_relate := relate[which(pernum==cohabp2_mom_pernum)], by=key(d)]
# table(as.character(d$cohabp2_mom_relate))

#     Housemate/roomate    Other nonrelatives      Partner/roommate Roomer/boarder/lodger     Unmarried partner 
#                   139                   625                    17                    20                  1159 

d[ , cohabp2_pop_relate := relate[which(pernum==cohabp2_pop_pernum)], by=key(d)]
# table(as.character(d$cohabp2_pop_relate))

#     Housemate/roomate    Other nonrelatives      Partner/roommate Roomer/boarder/lodger     Unmarried partner 
#                    34                   192                     6                    14                   231

# All of the parents of cohabp2's are non-relatives of the head/householder. That means
# all cohabp2's with momloc > 0 or poploc > 0 are disqualified from being a cohabp2.

d[ , cohabp2 := cohabp2 & momloc==0 & poploc==0]
# table(d$cohabp2)
# That reduces cohabp2's to 39,867

# recalculate hhcohabp2
d[ , hhcohabp2 := NULL]
d[ , hhcohabp2 := sum(cohabp2), by=key(d)]

d[, hhcohabstep1 := hhcohabp1==1 & hhcohabp2 > 0]
# a = d[ , list(step1=hhcohabstep1[1], n_cohab2=hhcohabp2[1]), by = key(d)]
# table(a[ , list(step1, n_cohab2)])
# rm(a)

#            n_cohab2
#     step1        0      1      2      3      4      5      6      7
#       FALSE 864208      0      0      0      0      0      0      0
#       TRUE       0  36511   1359    158     26      7      3      1

# Although the number of cohabp2's went down, the number of households
# with just one cohabp1 and one cohabp2 went up.

# Are there any other adults (age > 14) who are themselves unrelated to the householder
# but aren't foster children or children of unrelated subfamilies?

d[ , cohaboth := age > 14 & relate %in% c("Partner/roommate", "Unmarried partner", "Housemate/roomate",
                                          "Roomer/boarder/lodger", "Other nonrelatives") & momloc==0 &
       poploc==0 & cohabp2==0]

d[ , hhcohaboth := sum(cohaboth), by=key(d)]

# table(d[ , list(step1=hhcohabp1[1]==1 & hhcohabp2[1]==1, any_cohaboth=hhcohaboth[1] > 0), by=key(d)][ , list(step1, any_cohaboth)])

# 1,844 households have just one cohabp1 and cohabp2, but also have another unrelated adult
# that is not a foster child or child of an unrelated subfamily.

d[ , hhcohb := hhcohabp1==1 & hhcohabp2==1 & hhcohaboth==0 & gq != "Group Quarters"]
# table(d[ , hhcohb[1], by=key(d)][ , V1])

# Thus, out of 908,849 households, adjusted POSSLQ identifies 34,659 (3.81%) as cohabitating households

d[ , cohabp1 := cohabp1 & hhcohb]
d[ , cohabp2 := cohabp2 & hhcohb]

# table(d[cohabp2==1 & cohabp1_sex=="Male", sex])
# table(d[cohabp2==1 & cohabp1_sex=="Female", sex])

d[ , cohabp1_sex := NULL]
d[ , hhcohabp1 := NULL]
d[ , hhcohabp2 := NULL]
d[ , hhcohabstep1 := NULL]
d[ , cohaboth := NULL]
d[ , hhcohaboth := NULL]
d[ , cohabp2_mom_relate := NULL]
d[ , cohabp2_pop_relate := NULL]
d[ , cohabp2_mom_pernum := NULL]
d[ , cohabp2_pop_pernum := NULL]
d[ , hhcohb := NULL]


# Now that we've identified cohabitors, we need to add the cohabp2's 
# and their family members to the primary family
d[ , cohabp2_famunit := famunit[cohabp2], by=key(d)]
d[famunit==cohabp2_famunit, famunit := "1st family in household or group quarters"]
d[ , cohabp2_famunit := NULL]

save(d, file="main/1_clean_data/cleaned_data_step_1.Rdata")