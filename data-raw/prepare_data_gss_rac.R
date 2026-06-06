library("gssr")
library("data.table")
data(gss_all)

# RACDIN: whether blacks are welcome for dinner
# RACSEG: whether whites have a right to keep blacks out of their neighborhoods
# RACPUSH: whether blacks should not "push" themselves where they are not wanted
# RACMAR: whether there should be laws against black-white marriage

gss_rac <- as.data.table(gss_all)
rm("gss_all")

gss_rac <- gss_rac[
    year %in% c(1972, 1976, 1980, 1984) & race == 1,
    .(year, age, wtssall, racdin, racseg, racpush, racmar, region, sex)
]
gss_rac[, `:=`(id = .I, cohort = year - age)]
gss_rac <- haven::zap_labels(gss_rac)

# Negate and z-score each item so higher values = more tolerant
for (col in c("racdin", "racseg", "racpush", "racmar")) {
    gss_rac[, (col) := -1 * scale(get(col))[, 1]]
}

# Sum items; impute scale for up to 2 missing items
gss_rac[, n_na := rowSums(is.na(.SD)), .SDcols = c("racdin", "racseg", "racpush", "racmar")]
gss_rac[, rac := rowSums(.SD, na.rm = TRUE), .SDcols = c("racdin", "racseg", "racpush", "racmar")]
gss_rac[n_na == 1, rac := rac * 4 / 3]
gss_rac[n_na == 2, rac := rac * 2]

gss_rac <- gss_rac[n_na <= 2 & !is.na(cohort)]
gss_rac[, rac := rac + 6]
gss_rac <- gss_rac[, .(rac, year, age, cohort, region, sex, wtssall)]

save(gss_rac, file = "../data/gss_rac.rda", version = 2, compress = "bzip2")
