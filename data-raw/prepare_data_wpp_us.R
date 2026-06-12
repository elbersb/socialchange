library("tidywpp")
library("data.table")

# US population by single year of age and sex, from UN World Population
# Prospects (WPP) 2022. Counts are in thousands and are *not* rescaled here:
# the decompose_aggregated() population frame only uses the relative cell
# structure, and the vignette rescales each wave to the survey sample size.

d <- get_wpp(
    indicator = "pop", pop_age = "single", pop_sex = "all",
    pop_freq = "annual", clean_names = TRUE, tidy_pop_sex = TRUE,
    messages = FALSE
)
setDT(d)

# all years spanning the gss_homosex survey waves
years <- 1973:2016

wpp_us <- d[
    location == "United States of America" & sex %in% c("Male", "Female") & time %in% years,
    .(period = time, age = age_grp_start, sex, n = pop)
]

# match gss_homosex coding/structure
wpp_us[, sex := fcase(sex == "Male", "male", sex == "Female", "female")]
wpp_us[age >= 89L, age := 89L]   # GSS top-codes age at "89 or older"
wpp_us <- wpp_us[age >= 18L, .(n = sum(n)), by = .(period, age, sex)]
setkey(wpp_us, period, age, sex)

save(wpp_us, file = "../data/wpp_us.rda", version = 2, compress = "bzip2")
