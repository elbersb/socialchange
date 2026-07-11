library("data.table")
library("HMDHFDplus")

hmd_user <- "fill-in"
hmd_pw <- "fill-in"

# Build central death rates m(x) = deaths / exposure from the raw counts, so
# that the top-coded 89+ group (to match GSS) is exposure-weighted:
# m(89+) = sum(deaths) / sum(exposure). For single ages this equals Mx_1x1.
deaths <- setDT(readHMDweb(CNTRY = "USA", username = hmd_user, password = hmd_pw, item = "Deaths_1x1"))
exposures <- setDT(readHMDweb(CNTRY = "USA", username = hmd_user, password = hmd_pw, item = "Exposures_1x1"))

prep <- function(d, value.name) {
    d <- d[, .(year = Year, age = as.numeric(Age), female = Female, male = Male)]
    melt(d, id.vars = c("year", "age"), variable.name = "sex", value.name = value.name)
}

mortality_us <- merge(prep(deaths, "deaths"), prep(exposures, "exposure"),
    by = c("year", "age", "sex")
)

# top-coded age categories (match to GSS)
mortality_us[age > 89, age := 89]
mortality_us <- mortality_us[,
    .(death_rate = sum(deaths) / sum(exposure)),
    by = .(year, age, sex)
]

save(mortality_us, file = "../data/mortality_us.rda", version = 2, compress = "bzip2")
