library("data.table")

# US population by single year of age and sex, from UN World Population
# Prospects (WPP) 2024. Counts are in thousands and are *not* rescaled here:
# decompose_aggregated() uses only the relative cell structure, and the
# vignette rescales each wave to the survey sample size.
#
# WPP 2024 historical estimates end in 2023. The 2024 population is the median
# projection. Keep this explicit here even though the packaged table presents
# one continuous series.

revision <- "2da7768ae64fc74105d3f9e98f9a74d37b62f99a"
base_url <- sprintf(
    "https://raw.githubusercontent.com/PPgp/wpp2024/%s/data/",
    revision
)

load_remote <- function(name) {
    path <- tempfile(fileext = ".rda")
    on.exit(unlink(path))
    download.file(paste0(base_url, name, ".rda"), path, mode = "wb")
    env <- new.env(parent = emptyenv())
    load(path, envir = env)
    env[[name]]
}

historical <- as.data.table(load_remote("popAge1dt"))[
    country_code == 840L & year %in% 1973:2023,
    .(period = as.integer(year), age, popM, popF)
]
projection <- as.data.table(load_remote("popprojAge1dt"))[
    country_code == 840L & year == 2024L,
    .(period = as.integer(year), age, popM, popF)
]

wpp_us <- melt(
    rbind(historical, projection),
    id.vars = c("period", "age"),
    measure.vars = c("popM", "popF"),
    variable.name = "sex",
    value.name = "n"
)
wpp_us[, sex := fcase(sex == "popM", "male", sex == "popF", "female")]
wpp_us[age >= 89L, age := 89L] # GSS top-codes age at "89 or older"
wpp_us <- wpp_us[age >= 18L, .(n = sum(n)), by = .(period, age, sex)]
setkey(wpp_us, period, age, sex)

save(wpp_us, file = "../data/wpp_us.rda", version = 2, compress = "bzip2")
