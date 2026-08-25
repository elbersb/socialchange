library("data.table")

# US population by single year of age and sex, from UN World Population
# Prospects (WPP) 2022. Counts are in thousands and are *not* rescaled here:
# decompose_aggregated() uses only the relative cell structure, and the
# vignette rescales each wave to the survey sample size.

base_url <- paste0(
    "https://raw.githubusercontent.com/guyabel/tidywpp/main/",
    "data-host/WPP2022/PopulationBySingleAgeSex/2/"
)
base <- readRDS(url(paste0(base_url, "base.rds")))[[1L]]
keep <- base$LocID == 840 & base$Time %in% 1973:2024 # UN M49 code for the US
base <- base[keep, ]
male <- readRDS(url(paste0(base_url, "PopMale.rds")))[keep, 1L, drop = TRUE]
female <- readRDS(url(paste0(base_url, "PopFemale.rds")))[keep, 1L, drop = TRUE]
age <- suppressWarnings(as.integer(base$AgeGrp))
age[base$AgeGrp == "100+"] <- 100L

wpp_us <- rbind(
    data.table(period = base$Time, age, sex = "male", n = male),
    data.table(period = base$Time, age, sex = "female", n = female)
)
wpp_us[age >= 89L, age := 89L] # GSS top-codes age at "89 or older"
wpp_us <- wpp_us[age >= 18L, .(n = sum(n)), by = .(period, age, sex)]
setkey(wpp_us, period, age, sex)

save(wpp_us, file = "../data/wpp_us.rda", version = 2, compress = "bzip2")
