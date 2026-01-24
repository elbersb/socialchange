library("tidywpp")
library("data.table")

# Population data

wpp_data <- get_wpp(
    indicator = "pop",
    pop_age = "total",
    pop_sex = "total",
    drop_id_cols = TRUE)
wpp_data$Variant <- NULL

wpp_data <- as.data.frame(wpp_data)
save(wpp_data, file = "../data/wpp_data.rda", version = 2)

# EU entry/exit data

eu_membership <- tibble::tribble(
    ~country, ~date, ~event_type,
    "Belgium","1952-07-23","initial",
    "France","1952-07-23","initial",
    "Germany","1952-07-23","initial",
    "Italy","1952-07-23","initial",
    "Luxembourg","1952-07-23","initial",
    "Netherlands","1952-07-23","initial",
    "United Kingdom","1973-01-01","entry",
    "Denmark","1973-01-01","entry",
    "Ireland","1973-01-01","entry",
    "Greece","1981-01-01","entry",
    "Portugal","1986-01-01","entry",
    "Spain","1986-01-01","entry",
    "Austria","1995-01-01","entry",
    "Sweden","1995-01-01","entry",
    "Finland","1995-01-01","entry",
    "Cyprus","2004-05-01","entry",
    "Malta","2004-05-01","entry",
    "Hungary","2004-05-01","entry",
    "Poland","2004-05-01","entry",
    "Slovakia","2004-05-01","entry",
    "Latvia","2004-05-01","entry",
    "Estonia","2004-05-01","entry",
    "Lithuania","2004-05-01","entry",
    "Czechia","2004-05-01","entry",
    "Slovenia","2004-05-01","entry",
    "Bulgaria","2007-01-01","entry",
    "Romania","2007-01-01","entry",
    "Croatia","2013-07-01","entry",
    "United Kingdom","2020-01-31","exit"
)

eu_membership <- as.data.frame(eu_membership)
save(eu_membership, file = "../data/eu_membership.rda", version = 2)
