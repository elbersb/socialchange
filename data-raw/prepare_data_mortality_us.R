library("data.table")
library("HMDHFDplus")

hmd_user <- "fill-in"
hmd_pw <- "fill-in"

# Mx_1x1 is the central death rate m(x) (deaths / exposure), not a probability.
mortality_us <- setDT(readHMDweb(CNTRY = "USA", username = hmd_user, password = hmd_pw, item = "Mx_1x1"))

mortality_us[, Total := NULL]
names(mortality_us) <- c("year", "age", "female", "male")
mortality_us[, age := as.numeric(ifelse(age == "110+", "110", age))]
mortality_us <- melt(mortality_us, id.vars = c("year", "age"), variable.name = "sex", value.name = "death_rate")

# tricky: top-coded age categories (match to GSS)
# TODO: we're taking the mean here which is a questionable assumption
mortality_us[age > 89, age := 89]
mortality_us <- mortality_us[, .(death_rate = mean(death_rate)), by = .(year, age, sex)]

save(mortality_us, file = "../data/mortality_us.rda", version = 2, compress = "bzip2")
