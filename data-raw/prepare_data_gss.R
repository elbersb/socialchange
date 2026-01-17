library("tidyverse")
library("gssr")
data(gss_all)

# RACDIN whether blacks are welcome for dinner
# RACSEG whether whites have a right to keep blacks out of their neighborhoods,
# RACPUSH whether blacks should not "push" themselves where they are not wanted, and
# RACMAR whether there should be laws against black-white marriage

select(gss_all, contains("region"))

gssrac <- gss_all %>%
    filter(year %in% c(1972, 1976, 1980, 1984)) %>%
    filter(race == 1) %>%
    select(year, age, wtssall, racdin, racseg, racpush, racmar, region, sex) %>%
    mutate(id = 1:n(), cohort = year - age) %>%
    mutate(across(racdin:racmar, ~ -1 * scale(.x)[,1])) %>%
    mutate(across(region:sex, as_factor)) %>%
    pivot_longer(racdin:racmar) %>%
    group_by(id) %>%
    summarize(across(year:cohort, first),
        rac = sum(value, na.rm = TRUE), n_na = sum(is.na(value))) %>%
    filter(n_na <= 2, !is.na(cohort)) %>%
    mutate(rac = case_when(
        n_na == 0 ~ rac,
        n_na == 1 ~ rac * 4/3,
        n_na == 2 ~ rac * 2
    )) %>%
    select(-n_na, -id) %>%
    mutate(rac = rac + 6) %>%
    select(rac, year, age, cohort, region, sex, wtssall)

save(gssrac, file = "../data/gssrac.rda", version = 2)
