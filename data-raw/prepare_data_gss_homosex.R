library("gssr")
library("data.table")
data(gss_all)

cols <- c(
    "id", "year", "wtssall", "sample", "vstrat", "vpsu", "homosex",
    "age", "cohort", "sex", "educ", "marital", "race", "region",
    "born", "physhlth", "compuse", "relig16", "pray"
)

gss_homosex <- gss_all[, cols]
rm("gss_all")

setDT(gss_homosex)
gss_homosex[, `:=`(cohort = year - age)]
gss_homosex <- haven::zap_labels(gss_homosex)
gss_homosex <- gss_homosex[!is.na(homosex) & homosex %in% 1:4]
gss_homosex <- gss_homosex[!is.na(cohort) & !is.na(educ) & !is.na(marital) & !is.na(relig16)]
gss_homosex <- gss_homosex[year <= 2016]
gss_homosex <- gss_homosex[cohort %in% 1892:1995]
gss_homosex <- gss_homosex[!sample %in% c(4, 5, 7)]
# 35106 cases, Ekstam has 35,114
gss_homosex[, homosex := scales::rescale(homosex)]
gss_homosex[, sex := fcase(sex == 1, "male", sex == 2, "female")]

save(gss_homosex, file = "../data/gss_homosex.rda", version = 2, compress = "bzip2")
