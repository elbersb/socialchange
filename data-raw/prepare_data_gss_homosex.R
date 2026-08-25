library("gssr")
library("data.table")
data(gss_all)

cols <- c(
    "id", "year", "wtssps", "vstrat", "vpsu", "mode", "homosex",
    "age", "cohort", "sex", "educ", "marital", "race", "region",
    "born", "physhlth", "compuse", "relig16", "pray"
)

gss_homosex <- gss_all[, cols]
rm("gss_all")

setDT(gss_homosex)
gss_homosex[, `:=`(cohort = year - age)]
gss_homosex <- haven::zap_labels(gss_homosex)

# HOMOSEX was not asked in these survey years. In included years, remove both
# planned questionnaire non-assignment and item nonresponse.
gss_homosex <- gss_homosex[!year %in% c(1972, 1975, 1978, 1983, 1986)]
gss_homosex <- gss_homosex[!is.na(homosex) & homosex %in% 1:4]
gss_homosex <- gss_homosex[!is.na(cohort)]
gss_homosex <- gss_homosex[!is.na(sex) & sex %in% 1:2]
gss_homosex[, homosex := scales::rescale(homosex)]
gss_homosex[, mode := fcase(
    mode == 1, "in-person",
    mode == 2, "phone",
    mode == 3, "multimode",
    mode == 4, "web"
)]
gss_homosex[, sex := fcase(sex == 1, "male", sex == 2, "female")]
gss_homosex[, race := fcase(race == 1, "white", race == 2, "black", race == 3, "other")]

save(gss_homosex, file = "../data/gss_homosex.rda", version = 2, compress = "bzip2")
