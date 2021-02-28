# get impact_genes from gnomeR (this is where that dataset is maintained)
# eventually this can load right form package but newest data not committed yet
# impact_gene_info <- gnomeR::impact_gene_info
# save(impact_gene_info,
#      file = here::here("data-raw",
#                 "impact_gene_info_2021-02-09.RData"))

load(here::here("data-raw",
                "impact_gene_info_2021-02-23.RData"))

usethis::use_data(impact_gene_info, overwrite = TRUE)
