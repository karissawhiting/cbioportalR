

# get a table of gene ID and entrez to match on
all_genes_lookup <- get_genes()

all_genes_lookup <- all_genes_lookup %>%
  filter(type == "protein-coding")

usethis::use_data(all_genes_lookup, overwrite = TRUE)

