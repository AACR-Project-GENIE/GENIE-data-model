#BiocManager::install('maftools')

library(maftools)

# load my other stuff for convenience:
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)

?maftools::somaticInteractions

# example they give:
laml.maf <- system.file('extdata', 'tcga_laml.maf.gz', package = 'maftools')
laml <- read.maf(maf = laml.maf)
somaticInteractions(
  maf = laml,
  top = 5
)

laml %>% lobstr::tree(., max_depth = 2)
laml %>% glimpse

# FDR (assume BF).
si_rtn <- somaticInteractions(
  maf = laml,
  top = 10,
  plotPadj = T
) 
lobstr::tree(si_rtn) # so it returns the dataframe here.  The plot is spit out as it returns.  Grosssssss, but useful for me to compare P values with.

# Cherry picking some genes that have significant P values here:
si_rtn %>% 
  filter(
    gene1 %in% c("DNMT3A") & gene2 %in% c("NPM1", "IDH1")
  )
# Can we reproduce this P value?
fisher.test(x = matrix(c(128, 32, 17, 16), nrow = 2, byrow = T))
# Yep.

si_rtn %>% 
  filter(gene1 %in% "FLT3" & gene2 %in% "TP53")



# Observations: Only 1 test is done.  They just do (literally)
# ifelse(f$estimate > 1,-log10(f$p.val), log10(f$p.val))

somaticInteractions(
  maf = laml,
  top = 5,
  plotPadj = T,
  showCounts = T, # show counts and ignore P values.
  countType = "all", # shows co-occur / mut excl.
  showSum = T,
  pvalue = -Inf,
  colPal = "PuOr", # just for fun.
  limitColorBreaks = F
) 

# A tiny little problem here is the number of tops cahnges the p values dramatically:
somaticInteractions(
  maf = laml,
  top = 5,
  plotPadj = T
)
somaticInteractions(
  maf = laml,
  top = 5,
  plotPadj = T
)

# Two things we haven't touched on yet:
# - Getting the list of "top" genes, done by getGeneSummary().
# - Making sure the P value adjustment and end-to-end steps line up with what
#   maftools is doing.

getGeneSummary(laml) %>% head # which just pulls
laml@gene.summary %>% head
# So the numbers in brackets are the number of altered samples.

# The counts of altered samples look unfiltered - the "total" just counts
#   total number of alterations without accounting for 2+ in specific samples.
laml@data %>% 
  as_tibble(.) %>%
  dplyr::count(Tumor_Sample_Barcode, Hugo_Symbol) %>%
  dplyr::count(Hugo_Symbol, sort = T, name = "AlteredSamples")
laml@gene.summary %>% head(., 10) %>% select(Hugo_Symbol, AlteredSamples)

# Last thing to do: Make sure that the P value adjustment I get from top genes
#   gives me identical results to what they had.  Probably time to just make
#   a function I think.

laml_wide <- laml@data %>% 
  as_tibble(.) %>%
  select(Hugo_Symbol, Tumor_Sample_Barcode) %>%
  mutate(
    alt = 1,
    Tumor_Sample_Barcode = as.character(Tumor_Sample_Barcode)
  ) %>%
  group_by(Hugo_Symbol, Tumor_Sample_Barcode) %>%
  slice(1) %>%
  ungroup(.) %>%
  pivot_wider(
    names_from = Hugo_Symbol,
    values_from = alt,
    values_fill = 0
  )

# Just to make this all perfect we'll add the one sample with no alterations:
laml_wide %<>%
  add_row(
    Tumor_Sample_Barcode = "TCGA-AB-2903"
  ) %>%
  mutate(
    across(
      .cols = -Tumor_Sample_Barcode,
      .fns = (function(x) {
        if_else(is.na(x), 0, x)
      })
    )
  )
  

get_binary_feature_pos(
  laml_wide,
  ignore_cols = "Tumor_Sample_Barcode"
)


dft_co_me <- test_fisher_co_occur(
  laml_wide,
  ignore_cols = "Tumor_Sample_Barcode"
)



dft_co_me %<>%
  mutate(
    co_me_lab = glue("{ct_11} / {ct_10+ct_01}"),
    p_value_adj = p.adjust(p.value, method = "BH")
  )

plot_binary_association(
  dat = dft_co_me,
  x_var = "var1_lab",
  y_var = "var2_lab",
  show_p_sig = T,
  label_var = "co_me_lab",
  pval_var = "p_value_adj"
) 



fisher_test_helper(
  17, 35, 16, 125
)

