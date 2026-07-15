# A file to explore VAF filtering and figure out gnomAD a bit.
# Lessons learned are at the bottom.

# load my other stuff for convenience:
library(purrr); library(here); library(fs)
purrr::walk(.x = fs::dir_ls(here('R')), .f = source)


dft_maf <- readr::read_tsv(
  here('data-raw', 'genomic', 'data_mutations_extended.txt')
)

# Looks like it varies a bit by center:
dft_maf %>% 
  group_by(Center) %>%
  summarize(
    across(
      .cols = contains("gnomAD"),
      .fns = ~mean(is.na(.x))
    )
  )

# just making sure my assumption is correct that some HGVSc codes will
#   have multiple (hopefully dozens or hundreds) hits:
dft_maf %>% count(HGVSc, sort = T)

dft_missing_prop_long <- dft_maf %>% 
  group_by(HGVSc) %>%
  summarize(
    across(
      .cols = contains("gnomAD"),
      .fns = ~mean(is.na(.x))
    )
  ) %>%
  pivot_longer(
    cols = -HGVSc,
    names_to = "colname",
    values_to = "prop_missing"
  )

# I'm guessing that whether or not the VAF is populated depends only upon
#   the allele passed and whether we could tag it.  Let's check:
dft_missing_prop_long %>% 
  filter(colname %in% "gnomAD_AF") %>%
  tabyl(prop_missing)

# OK one exception, lets see it:
vec_the_exception <- dft_missing_prop_long %>% 
  filter(colname %in% "gnomAD_AF") %>%
  filter(prop_missing > 0 & prop_missing < 1) %>%
  pull(HGVSc)
# Just the missing one.  Probably taggable by something else, not important to me.

# Average the prop missing over the various subtypes to see if different
#   subgroups are tagged for different genes.
dft_missing_prop_long %>%
  group_by(HGVSc) %>%
  summarize(
    ave_prop_missing = mean(prop_missing)
  ) %>%
  filter(ave_prop_missing > 0 & ave_prop_missing < 1)
# Nope.  All or nothing.

dft_maf %>%
  count(HGVSc, sort = T)


dft_maf %>% 
  mutate(max_AF = 
           pmax(gnomAD_AF,
                gnomAD_AFR_AF,
                gnomAD_AMR_AF,
                gnomAD_ASJ_AF,
                gnomAD_EAS_AF,
                gnomAD_FIN_AF,
                gnomAD_NFE_AF,
                gnomAD_OTH_AF,
                gnomAD_SAS_AF
           )
  ) %>%
  arrange(desc(max_AF)) %>%
  select(Hugo_Symbol, max_AF, HGVSc)
                







# What did we learn?
# - Variants are tagged using Genome Nexus, which calls the Genome Aggregation Database.
# - The GENIE pipeline removes variants with an allele count (!!!) greater #   than 10 in ExAC (gnomAD now).  
# - That number tuned to remove fewer than 1% of known somatic variants 
#   with a tumor-normal sequenced group.  Why a number?  Unclear.

# gnomAD populations (https://gnomad.broadinstitute.org/news/2017-02-the-genome-aggregation-database/)
# - AFR = African / African American
# - AMR = Admixed American
# - ASJ = Ashkenazi Jewish
# - EAS = East Asian
# - FIN = Finnish (weird)
# - NFE = Non-Finnish European
# - SAS = South Asian
# - OTH = Other (~2% of data)