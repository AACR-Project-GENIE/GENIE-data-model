library(genieBPC)
library(lobstr)
library(magrittr)
library(dplyr)
library(sunburstR)
library(gtsummary)

# need to use set_synapse_credentials(), which only supports username and password unfortunately.
# Never set credentials in a tracked script, use the command line or set them 
#   in your system environment.

# Modern R packages often have good documentation supplied by {pkgdown}.
# These contain "vignettes", which are narratives meant for new users to work
#   through to understand the basic workflow of the package.  I recommend starting
#   there!  These can be found in two ways:
# The website when one exists:  https://genie-bpc.github.io/genieBPC/
# ALL R packages have any vignettes which exist in the installation bundle too.
# Find them with:
vignette(package = "genieBPC")
# Then pull up a specific one from that list with:
vignette("drug_regimen_sunburst_vignette") 
# Because that doesnt' seem to work here's a dplyr example:
vignette("programming", package = "dplyr")






nsclc <- pull_data_synapse("NSCLC", version = "v2.0-public")

# We want to look at the structure of this object.  The basic way 
#  (using a much simpler object) is (str = "structure"):
str(data.frame(a = 1:3))
# Can also use things like class(), names(), etc.  One of my favorites is tree():
lobstr::tree(data.frame(a = 1:3))

# Back to our example:
lobstr::tree(nsclc, max_depth = 2)


nsclc_stg_iv <- create_analytic_cohort(
    data_synapse = nsclc$NSCLC_v2.0,
    stage = c("Stage IV")
)
lobstr::tree(nsclc_stg_iv, max_depth = 1)
# Similar structure, but different names and classes.

sunplot <- drug_regimen_sunburst(
    data_synapse = nsclc$NSCLC_v2.0,
    data_cohort = nsclc_stg_iv,
    max_n_regimens = 3
)
# We can see there's two things returned:
lobstr::tree(sunplot, max_depth = 2)

treat_hist_df <- sunplot$treatment_history
# The treatment history dataframe is actually very simple.  It's just all the regimen orders (concatenated by dashes) and the number of people who did that sequence:
glimpse(treat_hist_df)
print(treat_hist_df, rows = 500)

# The rest of the work to get a sunburst plot is done by other code:
sunburstR::sunburst(
    data = treat_hist_df
)

# So if you can get to the treatment history table on your own (not hard), you can lose the reliance on {genieBPC} for sunburst plots.  This is what my code for the manuscript projects currently does.




# Some cases where genieBPC create_analytic_cohort fails:
breast <- pull_data_synapse("BrCa", version = "v1.2-consortium")
# The same call we did above for the nsclc patients fails here:
breast <- create_analytic_cohort(
    data_synapse = breast$BrCa_v1.2,
    stage = c("Stage IV")
)




# Other use cases (copied from vignettes):
nsclc_2_0 = pull_data_synapse("NSCLC", version = "v2.0-public")
ex1 <- create_analytic_cohort(
    data_synapse = nsclc_2_0$NSCLC_v2.0,
    stage_dx = c("Stage IV"),
    # adding more stuff just for fun:
    histology = "Adenocarcinoma",
    regimen_drugs = c("Cisplatin, Pemetrexed Disodium", "Cisplatin, Etoposide"),
    # The important argument to unlocking more tables:
    return_summary = T
)
# You can see what's been added now:
lobstr::tree(ex1, max_depth = 1)

# Pull up those tables to see what's there:
ex1$tbl_overall_summary
ex1$tbl_cohort
ex1$tbl_drugs
ex1$tbl_ngs


# One more function helps us get one NGS test in cases where there is more than one:
samples_data1 <- select_unique_ngs(
    data_cohort = ex1$cohort_ngs,
    oncotree_code = "LUAD",
    sample_type = "Metastasis",
    min_max_time = "max"
)


# That's about it!  A more granular view of available functions is available on the site with:
# https://genie-bpc.github.io/genieBPC/reference/index.html


