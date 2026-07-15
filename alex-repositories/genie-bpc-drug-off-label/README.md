
# GENIE BPC Off Label Cancer Landscape Manuscript

![GENIE logo](genie-logo-portal.jpeg)

# Overview

An analysis of off-guideline and off label use in GENIE BPC, using indications data from hemonc.org.  Originally completed by Alex Paynter at Sage Bionetworks.

# Installation and Setup

To clone this repository, run the following in the command line from a machine with git installed:

```
git clone https://github.com/Sage-Bionetworks/genie-bpc-drug-off-label
```

## Reproducibility tools

This repository:
- Was tested and run on R version 4.4.2.
- Uses R projects.  When running any codes, please open the `.RProj` file first.  
- Does **not** use `renv` to manage package environments.
- Does **not** use `docker` or other containerization to manage deployment.

The code may work without appreciation of these tools, but no guarantees.

## Requirements

To run the code in this respository you will need:

- A Synapse account which has download rights for GENIE data.  See below on data versions.
- The [synapser](https://r-docs.synapse.org/articles/synapser.html) R package, which will also require a python installation (follow instructions at that link).
	- *Note:*  This is only used to acquire the data.  It is technically possible to download the data by pointing and clicking if you want to.

# Code structure

The top-level workflow of the project is in `main.R`.  This calls the other analysis scripts in the correct sequence to reproduce my workflow.  Other top level folders include:

- `/analysis` - Scripts (`analysis/scripts`), quarto/rmarkdown files (`analysis/reports`) and any other analysis code excluding function definitions.
- `/data-raw` - Raw data, where raw means "as it comes in the data release."
- `/data` - Processed data, saved at various stages in the analysis.
- `/output` - Figures, rendered reports, tables, etc.
- `/R` - Function definitions.  These are sometimes written with {roxygen}-style documentation like a package would be.


# Data

We use the latest available version of each BPC cohort, public if available and private (consortium) if not.  The public versions are listed  [here](https://www.synapse.org/Synapse:syn27056172/wiki/616631).

If you are not a consortium member and you want to access the exact data version to reproduce this analysis, please send a request explaining this to genieinfo@aacr.org.  If your request is denied then please write me at the email below - reproduction or critique of my analysis is a great service and I want to help you do it.

The structure, processing and flow of data is described in detail in the PDF data guides, which accompany the data files.

This code also makes use of hemonc.org data extracts.  I have not yet made this programmatic, work in progress.

# Acknowledgments/References

We wish to thank the following groups for their upstream contributions to the data:

- [AACR Project GENIE team](https://www.aacr.org/professionals/research/aacr-project-genie/about-us/)
- Sage bionetworks GENIE team - processing and releases.
- [MSKCC biostatistics team](https://www.mskcc.org/departments/epidemiology-biostatistics/biostatistics/project-genie-bpc-genomics-evidence-neoplasia-information-exchange-biopharma-collaborative)
- The patients and institutions who contributed data to the GENIE and GENIE BPC registries.

# License

The license for this material is [GNU GPLv3](https://choosealicense.com/licenses/gpl-3.0/).  That means you can use this code, as long as your code remains open for others to use.  We're on a strict honor system with no repercussions here so thanks in advance for definitely following this.

# Contact

If you have additional questions please write alexander.paynter@sagebase.org.  If that fails, try genie.dcc@sagebase.org and ask them to put you in touch with me.
