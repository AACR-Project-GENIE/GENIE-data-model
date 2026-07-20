sor <- readxl::read_xlsx(
  '/Users/apaynter/Downloads/GENIE BPC Scope of Release 2025-09-03.xlsx',
  sheet = 2
)

released_list <- c(
  "Index Cancer Only",
  "always",
  "yes",
  "non-Index Cancer Only"
)

sor %>%
  filter(TYPE %in% "Derived") %>%
  filter(
    # peppered tolower everywhere because the captialization seems to be all over generally
    tolower(`Shared for NSCLC 3.1-consortium Release`) %in%
      tolower(released_list) |
      tolower(`Shared for CRC v3.1 Consortium Release`) %in%
        tolower(released_list) |
      tolower(`Shared for BrCa V1.2-consortium Release`) %in%
        tolower(released_list) |
      tolower(`Shared for Pancreas V1.2-Consortium Release`) %in%
        tolower(released_list) |
      tolower(`Shared for Bladder V1.2-Consortium Release`) %in%
        tolower(released_list) |
      tolower(`Shared for Prostate V1.2-Consortium Release`) %in%
        tolower(released_list)
  ) %>%
  count(VARNAME) %>%
  print(n = 500)

# count(sor, `Shared for Bladder V1.2-Consortium Release`)
# count(sor, TYPE)
