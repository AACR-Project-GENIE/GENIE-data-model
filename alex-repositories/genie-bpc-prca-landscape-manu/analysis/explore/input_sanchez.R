# Description:  Takes the genes in the paper from Sanchez (https://doi.org/10.1016/j.cell.2018.03.035) and puts them into a tidy dataframe.  
# Note:  There are probably some complexities beyond hugo name needed to do this
#.  respectably (e.g. Amp vs mutation, etc).

Cell_Cycle <- c(
  "CDKN1A", "CDKN1B", "CDKN2A", "CDKN2B", "CDKN2C", "CCND1", "CCND2", "CCND3",
  "CCNE1", "CDK2", "CDK4", "CDK6", "RB1", "E2F1", "E2F3"
)
HIPPO <- c(
  "STK4", "STK3", "SAV1", "LATS1", "LATS2", "MOB1A", "MOB1B", "YAP1", "WWTR1",
  "TEAD1", "TEAD2", "TEAD3", "TEAD4", "PTPN14", "NF2", "WWC1", "TAOK1", "TAOK2",
  "TAOK3", "CRB1", "CRB2", "CRB3", "LLGL1", "LLGL2", "HMCN1", "SCRIB", "HIPK2",
  "FAT1", "FAT2", "FAT3", "FAT4", "DCHS1", "DCHS2", "CSNK1E", "CSNK1D", "AJUBA",
  "LIMD1", "WTIP"
)
MYC <- c(
  "MAX", "MGA", "MLX", "MLXIP", "MLXIPL", "MNT", "MXD1", "MXD3", "MXD4", "MXI1",
  "MYC", "MYCL", "MYCN"
)
NOTCH <- c(
  "ARRDC1", "CNTN6", "CREBBP", "EP300", "HES1", "HES2", "HES3", "HES4", "HES5",
  "HEY1", "HEY2", "HEYL", "KAT2B", "KDM5A", "NOTCH1", "NOTCH2", "NOTCH3", 
  "NOTCH4", "NOV", "NRARP", "PSEN2", "LFNG", "ITCH", "NCSTN", "SPEN", "JAG1",
  "APH1A", "FBXW7", "FHL1", "THBS2", "HDAC2", "MFAP2", "CUL1", "RFNG", 
  "NCOR1", "NCOR2", "MFAP5", "HDAC1", "NUMB", "JAG2", "MAML3", "MFNG", 
  "CIR1", "CNTN1", "MAML1", "MAML2", "NUMBL", "PSEN1", "PSENEN", "RBPJ", 
  "RBPJL", "RBX1", "SAP30", "SKP1", "SNW1", "CTBP1", "CTBP2", "ADAM10", "APH1B",
  "ADAM17", "DLK1", "DLL1", "DLL3", "DLL4", "DNER", "DTX1", "DTX2", "DTX3", 
  "DTX3L", "DTX4", "EGFL7"
)
NRF2 <- c("NFE2L2", "KEAP1", "CUL3")
PI3K <- c("EIF4EBP1", "AKT1", "AKT2", "AKT3", "AKT1S1", "DEPDC5", "DEPTOR", "INPP4B", "MAPKAP1", "MLST8", "MTOR", "NPRL2", "NPRL3", "PDK1", "PIK3CA", "PIK3CB", "PIK3R1", "PIK3R2", "PIK3R3", "PPP2R1A", "PTEN", "RHEB", "RICTOR", "RPTOR", "RPS6", "RPS6KB1", "STK11", "TSC1", "TSC2")
TGF_Beta <- c("TGFBR1", "TGFBR2", "ACVR2A", "ACVR1B", "SMAD2", "SMAD3", "SMAD4")
RTK_RAS <- c(
  "ABL1", "EGFR", "ERBB2", "ERBB3", "ERBB4", "PDGFRA", "PDGFRB", "MET", 
  "FGFR1", "FGFR2", "FGFR3", "FGFR4", "FLT3", "ALK", "RET", "ROS1", 
  "KIT", "IGF1R", "NTRK1", "NTRK2", "NTRK3", "SOS1", "GRB2", "PTPN11", 
  "KRAS", "HRAS", "NRAS", "RIT1", "ARAF", "BRAF", "RAF1", "RAC1", 
  "MAP2K1", "MAP2K2", "MAPK1", "NF1", "RASA1", "CBL", "ERRFI1", 
  "CBLB", "CBLC", "INSR", "INSRR", "IRS1", "SOS2", "SHC1", "SHC2", 
  "SHC3", "SHC4", "RASGRP1", "RASGRP2", "RASGRP3", "RASGRP4", "RAPGEF1", 
  "RAPGEF2", "RASGRF1", "RASGRF2", "FNTA", "FNTB", "RCE1", "ICMT", 
  "MRAS", "PLXNB1", "MAPK3", "ARHGAP35", "RASA2", "RASA3", "RASAL1", 
  "RASAL2", "RASAL3", "SPRED1", "SPRED2", "SPRED3", "DAB2IP", "SHOC2", 
  "PPP1CA", "SCRIB", "PIN1", "KSR1", "KSR2", "PEBP1", "ERF", "PEA15", 
  "JAK2", "IRS2"
)
TP53 <- c("TP53", "MDM2", "MDM4", "ATM", "CHEK2", "RPS6KA3")
WNT <-  c(
  "CHD8", "LEF1", "LGR4", "LGR5", "LRP5", "LRP6", "LZTR1", "NDP", "PORCN",
  "RSPO1", "SFRP1", "SFRP2", "SFRP4", "SFRP5", "SOST", "TCF7L1", "TLE1",
  "TLE2", "TLE3", "TLE4", "WIF1", "ZNRF3", "CTNNB1", "DVL1", "DVL2", "DVL3",
  "FRAT1", "FRAT2", "FZD1", "FZD10", "FZD2", "FZD3", "FZD4", "FZD5", "FZD6",
  "FZD7", "FZD8", "FZD9", "WNT1", "WNT10A", "WNT10B", "WNT11", "WNT16", "WNT2",
  "WNT3A", "WNT4", "WNT5A", "WNT5B", "WNT6", "WNT7A", "WNT7B", "WNT8A", "WNT8B",
  "WNT9A", "WNT9B", "AMER1", "APC", "AXIN1", "AXIN2", "DKK1", "DKK2", "DKK3", "DKK4", "GSK3B", "RNF43", "TCF7", "TCF7L2", "CHD4"
)

tibble_make_help <- function(
    full_name,
    vec) {
  vec_name <- deparse(substitute(vec))
  
  tibble(
    ss_name = vec_name,
    full_name = full_name,
    hugo = vec
  )
}

sanchez_pathways <- bind_rows(
  tibble_make_help(
    full_name = "Cell cycle",
    vec = Cell_Cycle
  ),
  tibble_make_help(
    full_name = "Hippo signaling",
    vec = HIPPO
  ),
  tibble_make_help(
    full_name = "Myc signaling",
    vec = MYC
  ),  
  tibble_make_help(
    full_name = "Notch signaling",
    vec = NOTCH
  ),
  tibble_make_help(
    full_name = "Oxidative stress response/Nrf2",
    vec = NRF2
  ),
  tibble_make_help(
    full_name = "PI-3-Kinase signaling",
    vec = PI3K
  ),
  tibble_make_help(
    full_name =  "receptor-tyrosine kinase (RTK)/RAS/MAP-Kinase signaling",
    vec = RTK_RAS
  ),
  tibble_make_help(
    full_name = "TGF Beta signaling",
    vec = TGF_Beta
  ),
  tibble_make_help(
    full_name = "p53",
    vec = TP53
  ),
  tibble_make_help(
    full_name = "Beta-catenin/Wnt signaling",
    vec = WNT
  )
)

sanchez_pathways %<>%
  select(hugo, ss_name, full_name)

readr::write_rds(
  x = sanchez_pathways,
  file = here('data-raw', 'genomic', 'sanchez-vega_gene_pathways.rds')
)
  
  
  
  
  
  