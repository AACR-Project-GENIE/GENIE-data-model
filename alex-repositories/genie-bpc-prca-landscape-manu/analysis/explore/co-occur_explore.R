# install.packages("Rediscover")

# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("maftools")

library(Rediscover)
library(tidyverse)

data("A_example")
PMA <- getPM(A_example) # estimates prob that gene i is mutated in sample j.
PMA[1:4, 1:4]

data("TCGA_COAD")
# Very helpful to visualize the row/col setup:
TCGA_COAD %>% str
TCGA_COAD[1:4, 1:4]
PM_COAD <- getPM(TCGA_COAD)

prob_matrix <- getPM(A_example[1:11,])
mut_excl_result <- getMutex(A_example[1:11,])

COAD_mutex <- getMutex(TCGA_COAD[1:100,], PM_COAD[1:100,])
COAD_mutex_qvalue <- COAD_mutex
COAD_mutex_qvalue@x <- qvalue::qvalue(COAD_mutex_qvalue@x)$qvalue

coad.maf <- GDCquery_Maf("COAD", pipelines = "muse") %>% read.maf
discoversomaticInteractions(maf = coad.maf, top = 35, pvalue = c(1e-2, 2e-3),getMutexMixed=FALSE)

# Test to see how this looks on something I understand:
n_test <- 100
basic_test <- tibble(
  feat_A = rbinom(n_test, size = 1, prob = 0.1),
  # highly correlated with A:
  feat_B = if_else(
    as.logical(feat_A),
    rbinom(n_test, size = 1, prob = 0.5),
    rbinom(n_test, size = 1, prob = 0.05)
  ),
  # anti-correlated with A:
  feat_C = if_else(
    !as.logical(feat_A),
    rbinom(n_test, size = 1, prob = 0.9),
    rbinom(n_test, size = 1, prob = 0.05)
  )
) %>%
  mutate(sample_id = paste0('s', 1:n()))

# basic_test_mat <- 

basic_test_mat <- basic_test %>% select(-sample_id) %>% as.matrix
rownames(basic_test_mat) <- basic_test$sample_id
basic_test_mat <- t(basic_test_mat)

getMutex(basic_test_mat, method = "ShiftedBinomial", lower.tail = F)
getMutex(basic_test_mat, method = "ShiftedBinomial")
getMutex(basic_test_mat, method = "Exact")
getMutex(basic_test_mat, method = "Binomial")
getMutex(basic_test_mat, method = "RefinedNormal")




gen_mmult <- function(f=`*`, g=sum) {
  function(x, y) {
    apply(
      y, 
      2, 
      function(a) {
        apply(
          x, 
          1, 
          function(b) {
            g(f(a,b))
          }
        )
      }
    )
  }
}

pct_agree_mat <- function(dat) {
  if (is.data.frame(dat)) {
    dat <- as.matrix(dat)
  }
  
  # testing equality of each pair, combine into a with a mean.
  gen_mmult(`==`, mean)(t(dat), dat)
}

pct_agree_mat(basic_test)






pct_agree_mat(test_dat)
    
    
