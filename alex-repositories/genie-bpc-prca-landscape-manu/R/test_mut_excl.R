

test_mut_excl <- function(
    dat,
    method = "ShiftedBinomial",
    lower.tail = T,
    return_q = F
) {
  cn <- colnames(dat)
  
  mat <- dat %>%
    as.matrix %>%
    t
  
  res <- getMutex(
    mat,
    method = method,
    lower.tail = lower.tail,
  )
  
  colnames(res) <- cn
  rownames(res) <- cn
  
  if (return_q) {
    print(res@x)
    res@x <- qvalue::qvalue(res@x)$qvalue
  }
  
  return(res)
}
