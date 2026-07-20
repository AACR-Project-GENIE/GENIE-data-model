
gen_mmult <- function(f=`*`, g=sum) { 
  function(x, y) {
    apply(
      y, 
      2, 
      (function(a) {
        apply(
          x, 
          1, 
          (function(b) g(f(a,b)))
        )
      })
    )
  }
}