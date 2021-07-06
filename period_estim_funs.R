LS_period <- function(signal, time, from_freq, to_freq, oversampling_freq) {
  
  library(lomb)
  
  ls <- lsp(signal, 
            times = time, 
            from = from_freq, 
            to = to_freq,
            ofac = oversampling_freq,
            plot = FALSE)
  
  period <- as.numeric(summary.lsp(ls)$Value[11])
  pvalue <- as.numeric(summary.lsp(ls)$Value[12])  
  
  return(tibble(period = period, 
                pvalue = pvalue))
  
}

twentyfour_period <- function() {
  
  period <- 24
  pvalue <- NA
  
  return(tibble(period = period, 
                pvalue = pvalue))
  
}