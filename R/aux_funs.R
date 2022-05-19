# Period estimation functions ####
pacman::p_load(lomb)
period_estimation <- function(signal, time, from_freq, to_freq, oversampling_freq, method, period24) {
  
  if (method == "ls") {
    
    ls <- lsp(
      signal,
      times = time,
      from = from_freq,
      to = to_freq,
      ofac = oversampling_freq,
      plot = FALSE
    )
    
    period <- as.numeric(summary.lsp(ls)$Value[11])
    pvalue <- as.numeric(summary.lsp(ls)$Value[12])
    
  } else if (method == "twentyfour") {
    period <- period24
    pvalue <- NA
    
  } else {
    stop("Wrong period estimation method!")
    
  }

  return(tibble(period = period, 
                pvalue = pvalue))
  
}

# Detrending functions ####
pacman::p_load(pracma)
circaluc_detrending <- function(signal, method) {
  
  if (method == "linear") {
    signal <- pracma::detrend(signal, "linear")[, 1]
    
  } else if (method == "exponential") {
    signal <- exp(pracma::detrend(log(signal), "linear")[, 1])
    
  } else {
    stop("Wrong detrending estimation method!")
    
  }
  
  return(signal)
  
}