findCompoundByExactMass <- function(exact_mass, onlyFirstName = TRUE){
  names <- c()
  link  <- stringr::str_replace('http://rest.kegg.jp/find/compound/replace/exact_mass', 
                               pattern = 'replace', replacement = as.character(exact_mass))
  raw   <- readLines(link)
  if (length(raw) == 1){
    return(list('mass' = exact_mass, 'compound' = NA, names = NA))
  }
  else{
    compound <- stringr::str_split(raw, pattern = '\t', simplify = T)[1]
    link     <- stringr::str_replace('http://rest.kegg.jp/get/replace', 
                                 pattern = 'replace', replacement = compound)
    raw      <- readLines(link)
    
    from     <- which(str_sub(raw, 1, 4) == "NAME")
    to       <- which(str_sub(raw, 1, 7) == "FORMULA") - 1
    
    for (i in from:to){
      if (i < to){
        buffor <- str_split(raw[i], pattern = " ", simplify = T)
        buffor <- str_split(buffor[length(buffor)], pattern = ";", simplify = T) 
        names  <- c(names, buffor[length(buffor)-1])
      }
      else{
        buffor <- str_split(raw[i], pattern = " ", simplify = T)
        names  <- c(names, buffor[length(buffor)])
      }
    }
  }
  if (onlyFirstName){
    names = names[1]
  }
  return(list('mass' = exact_mass, 'compound' = compound, 'names' = names))
}

# Example
# findCompoundByExactMass(108.0687)

