omegaSquared <- function(aov_table){
  for(row in nrow(aov_table)){ 
    df_effect <- as.numeric(strsplit(aov_table[row,2][1], ',')[[1]][1])
    df_error <- as.numeric(strsplit(aov_table[row,2][1], ',')[[1]][2])
    f <- as.numeric(strsplit(aov_table[row,4], ' ')[[1]][1])
    omsqrd <- (f - 1)/(f +(df_error + 1)/df_effect)
    aov_table[row, 5] <- round(omsqrd, 3)
    return (aov_table)
  }
}


