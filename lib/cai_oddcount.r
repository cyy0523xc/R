cai_oddcount <- function (x) {
  count <- 0
  for (i in x) {
    if (i %% 2 == 1){
      count <- count + 1
    }
  }
  
  return(count)
}
