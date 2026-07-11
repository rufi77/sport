schwimmen_print <- function(dat, cut = 16){
  
  t <- (dat 
  %>% select(start, date, Distanz, time, min_100m, beg) 
  %>% arrange(min_100m)
  %>% mutate(start = substr(start, 1, cut)))

  return(t)
}