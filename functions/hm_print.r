hm_print <- function(dat, cut = 16){
  
  t <- (dat 
  %>% select(start, end, date, hm_diff, time, hm_h, beg) 
  %>% arrange(desc(hm_h)) 
  %>% mutate(start = substr(start, 1, cut), end = substr(end, 1, cut)))

  return(t)
}