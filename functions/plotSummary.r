plotSummary <- function(dat = dat, sportKR = sportKR, i, s){
  
  par(mar = c(4, 4, 4, 1), las = 1)
  
  dat.i <- as.numeric(dat[i, ] / skal[s])
  tit.i <- paste(sportKR[i, "Sport"], "\n(", sportKR[i, "Einheit"], skal.nam[s], ")", sep = "")
  barplot(dat.i, names = colnames(dat), main = tit.i, 
          density = c(rep(-1, ncol(dat) - 1), 40), 
          col = c(rep(grey(0.5), ncol(dat) - 1), 2))     
  
}