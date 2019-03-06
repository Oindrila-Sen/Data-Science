#########################################
## readInputandRun
#########################################
readInputandRun <- function()
{
  stdin <- file('input')
  open(stdin)
  input <- readLines(stdin,  warn=FALSE)
  close(stdin)
  #setwd("/Users/oindrilasen/WORK_AREA/Data Science/HackerRank/StockPrediction")
  #input <- readLines("input2.txt", warn = FALSE)
  input = unlist(strsplit(input, split = "\n"))
  input = unlist(input)
  # read the first line
  tmp <-  strsplit(input, split = " ")[1]
  m <- as.numeric(sapply(tmp, "[[", 1))
  k <- as.numeric(sapply(tmp, "[[", 2))
  d <- as.numeric(sapply(tmp, "[[", 3))
  line <- c()
  name <- c()
  owned <- c()
  prices <- c()
  for (i in 1:k) {
    # i = 5
    line[i] <- as.vector(strsplit(input, split = " ")[i+1])
    name <- c(name, as.character(sapply(line[i], "[[", 1)))
    owned <- c(owned , as.numeric(sapply(line[i], "[[", 2)))
    # prices <- #c(prices, as.list(unlist(line[1])[3:7]))
    #prices[[i]] <- unlist(line[i])[3:7]
    prices[[i]] <- as.list(unlist(line[i])[3:7])
  }
  printTransactions(m, k, d, name, owned, prices)
}

#SMA
#(9.81 + 6.54 + 1.43 + 2.41 + 3.72)/5

#wma
#((9.81 *1) + (6.54 *2) + (1.43 *3) + (2.41 *4) + (3.72 * 5))/((5 *(5 +1))/2)

#########################################
## calcWma
#########################################
calcWma <- function(k,prices,d){
  wma <- c()
  a <- 0
  for (i in 1:k) {
    for (j in 1:4){
      a <- a + (as.numeric(prices[[i]][[j]]) * j)
    }
    b <- ((j * (j+1))/2)
    wma[i] <- round(a/b,2)
    a <- 0
  }
  return(wma[d])
}
#########################################
## printTransactions
#########################################
printTransactions <- function(m, k, d, name, owned, prices){
  
  trx_count <- 0
  stock_name <- c()
  stock_num <- c()
  trx <- c()
  m1 <- 0
  f <- c()
  for (i in 1:k) {
    f[i]  <- as.numeric(prices[[i]][[5]]) - calcWma(k,prices,i) 
  }
  minF <- min(f)
  for (i in 1:k) {
    if (owned[i] > 0 & 
        as.numeric(prices[[i]][[5]]) > calcWma(k,prices,i) 
    ) {
      trx_count = trx_count + 1
      stock_name <- c(stock_name,as.character(name[i]))
      trx <- c(trx, "SELL")
      stock_num <- c(stock_num, owned[i])
      m1 <- m1 + owned[i] * as.numeric(prices[[i]][[5]])
      #print("SELL")
      #print(m1)
      owned[i] <- owned[1] - stock_num[trx_count]
      #print(owned[i])
    }
    else if ( as.numeric(m) > 0 &
              floor(as.numeric(m) / as.numeric(prices[[i]][[5]])) > 0 &
              f[i] == minF
    ) {
      
      trx_count = trx_count + 1
      stock_num <- c(stock_num, floor(m /as.numeric(prices[[i]][[5]])))
      stock_name <- c(stock_name,as.character(name[i]))
      trx <- c(trx, "BUY")
      owned[i] <- stock_num[trx_count]
      m <- m - as.numeric(stock_num[trx_count] * as.numeric(prices[[i]][[5]]))
      
      
    }
  }
  m <- (m + m1)
  m1 <-0
  
  
  if (trx_count > 0) {
    #cat(m,"\n")
    cat(trx_count, sep = "\n")
    for (i in 1:trx_count) {
      cat(stock_name[i], trx[i], stock_num[i])
      cat(sep = "\n")
    }
  }
  if (trx_count == 0){
    cat(trx_count, sep = "\n")
  }
}

readInputandRun()


