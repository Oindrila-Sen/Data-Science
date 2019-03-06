# Enter your code here. Read input from STDIN. Print output to STDOUT
stdin <- file('input')
open(stdin)
#input <- suppressWarnings(read.delim(stdin,stringsAsFactors = FALSE))
#input <- readLines(stdin,  warn=FALSE)
input <- suppressWarnings(read.table(stdin, sep=" ",stringsAsFactors = FALSE));
close(stdin)
#cat(class(input))
#cat(str(input))
#cat(input$V1[1])
#Read the first line
n <- as.integer(input$V1[1])
#cat(n)
position <- matrix(0, ncol = n, nrow = n)
i <- 1
#inputline <- c()
for( i in 1: n){
  j <- 1
  inputline <- unlist(strsplit(input$V1[i+1],""))
  inputline <- unlist(strsplit(inputline,""))
  #print(i)
  #print(inputline)
  for (j in 1:n){
    #print(i)
    #print(j)
    #print(inputline[j])
    if (inputline[j] == "p"){
      position[i,j] <- "p"
      rp=i
      cp=j
    }
    else if (inputline[j]  == "m"){
      position[i,j] <- "m"
      rm=i
      cm=j
    }
      
  }
}
x=rp-rm
y=cp-cm

#cat(x)
#cat(y)

displayPathtoPrincess <- function(n, x, y){
 while (x!= 0 | y!= 0){
  if(x > 0){
    #cat('DOWN', sep="\n")
    cat('DOWN')
    cat("\n")
    x = x - 1
    if(y > 0){
      #cat('RIGHT', sep="\n")
      cat('RIGHT')
      cat("\n")
      y = y - 1
    }
    else if(y < 0){
      #cat('LEFT', sep="\n")
      cat('LEFT')
      cat("\n")
      y = y + 1
    }
  }
  else if (x < 0){
    cat('UP')
    cat("\n")
    x = x + 1
    if(y > 0){
      #cat('RIGHT', sep="\n")
      cat('RIGHT')
      cat("\n")
      y = y - 1
    }
    else if(y < 0){
      #cat('LEFT', sep="\n")
      cat('LEFT')
      cat("\n")
      y = y + 1
    }
  }
     else {
      if(y > 0){
        #cat('RIGHT', sep="\n")
        cat('RIGHT')
        cat("\n")
        y = y - 1
      }
      else if(y < 0){
        cat('LEFT')
        cat("\n")
        y = y + 1
      }
    }
  }
}


displayPathtoPrincess(n,x,y)
