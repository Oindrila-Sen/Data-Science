# Enter your code here. Read input from STDIN. Print output to STDOUT
stdin <- file('stdin')
open(stdin)
# Input Format
# The first line contains the number of integers. 
# The second line contains space separated integers for which you need to find the mean, median, mode, standard deviation and confidence interval boundaries.

# Read the second line
input <- readLines(stdin, warn = FALSE)[[2]]
#cat(input)
elements <- c()
# count total elements in the input
n <- length(as.numeric(unlist(strsplit(input, split=" "))))

# Loop through the elements
  for (i in 1:n) {
      # read each element
      tmp = as.numeric(unlist(strsplit(input, split=" "))[i])
      elements[i]  <- tmp
  }
# mean
mean <- mean(elements, na.rm = FALSE)
# median
median <- median(elements, na.rm = FALSE)
# mode
CalcMode <- function(x) {
  ux <- unique(sort(x))
  ux[which.max(tabulate(match(x, ux)))]
}
mode = CalcMode(elements)
# standard deviation
# sqaure root of the average squared distance from the mean
Calcsd<- function(x,n,m) {
    # calculate average squared distance
    #sqrt(sum((a-mean(a))^2/(length(a)-1)))
     a = sum((x - m)^2)
     b = a/n
     c = sqrt(b)
     c= round(c,1)
}
#result4 <- sd(elements,na.rm = FALSE)
sd <- Calcsd(elements,n,mean)
#
# error <- qt(0.975,df=length(w1$vals)-1)*sd(w1$vals)/sqrt(length(w1$vals))
error <- 1.96 * sd * (1/sqrt(n))
error <- round(error,1)
lower = mean - error
upper = mean + error

# output
cat(mean, sep="\n")
cat(median, sep="\n")
cat(mode, sep="\n")
cat(sd, sep="\n")
cat(lower,upper)
#cat(upper)
close(stdin)


