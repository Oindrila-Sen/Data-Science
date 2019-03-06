# Function to Calculate life time based on input value
calcLifeTime <- function(input){
  if (input <= 4){
    output <- 2*input
  }
  else{
    output <- 8.00
  }
  return(output)
}
input <- read.table("stdin", sep=" ");
input <- as.matrix(as.data.frame(t(input)))
value <- calcLifeTime(input)
cat(value) 
