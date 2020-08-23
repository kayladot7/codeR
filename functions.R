cal_bmi <- function(weight, height){
  bmi <- round(weight/(height^2),2)
  return(bmi)
}

sum_even <- function(start, end){
  sum_even <- 0
  for(i in start:end){
    if(i %% 2 == 0){
      sum_even <- sum_even + i
      #print(i)
    }      
  }
  return(sum_even)
}

drink <- function(price, type="Tea"){
  print(paste("With", price, "USD, you can drink", type,"then."))
}