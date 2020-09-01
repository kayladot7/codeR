cal_bmi <- function(weight, height){
  bmi <- round(weight/(height^2),2)
  return(bmi)
}

solve_equation <- function(a, b){
  a <- as.numeric(readline(prompt = "Input value of a: "))
  b <- as.numeric(readline(prompt = "Input value of b: "))
  if(a == 0){
    if(b != 0){
      x <- "The equation has no solution."
    }else{
      x <- "The equation has countless solutions."
    }
  }else{
  x <- paste("x =", -b/a)
  }
  print(x)
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

electric_cal <- function(n){
  p1 = 1.678 * n
  p2 = 1.734 * (n - 50)
  p3 = 2.014 * (n - 100)
  p4 = 2.536 * (n - 200)
  p5 = 2.834 * (n - 300)
  p6 = 2.927 * (n - 400)

  if(n <= 50){
    print(paste("Price: ", p1))
  } else if(n <= 100){
    print(paste("Price: ", p1 + p2))
  } else if(n <= 200){
    print(paste("Price: ", p1 + p2 + p3))
  } else if(n <= 300){
    print(paste("Price: ", p1 + p2 + p3 + p4))
  } else if(n <= 400){
    print(paste("Price: ", p1 + p2 + p3 + p4 + p5))
  } else { 
    print(paste("Price: ", p1 + p2 + p3 + p4 + p5 + p6))
  }
  return 
}
