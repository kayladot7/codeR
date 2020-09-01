cal_bmi <- function(weight, height){
  bmi <- round(weight/(height^2),2)
  return(bmi)
}

solve_equation <- function(a, b){
  if(a == 0){
    if(b != 0){
      x <- "The equation has no solution."
    }else{
      x <- "The equation has countless solutions."
    }
  }else{
  x <- paste("x =", -b/a)
  }
  return(x)
}

equation_solver_2 <- function(a,b,c){
  if(a == 0){
    message("Back to use the function of equation solver 1:\na <- b\nb <- c")
    x <- solve_equation(b, c)
  }else{
    delta <- b^2 - 4*a*c
    #print(paste("delta: ", delta))
    if(delta < 0){
      x <- "The equation has no solution."
    }else if(delta == 0){
      x <- paste("Equations have double solutions:\nx1 = x2 = ", (-b)/(2*a)) 
    }else{
      x1 <- (-b + sqrt(delta))/(2*a)
      x2 <- (-b - sqrt(delta))/(2*a)
      x <- paste("Equations have two distinct solutions:\n\tx1 = ", x1, "\n\tx2 = ", x2)
    }
  }
  return(x)
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
