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
  r1 <- 1678
  r2 <- 1734
  r3 <- 2014
  r4 <- 2536
  r5 <- 2834
  r6 <- 2927
  
  p_1 <- r1 * 50
  p_2 <- r2 * 50
  p_3 <- r3 * 100
  p_4 <- r4 * 100
  p_5 <- r5 * 100
  
  p1 = r1 * n
  p2 = r2 * (n - 50) + p_1
  p3 = r3 * (n - 100) + p_2 + p_1
  p4 = r4 * (n - 200) + p_3 + p_2 + p_1
  p5 = r5 * (n - 300) + p_4 + p_3 + p_2 + p_1
  p6 = r6 * (n - 400) + p_5 + p_4 + p_3 + p_2 + p_1

  if(n <= 50){
    print(paste("Price: ", p1))
  }else if(n <= 100){
    print(paste("Price: ", p2))
  }else if(n <= 200){
    print(paste("Price: ", p3))
  }else if(n <= 300){
    print(paste("Price: ", p4))
  }else if(n <= 400){
    print(paste("Price: ", p5))
  }else{ 
    print(paste("Price: ", p6))
  }
}
    
check_prime <- function(n){
  if(n == 2){
    ch <- paste(n, "is a prime.")
  }else{
    for(i in n){
      if(n %% 2 == 0){
        ch <- paste(n, "is not a prime.")
      }else{
        ch <- paste(n, "is a prime.")
      }
    }
  }
  print(ch)
}
    
getmode <- function(v){
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}
  
clusters <- function(x, centers){
  #compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
  function(i) apply(centers, 1, function(v) sum ((x[i, ]-v)^2)))
  max.col(-t(tmp)) #find index of min distance
}
