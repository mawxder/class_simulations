# Numerical solution to differential equations

# Instructions: Fix the code


y.prime.diff.eqn <- function(p, y) {return(5*y-p)} # Return desired function dY/dP


initial.condition.x1 <- 0
initial.condition.y1 <- 0

y.prime <- y.prime.diff.eqn(initial.condition.x1, initial.condition.y1)

euler <-  function(x1 = initial.condition.x1,
                   y1 = initial.condition.y1,
                   iter = 100,
                   step.size = 1) {
  
  for (i in 2:iter)
    
  {
  
    x1[i]     <-  x1[i-1] + step.size
    y1[i]     <-  y1[i-1] + step.size * (y.prime)
    y.prime   <-  y.prime.diff.eqn(x1[i], y1[i]) 
    
  }
  
  return(data.frame(cbind(x1,y1)))
  
}


output <- euler()

output

