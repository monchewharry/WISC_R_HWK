rm(list=ls())

f = function(x,y) { # Define function z = f(x, y).
  return(-(cos(x)^2 + cos(y)^2)^2)
}
gradient.f = function(x, y) { # Define function grad(z) = (df.dx, df.dy).
  df.dx = -2 * (cos(x)^2 + cos(y)^2) * (2 * cos(x) * (-sin(x)))
  df.dy = -2 * (cos(x)^2 + cos(y)^2) * (2 * cos(y) * (-sin(y)))
  return(c(df.dx, df.dy))
}

# Plot function, z = f(x, y) over the region -pi/2 <= x, y <= pi/2.
grid.n = 20
limit = pi/2
grid.x = seq(-limit, limit, length.out=grid.n)
grid.y = grid.x
grid.z = outer(grid.x, grid.y, f)
persp.out = persp(grid.x, grid.y, grid.z, theta = 45, phi=60,
  main="Gradient descent", ticktype="detailed",
  xlab="x", ylab="y", zlab="z")

x = -.98*limit # Starting x
y = .7*limit   # Starting y
n = 20
gamma = .1 # Step size parameter.

for(i in seq_len(n)) {
  z = f(x, y)
  points(trans3d(x=x, y=y, z=z, pmat=persp.out), col="red")
  scan(what=character(), n=1, quiet=TRUE) # Require "Enter" to move.
  g = gradient.f(x, y)
  lines(trans3d(x=c(x, x), y=c(y, y), z=c(0, z), pmat=persp.out), col="red")
  old.x = x
  old.y = y
  x = x - gamma * g[1]
  y = y - gamma * g[2]
  lines(trans3d(x=c(old.x, x), y=c(old.y, y), z=c(0, 0), pmat=persp.out),
        lwd=3, col=ifelse(test=(i %% 2 == 0), yes="green", no="blue"))
}

cat(sep="", "f(", x, ", ", y, ")=", f(x, y), ", gradient.f()=(", g[1], ", ", g[2], ")\n")

# ----------------------------------------------------------------------
# Now convert the code above into a function in the style of R's optim():
# Description: gradient.descent minimizes a function whose gradient is given
# Usage: gradient.descent(par, gr, gamma=.1, epsilon=.01, n=20, verbose=FALSE, ...)
# Parameters:
#   par: a vector of initial values for the function to be minimized
#   gr: gradient function to be evaluated at par
#   gamma: step size parameter
#   epsilon: stop the algorithm as converged when the distance a step
#     took is smaller than this value
#   n: stop after executing n iterations (to prevent an infinite loop)
#   verbose: if true, print x and |x_{i+1} - x_i| on each iteration
#   ...: additional arguments to be passed to gr()
# Details: gradient.descent iteratively steps in the direction
#   opposite the gradient, quitting after convergence or n iterations
# Value: the value of par at convergence or n iterations
#
gradient.descent = function(par, gr, gamma=.1, epsilon=.01, n=30, verbose=FALSE, ...) {
  for (i in 1:n) {
    gradient = gr(par, ...)
    par = par - gamma * gradient#move the point in the reverse direction of gradiant
    gradient.size = sum(abs(gradient))
    if (verbose) {
      cat(sep="", "i=", i, ", par=c(", paste(signif(par, 4), collapse=","),
          "), gradient=c(", paste(signif(gradient, 4), collapse=","),
          "), size=", signif(gradient.size, 4), "\n")
    }
    if (gradient.size < epsilon) {
      break
    }
  }
  return(par)
}

# Test it on the simple function
f = function(x) { x^2 }
# which has a minimum at x=0. Use x=1 as the starting point.
g = function(x) { 2*x }#function of gradiant
gradient.descent(par=c(1), gr=g, verbose=TRUE)

# Test it on the cool cosine mess above. First convert its gradient
# function to receive the current location as a vector "par", not as x
# and y.

gradient.f.using.par = function(par) { # Define function grad(z) = (df.dx, df.dy).
  x = par[1]
  y = par[2]
  df.dx = -2 * (cos(x)^2 + cos(y)^2) * (2 * cos(x) * (-sin(x)))
  df.dy = -2 * (cos(x)^2 + cos(y)^2) * (2 * cos(y) * (-sin(y)))
  return(c(df.dx, df.dy))
}
gradient.descent(par=c(.98*limit, .7*limit), gr=gradient.f.using.par, verbose=TRUE)
