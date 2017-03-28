##### Trapezoidal Integration

trapezoidal.integration = function(x, f)
{
  ### 3 checks to ensure that the arguments are numeric and of equal lengths
  # check if the variable of integration is numeric
  if (!is.numeric(x))
  {
    stop('The variable of integration "x" is not numeric.')
  }
  
  # check if the integrand is numeric
  if (!is.numeric(f))
  {
    stop('The integrand "f" is not numeric.')
  }
  
  # check if the variable of integration and the integrand have equal lengths
  if (length(x) != length(f))
  {
    stop('The lengths of the variable of integration and the integrand do not match.')
  }
  
  ### finish checks
  
  # obtain length of variable of integration and integrand
  n = length(x)
  
  # integrate using the trapezoidal rule
  integral = 0.5*sum((x[2:n] - x[1:(n-1)]) * (f[2:n] + f[1:(n-1)]))
  
  # print the definite integral
  return(integral)
}


##PRUBA CON UNA FUNCION DE DENSIDAD
beta.support = seq(0, 1, by = 0.005)

# obtain the Beta(2, 5) PDF based on the given support set
beta.pdf = dbeta(beta.support, 2, 5)

# export image as PNG file to a user-defined folder
png('INSERT YOUR DIRECTORY PATH HERE/beta pdf.png')

# plot the PDF
plot(beta.support, beta.pdf, main = 'Probability Density Function\nBeta(2, 5)', xlab = 'x', ylab = 'f(x)')

dev.off()

# use trapezoidal integration to integrate the PDF over its support set
trapezoidal.integration(beta.support, beta.pdf)
