source('~/cmsc150-project-api/controllers/gauss_jordan.r')

summation <- function(xValues, power) {
  acc = 0;
  n = length(xValues);
  for(i in 1:n) {
    acc = acc + (xValues[i] ^ power);
  }
  
  return (acc);
}

generatePolynomialString <- function(vector) {
  string = "function(x) "
  for(i in 1:length(vector)){
    if(i == 1) {
      string = paste(string, vector[i], sep="")
    } else {
      string = paste(string, " + ", vector[i], " * x^", i-1, sep="");
    }
  }
  
  return (string);
}

summationRHS <- function(xValues, yValues, power) {
  acc = 0;
  n = length(xValues);
  for(i in 1:n) {
    x = (xValues[i] ^ power);
    y = yValues[i];
    acc = acc + (x*y);
  }
  
  return (acc);
}


PolynomialRegression <- function(order, list) {
  xValues = list[[1]];
  yValues = list[[2]];
  system = list();
  
  for(i in 1:(order+1)) {
    func = "function (";
    equation = "";
    xSub = 0;
    for(j in (i-1):(order+(i-1))) { 
      func = paste(func, "x", xSub, sep="");
      if(j != (order+(i-1))) {
        func = paste(func, ", ", sep="");
      } else {
        func = paste(func, ") ", sep="");
      }

      coefficient = formatC(summation(xValues, j), format="f");
      term = paste(coefficient, " * x", xSub, sep=""); 
      equation = paste(equation, term, " + ", sep="");

      xSub = xSub + 1;
    }
    
    rhs = -summationRHS(xValues, yValues, i - 1);
    equation = paste(func, equation, rhs, sep="");

    system[[i]] <- eval(parse(text=equation))
  }
  
  augcoeff <- augcoeffmatrix(system);
  unknowns <- gaussJordan(system);
  polynomial_string <- generatePolynomialString(unknowns)
  polynomial_function <- eval(parse(text=polynomial_string))
  
  return (list(augcoeffmatrix=augcoeff, unknowns=unknowns, polynomial_string=polynomial_string, polynomial_function=polynomial_function));
}