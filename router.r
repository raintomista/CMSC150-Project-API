source('~/CMSC150-Project-API/controllers/regression.r')
source('~/CMSC150-Project-API/controllers/interpolation.r')

#* @filter cors
cors <- function(res) {
  res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
  plumber::forward()
}

#* Echo back the input
#* @param msg The message to echo
#* @post /regression
function(req){
  formContents = Rook::Multipart$parse(req);
  file <- readLines(con = formContents$file$tempfile)
  data = strsplit(file, '\n');
  x = c();
  y = c();
  
  for(i in 1:length(data)) {
    tuple = strsplit(data[[i]][1], ',');
    x[i] = as.numeric(tuple[[1]][1]);
    y[i] = as.numeric(tuple[[1]][2]);
  }
  
  result = PolynomialRegression(2, list(x, y))
  inputValues = result$inputValues;
  augcoeffmatrix = result$augcoeffmatrix;
  unknowns = result$unknowns;
  fx = result$polynomial_string;
  estimate = result$polynomial_function(as.numeric(formContents$valueToEstimate));
    
  list(inputValues=inputValues, augcoeffmatrix=augcoeffmatrix, unknowns=unknowns, fx=fx, estimate=estimate)
}

#* Echo back the input
#* @param msg The message to echo
#* @post /interpolation
function(req){
  formContents = Rook::Multipart$parse(req);
  
  valueToEvaluate <- as.numeric(formContents$valueToEstimate);
  file <- readLines(con = formContents$file$tempfile);
  
  #get x-y data from file
  data = strsplit(file, '\n');
  x = c(); 
  y = c();
  for(i in 1:length(data)) {
    tuple = strsplit(data[[i]][1], ',');
    x[i] = as.numeric(tuple[[1]][1]);
    y[i] = as.numeric(tuple[[1]][2]);
  }
  
  #store input values
  inputValues = list(x,y);
  
  #perform gauss jordan
  result = quadraticInterpolation(list(x, y))
  
  #get the augcoeff
  augcoeffmatrix = result$augcoeffmatrix;
  
  #get the unknowns
  unknowns = result$unknowns;
  
  #get the function strings for each interval
  interval_strings = result$interval_strings;
  interval_functions = result$interval_functions;
  
  estimates = c();
  for(i in 1:length(interval_functions)) {
    estimates[i] = interval_functions[[i]](valueToEvaluate);
  }
  
  
  list(inputValues=inputValues, augcoeffmatrix=augcoeffmatrix, unknowns=unknowns, interval_strings=interval_strings, estimates=estimates)
}