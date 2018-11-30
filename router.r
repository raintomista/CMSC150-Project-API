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
  file <- readLines(con = formContents$file$tempfile)
  data = strsplit(file, '\n');
  x = c();
  y = c();
  
  for(i in 1:length(data)) {
    tuple = strsplit(data[[i]][1], ',');
    x[i] = as.numeric(tuple[[1]][1]);
    y[i] = as.numeric(tuple[[1]][2]);
  }
  
  result = gaussJordanQSI(list(x, y))
  inputValues = result$inputValues;
  augcoeffmatrix = result$augcoeffmatrix;
  unknowns = result$unknowns;
  
  estimate = result$estimate;
  
  list(augcoeffmatrix=augcoeffmatrix, unknowns=unknowns, estimate=estimate)
}