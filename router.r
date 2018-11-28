

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
  augcoeffmatrix = result$augcoeffmatrix;
  unknowns = result$unknowns;
  
  
  list(augcoeffmatrix=augcoeffmatrix, unknowns=unknowns)
}