x <- c(3.0, 4.5, 7.0, 9.0)
y <- c(2.5, 1.0, 2.5, 0.5)

generateEquations <- function(list) {
  x = list[[1]]
  y = list[[2]]
  
  baseIdx = 0;
  maxIdx = length(x);
  equations = list();
  noOfEquations = 0;

  for(i in 2:(length(x)-1)) {
    #Condition 1
    equations[[noOfEquations+1]] = paste(x[i]^2, "a", i-1, " + ", x[i], "b", i-1, " + ", "1c", i-1, " = ", y[i], sep="");
    equations[[noOfEquations+2]] = paste(x[i]^2, "a", i, " + ", x[i], "b", i, " + ", "1c", i, " = ", y[i], sep="");
    noOfEquations = noOfEquations + 2;
  }
  
  #Condition 2
  equations[[noOfEquations+1]] <- paste(x[baseIdx+1]^2, "a", 1, " + ", x[baseIdx+1], "b", 1, " + ", "1c", 1, " = ", y[baseIdx+1], sep="");
  equations[[noOfEquations+2]] <- paste(x[maxIdx]^2, "a", maxIdx-1, " + ", x[maxIdx], "b", maxIdx-1, " + ", "1c", maxIdx-1, " = ", y[maxIdx], sep="");
  noOfEquations = noOfEquations + 2;
  
  for(i in 2:(length(x)-1)) {
    #Condition 3
    equations[[noOfEquations+1]] = paste(2*x[i], "a", i-1, " + ", "1b", i-1, " + ", "-", 2*x[i], "a", i, " + ", "-", "1b", i, " = 0", sep="");
    noOfEquations = noOfEquations + 1;
  }
  

  
  return (equations)
}

augcoeffQSI <- function(equations) {
  augcoeff = list();
  for(i in 1:length(equations)) {
    terms_string = trimws(strsplit(equations[[i]], "=")[[1]][1]);
    RHS = trimws(strsplit(equations[[i]], "=")[[1]][2]);
    
    terms = strsplit(terms_string, "+", fixed=TRUE)
    
    if(i > length(equations) - 2) {
      row = rep(0, length(equations)+2);
      a1_string = trimws(terms[[1]][1]);
      a1_value = as.numeric(strsplit(a1_string, 'a', fixed=FALSE)[[1]][1]);
      a1_idx = as.numeric(strsplit(a1_string, 'a', fixed=FALSE)[[1]][2]);
      row[(3*a1_idx)-2] = a1_value;
  
      b1_string = trimws(terms[[1]][2]);
      b1_value = as.numeric(strsplit(b1_string, 'b', fixed=FALSE)[[1]][1]);
      b1_idx = as.numeric(strsplit(b1_string, 'b', fixed=FALSE)[[1]][2]);
      row[(3*b1_idx)-1] = b1_value;

      a2_string = trimws(terms[[1]][3]);
      a2_value = as.numeric(strsplit(a2_string, 'a', fixed=FALSE)[[1]][1]);
      a2_idx = as.numeric(strsplit(a2_string, 'a', fixed=FALSE)[[1]][2]);
      row[(3*a2_idx)-2] = a2_value;
      
      b2_string = trimws(terms[[1]][4]);
      b2_value = as.numeric(strsplit(b2_string, 'b', fixed=FALSE)[[1]][1]);
      b2_idx = as.numeric(strsplit(b2_string, 'b', fixed=FALSE)[[1]][2]);
      row[(3*b2_idx)-1] = b2_value;
      
      augcoeff[[i]] <- row;
    } else {
      row = rep(0, length(equations)+2);
      
      #A
      a_string = trimws(terms[[1]][1]);
      a_value = as.numeric(strsplit(a_string, 'a', fixed=FALSE)[[1]][1]);
      a_idx = as.numeric(strsplit(a_string, 'a', fixed=FALSE)[[1]][2]);
      row[(3*a_idx)-2] = a_value;
      
      #B
      b_string = trimws(terms[[1]][2]);
      b_value = as.numeric(strsplit(b_string, 'b', fixed=FALSE)[[1]][1]);
      b_idx = as.numeric(strsplit(b_string, 'b', fixed=FALSE)[[1]][2]);
      row[(3*b_idx)-1] = b_value;
      
      #C
      c_string = trimws(terms[[1]][3]);
      c_value = as.numeric(strsplit(c_string, 'c', fixed=FALSE)[[1]][1]);
      c_idx = as.numeric(strsplit(c_string, 'c', fixed=FALSE)[[1]][2]);
      row[(3*c_idx)] = c_value;
      
      #RHS
      row[length(equations)+2] = as.numeric(RHS);
      
      augcoeff[[i]] <- row;

    }
  }
  
  mat = matrix(unlist(augcoeff), ncol=length(equations)+2, byrow=TRUE);
  mat = mat[,-1]

  return (mat);
}

gaussJordanQSI <- function(list) {
  equations = generateEquations(list);
  matrix = augcoeffQSI(equations)
  
  ncol = ncol(matrix) - 1;
  
  #traverse all columns except RHS
  for(j in 1:ncol) {
    
    #initial jth column for every iteration
    jthColumn = unname(matrix[,j]);
    
    #get the total number of rows
    nrow = nrow(matrix)
    
    pivotIdx = j; #pivot row index and pivot element column
    
    #get the row index of the highest element found in the column
    newPivotIdx = getMaxIdx(pivotIdx, jthColumn);
    
    #swap the row of the highest element with the current pivot row to avoid division by zero
    matrix = swapRows(matrix, pivotIdx, newPivotIdx);
    
    
    jthColumn = unname(matrix[, j]); #update jthColumn to reflect swaping
    pivotElement = jthColumn[pivotIdx] #set the pivot element of the new pivot row
    
    #normalize the pivot row by dividing the it to the pivot element
    matrix[pivotIdx, ] <- matrix[pivotIdx, ] / pivotElement;
    
    #traverse every element of the jth row
    for(i in 1:nrow) {
      
      #perform elimination operation on all column elements except the pivot element
      if(i != pivotIdx) {
        jthColumn = unname(matrix[, j]); #update jthColumn again to reflect normalization or elimination
        pivotElement = jthColumn[pivotIdx] #get pivot element again to reflect normalitation or elimation
        
        tbe = jthColumn[i]; #element to be eliminated
        mult = tbe / pivotElement; #get the multiplier by dividing the element to be eliminated by the pivot element
        productRow = mult * matrix[pivotIdx, ]; #multiply the pivot row by the multiplier
        
        matrix[i, ] <- (matrix[i, ] - productRow) #subtract the target row by productRow to eliminate the element tbe
        
      }
    }
  }
  return (list(augcoeffmatrix=augcoeffQSI(equations), unknowns=unname(matrix[ ,ncol(matrix)])));
}
