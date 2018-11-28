source('~/cmsc150-project-api/controllers/augcoeff.r')

getMaxIdx <- function (pivotIdx, column) {
  idx = pivotIdx;
  
  for(i in idx:length(column)) {
    if(abs(column[i]) > abs(column[idx])) {
      idx = i;
    }
  }
  return (idx);
}

swapRows <- function (matrix, pivotIdx, newPivotIdx) {
  temp <- unname(matrix[pivotIdx,]);
  matrix[pivotIdx, ] <- unname(matrix[newPivotIdx, ]);
  matrix[newPivotIdx, ] <- temp;
  return (matrix);
}

gaussJordan <- function(system) {
  matrix = augcoeffmatrix(system);
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
  return (unname(matrix[ ,ncol(matrix)]));
}