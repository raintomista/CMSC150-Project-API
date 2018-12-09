# objective = 'Z = 10x1 + 6x2 + 3x3 + 8x4 + 5x5 + 4x6 + 6x7 + 4x8 + 5x9 + 5x10 + 3x11 + 5x12 + 4x13 + 6x14 + 9x15'
# constraints = '1x1 + 1x2 + 1x3 >= 180 \n 1x4 + 1x5 + 1x6 >= 80 \n 1x7 + 1x8 + 1x9 >= 200 \n 1x10 + 1x11 + 1x12 >= 160 \n 1x13 + 1x14 + 1x15 >= 220 \n 1x1 + 1x4 + 1x7 + 1x10 + 1x13 <= 310 \n 1x2 + 1x5 + 1x8 + 1x11 + 1x14 <= 260 \n 1x3 + 1x6 + 1x9 + 1x12 + 1x15 <= 280'

generateObjectiveFunction <- function(matrix) {
  cols = ncol(matrix)-1;
  rows = nrow(matrix)-1;
  
  fxn = paste('Z', sep='');
  idx = 1;
  
  for(i in 1:cols) {
    for(j in 1:rows) {
      if(idx == 1) {
        fxn = paste(fxn, ' = ', matrix[j, i], 'x', idx, sep='')
      } else {
        fxn = paste(fxn, ' + ', matrix[j, i], 'x', idx, sep='')
      }
      idx = idx + 1;
    }
  }
  
  return(fxn)
}

generateConstraints <- function(matrix) {
  cols = ncol(matrix)-1;
  rows = nrow(matrix);
  
  constraints = '';
  
  idx = 1;
  
  #demands
  for(i in 1:cols) {
    constraint = paste('', sep='');
    
    for(j in 1:rows) {
      if(j < (rows-1)) {
        constraint = paste(constraint, '1x', idx, ' + ', sep='');
        idx = idx + 1;
      } else if (j == (rows-1)) {
        constraint = paste(constraint, '1x', idx, ' >= ', sep='');
        idx = idx + 1;
      } else {
        constraint = paste(constraint, matrix[j,i], sep='')
      }
    }
    
    constraints = paste(constraints, constraint, ' \n ', sep='')
  }
  
  #supply
  for(i in 1:(rows-1)) {
    constraint = paste('', sep='');
    for(j in 1:(cols+1)) {
      if(j == (cols+1)) {
        constraint = paste(constraint, matrix[i, j], sep='');
      } else if(j == cols) {
        idx = i + ((j-1) *3);
        constraint = paste(constraint, '1x', idx, ' <= ', sep='');
      } 
      else {
        idx = i + ((j-1) *3);
        constraint = paste(constraint, '1x', idx, ' + ', sep='');
      }
    }
    
    constraints = paste(constraints, constraint, ' \n ', sep='')
  }
  
  return(constraints)
}


parseConstraints <- function(constraintString) {
  constraints = strsplit(constraintString, '\n', fixed=FALSE)[[1]]
  equations = c();
  for(i in 1:length(constraints)) {
    str = trimws(constraints[i]);
    
    if(grepl('<=', str, fixed=TRUE)) {
      token = strsplit(str, '<=', fixed=TRUE)[[1]]
      equations[i] = paste(token[1], '+ 1s', i, ' =', token[2], sep='')
    } else if(grepl('>=', str, fixed=TRUE)) {
      token = strsplit(str, '>=', fixed=TRUE)[[1]]
      terms = strsplit(token[1], '+', fixed=TRUE)[[1]]
      
      equations[i] = '';
      
      for(j in 1:length(terms)) {
        equations[i] = paste(equations[i], '-', trimws(terms[j]), ' + ', sep='');
  
      }
      
      equations[i] = paste(equations[i], '1s', i, ' = -', trimws(token[2]), sep='')
    }
  }

  return (equations);
}


createTableau <- function(objective, constraints) {
  #split Z with the terms
  objectiveZ = trimws(strsplit(objective, '=')[[1]][1]);
  objectiveTermString = trimws(strsplit(objective, '=')[[1]][2]);
  objectiveTerms = strsplit(objectiveTermString, '+', fixed=TRUE)[[1]];
  
      
  parsedConstraints = parseConstraints(constraints);
  
  nslackvars = length(parsedConstraints)
  nvariables = length(objectiveTerms);
  
  tableau = list();
  
  #constraints
  for(i in 1:length(parsedConstraints)) {
    row = rep(c(0), times=nslackvars+nvariables+2);
    termString = trimws(strsplit(parsedConstraints[i], '=')[[1]][1]);
    RHS = trimws(strsplit(parsedConstraints[i], '=')[[1]][2]);
    
    terms = strsplit(termString, '+', fixed=TRUE)[[1]]
    
    for(j in 1:length(terms)) {
      term = trimws(terms[j]) #get each term
      
      x = strsplit(term, 'x', fixed=TRUE)[[1]]
      
      if(length(x) == 2) {
        idx = as.numeric(x[2]);
        row[idx] = as.numeric(x[1])
      } else  {
        s = strsplit(term, 's', fixed=TRUE)[[1]]
        idx = as.numeric(s[2]) + nvariables;
        row[idx] = as.numeric(s[1]);
      }
    }
    
    row[nvariables+nslackvars+2] = as.numeric(RHS);
    tableau[[i]] = row;
  }

  #Initialize objective function row to all zero elements
  row = rep(c(0), times=nslackvars+nvariables+2);
  
  #Objective Z
  if(objectiveZ == 'Z') {
    row[nvariables+nslackvars+1] = 1;
  }
  
  #Other Objective Function Columns
  for(i in 1:length(objectiveTerms)) {
    x = strsplit(objectiveTerms[i], 'x', fixed=TRUE)[[1]]
    idx = as.numeric(x[2]);
    row[idx] = as.numeric(x[1]);
    
  }
  
  tableau[[nslackvars+1]] = row;
  
  
  #label
  labels = c();
  for(i in 1:nvariables) {
    labels[i] = paste('x', i, sep='');
  }
  
  #label for slack vars
  for(i in 1:nslackvars) {
    labels[nvariables+i] = paste('s', i, sep='');
  }
  
  #label for Z
  labels[nvariables+nslackvars+1] = 'Z'
  labels[nvariables+nslackvars+2] = 'RHS'
  
  tableauMatrix = matrix(unlist(tableau), nrow=nslackvars+1, byrow=TRUE);
  colnames(tableauMatrix) <- labels
  
  
  labelRow = c();
  #label for slack vars
  for(i in 1:nslackvars) {
    labelRow[i] = paste('s', i, sep='');
  }
  
  labelRow[nslackvars+1] = 'Z'
  rownames(tableauMatrix) <- labelRow
  
  
  return (list(tableau=tableauMatrix, nvariables=nvariables, nslackvars=nslackvars))
}


checkForNegative <- function(row) {
  return (sign(min(row)));
}

gaussJrdn <- function(matrix, pivotElementIdx, pivotColumnIdx, lastIdx) {
  lastColIdx = ncol(matrix)
  pivotElement = matrix[pivotElementIdx, pivotColumnIdx]

  #normalize the pivot row
  for(i in 1:lastColIdx) {
    matrix[pivotElementIdx, i] = matrix[pivotElementIdx, i]/pivotElement;
  }
  
  #elimination
  for(i in 1:lastIdx) {
    if(i != pivotElementIdx) {
      #get the first element of the tbe row
      tbe = matrix[i, pivotColumnIdx];
      
      for(j in 1:lastColIdx) {
        #get the multiplier in the pivot row
        mult = matrix[pivotElementIdx, j];
        
        #new value of the element to be eliminated 
        matrix[i, j] = matrix[i, j] - (tbe*mult);
      }
    }
  }
  
  return(matrix);
}

#returns index of the smallest positive
getSmallestPositive <- function(testRatio) {
  smallestIdx = 1; smallest = 999999;
  for(i in 1:length(testRatio)) {
    if(testRatio[i] < smallest && testRatio[i]>0) {
      smallestIdx = i;
      smallest = testRatio[i];
    }
  }
  
  return (smallestIdx);
}


phaseOne <- function(matrix) {
  lastIdx = nrow(matrix);
  lastColIdx = ncol(matrix);
  
  pivotElementIdx = which.min(matrix[,lastColIdx][1:(lastIdx-1)]);
  pivotColumnIdx = which.min(matrix[pivotElementIdx, 1:(lastColIdx-1)])
  pivotElement = matrix[pivotElementIdx, pivotColumnIdx]
  #normalize the pivot row
  for(i in 1:lastColIdx) {
    matrix[pivotElementIdx, i] = matrix[pivotElementIdx, i]/pivotElement;
  }

  #elimination
  for(i in 1:lastIdx) {
    if(i != pivotElementIdx) {
      #get the first element of the tbe row
      tbe = matrix[i, pivotColumnIdx];

      for(j in 1:lastColIdx) {
        #get the multiplier in the pivot row
        mult = matrix[pivotElementIdx, j];

        #new value of the element to be eliminated
        matrix[i, j] = matrix[i, j] - (tbe*mult);
      }
    }
  }

  return (matrix)
}

checkNegative <- function(matrix) {
  lastCol = ncol(matrix);
  lastRow = nrow(matrix)
  
  minimum = min(matrix[,lastCol][1:(lastRow-1)]);
  
  if(minimum >= 0) {
    return (FALSE);
  } else {
    return (TRUE);
  }
}

Simplex <- function(objective, constraints) {
  result = createTableau(objective, constraints);
  matrix = result$tableau;
  nvariables = result$nvariables;
  nslackvars = result$nslackvars;
  
  response = list();
  response_count = 0;
  
  response[[response_count+1]] = list(title='Initial Tableau', iteration=response_count, tableau=matrix);
  response_count = response_count + 1;
  
  while(checkNegative(matrix)) {
    matrix = phaseOne(matrix);
    response[[response_count+1]] = list(title='Phase 1', iteration=response_count, tableau=matrix);
    response_count = response_count + 1;
  }
  


  lastIdx = nrow(matrix);
  lastRow = matrix[lastIdx,];
  lastCol = ncol(matrix)
  
  

  while(checkForNegative(lastRow) == -1) {
    lastIdx = nrow(matrix);
    lastRow = matrix[lastIdx,1:(lastCol-1)];

    highestMagnitude = min(lastRow);
    pivotColumnIdx = match(highestMagnitude, lastRow);

    #get pivot column without the last row of the matrix
    pivotColumn = matrix[1:(lastIdx-1),][,pivotColumnIdx];

    solutionIdx = ncol(matrix);
    solutionColumn = matrix[1:(lastIdx-1),][,solutionIdx];

    #get pivot element which is the smallest positive test ratio
    pivotElementIdx = getSmallestPositive(solutionColumn/pivotColumn);

    matrix = gaussJrdn(matrix, pivotElementIdx, pivotColumnIdx, lastIdx);
    
    response[[response_count+1]] = list(title='Phase 2', iteration=response_count, tableau=matrix);
    response_count = response_count + 1;
  }

  unknowns = rep(c(0), times=nvariables)
  for(i in 1:nvariables) {
    col = matrix[,i];
    nzeroes = length(which(col == 0));
    nones = length(which(col == 1));
    
    if(nones == 1 && nzeroes == (length(col)-1)) {
      idx = which.max(col);
      unknowns[i] = matrix[,lastCol][idx];
    }
  }
  
  zValue = matrix[nslackvars+1, nvariables+nslackvars+2] * -1;
  
  response[[response_count+1]] = list(title='Final Tableau', iteration=response_count, tableau=matrix, unknowns=unknowns, zValue=zValue);
  response_count = response_count + 1;

  return (response);
}