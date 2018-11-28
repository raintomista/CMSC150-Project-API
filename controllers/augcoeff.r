augcoeffmatrix <- function(equations) {
  vector = c();
  # check if the variables of all the equations are the same
  if(length(variables(equations)) > 1){
    for(i in 1:length(equations)) {
      rawString = equations[[i]];
      equation = "";
      for(x in 2:length(deparse(rawString))) {
        equation = paste(equation, deparse(rawString)[x]);
      }
      
      terms = strsplit(equation, "+", fixed="true")[[1]];
      

      
      for(j in 1: length(terms)) {
        term = strsplit(terms[j], "*", fixed="true")[[1]];
        index = ((i-1) * length(terms)) + j
        coefficient = as.numeric(term[1]); #get 
        

        # store the coefficient to vector
        if(j != length(terms)) {
          variable = gsub(" ", "", term[2]);
          variable = gsub("x", "", variable);
          variableCount = as.numeric(variable) + 1;
          
          index = ((i-1) * length(terms)) + variableCount;
          vector[index] = coefficient;
        }
        
        # transpose constant to the rhs
        else {
          vector[index] = -(coefficient);
        }
      }
    }
    
    # create vector for column labels
    labelCol = 1:length(equations)
    
    #create vector for row labels
    labelRow = variables(equations); #get the variable names
    labelRow[length(labelRow)+1] = "RHS"; #add rhs to row labels

    return (matrix(vector, nrow=length(equations), byrow=TRUE));
  }
  else {
    return (NA);
  }
}


variables <- function(equations) {
  variableNames = c();
  
  for(i in 1:length(equations)){
    rawString = equations[[i]];
    equation = "";
    for(x in 2:length(deparse(rawString))) {
      equation = paste(equation, deparse(rawString)[x]);
    }
    terms = strsplit(equation, "+", fixed="true")[[1]];
    
    #get variable in every term
    for(j in 1: length(terms)) {
      #separate coefficient with variable
      term = strsplit(terms[j], "*", fixed="true")[[1]];
      variable = term[2];
      
      #disregard constant term
      if(!is.na(variable)) {
        variable =  gsub(" ", "", variable); #remove whitespaces
        
        #Store to variable names when parsing first equation
        if(i == 1) {
          variableNames[j] = variable;
        }
        # compare variable to the first equation's variables
        else {
          
          # if variable is not the same with the first equation, NA will be return
          if(!is.element(variable, variableNames)) {
            return (NA);
          }
        }
      }
    }
  }
  return (variableNames)
}