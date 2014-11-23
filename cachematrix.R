## Put comments here that give an overall description of what your
## functions do

## Create a special "vector", which is really a list cotaining a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL;
  
  ## set the value of the matrix
  set <- function(y) {
    matrix <<- y;
    inverseMatrix <- NULL;
  }
  
  ## get the value of the matrix
  get <- function() matrix;
  
  ## set the inverse of the matrix
  setInverse <- function(inverse) {
    
    ## set the global inverse matrix variable to the inverse of matrix x
    inverseMatrix <<- inverse;
  }
  
  ## gets the inverse of the matrix
  getInverse <- function() inverseMatrix;
  
  list(setMatrix = set, getMatrix = get, 
       setInverseMatrix = setInverse, getInverseMatrix = getInverse);
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x$getInverse();
  
  if(!is.null(inverse)) {
    message("getting the cached data");
    return(inverse);
  }
  
  message("calculating the inverse");
  ## get the matrix from the cache
  matrix <- x$getMatrix();
  
  ## solve it for inverse
  inverse <- solve(matrix);
  
  ## set the inverse to cache
  x$setInverse(inverse);
  
  ## return the calulated inverse
  inverse;
}
