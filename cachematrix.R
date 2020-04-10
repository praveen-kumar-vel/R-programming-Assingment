## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix(sample(1:100,9),3,3)) {
  
  s <- NULL
  
  set <- function(y) {
    
    x <<- y
    
    s <<- NULL
    
  }
  
  get <- function() x
  
  setsolve <- function(solve) s <<- solve
  
  getsolve <- function() s
  
  list(set = set, get = get,
       
       setsolve = setsolve,
       
       getsolve = getsolve)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.


cacheSolve <- function(x, ...) {
  
  s <- x$getsolve()
  
  if(!is.null(s)) {
    
    message("getting inversed matrix")  
    
    return(s) #return s value
    
  }
  
  data <- x$get()
  
  s <- solve(data, ...)
  
  x$setsolve(s)
  
  s
  
}

#SHA-1 #b222f64926380ac7e69332993ba1d0f0fd680de0
