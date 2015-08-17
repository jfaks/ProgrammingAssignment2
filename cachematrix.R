## 2015-08-18 DD R-Prod Assigmment 2
## Two functions with the purpose of being able to take(both set and get functionality)
## a matrix and calculate the inverse thereof.
## The calculated value is kept so that if the same matrix
## is used again , the cached result is reused hence the return of result

## Function that creates a list of functions (get,set,setInverse,getInverse)
## and initiates the matrix object within function environment (makeCacheMatrix)

makeCacheMatrix <- function(x = matrix()) {

 
  m <- NULL
  #create set and get function (as per best practice)
  set <- function(y) {
    x <<- y
    # Reset result so cacheSolve knows
    # recalculation of inverse in necessary 
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseM) m <<- inverseM
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function that calculates inverse of matrix created in makeCacheMatrix
## Function first check if result exists for in cache 

cacheSolve <- function(x, ...) {
  
  #get currently stored result
  m <- x$getInverse()
  #if result exists in m, use cached value instead
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # if no cached result exist , get matix
  data <- x$get()
  #calculate result, set new result value and return result
  m <- solve(data, ...)
  x$setInverse(m)
  m
  

}
