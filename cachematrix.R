##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
##matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not 
## discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.



## Function to create special matrix object that cache its inverse
makeCacheMatrix <- function(x = matrix()){
  m <- NULL # set m to NULL to be used as a function later
  
  # Take the matrix inside the function makeCacheMatrix to assign a value
  # which is different from the current environment through <<-
  set <- function(y){ 
    x <<- y
    m <<- NULL
  }
  
  # Enable to to get the value of the matrix passed as an argument above
  # or provide the code to set and get inverse values
  get <- function() x
  setInverse <- function(m) m <<- m
  getInverse <- function() m 
  
  
  # Returns list of functions to make them available as an object in cachesolve
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Function checks if matrix was inversed. If yes then get it from the cache
cacheSolve <- function(x, ...){
  m <- x$getInverse()
  
  # check if matriy was inversed. If yes, return the inverse
  if(!is.null(m)){
    message ("getting cached data")
    return(m)
  }
  # if not inversed get the matrix
  data <- x$get()
  
  # invert the matrix
  m <- solve(data, ...)
  
  # call set inverse from makeCacheMatrix
  x$setInverse(m)
  
  # Prints result
  m
}


## Check the functionality

# create a matrix
mymatrix <- matrix(c(7, 6, 13, 90), nrow = 2, ncol = 2)
mymatrix

# Check functionality of makeCacheMatriy and cachesolve
mymatrix_c <- makeCacheMatrix(mymatrix)
mymatrix_c
mymatrix_c$get()
cacheSolve(mymatrix_c)
                            
