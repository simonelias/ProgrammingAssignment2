## Put comments here that give an overall description of what your
## functions do
## These 2 functions will demonstrate how to cache result for long running 
## and provide the cashe value when it is requested if the data used to 
## calculate the the result did't change. In these 2 functions we will 
## create a matrix and calculate its inverse then cashe it for later use. 

## Write a short comment describing this function
## In this function we create a special matrix object and cashe its inverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of matrix
  setinv <- function(inv) i <<- inv
  
  ## get the value of the inverse of matrix
  getinv <- function() i
  
  ## return list containing the above functions
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## In this function we compute the inverse of the special matrix returned
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.
cacheSolve <- function(x, ...) {
  ## get inverse value from cache
  i <- x$getinv()
  
  ## if inverse value returned is not null then return the cached inverse
  ## else the inverse hasn't been calculated yet
  if(!is.null(i)) {
    message("getting cached inverse")
    return(i)
  }
  
  ## get the special matrix and calculate its inverse
  data <- x$get()
  i <- solve(data, ...) 

  ## test matrix multiplication 
  ## i <- solve(data, ...) %*% data
  
  ## cache the inverse for later use
  x$setinv(i)

  ## Return a matrix that is the inverse of 'x'
  i
}

## Examples:
## cacheSolve(makeCacheMatrix(rbind(c(1, -1/4), c(-1/4, 1))))
##        [,1] [,2]
##  [1,]    1    0
##  [2,]    0    1

## cacheSolve(makeCacheMatrix(matrix(c(1,0,0,0,1,-4,0,0,1), 3, 3)))
##        [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    1    0
## [3,]    0    0    1

