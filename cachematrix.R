## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.
## Return a functions list of 'x' matrix
makeCacheMatrix <- function(x = matrix()) {
  ##init
  i <- NULL
  
  ## set matrix and init inverse matrix
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  ## get matrix
  get <- function() x
  
  ## set inverse matrix
  setinverse <- function(inverse) i <<- inverse
  
  ## get inverse matrix
  getinverse <- function() i
  
  ##return list object
  list(set = set, get=get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  ## Return a cached matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  
  ## if inverse is not cached, solve the matrix inverse and return the inverse of 'x'
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}