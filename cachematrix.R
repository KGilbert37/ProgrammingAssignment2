## Together these functions cache the inverse of a matrix

## makeCacheMatrix creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { ## initializes x as a matrix
    m <- NULL ## initializes m as an object set to NULL
    set <- function(y) {  
      x <<- y ## assigns the input y to the x object in the parent environment 
      m <<- NULL ## assigns NULL to the m object in the parent environment
    }
    get <- function() x ## retrieves x from the parent environment
    setinverse <- function(inv) m <<- inv ## assigns the input inv to the m object in the parent environment
    getinverse <- function() m ## retrieves m from the parent environment
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse) ## assigns each of these functions to a list in the parent environment 
  }

## cacheSolve computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated and the matrix has not changed, 
## then cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
        m <- x$getinverse()  ## retrieves m from the parent environment using the x$getinverse function
        if(!is.null(m)) {
      message("getting cached data")
      return(m) ## if m is a cached value in the parent environment, returns m and text "getting cached data" 
    }
    data <- x$get() ## retrieves x from the parent environment
    m <- solve(data, ...) ## solves the inverse of x
    x$setinverse(m) ## sets the inverse of x to value m
    m ## Returns a matrix that is the inverse of 'x'
}
