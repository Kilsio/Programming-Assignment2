### Function to create an object with methods to place inverse of a matrix in 
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

### Function that finds the inverse of a matrix
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

the =  matrix(c(4,3,3,2),2,2)
the ### Test Matrix
the2 = MakeCacheMatrix(the)
cacheSolve(the2) ### Validate if this is the inverse of matrix
