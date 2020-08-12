## This function generates an object including a matrix and a cache of a  
## calculated Matrix Inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) {
          x <<- y
          m <<-NULL
        }
        get <- function () { x }
        setMatrixInversion <- function(mi) { m <<- mi }
        getMatrixInversion <- function() { m }
        list (set=set, 
              get=get, 
              setMatrixInversion = setMatrixInversion, 
              getMatrixInversion = getMatrixInversion)
}


## This function either returns the cached matrix inversion or calculates the
## matrix inversion and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x  
        m <- x$getMatrixInversion()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        } 
        data <- x$get()
        m <- solve(data)
        x$setMatrixInversion(m)
        m
}
