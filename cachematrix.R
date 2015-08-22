## gets a matrix input and inverses the matrix 
## caches the value of the inverse and returns the cache value for 
## further calls instead of computing again



## this function can be used as a<- makeCacheMatrix(matrix(1:4,2,2))
## this function has 4 functions: set, get, setinverse and getinverse
## which can be used to set and get the matrix and set and get the ## ## inverse of the matrix respectively

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## computes the inverse of the matrix and caches the value
## returns the cache value if value is already computed/available

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
