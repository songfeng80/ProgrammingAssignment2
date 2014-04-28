#####This function creates a special "matrix" object that can cache its inverse.#####
#####the function returns a list of four functions: set() which can be used to ######
#####input a new matrix; get() which can be used to print the existing matrix; ######
#####setinverse()which can be used to update the cached inverse matrix; ######
#####getinverse()which can be used to print the cached inverse matrix; ######

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



#####computes the inverse of the special "matrix" returned by makeCacheMatrix above.#####
#####If the inverse has already been calculated (and the matrix has not changed), then ####
#####the cachesolve() function should retrieve the inverse from the cache. ###########
#####If there is no inverser matrix cached, then the function calculate the inverse matrix####
###### and then save the inverse matrix into the cache using the setinverse() function.#######
###### from makeCacheMatrix. For this assignment, assume that the matrix supplied is always invertible.########

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
