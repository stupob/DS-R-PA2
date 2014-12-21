## makeCacheMatrix() creates a vector to cach a matrix and its inverse
## cacheSolve returns the inverse of a cached matrix created by makeCacheMatrix
 
makeCacheMatrix <- function(x = matrix()) {
	# caches a matrix and its inverse
	# Usage example:
	# m <- makeCacheMatrix(matrix(1:4, 2, 2))
	# m$set(matrix(c(1, -1, 0, 1), 2, 2))
    inv <- NULL				# will serve to store the cached inverse matrix
    set <- function(y){		# clears both the data and the cached inverse
        x <<- y
        inv <<- NULL
    }
    get <- function() x		# returns the matrix
    setinv <- function(i) inv <<- i	# saves the inverse in the cache
    getinv <- function() inv		# returns the cached inverse

    list(set= set, 			# returns the matrix with the newly defined functions
	    get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
    # returns the inverse matrix of the special "matrix" returned by 
	# makeCacheMatrix. If the inverse has already been calculated (and the 
	# matrix has not changed), cachesolve retrieves the inverse from the cache.
	# Usage example:
	# m <- makeCacheMatrix(...)
	# cacheSolve(m)
    inv <- x$getinv()
	
	# if the inverse is already calculated, returns it and exits
    if (!is.null(inv)){		
        message("getting cached inverse matrix")
        return(inv)
    }
	
    # calculation of the inverse if it is not yet calculated
	data <- x$get()			# gets the matrix from the object
    inv <- solve(data, ...)	# calculates the inverse by matrix multiplication
#    inv <- solve(data)
#    inv <- solve(data) %*% data
    x$setinv(inv) 		    # caches the inverse
    inv						# returns the inverse matrix of 'x'
}

# Example:
# n <- 4
# x <- matrix(rnorm(n^2), nrow = n) // Create a square matrix x
# cx <- makeCacheMatrix(x) // Create our special matrix
# cx$get() // Return the matrix
# cacheSolve(cx) // Return the inverse
# cacheSolve(cx) == solve(x)

# Example:
# x = rbind(c(1, -1/4), c(-1/4, 1))
# m = makeCacheMatrix(x)
# m$get()
# [,1] [,2]
# [1,] 1.00 -0.25
# [2,] -0.25 1.00

# No cache in the first run
# cacheSolve(m)
# [,1] [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667

# Retrieving from the cache in the second run
# cacheSolve(m)
# getting cached data.
# [,1] [,2]
# [1,] 1.0666667 0.2666667
# [2,] 0.2666667 1.0666667
