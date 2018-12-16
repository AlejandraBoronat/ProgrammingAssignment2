## These two functions are part of the second assignment on R Programing Coursera class. 

## Description of the assignment: Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than computing it repeatedly (there are also alternatives to matrix inversion 
## that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## The first part, makeCacheMatrix, creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverted_matrix <- function(solve) m <<- solve 
        getinverted_matrix <- function() m
        list(set = set, get = get,
             setinverted_matrix = setinverted_matrix,
             getinverted_matrix = getinverted_matrix)
}

## The second part, cacheSolve, computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverted_matrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        matrix_data <- x$get()
        m <- solve(matrix_data, ...)
        x$setinverted_matrix(m)
        print(m)
        }

