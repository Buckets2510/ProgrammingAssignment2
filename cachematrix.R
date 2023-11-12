## The assignment is to create a pair of functions that calculate the 
## inverse of a matrix


## The first function will create a matrix named 'x' as an argument

makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        
        set <- function(y) {
                ## assign the value of x to y - stored in the parent environment
                x <<- y
                
                ## assign invMatrix to NULL - stored in the parent environment
                invMatrix <<- NULL
        }
        ## return matrix x
        get <- function() x
        
        ## set the values of the inverse matrix to the inverse variable and
        ## return the cached value of the inversed matrix
        setInverse <- function(inverse) invMatrix <<- inverse
        getInverse <- function() invMatrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The second function will solve the inverse of the matrix from makeCacheMatrix
## The function will also take additional arguments

cacheSolve <- function(x, ...) {
        ## Assign the value of the cached matrix from makeCacheMatrix to the
        ## inverse variable
        invMatrix <- x$getinverse()
        
        ## If the inverse was calculated already, return the matrix inverse
        if(!is.null(invMatrix)) {
                message("Getting cached data")
                return(invMatrix)
        }
        
        ## Assign the value of x to a new matrix to calculate the inverse, if it
        ## was not cached
        data <- x$get()
        
        ## Set the value of the inverse matrix to the invMatrix variable
        invMatrix <- solve(data)
        
        ## Cache the matrix and print the inverse
        x$setInverse(invMatrix)
        invMatrix

}
