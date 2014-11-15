## Put comments here that give an overall description of what your
## functions do

## A short comment describing the function makeCacheMatrix
##
## The function makeCacheMatrix gets a matrix as an argument,
## saves it and returns a list containing the following functions
##
## set         - to set the value of the matrix
## get         - to get the value of the matrix
## setinverse  - to set the value of the matrix inverse
## getinverse  - to get the value of the matrix inverse
##
## We have to assume that the matrix supplied is always invertible.
## No further checks will be conducted.

makeCacheMatrix <- function(x = matrix()) {
        
        ## Initialise the inverse as NULL
        i <- NULL
        
        ## This function allows to set/chage the matrix created
        ## by the initialization of makeCacheMatrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        ## This function reads the matrix.
        get <- function() x
        
        ## This function sets the inverse of the matrix in the variable i
        setinverse <- function(inverse) i <<- inverse
        
        ## This function gets the inverse of the matrix that is stored in i
        getinverse <- function() i
        
        ## The function makeCacheMatrix returns the list of the above created functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Short comment describing the function cacheSolve

## The function cacheSolve takes the list created by makeCacheMatrix
## as an argument, computes the inverse of the matrix that is returned
## by its subfunction get (i.e x$get, if x is the created special list).
## If the inverse has already been calculated and the matrix has not changed,
## then the cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        ## Get the inverse stored in the variable i
        i <- x$getinverse()
        
        ## Check if the inverse is not NULL
        if(!is.null(i)) {
                ## The inverse is not NULL, so no calculation needed, just return it
                message("getting the cached inverse")
                return(i)
        }
        ## The inverse is NULL, so the original matrix is getted
        data <- x$get()
        
        ## Calculate the inverse
        i <- solve(data, ...)
        
        ## Save the inverse of the matrix in i for future usage
        x$setinverse(i)
        
        ## Return the inverse of the matrix
        i
}
