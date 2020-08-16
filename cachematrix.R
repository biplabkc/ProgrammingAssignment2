## The function will return the inverse of the matrix and cache it. If the 
## subsequent input is same matrix then it return the previous cache of 
## inverse matrix.

## 'makeCacheMatrix' takes invertible square matrix as input and returns
## list of functions while, for the new input, clears the previous cache 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(a){
                x <<- a
                inv <<- NULL
        }
        
        get <- function () 
        {
                x
        }
        set_inverse <- function (inverse)
        {
                inv <<- inverse
        }
        
        get_inverse <- function ()
        {
                inv
        }
        
        list (set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}



## 'cacheSolve' function actually compute the inverse of the matrix but it
## takes object from  makeCacheMatrix.R as input while checking the condition for
## either returning cached value or computing again for new input.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## 'x' is not the same argument of that of 'makeCacheMatrix' function.
        inv <- x$get_inverse()
        if (!is.null(inv))
        {
                message ("getting cached inverse matrix")
                return (inv)
        }
        
        matrix <- x$get()
        inv <- solve(matrix,...)
        x$set_inverse(inv)
        inv
}
