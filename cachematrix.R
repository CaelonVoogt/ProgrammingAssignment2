# Matrix inversion - ProgrammingAssignment2
# Reducing the costly computation of Inversion by caching caching the result, 
# rather than repeatedly computing the result.

# makeCacheMatrix creates a list containing a function to 
# 1 - Set the value of the matrix 
# 2 - Get the value of the matrix 
# 3 - Set the value of inverse of the matrix 
# 4 - Get the value of inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 
        set <- function(y) { 
                x <<- y 
                inv <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inverse) inv <<- inverse 
        getinverse <- function() inv 
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


# cacheSolve returns the inverse of the matrix. 
# (Checks to see if the matrix has changed:
# If yes, it computes the new result
# If not, it retrieves the cached result

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() 
        if(!is.null(inv)) { message("Retrieving cached result") 
                            return(inv) 
        }
        data <- x$get() 
        inv <- solve(data) 
        x$setinverse(inv) 
        inv 
}