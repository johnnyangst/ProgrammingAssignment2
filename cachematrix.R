## Programming Assignment Week 2
## Course rprog-032
## by John Kirker
## The purpose of this these functions is to
## 1) create a workable matrix that can be cahed to memory 
## 2) return the inverse of the cached matrix

## the makeCacheMatrix function will take a matrix and cache it
makeCacheMatrix <- function(x = matrix()) 
{
    ## initialize inverse
    inv <- NULL
    ## getters and setters
    set <- function(y)
    {
        ## assign values external to current environment
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setMatrixInv <- function(inverse) inv <<- inverse
    getMatrixInv <- function() inv
    list(set=set, get=get, setMatrixInv=setMatrixInv,getMatrixInv=getMatrixInv)
}


## the cacheSolve function will look for cached inverse and print it.  If it's not found,
## it will take the cached matrix, invert it and set the cached value to the inverse.
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getMatrixInv()
    ## has inverse been calculated? look for cached matrix
    if (!is.null(inv))
    {
        message("retrieving cached matrix")
        return(inv)
    }
    ## if no cached solution, calculate inverse
    message("calculating inverse")
    data <- x$get()
    inv <- solve(data, ...)
    x$setMatrixInv(inv)
    inv
}
