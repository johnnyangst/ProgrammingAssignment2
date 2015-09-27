## Programming Assignment Week 2
## Course rprog-032
## by John Kirker
## The purpose of this these functions is to
## 1) create a workable matrix that can be cahed to memory 
## 2) return the inverse of the cached matrix

## the makeCacheMatrix function will take a matrix and cache it to memory
makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y)
    {
        
        x <<- y
        m <<- NULL
    }
    getMatrix <- function() x
    setMatrixInv <- function(solve) m <<- solve
    getMatrixInv <- function() m
    list(set=set, get=get, setMatrixInv=setMatrixInv,getMatrixInv=getMatrixInv)
}


## the cacheSolve function will take the cached matrix and invert it
cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getMatrixInv()
    ## has inverse been calculated? look for cached matrix
    if (!is.null(m))
    {
        message("retrieving matrix")
        return(m)
    }
    ## calc inverse
    cachedMatrix <- x$getMatrix()
    m <- solve(cachedMatrix, ...)
    x$setMatrixInv(m)
    return(m)
}
