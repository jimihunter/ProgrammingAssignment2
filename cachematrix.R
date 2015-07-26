## Put comments here that give an overall description of what your
## functions do

## The functio computes and cache an inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {

    set <- function(matrixObj) {
        x <<- matrixObj
        invMatrix <<- NULL
    }
    get <- function() {
        x
    }
    setMatrixInv <- function(x)  {
        
        invMatrix <<- solve(get())
        
    }
    getMatrixInv <- function() {
        
        invMatrix 
        
    }
    list(set=set, get=get, setMatrixInv=setMatrixInv, getMatrixInv = getMatrixInv)
}

    


## This function returns a matrix inverse from memory

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getMatrixInv()
    
    cols = ncol(invMatrix)
    
    rows = nrow(invMatrix)
    
    identMatrix <- diag(nrow=rows, ncol=cols)
    
    inverseTest <- identical(identMatrix, zapsmall(invMatrix %*% x$get())) 
    
    if(inverseTest) {
        
        message("Getting cached matrix inverse")
        return(invMatrix)
    }
    data <- x$get()
    
    
    calInv <- solve(data)
    
    x$setMatrixInv(calInv)
    
    calInv
}
