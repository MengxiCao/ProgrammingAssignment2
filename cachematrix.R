## 

## The makeCacheMatrix creates a special matrix object

makeCacheMatrix <- function(x = matrix()) {
    elc <- NULL
    set <- function(y){
        x <<- y
        elc <<- NULL
    }
    get <- function() x
    setreverse <- function(reverse) elc <<- reverse
    getreverse <- function() els
    list(set = set, get = get, setreverse = setreverse, getreverse = getreverse)

}


## The CacheSolve calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    elc <- x$getreverse()
    if(!is.null(elc)){
        message("getting cached reverse matrix")
        return(elc)
    }else{
        elc <- solve(x$get())
        x$setreverse(elc)
        return(elc)
    }
}
