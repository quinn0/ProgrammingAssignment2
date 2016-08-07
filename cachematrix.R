#makeCacheMatrix was made using the example as a template 
#as you can see the outer most function deals with the lexical
#scoping we need to access/re-assign inv which becomes the 
#output of the function

## Using the same set/get style as example:
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #setting initial 'unchached' matrix
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv <-solve
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Here if the matrix (not actually a matrix yet in null state) is not NULL then 
## we know there is an inverse cached  otherwise we set the value of the solved
##matrix from makeCacheMatrix and return inv

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)){ 
                print("Getting cached data")
                return(inv)
        }else
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
##Stay hydrated and enjoy your day!
