## Put comments here that give an overall description of what your
## functions do

## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv = NULL
        set = function(y) { 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function creates the inverse of the “matrix” returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it’ll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv = x$getinv()
        if (!is.null(inv)){ 
                message("getting cached data")
                return(inv)
        }
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
##  To test out these functions:
test = function(mat){
       temp = makeCacheMatrix(mat)
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
        
        start.time = Sys.time()
        cacheSolve(temp)
        dur = Sys.time() - start.time
        print(dur)
}
## Trial on matrix of 1000/1000 columns filled with normal random numbers.
set.seed(666)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)

