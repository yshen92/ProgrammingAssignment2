## Matrix inversion is usually a costly computation
## The inversion is hence cached to reduce repeated computation

## Creates a special matrix object that caches the inverse
makeCacheMatrix <- function(x = matrix()) { #X is an empty mat by default
  mat_inv <- NULL
  set <- function(y){
    x <<- y #Save y to x in the other env
    mat_inv <<- NULL #to clear previously stored mat_inv
  }
  get <- function() x #return input matrix
  setmat_inv <- function(inv) mat_inv <<- inv #store the solved mat_inv
  getmat_inv <- function() mat_inv #return the stored mat_inv
  list(set = set, get = get,
       setmat_inv = setmat_inv,
       getmat_inv = getmat_inv) #list of the functions
}

## Computes inverse of matrix returned by makeCacheMatrix
## If inverse has been calculated, this function retrieves inverse from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inv <- x$getmat_inv() #get mat_inv stored
  if(!is.null(mat_inv)){
    message("Returning cached data...")
    return(mat_inv) #if stored mat_inv is not NULL then return the inverse matrix
  }
  data <- x$get() #if mat_inv is null, get the matrix
  mat_inv <- solve(data, ...) #compute matrix inversion
  x$setmat_inv(mat_inv) #store mat_inv in cache
  mat_inv #Return mat_inv
}
