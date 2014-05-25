makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  ## Create a "cache holder" m in Env "makeCacheMatrix"
  set <- function(y) {  ##Create a funtion "set"
    x <<- y
    m <<- NULL
  }
  get <- function() x   ##Create function "get"
  setinverse <- function(inverse) m <<- inverse  ##"setinverse" loads inverse of matrix in "cache"
  getinverse <- function() m  ## retrieves the inverse matrix date in cache "m"
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {  ## Create the funtion to provide the inverse of the cache data in M
  m <- x$getinverse()
  if(!is.null(m)) {  ## if the cache is empty act upon x and place in m, otherwise fine in cache and return
    message("fetching the cached data")
    return(m)
  }
  data <- x$get()  ## Create a "data" to get the new matrix of x and set the inverse in m
  m <- solve(data, ...)
  x$setinverse(m)  ## Resets the cache with the inverse of the new matrix
  m
}

