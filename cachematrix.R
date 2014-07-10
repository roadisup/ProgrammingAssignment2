##### makeCacheMatrix ########################
# This is a function that will create a      #
# special list, that will allow R to         #
# get/set the matrix and get/set the cached  #
# mean                                       #
#                                            #
##### ARGUMENTS: #############################
# storedMatrix: The initial matrix to store  #
#                                            #
##### RETURNS: ###############################
# A list with the defined functions          #
#                                            #
##############################################

makeCacheMatrix <- function(storedMatrix = matrix()) {
	# Initialise the cache. Don't calculate the inverse until it's needed
	cachedInverse <- NULL
	# This function can be used to change the stored matrix
	setMatrix <- function(y) {
                storedMatrix <<- y # Store the new matrix
                cachedInverse <<- NULL # Our old cache is now invalid
        }
	# This function can be used to retrieve the stored matrix
	getMatrix <- function()
		storedMatrix
	# This function can be used to store a value in the cache
	setInverse <- function(inverse)
		cachedInverse <<- inverse
	# This function can be used to retrieve the cached inverse
	getInverse <- function()
		cachedInverse
	# Finally, return a list with all of the functions that have just been defined.
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}

##### cacheSolve #############################
# This is a function that will return the    #
# inverse of a matrix. It will cache this    #
# result for future reference in case the    #
# inverse is needed again.                   #
#                                            #
##### ARGUMENTS: #############################
# cachedMatrix: The cachedMatrix object      #
# from the makeCacheMatrix function to act   #
# upon                                       #
#                                            #
##### RETURNS: ###############################
# If this is the first time it is run, the   #
# inverse will be found and this value will  #
# be stored in the cache and returned. Upon  #
# subsequent calls to the same cacheMatrix   #
# object, the calculation step will be       #
# skipped and the cached inverse will be     #
# returned.                                  #
#                                            #
##############################################

cacheSolve <- function(cachedMatrix, ...) {
	# Fetch cached inverse from cachedMatrix
        inverse <- cachedMatrix$getInverse()
	# If this exists, return it and finish.
	if(!is.null(inverse))
		return(inverse)
	# If not, actually solve for the inverse
	storedMatrix <- cachedMatrix$getMatrix()
	inverse <- solve(storedMatrix, ...)
	# Store this in the cache
        cachedMatrix$setInverse(inverse)
	# And return the inverse
        inverse
}
