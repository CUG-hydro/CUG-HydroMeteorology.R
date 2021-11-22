# #' @export
# `-.array` <- function(x, y) {
#     print(1)
# }

# `-` <- function(e1, e2){
#     UseMethod("-")
# }

# #' @export
# `-.default` <- .Primitive("-")


# # #' @export
# `-.array` <- function(e1, e2) {
#     print(1)
# }

# set_dim <- function(x, dim) {
#     dim(x) <- dim
#     x
# }
#
# subtract <- function(e1, e2) {
#     dim3 = dim(e1)
#     dim2 = dim(e2)
#     dim1 = c(prod(dim2), dim3[3])
#     dim2 = c(prod(dim2), 1)
#
#     # ans = as.matrix(e1) - as.matrix(e2)
#     browser()
#     ans = e1 - as.vector(e2)
#     set_dim(ans)
# }
