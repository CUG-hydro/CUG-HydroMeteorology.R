# #' @export 
# `-.array` <- function(x, y) {
#     print(1)
# }

`-` <- function(e1, e2){
    UseMethod("-")
}

`-.array.numeric` <- function(e1, e2) {
    print(1)
}
