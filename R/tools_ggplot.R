# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
# Function factory for secondary axis transforms
#' @importFrom scales rescale
#' @export
train_sec <- function(primary, secondary, na.rm = TRUE, to = NULL) {
    # Thanks Henry Holm for including the na.rm argument!
    from <- range(secondary, na.rm = na.rm)
    if (is.null(to)) to <- range(primary, na.rm = na.rm)
    # Forward transform for the data
    forward <- function(x) {
        rescale(x, from = from, to = to)
    }
    # Reverse transform for the secondary axis
    reverse <- function(x) {
        rescale(x, from = to, to = from)
    }
    list(fwd = forward, rev = reverse)
}
