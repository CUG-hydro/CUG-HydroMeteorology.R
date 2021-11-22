get_anomaly <- function(arr, ref_period = 1:30) {
    u_ref <- apply_3d(arr[, , ref_period])
    arr - as.vector(u_ref) # anomaly
}
