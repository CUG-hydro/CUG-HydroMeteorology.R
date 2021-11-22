{
    x = array(1, dim = c(2, 2, 2))
    y = array(0.5, dim = c(2, 2, 1))
    isGeneric("-")

    # class(x) <- c("foo", "array")[c(2)]
    x - y
}

methods("-")
methods(class = "array")
subtract(x, y)
