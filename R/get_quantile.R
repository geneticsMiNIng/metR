
data('quantile.function')

get_quantile <- function(x, n){
  if (n <= 150){
    quantile.function[[n]](x)
  } else
    quantile.function[[150]](x)
}

