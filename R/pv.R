pv <- function(FV, r, n = 5) {

  if(!is.atomic(FV)) {
    stop('FV must be an atomic vector')
  }

  if(!is.numeric(FV) | !is.numeric(r) | !is.numeric(n)) {
    stop('This function only works for numeric inputs!\n',
         'You have provided objects of the following classes:\n',
         'FV: ', class(FV), '\n',
         'r: ', class(r), '\n',
         'n: ', class(n))
  }

  # if(r < 0 | r > .25) {
  #   message('The input for r exceeds the normal\n',
  #           'range for interest rates (0-25%)')
  # }

  present_value <- FV / (1 + r)^n
  round(present_value, 2)
}
