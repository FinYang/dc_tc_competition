trp_weight <- function(res, pre){
  df <- data.frame(res,pre)
  df <- dplyr::arrange(df, desc(pre))
  res <- df$res
  pre <- df$pre
  PosAll <- sum(res)
  NegAll <- sum(!res)
  pCumsum <- cumsum(res)
  nCumsum <- cumsum(!res)
  pCumsumPer <- pCumsum / PosAll
  nCumsumPer <- nCumsum / NegAll
  TR1 = pCumsumPer[which.min(abs(nCumsumPer-0.001))]
  TR2 = pCumsumPer[which.min(abs(nCumsumPer-0.005))]
  TR3 = pCumsumPer[which.min(abs(nCumsumPer-0.01))]
  return(0.4 * TR1 + 0.3 * TR2 + 0.3 * TR3)
}

