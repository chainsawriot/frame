require(irr)
require(tidyverse)

HJ <- rio::import(here::here("coded/Test coding HJ.xlsx")) %>% tibble::as_tibble()

ZO <- rio::import(here::here("coded/Test coding ZO.xlsx")) %>% tibble::as_tibble()

x <- 3
cal_ka <- function(x, HJ, ZO) {
    res <- suppressMessages(bind_cols(HJ[,x], ZO[,x]))
    kripp.alpha(t(res), "ordinal")$value
}

cal_ka2 <- function(x, HJ, ZO) {
    res <- suppressMessages(bind_cols(as.numeric(HJ[,x] != 0), as.numeric(ZO[,x] != 0)))
    kripp.alpha(t(res), "nominal")$value
}




res <- purrr::map_dbl(3:21, cal_ka, HJ = HJ, ZO = ZO)
res2 <- purrr::map_dbl(3:21, cal_ka2, HJ = HJ, ZO = ZO)

tibble(Q= colnames(HJ)[3:21], res, res2)

## A1, A4, A5, B4, C2, C3, C4, D3, E1, E2, E3

gen_data <- function(x, HJ, ZO) {
    res <- suppressMessages(bind_cols(HJ[,x], ZO[,x]))
    colnames(res) <- NULL
    return(res)
}
