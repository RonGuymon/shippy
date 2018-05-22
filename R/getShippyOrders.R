#' @title ShipStation Orders Endpoint
#' @description Gets shipStation Orders
#'
#' Documentation: # https://www.shipstation.com/developer-api/#/reference/customers/list-customers/list-customers
#' @param shippyPath Something like: https://api.mywebsite.com/
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param verbose Whether it will return the results of the api call. Defaults to T.
#' @param page Returns a specific page of results. Defaults to 1.

#' @return Returns a list with (1) the number of pages, and (2) a dataframe of orders.
#' @export
#'
getShippyOrders <- function(shippyPath, apiKey, apiSecret, verbose = T, page = NULL){
  shippyApiKey <- paste0(apiKey,":",apiSecret) %>% jsonlite::base64_enc() %>%gsub("[\r\n]", "", .) %>% paste0("BASIC ", .)

  if(is.null(page)){
    apicall <- paste0(shippyPath, "orders?pageSize=500")
  }else {
    apicall <- paste0(shippyPath, "orders?page=", page,"&pageSize=500")
  }

  if(verbose == T){
    r <- GET(apicall
             , add_headers("Authorization" = shippyApiKey
                           , "Content-Type" = "application/json"
             )
             , verbose()
    )
  }else{
    r <- GET(apicall
             , add_headers("Authorization" = shippyApiKey
                           , "Content-Type" = "application/json"
             )
             # , verbose()
    )
  }

  # Number of pages
  pages <- orders$pages
  # Parse the data and return a dataframe
  orders <- httr::content(rOrders, "parsed")
  odf <- data.frame()
  for(i in 1:length(orders)){
    temp <- orders[i][[1]] %>% .[which(!names(.) %in% c("items", "weight", "internationalOptions"))] %>% unlist() # Everything except items
    temp[sapply(temp, is.null)] <- NULL # Remove
    temp %<>%
      t() %>%
      as.data.frame(stringsAsFactors = F)

    # Items in the order
    tempitems <-  orders[i][[1]]$items
    ities <- data.frame()
    if(length(tempitems) > 0){
      for(j in 1:length(tempitems)){
        tempi <- tempitems[j] %>% unlist() %>% .[which(grepl("createDate|modifyDate|shippingAmount", names(.)) == F)]

        # Adjust names of list items that have the same name
        dupeNames <- table(names(tempi)) %>%
          as.data.frame(stringsAsFactors = F) %>%
          dplyr::filter(Freq > 1)
        if(nrow(dupeNames) > 0){
          for(k in 1:nrow(dupeNames)){
            tempiIndices <- which(names(tempi) == dupeNames$Var1[k]) %>% .[-1]
            indNumber <- seq(1, length(tempiIndices), by = 1)
            names(tempi)[tempiIndices] <- paste(names(tempi)[tempiIndices], indNumber, sep = "_")
          }
        }

        tempi[sapply(tempi, is.null)] <- NULL # Remove null entries
        # names(tempi)
        tempi %<>%
          t() %>%
          as.data.frame(stringsAsFactors = F) %>%
          dplyr::mutate(
            orderId = temp$orderId
            , itemNumber = j
          )
        ities %<>% bind_rows(., tempi)
      }
      temp %<>% left_join(., ities, by = "orderId")
    }

    odf %<>% bind_rows(., temp)
    # cat(i, "\n")
  }
  returnList <- list()
  returnList$pages <- pages
  returnList$orders <- odf
  return(returnList)
}
