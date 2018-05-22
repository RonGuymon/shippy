#' @title ShipStation Orders Endpoint
#' @description Gets shipStation Orders
#'
#' Documentation: https://help.shopify.com/api/reference/inventory/inventoryitem
#' @param shopifyPath Something like: mywebsite.com
#' @param apiKey Unencoded api key
#' @param apiPassword Unencoded apiPassword
#' @param verbose Whether it will return the results of the api call. Defaults to T.
#' @param page Returns a specific page of results. Defaults to 1.

#' @return Returns a dataframe of inventory items. May contain nested dataframes.
#' @export
#'
getShippyOrders <- function(shippyPath, apiKey, apiPassword, verbose = T, page = NULL){
  shopifyApiKey <- paste0(apiKey,":",apiPassword) %>% jsonlite::base64_enc() %>% gsub("[\r\n]", "", .) %>% paste0("Basic ", .)

  if(is.null(page)){
    apicall <- paste0("https://", shopifyPath, "/admin/inventory_items.json?limit=250")
  }else {
    apicall <- paste0("https://", shopifyPath, "/admin/inventory_items.json?limit=250&page=", page)
  }

  if(verbose == T){
    r <- GET(apicall,
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
             ,verbose()
    )
  }else{
    r <- GET(apicall,
             add_headers("Authorization" = shopifyApiKey
                         , "Content-Type" = "application/json"
             )
             # ,verbose()
    )
  }

  # Parse the data and return a dataframe
  df <- content(r, "text") %>% jsonlite::fromJSON() %>% as.data.frame()
  return(df)
}
