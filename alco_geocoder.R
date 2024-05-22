require(httr)
require(jsonlite)
require(utils)
require(plyr)
require(tidyverse)

countyGeo <- function(locs, endpoint = "verification") {
  # Build Empty Data frace
  final <- NULL
  # Run for each address
  for (address in locs) {
    # Encode address for API call
    address_encode <- URLencode(address, reserved = TRUE, repeated = TRUE)
    # Build URL for geocode
    if (endpoint == "verification") {
      url <- paste0("https://gisdata.alleghenycounty.us/arcgis/rest/services/Geocoders/AddressVerification/GeocodeServer/findAddressCandidates?SingleLine=", address_encode, '&outSR=4326&outFields=*&f=pjson')
    } else if (endpoint == "search") {
      url <- paste0("https://gisdata.alleghenycounty.us/arcgis/rest/services/Geocoders/AddressSearch/GeocodeServer/findAddressCandidates?SingleLine=", address_encode, '&outSR=4326&outFields=*&f=pjson')
    } else {
      stop('Please provide a valid value for the endpoint argument of either `search` or `verification`')
    }
    # Print message for user
    message(paste("Source :", url))
    # Send geet request
    r <- httr::GET(url)
    # Load Content
    c <- jsonlite::fromJSON(httr::content(r))
    
    # Check status code & number of candidates
    if (r$status_code == 200 & length(jsonlite::fromJSON(httr::content(r))$candidates) > 0) {
      # Successful add top candidate
      longitude <- ifelse(c$candidates$location[1,1] == 'NaN', NA, c$candidates$location[1,1])
      latitude <- ifelse(c$candidates$location[1,2] == 'NaN', NA, c$candidates$location[1,2])
      muni <- c$candidates$attributes$City[1]
      parcel <- c$candidates$attributes$PARCELID[1]
      school_dist <- c$candidates$attributes$SCHOOLDISTRICT[1]
      match_address <- c$candidates$attributes$Place_addr[1]
      score <- c$candidates$score[1]
    } else if (r$status_code == 200) {
      # Error if not address candidates
      message("    Failed with error (200): No address candidates")
      longitude <- NA
      latitude <- NA
      muni <- NA
      parcel <- NA
      school_dist <- NA
      match_address <- NA
      score <- NA
    } else {
      # Print other error
      message(paste0("    Failed with error (", c$error$code, "): ", c$error$message))
      longitude <- NA
      latitude <- NA
      muni <- NA
      parcel <- NA
      school_dist <- NA
      match_address <- NA
      score <- NA
    }
    # Build columns for bind
    df <- data.frame(longitude, latitude, muni, parcel, school_dist, match_address)
    
    # Build Dataframe for results
    if (is.null(final)) {
      final <- df
    } else {
      # Merge to results dataframe
      final <- plyr::rbind.fill(final, df)
    }
  }
  
  return(final)
}

mutate_countyGeo <- function(data, location, ...){
  # Get data location
  locs <- data[[deparse(substitute(location))]]
  # Run geocde script
  gcdf <- countyGeo(locs, ...)
  # Bind to x
  dplyr::bind_cols(data, gcdf)
}
