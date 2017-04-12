base_url <- 'https://api.spotify.com'
authorize_url <- 'https://accounts.spotify.com/authorize/'
search_url <- paste(base_url,'/v1/search',sep='')
albums_url <- paste(base_url,'/v1/albums/',sep='')
artists_url <- paste(base_url,'/v1/artists/',sep='')
browse_featured_playlists_url<- paste(base_url,'/v1/browse/featured-playlists/',sep='')
browse_new_releases_url<- paste(base_url,'/v1/browse/new-releases/',sep='')
browse_categories_url <- paste(base_url,'/v1/browse/categories/',sep='')
following_url <- 'https://api.spotify.com/v1/me/following'
library_url <- 'https://api.spotify.com/v1/me/tracks'
user_url <- 'https://api.spotify.com/v1/users'

#' Get a List of Featured Playlists
#' Get a list of Spotify featured playlists (shown, for example, on a Spotify player's "Browse" tab).


get_featured_playlists <- function(...){
  
  response <- GET(url = browse_featured_playlists_url,
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',mytoken)))
  get_response_content(response)
}

##Oauth Authentication Needs to be resolved 


#' Get a List of New Releases
#' Get a list of new album releases featured in Spotify (shown, for example, on a Spotify player's "Browse" tab).
#' @param country Required if you want any results.
#' A country: an ISO 3166-1 alpha-2 country code.
#' Provide this parameter if you want the list of
#' returned items to be relevant to a particular country.

get_new_releases <- function(country,...){
  
  response <- GET(url = browse_new_releases_url,
                  query=list(country=country,...),
                  add_headers(Authorization=paste('Bearer',mytoken)))
  get_response_content(response)
}

##Oauth Authentication Needs to be resolved 



#' Get a List of Categories
#' Get a list of categories used to tag items in Spotify (on, for example, the Spotify player's "Browse" tab).
#'
#' For more information: https://developer.spotify.com/web-api/get-list-categories/
get_categories <- function(...){
  
  response <- GET(url = browse_categories_url,
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',access_token)))
  get_response_content(response)
}

##Oauth Authentication Needs to be resolved 


#' Get a Category
#' Get a single category used to tag items in Spotify (on, for example, the Spotify player's "Browse" tab).


get_category <- function(category_id,...){
  
  response <- GET(url = paste(browse_categories_url,category_id,sep=''),
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',access_token)))
  get_response_content(response)
}

#' Get a Category's Playlists
#' Get a list of Spotify playlists tagged with a particular category.


##Oauth Authentication Needs to be resolved 

get_category_playlists <- function(category_id,...){
  
  response <- GET(url = paste(browse_categories_url,category_id,'/playlists',sep=''),
                  query=list(...),
                  add_headers(Authorization=paste('Bearer',access_token)))
  get_response_content(response)
}


