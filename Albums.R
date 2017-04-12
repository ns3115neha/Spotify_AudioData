#' Get an Album
#' Get Spotify catalog information for a single album
#' @param id Required. The Spotify ID for the album



get_album <- function(id,...){
  search <- GET(url = paste(albums_url,id,sep=''),query=list(...))
  get_response_content(search)
  
}

rihanaantialbum <- get_album("3Q149ZH46Z0f3oDR7vlDYV")

#' Get Several Albums
#' Get Spotify catalog information for multiple albums identified by their Spotify IDs.
#' @param ids Required. A comma-separated list of the Spotify IDs for the albums. Maximum: 20 IDs.



get_albums <- function(ids,...){
  query <- list(ids=paste(ids,collapse=','),...)
  search <- GET(url = albums_url,query=query)
  get_response_content(search)
}

#get_albums('3Q149ZH46Z0f3oDR7vlDYV') #for this Artist Id ###########################################


#' Get an Album's Tracks
#' Get Spotify catalog information about an album's tracks.
#' Optional parameters can be used to limit the number of tracks returned.
#' @param id Required. The Spotify ID for the album


get_album_tracks <- function(id,...){
  search <- GET(url = paste(albums_url,id,'/','tracks',sep=''),query=list(...))
  get_response_content(search)
}


rihanaalbumtracks <- get_album_tracks("3Q149ZH46Z0f3oDR7vlDYV")

