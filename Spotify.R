#' Get an Artist's Top Tracks
#' Get Spotify catalog information about an artist's top tracks by country.
#' filter and sort the response.
#' @param id Required. The Spotify ID for the artist
#'
#' For more information: https://developer.spotify.com/web-api/get-artists-top-tracks/


#https://github.com/rweyant/spotifyr/tree/master/R

##Usuage Of data for Insights

#https://insights.spotify.com/us/2016/01/22/meet-the-beatles-data/

#########################
#####Authentication######
########################

install.packages("httr")
library(httr)

clientID = 'acf731b71d7f444c91a253b7c0b64cf5'
secret = '1d43573f871642e2b0f452eb8861e037'
response = POST('https://accounts.spotify.com/api/token',accept_json(),authenticate(clientID, secret),body = list(grant_type = 'client_credentials'),encode = 'form',verbose(),httr::config(http_version = 2))
access_token = content(response)$access_token


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
tracks_url <-paste("https://api.spotify.com/v1/audio-features/",sep='')



all_scopes <- paste('playlist-read-private playlist-read-collaborative playlist-modify-public playlist-modify-private',
                    'streaming user-follow-modify user-follow-read user-library-modify user-library-read user-read-private',
                    'user-read-birthdate user-read-email')


###########################################Functions######################################################
###Miscellanious Function 
##https://github.com/rweyant/spotifyr

get_response_content <- function(response){
  
  if(!(status_code(response) %in% c(200,201,204)))
    stop(paste('\nError Code: ', content(response)$error$status,'\n',content(response)$error$message))
  
  # Otherwise, return content
  content(response)
}

###Function to get the the top tracks for Artists
get_artist_toptracks <- function(id,country){
  search <- GET(url = paste(artists_url,id,'/top-tracks',sep=''),
                query=list(country=country))
  get_response_content(search)
}


top_tracks <- c(get_artist_toptracks('1IQ2e1buppatiN1bxUVkrk','US'),get_artist_toptracks('1Mxqyy3pSjf8kZZL4QVxS0','US'))


#' Get an Artist's Related Artists
#' Get Spotify catalog information about artists similar to a given artist.
#' Similarity is based on analysis of the Spotify community's listening history.
#' @param id Required. The Spotify ID for the artist
#'
#' For more information: https://developer.spotify.com/web-api/get-related-artists/
get_artist_relatedartists <- function(id,country){
  search <- GET(url = paste(artists_url,id,'/related-artists',sep=''))
  get_response_content(search)
}

relatedArtistis <- get_artist_relatedartists('1IQ2e1buppatiN1bxUVkrk','US')



#Check if an artist is being followed 
#Check if Slayer is being followed. 
#Use Spotify Artist ID

following <- function(ids,type=c('artist','user'),...){
  
  type <- match.arg(type)
  
  response <- GET(url = paste(following_url,'/contains',sep=''),
                  query=list(type=type,ids=ids),
                  add_headers(Authorization=paste('Bearer',mytoken)))
  
  get_response_content(response)[[1]]
}



following('1IQ2e1buppatiN1bxUVkrk',type='artist')

##########################################################Solve for Oauth Code Validation 

get_albums <- function(ids,...){
  query <- list(ids=paste(ids,collapse=','),...)
  search <- GET(url = albums_url,query=query)
  get_response_content(search)
}



get_user_profile <- function(user_id){
  response <- GET(url = paste(user_url,'/',user_id,sep=''))
  get_response_content(response)
}


get_current_user_profile <- function(){
  response <- GET(url = paste(base_url,'/v1/me/',sep=''),
                  add_headers(Authorization=paste('Bearer',mytoken)))
  get_response_content(response)
}



##################
search <- function(q,type,...){
  
  response <- GET(url = search_url,
                  query=list(q=q,type=type,...))
  get_response_content(response)
}
search('slayer',type='artist')
search('South Of Heaven',type='album')
search("Johnny Cash",type = 'artist')

##Converting list into data frame format 
b2 <- as.data.frame(lapply(martin$items, function(x) c(id=x['id'], name=x['name'])))




############



