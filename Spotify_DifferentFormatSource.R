############################################################
#https://github.com/tiagomendesdantas/Rspotify/tree/master/R
############################################################
##Works with the Spotify.R code stored at the same location 


###O-Auth authorization

spotifyOAuth<-function(app_id,client_id,client_secret){
  spotifyR <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access = "https://accounts.spotify.com/api/token")
  #if (packageVersion('httr') > "0.6.1"){Sys.setenv("HTTR_SERVER_PORT" = "1410/")}
  myapp <- httr::oauth_app(app_id, client_id, client_secret)
  return(httr::oauth2.0_token(spotifyR,myapp,scope = "playlist-read-private"))}


######################################
#######     Albums       #############
######################################

# Get an Album Information

getAlbum<-function(id){
  req<-httr::GET(paste0("https://api.spotify.com/v1/albums/",id,"/tracks"))
  json1<-httr::content(req)
  json2<-jsonlite::fromJSON(jsonlite::toJSON(json1))$items
  return(json2[,c("id","name","duration_ms","track_number","disc_number","available_markets","preview_url")])
}

#Also Refer to Track information in the Album 
##Id Refers to Album id of Kanye West Wacth the Throne Album 
kanye_West <- getAlbum("0OcMap99vLEeGkBCfCwRwS") 

#'Get an Artist's Albums  
#'function to get albums from an artist using their ID

getAlbums<-function(id,type="album",market="US"){
  total<-jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/",id,"/albums??&album_type=album"))$total
  req<-jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/",id,"/albums??offset=0&limit=",total,"&album_type=",type,"&market=",market))
  return(req$items[,c("id","name","album_type","available_markets")])
}


albumsKanyeWest <- getAlbums("5K4W6rqBFWDnAN6FQUkS6x",type="album",market="US")  # Id Refers to Kanye West (Spotify User Id )



################################
#######    ARTIST    ###########
################################

getArtist<-function(id){
  req <- httr::GET(paste0("https://api.spotify.com/v1/artists/",id))
  json1<-httr::content(req)
  dados<-data.frame(id=json1$id,name=json1$name,
                    popularity=json1$popularity,
                    followers=json1$followers$total,
                    genres=paste(json1$genres,collapse =";"))
  return(dados)
}

kANYEwESTaRTIST <- getArtist("5K4W6rqBFWDnAN6FQUkS6x")


#Get an Artist Top Tracks 

getTop<-function(id,country){
  req<-jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/",id,"/top-tracks?country=",country))
  return(req$tracks[,c("id","name","popularity","duration_ms","track_number","available_markets")])
}

getTop("5K4W6rqBFWDnAN6FQUkS6x","US")


#Get Related artists

getRelated <-function(artistName){
  info<-searchArtist(artistName)
  id<-info$id[1]
  name<-info$name[1]
  relatedArtists<-jsonlite::fromJSON(paste0("https://api.spotify.com/v1/artists/",id,"/related-artists"))$artists
  relatedName<-relatedArtists$name
  relatedID<-relatedArtists$id
  followers<-relatedArtists$followers$total
  popularity<-relatedArtists$popularity
  return(data.frame(sourceID=id, sourceName=name, names=relatedName,popularity=popularity,followers=followers, id=relatedID,stringsAsFactors=F))
}

######     Creating a Artist Network    ##########
#################################################

#function to search for a specifc artist
#note that if you don't provide a specifc name the function will return possible cases that match
#sorted by popularity
searchArtist<-function(artistName){
  req<-jsonlite::fromJSON(paste0("https://api.spotify.com/v1/search?q=", gsub(' ', '+', artistName),"&type=artist"))
  artist<-req$artists$items[,c("id","name","popularity","genres","type")]
  artist$followers<-as.numeric(req$artists$items$followers$total)
  return(artist)
}

kanye <- getRelated("Kanye West")
newdataartists1 <- kanye$names
a1 <- getRelated(newdataartists1[1])
a2 <- getRelated(newdataartists1[2])
a3 <- getRelated(newdataartists1[3])
a4<- getRelated(newdataartists1[4])
a5 <- getRelated(newdataartists1[5])
a6 <- getRelated(newdataartists1[6])
a7 <- getRelated(newdataartists1[7])
a8 <- getRelated(newdataartists1[8])
a9 <- getRelated(newdataartists1[9])
a10 <- getRelated(newdataartists1[10])
a11 <- getRelated(newdataartists1[11])


Akon <- getRelated("Akon")
newdataartists2 <- Akon$names
b1 <- getRelated(newdataartists2[1])
b2 <- getRelated(newdataartists2[2])
b3 <- getRelated(newdataartists2[3])
b4<- getRelated(newdataartists2[4])
b5 <- getRelated(newdataartists2[5])
b6 <- getRelated(newdataartists2[6])
b7 <- getRelated(newdataartists2[7])
b8 <- getRelated(newdataartists2[8])
b9 <- getRelated(newdataartists2[9])
b10 <- getRelated(newdataartists2[10])
b11 <- getRelated(newdataartists2[11])

Artist_Network1 <- rbind(a1,a2,a3,a5,a6,a7,a8,a9,a11,b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11)

df_list <- lapply(newdataartists1,getRelated)
masterdata <- rbind(df_list[[1]],df_list[[2]],df_list[[3]],df_list[[4]],df_list[[5]],df_list[[6]])
df_list1 <- lapply(newdataartists1,getRelated)


################################################
##########        TRACKS          ##############
################################################

getFeatures<-function(spotify_ID,token){
  req <- httr::GET(paste0("https://api.spotify.com/v1/audio-features/",spotify_ID), add_headers(Authorization=paste('Bearer',access_token)))
  json1<-httr::content(req)
  dados=data.frame(id=json1$id,
                   danceability=json1$danceability,
                   energy=json1$energy,
                   key=json1$key,
                   loudness=json1$loudness,
                   mode=json1$mode,
                   speechiness=json1$speechiness,
                   acousticness=json1$acousticness,
                   instrumentalness=json1$instrumentalness,
                   liveness=json1$liveness,
                   valence=json1$valence,
                   tempo=json1$tempo,
                   duration_ms=json1$duration_ms,
                   time_signature=json1$time_signature,
                   uri=json1$uri,
                   analysis_url=json1$analysis_url,stringsAsFactors = F)
  return(dados)
}


#Get the top songs of Kanye West and the Audio Features Information 
kanye_Top10Songs <- getTop("5K4W6rqBFWDnAN6FQUkS6x","US")
kanyeTrackIds <- kanye_Top10Songs$id
#kanyeTrackIds <- as.list(kanyeTrackIds)



KanyeFatherStretch <- getFeatures("4KW1lqgSr8TKrvBII0Brf8")
aa <- getFeatures("19a3JfW8BQwqHWUMbcqSx8")
aa1 <- getFeatures("6C7RJEIUDqKkJRZVWdkfkH")
aa2 <- getFeatures("3nAq2hCr1oWsIU54tS98pL")
aa3 <- getFeatures("2Zb7wnGUnNPCas2E0wWSQ5")
aa4 <- getFeatures("3cCxoOgfi6hgt8MNteuiiD")
aa5 <- getFeatures("2nBI3iWLhupR7LyAJ5GGkE")
aa6 <- getFeatures("78TTtXnFQPzwqlbtbwqN0y")
aa7 <- getFeatures("1eQBEelI2NCy7AUTerX0KS")
aa8 <- getFeatures("2gZUPNdnz5Y45eiGxpHGSc")


trackfeatures <- rbind(KanyeFatherStretch,aa,aa1,aa2,aa3,aa4,aa5,aa6,aa7,aa8)
trackfeatures <- cbind(kanye_Top10Songs$name,trackfeatures)
trackfeatures <- trackfeatures[,1:14]
colnames(trackfeatures)[colnames(trackfeatures)=="kanye_Top10Songs$name"] <- "songname"


plot(trackfeatures$danceability, trackfeatures$energy, main="Scatterplot of audio featues",xlab="danceability", ylab="energy", pch=19)
textxy(trackfeatures$danceability, trackfeatures$energy, trackfeatures$songname,cex=0.7,cex.axis=0.5)

#Require library(scatterplot3d)
scatterplot3d(trackfeatures$danceability, trackfeatures$energy,trackfeatures$key, main="3D Scatterplot of Audio Features")
textxy(trackfeatures$danceability, trackfeatures$energy, trackfeatures$key,trackfeatures$songname)


######################################################################


######################    Audit Data for Xbox Gaming Adverts ##################
#''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''

gaming1 <- getFeatures("4dDZyGxClFiOBUjcpxhXWg")
gaming2 <- getFeatures("6fpCDu6Hg9lV1tcscnIy1r")
gaming3 <- getFeatures("2P2HvdhIRheHI2BrLC26KM")
gaming4 <- getFeatures("0ZfM5XfJTtFPhOxAERRnNY")
gaming5 <- getFeatures("6yr8GiTHWvFfi4o6Q5ebdT")
gaming6 <- getFeatures("3VtXM5yXDsNx7p1uWhNoJU")
gaming7 <- getFeatures("6RcByPoFlaUVAn2PnTxSVr")
gaming8 <- getFeatures("5KzRfDOUpMVaB4eYugEVjE")
gaming9 <- getFeatures("2swgzIaJCqlYmiYeV8beOU")
gaming10 <- getFeatures("6cTLMb7FncAoQNMUZPFrCT")
#gaming11 <- getFeatures("3lzodW8PPEWaAqQDDshcIG")
gaming12 <- getFeatures("753KutoAy00apPsplMRetG")
gaming13 <- getFeatures("5DTOOkooKFUvWj1XQTFa09")

gamingTracks <- rbind(gaming1,gaming2,gaming3,gaming4,gaming5,gaming6,gaming7,gaming8,gaming9,gaming10,gaming12,gaming13)

songname <- c("Prototype 2 for Xbox PS3 Game Commercial","Saints Row: The Third - 2011 Commercial","Grand Theft Auto V official game trailer
","Grand Theft Auto: Episodes from Liberty City","Call of Duty: Modern Warfare 2 for XBOX 360","Left For Dead 2 Game For XBOX 360 Commercial
","Sega Bayonetta Game XBOX and PS3 Commercial","Call Of Duty: Black Ops XBOX 360 Won't Back Down","Need For Speed: Hot Pursuit PS3 & XBOX360 Racing Game
","NBA2K12 Video Game The Great Debate Commercial","Rock Band 2 Commercial for XBOX 360","Tom Clancy's EndWar Video Game Commercial
")

gamingTracksdata <- cbind(songname,gamingTracks)
#write.csv(gamingTracksdata,"gamingTracksdataAudit.csv")


plot(trackfeatures$danceability, trackfeatures$energy, main="Scatterplot of audio featues",xlab="danceability", ylab="energy", pch=19)
textxy(trackfeatures$danceability, trackfeatures$energy, trackfeatures$songname,cex=0.7,cex.axis=0.5)

