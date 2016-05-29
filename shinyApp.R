# ui.R

library(shiny)
shinyUI(fluidPage(
  titlePanel("Audio features for album tracks"),
  sidebarLayout(
    sidebarPanel(
      helpText("Track and Album IDs can be found in a Spotify URL"),
      helpText("ex: https://play.spotify.com/album/",tags$b("4jHuw0FsxTYTS3TFOmnYyD")),
      helpText("Output may take a few seconds."),
      textInput("albumId", "Track or Album ID", value = "", width = NULL, placeholder = NULL),
      radioButtons("type", "", c("Album", "Single Track"), selected = "Album", inline = TRUE),
      submitButton(text = "Submit", icon = NULL, width = NULL)
    ),
    mainPanel(
      h2(textOutput("name")),
      tableOutput("result")
    )
  )
))

########

# server.R

spotifyKey <- "your_key"
spotifySecret <- "your_secret"

library("httr")
library("jsonlite")
spotifyEndpoint <- oauth_endpoint(NULL, 
                                  "https://accounts.spotify.com/authorize", 
                                  "https://accounts.spotify.com/api/token")
spotifyApp <- oauth_app("spotify", spotifyKey, spotifySecret)
spotifyToken <- oauth2.0_token(spotifyEndpoint, spotifyApp)

library(shiny)
keys <- c("C", "C sharp / D flat", "D", "D sharp / E flat","E", "F", 
          "F sharp / G flat", "G", "G sharp / A flat", "A", "A sharp / B flat", "B")
trackMode <- c("Minor", "Major")

shinyServer(function(input, output) {
  output$name <- renderText({
    if(input$albumId == "") {
      
    } else if(input$type == "Album") {
      
      spotifyAlbum <- input$albumId
      albumURL <- paste("https://api.spotify.com/v1/albums/",spotifyAlbum,sep="")
      getAlbum <- GET(albumURL, spotifyToken)
      albumContent <- jsonlite::fromJSON(toJSON(content(getAlbum)))
      return(paste(albumContent$artists$name[[1]],albumContent$name, sep=" - "))
      
    } else if(input$type == "Single Track") {
      
      trackID <- input$albumId
      trackURL <- paste("https://api.spotify.com/v1/tracks/",trackID, sep="")
      getTrack <- GET(trackURL, spotifyToken)
      singleTrack <- jsonlite::fromJSON(toJSON(content(getTrack)))
      return(paste(singleTrack$artists$name[[1]], singleTrack$name, sep=" - "))
      
    }
  })
  
  output$result <- renderTable({
    if(input$albumId == "") {
    } else if(input$type == "Album") {
        spotifyAlbum <- input$albumId
        albumTracksURL <- paste("https://api.spotify.com/v1/albums/",
                                spotifyAlbum,"/tracks?limit=50",sep="")
        getTracks <- GET(albumTracksURL, spotifyToken)
        albumTracks <- jsonlite::fromJSON(toJSON(content(getTracks)))
        ids <- data.frame(matrix(unlist(albumTracks$items$id), nrow=albumTracks$total, byrow=T),stringsAsFactors=FALSE)
        names <- data.frame(matrix(unlist(albumTracks$items$name), nrow=albumTracks$total, byrow=T),stringsAsFactors=FALSE)
        result <- cbind(ids, names)
        for(i in 1:length(result[,1])) {
          audioFeaturesURL <- paste("https://api.spotify.com/v1/audio-features/", 
                                    result[i,1], 
                                    sep="")
          getaudioFeatures <- GET(audioFeaturesURL, spotifyToken)
          audioFeatures <- jsonlite::fromJSON(toJSON(content(getaudioFeatures)))
          result[i,3] <- paste(keys[audioFeatures$key + 1],trackMode[audioFeatures$mode + 1], sep=" - ")
          result[i,4] <- paste(audioFeatures$time_signature, "/4", sep="")
        }
        names(result) <- c("Track ID", "Track Name", "Key & Mode", "Time Signature")
        return(result[,2:4])
    } else if(input$type == "Single Track") {
      trackID <- input$albumId
      result <- data.frame()
      audioFeaturesURL <- paste("https://api.spotify.com/v1/audio-features/", 
                                trackID, 
                                sep="")
      getaudioFeatures <- GET(audioFeaturesURL, spotifyToken)
      audioFeatures <- jsonlite::fromJSON(toJSON(content(getaudioFeatures)))
      result[1,1] <- paste(keys[audioFeatures$key + 1],trackMode[audioFeatures$mode + 1], sep=" - ")
      result[1,2] <- paste(audioFeatures$time_signature, "/4", sep="")
      names(result) <- c("Key & Mode", "Time Signature")
      return(result)
      
    }
  })
})
