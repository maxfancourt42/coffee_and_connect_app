
# Create a data frame with a list of people
membership <- read.csv("E:\\NE Data\\NaturalEnglandWork\\coffee_and_meet\\coffee_and_meet\\data\\membership.csv")
historic_connections <- read.csv("E:\\NE Data\\NaturalEnglandWork\\coffee_and_meet\\coffee_and_meet\\data\\historic_connections.csv")


generate_meeting_pairs <- function(people_to_match) {  
  # Create a matrix of all possible combinations 
  incidentmatrix <- matrix(0, nrow=length(people_to_match),ncol=length(people_to_match))
  rownames(incidentmatrix) <- people_to_match
  colnames(incidentmatrix) <- people_to_match

  # Fill in the matrix with all historic connections
  for (i in 1:nrow(historic_connections)) {
    try({
      row <- historic_connections[i, ]
      variable1 <- row$name
      variable2 <- row$connection
      incidentmatrix[variable1, variable2] <- 1
      incidentmatrix[variable2, variable1] <- 1
    })
  }
  diag(incidentmatrix) <- 1

  # Get the people with the most connections as they are going to be the hardest to place 
  most_connected <- sort(rowSums(incidentmatrix), decreasing = TRUE)
  not_matched <- list()

  generate_pairs <- function() {
    for (i in 1:length(most_connected)){
      # check if person already has a meeting
      if (!any(incidentmatrix[names(most_connected[i]), ] == 2)){
        # create a list of all the people that this person hasn't yet met 
        notmet <- sort(incidentmatrix[names(most_connected[i]), incidentmatrix[, names(most_connected[i])] == 0],decreasing = TRUE)
        notmet_updated <- list()
        # remove all people that already have a potential match
        for (person in 1:length(notmet)){
          if (!any(incidentmatrix[names(notmet[person]),] == 2)){
            notmet_updated <- c(notmet_updated,notmet[person])
          }
        }
        # from the people that the person hasn't met, pick the person with the highest number of connections
        if (length(notmet_updated) > 0){
          # generate a random number within limits
          random_value <- sample(1:length(notmet_updated), 1)
          # make the potentiaal match by recording in matrix
          incidentmatrix[names(notmet_updated[random_value]),names(most_connected[i])] <- 2
          incidentmatrix[names(most_connected[i]),names(notmet_updated[random_value])] <- 2
        }else {
          not_matched <- c(not_matched, most_connected[i])
          print("someone not matched")
        }
      }
    }
    return(list(incidentmatrix, not_matched))
  }

  # First attempt matching 
  output <- generate_pairs()

  if (length(output[[2]]) == 0) {
    print("Solution Reached")
    return(output)
  } else {
    print("Solution not reacted, retrying")
    maxRetries <- 1000
    retries <- 0

    while (length(output[[2]]) != 0) {
      print(retries)
      if (retries >= maxRetries) {
        print("Max retries reached no solution possible")
        break
      }else {
        output <- generate_pairs()
        retries <- retries + 1
      }
    }
  }
}


# function to printing out stuff
people_to_meet <- membership %>% dplyr::filter(status == "active") %>% dplyr::distinct() %>% dplyr::pull(person)
# if odd number of people remove nick
if (length(people_to_meet) %% 2 == 0) {
  print("Even number, Nick is in")
} else {
  print("Odd number, Bye bye Nick")
  people_to_meet <- people_to_meet[!people_to_meet == "Nick Biddiscombe"]
}

output <- generate_meeting_pairs(people_to_meet)
meeting_matches <- data.frame(output[1])

# Convert and add back to the list of historic connections to save with the provided date of the meeting
export_list <- data.frame(name = character(0), connection = character(0), date=dmy())
for (person in people_to_meet){
  p_to_meet <- colnames(meeting_matches)[meeting_matches[person, ] == 2]
  temp <- data.frame(name = person, connection = str_replace_all(p_to_meet, "[.]", " "), date=dmy("31/07/2023"))
  export_list <- rbind(export_list, temp)
}
# bind with the original data and save out
historic_connections$date <- dmy(historic_connections$date)
to_save_out <- rbind(historic_connections, export_list)
write.csv(to_save_out, "E:\\NE Data\\NaturalEnglandWork\\coffee_and_meet2\\coffee_and_meet_v2\\data\\historic_connections_update.csv")



# Convience viewing function
for (person in people_to_meet){
  p_to_meet <- colnames(meeting_matches)[meeting_matches[person, ] == 2]
  print(sprintf("%s is going to meet %s", person, str_replace_all(p_to_meet, "[.]", " ")))
}






# if sure then conver
#X[X < .1] <- 0

