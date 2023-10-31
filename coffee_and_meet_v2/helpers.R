# Initialise data structures
membership <- read.csv("data/membership.csv")
historic_connections <- read.csv("data/historic_connections.csv")
historic_connections$date <- parse_date_time(historic_connections$date, orders = c("ymd","dmy"))
list_of_members <- membership %>% dplyr::filter(status == "active") %>% dplyr::select(person) %>% dplyr::distinct(person) %>% dplyr::arrange(person)
list_of_meeting_dates <- historic_connections %>% dplyr::select("date") %>% dplyr::distinct() %>% arrange(desc("date")) %>% pull("date")

# Fill the written email with names and copy to clipboard
fill_copy_email <- function(letterbody){
  initial_setup <- connection_tracker %>% dplyr::filter(contacted == 0) %>% slice_head()
  updated_letter <- stringr::str_replace_all(letterbody, c("%pn%" = initial_setup$name, "%cn%" = initial_setup$connection))
  writeLines(text = updated_letter, con = "clipboard", sep = "")
}

# Copy the primary persons email to clipboard 
copy_primary_email <- function(){
  # Get name
  primary_name <- connection_tracker %>% dplyr::filter(contacted == 0)%>% slice_head() %>% pull(name)
  # Get email 
  pimary_person_email <- membership %>% dplyr::filter(person == primary_name) %>% pull(email)
  # copy to clipboard
  writeLines(text = pimary_person_email, con = "clipboard", sep = "")
}

# Copy the secondary persons email to clipboard 
copy_secondary_email <- function(){
  # Get name
  secondary_name <- connection_tracker %>% dplyr::filter(contacted == 0) %>% slice_head() %>% pull(connection)
  print(secondary_name)
  # Get email 
  secondary_name_email <- membership %>% dplyr::filter(person == secondary_name) %>% pull(email)
  # copy to clipboard
  writeLines(text = secondary_name_email, con = "clipboard", sep = "")
}

tidy_and_next_pair <- function(){
  # update the connection tracker for both people
  connection <- connection_tracker %>% dplyr::filter(contacted == 0) %>% slice_head()
  # check to see if end reached if so then skip
  if (nrow(connection) != 0){
    primary_name <- connection$name
    secondary_name <- connection$connection
    # Update global variable
    connection_tracker <<- connection_tracker %>% mutate(contacted = ifelse(name == primary_name & connection == secondary_name, 1, contacted))
    connection_tracker <<- connection_tracker %>% mutate(contacted = ifelse(name == secondary_name & connection == primary_name, 1, contacted))
    # save to file 
    write.csv(connection_tracker, file_name, row.names=FALSE)
  }
}

# Function to create new meeting pairs 
generate_meeting_pairs_wrapper <- function(date_for_new_meeting){
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
      maxRetries <- 100000
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
  people_to_meet <- membership %>% dplyr::filter(status == "active") %>% dplyr::distinct() %>% dplyr::pull(person)
  # if odd number of people remove nick
  if (length(people_to_meet) %% 2 == 0) {
    print("Even number, Nick is in")
  } else {
    print("Odd number, Bye bye Nick")
    people_to_meet <- people_to_meet[!people_to_meet == "Nick Biddiscombe"]
  }
  print("here")
  output <- generate_meeting_pairs(people_to_meet)
  # Check if success if yes then continue and report success
  if (length(output[[2]]) != 0){
    return(FALSE)
  } else {
      meeting_matches <- data.frame(output[1])
      
      # Convert and add back to the list of historic connections to save with the provided date of the meeting
      export_list <- data.frame(name = character(0), connection = character(0), date=dmy())
      for (person in people_to_meet){
        p_to_meet <- colnames(meeting_matches)[meeting_matches[person, ] == 2]
        temp <- data.frame(name = person, connection = str_replace_all(p_to_meet, "[.]", " "), date=ymd(date_for_new_meeting))
        export_list <- rbind(export_list, temp)
      }
      # bind with the original data and save out
      historic_connections$date <- ymd(historic_connections$date)
      to_save_out <- rbind(historic_connections, export_list)
      # update global version
      historic_connections <<- to_save_out
      write.csv(to_save_out, "data/historic_connections.csv", row.names = FALSE)
      # Add the meeting date to the list of dates 
      list_of_meeting_dates <<- append(list_of_meeting_dates, ymd(date_for_new_meeting))
      return(TRUE)
  }
}
