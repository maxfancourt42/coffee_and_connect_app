library(shiny)
library(tidyverse)
library(stringr)
library(lubridate)

source("helpers.R")
# Define UI ----
ui <- navbarPage("Coffee and Meet 2.0",
                 tabPanel("Create a new meeting",
                          titlePanel("Create a New Meeting"),
                          sidebarLayout(
                            sidebarPanel(h4("Create a new meeting by:"),
                                         h5("1. Select a date for the meeting"),
                                         h5("2. Check that all participants you want to invite are set to active"),
                                         h5("3. Press create and save matches")),
                            mainPanel(h4("1. Select a date for the meeting"),
                                      dateInput("date_for_new_meeting", label = "New Meeting Date"),
                                      textOutput("date_for_new_meeting"),
                                      textOutput("is_date_used"),
                                      h4("2. Check that all participants you want to invite are set to active"),
                                      h5("There are currently", length(list_of_members$person), "active members"),
                                      h5(strong(textOutput(outputId = "is_nick_needed"))),
                                      h5("If this is incorrect then please close the program and update data/membership.csv"),
                                      h4("3. Press create and save matches"),
                                      actionButton("create_meeting_pairs", label = "Create"),
                                      textOutput(outputId = "matches"))
                            )
                          ),
                 tabPanel("Select a meeting",
                          titlePanel("Select a meeting to send emails out to"),
                          sidebarLayout(
                            sidebarPanel(h4("Email Preparation"),
                                         h5("1. Select the meeting from the drop down menu that you want to email everyone about"),
                                         h5("2. Check that you have selected the right date by reading the confirmation text below the box")),
                            mainPanel(
                              h4("1. Select a date for the meeting"),
                              selectInput("meetingdatesoptions", label = NULL, choices = list_of_meeting_dates, multiple = FALSE, selectize = TRUE),
                              textOutput(outputId = "date_selected")
                            ),
                            )
                          ),

                 tabPanel("Send emails to selected meeting participants",
                          titlePanel("Send emails to meeting participants"),
                          sidebarLayout(
                            sidebarPanel(h4("Email Assistant"),
                                         h5("1. Type/paste the email that you want to send to everyone in the email template box"),
                                         h5("2. Replace the name of the person this will be sent to with '%pn%'"),
                                         h5("3. Replace the name of the person they will be meeting with '%cn%'"),
                                         h5("4. Press the fill and copy button to copy the email to your clipboard"),
                                         h5("5. Press the copy first person email, to copy the first persons email to your clipboard"),
                                         h5("6. Press the copy second person email, to copy the second persons email to your clipboard"),
                                         h5("7. Press the done button to move to the next person")
                                         ),
                            mainPanel(
                              fluidRow(column(3, textOutput("primary_person"), align="center"),
                                       column(3, "connecting with", align="center"),
                                       column(3, textOutput("secondary_person"), align="center")),
                              h4("1. Type/paste the email that you want to send to everyone in the email template box"),
                              textAreaInput("letterbody", label = NULL, value = "", placeholder="Paste letter template here", width="100%",resize="vertical"),
                              h4("2. Replace the name of the person this will be sent to with"),
                              h4("%pn%"),
                              h4("3. Replace the name of the person they will be meeting with "),
                              h4("%cn%"),
                              h4("4. Fill email template and copy to clipboard"),
                              actionButton("fill_email", label = "Click to fill provided and copy to clipboard"),
                              h4("5. Copy primary person email"),
                              actionButton("copy_pp_email", label = "Click to copy primary person email"),
                              h4("6. Copy secondary person email"),
                              actionButton("copy_sp_email", label = "Click to copy secondary person email"),
                              h4("7. Press the done button to move onto the next pair to email"),
                              actionButton("tidy_move_on", label = "Click to move onto next pair to contact")
                            )),
                          ),
)

# Define server logic ----
server <- function(input, output, session) {
  # reactive values 
  v <- reactiveValues()
  
  # Primary person 
  output$primary_person <- renderText({"Primary Person"})
  output$secondary_person <- renderText({"Secondary Person"})
  
  observeEvent(input$meetingdatesoptions, {
    # check to see if a contact tracker exists for this meeting, if not then make one
    # make the file name 
    file_name <<- paste("data/contact_trackers/", input$meetingdatesoptions, "_contracttracker.csv", sep="")
    print(ymd(input$meetingdatesoptions))

    if (!file.exists(file_name)){
      contacts_to_track <- as.data.frame(historic_connections %>% dplyr::filter(date == ymd(input$meetingdatesoptions)))
      contacts_to_track$contacted <- 0
      file.create(file_name)
      write.csv(contacts_to_track, file_name, row.names = FALSE)
    }

    # open the file for use
    connection_tracker <<- read.csv(file_name)
    # attempt initial setup, if nothing then show blank data 
    initial_setup <- connection_tracker %>% dplyr::filter(contacted == 0) %>% slice_head()
    output$primary_person <- renderText({initial_setup$name})
    output$secondary_person <- renderText({initial_setup$connection})
  })
  
  observeEvent(input$fill_email, {
    fill_copy_email(input$letterbody)
  })
  
  observeEvent(input$copy_pp_email,{
    copy_primary_email()
  })
  
  observeEvent(input$copy_sp_email,{
    copy_secondary_email()
  })
  
  observeEvent(input$tidy_move_on, {
    tidy_and_next_pair()
    # attempt initial setup, if nothing then show blank data 
    initial_setup <- connection_tracker %>% dplyr::filter(contacted == 0) %>% slice_head()
    if (nrow(initial_setup) != 0){
      output$primary_person <- renderText({initial_setup$name})
      output$secondary_person <- renderText({initial_setup$connection})
    }else {
      output$primary_person <- renderText({"No More Pairs to match"})
      output$secondary_person <- renderText({"No More Pairs to match"})
    }
  })

  # What date has been selected to create all the emails for 
  output$date_selected <- renderText({
    paste("You have selected: ", input$meetingdatesoptions)
    })
  
  # reactive output for which meeting the user has selected
  output$date_for_new_meeting <- renderText({ 
    paste("You have selected", input$date_for_new_meeting)
  })
  # reactive output for whether Nick is required or not 
  output$is_nick_needed <- renderText({
    if ((length(list_of_members$person) %% 2) == 0) {
      return("Even number of participants including you so you will be required Nick")
    }else{
      return("Odd number of participants including you,so you will not be required Nick")
    }
  })
  # reactive output to check if the current day selected has already had a meeting or not 
  output$is_date_used <- renderText({
    # Get all unique dates 
    historic_dates <- historic_connections %>% dplyr::select("date") %>% unique()
    # check if current selected date is in this list
    if (input$date_for_new_meeting %in% historic_dates$date){
      return("The selected date has already been used for a meeting")
    }
    else{
      return("The selected date is free for a meeting")
    }
  })
  
  # reactive button that calls the meeting pair matching algorith and reports success
  output$matches <- eventReactive(input$create_meeting_pairs, {
   if (generate_meeting_pairs_wrapper(input$date_for_new_meeting)){
     # Update the thing with choices
     updateSelectInput(session, "meetingdatesoptions", choices = list_of_meeting_dates)
     print("New meeting pairs created successfully, select a meeting to send emails")
   } else {
     print("Failure to create meeting pairs, try again")
   }
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)