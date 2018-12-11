#
# Shiny App for Bladen County Mail-In Map for 11/

library(shinydashboard)
library(dplyr)
library(leaflet)
library(RColorBrewer)
library(lubridate)
library(scales)
library(tidyr)

url <- a("https://link2matt.github.io/bladen-co-nc-election-2018/", 
         href="https://link2matt.github.io/bladen-co-nc-election-2018")

# Define UI for Shiny application 
ui <- dashboardPage(
  dashboardHeader(title = "Bladen Co. Mail-In"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(width = 9,
             box(width = NULL, solidHeader = TRUE,
                 leafletOutput("map", height = 500)
             ),
             p(
               #class = "text-muted",
               paste("This is an interactive map of mail-in ballots cast for the US House 9th District 
                     election that occurred on 11/6/18. The larger gray circles are around 
                     areas where press reports indicate suspicions that voters were targeted 
                     as part of a campaign of electoral fraud in Bladen County, North Carolina."
               )
             ),
            box(width = NULL,
                h3(
                  paste("Bladen Mail-In Ballots Acceptance Rate")
                ),
                  tableOutput("votesTable")
                ),
            p(
              paste("The table reports the rate at which mail-in ballots for the 9th District were accepted in two cities and the entire county. 
                    Ballots are further categorized by whether they were cast by African-American voters or by voters who were not African-American.
                    Bladenboro in the southwest portion of Bladen County is one area where suspected electoral fraud occurred. 
                    Rejected ballots include both ballots that were received by the elections board but not counted and ballots that were never returned to the election board.")
            ),
            p(class = "text-muted",
                    paste("Data for this analysis was retrieved from the North Carolina State Board of Elections’s website,
                          ncsbe.gov.  A detailed description of this analysis is available on this webpage 
                          "),
              tagList(url)
            )
      ),
      column(width = 3,
             box(width = NULL, status = "warning",
                checkboxGroupInput("partyButtons", "Party of Voter",
                                   c ("Republican" = "REP", "Unaffiliated"="UNA", "Democratic"="DEM"),
                                   selected = c("DEM", "UNA")),
                checkboxGroupInput("raceButtons", "Race of voter",
                                   c("African-American" = "African-American",
                                     "Race Other than African-Am." = "Other"  
                                   ),
                                   selected = c("African-American")
                ),
                checkboxGroupInput("ballotButtons", "Ballot Status",
                                   c ("Accepted"="ACCEPTED", "Rejected"="REJECTED", "Not Returned"="MISSING"),
                                   selected = c("ACCEPTED","REJECTED","MISSING")),
                checkboxInput("suspect", "Suspect Areas", value = T)
                ) ## Close box
      ) ## Close Column
    ) ## Close Fluid Row
  ) ## Close Body
) ## Close Page


# R Code to Process Data

Mail9thBladen <- read.csv("data/Mail9thBladenx_geocodio.csv")
names(Mail9thBladen)[names(Mail9thBladen)=="fullAdress"] <- "fullAddress"
Mail9thBladen$ballot_req_dt <- mdy(Mail9thBladen$ballot_req_dt)
Mail9thBladen$ballot_rtn_dt <- mdy(Mail9thBladen$ballot_rtn_dt)

## Suspect Areas
votersPress <-  Mail9thBladen %>%
        filter(voter_first_name %in% "EMMA" & voter_last_name %in% "SHIPMAN")
votersPress <-  Mail9thBladen %>%
        filter(voter_first_name %in% "DATESHA" & voter_last_name %in% "MONTGOMERY") %>%
        bind_rows(votersPress)
votersPress <-  Mail9thBladen %>%
        filter(voter_first_name %in% "JENEVA" & voter_last_name %in% "LEGIONS") %>%
        bind_rows(votersPress)
suspectAreas <- votersPress %>%
        filter(ballot_rtn_status != "ACCEPTED")

## Add ballot2 variable with simplified categorization
Mail9thBladen <- Mail9thBladen %>%
  mutate(ballot2 = case_when(
    ballot_rtn_status == "ACCEPTED" ~ "ACCEPTED",
    ballot_rtn_status == "RETURNED UNDELIVERABLE" ~ "REJECTED",
    ballot_rtn_status == "SPOILED" ~ "REJECTED",
    ballot_rtn_status == "VOTER SIGNATURE MISSING" ~ "REJECTED",
    ballot_rtn_status == "WITNESS INFO INCOMPLETE" ~ "REJECTED",
    ballot_rtn_status == "" ~ "MISSING"
  ))

## Add race2 variable with simplified categorization
Mail9thBladen <- Mail9thBladen %>%
  mutate(race2 = case_when(
    race=="BLACK or AFRICAN AMERICAN" ~ "African-American",
    race!="BLACK or AFRICAN AMERICAN" ~ "Other"
  ))

Mail9thBladen$race2 <- as.factor(Mail9thBladen$race2)


## Add Variaable combining race and ballot status
Mail9thBladen <-  Mail9thBladen %>%
        mutate(mapRaceBallot = ifelse(race2=="African-American", "African-American ", "Other Race ")) %>%
        mutate(mapRaceBallot = paste0(mapRaceBallot, ifelse(ballot_rtn_status=="ACCEPTED", "Accepted", "Rejected")))

Mail9thBladen$mapRaceBallot <- as.factor(Mail9thBladen$mapRaceBallot)

## Set colors for map
levelsRace3 <- levels(Mail9thBladen$mapRaceBallot)

cofrace3 <- colorFactor(c("#339933", "#FF3030", "#DDDD00", "#888888"), 
                        domain=levelsRace3)


## Table Generation

tableCity2 <- Mail9thBladen %>% 
  group_by(voter_city, race2) %>% 
  count(ballot_rtn_status) %>% 
  spread(key = ballot_rtn_status, value = n)

## Turns NA's to Zero's
tableCity2$ACCEPTED <- ifelse(is.na(tableCity2$ACCEPTED), 0, tableCity2$ACCEPTED)
tableCity2$V1 <- ifelse(is.na(tableCity2$V1), 0, tableCity2$V1)
tableCity2$`RETURNED UNDELIVERABLE` <- ifelse(is.na(tableCity2$`RETURNED UNDELIVERABLE`), 0, tableCity2$`RETURNED UNDELIVERABLE`)
tableCity2$SPOILED <- ifelse(is.na(tableCity2$SPOILED), 0, tableCity2$SPOILED)
tableCity2$`VOTER SIGNATURE MISSING` <- ifelse(is.na(tableCity2$`VOTER SIGNATURE MISSING`), 0, tableCity2$`VOTER SIGNATURE MISSING`)
tableCity2$`WITNESS INFO INCOMPLETE` <- ifelse(is.na(tableCity2$`WITNESS INFO INCOMPLETE`), 0, tableCity2$`WITNESS INFO INCOMPLETE`)

## Calculate all 9th Mail-In Votes and add back to tableCity2
countyNums <- tableCity2 %>% 
  group_by(race2)  %>% 
  summarise_at(vars(V1:`WITNESS INFO INCOMPLETE`), sum, na.rm =T) 

countyNums <- countyNums %>% 
  mutate(propBallot = ACCEPTED/(V1 + ACCEPTED + `RETURNED UNDELIVERABLE` + 
                      SPOILED + `VOTER SIGNATURE MISSING` + `WITNESS INFO INCOMPLETE`))
countyNums<- countyNums %>% 
  select(race2, propBallot)

countyResults <- select(spread(countyNums, key=race2, value = propBallot), `African-American`, Other) ## Switched below from White to Other
countyResults <- cbind(as.data.frame("All Ballots for US 9th"), countyResults)

names(countyResults)[names(countyResults)=="\"All Ballots for US 9th\""] <- 'Location'

tableCity2 <- tableCity2 %>% 
  mutate(propBallot = ACCEPTED/(V1 + ACCEPTED + `RETURNED UNDELIVERABLE` + 
                      SPOILED + `VOTER SIGNATURE MISSING` + `WITNESS INFO INCOMPLETE`))

tableCity3 <- tableCity2 %>% 
  select( voter_city, race2, propBallot) %>% 
  filter(voter_city %in% c("BLADENBORO", "ELIZABETHTOWN"))

cityResults <- select(spread(tableCity3, key=race2, value = propBallot), 
                      voter_city, `African-American`, Other)  # Switched form White to Ohter

names(cityResults)[names(cityResults)=="voter_city"] <- 'Location'

cityResults$Location <- as.character(cityResults$Location)
countyResults$Location <- as.character(countyResults$Location)

## Convert numbers to percentage
resultsTable <- bind_rows(as.data.frame(cityResults), countyResults)
resultsTable$`African-American` <- percent(resultsTable$`African-American`)
resultsTable$Other <- percent(resultsTable$Other)  ## Switched from White to Other

names(resultsTable)[names(resultsTable)=="Other"] <- "Race Other than African-Am."




## Shiny Server

server <- function(input, output, session) {
  output$votesTable <- renderTable(resultsTable)

        # Reactive expression for the data subsetted to what the user selected
        filteredData <- reactive({
                filter(Mail9thBladen, ballot_request_party == input$party)
        })       
        
        filteredData2 <- reactive({
                Mail9thBladen %>% 
                   filter(voter_party_code %in% input$partyButtons) %>% 
                   filter(race2 %in% input$raceButtons) %>% 
                   filter(ballot2 %in% input$ballotButtons)
        })    

        output$map <- renderLeaflet({
            leaflet(Mail9thBladen) %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            setView(-78.604867, 34.629176, zoom = 11) %>%             
            addLegend(position = "bottomright",
                                  pal = cofrace3, values = ~mapRaceBallot,
                                  title = "Absentee Ballots")
        })
        
        # Incremental changes to the map (in this case, replacing the
        # circles when a cohort is chosen) should be performed in
        # an observer. 
        observe({
                if (input$suspect) {
                    leafletProxy("map", data = filteredData2()) %>%
                    clearShapes() %>%
                        addCircles(suspectAreas$Longitude, suspectAreas$Latitude,
                                   radius = 2000, color = "#444444", stroke = T, 
                                   fillOpacity = 0.1
                        ) %>% 
                    addCircles(radius=150, weight = 1,
                              color=~cofrace3(mapRaceBallot),
                              fillOpacity = 0.6, stroke = T, popup = ~fullAddress)
                }
                if (!(input$suspect)) {
                  leafletProxy("map", data = filteredData2()) %>%
                  clearShapes() %>%
                  addCircles(radius=150, weight = 1,
                         color=~cofrace3(mapRaceBallot),
                         fillOpacity = 0.6, stroke = T, popup = ~fullAddress)
          }
        }) ## End Observer
} ## Ender Shiny Server


# Run the application 
shinyApp(ui = ui, server = server)

