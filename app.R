
# SET UP ---------------------------------------------------------------

# Load libraries 
library(tidyverse)
library(lubridate)
library(plotly)
library(shiny)
library(shinyWidgets)

# Load in data 
wta_slams <- read_csv("data/cleaned/wta_slams_cleaned.csv")
atp_slams <- read_csv("data/cleaned/atp_slams_cleaned.csv")
country_mapping <- read_csv("data/cleaned/country_mapping_cleaned.csv")

# FUNCTIONS ------------------------------------------------------------

# Create function to join country mapping data to (atp or wta) slams data 
join_country_mapping <- function(data, column) {
    # Create keys connecting ioc codes to their continent and to their full country name
    code_to_continent_key <- setNames(country_mapping$continent, country_mapping$code)
    code_to_country_key <- setNames(country_mapping$country, country_mapping$code)
    # Add new columns for country name and continent corresponding to the ioc code in slams data 
    result <- data %>% 
        mutate(
            country = recode({{ column }}, !!!code_to_country_key),
            continent = recode({{ column }}, !!!code_to_continent_key),
        )
    
    return(result)
    
}

# Create function to get long format of slams data with each match having two rows (one for the winner and one for the loser)
create_long_slams_data <- function(data) {
    # Get long format of slams data
    long_data <- data %>%
        # Create column for year of tournament
        mutate(tourney_year = year(tourney_date)) %>%
        # Select columns of interest
        select(tourney_year, tourney_name, round, winner_ioc, loser_ioc) %>% 
        # Gather data to move winner_ioc and loser_ioc columns into rows
        gather(winner_loser, ioc, winner_ioc:loser_ioc)
    # Return long slams data
    return(long_data)
    
}

# Create function to group slams data by variable of interest (i.e. ioc or continent) to get participant information
group_slams_data <- function(data, variable, tourney_input) {
    # Group slams data by variable of interest
    grouped_data <- data %>%
        # Join country mapping data to slams data to connect ioc codes to their continent and to their full country name
        join_country_mapping(ioc) %>%
        # Group slams data based on variable of interest, slam tournament, year and round
        group_by(.data[[variable]], tourney_name, tourney_year, round) %>%
        # Add new column for number of participants
        summarise(n = n(), .groups = "drop") 
    # Regroup slams data if looking overall at all slams instead of one specific slam tournament 
    if (!(tourney_input %in% c("Australian Open", "French Open", "Wimbledon", "US Open"))) {
        grouped_data <- grouped_data %>%
            # Group by variable of interest, year and round only
            group_by(.data[[variable]], tourney_year, round) %>%
            # Add new column for number of participants 
            summarise(n = sum(n), .groups = "drop")
        
    }
    # Return grouped slams data 
    return(grouped_data)
    
} 

# Create function to add rows for slam tournaments/years/rounds with 0 participants
expand_slams_data <- function(data){
    # Create vector of all unique ioc codes in wta and atp slams data
    ioc <- unique(c(create_long_slams_data(wta_slams) %>% pull(ioc),
                    create_long_slams_data(atp_slams) %>% pull(ioc)))
    # Create vector of the 6 continents
    continent <- unique(country_mapping$continent)
    # Create vector with the 4 slam tournaments 
    tourney_name <- c("Australian Open", "French Open", "Wimbledon", "US Open")
    # Create vector with all years since 1968
    tourney_year <- 1968:(as.numeric(format(Sys.Date(), "%Y"))-1)
    # Create vector with all 7 rounds in a slam
    round <- c("R64", "R32", "R16", "QF", "SF", "F", "R128")
    
    # Create empty data frame with all possible combinations of slam information
    # If looking at specific slam and using ioc, get all possible combinations of ioc codes, slam tournaments, years and rounds 
    if ("tourney_name" %in% colnames(data) & "ioc" %in% colnames(data)) {
        empty_df <- expand_grid(ioc, tourney_name, tourney_year, round)
    # If looking at specific slam and using continent, get all possible combinations of continents, slam tournaments, years and rounds    
    } else if ("tourney_name" %in% colnames(data) & "continent" %in% colnames(data)) {
        empty_df <- expand_grid(continent, tourney_name, tourney_year, round)
    # If looking at all slams and using ioc, get all possible combinations of ioc codes, years and rounds    
    } else if (!("tourney_name" %in% colnames(data)) & "ioc" %in% colnames(data)) {
        empty_df <- expand_grid(ioc, tourney_year, round)
    # If looking at all slams and using continent, get all possible combinations of continents, years and rounds     
    } else {
        
        empty_df <- expand_grid(continent, tourney_year, round)
        
    }
    
    # Expand slams data to add rows for 0 participants 
    expanded_data <- data %>% 
        # Join the empty data frame to the slam data
        right_join(empty_df, by = colnames(empty_df)) %>%
        # Change any NAs created by joining to a 0 
        mutate(n = ifelse(is.na(n), 0, n)) 
    
    # Return expanded data
    return(expanded_data)
    
}

# Create function to filter slams data based on relevant variable of interest (ioc or continent), slam tournament and round information
filter_slams_data <- function(data, variable, variable_input, round_input, tourney_input) {
    # Filter data to look only at information for specified round and variable of interest 
    filtered_data <- data %>%
        filter(round == round_input & .data[[variable]] %in% variable_input)
    
    # Filter further if looking at a specific slam tournament 
    if (tourney_input %in% c("Australian Open", "French Open", "Wimbledon", "US Open")) {
        # Filter data to look only at information for specified slam tournament 
        filtered_data <- filtered_data %>% 
            filter(tourney_name == tourney_input)
        
    } 
    
    # Return filtered data
    return(filtered_data)
    
}

# Create function to generate interactive ggplotly
create_slams_ggplot <- function(data, variable, tourney_input, round_input) {
    
    g <- data %>% 
        rename(year = tourney_year) %>%
        ggplot(aes(x = year, y = n, color = .data[[variable]])) +
        geom_line() +
        ggtitle("Number of participants since 1968") +
        xlab("Year") +
        ylab("# of participants") +
        theme_bw() +
        theme(legend.title = element_blank()) 
    
    return(ggplotly(g))
    
}

# MISC WRANGLING -------------------------------------------------------

# Create data frame off all ioc codes and their corresponding country name and continent in WTA and ATP slams data
countries_in_data <- distinct(
    # Bind WTA and ATP information
    bind_rows(
        # Get WTA information
        tibble(
            ioc = create_long_slams_data(wta_slams) %>% 
                pull(ioc) %>% unique(.),
            country = create_long_slams_data(wta_slams) %>% 
                join_country_mapping(ioc) %>%
                pull(country) %>% unique(.)
        ),
        # Get ATP information
        tibble(
            ioc = create_long_slams_data(atp_slams) %>% 
                pull(ioc) %>% unique(.),
            country = create_long_slams_data(atp_slams) %>% 
                join_country_mapping(ioc) %>% 
                pull(country) %>% unique(.)
        )
    )
) 

# APP ------------------------------------------------------------------

# Defining UI
ui <- fluidPage(
    
    # Set shiny theme
    theme = shinythemes::shinytheme("simplex"),
    
    # Set app title
    titlePanel("Investigating Performance in Tennis Grand Slams"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            # Generate short description describing app
            p("Modify the inputs below to find out the number of (non-unique)",
              "participants in grand slams by country or continent.",
              "The raw tennis data for this app was obtained from",
              a("JeffSackmann.", 
                href = "https://github.com/JeffSackmann")),
            
            # Create input to select either WTA or ATP data
            radioGroupButtons(
                inputId = "tour",
                label = "Select tour:",
                choices = c("WTA", "ATP"),
                selected = "WTA",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            # Create input to select one of the four specific slam tournaments or all tournaments 
            radioGroupButtons(
                inputId = "tournament", label = "Select tournament:", 
                choices = c("Australian Open", "French Open", "Wimbledon", "US Open", "All"), 
                selected = "All",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            # Create input to select round
            radioGroupButtons(
                inputId = "round", label = "Select round:",
                choices = c("R128", "R64", "R32", "R16", "QF", "SF", "F"),
                selected = "R128",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            # Create geography input to display information by country or continent 
            radioGroupButtons(
                inputId = "geography",
                label = "Select geography variable:", 
                choices = c("Country" = "ioc", "Continent" = "continent"),
                selected = "ioc",
                checkIcon = list(yes = icon("ok", lib = "glyphicon"))
            ),
            
            # Add hole in ui for server code to fill in based on geography input selection
            uiOutput(outputId = "geography_input"),
        
        ),
        
        # Display ggplotly
        mainPanel(plotlyOutput(outputId = "line_plot"))
                     
    )
    
)

# Defining server 
server <- function(input, output) {
    
    # Fill in the placeholder created earlier with dynamically generated UI that is dependent on geography input
    output$geography_input <- renderUI({
        # Add freeze time to stop error stemming from dynamic changes to input value
        freezeReactiveValue(input, "dynamic")
        # Display either country choices or continent choices based on geography input selection 
        if (input$geography == "ioc") {
            pickerInput(
                inputId = "dynamic",
                label = "Select countries:", 
                choices = sort(setNames(countries_in_data$ioc, countries_in_data$country)),
                selected = c("USA", "FRA", "AUS"),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `none-selected-text` = "Please select some countries!"
                )
            )
        } else if (input$geography == "continent") {
            pickerInput(
                inputId = "dynamic",
                label = "Select continents:", 
                choices = unique(country_mapping$continent),
                selected = c("Europe", "North America", "Oceania"),
                multiple = TRUE,
                options = list(
                    `actions-box` = TRUE,
                    `live-search` = TRUE,
                    `none-selected-text` = "Please select some continents!"
                )
            )
        }
    })
    
    # Wrangle data based on user input selections
    df <- reactive({
        # Require that dynamic input is available before proceeding with data wrangling and plot generation
        req(input$dynamic)
        
        if (input$tour == "WTA") {
            slams <- wta_slams
        } else {
            slams <- atp_slams
        }
        
        slams %>% create_long_slams_data(.) %>% 
            group_slams_data(
                variable = input$geography, 
                tourney_input = input$tournament
            ) %>% 
            expand_slams_data(.) %>% 
            filter_slams_data(
                variable = input$geography, 
                variable_input = input$dynamic, 
                input$round, 
                tourney_input = input$tournament
            )
    })
    
    # Generate ggplotly using wrangled data 
    output$line_plot <- renderPlotly({
        
         df() %>%
            create_slams_ggplot(
                variable = input$geography, 
                tourney_input = input$tournament, 
                round = input$round
            )
        
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
