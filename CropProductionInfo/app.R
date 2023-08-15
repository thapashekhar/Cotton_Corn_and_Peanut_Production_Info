########### library ##############
#library(shiny)
library(tidyverse)
require(gridExtra)
library(USAboundaries)
library(viridis)
library(sf)

######################### Data importing and wrangling ################################################
# data downloaded from NASS
crop_production <- read_csv("CropProductionData.csv",show_col_types = FALSE) %>%
  mutate(State = str_to_title(State), Commodity = str_to_title(Commodity)) %>%
  mutate(Value_c = case_when(
    Commodity =="Cotton" ~ Value/10000,
    Commodity =="Corn" ~ Value/10000,
    Commodity == "Peanuts" ~ Value/1000000
  )) %>%
  mutate(Unit_c = case_when(
    Commodity =="Cotton" ~ "Cotton production \n(10 thousands Bales)",
    Commodity =="Corn" ~ "Corn production \n(10 thousands bushels)",
    Commodity == "Peanuts" ~ "Peanuts production \n(millions of pounds)"
  )) %>%
  dplyr::select(Year, State, Commodity,Value, Value_c, Unit_c)

# min and max year for default range in plots
minYear = min(crop_production$Year)
maxYear = max(crop_production$Year)

# US states name and their geometry for map plotting
US_state_boundry <- us_states(resolution ="high", states = state.name) %>%
  dplyr::select(State = state_name, geometry)%>%
  filter(State != c("Alaska","Hawaii"))

##################### function to plot trend line ##################################
trend_line <- function(data1, line_color,nbreaks){
  p1 <- ggplot(data = data1) +
    geom_line(mapping = aes(x=Year, y= Value_c), color = line_color)+
    labs(x=  "Year",
         y= unique(data1$Unit_c),
         title = paste(unique(data1$Commodity),"Production Trend in",unique(data1$State),min(data1$Year),"-",max(data1$Year)))+
    scale_x_continuous(n.breaks = nbreaks)+
    theme(plot.title = element_text(hjust = 0.5, face = "bold"), text = element_text(size =15))
  return(p1)}

######################## function to combine trend line as per user input ##############
plotting_trend <- function(state_name, crop = c("Corn","Cotton","Peanuts"), Date_Range = c(minYear,maxYear)){
  # defining a not in operator
  `%!in%` <- Negate(`%in%`)
  
  # converting to title case
  state_name <- str_to_title(state_name)
  crop <- str_to_title(crop)
  
  if (state_name %!in% state.name){
    stop("Error: Enter correct name of the state.")
  }
  if (state_name %!in% crop_production$State){
    stop("Error: Entered name of state is not in the data set.")
  }
  if (all(crop %!in% c("Corn", "Cotton","Peanuts"))){
    stop("Error: Select or enter among Corn, Cotton or Peanuts.")}
  
  data1 <- crop_production %>%
    filter(State == state_name & (Year>=Date_Range[1] & Year<=Date_Range[2]))
  
  
  data_corn <- data1%>%
    filter(Commodity == "Corn" )
  nbreaks_corn = ifelse(length(unique(data_corn$Year))>=8,8,length(unique(data_corn$Year)))
  line_color_corn ="red"
    
  data_cotton <- data1%>%
    filter(Commodity == "Cotton" )
  nbreaks_cotton = ifelse(length(unique(data_cotton$Year))>=8,8,length(unique(data_cotton$Year)))
  line_color_cotton ="blue"
    
  data_peanuts <- data1%>%
    filter( Commodity == "Peanuts" )
  nbreaks_peanuts = ifelse(length(unique(data_peanuts$Year))>=8,8,length(unique(data_peanuts$Year)))
  line_color_peanuts ="black"
    
  
  corn_p_t = trend_line(data_corn, line_color_corn, nbreaks_corn)
  cotton_p_t = trend_line(data_cotton, line_color_cotton, nbreaks_cotton)
  peanuts_p_t = trend_line(data_peanuts, line_color_peanuts, nbreaks_peanuts)
  
  vv = lapply(crop, switch,"Corn"= corn_p_t, "Cotton" = cotton_p_t, "Peanuts" = peanuts_p_t)
  grid.arrange(arrangeGrob(grobs=vv))
  return()
}

############################ function to plot map#############################################################################
map_plot <- function(CropName,year){
  data2 <- crop_production%>%
    filter(Commodity ==CropName & Year == year) %>%
    dplyr::select(State, Commodity, Year, Value_c, Unit_c)
  
  US_state_boundry %>%
    left_join(data2, by ="State")%>%
    ggplot()+
    geom_sf(data = ~., size =.4)+
    geom_sf(data = ~.,aes(fill = Value_c), size = .2)+
    # Selecting a colorblind-safe palette
    scale_fill_viridis_c(option = "C")+
    # Changing fill legend label, giving a title
    labs(fill = unique(data2$Unit_c),
         title = paste("State-level",CropName,"Production in",year),
         caption = "Alaska and Hawaii are excluded from the USA map as they are not in the dataset."
    )+
    # Adding county names
    geom_sf_text(data = ~.,
                 aes(label = State),
                 size = 3.5,
                 color = "white"
    )+
    # Changing to a cleaner theme
    theme_void()+
    theme(text = element_text(size = 4), plot.title = element_text(hjust = 0.5, size = 18, face = "bold"), 
          legend.title = element_text(size = 14,vjust = 0),legend.position = "bottom",
          legend.text = element_text(size=12),legend.key.width = unit(1.7,"cm"), legend.key.height = unit(0.5,"cm"), 
          plot.caption = element_text(hjust = 1, vjust = 0, size = 10, face ="italic"))
}
################################ function to plot top ranked states producing specific crop ###################################
rankBar_plot <- function(CropN, inYear,toprank){
  data3<- crop_production%>%
    filter(Commodity ==CropN & Year==inYear)%>%
    arrange(desc(Value_c))
  if (toprank > nrow(data3)){
    toprank = nrow(data3)
  }
  
  data3 <- data3%>%
    head(toprank) %>%
    mutate(index = 1:toprank)
  
  ggplot(data= data3)+
    geom_col(mapping = aes(x = factor(index), y= Value_c), fill = "blue", color ="red")+
    scale_x_discrete(limits=rev, label= rev(data3$State))+
    labs(x= "",
         y = unique(data3$Unit_c),
         title = paste("Top",toprank, CropN, "Producing States in USA,",inYear))+
    theme(plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),text = element_text(size =15))+
    coord_flip()
}

################################ user interface ###############################################################################
ui <- fluidPage(
  # App title
  titlePanel(strong("Crop Production Info")),
  tabsetPanel(
    tabPanel(h5(strong("Trends of Crop Production")),fluid= TRUE,
             sidebarLayout(
               sidebarPanel(
                 (" In this tab, we can visualize the trends of crops production in a particular state in a desired time frame."),
                 selectizeInput("StateName", 
                                label = h4(strong("State Name:")),
                                choices = state.name,
                                selected = "Georgia"),
                 helpText("Select or type the name of a state of USA to view crop production trend."),
                 
                 checkboxGroupInput("croptypes",
                                    h4(strong("Select Crops to Plot:")),
                                    choices = list("Corn"="Corn","Cotton"="Cotton", "Peanuts"="Peanuts"),
                                    selected = c("Corn","Cotton","Peanuts")),
                 helpText("Select any or all of the above crops to see the corresponding production trend plots."),
                 sliderInput("year_range",
                             h4(strong("Year (from - to):")),
                             min = minYear, max = maxYear, value = c(minYear, maxYear)),
                 helpText("Slide to select the range of year to view the crop production range."),
                 br(),
                 helpText("Note:If some plots apears blank, that means the data for the state is not available in source data.")
               ),
               
               mainPanel(
                 plotOutput("trendPlot", height = "800px")
               ))),
    
    tabPanel(h5(strong("Top Ranked States in Crop Production")), fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 ("In this tab, we can see and compare the top ranked states in producing a specific crop in a particular year."),
                 radioButtons("CropNme",
                              label = h4(strong("Crop")),
                              choices = list("Corn"="Corn","Cotton"="Cotton", "Peanuts"="Peanuts"),
                              selected = "Corn"),
                 helpText("Select a crop to see the top ranked states in its production."),
                 sliderInput("yearIn",
                             h4(strong("Year:")),
                             min = minYear, max = maxYear, value = 2022, dragRange = FALSE),
                 helpText("Slide to select the year to view the states' rank in the crop production in that year."),
                 sliderInput("TopRank",
                             h4(strong("Number of States in the Rank List:")),
                             min = 1, max = 50, value = 10, dragRange = FALSE),
                 helpText("Slide to select number of states to view on top list.")),
               
               mainPanel(
                 plotOutput("rankplot", height = "750px")
               ))),
    
    tabPanel(h5(strong("Map: State-wise Crop Production Distribution")), fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 ("In this tab, we can visualize a particular crop production distribution across the country in a particular year."),
                 radioButtons("Crop",
                              label = h3(strong("Crop")),
                              choices = list("Corn"="Corn","Cotton"="Cotton", "Peanuts"="Peanuts"),
                              selected = "Corn"),
                 helpText("Select a crop to see its production heat map in USA map"),
                 sliderInput("year",
                             h3(strong("Year:")),
                             min = minYear, max = maxYear, value = 2022, dragRange = FALSE),
                 helpText("Slide to select the year to view the crop production distribution in USA")),
               
               mainPanel(
                 plotOutput("Heatmap", height = "750px")
               ))),
    tabPanel(h5(strong("Info")), fluid = TRUE,
             h2(strong("Crop Production Info App")),
             sidebarLayout(
               sidebarPanel(
                 h2(strong("Introduction"), align = "center"),
                 br(),
                 p("This R Shiny app “Crop Production Info” explores and visualizes data on the production of
                 crops (cotton, corn, and peanuts) at the state level over time in USA. The data used in the app is sourced from
                 the National Agricultural Statistics Service, a part of United States Department of Agriculture ",a("(NASS,USDA).", href = "https://www.nass.usda.gov/")),
                 br(),
                 p("The app utilizes data from USDA’s National Agricultural Statistics Service (NASS) specifically on the
                  production of cotton, corn, and peanuts in all states over the years. NASS conducts numerous surveys
                  every year, including a census of agriculture every five years, and prepares reports covering various aspects
                  of U.S. agriculture. NASS provides a database ", a("'Quick Stat'", href = "https://quickstats.nass.usda.gov/"), "from which 
                  agriculture related data can be accessed using query by commodity, location, or time period. By selecting appropriate queries to extract
                  data from NASS’s Quick Stat, I have used this ", a("data",href ="https://quickstats.nass.usda.gov/results/06553643-76F2-3034-91DB-1F5DE45A1D5B"),", which is a surveyed crop production data for different states in USA."),
                 br(),
                 img(src = "crops.jpg", style="width: 100%", align ="center"),
                 br(),
                 br(),
                 p("The purpose of this app is to explore the production trends of each crop in each state over time, compare
                  the production of crops between states, and visualize (heat map) the distribution of crop production across the country for
                  a particular year. In other words, this app will provide visualization of crop production trends, the distribution of 
                 production, and the ranking of states based on the highest production of these commodities.")
               ),
               
               mainPanel(
                 h2(strong("Features:")),
                 h3(strong("Tab 1: Trends of Crop Production")),
                 p("- This tab allows user to visualize the crop production trends for user's desired state, crops types, and year of span."),
                 p("- If user selects a name of state that is not in the dataset, then it will shows a error message."),
                 p("- If a state does not have data for some of the crops, then a blank plot shows up."),
                 h3(strong("Tab 2: Top Ranked States in Crop Production")),
                 p("- This tab shows a horizontal bar plot of top ranked states in a particular crop production in a certain year."),
                 p("- User can select the number of states to be listed in ranking."),
                 p("- If user selects the number of states in the rank list greater than the total number of states producing the particular crops, then it will show total number of states in the ranking."),
                 h3(strong("Tab 3: Map: State-wise Crop Production Distribution")),
                 p("- This tab shows a map of states in USA and fill color based on the value of a particular crop production for a selected year."),
                 p("- If a state does not grow a particular crop or data is missing for the state, then a grey color will be filled on the corresponding state's region or boundary."),
                 p("- I removed Alaska and Hawaii from USA map in order to make the visually good looking aspect ratio of the map. And also, these states are not available in the given dataset."),
                 h3(strong("Tab 4: Info")),
                 p("- This tab provides an introduction of this app and details of each tabs available in the app."),
                 br(),
                 br(),
                 h3(strong("Created by:")),
                 strong("- Shekhar Thapa"),(", Graduate Student, University of Georgia"),
                 p("- For any questions, suggestions, or comments, contact at shekhar.thapa@uga.edu")
                 
               )
             )
             
    )
    
    
    
  )
  
)

################################### server ##########################################################################
server <- function(input, output) {
  output$trendPlot <- renderPlot({plotting_trend(input$StateName, input$croptypes, input$year_range)})
  
  output$Heatmap <- renderPlot({map_plot(input$Crop, input$year)})
  
  output$rankplot <- renderPlot({rankBar_plot(input$CropNme, input$yearIn,input$TopRank)})
}


################ run app #####################
shinyApp(ui = ui, server = server)