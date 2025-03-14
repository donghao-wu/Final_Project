
library(shiny)
library(shinythemes)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(htmlwidgets)
library(colorspace)
library(maps)
library(sf)


ncvs_data <- read.csv("data/NCVS_2020.csv")

ncvs_data <- ncvs_data %>%
  separate(YEARQ, into = c("YEAR", "QUARTER"), sep = "\\.", convert = TRUE) %>%
  mutate(Region_Label = recode(REGION, `1` = "Northeast", `2` = "Midwest", `3` = "South", `4` = "West")) %>%
  mutate(Quarter_Label = recode(QUARTER, 
                                `1` = "Q1 (Jan - Mar)",
                                `2` = "Q2 (Apr - Jun)",
                                `3` = "Q3 (Jul - Sep)",
                                `4` = "Q4 (Oct - Dec)"))

ncvs_p1 <- ncvs_data %>%
  filter(!AGE %in% c(98, 99), !NUM_INCIDENTS %in% c(98, 99)) %>%
  group_by(AGE) %>%
  summarise(total_incident = sum(NUM_INCIDENTS, na.rm = TRUE))

ncvs_p2 <- ncvs_data %>%
  filter(!INCOME %in% c(98, 99), !NUM_INCIDENTS %in% c(98, 99)) %>%
  group_by(INCOME) %>%
  summarise(total_incident = sum(NUM_INCIDENTS, na.rm = TRUE)) %>%
  mutate(INCOME = as.character(INCOME)) %>%
  mutate(Income_Label = recode(INCOME, 
                               "1" = "<$5K", "2" = "$5K-$7.5K", "3" = "$7.5K-$10K",
                               "4" = "$10K-$12.5K", "5" = "$12.5K-$15K", "6" = "$15K-$17.5K",
                               "7" = "$17.5K-$20K", "8" = "$20K-$25K", "9" = "$25K-$30K",
                               "10" = "$30K-$35K", "11" = "$35K-$40K", "12" = "$40K-$50K",
                               "13" = "$50K-$75K", "15" = "$75K-$100K", "16" = "$100K-$150K",
                               "17" = "$150K-$200K", "18" = ">$200K")) %>%
  mutate(Income_Label = factor(Income_Label, levels = c(
    "<$5K", "$5K-$7.5K", "$7.5K-$10K", "$10K-$12.5K", "$12.5K-$15K",
    "$15K-$17.5K", "$17.5K-$20K", "$20K-$25K", "$25K-$30K", "$30K-$35K",
    "$35K-$40K", "$40K-$50K", "$50K-$75K", "$75K-$100K", "$100K-$150K",
    "$150K-$200K", ">$200K"
  )))

ncvs_p3 <- ncvs_data %>%
  group_by(AGE) %>%
  summarise(total_incident = sum(NUM_INCIDENTS, na.rm = TRUE)) %>%
  drop_na(total_incident)

incident_median <- median(ncvs_p3$total_incident, na.rm = TRUE)
ncvs_p3 <- ncvs_p3 %>%
  mutate(Risk_Group = ifelse(total_incident > incident_median, "High-Risk Group", "Low-Risk Group"))

risk_colors <- c("Low-Risk Group" = "#3498DB",  
                 "High-Risk Group" = "#ff659f") 

us_states <- map_data("state")

region_mapping <- data.frame(
  region = tolower(c("Maine", "New Hampshire", "Vermont", "Massachusetts", "Rhode Island", "Connecticut",
                     "New York", "New Jersey", "Pennsylvania")),
  Region_Label = "Northeast"
) %>%
  bind_rows(data.frame(
    region = tolower(c("Ohio", "Indiana", "Illinois", "Michigan", "Wisconsin", "Minnesota", "Iowa",
                       "Missouri", "North Dakota", "South Dakota", "Nebraska", "Kansas")),
    Region_Label = "Midwest"
  )) %>%
  bind_rows(data.frame(
    region = tolower(c("Delaware", "Maryland", "Virginia", "West Virginia", "Kentucky", "North Carolina",
                       "South Carolina", "Georgia", "Florida", "Alabama", "Mississippi", "Tennessee",
                       "Arkansas", "Louisiana", "Oklahoma", "Texas")),
    Region_Label = "South"
  )) %>%
  bind_rows(data.frame(
    region = tolower(c("Montana", "Idaho", "Wyoming", "Colorado", "New Mexico", "Arizona", "Utah",
                       "Nevada", "Washington", "Oregon", "California", "Alaska", "Hawaii")),
    Region_Label = "West"
  ))

region_crime <- ncvs_data %>%
  group_by(Region_Label) %>%
  summarise(Total_Incidents = sum(NUM_INCIDENTS, na.rm = TRUE))

us_map_data <- us_states %>%
  left_join(region_mapping, by = "region") %>%
  left_join(region_crime, by = "Region_Label")

ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Crime Data Exploration"),
  navlistPanel(
    "Analysis options",
    
    tabPanel("Age Distribution",
             mainPanel(
               plotlyOutput("hist_variable"),
               width = 11
             )
    ),
    
    tabPanel("Seasonal Crime Trends",
             sidebarPanel(
               checkboxGroupInput("selected_quarters", "Select Quarters:", 
                                  choices = unique(ncvs_data$QUARTER),
                                  selected = unique(ncvs_data$QUARTER))
             ),
             mainPanel(
               textOutput("summary_quarters"),
               plotlyOutput("quarter_plot"),
               width = 11
             )
    ),
    
    tabPanel("Crime Rates Across U.S. Regions",
             sidebarPanel(
               selectInput("plot_type", "Select View:", 
                           choices = c("Map View", "Bar Chart View"),
                           selected = "Map View")
             ),
             mainPanel(
               plotlyOutput("region_plot"),
               width = 11
             )
    ),
    
    tabPanel("Age-Related Crime Trends",
             mainPanel(
               plotlyOutput("scatterplot_age"),
               width = 11
             )
    ),
    
    tabPanel("Crime Disparities by Income",
             mainPanel(
               plotlyOutput("heatmap_income"),
               width = 11
             )
    )
  )
)

server <- function(input, output) {
  
  output$hist_variable <- renderPlotly({
    gg <- ggplot(ncvs_data, aes(x = AGE)) +
      geom_histogram(fill = "#3498db", color = "white", bins = 30, alpha = 0.8) +
      labs(title = "Distribution of AGE", x = "AGE", y = "Count") +
      theme_minimal()
    ggplotly(gg)
  })
  
  output$summary_quarters <- renderText({
    filtered_data <- ncvs_data %>% filter(QUARTER %in% input$selected_quarters)
    paste("Total Incidents in Selected Quarters:", sum(filtered_data$NUM_INCIDENTS, na.rm = TRUE))
  })
  
  output$quarter_plot <- renderPlotly({
    filtered_data <- ncvs_data %>%
      filter(QUARTER %in% input$selected_quarters) %>%
      group_by(Quarter_Label) %>%
      summarise(Total_Incidents = sum(NUM_INCIDENTS, na.rm = TRUE))
    
    gg <- ggplot(filtered_data, aes(x = Quarter_Label, y = Total_Incidents, fill = Quarter_Label)) +
      geom_col(alpha = 0.7) +
      scale_fill_manual(values = c("#1E90FF", "#32CD32", "#DC143C", "#FFA500")) +
      labs(title = "Seasonal Crime Trends", x = "Quarter", y = "Total Incidents") +
      theme_minimal()
    
    ggplotly(gg)
  })
  
  output$region_plot <- renderPlotly({
    if (input$plot_type == "Map View") {
      gg <- ggplot(us_map_data, aes(x = long, y = lat, group = group, fill = Total_Incidents)) +
        geom_polygon(color = "white", size = 0.2) +
        scale_fill_continuous_sequential(palette = "Blues", na.value = "grey90") +
        labs(title = "Crime Rate Across U.S. Regions", fill = "Total Incidents") +
        theme_void() +
        theme(legend.position = "bottom")
      ggplotly(gg)
    } else {
      gg <- ggplot(ncvs_data %>% count(Region_Label), aes(x = Region_Label, y = n, fill = Region_Label)) +
        geom_col(show.legend = FALSE) +
        scale_fill_brewer(palette = "Blues") +
        labs(title = "Crime Rates Across U.S. Regions", x = "Region", y = "Total Incidents") +
        theme_minimal()
      ggplotly(gg)
    }
  })
  
  output$scatterplot_age <- renderPlotly({
    gg <- ggplot(ncvs_p3, aes(x = AGE, y = total_incident, color = Risk_Group, text = paste("Age:", AGE, "<br>Total Incidents:", total_incident))) +
      geom_point(size = 2, alpha = 0.7) +
      geom_smooth(aes(group = 1), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, color = "#FF7F00") +
      scale_x_continuous(breaks = seq(10, max(ncvs_p1$AGE, na.rm = TRUE), by = 5)) +
      scale_y_continuous(limits = c(0, max(ncvs_p1$total_incident, na.rm = TRUE) * 1.1)) +
      scale_color_manual(values = risk_colors, name = "Risk Group") +
      labs(
        title = "Crime Incidents by Age Group",
        subtitle = "High-Risk Group is defined as having total incidents above the median",
        x = "Age",
        y = "Total Number of Incidents"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11, face = "italic"),
        legend.position = "right",
        plot.background = element_rect(fill = "white", color = NA)
      )
  })
  
  output$heatmap_income <- renderPlotly({
    gg <- ggplot(ncvs_p2, aes(x = Income_Label, y = "", fill = total_incident)) +
      geom_tile(color = "white") +
      scale_fill_continuous_sequential(palette = "Blues") +  
      labs(
        title = "Crime Incidents Variation across Income Levels",
        subtitle = "Source: https://www.icpsr.umich.edu/web/NACJD/studies/38090/summary",
        x = "Income Level",
        y = "",
        fill = "Total Incidents"
      ) +
      theme_minimal() +
      theme(
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1)  
      )
    
    ggplotly(gg)
  })
}

shinyApp(ui = ui, server = server)
