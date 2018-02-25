
# Last pakker
library(shiny)
library(shinydashboard)
library(broom)
library(ggplot2)

# Minimum eksempel --------------------------------------------------------

# Definer UI for et dashboard
ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

# Definer server (tom her)
server <- function(input, output) {
}

# Kjør 
shinyApp(ui = ui, server = server)


# Fyll inn med modellresultat ----------------------------------------------------------------

ui <- dashboardPage(
  # Legg til en tittel
  dashboardHeader(title = "Airquality-modeller"),
  dashboardSidebar(),
  
  # Legg til en tabell, merk at vi referer til en tabell fra "server"
  dashboardBody(fluidRow(
    box(
      title = "Modell output",
      width = 350,
      tableOutput("coeff_table")
    )
  ))
)

# Definer server, husk at tabellnavn må henge sammen med output fra ui
server <- function(input, output) {
  
  # Henter data
  df <- airquality
  
  # Replace NA med monthly mean
  df <- df %>%
    group_by(Month) %>% 
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  # Lag modell
  modell <- lm(Ozone ~ ., data = df)
  modell_coeff <- tidy(modell)
  
  # Put tabell i en "render"-funksjon for å vise frem i app. 
  output$coeff_table <- renderTable({
    modell_coeff
  })
}

# Kjør 
shinyApp(ui = ui, server = server)


# Legg til plot -----------------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Airquality-modeller"),
  dashboardSidebar(),
  dashboardBody(
    
    # Legg til plott
    fluidRow(box(heigth = 350,
                 title = "Ozone vs temperatur",
                 plotOutput("predPlot"))),
    
    # Legg til table
    fluidRow(box(title = "Modell output",
                 width = 300,
                 tableOutput("coeff_table"
                 )))
  )
)

# Definer server
server <- function(input, output) {
  
  df <- airquality
  
  # Replace NA med monthly mean
  df <- df %>%
    group_by(Month) %>% 
    mutate_all(funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
  
  modell <- lm(Ozone ~., data = df)
  
  df$predicted <- predict(modell, newdata = df)
  
  # Plot: geom_smooth
  output$predPlot <- renderPlot({
    ggplot(df, aes(x = Temp, y = Ozone)) +
      geom_point() +
      geom_smooth()
  })
  
  # Tabell med koeffisienter
  modell_coeff <- tidy(modell)
  output$coeff_table <- renderTable({
    modell_coeff
  })
  
}

# Kjør 
shinyApp(ui = ui, server = server)

# Legg til widget for å endre plottet -------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Airquality-modeller"),
  dashboardSidebar(),
  dashboardBody(
    
    # Legg til plott + widgets
    fluidRow(box(heigth = 350,
                 title = "Univariat plot",
                 plotOutput("predPlot")),
             
             # Widget: Velg variabel
             box(Title = "Velg variabel",
                 selectInput(
                   "var",
                   label = "Velg en variabel å vise",
                   choices = list("Wind", "Temp", "Solar.R"),
                   selected = "Temp"))),
    
    # Legg til table
    fluidRow(box(title = "Modell output",
                 width = 300,
                 tableOutput("coeff_table"
                 )))
  )
)

# Definer server
server <- function(input, output) {
  
  df <- airquality
  
  modell <- lm(Ozone ~., data = df)
  
  df$predicted <- predict(modell, newdata = df)
  
  # Plot: Predicted vs actual
  # Merk at vi nå bytter til aes_string fordi vi mottar tekst-verdier fra input
  # NB: ALT i koden som skal reagere på user-input må være inne i en reaktiv-expression!
  output$predPlot <- renderPlot({
    ggplot(df, aes_string(x = input$var, y = "Ozone")) +
      geom_point(na.rm = T) +
      geom_smooth(na.rm = T)
  })
  
  # Tabell med koeffisienter
  modell_coeff <- tidy(modell)
  output$coeff_table <- renderTable({
    modell_coeff
  })
  
}

# Kjør 
shinyApp(ui = ui, server = server)


# Legg til mer i sidebar --------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Airquality-modeller"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Validering", tabName = "validering", icon = icon("th"))
    )
  ),
  
  # Legg til innhold i første tab
  dashboardBody(
    tabItems(
      tabItem(
        # Først: innholdet i "dashboard"
        tabName = "dashboard",
             
             # Legg til plott + widgets
             fluidRow(
               box(heigth = 350,
                   title = "Univariat plot",
                   plotOutput("predPlot")),
               
               # Widget: Velg variabel
               box(
                 Title = "Velg variabel",
                 selectInput(
                   "var",
                   label = "Velg en variabel å vise",
                   choices = list("Wind", "Temp", "Solar.R"),
                   selected = "Temp"
                 )
               )
             ),
             
             # Legg til table
             fluidRow(box(
               title = "Modell output",
               width = 300,
               tableOutput("coeff_table")
             ))),
      
      # Legg til innhold i andre tab
      tabItem(tabName = "validering",
              h2("Validering content"))
  )))

# Definer server
server <- function(input, output) {
  
  df <- airquality
  
  modell <- lm(Ozone ~., data = df)
  
  df$predicted <- predict(modell, newdata = df)
  
  # Plot: Predicted vs actual
  # Merk at vi nå bytter til aes_string fordi vi mottar tekst-verdier fra input
  output$predPlot <- renderPlot({
    ggplot(df, aes_string(x = input$var, y = "Ozone")) +
      geom_point(na.rm = T) +
      geom_smooth(na.rm = T)
  })
  
  # Tabell med koeffisienter
  modell_coeff <- tidy(modell)
  output$coeff_table <- renderTable({
    modell_coeff
  })
  
}

# Kjør 
shinyApp(ui = ui, server = server)


# Utfyll "validering" -----------------------------------------------------

ui <- dashboardPage(
  dashboardHeader(title = "Airquality-modeller"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Validering", tabName = "validering", icon = icon("th"))
    )
  ),
  
  # Legg til innhold i første tab
  dashboardBody(
    tabItems(
      tabItem(
        # Først: innholdet i "dashboard"
        tabName = "dashboard",
        
        # Legg til plott + widgets
        fluidRow(
          box(heigth = 350,
              title = "Univariat plot",
              plotOutput("predPlot")),
          
          # Widget: Velg variabel
          box(
            Title = "Velg variabel",
            selectInput(
              "var",
              label = "Velg en variabel å vise",
              choices = list("Wind", "Temp", "Solar.R"),
              selected = "Temp"
            )
          )
        ),
        
        # Legg til table
        fluidRow(box(
          title = "Modell output",
          width = 300,
          tableOutput("coeff_table")
        ))),
      
      # Legg til innhold i andre tab
      tabItem("validering",
              fluidRow(
                valueBoxOutput("R2"),
                valueBoxOutput("AIC"),
                valueBoxOutput("logLik"),
                plotOutput("perf_time")
              ))
    )))

# Definer server
server <- function(input, output) {
  
  df <- airquality
  
  modell <- lm(Ozone ~., data = df)
  
  df$predicted <- predict(modell, newdata = df)
  
  performance <- glance(modell)
  
  # Plot: Predicted vs actual
  # Merk at vi nå bytter til aes_string fordi vi mottar tekst-verdier fra input
  output$predPlot <- renderPlot({
    ggplot(df, aes_string(x = input$var, y = "Ozone")) +
      geom_point(na.rm = T) +
      geom_smooth(na.rm = T)
  })
  
  # Tabell med koeffisienter
  modell_coeff <- tidy(modell)
  output$coeff_table <- renderTable({
    modell_coeff
  })
  
  # Rene talloutput
  output$R2 <- renderValueBox({
    valueBox(
      value = formatC(performance$r.squared, digits = 3, format = "f"),
      subtitle = "R-squared",
      icon = icon("area-chart"),
      color = if (performance$r.squared >= 0.5) "green" else "red"
    )
  })
  
  output$AIC <- renderValueBox({
    valueBox(
      value = formatC(performance$AIC, digits = 0, format = "f"),
      subtitle = "AIC",
      icon = icon("download"),
      color = if (performance$AIC <= 900) "green" else "red"
    )
  })
  
  output$logLik <- renderValueBox({
    valueBox(
      value = formatC(performance$logLik, digits = 0, format = "f"),
      subtitle = "Log Likelihood",
      icon = icon("balance-scale"),
      color = if (performance$logLik <= -500) "green" else "yellow"
    )
  })
  
  
  # Plot performance over tid (simulert)
  Perf_time <- tibble(time = seq(1, 30),
                      R2 = runif(30, 0.7, 1))
  
  output$perf_time <- renderPlot({
    ggplot(Perf_time, aes(x = time, y = R2)) +
      geom_line(color = "cornflowerblue") +
      ylim(0.5, 1) +
      ggtitle("Utvikling i forklaringsgrad over tid")
  })
  
}

# Kjør 
shinyApp(ui = ui, server = server)

