library(shiny)
library(DT)
library(tidyverse)

ui <- fluidPage(

    # Application title
    titlePanel("2018 Olympics - Women's Hockey Game Score Plotter"),

    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "team",
                        label = "Team",
                        choices = unique(rosters$Team)),
            selectInput(inputId = "player",
                        label = "Player",
                        choices = unique(rosters$Player)),
            br(),
            actionButton(inputId = "update",
                         label = "Generate plot"),
            br(),
            br(),
            htmlOutput(outputId = "totalGS"),
            br(),
            br(),
            helpText(
                "This tool plots Game Score for women's hockey",
                "games from the 2018 Winter Olympics."
            ),
            br(),
            helpText(
                "Click the Explanation tab to read more about how this is calculated."
            ),
            br(),
            helpText(
                "Created by Trevor Greissinger using open-source packages in R for the 2021 Big Data Cup
                using data from Stathletes.  For information about this data set, please see the legal agreement
                ",tags$a(href="https://github.com/bigdatacup/Big-Data-Cup-2021/blob/main/legal.md","here.")
            )
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            div(
                style = "position:relative",
            tabsetPanel(
                type = "tabs",
                tabPanel("Game Score Plots",
                         plotOutput("rinkPlot",
                                    height="auto",
                                    hover = hoverOpts("plot_hover",delay=100,delayType="debounce")),
                         uiOutput("hover_info"),
                         DT::dataTableOutput("skaterTable")),
                tabPanel("Explanation",withMathJax(includeMarkdown("writeup.Rmd")))
            ))
        )
    )
)

server <- function(input, output, session) {
    #load RData
    
    observeEvent(input$team,{
        updateSelectInput(session,'player',
                          choices = unique(rosters %>%
                                               filter(Team == input$team) %>%
                                               .$Player))
    })
    
    total.gs <- eventReactive(input$update, {
        womens.final %>%
            filter(Player == input$player) %>%
            group_by(Player) %>%
            summarize(total.gs = sum(weight)) %>%
            .$total.gs %>%
            round(3)
    })
    
    output$totalGS <- renderText(
        paste0("<b>Total Game Score: </b>",total.gs())
    )
    
    rink.img <- png::readPNG("rink.png")
    rink.plot <- eventReactive(input$update, {
        womens.final %>%
            filter(Player == input$player) %>%
            mutate(weight = round(weight,3)) %>%
            ggplot() +
            ggpubr::background_image(rink.img) +
            geom_point(mapping = aes(x = `X Coordinate`,
                                     y = `Y Coordinate`,
                                     size = log(abs(weight)),
                                     color = weight > 0),
                       show.legend = FALSE) +
            coord_cartesian(xlim = c(0,200),
                            ylim = c(0,85),
                            expand = TRUE)
    })
    output$rinkPlot <- renderPlot({
        rink.plot()
    },
        height = function() {
            session$clientData$output_rinkPlot_width / 2.5
        })
    
    output$hover_info <- renderUI({
        hover <- input$plot_hover
        point <- nearPoints(womens.final %>%
                                filter(Player == input$player), 
                            hover, 
                            threshold = 5, 
                            maxpoints = 1, 
                            addDist = TRUE,
                            xvar = "X Coordinate",
                            yvar="Y Coordinate")
        if (nrow(point) == 0) return(NULL)
        
        left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
        top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
        top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Event: </b>",point$Event,"<br/>",
                          "<b> GS Weight: ",point$weight,"<br/>")))
        )
    })
    
    skater.table <- eventReactive(input$update, {
        womens.final %>%
            filter(Player == input$player) %>%
            DT::datatable() %>%
            DT::formatRound(columns = 6,
                            digits = 3)
    })
    output$skaterTable <- renderDataTable({
        skater.table()
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
