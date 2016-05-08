# 2015 Contracting Dashboard
# app.R

library(dplyr)
library(ggplot2)
library(readr)
library(shiny)
library(stringr)
library(tidyr)


fpds <- read_rds("data/fpds.rds" )
agencies <- levels(factor(fpds$maj_agency_cat1))

ui <- fluidPage(
    includeCSS("www/bootstrap.css"),

    titlePanel("Trends in Federal Contracting",
    windowTitle = "No VCAs were harmed in the production of this dashboard."),

    fluidRow(
        column(width=3,
            wellPanel(

                selectInput("agency",
                    label = "Select an agency:",
                    choices = agencies,
                    selected = "GAO"
                )
            )
        )
    ),

    navlistPanel(id = "which_panel", widths = c(3,9) ,

        tabPanel("Products & Services",
            splitLayout(
                plotOutput("psc_plot"),
                dataTableOutput("psc_table")
            )
        ),

        tabPanel("Contract Pricing Types",
            splitLayout(
                plotOutput("pricetype_plot"),
                dataTableOutput("pricetype_table")
            )
        ),

        tabPanel("Competed Contracts",
            splitLayout(
                plotOutput("comp_plot"),
                dataTableOutput("comp_table")
            )
        ),

        tabPanel("Agency Breakdown",
            splitLayout(
                plotOutput("agency_plot"),
                dataTableOutput("agency_table")
            )
        ),

        tabPanel("Top Products & Services",
            splitLayout(
                plotOutput("product_plot"),
                plotOutput("service_plot")
            )
        )
    )

    # fluidRow(
        # column(width = 1,
            # downloadButton(
                # outputId = "graphicDownload", label = "graphic:"
            # )
        # ),
        # column(width = 1,
            # downloadButton(
                # outputId = "dataDownload", label = "data:"
            # )
        # )
    # )
)

server <- shinyServer(function(input, output) {

    psc_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency) %>%
        group_by(psc, year) %>%
        summarise(psc_spend = sum(round(dollarsobligated/1E7,digits=2), na.rm=T)) %>%
        spread(psc, psc_spend) %>%
        mutate(percent_services = round((Services/(Products+Services))*100, digits = 2))
    })

    output$psc_plot <- renderPlot({

        ggplot(psc_data(), aes(x=year, y=percent_services ),
            environment = environment()) + geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            labs(title = "Percent of Contract Spending on Services",
                x= "Fiscal Year", y = "% Services")
    })

    output$psc_table <- renderDataTable({
        psc_data()
    })

    pricetype_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency) %>%
        group_by(pricetype, year) %>%
        summarise(pricetype_spend = sum(round(dollarsobligated/1E7,digits=2), na.rm=T)) %>%
        spread(pricetype, pricetype_spend) %>%
        mutate(percent_fixed_price = round(
            (`Fixed Price`/(`Fixed Price` + `Other`+`Cost Reimbursable` + `Time & Materials`))*100
            , digits = 2)
        )
    })

    output$pricetype_plot <- renderPlot({

        ggplot(pricetype_data(), aes(x=year, y=percent_fixed_price ),
            environment = environment()) + geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            labs(title = "Percent of Spending on Fixed Price Contracts",
                x= "Fiscal Year", y = "% Fixed Price")
    })

    output$pricetype_table <- renderDataTable({
        pricetype_data()
    })

    comp_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency) %>%
        group_by(comp, year) %>%
        summarise(comp_spend = sum(round(dollarsobligated/1E7,digits=2), na.rm=T)) %>%
        spread(comp, comp_spend) %>%
        mutate(percent_competed = round((Competed/(Competed+`Not Competed`))*100, digits = 2))
    })

    output$comp_plot <- renderPlot({

        ggplot(comp_data(), aes(x=year, y=percent_competed ),
            environment = environment()) + geom_line() +
            scale_y_continuous(limits = c(0, 100)) +
            labs(title = "Percent of Contract Spending Competed",
                x= "Fiscal Year", y = "% Competed")
    })

    output$comp_table <- renderDataTable({
        comp_data()
    })

    agency_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency, year==2015, mod_agency1!=maj_agency_cat1) %>%
        group_by(mod_agency1) %>%
        summarise(spend = sum(round(dollarsobligated/1E7,digits=2), na.rm=T))
    })

    output$agency_plot <- renderPlot({
        ggplot(agency_data(), aes(reorder(mod_agency1, spend), y=spend ),
            environment = environment()) + geom_point(stat="identity") + coord_flip() +
            labs(title = "Total Spending by Agency, FY 2015",
                x= "Agency", y = "Spending (FY 2015 millions)")
    })

    output$agency_table <- renderDataTable({
        agency_data()
    })

    product_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency, year==2015, psc=="Products") %>%
        group_by(level1psc) %>%
        summarise(spend = sum(round(dollarsobligated/1E7,digits=3), na.rm=T)) %>%
        top_n(.,10, spend)
    })

    output$product_plot <- renderPlot({
        ggplot(product_data(), aes(reorder(level1psc, spend), y=spend ),
            environment = environment()) + geom_point(stat="identity") + coord_flip() +
            labs(title = "Top Products, FY 2015",
                x= "Product", y = "Spending (FY 2015 millions)") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
    })


    service_data <- reactive({
        fpds %>%
        filter(maj_agency_cat1==input$agency, year==2015, psc=="Services") %>%
        group_by(level1psc) %>%
        summarise(spend = sum(round(dollarsobligated/1E7,digits=3), na.rm=T)) %>%
        top_n(.,10, spend)
    })

    output$service_plot <- renderPlot({
       ggplot(service_data(), aes(reorder(level1psc, spend), y=spend ),
            environment = environment()) + geom_point(stat="identity") + coord_flip() +
            labs(title = "Top Services, FY 2015",
                x= "Service", y = "Spending (FY 2015 millions)") +
            scale_x_discrete(labels = function(x) str_wrap(x, width = 30))
    })

})

shinyApp(ui = ui, server = server)
