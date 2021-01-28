library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)
library(ggplot2)
library(plotly)

df <- read_csv('data/processed/cleaned_data.csv') %>% filter(price <= 100, points >= 80) 


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(list(
        dbcRow(
            list(
                dbcCol(
                    list( 
                        htmlBr(),
                        htmlLabel('State Selection'),
                        dccDropdown(
                            id = 'state',
                            options = map(
                                    unique(df$state), function(x){
                                    list(label=x, value=x)
                                        }),
                            value = 'select your state',
                            placeholder = 'Select a State'
                        ),
                        htmlLabel('Wine Type'),
                        dccDropdown(
                            id = 'variety',
                            options = map(
                                    unique(df$variety), function(x){
                                    list(label=x, value=x)
                                        }),
                            value = 'select a variety',
                            placeholder = 'Select a Variety'
                        ),
                        htmlLabel('Price Range'),
                        dccRangeSlider(
                                id = 'price',
                                min = min(df$price),
                                max = max(df$price),
                                marks = list(
                                    "4" = "4$",
                                    "25" = "25$",
                                    "50" = "50$",
                                    "75" = "75$",
                                    "100" = "100$"
                                ),
                                value = list(min(df$price), max(df$price))
                            ),
                       htmlLabel('Points Range'),
                       dccRangeSlider(
                                id = 'points',
                                min = min(df$points),
                                max = max(df$points),
                                marks = list(
                                    "80" = "80",
                                    "85" = "85",
                                    "90" = "90",
                                    "95" = "95",
                                    "100" = "100"
                                ),
                                value = list(min(df$points), max(df$points))
                                ),
                        htmlLabel('Value Ratio'),
                        dccSlider(
                                id = 'points_ratio',
                                min = 1,
                                max = 10,
                                marks = list(
                                    "1" = "1°C",
                                    "5" = "5°C",
                                    "10" = "10°C"
                                ),
                                value = 5
                            )
                ), md = 4),
                dbcCol(
                    list(
                        htmlBr(),
                        htmlLabel('Map Should go here')
                   ), md = 8)
                )
            ),
        dbcRow(
            list(
                dbcCol(
                    list(
                         dccGraph(id='plots')
                        # htmlLabel('Scatter Plot | Bar Plot')
                    ), md = 4
                ),
            dbcCol(
                list(
                    dbcRow(
                        list(
                            htmlLabel('Top Card')
                        )
                    ),
                    dbcRow(
                        list(
                            htmlLabel('Bottom Card')
                        )
                    )
                ), md = 8
            )
        )
    )  # Change left/right whitespace for the container
)))

# app$callback(
#     list(output('toy', 'children')),
#     list(input('price', 'value')),
#     function(price){
#         return (list(price[1], price[2]))
#     })
app$callback(
    output('plots', 'figure'),
    list(input('state', 'value'),
         input('variety', 'value'),
         input('points', 'value'),
         input('price', 'value')),
    function(state_filter, variety_filter, points_filter, price_filter) {

        filtered_df <- filter(df, 
            points %in% seq(points_filter[1],points_filter[2]),
            price %in% seq(price_filter[1], price_filter[2]),
            state == state_filter,
            variety == variety_filter)

        scatter = ggplot(filtered_df) + aes(x = price, y = points) + geom_point()
        plotly(scatter)
    })

app$run_server(debug = T)