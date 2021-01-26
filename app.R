library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(tidyverse)

df = read_csv('data/processed/cleaned_data.csv') %>% filter(price <= 100, points >= 80) 


app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(list(
        dbcRow(
            list(
                dbcCol(
                    list( 
                        htmlLabel('State Selection'),
                        dccDropdown(
                           # id = 'state-widget',
                            options = map(
                                    unique(df$state), function(x){
                                    list(label=x, value=x)
                                        }),
                            value = 'select your state',
                            placeholder = 'Select a State'
                        ),
                        htmlLabel('Wine Type'),
                        dccDropdown(
                            #id = 'wine_variety',
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
                                #id = 'points',
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
                               # id = 'points',
                                min = 1,
                                max = 10,
                                marks = list(
                                    "1" = "1°C",
                                    "5" = "5°C",
                                    "10" = "10°C"
                                ),
                                value = 5
                            )
                )),
                dbcCol(
                    list(
                        htmlLabel('Map Should go here')
                   ))
                )
            ),
        dbcRow(
            list(
                dbcCol(
                    list(
                        htmlLabel('Scatter Plot | Bar Plot')
                    )
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
                )
            )
        )
    )  # Change left/right whitespace for the container
)))

app$run_server(debug = T)