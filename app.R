library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)

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
                            options = list(list(label = "Country", value = "D"),
                                           list(label = "San Francisco", value = "SF")),
                            value = 'select your state',
                            placeholder = 'Select a State'
                        ),
                        htmlLabel('Wine Type'),
                        dccDropdown(
                            #id = 'wine_variety',
                            options = list(list(label = "Wine", value = "_"),
                                           list(label = "Red", value = "_")),
                            value = 'select a variety',
                            placeholder = 'Select a Variety'
                        ),
                        htmlLabel('Price Range'),
                        dccSlider(
                                id = 'price',
                                min = 1,
                                max = 10,
                                marks = list(
                                    "1" = "1°C",
                                    "5" = "5°C",
                                    "10" = "10°C"
                                ),
                                value = 5
                            ),
                       htmlLabel('Points Range'),
                       dccSlider(
                                #id = 'points',
                                min = 1,
                                max = 10,
                                marks = list(
                                    "1" = "1°C",
                                    "5" = "5°C",
                                    "10" = "10°C"
                                ),
                                value = 5
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