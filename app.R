library("sf")
library("maps")
library("rnaturalearth")
#library("rnaturalearthdata")
library("tidyr")
library(dash)
library(dashCoreComponents)
library(dashHtmlComponents)
library(dashBootstrapComponents)
library(ggplot2)
library(plotly)
library(readr)
library(dashTable)
library(stringr)
library(scales)
library(rlang)
library(dplyr)
library(purrr)

df <- read_csv('data/processed/cleaned_data.csv') %>% filter(price <= 100, points >= 80) %>% mutate(log_value = log(value))


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
                            value = 'Oregon',
                            multi = TRUE, 
                            placeholder = 'Select a State'
                        ),
                        htmlLabel('Wine Type'),
                        dccDropdown(
                            id = 'variety',
                            options = map(
                                    unique(df$variety), function(x){
                                    list(label=x, value=x)
                                        }),
                            value = list('Red Blend', 'Chardonnay', 'Merlot'),
                            multi = TRUE,
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
                        dccRangeSlider(
                                id = 'value_ratio',
                                min = 0 ,
                                max = 22,
                                marks = list(
                                    "0" = 'low',
                                    '11' = 'medium',
                                    '22' = 'high'
                                ),
                                value = list(0, max(df$value))
                            )
                ), md = 4),
                dbcCol(
                    list(
                        htmlBr(),
                        dccGraph(id='map')
                   ), md = 8)
                )
            ),
        dbcRow(
            list(
                dbcCol(
                    list(
                         dccGraph(id='bar')
                        # htmlLabel('Scatter Plot | Bar Plot')
                    ), md = 8
                ),
            dbcCol(
                list(
                    htmlBr(),
                    dbcRow(
                        list(
                            dbcCol(
                                list(
                                    htmlH4('Top Wine Value:'),
                                    htmlH5(id = 'value_name'),
                                    htmlH6(id = 'value_number'),
                                    htmlH6(id = 'value_price'))
                            )
                        )
                    ),
                    htmlBr(),
                    htmlBr(),
                    htmlBr(),
                    dbcRow(
                        list(
                            dbcCol(
                                list(
                                    htmlH4('Top Wine Score:'),
                                    htmlH5(id = 'points_name'),
                                    htmlH6(id = 'points_number'),
                                    htmlH6(id = 'points_price'))
                            )
                        )
                    )
                ), md = 4
            )
        )
    )  # Change left/right whitespace for the container
)))

app$callback(
    output('map', 'figure'),
    list(input('state', 'value')),
    function(selected_state) {
        states_data = states_data %>% filter(state %in% selected_state)
        wine_colors <- c('#C7DBEA', '#CCCCFF', '#C1BDF4',
                        '#B6AEE9', '#948EC0', '#8475B2',
                        '#735BA4', '#624296', '#512888')
        p <- plot_ly(states_data, 
        type = 'choropleth', locationmode = 'USA-states',
        z = ~num_reviews, locations = ~code, color = ~num_reviews, colors = wine_colors)
        p <- p %>% layout(geo = list(scope = 'usa', projection = list(type = 'albers usa')),
             title = 'USA exports')
    }
)
app$callback(
    list(output('points_number', 'children'),
         output('points_name', 'children'),
         output('points_price', 'children')),
    list(input('state', 'value'),
         input('variety', 'value'),
         input('price', 'value'),
         input('points', 'value'),
         input('value_ratio', 'value')),
    function(input_value, input_value2, price_range, points_range, value_range) {

        df_filtered <- df %>% 
            filter(state %in% input_value,
                variety %in% input_value2,
                between(value, value_range[1], value_range[2]),
                between(price, price_range[1], price_range[2]),
                between(points, points_range[1], points_range[2])) %>% 
            arrange(desc(points)) %>% 
            separate(title, c("title", "throwaway"), ' \\(') %>%
            select(points, title, price) %>% 
            slice(1) %>%
            mutate(points = paste('Score:', points, 'pts'), price = paste('Price:', price, '$'))
        return(list(df_filtered[[1]], df_filtered[[2]], df_filtered[[3]]))
    })



app$callback(
    list(output('value_number', 'children'),
         output('value_name', 'children'),
         output('value_price', 'children')),
    list(input('state', 'value'),
         input('variety', 'value'),
         input('price', 'value'),
         input('points', 'value'),
         input('value_ratio', 'value')),
    function(selected_state, selected_variety, price_range, points_range, value_range) {

        df_filtered <- df %>% 
            filter(state %in% selected_state,
                variety %in% selected_variety,
                between(price, price_range[1], price_range[2]),
                between(value, value_range[1], value_range[2]),
                between(points, points_range[1], points_range[2])) %>% 
            arrange(desc(value)) %>% 
            separate(title, c("title", "throwaway"), ' \\(') %>%
            select(value, title, price) %>% 
            slice(1) %>% 
            mutate(value = paste('Value Ratio:', round(value, 2)), price = paste('Price:', price, '$'))
        return(list(df_filtered[[1]], df_filtered[[2]], df_filtered[[3]]))
    })

app$callback(
    output('bar', 'figure'),
    list(input('state', 'value'),
         input('variety', 'value'),
         input('price', 'value'),
         input('points', 'value'),
         input('value_ratio', 'value')),
    function(selected_state, selected_variety, price_range, points_range, value_range) {
        data = data %>% filter(state %in% selected_state, 
                variety %in% selected_variety,
                between(price, price_range[1], price_range[2]),
                between(points, points_range[1], points_range[2]),
                between(value, value_range[1], value_range[2]))
        # data for bar plot
        wine_data <- data %>%
            group_by(variety) %>%
            summarize(rating = mean(points),
            price = mean(price),
            value = mean(value)) %>%
            arrange(desc(rating)) %>%
            head(10) %>%
            mutate(highlight_flag = ifelse(rating == max(rating), T, F))
        
        bar_plot <- ggplot(wine_data, aes(x=reorder(variety, -rating),
                                     rating, fill= variety)) +
                    geom_bar(stat='identity', show.legend = FALSE) +
                    scale_y_continuous(limits = c(min(wine_data$rating),
                                        max(wine_data$rating)),
                                        oob=rescale_none) +
                    xlab("Wine Variety") +
                    ylab("Rating") +
                    ggtitle(paste0("Rating", ' by ', "Wine Variety")) +
                    theme_bw() +
                    ggthemes::scale_fill_tableau() +
                    theme(axis.text.x = element_text(angle=60, hjust=1),
                    legend.position = 'none',
                    panel.grid.major = element_blank()) 
                    

        scatter_plot <- ggplot(wine_data) +
                    aes(x = price,
                        y = value,
                        color = variety) +
                    geom_point(size = 2) +
                    ggthemes::scale_color_tableau()
                    #ggplotly(p, tooltip = 'variety') %>% layout(dragmode = 'select')

        subplot(ggplotly(bar_plot), ggplotly(scatter_plot), nrows = 1) %>% layout(dragmode = 'select')
    
        #ggplotly(new_plot, tooltip = 'rating') %>% layout(dragmode = 'select')
    }
)


# app$callback(
#     list(output('value_card', 'children'),
#          output('points_card', 'children')),
#     list(input('state', 'value'),
#          input('variety', 'value'),
#          input('price', 'value')),
#     function(input_value, input_value2, price_range) {

#         df_filtered <- df %>% 
#             filter(state %in% input_value,
#                 variety %in% input_value2) %>% 
#             arrange(desc(price)) %>% 
#             select(price, title) %>% 
#             slice(1)
#         return(list(df_filtered[[1]], df_filtered[[2]]))
#     })

# app$callback(
#     list(output('value_card', 'children')),
#     list(input('variety', 'value'),
#         input('state', 'value')),
#     function(variety_selection, state_selection){
#         variety <- variety_selection
#         return (variety)
# })



# app$callback(
#     list(output('toy', 'children')),
#     list(input('price', 'value')),
#     function(price){
#         return (list(price[1], price[2]))
#     })

# app$callback(
#     output('plots', 'figure'),
#     list(input('state', 'value'),
#          input('variety', 'value'),
#          input('points', 'value'),
#          input('price', 'value')),
#     function(state_filter, variety_filter, points_filter, price_filter) {

#         # filtered_df <- filter(df, 
#         #     points %in% seq(points_filter[1],points_filter[2]),
#         #     price %in% seq(price_filter[1], price_filter[2]),
#         #     state == state_filter,
#         #     variety == variety_filter)

#         scatter = ggplot(df) + aes(x = price, y = points) + geom_point()
#         plotly(scatter)
#     })

app$run_server(debug = T)