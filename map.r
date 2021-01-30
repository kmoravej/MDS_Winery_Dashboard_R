library(sf)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyr)
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

# loading a cleaned data

#here::here("data", "raw", "provincial_voting_results.csv")

data <- read_csv("data/processed/cleaned_data.csv") 

# grouping states data for map plot
states_data <- data %>%
  group_by(state) %>%
  drop_na(price) %>%
  summarise(mean_price = mean(price),
            mean_rating = mean(points),
            mean_value = mean(value),
            num_reviews = n()) 

# joing the state id

df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/2011_us_ag_exports.csv")
states_data <- left_join(states_data, df %>% select(c('code','state')), by = c("state" = "state"))
   

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

app$layout(
    dbcContainer(
        list(
            dccGraph(id='map'),
            dccGraph(id='bar'),
            dccDropdown(
                id='state',
                options = purrr::map(data %>% distinct(state) %>% pull(), function(state) list(label = state, value = state)),
                value='Oregon',
                multi=TRUE)
        )
    )
)
# callback for map
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
    output('scatter', 'figure'),
    list(input('state', 'value')),
    function(selected_state) {
        data = data %>% filter(state %in% selected_state)
        # data for scatter plot
        wine_data <- data %>%
            group_by(variety) %>%
            summarize(rating = mean(points),
            price = mean(price),
            value = mean(value)) %>%
            arrange(desc(rating)) %>%
            head(10) 


    }
)

#call bak for barchart
app$callback(
    output('bar', 'figure'),
    list(input('state', 'value')),
    function(selected_state) {
        data = data %>% filter(state %in% selected_state)
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

app$run_server(debug = T)
