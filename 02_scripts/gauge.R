# with plotly

library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 750  # Replace this with the actual credit score

# Create a gauge chart
fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = credit_score,
    type = "indicator",
    mode = "gauge+number",
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(NULL, 850)),
        bar = list(color = "darkblue"),
        steps = list(
            list(range = c(0, 300), color = "red"),
            list(range = c(300, 579), color = "orange"),
            list(range = c(580, 669), color = "yellow"),
            list(range = c(670, 739), color = "lightgreen"),
            list(range = c(740, 799), color = "green"),
            list(range = c(800, 850), color = "darkgreen")
        )
    )
)

fig <- fig %>% layout(
    font = list(color = "black", family = "Arial"),
    xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
)

# Display the chart
fig


# Sample data (replace this with your actual data)
credit_score <- 750  # Replace this with the actual credit score

# Create a gauge chart with pointer
fig2 <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = credit_score,
    type = "indicator",
    mode = "gauge",
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(NULL, 850)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "red", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(0, 300), color = "red"),
            list(range = c(300, 579), color = "orange"),
            list(range = c(580, 669), color = "yellow"),
            list(range = c(670, 739), color = "lightgreen"),
            list(range = c(740, 799), color = "green"),
            list(range = c(800, 850), color = "darkgreen")
        )
    )
)

fig2 <- fig2 %>% layout(
    font = list(color = "black", family = "Arial"),
    xaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE),
    yaxis = list(showticklabels = FALSE, showgrid = FALSE, zeroline = FALSE)
)

# Display the chart
fig2


library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 750  # Replace this with the actual credit score

# Create a gauge chart with a pointer
figx <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(NULL, 850)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(0, 300), color = "red"),
            list(range = c(300, 579), color = "orange"),
            list(range = c(580, 669), color = "yellow"),
            list(range = c(670, 739), color = "lightgreen"),
            list(range = c(740, 799), color = "green"),
            list(range = c(800, 850), color = "darkgreen")
        )
    )
)

# Display the chart
figx


library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 750  # Replace this with the actual credit score

# Create a gauge chart with a pointer and categories
fig.y <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(NULL, 850)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(0, 579), color = "red", category = "Very Poor"),
            list(range = c(580, 669), color = "orange", category = "Bad"),
            list(range = c(670, 739), color = "yellow", category = "Average"),
            list(range = c(740, 799), color = "lightgreen", category = "Good"),
            list(range = c(800, 850), color = "green", category = "Very Good")
        )
    )
)

# Define custom category for the current credit score
category <- switch(
    TRUE,
    credit_score < 580, "Very Poor",
    credit_score < 670, "Bad",
    credit_score < 740, "Average",
    credit_score < 800, "Good",
    TRUE, "Excellent"
)

# Add category to the chart title
fig.y <- fig.y %>% layout(
    title = list(text = paste("Credit Score:", category))
)

# Display the chart
fig.y


library(plotly)

# Define the credit score
credit_score <- 700  # Replace this with the actual credit score

# Create a gauge chart
fig.z <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900), tickvals = c(300, 560, 660, 725, 760, 900), ticktext = c("Poor", "Fair", "Good", "Very Good", "Excellent")),
        bar = list(color = "darkblue"),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent")
        ),
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        )
    )
)

# Display the chart
fig.z


library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 700  # Replace this with the actual credit score

# Create a gauge chart with specified categories
fig <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent")
        )
    )
)

# Define the category for the current credit score
category <- switch(
    TRUE,
    credit_score < 560, "Poor",
    credit_score < 660, "Fair",
    credit_score < 725, "Good",
    credit_score < 760, "Very Good",
    TRUE, "Excellent"
)

# Add category to the chart title
# fig <- fig %>% layout(
#     title = list(text = paste("Credit Score:", category))
)

# Display the chart
fig


# Sample data (replace this with your actual data)
credit_score <- "."  # Replace this with the actual credit score
low <- min(df_scored_train$SCORE)
high <- max(df_scored_train$SCORE)
# Create a gauge chart with specified categories
fig.s <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(low, high)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(low, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, high), color = "green", category = "Excellent")
        )
    )
)

# Define the category for the current credit score
category <- switch(
    TRUE,
    credit_score < 560, "Poor",
    credit_score < 660, "Fair",
    credit_score < 725, "Good",
    credit_score < 760, "Very Good",
    TRUE, "Excellent"
)

# Add category to the chart title
# fig.s <- fig.s %>% layout(
#     title = list(text = paste("Credit Score:", category))
)

# Display the chart
fig.s


hist(df_scored_train$SCORE)



library(plotly)

# Sample data (replace this with your actual data)
credit_score <- ".."  # Replace this with the actual credit score

# Create a gauge chart with specified categories
fig3 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent")
        )
    )
)
# 
# # Define the category for the current credit score
# category <- switch(
#     TRUE,
#     credit_score < 560, "Poor",
#     credit_score < 660, "Fair",
#     credit_score < 725, "Good",
#     credit_score < 760, "Very Good",
#     TRUE, "Excellent"
)

# Add category and score to the arc
category_label <- paste(category, ":", credit_score)

# Add category and score to the chart title
fig3 <- fig3 %>% layout(
    title = list(text = paste("Credit Score:", category_label)),
    annotations = list(
        text = category_label,
        showarrow = TRUE,
        x = 0.5,
        y = 0.4
    )
)

# Display the chart
fig3

library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 700  # Replace this with the actual credit score

# Create a gauge chart with specified categories
fig4 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor", label = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair", label = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good", label = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good", label = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent", label = "Excellent")
        )
    )
)

# Define the category for the current credit score
category <- switch(
    TRUE,
    credit_score < 560, "Poor",
    credit_score < 660, "Fair",
    credit_score < 725, "Good",
    credit_score < 760, "Very Good",
    TRUE, "Excellent"
)

# Add category and score to the chart title
fig4 <- fig4 %>% layout(
    title = list(text = paste("Credit Score:", category, "(", credit_score, ")"))
)

# Display the chart
fig4


library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 700  # Replace this with the actual credit score

# Create a gauge chart with specified categories
fig5 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent")
        )
    )
)

# Define text annotations for each step
annotations <- list(
    list(
        x = 0.5,
        y = 0.5,
        text = "Poor",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14, color = "darkred")
    ),
    list(
        x = 0.75,
        y = 0.5,
        text = "Fair",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14, color = "orange")
    ),
    list(
        x = 0.85,
        y = 0.5,
        text = "Good",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14, color = "yellow")
    ),
    list(
        x = 0.92,
        y = 0.5,
        text = "Very Good",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14, color = "lightgreen")
    ),
    list(
        x = 1,
        y = 0.5,
        text = "Excellent",
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14, color = "green")
    )
)

# Add text annotations to the chart
fig5 <- fig5 %>% layout(annotations = annotations)

# Display the chart
fig5


library(plotly)

# Sample data (replace this with your actual data)
credit_score <- 800  # Replace this with the actual credit score

# Create a gauge chart with specified categories
fig6 <- plot_ly(
    type = "indicator",
    mode = "gauge+number",
    value = credit_score,
    title = list(text = "Credit Score"),
    gauge = list(
        axis = list(range = list(300, 900)),
        bar = list(color = "darkblue"),
        shape = "angular",
        threshold = list(
            line = list(color = "black", width = 4),
            thickness = 0.75,
            value = credit_score
        ),
        steps = list(
            list(range = c(300, 559), color = "darkred", category = "Poor"),
            list(range = c(560, 659), color = "orange", category = "Fair"),
            list(range = c(660, 724), color = "yellow", category = "Good"),
            list(range = c(725, 759), color = "lightgreen", category = "Very Good"),
            list(range = c(760, 900), color = "green", category = "Excellent")
        )
    )
)

# Define text annotations for each step
annotations <- lapply(seq_along(fig$gauge$steps), function(i) {
    step <- fig$gauge$steps[[i]]
    category <- step$category
    label <- ifelse(i == length(fig$gauge$steps), "Excellent", category)
    position <- 0.2 + 0.6 * (i - 1) / length(fig$gauge$steps)
    list(
        x = position,
        y = 0.5,
        text = label,
        showarrow = FALSE,
        xref = "paper",
        yref = "paper",
        font = list(size = 14)
    )
})

# Add text annotations to the chart
fig6 <- fig6 %>% layout(annotations = annotations)

# Display the chart
fig6

