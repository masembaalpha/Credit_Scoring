info_card <- function(title, value,
                      main_icon = "chart-line",
                      bg_color = "default", text_color = "default", sub_text_color = "success") {
    
    div(
        class = "panel panel-default",
        style = "padding: 0px;",
        div(
            class = str_glue("panel-body bg-{bg_color} text-{text_color}text-center"),
            h4(title),
            p(class = str_glue("fa-solid fa-{main_icon} fa-3x")),
            h5(value)
        )
    )
    
}
