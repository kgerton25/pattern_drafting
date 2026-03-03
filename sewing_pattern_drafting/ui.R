title = "Sewing Pattern DraftR"
# Header ----
header <- bs4DashNavbar(
    
    tags$img(src = "header_sewing_machine.png"),
    
    div(
        tags$h2(
            class = "primary-title",
            align = "left",
            style = "margin-top: 10px; margin-right: 50px; font-size: 50px; font-weight: bold",
            "Sewing Pattern DraftR"),
        tags$h3(
            class = "primary-subtitle",
            align = "right",
            style = "margin-top: 10px; margin-right: 50px; font-size: 25px; font-style: italic",
            "Draft Customized Sewing Patterns in R"
        )
    )
    
)
# Sidebar ----
sidebar <- bs4DashSidebar(
    
    sidebarMenu(
        id = "mainmenu",
        menuItem(
            "Welcome!",
            tabName = "welcome"
        ),
        menuItem(
            "My Measurements",
            tabName = "measurements"
        ),
        menuItem(
            "Patterns",
            tabName = "patterns"
        ),
        menuItem(
            "Gallery",
            tabName = "gallery"
        )
    )
)
# App Body ----
body <- bs4DashBody(
    
    tabItems(
        ## Welcome Tab ----
        tabItem(
            tabName = "welcome",
            fluidRow(
                collapsible = FALSE,
                closeable = FALSE,
                solidHeader = TRUE,
                width = 12,
                br(),
                includeMarkdown("info/app_description.md")
            )
        )
    )
)
# Build UI ----
ui <- bs4DashPage(
    header,
    sidebar,
    body,
    title = title,
    sidebar_collapsed = FALSE
)