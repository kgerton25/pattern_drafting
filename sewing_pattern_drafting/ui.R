title = "Sewing Pattern DraftR"
# Header ----
header <- bs4DashNavbar(
    
)
# Sidebar ----
sidebar <- bs4DashSidebar(
    
)
# App Body ----
body <- bs4DashBody(
    
)
# Build UI ----
ui <- bs4DashPage(
    header,
    sidebar,
    body,
    title = title,
    sidebar_collapsed = FALSE
)