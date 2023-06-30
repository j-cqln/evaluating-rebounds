library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = 'Hockey visualization'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Rebounds',
               tabName = 'rebounds',
               icon = icon('bullseye')),
      
      menuItem('Work in progress',
               tabName = 'othertab',
               icon = icon('route')),
      
      menuItem('Source code & info',
               tabName = 'info',
               icon = icon('code-branch'))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Rebounds tab
      tabItem(tabName = 'rebounds',
              fluidRow(
                box(title = 'Rebound value of shots',
                    p('Using data from the 2023 PWHPA season, shots were plotted 
                    by region in terms of rebound quality.',
                      br(),
                      br(),
                      'By overlaying shots\' rebound probability and rebounds\' 
                    rebound shot probability with isolated rebound shot xG, we 
                    obtain a rebound value for shots in each region.',
                      br(),
                      br(),
                      'We notice that, logically, not only do shots from the 
                    inner slot region generate more rebounds and rebound shots 
                    but those rebound shots are also more valuable in terms of 
                    xG. Also, rebound shots originating from wide angle shots 
                    are less frequent as well as less valuable.',
                      br(),
                      br(),
                      'The xG model used here includes a variable for the 
                    previous event. Isolated xG refers to values from not 
                    using the previous event variable in the model.',
                      br(),
                      br(),
                      'The regions used in the plots below are shown to the 
                    right, shaded by number of shot attempts originating from 
                    each region.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('regions')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds1')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(title = 'Comparison between xG* and xG with previous event',
                    p('By adding isolated shot xG to the rebound value 
                    shown in the rightmost plot above, we obtain a method of 
                    accounting for rebounds in expected goals, referred to as 
                      xG* here.',
                      br(),
                      br(),
                      'To compare xG* with xG that includes the previous event, 
                    we compute the difference between the two. We observe that 
                    xG* actually has lower values for slot shots and higher 
                    values everywhere else compared to xG with last event.',
                      br(),
                      br(),
                      'It is possible that this may be an effect of the the small 
                    sample size, but it could also be that slot shots are more 
                    valuable due to many of them being rebound shots 
                    (and xG is not aware of rebound shot probabilities).',
                      br(),
                      br(),
                      'Average xG values per region from the model with previous 
                    event are shown to the right.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('xg')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds2')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      ),
      
      # Other tab
      tabItem(tabName = 'othertab',
              fluidRow(
                box(title = 'Work in progress...',
                    p('Coming soon!', style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      ),
      
      # Source code & info tab
      tabItem(tabName = 'info',
              fluidRow(
                box(title = 'Project info',
                    p('Created by Jacqueline (github.com/j-cqln).',
                      'Source code available',
                      a('here',
                        href = 'https://github.com/j-cqln/hockey-visualization'),
                      'on GitHub.',
                      br(),
                      br(),
                      'Thanks to Brian Macdonald for guidance and providing the 
                    rink plotting function.',
                      br(),
                      br(),
                      'Initial version (July 2, 2023) made as an entry to the 
                    Viz Launchpad competition hosted by WHKYHAC + Sportlogiq.',
                      br(),
                      br(),
                      'Women\'s hockey data from Viz Launchpad, WHKYHAC + 
                    Sportlogiq.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      )
    )
  )
)

server <- function(input, output) {
  output$regions <- renderUI({
    img(src = 'regions_all.jpg', height = '375px')
  })
  
  output$xg <- renderUI({
    img(src = 'xg_all.jpg', height = '375px')
  })
  
  output$rebounds1 <- renderUI({
    img(src = 'rebounds_all.jpg', height = '375px')
  })
  
  output$rebounds2 <- renderUI({
    img(src = 'xg_rebounds_all.jpg', height = '375px')
  })
}

shinyApp(ui, server)