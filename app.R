library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = 'Hockey visualization'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Rebounds',
               icon = icon('bullseye'), startExpanded = TRUE,
               menuSubItem('PWHPA, all shots', tabName = 'reboundsAll'),
               menuSubItem('PWHPA, unblocked shots', tabName = 'reboundsUnblocked'),
               menuSubItem('NHL, unblocked shots', tabName = 'reboundsUnblockedNHL')),
      
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
      # Rebounds PWHPA all shots tab
      tabItem(tabName = 'reboundsAll',
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
                                uiOutput('regions_all')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds1_all')),
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
                    xG* actually has lower values for slot shots and higher or 
                    similar values everywhere else compared to xG with last event.',
                      br(),
                      br(),
                      'It is possible that this may be an effect of the the small 
                    sample size, but it could also be that slot shots are more 
                    valuable due to many of them being rebound shots 
                    (and xG is not aware of rebound shot probabilities). By 
                      comparing with NHL data, a larger dataset, we notice 
                      that the effect is reversed, suggesting this is due to 
                      the sample size. See the NHL rebounds tab for more.',
                      br(),
                      br(),
                      'Average xG values per region from the model with previous 
                    event are shown to the right.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('xg_all')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds2_all')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      ),
      
      # Rebounds PWHPA unblocked shots tab
      tabItem(tabName = 'reboundsUnblocked',
              fluidRow(
                box(title = 'Rebound value and xG* vs xG comparison, unblocked shots',
                    p('Modeling and plotting is done for unblocked shots, 
                    analogous to for all shot attempts.',
                      br(),
                      br(),
                      'Notable observations include that xG* values all non-slot 
                      shots more than regular xG. This could be attributed to 
                      the smaller sample size, or the fact that the xG model 
                      is unaware of contribution from subsequent shots, but 
                      does value slot shots more  due to them more often being 
                      rebound shots.',
                      br(),
                      br(),
                      'By comparing with NHL data, a larger dataset, we notice 
                      that the effect is reversed, suggesting this is due to 
                      the sample size. See the NHL rebounds tab for more.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('regions_unblocked')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('xg_unblocked')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds1_unblocked')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds2_unblocked')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      ),
      
      # Rebounds NHL unblocked shots tab
      tabItem(tabName = 'reboundsUnblockedNHL',
              fluidRow(
                box(title = 'Rebound value and xG* vs xG comparison, NHL unblocked shots',
                    p('Modeling and plotting is done for unblocked shots from 
                    the 2022-2023 NHL season, analogous to for PWHPA all shot 
                    attempts. This is a larger dataset, with over 122000 
                      unblocked shots.',
                      br(),
                      br(),
                      'Rebounds are not distinguished from rebound shots, so 
                      rebound probabilities cannot be plotted.',
                      br(),
                      br(),
                      'We observe that xG* has higher values than xG across 
                      all regions, but particularly for slot shots. This fits 
                      the theory that slot shots should create more value due 
                      to higher rebound potential. The results in the model 
                      difference plot here suggest the trend shown in the 
                      PWHPA model difference plots could be an effect of 
                      sample size.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('regions_unblocked_nhl')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('xg_unblocked_nhl')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds1_unblocked_nhl')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('rebounds2_unblocked_nhl')),
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
                      'Initial version (July 2, 2023) with women\'s hockey 
                      data was made as an entry to the Viz Launchpad 
                      competition hosted by WHKYHAC + Sportlogiq.',
                      br(),
                      br(),
                      'Women\'s hockey data from Viz Launchpad, WHKYHAC + 
                    Sportlogiq. NHL shot data from MoneyPuck.com.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                tabBox(
                  title = 'Definitions of terms', side = 'left',
                  # input$definitionTab to find current tab
                  id = 'definitionTab',
                  tabPanel('Rebounds',
                           p('xG: ',
                             br(),
                             'Expected goals model taking into account 
                             distance, angle, goal differential, previous 
                             event, shot type, shooter and goalie involved',
                             br(),
                             br(),
                             'Isolated xG: ',
                             br(),
                             'xG without previous event',
                             br(),
                             br(),
                             'Rebound value: ',
                             br(),
                             'Probability-weighted xG of 
                             rebound shots; this value is assigned to 
                             the original shot',
                             br(),
                             br(),
                             'xG*: ',
                             br(),
                             'Isolated xG with rebound value added',
                             style = 'font-size:16px;margin:5px;')),
                  tabPanel('Work in progress',
                           'Coming soon!'),
                  width = 4
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  # Rebounds
  # PWHPA, all shots
  output$regions_all <- renderUI({
    img(src = 'regions_all.jpg', height = '375px')
  })
  
  output$xg_all <- renderUI({
    img(src = 'xg_all.jpg', height = '375px')
  })
  
  output$rebounds1_all <- renderUI({
    img(src = 'rebounds_all.jpg', height = '375px')
  })
  
  output$rebounds2_all <- renderUI({
    img(src = 'xg_rebounds_all.jpg', height = '375px')
  })
  
  # PWHPA, unblocked shots
  output$regions_unblocked <- renderUI({
    img(src = 'regions_unblocked.jpg', height = '375px')
  })
  
  output$xg_unblocked <- renderUI({
    img(src = 'xg_unblocked.jpg', height = '375px')
  })
  
  output$rebounds1_unblocked <- renderUI({
    img(src = 'rebounds_unblocked.jpg', height = '375px')
  })
  
  output$rebounds2_unblocked <- renderUI({
    img(src = 'xg_rebounds_unblocked.jpg', height = '375px')
  })
  
  # NHL, unblocked shots
  output$regions_unblocked_nhl <- renderUI({
    img(src = 'regions_unblocked_nhl.jpg', height = '375px')
  })
  
  output$xg_unblocked_nhl <- renderUI({
    img(src = 'xg_unblocked_nhl.jpg', height = '375px')
  })
  
  output$rebounds1_unblocked_nhl <- renderUI({
    img(src = 'rebounds_unblocked_nhl.jpg', height = '375px')
  })
  
  output$rebounds2_unblocked_nhl <- renderUI({
    img(src = 'xg_rebounds_unblocked_nhl.jpg', height = '375px')
  })
}

shinyApp(ui, server)