library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = 'Evaluating rebounds'),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem('Rebounds',
               icon = icon('bullseye'), startExpanded = TRUE,
               menuSubItem('PWHPA, all shots', tabName = 'reboundsAll'),
               menuSubItem('PWHPA, unblocked shots', tabName = 'reboundsUnblocked'),
               menuSubItem('NHL, unblocked shots', tabName = 'reboundsUnblockedNHL')),
      
      menuItem('Passes & zone entries',
               tabName = 'passesZoneEntries',
               icon = icon('route')),
      
      menuItem('Summary',
               tabName = 'summary',
               icon = icon('paperclip')),
      
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
                    valuable due to many of them being rebound shots (and xG 
                    is not aware of rebound shot probabilities). By comparing 
                    with a larger dataset of NHL shots, we notice that the 
                    effect is reversed, suggesting this may be due to the 
                      sample size or difference in rebound shot definition.',
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
                box(title = 'Rebound value, xG* vs xG, unblocked shots',
                    p('Modeling and visualization done analogously using 
                      PWHPA unblocked shots.',
                      br(),
                      br(),
                      'Notably, the xG* model values all non-slot shots more 
                      than regular xG. This could be attributed to the smaller 
                      sample size, or the fact that the xG model is unaware of 
                      contribution from subsequent shots, but does value slot 
                      shots more due to them more often being rebound shots.',
                      br(),
                      br(),
                      'By comparing with NHL data, a larger dataset, we notice 
                      that the effect is reversed, suggesting this is due to 
                      sample size and differences in rebound definition.',
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
                box(title = 'Rebound value, xG* vs xG, NHL',
                    p('Modeling and visualization done analogously using 
                    unblocked shots from the 2022-2023 NHL season, a larger 
                    dataset. Rebounds are not separately defined from rebound 
                    shots, so rebound probabilities cannot be plotted.',
                      br(),
                      br(),
                      'We observe that xG* is higher than xG across all 
                      regions, but particularly for slot shots. This fits 
                      the theory that slot shots should create more value due 
                      to higher rebound potential. This suggests that the 
                      difference in trends shown in PWHPA and NHL model 
                      comparison plots may be an effect of sample size and 
                      rebound definition.',
                      br(),
                      br(),
                      'The difference between PWHPA and NHL data when comparing 
                      xG* and xG may be due to PWHPA P(rebound shot | rebound) 
                      being 0.015-0.075, while NHL P(rebound shot) is 0.05-0.10. 
                      Compounding on the probability difference, the NHL data 
                      has a less strict rebound shot definition as rebounds are 
                      not separately defined. This makes all NHL shots more 
                      valuable in terms of xG* than xG. For PWHPA data, the 
                      lower rebound shot probability means original shots get 
                      less credit for generating rebound value. However, when 
                      rebounds do occur, they are still valuable, so removing 
                      the previous event has roughly similar negative impact as 
                      that for NHL data. Since rebound value has less positive 
                      impact, the net effect is negative, particularly for the 
                      inner slot, the highest rebound value region.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE)),
              
              fluidRow(
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
                    width = 10,
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
      
      # Passes & zone entries tab
      tabItem(tabName = 'passesZoneEntries',
              fluidRow(
                box(title = 'Passes by subsequent shot xG',
                    p('Passes directly leading to low danger shots largely 
                    originate from regions far from the slot, while passes 
                      directly leading to high danger shots tend to originate 
                      from near the goal line. Passes from closer to the net 
                      lead to more dangerous shots.',
                      br(),
                      br(),
                      'Overall, passes originate from regions other than the 
                      slot, likely due to players shooting more in the slot 
                      as opposed to passing.',
                      br(),
                      br(),
                      'High danger refers to shot attempts in the top 20% in 
                      terms of xG, while low danger refers to the bottom 80%.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('passes')),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(title = 'Offensive zone entries',
                    p('Successful controlled entries into the offensive zone 
                      are noticeably concentrated at the two point areas.',
                      br(),
                      br(),
                      'Looking at this data at a player level per 60 minutes 
                      of play, we can see that all 20 of the top players 
                      are forwards, with Victoria Bach the most effective 
                      at contributing successful offensive zone entries.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('ozone_entries_players')),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE)
              ),
              
              fluidRow(
                box(splitLayout(cellWidths = c('100%'),
                                uiOutput('ozone_entries_map')),
                    width = 12,
                    status = 'primary',
                    solidHeader = TRUE)
              )
      ),
      
      # Summary
      tabItem(tabName = 'summary',
              fluidRow(
                box(title = 'Summary',
                    p('Rebound value modeling shows that shots from the inner 
                    slot are better in terms of xG and the value of the rebound 
                    generated. A modified form of xG using rebound value 
                    accounting can then be compared to regular xG to examine 
                    their difference the largest difference occurring in the 
                    inner slot region where more frequent and higher value 
                    rebounds occur. The result of this model comparison is 
                    also examined through comparing trends in PWHPA and other 
                    data.',
                      br(),
                      br(),
                      'Visualizing the location of passes leading directly to 
                    shot attempts shows that passes leading to high danger 
                    shots in terms of xG primarily originate from near the 
                    goal line, while those leading to low danger shots 
                    primarily originate from the point. Overall, these passes 
                    originate from regions outside the slot (where players are 
                    more likely to shoot). Visualizing successful offensive 
                    zone entry locations show that they are concentrated near 
                    the point as opposed to center ice.',
                      br(),
                      br(),
                      'Forwards, reasonably, are ranked high in both high danger 
                    passes and successful offensive zone entries, reasonably, 
                    with only 2 defensemen in the top 20 for high danger 
                    passes, and no defensemen in the top 20 for successful 
                    entries. These features are candidates for future shot and 
                    rebound valuation models.',
                      br(),
                      br(),
                      'Data cleaning and visualization were done using R.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 8,
                    status = 'primary',
                    solidHeader = TRUE))),
      
      # Source code & info tab
      tabItem(tabName = 'info',
              fluidRow(
                box(title = 'Project info',
                    p('Source code available',
                      a('here',
                        href = 'https://github.com/j-cqln/evaluating-rebounds'),
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
                      br(),
                      br(),
                      'NHL shot data from MoneyPuck.com.',
                      style = 'font-size:16px;margin:5px;'),
                    width = 4,
                    status = 'primary',
                    solidHeader = TRUE),
                
                box(
                  title = 'Definitions of terms',
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
                    style = 'font-size:16px;margin:5px;'),
                  width = 4,
                  status = 'primary',
                  solidHeader = TRUE
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
  
  # Passes & zone entries
  output$passes <- renderUI({
    img(src = 'passes.jpg', height = '350px')
  })
  
  output$ozone_entries_map <- renderUI({
    img(src = 'ozone_entries_map.jpg', height = '260px')
  })
  
  output$ozone_entries_players <- renderUI({
    img(src = 'ozone_entries_players.jpg', height = '325px')
  })
}

shinyApp(ui, server)