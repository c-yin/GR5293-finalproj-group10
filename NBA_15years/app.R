#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(scales)
library(shinyWidgets)
library(shinythemes)

load('Player.RData')
load('Team_splits.RData')
load('Team_shooting.RData')

season_list <- sprintf("%02d-%02d", 03:18, 04:19)
team_list <- c('Atlanta Hawks', 'Brooklyn Nets', 'Boston Celtics', 'Charlotte Hornets', 'Charlotte Bobcats',
               'Chicago Bulls', 'Cleveland Cavaliers', 'Dallas Mavericks', 'Denver Nuggets', 'Detroit Pistons',
               'Golden State Warriors', 'Houston Rockets', 'Indiana Pacers', 'Los Angeles Clippers', 'Los Angeles Lakers',
               'Memphis Grizzlies', 'Miami Heat', 'Milwaukee Bucks', 'Minnesota Timberwolves', 'New Jersey Nets',
               'New Orleans Hornets', 'NO/Ok. City Hornets', 'New Orleans Pelicans', 'New York Knicks', 'Oklahoma City Thunder',
               'Orlando Magic', 'Philadelphia 76ers', 'Phoenix Suns', 'Portland Trail Blazers', 'Sacramento Kings',
               'San Antonio Spurs', 'Seattle SuperSonics', 'Toronto Raptors', 'Utah Jazz', 'Washington Wizards')
team_abbr <- sort(unique(Player$Tm))
names(team_list) <- team_abbr
names(team_abbr) <- team_list

colors = c('#bebada', '#fb8072', '#8dd3c7')

tab_score <- navbarMenu(
  'Scoring',
  tabPanel(
    title = 'Brief Introduction',
    sidebarPanel(
      h2('Scoring'),
      p("This part shows analysis of changes in players' scoring methods and efficiency during last 15 years in NBA. Definitions of terminologies we'll mention are on the right part.", style = "font-family: 'times'; font-size: 14pt"),
      img(src = "kobe.png")
    ),
    mainPanel(
      setBackgroundImage(src = 'court.jpg'),
      br(),
      strong('Field Goal (FG)', style = "font-family: 'times'; font-size: 16pt"),
      p('In basketball, a field goal is a basket scored on any shot or tap other than a free throw, worth two or three points depending on the distance of the attempt from the basket.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Points (PT)', style = "font-family: 'times'; font-size: 16pt"),
      p('Points can be accumulated by making field goals (two or three points) or free throws (one point). If a player makes a field goal from within the three-point line, the player scores two points.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Three-Point Field Goal/3-Pointer (3PT)', style = "font-family: 'times'; font-size: 16pt"),
      p('A three-point field goal (also 3-pointer or informally, trey) is a field goal in a basketball game made from beyond the three-point line, a designated arc surrounding the basket.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Free Throw (FT)', style = "font-family: 'times'; font-size: 16pt"),
      p('Free throws or foul shots are unopposed attempts to score points by shooting from behind the free throw line (informally known as the foul line or the charity stripe), a line situated at the end of the restricted area. Free throws are generally awarded after a foul on the shooter by the opposing team. Each successful free throw is worth one point.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('True Shooting Percentage (TS%)',  style = "font-family: 'times'; font-size: 16pt"),
      p("True shooting percentage is an APBRmetrics statistic that measures a player's efficiency at shooting the ball. It is intended to more accurately calculate a player's shooting than field goal percentage, free throw percentage, and three-point field goal percentage taken individually. Two- and three-point field goals and free throws are all considered in its calculation.",  style = "font-family: 'times'; font-size: 14pt")
    )
  ),
  
  tabPanel(
    title = 'Field Goal Attempts',
    sidebarLayout(
      sidebarPanel(
        h2('Field Goal Attemps'),
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total), and at least 0.1 3-pointer attempts.', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses 3-pointer and 2-pointer FGA (Field Goal Attempts) as axises and total FGA as markers' size to demostrate scoring methods of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season1',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
                    ),
        width = 3,
        uiOutput('team1')
      ),
      mainPanel(
        plotlyOutput('Field_Goal1')
      )
    )
  ),
  
  tabPanel(
    title = 'Field Goal Percentage',
    sidebarLayout(
      sidebarPanel(
        h2('Field Goal Percentage'),
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total), and at least 0.1 3-pointer attempts.', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses 3-pointer and 2-pointer FG% (Field Goal Percentage) as axises and total FGA as markers' size to demostrate scoring effieciency of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season2',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team2')
      ),
      mainPanel(
        plotlyOutput('Field_Goal2')
      )
    )
  ),
  
  tabPanel(
    title = '3-Pointer Perforamnce',
    sidebarLayout(
      sidebarPanel(
        h2('3-Pointer Perforamnce'),
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total), and at least 0.1 3-pointer attempts.', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses 3-pointer and 3-pointer percentage as axises and total FGA as markers' size to demostrate three pointer performance of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season3',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team3')
      ),
      mainPanel(
        plotlyOutput('three_pointer')
      )
    )
  ),
  
  tabPanel(
    title = '2-Pointer Perforamnce',
    sidebarLayout(
      sidebarPanel(
        h2('2-Pointer Perforamnce'),
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total).', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses 2-pointer and 2 pointer percentage as axises and total FGA as markers' size to demostrate two pointer performance of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season4',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team4')
      ),
      mainPanel(
        plotlyOutput('two_pointer')
      )
    )
  ),
  
  tabPanel(
    title = 'True Shooting Percentage',
    sidebarLayout(
      sidebarPanel(
        h2('True Shooting Percentage'),
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total).', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses TS% and FG as axises and FGA as markers' size to demostrate shooting ability of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season5',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team5')
      ),
      mainPanel(
        plotlyOutput('true_shooting')
      )
    )
  )
)

tab_pass <- navbarMenu(
  'Sharing Ball',
  tabPanel(
    title = 'Brief Introduction',
    sidebarPanel(
      h2('Sharing Ball'),
      p("This part shows analysis of changes in players' willingness and ability of sharing ball during last 15 years in NBA. Definitions of terminologies we'll mention are on the right part.", style = "font-family: 'times'; font-size: 14pt"),
      img(src = "nash.png")
    ),
    mainPanel(
      setBackgroundImage(src = 'court.jpg'),
      br(),
      strong('Assit (Ast)', style = "font-family: 'times'; font-size: 16pt"),
      p('In basketball, an assist is attributed to a player who passes the ball to a teammate in a way that leads to a score by field goal, meaning that he or she was "assisting" in the basket.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Turnover (Tov)', style = "font-family: 'times'; font-size: 16pt"),
      p("A turnover occurs when a team loses possession of the ball to the opposing team before a player takes a shot at their team's basket.", style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Assist Percentage Ast%', style = "font-family: 'times'; font-size: 16pt"),
      p('Assist percentage is an estimate of the percentage of teammate field goals a player assisted while he was on the floor.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Turnover Percentage (Tov%)', style = "font-family: 'times'; font-size: 16pt"),
      p(' Turnover percentage is an estimate of turnovers per 100 plays.', style = "font-family: 'times'; font-size: 14pt"),
      br(),
      br(),
      strong('Usage Rate (USG%)',  style = "font-family: 'times'; font-size: 16pt"),
      p("Usage rate, a.k.a., usage percentage is an estimate of the percentage of team plays used by a player while he was on the floor. By balancing usage rates and the varying offensive ratings of the five players on the court, a team can achieve optimal offensive output.",  style = "font-family: 'times'; font-size: 14pt")
    )
  ),
  tabPanel(
    title = 'Assist VS Turnover',
    sidebarLayout(
      sidebarPanel(
        h2('Assist VS Turnover'),
        
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total).', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses Assist and Turnover as axises and USG% as markers' size to demostrate shooting ability of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season6',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team6')
      ),
      mainPanel(
        plotlyOutput('ast_tov')
      )
    )
  ),
  
  tabPanel(
    title = 'Ast% VS Tov%',
    sidebarLayout(
      sidebarPanel(
        h2('Ast% VS Tov%'),
       
        p('We choose players attend more than 30 games in a season (82 games in total), play for more than 15 minutes per game (48 minutes in total).', style = "font-family: 'times'; font-size: 14pt"),
        p("The plot uses Ast% and Tov% as axises and USG% as markers' size to demostrate shooting ability of teams in different seasons.", style = "font-family: 'times'; font-size: 14pt"),
        p('You can also click on the buttons at top left of the plot to show only your intreseted position.', style = "font-family: 'times'; font-size: 14pt"),
        sliderInput('Season7',
                    label = h4('Season'),
                    min = 2004, max = 2019,
                    value = c(2004, 2019),
                    sep = '',
        ),
        width = 3,
        uiOutput('team7')
      ),
      mainPanel(
        plotlyOutput('astper_tovper')
      )
    )
  )
)

ui <- fluidPage(
  navbarPage(
    title = "NBA's 15 years",
    tab_score,
    tab_pass
  )
)

server <- function(input, output) {
  output$team1 <- renderUI({
    Year <- input$Season1[1]:input$Season1[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team1',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
                )
  })
  
  output$Field_Goal1 <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15 & `3PA` >= 0.1) %>% 
      filter(year >= input$Season1[1] & year <= input$Season1[2]) %>%
      select(Player, Tm, Pos, FGA, `2PA`, `3PA`, year)
    if(!('All' %in% input$Team1)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team1])
    
    FG %>% plot_ly(x = ~`2PA`, y = ~`3PA`, mode = 'markers', color = ~Pos, colors = colors, size = ~FGA, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>2PA <b>', `2PA`, '</b>' ,
                                 '<br>3PA <b>', `3PA`, '</b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
        ) %>%
      layout(xaxis = list(title = '2 pointer field goal attempts',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             yaxis = list(title = '3 pointer field goal attempts',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
        )
  })
  
  output$team2 <- renderUI({
    Year <- input$Season2[1]:input$Season2[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team2',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$Field_Goal2 <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15 & `3PA` >= 0.1) %>% 
      filter(year >= input$Season2[1] & year <= input$Season2[2]) %>%
      select(Player, Tm, Pos, FGA, `2P%`, `3P%`, year)
    if(!('All' %in% input$Team2)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team2])
    
    FG %>% plot_ly(x = ~`2P%`, y = ~`3P%`, mode = 'markers', color = ~Pos, colors = colors, size = ~FGA, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>2P% <b>', percent(`2P%`), '</b>', 
                                 '<br>3P% <b>', percent(`3P%`), '</b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = '2 pointer field goal percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          tickformat = '%'),
             yaxis = list(title = '3 pointer field goal percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          tickformat = '%'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
  
  output$team3 <- renderUI({
    Year <- input$Season3[1]:input$Season3[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team3',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$three_pointer <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15 & `3PA` >= 0.1) %>% 
      filter(year >= input$Season3[1] & year <= input$Season3[2]) %>%
      select(Player, Tm, Pos, `3PA`, `3P`, `3P%`, year)
    if(!('All' %in% input$Team3)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team3])
    
    FG %>% plot_ly(x = ~`3P`, y = ~`3P%`, mode = 'markers', color = ~Pos, colors = colors, size = ~`3PA`, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>3P <b>', `3P`,'</b>', 
                                 '<br>3P% <b>', percent(`3P%`), '</b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = '3 pointer field goals',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             yaxis = list(title = '3 pointer field goal percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          tickformat = '%'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
  
  output$team4 <- renderUI({
    Year <- input$Season4[1]:input$Season4[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team4',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$two_pointer <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15) %>% 
      filter(year >= input$Season4[1] & year <= input$Season4[2]) %>%
      select(Player, Tm, Pos, `2PA`, `2P`, `2P%`, year)
    if(!('All' %in% input$Team4)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team4])
    
    FG %>% plot_ly(x = ~`2P`, y = ~`2P%`, mode = 'markers', color = ~Pos, colors = colors, size = ~`2PA`, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>2P <b>', `2P`, '</b>',
                                 '<br>2P% <b>', percent(`2P%`), ' </b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = '2 pointer field goals',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             yaxis = list(title = '2 pointer field goal percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          tickformat = '%'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
  
  output$team5 <- renderUI({
    Year <- input$Season5[1]:input$Season5[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team5',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$true_shooting <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15) %>% 
      filter(year >= input$Season5[1] & year <= input$Season5[2]) %>%
      select(Player, Tm, Pos, FG, `TS%`, FGA, year)
    if(!('All' %in% input$Team5)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team5])
    
    FG %>% plot_ly(x = ~FG, y = ~`TS%`, mode = 'markers', color = ~Pos, colors = colors, size = ~`FGA`, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>TS% <b>', `TS%`, ' </b>',
                                 '<br>FG <b>', FG, ' </b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = 'total field goals',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             yaxis = list(title = 'true shooting percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          tickformat = '%'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
  
  output$team6 <- renderUI({
    Year <- input$Season6[1]:input$Season6[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team6',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$ast_tov <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15) %>% 
      filter(year >= input$Season6[1] & year <= input$Season6[2]) %>%
      select(Player, Tm, Pos, AST, TOV, `USG%`, year)
    if(!('All' %in% input$Team6)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team6])
    
    FG %>% plot_ly(x = ~TOV, y = ~AST, mode = 'markers', color = ~Pos, colors = colors, size = ~`USG%`, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>Ast  <b>', AST, ' </b>',
                                 '<br>TOV  <b>', TOV,  ' </b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = 'Turnover',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             yaxis = list(title = 'Assist',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
  
  output$team7 <- renderUI({
    Year <- input$Season7[1]:input$Season7[2]
    abbr <- sort(Player[Player$year %in% Year,]$Tm)
    team_names <- unname(team_list[abbr])
    selectInput('Team7',
                label = h4('Team'),
                choices = c('All' = 'All', team_names),
                selected = 'All',
                multiple = TRUE
    )
  })
  
  output$astper_tovper <- renderPlotly({
    FG <- Player %>% distinct(Player, Tm, year, .keep_all = TRUE) %>%
      filter(G >= 30 & MP >= 15) %>% 
      filter(year >= input$Season7[1] & year <= input$Season7[2]) %>%
      select(Player, Tm, Pos, `AST%`, `TOV%`, `USG%`, year)
    if(!('All' %in% input$Team7)) FG <- FG %>% filter(Tm %in% team_abbr[input$Team7])
    
    FG %>% plot_ly(x = ~`TOV%`, y = ~`AST%`, mode = 'markers', color = ~Pos, colors = colors, size = ~`USG%`, sizes = c(20,800),
                   marker = list(opacity = 0.8,
                                 line = list(width = 1, color = '#FFFFFF')),
                   frame = ~year, 
                   type = 'scatter',
                   hoverinfo = 'text',
                   text = ~paste('<b>', Player, ',',  '</b>', Tm,
                                 '<br>Ast%  <b>', `AST%`, ' </b>',
                                 '<br>TOV%  <b>', `TOV%`, ' </b>')) %>%
      animation_opts(
        1000, easing = "elastic", redraw = FALSE
      ) %>%
      layout(xaxis = list(title = 'Turnover Percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          ticksuffix = '%'),
             yaxis = list(title = 'Assist Percentage',
                          titlefont = list(
                            size = 15
                          ),
                          showgrid = TRUE,
                          showline = FALSE,
                          zerolinecolor = '939393',
                          layer = 'belowtraces',
                          ticksuffix = '%'),
             autosize = F,
             width = 1000,
             height = 800,
             margin = list(
               l = 50, r = 50, b = 100, t = 50, pad = 4
             )
      )
  })
}

shinyApp(ui = ui, server = server)
