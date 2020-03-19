library(shiny)
library(shinythemes)
library(waiter)
library(nCov2019)
library(tidyverse)
library(echarts4r)
library(echarts4r.maps)
library(lubridate)
library(stringr)
library(purrr)
library(DT)
library(readr)
library(showtext)
showtext_auto()
font.add("chinese", "SIMYOU.TTF")
#library(zoo)

# raw_data <- load_nCov2019(source = 'dxy')
# glimpse(raw_data)
# data <- raw_data[][, c(1:6)]
# data <- data[data$province != data$city | data$province %in% c('重庆', '上海', '北京', '天津'), ]
# 
# 
# data <- data %>%
#   filter(time %in% seq(ymd('2020-02-07'), ymd('2020-03-15'), by = 'day')) %>%
#           group_by(province, city) %>%
#           transmute(time = time, confirm = c(cum_confirm[1], diff(cum_confirm)),
#                  heal = c(cum_heal[1], diff(cum_heal)),
#                  dead = c(cum_dead[1], diff(cum_dead)),
#                  cum_confirm = cum_confirm,
#                  cum_heal = cum_heal,
#                  cum_dead = cum_dead)
# data$confirm[data$confirm < 0] <- NA
# data$heal[data$heal < 0] <- NA
# data$dead[data$dead < 0] <- NA
# 
# data <- data %>%
#   group_by(province, city) %>%
#   transmute(time = time,
#             province = province,
#             city = city,
#             confirm = na.locf(confirm),
#             heal = na.locf(heal),
#             dead = na.locf(dead))
# 
# 
# 
# ############################################################
# 
# empty_data <- merge(unique(data$time), unique(data[, c('province', 'city')]))
# names(empty_data)[1] <- 'time'
# 
# merge_data <- left_join(empty_data, data)
# merge_data[is.na(merge_data)] <- 0
# 
# setwd('C:/Users/Cooper97/Desktop/shiny_2019nCov')
# write.csv(merge_data, 'data.csv')
merge_data <- read_csv('data.csv')

merge_data$time <- ymd(merge_data$time)
#glimpse(merge_data)

three_class_list <- list('新增确诊患者情况' = 'confirm',
                         '新增死亡患者情况' = 'dead',
                         '新增治愈患者情况' = 'heal')

#################################################################
##读入中国地图数据
china_sp <- readRDS('./gadm36_CHN_2_sp.rds')

province <- unique(merge_data$province)
city <- unique(merge_data$city)

##将原始中国地图数据中的省份名称按照正确的形式修改过来
walk(province, function(x){
  index <- str_detect(china_sp$NL_NAME_1, x)
  china_sp$NL_NAME_1[index] <<- x
})

##将原始中国地图数据中的市区名称按照正确的形式修改过来
walk(city, function(x){
  index <- str_detect(china_sp$NL_NAME_2, x)
  china_sp$NL_NAME_2[index] <<- x
})

map_name_list <- map(1:2, ~ paste0(letters, .x)) %>% unlist() %>%
  head(length(province))
map_name_list = map(map_name_list, ~ .x)
names(map_name_list) <- province

#############################################################
##在画图中，颜色比例尺，自己重新定义的函数
.get_data <- function(e, serie, i = 1){
  e$x$data[[i]] %>% 
    dplyr::select_(serie) %>% 
    unname() %>% 
    .[[1]]
}

my_e_visual_map_ <- function (e, serie = NULL, min, max, calculable = TRUE, type = c("continuous", 
                                                                                     "piecewise"), scale = NULL, ...) 
{
  if (missing(e)) 
    stop("must pass e", call. = FALSE)
  if (!length(e$x$opts$visualMap)) 
    e$x$opts$visualMap <- list()
  vm <- list(...)
  vm$calculable <- calculable
  vm$type <- type[1]
  if (!is.null(serie)) {
    dat <- .get_data(e, serie)
    if (!is.null(scale)) 
      dat <- scale(dat)
    vm$min <- min
    vm$max <- max
  }
  if (!e$x$tl) 
    e$x$opts$visualMap <- append(e$x$opts$visualMap, list(vm))
  else e$x$opts$baseOption$visualMap <- append(e$x$opts$baseOption$visualMap, 
                                               list(vm))
  e
}



my_e_visual_map <- function (e, serie, calculable = TRUE, type = c("continuous", 
                                                                   "piecewise"), scale = NULL, ...) 
{
  if (missing(e)) 
    stop("must pass e", call. = FALSE)
  if (!missing(serie)) 
    serie <- deparse(substitute(serie))
  else serie <- NULL
  my_e_visual_map_(e, serie, calculable, type, scale, ...)
}

#############################################################
##前端布局
##做一个导航栏
ui <- tagList(
  
  # head + css
  tags$head(
    tags$link(href="homepage.css", rel="stylesheet", type="text/css")
  ),
  
  
  
  shinyUI(
    navbarPage(title = a(img(src = 'R.png', height = '20px', hspace = '10px')),
               theme = shinytheme('cerulean'),
               windowTitle = 'Simple 2019_nCov Map Dashboard',
               ##第一页网页
               ##卷首语，讲一下这次疫情的一些话，激励大家
               tabPanel('页首语', icon = icon('book'),
                        # parent container
                        tags$div(class="landing-wrapper",
                                 
                                 # child element 1: images
                                 tags$div(class="landing-block background-content",
                                          
                                          ##image
                                          img(src=base64enc::dataURI(file="homepage.jpg", mime="image/jpg"))
                                          #img(src='https://wx1.sinaimg.cn/crop.0.2.640.355/a716fd45gy1gbdmkn1tgpj20hs0a0afh.jpg')
                                 )
                        )
                        
               ),
               ##第二页网页，画一些总体上的图表
               ##总体情况
               tabPanel('总体情况', icon = icon('globe-asia'),
                        
                        fluidPage(
                          ##
                          fluidRow(
                            column(width = 2),
                            column(width = 5,
                                   sliderInput('date', '选择你想查看的日期', value = ymd('2020-02-07'), min = ymd('2020-02-07'), max = ymd('2020-03-14'))),
                            column(width = 5,
                                   selectInput('barplot_class', list('选择查看不同数量统计', br(), br()), choices = three_class_list))),
                          
                          ##
                          br(),
                          br(),
                          br(),
                          fluidRow(plotOutput('barplot')),
                          br(),
                          br(),
                          br(),
                          fluidRow(plotOutput('lineplot'))
                          
                          
                        )
                        
                        ),
               
               
               ##第三页，有关省一级的情况展示
               tabPanel('省级情况', icon = icon('refresh'),
                        
                        use_waiter(),
                        ##
                        titlePanel(title = '分省情况的展示'),
                        
                        '从省一级别来看国内疫情发展的情况，
                                有助于我们从总体层面来发现一些规律。',
                        
                        br(),
                        br(),
                        br(),
                        
                        ##第一行
                        fluidPage(
                          ##前四列空白占位
                          column(width = 4),
                          ##中间四列放上选项
                          column(width = 4,
                                 selectInput('province_map1_choices', '请选择你想查看的类别',
                                             choices = three_class_list)),
                          column(width = 4)
                        ),
                        
                        ##第二行，显示“点击查看”按钮
                        fluidPage(
                          column(width = 4),
                          column(width = 8, actionButton('submit_class', '点击查看'))
                        ),
                        
                        br(),
                        br(),
                        br(),
                        
                        ##第三行，展示动态地图
                        fluidPage(
                          ##
                          column(width = 1),
                          column(width = 10,
                                 echarts4rOutput('map1')),
                          column(width = 1)
                          
                        )
               ),
               
               ##第四页
               tabPanel('市级情况', icon = icon('table'),
                        
                        use_waiter(),
                        
                        titlePanel(title = '分市情况的展示'),
                        
                        '从省一级别下钻到市一级别，能够更加清楚的看到
                                各省下面具体疫情的情况，有利于掌握细节。',
                        
                        br(),
                        br(),
                        br(),
                        
                        ##第一行，用于选择省份和市区
                        fluidPage(column(width = 2),
                                  column(width = 2,
                                         selectInput('province', '请选择省份', choices = c('', unique(merge_data$province)))
                                  ),
                                  column(width = 2,
                                         selectInput('city', '请选择市级', choices = NULL)),
                                  column(width = 3,
                                         selectInput('map_city_choices', '请选择你想查看的类别',
                                                     choices = three_class_list)),
                                  column(width = 3,
                                         br(),
                                         actionButton('submit_prov_city', '点击查询'))
                        ),
                        
                        br(),
                        br(),
                        br(),
                        
                        ##第二行，用于点击“点击查询”
                        fluidPage(column(width = 2),
                                  column(width = 8,
                                         DTOutput('tb1')),
                                  column(width = 2)
                        ),
                        
                        br(),
                        br(),
                        br(),
                        
                        ##第三行，用于展示动态地图（市区级别）
                        fluidPage(column(width = 1),
                                  column(width = 10,
                                         echarts4rOutput('map_city')),
                                  column(width = 1)
                        )
                        
               ),
               
               
               ##第五页
               tabPanel('关于', icon = icon('list-alt'),
                        column(width = 8,
                               includeMarkdown('about.Rmd')),
                        column(width = 4,
                               
                               img(class = 'mypict',
                                   height = '300',
                                   width = '300',
                                   src = 'ccj.png'),
                               br(),
                               br(),
                               tags$small('目前就读于海淀区某取数专业'),
                               br(),
                               tags$small('特别感谢John Coene, Guangchuang Yu'),
                               br(),
                               tags$small(icon('envelope'), a(href = '', 'chenchijie97@163.com')),
                               br(),
                               tags$small(icon('linkedin'), a(href = '', 'Click here!')),
                               br(),
                               tags$small(icon('twitter'), a(href = '', 'Coming soon~'))
                        )
               )
               
    ))
)

##服务器端，用于数据的处理和输出展示
server <- function(input, output, session){
  
  ##这个是后台服务器数据计算的时候，一个加载的页面
  ##可以把这个需要很长时间运算的函数块放到load_in当中
  ##北大红
  
  
  ##这个是第三个页面画全国地区的操作
  draw_map <- reactive({
    
    waiter1 <- Waiter$new(html = spin_heart(), color = '#9f0d09')
    waiter1$show()
    on.exit(waiter1$hide())
    
    step_1 <- merge_data %>%
      filter(time %in% seq(ymd('2020-02-07'), ymd('2020-02-29'), by = 'day')) %>%
      group_by(time, province) %>%
      summarise(confirm = sum(!!sym(input$province_map1_choices))) %>%
      group_by(time) %>%
      e_charts(province, timeline = T) %>%
      em_map('China')
    
    step_2 <- do.call('e_map', list(e = step_1, serie = input$province_map1_choices, map = 'China'))
    #e_map(!!sym(input$three_class), map = 'China') %>%
    #e_visual_map(!!sym(input$three_class), scale = e_scale) %>%
    
    step_3 <- do.call('my_e_visual_map', list(e = step_2, serie = input$province_map1_choices, min = 0, max = 50)) %>%
      e_labels(color = 'black') %>%
      e_title(text = paste0('中国省级层面', names(three_class_list[three_class_list == input$province_map1_choices]), '热图'), subtext = '制作人：Chijie Chen', left = 250) %>%
      e_timeline_opts(
        playInterval = 1000,
        autoPlay = TRUE
      )
    
  })
  
  
  ################################################ 
  ##第三页交互的情况
  see_submit <- eventReactive(input$submit_class, {
    draw_map()
  })
  
  ##第三页画图
  output$map1 <- renderEcharts4r({
    req(input$province_map1_choices)
    see_submit()
  })
  ###############################################
  
  ##第四页级联显示省份和市区
  province <- reactive({
    filter(merge_data, province == input$province)
  })
  
  observeEvent(province(), {
    city <- unique(province()$city)
    updateSelectInput(session, 'city', choices = city)
  })
  
  #city_json <- eventReactive(province(),{
  #  geojsonio::geojson_list(china_sp[china_sp$NL_NAME_1 == input$province,])
  #})
  
  draw_map_city <- reactive({
    
    waiter2 <- Waiter$new(html = spin_flower(), color = '#9f0d09')
    waiter2$show()
    on.exit(waiter2$hide())
    
    
    china_json <- geojsonio::geojson_list(china_sp[china_sp$NL_NAME_1 == input$province,])
    
    
    china_json$features <- china_json$features %>% 
      purrr::map(function(x){ 
        x$properties$name <- x$properties$NL_NAME_2 # copy NAME_2 to name for tooltip
        return(x)
      })
    
    #print(china_json$features[[1]][[2]]$NL_NAME_1)
    #print(input$province)
    
    step1 <- merge_data %>%
      filter(province == input$province) %>%
      dplyr::select(time, city, !!sym(input$map_city_choices)) %>%
      group_by(time) %>%
      e_charts(city, timeline = TRUE) %>%
      e_map_register(name = input$province, json = china_json)
    
    step2 <- do.call('e_map_', list(e = step1, serie = input$map_city_choices, map = input$province))
    
    #print(step2$x$opts$baseOption$series[[1]][[2]])
    do.call('my_e_visual_map', list(e = step2, serie = input$map_city_choices, min = 0, max = 100)) %>%
      e_labels(color = 'black') %>%
      e_title(text = paste0(input$province, '境内市级层面', names(three_class_list[three_class_list == input$map_city_choices]), '热图'), subtext = '制作人：Chijie Chen', left = 220) %>%
      e_timeline_opts(
        playInterval = 1000,
        autoPlay = TRUE
      )
    
  })
  
  see_submit_city <- eventReactive(input$submit_prov_city, {
    draw_map_city()
  })
  
  ##第三页画图
  output$map_city <- renderEcharts4r({
    req(input$province)
    see_submit_city()
  })
  
  
  ##有关表格显示的部分
  see_submit_table <- eventReactive(input$submit_prov_city, {
    generate_table_data()
  })
  
  generate_table_data <- reactive({
    #waiter3 <- Waiter$new(html = spin_flower(), color = '#9f0d09')
    #waiter3$show()
    #on.exit(waiter3$hide())
    
    merge_data %>%
      filter(province == input$province, city == input$city) %>%
      dplyr::select(time, city, !!sym(input$map_city_choices)) %>%
      datatable() %>%
      formatStyle(
        input$map_city_choices,
        backgroundColor = styleInterval(c(0,5,10), c('white', '#FFEFD5', '#FFA07A', '#FF6347'))
      )
  })
  
  ##画DT表格
  output$tb1 <- renderDT({
    req(input$province, input$submit_prov_city)
    see_submit_table()
    
    
  })
  
  draw_barplot <- reactive({
    
    waiter3 <- Waiter$new(html = spin_whirly(), color = '#9f0d09')
    waiter3$show()
    on.exit(waiter3$hide())
    
    
    
    merge_data %>%
      filter(time == input$date) %>%
      select(province, !!sym(input$barplot_class)) %>%
      group_by(province) %>%
      summarise(!!sym(input$barplot_class):=sum(!!sym(input$barplot_class))) %>%
      mutate(color = case_when(0 < !!sym(input$barplot_class) & !!sym(input$barplot_class) <=(quantile(!!sym(input$barplot_class), 0.2)) ~ 1,
                               (quantile(!!sym(input$barplot_class), 0.2)) < !!sym(input$barplot_class) & !!sym(input$barplot_class) <= (quantile(!!sym(input$barplot_class), 0.4)) ~ 2,
                               (quantile(!!sym(input$barplot_class), 0.4)) < !!sym(input$barplot_class) & !!sym(input$barplot_class) <= (quantile(!!sym(input$barplot_class), 0.6)) ~ 3,
                               (quantile(!!sym(input$barplot_class), 0.6)) < !!sym(input$barplot_class) & !!sym(input$barplot_class) <= (quantile(!!sym(input$barplot_class), 0.8)) ~ 4,
                               (quantile(!!sym(input$barplot_class), 0.8)) < !!sym(input$barplot_class) ~ 5)) %>%
      ggplot() +
      geom_bar(aes(x = province, y = !!sym(input$barplot_class), fill = as.factor(color)), stat = 'identity') +
      coord_flip() +
      theme_bw() +
      labs(x = '省份',
           y = paste0(names(three_class_list[three_class_list == input$barplot_class]), '/人'),
           title = paste0(input$date, '新型冠状病毒在中国各省、市、自治区', names(three_class_list[three_class_list == input$barplot_class]), '数量统计直方图\n'),
           subtitle = '制作人：Chijie Chen') +
      scale_fill_brewer(palette="Set1", guide = FALSE) +
      geom_text(aes(x = province, y = !!sym(input$barplot_class) + quantile(!!sym(input$barplot_class), 0.8), label= !!sym(input$barplot_class)), color="black", size=3.5) +
      theme(plot.title = element_text(hjust = 0.5, size = 20))
    
  })
  
  
  draw_lines <- reactive({
    
    merge_data %>%
      group_by(time) %>%
      summarise(confirm = sum(confirm),
                heal = sum(heal),
                dead = sum(dead)) %>%
      ggplot(aes(x = time)) +
      geom_line(aes(y = confirm, color = 'co1'), size = 2) +
      geom_point(aes(y = confirm), size = 2) +
      geom_line(aes(y = heal, color = 'co2'), size = 2) +
      geom_point(aes(y = heal), size = 2) +
      geom_line(aes(y = dead, color = 'co3'), size = 2) +
      geom_point(aes(y = dead), size = 2) +
      theme_bw() +
      scale_color_manual(values = c('#228B22',
                                    '#6495ED',
                                    '#EE2C2C'),
                         name = '三种不同类型的人数统计',
                         breaks = c('co1', 'co2', 'co3'),
                         labels = c('全国新增确诊患者人数',
                                    '全国新增死亡患者人数',
                                    '全国新增治愈患者人数')) +
      labs(x = '时间', y = '人数/人',
           title = '新型冠状病毒在中国的发展趋势折线图\n',
           subtitle = '制作人：Chijie Chen'
      ) +
      theme(axis.text.x = element_text(size = 10)) +
      scale_x_date(guide = guide_axis(angle = 30), breaks = seq(ymd('2020-02-07'), ymd('2020-03-14'), by = 2)) +
      theme(plot.title = element_text(hjust = 0.5, size = 20))
    
  })
  
  output$barplot <- renderPlot({
    draw_barplot()
  })
  
  output$lineplot <- renderPlot({
    draw_lines()
  })
  
}

shinyApp(ui, server)
