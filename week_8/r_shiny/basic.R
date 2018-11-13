source("Global.R", local  = TRUE)

## list ==============

price = read.csv("./Data/tw50_price.csv", fileEncoding = "UTF-8")
feature_list = c("Eps", "Div", "Current Ratio", "Hold")
target_list = c("Return", "Sharp","Cluster")


## monte carlos===================================

choose_target<-function(target){
  target_t = c("Date")
  for(i in c(1:length(target))){
    target_t = c(target_t, paste(c(target[i],".TW"), collapse=""))
    
  }
  return(target_t)
  
}
target = c("X1101", "X1102", "X1216")
target = choose_target(target)


price = read.csv("Data/tw50_price.csv")[target]
price_series <- xts(price[,2:(2+length(target)-2)], order.by=as.Date(price[,1]))
monthly_serie <- to.monthly(price_series, indexAt = "last", OHLC = FALSE)
price_rr = ROC(x = price_series)
asset_returns_xts <-na.omit(Return.calculate(monthly_serie, method = "log"))

cul_rt = data.matrix(colMeans(asset_returns_xts[69:80]))
cov_m = data.matrix(cov(asset_returns_xts[69:80]))


cri = "r/s+s"

criteria <- function(rt, st){
  temp = rt/st+20*rt
  return(temp)
  
}

t_rt = c()
t_var = c()
special = 0
max_sharp=0
for(i in c(1:5000)){
  weights = runif(length(target)-1)
  weights = matrix(weights/sum(weights))
  rt = sum(cul_rt*weights)
  t_rt = c(t_rt,rt)
  var_wei = t(weights) %*% cov_m %*% weights
  t_var = c(t_var,var_wei**(1/2))
  temp = criteria(rt, var_wei**(1/2))
  if(temp>max_sharp){
    max_sharp = temp
    opt_w = weights
    opt_r = rt
    opt_var = var_wei
    special = i
  }
  
}

special_list = rep(30, 5000)
special_list[special]=0

fro_data = data_frame(t_var, t_rt,special_list)
names(fro_data) <- c("pro_Std", "Return","opt")
ggplot(data=fro_data, aes(x=pro_Std, y=Return)) + geom_point(color="yellow")+geom_point(data=fro_data[special, ], aes(x=pro_Std, y=Return), colour="red", size=3)





##box_plot
price = read.csv("./Data/tw50_price.csv", fileEncoding = "UTF-8")
col = names(price)
target = c()
for(i in c(2:length(col))){
  target = c(target, substr(col[i],1,5))
  
}
eps = read.csv("./Data/EPS.csv", fileEncoding = "UTF-8")[target]

div = read.csv("./Data/div_data.csv", fileEncoding = "UTF-8")[target]

current_rt = read.csv("./Data/current ratio(%).csv", fileEncoding = "UTF-8")[target]

hold = read.csv("./Data/Stockholding.csv", fileEncoding = "UTF-8")[target]


eps = tail(eps,4)
mean_eps = colMeans((eps))
eps_data = data_frame(names(mean_eps), mean_eps)
names(eps_data) <- c("code", "Eps")
ggplot()+geom_boxplot(data = eps_data, aes(x =rep(0, length(mean_eps)),y=Eps),outlier.shape = NA)+scale_y_continuous(limits = quantile(eps_data$Eps, c(0.2, 0.8)))


current_rt = tail(current_rt,4)
mean_current_rt = colMeans((current_rt))
current_rt_data = data_frame(names(mean_current_rt), mean_current_rt)
names(current_rt_data) <- c("code", "current_rt")

ggplot() + geom_boxplot(data = current_rt_data
                        , aes(x = rep(0, length(mean_current_rt)), y= current_rt),outlier.shape = NA)+scale_y_continuous(limits = quantile(current_rt_data$current_rt, c(0.3, 0.7)))

div = tail(div,1)
mean_div = colMeans((div))
div_data = data_frame(names(mean_div), mean_div)
names(div_data) <- c("code", "div")
ggplot()+geom_boxplot(data = div_data, aes(x =rep(0, length(mean_div)),y=div),outlier.shape = NA)+scale_y_continuous(limits = quantile(div_data$div, c(0.2, 0.8)))


hold = tail(hold,13)[1:12,]
mean_hold = colMeans((hold))
hold_data = data_frame(names(mean_hold), mean_hold)
names(hold_data) <- c("code", "hold")
ggplot()+geom_boxplot(data = hold_data, aes(x =rep(0, length(mean_hold)),y=hold))#,outlier.shape = NA)+scale_y_continuous(limits = quantile(hold_data$hold, c(0.2, 0.8)))


### pca_and_kmeans==========

price = read.csv("./Data/tw50_price.csv",fileEncoding = "UTF-8")

price_series <- xts(price[,2:48], order.by=as.Date(price[,1]))
monthly_series <- to.monthly(price_series, indexAt = "last", OHLC = FALSE)
asset_returns_xts <-na.omit(Return.calculate(monthly_series, method = "log"))
asset_returns_xts = tail(asset_returns_xts,12)
mean_rt = round(colMeans(asset_returns_xts),digit = 3)
col_std = colSds(asset_returns_xts)
Sharp_rt = mean_rt/col_std

feature_data = data_frame(names(mean_hold),col_std,mean_hold,mean_eps,mean_div,mean_current_rt)
code_list = names(mean_hold)
names(feature_data) = c("code","STD", "Hold", "Eps", "Div", "curr_Rt")

pca <- prcomp(formula = ~ STD+Hold+Eps+Div+curr_Rt, data = feature_data,scale = TRUE)   
k_author = 4

kmeansData_code = pca$x[,1:2]
# kmeansData = kmeansData[kmeansData[,1] > -0.05, ]

cl_code <- kmeans(kmeansData_code, k_author)
kmeansData_code <- as.data.frame(kmeansData_code) 
kmeansData_code$cl <- as.factor(cl_code$cluster)
kmeansData_code$code <- feature_data$code
kmeansData_code$Return <- mean_rt
kmeansData_code$Sharp <- Sharp_rt
ggplot(data = kmeansData_code , aes(x = PC1, y=PC2, color = cl, label = Return)) + geom_point()+geom_point(data = kmeansData_code[kmeansData_code["code"]=="X2330",],aes(x = PC1, y=PC2), color = "red",size = 3)





##UI =======================================================================
ui <- navbarPage(
  
  theme = shinythemes::shinytheme("flatly"),
  
  # Application title
  "簡單選股系統與個人化投組",
  
  tabPanel(
    "簡介",
    tags$h2("簡介"),br(),
    tags$h5("作者:游博任"),br(),br(),
    tags$h4("本系統有三個部分，包含:"),br(),
    tags$h4("1, 基本資料的分析"),br(),
    tags$h4("2, 利用pca與kmeans等技術進行選股"),br(),
    tags$h4("3, 利用所選的股票使用蒙地卡羅法計算效率前緣並且"),br(),
    tags$h4("   並且找出最適投組"),br(),
    tags$h5("資料來源： python data_reader, gooinfo")
  ),
  tabPanel(
    "基本資料分析",
    tags$h2("特徵分析"),br(),
    sidebarPanel(
      selectInput("feature1", "特徵:",
                  choices=feature_list),
      hr(),
      helpText("此特徵的箱型圖")
    ),
    mainPanel(
      plotOutput("Boxplot_1")
    )
  ),
  tabPanel(
    "pca進行選股",
    tags$h2("選股分析"),br(),
    sidebarPanel(
      selectInput("target", "比較目標:",
                  choices=target_list),
      hr(),
      numericInput("k1",
                   "Number of k:",
                   min = 2,
                   max = 4,
                   value = 3),
      hr(),
      selectInput("code", "特定股票:",
                  choices = code_list),
      helpText("pca之分散圖")
    ),
    mainPanel(
      plotOutput("Pca_plot_1"),
      tableOutput("values2")
    )
  ),
  tabPanel(
    "自組效率前緣",
    tags$h2("自組效率前緣"),br(),
    sidebarPanel(
      selectInput("code2", "特定股票:",
                  choices = code_list),
      hr(),
      selectInput("code3", "特定股票:",
                  choices = code_list),
      hr(),
      selectInput("code4", "特定股票:",
                  choices = code_list),
      hr(),
      selectInput("code5", "特定股票:",
                  choices = code_list),
      hr(),
      helpText("最佳化目標為sharp_ratio "),
      helpText("+ (Target Return)*Rt +(Target Std)*Std" ),
      numericInput("k2",
                   "Target Return:",
                   min = -30,
                   max = 30,
                   value = 0),
      hr(),
      numericInput("k3",
                   "Target Std:",
                   min = -30,
                   max = 30,
                   value = 0),
      hr(),
      numericInput("k4",
                   "Number of Iter:",
                   min = 5000,
                   max = 50000,
                   value = 5000),
      helpText("pca之分散圖")
    ),
    mainPanel(
      plotOutput("f_plot_1"),
      tableOutput("values")
    )
  )
  
  
  
)
# 
 
# showtext_auto(enable = TRUE)
# font_add("康熙字典體", "康熙字典體.otf")

##SERVER =====================================================================
server <- function(input, output) {
  
  output$Boxplot_1 <- renderPlot({
    fea = input$feature1
    print(fea)
    if (fea == "Eps"){
      res <- ggplot()+geom_boxplot(data = eps_data, aes(x =rep(0, length(mean_eps)),y=Eps),outlier.shape = NA)+scale_y_continuous(limits = quantile(eps_data$Eps, c(0.2, 0.8)))
      res = res+labs(x=fea, y='numeric') 
    }
    if (fea == "Div"){
      res <- ggplot()+geom_boxplot(data = div_data, aes(x =rep(0, length(mean_div)),y=div),outlier.shape = NA)+scale_y_continuous(limits = quantile(div_data$div, c(0.2, 0.8)))
      res = res+labs(x=fea, y='numeric') 
      }
    if (fea == "Current Ratio"){
      res <- ggplot() + geom_boxplot(data = current_rt_data, aes(x = rep(0, length(mean_current_rt)), y= current_rt),outlier.shape = NA)+scale_y_continuous(limits = quantile(current_rt_data$current_rt, c(0.3, 0.7)))
      res = res+labs(x=fea, y='numeric') 
      }
    if (fea == "Hold"){
      res <- ggplot()+geom_boxplot(data = hold_data, aes(x =rep(0, length(mean_hold)),y=hold))        
      res = res+labs(x=fea, y='numeric') 
    
      }
    showtext.begin()
    print(res)
    showtext.end()
  })
  
  output$Pca_plot_1 <- renderPlot({
    tar = input$target
    print(tar)
    k = input$k1
    print(k)
    cod = input$code
    print(cod)
    
    kmeansData_code = pca$x[,1:2]
    # kmeansData = kmeansData[kmeansData[,1] > -0.05, ]
    
    cl_code <- kmeans(kmeansData_code, k)
    kmeansData_code <- as.data.frame(kmeansData_code) 
    kmeansData_code$cl <- as.factor(cl_code$cluster)
    kmeansData_code$code <- feature_data$code
    kmeansData_code$Return <- mean_rt
    kmeansData_code$Sharp <- Sharp_rt
    if(tar == "Return"){
      res = ggplot(data = kmeansData_code , aes(x = PC1, y=PC2, color = Return, label = Return)) + geom_point()
      
    }
    if(tar == "Sharp"){
      res = ggplot(data = kmeansData_code , aes(x = PC1, y=PC2, color = Sharp, label = Return)) + geom_point()
      
    }
    if(tar == "Cluster"){
      res = ggplot(data = kmeansData_code , aes(x = PC1, y=PC2, color = cl, label = Return)) + geom_point()
    }
    res = res+geom_point(data = kmeansData_code[kmeansData_code["code"]==cod,],aes(x = PC1, y=PC2), color = "red",size = 3)
    
    
    showtext.begin()
    print(res)
    showtext.end()
  })
  
  output$values2 <- renderTable({
    tar = input$target
    print(tar)
    k = input$k1
    print(k)
    
    kmeansData_code = pca$x[,1:2]
    # kmeansData = kmeansData[kmeansData[,1] > -0.05, ]
    
    cl_code <- kmeans(kmeansData_code, k)
    kmeansData_code <- as.data.frame(kmeansData_code) 
    kmeansData_code$cl <- as.factor(cl_code$cluster)
    
    kmeansData_code$Return <- mean_rt
    kmeansData_code$Sharp <- Sharp_rt
    kmeansData_code$code <- feature_data$code
    
    kmeansData_code
    
  })
  
  
  
  output$f_plot_1 <- renderPlot({
    cod1 = input$code2
    cod2 = input$code3
    cod3 = input$code4
    cod4 = input$code5
    k2 = input$k2
    k3 = input$k3
    k4 = input$k4
    all_target = c(cod1,cod2,cod3,cod4)
    print(all_target)
    all_target = choose_target(all_target)
    print(all_target)
    price = read.csv("Data/tw50_price.csv")[all_target]
    price_series <- xts(price[,2:(2+length(all_target)-2)], order.by=as.Date(price[,1]))
    monthly_serie <- to.monthly(price_series, indexAt = "last", OHLC = FALSE)
    price_rr = ROC(x = price_series)
    asset_returns_xts <-na.omit(Return.calculate(monthly_serie, method = "log"))
    
    cul_rt = data.matrix(colMeans(asset_returns_xts[69:80]))
    cov_m = data.matrix(cov(asset_returns_xts[69:80]))
    
    criteria <- function(rt, st,num_std, num_return){
      temp = rt/st + num_return*rt +num_std*st
      return(temp)
      
    }
    
    t_rt = c()
    t_var = c()
    special = 0
    max_sharp=0
    for(i in c(1:k4)){
      weights = runif(length(all_target)-1)
      weights = matrix(weights/sum(weights))
      rt = sum(cul_rt*weights)
      t_rt = c(t_rt,rt)
      var_wei = t(weights) %*% cov_m %*% weights
      t_var = c(t_var,var_wei**(1/2))
      temp = criteria(rt, var_wei**(1/2),k2,k3)
      if(temp>max_sharp){
        max_sharp = temp
        opt_w = weights
        opt_r = rt
        opt_var = var_wei
        special = i
      }
      
    }
    
    special_list = rep(0, k4)
    special_list[special]=0
    
    fro_data = data_frame(t_var, t_rt,special_list)
    names(fro_data) <- c("pro_Std", "Return","opt")
    res = ggplot(data=fro_data, aes(x=pro_Std, y=Return)) + geom_point(color="yellow")+geom_point(data=fro_data[special, ], aes(x=pro_Std, y=Return), colour="red", size=3)
    
    
    
    
    showtext.begin()
    print(res)
    showtext.end()
  })
  
  # Show the values in an HTML table ----
  output$values <- renderTable({
    cod1 = input$code2
    cod2 = input$code3
    cod3 = input$code4
    cod4 = input$code5
    k2 = input$k2
    k3 = input$k3
    k4 = input$k4
    all_target = c(cod1,cod2,cod3,cod4)
    all_target = choose_target(all_target)
    print(all_target)
    price = read.csv("Data/tw50_price.csv")[all_target]
    price_series <- xts(price[,2:(2+length(all_target)-2)], order.by=as.Date(price[,1]))
    monthly_serie <- to.monthly(price_series, indexAt = "last", OHLC = FALSE)
    price_rr = ROC(x = price_series)
    asset_returns_xts <-na.omit(Return.calculate(monthly_serie, method = "log"))
    
    cul_rt = data.matrix(colMeans(asset_returns_xts[69:80]))
    cov_m = data.matrix(cov(asset_returns_xts[69:80]))
    
    criteria <- function(rt, st,num_std, num_return){
      temp = rt/st + num_return*rt +num_std*st
      return(temp)
      
    }
    
    t_rt = c()
    t_var = c()
    special = 0
    max_sharp=0
    for(i in c(1:k4)){
      weights = runif(length(all_target)-1)
      weights = matrix(weights/sum(weights))
      rt = sum(cul_rt*weights)
      t_rt = c(t_rt,rt)
      var_wei = t(weights) %*% cov_m %*% weights
      t_var = c(t_var,var_wei**(1/2))
      temp = criteria(rt, var_wei**(1/2),k2,k3)
      if(temp>max_sharp){
        max_sharp = temp
        opt_w = weights
        opt_r = rt
        opt_var = var_wei
        special = i
      }
      
    }
    
    
    data.frame(Name = c("Return","Var","Criteria","1","2","3","4"),
               Value = as.character(c(opt_r,opt_var,max_sharp,opt_w[1],opt_w[2],opt_w[3],opt_w[4] )),
                      stringsAsFactors = FALSE)
    
  })
  
  
}
# fviz_eig(pcat_author)
# fviz_pca_ind(pcat_author, geom= c("point","text","arrow"), col.ind = "cos2")
# fviz_pca_var(pcat_author, col.var = "contrib")
# Run the application 
shinyApp(ui = ui, server = server)
