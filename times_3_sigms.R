library(lubridate)
library(influxdbr)
#Получим данные, необходимые для анализа времени подключения к серверу
get_box_time <- function(protocol = "http", ip = "192.168.159.145", p = 8086, db_name = "csv"){
  #Сначала получим из БД имена всех почтовых ящиков
  conn <- influx_connection(scheme = protocol,host = ip,port = p)
  res_measurements <<- influx_query(conn, db = db_name, query = "SHOW MEASUREMENTS", return_xts = FALSE, chunked = FALSE, simplifyList = FALSE)
  n <<- 0
  #Создадим список для дальнейшего хранения временных меток подключения
  #к серверу каждого из почтовых ящиков
  time_box_conn <<- list()
  name_box <- res_measurements[[1]]$name
  #Заполним список с временными метками каждого почтового ящика
  for (i in name_box) {
    time_box_conn[n] <<- influx_query(conn, db = db_name, query = paste("SELECT Process FROM \"",i,"\" WHERE Process='connect'", sep = ""), return_xts = FALSE, chunked = FALSE, simplifyList = FALSE)
    n <<- n+1
  }
  n <<- n-1
}
get_box_time()
#Пройдем по всем n почтовым ящикам, анализируя время их подключения
for (i in c(1:n)) {
  #Изначально принимаем условие отсутствия аномалий для каждого почтового ящика
  anomaly <- FALSE
  #Создаем для каждого почтового ящика вектор, содержащий часы
  #в которые этот почтовый ящик подключался к серверу
  hour_box <- c(hour(time_box_conn[[i]]$time))
  x_sred <- mean(hour_box) #Найдём среднее вектора часов
  std_otkl <- sd(hour_box) #Найдём стандартное отклонение в векторе часов
  #Если имеется более 1 записи о подключениях, то мы сможем рассчитать все параметры
  if (length(hour_box) > 1){
    #Каждое значение в векторе часов проверяем на выброс 
    for (a in c(1:(length(hour_box)))) {
      #Правило трёх сигм
      if (abs(hour_box[a] - x_sred) > (3 * std_otkl)){
        
        cat("Выброс ",hour_box[a]," числа\n")
        anomaly <- TRUE
                                                      } 
                                        }
  }else cat("Мало данных\n")
  if (anomaly){
    
  plot(time_box_conn[[i]]$time,hour_box, type = "b",col="red", xlab = time_box_conn[[i]]$series_names,ylab = "Время подключения")
  Sys.sleep(3)
              }
}


