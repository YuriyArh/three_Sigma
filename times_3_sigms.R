library(lubridate)
library(influxdbr)
#������� ������, ����������� ��� ������� ������� ����������� � �������
get_box_time <- function(protocol = "http", ip = "192.168.159.145", p = 8086, db_name = "csv"){
  #������� ������� �� �� ����� ���� �������� ������
  conn <- influx_connection(scheme = protocol,host = ip,port = p)
  res_measurements <<- influx_query(conn, db = db_name, query = "SHOW MEASUREMENTS", return_xts = FALSE, chunked = FALSE, simplifyList = FALSE)
  n <<- 0
  #�������� ������ ��� ����������� �������� ��������� ����� �����������
  #� ������� ������� �� �������� ������
  time_box_conn <<- list()
  name_box <- res_measurements[[1]]$name
  #�������� ������ � ���������� ������� ������� ��������� �����
  for (i in name_box) {
    time_box_conn[n] <<- influx_query(conn, db = db_name, query = paste("SELECT Process FROM \"",i,"\" WHERE Process='connect'", sep = ""), return_xts = FALSE, chunked = FALSE, simplifyList = FALSE)
    n <<- n+1
  }
  n <<- n-1
}
get_box_time()
#������� �� ���� n �������� ������, ���������� ����� �� �����������
for (i in c(1:n)) {
  #���������� ��������� ������� ���������� �������� ��� ������� ��������� �����
  anomaly <- FALSE
  #������� ��� ������� ��������� ����� ������, ���������� ����
  #� ������� ���� �������� ���� ����������� � �������
  hour_box <- c(hour(time_box_conn[[i]]$time))
  x_sred <- mean(hour_box) #����� ������� ������� �����
  std_otkl <- sd(hour_box) #����� ����������� ���������� � ������� �����
  #���� ������� ����� 1 ������ � ������������, �� �� ������ ���������� ��� ���������
  if (length(hour_box) > 1){
    #������ �������� � ������� ����� ��������� �� ������ 
    for (a in c(1:(length(hour_box)))) {
      #������� ��� ����
      if (abs(hour_box[a] - x_sred) > (3 * std_otkl)){
        
        cat("������ ",hour_box[a]," �����\n")
        anomaly <- TRUE
                                                      } 
                                        }
  }else cat("���� ������\n")
  if (anomaly){
    
  plot(time_box_conn[[i]]$time,hour_box, type = "b",col="red", xlab = time_box_conn[[i]]$series_names,ylab = "����� �����������")
  Sys.sleep(3)
              }
}


