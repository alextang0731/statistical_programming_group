mf <- 5 #number of control station (french)
mb <- 5 #number of control station (british)
a.rate <- .1 #probability of a car arriving each second
trb <- 40 #uniform distribuition parameter (british)
tmb <- 30 #uniform distribuition parameter (british)
trf <- 40 #uniform distribuition parameter (french)
tmf <- 30 #uniform distribuition parameter (french)
#remark: uniform between tmb and tmb+trb
maxb <- 20 #maximum british queue length (per station)


interarrival_time <- c(0)
service_time <- c()
service_time_begin <- c()
service_time_end <- c()
arrival_time <- c(0)
waiting_time <- c()
c <- 0
a_time <- 0
s_time_begin <- 0
s_time_end <- 0
w_time<-0

service_time_end_by_counter <- c(rep(0,mf))
service_time_begin_by_counter <- c(rep(0,mf))
service_time_by_counter <- c(rep(0,mf))

service_time_end_by_counter_db <- c()
service_time_begin_by_counter_db <- c()
service_time_by_counter_db <- c()
number_car <-c(rep(0,mf))
number_car_list <-c()


counter <- 0
counter_go <- c()

idle <- rep(TRUE,mf)
idle_list <- c()

repeat {
  counter <- which.min(number_car)
  idle[counter] <- FALSE
  counter_go <- append(counter_go, counter)
  
  
  s_time <- runif(1, min = tmf, max = tmf+trf) #service time
  service_time <- append(service_time, s_time)
  service_time_by_counter[counter] <- s_time
  
  #s_time_end <- s_time_begin + s_time
  #s_time_end <- service_time_begin_by_counter[counter] + service_time_by_counter[counter]
  
  #service_time_end_by_counter[counter] <- s_time_end
  #service_time_end <- append(service_time_end, s_time_end)
  
  
  
  if(s_time_begin > a_time){
    w_time <- s_time_begin - a_time
  }else{
    w_time <- 0
  }
  c <- c + 1
  
  
  i<-1
  for (idl in idle){
    if (c>1 & !idl & i != counter & service_time_end_by_counter[i] > a_time){ #need wait
      number_car[i] <-number_car[i] 
    }else if (c>1 & !idl & i != counter & service_time_end_by_counter[i] <= a_time){
      if (as.double(strsplit(number_car_list[c-1], ",")[[1]])[i] > 0){
        number_car[i] <- as.double(strsplit(number_car_list[c-1], ",")[[1]])[i] - 1
      }else{
        number_car[i] <- 0
      }
    }else if (c>1 & !idl & i == counter & service_time_end_by_counter[counter] == 0){
      s_time_begin <- a_time
      service_time_begin_by_counter[counter] <-s_time_begin
      number_car[counter] <-as.double(strsplit(number_car_list[c-1], ",")[[1]])[counter] + 1
    }else if (c>1 & !idl &i == counter & service_time_end_by_counter[counter] > a_time){ #need wait
      w_time <- service_time_end_by_counter[counter] - a_time
      if(service_time_begin_by_counter[counter] != 0){
        s_time_begin <- service_time_begin_by_counter[counter]
        service_time_begin_by_counter[counter] <- service_time_end_by_counter[counter]
      }else{
        s_time_begin <- a_time
        service_time_begin_by_counter[counter] <- a_time
      }
      number_car[counter] <-as.double(strsplit(number_car_list[c-1], ",")[[1]])[counter] + 1 
    }else if(c>1 & !idl & i == counter & service_time_end_by_counter[i] < a_time){
      s_time_begin <- a_time
      service_time_begin_by_counter[counter] <- a_time
      if ( as.double(strsplit(number_car_list[c-1], ",")[[1]])[i] > 0){
        #number_car[counter] <- as.double(strsplit(number_car_list[c-1], ",")[[1]])[i] 
        number_car[counter] <- 1
      }else{
        number_car[counter] <- 1
      }
    }
    i <- i +1
  }

  s_time_end <- service_time_begin_by_counter[counter] + service_time_by_counter[counter]
  service_time_end_by_counter[counter] <- s_time_end
  service_time_end <- append(service_time_end, s_time_end)
  number_car_list <- append(number_car_list,toString(number_car))
  
  service_time_end_by_counter_db <- append(service_time_end_by_counter_db, toString(round(service_time_end_by_counter, digits = 1)))
  service_time_begin_by_counter_db <- append(service_time_begin_by_counter_db, toString(round(service_time_begin_by_counter, digits = 1)))
  service_time_by_counter_db <- append(service_time_by_counter_db, toString(round(service_time_by_counter, digits = 1)))
  
  if(c==1){
    number_car[counter] <- 1
    s_time_end <- service_time_begin_by_counter[counter] + service_time_by_counter[counter]
    service_time_end_by_counter[counter] <- s_time_end
    s_time_begin <- a_time
  }
  idle_list <- append(idle_list,toString(idle))
  
  
  waiting_time <- append(waiting_time,w_time)
  
  i_time <- rpois(1,1/a.rate) #interarrival time
  a_time <- a_time + i_time #arrival time
  
  service_time_begin <- append(service_time_begin,s_time_begin)
  
  
  if(a_time > 7020) break
  
  interarrival_time <- append(interarrival_time, i_time) 
  arrival_time <- append(arrival_time , a_time)
  service_time_by_counter[counter] <- s_time_end
  
  
  
  
}

queue <- data.frame(car = rep(1:c),
                    counter=counter_go,
                    interarrival_time=interarrival_time, 
                    arrival_time=arrival_time,
                    waiting_time=waiting_time,
                    service_time=service_time,
                    service_time_begin=service_time_begin,
                    service_time_end=service_time_end,
                    number_car = number_car_list,
                    service_time_begin_by_counter_db=service_time_begin_by_counter_db,
                    service_time_end_by_counter_db=service_time_end_by_counter_db,
                    service_time_by_counter_db=service_time_by_counter_db
                    
)

aggregate(waiting_time~counter, queue, sum)







