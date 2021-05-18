if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(utils)     # untar

#
# download time out.  if you have slow internet, you may need to
# increase this value.  it is measured in seconds
#
download_timeout <- 600 # seconds == 10 minutes

#
# url of data file.  important stats about data file etc
#
on_time_performance_url <- "https://dax-cdn.cdn.appdomain.cloud/dax-airline/1.0.1/airline_2m.tar.gz"
on_time_performance_gz <- "airline_2m.tar.gz"
on_time_performance_gz_size <- 151681776
on_time_performance_csv <- "airline_2m.csv"
on_time_performance_csv_size <- 882162600
on_time_performance_rda <- "on_time_performance.rda"

#
# intermediate objects
#
validation_rda <- "validation.rda"
non_validation_rda <- "non_validation.rda"
test_rda <- "test.rda"
train_rda <- "train.rda"
investigate <- FALSE
#
# airport to investigate arrivals
#
destination_airport <- "LAX"
#
# percentage for validation set and then test set
#
percent <- 0.1         
#
# which iteration to run
#
build_late_models_iteration_1 <- TRUE
build_late_models_iteration_2 <- TRUE
build_late_models_iteration_3 <- TRUE
build_late_models_iteration_4 <- TRUE


#
# functions for time handling
#
previous_half_hour <- function(time) {
  minute <- time %% 100
  hour <- time - minute
  minute <- ifelse(minute < 30, 0, 30)
  hour+minute
}

RMSE <- function(true_ratings, predicted_ratings){
        sqrt(mean((true_ratings - predicted_ratings)^2))
}


#
# if the gz file has not been downloaded or if it is
# the wrong size, download it
#
if ( (!file.exists(on_time_performance_gz)) ||
     (file.info(on_time_performance_gz)$size != on_time_performance_gz_size) ) {

	#
	# Download gz file
	#
	print(paste("file", on_time_performance_gz, "does not exist or is the wrong size", sep=" "))
	print("Downloading it")
	options(timeout = download_timeout)
	download.file(on_time_performance_url, on_time_performance_gz)
	print(paste("Downloaded", file.info(on_time_performance_gz)$size, "bytes"))

	#
	# check to see if the entire file got downloaded
	#
	if ( file.info(on_time_performance_gz)$size == on_time_performance_gz_size) {
		print(paste(on_time_performance_gz, "downloaded OK", sep=" "))
	} else {
		print(paste(on_time_performance_gz, "was not completely downloaded", sep=" "))
		print(paste("You can download it through your browser by pointing your browser to"))
		print(on_time_performance-url)
		stop()
	}
}

#
# if the csv file does not exist or is the wrong size
# or if it is older than the gz file, then untar the
# gz file
#
if ( (!file.exists(on_time_performance_csv)) |
     (file.info(on_time_performance_csv)$size != on_time_performance_csv_size) ||
     (file.info(on_time_performance_gz)$ctime > file.info(on_time_performance_csv)$ctime) ) {
   
	#
	# untar (inclues gz decompress) 
	#
	print(paste("Untaring (will also do a gz decompress)", on_time_performance_gz, sep=" "))
	untar(on_time_performance_gz)

	#
	# check to see if the files sizes are correct
	#
	if ( file.info(on_time_performance_csv)$size == on_time_performance_csv_size) {
		print(paste(on_time_performance_csv, "untared OK", sep=" "))
	} else {
		print(paste(on_time_performance_csv, "was not untarred correctly", sep=" "))
		print(paste("You can try decrompressing and untaring it your self and rerunning this script"))
		stop()
	}
}

#
# if the on_time_performance_rda file does not exist,
# or if the csv file is younger than it, then rebuild it
#
if ( (!file.exists(on_time_performance_rda)) ||
     (file.info(on_time_performance_csv)$ctime > file.info(on_time_performance_rda)$ctime) ) {

	#
	# read in the csv
	#
	print("Reading data file")
	csv <- read.csv(on_time_performance_csv)
	print("read csv")

	#
	# filter out only flights landing at destination_airport
	# select the columns which might interest us (human choice)
	# only keep complete cases
	# add a flag for Late
	#
	on_time_performance <- csv %>%
		filter(Dest == destination_airport) %>%
		select(Year, Month, DayofMonth, DayOfWeek, 
 			DOT_ID_Reporting_Airline,
			Flight_Number_Reporting_Airline,
			OriginAirportID, 
			Tail_Number,
			Distance,
			ArrDel15,
			AirTime,
			ArrDel15,
			DepTime, ArrTime, 
			DepDelay, ArrDelay) %>%
		filter(complete.cases(.)) %>%		# NB the full stop
		mutate(late = factor(ifelse(ArrDelay > 0, 1, 0)),
		       arrival_delay = factor(ArrDelay),
		       departure_slot = previous_half_hour(DepTime),
		       arrival_slot = previous_half_hour(ArrTime),
		       arrival_slot_number = factor(arrival_slot),
		       airline = factor(DOT_ID_Reporting_Airline),
		       flight_number = factor(Flight_Number_Reporting_Airline),
		       tail_number = factor(Tail_Number),
		       origin_airport = factor(OriginAirportID))
	print("filtered out NAs and selected columns")
	save(on_time_performance, file=on_time_performance_rda)
	print(paste("Saved", on_time_performance_rda))
}



#
# if validation, train, and test do not exist or if
# one of them is younger than the big rda
#
# make sure that the late flag is equitably distributed
#
if ( !file.exists(test_rda) || !file.exists(train_rda) || !file.exists(validation_rda) ||
     !file.exists(non_validation_rda) ||
     (file.info(on_time_performance_rda)$ctime > file.info(validation_rda)$ctime) ||
     (file.info(on_time_performance_rda)$ctime > file.info(non_validation_rda)$ctime) ||
     (file.info(on_time_performance_rda)$ctime > file.info(train_rda)$ctime) ||
     (file.info(on_time_performance_rda)$ctime > file.info(test_rda)$ctime)) {

	set.seed(1, sample.kind="Rounding")
	load(on_time_performance_rda)

	#
	# make validation and non-validation datasets
	#
	validation_index <- createDataPartition(y=on_time_performance$late, times=1, p=percent, list=FALSE)
	validation <- on_time_performance[validation_index,]
	non_validation <- on_time_performance[-validation_index,]


	#
	# divide the non-validation dataset into test and train
	#
	test_index <- createDataPartition(y=non_validation$late, times=1, p=percent, list=FALSE)
	test <- non_validation[test_index,]
	train <- non_validation[-test_index,]

	#
	# print out stats on these subsets
	#
	#print(paste("non-validation has", nrow(non_validation), "rows with", sum(non_validation$late), 
		#"late", sum(non_validation$late)*100/nrow(non_validation), "%", sep=" "))
	#print(paste("validation has", nrow(validation), "rows with", sum(validation$late), 
		#"late", sum(validation$late) * 100 / nrow(validation), "%", sep=" "))
	#print(paste("train has", nrow(train), "rows with", sum(train$late), 
		#"late", sum(train$late) * 100 / nrow(train), "%", sep=" "))
	#print(paste("test has", nrow(test), "rows with", sum(test$late == 1), 
		#"late", sum(test$late) * 100 / nrow(test), "%", sep=" "))

	#
	# save on intermediate data objects
	#
	save(test, file=test_rda)
	save(train, file=train_rda)
	save(validation, file=validation_rda)
	save(non_validation, file=non_validation_rda)

	#
	# free up memory
	#
	rm(test, train, validation, non_validation, on_time_performance)
}


#
# now investigate averages et
#
if ( investigate ) {
	print("in memory right now")
	ls()

	#
	# load the train and test set
	#
	load(train_rda)
	load(test_rda)
	print(paste("Training set has", nrow(train), "rows"))
	print(paste("Test set has", nrow(test), "rows"))

	#
	# naive model
	#
	overall_average = mean(train$ArrDelay)
	print(paste("Overall Average", overall_average))
	print(paste("RMSE", RMSE(test$ArrDelay, overall_average)))

	#
	# use only the airline
	#
	print("Airline effect (average)")
	airline_averages <- train %>% group_by(DOT_ID_Reporting_Airline) %>%
		summarize(airline_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(airline_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$airline_average)))

	#
	# use only the tail number (physical airplane)
	#
	print("Tail number effect (average)")
	tail_number_averages <- train %>% group_by(Tail_Number) %>%
		summarize(tail_number_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(tail_number_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$tail_number_average)))

	#
	# use only the origin airport
	#
	print("Origin airport effect (average)")
	origin_airport_averages <- train %>% group_by(OriginAirportID) %>%
		summarize(origin_airport_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(origin_airport_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$origin_airport_average)))

	#
	# use only the flight number (route)
	#
	print("Flight number effect (average)")
	flight_number_averages <- train %>% group_by(Flight_Number_Reporting_Airline) %>%
		summarize(flight_number_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(flight_number_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$flight_number_average)))

	#
	# use only the arrival_slot_number
	#
	print("Arrival slot number effect (average)")
	arrival_slot_averages <- train %>% group_by(as.numeric(arrival_slot_number)) %>%
		summarize(arrival_slot_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(arrival_slot_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$arrival_slot_average)))

	#
	# use only the arrival time
	#
	print("Arrival time (average)")
	arrival_time_averages <- train %>% group_by(ArrTime) %>%
		summarize(arrival_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(arrival_time_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$arrival_average)))

	#
	# use only the departure delay
	#
	print("Departure delay effect (average)")
	departure_delay_averages <- train %>% group_by(as.numeric(DepDelay)) %>%
		summarize(departure_delay_average = mean(ArrDelay), 
			  Flight_Number_Reporting_Airline = first(Flight_Number_Reporting_Airline))
	model <- test %>% inner_join(departure_delay_averages,
		by = "Flight_Number_Reporting_Airline")
	print(paste("RMSE", RMSE(model$ArrDelay, model$departure_delay_average)))

	#
	# average of airport effect and airline effect
	#
	print("Average airport effect and airline effect")
	model <- test %>% inner_join(origin_airport_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(airline_averages, by = "Flight_Number_Reporting_Airline") %>%
		mutate( prediction = (origin_airport_average + airline_average) / 2)
	print(paste("RMSE", RMSE(model$ArrDelay, model$prediction)))

	#
	# average of airport effect and airline effect and flight number
	#
	print("Average airport effect, airline effect and flight number effect")
	model <- test %>% inner_join(origin_airport_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(airline_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(flight_number_averages, by = "Flight_Number_Reporting_Airline") %>%
		mutate( prediction = (origin_airport_average + airline_average + flight_number_average) / 3)
	print(paste("RMSE", RMSE(model$ArrDelay, model$prediction)))

	#
	# average of airport effect and airline effect flight number, tail number
	#
	print("Average airport effect, airline effect, flight number effect, tail number effect")
	model <- test %>% inner_join(origin_airport_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(airline_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(flight_number_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(tail_number_averages, by = "Flight_Number_Reporting_Airline") %>%
		mutate( prediction = (origin_airport_average + airline_average + flight_number_average + flight_number_average) / 4)
	print(paste("RMSE", RMSE(model$ArrDelay, model$prediction)))

	#
	# average of airport effect and airline effect flight number, tail number, arrival time
	#
	print("Average airport effect, airline effect, flight number effect, tail number effect, arrival time")
	model <- test %>% inner_join(origin_airport_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(airline_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(flight_number_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(tail_number_averages, by = "Flight_Number_Reporting_Airline") %>%
		inner_join(arrival_time_averages, by = "Flight_Number_Reporting_Airline") %>%
		mutate( prediction = (origin_airport_average + airline_average + flight_number_average + flight_number_average + arrival_average) / 5)
	print(paste("RMSE", RMSE(model$ArrDelay, model$prediction)))


}

if ( build_late_models_iteration_1 ) {
	load(train_rda)
	load(test_rda)
	models <- c("rpart", "glm", "mlp")		# rf and knn never returned
	fits <- lapply(models, function(model) {
		print(model)
		print(Sys.time())
		fit <- train(
			late ~ airline + origin_airport + ArrTime + DepDelay,        
			method=model, data=train,
			tuneLength = 2)
		print(Sys.time())
		print(fit)
		save(fit, file=paste(model, "_1.rda", sep=""))
		predictions <- predict(fit, test)
		print(paste("length predictions", length(predictions)))
		print(paste("length test$late", length(test$late)))
		cm <- confusionMatrix(data = predictions, reference = test$late)
		save(cm, file=paste(model, "_1_cm.rda", sep=""))
		print(cm)
	})
	rm(test, train)
}

if ( build_late_models_iteration_2 ) {
	load(train_rda)
	load(test_rda)
	#
	# models and tuning_parameters must be in same order.
	# NAs for models with no tuning parameters
	#
	models <- c("rpart", "glm", "mlp")
	tuning_parameters <- c(
			data.frame(cp = seq(.0005, .00100, 0.0001)), 	# rpart
			NA,		 				# glm - no tuning parameters
			data.frame(size = seq(3, 23, 10)))    		# mlp
	for(i in 1:length(models) ) {
		model <- models[i]
		print(model)
		print(Sys.time())
		fit <- NA			# scoping
		if ( is.na(tuning_parameters[i]  ) ) {
			print(paste("model=", model, " no tuning parameters", sep=" "))
			fit <- train(
				late ~ airline + origin_airport + ArrTime + DepDelay,        
				method=model, data=train)
		} else {
			print(paste("model=", model, "tuning parameters", tuning_parameters[i], sep = " "))
			fit <- train(
				late ~ airline + origin_airport + ArrTime + DepDelay,        
				method=model, data=train,
				tuneGrid = expand.grid(tuning_parameters[i]))
		}
		print(Sys.time())
		print(fit)
		save(fit, file=paste(model, "_2.rda", sep=""))
		predictions <- predict(fit, test)
		print(paste("length predictions", length(predictions)))
		print(paste("length test$late", length(test$late)))
		cm <- confusionMatrix(data = predictions, reference = test$late)
		save(cm, file=paste(model, "_2_cm.rda", sep=""))
		print(cm)
	}
	rm(test, train)
}

if ( build_late_models_iteration_3 ) {
	load(train_rda)
	load(test_rda)
	#
	# models and tuning_parameters must be in same order.
	# NAs for models with no tuning parameters
	#
	models <- c("rpart", "glm")
	tuning_parameters <- c(
			data.frame(cp = seq(.0001, .00200, 0.0001)), 	# rpart
			NA)		 				# glm - no tuning parameters
	for(i in 1:length(models) ) {
		model <- models[i]
		print(model)
		print(Sys.time())
		fit <- NA			# scoping
		if ( is.na(tuning_parameters[i]  ) ) {
			print(paste("model=", model, " no tuning parameters", sep=" "))
			fit <- train(
				late ~ airline + origin_airport + ArrTime + DepDelay,        
				method=model, data=train)
		} else {
			print(paste("model=", model, "tuning parameters", tuning_parameters[i], sep = " "))
			fit <- train(
				late ~ airline + origin_airport + ArrTime + DepDelay,        
				method=model, data=train,
				tuneGrid = expand.grid(tuning_parameters[i]))
		}
		print(Sys.time())
		print(fit)
		save(fit, file=paste(model, "_3.rda", sep=""))
		predictions <- predict(fit, test)
		print(paste("length predictions", length(predictions)))
		print(paste("length test$late", length(test$late)))
		cm <- confusionMatrix(data = predictions, reference = test$late)
		save(cm, file=paste(model, "_3_cm.rda", sep=""))
		print(cm)
	}
	rm(test, train)
}

if ( build_late_models_iteration_4 ) {
	#
	# now the entire non_validation data set (which comprises train and test)
	# will be used to train the rpart model and the validation set will be
	# used to validate it
	#
	load(non_validation_rda)
	load(validation_rda)
	print(Sys.time())
	fit <- train(
		late ~ airline + origin_airport + ArrTime + DepDelay,        
		method="rpart", data=non_validation,
		tuneGrid = data.frame(cp = 5e-04))
	print(Sys.time())
	print(fit)
	save(fit, file="rpart.rda")
	predictions <- predict(fit, validation)
	print(paste("length predictions", length(predictions)))
	print(paste("length validation$late", length(validation$late)))
	cm <- confusionMatrix(data = predictions, reference = validation$late)
	save(cm, file=paste("rpart", "_4_cm.rda", sep=""))
	print(cm)
}
