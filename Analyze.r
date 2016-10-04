#fairly optimized and automated --  for Viraj_project_26/04/2015 : 16:20

#lets get this done:
#-------------------------------------------------------------------
#Steps would be as follows:
#1> Load data
#2> Feature Extraction
#3> Trip wise sheet/ Driver wise/ db upload
#	3.1> Trip wise performance matrix
#	3.2> Averaging the features
#	3.3> Driver wise sheet creation for select number of drivers 
#4> Classification based on some weights



#-------------------------------------------------------------------
#SINGLE DRIVER SINGLE TRIP EXECUTION TEST
#-------------------------------------------------------------------
#numofdrivers = 1
#numoftrips = 1
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#SINGLE DRIVERS ALL TRIPS EXECUTION TEST
#-------------------------------------------------------------------
#numofdrivers = 1
#numoftrips = 200
#-------------------------------------------------------------------


#-------------------------------------------------------------------
#BATCH EXECUTION TEST ( TIME CONSUMING)
#-------------------------------------------------------------------
#numofdrivers = 700(OUT OF 2736)
#numoftrips = 200
#-------------------------------------------------------------------


#-------------------------------------------------------------------
# Enter this information before execution of this script:

numofdrivers = 700
numoftrips = 200

#-------------------------------------------------------------------

#creating the perfomance sheet for documenting the results:
perf_matrix = matrix(nrow=numoftrips,ncol=7);
colnames(perf_matrix) <- c("duration (secs)","mean_speed(meters/sec)","max_speed(meters/sec)","std_speed(meters/sec)","std_acceleration","std_breaking","total_distance(meters)")


#creating the safety check sheet for documenting results:
safety_matrix = matrix(nrow=numofdrivers,ncol=8);
colnames(safety_matrix) <- c("driver_ID","duration (hrs)","mean_speed (miles/hr)","max_speed (miles/hr)","std_speed (miles/hr)","std_acceleration","std_breaking","total_distance (miles)")


#-------------------------------------------------------------------



#main loop:


for (k in 1:numofdrivers){

	#Note: the 200 iteration count down here is the number of trips:
	    for (j in 1:numoftrips ) {

	    #Step 1> Load Data
		data_file_path = paste("C:/Users/Viraj/Documents/mansi_project/drivers/Driver",k,"/",as.character(j),".csv", sep = "");
		sample <- read.csv(data_file_path);

		#-------------------------------------------------------------------
		#Step 2> Feature extraction functions:
		#Feature 1: Duration

		duration = nrow(sample)


		#Feature 2: Speed
		sizeof_set = nrow(sample);
		speed_matrix = matrix(c(0),nrow=sizeof_set-1);


		#Euclidean distance calculation:

		for (i in 1:sizeof_set-1 ) {
			speed_matrix[i]= sqrt((sample[i,1]-sample[i+1,1])^2+(sample[i,2]-sample[i+1,2])^2)
		}


		#Feature 3: Acceleration

		#Smooth speed data by convolution i.e moving average:
		smooth_speed= matrix(c(0),nrow=nrow(speed_matrix));

		window = 10

		i = 0
		for(i in (window):nrow(speed_matrix)){
			sum = 0
			for(n in 1:window){
				sum = sum + speed_matrix[(i+n)-(window)]
			}
			smooth_speed[i] = sum/(window)
		}
		#print(smooth_speed)

		#Head changes not accomodated in this version of code

		smooth_speed = smooth_speed[window:(nrow(smooth_speed)),]
		#smooth_speed = matrix(smooth_speed)

		mean_speed = mean(smooth_speed)
		#print(paste("mean_speed",mean_speed))

		max_speed = max(smooth_speed)
		#print(paste("max_speed",max_speed))

		std_speed = sd(speed_matrix[])
		#print(paste("std_speed",std_speed))

		smooth_accel = matrix(diff(smooth_speed))

		accel_s = smooth_accel   
		neg_accel = accel_s[accel_s<0]
		pos_accel = accel_s[accel_s>0]

		mean_breaking  = mean(neg_accel)
		mean_acceleration = mean(pos_accel)

		std_breaking = sd(neg_accel)
		std_acceleration = sd(pos_accel)
		#print(paste("std_breaking", std_breaking))
		#print(paste("std_acceleration", std_acceleration))
		    
		#Feature 4: Distance Travelled

		total_distance = sum(smooth_speed)
		#print(paste("total_dist",total_dist))

		#Feature 5: Acceleration after stops
		#Feature 6: Acceleration on turns

		#-------------------------------------------------------------------	
		#Step 3.1> trip wise data generation
		#Driver performance matrix trip wise
		perf_matrix[j,1] = duration
		perf_matrix[j,2] = mean_speed
		perf_matrix[j,3] = max_speed
		perf_matrix[j,4] = std_speed
		perf_matrix[j,5] = std_acceleration
		perf_matrix[j,6] = std_breaking
		perf_matrix[j,7] = total_distance

		#Keeping a track on the iteration count for trips/driver:
		#print(paste("Driver_ID",k,"Trip_ID",j))

		#Writing driver analysis sheet:
		write.csv(perf_matrix, paste("C:/Users/Viraj/Documents/mansi_project/inR/super_safe_heaven/perf_matrix_driver",k,".csv", sep = ""), row.names = FALSE ) 

	}	

		#Step 3.2> Averaging the features of all the trips per driver:
		
		#Total_avg duration of trip of a driver in hrs:
		total_avg_duration = (sum(perf_matrix[,1])/(nrow(perf_matrix)))/3600
	
		#Total avg of mean_speed of a driver in miles/hr:
		total_avg_mean_speed= (sum(perf_matrix[,2])/(nrow(perf_matrix)))*2.23694
	

		#Total avg of max speed of a driver in miles/hr:
		total_avg_max_speed = (sum(perf_matrix[,3])/(nrow(perf_matrix)))*2.23694
	
		#Total avg of std_speed of a driver in miles/hr:
		total_avg_std_speed = (sum(perf_matrix[,4])/(nrow(perf_matrix)))*2.23694
		
		#Total avg of std_acceleration of a driver:
		total_avg_std_acceleration = sum(perf_matrix[,5])/(nrow(perf_matrix))
		
		#Total avg of std_breaking of a driver:
		total_avg_std_breaking = sum(perf_matrix[,6])/(nrow(perf_matrix))
		
		#Total avg of trip distance (in miles) driven by a driver:
		total_avg_total_distance = (sum(perf_matrix[,7])/(nrow(perf_matrix)))*0.000621371
		


		#Step 3.3> Driver wise sheet creation for all drivers:

		#Building the safety_matrix  
		#putting Driver ID:
		safety_matrix[k,1] = k;
		#putting total_avg_duration:
		safety_matrix[k,2] = total_avg_duration
		#putting total_avg_mean_speed
		safety_matrix[k,3] = total_avg_mean_speed
		#putting total_avg_max_speed
		safety_matrix[k,4] = total_avg_max_speed
		#putting total_avg_std_speed
		safety_matrix[k,5] = total_avg_std_speed
		#putting total_avg_std_acceleration
		safety_matrix[k,6] = total_avg_std_acceleration 
		#putting total_avg_std_breaking
		safety_matrix[k,7] = total_avg_std_breaking
		#putting total_avg_total_distance
		safety_matrix[k,8] = total_avg_total_distance

		#Writing safety analysis sheet:
		write.csv(safety_matrix, "C:/Users/Viraj/Documents/mansi_project/inR/super_safe_heaven/safety_analysis_sheet.csv", row.names = FALSE ) 

		


}

