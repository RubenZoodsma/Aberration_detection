library(ggthemes)

#---- Import patient frame ----
# Import frame with following parameters: pt-ID, timestamp, heart-rate, invasive BP (dia / mean / sys) / resp. rate / saturation / nirs L+R / End-tidaL CO2
# Database selected: [nirs_ruben_brm]
pt_frame <- import_pt_frame(pt_id = **** )

# pre-processing: Numerize, set column-classes, mean the NIRS, lose unwanted columns, dichotomize End-tidal CO2
pt_frame <- pre_proces_frame(pt_frame)

# Set timeframe if necessary - Format: "%Y-%m-%d %T"
pt_frame <- pt_frame %>% drop_na(mon_hr, mon_sat, mon_ibp_mean, mon_rr, nirs_mean)
pt_frame <- timeframe(frame =  pt_frame, t_start = "2001-01-01 00:00:00", t_end = "2001-01-01 00:00:00")
pt_frame <- timeframe(frame =  pt_frame, 
                      t_start = as.POSIXct(as.numeric(min(pt_frame$pat_datetime)) + ((1)* 3600), origin = ("1970-01-01")),
                      t_end = as.POSIXct(as.numeric(min(pt_frame$pat_datetime)) + ((26) * 3600), origin = ("1970-01-01")) )

#---- Predict SVM-class & label unstable instances ----
# predict accordingly One-Class SVM (mu=0.05)
pt_frame <- predict_high_group(svm_high_group, pt_frame)

#---- Detect abnormal baseline variation ----
# Calculate mahalanobis distance using corresponding correlation-matrix
pt_frame <- norm_maha_dir(pt_frame)
# calculate 5-m and global averages
pt_frame <- moving_averages_single_pt(pt_frame)
backup <- pt_frame

#---- Labeling + 300s-moving window for false-positive prevention ----
# Label each instance
pt_frame <- label_high(pt_frame)
#  300s-moving window for stable / unstable according to 80%-rule
pt_frame <- label_replace(pt_frame)

#---- Detect sensor-dysfunction ----
# NIRS = 15 / 95 - sensory dysfunction
# IBP_mean above 110 or below 25
pt_frame <- sensor_dysfunction(pt_frame)
#---- Plot the vitals and their labels ----
# Plot the frame
ggplot(pt_frame,
       aes(x=pat_datetime, omit.na=TRUE) )  +
  geom_vline(xintercept=as.POSIXct("2018-02-13 20:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct("2018-02-13 21:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct("2018-02-13 22:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct("2018-02-13 23:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct("2018-02-14 00:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_vline(xintercept=as.POSIXct("2018-02-13 20:00:00"), col="gray", lwd=0.4, alpha=0.5) +
  geom_line(aes(y=mon_hr, col="Heart Rate"), lwd=1) + labs (x="Time in hours", y= "", col="Parameters") +
  geom_line(aes(y=mon_sat, col="Venous sat"),lwd=0.6) +
  geom_line(aes(y=mon_ibp_mean, col="Blood pressure"), lwd=0.6) +
  geom_line(aes(y=nirs_mean, col="NIRS"),lwd=0.6) +
  geom_line(aes(y=mon_rr, col="Resp. Rate"), lwd=1) +
  #geom_line(aes(y=(glob_5_m - glob_12)*4, colour="det coef"), lwd=0.3) +
  geom_line(aes(y=0, x=pat_datetime), col=pt_frame$label, size=3) +
  coord_cartesian(xlim=c(as.POSIXct(as.numeric(min(pt_frame$pat_datetime)) +((0)* 3600), origin = ("1970-01-01") ),
                         as.POSIXct(as.numeric(min(pt_frame$pat_datetime)) + ((4)* 3600), origin = ("1970-01-01") )), 
                  ylim=c(0,200)) +
  theme_stata()  


