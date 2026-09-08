\### Data frames description ###



\# In all data frames, *mean\_t* is the mean temperature (°C), *mean\_w* is the mean wind speed (m/s) and *sum\_p* is the overall rainfall (mm) for that hour or day. *Period* represents the hour before or after the drone trial. *Phase* shows whether a drone trial was carried out that day.



\# df\_flock\_d.csv

1-hour mean flock density throughout the day. *Timestamp* is the 1-hour bin, *area\_den* is the mean flock density over that hour (1/m2). *area\_mean* is the area occupied by the flock (m2) and *gsize* is the group size.



\# df\_flock\_h.csv

hourly mean flock density for the periods before and after a drone trial (*period*). *area\_den* is the mean flock density over that hour (1/m2), *area\_mean* is the area occupied by the flock (m2) and *gsize* is the group size.



\# df\_iid\_d.csv

daily mean pairwise inter-individual distances (*mean\_iid*, m) per individual (*TagID*).



\# df\_iid\_h.csv

hourly mean pairwise inter-individual distances (*mean\_iid*, m) per individual (*TagID*).



\# df\_spaceuse\_d.csv

daily normalised space use (*home\_adj*, m2) at the 50% and 95% Utilization Distributions (*percentage*) per individual (*TagID*).



\# df\_tra\_d.csv

daily normalised distance travelled (*tra\_adj*, m) per individual (*TagID*).



\# df\_tra\_h.csv

hourly normalised distance travelled (*travel\_adj*, m) per individual (*TagID*).



\# faecal.csv

faecal cortisol concentrations (*cortisol*, ng/g dry faeces) per individual (*TagID*). Each row represents a faecal sample (*sample*), collected during one of three experimental contexts (*context*).



\# wool.csv

wool cortisol concentrations (*cortisol*, pg/mg of wool) per individual (*TagID*). Each row represents a wool sample (*sample*), collected either at the beginning or at the end of the study (*context*).

