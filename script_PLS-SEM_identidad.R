install.packages("seminr")
library(seminr)

library(foreign)
dt<-read.spss("C:/Users/ruizv/Dropbox/IDENTIDAD_sub.sav",to.data.frame = T)

colnames(dt)<-c("IDVI_1","IDVI_2","IDVI_3",
                "COMU_1","COMU_2","COMU_3","COMP_1","COMP_2","COMP_3","COMP_4",
                "CULT_1","CULT_2","CULT_3", "ATR_1", "ATR_2","ID_1","ID_2","ID_3","ID_4",
                "ACT_1","ACT_2","ACT_3","ACT_4","EXTRA_1","EXTRA_2","EXTRA_3")


dt2<-read.csv("C:/Users/ruizv/Downloads/IDENTIDAD_sub.csv",header = T)

library(readxl)
dt3<-read_excel("C:/Users/ruizv/Downloads/IDENTIDAD_sub.xlsx",na="NA")


# escribir tabla
write.csv(dt,"C:/Users/ruizv/Downloads/IDENTIDAD_sub.csv",sep=",",row.names = F,col.names = T)



modelo_medicion<-constructs(
  composite("IDVI", multi_items("IDVI_", 1:3)),
  composite("COMU", multi_items("COMU_", 1:3)),
  composite("COMP", multi_items("COMP_",1:4)),
  composite("CULT", multi_items("CULT_", 1:3)),
  composite("ATR", multi_items("ATR_", 1:2)),
  composite("ID", multi_items("ID_", 1:4)),
  composite("ACT", multi_items("ACT_",1:4)),
  composite("EXTRA", multi_items("EXTRA_", 1:3)))


# Create structural model
modelo_estructural <- relationships(
  paths(from = c("IDVI", "COMU","COMP","CULT"), to = c("ATR")),
  paths(from = c("ATR"), to = c("ID","ACT")),
  paths(from = c("ID"), to = c("ACT")),
  paths(from = c("ID","ACT"), to = c("EXTRA"))
  )

# Estimate the model
estimacion <- estimate_pls(data = dt,
                                      measurement_model = modelo_medicion,
                                      structural_model = modelo_estructural,
                                      inner_weights = path_weighting,
                                      missing = mean_replacement,
                                      missing_value = NA)


# Summarize the model results
summary_estimacion <- summary(estimacion)
# Inspect the model’s path coefficients and the R^2 values
summary_estimacion$paths
# Inspect the construct reliability metrics
summary_estimacion$reliability

# Bootstrap the model
boot_estimacion <- bootstrap_model(seminr_model = estimacion,
                                        nboot = 1000,
                                        cores = NULL,
                                        seed = 123)
# Store the summary of the bootstrapped model
summary_boot_estimacion <- summary(boot_estimacion)
# Inspect the bootstrapped structural paths
summary_boot_estimacion$bootstrapped_paths
# Inspect the bootstrapped indicator loadings
summary_boot_estimacion$bootstrapped_loadings

# Write the bootstrapped paths object to csv file
write.csv(x = summary_boot_estimacion$bootstrapped_loadings,
          file = "C:/Users/ruizv/Downloads/boot_loadings_estimacion.csv")

plot(summary_estimacion$reliability)

