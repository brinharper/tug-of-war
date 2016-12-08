# Read packages  ------------------------------------------------------------------------------
library(Hmisc)
library(lsr)
library(RSQLite)
library(dplyr)
library(rjson)
library(tidyr)
library(ggplot2)
library(stringr)

# Read in data  -------------------------------------------------------------------------------
rm(list = ls())

con = dbConnect(SQLite(),dbname = "../../javascript/Experiment_1/participants.db");
df.complete = dbReadTable(con,"tow")
dbDisconnect(con)

df.experiment = df.complete %>% 
  filter(codeversion == 'experiment_1') %>% 
  filter(status %in% c(3,4,5))

ntrials = 30
variables.unstructured = names(fromJSON(df.experiment$datastring[1])$questiondata)
variables.additional = c("experiment","participant","workerid","taskid","time")
variables.structured = c("id","rating")
nvariables = length(variables.structured)
variables.task = paste(variables.structured,rep(1:ntrials,each=nvariables),sep="_")

df.wide = data.frame(matrix(nrow=nrow(df.experiment),ncol=length(variables.unstructured)+
                              length(variables.additional)+length(variables.task)))
colnames(df.wide) = c(variables.additional,variables.task,variables.unstructured)

for (i in 1:nrow(df.wide)){
  a = fromJSON(df.experiment$datastring[i])
  # additional variables
  df.wide$experiment[i] = df.experiment$codeversion[i]
  df.wide$participant[i] = i
  df.wide$workerid[i] = a$workerId
  df.wide$taskid[i] = a$assignmentId
  df.wide$time[i] = as.numeric(as.POSIXct(df.experiment$endhit[i], format = "%Y-%m-%d %H:%M:%S")-
                                 as.POSIXct(df.experiment$beginhit[i], format = "%Y-%m-%d %H:%M:%S"))
  # unstructured variables 
  for (j in 1:length(variables.unstructured)){
    df.wide[[variables.unstructured[j]]][i] = a[["questiondata"]][[variables.unstructured[j]]]
  }
  
  # task variables 
  l = 1
  for (j in 1:ntrials){
    for (k in seq(2,by = 2, length.out = nvariables)){
      if (length(a$data[[j]]$trialdata[[k]])==0){
        b = "";
      }else{
        b = a$data[[j]]$trialdata[[k]]
      }
      df.wide[[variables.task[l]]][i] = str_c(as.character(b),collapse=",")
      l = l+1
    }
  }
}

df.long = df.wide %>%
  wideToLong(within='order') %>% 
  select(participant,order,id,rating) %>% 
  mutate(order = order %>% as.character() %>% as.numeric(),
         rating = as.numeric(rating),
         id = as.numeric(id)) %>% 
  arrange(participant,id)

attr(df.long,"reshapeLong") = NULL




