# Read packages  ------------------------------------------------------------------------------
library(Hmisc)
library(xtable)
library(lsr)
library(RSQLite)
library(dplyr)
library(rjson)
library(tidyr)
library(ggplot2)
library(stringr)

# EXP1: Read in data (no need to run) -------------------------------------------------------------------------------
rm(list = ls())

# con = dbConnect(SQLite(),dbname = "../../javascript/Experiment_1/participants.db");
con = dbConnect(SQLite(),dbname = "../../javascript/participants.db");
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

#add game information
list.info = fromJSON(file = "../../javascript/experiment_1/static/json/games.json")
list.games = list()
df.games = data.frame()

for (k in 1:(list.info[['scenarios']] %>% length())){
  tmp = list.info[['scenarios']][[k]][['games']]
  df.tmp = matrix(NA,ncol=4,nrow = length(tmp)) %>% 
    as.data.frame() %>% 
    setNames(c('id','team1','team2','winner')) %>% 
    mutate_each(funs(. %>% as.character()))
  
  for (i in 1:nrow(df.tmp)){
    df.tmp[i,] = c(id = k,
                   team1 = tmp[[i]]$team1 %>% str_c(collapse=","),
                   team2 = tmp[[i]]$team2 %>% str_c(collapse=","),
                   winner = tmp[[i]]$winner %>% as.character())
  }
  list.games[[k]] = df.tmp
  df.games = rbind(df.games,df.tmp)
}

# df.tmp = df.games %>% 
#   select(id,team1,winner,team2) %>% 
#   mutate(winner = ifelse(winner == 1, ">", "<"))
# 
# xtable(df.tmp) %>% print(include.rownames=F)

labels = c(rep(c('confounded evidence',
                 'strong indirect evidence',
                 'weak indirect evidence',
                 'diverse evidence',
                 'confounded with partner',
                 'confounded with opponent',
                 'strong indirect evidence',
                 'weak indirect evidence',
                 'diverse evidence',
                 'round robin'
),2),
rep(c('2 v 1',
      '1 v 2',
      '2 v 3',
      '2 v 3 repeated',
      'no difference'),2))

df.info = df.games %>% 
  mutate(players = ifelse((str_count(team1,",") == 0 & str_count(team2,",") == 0),'single','multiple'),
         teamsize = ifelse(str_count(team1,",") == str_count(team2,","),'same','different')) %>% 
  group_by(id) %>% 
  arrange(teamsize) %>% 
  filter(row_number()==1) %>% 
  ungroup %>%
  mutate(id = as.numeric(id),
         labels = labels) %>% 
  arrange(id)

# save data 

df.wide = df.wide %>% 
  select(-workerid)

save(df.wide,df.long,df.info,df.games,file='../../../data/exp1_data.RData')

# EXP1: Load data -------------------------------------------------------------------------------
rm(list = ls())
load(file='../../../data/exp1_data.RData')

# EXP1: Plot (just data) ----------------------------------------------------------------------------------

df.plot = df.long %>% 
  mutate(rating = rating-50,
         id = id +1) %>% 
  left_join(df.info) %>% 
  mutate(id = as.factor(id),
         winner = factor(winner,levels = c("1","2"),labels = c('Win', "Loss")),
         rating = ifelse(winner == "Loss", rating*-1, rating))

ggplot(df.plot,aes(x = id, y = rating))+
  stat_summary(fun.y = 'mean',geom='bar',color = 'black',fill = 'gray80')+
  stat_summary(fun.data = 'mean_cl_boot',geom='linerange',size = 1)+
  facet_wrap(~winner,scales = "free_x",ncol=1)+
  labs(y = 'weakness/strength judgment', x = 'tournament number')+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank())

ggsave('../../../figures/plots/exp1_bars.pdf',width=10,height=6)


# EXP1: Model predictions and regression ------------------------------------------------------

# create means and add "predictions"
df.regression = df.long %>% 
  mutate(id = id+1) %>% 
  group_by(id) %>% 
  summarise(mean = mean(rating),
            ci.low = smean.cl.boot(rating)[2], #bootstrapped confidence intervals 
            ci.high = smean.cl.boot(rating)[3]
  ) %>% 
  left_join(df.info) %>% 
  mutate(predictions = rnorm(nrow(.))) #just randomly drawn numbers 

df.regression$predictions = lm(mean~predictions,data=df.regression)$fitted.values

# EXP1: Plot (bars with model predictions) -----------------------------------------------------
df.plot = df.regression %>% 
  gather(index,value,c(mean,predictions)) %>% 
  mutate_each(funs(ifelse(index != "mean", NA, .)),contains("ci")) %>% 
  mutate(id = as.factor(id),
         winner = factor(winner,levels = c("1","2"),labels = c('Win', "Loss")))

ggplot(df.plot,aes(x = id, y = value, group = index, fill = index))+
  geom_bar(stat= "identity", color="black", position = position_dodge(0.9), width = 0.9)+
  geom_linerange(aes(min = ci.low, max = ci.high), size = 1, position = position_dodge(0.9))+
  facet_wrap(~winner,scales = "free_x",ncol=1)+
  scale_fill_grey(start = 1, end = 0.6)+
  labs(y = 'weakness/strength judgment', x = 'tournament number', fill = "")+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")


# EXP1: Plot (scatterplot)  -------------------------------------------------------------------

df.plot = df.regression 

ggplot(df.plot,aes(x = predictions, y = mean))+
  geom_point()+
  geom_errorbar(aes(min = ci.low,max = ci.high),width=0)+
  geom_text(aes(label=id),size=5,hjust=1.2,vjust=0)+
  theme_bw()+
  labs(x = 'model', y = 'data')+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")






