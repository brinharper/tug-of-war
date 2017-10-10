# To do list ----------------------------------------------------------------------------------
# explore how well different version of the tow model perform 
# - no laziness 
# - more laziness 
# - what happens when lazy ... 


# Read packages  ------------------------------------------------------------------------------
library(Hmisc)
library(xtable)
library(lsr)
library(RSQLite)
library(rjson)
library(stringr)
library(magrittr)
library(tidyjson)
library(ggrepel)
library(tidyverse)

# Misc functions  -----------------------------------------------------------------------------

rmse = function(x,y){
  return(sqrt(mean((x-y)^2)))
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------------------------
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
    mutate_all(funs(. %>% as.character()))
  
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
# rm(list = ls())
load(file='../../../data/exp1_data.RData')
df.long = df.long %>% 
  mutate(id = id+1)

# EXP1: Load model predictions -------------------------------------------------------------------------------
load(file='../models/exp1_predictions.RData')
df.long = df.long %>% 
  left_join(df.results %>% select(-winner) %>% rename(prediction = rating))
# EXP1: Model predictions and regression ------------------------------------------------------

# create means and add "predictions"
df.regression = df.long %>% 
  left_join(df.info) %>% 
  group_by(id) %>% 
  mutate(rating = rating -50,
         prediction = prediction - 50) %>% 
  summarise(response = mean(rating),
            ci.low = smean.cl.boot(rating)[2], #bootstrapped confidence intervals 
            ci.high = smean.cl.boot(rating)[3],
            prediction = mean(prediction)
  ) %>% 
  left_join(df.info)

df.regression$prediction = lm(response~prediction,data=df.regression)$fitted.values

# EXP1: Plot (points)  -------------------------------------------------------------------------

df.plot = df.long %>% 
  mutate(id = factor(id)) %>% 
  mutate(rating = rating - 50)

ggplot(df.plot,aes(x = reorder(id,rating), y = rating))+
  geom_hline(yintercept = 0,linetype = 2, color = "gray")+
  stat_summary(fun.y = 'mean', geom = 'point', size = 3)+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width=0)+
  geom_point(position = position_jitter(height = 0, width = 0.1), alpha = 0.1)+
  # geom_point(data = df.regression, aes(x = reorder(trial,mean), y = prediction), color = 'red',size=2)+
  geom_point(data = df.regression, aes(x = reorder(id,response), y = prediction), color = 'red',size=2)+
  labs(x = "trial", y = "rating")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

ggsave('../../../figures/plots/exp1_means.pdf',width=12,height=6)


# EXP1: Plot (scatter)  ------------------------------------------------------------------------------

df.plot = df.regression %>% 
  rename(trial = id) %>% 
  left_join(df.long %>% 
              mutate(response = rating-50) %>% 
              rename(trial = id) %>% 
              group_by(trial) %>% 
              summarise(low = smean.cl.boot(response)[2],
                        high = smean.cl.boot(response)[3]
              ))

ggplot(df.plot,aes(x = prediction, y = response))+
  # ggplot(df.plot,aes(x = model, y = mean))+
  geom_hline(yintercept = 0, linetype = 2, color = "gray")+
  geom_vline(xintercept = 0, linetype = 2, color = "gray")+
  geom_smooth(method = 'lm', color = 'black')+
  geom_errorbar(aes(ymin = low, ymax = high), alpha = 0.5)+
  geom_point(size=2)+
  geom_text_repel(aes(label = trial),size=6)+
  scale_x_continuous(breaks = seq(-50,50,10), labels = seq(-50,50,10))+
  scale_y_continuous(breaks = seq(-50,50,10), labels = seq(-50,50,10))+
  labs(x = "model", y = "data")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

ggsave('../../../figures/plots/exp1_scatter.pdf',width=8,height=6)

# cor(df.regression$model,df.regression$mean)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------------------------



# EXP2: Read in data  -------------------------------------------------------------------------

con = dbConnect(SQLite(),dbname = "../../javascript/experiment_2/participants.db");
# con = dbConnect(SQLite(),dbname = "../../data/experiment1.db");
df.data = dbReadTable(con,"tow")
dbDisconnect(con)

#filter out incompletes 
df.data = df.data %>% 
  filter(status %in% 3:5) %>%
  filter(!str_detect(uniqueid,'debug')) %>%
  filter(codeversion == 'experiment_2')

# demographic data 
df.demographics = df.data$datastring %>% 
  spread_values(condition = jnumber('condition'),
                age = jnumber('questiondata','age'),
                gender = jstring('questiondata','sex'),
                feedback = jstring('questiondata','feedback')
  ) %>% 
  rename(participant = document.id) %>% 
  mutate(time = difftime(df.data$endhit,df.data$beginhit,units = 'mins'))

# trial data 
df.long = df.data$datastring %>% 
  as.tbl_json() %>% 
  spread_values(workerid = jstring('workerId')) %>%
  enter_object('data') %>%
  gather_array('order') %>% 
  enter_object('trialdata') %>% 
  gather_keys('index') %>% 
  append_values_string('values') %>% 
  as.data.frame() %>% 
  spread(index,values) %>% 
  mutate_at(vars(trial,response),funs(as.numeric)) %>% 
  mutate(trial = trial + 1) %>% 
  rename(participant = document.id) %>% 
  select(participant,-workerid,order,trial,response) %>% 
  arrange(participant,trial)


# EXP2: Model predictions  --------------------------------------------------------------------

load('../models/tow_exp2_prediction.RData')

df.lazy = df.lazy %>% 
  # select(trial,rating) %>% 
  mutate(rating = rating*100) %>% 
  rename(model = rating)

df.regression = df.lazy %>% 
  select(trial,model) %>% 
  left_join(df.long %>% 
              group_by(trial) %>% 
              summarise(mean = mean(response))) %>% 
  mutate(prediction = lm(mean~model,data=.)$fitted.values)

tmp = df.lazy %>% 
  separate(id,c('y1','y2','y3','y4'),"\n",extra="merge") %>% 
  replace_na(list(y1 = "",
                  y2 = "",
                  y3 = "",
                  y4 = ""
                  )) %>% 
  select(trial,y2,y3,y4) %>% 
  gather(game,outcome,-trial) %>% 
  filter(outcome != "") %>% 
  select(trial,outcome) %>% 
  arrange(trial) %>% 
  xtable() %>% 
  print(include.rownames = F)
  

# EXP2: Plot (points)  -------------------------------------------------------------------------

df.plot = df.long %>% 
  mutate(trial = factor(trial))

ggplot(df.plot,aes(x = reorder(trial,response), y = response))+
  stat_summary(fun.y = 'mean', geom = 'point', size = 3)+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width=0)+
  geom_point(position = position_jitter(height = 0, width = 0.1), alpha = 0.1)+
  # geom_point(data = df.regression, aes(x = reorder(trial,mean), y = prediction), color = 'red',size=2)+
  geom_point(data = df.regression, aes(x = reorder(trial,mean), y = prediction), color = 'red',size=2)+
  labs(x = "trial", y = "rating")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

ggsave('../../../figures/plots/exp2_means.pdf',width=12,height=6)
  

# EXP2: Plot (scatter)  ------------------------------------------------------------------------------

df.plot = df.regression %>% 
  left_join(df.long %>% 
              group_by(trial) %>% 
              summarise(low = smean.cl.boot(response)[2],
                        high = smean.cl.boot(response)[3]
                        ))

ggplot(df.plot,aes(x = prediction, y = mean))+
  geom_hline(yintercept = 50, linetype = 2, color = "gray")+
  geom_vline(xintercept = 50, linetype = 2, color = "gray")+
  geom_smooth(method = 'lm', color = 'black')+
  geom_errorbar(aes(ymin = low, ymax = high), alpha = 0.5)+
  geom_point(size=2)+
  geom_text_repel(aes(label = trial),size=6)+
  scale_x_continuous(breaks = seq(0,100,10), labels = seq(0,100,10))+
  scale_y_continuous(breaks = seq(0,100,10), labels = seq(0,100,10))+
  labs(x = "model", y = "data")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

ggsave('../../../figures/plots/exp2_scatter.pdf',width=8,height=6)

cor(df.regression$model,df.regression$mean)

# EX: Gaussian with mean 50 and SD 10  --------------------------------------------------------

df.plot = data.frame(x = seq(25,75,0.1)) %>% 
  mutate(y = dnorm(x, mean = 50, sd = 10))

ggplot(df.plot,aes(x = x, y = y)) +
  # geom_hline(yintercept = 0, linetype = 2)+
  geom_line(size = 2)+
  labs(x = 'strength', y = 'p(strength)')+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")
# ggsave('../../../figures/plots/exp1_strength_prior.pdf',width=8,height=6)
