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
# rm(list = ls())
load(file='../../../data/exp1_data.RData')
df.long = df.long %>% 
  mutate(id = id+1)

# EXP1: Load model predictions -------------------------------------------------------------------------------
load(file='../models/exp1_predictions.RData')
df.long = df.long %>% 
  left_join(df.results %>% select(-winner) %>% rename(prediction = rating))
# EXP1: Plot (just data) ----------------------------------------------------------------------------------

df.plot = df.long %>% 
  mutate(rating = rating-50) %>% 
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

# ggsave('../../../figures/plots/exp1_bars.pdf',width=10,height=6)

# EXP1: Model predictions and regression ------------------------------------------------------

# create means and add "predictions"
df.regression = df.long %>% 
  left_join(df.info) %>% 
  group_by(id) %>% 
  mutate(rating = rating -50,
         prediction = prediction - 50) %>% 
  summarise(mean = mean(rating),
            ci.low = smean.cl.boot(rating)[2], #bootstrapped confidence intervals 
            ci.high = smean.cl.boot(rating)[3],
            predictions = mean(prediction)
  ) %>% 
  left_join(df.info)

df.regression$predictions = lm(mean~predictions,data=df.regression)$fitted.values

# EXP1: Plot (bars with model predictions) -----------------------------------------------------
df.plot = df.regression %>% 
  gather(index,value,c(mean,predictions)) %>% 
  mutate_each(funs(ifelse(index != "mean", NA, .)),contains("ci")) %>% 
  mutate(id = as.factor(id),
         winner = factor(winner,levels = c("1","2"),labels = c('Win', "Loss")),
         value = ifelse(winner == "Loss", value*-1, value),
         ci.low = ifelse(winner == "Loss", ci.low*-1, ci.low),
         ci.high = ifelse(winner == "Loss", ci.high*-1, ci.high)
         )

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
# ggsave('../../../figures/plots/exp1_data_model_bars.pdf',width=10,height=6)

# EXP1: Plot (scatterplot)  -------------------------------------------------------------------

df.plot = df.regression

ggplot(df.plot,aes(x = predictions, y = mean))+
  
  geom_hline(yintercept = 0, linetype = 2, size = 1, alpha = 0.5)+
  geom_smooth(method = lm,color = "black")+
  # geom_errorbar(aes(min = ci.low,max = ci.high),width=0, alpha = 0.5, size = 1.5)+
  geom_errorbar(aes(min = ci.low,max = ci.high),width=0, alpha = 0.5, size = 1)+
  geom_point(size = 3)+
  # geom_text(aes(label=id),size=5,hjust=1.2,vjust=0)+
  labs(x = 'model predictions', y = 'mean judgments')+
  annotate(geom = "text", x= -50, y = Inf, label = paste0("r = ", cor(df.plot$mean,df.plot$predictions) %>% round(2)), size = 8, hjust=0, vjust = 1.5)+
  annotate(geom = "text", x= -50, y = Inf, label = paste0("RMSE = ", rmse(df.plot$mean,df.plot$predictions) %>% round(2)), size = 8, hjust=0, vjust = 3)+
  scale_x_continuous(limits = c(-50,50),breaks = seq(-50,50,25),labels = seq(-50,50,25))+
  scale_y_continuous(limits = c(-50,50),breaks = seq(-50,50,25),labels = seq(-50,50,25))+
  theme_bw()+
  theme(text = element_text(size = 20),
        panel.grid = element_blank(),
        legend.position = "bottom")
ggsave('../../../figures/plots/exp1_scatter.pdf',width=8,height=6)

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

xtable(df.lazy %>% select(id) %>% mutate(id = str_replace_all(id,"\n","; ")))

# EXP2: Plot results  -------------------------------------------------------------------------

df.plot = df.long %>% 
  mutate(trial = factor(trial))

ggplot(df.plot,aes(x = reorder(trial,response), y = response))+
  stat_summary(fun.y = 'mean', geom = 'point', size = 3)+
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', width=0)+
  geom_point(position = position_jitter(height = 0, width = 0.2), alpha = 0.3)+
  # geom_point(data = df.regression, aes(x = reorder(trial,mean), y = prediction), color = 'red',size=2)+
  geom_point(data = df.regression, aes(x = reorder(trial,mean), y = model), color = 'red',size=2)+
  labs(x = "trial", y = "rating")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

# ggsave('../../../figures/plots/exp2_means.pdf',width=12,height=6)
  

# EXP2: Scatter  ------------------------------------------------------------------------------

df.plot = df.regression

ggplot(df.plot,aes(x = prediction, y = mean))+
# ggplot(df.plot,aes(x = model, y = mean))+
  geom_smooth(method = 'lm', color = 'black')+
  geom_point(size=2)+
  geom_text_repel(aes(label = trial))+
  labs(x = "model", y = "data")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        text = element_text(size = 20))

# ggsave('../../../figures/plots/exp2_scatter.pdf',width=8,height=6)

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
