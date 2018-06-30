library(ggplot2)
library(dplyr)
library(readr)
library(reshape2)

# Frequencies of Words in Articles

freq <- read_csv("freq.csv")
freq = cbind(as.data.frame(seq(1, 100)), freq)
names(freq) = c('seq', 'word', 'num')

ggplot(data = freq, aes(x=word, y=num)) +
  geom_bar(stat='identity', colour = '#0066CC', fill = '#0066CC',
           width = 0.5) +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0, hjust = 1, 
                                   size = 10, face = 'bold'),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()) +
  scale_x_discrete(limits=freq$word) +
  labs(list(title = '최빈 단어 100개',
            x = '단어', 
            y = '사용된 횟수'))

ggsave('freq.png', width = 30, height=24, dpi = 300, units="cm")

# Number of Articles by Year
numbers = data.frame(matrix(ncol=2))[-1,]
for(i in 2002:2018){
  numbers = rbind(numbers,
                  cbind(nrow(filter(data, grepl(as.character(i), date))), i))
}
crimenumbers = c(967+998+5581, 
                 7596+857+1065, #14231
                 8558+749+910,
                 967+832+9841,
                 9383+869+1008,
                 1093+1011+10508,
                 11380+1138+914,
                 1124+1295+12895,
                 13110+1428+1020,
                 1063+1770+13352,
                 1437+2651+19566,
                 20101+2262+1132,
                 1356+1632+21834,
                 2227+1573+28883,
                 3055+1534+26835,
                 5004+1249+31828,
                 14398+617+2313)
numbers = cbind(numbers, crimenumbers)
names(numbers) = c('num', 'time', 'cnum')

ggplot(data = numbers, 
       mapping = aes(x = time, y = num, group = 1)) +
  geom_line(lwd = 1, color = 'blue') +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(list(title = '연도별 성범죄 관련 기사 숫자',
            x = '시기(연도)', 
            y = '기사 숫자'))
ggsave('sexualharassment.png', width = 30, height=24, dpi = 300, units="cm")

ggplot(data = numbers, 
       mapping = aes(x = time, y = cnum, group = 1)) +
  geom_line(lwd = 1, color = 'red') +
  theme(panel.grid.minor = element_blank(), 
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.ticks.length = unit(.25, "cm"),
        axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(list(title = '연도별 일반 범죄 관련 기사 숫자',
            x = '시기(연도)', 
            y = '기사 숫자'))
ggsave('crime.png', width = 30, height=24, dpi = 300, units="cm")


topics <- read_csv("topicnadmeta.csv") 
topics$month = as.factor(floor(topics$month/3))
topics$press = topics$press %>% as.factor()

fil_data = as.data.frame(matrix(ncol=98))[-1,]
for(i in levels(topics$month)){
  for(j in levels(topics$press)){
    sub_set = filter(topics, month == i & press == j)
    sub_set[1:95] = sub_set[1:95]*sub_set$num
    metalevel_subset = data.frame(t(colSums(sub_set[1:95])))
    metalevel_subset$month = i
    metalevel_subset$press = j
    metalevel_subset$num = sum(sub_set$num)
    fil_data = rbind(fil_data, metalevel_subset)
  }
}

df = fil_data
df = filter(df, num != 0)
df$month = as.integer(df$month)
# 반응 및 대책의 방향
solution_punish = df$Topic1 + df$Topic5 + df$Topic29 + df$Topic65 + df$Topic69 + df$Topic76
solution_str = df$Topic2 + df$Topic35 + df$Topic49 + df$Topic55 + df$Topic70 + df$Topic88
# 범죄에 대한 묘사
sens_depiction = df$Topic28 + df$Topic36 + df$Topic61 + df$Topic75
empathy_nar = df$Topic51 + df$Topic73 + df$Topic80
objective_desc = df$Topic10 + df$Topic23 + df$Topic27 + df$Topic43 + 
  df$Topic45 + df$Topic60 + df$Topic64 + df$Topic67 + df$Topic87
# 가해자의 이미지
mental_disorder = df$Topic18 + df$Topic20
acquaintance_repeat = df$Topic30 + df$Topic84
intimate_partner = df$Topic14 + df$Topic41 
# 배경, 공간
space_world = df$Topic4 + df$Topic6 + df$Topic9 + df$Topic25 + df$Topic40 + df$Topic58 + 
  df$Topic62 + df$Topic74 + df$Topic82 + df$Topic91 + df$Topic92 + df$Topic93 + df$Topic8 
space_organization = df$Topic16 + df$Topic47 +
  df$Topic19+ df$Topic50 
space_politics = df$Topic12 + df$Topic71 + df$Topic37 + df$Topic38 + df$Topic46 + df$Topic54 + 
  df$Topic77 + df$Topic94
space_art = df$Topic13 + df$Topic17 + df$Topic53 + df$Topic68
space_sports_entertainment = df$Topic3 + df$Topic34 + df$Topic79
space_digital_emotion = df$Topic21 + df$Topic39 
space_school = df$Topic42 + df$Topic57 + df$Topic63
space_public = df$Topic48 + df$Topic78
space_professional = df$Topic59 + df$Topic86 + df$Topic31 + df$Topic24 + 
  df$Topic26 + df$Topic32
# merge reassigned topics and remove vector values
df = cbind(solution_punish, solution_str, sens_depiction, empathy_nar, objective_desc,
           mental_disorder, acquaintance_repeat, intimate_partner, space_art,
           space_organization, space_politics, space_professional, space_digital_emotion,
           space_public, space_school, space_sports_entertainment,
           space_world, df[96:98])
rm(solution_punish, solution_str, sens_depiction, empathy_nar, objective_desc,
   mental_disorder, acquaintance_repeat, intimate_partner, space_art,
   space_organization, space_politics, space_professional, 
   space_public, space_school, space_sports_entertainment,
   space_world)

graph = function(df, string, filename){
  sub_data = df %>% filter(grepl(string, press))
  sub_data = sub_data[1:18] %>%
    group_by(month) %>% 
    summarise_all(funs(sum))
  sub_data[2:18] = sub_data[2:18]/rowSums(sub_data[2:18])
  
  d = sub_data %>% select(c('solution_punish', 'solution_str',
                            'month'))
  names(d) = c('대책: 가해자 처벌', '대책: 피해자 보호 및 구조', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' solutions.png'), width = 30, height=24, dpi = 300, units="cm")
  sub_data
  
  d = sub_data %>% select(c('sens_depiction', 'empathy_nar', 'objective_desc',
                            'month'))
  names(d) = c('묘사: 자극적 표현 중', '묘사: 내러티브와 동정심', 
               '묘사: 사실관계 위주 서술', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' images.png'), width = 30, height=24, dpi = 300, units="cm")
  
  d = sub_data %>% select(c('mental_disorder', 'acquaintance_repeat', 'intimate_partner',
                            'month'))
  names(d) = c('정신병력과 음주', '계획적, 상습적 범죄', 
               '연인간 성범죄', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' stereotypes_criminal.png'), width = 30, height=24, dpi = 300, units="cm")
  
  
  d = sub_data %>% select(c('sens_depiction', 'empathy_nar', 'objective_desc',
                            'month'))
  names(d) = c('묘사: 자극적 표현 중', '묘사: 내러티브와 동정심', 
               '묘사: 사실관계 위주 서술', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' images.png'), width = 30, height=24, dpi = 300, units="cm")
  
  
  d = sub_data %>% select('space_art', 'space_politics', 'space_sports_entertainment',
                          'space_world', 'month')
  names(d) = c('배경_문화예술', '배경_정치', '배경_연예스포츠', '배경_국제', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' space_person.png'), width = 30, height=24, dpi = 300, units="cm")
  
  d = sub_data %>% select('space_organization', 'space_professional', 'space_digital_emotion',
                          'space_school', 'space_public', 'month')
  names(d) = c('배경_사회기업조직', '배경_고학력전문직', '배경_디지털과감정', '배경_학교', 
               '배경_공공장소', 'month')
  d = melt(d, id.vars='month')
  ggplot(data = d, 
         mapping = aes(x = month, y = value, fill = variable, color = variable)) +
    geom_line(lwd = 1.5) +
    scale_colour_discrete(name = '프레임') +
    theme(panel.grid.minor = element_blank(), 
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          axis.ticks.length = unit(.25, "cm"),
          axis.text.x = element_text(angle = 90, hjust = 1, size = 11, face='bold'),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank()) + 
    coord_cartesian(xlim = c(-0.0001, 67), 
                    ylim = c(-max(d$value)*0.08, max(d$value)*1.11), expand = FALSE) +
    labs(list(title = paste0('시간 변화에 따른 토픽 규모 변화 - ', filename),
              x = '시기(연도 및 월)', 
              y = '토픽이 차지하는 비율')) +
    scale_x_continuous(breaks = (c(0,50)), labels=c('', '')) +
    annotate(geom = 'text', 
             x = seq(1, 66, 1),
             y = -max(d$value)*0.03, size = 2,
             label = c(rep(c('1', '4', '7', '10'), 16), '1', '4')) +
    annotate(geom = 'text', 
             x = seq(2, 66, 4),
             y = -max(d$value)*0.06, size = 3,
             label = seq(2002, 2018, 1))
  ggsave(paste0('images/', filename, ' space_place.png'), width = 30, height=24, dpi = 300, units="cm")
}

graph(df, '', '전체')
graph(df, '경향신문|한겨레', '진보')
graph(df, '조선일보|동아일보', '보수')
graph(df, 'KBS|SBS|MBC', '방송사')
graph(df, '매일경제|한국경제', '경제신문')
for(i in levels(topics$press)){
  graph(df, i, i)
}

