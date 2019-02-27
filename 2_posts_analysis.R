#Posts_analysis

# preparation
library(foreign)
library(stargazer)
setwd("xxx")

posts <- read.csv("xxx/posts-analysis.csv")
#posts <- subset(posts, sep_loy_attack == 0)
#View(posts)

#posts$fat_plus_inj = posts$fatalities + 0.5 * posts$injured
#posts$fatinj =  posts$fatalities * posts$injured
#posts$logYYYYMM = log10(posts$YYYYMM)



# reg
#---- TEST REGs reactions_comb ----
# REGS for fatalities and injured
# 1a = fatalities and injured; 1b = fat + inj, 1c = fatalities * injured
reg1a = lm(reactions_comb ~  YYYYMM + video
          + fatalities
          + injured
          , data = posts)

reg1b = lm(reactions_comb ~  YYYYMM + video
          + fat_plus_inj
          , data = posts)

reg1c = lm(reactions_comb ~ YYYYMM + video
           + fatalities
           + injured
           + fatinj
           , data = posts)

#Output
stargazer(reg1a, reg1b, reg1c, type="html",
          title = 'Determinants of level of public engagement',
          column.labels = c("Model 1", "Model 2", "Model 3"),
          dep.var.labels = "Total reactions",
          covariate.labels = c("Time", "Fatalities (FAT)", 'Injured (INJ)',
                               "Weighed sum: FAT + (0.5) INJ", 
                               "Interaction FAT/ INJ", "Video"),
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          order = c('YYYYMM', 'fatalities', 'injured', 'fat_plus_inj','fatinj'),
          star.char = "*",
          #notes = c('','Again, "SEP Republican" is omitted by R.'),
          notes.align = "l",
          out = "Models_post_analysis_1a_1b_1c.html")

#---- ACTUAL REGs reactions_comb ----
# model 1c: orthodox only
reg1 = lm(reactions_comb ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
          + YYYYMM
          #+ page_id
          + fatalities
          + injured
          #+ fatinj
          #+ islamist_attack
          #+ right_attack
          #+ left_attack
          #+ sep_loy_attack
          #+ sep_rep_attack
          #+ separatist
          #+ photo
          #+ video
          #+ link
          #+ terror_mentioned
          , data = posts)

# model 1 + Islamist
reg2 = lm(reactions_comb ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
          YYYYMM
          #+ page_id
          + fatalities
          + injured
          #+ fatinj
          + islamist_attack
          #+ right_attack
          #+ left_attack
          #+ sep_loy_attack
          #+ separatist
          #+ sep_rep_attack
          #+ photo
          #+ video
          #+ link
          #+ terror_mentioned
          , data = posts)

# all ideologies
reg3 = lm(reactions_comb ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
          YYYYMM
          #+ page_id
          + fatalities
          + injured
          #+ fatinj
          + islamist_attack
          + right_attack
          + left_attack
          + sep_loy_attack
          #+ separatist
          + sep_rep_attack
          #+ photo
          #+ video
          #+ link
          + type_ord
          + terror_mentioned
          , data = posts)

#OUTPUT
#summary(reg)

stargazer(reg1, reg2, reg3,
          type="html",
          title = 'Determinants of public engagement',
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          #dep.var.labels = "Total reactions per post",
          covariate.labels = c("Time", "Fatalities", 'Injured',
                               #"FAT * INJ",
                               "Islamist", "Right", "Left", "S-LOY", "S-REP",
                               "Post complexity", "Terror mentioned"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Total reactions per post",
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          order = c('YYYYMM', 'fatalities', 'injured', #'fatinj',
                    'islamist_attack', 'right_attack', 'left_attack',
                    'sep_loy_attack', 'sep_rep_attack', 'type_ord',
                    'terror_mentioned'),
          star.char = "*",
          notes = c('', "SEP-L: loyalist attacks (Northern Ireland). SEP-R: republican attacks (NI). SEP-R is omitted as the dummies referring to the ideology cover the entire data set. Thus, R omits the last when calculating the regression."),
          notes.align = "l",
          out = "Models_post_analysis.html")

# ---- Regressions num shares ----
# model 1
reg11 = lm(num_shares ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
            + YYYYMM
          #+ page_id
          + fatalities
          + injured
          + fatinj
          + islamist_attack
          + right_attack
          + left_attack
          + sep_loy_attack
          + sep_rep_attack
          + separatist
          + terror_mentioned
          + type_ord
          , data = posts)



#OUTPUT
#summary(reg)

stargazer(reg11,
          type="html",
          title = 'Determinants of num_shares',
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.labels = "Total number of shares per post",
          #covariate.labels = c("Time", "Fatalities (FAT)", 'Injured (INJ)',
          #                     "FAT * INJ",
          #                     "Islamist", "Right", "Left", "S-LOY", "S-REP",
          #                     "Post complexity", "Terror mentioned"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Total number of shares per post",
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          #order = c('YYYYMM', 'fatalities', 'injured', 'fatinj',
          #          'islamist_attack', 'right_attack', 'left_attack',
          #          'sep_loy_attack', 'sep_rep_attack', 'type_ord',
          #          'terror_mentioned'),
          star.char = "*",
          notes = c('', "SEP-L: loyalist attacks (Northern Ireland). SEP-R: republican attacks (NI). SEP-R is omitted as the dummies referring to the ideology cover the entire data set. Thus, R omits the last when calculating the regression."),
          notes.align = "l",
          out = "Models_post_analysis_num_shares.html")

# ---- Regressions num comments ----
# model 1
reg21 = lm(num_comments ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
             + YYYYMM
           #+ page_id
           + fatalities
           + injured
           + fatinj
           + islamist_attack
           + right_attack
           + left_attack
           + sep_loy_attack
           + sep_rep_attack
           + separatist
           + terror_mentioned
           + type_ord
           , data = posts)

#OUTPUT
#summary(reg)

stargazer(reg21,
          type="html",
          title = 'Determinants of num_shares',
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.labels = "Total number of shares per post",
          #covariate.labels = c("Time", "Fatalities (FAT)", 'Injured (INJ)',
          #                     "FAT * INJ",
          #                     "Islamist", "Right", "Left", "S-LOY", "S-REP",
          #                     "Post complexity", "Terror mentioned"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Total number of shares per post",
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          #order = c('YYYYMM', 'fatalities', 'injured', 'fatinj',
          #          'islamist_attack', 'right_attack', 'left_attack',
          #          'sep_loy_attack', 'sep_rep_attack', 'type_ord',
          #          'terror_mentioned'),
          star.char = "*",
          notes = c('', "SEP-L: loyalist attacks (Northern Ireland). SEP-R: republican attacks (NI). SEP-R is omitted as the dummies referring to the ideology cover the entire data set. Thus, R omits the last when calculating the regression."),
          notes.align = "l",
          out = "Models_post_analysis_num_comments.html")

# ---- Regressions num haha ----
# model 1
reg31 = lm(num_haha ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
             + YYYYMM
           #+ page_id
           + fatalities
           + injured
           + fatinj
           + islamist_attack
           + right_attack
           + left_attack
           + sep_loy_attack
           + sep_rep_attack
           + separatist
           + terror_mentioned
           + type_ord
           , data = posts)

#OUTPUT
#summary(reg)

stargazer(reg31,
          type="html",
          title = 'Determinants of X',
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.labels = "Total number of X per post",
          #covariate.labels = c("Time", "Fatalities (FAT)", 'Injured (INJ)',
          #                     "FAT * INJ",
          #                     "Islamist", "Right", "Left", "S-LOY", "S-REP",
          #                     "Post complexity", "Terror mentioned"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Total number of X per post",
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          #order = c('YYYYMM', 'fatalities', 'injured', 'fatinj',
          #          'islamist_attack', 'right_attack', 'left_attack',
          #          'sep_loy_attack', 'sep_rep_attack', 'type_ord',
          #          'terror_mentioned'),
          star.char = "*",
          notes = c('', "SEP-L: loyalist attacks (Northern Ireland). SEP-R: republican attacks (NI). SEP-R is omitted as the dummies referring to the ideology cover the entire data set. Thus, R omits the last when calculating the regression."),
          notes.align = "l",
          out = "Models_post_analysis_num_haha.html")

# ---- Regressions num angry ----
# model 1
reg41 = lm(num_angry ~ # excluded: index, page_name, status_id, attack_date, attacker_categorized, publication_datetime, status_type, status_message, link_name, status_link, num_likes, num_love, num_wow, num_haha, num_sad, num_angry, num_comments, num_shares
             + YYYYMM
           #+ page_id
           + fatalities
           + injured
           + fatinj
           + islamist_attack
           + right_attack
           + left_attack
           + sep_loy_attack
           + sep_rep_attack
           + separatist
           + terror_mentioned
           + type_ord
           , data = posts)

#OUTPUT
#summary(reg)

stargazer(reg41,
          type="html",
          title = 'Determinants of X',
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4"),
          dep.var.labels = "Total number of X per post",
          #covariate.labels = c("Time", "Fatalities (FAT)", 'Injured (INJ)',
          #                     "FAT * INJ",
          #                     "Islamist", "Right", "Left", "S-LOY", "S-REP",
          #                     "Post complexity", "Terror mentioned"),
          dep.var.labels.include = FALSE,
          dep.var.caption = "Total number of X per post",
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          #order = c('YYYYMM', 'fatalities', 'injured', 'fatinj',
          #          'islamist_attack', 'right_attack', 'left_attack',
          #          'sep_loy_attack', 'sep_rep_attack', 'type_ord',
          #          'terror_mentioned'),
          star.char = "*",
          notes = c('', "SEP-L: loyalist attacks (Northern Ireland). SEP-R: republican attacks (NI). SEP-R is omitted as the dummies referring to the ideology cover the entire data set. Thus, R omits the last when calculating the regression."),
          notes.align = "l",
          out = "Models_post_analysis_num_angry.html")

# ---- summary statistics ----
posts_filt <- posts
posts_filt[c("level_0", "index", "page_id", "status_id", "type_ord", "separatist")] <- list(NULL)

stargazer(posts_filt,
          type = "html",
          title="Descriptive statistics for Posts Analysis",
          digits=2,
          out="posts_des.html",
          covariate.labels = c("Index", "Number like", "Number love", "Number wow", "Number haha", "Number sad", "Number angry", "Number comment", "Number share", 
                             "Fatalities", "Injured",
                             "Islamist attack", "Left attack", "Right attack", "SEP loyalist attack", "SEP Republican attack", #"Separatist attack",
                             "Photo", "Video", "Link",
                             "Time (YYYYMM)", "Terror mentioned", "Combined Reactions"),
          digit.separator = "",
          notes = 'Notes: Each case is one posts. The descriptive statistics refer to attributes of the attack and the number of posts referring to it. The variables showing the page ID were omitted since they are not meaningful without context.'
          )