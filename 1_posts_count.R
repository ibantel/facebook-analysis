# analyzing post count

# preparation
library(foreign)
library(stargazer)
setwd("xxx")

posts_count <- read.csv("xxx/post-count.csv")
#View(posts_count)

posts_count$fatinj = posts_count$fatalities + 0.5 * posts_count$injured

### REGRESSION ON NUMBER OF POSTS ###
#---- Preliminary Regressions ---- included only for transparency
# All potential IVs | fatalities and injured
reg1a = lm(count ~ 
           YYYYMM # to control for time
           + fatalities
           + injured # to account for standard explanations
           #+ fatinj 
           #+ islamist_attack 
           #+ left_attack 
           #+ right_attack
           #+ sep_loy_attack 
           #+ sep_rep_attack
           #+ separatist
           , data = posts_count)
# ==> not included

# All potential IVs | fatalities + 0.5 injured
reg1b = lm(count ~ 
             YYYYMM # to control for time
           #+ fatalities
           #+ injured 
           + fatinj # to account for standard explanations
           #+ islamist_attack 
           #+ left_attack 
           #+ right_attack
           #+ sep_loy_attack 
           #+ sep_rep_attack
           #+ separatist
           , data = posts_count)
# ==> not included

# All potential IVs | fatalities * injured
reg1c = lm(count ~ 
             YYYYMM # to control for time
           + fatalities
           + injured # to account for standard explanations
           + fatalities * injured
           #+ islamist_attack 
           #+ left_attack 
           #+ right_attack
           #+ sep_loy_attack 
           #+ sep_rep_attack
           #+ separatist
           , data = posts_count)
# ==> not included

stargazer(reg1a, reg1b, reg1c,
          type="html",
          title = "Determinants of the attacks' magnitude",
          column.labels = c("Fatalities, injured","Weighed Sum ","Interaction"),
          covariate.labels = c("Fatalities * injured", "Fatalities", 'Injured',
                               "Fatalities + 0.5 * Injured", "Time", "Constant"),
          dep.var.caption = "Total number of posts on an attack",
          dep.var.labels = "",#Total number of posts on an attack",
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          order = c('fatalities:injured', 'fatalities', 'injured', 'fatinj',
                    'YYYYMM', 'Constant'),
          star.char = "*",
          out = "post_count_1a_1b_1c_new.html")
# ==> fatalities and injured should stay

#---- Actual Regressions ----
# from here only fatalities and injured [= 1a]
# only 'orthodox model'
reg1 = lm(count ~ 
          YYYYMM # to control for time
          + fatalities
          + injured # to account for standard explanations
          #+ islamist_attack 
          #+ left_attack 
          #+ right_attack
          #+ sep_loy_attack 
          #+ sep_rep_attack
          #+ separatist
          , data = posts_count)

# Only islamist_attack
reg2 = lm(count ~ 
            YYYYMM # to control for time
          + fatalities
          + injured # to account for standard explanations
          + islamist_attack 
          #+ left_attack 
          + right_attack
          #+ sep_loy_attack 
          #+ sep_rep_attack
          #+ separatist
          , data = posts_count)

# include all ideologies | also fatalities and injured
reg3 = lm(count ~ 
          YYYYMM # to control for time
          + fatalities
          + injured # to account for standard explanations
          + islamist_attack 
          + left_attack 
          + right_attack
          + sep_loy_attack 
          + sep_rep_attack
          #+ separatist
          , data = posts_count)

#summary(reg2)

stargazer(reg1, reg2, reg3,
          type="html",
          title = "Determinants of the attention given to terrorist attacks",
          column.labels = c("Model 1","Model 2","Model 3"),
          covariate.labels = c("Time", "Fatalities", 'Injured', "Islamist",
                               "Right", "Left", 
                               "SEP-L", "SEP-R"
                               ),
          dep.var.caption = "Total number of posts on an attack",
          dep.var.labels = "",#Total number of posts on an attack",
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          order = c('YYYYMM', 'fatalities', 'injured', 'islamist_attack',
                    'right_attack', "left_attack",
                    'sep_loy_attack', 'sep_rep_attack', 'Constant'
                    ),
          star.char = "*",
          notes = c('',
                    "SEP-L refers to loyalist attacks in the United Kingdom's separatist conflict in Northern Ireland.",
                    "SEP-R refers to republican attacks in this conflict.",
                    'The Variable "SEP-R" is omitted since the 5 dummies referring to the ideology cover the entire data set, and thus R omits the last when calculating the regression.'),
          notes.align = "l",
          out = "post_count_Models.html")

#----

#Summary Statistics posts_count
stargazer(posts_count,
          type = "html",
          title="Descriptive statistics for Posts Count",
          digits=2,
          out="posts_count_des.html",
          covariate.labels=c("Index",
                             "Fatalities", "Injured",
                             "Islamist attack", "Left attack", "Right attack", "SEP loyalist attack", "SEP Republican attack", "Separatist",
                             "Time (YYYYMM)", "Terror mentioned", "Post count", "Weighed sum fatalities, injured (factor 0.5)"),
          digit.separator = "",
          notes = "Note: Each case is one attack. The descriptive statistics refer to attributes of the attack and the number of posts referring to it.")
#----
### REGRESSION ON NUMBER OF terror_mentioned ###
posts_count$isl_fat = posts_count$islamist_attack * posts_count$fatalities

reg11 = lm(terror_mentioned ~
          YYYYMM # to control for time
          + fatalities
          + injured # to account for standard explanations
          + islamist_attack 
          #+ islamist_attack * fatalities
          #+ left_attack 
          #+ right_attack
          #+ sep_loy_attack 
          #+ sep_rep_attack
          #+ separatist
          , data = posts_count)


reg12 = lm(terror_mentioned ~ 
             YYYYMM # to control for time
           + fatalities
           + injured # to account for standard explanations
           + islamist_attack 
           + isl_fat
           #+ left_attack 
           #+ right_attack
           #+ sep_loy_attack 
           #+ sep_rep_attack
           #+ separatist
           , data = posts_count)

reg13 = lm(terror_mentioned ~ 
             YYYYMM # to control for time
           +  fatalities
           + injured # to account for standard explanations
           + islamist_attack 
           + isl_fat
           + left_attack 
           + right_attack
           + sep_loy_attack 
           + sep_rep_attack
           #+ separatist
           , data = posts_count)

stargazer(reg11, reg12, reg13, type="html",
          title = "Determinants of terrorism being mentioned",
          column.labels = c("Model 1","Model 2","Model 3"),
          dep.var.caption = "Number of mentionings of terrorism",
          covariate.labels = c("Time", "Fatalities", 'Injured',
                               "Islamist", "Islamist * Fatalities",
                               "Right",
                               "Left",
                               "SEP Loyalist", "SEP Republican"),
          dep.var.labels.include = FALSE,
          intercept.bottom = TRUE,
          model.names = TRUE,
          multicolumn = TRUE,
          model.numbers = FALSE,
          no.space = TRUE,
          object.names = FALSE,
          order = c('YYYYMM', 'fatalities', 'injured', 'islamist_attack', 
                    "isl_fat", 'right_attack', "left_attack",
                    'sep_loy_attack', 'sep_rep_attack', 'Constant'),
          star.char = "*",
          notes = c('','Again, "SEP Republican" is omitted by R.'),
          notes.align = "l",
          out = "post_terrorism_mention.html")

#View(posts_count)