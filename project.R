rm(list = ls())

setwd("/Users/mayagabison/Desktop/IDC/R/Final paper")

movies <- read.csv("movies.csv")

sapply(movies, class)


# ----

movies$year <- as.ordered(movies$year)

movies$decade <- as.ordered(movies$decade)

movies$context.of.the.movie <- as.factor(movies$context.of.the.movie)

movies$terrorist.identity <- as.factor(movies$terrorist.identity)

movies$terrorist.type <- as.factor(movies$terrorist.type)

movies$good.guys <- as.factor(movies$good.guys)

ovies$terror.method <- as.factor(movies$terror.method)

movies$victims. <- as.factor(movies$victims.)

movies$target <- as.factor(movies$target)

movies$answer.def <- as.factor(movies$answer.def)

movies$terror.legitimacy <- as.factor(movies$terror.legitimacy)

movies$ideology.as.motive <- as.factor(movies$ideology.as.motive)




sapply(movies, class)


library("vcd")

#movies$motive[movies$motive<0] <- NA
#addNA(movies$motive)
#tbl_decade_identity <- table(movies$decade, movies$identity)
#chisq.test(tbl_decade_identity)

#chisq.test(table(movies$decade, movies$motive))

chisq.test(table(movies$decade, movies$ideology.as.motive))
chisq.test(table(movies$answer.def, movies$ideology.as.motive))

#mymodel1 <- table(ccga$Q5_20,ccga$Q30_11)


table(movies$decade, movies$ideology.as.motive)

decadeandmotive <- table(movies$ideology.as.motive, movies$decade)

barplot(decadeandmotive)

chisq.test(table(decadeandmotive))
#chisq.test(table(movies$terror.legitimacy, movies$good.guys)) p=9.327e-05 ########!!!!!!!!!########


# motives

chisq.test(table(movies$ideology.as.motive, movies$year)) # P=NA

chisq.test(table(movies$ideology.as.motive, movies$decade)) # p=.4655

chisq.test(table(movies$ideology.as.motive, movies$context.of.the.movie)) # p=0.03 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

chisq.test(table(movies$ideology.as.motive, movies$terrorist.identity)) # p=0.41

chisq.test(table(movies$ideology.as.motive, movies$terrorist.type)) # p=0.091

chisq.test(table(movies$ideology.as.motive, movies$good.guys)) # p=0.25

chisq.test(table(movies$ideology.as.motive, movies$terror.legitimacy))  # p=0.11

chisq.test(table(movies$ideology.as.motive, movies$terror.method)) # p=0.12

chisq.test(table(movies$ideology.as.motive, movies$victims.)) # p=0.4

chisq.test(table(movies$ideology.as.motive, movies$target)) # p=0.56

chisq.test(table(movies$ideology.as.motive, movies$answer.def)) # p=1.853e-08 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


# legitimacy 

chisq.test(table(movies$terror.legitimacy, movies$year)) # P=0.4

chisq.test(table(movies$terror.legitimacy, movies$decade)) # p=.03!!!!!!!!!!!!!!

chisq.test(table(movies$terror.legitimacy, movies$context.of.the.movie)) # p=0.0001075 !!!!!!!!!!!!!!!

chisq.test(table(movies$terror.legitimacy, movies$terrorist.identity)) # p=0.3

# chisq.test(table(movies$terror.legitimacy, movies$terrorist.type)) # p=0.51

chisq.test(table(movies$terror.legitimacy2, movies$good.guys)) # p= 9.327e-05 !!!!!!!!!!!!!!!!!!!!!!!

chisq.test(table(movies$terror.legitimacy, movies$terror.method)) # p=0.02!!!!!!!!!!!!!!!!!!!!!!!!!!!!

chisq.test(table(movies$terror.legitimacy, movies$victims.)) # p=1.835e-05 !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

chisq.test(table(movies$terror.legitimacy, movies$target)) # p=0.46

# chisq.test(table(movies$terror.legitimacy, movies$answer.def)) # p=0.087




#frequency tables leg

leg_decade <- table(movies$terror.legitimacy, movies$decade)
chisq.test(leg_decade)

prop.table(leg_decade, 2)



leg_context <- table(movies$terror.legitimacy, movies$context.of.the.movie)
barplot(leg_context)


leg_good <- table(movies$terror.legitimacy, movies$good.guys)
prop.table(leg_good, 2)


leg_method <- table(movies$terror.legitimacy, movies$terror.method)


leg_victim <- table(movies$terror.legitimacy, movies$victims.)


leg_def <- table(movies$terror.legitimacy, movies$answer.def)




library("vcd")

assocstats(leg_context)
assocstats(leg_decade)
assocstats(leg_good)
assocstats(leg_method)
assocstats(leg_victim)

chisq.test(leg_context)
chisq.test(leg_decade)
chisq.test(leg_context)

assocstats(leg_context, leg_decade)
chisq.test(leg_context)


# assocstats((table(movies$terror.legitimacy, movies$context.of.the.movie)))

mymodel2 <- table(movies$terror.legitimacy, movies$victims.)

prop.table(mymodel2, 2)

barplot(mymodel2)

prop.table(table(movies$terror.legitimacy, movies$victims., 1))

############################### Final


####### legitimacy 

plot(movies$terror.legitimacy
     ,las=1
     ,main="Legitimacy of Terror in Total"
     ,xlab = "Legitimacy"
     ,ylab = "# of movies"
     ,col = colorRampPalette(c("black","white"))(2)
     ,ylim =c(0,65)
)

pie(movies$terror.legitimacy)
     

######## Protagonist

prop.table(leg_good, 2)


######## victim
table(movies$victims., movies$terror.legitimacy)

assocstats(leg_victim)
table(leg_victim)
summary(movies$victims.)


######## decade
plot(movies$terror.legitimacy, movies$decade
     #,beside = TRUE
     )

# this one for vic+good

#terror by decade (not by years)
barplot(leg_decade
        ,main = "Legitimacy of Terror by Decades"
        ,beside=T
        ,las=1
        ,xlab = "years"
        ,ylab = "# of movies"
        ,col = colorRampPalette(c("black","white"))(2)
        ,ylim =c(0,30)
        )

#leg+vic plot
barplot(prop.table(table(movies$terror.legitimacy, movies$victims.), 2)
        ,main = "Legitimacy of Terror by Victim"
        ,beside=T
        ,las=1
        ,xlab = "Victim Identity"
        ,ylab = "# of movies"
        ,col = colorRampPalette(c("black","white"))(2)
        ,ylim =c(0,1)
         )
#######
plot(table(movies$decade[movies$terror.legitimacy == "no"])
     , type="o"
     , col="red"
     ,las=1
     ,ylim = c(0, 30)
     ,ylab ="# of movies"
     ,main="Legitimacy of Terror by Decades"
     ,xlab="Years"
     )

legend(1960,30
       , legend=c("No", "Yes")
       , col=c("red", "blue")
       , lty=1, cex=0.8)


lines(table(movies$decade[movies$terror.legitimacy == "yes"])
     ,type="o"
     ,col="blue"
     #,ylab = "__"
     ,las=1
     )


####

plot(prop.table(table(movies$decade[movies$terror.legitimacy == "no"]))
     , type="o"
     , col="red"
     ,las=1
     ,ylim = c(0, 0.6)
     ,ylab ="# of movies"
     ,main="Legitimacy of Terror by Decades"
     ,xlab="Years"
)

legend(1960,0.5
       , legend=c("No", "Yes")
       , col=c("red", "blue")
       , lty=1, cex=0.8)


lines(prop.table(table(movies$decade[movies$terror.legitimacy == "yes"]))
      ,type="o"
      ,col="blue"
      #,ylab = "__"
      ,las=1
)
prop.table(table(movies$decade[movies$terror.legitimacy == "yes"]))

plot(prop.table(table(movies$decade, movies$terror.legitimacy),1))

plot(prop.table(table(movies$decade, movies$terror.legitimacy),2)
     )

