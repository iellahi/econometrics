### Digital Literacy Project ###
# ECO4421 Econometrics 


# 1 midterms and attendance ***
# Observation: +ve correlation but not enough variation in attendance to produce stron results

mid_att <- lm(avg_midterm ~ excercises, data=microdata)
summary(mid_att)
plot(microdata$excercises, microdata$avg_midterm)
abline(lm(microdata$avg_midterm ~ microdata$excercises))

# 2 psets and attendance
# Observation: +ve correlation but not enough variation in attendance to produce strong results

pset_att <- lm(avg_pset ~ attendance, data=microdata)

summary(att_pset)
plot(microdata$avg_pset, microdata$attendance)
abline(lm(microdata$avg_pset ~ microdata$attendance))

# 3 midterms and psets ***
# Observation: uncertain?? doesnt seem to be a very strong trend?

mid_pset <- lm(avg_midterm ~ avg_pset, data=microdata)
summary(mid_pset)
plot(microdata$avg_pset, microdata$avg_midterm)
abline(lm(microdata$avg_midterm ~ microdata$avg_pset))

# Multiple regression extra
multreg <- lm(avg_midterm ~ excercises+avg_pset+year, data=microdata)
summary(multreg)

# 4 midterms and year ***
# Observation: Looking at the graph, Juniors generally score higher than Seniors; not enough data on Sophomores

mid_year <- lm(avg_midterm ~ year, data=microdata)
summary(mid_year)
plot(microdata$year, microdata$avg_midterm)
abline(lm(microdata$avg_midterm ~ microdata$year))

# 5 midterms and major
# Observation: no clear difference, except that top scorers tend to be econ majors; no clear result on STEM majors scoring higher (act sci scored 18 but IT scored 14)

mid_maj <- lm(avg_midterm ~ economics, data=microdata)
summary(mid_maj)
plot(microdata$economics, microdata$avg_midterm)

# midterm1 and midterm2
# Observation: Clear +ve correlation between performance in both midterms

mid1_mid2 <- lm(midterm1 ~ midterm2, data=microdata)
summary(mid1_mid2)
plot(microdata$midterm1, microdata$midterm2)
abline(lm(microdata$midterm1 ~ microdata$midterm2))
