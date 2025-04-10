exercise <- read.table("https://content.sph.harvard.edu/fitzmaur/ala2e/exercise-data.txt", sep="", na.strings=".")
colnames(exercise) <- c("ID", "PROGRAM", "day0","day2", "day4", "day6", "day8", "day10", "day12")
exercise_long <- exercise %>% 
   pivot_longer(!(c("ID","PROGRAM")), names_to="time", values_to="strength") %>% 
   mutate(time=parse_number(time))

exercise_long_complete <- exercise_long %>% 
   filter(complete.cases(.))


vario <- variogram( indv = dental$ID,
                    time = dental$age,
                    Y = dental$growth )


vario <- variogram( indv = cd4$newpid,
                    time = cd4$time,
                    Y = sqrt(cd4$CD4CNT) )
vario$svar <- vario$svar[complete.cases(vario$svar),]


plot(vario, smooth=T, ylim=c(0,600))
