#Alderman connections
ald <- read.csv("./aldermen.csv")
oldald <- stringr::str_split_fixed(ald$Person.Name, ",", 2)
oldald2 <- unlist(oldald[,1])

ald <- cbind(ald, oldald[,1])
write.csv(ald, "history_aldermen_wards.csv")

progressive<-c("King","Hairston","Sawyer", "Sadlowski Garza","Foulkes", "Moore", "Munoz" , 
               "Taliaferro" , "Waguespack" , "Ramirez-Rosa", "Arena")

ald$progressive <- ifelse(ald$'oldald[, 1]' %in% progressive, "yes", "no")

nepotism <- c("Sawyer","Thompson" , "Burke", "Brookins","Scott" , "Mell",
            "Austin" , "Laurino" , "Osterman", "Silverstein")

ald$nepotism <- ifelse(ald$'oldald[, 1]' %in% nepotism, "yes", "no")



#Source: http://www.chicagoreader.com/chicago/city-council-aldermen-caucuses-mayor-rahm-emanuel/Content?oid=17774160

beckycarroll <- c("Moreno","Hopkins", "Dowell","Burns", "Hairston","Harris","Beale","Thompson","O'Shea","Cochran", 
                  "Brookins",
                  "Zalewski", "Solis" , "Maldonado", "Burnett", "Mell" ,"Austin" ,"Mitts" ,"Laurino","O'Connor",
                  "Smith",
                  "Tunney","Cappleman","Pawar" ,"Moore","Silverstein")

  
labor <-  Proco Joe Moreno (First), Pat Dowell (Third), Will Burns (Fourth), Leslie Hairston (Fifth), Roderick Sawyer (Sixth), Sue Sadlowski Garza (Tenth)*, Patrick Daley Thompson (11th)*, Toni Foulkes (16th), David Moore (17th)*, Willie Cochran (20th), Howard Brookins Jr. (21st), Rick Munoz (22nd), Danny Solis (25th), Jason Ervin (28th), Scott Waguespack (32nd), Carlos Ramirez-Rosa (35th)*, Nicholas Sposato (38th), Anthony Napolitano (41st)*, John Arena (45th)


firstresponders <- Ed Burke (14th), Willie Cochran (20th), Chris Taliaferro (29th)*. Firefighters: Nicholas Sposato (38th). Anthony Napolitano (41st)* only gets one vote, even though he was a cop before he became a firefighter.

  
rauner <-Members: Brian Hopkins (Second)*, Brendan Reilly (42nd), Michele Smith (43rd), Tom Tunney (44th)

family <- Members: Roderick Sawyer (Sixth), son of a mayor; Patrick Daley Thompson (11th), nephew of one mayor and grandson of another mayor; Ed Burke (14th), son of an alderman; Howard Brookins Jr. (21st), son of a state senator; Michael Scott Jr. (24th)*, son of a mayoral adviser; Deb Mell (33rd), daughter of an alderman; Carrie Austin (34th), wife of an alderman; Margaret Laurino (39th), daughter of an alderman; Harry Osterman (48th), son of an alderman; and Debra Silverstein (50th), wife of a state senator

wonk <- Members: Pat Dowell (Third), Will Burns (Fourth), Leslie Hairston (Fifth), Scott Waguespack (32nd), John Arena (45th), Ameya Pawar (47th)
  
boss <- Members: Ed Burke (14th), Pat O'Connor (40th)
  
