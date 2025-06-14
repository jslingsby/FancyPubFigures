library(scholar)
library(tidyverse)
library(googledrive)
library(googlesheets4)
library(doParallel)


### Script to compare academics by rank between institutions. Currently based on existing Google Scholar profiles
### Could explore using easyPubMed too? see https://remybeugnon.netlify.app/post/extract-my-scientific-profile-with-r/

### Read in scholars from Google Sheet

gs4_auth(email = "jslingsby@gmail.com")

#gs4_auth()

spreadsheet <- "https://docs.google.com/spreadsheets/d/14MVh04HSjnPEhiBixTFr1qeNTU_FCWukmdptF_PUMrA/edit?gid=0#gid=0"

scholars <- read_sheet(ss = spreadsheet, sheet = "Input")


### Separate first and last names and do Google ID search

scholars <- scholars %>% separate(Name, c("FirstName", "Surname"), extra = "merge")

#ID <- foreach(i=1:nrow(scholars)) %do% get_scholar_id(first_name = scholars$FirstName[i], last_name = scholars$Surname[i])

ID <- list()

for (i in 1:nrow(scholars)) {
  ID[i] <- get_scholar_id(first_name = scholars$FirstName[i], last_name = scholars$Surname[i])
  Sys.sleep(10)
  }

scholars$ID <- c(unlist(ID), rep(NA, nrow(scholars)-length(ID))) #Add ID to match back onto


### Stitch in manual IDs where needed

scholars <- scholars %>% 
  mutate(ScholarID = coalesce(Manual_ID, ID))

### Save version and/or read in a previous sheet
#write_sheet(scholars, ss = spreadsheet, sheet = paste0("Output2_",Sys.Date()))
#scholars <- read_sheet(ss = spreadsheet, sheet = "Output2_2024-09-06")

### Extract profiles and convert to data frame
scholars <- scholars %>% filter(!is.na(ScholarID)) %>% filter(is.na(`No profile`))

#pro <- foreach(i=1:length(na.omit(unlist(ID)))) %do% get_profile(id = na.omit(unlist(ID))[i])

pro <- list()
  
for (i in 1:nrow(scholars)) {
  pro[[i]] <- get_profile(id = scholars$ScholarID[i])
  Sys.sleep(10)
}

fields <- c("id", "name", "affiliation", "total_cites", "h_index", "i10_index")

prout <- foreach(i=1:length(fields)) %do% unlist(purrr::map(pro, fields[i]))

names(prout) <- fields

pr <- as.data.frame(do.call(cbind,prout))


### Join with original list and plot
# # Add n? - see https://gist.github.com/konradmayer/d0330601eb0206ba46b2bbabed1bb107

dat <- inner_join(pr, scholars, join_by(id == ScholarID))
#write_sheet(dat, ss = spreadsheet, sheet = paste0("Data2_",Sys.Date()))
#dat <- read_sheet(ss = spreadsheet, sheet = "Data2_2025-06-12")

# dat %>% select(c("Institution", "Level", "total_cites", "h_index", "i10_index")) %>%
#   pivot_longer(c("total_cites", "h_index", "i10_index")) %>%
#   mutate(value = as.numeric(value)) %>%
#   mutate(Level = factor(Level, c("Lecturer", "Senior Lecturer", "Associate Professor", "Professor"))) %>%
#   mutate(Institution = factor(Institution, c("UCT", "Rhodes", "Wits"))) %>%
#   ggplot(aes(x = Level, y = value, colour = Institution)) +
#   geom_violin() +
#   facet_wrap(.~name, scales = "free") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

dat1 <- dat %>% filter(name != "Patrick O'Farrell")

my_xlab <- dat1 %>%   
  mutate(Level = factor(Level, c("Lecturer", "Senior Lecturer", "Associate Professor", "Professor"))) %>%
  mutate(Institution = factor(Institution, c("UCT", "NMU", "SUN", "Rhodes", "UP", "UJ", "UWC", "Wits"))) %>%
  group_by(Level, Institution) %>%
  mutate(n= as.character(n())) %>%
  #count(Level, Institution) %>% #group_by(Level, Institution) %>%  paste("\n(N=",table(Level, Institution),")",sep="") %>%
  mutate(Label = paste(Level," ", "\n(N=",n ,")",sep=""))

mystats <- data.frame(name = c("total_cites", "h_index", "i10_index"), Z = c(5478, 26, 42))

dat1 %>% select(c("Institution", "Level", "total_cites", "h_index", "i10_index", "ID")) %>%
  mutate(Level = factor(Level, c("Lecturer", "Senior Lecturer", "Associate Professor", "Professor"))) %>%
  mutate(Institution = factor(Institution, c("UCT", "NMU", "SUN", "Rhodes", "UP", "UJ", "UWC", "Wits"))) %>%
  pivot_longer(c("total_cites", "h_index", "i10_index")) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(Level, Institution) %>%
  mutate(n= as.character(n())) %>%
  #mutate(Label = paste(Level," ", "\n(N=",n ,")",sep="")) %>%
  ggplot(aes(x = Level, y = value, colour = Institution)) +
  geom_boxplot(outliers = F) +
#  geom_text(mapping = aes(label = n), y = 1) +
#  geom_hline(yintercept = .[which(ID == "ZjBauSkAAAAJ")]$value) +
#  geom_jitter(width = 0.1,alpha = 0.3) +
  facet_wrap(.~name, scales = "free") +
  #geom_text(aes(y = rep(1, nrow(my_xlab)), x = 1:nrow(my_xlab)), label = my_xlab$Label) +
  #coord_flip() +
  #scale_x_discrete(labels=my_xlab$Label) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  geom_hline(data = mystats, aes(yintercept = Z, color = "My Statistics"), linetype = "dashed") 
  
ggsave("scholar_rank_comparison.pdf", width = 8, height = 3.5)
  

dat %>% filter(Level == "Senior Lecturer") %>% filter(as.numeric(h_index) > 25)

dat %>% filter(as.numeric(h_index) > 100)

# Check affiliations

#scholars$H <- unlist(pro)
  
#compare_scholar_careers(ids = scholars, career = TRUE)

GGally:ggpairs()
ggplot()


###

tutu <- get_profile(id = "ZjBauSkAAAAJ")

coauthor_network <- get_coauthors("ZjBauSkAAAAJ", n_coauthors = 100, n_deep = 1)
plot_coauthors(coauthor_network)
