#####################################################################################################

library(plyr)
library(dplyr)

#####################################################################################################
#                      GET ALL LINKS OF MERGE REQUEST TOGETHER WITH TITLE                          #
#####################################################################################################

# Personal access tokens to authenticate against Git over HTTP
token = "nzP2Q9Ra55yTfuvmg2BG"

# Issues from the first page
url<-paste0("https://git2u.molpay.com/api/v4/groups/","18","/issues?private_token=",token,"&updated_after=",Sys.Date()-100,"&updated_before=",Sys.Date()+1,"&per_page=100&page=1")
data<-try(jsonlite::fromJSON(url, flatten = TRUE),silent = T)

# Checking to see if there are any links to be retrieved
if(length(data) >= 1 ){
  data_title<-data%>%select(`title`)
  data_link<-data%>%select(`_links.self`)
  data_labels <- data%>%select(`labels`)
  data_state <- data%>%select(`state`)
}else{
  data_link<-NULL
  data_title<-NULL
  data_labels<-NULL
  data_state<-NULL
}


page = 2 
# Running for issues for the rest of the pages and merging them to one variable
while(TRUE){
  
  url<-paste0("https://git2u.molpay.com/api/v4/groups/","18","/issues?private_token=",token,"&updated_after=",Sys.Date()-100,"&updated_before=",Sys.Date()+1,"&per_page=100&page=",page)
  data<-try(jsonlite::fromJSON(url, flatten = TRUE),silent = T)
  
  # if no more links then stop loop
  if(is.null(data$`_links.self`)){
    break
  }
  dataW<-data%>%select(`state`)
  dataY<-data%>%select(`title`)
  dataX<-data%>%select(`_links.self`)
  dataZ<-data%>%select(`labels`)
  data_link<-rbind(data_link,dataX)
  data_title<-rbind(data_title,dataY)
  data_labels <- rbind(data_labels,dataZ)
  data_state <- rbind(data_state,dataW)
  
  page <- page + 1
  
}

#####################################################################################################
#                         GET ALL RELATED PARAMETERS FROM NOTES AND STATE                           #
#####################################################################################################

pointer = 1
assign = "\\<assigned\\>"
unassign = "\\<unassigned\\>"
close = "\\<closed\\>"
move = "\\<moved to\\>"
reopened = "reopened"
try_error = "Error in open.connection"
table = c("Tittle","URL","Assignee_name","Time_taken(minutes)","Status","Priority", "Severity")

# Go through every issue
while(pointer < nrow(data_link)){
  
  # Current issue
  task = data_link$`_links.self`[pointer]
  
  if(grepl("P0",data_labels$labels[pointer])){
    priority = "P0"
  }else if(grepl("P1",data_labels$labels[pointer])){
    priority = "P1"
  }else if(grepl("P2",data_labels$labels[pointer])){
    priority = "P2"
  }else if(grepl("P3" ,data_labels$labels[pointer])){
    priority = "P3"
  }else if(grepl("P4" ,data_labels$labels[pointer])){
    priority = "P4"
  }else{
    priority = "NA"
  }
  
  if(grepl("Severity::1 - Minimal",data_labels$labels[pointer])){
    severity = "Severity::1 - Minimal"
  }else if(grepl("Severity::2 - Medium",data_labels$labels[pointer])){
    severity = "Severity::2 - Medium"
  }else if(grepl("Severity::3 - Serious",data_labels$labels[pointer])){
    severity = "Severity::3 - Serious"
  }else if(grepl("Severity::4 - Critical" ,data_labels$labels[pointer])){
    severity = "Severity::4 - Critical"
  }else if(grepl("Severity::5 - Urgent" ,data_labels$labels[pointer])){
    severity = "Severity::5 - Urgent"
  }else{
    severity = "NA"
  }
  
  # Get the list of all notes for a single issue from API for page 1
  task_note = paste0(task,"/notes?private_token=",token,"&per_page=100&page=1")
  tnote = try(jsonlite::fromJSON(task_note,flatten = TRUE),silent = T)
  attempt <- 1
  # Checking to see if there is any content in the notes and retrieve the relevant variables
  if (length(tnote) >= 1){
    #retry if connection fail
    while (grepl(try_error,tnote[1]) & attempt <= 5){
      attempt <- attempt + 1
      tnote = try(jsonlite::fromJSON(task_note, flatten = TRUE))
    }
    system = tnote%>%select(`system`)
    created_at = tnote%>%select(`created_at`)
    body = tnote%>%select(`body`)
    username = tnote%>%select(`author.username`)
  }else{
    system = NULL
    created_at = NULL
    body = NULL
    username = NULL
  }
  
  
  
  
  page = 2
  # Get the list of all notes for a single issue from API for the rest of the pages
  while(TRUE){    
    task_note = paste0(task,"/notes?private_token=",token,"&per_page=100&page=",page)
    tnoteX = try(jsonlite::fromJSON(task_note,flatten = TRUE),silent = T)
    
    # If no more notes then stop loop
    if(length(tnoteX) < 1 ){
      break
    }
    
    attempt <- 1
    # Checking to see if there is any content in the notes and retrieve the relevant variables
    if (!is.null(nrow(tnoteX)) & length(tnote) >= 1){
      #retry if connection fail
      while (grepl(try_error,tnoteX[1]) & attempt <= 5){
        attempt <- attempt + 1
        tnoteX = try(jsonlite::fromJSON(task_note,flatten = TRUE))
      }
      systemX = tnoteX%>%select(`system`)
      created_atX = tnoteX%>%select(`created_at`)
      bodyX = tnoteX%>%select(`body`)
      usernameX = tnoteX%>%select(`author.username`)
      system = rbind(system,systemX)
      created_at = rbind(created_at,created_atX)
      body = rbind(body,bodyX)
      username = rbind(username,usernameX)
    }
    
    page = page + 1
  }
  
  
  # Get the list of all states for a single issue from API for page 1
  task_state = paste0(task,"/resource_state_events?private_token=",token,"&per_page=100&page=1")
  tstate = try(jsonlite::fromJSON(task_state,flatten = TRUE),silent = T)
  attempt <- 1
  # Checking to see if there is any content in the state and retrieve the relevant variables
  if (length(tstate) >= 1){
    #retry if connection fail
    while (grepl(try_error,tstate[1]) & attempt <= 5){
      attempt <- attempt + 1
      tstate = try(jsonlite::fromJSON(task_state,flatten = TRUE))
    }
    state_created_at<-tstate%>%select(`created_at`)
    state<-tstate%>%select(`state`)
    state_username<-tstate%>%select(`user.username`)
  }else{
    state_created_at = NULL
    state = NULL
    state_username = NULL
  }
  
  
  
  page = 2
  # Get the list of all notes for a single issue from API for the rest of the pages
  while(TRUE){
    task_state = paste0(task,"/resource_state_events?private_token=",token,"&per_page=100&page=",page)
    tstateX = try(jsonlite::fromJSON(task_state,flatten = TRUE),silent = T)
    
    # If no more notes then stop loop
    if(length(tstateX) < 1 ){
      break
    }
    
    attempt <- 1
    # Checking to see if there is any content in the state and retrieve the relevant variables
    if (!is.null(nrow(tstateX)) & length(tstate) >= 1){
      #retry if connection fail
      while (grepl(try_error,tstateX[1]) & attempt <= 5){
        attempt <- attempt + 1
        tstateX = try(jsonlite::fromJSON(task_state, flatten = TRUE))
      }
      state_created_atX<-tstateX%>%select(`created_at`)
      state_created_at<-rbind(state_created_at,state_created_atX)
      
      stateX<-tstateX%>%select(`state`)
      state<-rbind(state,stateX)
      
      state_usernameX <- tstateX%>%select(`user.username`)
      state_username<- rbind(state_username,state_usernameX)
    }
    
    page = page + 1
  }
  
  #####################################################################################################
  #                    GET THE TIME TAKEN OF ALL ASSIGNEE UNTIL IT IS CLOSED                          #
  #####################################################################################################  
  
  flag = FALSE
  reopen = FALSE
  no_of_states = 0
  bug = FALSE
  reopening = FALSE
  current_status = data_state$state[pointer]
  
  
  # check for closed issue
  if ( !is.null(nrow(tstate)) ){
    # checks if the issues is currently closed
    if (grepl(close,state$state[nrow(state)])){
      # Retrieve the closing time of the issue
      close_time = state_created_at$created_at[1]
      close_time = gsub("T"," ",close_time)
      #current_status = "Closed"
    }else{
      close_time = Sys.time()
      close_time = gsub("T"," ",close_time)
      #current_status = "Open"
    }
  }else{
    close_time = Sys.time()
    close_time = gsub("T"," ",close_time)
    #current_status = "Open"
    counter = nrow(body)
    if(!is.null(nrow(body))){
      counter = nrow(body)
      while (counter > 0){
        if(grepl(close,body$body[counter]) & system$system[counter] | (grepl(move,body$body[counter])& system$system[counter])) {
          no_of_states = no_of_states + 1
          temp_close_time = created_at$created_at[counter]
          temp_close_time = gsub("T"," ",temp_close_time)
          counter = counter - 1
        }
        else if (grepl(reopened,body$body[counter]) & system$system[counter]){
          temp_start_time = created_at$created_at[counter]
          temp_start_time = gsub("T"," ",temp_start_time)
          no_of_states = no_of_states + 1
          counter = counter - 1
        }
        else{
          counter = counter - 1
        }
      }
      if(no_of_states >= 2){
        reopen = TRUE
        bug = TRUE
      }
    }
  }
  
  # check for reopen
  if((length(state)>0)){
    if(nrow(state) >= 2){
      reopen = TRUE
      no_of_states = nrow(state)
      close_time = state_created_at$created_at[1]
      close_time = gsub("T"," ",close_time)
    }
  }
  
  if (!is.null(body)){
    counter = nrow(body)
    # go through all of the notes
    while(counter > 0) {
      curr_time = created_at$created_at[counter]
      curr_time = gsub("T"," ",curr_time)
      if(bug){
        close_time = temp_close_time
      }
      if(difftime(close_time,curr_time) >= 0 ){
        # if no one assigned yet
        if (flag == FALSE){
          # if its a system command and its assigning the issue to someone
          if(grepl(assign,body$body[counter]) & system$system[counter]){
            flag = TRUE
            
            # get assignee's username
            author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
            author_name = unlist(strsplit(author_name, split=' ', fixed=TRUE))[1]
            # get start time
            stime = created_at$created_at[counter]
            next_author_name = author_name
            counter = counter - 1
          }
          else{
            counter = counter - 1
          }
        }
        # if someone is assigned
        else {
          # if the person current assigned is being unassigned as well as if another person is being assigned
          if((grepl(unassign,body$body[counter]) & grepl(assign,body$body[counter])) & system$system[counter]){
            # the next person being assigned to the issue
            next_author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
            next_author_name = unlist(strsplit(next_author_name, split=' ', fixed=TRUE))[1]
            
            # the end time of the assignee
            etime = created_at$created_at[counter]
            stime = gsub("T"," ",stime)
            etime = gsub("T"," ",etime)
            # calculating time taken for the assignee in minutes
            time_taken = abs(difftime(etime,stime,units="mins"))
            assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
            # append to table
            table = rbind(table,assignee)
            stime = created_at$created_at[counter]
            stime = gsub("T"," ",stime)
            author_name = next_author_name
            counter = counter - 1
          }
          else if ((grepl(unassign,body$body[counter]))& system$system[counter]){
            etime = created_at$created_at[counter]
            stime = gsub("T"," ",stime)
            etime = gsub("T"," ",etime)
            # calculating time taken for the assignee in minutes
            time_taken = abs(difftime(etime,stime,units="mins"))
            assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
            # append to table
            table = rbind(table,assignee)
            # NO assignee found for that issue, flag become FALSE
            flag = FALSE
            counter = counter - 1
          }
          else if (grepl(close,body$body[counter]) & system$system[counter] | (grepl(move,body$body[counter])& system$system[counter])){
            close_time = created_at$created_at[counter]
            close_time = gsub("T"," ",close_time)
            counter = counter - 1
            break
          }else if (reopened%in%body$body[counter] & system$system[counter]){
            temp_start_time = created_at$created_at[counter]
            
            break
            
          }else{ 
            counter = counter - 1
          }
        }
      }else{
        break
      }
    }
  }
  
  
  if(flag ){
    etime = close_time
    stime = gsub("T"," ",stime)
    etime = gsub("T"," ",etime)
    # count time taken
    time_taken = abs(difftime(etime,stime,units="mins"))
    # get info of the task
    assignee = c(data_title$title[pointer],task,next_author_name,time_taken,current_status,priority,severity)
    
    if(!is.null(nrow(table)) & length(state) > 0){
      # last assignee was at the last row of the table
      if(state_username$user.username[1] == table[nrow(table),3]){
        # count time taken
        ctime = as.numeric(difftime(etime,stime,units="mins"))
        # combine the time of assign --> unassigned and unassigned --> issue closed
        prev_time = as.numeric(table[nrow(table),4])
        table[nrow(table),4] = prev_time + ctime[1]
      }else{
        # bind the last assignee into the table
        table = rbind(table,assignee)
      }
    }else{
      # bind the last assignee into the table
      table = rbind(table,assignee)
    }
  }
  
  #####################################################################################################
  #                    GET THE TIME TAKEN OF ALL ASSIGNEE IF IT WAS REOPENED                          #
  #####################################################################################################  
  
  if(reopen){
    index = 1
    while(no_of_states >= 3){
      index = index + 2
      if(bug){
        close_time = temp_close_time
        stime = temp_start_time
        stime = gsub("T"," ",stime)
      }else{
        close_time = state_created_at$created_at[index]
        close_time = gsub("T"," ",close_time)
        
        stime = state_created_at$created_at[index-1]
        stime = gsub("T"," ",stime)
      }
      while(counter > 0 ){
        curr_time = created_at$created_at[counter]
        curr_time = gsub("T"," ",curr_time)
        if(!is.null(nrow(state_created_at))){
          reopen_time = state_created_at$created_at[index - 1]
          reopen_time = gsub("T"," ",reopen_time)
        }else{
          reopen_time = created_at$created_at[counter]
          reopen_time = gsub("T"," ",reopen_time)
        }
        if ((reopened%in%body$body[counter] & system$system[counter]) | (difftime(curr_time,reopen_time,units = "mins") >= 0)){
          reopening = TRUE}
        if ((close%in%body$body[counter] & system$system[counter]) | (difftime(curr_time,reopen_time,units = "mins") < 0)){
          reopening = FALSE}
        if(reopening){
          if(difftime(close_time,curr_time) >= 0 ){
            # if the person current assigned is being unassigned as well as if another person is being assigned
            if((grepl(unassign,body$body[counter]) & grepl(assign,body$body[counter])) & system$system[counter]){
              # the next person being assigned to the issue
              next_author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
              next_author_name = unlist(strsplit(next_author_name, split=' ', fixed=TRUE))[1]
              
              # the end time of the assignee
              etime = created_at$created_at[counter]
              stime = gsub("T"," ",stime)
              etime = gsub("T"," ",etime)
              # calculating time taken for the assignee in minutes
              time_taken = abs(difftime(etime,stime,units="mins"))
              assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
              # append to table
              table = rbind(table,assignee)
              author_name = next_author_name
              stime = etime
              counter = counter - 1
            }else if ((grepl(unassign,body$body[counter]))){
              etime = created_at$created_at[counter]
              stime = gsub("T"," ",stime)
              etime = gsub("T"," ",etime)
              # calculating time taken for the assignee in minutes
              time_taken = abs(difftime(etime,stime,units="mins"))
              assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
              # append to table
              table = rbind(table,assignee)
              # NO assignee found for that issue, flag become FALSE
              flag = FALSE
              counter = counter - 1
            }else if (grepl(close,body$body[counter]) & system$system[counter] | (grepl(move,body$body[counter])& system$system[counter])){
              close_time = created_at$created_at[counter]
              close_time = gsub("T"," ",close_time)
              counter = counter - 1
              break
            }else if (reopened%in%body$body[counter] & system$system[counter]){
              temp_start_time = created_at$created_at[counter]
              counter = counter - 1
              reopening = FALSE
              break
              
            }else{ 
              counter = counter - 1
            }
          }else{
            break
          }
        }else{
          if((grepl(unassign,body$body[counter]) & grepl(assign,body$body[counter])) & system$system[counter]){
            # the next person being assigned to the issue
            next_author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
            next_author_name = unlist(strsplit(next_author_name, split=' ', fixed=TRUE))[1]
            author_name = next_author_name
            counter = counter - 1
          }else{
            counter = counter - 1
          }
        }
      }
      
      if(flag){
        etime = close_time
        stime = gsub("T"," ",stime)
        etime = gsub("T"," ",etime)
        # count time taken
        time_taken = abs(difftime(etime,stime,units="mins"))
        # get info of the task
        assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
        
        if(!is.null(nrow(table)) & length(state) > 0){
          # last assignee was at the last row of the table
          if(state_username$user.username[index] == table[nrow(table),3]){
            # count time taken
            ctime = as.numeric(difftime(etime,stime,units="mins"))
            # combine the time of assign --> unassigned and unassigned --> issue closed
            prev_time = as.numeric(table[nrow(table),4])
            table[nrow(table),4] = prev_time + ctime[1]
          }else{
            # bind the last assignee into the table
            table = rbind(table,assignee)
          }
        }else{
          # bind the last assignee into the table
          table = rbind(table,assignee)
        }
      }
      no_of_states = no_of_states - 2
      
    }
    if(reopened%in%state$state[nrow(state)]){
      stime = state_created_at$created_at[no_of_states]
      stime = gsub("T"," ",stime)
      close_time = Sys.time()
      reopen_time = state_created_at$created_at[nrow(state)]
    }
    while(counter > 0 ){
      curr_time = created_at$created_at[counter]
      curr_time = gsub("T"," ",curr_time)
      if ((reopened%in%body$body[counter] & system$system[counter])){
        reopening = TRUE}
      if(reopening){
        if(difftime(close_time,curr_time) >= 0 ){
          # if the person current assigned is being unassigned as well as if another person is being assigned
          if((grepl(unassign,body$body[counter]) & grepl(assign,body$body[counter])) & system$system[counter]){
            # the next person being assigned to the issue
            next_author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
            next_author_name = unlist(strsplit(next_author_name, split=' ', fixed=TRUE))[1]
            
            # the end time of the assignee
            etime = created_at$created_at[counter]
            stime = gsub("T"," ",stime)
            etime = gsub("T"," ",etime)
            # calculating time taken for the assignee in minutes
            time_taken = abs(difftime(etime,stime,units="mins"))
            assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
            # append to table
            table = rbind(table,assignee)
            author_name = next_author_name
            stime = etime
            counter = counter - 1
          }else if ((grepl(unassign,body$body[counter]))){
            etime = created_at$created_at[counter]
            stime = gsub("T"," ",stime)
            etime = gsub("T"," ",etime)
            # calculating time taken for the assignee in minutes
            time_taken = abs(difftime(etime,stime,units="mins"))
            assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
            # append to table
            table = rbind(table,assignee)
            # NO assignee found for that issue, flag become FALSE
            flag = FALSE
            counter = counter - 1
          }else if (grepl(close,body$body[counter]) & system$system[counter] | (grepl(move,body$body[counter])& system$system[counter])){
            close_time = created_at$created_at[counter]
            close_time = gsub("T"," ",close_time)
            counter = counter - 1
            break
          }else if (reopened%in%body$body[counter] & system$system[counter]){
            temp_start_time = created_at$created_at[counter]
            counter = counter - 1
            break
            
          }else{ 
            counter = counter - 1
          }
        }else{
          break
        }
      }else{
        if((grepl(unassign,body$body[counter]) & grepl(assign,body$body[counter])) & system$system[counter]){
          # the next person being assigned to the issue
          next_author_name = unlist(strsplit(body$body[counter], split='@', fixed=TRUE))[2]
          next_author_name = unlist(strsplit(next_author_name, split=' ', fixed=TRUE))[1]
          author_name = next_author_name
          counter = counter - 1
        }else{
          counter = counter - 1
        }
      }
    }
    
    if(!(author_name == table[nrow(table),3]) & !(as.numeric(time_taken) == as.numeric(table[nrow(table),4])) ){
      etime = close_time
      etime = gsub("T"," ",etime)
      # count time taken
      time_taken = abs(difftime(etime,stime,units="mins"))
      # get info of the task
      assignee = c(data_title$title[pointer],task,author_name,time_taken,current_status,priority,severity)
      
      if(!is.null(nrow(table)) & length(state) > 0){
        # last assignee was at the last row of the table
        if( table[nrow(table),3] == author_name ){
          # count time taken
          ctime = as.numeric(difftime(etime,stime,units="mins"))
          # combine the time of assign --> unassigned and unassigned --> issue closed
          prev_time = as.numeric(table[nrow(table),4])
          table[nrow(table),4] = prev_time + ctime[1]
        }else{
          # bind the last assignee into the table
          table = rbind(table,assignee)
        }
      }else{
        # bind the last assignee into the table
        table = rbind(table,assignee)
      }
    }
  }

  # increment pointer
  pointer = pointer + 1
}

#####################################################################################################
#                                   BEAUTIFY THE TABLE CREATED                                      #    
#####################################################################################################

# beautify the table
colnames(table) = c("Title","URL","Assignee_name","Time_taken(minutes)","Status","Priority","Severity")
table = table[-1,] # remove first row
rownames(table) = NULL

#####################################################################################################
#                                         WRITE TO CSV                                              #    
#####################################################################################################

write.csv(table,"gitTable.csv")

#####################################################################################################
