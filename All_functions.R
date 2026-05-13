library(openxlsx)
library(readxl)
#Read in the data from Excel
raw<-read_xlsx("Raw_blanks.xlsx")

##Notes:
#May want to add a function that binarizes the working matrix for you so that the input raw matrix can 
  #contain multistate characters for ease of scoring individuals
#Can also have it interpret "?" and other non-integers as NA


##Set up working data frame for entire analysis. Use this object in each function (DF)
#Specimen names should be first col of spreadsheet
#Ontogenetic characters should be col names of spreadsheet
#Missing entries should be blanks in spreadsheet. If they are character strings, they may cause the columns to convert to character
Create_OSA_df<-function(DF){
  #Initiate empty list to begin building OSA df
  OSA_data<-list()
  #Ensure the input is a df and store it within the OSA df
  OSA_data$Raw<-as.data.frame(DF)
  #Prune identical rows from Raw to populate working matrix (first step in consolidating data)
  OSA_data$Working<-OSA_data$Raw[!duplicated(OSA_data$Raw[,-1]),]
  #Ensure that 1st column is type character
  OSA_data$Working[,1]<-as.character(OSA_data$Working[,1,drop=T])
  #Order by maturity for edge calculations
  OSA_data$Working<-OSA_data$Working[order(rowSums(OSA_data$Working[,-1],na.rm=T)),,drop=F]
  return(OSA_data)
}



##Insert a hypothetical Most/Least Mature Semaphoront as needed
End_members<-function(DF){
  #Extract numeric data columns
  num<-DF$Working[,-1,drop=F]
  #Generate data for Hypothetical Most Mature Semaphoront (column-wise max values)
  hmms_vals<-vapply(num,max,numeric(1),na.rm=T)
  #Generate data for Hypothetical Least Mature Semaphoront (all 0s)
  hlms_vals<-numeric(ncol(num))
  #Construct rows
  add_rows<-rbind(
    c("HMMS",hmms_vals),
    c("HLMS",hlms_vals)
  )
  #Ensure types are consistent
  add_rows<-as.data.frame(add_rows,stringsAsFactors=F)
  colnames(add_rows)<-colnames(DF$Working)
  
  #Convert data columns back to numeric
  add_rows[,-1]<-lapply(add_rows[,-1],as.numeric)
  #Append and remove if duplicated (i.e. L/MMS already present in dataset)
  out<-rbind(DF$Working,add_rows)
  out<-out[!duplicated(out[,-1]),]
  
  rownames(out)<-NULL
  DF$Working<-out
  #Reorder by maturity
  DF$Working<-DF$Working[order(rowSums(DF$Working[,-1],na.rm=T)),,drop=F]
  return(DF)
}




##Subfunction to find potentially equivalent semaphoronts
rows_equivalent<-function(r1,r2){
  #Vectorize data columns of each row being compared
  v1<-as.numeric(r1[-1])
  v2<-as.numeric(r2[-1])
  #Check for equivalence by subtracting corresponding values, taking their absolute vals, and summing them
  #If pairwise distance = 0, then the semaphoronts are potentially equivalent
  sum(abs(v1-v2),na.rm=T)==0
}

##Subfunction to fully consolidate semaphoront equivalence clique, with proper nomenclature
merge_group<-function(group_df){
  parent_names<-group_df[[1]]
  #Ensure data columns are numeric
  numeric_parents<-as.data.frame(lapply(group_df[-1],as.numeric))
  #Compute merged numeric values
  merged_vals<-sapply(numeric_parents,function(col){
    non_na<-col[!is.na(col)]
    if(length(non_na)==0)NA else non_na[1]
  })
  #Ensure merged_vals is numeric
  merged_vals<-as.numeric(merged_vals)
  #Check for exact value match with any parent row
  for(i in seq_len(nrow(numeric_parents))){
    parent_vals<-as.numeric(numeric_parents[i,])
    
    #Check whether an identical (including NAs) parent semaphoront already exists
    if(identical(parent_vals, merged_vals)){
      #Return parent semaphoront name instead of concatenated name
      return(c(parent_names[i],merged_vals))
    }
  }
  #Otherwise use concatenated name
  merged_name<-paste(parent_names,collapse="-")
  return(c(merged_name,merged_vals))
}


##Subfunction to find equivalence cliques, allowing semaphoronts to occupy multiple cliques as needed
find_cliques<-function(df){
  n<-nrow(df)
  #Initiate pairwise equivalence matrix
  eq<-matrix(F,n,n)
  diag(eq)<-T
  for(i in 1:(n-1)){
    for(j in (i+1):n){
      eq[i,j]<-rows_equivalent(df[i,],df[j,])
      eq[j,i]<-eq[i,j]
    }
  }
  #Initiate clique
  cliques<-list()
  for(i in seq_len(n)){
    group<-i
    added<-T
    
    #Grow clique around seed i
    while(added){
      added<-F
      for(candidate in setdiff(seq_len(n),group)){
        if(all(eq[candidate,group])){
          group<-c(group,candidate)
          added<-T
        }
      }
    }
    #Sort to avoid duplicates later
    cliques[[length(cliques)+1]]<-sort(group)
  }
  #Remove identical cliques
  unique_cliques<-unique(lapply(cliques,function(x)paste(x,collapse=",")))
  cliques<-lapply(unique_cliques,function(s)as.integer(strsplit(s,",")[[1]]))
  return(cliques)
}



##Consolidate equivalent incomplete individuals into maximally resolved semaphoronts
Consolidate<-function(DF){
  #Sort semaphoronts by equivalence allowing them to occupy multiple cliques
  groups<-find_cliques(DF$Working)
  #Consolidate each clique into a single, maximally resolved semaphoront
  merged_rows<-lapply(groups,function(idx){
    merge_group(DF$Working[idx,,drop=F])
  })
  #Build next data frame
  out<-as.data.frame(do.call(rbind,merged_rows),stringsAsFactors=F)
  names(out)<-names(DF$Working)
  #Ensure that data columns are numeric
  for(j in 2:ncol(out)){
    out[[j]]<-as.numeric(out[[j]])
    DF$Working<-out
  }
  #Reorder by maturity
  DF$Working<-DF$Working[order(rowSums(DF$Working[,-1],na.rm=T)),,drop=F]
  rownames(DF$Working)<-NULL
  return(DF)
}



##Permute remaining incomplete semaphoronts into min/max maturity variants
Permute<-function(DF){
  #Isolate incomplete semaphoronts
  incompletes<-DF$Working[apply(is.na(DF$Working[,-1]),1,any),]
  #Only proceed if there are incomplete semaphoronts to deal with
  if(nrow(incompletes)!=0){
    #Create a df of minimum maturity semaphoronts by replacing the NAs with 0s
    min<-incompletes
    min[]<-lapply(incompletes,function(col){
      if(is.numeric(col)){
        col[is.na(col)]<-0
      }
      col
    })
    #Rename these as "low_X" to keep track of them
    min[,1]<-paste("low",min[,1],sep = "-")
    #Create a df of maximum maturity semaphoronts by replacing the NAs with the max values
    #of the corresponding columns of the input df
    max<-incompletes
    max[]<-Map(function(col_max,col_DF){
      if(is.numeric(col_max)&&is.numeric(col_DF)){
        max_val<-max(col_DF,na.rm=TRUE)
        if(!is.infinite(max_val)){
          col_max[is.na(col_max)]<-max_val
        }}
      col_max
    },max,DF$Working)
    #Rename these as "high_X" to keep track of them
    max[,1]<-paste("high",max[,1],sep = "-")
    
    #Isolate the complete semaphoronts from the input df
    completes<-DF$Working[!do.call(paste,DF$Working)%in%do.call(paste,incompletes),]
    #Combine these with the max and min maturity permutations
    completes<-rbind(completes,min,max)
    #Prune any duplicates
    DF$Working<-completes[!duplicated(completes[,-1]),]
    #Reorder by maturity
    DF$Working<-DF$Working[order(rowSums(DF$Working[,-1],na.rm=T)),,drop=F]
  }
  return(DF)
}




#Find all edges not requiring event reversals
allowed_edges<-function(DF){
  #Isolate semaphoront names
  sems<-DF$Working[,1]
  #Isolate event scores
  values<-as.matrix(DF$Working[,-1])
  
  #Find all unique row pairs (i < j)
  pairs<-combn(seq_len(nrow(DF$Working)),2)
  
  #Create edges consisting of "From" and "To" semaphoronts
  #and calculate developmental events
  result<-lapply(seq_len(ncol(pairs)),function(k){
    i<-pairs[1,k]
    j<-pairs[2,k]
    c(From=sems[i],To=sems[j],values[j,]-values[i,])
  })
  
  #Bind results into a data frame
  Edge.Matrix<-as.data.frame(do.call(rbind,result),stringsAsFactors=F)
  #Convert event columns back to numeric
  Edge.Matrix[,-(1:2)]<-lapply(Edge.Matrix[,-(1:2)],as.numeric)
  #Prune edges involving reversals by removing any containing -1 values
  DF$Edges<-Edge.Matrix[rowSums(Edge.Matrix==-1)==0,,drop=F]
  #Calculate edge weights (semaphoront distances) and add as third column
  #Separate character from numeric columns
  edges<-DF$Edges[,1:2,drop=F]
  differences<-DF$Edges[,3:ncol(DF$Edges),drop=F]
  #Compute row-wise sum of numeric columns (edge weights)
  distances<-rowSums(differences)
  #rebind columns and create "Distance" column
  DF$Edges<-cbind(edges,Distance=distances,differences)
  return(DF)
}




##Find shortest edges - combined Up and Down passes
nearest_neighbors<-function(DF){
  #Split edges by "from" vertex
  From<-split(DF$Edges,DF$Edges[[1]])
  #Split edges by "to" vertex
  To<-split(DF$Edges,DF$Edges[[2]])
  
  #For each group, keep only the rows with minimum "Distance" value
  nearest_up<-lapply(From,function(g){
    g[g[[3]]==min(g[[3]]),,drop=F]
  })
  nearest_down<-lapply(To,function(g){
    g[g[[3]]==min(g[[3]]),,drop=F]
  })
  
  #Recombine into a single data frame
  nearest_up<-do.call(rbind,nearest_up)
  nearest_down<-do.call(rbind,nearest_down)
  
  #Combine and de-duplicate
  DF$Edges<-unique(rbind(nearest_up,nearest_down))
  #Make it prettier
  DF$Edges<-DF$Edges[order(DF$Edges$Distance),]
  rownames(DF$Edges)<-NULL
  return(DF)
}


##Find and remove remaining unnecessary edges
clean_up<-function(DF){
  #Helper to get counts of occurrences in specific columns
  get_counts<-function(data){
    from_counts<-table(data[[1]])
    to_counts<-table(data[[2]])
    list(from=from_counts,to=to_counts)
  }
  #Steps 1 & 2: Remove rows in single-row groups
  counts1<-get_counts(DF$Edges)
  
  #Identify which 'From' and 'To' values appear more than once
  keep_from<-names(counts1$from[counts1$from>1])
  keep_to<-names(counts1$to[counts1$to>1])
  
  #A row is kept only if BOTH its From and To vertices belong to groups > 1
  df_step2<-DF$Edges[DF$Edges[[1]]%in%keep_from&DF$Edges[[2]]%in%keep_to,]
  
  #Steps 3 & 4: Remove rows in groups > 1 (Keep only single-row groups)
  counts2<-get_counts(df_step2)
  
  #Identify which values appear EXACTLY once in the new subset
  only_one_from<-names(counts2$from[counts2$from==1])
  only_one_to<-names(counts2$to[counts2$to==1])
  
  #Keep rows where both From and To are now unique
  unnecessary<-df_step2[df_step2[[1]]%in%only_one_from&df_step2[[2]]%in%only_one_to,]
  
  #Remove unnecessary edges from edge matrix
  DF$Edges<-DF$Edges[!do.call(paste,DF$Edges)%in%do.call(paste,unnecessary),]
  return(DF)
}



##Apply all edge finding subfunctions
Edges<-function(DF){
  DF<-allowed_edges(DF)
  DF<-nearest_neighbors(DF)
  DF<-clean_up(DF)
  return(DF)
}



##Subfunction to find predicted semaphoronts
optimize<-function(DF){
  data_vals<-DF$Working[,-1]
  rownames(data_vals)<-DF$Working[[1]]
  
  get_fingerprint<-function(row)paste(as.numeric(row),collapse=",")
  existing_fingerprints<-apply(data_vals,1,get_fingerprint)
  calc_dist<-function(v1,v2)sum(abs(as.numeric(v1)-as.numeric(v2)))
  
  process_group<-function(common_vertex,parents,type){
    pairs<-combn(sort(parents),2,simplify=F)
    common_data<-as.numeric(data_vals[common_vertex,])
    
    lapply(pairs,function(p){
      v1_data<-as.numeric(data_vals[p[1],])
      v2_data<-as.numeric(data_vals[p[2],])
      
      res_vals<-if(type=="Maximum")pmax(v1_data,v2_data)else pmin(v1_data,v2_data)
      
      fingerprint<-get_fingerprint(res_vals)
      if(fingerprint %in% existing_fingerprints)return(NULL)
      
      dist_to_common<-calc_dist(res_vals,common_data)
      # Skip if distance is 0 (redundant)
      if(dist_to_common==0)return(NULL)
      
      prefix<-if(type=="Maximum")"max"else"min"
      v_name<-paste0(prefix,p[1],"-",p[2])
      
      rel<-data.frame(New_Vertex=v_name,Common=common_vertex, 
                      Parent_1=p[1],Parent_2=p[2], 
                      Distance_to_Common=dist_to_common,Type=type, 
                      stringsAsFactors=F)
      
      new_row<-as.data.frame(t(c(v_name,res_vals)),stringsAsFactors=F)
      colnames(new_row)<-colnames(DF$Working)
      return(list(row=new_row,rel=rel))
    })
  }
  # 1. Calculate Candidates
  groups_max<-split(DF$Edges[[1]],DF$Edges[[2]])
  groups_max<-Filter(function(x)length(x)>1,groups_max)
  groups_min<-split(DF$Edges[[2]],DF$Edges[[1]])
  groups_min<-Filter(function(x)length(x)>1,groups_min)
  
  all_candidates<-list()
  for(common in names(groups_max))all_candidates<-c(all_candidates,process_group(common,groups_max[[common]],"Maximum"))
  for(common in names(groups_min))all_candidates<-c(all_candidates,process_group(common,groups_min[[common]],"Minimum"))
  
  all_candidates<-Filter(Negate(is.null),all_candidates)
  
  if(length(all_candidates)>0){
    # Combine all proposed new vertices
    candidate_rows<-do.call(rbind,lapply(all_candidates,`[[`,"row"))
    candidate_rels<-do.call(rbind,lapply(all_candidates,`[[`,"rel"))
    
    # 2. Filter for GREATEST Distance_to_Common
    max_d<-max(candidate_rels$Distance_to_Common)
    best_indices<-which(candidate_rels$Distance_to_Common==max_d)
    
    selected_vertices<-candidate_rows[best_indices,,drop=F]
    selected_relations<-candidate_rels[best_indices,,drop=F]
    
    # Clean numeric types
    for(i in 2:ncol(selected_vertices)){
      selected_vertices[[i]]<-as.numeric(as.character(selected_vertices[[i]]))
    }
    
    # 3. Add to DF$Working (and ensure no duplicates in the final set)
    DF$Working<-rbind(DF$Working,selected_vertices)
    DF$Working<-DF$Working[!duplicated(apply(DF$Working[,-1],1,get_fingerprint)),]
    
    #Reorder by maturity
    DF$Working<-DF$Working[order(rowSums(DF$Working[,-1])),,drop=F]
    
    DF$New_Vertices<-selected_vertices
    DF$Relations<-selected_relations
  }else{
    DF$New_Vertices<-NULL
    DF$Relations<-NULL
  }
  return(DF)
}



##Semaphoront prediction
Predict_sems<-function(DF){
  repeat{
    old_vertex_count<-nrow(DF$Working)
    
    #1. Identify and add only the 'farthest' new vertices
    DF<-optimize(DF)
    
    # Check if any vertices were actually added
    if(nrow(DF$Working)==old_vertex_count){
      message("Graph converged. No new vertices added.")
      break
    }
    
    #2. Re-calculate the optimal path cover
    #In future, it may be necessary to redraw edges differently
    message(paste("Added",nrow(DF$Working)-old_vertex_count,"vertices. Recalculating edges..."))
    DF<-Edges(DF)
  }
  rownames(DF$Working)<-NULL
  return(DF)
}



##Calculate whole and partial semaphoront weights
#This one takes a while and can maybe be made more efficient
Sem_weights<-function(DF){
  #Identify character strings indicating hypothetical semaphoronts
  key<-"^(HMM|HLM|max|min)"
  #Separate hypothetical (HMMS,HLMS,and predicted) from observed semaphoronts
  hypothetical<-DF$Working[grepl(key,DF$Working[[1]]),]
  observed<-DF$Working[!grepl(key,DF$Working[[1]]),]
  
  #Add weights column to both, initially populated with 0s
  weights1<-rep(0,nrow(hypothetical))
  weights2<-rep(0,nrow(observed))
  
  #Insert weights column into hypotheticals
  sems1<-hypothetical[,1,drop=F]
  data1<-hypothetical[,-1,drop=F]
  hypothetical<-cbind(sems1,Weight=weights1,data1)
  
  #Do the same for the observed semaphoronts
  sems2<-observed[,1,drop=F]
  data2<-observed[,-1,drop=F]
  observed<-cbind(sems2,Weight=weights2,data2)
  
  #Isolate all incomplete individuals from the raw matrix
  incompletes<-DF$Raw[apply(is.na(DF$Raw[,-1]),1,any),-1]
  #Get complete individuals too
  completes<-as.data.frame(DF$Raw[!do.call(paste,DF$Raw[,-1])%in%do.call(paste,incompletes),-1])
  #Tally up whole weights for complete individuals
  completes<-aggregate(list(weights=1:nrow(completes)),completes,length)
  
  #Check whether there are any incomplete individuals
  if(nrow(incompletes)!=0){
    #Loop through each incomplete individual of the raw matrix
    for(i in 1:nrow(incompletes)){
      #Initialize an empty matrix to store potential equivalents from working matrix
      equiv<-matrix(nrow=0,ncol=ncol(data2))
      #Loop through each complete individual of the working matrix
      for(j in 1:nrow(data2)){
        #Add any potential equivalents to the temporary matrix
        if(sum(incompletes[i,]-data2[j,],na.rm=T)==0){
          equiv<-rbind(equiv,data2[j,])
        }
      }
      #Test if the equiv matrix isn't empty (or else you get a /0 error)
      if(nrow(equiv)!=0){
        #Add up the potential equivalents and divide 1 by the total to get partial scores contributed by 
        #incomplete individual i
        score<-1/nrow(equiv)
        #Distribute partial scores to the respective potential equivalents in the working matrix
        for(k in 1:nrow(equiv)){
          for(l in 1:nrow(data2)){
            if(identical(equiv[k,],data2[l,])){
              observed[[l,"Weight"]]<-sum(observed[[l,"Weight"]],score)
            }
          }
        }
      }
    }}
  
  #Add partial weights from incomplete individuals to whole weights from complete individuals
  for(i in 1:nrow(completes)){
    for(j in 1:nrow(data2)){
      if(identical(paste(completes[i,1:(ncol(completes)-1)]),paste(observed[j,-(1:2)]))){
        observed[[j,"Weight"]]<-sum(observed[[j,"Weight"]],completes[[i,"weights"]])
      }
    }
  }
  
  #Recombine and create new df for semaphoront weights
  DF$Weights<-rbind(observed,hypothetical)
  #Reorder by maturity for convenience
  DF$Weights<-DF$Weights[order(rowSums(DF$Weights[,-(1:2)])),,drop=F]
  rownames(DF$Weights)<-NULL
  return(DF)
}




##Find modal sequence and output a list of vertices w sequences ordered by sample frequency
Modal_seq<-function(DF,from_v,to_v){
  #load igraph
  if(!require("igraph")){
    install.packages("igraph")
    library("igraph")
  }
  
  #Convert edge matrix into igraph object
  graph<-graph_from_data_frame(d=DF$Edges,directed=T,vertices=DF$Working)
  
  #1. Find all sequences
  paths<-all_simple_paths(graph,from=from_v,to=to_v)
  
  #2. Create a named vector for fast sample frequency lookup
  #Assumes Col 1 = Name, Col 2 = Weight
  weight_map<-DF$Weights[[2]]
  names(weight_map)<-DF$Weights[[1]]
  
  #3. Calculate sum of semaphoront weights for each sequence
  path_sums<-sapply(paths,function(p){
    v_names<-names(p)
    sum(weight_map[v_names],na.rm=T)
  })
  
  #4. Extract sequences as a list of vertex name vectors
  path_list<-lapply(paths,function(p)names(p))
  
  #5. Order the list by total weight (Descending)
  order_idx<-order(path_sums,decreasing=T)
  
  # Reorder both the sequences and the weights
  DF$Sequences<-path_list[order_idx]
  DF$Sequence_Weights<-path_sums[order_idx]
  
  return(DF)
}




##Function to plot graph
Plot_OSA<-function(DF){
  #1. Setup
  #Extract semaphoront names and number them
  sems<-DF$Working[[1]]
  node_map<-setNames(seq_along(sems),sems)
  #Calculate y positions (maturity scores)
  y_vals<-rowSums(DF$Working[,-1])
  vertices<-data.frame(sems=sems,y=y_vals,stringsAsFactors=F)
  
  #2. Assign x based on sequence membership (laning)
  #Initialize x as NA
  vertices$x<-NA
  
  #First pass: The modal sequence (Sequence 1) gets x = 0
  spine_nodes<-DF$Sequences[[1]]
  vertices$x[vertices$sems%in%spine_nodes]<-0
  
  #Second pass: Assign other sequences to unique lanes
  current_lane<-0.5
  for(i in 2:length(DF$Sequences)){
    path<-DF$Sequences[[i]]
    #Find vertices in this sequence that haven't been assigned an x position yet
    unassigned<-is.na(vertices$x[match(path,vertices$sems)])
    
    if(any(unassigned)){
      #Assign all unassigned vertices in this specific sequence to a new vertical lane
      target_nodes<-path[unassigned]
      vertices$x[vertices$sems%in%target_nodes]<-current_lane
      current_lane<-current_lane+0.5
    }
  }
  #Fill any remaining vertices (not in any sequence) to the far right
  vertices$x[is.na(vertices$x)]<-max(vertices$x,na.rm=T)+0.5
  
  plot_data<-vertices
  plot_data$ID<-node_map[plot_data$sems]
  
  #3. Plotting
  max_x<-max(plot_data$x)
  y_ticks<-seq(floor(min(plot_data$y)),ceiling(max(plot_data$y)),by=1)
  
  plot(NULL,NULL, 
       xlim=c(-1.5,max_x+1.5), 
       ylim=c(min(plot_data$y)-1,max(plot_data$y)+1),
       xlab=NA,ylab="Maturity Score",
       main="Ontogenetic Sequence Graph",axes=F)
  
  axis(2,at=y_ticks,labels=y_ticks,lwd=0,las=1)
  abline(h=y_ticks,col="cyan",lwd=10)
  
  #4. Draw sequence edges
  path_indices<-seq_along(DF$Sequences)
  #Draw modal sequence last to keep it on top
  for(i in c(path_indices[path_indices!= 1],1)){
    p<-DF$Sequences[[i]]
    coords<-plot_data[match(p,plot_data$sems),]
    
    is_spine<-(i==1)
    lines(coords$x,coords$y, 
          col=if(is_spine)"black"else"gray60", 
          lwd=if(is_spine)4 else 1.5)
  }
  
  #5. Draw vertices on top of everything else
  points(plot_data$x,plot_data$y,pch=21,bg="white",col="black",cex=2.5)
  text(plot_data$x,plot_data$y,labels=plot_data$ID,cex=0.8,font=2)
  
  #Document sequences
  #Replace semaphoront names in $Sequences with corresponding numbers
  temp<-lapply(DF$Sequences,function(x){
    node_map[x]
  })
  #Start by finding the length of the longest sequence
  max_length<-max(sapply(temp,length))
  #Pad each shorter sequence with NAs and row-bind all of them
  seqs<-as.data.frame(do.call(rbind,lapply(temp,function(x){
    length(x)<-max_length
    return(x)
  })))
  seqs<-data.frame(cbind(seqs,Sequence_Weight=DF$Sequence_Weights))
  colnames(seqs)<-c(1:max_length,"Sequence_Weight")
  
  return(list(coordinates=plot_data,legend=data.frame(cbind(ID=plot_data[,"ID"],DF$Weights)),sequence_descriptions=seqs))
}