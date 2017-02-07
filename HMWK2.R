
DataBase = read.table("C:/Users/Merlin Mpoudeu/SkyDrive/Documents/CSCE_874/Homework/Weather.txt", header = T)
DataBase = as.data.frame(DataBase)
DataBase[,"windy"]= factor(DataBase[,"windy"])
library(sets)
#s = c("a","d","c")
#cset_power(s)
DataBase = read.csv("C:/Users/Merlin Mpoudeu/SkyDrive/Documents/CSCE_874/Homework/vote.csv", header = T)
#str(Vote)
# this function will test if two set are equal
SETEQUAL = function(A1,A2){
  sum = 0
  for(i in 1:length(A1)){
    if(A1[i]==A2[i]){
      sum = sum + 1
    }else{
      sum = sum
    }
    
  }
  if(sum == length(A1)){
    TRUE
  }else{
    FALSE
  }
  
}  

##############################################
# This function will count the occurence of each item
#####################################################
FreqCount = function(DataBase,AttNames,level){
  # is the dataset with first column to the attribute
  # AttNames is a vector containing the names of attrinute that
  # we want to do counting
  # Level is a vector containing the level of the attribute respectively
  sum = 0
  data = DataBase[,AttNames]
  if(length(AttNames)==1){
    for(j in 1:length(data)){
      for(k in 1:length(level)){
      if(SETEQUAL(data[j],level[k])==TRUE){
        sum = sum + 1
      }
      }
    }
    
  }else{
    for(j in 1:dim(data)[1]){
        if(SETEQUAL(data[j,],level)== TRUE){
          sum = sum + 1
        }
      
    }
    
  }
  sum
}
###############################################################################
# This is the power set function
powerset = function(s){
  len = length(s)
  l = vector(mode="list",length=2^len) ; l[[1]]=numeric()
  counter = 1L
  for(x in 1L:length(s)){
    for(subset in 1L:counter){
      counter=counter+1L
      l[[counter]] = c(l[[subset]],s[x])
    }
  }
  return(l)
}
###############################################################################
###############################################################################

# this function will tell us if a itemset is frequent or not.
GenerateFrequentItem = function(Attribute,Levels, FrequentItemSet){
  # FrequentItemSet is the set of frequent k-itemset with their levels, attribute and count.
  # k is the size of each element of Lk.
  # Min_con is the minimum count.
  

  Lklevels = FrequentItemSet[[1]]
  LkAtt = FrequentItemSet[[2]]
  Ck = FrequentItemSet[[3]]
  ## Lets find the cross product    
  #####################################
  
  k = length(FrequentItemSet[[1]][[1]])
  poweratt = powerset(Attribute)
  powerlevel = powerset(Levels)
  powerattkeep = list()
  powerlevelkeep = list()
  counterr = 0
  for(pp in 1:length(powerlevel)){
    if(length(powerlevel[[pp]])==k){
      counterr = counterr + 1
      powerattkeep[[counterr]] = poweratt[[pp]]
      powerlevelkeep[[counterr]] = powerlevel[[pp]]
    }
  }
  powerAttkeepfinal = list()
  powerlevelfinal = list()
  counter = 0
  for(h in 1:length(powerattkeep)){
    
    if(length(unique(powerattkeep[[h]])) == k){
      counter = counter + 1
      powerAttkeepfinal[[counter]] = powerattkeep[[h]]
      powerlevelfinal[[counter]] = powerlevelkeep[[h]]
    }
    
  }
 
  #powerlevelfinal = powerlevelkeep
  #powerAttkeepfinal = powerattkeep 
  sum = 0 # first sum 
  for(i in 1:length(powerlevelfinal)){
    for(j in 1:length(Lklevels)){
      if(SETEQUAL(powerlevelfinal[[i]], Lklevels[[j]])== TRUE){
        sum = sum + 1
      }
    }
  }
  if(sum == (length(powerAttkeepfinal))){
    message( " is a Frequent itemset")
    #return(TRUE)
    TRUE
  }else{
    message( " is not a Frequent itemset")
    #return(FALSE)
    FALSE
  }
  
  
}
################################################################################

AprioriGen = function(DataBase,FrequentItemSet,Min_con){
  # FrequentItemSet is a list where the first element of the list contains 
  # the levels of item and the second element contains the attributes
  # and the third contains their count
  # the output is a list containin set of size k + 1
  
  k = length(FrequentItemSet[[1]][[1]])
  
  ##########################################################################
      Lklevels = FrequentItemSet[[1]]
      LkAtt = FrequentItemSet[[2]]
      Ck = numeric() # count of the frequent item set
      AttList = list() # attribute list
      LevelList = list() # level list
      Index = 1
      for(i in 1:(length(Lklevels))){
        for(j in 1:length(Lklevels)){
          A1 = LkAtt[[i]]
          A2 = LkAtt[[j]]
            I1 = Lklevels[[i]]
            I2 = Lklevels[[j]]
            l = 1
            Check = "FALSE"
            while(l < length(I1)){
              if(I1[l]==I2[l]){
                Check = "TRUE"
              }else{
                Check = "FALSE"
              }
              l = l + 1
            }
            if(Check == "TRUE" & I1[length(I1)]< I2[length(I2)]){
              Levels = c(I1,I2[length(I1)])
              Attribute = c(A1,A2[length(I1)])
              
              if(GenerateFrequentItem(Attribute,Levels,FrequentItemSet) == TRUE 
                 & (FreqCount(DataBase,Attribute,Levels)/dim(DataBase)[1])>=Min_con){
                #message(c, " is a frequent item set ","\n")
                Ck[Index] = FreqCount(DataBase,Attribute,Levels)
                AttList[[Index]] = Attribute
                LevelList[[Index]] = Levels
                Index = Index + 1
              }
              
              
            }
          
        }
        
      }
      Output = list(LevelList, AttList, Ck)
      Output
    
  
}
#####################################################
Apriori = function(DataBase,Min_con){
  # DataBase is the data set that we use. this data set should have attribute names
  # Min_con is the minimum number of count fo a set of item to be frequent.
  # The output will be a list of all frequent item set.
  
  Att = names(DataBase) # attributes names
  counter = 0
  ItemList = numeric()
  AttList = numeric()
  Count = numeric()
  for(i in 1:length(Att)){
    for(j in levels(DataBase[,Att[i]])){
      if((FreqCount(DataBase, AttNames = Att[i],level =j)/dim(DataBase)[1])>=Min_con){
        counter = counter + 1
        ItemList[counter] = j
        AttList[counter] = Att[i]
        Count[counter] = FreqCount(DataBase, AttNames = Att[i],level =j)
      }
    }
  }
  if(length(ItemList)==0){
    message("There is no frequent item set for this Minimum support ", Min_con, ".", " Please reduce it")
  }else{
    target = order(ItemList)
    ItemList = ItemList[target]
    AttList = AttList[target]
    Count = Count[target]
    L1 = vector(mode="list",length=3)
    L1[[1]] = ItemList
    L1[[2]] = AttList
    L1[[3]] = Count
    Levels2 = list()
    Att2 = list()
    c2 = numeric()
    c = 0
    
    for(g in 1:(length(ItemList)-1)){
      for(r in (g+1):length(ItemList)){
        if(AttList[g] == AttList[r]){
          next
        }else{
          At = c(AttList[g],AttList[r])
          Le = c(ItemList[g],ItemList[r])
          rank = order(Le)
          At = At[rank]
          Le = Le[rank]
          if((FreqCount(DataBase,AttNames=At,
                        level=Le)/dim(DataBase)[1])>=Min_con & 
             length(Le)==2){
            c = c + 1
            Levels2[[c]] = Le
            Att2[[c]] = At
            c2[c] = FreqCount(DataBase,AttNames=At,
                              level=Le)
          }
        }
      }
    }
    L2 = list(Levels2,Att2,c2)
    
    c = list()
    ll = list()
    ll[[1]] = L1
    ll[[2]] = L2
    c[[1]] = L1
    c[[2]] = L2
    
    Count = 2
    Countll = 2
    # This is L2
    #c[[Count]] = AprioriGen(DataBase,L1,Min_con)
    while(length(c[[Count]][[1]])!=0){
      Freq = AprioriGen(DataBase,c[[Count]],Min_con)
      Count = Count + 1
      c[[Count]] = Freq
      
      if(length(Freq[[1]])!=0){
        Countll = Countll + 1
        ll[[Countll]] = Freq
      }
      
    }
    ll
    
  }
  
  
  
  
  
  # FirstItemSet is the 1-itemset
  # Let's generate Lk
 
  
}
AprioriAlgorithm = function(DataBase, Min_sup, Min_conf){
Min_con = Min_sup
Min_conf = Min_conf
L= Apriori(DataBase, Min_con)
RuleSum = 0
Con = 0

for(k in 1:length(L)){
  gen = L[[k]]
  if(length(gen[[1]])==0){
    break()
  }else{
    for(i in 1:length(gen[[1]])){
    if(length(gen[[2]][[i]])==1){
      message(paste(i,": "),paste("lhs","==>",gen[[2]][[i]], "=",gen[[1]][[i]]),paste(" "),paste("support","=", gen[[3]][[i]]))
      #RuleSum = RuleSum + 1
      for (l in 1:(length(gen[[1]])-1)) {
        for (t in (l+1):length(gen[[1]])) {
          attri = c(gen[[2]][[l]],gen[[2]][[t]])
          lev = c(gen[[1]][[l]],gen[[1]][[t]])
          a = FreqCount(DataBase,attri,lev)
          supp = a/dim(DataBase)[1]
          conf = a/FreqCount(DataBase,gen[[2]][l],gen[[1]][[l]])
          if(supp >= Min_con & conf>=Min_conf){
            message(paste(Con,": ") ,paste(gen[[2]][[l]],"=", paste(" "),gen[[1]][[l]]),paste(" "),paste(" " ," ==> "),
                    paste(gen[[2]][[t]], "=", gen[[1]][[t]]," support = ", a, "confidence = ", round(conf,2), sep = " "))
            RuleSum = RuleSum + 1
            Con = Con + 1
          }
          
        }
        
      }

    }
    if(length(gen[[2]][[i]])>=2){
      l = 1
      while(l<=length(gen[[2]][[i]])){
        a = FreqCount(DataBase,gen[[2]][[i]],gen[[1]][[i]])
        supp = (FreqCount(DataBase,gen[[2]][[i]],gen[[1]][[i]])/dim(DataBase)[1])
        conf = a/FreqCount(DataBase,gen[[2]][[i]][-l],gen[[1]][[i]][-l])
        if(supp >= Min_con & conf>=Min_conf){
          message(paste(Con,": ") ,paste(gen[[2]][[i]][-l],"=", paste(" "),gen[[1]][[i]][-l]),paste(" "),paste(" " ," ==> "),
                         paste(gen[[2]][[i]][l], "=", gen[[1]][[i]][l]," support = ", a, "confidence = ", round(conf,2), sep = " "))
          RuleSum = RuleSum + 1
          Con = Con + 1
      }
       l = l + 1 
      }
    }
  
    }
  }
}
message("The Number of Association Rule is: ", RuleSum)

}
system.time(AprioriAlgorithm(DataBase,Min_sup=0.9,Min_conf = 0.5))
system.time(AprioriAlgorithm(DataBase,Min_sup=0.4,Min_conf = 0.7))

################################################################################


