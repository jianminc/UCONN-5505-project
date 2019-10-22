data_root = "/Users/jinjun/Desktop/UCONN-5505-project/code_data/data"
data_files = list.files(data_root)
pd_output = c()
for (i in data_files){
  PD = read.csv(paste(data_root,'/',i,sep=''))
  if (startsWith(i,'3ST')){
    type_ctl = 0
  } else if (startsWith(i,'4SG')){
    type_ctl = 1
  } else {
    type_ctl = 2
  }
  TYPE = rep(type_ctl,nrow(PD)) 
  PD_type = cbind(PD, TYPE)
  if (is.null(pd_output)){
    pd_output = PD_type
  } else{
    pd_output = rbind(pd_output,PD_type)
  }
}

pd_3st = pd_output[pd_output$TYPE==0,]
pd_4sg = pd_output[pd_output$TYPE==1,]
pd_4st = pd_output[pd_output$TYPE==2,]
