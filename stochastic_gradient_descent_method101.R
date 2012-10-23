#確率的最急勾配法
f<-file("rnorm_data.txt","r")
data=function(){
    a<-readLines(con=f,1)    
    b=unlist(strsplit(a, "\\,")) # 文字ピリオッド "," で分割
    return(as.numeric(b))
}
#data()#ファイルの1行目が読み込まれる
#data()#ファイルの2行目が読み込まれる

r=0.01
epsilon=0.00001
end=0
end_list=list(0,0,0,0,0)
end_cnt_list=list(0,0,0,0,0)

cnt=0
N=1000
#初期
w1=data()
w2=data()
#w2=rnorm(5)
w3=data()
#w3=rnorm(5)
w4=data()
#w4=rnorm(5)
w5=data()
#w5=rnorm(5)
w_list=list(w1,w2,w3,w4,w5)
w_list

y1=numeric(5);y2=numeric(5);y3=numeric(5);y4=numeric(5);y5=numeric(5)
y_list=list(y1,y2,y3,y4,y5)

numeric=numeric(5)
w_previous_list=list(numeric,numeric,numeric,numeric,numeric)
w_previous_list

w_conv_list=list(numeric,numeric,numeric,numeric,numeric)#wが収束した時のリスト

f<-file("rnorm_data.txt","r")

for(cnt in 1:N){
    #初期値
    x=data()#N(0,1)の乱数
    for(i in 1:5){
        if(end_list[[i]]==0){#収束していないwについて処理する
            y_list[[i]]=w_list[[i]]%*%x
            w_previous_list[[i]]=w_list[[i]]
            orthogonal=numeric(5)
            if(i>=2){
                for(k in 1:5){
                    if(end_list[[k]]==1){#収束した時(end_list[[k]]==1)収束したものを使う
                        w_conv=w_conv_list[[k]]
                        for(j in 1:(i-1)){#
                            y_list[[j]]=w_conv%*%x
                            orthogonal=orthogonal+y_list[[j]]*w_conv
                        }
                    }else{
                        for(j in 1:(i-1)){#
                            y_list[[j]]=w_list[[j]]%*%x
                            orthogonal=orthogonal+y_list[[j]]*w_list[[j]]
                        }
                    }
                }    
            }
            w_list[[i]]=w_previous_list[[i]]+r*y_list[[i]]*(x-y_list[[i]]*w_previous_list[[i]]-2*orthogonal)
            cat("i==",i,"::")
            cat("cnt==",cnt,":\n")
            cat(w_list[[i]],"\n")
            cat(w_previous_list[[i]],"\n\n")
            if((w_list[[i]]-w_previous_list[[i]])%*%(w_list[[i]]-w_previous_list[[i]])<epsilon){
                w_conv_list[[i]]=w_list[[i]]
                end_list[[i]]=1
                end_cnt_list[[i]]=cnt
                cat("w_",i,"is convergence.(cnt=",cnt,")\n")
                cat("(w_list[[i]]-w_previous_list[[i]])%*%(w_list[[i]]-w_previous_list[[i]])<epsilon\n")
                cat((w_list[[i]]-w_previous_list[[i]])%*%(w_list[[i]]-w_previous_list[[i]]),"\n\n")
                cat(w_list[[i]],"\n")
                cat(w_previous_list[[i]],"\n\n")
            }else if(cnt==N){
                cat("Not Covergence\n")
                cat("cnt==",N,"\n")
                cat(w_list[[i]],"\n")
                cat(w_previous_list[[i]])
            }
        }
    }
    
}

end_list
end_cnt_list


close(f)
