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
cnt=0
N=10000

#初期値
w=data()

f<-file("rnorm_data.txt","r")

for(cnt in 1:N){
    if(end==0){
        #初期値
        x=data()#N(0,1)の乱数
        
        y=w%*%x
        w_previous=w
        w=w_previous+r*y*(x-y*w_previous)
        cat("cnt==",cnt,":\n")
        cat(w,"\n")
        cat(w_previous,"\n")
        
        if((w-w_previous)%*%(w-w_previous)<epsilon){
            end=1
            cat("(w-w_previous)%*%(w-w_previous)<epsilon\n")
            cat(w,"\n")
            cat(w_previous)
        }else if(cnt==N){
            cat("cnt==1000\n")
            cat(w,"\n")
            cat(w_previous)
        }
    }
}

close(f)

