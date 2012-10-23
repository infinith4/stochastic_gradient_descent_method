getwd()
setwd("/home/th4/Dropbox/R/stochastic_gradient_descent_method")

N=1000
out <- file("rnorm_data.txt", "w") # 書き込みモードで開く
for(cnt in 1:N){
    x=rnorm(5)
    for(i in 1:length(x)){
        if (i < 5) writeLines(paste(x[i]),out,sep=", ")
        else if(i==5) writeLines(paste(x[i]),out,sep="\n")
    }
}
close(out)                        # ファイルを閉じる

( x <- read.table("rnorm_data.txt") )

#一行ずつ読みベクトル化

f<-file("rnorm_data.txt","r")
data=function(){
    a<-readLines(con=f,1)    
    b=unlist(strsplit(a, "\\,")) # 文字ピリオッド "," で分割
    return(as.numeric(b))
}
data()#ファイルの1行目が読み込まれる
data()#ファイルの2行目が読み込まれる

a<-readLines(con=f,1)
b=unlist(strsplit(a, "\\,")) # 文字ピリオッド "," で分割
as.numeric(b)

x=rnorm(5)
x




