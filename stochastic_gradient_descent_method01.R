#確率的最急勾配法

r=0.1
epsilon=0.00001
end=0
cnt=0
N=10000

#初期値
w=rnorm(5)

for(cnt in 1:N){
    
    #初期値
    x=rnorm(5)#N(0,1)の乱数
    
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
        break
    }else if(cnt==N){
        cat("cnt==1000\n")
        cat(w,"\n")
        cat(w_previous)
        break
    }
    
}






#初期値
x=rnorm(5)#N(0,1)の乱数
w=rnorm(5)


w=c(1,2,3)
x=c(1,2,3)
y=w%*%x
y
w_previous=c(1,2,3)
w=w_previous+r*y*(x-y*w_previous)
w
1+0.1*14*(1-14)
2+0.1*14*(2-14*2)
3+0.1*14*(3-14*3)




for(cnt in 1:N){
cat(cnt,"\n")
    }