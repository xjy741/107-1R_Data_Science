# 猜數字遊戲
# 基本功能
# 1. 請寫一個由"電腦隨機產生"不同數字的四位數(1A2B遊戲)
# 2. 玩家可"重覆"猜電腦所產生的數字，並提示猜測的結果(EX:1A2B)
# 3. 一旦猜對，系統可自動計算玩家猜測的次數

# 額外功能：每次玩家輸入完四個數字後，檢查玩家的輸入是否正確(錯誤檢查)

target = sample(c(1000:9999), 1)
target = toString(target)
target = strsplit(target, '')[[1]]
try_num=0
while(1){
  try_num=try_num+1
  a=0
  b=0
  but = 1
  while(but == 1){
    but = 0
    num <- readline(prompt = "Enter number: ")
    num = as.integer(num)
    if (!is.integer(num)){
      print("Not integer")
      but = 1
    }
    num = toString(num)
    num = strsplit(num, '')[[1]]
    if(!length(num) == 4){
      print("Not 4 digits")
      but=1
    }
  }
  for( i in 1:length(num)){
    tmpa=0
    tmpb=0
    for(j in 1:length(target)){
      if(num[i]==target[j]){
        if (i==j){
          tmpa = 1
          tmpb=0
          break
        }
        else{
          tmpb = 1
        }
      }
    }
    a = a+tmpa
    b = b+tmpb
  }
  if(a==4&&b==0) {
    print("good!")
    cat("you try ",try_num," times")
    break}
  cat(a,"A",b,"B",'\n')
  
}

