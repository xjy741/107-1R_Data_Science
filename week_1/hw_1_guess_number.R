# �q�Ʀr�C��
# �򥻥\��
# 1. �мg�@�ӥ�"�q���H������"���P�Ʀr���|���(1A2B�C��)
# 2. ���a�i"����"�q�q���Ҳ��ͪ��Ʀr�A�ô��ܲq�������G(EX:1A2B)
# 3. �@���q��A�t�Υi�۰ʭp�⪱�a�q��������

# �B�~�\��G�C�����a��J���|�ӼƦr��A�ˬd���a����J�O�_���T(���~�ˬd)

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
