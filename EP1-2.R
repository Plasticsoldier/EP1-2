# Luís Fernando Emanuel Alves dos Santos, NºUSP: 11281742, Turma 22.
# Leonardo Springmann Rossi, NºUSP: 11189440, Turma 22.

## Provas:

Q<-0

x <- as.numeric(readline(" Digite a quantidade de alunos a serem avaliados: "))

for(i in 1:x){
  nome <- readline (" Digite o nome do aluno: ")
  
  
  P1<- as.numeric(readline(" Digite a nota do aluno na P1 - digite (-1) se não fez: "))
  if (P1!=-1) {
    P<-P1
    Q<- Q+1
  } else {
    P1<-0
  }
  
  P2<- as.numeric(readline(" Digite a nota do aluno na P2 - digite (-1) se não fez: "))
  if (P2!=-1 && P1!=0) {
    P<-(P1+P2)/2
    Q<- Q+1
  } else if (P2!=-1) {
    P<-P2
    Q<-Q+1
    
  } else {
    P<-P1
  }
  
  if (Q<2) {
    
    Psub<- as.numeric(readline(" Digite a nota do aluno na Psub - digite (-1) se não fez: "))
    
  }
  
  
  if (Q==0 && Psub!=-1) {
    P<-Psub
  } else if (Q==0 && Psub==-1) {
    P<-0
  }
  
  if (Q<2 && Q>0 && P1!=0 && Psub!=-1) {
    P<-(P1+Psub)/2
  } else if (Q<2 && Q>0 && P2!=-1 && Psub!=-1) {
    P<-(P2+Psub)/2
  }
  
  if (Psub==-1 && P1!=0 && P2==-1 ) {
    P<-P1/2
  } else if (Psub==-1 && P2!=-1 && P1==0)
    P<- P2/2
  
  Q<-0
  
  
  
  ##EP's:
  
  EP1<- as.numeric(readline(" Quero saber agora do seu EP1: "))
  if (EP1>0 && EP1<10) {
    print (paste0(" Você tirou ", EP1, " no EP1!"))
    mediaEP<-(EP1*4)/20
  }else {
    print (" Você não fez o EP1! ")
    EP1<--1
  }
  
  EP2<- as.numeric(readline(" E então, digite a nota do aluno (a) no EP2 : "))
  if (EP2>0 && EP2<10 && EP1==-1) {
    print (paste0(" Você tirou ", EP2, " no EP2!"))
    mediaEP<-(EP2*5)/20
    
  }else if ( EP2>0 && EP2<10 && EP1!=-1) {
    print (paste0(" Você tirou ", EP2, " no EP2!"))
    mediaEP<- (EP1*4+EP2*5)/20
    
  } else {
    print (" Você não fez o EP2?!")
    EP2<--1
  }
  
  EP3<- as.numeric(readline(" E por fim, a nota do aluno (a) no EP3 : "))
  if (EP3>0 && EP3<10 && EP1==-1 && EP2==-1) {
    print (paste0(" Você tirou ", EP3, " no EP3!"))
    mediaEP<-(EP3*11)/20
    
  }else if (EP3>0 && EP3<10 && EP1!=-1 && EP2!=-1){
    print (paste0(" Você tirou ", EP3, " no EP3!"))
    mediaEP<- (EP1*4+EP2*5+EP3*11)/20
    
  } else if (EP3>0 && EP3<10 && EP1!=-1) {
    print (paste0(" Você tirou ", EP3, " no EP3!"))
    mediaEP<- (EP1*4+EP3*11)/20
    
  } else if (EP3>0 && EP3<10 && EP2!=-1) {
    print (paste0(" Você tirou ", EP3, " no EP3!"))
    mediaEP<- (EP2*5+EP3*11)/20
    
  } else {
    print (" Me parece que você não fez o EP3...")
    EP3<--1
    
  }
  
  # Cálculo da média final:
  MF<-0
  
  if (P>=5 && mediaEP>=5){
    MF<- ((3*P)+mediaEP)/4
    
  } else if (P<mediaEP) {
    MF<-P
    
  } else if (P>mediaEP) {
    MF<-mediaEP
    
  }
  
  P<-signif(P, digits=2)
  MF<- signif(MF, digits=2)
  mediaEP<-signif(mediaEP, digits=2)
  
  if (MF>=5) {
    print (paste0(" O aluno ", nome, " ficou com ", P, " de média nas provas, ", mediaEP
                  , " de média nos EP's e sua média final foi: ", MF , ". Portanto, o aluno foi aprovado "))
  } else if (MF<5 && MF>=3) {
    print (paste0(" O aluno ", nome, " ficou com ", P, " de média nas provas, ", mediaEP
                  , " de média nos EP's e sua média final foi: ", MF , ". Portanto, o aluno ficou de recuperação "))
    
  } else if (MF<3) {
    print (paste0(" O aluno ", nome, " ficou com ", P, " de média nas provas, ", mediaEP
                  , " de média nos EP's e sua média final foi: ", MF , ". Portanto, o aluno foi reprovado "))
  } 
  
  
  
} # Término do laço for!
  