# Lu�s Fernando Emanuel Alves dos Santos, N�USP: 11281742, Turma 22.
# Leonardo Springmann Rossi, N�USP: 11189440, Turma 22.

## Provas:

Q<-0

x <- as.numeric(readline(" Digite a quantidade de alunos a serem avaliados: "))

for(i in 1:x){
  nome <- readline (" Digite o nome do aluno: ")
  
  
  P1<- as.numeric(readline(" Digite a nota do aluno na P1 - digite (-1) se n�o fez: "))
  if (P1!=-1) {
    P<-P1
    Q<- Q+1
  } else {
    P1<-0
  }
  
  P2<- as.numeric(readline(" Digite a nota do aluno na P2 - digite (-1) se n�o fez: "))
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
    
    Psub<- as.numeric(readline(" Digite a nota do aluno na Psub - digite (-1) se n�o fez: "))
    
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
    print (paste0(" Voc� tirou ", EP1, " no EP1!"))
    mediaEP<-(EP1*4)/20
  }else {
    print (" Voc� n�o fez o EP1! ")
    EP1<--1
  }
  
  EP2<- as.numeric(readline(" E ent�o, digite a nota do aluno (a) no EP2 : "))
  if (EP2>0 && EP2<10 && EP1==-1) {
    print (paste0(" Voc� tirou ", EP2, " no EP2!"))
    mediaEP<-(EP2*5)/20
    
  }else if ( EP2>0 && EP2<10 && EP1!=-1) {
    print (paste0(" Voc� tirou ", EP2, " no EP2!"))
    mediaEP<- (EP1*4+EP2*5)/20
    
  } else {
    print (" Voc� n�o fez o EP2?!")
    EP2<--1
  }
  
  EP3<- as.numeric(readline(" E por fim, a nota do aluno (a) no EP3 : "))
  if (EP3>0 && EP3<10 && EP1==-1 && EP2==-1) {
    print (paste0(" Voc� tirou ", EP3, " no EP3!"))
    mediaEP<-(EP3*11)/20
    
  }else if (EP3>0 && EP3<10 && EP1!=-1 && EP2!=-1){
    print (paste0(" Voc� tirou ", EP3, " no EP3!"))
    mediaEP<- (EP1*4+EP2*5+EP3*11)/20
    
  } else if (EP3>0 && EP3<10 && EP1!=-1) {
    print (paste0(" Voc� tirou ", EP3, " no EP3!"))
    mediaEP<- (EP1*4+EP3*11)/20
    
  } else if (EP3>0 && EP3<10 && EP2!=-1) {
    print (paste0(" Voc� tirou ", EP3, " no EP3!"))
    mediaEP<- (EP2*5+EP3*11)/20
    
  } else {
    print (" Me parece que voc� n�o fez o EP3...")
    EP3<--1
    
  }
  
  # C�lculo da m�dia final:
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
    print (paste0(" O aluno ", nome, " ficou com ", P, " de m�dia nas provas, ", mediaEP
                  , " de m�dia nos EP's e sua m�dia final foi: ", MF , ". Portanto, o aluno foi aprovado "))
  } else if (MF<5 && MF>=3) {
    print (paste0(" O aluno ", nome, " ficou com ", P, " de m�dia nas provas, ", mediaEP
                  , " de m�dia nos EP's e sua m�dia final foi: ", MF , ". Portanto, o aluno ficou de recupera��o "))
    
  } else if (MF<3) {
    print (paste0(" O aluno ", nome, " ficou com ", P, " de m�dia nas provas, ", mediaEP
                  , " de m�dia nos EP's e sua m�dia final foi: ", MF , ". Portanto, o aluno foi reprovado "))
  } 
  
  
  
} # T�rmino do la�o for!
  