exam=read.csv("D:/ISI Tezpur/Class/Class Notes/Module After Mid_1st Sem/Regression/Exam/exam.csv")

df=as.data.frame(exam)

View(df)

colnames(df)

Subset_exam=subset(df,select = c("work.hour..daily."))

unitary=c()
for (i in 1:32)
{
  unitary[i]=1
}

#importing dummy variable for gender
j=1
G1i=c()  
for (i in df$Gender)
{
  if (i=="Female")
  {
    G1i[j]=1  
  }else
  {
    G1i[j]=0
  }
  j=j+1
}

#importing dummy variable for Experinced
j=1
E1i=c()  
for (i in df$experinced)
{
  if (i=="yes")
  {
    E1i[j]=1  
  }else
  {
    E1i[j]=0
  }
  j=j+1
}

X=cbind(unitary,Subset_exam,G1i,E1i)

Y=as.matrix(df[,2])

X_prime=t(X)

X_M=as.matrix(X)

X_prime_M=as.matrix(X_prime)

X_prime_X=X_prime_M%*%X_M

library(MASS)

XPrime_X_inv=ginv(X_prime_X)
XPrime_Y=X_prime%*%Y
b_pred=XPrime_X_inv%*%XPrime_Y #b predicted is calculated

Y_pred=X_M%*%b_pred
U_pred=Y-Y_pred
RSS=t(U_pred)%*%U_pred

Sigma_hat=(RSS/28)
var_cov_mat=Sigma_hat[1]*XPrime_X_inv #Var-Cov Matrix

#BP Test
#Step1
Zi=as.matrix(U_pred^2)

#Step2
alpha_pred=XPrime_X_inv%*%(X_prime_M%*%Zi)

Zi_pred=X_M%*%alpha_pred

ei=Zi-Zi_pred

URSS=sum(ei^2)

#Step3
#After restriction

alpha_pred_restic=ginv((t(unitary)%*%unitary))%*%(t(unitary)%*%Zi)

ei_restric= Zi-c(alpha_pred_restic)*c(unitary)

RRSS=sum(ei_restric^2)

#step4
n=32
F_Stats=((RRSS-URSS)*(n-4))/(3*URSS)

#F tabulated 2.95

#CONCLUSION: WE CANNOT REJECT NULL HYPOTHESIS.THAT IS THERE IS NO HETEROSKEDASTICITY
#THAT IS THE MODEL WE STARTED WITH IS CORRECTLY SPECIFIED.


# H0:b4=0
# H1:b4>0

#Test Statistics:
T_calculated=b_pred[4]/((var_cov_mat[4,4])^0.5)

#T tab=1.701
T_Tab=qt(.95,28)

#T_calculated> T_tabulated

#we reject the null hypothesis that .
#Therefore experienced female have more salary.



