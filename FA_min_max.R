#  Author Nicola Bernabè ver 1.0 8/11/2021
# the implemented version of the FA refers to the following Journal:Firefly Algorithms for Multimodal Optimization
#author Xin-She Yang
controllo_intervallo<-function(x,y,ub,lb){
    #message(x," ",y," ",ub," ",lb)
    if(x>ub[1]){
        x<-ub[1]
    }
    if(x<lb[1]){
        x<-lb[1]
    }
    if(y>ub[2]){
        y<-ub[2]
    }
    if(y<lb[2]){
        y<-lb[2]
    }

    return (c(x,y))
}

#prendo 2 individui che saranno i genitori presi casualmente dalla popolazione
aggiornamento_posizione<-function(Sciame,MAX_DIMENSION_Sciame,particelle_1_2_indici,gamma_parametro,alpha,ub,lb,beta_0,BEST_FITNESS,b_0,MAX_UNKNOWNS,FUNZIONE_COSTO,min_max){
 
    #Sciame[3,particelle_1_2_indici[1]]<-FUNZIONE_COSTO(Sciame[1,particelle_1_2_indici[1]],Sciame[2,particelle_1_2_indici[1]])
    if(Sciame[3,particelle_1_2_indici[1]]>Sciame[3,particelle_1_2_indici[2]] && min_max==-1){
        x<-(Sciame[1,particelle_1_2_indici[2]]-Sciame[1,particelle_1_2_indici[1]])^2
        y<-(Sciame[1,particelle_1_2_indici[2]]-Sciame[2,particelle_1_2_indici[1]])^2
        r<-x+y#non faccio la radice perchè dopo devo fare un quadrato e quindi è inutile calcolare
        #beta_0<-1
        beta<-(beta_0-b_0)*exp(-gamma_parametro*r)+b_0
        #stemps<-alpha*(runif(1, 0,MAX_DIMENSION_Sciame,)-0.5)#*scale
        stemps_x<-alpha*(runif(1, 0,MAX_UNKNOWNS)-0.5)
        stemps_y<-alpha*(runif(1, 0,MAX_UNKNOWNS)-0.5)
        Sciame[1,particelle_1_2_indici[1]]<-(1-beta)*Sciame[1,particelle_1_2_indici[1]]+beta*Sciame[1,particelle_1_2_indici[2]]+stemps_x
        Sciame[2,particelle_1_2_indici[1]]<-(1-beta)*Sciame[2,particelle_1_2_indici[1]]+beta*Sciame[2,particelle_1_2_indici[2]]+stemps_y
        #message("prima beta ",beta," steps ",stemps," "," alpha ",alpha," ",Sciame[1,particelle_1_2_indici[1]]," ",Sciame[2,particelle_1_2_indici[1]]," ",Sciame[3,particelle_1_2_indici[1]])
        x1y1<-controllo_intervallo(Sciame[1,particelle_1_2_indici[1]],Sciame[2,particelle_1_2_indici[1]],ub,lb)
        Sciame[1,particelle_1_2_indici[1]]<-x1y1[1]
        Sciame[2,particelle_1_2_indici[1]]<-x1y1[2]
        Sciame[3,particelle_1_2_indici[1]]<-FUNZIONE_COSTO(x1y1[1],x1y1[2])
        #message("dopo beta ",beta," steps ",stemps," "," alpha ",alpha," ",Sciame[1,particelle_1_2_indici[1]]," ",Sciame[2,particelle_1_2_indici[1]]," ",Sciame[3,particelle_1_2_indici[1]])
    }else if(Sciame[3,particelle_1_2_indici[1]]<Sciame[3,particelle_1_2_indici[2]] && min_max==1){
        x<-(Sciame[1,particelle_1_2_indici[2]]-Sciame[1,particelle_1_2_indici[1]])^2
        y<-(Sciame[1,particelle_1_2_indici[2]]-Sciame[2,particelle_1_2_indici[1]])^2
        r<-x+y#non faccio la radice perchè dopo devo fare un quadrato e quindi è inutile calcolare
        #beta_0<-1
        beta<-(beta_0-b_0)*exp(-gamma_parametro*r)+b_0
        #stemps<-alpha*(runif(1, 0,MAX_DIMENSION_Sciame,)-0.5)#*scale
        stemps_x<-alpha*(runif(1, 0,MAX_UNKNOWNS)-0.5)
        stemps_y<-alpha*(runif(1, 0,MAX_UNKNOWNS)-0.5)
        Sciame[1,particelle_1_2_indici[1]]<-(1-beta)*Sciame[1,particelle_1_2_indici[1]]+beta*Sciame[1,particelle_1_2_indici[2]]+stemps_x
        Sciame[2,particelle_1_2_indici[1]]<-(1-beta)*Sciame[2,particelle_1_2_indici[1]]+beta*Sciame[2,particelle_1_2_indici[2]]+stemps_y
        #message("prima beta ",beta," steps ",stemps," "," alpha ",alpha," ",Sciame[1,particelle_1_2_indici[1]]," ",Sciame[2,particelle_1_2_indici[1]]," ",Sciame[3,particelle_1_2_indici[1]])
        x1y1<-controllo_intervallo(Sciame[1,particelle_1_2_indici[1]],Sciame[2,particelle_1_2_indici[1]],ub,lb)
        Sciame[1,particelle_1_2_indici[1]]<-x1y1[1]
        Sciame[2,particelle_1_2_indici[1]]<-x1y1[2]
        Sciame[3,particelle_1_2_indici[1]]<-FUNZIONE_COSTO(x1y1[1],x1y1[2])
    }
    
    
    


    
    
    #print(rand)
    return(Sciame)
}

#aggiorni migliore fitness
aggiorna_fitness<-function(BEST_FITNESS,Migliore_fitness_generazione){
            BEST_FITNESS[1]<-Migliore_fitness_generazione[1]
            BEST_FITNESS[2]<-Migliore_fitness_generazione[2]
            BEST_FITNESS[3]<-Migliore_fitness_generazione[3]
            #BEST_FITNESS[4]<-Migliore_fitness_generazione[4]
    return(BEST_FITNESS)


}

#scorro su tutti gli individui e cerco il fitneess migliore
controllo_migliore_fitness<-function(Sciame,min_max){
    BEST_FITNESS<-c(Sciame[1,1],Sciame[2,1],Sciame[3,1])
    for(i in 2:dim(Sciame)[2]) {
        #message("controllo best ",BEST_FITNESS[3]," ",Sciame[7,i])


        if(BEST_FITNESS[3]>Sciame[3,i] && min_max==-1){#ricordo che la riga 8 è fitness corrente
            BEST_FITNESS[1]<-Sciame[1,i]#x 
            BEST_FITNESS[2]<-Sciame[2,i]#y 
            BEST_FITNESS[3]<-Sciame[3,i]#fitess corrente

        }else if(BEST_FITNESS[3]<Sciame[3,i]  && min_max==1){
            BEST_FITNESS[1]<-Sciame[1,i]#x corrente
            BEST_FITNESS[2]<-Sciame[2,i]#y corrente
            BEST_FITNESS[3]<-Sciame[3,i]#fitess corrente
        }







       


    }
    return(c(BEST_FITNESS[1],BEST_FITNESS[2],BEST_FITNESS[3]))
}
#inizializzo la matrice
inizializzazione<-function(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,FUNZIONE_COSTO){
    Sciame<-matrix(1:1,nrow <- (MAX_UNKNOWNS)+1, ncol <- MAX_DIMENSION_Sciame)#oltre alla x e y ci aggiungo le 2 velocità, poi anche le x e y migliori della particella anche la fitness migliore della particella e quella corrente
    numero_casuale_x<-1
    numero_casuale_y <-1
    for(i in 1:dim(Sciame)[2]) {
        Sciame[1,i] <-runif(1, lb[1],ub[1] )#x
        Sciame[2,i] <-runif(1, lb[1],ub[1] )#y
        Sciame[3,i] <-FUNZIONE_COSTO(Sciame[1,i],Sciame[2,i])#fitness
        #Sciame[4,i] <-runif(1, 0,1 )#intensità luce
    }

    return (Sciame)
}
aggiorno_file_individui<-function(Sciame,MAX_DIMENSION_Sciame,generazione){
                individuo<-c()
                x<-c()
                y<-c()
                fitness<-c()
                for(i in 1:MAX_DIMENSION_Sciame) {
                    x<-c(x,Sciame[1,i])
                    y<-c(y,Sciame[2,i])
                    fitness<-c(fitness,Sciame[3,i])
                    
                    }
            df<-data.frame(x,y,fitness)
            #nome_file<-"generazione"+generazione+".cvs"
            nome_file = paste("FA Sciame generazione ",generazione,".txt",sep="")
            #write.csv(df, file=nome_file,sep=";",row.names=FALSE)
            write.table(df, file= nome_file,sep=";",row.names=FALSE)
            }

mainFA<-function(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,gamma_parametro,theta,alpha,max_alpha,alpha_min,alpha_decrescente,beta_0,b_0,FUNZIONE_COSTO){
   

    Sciame<-inizializzazione(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,FUNZIONE_COSTO)#inizializzo la popolazione
    BEST_FITNESS<-controllo_migliore_fitness(Sciame,min_max)
    FITNESS_grafico_migliore<-c()
    FITNESS_grafico_migliore_generazione<-c()
    #iterazioni calcolo per trovare il minimo
    indice_stazionarieta<-0#aumenta ad ogni generazione
    n_generazioni<-0
    for(i in 1:Generazioni){
        n_generazioni<-i
        if(alpha_decrescente==1){
            alpha<-alpha*theta
        }
        
        
        for(j_iterazioni_particelle1 in 1:MAX_DIMENSION_Sciame){#muovo tutte le particelle una volta
         for(j_interazioni_particelle2 in 1:MAX_DIMENSION_Sciame){
             particelle_1_2_indici<-c(j_iterazioni_particelle1,j_interazioni_particelle2)
             Sciame<-aggiornamento_posizione(Sciame,MAX_DIMENSION_Sciame,particelle_1_2_indici,gamma_parametro,alpha,ub,lb,beta_0,BEST_FITNESS,b_0,MAX_UNKNOWNS,FUNZIONE_COSTO,min_max)
         }
        }
        if(alpha<alpha_min && alpha_decrescente==1){
            alpha<-max_alpha
        }
        Migliore_fitness_generazione<-controllo_migliore_fitness(Sciame,min_max)
        #message(Migliore_fitness_generazione," alpha ",alpha)
        if(Migliore_fitness_generazione[3]<BEST_FITNESS[3]  && min_max==-1){#minimizzo
            BEST_FITNESS<-aggiorna_fitness(BEST_FITNESS,Migliore_fitness_generazione)
            indice_stazionarieta=0#ritorno a 0 la stazionarietà
            message("migliore soluzione-> x ",BEST_FITNESS[1]," y ",BEST_FITNESS[2]," fitness ",BEST_FITNESS[3])
        }else if(Migliore_fitness_generazione[3]>BEST_FITNESS[3]  && min_max==1){#minimizzo
            BEST_FITNESS<-aggiorna_fitness(BEST_FITNESS,Migliore_fitness_generazione)
            indice_stazionarieta=0#ritorno a 0 la stazionarietà
            message("migliore soluzione-> x ",BEST_FITNESS[1]," y ",BEST_FITNESS[2]," fitness ",BEST_FITNESS[3])
        }
        FITNESS_grafico_migliore<-c(FITNESS_grafico_migliore,BEST_FITNESS[3])
        FITNESS_grafico_migliore_generazione<-c(FITNESS_grafico_migliore_generazione,Migliore_fitness_generazione[3])

        if(indice_stazionarieta>=STAZIONARIETA){
            message("troppo stazionaria ","generazione-> ",i," stazionario->",indice_stazionarieta)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }else{
            indice_stazionarieta=indice_stazionarieta+1
        }
        if((i%%stampo_generazione)==0){#stampo così so che sta facendo qualcosa
            message("generazione-> ",i," stazionario->",indice_stazionarieta)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
        }
        if(BEST_FITNESS[3]<precisione && min_max==-1){
            message("finito algoritmo FA, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }else if(BEST_FITNESS[3]>precisione && min_max==1){
            message("finito algoritmo FA, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }
    }
    windows()
    par(mfrow=c(1,2))
    plot(FITNESS_grafico_migliore, type="l", col="red",lwd=10)
    #lines(FITNESS_grafico_migliore_generazione, type="l", col="blue",lty=5)
    title(main="migliore e andamento fitness per generazione", col.main="blue", font.main="4")
    
    x_particelle<-c()
    y_particelle<-c()
    for(x_y in 1:MAX_DIMENSION_Sciame){
        x_particelle<-c(x_particelle,Sciame[1,x_y])
        y_particelle<-c(y_particelle,Sciame[2,x_y])
    }

    plot(x_particelle, y_particelle, xlab="x",ylab="y", main="dove si trovano gli agenti") 
    #print(Sciame)
    
    #print(FUNZIONE_COSTO(512,404.2319))
    #print(FUNZIONE_COSTO(526.891824703338,-600))
    BEST_FITNESS<-list(BEST_FITNESS,"BAT",n_generazioni)
    return (BEST_FITNESS)

}





#riesco a trovare la soluzione con questi intervalli 
#miliardo
#ub<-c(1000000000,1000000000)  # upper bound of the unknowns
#lb<-c(-1000000000,-1000000000)  #lower bound of the unknowns

#milione
#ub<-c(1000000,1000000)  # upper bound of the unknowns
#lb<-c(-1000000,-1000000)  #lower bound of the unknowns

#ub<-c(10000,10000)  # upper bound of the unknowns
#lb<-c(-10000,-10000)  #lower bound of the unknowns

ub<-c(100,100)  # upper bound of the unknowns
lb<-c(-100,-100)  #lower bound of the unknowns

#ub<-c(512,512)  
#lb<-c(-512,-512)
MAX_UNKNOWNS<-2  #numero variabili
MAX_DIMENSION_Sciame<-100#dimensione popolazione
Generazioni<-500000
STAZIONARIETA<-5000
min_max<--1#-1 minimizzo,1 massimizzo
stampo_generazione<-STAZIONARIETA/2#quando creare file degli individui
#massimizzo
#precisione<-11.999999#Parabolic Function
#precisione<-79.999#rastrigin function 
#precisione<-414.23847913128#Parabolic Function

#minimizzo
precisione<-0.001#
#precisione<--0.999999#Easom function
#precisione<-0.2925789999#Schaffer function N. 4 f(0,+-1.25313)=0.292579
#precisione<--0.999999#per funzioni con minimo negativo
#precisione<--959.6406#Eggholder function   f(512,404.2319)=-959.6407   i domini x e y   ub<-c(512,512)  lb<-c(-512,-512)

gamma_parametro<-1#(da 0.01 a 100 o 1/sqrt(ub[1]) anche 1 va bene)
theta<-0.95#da 0.95 a 0.97  Randomness reduction factor theta=10^(-5/tMax) 
max_alpha<-1
alpha_decrescente<-1#0 costante,1 decrescente
alpha<-0.2#si potrebbe anche fare costante 0.2 ho visto che funziona bene lo stesso
alpha_min<-0.05
beta_0<-1
b_0<-1#numero positivo intero che determina l' attattività con r=0 in genere va bene 1



FUNZIONE_COSTO<-function(x,y){
    z<-(100*(y-(x)^2)^2)+(1-x)^2#FUNZIONE_COSTO banana  f(1,1)=0
    #z<-((1-x)^2)+100*((y-x^2))^2#Rosenbrock function constrained with a cubic and a line  sol f(1,1)=0
    #z<-(1.5-x+x*y)^2+(2.25-x+x*y^2)^2+(2.625-x+x*y^3)^2#Beale function f(3,0.5)=0
    #z<-0.26*(x^2+y^2)-0.48*x*y#Matyas function f(0,0)=0
    #z<-(sin(3*pi*x))^2+(x-1)^2*(1+sin(3*pi*y))+(y-1)^2*(1+(sin(2*pi*y))^2)#Lévi function N.13  f(1,1)=0
    #z<-0.5+((sin(x^2-y^2))^2-0.5)/(1+0.001*(x^2+y^2))^2#Schaffer function N. 2  f(0,0)=0
    #z<-2*x^2-1.05*x^4+(x^6/6)+x*y+y^2#THREE-HUMP CAMEL FUNCTION xi  [-5, 5]  f(0,0)=0
    #z<-100*sqrt(abs(y-0.01*x^2))+0.01*abs(x+10)#Bukin function N.6 f(-10,1)=0
    #z<--20*exp(-0.2*sqrt((1/2)*(x*x+y*y)))-exp(1/2*((cos(2*pi*x)+cos(2*pi*y))))+20+exp(1)#ACKLEY FUNCTION xi  [-32.768, 32.768]  f(0,0)=0


    #z<-0.5+(((cos(sin(abs(x^2-y^2))))^2 - 0.5)/((1 + 0.001*(x^2+y^2))^2))#Schaffer function N. 4 f(0,+-1.25313)=0.292579

    #con minimo negativo
    #z<--cos(x)*cos(y)*exp(-((x-pi)^2+(y-pi)^2))#Easom function f(pi,pi)=-1  mettere precisione -0.999999
    
    #z<--(y+47)*sin(sqrt(abs(x/2+(y+47))))-x*sin(sqrt(abs(x-(y+47))))#Eggholder function   f(512,404.2319)=-959.6407    
    #precisione<--959.6406999
    #ub<-c(512,512)  
    #lb<-c(-512,-512)  importante il dominio di x e y


    #massimizzare
    #z<-12-(x^2+y^2)/100 #Parabolic Function  f(0,0)=12 non so ma il massimo è 12
    #z<-80-(20+x^2+y^2-10*(cos(2*pi*x)+cos(2*pi*y)))#rastrigin function f(0,0)=80
    #z<-275-((x^4-16*x^2+5*x*0.5)+(y^4-16*y^2+5*y*0.5)+3)#Styblinski Function f(-2.8667,-2.8667)=414.23847913
    return (z)
}
soluzione<-mainFA(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,gamma_parametro,theta,alpha,max_alpha,alpha_min,alpha_decrescente,beta_0,b_0,FUNZIONE_COSTO)
#print(soluzione)


algoritmo<-c(soluzione[[2]][1])
x<-c(soluzione[[1]][1])
y<-c(soluzione[[1]][2])
fitness<-c(soluzione[[1]][3])
generazioni_soluzione<-c(soluzione[[3]][1])
df<-data.frame(algoritmo,x,y,fitness,generazioni_soluzione)
write.table(df, file="soluzione FA.txt",sep=";",row.names=FALSE)