#  Author Nicola Bernabè ver 1.0 8/11/2021
# the implemented version of the PSO refers to the following Journal: Particle swarm optimization  
#authors Riccardo Poli · James Kennedy · Tim Blackwell 
#

#prendo 2 individui che saranno i genitori presi casualmente dalla popolazione
aggiornamento_posizione<-function(Sciame,MAX_DIMENSION_Sciame,j_iterazioni_particelle,C_1,C_2,W,ub,lb,BEST_FITNESS,min_max,FUNZIONE_COSTO){
    
    Sciame[3,j_iterazioni_particelle]<-W*Sciame[3,j_iterazioni_particelle]+C_1*runif(1,  0,1)*(BEST_FITNESS[1]-Sciame[1,j_iterazioni_particelle])+C_2*runif(1,  0,1)*(Sciame[5,j_iterazioni_particelle]-Sciame[1,j_iterazioni_particelle])#vx
    Sciame[4,j_iterazioni_particelle]<-W*Sciame[4,j_iterazioni_particelle]+C_1*runif(1,  0,1)*(BEST_FITNESS[2]-Sciame[2,j_iterazioni_particelle])+C_2*runif(1,  0,1)*(Sciame[6,j_iterazioni_particelle]-Sciame[2,j_iterazioni_particelle])#vy
    Sciame[1,j_iterazioni_particelle]<-Sciame[1,j_iterazioni_particelle]+Sciame[3,j_iterazioni_particelle]
    Sciame[2,j_iterazioni_particelle]<-Sciame[2,j_iterazioni_particelle]+Sciame[4,j_iterazioni_particelle]
    
    if(Sciame[1,j_iterazioni_particelle]>ub[1]){
        Sciame[1,j_iterazioni_particelle]<-ub[1]
    }
    if(Sciame[1,j_iterazioni_particelle]<lb[1]){
        Sciame[1,j_iterazioni_particelle]<-lb[1]
    }
    if(Sciame[2,j_iterazioni_particelle]>ub[2]){
        Sciame[2,j_iterazioni_particelle]<-ub[2]
    }
    if(Sciame[2,j_iterazioni_particelle]<lb[2]){
        Sciame[2,j_iterazioni_particelle]<-lb[2]
    }
    fitness<-FUNZIONE_COSTO(Sciame[1,j_iterazioni_particelle],Sciame[2,j_iterazioni_particelle])
    if(fitness<Sciame[7,j_iterazioni_particelle] && min_max==-1){#minimizzare
        Sciame[7,j_iterazioni_particelle]<-fitness
        Sciame[5,j_iterazioni_particelle]<- Sciame[1,j_iterazioni_particelle]
        Sciame[6,j_iterazioni_particelle]<- Sciame[2,j_iterazioni_particelle]
    }else if(fitness>Sciame[7,j_iterazioni_particelle] && min_max==1){#massimizzare
        Sciame[7,j_iterazioni_particelle]<-fitness
        Sciame[5,j_iterazioni_particelle]<- Sciame[1,j_iterazioni_particelle]
        Sciame[6,j_iterazioni_particelle]<- Sciame[2,j_iterazioni_particelle]
    }
    Sciame[8,j_iterazioni_particelle]<-fitness

    return(Sciame)
}

#aggiorni migliore fitness
aggiorna_fitness<-function(BEST_FITNESS,Migliore_fitness_generazione){
            BEST_FITNESS[1]<-Migliore_fitness_generazione[1]
            BEST_FITNESS[2]<-Migliore_fitness_generazione[2]
            BEST_FITNESS[3]<-Migliore_fitness_generazione[3]
    return(BEST_FITNESS)


}

#scorro su tutti gli individui e cerco il fitneess migliore
controllo_migliore_fitness<-function(Sciame,min_max){
    BEST_FITNESS<-c(Sciame[1,1],Sciame[2,1],Sciame[7,1])
    for(i in 2:dim(Sciame)[2]) {
        #message("controllo best ",BEST_FITNESS[3]," ",Sciame[7,i])
        if(BEST_FITNESS[3]>Sciame[8,i] && min_max==-1){#ricordo che la riga 8 è fitness corrente
            BEST_FITNESS[1]<-Sciame[1,i]#x corrente
            BEST_FITNESS[2]<-Sciame[2,i]#y corrente
            BEST_FITNESS[3]<-Sciame[8,i]#fitess corrente

        }else if(BEST_FITNESS[3]<Sciame[8,i]  && min_max==1){
            BEST_FITNESS[1]<-Sciame[1,i]#x corrente
            BEST_FITNESS[2]<-Sciame[2,i]#y corrente
            BEST_FITNESS[3]<-Sciame[8,i]#fitess corrente


        }


    }
    return(c(BEST_FITNESS[1],BEST_FITNESS[2],BEST_FITNESS[3]))
}






#inizializzo la matrice
inizializzazione<-function(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,W,FUNZIONE_COSTO){
    Sciame<-matrix(1:1,nrow <- (MAX_UNKNOWNS*3)+2, ncol <- MAX_DIMENSION_Sciame)#oltre alla x e y ci aggiungo le 2 velocità, poi anche le x e y migliori della particella anche la fitness migliore della particella e quella corrente
    numero_casuale_x<-1
    numero_casuale_y <-1
    for(i in 1:dim(Sciame)[2]) {
        numero_casuale_x <- runif(1, lb[1],ub[1] )
        numero_casuale_y <- runif(1,  lb[2],ub[2])
        Sciame[1,i] <-numero_casuale_x
        Sciame[2,i] <-numero_casuale_y
        Sciame[3,i]<-W
        Sciame[4,i]<-W
        Sciame[5,i]<-Sciame[1,i]#migliore della particella x
        Sciame[6,i]<-Sciame[2,i]#migliore della particella y
        Sciame[7,i] <-FUNZIONE_COSTO(Sciame[1,i],Sciame[2,i])#usero solo per una volta questa fitness per calcolare la velocità sia per soluzione relativa migliore alla particella,sia per soluzione globale
        Sciame[8,i] <-FUNZIONE_COSTO(Sciame[1,i],Sciame[2,i])#fitness corrente
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
            nome_file = paste("PSO Sciaame generazione ",generazione,".txt",sep="")
            #write.csv(df, file=nome_file,sep=";",row.names=FALSE)
            write.table(df, file= nome_file,sep=";",row.names=FALSE)
            }

mainPSO<-function(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,C_1,C_2,W,FUNZIONE_COSTO){
    

    
    Sciame<-inizializzazione(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,W,FUNZIONE_COSTO)#inizializzo la popolazione
    BEST_FITNESS<-controllo_migliore_fitness(Sciame,min_max)
    FITNESS_grafico_migliore<-c()
    FITNESS_grafico_migliore_generazione<-c()
    #iterazioni calcolo per trovare il minimo
    indice_stazionarieta<-0#aumenta ad ogni generazione
    n_generazioni<-0

 

    for(i in 1:Generazioni){
        n_generazioni<-i
        for(j_iterazioni_particelle in 1:MAX_DIMENSION_Sciame){#muovo tutte le particelle una volta
            Sciame<-aggiornamento_posizione(Sciame,MAX_DIMENSION_Sciame,j_iterazioni_particelle,C_1,C_2,W,ub,lb,BEST_FITNESS,min_max,FUNZIONE_COSTO)
        }
        Migliore_fitness_generazione<-controllo_migliore_fitness(Sciame,min_max)
        
        #print(Migliore_fitness_generazione)
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
            message("finito algoritmo PSO, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }else if(BEST_FITNESS[3]>precisione && min_max==1){
            message("finito algoritmo PSO, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }
    }
    windows()
    par(mfrow=c(1,2))
    plot(FITNESS_grafico_migliore, type="l", col="red",lwd=10)
    #lines(FITNESS_grafico_migliore_generazione, type="l", col="blue",lty=5)
    #title(main="migliore e andamento fitness per generazione", col.main="blue", font.main="4")
    
    x_particelle<-c()
    y_particelle<-c()
    for(x_y in 1:MAX_DIMENSION_Sciame){
        x_particelle<-c(x_particelle,Sciame[5,x_y])
        y_particelle<-c(y_particelle,Sciame[6,x_y])

    }

    plot(x_particelle, y_particelle, xlab="x",ylab="y", main="dove si trovano gli agenti") 

BEST_FITNESS<-list(BEST_FITNESS,"PSO",n_generazioni)
 return(BEST_FITNESS)
}


#riesco a trovare la soluzione con questi intervalli 
#ub<-c(1000000000,1000000000)  # upper bound of the unknowns
#lb<-c(-1000000000,-1000000000)  #lower bound of the unknowns
#W=0.2

#milione
#ub<-c(1000000,1000000)  # upper bound of the unknowns
#lb<-c(-1000000,-1000000)  #lower bound of the unknowns
#W=0.3

#ub<-c(10000,10000)  # upper bound of the unknowns
#lb<-c(-10000,-10000)  #lower bound of the unknowns
#W=0.1

#ub<-c(100,100)  # upper bound of the unknowns
#lb<-c(-100,-100)  #lower bound of the unknowns
ub<-c(512,512)  
lb<-c(-512,-512)
#ub<-c(5,5)  # upper bound of the unknowns
#lb<-c(-5,-5)  #lower bound of the unknowns
#ub<-c(20,20)  # upper bound of the unknowns
#lb<-c(-20,-20)  #lower bound of the unknowns
W=0.3#0.02 o 0.3
MAX_UNKNOWNS<-2  #numero variabili
MAX_DIMENSION_Sciame<-100#dimensione popolazione
Generazioni<-1000000
STAZIONARIETA<-50000
min_max<--1#-1 minimizzo,1 massimizzo
stampo_generazione<-STAZIONARIETA/2#quando creare file degli individui
#massimizzo
#precisione<-11.99999999#Parabolic Function
#precisione<-79.9999#rastrigin function 
#precisione<-415.2384791312#Parabolic Function

#minimizzo
precisione<-0.001#
#precisione<--0.999999#Easom function
#precisione<-0.2925789999#Schaffer function N. 4 f(0,+-1.25313)=0.292579
#precisione<--0.999999#per funzioni con minimo negativo
#precisione<--959.6406#Eggholder function   f(512,404.2319)=-959.6407   i domini x e y   ub<-c(512,512)  lb<-c(-512,-512)
#precisione<--2.06261 #CROSS-IN-TRAY FUNCTION
C_1<-2#forza attrazione cognitiva
C_2<-2#forza attrazione sociale


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
    #z<--0.0001*(abs((sin(x)*sin(y)*exp(abs(100 - sqrt(x^2+y^2)/pi))))+1)^0.1#CROSS-IN-TRAY FUNCTION  xi  [-10, 10]  f(1.3491,-1.3491 | 1.3491,1.3491 | -1.3491,1.3491)=-2.06261
    #z<--cos(x)*cos(y)*exp(-((x-pi)^2+(y-pi)^2))#Easom function f(pi,pi)=-1  mettere precisione -0.999999
    
    #z<--(y+47)*sin(sqrt(abs(x/2+(y+47))))-x*sin(sqrt(abs(x-(y+47))))#Eggholder function   f(512,404.2319)=-959.6407    
    #precisione<--959.6406999
    #ub<-c(512,512)  
    #lb<-c(-512,-512)


    #massimizzare
    #z<-12-(x^2+y^2)/100 #Parabolic Function  f(0,0)=12 non so ma il massimo è 12
    #z<-80-(20+x^2+y^2-10*(cos(2*pi*x)+cos(2*pi*y)))#rastrigin function f(0,0)=80
    #z<-275-((x^4-16*x^2+5*x*0.5)+(y^4-16*y^2+5*y*0.5)+3)#Styblinski Function f(-2.8667,-2.8667)=414.23847913
    return (z)
}

soluzione<-mainPSO(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,C_1,C_2,W,FUNZIONE_COSTO)
#print(soluzione)
#W<-0
#x<-c()
#y<-c()
#fitness<-c()
#w_critico<-c()
#generazioni_soluzione<-c()
#for(i in 1:50){
#    W<-W+0.01
#    message(i," W ",W)
#    soluzione<-mainPSO(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,C_1,C_2,W,FUNZIONE_COSTO)
#    w_critico<-c(w_critico,W)
#    x<-c(x,soluzione[[1]][1])
#    y<-c(y,soluzione[[1]][2])
#    fitness<-c(fitness,soluzione[[1]][3])
#    generazioni_soluzione<-c(generazioni_soluzione,soluzione[[3]][1])
    

#}
#df<-data.frame(x,y,fitness,generazioni_soluzione,w_critico)
#write.table(df, file="tabella PSO.csv",sep=";",row.names=FALSE)

algoritmo<-c(soluzione[[2]][1])
x<-c(soluzione[[1]][1])
y<-c(soluzione[[1]][2])
fitness<-c(soluzione[[1]][3])
generazioni_soluzione<-c(soluzione[[3]][1])
df<-data.frame(algoritmo,x,y,fitness,generazioni_soluzione)
write.table(df, file="soluzione PSO.txt",sep=";",row.names=FALSE)