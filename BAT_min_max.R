#  Author Nicola Bernabè ver 1.0 8/11/2021
# the implemented version of the GA refers to the following Journal: 
#



controllo_intervallo<-function(x,y,ub,lb){
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


aggiornamento_posizione<-function(Sciame,MAX_DIMENSION_Sciame,j_iterazioni_particelle,f_min,f_max,ub,lb,BEST_FITNESS,r,A,min_max,FUNZIONE_COSTO){
    #message("f_min ",f_min)
    f_i<-f_min+(f_max-f_min)*runif(1,  0,1)
    
    v_x<-Sciame[3,j_iterazioni_particelle]+(Sciame[1,j_iterazioni_particelle]-BEST_FITNESS[1])*f_i
    v_y<-Sciame[4,j_iterazioni_particelle]+(Sciame[2,j_iterazioni_particelle]-BEST_FITNESS[2])*f_i
    #message(v_x," ",Sciame[3,j_iterazioni_particelle]," ",Sciame[1,j_iterazioni_particelle]," ",BEST_FITNESS[1]," ",f_i)
    x<-Sciame[1,j_iterazioni_particelle]+v_x
    y<-Sciame[2,j_iterazioni_particelle]+v_y
    x_y<-controllo_intervallo(x,y,ub,lb)
    x<-x_y[1]
    y<-x_y[2]
    rand<-runif(1,  0,1)
    if(rand>r){
        x<-BEST_FITNESS[1]+0.1*rnorm(1, mean = 0, sd = 1)
        y<-BEST_FITNESS[2]+0.1*rnorm(1, mean = 0, sd = 1)
    }
    x_y<-controllo_intervallo(x,y,ub,lb)
    x<-x_y[1]
    y<-x_y[2]
    sol<-FUNZIONE_COSTO(x_y[1],x_y[2])
    f_new<-sol
    rand<-runif(1,  0,1)
    if(min_max==-1){#minimizzo
        if(f_new<=Sciame[5,j_iterazioni_particelle]&&(rand<A)){
        Sciame[1,j_iterazioni_particelle]<-x_y[1]
        Sciame[2,j_iterazioni_particelle]<-x_y[2]
        Sciame[3,j_iterazioni_particelle]<-v_x
        Sciame[4,j_iterazioni_particelle]<-v_y
        Sciame[5,j_iterazioni_particelle]<-f_new
        
    }
        if(f_new<f_min){
            f_min<-f_new
            
        }


    }else{#massimizzo
        if(f_new>=Sciame[5,j_iterazioni_particelle]&&(rand<A)){
            Sciame[1,j_iterazioni_particelle]<-x_y[1]
            Sciame[2,j_iterazioni_particelle]<-x_y[2]
            Sciame[3,j_iterazioni_particelle]<-v_x
            Sciame[4,j_iterazioni_particelle]<-v_y
            Sciame[5,j_iterazioni_particelle]<-f_new
            
        }
        if(f_new>f_min){
            f_min<-f_new
            
        }


    }
    
    return(list(Sciame,f_min))
}

#aggiorni migliore fitness
aggiorna_fitness<-function(BEST_FITNESS,Migliore_fitness_generazione){
            BEST_FITNESS[1]<-Migliore_fitness_generazione[1]
            BEST_FITNESS[2]<-Migliore_fitness_generazione[2]
            BEST_FITNESS[3]<-Migliore_fitness_generazione[3]#vx 
            BEST_FITNESS[4]<-Migliore_fitness_generazione[4]#vy 
            BEST_FITNESS[5]<-Migliore_fitness_generazione[5]#fitess corrente
    return(BEST_FITNESS)


}

#scorro su tutti gli individui e cerco il fitneess migliore
controllo_migliore_fitness<-function(Sciame,min_max){
    BEST_FITNESS<-c(Sciame[1,1],Sciame[2,1],Sciame[3,1],Sciame[4,1],Sciame[5,1])
    for(i in 2:dim(Sciame)[2]) {
        if(BEST_FITNESS[5]>Sciame[5,i] && min_max==-1){#ricordo che la riga 8 è fitness corrente
            BEST_FITNESS[1]<-Sciame[1,i]#x 
            BEST_FITNESS[2]<-Sciame[2,i]#y 
            BEST_FITNESS[3]<-Sciame[3,i]#vx 
            BEST_FITNESS[4]<-Sciame[4,i]#vy 
            BEST_FITNESS[5]<-Sciame[5,i]#fitess corrente

        }else if(BEST_FITNESS[5]<Sciame[5,i]  && min_max==1){
            BEST_FITNESS[1]<-Sciame[1,i]#x corrente
            BEST_FITNESS[2]<-Sciame[2,i]#y corrente
            BEST_FITNESS[3]<-Sciame[3,i]#vx 
            BEST_FITNESS[4]<-Sciame[4,i]#vy 
            BEST_FITNESS[5]<-Sciame[5,i]#fitess corrente
        }
    }
    return(c(BEST_FITNESS[1],BEST_FITNESS[2],BEST_FITNESS[3],BEST_FITNESS[4],BEST_FITNESS[5]))
}


#inizializzo la matrice
inizializzazione<-function(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,A,FUNZIONE_COSTO){
    Sciame<-matrix(1:1,nrow <- (MAX_UNKNOWNS*3)+2, ncol <- MAX_DIMENSION_Sciame)#oltre alla x e y ci aggiungo le 2 velocità, poi anche le x e y migliori della particella anche la fitness migliore della particella e quella corrente
    numero_casuale_x<-1
    numero_casuale_y <-1
    for(i in 1:dim(Sciame)[2]) {
        numero_casuale_x <- runif(1, lb[1],ub[1] )
        numero_casuale_y <- runif(1,  lb[2],ub[2])
        Sciame[1,i] <-numero_casuale_x
        Sciame[2,i] <-numero_casuale_y
        Sciame[3,i]<-0#vx
        Sciame[4,i]<-0#vy
        Sciame[5,i] <-FUNZIONE_COSTO(Sciame[1,i],Sciame[2,i])#fitness
    }
    return (Sciame)
}
media_sciame<-function(Sciame,j){
    x_y <- c()
    
    for(i in 1:dim(Sciame)[2]) {
        x_y <- c(x_y,Sciame[j,i])

    }
    return(mean(x_y))
}

aggiorno_file_individui<-function(Sciame,MAX_DIMENSION_Sciame,generazione){
                individuo<-c()
                x<-c()
                y<-c()
                fitness<-c()
                v_x<-c()
                v_y<-c()

                for(i in 1:MAX_DIMENSION_Sciame) {
                    x<-c(x,Sciame[1,i])
                    y<-c(y,Sciame[2,i])
                    fitness<-c(fitness,Sciame[5,i])
                    
                    }
            df<-data.frame(x,y,fitness)
            #nome_file<-"generazione"+generazione+".cvs"
            nome_file = paste("BAT generazione ",generazione,".txt",sep="")
            #write.csv(df, file=nome_file,sep=";",row.names=FALSE)
            write.table(df, file= nome_file,sep=";",row.names=FALSE)
            }

mainBAT<-function(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,f_min,f_max,r,A,FUNZIONE_COSTO){
    

    
 
    Sciame<-inizializzazione(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,A,FUNZIONE_COSTO)#inizializzo la popolazione
    BEST_FITNESS<-controllo_migliore_fitness(Sciame,min_max)
    FITNESS_grafico_migliore<-c()
    FITNESS_grafico_migliore_generazione<-c()
    #iterazioni calcolo per trovare il minimo
    indice_stazionarieta<-0#aumenta ad ogni generazione
    n_generazioni<-0

    
    for(i in 1:Generazioni){
        n_generazioni<-i
        for(j_iterazioni_particelle in 1:MAX_DIMENSION_Sciame){#muovo tutte le particelle una volta
            Sciame_lista<-aggiornamento_posizione(Sciame,MAX_DIMENSION_Sciame,j_iterazioni_particelle,f_min,f_max,ub,lb,BEST_FITNESS,r,A,min_max,FUNZIONE_COSTO)
            Sciame<-Sciame_lista[[1]]
            f_min<-Sciame_lista[[2]]


           
        }
        Migliore_fitness_generazione<-controllo_migliore_fitness(Sciame,min_max)
        #message(x_media)
        #message(Migliore_fitness_generazione[1]," ",BEST_FITNESS[1]," x ",x_media)
        #message(Migliore_fitness_generazione[1]," ",Migliore_fitness_generazione[2]," ",Migliore_fitness_generazione[3])
        if(Migliore_fitness_generazione[5]<BEST_FITNESS[5]  && min_max==-1){#minimizzo
            BEST_FITNESS<-aggiorna_fitness(BEST_FITNESS,Migliore_fitness_generazione)
            indice_stazionarieta=0#ritorno a 0 la stazionarietà
            message("migliore soluzione-> x ",BEST_FITNESS[1]," y ",BEST_FITNESS[2]," fitness ",BEST_FITNESS[5])
        }else if(Migliore_fitness_generazione[5]>BEST_FITNESS[5]  && min_max==1){#minimizzo
            BEST_FITNESS<-aggiorna_fitness(BEST_FITNESS,Migliore_fitness_generazione)
            indice_stazionarieta=0#ritorno a 0 la stazionarietà
            message("migliore soluzione-> x ",BEST_FITNESS[1]," y ",BEST_FITNESS[2]," fitness ",BEST_FITNESS[5])
        }
        FITNESS_grafico_migliore<-c(FITNESS_grafico_migliore,BEST_FITNESS[5])
        FITNESS_grafico_migliore_generazione<-c(FITNESS_grafico_migliore_generazione,Migliore_fitness_generazione[5])

        if(indice_stazionarieta>=STAZIONARIETA){
            message("troppo stazionaria ","generazione-> ",i," stazionario->",indice_stazionarieta)
            print(BEST_FITNESS)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }else{
            indice_stazionarieta=indice_stazionarieta+1
        }
        if((i%%stampo_generazione)==0){#stampo così so che sta facendo qualcosa
            message("generazione-> ",i," stazionario->",indice_stazionarieta)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            

        }
        if(BEST_FITNESS[5]<precisione && min_max==-1){
            message("finito algoritmo BAT, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[5]," alla generazione-> ",i)
            aggiorno_file_individui(Sciame,MAX_DIMENSION_Sciame,i)
            break
        }else if(BEST_FITNESS[5]>precisione && min_max==1){
            message("finito algoritmo BAT, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[5]," alla generazione-> ",i)
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
    BEST_FITNESS<-list(BEST_FITNESS,"BAT",n_generazioni)
    return(BEST_FITNESS)
    
 
}

#riesco a trovare la soluzione con questi intervalli 
#ub<-c(1000000000,1000000000)  # upper bound of the unknowns
#lb<-c(-1000000000,-1000000000)  #lower bound of the unknowns


#milione
#ub<-c(1000000,1000000)  # upper bound of the unknowns
#lb<-c(-1000000,-1000000)  #lower bound of the unknowns


#ub<-c(10000,10000)  # upper bound of the unknowns
#lb<-c(-10000,-10000)  #lower bound of the unknowns


ub<-c(100,100)# upper bound of the unknowns
lb<-c(-100,-100)  #lower bound of the unknowns

#ub<-c(512,512)  
#lb<-c(-512,-512)


MAX_UNKNOWNS<-2  #numero variabili
MAX_DIMENSION_Sciame<-300#dimensione popolazione
Generazioni<-10000
STAZIONARIETA<-1000
f_min<-0
f_max<-2#ub[1]
r<-0.95
A<-0.95#A_max#puo essere costante o decrescere
min_max<--1#-1 minimizzo,1 massimizzo
percentuale_generazione_stampo<-10  #ogni 10% delle generazioni fatte stampo su file tutti gli individui
stampo_generazione<-Generazioni/percentuale_generazione_stampo
#massimizzo
#precisione<-11.999999#Parabolic Function
#precisione<-79.999#rastrigin function 
#precisione<-414.23847913128#Parabolic Function

#minimizzo
precisione<-0.0001#
#precisione<--0.999999#Easom function
#precisione<-0.2925789999#Schaffer function N. 4 f(0,+-1.25313)=0.292579
#precisione<--0.999999#per funzioni con minimo negativo
#precisione<--959.6406#Eggholder function   f(512,404.2319)=-959.6407   i domini x e y   ub<-c(512,512)  lb<-c(-512,-512)


FUNZIONE_COSTO<-function(x,y){
    z<-(100*(y-(x)^2)^2)+(1-x)^2#FUNZIONE_COSTO banana  f(1,1)=0
    #z<-((1-x)^2)+100*((y-x^2))^2#Rosenbrock function constrained with a cubic and a line  sol f(1,1)=0
    #z<-(1.5-x+x*y)^2+(2.25-x+x*y^2)^2+(2.625-x+x*y^3)^2#Beale function f(3,0.5)=0
    #z<-0.26*(x^2+y^2)-0.48*x*y#Matyas function f(0,0)=0
    #z<-(sin(3*pi*x))^2+(x-1)^2*(1+sin(3*pi*y))+(y-1)^2*(1+(sin(2*pi*y))^2)#Lévi function N.13  f(1,1)=0
    #z<-0.5+((sin(x^2-y^2))^2-0.5)/(1+0.001*(x^2+y^2))^2#Schaffer function N. 2  f(0,0)=0
    #z<-2*x^2-1.05*x^4+(x^6/6)+x*y+y^2#THREE-HUMP CAMEL FUNCTION xi  [-5, 5]  f(0,0)=0
    #z<--20*exp(-0.2*sqrt((1/2)*(x*x+y*y)))-exp(1/2*((cos(2*pi*x)+cos(2*pi*y))))+20+exp(1)#ACKLEY FUNCTION xi  [-32.768, 32.768]  f(0,0)=0
    #z<-10*2+((x^2-10*cos(2*pi*x))+(y^2-10*cos(2*pi*y)))#RASTRIGIN FUNCTION f(0,0)=0 xi [-5.12, 5.12]
    #z<-(x+2*y-7)^2+(2*x+y-5)^2#BOOTH FUNCTION  xi [-10, 10] f(1,3)=0
    #z<-0.5+(((cos(sin(abs(x^2-y^2))))^2 - 0.5)/((1 + 0.001*(x^2+y^2))^2))#Schaffer function N. 4 f(0,+-1.25313)=0.292579
    #massimizzare
    #z<-12-(x^2+y^2)/100 #Parabolic Function  f(0,0)=12 non so ma il massimo è 12
    #z<-80-(20+x^2+y^2-10*(cos(2*pi*x)+cos(2*pi*y)))#rastrigin function f(0,0)=80
    #z<-275-((x^4-16*x^2+5*x*0.5)+(y^4-16*y^2+5*y*0.5)+3)#Styblinski Function f(-2.8667,-2.8667)=414.23847913
    return (z)
}




soluzione<-mainBAT(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_Sciame,Generazioni,STAZIONARIETA,min_max,stampo_generazione,precisione,f_min,f_max,r,A,FUNZIONE_COSTO)
#print(soluzione)

algoritmo<-c(soluzione[[2]][1])
x<-c(soluzione[[1]][1])
y<-c(soluzione[[1]][2])
fitness<-c(soluzione[[1]][5])
generazioni_soluzione<-c(soluzione[[3]][1])
df<-data.frame(algoritmo,x,y,fitness,generazioni_soluzione)
write.table(df, file="soluzione BAT.txt",sep=";",row.names=FALSE)