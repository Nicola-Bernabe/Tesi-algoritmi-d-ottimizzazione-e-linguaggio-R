#  Author Nicola Bernabè ver 1.0 8/11/2021
# the implemented version of the GA refers to the following Journal: 
#


#prendo 2 individui che saranno i genitori presi casualmente dalla popolazione
selezione<-function(Population,MAX_DIMENSION_POPULATION,genitori_1_2){
    #message(genitori_1_2[1]," rgrg ",genitori_1_2[2])
    #print(Population[1][genitori_1_2[1]])
    g1<-c(Population[1,genitori_1_2[1]],Population[2,genitori_1_2[1]],Population[3,genitori_1_2[1]])
    g2<-c(Population[1,genitori_1_2[2]],Population[2,genitori_1_2[2]],Population[3,genitori_1_2[2]])
    #print(g1[1][genitori_1_2[1]])
    
    #print(Population)
    #print(Population[1,genitori_1_2[1]])
    return(c(g1,g2))
}
crossover<-function(genitori_1_2,FUNZIONE_COSTO){#dato che ho solo 2 variabili ce poco da fare, la x del genitore 1 va al primo figlio , la y del primo genitore va al primo figlio, i restanti al 2 figlio
    figlio_1<-c(genitori_1_2[1],genitori_1_2[5],FUNZIONE_COSTO(genitori_1_2[1],genitori_1_2[5]))
    figlio_2<-c(genitori_1_2[2],genitori_1_2[4],FUNZIONE_COSTO(genitori_1_2[2],genitori_1_2[4]))
    return(c(figlio_1,figlio_2))


}
#parte mutazione, ricordo figlio 1(1,2,3) figlio 2 (4,5,6) e si intendono (x,y,fitness)
mutazione<-function(figli_1_2,percentuale,ub,lb,MAX_DIMENSION_POPULATION,FUNZIONE_COSTO){
    if(runif(1, 0, 1)>0.5){#provo a generate un numero da 0 a 1, se >0.5 aumento al massimo del della percentuale,altrimenti lo diminuisco al massimo dela percentuale, X
        if(runif(1, 0, 1)>0.5){
            figli_1_2[1]<-figli_1_2[1]*runif(1,  1,1+percentuale)

            figli_1_2[4]<-figli_1_2[4]*runif(1,  1,1+percentuale)

            if(figli_1_2[1]>=ub[1]){#se vado oltre all intermallo allora metto il massimo
                figli_1_2[1]<-ub[1]
            }
            if(figli_1_2[4]>=ub[1]){#se vado oltre all intermallo allora metto il massimo
                figli_1_2[4]<-ub[1]
            }
            }
    }else{#Y
        figli_1_2[2]<-figli_1_2[2]*runif(1,percentuale,  1)

        figli_1_2[5]<-figli_1_2[5]*runif(1,percentuale,  1)
        if(figli_1_2[2]<=lb[2]){#se vado oltre all intermallo allora metto il massimo
            figli_1_2[2]<-lb[2]
        }
        if(figli_1_2[5]<=lb[2]){#se vado oltre all intermallo allora metto il massimo
            figli_1_2[5]<-lb[2]
        }


    }
    figli_1_2[3]<-FUNZIONE_COSTO(figli_1_2[1],figli_1_2[2])
    figli_1_2[6]<-FUNZIONE_COSTO(figli_1_2[4],figli_1_2[5])
    
    


       
    
    return(figli_1_2)
}
aggiorno_file_individui<-function(Population,MAX_DIMENSION_POPULATION,generazione){
                individuo<-c()
                x<-c()
                y<-c()
                fitness<-c()
                for(i in 1:MAX_DIMENSION_POPULATION) {
                    x<-c(x,Population[1,i])
                    y<-c(y,Population[2,i])
                    fitness<-c(fitness,Population[3,i])
                    
                    }
            df<-data.frame(x,y,fitness)
            #nome_file<-"generazione"+generazione+".cvs"
            nome_file = paste("GA generazione ",generazione,".txt",sep="")
            #write.csv(df, file=nome_file,sep=";",row.names=FALSE)
            write.table(df, file= nome_file,sep=";",row.names=FALSE)
            }



#aggiorni migliore fitness
aggiorna_fitness<-function(BEST_FITNESS,Migliore_fitness_generazione){
            BEST_FITNESS[1]<-Migliore_fitness_generazione[1]
            BEST_FITNESS[2]<-Migliore_fitness_generazione[2]
            BEST_FITNESS[3]<-Migliore_fitness_generazione[3]
    return(BEST_FITNESS)


}

#scorro su tutti gli individui e cerco il fitneess migliore
controllo_migliore_fitness<-function(Population,MAX_UNKNOWNS,min_max){
    BEST_FITNESS<-c(Population[1,1],Population[2,1],Population[3,1])
    for(i in 2:dim(Population)[2]) {
        if(BEST_FITNESS[3]>Population[3,i] && min_max==-1){#minimizzo
            BEST_FITNESS[1]<-Population[1,i]
            BEST_FITNESS[2]<-Population[2,i]
            BEST_FITNESS[3]<-Population[3,i]

        }else if(BEST_FITNESS[3]<Population[3,i] && min_max==1){
            BEST_FITNESS[1]<-Population[1,i]
            BEST_FITNESS[2]<-Population[2,i]
            BEST_FITNESS[3]<-Population[3,i]
        }


    }

    return(c(BEST_FITNESS[1],BEST_FITNESS[2],BEST_FITNESS[3]))
}




#inizializzo la matrice
inizializzazione<-function(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_POPULATION,FUNZIONE_COSTO){
    Population<-matrix(1:30,nrow <- MAX_UNKNOWNS+1, ncol <- MAX_DIMENSION_POPULATION)#oltre alla x e y ci aggiungo anche la fitness, così ho 3 righe
    numero_casuale_x<-1
    numero_casuale_y <-1
    for(i in 1:dim(Population)[2]) {
        numero_casuale_x <- runif(1, lb[1],ub[1] )
        numero_casuale_y <- runif(1,  lb[2],ub[2])
        Population[1,i] <-numero_casuale_x
        Population[2,i] <-numero_casuale_y
        Population[3,i] <-FUNZIONE_COSTO(Population[1,i],Population[2,i])

    }


    return (Population)
}
aggiorno_figli_e_genitori<-function(genitori_1_2_indici,figli_1_2,Population){
    Population[1,genitori_1_2_indici[1]]<-figli_1_2[1]#x
    Population[2,genitori_1_2_indici[1]]<-figli_1_2[2]#y
    Population[3,genitori_1_2_indici[1]]<-figli_1_2[3]#fitness
    Population[1,genitori_1_2_indici[2]]<-figli_1_2[4]#x
    Population[2,genitori_1_2_indici[2]]<-figli_1_2[5]#x
    Population[3,genitori_1_2_indici[2]]<-figli_1_2[6]#x


    return (Population)
}

mainGA<-function(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_POPULATION,MAXIMUM_GENERATION_NUMBER,STAZIONARIETA,precisione,min_max,stampo_generazione,CROSS_OVER_PROBABILITY,MUTATION_PROBABILITY,aumeto_diminuisco_percentuale_valore,FUNZIONE_COSTO){
    Population<-inizializzazione(ub, lb,MAX_UNKNOWNS,MAX_DIMENSION_POPULATION,FUNZIONE_COSTO)#inizializzo la popolazione
    BEST_FITNESS<-controllo_migliore_fitness(Population,MAX_UNKNOWNS,min_max)
    FITNESS_grafico_migliore<-c()
    FITNESS_grafico_migliore_generazione<-c()
    FITNESS_migliore_file<-c()
    #iterazioni calcolo per trovare il minimo
    indice_stazionarieta<-0#aumenta ad ogni generazione
    figli_1_2<-0
    n_generazioni<-0
   
    for(i in 1:MAXIMUM_GENERATION_NUMBER){
        n_generazioni<-i
        #print(runif(1, 0,1))
        for(j_iterazioni_individui in 1:MAX_DIMENSION_POPULATION/2){
            genitori_1_2_indici<-sample(1:(MAX_DIMENSION_POPULATION), 2)#2 individui casuali
            genitori_1_2<-selezione(Population,MAX_DIMENSION_POPULATION,genitori_1_2_indici)
            
            if(runif(1, 0,1)<=CROSS_OVER_PROBABILITY){#probabilità crossover
                figli_1_2<-crossover(genitori_1_2,FUNZIONE_COSTO)#ho 2 figli
                #print(figli_1_2)

                }
            
            if(runif(1, 0,1)<=MUTATION_PROBABILITY){
                #message("generazione-> ",i," ",figli_1_2)
               
    
                figli_1_2<-mutazione(figli_1_2,aumeto_diminuisco_percentuale_valore,ub,lb,MAX_DIMENSION_POPULATION,FUNZIONE_COSTO)
                


            }
            Population<-aggiorno_figli_e_genitori(genitori_1_2_indici,figli_1_2,Population)
            
            #print(BEST_FITNESS)


        }
        Migliore_fitness_generazione<-controllo_migliore_fitness(Population,MAX_UNKNOWNS,min_max)
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
            aggiorno_file_individui(Population,MAX_DIMENSION_POPULATION,i)
            break
        }else{
            indice_stazionarieta=indice_stazionarieta+1
        }
        if((i%%stampo_generazione)==0){#stampo così so che sta facendo qualcosa

            

            aggiorno_file_individui(Population,MAX_DIMENSION_POPULATION,i)

            
            message("generazione-> ",i," stazionario->",indice_stazionarieta)
            

        }
        if(BEST_FITNESS[3]<precisione && min_max==-1){
            message("finito algoritmo GA, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Population,MAX_DIMENSION_POPULATION,i)
            break
        }else if(BEST_FITNESS[3]>precisione && min_max==1){
            message("finito algoritmo GA, sono riuscito ad avere una soluzione molto buona ed è ",BEST_FITNESS[3]," alla generazione-> ",i)
            aggiorno_file_individui(Population,MAX_DIMENSION_POPULATION,i)
            break
        }
            
        
    }
    windows()
    par(mfrow=c(1,2))
    plot(FITNESS_grafico_migliore, type="l", col="red",lwd=10)
    lines(FITNESS_grafico_migliore_generazione, type="l", col="blue",lty=5)
    title(main="migliore e andamento fitness per generazione", col.main="blue", font.main="4")

    x_individui<-c()
    y_individui<-c()
    for(x_y in 1:MAX_DIMENSION_POPULATION){
        x_individui<-c(x_individui,Population[1,x_y])
        y_individui<-c(y_individui,Population[2,x_y])

    }

    plot(x_individui, y_individui, xlab="x",ylab="y", main="dove si trovano gli individui") 
    BEST_FITNESS<-list(BEST_FITNESS,"GA",n_generazioni)

    return (BEST_FITNESS)
}


#miliardo
#ub<-c(1000000000,1000000000)  # upper bound of the unknowns
#lb<-c(-1000000000,-1000000000)  #lower bound of the unknowns
#aumeto_diminuisco_percentuale_valore=0.1
#milione
#ub<-c(1000000,1000000)  # upper bound of the unknowns
#lb<-c(-1000000,-1000000)  #lower bound of the unknowns
#aumeto_diminuisco_percentuale_valore=0.1
#mille
#ub<-c(100,100)  # upper bound of the unknowns
#lb<-c(-100,-100)  #lower bound of the unknowns

ub<-c(512,512)  
lb<-c(-512,-512)

aumeto_diminuisco_percentuale_valore=0.05

MAX_UNKNOWNS<-2  #numero variabili
MAX_DIMENSION_POPULATION<-100#dimensione popolazione
CROSS_OVER_PROBABILITY<-0.95#probabilità per ogni generazione di usare il cross-over
MUTATION_PROBABILITY<-0.001#probabilità per ogni generazione di usare la mutazione
#aumeto_diminuisco_percentuale_valore=0.1#quando c'è mutazione scelgo casualmente x o y e modifico del 5% il valore aumentandolo o diminuendolo stando attendo a lb e ub
MAXIMUM_GENERATION_NUMBER<-100000
STAZIONARIETA<-10000
min_max<--1#-1 minimizzo,1 massimizzo
percentuale_generazione_stampo<-10  #ogni 10% delle generazioni fatte stampo su file tutti gli individui
stampo_generazione<-MAXIMUM_GENERATION_NUMBER/percentuale_generazione_stampo
#massimizzo
#precisione<-11.999#Parabolic Function
#precisione<-79.999#rastrigin function 
#precisione<-415.2384791312#Parabolic Function

#minimizzo
precisione<-0.001#
#precisione<--0.999999#Easom function
#precisione<-0.2925789999#Schaffer function N. 4 f(0,+-1.25313)=0.292579
#precisione<--0.999999#per funzioni con minimo negativo
#precisione<--959.6406#Eggholder function   f(512,404.2319)=-959.6407   i domini x e y   ub<-c(512,512)  lb<-c(-512,-512)


FUNZIONE_COSTO<-function(x,y){
        #z<-(100*(y-(x)^2)^2)+(1-x)^2#FUNZIONE_COSTO banana  f(1,1)=0
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




soluzione<-mainGA(ub,lb,MAX_UNKNOWNS,MAX_DIMENSION_POPULATION,MAXIMUM_GENERATION_NUMBER,STAZIONARIETA,precisione,min_max,stampo_generazione,CROSS_OVER_PROBABILITY,MUTATION_PROBABILITY,aumeto_diminuisco_percentuale_valore,FUNZIONE_COSTO)


#print(soluzione)


algoritmo<-c(soluzione[[2]][1])
x<-c(soluzione[[1]][1])
y<-c(soluzione[[1]][2])
fitness<-c(soluzione[[1]][3])
generazioni_soluzione<-c(soluzione[[3]][1])
df<-data.frame(algoritmo,x,y,fitness,generazioni_soluzione)
write.table(df, file="soluzione GA.txt",sep=";",row.names=FALSE)