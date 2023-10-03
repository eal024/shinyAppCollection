



# Forutsetning
x <- 4
y <- 10

# Resultat
resultat <- (x + y)



## Funksjoner
utregning <- function(x,y){
    
    resultat <- (x+y)
    
    return(resultat)
}


utregning( x = 100, y = 200) # Returnerer 300



#
hel_utregning <- function( ){
    
    
    vec <- vector()
    
    # Intraksjon med bruker
    for( i in 1:2){
        
        print("Gi verdi") 
        vec[i] <- readline();
        print( paste0( "Du ga verdi for", ifelse(i == 1, " x lik ", " y lik "), vec[i] ) )
        
    }
    
    # Print input
    print( as.numeric(vec[1]) )
    print("Resultatet er:")
    utregning( x = as.numeric(vec[1]), y = as.numeric(vec[2])) # Returnerer 300
    
}


hel_utregning()

