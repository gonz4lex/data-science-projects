-   [**EXPLORACI?N DE LOS DATOS**](#exploracin-de-los-datos)
-   [**SUBGRUPOS ELIMINANDO
    REGISTROS**](#subgrupos-eliminando-registros)
-   [LIMPIEZA DE DATOS](#limpieza-de-datos)
-   [SELECCI?N Y TRANSFORMACI?N DE
    VARIABLES](#seleccin-y-transformacin-de-variables)
-   [DESCUBRIMIENTO DE SUBGRUPOS](#descubrimiento-de-subgrupos)
    -   [A. ACCIDENTES](#a.-accidentes)
    -   [B. VEHICLES](#b.-vehicles)
    -   [C. CASUALTIES](#c.-casualties)
-   [**SUBGRUPOS ELIMINANDO
    VARIABLES**](#subgrupos-eliminando-variables)
-   [LIMPIEZA DE DATOS](#limpieza-de-datos-1)
-   [SELECCI?N Y TRANSFORMACI?N DE
    VARIABLES](#seleccin-y-transformacin-de-variables-1)
-   [DESCUBRIMIENTO DE SUBGRUPOS](#descubrimiento-de-subgrupos-1)
    -   [A. ACCIDENTES](#a.-accidentes-1)
    -   [B. VEHICLES](#b.-vehicles-1)
    -   [C. CASUALTIES](#c.-casualties-1)
-   [**COMPARACI?N DE MODELOS**](#comparacin-de-modelos)
-   [**MEDIDAS DE CALIDAD**](#medidas-de-calidad)
-   [**COMPARACI?N DE LA CALIDAD DE LOS
    MODELOS**](#comparacin-de-la-calidad-de-los-modelos)
-   [**ANEXO**](#anexo)

El gobierno brit?nico publica cada a?o los datos relativos a la
seguridad vial sobre las circunstancias que han envuelto los accidentes
de tr?fico en Gran Breta?a.

El objetivo principal es descubrir subgrupos en los datos que puedan
resultar de inter?s y su posterior interpretaci?n.

Se valorar?n los siguientes aspectos: 1. Calidad de los subgrupos
encontrados, su inter?s e interpretaci?n. 2. Uso de diferentes
algoritmos de descubrimiento de subgrupos. 3. Uso de distintas medidas
de calidad.

**EXPLORACI?N DE LOS DATOS**
----------------------------

Se cargan los packages que se van a utilizar y los archivos de datos que
se van a analizar:

    library(ggplot2)
    library(readr)
    library(data.table)
    library(rsubgroup)
    accidents = read.csv("./acc/Accidents0514.csv", header = T, sep = ",")
    casualties = read.csv("./acc/Casualties0514.csv", header = T, sep = ",")
    vehicles <- read.csv("./acc/Vehicles0514.csv", header = T, sep = ",")

Exploramos los datos para ver c?mo se distribuye la informaci?n:

    summary(accidents)

    ##      ï..Accident_Index   Location_Easting_OSGR Location_Northing_OSGR
    ##  200501BS00001:      1   Min.   : 64950        Min.   :  10290       
    ##  200501BS00002:      1   1st Qu.:375484        1st Qu.: 178120       
    ##  200501BS00003:      1   Median :440380        Median : 266620       
    ##  200501BS00004:      1   Mean   :439605        Mean   : 299248       
    ##  200501BS00005:      1   3rd Qu.:523020        3rd Qu.: 396990       
    ##  200501BS00006:      1   Max.   :655540        Max.   :1208800       
    ##  (Other)      :1640591   NA's   :111           NA's   :111           
    ##    Longitude          Latitude      Police_Force   Accident_Severity
    ##  Min.   :-7.5162   Min.   :49.91   Min.   : 1.00   Min.   :1.000    
    ##  1st Qu.:-2.3678   1st Qu.:51.49   1st Qu.: 7.00   1st Qu.:3.000    
    ##  Median :-1.3987   Median :52.29   Median :31.00   Median :3.000    
    ##  Mean   :-1.4369   Mean   :52.58   Mean   :30.83   Mean   :2.839    
    ##  3rd Qu.:-0.2217   3rd Qu.:53.47   3rd Qu.:46.00   3rd Qu.:3.000    
    ##  Max.   : 1.7620   Max.   :60.76   Max.   :98.00   Max.   :3.000    
    ##  NA's   :111       NA's   :111                                      
    ##  Number_of_Vehicles Number_of_Casualties         Date        
    ##  Min.   : 1.000     Min.   : 1.000       21/10/2005:    822  
    ##  1st Qu.: 1.000     1st Qu.: 1.000       18/11/2005:    787  
    ##  Median : 2.000     Median : 1.000       29/09/2006:    784  
    ##  Mean   : 1.831     Mean   : 1.351       22/09/2006:    780  
    ##  3rd Qu.: 2.000     3rd Qu.: 1.000       07/12/2005:    775  
    ##  Max.   :67.000     Max.   :93.000       01/12/2006:    750  
    ##                                          (Other)   :1635899  
    ##   Day_of_Week         Time         Local_Authority_.District.
    ##  Min.   :1.000   17:00  :  16011   Min.   :  1.0             
    ##  1st Qu.:2.000   17:30  :  15241   1st Qu.:122.0             
    ##  Median :4.000   16:00  :  14641   Median :328.0             
    ##  Mean   :4.117   18:00  :  14434   Mean   :354.1             
    ##  3rd Qu.:6.000   15:30  :  14384   3rd Qu.:532.0             
    ##  Max.   :7.000   16:30  :  13910   Max.   :941.0             
    ##                  (Other):1551976                             
    ##  Local_Authority_.Highway. X1st_Road_Class X1st_Road_Number
    ##  E10000016:  44987         Min.   :1.00    Min.   :  -1    
    ##  E10000030:  41574         1st Qu.:3.00    1st Qu.:   0    
    ##  E10000017:  36555         Median :4.00    Median : 129    
    ##  E10000012:  35550         Mean   :4.09    Mean   :1010    
    ##  E10000014:  33085         3rd Qu.:6.00    3rd Qu.: 726    
    ##  E08000025:  31181         Max.   :6.00    Max.   :9999    
    ##  (Other)  :1417665                                         
    ##    Road_Type      Speed_limit   Junction_Detail  Junction_Control
    ##  Min.   :1.000   Min.   :10.0   Min.   :-1.000   Min.   :-1.000  
    ##  1st Qu.:6.000   1st Qu.:30.0   1st Qu.: 0.000   1st Qu.:-1.000  
    ##  Median :6.000   Median :30.0   Median : 3.000   Median : 2.000  
    ##  Mean   :5.167   Mean   :39.1   Mean   : 2.337   Mean   : 1.812  
    ##  3rd Qu.:6.000   3rd Qu.:50.0   3rd Qu.: 3.000   3rd Qu.: 4.000  
    ##  Max.   :9.000   Max.   :70.0   Max.   : 9.000   Max.   : 4.000  
    ##                                                                  
    ##  X2nd_Road_Class  X2nd_Road_Number Pedestrian_Crossing.Human_Control
    ##  Min.   :-1.000   Min.   :  -1.0   Min.   :-1.000000                
    ##  1st Qu.:-1.000   1st Qu.:   0.0   1st Qu.: 0.000000                
    ##  Median : 3.000   Median :   0.0   Median : 0.000000                
    ##  Mean   : 2.658   Mean   : 379.6   Mean   : 0.009437                
    ##  3rd Qu.: 6.000   3rd Qu.:   0.0   3rd Qu.: 0.000000                
    ##  Max.   : 6.000   Max.   :9999.0   Max.   : 2.000000                
    ##                                                                     
    ##  Pedestrian_Crossing.Physical_Facilities Light_Conditions
    ##  Min.   :-1.0000                         Min.   :1.00    
    ##  1st Qu.: 0.0000                         1st Qu.:1.00    
    ##  Median : 0.0000                         Median :1.00    
    ##  Mean   : 0.7289                         Mean   :1.95    
    ##  3rd Qu.: 0.0000                         3rd Qu.:4.00    
    ##  Max.   : 8.0000                         Max.   :7.00    
    ##                                                          
    ##  Weather_Conditions Road_Surface_Conditions Special_Conditions_at_Site
    ##  Min.   :-1.000     Min.   :-1.000          Min.   :-1.0000           
    ##  1st Qu.: 1.000     1st Qu.: 1.000          1st Qu.: 0.0000           
    ##  Median : 1.000     Median : 1.000          Median : 0.0000           
    ##  Mean   : 1.581     Mean   : 1.363          Mean   : 0.1099           
    ##  3rd Qu.: 1.000     3rd Qu.: 2.000          3rd Qu.: 0.0000           
    ##  Max.   : 9.000     Max.   : 5.000          Max.   : 7.0000           
    ##                                                                       
    ##  Carriageway_Hazards Urban_or_Rural_Area
    ##  Min.   :-1.00000    Min.   :1.000      
    ##  1st Qu.: 0.00000    1st Qu.:1.000      
    ##  Median : 0.00000    Median :1.000      
    ##  Mean   : 0.07308    Mean   :1.357      
    ##  3rd Qu.: 0.00000    3rd Qu.:2.000      
    ##  Max.   : 7.00000    Max.   :3.000      
    ##                                         
    ##  Did_Police_Officer_Attend_Scene_of_Accident LSOA_of_Accident_Location
    ##  Min.   :-1.000                                       : 120574        
    ##  1st Qu.: 1.000                              E01000004:   2367        
    ##  Median : 1.000                              E01011365:   1503        
    ##  Mean   : 1.193                              E01004736:   1407        
    ##  3rd Qu.: 1.000                              E01008440:   1066        
    ##  Max.   : 3.000                              E01004764:    927        
    ##                                              (Other)  :1512753

    summary(casualties)

    ##      ï..Accident_Index   Vehicle_Reference Casualty_Reference
    ##  20144100J0489:     93   Min.   : 1.000    Min.   :  1.000   
    ##  201106X047581:     87   1st Qu.: 1.000    1st Qu.:  1.000   
    ##  201411NH11644:     87   Median : 1.000    Median :  1.000   
    ##  2013460234852:     70   Mean   : 1.485    Mean   :  1.434   
    ##  200743N002017:     68   3rd Qu.: 2.000    3rd Qu.:  2.000   
    ##  2011130049840:     63   Max.   :91.000    Max.   :852.000   
    ##  (Other)      :2216252                                       
    ##  Casualty_Class  Sex_of_Casualty  Age_of_Casualty  Age_Band_of_Casualty
    ##  Min.   :1.000   Min.   :-1.000   Min.   : -1.00   Min.   :-1.00       
    ##  1st Qu.:1.000   1st Qu.: 1.000   1st Qu.: 20.00   1st Qu.: 4.00       
    ##  Median :1.000   Median : 1.000   Median : 31.00   Median : 6.00       
    ##  Mean   :1.494   Mean   : 1.416   Mean   : 34.35   Mean   : 6.02       
    ##  3rd Qu.:2.000   3rd Qu.: 2.000   3rd Qu.: 46.00   3rd Qu.: 8.00       
    ##  Max.   :3.000   Max.   : 2.000   Max.   :103.00   Max.   :11.00       
    ##                                                                        
    ##  Casualty_Severity Pedestrian_Location Pedestrian_Movement
    ##  Min.   :1.000     Min.   :-1.0000     Min.   :-1.0000    
    ##  1st Qu.:3.000     1st Qu.: 0.0000     1st Qu.: 0.0000    
    ##  Median :3.000     Median : 0.0000     Median : 0.0000    
    ##  Mean   :2.867     Mean   : 0.6664     Mean   : 0.4741    
    ##  3rd Qu.:3.000     3rd Qu.: 0.0000     3rd Qu.: 0.0000    
    ##  Max.   :3.000     Max.   :10.0000     Max.   : 9.0000    
    ##                                                           
    ##  Car_Passenger     Bus_or_Coach_Passenger
    ##  Min.   :-1.0000   Min.   :-1.00000      
    ##  1st Qu.: 0.0000   1st Qu.: 0.00000      
    ##  Median : 0.0000   Median : 0.00000      
    ##  Mean   : 0.2829   Mean   : 0.09075      
    ##  3rd Qu.: 0.0000   3rd Qu.: 0.00000      
    ##  Max.   : 2.0000   Max.   : 4.00000      
    ##                                          
    ##  Pedestrian_Road_Maintenance_Worker Casualty_Type   
    ##  Min.   :-1.0000                    Min.   : 0.000  
    ##  1st Qu.:-1.0000                    1st Qu.: 3.000  
    ##  Median :-1.0000                    Median : 9.000  
    ##  Mean   :-0.6254                    Mean   : 7.511  
    ##  3rd Qu.: 0.0000                    3rd Qu.: 9.000  
    ##  Max.   : 2.0000                    Max.   :98.000  
    ##                                                     
    ##  Casualty_Home_Area_Type
    ##  Min.   :-1.0000        
    ##  1st Qu.: 1.0000        
    ##  Median : 1.0000        
    ##  Mean   : 0.9905        
    ##  3rd Qu.: 1.0000        
    ##  Max.   : 3.0000        
    ## 

    summary(vehicles)

    ##      ï..Accident_Index   Vehicle_Reference  Vehicle_Type   
    ##  2013460234852:     67   Min.   : 1.000    Min.   :-1.000  
    ##  2011520104001:     34   1st Qu.: 1.000    1st Qu.: 9.000  
    ##  2009559D05333:     32   Median : 1.000    Median : 9.000  
    ##  2011160B00431:     29   Mean   : 1.555    Mean   : 9.651  
    ##  2007460143303:     28   3rd Qu.: 2.000    3rd Qu.: 9.000  
    ##  2006460128486:     22   Max.   :91.000    Max.   :98.000  
    ##  (Other)      :3004213                                     
    ##  Towing_and_Articulation Vehicle_Manoeuvre
    ##  Min.   :-1.00000        Min.   :-1.00    
    ##  1st Qu.: 0.00000        1st Qu.: 7.00    
    ##  Median : 0.00000        Median :17.00    
    ##  Mean   : 0.03124        Mean   :12.69    
    ##  3rd Qu.: 0.00000        3rd Qu.:18.00    
    ##  Max.   : 5.00000        Max.   :18.00    
    ##                                           
    ##  Vehicle_Location.Restricted_Lane Junction_Location
    ##  Min.   :-1.0000                  Min.   :-1.000   
    ##  1st Qu.: 0.0000                  1st Qu.: 0.000   
    ##  Median : 0.0000                  Median : 1.000   
    ##  Mean   : 0.1327                  Mean   : 2.533   
    ##  3rd Qu.: 0.0000                  3rd Qu.: 6.000   
    ##  Max.   : 9.0000                  Max.   : 8.000   
    ##                                                    
    ##  Skidding_and_Overturning Hit_Object_in_Carriageway
    ##  Min.   :-1.0000          Min.   :-1.0000          
    ##  1st Qu.: 0.0000          1st Qu.: 0.0000          
    ##  Median : 0.0000          Median : 0.0000          
    ##  Mean   : 0.2144          Mean   : 0.3048          
    ##  3rd Qu.: 0.0000          3rd Qu.: 0.0000          
    ##  Max.   : 5.0000          Max.   :12.0000          
    ##                                                    
    ##  Vehicle_Leaving_Carriageway Hit_Object_off_Carriageway
    ##  Min.   :-1.0000             Min.   :-1.0000           
    ##  1st Qu.: 0.0000             1st Qu.: 0.0000           
    ##  Median : 0.0000             Median : 0.0000           
    ##  Mean   : 0.3742             Mean   : 0.5724           
    ##  3rd Qu.: 0.0000             3rd Qu.: 0.0000           
    ##  Max.   : 8.0000             Max.   :11.0000           
    ##                                                        
    ##  X1st_Point_of_Impact Was_Vehicle_Left_Hand_Drive.
    ##  Min.   :-1.000       Min.   :-1.0000             
    ##  1st Qu.: 1.000       1st Qu.: 1.0000             
    ##  Median : 1.000       Median : 1.0000             
    ##  Mean   : 1.765       Mean   : 0.9892             
    ##  3rd Qu.: 3.000       3rd Qu.: 1.0000             
    ##  Max.   : 4.000       Max.   : 2.0000             
    ##                                                   
    ##  Journey_Purpose_of_Driver Sex_of_Driver    Age_of_Driver   
    ##  Min.   :-1.000            Min.   :-1.000   Min.   : -1.00  
    ##  1st Qu.: 2.000            1st Qu.: 1.000   1st Qu.: 22.00  
    ##  Median : 6.000            Median : 1.000   Median : 34.00  
    ##  Mean   : 8.714            Mean   : 1.399   Mean   : 34.33  
    ##  3rd Qu.:15.000            3rd Qu.: 2.000   3rd Qu.: 47.00  
    ##  Max.   :15.000            Max.   : 3.000   Max.   :100.00  
    ##                                                             
    ##  Age_Band_of_Driver Engine_Capacity_.CC. Propulsion_Code  
    ##  Min.   :-1.00      Min.   :   -1        Min.   :-1.0000  
    ##  1st Qu.: 5.00      1st Qu.:   -1        1st Qu.:-1.0000  
    ##  Median : 6.00      Median : 1388        Median : 1.0000  
    ##  Mean   : 5.85      Mean   : 1412        Mean   : 0.7518  
    ##  3rd Qu.: 8.00      3rd Qu.: 1896        3rd Qu.: 1.0000  
    ##  Max.   :11.00      Max.   :99999        Max.   :12.0000  
    ##                                                           
    ##  Age_of_Vehicle    Driver_IMD_Decile Driver_Home_Area_Type
    ##  Min.   : -1.000   Min.   :-1.000    Min.   :-1.0000      
    ##  1st Qu.: -1.000   1st Qu.:-1.000    1st Qu.: 1.0000      
    ##  Median :  4.000   Median : 3.000    Median : 1.0000      
    ##  Mean   :  4.841   Mean   : 3.559    Mean   : 0.8822      
    ##  3rd Qu.:  9.000   3rd Qu.: 7.000    3rd Qu.: 1.0000      
    ##  Max.   :111.000   Max.   :10.000    Max.   : 3.0000      
    ## 

En los datos se observa que algunas variables contienen el valor "-1",
que se exporta para valores NULL o fuera de rango". Analizamos el caso
con mayor detenimiento:

    ##### Accidents:
    total_missing_acc <- matrix(NA,length(accidents),length(1:1))
    perc_missing_acc <- matrix(NA,length(accidents),length(1:1))
    rownames(total_missing_acc) <- c(names(accidents))
    colnames(total_missing_acc) <- c("Total -1 en cada variable Accidents")
    colnames(perc_missing_acc) <- c("% -1 en cada variable Accidents")
    n_accidents <- length(accidents)
    for(i in 1:n_accidents){
      total_missing_acc[i] <- length(which(accidents[,i] == -1))
      perc_missing_acc[i] <- round((length(which(accidents[,i] == -1))/nrow(accidents)), digits = 5)*100
    }
    cbind(total_missing_acc,perc_missing_acc)

    ##                                             Total -1 en cada variable Accidents
    ## ï..Accident_Index                                                             0
    ## Location_Easting_OSGR                                                         0
    ## Location_Northing_OSGR                                                        0
    ## Longitude                                                                     1
    ## Latitude                                                                      0
    ## Police_Force                                                                  0
    ## Accident_Severity                                                             0
    ## Number_of_Vehicles                                                            0
    ## Number_of_Casualties                                                          0
    ## Date                                                                          0
    ## Day_of_Week                                                                   0
    ## Time                                                                          0
    ## Local_Authority_.District.                                                    0
    ## Local_Authority_.Highway.                                                     0
    ## X1st_Road_Class                                                               0
    ## X1st_Road_Number                                                              2
    ## Road_Type                                                                     0
    ## Speed_limit                                                                   0
    ## Junction_Detail                                                              18
    ## Junction_Control                                                         585322
    ## X2nd_Road_Class                                                          676268
    ## X2nd_Road_Number                                                          16118
    ## Pedestrian_Crossing.Human_Control                                            21
    ## Pedestrian_Crossing.Physical_Facilities                                      37
    ## Light_Conditions                                                              0
    ## Weather_Conditions                                                          161
    ## Road_Surface_Conditions                                                    2155
    ## Special_Conditions_at_Site                                                   18
    ## Carriageway_Hazards                                                          32
    ## Urban_or_Rural_Area                                                           0
    ## Did_Police_Officer_Attend_Scene_of_Accident                                 270
    ## LSOA_of_Accident_Location                                                     0
    ##                                             % -1 en cada variable Accidents
    ## ï..Accident_Index                                                     0.000
    ## Location_Easting_OSGR                                                 0.000
    ## Location_Northing_OSGR                                                0.000
    ## Longitude                                                             0.000
    ## Latitude                                                              0.000
    ## Police_Force                                                          0.000
    ## Accident_Severity                                                     0.000
    ## Number_of_Vehicles                                                    0.000
    ## Number_of_Casualties                                                  0.000
    ## Date                                                                  0.000
    ## Day_of_Week                                                           0.000
    ## Time                                                                  0.000
    ## Local_Authority_.District.                                            0.000
    ## Local_Authority_.Highway.                                             0.000
    ## X1st_Road_Class                                                       0.000
    ## X1st_Road_Number                                                      0.000
    ## Road_Type                                                             0.000
    ## Speed_limit                                                           0.000
    ## Junction_Detail                                                       0.001
    ## Junction_Control                                                     35.677
    ## X2nd_Road_Class                                                      41.221
    ## X2nd_Road_Number                                                      0.982
    ## Pedestrian_Crossing.Human_Control                                     0.001
    ## Pedestrian_Crossing.Physical_Facilities                               0.002
    ## Light_Conditions                                                      0.000
    ## Weather_Conditions                                                    0.010
    ## Road_Surface_Conditions                                               0.131
    ## Special_Conditions_at_Site                                            0.001
    ## Carriageway_Hazards                                                   0.002
    ## Urban_or_Rural_Area                                                   0.000
    ## Did_Police_Officer_Attend_Scene_of_Accident                           0.016
    ## LSOA_of_Accident_Location                                             0.000

    ##### Casualties:
    total_missing_cas <- matrix(NA,length(casualties),length(1:1))
    perc_missing_cas <- matrix(NA,length(casualties),length(1:1))
    rownames(total_missing_cas) <- c(names(casualties))
    colnames(total_missing_cas) <- c("Total -1 en cada variable Casualties")
    colnames(perc_missing_cas) <- c("% -1 en cada variable Casualties")
    n_casualties <- length(casualties)
    for(i in 1:n_casualties){
      total_missing_cas[i] <- length(which(casualties[,i] == -1))
      perc_missing_cas[i] <- round((length(which(casualties[,i] == -1))/nrow(casualties)), digits = 5)*100
    }
    cbind(total_missing_cas,perc_missing_cas)

    ##                                    Total -1 en cada variable Casualties
    ## ï..Accident_Index                                                     0
    ## Vehicle_Reference                                                     0
    ## Casualty_Reference                                                    0
    ## Casualty_Class                                                        0
    ## Sex_of_Casualty                                                     630
    ## Age_of_Casualty                                                   45920
    ## Age_Band_of_Casualty                                              45920
    ## Casualty_Severity                                                     0
    ## Pedestrian_Location                                                  12
    ## Pedestrian_Movement                                                  15
    ## Car_Passenger                                                       404
    ## Bus_or_Coach_Passenger                                               38
    ## Pedestrian_Road_Maintenance_Worker                              1439022
    ## Casualty_Type                                                         0
    ## Casualty_Home_Area_Type                                          322715
    ##                                    % -1 en cada variable Casualties
    ## ï..Accident_Index                                             0.000
    ## Vehicle_Reference                                             0.000
    ## Casualty_Reference                                            0.000
    ## Casualty_Class                                                0.000
    ## Sex_of_Casualty                                               0.028
    ## Age_of_Casualty                                               2.072
    ## Age_Band_of_Casualty                                          2.072
    ## Casualty_Severity                                             0.000
    ## Pedestrian_Location                                           0.001
    ## Pedestrian_Movement                                           0.001
    ## Car_Passenger                                                 0.018
    ## Bus_or_Coach_Passenger                                        0.002
    ## Pedestrian_Road_Maintenance_Worker                           64.917
    ## Casualty_Type                                                 0.000
    ## Casualty_Home_Area_Type                                      14.558

    ##### Vehicles:
    total_missing_vec <- matrix(NA,length(vehicles),length(1:1))
    perc_missing_vec <- matrix(NA,length(vehicles),length(1:1))
    rownames(total_missing_vec) <- c(names(vehicles))
    colnames(total_missing_vec) <- c("Total -1 en cada variable Vehicles")
    colnames(perc_missing_vec) <- c("% -1 en cada variable Vehicles")
    n_vehicles <- length(vehicles)
    for(i in 1:n_vehicles){
      total_missing_vec[i] <- length(which(vehicles[,i] == -1))
      perc_missing_vec[i] <- round((length(which(vehicles[,i] == -1))/nrow(vehicles)), digits = 5)*100
    }
    cbind(total_missing_vec,perc_missing_vec)

    ##                                  Total -1 en cada variable Vehicles
    ## ï..Accident_Index                                                 0
    ## Vehicle_Reference                                                 0
    ## Vehicle_Type                                                    473
    ## Towing_and_Articulation                                         184
    ## Vehicle_Manoeuvre                                              1080
    ## Vehicle_Location.Restricted_Lane                                 74
    ## Junction_Location                                              9876
    ## Skidding_and_Overturning                                        132
    ## Hit_Object_in_Carriageway                                        87
    ## Vehicle_Leaving_Carriageway                                     127
    ## Hit_Object_off_Carriageway                                       54
    ## X1st_Point_of_Impact                                            565
    ## Was_Vehicle_Left_Hand_Drive.                                  23049
    ## Journey_Purpose_of_Driver                                     44921
    ## Sex_of_Driver                                                    33
    ## Age_of_Driver                                                330608
    ## Age_Band_of_Driver                                           330608
    ## Engine_Capacity_.CC.                                         788471
    ## Propulsion_Code                                              770004
    ## Age_of_Vehicle                                               874989
    ## Driver_IMD_Decile                                            849921
    ## Driver_Home_Area_Type                                        588611
    ##                                  % -1 en cada variable Vehicles
    ## ï..Accident_Index                                         0.000
    ## Vehicle_Reference                                         0.000
    ## Vehicle_Type                                              0.016
    ## Towing_and_Articulation                                   0.006
    ## Vehicle_Manoeuvre                                         0.036
    ## Vehicle_Location.Restricted_Lane                          0.002
    ## Junction_Location                                         0.329
    ## Skidding_and_Overturning                                  0.004
    ## Hit_Object_in_Carriageway                                 0.003
    ## Vehicle_Leaving_Carriageway                               0.004
    ## Hit_Object_off_Carriageway                                0.002
    ## X1st_Point_of_Impact                                      0.019
    ## Was_Vehicle_Left_Hand_Drive.                              0.767
    ## Journey_Purpose_of_Driver                                 1.495
    ## Sex_of_Driver                                             0.001
    ## Age_of_Driver                                            11.004
    ## Age_Band_of_Driver                                       11.004
    ## Engine_Capacity_.CC.                                     26.244
    ## Propulsion_Code                                          25.629
    ## Age_of_Vehicle                                           29.123
    ## Driver_IMD_Decile                                        28.289
    ## Driver_Home_Area_Type                                    19.591

Se observa que en la base de datos "accidents" la mayor?a de -1 se
encuentran en las variables **Junction\_Control** y
**X2nd\_Road\_Class** (35.7% y 41.2% de sus datos son -1
respectivamente). En el caso de la base de datos "casualties" los tiene
casi todos la variable **Pedestrian\_Road\_Maintenance\_Worker**
(64.9%). Por ?ltimo, la base de datos "vehicles" est?n m?s repartidos
siendo **Age\_of\_Vehicle** (29.1%) y **Driver\_IMD\_Decile** (28.3%)
las que m?s -1 contienen.

Dados los resultados, la cantidad de datos omitidos y tan centrados en
ciertas variables puede llegar a generar distorsiones, por lo que se
realizar?n dos modelos de subgrupos: en el primero se eliminan los
registros, es decir, se limpia la base de datos borrando todos los -1
(esto lleva a eliminar multitud de filas, donde se puede borrar
informaci?n interesante). En el segundo modelo se eliminan las variables
con mayor porcentaje de -1. Una vez descartadas las variables, se limpia
la base datos de los -1 restantes.

Finalmente, se comparar?n las primeras reglas que aparecen en los
subgrupos de cada base datos para determinar cuales ofrecen mayor
calidad y exactitud (p = accuracy).

**SUBGRUPOS ELIMINANDO REGISTROS**
----------------------------------

LIMPIEZA DE DATOS
-----------------

Como hemos comentado, el valor "-1" se exporta para valores NULL o fuera
de rango", por lo tanto, se eliminan estos registros. A pesar que se
reducen las bases de datos, contamos con un volumen considerable para
realizar el an?lisis.

    ## [1] "Se comprueba si quedan -1:"

    ##  [1] ï..Accident_Index                          
    ##  [2] Location_Easting_OSGR                      
    ##  [3] Location_Northing_OSGR                     
    ##  [4] Longitude                                  
    ##  [5] Latitude                                   
    ##  [6] Police_Force                               
    ##  [7] Accident_Severity                          
    ##  [8] Number_of_Vehicles                         
    ##  [9] Number_of_Casualties                       
    ## [10] Date                                       
    ## [11] Day_of_Week                                
    ## [12] Time                                       
    ## [13] Local_Authority_.District.                 
    ## [14] Local_Authority_.Highway.                  
    ## [15] X1st_Road_Class                            
    ## [16] X1st_Road_Number                           
    ## [17] Road_Type                                  
    ## [18] Speed_limit                                
    ## [19] Junction_Detail                            
    ## [20] Junction_Control                           
    ## [21] X2nd_Road_Class                            
    ## [22] X2nd_Road_Number                           
    ## [23] Pedestrian_Crossing.Human_Control          
    ## [24] Pedestrian_Crossing.Physical_Facilities    
    ## [25] Light_Conditions                           
    ## [26] Weather_Conditions                         
    ## [27] Road_Surface_Conditions                    
    ## [28] Special_Conditions_at_Site                 
    ## [29] Carriageway_Hazards                        
    ## [30] Urban_or_Rural_Area                        
    ## [31] Did_Police_Officer_Attend_Scene_of_Accident
    ## [32] LSOA_of_Accident_Location                  
    ## <0 rows> (or 0-length row.names)

    ## [1] 961886     32

    ## [1] "Se comprueba si quedan -1:"

    ##  [1] ï..Accident_Index                  Vehicle_Reference                 
    ##  [3] Casualty_Reference                 Casualty_Class                    
    ##  [5] Sex_of_Casualty                    Age_of_Casualty                   
    ##  [7] Age_Band_of_Casualty               Casualty_Severity                 
    ##  [9] Pedestrian_Location                Pedestrian_Movement               
    ## [11] Car_Passenger                      Bus_or_Coach_Passenger            
    ## [13] Pedestrian_Road_Maintenance_Worker Casualty_Type                     
    ## [15] Casualty_Home_Area_Type           
    ## <0 rows> (or 0-length row.names)

    ## [1] 681643     15

    ## [1] "Se comprueba si quedan -1:"

    ##  [1] ï..Accident_Index                Vehicle_Reference               
    ##  [3] Vehicle_Type                     Towing_and_Articulation         
    ##  [5] Vehicle_Manoeuvre                Vehicle_Location.Restricted_Lane
    ##  [7] Junction_Location                Skidding_and_Overturning        
    ##  [9] Hit_Object_in_Carriageway        Vehicle_Leaving_Carriageway     
    ## [11] Hit_Object_off_Carriageway       X1st_Point_of_Impact            
    ## [13] Was_Vehicle_Left_Hand_Drive.     Journey_Purpose_of_Driver       
    ## [15] Sex_of_Driver                    Age_of_Driver                   
    ## [17] Age_Band_of_Driver               Engine_Capacity_.CC.            
    ## [19] Propulsion_Code                  Age_of_Vehicle                  
    ## [21] Driver_IMD_Decile                Driver_Home_Area_Type           
    ## <0 rows> (or 0-length row.names)

    ## [1] 1554246      22

SELECCI?N Y TRANSFORMACI?N DE VARIABLES
---------------------------------------

Se descartan de antemano algunas variables con las que no se va a
trabajar, como pueden ser indicadores de registro y de
geoposicionamiento.

    Accidents <- Accidents[, -c(2,3,4,5,12,13,14,32)] 
    dim(Accidents)

    ## [1] 961886     24

    Casualties <- Casualties[, -c(2,3)]
    dim(Casualties)

    ## [1] 681643     13

    Vehicles <- Vehicles[, -c(2)]
    dim(Vehicles)

    ## [1] 1554246      21

Adem?s, ha sido necesario factorizar gunas variables para poder realizar
el an?lisis.

DESCUBRIMIENTO DE SUBGRUPOS
---------------------------

Se han analizado las bases de datos de forma independiente y en muestras
aleatorias de ~ 30.000 casos.

Se toma la **medida de calidad "lift"**, que es una medida de
correlaci?n que denota inter?s. Si el valor es mayor a 1, las variables
estan correlacionadas positivamente con la variable clase. Tambi?n se
fija un tama?o m?nimo de los subgrupos detectados en 1000 (en algunos
casos inferior), ya que sin esta restricci?n aparecen subgrupos con alta
calidad y probabilidad pero solo con un caso.

### A. ACCIDENTES

Se quieren detectar situaciones recurrentes en las que se da un
accidente mortal, grave o leve con la finalidad de poder mejorar las
advertencias de seguridad vial. Para ello tomamos **"Accident
Severity"** como variable clase.

    set.seed(1)
    n_muestra_accidents <- nrow(Accidents) *0.0309
    indices <- sample(1:nrow(Accidents), size = n_muestra_accidents)
    sample <- Accidents[indices, ]

Accidentes mortales:

    result1 <- DiscoverSubgroups(sample, as.target("Accident_Severity", "1"), new("SDTaskConfig", qf = "lift",  minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result1)

    ##    quality    p size
    ## 1     3.49 0.03 1296
    ## 2     3.42 0.03 1359
    ## 3     3.28 0.03 1378
    ## 4     3.23 0.02 2160
    ## 5     3.20 0.02 1453
    ## 6     3.05 0.02 2289
    ## 7     2.85 0.02 1314
    ## 8     2.84 0.02 1002
    ## 9     2.73 0.02 2701
    ## 10    2.56 0.02 2931
    ## 11    2.41 0.02 1558
    ## 12    2.34 0.02 1103
    ## 13    2.26 0.02 1085
    ## 14    2.25 0.02 1662
    ## 15    2.24 0.02 3114
    ## 16    2.24 0.02 3406
    ## 17    2.13 0.02 1030
    ## 18    2.12 0.02 1946
    ## 19    2.10 0.02 1045
    ## 20    2.06 0.02 1065
    ##                                                                 description
    ## 1     Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2, Road_Type=6
    ## 2                  Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2
    ## 3                            Speed_limit=60, Junction_Detail=3, Road_Type=6
    ## 4                        Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 5                                         Speed_limit=60, Junction_Detail=3
    ## 6                                               Speed_limit=60, Road_Type=6
    ## 7                                                    Number_of_Casualties=3
    ## 8                                       Number_of_Casualties=3, Road_Type=6
    ## 9                                     Speed_limit=60, Urban_or_Rural_Area=2
    ## 10                                                           Speed_limit=60
    ## 11 Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2
    ## 12                                        Speed_limit=40, Junction_Detail=3
    ## 13               Number_of_Casualties=2, Urban_or_Rural_Area=2, Road_Type=6
    ## 14                        Speed_limit=60, Number_of_Vehicles=2, Road_Type=6
    ## 15                    Urban_or_Rural_Area=2, Junction_Detail=3, Road_Type=6
    ## 16                                 Urban_or_Rural_Area=2, Junction_Detail=3
    ## 17                  Speed_limit=60, Junction_Detail=3, Number_of_Vehicles=2
    ## 18              Speed_limit=60, Number_of_Vehicles=2, Urban_or_Rural_Area=2
    ## 19                                  Number_of_Vehicles=3, Junction_Detail=3
    ## 20      Urban_or_Rural_Area=2, Road_Surface_Conditions=2, Junction_Detail=3

Algunos subgrupos detectados son: \* Ocurren a velocidades de 60 mph
(aprox. 96kph) en zonas rurales en carreteras sin divisiones f?sicas
entre los dos sentidos de circulaci?n en cruces T. \* Ocurren cuando el
n?mero de heridos son 3.

Accidentes graves:

    result2 <- DiscoverSubgroups(sample, as.target("Accident_Severity", "2"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result2)

    ##    quality    p size
    ## 1     1.52 0.18 1558
    ## 2     1.50 0.18 1662
    ## 3     1.48 0.17 2160
    ## 4     1.46 0.17 2289
    ## 5     1.46 0.17 1030
    ## 6     1.43 0.17 1296
    ## 7     1.43 0.17 1359
    ## 8     1.43 0.17 1378
    ## 9     1.41 0.17 1453
    ## 10    1.40 0.16 1946
    ## 11    1.39 0.16 2701
    ## 12    1.36 0.16 2124
    ## 13    1.35 0.16 2931
    ## 14    1.31 0.15 3406
    ## 15    1.31 0.15 3114
    ## 16    1.30 0.15 4954
    ## 17    1.30 0.15 1204
    ## 18    1.29 0.15 2946
    ## 19    1.29 0.15 1071
    ## 20    1.27 0.15 2194
    ##                                                                                      description
    ## 1                       Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2
    ## 2                                              Speed_limit=60, Number_of_Vehicles=2, Road_Type=6
    ## 3                                             Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 4                                                                    Speed_limit=60, Road_Type=6
    ## 5                                        Speed_limit=60, Junction_Detail=3, Number_of_Vehicles=2
    ## 6                          Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2, Road_Type=6
    ## 7                                       Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2
    ## 8                                                 Speed_limit=60, Junction_Detail=3, Road_Type=6
    ## 9                                                              Speed_limit=60, Junction_Detail=3
    ## 10                                   Speed_limit=60, Number_of_Vehicles=2, Urban_or_Rural_Area=2
    ## 11                                                         Speed_limit=60, Urban_or_Rural_Area=2
    ## 12                                                          Speed_limit=60, Number_of_Vehicles=2
    ## 13                                                                                Speed_limit=60
    ## 14                                                      Urban_or_Rural_Area=2, Junction_Detail=3
    ## 15                                         Urban_or_Rural_Area=2, Junction_Detail=3, Road_Type=6
    ## 16                                                            Urban_or_Rural_Area=2, Road_Type=6
    ## 17                 Light_Conditions=4, Road_Surface_Conditions=2, Junction_Detail=3, Road_Type=6
    ## 18                                            Light_Conditions=4, Junction_Detail=3, Road_Type=6
    ## 19 Light_Conditions=4, Road_Surface_Conditions=2, Junction_Detail=3, Speed_limit=30, Road_Type=6
    ## 20                   Urban_or_Rural_Area=2, Junction_Detail=3, Number_of_Vehicles=2, Road_Type=6

Algunos subgrupos detectados son: \* Ocurren cuando hay 2 veh?culos
implicados a velocidades de 60 mph (aprox. 96kph) en zona rural en
carreteras sin divisiones f?sicas entre los dos sentidos de circulaci?n.
\* Ocurren a velocidades de 60 mph en intersecciones en forma de T en
carreteras sin divisiones f?sicas entre los dos sentidos de circulaci?n
en zona rural. \* Ocurren de noche en calzadas mojadas en intersecciones
en forma de T en carreteras sin divisiones f?sicas entre los dos
sentidos de circulaci?n en zona rural.

Accidentes leves:

    result3 <- DiscoverSubgroups(sample, as.target("Accident_Severity", "3"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result3)

    ##    quality    p size
    ## 1     1.07 0.94 1911
    ## 2     1.07 0.94 3326
    ## 3     1.07 0.93 1255
    ## 4     1.06 0.93 1037
    ## 5     1.05 0.92 1221
    ## 6     1.05 0.92 1907
    ## 7     1.05 0.92 2037
    ## 8     1.05 0.92 3914
    ## 9     1.05 0.92 1291
    ## 10    1.05 0.92 2481
    ## 11    1.05 0.92 1773
    ## 12    1.05 0.92 1076
    ## 13    1.05 0.92 1794
    ## 14    1.05 0.92 1036
    ## 15    1.05 0.92 1861
    ## 16    1.05 0.92 4364
    ## 17    1.05 0.92 1494
    ## 18    1.05 0.92 3027
    ## 19    1.04 0.91 2437
    ## 20    1.04 0.91 1111
    ##                                                                                        description
    ## 1                                          Junction_Detail=1, Speed_limit=30, Number_of_Vehicles=2
    ## 2                                                          Junction_Detail=1, Number_of_Vehicles=2
    ## 3                                   Junction_Detail=1, Urban_or_Rural_Area=2, Number_of_Vehicles=2
    ## 4                                                Road_Type=3, Speed_limit=30, Number_of_Vehicles=2
    ## 5                                                     Junction_Detail=1, Road_Surface_Conditions=2
    ## 6                                              Day_of_Week=7, Number_of_Vehicles=2, Speed_limit=30
    ## 7               Road_Surface_Conditions=2, Junction_Detail=3, Number_of_Vehicles=2, Speed_limit=30
    ## 8                                  Road_Surface_Conditions=2, Number_of_Vehicles=2, Speed_limit=30
    ## 9                                                             Number_of_Vehicles=3, Speed_limit=30
    ## 10                                                               Junction_Detail=1, Speed_limit=30
    ## 11           Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30, Road_Surface_Conditions=2
    ## 12                                               Number_of_Vehicles=3, Speed_limit=30, Road_Type=6
    ## 13                                      Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30
    ## 14                          Day_of_Week=7, Junction_Detail=3, Number_of_Vehicles=2, Speed_limit=30
    ## 15 Road_Surface_Conditions=2, Junction_Detail=3, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 16                                                                               Junction_Detail=1
    ## 17                                Day_of_Week=7, Number_of_Vehicles=2, Road_Type=6, Speed_limit=30
    ## 18                    Road_Surface_Conditions=2, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 19                                             Day_of_Week=5, Number_of_Vehicles=2, Speed_limit=30
    ## 20                         Number_of_Casualties=2, Road_Surface_Conditions=2, Number_of_Vehicles=2

Algunos subgrupos detectados son: \* Ocurren en rotondas a 30 mph y hay
dos vehiculos implicados. \* Ocurren en s?bado a 30 mph y hay dos
vehiculos implicados. \* Ocurren en d?as de lluvia o cuando la calzada
est? ligeramente h?meda, a 30 mph y hay dos coches implicados.

### B. VEHICLES

Se quieren detectar situaciones recurrentes seg?n el veh?culo
accidentado poder mejorar las advertencias de seguridad vial. Para ello
tomamos **"Vehicle Type"** como variable clase.

    set.seed(1)
    n_muestra_vehicles <- nrow(Vehicles)*0.0213
    indices <- sample(1:nrow(Vehicles), size = n_muestra_vehicles)
    sample_v <- Vehicles[indices, ]

Nota: variables como Propulsion Code = Petrol,
Journey\_Purpose\_of\_Driver=Other/Not known (2005-10) aparecen siempre
en los subgrupos detectados y es debido, como hemos comprobado en la
exploraci?n de datos, a que son clase mayoritaria. A modo de prueba, los
vamos a excluir de antemano en este an?lisis.

Accidentes de coche:

    result4 <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "9"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result4)

    ##    quality    p  size
    ## 1     1.23 0.98  1112
    ## 2     1.23 0.98  1285
    ## 3     1.22 0.98  1583
    ## 4     1.22 0.97  1114
    ## 5     1.22 0.97  1438
    ## 6     1.21 0.97  1663
    ## 7     1.21 0.96 11198
    ## 8     1.21 0.96  2619
    ## 9     1.21 0.96  2212
    ## 10    1.20 0.96  2440
    ## 11    1.20 0.96  4874
    ## 12    1.20 0.96  2766
    ## 13    1.20 0.95  1191
    ## 14    1.20 0.95  1179
    ## 15    1.19 0.95  1156
    ## 16    1.19 0.95  1027
    ## 17    1.18 0.94  1207
    ## 18    1.16 0.92  1397
    ## 19    1.12 0.89  1178
    ## 20    1.11 0.88  2669
    ##                                                    description
    ## 1                         Vehicle_Manoeuvre=3, Sex_of_Driver=2
    ## 2                         Vehicle_Manoeuvre=9, Sex_of_Driver=2
    ## 3                        Age_Band_of_Driver=5, Sex_of_Driver=2
    ## 4                     Driver_Home_Area_Type=2, Sex_of_Driver=2
    ## 5                     Driver_Home_Area_Type=3, Sex_of_Driver=2
    ## 6                        Age_Band_of_Driver=8, Sex_of_Driver=2
    ## 7                                              Sex_of_Driver=2
    ## 8                        Age_Band_of_Driver=6, Sex_of_Driver=2
    ## 9                         Junction_Location=8, Sex_of_Driver=2
    ## 10                       Age_Band_of_Driver=7, Sex_of_Driver=2
    ## 11                       Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 12                        Junction_Location=1, Sex_of_Driver=2
    ## 13  Junction_Location=8, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 14 Age_Band_of_Driver=6, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 15  Junction_Location=1, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 16                 Skidding_and_Overturning=1, Sex_of_Driver=2
    ## 17                       Age_Band_of_Driver=4, Sex_of_Driver=2
    ## 18                                       Age_Band_of_Driver=10
    ## 19                    Vehicle_Manoeuvre=3, Junction_Location=1
    ## 20                                         Vehicle_Manoeuvre=3

Algunos subgrupos detectados son: \* Mujeres de entre 21 - 25 a?os que
viven en peque?as ciudades y zonas rurales que estan esperando para
salir o girando a la derecha. \* Mujeres entre 26 - 45 a?os.

Accidentes de moto de alta cilindrada &gt; 500cc:

    result5 <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "5"), new("SDTaskConfig", qf = "lift", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result5)

    ##    quality    p size
    ## 1     5.40 0.20  670
    ## 2     3.82 0.14  667
    ## 3     3.66 0.13  360
    ## 4     3.53 0.13  528
    ## 5     3.51 0.13  219
    ## 6     3.47 0.13  372
    ## 7     3.37 0.12  440
    ## 8     3.32 0.12  289
    ## 9     3.30 0.12  216
    ## 10    3.16 0.12  452
    ## 11    3.01 0.11  273
    ## 12    2.98 0.11  267
    ## 13    2.75 0.10  439
    ## 14    2.62 0.10  408
    ## 15    2.60 0.09  822
    ## 16    2.51 0.09  371
    ## 17    2.50 0.09  241
    ## 18    2.48 0.09  740
    ## 19    2.44 0.09  718
    ## 20    2.42 0.09  295
    ##                                                               description
    ## 1                                                    Vehicle_Manoeuvre=13
    ## 2                        Skidding_and_Overturning=1, Age_Band_of_Driver=7
    ## 3   Skidding_and_Overturning=1, Junction_Location=8, Vehicle_Manoeuvre=18
    ## 4                         Skidding_and_Overturning=1, Junction_Location=8
    ## 5                              Vehicle_Manoeuvre=16, Age_Band_of_Driver=7
    ## 6  Skidding_and_Overturning=1, Age_Band_of_Driver=7, Vehicle_Manoeuvre=18
    ## 7                                                    Vehicle_Manoeuvre=14
    ## 8  Skidding_and_Overturning=1, Age_Band_of_Driver=8, Vehicle_Manoeuvre=18
    ## 9                        Age_Band_of_Driver=9, Skidding_and_Overturning=1
    ## 10                       Skidding_and_Overturning=1, Age_Band_of_Driver=8
    ## 11                             Vehicle_Manoeuvre=16, Age_Band_of_Driver=6
    ## 12                             Vehicle_Manoeuvre=17, Age_Band_of_Driver=7
    ## 13                    Driver_Home_Area_Type=2, Skidding_and_Overturning=1
    ## 14                       Vehicle_Manoeuvre=16, Skidding_and_Overturning=1
    ## 15                       Skidding_and_Overturning=1, Age_Band_of_Driver=6
    ## 16                       Vehicle_Manoeuvre=17, Skidding_and_Overturning=1
    ## 17     Driver_Home_Area_Type=2, Junction_Location=8, Vehicle_Manoeuvre=18
    ## 18                        Skidding_and_Overturning=1, Junction_Location=1
    ## 19                                             Skidding_and_Overturning=2
    ## 20                             Vehicle_Manoeuvre=17, Age_Band_of_Driver=6

Algunos subgrupos detectados son: \* Motos de alta cilindrada realizando
adelantamientos. \* Motos conducidas por personas de entre 36 - 45 a?os
y derrapan.

Accidentes de moto de baja cilindrada &lt;= 50cc

    result6 <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "2"), new("SDTaskConfig", qf = "lift", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result6)

    ##    quality    p size
    ## 1     9.02 0.13  439
    ## 2     7.81 0.12  215
    ## 3     7.27 0.11  434
    ## 4     7.16 0.11  403
    ## 5     7.15 0.11  733
    ## 6     6.98 0.10 1858
    ## 7     6.92 0.10  262
    ## 8     6.91 0.10  748
    ## 9     6.90 0.10  827
    ## 10    6.88 0.10  449
    ## 11    6.03 0.09 3695
    ## 12    5.67 0.08  533
    ## 13    5.39 0.08  249
    ## 14    4.45 0.07  332
    ## 15    3.50 0.05  461
    ## 16    3.45 0.05  740
    ## 17    3.36 0.05  440
    ## 18    3.06 0.05  263
    ## 19    3.01 0.04  290
    ## 20    2.79 0.04  337
    ##                                                               description
    ## 1         Age_Band_of_Driver=4, Junction_Location=8, Vehicle_Manoeuvre=18
    ## 2     Driver_Home_Area_Type=2, Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 3         Age_Band_of_Driver=4, Junction_Location=1, Vehicle_Manoeuvre=18
    ## 4  Skidding_and_Overturning=1, Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 5                               Age_Band_of_Driver=4, Junction_Location=8
    ## 6                              Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 7     Driver_Home_Area_Type=3, Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 8                        Skidding_and_Overturning=1, Age_Band_of_Driver=4
    ## 9                               Age_Band_of_Driver=4, Junction_Location=1
    ## 10                          Driver_Home_Area_Type=2, Age_Band_of_Driver=4
    ## 11                                                   Age_Band_of_Driver=4
    ## 12                          Driver_Home_Area_Type=3, Age_Band_of_Driver=4
    ## 13                              Vehicle_Manoeuvre=4, Age_Band_of_Driver=4
    ## 14                              Vehicle_Manoeuvre=9, Age_Band_of_Driver=4
    ## 15  Skidding_and_Overturning=1, Junction_Location=1, Vehicle_Manoeuvre=18
    ## 16                        Skidding_and_Overturning=1, Junction_Location=1
    ## 17                                                   Vehicle_Manoeuvre=14
    ## 18             Age_Band_of_Driver=4, Junction_Location=1, Sex_of_Driver=2
    ## 19                             Vehicle_Manoeuvre=17, Age_Band_of_Driver=4
    ## 20                        Vehicle_Manoeuvre=4, Skidding_and_Overturning=1

Algunos subgrupos detectados son: \* Motoristas de entre 16 - 20 a?os
yendo detras de otro veh?culo en zona urbana en rotodas o carreteras
principales o acerc?ndose a cruces.

### C. CASUALTIES

Se quieren detectar situaciones recurrentes en las que se da un herido
mortal, grave o leve con la finalidad de poder mejorar las advertencias
de seguridad vial. Para ello tomamos **"Casualty Severity"** como
variable clase.

En esta base datos vamos a probar el **algoritmo SDMap**.

    set.seed(1)
    n_muestra_casualties <- nrow(Casualties)*0.0506
    indices <- sample(1:nrow(Casualties), size = n_muestra_casualties)
    sample_c <- Casualties[indices, ]

Heridos mortales:

    result7 <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "1"), new("SDTaskConfig", qf = "lift", minsize = 100, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result7)

    ##    quality    p size
    ## 1     8.90 0.08  156
    ## 2     8.90 0.08  156
    ## 3     8.08 0.08  119
    ## 4     8.08 0.08  119
    ## 5     6.85 0.06  187
    ## 6     6.83 0.06  297
    ## 7     6.67 0.06  160
    ## 8     6.51 0.06  164
    ## 9     6.15 0.06  191
    ## 10    6.15 0.06  191
    ## 11    5.90 0.06  163
    ## 12    5.67 0.05  113
    ## 13    5.05 0.05  148
    ## 14    4.97 0.05  172
    ## 15    4.50 0.04  261
    ## 16    4.46 0.04 1292
    ## 17    4.43 0.04  193
    ## 18    4.36 0.04  245
    ## 19    4.30 0.04  149
    ## 20    4.24 0.04  126
    ##                                                                      description
    ## 1                                 Age_Band_of_Casualty=11, Pedestrian_Location=5
    ## 2               Age_Band_of_Casualty=11, Pedestrian_Location=5, Casualty_Class=3
    ## 3                                 Age_Band_of_Casualty=11, Pedestrian_Movement=1
    ## 4               Age_Band_of_Casualty=11, Pedestrian_Movement=1, Casualty_Class=3
    ## 5                             Age_Band_of_Casualty=11, Casualty_Home_Area_Type=3
    ## 6                                      Age_Band_of_Casualty=11, Casualty_Class=3
    ## 7                                     Casualty_Type=5, Casualty_Home_Area_Type=3
    ## 8            Age_Band_of_Casualty=11, Casualty_Home_Area_Type=3, Casualty_Type=9
    ## 9                                  Pedestrian_Location=10, Pedestrian_Movement=9
    ## 10               Pedestrian_Location=10, Pedestrian_Movement=9, Casualty_Class=3
    ## 11                            Age_Band_of_Casualty=11, Casualty_Home_Area_Type=2
    ## 12           Age_Band_of_Casualty=11, Casualty_Home_Area_Type=2, Casualty_Type=9
    ## 13                                       Casualty_Type=5, Age_Band_of_Casualty=9
    ## 14                  Age_Band_of_Casualty=11, Casualty_Class=3, Sex_of_Casualty=2
    ## 15                            Age_Band_of_Casualty=10, Casualty_Home_Area_Type=3
    ## 16                                                       Age_Band_of_Casualty=11
    ## 17 Age_Band_of_Casualty=11, Casualty_Class=2, Casualty_Type=9, Sex_of_Casualty=2
    ## 18                    Age_Band_of_Casualty=11, Casualty_Class=2, Casualty_Type=9
    ## 19                                    Casualty_Type=5, Casualty_Home_Area_Type=2
    ## 20               Pedestrian_Movement=9, Age_Band_of_Casualty=7, Casualty_Class=3

Algunos subgrupos detectados son: \* Peatones mayores de 75 a?os
cruzando la calle en zonas rurales. \* Motoristas de alta cilindrada en
zonas rurales. \* Ocupantes de coche mayores de 75 a?os de zonas
rurales.

Heridos graves:

    result8 <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "2"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result8)

    ##    quality    p size                                  description
    ## 1     2.81 0.33 1444                              Casualty_Type=5
    ## 2     1.97 0.23 1979                        Pedestrian_Location=5
    ## 3     1.97 0.23 1979      Pedestrian_Location=5, Casualty_Class=3
    ## 4     1.95 0.23 1356                              Casualty_Type=3
    ## 5     1.88 0.22 1416                        Pedestrian_Movement=1
    ## 6     1.88 0.22 1416      Pedestrian_Movement=1, Casualty_Class=3
    ## 7     1.80 0.21 4241                             Casualty_Class=3
    ## 8     1.76 0.21 1292                      Age_Band_of_Casualty=11
    ## 9     1.71 0.20 1905          Casualty_Class=3, Sex_of_Casualty=2
    ## 10    1.42 0.17 3429                              Casualty_Type=1
    ## 11    1.35 0.16 1508                      Age_Band_of_Casualty=10
    ## 12    1.23 0.15 3214                    Casualty_Home_Area_Type=2
    ## 13    1.21 0.14 3977                    Casualty_Home_Area_Type=3
    ## 14    1.19 0.14 1414                       Age_Band_of_Casualty=3
    ## 15    1.10 0.13 2644                       Age_Band_of_Casualty=9
    ## 16    1.04 0.12 4694                       Age_Band_of_Casualty=8
    ## 17    0.99 0.12 1796 Casualty_Home_Area_Type=3, Sex_of_Casualty=2
    ## 18    0.95 0.11 2863   Casualty_Home_Area_Type=3, Casualty_Type=9
    ## 19    0.94 0.11 4459                       Age_Band_of_Casualty=4
    ## 20    0.91 0.11 1397 Casualty_Home_Area_Type=2, Sex_of_Casualty=2

Algunos subgrupos detectados son: \* Motoristas en moto de cilindrada
&gt; 500cc y &gt; 125cc. \* Peatones cruzando la calle. \* Peatones
cruzando desde el lado del conductor.

Heridos leves:

    result9 <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "3"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result9)

    ##    quality    p  size
    ## 1     1.10 0.96  1985
    ## 2     1.09 0.95  1688
    ## 3     1.09 0.95  1465
    ## 4     1.08 0.94  3298
    ## 5     1.08 0.94  4127
    ## 6     1.08 0.94  1509
    ## 7     1.07 0.94 10393
    ## 8     1.07 0.94  1316
    ## 9     1.07 0.93  2458
    ## 10    1.07 0.93  2458
    ## 11    1.07 0.93  2505
    ## 12    1.07 0.93  2505
    ## 13    1.07 0.93  2774
    ## 14    1.07 0.93  2161
    ## 15    1.07 0.93  3578
    ## 16    1.06 0.93  1880
    ## 17    1.06 0.93  1061
    ## 18    1.06 0.93  2605
    ## 19    1.06 0.93  3927
    ## 20    1.06 0.93  3927
    ##                                                              description
    ## 1             Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9
    ## 2             Age_Band_of_Casualty=7, Sex_of_Casualty=2, Casualty_Type=9
    ## 3             Age_Band_of_Casualty=8, Sex_of_Casualty=2, Casualty_Type=9
    ## 4                                Age_Band_of_Casualty=7, Casualty_Type=9
    ## 5                                Age_Band_of_Casualty=6, Casualty_Type=9
    ## 6             Age_Band_of_Casualty=5, Sex_of_Casualty=2, Casualty_Type=9
    ## 7                                     Sex_of_Casualty=2, Casualty_Type=9
    ## 8             Age_Band_of_Casualty=4, Sex_of_Casualty=2, Casualty_Type=9
    ## 9  Car_Passenger=1, Sex_of_Casualty=2, Casualty_Class=2, Casualty_Type=9
    ## 10                   Car_Passenger=1, Sex_of_Casualty=2, Casualty_Type=9
    ## 11                                    Car_Passenger=1, Sex_of_Casualty=2
    ## 12                  Car_Passenger=1, Sex_of_Casualty=2, Casualty_Class=2
    ## 13                               Age_Band_of_Casualty=8, Casualty_Type=9
    ## 14                             Age_Band_of_Casualty=7, Sex_of_Casualty=2
    ## 15                  Casualty_Class=2, Sex_of_Casualty=2, Casualty_Type=9
    ## 16                             Age_Band_of_Casualty=5, Sex_of_Casualty=2
    ## 17                              Age_Band_of_Casualty=6, Casualty_Class=2
    ## 18                             Age_Band_of_Casualty=6, Sex_of_Casualty=2
    ## 19                                      Car_Passenger=1, Casualty_Type=9
    ## 20                    Car_Passenger=1, Casualty_Type=9, Casualty_Class=2

Algunos subgrupos detectados son: \* Mujeres ocupantes de coche entre 21
y 55 a?os sentadas en el asiento delantero.

**SUBGRUPOS ELIMINANDO VARIABLES**
----------------------------------

LIMPIEZA DE DATOS
-----------------

En este apartado vamos a repetir el anterior an?lisis, pero eliminando
primero aquellas variables que agrupan mayores valores -1 y a
continuaci?n el resto de -1 presentes a?n en la base de datos, con el
objetivo de mantener m?s registros para el an?lisis.

    ##  [1] ï..Accident_Index                          
    ##  [2] Location_Easting_OSGR                      
    ##  [3] Location_Northing_OSGR                     
    ##  [4] Longitude                                  
    ##  [5] Latitude                                   
    ##  [6] Police_Force                               
    ##  [7] Accident_Severity                          
    ##  [8] Number_of_Vehicles                         
    ##  [9] Number_of_Casualties                       
    ## [10] Date                                       
    ## [11] Day_of_Week                                
    ## [12] Time                                       
    ## [13] Local_Authority_.District.                 
    ## [14] Local_Authority_.Highway.                  
    ## [15] X1st_Road_Class                            
    ## [16] X1st_Road_Number                           
    ## [17] Road_Type                                  
    ## [18] Speed_limit                                
    ## [19] Junction_Detail                            
    ## [20] X2nd_Road_Number                           
    ## [21] Pedestrian_Crossing.Human_Control          
    ## [22] Pedestrian_Crossing.Physical_Facilities    
    ## [23] Light_Conditions                           
    ## [24] Weather_Conditions                         
    ## [25] Road_Surface_Conditions                    
    ## [26] Special_Conditions_at_Site                 
    ## [27] Carriageway_Hazards                        
    ## [28] Urban_or_Rural_Area                        
    ## [29] Did_Police_Officer_Attend_Scene_of_Accident
    ## [30] LSOA_of_Accident_Location                  
    ## <0 rows> (or 0-length row.names)

    ## [1] 1621768      30

    ##  [1] ï..Accident_Index       Vehicle_Reference      
    ##  [3] Casualty_Reference      Casualty_Class         
    ##  [5] Sex_of_Casualty         Age_of_Casualty        
    ##  [7] Age_Band_of_Casualty    Casualty_Severity      
    ##  [9] Pedestrian_Location     Pedestrian_Movement    
    ## [11] Car_Passenger           Bus_or_Coach_Passenger 
    ## [13] Casualty_Type           Casualty_Home_Area_Type
    ## <0 rows> (or 0-length row.names)

    ## [1] 1870148      14

    ##  [1] ï..Accident_Index                Vehicle_Reference               
    ##  [3] Vehicle_Type                     Towing_and_Articulation         
    ##  [5] Vehicle_Manoeuvre                Vehicle_Location.Restricted_Lane
    ##  [7] Junction_Location                Skidding_and_Overturning        
    ##  [9] Hit_Object_in_Carriageway        Vehicle_Leaving_Carriageway     
    ## [11] Hit_Object_off_Carriageway       X1st_Point_of_Impact            
    ## [13] Was_Vehicle_Left_Hand_Drive.     Journey_Purpose_of_Driver       
    ## [15] Sex_of_Driver                    Age_of_Driver                   
    ## [17] Age_Band_of_Driver               Engine_Capacity_.CC.            
    ## [19] Propulsion_Code                  Driver_Home_Area_Type           
    ## <0 rows> (or 0-length row.names)

    ## [1] 1832606      20

SELECCI?N Y TRANSFORMACI?N DE VARIABLES
---------------------------------------

Se descartan las mismas variables que en el anterior an?lisis y se
factorizan algunas de ellas.

DESCUBRIMIENTO DE SUBGRUPOS
---------------------------

Se han analizado las bases de datos de forma independiente y en muestras
aleatorias de ~ 30.000 casos.

Se toma la **medida de calidad "lift"**, que es una medida de
correlaci?n que denota inter?s. Si el valor es mayor a 1, las variables
estan correlacionadas positivamente con la variable clase. Tambi?n se
fija un tama?o m?nimo de los subgrupos detectados en 1000 (en algunos
casos inferior), ya que sin esta restricci?n aparecen subgrupos con alta
calidad y probabilidad pero solo con un caso.

### A. ACCIDENTES

Se quieren detectar situaciones recurrentes en las que se da un
accidente mortal, grave o leve con la finalidad de poder mejorar las
advertencias de seguridad vial. Para ello tomamos **"Accident
Severity"** como variable clase.

Accidentes mortales:

    result1_2 <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "1"), new("SDTaskConfig", qf = "lift",  minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result1_2)

    ##    quality    p size
    ## 1     4.04 0.05 1010
    ## 2     3.99 0.05 1023
    ## 3     3.89 0.05 1049
    ## 4     3.84 0.05 1064
    ## 5     3.73 0.05 1319
    ## 6     3.64 0.05 1684
    ## 7     3.57 0.05 1378
    ## 8     3.53 0.05 1776
    ## 9     2.75 0.04 4367
    ## 10    2.71 0.04 4526
    ## 11    2.58 0.03 1700
    ## 12    2.55 0.03 4826
    ## 13    2.51 0.03 1023
    ## 14    2.50 0.03 1752
    ## 15    2.48 0.03 5064
    ## 16    2.46 0.03 2246
    ## 17    2.44 0.03 1117
    ## 18    2.42 0.03 1840
    ## 19    2.41 0.03 2350
    ## 20    2.32 0.03 1920
    ##                                                                      description
    ## 1         Light_Conditions=6, Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 2                                Light_Conditions=6, Speed_limit=60, Road_Type=6
    ## 3                      Light_Conditions=6, Speed_limit=60, Urban_or_Rural_Area=2
    ## 4                                             Light_Conditions=6, Speed_limit=60
    ## 5                         Light_Conditions=6, Road_Type=6, Urban_or_Rural_Area=2
    ## 6                                      Light_Conditions=6, Urban_or_Rural_Area=2
    ## 7                                                Light_Conditions=6, Road_Type=6
    ## 8                                                             Light_Conditions=6
    ## 9                             Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 10                                                   Speed_limit=60, Road_Type=6
    ## 11 Speed_limit=60, Road_Surface_Conditions=2, Road_Type=6, Urban_or_Rural_Area=2
    ## 12                                         Speed_limit=60, Urban_or_Rural_Area=2
    ## 13                      Weather_Conditions=2, Urban_or_Rural_Area=2, Road_Type=6
    ## 14                        Speed_limit=60, Road_Surface_Conditions=2, Road_Type=6
    ## 15                                                                Speed_limit=60
    ## 16      Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2
    ## 17                             Day_of_Week=3, Urban_or_Rural_Area=2, Road_Type=6
    ## 18              Speed_limit=60, Road_Surface_Conditions=2, Urban_or_Rural_Area=2
    ## 19                             Speed_limit=60, Number_of_Vehicles=2, Road_Type=6
    ## 20                                     Speed_limit=60, Road_Surface_Conditions=2

Algunos subgrupos detectados son: \* Ocurren a velocidades de 60 mph
(aprox. 96kph) durante el d?a en carreteras sin divisiones f?sicas entre
los dos sentidos de circulaci?n y sin peligros en la calzada. \* Ocurren
cuando hay 2 veh?culos implicados a velocidades de 60 mph (aprox. 96kph)
durante el d?a en carreteras sin divisiones f?sicas entre los dos
sentidos de circulaci?n y sin peligros en la calzada.

Accidentes graves:

    result2_2 <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "2"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result2_2)

    ##    quality    p size
    ## 1     1.56 0.22 1103
    ## 2     1.46 0.20 2246
    ## 3     1.45 0.20 1548
    ## 4     1.45 0.20 1064
    ## 5     1.45 0.20 1049
    ## 6     1.44 0.20 1010
    ## 7     1.44 0.20 4367
    ## 8     1.44 0.20 1023
    ## 9     1.42 0.20 2350
    ## 10    1.42 0.20 4526
    ## 11    1.39 0.19 4826
    ## 12    1.39 0.19 2536
    ## 13    1.39 0.19 1319
    ## 14    1.37 0.19 1684
    ## 15    1.37 0.19 1378
    ## 16    1.36 0.19 5064
    ## 17    1.36 0.19 1776
    ## 18    1.34 0.18 2692
    ## 19    1.31 0.18 8039
    ## 20    1.27 0.17 1215
    ##                                                                 description
    ## 1                         Day_of_Week=7, Urban_or_Rural_Area=2, Road_Type=6
    ## 2  Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2
    ## 3                                      Day_of_Week=7, Urban_or_Rural_Area=2
    ## 4                                        Light_Conditions=6, Speed_limit=60
    ## 5                 Light_Conditions=6, Speed_limit=60, Urban_or_Rural_Area=2
    ## 6    Light_Conditions=6, Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 7                        Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2
    ## 8                           Light_Conditions=6, Speed_limit=60, Road_Type=6
    ## 9                         Speed_limit=60, Number_of_Vehicles=2, Road_Type=6
    ## 10                                              Speed_limit=60, Road_Type=6
    ## 11                                    Speed_limit=60, Urban_or_Rural_Area=2
    ## 12              Speed_limit=60, Number_of_Vehicles=2, Urban_or_Rural_Area=2
    ## 13                   Light_Conditions=6, Road_Type=6, Urban_or_Rural_Area=2
    ## 14                                Light_Conditions=6, Urban_or_Rural_Area=2
    ## 15                                          Light_Conditions=6, Road_Type=6
    ## 16                                                           Speed_limit=60
    ## 17                                                       Light_Conditions=6
    ## 18                                     Speed_limit=60, Number_of_Vehicles=2
    ## 19                                       Urban_or_Rural_Area=2, Road_Type=6
    ## 20                        Day_of_Week=5, Urban_or_Rural_Area=2, Road_Type=6

Algunos subgrupos detectados son: \* Ocurren cuando hay 2 veh?culos
implicados a velocidades de 60 mph (aprox. 96kph) durante el d?a en
carreteras sin divisiones f?sicas entre los dos sentidos de circulaci?n
y sin peligros en la calzada. \* Ocurren en s?bado en intersecciones en
forma de T en carreteras sin divisiones f?sicas entre los dos sentidos
de circulaci?n.

Accidentes leves:

    result3_2 <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "3"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result3_2)

    ##    quality    p size
    ## 1     1.09 0.93 2182
    ## 2     1.08 0.92 1254
    ## 3     1.08 0.91 1457
    ## 4     1.08 0.91 1474
    ## 5     1.08 0.91 2080
    ## 6     1.07 0.91 2821
    ## 7     1.07 0.91 1684
    ## 8     1.07 0.91 3358
    ## 9     1.07 0.91 1178
    ## 10    1.07 0.91 1191
    ## 11    1.07 0.91 1048
    ## 12    1.07 0.91 1595
    ## 13    1.07 0.91 2715
    ## 14    1.07 0.91 1860
    ## 15    1.07 0.90 1155
    ## 16    1.06 0.90 1844
    ## 17    1.06 0.90 1502
    ## 18    1.06 0.90 2097
    ## 19    1.06 0.90 1592
    ## 20    1.06 0.90 2012
    ##                                                                                           description
    ## 1                                                             Junction_Detail=1, Number_of_Vehicles=2
    ## 2                                             Junction_Detail=1, Speed_limit=30, Number_of_Vehicles=2
    ## 3               Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30, Road_Surface_Conditions=2
    ## 4                                          Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30
    ## 5                                                 Day_of_Week=3, Number_of_Vehicles=2, Speed_limit=30
    ## 6                                                                                   Junction_Detail=1
    ## 7                                    Day_of_Week=3, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 8                                     Road_Surface_Conditions=2, Speed_limit=30, Number_of_Vehicles=2
    ## 9  Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6, Road_Surface_Conditions=2
    ## 10                            Weather_Conditions=2, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 11                                             Day_of_Week=3, Junction_Detail=3, Number_of_Vehicles=2
    ## 12                                                                  Junction_Detail=1, Speed_limit=30
    ## 13                       Road_Surface_Conditions=2, Speed_limit=30, Number_of_Vehicles=2, Road_Type=6
    ## 14                                                Day_of_Week=2, Number_of_Vehicles=2, Speed_limit=30
    ## 15                                                           Junction_Detail=1, Urban_or_Rural_Area=2
    ## 16                                            Junction_Detail=6, Number_of_Vehicles=2, Speed_limit=30
    ## 17                                   Day_of_Week=2, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 18                                                Day_of_Week=6, Number_of_Vehicles=2, Speed_limit=30
    ## 19                               Junction_Detail=6, Number_of_Vehicles=2, Speed_limit=30, Road_Type=6
    ## 20                                                Day_of_Week=4, Number_of_Vehicles=2, Speed_limit=30

Algunos subgrupos detectados son: \* Ocurren en rotondas a 30 mph
durante el dia y hay dos coches implicados.

### B. VEHICLES

Se quieren detectar situaciones recurrentes seg?n el veh?culo
accidentado poder mejorar las advertencias de seguridad vial. Para ello
tomamos **"Vehicle Type"** como variable clase.

Nota: variables como Propulsion Code = Petrol,
Journey\_Purpose\_of\_Driver=Other/Not known (2005-10) aparecen siempre
en los subgrupos detectados y es debido, como hemos comprobado en la
exploraci?n de datos, a que son clase mayoritaria. A modo de prueba, los
vamos a excluir de antemano en este an?lisis.

Accidentes de coche:

    result4_2 <- DiscoverSubgroups(sample_v2, as.target("Vehicle_Type", "9"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver")))
    ToDataFrame(result4_2)

    ##    quality    p  size
    ## 1     1.24 0.99  1395
    ## 2     1.22 0.97  1240
    ## 3     1.22 0.97  1705
    ## 4     1.22 0.97  1075
    ## 5     1.21 0.97  3033
    ## 6     1.21 0.96  2794
    ## 7     1.21 0.96 12216
    ## 8     1.21 0.96  2365
    ## 9     1.21 0.96  1233
    ## 10    1.20 0.96  2780
    ## 11    1.20 0.96  5388
    ## 12    1.20 0.96  1232
    ## 13    1.20 0.96  1057
    ## 14    1.20 0.96  1177
    ## 15    1.20 0.95  1891
    ## 16    1.20 0.95  1202
    ## 17    1.20 0.95  1241
    ## 18    1.19 0.95  1069
    ## 19    1.16 0.93  1652
    ## 20    1.12 0.90  1299
    ##                                                    description
    ## 1                         Vehicle_Manoeuvre=9, Sex_of_Driver=2
    ## 2                         Vehicle_Manoeuvre=3, Sex_of_Driver=2
    ## 3                        Age_Band_of_Driver=5, Sex_of_Driver=2
    ## 4                        Age_Band_of_Driver=9, Sex_of_Driver=2
    ## 5                         Junction_Location=1, Sex_of_Driver=2
    ## 6                        Age_Band_of_Driver=6, Sex_of_Driver=2
    ## 7                                              Sex_of_Driver=2
    ## 8                         Junction_Location=8, Sex_of_Driver=2
    ## 9   Junction_Location=8, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 10                       Age_Band_of_Driver=7, Sex_of_Driver=2
    ## 11                       Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 12 Age_Band_of_Driver=6, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 13                        Vehicle_Manoeuvre=4, Sex_of_Driver=2
    ## 14 Age_Band_of_Driver=7, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 15                       Age_Band_of_Driver=8, Sex_of_Driver=2
    ## 16                       Age_Band_of_Driver=4, Sex_of_Driver=2
    ## 17  Junction_Location=1, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 18                 Skidding_and_Overturning=1, Sex_of_Driver=2
    ## 19                                       Age_Band_of_Driver=10
    ## 20                    Vehicle_Manoeuvre=3, Junction_Location=1

Algunos subgrupos detectados son: \* Aquellos conducidos por personas de
entre 66 - 75 a?os. \* Aquellos producidos al incorporarse en carreteras
principales

Accidentes de moto de alta cilindrada &gt; 500cc:

    result5_2 <- DiscoverSubgroups(sample_v2, as.target("Vehicle_Type", "5"), new("SDTaskConfig", qf = "lift", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver")))
    ToDataFrame(result5_2)

    ##    quality    p size
    ## 1     5.21 0.18  753
    ## 2     4.21 0.15  293
    ## 3     3.89 0.14  780
    ## 4     3.74 0.13  483
    ## 5     3.68 0.13  475
    ## 6     3.66 0.13  274
    ## 7     3.56 0.12  418
    ## 8     3.49 0.12  574
    ## 9     3.46 0.12  364
    ## 10    3.33 0.12  258
    ## 11    3.29 0.11  314
    ## 12    2.58 0.09  300
    ## 13    2.45 0.09  515
    ## 14    2.37 0.08  483
    ## 15    2.35 0.08  767
    ## 16    2.33 0.08 1523
    ## 17    2.22 0.08 4004
    ## 18    2.16 0.08  239
    ## 19    2.15 0.07 2229
    ## 20    2.14 0.07  871
    ##                                                               description
    ## 1                                                    Vehicle_Manoeuvre=13
    ## 2                              Vehicle_Manoeuvre=16, Age_Band_of_Driver=7
    ## 3                        Skidding_and_Overturning=1, Age_Band_of_Driver=7
    ## 4                        Skidding_and_Overturning=1, Age_Band_of_Driver=8
    ## 5  Skidding_and_Overturning=1, Age_Band_of_Driver=7, Vehicle_Manoeuvre=18
    ## 6  Skidding_and_Overturning=1, Age_Band_of_Driver=8, Vehicle_Manoeuvre=18
    ## 7                                                    Vehicle_Manoeuvre=14
    ## 8                         Skidding_and_Overturning=1, Junction_Location=8
    ## 9   Skidding_and_Overturning=1, Junction_Location=8, Vehicle_Manoeuvre=18
    ## 10                       Age_Band_of_Driver=9, Skidding_and_Overturning=1
    ## 11                             Vehicle_Manoeuvre=16, Age_Band_of_Driver=6
    ## 12                             Vehicle_Manoeuvre=17, Age_Band_of_Driver=7
    ## 13                       Vehicle_Manoeuvre=16, Skidding_and_Overturning=1
    ## 14  Skidding_and_Overturning=1, Junction_Location=1, Vehicle_Manoeuvre=18
    ## 15                        Skidding_and_Overturning=1, Junction_Location=1
    ## 16                                                   Vehicle_Manoeuvre=16
    ## 17                                             Skidding_and_Overturning=1
    ## 18                              Junction_Location=3, Vehicle_Manoeuvre=18
    ## 19                       Skidding_and_Overturning=1, Vehicle_Manoeuvre=18
    ## 20                                             Skidding_and_Overturning=2

Algunos subgrupos detectados para accidentes en moto de alta cilindrada
son: \* Motos conducidas por hombres en area urbana y mientras van en
movimiento. \* Motos conducidas por hombres de entre 36 - 45 a?os y
derrapan.

Accidentes de moto de baja cilindrada &lt;= 50cc:

    result6_2 <- DiscoverSubgroups(sample_v2, as.target("Vehicle_Type", "2"), new("SDTaskConfig", qf = "lift", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver")))
    ToDataFrame(result6_2)

    ##    quality    p size
    ## 1     7.13 0.11  223
    ## 2     7.01 0.11  274
    ## 3     6.88 0.10  742
    ## 4     6.80 0.10  907
    ## 5     6.61 0.10  401
    ## 6     6.47 0.10 1978
    ## 7     6.41 0.10  424
    ## 8     6.25 0.09  477
    ## 9     5.99 0.09  819
    ## 10    5.85 0.09 4090
    ## 11    5.23 0.08  418
    ## 12    3.75 0.06  336
    ## 13    3.70 0.06  394
    ## 14    3.21 0.05  330
    ## 15    3.10 0.05  214
    ## 16    2.99 0.05  753
    ## 17    2.68 0.04  544
    ## 18    2.61 0.04  305
    ## 19    2.16 0.03  767
    ## 20    1.96 0.03  574
    ##                                                               description
    ## 1                               Junction_Location=2, Age_Band_of_Driver=4
    ## 2                               Vehicle_Manoeuvre=4, Age_Band_of_Driver=4
    ## 3                               Age_Band_of_Driver=4, Junction_Location=8
    ## 4                               Age_Band_of_Driver=4, Junction_Location=1
    ## 5         Age_Band_of_Driver=4, Junction_Location=8, Vehicle_Manoeuvre=18
    ## 6                              Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 7  Skidding_and_Overturning=1, Age_Band_of_Driver=4, Vehicle_Manoeuvre=18
    ## 8         Age_Band_of_Driver=4, Junction_Location=1, Vehicle_Manoeuvre=18
    ## 9                        Skidding_and_Overturning=1, Age_Band_of_Driver=4
    ## 10                                                   Age_Band_of_Driver=4
    ## 11                                                   Vehicle_Manoeuvre=14
    ## 12                             Vehicle_Manoeuvre=17, Age_Band_of_Driver=4
    ## 13                              Vehicle_Manoeuvre=9, Age_Band_of_Driver=4
    ## 14                        Vehicle_Manoeuvre=4, Skidding_and_Overturning=1
    ## 15                       Skidding_and_Overturning=2, Age_Band_of_Driver=4
    ## 16                                                   Vehicle_Manoeuvre=13
    ## 17            Age_Band_of_Driver=4, Sex_of_Driver=2, Vehicle_Manoeuvre=18
    ## 18                             Vehicle_Manoeuvre=16, Age_Band_of_Driver=4
    ## 19                        Skidding_and_Overturning=1, Junction_Location=1
    ## 20                        Skidding_and_Overturning=1, Junction_Location=8

Algunos subgrupos detectados para accidentes en moto de baja cilindrada
son: \* Motos conducidas por hombres de entre 16 - 20 a?os adelantado en
zona urbana en rotodas o carreteras principales.

### C. CASUALTIES

Se quieren detectar situaciones recurrentes en las que se da un herido
mortal, grave o leve con la finalidad de poder mejorar las advertencias
de seguridad vial. Para ello tomamos **"Casualty Severity"** como
variable clase.

En esta base datos vamos a probar el **algoritmo SDMap**.

Heridos mortales:

    result7_2 <- DiscoverSubgroups(sample_c2, as.target("Casualty_Severity", "1"), new("SDTaskConfig", qf = "lift", minsize = 100, nodefaults = TRUE, attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result7_2)

    ##    quality    p size
    ## 1     6.88 0.07  160
    ## 2     6.54 0.07  153
    ## 3     6.35 0.06  126
    ## 4     6.35 0.06  126
    ## 5     6.20 0.06  274
    ## 6     5.43 0.05  129
    ## 7     5.36 0.05  112
    ## 8     5.36 0.05  112
    ## 9     5.13 0.05  117
    ## 10    5.13 0.05  117
    ## 11    5.07 0.05  138
    ## 12    5.07 0.05  138
    ## 13    4.96 0.05  121
    ## 14    4.68 0.05  171
    ## 15    4.68 0.05  171
    ## 16    4.50 0.05  111
    ## 17    4.50 0.05  111
    ## 18    4.49 0.04  245
    ## 19    4.46 0.04  202
    ## 20    4.46 0.04  202
    ##                                                                    description
    ## 1                 Age_Band_of_Casualty=11, Casualty_Class=3, Sex_of_Casualty=2
    ## 2                                   Casualty_Type=5, Casualty_Home_Area_Type=2
    ## 3                               Age_Band_of_Casualty=11, Pedestrian_Location=5
    ## 4             Age_Band_of_Casualty=11, Pedestrian_Location=5, Casualty_Class=3
    ## 5                                    Age_Band_of_Casualty=11, Casualty_Class=3
    ## 6        Age_Band_of_Casualty=10, Casualty_Home_Area_Type=3, Sex_of_Casualty=2
    ## 7                               Age_Band_of_Casualty=11, Pedestrian_Movement=1
    ## 8             Age_Band_of_Casualty=11, Pedestrian_Movement=1, Casualty_Class=3
    ## 9                                Pedestrian_Location=5, Age_Band_of_Casualty=9
    ## 10             Pedestrian_Location=5, Age_Band_of_Casualty=9, Casualty_Class=3
    ## 11                                Pedestrian_Location=9, Pedestrian_Movement=9
    ## 12              Pedestrian_Location=9, Pedestrian_Movement=9, Casualty_Class=3
    ## 13         Age_Band_of_Casualty=11, Casualty_Home_Area_Type=2, Casualty_Type=9
    ## 14                               Pedestrian_Location=10, Pedestrian_Movement=9
    ## 15             Pedestrian_Location=10, Pedestrian_Movement=9, Casualty_Class=3
    ## 16                               Pedestrian_Movement=9, Age_Band_of_Casualty=7
    ## 17             Pedestrian_Movement=9, Age_Band_of_Casualty=7, Casualty_Class=3
    ## 18                  Age_Band_of_Casualty=11, Casualty_Class=2, Casualty_Type=9
    ## 19                   Age_Band_of_Casualty=11, Car_Passenger=1, Casualty_Type=9
    ## 20 Age_Band_of_Casualty=11, Car_Passenger=1, Casualty_Type=9, Casualty_Class=2

Algunos subgrupos detectados son: \* Peatones mayores de 75 a?os
cruzando la calle en zona urbana.

Heridos graves:

    result8_2 <- DiscoverSubgroups(sample_c2, as.target("Casualty_Severity", "2"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result8_2)

    ##    quality    p size                                description
    ## 1     3.04 0.34 1479                            Casualty_Type=5
    ## 2     2.12 0.24 1424                      Pedestrian_Movement=1
    ## 3     2.12 0.24 1424    Pedestrian_Movement=1, Casualty_Class=3
    ## 4     1.97 0.22 1968                      Pedestrian_Location=5
    ## 5     1.97 0.22 1968    Pedestrian_Location=5, Casualty_Class=3
    ## 6     1.92 0.22 4122                           Casualty_Class=3
    ## 7     1.90 0.22 1206                            Casualty_Type=3
    ## 8     1.83 0.21 1800        Casualty_Class=3, Sex_of_Casualty=2
    ## 9     1.64 0.19 1242                    Age_Band_of_Casualty=11
    ## 10    1.40 0.16 2968                            Casualty_Type=1
    ## 11    1.28 0.15 1682                     Age_Band_of_Casualty=3
    ## 12    1.21 0.14 1460                    Age_Band_of_Casualty=10
    ## 13    1.18 0.13 4498                  Casualty_Home_Area_Type=3
    ## 14    1.09 0.12 2771                     Age_Band_of_Casualty=9
    ## 15    1.09 0.12 3625                  Casualty_Home_Area_Type=2
    ## 16    1.07 0.12 5559                     Age_Band_of_Casualty=4
    ## 17    0.99 0.11 4771                     Age_Band_of_Casualty=8
    ## 18    0.93 0.11 6263                     Age_Band_of_Casualty=7
    ## 19    0.90 0.10 1002   Age_Band_of_Casualty=10, Casualty_Type=9
    ## 20    0.87 0.10 3341 Casualty_Home_Area_Type=3, Casualty_Type=9

Algunos subgrupos detectados son: \* Hombres en moto de alta cilindrada

Heridos leves:

    result9_2 <- DiscoverSubgroups(sample_c2, as.target("Casualty_Severity", "3"), new("SDTaskConfig", qf = "lift", minsize = 1000, nodefaults = TRUE, attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result9_2)

    ##    quality    p  size
    ## 1     1.09 0.95  2384
    ## 2     1.08 0.95  2102
    ## 3     1.07 0.94  1670
    ## 4     1.07 0.94  4017
    ## 5     1.07 0.94  4691
    ## 6     1.07 0.94  1681
    ## 7     1.07 0.94 11946
    ## 8     1.07 0.93  1148
    ## 9     1.06 0.93  2937
    ## 10    1.06 0.93  1563
    ## 11    1.06 0.93  2579
    ## 12    1.06 0.93  2999
    ## 13    1.06 0.93  2020
    ## 14    1.06 0.93  2939
    ## 15    1.06 0.93  2939
    ## 16    1.06 0.93  2883
    ## 17    1.06 0.93  2883
    ## 18    1.05 0.92  4137
    ## 19    1.05 0.92 23630
    ## 20    1.05 0.92  3410
    ##                                                              description
    ## 1             Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9
    ## 2             Age_Band_of_Casualty=7, Sex_of_Casualty=2, Casualty_Type=9
    ## 3             Age_Band_of_Casualty=5, Sex_of_Casualty=2, Casualty_Type=9
    ## 4                                Age_Band_of_Casualty=7, Casualty_Type=9
    ## 5                                Age_Band_of_Casualty=6, Casualty_Type=9
    ## 6             Age_Band_of_Casualty=4, Sex_of_Casualty=2, Casualty_Type=9
    ## 7                                     Sex_of_Casualty=2, Casualty_Type=9
    ## 8                               Age_Band_of_Casualty=6, Casualty_Class=2
    ## 9                              Age_Band_of_Casualty=6, Sex_of_Casualty=2
    ## 10            Age_Band_of_Casualty=8, Sex_of_Casualty=2, Casualty_Type=9
    ## 11                             Age_Band_of_Casualty=7, Sex_of_Casualty=2
    ## 12                               Age_Band_of_Casualty=8, Casualty_Type=9
    ## 13                             Age_Band_of_Casualty=5, Sex_of_Casualty=2
    ## 14                                    Car_Passenger=1, Sex_of_Casualty=2
    ## 15                  Car_Passenger=1, Sex_of_Casualty=2, Casualty_Class=2
    ## 16 Car_Passenger=1, Sex_of_Casualty=2, Casualty_Class=2, Casualty_Type=9
    ## 17                   Car_Passenger=1, Sex_of_Casualty=2, Casualty_Type=9
    ## 18                  Casualty_Class=2, Sex_of_Casualty=2, Casualty_Type=9
    ## 19                                                       Casualty_Type=9
    ## 20                               Age_Band_of_Casualty=5, Casualty_Type=9

Algunos subgrupos detectados son: \*

**COMPARACI?N DE MODELOS**
--------------------------

A continuaci?n vamos a comparar los modelos anteriores.

    results_accidents <- matrix(c(ToDataFrame(result1[1]),ToDataFrame(result1_2[1]),ToDataFrame(result2[1]),ToDataFrame(result2_2[1]),
                                  ToDataFrame(result3[1]),ToDataFrame(result3_2[1])),nrow = 6, ncol = 4, byrow = TRUE)
    rownames(results_accidents) <- c("result1","result1_2","result2","result2_2","result3","result3_2")
    colnames(results_accidents) <- c("quality", "p", "size", "type")
    description_accidents <- c("Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2, Road_Type=6","Light_Conditions=6, Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2","Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2","Day_of_Week=7, Urban_or_Rural_Area=2, Road_Type=6","Junction_Detail=1, Speed_limit=30, Number_of_Vehicles=2","Junction_Detail=1, Number_of_Vehicles=2")
    cbind(results_accidents,description_accidents)

    ##           quality p    size type    
    ## result1   3.49    0.03 1296 factor,1
    ## result1_2 4.04    0.05 1010 factor,1
    ## result2   1.52    0.18 1558 factor,1
    ## result2_2 1.56    0.22 1103 factor,1
    ## result3   1.07    0.94 1911 factor,1
    ## result3_2 1.09    0.93 2182 factor,1
    ##           description_accidents                                                     
    ## result1   "Speed_limit=60, Junction_Detail=3, Urban_or_Rural_Area=2, Road_Type=6"   
    ## result1_2 "Light_Conditions=6, Speed_limit=60, Road_Type=6, Urban_or_Rural_Area=2"  
    ## result2   "Speed_limit=60, Number_of_Vehicles=2, Road_Type=6, Urban_or_Rural_Area=2"
    ## result2_2 "Day_of_Week=7, Urban_or_Rural_Area=2, Road_Type=6"                       
    ## result3   "Junction_Detail=1, Speed_limit=30, Number_of_Vehicles=2"                 
    ## result3_2 "Junction_Detail=1, Number_of_Vehicles=2"

En la base de datos "Accidents" se consigue una mayor calidad eliminando
las variables.

    results_vehicles <- matrix(c(ToDataFrame(result4[1]),ToDataFrame(result4_2[1]),ToDataFrame(result5[1]),ToDataFrame(result5_2[1]),
                                  ToDataFrame(result6[1]),ToDataFrame(result6_2[1])),nrow = 6, ncol = 4, byrow = TRUE)
    rownames(results_vehicles) <- c("result4","result4_2","result5","result5_2","result6","result6_2")
    colnames(results_vehicles) <- c("quality", "p", "size", "type")
    description_vehicles <- c("Vehicle_Manoeuvre=3, Sex_of_Driver=2","Vehicle_Manoeuvre=9, Sex_of_Driver=2", "Vehicle_Manoeuvre=13", "Vehicle_Manoeuvre=13","Age_Band_of_Driver=4, Junction_Location=8, Vehicle_Manoeuvre=18","Junction_Location=2, Age_Band_of_Driver=4")
    cbind(results_vehicles,description_vehicles)

    ##           quality p    size type    
    ## result4   1.23    0.98 1112 factor,1
    ## result4_2 1.24    0.99 1395 factor,1
    ## result5   5.4     0.2  670  factor,1
    ## result5_2 5.21    0.18 753  factor,1
    ## result6   9.02    0.13 439  factor,1
    ## result6_2 7.13    0.11 223  factor,1
    ##           description_vehicles                                             
    ## result4   "Vehicle_Manoeuvre=3, Sex_of_Driver=2"                           
    ## result4_2 "Vehicle_Manoeuvre=9, Sex_of_Driver=2"                           
    ## result5   "Vehicle_Manoeuvre=13"                                           
    ## result5_2 "Vehicle_Manoeuvre=13"                                           
    ## result6   "Age_Band_of_Driver=4, Junction_Location=8, Vehicle_Manoeuvre=18"
    ## result6_2 "Junction_Location=2, Age_Band_of_Driver=4"

En "Vehicles" la calidad solo mejora en un subgrupo, por lo que en este
caso es mejor eliminar registros.

    results_casualties <- matrix(c(ToDataFrame(result7[1]),ToDataFrame(result7_2[1]),ToDataFrame(result8[1]),ToDataFrame(result8_2[1]),
                                  ToDataFrame(result9[1]),ToDataFrame(result9_2[1])),nrow = 6, ncol = 4, byrow = TRUE)
    rownames(results_casualties) <- c("result7","result7_2","result8","result8_2","result9","result9_2")
    colnames(results_casualties) <- c("quality", "p", "size", "type")
    description_casualties <- c("Age_Band_of_Casualty=11, Pedestrian_Location=5","Age_Band_of_Casualty=11, Casualty_Class=3, Sex_of_Casualty=2", "Casualty_Type=5", "Casualty_Type=5","Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9","Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9")
    cbind(results_casualties,description_casualties)

    ##           quality p    size type    
    ## result7   8.9     0.08 156  factor,1
    ## result7_2 6.88    0.07 160  factor,1
    ## result8   2.81    0.33 1444 factor,1
    ## result8_2 3.04    0.34 1479 factor,1
    ## result9   1.1     0.96 1985 factor,1
    ## result9_2 1.09    0.95 2384 factor,1
    ##           description_casualties                                        
    ## result7   "Age_Band_of_Casualty=11, Pedestrian_Location=5"              
    ## result7_2 "Age_Band_of_Casualty=11, Casualty_Class=3, Sex_of_Casualty=2"
    ## result8   "Casualty_Type=5"                                             
    ## result8_2 "Casualty_Type=5"                                             
    ## result9   "Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9"  
    ## result9_2 "Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9"

En "Casualties" sucede lo mismo, solo mejora un subgrupo, por lo tanto
tambi?n es mejor eliminar registros.

    (size_model_ER <- nrow(Accidents)*ncol(Accidents))

    ## [1] 23085264

    (size_modelo_EV <- nrow(Accidents2)*ncol(Accidents2))

    ## [1] 35678896

A la hora de elegir las muestras con las que se han generado los
subgrupos, se han tenido en cuenta las dimensiones con las que se
trabaja en cada base de datos. Es decir, la diferencia de contar con m?s
variables (modelo que elimina registros ER) o con m?s observaciones
(modelo que elimina variables EV) puede generar distorsiones de tama?o
en las muestra si para ambas se aplica el mismo porcentaje. Por ello, se
compensa el % de las muestras para que en los modelos a comparar haya un
n?mero similar entre filas y columnas. Si pudi?ramos trabajar con las
bases de datos completas, al haber tal diferencia entre ambas, es muy
probable que el modelo EV obtuviera mejor calidad en todos los
subgrupos, ya que contienen mayor informaci?n.

**Una vez elegidos los modelos, ?cual es la primera regla significativa
al 5%?**

A?adiendo postfilter= "sig-improve-set" en SDTaskConfig se obtienen
subgrupos d?nde solo aparecen los subgrupos que son significativos al 5%
del nivel de significaci?n (95% nivel de confianza), desapareciendo
subgrupos irrelevantes.

Accidents:

    result1_2_sig <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "1"), new("SDTaskConfig",  qf = "lift", postfilter= "sig-improve-set",  minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result1_2_sig[1])

    ##   quality    p size        description
    ## 1    3.53 0.05 1776 Light_Conditions=6

    # Octava regla en el result1_2 es la primera significativa.

    result2_2_sig <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "2"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result2_2_sig[1])

    ##   quality    p size                                       description
    ## 1    1.56 0.22 1103 Day_of_Week=7, Urban_or_Rural_Area=2, Road_Type=6

    # La primera regla de result2_2 es significativa.

    result3_2_sig <- DiscoverSubgroups(sample2, as.target("Accident_Severity", "3"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 1000, nodefaults = TRUE, attributes=c("Number_of_Vehicles","Number_of_Casualties","Day_of_Week","Speed_limit","Road_Type","Junction_Detail","Light_Conditions","Weather_Conditions","Road_Surface_Conditions","Carriageway_Hazards","Urban_or_Rural_Area")))
    ToDataFrame(result3_2_sig[1])

    ##   quality    p size                             description
    ## 1    1.09 0.93 2182 Junction_Detail=1, Number_of_Vehicles=2

    # La primera regla de result3_2 es significativa.

Vehicles:

    result4_sig <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "9"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 1000, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result4_sig[1])

    ##   quality    p size                          description
    ## 1    1.23 0.98 1112 Vehicle_Manoeuvre=3, Sex_of_Driver=2

    # La primera regla de result4 es significativa.

    result5_sig <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "5"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result5_sig[1])

    ##   quality   p size          description
    ## 1     5.4 0.2  670 Vehicle_Manoeuvre=13

    # La primera regla de result5 es significativa.

    result6_sig <- DiscoverSubgroups(sample_v, as.target("Vehicle_Type", "2"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 200, nodefaults = TRUE, attributes=c("Vehicle_Manoeuvre","Vehicle_Location.Restricted_Lane","Junction_Location","Skidding_and_Overturning","Was_Vehicle_Left_Hand_Drive.","Sex_of_Driver","Age_Band_of_Driver","Age_of_Vehicle","Driver_Home_Area_Type")))
    ToDataFrame(result6_sig[1])

    ##   quality    p size
    ## 1    9.02 0.13  439
    ##                                                       description
    ## 1 Age_Band_of_Driver=4, Junction_Location=8, Vehicle_Manoeuvre=18

    # La primera regla de result6 es significativa.

Casualties:

    result7_sig <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "1"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 100, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result7_sig[1])

    ##   quality    p size                                    description
    ## 1     8.9 0.08  156 Age_Band_of_Casualty=11, Pedestrian_Location=5

    # La primera regla de result7 es significativa.

    result8_sig <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "2"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 1000, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result8_sig[1])

    ##   quality    p size     description
    ## 1    2.81 0.33 1444 Casualty_Type=5

    # La primera regla de result8 es significativa.

    result9_sig <- DiscoverSubgroups(sample_c, as.target("Casualty_Severity", "3"), new("SDTaskConfig", qf = "lift", postfilter= "sig-improve-set", minsize = 1000, nodefaults = TRUE, method ="sdmap", attributes=c("Casualty_Class","Sex_of_Casualty","Age_Band_of_Casualty","Pedestrian_Location","Pedestrian_Movement","Car_Passenger","Casualty_Type","Casualty_Home_Area_Type")))
    ToDataFrame(result9_sig[1])

    ##   quality    p size
    ## 1     1.1 0.96 1985
    ##                                                  description
    ## 1 Age_Band_of_Casualty=6, Sex_of_Casualty=2, Casualty_Type=9

    # La primera regla de result9 es significativa.

Unicamente en result1\_2 (Accident\_severity=1) la primera regla no es
significativa, por lo que se comparar?n la primera regla del original y
la significativa al 5%. En el resto de resultados la primera regla es
signficativa, por lo que no es necesario comparar nada.

**MEDIDAS DE CALIDAD**
----------------------

Con el fin de obtener el inter?s y la calidad de los subgrupos obtenidos
hay que analizar su calidad. De este modo, se pueden analizar todos los
modelos realizados.

Accidents:

    ## 
    ## FALSE  TRUE 
    ## 32006   429

    ## 
    ## FALSE  TRUE 
    ## 31425  1010

    ## 
    ## FALSE  TRUE 
    ## 32381    54

    ## 
    ## FALSE  TRUE 
    ## 31479   956

    ## 
    ## FALSE  TRUE 
    ## 27963  4472

    ## 
    ## FALSE  TRUE 
    ##  4901 27534

    ## 
    ## FALSE  TRUE 
    ## 32402    33

    ## 
    ## FALSE  TRUE 
    ## 27804  4631

    ## 
    ## FALSE  TRUE 
    ## 32006   429

    ## 
    ## FALSE  TRUE 
    ## 30659  1776

    ## 
    ## FALSE  TRUE 
    ## 32352    83

    ## 
    ## FALSE  TRUE 
    ## 30742  1693

    ## 
    ## FALSE  TRUE 
    ## 27963  4472

    ## 
    ## FALSE  TRUE 
    ##  4901 27534

    ## 
    ## FALSE  TRUE 
    ## 32172   263

    ## 
    ## FALSE  TRUE 
    ##  5327 27108

    ## 
    ## FALSE  TRUE 
    ## 27963  4472

    ## 
    ## FALSE  TRUE 
    ## 31332  1103

    ## 
    ## FALSE  TRUE 
    ## 32197   238

    ## 
    ## FALSE  TRUE 
    ## 31570   865

    ## 
    ## FALSE  TRUE 
    ## 32006   429

    ## 
    ## FALSE  TRUE 
    ##  4901 27534

    ## 
    ## FALSE  TRUE 
    ## 32000   435

    ## 
    ## FALSE  TRUE 
    ## 28720  3715

    ## 
    ## FALSE  TRUE 
    ##  4901 27534

    ## 
    ## FALSE  TRUE 
    ## 29614  2821

    ## 
    ## FALSE  TRUE 
    ## 30415  2020

    ## 
    ## FALSE  TRUE 
    ## 32273   162

    ## 
    ## FALSE  TRUE 
    ## 32006   429

    ## 
    ## FALSE  TRUE 
    ## 27963  4472

    ## 
    ## FALSE  TRUE 
    ## 22454  9981

    ## 
    ## FALSE  TRUE 
    ## 29943  2492

Vehicles:

    ## 
    ## FALSE  TRUE 
    ##  6739 26366

    ## 
    ## FALSE  TRUE 
    ## 31993  1112

    ## 
    ## FALSE  TRUE 
    ## 32017  1088

    ## 
    ## FALSE  TRUE 
    ## 33081    24

    ## 
    ## FALSE  TRUE 
    ## 31898  1207

    ## 
    ## FALSE  TRUE 
    ## 32612   493

    ## 
    ## FALSE  TRUE 
    ## 18808 14297

    ## 
    ## FALSE  TRUE 
    ## 27052  6053

    ## 
    ## FALSE  TRUE 
    ## 31898  1207

    ## 
    ## FALSE  TRUE 
    ## 32435   670

    ## 
    ## FALSE  TRUE 
    ## 32973   132

    ## 
    ## FALSE  TRUE 
    ## 32567   538

    ## 
    ## FALSE  TRUE 
    ##  6739 26366

    ## 
    ## FALSE  TRUE 
    ## 32612   493

    ## 
    ## FALSE  TRUE 
    ## 32030  1075

    ## 
    ## FALSE  TRUE 
    ##  1745 31360

    ## 
    ## FALSE  TRUE 
    ## 32612   493

    ## 
    ## FALSE  TRUE 
    ## 32666   439

    ## 
    ## FALSE  TRUE 
    ## 33046    59

    ## 
    ## FALSE  TRUE 
    ## 32725   380

    ## 
    ## FALSE  TRUE 
    ##  6739 26366

    ## 
    ## FALSE  TRUE 
    ## 31898  1207

    ## 
    ## FALSE  TRUE 
    ## 33053    52

    ## 
    ## FALSE  TRUE 
    ## 19855 13250

Casualties:

    ## 
    ## FALSE  TRUE 
    ## 34168   323

    ## 
    ## FALSE  TRUE 
    ## 34335   156

    ## 
    ## FALSE  TRUE 
    ## 34478    13

    ## 
    ## FALSE  TRUE 
    ## 34348   143

    ## 
    ## FALSE  TRUE 
    ## 30403  4088

    ## 
    ## FALSE  TRUE 
    ##  4411 30080

    ## 
    ## FALSE  TRUE 
    ## 34241   250

    ## 
    ## FALSE  TRUE 
    ##  3365 31126

    ## 
    ## FALSE  TRUE 
    ## 30403  4088

    ## 
    ## FALSE  TRUE 
    ## 33047  1444

    ## 
    ## FALSE  TRUE 
    ## 34010   481

    ## 
    ## FALSE  TRUE 
    ## 33528   963

    ## 
    ## FALSE  TRUE 
    ## 34168   323

    ## 
    ## FALSE  TRUE 
    ##  4411 30080

    ## 
    ## FALSE  TRUE 
    ## 30884  3607

    ## 
    ## FALSE  TRUE 
    ##  5051 29440

    ## 
    ## FALSE  TRUE 
    ##  4411 30080

    ## 
    ## FALSE  TRUE 
    ## 32506  1985

    ## 
    ## FALSE  TRUE 
    ## 32591  1900

    ## 
    ## FALSE  TRUE 
    ## 34406    85

    ## 
    ## FALSE  TRUE 
    ## 34168   323

    ## 
    ## FALSE  TRUE 
    ## 30403  4088

    ## 
    ## FALSE  TRUE 
    ## 28081  6410

    ## 
    ## FALSE  TRUE 
    ## 32720  1771

**Medidas de complejidad:**

Accidents:

    ## [1] 2.65

    ## [1] 2

    ## [1] 2.55

    ## [1] 3.15

Vehicles:

    ## [1] 2

    ## [1] 2.05

    ## [1] 2.25

Casualties:

    ## [1] 2.45

    ## [1] 1.3

    ## [1] 2.6

**Medidas de generalidad:**

Accidents:

    ## [1] 0.0622784

Vehicles:

Casualties:

**Medidas de precisi?n:**

Accidents:

Vehicles:

Casualties:

**Medidas de inter?s:**

Accidents:

Vehicles:

Casualties:

**Medidas h?bridas:**

Accidents:

Vehicles:

Casualties:

**COMPARACI?N DE LA CALIDAD DE LOS MODELOS**
--------------------------------------------

    result1_2 <- data.frame(num_var_result1_2,Cov_result1_2,Sup_result1_2,Cnf_result1_2,Qc_result1_2,
                                      Qg_result1_2,Nov_result1_2,Sens_result1_2,FA_result1_2,Spec_result1_2,
                                      WRAcc_result1_2,4.04)

    result1_2_sig <- data.frame(num_var_result1_2_sig,Cov_result1_2_sig,Sup_result1_2_sig,Cnf_result1_2_sig,Qc_result1_2_sig,
                                      Qg_result1_2_sig,Nov_result1_2_sig,Sens_result1_2_sig,FA_result1_2_sig,Spec_result1_2_sig,
                                      WRAcc_result1_2_sig,3.53)

    result2_2 <- data.frame(num_var_result2_2,Cov_result2_2,Sup_result2_2,Cnf_result2_2,Qc_result2_2,
                                      Qg_result2_2,Nov_result2_2,Sens_result2_2,FA_result2_2,Spec_result2_2,
                                      WRAcc_result2_2,1.56)

    result3_2 <- data.frame(num_var_result3_2,Cov_result3_2,Sup_result3_2,Cnf_result3_2,Qc_result3_2,
                                      Qg_result3_2,Nov_result3_2,Sens_result3_2,FA_result3_2,Spec_result3_2,
                                      WRAcc_result3_2,1.09)

    result4 <- data.frame(num_var_result4,Cov_result4,Sup_result4,Cnf_result4,Qc_result4,
                                      Qg_result4,Nov_result4,Sens_result4,FA_result4,Spec_result4,
                                      WRAcc_result4,1.23)

    result5 <- data.frame(num_var_result5,Cov_result5,Sup_result5,Cnf_result5,Qc_result5,
                                      Qg_result5,Nov_result5,Sens_result5,FA_result5,Spec_result5,
                                      WRAcc_result5,5.40)

    result6 <- data.frame(num_var_result6,Cov_result6,Sup_result6,Cnf_result6,Qc_result6,
                                      Qg_result6,Nov_result6,Sens_result6,FA_result6,Spec_result6,
                                      WRAcc_result6,9.02)

    result7 <- data.frame(num_var_result7,Cov_result7,Sup_result7,Cnf_result7,Qc_result7,
                                      Qg_result7,Nov_result7,Sens_result7,FA_result7,Spec_result7,
                                      WRAcc_result7,8.90)

    result8 <- data.frame(num_var_result8,Cov_result8,Sup_result8,Cnf_result8,Qc_result8,
                                      Qg_result8,Nov_result8,Sens_result8,FA_result8,Spec_result8,
                                      WRAcc_result8,2.81)

    result9 <- data.frame(num_var_result9,Cov_result9,Sup_result9,Cnf_result9,Qc_result9,
                                      Qg_result9,Nov_result9,Sens_result9,FA_result9,Spec_result9,
                                      WRAcc_result9,1.10)

    rownames = c("acc_result1_2", "acc_result1_2_sig", "acc_result2_2", "acc_result3_2", "veh_result4", "veh_result5", "veh_result6", "cas_result7", "cas_result8", "cas_result9")
    colnames = c("N? medio variables", "Cubrimiento en % (Cov)", "Soporte (Sup)", "Accuracy (Cnf)", "Precisi?n (Qc)", "Precisi?n (Qg)", "Novedad (Nov)", "Sensitividad (Sens)", "Falsa Alarma (FA)", "Especificidad (Spec)", "Rareza (WRAcc)","Quality (Q)")
    comp_modelos <- matrix(c(result1_2,result1_2_sig,result2_2,result3_2,result4,result5,result6,result7,result8,result9), 
                           nrow=10, ncol=12, byrow = TRUE, dimnames = list(rownames, colnames))
    comp_modelos 

    ##                   N? medio variables Cubrimiento en % (Cov) Soporte (Sup)
    ## acc_result1_2     2.65               3.11392                0.001664868  
    ## acc_result1_2_sig 2                  5.475567               0.002558964  
    ## acc_result2_2     2.55               3.400647               0.007337752  
    ## acc_result3_2     3.15               8.697395               0.0622784    
    ## veh_result4       2                  3.359009               0.03286513   
    ## veh_result5       2.05               2.023863               0.003987313  
    ## veh_result6       2.25               1.326084               0.001782208  
    ## cas_result7       2.45               0.4522919              0.0003769099 
    ## cas_result8       1.3                4.186599               0.01394567   
    ## cas_result9       2.6                5.755125               0.05508683   
    ##                   Accuracy (Cnf) Precisi?n (Qc) Precisi?n (Qg)
    ## acc_result1_2     0.05346535     -9506          0.05113636    
    ## acc_result1_2_sig 0.04673423     -16847         0.04629113    
    ## acc_result2_2     0.2157752      -8412          0.2466321     
    ## acc_result3_2     0.7160581      400            7.709924      
    ## veh_result4       0.9784173      848            8.774194      
    ## veh_result5       0.1970149      -5248          0.2068966     
    ## veh_result6       0.1343964      -3741          0.1229167     
    ## cas_result7       0.08333333     -1417          0.05349794    
    ## cas_result8       0.3331025      -9149          0.4524929     
    ## cas_result9       0.9571788      1050           10.27027      
    ##                   Novedad (Nov) Sensitividad (Sens) Falsa Alarma (FA)
    ## acc_result1_2     -32759296     0.001664868         0.0298694        
    ## acc_result1_2_sig -57604477     0.002558964         0.05289633       
    ## acc_result2_2     -35775567     0.007337752         0.03093373       
    ## acc_result3_2     -91497115     0.0622784           0.03305448       
    ## veh_result4       -36811672     0.03286513          0.01411765       
    ## veh_result5       -22180218     0.003987313         0.02003053       
    ## veh_result6       -14533036     0.001782208         0.0137816        
    ## cas_result7       -5380583      0.0003769099        0.004185203      
    ## cas_result8       -49804523     0.01394567          0.03167451       
    ## cas_result9       -68462735     0.05508683          0.01927001       
    ##                   Especificidad (Spec) Rareza (WRAcc) Quality (Q)
    ## acc_result1_2     0.007075472          -2.947433      4.04       
    ## acc_result1_2_sig 0.00960871           -5.21967       3.53       
    ## acc_result2_2     0.1048193            -2.666872      1.56       
    ## acc_result3_2     0.8002085            -2.469554      1.09       
    ## veh_result4       0.7025553            -0.0724966     1.23       
    ## veh_result5       0.03314321           -1.625132      5.4        
    ## veh_result6       0.003909187          -1.147863      9.02       
    ## cas_result7       0.007967874          -0.4146009     8.9        
    ## cas_result8       0.1091476            -2.792033      2.81       
    ## cas_result9       0.7835228            -0.2464411     1.1

Medidas de generalidad: CUBRIMIENTO: Mide el porcentaje de ejemplos
cubiertos de media. Los m?s altos son acc\_result\_1\_2\_sig y cas\_
result9. SOPORTE: Mide la frecuencia de ejemplos correctamente
clasificados cubiertos por la regla. El m?s alto es acc\_result\_3\_2.
Medidas de precisi?n: ACCURACY: Mide la frecuencia relativa de ejemplos
que satisfacen la regla completa entre los que satisfacen s?lo el
antecedente. Claramente el mejor es veh\_result4. PRECISI?N: Mide el
balance entre los True y false positives cubiertos mediante una funci?n
lineal. c = 10. veh\_result4. Vuelve a ser el mejor. PRECISI?N: Mide el
balance entre el n?mero de ejemplos clasificados perfectamente y la
"rareza" de su distribuci?n. g=100. veh\_result4. Vuelve a ser el mejor.
Medidas de inter?s: NOVEDAD: Esta medida es capaz de detectar grupos
inusuales. Veh\_result6 es el que menos grupos inusuales tiene. Medidas
h?bridas: SENSITIVIDAD: Es la proporci?n de individuos con valor
correcto de la variable objetivo que han sido clasificados
correctamente. acc\_result\_3\_2 obtiene el mejor resultado. FALSA
ALARMA: Esta medida cubre los ejemplos que no est?n en la variable
objetivo. El mayor resultado (peor medida) lo tiene
acc\_result\_1\_2\_sig. ESPECIFICIDAD: Mide la proporci?n de casos
negativos incorrectamente clasificados. acc\_result\_3\_2 es el que
clasifica m?s err?neamente. RAREZA: Esta medida se define como la
precisi?n relativa ponderada de una regla. La mayor precisi?n la tiene
veh\_result4. QUALITY: Es la medida de calidad "lift" que se obtiene con
los subgrupos. El m?s alto es veh\_result6.

\[Conclusiones\]

**ANEXO**
---------

Durante el an?lisis se han probado diferentes medidas de calidad, aunque
no dan resultados relevantes. A modo de ejemplo, la medida de calidad
Weighted Relative Accuracy wracc da 0 en todos los casos.

Tambi?n se han probado otros algoritmos, como SDmap, pero los resultados
no son diferentes que al utilizar el algoritmo por defecto.

En lo que refiere a modelo general, se observa que los accidentes fueron
entre dos veh?culos, en zona urbana, n?ng?n paso de peatones a menos de
50 metros, sin peligros ni condiciones peligrosas en la carretera y con
un l?mite de velocidad de 30 mph (48 kmh). Por lo tanto, se puede
interpretar que en carreteras donde no hay mucho movimiento peatonal,
donde la velocidad no es muy elevada, cuando suceden accidentes suelen
ser de gravedad leve. Otra intuici?n que se puede pensar es que al no
haber ni peligros ni defectos en la carretera, los accidentes peuden ser
debdiso a despistes de los conductores.
