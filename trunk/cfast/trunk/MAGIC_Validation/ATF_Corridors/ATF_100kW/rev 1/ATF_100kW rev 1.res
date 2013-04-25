#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
Lower_Corridor 0 MONOZONE(1=OUI,0=NON)                                                              
0.000000 0.000000 0.000000                                                                          
17.000000 1.800000 2.500000                                                                         
#PAROI PAR_1                                                                                        
TRA_2                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_2                                                                                        
TRA_1                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_3                                                                                        
TRA_1                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_4                                                                                        
TRA_1                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_5                                                                                        
TRA_1                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_6                                                                                        
TRA_6                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#FOYER FOY_1                                                                                        
ATF_100kW                                                                                           
CIRCULAIRE                                                                                          
15.000000 0.900000 0.400000 0.450000                                                                
1.000000 0.000000 0                                                                                 
Methane_CH4_D=1m                                                                                    
#PCI                                                                                                
0.000000 50000000.000000                                                                            
#FINPCI                                                                                             
#PARTRAYONNEE                                                                                       
0.000000 0.200000                                                                                   
#FINPARTRAYONNEE                                                                                    
#RAPPORT                                                                                            
0.000000 4.000000                                                                                   
#FINRAPPORT                                                                                         
#TAUX_SUIE                                                                                          
0.010000                                                                                            
#DEBITPYROLYSE                                                                                      
0.000000 0.000230                                                                                   
60.000000 0.001700                                                                                  
70.000000 0.001800                                                                                  
900.000000 0.002000                                                                                 
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
0.000000                                                                                            
#FINFOYER                                                                                           
#CIBLE CIB_1                                                                                        
detecteur                                                                                           
HeatDet_1                                                                                           
13.000000 0.800000 2.340000                                                                         
0.000000 0.000000 -1.000000                                                                         
5.000000 30.000000                                                                                  
#FINCIBLE                                                                                           
#CIBLE CIB_2                                                                                        
detecteur                                                                                           
HeatDet_2                                                                                           
6.000000 1.000000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
5.000000 30.000000                                                                                  
#FINCIBLE                                                                                           
#CIBLE CIB_3                                                                                        
detecteur                                                                                           
HeatDet_3                                                                                           
0.500000 0.500000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
5.000000 30.000000                                                                                  
#FINCIBLE                                                                                           
#CIBLE CIB_5                                                                                        
thermique                                                                                           
TC_1                                                                                                
13.050000 0.800000 2.340000                                                                         
0.000000 0.000000 -1.000000                                                                         
0.001500 7854.000000 559.000000 48.000000 0.000000 1.000000                                         
#FINCIBLE                                                                                           
#CIBLE CIB_6                                                                                        
thermique                                                                                           
TC_2                                                                                                
6.050000 1.000000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
0.001500 7854.000000 559.000000 48.000000 0.000000 1.000000                                         
#FINCIBLE                                                                                           
#CIBLE CIB_7                                                                                        
thermique                                                                                           
TC_3                                                                                                
0.550000 0.500000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
0.001500 7854.000000 559.000000 48.000000 0.000000 1.000000                                         
#FINCIBLE                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Upper_Corridor 0 MONOZONE(1=OUI,0=NON)                                                              
0.000000 0.000000 2.517000                                                                          
17.000000 1.800000 2.500000                                                                         
#PAROI PAR_7                                                                                        
TRA_6                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_8                                                                                        
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_9                                                                                        
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_10                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_11                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_12                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_3                                                                                        
Stairwell 0 MONOZONE(1=OUI,0=NON)                                                                   
0.000000 1.817000 0.000000                                                                          
2.600000 3.200000 5.017000                                                                          
#PAROI PAR_13                                                                                       
TRA_3                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_14                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_15                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_16                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_17                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_18                                                                                       
TRA_1                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#CIBLE CIB_4                                                                                        
detecteur                                                                                           
Heat_4                                                                                              
0.500000 0.100000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
5.000000 30.000000                                                                                  
#FINCIBLE                                                                                           
#CIBLE CIB_8                                                                                        
thermique                                                                                           
TC_4                                                                                                
0.550000 0.100000 2.340000                                                                          
0.000000 0.000000 -1.000000                                                                         
0.001500 7854.000000 559.000000 48.000000 0.000000 1.000000                                         
#FINCIBLE                                                                                           
#FINLOCAL                                                                                           
#TRANCHEPAROI TRA_1                                                                                 
#TRANCHE 0                                                                                          
GYPSUMBOARD_ATF                                                                                     
0.017000 930.000000 1090.000000 0.130000 0                                                          
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_2                                                                                 
#TRANCHE 0                                                                                          
CONCRETE_NORM_WT_6_IN                                                                               
0.017000 2200.000000 1000.000000 1.750000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_3                                                                                 
#TRANCHE 0                                                                                          
CONCRETE_NORM_WT_6_IN                                                                               
0.017000 2200.000000 1000.000000 1.750000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_6                                                                                 
#TRANCHE 0                                                                                          
CONCRETE_NORM_WT_6_IN                                                                               
0.017000 2200.000000 1000.000000 1.750000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#OUVERTURE OUV_1                                                                                    
LowerCorridor_Stairwell                                                                             
LOC_1 LOC_3                                                                                         
PAR_4                                                                                               
14.400000 0.000000                                                                                  
17.000000 2.500000                                                                                  
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#OUVERTURE OUV_2                                                                                    
UpperCorridor_Outside                                                                               
LOC_2 EXT                                                                                           
PAR_9                                                                                               
0.000000 0.000000                                                                                   
1.800000 2.000000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#OUVERTURE OUV_3                                                                                    
Stairwell_UpperCorridor                                                                             
LOC_3 LOC_2                                                                                         
PAR_14                                                                                              
0.000000 2.520000                                                                                   
2.600000 5.000000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#CONDINIT 900.000000 10.000000 20.000000 0.230000 0.001000 101325.000000                            
#OPTION                                                                                             
1                                                                                                   
#FINOPTION                                                                                          
#PARAMOPT                                                                                           
Limite-O2 0.10000000                                                                                
Rendement 1.00000000                                                                                
Cible_thermique_simplifiée FAUX                                                                     
Seuil_décroissance 1.00000000                                                                       
Choix_pyrolyse 1.00000000                                                                           
Calc_masse_comb 1.00000000                                                                          
#FINPARAMOPT                                                                                        
 #FORMAT_RESULTAT
 2
 CALCUL
'  MAGIC 4.1.3                                               '
'Calculation date 2008        '
'Calculation author LICENCIE        '
#DEBUT ENTETE
#TIME
#NOUVELLE LIGNE
#ROOM#LOC_1 #Lower_Corridor    #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #Lower_Corridor    #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #Lower_Corridor    #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #Lower_Corridor    #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #Lower_Corridor    #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #Lower_Corridor    #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #Lower_Corridor    #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #Lower_Corridor    #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #Lower_Corridor    #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #Lower_Corridor    #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #Lower_Corridor    #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #Lower_Corridor    #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #Lower_Corridor    #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #Lower_Corridor    #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #Lower_Corridor    #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #Lower_Corridor    #VISIBILITY#m#Upper layer light-reflecting sign
#WALL#PAR_1 # #TEMPERATURE#K#Internal temperature
#WALL#PAR_1 # #TEMPERATURE#K#External temperature
#WALL#PAR_2 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_2 # #TEMPERATURE#K#Internal upper temperature
#NOUVELLE LIGNE
#WALL#PAR_2 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_2 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_3 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_3 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_3 # #TEMPERATURE#K#External lower temperature
#NOUVELLE LIGNE
#WALL#PAR_3 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_4 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_4 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_4 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_4 # #TEMPERATURE#K#External upper temperature
#NOUVELLE LIGNE
#WALL#PAR_5 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_5 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_5 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_5 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_6 # #TEMPERATURE#K#Internal temperature
#NOUVELLE LIGNE
#WALL#PAR_6 # #TEMPERATURE#K#External temperature
#WALL#PAR_1 # #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_1 # #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_6 # #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_6 # #AREA_HEAT_FLUX#W/m2#Total heat flux
#NOUVELLE LIGNE
#WALL#PAR_6 # #AREA_HEAT_FLUX#W/m2#Convective heat flux
#WALL#PAR_2 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_2 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_2 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_2 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_3 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_3 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_3 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_3 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_4 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_4 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_4 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_4 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_5 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_5 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_5 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_5 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#ROOM#LOC_1 #Lower_Corridor    #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #Lower_Corridor    #HEAT_POWER#W#Total sprinkling power
#TARGET#CIB_1 #HeatDet_1   #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_1 #HeatDet_1   #VELOCITY#m/s#Gas velocity
#TARGET#CIB_1 #HeatDet_1   #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_1 #HeatDet_1   #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_1 #HeatDet_1   #POSITION#m/m#R on H
#TARGET#CIB_2 #HeatDet_2   #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_2 #HeatDet_2   #VELOCITY#m/s#Gas velocity
#TARGET#CIB_2 #HeatDet_2   #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_2 #HeatDet_2   #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_2 #HeatDet_2   #POSITION#m/m#R on H
#TARGET#CIB_3 #HeatDet_3   #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_3 #HeatDet_3   #VELOCITY#m/s#Gas velocity
#TARGET#CIB_3 #HeatDet_3   #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_3 #HeatDet_3   #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_3 #HeatDet_3   #POSITION#m/m#R on H
#TARGET#CIB_5 #TC_1        #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_5 #TC_1        #VELOCITY#m/s#Gas velocity
#TARGET#CIB_5 #TC_1        #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_5 #TC_1        #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_5 #TC_1        #POSITION#m/m#R on H
#TARGET#CIB_6 #TC_2        #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_6 #TC_2        #VELOCITY#m/s#Gas velocity
#TARGET#CIB_6 #TC_2        #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_6 #TC_2        #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_6 #TC_2        #POSITION#m/m#R on H
#TARGET#CIB_7 #TC_3        #TEMPERATURE#K#Gas temperature
#NOUVELLE LIGNE
#TARGET#CIB_7 #TC_3        #VELOCITY#m/s#Gas velocity
#TARGET#CIB_7 #TC_3        #TEMPERATURE#K#Tmax gas CJ
#TARGET#CIB_7 #TC_3        #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_7 #TC_3        #POSITION#m/m#R on H
#TARGET#CIB_1 #HeatDet_1   #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#NOUVELLE LIGNE
#TARGET#CIB_1 #HeatDet_1   #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_1 #HeatDet_1   #AREA_HEAT_FLUX#W/m2#Convective heat flux 'fluxmeter'
#TARGET#CIB_1 #HeatDet_1   #AREA_HEAT_FLUX#W/m2#Radiation flux 'fluxmeter'
#TARGET#CIB_1 #HeatDet_1   #TEMPERATURE#K#Temperature of the detector
#TARGET#CIB_2 #HeatDet_2   #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#NOUVELLE LIGNE
#TARGET#CIB_2 #HeatDet_2   #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_2 #HeatDet_2   #AREA_HEAT_FLUX#W/m2#Convective heat flux 'fluxmeter'
#TARGET#CIB_2 #HeatDet_2   #AREA_HEAT_FLUX#W/m2#Radiation flux 'fluxmeter'
#TARGET#CIB_2 #HeatDet_2   #TEMPERATURE#K#Temperature of the detector
#TARGET#CIB_3 #HeatDet_3   #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#NOUVELLE LIGNE
#TARGET#CIB_3 #HeatDet_3   #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_3 #HeatDet_3   #AREA_HEAT_FLUX#W/m2#Convective heat flux 'fluxmeter'
#TARGET#CIB_3 #HeatDet_3   #AREA_HEAT_FLUX#W/m2#Radiation flux 'fluxmeter'
#TARGET#CIB_3 #HeatDet_3   #TEMPERATURE#K#Temperature of the detector
#TARGET#CIB_5 #TC_1        #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#NOUVELLE LIGNE
#TARGET#CIB_5 #TC_1        #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_5 #TC_1        #AREA_HEAT_FLUX#W/m2#Convective heat flux
#TARGET#CIB_5 #TC_1        #AREA_HEAT_FLUX#W/m2#Radiation flux
#TARGET#CIB_5 #TC_1        #AREA_HEAT_FLUX#W/m2#Total absorbed heat flux
#TARGET#CIB_5 #TC_1        #TEMPERATURE#K#Surface temperature of the target
#NOUVELLE LIGNE
#TARGET#CIB_5 #TC_1        #TEMPERATURE#K#Center Temperature of the target
#TARGET#CIB_6 #TC_2        #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#TARGET#CIB_6 #TC_2        #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_6 #TC_2        #AREA_HEAT_FLUX#W/m2#Convective heat flux
#TARGET#CIB_6 #TC_2        #AREA_HEAT_FLUX#W/m2#Radiation flux
#NOUVELLE LIGNE
#TARGET#CIB_6 #TC_2        #AREA_HEAT_FLUX#W/m2#Total absorbed heat flux
#TARGET#CIB_6 #TC_2        #TEMPERATURE#K#Surface temperature of the target
#TARGET#CIB_6 #TC_2        #TEMPERATURE#K#Center Temperature of the target
#TARGET#CIB_7 #TC_3        #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#TARGET#CIB_7 #TC_3        #AREA_HEAT_FLUX#W/m2#Incident heat flux
#NOUVELLE LIGNE
#TARGET#CIB_7 #TC_3        #AREA_HEAT_FLUX#W/m2#Convective heat flux
#TARGET#CIB_7 #TC_3        #AREA_HEAT_FLUX#W/m2#Radiation flux
#TARGET#CIB_7 #TC_3        #AREA_HEAT_FLUX#W/m2#Total absorbed heat flux
#TARGET#CIB_7 #TC_3        #TEMPERATURE#K#Surface temperature of the target
#TARGET#CIB_7 #TC_3        #TEMPERATURE#K#Center Temperature of the target
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #TEMPERATURE#K#Medium plume temperature
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #ATF_100kW   #MASS#kg#Consumed combustible mass
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#HEAT_POWER#W#Heat Release Rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#HEIGHT#m#Length of flame
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_3 #Ope_ 1_Roo 1#MASS#kg#Consumed combustible mass
#ROOM#LOC_2 #Upper_Corridor    #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Upper_Corridor    #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Upper_Corridor    #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_2 #Upper_Corridor    #TEMPERATURE#K#Geometrical average temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Upper_Corridor    #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Upper_Corridor    #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Upper_Corridor    #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Upper_Corridor    #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_2 #Upper_Corridor    #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Upper_Corridor    #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Upper_Corridor    #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Upper_Corridor    #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Upper_Corridor    #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_2 #Upper_Corridor    #VISIBILITY#m#Lower layer light-reflecting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Upper_Corridor    #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Upper_Corridor    #VISIBILITY#m#Upper layer light-reflecting sign
#WALL#PAR_7 # #TEMPERATURE#K#Internal temperature
#WALL#PAR_7 # #TEMPERATURE#K#External temperature
#WALL#PAR_8 # #TEMPERATURE#K#Internal lower temperature
#NOUVELLE LIGNE
#WALL#PAR_8 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_8 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_8 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_9 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_9 # #TEMPERATURE#K#Internal upper temperature
#NOUVELLE LIGNE
#WALL#PAR_9 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_9 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_10# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_10# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_10# #TEMPERATURE#K#External lower temperature
#NOUVELLE LIGNE
#WALL#PAR_10# #TEMPERATURE#K#External upper temperature
#WALL#PAR_11# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_11# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_11# #TEMPERATURE#K#External lower temperature
#WALL#PAR_11# #TEMPERATURE#K#External upper temperature
#NOUVELLE LIGNE
#WALL#PAR_12# #TEMPERATURE#K#Internal temperature
#WALL#PAR_12# #TEMPERATURE#K#External temperature
#WALL#PAR_7 # #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_7 # #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#NOUVELLE LIGNE
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Convective heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#ROOM#LOC_2 #Upper_Corridor    #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_2 #Upper_Corridor    #HEAT_POWER#W#Total sprinkling power
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#TEMPERATURE#K#Medium plume temperature
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_4 #Ope_ 3_Roo 2#MASS#kg#Consumed combustible mass
#ROOM#LOC_3 #Stairwell         #HEIGHT#m#Layer interface height
#ROOM#LOC_3 #Stairwell         #TEMPERATURE#K#Lower layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_3 #Stairwell         #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_3 #Stairwell         #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_3 #Stairwell         #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_3 #Stairwell         #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_3 #Stairwell         #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#NOUVELLE LIGNE
#ROOM#LOC_3 #Stairwell         #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_3 #Stairwell         #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_3 #Stairwell         #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_3 #Stairwell         #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_3 #Stairwell         #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#NOUVELLE LIGNE
#ROOM#LOC_3 #Stairwell         #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_3 #Stairwell         #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_3 #Stairwell         #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_3 #Stairwell         #VISIBILITY#m#Upper layer light-reflecting sign
#WALL#PAR_13# #TEMPERATURE#K#Internal temperature
#NOUVELLE LIGNE
#WALL#PAR_13# #TEMPERATURE#K#External temperature
#WALL#PAR_14# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_14# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_14# #TEMPERATURE#K#External lower temperature
#WALL#PAR_14# #TEMPERATURE#K#External upper temperature
#NOUVELLE LIGNE
#WALL#PAR_15# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_15# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_15# #TEMPERATURE#K#External lower temperature
#WALL#PAR_15# #TEMPERATURE#K#External upper temperature
#WALL#PAR_16# #TEMPERATURE#K#Internal lower temperature
#NOUVELLE LIGNE
#WALL#PAR_16# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_16# #TEMPERATURE#K#External lower temperature
#WALL#PAR_16# #TEMPERATURE#K#External upper temperature
#WALL#PAR_17# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_17# #TEMPERATURE#K#Internal upper temperature
#NOUVELLE LIGNE
#WALL#PAR_17# #TEMPERATURE#K#External lower temperature
#WALL#PAR_17# #TEMPERATURE#K#External upper temperature
#WALL#PAR_18# #TEMPERATURE#K#Internal temperature
#WALL#PAR_18# #TEMPERATURE#K#External temperature
#WALL#PAR_13# #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#NOUVELLE LIGNE
#WALL#PAR_13# #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_18# #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_18# #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_18# #AREA_HEAT_FLUX#W/m2#Convective heat flux
#WALL#PAR_14# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_14# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_14# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_14# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_15# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_15# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_15# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_15# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_16# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_16# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_16# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_16# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_17# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_17# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_17# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_17# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#NOUVELLE LIGNE
#ROOM#LOC_3 #Stairwell         #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_3 #Stairwell         #HEAT_POWER#W#Total sprinkling power
#TARGET#CIB_4 #Heat_4      #TEMPERATURE#K#Gas temperature
#TARGET#CIB_4 #Heat_4      #VELOCITY#m/s#Gas velocity
#TARGET#CIB_4 #Heat_4      #TEMPERATURE#K#Tmax gas CJ
#NOUVELLE LIGNE
#TARGET#CIB_4 #Heat_4      #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_4 #Heat_4      #POSITION#m/m#R on H
#TARGET#CIB_8 #TC_4        #TEMPERATURE#K#Gas temperature
#TARGET#CIB_8 #TC_4        #VELOCITY#m/s#Gas velocity
#TARGET#CIB_8 #TC_4        #TEMPERATURE#K#Tmax gas CJ
#NOUVELLE LIGNE
#TARGET#CIB_8 #TC_4        #HEIGHT#m#Distance below ceiling surface where Tgas=Tmax
#TARGET#CIB_8 #TC_4        #POSITION#m/m#R on H
#TARGET#CIB_4 #Heat_4      #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#TARGET#CIB_4 #Heat_4      #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_4 #Heat_4      #AREA_HEAT_FLUX#W/m2#Convective heat flux 'fluxmeter'
#NOUVELLE LIGNE
#TARGET#CIB_4 #Heat_4      #AREA_HEAT_FLUX#W/m2#Radiation flux 'fluxmeter'
#TARGET#CIB_4 #Heat_4      #TEMPERATURE#K#Temperature of the detector
#TARGET#CIB_8 #TC_4        #AREA_HEAT_FLUX#W/m2#Total heat flux 'fluxmeter'
#TARGET#CIB_8 #TC_4        #AREA_HEAT_FLUX#W/m2#Incident heat flux
#TARGET#CIB_8 #TC_4        #AREA_HEAT_FLUX#W/m2#Convective heat flux
#NOUVELLE LIGNE
#TARGET#CIB_8 #TC_4        #AREA_HEAT_FLUX#W/m2#Radiation flux
#TARGET#CIB_8 #TC_4        #AREA_HEAT_FLUX#W/m2#Total absorbed heat flux
#TARGET#CIB_8 #TC_4        #TEMPERATURE#K#Surface temperature of the target
#TARGET#CIB_8 #TC_4        #TEMPERATURE#K#Center Temperature of the target
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#HEIGHT#m#Flame height
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 3#MASS#kg#Consumed combustible mass
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#HEAT_POWER#W#Lower layer Heat Release Rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_5 #Ope_ 3_Roo 3#MASS#kg#Consumed combustible mass
#NOUVELLE LIGNE
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Upstream upper layer to downstream upper layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Upstream upper layer to downstream lower layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Upstream lower layer to downstream upper layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Upstream lower layer to downstream lower layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Downstream upper layer to upstream upper layer
#NOUVELLE LIGNE
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Downstream lower layer to upstream upper layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Downstream upper layer to upstream lower layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Downstream lower layer to upstream lower layer
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Upstream to downstream
#OPENING#OUV_1 #LowerCorrido#MASS_FLOW_RATE#kg/s#Downstream to upstream
#NOUVELLE LIGNE
#OPENING#OUV_1 #LowerCorrido#HEIGHT#m#Upstream upper layer to downstream flow height
#OPENING#OUV_1 #LowerCorrido#HEIGHT#m#Upstream lower layer to downstream flow height
#OPENING#OUV_1 #LowerCorrido#HEIGHT#m#Downstream upper layer to upstream flow height
#OPENING#OUV_1 #LowerCorrido#HEIGHT#m#Downstream lower layer to upstream flow height
#OPENING#OUV_1 #LowerCorrido#AREA#m2#Area
#NOUVELLE LIGNE
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Outside to upper layer
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Upper layer to the outside
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Outside to lower layer
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Lower layer to the outside
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Global mass flow rate to the outside
#NOUVELLE LIGNE
#OPENING#OUV_2 #UpperCorrido#MASS_FLOW_RATE#kg/s#Outside to upper and lower layers
#OPENING#OUV_2 #UpperCorrido#HEIGHT#m#Upper layer to the outside flow height
#OPENING#OUV_2 #UpperCorrido#HEIGHT#m#Lower layer to the outside flow height
#OPENING#OUV_2 #UpperCorrido#HEIGHT#m#Outside to inside flow height
#OPENING#OUV_2 #UpperCorrido#AREA#m2#Area
#NOUVELLE LIGNE
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Upstream upper layer to downstream upper layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Upstream upper layer to downstream lower layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Upstream lower layer to downstream upper layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Upstream lower layer to downstream lower layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Downstream upper layer to upstream upper layer
#NOUVELLE LIGNE
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Downstream lower layer to upstream upper layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Downstream upper layer to upstream lower layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Downstream lower layer to upstream lower layer
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Upstream to downstream
#OPENING#OUV_3 #Stairwell_Up#MASS_FLOW_RATE#kg/s#Downstream to upstream
#NOUVELLE LIGNE
#OPENING#OUV_3 #Stairwell_Up#HEIGHT#m#Upstream upper layer to downstream flow height
#OPENING#OUV_3 #Stairwell_Up#HEIGHT#m#Upstream lower layer to downstream flow height
#OPENING#OUV_3 #Stairwell_Up#HEIGHT#m#Downstream upper layer to upstream flow height
#OPENING#OUV_3 #Stairwell_Up#HEIGHT#m#Downstream lower layer to upstream flow height
#OPENING#OUV_3 #Stairwell_Up#AREA#m2#Area
#NOUVELLE LIGNE
#FIN ENTETE
#DEBUTRESULTAT
      0.00000000
 0.25000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.63452693799179E-06
 0.10000000000062E-02 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03 0.29315000000001E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000002E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000001E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.25407439830542E+02 0.25407439830542E+02 0.91409801291082E+01 0.91866850297644E+01
 0.10652452454569E-10 0.13940032821121E+02 0.50528818790073E+01 0.13940032821121E+02 0.50528818790073E+01
 0.31565322076868E+02 0.50521868649681E+01 0.31565322076868E+02 0.50521868649681E+01 0.13940032821121E+02
 0.50528818790074E+01 0.13940032821121E+02 0.50528818790074E+01 0.22368174063151E+01 0.50521868649681E+01
 0.22368174063151E+01 0.50521868649681E+01 0.23955926381950E+01 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29992461643959E+03 0.46339374925158E-01 0.95357068545242E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29470699994847E+03 0.17893811785030E+00 0.42859788278142E+01 0.29315000000000E+03
 0.00000000000000E+00 0.29402624704362E+03 0.27491246439131E+00 0.69073886629167E+01 0.29315000000000E+03
 0.00000000000000E+00 0.30012427189296E+03 0.45027174627534E-01 0.92979162808413E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29471515965224E+03 0.17818627677887E+00 0.42621707818015E+01 0.29315000000000E+03
 0.00000000000000E+00 0.29402957843933E+03 0.27419571687010E+00 0.68835882247613E+01 0.18605152838114E+02
 0.43734342142338E+03 0.54569682106376E-10 0.18605157547510E+02 0.29315000000000E+03 0.24183507853632E+01
 0.42115661937062E+03 0.54569682106376E-10 0.24183554947588E+01 0.29315000000000E+03 0.13091865626913E+01
 0.42004745514795E+03 0.54569682106376E-10 0.13091912720869E+01 0.29315000000000E+03 0.19282598192194E+02
 0.43802086677746E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.24259449914684E+01 0.42116421357673E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13303132447085E+01 0.42006858182997E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.23000000000218E-03 0.11500000000109E+05 0.11500000000109E+05 0.50000000000000E+08 0.30316835279335E+03
 0.13609036469163E+00 0.13609036469163E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 0.54364792247790E-06 0.99970725464972E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.43811029893013E-03 0.43811029893013E-03 0.64339701226783E-03
 0.64661399732917E-03 0.00000000000000E+00 0.41906623042675E-03 0.50769943802902E-03 0.41906623042675E-03
 0.50769943802902E-03 0.41257063501201E-03 0.50768727592664E-03 0.41257063501201E-03 0.50768727592664E-03
 0.41906623042675E-03 0.50769943808652E-03 0.41906623042675E-03 0.50769943808652E-03 0.41257063512700E-03
 0.50768727604164E-03 0.41257063512700E-03 0.50768727604164E-03 0.72526328767434E-04 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29315000000009E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29315000000009E+03 0.23000000000000E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 0.13703206490182E-05 0.10000000000132E-02 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.36307940225908E-03
 0.36307940225908E-03 0.63469062418447E-03 0.63786407730540E-03 0.00000000000000E+00 0.37793395925277E-03
 0.50287268965546E-03 0.37793395925277E-03 0.50287268965546E-03 0.37748809417286E-03 0.50287741215895E-03
 0.37748809417286E-03 0.50287741215895E-03 0.37793395925277E-03 0.50287268965546E-03 0.37793395925277E-03
 0.50287268965546E-03 0.37748809423036E-03 0.50287741221645E-03 0.37748809423036E-03 0.50287741221645E-03
 0.30308231771647E-04 0.00000000000000E+00 0.29315000000009E+03 0.00000000000000E+00 0.29315000000009E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000009E+03 0.00000000000000E+00 0.29315000000009E+03
 0.00000000000000E+00 0.00000000000000E+00 0.10711763613216E+00 0.41884538622014E+03 0.13105250218359E-08
 0.10712234427183E+00 0.29315000000000E+03 0.10821466176316E+00 0.41884648324577E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.57018150095952E-04 0.00000000000000E+00 0.57018150095952E-04
 0.25000000000000E+01 0.12500000000000E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.21010557987726E+00 0.21010557987726E+00
 0.00000000000000E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.41927215382949E+00 0.00000000000000E+00 0.41927215382949E+00
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     10.14262215
 0.22576467846242E+01 0.29315229315454E+03 0.30583895923687E+03 0.29438215488152E+03 0.29433589794006E+03
 0.22999994059856E+00 0.00000000000000E+00 0.22831877731109E+00 0.00000000000000E+00 -.12445101718561E-01
 0.99995057272989E-03 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29322364062628E+03 0.29315001103318E+03 0.29339263893292E+03 0.29438925053465E+03
 0.29315000000000E+03 0.29315000000001E+03 0.29367763578520E+03 0.29438681312467E+03 0.29315000000000E+03
 0.29315000000001E+03 0.29339566939690E+03 0.29441418849575E+03 0.29315133145841E+03 0.29315133136724E+03
 0.29319274042031E+03 0.29438681312467E+03 0.29315000000000E+03 0.29315000000001E+03 0.29332775195961E+03
 0.29315001743566E+03 0.48931397360422E+02 0.48876641395287E+02 0.20310006680670E+02 0.11863000442884E+03
 0.98218447714768E+02 0.31515977411194E+02 0.63482814882866E+01 0.31284019246918E+02 0.12028551082476E+03
 0.67895448643780E+02 0.59080934258645E+01 0.67251850161591E+02 0.11987231036819E+03 0.31494645491128E+02
 0.62112464363562E+01 0.31258867875785E+02 0.11987240117181E+03 0.55746985706146E+01 0.59080934258658E+01
 0.55517912919918E+01 0.11987231036820E+03 0.89159928114801E+01 0.00000000000000E+00 0.31035869787258E+03
 0.31964617389213E+00 0.31243050643088E+03 0.46958171155747E-01 0.84286863543840E+00 0.30592162114265E+03
 0.89474486201719E-01 0.30609420125725E+03 0.18105036983817E+00 0.37884104253936E+01 0.30583895923687E+03
 0.51889980909752E-01 0.30583895923687E+03 0.00000000000000E+00 0.61054952145398E+01 0.31037541154854E+03
 0.32098182559213E+00 0.31260325359850E+03 0.45956684597702E-01 0.82185013944036E+00 0.30594598611803E+03
 0.90005320617715E-01 0.30610640794141E+03 0.18018913621876E+00 0.37673663061045E+01 0.30583895923687E+03
 0.52087666151643E-01 0.30583895923687E+03 0.00000000000000E+00 0.60844578198958E+01 0.31946934779002E+03
 0.48007714828660E+03 0.25813046808874E+03 0.61338884410732E+02 0.29819606686572E+03 0.21856899656725E+03
 0.44573294801287E+03 0.19157431713970E+03 0.26994684137000E+02 0.29535267318363E+03 0.21652118176160E+03
 0.44492506179383E+03 0.19033438855309E+03 0.26186797917961E+02 0.29487206562125E+03 0.32123333716846E+03
 0.48159043252566E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.21894855512142E+03 0.44574703193635E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.21654195617287E+03 0.44494583620510E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.47849424263602E-03 0.23924712131801E+05 0.23924712131801E+05 0.50000000000000E+08 0.31344767547873E+03
 0.33870532727770E+00 0.33870532727770E+00 0.36132221248841E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29315026010007E+03 0.29315026010007E+03 0.29315026010007E+03
 0.29315026010007E+03 0.22999996603706E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.26510466424763E-02 0.99968169842293E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315001740291E+03 0.29332858811970E+03 0.29315005249684E+03
 0.29315005249684E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315005350368E+03 0.29315005350368E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315005294275E+03 0.29315005294275E+03 0.29315133145841E+03
 0.29315118807064E+03 0.29315005350368E+03 0.29315005350368E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315006685976E+03 0.29315000000000E+03 0.79576478117958E-02 0.79576478117958E-02 0.10388843113412E-01
 0.10440787328979E-01 0.00000000000000E+00 0.82368459307785E-02 0.59681562706828E-02 0.82636855909881E-02
 0.81454081186615E+10 0.84022187997126E-02 0.59626959264278E-02 0.84288950824965E-02 0.81059044334794E+10
 0.82336915210721E-02 0.59659721329808E-02 0.82604587987669E-02 0.81279127090500E+10 0.84022187998276E-02
 0.59626959264278E-02 0.84288950826115E-02 0.81059044334794E+10 0.13412641224014E-02 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29327812691149E+03
 0.29327812691149E+03 0.29327812691149E+03 0.29327812691149E+03 0.22998331358815E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.17211548765169E-01 0.98840803302081E-03 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315178487965E+03
 0.29315000012390E+03 0.29315915618188E+03 0.29315915618188E+03 0.29330236709884E+03 0.29328861376712E+03
 0.29315909395873E+03 0.29315909395873E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315908242723E+03
 0.29315908242723E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315909395873E+03 0.29315909395873E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29316030394705E+03 0.29315000000000E+03 0.14616639295144E+01
 0.15193778394036E+01 0.15284243235476E+01 0.16418486537220E+01 0.10578220855659E+00 0.13591972647879E+01
 0.85082997358238E+00 0.14460680317144E+01 0.85082997358238E+00 0.13614314807037E+01 0.85116691735379E+00
 0.14483617898721E+01 0.85116691735379E+00 0.13596365214584E+01 0.85120927796355E+00 0.14465778663563E+01
 0.85120927796355E+00 0.13614314807037E+01 0.85116691735384E+00 0.14483617898721E+01 0.85116691735384E+00
 0.11053881508507E+00 0.00000000000000E+00 0.11466011777407E+04 0.19514129954922E+01 0.29327812691149E+03
 0.00000000000000E+00 0.00000000000000E+00 0.11466011777407E+04 0.19514129954922E+01 0.29327812691149E+03
 0.00000000000000E+00 0.00000000000000E+00 0.12810053507115E+05 0.42702410959039E+03 0.12801767666110E+05
 0.82858457145211E+01 0.30464999304056E+03 0.12810096780850E+05 0.42706738332590E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29482637587967E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.13598646780214E+00 0.00000000000000E+00 0.92850388967359E-01 0.00000000000000E+00
 0.27090254727224E-03 0.00000000000000E+00 0.18232659420230E+00 0.22883685676950E+00 0.18259749674957E+00
 0.23788233923121E+01 0.11288233923121E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.49887575707074E-01 0.10197725052623E+00 0.10197725052623E+00
 0.49887575707074E-01 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.83277334240405E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.31224006395136E-01 0.83277334240405E-01 0.31224006395136E-01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     20.41513962
 0.20758901787929E+01 0.29317781186743E+03 0.31115021319367E+03 0.29622672063269E+03 0.29607904074096E+03
 0.22999726599479E+00 0.00000000000000E+00 0.22756689010486E+00 0.00000000000000E+00 -.91271267856603E-01
 0.99839992378845E-03 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29329181412801E+03 0.29315036010234E+03 0.29366091195731E+03 0.29505801619336E+03
 0.29315000000082E+03 0.29315000000978E+03 0.29423121958964E+03 0.29504963275673E+03 0.29315000000183E+03
 0.29315000000977E+03 0.29367220961311E+03 0.29510502896483E+03 0.29315447169358E+03 0.29315447163535E+03
 0.29324089638576E+03 0.29504963275673E+03 0.29315000000014E+03 0.29315000000977E+03 0.29351193927784E+03
 0.29315063398438E+03 0.70718902032601E+02 0.70620538214683E+02 0.32824397839612E+02 0.18668128485559E+03
 0.15369276502678E+03 0.51183833517477E+02 0.11862016308637E+02 0.50603042791443E+02 0.15645951130333E+03
 0.10698087602306E+03 0.10780056728606E+02 0.10537237007589E+03 0.15546299669334E+03 0.51105120824366E+02
 0.11592219468527E+02 0.50506538332241E+02 0.15571068690483E+03 0.90994907771698E+01 0.10780056728606E+02
 0.90583646980708E+01 0.15546299669334E+03 0.14357254284171E+02 0.00000000000000E+00 0.31833560385381E+03
 0.38213722255720E+00 0.32173661046209E+03 0.46489935915788E-01 0.86947195565473E+00 0.31167504170432E+03
 0.10714240777390E+00 0.31191261375546E+03 0.18051087425192E+00 0.39079833830529E+01 0.31115021319367E+03
 0.62185684198820E-01 0.31115021319367E+03 0.00000000000000E+00 0.62982019275939E+01 0.31835276459189E+03
 0.38352617896513E+00 0.32199906648189E+03 0.45521387305612E-01 0.84779005642161E+00 0.31170967042582E+03
 0.10775821914791E+00 0.31193059994621E+03 0.17967860042919E+00 0.38862750517852E+01 0.31115021319367E+03
 0.62415366250888E-01 0.31115021319367E+03 0.00000000000000E+00 0.62765005332200E+01 0.47715805093729E+03
 0.51811226171541E+03 0.37778405780720E+03 0.99373997839540E+02 0.30379765853247E+03 0.32412827157715E+03
 0.46499091459770E+03 0.27787562556477E+03 0.46252650721832E+02 0.29826082675871E+03 0.31293839830698E+03
 0.46167346898727E+03 0.27000319790502E+03 0.42935205111405E+02 0.29719901668956E+03 0.47984268361797E+03
 0.52053948332491E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.32466851707599E+03 0.46501172927404E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.31319733818352E+03 0.46193240886382E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.73017092062089E-03 0.36508546031044E+05 0.36508546031044E+05 0.50000000000000E+08 0.32398759281670E+03
 0.48563004452151E+00 0.48563004452151E+00 0.98696834508496E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29316567525927E+03 0.29316567525927E+03 0.29316567525927E+03
 0.29316567525927E+03 0.22999792088501E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.27810943116581E-02 0.99851492484747E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315063385459E+03 0.29351358702001E+03 0.29315019005283E+03
 0.29315019005283E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315019313952E+03 0.29315019313952E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315019303165E+03 0.29315019303165E+03 0.29315447169341E+03
 0.29315361204726E+03 0.29315019313952E+03 0.29315019313952E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315023871966E+03 0.29315000000000E+03 0.15724373661774E-01 0.19635366840306E-01 0.24430652578530E-01
 0.32632483237147E-01 0.80796773957251E-02 0.19623908752933E-01 0.13908588886261E-01 0.26164438842903E-01
 0.60758214505817E+12 0.19973530859144E-01 0.13908588886261E-01 0.26512378503736E-01 0.60746103729042E+12
 0.19602836165587E-01 0.13908588886261E-01 0.26141742604593E-01 0.60746526961959E+12 0.19973530859202E-01
 0.13908588886261E-01 0.26512378503793E-01 0.60746103729042E+12 0.40711645118213E-02 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29394805465469E+03
 0.29394805465469E+03 0.29394805465469E+03 0.29394805465469E+03 0.22989348845074E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.11597647275926E+00 0.94394192268509E-03 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315571813636E+03
 0.29315000641658E+03 0.29318127450820E+03 0.29318127450820E+03 0.29349345385019E+03 0.29343836858239E+03
 0.29318078920689E+03 0.29318078920689E+03 0.29315000000002E+03 0.29315000000002E+03 0.29318075663469E+03
 0.29318075663469E+03 0.29315000000002E+03 0.29315000000002E+03 0.29318078920689E+03 0.29318078920689E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29318541697682E+03 0.29315000000002E+03 0.30919981375936E+01
 0.36643955092441E+01 0.31645837572724E+01 0.44541624009653E+01 0.12737557249066E+01 0.28218599566135E+01
 0.17705566089350E+01 0.38247086469295E+01 0.17705566089350E+01 0.28286401689782E+01 0.17730883506051E+01
 0.38323245703838E+01 0.17730883506051E+01 0.28249449564703E+01 0.17732205182843E+01 0.38286854547304E+01
 0.17732205182843E+01 0.28286401689777E+01 0.17730883506047E+01 0.38323245703832E+01 0.17730883506047E+01
 0.29043014831934E+00 0.00000000000000E+00 0.73044009283090E+03 0.23136654545146E+01 0.29394805465469E+03
 0.00000000000000E+00 0.00000000000000E+00 0.73044009283090E+03 0.23136654545146E+01 0.29394805465469E+03
 0.00000000000000E+00 0.00000000000000E+00 0.65756139258556E+04 0.43500080197749E+03 0.65593513924634E+04
 0.16262538101628E+02 0.30464999304056E+03 0.65757054606727E+04 0.43509233679457E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29701899302001E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.31324213295678E+00 0.00000000000000E+00 0.22972909436823E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.48400363718221E+00 0.54297122732502E+00 0.48400363718221E+00
 0.22879450893964E+01 0.10379450893964E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.45464532558229E-01 0.12228333848725E+00 0.12228333848725E+00
 0.45464532558229E-01 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.45961577180838E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.38401756691364E+00 0.45961577180838E+00 0.38401756691364E+00
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     30.54287662
 0.19555338410929E+01 0.29330070088043E+03 0.31705266097120E+03 0.29847355627128E+03 0.29816540223186E+03
 0.22998179888474E+00 0.00000000000000E+00 0.22669215712551E+00 0.00000000000000E+00 -.24520855298948E+00
 0.99135102106045E-03 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29336694271556E+03 0.29315200287092E+03 0.29398742570306E+03 0.29590307529659E+03
 0.29315000005785E+03 0.29315000052683E+03 0.29488485204557E+03 0.29588650569545E+03 0.29315000012786E+03
 0.29315000052651E+03 0.29400732048550E+03 0.29596392107837E+03 0.29316042469215E+03 0.29316042484107E+03
 0.29330069773202E+03 0.29588650569545E+03 0.29315000000999E+03 0.29315000052651E+03 0.29374349079734E+03
 0.29315430064584E+03 0.91138381829844E+02 0.91088487350360E+02 0.46359639790062E+02 0.26501920802129E+03
 0.21842777003227E+03 0.71389213194401E+02 0.17518736351791E+02 0.70465634164990E+02 0.20455394004571E+03
 0.14600255493048E+03 0.15763809527145E+02 0.14324797283580E+03 0.20296707840399E+03 0.71251436374671E+02
 0.17158518674997E+02 0.70292761583478E+02 0.20357676243989E+03 0.12827984401132E+02 0.15763809527141E+02
 0.12827984401132E+02 0.20296707840398E+03 0.20301117845198E+02 0.00000000000000E+00 0.32653901194238E+03
 0.42603481299263E+00 0.33092665854326E+03 0.46826691823092E-01 0.88116146372884E+00 0.31784101987192E+03
 0.11957058570494E+00 0.31815805183389E+03 0.18057739785435E+00 0.39605237818691E+01 0.31705266097120E+03
 0.69482486119210E-01 0.31705266097120E+03 0.00000000000000E+00 0.63828773237423E+01 0.32657137556881E+03
 0.42838295329969E+00 0.33143297966872E+03 0.45395628905658E-01 0.85918806488552E+00 0.31790840147501E+03
 0.12058841231353E+00 0.31819292543483E+03 0.17934727446564E+00 0.39385235956290E+01 0.31705266097120E+03
 0.69735479951307E-01 0.31705266097120E+03 0.00000000000000E+00 0.63608841676582E+01 0.63957276419972E+03
 0.55747585364934E+03 0.50083517913570E+03 0.13873758977347E+03 0.30464999304056E+03 0.43728242447628E+03
 0.48565539498278E+03 0.37036529807881E+03 0.66917131106916E+02 0.30049603806109E+03 0.41823261687836E+03
 0.47843097089569E+03 0.35853991456799E+03 0.59692707019823E+02 0.29897563720841E+03 0.64343601438455E+03
 0.56085364943776E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.43832033367776E+03 0.48568258013790E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.41882487172948E+03 0.47902322574680E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.97830047715885E-03 0.48915023857943E+05 0.48915023857943E+05 0.50000000000000E+08 0.33443175978188E+03
 0.60289386483267E+00 0.60289386483267E+00 0.18604015542429E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29325717930272E+03 0.29325717930272E+03 0.29325717930272E+03
 0.29325717930272E+03 0.22998545683605E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.54510210604078E-02 0.99285890371878E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315430032988E+03 0.29374574966869E+03 0.29315077930504E+03
 0.29315077930504E+03 0.29315000000001E+03 0.29315000000001E+03 0.29315078435585E+03 0.29315078435585E+03
 0.29315000000001E+03 0.29315000000001E+03 0.29315079015637E+03 0.29315079015637E+03 0.29316042466946E+03
 0.29315808706313E+03 0.29315078435585E+03 0.29315078435585E+03 0.29315000000001E+03 0.29315000000001E+03
 0.29315093363009E+03 0.29315000000001E+03 0.14974053029728E-01 0.58229984465203E-01 0.47246417506620E-01
 0.13754777110957E+00 0.90065121515421E-01 0.38608726062806E-01 0.26838684082031E-01 0.11811095958931E+00
 0.41752666768969E+13 0.39008216976204E-01 0.26838684082031E-01 0.11850553990597E+00 0.41750685056610E+13
 0.38531961650073E-01 0.26838684082031E-01 0.11802364517984E+00 0.41748409194183E+13 0.39008216976204E-01
 0.26838684082031E-01 0.11850553990597E+00 0.41750685056610E+13 0.17162783928415E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29523381720893E+03
 0.29523381720893E+03 0.29523381720893E+03 0.29523381720893E+03 0.22971481711747E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.30435859229229E+00 0.88211113676400E-03 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29316231742344E+03
 0.29315005086753E+03 0.29322375637527E+03 0.29322375637527E+03 0.29373659651954E+03 0.29362914902351E+03
 0.29322238038522E+03 0.29322238038522E+03 0.29315000000149E+03 0.29315000000151E+03 0.29322232040514E+03
 0.29322232040514E+03 0.29315000000149E+03 0.29315000000151E+03 0.29322238038522E+03 0.29322238038522E+03
 0.29315000000149E+03 0.29315000000151E+03 0.29323523186394E+03 0.29315000000169E+03 0.48556697350465E+01
 0.67561841038832E+01 0.48163153746479E+01 0.94320078940172E+01 0.45916109424961E+01 0.43174294877310E+01
 0.27212305781580E+01 0.78795011425274E+01 0.27212305781580E+01 0.43316264770285E+01 0.27283662219153E+01
 0.78969163028097E+01 0.27283662219153E+01 0.43259874589961E+01 0.27286202965184E+01 0.78914175825633E+01
 0.27286202965184E+01 0.43316264770272E+01 0.27283662219142E+01 0.78969163028079E+01 0.27283662219142E+01
 0.59396528163999E+00 0.00000000000000E+00 0.67653806057361E+03 0.26033780944388E+01 0.29523381720893E+03
 0.00000000000000E+00 0.00000000000000E+00 0.67653806057361E+03 0.26033780944388E+01 0.29523381720893E+03
 0.00000000000000E+00 0.00000000000000E+00 0.57741588233989E+04 0.44207618338010E+03 0.57508209086041E+04
 0.23337919504232E+02 0.30464999304056E+03 0.57742955797478E+04 0.44221293972895E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29957417846771E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.44546625046965E+00 0.00000000000000E+00 0.34066239705131E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.71799034774868E+00 0.78612864752096E+00 0.71799034774868E+00
 0.22277669205465E+01 0.97776692054645E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.65627670506021E-01 0.16452955732306E+00 0.16452955732306E+00
 0.65627670506021E-01 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.73910840730413E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.64512908308409E+00 0.73910840730413E+00 0.64512908308409E+00
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     40.05792138
 0.18805892944494E+01 0.29359934096490E+03 0.32312456982962E+03 0.30091463810196E+03 0.30040017489891E+03
 0.22994158635928E+00 0.00000000000000E+00 0.22575037105097E+00 0.00000000000000E+00 -.44355097334846E+00
 0.97665884991793E-03 0.19314405866434E-02 0.80000000000000E+04 0.30000000000000E+04 0.41419860674581E+04
 0.15532447752968E+04 0.29344461764279E+03 0.29315563193338E+03 0.29434752129292E+03 0.29687255616634E+03
 0.29315000075720E+03 0.29315000569480E+03 0.29558989585051E+03 0.29684677986397E+03 0.29315000166114E+03
 0.29315000568908E+03 0.29437539638570E+03 0.29694780082514E+03 0.29316969383171E+03 0.29316969655207E+03
 0.29336924853679E+03 0.29684677986397E+03 0.29315000013179E+03 0.29315000568908E+03 0.29401340137248E+03
 0.29316335574292E+03 0.10980945122029E+03 0.10988146579547E+03 0.60241358812849E+02 0.34818295607721E+03
 0.28764039047030E+03 0.90636543134475E+02 0.23064764593567E+02 0.89600898470207E+02 0.25697428358477E+03
 0.18254070131696E+03 0.20693619807188E+02 0.17881848462729E+03 0.25486846412535E+03 0.90443977859402E+02
 0.22608929304481E+02 0.89357665807079E+02 0.25574420243751E+03 0.16621986643033E+02 0.20693619807187E+02
 0.16844779918566E+02 0.25486846412535E+03 0.26367654009293E+02 0.00000000000000E+00 0.33459172858555E+03
 0.46005114326645E+00 0.34000279599615E+03 0.46534486008360E-01 0.88669457530920E+00 0.32407463001528E+03
 0.12923018328745E+00 0.32445940008692E+03 0.18026501758858E+00 0.39853932534743E+01 0.32312456982962E+03
 0.74996248267577E-01 0.32312456982962E+03 0.00000000000000E+00 0.64229575745133E+01 0.33461681283006E+03
 0.46170977103244E+00 0.34040811010679E+03 0.45586943491639E-01 0.86458319804467E+00 0.32412936373882E+03
 0.12995784656823E+00 0.32448738105434E+03 0.17945056690372E+00 0.39632549206058E+01 0.32312456982962E+03
 0.75267601398532E-01 0.32312456982962E+03 0.00000000000000E+00 0.64008263159456E+01 0.79942234416541E+03
 0.59653468396745E+03 0.62162592878328E+03 0.17779642009158E+03 0.30464999304056E+03 0.55230383941855E+03
 0.50717265777466E+03 0.46386945022921E+03 0.88434393898794E+02 0.30279088004277E+03 0.52685322886297E+03
 0.49597295000392E+03 0.44961854744436E+03 0.77234686128056E+02 0.30082409191077E+03 0.80410198295364E+03
 0.60083805908809E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.55315765693984E+03 0.50720546944289E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.52775152210714E+03 0.49687124324810E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.12114190737382E-02 0.60570953686911E+05 0.60570953686911E+05 0.50000000000000E+08 0.34427290876777E+03
 0.69767222950591E+00 0.69767222950591E+00 0.29113652015016E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29349143676968E+03 0.29349143676968E+03 0.29349143676968E+03
 0.29349143676968E+03 0.22995253386974E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.13092915781043E-01 0.98084237153280E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29316335481916E+03 0.29401622819561E+03 0.29315301364809E+03
 0.29315301364809E+03 0.29315000000014E+03 0.29315000000014E+03 0.29315301895701E+03 0.29315301895701E+03
 0.29315000000015E+03 0.29315000000015E+03 0.29315305544065E+03 0.29315305544065E+03 0.29316969343255E+03
 0.29316864716120E+03 0.29315301895701E+03 0.29315301895701E+03 0.29315000000015E+03 0.29315000000015E+03
 0.29315358173729E+03 0.29315000000018E+03 0.34443543244920E-02 0.18774001484709E+00 0.83180823635460E-01
 0.51411664125151E+00 0.43051991349788E+00 0.70049608186042E-01 0.49204254150391E-01 0.42931011064428E+00
 0.13284424337211E+14 0.70304846923096E-01 0.49204254150391E-01 0.42955798636338E+00 0.13284216037516E+14
 0.69753950867419E-01 0.49204254150391E-01 0.42895649161167E+00 0.13282784573510E+14 0.70304846924131E-01
 0.49204254150391E-01 0.42955798636468E+00 0.13284216037523E+14 0.62040973490807E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29687489143297E+03
 0.29687489143297E+03 0.29687489143297E+03 0.29687489143297E+03 0.22947687178499E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.54133511210404E+00 0.82886515977783E-03 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29317171478391E+03
 0.29315018708785E+03 0.29329051683829E+03 0.29329051683829E+03 0.29401515326662E+03 0.29397872202208E+03
 0.29328773805464E+03 0.29328773805464E+03 0.29315000002373E+03 0.29315000002378E+03 0.29328764674493E+03
 0.29328764674493E+03 0.29315000002370E+03 0.29315000002375E+03 0.29328773805464E+03 0.29328773805464E+03
 0.29315000002373E+03 0.29315000002378E+03 0.29331517290259E+03 0.29315000002692E+03 0.68077349334954E+01
 0.10730027624822E+02 0.65125050808838E+01 0.16429571539287E+02 0.98845039329985E+01 0.58781533272029E+01
 0.37266531242416E+01 0.13497597473060E+02 0.37266531242416E+01 0.59029670717659E+01 0.37410378926274E+01
 0.13530219820625E+02 0.37410378926274E+01 0.58952700685998E+01 0.37414330388333E+01 0.13522779438386E+02
 0.37414330388333E+01 0.59029670717646E+01 0.37410378926264E+01 0.13530219820623E+02 0.37410378926264E+01
 0.10128661877301E+01 0.00000000000000E+00 0.66884187280044E+03 0.28029892699040E+01 0.29687489143297E+03
 0.00000000000000E+00 0.00000000000000E+00 0.66884187280044E+03 0.28029892699040E+01 0.29687489143297E+03
 0.00000000000000E+00 0.00000000000000E+00 0.56658582400198E+04 0.44921841659842E+03 0.56353780920067E+04
 0.30480152722559E+02 0.30464999304056E+03 0.56660362517521E+04 0.44939642833072E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.30230587422882E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.53572695329909E+00 0.00000000000000E+00 0.42992988697211E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.89072390428401E+00 0.96565684027120E+00 0.89072390428401E+00
 0.21902946472247E+01 0.94029464722471E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.11044173399605E+00 0.22774224444344E+00 0.22774224444344E+00
 0.11044173399605E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.96271903767457E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.85622128764283E+00 0.96271903767457E+00 0.85622128764283E+00
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     50.25550915
 0.18288648240096E+01 0.29419663953353E+03 0.32998236214913E+03 0.30380346243176E+03 0.30301845851466E+03
 0.22985708465737E+00 0.00000000000000E+00 0.22461773153814E+00 0.00000000000000E+00 -.69555650340518E+00
 0.11016067351370E-02 0.45858852107115E-01 0.72621197246084E+04 0.27232948967281E+04 0.17444832638449E+03
 0.65418122394183E+02 0.29353809402246E+03 0.29316247519301E+03 0.29479673449343E+03 0.29812318674532E+03
 0.29315000537284E+03 0.29315003441522E+03 0.29643926628602E+03 0.29808747002880E+03 0.29315001169460E+03
 0.29315003436394E+03 0.29483317104263E+03 0.29821653140589E+03 0.29318403141739E+03 0.29318405079577E+03
 0.29347197184959E+03 0.29808747002880E+03 0.29315000094090E+03 0.29315003436394E+03 0.29436169015902E+03
 0.29318194626298E+03 0.13256697815990E+03 0.13300697438979E+03 0.81633915361978E+02 0.45034200391668E+03
 0.36829991897789E+03 0.11353215186378E+03 0.35400614848899E+02 0.11275462880670E+03 0.32397524154154E+03
 0.22336497749099E+03 0.32547606541602E+02 0.21901079659095E+03 0.32149804989891E+03 0.11328040762102E+03
 0.34825438585445E+02 0.11244073588382E+03 0.32241826398918E+03 0.23049557089891E+02 0.32547606541600E+02
 0.24044709224912E+02 0.32149804989891E+03 0.33803752175820E+02 0.00000000000000E+00 0.34342685540366E+03
 0.49061122542924E+00 0.34984101254753E+03 0.46374951756407E-01 0.88974488911925E+00 0.33102665267400E+03
 0.13792453939627E+00 0.33148714014850E+03 0.18005269113339E+00 0.39991033859349E+01 0.32998236214913E+03
 0.79990505804981E-01 0.32998236214913E+03 0.00000000000000E+00 0.64450531604529E+01 0.34345443756457E+03
 0.49230429715068E+00 0.35031826844759E+03 0.45430657701193E-01 0.86755744661049E+00 0.33109188558833E+03
 0.13869910061542E+00 0.33152008597095E+03 0.17924123654041E+00 0.39768888950923E+01 0.32998236214913E+03
 0.80279016186210E-01 0.32998236214913E+03 0.00000000000000E+00 0.64228457682474E+01 0.99134493666811E+03
 0.65593037419849E+03 0.75415283105494E+03 0.23719211032263E+03 0.30464999304056E+03 0.70102317377196E+03
 0.55161165224728E+03 0.56814979011000E+03 0.13287338837142E+03 0.30464999304056E+03 0.66889563648509E+03
 0.53514847283346E+03 0.55248543223695E+03 0.11641020895760E+03 0.30291644963111E+03 0.99695337558086E+03
 0.66112508069761E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.70204168839253E+03 0.55165167315290E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.67010511191493E+03 0.53635794826330E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.14612599742068E-02 0.73062998710339E+05 0.73062998710339E+05 0.50000000000000E+08 0.35475522409427E+03
 0.78776319593424E+00 0.78776319593424E+00 0.42823933061941E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29396913221798E+03 0.29396913221798E+03 0.29396913221798E+03
 0.29396913221798E+03 0.22988274429871E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.29559661445207E-01 0.10831147603427E-02 0.10000000000000E-02 0.73861056029454E+04 0.27697896011045E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29318194317967E+03 0.29436517794101E+03 0.29315970957861E+03
 0.29315970957861E+03 0.29315000000125E+03 0.29315000000127E+03 0.29315971217752E+03 0.29315971217752E+03
 0.29315000000127E+03 0.29315000000129E+03 0.29315987006164E+03 0.29315987006164E+03 0.29318402801106E+03
 0.29317730492441E+03 0.29315971217752E+03 0.29315971217752E+03 0.29315000000127E+03 0.29315000000129E+03
 0.29316170574026E+03 0.29315000000158E+03 -.20510157895307E-01 0.52957745690374E+00 0.13520106390440E+00
 0.15103075911604E+01 0.13744305219365E+01 0.12043323543156E+00 0.84989166259766E-01 0.12436607571019E+01
 0.31793975770679E+14 0.12018564006876E+00 0.84989166259766E-01 0.12434084369275E+01 0.31793873798424E+14
 0.11929788964773E+00 0.84989166259766E-01 0.12422336628503E+01 0.31787678976883E+14 0.12018564006876E+00
 0.84989166259766E-01 0.12434084369275E+01 0.31793873798424E+14 0.17979251154563E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.29894154698491E+03
 0.29894154698491E+03 0.29894154698491E+03 0.29894154698491E+03 0.22916153026282E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.83434058703940E+00 0.36154038667826E-02 0.10000000000000E-02
 0.22127541748522E+04 0.82978281556957E+03 0.80000000000000E+04 0.30000000000000E+04 0.29318572169289E+03
 0.29315052775709E+03 0.29339505279556E+03 0.29339505279556E+03 0.29437318386096E+03 0.29417491735014E+03
 0.29338998806151E+03 0.29338998806151E+03 0.29315000020252E+03 0.29315000020543E+03 0.29338985711149E+03
 0.29338985711149E+03 0.29315000020227E+03 0.29315000020519E+03 0.29338998806151E+03 0.29338998806151E+03
 0.29315000020252E+03 0.29315000020543E+03 0.29344125603198E+03 0.29315000023074E+03 0.92126405906997E+01
 0.16006062818575E+02 0.84407482452152E+01 0.26075149056920E+02 0.17592197070479E+02 0.77022065778047E+01
 0.49036563512990E+01 0.21215950661887E+02 0.49036563512990E+01 0.77430963728499E+01 0.49298646911082E+01
 0.21273177502434E+02 0.49298646911082E+01 0.77332023001905E+01 0.49304404952892E+01 0.21263705893120E+02
 0.49304404952892E+01 0.77430963728492E+01 0.49298646911075E+01 0.21273177502433E+02 0.49298646911075E+01
 0.15872850206291E+01 0.00000000000000E+00 0.67733929760519E+03 0.29716963969556E+01 0.29894154698491E+03
 0.00000000000000E+00 0.00000000000000E+00 0.67733929760519E+03 0.29716963969556E+01 0.29894154698491E+03
 0.00000000000000E+00 0.00000000000000E+00 0.58043089538438E+04 0.46020775835120E+03 0.57628394640779E+04
 0.41469494475331E+02 0.30464999304056E+03 0.58045451428860E+04 0.46044394739344E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.30548105308788E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.60680531827490E+00 0.00000000000000E+00 0.50584377084101E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10331866246368E+01 0.11126490891159E+01 0.10331866246368E+01
 0.21644324120048E+01 0.91443241200478E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.17717155181683E+00 0.31040905410165E+00 0.31040905410165E+00
 0.17717155181683E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.11571835177337E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10425109850404E+01 0.11571835177337E+01 0.10425109850404E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     61.07677633
 0.17959427852960E+01 0.29518489796172E+03 0.33745880728848E+03 0.30709019830382E+03 0.30597963612827E+03
 0.22971035421043E+00 0.00000000000000E+00 0.22329080685996E+00 0.00000000000000E+00 -.99270789243536E+00
 0.21063379424835E-02 0.84905757619686E-01 0.37980610037189E+04 0.14242728763946E+04 0.94222114309774E+02
 0.35333292866165E+02 0.29364857577237E+03 0.29317383309309E+03 0.29533731517437E+03 0.29966892591900E+03
 0.29315002710716E+03 0.29315014972003E+03 0.29743284838691E+03 0.29962313047898E+03 0.29315005846916E+03
 0.29315014940416E+03 0.29538303949028E+03 0.29978390988774E+03 0.29320454076874E+03 0.29320463352592E+03
 0.29361844800599E+03 0.29962313047898E+03 0.29315000477506E+03 0.29315014940416E+03 0.29479384115689E+03
 0.29321556779153E+03 0.15629081697184E+03 0.15755816137219E+03 0.10624313101505E+03 0.56485021846321E+03
 0.45807587179308E+03 0.13695721949469E+03 0.49732777123747E+02 0.13682654181959E+03 0.39868524449431E+03
 0.26397048306617E+03 0.46527460536769E+02 0.25960585129229E+03 0.39597332507419E+03 0.13664057948425E+03
 0.49012421168650E+02 0.13645686580818E+03 0.39672658745805E+03 0.30599777140718E+02 0.46527460536767E+02
 0.33324913427806E+02 0.39597332507418E+03 0.41920174840793E+02 0.00000000000000E+00 0.35270531469705E+03
 0.51519300378320E+00 0.35977460019313E+03 0.46790551102986E-01 0.89080490662045E+00 0.33845227174170E+03
 0.14490248745890E+00 0.33901001487966E+03 0.18024352882458E+00 0.40038678073213E+01 0.33745880728848E+03
 0.84125737311558E-01 0.33745880728848E+03 0.00000000000000E+00 0.64527316188849E+01 0.35275886555776E+03
 0.51797969389216E+00 0.36058370823700E+03 0.45376597275733E-01 0.86859103060513E+00 0.33852861056138E+03
 0.14571429155405E+00 0.33904769238100E+03 0.17943324293106E+00 0.39816268507476E+01 0.33745880728848E+03
 0.84428695137698E-01 0.33745880728848E+03 0.00000000000000E+00 0.64304977694054E+01 0.11927902445386E+04
 0.71819879266821E+03 0.89332972045576E+03 0.29946052879234E+03 0.30464999304056E+03 0.86177106133800E+03
 0.60097525379784E+03 0.67953407612547E+03 0.18223698992197E+03 0.30464999304056E+03 0.82348895000343E+03
 0.57759510926150E+03 0.66463210932725E+03 0.15885684538563E+03 0.30464999304056E+03 0.11996307713118E+04
 0.72423605653068E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.86297118068178E+03 0.60103029084639E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.82501729364942E+03 0.57912345290748E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.17107677633054E-02 0.85538388165268E+05 0.85538388165268E+05 0.50000000000000E+08 0.36538678031542E+03
 0.86891388201036E+00 0.86891388201036E+00 0.60199138180648E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29476189332958E+03 0.29476189332958E+03 0.29476189332958E+03
 0.29476189332958E+03 0.22976108568318E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.57823244852194E-01 0.19132183146459E-02 0.10000000000000E-02 0.41814360330753E+04 0.15680385124032E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29321555958685E+03 0.29479799186818E+03 0.29317554815674E+03
 0.29317554815674E+03 0.29315000000887E+03 0.29315000000908E+03 0.29317554446703E+03 0.29317554446703E+03
 0.29315000000899E+03 0.29315000000920E+03 0.29317603505513E+03 0.29317603505513E+03 0.29320452115574E+03
 0.29319088073258E+03 0.29317554446703E+03 0.29317554446703E+03 0.29315000000899E+03 0.29315000000920E+03
 0.29318111918176E+03 0.29315000001103E+03 -.30097696182585E-01 0.12481761343671E+01 0.23341873302146E+00
 0.35961023462407E+01 0.33615165195542E+01 0.22241052708157E+00 0.16103210449219E+00 0.29325983970467E+01
 0.62379998983235E+14 0.22143292551552E+00 0.16103210449219E+00 0.29316290710106E+01 0.62380143761116E+14
 0.21896670111666E+00 0.15208587646484E+00 0.29280625778050E+01 0.62360893946353E+14 0.22143292551494E+00
 0.16103210449219E+00 0.29316290710097E+01 0.62380143761111E+14 0.42473913647021E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.30135787697050E+03
 0.30135787697050E+03 0.30135787697050E+03 0.30135787697050E+03 0.22876827109410E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.11698259559967E+01 0.10858899179311E-01 0.10000000000000E-02
 0.73672292816217E+03 0.27627109806081E+03 0.80000000000000E+04 0.30000000000000E+04 0.29320559476430E+03
 0.29315127144642E+03 0.29354642520856E+03 0.29354642520856E+03 0.29481538645433E+03 0.29446795455551E+03
 0.29353813811325E+03 0.29353813811325E+03 0.29315000124248E+03 0.29315000127193E+03 0.29353795977432E+03
 0.29353795977432E+03 0.29315000124111E+03 0.29315000127052E+03 0.29353813811325E+03 0.29353813811325E+03
 0.29315000124248E+03 0.29315000127193E+03 0.29362397939590E+03 0.29315000142591E+03 0.12474282338695E+02
 0.22946660242374E+02 0.10951946404409E+02 0.38600274122135E+02 0.27593567985703E+02 0.10150771641283E+02
 0.64539450085600E+01 0.31309904711157E+02 0.64539450085600E+01 0.10213497835054E+02 0.64968622483989E+01
 0.31402392348726E+02 0.64968622483989E+01 0.10201253439638E+02 0.64976518129228E+01 0.31390788536469E+02
 0.64976518129228E+01 0.10213497835054E+02 0.64968622483989E+01 0.31402392348726E+02 0.64968622483989E+01
 0.23382440122202E+01 0.00000000000000E+00 0.69540792384923E+03 0.31161052883603E+01 0.30135787697050E+03
 0.00000000000000E+00 0.00000000000000E+00 0.69540792384923E+03 0.31161052883603E+01 0.30135787697050E+03
 0.00000000000000E+00 0.00000000000000E+00 0.60881678693997E+04 0.47303728024653E+03 0.60338688577385E+04
 0.54299016370667E+02 0.30464999304056E+03 0.60884735526782E+04 0.47334296352498E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.30901973353190E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.66145839209063E+00 0.00000000000000E+00 0.56605657909361E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11465553263485E+01 0.12275149711842E+01 0.11465553263485E+01
 0.21479713926480E+01 0.89797139264802E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.25907604473240E+00 0.40427504430555E+00 0.40427504430555E+00
 0.25907604473240E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13183148836873E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11999636398761E+01 0.13183148836873E+01 0.11999636398761E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     71.12328241
 0.17807906073844E+01 0.29642239296887E+03 0.34340977316245E+03 0.30993989903672E+03 0.30856845365375E+03
 0.22951783338189E+00 0.00000000000000E+00 0.22208568499546E+00 0.00000000000000E+00 -.12788388254091E+01
 0.43722924226742E-02 0.11503850380613E+00 0.18297037861679E+04 0.68613891981295E+03 0.69541933659727E+02
 0.26078225122398E+02 0.29374952643994E+03 0.29318865056468E+03 0.29581695487153E+03 0.30119142934485E+03
 0.29315009005044E+03 0.29315044313692E+03 0.29828568948433E+03 0.30114018574840E+03 0.29315019254806E+03
 0.29315044192453E+03 0.29587021322119E+03 0.30132611379817E+03 0.29322814518781E+03 0.29322843338182E+03
 0.29378086654136E+03 0.30114018574840E+03 0.29315001595548E+03 0.29315044192453E+03 0.29522460238979E+03
 0.29326305601412E+03 0.16896539496242E+03 0.17149460670570E+03 0.12219898333740E+03 0.64954563697954E+03
 0.52673565872544E+03 0.14743629632307E+03 0.58468014239875E+02 0.14822172307513E+03 0.45154662138858E+03
 0.27867097605830E+03 0.55533703404971E+02 0.27526289215595E+03 0.44917087545477E+03 0.14706646419532E+03
 0.57612135761762E+02 0.14776300547339E+03 0.44922332302822E+03 0.36345493694987E+02 0.55533703404968E+02
 0.41739761857834E+02 0.44917087545476E+03 0.47276725064998E+02 0.00000000000000E+00 0.35900958007425E+03
 0.51970697345592E+00 0.36642165568334E+03 0.46434267630394E-01 0.88860831480857E+00 0.34394707813798E+03
 0.14590838570610E+00 0.34457169823329E+03 0.18028298749671E+00 0.39939948674935E+01 0.34340977316245E+03
 0.84768073641029E-01 0.34340977316245E+03 0.00000000000000E+00 0.64368201467624E+01 0.35904955537439E+03
 0.52153036029444E+00 0.36698535691403E+03 0.45488765776033E-01 0.86644921489271E+00 0.34403007397349E+03
 0.14672790220548E+00 0.34461134441916E+03 0.17947049501287E+00 0.39718087537958E+01 0.34340977316245E+03
 0.85074290080562E-01 0.34340977316245E+03 0.00000000000000E+00 0.64146411226339E+01 0.13262662651487E+04
 0.75711083262033E+03 0.98789370111369E+03 0.33837256874446E+03 0.30464999304056E+03 0.98191026987520E+03
 0.63869236639082E+03 0.76195617206969E+03 0.21995410251496E+03 0.30464999304056E+03 0.94516908615769E+03
 0.61001075730625E+03 0.75389659743676E+03 0.19127249343038E+03 0.30464999304056E+03 0.13330890421252E+04
 0.76333398009472E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.98322829011142E+03 0.63876544909436E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.94680874879386E+03 0.61165041994241E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18002706704597E-02 0.90013533522987E+05 0.90013533522987E+05 0.50000000000000E+08 0.37152486060843E+03
 0.89628022299002E+00 0.89628022299002E+00 0.77956793346688E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29573755680210E+03 0.29573755680210E+03 0.29573755680210E+03
 0.29573755680210E+03 0.22960247166811E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.94513006262603E-01 0.37873950054488E-02 0.10000000000000E-02 0.21122697760573E+04 0.79210116602150E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29326303984588E+03 0.29522920929331E+03 0.29320239457455E+03
 0.29320239457455E+03 0.29315000004598E+03 0.29315000004707E+03 0.29320238113355E+03 0.29320238113355E+03
 0.29315000004641E+03 0.29315000004751E+03 0.29320345941051E+03 0.29320345941051E+03 0.29322807438612E+03
 0.29321008035993E+03 0.29320238113355E+03 0.29320238113355E+03 0.29315000004641E+03 0.29315000004751E+03
 0.29321419629580E+03 0.29315000005622E+03 0.18584857884927E-01 0.23170741459340E+01 0.39881685329609E+00
 0.66611308708641E+01 0.62603199333015E+01 0.40117767551438E+00 0.28627929687500E+00 0.54086937394894E+01
 0.99826388446765E+14 0.39940823982270E+00 0.28627929687500E+00 0.54069592098347E+01 0.99826915886714E+14
 0.39365055257015E+00 0.26838684082031E+00 0.53984014435058E+01 0.99784603022242E+14 0.39940823982265E+00
 0.28627929687500E+00 0.54069592098347E+01 0.99826915886714E+14 0.78436381356928E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.30364626485871E+03
 0.30364626485871E+03 0.30364626485871E+03 0.30364626485871E+03 0.22836563271056E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.14832600994561E+01 0.19677047804029E-01 0.10000000000000E-02
 0.40656505384725E+03 0.15246189519272E+03 0.80000000000000E+04 0.30000000000000E+04 0.29322845352786E+03
 0.29315249984139E+03 0.29372271515907E+03 0.29372271515907E+03 0.29523665837973E+03 0.29484247520590E+03
 0.29371088377725E+03 0.29371088377725E+03 0.29315000492820E+03 0.29315000504498E+03 0.29371066221900E+03
 0.29371066221900E+03 0.29315000492319E+03 0.29315000503986E+03 0.29371088377725E+03 0.29371088377725E+03
 0.29315000492820E+03 0.29315000504498E+03 0.29383661753226E+03 0.29315000570438E+03 0.15635585376584E+02
 0.29833324380888E+02 0.13152423865659E+02 0.50950901420076E+02 0.37732715435089E+02 0.12409490554130E+02
 0.78590668624914E+01 0.41330364579029E+02 0.78590668624914E+01 0.12495021981086E+02 0.79204213317824E+01
 0.41461666785178E+02 0.79204213317824E+01 0.12481327195665E+02 0.79214069029752E+01 0.41448829291207E+02
 0.79214069029752E+01 0.12495021981086E+02 0.79204213317827E+01 0.41461666785179E+02 0.79204213317827E+01
 0.30831974858181E+01 0.00000000000000E+00 0.71058267216820E+03 0.31983202577802E+01 0.30364626485871E+03
 0.00000000000000E+00 0.00000000000000E+00 0.71058267216820E+03 0.31983202577802E+01 0.30364626485871E+03
 0.00000000000000E+00 0.00000000000000E+00 0.63250372693134E+04 0.48228545537581E+03 0.62614900825229E+04
 0.63547191499942E+02 0.30464999304056E+03 0.63253931241618E+04 0.48264131022414E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31204015679016E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68404607776179E+00 0.00000000000000E+00 0.60558380838510E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12198559129952E+01 0.12896298861469E+01 0.12198559129952E+01
 0.21403953036922E+01 0.89039530369221E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.34494390675729E+00 0.48251166313843E+00 0.48251166313843E+00
 0.34494390675729E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.14213509904677E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.13164155384374E+01 0.14213509904677E+01 0.13164155384374E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     81.16978849
 0.17750727865248E+01 0.29788308329106E+03 0.34746882469412E+03 0.31226150462843E+03 0.31074171997849E+03
 0.22927802710084E+00 0.00000000000000E+00 0.22105932291378E+00 0.00000000000000E+00 -.15430770375815E+01
 0.79911130276970E-02 0.13882748191997E+00 0.10011121069458E+04 0.37541704010469E+03 0.57625477962728E+02
 0.21609554236023E+02 0.29384207016749E+03 0.29320780938552E+03 0.29623969598868E+03 0.30262823056857E+03
 0.29315023868738E+03 0.29315107070042E+03 0.29899897534864E+03 0.30257612294891E+03 0.29315050604902E+03
 0.29315106712165E+03 0.29629677961674E+03 0.30277745205732E+03 0.29325472562510E+03 0.29325543988106E+03
 0.29396001549917E+03 0.30257612294891E+03 0.29315004260293E+03 0.29315106712165E+03 0.29564077987550E+03
 0.29332874841097E+03 0.17492535285286E+03 0.17915941380240E+03 0.13190771274002E+03 0.70314807054636E+03
 0.57058081924265E+03 0.15025497071563E+03 0.62814100662574E+02 0.15314368051103E+03 0.48098015483379E+03
 0.27886032259439E+03 0.60446437350210E+02 0.27712118319332E+03 0.47918258751097E+03 0.14985757370014E+03
 0.61853842318589E+02 0.15261565053342E+03 0.47838807000617E+03 0.40498201202144E+02 0.60446437350208E+02
 0.49538493085304E+02 0.47918258751096E+03 0.50342562905599E+02 0.00000000000000E+00 0.36275795696700E+03
 0.51449924100982E+00 0.36995300136555E+03 0.46573927079750E-01 0.88594367912675E+00 0.34748006175752E+03
 0.14439730588060E+00 0.34815259901184E+03 0.18041780417843E+00 0.39820182282256E+01 0.34746882469412E+03
 0.83946215475389E-01 0.34746882469412E+03 0.00000000000000E+00 0.64175183009945E+01 0.36280528960332E+03
 0.51637370379380E+00 0.37051293348536E+03 0.45625581457735E-01 0.86385102685416E+00 0.34760959694674E+03
 0.14562012691773E+00 0.34821281842313E+03 0.17919493861458E+00 0.39598986431764E+01 0.34746882469412E+03
 0.84250415580915E-01 0.34746882469412E+03 0.00000000000000E+00 0.63954057842552E+01 0.14059419028104E+04
 0.78056081689069E+03 0.10441193545051E+04 0.36182255301482E+03 0.30464999304056E+03 0.10627208313166E+04
 0.66650817353915E+03 0.81495092636280E+03 0.24776990966329E+03 0.30464999304056E+03 0.10298450216299E+04
 0.63380091980335E+03 0.81478237041187E+03 0.21506265592749E+03 0.30464999304056E+03 0.14127247199178E+04
 0.78663364445324E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.10647560217244E+04 0.66660033610857E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.10314719873732E+04 0.63542788554666E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18026915152977E-02 0.90134575764884E+05 0.90134575764884E+05 0.50000000000000E+08 0.37466233242601E+03
 0.89701032805082E+00 0.89701032805082E+00 0.96057410847328E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29685747496264E+03 0.29685747496264E+03 0.29685747496264E+03
 0.29685747496264E+03 0.22940739608796E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.13778255433475E+00 0.67702894763399E-02 0.10000000000000E-02 0.11816333744602E+04 0.44311251542258E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29332872066706E+03 0.29564549204275E+03 0.29324374432816E+03
 0.29324374432816E+03 0.29315000019992E+03 0.29315000020466E+03 0.29324371645286E+03 0.29324371645286E+03
 0.29315000020109E+03 0.29315000020585E+03 0.29324572323658E+03 0.29324572323658E+03 0.29325452703890E+03
 0.29323256024008E+03 0.29324371645286E+03 0.29324371645286E+03 0.29315000020109E+03 0.29315000020585E+03
 0.29326518766035E+03 0.29315000024209E+03 0.15349657926671E+00 0.37314142308751E+01 0.65648708666385E+00
 0.10665652790891E+02 0.10005883268794E+02 0.68901314137841E+00 0.46520385742188E+00 0.86532628963333E+01
 0.14252130445975E+15 0.68600179015144E+00 0.46520385742188E+00 0.86503324157895E+01 0.14252239844276E+15
 0.67503790394071E+00 0.46520385742188E+00 0.86335470395088E+01 0.14244364077041E+15 0.68600179015144E+00
 0.46520385742188E+00 0.86503324157895E+01 0.14252239844276E+15 0.12554718660301E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.30571547291753E+03
 0.30571547291753E+03 0.30571547291753E+03 0.30571547291753E+03 0.22796125772569E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.17622534131099E+01 0.28886924036494E-01 0.10000000000000E-02
 0.27694191288395E+03 0.10385321733148E+03 0.80000000000000E+04 0.30000000000000E+04 0.29325455626652E+03
 0.29315442508199E+03 0.29392390653458E+03 0.29392390653458E+03 0.29562842728735E+03 0.29520700526479E+03
 0.29390823753709E+03 0.29390823753709E+03 0.29315001537628E+03 0.29315001574064E+03 0.29390797755498E+03
 0.29390797755498E+03 0.29315001536202E+03 0.29315001572605E+03 0.29390823753709E+03 0.29390823753709E+03
 0.29315001537628E+03 0.29315001574064E+03 0.29407851123749E+03 0.29315001795409E+03 0.18653147960350E+02
 0.36378079031933E+02 0.15010226292272E+02 0.62295394967628E+02 0.47210117543894E+02 0.14432537728081E+02
 0.90983360838670E+01 0.50635054735962E+02 0.90983360838670E+01 0.14541796637304E+02 0.91797304480431E+01
 0.50808235764426E+02 0.91797304480431E+01 0.14527442660258E+02 0.91808915050937E+01 0.50794942631522E+02
 0.91808915050937E+01 0.14541796637304E+02 0.91797304480434E+01 0.50808235764426E+02 0.91797304480434E+01
 0.37754279558501E+01 0.00000000000000E+00 0.71998755466321E+03 0.32324795789648E+01 0.30571547291753E+03
 0.00000000000000E+00 0.00000000000000E+00 0.71998755466321E+03 0.32324795789648E+01 0.30571547291753E+03
 0.00000000000000E+00 0.00000000000000E+00 0.64728059165082E+04 0.48898086514537E+03 0.64025633199481E+04
 0.70242601269506E+02 0.30464999304056E+03 0.64731980440907E+04 0.48937299272796E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31444687346395E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68698604132086E+00 0.00000000000000E+00 0.62584842714971E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12560677122634E+01 0.13128344684706E+01 0.12560677122634E+01
 0.21375363932624E+01 0.88753639326241E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.42952280735466E+00 0.55129514107554E+00 0.55129514107554E+00
 0.42952280735466E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.14813329178012E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.13946691628391E+01 0.14813329178012E+01 0.13946691628391E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
     91.21629456
 0.17734275137853E+01 0.29944134112829E+03 0.35008620661969E+03 0.31416020746193E+03 0.31258345887617E+03
 0.22900498602306E+00 0.00000000000000E+00 0.22021714012363E+00 0.00000000000000E+00 -.17735966813237E+01
 0.12685126596285E-01 0.15746616137162E+00 0.63065984712702E+03 0.23649744267263E+03 0.50804566075121E+02
 0.19051712278170E+02 0.29392892950843E+03 0.29323138678862E+03 0.29663083355543E+03 0.30392134535499E+03
 0.29315053993516E+03 0.29315224640716E+03 0.29962444892586E+03 0.30387071590872E+03 0.29315113513728E+03
 0.29315223762900E+03 0.29668831068525E+03 0.30407616851747E+03 0.29328268529913E+03 0.29328420288715E+03
 0.29415295600921E+03 0.30387071590872E+03 0.29315009737394E+03 0.29315223762900E+03 0.29602589279916E+03
 0.29341443164072E+03 0.17956715764467E+03 0.18579968821877E+03 0.13958746746545E+03 0.73804119071540E+03
 0.59775578591262E+03 0.15208267303656E+03 0.65555309052336E+02 0.15790032873982E+03 0.49642633259568E+03
 0.27823027841853E+03 0.63681828516457E+02 0.27806552733804E+03 0.49510769483273E+03 0.15168166532296E+03
 0.64547930479237E+02 0.15734378242145E+03 0.49372305871177E+03 0.43805460998406E+02 0.63681828516455E+02
 0.57149437771721E+02 0.49510769483273E+03 0.52368751804267E+02 0.00000000000000E+00 0.36527829769128E+03
 0.51246430997010E+00 0.37239806836088E+03 0.46634404082179E-01 0.88479475873012E+00 0.35008620661969E+03
 0.14403564232775E+00 0.35051808525012E+03 0.18024403123740E+00 0.39768542182891E+01 0.35008620661969E+03
 0.83647390044549E-01 0.35008620661969E+03 0.00000000000000E+00 0.64091958558488E+01 0.36532968367035E+03
 0.51436111261377E+00 0.37295891925533E+03 0.45684827017937E-01 0.86273075692304E+00 0.35008620661969E+03
 0.14484871143112E+00 0.35055866366025E+03 0.17942762615925E+00 0.39547633186327E+01 0.35008620661969E+03
 0.83950979975953E-01 0.35008620661969E+03 0.00000000000000E+00 0.63871120153359E+01 0.14616347254414E+04
 0.79844852865755E+03 0.10819244653691E+04 0.37971026478168E+03 0.30464999304056E+03 0.11234280654575E+04
 0.68812323474740E+03 0.85404309929540E+03 0.26938497087154E+03 0.30464999304056E+03 0.10873964429087E+04
 0.65209161219867E+03 0.85404309929540E+03 0.23335334832280E+03 0.30464999304056E+03 0.14683495200959E+04
 0.80439253362595E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.11235371595785E+04 0.68823232886844E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.10890004403322E+04 0.65369560962211E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18051123601356E-02 0.90255618006780E+05 0.90255618006780E+05 0.50000000000000E+08 0.37691494806494E+03
 0.89773966713485E+00 0.89773966713485E+00 0.11418234938034E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29801297879517E+03 0.29801297879517E+03 0.29801297879517E+03
 0.29801297879517E+03 0.22918859361208E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.18242167343234E+00 0.10618959414368E-01 0.10000000000000E-02 0.75336948639011E+03 0.28251355739629E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29341439022664E+03 0.29603031055406E+03 0.29329950041119E+03
 0.29329950041119E+03 0.29315000073964E+03 0.29315000075717E+03 0.29329944922351E+03 0.29329944922351E+03
 0.29315000074220E+03 0.29315000075978E+03 0.29330271818798E+03 0.29330271818798E+03 0.29328221752701E+03
 0.29325681788459E+03 0.29329944922351E+03 0.29329944922351E+03 0.29315000074220E+03 0.29315000075978E+03
 0.29333378743750E+03 0.29315000089345E+03 0.37943517736011E+00 0.53555917298098E+01 0.10207969675407E+01
 0.15230513273482E+02 0.14204612321103E+02 0.11015339857768E+01 0.75148315429688E+00 0.12375233690740E+02
 0.18619926637424E+15 0.10965027911870E+01 0.75148315429688E+00 0.12370363823045E+02 0.18620127557582E+15
 0.10788368713552E+01 0.71569824218750E+00 0.12342396323098E+02 0.18607296271060E+15 0.10965027911870E+01
 0.75148315429688E+00 0.12370363823045E+02 0.18620127557582E+15 0.17948755223757E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.30748121553363E+03
 0.30748121553363E+03 0.30748121553363E+03 0.30748121553363E+03 0.22757150050843E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.19956066162836E+01 0.37714716602322E-01 0.10000000000000E-02
 0.21211878865099E+03 0.79544545744122E+02 0.80000000000000E+04 0.30000000000000E+04 0.29328283912580E+03
 0.29315723242336E+03 0.29413850974895E+03 0.29413850974895E+03 0.29599032120423E+03 0.29554912985162E+03
 0.29411912932530E+03 0.29411912932530E+03 0.29315004046916E+03 0.29315004142814E+03 0.29411883531855E+03
 0.29411883531855E+03 0.29315004043503E+03 0.29315004139320E+03 0.29411912932530E+03 0.29411912932530E+03
 0.29315004046916E+03 0.29315004142814E+03 0.29433534432081E+03 0.29315004764526E+03 0.21738166219373E+02
 0.42569770494505E+02 0.16847453633536E+02 0.72300473425348E+02 0.55368782523644E+02 0.16468289842886E+02
 0.10335279259250E+02 0.58971867360950E+02 0.10335279259250E+02 0.16600324688366E+02 0.10436153066782E+02
 0.59186000347349E+02 0.10436153066782E+02 0.16585500698483E+02 0.10437467863998E+02 0.59172422119336E+02
 0.10437467863998E+02 0.16600324688366E+02 0.10436153066782E+02 0.59186000347349E+02 0.10436153066782E+02
 0.43972096182049E+01 0.00000000000000E+00 0.72613153376184E+03 0.32448869963886E+01 0.30748121553363E+03
 0.00000000000000E+00 0.00000000000000E+00 0.72613153376184E+03 0.32448869963886E+01 0.30748121553363E+03
 0.00000000000000E+00 0.00000000000000E+00 0.65707576185853E+04 0.49477288074307E+03 0.64947230064276E+04
 0.76034616867206E+02 0.30464999304056E+03 0.65711818954722E+04 0.49519715762999E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31634405709043E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68627426621361E+00 0.00000000000000E+00 0.62678283973849E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12639811067775E+01 0.13130571059521E+01 0.12639811067775E+01
 0.21367137568926E+01 0.88671375689264E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.50351547062594E+00 0.61240010980450E+00 0.61240010980450E+00
 0.50351547062594E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15135939373844E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14392256694817E+01 0.15135939373844E+01 0.14392256694817E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    101.26280064
 0.17734996548931E+01 0.30100355796886E+03 0.35190559972457E+03 0.31579569832973E+03 0.31421123851357E+03
 0.22871080428202E+00 0.00000000000000E+00 0.21951215020285E+00 0.00000000000000E+00 -.19769551691188E+01
 0.18121384566240E-01 0.17242881894214E+00 0.44146737081579E+03 0.16555026405592E+03 0.46395956598674E+02
 0.17398483724503E+02 0.29401217543774E+03 0.29325919351261E+03 0.29700434305635E+03 0.30508780295357E+03
 0.29315108569069E+03 0.29315424837335E+03 0.30018879124958E+03 0.30503986687156E+03 0.29315226340494E+03
 0.29315422963313E+03 0.29706050898910E+03 0.30524021624774E+03 0.29331135747244E+03 0.29331424312234E+03
 0.29435778079240E+03 0.30503986687156E+03 0.29315019864927E+03 0.29315422963313E+03 0.29638326590836E+03
 0.29352051185343E+03 0.18325408263521E+03 0.19163007035861E+03 0.14574322641208E+03 0.76229507069672E+03
 0.61582312815258E+03 0.15321821696812E+03 0.67173077557194E+02 0.16243208559051E+03 0.50435291234188E+03
 0.27705928073740E+03 0.65729295227307E+02 0.27820678872973E+03 0.50343483115073E+03 0.15282561748450E+03
 0.66171516724403E+02 0.16186991876654E+03 0.50168063397980E+03 0.46511111906704E+02 0.65729295227305E+02
 0.64477216574754E+02 0.50343483115073E+03 0.53803307930209E+02 0.00000000000000E+00 0.36711978247720E+03
 0.51216534735829E+00 0.37424233464670E+03 0.46649900350627E-01 0.88450084562421E+00 0.35190559972457E+03
 0.14390196997746E+00 0.35219237995236E+03 0.18030392499930E+00 0.39755331779421E+01 0.35190559972457E+03
 0.83568457505139E-01 0.35190559972457E+03 0.00000000000000E+00 0.64070668348054E+01 0.36717337748953E+03
 0.51406864769224E+00 0.37480667344084E+03 0.45700007748932E-01 0.86244417308669E+00 0.35190559972457E+03
 0.14471431523439E+00 0.35223335872067E+03 0.17948724863576E+00 0.39534496164903E+01 0.35190559972457E+03
 0.83871767549564E-01 0.35190559972457E+03 0.00000000000000E+00 0.63849903301521E+01 0.15031398878566E+04
 0.81233141928393E+03 0.11095467371580E+04 0.39359315540807E+03 0.30464999304056E+03 0.11678250810811E+04
 0.70522935379790E+03 0.88133399586855E+03 0.28649108992203E+03 0.30464999304056E+03 0.11290942595450E+04
 0.66649853226173E+03 0.88133399586855E+03 0.24776026838586E+03 0.30464999304056E+03 0.15097758700783E+04
 0.81816347632065E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.11679485547663E+04 0.70535282748303E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.11306702584049E+04 0.66807453112171E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18075332049735E-02 0.90376660248677E+05 0.90376660248677E+05 0.50000000000000E+08 0.37866796890047E+03
 0.89846827634788E+00 0.89846827634788E+00 0.13233160894574E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29912594789925E+03 0.29912594789925E+03 0.29912594789925E+03
 0.29912594789925E+03 0.22895698984401E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.22536143959995E+00 0.15037966287561E-01 0.10000000000000E-02 0.53198682900475E+03 0.19949506087678E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29352045696384E+03 0.29638718623594E+03 0.29336791218091E+03
 0.29336791218091E+03 0.29315000235487E+03 0.29315000241068E+03 0.29336782522880E+03 0.29336782522880E+03
 0.29315000235959E+03 0.29315000241551E+03 0.29337258483462E+03 0.29337258483462E+03 0.29331038831603E+03
 0.29328201039578E+03 0.29336782522880E+03 0.29336782522880E+03 0.29315000235959E+03 0.29315000241551E+03
 0.29341760665046E+03 0.29315000284817E+03 0.68169431503807E+00 0.70483096945573E+01 0.14931374199625E+01
 0.19979950325256E+02 0.18479347218194E+02 0.16369912118891E+01 0.10735473632813E+01 0.16280860580694E+02
 0.22782650850706E+15 0.16290539201796E+01 0.10735473632813E+01 0.16273214920306E+02 0.22782992216781E+15
 0.16040752562163E+01 0.10735473632813E+01 0.16232274965263E+02 0.22764306319515E+15 0.16290539201796E+01
 0.10735473632813E+01 0.16273214920306E+02 0.22782992216781E+15 0.23591347631862E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.30900872480878E+03
 0.30900872480878E+03 0.30900872480878E+03 0.30900872480878E+03 0.22719462394383E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.21944196121827E+01 0.46067968148988E-01 0.10000000000000E-02
 0.17365645417934E+03 0.65121170317253E+02 0.80000000000000E+04 0.30000000000000E+04 0.29331283118185E+03
 0.29316108780230E+03 0.29436094822608E+03 0.29436094822608E+03 0.29633021369234E+03 0.29587325139814E+03
 0.29433825491786E+03 0.29433825491786E+03 0.29315009365821E+03 0.29315009587758E+03 0.29433792963650E+03
 0.29433792963650E+03 0.29315009358632E+03 0.29315009580399E+03 0.29433825491786E+03 0.29433825491786E+03
 0.29315009365821E+03 0.29315009587758E+03 0.29460032987401E+03 0.29315011108093E+03 0.24856434140939E+02
 0.48435931635372E+02 0.18671882961091E+02 0.81163956506284E+02 0.62398714130387E+02 0.18506761792105E+02
 0.11567963188257E+02 0.66469874523506E+02 0.11567963188257E+02 0.18659126923781E+02 0.11686335360983E+02
 0.66721118510638E+02 0.11686335360983E+02 0.18643916272783E+02 0.11687790705106E+02 0.66707325545549E+02
 0.11687790705106E+02 0.18659126923781E+02 0.11686335360983E+02 0.66721118510638E+02 0.11686335360983E+02
 0.49577961525137E+01 0.00000000000000E+00 0.73096963409260E+03 0.32494229746831E+01 0.30900872480878E+03
 0.00000000000000E+00 0.00000000000000E+00 0.73096963409260E+03 0.32494229746831E+01 0.30900872480878E+03
 0.00000000000000E+00 0.00000000000000E+00 0.66484244782026E+04 0.49986823539889E+03 0.65672945113890E+04
 0.81129971523025E+02 0.30464999304056E+03 0.66488774517773E+04 0.50032120897354E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31792175142140E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68638200191035E+00 0.00000000000000E+00 0.61489740747059E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12575889428913E+01 0.13012794093809E+01 0.12575889428913E+01
 0.21367498274465E+01 0.88674982744653E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.56654779932473E+00 0.66438212123316E+00 0.66438212123316E+00
 0.56654779932473E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15316568323259E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14660532510668E+01 0.15316568323259E+01 0.14660532510668E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    111.30930672
 0.17744894684845E+01 0.30251620323562E+03 0.35327856884461E+03 0.31724765557720E+03 0.31567981566439E+03
 0.22840371513795E+00 0.00000000000000E+00 0.21890921045526E+00 0.00000000000000E+00 -.21588418046177E+01
 0.24036096868060E-01 0.18472911302411E+00 0.33283274085280E+03 0.12481227781980E+03 0.43306655183019E+02
 0.16239995693632E+02 0.29409270485592E+03 0.29329086373855E+03 0.29736472654777E+03 0.30614920651042E+03
 0.29315199263349E+03 0.29315741946092E+03 0.30071256162736E+03 0.30610470579674E+03 0.29315411942630E+03
 0.29315738357719E+03 0.29741897153842E+03 0.30629416022407E+03 0.29334050082354E+03 0.29334554848940E+03
 0.29457213657357E+03 0.30610470579673E+03 0.29315037164893E+03 0.29315738357719E+03 0.29671808186077E+03
 0.29364603702866E+03 0.18629318572412E+03 0.19685081656513E+03 0.15082615397208E+03 0.78042960139981E+03
 0.62884931665786E+03 0.15391114264409E+03 0.68064183428936E+02 0.16672244202903E+03 0.50832958109716E+03
 0.27554730818804E+03 0.66996641518734E+02 0.27878081658443E+03 0.50775022349247E+03 0.15353133663061E+03
 0.67103052234119E+02 0.16616557692210E+03 0.50577894482285E+03 0.48811366535637E+02 0.66996641518776E+02
 0.71466493305497E+02 0.50775022349259E+03 0.54899356257944E+02 0.00000000000000E+00 0.36857265448781E+03
 0.51271914472555E+00 0.37573576981571E+03 0.46643471585027E-01 0.88462275440194E+00 0.35327856884461E+03
 0.14407817901395E+00 0.35348749154536E+03 0.18027907753635E+00 0.39760811168084E+01 0.35327856884461E+03
 0.83671329033005E-01 0.35327856884461E+03 0.00000000000000E+00 0.64079499065295E+01 0.36862748729189E+03
 0.51462134572187E+00 0.37630472810339E+03 0.45693709886866E-01 0.86256304184247E+00 0.35327856884461E+03
 0.14489150623529E+00 0.35352888376437E+03 0.17946251371798E+00 0.39539945116286E+01 0.35327856884461E+03
 0.83975009710510E-01 0.35327856884461E+03 0.00000000000000E+00 0.63858703591208E+01 0.15361459220939E+04
 0.82354437336209E+03 0.11313398173171E+04 0.40480610948622E+03 0.30464999304056E+03 0.12024668969371E+04
 0.71927663285333E+03 0.90192853266912E+03 0.30053836897747E+03 0.30464999304056E+03 0.11615112137116E+04
 0.67832094962779E+03 0.90192853266912E+03 0.25958268575193E+03 0.30464999304056E+03 0.15427015550276E+04
 0.82927751423458E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.12026026036991E+04 0.71941233961530E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.11630571239786E+04 0.67986685989479E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18099540498115E-02 0.90497702490573E+05 0.90497702490573E+05 0.50000000000000E+08 0.38011953378515E+03
 0.89919618721295E+00 0.89919618721295E+00 0.15050518954351E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30015661440743E+03 0.30015661440743E+03 0.30015661440743E+03
 0.30015661440743E+03 0.22871994800963E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.26506322187854E+00 0.19789498797498E-01 0.10000000000000E-02 0.40425480614050E+03 0.15159555230269E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29364597071926E+03 0.29672145866959E+03 0.29344660936423E+03
 0.29344660936423E+03 0.29315000656072E+03 0.29315000671618E+03 0.29344647193596E+03 0.29344647193596E+03
 0.29315000656818E+03 0.29315000672383E+03 0.29345281213140E+03 0.29345281213140E+03 0.29333868132282E+03
 0.29330768021772E+03 0.29344647193596E+03 0.29344647193596E+03 0.29315000656818E+03 0.29315000672383E+03
 0.29351352580796E+03 0.29315000795591E+03 0.10361438205365E+01 0.87050692724575E+01 0.20626555057724E+01
 0.24651708593668E+02 0.22578739810366E+02 0.22799486284207E+01 0.15029663085937E+01 0.20160537472334E+02
 0.26589040709011E+15 0.22682691571230E+01 0.15029663085937E+01 0.20149341283832E+02 0.26589580357095E+15
 0.22363130050469E+01 0.14671813964844E+01 0.20095092428815E+02 0.26564683709925E+15 0.22682691571211E+01
 0.15029663085937E+01 0.20149341283829E+02 0.26589580357094E+15 0.29173636875343E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31035712522661E+03
 0.31035712522661E+03 0.31035712522661E+03 0.31035712522661E+03 0.22682887677492E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.23680122370834E+01 0.53994430288637E-01 0.10000000000000E-02
 0.14816343013964E+03 0.55561286302364E+02 0.80000000000000E+04 0.30000000000000E+04 0.29334423952066E+03
 0.29316612099837E+03 0.29458839216237E+03 0.29458839216237E+03 0.29665287109621E+03 0.29618258244223E+03
 0.29456276948198E+03 0.29456276948198E+03 0.29315019581685E+03 0.29315020045702E+03 0.29456241498245E+03
 0.29456241498245E+03 0.29315019567974E+03 0.29315020031666E+03 0.29456276948198E+03 0.29456276948198E+03
 0.29315019581685E+03 0.29315020045702E+03 0.29486998716065E+03 0.29315023371068E+03 0.27987363654218E+02
 0.54032533359657E+02 0.20483610485804E+02 0.89123241227398E+02 0.68537212689165E+02 0.20539495633652E+02
 0.12793061987340E+02 0.73295977485914E+02 0.12793061987340E+02 0.20709884002137E+02 0.12927016395037E+02
 0.73580498279372E+02 0.12927016395037E+02 0.20694330803764E+02 0.12928602667242E+02 0.73566524587306E+02
 0.12928602667242E+02 0.20709884002134E+02 0.12927016395035E+02 0.73580498279367E+02 0.12927016395035E+02
 0.54693413972604E+01 0.00000000000000E+00 0.73527664594596E+03 0.32508598625148E+01 0.31035712522661E+03
 0.00000000000000E+00 0.00000000000000E+00 0.73527664594596E+03 0.32508598625148E+01 0.31035712522661E+03
 0.00000000000000E+00 0.00000000000000E+00 0.67176297465804E+04 0.50446832597637E+03 0.66318996891893E+04
 0.85730062100505E+02 0.30464999304056E+03 0.67181087939707E+04 0.50494737336664E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31928714160368E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68750716586310E+00 0.00000000000000E+00 0.59628557676657E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12443124462804E+01 0.12837927426297E+01 0.12443124462804E+01
 0.21372447342422E+01 0.88724473424224E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.61978191037353E+00 0.70783690937430E+00 0.70783690937430E+00
 0.61978191037353E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15434776391682E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14846740075046E+01 0.15434776391682E+01 0.14846740075046E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    121.75729103
 0.17761437803646E+01 0.30400721648448E+03 0.35443451734855E+03 0.31860806263243E+03 0.31706879034229E+03
 0.22807672440715E+00 0.00000000000000E+00 0.21836261247192E+00 0.00000000000000E+00 -.23289567390306E+01
 0.30490337226057E-01 0.19546926100910E+00 0.26237820659994E+03 0.98391827474976E+02 0.40927151198610E+02
 0.15347681699479E+02 0.29417419926541E+03 0.29332744672322E+03 0.29772787124311E+03 0.30716229496635E+03
 0.29315347049777E+03 0.29316239914103E+03 0.30122397256240E+03 0.30712184920589E+03 0.29315711228124E+03
 0.29316233485244E+03 0.29777994082219E+03 0.30729682599385E+03 0.29337117868506E+03 0.29337961306644E+03
 0.29480247698022E+03 0.30712184920588E+03 0.29315066374963E+03 0.29316233485244E+03 0.29704746491873E+03
 0.29379525001080E+03 0.18899746390222E+03 0.20178950362879E+03 0.15532498316158E+03 0.79544880572861E+03
 0.63934719765122E+03 0.15434444556819E+03 0.68513002791279E+02 0.17091583561536E+03 0.51030499672091E+03
 0.27378470966875E+03 0.67790919293015E+02 0.27947622857237E+03 0.51002670323722E+03 0.15397929655642E+03
 0.67613320967580E+02 0.17037010903452E+03 0.50792975262182E+03 0.50918945358365E+02 0.67790919293042E+02
 0.78343779466266E+02 0.51002670323730E+03 0.55823417370963E+02 0.00000000000000E+00 0.36983555086505E+03
 0.51367357395487E+00 0.37705656927413E+03 0.46627684476030E-01 0.88492226822085E+00 0.35443451734855E+03
 0.14439713474148E+00 0.35459834327605E+03 0.18021805966289E+00 0.39774273304726E+01 0.35443451734855E+03
 0.83706462518728E-01 0.35443451734855E+03 0.00000000000000E+00 0.64101194975086E+01 0.36989115968408E+03
 0.51557154343749E+00 0.37763064293289E+03 0.45678244237464E-01 0.86285508672762E+00 0.35443451734855E+03
 0.14521223106394E+00 0.35464015121610E+03 0.17940177222151E+00 0.39553332472537E+01 0.35443451734855E+03
 0.84162242030669E-01 0.35443451734855E+03 0.00000000000000E+00 0.63880324744506E+01 0.15649063891956E+04
 0.83336139480511E+03 0.11502832629758E+04 0.41462313092924E+03 0.30464999304056E+03 0.12322557642844E+04
 0.73172627264141E+03 0.91926776022830E+03 0.31298800876554E+03 0.30464999304056E+03 0.11893407607292E+04
 0.68881126908621E+03 0.91926776022830E+03 0.27007300521035E+03 0.30464999304056E+03 0.15713813797595E+04
 0.83900225308355E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.12324024808229E+04 0.73187298917992E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.11908550250941E+04 0.69032553345113E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18124716363928E-02 0.90623581819642E+05 0.90623581819642E+05 0.50000000000000E+08 0.38142076899904E+03
 0.89995246725281E+00 0.89995246725281E+00 0.16943091136387E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30112567390815E+03 0.30112567390815E+03 0.30112567390815E+03
 0.30112567390815E+03 0.22847275290176E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.30233074233269E+00 0.24900956386883E-01 0.10000000000000E-02 0.32127280075933E+03 0.12047730028475E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29379517513158E+03 0.29705031086066E+03 0.29353659975172E+03
 0.29353659975172E+03 0.29315001688965E+03 0.29315001732859E+03 0.29353639300139E+03 0.29353639300139E+03
 0.29315001689975E+03 0.29315001733895E+03 0.29354434340064E+03 0.29354434340064E+03 0.29336795200425E+03
 0.29333238283255E+03 0.29353639300139E+03 0.29353639300139E+03 0.29315001689975E+03 0.29315001733895E+03
 0.29362257032219E+03 0.29315002054108E+03 0.14288397963952E+01 0.10314920708692E+02 0.27358795102327E+01
 0.29261326716882E+02 0.26511767809098E+02 0.30362519391034E+01 0.19681701660156E+01 0.24029578539517E+02
 0.30115351181288E+15 0.30198432621320E+01 0.19681701660156E+01 0.24013925054702E+02 0.30116163242004E+15
 0.29818519400973E+01 0.19323852539062E+01 0.23946898318567E+02 0.30084935843385E+15 0.30198432621303E+01
 0.19681701660156E+01 0.24013925054699E+02 0.30116163242002E+15 0.34711850672990E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31160792432143E+03
 0.31160792432143E+03 0.31160792432143E+03 0.31160792432143E+03 0.22646017218149E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.25277333162132E+01 0.61836848377170E-01 0.10000000000000E-02
 0.12937269944944E+03 0.48514762293538E+02 0.80000000000000E+04 0.30000000000000E+04 0.29337813180529E+03
 0.29317270552296E+03 0.29482800108538E+03 0.29482800108538E+03 0.29697340138121E+03 0.29645953094195E+03
 0.29479955939516E+03 0.29479955939516E+03 0.29315038671624E+03 0.29315039676646E+03 0.29479917626083E+03
 0.29479917626083E+03 0.29315038646939E+03 0.29315039651320E+03 0.29479955939516E+03 0.29479955939516E+03
 0.29315038671624E+03 0.29315039676646E+03 0.29515252310861E+03 0.29315046405471E+03 0.31230136988161E+02
 0.59591423970881E+02 0.22343225772959E+02 0.96595809309636E+02 0.74140867407812E+02 0.22630536856926E+02
 0.14049706470196E+02 0.79786252984761E+02 0.14049706470196E+02 0.22818301914443E+02 0.14198755867868E+02
 0.80103062371149E+02 0.14198755867868E+02 0.22802432207258E+02 0.14200470375319E+02 0.80088931510958E+02
 0.14200470375319E+02 0.22818301914441E+02 0.14198755867867E+02 0.80103062371145E+02 0.14198755867867E+02
 0.59569348715556E+01 0.00000000000000E+00 0.73960904511198E+03 0.32513057539229E+01 0.31160792432143E+03
 0.00000000000000E+00 0.00000000000000E+00 0.73960904511198E+03 0.32513057539229E+01 0.31160792432143E+03
 0.00000000000000E+00 0.00000000000000E+00 0.67870275124380E+04 0.50888010434365E+03 0.66968856766797E+04
 0.90141840467780E+02 0.30464999304056E+03 0.67875315765944E+04 0.50938416849997E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32054433786062E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.68950249866242E+00 0.00000000000000E+00 0.57356157067151E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12272600391920E+01 0.12630640693339E+01 0.12272600391920E+01
 0.21380718901823E+01 0.88807189018232E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.66637338261267E+00 0.74533483933341E+00 0.74533483933341E+00
 0.66637338261267E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15531570564270E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15002226376074E+01 0.15531570564270E+01 0.15002226376074E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    131.13307502
 0.17780339663229E+01 0.30526236122717E+03 0.35532649546251E+03 0.31972020299652E+03 0.31820997277473E+03
 0.22778037959472E+00 0.00000000000000E+00 0.21792471247578E+00 0.00000000000000E+00 -.24671090695469E+01
 0.36421870407540E-01 0.20379734072085E+00 0.21964824734382E+03 0.82368092753934E+02 0.39254682969381E+02
 0.14720506113518E+02 0.29424587149576E+03 0.29336305817756E+03 0.29804520820210E+03 0.30800717996860E+03
 0.29315540593977E+03 0.29316875260130E+03 0.30165898006143E+03 0.30797065253842E+03 0.29316099230885E+03
 0.29316865111003E+03 0.29809531795312E+03 0.30813157061728E+03 0.29339909648977E+03 0.29341184609434E+03
 0.29501431799993E+03 0.30797065253842E+03 0.29315106198223E+03 0.29316865111003E+03 0.29733058985390E+03
 0.29394335503158E+03 0.19116432011079E+03 0.20589137254771E+03 0.15888261001620E+03 0.80681052577474E+03
 0.64713350270846E+03 0.15458836712769E+03 0.68663076638636E+02 0.17444057732237E+03 0.51109067446703E+03
 0.27214225633245E+03 0.68218526516808E+02 0.28010873740019E+03 0.51104698770144E+03 0.15423649797171E+03
 0.67825271548224E+02 0.17390741446754E+03 0.50888825617491E+03 0.52647203841866E+02 0.68218526516830E+02
 0.84162171135288E+02 0.51104698770151E+03 0.56530763792256E+02 0.00000000000000E+00 0.37082718499760E+03
 0.51462371515471E+00 0.37810276030220E+03 0.46611983605113E-01 0.88522034715315E+00 0.35532649546251E+03
 0.14471486309220E+00 0.35546498230680E+03 0.18015737510341E+00 0.39787670948050E+01 0.35532649546251E+03
 0.83891978985109E-01 0.35532649546251E+03 0.00000000000000E+00 0.64122786945865E+01 0.37088328138081E+03
 0.51651744458461E+00 0.37868128959022E+03 0.45662863070148E-01 0.86314573250773E+00 0.35532649546251E+03
 0.14553172159713E+00 0.35550713790881E+03 0.17934136252929E+00 0.39566655693723E+01 0.35532649546251E+03
 0.84195998785089E-01 0.35532649546251E+03 0.00000000000000E+00 0.63901842316928E+01 0.15875272661682E+04
 0.84110775978951E+03 0.11651577749640E+04 0.42236949591365E+03 0.30464999304056E+03 0.12555319106530E+04
 0.74162274730058E+03 0.93264743193771E+03 0.32288448342472E+03 0.30464999304056E+03 0.12110724813829E+04
 0.69716331803050E+03 0.93264743193771E+03 0.27842505415464E+03 0.30464999304056E+03 0.15939339166997E+04
 0.84667296457286E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.12556874137951E+04 0.74177825044275E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.12125589718357E+04 0.69864980848328E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18147308614497E-02 0.90736543072485E+05 0.90736543072485E+05 0.50000000000000E+08 0.38245795179577E+03
 0.90063052304030E+00 0.90063052304030E+00 0.18643696311113E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30190522940097E+03 0.30190522940097E+03 0.30190522940097E+03
 0.30190522940097E+03 0.22825316137196E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.33227030854258E+00 0.29528122909441E-01 0.10000000000000E-02 0.27092815972539E+03 0.10159805989702E+03
 0.80000000000000E+04 0.30000000000000E+04 0.29394327569677E+03 0.29733301663891E+03 0.29362277667343E+03
 0.29362277667343E+03 0.29315003608472E+03 0.29315003702251E+03 0.29362249273529E+03 0.29362249273529E+03
 0.29315003609539E+03 0.29315003703346E+03 0.29363177644861E+03 0.29363177644861E+03 0.29339400678562E+03
 0.29335637147901E+03 0.29362249273529E+03 0.29362249273529E+03 0.29315003609539E+03 0.29315003703346E+03
 0.29372639380200E+03 0.29315004398682E+03 0.17867227500385E+01 0.11632192764907E+02 0.33970436760003E+01
 0.33114415828948E+02 0.29700386934567E+02 0.37739081659702E+01 0.24333740234375E+01 0.27298552512281E+02
 0.32905125117870E+15 0.37527672881328E+01 0.24333740234375E+01 0.27278476993203E+02 0.32906240616524E+15
 0.37107593481207E+01 0.23618041992187E+01 0.27201641993204E+02 0.32869767543067E+15 0.37527672881325E+01
 0.24333740234375E+01 0.27278476993202E+02 0.32906240616523E+15 0.39365025478428E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31261873748786E+03
 0.31261873748786E+03 0.31261873748786E+03 0.31261873748786E+03 0.22613980835226E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.26560279947657E+01 0.68558096908141E-01 0.10000000000000E-02
 0.11668935342122E+03 0.43758507532956E+02 0.80000000000000E+04 0.30000000000000E+04 0.29340951832864E+03
 0.29317983910190E+03 0.29504491942606E+03 0.29504491942606E+03 0.29725027110875E+03 0.29672607851578E+03
 0.29501396255107E+03 0.29501396255107E+03 0.29315066900218E+03 0.29315068638863E+03 0.29501355494620E+03
 0.29501355494620E+03 0.29315066860799E+03 0.29315068598420E+03 0.29501396255107E+03 0.29501396255107E+03
 0.29315066900218E+03 0.29315068638863E+03 0.29540679986308E+03 0.29315080593976E+03 0.34127900193588E+02
 0.64378098472468E+02 0.23997749485802E+02 0.10269294610105E+03 0.78575207867824E+02 0.24488718677706E+02
 0.15164452500905E+02 0.85144083270956E+02 0.15164452500905E+02 0.24692009678117E+02 0.15327035863475E+02
 0.85489749928566E+02 0.15327035863475E+02 0.24675865705497E+02 0.15328859943074E+02 0.85475481166395E+02
 0.15328859943074E+02 0.24692009678113E+02 0.15327035863472E+02 0.85489749928560E+02 0.15327035863472E+02
 0.63606000957916E+01 0.00000000000000E+00 0.74354311090472E+03 0.32516476483532E+01 0.31261873748786E+03
 0.00000000000000E+00 0.00000000000000E+00 0.74354311090472E+03 0.32516476483532E+01 0.31261873748786E+03
 0.00000000000000E+00 0.00000000000000E+00 0.68497604832157E+04 0.51260208823021E+03 0.67558966635708E+04
 0.93863824354344E+02 0.30464999304056E+03 0.68502855252059E+04 0.51312713022039E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32156059410851E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.69178466287716E+00 0.00000000000000E+00 0.55199799974303E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12109062495938E+01 0.12437826626202E+01 0.12109062495938E+01
 0.21390169831615E+01 0.88901698316147E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.70188495510253E+00 0.77346477217624E+00 0.77346477217624E+00
 0.70188495510253E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15609999978448E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15126861983840E+01 0.15609999978448E+01 0.15126861983840E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    140.50885900
 0.17802318214954E+01 0.30643501516650E+03 0.35612534189000E+03 0.32074122154853E+03 0.31926028477330E+03
 0.22748386005122E+00 0.00000000000000E+00 0.21752503087263E+00 0.00000000000000E+00 -.25928560033277E+01
 0.42403563851637E-01 0.21120068985722E+00 0.18866338753957E+03 0.70748770327337E+02 0.37878664153077E+02
 0.14204499057404E+02 0.29431651880308E+03 0.29340095754847E+03 0.29835538090494E+03 0.30880179381877E+03
 0.29315805316697E+03 0.29317728941290E+03 0.30207481296441E+03 0.30876935484286E+03 0.29316624846737E+03
 0.29317713768493E+03 0.29840359333073E+03 0.30891612066523E+03 0.29342743495236E+03 0.29344598258816E+03
 0.29522978331853E+03 0.30876935484286E+03 0.29315163028339E+03 0.29317713768493E+03 0.29760471599242E+03
 0.29410291951118E+03 0.19316002201540E+03 0.20973255717456E+03 0.16210337676156E+03 0.81677255315267E+03
 0.65385865950730E+03 0.15474964416897E+03 0.68660686075572E+02 0.17772845785372E+03 0.51134672352762E+03
 0.27049502723659E+03 0.68468402722493E+02 0.28070307899648E+03 0.51151050252046E+03 0.15441067079904E+03
 0.67885511283503E+02 0.17720900476384E+03 0.50931706311174E+03 0.54264122568867E+02 0.68468402722507E+02
 0.89634191332953E+02 0.51151050252050E+03 0.57153888624516E+02 0.00000000000000E+00 0.37172124854979E+03
 0.51555405876985E+00 0.37904868416537E+03 0.46597719411738E-01 0.88549132509736E+00 0.35612534189000E+03
 0.14502262610310E+00 0.35624518551651E+03 0.18010224336609E+00 0.39799850493305E+01 0.35612534189000E+03
 0.84071603108331E-01 0.35612534189000E+03 0.00000000000000E+00 0.64142415799901E+01 0.37177774687742E+03
 0.51744415191067E+00 0.37963135613633E+03 0.45648889326520E-01 0.86340995311325E+00 0.35612534189000E+03
 0.14584119325759E+00 0.35628765906723E+03 0.17928648050803E+00 0.39578767583214E+01 0.35612534189000E+03
 0.84376267761589E-01 0.35612534189000E+03 0.00000000000000E+00 0.63921403536819E+01 0.16079809920383E+04
 0.84815053237683E+03 0.11785687282468E+04 0.42941226850097E+03 0.30464999304056E+03 0.12765475023552E+04
 0.75065564259060E+03 0.94463012834993E+03 0.33191737871473E+03 0.30464999304056E+03 0.12306873075538E+04
 0.70479544778919E+03 0.94463012834993E+03 0.28605718391332E+03 0.30464999304056E+03 0.16143234253602E+04
 0.85364549078420E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.12767110226545E+04 0.75081916288992E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.12321469549488E+04 0.70625509518414E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18169900865066E-02 0.90849504325329E+05 0.90849504325329E+05 0.50000000000000E+08 0.38339730850958E+03
 0.90130800720851E+00 0.90130800720851E+00 0.20346419686450E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30260410409180E+03 0.30260410409180E+03 0.30260410409180E+03
 0.30260410409180E+03 0.22803743027485E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.35908054273896E+00 0.34125400358237E-01 0.10000000000000E-02 0.23442948407985E+03 0.87911056529945E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29410283825812E+03 0.29760678475814E+03 0.29371270864247E+03
 0.29371270864247E+03 0.29315007139527E+03 0.29315007325073E+03 0.29371233393952E+03 0.29371233393952E+03
 0.29315007140223E+03 0.29315007325787E+03 0.29372282340400E+03 0.29372282340400E+03 0.29341977843626E+03
 0.29338029866457E+03 0.29371233393952E+03 0.29371233393952E+03 0.29315007140223E+03 0.29315007325787E+03
 0.29383415158108E+03 0.29315008718820E+03 0.21325692335659E+01 0.12811270870094E+02 0.40926073100664E+01
 0.36668209750306E+02 0.32555139403690E+02 0.45464430745648E+01 0.28627929687500E+01 0.30345233576567E+02
 0.35361803012160E+15 0.45201469457968E+01 0.28627929687500E+01 0.30320374209694E+02 0.35363275459645E+15
 0.44755511916208E+01 0.28627929687500E+01 0.30235565115306E+02 0.35322055079677E+15 0.45201469457954E+01
 0.28627929687500E+01 0.30320374209692E+02 0.35363275459644E+15 0.43675410690193E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31353614101604E+03
 0.31353614101604E+03 0.31353614101604E+03 0.31353614101604E+03 0.22582973313518E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.27719193942521E+01 0.75001128569779E-01 0.10000000000000E-02
 0.10666506161380E+03 0.39999398105175E+02 0.80000000000000E+04 0.30000000000000E+04 0.29344172927787E+03
 0.29318814460768E+03 0.29526291066648E+03 0.29526291066648E+03 0.29751841208470E+03 0.29698492234337E+03
 0.29522924858556E+03 0.29522924858556E+03 0.29315109838338E+03 0.29315112692889E+03 0.29522881746769E+03
 0.29522881746769E+03 0.29315109778376E+03 0.29315112631368E+03 0.29522924858556E+03 0.29522924858556E+03
 0.29315109838338E+03 0.29315112692889E+03 0.29566072185181E+03 0.29315132729255E+03 0.36990594436212E+02
 0.68965999698531E+02 0.25620581467791E+02 0.10824923073861E+03 0.82500546363485E+02 0.26309438764812E+02
 0.16255120704854E+02 0.90078394898796E+02 0.16255120704854E+02 0.26529399625862E+02 0.16432295971391E+02
 0.90454941629923E+02 0.16432295971391E+02 0.26513002470369E+02 0.16434225590306E+02 0.90440550543722E+02
 0.16434225590306E+02 0.26529399625861E+02 0.16432295971390E+02 0.90454941629920E+02 0.16432295971390E+02
 0.67335555751852E+01 0.00000000000000E+00 0.74758493627463E+03 0.32522580313539E+01 0.31353614101604E+03
 0.00000000000000E+00 0.00000000000000E+00 0.74758493627463E+03 0.32522580313539E+01 0.31353614101604E+03
 0.00000000000000E+00 0.00000000000000E+00 0.69139300076472E+04 0.51614423211299E+03 0.68165240441195E+04
 0.97405968237126E+02 0.30464999304056E+03 0.69144748363254E+04 0.51668906079122E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32248690377376E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.69436175258892E+00 0.00000000000000E+00 0.53024570064956E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11943963488381E+01 0.12246074532385E+01 0.11943963488381E+01
 0.21401159107477E+01 0.89011591074772E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.73245245549627E+00 0.79732021191646E+00 0.79732021191646E+00
 0.73245245549627E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15685400167016E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15243942469346E+01 0.15685400167016E+01 0.15243942469346E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    152.52996399
 0.17834046024238E+01 0.30781803744525E+03 0.35704941111651E+03 0.32192962776092E+03 0.32048447950188E+03
 0.22710642783172E+00 0.00000000000000E+00 0.21705583929561E+00 0.00000000000000E+00 -.27376056780625E+01
 0.50056486702501E-01 0.21968354230130E+00 0.15981944652940E+03 0.59932292448525E+02 0.36416018770437E+02
 0.13656007038914E+02 0.29440581152121E+03 0.29345248101494E+03 0.29874236809338E+03 0.30975822617543E+03
 0.29316271575995E+03 0.29319211047163E+03 0.30258304710677E+03 0.30973115043101E+03 0.29317540220625E+03
 0.29319187277460E+03 0.29878826793157E+03 0.30986035430120E+03 0.29346430091863E+03 0.29349292806764E+03
 0.29550826620625E+03 0.30973115043101E+03 0.29315268609103E+03 0.29319187277460E+03 0.29794566419123E+03
 0.29432163993765E+03 0.19552777716851E+03 0.21431598929123E+03 0.16584615405906E+03 0.82799772184216E+03
 0.66132233701281E+03 0.15487970751472E+03 0.68509962925115E+02 0.18158609257683E+03 0.51120228027272E+03
 0.26841475971579E+03 0.68610621327520E+02 0.28135838229079E+03 0.51160021585791E+03 0.15455649253204E+03
 0.67811942534961E+02 0.18108496156822E+03 0.50938326854973E+03 0.56208270328484E+02 0.68610621327536E+02
 0.96137819675112E+02 0.51160021585796E+03 0.57854722374351E+02 0.00000000000000E+00 0.37275565178398E+03
 0.51664494875579E+00 0.38014221298552E+03 0.46583338192976E-01 0.88576469418064E+00 0.35704941111651E+03
 0.14496901003277E+00 0.35712820939257E+03 0.18045426089337E+00 0.39812137512201E+01 0.35704941111651E+03
 0.84277781640959E-01 0.35704941111651E+03 0.00000000000000E+00 0.64162217861095E+01 0.37281262245242E+03
 0.51853190309825E+00 0.38072962743004E+03 0.45634800940395E-01 0.86367650523035E+00 0.35704941111651E+03
 0.14619670080212E+00 0.35719232316624E+03 0.17923114822718E+00 0.39590986349344E+01 0.35704941111651E+03
 0.84583187295082E-01 0.35704941111651E+03 0.00000000000000E+00 0.63941137367057E+01 0.16317692241125E+04
 0.85642271593811E+03 0.11940847767597E+04 0.43768445206224E+03 0.30464999304056E+03 0.13010431667778E+04
 0.76129026861545E+03 0.95849116674763E+03 0.34255200473958E+03 0.30464999304056E+03 0.12535383366475E+04
 0.71378543848521E+03 0.95849116674763E+03 0.29504717460935E+03 0.30464999304056E+03 0.16380350828481E+04
 0.86183401464719E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13012161209401E+04 0.76146322277781E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.12549650514749E+04 0.71521215331260E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18198867383097E-02 0.90994336915484E+05 0.90994336915484E+05 0.50000000000000E+08 0.38448204983448E+03
 0.90217581360624E+00 0.90217581360624E+00 0.22532679722472E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30339299050910E+03 0.30339299050910E+03 0.30339299050910E+03
 0.30339299050910E+03 0.22776824415985E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.38929746298203E+00 0.39906556084888E-01 0.10000000000000E-02 0.20046831360197E+03 0.75175617600739E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29432155965556E+03 0.29794735548041E+03 0.29383103119643E+03
 0.29383103119643E+03 0.29315015601385E+03 0.29315016133144E+03 0.29383052085725E+03 0.29383052085725E+03
 0.29315015600288E+03 0.29315016132009E+03 0.29384235806722E+03 0.29384235806722E+03 0.29345209929190E+03
 0.29340202993502E+03 0.29383052085725E+03 0.29383052085725E+03 0.29315015600288E+03 0.29315016132009E+03
 0.29397506927161E+03 0.29315019083368E+03 0.25269791148297E+01 0.14099315845271E+02 0.50027476276019E+01
 0.40777402030013E+02 0.35749640664273E+02 0.55552858272138E+01 0.35069213867187E+01 0.33909823822118E+02
 0.38074967302416E+15 0.55216050317949E+01 0.35069213867187E+01 0.33878143688605E+02 0.38076973402925E+15
 0.54760063565121E+01 0.34353515625000E+01 0.33786149098285E+02 0.38030441607136E+15 0.55216050317814E+01
 0.35069213867187E+01 0.33878143688581E+02 0.38076973402915E+15 0.48675640443817E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31459015508899E+03
 0.31459015508899E+03 0.31459015508899E+03 0.31459015508899E+03 0.22544751432467E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.29044471380729E+01 0.82880124512645E-01 0.10000000000000E-02
 0.96524951513308E+02 0.36196856817491E+02 0.80000000000000E+04 0.30000000000000E+04 0.29348387876026E+03
 0.29320052508783E+03 0.29554167832023E+03 0.29554167832023E+03 0.29785004712718E+03 0.29719310713976E+03
 0.29550398635586E+03 0.29550398635586E+03 0.29315194590609E+03 0.29315201223078E+03 0.29550352651879E+03
 0.29550352651879E+03 0.29315194493812E+03 0.29315201122982E+03 0.29550398635586E+03 0.29550398635586E+03
 0.29315194590609E+03 0.29315201223078E+03 0.29598286397389E+03 0.29315235831092E+03 0.40556715507332E+02
 0.74524194184560E+02 0.27614705253608E+02 0.11461832610329E+03 0.86865547323411E+02 0.28547642838847E+02
 0.17592723748076E+02 0.95797891715953E+02 0.17592723748076E+02 0.28792305385185E+02 0.17791654978474E+02
 0.96219997225989E+02 0.17791654978474E+02 0.28775656405262E+02 0.17793714174297E+02 0.96205513768914E+02
 0.17793714174297E+02 0.28792305385201E+02 0.17791654978487E+02 0.96219997226016E+02 0.17791654978487E+02
 0.71677052375509E+01 0.00000000000000E+00 0.75293602446764E+03 0.32535772258389E+01 0.31459015508899E+03
 0.00000000000000E+00 0.00000000000000E+00 0.75293602446764E+03 0.32535772258389E+01 0.31459015508899E+03
 0.00000000000000E+00 0.00000000000000E+00 0.69985061649726E+04 0.52045406654328E+03 0.68967903670147E+04
 0.10171580266742E+03 0.30464999304056E+03 0.69990748141776E+04 0.52102271574829E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32355916168158E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.69790052468541E+00 0.00000000000000E+00 0.50287716962463E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11736519818054E+01 0.12007776943100E+01 0.11736519818054E+01
 0.21417023012119E+01 0.89170230121191E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.76566581693737E+00 0.82288727083243E+00 0.82288727083243E+00
 0.76566581693737E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15779531627024E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15385887753262E+01 0.15779531627024E+01 0.15385887753262E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    160.46592698
 0.17856649986457E+01 0.30865930276581E+03 0.35760993035890E+03 0.32264616141701E+03 0.32122299360796E+03
 0.22686030765148E+00 0.00000000000000E+00 0.21676733632063E+00 0.00000000000000E+00 -.28239563436412E+01
 0.55060235636922E-01 0.22480774899131E+00 0.14529541887095E+03 0.54485782076608E+02 0.35585961942573E+02
 0.13344735728465E+02 0.29446410805766E+03 0.29348810620950E+03 0.29899125116102E+03 0.31035593576590E+03
 0.29316672266401E+03 0.29320472834117E+03 0.30290439671353E+03 0.31033244921041E+03 0.29318318538844E+03
 0.29320441951295E+03 0.29903568104378E+03 0.31045062492999E+03 0.29348902896578E+03 0.29352625480469E+03
 0.29569243219404E+03 0.31033244921041E+03 0.29315364159482E+03 0.29320441951295E+03 0.29816559399111E+03
 0.29447345894558E+03 0.19701021939475E+03 0.21716328541265E+03 0.16813270521022E+03 0.83463887906370E+03
 0.66566551032742E+03 0.15494970213511E+03 0.68361954883024E+02 0.18392710494750E+03 0.51092253323302E+03
 0.26709145602689E+03 0.68639594711156E+02 0.28172566538514E+03 0.51145809613644E+03 0.15463652777675E+03
 0.67711551100831E+02 0.18343844168460E+03 0.50923255020866E+03 0.57438616778383E+02 0.68639594711168E+02
 0.10013244621879E+03 0.51145809613647E+03 0.58268094921155E+02 0.00000000000000E+00 0.37338078537444E+03
 0.51728701181165E+00 0.38080119975961E+03 0.46576533717737E-01 0.88589409762586E+00 0.35760993035890E+03
 0.14517124940348E+00 0.35767658772244E+03 0.18042790175730E+00 0.39817953760906E+01 0.35760993035890E+03
 0.84395930856617E-01 0.35760993035890E+03 0.00000000000000E+00 0.64171591470247E+01 0.37343806290914E+03
 0.51917292767613E+00 0.38139138283846E+03 0.45628135018949E-01 0.86380268176015E+00 0.35760993035890E+03
 0.14640063238745E+00 0.35774101798938E+03 0.17920496775241E+00 0.39596770289556E+01 0.35760993035890E+03
 0.84701761737816E-01 0.35760993035890E+03 0.00000000000000E+00 0.63950478678040E+01 0.16462472969335E+04
 0.86152378490231E+03 0.12034617806166E+04 0.44278552102644E+03 0.30464999304056E+03 0.13160137114318E+04
 0.76785302463357E+03 0.96689895538355E+03 0.34911476075770E+03 0.30464999304056E+03 0.12674928738987E+04
 0.71933218710043E+03 0.96689895538355E+03 0.30059392322457E+03 0.30464999304056E+03 0.16524658487921E+04
 0.86688317374032E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13161924745055E+04 0.76803178770727E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.12688990038248E+04 0.72073831702652E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18217990185484E-02 0.91089950927418E+05 0.91089950927418E+05 0.50000000000000E+08 0.38513405940271E+03
 0.90274821001908E+00 0.90274821001908E+00 0.23977946821708E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30385492899384E+03 0.30385492899384E+03 0.30385492899384E+03
 0.30385492899384E+03 0.22759577886438E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.40697565915100E+00 0.43626348202014E-01 0.10000000000000E-02 0.18337542172807E+03 0.68765783148027E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29447338142269E+03 0.29816707803683E+03 0.29391010326125E+03
 0.29391010326125E+03 0.29315024960730E+03 0.29315025811494E+03 0.29390949033645E+03 0.29390949033645E+03
 0.29315024956977E+03 0.29315025807613E+03 0.29392208080690E+03 0.29392208080690E+03 0.29347291170157E+03
 0.29342140770219E+03 0.29390949033646E+03 0.29390949033646E+03 0.29315024956977E+03 0.29315025807613E+03
 0.29406871741288E+03 0.29315030551811E+03 0.27595142921140E+01 0.14822944344082E+02 0.56200568142644E+01
 0.43241948559917E+02 0.37593791461581E+02 0.62335681940163E+01 0.39363403320312E+01 0.36070084208082E+02
 0.39628206467673E+15 0.61948710946056E+01 0.39363403320312E+01 0.36033818710344E+02 0.39630616346036E+15
 0.61498045330111E+01 0.38647705078125E+01 0.35938812983672E+02 0.39581112725933E+15 0.61948710945977E+01
 0.39363403320312E+01 0.36033818710330E+02 0.39630616346030E+15 0.51680817256280E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31521813212770E+03
 0.31521813212770E+03 0.31521813212770E+03 0.31521813212770E+03 0.22520464799406E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.29831096603377E+01 0.87859040591136E-01 0.10000000000000E-02
 0.91054943761896E+02 0.34145603910711E+02 0.80000000000000E+04 0.30000000000000E+04 0.29351211463763E+03
 0.29320977745879E+03 0.29572455019044E+03 0.29572455019044E+03 0.29806186398939E+03 0.29739745799666E+03
 0.29568371848637E+03 0.29568371848637E+03 0.29315274850620E+03 0.29315284218705E+03 0.29568324070839E+03
 0.29568324070839E+03 0.29315274721750E+03 0.29315284085442E+03 0.29568371848637E+03 0.29568371848637E+03
 0.29315274850620E+03 0.29315284218705E+03 0.29619249194381E+03 0.29315333563087E+03 0.42867716526423E+02
 0.78025567820618E+02 0.28906307208878E+02 0.11842570506337E+03 0.89374866318444E+02 0.29988689912836E+02
 0.18452810249900E+02 0.99250382313648E+02 0.18452810249900E+02 0.30252553603594E+02 0.18668696579798E+02
 0.99707740265932E+02 0.18668696579798E+02 0.30235742649638E+02 0.18670836658723E+02 0.99693194239542E+02
 0.18670836658723E+02 0.30252553603601E+02 0.18668696579804E+02 0.99707740265945E+02 0.18668696579804E+02
 0.74310302823208E+01 0.00000000000000E+00 0.75655372465993E+03 0.32547710943037E+01 0.31521813212770E+03
 0.00000000000000E+00 0.00000000000000E+00 0.75655372465993E+03 0.32547710943037E+01 0.31521813212770E+03
 0.00000000000000E+00 0.00000000000000E+00 0.70554910752611E+04 0.52317347394741E+03 0.69510558698990E+04
 0.10443521007155E+03 0.30464999304056E+03 0.70560745694179E+04 0.52375696810420E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32420333816120E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.70029450738415E+00 0.00000000000000E+00 0.48538567242162E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11604332760101E+01 0.11856801798058E+01 0.11604332760101E+01
 0.21428324993229E+01 0.89283249932286E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.78456508916234E+00 0.83722091975251E+00 0.83722091975251E+00
 0.78456508916234E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15839753196567E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15474968139377E+01 0.15839753196567E+01 0.15474968139377E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    171.04721096
 0.17888181819713E+01 0.30969588621829E+03 0.35830426597530E+03 0.32352364457310E+03 0.32212750284053E+03
 0.22653723698942E+00 0.00000000000000E+00 0.21640392457342E+00 0.00000000000000E+00 -.29286808041901E+01
 0.61636858825015E-01 0.23118613919680E+00 0.12979246756737E+03 0.48672175337762E+02 0.34604150697762E+02
 0.12976556511661E+02 0.29454144689213E+03 0.29353729582075E+03 0.29931667057584E+03 0.31111767658764E+03
 0.29317333124130E+03 0.29322540998693E+03 0.30331811867198E+03 0.31109901792733E+03 0.29319589426320E+03
 0.29322498893103E+03 0.29935924719216E+03 0.31120329090415E+03 0.29352279617591E+03 0.29357414725393E+03
 0.29593915725509E+03 0.31109901792733E+03 0.29315529686506E+03 0.29322498893103E+03 0.29845443532135E+03
 0.29468336972007E+03 0.19890332431344E+03 0.22075149890857E+03 0.17098367512911E+03 0.84264837680242E+03
 0.67080978329767E+03 0.15502457017789E+03 0.68116804538662E+02 0.18677691452448E+03 0.51033775151124E+03
 0.26538881516124E+03 0.68613141006077E+02 0.28210515742317E+03 0.51103904917719E+03 0.15472406695454E+03
 0.67525008450074E+02 0.18630467956687E+03 0.50880571807587E+03 0.59009957434639E+02 0.68613141006091E+02
 0.10507334206502E+03 0.51103904917723E+03 0.58762006291073E+02 0.00000000000000E+00 0.37415126877929E+03
 0.51804563628774E+00 0.38161053821242E+03 0.46570623078938E-01 0.88600653331341E+00 0.35830426597530E+03
 0.14540336621540E+00 0.35835542661190E+03 0.18040500515956E+00 0.39823007366094E+01 0.35830426597530E+03
 0.84531375378607E-01 0.35830426597530E+03 0.00000000000000E+00 0.64179735984391E+01 0.37420895623194E+03
 0.51993137359433E+00 0.38220398367956E+03 0.45622344733506E-01 0.86391231365486E+00 0.35830426597530E+03
 0.14622216901654E+00 0.35839853537449E+03 0.17958992200050E+00 0.39601795822635E+01 0.35830426597530E+03
 0.84837694535879E-01 0.35830426597530E+03 0.00000000000000E+00 0.63958595129046E+01 0.16642345038561E+04
 0.86795374075202E+03 0.12150190316894E+04 0.44921547687615E+03 0.30464999304056E+03 0.13347041172597E+04
 0.77612839621544E+03 0.97731398962955E+03 0.35739013233957E+03 0.30464999304056E+03 0.12848990253383E+04
 0.72632330429411E+03 0.97731398962955E+03 0.30758504041824E+03 0.30464999304056E+03 0.16703935032041E+04
 0.87324742831029E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13348901952847E+04 0.77631447424051E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.12862789428500E+04 0.72770322180573E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18243487255333E-02 0.91217436276664E+05 0.91217436276664E+05 0.50000000000000E+08 0.38593239274086E+03
 0.90351078981533E+00 0.90351078981533E+00 0.25907330298387E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30440610623263E+03 0.30440610623263E+03 0.30440610623263E+03
 0.30440610623263E+03 0.22737285835032E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.42804415495467E+00 0.48444442155075E-01 0.10000000000000E-02 0.16513762248291E+03 0.61926608431092E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29468329704568E+03 0.29845569003453E+03 0.29401691145261E+03
 0.29401691145261E+03 0.29315044028006E+03 0.29315045528661E+03 0.29401614866221E+03 0.29401614866221E+03
 0.29315044017779E+03 0.29315045518085E+03 0.29402962489166E+03 0.29402962489166E+03 0.29350021918481E+03
 0.29344702062861E+03 0.29401614866221E+03 0.29401614866221E+03 0.29315044017779E+03 0.29315045518085E+03
 0.29419460652278E+03 0.29315053912841E+03 0.30263384006451E+01 0.15633845818981E+02 0.64399773983450E+01
 0.46219400233479E+02 0.39747222948143E+02 0.71324589285676E+01 0.45088989257812E+01 0.38708433018127E+02
 0.41435382927781E+15 0.70868078450486E+01 0.45088989257812E+01 0.38665848293011E+02 0.41438382920385E+15
 0.70436402119769E+01 0.44373291015625E+01 0.38568518165693E+02 0.41385381090813E+15 0.70868078450432E+01
 0.45088989257812E+01 0.38665848293001E+02 0.41438382920381E+15 0.55318874406498E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31597926387733E+03
 0.31597926387733E+03 0.31597926387733E+03 0.31597926387733E+03 0.22489242214999E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.30781454502702E+01 0.94236439003445E-01 0.10000000000000E-02
 0.84892851264335E+02 0.31834819224126E+02 0.80000000000000E+04 0.30000000000000E+04 0.29355044824595E+03
 0.29322338287743E+03 0.29596810422032E+03 0.29596810422031E+03 0.29833738193671E+03 0.29766394869593E+03
 0.29592229864690E+03 0.29592229864690E+03 0.29315418692123E+03 0.29315432962986E+03 0.29592179776311E+03
 0.29592179776311E+03 0.29315418509853E+03 0.29315432774503E+03 0.29592229864689E+03 0.29592229864689E+03
 0.29315418692123E+03 0.29315432962986E+03 0.29646942015249E+03 0.29315508770375E+03 0.45867705057581E+02
 0.82469338512208E+02 0.30562691281385E+02 0.12300426763311E+03 0.92288762895314E+02 0.31832155942862E+02
 0.19551893373937E+02 0.10344213809030E+03 0.19551893373937E+02 0.32126351263736E+02 0.19794646594727E+02
 0.10395500875985E+03 0.19794646594727E+02 0.32109362111049E+02 0.19796891394586E+02 0.10394041177043E+03
 0.19796891394586E+02 0.32126351263740E+02 0.19794646594731E+02 0.10395500875986E+03 0.19794646594731E+02
 0.77525504452909E+01 0.00000000000000E+00 0.76143819617542E+03 0.32566973773453E+01 0.31597926387733E+03
 0.00000000000000E+00 0.00000000000000E+00 0.76143819617542E+03 0.32566973773453E+01 0.31597926387733E+03
 0.00000000000000E+00 0.00000000000000E+00 0.71322398533243E+04 0.52665517927837E+03 0.70243229426313E+04
 0.10791691540251E+03 0.30464999304056E+03 0.71328420664839E+04 0.52725739243792E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32499045766912E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.70347771047887E+00 0.00000000000000E+00 0.46299944884940E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11435245274836E+01 0.11664771593283E+01 0.11435245274836E+01
 0.21444090909856E+01 0.89440909098563E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.80660515827950E+00 0.85377924757835E+00 0.85377924757835E+00
 0.80660515827950E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15917322865301E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15587586794849E+01 0.15917322865301E+01 0.15587586794849E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    181.62849495
 0.17920632625035E+01 0.31064102788404E+03 0.35894496690268E+03 0.32431948108287E+03 0.32294772023010E+03
 0.22622104187414E+00 0.00000000000000E+00 0.21606084536214E+00 0.00000000000000E+00 -.30226180759048E+01
 0.68078041147537E-01 0.23714959995425E+00 0.11751219431626E+03 0.44067072868599E+02 0.33733980582481E+02
 0.12650242718431E+02 0.29461835243837E+03 0.29358819810892E+03 0.29963412736404E+03 0.31184315682273E+03
 0.29318152951808E+03 0.29325092652171E+03 0.30371591615672E+03 0.31182933078233E+03 0.29321148119747E+03
 0.29325037416913E+03 0.29967499200898E+03 0.31192061461280E+03 0.29355742145824E+03 0.29362624298000E+03
 0.29618523375009E+03 0.31182933078233E+03 0.29315746695446E+03 0.29325037416913E+03 0.29873903412969E+03
 0.29490045650615E+03 0.20071178552888E+03 0.22411487636353E+03 0.17363608660123E+03 0.84979850362637E+03
 0.67529423659214E+03 0.15508798006215E+03 0.67834424429458E+02 0.18933010789860E+03 0.50954771550242E+03
 0.26376091582429E+03 0.68532175146716E+02 0.28235355002645E+03 0.51039738230004E+03 0.15479918954676E+03
 0.67295791098660E+02 0.18887363286460E+03 0.50815817248666E+03 0.60510752076881E+02 0.68532175146725E+02
 0.10959576028782E+03 0.51039738230007E+03 0.59196869428670E+02 0.00000000000000E+00 0.37485753885413E+03
 0.51870019870178E+00 0.38234905771995E+03 0.46567928994519E-01 0.88605779125955E+00 0.35894496690268E+03
 0.14559585996263E+00 0.35898116518824E+03 0.18039456883122E+00 0.39825311238008E+01 0.35894496690268E+03
 0.84643512464872E-01 0.35894496690268E+03 0.00000000000000E+00 0.64183448960907E+01 0.37491563848740E+03
 0.52058697464372E+00 0.38294531661319E+03 0.45619705506457E-01 0.86396229338885E+00 0.35894496690268E+03
 0.14641574135014E+00 0.35902448963514E+03 0.17957953282426E+00 0.39604086896844E+01 0.35894496690268E+03
 0.84950236816240E-01 0.35894496690268E+03 0.00000000000000E+00 0.63962295311947E+01 0.16808892035105E+04
 0.87401438928383E+03 0.12256130828120E+04 0.45527612540796E+03 0.30464999304056E+03 0.13521166903023E+04
 0.78393045534742E+03 0.98692450354015E+03 0.36519219147156E+03 0.30464999304056E+03 0.13010947116549E+04
 0.73290847670004E+03 0.98692450354015E+03 0.31417021282417E+03 0.30464999304056E+03 0.16869922173919E+04
 0.87924590866616E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13523096589540E+04 0.78412342399920E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13024497711136E+04 0.73426353615872E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18268984325182E-02 0.91344921625910E+05 0.91344921625910E+05 0.50000000000000E+08 0.38665807424864E+03
 0.90427267345764E+00 0.90427267345764E+00 0.27839411692436E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30489208931166E+03 0.30489208931166E+03 0.30489208931166E+03
 0.30489208931166E+03 0.22715828242631E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.44659412284849E+00 0.53087185429398E-01 0.10000000000000E-02 0.15069550090651E+03 0.56510812839941E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29490038977305E+03 0.29874010220997E+03 0.29412399079120E+03
 0.29412399079120E+03 0.29315073263294E+03 0.29315075760409E+03 0.29412306259931E+03 0.29412306259931E+03
 0.29315073241530E+03 0.29315075737904E+03 0.29413733772108E+03 0.29413733772107E+03 0.29352684305141E+03
 0.29347221605125E+03 0.29412306259931E+03 0.29412306259931E+03 0.29315073241530E+03 0.29315075737904E+03
 0.29432013161798E+03 0.29315089711075E+03 0.32353351175527E+01 0.16273826187564E+02 0.72489791944074E+01
 0.48876392058995E+02 0.41591167968616E+02 0.80164282846547E+01 0.50098876953125E+01 0.41091170373440E+02
 0.42980616687548E+15 0.79635051060792E+01 0.50098876953125E+01 0.41042018824218E+02 0.42984268260109E+15
 0.79232712161509E+01 0.49383178710937E+01 0.40943791026158E+02 0.42928107904155E+15 0.79635051060751E+01
 0.50098876953125E+01 0.41042018824211E+02 0.42984268260106E+15 0.58566335992317E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31666175110972E+03
 0.31666175110972E+03 0.31666175110972E+03 0.31666175110972E+03 0.22459318412314E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.31630718565952E+01 0.10032967011883E+00 0.10000000000000E-02
 0.79737130507102E+02 0.29901423940163E+02 0.80000000000000E+04 0.30000000000000E+04 0.29358932768217E+03
 0.29323839962463E+03 0.29620990379175E+03 0.29620990379174E+03 0.29860492835442E+03 0.29792362926227E+03
 0.29615802268485E+03 0.29615802268485E+03 0.29315613155909E+03 0.29315634055040E+03 0.29615749989428E+03
 0.29615749989428E+03 0.29315612906853E+03 0.29315633797494E+03 0.29615802268485E+03 0.29615802268485E+03
 0.29315613155909E+03 0.29315634055040E+03 0.29674159073839E+03 0.29315745596394E+03 0.48761523779205E+02
 0.86656904601065E+02 0.32139722169943E+02 0.12706379376919E+03 0.94763372988400E+02 0.33580036390874E+02
 0.20591995348257E+02 0.10719546971417E+03 0.20591995348257E+02 0.33911181698177E+02 0.20867580779677E+02
 0.10777580193698E+03 0.20867580779677E+02 0.33894065846719E+02 0.20869925250438E+02 0.10776119817314E+03
 0.20869925250438E+02 0.33911181698179E+02 0.20867580779680E+02 0.10777580193699E+03 0.20867580779680E+02
 0.80426456469303E+01 0.00000000000000E+00 0.76634589518241E+03 0.32589418480859E+01 0.31666175110972E+03
 0.00000000000000E+00 0.00000000000000E+00 0.76634589518241E+03 0.32589418480859E+01 0.31666175110972E+03
 0.00000000000000E+00 0.00000000000000E+00 0.72091770708742E+04 0.52997691172345E+03 0.70979384277361E+04
 0.11123864784759E+03 0.30464999304056E+03 0.72097968471374E+04 0.53059668798665E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32570311488459E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.70660279905073E+00 0.00000000000000E+00 0.44179097452031E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11275082271007E+01 0.11483937735710E+01 0.11275082271007E+01
 0.21460316312517E+01 0.89603163125174E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.82559916991540E+00 0.86792655764633E+00 0.86792655764633E+00
 0.82559916991540E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15991144911152E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15692798577822E+01 0.15991144911152E+01 0.15692798577822E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    192.20977894
 0.17953366599843E+01 0.31150160365150E+03 0.35953777078723E+03 0.32504133404166E+03 0.32369140601195E+03
 0.22591255789857E+00 0.00000000000000E+00 0.21573488040785E+00 0.00000000000000E+00 -.31069095848158E+01
 0.74363514795699E-01 0.24278021603774E+00 0.10757963797137E+03 0.40342364239264E+02 0.32951614141230E+02
 0.12356855302961E+02 0.29469490315644E+03 0.29364061235090E+03 0.29994360655381E+03 0.31253603581615E+03
 0.29319145382754E+03 0.29328167341313E+03 0.30409891949548E+03 0.31252702000427E+03 0.29323013363990E+03
 0.29328097241364E+03 0.29998289061766E+03 0.31260618987291E+03 0.29359297427514E+03 0.29368289258477E+03
 0.29642960946950E+03 0.31252702000426E+03 0.29316024045891E+03 0.29328097241364E+03 0.29902020420799E+03
 0.29512350363986E+03 0.20244409259933E+03 0.22726723387339E+03 0.17611213492820E+03 0.85617670344794E+03
 0.67918400784510E+03 0.15514476651988E+03 0.67527852882870E+02 0.19160149317502E+03 0.50857677666263E+03
 0.26220824536941E+03 0.68411837764194E+02 0.28247034932305E+03 0.50955984780675E+03 0.15486679554655E+03
 0.67037218088662E+02 0.19116002315248E+03 0.50731526416208E+03 0.61944876768171E+02 0.68411837764204E+02
 0.11372131303519E+03 0.50955984780677E+03 0.59578175384857E+02 0.00000000000000E+00 0.37550657388518E+03
 0.51926265415730E+00 0.38302456227587E+03 0.46567936113199E-01 0.88605765581095E+00 0.35953777078723E+03
 0.14575371443621E+00 0.35955957748205E+03 0.18039459640752E+00 0.39825305150049E+01 0.35953777078723E+03
 0.84735282030087E-01 0.35953777078723E+03 0.00000000000000E+00 0.64183439149404E+01 0.37556508493558E+03
 0.52115147959077E+00 0.38362323767760E+03 0.45619712480186E-01 0.86396216131791E+00 0.35953777078723E+03
 0.14657448475025E+00 0.35960308819339E+03 0.17957956027597E+00 0.39604080842704E+01 0.35953777078723E+03
 0.85042338931694E-01 0.35953777078723E+03 0.00000000000000E+00 0.63962285534251E+01 0.16963585796221E+04
 0.87974823992978E+03 0.12353486082777E+04 0.46100997605392E+03 0.30464999304056E+03 0.13683917651589E+04
 0.79131347193567E+03 0.99581656180850E+03 0.37257520805981E+03 0.30464999304056E+03 0.13162109360080E+04
 0.73913264278487E+03 0.99581656180850E+03 0.32039437890900E+03 0.30464999304056E+03 0.17024087356410E+04
 0.88492073019267E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13685912502469E+04 0.79151295702376E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13175424190886E+04 0.74046412586543E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18294481395031E-02 0.91472406975156E+05 0.91472406975156E+05 0.50000000000000E+08 0.38731922876752E+03
 0.90503386842004E+00 0.90503386842004E+00 0.29774191003853E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30532147234544E+03 0.30532147234544E+03 0.30532147234544E+03
 0.30532147234544E+03 0.22695217588414E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.46295804303824E+00 0.57547387458323E-01 0.10000000000000E-02 0.13901586767590E+03 0.52130950378463E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29512344379202E+03 0.29902111835802E+03 0.29423065246664E+03
 0.29423065246664E+03 0.29315115965195E+03 0.29315119917771E+03 0.29422954368059E+03 0.29422954368059E+03
 0.29315115924401E+03 0.29315119875587E+03 0.29424455467445E+03 0.29424455467445E+03 0.29355274055147E+03
 0.29349692611263E+03 0.29422954368059E+03 0.29422954368059E+03 0.29315115924401E+03 0.29315119875587E+03
 0.29444452108993E+03 0.29315141949926E+03 0.33824723838896E+01 0.16752589037140E+02 0.80404516106027E+01
 0.51244417864966E+02 0.43163763996310E+02 0.88784484316407E+01 0.55824462890625E+01 0.43240281682641E+02
 0.44300440813955E+15 0.88179605329764E+01 0.55824462890625E+01 0.43184339168286E+02 0.44304804127963E+15
 0.87814983140286E+01 0.55108764648437E+01 0.43086353238528E+02 0.44245731491720E+15 0.88179605329730E+01
 0.55824462890625E+01 0.43184339168279E+02 0.44304804127961E+15 0.61456425660932E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31727414197789E+03
 0.31727414197789E+03 0.31727414197789E+03 0.31727414197789E+03 0.22430655033191E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.32390362164437E+01 0.10615342240377E+00 0.10000000000000E-02
 0.75362619676744E+02 0.28260982378779E+02 0.80000000000000E+04 0.30000000000000E+04 0.29362863576568E+03
 0.29325477985149E+03 0.29644935163469E+03 0.29644935163468E+03 0.29886489234036E+03 0.29817677879563E+03
 0.29639014505686E+03 0.29639014505686E+03 0.29315867752963E+03 0.29315897330081E+03 0.29638960165519E+03
 0.29638960165519E+03 0.29315867422583E+03 0.29315896988440E+03 0.29639014505686E+03 0.29639014505686E+03
 0.29315867752963E+03 0.29315897330081E+03 0.29700825065524E+03 0.29316055462766E+03 0.51542156503686E+02
 0.90595749511299E+02 0.33634161112975E+02 0.13065166145483E+03 0.96849329536286E+02 0.35227942025588E+02
 0.21570336238943E+02 0.11054278251558E+03 0.21570336238943E+02 0.35603562304324E+02 0.21885545780104E+02
 0.11120418808365E+03 0.21885545780104E+02 0.35586375670136E+02 0.21887984283359E+02 0.11118962569808E+03
 0.21887984283359E+02 0.35603562304326E+02 0.21885545780106E+02 0.11120418808366E+03 0.21885545780106E+02
 0.83037333727991E+01 0.00000000000000E+00 0.77122809174779E+03 0.32614299229818E+01 0.31727414197789E+03
 0.00000000000000E+00 0.00000000000000E+00 0.77122809174779E+03 0.32614299229818E+01 0.31727414197789E+03
 0.00000000000000E+00 0.00000000000000E+00 0.72855782248388E+04 0.53314511720723E+03 0.71711713762169E+04
 0.11440685333136E+03 0.30464999304056E+03 0.72862144712653E+04 0.53378136363370E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32634888882080E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.70963296452587E+00 0.00000000000000E+00 0.42181746591040E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11124170624329E+01 0.11314504304363E+01 0.11124170624329E+01
 0.21476683299921E+01 0.89766832999215E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.84204792150224E+00 0.88010542761141E+00 0.88010542761141E+00
 0.84204792150224E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16060820041326E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15790451348676E+01 0.16060820041326E+01 0.15790451348676E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    201.51762300
 0.17981986881695E+01 0.31219492019554E+03 0.36002403741103E+03 0.32562153507775E+03 0.32428883864151E+03
 0.22564802969320E+00 0.00000000000000E+00 0.21546039053018E+00 0.00000000000000E+00 -.31739642228997E+01
 0.79753067457216E-01 0.24750437439396E+00 0.10030962137339E+03 0.37616108015022E+02 0.32322661042210E+02
 0.12120997890829E+02 0.29476145470401E+03 0.29368793293859E+03 0.30020650732282E+03 0.31311939336419E+03
 0.29320186416167E+03 0.29331378022458E+03 0.30442281390425E+03 0.31311449708815E+03 0.29324946293904E+03
 0.29331293509641E+03 0.30024447494220E+03 0.31318374398974E+03 0.29362475095961E+03 0.29373673699389E+03
 0.29663871011635E+03 0.31311449708815E+03 0.29316331556317E+03 0.29331293509641E+03 0.29926412482608E+03
 0.29532410387450E+03 0.20388309375788E+03 0.22985184887764E+03 0.17813956164405E+03 0.86121122260296E+03
 0.68218096315069E+03 0.15517792725438E+03 0.67233047880814E+02 0.19338159670725E+03 0.50760003815790E+03
 0.26088270154851E+03 0.68270235296915E+02 0.28245521854048E+03 0.50869104697330E+03 0.15490898525943E+03
 0.66780840107556E+02 0.19295296454407E+03 0.50644057628094E+03 0.63144097086902E+02 0.68270235296921E+02
 0.11705282816084E+03 0.50869104697331E+03 0.59872983557775E+02 0.00000000000000E+00 0.37603561186639E+03
 0.51969135856315E+00 0.38357278349075E+03 0.46569777804330E-01 0.88602261496250E+00 0.36002403741103E+03
 0.14586803988015E+00 0.36003361427515E+03 0.18040173073977E+00 0.39823730182017E+01 0.36002403741103E+03
 0.84801589110900E-01 0.36002403741103E+03 0.00000000000000E+00 0.64180900892272E+01 0.37609448271567E+03
 0.52158266398193E+00 0.38417330065798E+03 0.45621516670513E-01 0.86392799427814E+00 0.36002403741103E+03
 0.14668945767964E+00 0.36007726778567E+03 0.17958666237478E+00 0.39602514623411E+01 0.36002403741103E+03
 0.85108887085798E-01 0.36002403741103E+03 0.00000000000000E+00 0.63959756023062E+01 0.17090825554026E+04
 0.88453664599207E+03 0.12432841779959E+04 0.46579838211621E+03 0.30464999304056E+03 0.13818569259882E+04
 0.79748463340807E+03 0.10031105611654E+04 0.37874636953221E+03 0.30464999304056E+03 0.13286976619393E+04
 0.74432536935915E+03 0.10031105611654E+04 0.32558710548328E+03 0.30464999304056E+03 0.17150885039730E+04
 0.88965953182322E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13820618848893E+04 0.79768959230915E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13300091748733E+04 0.74563688229318E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18316909934933E-02 0.91584549674667E+05 0.91584549674667E+05 0.50000000000000E+08 0.38785385310830E+03
 0.90570289129844E+00 0.90570289129844E+00 0.31478578513730E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30565879881620E+03 0.30565879881620E+03 0.30565879881620E+03
 0.30565879881620E+03 0.22677784134628E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.47578569620528E+00 0.61318605801010E-01 0.10000000000000E-02 0.13046611049771E+03 0.48924791436641E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29532405096977E+03 0.29926492309638E+03 0.29432174280988E+03
 0.29432174280988E+03 0.29315169347844E+03 0.29315178074988E+03 0.29432046330793E+03 0.29432046330794E+03
 0.29315169280450E+03 0.29315178004122E+03 0.29433606665062E+03 0.29433606665041E+03 0.29357443764503E+03
 0.29349815210757E+03 0.29432046330794E+03 0.29432046330794E+03 0.29315169280450E+03 0.29315178004122E+03
 0.29455030596716E+03 0.29315207168391E+03 0.34295338974249E+01 0.17019416860230E+02 0.86980351942837E+01
 0.53106386819323E+02 0.44364861449068E+02 0.95975069120011E+01 0.60118652343750E+01 0.44947658114376E+02
 0.45309874970385E+15 0.95295357258121E+01 0.60118652343750E+01 0.44884966513334E+02 0.45314911340537E+15
 0.94976866722705E+01 0.59402954101562E+01 0.44788747100953E+02 0.45253492336286E+15 0.95295357258094E+01
 0.60118652343750E+01 0.44884966513329E+02 0.45314911340534E+15 0.63711730683588E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31776147057607E+03
 0.31776147057607E+03 0.31776147057607E+03 0.31776147057607E+03 0.22406445992342E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.32993091902125E+01 0.11106478046351E+00 0.10000000000000E-02
 0.72030034783422E+02 0.27011263043783E+02 0.80000000000000E+04 0.30000000000000E+04 0.29366285427196E+03
 0.29327037790780E+03 0.29665455420526E+03 0.29665455420269E+03 0.29908541530225E+03 0.29814980693685E+03
 0.29658759126021E+03 0.29658759126021E+03 0.29316155590684E+03 0.29316215143924E+03 0.29658703140282E+03
 0.29658703140282E+03 0.29316155174654E+03 0.29316214706453E+03 0.29658759126021E+03 0.29658759126021E+03
 0.29316155590684E+03 0.29316215143924E+03 0.29723424373988E+03 0.29316405419730E+03 0.53828142238766E+02
 0.93801527024038E+02 0.34828543731532E+02 0.13342850921193E+03 0.98425822761743E+02 0.36546657825830E+02
 0.22348085724479E+02 0.11314768215682E+03 0.22348085724479E+02 0.36969214302011E+02 0.22705274490105E+02
 0.11389464489485E+03 0.22705274490105E+02 0.36952098813676E+02 0.22707788206376E+02 0.11388024316345E+03
 0.22707788206376E+02 0.36969214302012E+02 0.22705274490106E+02 0.11389464489485E+03 0.22705274490106E+02
 0.85089719389457E+01 0.00000000000000E+00 0.77547499957446E+03 0.32637861940874E+01 0.31776147057607E+03
 0.00000000000000E+00 0.00000000000000E+00 0.77547499957446E+03 0.32637861940874E+01 0.31776147057607E+03
 0.00000000000000E+00 0.00000000000000E+00 0.73519313795123E+04 0.53579465448074E+03 0.72348749936169E+04
 0.11705639060488E+03 0.30464999304056E+03 0.73525813390919E+04 0.53644461406033E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32686771555878E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.71221799457607E+00 0.00000000000000E+00 0.40525646332865E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10998871454388E+01 0.11174744579047E+01 0.10998871454388E+01
 0.21490993440847E+01 0.89909934408474E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.85473303686227E+00 0.88950116767204E+00 0.88950116767204E+00
 0.85473303686227E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16118597478456E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15869998991455E+01 0.16118597478456E+01 0.15869998991455E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    210.82546706
 0.18010162031864E+01 0.31283320016572E+03 0.36047969405288E+03 0.32615485104656E+03 0.32483768849174E+03
 0.22539015411359E+00 0.00000000000000E+00 0.21519606693521E+00 0.00000000000000E+00 -.32350756861161E+01
 0.85006339639790E-01 0.25204529525379E+00 0.94110627911984E+02 0.35291485466994E+02 0.31740326642260E+02
 0.11902622490847E+02 0.29482811846125E+03 0.29373617279077E+03 0.30046482507824E+03 0.31368180753822E+03
 0.29321377324331E+03 0.29335036290650E+03 0.30473707760631E+03 0.31368105022972E+03 0.29327134451959E+03
 0.29334936467149E+03 0.30050158699476E+03 0.31374097658231E+03 0.29365767178034E+03 0.29379482486049E+03
 0.29684780656865E+03 0.31368105022972E+03 0.29316699876629E+03 0.29334936467149E+03 0.29950691082886E+03
 0.29552794752152E+03 0.20529447986483E+03 0.23231575153125E+03 0.18007038952895E+03 0.86575574957419E+03
 0.68478500809759E+03 0.15522640488217E+03 0.66949843708994E+02 0.19498321086427E+03 0.50652612515967E+03
 0.25963591802941E+03 0.68129963350101E+02 0.28237008125190E+03 0.50771457061387E+03 0.15496572646839E+03
 0.66532127371681E+02 0.19456653400928E+03 0.50545799325707E+03 0.64309164596818E+02 0.68129963350107E+02
 0.12011857403753E+03 0.50771457061388E+03 0.60135451090973E+02 0.00000000000000E+00 0.37652868878397E+03
 0.52006746875173E+00 0.38408180483414E+03 0.46572998673484E-01 0.88596133991061E+00 0.36047969405288E+03
 0.14596309272971E+00 0.36047969405288E+03 0.00000000000000E+00 0.39820976074964E+01 0.36047969405288E+03
 0.84856574007481E-01 0.36047969405288E+03 0.00000000000000E+00 0.64176462305755E+01 0.37658791159825E+03
 0.52196175384074E+00 0.38468393028346E+03 0.45624671955823E-01 0.86386824723355E+00 0.36047969405288E+03
 0.14719917074072E+00 0.36054359025931E+03 0.17919136653651E+00 0.39599775815058E+01 0.36047969405288E+03
 0.85164072623867E-01 0.36047969405288E+03 0.00000000000000E+00 0.63955332730357E+01 0.17210672973491E+04
 0.88912523417478E+03 0.12506803317596E+04 0.47038697029892E+03 0.30464999304056E+03 0.13946025017398E+04
 0.80339535953189E+03 0.10099454107932E+04 0.38465709565602E+03 0.30464999304056E+03 0.13405019742810E+04
 0.74929483207303E+03 0.10099454107932E+04 0.33055656819717E+03 0.30464999304056E+03 0.17270309929857E+04
 0.89420058759719E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.13948126737757E+04 0.80360553156781E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13417947040160E+04 0.75058756180808E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18339338474836E-02 0.91696692374179E+05 0.91696692374179E+05 0.50000000000000E+08 0.38834864101684E+03
 0.90637139142168E+00 0.90637139142168E+00 0.33185053637126E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30596272956328E+03 0.30596272956328E+03 0.30596272956328E+03
 0.30596272956328E+03 0.22660993159609E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.48733356655026E+00 0.64948691634210E-01 0.10000000000000E-02 0.12317415176053E+03 0.46190306910198E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29552790161482E+03 0.29950761151432E+03 0.29441311878169E+03
 0.29441311878169E+03 0.29315239240949E+03 0.29315251569971E+03 0.29441165491102E+03 0.29441165491102E+03
 0.29315239135572E+03 0.29315251459164E+03 0.29442784588890E+03 0.29442784588869E+03 0.29359588669657E+03
 0.29351860023768E+03 0.29441165491102E+03 0.29441165491102E+03 0.29315239135572E+03 0.29315251459164E+03
 0.29465596318055E+03 0.29315292442654E+03 0.34533855075809E+01 0.17203681945056E+02 0.93607375881935E+01
 0.54804942922851E+02 0.45397401646716E+02 0.10313884396952E+02 0.64412841796875E+01 0.46520939227876E+02
 0.46183703808387E+15 0.10238810235082E+02 0.64412841796875E+01 0.46451938660389E+02 0.46189467325861E+15
 0.10211318770384E+02 0.63697143554687E+01 0.46357285030841E+02 0.46125719313426E+15 0.10238810235080E+02
 0.64412841796875E+01 0.46451938660385E+02 0.46189467325859E+15 0.65765632558745E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31820571147636E+03
 0.31820571147636E+03 0.31820571147636E+03 0.31820571147636E+03 0.22383143225862E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.33541330007867E+01 0.11578757256120E+00 0.10000000000000E-02
 0.69092043498636E+02 0.25909516311989E+02 0.80000000000000E+04 0.30000000000000E+04 0.29369765824568E+03
 0.29328693883611E+03 0.29685945378191E+03 0.29685945377932E+03 0.29930190128581E+03 0.29835999783293E+03
 0.29678361082598E+03 0.29678361082599E+03 0.29316504841505E+03 0.29316582393930E+03 0.29678303556903E+03
 0.29678303556904E+03 0.29316504327295E+03 0.29316581853218E+03 0.29678361082598E+03 0.29678361082599E+03
 0.29316504841505E+03 0.29316582393930E+03 0.29745758143261E+03 0.29316829556487E+03 0.56086203264868E+02
 0.96893668165078E+02 0.36015868944239E+02 0.13595040006012E+03 0.99754451771156E+02 0.37837903301718E+02
 0.23109781141404E+02 0.11552792824046E+03 0.23109781141404E+02 0.38314281970159E+02 0.23515112945357E+02
 0.11637283785112E+03 0.23515112945357E+02 0.38297204989136E+02 0.23517696472053E+02 0.11635855784115E+03
 0.23517696472053E+02 0.38314281970160E+02 0.23515112945357E+02 0.11637283785112E+03 0.23515112945357E+02
 0.86986285253734E+01 0.00000000000000E+00 0.77964500283357E+03 0.32662285742050E+01 0.31820571147636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.77964500283357E+03 0.32662285742050E+01 0.31820571147636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.74170342973954E+04 0.53834752347705E+03 0.72974250425036E+04
 0.11960925960119E+03 0.30464999304056E+03 0.74176971625993E+04 0.53901038868098E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32734464102978E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.71469379725396E+00 0.00000000000000E+00 0.38963980038705E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10880725809900E+01 0.11043335976410E+01 0.10880725809900E+01
 0.21505081015932E+01 0.90050810159321E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.86602150136315E+00 0.89781166088220E+00 0.89781166088220E+00
 0.86602150136315E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16172969179959E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15944223617485E+01 0.16172969179959E+01 0.15944223617485E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    220.13331112
 0.18037637194558E+01 0.31342079041666E+03 0.36090731972718E+03 0.32664552823391E+03 0.32534231883193E+03
 0.22513907770506E+00 0.00000000000000E+00 0.21494093989495E+00 0.00000000000000E+00 -.32908135837445E+01
 0.90120256845446E-01 0.25642470918288E+00 0.88770275185964E+02 0.33288853194736E+02 0.31198241485747E+02
 0.11699340557155E+02 0.29489471896855E+03 0.29378524605326E+03 0.30071775633733E+03 0.31422495212174E+03
 0.29322722856064E+03 0.29339152139549E+03 0.30504218180795E+03 0.31422829078487E+03 0.29329581083352E+03
 0.29339036339977E+03 0.30075346067942E+03 0.31427946897082E+03 0.29369158933360E+03 0.29385706992043E+03
 0.29705497367877E+03 0.31422829078487E+03 0.29317134724228E+03 0.29339036339977E+03 0.29974835564234E+03
 0.29573455835485E+03 0.20665757480221E+03 0.23464431351139E+03 0.18189498159908E+03 0.86983880697495E+03
 0.68703435046787E+03 0.15527084972182E+03 0.66661426426340E+02 0.19640314532185E+03 0.50534374109029E+03
 0.25844063153384E+03 0.67975912867388E+02 0.28219594069503E+03 0.50662151849146E+03 0.15501740387024E+03
 0.66274838185964E+02 0.19599704562339E+03 0.50435780067141E+03 0.65424529678768E+02 0.67975912867394E+02
 0.12292615348823E+03 0.50662151849148E+03 0.60365742719829E+02 0.00000000000000E+00 0.37698915184695E+03
 0.52039860336468E+00 0.38455548052673E+03 0.46577313197904E-01 0.88587927202017E+00 0.36090731972718E+03
 0.14604194035166E+00 0.36090731972718E+03 0.00000000000000E+00 0.39817287399901E+01 0.36090731972718E+03
 0.84902044250654E-01 0.36090731972718E+03 0.00000000000000E+00 0.64170517546497E+01 0.37704871653563E+03
 0.52229625340160E+00 0.38515901213376E+03 0.45628898627219E-01 0.86378822585850E+00 0.36090731972718E+03
 0.14727869914377E+00 0.36096068513964E+03 0.17920796683171E+00 0.39596107630098E+01 0.36090731972718E+03
 0.85209709503782E-01 0.36090731972718E+03 0.00000000000000E+00 0.63949408454655E+01 0.17323662873907E+04
 0.89351727827179E+03 0.12575872777043E+04 0.47477901439593E+03 0.30464999304056E+03 0.14066753748795E+04
 0.80905384755713E+03 0.10163597959077E+04 0.39031558368126E+03 0.30464999304056E+03 0.13516690525799E+04
 0.75404752525747E+03 0.10163597959077E+04 0.33530926138161E+03 0.30464999304056E+03 0.17382894902700E+04
 0.89854701082098E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14068905295440E+04 0.80926900222154E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13529438019033E+04 0.75532227458085E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18361767014738E-02 0.91808835073690E+05 0.91808835073690E+05 0.50000000000000E+08 0.38880766883803E+03
 0.90703937277467E+00 0.90703937277467E+00 0.34893616374041E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30623710655312E+03 0.30623710655312E+03 0.30623710655312E+03
 0.30623710655312E+03 0.22644829512152E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.49773894000354E+00 0.68440660722225E-01 0.10000000000000E-02 0.11688957873257E+03 0.43833592024715E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29573451918703E+03 0.29974897462471E+03 0.29450378708467E+03
 0.29450378708467E+03 0.29315328405634E+03 0.29315345329694E+03 0.29450212869603E+03 0.29450212869603E+03
 0.29315328247805E+03 0.29315345163731E+03 0.29451895191186E+03 0.29451895191165E+03 0.29361691747517E+03
 0.29353881958309E+03 0.29450212869603E+03 0.29450212869603E+03 0.29315328247805E+03 0.29315345163731E+03
 0.29476039393769E+03 0.29315401069466E+03 0.34306406014677E+01 0.17288701508559E+02 0.10002206942870E+02
 0.56337148585373E+02 0.46284930607789E+02 0.11006069345339E+02 0.68707031250000E+01 0.47956277356666E+02
 0.46941124889836E+15 0.10923697640915E+02 0.68707031250000E+01 0.47880818259165E+02 0.46947655871959E+15
 0.10900515847672E+02 0.67991333007812E+01 0.47787527784697E+02 0.46881402174790E+15 0.10923697640912E+02
 0.68707031250000E+01 0.47880818259160E+02 0.46947655871958E+15 0.67608238050459E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31861117425196E+03
 0.31861117425196E+03 0.31861117425196E+03 0.31861117425196E+03 0.22360715465784E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.34040554872454E+01 0.12032987071255E+00 0.10000000000000E-02
 0.66483907550361E+02 0.24931465331385E+02 0.80000000000000E+04 0.30000000000000E+04 0.29373276822300E+03
 0.29330441840580E+03 0.29706271154431E+03 0.29706271154171E+03 0.29951396438089E+03 0.29856654276750E+03
 0.29697672193164E+03 0.29697672193165E+03 0.29316921382932E+03 0.29317020402828E+03 0.29697613229866E+03
 0.29697613229866E+03 0.29316920757688E+03 0.29317019745361E+03 0.29697672193164E+03 0.29697672193165E+03
 0.29316921382932E+03 0.29317020402828E+03 0.29767672204994E+03 0.29317334751274E+03 0.58256192818757E+02
 0.99820455864368E+02 0.37138650373554E+02 0.13819464610080E+03 0.10087030247538E+03 0.39050055395363E+02
 0.23822427092516E+02 0.11765708882093E+03 0.23822427092516E+02 0.39587937122119E+02 0.24282859024216E+02
 0.11861383512516E+03 0.24282859024216E+02 0.39570949328587E+02 0.24285507654567E+02 0.11859972148268E+03
 0.24285507654567E+02 0.39587937122120E+02 0.24282859024217E+02 0.11861383512516E+03 0.24282859024217E+02
 0.88706146425792E+01 0.00000000000000E+00 0.78371887184919E+03 0.32687354839292E+01 0.31861117425196E+03
 0.00000000000000E+00 0.00000000000000E+00 0.78371887184919E+03 0.32687354839292E+01 0.31861117425196E+03
 0.00000000000000E+00 0.00000000000000E+00 0.74805926465835E+04 0.54079783743096E+03 0.73585330777378E+04
 0.12205957355510E+03 0.30464999304056E+03 0.74812676805106E+04 0.54147287135810E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32778361945626E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.71706480073117E+00 0.00000000000000E+00 0.37495472515650E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10769452935107E+01 0.10920195258877E+01 0.10769452935107E+01
 0.21518818597279E+01 0.90188185972792E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.87607139761313E+00 0.90522217887631E+00 0.90522217887631E+00
 0.87607139761313E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16224217070093E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16013192577428E+01 0.16224217070093E+01 0.16013192577428E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    234.09507721
 0.18077185633712E+01 0.31421724617136E+03 0.36150253286925E+03 0.32731113665401E+03 0.32602615590719E+03
 0.22477534537554E+00 0.00000000000000E+00 0.21457382245574E+00 0.00000000000000E+00 -.33656110893943E+01
 0.97527017248077E-01 0.26272541284739E+00 0.82028551941157E+02 0.30760706977934E+02 0.30450042549356E+02
 0.11418765956008E+02 0.29499441723888E+03 0.29386025830170E+03 0.30108653119273E+03 0.31500602501629E+03
 0.29325038619809E+03 0.29346195154559E+03 0.30548321942839E+03 0.31501541387493E+03 0.29333737472694E+03
 0.29346054679084E+03 0.30112091861435E+03 0.31505445329064E+03 0.29374428485435E+03 0.29395823404224E+03
 0.29736058924298E+03 0.31501541387493E+03 0.29317923531657E+03 0.29346054679084E+03 0.30010821944171E+03
 0.29604879836630E+03 0.20861485293454E+03 0.23790160752724E+03 0.18444877165841E+03 0.87519675385470E+03
 0.68982573833799E+03 0.15533290998690E+03 0.66227154673570E+02 0.19823150421529E+03 0.50341021310674E+03
 0.25674003729527E+03 0.67728705029235E+02 0.28178957694370E+03 0.50480852311429E+03 0.15508843293264E+03
 0.65881540377887E+02 0.19783864713882E+03 0.50253212508412E+03 0.67010677516766E+02 0.67728705029240E+02
 0.12670420648820E+03 0.50480852311431E+03 0.60658531265284E+02 0.00000000000000E+00 0.37762642882802E+03
 0.52082445910138E+00 0.38520835968543E+03 0.46585358248862E-01 0.88572628524163E+00 0.36150253286925E+03
 0.14613517118873E+00 0.36150253286925E+03 0.00000000000000E+00 0.39810411159851E+01 0.36150253286925E+03
 0.84955557329806E-01 0.36150253286925E+03 0.00000000000000E+00 0.64159435629280E+01 0.37768648548391E+03
 0.52272768946605E+00 0.38581368068256E+03 0.45636779863585E-01 0.86363905408962E+00 0.36150253286925E+03
 0.14737274387892E+00 0.36154154236479E+03 0.17923892046829E+00 0.39589269586652E+01 0.36150253286925E+03
 0.85263419979070E-01 0.36150253286925E+03 0.00000000000000E+00 0.63938364721836E+01 0.17481785913722E+04
 0.89977042753721E+03 0.12671464324203E+04 0.48103216366134E+03 0.30464999304056E+03 0.14236594861898E+04
 0.81710976173634E+03 0.10252879930387E+04 0.39837149786048E+03 0.30464999304056E+03 0.13673554509686E+04
 0.76080572651513E+03 0.10252879930387E+04 0.34206746263927E+03 0.30464999304056E+03 0.17540440850301E+04
 0.90473507135677E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14238817173468E+04 0.81733199289334E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13686046420938E+04 0.76205491764043E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18395409824592E-02 0.91977049122958E+05 0.91977049122958E+05 0.50000000000000E+08 0.38943809884058E+03
 0.90804038094282E+00 0.90804038094282E+00 0.37460374754761E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30660122125584E+03 0.30660122125584E+03 0.30660122125584E+03
 0.30660122125584E+03 0.22621718499288E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.51151275949525E+00 0.73428633624088E-01 0.10000000000000E-02 0.10894932406009E+03 0.40855996522532E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29604876911698E+03 0.30010873643678E+03 0.29463764212369E+03
 0.29463764212370E+03 0.29315504338963E+03 0.29315530329669E+03 0.29463567094868E+03 0.29463567094868E+03
 0.29315504067615E+03 0.29315530044338E+03 0.29465355175457E+03 0.29465355175435E+03 0.29364761012407E+03
 0.29356864524222E+03 0.29463567094868E+03 0.29463567094868E+03 0.29315504067615E+03 0.29315530044338E+03
 0.29491383666095E+03 0.29315614974769E+03 0.33092815137469E+01 0.17244318445781E+02 0.10925307711480E+02
 0.58367415503750E+02 0.47387481253712E+02 0.11999107441533E+02 0.75148315429687E+01 0.49883572246853E+02
 0.47894054429021E+15 0.11905429623127E+02 0.75148315429687E+01 0.49798157422505E+02 0.47901820039116E+15
 0.11888454414078E+02 0.73716918945312E+01 0.49706243915828E+02 0.47831375579893E+15 0.11905429623125E+02
 0.75148315429687E+01 0.49798157422500E+02 0.47901820039114E+15 0.70024622863007E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31915592567330E+03
 0.31915592567330E+03 0.31915592567330E+03 0.31915592567330E+03 0.22328637175095E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.34709462181689E+01 0.12682284712432E+00 0.10000000000000E-02
 0.63080116725008E+02 0.23655043771878E+02 0.80000000000000E+04 0.30000000000000E+04 0.29378578562913E+03
 0.29333226727367E+03 0.29736350774231E+03 0.29736350773969E+03 0.29982377301125E+03 0.29886953566660E+03
 0.29725982761616E+03 0.29725982761617E+03 0.29317684250986E+03 0.29317822588162E+03 0.29725921881970E+03
 0.29725921881971E+03 0.29317683434677E+03 0.29317821729780E+03 0.29725982761616E+03 0.29725982761617E+03
 0.29317684250986E+03 0.29317822588162E+03 0.29799646050644E+03 0.29318258285641E+03 0.61347950203334E+02
 0.10392035838318E+03 0.38708963049021E+02 0.14111020025977E+03 0.10220769239551E+03 0.40726435184063E+02
 0.24802653221038E+02 0.12043275972918E+03 0.24802653221038E+02 0.41371595151182E+02 0.25359368104344E+02
 0.12158440077633E+03 0.25359368104344E+02 0.41354836951154E+02 0.25362102760558E+02 0.12157061901821E+03
 0.25362102760558E+02 0.41371595151183E+02 0.25359368104345E+02 0.12158440077633E+03 0.25359368104345E+02
 0.90993239286132E+01 0.00000000000000E+00 0.78962638171961E+03 0.32725839853451E+01 0.31915592567330E+03
 0.00000000000000E+00 0.00000000000000E+00 0.78962638171961E+03 0.32725839853451E+01 0.31915592567330E+03
 0.00000000000000E+00 0.00000000000000E+00 0.75726981354366E+04 0.54429067822773E+03 0.74471457257942E+04
 0.12555241435186E+03 0.30464999304056E+03 0.75733901638797E+04 0.54498270667087E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32837965994330E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.72043647272957E+00 0.00000000000000E+00 0.35457681078265E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10614868431032E+01 0.10750132835122E+01 0.10614868431032E+01
 0.21538592816856E+01 0.90385928168562E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.88919367229686E+00 0.91494125095453E+00 0.91494125095453E+00
 0.88919367229686E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16295532953092E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16107531558782E+01 0.16295532953092E+01 0.16107531558782E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    243.40292126
 0.18102297191598E+01 0.31469777061868E+03 0.36187207467619E+03 0.32771354380196E+03 0.32643909444206E+03
 0.22454143422118E+00 0.00000000000000E+00 0.21433858628543E+00 0.00000000000000E+00 -.34103520410598E+01
 0.10228908156701E+00 0.26676395254694E+00 0.78209715811746E+02 0.29328643429405E+02 0.29989059329868E+02
 0.11245897248700E+02 0.29506075473735E+03 0.29391110968418E+03 0.30132545720520E+03 0.31550600382705E+03
 0.29326784066020E+03 0.29351471182891E+03 0.30576681751703E+03 0.31551935515647E+03 0.29336830845467E+03
 0.29351314144867E+03 0.30135914040048E+03 0.31555091811199E+03 0.29378066712713E+03 0.29403089934547E+03
 0.29756060426582E+03 0.31551935515647E+03 0.29318547712116E+03 0.29351314144867E+03 0.30034678773319E+03
 0.29626071563440E+03 0.20986439261716E+03 0.23992853700104E+03 0.18603861043078E+03 0.87831245852592E+03
 0.69134365504299E+03 0.15537151077688E+03 0.65939619385414E+02 0.19927095220618E+03 0.50203512653148E+03
 0.25566438144418E+03 0.67557179261736E+02 0.28143451776605E+03 0.50350587381845E+03 0.15513179658378E+03
 0.65617769674994E+02 0.19888517254433E+03 0.50121942920540E+03 0.68012589702772E+02 0.67557179261753E+02
 0.12896188827590E+03 0.50350587381850E+03 0.60822784698833E+02 0.00000000000000E+00 0.37802003965571E+03
 0.52106914779770E+00 0.38561008623253E+03 0.46591503766707E-01 0.88560945607212E+00 0.36187207467619E+03
 0.14618375538813E+00 0.36187207467619E+03 0.00000000000000E+00 0.39805160082457E+01 0.36187207467619E+03
 0.84983276906505E-01 0.36187207467619E+03 0.00000000000000E+00 0.64150972864083E+01 0.37808040915062E+03
 0.52297634912296E+00 0.38621642105647E+03 0.45642800245429E-01 0.86352513827259E+00 0.36187207467619E+03
 0.14742175813917E+00 0.36190248949574E+03 0.17926256557966E+00 0.39584047678299E+01 0.36187207467619E+03
 0.85291242662378E-01 0.36187207467619E+03 0.00000000000000E+00 0.63929931116360E+01 0.17580476929390E+04
 0.90373536668871E+03 0.12730505948356E+04 0.48499710281284E+03 0.30464999304056E+03 0.14343089329244E+04
 0.82221608136674E+03 0.10308311201429E+04 0.40347781749088E+03 0.30464999304056E+03 0.13771778065672E+04
 0.76508495500961E+03 0.10308311201429E+04 0.34634669113375E+03 0.30464999304056E+03 0.17638765839469E+04
 0.90865871527283E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14345356352286E+04 0.82244278367099E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13784108295785E+04 0.76631797802093E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18417838364494E-02 0.92089191822470E+05 0.92089191822470E+05 0.50000000000000E+08 0.38982471560947E+03
 0.90870708252692E+00 0.90870708252692E+00 0.39174156525473E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30681682268393E+03 0.30681682268393E+03 0.30681682268393E+03
 0.30681682268393E+03 0.22607036613415E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.51964596611244E+00 0.76594222483069E-01 0.10000000000000E-02 0.10444652012452E+03 0.39167445046696E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29626069287331E+03 0.30034724720686E+03 0.29472530415707E+03
 0.29472530415708E+03 0.29315653699328E+03 0.29315687387311E+03 0.29472311082229E+03 0.29472311082229E+03
 0.29315653323333E+03 0.29315686991938E+03 0.29474177981421E+03 0.29474177981399E+03 0.29366754640990E+03
 0.29358820091410E+03 0.29472311082229E+03 0.29472311082229E+03 0.29315653323333E+03 0.29315686991938E+03
 0.29501387926156E+03 0.29315796203556E+03 0.31723646940504E+01 0.17110281479062E+02 0.11514858067596E+02
 0.59565183158633E+02 0.47992750800699E+02 0.12631352009891E+02 0.78726806640625E+01 0.51035904689509E+02
 0.48425768811712E+15 0.12529910463671E+02 0.78726806640625E+01 0.50943685631500E+02 0.48434411698809E+15
 0.12516813844246E+02 0.78011108398437E+01 0.50852107721308E+02 0.48360844458255E+15 0.12529910463669E+02
 0.78726806640625E+01 0.50943685631497E+02 0.48434411698808E+15 0.71430841686469E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31948228746308E+03
 0.31948228746308E+03 0.31948228746308E+03 0.31948228746308E+03 0.22308242746958E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.35109162696086E+01 0.13094920043994E+00 0.10000000000000E-02
 0.61092392875429E+02 0.22909647328286E+02 0.80000000000000E+04 0.30000000000000E+04 0.29382131772861E+03
 0.29335186050186E+03 0.29756123624529E+03 0.29756123624266E+03 0.30002516698025E+03 0.29906718340342E+03
 0.29744408116278E+03 0.29744408116279E+03 0.29318292040181E+03 0.29318461703019E+03 0.29744346125810E+03
 0.29744346125811E+03 0.29318291080048E+03 0.29318460693400E+03 0.29744408116278E+03 0.29744408116279E+03
 0.29318292040181E+03 0.29318461703019E+03 0.29820361535350E+03 0.29318992667105E+03 0.63304373199742E+02
 0.10647474346100E+03 0.39684017391773E+02 0.14279091161674E+03 0.10290847413800E+03 0.41753716037895E+02
 0.25399378975816E+02 0.12203399285715E+03 0.25399378975816E+02 0.42480654339180E+02 0.26029608002132E+02
 0.12333406134212E+03 0.26029608002132E+02 0.42464112689247E+02 0.26032391761175E+02 0.12332055511012E+03
 0.26032391761175E+02 0.42480654339180E+02 0.26029608002132E+02 0.12333406134212E+03 0.26029608002132E+02
 0.92344527536631E+01 0.00000000000000E+00 0.79341736913982E+03 0.32751805807614E+01 0.31948228746308E+03
 0.00000000000000E+00 0.00000000000000E+00 0.79341736913982E+03 0.32751805807614E+01 0.31948228746308E+03
 0.00000000000000E+00 0.00000000000000E+00 0.76317775516010E+04 0.54650528308903E+03 0.75040105370973E+04
 0.12776701921316E+03 0.30464999304056E+03 0.76324801325371E+04 0.54720786402509E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32874051292328E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.72256234053452E+00 0.00000000000000E+00 0.34203039012421E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10519593416935E+01 0.10645927306587E+01 0.10519593416935E+01
 0.21551148595799E+01 0.90511485957990E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.89684187485243E+00 0.92064594358434E+00 0.92064594358434E+00
 0.89684187485243E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16339687754528E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16164923796965E+01 0.16339687754528E+01 0.16164923796965E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    252.71076532
 0.18126337781638E+01 0.31514251476269E+03 0.36222241339202E+03 0.32808696758080E+03 0.32682188727913E+03
 0.22431431653383E+00 0.00000000000000E+00 0.21411040118798E+00 0.00000000000000E+00 -.34515274175639E+01
 0.10691198288696E+00 0.27068304135608E+00 0.74827907817016E+02 0.28060465431381E+02 0.29554862247451E+02
 0.11083073342794E+02 0.29512699550140E+03 0.29396257158915E+03 0.30155898357520E+03 0.31599059018550E+03
 0.29328692169880E+03 0.29357208276599E+03 0.30604254647608E+03 0.31600784354576E+03 0.29340178625255E+03
 0.29357034872447E+03 0.30159210433293E+03 0.31603239572041E+03 0.29381807629933E+03 0.29410772304446E+03
 0.29775746292180E+03 0.31600784354575E+03 0.29319255837053E+03 0.29357034872447E+03 0.30058441171101E+03
 0.29647426445598E+03 0.21107206825779E+03 0.24184950477930E+03 0.18754505216951E+03 0.88110343321059E+03
 0.69262065578023E+03 0.15540827861577E+03 0.65655701198322E+02 0.20018365526650E+03 0.50060722094806E+03
 0.25463274404266E+03 0.67382961741795E+02 0.28102254936938E+03 0.50214476815512E+03 0.15517233035949E+03
 0.65354938374105E+02 0.19980347531811E+03 0.49984680061984E+03 0.68972258202321E+02 0.67382961741827E+02
 0.13103227896869E+03 0.50214476815521E+03 0.60965398709723E+02 0.00000000000000E+00 0.37839183062680E+03
 0.52128792493491E+00 0.38598852004939E+03 0.46598101252052E-01 0.88548406908742E+00 0.36222241339202E+03
 0.14663428253415E+00 0.36222241339202E+03 0.00000000000000E+00 0.39799524360115E+01 0.36222241339202E+03
 0.85005869597650E-01 0.36222241339202E+03 0.00000000000000E+00 0.64141890195648E+01 0.37845250036981E+03
 0.52319923038582E+00 0.38659574906860E+03 0.45649263391742E-01 0.86340287804541E+00 0.36222241339202E+03
 0.14746194731130E+00 0.36224500514343E+03 0.17928794965302E+00 0.39578443261650E+01 0.36222241339202E+03
 0.85313920077251E-01 0.36222241339202E+03 0.00000000000000E+00 0.63920879743613E+01 0.17674409469279E+04
 0.90755175611119E+03 0.12786274594020E+04 0.48881349223532E+03 0.30464999304056E+03 0.14444762901034E+04
 0.82712835780842E+03 0.10360862008803E+04 0.40839009393255E+03 0.30464999304056E+03 0.13865466252684E+04
 0.76919869297338E+03 0.10360862008803E+04 0.35046042909751E+03 0.30464999304056E+03 0.17732346340430E+04
 0.91243539708118E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14447072798530E+04 0.82735934755793E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13877641259283E+04 0.77041619363332E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18440266904396E-02 0.92201334521981E+05 0.92201334521981E+05 0.50000000000000E+08 0.39018803033403E+03
 0.90937327830880E+00 0.90937327830880E+00 0.40890025909703E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30701383401239E+03 0.30701383401239E+03 0.30701383401239E+03
 0.30701383401239E+03 0.22592908966752E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.52706026967985E+00 0.79637970325355E-01 0.10000000000000E-02 0.10045459430114E+03 0.37670472862928E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29647424801029E+03 0.30058482050763E+03 0.29481163041254E+03
 0.29481163041254E+03 0.29315831738572E+03 0.29315874601859E+03 0.29480920412651E+03 0.29480920412651E+03
 0.29315831229723E+03 0.29315874066787E+03 0.29482873992642E+03 0.29482873992620E+03 0.29368710303734E+03
 0.29360751947294E+03 0.29480920412651E+03 0.29480920412651E+03 0.29315831229723E+03 0.29315874066787E+03
 0.29511206482282E+03 0.29316011862288E+03 0.29927060918280E+01 0.16900381408346E+02 0.12084377278507E+02
 0.60654980002682E+02 0.48510180837782E+02 0.13240543897676E+02 0.82305297851563E+01 0.52095340820109E+02
 0.48887469299323E+15 0.13131165729649E+02 0.82305297851563E+01 0.51996191218583E+02 0.48897032379087E+15
 0.13121647640520E+02 0.81589599609375E+01 0.51904334585771E+02 0.48820031260364E+15 0.13131165729649E+02
 0.82305297851563E+01 0.51996191218583E+02 0.48897032379088E+15 0.72693041560380E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.31978310242342E+03
 0.31978310242342E+03 0.31978310242342E+03 0.31978310242342E+03 0.22288601337946E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.35476848761345E+01 0.13492226731523E+00 0.10000000000000E-02
 0.59293400260676E+02 0.22235025097753E+02 0.80000000000000E+04 0.30000000000000E+04 0.29385697144928E+03
 0.29337222991594E+03 0.29775670988437E+03 0.29775670988174E+03 0.30022274298143E+03 0.29926157476537E+03
 0.29762471140496E+03 0.29762471140497E+03 0.29318984278697E+03 0.29319189620784E+03 0.29762408179014E+03
 0.29762408179015E+03 0.29318983161632E+03 0.29319188446142E+03 0.29762471140496E+03 0.29762471140497E+03
 0.29318984278697E+03 0.29319189620784E+03 0.29840599783063E+03 0.29319827718803E+03 0.65180885030434E+02
 0.10889750084529E+03 0.40605293635007E+02 0.14428862586879E+03 0.10348030576561E+03 0.42712584061852E+02
 0.25952863809814E+02 0.12345762575989E+03 0.25952863809814E+02 0.43529680285245E+02 0.26664235335830E+02
 0.12492119520926E+03 0.26664235335830E+02 0.43513405390670E+02 0.26667061144692E+02 0.12490800709966E+03
 0.26667061144692E+02 0.43529680285244E+02 0.26664235335829E+02 0.12492119520926E+03 0.26664235335829E+02
 0.93573092351429E+01 0.00000000000000E+00 0.79708573673071E+03 0.32777870355582E+01 0.31978310242342E+03
 0.00000000000000E+00 0.00000000000000E+00 0.79708573673071E+03 0.32777870355582E+01 0.31978310242342E+03
 0.00000000000000E+00 0.00000000000000E+00 0.76889327644280E+04 0.54863498205267E+03 0.75590360509606E+04
 0.12989671817680E+03 0.30464999304056E+03 0.76896453240510E+04 0.54934754167569E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32907584524265E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.72459552786289E+00 0.00000000000000E+00 0.33025912006084E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10430157781020E+01 0.10548546479237E+01 0.10430157781020E+01
 0.21563168890819E+01 0.90631688908192E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.90374614872024E+00 0.92583422273981E+00 0.92583422273981E+00
 0.90374614872024E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16381382023221E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16218361404062E+01 0.16381382023221E+01 0.16218361404062E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    262.01860938
 0.18149286355318E+01 0.31555507048941E+03 0.36255557213532E+03 0.32843454960671E+03 0.32717778368564E+03
 0.22409389607846E+00 0.00000000000000E+00 0.21388888224224E+00 0.00000000000000E+00 -.34895434299826E+01
 0.11139771960923E+00 0.27448919920032E+00 0.71814755526979E+02 0.26930533322617E+02 0.29145044771550E+02
 0.10929391789331E+02 0.29519314565629E+03 0.29401459526549E+03 0.30178726412612E+03 0.31646072909238E+03
 0.29330763142771E+03 0.29363400192063E+03 0.30631080081921E+03 0.31648182205724E+03 0.29343776289346E+03
 0.29363210854181E+03 0.30181996890197E+03 0.31649980876465E+03 0.29385653136253E+03 0.29418866131473E+03
 0.29795109219304E+03 0.31648182205724E+03 0.29320051951769E+03 0.29363210854181E+03 0.30082118448750E+03
 0.29668922196363E+03 0.21224000901442E+03 0.24367257406586E+03 0.18897334775290E+03 0.88360091375422E+03
 0.69368269926255E+03 0.15544355041021E+03 0.65376619975124E+02 0.20098312647108E+03 0.49913888626184E+03
 0.25364321043488E+03 0.67207767002344E+02 0.28056228330311E+03 0.50073811686237E+03 0.15521033455157E+03
 0.65094383828081E+02 0.20060698380533E+03 0.49842698757800E+03 0.69891610502993E+02 0.67207767002356E+02
 0.13293265203563E+03 0.50073811686240E+03 0.61088792895811E+02 0.00000000000000E+00 0.37874422929426E+03
 0.52148471410909E+00 0.38634634112781E+03 0.46605033050879E-01 0.88535236662892E+00 0.36255557213532E+03
 0.14666694872783E+00 0.36255557213532E+03 0.00000000000000E+00 0.39793604778511E+01 0.36255557213532E+03
 0.85024210747824E-01 0.36255557213532E+03 0.00000000000000E+00 0.64132350052661E+01 0.37880518665226E+03
 0.52340021191923E+00 0.38695436193945E+03 0.45656054044191E-01 0.86327445983250E+00 0.36255557213532E+03
 0.14749481192098E+00 0.36257109035669E+03 0.17931462001009E+00 0.39572556562540E+01 0.36255557213532E+03
 0.85332330689066E-01 0.36255557213532E+03 0.00000000000000E+00 0.63911372472613E+01 0.17764063649744E+04
 0.91123119414580E+03 0.12839134394139E+04 0.49249293026993E+03 0.30464999304056E+03 0.14542056777022E+04
 0.83186036425780E+03 0.10410835820298E+04 0.41312210038193E+03 0.30464999304056E+03 0.13955045601494E+04
 0.77315924670501E+03 0.10410835820298E+04 0.35442098282914E+03 0.30464999304056E+03 0.17821661787859E+04
 0.91607664758734E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14544407819289E+04 0.83209546848442E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.13967071422256E+04 0.77436182878120E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18462695444299E-02 0.92313477221493E+05 0.92313477221493E+05 0.50000000000000E+08 0.39053078830992E+03
 0.91003897140250E+00 0.91003897140250E+00 0.42607982907453E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30719456971086E+03 0.30719456971086E+03 0.30719456971086E+03
 0.30719456971086E+03 0.22579312790164E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.53384501720081E+00 0.82565069832588E-01 0.10000000000000E-02 0.96893274797940E+02 0.36334978049227E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29668921160736E+03 0.30082154848691E+03 0.29489659508957E+03
 0.29489659508957E+03 0.29316040883511E+03 0.29316094525238E+03 0.29489392516046E+03 0.29489392516046E+03
 0.29316040208958E+03 0.29316093815922E+03 0.29491441701340E+03 0.29491441701319E+03 0.29370632062671E+03
 0.29362662775571E+03 0.29489392516046E+03 0.29489392516046E+03 0.29316040208958E+03 0.29316093815922E+03
 0.29520839107014E+03 0.29316264759734E+03 0.27723081819641E+01 0.16620830403393E+02 0.12634618821736E+02
 0.61649374541293E+02 0.48951582625448E+02 0.13827576816474E+02 0.85883789062500E+01 0.53071947741876E+02
 0.49288572191178E+15 0.13710095455307E+02 0.86599487304687E+01 0.52965746964178E+02 0.49299098005512E+15
 0.13703775405047E+02 0.85168090820312E+01 0.52872868585691E+02 0.49218309852880E+15 0.13710095455307E+02
 0.86599487304687E+01 0.52965746964178E+02 0.49299098005512E+15 0.73826396363958E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32006133392350E+03
 0.32006133392350E+03 0.32006133392350E+03 0.32006133392350E+03 0.22269679725931E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.35816284366715E+01 0.13874898739343E+00 0.10000000000000E-02
 0.57658078450083E+02 0.21621779418781E+02 0.80000000000000E+04 0.30000000000000E+04 0.29389273084953E+03
 0.29339333730249E+03 0.29794996656524E+03 0.29794996656260E+03 0.30041676684503E+03 0.29945290503261E+03
 0.29780174377179E+03 0.29780174377181E+03 0.29319764729053E+03 0.29320010298323E+03 0.29780110589014E+03
 0.29780110589015E+03 0.29319763442087E+03 0.29320008945021E+03 0.29780174377179E+03 0.29780174377181E+03
 0.29319764729053E+03 0.29320010298323E+03 0.29860369577579E+03 0.29320767592609E+03 0.66981226444019E+02
 0.11119819698765E+03 0.41476044355473E+02 0.14562436328134E+03 0.10394093870409E+03 0.43606502057338E+02
 0.26465092299357E+02 0.12472065840569E+03 0.26465092299357E+02 0.44522238386877E+02 0.27265337565267E+02
 0.12636295295288E+03 0.27265337565267E+02 0.44506279702133E+02 0.27268198035020E+02 0.12635012465735E+03
 0.27268198035020E+02 0.44522238386878E+02 0.27265337565268E+02 0.12636295295288E+03 0.27265337565268E+02
 0.94691439936423E+01 0.00000000000000E+00 0.80063040631468E+03 0.32803930946557E+01 0.32006133392350E+03
 0.00000000000000E+00 0.00000000000000E+00 0.80063040631468E+03 0.32803930946557E+01 0.32006133392350E+03
 0.00000000000000E+00 0.00000000000000E+00 0.77441529430963E+04 0.55068511696131E+03 0.76122060947203E+04
 0.13194685308544E+03 0.30464999304056E+03 0.77448749516834E+04 0.55140712554845E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32938847983977E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.72654029292797E+00 0.00000000000000E+00 0.31921431537333E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10346212609472E+01 0.10457546083013E+01 0.10346212609472E+01
 0.21574643177659E+01 0.90746431776592E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.91000487473967E+00 0.93057926058430E+00 0.93057926058430E+00
 0.91000487473967E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16420834106623E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16268217544196E+01 0.16420834106623E+01 0.16268217544196E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    271.32645344
 0.18171144939807E+01 0.31593872980559E+03 0.36287339278041E+03 0.32875913023575E+03 0.32750972367229E+03
 0.22388005216587E+00 0.00000000000000E+00 0.21367370748334E+00 0.00000000000000E+00 -.35247630329148E+01
 0.11574876030680E+00 0.27818757515913E+00 0.69115211072633E+02 0.25918204152238E+02 0.28757574796157E+02
 0.10784090548559E+02 0.29525921197857E+03 0.29406713635752E+03 0.30201047111198E+03 0.31691729768429E+03
 0.29332996434176E+03 0.29370038255075E+03 0.30657196318481E+03 0.31694216658601E+03 0.29347618239369E+03
 0.29369833639952E+03 0.30204291132303E+03 0.31695401456352E+03 0.29389604712591E+03 0.29427364849705E+03
 0.29814146614292E+03 0.31694216658601E+03 0.29320939638399E+03 0.29369833639952E+03 0.30105718520948E+03
 0.29690539467564E+03 0.21337039022117E+03 0.24540547621898E+03 0.19032835060522E+03 0.88583342429887E+03
 0.69455343194063E+03 0.15547769847280E+03 0.65103261153854E+02 0.20168197521709E+03 0.49764134047906E+03
 0.25269402843423E+03 0.67032925718638E+02 0.28006186694536E+03 0.49929760048330E+03 0.15524614265911E+03
 0.64837095565674E+02 0.20130822419665E+03 0.49697152624684E+03 0.70772629633252E+02 0.67032925718650E+02
 0.13467915054850E+03 0.49929760048333E+03 0.61195177673972E+02 0.00000000000000E+00 0.37907942350239E+03
 0.52166286761570E+00 0.38668595064867E+03 0.46612203799609E-01 0.88521616540175E+00 0.36287339278041E+03
 0.14669358900483E+00 0.36287339278041E+03 0.00000000000000E+00 0.39787482992419E+01 0.36287339278041E+03
 0.85039038041520E-01 0.36287339278041E+03 0.00000000000000E+00 0.64122484031958E+01 0.37914065603742E+03
 0.52358261018301E+00 0.38729467575520E+03 0.45663078781009E-01 0.86314165503607E+00 0.36287339278041E+03
 0.14752161708523E+00 0.36288255614209E+03 0.17934220973591E+00 0.39566468782161E+01 0.36287339278041E+03
 0.85347214813950E-01 0.36287339278041E+03 0.00000000000000E+00 0.63901540446756E+01 0.17849876360697E+04
 0.91478455211907E+03 0.12889413525359E+04 0.49604628824321E+03 0.30464999304056E+03 0.14635375731871E+04
 0.83642495006631E+03 0.10458508917061E+04 0.41768668619044E+03 0.30464999304056E+03 0.14040907206311E+04
 0.77697809751032E+03 0.10458508917061E+04 0.35823983363445E+03 0.30464999304056E+03 0.17907148489951E+04
 0.91959327701917E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14637766290027E+04 0.83666400588191E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14052789482231E+04 0.77816632510228E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18485123984201E-02 0.92425619921005E+05 0.92425619921005E+05 0.50000000000000E+08 0.39085543861386E+03
 0.91070416468944E+00 0.91070416468944E+00 0.44328027518722E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30736106150054E+03 0.30736106150054E+03 0.30736106150054E+03
 0.30736106150054E+03 0.22566225455550E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.54007864608629E+00 0.85380671637534E-01 0.10000000000000E-02 0.93698021420613E+02 0.35136758032730E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29690539014192E+03 0.30105750950984E+03 0.29498019588073E+03
 0.29498019588073E+03 0.29316283383858E+03 0.29316349523145E+03 0.29497727173069E+03 0.29497727173069E+03
 0.29316282505848E+03 0.29316348599885E+03 0.29499881854146E+03 0.29499881854124E+03 0.29372524176123E+03
 0.29364555637679E+03 0.29497727173069E+03 0.29497727173069E+03 0.29316282505848E+03 0.29316348599885E+03
 0.29530288111081E+03 0.29316557475219E+03 0.25132709854204E+01 0.16277449574336E+02 0.13166452052089E+02
 0.62559586814199E+02 0.49327302501850E+02 0.14393471486393E+02 0.89462280273438E+01 0.53974804450385E+02
 0.49637276042751E+15 0.14267726663132E+02 0.90177978515625E+01 0.53861437494784E+02 0.49648806702338E+15
 0.14264150337955E+02 0.88746582031250E+01 0.53766676675184E+02 0.49563840064151E+15 0.14267726663133E+02
 0.90177978515625E+01 0.53861437494785E+02 0.49648806702338E+15 0.74844621494507E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32031962408856E+03
 0.32031962408856E+03 0.32031962408856E+03 0.32031962408856E+03 0.22251445937804E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.36130812636520E+01 0.14243593011416E+00 0.10000000000000E-02
 0.56165603675899E+02 0.21062101378462E+02 0.80000000000000E+04 0.30000000000000E+04 0.29392858530222E+03
 0.29341514525083E+03 0.29814106994703E+03 0.29814106994438E+03 0.30060750302630E+03 0.29964137668155E+03
 0.29797523694488E+03 0.29797523694490E+03 0.29320636616720E+03 0.29320927127856E+03 0.29797459227659E+03
 0.29797459227661E+03 0.29320635147107E+03 0.29320925582490E+03 0.29797523694488E+03 0.29797523694490E+03
 0.29320636616720E+03 0.29320927127856E+03 0.29879683072882E+03 0.29321815769702E+03 0.68709299191286E+02
 0.11338596251079E+03 0.42299515691167E+02 0.14681702239866E+03 0.10430600912904E+03 0.44438978342119E+02
 0.26938094457635E+02 0.12583860388995E+03 0.26938094457635E+02 0.45461894928541E+02 0.27835006696060E+02
 0.12767491467932E+03 0.27835006696060E+02 0.45446300926180E+02 0.27837894149565E+02 0.12766248679296E+03
 0.27837894149565E+02 0.45461894928542E+02 0.27835006696061E+02 0.12767491467932E+03 0.27835006696061E+02
 0.95710960619880E+01 0.00000000000000E+00 0.80405244502943E+03 0.32829912468598E+01 0.32031962408856E+03
 0.00000000000000E+00 0.00000000000000E+00 0.80405244502943E+03 0.32829912468598E+01 0.32031962408856E+03
 0.00000000000000E+00 0.00000000000000E+00 0.77974593104126E+04 0.55266090355656E+03 0.76635366754414E+04
 0.13392263968069E+03 0.30464999304056E+03 0.77981902723700E+04 0.55339186551392E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32968095629389E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.72840162026580E+00 0.00000000000000E+00 0.30884666735245E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10267405364778E+01 0.10372482876182E+01 0.10267405364778E+01
 0.21585572469904E+01 0.90855724699037E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.91570287443108E+00 0.93494307642418E+00 0.93494307642418E+00
 0.91570287443108E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16458251122588E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16314841916508E+01 0.16458251122588E+01 0.16314841916508E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    280.63429750
 0.18191933849857E+01 0.31629649485710E+03 0.36317755047514E+03 0.32906326797054E+03 0.32782035727030E+03
 0.22367264641126E+00 0.00000000000000E+00 0.21346460326017E+00 0.00000000000000E+00 -.35575100016846E+01
 0.11996790355096E+00 0.28178232222693E+00 0.66684502797883E+02 0.25006688549206E+02 0.28390709313401E+02
 0.10646515992525E+02 0.29532520125490E+03 0.29412015427278E+03 0.30222878707752E+03 0.31736111134105E+03
 0.29335390816018E+03 0.29377111756569E+03 0.30682640133637E+03 0.31738969190728E+03 0.29351697995790E+03
 0.29376892730490E+03 0.30226111853405E+03 0.31739581047143E+03 0.29393663339055E+03 0.29436259955268E+03
 0.29832859305867E+03 0.31738969190728E+03 0.29321922005652E+03 0.29376892730490E+03 0.30129248090863E+03
 0.29712261351723E+03 0.21446540677007E+03 0.24705556406501E+03 0.19161456418454E+03 0.88782699569573E+03
 0.69525435869026E+03 0.15551113630780E+03 0.64836245417234E+02 0.20229188424110E+03 0.49612470896471E+03
 0.25178360624766E+03 0.66859460782114E+02 0.27952890880043E+03 0.49783375619020E+03 0.15528013334527E+03
 0.64583789607823E+02 0.20191880659765E+03 0.49549083268066E+03 0.71617337651378E+02 0.66859460782126E+02
 0.13628677427713E+03 0.49783375619023E+03 0.61286566631237E+02 0.00000000000000E+00 0.37939938128389E+03
 0.52182521908621E+00 0.38700949473429E+03 0.46619537723675E-01 0.88507690816206E+00 0.36317755047514E+03
 0.14671525101313E+00 0.36317755047514E+03 0.00000000000000E+00 0.39781223848865E+01 0.36317755047514E+03
 0.85050965397621E-01 0.36317755047514E+03 0.00000000000000E+00 0.64112396642598E+01 0.37946087695667E+03
 0.52374923092830E+00 0.38761884943319E+03 0.45670263370561E-01 0.86300587043448E+00 0.36317755047514E+03
 0.14754341621035E+00 0.36318103915270E+03 0.17937042728498E+00 0.39560244407323E+01 0.36317755047514E+03
 0.85514060397435E-01 0.36317755047514E+03 0.00000000000000E+00 0.63891487815001E+01 0.17932244920091E+04
 0.91822204133599E+03 0.12937407192584E+04 0.49948377746012E+03 0.30464999304056E+03 0.14725091375748E+04
 0.84083414903295E+03 0.10504132571271E+04 0.42209588515709E+03 0.30464999304056E+03 0.14123409649241E+04
 0.78066597638226E+03 0.10504132571271E+04 0.36192771250640E+03 0.30464999304056E+03 0.17989203263201E+04
 0.92299544055531E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14727519916653E+04 0.84107700312352E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14135153648022E+04 0.78184037626043E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18507552524103E-02 0.92537762620516E+05 0.92537762620516E+05 0.50000000000000E+08 0.39116416011515E+03
 0.91136886083997E+00 0.91136886083997E+00 0.46050159743510E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30751509298611E+03 0.30751509298611E+03 0.30751509298611E+03
 0.30751509298611E+03 0.22553624804194E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.54583002740235E+00 0.88089799465822E-01 0.10000000000000E-02 0.90816417434393E+02 0.34056156537897E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29712261451144E+03 0.30129276995490E+03 0.29506244631655E+03
 0.29506244631655E+03 0.29316561291110E+03 0.29316641752862E+03 0.29505925747746E+03 0.29505925747746E+03
 0.29316560166765E+03 0.29316640570572E+03 0.29508196638904E+03 0.29508196638882E+03 0.29374390905104E+03
 0.29366433738151E+03 0.29505925747746E+03 0.29505925747746E+03 0.29316560166765E+03 0.29316640570572E+03
 0.29539557447208E+03 0.29316892335209E+03 0.22177223383987E+01 0.15875653134889E+02 0.13680817020288E+02
 0.63395622947764E+02 0.49646401842375E+02 0.14939322253459E+02 0.93040771484375E+01 0.54812080971202E+02
 0.49940726889746E+15 0.14805160237850E+02 0.93040771484375E+01 0.54691438754118E+02 0.49953304090049E+15
 0.14803809140454E+02 0.92325073242187E+01 0.54593833130151E+02 0.49863734795255E+15 0.14805160237850E+02
 0.93040771484375E+01 0.54691438754118E+02 0.49953304090049E+15 0.75760087146340E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32056032658517E+03
 0.32056032658517E+03 0.32056032658517E+03 0.32056032658517E+03 0.22233869307683E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.36423401579038E+01 0.14598931533089E+00 0.10000000000000E-02
 0.54798530850479E+02 0.20549449068930E+02 0.80000000000000E+04 0.30000000000000E+04 0.29396452761774E+03
 0.29343761730209E+03 0.29833009786779E+03 0.29833009786513E+03 0.30079520847365E+03 0.29982719137727E+03
 0.29814527123640E+03 0.29814527123642E+03 0.29321602632215E+03 0.29321942939157E+03 0.29814462129295E+03
 0.29814462129297E+03 0.29321600967508E+03 0.29321941188637E+03 0.29814527123640E+03 0.29814527123642E+03
 0.29321602632215E+03 0.29321942939157E+03 0.29898554480746E+03 0.29322975069222E+03 0.70369058001724E+02
 0.11546943821120E+03 0.43078902937021E+02 0.14788358641456E+03 0.10458928896286E+03 0.45213513561806E+02
 0.27373912587908E+02 0.12682561252501E+03 0.27373912587908E+02 0.46352163874644E+02 0.28375306058813E+02
 0.12887121511986E+03 0.28375306058813E+02 0.46336981827673E+02 0.28378212564525E+02 0.12885922694917E+03
 0.28378212564525E+02 0.46352163874644E+02 0.28375306058813E+02 0.12887121511986E+03 0.28375306058813E+02
 0.96642014199961E+01 0.00000000000000E+00 0.80735460569557E+03 0.32855759534825E+01 0.32056032658517E+03
 0.00000000000000E+00 0.00000000000000E+00 0.80735460569557E+03 0.32855759534825E+01 0.32056032658517E+03
 0.00000000000000E+00 0.00000000000000E+00 0.78488981886593E+04 0.55456737181113E+03 0.77130690854335E+04
 0.13582910793527E+03 0.30464999304056E+03 0.78496376459653E+04 0.55530682911711E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32995555482124E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73018466567981E+00 0.00000000000000E+00 0.29910789375052E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10193383906313E+01 0.10292925594303E+01 0.10193383906313E+01
 0.21595966924928E+01 0.90959669249283E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.92091347748936E+00 0.93897841288647E+00 0.93897841288647E+00
 0.92091347748936E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16493827780189E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16358559892029E+01 0.16493827780189E+01 0.16358559892029E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    294.59606359
 0.18221186572449E+01 0.31679046710082E+03 0.36361144276034E+03 0.32948609344049E+03 0.32825148769346E+03
 0.22337328025434E+00 0.00000000000000E+00 0.21316182515194E+00 0.00000000000000E+00 -.36026158110787E+01
 0.12605590435067E+00 0.28698741852409E+00 0.63463905488670E+02 0.23798964558251E+02 0.27875786475735E+02
 0.10453419928401E+02 0.29542405577893E+03 0.29420049448193E+03 0.30254750778245E+03 0.31800456614530E+03
 0.29339280292648E+03 0.29388510998267E+03 0.30719622131454E+03 0.31803859349599E+03 0.29358247638015E+03
 0.29388272399453E+03 0.30257997553666E+03 0.31803684942779E+03 0.29399952966065E+03 0.29450323326251E+03
 0.29860328125305E+03 0.31803859349599E+03 0.29323578713755E+03 0.29388272399453E+03 0.30164422514219E+03
 0.29745008384260E+03 0.21604641044834E+03 0.24939046629505E+03 0.19342397487089E+03 0.89042045971255E+03
 0.69602936496731E+03 0.15556092475775E+03 0.64448428307924E+02 0.20306312928477E+03 0.49383332355552E+03
 0.25048751344551E+03 0.66603446857504E+02 0.27868355593218E+03 0.49561439417662E+03 0.15532861532838E+03
 0.64212304640068E+02 0.20268771608314E+03 0.49324241724374E+03 0.72820727578159E+02 0.66603446857504E+02
 0.13846768049981E+03 0.49561439417662E+03 0.61399465570811E+02 0.00000000000000E+00 0.37985455323772E+03
 0.52204423652719E+00 0.38746880866692E+03 0.46630717636698E-01 0.88486470720626E+00 0.36361144276034E+03
 0.14674026943546E+00 0.36361144276034E+03 0.00000000000000E+00 0.39771686131131E+01 0.36361144276034E+03
 0.85218394017519E-01 0.36361144276034E+03 0.00000000000000E+00 0.64097025422628E+01 0.37991642225291E+03
 0.52397465288198E+00 0.38807899120273E+03 0.45681215636438E-01 0.86279896110392E+00 0.36361144276034E+03
 0.14756859847538E+00 0.36361144276034E+03 0.00000000000000E+00 0.39550759670350E+01 0.36361144276034E+03
 0.85527684152925E-01 0.36361144276034E+03 0.00000000000000E+00 0.63876169558862E+01 0.18050117454310E+04
 0.92318171545053E+03 0.13005682985657E+04 0.50444345157466E+03 0.30464999304056E+03 0.14853643271424E+04
 0.84718095432269E+03 0.10569216414051E+04 0.42844269044682E+03 0.30464999304056E+03 0.14241572615852E+04
 0.78597388876545E+03 0.10569216414051E+04 0.36723562488959E+03 0.30464999304056E+03 0.18106627090993E+04
 0.92790464389096E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14856126108796E+04 0.84742923805990E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14253118314924E+04 0.78712845867267E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18541195333957E-02 0.92705976669784E+05 0.92705976669784E+05 0.50000000000000E+08 0.39160153953966E+03
 0.91236497830809E+00 0.91236497830809E+00 0.48637272356039E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30772614528171E+03 0.30772614528171E+03 0.30772614528171E+03
 0.30772614528171E+03 0.22535589592365E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.55368295173317E+00 0.91964395958423E-01 0.10000000000000E-02 0.86990186980805E+02 0.32621320117802E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29745009254321E+03 0.30164446845682E+03 0.29518334978295E+03
 0.29518334978296E+03 0.29317048502078E+03 0.29317154073583E+03 0.29517974454042E+03 0.29517974454043E+03
 0.29317046916174E+03 0.29317152405944E+03 0.29520441170826E+03 0.29520441170805E+03 0.29377152524199E+03
 0.29369230279233E+03 0.29517974454042E+03 0.29517974454043E+03 0.29317046916174E+03 0.29317152405944E+03
 0.29553136264154E+03 0.29317478073624E+03 0.17105523233511E+01 0.15174282773253E+02 0.14421732458881E+02
 0.64529751543628E+02 0.50035910422453E+02 0.15722975688165E+02 0.98050659179687E+01 0.55960825810267E+02
 0.50324432067848E+15 0.15575914588617E+02 0.98050659179687E+01 0.55829079757547E+02 0.50338656277855E+15
 0.15576814295893E+02 0.96619262695313E+01 0.55725222648241E+02 0.50241331073790E+15 0.15575914588608E+02
 0.98050659179687E+01 0.55829079757532E+02 0.50338656277849E+15 0.76964592310978E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32089291483663E+03
 0.32089291483663E+03 0.32089291483663E+03 0.32089291483663E+03 0.22208672507424E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.36826818548257E+01 0.15108182545089E+00 0.10000000000000E-02
 0.52951438573931E+02 0.19856789465224E+02 0.80000000000000E+04 0.30000000000000E+04 0.29401859683358E+03
 0.29347249368975E+03 0.29860994238022E+03 0.29860994237756E+03 0.30107162404245E+03 0.30010136371777E+03
 0.29839405367956E+03 0.29839405367959E+03 0.29323232744092E+03 0.29323657084404E+03 0.29839339871000E+03
 0.29839339871002E+03 0.29323230764215E+03 0.29323655002460E+03 0.29839405367956E+03 0.29839405367959E+03
 0.29323232744092E+03 0.29323657084404E+03 0.29926067447080E+03 0.29324926962733E+03 0.72739200930038E+02
 0.11841673383226E+03 0.44172070842626E+02 0.14927979136674E+03 0.10488686016990E+03 0.46274179221017E+02
 0.27962335872706E+02 0.12808841383241E+03 0.27962335872706E+02 0.47602395293839E+02 0.29135055312789E+02
 0.13047637542975E+03 0.29135055312789E+02 0.47587916758450E+02 0.29137975064617E+02 0.13046511735069E+03
 0.29137975064617E+02 0.47602395293830E+02 0.29135055312782E+02 0.13047637542973E+03 0.29135055312782E+02
 0.97892989045125E+01 0.00000000000000E+00 0.81209160601946E+03 0.32894181743125E+01 0.32089291483663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81209160601946E+03 0.32894181743125E+01 0.32089291483663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.79226934066962E+04 0.55730758498970E+03 0.77841240902918E+04
 0.13856932111383E+03 0.30464999304056E+03 0.79234448201306E+04 0.55805899842403E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33033832631756E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73272239774259E+00 0.00000000000000E+00 0.28557607234464E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10090621222836E+01 0.10182984700872E+01 0.10090621222836E+01
 0.21610593286225E+01 0.91105932862247E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.92795195300055E+00 0.94451208967580E+00 0.94451208967580E+00
 0.92795195300055E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16544134778560E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16419337198681E+01 0.16544134778560E+01 0.16419337198681E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    303.90390765
 0.18239467688053E+01 0.31709484037373E+03 0.36388774023747E+03 0.32974863683367E+03 0.32851871793361E+03
 0.22318129230951E+00 0.00000000000000E+00 0.21296693101561E+00 0.00000000000000E+00 -.36303708965931E+01
 0.12995884956106E+00 0.29033692779200E+00 0.61557947204215E+02 0.23084230201581E+02 0.27554193883773E+02
 0.10332822706415E+02 0.29548988255543E+03 0.29425454786152E+03 0.30275443659318E+03 0.31841970964040E+03
 0.29342068280804E+03 0.29396616074902E+03 0.30743536661749E+03 0.31845728816277E+03 0.29362889283060E+03
 0.29396366041690E+03 0.30278719795124E+03 0.31845076702633E+03 0.29404280377765E+03 0.29460162244257E+03
 0.29878249627924E+03 0.31845728816277E+03 0.29324808275414E+03 0.29396366041690E+03 0.30187799004304E+03
 0.29766930753277E+03 0.21706252690301E+03 0.25086292103038E+03 0.19455586898606E+03 0.89191520951352E+03
 0.69638656118253E+03 0.15559452574924E+03 0.64198601721666E+02 0.20349548189646E+03 0.49230585785068E+03
 0.24966788192322E+03 0.66436280676085E+02 0.27809842192825E+03 0.49413064322124E+03 0.15535989406168E+03
 0.63970667301642E+02 0.20311622318651E+03 0.49173667580308E+03 0.73583457591353E+02 0.66436280676077E+02
 0.13978604453500E+03 0.49413064322122E+03 0.61460935142210E+02 0.00000000000000E+00 0.38014371683871E+03
 0.52217675295135E+00 0.38776008078838E+03 0.46638226222503E-01 0.88472224718756E+00 0.36388774023747E+03
 0.14675297717737E+00 0.36388774023747E+03 0.00000000000000E+00 0.39765283033455E+01 0.36388774023747E+03
 0.85225129580328E-01 0.36388774023747E+03 0.00000000000000E+00 0.64086706033272E+01 0.38020582143726E+03
 0.52411141744153E+00 0.38837075069924E+03 0.45688571331239E-01 0.86266005359055E+00 0.36388774023747E+03
 0.14758139313779E+00 0.36388774023747E+03 0.00000000000000E+00 0.39544392141035E+01 0.36388774023747E+03
 0.85534447419681E-01 0.36388774023747E+03 0.00000000000000E+00 0.63865885726502E+01 0.18125373153315E+04
 0.92636983133618E+03 0.13049057525806E+04 0.50763156746032E+03 0.30464999304056E+03 0.14935770010113E+04
 0.85124916603450E+03 0.10610661035621E+04 0.43251090215863E+03 0.30464999304056E+03 0.14317043505932E+04
 0.78937651561637E+03 0.10610661035621E+04 0.37063825174050E+03 0.30464999304056E+03 0.18181597730423E+04
 0.93106072006879E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.14938287384194E+04 0.85150090344257E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14328462609738E+04 0.79051842599698E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18563623873859E-02 0.92818119369295E+05 0.92818119369295E+05 0.50000000000000E+08 0.39187840390739E+03
 0.91302844207469E+00 0.91302844207469E+00 0.50364623614624E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30785545145635E+03 0.30785545145635E+03 0.30785545145635E+03
 0.30785545145635E+03 0.22524113580698E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.55847685012953E+00 0.94428057217611E-01 0.10000000000000E-02 0.84720582374832E+02 0.31770218390562E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29766932097272E+03 0.30187820697465E+03 0.29526235758852E+03
 0.29526235758853E+03 0.29317422486624E+03 0.29317547332822E+03 0.29525846201414E+03 0.29525846201414E+03
 0.29317420523627E+03 0.29317545268654E+03 0.29528458512570E+03 0.29528458512548E+03 0.29378973757789E+03
 0.29371085510343E+03 0.29525846201414E+03 0.29525846201415E+03 0.29317420523627E+03 0.29317545268654E+03
 0.29561981110756E+03 0.29317926696245E+03 0.13328561552655E+01 0.14647503868525E+02 0.14896660230221E+02
 0.65217013621161E+02 0.50245870089789E+02 0.16223622508832E+02 0.10091345214844E+02 0.56664467176158E+02
 0.50540205666290E+15 0.16067790053516E+02 0.10162915039062E+02 0.56525200586985E+02 0.50555578647518E+15
 0.16069405496942E+02 0.10019775390625E+02 0.56415760417638E+02 0.50452486746939E+15 0.16067790053503E+02
 0.10162915039062E+02 0.56525200586961E+02 0.50555578647508E+15 0.77669234159802E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32109827591572E+03
 0.32109827591572E+03 0.32109827591572E+03 0.32109827591572E+03 0.22192614033332E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.37075392443249E+01 0.15432629597288E+00 0.10000000000000E-02
 0.51838216873978E+02 0.19439331327742E+02 0.80000000000000E+04 0.30000000000000E+04 0.29405474396648E+03
 0.29349647447219E+03 0.29879417715788E+03 0.29879417715521E+03 0.30125281435197E+03 0.30028139074267E+03
 0.29855589074000E+03 0.29855589074003E+03 0.29324442451789E+03 0.29324929157401E+03 0.29855523436939E+03
 0.29855523436942E+03 0.29324440247425E+03 0.29324926839390E+03 0.29855589074000E+03 0.29855589074003E+03
 0.29324442451789E+03 0.29324929157401E+03 0.29943904501272E+03 0.29326372245348E+03 0.74245140185021E+02
 0.12027374766514E+03 0.44854447962873E+02 0.15009388259581E+03 0.10501516239313E+03 0.46918619757269E+02
 0.28313907673075E+02 0.12880157343874E+03 0.28313907673075E+02 0.48383748131046E+02 0.29610493311811E+02
 0.13143646605631E+03 0.29610493311811E+02 0.48369793383539E+02 0.29613411426733E+02 0.13142573940514E+03
 0.29613411426733E+02 0.48383748131033E+02 0.29610493311801E+02 0.13143646605628E+03 0.29610493311801E+02
 0.98641829992776E+01 0.00000000000000E+00 0.81511320523023E+03 0.32919537072451E+01 0.32109827591572E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81511320523023E+03 0.32919537072451E+01 0.32109827591572E+03
 0.00000000000000E+00 0.00000000000000E+00 0.79697710743173E+04 0.55906126444918E+03 0.78294480784535E+04
 0.14032300057332E+03 0.30464999304056E+03 0.79705299828062E+04 0.55982017293799E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33057666930525E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73433132535622E+00 0.00000000000000E+00 0.27720753029784E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10027118439751E+01 0.10115388556541E+01 0.10027118439751E+01
 0.21619733844027E+01 0.91197338440267E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.93220336076034E+00 0.94790895982054E+00 0.94790895982054E+00
 0.93220336076034E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16575864915354E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16457038763107E+01 0.16575864915354E+01 0.16457038763107E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    313.21175171
 0.18256830725702E+01 0.31738173590677E+03 0.36415504005648E+03 0.32999774820276E+03 0.32877191328830E+03
 0.22299518558139E+00 0.00000000000000E+00 0.21277740050297E+00 0.00000000000000E+00 -.36565261991691E+01
 0.13374098884784E+00 0.29359275910723E+00 0.59817114176578E+02 0.22431417816217E+02 0.27248628420969E+02
 0.10218235657863E+02 0.29555565625459E+03 0.29430896014465E+03 0.30295715175777E+03 0.31882454599688E+03
 0.29345008818242E+03 0.29405108014485E+03 0.30766896393802E+03 0.31886561240134E+03 0.29367741427805E+03
 0.29404848016513E+03 0.30299037001280E+03 0.31885466818988E+03 0.29408714565998E+03 0.29470356788195E+03
 0.29895867538318E+03 0.31886561240134E+03 0.29326139697809E+03 0.29404848016513E+03 0.30211121572600E+03
 0.29788913098860E+03 0.21805077145275E+03 0.25227492535337E+03 0.19563236529113E+03 0.89324429337095E+03
 0.69663376625336E+03 0.15562898011975E+03 0.63955774926047E+02 0.20387226620914E+03 0.49078606032649E+03
 0.24888240574198E+03 0.66272289709699E+02 0.27750239528358E+03 0.49265145496798E+03 0.15539085315263E+03
 0.63733965487826E+02 0.20348730389703E+03 0.49023334635013E+03 0.74316811866851E+02 0.66272289709687E+02
 0.14100888830342E+03 0.49265145496795E+03 0.61513010689531E+02 0.00000000000000E+00 0.38042301678703E+03
 0.52230031170709E+00 0.38804107647865E+03 0.46645741763022E-01 0.88457970114487E+00 0.36415504005648E+03
 0.14676314832734E+00 0.36415504005648E+03 0.00000000000000E+00 0.39758876069291E+01 0.36415504005648E+03
 0.85230391582432E-01 0.36415504005648E+03 0.00000000000000E+00 0.64076380412589E+01 0.38048534715280E+03
 0.52423919498362E+00 0.38865219089298E+03 0.45695933839141E-01 0.86252106219838E+00 0.36415504005648E+03
 0.14759163690020E+00 0.36415504005648E+03 0.00000000000000E+00 0.39538020766710E+01 0.36415504005648E+03
 0.85539731779906E-01 0.36415504005648E+03 0.00000000000000E+00 0.63855595684286E+01 0.18198296197869E+04
 0.92947263656676E+03 0.13090952518055E+04 0.51073437269089E+03 0.30464999304056E+03 0.15015353731090E+04
 0.85519804084706E+03 0.10650756008472E+04 0.43645977697120E+03 0.30464999304056E+03 0.14390174500979E+04
 0.79268011783602E+03 0.10650756008472E+04 0.37394185396016E+03 0.30464999304056E+03 0.18254246577160E+04
 0.93413271900926E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.15017904406287E+04 0.85545310836685E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14401471144395E+04 0.79380978217757E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18586052413761E-02 0.92930262068807E+05 0.92930262068807E+05 0.50000000000000E+08 0.39214516683552E+03
 0.91369141664271E+00 0.91369141664271E+00 0.52094062486728E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30797697411026E+03 0.30797697411026E+03 0.30797697411026E+03
 0.30797697411026E+03 0.22513053015124E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.56296912263711E+00 0.96801215303923E-01 0.10000000000000E-02 0.82643590526036E+02 0.30991346447263E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29788914885171E+03 0.30211140911120E+03 0.29534014006262E+03
 0.29534014006263E+03 0.29317837339816E+03 0.29317983567409E+03 0.29533594412879E+03 0.29533594412880E+03
 0.29317834937653E+03 0.29317981041439E+03 0.29536364736635E+03 0.29536364736613E+03 0.29380783473843E+03
 0.29372936953665E+03 0.29533594412880E+03 0.29533594412881E+03 0.29317834937653E+03 0.29317981041439E+03
 0.29570667701436E+03 0.29318423457968E+03 0.92577772563722E+00 0.14078143194184E+02 0.15357471218136E+02
 0.65856958078841E+02 0.50422699504614E+02 0.16708103647650E+02 0.10449194335937E+02 0.57324855656901E+02
 0.50729156425512E+15 0.16543370602278E+02 0.10449194335937E+02 0.57177981747084E+02 0.50745718192286E+15
 0.16545038005809E+02 0.10306054687500E+02 0.57061772327941E+02 0.50636367294493E+15 0.16543370602264E+02
 0.10449194335937E+02 0.57177981747058E+02 0.50745718192275E+15 0.78305115135731E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32129237590387E+03
 0.32129237590387E+03 0.32129237590387E+03 0.32129237590387E+03 0.22177116544542E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.37309957321245E+01 0.15745639177309E+00 0.10000000000000E-02
 0.50807718314342E+02 0.19052894367878E+02 0.80000000000000E+04 0.30000000000000E+04 0.29409097145651E+03
 0.29352100191652E+03 0.29897666064862E+03 0.29897666064595E+03 0.30143179253201E+03 0.30045942723120E+03
 0.29871465137744E+03 0.29871465137748E+03 0.29325751698418E+03 0.29326305904860E+03 0.29871399517866E+03
 0.29871399517869E+03 0.29325749258654E+03 0.29326303339306E+03 0.29871465137745E+03 0.29871465137748E+03
 0.29325751698418E+03 0.29326305904860E+03 0.29961357748163E+03 0.29327933636021E+03 0.75696032526077E+02
 0.12205245662467E+03 0.45502913662762E+02 0.15082804250544E+03 0.10509761427436E+03 0.47516599099662E+02
 0.28635109990256E+02 0.12942357241987E+03 0.28635109990256E+02 0.49126939579089E+02 0.30063172562067E+02
 0.13232007021155E+03 0.30063172562067E+02 0.49113550500630E+02 0.30066080513259E+02 0.13230990880479E+03
 0.30066080513259E+02 0.49126939579074E+02 0.30063172562055E+02 0.13232007021152E+03 0.30063172562055E+02
 0.99331082989616E+01 0.00000000000000E+00 0.81803196483872E+03 0.32944658071335E+01 0.32129237590387E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81803196483872E+03 0.32944658071335E+01 0.32129237590387E+03
 0.00000000000000E+00 0.00000000000000E+00 0.80152525692678E+04 0.56076136527239E+03 0.78732294725808E+04
 0.14202310139653E+03 0.30464999304056E+03 0.80160186177570E+04 0.56152741376159E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33080334800066E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73587859147857E+00 0.00000000000000E+00 0.26931363537033E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99672723735271E+00 0.10051922268489E+01 0.99672723735271E+00
 0.21628415362851E+01 0.91284153628512E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.93615477531785E+00 0.95110829338696E+00 0.95110829338696E+00
 0.93615477531785E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16606320683332E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16492767667451E+01 0.16606320683332E+01 0.16492767667451E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    324.14573906
 0.18276133248575E+01 0.31769897462863E+03 0.36445880198731E+03 0.33027524856780E+03 0.32905352910818E+03
 0.22278385058549E+00 0.00000000000000E+00 0.21256139939152E+00 0.00000000000000E+00 -.36854531880241E+01
 0.13803405152324E+00 0.29730070640970E+00 0.57956713663895E+02 0.21733767623961E+02 0.26908782345695E+02
 0.10090793379636E+02 0.29563274536984E+03 0.29437331464087E+03 0.30318979562331E+03 0.31928784019714E+03
 0.29348660763299E+03 0.29415574249747E+03 0.30793663548677E+03 0.31933291248836E+03 0.29373708312073E+03
 0.29415304692219E+03 0.30322376773447E+03 0.31931723523964E+03 0.29414059939158E+03 0.29482780988031E+03
 0.29916124542052E+03 0.31933291248836E+03 0.29327840542350E+03 0.29415304692219E+03 0.30238445048722E+03
 0.29814806187514E+03 0.21917368212772E+03 0.25385884907854E+03 0.19682613731189E+03 0.89461137476691E+03
 0.69680110676846E+03 0.15566739776160E+03 0.63674018541827E+02 0.20425196411648E+03 0.48901024803593E+03
 0.24799620371323E+03 0.66078826740013E+02 0.27678939486658E+03 0.49092006571575E+03 0.15542360127069E+03
 0.63456734496181E+02 0.20385781467519E+03 0.48847013201658E+03 0.75140781709712E+02 0.66078826740008E+02
 0.14233737413466E+03 0.49092006571574E+03 0.61563207142699E+02 0.00000000000000E+00 0.38073997175052E+03
 0.52243607355252E+00 0.38835961188251E+03 0.46654540825027E-01 0.88441286911735E+00 0.36445880198731E+03
 0.14677256169949E+00 0.36445880198731E+03 0.00000000000000E+00 0.39751377531965E+01 0.36445880198731E+03
 0.85235103604105E-01 0.36445880198731E+03 0.00000000000000E+00 0.64064295585811E+01 0.38080255552236E+03
 0.52437986337429E+00 0.38897120271637E+03 0.45704553733263E-01 0.86235839043754E+00 0.36445880198731E+03
 0.14760112119759E+00 0.36445880198731E+03 0.00000000000000E+00 0.39530563882769E+01 0.36445880198731E+03
 0.85544464719310E-01 0.36445880198731E+03 0.00000000000000E+00 0.63843552497581E+01 0.18281266937437E+04
 0.93301538607110E+03 0.13138495762579E+04 0.51427712219523E+03 0.30464999304056E+03 0.15105870575992E+04
 0.85969329637498E+03 0.10696320298096E+04 0.44095503249911E+03 0.30464999304056E+03 0.14473356856725E+04
 0.79644192444822E+03 0.10696320298096E+04 0.37770366057236E+03 0.30464999304056E+03 0.18336908548327E+04
 0.93764079058260E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.15108458933244E+04 0.85995213210013E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14484513729901E+04 0.79755761176583E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18612399371223E-02 0.93061996856117E+05 0.93061996856117E+05 0.50000000000000E+08 0.39244721961356E+03
 0.91446959505476E+00 0.91446959505476E+00 0.54128419685652E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30811108423891E+03 0.30811108423891E+03 0.30811108423891E+03
 0.30811108423891E+03 0.22500566279493E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.56791201145614E+00 0.99478811004289E-01 0.10000000000000E-02 0.80419135685639E+02 0.30157175882115E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29814808433130E+03 0.30238461920049E+03 0.29542966842252E+03
 0.29542966842256E+03 0.29318380500287E+03 0.29318575121947E+03 0.29542510841808E+03 0.29542510841812E+03
 0.29318377491324E+03 0.29318571939740E+03 0.29545483576734E+03 0.29545483576639E+03 0.29382895288024E+03
 0.29374426115470E+03 0.29542510841808E+03 0.29542510841812E+03 0.29318377491324E+03 0.29318571939740E+03
 0.29580644062361E+03 0.29319072544903E+03 0.40615822114957E+00 0.13353368311970E+02 0.15876605996236E+02
 0.66552433996762E+02 0.50596444970545E+02 0.17253756307404E+02 0.10735473632813E+02 0.58049065809950E+02
 0.50923220956753E+15 0.17078258937018E+02 0.10807043457031E+02 0.57892994420691E+02 0.50941224106398E+15
 0.17079208263536E+02 0.10663903808594E+02 0.57767344535858E+02 0.50823854901331E+15 0.17078258937009E+02
 0.10807043457031E+02 0.57892994420675E+02 0.50941224106391E+15 0.78968477606056E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32150773375326E+03
 0.32150773375326E+03 0.32150773375326E+03 0.32150773375326E+03 0.22159595298103E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.37569779572564E+01 0.16099377213600E+00 0.10000000000000E-02
 0.49691363174236E+02 0.18634261190339E+02 0.80000000000000E+04 0.30000000000000E+04 0.29413348390599E+03
 0.29355049624811E+03 0.29918856663099E+03 0.29918856661913E+03 0.30163926985162E+03 0.30058071965008E+03
 0.29889684812898E+03 0.29889684812914E+03 0.29327422604171E+03 0.29328137939551E+03 0.29889619416005E+03
 0.29889619416021E+03 0.29327419874216E+03 0.29328135052352E+03 0.29889684812898E+03 0.29889684812914E+03
 0.29327422604171E+03 0.29328137939551E+03 0.29981342921248E+03 0.29329922253491E+03 0.77319264180338E+02
 0.12403488814058E+03 0.46210210469866E+02 0.15158984788949E+03 0.10514858636728E+03 0.48150006637737E+02
 0.28967123516671E+02 0.13003741271705E+03 0.28967123516671E+02 0.49942441637630E+02 0.30560202159275E+02
 0.13326159246085E+03 0.30560202159275E+02 0.49929795581363E+02 0.30563087181940E+02 0.13325216327330E+03
 0.30563087181940E+02 0.49942441637621E+02 0.30560202159268E+02 0.13326159246083E+03 0.30560202159268E+02
 0.10006469510910E+02 0.00000000000000E+00 0.82133846754726E+03 0.32973883438596E+01 0.32150773375326E+03
 0.00000000000000E+00 0.00000000000000E+00 0.82133846754726E+03 0.32973883438596E+01 0.32150773375326E+03
 0.00000000000000E+00 0.00000000000000E+00 0.80667810731183E+04 0.56269232849476E+03 0.79228270132088E+04
 0.14395406461889E+03 0.30464999304056E+03 0.80675551139276E+04 0.56346636930408E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33105647948617E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73762704217244E+00 0.00000000000000E+00 0.26058401150337E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99012171618617E+00 0.99821105367581E+00 0.99012171618617E+00
 0.21638066624288E+01 0.91380666242877E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.94046648600062E+00 0.95464671440239E+00 0.95464671440239E+00
 0.94046648600062E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16640587785548E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16532475788408E+01 0.16640587785548E+01 0.16532475788408E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    335.07972640
 0.18294336980277E+01 0.31799734424411E+03 0.36475302062017E+03 0.33053845664559E+03 0.32932018809033E+03
 0.22258012801677E+00 0.00000000000000E+00 0.21235235575381E+00 0.00000000000000E+00 -.37126920559930E+01
 0.14217036552635E+00 0.30088547017936E+00 0.56270517209281E+02 0.21101443953480E+02 0.26588189835923E+02
 0.99705711884713E+01 0.29570984638691E+03 0.29443808068094E+03 0.30341735077074E+03 0.31973836506643E+03
 0.29352512032439E+03 0.29426523827820E+03 0.30819745344511E+03 0.31978736792872E+03 0.29379938513323E+03
 0.29426247119393E+03 0.30345229888156E+03 0.31976739480334E+03 0.29419551486650E+03 0.29495654655553E+03
 0.29936026984832E+03 0.31978736792872E+03 0.29329684326686E+03 0.29426247119393E+03 0.30265709507872E+03
 0.29840752106328E+03 0.22026863489334E+03 0.25537924861657E+03 0.19795636875703E+03 0.89579865275026E+03
 0.69685250214945E+03 0.15571266094123E+03 0.63407185008754E+02 0.20457966280797E+03 0.48726817667454E+03
 0.24715979607986E+03 0.65895518799478E+02 0.27608067308762E+03 0.48921850949675E+03 0.15546158826934E+03
 0.63191800651266E+02 0.20417377073901E+03 0.48673406146081E+03 0.75933113194172E+02 0.65895518799477E+02
 0.14356372592059E+03 0.48921850949675E+03 0.61604563614444E+02 0.00000000000000E+00 0.38104654284713E+03
 0.52256292108806E+00 0.38866738970058E+03 0.46663295239568E-01 0.88424694605422E+00 0.36475302062017E+03
 0.14677962336122E+00 0.36475302062017E+03 0.00000000000000E+00 0.39743919849525E+01 0.36475302062017E+03
 0.85238453981320E-01 0.36475302062017E+03 0.00000000000000E+00 0.64052276601768E+01 0.38110936863098E+03
 0.52451156213592E+00 0.38927941441863E+03 0.45713129889039E-01 0.86219660497438E+00 0.36475302062017E+03
 0.14760824041295E+00 0.36475302062017E+03 0.00000000000000E+00 0.39523147626769E+01 0.36475302062017E+03
 0.85547831051622E-01 0.36475302062017E+03 0.00000000000000E+00 0.63831574926741E+01 0.18361729348723E+04
 0.93646306075068E+03 0.13184481427070E+04 0.51772479687482E+03 0.30464999304056E+03 0.15193573239307E+04
 0.86405028321347E+03 0.10740453093025E+04 0.44531201933760E+03 0.30464999304056E+03 0.14553974590635E+04
 0.80009041834625E+03 0.10740453093025E+04 0.38135215447039E+03 0.30464999304056E+03 0.18417076329918E+04
 0.94105537211231E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.15196197745893E+04 0.86431273387209E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14564997479314E+04 0.80119270721421E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18638746328685E-02 0.93193731643427E+05 0.93193731643427E+05 0.50000000000000E+08 0.39273874039412E+03
 0.91524710396467E+00 0.91524710396467E+00 0.56165657657570E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30823714238752E+03 0.30823714238752E+03 0.30823714238752E+03
 0.30823714238752E+03 0.22488599923128E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.57254209519376E+00 0.10204322838156E+00 0.10000000000000E-02 0.78398146813682E+02 0.29399305055131E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29840754781918E+03 0.30265724241853E+03 0.29551783929025E+03
 0.29551783929030E+03 0.29318982907999E+03 0.29319212214488E+03 0.29551290029549E+03 0.29551290029554E+03
 0.29318979188633E+03 0.29319208280970E+03 0.29554482528335E+03 0.29554482528241E+03 0.29385004318031E+03
 0.29376607551581E+03 0.29551290029549E+03 0.29551290029555E+03 0.29318979188633E+03 0.29319208280970E+03
 0.29590442586367E+03 0.29319790932667E+03 -.14295669591723E+00 0.12586003429464E+02 0.16384819595236E+02
 0.67203340729550E+02 0.50736597036338E+02 0.17784788349513E+02 0.11093322753906E+02 0.58730917842460E+02
 0.51089863309633E+15 0.17598512147533E+02 0.11093322753906E+02 0.58565697462631E+02 0.51109367383349E+15
 0.17597655522164E+02 0.10950183105469E+02 0.58428786745249E+02 0.50983291122349E+15 0.17598512147526E+02
 0.11093322753906E+02 0.58565697462618E+02 0.51109367383343E+15 0.79564557793806E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32171130847014E+03
 0.32171130847014E+03 0.32171130847014E+03 0.32171130847014E+03 0.22142778017957E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.37814944046343E+01 0.16438719309822E+00 0.10000000000000E-02
 0.48665591578171E+02 0.18249596841814E+02 0.80000000000000E+04 0.30000000000000E+04 0.29417619391144E+03
 0.29358065268306E+03 0.29939853789596E+03 0.29939853788409E+03 0.30184440112288E+03 0.30078512494929E+03
 0.29907543971592E+03 0.29907543971611E+03 0.29329231549317E+03 0.29330051083533E+03 0.29907479026279E+03
 0.29907479026298E+03 0.29329228515964E+03 0.29330047875445E+03 0.29907543971592E+03 0.29907543971611E+03
 0.29329231549317E+03 0.29330051083533E+03 0.30000874669374E+03 0.29332070586836E+03 0.78891730654655E+02
 0.12594190386701E+03 0.46893420836493E+02 0.15228289604075E+03 0.10515500810008E+03 0.48741643776448E+02
 0.29271168374101E+02 0.13056329820974E+03 0.29271168374101E+02 0.50726776616876E+02 0.31038995974275E+02
 0.13413372927033E+03 0.31038995974275E+02 0.50714902330541E+02 0.31041845345526E+02 0.13412504948267E+03
 0.31041845345526E+02 0.50726776616870E+02 0.31038995974270E+02 0.13413372927032E+03 0.31038995974270E+02
 0.10074351166597E+02 0.00000000000000E+00 0.82452108607225E+03 0.33002754280914E+01 0.32171130847014E+03
 0.00000000000000E+00 0.00000000000000E+00 0.82452108607225E+03 0.33002754280914E+01 0.32171130847014E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81163905038503E+04 0.56456248135189E+03 0.79705662910838E+04
 0.14582421747602E+03 0.30464999304056E+03 0.81171721199457E+04 0.56534409744730E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33129726289098E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.73930168920716E+00 0.00000000000000E+00 0.25240204971824E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98393478614950E+00 0.99170373892540E+00 0.98393478614950E+00
 0.21647168490139E+01 0.91471684901386E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.94446813595767E+00 0.95798475145216E+00 0.95798475145216E+00
 0.94446813595767E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16673478039782E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16570052503611E+01 0.16673478039782E+01 0.16570052503611E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    340.54672008
 0.18303049962183E+01 0.31814019980186E+03 0.36489698740952E+03 0.33066531462335E+03 0.32944854431467E+03
 0.22248103694490E+00 0.00000000000000E+00 0.21225037491060E+00 0.00000000000000E+00 -.37257523831729E+01
 0.14418141111812E+00 0.30263258789944E+00 0.55485654759238E+02 0.20807120534714E+02 0.26434694477312E+02
 0.99130104289919E+01 0.29574838832263E+03 0.29447060608128E+03 0.30352928465922E+03 0.31995920776240E+03
 0.29354510388195E+03 0.29432171450174E+03 0.30832547167049E+03 0.32001014472097E+03 0.29383147878880E+03
 0.29431892118109E+03 0.30356480438513E+03 0.31998818107882E+03 0.29422350522449E+03 0.29502251973977E+03
 0.29945841543284E+03 0.32001014472097E+03 0.29330659892799E+03 0.29431892118109E+03 0.30279318926850E+03
 0.29853741847786E+03 0.22080483336685E+03 0.25611623234532E+03 0.19849777693458E+03 0.89633080811112E+03
 0.69684054229186E+03 0.15573630737389E+03 0.63277156739097E+02 0.20472536080044E+03 0.48640862184826E+03
 0.24675756011775E+03 0.65805669080821E+02 0.27572735099308E+03 0.48837798286632E+03 0.15548099023077E+03
 0.63061797672363E+02 0.20431263168318E+03 0.48587519776319E+03 0.76316785255113E+02 0.65805669080809E+02
 0.14414157072828E+03 0.48837798286629E+03 0.61622153891730E+02 0.00000000000000E+00 0.38119641717967E+03
 0.52262344008878E+00 0.38881775261315E+03 0.46667649219076E-01 0.88416444793942E+00 0.36489698740952E+03
 0.14678241688542E+00 0.36489698740952E+03 0.00000000000000E+00 0.39740211837327E+01 0.36489698740952E+03
 0.85239703082372E-01 0.36489698740952E+03 0.00000000000000E+00 0.64046300678309E+01 0.38125936009767E+03
 0.52457448433189E+00 0.38942998065837E+03 0.45717395212132E-01 0.86211616410340E+00 0.36489698740952E+03
 0.14761105850252E+00 0.36489698740952E+03 0.00000000000000E+00 0.39519460212088E+01 0.36489698740952E+03
 0.85549086573180E-01 0.36489698740952E+03 0.00000000000000E+00 0.63825619594218E+01 0.18401117822998E+04
 0.93815379319007E+03 0.13206962576950E+04 0.51941552931420E+03 0.30464999304056E+03 0.15236467533550E+04
 0.86618021079758E+03 0.10762048111428E+04 0.44744194692171E+03 0.30464999304056E+03 0.14593415120362E+04
 0.80187496947874E+03 0.10762048111428E+04 0.38313670560287E+03 0.30464999304056E+03 0.18456322617164E+04
 0.94273012883659E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.15239109590297E+04 0.86644441647221E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14604372749379E+04 0.80297073238048E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18651919807416E-02 0.93259599037082E+05 0.93259599037082E+05 0.50000000000000E+08 0.39288105454972E+03
 0.91563560821949E+00 0.91563560821949E+00 0.57185356933403E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30829753555565E+03 0.30829753555565E+03 0.30829753555565E+03
 0.30829753555565E+03 0.22482803654230E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.57475451864047E+00 0.10328479994964E+00 0.10000000000000E-02 0.77455734085761E+02 0.29045900282160E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29853744727809E+03 0.30279332698443E+03 0.29556139587103E+03
 0.29556139587108E+03 0.29319306626325E+03 0.29319554571936E+03 0.29555626252043E+03 0.29555626252049E+03
 0.29319302509968E+03 0.29319550218566E+03 0.29558935295212E+03 0.29558935295118E+03 0.29386058593836E+03
 0.29377701339993E+03 0.29555626252043E+03 0.29555626252049E+03 0.29319302509968E+03 0.29319550218566E+03
 0.29595274274662E+03 0.29320176380867E+03 -.42975062935220E+00 0.12185884365545E+02 0.16632928021793E+02
 0.67512273916998E+02 0.50796181255096E+02 0.18043477872504E+02 0.11236462402344E+02 0.59056285630955E+02
 0.51164537340200E+15 0.17851757539736E+02 0.11308032226562E+02 0.58886456556094E+02 0.51184811311141E+15
 0.17849623345264E+02 0.11093322753906E+02 0.58743259024166E+02 0.51054116992580E+15 0.17851757539738E+02
 0.11308032226562E+02 0.58886456556098E+02 0.51184811311143E+15 0.79837577169440E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32180921519808E+03
 0.32180921519808E+03 0.32180921519808E+03 0.32180921519808E+03 0.22134622949605E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.37932695488482E+01 0.16603196765465E+00 0.10000000000000E-02
 0.48183492088947E+02 0.18068809533355E+02 0.80000000000000E+04 0.30000000000000E+04 0.29419760465520E+03
 0.29359596544757E+03 0.29950280164417E+03 0.29950280163230E+03 0.30194615990198E+03 0.30088659073716E+03
 0.29916338571729E+03 0.29916338571749E+03 0.29330187589160E+03 0.29331062196673E+03 0.29916273931694E+03
 0.29916273931714E+03 0.29330184399489E+03 0.29331058823257E+03 0.29916338571729E+03 0.29916338571749E+03
 0.29330187589160E+03 0.29331062196673E+03 0.30010473826363E+03 0.29333204204474E+03 0.79654837226786E+02
 0.12686417885804E+03 0.47221187896150E+02 0.15260198584747E+03 0.10514469201183E+03 0.49017846575195E+02
 0.29410184326811E+02 0.13079185047889E+03 0.29410184326811E+02 0.51103318256175E+02 0.31269056998867E+02
 0.13454238523321E+03 0.31269056998867E+02 0.51091848395247E+02 0.31271884142517E+02 0.13453409468007E+03
 0.31271884142517E+02 0.51103318256177E+02 0.31269056998869E+02 0.13454238523321E+03 0.31269056998869E+02
 0.10106117723168E+02 0.00000000000000E+00 0.82606860723560E+03 0.33017057542067E+01 0.32180921519808E+03
 0.00000000000000E+00 0.00000000000000E+00 0.82606860723560E+03 0.33017057542067E+01 0.32180921519808E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81405166825739E+04 0.56547584262518E+03 0.79937791085341E+04
 0.14673757874931E+03 0.30464999304056E+03 0.81413019426263E+04 0.56626110267761E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33141356592388E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.74011311597461E+00 0.00000000000000E+00 0.24850053612301E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98098740324159E+00 0.98861365209762E+00 0.98098740324159E+00
 0.21651524981092E+01 0.91515249810917E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.94636715376554E+00 0.95958878651912E+00 0.95958878651912E+00
 0.94636715376554E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16689462233977E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16588124439617E+01 0.16689462233977E+01 0.16588124439617E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
    351.48070742
 0.18319748724088E+01 0.31841459770491E+03 0.36517932824436E+03 0.33091060373907E+03 0.32969642047257E+03
 0.22228823115083E+00 0.00000000000000E+00 0.21205136456886E+00 0.00000000000000E+00 -.37508819298552E+01
 0.14809251440240E+00 0.30603819245815E+00 0.54020286118327E+02 0.20257607294373E+02 0.26140528199251E+02
 0.98026980747192E+01 0.29582545084144E+03 0.29453592006845E+03 0.30374960953516E+03 0.32039254319123E+03
 0.29358648277472E+03 0.29443795406362E+03 0.30857697367034E+03 0.32044728816140E+03 0.29389746170148E+03
 0.29443512811817E+03 0.30378643603269E+03 0.32042164476773E+03 0.29428052273361E+03 0.29515751756691E+03
 0.29965202404442E+03 0.32044728816140E+03 0.29332718140028E+03 0.29443512811817E+03 0.30306493946554E+03
 0.29879748358400E+03 0.22185637300745E+03 0.25754796552473E+03 0.19953572279754E+03 0.89728363125898E+03
 0.69675022984746E+03 0.15578612394785E+03 0.63023521044501E+02 0.20498607345800E+03 0.48471510176556E+03
 0.24598408452708E+03 0.65629424115739E+02 0.27502601623277E+03 0.48672023781382E+03 0.15552112830213E+03
 0.62806430244522E+02 0.20455777497546E+03 0.48417863418174E+03 0.77060963284690E+02 0.65629424115737E+02
 0.14523436575538E+03 0.48672023781381E+03 0.61652017163260E+02 0.00000000000000E+00 0.38149012718248E+03
 0.52273958637542E+00 0.38911226016207E+03 0.46676297638803E-01 0.88400062549333E+00 0.36517932824436E+03
 0.14678683029814E+00 0.36517932824436E+03 0.00000000000000E+00 0.39732848570543E+01 0.36517932824436E+03
 0.85241525027680E-01 0.36517932824436E+03 0.00000000000000E+00 0.64034433856855E+01 0.38155329715728E+03
 0.52469538895817E+00 0.38972487184572E+03 0.45725867531377E-01 0.86195642687450E+00 0.36517932824436E+03
 0.14761551429549E+00 0.36517932824436E+03 0.00000000000000E+00 0.39512137847278E+01 0.36517932824436E+03
 0.85550918878185E-01 0.36517932824436E+03 0.00000000000000E+00 0.63813793661668E+01 0.18478381155469E+04
 0.94147447639506E+03 0.13251019077372E+04 0.52273621251920E+03 0.30464999304056E+03 0.15320512223641E+04
 0.87034956728401E+03 0.10804399236654E+04 0.45161130340814E+03 0.30464999304056E+03 0.14670721033470E+04
 0.80537044826692E+03 0.10804399236654E+04 0.38663218439106E+03 0.30464999304056E+03 0.18533311526662E+04
 0.94601996389236E+03 0.66927825898743E-10 0.19282602901590E+02 0.19282602901657E+02 0.27314999389648E+03
 0.27314999389648E+03 0.15323188392977E+04 0.87061718421756E+03 0.66927825898743E-10 0.24259497008641E+01
 0.24259497009310E+01 0.27314999389648E+03 0.27314999389648E+03 0.14681551374045E+04 0.80645348232438E+03
 0.66927825898743E-10 0.13303179541041E+01 0.13303179541711E+01 0.27314999389648E+03 0.27314999389648E+03
 0.18678266764878E-02 0.93391333824392E+05 0.93391333824392E+05 0.50000000000000E+08 0.39315962195370E+03
 0.91641211794109E+00 0.91641211794109E+00 0.59226916064814E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.30841371086626E+03 0.30841371086626E+03 0.30841371086626E+03
 0.30841371086626E+03 0.22471569656901E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.57900017923774E+00 0.10569001237045E+00 0.10000000000000E-02 0.75693055763486E+02 0.28384895911307E+02
 0.80000000000000E+04 0.30000000000000E+04 0.29879751622241E+03 0.30306505971454E+03 0.29564746951056E+03
 0.29564746951063E+03 0.29319999483375E+03 0.29320287323383E+03 0.29564193763729E+03 0.29564193763736E+03
 0.29319994483576E+03 0.29320282035696E+03 0.29567749068683E+03 0.29567749068590E+03 0.29388168568580E+03
 0.29379896782434E+03 0.29564193763729E+03 0.29564193763736E+03 0.29319994483576E+03 0.29320282035696E+03
 0.29604805997042E+03 0.29321000078705E+03 -.10264128138506E+01 0.11355418841474E+02 0.17117990156629E+02
 0.68101304436711E+02 0.50897724329299E+02 0.18548135405766E+02 0.11594311523437E+02 0.59679496983168E+02
 0.51299205013262E+15 0.18345423863560E+02 0.11594311523437E+02 0.59500389007238E+02 0.51321058015670E+15
 0.18339989726145E+02 0.11451171875000E+02 0.59343315174579E+02 0.51180604265931E+15 0.18345423863561E+02
 0.11594311523437E+02 0.59500389007240E+02 0.51321058015671E+15 0.80339059608734E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50170000000000E+01 0.32199816552253E+03
 0.32199816552253E+03 0.32199816552253E+03 0.32199816552253E+03 0.22118799700988E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.38159657386515E+01 0.16922165776139E+00 0.10000000000000E-02
 0.47275272597083E+02 0.17728227223906E+02 0.80000000000000E+04 0.30000000000000E+04 0.29424052456857E+03
 0.29362703480189E+03 0.29970992333201E+03 0.29970992332014E+03 0.30214820063952E+03 0.30108817767891E+03
 0.29933664534848E+03 0.29933664534870E+03 0.29332202051137E+03 0.29333192710994E+03 0.29933600667831E+03
 0.29933600667853E+03 0.29332198540311E+03 0.29333188997901E+03 0.29933664534848E+03 0.29933664534870E+03
 0.29332202051137E+03 0.29333192710994E+03 0.30029349820205E+03 0.29335589044975E+03 0.81137542587375E+02
 0.12865087309742E+03 0.47851139851853E+02 0.15319234592566E+03 0.10510195037455E+03 0.49533504327210E+02
 0.29663719044121E+02 0.13118675330809E+03 0.29663719044121E+02 0.51827480953433E+02 0.31711888236174E+02
 0.13531114206600E+03 0.31711888236174E+02 0.51816855354399E+02 0.31714661874473E+02 0.13530365725093E+03
 0.31714661874473E+02 0.51827480953433E+02 0.31711888236174E+02 0.13531114206600E+03 0.31711888236174E+02
 0.10165768194055E+02 0.00000000000000E+00 0.82908209521109E+03 0.33045417277624E+01 0.32199816552253E+03
 0.00000000000000E+00 0.00000000000000E+00 0.82908209521109E+03 0.33045417277624E+01 0.32199816552253E+03
 0.00000000000000E+00 0.00000000000000E+00 0.81875052512142E+04 0.56726209163328E+03 0.80389814281663E+04
 0.14852382775741E+03 0.30464999304056E+03 0.81882975309768E+04 0.56805437139584E+03 0.91248607330783E-10
 0.10821936990283E+00 0.10821936999408E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33163891608615E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.74168957839807E+00 0.00000000000000E+00 0.24104152426628E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97536111601680E+00 0.98273110266435E+00 0.97536111601680E+00
 0.21659874362044E+01 0.91598743620441E+00 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.94998822284244E+00 0.96268232824500E+00 0.96268232824500E+00
 0.94998822284244E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16720571349555E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16622976171639E+01 0.16720571349555E+01 0.16622976171639E+01
 0.24800000190735E+01 0.12400000095367E+01 0.12400000095367E+01 0.00000000000000E+00 0.64479998130798E+01
