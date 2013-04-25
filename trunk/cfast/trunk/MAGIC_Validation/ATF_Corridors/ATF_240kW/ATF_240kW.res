#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
Lower_Corridor 0 MONOZONE(1=OUI,0=NON)                                                              
0.000000 0.000000 0.000000                                                                          
17.000000 1.800000 2.500000                                                                         
#PAROI PAR_1                                                                                        
TRA_1                                                                                               
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
TRA_2                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#FOYER FOY_1                                                                                        
ATF_240kW                                                                                           
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
60.000000 0.004800                                                                                  
900.000000 0.004800                                                                                 
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
TRA_2                                                                                               
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
2.600000 3.200000 5.000000                                                                          
#PAROI PAR_13                                                                                       
TRA_1                                                                                               
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
0.000000 2.517000                                                                                   
2.600000 5.000000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#CONDINIT 900.000000 10.000000 20.000000 0.230000 0.001000 101325.000000                            
#OPTION                                                                                             
1                                                                                                   
#FINOPTION                                                                                          
#PARAMOPT                                                                                           
Limite-O2 0.00000000                                                                                
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
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #TEMPERATURE#K#Medium plume temperature
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #ATF_240kW   #MASS#kg#Consumed combustible mass
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.63452689742383E-06
 0.10000000000062E-02 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000000001E+03 0.29315000000000E+03 0.29315000000001E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000002E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000001E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.25407439830949E+02 0.25407439830949E+02 0.91409801293134E+01 0.91866850299706E+01
 0.10652452454605E-10 0.13940032821423E+02 0.50528818791207E+01 0.13940032821423E+02 0.50528818791207E+01
 0.31565322077529E+02 0.50521868650815E+01 0.31565322077529E+02 0.50521868650815E+01 0.13940032821423E+02
 0.50528818791208E+01 0.13940032821423E+02 0.50528818791208E+01 0.22368174063700E+01 0.50521868650815E+01
 0.22368174063700E+01 0.50521868650815E+01 0.23955926382426E+01 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29992461643969E+03 0.46339374925158E-01 0.95357068545242E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29470699994849E+03 0.17893811785030E+00 0.42859788278142E+01 0.29315000000000E+03
 0.00000000000000E+00 0.29402624704364E+03 0.27491246439131E+00 0.69073886629167E+01 0.29315000000000E+03
 0.00000000000000E+00 0.30012427189306E+03 0.45027174627534E-01 0.92979162808413E+00 0.29315000000000E+03
 0.00000000000000E+00 0.29471515965226E+03 0.17818627677887E+00 0.42621707818015E+01 0.29315000000000E+03
 0.00000000000000E+00 0.29402957843934E+03 0.27419571687010E+00 0.68835882247613E+01 0.18605152838529E+02
 0.43734342142379E+03 0.54569682106376E-10 0.18605157547924E+02 0.29315000000000E+03 0.24183507859340E+01
 0.42115661937120E+03 0.54569682106376E-10 0.24183554953296E+01 0.29315000000000E+03 0.13091865855106E+01
 0.42004745517077E+03 0.54569682106376E-10 0.13091912949063E+01 0.29315000000000E+03 0.19282598192624E+02
 0.43802086677788E+03 0.66927825898953E-10 0.19282602902019E+02 0.19282602902086E+02 0.27314999389648E+03
 0.27314999389648E+03 0.24259449920194E+01 0.42116421357728E+03 0.66927825898953E-10 0.24259497014150E+01
 0.24259497014819E+01 0.27314999389648E+03 0.27314999389648E+03 0.13303132679783E+01 0.42006858185324E+03
 0.66927825898953E-10 0.13303179773740E+01 0.13303179774409E+01 0.27314999389648E+03 0.27314999389648E+03
 0.23000000000676E-03 0.11500000000338E+05 0.11500000000338E+05 0.50000000000000E+08 0.30316835279350E+03
 0.13609036469638E+00 0.13609036469638E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 0.54358391995443E-06 0.99970725464972E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.43810821590223E-03 0.43810821590223E-03 0.64339417287356E-03
 0.64661114373792E-03 0.00000000000000E+00 0.41906397358801E-03 0.50769786849921E-03 0.41906397358801E-03
 0.50769786849921E-03 0.41256832820911E-03 0.50768570668432E-03 0.41256832820911E-03 0.50768570668432E-03
 0.41906397353051E-03 0.50769786861420E-03 0.41906397353051E-03 0.50769786861420E-03 0.41256832826661E-03
 0.50768570679931E-03 0.41256832826661E-03 0.50768570679931E-03 0.72525965111666E-04 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50000000000000E+01 0.29315000000009E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29315000000009E+03 0.23000000000000E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 0.13665083613273E-05 0.10000000000132E-02 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.36322944627289E-03
 0.36322944627289E-03 0.63467367683772E-03 0.63784704522191E-03 0.00000000000000E+00 0.37804419385466E-03
 0.50286332261326E-03 0.37804419385466E-03 0.50286332261326E-03 0.37760005268172E-03 0.50286804494426E-03
 0.37760005268172E-03 0.50286804494426E-03 0.37804419379717E-03 0.50286332261326E-03 0.37804419379717E-03
 0.50286332261326E-03 0.37760005262422E-03 0.50286804500176E-03 0.37760005262422E-03 0.50286804500176E-03
 0.30241307133606E-04 0.00000000000000E+00 0.29315000000009E+03 0.00000000000000E+00 0.29315000000009E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000009E+03 0.00000000000000E+00 0.29315000000009E+03
 0.00000000000000E+00 0.00000000000000E+00 0.10711764931824E+00 0.41884538623332E+03 0.13147882782505E-08
 0.10712235745365E+00 0.29315000000000E+03 0.10821467496021E+00 0.41884648325896E+03 0.91588579787306E-10
 0.10821938309562E+00 0.10821938318720E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.56722710625278E-04 0.00000000000000E+00 0.56722710625278E-04
 0.25000000000000E+01 0.12500000000000E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.21010557899050E+00 0.21010557899050E+00
 0.00000000000000E+00 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.40821498074970E-08 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.41973855421352E+00 0.40821498074970E-08 0.41973855421352E+00
 0.24830000400543E+01 0.12415000200272E+01 0.12415000200272E+01 0.00000000000000E+00 0.64557998673439E+01
     10.05285432
 0.22295994246553E+01 0.29315611006128E+03 0.31344721078965E+03 0.29535080018582E+03 0.29522319956894E+03
 0.22999983221706E+00 0.00000000000000E+00 0.22737469246571E+00 0.00000000000000E+00 -.25218999324148E-01
 0.99989593176777E-03 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29379939528926E+03 0.29315000000000E+03 0.29362164676580E+03 0.29506522523070E+03
 0.29315000000000E+03 0.29315000000001E+03 0.29416196026034E+03 0.29505822837185E+03 0.29315000000000E+03
 0.29315000000001E+03 0.29362621935956E+03 0.29509575034997E+03 0.29315269768177E+03 0.29315269765443E+03
 0.29323175973773E+03 0.29505822837185E+03 0.29315000000000E+03 0.29315000000001E+03 0.29345093621614E+03
 0.29315002703601E+03 0.88825330896977E+02 0.87809147567035E+02 0.45738616396336E+02 0.22728345919481E+03
 0.18131614971649E+03 0.68886119018689E+02 0.18426433045090E+02 0.68335968713863E+02 0.21942983264893E+03
 0.14637509625986E+03 0.17073619725966E+02 0.14486877244871E+03 0.21816256760776E+03 0.68853918366523E+02
 0.18256545464815E+02 0.68296700150839E+02 0.21888677904714E+03 0.11915097056656E+02 0.17073619725969E+02
 0.11863456101404E+02 0.21816256760777E+03 0.17676884670358E+02 0.00000000000000E+00 0.32205672837478E+03
 0.42129930626251E+00 0.32608542135760E+03 0.46656673909328E-01 0.85734527264282E+00 0.31429241854509E+03
 0.11784835255701E+00 0.31457566091243E+03 0.18095317206972E+00 0.38534780302420E+01 0.31344721078965E+03
 0.68383923416383E-01 0.31344721078965E+03 0.00000000000000E+00 0.62103597633652E+01 0.32207550540841E+03
 0.42291630086383E+00 0.32639738640928E+03 0.45673269366868E-01 0.83596577478951E+00 0.31433339955322E+03
 0.11853471936013E+00 0.31459697325500E+03 0.18010803496457E+00 0.38320724690065E+01 0.31344721078965E+03
 0.68768394555332E-01 0.31344721078965E+03 0.00000000000000E+00 0.61889610422734E+01 0.56790022271699E+03
 0.55303756568056E+03 0.43360092562175E+03 0.13429930180470E+03 0.30282904089164E+03 0.37831184176485E+03
 0.47991383217384E+03 0.31713627817632E+03 0.61175568297975E+02 0.29740319482852E+03 0.36367286486349E+03
 0.47795297160400E+03 0.30445816184481E+03 0.59214707728133E+02 0.29638926824995E+03 0.57162764628192E+03
 0.55648333374112E+03 0.66927825898953E-10 0.19282602902019E+02 0.19282602902086E+02 0.27314999389648E+03
 0.27314999389648E+03 0.37895400334706E+03 0.47994127863412E+03 0.66927825898953E-10 0.24259497014150E+01
 0.24259497014819E+01 0.27314999389648E+03 0.27314999389648E+03 0.36375954617989E+03 0.47803965292040E+03
 0.66927825898953E-10 0.13303179773740E+01 0.13303179774409E+01 0.27314999389648E+03 0.27314999389648E+03
 0.99569240433753E-03 0.49784620216876E+05 0.49784620216876E+05 0.50000000000000E+08 0.32864021318671E+03
 0.61040425067472E+00 0.61040425067472E+00 0.62194615452837E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29315108188180E+03 0.29315108188180E+03 0.29315108188180E+03
 0.29315108188180E+03 0.22999986285594E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.17993263489277E-02 0.99963450097128E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315002698286E+03 0.29345215157912E+03 0.29315010229934E+03
 0.29315010229934E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315010440096E+03 0.29315010440096E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315010296998E+03 0.29315010296998E+03 0.29315267917655E+03
 0.29315217166784E+03 0.29315010440096E+03 0.29315010440096E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315012901716E+03 0.29315000000000E+03 0.16610065570642E-01 0.16610065570642E-01 0.21630954753622E-01
 0.21739109527390E-01 0.00000000000000E+00 0.17234455665425E-01 0.12244275957346E-01 0.17424990088926E-01
 0.38434414920847E+11 0.17598555968514E-01 0.12235539406538E-01 0.17788571302606E-01 0.38351956818981E+11
 0.17229711534127E-01 0.12244275957346E-01 0.17420080278785E-01 0.38408101925283E+11 0.17598555968514E-01
 0.12235539406538E-01 0.17788571302606E-01 0.38351956818981E+11 0.28151388282897E-02 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50000000000000E+01 0.29340660432157E+03
 0.29340660432157E+03 0.29340660432157E+03 0.29340660432157E+03 0.22996729209518E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.34803076648199E-01 0.98411020528190E-03 0.10000000000000E-02
 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29316884937666E+03
 0.29315000000000E+03 0.29316849680375E+03 0.29316849680375E+03 0.29343999948961E+03 0.29339219554829E+03
 0.29316840221530E+03 0.29316840221530E+03 0.29315000000000E+03 0.29315000000000E+03 0.29316837948016E+03
 0.29316837948016E+03 0.29315000000000E+03 0.29315000000000E+03 0.29316840221530E+03 0.29316840221530E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29317081734276E+03 0.29315000000000E+03 0.30881629232972E+01
 0.32153526365082E+01 0.33245481870806E+01 0.35929210552876E+01 0.25175012727161E+00 0.29580158910082E+01
 0.18495979857025E+01 0.31738329130749E+01 0.18495979857025E+01 0.29625340025136E+01 0.18501213063150E+01
 0.31784635809214E+01 0.18501213063150E+01 0.29587144666039E+01 0.18502014133502E+01 0.31746711009846E+01
 0.18502014133502E+01 0.29625340025140E+01 0.18501213063154E+01 0.31784635809219E+01 0.18501213063154E+01
 0.24088622388801E+00 0.00000000000000E+00 0.11471034734668E+04 0.22594075780922E+01 0.29340660432157E+03
 0.00000000000000E+00 0.00000000000000E+00 0.11471034734668E+04 0.22594075780922E+01 0.29340660432157E+03
 0.00000000000000E+00 0.00000000000000E+00 0.12828133268343E+05 0.43756943492645E+03 0.12809302102002E+05
 0.18831171050579E+02 0.30464999304056E+03 0.12828229088706E+05 0.43766525528944E+03 0.91588579787306E-10
 0.10821938309562E+00 0.10821938318720E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29623110293029E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.19546650854301E+00 0.00000000000000E+00 0.17035855375674E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.26791509246286E+00 0.36582506229974E+00 0.26791509246286E+00
 0.23647997123276E+01 0.11147997123276E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.23940445467165E-01 0.13526468608388E+00 0.13526468608388E+00
 0.23940445467165E-01 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.26287682142176E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15176631004182E+00 0.26287682142176E+00 0.15176631004182E+00
 0.24830000400543E+01 0.12415000200272E+01 0.12415000200272E+01 0.00000000000000E+00 0.64557998673439E+01
     20.30340336
 0.20227830906797E+01 0.29324584239672E+03 0.32749310391088E+03 0.29978319131370E+03 0.29921878527241E+03
 0.22999077298563E+00 0.00000000000000E+00 0.22543031394835E+00 0.00000000000000E+00 -.22324741016596E+00
 0.10335687494624E-02 0.53622428377371E-01 0.77401721019151E+04 0.29025645382182E+04 0.14919130375259E+03
 0.55946738907223E+02 0.29456820006306E+03 0.29315000000177E+03 0.29436333764164E+03 0.29685950695051E+03
 0.29315000000109E+03 0.29315000001072E+03 0.29565804220786E+03 0.29683213843760E+03 0.29315000000240E+03
 0.29315000001071E+03 0.29438725315498E+03 0.29693882837738E+03 0.29316167054107E+03 0.29316167052518E+03
 0.29336959234243E+03 0.29683213843760E+03 0.29315000000019E+03 0.29315000001071E+03 0.29391701050033E+03
 0.29315092873198E+03 0.14491089809217E+03 0.14225656456989E+03 0.95115599116518E+02 0.44395551625326E+03
 0.34836433914116E+03 0.13349406182691E+03 0.45780483194891E+02 0.13175325688377E+03 0.35742043354898E+03
 0.27169807169665E+03 0.42106524149916E+02 0.26693141130787E+03 0.35406517464878E+03 0.13332691379894E+03
 0.45313656706148E+02 0.13153721855748E+03 0.35603020894080E+03 0.24826214894369E+02 0.42106524149919E+02
 0.24727395272849E+02 0.35406517464879E+03 0.34450153322944E+02 0.00000000000000E+00 0.34235001155970E+03
 0.52271124921832E+00 0.34938047795792E+03 0.46493330388422E-01 0.87844913710544E+00 0.32933618658805E+03
 0.14675827434942E+00 0.32979144504967E+03 0.18031244399876E+00 0.39483327878932E+01 0.32749310391088E+03
 0.85097653729806E-01 0.32749310391088E+03 0.00000000000000E+00 0.63632300186663E+01 0.34237381238588E+03
 0.52459378447305E+00 0.34989958637184E+03 0.45535793848243E-01 0.85654337516767E+00 0.32940338102574E+03
 0.14759308077358E+00 0.32982676695731E+03 0.17948951670903E+00 0.39264003210136E+01 0.32749310391088E+03
 0.85408486301229E-01 0.32749310391088E+03 0.00000000000000E+00 0.63413045603031E+01 0.10152444779081E+04
 0.69598257309786E+03 0.73800017339553E+03 0.27724430922200E+03 0.30464999304056E+03 0.69151514132294E+03
 0.56746061108754E+03 0.54279279882072E+03 0.14872234721167E+03 0.30390809535226E+03 0.65119370044364E+03
 0.55478541036574E+03 0.51514655866321E+03 0.13604714648987E+03 0.30149346159855E+03 0.10221429671793E+04
 0.70252404997644E+03 0.66927825898953E-10 0.19282602902019E+02 0.19282602902086E+02 0.27314999389648E+03
 0.27314999389648E+03 0.69257140945814E+03 0.56750896265740E+03 0.66927825898953E-10 0.24259497014150E+01
 0.24259497014819E+01 0.27314999389648E+03 0.27314999389648E+03 0.65218766909690E+03 0.55577937901901E+03
 0.66927825898953E-10 0.13303179773740E+01 0.13303179774409E+01 0.27314999389648E+03 0.27314999389648E+03
 0.17764425559896E-02 0.88822127799480E+05 0.88822127799480E+05 0.50000000000000E+08 0.35474028656164E+03
 0.88907059609761E+00 0.88907059609761E+00 0.20581375269213E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00
 .00000000000000E+00 0.25000000000000E+01 0.29321379406358E+03 0.29321379406358E+03 0.29321379406358E+03
 0.29321379406358E+03 0.22999169527486E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00
 -.21164570016982E-02 0.10299529701245E-02 0.10000000000000E-02 0.77673449488021E+04 0.29127543558008E+04
 0.80000000000000E+04 0.30000000000000E+04 0.29315092839528E+03 0.29392046637608E+03 0.29315056543223E+03
 0.29315056543223E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315057373853E+03 0.29315057373853E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315057231941E+03 0.29315057231941E+03 0.29316159026430E+03
 0.29315905678351E+03 0.29315057373853E+03 0.29315057373853E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315069811600E+03 0.29315000000000E+03 0.47697005242353E-01 0.71067772785295E-01 0.66086713413854E-01
 0.11337280132865E+00 0.46955654347723E-01 0.53264379060579E-01 0.37462329864502E-01 0.93685181287978E-01
 0.24810215725104E+13 0.54326068915016E-01 0.37182760238647E-01 0.94739975893353E-01 0.24806956706086E+13
 0.53215659607282E-01 0.37462329864502E-01 0.93630744610677E-01 0.24807513505317E+13 0.54326068915016E-01
 0.37182760238647E-01 0.94739975893353E-01 0.24806956706086E+13 0.14498893129155E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.50000000000000E+01 0.29514126817121E+03
 0.29514126817121E+03 0.29514126817121E+03 0.29514126817121E+03 0.22973881599490E+00 0.00000000000000E+00
 0.23000000000000E+00 0.00000000000000E+00 -.28536953227858E+00 0.27477651736819E-02 0.10000000000000E-02
 0.29114569456750E+04 0.10917963546281E+04 0.80000000000000E+04 0.30000000000000E+04 0.29322457991605E+03
 0.29315000000003E+03 0.29323125925667E+03 0.29323125925667E+03 0.29395097827660E+03 0.29380411507063E+03
 0.29323015793796E+03 0.29323015793796E+03 0.29315000000003E+03 0.29315000000003E+03 0.29323007992051E+03
 0.29323007992051E+03 0.29315000000003E+03 0.29315000000003E+03 0.29323015793796E+03 0.29323015793796E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29324286622814E+03 0.29315000000003E+03 0.79186979191548E+01
 0.96435033500406E+01 0.84443917974992E+01 0.12774606854035E+02 0.42879930975479E+01 0.75470965962396E+01
 0.47271870145152E+01 0.10878257681571E+02 0.47271870145152E+01 0.75636390934451E+01 0.47329494213595E+01
 0.10897334729099E+02 0.47329494213595E+01 0.75541198256210E+01 0.47332565297233E+01 0.10887995026727E+02
 0.47332565297233E+01 0.75636390934451E+01 0.47329494213595E+01 0.10897334729099E+02 0.47329494213595E+01
 0.81819467343693E+00 0.00000000000000E+00 0.84662320500180E+03 0.28443580173157E+01 0.29514126817121E+03
 0.00000000000000E+00 0.00000000000000E+00 0.84662320500180E+03 0.28443580173157E+01 0.29514126817121E+03
 0.00000000000000E+00 0.00000000000000E+00 0.83471106192518E+04 0.46375081281015E+03 0.83020980750269E+04
 0.45012548934288E+02 0.30464999304056E+03 0.83473569857795E+04 0.46399717933791E+03 0.91588579787306E-10
 0.10821938309562E+00 0.10821938318720E+00 0.27314999389648E+03 0.27314999389648E+03 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.30154863281342E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00
 0.00000000000000E+00 0.46771648586215E+00 0.00000000000000E+00 0.37379433653953E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.70303026035188E+00 0.84151082240168E+00 0.70303026035188E+00
 0.22613915453399E+01 0.10113915453399E+01 0.25000000000000E+01 0.12500000000000E+01 0.65000009536743E+01
 0.00000000000000E+00 0.00000000000000E+00 0.19243567640009E-01 0.20746168077288E+00 0.20746168077288E+00
 0.19243567640009E-01 0.20000000000000E+01 0.10000000000000E+01 0.10000000000000E+01 0.35999999046326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.77757334364520E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.59444506456653E+00 0.77757334364520E+00 0.59444506456653E+00
 0.24830000400543E+01 0.12415000200272E+01 0.12415000200272E+01 0.00000000000000E+00 0.64557998673439E+01
