#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-60 0 MONOZONE(1=OUI,0=NON)                                                                     
0.000000 0.000000 0.000000                                                                          
6.000000 4.000000 3.000000                                                                          
#PAROI PAR_1                                                                                        
TRA_10                                                                                              
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_2                                                                                        
TRA_3                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_3                                                                                        
TRA_3                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_4                                                                                        
TRA_3                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_5                                                                                        
TRA_3                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_6                                                                                        
TRA_8                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#FOYER FOY_1                                                                                        
400kW                                                                                               
CIRCULAIRE                                                                                          
3.000000 2.000000 0.001000 0.560000                                                                 
1.000000 0.000000 0                                                                                 
Methane_CH4_D=1m                                                                                    
#PCI                                                                                                
0.000000 50050000.000000                                                                            
#FINPCI                                                                                             
#PARTRAYONNEE                                                                                       
0.000000 0.200000                                                                                   
#FINPARTRAYONNEE                                                                                    
#RAPPORT                                                                                            
0.000000 4.000000                                                                                   
#FINRAPPORT                                                                                         
#AIRE_SPECIFIQUE                                                                                    
76.000000 0.000000 0.000000                                                                         
#DEBITPYROLYSE                                                                                      
0.000000 0.000000                                                                                   
1.000000 0.000800                                                                                   
4.000000 0.003200                                                                                   
8.000000 0.006400                                                                                   
10.000000 0.008000                                                                                  
53.300000 0.008000                                                                                  
63.900000 0.008000                                                                                  
74.600000 0.008000                                                                                  
100.000000 0.008000                                                                                 
1500.000000 0.008000                                                                                
2000.000000 0.008000                                                                                
2001.000000 0.000000                                                                                
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL60 0 MONOZONE(1=OUI,0=NON)                                                               
0.000000 0.000000 3.019000                                                                          
6.000000 4.000000 1.500000                                                                          
#PAROI PAR_7                                                                                        
TRA_8                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_8                                                                                        
TRA_3                                                                                               
CROISSANT                                                                                           
#FINPAROI                                                                                           
#PAROI PAR_9                                                                                        
TRA_3                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_10                                                                                       
TRA_3                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_11                                                                                       
TRA_3                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#PAROI PAR_12                                                                                       
TRA_9                                                                                               
DECROISSANT                                                                                         
#FINPAROI                                                                                           
#FINLOCAL                                                                                           
#TRANCHEPAROI TRA_3                                                                                 
#TRANCHE 0                                                                                          
Al2O3-SiO2(2)                                                                                       
0.100000 1440.000000 1000.000000 0.390000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_8                                                                                 
#TRANCHE 0                                                                                          
Marinite                                                                                            
0.019000 700.000000 1120.000000 0.120000 0                                                          
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_9                                                                                 
#TRANCHE 0                                                                                          
Al2O3-SiO2                                                                                          
0.100000 1920.000000 1000.000000 0.630000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#TRANCHEPAROI TRA_10                                                                                
#TRANCHE 0                                                                                          
Al2O3-SiO2                                                                                          
0.100000 1920.000000 1000.000000 0.630000 0                                                         
#OPTIONTEMPERATURE                                                                                  
0 0.000000                                                                                          
#FINTRANCHE                                                                                         
#FINTRANCHEPAROI                                                                                    
#OUVERTURE OUV_2                                                                                    
Door                                                                                                
LOC_1 EXT                                                                                           
PAR_2                                                                                               
4.500000 0.000000                                                                                   
5.250000 2.060000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#OUVERTURE OUV_1                                                                                    
Plenum                                                                                              
LOC_1 LOC_2                                                                                         
PAR_6                                                                                               
4.600000 1.800000                                                                                   
5.000000 2.200000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#CONDINIT 2000.000000 10.000000 22.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-60           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-60           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-60           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-60           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-60           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-60           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-60           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-60           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-60           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-60           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-60           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-60           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-60           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-60           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-60           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-60           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-60           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-60           #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_1 #400kW       #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #400kW       #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #400kW       #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #400kW       #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #400kW       #TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_1 #400kW       #HEIGHT#m#Flame height
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #400kW       #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #400kW       #MASS#kg#Consumed combustible mass
#ROOM#LOC_2 #Plenum-LLNL60     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL60     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL60     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL60     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL60     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL60     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL60     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL60     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL60     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL60     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL60     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL60     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL60     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL60     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL60     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL60     #VISIBILITY#m#Upper layer light-reflecting sign
#WALL#PAR_7 # #TEMPERATURE#K#Internal temperature
#WALL#PAR_7 # #TEMPERATURE#K#External temperature
#NOUVELLE LIGNE
#WALL#PAR_8 # #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_8 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_8 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_8 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_9 # #TEMPERATURE#K#Internal lower temperature
#NOUVELLE LIGNE
#WALL#PAR_9 # #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_9 # #TEMPERATURE#K#External lower temperature
#WALL#PAR_9 # #TEMPERATURE#K#External upper temperature
#WALL#PAR_10# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_10# #TEMPERATURE#K#Internal upper temperature
#NOUVELLE LIGNE
#WALL#PAR_10# #TEMPERATURE#K#External lower temperature
#WALL#PAR_10# #TEMPERATURE#K#External upper temperature
#WALL#PAR_11# #TEMPERATURE#K#Internal lower temperature
#WALL#PAR_11# #TEMPERATURE#K#Internal upper temperature
#WALL#PAR_11# #TEMPERATURE#K#External lower temperature
#NOUVELLE LIGNE
#WALL#PAR_11# #TEMPERATURE#K#External upper temperature
#WALL#PAR_12# #TEMPERATURE#K#Internal temperature
#WALL#PAR_12# #TEMPERATURE#K#External temperature
#WALL#PAR_7 # #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_7 # #AREA_HEAT_FLUX#W/m2#Total heat flux
#NOUVELLE LIGNE
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Radiative heat flux
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Total heat flux
#WALL#PAR_12# #AREA_HEAT_FLUX#W/m2#Convective heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_8 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#NOUVELLE LIGNE
#WALL#PAR_9 # #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_10# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#NOUVELLE LIGNE
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Radiative lower heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Radiative upper heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Total lower heat flux
#WALL#PAR_11# #AREA_HEAT_FLUX#W/m2#Total upper heat flux
#ROOM#LOC_2 #Plenum-LLNL60     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL60     #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_2 #Ope_ 1_Roo 2#MASS#kg#Consumed combustible mass
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Outside to upper layer
#NOUVELLE LIGNE
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Upper layer to the outside
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Outside to lower layer
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Lower layer to the outside
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Global mass flow rate to the outside
#OPENING#OUV_2 #Door        #MASS_FLOW_RATE#kg/s#Outside to upper and lower layers
#NOUVELLE LIGNE
#OPENING#OUV_2 #Door        #HEIGHT#m#Upper layer to the outside flow height
#OPENING#OUV_2 #Door        #HEIGHT#m#Lower layer to the outside flow height
#OPENING#OUV_2 #Door        #HEIGHT#m#Outside to inside flow height
#OPENING#OUV_2 #Door        #AREA#m2#Area
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Down upper layer to up upper layer
#NOUVELLE LIGNE
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Down upper layer to up lower layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Down lower layer to up upper layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Down lower layer to up lower layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Up upper layer to down upper layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Up lower layer to down upper layer
#NOUVELLE LIGNE
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Up upper layer to down lower layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#Up lower layer to down lower layer
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#From down to up
#OPENING#OUV_1 #Plenum      #MASS_FLOW_RATE#kg/s#From up to down
#OPENING#OUV_1 #Plenum      #HEIGHT#m#Down to up flow height
#NOUVELLE LIGNE
#OPENING#OUV_1 #Plenum      #HEIGHT#m#Up to down flow height
#OPENING#OUV_1 #Plenum      #AREA#m2#Area
#NOUVELLE LIGNE
#FIN ENTETE
#DEBUTRESULTAT
      0.00000000
 0.30000000000000E+01 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.75113568449155E-06
 0.10000000000074E-02 0.00000000000000E+00 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.46964168579047E-03 0.46964168579047E-03 0.66517591985549E-03 0.66850179945477E-03
 0.00000000000000E+00 0.44494323696705E-03 0.52392372935195E-03 0.44494323696705E-03 0.52392372935195E-03
 0.44268197994836E-03 0.52391746634689E-03 0.44268197994836E-03 0.52391746634689E-03 0.44494323690916E-03
 0.52392372940984E-03 0.44494323690916E-03 0.52392372940984E-03 0.44268197994836E-03 0.52391746634689E-03
 0.44268197994836E-03 0.52391746634689E-03 0.53957767694418E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29515000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.37541551379067E-06 0.99965124775045E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.54759990970877E-03 0.54759990970877E-03
 0.67257809586107E-03 0.67594098634038E-03 .00000000000000E+00 0.47865696217437E-03 0.52801643803167E-03
 0.47865696217437E-03 0.52801643803167E-03 0.47449087611325E-03 0.52800918640833E-03 0.47449087611325E-03
 0.52800918640833E-03 0.47865696223226E-03 0.52801643808956E-03 0.47865696223226E-03 0.52801643808956E-03
 0.47449087611325E-03 0.52800918635044E-03 0.47449087611325E-03 0.52800918635044E-03 0.43674697338198E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29515000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.58010340334375E-04 0.58010340334375E-04 0.00000000000000E+00
 0.20599999427795E+01 0.10299999713898E+01 0.10299999713898E+01 0.15449999570847E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29000309852526E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29000309852526E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     12.24173261
 0.30000000000000E+01 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.22999999999883E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.25524554772008E-08
 0.99999999999488E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29515000165690E+03 0.29515000000000E+03 0.29515000228351E+03 0.29515000228351E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000227169E+03 0.29515000227169E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000228351E+03 0.29515000228351E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000227169E+03 0.29515000227169E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000802709E+03
 0.29515000663970E+03 0.48268517893854E-03 0.48268517893854E-03 0.63508368159809E-03 0.63825910000608E-03
 .00000000000000E+00 0.45201543552515E-03 0.53743311294593E-03 0.45201543552515E-03 0.53743311294593E-03
 0.44966758832683E-03 0.53748852998476E-03 0.44966758832683E-03 0.53748852998476E-03 0.45201543546726E-03
 0.53743311300382E-03 0.45201543546726E-03 0.53743311300382E-03 0.44966758832683E-03 0.53748852998476E-03
 0.44966758832683E-03 0.53748852998476E-03 0.53967240492378E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29515000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.34861727321802E-08 0.99965124775001E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515000661443E+03 0.29515000805777E+03
 0.29515000245657E+03 0.29515000245657E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000243505E+03
 0.29515000243505E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000245657E+03 0.29515000245657E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000243505E+03 0.29515000243505E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000236651E+03 0.29515000000000E+03 0.52667969800912E-03 0.52667969800912E-03
 0.68483168980029E-03 0.68825584824929E-03 .00000000000000E+00 0.48628765301159E-03 0.53431679558832E-03
 0.48628765301159E-03 0.53431679558832E-03 0.48202131637943E-03 0.53442233154494E-03 0.48202131637943E-03
 0.53442233154494E-03 0.48628765306948E-03 0.53431679564621E-03 0.48628765306948E-03 0.53431679564621E-03
 0.48202131632154E-03 0.53442233148706E-03 0.48202131632154E-03 0.53442233148706E-03 0.43695886661137E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29515000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19667189789350E-06 0.00000000000000E+00 0.00000000000000E+00 0.19667189789350E-06
 0.20599999427795E+01 0.10299999713898E+01 0.10299999713898E+01 0.15449999570847E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.72685055724456E-07 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.72685055724456E-07 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     22.88014993
 0.30000000000000E+01 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.22999999999883E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.27432225876203E-08
 0.99999999999487E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29515000230476E+03 0.29515000000000E+03 0.29515000316638E+03 0.29515000316638E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000314990E+03 0.29515000314990E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000316638E+03 0.29515000316638E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000314990E+03 0.29515000314990E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515001095377E+03
 0.29515000906836E+03 0.48728413508557E-03 0.48728413508557E-03 0.62420733025303E-03 0.62732836690429E-03
 .00000000000000E+00 0.45440219427126E-03 0.54214729719045E-03 0.45440219427126E-03 0.54214729719045E-03
 0.45202576247032E-03 0.54222706070556E-03 0.45202576247032E-03 0.54222706070556E-03 0.45440219432915E-03
 0.54214729719045E-03 0.45440219432915E-03 0.54214729719045E-03 0.45202576235454E-03 0.54222706076345E-03
 0.45202576235454E-03 0.54222706076345E-03 0.53957797340463E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29515000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.40316521537992E-08 0.99965124786639E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515000904581E+03 0.29515001098104E+03
 0.29515000340656E+03 0.29515000340656E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000337666E+03
 0.29515000337666E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000340656E+03 0.29515000340656E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000337666E+03 0.29515000337666E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000328328E+03 0.29515000000000E+03 0.51914652419429E-03 0.51914652419429E-03
 0.68905063531536E-03 0.69249588849194E-03 .00000000000000E+00 0.48889900049911E-03 0.53647438945626E-03
 0.48889900049911E-03 0.53647438945626E-03 0.48459949612306E-03 0.53662387545218E-03 0.48459949612306E-03
 0.53662387545218E-03 0.48889900049911E-03 0.53647438945626E-03 0.48889900049911E-03 0.53647438945626E-03
 0.48459949629672E-03 0.53662387539429E-03 0.48459949629672E-03 0.53662387539429E-03 0.43694793867972E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29515000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21240561506904E-06 0.00000000000000E+00 0.00000000000000E+00 0.21240561506904E-06
 0.20599999427795E+01 0.10299999713898E+01 0.10299999713898E+01 0.15449999570847E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98542473426492E-07 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98542473426492E-07 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     30.07189807
 0.30000000000000E+01 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.22999999999883E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.74835681204945E-06
 0.10000000000023E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29515000266178E+03 0.29515000000000E+03 0.29515000365016E+03 0.29515000365016E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000363111E+03 0.29515000363111E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000365016E+03 0.29515000365016E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000363111E+03 0.29515000363111E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515001250129E+03
 0.29515001035310E+03 0.48970017251635E-03 0.48970017251635E-03 0.61853897027157E-03 0.62163166512292E-03
 .00000000000000E+00 0.45563820568028E-03 0.54459669322321E-03 0.45563820568028E-03 0.54459669322321E-03
 0.45324790109533E-03 0.54468988140813E-03 0.45324790109533E-03 0.54468988140813E-03 0.45563820562239E-03
 0.54459669316532E-03 0.45563820562239E-03 0.54459669316532E-03 0.45324790109533E-03 0.54468988140813E-03
 0.45324790109533E-03 0.54468988140813E-03 0.53952889133079E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29515000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.37146711777226E-06 0.99965124788390E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515001033624E+03 0.29515001252160E+03
 0.29515000392720E+03 0.29515000392720E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000389270E+03
 0.29515000389270E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000392720E+03 0.29515000392720E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000389270E+03 0.29515000389270E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000378655E+03 0.29515000000000E+03 0.51523379961454E-03 0.51523379961454E-03
 0.69124822742472E-03 0.69470446856185E-03 .00000000000000E+00 0.49025889930143E-03 0.53759795853179E-03
 0.49025889930143E-03 0.53759795853179E-03 0.48594366415494E-03 0.53777156570230E-03 0.48594366415494E-03
 0.53777156570230E-03 0.49025889924354E-03 0.53759795853179E-03 0.49025889924354E-03 0.53759795853179E-03
 0.48594366421283E-03 0.53777156570230E-03 0.48594366421283E-03 0.53777156570230E-03 0.43694502593344E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29515000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.57795686946405E-04 0.57795686946405E-04 0.00000000000000E+00
 0.20599999427795E+01 0.10299999713898E+01 0.10299999713898E+01 0.15449999570847E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29090302337406E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29090302337406E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     40.00090747
 0.30000000000000E+01 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000000000E+03
 0.22999999999883E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.19614440091445E-08
 0.99999999999489E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29515000309782E+03 0.29515000000000E+03 0.29515000423878E+03 0.29515000423878E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000421660E+03 0.29515000421660E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000423878E+03 0.29515000423878E+03 0.29515000000000E+03 0.29515000000000E+03
 0.29515000421660E+03 0.29515000421660E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515001434191E+03
 0.29515001188666E+03 0.49255662821872E-03 0.49255662821872E-03 0.61185568158267E-03 0.61491495999058E-03
 .00000000000000E+00 0.45708364387179E-03 0.54747307799512E-03 0.45708364387179E-03 0.54747307799512E-03
 0.45467776151548E-03 0.54758265936755E-03 0.45467776151548E-03 0.54758265936755E-03 0.45708364387179E-03
 0.54747307805300E-03 0.45708364387179E-03 0.54747307805300E-03 0.45467776145759E-03 0.54758265948333E-03
 0.45467776145759E-03 0.54758265948333E-03 0.53946595572085E-04 0.00000000000000E+00 0.72597662518206E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29515000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.83896510764158E-09 0.15000000000000E+01 0.29515000000000E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.64416302130123E-08 0.99965125010785E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515001187523E+03 0.29515001435561E+03
 0.29515000456075E+03 0.29515000456075E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000452064E+03
 0.29515000452064E+03 0.29515000000000E+03 0.29515000000000E+03 0.29515000456075E+03 0.29515000456075E+03
 0.29515000000000E+03 0.29515000000000E+03 0.29515000452064E+03 0.29515000452064E+03 0.29515000000000E+03
 0.29515000000000E+03 0.29515000439959E+03 0.29515000000000E+03 0.51062740555925E-03 0.51062740555925E-03
 0.69382604622634E-03 0.69729517645747E-03 .00000000000000E+00 0.49185298167558E-03 0.53891514072318E-03
 0.49185298167558E-03 0.53891514072318E-03 0.48752028107454E-03 0.53911811271632E-03 0.48752028107454E-03
 0.53911811271632E-03 0.49185298167558E-03 0.53891514060741E-03 0.49185298167558E-03 0.53891514060741E-03
 0.48752028107454E-03 0.53911811271632E-03 0.48752028107454E-03 0.53911811271632E-03 0.43693739011456E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29515000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15171834598638E-06 0.00000000000000E+00 0.00000000000000E+00 0.15171834598638E-06
 0.20599999427795E+01 0.10299999713898E+01 0.10299999713898E+01 0.15449999570847E+01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.34579514625976E-06 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.34579514625976E-06 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     50.31227337
 0.19560899638115E+01 0.29518391274986E+03 0.35935759183403E+03 0.31751442863490E+03 0.31474201215213E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22270034036673E+00 0.00000000000000E+00 0.36708483049452E+00
 0.99988872752534E-03 0.12696705330594E+00 0.80000000000000E+04 0.30000000000000E+04 0.63008471817670E+02
 0.23628176931626E+02 0.29765571349372E+03 0.29515000000001E+03 0.29772033030867E+03 0.29838214687580E+03
 0.29515000000001E+03 0.29515000000001E+03 0.29706540302217E+03 0.29835184174818E+03 0.29515000000001E+03
 0.29515000000001E+03 0.29772033030867E+03 0.29838214687580E+03 0.29515000000001E+03 0.29515000000001E+03
 0.29706540302217E+03 0.29835184174818E+03 0.29515000000001E+03 0.29515000000001E+03 0.30434484843561E+03
 0.29515658017048E+03 0.11145355244138E+04 0.11084560562436E+04 0.63780126934971E+03 0.12444138618419E+04
 0.60342358614545E+03 0.84667171461994E+03 0.45127399485008E+03 0.84158969606174E+03 0.10415115610135E+04
 0.62473327251107E+03 0.43995185881295E+03 0.62129627797715E+03 0.10305367944772E+04 0.84667171461994E+03
 0.45127399485008E+03 0.84158969606174E+03 0.10415115610135E+04 0.62473327251107E+03 0.43995185881295E+03
 0.62129627797716E+03 0.10305367944772E+04 0.10759930531816E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.40025647611178E+03 0.18908721972624E+01
 0.18908721972624E+01 0.43233898902325E-01 0.14346471218927E+01 0.29515042234276E+03 0.29643359750809E+03
 0.29520632846954E+03 0.29520609696811E+03 0.22999951683771E+00 0.00000000000000E+00 0.22980258273485E+00
 0.00000000000000E+00 0.25766501168060E+01 0.99966886511549E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29515656293508E+03 0.30436857106919E+03
 0.29515377825383E+03 0.29519072482376E+03 0.29515000000001E+03 0.29515000000001E+03 0.29515383873231E+03
 0.29519071388531E+03 0.29515000000001E+03 0.29515000000001E+03 0.29515377825383E+03 0.29519072482376E+03
 0.29515000000001E+03 0.29515000000001E+03 0.29515383873231E+03 0.29519071388531E+03 0.29515000000001E+03
 0.29515000000001E+03 0.29516263168002E+03 0.29515000000001E+03 0.88909184166239E+00 0.88678055424006E+00
 0.17351532059732E+01 0.64735117920100E+01 0.47296828200070E+01 0.12586122999916E+01 0.81442838819328E+00
 0.12576028331378E+01 0.11571868758080E+02 0.12787199078024E+01 0.80973428089720E+00 0.12776873501870E+01
 0.11567276562842E+02 0.12586122999916E+01 0.81442838819362E+00 0.12576028331378E+01 0.11571868758081E+02
 0.12787199078024E+01 0.80973428089726E+00 0.12776873501870E+01 0.11567276562842E+02 0.22808455323054E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29795694364635E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.51197919367602E-01 0.00000000000000E+00 0.93526735389673E+00 0.98646527326433E+00 0.00000000000000E+00
 0.20080449532955E+01 0.97804498190576E+00 0.10299999713898E+01 0.15449999570847E+01 0.10478515051779E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12227605241244E-01 0.10478515051779E-01 0.12227605241244E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     60.05199373
 0.12067539619286E+01 0.29532780564457E+03 0.39802697208962E+03 0.35671609675817E+03 0.34918278527521E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21761109831096E+00 0.00000000000000E+00 -.20993594443084E-02
 0.99939788926122E-03 0.19643002900519E+00 0.80000000000000E+04 0.30000000000000E+04 0.40726970517267E+02
 0.15272613943975E+02 0.30009360052756E+03 0.29515000000001E+03 0.30022758871413E+03 0.30236883143189E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29894090052930E+03 0.30231317568638E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30022758871413E+03 0.30236883143189E+03 0.29515000000002E+03 0.29515000000002E+03
 0.29894090052930E+03 0.30231317568638E+03 0.29515000000002E+03 0.29515000000002E+03 0.31508670836317E+03
 0.29516380863502E+03 0.14151066647554E+04 0.14005431708744E+04 0.65087338559566E+03 0.15993855318096E+04
 0.94525777928593E+03 0.96954973722755E+03 0.59855983244846E+03 0.95710855233382E+03 0.14600912128215E+04
 0.72754452258057E+03 0.58858937592208E+03 0.71918563619556E+03 0.14507339015698E+04 0.96954973722755E+03
 0.59855983244846E+03 0.95710855233382E+03 0.14600912128215E+04 0.72754452258057E+03 0.58858937592208E+03
 0.71918563619557E+03 0.14507339015698E+04 0.14503308020311E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.43260712433450E+03 0.24041168759811E+01
 0.24041168759811E+01 0.12115166171576E+00 0.13516032774476E+01 0.29515294359133E+03 0.29749064850777E+03
 0.29538421542326E+03 0.29538257658971E+03 0.22999951683771E+00 0.00000000000000E+00 0.22964900652575E+00
 0.00000000000000E+00 0.54565597536369E+01 0.99968874780085E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29516375716599E+03 0.31516143285247E+03
 0.29515797669714E+03 0.29523380779870E+03 0.29515000000002E+03 0.29515000000002E+03 0.29515809967711E+03
 0.29523377122407E+03 0.29515000000002E+03 0.29515000000002E+03 0.29515797669714E+03 0.29523380779870E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29515809967711E+03 0.29523377122407E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29518599556585E+03 0.29515000000001E+03 0.11015271097831E+01 0.10968384225315E+01
 0.20101975283359E+01 0.11989001239649E+02 0.99687527236713E+01 0.15785365993219E+01 0.87300182961709E+00
 0.15768293953377E+01 0.16264377580358E+02 0.16023084166337E+01 0.86400956145000E+00 0.16005481351602E+01
 0.16255660665399E+02 0.15785365993218E+01 0.87300182961709E+00 0.15768293953376E+01 0.16264377580358E+02
 0.16023084166337E+01 0.86400956145000E+00 0.16005481351602E+01 0.16255660665399E+02 0.40520283213117E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29988831087550E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.62225760538366E+00 0.71688290498683E-02 0.38192095680535E-01 0.66044970106420E+00 0.71688290498683E-02
 0.16333769523541E+01 0.60337698096432E+00 0.10299999713898E+01 0.15449999570847E+01 0.11780915965001E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14852062386595E-01 0.11780915965001E-01 0.14852062386595E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     70.01764743
 0.89455529086342E+00 0.29558405577806E+03 0.42217747496336E+03 0.38442920398979E+03 0.37436792141913E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21357259507470E+00 0.00000000000000E+00 -.43477722566465E+00
 0.99852719042009E-03 0.24602233241945E+00 0.80000000000000E+04 0.30000000000000E+04 0.32517373204806E+02
 0.12194014951802E+02 0.30240063590718E+03 0.29515000000001E+03 0.30229765782344E+03 0.30626878552554E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30057415217959E+03 0.30620380392091E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30229765782344E+03 0.30626878552554E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30057415217959E+03 0.30620380392091E+03 0.29515000000002E+03 0.29515000000002E+03 0.32466909704280E+03
 0.29517129228582E+03 0.16684595971852E+04 0.16450336284496E+04 0.74340140640757E+03 0.18719195572995E+04
 0.11248011438599E+04 0.10846724033067E+04 0.77191635680973E+03 0.10654696631181E+04 0.18010283492780E+04
 0.83608865145233E+03 0.76347063945942E+03 0.82304033993863E+03 0.17932965962113E+04 0.10846724033067E+04
 0.77191635680973E+03 0.10654696631181E+04 0.18010283492780E+04 0.83608865145233E+03 0.76347063945943E+03
 0.82304033993863E+03 0.17932965962113E+04 0.17744345742738E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.45796270812968E+03 0.21262832942577E+01
 0.21262832942577E+01 0.20087689137250E+00 0.12682950569197E+01 0.29515442287969E+03 0.29823511404358E+03
 0.29563029712687E+03 0.29562613414845E+03 0.22999951683771E+00 0.00000000000000E+00 0.22951035159501E+00
 0.00000000000000E+00 0.70176562690333E+01 0.99969914394310E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29517123931228E+03 0.32474372659696E+03
 0.29516244236694E+03 0.29527334281237E+03 0.29515000000002E+03 0.29515000000002E+03 0.29516262874035E+03
 0.29527327010085E+03 0.29515000000002E+03 0.29515000000002E+03 0.29516244236694E+03 0.29527334281237E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29516262874035E+03 0.29527327010085E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29521285653416E+03 0.29515000000001E+03 0.14182550001427E+01 0.14101125340057E+01
 0.24740783155175E+01 0.16527240113033E+02 0.14040791405937E+02 0.20601933819866E+01 0.10786321602168E+01
 0.20570716765240E+01 0.19025232510432E+02 0.20903471266213E+01 0.10637463287749E+01 0.20871329499316E+01
 0.19010840907396E+02 0.20601933819865E+01 0.10786321602168E+01 0.20570716765239E+01 0.19025232510432E+02
 0.20903471266213E+01 0.10637463287749E+01 0.20871329499316E+01 0.19010840907396E+02 0.57111908429982E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30133016468628E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.88213808301207E+00 0.50105765034814E+00 0.00000000000000E+00 0.88213808301207E+00 0.50105765034814E+00
 0.14772776168215E+01 0.44727764543171E+00 0.10299999713898E+01 0.15449999570847E+01 0.12231591887243E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16055278530843E-01 0.12231591887243E-01 0.16055278530843E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     80.22794899
 0.75857454017351E+00 0.29589747594144E+03 0.43897834826805E+03 0.40279917929046E+03 0.39115242590194E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20993567163486E+00 0.00000000000000E+00 -.94168774699009E+00
 0.99746451540846E-03 0.28920733275805E+00 0.80000000000000E+04 0.30000000000000E+04 0.27661815914926E+02
 0.10373180968097E+02 0.30465672262998E+03 0.29515000000001E+03 0.30427878468234E+03 0.31021562160448E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30219927883269E+03 0.31014530539301E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30427878468234E+03 0.31021562160448E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30219927883269E+03 0.31014530539301E+03 0.29515000000002E+03 0.29515000000002E+03 0.33375589418944E+03
 0.29517975422090E+03 0.18740497260150E+04 0.18413863070954E+04 0.84618900772010E+03 0.20608993645956E+04
 0.12104794118369E+04 0.11886740840729E+04 0.93951201285492E+03 0.11626958098142E+04 0.20746144402423E+04
 0.93778188748168E+03 0.93206342841876E+03 0.91985354020467E+03 0.20679373343586E+04 0.11886740840729E+04
 0.93951201285493E+03 0.11626958098142E+04 0.20746144402423E+04 0.93778188748168E+03 0.93206342841876E+03
 0.91985354020467E+03 0.20679373343586E+04 0.20270311583504E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.47397848121586E+03 0.19737516709041E+01
 0.19737516709041E+01 0.28255930386612E+00 0.11884624756083E+01 0.29515525148446E+03 0.29884384783573E+03
 0.29592134226830E+03 0.29591383112556E+03 0.22999951683771E+00 0.00000000000000E+00 0.22937272298884E+00
 0.00000000000000E+00 0.76927724800170E+01 0.99970300015328E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29517970027813E+03 0.33381975014763E+03
 0.29516763034867E+03 0.29531167323174E+03 0.29515000000002E+03 0.29515000000002E+03 0.29516788959316E+03
 0.29531155139897E+03 0.29515000000002E+03 0.29515000000002E+03 0.29516763034867E+03 0.29531167323174E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29516788959316E+03 0.29531155139897E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29524203008594E+03 0.29515000000001E+03 0.17421531729901E+01 0.17291541832895E+01
 0.29549259999281E+01 0.20508648564384E+02 0.17538947934456E+02 0.25740282451089E+01 0.13171181934828E+01
 0.25685409958776E+01 0.21245804873911E+02 0.26111058702889E+01 0.12953398988494E+01 0.26054719592641E+01
 0.21224806666569E+02 0.25740282451089E+01 0.13171181934824E+01 0.25685409958776E+01 0.21245804873910E+02
 0.26111058702890E+01 0.12953398988494E+01 0.26054719592641E+01 0.21224806666569E+02 0.72743566151708E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30256756143703E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92271537027735E+00 0.69715362443450E+00 0.00000000000000E+00 0.92271537027735E+00 0.69715362443450E+00
 0.14092872414765E+01 0.37928727008675E+00 0.10299999713898E+01 0.15449999570847E+01 0.12470508620084E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16701497310781E-01 0.12470508620084E-01 0.16701497310781E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
     90.11217766
 0.69776643243446E+00 0.29621007379290E+03 0.44945798437459E+03 0.41381423509305E+03 0.40118260951461E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20690329379932E+00 0.00000000000000E+00 -.12595652743724E+01
 0.99640872290146E-03 0.32527678106980E+00 0.80000000000000E+04 0.30000000000000E+04 0.24594439153292E+02
 0.92229146824846E+01 0.30672797337198E+03 0.29515000000001E+03 0.30608904770839E+03 0.31384630434888E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30371851227761E+03 0.31377255820158E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30608904770839E+03 0.31384630434888E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30371851227761E+03 0.31377255820158E+03 0.29515000000002E+03 0.29515000000002E+03 0.34161001098423E+03
 0.29518839553162E+03 0.20250993326500E+04 0.19834864667735E+04 0.92416942305524E+03 0.21610554976010E+04
 0.12322652274305E+04 0.12695159767801E+04 0.10708073839277E+04 0.12371298787630E+04 0.22605212605291E+04
 0.10179344053435E+04 0.10640147440280E+04 0.99526196480807E+03 0.22545357890714E+04 0.12695159767801E+04
 0.10708073839277E+04 0.12371298787630E+04 0.22605212605291E+04 0.10179344053435E+04 0.10640147440280E+04
 0.99526196480807E+03 0.22545357890714E+04 0.21935814158996E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.48390474856427E+03 0.19006320069728E+01
 0.19006320069728E+01 0.36163313322349E+00 0.11174331837803E+01 0.29515588812388E+03 0.29933411097108E+03
 0.29622152106529E+03 0.29621040061985E+03 0.22999951683771E+00 0.00000000000000E+00 0.22923996602618E+00
 0.00000000000000E+00 0.80304916298497E+01 0.99970417677507E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29518833962548E+03 0.34166436665618E+03
 0.29517303894051E+03 0.29534715114489E+03 0.29515000000002E+03 0.29515000000002E+03 0.29517337306337E+03
 0.29534697290249E+03 0.29515000000002E+03 0.29515000000002E+03 0.29517303894051E+03 0.29534715114489E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29517337306337E+03 0.29534697290249E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29527110579773E+03 0.29515000000001E+03 0.20031621701258E+01 0.19846426147418E+01
 0.33225670278728E+01 0.23767144264376E+02 0.20427964401364E+02 0.30086940704526E+01 0.15087440533785E+01
 0.30002904245314E+01 0.22983833551713E+02 0.30513241104153E+01 0.14807192609706E+01 0.30427113736365E+01
 0.22956907671681E+02 0.30086940704526E+01 0.15087440533785E+01 0.30002904245314E+01 0.22983833551713E+02
 0.30513241104153E+01 0.14807192609706E+01 0.30427113736365E+01 0.22956907671681E+02 0.86124922027804E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30357627905003E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92601443729459E+00 0.79169246793248E+00 0.00000000000000E+00 0.92601443729459E+00 0.79169246793248E+00
 0.13788831876070E+01 0.34888321621723E+00 0.10299999713898E+01 0.15449999570847E+01 0.12612408106193E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17000243019445E-01 0.12612408106193E-01 0.17000243019445E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    100.59610742
 0.67103893062826E+00 0.29652859906686E+03 0.45645550200802E+03 0.42068310936525E+03 0.40731780501296E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20416613580263E+00 0.00000000000000E+00 -.14463557352944E+01
 0.99533655447505E-03 0.35833274234979E+00 0.80000000000000E+04 0.30000000000000E+04 0.22325618216017E+02
 0.83721068310066E+01 0.30877468892337E+03 0.29515000000001E+03 0.30788222322773E+03 0.31740486866047E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30524375743242E+03 0.31732846105116E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30788222322773E+03 0.31740486866047E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30524375743242E+03 0.31732846105116E+03 0.29515000000002E+03 0.29515000000002E+03 0.34886413397574E+03
 0.29519779661048E+03 0.21394754369005E+04 0.20885962912619E+04 0.97894798876956E+03 0.22019674674634E+04
 0.12181247387500E+04 0.13336292845884E+04 0.11741609595073E+04 0.12947158914885E+04 0.23889318215154E+04
 0.10821383024612E+04 0.11678644602095E+04 0.10545395830218E+04 0.23834688030632E+04 0.13336292845884E+04
 0.11741609595073E+04 0.12947158914885E+04 0.23889318215154E+04 0.10821383024612E+04 0.11678644602095E+04
 0.10545395830218E+04 0.23834688030632E+04 0.23023308326516E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49027909949163E+03 0.18908898231648E+01
 0.18908898231648E+01 0.44550457130871E+00 0.10487647623475E+01 0.29515654039485E+03 0.29977498067107E+03
 0.29654587572527E+03 0.29653084045244E+03 0.22999951683771E+00 0.00000000000000E+00 0.22909864441213E+00
 0.00000000000000E+00 0.82250515991142E+01 0.99970388761627E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29519774000218E+03 0.34891132794412E+03
 0.29517887802951E+03 0.29538329171771E+03 0.29515000000002E+03 0.29515000000002E+03 0.29517929077192E+03
 0.29538304930551E+03 0.29515000000002E+03 0.29515000000002E+03 0.29517887802951E+03 0.29538329171771E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29517929077192E+03 0.29538304930551E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29530200019995E+03 0.29515000000001E+03 0.22490598923309E+01 0.22241023043475E+01
 0.36644773549535E+01 0.26749617455065E+02 0.23066817713337E+02 0.34380624430938E+01 0.16949328903463E+01
 0.34261619697798E+01 0.24529466930233E+02 0.34859847788824E+01 0.16605652282986E+01 0.34738033755424E+01
 0.24496552299558E+02 0.34380624430939E+01 0.16949328903474E+01 0.34261619697798E+01 0.24529466930235E+02
 0.34859847788824E+01 0.16605652282987E+01 0.34738033755424E+01 0.24496552299558E+02 0.98888612899217E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30449453108479E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92176579194839E+00 0.84428590478721E+00 0.00000000000000E+00 0.92176579194839E+00 0.84428590478721E+00
 0.13655194367039E+01 0.33551946531413E+00 0.10299999713898E+01 0.15449999570847E+01 0.12722467829599E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17118594652052E-01 0.12722467829599E-01 0.17118594652052E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    111.08003719
 0.66286854790042E+00 0.29683456248492E+03 0.46073152117706E+03 0.42451747483921E+03 0.41063387111874E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20189581286990E+00 0.00000000000000E+00 -.15377002311252E+01
 0.99430970579284E-03 0.38626493428154E+00 0.80000000000000E+04 0.30000000000000E+04 0.20711173316522E+02
 0.77666899936957E+01 0.31066548314525E+03 0.29515000000001E+03 0.30954682117948E+03 0.32064763332939E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30667138887565E+03 0.32056915401316E+03 0.29515000000002E+03
 0.29515000000002E+03 0.30954682117948E+03 0.32064763332939E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30667138887564E+03 0.32056915401316E+03 0.29515000000002E+03 0.29515000000002E+03 0.35508515317178E+03
 0.29520814820867E+03 0.22171863315019E+04 0.21574435858995E+04 0.10089595853506E+04 0.21985439531505E+04
 0.11845395698731E+04 0.13790996263493E+04 0.12456610304437E+04 0.13340104893107E+04 0.24648165430661E+04
 0.11281716097189E+04 0.12397280720120E+04 0.10958551806032E+04 0.24597363076545E+04 0.13790996263493E+04
 0.12456610304437E+04 0.13340104893107E+04 0.24648165430661E+04 0.11281716097189E+04 0.12397280720120E+04
 0.10958551806033E+04 0.24597363076545E+04 0.23608326745199E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49395929107112E+03 0.18908907110078E+01
 0.18908907110078E+01 0.52937600939392E+00 0.98646815175507E+00 0.29515725131019E+03 0.30015266443770E+03
 0.29686745380093E+03 0.29684862814397E+03 0.22999951683771E+00 0.00000000000000E+00 0.22895683894997E+00
 0.00000000000000E+00 0.83314610925981E+01 0.99970252987845E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29520809011252E+03 0.35512841442420E+03
 0.29518483921295E+03 0.29541791112485E+03 0.29515000000002E+03 0.29515000000002E+03 0.29518532947938E+03
 0.29541760102710E+03 0.29515000000002E+03 0.29515000000002E+03 0.29518483921295E+03 0.29541791112485E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29518532947938E+03 0.29541760102710E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29533300254111E+03 0.29515000000001E+03 0.24395317085637E+01 0.24070731274334E+01
 0.39117412785707E+01 0.29279006828165E+02 0.25347706843202E+02 0.37973009098143E+01 0.18364147025890E+01
 0.37815573012861E+01 0.25815046471202E+02 0.38490519148162E+01 0.17967352535268E+01 0.38329508336591E+01
 0.25777187869574E+02 0.37973009098136E+01 0.18364147025912E+01 0.37815573012853E+01 0.25815046471207E+02
 0.38490519148162E+01 0.17967352535261E+01 0.38329508336591E+01 0.25777187869573E+02 0.11004606581890E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30529265473929E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91721766260933E+00 0.87080953798575E+00 0.00000000000000E+00 0.91721766260933E+00 0.87080953798575E+00
 0.13614342453400E+01 0.33143427395021E+00 0.10299999713898E+01 0.15449999570847E+01 0.12811092981934E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17123288566102E-01 0.12811092981934E-01 0.17123288566102E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    120.74233998
 0.66249506376503E+00 0.29710621110230E+03 0.46320072233957E+03 0.42652179106852E+03 0.41230059137930E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20018085719523E+00 0.00000000000000E+00 -.15762135258096E+01
 0.99340021402392E-03 0.40769090768139E+00 0.80000000000000E+04 0.30000000000000E+04 0.19622708893601E+02
 0.73585158351004E+01 0.31226714521740E+03 0.29515000000001E+03 0.31096415875768E+03 0.32335287338787E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30789212071643E+03 0.32327277110644E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31096415875768E+03 0.32335287338787E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30789212071643E+03 0.32327277110644E+03 0.29515000000002E+03 0.29515000000002E+03 0.36001328297444E+03
 0.29521939061542E+03 0.22660174547012E+04 0.21985912113683E+04 0.10208381821465E+04 0.21728622917915E+04
 0.11469199187343E+04 0.14087777772929E+04 0.12904060694284E+04 0.13583655407071E+04 0.25034989527191E+04
 0.11586039309668E+04 0.12847270281854E+04 0.11221972914104E+04 0.24986869447745E+04 0.14087777772929E+04
 0.12904060694284E+04 0.13583655407071E+04 0.25034989527191E+04 0.11586039309668E+04 0.12847270281854E+04
 0.11221972914104E+04 0.24986869447745E+04 0.23861024944917E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49595801000713E+03 0.18908910853465E+01
 0.18908910853465E+01 0.60667443175452E+00 0.93413627254768E+00 0.29515801065212E+03 0.30045701553629E+03
 0.29715702042248E+03 0.29713491774021E+03 0.22999951683771E+00 0.00000000000000E+00 0.22882615479394E+00
 0.00000000000000E+00 0.83881342055639E+01 0.99970051728266E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29521933141430E+03 0.36005529152027E+03
 0.29519024756076E+03 0.29544852327870E+03 0.29515000000002E+03 0.29515000000002E+03 0.29519080457756E+03
 0.29544815135281E+03 0.29515000000002E+03 0.29515000000002E+03 0.29519024756076E+03 0.29544852327870E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29519080457756E+03 0.29544815135281E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29536108148980E+03 0.29515000000001E+03 0.25661800508496E+01 0.25221245824213E+01
 0.40700268194063E+01 0.31287877267461E+02 0.27197500313958E+02 0.40722303191058E+01 0.19309592052423E+01
 0.40528445989597E+01 0.26821793169478E+02 0.41262328881101E+01 0.18873457121724E+01 0.41064192406851E+01
 0.26780332105336E+02 0.40722303191054E+01 0.19309592052448E+01 0.40528445989592E+01 0.26821793169483E+02
 0.41262328881101E+01 0.18873457121710E+01 0.41064192406851E+01 0.26780332105333E+02 0.11909205021622E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30594620899277E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91421656334831E+00 0.88285320709024E+00 0.00000000000000E+00 0.91421656334831E+00 0.88285320709024E+00
 0.13612475032723E+01 0.33124753188251E+00 0.10299999713898E+01 0.15449999570847E+01 0.12881393854099E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17073997083150E-01 0.12881393854099E-01 0.17073997083150E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    131.79318477
 0.66508157274608E+00 0.29740558359770E+03 0.46503992700357E+03 0.42787642275069E+03 0.41338388963328E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19860524447996E+00 0.00000000000000E+00 -.15944393053833E+01
 0.99240006461111E-03 0.42757040417873E+00 0.80000000000000E+04 0.30000000000000E+04 0.18710368916591E+02
 0.70163883437217E+01 0.31395307149072E+03 0.29515000000001E+03 0.31246264932986E+03 0.32615411375606E+03
 0.29515000000002E+03 0.29515000000002E+03 0.30918677391742E+03 0.32607238330486E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31246264932986E+03 0.32615411375606E+03 0.29515000000002E+03 0.29515000000002E+03
 0.30918677391742E+03 0.32607238330485E+03 0.29515000000002E+03 0.29515000000002E+03 0.36486891389421E+03
 0.29523640430586E+03 0.23044909692546E+04 0.22288355522973E+04 0.10227661334164E+04 0.21309556840881E+04
 0.11030757200046E+04 0.14329771444329E+04 0.13246668161627E+04 0.13768799719434E+04 0.25250855593109E+04
 0.11838087849830E+04 0.13192091788017E+04 0.11430170365722E+04 0.25205086934904E+04 0.14329771444329E+04
 0.13246668161627E+04 0.13768799719435E+04 0.25250855593109E+04 0.11838087849830E+04 0.13192091788017E+04
 0.11430170365722E+04 0.25205086934904E+04 0.23953731937660E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49736567579257E+03 0.18908912624962E+01
 0.18908912624962E+01 0.69508119004948E+00 0.87958233365932E+00 0.29515907678634E+03 0.30076476618277E+03
 0.29747765594205E+03 0.29745211892527E+03 0.22999951683771E+00 0.00000000000000E+00 0.22867727774445E+00
 0.00000000000000E+00 0.84283569647908E+01 0.99969730325093E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29523634454132E+03 0.36491162894332E+03
 0.29519634820215E+03 0.29548222148234E+03 0.29515000000002E+03 0.29515000000002E+03 0.29519697515872E+03
 0.29548177994490E+03 0.29515000000002E+03 0.29515000000002E+03 0.29519634820215E+03 0.29548222148233E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29519697515872E+03 0.29548177994490E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29539266582491E+03 0.29515000000001E+03 0.26653398794163E+01 0.26053816557296E+01
 0.42199392112663E+01 0.33317283668763E+02 0.29076244761440E+02 0.43586260035432E+01 0.20248608984086E+01
 0.43350528228122E+01 0.27833681569162E+02 0.44142138254664E+01 0.19772916136739E+01 0.43901342069940E+01
 0.27788634829258E+02 0.43586260035433E+01 0.20248608984101E+01 0.43350528228123E+01 0.27833681569165E+02
 0.44142138254663E+01 0.19772916136735E+01 0.43901342069939E+01 0.27788634829257E+02 0.12839388472061E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30662030874818E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91226056461072E+00 0.88910290024447E+00 0.00000000000000E+00 0.91226056461072E+00 0.88910290024447E+00
 0.13625407577628E+01 0.33254078637304E+00 0.10299999713898E+01 0.15449999570847E+01 0.12952961418831E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16986621032452E-01 0.12952961418831E-01 0.16986621032452E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    140.63386060
 0.66767612781059E+00 0.29763732599463E+03 0.46609719413701E+03 0.42860498331942E+03 0.41395313536410E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19759509359450E+00 0.00000000000000E+00 -.16004830065039E+01
 0.99162731617312E-03 0.44034860582914E+00 0.80000000000000E+04 0.30000000000000E+04 0.18167424386269E+02
 0.68127841448510E+01 0.31520494206840E+03 0.29515000000001E+03 0.31357904651272E+03 0.32820287415716E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31015349900570E+03 0.32811994779208E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31357904651272E+03 0.32820287415716E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31015349900570E+03 0.32811994779208E+03 0.29515000000002E+03 0.29515000000002E+03 0.36827415093293E+03
 0.29525519519019E+03 0.23268110769822E+04 0.22449679313179E+04 0.10190656527037E+04 0.20940328227803E+04
 0.10698718418131E+04 0.14474104551283E+04 0.13436050584436E+04 0.13870438748555E+04 0.25321807192096E+04
 0.11990960299409E+04 0.13382842573860E+04 0.11550030976388E+04 0.25277505162177E+04 0.14474104551283E+04
 0.13436050584436E+04 0.13870438748555E+04 0.25321807192096E+04 0.11990960299410E+04 0.13382842573860E+04
 0.11550030976388E+04 0.25277505162177E+04 0.23945316452332E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49814752019018E+03 0.18908913212394E+01
 0.18908913212394E+01 0.76580659668544E+00 0.83955097878923E+00 0.29516013195340E+03 0.30098620650912E+03
 0.29772534877862E+03 0.29769731855486E+03 0.22999951683771E+00 0.00000000000000E+00 0.22855896903244E+00
 0.00000000000000E+00 0.84512227076630E+01 0.99969395509423E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29525513515893E+03 0.36831841056316E+03
 0.29520119156240E+03 0.29550828653023E+03 0.29515000000002E+03 0.29515000000002E+03 0.29520186896207E+03
 0.29550778995570E+03 0.29515000000002E+03 0.29515000000002E+03 0.29520119156240E+03 0.29550828653023E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29520186896207E+03 0.29550778995570E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29541753436379E+03 0.29515000000001E+03 0.27026030114103E+01 0.26236268219304E+01
 0.43257395379209E+01 0.34779590024100E+02 0.30432221788490E+02 0.45731737258531E+01 0.20940066762636E+01
 0.45462846782832E+01 0.28563740146775E+02 0.46292212270011E+01 0.20435643958098E+01 0.46017657942988E+01
 0.28516111674380E+02 0.45731737258531E+01 0.20940066762654E+01 0.45462846782832E+01 0.28563740146778E+02
 0.46292212270010E+01 0.20435643958093E+01 0.46017657942986E+01 0.28516111674379E+02 0.13514328281543E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30711484117213E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91152201326994E+00 0.89130044256581E+00 0.00000000000000E+00 0.91152201326994E+00 0.89130044256581E+00
 0.13638380352951E+01 0.33383806390530E+00 0.10299999713898E+01 0.15449999570847E+01 0.13004651066940E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16907440803323E-01 0.13004651066940E-01 0.16907440803323E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    151.68470538
 0.67067353940078E+00 0.29791656758572E+03 0.46715795399759E+03 0.42932271411827E+03 0.41451480254450E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19658778612551E+00 0.00000000000000E+00 -.16044449920504E+01
 0.99069781086632E-03 0.45302764473263E+00 0.80000000000000E+04 0.30000000000000E+04 0.17658966495789E+02
 0.66221124359210E+01 0.31666645159718E+03 0.29515000000001E+03 0.31488563468044E+03 0.33056275508632E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31128707352702E+03 0.33047840879998E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31488563468044E+03 0.33056275508632E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31128707352701E+03 0.33047840879998E+03 0.29515000000002E+03 0.29515000000002E+03 0.37206093848145E+03
 0.29528821200031E+03 0.23482085909516E+04 0.22590610801159E+04 0.10109967976621E+04 0.20480114977093E+04
 0.10319597160589E+04 0.14615279346185E+04 0.13606914469549E+04 0.13961229468022E+04 0.25342286800300E+04
 0.12142961263342E+04 0.13555032157522E+04 0.11662992061489E+04 0.25299425919093E+04 0.14615279346185E+04
 0.13606914469549E+04 0.13961229468022E+04 0.25342286800300E+04 0.12142961263342E+04 0.13555032157523E+04
 0.11662992061489E+04 0.25299425919093E+04 0.23884455146285E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49892735806359E+03 0.18908913597488E+01
 0.18908913597488E+01 0.85421335498040E+00 0.79349060220999E+00 0.29516183363850E+03 0.30123836616280E+03
 0.29802391852810E+03 0.29799308490431E+03 0.22999951683771E+00 0.00000000000000E+00 0.22841238672971E+00
 0.00000000000000E+00 0.84746968851463E+01 0.99968842325871E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29528815288761E+03 0.37210768101237E+03
 0.29520724543672E+03 0.29553992780331E+03 0.29515000000002E+03 0.29515000000002E+03 0.29520797799925E+03
 0.29553936257849E+03 0.29515000000002E+03 0.29515000000002E+03 0.29520724543672E+03 0.29553992780331E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29520797799925E+03 0.29553936257849E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29544812649399E+03 0.29515000000001E+03 0.26880951171552E+01 0.25726272847139E+01
 0.44618280914427E+01 0.36463707192576E+02 0.31979569960676E+02 0.48396711137356E+01 0.21847001845759E+01
 0.48087900923156E+01 0.29410559831222E+02 0.48953351723218E+01 0.21307107562279E+01 0.48638184417547E+01
 0.29359746898843E+02 0.48396711137359E+01 0.21847001845781E+01 0.48087900923160E+01 0.29410559831227E+02
 0.48953351723218E+01 0.21307107562272E+01 0.48638184417547E+01 0.29359746898842E+02 0.14288254779916E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30768753443298E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91126568305001E+00 0.89255948256796E+00 0.00000000000000E+00 0.91126568305001E+00 0.89255948256796E+00
 0.13653367410902E+01 0.33533676970039E+00 0.10299999713898E+01 0.15449999570847E+01 0.13063419370768E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16806661726347E-01 0.13063419370768E-01 0.16806661726347E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    160.52538121
 0.67269285840913E+00 0.29813120659954E+03 0.46790164684413E+03 0.42983385927029E+03 0.41492117752028E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19594854631354E+00 0.00000000000000E+00 -.16070172277043E+01
 0.98998453507548E-03 0.46097495361402E+00 0.80000000000000E+04 0.30000000000000E+04 0.17354522056525E+02
 0.65079457711968E+01 0.31776466495523E+03 0.29515000000001E+03 0.31586932239518E+03 0.33231529465289E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31214203870694E+03 0.33222984528155E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31586932239518E+03 0.33231529465289E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31214203870693E+03 0.33222984528155E+03 0.29515000000002E+03 0.29515000000002E+03 0.37478927268493E+03
 0.29532471600405E+03 0.23619624945951E+04 0.22672669707239E+04 0.10031577517491E+04 0.20128393128341E+04
 0.10046657723262E+04 0.14707343784021E+04 0.13709782657280E+04 0.14015019436938E+04 0.25329691015562E+04
 0.12243619435775E+04 0.13658719266243E+04 0.11733927043667E+04 0.25287738745950E+04 0.14707343784021E+04
 0.13709782657280E+04 0.14015019436938E+04 0.25329691015562E+04 0.12243619435775E+04 0.13658719266243E+04
 0.11733927043667E+04 0.25287738745950E+04 0.23817241106319E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.49948082374213E+03 0.18908913847503E+01
 0.18908913847503E+01 0.92493876161637E+00 0.75947658689331E+00 0.29516365068806E+03 0.30142394441620E+03
 0.29825424674047E+03 0.29822141013277E+03 0.22999951683771E+00 0.00000000000000E+00 0.22829630318578E+00
 0.00000000000000E+00 0.84917871453647E+01 0.99968243776070E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29532465934352E+03 0.37483802259727E+03
 0.29521214403643E+03 0.29556461791543E+03 0.29515000000002E+03 0.29515000000002E+03 0.29521291371935E+03
 0.29556399690110E+03 0.29515000000002E+03 0.29515000000002E+03 0.29521214403643E+03 0.29556461791543E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29521291371935E+03 0.29556399690110E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29547225908387E+03 0.29515000000001E+03 0.26186815976320E+01 0.24591098364118E+01
 0.45866987859267E+01 0.37729225415075E+02 0.33119593135218E+02 0.50618883388373E+01 0.22680399789933E+01
 0.50280482230538E+01 0.30054282748032E+02 0.51164684214595E+01 0.22110304228147E+01 0.50819432122139E+01
 0.30000744615646E+02 0.50618883388373E+01 0.22680399789952E+01 0.50280482230538E+01 0.30054282748036E+02
 0.51164684214596E+01 0.22110304228148E+01 0.50819432122139E+01 0.30000744615646E+02 0.14860220308154E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30811459041566E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91138828123363E+00 0.89311175556533E+00 0.00000000000000E+00 0.91138828123363E+00 0.89311175556533E+00
 0.13663464005943E+01 0.33634642920456E+00 0.10299999713898E+01 0.15449999570847E+01 0.13106289337740E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16728298252407E-01 0.13106289337740E-01 0.16728298252407E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    171.57622600
 0.67476250411523E+00 0.29838844079463E+03 0.46877133256171E+03 0.43044867032599E+03 0.41541827921161E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19531516016255E+00 0.00000000000000E+00 -.16105902025668E+01
 0.98913105661575E-03 0.46869228372487E+00 0.80000000000000E+04 0.30000000000000E+04 0.17068768310886E+02
 0.64007881165824E+01 0.31906173192259E+03 0.29515000000002E+03 0.31703288005978E+03 0.33436492280184E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31315508460691E+03 0.33427811207752E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31703288005978E+03 0.33436492280184E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31315508460691E+03 0.33427811207752E+03 0.29515000000002E+03 0.29515000000002E+03 0.37789940233499E+03
 0.29538631505657E+03 0.23764173805439E+04 0.22750992914789E+04 0.99265316711373E+03 0.19717002148082E+04
 0.97408378185893E+03 0.14805058422824E+04 0.13811581001934E+04 0.14067030175101E+04 0.25296752667698E+04
 0.12351867945920E+04 0.13761307707891E+04 0.11806595939232E+04 0.25255701939394E+04 0.14805058422824E+04
 0.13811581001934E+04 0.14067030175101E+04 0.25296752667698E+04 0.12351867945920E+04 0.13761307707891E+04
 0.11806595939232E+04 0.25255701939394E+04 0.23725234565138E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50013960387167E+03 0.18908914194787E+01
 0.18908914194787E+01 0.10133455199113E+01 0.72009445231748E+00 0.29516679573419E+03 0.30163935902194E+03
 0.29853212107808E+03 0.29849706316012E+03 0.22999951683771E+00 0.00000000000000E+00 0.22815276854205E+00
 0.00000000000000E+00 0.85125097059641E+01 0.99967199049840E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29538626452142E+03 0.37795028400401E+03
 0.29521842399737E+03 0.29559486851677E+03 0.29515000000002E+03 0.29515000000002E+03 0.29521923024295E+03
 0.29559417501836E+03 0.29515000000002E+03 0.29515000000002E+03 0.29521842399737E+03 0.29559486851677E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29521923024295E+03 0.29559417501836E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29550209092276E+03 0.29515000000001E+03 0.24458398164617E+01 0.22046817700025E+01
 0.47769937888696E+01 0.39243593616528E+02 0.34442714858714E+02 0.53626778416932E+01 0.23940355758560E+01
 0.53256715070591E+01 0.30835904183402E+02 0.54148793168732E+01 0.23327633935721E+01 0.53771363501190E+01
 0.30778487511590E+02 0.53626778416928E+01 0.23940355758575E+01 0.53256715070587E+01 0.30835904183405E+02
 0.54148793168732E+01 0.23327633935721E+01 0.53771363501190E+01 0.30778487511590E+02 0.15523784339644E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30861445020445E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91176506712593E+00 0.89367606471518E+00 0.00000000000000E+00 0.91176506712593E+00 0.89367606471518E+00
 0.13673812234474E+01 0.33738125205761E+00 0.10299999713898E+01 0.15449999570847E+01 0.13155187586284E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16635545010056E-01 0.13155187586284E-01 0.16635545010056E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    182.50333362
 0.67642827902730E+00 0.29863004344970E+03 0.46959811272683E+03 0.43104890043691E+03 0.41590964574584E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19483248579365E+00 0.00000000000000E+00 -.16147581526305E+01
 0.98833077328562E-03 0.47438878383226E+00 0.80000000000000E+04 0.30000000000000E+04 0.16863805116498E+02
 0.63239269186869E+01 0.32027102028419E+03 0.29515000000003E+03 0.31811922140139E+03 0.33626025975660E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31410229287268E+03 0.33617209568110E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31811922140139E+03 0.33626025975660E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31410229287268E+03 0.33617209568110E+03 0.29515000000002E+03 0.29515000000002E+03 0.38071812721710E+03
 0.29546902285003E+03 0.23886328218934E+04 0.22810641695692E+04 0.98190569225323E+03 0.19338800801682E+04
 0.94706485945374E+03 0.14888339152931E+04 0.13892662896636E+04 0.14107149697252E+04 0.25255661633017E+04
 0.12445290889806E+04 0.13842970009134E+04 0.11866334858350E+04 0.25215303559707E+04 0.14888339152931E+04
 0.13892662896636E+04 0.14107149697251E+04 0.25255661633017E+04 0.12445290889806E+04 0.13842970009134E+04
 0.11866334858350E+04 0.25215303559707E+04 0.23632267398459E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50077583077322E+03 0.18908914599900E+01
 0.18908914599900E+01 0.11007623808720E+01 0.68419806116262E+00 0.29517133266079E+03 0.30183725944986E+03
 0.29879671665990E+03 0.29875975255456E+03 0.22999951683771E+00 0.00000000000000E+00 0.22801258106928E+00
 0.00000000000000E+00 0.85327634798792E+01 0.99965682492834E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29546898226424E+03 0.38077048603532E+03
 0.29522484629266E+03 0.29562418408790E+03 0.29515000000002E+03 0.29515000000002E+03 0.29522567663124E+03
 0.29562341475705E+03 0.29515000000002E+03 0.29515000000002E+03 0.29522484629266E+03 0.29562418408790E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29522567663124E+03 0.29562341475705E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29553111129480E+03 0.29515000000001E+03 0.21577004848576E+01 0.17956914582326E+01
 0.50118208202459E+01 0.40694623008559E+02 0.35657743084212E+02 0.56921778333758E+01 0.25474286469762E+01
 0.56530755252771E+01 0.31597256413467E+02 0.57406877307470E+01 0.24812317590529E+01 0.57008126685567E+01
 0.31535318121752E+02 0.56921778333759E+01 0.25474286469781E+01 0.56530755252771E+01 0.31597256413471E+02
 0.57406877307470E+01 0.24812317590530E+01 0.57008126685567E+01 0.31535318121752E+02 0.16125231326232E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30907526071454E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91223303083393E+00 0.89429313147128E+00 0.00000000000000E+00 0.91223303083393E+00 0.89429313147128E+00
 0.13682141109034E+01 0.33821413951365E+00 0.10299999713898E+01 0.15449999570847E+01 0.13198614820380E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16550958656520E-01 0.13198614820380E-01 0.16550958656520E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    192.26348818
 0.67769049683428E+00 0.29883592691299E+03 0.47032133694666E+03 0.43158332603815E+03 0.41635001084630E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19449627563705E+00 0.00000000000000E+00 -.16187745077330E+01
 0.98764982271297E-03 0.47819536007624E+00 0.80000000000000E+04 0.30000000000000E+04 0.16729564249064E+02
 0.62735865933992E+01 0.32129906897594E+03 0.29515000000005E+03 0.31904388924364E+03 0.33785946920679E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31491004306657E+03 0.33777009738076E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31904388924364E+03 0.33785946920679E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31491004306657E+03 0.33777009738076E+03 0.29515000000002E+03 0.29515000000002E+03 0.38304089701889E+03
 0.29556379396862E+03 0.23982520059463E+04 0.22853087479543E+04 0.97245565642401E+03 0.19027758815828E+04
 0.92545794687666E+03 0.14954414794120E+04 0.13953530008423E+04 0.14136087573306E+04 0.25216096736236E+04
 0.12520172445252E+04 0.13904230275999E+04 0.11912140581269E+04 0.25176232938770E+04 0.14954414794120E+04
 0.13953530008423E+04 0.14136087573306E+04 0.25216096736236E+04 0.12520172445252E+04 0.13904230275999E+04
 0.11912140581269E+04 0.25176232938770E+04 0.23551319864307E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50133800166009E+03 0.18908914990279E+01
 0.18908914990279E+01 0.11788436173359E+01 0.65441078563935E+00 0.29517717087978E+03 0.30200323817046E+03
 0.29902520346478E+03 0.29898676853434E+03 0.22999951683771E+00 0.00000000000000E+00 0.22788880776960E+00
 0.00000000000000E+00 0.85509444624771E+01 0.99963723243643E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29556376639132E+03 0.38309392844065E+03
 0.29523092864432E+03 0.29565012004174E+03 0.29515000000002E+03 0.29515000000002E+03 0.29523176885487E+03
 0.29564927650263E+03 0.29515000000002E+03 0.29515000000002E+03 0.29523092864432E+03 0.29565012004174E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29523176885487E+03 0.29564927650263E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29555697798705E+03 0.29515000000001E+03 0.17955763177682E+01 0.12826069754698E+01
 0.52801065894023E+01 0.41979931694737E+02 0.36673424572387E+02 0.60308820144863E+01 0.27211193671572E+01
 0.59912212743066E+01 0.32284095917818E+02 0.60751097419593E+01 0.26495588450890E+01 0.60346601710496E+01
 0.32217189978771E+02 0.60308820144863E+01 0.27211193671587E+01 0.59912212743066E+01 0.32284095917821E+02
 0.60751097419589E+01 0.26495588450891E+01 0.60346601710492E+01 0.32217189978771E+02 0.16624718061128E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30946140063926E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91272101842231E+00 0.89486159890387E+00 0.00000000000000E+00 0.91272101842231E+00 0.89486159890387E+00
 0.13688452198069E+01 0.33884524841714E+00 0.10299999713898E+01 0.15449999570847E+01 0.13233791653900E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16481018393826E-01 0.13233791653900E-01 0.16481018393826E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    202.02364274
 0.67881014525240E+00 0.29903437879849E+03 0.47103508493241E+03 0.43211647682765E+03 0.41679076520600E+03
 0.22999999952827E+00 0.00000000000000E+00 0.19423115568328E+00 0.00000000000000E+00 -.16230596289072E+01
 0.98699433524237E-03 0.48104253330651E+00 0.80000000000000E+04 0.30000000000000E+04 0.16630546045504E+02
 0.62364547670642E+01 0.32228464014560E+03 0.29515000000010E+03 0.31993129844084E+03 0.33938373756016E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31568651110338E+03 0.33929315325647E+03 0.29515000000002E+03
 0.29515000000002E+03 0.31993129844084E+03 0.33938373756016E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31568651110338E+03 0.33929315325647E+03 0.29515000000002E+03 0.29515000000002E+03 0.38521313107151E+03
 0.29567993609455E+03 0.24069445126944E+04 0.22888027158520E+04 0.96320034922000E+03 0.18738352176971E+04
 0.90581886673100E+03 0.15014537374998E+04 0.14006435009325E+04 0.14160273283587E+04 0.25175802244810E+04
 0.12588877191182E+04 0.13957436022976E+04 0.11952636366561E+04 0.25136341596242E+04 0.15014537374998E+04
 0.14006435009325E+04 0.14160273283587E+04 0.25175802244811E+04 0.12588877191182E+04 0.13957436022976E+04
 0.11952636366561E+04 0.25136341596242E+04 0.23473108865090E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50189615125477E+03 0.18908915406782E+01
 0.18908915406782E+01 0.12569248537999E+01 0.62654875388593E+00 0.29518542315789E+03 0.30216030095782E+03
 0.29924690029512E+03 0.29920720637741E+03 0.22999951683771E+00 0.00000000000000E+00 0.22776635489687E+00
 0.00000000000000E+00 0.85691737631172E+01 0.99960946621239E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29567992650069E+03 0.38526619902075E+03
 0.29523742133313E+03 0.29567591460339E+03 0.29515000000002E+03 0.29515000000002E+03 0.29523825968124E+03
 0.29567498906144E+03 0.29515000000002E+03 0.29515000000002E+03 0.29523742133313E+03 0.29567591460339E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29523825968124E+03 0.29567498906144E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29558287838282E+03 0.29515000000001E+03 0.13244906634596E+01 0.61228777643770E+00
 0.56031265914008E+01 0.43261300341748E+02 0.37630158117390E+02 0.64115996224252E+01 0.29294344908440E+01
 0.63732595524437E+01 0.32980009768634E+02 0.64505107994326E+01 0.28515732923121E+01 0.64113849408274E+01
 0.32907242153314E+02 0.64115996224252E+01 0.29294344908451E+01 0.63732595524437E+01 0.32980009768637E+02
 0.64505107994322E+01 0.28515732923122E+01 0.64113849408269E+01 0.32907242153314E+02 0.17086405116398E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30982531454115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91321513909137E+00 0.89548863518093E+00 0.00000000000000E+00 0.91321513909137E+00 0.89548863518093E+00
 0.13694050440160E+01 0.33940507262620E+00 0.10299999713898E+01 0.15449999570847E+01 0.13265505263407E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16417025869888E-01 0.13265505263407E-01 0.16417025869888E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    211.78379729
 0.67983563183170E+00 0.29922588066925E+03 0.47173863716961E+03 0.43264519756479E+03 0.41722834226091E+03
 0.22999999722042E+00 0.00000000000000E+00 0.19402306539317E+00 0.00000000000000E+00 -.16274606859372E+01
 0.98636262543226E-03 0.48312462006525E+00 0.80000000000000E+04 0.30000000000000E+04 0.16558874600345E+02
 0.62095779751295E+01 0.32323231091123E+03 0.29515000000020E+03 0.32078546610537E+03 0.34084224515952E+03
 0.29515000000002E+03 0.29515000000002E+03 0.31643502591990E+03 0.34075044081191E+03 0.29515000000002E+03
 0.29515000000002E+03 0.32078546610537E+03 0.34084224515952E+03 0.29515000000002E+03 0.29515000000002E+03
 0.31643502591990E+03 0.34075044081191E+03 0.29515000000002E+03 0.29515000000002E+03 0.38725570514853E+03
 0.29581897901072E+03 0.24148782432183E+04 0.22916948419110E+04 0.95416028714605E+03 0.18467711443823E+04
 0.88784005580048E+03 0.15069809507914E+04 0.14052971494225E+04 0.14180676648383E+04 0.25135327356977E+04
 0.12652523130025E+04 0.14004201366607E+04 0.11988842994334E+04 0.25096199424487E+04 0.15069809507914E+04
 0.14052971494225E+04 0.14180676648383E+04 0.25135327356977E+04 0.12652523130025E+04 0.14004201366607E+04
 0.11988842994334E+04 0.25096199424488E+04 0.23397572815949E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50244809435286E+03 0.18908915834553E+01
 0.18908915834553E+01 0.13350060902638E+01 0.60042632143687E+00 0.29519695000041E+03 0.30230944995483E+03
 0.29946242849893E+03 0.29942168313929E+03 0.22999951683771E+00 0.00000000000000E+00 0.22764517350715E+00
 0.00000000000000E+00 0.85873874684558E+01 0.99957061321455E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29581899228859E+03 0.38730830242679E+03
 0.29524439880857E+03 0.29570165490355E+03 0.29515000000002E+03 0.29515000000002E+03 0.29524522265640E+03
 0.29570063805435E+03 0.29515000000002E+03 0.29515000000002E+03 0.29524439880857E+03 0.29570165490355E+03
 0.29515000000002E+03 0.29515000000002E+03 0.29524522265640E+03 0.29570063805435E+03 0.29515000000002E+03
 0.29515000000002E+03 0.29560886131492E+03 0.29515000000001E+03 0.73693046789883E+00 -.23006640343546E+00
 0.59857416731768E+01 0.44549137176832E+02 0.38533466795289E+02 0.68387059184736E+01 0.31754459592330E+01
 0.68042785004902E+01 0.33690116365776E+02 0.68712584825630E+01 0.30902411300163E+01 0.68360722293013E+01
 0.33610495237362E+02 0.68387059184736E+01 0.31754459592337E+01 0.68042785004902E+01 0.33690116365778E+02
 0.68712584825630E+01 0.30902411300163E+01 0.68360722293013E+01 0.33610495237362E+02 0.17510816127328E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31016851383754E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91370284429536E+00 0.89614947782061E+00 0.00000000000000E+00 0.91370284429536E+00 0.89614947782061E+00
 0.13699177873056E+01 0.33991781591585E+00 0.10299999713898E+01 0.15449999570847E+01 0.13293846608661E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16358990155563E-01 0.13293846608661E-01 0.16358990155563E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    221.54395185
 0.68079873956667E+00 0.29941093128573E+03 0.47243037735395E+03 0.43316657041936E+03 0.41765978766091E+03
 0.22999999620433E+00 0.00000000000000E+00 0.19386076451933E+00 0.00000000000000E+00 -.16318646311824E+01
 0.98575296268520E-03 0.48459774612985E+00 0.80000000000000E+04 0.30000000000000E+04 0.16508537367106E+02
 0.61907015126648E+01 0.32414604656733E+03 0.29515000000040E+03 0.32160988403554E+03 0.34224262637358E+03
 0.29515000000002E+03 0.29515000000003E+03 0.31715851142267E+03 0.34214959442761E+03 0.29515000000002E+03
 0.29515000000003E+03 0.32160988403554E+03 0.34224262637358E+03 0.29515000000002E+03 0.29515000000003E+03
 0.31715851142267E+03 0.34214959442761E+03 0.29515000000002E+03 0.29515000000003E+03 0.38918495112856E+03
 0.29598204833283E+03 0.24221733978573E+04 0.22940887792384E+04 0.94533715663980E+03 0.18213337646827E+04
 0.87126992225975E+03 0.15121018814670E+04 0.14094199724954E+04 0.14197970196232E+04 0.25094764378686E+04
 0.12711913856334E+04 0.14045602910159E+04 0.12021478912671E+04 0.25055914947229E+04 0.15121018814669E+04
 0.14094199724954E+04 0.14197970196232E+04 0.25094764378686E+04 0.12711913856334E+04 0.14045602910159E+04
 0.12021478912671E+04 0.25055914947230E+04 0.23324419809842E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50299154745807E+03 0.18908916262605E+01
 0.18908916262605E+01 0.14130873267278E+01 0.57588074969633E+00 0.29521279602539E+03 0.30245148690176E+03
 0.29967240508262E+03 0.29963081462539E+03 0.22999951683771E+00 0.00000000000000E+00 0.22752521017805E+00
 0.00000000000000E+00 0.86055205637319E+01 0.99951713856340E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29598208943434E+03 0.38923666035563E+03
 0.29525193730328E+03 0.29572742922004E+03 0.29515000000001E+03 0.29515000000002E+03 0.29525273321693E+03
 0.29572631025568E+03 0.29515000000001E+03 0.29515000000002E+03 0.29525193730328E+03 0.29572742922004E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29525273321693E+03 0.29572631025568E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29563498296215E+03 0.29515000000001E+03 0.27248982207459E-01 -.12562589619404E+01
 0.64316063551566E+01 0.45851451072179E+02 0.39387686685247E+02 0.73156406025287E+01 0.34615247808587E+01
 0.72884221610901E+01 0.34418235434331E+02 0.73408052883661E+01 0.33678505376123E+01 0.73128855662108E+01
 0.34330694180241E+02 0.73156406025287E+01 0.34615247808591E+01 0.72884221610900E+01 0.34418235434332E+02
 0.73408052883661E+01 0.33678505376122E+01 0.73128855662109E+01 0.34330694180241E+02 0.17898264451052E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31049235601416E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91417884386763E+00 0.89682152679846E+00 0.00000000000000E+00 0.91417884386763E+00 0.89682152679846E+00
 0.13703993411731E+01 0.34039936978334E+00 0.10299999713898E+01 0.15449999570847E+01 0.13318887652968E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16306863100174E-01 0.13318887652968E-01 0.16306863100174E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    231.30410641
 0.68171782212800E+00 0.29959016945102E+03 0.47310909564901E+03 0.43367878082712E+03 0.41808339233038E+03
 0.22999999331786E+00 0.00000000000000E+00 0.19373524964929E+00 0.00000000000000E+00 -.16362114724982E+01
 0.98516316671555E-03 0.48558667050733E+00 0.80000000000000E+04 0.30000000000000E+04 0.16474916808655E+02
 0.61780938032456E+01 0.32502920212770E+03 0.29515000000078E+03 0.32240750364182E+03 0.34359109243411E+03
 0.29515000000003E+03 0.29515000000004E+03 0.31785944679701E+03 0.34349682676016E+03 0.29515000000003E+03
 0.29515000000004E+03 0.32240750364182E+03 0.34359109243411E+03 0.29515000000003E+03 0.29515000000004E+03
 0.31785944679701E+03 0.34349682676016E+03 0.29515000000003E+03 0.29515000000004E+03 0.39101341353103E+03
 0.29616988391649E+03 0.24289261397918E+04 0.22960680090320E+04 0.93673732802721E+03 0.17973369319704E+04
 0.85591591730310E+03 0.15168783207584E+04 0.14130935575348E+04 0.14212684672524E+04 0.25054145964968E+04
 0.12767681256031E+04 0.14082469651948E+04 0.12051113352493E+04 0.25015533691770E+04 0.15168783207584E+04
 0.14130935575348E+04 0.14212684672524E+04 0.25054145964968E+04 0.12767681256031E+04 0.14082469651948E+04
 0.12051113352493E+04 0.25015533691770E+04 0.23253448294195E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50352505801089E+03 0.18908916685107E+01
 0.18908916685107E+01 0.14911685631918E+01 0.55276884112048E+00 0.29523419301321E+03 0.30258706236478E+03
 0.29987743765119E+03 0.29983521005769E+03 0.22999951683771E+00 0.00000000000000E+00 0.22740641060174E+00
 0.00000000000000E+00 0.86235302483042E+01 0.99944487662224E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29616995788732E+03 0.39106389399679E+03
 0.29526011277803E+03 0.29575331224580E+03 0.29515000000001E+03 0.29515000000002E+03 0.29526086674354E+03
 0.29575207888168E+03 0.29515000000001E+03 0.29515000000002E+03 0.29526011277803E+03 0.29575331224580E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29526086674354E+03 0.29575207888168E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29566130803298E+03 0.29515000000002E+03 -.80842285224591E+00 -.24747669323372E+01
 0.69432233118834E+01 0.47174182492533E+02 0.40196243064090E+02 0.78449564777746E+01 0.37894436385129E+01
 0.78286272863718E+01 0.35167246580006E+02 0.78617346895053E+01 0.36861115878598E+01 0.78448053972784E+01
 0.35070663879561E+02 0.78449564777746E+01 0.37894436385136E+01 0.78286272863718E+01 0.35167246580007E+02
 0.78617346895057E+01 0.36861115878598E+01 0.78448053972788E+01 0.35070663879561E+02 0.18249042856481E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31079809904630E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91464098870995E+00 0.89749111021172E+00 0.00000000000000E+00 0.91464098870995E+00 0.89749111021172E+00
 0.13708588824538E+01 0.34085891106400E+00 0.10299999713898E+01 0.15449999570847E+01 0.13340686595386E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16260567354816E-01 0.13340686595386E-01 0.16260567354816E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    241.06426097
 0.68260354975197E+00 0.29976414305235E+03 0.47377378599865E+03 0.43418058600991E+03 0.41849806958017E+03
 0.22999999296868E+00 0.00000000000000E+00 0.19363928241537E+00 0.00000000000000E+00 -.16404650289465E+01
 0.98459136835777E-03 0.48619148107789E+00 0.80000000000000E+04 0.30000000000000E+04 0.16454422406299E+02
 0.61704084023623E+01 0.32588469692200E+03 0.29515000000144E+03 0.32318089090921E+03 0.34489292401549E+03
 0.29515000000005E+03 0.29515000000008E+03 0.31853999182916E+03 0.34479742055283E+03 0.29515000000005E+03
 0.29515000000007E+03 0.32318089090921E+03 0.34489292401549E+03 0.29515000000005E+03 0.29515000000008E+03
 0.31853999182916E+03 0.34479742055283E+03 0.29515000000005E+03 0.29515000000007E+03 0.39275153740892E+03
 0.29638287184824E+03 0.24352128465545E+04 0.22976975816035E+04 0.92836072514214E+03 0.17746213365997E+04
 0.84161880783180E+03 0.15213587712107E+04 0.14163793808337E+04 0.14225227097029E+04 0.25013420594482E+04
 0.12820323648149E+04 0.14115426790490E+04 0.12078188163255E+04 0.24975014265467E+04 0.15213587712107E+04
 0.14163793808337E+04 0.14225227097029E+04 0.25013420594482E+04 0.12820323648149E+04 0.14115426790490E+04
 0.12078188163255E+04 0.24975014265467E+04 0.23184439597968E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50404758893361E+03 0.18908917098541E+01
 0.18908917098541E+01 0.15692497996557E+01 0.53096407936438E+00 0.29526255607525E+03 0.30271672136905E+03
 0.30007812536061E+03 0.30003547248127E+03 0.22999951683771E+00 0.00000000000000E+00 0.22728872198170E+00
 0.00000000000000E+00 0.86413866492007E+01 0.99934904563112E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29638298376373E+03 0.39280052055463E+03
 0.29526899335735E+03 0.29577937342225E+03 0.29515000000001E+03 0.29515000000002E+03 0.29526969100906E+03
 0.29577801201140E+03 0.29515000000001E+03 0.29515000000002E+03 0.29526899335735E+03 0.29577937342225E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29526969100906E+03 0.29577801201140E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29568789367140E+03 0.29515000000002E+03 -.17723522942384E+01 -.38905927200730E+01
 0.75221578106372E+01 0.48521788257220E+02 0.40962019657530E+02 0.84284221231918E+01 0.41603748232415E+01
 0.84255343298600E+01 0.35939204175235E+02 0.84358610402888E+01 0.40461537488581E+01 0.84325790742380E+01
 0.35832423161030E+02 0.84284221231918E+01 0.41603748232422E+01 0.84255343298599E+01 0.35939204175237E+02
 0.84358610402887E+01 0.40461537488576E+01 0.84325790742379E+01 0.35832423161029E+02 0.18563515375033E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31108690733290E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91508947894844E+00 0.89814833490140E+00 0.00000000000000E+00 0.91508947894844E+00 0.89814833490140E+00
 0.13713017462658E+01 0.34130177487599E+00 0.10299999713898E+01 0.15449999570847E+01 0.13359295462990E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16219971591764E-01 0.13359295462990E-01 0.16219971591764E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    250.82441553
 0.68346171423325E+00 0.29993337352414E+03 0.47442386689491E+03 0.43467134298938E+03 0.41890332202449E+03
 0.22999999241854E+00 0.00000000000000E+00 0.19356703378288E+00 0.00000000000000E+00 -.16446095715693E+01
 0.98403579558490E-03 0.48649240164517E+00 0.80000000000000E+04 0.30000000000000E+04 0.16444244499907E+02
 0.61665916874650E+01 0.32671506732652E+03 0.29515000000257E+03 0.32393227041142E+03 0.34615260634400E+03
 0.29515000000009E+03 0.29515000000013E+03 0.31920202284513E+03 0.34605586348397E+03 0.29515000000007E+03
 0.29515000000013E+03 0.32393227041142E+03 0.34615260634400E+03 0.29515000000009E+03 0.29515000000013E+03
 0.31920202284513E+03 0.34605586348397E+03 0.29515000000007E+03 0.29515000000013E+03 0.39440810531815E+03
 0.29662108663902E+03 0.24410970553720E+04 0.22990313666198E+04 0.92020630067028E+03 0.17530558811353E+04
 0.82824854896163E+03 0.15255830940031E+04 0.14193275932588E+04 0.14235929908799E+04 0.24972547575316E+04
 0.12870251495321E+04 0.14144984112604E+04 0.12103066202403E+04 0.24934323923093E+04 0.15255830940031E+04
 0.14193275932588E+04 0.14235929908799E+04 0.24972547575316E+04 0.12870251495321E+04 0.14144984112604E+04
 0.12103066202403E+04 0.24934323923093E+04 0.23117222337613E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50455860385118E+03 0.18908917501380E+01
 0.18908917501380E+01 0.16473310361197E+01 0.51035429143054E+00 0.29529949493712E+03 0.30284092684354E+03
 0.30027505875222E+03 0.30023219831253E+03 0.22999951683771E+00 0.00000000000000E+00 0.22717209448750E+00
 0.00000000000000E+00 0.86590731310331E+01 0.99922421203574E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29662124153420E+03 0.39445538783600E+03
 0.29527862267552E+03 0.29580567408274E+03 0.29515000000001E+03 0.29515000000002E+03 0.29527924973597E+03
 0.29580416972770E+03 0.29515000000001E+03 0.29515000000002E+03 0.29527862267552E+03 0.29580567408274E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29527924973597E+03 0.29580416972770E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29571479284215E+03 0.29515000000003E+03 -.28654036001281E+01 -.55051907807762E+01
 0.81691420847192E+01 0.49897456291982E+02 0.41687468496840E+02 0.90672016503893E+01 0.45749801480587E+01
 0.90798715508281E+01 0.36735511028258E+02 0.90644034526026E+01 0.44486131195911E+01 0.90765979041106E+01
 0.36617355788440E+02 0.90672016503907E+01 0.45749801480551E+01 0.90798715508297E+01 0.36735511028251E+02
 0.90644034525901E+01 0.44486131195956E+01 0.90765979040963E+01 0.36617355788449E+02 0.18842473969600E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31135986874350E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91552529740255E+00 0.89878753613639E+00 0.00000000000000E+00 0.91552529740255E+00 0.89878753613639E+00
 0.13717308285064E+01 0.34173085711662E+00 0.10299999713898E+01 0.15449999570847E+01 0.13374765383632E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16184879285372E-01 0.13374765383632E-01 0.16184879285372E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    260.58457008
 0.68429527649156E+00 0.30009832999884E+03 0.47505910009167E+03 0.43515082390972E+03 0.41929903712652E+03
 0.22999999157634E+00 0.00000000000000E+00 0.19351379892858E+00 0.00000000000000E+00 -.16486423108559E+01
 0.98349485716836E-03 0.48655378100390E+00 0.80000000000000E+04 0.30000000000000E+04 0.16442170038210E+02
 0.61658137643287E+01 0.32752252838497E+03 0.29515000000444E+03 0.32466358070118E+03 0.34737398182553E+03
 0.29515000000016E+03 0.29515000000023E+03 0.31984717794052E+03 0.34727600053679E+03 0.29515000000012E+03
 0.29515000000023E+03 0.32466358070118E+03 0.34737398182553E+03 0.29515000000016E+03 0.29515000000023E+03
 0.31984717794052E+03 0.34727600053679E+03 0.29515000000012E+03 0.29515000000023E+03 0.39599059185002E+03
 0.29688433649550E+03 0.24466320493922E+04 0.23001141919283E+04 0.91227252224653E+03 0.17325318938975E+04
 0.81569800903974E+03 0.15295844415467E+04 0.14219799163775E+04 0.14245066997619E+04 0.24931502928647E+04
 0.12917806892190E+04 0.14171565398650E+04 0.12126048099060E+04 0.24893444919734E+04 0.15295844415467E+04
 0.14219799163775E+04 0.14245066997619E+04 0.24931502928647E+04 0.12917806892190E+04 0.14171565398650E+04
 0.12126048099060E+04 0.24893444919733E+04 0.23051663578417E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50505792417167E+03 0.18908917893352E+01
 0.18908917893352E+01 0.17254122725837E+01 0.49083956226521E+00 0.29534668579443E+03 0.30296008667093E+03
 0.30046878110188E+03 0.30042593665311E+03 0.22999951683771E+00 0.00000000000000E+00 0.22705648045214E+00
 0.00000000000000E+00 0.86765614414863E+01 0.99906472724032E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29688453930151E+03 0.39603602728490E+03
 0.29528911983316E+03 0.29583226873529E+03 0.29515000000001E+03 0.29515000000002E+03 0.29528965879485E+03
 0.29583060541077E+03 0.29515000000001E+03 0.29515000000002E+03 0.29528911983316E+03 0.29583226873529E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29528965879485E+03 0.29583060541077E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29574205335659E+03 0.29515000000005E+03 -.40871597992408E+01 -.73167788506684E+01
 0.88842690333568E+01 0.51303499260764E+02 0.42374808882240E+02 0.97614880319156E+01 0.50335148378585E+01
 0.98073046092438E+01 0.37557135555061E+02 0.97476472261710E+01 0.48937330276485E+01 0.97929228274100E+01
 0.37426424292992E+02 0.97614880319181E+01 0.50335148378537E+01 0.98073046092468E+01 0.37557135555052E+02
 0.97476472261626E+01 0.48937330276484E+01 0.97929228274000E+01 0.37426424292992E+02 0.19088693167509E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31161806296373E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91594946500683E+00 0.89940621808871E+00 0.00000000000000E+00 0.91594946500683E+00 0.89940621808871E+00
 0.13721476096355E+01 0.34214763824578E+00 0.10299999713898E+01 0.15449999570847E+01 0.13387312023661E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16154580515472E-01 0.13387312023661E-01 0.16154580515472E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    270.34472464
 0.68510575918544E+00 0.30025942838519E+03 0.47567950676670E+03 0.43561907144139E+03 0.41968533731495E+03
 0.22999999179055E+00 0.00000000000000E+00 0.19347577557646E+00 0.00000000000000E+00 -.16525663343489E+01
 0.98296714428699E-03 0.48642726188757E+00 0.80000000000000E+04 0.30000000000000E+04 0.16446446625866E+02
 0.61674174846997E+01 0.32830902287066E+03 0.29515000000746E+03 0.32537651844147E+03 0.34856036831016E+03
 0.29515000000026E+03 0.29515000000040E+03 0.32047689333022E+03 0.34846115211880E+03 0.29515000000021E+03
 0.29515000000040E+03 0.32537651844147E+03 0.34856036831016E+03 0.29515000000026E+03 0.29515000000040E+03
 0.32047689333022E+03 0.34846115211880E+03 0.29515000000021E+03 0.29515000000040E+03 0.39750547022895E+03
 0.29717220749029E+03 0.24518626727218E+04 0.23009834340617E+04 0.90455689387861E+03 0.17129572111122E+04
 0.80387753276422E+03 0.15333906092069E+04 0.14243715209348E+04 0.14252865630455E+04 0.24890278246437E+04
 0.12963277230962E+04 0.14195527535166E+04 0.12147384699860E+04 0.24852373709709E+04 0.15333906092069E+04
 0.14243715209349E+04 0.14252865630455E+04 0.24890278246438E+04 0.12963277230962E+04 0.14195527535166E+04
 0.12147384699860E+04 0.24852373709710E+04 0.22987656198657E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50554561309475E+03 0.18908918274757E+01
 0.18908918274757E+01 0.18034935090476E+01 0.47233063897252E+00 0.29540588037676E+03 0.30307456384195E+03
 0.30065979440115E+03 0.30061719496744E+03 0.22999951683771E+00 0.00000000000000E+00 0.22694183519701E+00
 0.00000000000000E+00 0.86938425537297E+01 0.99886470115663E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29717246298975E+03 0.39754896000722E+03
 0.29530056547630E+03 0.29585920573819E+03 0.29515000000001E+03 0.29515000000002E+03 0.29530100186102E+03
 0.29585736641555E+03 0.29515000000001E+03 0.29515000000002E+03 0.29530056547630E+03 0.29585920573819E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29530100186102E+03 0.29585736641555E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29576971792339E+03 0.29515000000008E+03 -.54362319133312E+01 -.93206052062050E+01
 0.96670350554171E+01 0.52741389553668E+02 0.43026019322974E+02 0.10511116485727E+02 0.55358485305196E+01
 0.10610255784987E+02 0.38404601427159E+02 0.10485488809693E+02 0.53813841834998E+01 0.10584107855923E+02
 0.38260158755749E+02 0.10511116485728E+02 0.55358485305166E+01 0.10610255784988E+02 0.38404601427153E+02
 0.10485488809683E+02 0.53813841835031E+01 0.10584107855911E+02 0.38260158755755E+02 0.19303881282070E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31186244467202E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91636318758961E+00 0.90000336294359E+00 0.00000000000000E+00 0.91636318758961E+00 0.90000336294359E+00
 0.13725528509825E+01 0.34255287959272E+00 0.10299999713898E+01 0.15449999570847E+01 0.13397048826778E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16128600129628E-01 0.13397048826778E-01 0.16128600129628E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    280.10487920
 0.68589396682757E+00 0.30041703939808E+03 0.47628530407191E+03 0.43607631017317E+03 0.42006249367738E+03
 0.22999999145671E+00 0.00000000000000E+00 0.19344989198186E+00 0.00000000000000E+00 -.16563885717768E+01
 0.98245140292029E-03 0.48615427081259E+00 0.80000000000000E+04 0.30000000000000E+04 0.16455681828380E+02
 0.61708806856424E+01 0.32907625815074E+03 0.29515000001220E+03 0.32607257180958E+03 0.34971464242366E+03
 0.29515000000045E+03 0.29515000000069E+03 0.32109243066045E+03 0.34961419729665E+03 0.29515000000035E+03
 0.29515000000068E+03 0.32607257180958E+03 0.34971464242367E+03 0.29515000000045E+03 0.29515000000069E+03
 0.32109243066045E+03 0.34961419729665E+03 0.29515000000035E+03 0.29515000000068E+03 0.39895838815884E+03
 0.29748410918662E+03 0.24568268340003E+04 0.23016704199491E+04 0.89705627135290E+03 0.16942531202737E+04
 0.79271156756401E+03 0.15370250672812E+04 0.14265324351860E+04 0.14259516050244E+04 0.24848880040162E+04
 0.13006905620391E+04 0.14217174874007E+04 0.12167287027982E+04 0.24811120579665E+04 0.15370250672812E+04
 0.14265324351860E+04 0.14259516050244E+04 0.24848880040162E+04 0.13006905620391E+04 0.14217174874007E+04
 0.12167287027982E+04 0.24811120579665E+04 0.22925114213272E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50602190181945E+03 0.18908918646269E+01
 0.18908918646269E+01 0.18815747455116E+01 0.45474752064148E+00 0.29547889255080E+03 0.30318468242951E+03
 0.30084855653489E+03 0.30080643572062E+03 0.22999951683771E+00 0.00000000000000E+00 0.22682811724637E+00
 0.00000000000000E+00 0.87109086030154E+01 0.99861805216142E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29748442197438E+03 0.39899987303522E+03
 0.29531305603617E+03 0.29588652744341E+03 0.29515000000001E+03 0.29515000000002E+03 0.29531337480685E+03
 0.29588449421537E+03 0.29515000000001E+03 0.29515000000002E+03 0.29531305603617E+03 0.29588652744341E+03
 0.29515000000001E+03 0.29515000000002E+03 0.29531337480686E+03 0.29588449421537E+03 0.29515000000001E+03
 0.29515000000002E+03 0.29579782424196E+03 0.29515000000012E+03 -.69104235616512E+01 -.11509195591827E+02
 0.10516491790965E+02 0.54211968899886E+02 0.43642894649967E+02 0.11315407426649E+02 0.60815576421846E+01
 0.11493058701364E+02 0.39278103100474E+02 0.11277336783164E+02 0.59111540794871E+01 0.11454554444596E+02
 0.39118769371897E+02 0.11315407426649E+02 0.60815576421825E+01 0.11493058701364E+02 0.39278103100470E+02
 0.11277336783159E+02 0.59111540794897E+01 0.11454554444590E+02 0.39118769371902E+02 0.19490336218583E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31209390297569E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91676746581409E+00 0.90057920006859E+00 0.00000000000000E+00 0.91676746581409E+00 0.90057920006859E+00
 0.13729469548036E+01 0.34294698341378E+00 0.10299999713898E+01 0.15449999570847E+01 0.13404109461239E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16106358987829E-01 0.13404109461239E-01 0.16106358987829E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    293.11841861
 0.68691113336840E+00 0.30062232461867E+03 0.47707092373402E+03 0.43666942133416E+03 0.42055175837641E+03
 0.22999999150531E+00 0.00000000000000E+00 0.19343004277744E+00 0.00000000000000E+00 -.16613412625822E+01
 0.98178047111726E-03 0.48561885921399E+00 0.80000000000000E+04 0.30000000000000E+04 0.16473824787095E+02
 0.61776842951604E+01 0.33007185445661E+03 0.29515000002261E+03 0.32697661861302E+03 0.35120800862730E+03
 0.29515000000087E+03 0.29515000000136E+03 0.32189299410163E+03 0.35110593807988E+03 0.29515000000067E+03
 0.29515000000135E+03 0.32697661861302E+03 0.35120800862730E+03 0.29515000000087E+03 0.29515000000136E+03
 0.32189299410163E+03 0.35110593807988E+03 0.29515000000067E+03 0.29515000000135E+03 0.40080773595239E+03
 0.29793608785796E+03 0.24630858754033E+04 0.23023475739907E+04 0.88738305241981E+03 0.16705517946646E+04
 0.77873182698271E+03 0.15416377995874E+04 0.14290987268139E+04 0.14266869023088E+04 0.24793441334552E+04
 0.13062563337760E+04 0.14242881824947E+04 0.12191895687042E+04 0.24755867178699E+04 0.15416377995874E+04
 0.14290987268139E+04 0.14266869023088E+04 0.24793441334552E+04 0.13062563337760E+04 0.14242881824947E+04
 0.12191895687042E+04 0.24755867178699E+04 0.22843880570309E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50663980652505E+03 0.18908919127657E+01
 0.18908919127657E+01 0.19856830607969E+01 0.43262024690349E+00 0.29560089891921E+03 0.30332523368408E+03
 0.30109743127532E+03 0.30105631640637E+03 0.22999951683771E+00 0.00000000000000E+00 0.22667786970702E+00
 0.00000000000000E+00 0.87333109312661E+01 0.99820610312134E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29793648378635E+03 0.40084650929083E+03
 0.29533149338438E+03 0.29592361820940E+03 0.29515000000002E+03 0.29515000000003E+03 0.29533163287047E+03
 0.29592129728811E+03 0.29515000000002E+03 0.29515000000003E+03 0.29533149338438E+03 0.29592361820940E+03
 0.29515000000002E+03 0.29515000000003E+03 0.29533163287047E+03 0.29592129728811E+03 0.29515000000002E+03
 0.29515000000003E+03 0.29583604301878E+03 0.29515000000022E+03 -.90657305934191E+01 -.14697568931134E+02
 0.11750548612448E+02 0.56224157294151E+02 0.44414855938640E+02 0.12471033861169E+02 0.68754683989100E+01
 0.12803282195190E+02 0.40483151663194E+02 0.12415498059967E+02 0.66818919975847E+01 0.12747527428346E+02
 0.40302200440031E+02 0.12471033861171E+02 0.68754683989085E+01 0.12803282195191E+02 0.40483151663192E+02
 0.12415498059962E+02 0.66818919975857E+01 0.12747527428340E+02 0.40302200440033E+02 0.19698971168349E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31238384424510E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91729320404287E+00 0.90131545086584E+00 0.00000000000000E+00 0.91729320404287E+00 0.90131545086584E+00
 0.13734555380740E+01 0.34345556668420E+00 0.10299999713898E+01 0.15449999570847E+01 0.13409656716980E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16081265123845E-01 0.13409656716980E-01 0.16081265123845E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    302.57979209
 0.68762899592530E+00 0.30076716758686E+03 0.47762489503409E+03 0.43708739451870E+03 0.42089634357224E+03
 0.22999999300552E+00 0.00000000000000E+00 0.19342374990704E+00 0.00000000000000E+00 -.16649323010597E+01
 0.98130763229860E-03 0.48514004724384E+00 0.80000000000000E+04 0.30000000000000E+04 0.16490083730357E+02
 0.61837813988837E+01 0.33077646759437E+03 0.29515000003514E+03 0.32761692219740E+03 0.35226403015897E+03
 0.29515000000142E+03 0.29515000000223E+03 0.32246017153320E+03 0.35216078314469E+03 0.29515000000110E+03
 0.29515000000222E+03 0.32761692219740E+03 0.35226403015897E+03 0.29515000000142E+03 0.29515000000223E+03
 0.32246017153320E+03 0.35216078314469E+03 0.29515000000110E+03 0.29515000000222E+03 0.40211071162864E+03
 0.29829490363799E+03 0.24674192075341E+04 0.23027021241064E+04 0.88034006859562E+03 0.16536747374993E+04
 0.76893296856066E+03 0.15448490342373E+04 0.14307556509363E+04 0.14271325453807E+04 0.24752681191098E+04
 0.13101511291740E+04 0.14259475913557E+04 0.12208674834324E+04 0.24715234070178E+04 0.15448490342373E+04
 0.14307556509363E+04 0.14271325453807E+04 0.24752681191098E+04 0.13101511291740E+04 0.14259475913558E+04
 0.12208674834324E+04 0.24715234070178E+04 0.22785061957317E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50707541261915E+03 0.18908919476697E+01
 0.18908919476697E+01 0.20613740486184E+01 0.41739899524016E+00 0.29570922328616E+03 0.30342329756055E+03
 0.30127673299299E+03 0.30123660948506E+03 0.22999951683771E+00 0.00000000000000E+00 0.22656957618334E+00
 0.00000000000000E+00 0.87490811055332E+01 0.99784059505128E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29829535648211E+03 0.40214757105889E+03
 0.29534585027777E+03 0.29595070604860E+03 0.29515000000002E+03 0.29515000000004E+03 0.29534584856140E+03
 0.29594816099010E+03 0.29515000000002E+03 0.29515000000004E+03 0.29534585027777E+03 0.29595070604860E+03
 0.29515000000002E+03 0.29515000000004E+03 0.29534584856140E+03 0.29594816099010E+03 0.29515000000002E+03
 0.29515000000004E+03 0.29586389577352E+03 0.29515000000034E+03 -.10798150647161E+02 -.17246949428370E+02
 0.12723759103159E+02 0.57733125216240E+02 0.44945747317565E+02 0.13372663329606E+02 0.75018519664008E+01
 0.13862102976508E+02 0.41393048208135E+02 0.13303446310154E+02 0.72899623224467E+01 0.13792888903639E+02
 0.41194995509371E+02 0.13372663329606E+02 0.75018519663996E+01 0.13862102976507E+02 0.41393048208133E+02
 0.13303446310148E+02 0.72899623224512E+01 0.13792888903631E+02 0.41194995509380E+02 0.19816854721179E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31258234870857E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91762561092302E+00 0.90187940258338E+00 0.00000000000000E+00 0.91762561092302E+00 0.90187940258338E+00
 0.13738144693524E+01 0.34381449796265E+00 0.10299999713898E+01 0.15449999570847E+01 0.13411634772730E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16063887679801E-01 0.13411634772730E-01 0.16063887679801E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    314.99576934
 0.68853972991224E+00 0.30095144414974E+03 0.47833038476101E+03 0.43761956880748E+03 0.42133485867305E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19342330923775E+00 0.00000000000000E+00 -.16692369495469E+01
 0.98070672320742E-03 0.48443131692728E+00 0.80000000000000E+04 0.30000000000000E+04 0.16514208971343E+02
 0.61928283642536E+01 0.33167981356656E+03 0.29515000006122E+03 0.32843852516094E+03 0.35361318321719E+03
 0.29515000000265E+03 0.29515000000419E+03 0.32318918179766E+03 0.35350841258601E+03 0.29515000000204E+03
 0.29515000000416E+03 0.32843852516094E+03 0.35361318321719E+03 0.29515000000265E+03 0.29515000000419E+03
 0.32318918179766E+03 0.35350841258600E+03 0.29515000000204E+03 0.29515000000416E+03 0.40373913291011E+03
 0.29879586760049E+03 0.24728069818022E+04 0.23029483956995E+04 0.87152344383536E+03 0.16327679885971E+04
 0.75688692754254E+03 0.15488693667241E+04 0.14326603450268E+04 0.14275755602710E+04 0.24698473185679E+04
 0.13150521260320E+04 0.14278560670594E+04 0.12228918779670E+04 0.24661195925890E+04 0.15488693667241E+04
 0.14326603450268E+04 0.14275755602709E+04 0.24698473185679E+04 0.13150521260320E+04 0.14278560670594E+04
 0.12228918779670E+04 0.24661195925891E+04 0.22709872733550E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50763013354112E+03 0.18908919895098E+01
 0.18908919895098E+01 0.21607018666187E+01 0.39843412940974E+00 0.29587921188840E+03 0.30354713935804E+03
 0.30151036335421E+03 0.30147186460072E+03 0.22999951683771E+00 0.00000000000000E+00 0.22642861262472E+00
 0.00000000000000E+00 0.87696953342199E+01 0.99726751835542E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29879640199844E+03 0.40377349785369E+03
 0.29536688220536E+03 0.29598722441467E+03 0.29515000000002E+03 0.29515000000006E+03 0.29536667130272E+03
 0.29598435139842E+03 0.29515000000002E+03 0.29515000000006E+03 0.29536688220536E+03 0.29598722441467E+03
 0.29515000000002E+03 0.29515000000006E+03 0.29536667130272E+03 0.29598435139842E+03 0.29515000000002E+03
 0.29515000000006E+03 0.29590145814062E+03 0.29515000000059E+03 -.13227767115231E+02 -.20795532050034E+02
 0.14102417191109E+02 0.59772376257453E+02 0.45599446980389E+02 0.14639732586700E+02 0.83902522626516E+01
 0.15404257740895E+02 0.42630386284122E+02 0.14552320282728E+02 0.81524299027088E+01 0.15317247112944E+02
 0.42408173887191E+02 0.14639732586699E+02 0.83902522626509E+01 0.15404257740894E+02 0.42630386284121E+02
 0.14552320282720E+02 0.81524299027140E+01 0.15317247112933E+02 0.42408173887202E+02 0.19951638762652E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31282757620501E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91812136682428E+00 0.90248284278435E+00 0.00000000000000E+00 0.91812136682428E+00 0.90248284278435E+00
 0.13742698363459E+01 0.34426986495612E+00 0.10299999713898E+01 0.15449999570847E+01 0.13410366514763E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16045431923147E-01 0.13410366514763E-01 0.16045431923147E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    321.20375796
 0.68897947416369E+00 0.30104301171970E+03 0.47867676081109E+03 0.43788142513019E+03 0.42155098984821E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19342570585443E+00 0.00000000000000E+00 -.16714105755531E+01
 0.98040840295358E-03 0.48404950664814E+00 0.80000000000000E+04 0.30000000000000E+04 0.16527235107411E+02
 0.61977131652790E+01 0.33212304766793E+03 0.29515000007967E+03 0.32884189691950E+03 0.35427316788388E+03
 0.29515000000355E+03 0.29515000000564E+03 0.32354756933353E+03 0.35416764575538E+03 0.29515000000274E+03
 0.29515000000561E+03 0.32884189691951E+03 0.35427316788388E+03 0.29515000000355E+03 0.29515000000564E+03
 0.32354756933353E+03 0.35416764575538E+03 0.29515000000274E+03 0.29515000000561E+03 0.40452295997045E+03
 0.29905791440044E+03 0.24754153136345E+04 0.23030308049860E+04 0.86729849947808E+03 0.16228210493817E+04
 0.75118605740619E+03 0.15508217851526E+04 0.14335440610220E+04 0.14277703113668E+04 0.24671669119678E+04
 0.13174380703515E+04 0.14287418597268E+04 0.12238622426950E+04 0.24634477621570E+04 0.15508217851526E+04
 0.14335440610220E+04 0.14277703113668E+04 0.24671669119678E+04 0.13174380703515E+04 0.14287418597268E+04
 0.12238622426950E+04 0.24634477621570E+04 0.22673533271244E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50790296026122E+03 0.18908920106369E+01
 0.18908920106369E+01 0.22103657756189E+01 0.38935193575049E+00 0.29597739977417E+03 0.30360705644157E+03
 0.30162664204653E+03 0.30158909364140E+03 0.22999951683771E+00 0.00000000000000E+00 0.22635860418967E+00
 0.00000000000000E+00 0.87798616903878E+01 0.99693678372559E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29905849766053E+03 0.40455606195868E+03
 0.29537836716323E+03 0.29600585154710E+03 0.29515000000002E+03 0.29515000000008E+03 0.29537804330635E+03
 0.29600280096431E+03 0.29515000000002E+03 0.29515000000008E+03 0.29537836716323E+03 0.29600585154710E+03
 0.29515000000002E+03 0.29515000000008E+03 0.29537804330635E+03 0.29600280096431E+03 0.29515000000002E+03
 0.29515000000008E+03 0.29592067700689E+03 0.29515000000077E+03 -.14505400909694E+02 -.22642648043792E+02
 0.14825455603463E+02 0.60808056667439E+02 0.45908473785959E+02 0.15300854413699E+02 0.88571123948441E+01
 0.16237808544852E+02 0.43262258614587E+02 0.15204214491036E+02 0.86056787721748E+01 0.16141815497230E+02
 0.43027394829170E+02 0.15300854413699E+02 0.88571123948433E+01 0.16237808544851E+02 0.43262258614585E+02
 0.15204214491029E+02 0.86056787721790E+01 0.16141815497220E+02 0.43027394829178E+02 0.20010113177599E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31294432684850E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91836046516939E+00 0.90279011285361E+00 0.00000000000000E+00 0.91836046516939E+00 0.90279011285361E+00
 0.13744897084716E+01 0.34448973708185E+00 0.10299999713898E+01 0.15449999570847E+01 0.13408607397168E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16036340727416E-01 0.13408607397168E-01 0.16036340727416E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    333.61973521
 0.68983494106181E+00 0.30122482931333E+03 0.47935632454215E+03 0.43839588137134E+03 0.42197615926667E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19343447297890E+00 0.00000000000000E+00 -.16757459619574E+01
 0.97981659231508E-03 0.48324829509799E+00 0.80000000000000E+04 0.30000000000000E+04 0.16554636780204E+02
 0.62079887925764E+01 0.33299399394939E+03 0.29515000013161E+03 0.32963496470936E+03 0.35556675622395E+03
 0.29515000000622E+03 0.29515000000996E+03 0.32425300875015E+03 0.35545975173336E+03 0.29515000000482E+03
 0.29515000000991E+03 0.32963496470936E+03 0.35556675622395E+03 0.29515000000622E+03 0.29515000000996E+03
 0.32425300875015E+03 0.35545975173336E+03 0.29515000000482E+03 0.29515000000991E+03 0.40603997116982E+03
 0.29960460077969E+03 0.24804732889760E+04 0.23031175185215E+04 0.85911197528957E+03 0.16037013069781E+04
 0.74029377181209E+03 0.15546207299878E+04 0.14351766695804E+04 0.14281095376388E+04 0.24618350137259E+04
 0.13220922352660E+04 0.14303789381850E+04 0.12257260786749E+04 0.24581331229947E+04 0.15546207299878E+04
 0.14351766695804E+04 0.14281095376388E+04 0.24618350137259E+04 0.13220922352660E+04 0.14303789381850E+04
 0.12257260786749E+04 0.24581331229947E+04 0.22602644829729E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50843889649754E+03 0.18908920527757E+01
 0.18908920527757E+01 0.23096935936193E+01 0.37192803332401E+00 0.29620207659419E+03 0.30372316480018E+03
 0.30185829577024E+03 0.30182290738476E+03 0.22999951683771E+00 0.00000000000000E+00 0.22621949808921E+00
 0.00000000000000E+00 0.87998814614224E+01 0.99618077865772E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29960528810342E+03 0.40607059988910E+03
 0.29540332450809E+03 0.29604379014843E+03 0.29515000000003E+03 0.29515000000012E+03 0.29540276016110E+03
 0.29604035853683E+03 0.29515000000003E+03 0.29515000000012E+03 0.29540332450809E+03 0.29604379014843E+03
 0.29515000000003E+03 0.29515000000012E+03 0.29540276016110E+03 0.29604035853683E+03 0.29515000000003E+03
 0.29515000000012E+03 0.29595988471361E+03 0.29515000000130E+03 -.17185398659890E+02 -.26469486919715E+02
 0.16338980774355E+02 0.62914170035787E+02 0.46493494357561E+02 0.16677704919559E+02 0.98359994783951E+01
 0.18040886150982E+02 0.44553458174654E+02 0.16562410613928E+02 0.95560508302308E+01 0.17926822992119E+02
 0.44292138759789E+02 0.16677704919559E+02 0.98359994783944E+01 0.18040886150982E+02 0.44553458174652E+02
 0.16562410613923E+02 0.95560508302337E+01 0.17926822992112E+02 0.44292138759795E+02 0.20113630814620E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31316680408181E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91882644322878E+00 0.90340058659851E+00 0.00000000000000E+00 0.91882644322878E+00 0.90340058659851E+00
 0.13749174419207E+01 0.34491747053090E+00 0.10299999713898E+01 0.15449999570847E+01 0.13403099340665E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16017315243733E-01 0.13403099340665E-01 0.16017315243733E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    346.03571246
 0.69066200189981E+00 0.30140391350824E+03 0.48001750747458E+03 0.43889696668281E+03 0.42239073295067E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19344726303482E+00 0.00000000000000E+00 -.16800067170051E+01
 0.97923437687551E-03 0.48241468747052E+00 0.80000000000000E+04 0.30000000000000E+04 0.16583243022610E+02
 0.62187161334787E+01 0.33384570070501E+03 0.29515000021092E+03 0.33041107741503E+03 0.35682773963846E+03
 0.29515000001056E+03 0.29515000001699E+03 0.32494438176487E+03 0.35671928059804E+03 0.29515000000818E+03
 0.29515000001691E+03 0.33041107741503E+03 0.35682773963846E+03 0.29515000001056E+03 0.29515000001699E+03
 0.32494438176487E+03 0.35671928059804E+03 0.29515000000818E+03 0.29515000001691E+03 0.40749535368681E+03
 0.30017999588711E+03 0.24853238240014E+04 0.23030884646032E+04 0.85122596328030E+03 0.15854017154983E+04
 0.72991962240165E+03 0.15582824778666E+04 0.14366240950690E+04 0.14283740944173E+04 0.24572287853764E+04
 0.13265948960436E+04 0.14318313633345E+04 0.12274830277299E+04 0.24535450441751E+04 0.15582824778666E+04
 0.14366240950690E+04 0.14283740944173E+04 0.24572287853764E+04 0.13265948960436E+04 0.14318313633345E+04
 0.12274830277299E+04 0.24535450441751E+04 0.22536763599608E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50868661210906E+03 0.18908920941892E+01
 0.18908920941892E+01 0.24090214116196E+01 0.35541903727102E+00 0.29646638837391E+03 0.30383467906207E+03
 0.30208879187359E+03 0.30205587544780E+03 0.22999951683771E+00 0.00000000000000E+00 0.22608155988257E+00
 0.00000000000000E+00 0.88194276713900E+01 0.99529283523821E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30018078966686E+03 0.40752362161808E+03
 0.29543110685252E+03 0.29608265650517E+03 0.29515000000004E+03 0.29515000000020E+03 0.29543028396044E+03
 0.29607880925159E+03 0.29515000000004E+03 0.29515000000020E+03 0.29543110685252E+03 0.29608265650517E+03
 0.29515000000004E+03 0.29515000000020E+03 0.29543028396044E+03 0.29607880925159E+03 0.29515000000004E+03
 0.29515000000020E+03 0.29600009185051E+03 0.29515000000211E+03 -.20026462687867E+02 -.30450893428119E+02
 0.17939827265731E+02 0.65066680140488E+02 0.47037153738428E+02 0.18124400263184E+02 0.10873375650162E+02
 0.20037173728189E+02 0.45880747942447E+02 0.17990301760606E+02 0.10563242689297E+02 0.19905020201233E+02
 0.45591490076714E+02 0.18124400263184E+02 0.10873375650162E+02 0.20037173728189E+02 0.45880747942447E+02
 0.17990301760601E+02 0.10563242689300E+02 0.19905020201226E+02 0.45591490076719E+02 0.20204709626267E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31337542762708E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91928050267171E+00 0.90399233257568E+00 0.00000000000000E+00 0.91928050267171E+00 0.90399233257568E+00
 0.13753309723397E+01 0.34533100094990E+00 0.10299999713898E+01 0.15449999570847E+01 0.13395303164694E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15995742041100E-01 0.13395303164694E-01 0.15995742041100E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    352.24370109
 0.69106823203135E+00 0.30149246690744E+03 0.48033967034900E+03 0.43914113012029E+03 0.42259291650887E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19345478601159E+00 0.00000000000000E+00 -.16821868729720E+01
 0.97894673834445E-03 0.48199228071836E+00 0.80000000000000E+04 0.30000000000000E+04 0.16597776188608E+02
 0.62241660707279E+01 0.33426472839777E+03 0.29515000026429E+03 0.33079311181512E+03 0.35744771212778E+03
 0.29515000001359E+03 0.29515000002194E+03 0.32528506854663E+03 0.35733853781862E+03 0.29515000001055E+03
 0.29515000002184E+03 0.33079311181512E+03 0.35744771212778E+03 0.29515000001359E+03 0.29515000002194E+03
 0.32528506854663E+03 0.35733853781862E+03 0.29515000001055E+03 0.29515000002184E+03 0.40820109186808E+03
 0.30047786365550E+03 0.24876574561572E+04 0.23030149067064E+04 0.84737261957012E+03 0.15765751728892E+04
 0.72496569022121E+03 0.15600546346125E+04 0.14372498678417E+04 0.14284699362038E+04 0.24544311438520E+04
 0.13287827071502E+04 0.14324599482740E+04 0.12283138799084E+04 0.24507562802905E+04 0.15600546346125E+04
 0.14372498678417E+04 0.14284699362038E+04 0.24544311438520E+04 0.13287827071502E+04 0.14324599482740E+04
 0.12283138799084E+04 0.24507562802904E+04 0.22502139457397E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50896738152213E+03 0.18908921153797E+01
 0.18908921153797E+01 0.24586853206198E+01 0.34748278138557E+00 0.29661405551519E+03 0.30388882795937E+03
 0.30220358918414E+03 0.30217201117153E+03 0.22999951683771E+00 0.00000000000000E+00 0.22601301252226E+00
 0.00000000000000E+00 0.88288626301187E+01 0.99479742862469E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30047871119007E+03 0.40822822178423E+03
 0.29544613491011E+03 0.29610244214430E+03 0.29515000000004E+03 0.29515000000026E+03 0.29544517657673E+03
 0.29609837403321E+03 0.29515000000004E+03 0.29515000000026E+03 0.29544613491011E+03 0.29610244214430E+03
 0.29515000000004E+03 0.29515000000026E+03 0.29544517657673E+03 0.29609837403321E+03 0.29515000000004E+03
 0.29515000000026E+03 0.29602057360284E+03 0.29515000000266E+03 -.21505037842464E+02 -.32490644012456E+02
 0.18771782180774E+02 0.66159827636932E+02 0.47294186545254E+02 0.18872636282694E+02 0.11413267863167E+02
 0.21112095368052E+02 0.46557535494654E+02 0.18729124324632E+02 0.11087439858922E+02 0.20970934994674E+02
 0.46253775657406E+02 0.18872636282693E+02 0.11413267863167E+02 0.21112095368050E+02 0.46557535494654E+02
 0.18729124324628E+02 0.11087439858924E+02 0.20970934994668E+02 0.46253775657411E+02 0.20247563117163E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31347477201409E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91947326521704E+00 0.90431842578485E+00 0.00000000000000E+00 0.91947326521704E+00 0.90431842578485E+00
 0.13755340874054E+01 0.34553411601568E+00 0.10299999713898E+01 0.15449999570847E+01 0.13390678772157E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15983463510290E-01 0.13390678772157E-01 0.15983463510290E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    364.65967834
 0.69185735150805E+00 0.30166783398337E+03 0.48097048214649E+03 0.43961986372099E+03 0.42298984949684E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19347149873498E+00 0.00000000000000E+00 -.16862484125263E+01
 0.97837761306899E-03 0.48114062476393E+00 0.80000000000000E+04 0.30000000000000E+04 0.16627155530517E+02
 0.62351833239439E+01 0.33508983205535E+03 0.29515000040723E+03 0.33154574335747E+03 0.35866566229252E+03
 0.29515000002207E+03 0.29515000003583E+03 0.32595694127057E+03 0.35855508080258E+03 0.29515000001716E+03
 0.29515000003566E+03 0.33154574335747E+03 0.35866566229252E+03 0.29515000002207E+03 0.29515000003583E+03
 0.32595694127057E+03 0.35855508080258E+03 0.29515000001716E+03 0.29515000003566E+03 0.40957187035820E+03
 0.30109277731313E+03 0.24921876284507E+04 0.23027940673263E+04 0.83986619750829E+03 0.15594956935764E+04
 0.71543016508060E+03 0.15635073526257E+04 0.14383736944245E+04 0.14286133054177E+04 0.24488253947053E+04
 0.13330570489366E+04 0.14335899081128E+04 0.12299053870906E+04 0.24451685535399E+04 0.15635073526258E+04
 0.14383736944245E+04 0.14286133054177E+04 0.24488253947054E+04 0.13330570489366E+04 0.14335899081127E+04
 0.12299053870906E+04 0.24451685535399E+04 0.22434030890136E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50951742858042E+03 0.18908921548569E+01
 0.18908921548569E+01 0.25580131386202E+01 0.33220173545008E+00 0.29694139714825E+03 0.30399411130263E+03
 0.30243216204815E+03 0.30240343105989E+03 0.22999951683771E+00 0.00000000000000E+00 0.22587673095837E+00
 0.00000000000000E+00 0.88475847864407E+01 0.99370096958435E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30109373359218E+03 0.40959680663930E+03
 0.29547862564500E+03 0.29614272625089E+03 0.29515000000006E+03 0.29515000000041E+03 0.29547738531783E+03
 0.29613819031303E+03 0.29515000000006E+03 0.29515000000041E+03 0.29547862564500E+03 0.29614272625089E+03
 0.29515000000006E+03 0.29515000000041E+03 0.29547738531783E+03 0.29613819031303E+03 0.29515000000006E+03
 0.29515000000041E+03 0.29606229758306E+03 0.29515000000417E+03 -.24573734113159E+02 -.36651562817982E+02
 0.20496197643340E+02 0.68378727263067E+02 0.47780048631510E+02 0.20416081845061E+02 0.12533896279785E+02
 0.23423585928546E+02 0.47936489918947E+02 0.20253802119988E+02 0.12175521929806E+02 0.23264571597851E+02
 0.47602710206535E+02 0.20416081845060E+02 0.12533896279785E+02 0.23423585928545E+02 0.47936489918947E+02
 0.20253802119984E+02 0.12175521929808E+02 0.23264571597845E+02 0.47602710206540E+02 0.20331411616757E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31366417658639E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91992832307345E+00 0.90484714353335E+00 0.00000000000000E+00 0.91992832307345E+00 0.90484714353335E+00
 0.13759286471438E+01 0.34592867575402E+00 0.10299999713898E+01 0.15449999570847E+01 0.13380211577582E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15955137877036E-01 0.13380211577582E-01 0.15955137877036E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    370.86766696
 0.69223691643631E+00 0.30175468305176E+03 0.48128156073944E+03 0.43985651666345E+03 0.42318631707964E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19348047595365E+00 0.00000000000000E+00 -.16883057812987E+01
 0.97809600305427E-03 0.48071174053508E+00 0.80000000000000E+04 0.30000000000000E+04 0.16641990043961E+02
 0.62407462664854E+01 0.33549624169250E+03 0.29515000050110E+03 0.33191662721795E+03 0.35926333459235E+03
 0.29515000002785E+03 0.29515000004533E+03 0.32628837266710E+03 0.35915206033991E+03 0.29515000002167E+03
 0.29515000004512E+03 0.33191662721795E+03 0.35926333459235E+03 0.29515000002785E+03 0.29515000004533E+03
 0.32628837266710E+03 0.35915206033991E+03 0.29515000002167E+03 0.29515000004512E+03 0.41023853855504E+03
 0.30140936716046E+03 0.24944144017702E+04 0.23026753660090E+04 0.83623135964046E+03 0.15512672267576E+04
 0.71085471031899E+03 0.15652053112764E+04 0.14389142996322E+04 0.14286770677501E+04 0.24460841130529E+04
 0.13351607771307E+04 0.14341337643626E+04 0.12306823390977E+04 0.24424363597106E+04 0.15652053112764E+04
 0.14389142996322E+04 0.14286770677501E+04 0.24460841130529E+04 0.13351607771307E+04 0.14341337643626E+04
 0.12306823390977E+04 0.24424363597106E+04 0.22401011540608E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.50978883966435E+03 0.18908921748540E+01
 0.18908921748540E+01 0.26076770476203E+01 0.32484031350011E+00 0.29712133944892E+03 0.30404532709203E+03
 0.30254586688092E+03 0.30251862878144E+03 0.22999951683771E+00 0.00000000000000E+00 0.22580898541398E+00
 0.00000000000000E+00 0.88567858190144E+01 0.99309925569391E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30141037844955E+03 0.41026242104494E+03
 0.29549615429526E+03 0.29616322837127E+03 0.29515000000007E+03 0.29515000000051E+03 0.29549476789168E+03
 0.29615844544855E+03 0.29515000000007E+03 0.29515000000051E+03 0.29549615429526E+03 0.29616322837127E+03
 0.29515000000007E+03 0.29515000000051E+03 0.29549476789168E+03 0.29615844544855E+03 0.29515000000007E+03
 0.29515000000051E+03 0.29608354108508E+03 0.29515000000517E+03 -.26162011238150E+02 -.38766510795997E+02
 0.21387853121141E+02 0.69504254754718E+02 0.48009462367971E+02 0.21210340148812E+02 0.13114135230621E+02
 0.24663315458333E+02 0.48638447161268E+02 0.21038740213168E+02 0.12738925580169E+02 0.24495488287620E+02
 0.48289165572065E+02 0.21210340148812E+02 0.13114135230621E+02 0.24663315458333E+02 0.48638447161268E+02
 0.21038740213164E+02 0.12738925580171E+02 0.24495488287614E+02 0.48289165572069E+02 0.20373776544867E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31375452313025E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92014819847845E+00 0.90512035068832E+00 0.00000000000000E+00 0.92014819847845E+00 0.90512035068832E+00
 0.13761184296079E+01 0.34611845821816E+00 0.10299999713898E+01 0.15449999570847E+01 0.13374442523334E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15938885293119E-01 0.13374442523334E-01 0.15938885293119E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    383.28364421
 0.69297501578932E+00 0.30192670347255E+03 0.48189419558542E+03 0.44032320368926E+03 0.42357409629630E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19349924122660E+00 0.00000000000000E+00 -.16924220147682E+01
 0.97753870091658E-03 0.47985441479505E+00 0.80000000000000E+04 0.30000000000000E+04 0.16671723242177E+02
 0.62518962158165E+01 0.33629752843401E+03 0.29515000074664E+03 0.33264817999173E+03 0.36043837269859E+03
 0.29515000004356E+03 0.29515000007125E+03 0.32694278263934E+03 0.36032573528374E+03 0.29515000003397E+03
 0.29515000007092E+03 0.33264817999173E+03 0.36043837269859E+03 0.29515000004356E+03 0.29515000007125E+03
 0.32694278263934E+03 0.36032573528374E+03 0.29515000003397E+03 0.29515000007092E+03 0.41153726660144E+03
 0.30205968997975E+03 0.24987817419490E+04 0.23024073925616E+04 0.82916568652186E+03 0.15353527977963E+04
 0.70204128284181E+03 0.15685411164308E+04 0.14399323859324E+04 0.14287825047210E+04 0.24406724841962E+04
 0.13392987499283E+04 0.14351587889367E+04 0.12321936760139E+04 0.24370431156298E+04 0.15685411164308E+04
 0.14399323859324E+04 0.14287825047210E+04 0.24406724841962E+04 0.13392987499283E+04 0.14351587889367E+04
 0.12321936760139E+04 0.24370431156298E+04 0.22336547060519E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51032355481835E+03 0.18908922148627E+01
 0.18908922148627E+01 0.27070048656207E+01 0.31063724422761E+00 0.29751414876311E+03 0.30414510081399E+03
 0.30277188703287E+03 0.30274773078020E+03 0.22999951683771E+00 0.00000000000000E+00 0.22567425806029E+00
 0.00000000000000E+00 0.88748279762105E+01 0.99178823877443E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30206081248360E+03 0.41155912835259E+03
 0.29553394610635E+03 0.29620496178659E+03 0.29515000000011E+03 0.29515000000080E+03 0.29553225859727E+03
 0.29619965872107E+03 0.29515000000011E+03 0.29515000000080E+03 0.29553394610635E+03 0.29620496178659E+03
 0.29515000000011E+03 0.29515000000080E+03 0.29553225859727E+03 0.29619965872107E+03 0.29515000000011E+03
 0.29515000000080E+03 0.29612679616067E+03 0.29515000000782E+03 -.29441897479280E+02 -.43051146142504E+02
 0.23227880144117E+02 0.71786537282052E+02 0.48442517737215E+02 0.22841375541325E+02 0.14313114528808E+02
 0.27317733195353E+02 0.50066602444370E+02 0.22651341499671E+02 0.13903153431933E+02 0.27132587129595E+02
 0.49685376598555E+02 0.22841375541324E+02 0.14313114528807E+02 0.27317733195351E+02 0.50066602444369E+02
 0.22651341499668E+02 0.13903153431935E+02 0.27132587129591E+02 0.49685376598559E+02 0.20462271069345E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31392691034544E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92057518129215E+00 0.90567101617212E+00 0.00000000000000E+00 0.92057518129215E+00 0.90567101617212E+00
 0.13764874792844E+01 0.34648750789466E+00 0.10299999713898E+01 0.15449999570847E+01 0.13362033638106E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15901551073592E-01 0.13362033638106E-01 0.15901551073592E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    395.69962146
 0.69369107547297E+00 0.30209660971069E+03 0.48249261131691E+03 0.44077957919516E+03 0.42395373588835E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19351870750877E+00 0.00000000000000E+00 -.16964925320091E+01
 0.97698887120740E-03 0.47900413999831E+00 0.80000000000000E+04 0.30000000000000E+04 0.16701317028342E+02
 0.62629938856282E+01 0.33708417111721E+03 0.29515000109064E+03 0.33336675494316E+03 0.36158824759711E+03
 0.29515000006666E+03 0.29515000010951E+03 0.32758645827935E+03 0.36147427795859E+03 0.29515000005207E+03
 0.29515000010902E+03 0.33336675494316E+03 0.36158824759712E+03 0.29515000006666E+03 0.29515000010951E+03
 0.32758645827935E+03 0.36147427795859E+03 0.29515000005207E+03 0.29515000010902E+03 0.41279230546548E+03
 0.30273141383881E+03 0.25030176583344E+04 0.23020791747350E+04 0.82233586693685E+03 0.15200754146919E+04
 0.69362786842034E+03 0.15717886345378E+04 0.14408387310981E+04 0.14288483827233E+04 0.24352941450889E+04
 0.13433367908281E+04 0.14360727147715E+04 0.12336395694503E+04 0.24316834860085E+04 0.15717886345378E+04
 0.14408387310981E+04 0.14288483827233E+04 0.24352941450889E+04 0.13433367908281E+04 0.14360727147715E+04
 0.12336395694503E+04 0.24316834860085E+04 0.22273617473051E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51084609135705E+03 0.18908922544271E+01
 0.18908922544271E+01 0.28063326836211E+01 0.29708051978521E+00 0.29795093056208E+03 0.30424155243887E+03
 0.30299567162758E+03 0.30297466489940E+03 0.22999951683771E+00 0.00000000000000E+00 0.22554051555334E+00
 0.00000000000000E+00 0.88923378687199E+01 0.99033449588463E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30273264887100E+03 0.41281225673027E+03
 0.29557560635130E+03 0.29624767667822E+03 0.29515000000016E+03 0.29515000000122E+03 0.29557360740909E+03
 0.29624181860579E+03 0.29515000000016E+03 0.29515000000122E+03 0.29557560635130E+03 0.29624767667822E+03
 0.29515000000016E+03 0.29515000000122E+03 0.29557360740909E+03 0.29624181860579E+03 0.29515000000016E+03
 0.29515000000122E+03 0.29617108049756E+03 0.29515000001158E+03 -.32853755037420E+02 -.47392128291558E+02
 0.25140334316288E+02 0.74108685584655E+02 0.48842649596785E+02 0.24525409044763E+02 0.15561440775061E+02
 0.30213089769083E+02 0.51525790045458E+02 0.24317310198814E+02 0.15115350752767E+02 0.30011126165854E+02
 0.51111429104987E+02 0.24525409044763E+02 0.15561440775061E+02 0.30213089769084E+02 0.51525790045458E+02
 0.24317310198809E+02 0.15115350752771E+02 0.30011126165847E+02 0.51111429104994E+02 0.20558717764806E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31408868032565E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92099004745465E+00 0.90621223921590E+00 0.00000000000000E+00 0.92099004745465E+00 0.90621223921590E+00
 0.13768455091263E+01 0.34684553773648E+00 0.10299999713898E+01 0.15449999570847E+01 0.13348703711827E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15857080524366E-01 0.13348703711827E-01 0.15857080524366E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    401.90761009
 0.69404061376089E+00 0.30218082707065E+03 0.48278668731595E+03 0.44100408661813E+03 0.42414067512443E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19352859634494E+00 0.00000000000000E+00 -.16985143926136E+01
 0.97671656649290E-03 0.47858284501131E+00 0.80000000000000E+04 0.30000000000000E+04 0.16716019145674E+02
 0.62685071796277E+01 0.33747224885171E+03 0.29515000130912E+03 0.33372139273526E+03 0.36215411834638E+03
 0.29515000008184E+03 0.29515000013472E+03 0.32790445101099E+03 0.36203949430117E+03 0.29515000006397E+03
 0.29515000013412E+03 0.33372139273526E+03 0.36215411834638E+03 0.29515000008184E+03 0.29515000013472E+03
 0.32790445101099E+03 0.36203949430117E+03 0.29515000006397E+03 0.29515000013412E+03 0.41340434830918E+03
 0.30307476941310E+03 0.25050895772651E+04 0.23018948395095E+04 0.81900558524128E+03 0.15126619898263E+04
 0.68956137665880E+03 0.15733812758666E+04 0.14412531032589E+04 0.14288678815161E+04 0.24326182649408E+04
 0.13453204819540E+04 0.14364911138498E+04 0.12343397224204E+04 0.24290170773619E+04 0.15733812758665E+04
 0.14412531032589E+04 0.14288678815161E+04 0.24326182649408E+04 0.13453204819540E+04 0.14364911138498E+04
 0.12343397224204E+04 0.24290170773619E+04 0.22242707207515E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51110297885228E+03 0.18908922740791E+01
 0.18908922740791E+01 0.28559965926212E+01 0.29052833824035E+00 0.29818570196681E+03 0.30428860643980E+03
 0.30310656197648E+03 0.30308713211004E+03 0.22999951683771E+00 0.00000000000000E+00 0.22547400188612E+00
 0.00000000000000E+00 0.89008920716825E+01 0.98955485654646E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30307606105457E+03 0.41342338398770E+03
 0.29559796892219E+03 0.29626940470596E+03 0.29515000000019E+03 0.29515000000150E+03 0.29559581098800E+03
 0.29626325609095E+03 0.29515000000019E+03 0.29515000000149E+03 0.29559796892219E+03 0.29626940470596E+03
 0.29515000000019E+03 0.29515000000150E+03 0.29559581098800E+03 0.29626325609095E+03 0.29515000000019E+03
 0.29515000000149E+03 0.29619360965734E+03 0.29515000001400E+03 -.34607038732784E+02 -.49578272922309E+02
 0.26122574354447E+02 0.75284054255957E+02 0.49030867029737E+02 0.25385952148975E+02 0.16203391210829E+02
 0.31753523398046E+02 0.52266541912631E+02 0.25168993130875E+02 0.15738743137371E+02 0.31543371681595E+02
 0.51835189518142E+02 0.25385952148975E+02 0.16203391210830E+02 0.31753523398047E+02 0.52266541912632E+02
 0.25168993130871E+02 0.15738743137375E+02 0.31543371681588E+02 0.51835189518150E+02 0.20610806375492E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31416576526659E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92119315604464E+00 0.90648003782585E+00 0.00000000000000E+00 0.92119315604464E+00 0.90648003782585E+00
 0.13770202782702E+01 0.34702030688044E+00 0.10299999713898E+01 0.15449999570847E+01 0.13341764844653E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15832009641368E-01 0.13341764844653E-01 0.15832009641368E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    414.32358734
 0.69472314716544E+00 0.30234786662901E+03 0.48336498861938E+03 0.44144606039272E+03 0.42450905895091E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19354851096838E+00 0.00000000000000E+00 -.17025329854268E+01
 0.97617691690466E-03 0.47774977666079E+00 0.80000000000000E+04 0.30000000000000E+04 0.16745167430356E+02
 0.62794377863834E+01 0.33823839576171E+03 0.29515000186215E+03 0.33442178575938E+03 0.36326853082066E+03
 0.29515000012158E+03 0.29515000020095E+03 0.32853309541405E+03 0.36315262138822E+03 0.29515000009521E+03
 0.29515000020008E+03 0.33442178575938E+03 0.36326853082066E+03 0.29515000012158E+03 0.29515000020095E+03
 0.32853309541405E+03 0.36315262138822E+03 0.29515000009521E+03 0.29515000020008E+03 0.41459930691420E+03
 0.30377543825005E+03 0.25091471422224E+04 0.23014895009085E+04 0.81250529398955E+03 0.14982572726763E+04
 0.68168945221681E+03 0.15765081016181E+04 0.14420094402908E+04 0.14288824118741E+04 0.24272931986175E+04
 0.13492212772321E+04 0.14372559650948E+04 0.12356975282793E+04 0.24237111750685E+04 0.15765081016181E+04
 0.14420094402908E+04 0.14288824118740E+04 0.24272931986175E+04 0.13492212772321E+04 0.14372559650948E+04
 0.12356975282793E+04 0.24237111750685E+04 0.22181939227416E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51160835424843E+03 0.18908923131388E+01
 0.18908923131388E+01 0.29553244106216E+01 0.27765378226977E+00 0.29868759806838E+03 0.30437957556292E+03
 0.30332597617629E+03 0.30330967213677E+03 0.22999951683769E+00 0.00000000000000E+00 0.22534241787838E+00
 0.00000000000000E+00 0.89176121522466E+01 0.98789223304162E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30377684345149E+03 0.41461658941841E+03
 0.29564592173963E+03 0.29631361757705E+03 0.29515000000028E+03 0.29515000000222E+03 0.29564344069415E+03
 0.29630686183524E+03 0.29515000000028E+03 0.29515000000222E+03 0.29564592173963E+03 0.29631361757705E+03
 0.29515000000028E+03 0.29515000000222E+03 0.29564344069415E+03 0.29630686183523E+03 0.29515000000028E+03
 0.29515000000222E+03 0.29623942263654E+03 0.29515000002020E+03 -.38204487267419E+02 -.53972590787726E+02
 0.28137642088126E+02 0.77638261523885E+02 0.49359931225319E+02 0.27142871475030E+02 0.17522266149152E+02
 0.35026101551346E+02 0.53784723999440E+02 0.26908707009770E+02 0.17019521814492E+02 0.34800182397137E+02
 0.53318562899821E+02 0.27142871475027E+02 0.17522266149168E+02 0.35026101551343E+02 0.53784723999471E+02
 0.26908707009764E+02 0.17019521814546E+02 0.34800182397127E+02 0.53318562899928E+02 0.20722894676771E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31421343270758E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92159145538989E+00 0.90701010019243E+00 0.00000000000000E+00 0.92159145538989E+00 0.90701010019243E+00
 0.13773615449725E+01 0.34736157358272E+00 0.10299999713898E+01 0.15449999570847E+01 0.13327376591610E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15776238543388E-01 0.13327376591610E-01 0.15776238543388E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    420.53157596
 0.69505646614394E+00 0.30243071386327E+03 0.48364931569225E+03 0.44166359536332E+03 0.42469055853085E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19355847747968E+00 0.00000000000000E+00 -.17045290361519E+01
 0.97590948602123E-03 0.47733860672755E+00 0.80000000000000E+04 0.30000000000000E+04 0.16759591382823E+02
 0.62848467685587E+01 0.33861664017213E+03 0.29515000220753E+03 0.33476769388349E+03 0.36381738783563E+03
 0.29515000014720E+03 0.29515000024376E+03 0.32884387490697E+03 0.36370084742180E+03 0.29515000011537E+03
 0.29515000024271E+03 0.33476769388349E+03 0.36381738783563E+03 0.29515000014720E+03 0.29515000024376E+03
 0.32884387490697E+03 0.36370084742180E+03 0.29515000011537E+03 0.29515000024271E+03 0.41518288209447E+03
 0.30413235426582E+03 0.25111341638330E+04 0.23012691138632E+04 0.80933131623761E+03 0.14912544726760E+04
 0.67787649985722E+03 0.15780432563848E+04 0.14423523049980E+04 0.14288778995965E+04 0.24246424064063E+04
 0.13511394702475E+04 0.14376033126600E+04 0.12363558974163E+04 0.24210700710490E+04 0.15780432563848E+04
 0.14423523049980E+04 0.14288778995965E+04 0.24246424064063E+04 0.13511394702475E+04 0.14376033126600E+04
 0.12363558974163E+04 0.24210700710490E+04 0.22152047772684E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51185692946539E+03 0.18908923325399E+01
 0.18908923325399E+01 0.30049883196218E+01 0.27130207375255E+00 0.29895462498971E+03 0.30442350104770E+03
 0.30343435610395E+03 0.30341958299037E+03 0.22999951683777E+00 0.00000000000000E+00 0.22527742447954E+00
 0.00000000000000E+00 0.89257798752540E+01 0.98700992513333E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30413381625327E+03 0.41519932652562E+03
 0.29567157828692E+03 0.29633611612343E+03 0.29515000000034E+03 0.29515000000269E+03 0.29566893381720E+03
 0.29632904380191E+03 0.29515000000034E+03 0.29515000000269E+03 0.29567157828692E+03 0.29633611612343E+03
 0.29515000000034E+03 0.29515000000269E+03 0.29566893381720E+03 0.29632904380191E+03 0.29515000000034E+03
 0.29515000000269E+03 0.29626268885215E+03 0.29515000002410E+03 -.40047110440512E+02 -.56176948108493E+02
 0.29169806206458E+02 0.78828927797909E+02 0.49513272560419E+02 0.28038411431199E+02 0.18198698582975E+02
 0.36760437373477E+02 0.54548112805408E+02 0.27795948859501E+02 0.17676430469479E+02 0.36526984194402E+02
 0.54064138291762E+02 0.28038411431199E+02 0.18198698582968E+02 0.36760437373477E+02 0.54548112805394E+02
 0.27795948859495E+02 0.17676430469467E+02 0.36526984194391E+02 0.54064138291739E+02 0.20783102735665E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31424961956194E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92178697034075E+00 0.90727205045214E+00 0.00000000000000E+00 0.92178697034075E+00 0.90727205045214E+00
 0.13775282044617E+01 0.34752823307197E+00 0.10299999713898E+01 0.15449999570847E+01 0.13319953763629E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15745504216045E-01 0.13319953763629E-01 0.15745504216045E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    432.94755321
 0.69570755766734E+00 0.30259511999516E+03 0.48420869935114E+03 0.44209205277348E+03 0.42504841346025E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19357831978577E+00 0.00000000000000E+00 -.17084979741979E+01
 0.97537921661056E-03 0.47652796299896E+00 0.80000000000000E+04 0.30000000000000E+04 0.16788101897847E+02
 0.62955382116927E+01 0.33936387089940E+03 0.29515000306747E+03 0.33545128296783E+03 0.36489908838859E+03
 0.29515000021308E+03 0.29515000035412E+03 0.32945863975729E+03 0.36478130935361E+03 0.29515000016727E+03
 0.29515000035263E+03 0.33545128296783E+03 0.36489908838859E+03 0.29515000021308E+03 0.29515000035412E+03
 0.32945863975729E+03 0.36478130935361E+03 0.29515000016727E+03 0.29515000035263E+03 0.41632373436070E+03
 0.30485837804122E+03 0.25150293859401E+04 0.23007958826995E+04 0.80312830381834E+03 0.14776249597165E+04
 0.67048101437905E+03 0.15810600961708E+04 0.14429716563961E+04 0.14288471901162E+04 0.24193644201120E+04
 0.13549147416355E+04 0.14382320646906E+04 0.12376340161350E+04 0.24158116583020E+04 0.15810600961708E+04
 0.14429716563961E+04 0.14288471901162E+04 0.24193644201119E+04 0.13549147416355E+04 0.14382320646906E+04
 0.12376340161350E+04 0.24158116583020E+04 0.22093203854693E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51234618381931E+03 0.18908923711170E+01
 0.18908923711170E+01 0.31043161376221E+01 0.25881140350224E+00 0.29952026788528E+03 0.30450884049442E+03
 0.30364810750879E+03 0.30363627930490E+03 0.22999951683762E+00 0.00000000000000E+00 0.22514878566766E+00
 0.00000000000000E+00 0.89417286250305E+01 0.98514611571814E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30485995326255E+03 0.41633857797345E+03
 0.29572640827360E+03 0.29638185468940E+03 0.29515000000049E+03 0.29515000000390E+03 0.29572343460538E+03
 0.29637412325401E+03 0.29515000000049E+03 0.29515000000390E+03 0.29572640827360E+03 0.29638185468941E+03
 0.29515000000049E+03 0.29515000000390E+03 0.29572343460538E+03 0.29637412325402E+03 0.29515000000049E+03
 0.29515000000390E+03 0.29630995877778E+03 0.29515000003394E+03 -.43816136569136E+02 -.60592091666407E+02
 0.31280907015695E+02 0.81234004427724E+02 0.49796692876951E+02 0.29860774469109E+02 0.19584232091960E+02
 0.40428202230642E+02 0.56091231891518E+02 0.29602379268525E+02 0.19022011742499E+02 0.40180429930650E+02
 0.55570862941875E+02 0.29860774469099E+02 0.19584232091950E+02 0.40428202230627E+02 0.56091231891500E+02
 0.29602379268505E+02 0.19022011742456E+02 0.40180429930618E+02 0.55570862941790E+02 0.20913874308001E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31432070390676E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92217081948725E+00 0.90779071238724E+00 0.00000000000000E+00 0.92217081948725E+00 0.90779071238724E+00
 0.13778537502234E+01 0.34785377883367E+00 0.10299999713898E+01 0.15449999570847E+01 0.13304786687158E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15678116366246E-01 0.13304786687158E-01 0.15678116366246E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    445.36353046
 0.69633837350164E+00 0.30275787857530E+03 0.48475627451725E+03 0.44251211884721E+03 0.42539975632300E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19359793638915E+00 0.00000000000000E+00 -.17124397876070E+01
 0.97485482835752E-03 0.47573370224798E+00 0.80000000000000E+04 0.30000000000000E+04 0.16816130457434E+02
 0.63060489215377E+01 0.34009924999968E+03 0.29515000420267E+03 0.33612433308328E+03 0.36596027043093E+03
 0.29515000030363E+03 0.29515000050631E+03 0.33006470427141E+03 0.36584128366116E+03 0.29515000023874E+03
 0.29515000050422E+03 0.33612433308328E+03 0.36596027043093E+03 0.29515000030363E+03 0.29515000050631E+03
 0.33006470427141E+03 0.36584128366117E+03 0.29515000023874E+03 0.29515000050422E+03 0.41743140950628E+03
 0.30559944293943E+03 0.25188260101608E+04 0.23002837501405E+04 0.79710976781068E+03 0.14644695750253E+04
 0.66337425837555E+03 0.15840098154346E+04 0.14435088472818E+04 0.14287902910152E+04 0.24141200610755E+04
 0.13586129926272E+04 0.14387792089165E+04 0.12388640676141E+04 0.24105871118032E+04 0.15840098154347E+04
 0.14435088472818E+04 0.14287902910152E+04 0.24141200610755E+04 0.13586129926272E+04 0.14387792089164E+04
 0.12388640676141E+04 0.24105871118030E+04 0.22035569178233E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51282538990176E+03 0.18908924094304E+01
 0.18908924094304E+01 0.32036439556225E+01 0.24660878582844E+00 0.30012691864882E+03 0.30459128496908E+03
 0.30385731699726E+03 0.30384821646930E+03 0.22999951683754E+00 0.00000000000000E+00 0.22502180987047E+00
 0.00000000000000E+00 0.89571684836429E+01 0.98315497586452E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30560113035943E+03 0.41744475008886E+03
 0.29578613372706E+03 0.29642856789435E+03 0.29515000000070E+03 0.29515000000558E+03 0.29578282969473E+03
 0.29642014288966E+03 0.29515000000071E+03 0.29515000000557E+03 0.29578613372706E+03 0.29642856789435E+03
 0.29515000000070E+03 0.29515000000558E+03 0.29578282969473E+03 0.29642014288966E+03 0.29515000000071E+03
 0.29515000000557E+03 0.29635822790735E+03 0.29515000004711E+03 -.47691861467059E+02 -.65007858782412E+02
 0.33451501824882E+02 0.83670848030823E+02 0.50052088696817E+02 0.31721503751118E+02 0.21011676828239E+02
 0.44364416845676E+02 0.57656972276638E+02 0.31448148390598E+02 0.20408353187343E+02 0.44103403895880E+02
 0.57099239518763E+02 0.31721503751110E+02 0.21011676828255E+02 0.44364416845662E+02 0.57656972276669E+02
 0.31448148390598E+02 0.20408353187360E+02 0.44103403895881E+02 0.57099239518795E+02 0.21059522877520E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31439030181379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92254544991338E+00 0.90830333853082E+00 0.00000000000000E+00 0.92254544991338E+00 0.90830333853082E+00
 0.13781691581406E+01 0.34816918675082E+00 0.10299999713898E+01 0.15449999570847E+01 0.13289325769194E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15602759008290E-01 0.13289325769194E-01 0.15602759008290E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    451.57151909
 0.69664643104911E+00 0.30283866014858E+03 0.48502582299939E+03 0.44271914407173E+03 0.42557309557685E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19360762822069E+00 0.00000000000000E+00 -.17144013136714E+01
 0.97459476927823E-03 0.47534291241907E+00 0.80000000000000E+04 0.30000000000000E+04 0.16829955366931E+02
 0.63112332625992E+01 0.34046266779762E+03 0.29515000489487E+03 0.33645705707499E+03 0.36648346238933E+03
 0.29515000036045E+03 0.29515000060203E+03 0.33036459957274E+03 0.36636388321823E+03 0.29515000028364E+03
 0.29515000059957E+03 0.33645705707499E+03 0.36648346238932E+03 0.29515000036045E+03 0.29515000060203E+03
 0.33036459957274E+03 0.36636388321824E+03 0.29515000028364E+03 0.29515000059957E+03 0.41797346949108E+03
 0.30597516972221E+03 0.25206894472494E+04 0.23000144135226E+04 0.79416635921510E+03 0.14580597995788E+04
 0.65992260856761E+03 0.15854608801163E+04 0.14437486940424E+04 0.14287528597990E+04 0.24115110129784E+04
 0.13604347160402E+04 0.14390242289551E+04 0.12394621585790E+04 0.24079880504404E+04 0.15854608801163E+04
 0.14437486940424E+04 0.14287528597990E+04 0.24115110129785E+04 0.13604347160402E+04 0.14390242289550E+04
 0.12394621585790E+04 0.24079880504402E+04 0.22007187613503E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51306138696353E+03 0.18908924284960E+01
 0.18908924284960E+01 0.32533078646227E+01 0.24061016243986E+00 0.30044517503798E+03 0.30463153390344E+03
 0.30396001357901E+03 0.30395217513779E+03 0.22999951683757E+00 0.00000000000000E+00 0.22495892482813E+00
 0.00000000000000E+00 0.89647023712235E+01 0.98211360980776E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30597691266331E+03 0.41798609413550E+03
 0.29581790486526E+03 0.29645229270137E+03 0.29515000000083E+03 0.29515000000663E+03 0.29581443592386E+03
 0.29644350807216E+03 0.29515000000084E+03 0.29515000000662E+03 0.29581790486526E+03 0.29645229270137E+03
 0.29515000000083E+03 0.29515000000663E+03 0.29581443592386E+03 0.29644350807216E+03 0.29515000000084E+03
 0.29515000000662E+03 0.29638273742829E+03 0.29515000005521E+03 -.49667888109857E+02 -.67213359472171E+02
 0.34558102838139E+02 0.84900918828741E+02 0.50170025476411E+02 0.32665072879499E+02 0.21740486572684E+02
 0.46434189839651E+02 0.58448237528499E+02 0.32384638058199E+02 0.21116200118416E+02 0.46166993875875E+02
 0.57871479200854E+02 0.32665072879496E+02 0.21740486572690E+02 0.46434189839646E+02 0.58448237528511E+02
 0.32384638058201E+02 0.21116200118421E+02 0.46166993875878E+02 0.57871479200865E+02 0.21138092819668E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31442454581147E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92272942495830E+00 0.90855762189555E+00 0.00000000000000E+00 0.92272942495830E+00 0.90855762189555E+00
 0.13783231869143E+01 0.34832321552455E+00 0.10299999713898E+01 0.15449999570847E+01 0.13281514107875E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15562133810500E-01 0.13281514107875E-01 0.15562133810500E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    465.28132890
 0.69731058680697E+00 0.30301518045383E+03 0.48561090137290E+03 0.44316892493873E+03 0.42594999691253E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19362870167266E+00 0.00000000000000E+00 -.17187290079153E+01
 0.97402698192218E-03 0.47449567122199E+00 0.80000000000000E+04 0.30000000000000E+04 0.16860006287091E+02
 0.63225023576590E+01 0.34125540814986E+03 0.29515000679552E+03 0.33718305420190E+03 0.36762271564415E+03
 0.29515000052167E+03 0.29515000087429E+03 0.33101941393641E+03 0.36750185176188E+03 0.29515000041122E+03
 0.29515000087079E+03 0.33718305420190E+03 0.36762271564415E+03 0.29515000052167E+03 0.29515000087429E+03
 0.33101941393641E+03 0.36750185176187E+03 0.29515000041122E+03 0.29515000087079E+03 0.41914902449433E+03
 0.30681844213026E+03 0.25247291499831E+04 0.22993901733383E+04 0.78773481519666E+03 0.14441358744168E+04
 0.65246238514417E+03 0.15886137526939E+04 0.14442075756911E+04 0.14286500100158E+04 0.24057618676453E+04
 0.13643993174467E+04 0.14394948405969E+04 0.12407472951553E+04 0.24022610242872E+04 0.15886137526939E+04
 0.14442075756910E+04 0.14286500100157E+04 0.24057618676453E+04 0.13643993174468E+04 0.14394948405969E+04
 0.12407472951553E+04 0.24022610242873E+04 0.21945062509005E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51357380838564E+03 0.18908924705601E+01
 0.18908924705601E+01 0.33629863430901E+01 0.22760156128393E+00 0.30118213785476E+03 0.30471839269885E+03
 0.30418182128311E+03 0.30417648669717E+03 0.22999951683760E+00 0.00000000000000E+00 0.22482139010404E+00
 0.00000000000000E+00 0.89808836424889E+01 0.97971063171880E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30682030070301E+03 0.41916016677823E+03
 0.29589197179971E+03 0.29650529744623E+03 0.29515000000122E+03 0.29515000000963E+03 0.29588814269958E+03
 0.29649569445749E+03 0.29515000000123E+03 0.29515000000962E+03 0.29589197179971E+03 0.29650529744623E+03
 0.29515000000122E+03 0.29515000000963E+03 0.29588814269958E+03 0.29649569445749E+03 0.29515000000123E+03
 0.29515000000962E+03 0.29643746662001E+03 0.29515000007772E+03 -.54132889278467E+02 -.72098031402987E+02
 0.37053559059514E+02 0.87650097017399E+02 0.50411270162588E+02 0.34782722252706E+02 0.23386382028381E+02
 0.51255790447534E+02 0.60219002454209E+02 0.34487494159765E+02 0.22714774243139E+02 0.50975853421924E+02
 0.59599327039659E+02 0.34782722252706E+02 0.23386382028376E+02 0.51255790447533E+02 0.60219002454198E+02
 0.34487494159762E+02 0.22714774243126E+02 0.50975853421919E+02 0.59599327039633E+02 0.21322013223626E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31449882133342E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92311749116087E+00 0.90912711886246E+00 0.00000000000000E+00 0.92311749116087E+00 0.90912711886246E+00
 0.13786552647933E+01 0.34865529340348E+00 0.10299999713898E+01 0.15449999570847E+01 0.13263947228115E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15466148325425E-01 0.13263947228115E-01 0.15466148325425E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    472.78315008
 0.69766457515478E+00 0.30311047520836E+03 0.48592511128417E+03 0.44341067948087E+03 0.42615271182424E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19364002208617E+00 0.00000000000000E+00 -.17210410192853E+01
 0.97372073619303E-03 0.47404146421566E+00 0.80000000000000E+04 0.30000000000000E+04 0.16876160850690E+02
 0.63285603190087E+01 0.34168366912135E+03 0.29515000808923E+03 0.33757540546696E+03 0.36823651058210E+03
 0.29515000063501E+03 0.29515000106616E+03 0.33137369174468E+03 0.36811495928935E+03 0.29515000050103E+03
 0.29515000106193E+03 0.33757540546696E+03 0.36823651058211E+03 0.29515000063501E+03 0.29515000106616E+03
 0.33137369174468E+03 0.36811495928935E+03 0.29515000050103E+03 0.29515000106193E+03 0.41977666021965E+03
 0.30728581933978E+03 0.25268888724957E+04 0.22990217750273E+04 0.78430379417088E+03 0.14367356058103E+04
 0.64851029266860E+03 0.15903048175484E+04 0.14444152607282E+04 0.14285755714143E+04 0.24026226507335E+04
 0.13665297605342E+04 0.14397092263433E+04 0.12414219923675E+04 0.23991340354549E+04 0.15903048175484E+04
 0.14444152607281E+04 0.14285755714143E+04 0.24026226507334E+04 0.13665297605342E+04 0.14397092263433E+04
 0.12414219923675E+04 0.23991340354549E+04 0.21911555427060E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51384907198965E+03 0.18908924930322E+01
 0.18908924930322E+01 0.34230009125574E+01 0.22062153693584E+00 0.30160477501882E+03 0.30476484758171E+03
 0.30430006087127E+03 0.30429591370385E+03 0.22999951683760E+00 0.00000000000000E+00 0.22474687377107E+00
 0.00000000000000E+00 0.89895099730655E+01 0.97833785174387E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30728773979882E+03 0.41978703826810E+03
 0.29593527631001E+03 0.29653484220144E+03 0.29515000000149E+03 0.29515000001174E+03 0.29593125164166E+03
 0.29652477298273E+03 0.29515000000151E+03 0.29515000001173E+03 0.29593527631001E+03 0.29653484220144E+03
 0.29515000000149E+03 0.29515000001174E+03 0.29593125164166E+03 0.29652477298273E+03 0.29515000000151E+03
 0.29515000001173E+03 0.29646796004224E+03 0.29515000009322E+03 -.56622880908740E+02 -.74760398236624E+02
 0.38447581887168E+02 0.89171217715222E+02 0.50531397918618E+02 0.35958503078587E+02 0.24307271055076E+02
 0.54038430137033E+02 0.61200174334240E+02 0.35655845292815E+02 0.23609227909617E+02 0.53752230559841E+02
 0.60556570063641E+02 0.35958503078585E+02 0.24307271055073E+02 0.54038430137030E+02 0.61200174334234E+02
 0.35655845292812E+02 0.23609227909607E+02 0.53752230559834E+02 0.60556570063620E+02 0.21431717554098E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31453872324709E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92333350548599E+00 0.90942208724166E+00 0.00000000000000E+00 0.92333350548599E+00 0.90942208724166E+00
 0.13788322589672E+01 0.34883228757739E+00 0.10299999713898E+01 0.15449999570847E+01 0.13254262694093E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15409778015840E-01 0.13254262694093E-01 0.15409778015840E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    480.28497126
 0.69801131981082E+00 0.30320538023616E+03 0.48623590609366E+03 0.44365011312051E+03 0.42635369035970E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19365118086755E+00 0.00000000000000E+00 -.17233449872941E+01
 0.97341593424270E-03 0.47359327020183E+00 0.80000000000000E+04 0.30000000000000E+04 0.16892131927024E+02
 0.63345494726339E+01 0.34210814387516E+03 0.29515000958802E+03 0.33796438434307E+03 0.36884359813314E+03
 0.29515000076927E+03 0.29515000129379E+03 0.33172521846367E+03 0.36872137070226E+03 0.29515000060752E+03
 0.29515000128872E+03 0.33796438434307E+03 0.36884359813314E+03 0.29515000076927E+03 0.29515000129379E+03
 0.33172521846367E+03 0.36872137070226E+03 0.29515000060752E+03 0.29515000128872E+03 0.42039332345272E+03
 0.30775673299169E+03 0.25290209072494E+04 0.22986463272970E+04 0.78094786342257E+03 0.14295100854589E+04
 0.64465748271917E+03 0.15919765453765E+04 0.14446035745646E+04 0.14284960244709E+04 0.23995055659679E+04
 0.13686372837570E+04 0.14399044308436E+04 0.12420838484333E+04 0.23960292564210E+04 0.15919765453765E+04
 0.14446035745646E+04 0.14284960244709E+04 0.23995055659679E+04 0.13686372837570E+04 0.14399044308436E+04
 0.12420838484333E+04 0.23960292564210E+04 0.21878561295802E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51412146704995E+03 0.18908925154262E+01
 0.18908925154262E+01 0.34830154820247E+01 0.21373787811594E+00 0.30204053000391E+03 0.30481061768791E+03
 0.30441590257873E+03 0.30441280242602E+03 0.22999951683759E+00 0.00000000000000E+00 0.22467285371096E+00
 0.00000000000000E+00 0.89979809519989E+01 0.97692648181607E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30775871752530E+03 0.42040295833659E+03
 0.29598074021678E+03 0.29656478526315E+03 0.29515000000181E+03 0.29515000001425E+03 0.29597652179101E+03
 0.29655423665343E+03 0.29515000000184E+03 0.29515000001424E+03 0.29598074021678E+03 0.29656478526315E+03
 0.29515000000181E+03 0.29515000001425E+03 0.29597652179101E+03 0.29655423665343E+03 0.29515000000184E+03
 0.29515000001424E+03 0.29649886476374E+03 0.29515000011131E+03 -.59142872978733E+02 -.77411663000263E+02
 0.39858366538192E+02 0.90701120240379E+02 0.50643461869495E+02 0.37142463780180E+02 0.25240359869657E+02
 0.56919639214350E+02 0.62188165550732E+02 0.36832871903034E+02 0.24515567202878E+02 0.56627700031740E+02
 0.61520387048309E+02 0.37142463780177E+02 0.25240359869655E+02 0.56919639214346E+02 0.62188165550730E+02
 0.36832871903029E+02 0.24515567202872E+02 0.56627700031731E+02 0.61520387048296E+02 0.21546995742467E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31457814824547E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92354925039898E+00 0.90971328737525E+00 0.00000000000000E+00 0.92354925039898E+00 0.90971328737525E+00
 0.13790056312952E+01 0.34900565990541E+00 0.10299999713898E+01 0.15449999570847E+01 0.13244597300058E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15350641689995E-01 0.13244597300058E-01 0.15350641689995E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    495.28861363
 0.69868540565162E+00 0.30339437985213E+03 0.48684786836441E+03 0.44412244335128E+03 0.42675081006200E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19367298234586E+00 0.00000000000000E+00 -.17279987429583E+01
 0.97280950062617E-03 0.47271464458750E+00 0.80000000000000E+04 0.30000000000000E+04 0.16923529007613E+02
 0.63463233778548E+01 0.34294612820063E+03 0.29515001330760E+03 0.33873256124037E+03 0.37003857689516E+03
 0.29515000111368E+03 0.29515000187909E+03 0.33242024072528E+03 0.36991502978790E+03 0.29515000088107E+03
 0.29515000187188E+03 0.33873256124037E+03 0.37003857689516E+03 0.29515000111368E+03 0.29515000187909E+03
 0.33242024072527E+03 0.36991502978789E+03 0.29515000088107E+03 0.29515000187188E+03 0.42159737696014E+03
 0.30870870817370E+03 0.25332108920945E+04 0.22978840687514E+04 0.77442373645987E+03 0.14155077859043E+04
 0.63721193076212E+03 0.15952680062312E+04 0.14449299356362E+04 0.14283284835979E+04 0.23933413693312E+04
 0.13727902060266E+04 0.14402450324233E+04 0.12433770585256E+04 0.23898898063229E+04 0.15952680062312E+04
 0.14449299356362E+04 0.14283284835979E+04 0.23933413693312E+04 0.13727902060267E+04 0.14402450324234E+04
 0.12433770585257E+04 0.23898898063230E+04 0.21813975033084E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51465819629273E+03 0.18908925606596E+01
 0.18908925606596E+01 0.36030446209593E+01 0.20025480934985E+00 0.30294920421617E+03 0.30490031437242E+03
 0.30463983491084E+03 0.30463838253450E+03 0.22999951683758E+00 0.00000000000000E+00 0.22452622321896E+00
 0.00000000000000E+00 0.90144547073609E+01 0.97399641993513E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30871082038087E+03 0.42160560606714E+03
 0.29607815505806E+03 0.29662578546056E+03 0.29515000000266E+03 0.29515000002072E+03 0.29607355736501E+03
 0.29661424101657E+03 0.29515000000270E+03 0.29515000002070E+03 0.29607815505806E+03 0.29662578546056E+03
 0.29515000000266E+03 0.29515000002072E+03 0.29607355736501E+03 0.29661424101657E+03 0.29515000000270E+03
 0.29515000002070E+03 0.29656182224479E+03 0.29515000015673E+03 -.64272008094743E+02 -.82686976595902E+02
 0.42729713614395E+02 0.93788716849512E+02 0.50845354667045E+02 0.39533977591780E+02 0.27142788296156E+02
 0.62978492744446E+02 0.64185693319761E+02 0.39212042762317E+02 0.26363555583906E+02 0.62676666658702E+02
 0.63468818074850E+02 0.39533977591779E+02 0.27142788296154E+02 0.62978492744445E+02 0.64185693319756E+02
 0.39212042762316E+02 0.26363555583904E+02 0.62676666658700E+02 0.63468818074846E+02 0.21792400330413E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31465543931537E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92396088304823E+00 0.91031282855255E+00 0.00000000000000E+00 0.92396088304823E+00 0.91031282855255E+00
 0.13793426742156E+01 0.34934270282581E+00 0.10299999713898E+01 0.15449999570847E+01 0.13225049930487E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15225468593531E-01 0.13225049930487E-01 0.15225468593531E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    502.79043481
 0.69901401393489E+00 0.30348823785868E+03 0.48714891253482E+03 0.44435511739903E+03 0.42694668204841E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19368362028592E+00 0.00000000000000E+00 -.17303022974573E+01
 0.97250862391843E-03 0.47228435554572E+00 0.80000000000000E+04 0.30000000000000E+04 0.16938947703987E+02
 0.63521053889950E+01 0.34335986516364E+03 0.29515001558813E+03 0.33911196368835E+03 0.37062691930728E+03
 0.29515000133139E+03 0.29515000224985E+03 0.33276389865295E+03 0.37050272811309E+03 0.29515000105422E+03
 0.29515000224130E+03 0.33911196368835E+03 0.37062691930728E+03 0.29515000133139E+03 0.29515000224985E+03
 0.33276389865294E+03 0.37050272811308E+03 0.29515000105422E+03 0.29515000224130E+03 0.42218585036258E+03
 0.30918933703868E+03 0.25352670219991E+04 0.22974921191056E+04 0.77124112593390E+03 0.14087016174350E+04
 0.63360428587140E+03 0.15968870206180E+04 0.14450645189384E+04 0.14282372998375E+04 0.23902848723696E+04
 0.13748352003545E+04 0.14403869442948E+04 0.12440058963756E+04 0.23868457339483E+04 0.15968870206180E+04
 0.14450645189384E+04 0.14282372998375E+04 0.23902848723696E+04 0.13748352003545E+04 0.14403869442948E+04
 0.12440058963757E+04 0.23868457339484E+04 0.21782249737660E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51492237093249E+03 0.18908925830496E+01
 0.18908925830496E+01 0.36630591904266E+01 0.19365320356273E+00 0.30342109726983E+03 0.30494432533775E+03
 0.30474767334101E+03 0.30474681412302E+03 0.22999951683758E+00 0.00000000000000E+00 0.22445357503352E+00
 0.00000000000000E+00 0.90224833317798E+01 0.97248169745033E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30919151090683E+03 0.42219342105660E+03
 0.29613011874139E+03 0.29665683205047E+03 0.29515000000321E+03 0.29515000002482E+03 0.29612533648790E+03
 0.29664477159338E+03 0.29515000000325E+03 0.29515000002479E+03 0.29613011874139E+03 0.29665683205047E+03
 0.29515000000321E+03 0.29515000002482E+03 0.29612533648790E+03 0.29664477159338E+03 0.29515000000325E+03
 0.29515000002479E+03 0.29659385934101E+03 0.29515000018489E+03 -.66879424228188E+02 -.85311801579825E+02
 0.44189256238691E+02 0.95346068413958E+02 0.50935865894073E+02 0.40740486159681E+02 0.28111444409592E+02
 0.66156054575329E+02 0.65195031504939E+02 0.40413169558987E+02 0.27304542403456E+02 0.65850102458209E+02
 0.64453252398859E+02 0.40740486159680E+02 0.28111444409591E+02 0.66156054575328E+02 0.65195031504935E+02
 0.40413169558985E+02 0.27304542403453E+02 0.65850102458205E+02 0.64453252398853E+02 0.21921692981608E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31469330937065E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92416673801694E+00 0.91060462193313E+00 0.00000000000000E+00 0.92416673801694E+00 0.91060462193313E+00
 0.13795069783572E+01 0.34950700696745E+00 0.10299999713898E+01 0.15449999570847E+01 0.13215251728506E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15159432833132E-01 0.13215251728506E-01 0.15159432833132E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    510.29225600
 0.69933414088119E+00 0.30358156835793E+03 0.48744635828065E+03 0.44458538331435E+03 0.42714075620530E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19369415731236E+00 0.00000000000000E+00 -.17326182200340E+01
 0.97220962249688E-03 0.47185938918766E+00 0.80000000000000E+04 0.30000000000000E+04 0.16954203271811E+02
 0.63578262269290E+01 0.34377021289506E+03 0.29515001819354E+03 0.33948834286030E+03 0.37120936482527E+03
 0.29515000158523E+03 0.29515000268271E+03 0.33310506802075E+03 0.37108453976313E+03 0.29515000125628E+03
 0.29515000267262E+03 0.33948834286030E+03 0.37120936482527E+03 0.29515000158523E+03 0.29515000268271E+03
 0.33310506802075E+03 0.37108453976313E+03 0.29515000125628E+03 0.29515000267262E+03 0.42276568671129E+03
 0.30967273957655E+03 0.25372950258783E+04 0.22970892660831E+04 0.76810247333862E+03 0.14020080529009E+04
 0.63006506719563E+03 0.15984863788840E+04 0.14451741987905E+04 0.14281381804295E+04 0.23872346208174E+04
 0.13768575272528E+04 0.14405040840739E+04 0.12446202288943E+04 0.23838079360591E+04 0.15984863788840E+04
 0.14451741987904E+04 0.14281381804296E+04 0.23872346208174E+04 0.13768575272529E+04 0.14405040840740E+04
 0.12446202288944E+04 0.23838079360591E+04 0.21750804299659E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51518352793415E+03 0.18908926055598E+01
 0.18908926055598E+01 0.37230737598938E+01 0.18714354003531E+00 0.30390403892377E+03 0.30498782232143E+03
 0.30485260694698E+03 0.30485218509033E+03 0.22999951683758E+00 0.00000000000000E+00 0.22438134770533E+00
 0.00000000000000E+00 0.90303645448940E+01 0.97093637753952E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30967497340934E+03 0.42277262764516E+03
 0.29618428413947E+03 0.29668823995248E+03 0.29515000000385E+03 0.29515000002962E+03 0.29617932129748E+03
 0.29667565160061E+03 0.29515000000390E+03 0.29515000002958E+03 0.29618428413947E+03 0.29668823995248E+03
 0.29515000000385E+03 0.29515000002962E+03 0.29617932129748E+03 0.29667565160061E+03 0.29515000000390E+03
 0.29515000002958E+03 0.29662626384618E+03 0.29515000021730E+03 -.69514011100244E+02 -.87928190829453E+02
 0.45663890002796E+02 0.96911912870782E+02 0.51019703417972E+02 0.41953250833192E+02 0.29091186748190E+02
 0.69432093430284E+02 0.66211080713854E+02 0.41621098752867E+02 0.28256333380860E+02 0.69122576232494E+02
 0.65444176971742E+02 0.41953250833191E+02 0.29091186748188E+02 0.69432093430283E+02 0.66211080713850E+02
 0.41621098752863E+02 0.28256333380856E+02 0.69122576232487E+02 0.65444176971734E+02 0.22054795154437E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31473058111153E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92436682541114E+00 0.91090144020208E+00 0.00000000000000E+00 0.92436682541114E+00 0.91090144020208E+00
 0.13796670418304E+01 0.34966707044060E+00 0.10299999713898E+01 0.15449999570847E+01 0.13205311273365E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.26468416919039E-04
 0.00000000000000E+00 0.15065172284314E-01 0.13205311273365E-01 0.15091640701233E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    525.29589836
 0.69993959478787E+00 0.30376666249797E+03 0.48803039140517E+03 0.44503923149003E+03 0.42752420742253E+03
 0.22999999730504E+00 0.00000000000000E+00 0.19371510953370E+00 0.00000000000000E+00 -.17372530126286E+01
 0.97177736973331E-03 0.47102307176982E+00 0.80000000000000E+04 0.30000000000000E+04 0.16984306033972E+02
 0.63691147627394E+01 0.34458100280942E+03 0.29515002452980E+03 0.34023224973177E+03 0.37235695628830E+03
 0.29515000222153E+03 0.29515000376989E+03 0.33378011099223E+03 0.37223089353245E+03 0.29515000176345E+03
 0.29515000375598E+03 0.34023224973177E+03 0.37235695628830E+03 0.29515000222153E+03 0.29515000376989E+03
 0.33378011099223E+03 0.37223089353245E+03 0.29515000176345E+03 0.29515000375598E+03 0.42390032390716E+03
 0.31064693152154E+03 0.25412698248212E+04 0.22962527846757E+04 0.76194675783298E+03 0.13889367300729E+04
 0.62318023845080E+03 0.16016261998762E+04 0.14453155351305E+04 0.14279152306504E+04 0.23811443755451E+04
 0.13808344731214E+04 0.14406607110744E+04 0.12458050066790E+04 0.23777426685999E+04 0.16016261998762E+04
 0.14453155351305E+04 0.14279152306504E+04 0.23811443755451E+04 0.13808344731215E+04 0.14406607110744E+04
 0.12458050066790E+04 0.23777426686000E+04 0.21688683215570E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51569690795220E+03 0.18908926506088E+01
 0.18908926506088E+01 0.38431028988284E+01 0.17439611140101E+00 0.30490109445332E+03 0.30507337425460E+03
 0.30505334430299E+03 0.30505333430186E+03 0.22999951683758E+00 0.00000000000000E+00 0.22423808988851E+00
 0.00000000000000E+00 0.90457437046243E+01 0.10130270266633E-02 0.00000000000000E+00 0.78971239556660E+04
 0.29614214833748E+04 0.80000000000000E+04 0.30000000000000E+04 0.31064928048093E+03 0.42390608447541E+03
 0.29629935547982E+03 0.29675212946166E+03 0.29515000000548E+03 0.29515000004168E+03 0.29629404521242E+03
 0.29673845026883E+03 0.29515000000555E+03 0.29515000004164E+03 0.29629935547982E+03 0.29675212946167E+03
 0.29515000000548E+03 0.29515000004168E+03 0.29629404521243E+03 0.29673845026883E+03 0.29515000000555E+03
 0.29515000004164E+03 0.29669216297701E+03 0.29515000029700E+03 -.74860008339626E+02 -.93135972451817E+02
 0.48655682133468E+02 0.10006708590871E+03 0.51168125364571E+02 0.44394600053920E+02 0.31082129558232E+02
 0.76278374026726E+02 0.68262077450940E+02 0.44054478183002E+02 0.30190584744019E+02 0.75963459638042E+02
 0.67444315772178E+02 0.44394600053919E+02 0.31082129558230E+02 0.76278374026724E+02 0.68262077450936E+02
 0.44054478182998E+02 0.30190584744016E+02 0.75963459638034E+02 0.67444315772171E+02 0.22330576822575E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31480334081212E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92477155815057E+00 0.91148703784075E+00 0.00000000000000E+00 0.92477155815057E+00 0.91148703784075E+00
 0.13799697687837E+01 0.34996979739394E+00 0.10299999713898E+01 0.15449999570847E+01 0.13185203279615E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.92044801735005E-04
 0.00000000000000E+00 0.14858625720421E-01 0.13185203279615E-01 0.14950670522156E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    530.29604858
 0.70012789457928E+00 0.30383152589327E+03 0.48822664656746E+03 0.44519325736469E+03 0.42765537981053E+03
 0.22999794518263E+00 0.00000000000000E+00 0.19372196749731E+00 0.00000000000000E+00 -.17387821674405E+01
 0.97173539576745E-03 0.47074427931993E+00 0.80000000000000E+04 0.30000000000000E+04 0.16994364778171E+02
 0.63728867918140E+01 0.34484783117492E+03 0.29515002687356E+03 0.34047727561545E+03 0.37272927872295E+03
 0.29515000246052E+03 0.29515000417861E+03 0.33400368863819E+03 0.37260283067026E+03 0.29515000195406E+03
 0.29515000416327E+03 0.34047727561545E+03 0.37272927872295E+03 0.29515000246052E+03 0.29515000417861E+03
 0.33400368863819E+03 0.37260283067026E+03 0.29515000195406E+03 0.29515000416327E+03 0.42424840926185E+03
 0.31096357169264E+03 0.25425839856130E+04 0.22960128273560E+04 0.76034236080053E+03 0.13854031582772E+04
 0.62125908567263E+03 0.16026614005651E+04 0.14454207545527E+04 0.14278652342663E+04 0.23792919175286E+04
 0.13821371471747E+04 0.14407716147838E+04 0.12462019319852E+04 0.23758988762424E+04 0.16026614005651E+04
 0.14454207545527E+04 0.14278652342663E+04 0.23792919175286E+04 0.13821371471747E+04 0.14407716147838E+04
 0.12462019319852E+04 0.23758988762424E+04 0.21670916264409E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51587006488724E+03 0.18908926654718E+01
 0.18908926654718E+01 0.38831041005712E+01 0.17020404417597E+00 0.30519918256239E+03 0.30510670168022E+03
 0.30511719542699E+03 0.30511719260790E+03 0.22742736690278E+00 0.00000000000000E+00 0.22452092837447E+00
 0.00000000000000E+00 0.90510201039369E+01 0.10379779788916E-02 0.00000000000000E+00 0.77072926041676E+04
 0.28902347265629E+04 0.80000000000000E+04 0.30000000000000E+04 0.31096597726146E+03 0.42425373436463E+03
 0.29634614564216E+03 0.29677516965273E+03 0.29515000000609E+03 0.29515000004622E+03 0.29634072316303E+03
 0.29676108316634E+03 0.29515000000617E+03 0.29515000004617E+03 0.29634614564216E+03 0.29677516965273E+03
 0.29515000000609E+03 0.29515000004622E+03 0.29634072316303E+03 0.29676108316634E+03 0.29515000000617E+03
 0.29515000004617E+03 0.29671603114187E+03 0.29515000032665E+03 -.76591841790969E+02 -.94934042423360E+02
 0.49649542697581E+02 0.10113582351391E+03 0.51238033102841E+02 0.45174774351546E+02 0.31745632126386E+02
 0.78375535613678E+02 0.68962050751647E+02 0.44833401658033E+02 0.30835450831975E+02 0.78060199104190E+02
 0.68127836999891E+02 0.45174774351545E+02 0.31745632126385E+02 0.78375535613677E+02 0.68962050751644E+02
 0.44833401658029E+02 0.30835450831972E+02 0.78060199104182E+02 0.68127836999887E+02 0.22404444876081E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31483144809308E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92495318692274E+00 0.91163645903215E+00 0.00000000000000E+00 0.92495318692274E+00 0.91163645903215E+00
 0.13800639186794E+01 0.35006394728964E+00 0.10299999713898E+01 0.15449999570847E+01 0.13180044760828E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.11056207247752E-03
 0.00000000000000E+00 0.14796034851143E-01 0.13180044760828E-01 0.14906596923620E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    541.15336260
 0.70055643221021E+00 0.30396601478780E+03 0.48864320767754E+03 0.44551760922373E+03 0.42793004159244E+03
 0.22996789628526E+00 0.00000000000000E+00 0.19373253362513E+00 0.00000000000000E+00 -.17420987126354E+01
 0.97184395355839E-03 0.47015360217471E+00 0.80000000000000E+04 0.30000000000000E+04 0.17015715636328E+02
 0.63808933636229E+01 0.34542465730251E+03 0.29515003266909E+03 0.34100684002611E+03 0.37354027759643E+03
 0.29515000306276E+03 0.29515000520978E+03 0.33448552220471E+03 0.37341297552917E+03 0.29515000243479E+03
 0.29515000519089E+03 0.34100684002611E+03 0.37354027759643E+03 0.29515000306276E+03 0.29515000520978E+03
 0.33448552220471E+03 0.37341297552917E+03 0.29515000243479E+03 0.29515000519089E+03 0.42503704506283E+03
 0.31165199758380E+03 0.25454016257880E+04 0.22954159774892E+04 0.75618983172047E+03 0.13765786432989E+04
 0.61660786241983E+03 0.16048931981161E+04 0.14455211163083E+04 0.14277106949750E+04 0.23750367334422E+04
 0.13849617606347E+04 0.14408834756565E+04 0.12470386522485E+04 0.23716617981216E+04 0.16048931981161E+04
 0.14455211163083E+04 0.14277106949750E+04 0.23750367334422E+04 0.13849617606347E+04 0.14408834756566E+04
 0.12470386522485E+04 0.23716617981216E+04 0.21628231643274E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51623657468122E+03 0.18908926977078E+01
 0.18908926977078E+01 0.39699626126836E+01 0.16095629767450E+00 0.30538083159037E+03 0.30524259924130E+03
 0.30525743215272E+03 0.30525742615869E+03 0.22476564608080E+00 0.00000000000000E+00 0.22473884203322E+00
 0.00000000000000E+00 0.90654080188353E+01 0.10976149758194E-02 0.00000000000000E+00 0.72885302918065E+04
 0.27331988594274E+04 0.80000000000000E+04 0.30000000000000E+04 0.31165457007666E+03 0.42504150531258E+03
 0.29643216504009E+03 0.29682442363782E+03 0.29515000000766E+03 0.29515000005770E+03 0.29642651165367E+03
 0.29680949822302E+03 0.29515000000775E+03 0.29515000005763E+03 0.29643216504009E+03 0.29682442363782E+03
 0.29515000000766E+03 0.29515000005770E+03 0.29642651165367E+03 0.29680949822303E+03 0.29515000000775E+03
 0.29515000005763E+03 0.29676673275831E+03 0.29515000040049E+03 -.80399222386024E+02 -.10090752826298E+03
 0.51775688545293E+02 0.10388257850955E+03 0.51848011521526E+02 0.46920923204336E+02 0.33167132916524E+02
 0.80891069569376E+02 0.70799282374732E+02 0.46576658735285E+02 0.32216796121034E+02 0.80574254880941E+02
 0.69929530612845E+02 0.46920923204336E+02 0.33167132916524E+02 0.80891069569375E+02 0.70799282374731E+02
 0.46576658735281E+02 0.32216796121030E+02 0.80574254880933E+02 0.69929530612839E+02 0.22181413797259E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31493321708499E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92525530679725E+00 0.91203711483613E+00 0.00000000000000E+00 0.92525530679725E+00 0.91203711483613E+00
 0.13802781874949E+01 0.35027821610510E+00 0.10299999713898E+01 0.15449999570847E+01 0.13156593435594E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.11451745628962E-03
 0.00000000000000E+00 0.14833539653276E-01 0.13156593435594E-01 0.14948057109566E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    550.72718351
 0.70094827462735E+00 0.30407955527951E+03 0.48900308393541E+03 0.44579580781896E+03 0.42816434961178E+03
 0.22994859018795E+00 0.00000000000000E+00 0.19373727672859E+00 0.00000000000000E+00 -.17447397483831E+01
 0.97212073785325E-03 0.46964499825698E+00 0.80000000000000E+04 0.30000000000000E+04 0.17034142873214E+02
 0.63878035774554E+01 0.34592792800113E+03 0.29515003867534E+03 0.34146899089589E+03 0.37424708498318E+03
 0.29515000370179E+03 0.29515000630549E+03 0.33490621348152E+03 0.37411904221688E+03 0.29515000294540E+03
 0.29515000628286E+03 0.34146899089589E+03 0.37424708498318E+03 0.29515000370179E+03 0.29515000630549E+03
 0.33490621348152E+03 0.37411904221688E+03 0.29515000294540E+03 0.29515000628286E+03 0.42572371100627E+03
 0.31225083751290E+03 0.25478113661251E+04 0.22948071092336E+04 0.75250319739439E+03 0.13688004161358E+04
 0.61253470275442E+03 0.16068177444689E+04 0.14455426452355E+04 0.14275231402443E+04 0.23712405736413E+04
 0.13874063518203E+04 0.14409152170519E+04 0.12477196132450E+04 0.23678815486066E+04 0.16068177444689E+04
 0.14455426452355E+04 0.14275231402443E+04 0.23712405736413E+04 0.13874063518203E+04 0.14409152170519E+04
 0.12477196132450E+04 0.23678815486066E+04 0.21590102853925E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51655238758555E+03 0.18908927233780E+01
 0.18908927233780E+01 0.40465531800154E+01 0.15291609408333E+00 0.30552355613554E+03 0.30537020284223E+03
 0.30538583629999E+03 0.30538582925258E+03 0.22466361335340E+00 0.00000000000000E+00 0.22464598714160E+00
 0.00000000000000E+00 0.90780216826091E+01 0.11553266356805E-02 0.00000000000000E+00 0.69244486822446E+04
 0.25966682558417E+04 0.80000000000000E+04 0.30000000000000E+04 0.31225355075295E+03 0.42572760051245E+03
 0.29650411674182E+03 0.29686902474925E+03 0.29515000000934E+03 0.29515000006991E+03 0.29649827164450E+03
 0.29685335224484E+03 0.29515000000946E+03 0.29515000006982E+03 0.29650411674182E+03 0.29686902474925E+03
 0.29515000000934E+03 0.29515000006991E+03 0.29649827164450E+03 0.29685335224484E+03 0.29515000000946E+03
 0.29515000006982E+03 0.29681249693502E+03 0.29515000047770E+03 -.83726947371996E+02 -.10623417054811E+03
 0.53635361151659E+02 0.10633856921542E+03 0.52435031258006E+02 0.48478429778869E+02 0.34412571889046E+02
 0.83089687226193E+02 0.72446935827722E+02 0.48132697032387E+02 0.33427122923159E+02 0.82772616693679E+02
 0.71546205587740E+02 0.48478429778869E+02 0.34412571889045E+02 0.83089687226193E+02 0.72446935827720E+02
 0.48132697032382E+02 0.33427122923157E+02 0.82772616693671E+02 0.71546205587736E+02 0.21983678471523E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31503063737527E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92553121613472E+00 0.91233029471201E+00 0.00000000000000E+00 0.92553121613472E+00 0.91233029471201E+00
 0.13804741087034E+01 0.35047413731368E+00 0.10299999713898E+01 0.15449999570847E+01 0.13135658784881E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.11704238869337E-03
 0.00000000000000E+00 0.14871793268162E-01 0.13135658784881E-01 0.14988835656855E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    560.30100443
 0.70134362402328E+00 0.30419075676766E+03 0.48935748802837E+03 0.44606898591140E+03 0.42839398556071E+03
 0.22993572676258E+00 0.00000000000000E+00 0.19374023679322E+00 0.00000000000000E+00 -.17472779425294E+01
 0.97253375657385E-03 0.46914497425689E+00 0.80000000000000E+04 0.30000000000000E+04 0.17052298199873E+02
 0.63946118249522E+01 0.34642618843448E+03 0.29515004560819E+03 0.34192666443790E+03 0.37494553724146E+03
 0.29515000445521E+03 0.29515000759894E+03 0.33532314866391E+03 0.37481676783481E+03 0.29515000354798E+03
 0.29515000757195E+03 0.34192666443790E+03 0.37494553724146E+03 0.29515000445521E+03 0.29515000759894E+03
 0.33532314866391E+03 0.37481676783481E+03 0.29515000354798E+03 0.29515000757195E+03 0.42639940166599E+03
 0.31284426465410E+03 0.25501676286485E+04 0.22941576997333E+04 0.74885786800650E+03 0.13611393832658E+04
 0.60853722591927E+03 0.16087093311452E+04 0.14455239864625E+04 0.14273105452034E+04 0.23674447336195E+04
 0.13898140200240E+04 0.14409068874114E+04 0.12483671573575E+04 0.23641015961045E+04 0.16087093311452E+04
 0.14455239864625E+04 0.14273105452034E+04 0.23674447336195E+04 0.13898140200240E+04 0.14409068874114E+04
 0.12483671573575E+04 0.23641015961045E+04 0.21552118740578E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51686309790187E+03 0.18908927480486E+01
 0.18908927480486E+01 0.41231437473471E+01 0.14499335927592E+00 0.30567019413303E+03 0.30550232468885E+03
 0.30551855132527E+03 0.30551854327481E+03 0.22456867520275E+00 0.00000000000000E+00 0.22455271910810E+00
 0.00000000000000E+00 0.90903492371762E+01 0.12184560102962E-02 0.00000000000000E+00 0.65656863542044E+04
 0.24621323828266E+04 0.80000000000000E+04 0.30000000000000E+04 0.31284710931614E+03 0.42640271306280E+03
 0.29657517420257E+03 0.29691467625615E+03 0.29515000001134E+03 0.29515000008435E+03 0.29656914936287E+03
 0.29689824574918E+03 0.29515000001148E+03 0.29515000008424E+03 0.29657517420257E+03 0.29691467625615E+03
 0.29515000001134E+03 0.29515000008435E+03 0.29656914936287E+03 0.29689824574918E+03 0.29515000001148E+03
 0.29515000008424E+03 0.29685924869948E+03 0.29515000056755E+03 -.87039272243764E+02 -.11154640857525E+03
 0.55485753080003E+02 0.10881202127473E+03 0.53048839429327E+02 0.50042244456156E+02 0.35653760438921E+02
 0.85345986140688E+02 0.74109688140660E+02 0.49696127641088E+02 0.34633402904227E+02 0.85029723755012E+02
 0.73178259464245E+02 0.50042244456155E+02 0.35653760438919E+02 0.85345986140688E+02 0.74109688140656E+02
 0.49696127641084E+02 0.34633402904225E+02 0.85029723755005E+02 0.73178259464242E+02 0.21797855369633E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31513371458958E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92580190491453E+00 0.91260680916316E+00 0.00000000000000E+00 0.92580190491453E+00 0.91260680916316E+00
 0.13806717834014E+01 0.35067181201164E+00 0.10299999713898E+01 0.15449999570847E+01 0.13115462718074E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.12005640050151E-03
 0.00000000000000E+00 0.14906231138430E-01 0.13115462718074E-01 0.15026287538931E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    574.25600684
 0.70191784340231E+00 0.30434713765715E+03 0.48986196569515E+03 0.44645657635662E+03 0.42871891800308E+03
 0.22992393032358E+00 0.00000000000000E+00 0.19374405844839E+00 0.00000000000000E+00 -.17508243056412E+01
 0.97336319318659E-03 0.46843243903236E+00 0.80000000000000E+04 0.30000000000000E+04 0.17078236546823E+02
 0.64043387050587E+01 0.34714371582900E+03 0.29515005790659E+03 0.34258586070006E+03 0.37595159140113E+03
 0.29515000583242E+03 0.29515000996728E+03 0.33592360651300E+03 0.37582177604650E+03 0.29515000465088E+03
 0.29515000993242E+03 0.34258586070006E+03 0.37595159140113E+03 0.29515000583242E+03 0.29515000996728E+03
 0.33592360651300E+03 0.37582177604650E+03 0.29515000465088E+03 0.29515000993242E+03 0.42737775577008E+03
 0.31371258309116E+03 0.25535107527333E+04 0.22931284467858E+04 0.74338691396157E+03 0.13497730345240E+04
 0.60266918599264E+03 0.16114089710005E+04 0.14453854354785E+04 0.14269467145472E+04 0.23618227896193E+04
 0.13932639610670E+04 0.14407833366185E+04 0.12492516856362E+04 0.23585026110094E+04 0.16114089710005E+04
 0.14453854354785E+04 0.14269467145472E+04 0.23618227896193E+04 0.13932639610670E+04 0.14407833366185E+04
 0.12492516856362E+04 0.23585026110094E+04 0.21495602012176E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51730481642423E+03 0.18908927825184E+01
 0.18908927825184E+01 0.42347837666361E+01 0.13365522716856E+00 0.30588963140324E+03 0.30570206430512E+03
 0.30571877718719E+03 0.30571876785172E+03 0.22443121361902E+00 0.00000000000000E+00 0.22441740618990E+00
 0.00000000000000E+00 0.91075937210892E+01 0.13218190121991E-02 0.00000000000000E+00 0.60522657989996E+04
 0.22695996746249E+04 0.80000000000000E+04 0.30000000000000E+04 0.31371554735144E+03 0.42738029800867E+03
 0.29667597030532E+03 0.29698175323407E+03 0.29515000001507E+03 0.29515000011085E+03 0.29666970436112E+03
 0.29696421786931E+03 0.29515000001525E+03 0.29515000011072E+03 0.29667597030532E+03 0.29698175323407E+03
 0.29515000001507E+03 0.29515000011085E+03 0.29666970436112E+03 0.29696421786931E+03 0.29515000001525E+03
 0.29515000011072E+03 0.29692777232473E+03 0.29515000072881E+03 -.91925045468570E+02 -.11940643356222E+03
 0.58203814102134E+02 0.11249027509269E+03 0.53995441920047E+02 0.52364235765757E+02 0.37479298114593E+02
 0.88777207565005E+02 0.76586911531340E+02 0.52019100780992E+02 0.36407558238551E+02 0.88463635525597E+02
 0.75610271820971E+02 0.52364235765757E+02 0.37479298114592E+02 0.88777207565005E+02 0.76586911531337E+02
 0.52019100780988E+02 0.36407558238549E+02 0.88463635525588E+02 0.75610271820968E+02 0.21531947815412E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31529340625181E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92617312448152E+00 0.91299968564094E+00 0.00000000000000E+00 0.92617312448152E+00 0.91299968564094E+00
 0.13809588930909E+01 0.35095892170116E+00 0.10299999713898E+01 0.15449999570847E+01 0.13088212115121E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.12532365165764E-03
 0.00000000000000E+00 0.14947279232625E-01 0.13088212115121E-01 0.15072602884283E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    583.55934178
 0.70229280307164E+00 0.30445073453656E+03 0.49019380653169E+03 0.44671179897083E+03 0.42893308506751E+03
 0.22991875591151E+00 0.00000000000000E+00 0.19374714162628E+00 0.00000000000000E+00 -.17531281712680E+01
 0.97405567422701E-03 0.46796468523545E+00 0.80000000000000E+04 0.30000000000000E+04 0.17095307087061E+02
 0.64107401576481E+01 0.34761618633235E+03 0.29515006758339E+03 0.34302010262658E+03 0.37661135107005E+03
 0.29515000694391E+03 0.29515001188134E+03 0.33631979402388E+03 0.37648085959101E+03 0.29515000554200E+03
 0.29515001184021E+03 0.34302010262658E+03 0.37661135107005E+03 0.29515000694391E+03 0.29515001188134E+03
 0.33631979402388E+03 0.37648085959101E+03 0.29515000554200E+03 0.29515001184021E+03 0.42801096982045E+03
 0.31428234192070E+03 0.25556936812858E+04 0.22924298988797E+04 0.73991431708294E+03 0.13425519323681E+04
 0.59893804369972E+03 0.16131772222705E+04 0.14452765212872E+04 0.14266955011311E+04 0.23581339122144E+04
 0.13955251165762E+04 0.14406847076791E+04 0.12498178636474E+04 0.23548291232865E+04 0.16131772222705E+04
 0.14452765212872E+04 0.14266955011311E+04 0.23581339122144E+04 0.13955251165762E+04 0.14406847076791E+04
 0.12498178636474E+04 0.23548291232865E+04 0.21459024219758E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51759548746863E+03 0.18908928049114E+01
 0.18908928049114E+01 0.43092104461621E+01 0.12623373232957E+00 0.30604051304129E+03 0.30583982636544E+03
 0.30585671531752E+03 0.30585670517402E+03 0.22434016352799E+00 0.00000000000000E+00 0.22432762049221E+00
 0.00000000000000E+00 0.91190039231809E+01 0.13995309368970E-02 0.00000000000000E+00 0.57162008992367E+04
 0.21435753372138E+04 0.80000000000000E+04 0.30000000000000E+04 0.31428540342246E+03 0.42801298680140E+03
 0.29674435960561E+03 0.29702807896709E+03 0.29515000001811E+03 0.29515000013233E+03 0.29673794931715E+03
 0.29700978499295E+03 0.29515000001833E+03 0.29515000013216E+03 0.29674435960561E+03 0.29702807896709E+03
 0.29515000001811E+03 0.29515000013233E+03 0.29673794931715E+03 0.29700978499295E+03 0.29515000001833E+03
 0.29515000013216E+03 0.29697502518084E+03 0.29515000085698E+03 -.95143335463736E+02 -.12458631525732E+03
 0.60000623256871E+02 0.11495055561289E+03 0.54649929239735E+02 0.53906404206400E+02 0.38688561994402E+02
 0.91120119061608E+02 0.78247536158116E+02 0.53563515153150E+02 0.37582955591844E+02 0.90809900512754E+02
 0.77241283780953E+02 0.53906404206400E+02 0.38688561994401E+02 0.91120119061608E+02 0.78247536158114E+02
 0.53563515153146E+02 0.37582955591842E+02 0.90809900512745E+02 0.77241283780949E+02 0.21372948120275E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31540544852095E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92643159237964E+00 0.91323824731657E+00 0.00000000000000E+00 0.92643159237964E+00 0.91323824731657E+00
 0.13811463729256E+01 0.35114640153582E+00 0.10299999713898E+01 0.15449999570847E+01 0.13069831802568E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.12926632067583E-03
 0.00000000000000E+00 0.14973669991768E-01 0.13069831802568E-01 0.15102936312443E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    592.86267672
 0.70266232123970E+00 0.30455408034907E+03 0.49052222694134E+03 0.44696462375429E+03 0.42914545182565E+03
 0.22991485782749E+00 0.00000000000000E+00 0.19375089895878E+00 0.00000000000000E+00 -.17554379708456E+01
 0.97486622763020E-03 0.46750270974920E+00 0.80000000000000E+04 0.30000000000000E+04 0.17112200278565E+02
 0.64170751044617E+01 0.34808428656666E+03 0.29515007859818E+03 0.34345044071086E+03 0.37726356142036E+03
 0.29515000823476E+03 0.29515001410660E+03 0.33671276499755E+03 0.37713240746284E+03 0.29515000657782E+03
 0.29515001405825E+03 0.34345044071086E+03 0.37726356142036E+03 0.29515000823476E+03 0.29515001410660E+03
 0.33671276499754E+03 0.37713240746284E+03 0.29515000657782E+03 0.29515001405825E+03 0.42863397215834E+03
 0.31484772760265E+03 0.25578490346706E+04 0.22917305320517E+04 0.73650990496804E+03 0.13354874885038E+04
 0.59529503401095E+03 0.16149261873446E+04 0.14451543827635E+04 0.14264436066014E+04 0.23544816791071E+04
 0.13977626472242E+04 0.14405729531417E+04 0.12503740826032E+04 0.23511922475599E+04 0.16149261873446E+04
 0.14451543827635E+04 0.14264436066014E+04 0.23544816791071E+04 0.13977626472242E+04 0.14405729531417E+04
 0.12503740826033E+04 0.23511922475599E+04 0.21422999067151E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51788327777972E+03 0.18908928273621E+01
 0.18908928273621E+01 0.43836371256881E+01 0.11892111407306E+00 0.30619450970873E+03 0.30598109895590E+03
 0.30599801831888E+03 0.30599800746078E+03 0.22424957643139E+00 0.00000000000000E+00 0.22423817160102E+00
 0.00000000000000E+00 0.91302006169250E+01 0.14855899074474E-02 0.00000000000000E+00 0.53850662015776E+04
 0.20193998255916E+04 0.80000000000000E+04 0.30000000000000E+04 0.31485089178148E+03 0.42863548169802E+03
 0.29681344383153E+03 0.29707533352647E+03 0.29515000002170E+03 0.29515000015734E+03 0.29680690337461E+03
 0.29705626991679E+03 0.29515000002196E+03 0.29515000015714E+03 0.29681344383153E+03 0.29707533352647E+03
 0.29515000002170E+03 0.29515000015734E+03 0.29680690337461E+03 0.29705626991679E+03 0.29515000002196E+03
 0.29515000015714E+03 0.29702316944803E+03 0.29515000100405E+03 -.98350057829421E+02 -.12974982208770E+03
 0.61791067737643E+02 0.11742494845077E+03 0.55324925374437E+02 0.55448222097345E+02 0.39895283903733E+02
 0.93516117633668E+02 0.79920368007734E+02 0.55108743483062E+02 0.38755969801765E+02 0.93210388877684E+02
 0.78884739518171E+02 0.55448222097345E+02 0.39895283903732E+02 0.93516117633668E+02 0.79920368007732E+02
 0.55108743483058E+02 0.38755969801764E+02 0.93210388877676E+02 0.78884739518169E+02 0.21224173688302E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31552209674020E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92668461668945E+00 0.91347949623892E+00 0.00000000000000E+00 0.92668461668945E+00 0.91347949623892E+00
 0.13813311320096E+01 0.35133116061985E+00 0.10299999713898E+01 0.15449999570847E+01 0.13052091801915E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13346676103428E-03
 0.00000000000000E+00 0.14997063569354E-01 0.13052091801915E-01 0.15130530330388E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    602.16601166
 0.70302676040348E+00 0.30465688582469E+03 0.49084701385397E+03 0.44721479967813E+03 0.42935573538440E+03
 0.22991179109832E+00 0.00000000000000E+00 0.19375536684538E+00 0.00000000000000E+00 -.17577393285161E+01
 0.97580777038613E-03 0.46704672845349E+00 0.80000000000000E+04 0.30000000000000E+04 0.17128907050671E+02
 0.64233401440017E+01 0.34854818213995E+03 0.29515009109646E+03 0.34387702463672E+03 0.37790856884916E+03
 0.29515000972860E+03 0.29515001668439E+03 0.33710263144690E+03 0.37777676539634E+03 0.29515000777757E+03
 0.29515001662776E+03 0.34387702463672E+03 0.37790856884916E+03 0.29515000972860E+03 0.29515001668439E+03
 0.33710263144690E+03 0.37777676539634E+03 0.29515000777757E+03 0.29515001662776E+03 0.42924752965800E+03
 0.31540914582608E+03 0.25599746282726E+04 0.22910246105906E+04 0.73316071012534E+03 0.13285550785275E+04
 0.59172856485150E+03 0.16166547240750E+04 0.14450144617666E+04 0.14261872161328E+04 0.23508559895716E+04
 0.13999757404244E+04 0.14404435057845E+04 0.12509172225021E+04 0.23475818797828E+04 0.16166547240750E+04
 0.14450144617666E+04 0.14261872161328E+04 0.23508559895716E+04 0.13999757404244E+04 0.14404435057845E+04
 0.12509172225021E+04 0.23475818797828E+04 0.21387401376130E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51816795778368E+03 0.18908928497307E+01
 0.18908928497307E+01 0.44580638052141E+01 0.11170947450773E+00 0.30635142575884E+03 0.30612564599366E+03
 0.30614246048627E+03 0.30614244901632E+03 0.22415943258834E+00 0.00000000000000E+00 0.22414905792129E+00
 0.00000000000000E+00 0.91411878090312E+01 0.15814952193004E-02 0.00000000000000E+00 0.50585040677762E+04
 0.18969390254161E+04 0.80000000000000E+04 0.30000000000000E+04 0.31541240967286E+03 0.42924856406756E+03
 0.29688330868303E+03 0.29712346523643E+03 0.29515000002589E+03 0.29515000018638E+03 0.29687665302667E+03
 0.29710362171315E+03 0.29515000002620E+03 0.29515000018614E+03 0.29688330868303E+03 0.29712346523643E+03
 0.29515000002589E+03 0.29515000018638E+03 0.29687665302667E+03 0.29710362171315E+03 0.29515000002620E+03
 0.29515000018614E+03 0.29707215168722E+03 0.29515000117227E+03 -.10154751272070E+03 -.13489937094628E+03
 0.63576466584398E+02 0.11991226671897E+03 0.56017917801645E+02 0.56990148114473E+02 0.41100295440528E+02
 0.95970831740355E+02 0.81608083225254E+02 0.56655277005077E+02 0.39927403527031E+02 0.95670760292490E+02
 0.80543288674017E+02 0.56990148114473E+02 0.41100295440527E+02 0.95970831740355E+02 0.81608083225251E+02
 0.56655277005073E+02 0.39927403527030E+02 0.95670760292482E+02 0.80543288674016E+02 0.21085469769976E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31563454357645E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92693340399330E+00 0.91372017095582E+00 0.00000000000000E+00 0.92693340399330E+00 0.91372017095582E+00
 0.13815133515915E+01 0.35151338020174E+00 0.10299999713898E+01 0.15449999570847E+01 0.13034959552934E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13793248069752E-03
 0.00000000000000E+00 0.15017606626348E-01 0.13034959552934E-01 0.15155539107046E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    611.46934660
 0.70338598741843E+00 0.30475906674266E+03 0.49116819308399E+03 0.44746233728552E+03 0.42956392582180E+03
 0.22990926676812E+00 0.00000000000000E+00 0.19376050747092E+00 0.00000000000000E+00 -.17600298490557E+01
 0.97689944499173E-03 0.46659671136280E+00 0.80000000000000E+04 0.30000000000000E+04 0.17145427314809E+02
 0.64295352430536E+01 0.34900797745048E+03 0.29515010523504E+03 0.34429994654213E+03 0.37854657334219E+03
 0.29515001145145E+03 0.29515001966020E+03 0.33748946370892E+03 0.37841413299359E+03 0.29515000916242E+03
 0.29515001959411E+03 0.34429994654213E+03 0.37854657334219E+03 0.29515001145145E+03 0.29515001966020E+03
 0.33748946370892E+03 0.37841413299359E+03 0.29515000916242E+03 0.29515001959411E+03 0.42985206683875E+03
 0.31596671393569E+03 0.25620708924034E+04 0.22903112320676E+04 0.72986217214226E+03 0.13217451499386E+04
 0.58823366693566E+03 0.16183630964695E+04 0.14448564706050E+04 0.14259256044795E+04 0.23472546036339E+04
 0.14021647951511E+04 0.14402960711190E+04 0.12514468660534E+04 0.23439957770041E+04 0.16183630964695E+04
 0.14448564706050E+04 0.14259256044795E+04 0.23472546036339E+04 0.14021647951511E+04 0.14402960711190E+04
 0.12514468660534E+04 0.23439957770041E+04 0.21352194989712E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51844954040950E+03 0.18908928719940E+01
 0.18908928719940E+01 0.45324904847401E+01 0.10456561992045E+00 0.30651116591226E+03 0.30627323958774E+03
 0.30628982553015E+03 0.30628981355232E+03 0.22406967986889E+00 0.00000000000000E+00 0.22406028034172E+00
 0.00000000000000E+00 0.91519635169982E+01 0.16895418678297E-02 0.00000000000000E+00 0.47350113970696E+04
 0.17756292739011E+04 0.80000000000000E+04 0.30000000000000E+04 0.31597007417752E+03 0.42985265789751E+03
 0.29695408453290E+03 0.29717245671945E+03 0.29515000003079E+03 0.29515000021998E+03 0.29694732976632E+03
 0.29715182350439E+03 0.29515000003115E+03 0.29515000021970E+03 0.29695408453290E+03 0.29717245671945E+03
 0.29515000003079E+03 0.29515000021998E+03 0.29694732976632E+03 0.29715182350439E+03 0.29515000003115E+03
 0.29515000021970E+03 0.29712193837603E+03 0.29515000136410E+03 -.10473629999690E+03 -.14003416844371E+03
 0.65357351312472E+02 0.12241216453364E+03 0.56728026464601E+02 0.58532139331011E+02 0.42303993992113E+02
 0.98493053518463E+02 0.83308824279803E+02 0.58203180669430E+02 0.41097638829588E+02 0.98199913923348E+02
 0.82215053734813E+02 0.58532139331011E+02 0.42303993992111E+02 0.98493053518463E+02 0.83308824279799E+02
 0.58203180669426E+02 0.41097638829587E+02 0.98199913923341E+02 0.82215053734811E+02 0.20956361026522E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31574578961284E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92717830273274E+00 0.91395979658577E+00 0.00000000000000E+00 0.92717830273274E+00 0.91395979658577E+00
 0.13816929650990E+01 0.35169299370922E+00 0.10299999713898E+01 0.15449999570847E+01 0.13018473510711E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.14266020794604E-03
 0.00000000000000E+00 0.15035227658392E-01 0.13018473510711E-01 0.15177887866338E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    620.77268154
 0.70373999823185E+00 0.30486058646373E+03 0.49148584348570E+03 0.44770729080348E+03 0.42977005637037E+03
 0.22990709747391E+00 0.00000000000000E+00 0.19376624358071E+00 0.00000000000000E+00 -.17623082438082E+01
 0.97808749823871E-03 0.46615259775750E+00 0.80000000000000E+04 0.30000000000000E+04 0.17161762132154E+02
 0.64356607995579E+01 0.34946376872356E+03 0.29515012118245E+03 0.34471929138566E+03 0.37917775269664E+03
 0.29515001343186E+03 0.29515002308401E+03 0.33787332879024E+03 0.37904468774104E+03 0.29515001075565E+03
 0.29515002300715E+03 0.34471929138566E+03 0.37917775269664E+03 0.29515001343186E+03 0.29515002308401E+03
 0.33787332879024E+03 0.37904468774104E+03 0.29515001075565E+03 0.29515002300715E+03 0.43044794693101E+03
 0.31652046819609E+03 0.25641389081979E+04 0.22895906024579E+04 0.72661135147935E+03 0.13150510875573E+04
 0.58480667932052E+03 0.16200519968688E+04 0.14446810093141E+04 0.14256588257087E+04 0.23436768763471E+04
 0.14043305809673E+04 0.14401312418650E+04 0.12519632739232E+04 0.23404332901966E+04 0.16200519968688E+04
 0.14446810093141E+04 0.14256588257087E+04 0.23436768763471E+04 0.14043305809673E+04 0.14401312418650E+04
 0.12519632739232E+04 0.23404332901966E+04 0.21317361571896E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51872808844852E+03 0.18908928941395E+01
 0.18908928941395E+01 0.46069171642661E+01 0.97520526331684E-01 0.30667343659248E+03 0.30642367966137E+03
 0.30643991727962E+03 0.30643990491468E+03 0.22398034098128E+00 0.00000000000000E+00 0.22397183296132E+00
 0.00000000000000E+00 0.91625418735023E+01 0.18115979499341E-02 0.00000000000000E+00 0.44159908661252E+04
 0.16559965747970E+04 0.80000000000000E+04 0.30000000000000E+04 0.31652392292454E+03 0.43044812306675E+03
 0.29702590974072E+03 0.29722228305693E+03 0.29515000003649E+03 0.29515000025873E+03 0.29701907304512E+03
 0.29720085077415E+03 0.29515000003691E+03 0.29515000025838E+03 0.29702590974072E+03 0.29722228305693E+03
 0.29515000003649E+03 0.29515000025873E+03 0.29701907304512E+03 0.29720085077415E+03 0.29515000003691E+03
 0.29515000025838E+03 0.29717250632660E+03 0.29515000158218E+03 -.10791629338064E+03 -.14515354753810E+03
 0.67133601937593E+02 0.12492422023853E+03 0.57454950291251E+02 0.60072857475478E+02 0.43506088911360E+02
 0.10108649713135E+03 0.85019667897075E+02 0.59751081319695E+02 0.42266395549105E+02 0.10080152526402E+03
 0.83897116205810E+02 0.60072857475477E+02 0.43506088911358E+02 0.10108649713135E+03 0.85019667897072E+02
 0.59751081319691E+02 0.42266395549104E+02 0.10080152526401E+03 0.83897116205809E+02 0.20836059240978E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31586162012131E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92741963414082E+00 0.91419805501258E+00 0.00000000000000E+00 0.92741963414082E+00 0.91419805501258E+00
 0.13818699705057E+01 0.35186999911593E+00 0.10299999713898E+01 0.15449999570847E+01 0.13002765979106E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.36510079992962E-05 0.14348135984942E-03
 0.37349314746103E-03 0.14677947761955E-01 0.13002765979106E-01 0.15198573277265E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    630.07601649
 0.70408923361812E+00 0.30496136120394E+03 0.49180006479072E+03 0.44794969158451E+03 0.42997411612997E+03
 0.22990515674221E+00 0.00000000000000E+00 0.19377247923184E+00 0.00000000000000E+00 -.17645716518837E+01
 0.97891565971395E-03 0.46571432661384E+00 0.80000000000000E+04 0.30000000000000E+04 0.17177912601846E+02
 0.64417172256921E+01 0.34991564899507E+03 0.29515013911939E+03 0.34513514189476E+03 0.37980227444051E+03
 0.29515001570112E+03 0.29515002701059E+03 0.33825429309294E+03 0.37966859687571E+03 0.29515001258276E+03
 0.29515002692148E+03 0.34513514189476E+03 0.37980227444051E+03 0.29515001570112E+03 0.29515002701059E+03
 0.33825429309294E+03 0.37966859687571E+03 0.29515001258276E+03 0.29515002692148E+03 0.43103549193062E+03
 0.31707041147414E+03 0.25661807743221E+04 0.22888635468285E+04 0.72340626793050E+03 0.13084679263330E+04
 0.58144462706288E+03 0.16217229326416E+04 0.14444890661668E+04 0.14253874474378E+04 0.23401228362342E+04
 0.14064745679533E+04 0.14399499960567E+04 0.12524671139581E+04 0.23368944407128E+04 0.16217229326416E+04
 0.14444890661668E+04 0.14253874474378E+04 0.23401228362342E+04 0.14064745679534E+04 0.14399499960567E+04
 0.12524671139581E+04 0.23368944407128E+04 0.21282891112781E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51900366721256E+03 0.18908929161393E+01
 0.18908929161393E+01 0.46813438437921E+01 0.90800228075640E-01 0.30683803224277E+03 0.30657678788798E+03
 0.30659260191931E+03 0.30659258926950E+03 0.22389142180721E+00 0.00000000000000E+00 0.22388370924780E+00
 0.00000000000000E+00 0.91729245196088E+01 0.19456776926347E-02 0.00000000000000E+00 0.41116779157636E+04
 0.15418792184114E+04 0.80000000000000E+04 0.30000000000000E+04 0.31707395974140E+03 0.43103527860104E+03
 0.29709889051162E+03 0.29727291333199E+03 0.29515000004310E+03 0.29515000030326E+03 0.29709198851202E+03
 0.29725067316097E+03 0.29515000004359E+03 0.29515000030286E+03 0.29709889051162E+03 0.29727291333199E+03
 0.29515000004310E+03 0.29515000030326E+03 0.29709198851202E+03 0.29725067316097E+03 0.29515000004359E+03
 0.29515000030286E+03 0.29722383512314E+03 0.29515000182942E+03 -.11108635219340E+03 -.15025544474423E+03
 0.68903877828728E+02 0.12744625701482E+03 0.58197859796951E+02 0.61607469528763E+02 0.44704334532018E+02
 0.10373361697971E+03 0.86738657171320E+02 0.61293566583812E+02 0.43431508468201E+02 0.10345745911959E+03
 0.85587599390242E+02 0.61607469528763E+02 0.44704334532016E+02 0.10373361697971E+03 0.86738657171317E+02
 0.61293566583809E+02 0.43431508468201E+02 0.10345745911959E+03 0.85587599390242E+02 0.20724249696946E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31598313612743E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92765730070482E+00 0.91443473505027E+00 0.00000000000000E+00 0.92765730070482E+00 0.91443473505027E+00
 0.13820445881988E+01 0.35204461680906E+00 0.10299999713898E+01 0.15449999570847E+01 0.12988033602171E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13893139772441E-04 0.13700624526093E-03
 0.13872318555896E-02 0.13680091826161E-01 0.12988033602171E-01 0.15218223066784E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    644.03101890
 0.70460428472197E+00 0.30511109517925E+03 0.49226511700460E+03 0.44830860844424E+03 0.43027639161798E+03
 0.22990251501708E+00 0.00000000000000E+00 0.19378257106833E+00 0.00000000000000E+00 -.17679371246590E+01
 0.97943029805870E-03 0.46506754644789E+00 0.80000000000000E+04 0.30000000000000E+04 0.17201802321196E+02
 0.64506758704483E+01 0.35058632679401E+03 0.29515017018163E+03 0.34575253550038E+03 0.38072691340672E+03
 0.29515001972211E+03 0.29515003397545E+03 0.33882043757617E+03 0.38059233882518E+03 0.29515001582362E+03
 0.29515003386489E+03 0.34575253550038E+03 0.38072691340672E+03 0.29515001972211E+03 0.29515003397545E+03
 0.33882043757617E+03 0.38059233882518E+03 0.29515001582362E+03 0.29515003386489E+03 0.43190179967345E+03
 0.31788813346632E+03 0.25691959749402E+04 0.22877609459610E+04 0.71867969780717E+03 0.12987902788126E+04
 0.57651718251636E+03 0.16241966142308E+04 0.14441710786180E+04 0.14249717049948E+04 0.23348346409829E+04
 0.14096507882831E+04 0.14396481629981E+04 0.12531996330333E+04 0.23316289302706E+04 0.16241966142308E+04
 0.14441710786180E+04 0.14249717049948E+04 0.23348346409829E+04 0.14096507882831E+04 0.14396481629981E+04
 0.12531996330333E+04 0.23316289302706E+04 0.21231832368914E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51941159020495E+03 0.18908929488508E+01
 0.18908929488508E+01 0.47929838630811E+01 0.81472610460647E-01 0.30708892139044E+03 0.30681105336056E+03
 0.30682614578306E+03 0.30682613286786E+03 0.22375882786411E+00 0.00000000000000E+00 0.22375212113856E+00
 0.00000000000000E+00 0.91881551222543E+01 0.21684338585256E-02 0.00000000000000E+00 0.36892986007144E+04
 0.13834869752679E+04 0.80000000000000E+04 0.30000000000000E+04 0.31789182100685E+03 0.43190104496002E+03
 0.29721060020062E+03 0.29735031147769E+03 0.29515000005500E+03 0.29515000038251E+03 0.29720362777066E+03
 0.29732684433702E+03 0.29515000005561E+03 0.29515000038198E+03 0.29721060020062E+03 0.29735031147769E+03
 0.29515000005500E+03 0.29515000038251E+03 0.29720362777066E+03 0.29732684433702E+03 0.29515000005561E+03
 0.29515000038198E+03 0.29730221062756E+03 0.29515000226182E+03 -.11582123529470E+03 -.15787163858644E+03
 0.71546664846639E+02 0.13124402148248E+03 0.59339623311604E+02 0.63893329339175E+02 0.46492339558296E+02
 0.10779101708401E+03 0.89329036823465E+02 0.63591965316972E+02 0.45170264914836E+02 0.10752873048180E+03
 0.88135795188483E+02 0.63893329339175E+02 0.46492339558295E+02 0.10779101708401E+03 0.89329036823464E+02
 0.63591965316969E+02 0.45170264914836E+02 0.10752873048179E+03 0.88135795188483E+02 0.20571790878048E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31617589532328E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92800737622598E+00 0.91478639738089E+00 0.00000000000000E+00 0.92800737622598E+00 0.91478639738089E+00
 0.13823021137508E+01 0.35230214236098E+00 0.10299999713898E+01 0.15449999570847E+01 0.12966938668751E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29091342400574E-04 0.12781093552025E-03
 0.27972783319569E-02 0.12289661838049E-01 0.12966938668751E-01 0.15243842447926E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    653.33435384
 0.70494166976575E+00 0.30521000289950E+03 0.49257107026887E+03 0.44854486237538E+03 0.43047545798189E+03
 0.22990087171480E+00 0.00000000000000E+00 0.19378967313048E+00 0.00000000000000E+00 -.17701624832875E+01
 0.97949540252702E-03 0.46464321626470E+00 0.80000000000000E+04 0.30000000000000E+04 0.17217511673392E+02
 0.64565668775221E+01 0.35102880225665E+03 0.29515019396688E+03 0.34615998187968E+03 0.38133545476177E+03
 0.29515002286959E+03 0.29515003943246E+03 0.33919441786512E+03 0.38120029640045E+03 0.29515001836292E+03
 0.29515003930528E+03 0.34615998187968E+03 0.38133545476177E+03 0.29515002286959E+03 0.29515003943246E+03
 0.33919441786512E+03 0.38120029640045E+03 0.29515001836292E+03 0.29515003930528E+03 0.43246970089901E+03
 0.31842843989546E+03 0.25711748241763E+04 0.22870179377154E+04 0.71558011050638E+03 0.12924638435670E+04
 0.57330583250808E+03 0.16258241380566E+04 0.14439396720749E+04 0.14246887247325E+04 0.23313370183773E+04
 0.14117421274978E+04 0.14394275921884E+04 0.12536727135757E+04 0.23281463611057E+04 0.16258241380566E+04
 0.14439396720749E+04 0.14246887247325E+04 0.23313370183773E+04 0.14117421274978E+04 0.14394275921884E+04
 0.12536727135758E+04 0.23281463611057E+04 0.21198206815708E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51968000717632E+03 0.18908929704808E+01
 0.18908929704808E+01 0.48674105426071E+01 0.75722530297274E-01 0.30725864807122E+03 0.30697003333159E+03
 0.30698460309050E+03 0.30698459009506E+03 0.22367081288585E+00 0.00000000000000E+00 0.22366479687914E+00
 0.00000000000000E+00 0.91980860668666E+01 0.23330963557711E-02 0.00000000000000E+00 0.34289196758682E+04
 0.12858448784506E+04 0.80000000000000E+04 0.30000000000000E+04 0.31843221951320E+03 0.43246861130539E+03
 0.29728661672832E+03 0.29740284711296E+03 0.29515000006447E+03 0.29515000044478E+03 0.29727961415130E+03
 0.29737855263678E+03 0.29515000006517E+03 0.29515000044416E+03 0.29728661672832E+03 0.29740284711296E+03
 0.29515000006447E+03 0.29515000044478E+03 0.29727961415130E+03 0.29737855263678E+03 0.29515000006517E+03
 0.29515000044416E+03 0.29735535464684E+03 0.29515000259613E+03 -.11896371449345E+03 -.16292242085451E+03
 0.73299786800387E+02 0.13378365061758E+03 0.60117364883192E+02 0.65405627587291E+02 0.47677833835062E+02
 0.11055759339174E+03 0.91062313412444E+02 0.65113052271401E+02 0.46323227985299E+02 0.11030493800150E+03
 0.89841329670033E+02 0.65405627587291E+02 0.47677833835062E+02 0.11055759339174E+03 0.91062313412443E+02
 0.65113052271398E+02 0.46323227985298E+02 0.11030493800149E+03 0.89841329670032E+02 0.20479983445397E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31631079783062E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92823676469433E+00 0.91501871904509E+00 0.00000000000000E+00 0.92823676469433E+00 0.91501871904509E+00
 0.13824708062726E+01 0.35247083488287E+00 0.10299999713898E+01 0.15449999570847E+01 0.12953531739154E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.39146722901911E-04 0.12198571457864E-03
 0.36678469750532E-02 0.11429435238757E-01 0.12953531739154E-01 0.15258414651291E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    662.63768878
 0.70527426863241E+00 0.30530820764239E+03 0.49287385425584E+03 0.44877877951055E+03 0.43067263709814E+03
 0.22989929257197E+00 0.00000000000000E+00 0.19379699187516E+00 0.00000000000000E+00 -.17723742910981E+01
 0.97943352066028E-03 0.46422422067200E+00 0.80000000000000E+04 0.30000000000000E+04 0.17233051710269E+02
 0.64623943913509E+01 0.35146765399388E+03 0.29515022047636E+03 0.34656418935327E+03 0.38193784973068E+03
 0.29515002643913E+03 0.29515004562572E+03 0.33956570402548E+03 0.38180211869856E+03 0.29515002124499E+03
 0.29515004547985E+03 0.34656418935327E+03 0.38193784973068E+03 0.29515002643913E+03 0.29515004562572E+03
 0.33956570402548E+03 0.38180211869856E+03 0.29515002124499E+03 0.29515004547985E+03 0.43303016597399E+03
 0.31896482781790E+03 0.25731293005792E+04 0.22862689480882E+04 0.71252011679337E+03 0.12862337014343E+04
 0.57015098405700E+03 0.16274347718364E+04 0.14436934865096E+04 0.14244012899120E+04 0.23278615139373E+04
 0.14138130347453E+04 0.14391922903548E+04 0.12541339621373E+04 0.23246858524042E+04 0.16274347718364E+04
 0.14436934865096E+04 0.14244012899120E+04 0.23278615139373E+04 0.14138130347453E+04 0.14391922903548E+04
 0.12541339621373E+04 0.23246858524042E+04 0.21164902995160E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.51994568591177E+03 0.18908929919790E+01
 0.18908929919790E+01 0.49418372221331E+01 0.70322306729554E-01 0.30743013448620E+03 0.30713105529216E+03
 0.30714507658471E+03 0.30714506358318E+03 0.22358330339494E+00 0.00000000000000E+00 0.22357777813941E+00
 0.00000000000000E+00 0.92078450022563E+01 0.25122604766016E-02 0.00000000000000E+00 0.31843831778231E+04
 0.11941436916837E+04 0.80000000000000E+04 0.30000000000000E+04 0.31896869859104E+03 0.43302876077678E+03
 0.29736392883255E+03 0.29745610845945E+03 0.29515000007535E+03 0.29515000051564E+03 0.29735690874971E+03
 0.29743097965428E+03 0.29515000007616E+03 0.29515000051491E+03 0.29736392883255E+03 0.29745610845945E+03
 0.29515000007535E+03 0.29515000051564E+03 0.29735690874971E+03 0.29743097965428E+03 0.29515000007616E+03
 0.29515000051491E+03 0.29740919259163E+03 0.29515000297163E+03 -.12209433343164E+03 -.16795042763247E+03
 0.75045632555094E+02 0.13632792743819E+03 0.60907066720316E+02 0.66907796156369E+02 0.48857891507066E+02
 0.11337645051916E+03 0.92799458270934E+02 0.66624307098602E+02 0.47470997346471E+02 0.11313369060849E+03
 0.91551036062999E+02 0.66907796156369E+02 0.48857891507065E+02 0.11337645051916E+03 0.92799458270933E+02
 0.66624307098599E+02 0.47470997346471E+02 0.11313369060849E+03 0.91551036062999E+02 0.20395688721192E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31645037649724E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92846311304707E+00 0.91524948072060E+00 0.00000000000000E+00 0.92846311304707E+00 0.91524948072060E+00
 0.13826371057060E+01 0.35263713431621E+00 0.10299999713898E+01 0.15449999570847E+01 0.12940631549644E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.49160526930337E-04 0.11637422247711E-03
 0.44860428525185E-02 0.10619490505068E-01 0.12940631549644E-01 0.15271068106994E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    671.94102372
 0.70560214269742E+00 0.30540573014815E+03 0.49317355095946E+03 0.44901042539477E+03 0.43086798483059E+03
 0.22989776178834E+00 0.00000000000000E+00 0.19380446764895E+00 0.00000000000000E+00 -.17745731997606E+01
 0.97928975489248E-03 0.46381044579994E+00 0.80000000000000E+04 0.30000000000000E+04 0.17248425671402E+02
 0.64681596267756E+01 0.35190295677592E+03 0.29515024995090E+03 0.34696522440378E+03 0.38253423128476E+03
 0.29515003047632E+03 0.29515005263517E+03 0.33993435007365E+03 0.38239793846384E+03 0.29515002450714E+03
 0.29515005246834E+03 0.34696522440378E+03 0.38253423128476E+03 0.29515003047632E+03 0.29515005263517E+03
 0.33993435007365E+03 0.38239793846384E+03 0.29515002450714E+03 0.29515005246834E+03 0.43358341426456E+03
 0.31949725584603E+03 0.25750600301442E+04 0.22855143414387E+04 0.70949855778244E+03 0.12800968229120E+04
 0.56705077234064E+03 0.16290289086138E+04 0.14434331919863E+04 0.14241096185175E+04 0.23244080758213E+04
 0.14158639707902E+04 0.14389429235354E+04 0.12545837370584E+04 0.23212473507415E+04 0.16290289086138E+04
 0.14434331919863E+04 0.14241096185175E+04 0.23244080758213E+04 0.14158639707902E+04 0.14389429235354E+04
 0.12545837370584E+04 0.23212473507415E+04 0.21131914379447E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52020869909159E+03 0.18908930133519E+01
 0.18908930133519E+01 0.50162639016591E+01 0.65253636130724E-01 0.30760324502184E+03 0.30729395308843E+03
 0.30730740803728E+03 0.30730739509644E+03 0.22349609036889E+00 0.00000000000000E+00 0.22349107202634E+00
 0.00000000000000E+00 0.92174376591308E+01 0.27074038252858E-02 0.00000000000000E+00 0.29548602706711E+04
 0.11080726015017E+04 0.80000000000000E+04 0.30000000000000E+04 0.31950121651569E+03 0.43358171165288E+03
 0.29744259298908E+03 0.29751007579526E+03 0.29515000008782E+03 0.29515000059605E+03 0.29743556742278E+03
 0.29748410605376E+03 0.29515000008875E+03 0.29515000059520E+03 0.29744259298908E+03 0.29751007579526E+03
 0.29515000008782E+03 0.29515000059605E+03 0.29743556742278E+03 0.29748410605376E+03 0.29515000008875E+03
 0.29515000059520E+03 0.29746370693725E+03 0.29515000339236E+03 -.12521257514639E+03 -.17295426960857E+03
 0.76783934580524E+02 0.13887550409421E+03 0.61707649840788E+02 0.68399057729411E+02 0.50032297172317E+02
 0.11625050691641E+03 0.94539442094535E+02 0.68124908666936E+02 0.48613360270451E+02 0.11601786539924E+03
 0.93263885581373E+02 0.68399057729411E+02 0.50032297172316E+02 0.11625050691641E+03 0.94539442094534E+02
 0.68124908666933E+02 0.48613360270451E+02 0.11601786539923E+03 0.93263885581373E+02 0.20318651627768E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31659428319388E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92868652319879E+00 0.91547877215052E+00 0.00000000000000E+00 0.92868652319879E+00 0.91547877215052E+00
 0.13828010427385E+01 0.35280107134871E+00 0.10299999713898E+01 0.15449999570847E+01 0.12928217084289E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.59143398665566E-04 0.11095956174694E-03
 0.52542424549135E-02 0.98575403724439E-02 0.12928217084289E-01 0.15281885787770E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    687.46591418
 0.70613976730292E+00 0.30556432891825E+03 0.49366446630548E+03 0.44938947055739E+03 0.43118723574398E+03
 0.22989529528246E+00 0.00000000000000E+00 0.19381716416639E+00 0.00000000000000E+00 -.17782560621539E+01
 0.97895033368289E-03 0.46313363908711E+00 0.80000000000000E+04 0.30000000000000E+04 0.17273631895470E+02
 0.64776119608011E+01 0.35262165305181E+03 0.29515030795076E+03 0.34762736247667E+03 0.38352003081269E+03
 0.29515003864263E+03 0.29515006682863E+03 0.34054269635297E+03 0.38338280609887E+03 0.29515003111399E+03
 0.29515006661995E+03 0.34762736247667E+03 0.38352003081269E+03 0.29515003864263E+03 0.29515006682863E+03
 0.34054269635297E+03 0.38338280609887E+03 0.29515003111399E+03 0.29515006661995E+03 0.43450903844708E+03
 0.32039330309939E+03 0.25782399504776E+04 0.22842292600952E+04 0.70421212070256E+03 0.12694970456867E+04
 0.56176386438060E+03 0.16316602527628E+04 0.14429257774598E+04 0.14236040187889E+04 0.23185926563079E+04
 0.14192575309300E+04 0.14384534405246E+04 0.12553136142450E+04 0.23154564874296E+04 0.16316602527628E+04
 0.14429257774598E+04 0.14236040187889E+04 0.23185926563079E+04 0.14192575309300E+04 0.14384534405246E+04
 0.12553136142450E+04 0.23154564874296E+04 0.21075700551110E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52063927784159E+03 0.18908930491484E+01
 0.18908930491484E+01 0.51404630253291E+01 0.57486216957264E-01 0.30789427214343E+03 0.30756935235002E+03
 0.30758180462318E+03 0.30758179198544E+03 0.22335134504315E+00 0.00000000000000E+00 0.22334705440148E+00
 0.00000000000000E+00 0.92327153065052E+01 0.30732224236186E-02 0.00000000000000E+00 0.26031308175151E+04
 0.97617405656817E+03 0.80000000000000E+04 0.30000000000000E+04 0.32039736180370E+03 0.43450693722620E+03
 0.29757203566092E+03 0.29759937900629E+03 0.29515000011364E+03 0.29515000075963E+03 0.29756501329152E+03
 0.29757202406247E+03 0.29515000011481E+03 0.29515000075850E+03 0.29757203566092E+03 0.29759937900629E+03
 0.29515000011364E+03 0.29515000075963E+03 0.29756501329152E+03 0.29757202406247E+03 0.29515000011481E+03
 0.29515000075850E+03 0.29755384854353E+03 0.29515000423088E+03 -.13050664699742E+03 -.18146151144069E+03
 0.79714849732464E+02 0.14319487859099E+03 0.63081454609864E+02 0.70917751328298E+02 0.52010115762584E+02
 0.12125910150490E+03 0.97489748918509E+02 0.70659249147245E+02 0.50537085592358E+02 0.12104342631503E+03
 0.96168246848974E+02 0.70917751328298E+02 0.52010115762584E+02 0.12125910150490E+03 0.97489748918507E+02
 0.70659249147241E+02 0.50537085592358E+02 0.12104342631502E+03 0.96168246848973E+02 0.20182720159537E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31684346624396E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92901603211658E+00 0.91589979564143E+00 0.00000000000000E+00 0.92901603211658E+00 0.91589979564143E+00
 0.13830698550412E+01 0.35306988365146E+00 0.10299999713898E+01 0.15449999570847E+01 0.12909980880701E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.75799939108802E-04 0.10238687383710E-03
 0.64293236954760E-02 0.86844179798314E-02 0.12909980880701E-01 0.15291928488253E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    698.33913717
 0.70650675132172E+00 0.30567242109305E+03 0.49400193673498E+03 0.44964991197692E+03 0.43140634837451E+03
 0.22989361765399E+00 0.00000000000000E+00 0.19382613644924E+00 0.00000000000000E+00 -.17806273472594E+01
 0.97867478699193E-03 0.46266929573124E+00 0.80000000000000E+04 0.30000000000000E+04 0.17290968027943E+02
 0.64841130104788E+01 0.35311926044959E+03 0.29515035533164E+03 0.34808604092353E+03 0.38419989080976E+03
 0.29515004548813E+03 0.29515007873749E+03 0.34096478811431E+03 0.38406203546403E+03 0.29515003665875E+03
 0.29515007849419E+03 0.34808604092353E+03 0.38419989080976E+03 0.29515004548813E+03 0.29515007873749E+03
 0.34096478811431E+03 0.38406203546404E+03 0.29515003665875E+03 0.29515007849419E+03 0.43513923873091E+03
 0.32100991590205E+03 0.25804011201012E+04 0.22832763501942E+04 0.70065752955159E+03 0.12623678047998E+04
 0.55820698760049E+03 0.16334591365683E+04 0.14425256728956E+04 0.14232126661627E+04 0.23145290343146E+04
 0.14215830058609E+04 0.14380661885156E+04 0.12557736105278E+04 0.23114101485026E+04 0.16334591365683E+04
 0.14425256728956E+04 0.14232126661627E+04 0.23145290343146E+04 0.14215830058609E+04 0.14380661885155E+04
 0.12557736105278E+04 0.23114101485026E+04 0.21036918141112E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52093513039015E+03 0.18908930721967E+01
 0.18908930721967E+01 0.52274488092361E+01 0.52525596424346E-01 0.30810073823214E+03 0.30776452865058E+03
 0.30777630172311E+03 0.30777628932534E+03 0.22325051717123E+00 0.00000000000000E+00 0.22324668296134E+00
 0.00000000000000E+00 0.92434776622829E+01 0.33634634116709E-02 0.00000000000000E+00 0.23785006764875E+04
 0.89193775368279E+03 0.80000000000000E+04 0.30000000000000E+04 0.32101405734412E+03 0.43513685991907E+03
 0.29766513292317E+03 0.29766359602474E+03 0.29515000013579E+03 0.29515000089750E+03 0.29765813231904E+03
 0.29763524839482E+03 0.29515000013714E+03 0.29515000089615E+03 0.29766513292317E+03 0.29766359602474E+03
 0.29515000013579E+03 0.29515000089750E+03 0.29765813231904E+03 0.29763524839482E+03 0.29515000013714E+03
 0.29515000089615E+03 0.29761860340289E+03 0.29515000492429E+03 -.13416137658962E+03 -.18732005352289E+03
 0.81749447546813E+02 0.14620951565492E+03 0.64051320870372E+02 0.72660096967331E+02 0.53382127109653E+02
 0.12487776812121E+03 0.99549255122493E+02 0.72412948793207E+02 0.51871802177970E+02 0.12467432151386E+03
 0.98196321508651E+02 0.72660096967331E+02 0.53382127109652E+02 0.12487776812121E+03 0.99549255122492E+02
 0.72412948793203E+02 0.51871802177970E+02 0.12467432151385E+03 0.98196321508652E+02 0.20106804664380E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31702294618901E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92928171601937E+00 0.91612779302107E+00 0.00000000000000E+00 0.92928171601937E+00 0.91612779302107E+00
 0.13832533470506E+01 0.35325337566086E+00 0.10299999713898E+01 0.15449999570847E+01 0.12896174549578E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.87558034430847E-04 0.96768342809240E-04
 0.71808052213050E-02 0.79361605798772E-02 0.12896174549578E-01 0.15301292178422E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    709.21236015
 0.70686484921200E+00 0.30578092625963E+03 0.49433695113300E+03 0.44990907576963E+03 0.43162481953989E+03
 0.22989196708399E+00 0.00000000000000E+00 0.19383510475410E+00 0.00000000000000E+00 -.17830574742413E+01
 0.97837599477570E-03 0.46221045003298E+00 0.80000000000000E+04 0.30000000000000E+04 0.17308133122973E+02
 0.64905499211148E+01 0.35361211952217E+03 0.29515040852167E+03 0.34854048064388E+03 0.38487115392033E+03
 0.29515005332539E+03 0.29515009238082E+03 0.34138347267025E+03 0.38473268480888E+03 0.29515004301253E+03
 0.29515009209825E+03 0.34854048064388E+03 0.38487115392033E+03 0.29515005332539E+03 0.29515009238082E+03
 0.34138347267025E+03 0.38473268480889E+03 0.29515004301253E+03 0.29515009209825E+03 0.43575662759392E+03
 0.32161777050927E+03 0.25825438370395E+04 0.22823414851253E+04 0.69723404098975E+03 0.12554964399252E+04
 0.55477622873052E+03 0.16352430391876E+04 0.14421333349372E+04 0.14228319703183E+04 0.23105412368631E+04
 0.14238873485752E+04 0.14376868313200E+04 0.12562317683748E+04 0.23074396086360E+04 0.16352430391876E+04
 0.14421333349372E+04 0.14228319703183E+04 0.23105412368631E+04 0.14238873485752E+04 0.14376868313200E+04
 0.12562317683749E+04 0.23074396086360E+04 0.20999176464149E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52122908925386E+03 0.18908930958170E+01
 0.18908930958170E+01 0.53144345931432E+01 0.47931025280950E-01 0.30830895621894E+03 0.30796158454299E+03
 0.30797268446338E+03 0.30797267235629E+03 0.22315014409499E+00 0.00000000000000E+00 0.22314671754433E+00
 0.00000000000000E+00 0.92540179273841E+01 0.36858780170721E-02 0.00000000000000E+00 0.21704462174130E+04
 0.81391733152989E+03 0.80000000000000E+04 0.30000000000000E+04 0.32162201565459E+03 0.43575396269025E+03
 0.29776119017792E+03 0.29772903864775E+03 0.29515000016161E+03 0.29515000105603E+03 0.29775422655626E+03
 0.29769968548976E+03 0.29515000016319E+03 0.29515000105441E+03 0.29776119017792E+03 0.29772903864775E+03
 0.29515000016161E+03 0.29515000105603E+03 0.29775422655626E+03 0.29769968548976E+03 0.29515000016319E+03
 0.29515000105441E+03 0.29768455917555E+03 0.29515000571014E+03 -.13777492465733E+03 -.19310015401550E+03
 0.83760565620286E+02 0.14920839893836E+03 0.65029030489976E+02 0.74370799081477E+02 0.54737688305856E+02
 0.12857774934085E+03 0.10159871015548E+03 0.74135215756870E+02 0.53190693974502E+02 0.12838670372273E+03
 0.10021507007020E+03 0.74370799081477E+02 0.54737688305855E+02 0.12857774934085E+03 0.10159871015548E+03
 0.74135215756866E+02 0.53190693974503E+02 0.12838670372272E+03 0.10021507007020E+03 0.20042754601793E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31720700887056E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92953797517144E+00 0.91637168213908E+00 0.00000000000000E+00 0.92953797517144E+00 0.91637168213908E+00
 0.13834323959958E+01 0.35343242460600E+00 0.10299999713898E+01 0.15449999570847E+01 0.12883232975005E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.99271555119903E-04 0.91279430685900E-04
 0.78755887142084E-02 0.72415431921154E-02 0.12883232975005E-01 0.15307682892130E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    720.00000000
 0.70721408967719E+00 0.30588949894351E+03 0.49466723082851E+03 0.45016514022639E+03 0.43184116256364E+03
 0.22989035389591E+00 0.00000000000000E+00 0.19384395401680E+00 0.00000000000000E+00 -.17855249612219E+01
 0.97806222665367E-03 0.46176040476747E+00 0.80000000000000E+04 0.30000000000000E+04 0.17325002138346E+02
 0.64968758018799E+01 0.35409654574111E+03 0.29515046752830E+03 0.34898726459210E+03 0.38552914350753E+03
 0.29515006218645E+03 0.29515010781586E+03 0.34179552499502E+03 0.38539008075314E+03 0.29515005020257E+03
 0.29515010748931E+03 0.34898726459210E+03 0.38552914350753E+03 0.29515006218645E+03 0.29515010781586E+03
 0.34179552499502E+03 0.38539008075314E+03 0.29515005020257E+03 0.29515010748931E+03 0.43635911425266E+03
 0.32221338235122E+03 0.25846575681453E+04 0.22814416683511E+04 0.69393103931362E+03 0.12488712098279E+04
 0.55147051531768E+03 0.16370024399921E+04 0.14417567015436E+04 0.14224722184888E+04 0.23066625971123E+04
 0.14261575548767E+04 0.14373231066814E+04 0.12566927358601E+04 0.23035779823046E+04 0.16370024399921E+04
 0.14417567015436E+04 0.14224722184888E+04 0.23066625971123E+04 0.14261575548767E+04 0.14373231066814E+04
 0.12566927358601E+04 0.23035779823045E+04 0.20962651545053E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52151917215427E+03 0.18908931198004E+01
 0.18908931198004E+01 0.54007357119090E+01 0.43711192542117E-01 0.30851676503869E+03 0.30815880514902E+03
 0.30816923638480E+03 0.30816922463413E+03 0.22305100308733E+00 0.00000000000000E+00 0.22304793563439E+00
 0.00000000000000E+00 0.92642892532864E+01 0.40417086063661E-02 0.00000000000000E+00 0.19793609038017E+04
 0.74226033892564E+03 0.80000000000000E+04 0.30000000000000E+04 0.32221773190863E+03 0.43635618168768E+03
 0.29785947687308E+03 0.29779499412199E+03 0.29515000019136E+03 0.29515000123606E+03 0.29785256386783E+03
 0.29776463478177E+03 0.29515000019318E+03 0.29515000123412E+03 0.29785947687308E+03 0.29779499412199E+03
 0.29515000019136E+03 0.29515000123606E+03 0.29785256386783E+03 0.29776463478177E+03 0.29515000019318E+03
 0.29515000123412E+03 0.29775100265300E+03 0.29515000659013E+03 -.14132703633481E+03 -.19877224515512E+03
 0.85737245504056E+02 0.15217299068120E+03 0.66007058949622E+02 0.76039375117145E+02 0.56069311329816E+02
 0.76039375117145E+02 0.10362525915484E+03 0.75815392298252E+02 0.54486433269573E+02 0.75815392298252E+02
 0.10221172744906E+03 0.76039375117145E+02 0.56069311329815E+02 0.76039375117145E+02 0.10362525915483E+03
 0.75815392298250E+02 0.54486433269573E+02 0.75815392298250E+02 0.10221172744906E+03 0.19497130218326E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31739376076808E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.92978446859837E+00 0.91662662825432E+00 0.00000000000000E+00 0.92978446859837E+00 0.91662662825432E+00
 0.13836070162284E+01 0.35360704483859E+00 0.10299999713898E+01 0.15449999570847E+01 0.12871103426141E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.11080566761760E-03 0.85946539428915E-04
 0.85122714259707E-02 0.66025528068349E-02 0.12871103426141E-01 0.15311576439852E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    730.00000000
 0.70753768297943E+00 0.30599064600777E+03 0.49497097527415E+03 0.45040074050820E+03 0.43204047224253E+03
 0.22988876032544E+00 0.00000000000000E+00 0.19385209870008E+00 0.00000000000000E+00 -.17876497176045E+01
 0.97777544079157E-03 0.46134802366002E+00 0.80000000000000E+04 0.30000000000000E+04 0.17340488285901E+02
 0.65026831072128E+01 0.35454176870523E+03 0.29515052801854E+03 0.34939802301697E+03 0.38613212263994E+03
 0.29515007142667E+03 0.29515012391987E+03 0.34217478480327E+03 0.38599252405373E+03 0.29515005770615E+03
 0.29515012354783E+03 0.34939802301697E+03 0.38613212263994E+03 0.29515007142667E+03 0.29515012391987E+03
 0.34217478480327E+03 0.38599252405373E+03 0.29515005770615E+03 0.29515012354783E+03 0.43690817755536E+03
 0.32275811012196E+03 0.25865930039811E+04 0.22806146741805E+04 0.69095360412388E+03 0.12429008634390E+04
 0.54849249129451E+03 0.16386173165790E+04 0.14414080671271E+04 0.14221446388559E+04 0.23031193008069E+04
 0.14282409264564E+04 0.14369864720418E+04 0.12571155389780E+04 0.23000503585976E+04 0.16386173165790E+04
 0.14414080671271E+04 0.14221446388559E+04 0.23031193008069E+04 0.14282409264564E+04 0.14369864720417E+04
 0.12571155389780E+04 0.23000503585976E+04 0.20929467759912E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52178607307557E+03 0.18908931404525E+01
 0.18908931404525E+01 0.54807357119090E+01 0.40078524575275E-01 0.30872222300823E+03 0.30834704884735E+03
 0.30835707313190E+03 0.30835706127501E+03 0.22295955292068E+00 0.00000000000000E+00 0.22295682000537E+00
 0.00000000000000E+00 0.92776687396211E+01 0.44080438601883E-02 0.00000000000000E+00 0.18148639745291E+04
 0.68057399044842E+03 0.80000000000000E+04 0.30000000000000E+04 0.32276255439834E+03 0.43690501332692E+03
 0.29784227449316E+03 0.29785722149786E+03 0.29515000022292E+03 0.29515000142454E+03 0.29783532805488E+03
 0.29782592022719E+03 0.29515000022499E+03 0.29515000142227E+03 0.29784227449316E+03 0.29785722149786E+03
 0.29515000022292E+03 0.29515000142454E+03 0.29783532805488E+03 0.29782592022719E+03 0.29515000022499E+03
 0.29515000142227E+03 0.29781366057201E+03 0.29515000750000E+03 -.14459304632586E+03 -.20390419256071E+03
 0.87545424595670E+02 0.15492209473385E+03 0.66938943015200E+02 0.78149434495445E+02 0.57286874203099E+02
 0.78149434495445E+02 0.10550591024794E+03 0.77934392096161E+02 0.55671341294807E+02 0.77934392096161E+02
 0.10406534685383E+03 0.78149434495445E+02 0.57286874203097E+02 0.78149434495445E+02 0.10550591024794E+03
 0.77934392096153E+02 0.55671341294805E+02 0.77934392096153E+02 0.10406534685383E+03 0.19507849917359E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31756196687075E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93005701051623E+00 0.91679230935484E+00 0.00000000000000E+00 0.93005701051623E+00 0.91679230935484E+00
 0.13837688128795E+01 0.35376884148972E+00 0.10299999713898E+01 0.15449999570847E+01 0.12829180294697E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.12247814844225E-03 0.81820050412429E-04
 0.91109128387082E-02 0.60864354764299E-02 0.12829180294697E-01 0.15401646513993E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    740.00000000
 0.70785974049083E+00 0.30609133151888E+03 0.49527167130245E+03 0.45063395589406E+03 0.43223783904042E+03
 0.22988713825569E+00 0.00000000000000E+00 0.19386018135108E+00 0.00000000000000E+00 -.17899572616550E+01
 0.97749097673258E-03 0.46094055844102E+00 0.80000000000000E+04 0.30000000000000E+04 0.17355817042999E+02
 0.65084313911245E+01 0.35498350387019E+03 0.29515059475676E+03 0.34980565963467E+03 0.38672938919653E+03
 0.29515008179304E+03 0.29515014199543E+03 0.34255140392421E+03 0.38658926479901E+03 0.29515006613071E+03
 0.29515014157280E+03 0.34980565963467E+03 0.38672938919653E+03 0.29515008179304E+03 0.29515014199543E+03
 0.34255140392421E+03 0.38658926479901E+03 0.29515006613071E+03 0.29515014157280E+03 0.43745172069446E+03
 0.32329895649584E+03 0.25885045831847E+04 0.22797838911058E+04 0.68798996954237E+03 0.12369793544189E+04
 0.54554943502880E+03 0.16402162814358E+04 0.14410457107587E+04 0.14218149560770E+04 0.22995956826216E+04
 0.14303052398456E+04 0.14366360875828E+04 0.12575294786673E+04 0.22965422914544E+04 0.16402162814358E+04
 0.14410457107587E+04 0.14218149560770E+04 0.22995956826216E+04 0.14303052398456E+04 0.14366360875828E+04
 0.12575294786673E+04 0.22965422914544E+04 0.20896482709407E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52205032005169E+03 0.18908931628813E+01
 0.18908931628813E+01 0.55607357119090E+01 0.36697050736672E-01 0.30892436665784E+03 0.30853996835418E+03
 0.30854937254355E+03 0.30854936112773E+03 0.22286857517434E+00 0.00000000000000E+00 0.22286611371532E+00
 0.00000000000000E+00 0.92863502497078E+01 0.48142257083179E-02 0.00000000000000E+00 0.16617417804441E+04
 0.62315316766654E+03 0.80000000000000E+04 0.30000000000000E+04 0.32330348286996E+03 0.43744835016369E+03
 0.29777703255717E+03 0.29792007332651E+03 0.29515000025897E+03 0.29515000163686E+03 0.29777001584776E+03
 0.29788782759412E+03 0.29515000026130E+03 0.29515000163420E+03 0.29777703255717E+03 0.29792007332651E+03
 0.29515000025897E+03 0.29515000163686E+03 0.29777001584776E+03 0.29788782759412E+03 0.29515000026130E+03
 0.29515000163420E+03 0.29787691810497E+03 0.29515000851245E+03 -.14784993883417E+03 -.20903716095094E+03
 0.89343108371604E+02 0.15770489523568E+03 0.67915071322220E+02 0.80510180412263E+02 0.58496595146572E+02
 0.80510180412263E+02 0.10741144646360E+03 0.80303603950102E+02 0.56848690037736E+02 0.80303603950102E+02
 0.10594421000078E+03 0.80510180412263E+02 0.58496595146569E+02 0.80510180412263E+02 0.10741144646359E+03
 0.80303603950091E+02 0.56848690037734E+02 0.80303603950091E+02 0.10594421000078E+03 0.19533883367166E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31775015042873E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93026878094215E+00 0.91703997882816E+00 0.00000000000000E+00 0.93026878094215E+00 0.91703997882816E+00
 0.13839298416352E+01 0.35392987024542E+00 0.10299999713898E+01 0.15449999570847E+01 0.12822624611509E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13342026210841E-03 0.77248183081737E-04
 0.96130730302473E-02 0.55658144698837E-02 0.12822624611509E-01 0.15389555945321E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    740.05000000
 0.70786129976724E+00 0.30609185657159E+03 0.49527319723861E+03 0.45063514733985E+03 0.43223885290698E+03
 0.22988713037827E+00 0.00000000000000E+00 0.19386022160791E+00 0.00000000000000E+00 -.17897056962365E+01
 0.97748945803035E-03 0.46093850598437E+00 0.80000000000000E+04 0.30000000000000E+04 0.17355894324592E+02
 0.65084603717220E+01 0.35498565378526E+03 0.29515059509060E+03 0.34980765420651E+03 0.38673211144700E+03
 0.29515008184490E+03 0.29515014208586E+03 0.34255329213409E+03 0.38659198538190E+03 0.29515006617286E+03
 0.29515014166297E+03 0.34980765420652E+03 0.38673211144700E+03 0.29515008184490E+03 0.29515014208586E+03
 0.34255329213409E+03 0.38659198538190E+03 0.29515006617286E+03 0.29515014166297E+03 0.43745347601016E+03
 0.32330084323894E+03 0.25885129043517E+04 0.22797791669241E+04 0.68799129386760E+03 0.12369770443136E+04
 0.54554579397666E+03 0.16402234174292E+04 0.14410454074693E+04 0.14218128693380E+04 0.22995823379892E+04
 0.14303142749601E+04 0.14366358664082E+04 0.12575303617798E+04 0.22965290384453E+04 0.16402234174292E+04
 0.14410454074693E+04 0.14218128693379E+04 0.22995823379892E+04 0.14303142749601E+04 0.14366358664082E+04
 0.12575303617798E+04 0.22965290384453E+04 0.20896400608653E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52205166447782E+03 0.18908931604361E+01
 0.18908931604361E+01 0.55611357119090E+01 0.36680778046794E-01 0.30892553188630E+03 0.30854093446141E+03
 0.30855033934993E+03 0.30855032792727E+03 0.22286812139877E+00 0.00000000000000E+00 0.22286566103102E+00
 0.00000000000000E+00 0.92868494549023E+01 0.48163614415333E-02 0.00000000000000E+00 0.16610049094350E+04
 0.62287684103811E+03 0.80000000000000E+04 0.30000000000000E+04 0.32330537021434E+03 0.43745010422349E+03
 0.29777631913986E+03 0.29792051071588E+03 0.29515000025915E+03 0.29515000163792E+03 0.29776930215220E+03
 0.29788825865528E+03 0.29515000026148E+03 0.29515000163527E+03 0.29777631913986E+03 0.29792051071588E+03
 0.29515000025915E+03 0.29515000163792E+03 0.29776930215220E+03 0.29788825865528E+03 0.29515000026148E+03
 0.29515000163527E+03 0.29787735783771E+03 0.29515000851752E+03 -.14785753162327E+03 -.20904866558588E+03
 0.89373599710280E+02 0.15773898515217E+03 0.67918517443338E+02 0.80544241940630E+02 0.58516778568456E+02
 0.80544241940630E+02 0.10743406753758E+03 0.80337799408193E+02 0.56868276454542E+02 0.80337799408193E+02
 0.10596627001743E+03 0.80544241940630E+02 0.58516778568454E+02 0.80544241940630E+02 0.10743406753757E+03
 0.80337799408182E+02 0.56868276454540E+02 0.80337799408182E+02 0.10596627001743E+03 0.19546194708466E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31775048853083E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93034499127988E+00 0.91693011484462E+00 0.00000000000000E+00 0.93034499127988E+00 0.91693011484462E+00
 0.13839306212734E+01 0.35393064988362E+00 0.10299999713898E+01 0.15449999570847E+01 0.12821045633049E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.13351486657879E-03 0.77248771736561E-04
 0.96182406627855E-02 0.55649029692762E-02 0.12821045633049E-01 0.15393907270377E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    750.67887523
 0.70819603237679E+00 0.30620477755893E+03 0.49559591477250E+03 0.45088723079184E+03 0.43245373425181E+03
 0.22988551608235E+00 0.00000000000000E+00 0.19386872509160E+00 0.00000000000000E+00 -.17925064824531E+01
 0.97715463014272E-03 0.46050489619820E+00 0.80000000000000E+04 0.30000000000000E+04 0.17372236573478E+02
 0.65145887150542E+01 0.35545034310475E+03 0.29515066992555E+03 0.35023685165639E+03 0.38735344171108E+03
 0.29515009357063E+03 0.29515016253679E+03 0.34295152360461E+03 0.38721279722650E+03 0.29515007570593E+03
 0.29515016205692E+03 0.35023685165639E+03 0.38735344171108E+03 0.29515009357063E+03 0.29515016253679E+03
 0.34295152360461E+03 0.38721279722651E+03 0.29515007570593E+03 0.29515016205692E+03 0.43799847094206E+03
 0.32384474271317E+03 0.25905415170692E+04 0.22789807461877E+04 0.68538954586644E+03 0.12316415092858E+04
 0.54282501569006E+03 0.16419152144029E+04 0.14407688422856E+04 0.14215180741524E+04 0.22961094038945E+04
 0.14324834080869E+04 0.14363724068040E+04 0.12579928729960E+04 0.22930726646499E+04 0.16419152144029E+04
 0.14407688422856E+04 0.14215180741524E+04 0.22961094038945E+04 0.14324834080869E+04 0.14363724068040E+04
 0.12579928729960E+04 0.22930726646499E+04 0.20865171609393E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52233617538375E+03 0.18908931876591E+01
 0.18908931876591E+01 0.56461667137102E+01 0.33351850722062E-01 0.30914106915933E+03 0.30874678537871E+03
 0.30875555210790E+03 0.30875554117497E+03 0.22277180335832E+00 0.00000000000000E+00 0.22276960158427E+00
 0.00000000000000E+00 0.92958157145371E+01 0.52970936095603E-02 0.00000000000000E+00 0.15102621531101E+04
 0.56634830741627E+03 0.80000000000000E+04 0.30000000000000E+04 0.32384947635762E+03 0.43799475904932E+03
 0.29775018315855E+03 0.29799199023505E+03 0.29515000030029E+03 0.29515000187859E+03 0.29774314496573E+03
 0.29795868799020E+03 0.29515000030294E+03 0.29515000187549E+03 0.29775018315855E+03 0.29799199023505E+03
 0.29515000030029E+03 0.29515000187859E+03 0.29774314496573E+03 0.29795868799020E+03 0.29515000030294E+03
 0.29515000187549E+03 0.29794924806668E+03 0.29515000965791E+03 -.15110503264546E+03 -.21412504800558E+03
 0.91169864860075E+02 0.16055514398921E+03 0.68929429804835E+02 0.82743857550842E+02 0.59726213828280E+02
 0.82743857550842E+02 0.10936423230747E+03 0.82547570661398E+02 0.58046016823137E+02 0.82547570661398E+02
 0.10787106841072E+03 0.82743857550842E+02 0.59726213828278E+02 0.82743857550842E+02 0.10936423230746E+03
 0.82547570661391E+02 0.58046016823136E+02 0.82547570661391E+02 0.10787106841072E+03 0.19599205567561E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31795363690559E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93052738499377E+00 0.91728382761287E+00 0.00000000000000E+00 0.93052738499377E+00 0.91728382761287E+00
 0.13840979875782E+01 0.35409801618840E+00 0.10299999713898E+01 0.15449999570847E+01 0.12815539704569E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.14473032864997E-03 0.72333109011398E-04
 0.10107954070898E-01 0.50517382950238E-02 0.12815539704569E-01 0.15376755803583E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    761.31850266
 0.70853183796035E+00 0.30631272463021E+03 0.49591088907059E+03 0.45113211042897E+03 0.43266161154402E+03
 0.22988400927357E+00 0.00000000000000E+00 0.19387718580861E+00 0.00000000000000E+00 -.17950252532989E+01
 0.97682304715295E-03 0.46008051577741E+00 0.80000000000000E+04 0.30000000000000E+04 0.17388260805790E+02
 0.65205978021713E+01 0.35591339222174E+03 0.29515075277113E+03 0.35066447270686E+03 0.38797558872445E+03
 0.29515010676537E+03 0.29515018556025E+03 0.34334759203244E+03 0.38783441555657E+03 0.29515008644137E+03
 0.29515018501679E+03 0.35066447270686E+03 0.38797558872445E+03 0.29515010676537E+03 0.29515018556025E+03
 0.34334759203244E+03 0.38783441555657E+03 0.29515008644137E+03 0.29515018501679E+03 0.43855870822079E+03
 0.32440334537317E+03 0.25925274935230E+04 0.22781040608456E+04 0.68239682510883E+03 0.12256640400503E+04
 0.53985523081589E+03 0.16435833399132E+04 0.14403787897708E+04 0.14211724627355E+04 0.22924517579862E+04
 0.14346364169965E+04 0.14359951386144E+04 0.12584173340411E+04 0.22894313286920E+04 0.16435833399132E+04
 0.14403787897708E+04 0.14211724627355E+04 0.22924517579862E+04 0.14346364169965E+04 0.14359951386144E+04
 0.12584173340411E+04 0.22894313286920E+04 0.20831305022758E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52261331595834E+03 0.18908932121409E+01
 0.18908932121409E+01 0.57312837331717E+01 0.30275359752280E-01 0.30935502716888E+03 0.30895212400472E+03
 0.30896025603022E+03 0.30896024565258E+03 0.22267573698968E+00 0.00000000000000E+00 0.22267376656466E+00
 0.00000000000000E+00 0.93046935840755E+01 0.58353680057372E-02 0.00000000000000E+00 0.13709503825868E+04
 0.51410639347004E+03 0.80000000000000E+04 0.30000000000000E+04 0.32440811796549E+03 0.43855485861345E+03
 0.29775989822884E+03 0.29806152588928E+03 0.29515000034742E+03 0.29515000215051E+03 0.29775286953445E+03
 0.29802720056533E+03 0.29515000035040E+03 0.29515000214690E+03 0.29775989822884E+03 0.29806152588928E+03
 0.29515000034742E+03 0.29515000215051E+03 0.29775286953445E+03 0.29802720056533E+03 0.29515000035040E+03
 0.29515000214690E+03 0.29801916404609E+03 0.29515001093118E+03 -.15447255973277E+03 -.21942530100389E+03
 0.93036220201003E+02 0.16345547318963E+03 0.69954071887624E+02 0.84802694251228E+02 0.60980614994271E+02
 0.84802694251228E+02 0.11134980159495E+03 0.84617043472198E+02 0.59267159678701E+02 0.84617043472198E+02
 0.10982958784984E+03 0.84802694251227E+02 0.60980614994269E+02 0.84802694251227E+02 0.11134980159495E+03
 0.84617043472192E+02 0.59267159678700E+02 0.84617043472192E+02 0.10982958784984E+03 0.19632243858519E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31815747248698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93073650827313E+00 0.91756575817244E+00 0.00000000000000E+00 0.93073650827313E+00 0.91756575817244E+00
 0.13842658903699E+01 0.35426591898018E+00 0.10299999713898E+01 0.15449999570847E+01 0.12809825663919E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.15604324348146E-03 0.67667792131345E-04
 0.10557989284382E-01 0.45784476679726E-02 0.12809825663919E-01 0.15360147987968E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    770.08477606
 0.70880388770122E+00 0.30639963451373E+03 0.49616674547353E+03 0.45133085680482E+03 0.43283010810951E+03
 0.22988281792445E+00 0.00000000000000E+00 0.19388412214201E+00 0.00000000000000E+00 -.17970310095621E+01
 0.97655091662016E-03 0.45973582878336E+00 0.80000000000000E+04 0.30000000000000E+04 0.17401297656463E+02
 0.65254866211737E+01 0.35629216127416E+03 0.29515082739198E+03 0.35101432369105E+03 0.38848403484985E+03
 0.29515011882445E+03 0.29515020661017E+03 0.34367173278211E+03 0.38834243209023E+03 0.29515009625938E+03
 0.29515020600899E+03 0.35101432369106E+03 0.38848403484985E+03 0.29515011882445E+03 0.29515020661017E+03
 0.34367173278211E+03 0.38834243209023E+03 0.29515009625938E+03 0.29515020600899E+03 0.43901701549418E+03
 0.32486188888212E+03 0.25941356288069E+04 0.22773545745213E+04 0.67990698363384E+03 0.12207168070866E+04
 0.53741028853462E+03 0.16449390711021E+04 0.14400264295476E+04 0.14208685435249E+04 0.22894186919312E+04
 0.14363901012601E+04 0.14356533064796E+04 0.12587447194969E+04 0.22864116366241E+04 0.16449390711021E+04
 0.14400264295476E+04 0.14208685435249E+04 0.22894186919312E+04 0.14363901012601E+04 0.14356533064796E+04
 0.12587447194969E+04 0.22864116366241E+04 0.20803177023587E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52283831159135E+03 0.18908932316364E+01
 0.18908932316364E+01 0.58014139203773E+01 0.27920692868950E-01 0.30953079625855E+03 0.30912086506985E+03
 0.30912849544506E+03 0.30912848552753E+03 0.22259684625386E+00 0.00000000000000E+00 0.22259504991945E+00
 0.00000000000000E+00 0.93119395761781E+01 0.63274883096304E-02 0.00000000000000E+00 0.12643247381151E+04
 0.47412177679317E+03 0.80000000000000E+04 0.30000000000000E+04 0.32486667367068E+03 0.43901307050587E+03
 0.29777736633878E+03 0.29811902010564E+03 0.29515000039120E+03 0.29515000239995E+03 0.29777035919962E+03
 0.29808385206123E+03 0.29515000039449E+03 0.29515000239586E+03 0.29777736633878E+03 0.29811902010564E+03
 0.29515000039120E+03 0.29515000239995E+03 0.29777035919962E+03 0.29808385206123E+03 0.29515000039449E+03
 0.29515000239586E+03 0.29807695710689E+03 0.29515001208695E+03 -.15724711349468E+03 -.22379483890999E+03
 0.94574421870681E+02 0.16584258635728E+03 0.70795292377245E+02 0.86448945485199E+02 0.62013639444269E+02
 0.86448945485199E+02 0.11298344324714E+03 0.86272167141614E+02 0.60272810566703E+02 0.86272167141614E+02
 0.11144098673102E+03 0.86448945485197E+02 0.62013639444267E+02 0.86448945485197E+02 0.11298344324713E+03
 0.86272167141610E+02 0.60272810566702E+02 0.86272167141610E+02 0.11144098673102E+03 0.19656275721531E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31832586257159E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93091147678557E+00 0.91778402480534E+00 0.00000000000000E+00 0.93091147678557E+00 0.91778402480534E+00
 0.13844019152404E+01 0.35440194385061E+00 0.10299999713898E+01 0.15449999570847E+01 0.12804937940561E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.16535998569357E-03 0.63969133693963E-04
 0.10900704703429E-01 0.42169127773404E-02 0.12804937940561E-01 0.15346946600157E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    781.61690781
 0.70915459580942E+00 0.30651240195505E+03 0.49649957143324E+03 0.45158947997315E+03 0.43304933253056E+03
 0.22988129526867E+00 0.00000000000000E+00 0.19389317684512E+00 0.00000000000000E+00 -.17996147218794E+01
 0.97619364913487E-03 0.45928823308832E+00 0.80000000000000E+04 0.30000000000000E+04 0.17418255952710E+02
 0.65318459822661E+01 0.35678656036192E+03 0.29515093484867E+03 0.35147108089228E+03 0.38914655257064E+03
 0.29515013645125E+03 0.29515023739038E+03 0.34409519129162E+03 0.38900439557531E+03 0.29515011062030E+03
 0.29515023670548E+03 0.35147108089228E+03 0.38914655257064E+03 0.29515013645125E+03 0.29515023739038E+03
 0.34409519129162E+03 0.38900439557531E+03 0.29515011062030E+03 0.29515023670548E+03 0.43961291551291E+03
 0.32546008444632E+03 0.25962225775900E+04 0.22763533129022E+04 0.67665927126178E+03 0.12142806809258E+04
 0.53423811330770E+03 0.16467024429859E+04 0.14395420761230E+04 0.14204569021329E+04 0.22854410264841E+04
 0.14386734341873E+04 0.14351828321037E+04 0.12591564635562E+04 0.22824514926193E+04 0.16467024429859E+04
 0.14395420761230E+04 0.14204569021329E+04 0.22854410264841E+04 0.14386734341873E+04 0.14351828321037E+04
 0.12591564635562E+04 0.22824514926193E+04 0.20766364404130E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52313097701683E+03 0.18908932567495E+01
 0.18908932567495E+01 0.58936709743687E+01 0.25054795198492E-01 0.30976143355159E+03 0.30934229856045E+03
 0.30934929945469E+03 0.30934929013987E+03 0.22249342147745E+00 0.00000000000000E+00 0.22249183451191E+00
 0.00000000000000E+00 0.93214116733407E+01 0.70512588876645E-02 0.00000000000000E+00 0.11345491815646E+04
 0.42545594308673E+03 0.80000000000000E+04 0.30000000000000E+04 0.32546491184398E+03 0.43960882042216E+03
 0.29780858476720E+03 0.29819524900251E+03 0.29515000045633E+03 0.29515000276599E+03 0.29780162152311E+03
 0.29815896877334E+03 0.29515000046005E+03 0.29515000276119E+03 0.29780858476720E+03 0.29819524900251E+03
 0.29515000045633E+03 0.29515000276599E+03 0.29780162152311E+03 0.29815896877334E+03 0.29515000046005E+03
 0.29515000276119E+03 0.29815356096904E+03 0.29515001376476E+03 -.16087796134936E+03 -.22951108646037E+03
 0.96589422785564E+02 0.16896809633999E+03 0.71895726440503E+02 0.88564728791594E+02 0.63365889006997E+02
 0.88564728791594E+02 0.11512180781206E+03 0.88399656185248E+02 0.61589301593154E+02 0.88399656185248E+02
 0.11355038402329E+03 0.88564728791597E+02 0.63365889006993E+02 0.88564728791597E+02 0.11512180781205E+03
 0.88399656185235E+02 0.61589301593148E+02 0.88399656185235E+02 0.11355038402328E+03 0.19687396292156E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31854797956242E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93114608238636E+00 0.91805747228876E+00 0.00000000000000E+00 0.93114608238636E+00 0.91805747228876E+00
 0.13845772692945E+01 0.35457729790471E+00 0.10299999713898E+01 0.15449999570847E+01 0.12798274646801E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.17754563543259E-03 0.59274646098023E-04
 0.11315615146412E-01 0.37777841260404E-02 0.12798274646801E-01 0.15330219553983E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    791.22701760
 0.70944128932003E+00 0.30660549822907E+03 0.49677412179707E+03 0.45180296396630E+03 0.43323034819176E+03
 0.22988005344249E+00 0.00000000000000E+00 0.19390064394339E+00 0.00000000000000E+00 -.18017510185069E+01
 0.97589654782420E-03 0.45892000649233E+00 0.80000000000000E+04 0.30000000000000E+04 0.17432231950719E+02
 0.65370869815198E+01 0.35719526829820E+03 0.29515103295226E+03 0.35184876074421E+03 0.38969323179943E+03
 0.29515015278841E+03 0.29515026592876E+03 0.34444557592158E+03 0.38955062244750E+03 0.29515012393981E+03
 0.29515026516685E+03 0.35184876074421E+03 0.38969323179943E+03 0.29515015278841E+03 0.29515026592876E+03
 0.34444557592158E+03 0.38955062244750E+03 0.29515012393981E+03 0.29515026516685E+03 0.44010352805826E+03
 0.32595398582324E+03 0.25979414040566E+04 0.22755139704813E+04 0.67398275753907E+03 0.12089890975492E+04
 0.53163642622241E+03 0.16481572968171E+04 0.14391269237538E+04 0.14201094011455E+04 0.22821466573758E+04
 0.14405584153268E+04 0.14347792620086E+04 0.12594886776207E+04 0.22791716536771E+04 0.16481572968171E+04
 0.14391269237538E+04 0.14201094011455E+04 0.22821466573758E+04 0.14405584153268E+04 0.14347792620086E+04
 0.12594886776207E+04 0.22791716536771E+04 0.20735945347464E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52337243228339E+03 0.18908932775137E+01
 0.18908932775137E+01 0.59705518526949E+01 0.22854899013276E-01 0.30995304742547E+03 0.30952637363418E+03
 0.30953287469179E+03 0.30953286587876E+03 0.22240754295721E+00 0.00000000000000E+00 0.22240611422859E+00
 0.00000000000000E+00 0.93292235924371E+01 0.77299767800592E-02 0.00000000000000E+00 0.10349319574462E+04
 0.38809948404231E+03 0.80000000000000E+04 0.30000000000000E+04 0.32595886512670E+03 0.44009929800886E+03
 0.29783975086551E+03 0.29825928649364E+03 0.29515000051780E+03 0.29515000310664E+03 0.29783283547138E+03
 0.29822207645371E+03 0.29515000052191E+03 0.29515000310116E+03 0.29783975086551E+03 0.29825928649364E+03
 0.29515000051780E+03 0.29515000310664E+03 0.29783283547139E+03 0.29822207645371E+03 0.29515000052191E+03
 0.29515000310116E+03 0.29821789691080E+03 0.29515001530927E+03 -.16388550593918E+03 -.23424377739250E+03
 0.98258701169968E+02 0.17155753209934E+03 0.72807537423525E+02 0.90292972159777E+02 0.64485321867443E+02
 0.90292972159777E+02 0.11689297444554E+03 0.90137606843449E+02 0.62679203703379E+02 0.90137606843449E+02
 0.11529771471519E+03 0.90292972159780E+02 0.64485321867439E+02 0.90292972159780E+02 0.11689297444553E+03
 0.90137606843433E+02 0.62679203703373E+02 0.90137606843433E+02 0.11529771471517E+03 0.19712806913774E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31873360943942E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93134024148639E+00 0.91828281478395E+00 0.00000000000000E+00 0.93134024148639E+00 0.91828281478395E+00
 0.13847206160498E+01 0.35472064466002E+00 0.10299999713898E+01 0.15449999570847E+01 0.12792706493220E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.18760130266316E-03 0.55501989819417E-04
 0.11631900652148E-01 0.34413067628597E-02 0.12792706493220E-01 0.15316310707491E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    801.35786638
 0.70973935299093E+00 0.30670168631411E+03 0.49705977899981E+03 0.45202490248670E+03 0.43341832646794E+03
 0.22987876696006E+00 0.00000000000000E+00 0.19390842858477E+00 0.00000000000000E+00 -.18040311238889E+01
 0.97558801968732E-03 0.45853746906936E+00 0.80000000000000E+04 0.30000000000000E+04 0.17446774886765E+02
 0.65425405825369E+01 0.35762325397902E+03 0.29515114673413E+03 0.35224424144895E+03 0.39026653603940E+03
 0.29515017204322E+03 0.29515029957579E+03 0.34481225875568E+03 0.39012344945609E+03 0.29515013964980E+03
 0.29515029872384E+03 0.35224424144895E+03 0.39026653603940E+03 0.29515017204322E+03 0.29515029957579E+03
 0.34481225875568E+03 0.39012344945609E+03 0.29515013964980E+03 0.29515029872384E+03 0.44062365282765E+03
 0.32647830695781E+03 0.25997388216919E+04 0.22746168687709E+04 0.67102954561622E+03 0.12032103430755E+04
 0.52882564973119E+03 0.16496810441469E+04 0.14386546996306E+04 0.14197341056241E+04 0.22786390564240E+04
 0.14425366839991E+04 0.14343191039237E+04 0.12598312136897E+04 0.22756792239446E+04 0.16496810441469E+04
 0.14386546996306E+04 0.14197341056241E+04 0.22786390564240E+04 0.14425366839991E+04 0.14343191039237E+04
 0.12598312136897E+04 0.22756792239445E+04 0.20703207828523E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52362353167501E+03 0.18908932996758E+01
 0.18908932996758E+01 0.60515986429892E+01 0.20708890528387E-01 0.31015383731500E+03 0.30971991646275E+03
 0.30972590714237E+03 0.30972589887666E+03 0.22231731770368E+00 0.00000000000000E+00 0.22231603458826E+00
 0.00000000000000E+00 0.93371826312099E+01 0.85310137386482E-02 0.00000000000000E+00 0.93775490757417E+03
 0.35165809034031E+03 0.80000000000000E+04 0.30000000000000E+04 0.32648322233488E+03 0.44061930960458E+03
 0.29787567791372E+03 0.29832600345346E+03 0.29515000059174E+03 0.29515000350992E+03 0.29786881637127E+03
 0.29828782544341E+03 0.29515000059630E+03 0.29515000350362E+03 0.29787567791372E+03 0.29832600345347E+03
 0.29515000059174E+03 0.29515000350992E+03 0.29786881637127E+03 0.29828782544341E+03 0.29515000059630E+03
 0.29515000350362E+03 0.29828491838763E+03 0.29515001711679E+03 -.16710092308699E+03 -.23931622184227E+03
 0.10003174861980E+03 0.17430528807325E+03 0.73773380710349E+02 0.92104799624284E+02 0.65673314907952E+02
 0.92104799624284E+02 0.11877180726437E+03 0.91959505404177E+02 0.63835754631758E+02 0.91959505404177E+02
 0.11715106338863E+03 0.92104799624287E+02 0.65673314907949E+02 0.92104799624287E+02 0.11877180726436E+03
 0.91959505404162E+02 0.63835754631753E+02 0.91959505404162E+02 0.11715106338862E+03 0.19726424375299E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31892990517415E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93151966423665E+00 0.91854724006101E+00 0.00000000000000E+00 0.93151966423665E+00 0.91854724006101E+00
 0.13848696478852E+01 0.35486967649547E+00 0.10299999713898E+01 0.15449999570847E+01 0.12787516168092E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.19813235694313E-03 0.51674908723589E-04
 0.11936681437303E-01 0.31132064103605E-02 0.12787516168092E-01 0.15299695113330E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    813.93147613
 0.71010140944527E+00 0.30681870821221E+03 0.49740971340233E+03 0.45229673293130E+03 0.43364837730106E+03
 0.22987719329784E+00 0.00000000000000E+00 0.19391795637348E+00 0.00000000000000E+00 -.18066657051535E+01
 0.97521153698138E-03 0.45806999835445E+00 0.80000000000000E+04 0.30000000000000E+04 0.17464579712137E+02
 0.65492173920516E+01 0.35814972439301E+03 0.29515130308175E+03 0.35273091854945E+03 0.39096956156819E+03
 0.29515019896484E+03 0.29515034663703E+03 0.34526403954597E+03 0.39082590045492E+03 0.29515016163297E+03
 0.29515034566031E+03 0.35273091854945E+03 0.39096956156819E+03 0.29515019896484E+03 0.29515034663703E+03
 0.34526403954597E+03 0.39082590045492E+03 0.29515016163297E+03 0.29515034566031E+03 0.44125540611814E+03
 0.32711838867040E+03 0.26019221043158E+04 0.22734682521676E+04 0.66748860111659E+03 0.11962746767494E+04
 0.52544863262719E+03 0.16515397041402E+04 0.14380430149598E+04 0.14192425236677E+04 0.22743072857231E+04
 0.14449532720194E+04 0.14337225796752E+04 0.12602189149600E+04 0.22713663047312E+04 0.16515397041402E+04
 0.14380430149598E+04 0.14192425236677E+04 0.22743072857231E+04 0.14449532720194E+04 0.14337225796752E+04
 0.12602189149600E+04 0.22713663047312E+04 0.20663135408809E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52393103051173E+03 0.18908933252833E+01
 0.18908933252833E+01 0.61521875209169E+01 0.18275694911674E-01 0.31040308972423E+03 0.30995942042067E+03
 0.30996482599722E+03 0.30996481836486E+03 0.22220575551287E+00 0.00000000000000E+00 0.22220464261209E+00
 0.00000000000000E+00 0.93472315658911E+01 0.96668181016913E-02 0.00000000000000E+00 0.82757324238886E+03
 0.31033996589582E+03 0.80000000000000E+04 0.30000000000000E+04 0.32712335206974E+03 0.44125092550305E+03
 0.29792534694331E+03 0.29841003090622E+03 0.29515000069752E+03 0.29515000407662E+03 0.29791856700005E+03
 0.29837063734403E+03 0.29515000070269E+03 0.29515000406914E+03 0.29792534694330E+03 0.29841003090622E+03
 0.29515000069752E+03 0.29515000407662E+03 0.29791856700005E+03 0.29837063734403E+03 0.29515000070269E+03
 0.29515000406914E+03 0.29836930669342E+03 0.29515001962526E+03 -.17103339506508E+03 -.24550458493104E+03
 0.10221388431241E+03 0.17768458490481E+03 0.74959631170840E+02 0.94314607130144E+02 0.67134369857402E+02
 0.94314607130144E+02 0.12108178905959E+03 0.94181662492246E+02 0.65258314303770E+02 0.94181662492246E+02
 0.11943004606388E+03 0.94314607130146E+02 0.67134369857400E+02 0.94314607130146E+02 0.12108178905958E+03
 0.94181662492235E+02 0.65258314303767E+02 0.94181662492235E+02 0.11943004606387E+03 0.19753910349612E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31917341238549E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93177994844766E+00 0.91880916833863E+00 0.00000000000000E+00 0.93177994844766E+00 0.91880916833863E+00
 0.13850506761124E+01 0.35505070472263E+00 0.10299999713898E+01 0.15449999570847E+01 0.12779738729047E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.21118578592311E-03 0.47159169888590E-04
 0.12281881127717E-01 0.27426245384921E-02 0.12779738729047E-01 0.15282850622020E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    820.14104108
 0.71027570042670E+00 0.30687977419651E+03 0.49758456542254E+03 0.45243357236828E+03 0.43376503784898E+03
 0.22987641982806E+00 0.00000000000000E+00 0.19392259382223E+00 0.00000000000000E+00 -.18080237929536E+01
 0.97890316141105E-03 0.45783871495707E+00 0.80000000000000E+04 0.30000000000000E+04 0.17473402179958E+02
 0.65525258174843E+01 0.35840691488046E+03 0.29515138443297E+03 0.35296885190020E+03 0.39130938134214E+03
 0.29515021309620E+03 0.29515037134405E+03 0.34548577821108E+03 0.39116545753525E+03 0.29515017317684E+03
 0.29515037030213E+03 0.35296885190020E+03 0.39130938134214E+03 0.29515021309620E+03 0.29515037134405E+03
 0.34548577821108E+03 0.39116545753525E+03 0.29515017317684E+03 0.29515037030213E+03 0.44154983584991E+03
 0.32741904608291E+03 0.26029921586632E+04 0.22729437851074E+04 0.66604672285778E+03 0.11933779519918E+04
 0.52400099551975E+03 0.16524479084164E+04 0.14378078390613E+04 0.14190269551260E+04 0.22723262287498E+04
 0.14461284225236E+04 0.14334951221359E+04 0.12604205219491E+04 0.22693946125366E+04 0.16524479084164E+04
 0.14378078390613E+04 0.14190269551260E+04 0.22723262287498E+04 0.14461284225236E+04 0.14334951221359E+04
 0.12604205219491E+04 0.22693946125366E+04 0.20645480552405E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52408518546547E+03 0.18908933384836E+01
 0.18908933384836E+01 0.62018640405853E+01 0.17162038256860E-01 0.31052641925108E+03 0.31007749563955E+03
 0.31008263193568E+03 0.31008262459504E+03 0.22215083854302E+00 0.00000000000000E+00 0.22214979894873E+00
 0.00000000000000E+00 0.93520509773987E+01 0.10294104361255E-01 0.35749817982151E-02 0.77714386014104E+03
 0.29142894755289E+03 0.22377736311816E+04 0.83916511169310E+03 0.32742408734995E+03 0.44154523035781E+03
 0.29795311360554E+03 0.29845393378409E+03 0.29515000075371E+03 0.29515000437486E+03 0.29794638753708E+03
 0.29841393661670E+03 0.29515000075919E+03 0.29515000436675E+03 0.29795311360554E+03 0.29845393378409E+03
 0.29515000075371E+03 0.29515000437486E+03 0.29794638753708E+03 0.29841393661670E+03 0.29515000075919E+03
 0.29515000436675E+03 0.29841315698393E+03 0.29515002093712E+03 -.17253677377934E+03 -.24803495385069E+03
 0.10305001347017E+03 0.17909403113461E+03 0.75528767597090E+02 0.95539395296815E+02 0.67823318295539E+02
 0.95539395296815E+02 0.12217678753398E+03 0.95412145387374E+02 0.65941367017310E+02 0.95412145387374E+02
 0.12052287175195E+03 0.95539395296817E+02 0.67823318295538E+02 0.95539395296817E+02 0.12217678753398E+03
 0.95412145387364E+02 0.65941367017308E+02 0.95412145387364E+02 0.12052287175195E+03 0.19819410581413E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31929481307347E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93192449580523E+00 0.91893095828194E+00 0.00000000000000E+00 0.93192449580523E+00 0.91893095828194E+00
 0.13851378216031E+01 0.35513785021335E+00 0.10299999713898E+01 0.15449999570847E+01 0.12777905234023E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.21725052717616E-03 0.44944028031247E-04
 0.12434254308254E-01 0.25723549739637E-02 0.12777905234023E-01 0.15268803837425E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    830.60323553
 0.71057284072099E+00 0.30698137682763E+03 0.49787437458368E+03 0.45265991468726E+03 0.43395788666476E+03
 0.22987516577384E+00 0.00000000000000E+00 0.19393032269395E+00 0.00000000000000E+00 -.18103922006949E+01
 0.10165332965117E-02 0.45745831931444E+00 0.78698848600953E+04 0.29512068225357E+04 0.17487932041522E+02
 0.65579745155709E+01 0.35883900277984E+03 0.29515153124890E+03 0.35336849520717E+03 0.39188318864939E+03
 0.29515023889537E+03 0.29515041646058E+03 0.34585762400449E+03 0.39173881344899E+03 0.29515019426350E+03
 0.29515041530031E+03 0.35336849520717E+03 0.39188318864939E+03 0.29515023889537E+03 0.29515041646058E+03
 0.34585762400449E+03 0.39173881344899E+03 0.29515019426350E+03 0.29515041530031E+03 0.44205899870794E+03
 0.32794176515418E+03 0.26046852977292E+04 0.22719459962136E+04 0.66330025322770E+03 0.11879857939377E+04
 0.52136903944384E+03 0.16539041688121E+04 0.14373336529944E+04 0.14185869278752E+04 0.22688603391281E+04
 0.14480486281684E+04 0.14330339272805E+04 0.12607008978140E+04 0.22659445526087E+04 0.16539041688121E+04
 0.14373336529944E+04 0.14185869278752E+04 0.22688603391281E+04 0.14480486281684E+04 0.14330339272804E+04
 0.12607008978140E+04 0.22659445526087E+04 0.20613543940094E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52434057659943E+03 0.18908933615040E+01
 0.18908933615040E+01 0.62855615961104E+01 0.15409319329898E-01 0.31073150108896E+03 0.31027482817793E+03
 0.31027951952374E+03 0.31027951269974E+03 0.22205853775071E+00 0.00000000000000E+00 0.22205761501194E+00
 0.00000000000000E+00 0.93598271002951E+01 0.11464997243592E-01 0.98473651585495E-02 0.69777600727040E+03
 0.26166600272640E+03 0.81240005536450E+03 0.30465002076169E+03 0.32794687598194E+03 0.44205423886438E+03
 0.29800110952527E+03 0.29852593725008E+03 0.29515000085794E+03 0.29515000492122E+03 0.29799446629280E+03
 0.29848502032124E+03 0.29515000086398E+03 0.29515000491194E+03 0.29800110952527E+03 0.29852593725008E+03
 0.29515000085794E+03 0.29515000492122E+03 0.29799446629280E+03 0.29848502032125E+03 0.29515000086398E+03
 0.29515000491194E+03 0.29848425189828E+03 0.29515002332073E+03 -.17517771597690E+03 -.25252509516323E+03
 0.10448501869675E+03 0.18150631091564E+03 0.76498867125412E+02 0.97643709305869E+02 0.69007705620517E+02
 0.97643709305869E+02 0.12404913058375E+03 0.97525807967989E+02 0.67115564640668E+02 0.97525807967989E+02
 0.12239076276941E+03 0.97643709305870E+02 0.69007705620515E+02 0.97643709305870E+02 0.12404913058375E+03
 0.97525807967980E+02 0.67115564640665E+02 0.97525807967980E+02 0.12239076276940E+03 0.19891747907468E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31949842778969E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93212223295538E+00 0.91918665546703E+00 0.00000000000000E+00 0.93212223295538E+00 0.91918665546703E+00
 0.13852863917503E+01 0.35528642036049E+00 0.10299999713898E+01 0.15449999570847E+01 0.12775288478918E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.22728050865119E-03 0.41341324740444E-04
 0.12670626572599E-01 0.23047312367935E-02 0.12775288478918E-01 0.15243979642784E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    840.30914757
 0.71084845648949E+00 0.30707590432079E+03 0.49813989154965E+03 0.45286737807887E+03 0.43413493562600E+03
 0.22987401487268E+00 0.00000000000000E+00 0.19393742760926E+00 0.00000000000000E+00 -.18125536907892E+01
 0.10597712567108E-02 0.45711511589652E+00 0.75487988085554E+04 0.28307995532083E+04 0.17501062034035E+02
 0.65628982627631E+01 0.35923691423055E+03 0.29515167886265E+03 0.35373658327100E+03 0.39241131325640E+03
 0.29515026518846E+03 0.29515046245174E+03 0.34620024398759E+03 0.39226652672951E+03 0.29515021576753E+03
 0.29515046117168E+03 0.35373658327100E+03 0.39241131325640E+03 0.29515026518846E+03 0.29515046245174E+03
 0.34620024398759E+03 0.39226652672951E+03 0.29515021576753E+03 0.29515046117168E+03 0.44252795543208E+03
 0.32842362378165E+03 0.26062076231933E+04 0.22709955795456E+04 0.66073753159053E+03 0.11829753148608E+04
 0.51893409561234E+03 0.16552223253872E+04 0.14368702565543E+04 0.14181628442848E+04 0.22656369278956E+04
 0.14497972548798E+04 0.14325826765528E+04 0.12609423229801E+04 0.22627358506097E+04 0.16552223253872E+04
 0.14368702565543E+04 0.14181628442848E+04 0.22656369278956E+04 0.14497972548798E+04 0.14325826765528E+04
 0.12609423229801E+04 0.22627358506097E+04 0.20583779649094E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52457469701764E+03 0.18908933825132E+01
 0.18908933825132E+01 0.63632088924944E+01 0.13913768671635E-01 0.31092069085215E+03 0.31045692129732E+03
 0.31046122315219E+03 0.31046121679497E+03 0.22197318476725E+00 0.00000000000000E+00 0.22197236044687E+00
 0.00000000000000E+00 0.93672551169279E+01 0.12697335948879E-01 0.12715837929961E-01 0.63005342476635E+03
 0.23627003428738E+03 0.62913667538580E+03 0.23592625326967E+03 0.32842874146112E+03 0.44252312688756E+03
 0.29804739022862E+03 0.29859283635756E+03 0.29515000096622E+03 0.29515000548036E+03 0.29804082835130E+03
 0.29855106466101E+03 0.29515000097282E+03 0.29515000546986E+03 0.29804739022862E+03 0.29859283635756E+03
 0.29515000096622E+03 0.29515000548036E+03 0.29804082835130E+03 0.29855106466101E+03 0.29515000097282E+03
 0.29515000546986E+03 0.29855032976608E+03 0.29515002573670E+03 -.17788551621798E+03 -.25694464208135E+03
 0.10596410686709E+03 0.18388520058026E+03 0.77391273178828E+02 0.99435472575640E+02 0.70099654585332E+02
 0.99435472575640E+02 0.12577433112489E+03 0.99326357037157E+02 0.68188569486114E+02 0.99326357037157E+02
 0.12410236175047E+03 0.99435472575640E+02 0.70099654585330E+02 0.99435472575640E+02 0.12577433112489E+03
 0.99326357037148E+02 0.68188569486110E+02 0.99326357037148E+02 0.12410236175046E+03 0.19925842110227E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31968599362707E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93231077436880E+00 0.91940911049876E+00 0.00000000000000E+00 0.93231077436880E+00 0.91940911049876E+00
 0.13854241996345E+01 0.35542422824474E+00 0.10299999713898E+01 0.15449999570847E+01 0.12770563588839E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.23644805347428E-03 0.38159137358941E-04
 0.12875181745996E-01 0.20778594771567E-02 0.12770563588839E-01 0.15227648413986E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    851.11339120
 0.71115264260007E+00 0.30718106209962E+03 0.49843251189287E+03 0.45309618725235E+03 0.43433043063234E+03
 0.22987273224241E+00 0.00000000000000E+00 0.19394526854574E+00 0.00000000000000E+00 -.18149485241146E+01
 0.11037698773422E-02 0.45674187823063E+00 0.72478875934390E+04 0.27179578475396E+04 0.17515363449901E+02
 0.65682612937129E+01 0.35967658998118E+03 0.29515185691797E+03 0.35414339136036E+03 0.39299412606646E+03
 0.29515029733896E+03 0.29515051870090E+03 0.34657911379098E+03 0.39284889102475E+03 0.29515024207899E+03
 0.29515051727540E+03 0.35414339136036E+03 0.39299412606646E+03 0.29515029733896E+03 0.29515051870090E+03
 0.34657911379098E+03 0.39284889102475E+03 0.29515024207899E+03 0.29515051727540E+03 0.44304486329607E+03
 0.32895479781742E+03 0.26078951904722E+04 0.22699535979495E+04 0.65790629690659E+03 0.11774498190795E+04
 0.51625399068838E+03 0.16566847430688E+04 0.14363443512491E+04 0.14177019773670E+04 0.22620660819452E+04
 0.14517347841098E+04 0.14320702969147E+04 0.12612148155687E+04 0.22591812985971E+04 0.16566847430688E+04
 0.14363443512491E+04 0.14177019773670E+04 0.22620660819452E+04 0.14517347841098E+04 0.14320702969147E+04
 0.12612148155687E+04 0.22591812985971E+04 0.20550909894055E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52483283427085E+03 0.18908934057904E+01
 0.18908934057904E+01 0.64496428415366E+01 0.12386467131879E-01 0.31113051119138E+03 0.31065897818465E+03
 0.31066287193671E+03 0.31066286608420E+03 0.22187849417811E+00 0.00000000000000E+00 0.22187776914214E+00
 0.00000000000000E+00 0.93755316068856E+01 0.14262968111139E-01 0.14811713760574E-01 0.56089307202141E+03
 0.21033490200803E+03 0.54011305709233E+03 0.20254239640962E+03 0.32895994387186E+03 0.44303995734241E+03
 0.29810062014093E+03 0.29866759072850E+03 0.29515000110131E+03 0.29515000616701E+03 0.29809415417746E+03
 0.29862485846973E+03 0.29515000110856E+03 0.29515000615497E+03 0.29810062014093E+03 0.29866759072850E+03
 0.29515000110131E+03 0.29515000616701E+03 0.29809415417747E+03 0.29862485846973E+03 0.29515000110856E+03
 0.29515000615497E+03 0.29862425709959E+03 0.29515002867504E+03 -.18097544219262E+03 -.26192283504467E+03
 0.10765314217173E+03 0.18656982871260E+03 0.78378420830020E+02 0.10135663861946E+03 0.71304927033630E+02
 0.10135663861946E+03 0.12768091760274E+03 0.10125715129134E+03 0.69369585201978E+02 0.10125715129134E+03
 0.12599070571802E+03 0.10135663861946E+03 0.71304927033628E+02 0.10135663861946E+03 0.12768091760273E+03
 0.10125715129133E+03 0.69369585201975E+02 0.10125715129133E+03 0.12599070571802E+03 0.19953091648274E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31989443550770E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93252000493998E+00 0.91965257406342E+00 0.00000000000000E+00 0.93252000493998E+00 0.91965257406342E+00
 0.13855762926898E+01 0.35557632130004E+00 0.10299999713898E+01 0.15449999570847E+01 0.12764571629991E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.24647614445767E-03 0.34793046734998E-04
 0.13083451132867E-01 0.18468851325248E-02 0.12764571629991E-01 0.15211605456584E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    861.32585917
 0.71143654420137E+00 0.30727996243518E+03 0.49870666425083E+03 0.45331068051496E+03 0.43451379863272E+03
 0.22987151896447E+00 0.00000000000000E+00 0.19395260678198E+00 0.00000000000000E+00 -.18171931047853E+01
 0.11400991676317E-02 0.45639559080713E+00 0.70169334625676E+04 0.26313500484628E+04 0.17528653127109E+02
 0.65732449226658E+01 0.36008906545971E+03 0.29515203921628E+03 0.35452512990498E+03 0.39353989294253E+03
 0.29515033070689E+03 0.29515057709188E+03 0.34693488704107E+03 0.39339424383845E+03 0.29515026940439E+03
 0.29515057551647E+03 0.35452512990498E+03 0.39353989294253E+03 0.29515033070689E+03 0.29515057709188E+03
 0.34693488704107E+03 0.39339424383845E+03 0.29515026940439E+03 0.29515057551647E+03 0.44352758290741E+03
 0.32945117293254E+03 0.26094888845584E+04 0.22689853505511E+04 0.65527221020456E+03 0.11723129199910E+04
 0.51376434873546E+03 0.16580656136420E+04 0.14358433534636E+04 0.14172771886413E+04 0.22587186831861E+04
 0.14535600184057E+04 0.14315820748088E+04 0.12614751267405E+04 0.22558492032235E+04 0.16580656136420E+04
 0.14358433534636E+04 0.14172771886413E+04 0.22587186831861E+04 0.14535600184057E+04 0.14315820748088E+04
 0.12614751267405E+04 0.22558492032235E+04 0.20520235276382E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52507473540988E+03 0.18908934276072E+01
 0.18908934276072E+01 0.65313425852543E+01 0.11068125624399E-01 0.31132826924915E+03 0.31084946357964E+03
 0.31085299656718E+03 0.31085299117367E+03 0.22178930353008E+00 0.00000000000000E+00 0.22178866176317E+00
 0.00000000000000E+00 0.93833147959247E+01 0.15961851430741E-01 0.16537878189208E-01 0.50119499199152E+03
 0.18794812199682E+03 0.48373799277470E+03 0.18140174729051E+03 0.32945635547782E+03 0.44352260035793E+03
 0.29815254713010E+03 0.29873867343644E+03 0.29515000124444E+03 0.29515000688281E+03 0.29814617682696E+03
 0.29869502825528E+03 0.29515000125234E+03 0.29515000686913E+03 0.29815254713010E+03 0.29873867343644E+03
 0.29515000124444E+03 0.29515000688281E+03 0.29814617682696E+03 0.29869502825528E+03 0.29515000125234E+03
 0.29515000686913E+03 0.29869458463302E+03 0.29515003170877E+03 -.18389008174140E+03 -.26659998718312E+03
 0.10924540153375E+03 0.18909784058987E+03 0.79306212048456E+02 0.10314114372079E+03 0.72432704610227E+02
 0.10314114372079E+03 0.12946701635979E+03 0.10305049713631E+03 0.70474088064254E+02 0.10305049713631E+03
 0.12775925425979E+03 0.10314114372079E+03 0.72432704610225E+02 0.10314114372079E+03 0.12946701635979E+03
 0.10305049713630E+03 0.70474088064250E+02 0.10305049713630E+03 0.12775925425979E+03 0.19977814535464E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32009359233032E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93271831574480E+00 0.91987756724621E+00 0.00000000000000E+00 0.93271831574480E+00 0.91987756724621E+00
 0.13857182434905E+01 0.35571827210069E+00 0.10299999713898E+01 0.15449999570847E+01 0.12758698173720E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.25579744033519E-03 0.31786623159955E-04
 0.13261528986336E-01 0.16479415269407E-02 0.12758698173720E-01 0.15197054576772E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    871.53832713
 0.71171691809049E+00 0.30737823165698E+03 0.49897859547104E+03 0.45352352199145E+03 0.43469580699363E+03
 0.22987030820770E+00 0.00000000000000E+00 0.19395986573131E+00 0.00000000000000E+00 -.18194287205700E+01
 0.11731103280629E-02 0.45605436599961E+00 0.68194779370924E+04 0.25573042264096E+04 0.17541768254899E+02
 0.65781630955871E+01 0.36049859231507E+03 0.29515223577551E+03 0.35490423297463E+03 0.39408078397880E+03
 0.29515036715251E+03 0.29515064087972E+03 0.34728844679148E+03 0.39393473018542E+03 0.29515029926843E+03
 0.29515063914165E+03 0.35490423297463E+03 0.39408078397880E+03 0.29515036715251E+03 0.29515064087972E+03
 0.34728844679148E+03 0.39393473018542E+03 0.29515029926843E+03 0.29515063914165E+03 0.44400477510926E+03
 0.32994229673283E+03 0.26110763091637E+04 0.22680265010268E+04 0.65267714835554E+03 0.11672570101685E+04
 0.51131647607119E+03 0.16594416334474E+04 0.14353388296868E+04 0.14168580394224E+04 0.22553987614437E+04
 0.14553763362405E+04 0.14310903126288E+04 0.12617338656429E+04 0.22525444868030E+04 0.16594416334474E+04
 0.14353388296868E+04 0.14168580394224E+04 0.22553987614437E+04 0.14553763362405E+04 0.14310903126288E+04
 0.12617338656429E+04 0.22525444868029E+04 0.20489917048986E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52531470389301E+03 0.18908934493368E+01
 0.18908934493368E+01 0.66130423289721E+01 0.98817884339262E-02 0.31152542022340E+03 0.31103947132406E+03
 0.31104267268687E+03 0.31104266772590E+03 0.22170041079093E+00 0.00000000000000E+00 0.22169984837722E+00
 0.00000000000000E+00 0.93910211900928E+01 0.17878116817311E-01 0.18320613982817E-01 0.44747442259990E+03
 0.16780290847496E+03 0.43666658811233E+03 0.16374997054213E+03 0.32994751697004E+03 0.44399971610305E+03
 0.29820590611056E+03 0.29881015238136E+03 0.29515000140396E+03 0.29515000766800E+03 0.29819963502513E+03
 0.29876559426395E+03 0.29515000141255E+03 0.29515000765249E+03 0.29820590611056E+03 0.29881015238136E+03
 0.29515000140396E+03 0.29515000766800E+03 0.29819963502513E+03 0.29876559426395E+03 0.29515000141255E+03
 0.29515000765249E+03 0.29876530007232E+03 0.29515003500641E+03 -.18677237825957E+03 -.27122459492712E+03
 0.11081580742015E+03 0.19160005898222E+03 0.80230172524965E+02 0.10491024616633E+03 0.73548327159883E+02
 0.10491024616633E+03 0.13123497660787E+03 0.10482796092640E+03 0.71567135362078E+02 0.10482796092640E+03
 0.12951036844348E+03 0.10491024616633E+03 0.73548327159880E+02 0.10491024616633E+03 0.13123497660787E+03
 0.10482796092639E+03 0.71567135362071E+02 0.10482796092639E+03 0.12951036844346E+03 0.20002922619510E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32029883959758E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93291365658745E+00 0.92010268327301E+00 0.00000000000000E+00 0.93291365658745E+00 0.92010268327301E+00
 0.13858584304350E+01 0.35585845904525E+00 0.10299999713898E+01 0.15449999570847E+01 0.12752856491729E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.26491775568005E-03 0.29003877946092E-04
 0.13419304130801E-01 0.14691799654278E-02 0.12752856491729E-01 0.15182405729855E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    882.58592347
 0.71201689229511E+00 0.30748311696214E+03 0.49926961615825E+03 0.45375120711099E+03 0.43489040831156E+03
 0.22986900662864E+00 0.00000000000000E+00 0.19396762518003E+00 0.00000000000000E+00 -.18218613883855E+01
 0.12074437502881E-02 0.45569083399979E+00 0.66255674420372E+04 0.24845877907639E+04 0.17555762379025E+02
 0.65834108921344E+01 0.36093859412662E+03 0.29515246650175E+03 0.35531159006451E+03 0.39466186980071E+03
 0.29515041054153E+03 0.29515071683318E+03 0.34766837068479E+03 0.39451538280147E+03 0.29515033484594E+03
 0.29515071490287E+03 0.35531159006451E+03 0.39466186980071E+03 0.29515041054153E+03 0.29515071683318E+03
 0.34766837068479E+03 0.39451538280147E+03 0.29515033484594E+03 0.29515071490287E+03 0.44451968920415E+03
 0.33047253495846E+03 0.26127805567272E+04 0.22669849097687E+04 0.64981940902986E+03 0.11617225086218E+04
 0.50865400254683E+03 0.16609211993981E+04 0.14347718804629E+04 0.14164010922882E+04 0.22518014107965E+04
 0.14573308403080E+04 0.14305370877681E+04 0.12620074088424E+04 0.22489634603092E+04 0.16609211993981E+04
 0.14347718804629E+04 0.14164010922882E+04 0.22518014107965E+04 0.14573308403080E+04 0.14305370877681E+04
 0.12620074088424E+04 0.22489634603092E+04 0.20456928716941E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52557145196909E+03 0.18908934729818E+01
 0.18908934729818E+01 0.67014230996885E+01 0.87410217021238E-02 0.31173761431163E+03 0.31124442016296E+03
 0.31124729417680E+03 0.31124728965633E+03 0.22160459642221E+00 0.00000000000000E+00 0.22160410085849E+00
 0.00000000000000E+00 0.93991465074043E+01 0.20211339643347E-01 0.20437421052925E-01 0.39581740454465E+03
 0.14843152670425E+03 0.39143882093945E+03 0.14678955785229E+03 0.33047777804711E+03 0.44451456695706E+03
 0.29826447359670E+03 0.29888717295224E+03 0.29515000159828E+03 0.29515000860732E+03 0.29825830910792E+03
 0.29884163704636E+03 0.29515000160765E+03 0.29515000858957E+03 0.29826447359670E+03 0.29888717295224E+03
 0.29515000159828E+03 0.29515000860732E+03 0.29825830910793E+03 0.29884163704636E+03 0.29515000160765E+03
 0.29515000858957E+03 0.29884146421491E+03 0.29515003891236E+03 -.18988010632130E+03 -.27622580496920E+03
 0.11249569541946E+03 0.19428763462969E+03 0.81229460733134E+02 0.10682560542016E+03 0.74749834514456E+02
 0.10682560542016E+03 0.13314332015599E+03 0.10675173349994E+03 0.72745127185089E+02 0.10675173349994E+03
 0.13140136023073E+03 0.10682560542016E+03 0.74749834514454E+02 0.10682560542016E+03 0.13314332015598E+03
 0.10675173349993E+03 0.72745127185081E+02 0.10675173349993E+03 0.13140136023072E+03 0.20023957694626E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32051930709510E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93310777100521E+00 0.92036321187953E+00 0.00000000000000E+00 0.93310777100521E+00 0.92036321187953E+00
 0.13860084175373E+01 0.35600844614756E+00 0.10299999713898E+01 0.15449999570847E+01 0.12747050560989E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.27456785892024E-03 0.26257214995878E-04
 0.13566832325925E-01 0.12974105366729E-02 0.12747050560989E-01 0.15165067936514E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    892.42615459
 0.71228046447916E+00 0.30757488442548E+03 0.49952578767042E+03 0.45395149483026E+03 0.43506143097908E+03
 0.22986785646522E+00 0.00000000000000E+00 0.19397445527581E+00 0.00000000000000E+00 -.18239175629267E+01
 0.12384738975982E-02 0.45537195224462E+00 0.64595628664557E+04 0.24223360749209E+04 0.17568056092534E+02
 0.65880210347001E+01 0.36132765259308E+03 0.29515268910998E+03 0.35567188603995E+03 0.39517472942570E+03
 0.29515045299099E+03 0.29515079115311E+03 0.34800464986662E+03 0.39502786593517E+03 0.29515036967628E+03
 0.29515078903607E+03 0.35567188603995E+03 0.39517472942570E+03 0.29515045299099E+03 0.29515079115311E+03
 0.34800464986662E+03 0.39502786593517E+03 0.29515036967628E+03 0.29515078903607E+03 0.44497207326662E+03
 0.33093976853575E+03 0.26142686101925E+04 0.22660328921976E+04 0.64731275419661E+03 0.11568726166065E+04
 0.50632329863892E+03 0.16622187772765E+04 0.14342468079021E+04 0.14159765854266E+04 0.22485991060617E+04
 0.14590485372590E+04 0.14300242999072E+04 0.12622279506047E+04 0.22457756702538E+04 0.16622187772765E+04
 0.14342468079021E+04 0.14159765854266E+04 0.22485991060617E+04 0.14590485372590E+04 0.14300242999072E+04
 0.12622279506047E+04 0.22457756702538E+04 0.20427669243409E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52579735727422E+03 0.18908934929673E+01
 0.18908934929673E+01 0.67801449486601E+01 0.78364217084621E-02 0.31192638037283E+03 0.31142640525773E+03
 0.31142901726829E+03 0.31142901310344E+03 0.22151954373303E+00 0.00000000000000E+00 0.22151910328706E+00
 0.00000000000000E+00 0.94064275364187E+01 0.22544441400168E-01 0.22540611533277E-01 0.35485465609898E+03
 0.13307049603712E+03 0.35491494932111E+03 0.13309310599542E+03 0.33094501801974E+03 0.44496690848881E+03
 0.29831794463139E+03 0.29895615146483E+03 0.29515000179290E+03 0.29515000953084E+03 0.29831187644132E+03
 0.29890974877343E+03 0.29515000180300E+03 0.29515000951083E+03 0.29831794463139E+03 0.29895615146483E+03
 0.29515000179290E+03 0.29515000953084E+03 0.29831187644132E+03 0.29890974877343E+03 0.29515000180300E+03
 0.29515000951083E+03 0.29890959843649E+03 0.29515004271523E+03 -.19260043380764E+03 -.28061040148157E+03
 0.11396571696216E+03 0.19664948645716E+03 0.82113940910182E+02 0.10853099714826E+03 0.75810814257322E+02
 0.10853099714826E+03 0.13483055554704E+03 0.10846410821529E+03 0.73786319994305E+02 0.10846410821529E+03
 0.13307428949647E+03 0.10853099714826E+03 0.75810814257320E+02 0.10853099714826E+03 0.13483055554703E+03
 0.10846410821529E+03 0.73786319994299E+02 0.10846410821529E+03 0.13307428949646E+03 0.20047635339832E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32071394370330E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93329774308659E+00 0.92056144285932E+00 0.00000000000000E+00 0.93329774308659E+00 0.92056144285932E+00
 0.13861402036293E+01 0.35614023223958E+00 0.10299999713898E+01 0.15449999570847E+01 0.12741348546818E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.28307220671474E-03 0.24030290921747E-04
 0.13682525738799E-01 0.11615236898865E-02 0.12741348546818E-01 0.15151151926322E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    902.26638572
 0.71254026785361E+00 0.30766686016898E+03 0.49978054473166E+03 0.45415096597945E+03 0.43523198660214E+03
 0.22986671276935E+00 0.00000000000000E+00 0.19398120231810E+00 0.00000000000000E+00 -.18259998943468E+01
 0.12711544917682E-02 0.45505646184424E+00 0.62934915085513E+04 0.23600593157068E+04 0.17580236016379E+02
 0.65925885061420E+01 0.36171387491730E+03 0.29515292755526E+03 0.35602964452676E+03 0.39568267525493E+03
 0.29515049901244E+03 0.29515087173606E+03 0.34833885239377E+03 0.39553544528054E+03 0.29515040745942E+03
 0.29515086941784E+03 0.35602964452676E+03 0.39568267525493E+03 0.29515049901244E+03 0.29515087173606E+03
 0.34833885239377E+03 0.39553544528054E+03 0.29515040745942E+03 0.29515086941784E+03 0.44541783169031E+03
 0.33140104089239E+03 0.26157404854537E+04 0.22650876149771E+04 0.64487658235356E+03 0.11521554138615E+04
 0.50405444859617E+03 0.16635037589752E+04 0.14337285014670E+04 0.14155555569041E+04 0.22454415237495E+04
 0.14607502111839E+04 0.14295183147125E+04 0.12624448242814E+04 0.22426325525282E+04 0.16635037589752E+04
 0.14337285014670E+04 0.14155555569041E+04 0.22454415237495E+04 0.14607502111839E+04 0.14295183147125E+04
 0.12624448242814E+04 0.22426325525282E+04 0.20398959696708E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52602214961402E+03 0.18908935132071E+01
 0.18908935132071E+01 0.68588667976318E+01 0.70253080752969E-02 0.31211444300427E+03 0.31160790683842E+03
 0.31161027922017E+03 0.31161027538799E+03 0.22143476499087E+00 0.00000000000000E+00 0.22143437528070E+00
 0.00000000000000E+00 0.94136147120000E+01 0.25147330148675E-01 0.24885286107816E-01 0.31812522254660E+03
 0.11929695845497E+03 0.32147510642795E+03 0.12055316491048E+03 0.33140631233951E+03 0.44541260901944E+03
 0.29837265565923E+03 0.29902569009142E+03 0.29515000200837E+03 0.29515001053648E+03 0.29836668504060E+03
 0.29897842696091E+03 0.29515000201922E+03 0.29515001051396E+03 0.29837265565923E+03 0.29902569009142E+03
 0.29515000200837E+03 0.29515001053648E+03 0.29836668504060E+03 0.29897842696091E+03 0.29515000201922E+03
 0.29515001051396E+03 0.29897818851066E+03 0.29515004682143E+03 -.19526273059487E+03 -.28491137323353E+03
 0.11539763335044E+03 0.19896702546888E+03 0.82992403951682E+02 0.11022928244225E+03 0.76855743618264E+02
 0.11022928244225E+03 0.13649778988947E+03 0.11016891397599E+03 0.74812913510481E+02 0.11016891397599E+03
 0.13472864517142E+03 0.11022928244225E+03 0.76855743618262E+02 0.11022928244225E+03 0.13649778988947E+03
 0.11016891397598E+03 0.74812913510477E+02 0.11016891397598E+03 0.13472864517141E+03 0.20074447640027E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32090743613750E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93348283662343E+00 0.92076695833682E+00 0.00000000000000E+00 0.93348283662343E+00 0.92076695833682E+00
 0.13862701053166E+01 0.35627013392681E+00 0.10299999713898E+01 0.15449999570847E+01 0.12735938426228E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29133633259559E-03 0.21978086473918E-04
 0.13783310437301E-01 0.10397974944936E-02 0.12735938426228E-01 0.15136422350865E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    912.10661684
 0.71279771977835E+00 0.30775895184911E+03 0.50003360016527E+03 0.45434928986829E+03 0.43540175512694E+03
 0.22986557791308E+00 0.00000000000000E+00 0.19398786543038E+00 0.00000000000000E+00 -.18280908707985E+01
 0.13063642343169E-02 0.45474478792757E+00 0.61238663688485E+04 0.22964498883182E+04 0.17592285194645E+02
 0.65971069479917E+01 0.36209745772280E+03 0.29515318263964E+03 0.35638503921796E+03 0.39618625455071E+03
 0.29515054883372E+03 0.29515095898107E+03 0.34867108128111E+03 0.39603866677476E+03 0.29515044838567E+03
 0.29515095644640E+03 0.35638503921796E+03 0.39618625455071E+03 0.29515054883372E+03 0.29515095898107E+03
 0.34867108128111E+03 0.39603866677476E+03 0.29515044838567E+03 0.29515095644640E+03 0.44585851359146E+03
 0.33185765997751E+03 0.26171917000837E+04 0.22641422039420E+04 0.64248086741953E+03 0.11475204217344E+04
 0.50182714997773E+03 0.16647736639777E+04 0.14332098423576E+04 0.14151338449946E+04 0.22423137947393E+04
 0.14624343485873E+04 0.14290119941163E+04 0.12626553570855E+04 0.22395192292487E+04 0.16647736639777E+04
 0.14332098423576E+04 0.14151338449946E+04 0.22423137947393E+04 0.14624343485873E+04 0.14290119941163E+04
 0.12626553570855E+04 0.22395192292487E+04 0.20370586661963E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52624554583495E+03 0.18908935335309E+01
 0.18908935335309E+01 0.69375886466034E+01 0.62980695091346E-02 0.31230193545850E+03 0.31178887437910E+03
 0.31179102857532E+03 0.31179102505116E+03 0.22135025532325E+00 0.00000000000000E+00 0.22134991486908E+00
 0.00000000000000E+00 0.94207241791434E+01 0.28051092944318E-01 0.27502359936878E-01 0.28519387875119E+03
 0.10694770453170E+03 0.29088412842975E+03 0.10908154816115E+03 0.33186295382782E+03 0.44585323605705E+03
 0.29842840738715E+03 0.29909560850245E+03 0.29515000224662E+03 0.29515001162999E+03 0.29842253448240E+03
 0.29904749454538E+03 0.29515000225825E+03 0.29515001160468E+03 0.29842840738715E+03 0.29909560850245E+03
 0.29515000224662E+03 0.29515001162999E+03 0.29842253448240E+03 0.29904749454539E+03 0.29515000225825E+03
 0.29515001160468E+03 0.29904703837173E+03 0.29515005124956E+03 -.19787337932429E+03 -.28914175765876E+03
 0.11679397734586E+03 0.20124387006379E+03 0.83865922831201E+02 0.11192732486655E+03 0.77887598758985E+02
 0.11192732486655E+03 0.13814859910789E+03 0.11187303412430E+03 0.75827893816223E+02 0.11187303412430E+03
 0.13636799399849E+03 0.11192732486655E+03 0.77887598758983E+02 0.11192732486655E+03 0.13814859910789E+03
 0.11187303412429E+03 0.75827893816219E+02 0.11187303412429E+03 0.13636799399848E+03 0.20103255210720E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32109973674995E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93366514555551E+00 0.92097455545149E+00 0.00000000000000E+00 0.93366514555551E+00 0.92097455545149E+00
 0.13863988312789E+01 0.35639885988917E+00 0.10299999713898E+01 0.15449999570847E+01 0.12730693308855E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.29938105057112E-03 0.20089501363831E-04
 0.13870990555921E-01 0.93079132149234E-03 0.12730693308855E-01 0.15121252429348E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    921.94684796
 0.71305331000835E+00 0.30785098485326E+03 0.50028472899509E+03 0.45454622292254E+03 0.43557048634260E+03
 0.22986445303018E+00 0.00000000000000E+00 0.19399445048715E+00 0.00000000000000E+00 -.18301792615876E+01
 0.13448384230768E-02 0.45443728961167E+00 0.59486700132325E+04 0.22307512549622E+04 0.17604189143096E+02
 0.66015709286612E+01 0.36247847911680E+03 0.29515345518662E+03 0.35673814068237E+03 0.39668565633931E+03
 0.29515060269204E+03 0.29515105330341E+03 0.34900138567930E+03 0.39653771904868E+03 0.29515049265328E+03
 0.29515105053617E+03 0.35673814068237E+03 0.39668565633931E+03 0.29515060269204E+03 0.29515105330341E+03
 0.34900138567930E+03 0.39653771904868E+03 0.29515049265328E+03 0.29515105053617E+03 0.44629444144540E+03
 0.33231003941036E+03 0.26186174229069E+04 0.22631896627590E+04 0.64011735528611E+03 0.11429532737923E+04
 0.49963533172978E+03 0.16660255063942E+04 0.14326862044802E+04 0.14147068476875E+04 0.22392074544819E+04
 0.14640983927005E+04 0.14285007210331E+04 0.12628555946885E+04 0.22364272478085E+04 0.16660255063942E+04
 0.14326862044802E+04 0.14147068476875E+04 0.22392074544819E+04 0.14640983927005E+04 0.14285007210331E+04
 0.12628555946885E+04 0.22364272478085E+04 0.20342453022634E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52646732007231E+03 0.18908935538295E+01
 0.18908935538295E+01 0.70163104955750E+01 0.56460599816952E-02 0.31248862126866E+03 0.31196926636029E+03
 0.31197122123293E+03 0.31197121799615E+03 0.22126602184049E+00 0.00000000000000E+00 0.22126572021574E+00
 0.00000000000000E+00 0.94277599564294E+01 0.31290444200529E-01 0.30424778007366E-01 0.25566910935271E+03
 0.95875916007268E+02 0.26294357835785E+03 0.98603841884192E+02 0.33231534922623E+03 0.44628911860859E+03
 0.29848512761024E+03 0.29916585164819E+03 0.29515000250977E+03 0.29515001281741E+03 0.29847935197656E+03
 0.29911689859696E+03 0.29515000252220E+03 0.29515001278902E+03 0.29848512761024E+03 0.29916585164819E+03
 0.29515000250977E+03 0.29515001281741E+03 0.29847935197656E+03 0.29911689859696E+03 0.29515000252220E+03
 0.29515001278902E+03 0.29911607415616E+03 0.29515005601899E+03 -.20043172019339E+03 -.29330455445749E+03
 0.11815337246169E+03 0.20347884134448E+03 0.84734702020481E+02 0.11362867414644E+03 0.78906839306448E+02
 0.11362867414644E+03 0.13978340016499E+03 0.11358004031114E+03 0.76831824527330E+02 0.11358004031114E+03
 0.13799284385417E+03 0.11362867414644E+03 0.78906839306447E+02 0.11362867414644E+03 0.13978340016499E+03
 0.11358004031113E+03 0.76831824527326E+02 0.11358004031113E+03 0.13799284385417E+03 0.20133234714597E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32129083518332E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93384562172116E+00 0.92118149110741E+00 0.00000000000000E+00 0.93384562172116E+00 0.92118149110741E+00
 0.13865266263939E+01 0.35652665500418E+00 0.10299999713898E+01 0.15449999570847E+01 0.12725564853383E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.30722774748037E-03 0.18353694054917E-04
 0.13947020547870E-01 0.83319085015133E-03 0.12725564853383E-01 0.15105792839557E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    931.78707908
 0.71330710696655E+00 0.30794296886939E+03 0.50053389005797E+03 0.45474173245096E+03 0.43573815713345E+03
 0.22986333825551E+00 0.00000000000000E+00 0.19400096396962E+00 0.00000000000000E+00 -.18322635361392E+01
 0.13872387861918E-02 0.45413419364525E+00 0.57668514459295E+04 0.21625692922236E+04 0.17615938442744E+02
 0.66059769160291E+01 0.36285697329293E+03 0.29515374604083E+03 0.35708897781309E+03 0.39718097315727E+03
 0.29515066083400E+03 0.29515115513470E+03 0.34932978359924E+03 0.39703269444791E+03 0.29515054046853E+03
 0.29515115211789E+03 0.35708897781309E+03 0.39718097315727E+03 0.29515066083400E+03 0.29515115513470E+03
 0.34932978359923E+03 0.39703269444790E+03 0.29515054046853E+03 0.29515115211789E+03 0.44672575932579E+03
 0.33275840801654E+03 0.26200153399096E+04 0.22622275770848E+04 0.63778353518380E+03 0.11384493209793E+04
 0.49747686811957E+03 0.16672576945376E+04 0.14321565251367E+04 0.14142729199267E+04 0.22361201363026E+04
 0.14657410045764E+04 0.14279834406230E+04 0.12630442124273E+04 0.22333542510975E+04 0.16672576945376E+04
 0.14321565251367E+04 0.14142729199267E+04 0.22361201363026E+04 0.14657410045765E+04 0.14279834406231E+04
 0.12630442124273E+04 0.22333542510975E+04 0.20314528659104E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52668743825834E+03 0.18908935740882E+01
 0.18908935740882E+01 0.70950323445466E+01 0.50615265157057E-02 0.31267561905498E+03 0.31214904964856E+03
 0.31215082647857E+03 0.31215082349633E+03 0.22118192661911E+00 0.00000000000000E+00 0.22118179032191E+00
 0.00000000000000E+00 0.94347247238970E+01 0.34904038522245E-01 0.33688011663033E-01 0.22919983872071E+03
 0.85949939520266E+02 0.23747320204055E+03 0.89052450765208E+02 0.33276372739647E+03 0.44672039966751E+03
 0.29854277254779E+03 0.29923638863496E+03 0.29515000280008E+03 0.29515001410508E+03 0.29853709332576E+03
 0.29918661035912E+03 0.29515000281331E+03 0.29515001407328E+03 0.29854277254779E+03 0.29923638863496E+03
 0.29515000280008E+03 0.29515001410508E+03 0.29853709332576E+03 0.29918661035912E+03 0.29515000281331E+03
 0.29515001407328E+03 0.29918524515703E+03 0.29515006114991E+03 -.20293548117339E+03 -.29739141646020E+03
 0.11947347679747E+03 0.20566969179532E+03 0.85598847613860E+02 0.11533633221397E+03 0.79913379974502E+02
 0.11533633221397E+03 0.14140199197785E+03 0.11529295650574E+03 0.77824750623033E+02 0.11529295650574E+03
 0.13960311064145E+03 0.11533633221397E+03 0.79913379974502E+02 0.11533633221397E+03 0.14140199197785E+03
 0.11529295650573E+03 0.77824750623030E+02 0.11529295650573E+03 0.13960311064145E+03 0.20165842350869E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32148074716527E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93402464533487E+00 0.92138712224877E+00 0.00000000000000E+00 0.93402464533487E+00 0.92138712224877E+00
 0.13866535248730E+01 0.35665355348328E+00 0.10299999713898E+01 0.15449999570847E+01 0.12720534665551E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.31488815429948E-03 0.16759597868215E-04
 0.14012638864910E-01 0.74580827904074E-03 0.12720534665551E-01 0.15090094896118E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    941.62731020
 0.71355915524865E+00 0.30803496286912E+03 0.50078109064376E+03 0.45493583593965E+03 0.43590479773029E+03
 0.22986223318302E+00 0.00000000000000E+00 0.19400741109177E+00 0.00000000000000E+00 -.18343436160464E+01
 0.14342049109408E-02 0.45383572605774E+00 0.55780034909742E+04 0.20917513091153E+04 0.17627523662565E+02
 0.66103213734618E+01 0.36323297058036E+03 0.29515405606785E+03 0.35743757617684E+03 0.39767228440739E+03
 0.29515072351577E+03 0.29515126492302E+03 0.34965629245698E+03 0.39752367225463E+03 0.29515059204583E+03
 0.29515126163875E+03 0.35743757617684E+03 0.39767228440739E+03 0.29515072351577E+03 0.29515126492302E+03
 0.34965629245698E+03 0.39752367225463E+03 0.29515059204583E+03 0.29515126163875E+03 0.44715258580711E+03
 0.33320293391341E+03 0.26213837762359E+04 0.22612546594560E+04 0.63547808344681E+03 0.11340059096814E+04
 0.49535043581734E+03 0.16684690101363E+04 0.14316206195269E+04 0.14138311602926E+04 0.22330509059278E+04
 0.14673611778462E+04 0.14274599753907E+04 0.12632205509200E+04 0.22302993133271E+04 0.16684690101363E+04
 0.14316206195269E+04 0.14138311602926E+04 0.22330509059279E+04 0.14673611778462E+04 0.14274599753907E+04
 0.12632205509200E+04 0.22302993133272E+04 0.20286798555855E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52690591535976E+03 0.18908935943061E+01
 0.18908935943061E+01 0.71737541935182E+01 0.45374923438231E-02 0.31286292755440E+03 0.31232820856157E+03
 0.31232982608379E+03 0.31232982332761E+03 0.22109794921007E+00 0.00000000000000E+00 0.22109812387569E+00
 0.00000000000000E+00 0.94416392460670E+01 0.38935097805353E-01 0.37333089850839E-01 0.20547014007758E+03
 0.77051302529091E+02 0.21428711183465E+03 0.80357666937995E+02 0.33320825779534E+03 0.44714719595322E+03
 0.29860131437102E+03 0.29930719487022E+03 0.29515000311997E+03 0.29515001549965E+03 0.29859573038155E+03
 0.29925660747994E+03 0.29515000313403E+03 0.29515001546407E+03 0.29860131437102E+03 0.29930719487022E+03
 0.29515000311997E+03 0.29515001549965E+03 0.29859573038156E+03 0.29925660747994E+03 0.29515000313403E+03
 0.29515001546407E+03 0.29925450542989E+03 0.29515006666331E+03 -.20538141832751E+03 -.30139979289854E+03
 0.12075126914423E+03 0.20781353735964E+03 0.86458511869689E+02 0.11705332593600E+03 0.80906890639774E+02
 0.11705332593600E+03 0.14300395943925E+03 0.11701483153156E+03 0.78806499060686E+02 0.11701483153156E+03
 0.14119852229322E+03 0.11705332593600E+03 0.80906890639776E+02 0.11705332593600E+03 0.14300395943925E+03
 0.11701483153155E+03 0.78806499060684E+02 0.11701483153155E+03 0.14119852229322E+03 0.20200925283879E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32166944589262E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93420265427595E+00 0.92159096779434E+00 0.00000000000000E+00 0.93420265427595E+00 0.92159096779434E+00
 0.13867795490141E+01 0.35677957762433E+00 0.10299999713898E+01 0.15449999570847E+01 0.12715452751159E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32237399313028E-03 0.15296785292228E-04
 0.14069337916589E-01 0.66759616439309E-03 0.12715452751159E-01 0.15074604859405E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    951.46754132
 0.71380948586619E+00 0.30812703678933E+03 0.50102635337086E+03 0.45512856603986E+03 0.43607045353705E+03
 0.22986113834780E+00 0.00000000000000E+00 0.19401379613300E+00 0.00000000000000E+00 -.18364231203663E+01
 0.14863591222970E-02 0.45354212980703E+00 0.53822793428529E+04 0.20183547535698E+04 0.17638934674942E+02
 0.66146005031031E+01 0.36360649999636E+03 0.29515438615385E+03 0.35778396034114E+03 0.39815966386383E+03
 0.29515079100308E+03 0.29515138313310E+03 0.34998093003018E+03 0.39801072615973E+03 0.29515064760776E+03
 0.29515137956254E+03 0.35778396034114E+03 0.39815966386383E+03 0.29515079100308E+03 0.29515138313310E+03
 0.34998093003018E+03 0.39801072615973E+03 0.29515064760776E+03 0.29515137956254E+03 0.44757502171755E+03
 0.33364386937255E+03 0.26227212854713E+04 0.22602699644782E+04 0.63320023448771E+03 0.11296213092701E+04
 0.49325507360998E+03 0.16696583681497E+04 0.14310785956534E+04 0.14133808855495E+04 0.22299993112874E+04
 0.14689580199225E+04 0.14269304409000E+04 0.12633841366569E+04 0.22272619908575E+04 0.16696583681497E+04
 0.14310785956534E+04 0.14133808855495E+04 0.22299993112874E+04 0.14689580199225E+04 0.14269304409001E+04
 0.12633841366569E+04 0.22272619908575E+04 0.20259253256903E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52712278126029E+03 0.18908936145184E+01
 0.18908936145184E+01 0.72524760424898E+01 0.40676268248276E-02 0.31304474126730E+03 0.31250672011035E+03
 0.31250817908988E+03 0.31250817658916E+03 0.22101493820373E+00 0.00000000000000E+00 0.22101471689441E+00
 0.00000000000000E+00 0.94484494127091E+01 0.43432622400629E-01 0.41400655922077E-01 0.18419334495179E+03
 0.69072504356922E+02 0.19323365347296E+03 0.72462620052362E+02 0.33364919358986E+03 0.44756960698522E+03
 0.29866073703722E+03 0.29937824822511E+03 0.29515000347209E+03 0.29515001700806E+03 0.29865524684855E+03
 0.29932687014408E+03 0.29515000348697E+03 0.29515001696831E+03 0.29866073703722E+03 0.29937824822511E+03
 0.29515000347209E+03 0.29515001700806E+03 0.29865524684855E+03 0.29932687014408E+03 0.29515000348697E+03
 0.29515001696831E+03 0.29932381042999E+03 0.29515007258101E+03 -.20776698965188E+03 -.30536596771201E+03
 0.12198398059868E+03 0.20990790887612E+03 0.87314008374447E+02 0.11878287512819E+03 0.81887176232225E+02
 0.11878287512819E+03 0.14458913637935E+03 0.11874890760092E+03 0.79777024399566E+02 0.11874890760092E+03
 0.14277904832917E+03 0.11878287512819E+03 0.81887176232226E+02 0.11878287512819E+03 0.14458913637935E+03
 0.11874890760091E+03 0.79777024399564E+02 0.11874890760091E+03 0.14277904832916E+03 0.20229180146705E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32185712280630E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93437916720503E+00 0.92179398497012E+00 0.00000000000000E+00 0.93437916720503E+00 0.92179398497012E+00
 0.13869047143229E+01 0.35690474293309E+00 0.10299999713898E+01 0.15449999570847E+01 0.12710754485447E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.32964230917954E-03 0.13953135460225E-04
 0.14116976703860E-01 0.59754492294408E-03 0.12710754485447E-01 0.15058117071444E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    961.30777244
 0.71405819677918E+00 0.30821927052591E+03 0.50126970867228E+03 0.45531995942220E+03 0.43623517393313E+03
 0.22986005271568E+00 0.00000000000000E+00 0.19402012276508E+00 0.00000000000000E+00 -.18385009463792E+01
 0.15443781151868E-02 0.45325366748400E+00 0.51800785839498E+04 0.19425294689812E+04 0.17650160547862E+02
 0.66188102054483E+01 0.36397758860805E+03 0.29515473720527E+03 0.35812815312053E+03 0.39864318102734E+03
 0.29515086357137E+03 0.29515151024635E+03 0.35030371324651E+03 0.39849392560695E+03 0.29515070738521E+03
 0.29515150636972E+03 0.35812815312053E+03 0.39864318102734E+03 0.29515086357137E+03 0.29515151024635E+03
 0.35030371324651E+03 0.39849392560695E+03 0.29515070738521E+03 0.29515150636972E+03 0.44799315704052E+03
 0.33408131552208E+03 0.26240262945271E+04 0.22592725306266E+04 0.63094950872124E+03 0.11252942805346E+04
 0.49119002426970E+03 0.16708245980930E+04 0.14305307061312E+04 0.14129214125005E+04 0.22269651584916E+04
 0.14705305580068E+04 0.14263950978979E+04 0.12635344911831E+04 0.22242420985791E+04 0.16708245980930E+04
 0.14305307061312E+04 0.14129214125005E+04 0.22269651584916E+04 0.14705305580068E+04 0.14263950978979E+04
 0.12635344911831E+04 0.22242420985791E+04 0.20231885509297E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52733807272669E+03 0.18908936347144E+01
 0.18908936347144E+01 0.73311978914615E+01 0.36464298338323E-02 0.31322864853558E+03 0.31268455896394E+03
 0.31268588162024E+03 0.31268587932831E+03 0.22093176329498E+00 0.00000000000000E+00 0.22093157183488E+00
 0.00000000000000E+00 0.94552198665149E+01 0.48449497121893E-01 0.45943460937247E-01 0.16512039288815E+03
 0.61920147333055E+02 0.17412706480531E+03 0.65297649301990E+02 0.33408663661693E+03 0.44798772191830E+03
 0.29872103576236E+03 0.29944952801845E+03 0.29515000385925E+03 0.29515001863760E+03 0.29871563774507E+03
 0.29939738021683E+03 0.29515000387495E+03 0.29515001859325E+03 0.29872103576236E+03 0.29944952801845E+03
 0.29515000385925E+03 0.29515001863760E+03 0.29871563774507E+03 0.29939738021683E+03 0.29515000387495E+03
 0.29515001859325E+03 0.29939311446643E+03 0.29515007892566E+03 -.21008740928078E+03 -.30923658499457E+03
 0.12316739832494E+03 0.21194852310735E+03 0.88165287790789E+02 0.12052825143011E+03 0.82853521133699E+02
 0.12052825143011E+03 0.14615656316209E+03 0.12049847585164E+03 0.80735817220238E+02 0.12049847585164E+03
 0.14434391892780E+03 0.12052825143011E+03 0.82853521133699E+02 0.12052825143011E+03 0.14615656316209E+03
 0.12049847585163E+03 0.80735817220236E+02 0.12049847585163E+03 0.14434391892780E+03 0.20262146507920E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32204360877316E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93455503181933E+00 0.92199513030620E+00 0.00000000000000E+00 0.93455503181933E+00 0.92199513030620E+00
 0.13870290697794E+01 0.35702909838959E+00 0.10299999713898E+01 0.15449999570847E+01 0.12705943040131E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.33674830893671E-03 0.12721854733008E-04
 0.14157688525583E-01 0.53485660357535E-03 0.12705943040131E-01 0.15042015292828E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    971.14800357
 0.71430539378717E+00 0.30831175253174E+03 0.50151118927725E+03 0.45551005602926E+03 0.43639901309283E+03
 0.22985897573180E+00 0.00000000000000E+00 0.19402639433237E+00 0.00000000000000E+00 -.18405796179268E+01
 0.16089876287098E-02 0.45297062340449E+00 0.49720705475001E+04 0.18645264553126E+04 0.17661189460528E+02
 0.66229460476979E+01 0.36434626151668E+03 0.29515511014853E+03 0.35847017553022E+03 0.39912290243812E+03
 0.29515094150578E+03 0.29515164676098E+03 0.35062465788912E+03 0.39897333710049E+03 0.29515077161735E+03
 0.29515164255751E+03 0.35847017553022E+03 0.39912290243812E+03 0.29515094150578E+03 0.29515164676098E+03
 0.35062465788912E+03 0.39897333710049E+03 0.29515077161735E+03 0.29515164255751E+03 0.44840707521894E+03
 0.33451546859173E+03 0.26252971098894E+04 0.22582613743132E+04 0.62872556196177E+03 0.11210238215957E+04
 0.48915463182410E+03 0.16719664370622E+04 0.14299772624441E+04 0.14124520403435E+04 0.22239483650135E+04
 0.14720777330217E+04 0.14258542666213E+04 0.12636711167332E+04 0.22212395631804E+04 0.16719664370622E+04
 0.14299772624441E+04 0.14124520403435E+04 0.22239483650136E+04 0.14720777330217E+04 0.14258542666213E+04
 0.12636711167332E+04 0.22212395631804E+04 0.20204689048789E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52755182964065E+03 0.18908936549186E+01
 0.18908936549186E+01 0.74099197404331E+01 0.32688302730266E-02 0.31341174551536E+03 0.31286172713673E+03
 0.31286292574788E+03 0.31286292364897E+03 0.22084886065829E+00 0.00000000000000E+00 0.22084868647227E+00
 0.00000000000000E+00 0.94619321905505E+01 0.54046147646905E-01 0.51015254156345E-01 0.14802165090962E+03
 0.55508119091108E+02 0.15681584130666E+03 0.58805940489996E+02 0.33452078352225E+03 0.44840162367493E+03
 0.29878221447211E+03 0.29952101363535E+03 0.29515000428448E+03 0.29515002039586E+03 0.29877690684116E+03
 0.29946811983036E+03 0.29515000430100E+03 0.29515002034645E+03 0.29878221447211E+03 0.29952101363535E+03
 0.29515000428448E+03 0.29515002039586E+03 0.29877690684117E+03 0.29946811983036E+03 0.29515000430100E+03
 0.29515002034645E+03 0.29946236982853E+03 0.29515008572080E+03 -.21233860096169E+03 -.31302731063473E+03
 0.12429754312762E+03 0.21393181269195E+03 0.89012781848695E+02 0.12229301120402E+03 0.83805367817131E+02
 0.12229301120402E+03 0.14770577869276E+03 0.12226711334305E+03 0.81682520568839E+02 0.12226711334305E+03
 0.14589285677412E+03 0.12229301120402E+03 0.83805367817130E+02 0.12229301120402E+03 0.14770577869276E+03
 0.12226711334304E+03 0.81682520568835E+02 0.12226711334304E+03 0.14589285677411E+03 0.20295116784853E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32222900423475E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93473013179895E+00 0.92219481434282E+00 0.00000000000000E+00 0.93473013179895E+00 0.92219481434282E+00
 0.13871526682834E+01 0.35715269689359E+00 0.10299999713898E+01 0.15449999570847E+01 0.12701175213350E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.34367421245434E-03 0.11593379389554E-04
 0.14191863487913E-01 0.47874309941716E-03 0.12701175213350E-01 0.15025874179174E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    980.98823469
 0.71455120157955E+00 0.30840457887000E+03 0.50175082844145E+03 0.45569889679073E+03 0.43656202764009E+03
 0.22985790694594E+00 0.00000000000000E+00 0.19403261403616E+00 0.00000000000000E+00 -.18426608362307E+01
 0.16809730922178E-02 0.45269330738439E+00 0.47591481606915E+04 0.17846805602593E+04 0.17672008553038E+02
 0.66270032073893E+01 0.36471254199936E+03 0.29515550592963E+03 0.35881004687217E+03 0.39959889243690E+03
 0.29515102510126E+03 0.29515179319205E+03 0.35094377854155E+03 0.39944902496344E+03 0.29515084055175E+03
 0.29515178863996E+03 0.35881004687217E+03 0.39959889243690E+03 0.29515102510126E+03 0.29515179319205E+03
 0.35094377854154E+03 0.39944902496344E+03 0.29515084055175E+03 0.29515178863996E+03 0.44881685519679E+03
 0.33494651795013E+03 0.26265318608704E+04 0.22572354237088E+04 0.62652812250605E+03 0.11168090629500E+04
 0.48714829983137E+03 0.16730824947221E+04 0.14294186023761E+04 0.14119720090508E+04 0.22209489046566E+04
 0.14735983690449E+04 0.14253082943246E+04 0.12637934602888E+04 0.22182543682324E+04 0.16730824947221E+04
 0.14294186023761E+04 0.14119720090509E+04 0.22209489046566E+04 0.14735983690449E+04 0.14253082943246E+04
 0.12637934602889E+04 0.22182543682324E+04 0.20177657862231E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52776409314762E+03 0.18908936751475E+01
 0.18908936751475E+01 0.74886415894047E+01 0.29303218777468E-02 0.31359438978172E+03 0.31303821886864E+03
 0.31303930537517E+03 0.31303930345197E+03 0.22076619786547E+00 0.00000000000000E+00 0.22076606000631E+00
 0.00000000000000E+00 0.94685909425064E+01 0.60289511803742E-01 0.56676928554426E-01 0.13269306319882E+03
 0.49759898699558E+02 0.14115090926845E+03 0.52931590975667E+02 0.33495182373417E+03 0.44881139085973E+03
 0.29884428577940E+03 0.29959268476311E+03 0.29515000475104E+03 0.29515002229080E+03 0.29883906663020E+03
 0.29953907163076E+03 0.29515000476834E+03 0.29515002223583E+03 0.29884428577940E+03 0.29959268476312E+03
 0.29515000475104E+03 0.29515002229080E+03 0.29883906663020E+03 0.29953907163076E+03 0.29515000476834E+03
 0.29515002223583E+03 0.29953152678184E+03 0.29515009299081E+03 -.21451588153255E+03 -.31673211114185E+03
 0.12536988293753E+03 0.21585354396860E+03 0.89856811616379E+02 0.12408095829666E+03 0.84742011917743E+02
 0.12408095829666E+03 0.14923608459284E+03 0.12405864330745E+03 0.82616654732870E+02 0.12405864330745E+03
 0.14742536980042E+03 0.12408095829666E+03 0.84742011917743E+02 0.12408095829666E+03 0.14923608459284E+03
 0.12405864330745E+03 0.82616654732866E+02 0.12405864330745E+03 0.14742536980041E+03 0.20328360204763E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32241333092291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93490461360837E+00 0.92239305932708E+00 0.00000000000000E+00 0.93490461360837E+00 0.92239305932708E+00
 0.13872755721795E+01 0.35727560078978E+00 0.10299999713898E+01 0.15449999570847E+01 0.12696426696627E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.35042210026254E-03 0.10559717639153E-04
 0.14220267816643E-01 0.42851753009978E-03 0.12696426696627E-01 0.15009767164644E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
    990.82846581
 0.71479576462889E+00 0.30849785425175E+03 0.50198865893093E+03 0.45588652303783E+03 0.43672427648868E+03
 0.22985684579805E+00 0.00000000000000E+00 0.19403878508906E+00 0.00000000000000E+00 -.18447464593494E+01
 0.17611982316268E-02 0.45242205816083E+00 0.45423620443967E+04 0.17033857666488E+04 0.17682603789305E+02
 0.66309764209895E+01 0.36507645140568E+03 0.29515592551378E+03 0.35914778461350E+03 0.40007121352230E+03
 0.29515111466254E+03 0.29515195007149E+03 0.35126108840602E+03 0.39992105169421E+03 0.29515091444439E+03
 0.29515194514799E+03 0.35914778461350E+03 0.40007121352230E+03 0.29515111466254E+03 0.29515195007149E+03
 0.35126108840602E+03 0.39992105169421E+03 0.29515091444439E+03 0.29515194514799E+03 0.44922257241531E+03
 0.33537466071716E+03 0.26277284338808E+04 0.22561934641236E+04 0.62435695514846E+03 0.11126492099137E+04
 0.48517046998950E+03 0.16741712074296E+04 0.14288550701435E+04 0.14114804621388E+04 0.22179667759206E+04
 0.14750911328683E+04 0.14247575355953E+04 0.12639008816965E+04 0.22152865227441E+04 0.16741712074296E+04
 0.14288550701435E+04 0.14114804621388E+04 0.22179667759206E+04 0.14750911328683E+04 0.14247575355953E+04
 0.12639008816965E+04 0.22152865227441E+04 0.20150785722603E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52797490494182E+03 0.18908936954193E+01
 0.18908936954193E+01 0.75673634383763E+01 0.26268628300308E-02 0.31377697526906E+03 0.31321403481282E+03
 0.31321502065773E+03 0.31321501889214E+03 0.22068371727787E+00 0.00000000000000E+00 0.22068369179576E+00
 0.00000000000000E+00 0.94751997128462E+01 0.67254241571065E-01 0.62996540242048E-01 0.11895160532807E+03
 0.44606851998026E+02 0.12699110092812E+03 0.47621662848043E+02 0.33537995436838E+03 0.44921709867436E+03
 0.29890727046787E+03 0.29966452072282E+03 0.29515000526240E+03 0.29515002433069E+03 0.29890213780657E+03
 0.29961021814971E+03 0.29515000528047E+03 0.29515002426961E+03 0.29890727046787E+03 0.29966452072282E+03
 0.29515000526240E+03 0.29515002433069E+03 0.29890213780657E+03 0.29961021814971E+03 0.29515000528047E+03
 0.29515002426961E+03 0.29960053253653E+03 0.29515010076099E+03 -.21661412606011E+03 -.32034430904718E+03
 0.12637941576146E+03 0.21770909179177E+03 0.90697778951503E+02 0.12589616839276E+03 0.85662639962151E+02
 0.12589616839276E+03 0.15074671932748E+03 0.12587716006279E+03 0.83537651771221E+02 0.12587716006279E+03
 0.14894092214779E+03 0.12589616839276E+03 0.85662639962150E+02 0.12589616839276E+03 0.15074671932748E+03
 0.12587716006278E+03 0.83537651771218E+02 0.12587716006278E+03 0.14894092214778E+03 0.20362187689607E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32259661656780E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93507861613308E+00 0.92258990182741E+00 0.00000000000000E+00 0.93507861613308E+00 0.92258990182741E+00
 0.13873978537042E+01 0.35739788231444E+00 0.10299999713898E+01 0.15449999570847E+01 0.12691680487987E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.35699281344511E-03 0.96134184907367E-05
 0.14243575888723E-01 0.38356361995482E-03 0.12691680487987E-01 0.14993745740614E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1000.66869693
 0.71503924501000E+00 0.30859169336418E+03 0.50222471277598E+03 0.45607297677291E+03 0.43688582151139E+03
 0.22985579165149E+00 0.00000000000000E+00 0.19404491079565E+00 0.00000000000000E+00 -.18468383719496E+01
 0.18506224425822E-02 0.45215724823481E+00 0.43228698711973E+04 0.16210762016990E+04 0.17692959763957E+02
 0.66348599114838E+01 0.36543800900084E+03 0.29515636988499E+03 0.35948340421371E+03 0.40053992656527E+03
 0.29515121050424E+03 0.29515211794817E+03 0.35157659910780E+03 0.40038947818120E+03 0.29515099355973E+03
 0.29515211262937E+03 0.35948340421371E+03 0.40053992656527E+03 0.29515121050424E+03 0.29515211794817E+03
 0.35157659910780E+03 0.40038947818120E+03 0.29515099355973E+03 0.29515211262937E+03 0.44962429944099E+03
 0.33580010455044E+03 0.26288844230361E+04 0.22551341029655E+04 0.62221184615238E+03 0.11085435183476E+04
 0.48322061296443E+03 0.16752308016729E+04 0.14282870121051E+04 0.14109764211423E+04 0.22150019938662E+04
 0.14765545019630E+04 0.14242023482595E+04 0.12639926322303E+04 0.22123360532211E+04 0.16752308016729E+04
 0.14282870121051E+04 0.14109764211423E+04 0.22150019938662E+04 0.14765545019630E+04 0.14242023482595E+04
 0.12639926322303E+04 0.22123360532211E+04 0.20124065979609E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52818430730630E+03 0.18908937157522E+01
 0.18908937157522E+01 0.76460852873479E+01 0.23548050747397E-02 0.31395686873693E+03 0.31338918693181E+03
 0.31339007811848E+03 0.31339007650960E+03 0.22060170253510E+00 0.00000000000000E+00 0.22060158075452E+00
 0.00000000000000E+00 0.94817648906251E+01 0.75024324173125E-01 0.70051700288287E-01 0.10663208350320E+03
 0.39987031313701E+02 0.11420136794792E+03 0.42825512980470E+02 0.33580538303063E+03 0.44961881949858E+03
 0.29897119735180E+03 0.29973650035266E+03 0.29515000582230E+03 0.29515002652418E+03 0.29896614912119E+03
 0.29968154171903E+03 0.29515000584110E+03 0.29515002645639E+03 0.29897119735180E+03 0.29973650035266E+03
 0.29515000582230E+03 0.29515002652418E+03 0.29896614912119E+03 0.29968154171903E+03 0.29515000584110E+03
 0.29515002645639E+03 0.29966933079100E+03 0.29515010905751E+03 -.21862762566929E+03 -.32387665651718E+03
 0.12732054366828E+03 0.21949334214784E+03 0.91536195761218E+02 0.12774308888658E+03 0.86566314356056E+02
 0.12774308888658E+03 0.15223686263631E+03 0.12772712918560E+03 0.84444846957919E+02 0.12772712918560E+03
 0.15043894485053E+03 0.12774308888658E+03 0.86566314356056E+02 0.12774308888658E+03 0.15223686263631E+03
 0.12772712918559E+03 0.84444846957915E+02 0.12772712918559E+03 0.15043894485052E+03 0.20392113288527E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32277888569648E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93525231898791E+00 0.92278532605806E+00 0.00000000000000E+00 0.93525231898791E+00 0.92278532605806E+00
 0.13875195938948E+01 0.35751962250500E+00 0.10299999713898E+01 0.15449999570847E+01 0.12686900159344E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.36338300407186E-03 0.87474751199848E-05
 0.14262469396393E-01 0.34333085146112E-03 0.12686900159344E-01 0.14977930727046E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1010.50892805
 0.71528182553638E+00 0.30868622360892E+03 0.50245902108240E+03 0.45625830097703E+03 0.43704672849568E+03
 0.22985474351416E+00 0.00000000000000E+00 0.19405099462296E+00 0.00000000000000E+00 -.18489388793681E+01
 0.19503195685710E-02 0.45189928971478E+00 0.41018918791147E+04 0.15382094546680E+04 0.17703059469399E+02
 0.66386473010247E+01 0.36579723175639E+03 0.29515684004565E+03 0.35981691890636E+03 0.40100509095753E+03
 0.29515131295081E+03 0.29515229738786E+03 0.35189032047808E+03 0.40085436385227E+03 0.29515107817067E+03
 0.29515229164881E+03 0.35981691890636E+03 0.40100509095753E+03 0.29515131295081E+03 0.29515229738786E+03
 0.35189032047808E+03 0.40085436385227E+03 0.29515107817067E+03 0.29515229164881E+03 0.45002210638720E+03
 0.33622305726236E+03 0.26299970733802E+04 0.22540557410793E+04 0.62009259252768E+03 0.11044912777679E+04
 0.48129822227756E+03 0.16762592535523E+04 0.14277147738322E+04 0.14104587667637E+04 0.22120545850045E+04
 0.14779867286443E+04 0.14236430906576E+04 0.12640678392357E+04 0.22094029987531E+04 0.16762592535523E+04
 0.14277147738322E+04 0.14104587667637E+04 0.22120545850045E+04 0.14779867286443E+04 0.14236430906576E+04
 0.12640678392357E+04 0.22094029987531E+04 0.20097491397086E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52839234331951E+03 0.18908937361686E+01
 0.18908937361686E+01 0.77248071363195E+01 0.21109214943409E-02 0.31413710125082E+03 0.31356369259463E+03
 0.31356449954174E+03 0.31356449807085E+03 0.22051983950642E+00 0.00000000000000E+00 0.22051972717163E+00
 0.00000000000000E+00 0.94882872466010E+01 0.83692194025201E-01 0.77927086253951E-01 0.95588365117911E+02
 0.35845636919217E+02 0.10266006833528E+03 0.38497525625731E+02 0.33622831750561E+03 0.45001662328638E+03
 0.29903610290951E+03 0.29980860176629E+03 0.29515000643472E+03 0.29515002888028E+03 0.29903113701046E+03
 0.29975302425187E+03 0.29515000645421E+03 0.29515002880514E+03 0.29903610290951E+03 0.29980860176629E+03
 0.29515000643472E+03 0.29515002888028E+03 0.29903113701046E+03 0.29975302425187E+03 0.29515000645421E+03
 0.29515002880514E+03 0.29973786126337E+03 0.29515011790749E+03 -.22055033223631E+03 -.32730434952948E+03
 0.12818727685497E+03 0.22120088737249E+03 0.92372674133243E+02 0.12962633324272E+03 0.87451977248960E+02
 0.12962633324272E+03 0.15370562875638E+03 0.12961318087640E+03 0.85337469919068E+02 0.12961318087640E+03
 0.15191881624609E+03 0.12962633324272E+03 0.87451977248959E+02 0.12962633324272E+03 0.15370562875638E+03
 0.12961318087639E+03 0.85337469919065E+02 0.12961318087639E+03 0.15191881624608E+03 0.20422573832093E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32296018792905E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93542584783092E+00 0.92297940927685E+00 0.00000000000000E+00 0.93542584783092E+00 0.92297940927685E+00
 0.13876408841580E+01 0.35764091276819E+00 0.10299999713898E+01 0.15449999570847E+01 0.12682087704600E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.36959618897267E-03 0.79555803159650E-05
 0.14277426088111E-01 0.30732245985799E-03 0.12682087704600E-01 0.14962300317257E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1020.34915917
 0.71552369514238E+00 0.30878158514330E+03 0.50269161454400E+03 0.45644254095668E+03 0.43720706862929E+03
 0.22985370077886E+00 0.00000000000000E+00 0.19405704022617E+00 0.00000000000000E+00 -.18510502582065E+01
 0.20614756540692E-02 0.45164864056780E+00 0.38807152459979E+04 0.14552682172492E+04 0.17712884046197E+02
 0.66423315173240E+01 0.36615413427864E+03 0.29515733701612E+03 0.36014833960553E+03 0.40146676477807E+03
 0.29515142233658E+03 0.29515248897323E+03 0.35220226045781E+03 0.40131576684224E+03 0.29515116855861E+03
 0.29515248278787E+03 0.36014833960553E+03 0.40146676477807E+03 0.29515142233658E+03 0.29515248897323E+03
 0.35220226045780E+03 0.40131576684224E+03 0.29515116855861E+03 0.29515248278787E+03 0.45041606136065E+03
 0.33664376408555E+03 0.26310633115509E+04 0.22529566051776E+04 0.61799901270140E+03 0.11004918228711E+04
 0.47940281510620E+03 0.16772543041929E+04 0.14271387164808E+04 0.14099262553713E+04 0.22091246060622E+04
 0.14793858543557E+04 0.14230801380891E+04 0.12641255215387E+04 0.22064874298415E+04 0.16772543041929E+04
 0.14271387164808E+04 0.14099262553713E+04 0.22091246060623E+04 0.14793858543557E+04 0.14230801380891E+04
 0.12641255215387E+04 0.22064874298415E+04 0.20071054392483E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52859905757628E+03 0.18908937566908E+01
 0.18908937566908E+01 0.78035289852912E+01 0.18922888698496E-02 0.31431680309246E+03 0.31373757346426E+03
 0.31373830417745E+03 0.31373830283257E+03 0.22043821723003E+00 0.00000000000000E+00 0.22043813057209E+00
 0.00000000000000E+00 0.94947742919963E+01 0.93361878364681E-01 0.86717183131911E-01 0.85688078904659E+02
 0.32133029589247E+02 0.92253919131929E+02 0.34595219674473E+02 0.33664900286742E+03 0.45041057799984E+03
 0.29910203111459E+03 0.29988080256032E+03 0.29515000710392E+03 0.29515003140835E+03 0.29909714542473E+03
 0.29982464742893E+03 0.29515000712403E+03 0.29515003132517E+03 0.29910203111459E+03 0.29988080256032E+03
 0.29515000710392E+03 0.29515003140835E+03 0.29909714542473E+03 0.29982464742893E+03 0.29515000712403E+03
 0.29515003132517E+03 0.29980606002986E+03 0.29515012733897E+03 -.22237599532556E+03 -.33062826348591E+03
 0.12897322283575E+03 0.22282595829538E+03 0.93207869345452E+02 0.13155088934286E+03 0.88318510453870E+02
 0.13155088934286E+03 0.15515208735362E+03 0.13154031940213E+03 0.86214708498044E+02 0.13154031940213E+03
 0.15337988683412E+03 0.13155088934286E+03 0.88318510453870E+02 0.13155088934286E+03 0.15515208735362E+03
 0.13154031940212E+03 0.86214708498041E+02 0.13154031940212E+03 0.15337988683412E+03 0.20451743522271E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32314054995404E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93559941887299E+00 0.92317212122163E+00 0.00000000000000E+00 0.93559941887299E+00 0.92317212122163E+00
 0.13877618189610E+01 0.35776184757119E+00 0.10299999713898E+01 0.15449999570847E+01 0.12677196726092E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.37563112238562E-03 0.72317731388426E-05
 0.14289033586420E-01 0.27509714481063E-03 0.12677196726092E-01 0.14946993626755E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1030.18939029
 0.71576506261639E+00 0.30887793333796E+03 0.50292252345425E+03 0.45662574405593E+03 0.43736691859874E+03
 0.22985266260767E+00 0.00000000000000E+00 0.19406305150745E+00 0.00000000000000E+00 -.18531751600938E+01
 0.21854140387416E-02 0.45140581089320E+00 0.36606335724861E+04 0.13727375896823E+04 0.17722412532019E+02
 0.66459046995072E+01 0.36650872864737E+03 0.29515786183422E+03 0.36047767473267E+03 0.40192500493711E+03
 0.29515153900573E+03 0.29515269330383E+03 0.35251242493713E+03 0.40177374413839E+03 0.29515126501349E+03
 0.29515268664494E+03 0.36047767473267E+03 0.40192500493711E+03 0.29515153900573E+03 0.29515269330383E+03
 0.35251242493713E+03 0.40177374413839E+03 0.29515126501349E+03 0.29515268664494E+03 0.45080623080902E+03
 0.33706247258864E+03 0.26320796629497E+04 0.22518346902405E+04 0.61593093719480E+03 0.10965445204423E+04
 0.47753392856151E+03 0.16782134036264E+04 0.14265592147489E+04 0.14093774826235E+04 0.22062121416040E+04
 0.14807496599128E+04 0.14225138810051E+04 0.12641645582514E+04 0.22035894462456E+04 0.16782134036264E+04
 0.14265592147489E+04 0.14093774826235E+04 0.22062121416040E+04 0.14807496599128E+04 0.14225138810051E+04
 0.12641645582514E+04 0.22035894462456E+04 0.20044746783883E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52880449632016E+03 0.18908937773443E+01
 0.18908937773443E+01 0.78822508342628E+01 0.16962906752696E-02 0.31449560769634E+03 0.31391086074543E+03
 0.31391152201263E+03 0.31391152078452E+03 0.22035688596680E+00 0.00000000000000E+00 0.22035679081008E+00
 0.00000000000000E+00 0.95012305211126E+01 0.10414938779672E+00 0.96528550082899E-01 0.76812741478758E+02
 0.28804778054534E+02 0.82877034754273E+02 0.31078888032852E+02 0.33706768656273E+03 0.45080074995072E+03
 0.29916903345875E+03 0.29995307948037E+03 0.29515000783445E+03 0.29515003411814E+03 0.29916422584813E+03
 0.29989639240043E+03 0.29515000785511E+03 0.29515003402617E+03 0.29916903345875E+03 0.29995307948037E+03
 0.29515000783445E+03 0.29515003411814E+03 0.29916422584813E+03 0.29989639240043E+03 0.29515000785511E+03
 0.29515003402617E+03 0.29987385881623E+03 0.29515013738094E+03 -.22409780813813E+03 -.33384548185736E+03
 0.12967139659869E+03 0.22436231586453E+03 0.94042562282848E+02 0.13352205002162E+03 0.89164646569580E+02
 0.13352205002162E+03 0.15657523075650E+03 0.13351385310571E+03 0.87075625549077E+02 0.13351385310571E+03
 0.15482145208526E+03 0.13352205002162E+03 0.89164646569579E+02 0.13352205002162E+03 0.15657523075650E+03
 0.13351385310571E+03 0.87075625549075E+02 0.13351385310571E+03 0.15482145208525E+03 0.20478571850142E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32332001564915E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93577322686146E+00 0.92336348488514E+00 0.00000000000000E+00 0.93577322686146E+00 0.92336348488514E+00
 0.13878825026980E+01 0.35788253130819E+00 0.10299999713898E+01 0.15449999570847E+01 0.12672203433253E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.38148552942025E-03 0.65705268665580E-05
 0.14297764460129E-01 0.24625795285017E-03 0.12672203433253E-01 0.14932078469266E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1040.02962142
 0.71600615568487E+00 0.30897544089160E+03 0.50315177778161E+03 0.45680796028108E+03 0.43752636172593E+03
 0.22985162803195E+00 0.00000000000000E+00 0.19406903264202E+00 0.00000000000000E+00 -.18553165481559E+01
 0.23236129702979E-02 0.45117137050044E+00 0.34429141609476E+04 0.12910928103553E+04 0.17731621559069E+02
 0.66493580846507E+01 0.36686102418283E+03 0.29515841555484E+03 0.36080492997454E+03 0.40237986729147E+03
 0.29515166331232E+03 0.29515291099602E+03 0.35282081753848E+03 0.40222835169773E+03 0.29515136783370E+03
 0.29515290383523E+03 0.36080492997454E+03 0.40237986729147E+03 0.29515166331232E+03 0.29515291099602E+03
 0.35282081753848E+03 0.40222835169773E+03 0.29515136783370E+03 0.29515290383523E+03 0.45119267976679E+03
 0.33747944473825E+03 0.26330422057724E+04 0.22506877362912E+04 0.61388820789277E+03 0.10926487671731E+04
 0.47569111824084E+03 0.16791336760281E+04 0.14259766608194E+04 0.14088108661440E+04 0.22033173080672E+04
 0.14820756347292E+04 0.14219447291711E+04 0.12641836743639E+04 0.22007091812212E+04 0.16791336760281E+04
 0.14259766608194E+04 0.14088108661440E+04 0.22033173080673E+04 0.14820756347292E+04 0.14219447291711E+04
 0.12641836743639E+04 0.22007091812212E+04 0.20018559726216E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52900870785589E+03 0.18908937981581E+01
 0.18908937981581E+01 0.79609726832344E+01 0.15205870617194E-02 0.31467427458134E+03 0.31408359440902E+03
 0.31408419319611E+03 0.31408419207325E+03 0.22027576619074E+00 0.00000000000000E+00 0.22027570811554E+00
 0.00000000000000E+00 0.95076601520967E+01 0.11618382918840E+00 0.10747885560102E+00 0.68856398139775E+02
 0.25821149302415E+02 0.74433245081225E+02 0.27912466905459E+02 0.33748463045726E+03 0.45118720404336E+03
 0.29923716867603E+03 0.30002540841075E+03 0.29515000863114E+03 0.29515003701979E+03 0.29923243702041E+03
 0.29996823977738E+03 0.29515000865226E+03 0.29515003691822E+03 0.29923716867603E+03 0.30002540841075E+03
 0.29515000863114E+03 0.29515003701979E+03 0.29923243702041E+03 0.29996823977738E+03 0.29515000865226E+03
 0.29515003691822E+03 0.29994118493425E+03 0.29515014806334E+03 -.22570876272482E+03 -.33694550827141E+03
 0.13027445543160E+03 0.22580347350619E+03 0.94877645797432E+02 0.13554531445840E+03 0.89989023276827E+02
 0.13554531445840E+03 0.15797401773468E+03 0.13553929579514E+03 0.87919201571850E+02 0.13553929579514E+03
 0.15624278450915E+03 0.13554531445840E+03 0.89989023276827E+02 0.13554531445840E+03 0.15797401773468E+03
 0.13553929579513E+03 0.87919201571847E+02 0.13553929579513E+03 0.15624278450915E+03 0.20503757897600E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32349863698865E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93594747927270E+00 0.92355353238609E+00 0.00000000000000E+00 0.93594747927270E+00 0.92355353238609E+00
 0.13880030492322E+01 0.35800307784243E+00 0.10299999713898E+01 0.15449999570847E+01 0.12667085598098E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.38715757352106E-03 0.59667468413600E-05
 0.14304038705252E-01 0.22044920105020E-03 0.12667085598098E-01 0.14917612226664E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1050.00956077
 0.71625058762708E+00 0.30907576152983E+03 0.50338268308937E+03 0.45699186750702E+03 0.43768780564615E+03
 0.22985058116059E+00 0.00000000000000E+00 0.19407507249999E+00 0.00000000000000E+00 -.18574185030618E+01
 0.24800482155785E-02 0.45094277650920E+00 0.32257437374595E+04 0.12096539015473E+04 0.17740610154417E+02
 0.66527288079063E+01 0.36721588030638E+03 0.29515900764846E+03 0.36113462733807E+03 0.40283749112221E+03
 0.29515179752871E+03 0.29515314602485E+03 0.35313174761917E+03 0.40268572661092E+03 0.29515147890576E+03
 0.29515313832494E+03 0.36113462733807E+03 0.40283749112220E+03 0.29515179752871E+03 0.29515314602485E+03
 0.35313174761917E+03 0.40268572661092E+03 0.29515147890576E+03 0.29515313832494E+03 0.45158001820897E+03
 0.33790016453674E+03 0.26339585914075E+04 0.22494972915003E+04 0.61185867457190E+03 0.10887762476126E+04
 0.47385827966784E+03 0.16800236498032E+04 0.14253862360595E+04 0.14082166652880E+04 0.22004059554262E+04
 0.14833779737788E+04 0.14213680940513E+04 0.12641808490720E+04 0.21978127195825E+04 0.16800236498032E+04
 0.14253862360595E+04 0.14082166652880E+04 0.22004059554262E+04 0.14833779737788E+04 0.14213680940513E+04
 0.12641808490721E+04 0.21978127195825E+04 0.19992209706211E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52921467275033E+03 0.18908938185886E+01
 0.18908938185886E+01 0.80408121980424E+01 0.13609594189656E-02 0.31485499210864E+03 0.31425827157621E+03
 0.31425881298449E+03 0.31425881195933E+03 0.22019381061562E+00 0.00000000000000E+00 0.22019373711281E+00
 0.00000000000000E+00 0.95143367287995E+01 0.12981108546851E+00 0.11988886734749E+00 0.61628018679041E+02
 0.23110507004640E+02 0.66728464260258E+02 0.25023174097597E+02 0.33790531811870E+03 0.45157455006492E+03
 0.29930766397165E+03 0.30009891621757E+03 0.29515000951175E+03 0.29515004016867E+03 0.29930300736150E+03
 0.30004131801192E+03 0.29515000953323E+03 0.29515004005646E+03 0.29930766397165E+03 0.30009891621757E+03
 0.29515000951175E+03 0.29515004016867E+03 0.29930300736150E+03 0.30004131801192E+03 0.29515000953323E+03
 0.29515004005646E+03 0.30000899297760E+03 0.29515015958082E+03 -.22721508195296E+03 -.33995517625272E+03
 0.13078638902930E+03 0.22716536891432E+03 0.95725047939870E+02 0.13766269310139E+03 0.90804911372684E+02
 0.13766269310139E+03 0.15936929728598E+03 0.13765870139202E+03 0.88759331381843E+02 0.13765870139202E+03
 0.15766535202937E+03 0.13766269310139E+03 0.90804911372683E+02 0.13766269310139E+03 0.15936929728598E+03
 0.13765870139202E+03 0.88759331381841E+02 0.13765870139202E+03 0.15766535202937E+03 0.20531038717724E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32367871716835E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93615054599472E+00 0.92370698586953E+00 0.00000000000000E+00 0.93615054599472E+00 0.92370698586953E+00
 0.13881252652033E+01 0.35812529381354E+00 0.10299999713898E+01 0.15449999570847E+01 0.12661063070027E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.39276893870699E-03 0.54089081349205E-05
 0.14310206644616E-01 0.19706902838923E-03 0.12661063070027E-01 0.14905453519847E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1060.51921552
 0.71650828060795E+00 0.30918521616436E+03 0.50362594250463E+03 0.45718647900122E+03 0.43785964473766E+03
 0.22984947663882E+00 0.00000000000000E+00 0.19408140570944E+00 0.00000000000000E+00 -.18598797779356E+01
 0.26598781889092E-02 0.45071114300327E+00 0.30076565285423E+04 0.11278711982034E+04 0.17749727567623E+02
 0.66561478378587E+01 0.36758648562622E+03 0.29515965872486E+03 0.36147909660305E+03 0.40331362105389E+03
 0.29515194628319E+03 0.29515340649364E+03 0.35345719976765E+03 0.40316161311367E+03 0.29515160205857E+03
 0.29515339819873E+03 0.36147909660305E+03 0.40331362105389E+03 0.29515194628319E+03 0.29515340649364E+03
 0.35345719976765E+03 0.40316161311367E+03 0.29515160205857E+03 0.29515339819873E+03 0.45197825068587E+03
 0.33833514309299E+03 0.26348773033245E+04 0.22482579942006E+04 0.60987865937912E+03 0.10849653065522E+04
 0.47203725387614E+03 0.16809248563988E+04 0.14248043633355E+04 0.14075994770445E+04 0.21974389099481E+04
 0.14847098972696E+04 0.14208009039931E+04 0.12641757387220E+04 0.21948613607253E+04 0.16809248563988E+04
 0.14248043633355E+04 0.14075994770445E+04 0.21974389099481E+04 0.14847098972696E+04 0.14208009039931E+04
 0.12641757387221E+04 0.21948613607253E+04 0.19965596306683E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52943222348062E+03 0.18908938425117E+01
 0.18908938425117E+01 0.81248894361022E+01 0.12109137314775E-02 0.31504421531121E+03 0.31444180450532E+03
 0.31444229081700E+03 0.31444228988785E+03 0.22010776257017E+00 0.00000000000000E+00 0.22010770358582E+00
 0.00000000000000E+00 0.95211166795323E+01 0.13091521723795E+00 0.13091546284458E+00 0.61108251346054E+02
 0.22915594254770E+02 0.61108136702670E+02 0.22915551263501E+02 0.33834029257162E+03 0.45197275505358E+03
 0.29938441486158E+03 0.30017751129599E+03 0.29515001050667E+03 0.29515004367324E+03 0.29937983679273E+03
 0.30011952419321E+03 0.29515001052843E+03 0.29515004354901E+03 0.29938441486158E+03 0.30017751129599E+03
 0.29515001050667E+03 0.29515004367324E+03 0.29937983679273E+03 0.30011952419321E+03 0.29515001052843E+03
 0.29515004354901E+03 0.30008080984322E+03 0.29515017233211E+03 -.22894849316952E+03 -.34323111241365E+03
 0.13143433401269E+03 0.22870375321159E+03 0.96612247528841E+02 0.13973802210905E+03 0.91654807952221E+02
 0.13973802210905E+03 0.16082113978604E+03 0.13973601280958E+03 0.89627441856560E+02 0.13973601280958E+03
 0.15913823653965E+03 0.13973802210905E+03 0.91654807952220E+02 0.13973802210905E+03 0.16082113978604E+03
 0.13973601280958E+03 0.89627441856557E+02 0.13973601280958E+03 0.15913823653965E+03 0.20552884203817E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32386810172360E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93632133777576E+00 0.92394198552301E+00 0.00000000000000E+00 0.93632133777576E+00 0.92394198552301E+00
 0.13882541116937E+01 0.35825414030398E+00 0.10299999713898E+01 0.15449999570847E+01 0.12655499219369E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.39824167389931E-03 0.48721394528969E-05
 0.14312267677233E-01 0.17509810896460E-03 0.12655499219369E-01 0.14890479599550E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1070.47877334
 0.71675188357835E+00 0.30928726340824E+03 0.50385364674099E+03 0.45736837282940E+03 0.43802008084365E+03
 0.22984842692695E+00 0.00000000000000E+00 0.19408739266638E+00 0.00000000000000E+00 -.18621327437076E+01
 0.27801366970693E-02 0.45049840572801E+00 0.28775563476549E+04 0.10790836303706E+04 0.17758109458949E+02
 0.66592910471059E+01 0.36793630074024E+03 0.29516030963746E+03 0.36180427884881E+03 0.40376336269113E+03
 0.29515209645470E+03 0.29515366941885E+03 0.35376432778711E+03 0.40361112434987E+03 0.29515172644659E+03
 0.29515366052639E+03 0.36180427884881E+03 0.40376336269113E+03 0.29515209645470E+03 0.29515366941885E+03
 0.35376432778711E+03 0.40361112434987E+03 0.29515172644659E+03 0.29515366052639E+03 0.45235708431573E+03
 0.33873875302569E+03 0.26358863479161E+04 0.22472164107792E+04 0.60794107545110E+03 0.10812495453055E+04
 0.47026876447712E+03 0.16818791327567E+04 0.14242349484393E+04 0.14071099562579E+04 0.21946011386320E+04
 0.14860601869575E+04 0.14202449540526E+04 0.12642545838523E+04 0.21920379989423E+04 0.16818791327567E+04
 0.14242349484393E+04 0.14071099562579E+04 0.21946011386320E+04 0.14860601869575E+04 0.14202449540526E+04
 0.12642545838523E+04 0.21920379989423E+04 0.19940458236964E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52963574007274E+03 0.18908938644100E+01
 0.18908938644100E+01 0.82045658986111E+01 0.10840013050997E-02 0.31522319987515E+03 0.31461548821422E+03
 0.31461592738771E+03 0.31461592654165E+03 0.22002650065340E+00 0.00000000000000E+00 0.22002644852679E+00
 0.00000000000000E+00 0.95276219271736E+01 0.12795785442567E+00 0.12930115880785E+00 0.62520585671804E+02
 0.23445219626926E+02 0.61871061897351E+02 0.23201648211507E+02 0.33874389615773E+03 0.45235161376134E+03
 0.29945455284643E+03 0.30025179879322E+03 0.29515001153545E+03 0.29515004722984E+03 0.29945004653236E+03
 0.30019329225093E+03 0.29515001155733E+03 0.29515004709318E+03 0.29945455284643E+03 0.30025179879322E+03
 0.29515001153545E+03 0.29515004722984E+03 0.29945004653236E+03 0.30019329225093E+03 0.29515001155733E+03
 0.29515004709318E+03 0.30015031634359E+03 0.29515018518925E+03 -.23167395561469E+03 -.34736840990344E+03
 0.13290501617464E+03 0.23100921638745E+03 0.97439675131946E+02 0.14106932539156E+03 0.92584458946944E+02
 0.14106932539156E+03 0.16232148493526E+03 0.14106916904453E+03 0.90529660157086E+02 0.14106916904453E+03
 0.16061472971356E+03 0.14106932539156E+03 0.92584458946943E+02 0.14106932539156E+03 0.16232148493525E+03
 0.14106916904452E+03 0.90529660157083E+02 0.14106916904452E+03 0.16061472971356E+03 0.20560968932832E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32404663771252E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93648847174536E+00 0.92414651480233E+00 0.00000000000000E+00 0.93648847174536E+00 0.92414651480233E+00
 0.13883759131789E+01 0.35837594178918E+00 0.10299999713898E+01 0.15449999570847E+01 0.12649139094236E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.40356597424332E-03 0.44140752524149E-05
 0.14314921956520E-01 0.15657202732962E-03 0.12649139094236E-01 0.14879474033346E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1080.46696949
 0.71698956332005E+00 0.30938522995262E+03 0.50408030911980E+03 0.45754886252225E+03 0.43817840698913E+03
 0.22984738127036E+00 0.00000000000000E+00 0.19409335775717E+00 0.00000000000000E+00 -.18643212432139E+01
 0.28595444815206E-02 0.45028230627968E+00 0.27976483848035E+04 0.10491181443013E+04 0.17766631929417E+02
 0.66624869735313E+01 0.36828550428978E+03 0.29516099603144E+03 0.36212899873562E+03 0.40421135663811E+03
 0.29515225627019E+03 0.29515394920125E+03 0.35407113621273E+03 0.40405889160995E+03 0.29515185888530E+03
 0.29515393967598E+03 0.36212899873562E+03 0.40421135663811E+03 0.29515225627019E+03 0.29515394920125E+03
 0.35407113621273E+03 0.40405889160995E+03 0.29515185888530E+03 0.29515393967598E+03 0.45273404131851E+03
 0.33913864109490E+03 0.26370095580682E+04 0.22462539402681E+04 0.60602051006859E+03 0.10775617759306E+04
 0.46851116331169E+03 0.16829149886456E+04 0.14236627397864E+04 0.14066742339527E+04 0.21917692145203E+04
 0.14874810696368E+04 0.14196858331403E+04 0.12643768197667E+04 0.21892200713789E+04 0.16829149886456E+04
 0.14236627397864E+04 0.14066742339527E+04 0.21917692145203E+04 0.14874810696368E+04 0.14196858331403E+04
 0.12643768197667E+04 0.21892200713789E+04 0.19915686806431E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.52983784314559E+03 0.18908938856817E+01
 0.18908938856817E+01 0.82844714678393E+01 0.97007558823936E-03 0.31540225556047E+03 0.31478932172081E+03
 0.31478971811558E+03 0.31478971734575E+03 0.21994527236990E+00 0.00000000000000E+00 0.21994522581852E+00
 0.00000000000000E+00 0.95339299435181E+01 0.14158049892502E+00 0.13430498040458E+00 0.56504956973182E+02
 0.21189358864943E+02 0.59565922096864E+02 0.22337220786324E+02 0.33914379547618E+03 0.45272859855980E+03
 0.29952471562981E+03 0.30032642701970E+03 0.29515001265570E+03 0.29515005103406E+03 0.29952027924751E+03
 0.30026737859312E+03 0.29515001267754E+03 0.29515005088384E+03 0.29952471562981E+03 0.30032642701970E+03
 0.29515001265570E+03 0.29515005103406E+03 0.29952027924751E+03 0.30026737859312E+03 0.29515001267754E+03
 0.29515005088384E+03 0.30022039199232E+03 0.29515019885823E+03 -.23374564471068E+03 -.35083058409745E+03
 0.13386569147643E+03 0.23280012131802E+03 0.98265101384210E+02 0.14273840855038E+03 0.93427825396932E+02
 0.14273840855038E+03 0.16373559137288E+03 0.14273988351655E+03 0.91371567980924E+02 0.14273988351655E+03
 0.16203109874913E+03 0.14273840855038E+03 0.93427825396931E+02 0.14273840855038E+03 0.16373559137288E+03
 0.14273988351654E+03 0.91371567980920E+02 0.14273988351654E+03 0.16203109874912E+03 0.20584885852456E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32422563191653E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93664856764223E+00 0.92435280693418E+00 0.00000000000000E+00 0.93664856764223E+00 0.92435280693418E+00
 0.13884947530498E+01 0.35849478166002E+00 0.10299999713898E+01 0.15449999570847E+01 0.12643911601466E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.40906139442622E-03 0.39992892860751E-05
 0.14312137447325E-01 0.13992613024313E-03 0.12643911601466E-01 0.14865124261281E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1091.00475223
 0.71723620129216E+00 0.30948582016401E+03 0.50431718497824E+03 0.45773714898093E+03 0.43834311803568E+03
 0.22984629443684E+00 0.00000000000000E+00 0.19409958417918E+00 0.00000000000000E+00 -.18665465624042E+01
 0.29681492736272E-02 0.45005352096787E+00 0.26952822322928E+04 0.10107308371098E+04 0.17775663620619E+02
 0.66658738577321E+01 0.36865176248008E+03 0.29516175848339E+03 0.36246963415065E+03 0.40468073307927E+03
 0.29515243547792E+03 0.29515426289738E+03 0.35439311752331E+03 0.40452803456152E+03 0.29515200746686E+03
 0.29515425266611E+03 0.36246963415065E+03 0.40468073307927E+03 0.29515243547792E+03 0.29515426289738E+03
 0.35439311752331E+03 0.40452803456152E+03 0.29515200746686E+03 0.29515425266611E+03 0.45312845625524E+03
 0.33956253433002E+03 0.26380940186888E+04 0.22451284815845E+04 0.60398864157988E+03 0.10736795934771E+04
 0.46667100868936E+03 0.16839365974104E+04 0.14230332993284E+04 0.14061349508597E+04 0.21887690001096E+04
 0.14889135886765E+04 0.14190703541358E+04 0.12644298415990E+04 0.21862347271121E+04 0.16839365974104E+04
 0.14230332993284E+04 0.14061349508597E+04 0.21887690001096E+04 0.14889135886765E+04 0.14190703541358E+04
 0.12644298415990E+04 0.21862347271121E+04 0.19889149491094E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53004879397167E+03 0.18908939073113E+01
 0.18908939073113E+01 0.83687737297625E+01 0.86282790092795E-03 0.31559058834865E+03 0.31497210513164E+03
 0.31497246089469E+03 0.31497246019788E+03 0.21985985762287E+00 0.00000000000000E+00 0.21985981664191E+00
 0.00000000000000E+00 0.95405508665276E+01 0.14180505904876E+00 0.14324109835719E+00 0.56415476666803E+02
 0.21155803750051E+02 0.55849892885150E+02 0.20943709831931E+02 0.33956763775738E+03 0.45312304010341E+03
 0.29960039094790E+03 0.30040490760908E+03 0.29515001394234E+03 0.29515005532272E+03 0.29959602579084E+03
 0.30034538972626E+03 0.29515001396395E+03 0.29515005515692E+03 0.29960039094790E+03 0.30040490760908E+03
 0.29515001394234E+03 0.29515005532272E+03 0.29959602579084E+03 0.30034538972626E+03 0.29515001396395E+03
 0.29515005515692E+03 0.30029303537588E+03 0.29515021417196E+03 -.23561148249040E+03 -.35418332288346E+03
 0.13461310709714E+03 0.23442872309199E+03 0.99142550459363E+02 0.14469446025663E+03 0.94275661752761E+02
 0.14469446025663E+03 0.16518433411579E+03 0.14469747221879E+03 0.92231428692965E+02 0.14469747221879E+03
 0.16349518688204E+03 0.14469446025664E+03 0.94275661752761E+02 0.14469446025664E+03 0.16518433411579E+03
 0.14469747221878E+03 0.92231428692961E+02 0.14469747221878E+03 0.16349518688203E+03 0.20608079573136E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32441362567534E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93682116733189E+00 0.92455594754823E+00 0.00000000000000E+00 0.93682116733189E+00 0.92455594754823E+00
 0.13886180720359E+01 0.35861810064608E+00 0.10299999713898E+01 0.15449999570847E+01 0.12638306532966E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.41497512962524E-03 0.36046057664149E-05
 0.14307483827907E-01 0.12427934839261E-03 0.12638306532966E-01 0.14850342911692E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1100.48875670
 0.71745538024026E+00 0.30957508821485E+03 0.50452862684282E+03 0.45790513844765E+03 0.43848994470515E+03
 0.22984532223424E+00 0.00000000000000E+00 0.19410512761787E+00 0.00000000000000E+00 -.18685288231552E+01
 0.30398080657764E-02 0.44984752160349E+00 0.26317451059058E+04 0.98690441471467E+03 0.17783803657479E+02
 0.66689263715547E+01 0.36897954784580E+03 0.29516247926713E+03 0.36277455487914E+03 0.40510017850201E+03
 0.29515260642818E+03 0.29515456210375E+03 0.35468147442598E+03 0.40494727500903E+03 0.29515214926865E+03
 0.29515455120227E+03 0.36277455487914E+03 0.40510017850201E+03 0.29515260642818E+03 0.29515456210375E+03
 0.35468147442598E+03 0.40494727500903E+03 0.29515214926865E+03 0.29515455120227E+03 0.45348021376118E+03
 0.33993451039871E+03 0.26391332647358E+04 0.22441811014692E+04 0.60218316417971E+03 0.10702276127819E+04
 0.46503353278129E+03 0.16849016530891E+04 0.14224617456761E+04 0.14056959291928E+04 0.21860788900920E+04
 0.14902397767705E+04 0.14185110956288E+04 0.12645137568790E+04 0.21835576859376E+04 0.16849016530891E+04
 0.14224617456761E+04 0.14056959291928E+04 0.21860788900920E+04 0.14902397767705E+04 0.14185110956288E+04
 0.12645137568790E+04 0.21835576859376E+04 0.19865637010786E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53023701956731E+03 0.18908939265784E+01
 0.18908939265784E+01 0.84446457654933E+01 0.77647506829681E-03 0.31575948852777E+03 0.31513621341276E+03
 0.31513653605115E+03 0.31513653541463E+03 0.21978323662275E+00 0.00000000000000E+00 0.21978319878924E+00
 0.00000000000000E+00 0.95464707003458E+01 0.14218021351992E+00 0.14198848064378E+00 0.56266619678970E+02
 0.21099982379614E+02 0.56342598806098E+02 0.21128474552287E+02 0.33993958674743E+03 0.45347481634639E+03
 0.29966736828250E+03 0.30047594615216E+03 0.29515001519854E+03 0.29515005943517E+03 0.29966306557206E+03
 0.30041590890180E+03 0.29515001521974E+03 0.29515005925413E+03 0.29966736828250E+03 0.30047594615216E+03
 0.29515001519854E+03 0.29515005943517E+03 0.29966306557206E+03 0.30041590890181E+03 0.29515001521974E+03
 0.29515005925413E+03 0.30035982497066E+03 0.29515022876933E+03 -.23810469784043E+03 -.35795959906825E+03
 0.13595020497947E+03 0.23654820636713E+03 0.99918250362769E+02 0.14594758204307E+03 0.95133912618397E+02
 0.14594758204307E+03 0.16657934796377E+03 0.14595193240638E+03 0.93065839796354E+02 0.14595193240638E+03
 0.16486992872172E+03 0.14594758204307E+03 0.95133912618397E+02 0.14594758204307E+03 0.16657934796377E+03
 0.14595193240637E+03 0.93065839796350E+02 0.14595193240637E+03 0.16486992872171E+03 0.20626842277395E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32458220085798E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93697282917399E+00 0.92473928169766E+00 0.00000000000000E+00 0.93697282917399E+00 0.92473928169766E+00
 0.13887276615099E+01 0.35872769012013E+00 0.10299999713898E+01 0.15449999570847E+01 0.12633100880204E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.42033300461848E-03 0.32828289822295E-05
 0.14302255079016E-01 0.11170157225044E-03 0.12633100880204E-01 0.14837572484868E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1110.59184963
 0.71768361360249E+00 0.30966728702839E+03 0.50475234958852E+03 0.45808256536918E+03 0.43864448391792E+03
 0.22984429934453E+00 0.00000000000000E+00 0.19411095820238E+00 0.00000000000000E+00 -.18705945820008E+01
 0.30940113324046E-02 0.44962387404088E+00 0.25856401740399E+04 0.96961506526497E+03 0.17792649505245E+02
 0.66722435644670E+01 0.36932699852732E+03 0.29516328413169E+03 0.36309785824875E+03 0.40554387711091E+03
 0.29515279898450E+03 0.29515489908462E+03 0.35498735099522E+03 0.40539075974420E+03 0.29515230906492E+03
 0.29515488743174E+03 0.36309785824875E+03 0.40554387711091E+03 0.29515279898450E+03 0.29515489908462E+03
 0.35498735099522E+03 0.40539075974420E+03 0.29515230906492E+03 0.29515488743174E+03 0.45385154435499E+03
 0.34032658768104E+03 0.26402916627219E+04 0.22432088902127E+04 0.60028454300012E+03 0.10665976271323E+04
 0.46331166141713E+03 0.16859662366680E+04 0.14218482618110E+04 0.14052526305874E+04 0.21832263293807E+04
 0.14916816000470E+04 0.14179104415629E+04 0.12646193524517E+04 0.21807187415819E+04 0.16859662366680E+04
 0.14218482618110E+04 0.14052526305874E+04 0.21832263293807E+04 0.14916816000470E+04 0.14179104415629E+04
 0.12646193524517E+04 0.21807187415819E+04 0.19840893020662E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53043588485695E+03 0.18908939466571E+01
 0.18908939466571E+01 0.85254705089157E+01 0.69397079077849E-03 0.31593877275147E+03 0.31531048320015E+03
 0.31531077387655E+03 0.31531077329877E+03 0.21970186937304E+00 0.00000000000000E+00 0.21970183684044E+00
 0.00000000000000E+00 0.95526307489621E+01 0.15218280146340E+00 0.14627788505812E+00 0.52568358073785E+02
 0.19713134277669E+02 0.54690427037697E+02 0.20508910139136E+02 0.34033165987272E+03 0.45384616533255E+03
 0.29973882135965E+03 0.30055175747148E+03 0.29515001664590E+03 0.29515006409120E+03 0.29973458330756E+03
 0.30049116295551E+03 0.29515001666645E+03 0.29515006389261E+03 0.29973882135965E+03 0.30055175747148E+03
 0.29515001664590E+03 0.29515006409120E+03 0.29973458330757E+03 0.30049116295551E+03 0.29515001666645E+03
 0.29515006389261E+03 0.30043116980710E+03 0.29515024520211E+03 -.24019829677220E+03 -.36139992534604E+03
 0.13693030129719E+03 0.23835469399508E+03 0.10073974119141E+03 0.14756633514956E+03 0.95968056847693E+02
 0.14756633514956E+03 0.16798223610094E+03 0.14757194211318E+03 0.93896842068425E+02 0.14757194211318E+03
 0.16627350309429E+03 0.14756633514957E+03 0.95968056847693E+02 0.14756633514957E+03 0.16798223610094E+03
 0.14757194211317E+03 0.93896842068421E+02 0.14757194211317E+03 0.16627350309429E+03 0.20656028913618E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32476138393354E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93712930395989E+00 0.92493536211016E+00 0.00000000000000E+00 0.93712930395989E+00 0.92493536211016E+00
 0.13888417781910E+01 0.35884180680125E+00 0.10299999713898E+01 0.15449999570847E+01 0.12628262289234E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.42611313221563E-03 0.29718437823445E-05
 0.14293227832124E-01 0.99685358303007E-04 0.12628262289234E-01 0.14821998166425E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1120.57585227
 0.71790645641842E+00 0.30975690717160E+03 0.50497152429718E+03 0.45825624628994E+03 0.43879557530569E+03
 0.22984330268499E+00 0.00000000000000E+00 0.19411663799851E+00 0.00000000000000E+00 -.18725793775111E+01
 0.31738342731125E-02 0.44940161860819E+00 0.25206105018693E+04 0.94522893820098E+03 0.17801449012970E+02
 0.66755433798638E+01 0.36966837228643E+03 0.29516411876965E+03 0.36341555641361E+03 0.40597935256119E+03
 0.29515300044981E+03 0.29515525160846E+03 0.35528805230007E+03 0.40582602878418E+03 0.29515247633259E+03
 0.29515523917319E+03 0.36341555641361E+03 0.40597935256119E+03 0.29515300044981E+03 0.29515525160846E+03
 0.35528805230007E+03 0.40582602878418E+03 0.29515247633259E+03 0.29515523917319E+03 0.45421536886090E+03
 0.34071589089477E+03 0.26413333850221E+04 0.22421462861092E+04 0.59840653774274E+03 0.10630246414011E+04
 0.46162607096963E+03 0.16869453644561E+04 0.14212194923600E+04 0.14047415903855E+04 0.21803986014024E+04
 0.14930386620055E+04 0.14172945205563E+04 0.12646544689973E+04 0.21779045969702E+04 0.16869453644561E+04
 0.14212194923600E+04 0.14047415903855E+04 0.21803986014024E+04 0.14930386620056E+04 0.14172945205563E+04
 0.12646544689973E+04 0.21779045969702E+04 0.19816094681834E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53063059923966E+03 0.18908939659488E+01
 0.18908939659488E+01 0.86053425300635E+01 0.62105120240208E-03 0.31611534779859E+03 0.31548204814022E+03
 0.31548231034790E+03 0.31548230982281E+03 0.21962172067846E+00 0.00000000000000E+00 0.21962169169482E+00
 0.00000000000000E+00 0.95587035938318E+01 0.15505406493375E+00 0.15514774425243E+00 0.51594906611564E+02
 0.19348089979337E+02 0.51563753237585E+02 0.19336407464094E+02 0.34072091703835E+03 0.45421001207099E+03
 0.29981093568465E+03 0.30062642636289E+03 0.29515001819623E+03 0.29515006898890E+03 0.29980675941090E+03
 0.30056537646906E+03 0.29515001821588E+03 0.29515006877150E+03 0.29981093568465E+03 0.30062642636289E+03
 0.29515001819623E+03 0.29515006898890E+03 0.29980675941090E+03 0.30056537646906E+03 0.29515001821588E+03
 0.29515006877150E+03 0.30050046462678E+03 0.29515026238719E+03 -.24184975115524E+03 -.36440086991712E+03
 0.13755108065432E+03 0.23979591994646E+03 0.10155708388887E+03 0.14941230954429E+03 0.96734400585880E+02
 0.14941230954429E+03 0.16930897295967E+03 0.14941901500896E+03 0.94677630275766E+02 0.14941901500896E+03
 0.16761790051640E+03 0.14941230954429E+03 0.96734400585880E+02 0.14941230954429E+03 0.16930897295967E+03
 0.14941901500895E+03 0.94677630275762E+02 0.14941901500895E+03 0.16761790051640E+03 0.20682188184963E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32493759732996E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93728830415875E+00 0.92511661335932E+00 0.00000000000000E+00 0.93728830415875E+00 0.92511661335932E+00
 0.13889531995990E+01 0.35895322820921E+00 0.10299999713898E+01 0.15449999570847E+01 0.12623330187974E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.43186063138184E-03 0.26934300024286E-05
 0.14283520348895E-01 0.89083513180897E-04 0.12623330187974E-01 0.14807157923461E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1130.55985491
 0.71812761316002E+00 0.30984640515302E+03 0.50518907934375E+03 0.45842875655545E+03 0.43894578506938E+03
 0.22984231369373E+00 0.00000000000000E+00 0.19412224520364E+00 0.00000000000000E+00 -.18745670938590E+01
 0.32377926029875E-02 0.44918006868031E+00 0.24708191601335E+04 0.92655718505006E+03 0.17810229255060E+02
 0.66788359706476E+01 0.37000776318659E+03 0.29516499337208E+03 0.36373147187638E+03 0.40641168640735E+03
 0.29515321340056E+03 0.29515562417633E+03 0.35558721840928E+03 0.40625816177853E+03 0.29515265321694E+03
 0.29515561091796E+03 0.36373147187638E+03 0.40641168640735E+03 0.29515321340056E+03 0.29515562417633E+03
 0.35558721840928E+03 0.40625816177853E+03 0.29515265321694E+03 0.29515561091796E+03 0.45457567939058E+03
 0.34109601666838E+03 0.26424079695083E+04 0.22411308095981E+04 0.59655617626344E+03 0.10595027814902E+04
 0.45996382434547E+03 0.16879484473569E+04 0.14205867585112E+04 0.14042643971545E+04 0.21775848408864E+04
 0.14944133456272E+04 0.14166744573671E+04 0.12647149943173E+04 0.21751041958912E+04 0.16879484473569E+04
 0.14205867585112E+04 0.14042643971545E+04 0.21775848408864E+04 0.14944133456272E+04 0.14166744573671E+04
 0.12647149943173E+04 0.21751041958912E+04 0.19791644299086E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53082394672041E+03 0.18908939852690E+01
 0.18908939852690E+01 0.86852145512113E+01 0.55579147683016E-03 0.31629116851532E+03 0.31565305289954E+03
 0.31565328933902E+03 0.31565328886218E+03 0.21954182984795E+00 0.00000000000000E+00 0.21954180373614E+00
 0.00000000000000E+00 0.95647184951239E+01 0.14539993610660E+00 0.15357251155701E+00 0.55020656915111E+02
 0.20632746343166E+02 0.52092655898451E+02 0.19534745961919E+02 0.34110100887154E+03 0.45457034426018E+03
 0.29988210771728E+03 0.30070151384899E+03 0.29515001987334E+03 0.29515007419361E+03 0.29987799157795E+03
 0.30063992285983E+03 0.29515001989181E+03 0.29515007395584E+03 0.29988210771728E+03 0.30070151384899E+03
 0.29515001987334E+03 0.29515007419361E+03 0.29987799157795E+03 0.30063992285983E+03 0.29515001989181E+03
 0.29515007395584E+03 0.30057106241340E+03 0.29515028054612E+03 -.24443219676223E+03 -.36828033232752E+03
 0.13894216073587E+03 0.24199688524497E+03 0.10236001370542E+03 0.15068874228596E+03 0.97619482887287E+02
 0.15068874228596E+03 0.17074937092957E+03 0.15069650947234E+03 0.95537520472933E+02 0.15069650947234E+03
 0.16903683616543E+03 0.15068874228596E+03 0.97619482887287E+02 0.15068874228596E+03 0.17074937092957E+03
 0.15069650947233E+03 0.95537520472929E+02 0.15069650947233E+03 0.16903683616542E+03 0.20709126392411E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32511314085257E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93744421405830E+00 0.92529987045025E+00 0.00000000000000E+00 0.93744421405830E+00 0.92529987045025E+00
 0.13890637779698E+01 0.35906380658001E+00 0.10299999713898E+01 0.15449999570847E+01 0.12618437497363E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.43753584883720E-03 0.24404446229085E-05
 0.14272716129571E-01 0.79608958729397E-04 0.12618437497363E-01 0.14792301381761E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1140.95663044
 0.71835322588834E+00 0.30993737374673E+03 0.50541423630761E+03 0.45860709137190E+03 0.43910068088483E+03
 0.22984129787857E+00 0.00000000000000E+00 0.19412800370093E+00 0.00000000000000E+00 -.18766080603959E+01
 0.32785554227193E-02 0.44894577457213E+00 0.24400990584336E+04 0.91503714691262E+03 0.17819523989561E+02
 0.66823214960852E+01 0.37035937111447E+03 0.29516594760237E+03 0.36405885607482E+03 0.40685859627400E+03
 0.29515344776252E+03 0.29515603414170E+03 0.35589739627764E+03 0.40670486723354E+03 0.29515284797558E+03
 0.29515601998169E+03 0.36405885607482E+03 0.40685859627400E+03 0.29515344776252E+03 0.29515603414170E+03
 0.35589739627764E+03 0.40670486723354E+03 0.29515284797558E+03 0.29515601998169E+03 0.45494722961109E+03
 0.34148636240462E+03 0.26435918016880E+04 0.22401307863721E+04 0.59466298568417E+03 0.10558959428713E+04
 0.45825964225872E+03 0.16890391027672E+04 0.14199281980636E+04 0.14038066582480E+04 0.21746757593581E+04
 0.14958822785167E+04 0.14160287943941E+04 0.12648073945547E+04 0.21722086904324E+04 0.16890391027672E+04
 0.14199281980636E+04 0.14038066582480E+04 0.21746757593581E+04 0.14958822785167E+04 0.14160287943941E+04
 0.12648073945547E+04 0.21722086904324E+04 0.19766622455832E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53102384209449E+03 0.18908940051067E+01
 0.18908940051067E+01 0.87683887554552E+01 0.49511392073365E-03 0.31647342045509E+03 0.31583038925251E+03
 0.31583060150164E+03 0.31583060107052E+03 0.21945890522875E+00 0.00000000000000E+00 0.21945888251067E+00
 0.00000000000000E+00 0.95708561167356E+01 0.16480200664903E+00 0.15761507782062E+00 0.48543098246595E+02
 0.18203661842473E+02 0.50756565365561E+02 0.19033712012085E+02 0.34149135390547E+03 0.45494191715288E+03
 0.29995609247910E+03 0.30077992104430E+03 0.29515002176280E+03 0.29515007995302E+03 0.29995203717483E+03
 0.30071774562818E+03 0.29515002177972E+03 0.29515007969230E+03 0.29995609247909E+03 0.30077992104430E+03
 0.29515002176280E+03 0.29515007995302E+03 0.29995203717483E+03 0.30071774562818E+03 0.29515002177972E+03
 0.29515007969230E+03 0.30064502841569E+03 0.29515030052703E+03 -.24653922298585E+03 -.37171031988410E+03
 0.13992720165164E+03 0.24381443914431E+03 0.10318760148441E+03 0.15230205032539E+03 0.98452559093657E+02
 0.15230205032539E+03 0.17215600468720E+03 0.15231078398041E+03 0.96367260951957E+02 0.15231078398041E+03
 0.17044414605682E+03 0.15230205032539E+03 0.98452559093657E+02 0.15230205032539E+03 0.17215600468720E+03
 0.15231078398041E+03 0.96367260951953E+02 0.15231078398041E+03 0.17044414605681E+03 0.20744469150261E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32529532723489E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93760110071219E+00 0.92549360774226E+00 0.00000000000000E+00 0.93760110071219E+00 0.92549360774226E+00
 0.13891765843339E+01 0.35917661294417E+00 0.10299999713898E+01 0.15449999570847E+01 0.12613908333283E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.44346797812922E-03 0.22021133554643E-05
 0.14258838616605E-01 0.70804613860725E-04 0.12613908333283E-01 0.14775313321951E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1150.55277731
 0.71855949109487E+00 0.31002036133844E+03 0.50562041212588E+03 0.45877032114189E+03 0.43924236889675E+03
 0.22984037126462E+00 0.00000000000000E+00 0.19413324347253E+00 0.00000000000000E+00 -.18784511082170E+01
 0.33394253260667E-02 0.44872895675460E+00 0.23956217668813E+04 0.89835816258047E+03 0.17828134065293E+02
 0.66855502744848E+01 0.37068206433608E+03 0.29516686946098E+03 0.36435936460064E+03 0.40726831830288E+03
 0.29515367610729E+03 0.29515643351779E+03 0.35618223217777E+03 0.40711440511029E+03 0.29515303781970E+03
 0.29515641848336E+03 0.36435936460064E+03 0.40726831830288E+03 0.29515367610729E+03 0.29515643351779E+03
 0.35618223217777E+03 0.40711440511029E+03 0.29515303781970E+03 0.29515641848336E+03 0.45528726991270E+03
 0.34184744087181E+03 0.26445937780056E+04 0.22391220451963E+04 0.59291738791206E+03 0.10525847450909E+04
 0.45670277023930E+03 0.16899814768665E+04 0.14193024998370E+04 0.14033227962854E+04 0.21719867026120E+04
 0.14971782048858E+04 0.14154151531281E+04 0.12648342295922E+04 0.21695322790518E+04 0.16899814768665E+04
 0.14193024998370E+04 0.14033227962854E+04 0.21719867026120E+04 0.14971782048858E+04 0.14154151531281E+04
 0.12648342295922E+04 0.21695322790518E+04 0.19743265505382E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53120683158839E+03 0.18908940230207E+01
 0.18908940230207E+01 0.88451579304150E+01 0.44500310623885E-03 0.31664096833118E+03 0.31599337989794E+03
 0.31599357201719E+03 0.31599357162438E+03 0.21938260987550E+00 0.00000000000000E+00 0.21938258940478E+00
 0.00000000000000E+00 0.95765266024816E+01 0.15613568343960E+00 0.16219832617339E+00 0.51237486676738E+02
 0.19214057503777E+02 0.49322333890474E+02 0.18495875208928E+02 0.34185238899072E+03 0.45528198393194E+03
 0.30002555985058E+03 0.30085209448785E+03 0.29515002364694E+03 0.29515008559538E+03 0.30002155890786E+03
 0.30078945385880E+03 0.29515002366210E+03 0.29515008531177E+03 0.30002555985058E+03 0.30085209448785E+03
 0.29515002364694E+03 0.29515008559538E+03 0.30002155890786E+03 0.30078945385880E+03 0.29515002366210E+03
 0.29515008531177E+03 0.30071236045473E+03 0.29515031999379E+03 -.24841846793248E+03 -.37482244575924E+03
 0.14077576831810E+03 0.24543394093766E+03 0.10395429377797E+03 0.15383754882375E+03 0.99211327981780E+02
 0.15383754882375E+03 0.17344181556107E+03 0.15384708469128E+03 0.97125919986119E+02 0.15384708469128E+03
 0.17173310659363E+03 0.15383754882375E+03 0.99211327981780E+02 0.15383754882375E+03 0.17344181556107E+03
 0.15384708469128E+03 0.97125919986116E+02 0.15384708469128E+03 0.17173310659362E+03 0.20772574813073E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32546256760702E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93774922046518E+00 0.92566281748599E+00 0.00000000000000E+00 0.93774922046518E+00 0.92566281748599E+00
 0.13892797169372E+01 0.35927974554743E+00 0.10299999713898E+01 0.15449999570847E+01 0.12609445413473E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.44895722492533E-03 0.20026996570122E-05
 0.14246076306546E-01 0.63548620111047E-04 0.12609445413473E-01 0.14760584851240E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1160.14892418
 0.71876371392933E+00 0.31010274545048E+03 0.50582523656856E+03 0.45893249503005E+03 0.43938313030520E+03
 0.22983945374956E+00 0.00000000000000E+00 0.19413841830267E+00 0.00000000000000E+00 -.18802921363947E+01
 0.33803047282203E-02 0.44851174590687E+00 0.23666505369212E+04 0.88749395134546E+03 0.17836768095837E+02
 0.66887880359391E+01 0.37100308584095E+03 0.29516783172392E+03 0.36465838283291E+03 0.40767527068036E+03
 0.29515391638000E+03 0.29515685368920E+03 0.35646578368127E+03 0.40752117770221E+03 0.29515323766631E+03
 0.29515683773867E+03 0.36465838283291E+03 0.40767527068036E+03 0.29515391638000E+03 0.29515685368920E+03
 0.35646578368127E+03 0.40752117770221E+03 0.29515323766631E+03 0.29515683773867E+03 0.45562426853609E+03
 0.34220090826658E+03 0.26456442395352E+04 0.22381689558486E+04 0.59119826732844E+03 0.10493207070438E+04
 0.45516644837873E+03 0.16909588316186E+04 0.14186756605700E+04 0.14028784333834E+04 0.21693127556862E+04
 0.14985020851708E+04 0.14148001538594E+04 0.12648919766562E+04 0.21668707249639E+04 0.16909588316186E+04
 0.14186756605700E+04 0.14028784333834E+04 0.21693127556862E+04 0.14985020851708E+04 0.14148001538594E+04
 0.12648919766562E+04 0.21668707249639E+04 0.19720280959333E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53138861436082E+03 0.18908940409151E+01
 0.18908940409151E+01 0.89219271053748E+01 0.39996419315713E-03 0.31680770965954E+03 0.31615570794401E+03
 0.31615588179557E+03 0.31615588143787E+03 0.21930656065102E+00 0.00000000000000E+00 0.21930652844758E+00
 0.00000000000000E+00 0.95820976723693E+01 0.17363529334530E+00 0.16398961593054E+00 0.46073582425958E+02
 0.17277593409734E+02 0.48783576658833E+02 0.18293841247062E+02 0.34220583449200E+03 0.45561900903294E+03
 0.30009421853369E+03 0.30092460002323E+03 0.29515002567382E+03 0.29515009156389E+03 0.30009027052701E+03
 0.30086142512617E+03 0.29515002568686E+03 0.29515009125564E+03 0.30009421853369E+03 0.30092460002323E+03
 0.29515002567382E+03 0.29515009156389E+03 0.30009027052701E+03 0.30086142512617E+03 0.29515002568686E+03
 0.29515009125564E+03 0.30078074449785E+03 0.29515034047879E+03 -.25050907266782E+03 -.37810335971267E+03
 0.14181210697543E+03 0.24722898058633E+03 0.10470781307602E+03 0.15520628503317E+03 0.99991808752747E+02
 0.15520628503317E+03 0.17474416007836E+03 0.15521656085452E+03 0.97896264758123E+02 0.15521656085452E+03
 0.17302899010608E+03 0.15520628503317E+03 0.99991808752747E+02 0.15520628503317E+03 0.17474416007836E+03
 0.15521656085451E+03 0.97896264758120E+02 0.15521656085451E+03 0.17302899010608E+03 0.20806068955751E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32562922174677E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93789314006111E+00 0.92583530902757E+00 0.00000000000000E+00 0.93789314006111E+00 0.92583530902757E+00
 0.13893818283544E+01 0.35938185696466E+00 0.10299999713898E+01 0.15449999570847E+01 0.12605382781573E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.45437710705334E-03 0.18208884687999E-05
 0.14231575723058E-01 0.57032169369233E-04 0.12605382781573E-01 0.14744805887949E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1170.87855371
 0.71898982022740E+00 0.31019421037817E+03 0.50605274627631E+03 0.45911264843784E+03 0.43953949096838E+03
 0.22983843860292E+00 0.00000000000000E+00 0.19414412850087E+00 0.00000000000000E+00 -.18823243047046E+01
 0.34449932898637E-02 0.44826878570302E+00 0.23222106189695E+04 0.87082898211354E+03 0.17846435565335E+02
 0.66924133370006E+01 0.37135996552691E+03 0.29516895644153E+03 0.36499086948513E+03 0.40812699779080E+03
 0.29515419956315E+03 0.29515734881308E+03 0.35678123938967E+03 0.40797270948695E+03 0.29515347330890E+03
 0.29515733178775E+03 0.36499086948513E+03 0.40812699779080E+03 0.29515419956315E+03 0.29515734881308E+03
 0.35678123938967E+03 0.40797270948695E+03 0.29515347330890E+03 0.29515733178775E+03 0.45599737634671E+03
 0.34259689510437E+03 0.26467431403936E+04 0.22370374900782E+04 0.58929567013897E+03 0.10457170305737E+04
 0.45347488208408E+03 0.16919975242268E+04 0.14179646373414E+04 0.14023339134191E+04 0.21663329375661E+04
 0.14999309234157E+04 0.14141024895337E+04 0.12649095394196E+04 0.21639048373027E+04 0.16919975242268E+04
 0.14179646373414E+04 0.14023339134191E+04 0.21663329375661E+04 0.14999309234157E+04 0.14141024895337E+04
 0.12649095394196E+04 0.21639048373027E+04 0.19694517235010E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53159052456093E+03 0.18908940606673E+01
 0.18908940606673E+01 0.90077641416024E+01 0.35498392998967E-03 0.31699333686834E+03 0.31633638274324E+03
 0.31633653821534E+03 0.31633653789321E+03 0.21922177251775E+00 0.00000000000000E+00 0.21922175648799E+00
 0.00000000000000E+00 0.95883271657462E+01 0.17283456207171E+00 0.17291939250928E+00 0.46287038333692E+02
 0.17357639375135E+02 0.46264330934258E+02 0.17349124100347E+02 0.34260178767096E+03 0.45599214618777E+03
 0.30017246359120E+03 0.30100550510369E+03 0.29515002811858E+03 0.29515009863775E+03 0.30016857296247E+03
 0.30094182375702E+03 0.29515002812883E+03 0.29515009829977E+03 0.30017246359120E+03 0.30100550510369E+03
 0.29515002811858E+03 0.29515009863775E+03 0.30016857296247E+03 0.30094182375702E+03 0.29515002812883E+03
 0.29515009829977E+03 0.30085612773438E+03 0.29515036462696E+03 -.25219568541450E+03 -.38113385507085E+03
 0.14242713549443E+03 0.24869258763908E+03 0.10555331646718E+03 0.15710375094915E+03 0.10076708231099E+03
 0.15710375094915E+03 0.17609964016991E+03 0.15711474168395E+03 0.98687244815951E+02 0.15711474168395E+03
 0.17440373999234E+03 0.15710375094915E+03 0.10076708231099E+03 0.15710375094915E+03 0.17609964016991E+03
 0.15711474168394E+03 0.98687244815948E+02 0.15711474168394E+03 0.17440373999233E+03 0.20839179672465E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32581453230605E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93805614000269E+00 0.92602170777884E+00 0.00000000000000E+00 0.93805614000269E+00 0.92602170777884E+00
 0.13894948815035E+01 0.35949491011370E+00 0.10299999713898E+01 0.15449999570847E+01 0.12600593180646E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.46041369790719E-03 0.16368178716589E-05
 0.14215457002911E-01 0.50537406210823E-04 0.12600593180646E-01 0.14728044924901E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1180.84577617
 0.71919941627007E+00 0.31027970429213E+03 0.50626247352257E+03 0.45927890911273E+03 0.43968407709393E+03
 0.22983750186789E+00 0.00000000000000E+00 0.19414937235442E+00 0.00000000000000E+00 -.18842197010895E+01
 0.35064692325782E-02 0.44804609082334E+00 0.22814972752856E+04 0.85556147823209E+03 0.17855305880025E+02
 0.66957397050093E+01 0.37168951309751E+03 0.29517004941383E+03 0.36529794279833E+03 0.40854370738687E+03
 0.29515447709462E+03 0.29515783396491E+03 0.35707272027504E+03 0.40838924291708E+03 0.29515370435423E+03
 0.29515781589109E+03 0.36529794279833E+03 0.40854370738687E+03 0.29515447709462E+03 0.29515783396491E+03
 0.35707272027504E+03 0.40838924291708E+03 0.29515370435423E+03 0.29515781589109E+03 0.45634087863301E+03
 0.34295861541505E+03 0.26477446266224E+04 0.22359876292814E+04 0.58754558758845E+03 0.10424055679719E+04
 0.45192225244547E+03 0.16929490976025E+04 0.14172967216931E+04 0.14018295760825E+04 0.21635733892044E+04
 0.15012432153286E+04 0.14134469737105E+04 0.12649233827966E+04 0.21611581762740E+04 0.16929490976026E+04
 0.14172967216931E+04 0.14018295760825E+04 0.21635733892044E+04 0.15012432153286E+04 0.14134469737105E+04
 0.12649233827966E+04 0.21611581762740E+04 0.19670715418460E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53177680194579E+03 0.18908940790901E+01
 0.18908940790901E+01 0.90875019212392E+01 0.31774310256205E-03 0.31716493762792E+03 0.31650349919184E+03
 0.31650363930351E+03 0.31650363901137E+03 0.21914328087897E+00 0.00000000000000E+00 0.21914326668920E+00
 0.00000000000000E+00 0.95940390257920E+01 0.17407106399127E+00 0.17247929096352E+00 0.45958241516815E+02
 0.17234340568806E+02 0.46382379909552E+02 0.17393392466082E+02 0.34296346519912E+03 0.45633567994543E+03
 0.30024478845875E+03 0.30108086161767E+03 0.29515003057216E+03 0.29515010561082E+03 0.30024094954705E+03
 0.30101667089582E+03 0.29515003057934E+03 0.29515010524300E+03 0.30024478845875E+03 0.30108086161767E+03
 0.29515003057216E+03 0.29515010561082E+03 0.30024094954705E+03 0.30101667089582E+03 0.29515003057934E+03
 0.29515010524300E+03 0.30092676474567E+03 0.29515038830107E+03 -.25456105889307E+03 -.38471478710725E+03
 0.14366898880120E+03 0.25071604755084E+03 0.10632871380564E+03 0.15838812968663E+03 0.10159871494615E+03
 0.15838812968663E+03 0.17746548471256E+03 0.15839974813423E+03 0.99499496037995E+02 0.15839974813423E+03
 0.17575374813371E+03 0.15838812968663E+03 0.10159871494615E+03 0.15838812968663E+03 0.17746548471256E+03
 0.15839974813423E+03 0.99499496037993E+02 0.15839974813423E+03 0.17575374813371E+03 0.20873370997908E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32598596033410E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93820641788407E+00 0.92619467296547E+00 0.00000000000000E+00 0.93820641788407E+00 0.92619467296547E+00
 0.13895996795248E+01 0.35959970813504E+00 0.10299999713898E+01 0.15449999570847E+01 0.12596383719760E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.46587950318967E-03 0.14819215677768E-05
 0.14199374579837E-01 0.45166956894939E-04 0.12596383719760E-01 0.14711902961490E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1190.81299862
 0.71940637230102E+00 0.31036428689935E+03 0.50647093284098E+03 0.45944414259398E+03 0.43982768445579E+03
 0.22983657557250E+00 0.00000000000000E+00 0.19415456102742E+00 0.00000000000000E+00 -.18861029728518E+01
 0.35565043515797E-02 0.44782358850888E+00 0.22493997501920E+04 0.84352490632198E+03 0.17864177335181E+02
 0.66990665006930E+01 0.37201737342940E+03 0.29517118987090E+03 0.36560351896651E+03 0.40895759066283E+03
 0.29515476901530E+03 0.29515834417559E+03 0.35736290965253E+03 0.40880295447423E+03 0.29515394748434E+03
 0.29515832500375E+03 0.36560351896651E+03 0.40895759066283E+03 0.29515476901530E+03 0.29515834417559E+03
 0.35736290965253E+03 0.40880295447423E+03 0.29515394748434E+03 0.29515832500375E+03 0.45668134798628E+03
 0.34331704787112E+03 0.26487702391269E+04 0.22349662328290E+04 0.58582126414718E+03 0.10391411886404E+04
 0.45039081817253E+03 0.16939177080996E+04 0.14166285792952E+04 0.14013445537284E+04 0.21608310035367E+04
 0.15025677525609E+04 0.14127910974319E+04 0.12649503053274E+04 0.21584285083261E+04 0.16939177080996E+04
 0.14166285792952E+04 0.14013445537284E+04 0.21608310035367E+04 0.15025677525609E+04 0.14127910974319E+04
 0.12649503053274E+04 0.21584285083261E+04 0.19647203444040E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53196190660313E+03 0.18908940973951E+01
 0.18908940973951E+01 0.91672397008759E+01 0.28441007481295E-03 0.31733567805710E+03 0.31666981514121E+03
 0.31666994139329E+03 0.31666994112843E+03 0.21906503583249E+00 0.00000000000000E+00 0.21906502307093E+00
 0.00000000000000E+00 0.95996996905352E+01 0.18454655788057E+00 0.17748353339762E+00 0.43349494522554E+02
 0.16256060445958E+02 0.45074604087792E+02 0.16902976532922E+02 0.34332188365944E+03 0.45667618098334E+03
 0.30031732748889E+03 0.30115626579368E+03 0.29515003321216E+03 0.29515011298675E+03 0.30031353880965E+03
 0.30109157357978E+03 0.29515003321580E+03 0.29515011258682E+03 0.30031732748889E+03 0.30115626579368E+03
 0.29515003321216E+03 0.29515011298675E+03 0.30031353880965E+03 0.30109157357978E+03 0.29515003321580E+03
 0.29515011258682E+03 0.30099739018969E+03 0.29515041321368E+03 -.25638129021188E+03 -.38773682377619E+03
 0.14446320469890E+03 0.25228399536901E+03 0.10709847464662E+03 0.15995241415871E+03 0.10234399076671E+03
 0.15995241415871E+03 0.17874055180297E+03 0.15996457599461E+03 0.10024734952635E+03 0.15996457599461E+03
 0.17703489072786E+03 0.15995241415872E+03 0.10234399076671E+03 0.15995241415872E+03 0.17874055180297E+03
 0.15996457599461E+03 0.10024734952635E+03 0.15996457599461E+03 0.17703489072786E+03 0.20907777772318E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32615654446615E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93835392672078E+00 0.92636862720849E+00 0.00000000000000E+00 0.93835392672078E+00 0.92636862720849E+00
 0.13897031575403E+01 0.35970318615051E+00 0.10299999713898E+01 0.15449999570847E+01 0.12592251069682E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.47131927854273E-03 0.13414821055280E-05
 0.14182661784469E-01 0.40367088423470E-04 0.12592251069682E-01 0.14695689633541E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1200.78022108
 0.71961178093469E+00 0.31044849063084E+03 0.50667802082403E+03 0.45960832692593E+03 0.43997042278533E+03
 0.22983565647253E+00 0.00000000000000E+00 0.19415969435379E+00 0.00000000000000E+00 -.18879718760284E+01
 0.36148426642327E-02 0.44760244098342E+00 0.22130977038521E+04 0.82991163894453E+03 0.17873003512723E+02
 0.67023763172710E+01 0.37234342785877E+03 0.29517237911633E+03 0.36590746913346E+03 0.40936867406122E+03
 0.29515507584496E+03 0.29515888034055E+03 0.35765169132697E+03 0.40921387106183E+03 0.29515420314195E+03
 0.29515886001963E+03 0.36590746913346E+03 0.40936867406122E+03 0.29515507584496E+03 0.29515888034055E+03
 0.35765169132697E+03 0.40921387106183E+03 0.29515420314195E+03 0.29515886001963E+03 0.45701878460141E+03
 0.34367321859343E+03 0.26497564244700E+04 0.22339159263650E+04 0.58411444970234E+03 0.10359145985535E+04
 0.44887957660269E+03 0.16948581438953E+04 0.14159536142256E+04 0.14008384722546E+04 0.21580982150049E+04
 0.15038643344852E+04 0.14121284405843E+04 0.12649546058537E+04 0.21557084395443E+04 0.16948581438953E+04
 0.14159536142256E+04 0.14008384722546E+04 0.21580982150049E+04 0.15038643344853E+04 0.14121284405843E+04
 0.12649546058537E+04 0.21557084395443E+04 0.19623729888086E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53214581332951E+03 0.18908941155604E+01
 0.18908941155604E+01 0.92469774805126E+01 0.25457450488275E-03 0.31750562089195E+03 0.31683537067722E+03
 0.31683548442963E+03 0.31683548418954E+03 0.21898703690910E+00 0.00000000000000E+00 0.21898702562592E+00
 0.00000000000000E+00 0.96053336360184E+01 0.18234503636886E+00 0.18216525432780E+00 0.43872869584544E+02
 0.16452326094204E+02 0.43916168478564E+02 0.16468563179462E+02 0.34367802039347E+03 0.45701365035703E+03
 0.30039039603265E+03 0.30123165699571E+03 0.29515003605034E+03 0.29515012078344E+03 0.30038665613379E+03
 0.30116649014339E+03 0.29515003604992E+03 0.29515012034897E+03 0.30039039603265E+03 0.30123165699571E+03
 0.29515003605034E+03 0.29515012078344E+03 0.30038665613379E+03 0.30116649014339E+03 0.29515003604992E+03
 0.29515012034897E+03 0.30106774419306E+03 0.29515043941340E+03 -.25822207407465E+03 -.39077164989347E+03
 0.14527523116434E+03 0.25386660924431E+03 0.10786500192415E+03 0.16149281172891E+03 0.10308829257350E+03
 0.16149281172891E+03 0.18001075589826E+03 0.16150546040233E+03 0.10099303780898E+03 0.16150546040233E+03
 0.17830981725121E+03 0.16149281172891E+03 0.10308829257350E+03 0.16149281172891E+03 0.18001075589826E+03
 0.16150546040233E+03 0.10099303780898E+03 0.16150546040233E+03 0.17830981725121E+03 0.20940996435592E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32632626952855E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93850165787987E+00 0.92653938309587E+00 0.00000000000000E+00 0.93850165787987E+00 0.92653938309587E+00
 0.13898058618571E+01 0.35980589046734E+00 0.10299999713898E+01 0.15449999570847E+01 0.12588064148822E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.47671111289871E-03 0.12141139179034E-05
 0.14165776732252E-01 0.36078174439778E-04 0.12588064148822E-01 0.14679780133509E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1210.74744353
 0.71981583760363E+00 0.31053250627333E+03 0.50688375328948E+03 0.45977150751100E+03 0.44011237798174E+03
 0.22983474508393E+00 0.00000000000000E+00 0.19416477696733E+00 0.00000000000000E+00 -.18898399407813E+01
 0.36673744832420E-02 0.44738307566154E+00 0.21813970829965E+04 0.81802390612370E+03 0.17881767181672E+02
 0.67056626931270E+01 0.37266772337032E+03 0.29517361846208E+03 0.36620984075617E+03 0.40977699645983E+03
 0.29515539811212E+03 0.29515944336958E+03 0.35793910660205E+03 0.40962203145719E+03 0.29515447177755E+03
 0.29515942184695E+03 0.36620984075617E+03 0.40977699645983E+03 0.29515539811212E+03 0.29515944336958E+03
 0.35793910660205E+03 0.40962203145719E+03 0.29515447177755E+03 0.29515942184695E+03 0.45735324142974E+03
 0.34402398531283E+03 0.26507483203407E+04 0.22328833138999E+04 0.58243051331184E+03 0.10327307760288E+04
 0.44738811015042E+03 0.16958027948426E+04 0.14152760207973E+04 0.14003448344426E+04 0.21553790084328E+04
 0.15051616714928E+04 0.14114630770256E+04 0.12649660839187E+04 0.21530018353379E+04 0.16958027948426E+04
 0.14152760207973E+04 0.14003448344426E+04 0.21553790084328E+04 0.15051616714928E+04 0.14114630770256E+04
 0.12649660839187E+04 0.21530018353379E+04 0.19600482224770E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53232856172147E+03 0.18908941337176E+01
 0.18908941337176E+01 0.93267152601494E+01 0.22786944951925E-03 0.31767468072887E+03 0.31700014560413E+03
 0.31700024807476E+03 0.31700024785721E+03 0.21890928346008E+00 0.00000000000000E+00 0.21890927315761E+00
 0.00000000000000E+00 0.96108812454142E+01 0.17330666633054E+00 0.18224786112461E+00 0.46160947927657E+02
 0.17310355472871E+02 0.43896262763435E+02 0.16461098536288E+02 0.34402875655219E+03 0.45734814079735E+03
 0.30046321933493E+03 0.30130722756864E+03 0.29515003909903E+03 0.29515012901928E+03 0.30045952685856E+03
 0.30124155763232E+03 0.29515003909399E+03 0.29515012854771E+03 0.30046321933493E+03 0.30130722756864E+03
 0.29515003909903E+03 0.29515012901928E+03 0.30045952685856E+03 0.30124155763232E+03 0.29515003909399E+03
 0.29515012854771E+03 0.30113858246200E+03 0.29515046695008E+03 -.26047093336495E+03 -.39418645042661E+03
 0.14643912811697E+03 0.25579332099364E+03 0.10862199723609E+03 0.16277427533307E+03 0.10388986852543E+03
 0.16277427533307E+03 0.18133318010947E+03 0.16278737030886E+03 0.10177814069751E+03 0.16278737030886E+03
 0.17961927623563E+03 0.16277427533307E+03 0.10388986852543E+03 0.16277427533307E+03 0.18133318010947E+03
 0.16278737030886E+03 0.10177814069751E+03 0.16278737030886E+03 0.17961927623562E+03 0.20979120185006E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32649529015242E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93864703075879E+00 0.92671119076633E+00 0.00000000000000E+00 0.93864703075879E+00 0.92671119076633E+00
 0.13899078901916E+01 0.35990791880182E+00 0.10299999713898E+01 0.15449999570847E+01 0.12584229398408E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.48200218645713E-03 0.10985067369903E-05
 0.14147639156694E-01 0.32243166862725E-04 0.12584229398408E-01 0.14662983016751E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1220.30021371
 0.72000893066290E+00 0.31061197512842E+03 0.50707980218743E+03 0.45992693883062E+03 0.44024745296950E+03
 0.22983388167389E+00 0.00000000000000E+00 0.19416959985425E+00 0.00000000000000E+00 -.18916114401565E+01
 0.37099374869710E-02 0.44717256064894E+00 0.21563705663762E+04 0.80863896239108E+03 0.17890185364662E+02
 0.67088195117481E+01 0.37297699520109E+03 0.29517485406941E+03 0.36649827566671E+03 0.41016572644416E+03
 0.29515572182473E+03 0.29516000881155E+03 0.35821340213474E+03 0.41001061036195E+03 0.29515474172941E+03
 0.29515998608680E+03 0.36649827566671E+03 0.41016572644416E+03 0.29515572182473E+03 0.29516000881155E+03
 0.35821340213474E+03 0.41001061036195E+03 0.29515474172941E+03 0.29515998608680E+03 0.45767092427693E+03
 0.34435754066673E+03 0.26517122602760E+04 0.22319087744948E+04 0.58084133506441E+03 0.10297247243600E+04
 0.44597918262023E+03 0.16967174791927E+04 0.14146260800252E+04 0.13998814789410E+04 0.21527886354882E+04
 0.15064106038871E+04 0.14108247508209E+04 0.12649817841342E+04 0.21504234003598E+04 0.16967174791927E+04
 0.14146260800252E+04 0.13998814789410E+04 0.21527886354882E+04 0.15064106038871E+04 0.14108247508209E+04
 0.12649817841342E+04 0.21504234003598E+04 0.19578435096853E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53250263182723E+03 0.18908941509362E+01
 0.18908941509362E+01 0.94031374215799E+01 0.20491013626689E-03 0.31783585355959E+03 0.31715725115309E+03
 0.31715734385477E+03 0.31715734365687E+03 0.21883499035847E+00 0.00000000000000E+00 0.21883498138184E+00
 0.00000000000000E+00 0.96161850631039E+01 0.18756848991105E+00 0.18851939492314E+00 0.42651087097805E+02
 0.15994157661677E+02 0.42435952031682E+02 0.15913482011881E+02 0.34436229824572E+03 0.45766585536066E+03
 0.30053334738434E+03 0.30137968592763E+03 0.29515004222843E+03 0.29515013733836E+03 0.30052969912049E+03
 0.30131355012703E+03 0.29515004221839E+03 0.29515013682869E+03 0.30053334738434E+03 0.30137968592763E+03
 0.29515004222843E+03 0.29515013733836E+03 0.30052969912049E+03 0.30131355012703E+03 0.29515004221839E+03
 0.29515013682869E+03 0.30120635859867E+03 0.29515049463159E+03 -.26200971977400E+03 -.39683284063798E+03
 0.14704067997439E+03 0.25711826016459E+03 0.10934237679033E+03 0.16432480601945E+03 0.10455785882073E+03
 0.16432480601945E+03 0.18249575561231E+03 0.16433826553318E+03 0.10245543700764E+03 0.16433826553318E+03
 0.18079442029214E+03 0.16432480601945E+03 0.10455785882073E+03 0.16432480601945E+03 0.18249575561231E+03
 0.16433826553317E+03 0.10245543700764E+03 0.16433826553317E+03 0.18079442029214E+03 0.21012350559613E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32665633610788E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93878457040506E+00 0.92687518399831E+00 0.00000000000000E+00 0.93878457040506E+00 0.92687518399831E+00
 0.13900044367212E+01 0.36000446533145E+00 0.10299999713898E+01 0.15449999570847E+01 0.12580385978805E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.48708469621147E-03 0.99799993255229E-06
 0.14130502915826E-01 0.28952338405643E-04 0.12580385978805E-01 0.14647537950376E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1230.29391617
 0.72021043959464E+00 0.31069526151922E+03 0.50728347619777E+03 0.46008851469351E+03 0.44038803566675E+03
 0.22983298099776E+00 0.00000000000000E+00 0.19417459667830E+00 0.00000000000000E+00 -.18934496583084E+01
 0.37776651080269E-02 0.44695491580849E+00 0.21177102181454E+04 0.79414133180453E+03 0.17898896996197E+02
 0.67120863735738E+01 0.37329873597395E+03 0.29517619844098E+03 0.36679838291187E+03 0.41056975668766E+03
 0.29515607667525E+03 0.29516062851546E+03 0.35849893133397E+03 0.41041448754797E+03 0.29515503777033E+03
 0.29516060447839E+03 0.36679838291187E+03 0.41056975668766E+03 0.29515607667525E+03 0.29516062851546E+03
 0.35849893133397E+03 0.41041448754798E+03 0.29515503777033E+03 0.29516060447839E+03 0.45800040930849E+03
 0.34470648130615E+03 0.26526335376612E+04 0.22308178441494E+04 0.57918881580665E+03 0.10266077291128E+04
 0.44452296922714E+03 0.16976121912611E+04 0.14139345923899E+04 0.13993457767460E+04 0.21500816961259E+04
 0.15076591090793E+04 0.14101455978810E+04 0.12649493903635E+04 0.21477290931236E+04 0.16976121912611E+04
 0.14139345923899E+04 0.13993457767460E+04 0.21500816961259E+04 0.15076591090793E+04 0.14101455978810E+04
 0.12649493903635E+04 0.21477290931236E+04 0.19555210163328E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53268355716057E+03 0.18908941688032E+01
 0.18908941688032E+01 0.94830870412656E+01 0.18336331543090E-03 0.31800377110568E+03 0.31732090020380E+03
 0.31732098367945E+03 0.31732098350022E+03 0.21875750892151E+00 0.00000000000000E+00 0.21875750093730E+00
 0.00000000000000E+00 0.96217464887358E+01 0.19482140205106E+00 0.19574035525192E+00 0.41063250319404E+02
 0.15398718869776E+02 0.40870468379930E+02 0.15326425642474E+02 0.34471119076249E+03 0.45799537602847E+03
 0.30060775058876E+03 0.30145531854910E+03 0.29515004573421E+03 0.29515014650954E+03 0.30060414727371E+03
 0.30138875928064E+03 0.29515004571831E+03 0.29515014595719E+03 0.30060775058876E+03 0.30145531854910E+03
 0.29515004573421E+03 0.29515014650954E+03 0.30060414727371E+03 0.30138875928064E+03 0.29515004571831E+03
 0.29515014595719E+03 0.30127645673746E+03 0.29515052500268E+03 -.26356468274208E+03 -.39955276946419E+03
 0.14761747230240E+03 0.25845303542085E+03 0.11009747575693E+03 0.16597925368278E+03 0.10524494188668E+03
 0.16597925368278E+03 0.18369746168465E+03 0.16599305027780E+03 0.10315461451965E+03 0.16599305027780E+03
 0.18201124681931E+03 0.16597925368278E+03 0.10524494188668E+03 0.16597925368278E+03 0.18369746168465E+03
 0.16599305027780E+03 0.10315461451964E+03 0.16599305027780E+03 0.18201124681931E+03 0.21042331714859E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32682391157233E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93893137160175E+00 0.92703997375481E+00 0.00000000000000E+00 0.93893137160175E+00 0.92703997375481E+00
 0.13901051911871E+01 0.36010521979732E+00 0.10299999713898E+01 0.15449999570847E+01 0.12576062535500E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.49234854636785E-03 0.90250286279723E-06
 0.14113282415291E-01 0.25870448643000E-04 0.12576062535500E-01 0.14632403913164E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1240.05185252
 0.72040752398084E+00 0.31077763231976E+03 0.50748099265474E+03 0.46024546572886E+03 0.44052498426828E+03
 0.22983210279097E+00 0.00000000000000E+00 0.19417943753200E+00 0.00000000000000E+00 -.18952558804644E+01
 0.38585546712563E-02 0.44674708411431E+00 0.20733151870556E+04 0.77749319514584E+03 0.17907223761427E+02
 0.67152089105351E+01 0.37361109002068E+03 0.29517756340867E+03 0.36708977659950E+03 0.41096167619119E+03
 0.29515643966082E+03 0.29516126229054E+03 0.35877630412583E+03 0.41080626285468E+03 0.29515534072373E+03
 0.29516123691660E+03 0.36708977659950E+03 0.41096167619119E+03 0.29515643966082E+03 0.29516126229054E+03
 0.35877630412583E+03 0.41080626285468E+03 0.29515534072373E+03 0.29516123691660E+03 0.45831933062866E+03
 0.34504544381972E+03 0.26534752355360E+04 0.22297192434160E+04 0.57759115450900E+03 0.10235980698085E+04
 0.44311895952699E+03 0.16984444646856E+04 0.14132531990241E+04 0.13987992900197E+04 0.21474471766443E+04
 0.15088386410206E+04 0.14094763798266E+04 0.12648938222281E+04 0.21451069966340E+04 0.16984444646856E+04
 0.14132531990241E+04 0.13987992900198E+04 0.21474471766443E+04 0.15088386410206E+04 0.14094763798266E+04
 0.12648938222281E+04 0.21451069966340E+04 0.19532540540336E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53285922000481E+03 0.18908941863593E+01
 0.18908941863593E+01 0.95611505320509E+01 0.16451299559606E-03 0.31816705121101E+03 0.31748004095373E+03
 0.31748011630181E+03 0.31748011613913E+03 0.21868209302828E+00 0.00000000000000E+00 0.21868208590700E+00
 0.00000000000000E+00 0.96271578458022E+01 0.20197221653414E+00 0.20293297639898E+00 0.39609408349726E+02
 0.14853528131147E+02 0.39421882741577E+02 0.14783206028091E+02 0.34505010639939E+03 0.45831433258245E+03
 0.30068101664041E+03 0.30152910439265E+03 0.29515004939942E+03 0.29515015594530E+03 0.30067745599626E+03
 0.30146216503678E+03 0.29515004937713E+03 0.29515015534832E+03 0.30068101664041E+03 0.30152910439265E+03
 0.29515004939942E+03 0.29515015594530E+03 0.30067745599626E+03 0.30146216503678E+03 0.29515004937713E+03
 0.29515015534832E+03 0.30134451110383E+03 0.29515055610119E+03 -.26506349073689E+03 -.40218371607665E+03
 0.14816292380542E+03 0.25973707049713E+03 0.11083333207268E+03 0.16759527115036E+03 0.10590843678737E+03
 0.16759527115036E+03 0.18486040680383E+03 0.16760935809819E+03 0.10383047150497E+03 0.16760935809819E+03
 0.18318930564146E+03 0.16759527115037E+03 0.10590843678737E+03 0.16759527115037E+03 0.18486040680383E+03
 0.16760935809818E+03 0.10383047150497E+03 0.16760935809818E+03 0.18318930564145E+03 0.21069849073571E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32698678537647E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93907586812458E+00 0.92719858414277E+00 0.00000000000000E+00 0.93907586812458E+00 0.92719858414277E+00
 0.13902037333802E+01 0.36020376199042E+00 0.10299999713898E+01 0.15449999570847E+01 0.12571754296081E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.49735552784464E-03 0.81779310323411E-06
 0.14096653058134E-01 0.23178883121265E-04 0.12571754296081E-01 0.14618005262203E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1250.08785316
 0.72062777263451E+00 0.31086619926490E+03 0.50768398665125E+03 0.46040653207156E+03 0.44066616751056E+03
 0.22982996276521E+00 0.00000000000000E+00 0.19418436489066E+00 0.00000000000000E+00 -.18965512466997E+01
 0.39638623488182E-02 0.44653759798810E+00 0.20182335550540E+04 0.75683758314524E+03 0.17915624655224E+02
 0.67183592457089E+01 0.37393071590635E+03 0.29517902302572E+03 0.36738799135698E+03 0.41136263906343E+03
 0.29515683072269E+03 0.29516194493516E+03 0.35906028591686E+03 0.41120708521051E+03 0.29515566724610E+03
 0.29516191812690E+03 0.36738799135698E+03 0.41136263906343E+03 0.29515683072269E+03 0.29516194493516E+03
 0.35906028591686E+03 0.41120708521051E+03 0.29515566724610E+03 0.29516191812690E+03 0.45864554706174E+03
 0.34539185042961E+03 0.26543197564927E+04 0.22286182995848E+04 0.57600433269732E+03 0.10205786300449E+04
 0.44169427568405E+03 0.16992816608258E+04 0.14126121638904E+04 0.13982565705558E+04 0.21448190673619E+04
 0.15100361986642E+04 0.14088486881774E+04 0.12648559136470E+04 0.21424924219230E+04 0.16992816608258E+04
 0.14126121638904E+04 0.13982565705558E+04 0.21448190673619E+04 0.15100361986642E+04 0.14088486881774E+04
 0.12648559136470E+04 0.21424924219230E+04 0.19509851289469E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53304000884697E+03 0.18908941989500E+01
 0.18908941989500E+01 0.96414385371629E+01 0.00000000000000E+00 0.31767241687881E+03 0.31767241687881E+03
 0.31767241687881E+03 0.31767241687881E+03 0.00000000000000E+00 0.00000000000000E+00 0.21860534533672E+00
 0.00000000000000E+00 0.96462633372076E+01 0.10000000000000E-02 0.20584245033605E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38864675322994E+02 0.14574253246123E+02 0.34539646901012E+03 0.45864058843584E+03
 0.30160443068367E+03 0.30160443068367E+03 0.29515016856339E+03 0.29515016617092E+03 0.30153784588140E+03
 0.30153784588140E+03 0.29515016790795E+03 0.29515016552478E+03 0.30160443068367E+03 0.30160443068367E+03
 0.29515016856339E+03 0.29515016617092E+03 0.30153784588140E+03 0.30153784588140E+03 0.29515016790795E+03
 0.29515016552478E+03 0.30140566616221E+03 0.29515058964281E+03 -.26205165084379E+03 -.40500679877146E+03
 0.14584094173272E+03 0.25841720333039E+03 0.11184705688901E+03 0.15987397630201E+03 0.10646168052854E+03
 0.15987397630201E+03 0.18606193431639E+03 0.16024288651884E+03 0.10456081758862E+03 0.16024288651884E+03
 0.18456622880497E+03 0.15987397630201E+03 0.10646168052854E+03 0.15987397630201E+03 0.18606193431639E+03
 0.16024288651883E+03 0.10456081758862E+03 0.16024288651883E+03 0.18456622880496E+03 0.20457592727688E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32713967568538E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93938551590600E+00 0.92711354865232E+00 0.00000000000000E+00 0.93938551590600E+00 0.92711354865232E+00
 0.13903138577070E+01 0.36031388631726E+00 0.10299999713898E+01 0.15449999570847E+01 0.12462063841932E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.51533072885581E-03 0.00000000000000E+00
 0.14386669115860E-01 0.00000000000000E+00 0.12462063841932E-01 0.14901999844716E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1261.66422560
 0.72086991481937E+00 0.31096687171326E+03 0.50791608664957E+03 0.46059116538461E+03 0.44082806152319E+03
 0.22982812969144E+00 0.00000000000000E+00 0.19418986262181E+00 0.00000000000000E+00 -.18988387704638E+01
 0.40446077792238E-02 0.44630007670155E+00 0.19779420988839E+04 0.74172828708146E+03 0.17925159366150E+02
 0.67219347623061E+01 0.37429740178638E+03 0.29518078064148E+03 0.36773021491490E+03 0.41182204021851E+03
 0.29515730553566E+03 0.29516277356426E+03 0.35938629304010E+03 0.41166632724324E+03 0.29515606388189E+03
 0.29516274502250E+03 0.36773021491490E+03 0.41182204021851E+03 0.29515730553566E+03 0.29516277356426E+03
 0.35938629304010E+03 0.41166632724324E+03 0.29515606388189E+03 0.29516274502250E+03 0.45901862155753E+03
 0.34578549369425E+03 0.26553480758201E+04 0.22274039953180E+04 0.57415531107660E+03 0.10170924654252E+04
 0.44006637779320E+03 0.17002917295663E+04 0.14118123989489E+04 0.13976744480349E+04 0.21417405491869E+04
 0.15114503240794E+04 0.14080631776404E+04 0.12648432307914E+04 0.21394283557744E+04 0.17002917295663E+04
 0.14118123989489E+04 0.13976744480349E+04 0.21417405491869E+04 0.15114503240794E+04 0.14080631776404E+04
 0.12648432307914E+04 0.21394283557744E+04 0.19483666012727E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53324680319959E+03 0.18908942211842E+01
 0.18908942211842E+01 0.97340495167086E+01 0.00000000000000E+00 0.31789374076783E+03 0.31789374076783E+03
 0.31789374076783E+03 0.31789374076783E+03 0.00000000000000E+00 0.00000000000000E+00 0.21851713495108E+00
 0.00000000000000E+00 0.96503965704752E+01 0.10000000000000E-02 0.20753214068478E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38548245942065E+02 0.14455592228274E+02 0.34579007930284E+03 0.45901371372428E+03
 0.30169309837092E+03 0.30169309837092E+03 0.29515018171468E+03 0.29515017866962E+03 0.30162637615712E+03
 0.30162637615712E+03 0.29515018099530E+03 0.29515017796229E+03 0.30169309837092E+03 0.30169309837092E+03
 0.29515018171468E+03 0.29515017866962E+03 0.30162637615712E+03 0.30162637615712E+03 0.29515018099530E+03
 0.29515017796229E+03 0.30148386544557E+03 0.29515063042467E+03 -.26437163714797E+03 -.40843007257179E+03
 0.14710904888353E+03 0.26086260653202E+03 0.11301801240407E+03 0.16141737884959E+03 0.10740182241188E+03
 0.16141737884959E+03 0.18778869331405E+03 0.16178735034501E+03 0.10548700833336E+03 0.16178735034501E+03
 0.18628059140608E+03 0.16141737884959E+03 0.10740182241188E+03 0.16141737884959E+03 0.18778869331405E+03
 0.16178735034501E+03 0.10548700833336E+03 0.16178735034501E+03 0.18628059140608E+03 0.20739443915712E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32737151261024E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93952272368506E+00 0.92734732894370E+00 0.00000000000000E+00 0.93952272368506E+00 0.92734732894370E+00
 0.13904349287995E+01 0.36043495740968E+00 0.10299999713898E+01 0.15449999570847E+01 0.12472987555006E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.52169501688081E-03 0.00000000000000E+00
 0.14311474659265E-01 0.00000000000000E+00 0.12472987555006E-01 0.14833169676146E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1271.96544720
 0.72107368212505E+00 0.31105327902126E+03 0.50812108289775E+03 0.46075428057458E+03 0.44097062972120E+03
 0.22982700675111E+00 0.00000000000000E+00 0.19419468010610E+00 0.00000000000000E+00 -.19008366878725E+01
 0.40908042566194E-02 0.44608827673363E+00 0.19556056702188E+04 0.73335212633204E+03 0.17933670121479E+02
 0.67251262955547E+01 0.37462207290746E+03 0.29518241221207E+03 0.36803330502647E+03 0.41222807299455E+03
 0.29515774990929E+03 0.29516354886699E+03 0.35967512396242E+03 0.41207222178473E+03 0.29515643526142E+03
 0.29516351871022E+03 0.36803330502647E+03 0.41222807299455E+03 0.29515774990929E+03 0.29516354886699E+03
 0.35967512396242E+03 0.41207222178473E+03 0.29515643526142E+03 0.29516351871022E+03 0.45934784104385E+03
 0.34613154562891E+03 0.26563365662769E+04 0.22263788269598E+04 0.57253290786617E+03 0.10140285056935E+04
 0.43863293328803E+03 0.17012407757972E+04 0.14111000546622E+04 0.13971915724843E+04 0.21390141224469E+04
 0.15127506310631E+04 0.14073632670114E+04 0.12648585015156E+04 0.21367145112454E+04 0.17012407757973E+04
 0.14111000546622E+04 0.13971915724844E+04 0.21390141224469E+04 0.15127506310631E+04 0.14073632670114E+04
 0.12648585015157E+04 0.21367145112454E+04 0.19460714866489E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53342924248500E+03 0.18908942406035E+01
 0.18908942406035E+01 0.98164592895265E+01 0.00000000000000E+00 0.31808516845309E+03 0.31808516845309E+03
 0.31808516845309E+03 0.31808516845309E+03 0.00000000000000E+00 0.00000000000000E+00 0.21843881789923E+00
 0.00000000000000E+00 0.96543382810762E+01 0.10000000000000E-02 0.20903386365639E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38271310973567E+02 0.14351741615088E+02 0.34613610742895E+03 0.45934297305358E+03
 0.30177249234032E+03 0.30177249234032E+03 0.29515019464645E+03 0.29515019044600E+03 0.30170553446362E+03
 0.30170553446362E+03 0.29515019386355E+03 0.29515018967999E+03 0.30177249234032E+03 0.30177249234032E+03
 0.29515019464645E+03 0.29515019044600E+03 0.30170553446362E+03 0.30170553446362E+03 0.29515019386355E+03
 0.29515018967999E+03 0.30155516783451E+03 0.29515066865048E+03 -.26641492811691E+03 -.41146665664285E+03
 0.14820687549548E+03 0.26294838281537E+03 0.11400047294241E+03 0.16277303839849E+03 0.10822093307109E+03
 0.16277303839849E+03 0.18927196013037E+03 0.16314459350094E+03 0.10629464450650E+03 0.16314459350094E+03
 0.18775442050626E+03 0.16277303839849E+03 0.10822093307109E+03 0.16277303839849E+03 0.18927196013036E+03
 0.16314459350094E+03 0.10629464450650E+03 0.16314459350094E+03 0.18775442050625E+03 0.20955097565621E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32757129935577E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93964341831855E+00 0.92755619098288E+00 0.00000000000000E+00 0.93964341831855E+00 0.92755619098288E+00
 0.13905368124523E+01 0.36053684106252E+00 0.10299999713898E+01 0.15449999570847E+01 0.12480334019306E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.52737110958921E-03 0.00000000000000E+00
 0.14252136930493E-01 0.00000000000000E+00 0.12480334019306E-01 0.14779508040082E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1280.39809500
 0.72123493357658E+00 0.31112166460927E+03 0.50828765929276E+03 0.46088665826639E+03 0.44108592794027E+03
 0.22982628474010E+00 0.00000000000000E+00 0.19419860108283E+00 0.00000000000000E+00 -.19024119645717E+01
 0.41180538378635E-02 0.44591363488703E+00 0.19426652285222E+04 0.72849946069583E+03 0.17940693834192E+02
 0.67277601878220E+01 0.37488670011709E+03 0.29518379708536E+03 0.36828039767031E+03 0.41255847599355E+03
 0.29515812974743E+03 0.29516421141811E+03 0.35991067657627E+03 0.41240251434625E+03 0.29515675283285E+03
 0.29516417988629E+03 0.36828039767031E+03 0.41255847599355E+03 0.29515812974743E+03 0.29516421141811E+03
 0.35991067657627E+03 0.41240251434625E+03 0.29515675283285E+03 0.29516417988629E+03 0.45961512468115E+03
 0.34641177701239E+03 0.26571677109098E+04 0.22255477976385E+04 0.57121947144879E+03 0.10115469568688E+04
 0.43747138806275E+03 0.17020329698630E+04 0.14105104008672E+04 0.13968002120300E+04 0.21367843533560E+04
 0.15138264691528E+04 0.14067836704959E+04 0.12648707746161E+04 0.21344948984339E+04 0.17020329698630E+04
 0.14105104008672E+04 0.13968002120300E+04 0.21367843533560E+04 0.15138264691528E+04 0.14067836704959E+04
 0.12648707746161E+04 0.21344948984339E+04 0.19442033305777E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53357728019905E+03 0.18908942559148E+01
 0.18908942559148E+01 0.98839204718939E+01 0.00000000000000E+00 0.31823851083632E+03 0.31823851083632E+03
 0.31823851083632E+03 0.31823851083632E+03 0.00000000000000E+00 0.00000000000000E+00 0.21837484118210E+00
 0.00000000000000E+00 0.96577019498436E+01 0.10000000000000E-02 0.21026138807301E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38047879704962E+02 0.14267954889361E+02 0.34641631382669E+03 0.45961029190489E+03
 0.30183768833609E+03 0.30183768833609E+03 0.29515020499552E+03 0.29515020057174E+03 0.30177049512263E+03
 0.30177049512263E+03 0.29515020416025E+03 0.29515019975449E+03 0.30183768833609E+03 0.30183768833609E+03
 0.29515020499552E+03 0.29515020057174E+03 0.30177049512263E+03 0.30177049512263E+03 0.29515020416025E+03
 0.29515019975449E+03 0.30161420405472E+03 0.29515070137188E+03 -.26807104834739E+03 -.41393977710572E+03
 0.14908800341941E+03 0.26460518573598E+03 0.11477174229947E+03 0.16387109737542E+03 0.10888066146015E+03
 0.16387109737542E+03 0.19045422042682E+03 0.16424418951319E+03 0.10694544296571E+03 0.16424418951319E+03
 0.18892965620576E+03 0.16387109737542E+03 0.10888066146015E+03 0.16387109737542E+03 0.19045422042682E+03
 0.16424418951319E+03 0.10694544296571E+03 0.16424418951319E+03 0.18892965620575E+03 0.21113016492780E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32773098652984E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93974636394309E+00 0.92771699214530E+00 0.00000000000000E+00 0.93974636394309E+00 0.92771699214530E+00
 0.13906174381781E+01 0.36061746678829E+00 0.10299999713898E+01 0.15449999570847E+01 0.12485183998366E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.53203036082340E-03 0.00000000000000E+00
 0.14207405050166E-01 0.00000000000000E+00 0.12485183998366E-01 0.14739435410989E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1291.64162539
 0.72144460915346E+00 0.31121071246377E+03 0.50850844720460E+03 0.46106198482895E+03 0.44123828338090E+03
 0.22982547504868E+00 0.00000000000000E+00 0.19420380879993E+00 0.00000000000000E+00 -.19044715499028E+01
 0.41455417691253E-02 0.44567838142384E+00 0.19297839572095E+04 0.72366898395357E+03 0.17950163915157E+02
 0.67313114681840E+01 0.37523777114946E+03 0.29518571170671E+03 0.36860828297843E+03 0.41299591449468E+03
 0.29515865859845E+03 0.29516513366624E+03 0.36022340931463E+03 0.41283981030187E+03 0.29515719516786E+03
 0.29516510022749E+03 0.36860828297843E+03 0.41299591449468E+03 0.29515865859845E+03 0.29516513366624E+03
 0.36022340931463E+03 0.41283981030187E+03 0.29515719516786E+03 0.29516510022749E+03 0.45996783494997E+03
 0.34678061523498E+03 0.26582894261661E+04 0.22244465123324E+04 0.56950233199748E+03 0.10082994411461E+04
 0.43594959748862E+03 0.17030984338949E+04 0.14097221049531E+04 0.13962812779840E+04 0.21338281434893E+04
 0.15152655976553E+04 0.14060086422976E+04 0.12648843089716E+04 0.21315520512517E+04 0.17030984338949E+04
 0.14097221049530E+04 0.13962812779841E+04 0.21338281434893E+04 0.15152655976554E+04 0.14060086422976E+04
 0.12648843089717E+04 0.21315520512517E+04 0.19417375259298E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53377331496446E+03 0.18908942759335E+01
 0.18908942759335E+01 0.99738687150503E+01 0.00000000000000E+00 0.31843874240987E+03 0.31843874240987E+03
 0.31843874240987E+03 0.31843874240987E+03 0.00000000000000E+00 0.00000000000000E+00 0.21828973633440E+00
 0.00000000000000E+00 0.96623393581743E+01 0.10000000000000E-02 0.21189510079542E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37754530283944E+02 0.14157948856479E+02 0.34678512156914E+03 0.45996304330941E+03
 0.30192494757007E+03 0.30192494757007E+03 0.29515021949215E+03 0.29515021475553E+03 0.30185740242930E+03
 0.30185740242930E+03 0.29515021858238E+03 0.29515021386540E+03 0.30192494757007E+03 0.30192494757007E+03
 0.29515021949215E+03 0.29515021475553E+03 0.30185740242930E+03 0.30185740242930E+03 0.29515021858238E+03
 0.29515021386540E+03 0.30169366942264E+03 0.29515074700165E+03 -.27025209982331E+03 -.41720860626974E+03
 0.15023797763006E+03 0.26674854513024E+03 0.11575937761203E+03 0.16531596552587E+03 0.10974369340915E+03
 0.16531596552587E+03 0.19198690859795E+03 0.16569132574545E+03 0.10779711968022E+03 0.16569132574545E+03
 0.19045373698239E+03 0.16531596552587E+03 0.10974369340915E+03 0.16531596552587E+03 0.19198690859796E+03
 0.16569132574545E+03 0.10779711968022E+03 0.16569132574545E+03 0.19045373698238E+03 0.21301677312037E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32793908167429E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93988332280926E+00 0.92792788283558E+00 0.00000000000000E+00 0.93988332280926E+00 0.92792788283558E+00
 0.13907222759665E+01 0.36072230457673E+00 0.10299999713898E+01 0.15449999570847E+01 0.12490238027443E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.53819792858372E-03 0.00000000000000E+00
 0.14152559157858E-01 0.00000000000000E+00 0.12490238027443E-01 0.14690757086442E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1300.07427319
 0.72159930597489E+00 0.31127621069556E+03 0.50867294188850E+03 0.46119249381165E+03 0.44135148982660E+03
 0.22982492897847E+00 0.00000000000000E+00 0.19420770091372E+00 0.00000000000000E+00 -.19059845770188E+01
 0.41621420760938E-02 0.44550040602415E+00 0.19220871978277E+04 0.72078269918540E+03 0.17957334924553E+02
 0.67340005967073E+01 0.37549980274705E+03 0.29518719983359E+03 0.36885306160374E+03 0.41332185179348E+03
 0.29515907250880E+03 0.29516585529320E+03 0.36045697746035E+03 0.41316564376911E+03 0.29515754150378E+03
 0.29516582036779E+03 0.36885306160374E+03 0.41332185179348E+03 0.29515907250880E+03 0.29516585529320E+03
 0.36045697746035E+03 0.41316564376911E+03 0.29515754150378E+03 0.29516582036779E+03 0.46022999431147E+03
 0.34705403820235E+03 0.26591308174326E+04 0.22236181382950E+04 0.56823063929990E+03 0.10058951115073E+04
 0.43482331901092E+03 0.17038978257208E+04 0.14091255481932E+04 0.13958896795854E+04 0.21316173193372E+04
 0.15163427997784E+04 0.14054219521702E+04 0.12648887604929E+04 0.21293511406735E+04 0.17038978257208E+04
 0.14091255481932E+04 0.13958896795854E+04 0.21316173193372E+04 0.15163427997784E+04 0.14054219521702E+04
 0.12648887604929E+04 0.21293511406735E+04 0.19398979858814E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53391925674766E+03 0.18908942906398E+01
 0.18908942906398E+01 0.10041329897418E+02 0.00000000000000E+00 0.31858606342548E+03 0.31858606342548E+03
 0.31858606342548E+03 0.31858606342548E+03 0.00000000000000E+00 0.00000000000000E+00 0.21822606193880E+00
 0.00000000000000E+00 0.96659144140613E+01 0.10000000000000E-02 0.21311783662385E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37537918584074E+02 0.14076719469028E+02 0.34705852082681E+03 0.46022523344358E+03
 0.30199051485195E+03 0.30199051485195E+03 0.29515023090714E+03 0.29515022592419E+03 0.30192268485454E+03
 0.30192268485454E+03 0.29515022993782E+03 0.29515022497579E+03 0.30199051485195E+03 0.30199051485195E+03
 0.29515023090714E+03 0.29515022592419E+03 0.30192268485454E+03 0.30192268485454E+03 0.29515022993782E+03
 0.29515022497579E+03 0.30175363342507E+03 0.29515078277337E+03 -.27187072065690E+03 -.41964232699021E+03
 0.15108445233413E+03 0.26831366078254E+03 0.11647378618674E+03 0.16638730102866E+03 0.11038013678912E+03
 0.16638730102866E+03 0.19310793539565E+03 0.16676448292480E+03 0.10842538370008E+03 0.16676448292480E+03
 0.19156875702614E+03 0.16638730102866E+03 0.11038013678912E+03 0.16638730102866E+03 0.19310793539565E+03
 0.16676448292480E+03 0.10842538370008E+03 0.16676448292480E+03 0.19156875702614E+03 0.21428799324513E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32809190140571E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93998647700266E+00 0.92808176337515E+00 0.00000000000000E+00 0.93998647700266E+00 0.92808176337515E+00
 0.13907996243772E+01 0.36079965298744E+00 0.10299999713898E+01 0.15449999570847E+01 0.12493101244820E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.54278222911679E-03 0.00000000000000E+00
 0.14114618095607E-01 0.00000000000000E+00 0.12493101244820E-01 0.14657400324723E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1311.31780358
 0.72180324719865E+00 0.31136225335903E+03 0.50889078727282E+03 0.46136520820836E+03 0.44150112632391E+03
 0.22982423576090E+00 0.00000000000000E+00 0.19421287003495E+00 0.00000000000000E+00 -.19079657303533E+01
 0.41812766944101E-02 0.44526154680286E+00 0.19132912229165E+04 0.71748420859367E+03 0.17966968083013E+02
 0.67376130311298E+01 0.37584748592719E+03 0.29518925511950E+03 0.36917791942638E+03 0.41375364238241E+03
 0.29515964811855E+03 0.29516685857751E+03 0.36076708684615E+03 0.41359729989616E+03 0.29515802333189E+03
 0.29516682159264E+03 0.36917791942638E+03 0.41375364238241E+03 0.29515964811855E+03 0.29516685857751E+03
 0.36076708684615E+03 0.41359729989616E+03 0.29515802333189E+03 0.29516682159264E+03 0.46057647704449E+03
 0.34741450819227E+03 0.26602448273115E+04 0.22225065793171E+04 0.56655418182322E+03 0.10027274043068E+04
 0.43334045157447E+03 0.17049586328648E+04 0.14083220372777E+04 0.13953621239270E+04 0.21286763790188E+04
 0.15177714457901E+04 0.14046315094112E+04 0.12648854116983E+04 0.21264232999119E+04 0.17049586328648E+04
 0.14083220372777E+04 0.13953621239270E+04 0.21286763790188E+04 0.15177714457901E+04 0.14046315094112E+04
 0.12648854116983E+04 0.21264232999119E+04 0.19374553436065E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53411242971007E+03 0.18908943098962E+01
 0.18908943098962E+01 0.10131278140574E+02 0.00000000000000E+00 0.31877908964012E+03 0.31877908964012E+03
 0.31877908964012E+03 0.31877908964012E+03 0.00000000000000E+00 0.00000000000000E+00 0.21814137605440E+00
 0.00000000000000E+00 0.96707845923463E+01 0.10000000000000E-02 0.21474436424280E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37253596983596E+02 0.13970098868849E+02 0.34741895662024E+03 0.46057175920043E+03
 0.30207802659102E+03 0.30207802659102E+03 0.29515024687927E+03 0.29515024155165E+03 0.30200979594046E+03
 0.30200979594046E+03 0.29515024582537E+03 0.29515024052048E+03 0.30207802659102E+03 0.30207802659102E+03
 0.29515024687927E+03 0.29515024155165E+03 0.30200979594046E+03 0.30200979594046E+03 0.29515024582537E+03
 0.29515024052048E+03 0.30183392738568E+03 0.29515083260745E+03 -.27400696793247E+03 -.42286274266523E+03
 0.15219366035620E+03 0.27035037160159E+03 0.11739574294361E+03 0.16780044211271E+03 0.11121546919657E+03
 0.16780044211271E+03 0.19456868307711E+03 0.16818017423290E+03 0.10925020344980E+03 0.16818017423290E+03
 0.19302199823755E+03 0.16780044211271E+03 0.11121546919657E+03 0.16780044211271E+03 0.19456868307712E+03
 0.16818017423290E+03 0.10925020344980E+03 0.16818017423290E+03 0.19302199823755E+03 0.21582033687115E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32829180640088E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94012467819019E+00 0.92828123878281E+00 0.00000000000000E+00 0.94012467819019E+00 0.92828123878281E+00
 0.13909015949891E+01 0.36090162359933E+00 0.10299999713898E+01 0.15449999570847E+01 0.12495882526085E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.54882316026338E-03 0.00000000000000E+00
 0.14067683139222E-01 0.00000000000000E+00 0.12495882526085E-01 0.14616506299486E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1327.86365096
 0.72209931335107E+00 0.31148398541451E+03 0.50920628594160E+03 0.46161457346000E+03 0.44171634079536E+03
 0.22982322020249E+00 0.00000000000000E+00 0.19422042225524E+00 0.00000000000000E+00 -.19107718263371E+01
 0.42065802510319E-02 0.44490990412929E+00 0.19017823321064E+04 0.71316837453988E+03 0.17981168604588E+02
 0.67429382267206E+01 0.37635659144607E+03 0.29519246230056E+03 0.36965357216495E+03 0.41438702719809E+03
 0.29516055680867E+03 0.29516844170669E+03 0.36122080282388E+03 0.41423048044737E+03 0.29515878448786E+03
 0.29516840149194E+03 0.36965357216495E+03 0.41438702719809E+03 0.29516055680867E+03 0.29516844170669E+03
 0.36122080282388E+03 0.41423048044737E+03 0.29515878448786E+03 0.29516840149194E+03 0.46108848732998E+03
 0.34794572758901E+03 0.26618515594717E+04 0.22208096137062E+04 0.56393951085597E+03 0.99784590834490E+03
 0.43108669993465E+03 0.17065006665770E+04 0.14070635565095E+04 0.13945443777480E+04 0.21242515759778E+04
 0.15198575582306E+04 0.14033921921398E+04 0.12648445206387E+04 0.21220177567269E+04 0.17065006665770E+04
 0.14070635565095E+04 0.13945443777480E+04 0.21242515759778E+04 0.15198575582306E+04 0.14033921921398E+04
 0.12648445206387E+04 0.21220177567269E+04 0.19337444898279E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53439170798348E+03 0.18908943371708E+01
 0.18908943371708E+01 0.10263644919609E+02 0.00000000000000E+00 0.31905714915077E+03 0.31905714915077E+03
 0.31905714915077E+03 0.31905714915077E+03 0.00000000000000E+00 0.00000000000000E+00 0.21801721668551E+00
 0.00000000000000E+00 0.96781325838705E+01 0.10000000000000E-02 0.21712901965003E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36844453186839E+02 0.13816669945065E+02 0.34795009371445E+03 0.46108386196534E+03
 0.30220489645496E+03 0.30220489645496E+03 0.29515027741039E+03 0.29515026648664E+03 0.30213604458183E+03
 0.30213604458183E+03 0.29515027619595E+03 0.29515026532001E+03 0.30220489645496E+03 0.30220489645496E+03
 0.29515027741039E+03 0.29515026648664E+03 0.30213604458183E+03 0.30213604458183E+03 0.29515027619595E+03
 0.29515026532001E+03 0.30195075816216E+03 0.29515091153649E+03 -.27719342214821E+03 -.42769483425920E+03
 0.15382143247023E+03 0.27330504010947E+03 0.11871450047689E+03 0.16989015878159E+03 0.11244074755889E+03
 0.16989015878159E+03 0.19669022902383E+03 0.17027382169804E+03 0.11045997745926E+03 0.17027382169804E+03
 0.19513263848400E+03 0.16989015878159E+03 0.11244074755889E+03 0.16989015878159E+03 0.19669022902383E+03
 0.17027382169804E+03 0.11045997745926E+03 0.17027382169804E+03 0.19513263848400E+03 0.21766607246434E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32857877334158E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94031939408779E+00 0.92856790090925E+00 0.00000000000000E+00 0.94031939408779E+00 0.92856790090925E+00
 0.13910496280653E+01 0.36104965667554E+00 0.10299999713898E+01 0.15449999570847E+01 0.12497376683321E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.55786460018314E-03 0.00000000000000E+00
 0.14007022146269E-01 0.00000000000000E+00 0.12497376683321E-01 0.14564886746452E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1336.13657465
 0.72224484738382E+00 0.31154443867590E+03 0.50936301602214E+03 0.46173853328710E+03 0.44182334675700E+03
 0.22982270406973E+00 0.00000000000000E+00 0.19422416264898E+00 0.00000000000000E+00 -.19121367671589E+01
 0.42186756770145E-02 0.44473357138488E+00 0.18963297045061E+04 0.71112363918979E+03 0.17988297971499E+02
 0.67456117393120E+01 0.37660926688291E+03 0.29519413607654E+03 0.36988976076299E+03 0.41470005761861E+03
 0.29516103508200E+03 0.29516927467306E+03 0.36144639581948E+03 0.41454341720079E+03 0.29515918530913E+03
 0.29516923276649E+03 0.36988976076299E+03 0.41470005761861E+03 0.29516103508200E+03 0.29516927467306E+03
 0.36144639581948E+03 0.41454341720079E+03 0.29515918530913E+03 0.29516923276649E+03 0.46133844583931E+03
 0.34820472003939E+03 0.26626395272444E+04 0.22199567931237E+04 0.56271648714425E+03 0.99554505850744E+03
 0.43001498892747E+03 0.17072604076855E+04 0.14064397942674E+04 0.13941312949385E+04 0.21220714559968E+04
 0.15208854179870E+04 0.14027779908352E+04 0.12648131362628E+04 0.21198471784803E+04 0.17072604076855E+04
 0.14064397942674E+04 0.13941312949385E+04 0.21220714559968E+04 0.15208854179871E+04 0.14027779908352E+04
 0.12648131362628E+04 0.21198471784803E+04 0.19319358532159E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53453046224311E+03 0.18908943504377E+01
 0.18908943504377E+01 0.10329828309126E+02 0.00000000000000E+00 0.31919377528596E+03 0.31919377528596E+03
 0.31919377528596E+03 0.31919377528596E+03 0.00000000000000E+00 0.00000000000000E+00 0.21795534872690E+00
 0.00000000000000E+00 0.96818418919615E+01 0.10000000000000E-02 0.21831711301817E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36643943708316E+02 0.13741478890618E+02 0.34820906068895E+03 0.46133385083643E+03
 0.30226888375188E+03 0.30226888375188E+03 0.29515029118240E+03 0.29515027971634E+03 0.30219970816265E+03
 0.30219970816265E+03 0.29515028989174E+03 0.29515027847650E+03 0.30226888375188E+03 0.30226888375188E+03
 0.29515029118240E+03 0.29515027971634E+03 0.30219970816265E+03 0.30219970816265E+03 0.29515028989174E+03
 0.29515027847650E+03 0.30200983702876E+03 0.29515095318785E+03 -.27873684495371E+03 -.43003404973815E+03
 0.15461013939466E+03 0.27473218282938E+03 0.11934899273774E+03 0.17090990493418E+03 0.11303698618958E+03
 0.17090990493418E+03 0.19771718754676E+03 0.17129561327145E+03 0.11104903368582E+03 0.17129561327145E+03
 0.19615478017665E+03 0.17090990493419E+03 0.11303698618958E+03 0.17090990493419E+03 0.19771718754676E+03
 0.17129561327145E+03 0.11104903368582E+03 0.17129561327145E+03 0.19615478017664E+03 0.21855219321508E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32871976248769E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94042513114422E+00 0.92869743073870E+00 0.00000000000000E+00 0.94042513114422E+00 0.92869743073870E+00
 0.13911223950817E+01 0.36112242369191E+00 0.10299999713898E+01 0.15449999570847E+01 0.12497931634027E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.56224191677339E-03 0.00000000000000E+00
 0.13977828911628E-01 0.00000000000000E+00 0.12497931634027E-01 0.14540070828401E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1344.40949834
 0.72238941019841E+00 0.31160534805204E+03 0.50951948554538E+03 0.46186245986080E+03 0.44193048935612E+03
 0.22982218259414E+00 0.00000000000000E+00 0.19422787260398E+00 0.00000000000000E+00 -.19135210136147E+01
 0.42305704255194E-02 0.44455688524326E+00 0.18909979495301E+04 0.70912423107380E+03 0.17995447299444E+02
 0.67482927372914E+01 0.37686076142196E+03 0.29519585768387E+03 0.37012488458951E+03 0.41501112034589E+03
 0.29516152979065E+03 0.29517013606147E+03 0.36167107816697E+03 0.41485438938144E+03 0.29515960004184E+03
 0.29517009241054E+03 0.37012488458951E+03 0.41501112034589E+03 0.29516152979065E+03 0.29517013606147E+03
 0.36167107816697E+03 0.41485438938144E+03 0.29515960004184E+03 0.29517009241054E+03 0.46158615737343E+03
 0.34846083781680E+03 0.26634257663500E+04 0.22191161779644E+04 0.56152282195078E+03 0.99329586261810E+03
 0.42896542655756E+03 0.17080181682429E+04 0.14058246520498E+04 0.13937262608285E+04 0.21199171308274E+04
 0.15219095540256E+04 0.14021723656207E+04 0.12647865253804E+04 0.21177023288585E+04 0.17080181682429E+04
 0.14058246520498E+04 0.13937262608285E+04 0.21199171308274E+04 0.15219095540256E+04 0.14021723656207E+04
 0.12647865253804E+04 0.21177023288585E+04 0.19301555169134E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53466908098508E+03 0.18908943638923E+01
 0.18908943638923E+01 0.10396011698643E+02 0.00000000000000E+00 0.31932888146279E+03 0.31932888146279E+03
 0.31932888146279E+03 0.31932888146279E+03 0.00000000000000E+00 0.00000000000000E+00 0.21789362315365E+00
 0.00000000000000E+00 0.96855797973236E+01 0.10000000000000E-02 0.21950232311239E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36446083515498E+02 0.13667281318312E+02 0.34846515730997E+03 0.46158158946919E+03
 0.30233297658849E+03 0.30233297658849E+03 0.29515030550493E+03 0.29515029347488E+03 0.30226347208092E+03
 0.30226347208092E+03 0.29515030413403E+03 0.29515029215796E+03 0.30233297658849E+03 0.30233297658849E+03
 0.29515030550493E+03 0.29515029347488E+03 0.30226347208092E+03 0.30226347208092E+03 0.29515030413403E+03
 0.29515029215796E+03 0.30206909363871E+03 0.29515099634945E+03 -.28026239251347E+03 -.43234723087034E+03
 0.15538706204085E+03 0.27613368477864E+03 0.11996968742759E+03 0.17191884650711E+03 0.11362506335143E+03
 0.17191884650711E+03 0.19872638878222E+03 0.17230663267936E+03 0.11163015159363E+03 0.17230663267936E+03
 0.19715941214374E+03 0.17191884650711E+03 0.11362506335143E+03 0.17191884650711E+03 0.19872638878223E+03
 0.17230663267936E+03 0.11163015159363E+03 0.17230663267936E+03 0.19715941214374E+03 0.21938628376041E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32885911232217E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94052909453233E+00 0.92883085052126E+00 0.00000000000000E+00 0.94052909453233E+00 0.92883085052126E+00
 0.13911946764890E+01 0.36119470509921E+00 0.10299999713898E+01 0.15449999570847E+01 0.12498186208292E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.56649839504034E-03 0.00000000000000E+00
 0.13949919840394E-01 0.00000000000000E+00 0.12498186208292E-01 0.14516418235434E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1352.68242203
 0.72253369016018E+00 0.31166640841419E+03 0.50967543300551E+03 0.46198603593118E+03 0.44203741594165E+03
 0.22982165497009E+00 0.00000000000000E+00 0.19423155202818E+00 0.00000000000000E+00 -.19149096520187E+01
 0.42423717486118E-02 0.44438024654040E+00 0.18857376189670E+04 0.70715160711262E+03 0.18002600390728E+02
 0.67509751465230E+01 0.37711117528851E+03 0.29519762790253E+03 0.37035903926362E+03 0.41532042255438E+03
 0.29516204130023E+03 0.29517102649234E+03 0.36189492658781E+03 0.41516360385214E+03 0.29516002900093E+03
 0.29517098104351E+03 0.37035903926362E+03 0.41532042255438E+03 0.29516204130023E+03 0.29517102649234E+03
 0.36189492658781E+03 0.41516360385214E+03 0.29516002900093E+03 0.29517098104351E+03 0.46183202284663E+03
 0.34871440280605E+03 0.26642068482511E+04 0.22182805003956E+04 0.56034737380774E+03 0.99108012997358E+03
 0.42793101929679E+03 0.17087720845209E+04 0.14052125170588E+04 0.13933245157644E+04 0.21177788162465E+04
 0.15229284326288E+04 0.14015697052029E+04 0.12647605258628E+04 0.21155734265860E+04 0.17087720845209E+04
 0.14052125170587E+04 0.13933245157644E+04 0.21177788162465E+04 0.15229284326289E+04 0.14015697052029E+04
 0.12647605258628E+04 0.21155734265860E+04 0.19283921520448E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53480728237291E+03 0.18908943773895E+01
 0.18908943773895E+01 0.10462195088161E+02 0.00000000000000E+00 0.31946254803786E+03 0.31946254803786E+03
 0.31946254803786E+03 0.31946254803786E+03 0.00000000000000E+00 0.00000000000000E+00 0.21783204119923E+00
 0.00000000000000E+00 0.96893425536217E+01 0.10000000000000E-02 0.22068458528415E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36250832787887E+02 0.13594062295458E+02 0.34871870083610E+03 0.46182748367193E+03
 0.30239711329001E+03 0.30239711329001E+03 0.29515032039455E+03 0.29515030777818E+03 0.30232727558662E+03
 0.30232727558662E+03 0.29515031893922E+03 0.29515030638016E+03 0.30239711329001E+03 0.30239711329001E+03
 0.29515032039455E+03 0.29515030777818E+03 0.30232727558662E+03 0.30232727558662E+03 0.29515031893922E+03
 0.29515030638016E+03 0.30212846117064E+03 0.29515104106137E+03 -.28177270967890E+03 -.43463860245412E+03
 0.15615357942431E+03 0.27751211745778E+03 0.12057777013635E+03 0.17291846508423E+03 0.11420593415791E+03
 0.17291846508423E+03 0.19971961962287E+03 0.17330835607579E+03 0.11220425692679E+03 0.17330835607579E+03
 0.19814828705447E+03 0.17291846508423E+03 0.11420593415791E+03 0.17291846508423E+03 0.19971961962287E+03
 0.17330835607579E+03 0.11220425692679E+03 0.17330835607579E+03 0.19814828705446E+03 0.22016969579533E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32899689179063E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94063204162222E+00 0.92896510634198E+00 0.00000000000000E+00 0.94063204162222E+00 0.92896510634198E+00
 0.13912668164699E+01 0.36126684508009E+00 0.10299999713898E+01 0.15449999570847E+01 0.12498138201569E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.57066395684701E-03 0.00000000000000E+00
 0.13923254689961E-01 0.00000000000000E+00 0.12498138201569E-01 0.14493918646808E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1360.95534572
 0.72267786185524E+00 0.31172728457955E+03 0.50983061641801E+03 0.46210898565821E+03 0.44214381201140E+03
 0.22982111988438E+00 0.00000000000000E+00 0.19423520378902E+00 0.00000000000000E+00 -.19162899100772E+01
 0.42541716333835E-02 0.44420397712726E+00 0.18805071091213E+04 0.70519016592049E+03 0.18009744198459E+02
 0.67536540744220E+01 0.37736057369828E+03 0.29519944751176E+03 0.37059228848792E+03 0.41562808091646E+03
 0.29516256997878E+03 0.29517194658974E+03 0.36211799547639E+03 0.41547117717266E+03 0.29516047250369E+03
 0.29517189928851E+03 0.37059228848792E+03 0.41562808091646E+03 0.29516256997878E+03 0.29517194658974E+03
 0.36211799547639E+03 0.41547117717266E+03 0.29516047250369E+03 0.29517189928851E+03 0.46207613327125E+03
 0.34896556139501E+03 0.26649793807516E+04 0.22174425302351E+04 0.55918511887277E+03 0.98888931644010E+03
 0.42690827197297E+03 0.17095201709674E+04 0.14045984826795E+04 0.13929211478932E+04 0.21156484162860E+04
 0.15239402730958E+04 0.14009651130153E+04 0.12647306398096E+04 0.21134523857155E+04 0.17095201709674E+04
 0.14045984826795E+04 0.13929211478932E+04 0.21156484162860E+04 0.15239402730958E+04 0.14009651130153E+04
 0.12647306398096E+04 0.21134523857155E+04 0.19266375726622E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53494480642877E+03 0.18908943908053E+01
 0.18908943908053E+01 0.10528378477678E+02 0.00000000000000E+00 0.31959486774823E+03 0.31959486774823E+03
 0.31959486774823E+03 0.31959486774823E+03 0.00000000000000E+00 0.00000000000000E+00 0.21777060430784E+00
 0.00000000000000E+00 0.96931223481053E+01 0.10000000000000E-02 0.22186382141436E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36058154723022E+02 0.13521808021133E+02 0.34896983622227E+03 0.46207162543797E+03
 0.30246125914970E+03 0.30246125914970E+03 0.29515033586817E+03 0.29515032264249E+03 0.30239108456786E+03
 0.30239108456786E+03 0.29515033432404E+03 0.29515032115917E+03 0.30246125914970E+03 0.30246125914970E+03
 0.29515033586817E+03 0.29515032264249E+03 0.30239108456786E+03 0.30239108456786E+03 0.29515033432404E+03
 0.29515032115917E+03 0.30218789939069E+03 0.29515108736429E+03 -.28326889020997E+03 -.43690956802142E+03
 0.15691045962130E+03 0.27886933735339E+03 0.12117432543399E+03 0.17390947438839E+03 0.11478014242700E+03
 0.17390947438839E+03 0.20069817980493E+03 0.17430149372228E+03 0.11277187855729E+03 0.17430149372228E+03
 0.19912268680987E+03 0.17390947438839E+03 0.11478014242700E+03 0.17390947438839E+03 0.20069817980493E+03
 0.17430149372228E+03 0.11277187855729E+03 0.17430149372228E+03 0.19912268680987E+03 0.22090739421745E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32913319611673E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94073456775110E+00 0.92909782497307E+00 0.00000000000000E+00 0.94073456775110E+00 0.92909782497307E+00
 0.13913389023174E+01 0.36133893092762E+00 0.10299999713898E+01 0.15449999570847E+01 0.12497816866577E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.57476740517206E-03 0.00000000000000E+00
 0.13897699622560E-01 0.00000000000000E+00 0.12497816866577E-01 0.14472467027732E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1377.50119310
 0.72296514282830E+00 0.31184828289910E+03 0.51013853887911E+03 0.46235289113376E+03 0.44235488877546E+03
 0.22982002584082E+00 0.00000000000000E+00 0.19424243347672E+00 0.00000000000000E+00 -.19190206681471E+01
 0.42779512091554E-02 0.44385304227289E+00 0.18700540536505E+04 0.70127027011893E+03 0.18023983701978E+02
 0.67589938882418E+01 0.37785636401879E+03 0.29520323801421E+03 0.37105610466325E+03 0.41623856687387E+03
 0.29516368032698E+03 0.29517387829787E+03 0.36256181453399E+03 0.41608150090162E+03 0.29516140442042E+03
 0.29517382712437E+03 0.37105610466325E+03 0.41623856687387E+03 0.29516368032698E+03 0.29517387829787E+03
 0.36256181453399E+03 0.41608150090162E+03 0.29516140442042E+03 0.29517382712437E+03 0.46255904605372E+03
 0.34946091611664E+03 0.26664970049764E+04 0.22157556528994E+04 0.55689982459568E+03 0.98458090708599E+03
 0.42489658336733E+03 0.17109975838257E+04 0.14033616809104E+04 0.13921065484797E+04 0.21114059996092E+04
 0.15259417644713E+04 0.13997471217997E+04 0.12646565917735E+04 0.21092285523921E+04 0.17109975838257E+04
 0.14033616809104E+04 0.13921065484797E+04 0.21114059996092E+04 0.15259417644713E+04 0.13997471217997E+04
 0.12646565917735E+04 0.21092285523921E+04 0.19231506881283E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53521767753209E+03 0.18908944173477E+01
 0.18908944173477E+01 0.10660745256713E+02 0.00000000000000E+00 0.31985579821243E+03 0.31985579821243E+03
 0.31985579821243E+03 0.31985579821243E+03 0.00000000000000E+00 0.00000000000000E+00 0.21764817070384E+00
 0.00000000000000E+00 0.97007131423701E+01 0.10000000000000E-02 0.22421294313392E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35680366566625E+02 0.13380137462484E+02 0.34946514039815E+03 0.46255460546860E+03
 0.30258952075343E+03 0.30258952075343E+03 0.29515036863663E+03 0.29515035412060E+03 0.30251866367871E+03
 0.30251866367871E+03 0.29515036690111E+03 0.29515035245342E+03 0.30258952075343E+03 0.30258952075343E+03
 0.29515036863663E+03 0.29515035412060E+03 0.30251866367871E+03 0.30251866367871E+03 0.29515036690111E+03
 0.29515035245342E+03 0.30230690750502E+03 0.29515118490911E+03 -.28622028940160E+03 -.44139103588648E+03
 0.15839700912288E+03 0.28152523681813E+03 0.12233624264964E+03 0.17586692681860E+03 0.11590977872952E+03
 0.17586692681860E+03 0.20261481984799E+03 0.17626326024776E+03 0.11388884394031E+03 0.17626326024776E+03
 0.20103154652790E+03 0.17586692681860E+03 0.11590977872952E+03 0.17586692681860E+03 0.20261481984799E+03
 0.17626326024776E+03 0.11388884394031E+03 0.17626326024776E+03 0.20103154652790E+03 0.22226661379583E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32940175923613E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94093888660383E+00 0.92935778058323E+00 0.00000000000000E+00 0.94093888660383E+00 0.92935778058323E+00
 0.13914825428039E+01 0.36148257141415E+00 0.10299999713898E+01 0.15449999570847E+01 0.12496490754648E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.58281468319667E-03 0.00000000000000E+00
 0.13849436545545E-01 0.00000000000000E+00 0.12496490754648E-01 0.14432251228741E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1385.77411679
 0.72310798375291E+00 0.31190852106501E+03 0.51029135119990E+03 0.46247394842989E+03 0.44245968977766E+03
 0.22981946798585E+00 0.00000000000000E+00 0.19424601207827E+00 0.00000000000000E+00 -.19203761505860E+01
 0.42899533210956E-02 0.44367849680450E+00 0.18648221556772E+04 0.69930830837894E+03 0.18031074432541E+02
 0.67616529122030E+01 0.37810275444238E+03 0.29520521046060E+03 0.37128666672363E+03 0.41654141475278E+03
 0.29516426274434E+03 0.29517489117362E+03 0.36278255503069E+03 0.41638427149056E+03 0.29516189347987E+03
 0.29517483797830E+03 0.37128666672363E+03 0.41654141475278E+03 0.29516426274434E+03 0.29517489117362E+03
 0.36278255503069E+03 0.41638427149056E+03 0.29516189347987E+03 0.29517483797830E+03 0.46279789982800E+03
 0.34970521859555E+03 0.26672434557099E+04 0.22149092874562E+04 0.55577740725305E+03 0.98246412125217E+03
 0.42390782696286E+03 0.17117276602935E+04 0.14027403088731E+04 0.13916969829802E+04 0.21092957924560E+04
 0.15269321730893E+04 0.13991351198598E+04 0.12646140888974E+04 0.21071275723987E+04 0.17117276602935E+04
 0.14027403088731E+04 0.13916969829802E+04 0.21092957924560E+04 0.15269321730893E+04 0.13991351198598E+04
 0.12646140888975E+04 0.21071275723987E+04 0.19214202912511E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53535311006749E+03 0.18908944305227E+01
 0.18908944305227E+01 0.10726928646230E+02 0.00000000000000E+00 0.31998454303251E+03 0.31998454303251E+03
 0.31998454303251E+03 0.31998454303251E+03 0.00000000000000E+00 0.00000000000000E+00 0.21758717586715E+00
 0.00000000000000E+00 0.97045183090125E+01 0.10000000000000E-02 0.22538272587052E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35495178120244E+02 0.13310691795091E+02 0.34970941658651E+03 0.46279349373501E+03
 0.30265362081494E+03 0.30265362081494E+03 0.29515038596693E+03 0.29515037076848E+03 0.30258241923097E+03
 0.30258241923097E+03 0.29515038412844E+03 0.29515036900238E+03 0.30265362081494E+03 0.30265362081494E+03
 0.29515038596693E+03 0.29515037076848E+03 0.30258241923097E+03 0.30258241923097E+03 0.29515038412844E+03
 0.29515036900238E+03 0.30236645148090E+03 0.29515123623556E+03 -.28767605801707E+03 -.44360182129405E+03
 0.15912734886773E+03 0.28282590607229E+03 0.12290292046022E+03 0.17683384909214E+03 0.11646567445768E+03
 0.17683384909214E+03 0.20355426069739E+03 0.17723236201437E+03 0.11443864083196E+03 0.17723236201437E+03
 0.20196734702335E+03 0.17683384909214E+03 0.11646567445768E+03 0.17683384909214E+03 0.20355426069739E+03
 0.17723236201437E+03 0.11443864083196E+03 0.17723236201437E+03 0.20196734702334E+03 0.22289628915109E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32953417633592E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94104056453917E+00 0.92948589887385E+00 0.00000000000000E+00 0.94104056453917E+00 0.92948589887385E+00
 0.13915539632662E+01 0.36155399187645E+00 0.10299999713898E+01 0.15449999570847E+01 0.12495540830448E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.58675528815759E-03 0.00000000000000E+00
 0.13826550560904E-01 0.00000000000000E+00 0.12495540830448E-01 0.14413305849061E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1394.04704048
 0.72325027000562E+00 0.31196859972649E+03 0.51044341491238E+03 0.46259442702154E+03 0.44256401818573E+03
 0.22981890396788E+00 0.00000000000000E+00 0.19424956597673E+00 0.00000000000000E+00 -.19217257595475E+01
 0.43020282499898E-02 0.44350465941043E+00 0.18595879745836E+04 0.69734549046886E+03 0.18038141945644E+02
 0.67643032296166E+01 0.37834815360860E+03 0.29520723540355E+03 0.37151634135330E+03 0.41684269644868E+03
 0.29516486382595E+03 0.29517593624562E+03 0.36300252267912E+03 0.41668547838406E+03 0.29516239837378E+03
 0.29517588097004E+03 0.37151634135330E+03 0.41684269644868E+03 0.29516486382595E+03 0.29517593624562E+03
 0.36300252267912E+03 0.41668547838406E+03 0.29516239837378E+03 0.29517588097004E+03 0.46303508300136E+03
 0.34994734149448E+03 0.26679820983237E+04 0.22140614621763E+04 0.55466782139574E+03 0.98037109219268E+03
 0.42292993168995E+03 0.17124522459716E+04 0.14021172168940E+04 0.13912862236965E+04 0.21071930240397E+04
 0.15279159743960E+04 0.13985213746651E+04 0.12645682910711E+04 0.21050339888209E+04 0.17124522459716E+04
 0.14021172168940E+04 0.13912862236965E+04 0.21071930240397E+04 0.15279159743960E+04 0.13985213746651E+04
 0.12645682910711E+04 0.21050339888209E+04 0.19196985211960E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53548789137086E+03 0.18908944436406E+01
 0.18908944436406E+01 0.10793112035747E+02 0.00000000000000E+00 0.32011221621339E+03 0.32011221621339E+03
 0.32011221621339E+03 0.32011221621339E+03 0.00000000000000E+00 0.00000000000000E+00 0.21752632997092E+00
 0.00000000000000E+00 0.97083264240585E+01 0.10000000000000E-02 0.22654926951069E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35312406953590E+02 0.13242152607596E+02 0.34995151298932E+03 0.46303071158943E+03
 0.30271769116150E+03 0.30271769116150E+03 0.29515040395215E+03 0.29515038804548E+03 0.30264614350400E+03
 0.30264614350400E+03 0.29515040200557E+03 0.29515038617555E+03 0.30271769116150E+03 0.30271769116150E+03
 0.29515040395215E+03 0.29515038804548E+03 0.30264614350400E+03 0.30264614350400E+03 0.29515040200557E+03
 0.29515038617555E+03 0.30242600712702E+03 0.29515128932215E+03 -.28911892267620E+03 -.44579294248117E+03
 0.15984947392083E+03 0.28410953856126E+03 0.12346081727083E+03 0.17779320655130E+03 0.11701590554864E+03
 0.17779320655130E+03 0.20448190151184E+03 0.17819391046038E+03 0.11498292202296E+03 0.17819391046038E+03
 0.20289150193966E+03 0.17779320655130E+03 0.11701590554864E+03 0.17779320655130E+03 0.20448190151184E+03
 0.17819391046038E+03 0.11498292202295E+03 0.17819391046038E+03 0.20289150193966E+03 0.22349705564113E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32966543779303E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94114188332333E+00 0.92961293350793E+00 0.00000000000000E+00 0.94114188332333E+00 0.92961293350793E+00
 0.13916251063926E+01 0.36162513500281E+00 0.10299999713898E+01 0.15449999570847E+01 0.12494428760126E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.59064253313974E-03 0.00000000000000E+00
 0.13804395321884E-01 0.00000000000000E+00 0.12494428760126E-01 0.14395037855023E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1402.31996417
 0.72339199863804E+00 0.31202849312794E+03 0.51059472306435E+03 0.46271431575244E+03 0.44266785801113E+03
 0.22981833448971E+00 0.00000000000000E+00 0.19425309513251E+00 0.00000000000000E+00 -.19230687903003E+01
 0.43141717510449E-02 0.44333159656961E+00 0.18543536191071E+04 0.69538260716518E+03 0.18045183474180E+02
 0.67669438028176E+01 0.37859257423118E+03 0.29520931361583E+03 0.37174514008384E+03 0.41714243746322E+03
 0.29516548395090E+03 0.29517701415381E+03 0.36322172666873E+03 0.41698514704654E+03 0.29516291942987E+03
 0.29517695673854E+03 0.37174514008384E+03 0.41714243746322E+03 0.29516548395090E+03 0.29517701415381E+03
 0.36322172666873E+03 0.41698514704654E+03 0.29516291942987E+03 0.29517695673854E+03 0.46327062908259E+03
 0.35018733024273E+03 0.26687129309667E+04 0.22132118246392E+04 0.55357053887395E+03 0.97830084058827E+03
 0.42196244901995E+03 0.17131713470001E+04 0.14014921929640E+04 0.13908740140370E+04 0.21050971668592E+04
 0.15288931963888E+04 0.13979056752025E+04 0.12645189919791E+04 0.21029472754104E+04 0.17131713470001E+04
 0.14014921929640E+04 0.13908740140370E+04 0.21050971668592E+04 0.15288931963888E+04 0.13979056752025E+04
 0.12645189919791E+04 0.21029472754104E+04 0.19179847832099E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53562201169038E+03 0.18908944566945E+01
 0.18908944566945E+01 0.10859295425265E+02 0.00000000000000E+00 0.32023886792782E+03 0.32023886792782E+03
 0.32023886792782E+03 0.32023886792782E+03 0.00000000000000E+00 0.00000000000000E+00 0.21746563360197E+00
 0.00000000000000E+00 0.97121347941681E+01 0.10000000000000E-02 0.22771254041432E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35132013306970E+02 0.13174504990114E+02 0.35019147522947E+03 0.46326629240603E+03
 0.30278172585022E+03 0.30278172585022E+03 0.29515042261085E+03 0.29515040596944E+03 0.30270983091869E+03
 0.30270983091869E+03 0.29515042055089E+03 0.29515040399060E+03 0.30278172585022E+03 0.30278172585022E+03
 0.29515042261085E+03 0.29515040596944E+03 0.30270983091869E+03 0.30270983091869E+03 0.29515042055089E+03
 0.29515040399060E+03 0.30248556487083E+03 0.29515134421274E+03 -.29054911850142E+03 -.44796456701017E+03
 0.16056364907131E+03 0.28537688931163E+03 0.12401042199496E+03 0.17874519113060E+03 0.11756065634669E+03
 0.17874519113060E+03 0.20539825986700E+03 0.17914809544818E+03 0.11552186633638E+03 0.17914809544818E+03
 0.20380452140668E+03 0.17874519113060E+03 0.11756065634669E+03 0.17874519113060E+03 0.20539825986700E+03
 0.17914809544818E+03 0.11552186633638E+03 0.17914809544818E+03 0.20380452140667E+03 0.22407186697212E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32979560109361E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94124285463476E+00 0.92973879494105E+00 0.00000000000000E+00 0.94124285463476E+00 0.92973879494105E+00
 0.13916959707088E+01 0.36169599931902E+00 0.10299999713898E+01 0.15449999570847E+01 0.12493173848438E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.59448020382422E-03 0.00000000000000E+00
 0.13782901550652E-01 0.00000000000000E+00 0.12493173848438E-01 0.14377381754476E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1410.59288786
 0.72353314053584E+00 0.31208818758720E+03 0.51074527483323E+03 0.46283361275829E+03 0.44277120474084E+03
 0.22981776021461E+00 0.00000000000000E+00 0.19425659976840E+00 0.00000000000000E+00 -.19244050201416E+01
 0.43263775794132E-02 0.44315935553955E+00 0.18491220086909E+04 0.69342075325910E+03 0.18052197025740E+02
 0.67695738846525E+01 0.37883602640869E+03 0.29521144586835E+03 0.37197307191711E+03 0.41744065812608E+03
 0.29516612350022E+03 0.29517812554075E+03 0.36344017404438E+03 0.41728329777698E+03 0.29516345697776E+03
 0.29517806592539E+03 0.37197307191711E+03 0.41744065812608E+03 0.29516612350022E+03 0.29517812554075E+03
 0.36344017404438E+03 0.41728329777698E+03 0.29516345697776E+03 0.29517806592539E+03 0.46350455873451E+03
 0.35042522388691E+03 0.26694360231653E+04 0.22123602347581E+04 0.55248535181933E+03 0.97625291045063E+03
 0.42100513187220E+03 0.17138850056173E+04 0.14008651438808E+04 0.13904602401557E+04 0.21030079139293E+04
 0.15298638959396E+04 0.13972879293447E+04 0.12644661130068E+04 0.21008671264920E+04 0.17138850056173E+04
 0.14008651438808E+04 0.13904602401557E+04 0.21030079139293E+04 0.15298638959396E+04 0.13972879293447E+04
 0.12644661130068E+04 0.21008671264920E+04 0.19162787833175E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53575546885585E+03 0.18908944696824E+01
 0.18908944696824E+01 0.10925478814782E+02 0.00000000000000E+00 0.32036454379510E+03 0.32036454379510E+03
 0.32036454379510E+03 0.32036454379510E+03 0.00000000000000E+00 0.00000000000000E+00 0.21740508725180E+00
 0.00000000000000E+00 0.97159410604429E+01 0.10000000000000E-02 0.22887250965586E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34953957607356E+02 0.13107734102758E+02 0.35042934247314E+03 0.46350025673107E+03
 0.30284572023717E+03 0.30284572023717E+03 0.29515044196195E+03 0.29515042455854E+03 0.30277347714638E+03
 0.30277347714638E+03 0.29515043978309E+03 0.29515042246549E+03 0.30284572023717E+03 0.30284572023717E+03
 0.29515044196195E+03 0.29515042455854E+03 0.30277347714638E+03 0.30277347714638E+03 0.29515043978309E+03
 0.29515042246549E+03 0.30254511681833E+03 0.29515140095182E+03 -.29196683140769E+03 -.45011680155299E+03
 0.16127010392441E+03 0.28662862704661E+03 0.12455217260258E+03 0.17968996184491E+03 0.11810008655347E+03
 0.17968996184491E+03 0.20630379394996E+03 0.18009507419601E+03 0.11605562872337E+03 0.18009507419601E+03
 0.20470685719647E+03 0.17968996184491E+03 0.11810008655347E+03 0.17968996184491E+03 0.20630379394996E+03
 0.18009507419601E+03 0.11605562872336E+03 0.18009507419601E+03 0.20470685719647E+03 0.22462343893039E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32992471878814E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94134348266240E+00 0.92986347286097E+00 0.00000000000000E+00 0.94134348266240E+00 0.92986347286097E+00
 0.13917665416577E+01 0.36176657026792E+00 0.10299999713898E+01 0.15449999570847E+01 0.12491793831720E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.59827098284776E-03 0.00000000000000E+00
 0.13762006396576E-01 0.00000000000000E+00 0.12491793831720E-01 0.14360277379424E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1427.13873524
 0.72381353472117E+00 0.31220699131528E+03 0.51104414286345E+03 0.46307046903146E+03 0.44297645518383E+03
 0.22981659994747E+00 0.00000000000000E+00 0.19426353613976E+00 0.00000000000000E+00 -.19270583890193E+01
 0.43509410345415E-02 0.44281745595070E+00 0.18386826979472E+04 0.68950601173020E+03 0.18066135136485E+02
 0.67748006761820E+01 0.37932005699806E+03 0.29521587556734E+03 0.37242636211074E+03 0.41803260452193E+03
 0.29516746240508E+03 0.29518045133349E+03 0.36387481943420E+03 0.41787511145863E+03 0.29516458287611E+03
 0.29518038713278E+03 0.37242636211074E+03 0.41803260452193E+03 0.29516746240508E+03 0.29518045133349E+03
 0.36387481943420E+03 0.41787511145863E+03 0.29516458287611E+03 0.29518038713278E+03 0.46396764321035E+03
 0.35089486571218E+03 0.26708597480589E+04 0.22106518512061E+04 0.55035107287707E+03 0.97222330783646E+03
 0.41912047959501E+03 0.17152964506233E+04 0.13996053085376E+04 0.13896283884211E+04 0.20988491689314E+04
 0.15317862145201E+04 0.13960466390584E+04 0.12643500829967E+04 0.20967264735409E+04 0.17152964506233E+04
 0.13996053085376E+04 0.13896283884211E+04 0.20988491689314E+04 0.15317862145201E+04 0.13960466390584E+04
 0.12643500829968E+04 0.20967264735409E+04 0.19128900368340E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53602042686167E+03 0.18908944954726E+01
 0.18908944954726E+01 0.11057845593817E+02 0.00000000000000E+00 0.32061312710068E+03 0.32061312710068E+03
 0.32061312710068E+03 0.32061312710068E+03 0.00000000000000E+00 0.00000000000000E+00 0.21728444610366E+00
 0.00000000000000E+00 0.97235398173619E+01 0.10000000000000E-02 0.23118245155635E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34604702675929E+02 0.12976763503473E+02 0.35089893204376E+03 0.46396341008840E+03
 0.30297357436470E+03 0.30297357436470E+03 0.29515048281872E+03 0.29515046380647E+03 0.30290063339157E+03
 0.30290063339157E+03 0.29515048038471E+03 0.29515046146831E+03 0.30297357436470E+03 0.30297357436470E+03
 0.29515048281872E+03 0.29515046380647E+03 0.30290063339157E+03 0.30290063339157E+03 0.29515048038471E+03
 0.29515046146831E+03 0.30266417821948E+03 0.29515152015664E+03 -.29476544440677E+03 -.45436347435582E+03
 0.16266064996879E+03 0.28908757087465E+03 0.12561361765601E+03 0.18155840098199E+03 0.11916353620669E+03
 0.18155840098199E+03 0.20808396547980E+03 0.18196794604579E+03 0.11710814207438E+03 0.18196794604579E+03
 0.20648103146443E+03 0.18155840098199E+03 0.11916353620669E+03 0.18155840098199E+03 0.20808396547980E+03
 0.18196794604578E+03 0.11710814207437E+03 0.18196794604578E+03 0.20648103146443E+03 0.22566620726614E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33018000164890E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94154365594796E+00 0.93010959608041E+00 0.00000000000000E+00 0.94154365594796E+00 0.93010959608041E+00
 0.13919067387504E+01 0.36190676736058E+00 0.10299999713898E+01 0.15449999570847E+01 0.12488719549632E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.60571702579450E-03 0.00000000000000E+00
 0.13721799341758E-01 0.00000000000000E+00 0.12488719549632E-01 0.14327516367552E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1435.41165893
 0.72395275372428E+00 0.31226610820458E+03 0.51119247854994E+03 0.46318804734997E+03 0.44307837683335E+03
 0.22981601523818E+00 0.00000000000000E+00 0.19426696800575E+00 0.00000000000000E+00 -.19283761355887E+01
 0.43632792846383E-02 0.44264783896282E+00 0.18334833683843E+04 0.68755626314410E+03 0.18073057848300E+02
 0.67773966931125E+01 0.37956065117845E+03 0.29521817454468E+03 0.37265173411159E+03 0.41832636243506E+03
 0.29516816253146E+03 0.29518166703618E+03 0.36409102790123E+03 0.41816880653737E+03 0.29516517189436E+03
 0.29518160044827E+03 0.37265173411159E+03 0.41832636243506E+03 0.29516816253146E+03 0.29518166703618E+03
 0.36409102790123E+03 0.41816880653737E+03 0.29516517189436E+03 0.29518160044827E+03 0.46419683956488E+03
 0.35112667973195E+03 0.26715607688912E+04 0.22097954087950E+04 0.54930181144845E+03 0.97024118868026E+03
 0.41819286817457E+03 0.17159944763258E+04 0.13989727315568E+04 0.13892105307361E+04 0.20967797223360E+04
 0.15327380868980E+04 0.13954233043868E+04 0.12642871877140E+04 0.20946660159372E+04 0.17159944763258E+04
 0.13989727315568E+04 0.13892105307361E+04 0.20967797223360E+04 0.15327380868980E+04 0.13954233043868E+04
 0.12642871877140E+04 0.20946660159373E+04 0.19112073243487E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53615194612361E+03 0.18908945082808E+01
 0.18908945082808E+01 0.11124028983334E+02 0.00000000000000E+00 0.32073610364167E+03 0.32073610364167E+03
 0.32073610364167E+03 0.32073610364167E+03 0.00000000000000E+00 0.00000000000000E+00 0.21722435184461E+00
 0.00000000000000E+00 0.97273292451148E+01 0.10000000000000E-02 0.23233238918256E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34433425439076E+02 0.12912534539654E+02 0.35113072023198E+03 0.46419264062111E+03
 0.30303742856154E+03 0.30303742856154E+03 0.29515050436394E+03 0.29515048450329E+03 0.30296413833648E+03
 0.30296413833648E+03 0.29515050179325E+03 0.29515048203383E+03 0.30303742856154E+03 0.30303742856154E+03
 0.29515050436394E+03 0.29515048450329E+03 0.30296413833648E+03 0.30296413833648E+03 0.29515050179325E+03
 0.29515048203383E+03 0.30272367720024E+03 0.29515158271452E+03 -.29614664546590E+03 -.45645812969808E+03
 0.16334509857568E+03 0.29029578531661E+03 0.12613396124805E+03 0.18248231195987E+03 0.11968780095284E+03
 0.18248231195987E+03 0.20895928987472E+03 0.18289407900824E+03 0.11762713080016E+03 0.18289407900824E+03
 0.20735354683258E+03 0.18248231195987E+03 0.11968780095284E+03 0.18248231195987E+03 0.20895928987472E+03
 0.18289407900824E+03 0.11762713080016E+03 0.18289407900824E+03 0.20735354683257E+03 0.22616135145806E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33030624669815E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94164316700911E+00 0.93023119109344E+00 0.00000000000000E+00 0.94164316700911E+00 0.93023119109344E+00
 0.13919763482519E+01 0.36197637686214E+00 0.10299999713898E+01 0.15449999570847E+01 0.12487051014643E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.60937442164997E-03 0.00000000000000E+00
 0.13702397382770E-01 0.00000000000000E+00 0.12487051014643E-01 0.14311771804420E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1443.68458262
 0.72409131340348E+00 0.31232503428812E+03 0.51134009202572E+03 0.46330506717762E+03 0.44317983562783E+03
 0.22981542824306E+00 0.00000000000000E+00 0.19427037580008E+00 0.00000000000000E+00 -.19296880737631E+01
 0.43756429933499E-02 0.44247913097202E+00 0.18283027230874E+04 0.68561352115778E+03 0.18079948725324E+02
 0.67799807719965E+01 0.37980030895928E+03 0.29522053062363E+03 0.37287626703777E+03 0.41861866500133E+03
 0.29516888362365E+03 0.29518291881110E+03 0.36430650115933E+03 0.41846104858301E+03 0.29516577873981E+03
 0.29518284977165E+03 0.37287626703777E+03 0.41861866500133E+03 0.29516888362365E+03 0.29518291881110E+03
 0.36430650115933E+03 0.41846104858301E+03 0.29516577873981E+03 0.29518284977165E+03 0.46442449931174E+03
 0.35135653062660E+03 0.26722547564203E+04 0.22089375726627E+04 0.54826429979563E+03 0.96828047708163E+03
 0.41727485578702E+03 0.17166874950099E+04 0.13983384580782E+04 0.13887914483685E+04 0.20947168344177E+04
 0.15336839026302E+04 0.13947982532884E+04 0.12642211288088E+04 0.20926120795120E+04 0.17166874950099E+04
 0.13983384580782E+04 0.13887914483685E+04 0.20947168344177E+04 0.15336839026302E+04 0.13947982532884E+04
 0.12642211288088E+04 0.20926120795120E+04 0.19095323032975E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53628283413280E+03 0.18908945210325E+01
 0.18908945210325E+01 0.11190212372851E+02 0.00000000000000E+00 0.32085824374318E+03 0.32085824374318E+03
 0.32085824374318E+03 0.32085824374318E+03 0.00000000000000E+00 0.00000000000000E+00 0.21716440870551E+00
 0.00000000000000E+00 0.97311102944380E+01 0.10000000000000E-02 0.23347895381302E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34264330336202E+02 0.12849123876076E+02 0.35136054547381E+03 0.46442033438679E+03
 0.30310123102545E+03 0.30310123102545E+03 0.29515052668067E+03 0.29515050594124E+03 0.30302759160960E+03
 0.30302759160960E+03 0.29515052396692E+03 0.29515050333435E+03 0.30310123102545E+03 0.30310123102545E+03
 0.29515052668067E+03 0.29515050594124E+03 0.30302759160960E+03 0.30302759160960E+03 0.29515052396692E+03
 0.29515050333435E+03 0.30278314903792E+03 0.29515164730520E+03 -.29751596752826E+03 -.45853382492407E+03
 0.16402254654575E+03 0.29149042150373E+03 0.12664776222526E+03 0.18339949755460E+03 0.12020723944647E+03
 0.18339949755460E+03 0.20982517691272E+03 0.18381348879990E+03 0.11814141679548E+03 0.18381348879990E+03
 0.20821674524126E+03 0.18339949755460E+03 0.12020723944647E+03 0.18339949755460E+03 0.20982517691272E+03
 0.18381348879990E+03 0.11814141679548E+03 0.18381348879990E+03 0.20821674524126E+03 0.22664124452360E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33043160716567E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94174227796481E+00 0.93035186321841E+00 0.00000000000000E+00 0.94174227796481E+00 0.93035186321841E+00
 0.13920456280915E+01 0.36204565670174E+00 0.10299999713898E+01 0.15449999570847E+01 0.12485309403765E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.61298972612884E-03 0.00000000000000E+00
 0.13683411455684E-01 0.00000000000000E+00 0.12485309403765E-01 0.14296401181812E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1457.03556593
 0.72431320191835E+00 0.31241822087122E+03 0.51157583038423E+03 0.46349166843997E+03 0.44334134360262E+03
 0.22981447493318E+00 0.00000000000000E+00 0.19427582410613E+00 0.00000000000000E+00 -.19317937796156E+01
 0.43956953355209E-02 0.44220966333951E+00 0.18199623471066E+04 0.68248588016496E+03 0.18090966035398E+02
 0.67841122632743E+01 0.38018580511732E+03 0.29522448481082E+03 0.37323737080506E+03 0.41908999402784E+03
 0.29517010357835E+03 0.29518503564657E+03 0.36465273668552E+03 0.41893227369866E+03 0.29516680593011E+03
 0.29518496247914E+03 0.37323737080506E+03 0.41908999402784E+03 0.29517010357835E+03 0.29518503564657E+03
 0.36465273668552E+03 0.41893227369866E+03 0.29516680593011E+03 0.29518496247914E+03 0.46479449548756E+03
 0.35172972057590E+03 0.26733610123772E+04 0.22075301579859E+04 0.54649002605368E+03 0.96496488667967E+03
 0.41574241049572E+03 0.17177978809572E+04 0.13972749611650E+04 0.13881000901419E+04 0.20913325071614E+04
 0.15352053391773E+04 0.13937496729811E+04 0.12641045395882E+04 0.20892422825225E+04 0.17177978809572E+04
 0.13972749611650E+04 0.13881000901419E+04 0.20913325071614E+04 0.15352053391773E+04 0.13937496729811E+04
 0.12641045395882E+04 0.20892422825225E+04 0.19067607454149E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53649169313106E+03 0.18908945414995E+01
 0.18908945414995E+01 0.11297020239348E+02 0.00000000000000E+00 0.32105373563433E+03 0.32105373563433E+03
 0.32105373563433E+03 0.32105373563433E+03 0.00000000000000E+00 0.00000000000000E+00 0.21706799186253E+00
 0.00000000000000E+00 0.97371725425841E+01 0.10000000000000E-02 0.23532207603847E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33995960492429E+02 0.12748485184661E+02 0.35173367633913E+03 0.46479040138617E+03
 0.30320279543277E+03 0.30320279543277E+03 0.29515057121833E+03 0.29515054255261E+03 0.30312859493444E+03
 0.30312859493444E+03 0.29515056822244E+03 0.29515053970706E+03 0.30320279543277E+03 0.30320279543277E+03
 0.29515057121833E+03 0.29515054255261E+03 0.30312859493444E+03 0.30312859493444E+03 0.29515056822244E+03
 0.29515053970706E+03 0.30287785960628E+03 0.29515175703882E+03 -.29976491647644E+03 -.46195106552901E+03
 0.16512360034215E+03 0.29342367267535E+03 0.12747445433149E+03 0.18489391823916E+03 0.12104966106540E+03
 0.18489391823916E+03 0.21122569433148E+03 0.18531148432401E+03 0.11897522722133E+03 0.18531148432401E+03
 0.20961261523813E+03 0.18489391823916E+03 0.12104966106539E+03 0.18489391823916E+03 0.21122569433148E+03
 0.18531148432401E+03 0.11897522722133E+03 0.18531148432401E+03 0.20961261523813E+03 0.22727564523362E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33063200464689E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94188964382319E+00 0.93055693314657E+00 0.00000000000000E+00 0.94188964382319E+00 0.93055693314657E+00
 0.13921565723489E+01 0.36215660095918E+00 0.10299999713898E+01 0.15449999570847E+01 0.12481985246973E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.61889783765490E-03 0.00000000000000E+00
 0.13654508816302E-01 0.00000000000000E+00 0.12481985246973E-01 0.14273406653957E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1470.00000000
 0.72452596227059E+00 0.31250787866040E+03 0.51180285044037E+03 0.46367139003885E+03 0.44349688389376E+03
 0.22981354400560E+00 0.00000000000000E+00 0.19428105036692E+00 0.00000000000000E+00 -.19337555999518E+01
 0.44152321154541E-02 0.44195049091291E+00 0.18119092701828E+04 0.67946597631853E+03 0.18101575096059E+02
 0.67880906610220E+01 0.38055748103011E+03 0.29522847063287E+03 0.37358572933908E+03 0.41954253748091E+03
 0.29517134275314E+03 0.29518718489698E+03 0.36498720128716E+03 0.41938472963776E+03 0.29516784981189E+03
 0.29518710755550E+03 0.37358572933908E+03 0.41954253748091E+03 0.29517134275314E+03 0.29518718489698E+03
 0.36498720128716E+03 0.41938472963775E+03 0.29516784981189E+03 0.29518710755550E+03 0.46514556914477E+03
 0.35208301071913E+03 0.26744073600806E+04 0.22061490835106E+04 0.54487965106491E+03 0.96192728926459E+03
 0.41432323994435E+03 0.17188559409488E+04 0.13962468466454E+04 0.13874166849487E+04 0.20880841440215E+04
 0.15366568932446E+04 0.13927360104047E+04 0.12639695735018E+04 0.20860078887465E+04 0.17188559409489E+04
 0.13962468466454E+04 0.13874166849487E+04 0.20880841440215E+04 0.15366568932446E+04 0.13927360104047E+04
 0.12639695735018E+04 0.20860078887466E+04 0.19041252547009E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53669281921378E+03 0.18908945605680E+01
 0.18908945605680E+01 0.11400735711909E+02 0.00000000000000E+00 0.32124173778192E+03 0.32124173778192E+03
 0.32124173778192E+03 0.32124173778192E+03 0.00000000000000E+00 0.00000000000000E+00 0.21697474537514E+00
 0.00000000000000E+00 0.97430632331849E+01 0.10000000000000E-02 0.23710327333705E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33740571723899E+02 0.12652714396462E+02 0.35208692586040E+03 0.46514152727366E+03
 0.30330201517175E+03 0.30330201517175E+03 0.29515061026503E+03 0.29515058008267E+03 0.30322726772548E+03
 0.30322726772548E+03 0.29515060700993E+03 0.29515057698857E+03 0.30330201517175E+03 0.30330201517175E+03
 0.29515061026503E+03 0.29515058008267E+03 0.30322726772548E+03 0.30322726772548E+03 0.29515060700993E+03
 0.29515057698857E+03 0.30297042746099E+03 0.29515186896631E+03 -.30187443787674E+03 -.46514635841932E+03
 0.16616257379519E+03 0.29525078079078E+03 0.12825739412662E+03 0.18631120890824E+03 0.12184856082007E+03
 0.18631120890824E+03 0.21255214694537E+03 0.18673226806269E+03 0.11976645792650E+03 0.18673226806269E+03
 0.21093525105277E+03 0.18631120890824E+03 0.12184856082007E+03 0.18631120890824E+03 0.21255214694537E+03
 0.18673226806269E+03 0.11976645792650E+03 0.18673226806269E+03 0.21093525105277E+03 0.22796677945650E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33082476513699E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94204919291753E+00 0.93072749233043E+00 0.00000000000000E+00 0.94204919291753E+00 0.93072749233043E+00
 0.13922629525251E+01 0.36226298113530E+00 0.10299999713898E+01 0.15449999570847E+01 0.12478859830423E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.62453777982846E-03 0.00000000000000E+00
 0.13626578190179E-01 0.00000000000000E+00 0.12478859830423E-01 0.14251115970007E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1480.00000000
 0.72468856283330E+00 0.31257818412921E+03 0.51197780770117E+03 0.46381026548922E+03 0.44361743399931E+03
 0.22981282643520E+00 0.00000000000000E+00 0.19428503545819E+00 0.00000000000000E+00 -.19352864842207E+01
 0.44302476736404E-02 0.44175134910303E+00 0.18057681171189E+04 0.67716304391958E+03 0.18109735298475E+02
 0.67911507369281E+01 0.38084211734842E+03 0.29523163331542E+03 0.37385264435404E+03 0.41988753777497E+03
 0.29517233172409E+03 0.29518889961325E+03 0.36524384007009E+03 0.41972967262797E+03 0.29516868322972E+03
 0.29518881895193E+03 0.37385264435404E+03 0.41988753777497E+03 0.29517233172409E+03 0.29518889961325E+03
 0.36524384007009E+03 0.41972967262797E+03 0.29516868322972E+03 0.29518881895193E+03 0.46541027305461E+03
 0.35234856240646E+03 0.26752090586694E+04 0.22051068185469E+04 0.54374100114958E+03 0.95975201983280E+03
 0.41329231367748E+03 0.17196662561146E+04 0.13954814959150E+04 0.13869038924697E+04 0.20856438595127E+04
 0.15377656656741E+04 0.13919817284521E+04 0.12638708051143E+04 0.20835782235602E+04 0.17196662561147E+04
 0.13954814959150E+04 0.13869038924698E+04 0.20856438595126E+04 0.15377656656741E+04 0.13919817284521E+04
 0.12638708051143E+04 0.20835782235603E+04 0.19021698135075E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53684802928414E+03 0.18908945754478E+01
 0.18908945754478E+01 0.11480735711909E+02 0.00000000000000E+00 0.32138550297921E+03 0.32138550297921E+03
 0.32138550297921E+03 0.32138550297921E+03 0.00000000000000E+00 0.00000000000000E+00 0.21690307425593E+00
 0.00000000000000E+00 0.97475864742378E+01 0.10000000000000E-02 0.23847148741410E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33546987469023E+02 0.12580120300884E+02 0.35235246087688E+03 0.46540625803102E+03
 0.30337919674992E+03 0.30337919674992E+03 0.29515063793748E+03 0.29515061024548E+03 0.30330402754579E+03
 0.30330402754579E+03 0.29515063449165E+03 0.29515060694923E+03 0.30337919674992E+03 0.30337919674992E+03
 0.29515063793748E+03 0.29515061024548E+03 0.30330402754579E+03 0.30330402754579E+03 0.29515063449165E+03
 0.29515060694923E+03 0.30304245709084E+03 0.29515195858399E+03 -.30344406162392E+03 -.46751492155691E+03
 0.16693965580205E+03 0.29662037214643E+03 0.12884601806537E+03 0.18737648225011E+03 0.12244864287702E+03
 0.18737648225011E+03 0.21354790197643E+03 0.18780024281427E+03 0.12036113799056E+03 0.18780024281427E+03
 0.21192856665133E+03 0.18737648225011E+03 0.12244864287702E+03 0.18737648225011E+03 0.21354790197643E+03
 0.18780024281427E+03 0.12036113799056E+03 0.18780024281427E+03 0.21192856665133E+03 0.22855358495400E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33097232605570E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94217639687915E+00 0.93085644537680E+00 0.00000000000000E+00 0.94217639687915E+00 0.93085644537680E+00
 0.13923442528064E+01 0.36234428141665E+00 0.10299999713898E+01 0.15449999570847E+01 0.12476723800608E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.62867592045825E-03 0.00000000000000E+00
 0.13604684313588E-01 0.00000000000000E+00 0.12476723800608E-01 0.14233360234046E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1490.00000000
 0.72485095561739E+00 0.31264902742302E+03 0.51215224125625E+03 0.46394887619100E+03 0.44373794785493E+03
 0.22981211207612E+00 0.00000000000000E+00 0.19428898322709E+00 0.00000000000000E+00 -.19368755051307E+01
 0.44451715616120E-02 0.44155319468576E+00 0.17997055657170E+04 0.67488958714387E+03 0.18117862346559E+02
 0.67941983799596E+01 0.38112553099360E+03 0.29523488557434E+03 0.37411841516332E+03 0.42023107645675E+03
 0.29517335453755E+03 0.29519067240810E+03 0.36549936419250E+03 0.42007315401042E+03 0.29516954548363E+03
 0.29519058832500E+03 0.37411841516331E+03 0.42023107645675E+03 0.29517335453755E+03 0.29519067240810E+03
 0.36549936419250E+03 0.42007315401041E+03 0.29516954548363E+03 0.29519058832500E+03 0.46567476981873E+03
 0.35261314750404E+03 0.26760115152131E+04 0.22040804775365E+04 0.54258929546883E+03 0.95756157681400E+03
 0.41225933486783E+03 0.17204766013076E+04 0.13947187181546E+04 0.13864023794698E+04 0.20832166578242E+04
 0.15388740160119E+04 0.13912299652543E+04 0.12637820840096E+04 0.20811615912274E+04 0.17204766013077E+04
 0.13947187181546E+04 0.13864023794698E+04 0.20832166578242E+04 0.15388740160119E+04 0.13912299652544E+04
 0.12637820840095E+04 0.20811615912275E+04 0.19002222269363E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53700287911005E+03 0.18908945908928E+01
 0.18908945908928E+01 0.11560735711909E+02 0.00000000000000E+00 0.32152815892917E+03 0.32152815892917E+03
 0.32152815892917E+03 0.32152815892917E+03 0.00000000000000E+00 0.00000000000000E+00 0.21683162296037E+00
 0.00000000000000E+00 0.97520660974752E+01 0.10000000000000E-02 0.23983480217526E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33356293279547E+02 0.12508609979830E+02 0.35261702290516E+03 0.46567078882187E+03
 0.30345603296491E+03 0.30345603296491E+03 0.29515067077801E+03 0.29515064166045E+03 0.30338044494890E+03
 0.30338044494890E+03 0.29515066710952E+03 0.29515063815121E+03 0.30345603296491E+03 0.30345603296491E+03
 0.29515067077801E+03 0.29515064166045E+03 0.30338044494890E+03 0.30338044494890E+03 0.29515066710952E+03
 0.29515063815121E+03 0.30311418047223E+03 0.29515205157423E+03 -.30501571763475E+03 -.46988740036524E+03
 0.16771226306375E+03 0.29797906073306E+03 0.12942823635399E+03 0.18843892490911E+03 0.12304464954511E+03
 0.18843892490911E+03 0.21453514843012E+03 0.18886537251175E+03 0.12095173492063E+03 0.18886537251175E+03
 0.21291334571986E+03 0.18843892490911E+03 0.12304464954511E+03 0.18843892490911E+03 0.21453514843012E+03
 0.18886537251175E+03 0.12095173492063E+03 0.18886537251175E+03 0.21291334571986E+03 0.22907926692079E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33111871923051E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94229038583648E+00 0.93100545818938E+00 0.00000000000000E+00 0.94229038583648E+00 0.93100545818938E+00
 0.13924254491985E+01 0.36242547780870E+00 0.10299999713898E+01 0.15449999570847E+01 0.12474466828002E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.63270840242785E-03 0.00000000000000E+00
 0.13583474747496E-01 0.00000000000000E+00 0.12474466828002E-01 0.14216183149924E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1500.00000000
 0.72501332132422E+00 0.31271937937460E+03 0.51232549442236E+03 0.46408646361323E+03 0.44385753630728E+03
 0.22981140006477E+00 0.00000000000000E+00 0.19429290066873E+00 0.00000000000000E+00 -.19384409896996E+01
 0.44600350270962E-02 0.44135651823803E+00 0.17937078860137E+04 0.67264045725515E+03 0.18125935993734E+02
 0.67972259976504E+01 0.38140774130678E+03 0.29523822871428E+03 0.37438310668518E+03 0.42057285484211E+03
 0.29517441189226E+03 0.29519250444712E+03 0.36575391992183E+03 0.42041487756592E+03 0.29517043718203E+03
 0.29519241683861E+03 0.37438310668518E+03 0.42057285484211E+03 0.29517441189226E+03 0.29519250444712E+03
 0.36575391992183E+03 0.42041487756592E+03 0.29517043718203E+03 0.29519241683861E+03 0.46593782676756E+03
 0.35287558673189E+03 0.26768028961408E+04 0.22030478557973E+04 0.54143837474034E+03 0.95537601583766E+03
 0.41123044922362E+03 0.17212795520324E+04 0.13939492234556E+04 0.13858964900436E+04 0.20807902480957E+04
 0.15399738892949E+04 0.13904714461066E+04 0.12636869936300E+04 0.20787456930177E+04 0.17212795520324E+04
 0.13939492234556E+04 0.13858964900436E+04 0.20807902480957E+04 0.15399738892949E+04 0.13904714461066E+04
 0.12636869936300E+04 0.20787456930178E+04 0.18982740438746E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53715664979858E+03 0.18908946061089E+01
 0.18908946061089E+01 0.11640735711909E+02 0.00000000000000E+00 0.32166973942057E+03 0.32166973942057E+03
 0.32166973942057E+03 0.32166973942057E+03 0.00000000000000E+00 0.00000000000000E+00 0.21676039141283E+00
 0.00000000000000E+00 0.97565389867153E+01 0.10000000000000E-02 0.24119321412688E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33168429008089E+02 0.12438160878033E+02 0.35287943223900E+03 0.46593388675540E+03
 0.30353271359913E+03 0.30353271359913E+03 0.29515070496782E+03 0.29515067436612E+03 0.30345670845487E+03
 0.30345670845487E+03 0.29515070106487E+03 0.29515067063259E+03 0.30353271359913E+03 0.30353271359913E+03
 0.29515070496782E+03 0.29515067436612E+03 0.30345670845487E+03 0.30345670845487E+03 0.29515070106487E+03
 0.29515067063259E+03 0.30318577499947E+03 0.29515214802750E+03 -.30657555983289E+03 -.47224146174101E+03
 0.16847737591556E+03 0.29932251564457E+03 0.13000275284943E+03 0.18949444143486E+03 0.12363543808032E+03
 0.18949444143486E+03 0.21551173563703E+03 0.18992356975632E+03 0.12153723327820E+03 0.18992356975632E+03
 0.21388757153230E+03 0.18949444143486E+03 0.12363543808032E+03 0.18949444143486E+03 0.21551173563703E+03
 0.18992356975632E+03 0.12153723327820E+03 0.18992356975632E+03 0.21388757153230E+03 0.22958073935394E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33126394712135E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94240519348219E+00 0.93114940615779E+00 0.00000000000000E+00 0.94240519348219E+00 0.93114940615779E+00
 0.13925066320519E+01 0.36250666066211E+00 0.10299999713898E+01 0.15449999570847E+01 0.12472072546021E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.63671225459664E-03 0.00000000000000E+00
 0.13562901938636E-01 0.00000000000000E+00 0.12472072546021E-01 0.14199614193232E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1510.00000000
 0.72517506740565E+00 0.31278893157142E+03 0.51249741083987E+03 0.46422287420154E+03 0.44397600888440E+03
 0.22981068859663E+00 0.00000000000000E+00 0.19429679315632E+00 0.00000000000000E+00 -.19399787067207E+01
 0.44748765769845E-02 0.44116139437451E+00 0.17877588045995E+04 0.67040955172480E+03 0.18133953020397E+02
 0.68002323826487E+01 0.38168875387091E+03 0.29524166403288E+03 0.37464673174111E+03 0.42091280877553E+03
 0.29517550448880E+03 0.29519439689777E+03 0.36600753492786E+03 0.42075477961567E+03 0.29517135893541E+03
 0.29519430565851E+03 0.37464673174111E+03 0.42091280877553E+03 0.29517550448880E+03 0.29519439689777E+03
 0.36600753492786E+03 0.42075477961567E+03 0.29517135893541E+03 0.29519430565851E+03 0.46619899656338E+03
 0.35313562395882E+03 0.26775803149138E+04 0.22020029575506E+04 0.54029475429106E+03 0.95320495424307E+03
 0.41020872618056E+03 0.17220731746441E+04 0.13931703831945E+04 0.13853818458650E+04 0.20783611839560E+04
 0.15410632436005E+04 0.13897035589298E+04 0.12635810726941E+04 0.20763270945785E+04 0.17220731746441E+04
 0.13931703831945E+04 0.13853818458650E+04 0.20783611839560E+04 0.15410632436005E+04 0.13897035589298E+04
 0.12635810726941E+04 0.20763270945786E+04 0.18963240150719E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53730917299863E+03 0.18908946210552E+01
 0.18908946210552E+01 0.11720735711909E+02 0.00000000000000E+00 0.32181029820099E+03 0.32181029820099E+03
 0.32181029820099E+03 0.32181029820099E+03 0.00000000000000E+00 0.00000000000000E+00 0.21668937990562E+00
 0.00000000000000E+00 0.97609913457475E+01 0.10000000000000E-02 0.24254669646821E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32983339358937E+02 0.12368752259601E+02 0.35313943712454E+03 0.46619509935765E+03
 0.30360927184062E+03 0.30360927184062E+03 0.29515074054799E+03 0.29515070840180E+03 0.30353285089038E+03
 0.30353285089038E+03 0.29515073639830E+03 0.29515070443224E+03 0.30360927184062E+03 0.30360927184062E+03
 0.29515074054799E+03 0.29515070840180E+03 0.30353285089038E+03 0.30353285089038E+03 0.29515073639830E+03
 0.29515070443224E+03 0.30325727032255E+03 0.29515224803554E+03 -.30812107950555E+03 -.47457242302267E+03
 0.16923418235996E+03 0.30065012846877E+03 0.13056977519701E+03 0.19054183955265E+03 0.12422046108273E+03
 0.19054183955265E+03 0.21647724682059E+03 0.19097364447726E+03 0.12211710566667E+03 0.19097364447726E+03
 0.21485084967997E+03 0.19054183955265E+03 0.12422046108273E+03 0.19054183955265E+03 0.21647724682059E+03
 0.19097364447726E+03 0.12211710566667E+03 0.19097364447726E+03 0.21485084967997E+03 0.23006655696366E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33140808402911E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94252030755144E+00 0.93128853609498E+00 0.00000000000000E+00 0.94252030755144E+00 0.93128853609498E+00
 0.13925875050926E+01 0.36258753370282E+00 0.10299999713898E+01 0.15449999570847E+01 0.12469603925371E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.64070451539248E-03 0.00000000000000E+00
 0.13542747912989E-01 0.00000000000000E+00 0.12469603925371E-01 0.14183452428382E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1520.00000000
 0.72533565556388E+00 0.31285796058068E+03 0.51266818419796E+03 0.46435835768603E+03 0.44409365479110E+03
 0.22980997705100E+00 0.00000000000000E+00 0.19430066110018E+00 0.00000000000000E+00 -.19415006889825E+01
 0.44897042435876E-02 0.44096765496864E+00 0.17818545645687E+04 0.66819546171326E+03 0.18141920183622E+02
 0.68032200688582E+01 0.38196852186570E+03 0.29524519281945E+03 0.37490924312554E+03 0.42125085385866E+03
 0.29517663302930E+03 0.29519635092853E+03 0.36626016818573E+03 0.42109277585833E+03 0.29517231135603E+03
 0.29519625595153E+03 0.37490924312554E+03 0.42125085385866E+03 0.29517663302930E+03 0.29519635092853E+03
 0.36626016818573E+03 0.42109277585832E+03 0.29517231135603E+03 0.29519625595153E+03 0.46645808756899E+03
 0.35339316494486E+03 0.26783463205414E+04 0.22009515125513E+04 0.53916529309222E+03 0.95105927692966E+03
 0.40919815737198E+03 0.17228588357002E+04 0.13923860889591E+04 0.13848622319272E+04 0.20759357662892E+04
 0.15421432901261E+04 0.13889301972303E+04 0.12634677919440E+04 0.20739120987600E+04 0.17228588357002E+04
 0.13923860889591E+04 0.13848622319272E+04 0.20759357662892E+04 0.15421432901261E+04 0.13889301972304E+04
 0.12634677919440E+04 0.20739120987600E+04 0.18943796038246E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53746066640385E+03 0.18908946358485E+01
 0.18908946358485E+01 0.11800735711909E+02 0.00000000000000E+00 0.32194987701282E+03 0.32194987701282E+03
 0.32194987701282E+03 0.32194987701282E+03 0.00000000000000E+00 0.00000000000000E+00 0.21661858852407E+00
 0.00000000000000E+00 0.97654217500711E+01 0.10000000000000E-02 0.24389523487391E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32800968842773E+02 0.12300363316040E+02 0.35339694583432E+03 0.46645423224720E+03
 0.30368572475168E+03 0.30368572475168E+03 0.29515077756037E+03 0.29515074380752E+03 0.30360888943529E+03
 0.30360888943529E+03 0.29515077315118E+03 0.29515073958973E+03 0.30368572475168E+03 0.30368572475168E+03
 0.29515077756037E+03 0.29515074380752E+03 0.30360888943529E+03 0.30360888943529E+03 0.29515077315118E+03
 0.29515073958973E+03 0.30332868073224E+03 0.29515235169137E+03 -.30965122286472E+03 -.47687825759085E+03
 0.16998241097445E+03 0.30196186558750E+03 0.13112954255818E+03 0.19158064086676E+03 0.12479953569221E+03
 0.19158064086676E+03 0.21743165452527E+03 0.19201511766902E+03 0.12269117517238E+03 0.19201511766902E+03
 0.21580315805760E+03 0.19158064086676E+03 0.12479953569221E+03 0.19158064086676E+03 0.21743165452527E+03
 0.19201511766902E+03 0.12269117517238E+03 0.19201511766902E+03 0.21580315805760E+03 0.23054142700657E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33155119357779E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94263528896312E+00 0.93142509878129E+00 0.00000000000000E+00 0.94263528896312E+00 0.93142509878129E+00
 0.13926677991717E+01 0.36266782778194E+00 0.10299999713898E+01 0.15449999570847E+01 0.12467096861452E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.64466428541175E-03 0.00000000000000E+00
 0.13522922492100E-01 0.00000000000000E+00 0.12467096861452E-01 0.14167586777512E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1530.00000000
 0.72549499571906E+00 0.31292675235358E+03 0.51283800346087E+03 0.46449313270545E+03 0.44421072855225E+03
 0.22980926599247E+00 0.00000000000000E+00 0.19430450171817E+00 0.00000000000000E+00 -.19430171886779E+01
 0.45045037630983E-02 0.44077517439301E+00 0.17760002923158E+04 0.66600010961841E+03 0.18149842515556E+02
 0.68061909433337E+01 0.38224701974494E+03 0.29524881635443E+03 0.37517061318465E+03 0.42158695809277E+03
 0.29517779821715E+03 0.29519836770851E+03 0.36651179310713E+03 0.42142883427015E+03 0.29517329505774E+03
 0.29519826888507E+03 0.37517061318465E+03 0.42158695809277E+03 0.29517779821715E+03 0.29519836770851E+03
 0.36651179310713E+03 0.42142883427014E+03 0.29517329505774E+03 0.29519826888507E+03 0.46671511560000E+03
 0.35364821116802E+03 0.26791035721298E+04 0.21998992753206E+04 0.53805273052842E+03 0.94894349038243E+03
 0.40820049620136E+03 0.17236380969605E+04 0.13915999741958E+04 0.13843415863035E+04 0.20735195284130E+04
 0.15432154984489E+04 0.13881549888365E+04 0.12633508725497E+04 0.20715062342170E+04 0.17236380969606E+04
 0.13915999741958E+04 0.13843415863036E+04 0.20735195284130E+04 0.15432154984489E+04 0.13881549888365E+04
 0.12633508725497E+04 0.20715062342171E+04 0.18924463587780E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53761133624648E+03 0.18908946505886E+01
 0.18908946505886E+01 0.11880735711909E+02 0.00000000000000E+00 0.32208850020457E+03 0.32208850020457E+03
 0.32208850020457E+03 0.32208850020457E+03 0.00000000000000E+00 0.00000000000000E+00 0.21654801704175E+00
 0.00000000000000E+00 0.97698317048794E+01 0.10000000000000E-02 0.24523883393074E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32621260963341E+02 0.12232972861253E+02 0.35365196052157E+03 0.46671130088843E+03
 0.30376207945637E+03 0.30376207945637E+03 0.29515081604761E+03 0.29515078062408E+03 0.30368483145449E+03
 0.30368483145449E+03 0.29515081136566E+03 0.29515077614537E+03 0.30376207945637E+03 0.30376207945637E+03
 0.29515081604761E+03 0.29515078062408E+03 0.30368483145449E+03 0.30368483145449E+03 0.29515081136566E+03
 0.29515077614537E+03 0.30340001136645E+03 0.29515245908929E+03 -.31116582090794E+03 -.47915860647334E+03
 0.17072208206742E+03 0.30325790700773E+03 0.13168221452997E+03 0.19261077823361E+03 0.12537267018853E+03
 0.19261077823361E+03 0.21837506962302E+03 0.19304792082004E+03 0.12325944875881E+03 0.19304792082004E+03
 0.21674460477901E+03 0.19261077823361E+03 0.12537267018853E+03 0.19261077823361E+03 0.21837506962302E+03
 0.19304792082004E+03 0.12325944875881E+03 0.19304792082004E+03 0.21674460477901E+03 0.23100697233875E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33169331203574E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94274971407408E+00 0.93156091352914E+00 0.00000000000000E+00 0.94274971407408E+00 0.93156091352914E+00
 0.13927474692493E+01 0.36274749785953E+00 0.10299999713898E+01 0.15449999570847E+01 0.12464563842519E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.64857212049774E-03 0.00000000000000E+00
 0.13503406972446E-01 0.00000000000000E+00 0.12464563842519E-01 0.14151979092944E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1540.00000000
 0.72565326068981E+00 0.31299535966912E+03 0.51300692014418E+03 0.46462723979939E+03 0.44432727140506E+03
 0.22980855619314E+00 0.00000000000000E+00 0.19430831263804E+00 0.00000000000000E+00 -.19445296336300E+01
 0.45192603082770E-02 0.44058393963927E+00 0.17702011953921E+04 0.66382544827203E+03 0.18157720425647E+02
 0.68091451596176E+01 0.38252425816954E+03 0.29525253590904E+03 0.37543085091479E+03 0.42192114394427E+03
 0.29517900075678E+03 0.29520044840697E+03 0.36676241684124E+03 0.42176297726858E+03 0.29517431065574E+03
 0.29520034562675E+03 0.37543085091479E+03 0.42192114394427E+03 0.29517900075678E+03 0.29520044840697E+03
 0.36676241684124E+03 0.42176297726857E+03 0.29517431065574E+03 0.29520034562675E+03 0.46697016242574E+03
 0.35390080359881E+03 0.26798527757311E+04 0.21988474045385E+04 0.53695642971965E+03 0.94685659050574E+03
 0.40721537863750E+03 0.17244114277747E+04 0.13908128902873E+04 0.13838207377303E+04 0.20711135344009E+04
 0.15442803362844E+04 0.13873787810774E+04 0.12632311350706E+04 0.20691105615213E+04 0.17244114277747E+04
 0.13908128902873E+04 0.13838207377303E+04 0.20711135344009E+04 0.15442803362844E+04 0.13873787810774E+04
 0.12632311350706E+04 0.20691105615213E+04 0.18905248922036E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53776123092556E+03 0.18908946652892E+01
 0.18908946652892E+01 0.11960735711909E+02 0.00000000000000E+00 0.32222618423285E+03 0.32222618423285E+03
 0.32222618423285E+03 0.32222618423285E+03 0.00000000000000E+00 0.00000000000000E+00 0.21647766508053E+00
 0.00000000000000E+00 0.97742215109336E+01 0.10000000000000E-02 0.24657750688590E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32444159652007E+02 0.12166559869502E+02 0.35390452204879E+03 0.46696638756450E+03
 0.30383833608271E+03 0.30383833608271E+03 0.29515085605313E+03 0.29515081889301E+03 0.30376067723868E+03
 0.30376067723868E+03 0.29515085108466E+03 0.29515081414022E+03 0.30383833608271E+03 0.30383833608271E+03
 0.29515085605313E+03 0.29515081889301E+03 0.30376067723868E+03 0.30376067723868E+03 0.29515085108466E+03
 0.29515081414022E+03 0.30347126109510E+03 0.29515257032487E+03 -.31266508916544E+03 -.48141382472431E+03
 0.17145334209863E+03 0.30453853727546E+03 0.13222792846633E+03 0.19363236434771E+03 0.12593995751690E+03
 0.19363236434771E+03 0.21930767708135E+03 0.19407216566552E+03 0.12382201565333E+03 0.19407216566552E+03
 0.21767537011158E+03 0.19363236434771E+03 0.12593995751690E+03 0.19363236434771E+03 0.21930767708135E+03
 0.19407216566552E+03 0.12382201565333E+03 0.19407216566552E+03 0.21767537011157E+03 0.23146357300308E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33183445894423E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94286343829656E+00 0.93169627129486E+00 0.00000000000000E+00 0.94286343829656E+00 0.93169627129486E+00
 0.13928266017347E+01 0.36282663034490E+00 0.10299999713898E+01 0.15449999570847E+01 0.12462008162304E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.65242546073378E-03 0.00000000000000E+00
 0.13484191868966E-01 0.00000000000000E+00 0.12462008162304E-01 0.14136617329699E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1550.00000000
 0.72581059404413E+00 0.31306370925519E+03 0.51317490935414E+03 0.46476063301457E+03 0.44444322205339E+03
 0.22980784806365E+00 0.00000000000000E+00 0.19431209352532E+00 0.00000000000000E+00 -.19460352818959E+01
 0.45339675843487E-02 0.44039397756190E+00 0.17644590198695E+04 0.66167213245107E+03 0.18165552681464E+02
 0.68120822555488E+01 0.38280026011251E+03 0.29525635274493E+03 0.37568997821197E+03 0.42225344788436E+03
 0.29518024135342E+03 0.29520259419296E+03 0.36701205892131E+03 0.42209524128439E+03 0.29517535876643E+03
 0.29520248734396E+03 0.37568997821197E+03 0.42225344788436E+03 0.29518024135342E+03 0.29520259419296E+03
 0.36701205892131E+03 0.42209524128439E+03 0.29517535876643E+03 0.29520248734396E+03 0.46722327822677E+03
 0.35415098339662E+03 0.26805935129171E+04 0.21977945649479E+04 0.53587520135049E+03 0.94479656465517E+03
 0.40624198729793E+03 0.17251786197455E+04 0.13900241773064E+04 0.13832987853244E+04 0.20687165474041E+04
 0.15453376290457E+04 0.13866009137559E+04 0.12631077614728E+04 0.20667238437429E+04 0.17251786197456E+04
 0.13900241773064E+04 0.13832987853244E+04 0.20687165474041E+04 0.15453376290457E+04 0.13866009137559E+04
 0.12631077614728E+04 0.20667238437429E+04 0.18886136886164E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53791031516653E+03 0.18908946799238E+01
 0.18908946799238E+01 0.12040735711909E+02 0.00000000000000E+00 0.32236294540838E+03 0.32236294540838E+03
 0.32236294540838E+03 0.32236294540838E+03 0.00000000000000E+00 0.00000000000000E+00 0.21640753224791E+00
 0.00000000000000E+00 0.97785903630438E+01 0.10000000000000E-02 0.24791126702248E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32269610397637E+02 0.12101103899114E+02 0.35415467133556E+03 0.46721954285619E+03
 0.30391449242398E+03 0.30391449242398E+03 0.29515089762114E+03 0.29515085865661E+03 0.30383642465697E+03
 0.30383642465697E+03 0.29515089235188E+03 0.29515085361608E+03 0.30391449242398E+03 0.30391449242398E+03
 0.29515089762114E+03 0.29515085865661E+03 0.30383642465697E+03 0.30383642465697E+03 0.29515089235188E+03
 0.29515085361608E+03 0.30354242681312E+03 0.29515268549489E+03 -.31414925880445E+03 -.48364428556921E+03
 0.17217634395202E+03 0.30580406033762E+03 0.13276683466585E+03 0.19464552134986E+03 0.12650149702027E+03
 0.19464552134986E+03 0.22022968046328E+03 0.19508797387091E+03 0.12437897183703E+03 0.19508797387091E+03
 0.21859565382373E+03 0.19464552134986E+03 0.12650149702027E+03 0.19464552134986E+03 0.22022968046328E+03
 0.19508797387090E+03 0.12437897183703E+03 0.19508797387090E+03 0.21859565382373E+03 0.23191166886657E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33197465057665E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94297650371851E+00 0.93183074265677E+00 0.00000000000000E+00 0.94297650371851E+00 0.93183074265677E+00
 0.13929052684118E+01 0.36290529702207E+00 0.10299999713898E+01 0.15449999570847E+01 0.12459432305938E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.65623050127876E-03 0.00000000000000E+00
 0.13465259800622E-01 0.00000000000000E+00 0.12459432305938E-01 0.14121490301901E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1560.00000000
 0.72596701836530E+00 0.31313175259130E+03 0.51334195220138E+03 0.46489328498229E+03 0.44455854455767E+03
 0.22980714172377E+00 0.00000000000000E+00 0.19431584531808E+00 0.00000000000000E+00 -.19475325127836E+01
 0.45486243511195E-02 0.44020529341245E+00 0.17587735065506E+04 0.65954006495649E+03 0.18173338939167E+02
 0.68150021021877E+01 0.38307504224140E+03 0.29526026811385E+03 0.37594801075412E+03 0.42258389562990E+03
 0.29518152071289E+03 0.29520480623488E+03 0.36726073318592E+03 0.42242565200360E+03 0.29517644000720E+03
 0.29520469520346E+03 0.37594801075412E+03 0.42258389562990E+03 0.29518152071289E+03 0.29520480623488E+03
 0.36726073318592E+03 0.42242565200359E+03 0.29517644000720E+03 0.29520469520346E+03 0.46747447498822E+03
 0.35439878268147E+03 0.26813254472671E+04 0.21967398041155E+04 0.53480868531347E+03 0.94276270400336E+03
 0.40527997526332E+03 0.17259394821147E+04 0.13892333104719E+04 0.13827750718812E+04 0.20663276191643E+04
 0.15463872093649E+04 0.13858208633318E+04 0.12629801528216E+04 0.20643451340299E+04 0.17259394821147E+04
 0.13892333104718E+04 0.13827750718812E+04 0.20663276191643E+04 0.15463872093649E+04 0.13858208633318E+04
 0.12629801528216E+04 0.20643451340299E+04 0.18867118207748E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53805856508158E+03 0.18908946944765E+01
 0.18908946944765E+01 0.12120735711909E+02 0.00000000000000E+00 0.32249880117025E+03 0.32249880117025E+03
 0.32249880117025E+03 0.32249880117025E+03 0.00000000000000E+00 0.00000000000000E+00 0.21633761816651E+00
 0.00000000000000E+00 0.97829374639915E+01 0.10000000000000E-02 0.24924012608025E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32097560396131E+02 0.12036585148549E+02 0.35440244034090E+03 0.46747077885322E+03
 0.30399054661433E+03 0.30399054661433E+03 0.29515094079664E+03 0.29515089995792E+03 0.30391207189160E+03
 0.30391207189160E+03 0.29515093521178E+03 0.29515089461548E+03 0.30399054661433E+03 0.30399054661433E+03
 0.29515094079664E+03 0.29515089995792E+03 0.30391207189160E+03 0.30391207189160E+03 0.29515093521178E+03
 0.29515089461548E+03 0.30361350584474E+03 0.29515280469741E+03 -.31561848266177E+03 -.48585021367904E+03
 0.17289121374749E+03 0.30705476089590E+03 0.13329909107966E+03 0.19565033523390E+03 0.12705737126077E+03
 0.19565033523390E+03 0.22114127257130E+03 0.19609543111444E+03 0.12493039724701E+03 0.19609543111444E+03
 0.21950564584251E+03 0.19565033523390E+03 0.12705737126077E+03 0.19565033523390E+03 0.22114127257130E+03
 0.19609543111444E+03 0.12493039724701E+03 0.19609543111444E+03 0.21950564584251E+03 0.23235197895979E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33211390523099E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94308897016430E+00 0.93196406360046E+00 0.00000000000000E+00 0.94308897016430E+00 0.93196406360046E+00
 0.13929834805724E+01 0.36298350918265E+00 0.10299999713898E+01 0.15449999570847E+01 0.12456840365867E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.65999153972179E-03 0.00000000000000E+00
 0.13446590356908E-01 0.00000000000000E+00 0.12456840365867E-01 0.14106581896629E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1570.00000000
 0.72612250182343E+00 0.31319950002848E+03 0.51350805880561E+03 0.46502520819364E+03 0.44467325263100E+03
 0.22980643725782E+00 0.00000000000000E+00 0.19431956891401E+00 0.00000000000000E+00 -.19490217744012E+01
 0.45632291665986E-02 0.44001786741806E+00 0.17531444746535E+04 0.65742917799505E+03 0.18181079888738E+02
 0.68179049582769E+01 0.38334861385794E+03 0.29526428325734E+03 0.37620495667206E+03 0.42291250304003E+03
 0.29518283954139E+03 0.29520708570015E+03 0.36750844629454E+03 0.42275422525644E+03 0.29517755499622E+03
 0.29520697037103E+03 0.37620495667206E+03 0.42291250304003E+03 0.29518283954139E+03 0.29520708570015E+03
 0.36750844629454E+03 0.42275422525644E+03 0.29517755499622E+03 0.29520697037103E+03 0.46772375796525E+03
 0.35464423019191E+03 0.26820486741959E+04 0.21956832550084E+04 0.53375709008816E+03 0.94075524492845E+03
 0.40432936938985E+03 0.17266940679832E+04 0.13884403465609E+04 0.13822496834031E+04 0.20639467268508E+04
 0.15474291372478E+04 0.13850386872251E+04 0.12628484100682E+04 0.20619744104712E+04 0.17266940679832E+04
 0.13884403465609E+04 0.13822496834031E+04 0.20639467268508E+04 0.15474291372478E+04 0.13850386872251E+04
 0.12628484100682E+04 0.20619744104712E+04 0.18848193829886E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53820599160209E+03 0.18908947089518E+01
 0.18908947089518E+01 0.12200735711909E+02 0.00000000000000E+00 0.32263376835008E+03 0.32263376835008E+03
 0.32263376835008E+03 0.32263376835008E+03 0.00000000000000E+00 0.00000000000000E+00 0.21626792244952E+00
 0.00000000000000E+00 0.97872625209022E+01 0.10000000000000E-02 0.25056409609267E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31927958254008E+02 0.11972984345253E+02 0.35464785771671E+03 0.46772010079740E+03
 0.30406649744557E+03 0.30406649744557E+03 0.29515098562543E+03 0.29515094284074E+03 0.30398761778978E+03
 0.30398761778978E+03 0.29515097970961E+03 0.29515093718172E+03 0.30406649744557E+03 0.30406649744557E+03
 0.29515098562543E+03 0.29515094284074E+03 0.30398761778978E+03 0.30398761778978E+03 0.29515097970961E+03
 0.29515093718172E+03 0.30368449619570E+03 0.29515292803166E+03 -.31707288533798E+03 -.48803178750880E+03
 0.17359806432594E+03 0.30829090133927E+03 0.13382484669170E+03 0.19664687704223E+03 0.12760765370699E+03
 0.19664687704223E+03 0.22204263049968E+03 0.19709460808358E+03 0.12547636292857E+03 0.19709460808358E+03
 0.22040552053938E+03 0.19664687704223E+03 0.12760765370699E+03 0.19664687704223E+03 0.22204263049968E+03
 0.19709460808358E+03 0.12547636292856E+03 0.19709460808358E+03 0.22040552053937E+03 0.23278523273978E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33225224177422E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94320084773235E+00 0.93209630366411E+00 0.00000000000000E+00 0.94320084773235E+00 0.93209630366411E+00
 0.13930612223015E+01 0.36306125091171E+00 0.10299999713898E+01 0.15449999570847E+01 0.12454236692216E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.66370872658173E-03 0.00000000000000E+00
 0.13428167181263E-01 0.00000000000000E+00 0.12454236692216E-01 0.14091875907844E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1580.00000000
 0.72627703083289E+00 0.31326697937519E+03 0.51367325126896E+03 0.46515642723186E+03 0.44478737382137E+03
 0.22980573481919E+00 0.00000000000000E+00 0.19432326468591E+00 0.00000000000000E+00 -.19505040551106E+01
 0.45777790358738E-02 0.43983167557871E+00 0.17475723352543E+04 0.65533962572035E+03 0.18188776398321E+02
 0.68207911493704E+01 0.38362098295060E+03 0.29526839940645E+03 0.37646082265573E+03 0.42323928503744E+03
 0.29518419854525E+03 0.29520943375478E+03 0.36775520343287E+03 0.42308097593337E+03 0.29517870435229E+03
 0.29520931401107E+03 0.37646082265573E+03 0.42323928503744E+03 0.29518419854525E+03 0.29520943375478E+03
 0.36775520343287E+03 0.42308097593337E+03 0.29517870435229E+03 0.29520931401107E+03 0.46797114406109E+03
 0.35488735620510E+03 0.26827634715047E+04 0.21946254198154E+04 0.53272057579150E+03 0.93877437569293E+03
 0.40339019702247E+03 0.17274425441662E+04 0.13876455788632E+04 0.13817229638759E+04 0.20615741962184E+04
 0.15484635826760E+04 0.13842546784778E+04 0.12627128815004E+04 0.20596119989182E+04 0.17274425441662E+04
 0.13876455788632E+04 0.13817229638759E+04 0.20615741962184E+04 0.15484635826760E+04 0.13842546784778E+04
 0.12627128815004E+04 0.20596119989182E+04 0.18829367391792E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53835261814034E+03 0.18908947233593E+01
 0.18908947233593E+01 0.12280735711909E+02 0.00000000000000E+00 0.32276786201609E+03 0.32276786201609E+03
 0.32276786201609E+03 0.32276786201609E+03 0.00000000000000E+00 0.00000000000000E+00 0.21619844468122E+00
 0.00000000000000E+00 0.97915655534599E+01 0.10000000000000E-02 0.25188319069774E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31760753775745E+02 0.11910282665904E+02 0.35489095368576E+03 0.46796752560296E+03
 0.30414234383657E+03 0.30414234383657E+03 0.29515103215408E+03 0.29515098734964E+03 0.30406306133304E+03
 0.30406306133304E+03 0.29515102589140E+03 0.29515098135881E+03 0.30414234383657E+03 0.30414234383657E+03
 0.29515103215408E+03 0.29515098734964E+03 0.30406306133304E+03 0.30406306133304E+03 0.29515102589140E+03
 0.29515098135881E+03 0.30375539605740E+03 0.29515305559812E+03 -.31851261029149E+03 -.49018922915853E+03
 0.17429700967744E+03 0.30951272932795E+03 0.13434423460212E+03 0.19763522414944E+03 0.12815241796337E+03
 0.19763522414944E+03 0.22293392020469E+03 0.19808558175533E+03 0.12601693993093E+03 0.19808558175533E+03
 0.22129544095926E+03 0.19763522414944E+03 0.12815241796337E+03 0.19763522414944E+03 0.22293392020469E+03
 0.19808558175533E+03 0.12601693993093E+03 0.19808558175533E+03 0.22129544095925E+03 0.23321198592617E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33238967734453E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94331211562946E+00 0.93222762806789E+00 0.00000000000000E+00 0.94331211562946E+00 0.93222762806789E+00
 0.13931384868062E+01 0.36313851541645E+00 0.10299999713898E+01 0.15449999570847E+01 0.12451624643856E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.66738101914821E-03 0.00000000000000E+00
 0.13409978636293E-01 0.00000000000000E+00 0.12451624643856E-01 0.14077359655441E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1590.00000000
 0.72643062288513E+00 0.31333420276495E+03 0.51383754641392E+03 0.46528695680810E+03 0.44490092247741E+03
 0.22980503458579E+00 0.00000000000000E+00 0.19432693276686E+00 0.00000000000000E+00 -.19519797673704E+01
 0.45922707237648E-02 0.43964670110271E+00 0.17420575748290E+04 0.65327159056088E+03 0.18196429041625E+02
 0.68236608906093E+01 0.38389215947428E+03 0.29527261778141E+03 0.37671561736546E+03 0.42356425956641E+03
 0.29518559843074E+03 0.29521185156301E+03 0.36800101159986E+03 0.42340592194269E+03 0.29517988869463E+03
 0.29521172728620E+03 0.37671561736546E+03 0.42356425956641E+03 0.29518559843074E+03 0.29521185156301E+03
 0.36800101159986E+03 0.42340592194269E+03 0.29517988869463E+03 0.29521172728620E+03 0.46821665905871E+03
 0.35512819275701E+03 0.26834700404904E+04 0.21935665498463E+04 0.53169904345447E+03 0.93681987483105E+03
 0.40246233615931E+03 0.17281850397156E+04 0.13868491958602E+04 0.13811950872008E+04 0.20592101737469E+04
 0.15494906825334E+04 0.13834690250801E+04 0.12625737574672E+04 0.20572580457059E+04 0.17281850397156E+04
 0.13868491958602E+04 0.13811950872008E+04 0.20592101737469E+04 0.15494906825334E+04 0.13834690250801E+04
 0.12625737574672E+04 0.20572580457059E+04 0.18810639846433E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53849846055796E+03 0.18908947377029E+01
 0.18908947377029E+01 0.12360735711909E+02 0.00000000000000E+00 0.32290109563246E+03 0.32290109563246E+03
 0.32290109563246E+03 0.32290109563246E+03 0.00000000000000E+00 0.00000000000000E+00 0.21612918441796E+00
 0.00000000000000E+00 0.97958465983724E+01 0.10000000000000E-02 0.25319742502391E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31595897941081E+02 0.11848461727905E+02 0.35513176024385E+03 0.46821307909799E+03
 0.30421808451664E+03 0.30421808451664E+03 0.29515108042996E+03 0.29515103352992E+03 0.30413840130981E+03
 0.30413840130981E+03 0.29515107380397E+03 0.29515102719156E+03 0.30421808451664E+03 0.30421808451664E+03
 0.29515108042996E+03 0.29515103352992E+03 0.30413840130981E+03 0.30413840130981E+03 0.29515107380397E+03
 0.29515102719156E+03 0.30382620351863E+03 0.29515318749844E+03 -.31993782302931E+03 -.49232280788776E+03
 0.17498816672475E+03 0.31072048261921E+03 0.13485737506084E+03 0.19861546228246E+03 0.12869173922718E+03
 0.19861546228246E+03 0.22381530048012E+03 0.19906843747746E+03 0.12655220079693E+03 0.19906843747746E+03
 0.22217556289827E+03 0.19861546228246E+03 0.12869173922718E+03 0.19861546228246E+03 0.22381530048012E+03
 0.19906843747746E+03 0.12655220079692E+03 0.19906843747746E+03 0.22217556289826E+03 0.23363263569763E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33252622686424E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94342275676955E+00 0.93235811544334E+00 0.00000000000000E+00 0.94342275676955E+00 0.93235811544334E+00
 0.13932152828323E+01 0.36321531144257E+00 0.10299999713898E+01 0.15449999570847E+01 0.12449006552952E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.67100846734975E-03 0.00000000000000E+00
 0.13392015252426E-01 0.00000000000000E+00 0.12449006552952E-01 0.14063023719775E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1600.00000000
 0.72658330436259E+00 0.31340116716791E+03 0.51400095300547E+03 0.46541680125604E+03 0.44501390022980E+03
 0.22980433669843E+00 0.00000000000000E+00 0.19433057341946E+00 0.00000000000000E+00 -.19534487968114E+01
 0.46067019000746E-02 0.43946293230036E+00 0.17366003213428E+04 0.65122512050355E+03 0.18204038183890E+02
 0.68265143189587E+01 0.38416215469173E+03 0.29527693959142E+03 0.37696935082865E+03 0.42388744622641E+03
 0.29518703990383E+03 0.29521434028695E+03 0.36824587910332E+03 0.42372908284647E+03 0.29518110864269E+03
 0.29521421135694E+03 0.37696935082865E+03 0.42388744622641E+03 0.29518703990383E+03 0.29521434028695E+03
 0.36824587910332E+03 0.42372908284647E+03 0.29518110864269E+03 0.29521421135694E+03 0.46846032881962E+03
 0.35536677194348E+03 0.26841684632422E+04 0.21925066131148E+04 0.53069226332490E+03 0.93489130572307E+03
 0.40154558108155E+03 0.17289216145768E+04 0.13860512297982E+04 0.13806660327029E+04 0.20568545649236E+04
 0.15505105086442E+04 0.13826817590083E+04 0.12624310443296E+04 0.20549124563778E+04 0.17289216145769E+04
 0.13860512297982E+04 0.13806660327029E+04 0.20568545649236E+04 0.15505105086442E+04 0.13826817590083E+04
 0.12624310443296E+04 0.20549124563778E+04 0.18792009566097E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53864352535235E+03 0.18908947519815E+01
 0.18908947519815E+01 0.12440735711909E+02 0.00000000000000E+00 0.32303348168212E+03 0.32303348168212E+03
 0.32303348168212E+03 0.32303348168212E+03 0.00000000000000E+00 0.00000000000000E+00 0.21606014119790E+00
 0.00000000000000E+00 0.98001056232146E+01 0.10000000000000E-02 0.25450681502950E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31433342950257E+02 0.11787503606347E+02 0.35537030946069E+03 0.46845678718094E+03
 0.30429371805742E+03 0.30429371805742E+03 0.29515113050124E+03 0.29515108142767E+03 0.30421363634189E+03
 0.30421363634189E+03 0.29515112349490E+03 0.29515107472547E+03 0.30429371805742E+03 0.30429371805742E+03
 0.29515113050124E+03 0.29515108142767E+03 0.30421363634189E+03 0.30421363634189E+03 0.29515112349490E+03
 0.29515107472547E+03 0.30389691659262E+03 0.29515332383546E+03 -.32134869501485E+03 -.49443280751720E+03
 0.17567165117768E+03 0.31191439002850E+03 0.13536438059493E+03 0.19958767875300E+03 0.12922569184823E+03
 0.19958767875300E+03 0.22468692423016E+03 0.20004326223203E+03 0.12708221726123E+03 0.20004326223203E+03
 0.22304603635291E+03 0.19958767875300E+03 0.12922569184823E+03 0.19958767875300E+03 0.22468692423016E+03
 0.20004326223202E+03 0.12708221726123E+03 0.20004326223202E+03 0.22304603635291E+03 0.23404750526490E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33266190376478E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94353277037928E+00 0.93248775701504E+00 0.00000000000000E+00 0.94353277037928E+00 0.93248775701504E+00
 0.13932916235711E+01 0.36329165218130E+00 0.10299999713898E+01 0.15449999570847E+01 0.12446384217807E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.67459215059237E-03 0.00000000000000E+00
 0.13374268145319E-01 0.00000000000000E+00 0.12446384217807E-01 0.14048860295912E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1610.00000000
 0.72673509286434E+00 0.31346786803465E+03 0.51416347702394E+03 0.46554596301184E+03 0.44512630709617E+03
 0.22980364124940E+00 0.00000000000000E+00 0.19433418710869E+00 0.00000000000000E+00 -.19549109780177E+01
 0.46210711550211E-02 0.43928035720038E+00 0.17312003497950E+04 0.64920013117312E+03 0.18211604204171E+02
 0.68293515765640E+01 0.38443097961780E+03 0.29528136603433E+03 0.37722203287024E+03 0.42420886406734E+03
 0.29518852367001E+03 0.29521690108618E+03 0.36848981409608E+03 0.42405047765791E+03 0.29518236481602E+03
 0.29521676738129E+03 0.37722203287024E+03 0.42420886406734E+03 0.29518852367001E+03 0.29521690108618E+03
 0.36848981409608E+03 0.42405047765791E+03 0.29518236481602E+03 0.29521676738129E+03 0.46870217554596E+03
 0.35560312489768E+03 0.26848587816508E+04 0.21914455238265E+04 0.52970002935657E+03 0.93298826686341E+03
 0.40063973736006E+03 0.17296523022423E+04 0.13852516622842E+04 0.13801357423430E+04 0.20545072067155E+04
 0.15515231067896E+04 0.13818928618204E+04 0.12622847117594E+04 0.20525750681661E+04 0.17296523022423E+04
 0.13852516622842E+04 0.13801357423430E+04 0.20545072067155E+04 0.15515231067896E+04 0.13818928618204E+04
 0.12622847117594E+04 0.20525750681661E+04 0.18773474526445E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53878781660969E+03 0.18908947661936E+01
 0.18908947661936E+01 0.12520735711909E+02 0.00000000000000E+00 0.32316503204670E+03 0.32316503204670E+03
 0.32316503204670E+03 0.32316503204670E+03 0.00000000000000E+00 0.00000000000000E+00 0.21599131454801E+00
 0.00000000000000E+00 0.98043425865962E+01 0.10000000000000E-02 0.25581137707908E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31273042236613E+02 0.11727390838730E+02 0.35560663245776E+03 0.46869867206263E+03
 0.30436924300835E+03 0.30436924300835E+03 0.29515118241689E+03 0.29515113108972E+03 0.30428876502113E+03
 0.30428876502113E+03 0.29515117501258E+03 0.29515112400683E+03 0.30436924300835E+03 0.30436924300835E+03
 0.29515118241689E+03 0.29515113108972E+03 0.30428876502113E+03 0.30428876502113E+03 0.29515117501258E+03
 0.29515112400683E+03 0.30396753333328E+03 0.29515346471317E+03 -.32274539325657E+03 -.49651950600461E+03
 0.17634757477859E+03 0.31309467124413E+03 0.13586535859164E+03 0.20055195799166E+03 0.12975434760543E+03
 0.20055195799166E+03 0.22554893845744E+03 0.20101014016360E+03 0.12760705859149E+03 0.20101014016360E+03
 0.22390700556772E+03 0.20055195799166E+03 0.12975434760543E+03 0.20055195799166E+03 0.22554893845744E+03
 0.20101014016360E+03 0.12760705859149E+03 0.20101014016360E+03 0.22390700556771E+03 0.23445689247949E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33279672070698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94364216542040E+00 0.93261652897527E+00 0.00000000000000E+00 0.94364216542040E+00 0.93261652897527E+00
 0.13933675178219E+01 0.36336754643217E+00 0.10299999713898E+01 0.15449999570847E+01 0.12443759259551E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.67813322348772E-03 0.00000000000000E+00
 0.13356728937712E-01 0.00000000000000E+00 0.12443759259551E-01 0.14034862161199E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1620.00000000
 0.72688599631704E+00 0.31353430599114E+03 0.51432512583522E+03 0.46567444745732E+03 0.44523814719564E+03
 0.22980294830345E+00 0.00000000000000E+00 0.19433777436629E+00 0.00000000000000E+00 -.19563663277207E+01
 0.46353775103932E-02 0.43909896168135E+00 0.17258572752840E+04 0.64719647823151E+03 0.18219127572899E+02
 0.68321728398372E+01 0.38469864450427E+03 0.29528589829640E+03 0.37747367257151E+03 0.42452853096788E+03
 0.29519005043408E+03 0.29521953511743E+03 0.36873282405603E+03 0.42437012422018E+03 0.29518365783401E+03
 0.29521939651441E+03 0.37747367257150E+03 0.42452853096788E+03 0.29519005043408E+03 0.29521953511743E+03
 0.36873282405603E+03 0.42437012422018E+03 0.29518365783401E+03 0.29521939651441E+03 0.46894221917285E+03
 0.35583728179316E+03 0.26855410597361E+04 0.21903832766768E+04 0.52872219761156E+03 0.93111045862646E+03
 0.39974465002685E+03 0.17303771468059E+04 0.13844505054329E+04 0.13796042129038E+04 0.20521679903114E+04
 0.15525285320037E+04 0.13811023456218E+04 0.12621347803275E+04 0.20502457725620E+04 0.17303771468059E+04
 0.13844505054329E+04 0.13796042129038E+04 0.20521679903114E+04 0.15525285320037E+04 0.13811023456218E+04
 0.12621347803275E+04 0.20502457725620E+04 0.18755033479245E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53893134059387E+03 0.18908947803393E+01
 0.18908947803393E+01 0.12600735711909E+02 0.00000000000000E+00 0.32329575807825E+03 0.32329575807825E+03
 0.32329575807825E+03 0.32329575807825E+03 0.00000000000000E+00 0.00000000000000E+00 0.21592270398629E+00
 0.00000000000000E+00 0.98085575004691E+01 0.10000000000000E-02 0.25711112785387E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31114950437100E+02 0.11668106413912E+02 0.35584075940743E+03 0.46893875366879E+03
 0.30444465796878E+03 0.30444465796878E+03 0.29515123622666E+03 0.29515118256367E+03 0.30436378598437E+03
 0.30436378598437E+03 0.29515122840617E+03 0.29515117508267E+03 0.30444465796878E+03 0.30444465796878E+03
 0.29515123622666E+03 0.29515118256367E+03 0.30436378598437E+03 0.30436378598437E+03 0.29515122840617E+03
 0.29515117508267E+03 0.30403805189254E+03 0.29515361023671E+03 -.32412807966729E+03 -.49858317423614E+03
 0.17701604529785E+03 0.31426153708644E+03 0.13636041156210E+03 0.20150838135140E+03 0.13027777567870E+03
 0.20150838135140E+03 0.22640148430221E+03 0.20196915236677E+03 0.12812679154834E+03 0.20196915236677E+03
 0.22475860904510E+03 0.20150838135140E+03 0.13027777567870E+03 0.20150838135140E+03 0.22640148430222E+03
 0.20196915236677E+03 0.12812679154833E+03 0.20196915236677E+03 0.22475860904509E+03 0.23486107343883E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33293068981327E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94375095039238E+00 0.93274443355321E+00 0.00000000000000E+00 0.94375095039238E+00 0.93274443355321E+00
 0.13934429695483E+01 0.36344299815852E+00 0.10299999713898E+01 0.15449999570847E+01 0.12441133144429E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.68163246315840E-03 0.00000000000000E+00
 0.13339390217532E-01 0.00000000000000E+00 0.12441133144429E-01 0.14021022680690E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1630.00000000
 0.72703601993495E+00 0.31360048480894E+03 0.51448590857307E+03 0.46580226225427E+03 0.44534942740562E+03
 0.22980225791590E+00 0.00000000000000E+00 0.19434133566825E+00 0.00000000000000E+00 -.19578149757448E+01
 0.46496201125164E-02 0.43891873069180E+00 0.17205706716694E+04 0.64521400187603E+03 0.18226608801567E+02
 0.68349783005877E+01 0.38496515917702E+03 0.29529053755207E+03 0.37772427859726E+03 0.42484646414810E+03
 0.29519162089993E+03 0.29522224353423E+03 0.36897491608592E+03 0.42468803971862E+03 0.29518498831578E+03
 0.29522209990828E+03 0.37772427859726E+03 0.42484646414810E+03 0.29519162089993E+03 0.29522224353423E+03
 0.36897491608592E+03 0.42468803971862E+03 0.29518498831578E+03 0.29522209990828E+03 0.46918047958986E+03
 0.35606927217369E+03 0.26862153886242E+04 0.21893199286022E+04 0.52775864971006E+03 0.92925762633087E+03
 0.39886018337226E+03 0.17310962077464E+04 0.13836478079984E+04 0.13790714832978E+04 0.20498368651800E+04
 0.15535268537623E+04 0.13803102590968E+04 0.12619813102361E+04 0.20479245192758E+04 0.17310962077464E+04
 0.13836478079984E+04 0.13790714832978E+04 0.20498368651800E+04 0.15535268537623E+04 0.13803102590968E+04
 0.12619813102361E+04 0.20479245192758E+04 0.18736685762409E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53907410560737E+03 0.18908947944198E+01
 0.18908947944198E+01 0.12680735711909E+02 0.00000000000000E+00 0.32342567057666E+03 0.32342567057666E+03
 0.32342567057666E+03 0.32342567057666E+03 0.00000000000000E+00 0.00000000000000E+00 0.21585430902209E+00
 0.00000000000000E+00 0.98127504330690E+01 0.10000000000000E-02 0.25840608437541E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30959023350153E+02 0.11609633756308E+02 0.35607271985769E+03 0.46917705187768E+03
 0.30451996158923E+03 0.30451996158923E+03 0.29515129198109E+03 0.29515123589787E+03 0.30443869791630E+03
 0.30443869791630E+03 0.29515128372561E+03 0.29515122800076E+03 0.30451996158923E+03 0.30451996158923E+03
 0.29515129198109E+03 0.29515123589787E+03 0.30443869791630E+03 0.30443869791630E+03 0.29515128372561E+03
 0.29515122800076E+03 0.30410847051511E+03 0.29515376051236E+03 -.32549691384274E+03 -.50062408123397E+03
 0.17767716752782E+03 0.31541519032751E+03 0.13684963696205E+03 0.20245702843889E+03 0.13079604327693E+03
 0.20245702843889E+03 0.22724469749780E+03 0.20292037820885E+03 0.12864148098476E+03 0.20292037820885E+03
 0.22560097996243E+03 0.20245702843889E+03 0.13079604327693E+03 0.20245702843889E+03 0.22724469749780E+03
 0.20292037820885E+03 0.12864148098476E+03 0.20292037820885E+03 0.22560097996242E+03 0.23526029327544E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33306382264358E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94385913030505E+00 0.93287149199720E+00 0.00000000000000E+00 0.94385913030505E+00 0.93287149199720E+00
 0.13935179813572E+01 0.36351800996747E+00 0.10299999713898E+01 0.15449999570847E+01 0.12438507147643E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.68509041142672E-03 0.00000000000000E+00
 0.13322245517870E-01 0.00000000000000E+00 0.12438507147643E-01 0.14007335929297E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1640.00000000
 0.72718517093991E+00 0.31366640775597E+03 0.51464583472798E+03 0.46592941507530E+03 0.44546015438988E+03
 0.22980157013658E+00 0.00000000000000E+00 0.19434487141611E+00 0.00000000000000E+00 -.19592570379888E+01
 0.46637982120909E-02 0.43873964971972E+00 0.17153400803791E+04 0.64325253014217E+03 0.18234048381792E+02
 0.68377681431721E+01 0.38523053340348E+03 0.29529528496372E+03 0.37797385956590E+03 0.42516268067829E+03
 0.29519323577034E+03 0.29522502748652E+03 0.36921609726149E+03 0.42500424118899E+03 0.29518635687999E+03
 0.29522487871130E+03 0.37797385956590E+03 0.42516268067829E+03 0.29519323577034E+03 0.29522502748652E+03
 0.36921609726149E+03 0.42500424118899E+03 0.29518635687999E+03 0.29522487871130E+03 0.46941697732266E+03
 0.35629912512477E+03 0.26868818648816E+04 0.21882555371661E+04 0.52680925773084E+03 0.92742950288923E+03
 0.39798619886973E+03 0.17318095481928E+04 0.13828436273729E+04 0.13785375924391E+04 0.20475137939270E+04
 0.15545181451151E+04 0.13795166595199E+04 0.12618243616716E+04 0.20456112711051E+04 0.17318095481928E+04
 0.13828436273729E+04 0.13785375924391E+04 0.20475137939270E+04 0.15545181451151E+04 0.13795166595199E+04
 0.12618243616716E+04 0.20456112711051E+04 0.18718430742628E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53921612013044E+03 0.18908948084364E+01
 0.18908948084364E+01 0.12760735711909E+02 0.00000000000000E+00 0.32355477981272E+03 0.32355477981272E+03
 0.32355477981272E+03 0.32355477981272E+03 0.00000000000000E+00 0.00000000000000E+00 0.21578612915648E+00
 0.00000000000000E+00 0.98169214822919E+01 0.10000000000000E-02 0.25969626399114E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30805217899757E+02 0.11551956712409E+02 0.35630254290052E+03 0.46941358720812E+03
 0.30459515255269E+03 0.30459515255269E+03 0.29515134973153E+03 0.29515129114144E+03 0.30451349953085E+03
 0.30451349953085E+03 0.29515134102163E+03 0.29515128280962E+03 0.30459515255269E+03 0.30459515255269E+03
 0.29515134973153E+03 0.29515129114144E+03 0.30451349953085E+03 0.30451349953085E+03 0.29515134102163E+03
 0.29515128280962E+03 0.30417878751690E+03 0.29515391564750E+03 -.32685205452811E+03 -.50264249661994E+03
 0.17833104391567E+03 0.31655582661313E+03 0.13733312747787E+03 0.20339797787724E+03 0.13130921606288E+03
 0.20339797787724E+03 0.22807870899833E+03 0.20386389609128E+03 0.12915119025827E+03 0.20386389609128E+03
 0.22643424678581E+03 0.20339797787724E+03 0.13130921606289E+03 0.20339797787724E+03 0.22807870899833E+03
 0.20386389609128E+03 0.12915119025827E+03 0.20386389609128E+03 0.22643424678581E+03 0.23565476432360E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33319613016005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94396670842740E+00 0.93299772526764E+00 0.00000000000000E+00 0.94396670842740E+00 0.93299772526764E+00
 0.13935925568597E+01 0.36359258546995E+00 0.10299999713898E+01 0.15449999570847E+01 0.12435882338168E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.68850762688657E-03 0.00000000000000E+00
 0.13305289079329E-01 0.00000000000000E+00 0.12435882338168E-01 0.13993796706215E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1650.00000000
 0.72733345868037E+00 0.31373207659019E+03 0.51480491317839E+03 0.46605591261760E+03 0.44557033354604E+03
 0.22980088500681E+00 0.00000000000000E+00 0.19434838197776E+00 0.00000000000000E+00 -.19606925770689E+01
 0.46779112365540E-02 0.43856170514257E+00 0.17101649850657E+04 0.64131186939964E+03 0.18241446770641E+02
 0.68405425389905E+01 0.38549477695049E+03 0.29530014168145E+03 0.37822242411201E+03 0.42547719755711E+03
 0.29519489574679E+03 0.29522788812036E+03 0.36945637469299E+03 0.42531874559559E+03 0.29518776414465E+03
 0.29522773406801E+03 0.37822242411201E+03 0.42547719755711E+03 0.29519489574679E+03 0.29522788812036E+03
 0.36945637469299E+03 0.42531874559559E+03 0.29518776414465E+03 0.29522773406801E+03 0.46965173304691E+03
 0.35652686926294E+03 0.26875405761193E+04 0.21871901354685E+04 0.52587387834861E+03 0.92562579820073E+03
 0.39712255046038E+03 0.17325172263525E+04 0.13820380110723E+04 0.13780025624780E+04 0.20451987248822E+04
 0.15555024744143E+04 0.13787215942881E+04 0.12616639787448E+04 0.20433059765663E+04 0.17325172263525E+04
 0.13820380110723E+04 0.13780025624781E+04 0.20451987248822E+04 0.15555024744143E+04 0.13787215942881E+04
 0.12616639787448E+04 0.20433059765663E+04 0.18700267581057E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53935739184364E+03 0.18908948223895E+01
 0.18908948223895E+01 0.12840735711909E+02 0.00000000000000E+00 0.32368309559861E+03 0.32368309559861E+03
 0.32368309559861E+03 0.32368309559861E+03 0.00000000000000E+00 0.00000000000000E+00 0.21571816388319E+00
 0.00000000000000E+00 0.98210707556020E+01 0.10000000000000E-02 0.26098168430823E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30653492106947E+02 0.11495059540105E+02 0.35653025715995E+03 0.46964838033147E+03
 0.30467022956602E+03 0.30467022956602E+03 0.29515140953012E+03 0.29515134834424E+03 0.30458818956261E+03
 0.30458818956261E+03 0.29515140034574E+03 0.29515133955854E+03 0.30467022956602E+03 0.30467022956602E+03
 0.29515140953012E+03 0.29515134834424E+03 0.30458818956261E+03 0.30458818956261E+03 0.29515140034574E+03
 0.29515133955854E+03 0.30424900127362E+03 0.29515407575061E+03 -.32819365937478E+03 -.50463868974595E+03
 0.17897777469537E+03 0.31768363524455E+03 0.13781097167570E+03 0.20433130730156E+03 0.13181735826432E+03
 0.20433130730156E+03 0.22890364555155E+03 0.20479978344581E+03 0.12965598134260E+03 0.20479978344581E+03
 0.22725853384457E+03 0.20433130730156E+03 0.13181735826432E+03 0.20433130730156E+03 0.22890364555155E+03
 0.20479978344581E+03 0.12965598134259E+03 0.20479978344581E+03 0.22725853384457E+03 0.23604467180292E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33332762277635E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94407368857244E+00 0.93312314622726E+00 0.00000000000000E+00 0.94407368857244E+00 0.93312314622726E+00
 0.13936667007300E+01 0.36366672934019E+00 0.10299999713898E+01 0.15449999570847E+01 0.12433259614059E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.69188475410415E-03 0.00000000000000E+00
 0.13288515643292E-01 0.00000000000000E+00 0.12433259614059E-01 0.13980400397396E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1660.00000000
 0.72748089282545E+00 0.31379749251177E+03 0.51496315219950E+03 0.46618176096102E+03 0.44567996962412E+03
 0.22980020255770E+00 0.00000000000000E+00 0.19435186771985E+00 0.00000000000000E+00 -.19621216320893E+01
 0.46919588071499E-02 0.43838488383595E+00 0.17050448072581E+04 0.63939180272180E+03 0.18248804406754E+02
 0.68433016525329E+01 0.38575789947597E+03 0.29530510884283E+03 0.37846998077980E+03 0.42579003156492E+03
 0.29519660152928E+03 0.29523082657757E+03 0.36969575542588E+03 0.42563156968465E+03 0.29518921072697E+03
 0.29523066711872E+03 0.37846998077980E+03 0.42579003156492E+03 0.29519660152928E+03 0.29523082657757E+03
 0.36969575542588E+03 0.42563156968465E+03 0.29518921072697E+03 0.29523066711872E+03 0.46988476705751E+03
 0.35675253268653E+03 0.26881916015245E+04 0.21861237432131E+04 0.52495236319681E+03 0.92384621547204E+03
 0.39626909045925E+03 0.17332192953742E+04 0.13812309972547E+04 0.13774664066430E+04 0.20428915942694E+04
 0.15564799050033E+04 0.13779251014596E+04 0.12615001967249E+04 0.20410085720843E+04 0.17332192953742E+04
 0.13812309972547E+04 0.13774664066430E+04 0.20428915942694E+04 0.15564799050033E+04 0.13779251014596E+04
 0.12615001967249E+04 0.20410085720843E+04 0.18682195322596E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53949792782243E+03 0.18908948362796E+01
 0.18908948362796E+01 0.12920735711909E+02 0.00000000000000E+00 0.32381062736052E+03 0.32381062736052E+03
 0.32381062736052E+03 0.32381062736052E+03 0.00000000000000E+00 0.00000000000000E+00 0.21565041268985E+00
 0.00000000000000E+00 0.98251983675765E+01 0.10000000000000E-02 0.26226236312082E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30503805063003E+02 0.11438926898626E+02 0.35675589074259E+03 0.46988145153773E+03
 0.30474519136255E+03 0.30474519136255E+03 0.29515147142977E+03 0.29515140755690E+03 0.30466276676951E+03
 0.30466276676951E+03 0.29515146175023E+03 0.29515139829754E+03 0.30474519136255E+03 0.30474519136255E+03
 0.29515147142977E+03 0.29515140755690E+03 0.30466276676951E+03 0.30466276676951E+03 0.29515146175023E+03
 0.29515139829754E+03 0.30431911021957E+03 0.29515424093124E+03 -.32952188429915E+03 -.50661292814351E+03
 0.17961745787950E+03 0.31879879979373E+03 0.13828325462483E+03 0.20525709316177E+03 0.13232053268846E+03
 0.20525709316177E+03 0.22971963014296E+03 0.20572811653671E+03 0.13015591484452E+03 0.20572811653671E+03
 0.22807396177745E+03 0.20525709316177E+03 0.13232053268846E+03 0.20525709316177E+03 0.22971963014297E+03
 0.20572811653670E+03 0.13015591484452E+03 0.20572811653670E+03 0.22807396177745E+03 0.23643018035079E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33345831044610E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94418007573836E+00 0.93324776301655E+00 0.00000000000000E+00 0.94418007573836E+00 0.93324776301655E+00
 0.13937404178025E+01 0.36374044641272E+00 0.10299999713898E+01 0.15449999570847E+01 0.12430639740745E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.69522245996226E-03 0.00000000000000E+00
 0.13271920387960E-01 0.00000000000000E+00 0.12430639740745E-01 0.13967142847922E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1670.00000000
 0.72762748224716E+00 0.31386265704241E+03 0.51512055986976E+03 0.46630696616423E+03 0.44578906747953E+03
 0.22979952281162E+00 0.00000000000000E+00 0.19435532901023E+00 0.00000000000000E+00 -.19635442502431E+01
 0.47059406991033E-02 0.43820917280864E+00 0.16999789227102E+04 0.63749209601634E+03 0.18256121725442E+02
 0.68460456470409E+01 0.38601991045598E+03 0.29531018757275E+03 0.37871653794960E+03 0.42610119916331E+03
 0.29519835381611E+03 0.29523384399541E+03 0.36993424637093E+03 0.42594272988407E+03 0.29519069724320E+03
 0.29523367899920E+03 0.37871653794960E+03 0.42610119916331E+03 0.29519835381611E+03 0.29523384399541E+03
 0.36993424637093E+03 0.42594272988407E+03 0.29519069724320E+03 0.29523367899920E+03 0.47011609916946E+03
 0.35697614296805E+03 0.26888350181550E+04 0.21850563825308E+04 0.52404456729899E+03 0.92209046496167E+03
 0.39542567482618E+03 0.17339158068353E+04 0.13804226225572E+04 0.13769291399741E+04 0.20405923384986E+04
 0.15574504984920E+04 0.13771272175834E+04 0.12613330521881E+04 0.20387189942779E+04 0.17339158068353E+04
 0.13804226225572E+04 0.13769291399742E+04 0.20405923384986E+04 0.15574504984920E+04 0.13771272175834E+04
 0.12613330521881E+04 0.20387189942779E+04 0.18664213039579E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53963773504142E+03 0.18908948501071E+01
 0.18908948501071E+01 0.13000735711909E+02 0.00000000000000E+00 0.32393738418974E+03 0.32393738418974E+03
 0.32393738418974E+03 0.32393738418974E+03 0.00000000000000E+00 0.00000000000000E+00 0.21558287505893E+00
 0.00000000000000E+00 0.98293044441771E+01 0.10000000000000E-02 0.26353831835696E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30356116901240E+02 0.11383543837965E+02 0.35697947122975E+03 0.47011282063574E+03
 0.30482003670702E+03 0.30482003670702E+03 0.29515153548422E+03 0.29515146883082E+03 0.30473722993804E+03
 0.30473722993804E+03 0.29515152528818E+03 0.29515145907737E+03 0.30482003670702E+03 0.30482003670702E+03
 0.29515153548422E+03 0.29515146883082E+03 0.30473722993804E+03 0.30473722993804E+03 0.29515152528818E+03
 0.29515145907737E+03 0.30438911284882E+03 0.29515441130001E+03 -.33083688322629E+03 -.50856547680931E+03
 0.18025018933443E+03 0.31990149861329E+03 0.13875005833219E+03 0.20617541068255E+03 0.13281880078253E+03
 0.20617541068255E+03 0.23052678234533E+03 0.20664897041892E+03 0.13065105006333E+03 0.20664897041892E+03
 0.22888064787935E+03 0.20617541068255E+03 0.13281880078253E+03 0.20617541068255E+03 0.23052678234533E+03
 0.20664897041892E+03 0.13065105006333E+03 0.20664897041892E+03 0.22888064787935E+03 0.23681143800636E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33358820273949E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94428587550672E+00 0.93337158406167E+00 0.00000000000000E+00 0.94428587550672E+00 0.93337158406167E+00
 0.13938137125133E+01 0.36381374112358E+00 0.10299999713898E+01 0.15449999570847E+01 0.12428023377479E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.69852137467362E-03 0.00000000000000E+00
 0.13255498907077E-01 0.00000000000000E+00 0.12428023377479E-01 0.13954020281750E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1680.00000000
 0.72777323511680E+00 0.31392757208625E+03 0.51527714430943E+03 0.46643153445401E+03 0.44589763223073E+03
 0.22979884578465E+00 0.00000000000000E+00 0.19435876620611E+00 0.00000000000000E+00 -.19649604915191E+01
 0.47198567956681E-02 0.43803455915475E+00 0.16949666793583E+04 0.63561250475935E+03 0.18263399160644E+02
 0.68487746852415E+01 0.38628081919417E+03 0.29531537898322E+03 0.37896210384588E+03 0.42641071650610E+03
 0.29520015330372E+03 0.29523694150627E+03 0.37017185431066E+03 0.42625224231439E+03 0.29519222430845E+03
 0.29523677084037E+03 0.37896210384588E+03 0.42641071650610E+03 0.29520015330372E+03 0.29523694150627E+03
 0.37017185431066E+03 0.42625224231439E+03 0.29519222430845E+03 0.29523677084037E+03 0.47034574887651E+03
 0.35719772718060E+03 0.26894709044495E+04 0.21839880820472E+04 0.52315034992968E+03 0.92035826553428E+03
 0.39459216385496E+03 0.17346068128775E+04 0.13796129264563E+04 0.13763907818931E+04 0.20383009003764E+04
 0.15584143168447E+04 0.13763279820481E+04 0.12611625855286E+04 0.20364371861594E+04 0.17346068128775E+04
 0.13796129264563E+04 0.13763907818931E+04 0.20383009003764E+04 0.15584143168447E+04 0.13763279820481E+04
 0.12611625855287E+04 0.20364371861594E+04 0.18646319877027E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53977682058298E+03 0.18908948638727E+01
 0.18908948638727E+01 0.13080735711909E+02 0.00000000000000E+00 0.32406337487655E+03 0.32406337487655E+03
 0.32406337487655E+03 0.32406337487655E+03 0.00000000000000E+00 0.00000000000000E+00 0.21551555046852E+00
 0.00000000000000E+00 0.98333891242985E+01 0.10000000000000E-02 0.26480956804407E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30210388767631E+02 0.11328895787862E+02 0.35720102570360E+03 0.47034250711271E+03
 0.30489476439802E+03 0.30489476439802E+03 0.29515160174796E+03 0.29515153221812E+03 0.30481157788606E+03
 0.30481157788606E+03 0.29515159101343E+03 0.29515152194957E+03 0.30489476439802E+03 0.30489476439802E+03
 0.29515160174796E+03 0.29515153221812E+03 0.30481157788606E+03 0.30481157788606E+03 0.29515159101343E+03
 0.29515152194957E+03 0.30445900771447E+03 0.29515458696857E+03 -.33213880817610E+03 -.51049659820087E+03
 0.18087606293239E+03 0.32099190528237E+03 0.13921146203532E+03 0.20708633396539E+03 0.13331222274030E+03
 0.20708633396539E+03 0.23132521861639E+03 0.20756241903842E+03 0.13114144509404E+03 0.20756241903842E+03
 0.22967870639407E+03 0.20708633396539E+03 0.13331222274031E+03 0.20708633396539E+03 0.23132521861639E+03
 0.20756241903842E+03 0.13114144509403E+03 0.20756241903842E+03 0.22967870639407E+03 0.23718857817798E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33371730889145E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94439109336655E+00 0.93349461953957E+00 0.00000000000000E+00 0.94439109336655E+00 0.93349461953957E+00
 0.13938865889482E+01 0.36388661755840E+00 0.10299999713898E+01 0.15449999570847E+01 0.12425411091494E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.70178208820634E-03 0.00000000000000E+00
 0.13239247169932E-01 0.00000000000000E+00 0.12425411091494E-01 0.13941029258138E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1690.00000000
 0.72791815945737E+00 0.31399223960765E+03 0.51543291365173E+03 0.46655547208839E+03 0.44600566904001E+03
 0.22979817148819E+00 0.00000000000000E+00 0.19436217964679E+00 0.00000000000000E+00 -.19663704186904E+01
 0.47337070629298E-02 0.43786103017633E+00 0.16900074072282E+04 0.63375277771059E+03 0.18270637139775E+02
 0.68514889274157E+01 0.38654063485882E+03 0.29532068417313E+03 0.37920668657309E+03 0.42671859949171E+03
 0.29520200068648E+03 0.29524012023736E+03 0.37040858593250E+03 0.42656012284120E+03 0.29519379253652E+03
 0.29523994376797E+03 0.37920668657309E+03 0.42671859949171E+03 0.29520200068648E+03 0.29524012023736E+03
 0.37040858593250E+03 0.42656012284120E+03 0.29519379253652E+03 0.29523994376797E+03 0.47057373548349E+03
 0.35741731192769E+03 0.26900993396797E+04 0.21829188727613E+04 0.52226957170592E+03 0.91864933996719E+03
 0.39376842040274E+03 0.17352923660312E+04 0.13788019505334E+04 0.13758513533227E+04 0.20360172275671E+04
 0.15593714222619E+04 0.13755274363431E+04 0.12609888382831E+04 0.20341630955912E+04 0.17352923660312E+04
 0.13788019505334E+04 0.13758513533227E+04 0.20360172275671E+04 0.15593714222619E+04 0.13755274363431E+04
 0.12609888382831E+04 0.20341630955912E+04 0.18628515019613E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.53991519155078E+03 0.18908948775769E+01
 0.18908948775769E+01 0.13160735711909E+02 0.00000000000000E+00 0.32418860793792E+03 0.32418860793792E+03
 0.32418860793792E+03 0.32418860793792E+03 0.00000000000000E+00 0.00000000000000E+00 0.21544843839280E+00
 0.00000000000000E+00 0.98374525569697E+01 0.10000000000000E-02 0.26607613028252E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30066582791571E+02 0.11274968546839E+02 0.35742058077668E+03 0.47057053026746E+03
 0.30496937326832E+03 0.30496937326832E+03 0.29515167027629E+03 0.29515159777171E+03 0.30488580946338E+03
 0.30488580946338E+03 0.29515165898063E+03 0.29515158696638E+03 0.30496937326832E+03 0.30496937326832E+03
 0.29515167027629E+03 0.29515159777171E+03 0.30488580946338E+03 0.30488580946338E+03 0.29515165898063E+03
 0.29515158696638E+03 0.30452879342639E+03 0.29515476804961E+03 -.33342780940842E+03 -.51240655236247E+03
 0.18149517070370E+03 0.32207018900060E+03 0.13966754244338E+03 0.20798993610678E+03 0.13380085760729E+03
 0.20798993610678E+03 0.23211505256368E+03 0.20846853534902E+03 0.13162715692949E+03 0.20846853534902E+03
 0.23046824877463E+03 0.20798993610678E+03 0.13380085760729E+03 0.20798993610678E+03 0.23211505256368E+03
 0.20846853534901E+03 0.13162715692949E+03 0.20846853534901E+03 0.23046824877462E+03 0.23756172107567E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33384563783201E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94449573452298E+00 0.93361688021892E+00 0.00000000000000E+00 0.94449573452298E+00 0.93361688021892E+00
 0.13939590511185E+01 0.36395907972869E+00 0.10299999713898E+01 0.15449999570847E+01 0.12422803367548E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.70500517274864E-03 0.00000000000000E+00
 0.13223161464919E-01 0.00000000000000E+00 0.12422803367548E-01 0.13928166637668E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1700.00000000
 0.72806226344543E+00 0.31405666144011E+03 0.51558787592813E+03 0.46667878520311E+03 0.44611318293006E+03
 0.22979749992954E+00 0.00000000000000E+00 0.19436556965522E+00 0.00000000000000E+00 -.19677740898730E+01
 0.47474915415260E-02 0.43768857346787E+00 0.16851004219859E+04 0.63191265824472E+03 0.18277836080149E+02
 0.68541885300559E+01 0.38679936650137E+03 0.29532610422815E+03 0.37945029413373E+03 0.42702486379321E+03
 0.29520389665654E+03 0.29524338131038E+03 0.37064444784538E+03 0.42686638710505E+03 0.29519540253978E+03
 0.29524319890228E+03 0.37945029413373E+03 0.42702486379321E+03 0.29520389665654E+03 0.29524338131038E+03
 0.37064444784538E+03 0.42686638710505E+03 0.29519540253978E+03 0.29524319890228E+03 0.47080007812136E+03
 0.35763492336276E+03 0.26907204022034E+04 0.21818487842062E+04 0.52140209251897E+03 0.91696341152113E+03
 0.39295430853956E+03 0.17359725182568E+04 0.13779897362224E+04 0.13753108741424E+04 0.20337412690768E+04
 0.15603218762730E+04 0.13747256218096E+04 0.12608118507215E+04 0.20318966717726E+04 0.17359725182568E+04
 0.13779897362224E+04 0.13753108741424E+04 0.20337412690768E+04 0.15603218762730E+04 0.13747256218096E+04
 0.12608118507215E+04 0.20318966717726E+04 0.18610797654190E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54005285493683E+03 0.18908948912203E+01
 0.18908948912203E+01 0.13240735711909E+02 0.00000000000000E+00 0.32431309164435E+03 0.32431309164435E+03
 0.32431309164435E+03 0.32431309164435E+03 0.00000000000000E+00 0.00000000000000E+00 0.21538153830257E+00
 0.00000000000000E+00 0.98414948980458E+01 0.10000000000000E-02 0.26733802322021E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29924662057556E+02 0.11221748271584E+02 0.35763816261131E+03 0.47079690922562E+03
 0.30504386218458E+03 0.30504386218458E+03 0.29515174112528E+03 0.29515166554523E+03 0.30495992355159E+03
 0.30495992355159E+03 0.29515172924516E+03 0.29515165418081E+03 0.30504386218458E+03 0.30504386218458E+03
 0.29515174112528E+03 0.29515166554523E+03 0.30495992355159E+03 0.30495992355159E+03 0.29515172924516E+03
 0.29515165418081E+03 0.30459846864877E+03 0.29515495465679E+03 -.33470403548890E+03 -.51429559690639E+03
 0.18210760295404E+03 0.32313651494006E+03 0.14011837397125E+03 0.20888628927288E+03 0.13428476336271E+03
 0.20888628927288E+03 0.23289639518429E+03 0.20936739138613E+03 0.13210824154016E+03 0.20936739138613E+03
 0.23124938391993E+03 0.20888628927288E+03 0.13428476336271E+03 0.20888628927288E+03 0.23289639518429E+03
 0.20936739138612E+03 0.13210824154015E+03 0.20936739138612E+03 0.23124938391993E+03 0.23793097531644E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33397319821282E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94459980410491E+00 0.93373837616505E+00 0.00000000000000E+00 0.94459980410491E+00 0.93373837616505E+00
 0.13940311031125E+01 0.36403113172271E+00 0.10299999713898E+01 0.15449999570847E+01 0.12420200617849E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.70819119637417E-03 0.00000000000000E+00
 0.13207238349092E-01 0.00000000000000E+00 0.12420200617849E-01 0.13915429545466E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1710.00000000
 0.72820555536431E+00 0.31412083931887E+03 0.51574203901219E+03 0.46680147978022E+03 0.44622017877191E+03
 0.22979683111230E+00 0.00000000000000E+00 0.19436893654315E+00 0.00000000000000E+00 -.19691715587357E+01
 0.47612103396265E-02 0.43751717690651E+00 0.16802450279119E+04 0.63009188546695E+03 0.18284996389317E+02
 0.68568736459941E+01 0.38705702305443E+03 0.29533164022051E+03 0.37969293442605E+03 0.42732952486018E+03
 0.29520584190365E+03 0.29524672584125E+03 0.37087944657727E+03 0.42717105052335E+03 0.29519705492900E+03
 0.29524653735778E+03 0.37969293442605E+03 0.42732952486018E+03 0.29520584190365E+03 0.29524672584125E+03
 0.37087944657727E+03 0.42717105052335E+03 0.29519705492900E+03 0.29524653735778E+03 0.47102479570594E+03
 0.35785058720183E+03 0.26913341686825E+04 0.21807778440168E+04 0.52054777146886E+03 0.91530020375266E+03
 0.39214969342645E+03 0.17366473204728E+04 0.13771763237287E+04 0.13747693629832E+04 0.20314729737009E+04
 0.15612657392701E+04 0.13739225785614E+04 0.12606316616366E+04 0.20296378636900E+04 0.17366473204728E+04
 0.13771763237287E+04 0.13747693629832E+04 0.20314729737009E+04 0.15612657392701E+04 0.13739225785614E+04
 0.12606316616366E+04 0.20296378636900E+04 0.18593166961007E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54018981758032E+03 0.18908949048033E+01
 0.18908949048033E+01 0.13320735711909E+02 0.00000000000000E+00 0.32443683404469E+03 0.32443683404469E+03
 0.32443683404469E+03 0.32443683404469E+03 0.00000000000000E+00 0.00000000000000E+00 0.21531484966565E+00
 0.00000000000000E+00 0.98455163076929E+01 0.10000000000000E-02 0.26859526502958E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29784590577645E+02 0.11169221466617E+02 0.35785379693221E+03 0.47102166289807E+03
 0.30511823004751E+03 0.30511823004751E+03 0.29515181435180E+03 0.29515173559307E+03 0.30503391906436E+03
 0.30503391906436E+03 0.29515180186322E+03 0.29515172364660E+03 0.30511823004751E+03 0.30511823004751E+03
 0.29515181435180E+03 0.29515173559307E+03 0.30503391906436E+03 0.30503391906436E+03 0.29515180186322E+03
 0.29515172364660E+03 0.30466803209827E+03 0.29515514690479E+03 -.33596763330026E+03 -.51616398690681E+03
 0.18271344835332E+03 0.32419104455107E+03 0.14056402895598E+03 0.20977546474147E+03 0.13476399698139E+03
 0.20977546474147E+03 0.23366935507241E+03 0.21025905830790E+03 0.13258475393433E+03 0.21025905830790E+03
 0.23202221837995E+03 0.20977546474147E+03 0.13476399698139E+03 0.20977546474147E+03 0.23366935507241E+03
 0.21025905830789E+03 0.13258475393433E+03 0.21025905830789E+03 0.23202221837994E+03 0.23829643953247E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33409999843487E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94470330729491E+00 0.93385911659487E+00 0.00000000000000E+00 0.94470330729491E+00 0.93385911659487E+00
 0.13941027490719E+01 0.36410277768215E+00 0.10299999713898E+01 0.15449999570847E+01 0.12417603194188E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.71134072125458E-03 0.00000000000000E+00
 0.13191474607947E-01 0.00000000000000E+00 0.12417603194188E-01 0.13902815329202E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1720.00000000
 0.72834804343404E+00 0.31418477497267E+03 0.51589541063951E+03 0.46692356169692E+03 0.44632666135543E+03
 0.22979616503678E+00 0.00000000000000E+00 0.19437228061430E+00 0.00000000000000E+00 -.19705628775507E+01
 0.47748636233167E-02 0.43734682859944E+00 0.16754405216799E+04 0.62829019562995E+03 0.18292118467211E+02
 0.68595444252042E+01 0.38731361332609E+03 0.29533729320886E+03 0.37993461523829E+03 0.42763259791278E+03
 0.29520783711494E+03 0.29525015493981E+03 0.37111358856928E+03 0.42747412828449E+03 0.29519875031318E+03
 0.29524996024291E+03 0.37993461523829E+03 0.42763259791278E+03 0.29520783711494E+03 0.29525015493981E+03
 0.37111358856928E+03 0.42747412828449E+03 0.29519875031318E+03 0.29524996024291E+03 0.47124790691038E+03
 0.35806432873514E+03 0.26919407144335E+04 0.21797060792675E+04 0.51970646776486E+03 0.91365944191888E+03
 0.39135444181520E+03 0.17373168227273E+04 0.13763617523722E+04 0.13742268381473E+04 0.20292122905999E+04
 0.15622030706652E+04 0.13731183458296E+04 0.12604483092131E+04 0.20273866206937E+04 0.17373168227273E+04
 0.13763617523722E+04 0.13742268381473E+04 0.20292122905999E+04 0.15622030706652E+04 0.13731183458296E+04
 0.12604483092131E+04 0.20273866206937E+04 0.18575622124465E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54032608620258E+03 0.18908949183267E+01
 0.18908949183267E+01 0.13400735711909E+02 0.00000000000000E+00 0.32455984298775E+03 0.32455984298775E+03
 0.32455984298775E+03 0.32455984298775E+03 0.00000000000000E+00 0.00000000000000E+00 0.21524837194729E+00
 0.00000000000000E+00 0.98495169499979E+01 0.10000000000000E-02 0.26984787388740E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29646333264565E+02 0.11117374974212E+02 0.35806750903807E+03 0.47124480995322E+03
 0.30519247579214E+03 0.30519247579214E+03 0.29515189001349E+03 0.29515180797036E+03 0.30510779494792E+03
 0.30510779494792E+03 0.29515187689176E+03 0.29515179541823E+03 0.30519247579214E+03 0.30519247579214E+03
 0.29515189001349E+03 0.29515180797036E+03 0.30510779494792E+03 0.30510779494792E+03 0.29515187689176E+03
 0.29515179541823E+03 0.30473748254259E+03 0.29515534490920E+03 -.33721874804983E+03 -.51801197480710E+03
 0.18331279401353E+03 0.32523393582750E+03 0.14100457784390E+03 0.21065753293716E+03 0.13523861448758E+03
 0.21065753293716E+03 0.23443403859804E+03 0.21114360642968E+03 0.13305674821050E+03 0.21114360642968E+03
 0.23278685653216E+03 0.21065753293716E+03 0.13523861448758E+03 0.21065753293716E+03 0.23443403859804E+03
 0.21114360642968E+03 0.13305674821050E+03 0.21114360642968E+03 0.23278685653216E+03 0.23865820376402E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33422604667366E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94480624937978E+00 0.93397911024733E+00 0.00000000000000E+00 0.94480624937978E+00 0.93397911024733E+00
 0.13941739931068E+01 0.36417402171702E+00 0.10299999713898E+01 0.15449999570847E+01 0.12415011395347E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.71445429827742E-03 0.00000000000000E+00
 0.13175867235136E-01 0.00000000000000E+00 0.12415011395347E-01 0.13890321533413E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1730.00000000
 0.72848973572177E+00 0.31424847015570E+03 0.51604799844576E+03 0.46704503676816E+03 0.44643263543394E+03
 0.22979550170053E+00 0.00000000000000E+00 0.19437560216481E+00 0.00000000000000E+00 -.19719480989488E+01
 0.47884516064915E-02 0.43717751684881E+00 0.16706861961714E+04 0.62650732356427E+03 0.18299202707550E+02
 0.68622010153314E+01 0.38756914600068E+03 0.29534306423814E+03 0.38017534424907E+03 0.42793409794312E+03
 0.29520988297482E+03 0.29525366970953E+03 0.37134688017555E+03 0.42777563534914E+03 0.29520048929941E+03
 0.29525346865976E+03 0.38017534424907E+03 0.42793409794312E+03 0.29520988297482E+03 0.29525366970953E+03
 0.37134688017555E+03 0.42777563534914E+03 0.29520048929941E+03 0.29525346865976E+03 0.47146943016893E+03
 0.35827617283970E+03 0.26925401140056E+04 0.21786335173749E+04 0.51887804132721E+03 0.91204085391637E+03
 0.39056842238253E+03 0.17379810745265E+04 0.13755460612401E+04 0.13736833181824E+04 0.20269591702500E+04
 0.15631339292077E+04 0.13723129626140E+04 0.12602618315828E+04 0.20251428934480E+04 0.17379810745265E+04
 0.13755460612401E+04 0.13736833181824E+04 0.20269591702500E+04 0.15631339292077E+04 0.13723129626140E+04
 0.12602618315828E+04 0.20251428934480E+04 0.18558162342789E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54046166744591E+03 0.18908949317907E+01
 0.18908949317907E+01 0.13480735711909E+02 0.00000000000000E+00 0.32468212614053E+03 0.32468212614053E+03
 0.32468212614053E+03 0.32468212614053E+03 0.00000000000000E+00 0.00000000000000E+00 0.21518210461049E+00
 0.00000000000000E+00 0.98534969926131E+01 0.10000000000000E-02 0.27109586795782E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29509855905456E+02 0.11066195964546E+02 0.35827932381410E+03 0.47146636882078E+03
 0.30526659838818E+03 0.30526659838818E+03 0.29515196816875E+03 0.29515188273299E+03 0.30518155018152E+03
 0.30518155018152E+03 0.29515195438848E+03 0.29515186955090E+03 0.30526659838818E+03 0.30526659838818E+03
 0.29515196816875E+03 0.29515188273299E+03 0.30518155018152E+03 0.30518155018152E+03 0.29515195438848E+03
 0.29515186955090E+03 0.30480681879921E+03 0.29515554878661E+03 -.33845752329303E+03 -.51983981037581E+03
 0.18390572556308E+03 0.32626534354090E+03 0.14144008935000E+03 0.21153256347004E+03 0.13570867100589E+03
 0.21153256347004E+03 0.23519055006372E+03 0.21202110526196E+03 0.13352427760674E+03 0.21202110526196E+03
 0.23354340073606E+03 0.21153256347004E+03 0.13570867100589E+03 0.21153256347004E+03 0.23519055006372E+03
 0.21202110526196E+03 0.13352427760674E+03 0.21202110526196E+03 0.23354340073605E+03 0.23901635059418E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33435135090189E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94490863565085E+00 0.93409836576708E+00 0.00000000000000E+00 0.94490863565085E+00 0.93409836576708E+00
 0.13942448392507E+01 0.36424486786089E+00 0.10299999713898E+01 0.15449999570847E+01 0.12412425474739E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.71753246527681E-03 0.00000000000000E+00
 0.13160413408751E-01 0.00000000000000E+00 0.12412425474739E-01 0.13877945874028E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1740.00000000
 0.72863064016086E+00 0.31431192662578E+03 0.51619980998155E+03 0.46716591075147E+03 0.44653810572060E+03
 0.22979484109883E+00 0.00000000000000E+00 0.19437890148273E+00 0.00000000000000E+00 -.19733272754993E+01
 0.48019745424153E-02 0.43700923014742E+00 0.16659813435779E+04 0.62474300384172E+03 0.18306249497983E+02
 0.68648435617435E+01 0.38782362964404E+03 0.29534895433942E+03 0.38041512903229E+03 0.42823403972344E+03
 0.29521198016477E+03 0.29525727124724E+03 0.37157932766746E+03 0.42807558645846E+03 0.29520227249275E+03
 0.29525706370381E+03 0.38041512903229E+03 0.42823403972344E+03 0.29521198016477E+03 0.29525727124724E+03
 0.37157932766746E+03 0.42807558645846E+03 0.29520227249275E+03 0.29525706370381E+03 0.47168938369275E+03
 0.35848614399212E+03 0.26931324413862E+04 0.21775601860418E+04 0.51806235278454E+03 0.91044417024408E+03
 0.38979150569562E+03 0.17386401249675E+04 0.13747292893988E+04 0.13731388218169E+04 0.20247135646893E+04
 0.15640583731201E+04 0.13715064678946E+04 0.12600722667765E+04 0.20229066341762E+04 0.17386401249675E+04
 0.13747292893988E+04 0.13731388218170E+04 0.20247135646893E+04 0.15640583731201E+04 0.13715064678946E+04
 0.12600722667765E+04 0.20229066341762E+04 0.18540786828812E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54059656788235E+03 0.18908949451960E+01
 0.18908949451960E+01 0.13560735711909E+02 0.00000000000000E+00 0.32480369100380E+03 0.32480369100380E+03
 0.32480369100380E+03 0.32480369100380E+03 0.00000000000000E+00 0.00000000000000E+00 0.21511604711632E+00
 0.00000000000000E+00 0.98574566060013E+01 0.10000000000000E-02 0.27233926537819E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29375125136254E+02 0.11015671926095E+02 0.35848926574486E+03 0.47168635770761E+03
 0.30534059684014E+03 0.30534059684014E+03 0.29515204887679E+03 0.29515195993758E+03 0.30525518377760E+03
 0.30525518377760E+03 0.29515203441187E+03 0.29515194610057E+03 0.30534059684014E+03 0.30534059684014E+03
 0.29515204887679E+03 0.29515195993758E+03 0.30525518377760E+03 0.30525518377760E+03 0.29515203441187E+03
 0.29515194610057E+03 0.30487603973405E+03 0.29515575865448E+03 -.33968410096575E+03 -.52164774069318E+03
 0.18449232721599E+03 0.32728541944601E+03 0.14187063059394E+03 0.21240062517445E+03 0.13617422080829E+03
 0.21240062517445E+03 0.23593899184167E+03 0.21289162354844E+03 0.13398739454632E+03 0.21289162354844E+03
 0.23429195146818E+03 0.21240062517445E+03 0.13617422080829E+03 0.21240062517445E+03 0.23593899184167E+03
 0.21289162354843E+03 0.13398739454632E+03 0.21289162354843E+03 0.23429195146818E+03 0.23937095608363E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33447591890820E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94501047135549E+00 0.93421689172583E+00 0.00000000000000E+00 0.94501047135549E+00 0.93421689172583E+00
 0.13943152914702E+01 0.36431532008043E+00 0.10299999713898E+01 0.15449999570847E+01 0.12409845647069E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.72057574876810E-03 0.00000000000000E+00
 0.13145110467078E-01 0.00000000000000E+00 0.12409845647069E-01 0.13865686215846E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1750.00000000
 0.72877076460114E+00 0.31437514611974E+03 0.51635085270609E+03 0.46728618933283E+03 0.44664307686870E+03
 0.22979418322507E+00 0.00000000000000E+00 0.19438217884813E+00 0.00000000000000E+00 -.19747004589538E+01
 0.48154327172866E-02 0.43684195718495E+00 0.16613252577035E+04 0.62299697163881E+03 0.18313259219771E+02
 0.68674722074140E+01 0.38807707270856E+03 0.29535496452978E+03 0.38065397706165E+03 0.42853243781479E+03
 0.29521412936319E+03 0.29526096064287E+03 0.37181093723749E+03 0.42837399614278E+03 0.29520410049605E+03
 0.29526074646361E+03 0.38065397706165E+03 0.42853243781479E+03 0.29521412936319E+03 0.29526096064287E+03
 0.37181093723749E+03 0.42837399614278E+03 0.29520410049605E+03 0.29526074646361E+03 0.47190778547947E+03
 0.35869426628054E+03 0.26937177698900E+04 0.21764861128579E+04 0.51725926324217E+03 0.90886912359161E+03
 0.38902356403323E+03 0.17392940226901E+04 0.13739114757205E+04 0.13725933676956E+04 0.20224754272012E+04
 0.15649764600558E+04 0.13706989004587E+04 0.12598796524800E+04 0.20206777963449E+04 0.17392940226901E+04
 0.13739114757205E+04 0.13725933676957E+04 0.20224754272012E+04 0.15649764600558E+04 0.13706989004587E+04
 0.12598796524800E+04 0.20206777963449E+04 0.18523494806134E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54073079400345E+03 0.18908949585431E+01
 0.18908949585431E+01 0.13640735711909E+02 0.00000000000000E+00 0.32492454492568E+03 0.32492454492568E+03
 0.32492454492568E+03 0.32492454492568E+03 0.00000000000000E+00 0.00000000000000E+00 0.21505019892416E+00
 0.00000000000000E+00 0.98613959627570E+01 0.10000000000000E-02 0.27357808424697E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29242108416763E+02 0.10965790656286E+02 0.35869735892611E+03 0.47190479460732E+03
 0.30541447018728E+03 0.30541447018728E+03 0.29515213219756E+03 0.29515203964148E+03 0.30532869478199E+03
 0.30532869478199E+03 0.29515211702119E+03 0.29515202512390E+03 0.30541447018728E+03 0.30541447018728E+03
 0.29515213219756E+03 0.29515203964148E+03 0.30532869478199E+03 0.30532869478199E+03 0.29515211702119E+03
 0.29515202512390E+03 0.30494514426032E+03 0.29515597463121E+03 -.34089862141535E+03 -.52343601014533E+03
 0.18507268183377E+03 0.32829431245972E+03 0.14229626721678E+03 0.21326178614301E+03 0.13663531735563E+03
 0.21326178614301E+03 0.23667946449259E+03 0.21375522929949E+03 0.13444615067784E+03 0.21375522929949E+03
 0.23503260743905E+03 0.21326178614301E+03 0.13663531735563E+03 0.21326178614301E+03 0.23667946449259E+03
 0.21375522929949E+03 0.13444615067784E+03 0.21375522929949E+03 0.23503260743905E+03 0.23972209056805E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33459975831186E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94511176168338E+00 0.93433469652162E+00 0.00000000000000E+00 0.94511176168338E+00 0.93433469652162E+00
 0.13943853536903E+01 0.36438538230057E+00 0.10299999713898E+01 0.15449999570847E+01 0.12407272091815E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.72358466622856E-03 0.00000000000000E+00
 0.13129955893282E-01 0.00000000000000E+00 0.12407272091815E-01 0.13853540559510E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1760.00000000
 0.72891011682850E+00 0.31443813034924E+03 0.51650113397894E+03 0.46740587811813E+03 0.44674755346338E+03
 0.22979352807099E+00 0.00000000000000E+00 0.19438543453377E+00 0.00000000000000E+00 -.19760676998895E+01
 0.48288264451035E-02 0.43667568684718E+00 0.16567172357400E+04 0.62126896340250E+03 0.18320232247782E+02
 0.68700870929183E+01 0.38832948353595E+03 0.29536109581217E+03 0.38089189571308E+03 0.42882930657289E+03
 0.29521633124525E+03 0.29526473897917E+03 0.37204171500122E+03 0.42867087872744E+03 0.29520597390984E+03
 0.29526451802061E+03 0.38089189571308E+03 0.42882930657289E+03 0.29521633124525E+03 0.29526473897917E+03
 0.37204171500122E+03 0.42867087872744E+03 0.29520597390984E+03 0.29526451802061E+03 0.47212465331400E+03
 0.35890056341536E+03 0.26942961720290E+04 0.21754113251073E+04 0.51646863418245E+03 0.90731544863845E+03
 0.38826447128509E+03 0.17399428158071E+04 0.13730926586848E+04 0.13720469742646E+04 0.20202447119889E+04
 0.15658882470352E+04 0.13698902987019E+04 0.12596840259287E+04 0.20184563343381E+04 0.17399428158071E+04
 0.13730926586848E+04 0.13720469742646E+04 0.20202447119889E+04 0.15658882470352E+04 0.13698902987019E+04
 0.12596840259287E+04 0.20184563343381E+04 0.18506285506507E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54086435221229E+03 0.18908949718324E+01
 0.18908949718324E+01 0.13720735711909E+02 0.00000000000000E+00 0.32504469511360E+03 0.32504469511360E+03
 0.32504469511360E+03 0.32504469511360E+03 0.00000000000000E+00 0.00000000000000E+00 0.21498455949192E+00
 0.00000000000000E+00 0.98653152366334E+01 0.10000000000000E-02 0.27481234261343E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29110774006440E+02 0.10916540252415E+02 0.35890362707560E+03 0.47212169730112E+03
 0.30548821750362E+03 0.30548821750362E+03 0.29515221819178E+03 0.29515212190278E+03 0.30540208227386E+03
 0.30540208227386E+03 0.29515220227642E+03 0.29515210667828E+03 0.30548821750362E+03 0.30548821750362E+03
 0.29515221819178E+03 0.29515212190278E+03 0.30540208227386E+03 0.30540208227386E+03 0.29515220227642E+03
 0.29515210667828E+03 0.30501413133727E+03 0.29515619683603E+03 -.34210122342736E+03 -.52520486041908E+03
 0.18564687097982E+03 0.32929216882103E+03 0.14271706348631E+03 0.21411611375513E+03 0.13709201333359E+03
 0.21411611375513E+03 0.23741206687147E+03 0.21461198982017E+03 0.13490059691007E+03 0.21461198982017E+03
 0.23576546569721E+03 0.21411611375513E+03 0.13709201333359E+03 0.21411611375513E+03 0.23741206687148E+03
 0.21461198982017E+03 0.13490059691007E+03 0.21461198982017E+03 0.23576546569721E+03 0.24006981936998E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33472287657731E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94521251180064E+00 0.93445178828765E+00 0.00000000000000E+00 0.94521251180064E+00 0.93445178828765E+00
 0.13944550298040E+01 0.36445505841425E+00 0.10299999713898E+01 0.15449999570847E+01 0.12404704960207E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.72655972635808E-03 0.00000000000000E+00
 0.13114947291785E-01 0.00000000000000E+00 0.12404704960207E-01 0.13841507018143E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1770.00000000
 0.72904870455177E+00 0.31450088100800E+03 0.51665066105979E+03 0.46752498263573E+03 0.44685154002627E+03
 0.22979287562697E+00 0.00000000000000E+00 0.19438866880601E+00 0.00000000000000E+00 -.19774290479683E+01
 0.48421560630857E-02 0.43651040820650E+00 0.16521565797906E+04 0.61955871742147E+03 0.18327168950838E+02
 0.68726883565644E+01 0.38858087035897E+03 0.29536734917534E+03 0.38112889226623E+03 0.42912466015195E+03
 0.29521858648272E+03 0.29526860733147E+03 0.37227166699826E+03 0.42896624833662E+03 0.29520789333216E+03
 0.29526837944882E+03 0.38112889226623E+03 0.42912466015195E+03 0.29521858648272E+03 0.29526860733147E+03
 0.37227166699826E+03 0.42896624833662E+03 0.29520789333216E+03 0.29526837944882E+03 0.47234000476684E+03
 0.35910505873921E+03 0.26948677195076E+04 0.21743358498275E+04 0.51569032754377E+03 0.90578288214191E+03
 0.38751410296042E+03 0.17405865519022E+04 0.13722728763346E+04 0.13714996598178E+04 0.20180213740907E+04
 0.15667937904434E+04 0.13690807005848E+04 0.12594854239554E+04 0.20162422033733E+04 0.17405865519022E+04
 0.13722728763345E+04 0.13714996598178E+04 0.20180213740907E+04 0.15667937904434E+04 0.13690807005848E+04
 0.12594854239554E+04 0.20162422033733E+04 0.18489158169886E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54099724882476E+03 0.18908949850644E+01
 0.18908949850644E+01 0.13800735711909E+02 0.00000000000000E+00 0.32516414864480E+03 0.32516414864480E+03
 0.32516414864480E+03 0.32516414864480E+03 0.00000000000000E+00 0.00000000000000E+00 0.21491912827626E+00
 0.00000000000000E+00 0.98692146022130E+01 0.10000000000000E-02 0.27604205846892E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28981090940896E+02 0.10867909102836E+02 0.35910809354299E+03 0.47233708335603E+03
 0.30556183789773E+03 0.30556183789773E+03 0.29515230692094E+03 0.29515220678030E+03 0.30547534536571E+03
 0.30547534536571E+03 0.29515229023832E+03 0.29515219082185E+03 0.30556183789773E+03 0.30556183789773E+03
 0.29515230692094E+03 0.29515220678030E+03 0.30547534536571E+03 0.30547534536571E+03 0.29515229023832E+03
 0.29515219082185E+03 0.30508299996909E+03 0.29515642538907E+03 -.34329204424928E+03 -.52695453049940E+03
 0.18621497496801E+03 0.33027913222970E+03 0.14313308238685E+03 0.21496367470147E+03 0.13754436068450E+03
 0.21496367470147E+03 0.23813689621866E+03 0.21546197173421E+03 0.13535078344258E+03 0.21546197173421E+03
 0.23649062171873E+03 0.21496367470147E+03 0.13754436068450E+03 0.21496367470147E+03 0.23813689621866E+03
 0.21546197173421E+03 0.13535078344258E+03 0.21546197173421E+03 0.23649062171873E+03 0.24041420340879E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33484528102511E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94531272682678E+00 0.93456817496210E+00 0.00000000000000E+00 0.94531272682678E+00 0.93456817496210E+00
 0.13945243236657E+01 0.36452435227588E+00 0.10299999713898E+01 0.15449999570847E+01 0.12402144376837E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.72950142928892E-03 0.00000000000000E+00
 0.13100082381340E-01 0.00000000000000E+00 0.12402144376837E-01 0.13829583810629E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1780.00000000
 0.72918653538460E+00 0.31456339977683E+03 0.51679944111352E+03 0.46764350834279E+03 0.44695504102240E+03
 0.22979222588220E+00 0.00000000000000E+00 0.19439188192535E+00 0.00000000000000E+00 -.19787845521615E+01
 0.48554219276571E-02 0.43634611051137E+00 0.16476425981501E+04 0.61786597430629E+03 0.18334069692118E+02
 0.68752761345441E+01 0.38883124130332E+03 0.29537372559368E+03 0.38136497390604E+03 0.42941851250845E+03
 0.29522089574384E+03 0.29527256676740E+03 0.37250079919351E+03 0.42926011889710E+03 0.29520985935848E+03
 0.29527233181460E+03 0.38136497390604E+03 0.42941851250845E+03 0.29522089574384E+03 0.29527256676740E+03
 0.37250079919351E+03 0.42926011889710E+03 0.29520985935848E+03 0.29527233181460E+03 0.47255385719457E+03
 0.35930777523636E+03 0.26954324832921E+04 0.21732597139188E+04 0.51492420584378E+03 0.90427116309840E+03
 0.38677233622539E+03 0.17412252780690E+04 0.13714521663289E+04 0.13709514425676E+04 0.20158053694356E+04
 0.15676931460704E+04 0.13682701436862E+04 0.12592838830624E+04 0.20140353595570E+04 0.17412252780690E+04
 0.13714521663289E+04 0.13709514425676E+04 0.20158053694356E+04 0.15676931460704E+04 0.13682701436863E+04
 0.12592838830624E+04 0.20140353595570E+04 0.18472112045455E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54112949007506E+03 0.18908949982396E+01
 0.18908949982396E+01 0.13880735711909E+02 0.00000000000000E+00 0.32528291247546E+03 0.32528291247546E+03
 0.32528291247546E+03 0.32528291247546E+03 0.00000000000000E+00 0.00000000000000E+00 0.21485390473276E+00
 0.00000000000000E+00 0.98730942344860E+01 0.10000000000000E-02 0.27726724973934E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28853029009091E+02 0.10819885878409E+02 0.35931078131927E+03 0.47255097012536E+03
 0.30563533051259E+03 0.30563533051259E+03 0.29515239844728E+03 0.29515229433357E+03 0.30554848320328E+03
 0.30554848320328E+03 0.29515238096840E+03 0.29515227761343E+03 0.30563533051259E+03 0.30563533051259E+03
 0.29515239844728E+03 0.29515229433357E+03 0.30554848320328E+03 0.30554848320328E+03 0.29515238096840E+03
 0.29515227761343E+03 0.30515174920389E+03 0.29515666041127E+03 -.34447121961349E+03 -.52868525667252E+03
 0.18677707290660E+03 0.33125534397166E+03 0.14354438570052E+03 0.21580453500585E+03 0.13799241063573E+03
 0.21580453500585E+03 0.23885404824181E+03 0.21630524100545E+03 0.13579675979318E+03 0.21630524100545E+03
 0.23720816948768E+03 0.21580453500585E+03 0.13799241063573E+03 0.21580453500585E+03 0.23885404824181E+03
 0.21630524100545E+03 0.13579675979318E+03 0.21630524100545E+03 0.23720816948768E+03 0.24075529973841E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33496697884339E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94541241186785E+00 0.93468386427958E+00 0.00000000000000E+00 0.94541241186785E+00 0.93468386427958E+00
 0.13945932390821E+01 0.36459326769230E+00 0.10299999713898E+01 0.15449999570847E+01 0.12399590444662E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.73241026628681E-03 0.00000000000000E+00
 0.13085358978673E-01 0.00000000000000E+00 0.12399590444662E-01 0.13817769244960E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1790.00000000
 0.72932361683987E+00 0.31462568832288E+03 0.51694748121467E+03 0.46776146062888E+03 0.44705806086295E+03
 0.22979157882493E+00 0.00000000000000E+00 0.19439507414670E+00 0.00000000000000E+00 -.19801342608711E+01
 0.48686244108717E-02 0.43618278317932E+00 0.16431746063911E+04 0.61619047739665E+03 0.18340934829404E+02
 0.68778505610265E+01 0.38908060438998E+03 0.29538022602717E+03 0.38160014772491E+03 0.42971087740547E+03
 0.29522325969314E+03 0.29527661834672E+03 0.37272911747878E+03 0.42955250414257E+03 0.29521187258150E+03
 0.29527637617642E+03 0.38160014772491E+03 0.42971087740547E+03 0.29522325969314E+03 0.29527661834672E+03
 0.37272911747878E+03 0.42955250414257E+03 0.29521187258150E+03 0.29527637617642E+03 0.47276622774249E+03
 0.35950873554155E+03 0.26959905336665E+04 0.21721829441766E+04 0.51417013224898E+03 0.90278003281949E+03
 0.38603904990927E+03 0.17418590409461E+04 0.13706305659848E+04 0.13704023406616E+04 0.20135966548757E+04
 0.15685863691469E+04 0.13674586652445E+04 0.12590794394431E+04 0.20118357599170E+04 0.17418590409461E+04
 0.13706305659848E+04 0.13704023406616E+04 0.20135966548757E+04 0.15685863691469E+04 0.13674586652445E+04
 0.12590794394431E+04 0.20118357599171E+04 0.18455146392058E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54126108211942E+03 0.18908950113585E+01
 0.18908950113585E+01 0.13960735711909E+02 0.00000000000000E+00 0.32540099344877E+03 0.32540099344877E+03
 0.32540099344877E+03 0.32540099344877E+03 0.00000000000000E+00 0.00000000000000E+00 0.21478888831605E+00
 0.00000000000000E+00 0.98769543084854E+01 0.10000000000000E-02 0.27848793427884E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28726558731230E+02 0.10772459524211E+02 0.35951171304560E+03 0.47276337475142E+03
 0.30570869452534E+03 0.30570869452534E+03 0.29515249283379E+03 0.29515238462285E+03 0.30562149496534E+03
 0.30562149496534E+03 0.29515247452890E+03 0.29515236711256E+03 0.30570869452534E+03 0.30570869452534E+03
 0.29515249283379E+03 0.29515238462285E+03 0.30562149496534E+03 0.30562149496534E+03 0.29515247452890E+03
 0.29515236711256E+03 0.30522037813255E+03 0.29515690202439E+03 -.34563888375947E+03 -.53039727253421E+03
 0.18733324273837E+03 0.33222094302951E+03 0.14395103407744E+03 0.21663876004501E+03 0.13843621372542E+03
 0.21663876004501E+03 0.23956361718775E+03 0.21714186295725E+03 0.13623857482269E+03 0.21714186295725E+03
 0.23791820156664E+03 0.21663876004501E+03 0.13843621372542E+03 0.21663876004501E+03 0.23956361718775E+03
 0.21714186295724E+03 0.13623857482268E+03 0.21714186295724E+03 0.23791820156664E+03 0.24109316200664E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33508797709707E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94551157197489E+00 0.93479886384672E+00 0.00000000000000E+00 0.94551157197489E+00 0.93479886384672E+00
 0.13946617798097E+01 0.36466180841993E+00 0.10299999713898E+01 0.15449999570847E+01 0.12397043247711E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.73528672008860E-03 0.00000000000000E+00
 0.13070774988591E-01 0.00000000000000E+00 0.12397043247711E-01 0.13806061708680E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1800.00000000
 0.72945995633399E+00 0.31468774829711E+03 0.51709478834904E+03 0.46787884481638E+03 0.44716060390465E+03
 0.22979093444258E+00 0.00000000000000E+00 0.19439824571962E+00 0.00000000000000E+00 -.19814782218459E+01
 0.48817638973617E-02 0.43602041579305E+00 0.16387519282371E+04 0.61453197308893E+03 0.18347764715212E+02
 0.68804117682047E+01 0.38932896753773E+03 0.29538685142127E+03 0.38183442072483E+03 0.43000176841718E+03
 0.29522567899135E+03 0.29528076312100E+03 0.37295662767452E+03 0.42984341761819E+03 0.29521393359109E+03
 0.29528051358462E+03 0.38183442072483E+03 0.43000176841718E+03 0.29522567899135E+03 0.29528076312100E+03
 0.37295662767452E+03 0.42984341761819E+03 0.29521393359109E+03 0.29528051358462E+03 0.47297713334764E+03
 0.35970796194851E+03 0.26965419402442E+04 0.21711055672601E+04 0.51342797059249E+03 0.90130923492573E+03
 0.38531412448028E+03 0.17424878867268E+04 0.13698081122702E+04 0.13698523721619E+04 0.20113951881449E+04
 0.15694735143566E+04 0.13666463021507E+04 0.12588721289675E+04 0.20096433623621E+04 0.17424878867268E+04
 0.13698081122702E+04 0.13698523721619E+04 0.20113951881449E+04 0.15694735143566E+04 0.13666463021507E+04
 0.12588721289675E+04 0.20096433623621E+04 0.18438260477894E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54139203103706E+03 0.18908950244215E+01
 0.18908950244215E+01 0.14040735711909E+02 0.00000000000000E+00 0.32551839830189E+03 0.32551839830189E+03
 0.32551839830189E+03 0.32551839830189E+03 0.00000000000000E+00 0.00000000000000E+00 0.21472407847998E+00
 0.00000000000000E+00 0.98807949990454E+01 0.10000000000000E-02 0.27970412986453E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28601651337342E+02 0.10725619251503E+02 0.35971091102181E+03 0.47297431416843E+03
 0.30578192914697E+03 0.30578192914697E+03 0.29515259014421E+03 0.29515247770912E+03 0.30569437986345E+03
 0.30569437986345E+03 0.29515257098282E+03 0.29515245937950E+03 0.30578192914697E+03 0.30578192914697E+03
 0.29515259014421E+03 0.29515247770912E+03 0.30569437986345E+03 0.30569437986345E+03 0.29515257098282E+03
 0.29515245937950E+03 0.30528888588781E+03 0.29515715035098E+03 -.34679516945530E+03 -.53209080900181E+03
 0.18788356127711E+03 0.33317606618064E+03 0.14435308709714E+03 0.21746641456635E+03 0.13887581982562E+03
 0.21746641456635E+03 0.24026569590583E+03 0.21797190228981E+03 0.13667627675711E+03 0.21797190228981E+03
 0.23862080915876E+03 0.21746641456635E+03 0.13887581982563E+03 0.21746641456635E+03 0.24026569590584E+03
 0.21797190228981E+03 0.13667627675711E+03 0.21797190228981E+03 0.23862080915875E+03 0.24142784085019E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33520828273557E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94561021214888E+00 0.93491318112874E+00 0.00000000000000E+00 0.94561021214888E+00 0.93491318112874E+00
 0.13947299495568E+01 0.36472997816700E+00 0.10299999713898E+01 0.15449999570847E+01 0.12394502852811E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.73813126544710E-03 0.00000000000000E+00
 0.13056328396995E-01 0.00000000000000E+00 0.12394502852811E-01 0.13794459662443E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1810.00000000
 0.72959556119174E+00 0.31474958133302E+03 0.51724136941408E+03 0.46799566616017E+03 0.44726267444907E+03
 0.22979029272192E+00 0.00000000000000E+00 0.19440139688858E+00 0.00000000000000E+00 -.19828164821711E+01
 0.48948407817874E-02 0.43585899809699E+00 0.16343738962391E+04 0.61289021108967E+03 0.18354559696895E+02
 0.68829598863356E+01 0.38957633856531E+03 0.29539360270683E+03 0.38206779981933E+03 0.43029119893308E+03
 0.29522815429520E+03 0.29528500213345E+03 0.37318333553138E+03 0.43013287268475E+03 0.29521604297412E+03
 0.29528474508119E+03 0.38206779981933E+03 0.43029119893308E+03 0.29522815429520E+03 0.29528500213345E+03
 0.37318333553138E+03 0.43013287268475E+03 0.29521604297412E+03 0.29528474508119E+03 0.47318659074095E+03
 0.35990547641785E+03 0.26970867719606E+04 0.21700276096579E+04 0.51269758538514E+03 0.89985851532988E+03
 0.38459744201781E+03 0.17431118611589E+04 0.13689848417758E+04 0.13693015550244E+04 0.20092009277910E+04
 0.15703546358379E+04 0.13658330909205E+04 0.12586619871665E+04 0.20074581256129E+04 0.17431118611589E+04
 0.13689848417758E+04 0.13693015550244E+04 0.20092009277910E+04 0.15703546358379E+04 0.13658330909205E+04
 0.12586619871665E+04 0.20074581256129E+04 0.18421453580035E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54152234283023E+03 0.18908950374291E+01
 0.18908950374291E+01 0.14120735711909E+02 0.00000000000000E+00 0.32563513367218E+03 0.32563513367218E+03
 0.32563513367218E+03 0.32563513367218E+03 0.00000000000000E+00 0.00000000000000E+00 0.21465947467772E+00
 0.00000000000000E+00 0.98846164805444E+01 0.10000000000000E-02 0.28091585419205E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28478278746528E+02 0.10679354529948E+02 0.35990839721431E+03 0.47318380510473E+03
 0.30585503362197E+03 0.30585503362197E+03 0.29515269044302E+03 0.29515257365405E+03 0.30576713714175E+03
 0.30576713714175E+03 0.29515267039387E+03 0.29515255447521E+03 0.30585503362197E+03 0.30585503362197E+03
 0.29515269044302E+03 0.29515257365405E+03 0.30576713714175E+03 0.30576713714175E+03 0.29515267039387E+03
 0.29515255447521E+03 0.30535727164320E+03 0.29515740551435E+03 -.34794020801815E+03 -.53376609432887E+03
 0.18842810424090E+03 0.33412084808566E+03 0.14475060332356E+03 0.21828756270361E+03 0.13931127816303E+03
 0.21828756270361E+03 0.24096037590475E+03 0.21879542309558E+03 0.13710991320764E+03 0.21879542309558E+03
 0.23931608216333E+03 0.21828756270361E+03 0.13931127816303E+03 0.21828756270361E+03 0.24096037590475E+03
 0.21879542309558E+03 0.13710991320764E+03 0.21879542309558E+03 0.23931608216332E+03 0.24175938424084E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33532790260011E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94570833734145E+00 0.93502682344396E+00 0.00000000000000E+00 0.94570833734145E+00 0.93502682344396E+00
 0.13947977519856E+01 0.36479778059587E+00 0.10299999713898E+01 0.15449999570847E+01 0.12391969312222E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.74094436936842E-03 0.00000000000000E+00
 0.13042017261544E-01 0.00000000000000E+00 0.12391969312222E-01 0.13782961630912E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1820.00000000
 0.72973043864677E+00 0.31481118904700E+03 0.51738723121958E+03 0.46811192984828E+03 0.44736427674324E+03
 0.22978965364918E+00 0.00000000000000E+00 0.19440452789323E+00 0.00000000000000E+00 -.19841490882858E+01
 0.49078554668329E-02 0.43569851999344E+00 0.16300398522458E+04 0.61126494459217E+03 0.18361320116764E+02
 0.68854950437866E+01 0.38982272519345E+03 0.29540048080003E+03 0.38230029183527E+03 0.43057918216151E+03
 0.29523068625733E+03 0.29528933641868E+03 0.37340924673165E+03 0.43042088252221E+03 0.29521820131435E+03
 0.29528907169952E+03 0.38230029183527E+03 0.43057918216151E+03 0.29523068625733E+03 0.29528933641868E+03
 0.37340924673165E+03 0.43042088252221E+03 0.29521820131435E+03 0.29528907169952E+03 0.47339461644650E+03
 0.36010130058201E+03 0.26976250970646E+04 0.21689490976687E+04 0.51197884188955E+03 0.89842762231673E+03
 0.38388888621773E+03 0.17437310095403E+04 0.13681607906946E+04 0.13687499070881E+04 0.20070138331228E+04
 0.15712297871806E+04 0.13650190676741E+04 0.12584490492251E+04 0.20052800091502E+04 0.17437310095403E+04
 0.13681607906946E+04 0.13687499070881E+04 0.20070138331228E+04 0.15712297871806E+04 0.13650190676741E+04
 0.12584490492251E+04 0.20052800091502E+04 0.18404724984321E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54165202342487E+03 0.18908950503817E+01
 0.18908950503817E+01 0.14200735711909E+02 0.00000000000000E+00 0.32575120610205E+03 0.32575120610205E+03
 0.32575120610205E+03 0.32575120610205E+03 0.00000000000000E+00 0.00000000000000E+00 0.21459507636188E+00
 0.00000000000000E+00 0.98884189265528E+01 0.10000000000000E-02 0.28212312487222E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28356413546828E+02 0.10633655080061E+02 0.36010419326102E+03 0.47339186408204E+03
 0.30592800722846E+03 0.30592800722846E+03 0.29515279379544E+03 0.29515267252004E+03 0.30583976607703E+03
 0.30583976607703E+03 0.29515277282651E+03 0.29515265246134E+03 0.30592800722846E+03 0.30592800722846E+03
 0.29515279379544E+03 0.29515267252004E+03 0.30583976607703E+03 0.30583976607703E+03 0.29515277282651E+03
 0.29515265246134E+03 0.30542553461266E+03 0.29515766763857E+03 -.34907412930902E+03 -.53542335408298E+03
 0.18896694627022E+03 0.33505542134510E+03 0.14514364034353E+03 0.21910226797641E+03 0.13974263732893E+03
 0.21910226797641E+03 0.24164774738709E+03 0.21961248885851E+03 0.13753953118004E+03 0.21961248885851E+03
 0.24000410920966E+03 0.21910226797641E+03 0.13974263732893E+03 0.21910226797641E+03 0.24164774738709E+03
 0.21961248885851E+03 0.13753953118003E+03 0.21961248885851E+03 0.24000410920965E+03 0.24208783777744E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33544684342934E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94580595244371E+00 0.93513979798463E+00 0.00000000000000E+00 0.94580595244371E+00 0.93513979798463E+00
 0.13948651907132E+01 0.36486521932338E+00 0.10299999713898E+01 0.15449999570847E+01 0.12389442665678E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.74372649124826E-03 0.00000000000000E+00
 0.13027839704478E-01 0.00000000000000E+00 0.12389442665678E-01 0.13771566195727E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1830.00000000
 0.72986459584063E+00 0.31487257303904E+03 0.51753238048915E+03 0.46822764100325E+03 0.44746541498100E+03
 0.22978901721015E+00 0.00000000000000E+00 0.19440763896868E+00 0.00000000000000E+00 -.19854760860184E+01
 0.49208083608599E-02 0.43553897153742E+00 0.16257491479717E+04 0.60965593048938E+03 0.18368046312275E+02
 0.68880173671032E+01 0.39006813504665E+03 0.29540748660231E+03 0.38253190351439E+03 0.43086573113390E+03
 0.29523327552614E+03 0.29529376700250E+03 0.37363436689028E+03 0.43070746013392E+03 0.29522040919234E+03
 0.29529349446426E+03 0.38253190351439E+03 0.43086573113390E+03 0.29523327552614E+03 0.29529376700250E+03
 0.37363436689028E+03 0.43070746013392E+03 0.29522040919234E+03 0.29529349446426E+03 0.47360122678818E+03
 0.36029545575789E+03 0.26981569831446E+04 0.21678700574240E+04 0.51127160605174E+03 0.89701630641258E+03
 0.38318834233058E+03 0.17443453767368E+04 0.13673359948202E+04 0.13681974460896E+04 0.20048338641777E+04
 0.15720990214468E+04 0.13642042681340E+04 0.12582333500032E+04 0.20031089731822E+04 0.17443453767368E+04
 0.13673359948202E+04 0.13681974460896E+04 0.20048338641777E+04 0.15720990214468E+04 0.13642042681340E+04
 0.12582333500032E+04 0.20031089731822E+04 0.18388073984972E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54178107867205E+03 0.18908950632799E+01
 0.18908950632799E+01 0.14280735711909E+02 0.00000000000000E+00 0.32586662204471E+03 0.32586662204471E+03
 0.32586662204471E+03 0.32586662204471E+03 0.00000000000000E+00 0.00000000000000E+00 0.21453088298461E+00
 0.00000000000000E+00 0.98922025096969E+01 0.10000000000000E-02 0.28332595942745E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28236028975836E+02 0.10588510865938E+02 0.36029832048407E+03 0.47359850742207E+03
 0.30600084927668E+03 0.30600084927668E+03 0.29515290026741E+03 0.29515277437015E+03 0.30591226597737E+03
 0.30591226597737E+03 0.29515287834591E+03 0.29515275340024E+03 0.30600084927668E+03 0.30600084927668E+03
 0.29515290026741E+03 0.29515277437015E+03 0.30591226597737E+03 0.30591226597737E+03 0.29515287834591E+03
 0.29515275340024E+03 0.30549367404837E+03 0.29515793684841E+03 -.35019706180413E+03 -.53706281124883E+03
 0.18950016098085E+03 0.33597991661467E+03 0.14553225482892E+03 0.21991059333262E+03 0.14016994531389E+03
 0.21991059333262E+03 0.24232789932540E+03 0.22042316249600E+03 0.13796517710794E+03 0.22042316249600E+03
 0.24068497773125E+03 0.21991059333262E+03 0.14016994531389E+03 0.21991059333262E+03 0.24232789932541E+03
 0.22042316249599E+03 0.13796517710794E+03 0.22042316249599E+03 0.24068497773124E+03 0.24241324494124E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33556511186593E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94590306229618E+00 0.93525211180967E+00 0.00000000000000E+00 0.94590306229618E+00 0.93525211180967E+00
 0.13949322693101E+01 0.36493229792032E+00 0.10299999713898E+01 0.15449999570847E+01 0.12386922942129E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.74647808306775E-03 0.00000000000000E+00
 0.13013793906096E-01 0.00000000000000E+00 0.12386922942129E-01 0.13760271989163E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1840.00000000
 0.72999803982079E+00 0.31493373489281E+03 0.51767682386255E+03 0.46834280468418E+03 0.44756609330460E+03
 0.22978838339030E+00 0.00000000000000E+00 0.19441073034575E+00 0.00000000000000E+00 -.19867975205216E+01
 0.49336998761577E-02 0.43538034293163E+00 0.16215011453494E+04 0.60806292950603E+03 0.18374738616199E+02
 0.68905269810748E+01 0.39031257565507E+03 0.29541462100031E+03 0.38276264151500E+03 0.43115085870843E+03
 0.29523592274567E+03 0.29529829490171E+03 0.37385870155617E+03 0.43099261835032E+03 0.29522266718528E+03
 0.29529801439103E+03 0.38276264151500E+03 0.43115085870843E+03 0.29523592274567E+03 0.29529829490171E+03
 0.37385870155617E+03 0.43099261835032E+03 0.29522266718528E+03 0.29529801439103E+03 0.47380643789135E+03
 0.36048796295422E+03 0.26986824971497E+04 0.21667905148977E+04 0.51057574455148E+03 0.89562432043361E+03
 0.38249569715937E+03 0.17449550071947E+04 0.13665104895560E+04 0.13676441896689E+04 0.20026609817120E+04
 0.15729623911859E+04 0.13633887276349E+04 0.12580149240461E+04 0.20009449786349E+04 0.17449550071947E+04
 0.13665104895560E+04 0.13676441896689E+04 0.20026609817120E+04 0.15729623911859E+04 0.13633887276350E+04
 0.12580149240461E+04 0.20009449786349E+04 0.18371499884689E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54190951434997E+03 0.18908950761239E+01
 0.18908950761239E+01 0.14360735711909E+02 0.00000000000000E+00 0.32598138786909E+03 0.32598138786909E+03
 0.32598138786909E+03 0.32598138786909E+03 0.00000000000000E+00 0.00000000000000E+00 0.21446689399768E+00
 0.00000000000000E+00 0.98959674016555E+01 0.10000000000000E-02 0.28452437528887E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28117098901906E+02 0.10543912088215E+02 0.36049079989706E+03 0.47380375124818E+03
 0.30607355910836E+03 0.30607355910836E+03 0.29515300992560E+03 0.29515287926818E+03 0.30598463618153E+03
 0.30598463618153E+03 0.29515298701796E+03 0.29515285735494E+03 0.30607355910836E+03 0.30607355910836E+03
 0.29515300992560E+03 0.29515287926818E+03 0.30598463618153E+03 0.30598463618153E+03 0.29515298701796E+03
 0.29515285735494E+03 0.30556168923966E+03 0.29515821326936E+03 -.35130913261647E+03 -.53868468625087E+03
 0.19002782099359E+03 0.33689446268004E+03 0.14591650258148E+03 0.22071260116340E+03 0.14059324952640E+03
 0.22071260116340E+03 0.24300091951039E+03 0.22122750637358E+03 0.13838689687084E+03 0.22122750637358E+03
 0.24135877401290E+03 0.22071260116340E+03 0.14059324952640E+03 0.22071260116340E+03 0.24300091951040E+03
 0.22122750637357E+03 0.13838689687084E+03 0.22122750637357E+03 0.24135877401290E+03 0.24273564736421E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33568271446187E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94599967170892E+00 0.93536377182259E+00 0.00000000000000E+00 0.94599967170892E+00 0.93536377182259E+00
 0.13949989913002E+01 0.36499901991040E+00 0.10299999713898E+01 0.15449999570847E+01 0.12384410160819E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.74919958966309E-03 0.00000000000000E+00
 0.12999878100354E-01 0.00000000000000E+00 0.12384410160819E-01 0.13749077690017E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1850.00000000
 0.73013077754053E+00 0.31499467617548E+03 0.51782056789729E+03 0.46845742588789E+03 0.44766631580565E+03
 0.22978775217482E+00 0.00000000000000E+00 0.19441380225111E+00 0.00000000000000E+00 -.19881134364527E+01
 0.49465304280564E-02 0.43522262452332E+00 0.16172952165875E+04 0.60648570622030E+03 0.18381397356725E+02
 0.68930240087719E+01 0.39055605445653E+03 0.29542188486581E+03 0.38299251241372E+03 0.43143457757287E+03
 0.29523862855547E+03 0.29530292112387E+03 0.37408225621368E+03 0.43127636983175E+03 0.29522497586692E+03
 0.29530263248626E+03 0.38299251241372E+03 0.43143457757287E+03 0.29523862855547E+03 0.29530292112387E+03
 0.37408225621368E+03 0.43127636983174E+03 0.29522497586692E+03 0.29530263248626E+03 0.47401026567962E+03
 0.36067884287246E+03 0.26992017053848E+04 0.21657104958856E+04 0.50989112494163E+03 0.89425141966572E+03
 0.38181083909938E+03 0.17455599449396E+04 0.13656843099140E+04 0.13670901553563E+04 0.20004951471813E+04
 0.15738199484325E+04 0.13625724811222E+04 0.12577938055728E+04 0.19987879871328E+04 0.17455599449396E+04
 0.13656843099139E+04 0.13670901553564E+04 0.20004951471813E+04 0.15738199484325E+04 0.13625724811222E+04
 0.12577938055728E+04 0.19987879871328E+04 0.18355001994931E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54203733616536E+03 0.18908950889143E+01
 0.18908950889143E+01 0.14440735711909E+02 0.00000000000000E+00 0.32609550986260E+03 0.32609550986260E+03
 0.32609550986260E+03 0.32609550986260E+03 0.00000000000000E+00 0.00000000000000E+00 0.21440310885255E+00
 0.00000000000000E+00 0.98997137728176E+01 0.10000000000000E-02 0.28571838979489E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27999597805878E+02 0.10499849177204E+02 0.36068165220605E+03 0.47400761148222E+03
 0.30614613609738E+03 0.30614613609738E+03 0.29515312283739E+03 0.29515298727857E+03 0.30605687605966E+03
 0.30605687605966E+03 0.29515309890927E+03 0.29515296438915E+03 0.30614613609738E+03 0.30614613609738E+03
 0.29515312283739E+03 0.29515298727857E+03 0.30605687605966E+03 0.30605687605966E+03 0.29515309890927E+03
 0.29515296438915E+03 0.30562957951332E+03 0.29515849702758E+03 -.35241046745950E+03 -.54028919688863E+03
 0.19054999793308E+03 0.33779918647142E+03 0.14629643854867E+03 0.22150835328308E+03 0.14101259678932E+03
 0.22150835328308E+03 0.24366689455665E+03 0.22202558228479E+03 0.13880473579034E+03 0.22202558228479E+03
 0.24202558319629E+03 0.22150835328309E+03 0.14101259678933E+03 0.22150835328309E+03 0.24366689455665E+03
 0.22202558228479E+03 0.13880473579034E+03 0.22202558228479E+03 0.24202558319628E+03 0.24305508503620E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33579965768200E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94609578542080E+00 0.93547478483728E+00 0.00000000000000E+00 0.94609578542080E+00 0.93547478483728E+00
 0.13950653601600E+01 0.36506538877027E+00 0.10299999713898E+01 0.15449999570847E+01 0.12381904332812E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.75189144881511E-03 0.00000000000000E+00
 0.12986090569674E-01 0.00000000000000E+00 0.12381904332812E-01 0.13737982018489E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1860.00000000
 0.73026281586218E+00 0.31505539843756E+03 0.51796361906901E+03 0.46857150954904E+03 0.44776608652504E+03
 0.22978712354871E+00 0.00000000000000E+00 0.19441685490737E+00 0.00000000000000E+00 -.19894238779457E+01
 0.49593004337286E-02 0.43506580680218E+00 0.16131307443266E+04 0.60492402912249E+03 0.18388022857511E+02
 0.68955085715667E+01 0.39079857879820E+03 0.29542927905569E+03 0.38322152270703E+03 0.43171690024772E+03
 0.29524139359046E+03 0.29530764666717E+03 0.37430503628397E+03 0.43155872707155E+03 0.29522733580745E+03
 0.29530734974703E+03 0.38322152270703E+03 0.43171690024772E+03 0.29524139359046E+03 0.29530764666717E+03
 0.37430503628397E+03 0.43155872707155E+03 0.29522733580745E+03 0.29530734974703E+03 0.47421272587628E+03
 0.36086811591146E+03 0.26997146735003E+04 0.21646300259826E+04 0.50921761567082E+03 0.89289736186905E+03
 0.38113365811988E+03 0.17461602335723E+04 0.13648574904967E+04 0.13665353605599E+04 0.19983363226931E+04
 0.15746717447042E+04 0.13617555631343E+04 0.12575700284661E+04 0.19966379609516E+04 0.17461602335723E+04
 0.13648574904966E+04 0.13665353605599E+04 0.19983363226931E+04 0.15746717447042E+04 0.13617555631343E+04
 0.12575700284661E+04 0.19966379609516E+04 0.18338579635625E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54216454975374E+03 0.18908951016515E+01
 0.18908951016515E+01 0.14520735711909E+02 0.00000000000000E+00 0.32620899423369E+03 0.32620899423369E+03
 0.32620899423369E+03 0.32620899423369E+03 0.00000000000000E+00 0.00000000000000E+00 0.21433952700045E+00
 0.00000000000000E+00 0.99034417921053E+01 0.10000000000000E-02 0.28690802019026E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27883500763397E+02 0.10456312786274E+02 0.36087089781426E+03 0.47421010384584E+03
 0.30621857964963E+03 0.30621857964963E+03 0.29515323907089E+03 0.29515309846647E+03 0.30612898501316E+03
 0.30612898501316E+03 0.29515321408716E+03 0.29515307456726E+03 0.30621857964963E+03 0.30621857964963E+03
 0.29515323907089E+03 0.29515309846647E+03 0.30612898501316E+03 0.30612898501316E+03 0.29515321408716E+03
 0.29515307456726E+03 0.30569734423304E+03 0.29515878824988E+03 -.35350119065030E+03 -.54187655834095E+03
 0.19106676244111E+03 0.33869421309784E+03 0.14667211684452E+03 0.22229791092888E+03 0.14142803334616E+03
 0.22229791092888E+03 0.24432590992186E+03 0.22281745145068E+03 0.13921873863609E+03 0.22281745145068E+03
 0.24268548929852E+03 0.22229791092888E+03 0.14142803334616E+03 0.22229791092888E+03 0.24432590992186E+03
 0.22281745145068E+03 0.13921873863609E+03 0.22281745145068E+03 0.24268548929852E+03 0.24337159643409E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33591594790718E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94619140809349E+00 0.93558515758678E+00 0.00000000000000E+00 0.94619140809349E+00 0.93558515758678E+00
 0.13951313793209E+01 0.36513140793109E+00 0.10299999713898E+01 0.15449999570847E+01 0.12379405462598E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.75455409129005E-03 0.00000000000000E+00
 0.12972429639547E-01 0.00000000000000E+00 0.12379405462598E-01 0.13726983730837E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1870.00000000
 0.73039416155942E+00 0.31511590321312E+03 0.51810598377200E+03 0.46868506054044E+03 0.44786540945325E+03
 0.22978649749681E+00 0.00000000000000E+00 0.19441988853327E+00 0.00000000000000E+00 -.19907288884491E+01
 0.49720103107752E-02 0.43490988039735E+00 0.16090071218603E+04 0.60337767069761E+03 0.18394615437780E+02
 0.68979807891674E+01 0.39104015593830E+03 0.29543680441192E+03 0.38344967881271E+03 0.43199783908975E+03
 0.29524421848086E+03 0.29531247252019E+03 0.37452704712596E+03 0.43183970239971E+03 0.29522974757341E+03
 0.29531216716082E+03 0.38344967881271E+03 0.43199783908975E+03 0.29524421848086E+03 0.29531247252019E+03
 0.37452704712596E+03 0.43183970239971E+03 0.29522974757341E+03 0.29531216716082E+03 0.47441383400967E+03
 0.36105580217705E+03 0.27002214665003E+04 0.21635491305821E+04 0.50855508602087E+03 0.89156190715672E+03
 0.38046404570574E+03 0.17467559162743E+04 0.13640300654854E+04 0.13659798225647E+04 0.19961844709623E+04
 0.15755178310095E+04 0.13609380077907E+04 0.12573436262787E+04 0.19944948629737E+04 0.17467559162743E+04
 0.13640300654854E+04 0.13659798225647E+04 0.19961844709623E+04 0.15755178310095E+04 0.13609380077907E+04
 0.12573436262787E+04 0.19944948629737E+04 0.18322232134667E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54229116067982E+03 0.18908951143360E+01
 0.18908951143360E+01 0.14600735711909E+02 0.00000000000000E+00 0.32632184711516E+03 0.32632184711516E+03
 0.32632184711516E+03 0.32632184711516E+03 0.00000000000000E+00 0.00000000000000E+00 0.21427614789247E+00
 0.00000000000000E+00 0.99071516272586E+01 0.10000000000000E-02 0.28809328362468E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27768783427878E+02 0.10413293785454E+02 0.36105855683156E+03 0.47441124386593E+03
 0.30629088920171E+03 0.30629088920171E+03 0.29515335869488E+03 0.29515321289768E+03 0.30620096247344E+03
 0.30620096247344E+03 0.29515333261962E+03 0.29515318795432E+03 0.30629088920171E+03 0.30629088920171E+03
 0.29515335869488E+03 0.29515321289768E+03 0.30620096247344E+03 0.30620096247344E+03 0.29515333261962E+03
 0.29515318795432E+03 0.30576498279769E+03 0.29515908706370E+03 -.35458142516089E+03 -.54344698324728E+03
 0.19157818421282E+03 0.33957966592048E+03 0.14704359078660E+03 0.22308133478838E+03 0.14183960488378E+03
 0.22308133478838E+03 0.24497804995390E+03 0.22360317454698E+03 0.13962894964741E+03 0.22360317454698E+03
 0.24333857525786E+03 0.22308133478839E+03 0.14183960488378E+03 0.22308133478839E+03 0.24497804995391E+03
 0.22360317454697E+03 0.13962894964740E+03 0.22360317454697E+03 0.24333857525785E+03 0.24368521864214E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33603159143745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94628654437561E+00 0.93569489662789E+00 0.00000000000000E+00 0.94628654437561E+00 0.93569489662789E+00
 0.13951970521695E+01 0.36519708077971E+00 0.10299999713898E+01 0.15449999570847E+01 0.12376913547728E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.75718794122774E-03 0.00000000000000E+00
 0.12958893678635E-01 0.00000000000000E+00 0.12376913547728E-01 0.13716081619862E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1880.00000000
 0.73052482131594E+00 0.31517619202004E+03 0.51824766832102E+03 0.46879808367464E+03 0.44796428853168E+03
 0.22978587400388E+00 0.00000000000000E+00 0.19442290334388E+00 0.00000000000000E+00 -.19920285108889E+01
 0.49846604764223E-02 0.43475483607337E+00 0.16049237531504E+04 0.60184640743138E+03 0.18401175412457E+02
 0.69004407796714E+01 0.39128079304764E+03 0.29544446176144E+03 0.38367698707122E+03 0.43227740629539E+03
 0.29524710385203E+03 0.29531739966175E+03 0.37474829403730E+03 0.43211930798612E+03 0.29523221172753E+03
 0.29531708570539E+03 0.38367698707121E+03 0.43227740629539E+03 0.29524710385203E+03 0.29531739966175E+03
 0.37474829403730E+03 0.43211930798612E+03 0.29523221172753E+03 0.29531708570539E+03 0.47461360541623E+03
 0.36124192148980E+03 0.27007221487595E+04 0.21624678348861E+04 0.50790340611483E+03 0.89024481798191E+03
 0.37980189483651E+03 0.17473470358191E+04 0.13632020686466E+04 0.13654235585394E+04 0.19940395552973E+04
 0.15763582578614E+04 0.13601198487982E+04 0.12571146322436E+04 0.19923586566744E+04 0.17473470358191E+04
 0.13632020686466E+04 0.13654235585394E+04 0.19940395552973E+04 0.15763582578614E+04 0.13601198487982E+04
 0.12571146322436E+04 0.19923586566744E+04 0.18305958827866E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54241717443920E+03 0.18908951269680E+01
 0.18908951269680E+01 0.14680735711909E+02 0.00000000000000E+00 0.32643407456751E+03 0.32643407456751E+03
 0.32643407456751E+03 0.32643407456751E+03 0.00000000000000E+00 0.00000000000000E+00 0.21421297097960E+00
 0.00000000000000E+00 0.99108434445178E+01 0.10000000000000E-02 0.28927419715128E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27655422014070E+02 0.10370783255276E+02 0.36124464908234E+03 0.47461104687767E+03
 0.30636306421991E+03 0.30636306421991E+03 0.29515348177887E+03 0.29515333063869E+03 0.30627280790099E+03
 0.30627280790099E+03 0.29515345457535E+03 0.29515330461605E+03 0.30636306421991E+03 0.30636306421991E+03
 0.29515348177887E+03 0.29515333063869E+03 0.30627280790099E+03 0.30627280790099E+03 0.29515345457535E+03
 0.29515330461605E+03 0.30583249464000E+03 0.29515939359709E+03 -.35565129265308E+03 -.54500068176043E+03
 0.19208433202651E+03 0.34045566661788E+03 0.14741091293124E+03 0.22385868501911E+03 0.14224735655117E+03
 0.22385868501911E+03 0.24562339793289E+03 0.22438281172332E+03 0.14003541255126E+03 0.22438281172332E+03
 0.24398492297458E+03 0.22385868501911E+03 0.14224735655117E+03 0.22385868501911E+03 0.24562339793290E+03
 0.22438281172331E+03 0.14003541255126E+03 0.22438281172331E+03 0.24398492297457E+03 0.24399598750660E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33614659449563E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94638119883953E+00 0.93580400844366E+00 0.00000000000000E+00 0.94638119883953E+00 0.93580400844366E+00
 0.13952623820477E+01 0.36526241065797E+00 0.10299999713898E+01 0.15449999570847E+01 0.12374428579354E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.75979341636310E-03 0.00000000000000E+00
 0.12945481096318E-01 0.00000000000000E+00 0.12374428579354E-01 0.13705274512681E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1890.00000000
 0.73065480172421E+00 0.31523626636012E+03 0.51838867895344E+03 0.46891058370572E+03 0.44806272765411E+03
 0.22978525305464E+00 0.00000000000000E+00 0.19442589955070E+00 0.00000000000000E+00 -.19933227878295E+01
 0.49972513467681E-02 0.43460066472636E+00 0.16008800528262E+04 0.60033001980983E+03 0.18407703092302E+02
 0.69028886596133E+01 0.39152049721125E+03 0.29545225191625E+03 0.38390345374720E+03 0.43255561390364E+03
 0.29525005032440E+03 0.29532242906074E+03 0.37496878225557E+03 0.43239755584357E+03 0.29523472882872E+03
 0.29532210634855E+03 0.38390345374720E+03 0.43255561390364E+03 0.29525005032440E+03 0.29532242906074E+03
 0.37496878225557E+03 0.43239755584357E+03 0.29523472882872E+03 0.29532210634855E+03 0.47481205524067E+03
 0.36142649338920E+03 0.27012167840381E+04 0.21613861639094E+04 0.50726244699217E+03 0.88894585922568E+03
 0.37914709999855E+03 0.17479336345808E+04 0.13623735333431E+04 0.13648665855378E+04 0.19919015395989E+04
 0.15771930752869E+04 0.13593011194626E+04 0.12568830792794E+04 0.19902293061207E+04 0.17479336345808E+04
 0.13623735333431E+04 0.13648665855378E+04 0.19919015395989E+04 0.15771930752869E+04 0.13593011194626E+04
 0.12568830792794E+04 0.19902293061207E+04 0.18289759059155E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54254259646022E+03 0.18908951395481E+01
 0.18908951395481E+01 0.14760735711909E+02 0.00000000000000E+00 0.32654568258144E+03 0.32654568258144E+03
 0.32654568258144E+03 0.32654568258144E+03 0.00000000000000E+00 0.00000000000000E+00 0.21414999571277E+00
 0.00000000000000E+00 0.99145174082448E+01 0.10000000000000E-02 0.29045077772605E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27543393282098E+02 0.10328772480787E+02 0.36142919410962E+03 0.47480952802465E+03
 0.30643510419989E+03 0.30643510419989E+03 0.29515360839303E+03 0.29515345175663E+03 0.30634452078505E+03
 0.30634452078505E+03 0.29515358002374E+03 0.29515342461882E+03 0.30643510419989E+03 0.30643510419989E+03
 0.29515360839303E+03 0.29515345175663E+03 0.30634452078505E+03 0.30634452078505E+03 0.29515358002374E+03
 0.29515342461882E+03 0.30589987922586E+03 0.29515970797869E+03 -.35671091347996E+03 -.54653786154694E+03
 0.19258527375716E+03 0.34132233522353E+03 0.14777413509759E+03 0.22463002124901E+03 0.14265133296664E+03
 0.22463002124901E+03 0.24626203609391E+03 0.22515642260363E+03 0.14043817056905E+03 0.22515642260363E+03
 0.24462461333317E+03 0.22463002124902E+03 0.14265133296664E+03 0.22463002124902E+03 0.24626203609392E+03
 0.22515642260363E+03 0.14043817056905E+03 0.22515642260363E+03 0.24462461333317E+03 0.24430393779267E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33626096323152E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94647537596866E+00 0.93591249946963E+00 0.00000000000000E+00 0.94647537596866E+00 0.93591249946963E+00
 0.13953273722519E+01 0.36532740086210E+00 0.10299999713898E+01 0.15449999570847E+01 0.12371950546055E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.76237092764562E-03 0.00000000000000E+00
 0.12932190330985E-01 0.00000000000000E+00 0.12371950546055E-01 0.13694561258631E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1900.00000000
 0.73078410928821E+00 0.31529612771910E+03 0.51852902183010E+03 0.46902256532978E+03 0.44816073066705E+03
 0.22978463463381E+00 0.00000000000000E+00 0.19442887736177E+00 0.00000000000000E+00 -.19946117610885E+01
 0.50097833361734E-02 0.43444735738200E+00 0.15968754461367E+04 0.59882829230126E+03 0.18414198783964E+02
 0.69053245439865E+01 0.39175927543006E+03 0.29546017567329E+03 0.38412908503090E+03 0.43283247379890E+03
 0.29525305851332E+03 0.29532756167592E+03 0.37518851695946E+03 0.43267445783052E+03 0.29523729943187E+03
 0.29532723004808E+03 0.38412908503090E+03 0.43283247379890E+03 0.29525305851332E+03 0.29532756167592E+03
 0.37518851695946E+03 0.43267445783052E+03 0.29523729943187E+03 0.29532723004808E+03 0.47500919843677E+03
 0.36160953713730E+03 0.27017054354764E+04 0.21603041424644E+04 0.50663208064673E+03 0.88766479822716E+03
 0.37849955717719E+03 0.17485157545329E+04 0.13615444925263E+04 0.13643089204899E+04 0.19897703883283E+04
 0.15780223328275E+04 0.13584818526805E+04 0.12566489999842E+04 0.19881067759398E+04 0.17485157545329E+04
 0.13615444925263E+04 0.13643089204899E+04 0.19897703883283E+04 0.15780223328275E+04 0.13584818526805E+04
 0.12566489999842E+04 0.19881067759398E+04 0.18273632180468E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54266743210466E+03 0.18908951520767E+01
 0.18908951520767E+01 0.14840735711909E+02 0.00000000000000E+00 0.32665667707972E+03 0.32665667707972E+03
 0.32665667707972E+03 0.32665667707972E+03 0.00000000000000E+00 0.00000000000000E+00 0.21408722154292E+00
 0.00000000000000E+00 0.99181736816251E+01 0.10000000000000E-02 0.29162304220763E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27432674522009E+02 0.10287252945753E+02 0.36161221117873E+03 0.47500670225971E+03
 0.30650700866647E+03 0.30650700866647E+03 0.29515373860824E+03 0.29515357631929E+03 0.30641610064343E+03
 0.30641610064343E+03 0.29515370903485E+03 0.29515354802966E+03 0.30650700866647E+03 0.30650700866647E+03
 0.29515373860824E+03 0.29515357631929E+03 0.30641610064343E+03 0.30641610064343E+03 0.29515370903485E+03
 0.29515354802966E+03 0.30596713605387E+03 0.29516003033770E+03 -.35776040668452E+03 -.54805872778621E+03
 0.19308107638601E+03 0.34217979014818E+03 0.14813330838024E+03 0.22539540257372E+03 0.14305157822201E+03
 0.22539540257372E+03 0.24689404563891E+03 0.22592406628323E+03 0.14083726642043E+03 0.22592406628323E+03
 0.24525772621371E+03 0.22539540257372E+03 0.14305157822201E+03 0.22539540257372E+03 0.24689404563892E+03
 0.22592406628323E+03 0.14083726642042E+03 0.22592406628323E+03 0.24525772621371E+03 0.24460910327523E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33637470372251E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94656908026320E+00 0.93602037593493E+00 0.00000000000000E+00 0.94656908026320E+00 0.93602037593493E+00
 0.13953920260339E+01 0.36539205464411E+00 0.10299999713898E+01 0.15449999570847E+01 0.12369479430873E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.76492087996567E-03 0.00000000000000E+00
 0.12919019857990E-01 0.00000000000000E+00 0.12369479430873E-01 0.13683940737956E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1910.00000000
 0.73091275042636E+00 0.31535577756676E+03 0.51866870303569E+03 0.46913403318512E+03 0.44825830136981E+03
 0.22978401872611E+00 0.00000000000000E+00 0.19443183698175E+00 0.00000000000000E+00 -.19958954721209E+01
 0.50222568567049E-02 0.43429490519362E+00 0.15929093688866E+04 0.59734101333246E+03 0.18420662790031E+02
 0.69077485462615E+01 0.39199713462236E+03 0.29546823381448E+03 0.38435388703947E+03 0.43310799771376E+03
 0.29525612902900E+03 0.29533279845581E+03 0.37540750326980E+03 0.43295002565397E+03 0.29523992408786E+03
 0.29533245775146E+03 0.38435388703947E+03 0.43310799771376E+03 0.29525612902900E+03 0.29533279845581E+03
 0.37540750326980E+03 0.43295002565397E+03 0.29523992408786E+03 0.29533245775146E+03 0.47520504976975E+03
 0.36179107172374E+03 0.27021881655905E+04 0.21592217951479E+04 0.50601218002397E+03 0.88640140475364E+03
 0.37785916382955E+03 0.17490934372464E+04 0.13607149787236E+04 0.13637505801948E+04 0.19876460664692E+04
 0.15788460795380E+04 0.13576620809274E+04 0.12564124266317E+04 0.19859910312807E+04 0.17490934372464E+04
 0.13607149787236E+04 0.13637505801948E+04 0.19876460664692E+04 0.15788460795380E+04 0.13576620809274E+04
 0.12564124266317E+04 0.19859910312807E+04 0.18257577551444E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54279168666804E+03 0.18908951645541E+01
 0.18908951645541E+01 0.14920735711909E+02 0.00000000000000E+00 0.32676706391887E+03 0.32676706391887E+03
 0.32676706391887E+03 0.32676706391887E+03 0.00000000000000E+00 0.00000000000000E+00 0.21402464792103E+00
 0.00000000000000E+00 0.99218124258929E+01 0.10000000000000E-02 0.29279100735725E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27323243538825E+02 0.10246216327059E+02 0.36179371928235E+03 0.47520258434725E+03
 0.30657877717304E+03 0.30657877717304E+03 0.29515387249601E+03 0.29515370439509E+03 0.30648754702198E+03
 0.30648754702198E+03 0.29515384167940E+03 0.29515367491621E+03 0.30657877717304E+03 0.30657877717304E+03
 0.29515387249601E+03 0.29515370439509E+03 0.30648754702198E+03 0.30648754702198E+03 0.29515384167940E+03
 0.29515367491621E+03 0.30603426465449E+03 0.29516036080384E+03 -.35879989001386E+03 -.54956348319680E+03
 0.19357180601586E+03 0.34302814821331E+03 0.14848848316737E+03 0.22615488756173E+03 0.14344813589076E+03
 0.22615488756173E+03 0.24751950675640E+03 0.22668580133387E+03 0.14123274233093E+03 0.22668580133387E+03
 0.24588434051079E+03 0.22615488756173E+03 0.14344813589076E+03 0.22615488756173E+03 0.24751950675640E+03
 0.22668580133386E+03 0.14123274233093E+03 0.22668580133386E+03 0.24588434051078E+03 0.24491151681410E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33648782197621E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94666231610233E+00 0.93612764407083E+00 0.00000000000000E+00 0.94666231610233E+00 0.93612764407083E+00
 0.13954563466029E+01 0.36545637521318E+00 0.10299999713898E+01 0.15449999570847E+01 0.12367015213577E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.76744367198020E-03 0.00000000000000E+00
 0.12905968182594E-01 0.00000000000000E+00 0.12367015213577E-01 0.13673411854574E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1920.00000000
 0.73104073147187E+00 0.31541521735719E+03 0.51880772857986E+03 0.46924499185315E+03 0.44835544351534E+03
 0.22978340531633E+00 0.00000000000000E+00 0.19443477861200E+00 0.00000000000000E+00 -.19971739618029E+01
 0.50346723175801E-02 0.43414329943946E+00 0.15889812673738E+04 0.59586797526516E+03 0.18427095409118E+02
 0.69101607784191E+01 0.39223408162518E+03 0.29547642710670E+03 0.38457786581824E+03 0.43338219723184E+03
 0.29525926247639E+03 0.29533814033852E+03 0.37562574625049E+03 0.43322427087216E+03 0.29524260334338E+03
 0.29533779039584E+03 0.38457786581824E+03 0.43338219723184E+03 0.29525926247639E+03 0.29533814033852E+03
 0.37562574625049E+03 0.43322427087216E+03 0.29524260334338E+03 0.29533779039584E+03 0.47539962381877E+03
 0.36197111587142E+03 0.27026650362784E+04 0.21581391463410E+04 0.50540261902454E+03 0.88515545098195E+03
 0.37722581886229E+03 0.17496667238943E+04 0.13598850240383E+04 0.13631915813206E+04 0.19855285395068E+04
 0.15796643639935E+04 0.13568418362569E+04 0.12561733911749E+04 0.19838820377941E+04 0.17496667238943E+04
 0.13598850240383E+04 0.13631915813206E+04 0.19855285395068E+04 0.15796643639935E+04 0.13568418362569E+04
 0.12561733911750E+04 0.19838820377941E+04 0.18241594539285E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54291536538060E+03 0.18908951769807E+01
 0.18908951769807E+01 0.15000735711909E+02 0.00000000000000E+00 0.32687684889097E+03 0.32687684889097E+03
 0.32687684889097E+03 0.32687684889097E+03 0.00000000000000E+00 0.00000000000000E+00 0.21396227429817E+00
 0.00000000000000E+00 0.99254338007238E+01 0.10000000000000E-02 0.29395468983874E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27215078638101E+02 0.10205654489288E+02 0.36197373714621E+03 0.47539718886579E+03
 0.30665040930083E+03 0.30665040930083E+03 0.29515401012856E+03 0.29515383605311E+03 0.30655885949387E+03
 0.30655885949387E+03 0.29515397802879E+03 0.29515380534677E+03 0.30665040930083E+03 0.30665040930083E+03
 0.29515401012856E+03 0.29515383605311E+03 0.30655885949387E+03 0.30655885949387E+03 0.29515397802879E+03
 0.29515380534677E+03 0.30610126458908E+03 0.29516069950736E+03 -.35982947994055E+03 -.55105232807394E+03
 0.19405752788961E+03 0.34386752468936E+03 0.14883970916030E+03 0.22690853426393E+03 0.14384104903865E+03
 0.22690853426393E+03 0.24813849864474E+03 0.22744168581300E+03 0.14162464004206E+03 0.22744168581300E+03
 0.24650453415608E+03 0.22690853426394E+03 0.14384104903865E+03 0.22690853426394E+03 0.24813849864474E+03
 0.22744168581300E+03 0.14162464004205E+03 0.22744168581300E+03 0.24650453415608E+03 0.24521121042485E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33660032393250E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94675508785097E+00 0.93623430995361E+00 0.00000000000000E+00 0.94675508785097E+00 0.93623430995361E+00
 0.13955203371257E+01 0.36552036573594E+00 0.10299999713898E+01 0.15449999570847E+01 0.12364557871123E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.76993969623103E-03 0.00000000000000E+00
 0.12893033838116E-01 0.00000000000000E+00 0.12364557871123E-01 0.13662973534347E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1930.00000000
 0.73116805867245E+00 0.31547444852903E+03 0.51894610439870E+03 0.46935544585967E+03 0.44845216081126E+03
 0.22978279438933E+00 0.00000000000000E+00 0.19443770245078E+00 0.00000000000000E+00 -.19984472705811E+01
 0.50470301248193E-02 0.43399253151970E+00 0.15850905982628E+04 0.59440897434853E+03 0.18433496935965E+02
 0.69125613509867E+01 0.39247012319577E+03 0.29548475630178E+03 0.38480102734200E+03 0.43365508379044E+03
 0.29526245945508E+03 0.29534358825158E+03 0.37584325090946E+03 0.43349720489738E+03 0.29524533774090E+03
 0.29534322890779E+03 0.38480102734200E+03 0.43365508379044E+03 0.29526245945508E+03 0.29534358825158E+03
 0.37584325090946E+03 0.43349720489738E+03 0.29524533774090E+03 0.29534322890779E+03 0.47559293497882E+03
 0.36214968804150E+03 0.27031361088308E+04 0.21570562202128E+04 0.50480327252803E+03 0.88392671151098E+03
 0.37659942262031E+03 0.17502356552583E+04 0.13590546601550E+04 0.13626319404058E+04 0.19834177734175E+04
 0.15804772342967E+04 0.13560211503067E+04 0.12559319252517E+04 0.19817797616211E+04 0.17502356552583E+04
 0.13590546601550E+04 0.13626319404059E+04 0.19834177734175E+04 0.15804772342967E+04 0.13560211503067E+04
 0.12559319252517E+04 0.19817797616211E+04 0.18225682518754E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54303847340868E+03 0.18908951893570E+01
 0.18908951893570E+01 0.15080735711909E+02 0.00000000000000E+00 0.32698603772536E+03 0.32698603772536E+03
 0.32698603772536E+03 0.32698603772536E+03 0.00000000000000E+00 0.00000000000000E+00 0.21390010012555E+00
 0.00000000000000E+00 0.99290379640432E+01 0.10000000000000E-02 0.29511410621856E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27108158611962E+02 0.10165559479486E+02 0.36215228323407E+03 0.47559053020979E+03
 0.30672190465824E+03 0.30672190465824E+03 0.29515415157874E+03 0.29515397136303E+03 0.30663003765894E+03
 0.30663003765894E+03 0.29515411815506E+03 0.29515393939025E+03 0.30672190465824E+03 0.30672190465824E+03
 0.29515415157874E+03 0.29515397136303E+03 0.30663003765894E+03 0.30663003765894E+03 0.29515411815506E+03
 0.29515393939025E+03 0.30616813544895E+03 0.29516104657901E+03 -.36084929167828E+03 -.55252546031768E+03
 0.19453830640639E+03 0.34469803332972E+03 0.14918703539129E+03 0.22765640022022E+03 0.14423036023275E+03
 0.22765640022022E+03 0.24875109953272E+03 0.22819177727028E+03 0.14201300081974E+03 0.22819177727028E+03
 0.24711838413815E+03 0.22765640022022E+03 0.14423036023275E+03 0.22765640022022E+03 0.24875109953272E+03
 0.22819177727028E+03 0.14201300081974E+03 0.22819177727028E+03 0.24711838413815E+03 0.24550821535356E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33671221546525E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94684739979456E+00 0.93634037960837E+00 0.00000000000000E+00 0.94684739979456E+00 0.93634037960837E+00
 0.13955840007260E+01 0.36558402933622E+00 0.10299999713898E+01 0.15449999570847E+01 0.12362107377641E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.77240933933430E-03 0.00000000000000E+00
 0.12880215385468E-01 0.00000000000000E+00 0.12362107377641E-01 0.13652624724802E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1940.00000000
 0.73129473819130E+00 0.31553347250557E+03 0.51908383635598E+03 0.46946539967574E+03 0.44854845692062E+03
 0.22978218593005E+00 0.00000000000000E+00 0.19444060869324E+00 0.00000000000000E+00 -.19997154384050E+01
 0.50593306808955E-02 0.43384259295411E+00 0.15812368284621E+04 0.59296381067327E+03 0.18439867661510E+02
 0.69149503730661E+01 0.39270526601299E+03 0.29549322213649E+03 0.38502337751619E+03 0.43392666868320E+03
 0.29526572055922E+03 0.29534914311185E+03 0.37606002219967E+03 0.43376883899849E+03 0.29524812781855E+03
 0.29534877420323E+03 0.38502337751619E+03 0.43392666868320E+03 0.29526572055922E+03 0.29534914311185E+03
 0.37606002219966E+03 0.43376883899849E+03 0.29524812781855E+03 0.29534877420323E+03 0.47578499746230E+03
 0.36232680643765E+03 0.27036014439363E+04 0.21559730407180E+04 0.50421401641812E+03 0.88271496337606E+03
 0.37597987687585E+03 0.17508002717322E+04 0.13582239183419E+04 0.13620716738583E+04 0.19813137346538E+04
 0.15812847380834E+04 0.13552000543004E+04 0.12556880601855E+04 0.19796841693790E+04 0.17508002717322E+04
 0.13582239183419E+04 0.13620716738583E+04 0.19813137346538E+04 0.15812847380834E+04 0.13552000543004E+04
 0.12556880601855E+04 0.19796841693790E+04 0.18209840872118E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54316101585574E+03 0.18908952016833E+01
 0.18908952016833E+01 0.15160735711909E+02 0.00000000000000E+00 0.32709463609010E+03 0.32709463609010E+03
 0.32709463609010E+03 0.32709463609010E+03 0.00000000000000E+00 0.00000000000000E+00 0.21383812485455E+00
 0.00000000000000E+00 0.99326250720378E+01 0.10000000000000E-02 0.29626927296605E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27002462725578E+02 0.10125923522092E+02 0.36232937575194E+03 0.47578262259128E+03
 0.30679326288030E+03 0.30679326288030E+03 0.29515429692006E+03 0.29515411039518E+03 0.30670108114322E+03
 0.30670108114322E+03 0.29515426213089E+03 0.29515407711618E+03 0.30679326288030E+03 0.30679326288030E+03
 0.29515429692006E+03 0.29515411039518E+03 0.30670108114322E+03 0.30670108114322E+03 0.29515426213089E+03
 0.29515407711618E+03 0.30623487685462E+03 0.29516140214998E+03 -.36185943919111E+03 -.55398307545140E+03
 0.19501420513414E+03 0.34551978639820E+03 0.14953051023839E+03 0.22839854246234E+03 0.14461611154809E+03
 0.22839854246234E+03 0.24935738669568E+03 0.22893613275026E+03 0.14239786546060E+03 0.22893613275026E+03
 0.24772596651803E+03 0.22839854246234E+03 0.14461611154809E+03 0.22839854246234E+03 0.24935738669568E+03
 0.22893613275026E+03 0.14239786546060E+03 0.22893613275026E+03 0.24772596651803E+03 0.24580256214617E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33682350238411E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94693925617338E+00 0.93644585896035E+00 0.00000000000000E+00 0.94693925617338E+00 0.93644585896035E+00
 0.13956473404854E+01 0.36564736909565E+00 0.10299999713898E+01 0.15449999570847E+01 0.12359663705112E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.77485298204049E-03 0.00000000000000E+00
 0.12867511410753E-01 0.00000000000000E+00 0.12359663705112E-01 0.13642364392793E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1950.00000000
 0.73142077610933E+00 0.31559229069489E+03 0.51922093024401E+03 0.46957485771831E+03 0.44864433546230E+03
 0.22978157992357E+00 0.00000000000000E+00 0.19444349753151E+00 0.00000000000000E+00 -.20009785047189E+01
 0.50715743844729E-02 0.43369347538006E+00 0.15774194349772E+04 0.59153228811644E+03 0.18446207872944E+02
 0.69173279523539E+01 0.39293951667862E+03 0.29550182533260E+03 0.38524492217818E+03 0.43419696306262E+03
 0.29526904637743E+03 0.29535480582533E+03 0.37627606502000E+03 0.43403918430350E+03 0.29525097411008E+03
 0.29535442718725E+03 0.38524492217818E+03 0.43419696306262E+03 0.29526904637743E+03 0.29535480582533E+03
 0.37627606502000E+03 0.43403918430350E+03 0.29525097411008E+03 0.29535442718725E+03 0.47597582530086E+03
 0.36250248901010E+03 0.27040611016810E+04 0.21548896315890E+04 0.50363472759445E+03 0.88151998604354E+03
 0.37536708481111E+03 0.17513606133231E+04 0.13573928294466E+04 0.13615107979503E+04 0.19792163901198E+04
 0.15820869225245E+04 0.13543785790439E+04 0.12554418269849E+04 0.19775952281365E+04 0.17513606133231E+04
 0.13573928294466E+04 0.13615107979503E+04 0.19792163901197E+04 0.15820869225245E+04 0.13543785790439E+04
 0.12554418269849E+04 0.19775952281365E+04 0.18194068989007E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54328299776308E+03 0.18908952139601E+01
 0.18908952139601E+01 0.15240735711909E+02 0.00000000000000E+00 0.32720264959323E+03 0.32720264959323E+03
 0.32720264959323E+03 0.32720264959323E+03 0.00000000000000E+00 0.00000000000000E+00 0.21377634793674E+00
 0.00000000000000E+00 0.99361952791968E+01 0.10000000000000E-02 0.29742020645392E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26897970704084E+02 0.10086739014031E+02 0.36250503265224E+03 0.47597348004167E+03
 0.30686448362813E+03 0.30686448362813E+03 0.29515444622665E+03 0.29515425322046E+03 0.30677198959840E+03
 0.30677198959840E+03 0.29515441002961E+03 0.29515421859471E+03 0.30686448362813E+03 0.30686448362813E+03
 0.29515444622665E+03 0.29515425322046E+03 0.30677198959840E+03 0.30677198959840E+03 0.29515441002961E+03
 0.29515421859471E+03 0.30630148845514E+03 0.29516176635194E+03 -.36286003520299E+03 -.55542536664177E+03
 0.19548528682135E+03 0.34633289469349E+03 0.14987018143803E+03 0.22913501751643E+03 0.14499834457357E+03
 0.22913501751643E+03 0.24995743646947E+03 0.22967480879478E+03 0.14277927429743E+03 0.22967480879478E+03
 0.24832735644260E+03 0.22913501751644E+03 0.14499834457357E+03 0.22913501751644E+03 0.24995743646948E+03
 0.22967480879478E+03 0.14277927429743E+03 0.22967480879478E+03 0.24832735644259E+03 0.24609428070030E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33693419043577E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94703066117970E+00 0.93655075384083E+00 0.00000000000000E+00 0.94703066117970E+00 0.93655075384083E+00
 0.13957103594444E+01 0.36571038805466E+00 0.10299999713898E+01 0.15449999570847E+01 0.12357226823453E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.77727099937928E-03 0.00000000000000E+00
 0.12854920524619E-01 0.00000000000000E+00 0.12357226823453E-01 0.13632191523998E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1960.00000000
 0.73154617842663E+00 0.31565090449006E+03 0.51935739178444E+03 0.46968382435080E+03 0.44873980001153E+03
 0.22978097635507E+00 0.00000000000000E+00 0.19444636915482E+00 0.00000000000000E+00 -.20022365085595E+01
 0.50837616301343E-02 0.43354517055054E+00 0.15736379047711E+04 0.59011421428915E+03 0.18452517853771E+02
 0.69196941951641E+01 0.39317288171874E+03 0.29551056659680E+03 0.38546566709837E+03 0.43446597794253E+03
 0.29527243749271E+03 0.29536057728709E+03 0.37649138421624E+03 0.43430825180205E+03 0.29525387714471E+03
 0.29536018875404E+03 0.38546566709837E+03 0.43446597794253E+03 0.29527243749271E+03 0.29536057728709E+03
 0.37649138421624E+03 0.43430825180205E+03 0.29525387714471E+03 0.29536018875404E+03 0.47616543234748E+03
 0.36267675345997E+03 0.27045151415508E+04 0.21538060163309E+04 0.50306528397904E+03 0.88034156139793E+03
 0.37476095099900E+03 0.17519167196531E+04 0.13565614238937E+04 0.13609493288158E+04 0.19771257071500E+04
 0.15828838343291E+04 0.13535567549225E+04 0.12551932563433E+04 0.19755129053927E+04 0.17519167196531E+04
 0.13565614238937E+04 0.13609493288158E+04 0.19771257071500E+04 0.15828838343291E+04 0.13535567549226E+04
 0.12551932563433E+04 0.19755129053927E+04 0.18178366266257E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54340442411055E+03 0.18908952261876E+01
 0.18908952261876E+01 0.15320735711909E+02 0.00000000000000E+00 0.32731008378389E+03 0.32731008378389E+03
 0.32731008378389E+03 0.32731008378389E+03 0.00000000000000E+00 0.00000000000000E+00 0.21371476882392E+00
 0.00000000000000E+00 0.99397487382299E+01 0.10000000000000E-02 0.29856692295872E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26794662719909E+02 0.10047998519966E+02 0.36267927163806E+03 0.47616311641380E+03
 0.30693556658838E+03 0.30693556658838E+03 0.29515459957330E+03 0.29515439991042E+03 0.30684276270126E+03
 0.30684276270126E+03 0.29515456192516E+03 0.29515436389657E+03 0.30693556658838E+03 0.30693556658838E+03
 0.29515459957330E+03 0.29515439991042E+03 0.30684276270126E+03 0.30684276270126E+03 0.29515456192516E+03
 0.29515436389657E+03 0.30636796992726E+03 0.29516213931695E+03 -.36385119121021E+03 -.55685252472398E+03
 0.19595161340964E+03 0.34713746757432E+03 0.15020609609764E+03 0.22986588140700E+03 0.14537710041858E+03
 0.22986588140700E+03 0.25055132426510E+03 0.23040786144682E+03 0.14315726720539E+03 0.23040786144682E+03
 0.24892262815859E+03 0.22986588140700E+03 0.14537710041858E+03 0.22986588140700E+03 0.25055132426510E+03
 0.23040786144682E+03 0.14315726720539E+03 0.23040786144682E+03 0.24892262815859E+03 0.24638340030830E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33704428530534E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94712161893646E+00 0.93665507002163E+00 0.00000000000000E+00 0.94712161893646E+00 0.93665507002163E+00
 0.13957730606031E+01 0.36577308921331E+00 0.10299999713898E+01 0.15449999570847E+01 0.12354796700874E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.77966376074554E-03 0.00000000000000E+00
 0.12842441360867E-01 0.00000000000000E+00 0.12354796700874E-01 0.13622105121613E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1970.00000000
 0.73167095106313E+00 0.31570931526940E+03 0.51949322662934E+03 0.46979230388398E+03 0.44883485410065E+03
 0.22978037520988E+00 0.00000000000000E+00 0.19444922374950E+00 0.00000000000000E+00 -.20034894884696E+01
 0.50958928082192E-02 0.43339767033189E+00 0.15698917345939E+04 0.58870940047273E+03 0.18458797883878E+02
 0.69220492064542E+01 0.39340536758497E+03 0.29551944662082E+03 0.38568561798137E+03 0.43473372420051E+03
 0.29527589448236E+03 0.29536645838109E+03 0.37670598458187E+03 0.43457605234779E+03 0.29525683744713E+03
 0.29536605978670E+03 0.38568561798137E+03 0.43473372420051E+03 0.29527589448236E+03 0.29536645838109E+03
 0.37670598458187E+03 0.43457605234778E+03 0.29525683744713E+03 0.29536605978670E+03 0.47635383227842E+03
 0.36284961724357E+03 0.27049636224363E+04 0.21527222182216E+04 0.50250556452721E+03 0.87917947373684E+03
 0.37416138138700E+03 0.17524686299624E+04 0.13557297316868E+04 0.13603872824505E+04 0.19750416534951E+04
 0.15836755197493E+04 0.13527346119035E+04 0.12549423786421E+04 0.19734371690627E+04 0.17524686299624E+04
 0.13557297316868E+04 0.13603872824505E+04 0.19750416534951E+04 0.15836755197493E+04 0.13527346119035E+04
 0.12549423786421E+04 0.19734371690627E+04 0.18162732107836E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54352529981756E+03 0.18908952383663E+01
 0.18908952383663E+01 0.15400735711909E+02 0.00000000000000E+00 0.32741694415343E+03 0.32741694415343E+03
 0.32741694415343E+03 0.32741694415343E+03 0.00000000000000E+00 0.00000000000000E+00 0.21365338696816E+00
 0.00000000000000E+00 0.99432856001158E+01 0.10000000000000E-02 0.29970943866145E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26692519380535E+02 0.10009694767701E+02 0.36285211016747E+03 0.47635154538394E+03
 0.30700651147257E+03 0.30700651147257E+03 0.29515475703538E+03 0.29515455053716E+03 0.30691340015308E+03
 0.30691340015308E+03 0.29515471789213E+03 0.29515451309310E+03 0.30700651147257E+03 0.30700651147257E+03
 0.29515475703538E+03 0.29515455053716E+03 0.30691340015308E+03 0.30691340015308E+03 0.29515471789213E+03
 0.29515451309310E+03 0.30643432097468E+03 0.29516252117749E+03 -.36483301749457E+03 -.55826473822841E+03
 0.19641324604638E+03 0.34793361298472E+03 0.15053830070811E+03 0.23059118966135E+03 0.14575241971969E+03
 0.23059118966135E+03 0.25113912458347E+03 0.23113534625478E+03 0.14353188360829E+03 0.23113534625478E+03
 0.24951185502683E+03 0.23059118966135E+03 0.14575241971969E+03 0.23059118966135E+03 0.25113912458347E+03
 0.23113534625478E+03 0.14353188360828E+03 0.23113534625478E+03 0.24951185502683E+03 0.24666994969760E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33715379261753E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94721213352862E+00 0.93675881317132E+00 0.00000000000000E+00 0.94721213352862E+00 0.93675881317132E+00
 0.13958354469213E+01 0.36583547553156E+00 0.10299999713898E+01 0.15449999570847E+01 0.12352373304094E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.78203162999847E-03 0.00000000000000E+00
 0.12830072575461E-01 0.00000000000000E+00 0.12352373304094E-01 0.13612104205460E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1980.00000000
 0.73179509985935E+00 0.31576752439659E+03 0.51962844036235E+03 0.46990030057682E+03 0.44892950121981E+03
 0.22977977647346E+00 0.00000000000000E+00 0.19445206149913E+00 0.00000000000000E+00 -.20047374825449E+01
 0.51079683046397E-02 0.43325096670165E+00 0.15661804308248E+04 0.58731766155930E+03 0.18465048239602E+02
 0.69243930898507E+01 0.39363698065569E+03 0.29552846608139E+03 0.38590478046706E+03 0.43500021258019E+03
 0.29527941791789E+03 0.29537244998014E+03 0.37691987085895E+03 0.43484259666069E+03 0.29525985553738E+03
 0.29537204115718E+03 0.38590478046706E+03 0.43500021258019E+03 0.29527941791789E+03 0.29537244998014E+03
 0.37691987085895E+03 0.43484259666069E+03 0.29525985553738E+03 0.29537204115718E+03 0.47654103859518E+03
 0.36302109757637E+03 0.27054066026387E+04 0.21516382603112E+04 0.50195544924016E+03 0.87803350976859E+03
 0.37356828328223E+03 0.17530163831134E+04 0.13548977824111E+04 0.13598246747108E+04 0.19729641973085E+04
 0.15844620245851E+04 0.13519121795380E+04 0.12546892239526E+04 0.19713679874643E+04 0.17530163831134E+04
 0.13548977824111E+04 0.13598246747108E+04 0.19729641973085E+04 0.15844620245851E+04 0.13519121795380E+04
 0.12546892239526E+04 0.19713679874643E+04 0.18147165924767E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54364562974400E+03 0.18908952504965E+01
 0.18908952504965E+01 0.15480735711909E+02 0.00000000000000E+00 0.32752323613646E+03 0.32752323613646E+03
 0.32752323613646E+03 0.32752323613646E+03 0.00000000000000E+00 0.00000000000000E+00 0.21359220182182E+00
 0.00000000000000E+00 0.99468060140639E+01 0.10000000000000E-02 0.30084776964816E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26591521716634E+02 0.99718206437377E+01 0.36302356545754E+03 0.47653878045371E+03
 0.30707731801652E+03 0.30707731801652E+03 0.29515491868891E+03 0.29515470517340E+03 0.30698390167910E+03
 0.30698390167910E+03 0.29515487800567E+03 0.29515466625620E+03 0.30707731801652E+03 0.30707731801652E+03
 0.29515491868891E+03 0.29515470517340E+03 0.30698390167910E+03 0.30698390167910E+03 0.29515487800567E+03
 0.29515466625620E+03 0.30650054132729E+03 0.29516291206640E+03 -.36580562313500E+03 -.55966219340459E+03
 0.19687024509654E+03 0.34872143747801E+03 0.15086684115598E+03 0.23131099731322E+03 0.14612434264689E+03
 0.23131099731322E+03 0.25172091102948E+03 0.23185731827603E+03 0.14390316248434E+03 0.23185731827603E+03
 0.25009510953572E+03 0.23131099731322E+03 0.14612434264689E+03 0.23131099731322E+03 0.25172091102949E+03
 0.23185731827603E+03 0.14390316248433E+03 0.23185731827603E+03 0.25009510953571E+03 0.24695395707213E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33726271793810E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94730220898680E+00 0.93686198888321E+00 0.00000000000000E+00 0.94730220898680E+00 0.93686198888321E+00
 0.13958975213194E+01 0.36589754992967E+00 0.10299999713898E+01 0.15449999570847E+01 0.12349956599239E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.78437496543874E-03 0.00000000000000E+00
 0.12817812843621E-01 0.00000000000000E+00 0.12349956599239E-01 0.13602187809059E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   1990.00000000
 0.73191863057775E+00 0.31582553322091E+03 0.51976303849961E+03 0.47000781863727E+03 0.44902374481755E+03
 0.22977918013146E+00 0.00000000000000E+00 0.19445488258455E+00 0.00000000000000E+00 -.20059805284511E+01
 0.51199885007843E-02 0.43310505174656E+00 0.15625035092900E+04 0.58593881598376E+03 0.18471269193788E+02
 0.69267259476703E+01 0.39386772723731E+03 0.29553762564026E+03 0.38612316013170E+03 0.43526545369362E+03
 0.29528300836498E+03 0.29537855294571E+03 0.37713304773895E+03 0.43510789532944E+03 0.29526293193080E+03
 0.29537813372615E+03 0.38612316013170E+03 0.43526545369362E+03 0.29528300836498E+03 0.29537855294571E+03
 0.37713304773895E+03 0.43510789532944E+03 0.29526293193080E+03 0.29537813372615E+03 0.47672706462636E+03
 0.36319121143690E+03 0.27058441398732E+04 0.21505541654189E+04 0.50141481917549E+03 0.87690345860723E+03
 0.37298156533587E+03 0.17535600175936E+04 0.13540656052347E+04 0.13592615213126E+04 0.19708933071319E+04
 0.15852433941886E+04 0.13510894869629E+04 0.12544338220379E+04 0.19693053293036E+04 0.17535600175936E+04
 0.13540656052347E+04 0.13592615213126E+04 0.19708933071319E+04 0.15852433941886E+04 0.13510894869629E+04
 0.12544338220379E+04 0.19693053293036E+04 0.18131667135043E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54376541869111E+03 0.18908952625787E+01
 0.18908952625787E+01 0.15560735711909E+02 0.00000000000000E+00 0.32762896511179E+03 0.32762896511179E+03
 0.32762896511179E+03 0.32762896511179E+03 0.00000000000000E+00 0.00000000000000E+00 0.21353121283759E+00
 0.00000000000000E+00 0.99503101275132E+01 0.10000000000000E-02 0.30198193191068E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26491651170594E+02 0.99343691889728E+01 0.36319365448819E+03 0.47672483495192E+03
 0.30714798597978E+03 0.30714798597978E+03 0.29515508461048E+03 0.29515486389240E+03 0.30705426702792E+03
 0.30705426702792E+03 0.29515504234157E+03 0.29515482345836E+03 0.30714798597978E+03 0.30714798597978E+03
 0.29515508461048E+03 0.29515486389240E+03 0.30705426702792E+03 0.30705426702792E+03 0.29515504234157E+03
 0.29515482345836E+03 0.30656663074043E+03 0.29516331211687E+03 -.36676911601794E+03 -.56104507424371E+03
 0.19732267015354E+03 0.34950104623772E+03 0.15119176273342E+03 0.23202535890565E+03 0.14649290890912E+03
 0.23202535890565E+03 0.25229675632398E+03 0.23257383207960E+03 0.14427114237140E+03 0.23257383207960E+03
 0.25067246331265E+03 0.23202535890565E+03 0.14649290890912E+03 0.23202535890565E+03 0.25229675632398E+03
 0.23257383207960E+03 0.14427114237140E+03 0.23257383207960E+03 0.25067246331265E+03 0.24723545014397E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33737106677455E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94739184928966E+00 0.93696460267425E+00 0.00000000000000E+00 0.94739184928966E+00 0.93696460267425E+00
 0.13959592866786E+01 0.36595931528888E+00 0.10299999713898E+01 0.15449999570847E+01 0.12347546551084E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.78669412006597E-03 0.00000000000000E+00
 0.12805660861680E-01 0.00000000000000E+00 0.12347546551084E-01 0.13592354981746E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   2000.00000000
 0.73204154890428E+00 0.31588334307742E+03 0.51989702649070E+03 0.47011486222286E+03 0.44911758830134E+03
 0.22977858616965E+00 0.00000000000000E+00 0.19445768718392E+00 0.00000000000000E+00 -.20072186634515E+01
 0.51319537734194E-02 0.43295991766083E+00 0.15588604950877E+04 0.58457268565790E+03 0.18477461015842E+02
 0.69290478809407E+01 0.39409761356542E+03 0.29554692594429E+03 0.38634076248899E+03 0.43552945802346E+03
 0.29528666638336E+03 0.29538476812788E+03 0.37734551986362E+03 0.43537195881357E+03 0.29526606713792E+03
 0.29538433834291E+03 0.38634076248899E+03 0.43552945802346E+03 0.29528666638336E+03 0.29538476812788E+03
 0.37734551986362E+03 0.43537195881357E+03 0.29526606713792E+03 0.29538433834291E+03 0.47691192352963E+03
 0.36335997557040E+03 0.27062762912721E+04 0.21494699561305E+04 0.50088355645335E+03 0.87578911176138E+03
 0.37240113752576E+03 0.17540995715171E+04 0.13532332289084E+04 0.13586978378292E+04 0.19688289518785E+04
 0.15860196734673E+04 0.13502665629003E+04 0.12541762023534E+04 0.19672491636585E+04 0.17540995715171E+04
 0.13532332289084E+04 0.13586978378292E+04 0.19688289518785E+04 0.15860196734673E+04 0.13502665629003E+04
 0.12541762023534E+04 0.19672491636585E+04 0.18116235163507E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54388467140226E+03 0.18908952746131E+01
 0.18908952746131E+01 0.15640735711909E+02 0.00000000000000E+00 0.32773413640326E+03 0.32773413640326E+03
 0.32773413640326E+03 0.32773413640326E+03 0.00000000000000E+00 0.00000000000000E+00 0.21347041946849E+00
 0.00000000000000E+00 0.99537980862254E+01 0.10000000000000E-02 0.30311194134736E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26392889585410E+02 0.98973335945287E+01 0.36336239400593E+03 0.47690972203659E+03
 0.30721851514507E+03 0.30721851514507E+03 0.29515525487730E+03 0.29515502676802E+03 0.30712449597098E+03
 0.30712449597098E+03 0.29515521097620E+03 0.29515498477264E+03 0.30721851514507E+03 0.30721851514507E+03
 0.29515525487730E+03 0.29515502676802E+03 0.30712449597098E+03 0.30712449597098E+03 0.29515521097620E+03
 0.29515498477264E+03 0.30663258899422E+03 0.29516372146244E+03 -.36772360284781E+03 -.56241356250139E+03
 0.19777058004973E+03 0.35027254309803E+03 0.15151311014805E+03 0.23273432849373E+03 0.14685815775967E+03
 0.23273432849373E+03 0.25286673231540E+03 0.23328494174889E+03 0.14463586137193E+03 0.23328494174889E+03
 0.25124398713523E+03 0.23273432849373E+03 0.14685815775967E+03 0.23273432849373E+03 0.25286673231541E+03
 0.23328494174889E+03 0.14463586137193E+03 0.23328494174889E+03 0.25124398713523E+03 0.24751445616193E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33747884457698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94748105835418E+00 0.93706666000185E+00 0.00000000000000E+00 0.94748105835418E+00 0.93706666000185E+00
 0.13960207458419E+01 0.36602077445214E+00 0.10299999713898E+01 0.15449999570847E+01 0.12345143123183E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.78898944167580E-03 0.00000000000000E+00
 0.12793615346433E-01 0.00000000000000E+00 0.12345143123183E-01 0.13582604788109E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
   2000.00000000
 0.73204154890428E+00 0.31588334307742E+03 0.51989702649070E+03 0.47011486222286E+03 0.44911758830135E+03
 0.22977858616965E+00 0.00000000000000E+00 0.19445768718392E+00 0.00000000000000E+00 -.20072186634445E+01
 0.51319537734196E-02 0.43295991766083E+00 0.15588604950877E+04 0.58457268565789E+03 0.18477461015842E+02
 0.69290478809407E+01 0.39409761356542E+03 0.29554692594429E+03 0.38634076248900E+03 0.43552945802347E+03
 0.29528666638336E+03 0.29538476812788E+03 0.37734551986362E+03 0.43537195881357E+03 0.29526606713792E+03
 0.29538433834291E+03 0.38634076248900E+03 0.43552945802347E+03 0.29528666638336E+03 0.29538476812788E+03
 0.37734551986362E+03 0.43537195881357E+03 0.29526606713792E+03 0.29538433834291E+03 0.47691192352964E+03
 0.36335997557040E+03 0.27062768770211E+04 0.21494705418795E+04 0.50088400151511E+03 0.87578955904844E+03
 0.37240113752575E+03 0.17540999185977E+04 0.13532337735783E+04 0.13586981849098E+04 0.19688294965483E+04
 0.15860200113024E+04 0.13502671149450E+04 0.12541765401885E+04 0.19672497157032E+04 0.17540999185977E+04
 0.13532337735783E+04 0.13586981849098E+04 0.19688294965483E+04 0.15860200113024E+04 0.13502671149450E+04
 0.12541765401885E+04 0.19672497157032E+04 0.18116240629492E+03 0.00000000000000E+00 0.80000000000000E-02
 0.40040000000000E+06 0.40040000000000E+06 0.50050000000000E+08 0.54388467140226E+03 0.18908952746131E+01
 0.18908952746131E+01 0.15640735711910E+02 0.00000000000000E+00 0.32773413640326E+03 0.32773413640326E+03
 0.32773413640326E+03 0.32773413640326E+03 0.00000000000000E+00 0.00000000000000E+00 0.21347041946849E+00
 0.00000000000000E+00 0.99537980862255E+01 0.10000000000000E-02 0.30311194134737E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26392889585409E+02 0.98973335945283E+01 0.36336239400593E+03 0.47690972203660E+03
 0.30721851514507E+03 0.30721851514507E+03 0.29515502676802E+03 0.29515502676802E+03 0.30712449597098E+03
 0.30712449597098E+03 0.29515498477264E+03 0.29515498477264E+03 0.30721851514507E+03 0.30721851514507E+03
 0.29515502676802E+03 0.29515502676802E+03 0.30712449597098E+03 0.30712449597098E+03 0.29515498477264E+03
 0.29515498477264E+03 0.30663258899422E+03 0.29516372146244E+03 -.36771689897485E+03 -.56240685862844E+03
 0.19777371915394E+03 0.35027569789776E+03 0.15151311014805E+03 0.23273801552043E+03 0.14686264839834E+03
 0.23273801552043E+03 0.25287122295408E+03 0.23328862868633E+03 0.14464043145879E+03 0.23328862868633E+03
 0.25124855722209E+03 0.23273801552043E+03 0.14686264839834E+03 0.23273801552043E+03 0.25287122295408E+03
 0.23328862868633E+03 0.14464043145879E+03 0.23328862868633E+03 0.25124855722209E+03 0.24755168423023E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33747884457698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.94748105835419E+00 0.93706666000185E+00 0.00000000000000E+00 0.94748105835419E+00 0.93706666000185E+00
 0.13960207458419E+01 0.36602077445214E+00 0.10299999713898E+01 0.15449999570847E+01 0.12345143123183E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.78898944167574E-03 0.00000000000000E+00
 0.12793615346432E-01 0.00000000000000E+00 0.12345143123183E-01 0.13582604788107E-01 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00
