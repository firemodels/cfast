#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-43 0 MONOZONE(1=OUI,0=NON)                                                                     
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
200kW                                                                                               
CIRCULAIRE                                                                                          
3.000000 2.000000 1.800000 0.560000                                                                 
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
1.000000 0.000400                                                                                   
4.000000 0.001600                                                                                   
8.000000 0.003200                                                                                   
10.000000 0.004000                                                                                  
53.300000 0.004000                                                                                  
63.900000 0.004000                                                                                  
74.600000 0.004000                                                                                  
100.000000 0.004000                                                                                 
4000.000000 0.004000                                                                                
5000.000000 0.004000                                                                                
5001.000000 0.000000                                                                                
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL43 0 MONOZONE(1=OUI,0=NON)                                                               
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
#OUVERTURE OUV_1                                                                                    
South                                                                                               
LOC_1 EXT                                                                                           
PAR_2                                                                                               
0.720000 2.700000                                                                                   
0.980000 2.900000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#OUVERTURE OUV_2                                                                                    
Plenum                                                                                              
LOC_1 LOC_2                                                                                         
PAR_6                                                                                               
4.600000 1.800000                                                                                   
5.000000 2.200000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#OUVERTURE OUV_3                                                                                    
East                                                                                                
LOC_2 EXT                                                                                           
PAR_9                                                                                               
1.675000 0.275000                                                                                   
2.325000 0.925000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#CONDINIT 500.000000 10.000000 34.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-43           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-43           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-43           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-43           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-43           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-43           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-43           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-43           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-43           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-43           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-43           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-43           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-43           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-43           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-43           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-43           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-43           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-43           #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_1 #200kW       #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #200kW       #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #200kW       #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #200kW       #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #200kW       #TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_1 #200kW       #HEIGHT#m#Flame height
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #200kW       #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #200kW       #MASS#kg#Consumed combustible mass
#ROOM#LOC_2 #Plenum-LLNL43     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL43     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL43     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL43     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL43     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL43     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL43     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL43     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL43     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL43     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL43     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL43     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL43     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL43     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL43     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL43     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL43     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL43     #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#MASS_FLOW_RATE#kg/s#Pyrolysis rate
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#HEIGHT#m#Flame height
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_2 #Ope_ 2_Roo 2#MASS#kg#Consumed combustible mass
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Outside to upper layer
#NOUVELLE LIGNE
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Upper layer to the outside
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Outside to lower layer
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Lower layer to the outside
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Global mass flow rate to the outside
#OPENING#OUV_1 #South       #MASS_FLOW_RATE#kg/s#Outside to upper and lower layers
#NOUVELLE LIGNE
#OPENING#OUV_1 #South       #HEIGHT#m#Upper layer to the outside flow height
#OPENING#OUV_1 #South       #HEIGHT#m#Lower layer to the outside flow height
#OPENING#OUV_1 #South       #HEIGHT#m#Outside to inside flow height
#OPENING#OUV_1 #South       #AREA#m2#Area
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Down upper layer to up upper layer
#NOUVELLE LIGNE
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Down upper layer to up lower layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Down lower layer to up upper layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Down lower layer to up lower layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Up upper layer to down upper layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Up lower layer to down upper layer
#NOUVELLE LIGNE
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Up upper layer to down lower layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#Up lower layer to down lower layer
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#From down to up
#OPENING#OUV_2 #Plenum      #MASS_FLOW_RATE#kg/s#From up to down
#OPENING#OUV_2 #Plenum      #HEIGHT#m#Down to up flow height
#NOUVELLE LIGNE
#OPENING#OUV_2 #Plenum      #HEIGHT#m#Up to down flow height
#OPENING#OUV_2 #Plenum      #AREA#m2#Area
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to upper layer
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Upper layer to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to lower layer
#NOUVELLE LIGNE
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Lower layer to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Global mass flow rate to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to upper and lower layers
#OPENING#OUV_3 #East        #HEIGHT#m#Upper layer to the outside flow height
#OPENING#OUV_3 #East        #HEIGHT#m#Lower layer to the outside flow height
#NOUVELLE LIGNE
#OPENING#OUV_3 #East        #HEIGHT#m#Outside to inside flow height
#OPENING#OUV_3 #East        #AREA#m2#Area
#NOUVELLE LIGNE
#FIN ENTETE
#DEBUTRESULTAT
      0.00000000
 0.30000000000000E+01 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.72178741619165E-06
 0.10000000000071E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.55149795552003E-03 0.55149795552003E-03 0.78114176619761E-03 0.78504747502859E-03
 0.00000000000000E+00 0.52261084134341E-03 0.61502638604533E-03 0.52261084134341E-03 0.61502638604533E-03
 0.51996535465099E-03 0.61501902718420E-03 0.51996535465099E-03 0.61501902718420E-03 0.52261084146389E-03
 0.61502638610558E-03 0.52261084146389E-03 0.61502638610558E-03 0.51996535459075E-03 0.61501902706371E-03
 0.51996535459075E-03 0.61501902706371E-03 0.63370249134599E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30715000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.35450475024481E-06 0.99966487310280E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.64200303468854E-03 0.64200303468854E-03
 0.78831338585606E-03 0.79225495278534E-03 0.00000000000000E+00 0.56103629336888E-03 0.61899187742253E-03
 0.56103629336888E-03 0.61899187742253E-03 0.55614516051556E-03 0.61898337914427E-03 0.55614516051556E-03
 0.61898337914427E-03 0.56103629348936E-03 0.61899187748278E-03 0.56103629348936E-03 0.61899187748278E-03
 0.55614516045532E-03 0.61898337920451E-03 0.55614516045532E-03 0.61898337920451E-03 0.51194586906923E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30715000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.45484353390043E-04 0.45484353390043E-04 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27791388749291E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27791388749291E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20419555939639E-01 0.20419555939639E-01 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     20.00000000
 0.30000000000000E+01 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18773276398110E-02
 0.99999998147222E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30715000242548E+03 0.30715000000000E+03 0.30715000332847E+03 0.30715000332847E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000331122E+03 0.30715000331122E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000332847E+03 0.30715000332847E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000331122E+03 0.30715000331122E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715001142534E+03
 0.30715000944644E+03 0.57246805483239E-03 0.57246805483239E-03 0.73346509389403E-03 0.73713241936350E-03
 .00000000000000E+00 0.53394465010304E-03 0.63656272494758E-03 0.53394465010304E-03 0.63656272494758E-03
 0.53116844683163E-03 0.63665690957479E-03 0.53116844683163E-03 0.63665690957479E-03 0.53394464907893E-03
 0.63656272410419E-03 0.53394464907893E-03 0.63656272410419E-03 0.53116844604849E-03 0.63665690879164E-03
 0.53116844604849E-03 0.63665690879164E-03 0.63400461480538E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30715000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20749052758715E-02 0.99966522856378E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30715000942192E+03 0.30715001145507E+03
 0.30715000357077E+03 0.30715000357077E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000353936E+03
 0.30715000353936E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000357077E+03 0.30715000357077E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000353936E+03 0.30715000353936E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000344263E+03 0.30715000000000E+03 0.60860928105946E-03 0.60860928105946E-03
 0.80722001818155E-03 0.81125611827246E-03 .00000000000000E+00 0.57277063321404E-03 0.62868433399868E-03
 0.57277063321404E-03 0.62868433399868E-03 0.56772617779165E-03 0.62886139932431E-03 0.56772617779165E-03
 0.62886139932431E-03 0.57277063194895E-03 0.62868433291433E-03 0.57277063194895E-03 0.62868433291433E-03
 0.56772617996036E-03 0.62886140125205E-03 0.56772617996036E-03 0.62886140125205E-03 0.51199355116945E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30715000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23196729941704E-02 0.00000000000000E+00 0.00000000000000E+00 0.23196729941704E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195313556136E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195313556136E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.39600605904574E-02
 0.62794899832052E-02 0.62794899832052E-02 0.39600605904574E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     30.00000000
 0.30000000000000E+01 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18773276366706E-02
 0.99999998147222E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30715000304508E+03 0.30715000000000E+03 0.30715000416734E+03 0.30715000416734E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000414565E+03 0.30715000414565E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000416734E+03 0.30715000416734E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000414565E+03 0.30715000414565E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715001410091E+03
 0.30715001167063E+03 0.57724273454409E-03 0.57724273454409E-03 0.72253007001033E-03 0.72614272036038E-03
 .00000000000000E+00 0.53642229933892E-03 0.64138175523912E-03 0.53642229933892E-03 0.64138175523912E-03
 0.53361991736556E-03 0.64150202784383E-03 0.53361991736556E-03 0.64150202784383E-03 0.53642230126666E-03
 0.64138175680541E-03 0.53642230126666E-03 0.64138175680541E-03 0.53361991694387E-03 0.64150202748238E-03
 0.53361991694387E-03 0.64150202748238E-03 0.63399331740321E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30715000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20749052671032E-02 0.99966541611334E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30715001164883E+03 0.30715001412725E+03
 0.30715000447031E+03 0.30715000447031E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000443093E+03
 0.30715000443093E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000447031E+03 0.30715000447031E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000443093E+03 0.30715000443093E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000431208E+03 0.30715000000000E+03 0.60098405390987E-03 0.60098405390987E-03
 0.81142317387899E-03 0.81548028974838E-03 .00000000000000E+00 0.57536694050886E-03 0.63082961392035E-03
 0.57536694050886E-03 0.63082961392035E-03 0.57029228270107E-03 0.63105372541421E-03 0.57029228270107E-03
 0.63105372541421E-03 0.57536694008717E-03 0.63082961355889E-03 0.57536694008717E-03 0.63082961355889E-03
 0.57029228565293E-03 0.63105372836607E-03 0.57029228565293E-03 0.63105372836607E-03 0.51195256583286E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30715000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23196729941704E-02 0.00000000000000E+00 0.00000000000000E+00 0.23196729941704E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195312704661E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195312704661E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.39600605072457E-02
 0.62794900802255E-02 0.62794900802255E-02 0.39600605072457E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00000000
 0.30000000000000E+01 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18773276359130E-02
 0.99999998147222E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30715000357402E+03 0.30715000000000E+03 0.30715000487972E+03 0.30715000487972E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000485425E+03 0.30715000485425E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000487972E+03 0.30715000487972E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000485425E+03 0.30715000485425E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715001630558E+03
 0.30715001350760E+03 0.58114701704030E-03 0.58114701704030E-03 0.71362782869381E-03 0.71719596783728E-03
 .00000000000000E+00 0.53842118120785E-03 0.64528731848019E-03 0.53842118120785E-03 0.64528731848019E-03
 0.53559888997741E-03 0.64542984176767E-03 0.53559888997741E-03 0.64542984176767E-03 0.53842118415971E-03
 0.64528732095011E-03 0.53842118415971E-03 0.64528732095011E-03 0.53559888991717E-03 0.64542984170743E-03
 0.53559888991717E-03 0.64542984170743E-03 0.63397767572413E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30715000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20749052674008E-02 0.99966560337787E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30715001349030E+03 0.30715001632642E+03
 0.30715000523413E+03 0.30715000523413E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000518799E+03
 0.30715000518799E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000523413E+03 0.30715000523413E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000518799E+03 0.30715000518799E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000505137E+03 0.30715000000000E+03 0.59478517230483E-03 0.59478517230483E-03
 0.81482081825038E-03 0.81889492234163E-03 .00000000000000E+00 0.57746381863664E-03 0.63256253255361E-03
 0.57746381863664E-03 0.63256253255361E-03 0.57236646869271E-03 0.63282661443171E-03 0.57236646869271E-03
 0.63282661443171E-03 0.57746381809446E-03 0.63256253219216E-03 0.57746381809446E-03 0.63256253219216E-03
 0.57236647152408E-03 0.63282661732333E-03 0.57236647152408E-03 0.63282661732333E-03 0.51191068643396E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30715000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23196729941704E-02 0.00000000000000E+00 0.00000000000000E+00 0.23196729941704E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195312704661E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195312704661E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.39600605072457E-02
 0.62794900802255E-02 0.62794900802255E-02 0.39600605072457E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00025000
 0.30000000000000E+01 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18773276370515E-02
 0.99999998147222E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30715000357403E+03 0.30715000000000E+03 0.30715000487974E+03 0.30715000487974E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000485427E+03 0.30715000485427E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000487974E+03 0.30715000487974E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30715000485427E+03 0.30715000485427E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715001630563E+03
 0.30715001350765E+03 0.58115468578627E-03 0.58115468578627E-03 0.71363846155612E-03 0.71720665386391E-03
 .00000000000000E+00 0.53842952803774E-03 0.64529341671729E-03 0.53842952803774E-03 0.64529341671729E-03
 0.53560730247107E-03 0.64543594024575E-03 0.53560730247107E-03 0.64543594024575E-03 0.53842953074863E-03
 0.64529341912697E-03 0.53842953074863E-03 0.64529341912697E-03 0.53560730253132E-03 0.64543594024575E-03
 0.53560730253132E-03 0.64543594024575E-03 0.63398710469787E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30715000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.30715000000000E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20749052755467E-02 0.99966560338255E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30715001349035E+03 0.30715001632647E+03
 0.30715000523415E+03 0.30715000523415E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000518801E+03
 0.30715000518801E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715000523415E+03 0.30715000523415E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715000518801E+03 0.30715000518801E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30715000505139E+03 0.30715000000000E+03 0.59479505976355E-03 0.59479505976355E-03
 0.81484110679587E-03 0.81891531232985E-03 .00000000000000E+00 0.57747775664674E-03 0.63257374352900E-03
 0.57747775664674E-03 0.63257374352900E-03 0.57238063833329E-03 0.63283782631073E-03 0.57238063833329E-03
 0.63283782631073E-03 0.57747775604432E-03 0.63257374316755E-03 0.57747775604432E-03 0.63257374316755E-03
 0.57238064122491E-03 0.63283782908186E-03 0.57238064122491E-03 0.63283782908186E-03 0.51192216221810E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30715000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23196729941704E-02 0.00000000000000E+00 0.00000000000000E+00 0.23196729941704E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195313556136E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23195313556136E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.39600605904574E-02
 0.62794899832052E-02 0.62794899832052E-02 0.39600605904574E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     50.07684446
 0.27818514840616E+01 0.30716486829675E+03 0.42560851447729E+03 0.31577763684229E+03 0.31350915344060E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21347440942543E+00 0.00000000000000E+00 0.33351620337958E+01
 0.99998450895769E-03 0.23212781355671E+00 0.80000000000000E+04 0.30000000000000E+04 0.34463771822180E+02
 0.12923914433317E+02 0.30789464246113E+03 0.30715000000000E+03 0.30831195053714E+03 0.31390807715234E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30807381733778E+03 0.31389811215180E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30831195053714E+03 0.31390807715234E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30807381733778E+03 0.31389811215180E+03 0.30715000000000E+03 0.30715000000000E+03 0.32106689855352E+03
 0.30716247380514E+03 0.36476010927410E+03 0.36359648341623E+03 0.56259161240992E+03 0.17407594908864E+04
 0.11753549204144E+04 0.38306578864145E+03 0.40334156186600E+03 0.38135208602070E+03 0.18584936870203E+04
 0.30719950704839E+03 0.39928181683831E+03 0.30593620197042E+03 0.18545857117816E+04 0.38306578864145E+03
 0.40334156186600E+03 0.38135208602070E+03 0.18584936870203E+04 0.30719950704839E+03 0.39928181683831E+03
 0.30593620197042E+03 0.18545857117816E+04 0.78145207042556E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49843664111784E+03 0.16428691188533E+01
 0.12000000000000E+01 0.20503072878594E-01 0.13223542066207E+01 0.30715044562521E+03 0.32433898497971E+03
 0.30918609343232E+03 0.30909038871291E+03 0.22999999996954E+00 0.00000000000000E+00 0.22722595939701E+00
 0.00000000000000E+00 0.40533563490356E+00 0.99966817548388E-03 0.23874042527333E-01 0.80000000000000E+04
 0.30000000000000E+04 0.33509197241484E+03 0.12565948965557E+03 0.30716245257010E+03 0.32110590828754E+03
 0.30715562924168E+03 0.30767073904250E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715565195731E+03
 0.30767076723792E+03 0.30715000000000E+03 0.30715000000000E+03 0.30715562924168E+03 0.30767073904250E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30715565195731E+03 0.30767076723792E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30737333623262E+03 0.30715000000000E+03 0.22302376042399E+01 0.22249621913262E+01
 0.17172754092400E+01 0.13041830100079E+03 0.12869243921450E+03 0.22436729355522E+01 -.39266808430323E+00
 0.22419213909048E+01 0.17825857097268E+03 0.22408699176999E+01 -.37809633084530E+00 0.22391086478548E+01
 0.17827280516976E+03 0.22436729355522E+01 -.39266808430337E+00 0.22419213909048E+01 0.17825857097268E+03
 0.22408699177024E+01 -.37809633084493E+00 0.22391086478575E+01 0.17827280516976E+03 0.38760587792176E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34199059612045E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.50414591555448E-01 0.00000000000000E+00 0.40022723255914E-01 0.90437314811362E-01 0.00000000000000E+00
 0.14092574203082E+00 0.40925718188962E-01 0.10000002384186E+00 0.52000010490417E-01 0.20919550369656E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20919550369656E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.27770369523542E+00 0.27770369523542E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     60.17751953
 0.25390810784984E+01 0.30720837599379E+03 0.45535600607992E+03 0.32996972462123E+03 0.32337241507010E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20736016001805E+00 0.00000000000000E+00 0.99425387393834E+00
 0.99981978981780E-03 0.31069862085199E+00 0.80000000000000E+04 0.30000000000000E+04 0.25748424560310E+02
 0.96556592101164E+01 0.30885760567909E+03 0.30715000000000E+03 0.30962075260907E+03 0.31910017227407E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30917945061949E+03 0.31907440631187E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30962075260907E+03 0.31910017227407E+03 0.30715000000000E+03 0.30715000000000E+03
 0.30917945061949E+03 0.31907440631187E+03 0.30715000000000E+03 0.30715000000000E+03 0.33438538782724E+03
 0.30723111196440E+03 0.50907350206303E+03 0.50562479709589E+03 0.64171732526290E+03 0.20094801964142E+04
 0.13645542845250E+04 0.48141866411109E+03 0.61313231918152E+03 0.47685013909577E+03 0.20765175498181E+04
 0.40239566990735E+03 0.60760267349516E+03 0.39889054928491E+03 0.20713222222983E+04 0.48141866411109E+03
 0.61313231918152E+03 0.47685013909578E+03 0.20765175498181E+04 0.40239566990735E+03 0.60760267349516E+03
 0.39889054928491E+03 0.20713222222983E+04 0.10211714499115E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52673104749957E+03 0.14198928080029E+01
 0.12000000000000E+01 0.60905773158512E-01 0.10950684439925E+01 0.30715097361326E+03 0.33316723518996E+03
 0.31417417713443E+03 0.31376517957733E+03 0.22999999997565E+00 0.00000000000000E+00 0.22528780395780E+00
 0.00000000000000E+00 0.22772018706228E+00 0.99966470414490E-03 0.74867545753336E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10685537931693E+03 0.40070767243847E+02 0.30723085904905E+03 0.33448713016825E+03
 0.30717713349862E+03 0.30828978710064E+03 0.30715000000000E+03 0.30715000000000E+03 0.30717676029789E+03
 0.30828998678074E+03 0.30715000000000E+03 0.30715000000000E+03 0.30717713349862E+03 0.30828978710064E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30717676029789E+03 0.30828998678074E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30782651147792E+03 0.30715000000000E+03 0.92238026205200E+01 0.91628441186305E+01
 0.81305124354685E+01 0.21216321132674E+03 0.20399204632910E+03 0.71778510772630E+01 0.56670016033108E+01
 0.71636551727438E+01 0.21350614953143E+03 0.70535807155030E+01 0.57215554497234E+01 0.70396425397667E+01
 0.21355876594556E+03 0.71778510772630E+01 0.56670016033107E+01 0.71636551727438E+01 0.21350614953143E+03
 0.70535807155047E+01 0.57215554497238E+01 0.70396425397685E+01 0.21355876594556E+03 0.71969107618971E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35014599141684E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.61391853998796E-01 0.00000000000000E+00 0.00000000000000E+00 0.61391853998796E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15614212574512E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15614212574512E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20860595987118E+00 0.20860595987118E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     70.27134743
 0.23496477349154E+01 0.30728428978188E+03 0.46436362160260E+03 0.34133658953107E+03 0.33160095590816E+03
 0.22999999986069E+00 0.00000000000000E+00 0.20334658160521E+00 0.00000000000000E+00 -.40969520151571E+00
 0.99955893708299E-03 0.36117193024440E+00 0.80000000000000E+04 0.30000000000000E+04 0.22150115582311E+02
 0.83062933433668E+01 0.30979315248222E+03 0.30715000000000E+03 0.31076386360883E+03 0.32305347842425E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31018223745162E+03 0.32301698555427E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31076386360883E+03 0.32305347842425E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31018223745162E+03 0.32301698555427E+03 0.30715000000000E+03 0.30715000000000E+03 0.34383560307105E+03
 0.30733936666699E+03 0.63444817505937E+03 0.62841859985940E+03 0.72384664111841E+03 0.20852213002632E+04
 0.13577554259392E+04 0.56244325134686E+03 0.78635936575579E+03 0.55502298315442E+03 0.21888237501717E+04
 0.48097405766271E+03 0.78053191576059E+03 0.47513633169802E+03 0.21834393220751E+04 0.56244325134686E+03
 0.78635936575579E+03 0.55502298315442E+03 0.21888237501717E+04 0.48097405766271E+03 0.78053191576059E+03
 0.47513633169802E+03 0.21834393220751E+04 0.11814999906552E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54528460126485E+03 0.13023069324308E+01
 0.12000000000000E+01 0.10128108477465E+00 0.92905916167626E+00 0.30715286011307E+03 0.33544651768327E+03
 0.31792219649465E+03 0.31734092586460E+03 0.22999999997329E+00 0.00000000000000E+00 0.22418272537911E+00
 0.00000000000000E+00 0.14567695066204E+00 0.99965775461502E-03 0.99128437498791E-01 0.80000000000000E+04
 0.30000000000000E+04 0.80703380400781E+02 0.30263767650293E+02 0.30733875868960E+03 0.34391865454512E+03
 0.30720945264859E+03 0.30868294618083E+03 0.30715000000000E+03 0.30715000000000E+03 0.30720841342425E+03
 0.30868332844942E+03 0.30715000000000E+03 0.30715000000000E+03 0.30720945264859E+03 0.30868294618083E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30720841342425E+03 0.30868332844942E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30817493280048E+03 0.30715000000000E+03 0.16231265106881E+02 0.16043297553870E+02
 0.15244787696268E+02 0.23734376170815E+03 0.22202275007340E+03 0.12136407077952E+02 0.12727322647596E+02
 0.12097314880427E+02 0.21505483793894E+03 0.11904085371373E+02 0.12801217265024E+02 0.11865907741538E+02
 0.21512532408251E+03 0.12136407077952E+02 0.12727322647596E+02 0.12097314880427E+02 0.21505483793894E+03
 0.11904085371373E+02 0.12801217265026E+02 0.11865907741538E+02 0.21512532408251E+03 0.87603456558475E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35352124930861E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.49797430075218E-01 0.00000000000000E+00 0.00000000000000E+00 0.49797430075218E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.12910900297870E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12910900297870E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.16733642256648E+00 0.16733642256648E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     80.06005235
 0.21964461803954E+01 0.30739097797335E+03 0.46841549979076E+03 0.35052160115881E+03 0.33856529234683E+03
 0.22999999986055E+00 0.00000000000000E+00 0.19996310943506E+00 0.00000000000000E+00 -.14364288515649E+01
 0.99920188851426E-03 0.40447816793665E+00 0.80000000000000E+04 0.30000000000000E+04 0.19778570598285E+02
 0.74169639743569E+01 0.31071910364869E+03 0.30715000000000E+03 0.31183137273342E+03 0.32633441269292E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31113427943485E+03 0.32629046836959E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31183137273342E+03 0.32633441269292E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31113427943485E+03 0.32629046836959E+03 0.30715000000000E+03 0.30715000000000E+03 0.35127455715078E+03
 0.30746184163049E+03 0.74234003234077E+03 0.73355788666115E+03 0.79047225130817E+03 0.21140239767150E+04
 0.13195993641503E+04 0.62934147298822E+03 0.92231970966820E+03 0.61908588521178E+03 0.22571952370598E+04
 0.54584344613015E+03 0.91659157109442E+03 0.53764528111973E+03 0.22519761766260E+04 0.62934147298823E+03
 0.92231970966820E+03 0.61908588521178E+03 0.22571952370598E+04 0.54584344613015E+03 0.91659157109443E+03
 0.53764528111973E+03 0.22519761766260E+04 0.13034870876801E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57002482461139E+03 0.12947150883581E+01
 0.12000000000000E+01 0.14043590445092E+00 0.81181596830274E+00 0.30715679514186E+03 0.33609316041989E+03
 0.32043249148833E+03 0.31978845481604E+03 0.23000000000000E+00 0.00000000000000E+00 0.22330528986081E+00
 0.00000000000000E+00 0.87177208775144E-01 0.99964437052407E-03 0.11725523729984E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68227229625088E+02 0.25585211109408E+02 0.30746102269942E+03 0.35133249586325E+03
 0.30724608555163E+03 0.30897629228833E+03 0.30715000000000E+03 0.30715000000000E+03 0.30724432691961E+03
 0.30897682140692E+03 0.30715000000000E+03 0.30715000000000E+03 0.30724608555163E+03 0.30897629228833E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30724432691961E+03 0.30897682140692E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30844983672616E+03 0.30715000000000E+03 0.22050151273553E+02 0.21687676057828E+02
 0.21159610466063E+02 0.24625109724144E+03 0.22498568872305E+03 0.16323374483296E+02 0.18568061131370E+02
 0.16251776360894E+02 0.21210478228072E+03 0.16013087183095E+02 0.18648977143513E+02 0.15943285837821E+02
 0.21218120712182E+03 0.16323374483296E+02 0.18568061131370E+02 0.16251776360894E+02 0.21210478228072E+03
 0.16013087183095E+02 0.18648977143515E+02 0.15943285837821E+02 0.21218120712182E+03 0.96121593292686E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35470953472811E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41282595705829E-01 0.00000000000000E+00 0.00000000000000E+00 0.41282595705829E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.11010521756716E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11010521756716E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.27445171335000E-01 0.00000000000000E+00
 0.10738413476366E+00 0.13482930609866E+00 0.00000000000000E+00 0.59340798415137E+00 0.26840798117114E+00
 0.32500000298023E+00 0.42250006586313E+00
     90.43905246
 0.20644408970125E+01 0.30754186706881E+03 0.47174770417989E+03 0.35874995562646E+03 0.34499049397342E+03
 0.22999999986038E+00 0.00000000000000E+00 0.19626498227666E+00 0.00000000000000E+00 -.23108688095426E+01
 0.99870303159148E-03 0.45161517120859E+00 0.80000000000000E+04 0.30000000000000E+04 0.17714196754265E+02
 0.66428237828494E+01 0.31172323404667E+03 0.30715000000000E+03 0.31294198860826E+03 0.32952249405576E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31213604466715E+03 0.32947303543626E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31294198860826E+03 0.32952249405576E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31213604466715E+03 0.32947303543625E+03 0.30715000000000E+03 0.30715000000000E+03 0.35813726349271E+03
 0.30759612604634E+03 0.84891177959141E+03 0.83701606874419E+03 0.85710590382842E+03 0.21415806005765E+04
 0.12801891672290E+04 0.69412350769536E+03 0.10506150409193E+04 0.68082336456706E+03 0.23362589914036E+04
 0.60874138013930E+03 0.10451960028700E+04 0.59798050639514E+03 0.23313941944564E+04 0.69412350769536E+03
 0.10506150409193E+04 0.68082336456706E+03 0.23362589914036E+04 0.60874138013930E+03 0.10451960028700E+04
 0.59798050639514E+03 0.23313941944565E+04 0.14190627542363E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.59709014702147E+03 0.12947215296656E+01
 0.12000000000000E+01 0.18195190489244E+00 0.72503031813339E+00 0.30716406554982E+03 0.33593417812719E+03
 0.32202804221072E+03 0.32138425627618E+03 0.23000000000000E+00 0.00000000000000E+00 0.22248521033784E+00
 0.00000000000000E+00 0.37870188245588E-01 0.99962022287976E-03 0.13389048532952E+00 0.80000000000000E+04
 0.30000000000000E+04 0.59750324904051E+02 0.22406371839019E+02 0.30759523953495E+03 0.35818021677882E+03
 0.30728697771901E+03 0.30922366772379E+03 0.30715000000000E+03 0.30715000000000E+03 0.30728447503104E+03
 0.30922431595348E+03 0.30715000000000E+03 0.30715000000000E+03 0.30728697771901E+03 0.30922366772379E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30728447503104E+03 0.30922431595348E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30868719130448E+03 0.30715000000000E+03 0.26828174546027E+02 0.26251173097239E+02
 0.25992912562462E+02 0.24622674844903E+03 0.22010387132375E+03 0.19907546636159E+02 0.23290391641504E+02
 0.19797792700183E+02 0.20583708794760E+03 0.19542773613793E+02 0.23371736228167E+02 0.19435866485180E+02
 0.20591313545431E+03 0.19907546636157E+02 0.23290391641505E+02 0.19797792700180E+02 0.20583708794760E+03
 0.19542773613793E+02 0.23371736228165E+02 0.19435866485180E+02 0.20591313545430E+03 0.10015272383434E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35443656249751E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.32991150324036E-01 0.00000000000000E+00 0.00000000000000E+00 0.32991150324036E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.92893484158456E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.92893484158456E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.46390584115967E-01 0.00000000000000E+00
 0.60237744449077E-01 0.10662832856504E+00 0.00000000000000E+00 0.55001515906670E+00 0.22501515608646E+00
 0.32500000298023E+00 0.42250006586313E+00
    100.38977953
 0.19657465506255E+01 0.30772526493969E+03 0.47490584475544E+03 0.36536096205398E+03 0.35023000740627E+03
 0.22999999986394E+00 0.00000000000000E+00 0.19233508381364E+00 0.00000000000000E+00 -.29726348341950E+01
 0.99810130639984E-03 0.50118258281988E+00 0.80000000000000E+04 0.30000000000000E+04 0.15962246642707E+02
 0.59858424910152E+01 0.31271388077378E+03 0.30715000000000E+03 0.31400511923242E+03 0.33243195534982E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31310390225910E+03 0.33237900336174E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31400511923242E+03 0.33243195534982E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31310390225910E+03 0.33237900336174E+03 0.30715000000000E+03 0.30715000000000E+03 0.36418142631723E+03
 0.30772115243008E+03 0.94669255210454E+03 0.93165357896376E+03 0.92191124235190E+03 0.21774857543340E+04
 0.12509649557703E+04 0.75349047038471E+03 0.11681849916599E+04 0.73723959180160E+03 0.24159534993434E+04
 0.66649095850199E+03 0.11631578235160E+04 0.65322557694307E+03 0.24115038601227E+04 0.75349047038471E+03
 0.11681849916599E+04 0.73723959180160E+03 0.24159534993434E+04 0.66649095850199E+03 0.11631578235161E+04
 0.65322557694307E+03 0.24115038601227E+04 0.15222001397801E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.63803017444519E+03 0.12947264044242E+01
 0.12000000000000E+01 0.22175481316924E+00 0.66692578412412E+00 0.30717464562853E+03 0.33525857430281E+03
 0.32277197686789E+03 0.32216271440498E+03 0.23000000000000E+00 0.00000000000000E+00 0.22177684405516E+00
 0.00000000000000E+00 0.81203990695864E-02 0.99958549917314E-03 0.14825925516769E+00 0.80000000000000E+04
 0.30000000000000E+04 0.53959531841379E+02 0.20234824440517E+02 0.30772028786474E+03 0.36422037544628E+03
 0.30732611616342E+03 0.30940931961462E+03 0.30715000000000E+03 0.30715000000000E+03 0.30732296262459E+03
 0.30941004829287E+03 0.30715000000000E+03 0.30715000000000E+03 0.30732611616342E+03 0.30940931961462E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30732296262459E+03 0.30941004829287E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30886921199306E+03 0.30715000000000E+03 0.30070391106381E+02 0.29280663419468E+02
 0.29300832728977E+02 0.24010831277079E+03 0.21066097587817E+03 0.22536419028222E+02 0.26471969823877E+02
 0.22391074306036E+02 0.19723760325464E+03 0.22141868366156E+02 0.26549381304551E+02 0.22000377140681E+02
 0.19730923973656E+03 0.22536419028221E+02 0.26471969823877E+02 0.22391074306034E+02 0.19723760325464E+03
 0.22141868366156E+02 0.26549381304550E+02 0.22000377140680E+02 0.19730923973655E+03 0.10048663257738E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35321443817481E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24955677368933E-01 0.68219572188798E-04 0.00000000000000E+00 0.24955677368933E-01 0.68219572188798E-04
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78498964589048E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78498964589048E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.57729707592091E-01 0.00000000000000E+00
 0.26634657580552E-01 0.84364365172643E-01 0.00000000000000E+00 0.52096289206206E+00 0.19596288908183E+00
 0.32500000298023E+00 0.42250006586313E+00
    110.21339128
 0.18938083093505E+01 0.30794475784970E+03 0.47818013497232E+03 0.37071574439224E+03 0.35447718575577E+03
 0.22999999986604E+00 0.00000000000000E+00 0.18791099387276E+00 0.00000000000000E+00 -.34689053215525E+01
 0.99738500739940E-03 0.55641462475037E+00 0.80000000000000E+04 0.30000000000000E+04 0.14377767305432E+02
 0.53916627395369E+01 0.31371595970934E+03 0.30715000000000E+03 0.31505982979667E+03 0.33524355533272E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31407128798871E+03 0.33518850071198E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31505982979667E+03 0.33524355533272E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31407128798871E+03 0.33518850071198E+03 0.30715000000000E+03 0.30715000000000E+03 0.36985964774677E+03
 0.30783625319909E+03 0.10398065033603E+04 0.10215598446025E+04 0.98871811225705E+03 0.22195101996433E+04
 0.12258484968250E+04 0.81073033623032E+03 0.12835957505906E+04 0.79155648656310E+03 0.25092221131430E+04
 0.72227965221306E+03 0.12789979876629E+04 0.70651788171515E+03 0.25052139597999E+04 0.81073033623032E+03
 0.12835957505906E+04 0.79155648656310E+03 0.25092221131430E+04 0.72227965221306E+03 0.12789979876629E+04
 0.70651788171515E+03 0.25052139597999E+04 0.16195657430523E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.65717134190088E+03 0.12947300601244E+01
 0.12000000000000E+01 0.26104926016328E+00 0.62684923602650E+00 0.30718878531866E+03 0.33427944324406E+03
 0.32295827109473E+03 0.32239775780781E+03 0.23000000000000E+00 0.00000000000000E+00 0.22113195869960E+00
 0.00000000000000E+00 -.23968663934345E-02 0.99953938502308E-03 0.16143087199518E+00 0.80000000000000E+04
 0.30000000000000E+04 0.49556815874965E+02 0.18583805953112E+02 0.30783544693539E+03 0.36990081183782E+03
 0.30736318244339E+03 0.30955137414606E+03 0.30715000000000E+03 0.30715000000000E+03 0.30735946608909E+03
 0.30955215391331E+03 0.30715000000000E+03 0.30715000000000E+03 0.30736318244339E+03 0.30955137414606E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30735946608909E+03 0.30955215391331E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30901096512937E+03 0.30715000000000E+03 0.32141279259747E+02 0.31150912570194E+02
 0.31479839789793E+02 0.23042883744906E+03 0.19879159846032E+03 0.24441803389258E+02 0.28515637066383E+02
 0.24265970308174E+02 0.18714889502775E+03 0.24033663969981E+02 0.28586783084352E+02 0.23862600829897E+02
 0.18721402689704E+03 0.24441803389257E+02 0.28515637066383E+02 0.24265970308173E+02 0.18714889502775E+03
 0.24033663969981E+02 0.28586783084352E+02 0.23862600829897E+02 0.18721402689704E+03 0.98487522575871E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35147339247583E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17987052934838E-01 0.31024925351366E-02 0.00000000000000E+00 0.17987052934838E-01 0.31024925351366E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.66430171126319E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.66430171126319E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.66946234576249E-01 0.25969360980015E-02
 0.21531784462143E-02 0.69099413022463E-01 0.25969360980015E-02 0.50092461801325E+00 0.17592461503302E+00
 0.32500000298023E+00 0.42250006586313E+00
    120.48369325
 0.18457260004838E+01 0.30821523675863E+03 0.48203371745790E+03 0.37509328772748E+03 0.35786609299683E+03
 0.22999999987289E+00 0.00000000000000E+00 0.18245496076791E+00 0.00000000000000E+00 -.38742753474998E+01
 0.99650575034612E-03 0.62365665602968E+00 0.80000000000000E+04 0.30000000000000E+04 0.12827570944131E+02
 0.48103391040490E+01 0.31478845081566E+03 0.30715000000000E+03 0.31617568556691E+03 0.33820334399633E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31510159383056E+03 0.33814727065377E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31617568556691E+03 0.33820334399633E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31510159383056E+03 0.33814727065376E+03 0.30715000000000E+03 0.30715000000000E+03 0.37563785892275E+03
 0.30794565629123E+03 0.11346297978322E+04 0.11129496969942E+04 0.10656461365245E+04 0.22774992539725E+04
 0.12065248867654E+04 0.87063815826047E+03 0.14092180960246E+04 0.84840452590052E+03 0.26266505318812E+04
 0.78078118690689E+03 0.14050917609291E+04 0.76239185256002E+03 0.26231179388767E+04 0.87063815826047E+03
 0.14092180960246E+04 0.84840452590052E+03 0.26266505318812E+04 0.78078118690689E+03 0.14050917609291E+04
 0.76239185256003E+03 0.26231179388767E+04 0.17202404170368E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.67682117379610E+03 0.12947330462390E+01
 0.12000000000000E+01 0.30213046805806E+00 0.59930736417463E+00 0.30720699291024E+03 0.33298556994719E+03
 0.32268604257640E+03 0.32218392093314E+03 0.23000000000000E+00 0.00000000000000E+00 0.22051923594789E+00
 0.00000000000000E+00 -.12779591414590E-01 0.99948003597792E-03 0.17412840691140E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45943106825015E+02 0.17228665059381E+02 0.30794491608706E+03 0.37568418570360E+03
 0.30739932421436E+03 0.30965923399514E+03 0.30715000000000E+03 0.30715000000000E+03 0.30739511226236E+03
 0.30966003992788E+03 0.30715000000000E+03 0.30715000000000E+03 0.30739932421436E+03 0.30965923399514E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30739511226236E+03 0.30966003992788E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30912186648984E+03 0.30715000000000E+03 0.33159775014149E+02 0.31978911202538E+02
 0.32678964364377E+02 0.21669222756380E+03 0.18384986837760E+03 0.25744717754890E+02 0.29563332815395E+02
 0.25543959114664E+02 0.17455375729500E+03 0.25336553717410E+02 0.29625898595278E+02 0.25141390154762E+02
 0.17461028881268E+03 0.25744717754890E+02 0.29563332815395E+02 0.25543959114663E+02 0.17455375729500E+03
 0.25336553717410E+02 0.29625898595278E+02 0.25141390154762E+02 0.17461028881268E+03 0.94171439014043E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34876111105988E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12370147798950E-01 0.77493004231986E-02 0.00000000000000E+00 0.12370147798950E-01 0.77493004231986E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.53559675283558E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.53559675283558E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.70945259155669E-01 0.22389866576383E-01
 0.00000000000000E+00 0.70945259155669E-01 0.22389866576383E-01 0.48715368208731E+00 0.16215367910708E+00
 0.32500000298023E+00 0.42250006586313E+00
    130.02040223
 0.18253497982986E+01 0.30850227157702E+03 0.48621115589871E+03 0.37808419718122E+03 0.36002573788404E+03
 0.22999999988842E+00 0.00000000000000E+00 0.17634956279867E+00 0.00000000000000E+00 -.41281161911454E+01
 0.99557609325738E-03 0.69750166830241E+00 0.80000000000000E+04 0.30000000000000E+04 0.11469506617053E+02
 0.43010649813949E+01 0.31574782272919E+03 0.30715000000000E+03 0.31713215022753E+03 0.34108043779382E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31601501135120E+03 0.34100695793466E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31713215022753E+03 0.34108043779382E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31601501135120E+03 0.34100695793466E+03 0.30715000000000E+03 0.30715000000000E+03 0.38107209078289E+03
 0.30803622540179E+03 0.11726566441836E+04 0.11479962614163E+04 0.11536885779654E+04 0.23483337374478E+04
 0.11888767165925E+04 0.87307446898656E+03 0.15432286251895E+04 0.84835866432546E+03 0.27686306851463E+04
 0.79876483363464E+03 0.15305245172309E+04 0.77815229273010E+03 0.27567026340272E+04 0.87307446898656E+03
 0.15432286251895E+04 0.84835866432546E+03 0.27686306851463E+04 0.79876483363464E+03 0.15305245172309E+04
 0.77815229273010E+03 0.27567026340272E+04 0.17878909837277E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.15681100147567E+06 0.50050000000000E+08 0.67742572932988E+03 0.11531915834488E+01
 0.11531915834488E+01 0.34027730396036E+00 0.58269731506669E+00 0.30722626563538E+03 0.33156705317339E+03
 0.32211151214339E+03 0.32166706583755E+03 0.23000000000000E+00 0.00000000000000E+00 0.22001854097243E+00
 0.00000000000000E+00 -.25009337047450E-01 0.99941720408095E-03 0.18474187467903E+00 0.80000000000000E+04
 0.30000000000000E+04 0.43303663632834E+02 0.16238873862313E+02 0.30803554428830E+03 0.38112463607721E+03
 0.30742974467641E+03 0.30972362623446E+03 0.30715000000000E+03 0.30715000000000E+03 0.30742516877006E+03
 0.30972443132191E+03 0.30715000000000E+03 0.30715000000000E+03 0.30742974467641E+03 0.30972362623446E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30742516877006E+03 0.30972443132191E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30919223686713E+03 0.30715000000000E+03 0.33158686582703E+02 0.31823286095090E+02
 0.32917670153640E+02 0.20201971150534E+03 0.16893745300093E+03 0.26403634091989E+02 0.29643092534537E+02
 0.26186875045723E+02 0.16181802013129E+03 0.26007360638826E+02 0.29695664349399E+02 0.25796803436928E+02
 0.16186471896752E+03 0.26403634091989E+02 0.29643092534537E+02 0.26186875045723E+02 0.16181802013129E+03
 0.26007360638826E+02 0.29695664349399E+02 0.25796803436928E+02 0.16186471896752E+03 0.88846588110432E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34628853286997E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91392183939300E-02 0.11476786943633E-01 0.00000000000000E+00 0.91392183939300E-02 0.11476786943633E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.44707011779330E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.44707011779330E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.69841427684232E-01 0.32333369729087E-01
 0.00000000000000E+00 0.69841427684232E-01 0.32333369729087E-01 0.47884865753334E+00 0.15384865455311E+00
 0.32500000298023E+00 0.42250006586313E+00
    140.29070420
 0.18160810840831E+01 0.30883537595662E+03 0.49060973216233E+03 0.38057074220347E+03 0.36172574263211E+03
 0.22999999988883E+00 0.00000000000000E+00 0.16918231317189E+00 0.00000000000000E+00 -.43212070590560E+01
 0.99450038721520E-03 0.78152681660971E+00 0.80000000000000E+04 0.30000000000000E+04 0.10236372994473E+02
 0.38386398729273E+01 0.31659928275145E+03 0.30715000000000E+03 0.31786577945020E+03 0.34429414891900E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31679743587969E+03 0.34416256075339E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31786577945020E+03 0.34429414891900E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31679743587969E+03 0.34416256075339E+03 0.30715000000000E+03 0.30715000000000E+03 0.38695100981054E+03
 0.30812717652442E+03 0.11884696344710E+04 0.11614578329444E+04 0.12447179403436E+04 0.24167447641702E+04
 0.11658032341249E+04 0.85544083521792E+03 0.16829744994857E+04 0.82922161172721E+03 0.29150654757414E+04
 0.80161641190953E+03 0.16616208864942E+04 0.77938242666023E+03 0.28950977008092E+04 0.85544083521792E+03
 0.16829744994857E+04 0.82922161172721E+03 0.29150654757414E+04 0.80161641190953E+03 0.16616208864942E+04
 0.77938242666023E+03 0.28950977008092E+04 0.18410634133792E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.99475777684116E+05 0.50050000000000E+08 0.67540650202715E+03 0.12060055598835E+01
 0.12000000000000E+01 0.38135851185514E+00 0.56953531390652E+00 0.30724921466853E+03 0.33007355667735E+03
 0.32140737748355E+03 0.32101899129410E+03 0.23000000000000E+00 0.00000000000000E+00 0.21949360966359E+00
 0.00000000000000E+00 -.35375366728957E-01 0.99934243673046E-03 0.19589533958578E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40838133346695E+02 0.15314300005011E+02 0.30812654263665E+03 0.38701039294080E+03
 0.30745967289933E+03 0.30976796812896E+03 0.30715000000000E+03 0.30715000000000E+03 0.30745479105280E+03
 0.30976874799501E+03 0.30715000000000E+03 0.30715000000000E+03 0.30745967289933E+03 0.30976796812896E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30745479105280E+03 0.30976874799501E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30924459493791E+03 0.30715000000000E+03 0.32747912678144E+02 0.31260871203594E+02
 0.32865763475181E+02 0.18698911017175E+03 0.15395901787919E+03 0.26864073564178E+02 0.29417854262068E+02
 0.26636902505569E+02 0.14906172124672E+03 0.26484367745957E+02 0.29458939795123E+02 0.26263898923813E+02
 0.14909726487806E+03 0.26864073564178E+02 0.29417854262068E+02 0.26636902505569E+02 0.14906172124672E+03
 0.26484367745957E+02 0.29458939795123E+02 0.26263898923813E+02 0.14909726487806E+03 0.83139111060301E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34382663887223E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.69638644259636E-02 0.14592685392649E-01 0.00000000000000E+00 0.69638644259636E-02 0.14592685392649E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.37526517802846E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.37526517802846E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.67640491121696E-01 0.38528083611986E-01
 0.00000000000000E+00 0.67640491121696E-01 0.38528083611986E-01 0.47226765695326E+00 0.14726765397303E+00
 0.32500000298023E+00 0.42250006586313E+00
    150.56100617
 0.18127458011170E+01 0.30918929246584E+03 0.49348038733857E+03 0.38212275120064E+03 0.36281052465071E+03
 0.22999999988922E+00 0.00000000000000E+00 0.16176219558014E+00 0.00000000000000E+00 -.44510841247275E+01
 0.99336074941105E-03 0.86906297514366E+00 0.80000000000000E+04 0.30000000000000E+04 0.92053167938464E+01
 0.34519937976924E+01 0.31744882776183E+03 0.30715000000000E+03 0.31862029788260E+03 0.34741675589989E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31758752243086E+03 0.34724697790838E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31862029788260E+03 0.34741675589989E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31758752243086E+03 0.34724697790838E+03 0.30715000000000E+03 0.30715000000000E+03 0.39241541152648E+03
 0.30821498275939E+03 0.12266611776837E+04 0.11973579563831E+04 0.13037538685795E+04 0.24385591060542E+04
 0.11282864681318E+04 0.87169375980161E+03 0.17871064960563E+04 0.84395758832710E+03 0.30129106313465E+04
 0.82444601454784E+03 0.17653780728512E+04 0.80061766205479E+03 0.29929632224899E+04 0.87169375980161E+03
 0.17871064960563E+04 0.84395758832710E+03 0.30129106313465E+04 0.82444601454784E+03 0.17653780728512E+04
 0.80061766205479E+03 0.29929632224899E+04 0.18858346163388E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.78844092336939E+05 0.50050000000000E+08 0.67625915945903E+03 0.12242516384124E+01
 0.12000000000000E+01 0.42243971974992E+00 0.56037323587794E+00 0.30727401295716E+03 0.32856201704649E+03
 0.32060919855521E+03 0.32027276681772E+03 0.23000000000000E+00 0.00000000000000E+00 0.21901060588410E+00
 0.00000000000000E+00 -.43304597731032E-01 0.99926168829112E-03 0.20625797065687E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38786379864605E+02 0.14544892449227E+02 0.30821436891919E+03 0.39247896603569E+03
 0.30748646460859E+03 0.30978788222705E+03 0.30715000000000E+03 0.30715000000000E+03 0.30748135006976E+03
 0.30978861993123E+03 0.30715000000000E+03 0.30715000000000E+03 0.30748646460859E+03 0.30978788222705E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30748135006976E+03 0.30978861993123E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30927430857851E+03 0.30715000000000E+03 0.31790550392143E+02 0.30159542731231E+02
 0.32316731644322E+02 0.17163338903627E+03 0.13915507373372E+03 0.26943698225186E+02 0.28724606137073E+02
 0.26713143971101E+02 0.13629186912912E+03 0.26583503358248E+02 0.28754933531896E+02 0.26360005911675E+02
 0.13631709696925E+03 0.26943698225186E+02 0.28724606137073E+02 0.26713143971101E+02 0.13629186912912E+03
 0.26583503358248E+02 0.28754933531896E+02 0.26360005911675E+02 0.13631709696925E+03 0.77022932653837E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34110420400652E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.55056855116523E-02 0.17016111144049E-01 0.00000000000000E+00 0.55056855116523E-02 0.17016111144049E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.31164295227946E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12249803409666E-06 0.31164295227946E-01 0.12249803409666E-06 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.64563462423967E-01 0.42722682192165E-01
 0.00000000000000E+00 0.64563462423967E-01 0.42722682192165E-01 0.46768661793897E+00 0.14268661495874E+00
 0.32500000298023E+00 0.42250006586313E+00
    160.45657352
 0.18118531203092E+01 0.30955072272762E+03 0.49484544631031E+03 0.38293650527693E+03 0.36345059495585E+03
 0.22999999988759E+00 0.00000000000000E+00 0.15457364595885E+00 0.00000000000000E+00 -.45309569422874E+01
 0.99220012296186E-03 0.95541365428251E+00 0.80000000000000E+04 0.30000000000000E+04 0.83733364748778E+01
 0.31400011780792E+01 0.31825905076620E+03 0.30715000000000E+03 0.31936916198000E+03 0.35022047036266E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31835094829207E+03 0.35003104862270E+03 0.30715000000000E+03
 0.30715000000000E+03 0.31936916198000E+03 0.35022047036266E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31835094829207E+03 0.35003104862270E+03 0.30715000000000E+03 0.30715000000000E+03 0.39708270723382E+03
 0.30830085705850E+03 0.12628440423897E+04 0.12314326154173E+04 0.13340804173728E+04 0.24234423170814E+04
 0.10826914976217E+04 0.89403398116580E+03 0.18550918577131E+04 0.86481655431473E+03 0.30641800528906E+04
 0.84799056823590E+03 0.18354479802199E+04 0.82267500271041E+03 0.30465140249134E+04 0.89403398116580E+03
 0.18550918577131E+04 0.86481655431474E+03 0.30641800528906E+04 0.84799056823590E+03 0.18354479802199E+04
 0.82267500271041E+03 0.30465140249134E+04 0.19109057609933E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.73322069248636E+05 0.50050000000000E+08 0.67710725911442E+03 0.12290550205460E+01
 0.12000000000000E+01 0.46202198911652E+00 0.55385041504240E+00 0.30729949763427E+03 0.32716038194870E+03
 0.31982707593493E+03 0.31953508932633E+03 0.23000000000000E+00 0.00000000000000E+00 0.21857015517408E+00
 0.00000000000000E+00 -.46163320696493E-01 0.99917877092585E-03 0.21574346549644E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37081076738948E+02 0.13905403777106E+02 0.30830025487814E+03 0.39714693359222E+03
 0.30750928597481E+03 0.30979056195178E+03 0.30715000000000E+03 0.30715000000000E+03 0.30750400188260E+03
 0.30979124911787E+03 0.30715000000000E+03 0.30715000000000E+03 0.30750928597481E+03 0.30979056195178E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30750400188260E+03 0.30979124911787E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30928733514962E+03 0.30715000000000E+03 0.30546609782792E+02 0.28774609458982E+02
 0.31523090180209E+02 0.15811183673863E+03 0.12643113110752E+03 0.26783013487138E+02 0.27822600028205E+02
 0.26555687662764E+02 0.12514960781446E+03 0.26441367621992E+02 0.27843678931464E+02 0.26221318413555E+02
 0.12516604366286E+03 0.26783013487138E+02 0.27822600028205E+02 0.26555687662764E+02 0.12514960781446E+03
 0.26441367621992E+02 0.27843678931464E+02 0.26221318413555E+02 0.12516604366286E+03 0.71462801530325E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33911238892776E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46405611078993E-02 0.18605857997569E-01 0.00000000000000E+00 0.46405611078993E-02 0.18605857997569E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27830472740625E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.50982497799830E-04 0.27830472740625E-01 0.50982497799830E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.62584039990663E-01 0.43764926082648E-01
 0.00000000000000E+00 0.62584039990663E-01 0.43764926082648E-01 0.46442520752120E+00 0.13942520454097E+00
 0.32500000298023E+00 0.42250006586313E+00
    170.69512296
 0.18118313171695E+01 0.30994475225727E+03 0.49537039945351E+03 0.38338373458799E+03 0.36389197273148E+03
 0.22999999988387E+00 0.00000000000000E+00 0.14720378781846E+00 0.00000000000000E+00 -.45809833638975E+01
 0.99093826003957E-03 0.10452520866184E+01 0.80000000000000E+04 0.30000000000000E+04 0.76536560915956E+01
 0.28701210343484E+01 0.31907296504132E+03 0.30715000000001E+03 0.32014257383998E+03 0.35288570748023E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31912490529932E+03 0.35268745580191E+03 0.30715000000000E+03
 0.30715000000000E+03 0.32014257383998E+03 0.35288570748022E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31912490529932E+03 0.35268745580190E+03 0.30715000000000E+03 0.30715000000000E+03 0.40129765667715E+03
 0.30839769299019E+03 0.12938443555856E+04 0.12604352089840E+04 0.13471511006866E+04 0.23869543529524E+04
 0.10330674967624E+04 0.91542557236382E+03 0.19012108830760E+04 0.88474513888237E+03 0.30874113917653E+04
 0.86881840825935E+03 0.18842108469219E+04 0.84208300333274E+03 0.30724706673735E+04 0.91542557236382E+03
 0.19012108830760E+04 0.88474513888237E+03 0.30874113917653E+04 0.86881840825935E+03 0.18842108469219E+04
 0.84208300333274E+03 0.30724706673735E+04 0.19220444947494E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.73187197477416E+05 0.50050000000000E+08 0.67761804971622E+03 0.12291600277080E+01
 0.12000000000000E+01 0.50297618688353E+00 0.54796689797485E+00 0.30732769883842E+03 0.32587120766323E+03
 0.31909705499103E+03 0.31884322835789E+03 0.23000000000000E+00 0.00000000000000E+00 0.21809838921362E+00
 0.00000000000000E+00 -.46172332196378E-01 0.99908706389030E-03 0.22578433766860E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35432041401128E+02 0.13287015525423E+02 0.30839709251322E+03 0.40136089008203E+03
 0.30753058404473E+03 0.30978360569699E+03 0.30715000000000E+03 0.30715000000000E+03 0.30752516040720E+03
 0.30978423249057E+03 0.30715000000000E+03 0.30715000000000E+03 0.30753058404473E+03 0.30978360569699E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30752516040720E+03 0.30978423249057E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30929123208278E+03 0.30715000000000E+03 0.29228342627593E+02 0.27292678524693E+02
 0.30804279734041E+02 0.14646686674760E+03 0.11550856561489E+03 0.26618344513124E+02 0.27002085223445E+02
 0.26400115970580E+02 0.11558187533085E+03 0.26292810047513E+02 0.27013990098362E+02 0.26081990710143E+02
 0.11558962374035E+03 0.26618344513124E+02 0.27002085223445E+02 0.26400115970580E+02 0.11558187533085E+03
 0.26292810047513E+02 0.27013990098362E+02 0.26081990710143E+02 0.11558962374035E+03 0.66589767428505E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33777431131767E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42214974408404E-02 0.19423760811553E-01 0.00000000000000E+00 0.42214974408404E-02 0.19423760811553E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26498801163301E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12105014275697E-03 0.26498801163301E-01 0.12105014275697E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.61617274189877E-01 0.43022194004639E-01
 0.00000000000000E+00 0.61617274189877E-01 0.43022194004639E-01 0.46148344898743E+00 0.13648344600719E+00
 0.32500000298023E+00 0.42250006586313E+00
    180.12159620
 0.18119984413462E+01 0.31032435863998E+03 0.49549746338746E+03 0.38365300432690E+03 0.36422617229519E+03
 0.22999999988025E+00 0.00000000000000E+00 0.14051932259676E+00 0.00000000000000E+00 -.46104440953446E+01
 0.98972580030800E-03 0.11273749832816E+01 0.80000000000000E+04 0.30000000000000E+04 0.70961304966279E+01
 0.26610489362355E+01 0.31979315162958E+03 0.30715000000001E+03 0.32083834391194E+03 0.35514839544294E+03
 0.30715000000000E+03 0.30715000000000E+03 0.31981366269388E+03 0.35494799337661E+03 0.30715000000000E+03
 0.30715000000000E+03 0.32083834391194E+03 0.35514839544294E+03 0.30715000000000E+03 0.30715000000000E+03
 0.31981366269388E+03 0.35494799337661E+03 0.30715000000000E+03 0.30715000000000E+03 0.40471976007615E+03
 0.30849955556249E+03 0.13164015410075E+04 0.12813568824732E+04 0.13501621770620E+04 0.23463926186676E+04
 0.98947963072028E+03 0.93152520415907E+03 0.19303292095186E+04 0.89961760581749E+03 0.30942754512045E+04
 0.88415691221718E+03 0.19155603174395E+04 0.85625752928640E+03 0.30815782919600E+04 0.93152520415907E+03
 0.19303292095186E+04 0.89961760581749E+03 0.30942754512046E+04 0.88415691221719E+03 0.19155603174396E+04
 0.85625752928641E+03 0.30815782919601E+04 0.19243896210855E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.74221008838503E+05 0.50050000000000E+08 0.67784189202635E+03 0.12282695558574E+01
 0.12000000000000E+01 0.54068207987301E+00 0.54268726681701E+00 0.30735572820688E+03 0.32486219417759E+03
 0.31852850339808E+03 0.31830291433155E+03 0.23000000000000E+00 0.00000000000000E+00 0.21762515652472E+00
 0.00000000000000E+00 -.45376106378778E-01 0.99899594245378E-03 0.23567906117338E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33944466513784E+02 0.12729174942669E+02 0.30849894912240E+03 0.40478180233793E+03
 0.30754896745180E+03 0.30977347578307E+03 0.30715000000000E+03 0.30715000000000E+03 0.30754343038400E+03
 0.30977403998751E+03 0.30715000000000E+03 0.30715000000000E+03 0.30754896745180E+03 0.30977347578307E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30754343038400E+03 0.30977403998751E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30929067355067E+03 0.30715000000001E+03 0.28095777184574E+02 0.25980146098942E+02
 0.30370776889559E+02 0.13784312308645E+03 0.10732049231244E+03 0.26559378140519E+02 0.26481205519752E+02
 0.26353944374498E+02 0.10851384895475E+03 0.26244255363984E+02 0.26485044219230E+02 0.26046292050308E+02
 0.10851399845960E+03 0.26559378140518E+02 0.26481205519751E+02 0.26353944374496E+02 0.10851384895475E+03
 0.26244255363984E+02 0.26485044219230E+02 0.26046292050308E+02 0.10851399845960E+03 0.62940967209115E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33696226263942E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41324661249626E-02 0.19603060911866E-01 0.00000000000000E+00 0.41324661249626E-02 0.19603060911866E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26184302373629E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14427863292128E-03 0.26184302373629E-01 0.14427863292128E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.61295235442287E-01 0.41792574303809E-01
 0.00000000000000E+00 0.61295235442287E-01 0.41792574303809E-01 0.45884363340850E+00 0.13384363042827E+00
 0.32500000298023E+00 0.42250006586313E+00
    190.60696320
 0.18121447374059E+01 0.31076318956998E+03 0.49554492566981E+03 0.38392784212246E+03 0.36459354633163E+03
 0.22999999987801E+00 0.00000000000000E+00 0.13320631465481E+00 0.00000000000000E+00 -.46364939921791E+01
 0.98832794731843E-03 0.12174140472736E+01 0.80000000000000E+04 0.30000000000000E+04 0.65713058083368E+01
 0.24642396781263E+01 0.32055932972317E+03 0.30715000000001E+03 0.32158576504278E+03 0.35748571166897E+03
 0.30715000000000E+03 0.30715000000000E+03 0.32054909861245E+03 0.35728671867721E+03 0.30715000000000E+03
 0.30715000000000E+03 0.32158576504278E+03 0.35748571166897E+03 0.30715000000000E+03 0.30715000000000E+03
 0.32054909861245E+03 0.35728671867721E+03 0.30715000000000E+03 0.30715000000000E+03 0.40812594153716E+03
 0.30863278038362E+03 0.13364544416078E+04 0.12998281018044E+04 0.13489167059510E+04 0.23012074055392E+04
 0.94554611605842E+03 0.94580137891056E+03 0.19544832422370E+04 0.91269100797269E+03 0.30947841355723E+04
 0.89781627511825E+03 0.19418159122207E+04 0.86879479450587E+03 0.30841635686463E+04 0.94580137891056E+03
 0.19544832422370E+04 0.91269100797268E+03 0.30947841355723E+04 0.89781627511825E+03 0.19418159122207E+04
 0.86879479450587E+03 0.30841635686464E+04 0.19228923284807E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75125979811235E+05 0.50050000000000E+08 0.67797723232981E+03 0.12275103898985E+01
 0.12000000000000E+01 0.58262354785833E+00 0.53673628827225E+00 0.30738987407509E+03 0.32393282095383E+03
 0.31801335435064E+03 0.31781264109848E+03 0.23000000000000E+00 0.00000000000000E+00 0.21704092132592E+00
 0.00000000000000E+00 -.44466453055173E-01 0.99888496135882E-03 0.24768739087035E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32298777793609E+02 0.12112041672603E+02 0.30863216244838E+03 0.40818708066339E+03
 0.30756878088510E+03 0.30976191842987E+03 0.30715000000000E+03 0.30715000000000E+03 0.30756311898962E+03
 0.30976240565402E+03 0.30715000000000E+03 0.30715000000000E+03 0.30756878088510E+03 0.30976191842987E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30756311898962E+03 0.30976240565402E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30928880332333E+03 0.30715000000001E+03 0.26954534977421E+02 0.24591317976192E+02
 0.30210054196868E+02 0.13034984034175E+03 0.99988735873896E+02 0.26669337444986E+02 0.26214251327540E+02
 0.26482753731581E+02 0.10238344220761E+03 0.26360924276675E+02 0.26208978158806E+02 0.26181825940799E+02
 0.10237502406260E+03 0.26669337444985E+02 0.26214251327540E+02 0.26482753731580E+02 0.10238344220761E+03
 0.26360924276675E+02 0.26208978158806E+02 0.26181825940799E+02 0.10237502406260E+03 0.59739609675877E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33636804894435E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41950305631564E-02 0.19481979960003E-01 0.00000000000000E+00 0.41950305631564E-02 0.19481979960003E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26320822175768E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13346385277483E-03 0.26320822175768E-01 0.13346385277483E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.61322786497926E-01 0.40336838407635E-01
 0.00000000000000E+00 0.61322786497926E-01 0.40336838407635E-01 0.45586814413612E+00 0.13086814115589E+00
 0.32500000298023E+00 0.42250006586313E+00
    201.09233020
 0.18122130860965E+01 0.31121729504389E+03 0.49565018772783E+03 0.38423962051834E+03 0.36498992255312E+03
 0.22999999987852E+00 0.00000000000000E+00 0.12601585760216E+00 0.00000000000000E+00 -.46631347472155E+01
 0.98688559212086E-03 0.13057842637427E+01 0.80000000000000E+04 0.30000000000000E+04 0.61265863145495E+01
 0.22974698679560E+01 0.32129313895601E+03 0.30715000000002E+03 0.32230590557894E+03 0.35967371928163E+03
 0.30715000000000E+03 0.30715000000001E+03 0.32125529896681E+03 0.35947847561404E+03 0.30715000000000E+03
 0.30715000000001E+03 0.32230590557894E+03 0.35967371928163E+03 0.30715000000000E+03 0.30715000000001E+03
 0.32125529896681E+03 0.35947847561404E+03 0.30715000000000E+03 0.30715000000001E+03 0.41121326443138E+03
 0.30879072385862E+03 0.13532192785973E+04 0.13152366771969E+04 0.13459529235500E+04 0.22595395734690E+04
 0.90685688530124E+03 0.95754591177222E+03 0.19741209264498E+04 0.92339660692770E+03 0.30929509313966E+04
 0.90920706494518E+03 0.19632009805349E+04 0.87922964289082E+03 0.30840297128692E+04 0.95754591177221E+03
 0.19741209264498E+04 0.92339660692769E+03 0.30929509313966E+04 0.90920706494518E+03 0.19632009805349E+04
 0.87922964289082E+03 0.30840297128692E+04 0.19201269948919E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75548777124953E+05 0.50050000000000E+08 0.67812641002103E+03 0.12271760051761E+01
 0.12000000000000E+01 0.62456501584366E+00 0.53068376881802E+00 0.30742807446279E+03 0.32318315762935E+03
 0.31760917968744E+03 0.31742787334931E+03 0.23000000000000E+00 0.00000000000000E+00 0.21638954553356E+00
 0.00000000000000E+00 -.43999002019118E-01 0.99876082912737E-03 0.26088000110621E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30665439919034E+02 0.11499539969638E+02 0.30879010083008E+03 0.41127384607806E+03
 0.30758862068938E+03 0.30975197992673E+03 0.30715000000000E+03 0.30715000000000E+03 0.30758282514336E+03
 0.30975238103169E+03 0.30715000000000E+03 0.30715000000000E+03 0.30758862068938E+03 0.30975197992673E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30758282514336E+03 0.30975238103169E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30928742028638E+03 0.30715000000001E+03 0.25914235208967E+02 0.23242957465425E+02
 0.30375108296246E+02 0.12470265836998E+03 0.94175674532257E+02 0.26971415153699E+02 0.26260978619720E+02
 0.26808526341592E+02 0.97775503594076E+02 0.26664727965921E+02 0.26246381816720E+02 0.26509281719166E+02
 0.97758345455661E+02 0.26971415153698E+02 0.26260978619720E+02 0.26808526341591E+02 0.97775503594076E+02
 0.26664727965921E+02 0.26246381816720E+02 0.26509281719166E+02 0.97758345455661E+02 0.57293359414640E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33597017220095E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42949532400658E-02 0.19291441657223E-01 0.00000000000000E+00 0.42949532400658E-02 0.19291441657223E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26623804372110E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11168820999418E-03 0.26623804372110E-01 0.11168820999418E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.61547630634983E-01 0.39087519696123E-01
 0.00000000000000E+00 0.61547630634983E-01 0.39087519696123E-01 0.45284188440901E+00 0.12784188142878E+00
 0.32500000298023E+00 0.42250006586313E+00
    211.57769719
 0.18122365862966E+01 0.31168434777874E+03 0.49585772390650E+03 0.38460248042633E+03 0.36542119598408E+03
 0.22999999987924E+00 0.00000000000000E+00 0.11893949702040E+00 0.00000000000000E+00 -.46928256307499E+01
 0.98540647513212E-03 0.13924281641843E+01 0.80000000000000E+04 0.30000000000000E+04 0.57453592262596E+01
 0.21545097098473E+01 0.32199892075256E+03 0.30715000000005E+03 0.32300140205976E+03 0.36174235193983E+03
 0.30715000000000E+03 0.30715000000001E+03 0.32193590813453E+03 0.36155221773919E+03 0.30715000000000E+03
 0.30715000000001E+03 0.32300140205976E+03 0.36174235193983E+03 0.30715000000000E+03 0.30715000000001E+03
 0.32193590813453E+03 0.36155221773919E+03 0.30715000000000E+03 0.30715000000001E+03 0.41406107654669E+03
 0.30897678722871E+03 0.13681004510501E+04 0.13289594095062E+04 0.13423982303008E+04 0.22219184271256E+04
 0.87280820567328E+03 0.96783691843677E+03 0.19911853907266E+04 0.93279588746537E+03 0.30907972974005E+04
 0.91929436463211E+03 0.19817391099641E+04 0.88850741824429E+03 0.30832889154453E+04 0.96783691843677E+03
 0.19911853907266E+04 0.93279588746537E+03 0.30907972974005E+04 0.91929436463211E+03 0.19817391099641E+04
 0.88850741824428E+03 0.30832889154453E+04 0.19173928883066E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75694146719870E+05 0.50050000000000E+08 0.67835136031070E+03 0.12270781642273E+01
 0.12000000000000E+01 0.66650648382898E+00 0.52461307986969E+00 0.30747141080843E+03 0.32258185363617E+03
 0.31729709633613E+03 0.31713106807318E+03 0.23000000000000E+00 0.00000000000000E+00 0.21567062488263E+00
 0.00000000000000E+00 -.44066751790019E-01 0.99862004214774E-03 0.27527951351667E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29061370742054E+02 0.10898014028270E+02 0.30897617088845E+03 0.41412122651525E+03
 0.30760896432362E+03 0.30974535361447E+03 0.30715000000000E+03 0.30715000000000E+03 0.30760301753533E+03
 0.30974565951882E+03 0.30715000000000E+03 0.30715000000000E+03 0.30760896432362E+03 0.30974535361447E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30760301753533E+03 0.30974565951882E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30928805805622E+03 0.30715000000002E+03 0.24928192144314E+02 0.21877851340368E+02
 0.30826875345613E+02 0.12053485737027E+03 0.89553847647929E+02 0.27461168667858E+02 0.26578646958357E+02
 0.27327062801762E+02 0.94383678004669E+02 0.27151701870259E+02 0.26554419654638E+02 0.27024917930659E+02
 0.94357514987762E+02 0.27461168667858E+02 0.26578646958358E+02 0.27327062801762E+02 0.94383678004670E+02
 0.27151701870259E+02 0.26554419654638E+02 0.27024917930659E+02 0.94357514987762E+02 0.55443675749237E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33569785634805E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43797932745701E-02 0.19135648170568E-01 0.00000000000000E+00 0.43797932745701E-02 0.19135648170568E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26952037951776E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.90951534215348E-04 0.26952037951776E-01 0.90951534215348E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.61865908921053E-01 0.38112194954798E-01
 0.00000000000000E+00 0.61865908921053E-01 0.38112194954798E-01 0.44980653993484E+00 0.12480653695461E+00
 0.32500000298023E+00 0.42250006586313E+00
    220.31550302
 0.18122450515715E+01 0.31208195606609E+03 0.49610619476917E+03 0.38494052278288E+03 0.36580427489343E+03
 0.22999999987933E+00 0.00000000000000E+00 0.11312541090561E+00 0.00000000000000E+00 -.47200577237680E+01
 0.98415075265180E-03 0.14633335588430E+01 0.80000000000000E+04 0.30000000000000E+04 0.54669695447462E+01
 0.20501135792798E+01 0.32256885318320E+03 0.30715000000009E+03 0.32356481045760E+03 0.36339039753314E+03
 0.30715000000001E+03 0.30715000000002E+03 0.32248643010106E+03 0.36320517086716E+03 0.30715000000001E+03
 0.30715000000002E+03 0.32356481045760E+03 0.36339039753314E+03 0.30715000000001E+03 0.30715000000002E+03
 0.32248643010106E+03 0.36320517086716E+03 0.30715000000001E+03 0.30715000000002E+03 0.41628759285416E+03
 0.30915507018404E+03 0.13796193567198E+04 0.13396433683892E+04 0.13392172954153E+04 0.21933293987035E+04
 0.84741601681115E+03 0.97576048528397E+03 0.20040265452331E+04 0.94007374676245E+03 0.30891492150571E+04
 0.92709641225341E+03 0.19956432257705E+04 0.89573126617311E+03 0.30826473209309E+04 0.97576048528397E+03
 0.20040265452331E+04 0.94007374676246E+03 0.30891492150571E+04 0.92709641225341E+03 0.19956432257706E+04
 0.89573126617311E+03 0.30826473209310E+04 0.19154261487268E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75746511958842E+05 0.50050000000000E+08 0.67860702200957E+03 0.12270498107721E+01
 0.12000000000000E+01 0.70145770715009E+00 0.51960304568894E+00 0.30751233724375E+03 0.32217299204710E+03
 0.31709451145536E+03 0.31693883546303E+03 0.23000000000000E+00 0.00000000000000E+00 0.21502157911921E+00
 0.00000000000000E+00 -.44507294255565E-01 0.99848711931565E-03 0.28817966007769E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27760460255395E+02 0.10410172595773E+02 0.30915447077765E+03 0.41634728036154E+03
 0.30762655718926E+03 0.30974289505784E+03 0.30715000000000E+03 0.30715000000000E+03 0.30762046661834E+03
 0.30974311454772E+03 0.30715000000000E+03 0.30715000000000E+03 0.30762655718926E+03 0.30974289505784E+03
 0.30715000000000E+03 0.30715000000000E+03 0.30762046661834E+03 0.30974311454772E+03 0.30715000000000E+03
 0.30715000000000E+03 0.30929065885856E+03 0.30715000000003E+03 0.24112761234632E+02 0.20686182192475E+02
 0.31390018478671E+02 0.11796789110490E+03 0.86420922533831E+02 0.28003179292416E+02 0.27017044606056E+02
 0.27897078323574E+02 0.92299741825308E+02 0.27688311946545E+02 0.26984527230988E+02 0.27589335685677E+02
 0.92265844587409E+02 0.28003179292416E+02 0.27017044606056E+02 0.27897078323573E+02 0.92299741825309E+02
 0.27688311946545E+02 0.26984527230988E+02 0.27589335685677E+02 0.92265844587409E+02 0.54258685176906E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33553870820379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44286745708952E-02 0.19051242858754E-01 0.00000000000000E+00 0.44286745708952E-02 0.19051242858754E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27210086706261E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76552123399769E-04 0.27210086706261E-01 0.76552123399769E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83875881399919E-04 0.62167831828141E-01 0.37409489309153E-01
 0.00000000000000E+00 0.62167831828141E-01 0.37493365190553E-01 0.44730152284447E+00 0.12230151986424E+00
 0.32500000298023E+00 0.42250006586313E+00
    231.27622364
 0.18122586410252E+01 0.31258971792582E+03 0.49649532058768E+03 0.38540048140205E+03 0.36630868321492E+03
 0.22999999988372E+00 0.00000000000000E+00 0.10593575101434E+00 0.00000000000000E+00 -.47570406351287E+01
 0.98255176748763E-03 0.15506618298580E+01 0.80000000000000E+04 0.30000000000000E+04 0.51590874592769E+01
 0.19346577972288E+01 0.32326189083609E+03 0.30715000000019E+03 0.32425199545359E+03 0.36537353441939E+03
 0.30715000000001E+03 0.30715000000003E+03 0.32315692903856E+03 0.36519482301847E+03 0.30715000000001E+03
 0.30715000000003E+03 0.32425199545359E+03 0.36537353441939E+03 0.30715000000001E+03 0.30715000000003E+03
 0.32315692903856E+03 0.36519482301847E+03 0.30715000000001E+03 0.30715000000003E+03 0.41893342417364E+03
 0.30941182455830E+03 0.13932790530880E+04 0.13524091145773E+04 0.13347743092597E+04 0.21600189252883E+04
 0.81857074448229E+03 0.98515360501533E+03 0.20186452654716E+04 0.94877135509846E+03 0.30872241106034E+04
 0.93635170351305E+03 0.20114234175469E+04 0.90437446461371E+03 0.30818101908387E+04 0.98515360501532E+03
 0.20186452654716E+04 0.94877135509845E+03 0.30872241106034E+04 0.93635170351305E+03 0.20114234175470E+04
 0.90437446461371E+03 0.30818101908387E+04 0.19132801802254E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75830574792212E+05 0.50050000000000E+08 0.67900571673024E+03 0.12269896229820E+01
 0.12000000000000E+01 0.74530058960639E+00 0.51341753427527E+00 0.30757109375224E+03 0.32175459175479E+03
 0.31689988737354E+03 0.31675492964107E+03 0.22999999772619E+00 0.00000000000000E+00 0.21414695085497E+00
 0.00000000000000E+00 -.45516400016921E-01 0.99829634825065E-03 0.30545990609762E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26190016562905E+02 0.98212562110893E+01 0.30941126060496E+03 0.41899224580995E+03
 0.30764954464542E+03 0.30974443494002E+03 0.30715000000000E+03 0.30715000000001E+03 0.30764324739268E+03
 0.30974453848778E+03 0.30715000000000E+03 0.30715000000001E+03 0.30764954464542E+03 0.30974443494002E+03
 0.30715000000000E+03 0.30715000000001E+03 0.30764324739268E+03 0.30974453848778E+03 0.30715000000000E+03
 0.30715000000001E+03 0.30929721556800E+03 0.30715000000007E+03 0.23043105079283E+02 0.19055764987788E+02
 0.32288741024538E+02 0.11565671610369E+03 0.83206531374026E+02 0.28836119214936E+02 0.27744010054993E+02
 0.28770045570914E+02 0.90427184899719E+02 0.28510651229861E+02 0.27700842450725E+02 0.28451212629137E+02
 0.90383370921497E+02 0.28836119214936E+02 0.27744010054993E+02 0.28770045570914E+02 0.90427184899720E+02
 0.28510651229861E+02 0.27700842450725E+02 0.28451212629137E+02 0.90383370921497E+02 0.53111426000899E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33540055779092E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44589778886346E-02 0.19007686622764E-01 0.00000000000000E+00 0.44589778886346E-02 0.19007686622764E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27494492708580E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.62451139693740E-04 0.27494492708580E-01 0.62451139693740E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.24557179248923E-03 0.62565364093943E-01 0.36688806062585E-01
 0.00000000000000E+00 0.62565364093943E-01 0.36934377855074E-01 0.44420876713763E+00 0.11920876415740E+00
 0.32500000298023E+00 0.42250006586313E+00
    242.71229787
 0.18122810544194E+01 0.31312827439451E+03 0.49697479279837E+03 0.38591427205694E+03 0.36685754331784E+03
 0.22999999988485E+00 0.00000000000000E+00 0.98555506464510E-01 0.00000000000000E+00 -.47978813396803E+01
 0.98086145890405E-03 0.16399006449113E+01 0.80000000000000E+04 0.30000000000000E+04 0.48783443221543E+01
 0.18293791208079E+01 0.32396461051514E+03 0.30715000000040E+03 0.32495100035307E+03 0.36735528420237E+03
 0.30715000000002E+03 0.30715000000007E+03 0.32383799357642E+03 0.36718377957930E+03 0.30715000000002E+03
 0.30715000000007E+03 0.32495100035307E+03 0.36735528420237E+03 0.30715000000002E+03 0.30715000000007E+03
 0.32383799357642E+03 0.36718377957930E+03 0.30715000000002E+03 0.30715000000007E+03 0.42152382059087E+03
 0.30971903725064E+03 0.14067906917311E+04 0.13651341020910E+04 0.13299799729518E+04 0.21283384717853E+04
 0.79170859896879E+03 0.99445748948157E+03 0.20324036902508E+04 0.95745759500603E+03 0.30853470165605E+04
 0.94551385968005E+03 0.20262193215025E+04 0.91300502939614E+03 0.30808908243535E+04 0.99445748948157E+03
 0.20324036902508E+04 0.95745759500603E+03 0.30853470165605E+04 0.94551385968005E+03 0.20262193215025E+04
 0.91300502939614E+03 0.30808908243535E+04 0.19115066310393E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.75969221528838E+05 0.50050000000000E+08 0.67949873897400E+03 0.12268736473132E+01
 0.12000000000000E+01 0.79104488653367E+00 0.50715112315806E+00 0.30764292541484E+03 0.32140895053720E+03
 0.31675464713571E+03 0.31661885112638E+03 0.22999999815218E+00 0.00000000000000E+00 0.21316573613607E+00
 0.00000000000000E+00 -.46981968409304E-01 0.99806322486842E-03 0.32474314613472E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24634854023004E+02 0.92380702586266E+01 0.30971852751076E+03 0.42158128044916E+03
 0.30767505817393E+03 0.30975083524875E+03 0.30715000000000E+03 0.30715000000001E+03 0.30766850768335E+03
 0.30975080599778E+03 0.30715000000000E+03 0.30715000000001E+03 0.30767505817393E+03 0.30975083524875E+03
 0.30715000000000E+03 0.30715000000001E+03 0.30766850768335E+03 0.30975080599778E+03 0.30715000000000E+03
 0.30715000000001E+03 0.30930752362370E+03 0.30715000000014E+03 0.21847590926865E+02 0.17167357398032E+02
 0.33434953491243E+02 0.11414389072603E+03 0.80541762467328E+02 0.29890298743879E+02 0.28688188140888E+02
 0.29868800093965E+02 0.89202539271254E+02 0.29550195827641E+02 0.28633320105005E+02 0.29534047169902E+02
 0.89147852704616E+02 0.29890298743879E+02 0.28688188140888E+02 0.29868800093965E+02 0.89202539271254E+02
 0.29550195827641E+02 0.28633320105005E+02 0.29534047169903E+02 0.89147852704615E+02 0.52239301572750E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33532741290523E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44707335690066E-02 0.19002730332032E-01 0.00000000000000E+00 0.44707335690066E-02 0.19002730332032E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27787457557057E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.49813183027540E-04 0.27787457557057E-01 0.49813183027540E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.39005558361254E-03 0.63025334083542E-01 0.36123985305253E-01
 0.00000000000000E+00 0.63025334083542E-01 0.36514040888866E-01 0.44107556157903E+00 0.11607555859880E+00
 0.32500000298023E+00 0.42250006586313E+00
    251.28935354
 0.18123051309982E+01 0.31353717632795E+03 0.49737352676250E+03 0.38631767304366E+03 0.36728132617769E+03
 0.22999999988406E+00 0.00000000000000E+00 0.93100901387838E-01 0.00000000000000E+00 -.48299208507905E+01
 0.97958195108635E-03 0.17055947109578E+01 0.80000000000000E+04 0.30000000000000E+04 0.46904460647087E+01
 0.17589172742658E+01 0.32448005248660E+03 0.30715000000070E+03 0.32546502183046E+03 0.36879108825955E+03
 0.30715000000003E+03 0.30715000000013E+03 0.32433827315673E+03 0.36862508501054E+03 0.30715000000003E+03
 0.30715000000013E+03 0.32546502183046E+03 0.36879108825955E+03 0.30715000000003E+03 0.30715000000013E+03
 0.32433827315673E+03 0.36862508501054E+03 0.30715000000003E+03 0.30715000000013E+03 0.42337065002225E+03
 0.30997559387436E+03 0.14165336703495E+04 0.13743708866701E+04 0.13262470504868E+04 0.21062281695361E+04
 0.77334988379686E+03 0.10011818069611E+04 0.20418579569130E+04 0.96378054397275E+03 0.30839823936107E+04
 0.95212661094174E+03 0.20363524169805E+04 0.91928033465087E+03 0.30801448942554E+04 0.10011818069611E+04
 0.20418579569130E+04 0.96378054397275E+03 0.30839823936107E+04 0.95212661094173E+03 0.20363524169805E+04
 0.91928033465086E+03 0.30801448942554E+04 0.19104262126075E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.76118156537059E+05 0.50050000000000E+08 0.67991116155312E+03 0.12267424938950E+01
 0.12000000000000E+01 0.82535310922913E+00 0.50261276491970E+00 0.30770518801633E+03 0.32119985232139E+03
 0.31667812529604E+03 0.31654817758480E+03 0.22999999812091E+00 0.00000000000000E+00 0.21238557514439E+00
 0.00000000000000E+00 -.48357988862389E-01 0.99786124629065E-03 0.34001657960302E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23528264443282E+02 0.88230991662306E+01 0.30997513745927E+03 0.42342675766560E+03
 0.30769531940604E+03 0.30975881879384E+03 0.30715000000000E+03 0.30715000000001E+03 0.30768854528360E+03
 0.30975868218695E+03 0.30715000000000E+03 0.30715000000001E+03 0.30769531940604E+03 0.30975881879384E+03
 0.30715000000000E+03 0.30715000000001E+03 0.30768854528360E+03 0.30975868218695E+03 0.30715000000000E+03
 0.30715000000001E+03 0.30931762495381E+03 0.30715000000025E+03 0.20880356637957E+02 0.15608234710702E+02
 0.34406677217050E+02 0.11349260358450E+03 0.78913892981363E+02 0.30789227596778E+02 0.29497156182510E+02
 0.30794152730161E+02 0.88677032566148E+02 0.30435903930565E+02 0.29433283299712E+02 0.30445352193179E+02
 0.88614003837913E+02 0.30789227596777E+02 0.29497156182510E+02 0.30794152730161E+02 0.88677032566148E+02
 0.30435903930565E+02 0.29433283299712E+02 0.30445352193179E+02 0.88614003837913E+02 0.51749722053414E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33530676513014E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44633432150612E-02 0.19031224720425E-01 0.00000000000000E+00 0.44633432150612E-02 0.19031224720425E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27991019932023E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.42061608981972E-04 0.27991019932023E-01 0.42061608981972E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.48575696506499E-03 0.63381849816093E-01 0.35812577483887E-01
 0.00000000000000E+00 0.63381849816093E-01 0.36298334448952E-01 0.43880638245985E+00 0.11380637947962E+00
 0.32500000298023E+00 0.42250006586313E+00
    262.72542778
 0.18123465609058E+01 0.31408780369800E+03 0.49794357304850E+03 0.38687344928681E+03 0.36785860764449E+03
 0.22999999988489E+00 0.00000000000000E+00 0.85935341550800E-01 0.00000000000000E+00 -.48740369741370E+01
 0.97786422006783E-03 0.17915726082518E+01 0.80000000000000E+04 0.30000000000000E+04 0.44653506998001E+01
 0.16745065124250E+01 0.32515346658884E+03 0.30715000000137E+03 0.32613815430930E+03 0.37064548393805E+03
 0.30715000000006E+03 0.30715000000027E+03 0.32499275776908E+03 0.37048677944880E+03 0.30715000000005E+03
 0.30715000000027E+03 0.32613815430930E+03 0.37064548393806E+03 0.30715000000006E+03 0.30715000000027E+03
 0.32499275776908E+03 0.37048677944880E+03 0.30715000000005E+03 0.30715000000027E+03 0.42572096643628E+03
 0.31035269235460E+03 0.14290596188256E+04 0.13863199333845E+04 0.13210466296525E+04 0.20785131876697E+04
 0.75086132486893E+03 0.10098480765057E+04 0.20534062199942E+04 0.97198383500709E+03 0.30821077012840E+04
 0.96063635799483E+03 0.20486924739932E+04 0.92741157724537E+03 0.30789829557689E+04 0.10098480765057E+04
 0.20534062199942E+04 0.97198383500709E+03 0.30821077012839E+04 0.96063635799483E+03 0.20486924739933E+04
 0.92741157724536E+03 0.30789829557689E+04 0.19092101039711E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.76374437287123E+05 0.50050000000000E+08 0.68050417244485E+03 0.12265121824484E+01
 0.12000000000000E+01 0.87109740615641E+00 0.49681056931795E+00 0.30780102262727E+03 0.32097580110872E+03
 0.31661222164338E+03 0.31648905467687E+03 0.22999999818566E+00 0.00000000000000E+00 0.21128880227585E+00
 0.00000000000000E+00 -.50525071388007E-01 0.99755052284814E-03 0.36142090248093E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22134857018742E+02 0.83005713820283E+01 0.31035231847136E+03 0.42577497236059E+03
 0.30772396320223E+03 0.30977365755986E+03 0.30715000000000E+03 0.30715000000003E+03 0.30771688064620E+03
 0.30977336813415E+03 0.30715000000000E+03 0.30715000000003E+03 0.30772396320223E+03 0.30977365755986E+03
 0.30715000000000E+03 0.30715000000003E+03 0.30771688064620E+03 0.30977336813415E+03 0.30715000000000E+03
 0.30715000000003E+03 0.30933426538435E+03 0.30715000000048E+03 0.19475878235854E+02 0.13317335167622E+02
 0.35825940946646E+02 0.11314799234819E+03 0.77142921696816E+02 0.32122527960069E+02 0.30686237132275E+02
 0.32187545749729E+02 0.88401201122069E+02 0.31748615410445E+02 0.30610116098561E+02 0.31821279180243E+02
 0.88326860618769E+02 0.32122527960069E+02 0.30686237132274E+02 0.32187545749729E+02 0.88401201122067E+02
 0.31748615410438E+02 0.30610116098562E+02 0.31821279180235E+02 0.88326860618770E+02 0.51266007897814E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33531896412960E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44370281177563E-02 0.19102012236499E-01 0.00000000000000E+00 0.44370281177563E-02 0.19102012236499E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28252255307711E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.33296233481085E-04 0.28252255307711E-01 0.33296233481085E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.59922475647439E-03 0.63872644384104E-01 0.35508221172011E-01
 0.00000000000000E+00 0.63872644384104E-01 0.36107445928485E-01 0.43590528465897E+00 0.11090528167874E+00
 0.32500000298023E+00 0.42250006586313E+00
    271.30248345
 0.18123836706288E+01 0.31450410943130E+03 0.49839321873701E+03 0.38730067909963E+03 0.36829875364030E+03
 0.22999999988473E+00 0.00000000000000E+00 0.80641602436430E-01 0.00000000000000E+00 -.49079547147963E+01
 0.97656950472297E-03 0.18548620018253E+01 0.80000000000000E+04 0.30000000000000E+04 0.43129893178725E+01
 0.16173709942022E+01 0.32564914495040E+03 0.30715000000222E+03 0.32663469340765E+03 0.37199536116560E+03
 0.30715000000010E+03 0.30715000000046E+03 0.32547511056163E+03 0.37184203039535E+03 0.30715000000008E+03
 0.30715000000046E+03 0.32663469340765E+03 0.37199536116560E+03 0.30715000000010E+03 0.30715000000046E+03
 0.32547511056163E+03 0.37184203039535E+03 0.30715000000008E+03 0.30715000000046E+03 0.42740766654236E+03
 0.31066157055368E+03 0.14381381521710E+04 0.13950308103048E+04 0.13169907589402E+04 0.20588656896972E+04
 0.73528997696235E+03 0.10161432711382E+04 0.20613480369709E+04 0.97797960205720E+03 0.30806271338267E+04
 0.96680953028149E+03 0.20571539566091E+04 0.93334810995708E+03 0.30779643065371E+04 0.10161432711382E+04
 0.20613480369709E+04 0.97797960205720E+03 0.30806271338267E+04 0.96680953028148E+03 0.20571539566091E+04
 0.93334810995707E+03 0.30779643065372E+04 0.19084264855911E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.76603993860136E+05 0.50050000000000E+08 0.68097414203208E+03 0.12263038281285E+01
 0.12000000000000E+01 0.90540562885186E+00 0.49265880048169E+00 0.30788345828643E+03 0.32084250208430E+03
 0.31658624410240E+03 0.31646757512289E+03 0.22999999822607E+00 0.00000000000000E+00 0.21042521560338E+00
 0.00000000000000E+00 -.52384360024659E-01 0.99728339944490E-03 0.37822931872757E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21151189513583E+02 0.79316960675935E+01 0.31066126546273E+03 0.42745992904428E+03
 0.30774682921589E+03 0.30978782393939E+03 0.30715000000001E+03 0.30715000000004E+03 0.30773949117503E+03
 0.30978741301007E+03 0.30715000000001E+03 0.30715000000004E+03 0.30774682921589E+03 0.30978782393939E+03
 0.30715000000001E+03 0.30715000000004E+03 0.30773949117503E+03 0.30978741301007E+03 0.30715000000001E+03
 0.30715000000004E+03 0.30934906850203E+03 0.30715000000077E+03 0.18326677989321E+02 0.11430738248133E+02
 0.36968606324452E+02 0.11321867315935E+03 0.76065223803280E+02 0.33217371114781E+02 0.31647854823841E+02
 0.33351870774963E+02 0.88461033527382E+02 0.32826062310927E+02 0.31562417799937E+02 0.32969795423784E+02
 0.88378117421328E+02 0.33217371114781E+02 0.31647854823843E+02 0.33351870774963E+02 0.88461033527384E+02
 0.32826062310927E+02 0.31562417799940E+02 0.32969795423784E+02 0.88378117421333E+02 0.51002425385065E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33535425409670E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44069135404504E-02 0.19175889899574E-01 0.00000000000000E+00 0.44069135404504E-02 0.19175889899574E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28444622485403E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27652514110621E-04 0.28444622485403E-01 0.27652514110621E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.67473129405987E-03 0.64252001857301E-01 0.35345096472869E-01
 0.00000000000000E+00 0.64252001857301E-01 0.36019827766929E-01 0.43382940024085E+00 0.10882939726061E+00
 0.32500000298023E+00 0.42250006586313E+00
    281.12364300
 0.18124317344339E+01 0.31498354808975E+03 0.49892468050963E+03 0.38779776528780E+03 0.36880827772951E+03
 0.22999999988318E+00 0.00000000000000E+00 0.74664744565315E-01 0.00000000000000E+00 -.49474892271840E+01
 0.97508268098953E-03 0.19260937685839E+01 0.80000000000000E+04 0.30000000000000E+04 0.41534841815526E+01
 0.15575565680822E+01 0.32620701372506E+03 0.30715000000377E+03 0.32719455480524E+03 0.37350148826720E+03
 0.30715000000018E+03 0.30715000000080E+03 0.32601856173194E+03 0.37335411129484E+03 0.30715000000015E+03
 0.30715000000080E+03 0.32719455480524E+03 0.37350148826720E+03 0.30715000000018E+03 0.30715000000080E+03
 0.32601856173194E+03 0.37335411129484E+03 0.30715000000015E+03 0.30715000000080E+03 0.42926972506395E+03
 0.31104336809758E+03 0.14482261690907E+04 0.14047617779062E+04 0.13121268968293E+04 0.20372831203555E+04
 0.71859558904202E+03 0.10231518642743E+04 0.20697409354621E+04 0.98469248974008E+03 0.30788092381979E+04
 0.97367439903698E+03 0.20660744987273E+04 0.93998846500886E+03 0.30766104217328E+04 0.10231518642743E+04
 0.20697409354621E+04 0.98469248974009E+03 0.30788092381978E+04 0.97367439903697E+03 0.20660744987273E+04
 0.93998846500886E+03 0.30766104217328E+04 0.19075968493798E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.76901311152477E+05 0.50050000000000E+08 0.68153180331775E+03 0.12260327454951E+01
 0.12000000000000E+01 0.94469026703910E+00 0.48812411341706E+00 0.30798990789556E+03 0.32072076107716E+03
 0.31657793679562E+03 0.31646395101012E+03 0.22999999826140E+00 0.00000000000000E+00 0.20939470022461E+00
 0.00000000000000E+00 -.54751408755221E-01 0.99693867534952E-03 0.39824399276348E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20088187506575E+02 0.75330703149657E+01 0.31104314614159E+03 0.42931988114985E+03
 0.30777445390535E+03 0.30980713146021E+03 0.30715000000001E+03 0.30715000000007E+03 0.30776679793981E+03
 0.30980657540106E+03 0.30715000000001E+03 0.30715000000007E+03 0.30777445390535E+03 0.30980713146021E+03
 0.30715000000001E+03 0.30715000000007E+03 0.30776679793981E+03 0.30980657540106E+03 0.30715000000001E+03
 0.30715000000007E+03 0.30936838980905E+03 0.30715000000129E+03 0.16893740184205E+02 0.90745361797441E+01
 0.38344866495440E+02 0.11358732372039E+03 0.75050732892475E+02 0.34565262228126E+02 0.32809601269034E+02
 0.34805579602631E+02 0.88762415150761E+02 0.34152000291092E+02 0.32713420825248E+02 0.34403238233829E+02
 0.88669636659945E+02 0.34565262228126E+02 0.32809601269034E+02 0.34805579602632E+02 0.88762415150760E+02
 0.34152000291091E+02 0.32713420825249E+02 0.34403238233828E+02 0.88669636659947E+02 0.50776822590990E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33541790675440E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43624649578550E-02 0.19280551992280E-01 0.00000000000000E+00 0.43624649578550E-02 0.19280551992280E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28662048488822E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22053123292968E-04 0.28662048488822E-01 0.22053123292968E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.75184080584530E-03 0.64695783479518E-01 0.35214007202704E-01
 0.00000000000000E+00 0.64695783479518E-01 0.35965848008549E-01 0.43156205670853E+00 0.10656205372830E+00
 0.32500000298023E+00 0.42250006586313E+00
    290.94480254
 0.18124843895574E+01 0.31546529268472E+03 0.49947124235145E+03 0.38830193856591E+03 0.36932270134586E+03
 0.22999999988464E+00 0.00000000000000E+00 0.68778426763408E-01 0.00000000000000E+00 -.49876203158513E+01
 0.97359325418911E-03 0.19960109254315E+01 0.80000000000000E+04 0.30000000000000E+04 0.40079940936548E+01
 0.15029977851205E+01 0.32675657712246E+03 0.30715000000620E+03 0.32774704504960E+03 0.37496881202813E+03
 0.30715000000031E+03 0.30715000000138E+03 0.32655450420829E+03 0.37482720830868E+03 0.30715000000026E+03
 0.30715000000137E+03 0.32774704504960E+03 0.37496881202813E+03 0.30715000000031E+03 0.30715000000138E+03
 0.32655450420829E+03 0.37482720830867E+03 0.30715000000026E+03 0.30715000000137E+03 0.43105688024185E+03
 0.31145286366083E+03 0.14580253232025E+04 0.14142585096195E+04 0.13072120948626E+04 0.20168281609819E+04
 0.70308000564507E+03 0.10299706351509E+04 0.20774944824777E+04 0.99125551286554E+03 0.30769074919257E+04
 0.98034724681994E+03 0.20742918529594E+04 0.94647613699597E+03 0.30751111354409E+04 0.10299706351509E+04
 0.20774944824777E+04 0.99125551286555E+03 0.30769074919257E+04 0.98034724681994E+03 0.20742918529594E+04
 0.94647613699597E+03 0.30751111354409E+04 0.19068993555921E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.77227029825507E+05 0.50050000000000E+08 0.68210696596968E+03 0.12257346305403E+01
 0.12000000000000E+01 0.98397490522634E+00 0.48382434572645E+00 0.30811033830903E+03 0.32062813340567E+03
 0.31659052405729E+03 0.31648083351628E+03 0.22999999832801E+00 0.00000000000000E+00 0.20832068860325E+00
 0.00000000000000E+00 -.57365579465133E-01 0.99654896585516E-03 0.41906041567468E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19090326121879E+02 0.71588722957047E+01 0.31145273209807E+03 0.43110482514284E+03
 0.30780391238924E+03 0.30982951436405E+03 0.30715000000001E+03 0.30715000000012E+03 0.30779590845234E+03
 0.30982880584162E+03 0.30715000000001E+03 0.30715000000012E+03 0.30780391238924E+03 0.30982951436405E+03
 0.30715000000001E+03 0.30715000000012E+03 0.30779590845234E+03 0.30982880584162E+03 0.30715000000001E+03
 0.30715000000012E+03 0.30939010875870E+03 0.30715000000210E+03 0.15344457803094E+02 0.65265498513834E+01
 0.39784666212225E+02 0.11422616516042E+03 0.74242575617132E+02 0.36010320507399E+02 0.34028708270334E+02
 0.36387258300183E+02 0.89282914709187E+02 0.35573309185208E+02 0.33921751056426E+02 0.35962848593225E+02
 0.89180282544445E+02 0.36010320507399E+02 0.34028708270334E+02 0.36387258300184E+02 0.89282914709187E+02
 0.35573309185208E+02 0.33921751056426E+02 0.35962848593224E+02 0.89180282544446E+02 0.50622314180341E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33550592413059E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43093514735877E-02 0.19403043849942E-01 0.00000000000000E+00 0.43093514735877E-02 0.19403043849942E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28883425789041E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17160783199173E-04 0.28883425789041E-01 0.17160783199173E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.81884581020993E-03 0.65154945496864E-01 0.35128778030705E-01
 0.00000000000000E+00 0.65154945496864E-01 0.35947623840915E-01 0.42941217286322E+00 0.10441216988299E+00
 0.32500000298023E+00 0.42250006586313E+00
    300.76596209
 0.18125418676864E+01 0.31594875254302E+03 0.50002836501955E+03 0.38881103015248E+03 0.36984054404203E+03
 0.22999999988506E+00 0.00000000000000E+00 0.62982755582236E-01 0.00000000000000E+00 -.50282543212035E+01
 0.97210308710379E-03 0.20646293304588E+01 0.80000000000000E+04 0.30000000000000E+04 0.38747875378785E+01
 0.14530453267044E+01 0.32729832833824E+03 0.30715000000993E+03 0.32829254907615E+03 0.37640060667402E+03
 0.30715000000051E+03 0.30715000000229E+03 0.32708335613761E+03 0.37626454729711E+03 0.30715000000043E+03
 0.30715000000229E+03 0.32829254907615E+03 0.37640060667402E+03 0.30715000000051E+03 0.30715000000229E+03
 0.32708335613761E+03 0.37626454729711E+03 0.30715000000043E+03 0.30715000000229E+03 0.43277796014651E+03
 0.31188942938507E+03 0.14675633447890E+04 0.14235432524322E+04 0.13022044040089E+04 0.19972843423397E+04
 0.68856891631082E+03 0.10366199978426E+04 0.20846529659337E+04 0.99768511902303E+03 0.30748890273660E+04
 0.98684710661114E+03 0.20818588501710E+04 0.95282615571354E+03 0.30734425510943E+04 0.10366199978426E+04
 0.20846529659337E+04 0.99768511902303E+03 0.30748890273660E+04 0.98684710661114E+03 0.20818588501711E+04
 0.95282615571354E+03 0.30734425510944E+04 0.19062861185141E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.77582583067722E+05 0.50050000000000E+08 0.68269529405519E+03 0.12254086406457E+01
 0.12000000000000E+01 0.10232595434136E+01 0.47976017197555E+00 0.30824568848846E+03 0.32056098805382E+03
 0.31662206122888E+03 0.31651637402437E+03 0.22999999836940E+00 0.00000000000000E+00 0.20720437782995E+00
 0.00000000000000E+00 -.60230658442414E-01 0.99611134235421E-03 0.44065612002315E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18154746153485E+02 0.68080298075569E+01 0.31188939438399E+03 0.43282363705432E+03
 0.30783523572276E+03 0.30985483482347E+03 0.30715000000001E+03 0.30715000000020E+03 0.30782685430904E+03
 0.30985396769088E+03 0.30715000000001E+03 0.30715000000020E+03 0.30783523572276E+03 0.30985483482347E+03
 0.30715000000001E+03 0.30715000000020E+03 0.30782685430904E+03 0.30985396769088E+03 0.30715000000001E+03
 0.30715000000020E+03 0.30941412720885E+03 0.30715000000333E+03 0.13673545237514E+02 0.37849189927858E+01
 0.41275203479899E+02 0.11509436231466E+03 0.73612782817364E+02 0.37545293388070E+02 0.35295000214006E+02
 0.38093349572941E+02 0.89990177755627E+02 0.37082782927954E+02 0.35177338897424E+02 0.37645175719282E+02
 0.89877800215264E+02 0.37545293388071E+02 0.35295000214006E+02 0.38093349572943E+02 0.89990177755627E+02
 0.37082782927954E+02 0.35177338897424E+02 0.37645175719282E+02 0.89877800215264E+02 0.50522101157089E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33561497071340E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42474466779474E-02 0.19543789480248E-01 0.00000000000000E+00 0.42474466779474E-02 0.19543789480248E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29107411393982E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12977547045387E-04 0.29107411393982E-01 0.12977547045387E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.87598085467439E-03 0.65626620403039E-01 0.35084519259268E-01
 0.00000000000000E+00 0.65626620403039E-01 0.35960500113942E-01 0.42738008598778E+00 0.10238008300754E+00
 0.32500000298023E+00 0.42250006586313E+00
    310.58712164
 0.18126039862349E+01 0.31643338764258E+03 0.50059262696636E+03 0.38932336986627E+03 0.37036062957985E+03
 0.22999999988503E+00 0.00000000000000E+00 0.57277981089430E-01 0.00000000000000E+00 -.50692851901258E+01
 0.97061386481269E-03 0.21319609462444E+01 0.80000000000000E+04 0.30000000000000E+04 0.37524139520908E+01
 0.14071552320340E+01 0.32783290171147E+03 0.30715000001555E+03 0.32883161357687E+03 0.37779941629687E+03
 0.30715000000084E+03 0.30715000000372E+03 0.32760568747243E+03 0.37766865576617E+03 0.30715000000071E+03
 0.30715000000372E+03 0.32883161357687E+03 0.37779941629687E+03 0.30715000000084E+03 0.30715000000372E+03
 0.32760568747243E+03 0.37766865576617E+03 0.30715000000071E+03 0.30715000000372E+03 0.43443799756129E+03
 0.31235205799212E+03 0.14768621106676E+04 0.14326318774583E+04 0.12971171473549E+04 0.19785528888638E+04
 0.67495015577218E+03 0.10431148928812E+04 0.20912641381689E+04 0.10039917886722E+04 0.30727424082641E+04
 0.99318870390817E+03 0.20888303303446E+04 0.95904880658169E+03 0.30716004369532E+04 0.10431148928812E+04
 0.20912641381689E+04 0.10039917886722E+04 0.30727424082641E+04 0.99318870390817E+03 0.20888303303446E+04
 0.95904880658169E+03 0.30716004369533E+04 0.19057390809023E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.77966841422930E+05 0.50050000000000E+08 0.68329331941254E+03 0.12250560885246E+01
 0.12000000000000E+01 0.10625441816008E+01 0.47592911190386E+00 0.30839674849762E+03 0.32051656144718E+03
 0.31667111357150E+03 0.31656920985712E+03 0.22999999843174E+00 0.00000000000000E+00 0.20604676216417E+00
 0.00000000000000E+00 -.63345595099391E-01 0.99562338081110E-03 0.46301183045494E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17278176223142E+02 0.64793160836782E+01 0.31235212397090E+03 0.43448139225091E+03
 0.30786850603668E+03 0.30988294823356E+03 0.30715000000002E+03 0.30715000000033E+03 0.30785971754750E+03
 0.30988191714355E+03 0.30715000000002E+03 0.30715000000033E+03 0.30786850603668E+03 0.30988294823356E+03
 0.30715000000002E+03 0.30715000000033E+03 0.30785971754750E+03 0.30988191714355E+03 0.30715000000002E+03
 0.30715000000033E+03 0.30944034171069E+03 0.30715000000516E+03 0.11880614489069E+02 0.85422396112682E+00
 0.42807322372111E+02 0.11616243721115E+03 0.73141078227180E+02 0.39165606919883E+02 0.36601280592954E+02
 0.39923076341420E+02 0.90860904041468E+02 0.38675959383815E+02 0.36473064120125E+02 0.39449587002133E+02
 0.90738961700735E+02 0.39165606919884E+02 0.36601280592953E+02 0.39923076341422E+02 0.90860904041467E+02
 0.38675959383812E+02 0.36473064120123E+02 0.39449587002130E+02 0.90738961700732E+02 0.50465692876915E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33574372189616E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41774844696260E-02 0.19701624796926E-01 0.00000000000000E+00 0.41774844696260E-02 0.19701624796926E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29336421507849E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94366359751194E-05 0.29336421507849E-01 0.94366359751194E-05 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.92314281243233E-03 0.66111959975471E-01 0.35074894009166E-01
 0.00000000000000E+00 0.66111959975471E-01 0.35998036821598E-01 0.42546455595193E+00 0.10046455297170E+00
 0.32500000298023E+00 0.42250006586313E+00
    320.40828118
 0.18126707125965E+01 0.31691871431952E+03 0.50116122656717E+03 0.38983755791166E+03 0.37088195354948E+03
 0.22999999988507E+00 0.00000000000000E+00 0.51664432651961E-01 0.00000000000000E+00 -.51106282014527E+01
 0.96912707938555E-03 0.21980159472596E+01 0.80000000000000E+04 0.30000000000000E+04 0.36396460225750E+01
 0.13648672584656E+01 0.32836087326028E+03 0.30715000002383E+03 0.32936473941301E+03 0.37916746381712E+03
 0.30715000000135E+03 0.30715000000591E+03 0.32812201736113E+03 0.37904174976018E+03 0.30715000000114E+03
 0.30715000000590E+03 0.32936473941301E+03 0.37916746381712E+03 0.30715000000135E+03 0.30715000000591E+03
 0.32812201736113E+03 0.37904174976018E+03 0.30715000000114E+03 0.30715000000590E+03 0.43604136882639E+03
 0.31283963367435E+03 0.14859414333533E+04 0.14415387420919E+04 0.12919631574016E+04 0.19605515966375E+04
 0.66212862344891E+03 0.10494692457053E+04 0.20973697896659E+04 0.10101853845922E+04 0.30704595268465E+04
 0.99938548844436E+03 0.20952542008800E+04 0.96515350695260E+03 0.30695828267610E+04 0.10494692457053E+04
 0.20973697896659E+04 0.10101853845922E+04 0.30704595268465E+04 0.99938548844436E+03 0.20952542008800E+04
 0.96515350695259E+03 0.30695828267610E+04 0.19052441660926E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.78379603192199E+05 0.50050000000000E+08 0.68389827013937E+03 0.12246773922954E+01
 0.12000000000000E+01 0.11018288197880E+01 0.47232691239036E+00 0.30856416904470E+03 0.32049272031972E+03
 0.31673660312437E+03 0.31663831741727E+03 0.22999999849437E+00 0.00000000000000E+00 0.20484868974280E+00
 0.00000000000000E+00 -.66710211602159E-01 0.99508313121862E-03 0.48611052538145E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16457162686865E+02 0.61714360075744E+01 0.31283980441443E+03 0.43608249260665E+03
 0.30790380555259E+03 0.30991372329179E+03 0.30715000000003E+03 0.30715000000052E+03 0.30789458039698E+03
 0.30991252373204E+03 0.30715000000003E+03 0.30715000000052E+03 0.30790380555259E+03 0.30991372329179E+03
 0.30715000000003E+03 0.30715000000052E+03 0.30789458039698E+03 0.30991252373204E+03 0.30715000000003E+03
 0.30715000000052E+03 0.30946865589214E+03 0.30715000000783E+03 0.99671196904061E+01 -.22584966855417E+01
 0.44373348385240E+02 0.11740726819162E+03 0.72812053064449E+02 0.40867108966633E+02 0.37941773484543E+02
 0.41876071708527E+02 0.91877039023833E+02 0.40348785305359E+02 0.37803226226152E+02 0.41375835335627E+02
 0.91745783689922E+02 0.40867108966633E+02 0.37941773484543E+02 0.41876071708529E+02 0.91877039023833E+02
 0.40348785305356E+02 0.37803226226151E+02 0.41375835335623E+02 0.91745783689920E+02 0.50445763352834E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33589120967281E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.40999873561757E-02 0.19875840264384E-01 0.00000000000000E+00 0.40999873561757E-02 0.19875840264384E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29572454077722E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.64969489220240E-05 0.29572454077722E-01 0.64969489220240E-05 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96015901807871E-03 0.66612129504223E-01 0.35095394986506E-01
 0.00000000000000E+00 0.66612129504223E-01 0.36055554004585E-01 0.42366345619518E+00 0.98663453214950E-01
 0.32500000298023E+00 0.42250006586313E+00
    334.13930645
 0.18127723827563E+01 0.31759751001597E+03 0.50195605751960E+03 0.39055602970639E+03 0.37161051042940E+03
 0.22999999989119E+00 0.00000000000000E+00 0.43969925136921E-01 0.00000000000000E+00 -.51688148678504E+01
 0.96705522537746E-03 0.22882564130002E+01 0.80000000000000E+04 0.30000000000000E+04 0.34961116920945E+01
 0.13110418845354E+01 0.32908575765710E+03 0.30715000004253E+03 0.33009782481090E+03 0.38103169985699E+03
 0.30715000000256E+03 0.30715000001109E+03 0.32883156655652E+03 0.38091251880690E+03 0.30715000000217E+03
 0.30715000001106E+03 0.33009782481090E+03 0.38103169985699E+03 0.30715000000256E+03 0.30715000001109E+03
 0.32883156655652E+03 0.38091251880690E+03 0.30715000000217E+03 0.30715000001106E+03 0.43820864867194E+03
 0.31356712696852E+03 0.14982806221032E+04 0.14537066551356E+04 0.12843627542220E+04 0.19359754973572E+04
 0.64519092936405E+03 0.10581297758890E+04 0.21050596253723E+04 0.10186730557037E+04 0.30669386643501E+04
 0.10078160910379E+04 0.21033294278913E+04 0.97350580739071E+03 0.30663790145984E+04 0.10581297758890E+04
 0.21050596253723E+04 0.10186730557037E+04 0.30669386643501E+04 0.10078160910379E+04 0.21033294278913E+04
 0.97350580739071E+03 0.30663790145985E+04 0.19044756598686E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.79008523368695E+05 0.50050000000000E+08 0.68474876360016E+03 0.12241012065627E+01
 0.12000000000000E+01 0.11567529208737E+01 0.46766665620500E+00 0.30882600667579E+03 0.32049031137937E+03
 0.31685364046092E+03 0.31676020894278E+03 0.22999999855134E+00 0.00000000000000E+00 0.20310765145701E+00
 0.00000000000000E+00 -.71831059207976E-01 0.99423938512098E-03 0.51961407512914E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15396041760439E+02 0.57735156601646E+01 0.31356743797287E+03 0.43824671423766E+03
 0.30795591021398E+03 0.30996067687992E+03 0.30715000000005E+03 0.30715000000097E+03 0.30794603288404E+03
 0.30995923915010E+03 0.30715000000005E+03 0.30715000000097E+03 0.30795591021398E+03 0.30996067687992E+03
 0.30715000000005E+03 0.30715000000097E+03 0.30794603288404E+03 0.30995923915010E+03 0.30715000000005E+03
 0.30715000000097E+03 0.30951129297578E+03 0.30715000001375E+03 0.70495769662880E+01 -.69712301370186E+01
 0.46609963555118E+02 0.11940951969698E+03 0.72566506324083E+02 0.43381757778864E+02 0.39865675085332E+02
 0.44820196286416E+02 0.93514269933942E+02 0.42820249166272E+02 0.39713110709754E+02 0.44279664536477E+02
 0.93370438039325E+02 0.43381757778864E+02 0.39865675085332E+02 0.44820196286416E+02 0.93514269933942E+02
 0.42820249166271E+02 0.39713110709754E+02 0.44279664536476E+02 0.93370438039325E+02 0.50452849863682E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33612440080096E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39787544987607E-02 0.20148002689004E-01 0.00000000000000E+00 0.39787544987607E-02 0.20148002689004E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29910263880642E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.33944880503116E-05 0.29910263880642E-01 0.33944880503116E-05 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.99514060093960E-03 0.67330712029923E-01 0.35173067756874E-01
 0.00000000000000E+00 0.67330712029923E-01 0.36168208357813E-01 0.42133332810250E+00 0.96333325122266E-01
 0.32500000298023E+00 0.42250006586313E+00
    344.59661188
 0.18128547465635E+01 0.31811402203498E+03 0.50256213231718E+03 0.39110292157726E+03 0.37216485813414E+03
 0.22999999989687E+00 0.00000000000000E+00 0.38231035302159E-01 0.00000000000000E+00 -.52132883953235E+01
 0.96548462355991E-03 0.23553240277423E+01 0.80000000000000E+04 0.30000000000000E+04 0.33965602633742E+01
 0.12737100987653E+01 0.32963135740548E+03 0.30715000006463E+03 0.33065038573425E+03 0.38241564878909E+03
 0.30715000000407E+03 0.30715000001747E+03 0.32936614572536E+03 0.38230116708501E+03 0.30715000000346E+03
 0.30715000001744E+03 0.33065038573425E+03 0.38241564878909E+03 0.30715000000407E+03 0.30715000001747E+03
 0.32936614572536E+03 0.38230116708501E+03 0.30715000000346E+03 0.30715000001744E+03 0.43978628039610E+03
 0.31414927015142E+03 0.15074365182440E+04 0.14627631167272E+04 0.12787049433576E+04 0.19183393250559E+04
 0.63324085698150E+03 0.10645702626090E+04 0.21103911370479E+04 0.10250038147511E+04 0.30641210095294E+04
 0.10140773263079E+04 0.21089130420686E+04 0.97972881525648E+03 0.30637645103723E+04 0.10645702626090E+04
 0.21103911370479E+04 0.10250038147511E+04 0.30641210095294E+04 0.10140773263079E+04 0.21089130420687E+04
 0.97972881525650E+03 0.30637645103724E+04 0.19040125175536E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.79518016619679E+05 0.50050000000000E+08 0.68540008870155E+03 0.12236345452916E+01
 0.12000000000000E+01 0.11985821425636E+01 0.46439815907289E+00 0.30904794354799E+03 0.32051159762519E+03
 0.31696246439206E+03 0.31687260530175E+03 0.22999999940741E+00 0.00000000000000E+00 0.20173044287075E+00
 0.00000000000000E+00 -.76040140520622E-01 0.99352533776066E-03 0.54606478749978E+00 0.80000000000000E+04
 0.30000000000000E+04 0.14650276273313E+02 0.54938536024925E+01 0.31414969842161E+03 0.43982204092566E+03
 0.30799878224701E+03 0.30999971697570E+03 0.30715000000008E+03 0.30715000000153E+03 0.30798836490563E+03
 0.30999809262646E+03 0.30715000000008E+03 0.30715000000153E+03 0.30799878224701E+03 0.30999971697570E+03
 0.30715000000008E+03 0.30715000000153E+03 0.30798836490563E+03 0.30999809262646E+03 0.30715000000008E+03
 0.30715000000153E+03 0.30954637308478E+03 0.30715000002064E+03 0.46906814077186E+01 -.10757483432370E+02
 0.48345576334948E+02 0.12112810063662E+03 0.72540796420001E+02 0.45398708569625E+02 0.41367208678794E+02
 0.47228884036048E+02 0.94922341916434E+02 0.44802807259749E+02 0.41204419181558E+02 0.46656345004232E+02
 0.94769416775078E+02 0.45398708569625E+02 0.41367208678793E+02 0.47228884036048E+02 0.94922341916432E+02
 0.44802807259748E+02 0.41204419181558E+02 0.46656345004231E+02 0.94769416775078E+02 0.50501959445634E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33632868195250E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.38795356826998E-02 0.20371971757433E-01 0.00000000000000E+00 0.38795356826998E-02 0.20371971757433E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30191554818333E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16786921310710E-05 0.30191554818333E-01 0.16786921310710E-05 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10071776371077E-02 0.67910920607720E-01 0.35255640763806E-01
 0.00000000000000E+00 0.67910920607720E-01 0.36262818400914E-01 0.41969907953645E+00 0.94699076556215E-01
 0.32500000298023E+00 0.42250006586313E+00
    355.05391730
 0.18129429916987E+01 0.31862975744731E+03 0.50316720149436E+03 0.39164857953067E+03 0.37271799927997E+03
 0.22999999989075E+00 0.00000000000000E+00 0.32597483517957E-01 0.00000000000000E+00 -.52579811493588E+01
 0.96392146105665E-03 0.24209644671806E+01 0.80000000000000E+04 0.30000000000000E+04 0.33044681607065E+01
 0.12391755602649E+01 0.33017161938779E+03 0.30715000009619E+03 0.33119814472428E+03 0.38377111824854E+03
 0.30715000000633E+03 0.30715000002693E+03 0.32989589306619E+03 0.38366107953111E+03 0.30715000000539E+03
 0.30715000002688E+03 0.33119814472428E+03 0.38377111824854E+03 0.30715000000633E+03 0.30715000002693E+03
 0.32989589306618E+03 0.38366107953110E+03 0.30715000000539E+03 0.30715000002688E+03 0.44131121212187E+03
 0.31475408375879E+03 0.15164228204151E+04 0.14716734492737E+04 0.12730727941338E+04 0.19013976066711E+04
 0.62195944856660E+03 0.10709090476396E+04 0.21153094617020E+04 0.10312494989103E+04 0.30611846589624E+04
 0.10202285869125E+04 0.21140531572572E+04 0.98585801045191E+03 0.30610038076989E+04 0.10709090476396E+04
 0.21153094617020E+04 0.10312494989103E+04 0.30611846589624E+04 0.10202285869125E+04 0.21140531572572E+04
 0.98585801045193E+03 0.30610038076990E+04 0.19036159554689E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.80063891099777E+05 0.50050000000000E+08 0.68605383538483E+03 0.12231349636003E+01
 0.12000000000000E+01 0.12404113642535E+01 0.46136401237307E+00 0.30929001765054E+03 0.32055172769156E+03
 0.31708789587109E+03 0.31700153230192E+03 0.23000000000000E+00 0.00000000000000E+00 0.20030940210717E+00
 0.00000000000000E+00 -.80544399108404E-01 0.99274767260741E-03 0.57331146445899E+00 0.80000000000000E+04
 0.30000000000000E+04 0.13954020625681E+02 0.52327577346303E+01 0.31475463902280E+03 0.44134470733610E+03
 0.30804435950680E+03 0.31004141294649E+03 0.30715000000013E+03 0.30715000000235E+03 0.30803336846149E+03
 0.31003959984995E+03 0.30715000000013E+03 0.30715000000235E+03 0.30804435950680E+03 0.31004141294649E+03
 0.30715000000013E+03 0.30715000000235E+03 0.30803336846149E+03 0.31003959984995E+03 0.30715000000013E+03
 0.30715000000235E+03 0.30958357272807E+03 0.30715000003034E+03 0.22175096804617E+01 -.14699927783545E+02
 0.50099889293975E+02 0.12299517782444E+03 0.72644789083996E+02 0.47493030539008E+02 0.42894957513299E+02
 0.49776033805099E+02 0.96456821894444E+02 0.46861326219446E+02 0.42722493074190E+02 0.49170245314790E+02
 0.96295370175117E+02 0.47493030539008E+02 0.42894957513298E+02 0.49776033805100E+02 0.96456821894441E+02
 0.46861326219444E+02 0.42722493074190E+02 0.49170245314788E+02 0.96295370175116E+02 0.50584765153461E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33655090329272E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37714425447905E-02 0.20616955016456E-01 0.00000000000000E+00 0.37714425447905E-02 0.20616955016456E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30480736183906E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.59309106359928E-06 0.30480736183906E-01 0.59309106359928E-06 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10071381857036E-02 0.68507800649363E-01 0.35369539025607E-01
 0.00000000000000E+00 0.68507800649363E-01 0.36376677211310E-01 0.41818200618653E+00 0.93182003206301E-01
 0.32500000298023E+00 0.42250006586313E+00
    360.28257001
 0.18129896709614E+01 0.31888722404286E+03 0.50346811981913E+03 0.39192036731940E+03 0.37299373859623E+03
 0.22999999989013E+00 0.00000000000000E+00 0.29820542054445E-01 0.00000000000000E+00 -.52803579218916E+01
 0.96314298707023E-03 0.24532534330224E+01 0.80000000000000E+04 0.30000000000000E+04 0.32609757688769E+01
 0.12228659133288E+01 0.33043982435299E+03 0.30715000011651E+03 0.33147029309773E+03 0.38443877413918E+03
 0.30715000000784E+03 0.30715000003318E+03 0.33015901965616E+03 0.38433086309916E+03 0.30715000000668E+03
 0.30715000003311E+03 0.33147029309773E+03 0.38443877413918E+03 0.30715000000784E+03 0.30715000003318E+03
 0.33015901965616E+03 0.38433086309915E+03 0.30715000000668E+03 0.30715000003311E+03 0.44205535123190E+03
 0.31506464844482E+03 0.15208500081066E+04 0.14760704775094E+04 0.12702425313200E+04 0.18931298484094E+04
 0.61653610443279E+03 0.10740398194229E+04 0.21176022776901E+04 0.10343392033098E+04 0.30596430440872E+04
 0.10232617842168E+04 0.21164467798281E+04 0.98888552255517E+03 0.30595409471337E+04 0.10740398194229E+04
 0.21176022776901E+04 0.10343392033098E+04 0.30596430440872E+04 0.10232617842168E+04 0.21164467798281E+04
 0.98888552255519E+03 0.30595409471338E+04 0.19034206563870E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.80352643769454E+05 0.50050000000000E+08 0.68638060325688E+03 0.12228710044755E+01
 0.12000000000000E+01 0.12613259750984E+01 0.45993299764787E+00 0.30941852971682E+03 0.32057848096226E+03
 0.31715659440898E+03 0.31707195451547E+03 0.23000000000000E+00 0.00000000000000E+00 0.19958265803270E+00
 0.00000000000000E+00 -.82900354022015E-01 0.99233532173306E-03 0.58722845486877E+00 0.80000000000000E+04
 0.30000000000000E+04 0.13623318035206E+02 0.51087442632023E+01 0.31506526818220E+03 0.44208774406613E+03
 0.30806816545434E+03 0.31006322655189E+03 0.30715000000016E+03 0.30715000000288E+03 0.30805687501127E+03
 0.31006131874457E+03 0.30715000000016E+03 0.30715000000288E+03 0.30806816545434E+03 0.31006322655189E+03
 0.30715000000016E+03 0.30715000000288E+03 0.30805687501127E+03 0.31006131874457E+03 0.30715000000016E+03
 0.30715000000288E+03 0.30960293747802E+03 0.30715000003652E+03 0.93901214106339E+00 -.16727299424729E+02
 0.50982761842597E+02 0.12398110629733E+03 0.72743430645524E+02 0.48568307480455E+02 0.43667751924082E+02
 0.51101471082322E+02 0.97268913165561E+02 0.47918186731792E+02 0.43490672798196E+02 0.50478608083676E+02
 0.97103424712794E+02 0.48568307480455E+02 0.43667751924081E+02 0.51101471082322E+02 0.97268913165558E+02
 0.47918186731790E+02 0.43490672798196E+02 0.50478608083673E+02 0.97103424712794E+02 0.50638114046871E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33666954762368E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37147881327011E-02 0.20746040460380E-01 0.00000000000000E+00 0.37147881327011E-02 0.20746040460380E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30631807412117E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.26119551098030E-06 0.30631807412117E-01 0.26119551098030E-06 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10024521789594E-02 0.68814688392974E-01 0.35435479291851E-01
 0.00000000000000E+00 0.68814688392974E-01 0.36437931470810E-01 0.41746649882393E+00 0.92466495843702E-01
 0.32500000298023E+00 0.42250006586313E+00
    370.73987543
 0.18130879373551E+01 0.31940115030165E+03 0.50406611981002E+03 0.39246151025413E+03 0.37354326436061E+03
 0.22999999989142E+00 0.00000000000000E+00 0.24347084599585E-01 0.00000000000000E+00 -.53251698255019E+01
 0.96159283513246E-03 0.25167673460779E+01 0.80000000000000E+04 0.30000000000000E+04 0.31786807836915E+01
 0.11920052938843E+01 0.33097254744879E+03 0.30715000016867E+03 0.33201127786277E+03 0.38575462129935E+03
 0.30715000001182E+03 0.30715000004963E+03 0.33068192533572E+03 0.38565078386670E+03 0.30715000001010E+03
 0.30715000004953E+03 0.33201127786277E+03 0.38575462129935E+03 0.30715000001182E+03 0.30715000004963E+03
 0.33068192533572E+03 0.38565078386670E+03 0.30715000001010E+03 0.30715000004953E+03 0.44350859142012E+03
 0.31570120084867E+03 0.15295798891107E+04 0.14847537488548E+04 0.12645660915119E+04 0.18769833849043E+04
 0.60609446293484E+03 0.10802286378076E+04 0.21218776901871E+04 0.10404553647282E+04 0.30564221759220E+04
 0.10292481766216E+04 0.21209057454345E+04 0.99486980651957E+03 0.30564614901004E+04 0.10802286378076E+04
 0.21218776901871E+04 0.10404553647282E+04 0.30564221759220E+04 0.10292481766216E+04 0.21209057454345E+04
 0.99486980651959E+03 0.30564614901005E+04 0.19030368791000E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.80960508630302E+05 0.50050000000000E+08 0.68703322877263E+03 0.12223158922426E+01
 0.12000000000000E+01 0.13031551967883E+01 0.45723744231638E+00 0.30969034525667E+03 0.32064503203318E+03
 0.31730577005780E+03 0.31722453206025E+03 0.23000000000000E+00 0.00000000000000E+00 0.19809683484283E+00
 0.00000000000000E+00 -.87816518152200E-01 0.99146428836871E-03 0.61564536832652E+00 0.80000000000000E+04
 0.30000000000000E+04 0.12994493927155E+02 0.48729352226831E+01 0.31570195007090E+03 0.44353884536650E+03
 0.30811785311327E+03 0.31010874324360E+03 0.30715000000024E+03 0.30715000000430E+03 0.30810593903033E+03
 0.31010664608896E+03 0.30715000000024E+03 0.30715000000430E+03 0.30811785311327E+03 0.31010874324360E+03
 0.30715000000024E+03 0.30715000000430E+03 0.30810593903033E+03 0.31010664608896E+03 0.30715000000024E+03
 0.30715000000430E+03 0.30964316187664E+03 0.30715000005221E+03 -.16970564830397E+01 -.20885699987386E+02
 0.52758811251196E+02 0.12605437444649E+03 0.73031769139039E+02 0.50773524901512E+02 0.45230926252258E+02
 0.53855939809052E+02 0.98980632812668E+02 0.50085568310814E+02 0.45045098988544E+02 0.53198058761804E+02
 0.98807555770895E+02 0.50773524901512E+02 0.45230926252256E+02 0.53855939809052E+02 0.98980632812664E+02
 0.50085568310813E+02 0.45045098988544E+02 0.53198058761802E+02 0.98807555770895E+02 0.50769628982636E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33692126512211E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35960585389490E-02 0.21018288703238E-01 0.00000000000000E+00 0.35960585389490E-02 0.21018288703238E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30945780837289E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.40216123804394E-10 0.30945780837289E-01 0.40216123804394E-10 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98366440524184E-03 0.69445806664960E-01 0.35585168401805E-01
 0.00000000000000E+00 0.69445806664960E-01 0.36568832807047E-01 0.41611872115819E+00 0.91118718177960E-01
 0.32500000298023E+00 0.42250006586313E+00
    381.19718086
 0.18131926150728E+01 0.31991348859025E+03 0.50465817373211E+03 0.39299894080772E+03 0.37408980886839E+03
 0.22999999989280E+00 0.00000000000000E+00 0.18981880869614E-01 0.00000000000000E+00 -.53699553072704E+01
 0.96005242935085E-03 0.25788608203853E+01 0.80000000000000E+04 0.30000000000000E+04 0.31021449225805E+01
 0.11633043459677E+01 0.33150058895797E+03 0.30715000024018E+03 0.33254805772179E+03 0.38704547873932E+03
 0.30715000001751E+03 0.30715000007293E+03 0.33120057748202E+03 0.38694548170113E+03 0.30715000001499E+03
 0.30715000007277E+03 0.33254805772179E+03 0.38704547873932E+03 0.30715000001751E+03 0.30715000007293E+03
 0.33120057748202E+03 0.38694548170113E+03 0.30715000001499E+03 0.30715000007277E+03 0.44491752716387E+03
 0.31635715029353E+03 0.15381514580827E+04 0.14932944726711E+04 0.12588713901305E+04 0.18613162830384E+04
 0.59615053595730E+03 0.10863254579045E+04 0.21257610767096E+04 0.10464902524076E+04 0.30530212972942E+04
 0.10351330281495E+04 0.21249511096214E+04 0.10007630458594E+04 0.30531829524181E+04 0.10863254579045E+04
 0.21257610767096E+04 0.10464902524077E+04 0.30530212972942E+04 0.10351330281495E+04 0.21249511096215E+04
 0.10007630458594E+04 0.30531829524181E+04 0.19026562748682E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.81608033220412E+05 0.50050000000000E+08 0.68768376563389E+03 0.12217252688505E+01
 0.12000000000000E+01 0.13449844184782E+01 0.45475737699152E+00 0.30998156743230E+03 0.32072848742226E+03
 0.31747032665868E+03 0.31739243238384E+03 0.23000000000000E+00 0.00000000000000E+00 0.19656807535311E+00
 0.00000000000000E+00 -.93003939536677E-01 0.99053276328501E-03 0.64483321281766E+00 0.80000000000000E+04
 0.30000000000000E+04 0.12406308857826E+02 0.46523658216847E+01 0.31635802972815E+03 0.44494572985655E+03
 0.30817036595456E+03 0.31015674394750E+03 0.30715000000036E+03 0.30715000000629E+03 0.30815779539899E+03
 0.31015445852215E+03 0.30715000000036E+03 0.30715000000629E+03 0.30817036595456E+03 0.31015674394750E+03
 0.30715000000036E+03 0.30715000000629E+03 0.30815779539899E+03 0.31015445852215E+03 0.30715000000036E+03
 0.30715000000629E+03 0.30968534837285E+03 0.30715000007342E+03 -.44325017208303E+01 -.25171431665736E+02
 0.54547137084423E+02 0.12825804947298E+03 0.73438176703131E+02 0.53049472415805E+02 0.46817054791096E+02
 0.56747829469287E+02 0.10080576929227E+03 0.52322380657408E+02 0.46623161238373E+02 0.56053813821952E+02
 0.10062578521115E+03 0.53049472415805E+02 0.46817054791094E+02 0.56747829469287E+02 0.10080576929226E+03
 0.52322380657406E+02 0.46623161238374E+02 0.56053813821949E+02 0.10062578521115E+03 0.50935085570035E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33719143796990E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34721825333576E-02 0.21305509384773E-01 0.00000000000000E+00 0.34721825333576E-02 0.21305509384773E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.31274422353007E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.31274422353007E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95248466273307E-03 0.70097146134267E-01 0.35760907194681E-01
 0.00000000000000E+00 0.70097146134267E-01 0.36713391857414E-01 0.41487868849576E+00 0.89878685515529E-01
 0.32500000298023E+00 0.42250006586313E+00
    391.65448628
 0.18133024688308E+01 0.32042398011639E+03 0.50524351186680E+03 0.39353227412974E+03 0.37463305537677E+03
 0.22999999989413E+00 0.00000000000000E+00 0.13726054281643E-01 0.00000000000000E+00 -.54146077351222E+01
 0.95852247523730E-03 0.26395320631433E+01 0.80000000000000E+04 0.30000000000000E+04 0.30308402431274E+01
 0.11365650911728E+01 0.33202419410804E+03 0.30715000033684E+03 0.33308084604762E+03 0.38831244390113E+03
 0.30715000002551E+03 0.30715000010543E+03 0.33171519315538E+03 0.38821606638497E+03 0.30715000002188E+03
 0.30715000010519E+03 0.33308084604762E+03 0.38831244390113E+03 0.30715000002551E+03 0.30715000010543E+03
 0.33171519315538E+03 0.38821606638496E+03 0.30715000002188E+03 0.30715000010519E+03 0.44628472929362E+03
 0.31703111589115E+03 0.15465630470396E+04 0.15016883375001E+04 0.12531646260539E+04 0.18460911630471E+04
 0.58666071386288E+03 0.10923253942551E+04 0.21292772686662E+04 0.10524370002392E+04 0.30494462267639E+04
 0.10409142854817E+04 0.21286102723686E+04 0.10065612493879E+04 0.30497135762596E+04 0.10923253942551E+04
 0.21292772686662E+04 0.10524370002392E+04 0.30494462267639E+04 0.10409142854817E+04 0.21286102723687E+04
 0.10065612493879E+04 0.30497135762597E+04 0.19022689160993E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.82287576214236E+05 0.50050000000000E+08 0.68833077074878E+03 0.12211059856145E+01
 0.12000000000000E+01 0.13868136401681E+01 0.45248712681717E+00 0.31029164692872E+03 0.32082802341030E+03
 0.31764964026282E+03 0.31757503497438E+03 0.23000000000000E+00 0.00000000000000E+00 0.19499687480150E+00
 0.00000000000000E+00 -.98455124397263E-01 0.98954284345713E-03 0.67477946100756E+00 0.80000000000000E+04
 0.30000000000000E+04 0.11855725407016E+02 0.44458970276310E+01 0.31703212648685E+03 0.44631096765057E+03
 0.30822576718515E+03 0.31020718732595E+03 0.30715000000054E+03 0.30715000000905E+03 0.30821250757814E+03
 0.31020471580836E+03 0.30715000000053E+03 0.30715000000905E+03 0.30822576718515E+03 0.31020718732595E+03
 0.30715000000054E+03 0.30715000000905E+03 0.30821250757814E+03 0.31020471580836E+03 0.30715000000053E+03
 0.30715000000905E+03 0.30972945849977E+03 0.30715000010166E+03 -.72612435793795E+01 -.29573299244484E+02
 0.56345150718148E+02 0.13058319650819E+03 0.73956320036448E+02 0.55392875462955E+02 0.48424485742969E+02
 0.59774896280877E+02 0.10273755767364E+03 0.54625404739127E+02 0.48223246889116E+02 0.59043689187884E+02
 0.10255138122147E+03 0.55392875462955E+02 0.48424485742968E+02 0.59774896280877E+02 0.10273755767364E+03
 0.54625404739126E+02 0.48223246889116E+02 0.59043689187882E+02 0.10255138122147E+03 0.51133561680419E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33747786517883E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.33453786557865E-02 0.21603529865171E-01 0.00000000000000E+00 0.33453786557865E-02 0.21603529865171E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.31613234100815E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.31613234100815E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.90920504990341E-03 0.70765726539391E-01 0.35963467994820E-01
 0.00000000000000E+00 0.70765726539391E-01 0.36872673044724E-01 0.41374356340859E+00 0.88743560428353E-01
 0.32500000298023E+00 0.42250006586313E+00
    400.06219238
 0.18142566261437E+01 0.32082543901671E+03 0.50417528089400E+03 0.39329405905124E+03 0.37468093615272E+03
 0.22999999989443E+00 0.00000000000000E+00 0.97552556815954E-02 0.35702522788645E-04 -.56134189395149E+01
 0.95732116764530E-03 0.26932783203525E+01 0.80000000000000E+04 0.30000000000000E+04 0.29703577010760E+01
 0.11138841379035E+01 0.33243430952081E+03 0.30715000043036E+03 0.33350291821619E+03 0.38921924528366E+03
 0.30715000003342E+03 0.30715000013744E+03 0.33211993073225E+03 0.38912624107395E+03 0.30715000002870E+03
 0.30715000013713E+03 0.33350291821619E+03 0.38921924528366E+03 0.30715000003342E+03 0.30715000013744E+03
 0.33211993073225E+03 0.38912624107394E+03 0.30715000002870E+03 0.30715000013713E+03 0.44710203835798E+03
 0.31756668396829E+03 0.15407111069900E+04 0.14958309479617E+04 0.12131077797768E+04 0.17804385653377E+04
 0.56126524666198E+03 0.10912167939821E+04 0.20930252614339E+04 0.10512750025023E+04 0.29909005883292E+04
 0.10379461641370E+04 0.20927219581633E+04 0.10036072363184E+04 0.29914922878382E+04 0.10912167939821E+04
 0.20930252614339E+04 0.10512750025023E+04 0.29909005883292E+04 0.10379461641371E+04 0.20927219581633E+04
 0.10036072363184E+04 0.29914922878382E+04 0.18701869253740E+03 0.00000000000000E+00 0.40000000000000E-02
 0.18564952868163E+06 0.88189886049063E+05 0.50050000000000E+08 0.67394237570148E+03 0.11559086087494E+01
 0.11559086087494E+01 0.14197176968466E+01 0.45209831275553E+00 0.31054884648434E+03 0.32070794166519E+03
 0.31764600180493E+03 0.31757670870751E+03 0.23000000000000E+00 0.00000000000000E+00 0.19391994437203E+00
 0.00000000000000E+00 -.11981282329928E+00 0.98872307658133E-03 0.69569578462686E+00 0.80000000000000E+04
 0.30000000000000E+04 0.11499279105580E+02 0.43122296645926E+01 0.31756785091505E+03 0.44712652565930E+03
 0.30827446374121E+03 0.31024178141295E+03 0.30715000000071E+03 0.30715000001176E+03 0.30826079305422E+03
 0.31023915410535E+03 0.30715000000070E+03 0.30715000001177E+03 0.30827446374121E+03 0.31024178141295E+03
 0.30715000000071E+03 0.30715000001176E+03 0.30826055078543E+03 0.31023915410535E+03 0.30715000000070E+03
 0.30715000001177E+03 0.30976158003268E+03 0.30715000012875E+03 -.10273951082760E+02 -.33806273129690E+02
 0.56826093065400E+02 0.12772281789955E+03 0.70612594368828E+02 0.56723549769159E+02 0.48752973351061E+02
 0.61682068362101E+02 0.99766023148108E+02 0.56041440578830E+02 0.48545544246126E+02 0.61038405247091E+02
 0.99573962994349E+02 0.56723549769159E+02 0.48752973351059E+02 0.61682068362101E+02 0.99766023148103E+02
 0.55893522074448E+02 0.48545544246125E+02 0.60891168686963E+02 0.99573962994348E+02 0.48984195999424E+01
 0.00000000000000E+00 0.70618090542696E-06 0.35017741388287E+02 0.35017741388287E+02 0.49587493968158E+08
 0.32860412180241E+03 0.00000000000000E+00 0.00000000000000E+00 0.12012177428528E-05 0.00000000000000E+00
 0.16660326158093E-03 0.31759321764338E-01 0.00000000000000E+00 0.16660326158093E-03 0.31759321764338E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19779579992357E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17174470204243E-02 0.19779579992357E-01 0.17174470204243E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16016679614261E-02 0.60582191126605E-01 0.45087064901011E-01
 0.00000000000000E+00 0.60582191126605E-01 0.46688732862438E-01 0.41354915637777E+00 0.88549153397534E-01
 0.32500000298023E+00 0.42250006586313E+00
    410.41193082
 0.18175675025999E+01 0.32128326624252E+03 0.49759024918367E+03 0.39077363495858E+03 0.37343486369007E+03
 0.22999999989351E+00 0.00000000000000E+00 0.63623468388469E-02 0.26723761291736E-03 -.55776267545080E+01
 0.95595732712282E-03 0.27705986379101E+01 0.80000000000000E+04 0.30000000000000E+04 0.28874626192824E+01
 0.10827984822309E+01 0.33279606512583E+03 0.30715000057728E+03 0.33391513763303E+03 0.38954681935443E+03
 0.30715000004622E+03 0.30715000018894E+03 0.33248891768558E+03 0.38945905398651E+03 0.30715000003976E+03
 0.30715000018851E+03 0.33391513763303E+03 0.38954681935443E+03 0.30715000004622E+03 0.30715000018894E+03
 0.33248891768558E+03 0.38945905398650E+03 0.30715000003976E+03 0.30715000018851E+03 0.44637688414630E+03
 0.31822923287590E+03 0.14862272455454E+04 0.14418831066232E+04 0.10639602988469E+04 0.15618301281710E+04
 0.49255002782983E+03 0.10639372384964E+04 0.19256070166324E+04 0.10242219486843E+04 0.27582580591105E+04
 0.10046902318458E+04 0.19257929507949E+04 0.97073736889521E+03 0.27592763159121E+04 0.10639372384964E+04
 0.19256070166324E+04 0.10242219486843E+04 0.27582580591105E+04 0.10046902318458E+04 0.19257929507949E+04
 0.97073736889521E+03 0.27592763159121E+04 0.17378198501533E+03 0.00000000000000E+00 0.40000000000000E-02
 0.17045650895371E+06 0.10867059547126E+06 0.50050000000000E+08 0.65451704732342E+03 0.10696657839453E+01
 0.10696657839453E+01 0.14563611979796E+01 0.45436211900548E+00 0.31086038327817E+03 0.32010162564225E+03
 0.31730237866706E+03 0.31724488725163E+03 0.23000000000000E+00 0.00000000000000E+00 0.19316772437950E+00
 0.00000000000000E+00 -.12264098033918E+00 0.98773216155240E-03 0.71146239819134E+00 0.80000000000000E+04
 0.30000000000000E+04 0.11244445272635E+02 0.42166669772380E+01 0.31823063071200E+03 0.44639248894602E+03
 0.30832700526767E+03 0.31025806355838E+03 0.30715000000099E+03 0.30715000001610E+03 0.30831488112908E+03
 0.31025523827673E+03 0.30715000000098E+03 0.30715000001611E+03 0.30832700526767E+03 0.31025806355838E+03
 0.30715000000099E+03 0.30715000001610E+03 0.30831168099087E+03 0.31025523827673E+03 0.30715000000098E+03
 0.30715000001611E+03 0.30977904290881E+03 0.30715000017081E+03 -.15611200458065E+02 -.40703947809001E+02
 0.55299513725083E+02 0.12120183851221E+03 0.65625827218496E+02 0.57134538350944E+02 0.46983147469489E+02
 0.62826033504815E+02 0.94251799292673E+02 0.57073812979398E+02 0.46765323342171E+02 0.62800461547321E+02
 0.94050244807350E+02 0.57134538350944E+02 0.46983147469487E+02 0.62826033504815E+02 0.94251799292670E+02
 0.56063142220978E+02 0.46765323342172E+02 0.61799077716979E+02 0.94050244807351E+02 0.44703308659760E+01
 0.00000000000000E+00 0.48919292114596E-05 0.24462133184341E+03 0.24462133184341E+03 0.50005084143567E+08
 0.32773361350932E+03 0.00000000000000E+00 0.00000000000000E+00 0.28900523900227E-04 0.00000000000000E+00
 0.00000000000000E+00 0.34028418115070E-01 0.00000000000000E+00 0.00000000000000E+00 0.34028418115070E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18305541491917E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.28032840235632E-02 0.18305541491917E-01 0.28032840235632E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.21363758373167E-02 0.58670537328355E-01 0.44384620210072E-01
 0.00000000000000E+00 0.58670537328355E-01 0.46520996047388E-01 0.41468105950274E+00 0.89681056522509E-01
 0.32500000298023E+00 0.42250006586313E+00
    420.35780017
 0.18188692690022E+01 0.32171141382753E+03 0.49094312616430E+03 0.38833967252765E+03 0.37222831283553E+03
 0.22999999989434E+00 0.00000000000000E+00 0.42603842103163E-02 0.61395674617885E-03 -.54886900020712E+01
 0.95468593559755E-03 0.28343364684851E+01 0.80000000000000E+04 0.30000000000000E+04 0.28225301014724E+01
 0.10584487880521E+01 0.33300117175641E+03 0.30715000075964E+03 0.33416863055472E+03 0.38936314272251E+03
 0.30715000006261E+03 0.30715000025451E+03 0.33270468582555E+03 0.38928001829644E+03 0.30715000005394E+03
 0.30715000025393E+03 0.33416863055472E+03 0.38936314272251E+03 0.30715000006261E+03 0.30715000025451E+03
 0.33270468582555E+03 0.38928001829644E+03 0.30715000005394E+03 0.30715000025393E+03 0.44474984237940E+03
 0.31887678748875E+03 0.14146241919839E+04 0.13714562884296E+04 0.93490447158848E+03 0.13750700209490E+04
 0.43549102700262E+03 0.10185369255630E+04 0.17682964012725E+04 0.97957169371957E+03 0.25412119147905E+04
 0.95819966949575E+03 0.17687244213906E+04 0.92511459645124E+03 0.25424173309029E+04 0.10185369255630E+04
 0.17682964012725E+04 0.97957169371957E+03 0.25412119147906E+04 0.95819966949575E+03 0.17687244213906E+04
 0.92511459645124E+03 0.25424173309030E+04 0.16079994547215E+03 0.00000000000000E+00 0.40000000000000E-02
 0.15766022017874E+06 0.11672317604163E+06 0.50050000000000E+08 0.63619194228162E+03 0.10017541130735E+01
 0.10017541130735E+01 0.14888374954979E+01 0.45565856924137E+00 0.31117609516032E+03 0.31967256272787E+03
 0.31709157055758E+03 0.31704290935979E+03 0.23000000000000E+00 0.00000000000000E+00 0.19246185897387E+00
 0.00000000000000E+00 -.12370698032827E+00 0.98673001003445E-03 0.72599546297364E+00 0.80000000000000E+04
 0.30000000000000E+04 0.11019352610321E+02 0.41322572288705E+01 0.31887838289582E+03 0.44475455008430E+03
 0.30837700477498E+03 0.31026841759566E+03 0.30715000000136E+03 0.30715000002160E+03 0.30836884324150E+03
 0.31026539075144E+03 0.30715000000134E+03 0.30715000002161E+03 0.30837700477498E+03 0.31026841759566E+03
 0.30715000000136E+03 0.30715000002160E+03 0.30835946247868E+03 0.31026539075144E+03 0.30715000000134E+03
 0.30715000002161E+03 0.30979147285835E+03 0.30715000022235E+03 -.19917071151254E+02 -.46504385111319E+02
 0.54530212855980E+02 0.11735197748900E+03 0.62549113568739E+02 0.58062918367872E+02 0.45952425395480E+02
 0.64530217136404E+02 0.90890084774233E+02 0.58927559247830E+02 0.45722796747000E+02 0.65419202964841E+02
 0.90677773397766E+02 0.58062918367872E+02 0.45952425395480E+02 0.64530217136404E+02 0.90890084774231E+02
 0.56635175842689E+02 0.45722796747000E+02 0.63154827247917E+02 0.90677773397766E+02 0.41841624275808E+01
 0.00000000000000E+00 0.11284988363537E-04 0.56463439473990E+03 0.56463439473990E+03 0.50034114041648E+08
 0.32764747201595E+03 0.00000000000000E+00 0.00000000000000E+00 0.11044713701412E-03 0.00000000000000E+00
 0.00000000000000E+00 0.33242953769990E-01 0.00000000000000E+00 0.00000000000000E+00 0.33242953769990E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18380754725431E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27764330543085E-02 0.18380754725431E-01 0.27764330543085E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23446450750983E-02 0.58800654154246E-01 0.42610632047543E-01
 0.00000000000000E+00 0.58800654154246E-01 0.44955277122641E-01 0.41532928462068E+00 0.90329281640452E-01
 0.32500000298023E+00 0.42250006586313E+00
    430.10417562
 0.18191336006206E+01 0.32211595712788E+03 0.48467368161718E+03 0.38610227539754E+03 0.37110961833157E+03
 0.22999999991895E+00 0.00000000000000E+00 0.28936083932145E-02 0.10568740278704E-02 -.54037799935751E+01
 0.95348775107378E-03 0.28881718616356E+01 0.80000000000000E+04 0.30000000000000E+04 0.27699182677687E+01
 0.10387193504133E+01 0.33309536359995E+03 0.30715000098686E+03 0.33429952928420E+03 0.38890026076505E+03
 0.30715000008364E+03 0.30715000033822E+03 0.33280910405793E+03 0.38882115314723E+03 0.30715000007217E+03
 0.30715000033743E+03 0.33429952928420E+03 0.38890026076504E+03 0.30715000008364E+03 0.30715000033822E+03
 0.33280910405792E+03 0.38882115314723E+03 0.30715000007217E+03 0.30715000033743E+03 0.44272199746655E+03
 0.31952745753661E+03 0.13406708666736E+04 0.12991069223580E+04 0.82682786901854E+03 0.12192421921419E+04
 0.38828018377830E+03 0.96832667227070E+03 0.16286684602473E+04 0.93050672548805E+03 0.23485495791318E+04
 0.90920506748082E+03 0.16292231163395E+04 0.87732164691156E+03 0.23498343161435E+04 0.96832667227072E+03
 0.16286684602474E+04 0.93050672548807E+03 0.23485495791319E+04 0.90920506748083E+03 0.16292231163395E+04
 0.87732164691158E+03 0.23498343161434E+04 0.14899929616647E+03 0.00000000000000E+00 0.40000000000000E-02
 0.14612855165381E+06 0.11835830171955E+06 0.50050000000000E+08 0.61896444630844E+03 0.94320402417047E+00
 0.94320402417047E+00 0.15182565279322E+01 0.45614075532222E+00 0.31150120608351E+03 0.31941644953231E+03
 0.31700947278212E+03 0.31696723664237E+03 0.23000000000000E+00 0.00000000000000E+00 0.19176724517615E+00
 0.00000000000000E+00 -.12589507248163E+00 0.98570013726953E-03 0.73995403512774E+00 0.80000000000000E+04
 0.30000000000000E+04 0.10811482362710E+02 0.40543058860164E+01 0.31952917377561E+03 0.44272038403376E+03
 0.30842835560599E+03 0.31027956132356E+03 0.30715000000184E+03 0.30715000002858E+03 0.30842664872142E+03
 0.31027632329877E+03 0.30715000000181E+03 0.30715000002858E+03 0.30842835560599E+03 0.31027956132356E+03
 0.30715000000184E+03 0.30715000002858E+03 0.30840770353556E+03 0.31027632329877E+03 0.30715000000181E+03
 0.30715000002858E+03 0.30980458512986E+03 0.30715000028576E+03 -.23345619132062E+02 -.51416626928873E+02
 0.54508801117718E+02 0.11564649171528E+03 0.60865146591978E+02 0.59523942137285E+02 0.45648834489494E+02
 0.66814202427677E+02 0.89289323587580E+02 0.61560263437527E+02 0.45406259712605E+02 0.68855752723177E+02
 0.89065239166863E+02 0.59523942137285E+02 0.45648834489492E+02 0.66814202427677E+02 0.89289323587577E+02
 0.57644382946872E+02 0.45406259712605E+02 0.64997968129689E+02 0.89065239166865E+02 0.40136998901485E+01
 0.00000000000000E+00 0.19593799481605E-04 0.98051100068436E+03 0.98051100068436E+03 0.50041902368393E+08
 0.32789090820726E+03 0.00000000000000E+00 0.00000000000000E+00 0.26443819593218E-03 0.00000000000000E+00
 0.14451434419920E-05 0.32333178695501E-01 0.00000000000000E+00 0.14451434419920E-05 0.32333178695501E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18539389714295E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.26695506970395E-02 0.18539389714295E-01 0.26695506970395E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23959321459950E-02 0.59342731903237E-01 0.41279379308603E-01
 0.00000000000000E+00 0.59342731903237E-01 0.43675311454598E-01 0.41557037766111E+00 0.90570374680879E-01
 0.32500000298023E+00 0.42250006586313E+00
    441.06884800
 0.18188488724741E+01 0.32254865984759E+03 0.47811505756014E+03 0.38379780186870E+03 0.36993998626529E+03
 0.22999999991825E+00 0.00000000000000E+00 0.18822529336173E-02 0.16578156225014E-02 -.53135892582038E+01
 0.95220948412970E-03 0.29404141857792E+01 0.80000000000000E+04 0.30000000000000E+04 0.27207051437483E+01
 0.10202644289056E+01 0.33311188777840E+03 0.30715000131214E+03 0.33434535032413E+03 0.38817855070069E+03
 0.30715000011467E+03 0.30715000046105E+03 0.33283612438776E+03 0.38810338536672E+03 0.30715000009912E+03
 0.30715000045996E+03 0.33434535032413E+03 0.38817855070068E+03 0.30715000011467E+03 0.30715000046105E+03
 0.33283612438776E+03 0.38810338536672E+03 0.30715000009912E+03 0.30715000045996E+03 0.44021059241702E+03
 0.32028176292422E+03 0.12608908492977E+04 0.12214417588094E+04 0.72546110380666E+03 0.10732303559682E+04
 0.34414194664248E+03 0.91244748630226E+03 0.14909142100303E+04 0.87622037783946E+03 0.21581531165988E+04
 0.85584139833070E+03 0.14915405818933E+04 0.82555607990498E+03 0.21594634674812E+04 0.91244748630227E+03
 0.14909142100303E+04 0.87622037783949E+03 0.21581531165989E+04 0.85584139833071E+03 0.14915405818933E+04
 0.82555607990501E+03 0.21594634674812E+04 0.13720365559098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.13473003913903E+06 0.11659700542460E+06 0.50050000000000E+08 0.60140088651684E+03 0.93189992305122E+00
 0.93189992305122E+00 0.15488515340356E+01 0.45591223415451E+00 0.31188511332660E+03 0.31932230284501E+03
 0.31706183238556E+03 0.31702458271180E+03 0.23000000000000E+00 0.00000000000000E+00 0.19097855418045E+00
 0.00000000000000E+00 -.12966988620841E+00 0.98448676636252E-03 0.75537003131184E+00 0.80000000000000E+04
 0.30000000000000E+04 0.10590835839895E+02 0.39715634399606E+01 0.32028356910279E+03 0.44020540051811E+03
 0.30849073338574E+03 0.31029812243228E+03 0.30715000000255E+03 0.30715000003875E+03 0.30849943537944E+03
 0.31029462972089E+03 0.30715000000250E+03 0.30715000003876E+03 0.30849073338574E+03 0.31029812243228E+03
 0.30715000000255E+03 0.30715000003875E+03 0.30846541264502E+03 0.31029462972089E+03 0.30715000000250E+03
 0.30715000003876E+03 0.30982446523573E+03 0.30715000037535E+03 -.26317644062184E+02 -.56097171751312E+02
 0.55352739633801E+02 0.11609189196152E+03 0.60462388629548E+02 0.61840647767767E+02 0.46140263590931E+02
 0.70126114782697E+02 0.89426261977690E+02 0.65457820598678E+02 0.45881596955710E+02 0.73715857676096E+02
 0.89187598264258E+02 0.61840647767767E+02 0.46140263590929E+02 0.70126114782697E+02 0.89426261977686E+02
 0.59350017492990E+02 0.45881596955710E+02 0.67715418153119E+02 0.89187598264258E+02 0.39468343593905E+01
 0.00000000000000E+00 0.31136255569875E-04 0.15582230927698E+04 0.15582230927698E+04 0.50045294922277E+08
 0.32852232265179E+03 0.00000000000000E+00 0.00000000000000E+00 0.54758777079241E-03 0.00000000000000E+00
 0.51848358401871E-04 0.31253011131048E-01 0.00000000000000E+00 0.51848358401871E-04 0.31253011131048E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18781494846149E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24943380357571E-02 0.18781494846149E-01 0.24943380357571E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23203698535809E-02 0.60389977153325E-01 0.40267652211260E-01
 0.00000000000000E+00 0.60389977153325E-01 0.42588022064841E-01 0.41545611707726E+00 0.90456114097024E-01
 0.32500000298023E+00 0.42250006586313E+00
    450.58193440
 0.18183650103013E+01 0.32290290865950E+03 0.47291797178505E+03 0.38199059118317E+03 0.36900776141664E+03
 0.22999999992064E+00 0.00000000000000E+00 0.13005436938298E-02 0.22511231559704E-02 -.52408992948231E+01
 0.95116552113265E-03 0.29797580179199E+01 0.80000000000000E+04 0.30000000000000E+04 0.26847817681466E+01
 0.10067931630550E+01 0.33307768971423E+03 0.30715000166807E+03 0.33432840883057E+03 0.38746895519655E+03
 0.30715000014964E+03 0.30715000059872E+03 0.33281003599542E+03 0.38739678500941E+03 0.30715000012954E+03
 0.30715000059730E+03 0.33432840883057E+03 0.38746895519655E+03 0.30715000014964E+03 0.30715000059872E+03
 0.33281003599542E+03 0.38739678500941E+03 0.30715000012954E+03 0.30715000059730E+03 0.43799937925047E+03
 0.32095831923924E+03 0.11973259570853E+04 0.11598195952091E+04 0.65231559052201E+03 0.96776386112282E+03
 0.31218669264819E+03 0.86717229483511E+03 0.13873240147834E+04 0.83245208398392E+03 0.20145777512127E+04
 0.81309956379740E+03 0.13879784813719E+04 0.78428995859238E+03 0.20158814722079E+04 0.86717229483511E+03
 0.13873240147834E+04 0.83245208398393E+03 0.20145777512128E+04 0.81309956379742E+03 0.13879784813719E+04
 0.78428995859241E+03 0.20158814722079E+04 0.12825766259306E+03 0.00000000000000E+00 0.40000000000000E-02
 0.12619907826131E+06 0.11360388843921E+06 0.50050000000000E+08 0.58793231622218E+03 0.91634477887435E+00
 0.91634477887435E+00 0.15735081279177E+01 0.45519440333294E+00 0.31223320349594E+03 0.31940477040584E+03
 0.31722846565883E+03 0.31719388905579E+03 0.23000000000000E+00 0.00000000000000E+00 0.19028439709527E+00
 0.00000000000000E+00 -.13399513991049E+00 0.98338916966852E-03 0.76855171167772E+00 0.80000000000000E+04
 0.30000000000000E+04 0.10409188969909E+02 0.39034458637157E+01 0.32096017954895E+03 0.43799269648534E+03
 0.30854956471663E+03 0.31032227509012E+03 0.30715000000335E+03 0.30715000005008E+03 0.30856986248403E+03
 0.31031854773788E+03 0.30715000000330E+03 0.30715000005009E+03 0.30854956471663E+03 0.31032227509012E+03
 0.30715000000335E+03 0.30715000005008E+03 0.30851922739835E+03 0.31031854773788E+03 0.30715000000330E+03
 0.30715000005009E+03 0.30984847574093E+03 0.30715000047209E+03 -.28186411012126E+02 -.59498896510853E+02
 0.56830636249375E+02 0.11845871516321E+03 0.61343925732591E+02 0.64447420980586E+02 0.47279000126369E+02
 0.73652962999753E+02 0.91167784626032E+02 0.69622163694299E+02 0.47005093369895E+02 0.78762201670944E+02
 0.90915364270646E+02 0.64447420980586E+02 0.47279000126368E+02 0.73652962999753E+02 0.91167784626029E+02
 0.61352920073744E+02 0.47005093369895E+02 0.70656566147879E+02 0.90915364270646E+02 0.39923533843294E+01
 0.00000000000000E+00 0.42856844357418E-04 0.21448442728822E+04 0.21448442728822E+04 0.50046714942300E+08
 0.32935379145293E+03 0.00000000000000E+00 0.00000000000000E+00 0.90692452918830E-03 0.00000000000000E+00
 0.12472342364141E-03 0.30341326995766E-01 0.00000000000000E+00 0.12472342364141E-03 0.30341326995766E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19037982992514E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23106980067332E-02 0.19037982992514E-01 0.23106980067332E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.21656301652012E-02 0.61647163435346E-01 0.39786197985017E-01
 0.00000000000000E+00 0.61647163435346E-01 0.41951828150218E-01 0.41509720166647E+00 0.90097198686239E-01
 0.32500000298023E+00 0.42250006586313E+00
    461.59845843
 0.18176922843107E+01 0.32328846305650E+03 0.46748592972903E+03 0.38011705553309E+03 0.36802656135116E+03
 0.22999999990074E+00 0.00000000000000E+00 0.85032888014830E-03 0.30047580227750E-02 -.51638107467746E+01
 0.95003188239635E-03 0.30194493288426E+01 0.80000000000000E+04 0.30000000000000E+04 0.26494897342976E+01
 0.99355865036158E+00 0.33299937093624E+03 0.30715000218824E+03 0.33426281328673E+03 0.38659680042176E+03
 0.30715000020228E+03 0.30715000080495E+03 0.33274014227128E+03 0.38652768813343E+03 0.30715000017542E+03
 0.30715000080302E+03 0.33426281328673E+03 0.38659680042176E+03 0.30715000020228E+03 0.30715000080495E+03
 0.33274014227128E+03 0.38652768813343E+03 0.30715000017542E+03 0.30715000080302E+03 0.43547164911958E+03
 0.32176832643503E+03 0.11312826546742E+04 0.10960587654386E+04 0.58255232380859E+03 0.86708771205301E+03
 0.28162262662538E+03 0.81964961165030E+03 0.12842378394517E+04 0.78673552777021E+03 0.18713146058085E+04
 0.76854572924091E+03 0.12849026492058E+04 0.74147614137930E+03 0.18725936135520E+04 0.81964961165031E+03
 0.12842378394517E+04 0.78673552777022E+03 0.18713146058086E+04 0.76854572924093E+03 0.12849026492059E+04
 0.74147614137932E+03 0.18725936135521E+04 0.11931972771712E+03 0.00000000000000E+00 0.40000000000000E-02
 0.11773434760037E+06 0.10944248110488E+06 0.50050000000000E+08 0.57430367728530E+03 0.89430400324372E+00
 0.89430400324372E+00 0.16001426787005E+01 0.45393283090413E+00 0.31265273205074E+03 0.31968688420198E+03
 0.31755819580263E+03 0.31752502293059E+03 0.23000000000000E+00 0.00000000000000E+00 0.18946403271700E+00
 0.00000000000000E+00 -.14015316277636E+00 0.98206955321650E-03 0.78367185037749E+00 0.80000000000000E+04
 0.30000000000000E+04 0.10208354423023E+02 0.38281329086338E+01 0.32177023273451E+03 0.43546387184494E+03
 0.30862436114797E+03 0.31036236940613E+03 0.30715000000458E+03 0.30715000006693E+03 0.30866110761848E+03
 0.31035835373363E+03 0.30715000000451E+03 0.30715000006694E+03 0.30862436114797E+03 0.31036236940613E+03
 0.30715000000458E+03 0.30715000006693E+03 0.30858705973446E+03 0.31035835373363E+03 0.30715000000451E+03
 0.30715000006694E+03 0.30988641520807E+03 0.30715000061153E+03 -.29598416292416E+02 -.62754528056780E+02
 0.59357388272618E+02 0.12341112438978E+03 0.63756949175802E+02 0.68058262932269E+02 0.49391897089356E+02
 0.78388915406642E+02 0.95006117480570E+02 0.75209740290182E+02 0.49099885728317E+02 0.85418710496082E+02
 0.94737525467775E+02 0.68058262932269E+02 0.49391897089356E+02 0.78388915406642E+02 0.95006117480569E+02
 0.64191920163842E+02 0.49099885728316E+02 0.74646432510861E+02 0.94737525467773E+02 0.41588011182555E+01
 0.00000000000000E+00 0.58246133595003E-04 0.29150818230841E+04 0.29150818230841E+04 0.50047645108142E+08
 0.33062084057062E+03 0.00000000000000E+00 0.00000000000000E+00 0.14792416916188E-02 0.00000000000000E+00
 0.22861025004339E-03 0.29344672913149E-01 0.00000000000000E+00 0.22861025004339E-03 0.29344672913149E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19384633688809E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20754246587305E-02 0.19384633688809E-01 0.20754246587305E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18992975517705E-02 0.63458566504147E-01 0.39674747191032E-01
 0.00000000000000E+00 0.63458566504147E-01 0.41574044742803E-01 0.41446641545206E+00 0.89466412471831E-01
 0.32500000298023E+00 0.42250006586313E+00
    470.41167766
 0.18171347969186E+01 0.32357719984754E+03 0.46357192937289E+03 0.37877549790431E+03 0.36731388936885E+03
 0.22999999990393E+00 0.00000000000000E+00 0.60655000086047E-03 0.36489526776290E-02 -.51082060433883E+01
 0.94918466395521E-03 0.30473209365040E+01 0.80000000000000E+04 0.30000000000000E+04 0.26252567966070E+01
 0.98447129872764E+00 0.33291575187857E+03 0.30715000270113E+03 0.33418519206312E+03 0.38587803492759E+03
 0.30715000025561E+03 0.30715000101289E+03 0.33266269469697E+03 0.38581110786567E+03 0.30715000022197E+03
 0.30715000101043E+03 0.33418519206312E+03 0.38587803492759E+03 0.30715000025561E+03 0.30715000101289E+03
 0.33266269469697E+03 0.38581110786567E+03 0.30715000022197E+03 0.30715000101043E+03 0.43350288240096E+03
 0.32243510609455E+03 0.10842580182698E+04 0.10508371352075E+04 0.53655885729335E+03 0.80069665089227E+03
 0.26145499931245E+03 0.78559841828173E+03 0.12132872843088E+04 0.75413125550678E+03 0.17725039702163E+04
 0.73675568005351E+03 0.12139502563347E+04 0.71106392638035E+03 0.17737564328830E+04 0.78559841828174E+03
 0.12132872843088E+04 0.75413125550680E+03 0.17725039702163E+04 0.73675568005352E+03 0.12139502563347E+04
 0.71106392638037E+03 0.17737564328831E+04 0.11316241797388E+03 0.00000000000000E+00 0.40000000000000E-02
 0.11194124272802E+06 0.10599392679293E+06 0.50050000000000E+08 0.56480170517668E+03 0.87565475014886E+00
 0.87565475014886E+00 0.16202256867952E+01 0.45272004542195E+00 0.31300066556856E+03 0.32004990805863E+03
 0.31792235247177E+03 0.31788912421176E+03 0.23000000000000E+00 0.00000000000000E+00 0.18879216926246E+00
 0.00000000000000E+00 -.14593172326369E+00 0.98097781459920E-03 0.79570865968032E+00 0.80000000000000E+04
 0.30000000000000E+04 0.10053931049606E+02 0.37702241436021E+01 0.32243705494180E+03 0.43349393123334E+03
 0.30869007713047E+03 0.31040566806760E+03 0.30715000000584E+03 0.30715000008380E+03 0.30874236654838E+03
 0.31040141014075E+03 0.30715000000574E+03 0.30715000008383E+03 0.30869007713047E+03 0.31040566806760E+03
 0.30715000000584E+03 0.30715000008380E+03 0.30864629456937E+03 0.31040141014075E+03 0.30715000000574E+03
 0.30715000008383E+03 0.30992616777251E+03 0.30715000074726E+03 -.30220289965897E+02 -.64898351513248E+02
 0.61999541078384E+02 0.12902544250547E+03 0.66715903721691E+02 0.71404197693089E+02 0.51682085320001E+02
 0.82677597136867E+02 0.99433702531791E+02 0.80271284477523E+02 0.51375131140772E+02 0.91368121550027E+02
 0.99151876905382E+02 0.71404197693089E+02 0.51682085320000E+02 0.82677597136867E+02 0.99433702531789E+02
 0.66872040846622E+02 0.51375131140771E+02 0.78293772756668E+02 0.99151876905381E+02 0.43757824789616E+01
 0.00000000000000E+00 0.71810209055922E-04 0.35939654532062E+04 0.35939654532062E+04 0.50048112941815E+08
 0.33184277663362E+03 0.15438732307147E-01 0.15438732307147E-01 0.20665451611585E-02 0.00000000000000E+00
 0.31270754814094E-03 0.28642880418314E-01 0.00000000000000E+00 0.31270754814094E-03 0.28642880418314E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19679676718248E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18890164460741E-02 0.19679676718248E-01 0.18890164460741E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16239236330175E-02 0.65126028184384E-01 0.39924178734269E-01
 0.00000000000000E+00 0.65126028184384E-01 0.41548102367286E-01 0.41386002271098E+00 0.88860019730743E-01
 0.32500000298023E+00 0.42250006586313E+00
    481.42820169
 0.18164591048458E+01 0.32391400301256E+03 0.45917823212776E+03 0.37727758528234E+03 0.36650786095767E+03
 0.22999999990514E+00 0.00000000000000E+00 0.39852303094095E-03 0.44947619908968E-02 -.50457275093202E+01
 0.94819829420625E-03 0.30779995356785E+01 0.80000000000000E+04 0.30000000000000E+04 0.25990907104658E+01
 0.97465901642468E+00 0.33279772148669E+03 0.30715000348714E+03 0.33407098534507E+03 0.38498688437921E+03
 0.30715000033954E+03 0.30715000133870E+03 0.33255176004060E+03 0.38492241756264E+03 0.30715000029535E+03
 0.30715000133541E+03 0.33407098534507E+03 0.38498688437920E+03 0.30715000033954E+03 0.30715000133870E+03
 0.33255176004060E+03 0.38492241756264E+03 0.30715000029535E+03 0.30715000133541E+03 0.43115403910639E+03
 0.32329202336873E+03 0.10322301443442E+04 0.10009764578040E+04 0.48891481311904E+03 0.73189224428470E+03
 0.24053285710007E+03 0.74777088817509E+03 0.11368412565626E+04 0.71806108311187E+03 0.16658095795286E+04
 0.70153415529388E+03 0.11374948655205E+04 0.67750147670710E+03 0.16670252645489E+04 0.74777088817510E+03
 0.11368412565626E+04 0.71806108311189E+03 0.16658095795286E+04 0.70153415529389E+03 0.11374948655205E+04
 0.67750147670711E+03 0.16670252645489E+04 0.10652860748620E+03 0.00000000000000E+00 0.40000000000000E-02
 0.10574725037631E+06 0.10181417161812E+06 0.50050000000000E+08 0.55446279699195E+03 0.85255652472269E+00
 0.85255652472269E+00 0.16440281708078E+01 0.45110612733527E+00 0.31344963912769E+03 0.32066699027198E+03
 0.31849646272243E+03 0.31846175559403E+03 0.23000000000000E+00 0.00000000000000E+00 0.18792876877853E+00
 0.00000000000000E+00 -.15411228210160E+00 0.97957260975425E-03 0.81075713251729E+00 0.80000000000000E+04
 0.30000000000000E+04 0.98673199151035E+01 0.37002449681638E+01 0.32329402537026E+03 0.43114331328256E+03
 0.30877995801858E+03 0.31047557337962E+03 0.30715000000783E+03 0.30715000011006E+03 0.30885455966237E+03
 0.31047099964088E+03 0.30715000000770E+03 0.30715000011009E+03 0.30877995801858E+03 0.31047557337962E+03
 0.30715000000783E+03 0.30715000011006E+03 0.30872698879398E+03 0.31047099964088E+03 0.30715000000770E+03
 0.30715000011009E+03 0.30998904951150E+03 0.30715000095259E+03 -.30393311863800E+02 -.67039665071843E+02
 0.66047834693185E+02 0.13804457041557E+03 0.71666496548920E+02 0.76129771225727E+02 0.55268679657356E+02
 0.88629008214754E+02 0.10661085107014E+03 0.87268464694866E+02 0.54942806332002E+02 0.99509931168192E+02
 0.10631244275031E+03 0.76129771225727E+02 0.55268679657354E+02 0.88629008214754E+02 0.10661085107013E+03
 0.70716356687275E+02 0.54942806332001E+02 0.83399370919746E+02 0.10631244275031E+03 0.47479021995822E+01
 0.00000000000000E+00 0.90241727210545E-04 0.45164640213083E+04 0.45164640213083E+04 0.50048510383348E+08
 0.33361195266071E+03 0.40099663249091E-01 0.40099663249091E-01 0.29780723912035E-02 0.00000000000000E+00
 0.41355237035499E-03 0.27872773324901E-01 0.00000000000000E+00 0.41355237035499E-03 0.27872773324901E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20077086927697E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16588717419487E-02 0.20077086927697E-01 0.16588717419487E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11944886780942E-02 0.67445890227257E-01 0.40625603984044E-01
 0.00000000000000E+00 0.67445890227257E-01 0.41820092662138E-01 0.41305306366763E+00 0.88053060687403E-01
 0.32500000298023E+00 0.42250006586313E+00
    490.24142091
 0.18159529534597E+01 0.32416488517415E+03 0.45602909231321E+03 0.37620936017661E+03 0.36592635464650E+03
 0.22999999998652E+00 0.00000000000000E+00 0.28525093947873E-03 0.51971868922187E-02 -.50011432933039E+01
 0.94746486847927E-03 0.30996314574149E+01 0.80000000000000E+04 0.30000000000000E+04 0.25809519970068E+01
 0.96785699887754E+00 0.33269838174831E+03 0.30715000425225E+03 0.33397262078717E+03 0.38429315113300E+03
 0.30715000042336E+03 0.30715000166270E+03 0.33245768282230E+03 0.38423046869672E+03 0.30715000036875E+03
 0.30715000165858E+03 0.33397262078717E+03 0.38429315113300E+03 0.30715000042336E+03 0.30715000166270E+03
 0.33245768282230E+03 0.38423046869672E+03 0.30715000036875E+03 0.30715000165858E+03 0.42937785336758E+03
 0.32399583840367E+03 0.99552732386959E+03 0.96591571055108E+03 0.45732088416366E+03 0.68626050793698E+03
 0.22665301935251E+03 0.72101294944298E+03 0.10841036763958E+04 0.69264582641477E+03 0.15920567972215E+04
 0.67666269325580E+03 0.10847465982858E+04 0.65388877892992E+03 0.15932422190227E+04 0.72101294944299E+03
 0.10841036763958E+04 0.69264582641478E+03 0.15920567972215E+04 0.67666269325581E+03 0.10847465982858E+04
 0.65388877892993E+03 0.15932422190228E+04 0.10195805786347E+03 0.00000000000000E+00 0.40000000000000E-02
 0.10151184061235E+06 0.98683176099432E+05 0.50050000000000E+08 0.54727009520137E+03 0.83487862278705E+00
 0.83487862278705E+00 0.16621747901453E+01 0.44931950886316E+00 0.31381908551059E+03 0.32128117078685E+03
 0.31904593045923E+03 0.31900896448959E+03 0.22999999550617E+00 0.00000000000000E+00 0.18723766806042E+00
 0.00000000000000E+00 -.16143755103363E+00 0.97841932048491E-03 0.82245204263305E+00 0.80000000000000E+04
 0.30000000000000E+04 0.97270109201605E+01 0.36476290950602E+01 0.32399787814605E+03 0.42936550807578E+03
 0.30885833325180E+03 0.31054528497733E+03 0.30715000000984E+03 0.30715000013599E+03 0.30895295055628E+03
 0.31054044951181E+03 0.30715000000968E+03 0.30715000013603E+03 0.30885833325180E+03 0.31054528497733E+03
 0.30715000000984E+03 0.30715000013599E+03 0.30879718938451E+03 0.31054044951181E+03 0.30715000000968E+03
 0.30715000013603E+03 0.31005072594335E+03 0.30715000114992E+03 -.30078199627161E+02 -.68354992413425E+02
 0.69846782887387E+02 0.14672864801743E+03 0.76532631215605E+02 0.80323563722102E+02 0.58681686564025E+02
 0.93838555456485E+02 0.11363694121810E+03 0.93358918671739E+02 0.58340541720105E+02 0.10654121365596E+03
 0.11332530314079E+03 0.80323563722101E+02 0.58681686564022E+02 0.93838555456484E+02 0.11363694121809E+03
 0.74173918674946E+02 0.58340541720105E+02 0.87904925886249E+02 0.11332530314079E+03 0.51217063217969E+01
 0.00000000000000E+00 0.10611988856610E-03 0.53111663269834E+04 0.53111663269834E+04 0.50048736374946E+08
 0.33497132502930E+03 0.60228241872754E-01 0.60228241872754E-01 0.38601647084838E-02 0.00000000000000E+00
 0.48582030109570E-03 0.27345810037562E-01 0.00000000000000E+00 0.48582030109570E-03 0.27345810037562E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20418717041903E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14798147552697E-02 0.20418717041903E-01 0.14798147552697E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.78132429060569E-03 0.69470334162532E-01 0.41477391680554E-01
 0.00000000000000E+00 0.69470334162532E-01 0.42258715971159E-01 0.41215975443158E+00 0.87159751451346E-01
 0.32500000298023E+00 0.42250006586313E+00
    500.00000000
 0.18154375697409E+01 0.32442436783381E+03 0.45288657788464E+03 0.37514820374523E+03 0.36534324639259E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19727808378243E-03 0.59957944891241E-02 -.49569468887399E+01
 0.94670747558761E-03 0.31209334397615E+01 0.80000000000000E+04 0.30000000000000E+04 0.25633356668483E+01
 0.96125087506810E+00 0.33258616450340E+03 0.30715000526437E+03 0.33385993504197E+03 0.38354868329983E+03
 0.30715000053687E+03 0.30715000209979E+03 0.33235095962108E+03 0.38348781339519E+03 0.30715000046831E+03
 0.30715000209454E+03 0.33385993504197E+03 0.38354868329983E+03 0.30715000053687E+03 0.30715000209979E+03
 0.33235095962108E+03 0.38348781339519E+03 0.30715000046831E+03 0.30715000209454E+03 0.42751596954537E+03
 0.32479282439475E+03 0.95946549377199E+03 0.93157076071230E+03 0.42805074248037E+03 0.64402360488233E+03
 0.21383260868956E+03 0.69468234377348E+03 0.10332355084702E+04 0.66772834658172E+03 0.15208238215492E+04
 0.65220992175307E+03 0.10338648827813E+04 0.63075336201917E+03 0.15219761183409E+04 0.69468234377349E+03
 0.10332355084702E+04 0.66772834658173E+03 0.15208238215492E+04 0.65220992175308E+03 0.10338648827813E+04
 0.63075336201919E+03 0.15219761183409E+04 0.97562776081566E+02 0.00000000000000E+00 0.40000000000000E-02
 0.97460762788756E+05 0.95495070378079E+05 0.50050000000000E+08 0.54027925564233E+03 0.81652899818950E+00
 0.81652899818950E+00 0.16814781759959E+01 0.44670962756787E+00 0.31423816569198E+03 0.32207809581527E+03
 0.31974331430492E+03 0.31970271285963E+03 0.22999998305286E+00 0.00000000000000E+00 0.18647818571442E+00
 0.00000000000000E+00 -.17035187081413E+00 0.97711436808869E-03 0.83493424251101E+00 0.80000000000000E+04
 0.30000000000000E+04 0.95815928880106E+01 0.35930973330040E+01 0.32479489899210E+03 0.42750168140137E+03
 0.30895252986886E+03 0.31063831978526E+03 0.30715000001258E+03 0.30715000017074E+03 0.30907158968089E+03
 0.31063318497179E+03 0.30715000001237E+03 0.30715000017079E+03 0.30895252986886E+03 0.31063831978526E+03
 0.30715000001258E+03 0.30715000017074E+03 0.30888146528667E+03 0.31063318497179E+03 0.30715000001237E+03
 0.30715000017079E+03 0.31013212890167E+03 0.30715000140786E+03 -.29282607075659E+02 -.69420083379329E+02
 0.74599689860874E+02 0.15785122211126E+03 0.82878533801080E+02 0.85356262147284E+02 0.62994568356930E+02
 0.10002822020303E+03 0.12262535906909E+03 0.10056186437217E+03 0.62636625735140E+02 0.11480738064472E+03
 0.12229933428955E+03 0.85356262147285E+02 0.62994568356926E+02 0.10002822020304E+03 0.12262535906908E+03
 0.78363649665878E+02 0.62636625735141E+02 0.93291528776538E+02 0.12229933428955E+03 0.56108443151622E+01
 0.00000000000000E+00 0.12488168390178E-03 0.62501941079308E+04 0.62501941079308E+04 0.50048925612237E+08
 0.33668680067363E+03 0.82842434166542E-01 0.82842434166542E-01 0.50057341827160E-02 0.00000000000000E+00
 0.55514289821200E-03 0.26847121608885E-01 0.00000000000000E+00 0.55514289821200E-03 0.26847121608885E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20828212862916E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12869119901599E-02 0.20828212862916E-01 0.12869119901599E-02 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23136747038563E-03 0.71857594684165E-01 0.42718717364317E-01
 0.00000000000000E+00 0.71857594684165E-01 0.42950084834702E-01 0.41085481378394E+00 0.85854810803705E-01
 0.32500000298023E+00 0.42250006586313E+00
