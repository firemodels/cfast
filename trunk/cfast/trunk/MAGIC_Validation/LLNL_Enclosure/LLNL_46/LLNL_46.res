#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-46 0 MONOZONE(1=OUI,0=NON)                                                                     
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
3.000000 2.000000 0.600000 0.560000                                                                 
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
3000.000000 0.004000                                                                                
4000.000000 0.004000                                                                                
4001.000000 0.000000                                                                                
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL46 0 MONOZONE(1=OUI,0=NON)                                                               
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
#VENTILATIONAMONT VEN_1                                                                             
Exhaust 0 0.000000 0.000000 0                                                                       
LOC_2                                                                                               
PAR_9                                                                                               
2.000000 0.750000                                                                                   
0.000000 0.733400                                                                                   
3000.000000                                                                                         
#DEBITVENTILATION                                                                                   
0.000000 0.175000                                                                                   
#FINDEBITVENTILATION                                                                                
#CLAPET 0                                                                                           
#LONGUEURCONDUIT                                                                                    
0.010000 0.000000                                                                                   
#PERTEDECHARGE                                                                                      
0.000000 0.000000 0.000000 0.000000                                                                 
#HAVAL 0.000000                                                                                     
#CALCULTH                                                                                           
3 0.000000 0.000000                                                                                 
#FINVENTILATIONAMONT                                                                                
#CONDINIT 500.000000 10.000000 19.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-46           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-46           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-46           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-46           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-46           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-46           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-46           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-46           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-46           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-46           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-46           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-46           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-46           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-46           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-46           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-46           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-46           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-46           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL46     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL46     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL46     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL46     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL46     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL46     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL46     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL46     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL46     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL46     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL46     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL46     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL46     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL46     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL46     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL46     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL46     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL46     #HEAT_POWER#W#Total sprinkling power
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
#VENT#VEN_1 #Exhaust     #VOLUMETRIC_FLOW_RATE#m3/s#Outlet volumetric flow rate
#VENT#VEN_1 #Exhaust     #MASS_FLOW_RATE#kg/s#Outlet mass flow rate
#VENT#VEN_1 #Exhaust     #TEMPERATURE#K#Outlet air temperature
#NOUVELLE LIGNE
#VENT#VEN_1 #Exhaust     #VOLUMETRIC_FLOW_RATE#m3/s#Vent inlet volume flow rate
#VENT#VEN_1 #Exhaust     #MASS_FLOW_RATE#kg/s#Vent inlet mass flow rate
#VENT#VEN_1 #Exhaust     #TEMPERATURE#K#Inlet vent air temperature
#NOUVELLE LIGNE
#FIN ENTETE
#DEBUTRESULTAT
      0.00000000
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.70907960632475E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.45140398703585E-03 0.45140398703585E-03 0.63936865810520E-03 0.64256550139572E-03
 0.00000000000000E+00 0.42775973169367E-03 0.50340234282081E-03 0.42775973169367E-03 0.50340234282081E-03
 0.42559438689977E-03 0.50339631950060E-03 0.42559438689977E-03 0.50339631950060E-03 0.42775973169367E-03
 0.50340234282081E-03 0.42775973169367E-03 0.50340234282081E-03 0.42559438689977E-03 0.50339631955790E-03
 0.42559438689977E-03 0.50339631955790E-03 0.51868883348924E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.61278100280577E-07 0.99964766651472E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.52567313538439E-03 0.52567313538439E-03
 0.64564699427881E-03 0.64887522925020E-03 0.00000000000000E+00 0.45949077358604E-03 0.50687381525555E-03
 0.45949077358604E-03 0.50687381525555E-03 0.45549150424429E-03 0.50686685399141E-03 0.45549150424429E-03
 0.50686685399141E-03 0.45949077352874E-03 0.50687381531285E-03 0.45949077352874E-03 0.50687381531285E-03
 0.45549150418699E-03 0.50686685399141E-03 0.45549150418699E-03 0.50686685399141E-03 0.41925892725851E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47578231087833E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47578231087833E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17499972426887E+00 0.21087385239361E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     10.85139439
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14672411240197E+02
 0.99985519455968E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000150628E+03 0.29215000000000E+03 0.29215000207922E+03 0.29215000207922E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000206854E+03 0.29215000206854E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000207922E+03 0.29215000207922E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000206854E+03 0.29215000206854E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000735767E+03
 0.29215000607270E+03 0.46318280219909E-03 0.46318280219909E-03 0.61278010532745E-03 0.61584400585409E-03
 .00000000000000E+00 0.43423993092993E-03 0.51557111861787E-03 0.43423993092993E-03 0.51557111861787E-03
 0.43199824108671E-03 0.51561916903469E-03 0.43199824108671E-03 0.51561916903469E-03 0.43423993058613E-03
 0.51557111844597E-03 0.43423993058613E-03 0.51557111844597E-03 0.43199824102941E-03 0.51561916897739E-03
 0.43199824102941E-03 0.51561916897739E-03 0.51897238685959E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999930237E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16216730390520E+02 0.99950641441834E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000604807E+03 0.29215000738766E+03
 0.29215000223287E+03 0.29215000223287E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000221333E+03
 0.29215000221333E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000223287E+03 0.29215000223287E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000221333E+03 0.29215000221333E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000215054E+03 0.29215000000000E+03 0.50707247526478E-03 0.50707247526478E-03
 0.65657985047093E-03 0.65986274972328E-03 .00000000000000E+00 0.46630259762941E-03 0.51249776800068E-03
 0.46630259762941E-03 0.51249776800068E-03 0.46221326859451E-03 0.51259013275880E-03 0.46221326859451E-03
 0.51259013275880E-03 0.46630259762941E-03 0.51249776805798E-03 0.46630259762941E-03 0.51249776805798E-03
 0.46221326865181E-03 0.51259013275880E-03 0.46221326865181E-03 0.51259013275880E-03 0.41946451380521E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21026951982843E+00 0.00000000000000E+00 0.00000000000000E+00 0.21026951982843E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939768682E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939768682E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17452596903652E+00 0.21026930938814E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     21.18039358
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999989E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14672411188988E+02
 0.99985519456019E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000212090E+03 0.29215000000000E+03 0.29215000291857E+03 0.29215000291857E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000290349E+03 0.29215000290349E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000291857E+03 0.29215000291857E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000290349E+03 0.29215000290349E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001016861E+03
 0.29215000839896E+03 0.46753598782983E-03 0.46753598782983E-03 0.60268932598402E-03 0.60570277261394E-03
 .00000000000000E+00 0.43653744863552E-03 0.52002823982783E-03 0.43653744863552E-03 0.52002823982783E-03
 0.43426910236723E-03 0.52009856688583E-03 0.43426910236723E-03 0.52009856688583E-03 0.43653744863552E-03
 0.52002823982783E-03 0.43653744863552E-03 0.52002823982783E-03 0.43426910236723E-03 0.52009856700043E-03
 0.43426910236723E-03 0.52009856700043E-03 0.51895536858343E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999933644E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16216730336917E+02 0.99952345144216E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000837596E+03 0.29215001019651E+03
 0.29215000313406E+03 0.29215000313406E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000310657E+03
 0.29215000310657E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000313406E+03 0.29215000313406E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000310657E+03 0.29215000310657E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000301985E+03 0.29215000000000E+03 0.50003728803001E-03 0.50003728803001E-03
 0.66052099950758E-03 0.66382360450512E-03 .00000000000000E+00 0.46874270447249E-03 0.51451390831314E-03
 0.46874270447249E-03 0.51451390831314E-03 0.46462169219686E-03 0.51464669238677E-03 0.46462169219686E-03
 0.51464669238677E-03 0.46874270441519E-03 0.51451390825584E-03 0.46874270441519E-03 0.51451390825584E-03
 0.46462169225416E-03 0.51464669244407E-03 0.46462169225416E-03 0.51464669244407E-03 0.41945490407538E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21026951946150E+00 0.00000000000000E+00 0.00000000000000E+00 0.21026951946150E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939752372E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939752372E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17452596903809E+00 0.21026930939014E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     36.18039358
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999988E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14672411187691E+02
 0.99985519456020E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000277927E+03 0.29215000000000E+03 0.29215000380856E+03 0.29215000380856E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000378877E+03 0.29215000378877E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000380856E+03 0.29215000380856E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000378877E+03 0.29215000378877E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001297407E+03
 0.29215001072940E+03 0.47182269203574E-03 0.47182269203574E-03 0.59285259255434E-03 0.59581685551711E-03
 .00000000000000E+00 0.43874290911216E-03 0.52434013552663E-03 0.43874290911216E-03 0.52434013552663E-03
 0.43645157625552E-03 0.52443433302571E-03 0.43645157625552E-03 0.52443433302571E-03 0.43874290911216E-03
 0.52434013552663E-03 0.43874290911216E-03 0.52434013552663E-03 0.43645157625552E-03 0.52443433308301E-03
 0.43645157625552E-03 0.52443433308301E-03 0.51892931699439E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999938118E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16216730335579E+02 0.99954594104227E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215001071467E+03 0.29215001299184E+03
 0.29215000408957E+03 0.29215000408957E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000405363E+03
 0.29215000405363E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000408957E+03 0.29215000408957E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000405363E+03 0.29215000405363E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000394411E+03 0.29215000000000E+03 0.49320533007951E-03 0.49320533007951E-03
 0.66432405197474E-03 0.66764567223462E-03 .00000000000000E+00 0.47109388272471E-03 0.51645694910640E-03
 0.47109388272471E-03 0.51645694910640E-03 0.46694620181911E-03 0.51663269799139E-03 0.46694620181911E-03
 0.51663269799139E-03 0.47109388278201E-03 0.51645694910640E-03 0.47109388278201E-03 0.51645694910640E-03
 0.46694620078771E-03 0.51663269707459E-03 0.46694620078771E-03 0.51663269707459E-03 0.41943468360740E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21026951945222E+00 0.00000000000000E+00 0.00000000000000E+00 0.21026951945222E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939752073E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21026939752073E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17452596903812E+00 0.21026930939019E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     40.01202516
 0.28212715938282E+01 0.29215087310154E+03 0.31307895616394E+03 0.29339768741153E+03 0.29331899479385E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22760089626102E+00 0.00000000000000E+00 -.12827573073517E+00
 0.99999574549112E-03 0.23808249216764E-01 0.80000000000000E+04 0.30000000000000E+04 0.33601798801597E+03
 0.12600674550599E+03 0.29241569941462E+03 0.29215000000001E+03 0.29240839490279E+03 0.29282162974829E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29234684313123E+03 0.29282040640950E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29240839490279E+03 0.29282162974829E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29234684313123E+03 0.29282040640950E+03 0.29215000000001E+03 0.29215000000001E+03 0.29344427892417E+03
 0.29215103755682E+03 0.20272526744536E+03 0.20241323805437E+03 0.12245343316942E+03 0.28647746020658E+03
 0.16341175987131E+03 0.14058334568793E+03 0.71839446793580E+02 0.14033376860391E+03 0.31668871227919E+03
 0.10665975578483E+03 0.70996672056802E+02 0.10648506463259E+03 0.31586240474516E+03 0.14058334568793E+03
 0.71839446793580E+02 0.14033376860391E+03 0.31668871227919E+03 0.10665975578483E+03 0.70996672056802E+02
 0.10648506463259E+03 0.31586240474516E+03 0.20018643061523E+02 0.00000000000000E+00 0.15326526305098E-02
 0.76709264157013E+05 0.76709264157013E+05 0.50050000000000E+08 0.32802119331648E+03 0.70008560632484E+00
 0.70008560632484E+00 0.29410682305305E-02 0.14360486153531E+01 0.29215000000000E+03 0.29582332430520E+03
 0.29230660945038E+03 0.29230474670848E+03 0.22999999943550E+00 0.00000000000000E+00 0.22953439582968E+00
 0.00000000000000E+00 -.21904659423409E+01 0.99968449309602E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215103746103E+03 0.29344459957636E+03
 0.29215053809704E+03 0.29223323270921E+03 0.29215000000001E+03 0.29215000000001E+03 0.29215054436410E+03
 0.29223323369244E+03 0.29215000000001E+03 0.29215000000001E+03 0.29215053809704E+03 0.29223323270921E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29215054436410E+03 0.29223323369244E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29217124753046E+03 0.29215000000001E+03 0.24800130125046E+00 0.24775011356868E+00
 0.29171335584762E+00 0.19016664357652E+02 0.18723492435025E+02 0.30700308088193E+00 -.15109027384035E+00
 0.30690174734593E+00 0.43023356021115E+02 0.31021839952055E+00 -.15022558180448E+00 0.31011558990035E+00
 0.43024208000567E+02 0.30700308088187E+00 -.15109027384035E+00 0.30690174734587E+00 0.43023356021115E+02
 0.31021839952055E+00 -.15022558180448E+00 0.31011558990035E+00 0.43024208000567E+02 0.52622588544530E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29961073914079E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18619061937224E-01 0.00000000000000E+00 0.00000000000000E+00 0.18619061937224E-01
 0.16063579691409E+00 0.60635773072234E-01 0.10000002384186E+00 0.52000010490417E-01 0.19849013374074E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19849013374074E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17493580698935E+00 0.21079227371167E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     50.02625537
 0.23049034232332E+01 0.29218950010557E+03 0.35871947434524E+03 0.30760441922103E+03 0.30530927216190E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22206451627468E+00 0.00000000000000E+00 0.15583240371265E+02
 0.10000185872323E-02 0.13600760200480E+00 0.79998513049056E+04 0.29999442393396E+04 0.58820241531187E+02
 0.22057590574195E+02 0.29361143817361E+03 0.29215000000004E+03 0.29388439691934E+03 0.29569009070825E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29345703110514E+03 0.29567070843219E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29388439691934E+03 0.29569009070825E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29345703110514E+03 0.29567070843219E+03 0.29215000000004E+03 0.29215000000004E+03 0.30147079521836E+03
 0.29215982299613E+03 0.50149448573605E+03 0.49856314342758E+03 0.36392446303979E+03 0.96809186144708E+03
 0.60234777609208E+03 0.41726009526273E+03 0.26812340287604E+03 0.41427760496628E+03 0.88164115991388E+03
 0.31505324966197E+03 0.26277596546395E+03 0.31301506597716E+03 0.87651784367419E+03 0.41726009526273E+03
 0.26812340287604E+03 0.41427760496628E+03 0.88164115991388E+03 0.31505324966197E+03 0.26277596546395E+03
 0.31301506597716E+03 0.87651784367419E+03 0.64657939979232E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39372490343030E+03 0.12945897334132E+01
 0.12945897334132E+01 0.35398759580712E-01 0.12398056544307E+01 0.29216187214699E+03 0.30276900363317E+03
 0.29400181590394E+03 0.29394821146861E+03 0.22999999944200E+00 0.00000000000000E+00 0.22854238766735E+00
 0.00000000000000E+00 0.14725200702972E+02 0.99981081803198E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215979067164E+03 0.30150353577502E+03
 0.29215457560141E+03 0.29250642309733E+03 0.29215000000004E+03 0.29215000000004E+03 0.29215460016585E+03
 0.29250644919651E+03 0.29215000000004E+03 0.29215000000004E+03 0.29215457560141E+03 0.29250642309733E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29215460016585E+03 0.29250644919651E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29232579490315E+03 0.29215000000004E+03 0.10925467365651E+01 0.10928466266685E+01
 0.48904202384085E+00 0.70935750648731E+02 0.70444263414771E+02 0.12032856596980E+01 -.62525130979567E+00
 0.12060781746607E+01 0.85090899435135E+02 0.12069332038719E+01 -.61584530005754E+00 0.12097137890875E+01
 0.85100059255272E+02 0.12032856596979E+01 -.62525130979567E+00 0.12060781746607E+01 0.85090899435135E+02
 0.12069332038720E+01 -.61584530005748E+00 0.12097137890876E+01 0.85100059255272E+02 0.22014509295806E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31298943470517E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.20230198590493E+00 0.00000000000000E+00 0.00000000000000E+00 0.20230198590493E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17783938524259E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17783938524259E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17542876596910E+00 0.21141298592476E+00 0.29216187214699E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     60.02146720
 0.19255236823951E+01 0.29224034006812E+03 0.37922074669966E+03 0.32339313574186E+03 0.31839645003151E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21889985978121E+00 0.00000000000000E+00 0.68482224853528E+01
 0.99975843645205E-03 0.18337685029897E+00 0.80000000000000E+04 0.30000000000000E+04 0.43626008337241E+02
 0.16359753126465E+02 0.29443362614755E+03 0.29215000000006E+03 0.29489373570036E+03 0.29832199009303E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29424348631296E+03 0.29828903840931E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29489373570036E+03 0.29832199009303E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29424348631296E+03 0.29828903840931E+03 0.29215000000004E+03 0.29215000000004E+03 0.30840959959932E+03
 0.29217209223002E+03 0.55927994214288E+03 0.55405890226972E+03 0.39671987285576E+03 0.11561598118859E+04
 0.75745633966590E+03 0.46122800328771E+03 0.35412854888731E+03 0.45579867619848E+03 0.10857919580020E+04
 0.35544916817948E+03 0.34809914156161E+03 0.35169195738063E+03 0.10801245592101E+04 0.46122800328771E+03
 0.35412854888731E+03 0.45579867619848E+03 0.10857919580020E+04 0.35544916817948E+03 0.34809914156161E+03
 0.35169195738063E+03 0.10801245592101E+04 0.80279031435689E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40916719964986E+03 0.12946540657457E+01
 0.12946540657457E+01 0.75379606905512E-01 0.10620203728242E+01 0.29215590278476E+03 0.30747974680041E+03
 0.29663025711068E+03 0.29647004721893E+03 0.22999999945932E+00 0.00000000000000E+00 0.22772289758938E+00
 0.00000000000000E+00 0.73347943314012E+01 0.99975830584827E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29217201990466E+03 0.30846428641263E+03
 0.29215938038309E+03 0.29276994459927E+03 0.29215000000004E+03 0.29215000000004E+03 0.29215938136287E+03
 0.29277003810888E+03 0.29215000000004E+03 0.29215000000004E+03 0.29215938038309E+03 0.29276994459927E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29215938136287E+03 0.29277003810888E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29252573067162E+03 0.29215000000006E+03 0.18796101024614E+01 0.18718630017309E+01
 0.95251124544607E-01 0.10991819818858E+03 0.10982247080841E+03 0.18758790269022E+01 -.11581399879647E+01
 0.18747489720265E+01 0.10937402858619E+03 0.18707770292156E+01 -.11346190309575E+01 0.18696465736317E+01
 0.10939673716636E+03 0.18758790269029E+01 -.11581399879647E+01 0.18747489720272E+01 0.10937402858619E+03
 0.18707770292156E+01 -.11346190309575E+01 0.18696465736318E+01 0.10939673716636E+03 0.36808900422253E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32080672547979E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.14638417605464E+00 0.00000000000000E+00 0.00000000000000E+00 0.14638417605464E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17199671235719E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17199671235719E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17521356629933E+00 0.21114255392867E+00 0.29215590278476E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     70.01553410
 0.16242628453078E+01 0.29232461291723E+03 0.39072736522239E+03 0.33745005374063E+03 0.33049360472582E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21660168705027E+00 0.00000000000000E+00 0.13784589389635E+01
 0.99941627080272E-03 0.21589017563843E+00 0.80000000000000E+04 0.30000000000000E+04 0.37055877954347E+02
 0.13895954232880E+02 0.29524345985095E+03 0.29215000000008E+03 0.29578560919040E+03 0.30062928305598E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29495913316626E+03 0.30058769717084E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29578560919040E+03 0.30062928305598E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29495913316626E+03 0.30058769717084E+03 0.29215000000004E+03 0.29215000000004E+03 0.31410333306654E+03
 0.29218791480491E+03 0.65048706296328E+03 0.64284939419370E+03 0.41991077692642E+03 0.12501275292845E+04
 0.82811719847348E+03 0.50782189668652E+03 0.42461581238746E+03 0.50004566947686E+03 0.12125424448252E+04
 0.39771830644013E+03 0.41884105913160E+03 0.39227662228138E+03 0.12072139666020E+04 0.50782189668652E+03
 0.42461581238746E+03 0.50004566947686E+03 0.12125424448252E+04 0.39771830644013E+03 0.41884105913160E+03
 0.39227662228138E+03 0.12072139666020E+04 0.93579345303363E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42167186872108E+03 0.17660608194454E+01
 0.17660608194454E+01 0.11535587452345E+00 0.91744527113210E+00 0.29215264674624E+03 0.31088316656091E+03
 0.29942701534103E+03 0.29915252966338E+03 0.22999999951039E+00 0.00000000000000E+00 0.22699287220926E+00
 0.00000000000000E+00 0.30180533573539E+01 0.99972684311936E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29218781621497E+03 0.31414905752302E+03
 0.29216527359886E+03 0.29301792069471E+03 0.29215000000004E+03 0.29215000000004E+03 0.29216522842422E+03
 0.29301810343378E+03 0.29215000000004E+03 0.29215000000004E+03 0.29216527359886E+03 0.29301792069471E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29216522842422E+03 0.29301810343378E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29273512266711E+03 0.29215000000008E+03 0.27340281652853E+01 0.27134816735473E+01
 -.27008349163206E+00 0.13939213302414E+03 0.13966356693323E+03 0.25934896640171E+01 -.15615839802154E+01
 0.25875516649447E+01 0.12664789845809E+03 0.25793062627859E+01 -.15250881891555E+01 0.25733951548749E+01
 0.12668285782574E+03 0.25934896640172E+01 -.15615839802172E+01 0.25875516649448E+01 0.12664789845809E+03
 0.25793062627860E+01 -.15250881891555E+01 0.25733951548750E+01 0.12668285782574E+03 0.49336591598534E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32659639594774E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10504443625009E+00 0.00000000000000E+00 0.00000000000000E+00 0.10504443625009E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17001542073335E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17001542073335E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17508774505179E+00 0.20572841545602E+00 0.29961645893663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     80.01422486
 0.13865811934881E+01 0.29244664948163E+03 0.39955096018999E+03 0.35004801920008E+03 0.34170925440677E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21444358326554E+00 0.00000000000000E+00 -.23764500883340E+01
 0.99896219877380E-03 0.24563760821251E+00 0.80000000000000E+04 0.30000000000000E+04 0.32568302786432E+02
 0.12213113544912E+02 0.29610280423508E+03 0.29215000000009E+03 0.29665295477997E+03 0.30281177910930E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29567009453531E+03 0.30276488977188E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29665295477997E+03 0.30281177910930E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29567009453531E+03 0.30276488977188E+03 0.29215000000005E+03 0.29215000000005E+03 0.31921182534505E+03
 0.29221540028983E+03 0.74520939110582E+03 0.73490428864677E+03 0.45090101573354E+03 0.13282548359390E+04
 0.87509931512682E+03 0.55486491034106E+03 0.49473786131531E+03 0.54471875337922E+03 0.13253087351574E+04
 0.44117752278783E+03 0.48940887948502E+03 0.43401212143131E+03 0.13204767686802E+04 0.55486491034106E+03
 0.49473786131530E+03 0.54471875337922E+03 0.13253087351574E+04 0.44117752278783E+03 0.48940887948502E+03
 0.43401212143131E+03 0.13204767686802E+04 0.10609691757281E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43267780881214E+03 0.15951899685767E+01
 0.15951899685767E+01 0.15535063753987E+00 0.81519972650458E+00 0.29215094503035E+03 0.31371583644973E+03
 0.30199604072494E+03 0.30161633154286E+03 0.22999999950566E+00 0.00000000000000E+00 0.22626478770395E+00
 0.00000000000000E+00 0.25316075439100E+00 0.99970537743587E-03 0.16495753631952E-01 0.80000000000000E+04
 0.30000000000000E+04 0.48497329546096E+03 0.18186498579786E+03 0.29221527241914E+03 0.31924692692629E+03
 0.29217436147205E+03 0.29326871637560E+03 0.29215000000005E+03 0.29215000000005E+03 0.29217419611298E+03
 0.29326899989291E+03 0.29215000000005E+03 0.29215000000005E+03 0.29217436147205E+03 0.29326871637560E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29217419611298E+03 0.29326899989291E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29295303446162E+03 0.29215000000009E+03 0.51224832934861E+01 0.50751867102996E+01
 0.14186894023463E+01 0.16646152709549E+03 0.16503574424613E+03 0.42786284965346E+01 0.11762470544473E+00
 0.42653312260153E+01 0.14440276155343E+03 0.42305947967644E+01 0.16566267920966E+00 0.42174165464256E+01
 0.14444843697212E+03 0.42786284965346E+01 0.11762470544359E+00 0.42653312260153E+01 0.14440276155342E+03
 0.42305947967645E+01 0.16566267920954E+00 0.42174165464256E+01 0.14444843697212E+03 0.61641018284666E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33129756656882E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.68511535579220E-01 0.00000000000000E+00 0.00000000000000E+00 0.68511535579220E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16924728101184E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16924728101184E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17500710740660E+00 0.20339157822508E+00 0.30291101359873E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     90.13315068
 0.11966409108996E+01 0.29261488275391E+03 0.40702565564620E+03 0.36138945181602E+03 0.35211051561585E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21224046142482E+00 0.00000000000000E+00 -.47820921708091E+01
 0.99836416063946E-03 0.27556953028192E+00 0.80000000000000E+04 0.30000000000000E+04 0.29030785775973E+02
 0.10886544665990E+02 0.29701396840387E+03 0.29215000000010E+03 0.29752672381975E+03 0.30495538803393E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29640161910227E+03 0.30490515398553E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29752672381975E+03 0.30495538803393E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29640161910227E+03 0.30490515398553E+03 0.29215000000005E+03 0.29215000000005E+03 0.32407158906006E+03
 0.29227123386329E+03 0.84119583435061E+03 0.82801950592067E+03 0.48825803404120E+03 0.13990297018000E+04
 0.90833037758859E+03 0.60263408068066E+03 0.56668583189740E+03 0.59007744446407E+03 0.14268744492137E+04
 0.48619331141090E+03 0.56182955974877E+03 0.47725265208299E+03 0.14225436580483E+04 0.60263408068066E+03
 0.56668583189740E+03 0.59007744446407E+03 0.14268744492137E+04 0.48619331141090E+03 0.56182955974877E+03
 0.47725265208299E+03 0.14225436580483E+04 0.11789217146658E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44493065606210E+03 0.14544179238937E+01
 0.14544179238937E+01 0.19582634082140E+00 0.72595366135143E+00 0.29215094625674E+03 0.31619162222208E+03
 0.30455667772981E+03 0.30408154046547E+03 0.22999999950497E+00 0.00000000000000E+00 0.22549471133443E+00
 0.00000000000000E+00 -.12991815811310E+01 0.99969005196550E-03 0.40756101575122E-01 0.80000000000000E+04
 0.30000000000000E+04 0.19628962758507E+03 0.73608610344401E+02 0.29227095567707E+03 0.32410149726830E+03
 0.29219101104819E+03 0.29353315772144E+03 0.29215000000005E+03 0.29215000000005E+03 0.29219055147825E+03
 0.29353354864047E+03 0.29215000000005E+03 0.29215000000005E+03 0.29219101104819E+03 0.29353315772144E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29219055147825E+03 0.29353354864047E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29318584371802E+03 0.29215000000010E+03 0.91505698041080E+01 0.90419504665348E+01
 0.52265423881531E+01 0.19278124270877E+03 0.18752856760868E+03 0.70292979024179E+01 0.38978388620349E+01
 0.70024129123570E+01 0.16221751024146E+03 0.69244769999123E+01 0.39558007090955E+01 0.68979831524191E+01
 0.16227222755534E+03 0.70292979024178E+01 0.38978388620328E+01 0.70024129123569E+01 0.16221751024145E+03
 0.69244769999123E+01 0.39558007090956E+01 0.68979831524192E+01 0.16227222755534E+03 0.74564349199486E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33549407419627E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35307601118589E-01 0.00000000000000E+00 0.00000000000000E+00 0.35307601118589E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16917739602186E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16917739602186E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17488327076058E+00 0.20240230413548E+00 0.30417148830196E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    100.03472838
 0.10417131746538E+01 0.29282943937610E+03 0.41269631300011E+03 0.37107401251388E+03 0.36133652651396E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21012938286899E+00 0.00000000000000E+00 -.60260054024062E+01
 0.99762040955716E-03 0.30427568249547E+00 0.80000000000000E+04 0.30000000000000E+04 0.26291946613641E+02
 0.98594799801153E+01 0.29794124854432E+03 0.29215000000010E+03 0.29838303750827E+03 0.30698572641157E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29713117972222E+03 0.30693353287723E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29838303750827E+03 0.30698572641157E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29713117972222E+03 0.30693353287723E+03 0.29215000000005E+03 0.29215000000005E+03 0.32851220432269E+03
 0.29234889655478E+03 0.93268532140295E+03 0.91660369956619E+03 0.52408696530192E+03 0.14498268611274E+04
 0.92311946099895E+03 0.64770341361860E+03 0.63262696538817E+03 0.63281147217557E+03 0.15116043723698E+04
 0.52920396782076E+03 0.62820981861724E+03 0.51853108681553E+03 0.15077278348357E+04 0.64770341361860E+03
 0.63262696538817E+03 0.63281147217557E+03 0.15116043723698E+04 0.52920396782076E+03 0.62820981861724E+03
 0.51853108681553E+03 0.15077278348357E+04 0.12816848291702E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45384849775987E+03 0.13355209990449E+01
 0.13355209990449E+01 0.23543265163702E+00 0.64819385423382E+00 0.29215303484233E+03 0.31830005938934E+03
 0.30700116564409E+03 0.30644830228604E+03 0.22999999968601E+00 0.00000000000000E+00 0.22470005952419E+00
 0.00000000000000E+00 -.18378530979215E+01 0.99967758870750E-03 0.62502246648726E-01 0.80000000000000E+04
 0.30000000000000E+04 0.12799539902880E+03 0.47998274635799E+02 0.29234844678523E+03 0.32854101331121E+03
 0.29221352128989E+03 0.29379818824871E+03 0.29215000000005E+03 0.29215000000005E+03 0.29221265451657E+03
 0.29379868445081E+03 0.29215000000005E+03 0.29215000000005E+03 0.29221352128989E+03 0.29379818824871E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29221265451657E+03 0.29379868445081E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29342122976251E+03 0.29215000000010E+03 0.13807174958031E+02 0.13599110074389E+02
 0.97837137293222E+01 0.21629613224738E+03 0.20646349994941E+03 0.10193791989263E+02 0.83979364127193E+01
 0.10147439472890E+02 0.17860187999134E+03 0.10028482434852E+02 0.84637261838456E+01 0.99829724548515E+01
 0.17866355758552E+03 0.10193791989263E+02 0.83979364127175E+01 0.10147439472890E+02 0.17860187999134E+03
 0.10028482434852E+02 0.84637261838456E+01 0.99829724548515E+01 0.17866355758552E+03 0.86912870009612E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33845420302035E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12256953731419E-01 0.73513010304594E-02 0.00000000000000E+00 0.12256953731419E-01 0.73513010304594E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16982521080063E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16982521080063E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17486342566322E+00 0.20146193751215E+00 0.30555497132398E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    110.70861930
 0.90880598184485E+00 0.29311867454142E+03 0.41767982976884E+03 0.37994585544344E+03 0.37004315517453E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20782947071544E+00 0.00000000000000E+00 -.70755398510908E+01
 0.99662568268293E-03 0.33557860113870E+00 0.80000000000000E+04 0.30000000000000E+04 0.23839422337581E+02
 0.89397833765929E+01 0.29896631554509E+03 0.29215000000010E+03 0.29930438710318E+03 0.30909999525110E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29792770917740E+03 0.30904665908821E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29930438710318E+03 0.30909999525110E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29792770917740E+03 0.30904665908821E+03 0.29215000000005E+03 0.29215000000005E+03 0.33298696999656E+03
 0.29245633131523E+03 0.10262492721189E+04 0.10070312887085E+04 0.56159424146952E+03 0.14938357965416E+04
 0.92943358386476E+03 0.69394512600915E+03 0.69931600665060E+03 0.67662532403710E+03 0.15923792365215E+04
 0.57385821323011E+03 0.69532775863928E+03 0.56138084213694E+03 0.15889388949165E+04 0.69394512600915E+03
 0.69931600665061E+03 0.67662532403710E+03 0.15923792365215E+04 0.57385821323011E+03 0.69532775863929E+03
 0.56138084213694E+03 0.15889388949165E+04 0.13802327719554E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46267462219212E+03 0.12947566285960E+01
 0.12947566285960E+01 0.27812821530244E+00 0.57466349452416E+00 0.29215780321919E+03 0.32015100048465E+03
 0.30942655477567E+03 0.30881510333147E+03 0.23000000000000E+00 0.00000000000000E+00 0.22382069974553E+00
 0.00000000000000E+00 -.22610599822666E+01 0.99965709587243E-03 0.84698998084321E-01 0.80000000000000E+04
 0.30000000000000E+04 0.94452120815357E+02 0.35419545305759E+02 0.29245571613159E+03 0.33301672171353E+03
 0.29224413638891E+03 0.29408493836369E+03 0.29215000000005E+03 0.29215000000005E+03 0.29224273019409E+03
 0.29408554398564E+03 0.29215000000005E+03 0.29215000000005E+03 0.29224413638891E+03 0.29408493836369E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29224273019409E+03 0.29408554398564E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29367538797493E+03 0.29215000000010E+03 0.19380071349967E+02 0.19015023154754E+02
 0.15331786467942E+02 0.23806889728198E+03 0.22266045188170E+03 0.13965854251082E+02 0.13845417589576E+02
 0.13891339804924E+02 0.19416446791153E+03 0.13734652627168E+02 0.13917738027012E+02 0.13661677181308E+02
 0.19423177323179E+03 0.13965854251082E+02 0.13845417589575E+02 0.13891339804924E+02 0.19416446791153E+03
 0.13734652627168E+02 0.13917738027012E+02 0.13661677181308E+02 0.19423177323179E+03 0.99224628232487E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34049947100617E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.32489684486288E-03 0.28390238225851E-01 0.00000000000000E+00 0.32489684486288E-03 0.28390238225851E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17077408643480E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17077408643480E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17484794959533E+00 0.19737095299514E+00 0.31185942174611E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    120.16282604
 0.82165069921706E+00 0.29342120910492E+03 0.42151333642363E+03 0.38643100776509E+03 0.37649808482832E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20571750318196E+00 0.00000000000000E+00 -.83123508680404E+01
 0.99558594936077E-03 0.36428494224241E+00 0.80000000000000E+04 0.30000000000000E+04 0.21960830856073E+02
 0.82353115710275E+01 0.29989492841762E+03 0.29215000000010E+03 0.30012447731579E+03 0.31093102771245E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29864607072406E+03 0.31087729518043E+03 0.29215000000005E+03
 0.29215000000005E+03 0.30012447731579E+03 0.31093102771245E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29864607072406E+03 0.31087729518043E+03 0.29215000000005E+03 0.29215000000005E+03 0.33675845065615E+03
 0.29257108950766E+03 0.11028539588107E+04 0.10808690204036E+04 0.59405574889399E+03 0.15275343889336E+04
 0.93050836129514E+03 0.73260327009632E+03 0.75561263527697E+03 0.71321968946466E+03 0.16570791160217E+04
 0.61156819159932E+03 0.75195798688860E+03 0.59755298149715E+03 0.16539726191521E+04 0.73260327009632E+03
 0.75561263527697E+03 0.71321968946466E+03 0.16570791160217E+04 0.61156819159932E+03 0.75195798688861E+03
 0.59755298149715E+03 0.16539726191521E+04 0.14570153629544E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47125614288419E+03 0.12947657399372E+01
 0.12947657399372E+01 0.31594504226340E+00 0.53222365237668E+00 0.29216482793415E+03 0.32150524846882E+03
 0.31109480461600E+03 0.31044349739510E+03 0.22999999984638E+00 0.00000000000000E+00 0.22301221144245E+00
 0.00000000000000E+00 -.30500774870234E+01 0.99962527338884E-03 0.10399348647939E+00 0.80000000000000E+04
 0.30000000000000E+04 0.76927894917588E+02 0.28847960594096E+02 0.29257034525994E+03 0.33679017375277E+03
 0.29227656294683E+03 0.29433974390179E+03 0.29215000000005E+03 0.29215000000005E+03 0.29227460420592E+03
 0.29434044077313E+03 0.29215000000005E+03 0.29215000000005E+03 0.29227656294683E+03 0.29433974390179E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29227460420592E+03 0.29434044077313E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29390090839433E+03 0.29215000000010E+03 0.24396860947907E+02 0.23846213401039E+02
 0.20429088134408E+02 0.25477748145520E+03 0.23424624788012E+03 0.17413591604363E+02 0.18834401579902E+02
 0.17308678094865E+02 0.20668836950406E+03 0.17125156050634E+02 0.18910980870149E+02 0.17022574657328E+02
 0.20675916901162E+03 0.17413591604363E+02 0.18834401579901E+02 0.17308678094865E+02 0.20668836950406E+03
 0.17125156050634E+02 0.18910980870149E+02 0.17022574657328E+02 0.20675916901162E+03 0.10871102717295E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34208216531940E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.57006281978627E-01 0.00000000000000E+00 0.00000000000000E+00 0.57006281978627E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17129917799097E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17129917799097E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17482247868332E+00 0.19142003306175E+00 0.32150524846882E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    130.04202106
 0.75860795602126E+00 0.29377275268250E+03 0.42508278820911E+03 0.39187850899047E+03 0.38191594095789E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20341617582817E+00 0.00000000000000E+00 -.99517803197324E+01
 0.99437848949900E-03 0.39552064651848E+00 0.80000000000000E+04 0.30000000000000E+04 0.20226504154508E+02
 0.75849390579407E+01 0.30085990087482E+03 0.29215000000010E+03 0.30097173618064E+03 0.31279228679232E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29939515919678E+03 0.31273848913670E+03 0.29215000000005E+03
 0.29215000000005E+03 0.30097173618064E+03 0.31279228679232E+03 0.29215000000005E+03 0.29215000000005E+03
 0.29939515919678E+03 0.31273848913670E+03 0.29215000000005E+03 0.29215000000005E+03 0.34051334549716E+03
 0.29270630031455E+03 0.11751900480807E+04 0.11504145441144E+04 0.62696222664603E+03 0.15581699771255E+04
 0.92807293934621E+03 0.77023403108809E+03 0.81180511553561E+03 0.74883078739409E+03 0.17206733072600E+04
 0.64854353230708E+03 0.80845482379264E+03 0.63302429893566E+03 0.17178687223582E+04 0.77023403108809E+03
 0.81180511553561E+03 0.74883078739410E+03 0.17206733072600E+04 0.64854353230708E+03 0.80845482379264E+03
 0.63302429893566E+03 0.17178687223582E+04 0.15274837441164E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47958265189197E+03 0.12947778175285E+01
 0.12947778175285E+01 0.35546182234415E+00 0.49694804035139E+00 0.29217585347013E+03 0.32270666536780E+03
 0.31259184727254E+03 0.31190871648827E+03 0.22999999926808E+00 0.00000000000000E+00 0.22212488563135E+00
 0.00000000000000E+00 -.43326546886816E+01 0.99957489389790E-03 0.12434402911370E+00 0.80000000000000E+04
 0.30000000000000E+04 0.64337628891570E+02 0.24126610834339E+02 0.29270545605542E+03 0.34054762947298E+03
 0.29231422233861E+03 0.29460276066287E+03 0.29215000000005E+03 0.29215000000005E+03 0.29231163348062E+03
 0.29460354792732E+03 0.29215000000005E+03 0.29215000000005E+03 0.29231422233861E+03 0.29460276066287E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29231163348062E+03 0.29460354792732E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29413161636743E+03 0.29215000000010E+03 0.29773837175758E+02 0.28987848967021E+02
 0.25985466199656E+02 0.27046274669011E+03 0.24434735315946E+03 0.21119609281093E+02 0.24256161360604E+02
 0.20980164279047E+02 0.21864927146009E+03 0.20771813436930E+02 0.24336358061867E+02 0.20635676329338E+02
 0.21872292713720E+03 0.21119609281093E+02 0.24256161360603E+02 0.20980164279047E+02 0.21864927146008E+03
 0.20771813436930E+02 0.24336358061867E+02 0.20635676329338E+02 0.21872292713720E+03 0.11780888433419E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34355584040769E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.84089540801517E-01 0.00000000000000E+00 0.00000000000000E+00 0.84089540801517E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17195594173689E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17195594173689E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17478290701970E+00 0.19066180652356E+00 0.32270666536780E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    140.08265865
 0.71810219774519E+00 0.29415600261455E+03 0.42825799045190E+03 0.39615834638923E+03 0.38612253869649E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20098669870560E+00 0.00000000000000E+00 -.11729165426293E+02
 0.99306551198693E-03 0.42850989032704E+00 0.80000000000000E+04 0.30000000000000E+04 0.18669347384011E+02
 0.70010052690041E+01 0.30183967924172E+03 0.29215000000010E+03 0.30183013109542E+03 0.31465613286417E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30016096317386E+03 0.31460259889591E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30183013109542E+03 0.31465613286417E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30016096317386E+03 0.31460259889591E+03 0.29215000000006E+03 0.29215000000006E+03 0.34416435634867E+03
 0.29286521405706E+03 0.12398877599938E+04 0.12123286036448E+04 0.65813401009377E+03 0.15825089835390E+04
 0.92108430339480E+03 0.80509727657208E+03 0.86524381333558E+03 0.78174228226697E+03 0.17805466800785E+04
 0.68298198741611E+03 0.86216348430219E+03 0.66600267997037E+03 0.17780073355128E+04 0.80509727657208E+03
 0.86524381333558E+03 0.78174228226697E+03 0.17805466800785E+04 0.68298198741611E+03 0.86216348430219E+03
 0.66600267997037E+03 0.17780073355129E+04 0.15885277113293E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48598021460157E+03 0.12947909117411E+01
 0.12947909117411E+01 0.39562437269744E+00 0.46269106332200E+00 0.29219271041509E+03 0.32373690093981E+03
 0.31400675756946E+03 0.31330375081248E+03 0.22999999927979E+00 0.00000000000000E+00 0.22117415583169E+00
 0.00000000000000E+00 -.58449970356516E+01 0.99950230288421E-03 0.14544397915280E+00 0.80000000000000E+04
 0.30000000000000E+04 0.55003995673106E+02 0.20626498377415E+02 0.29286426430282E+03 0.34420127774800E+03
 0.29235780919081E+03 0.29486932189889E+03 0.29215000000006E+03 0.29215000000006E+03 0.29235452102667E+03
 0.29487019188624E+03 0.29215000000006E+03 0.29215000000006E+03 0.29235780919081E+03 0.29486932189889E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29235452102667E+03 0.29487019188624E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29436544327133E+03 0.29215000000010E+03 0.35465323435074E+02 0.34386705704579E+02
 0.31958948149260E+02 0.28487183305532E+03 0.25275309016532E+03 0.25092721735382E+02 0.30059150831264E+02
 0.24915840558869E+02 0.22962066340613E+03 0.24686881144970E+02 0.30141088693124E+02 0.24514464601815E+02
 0.22969536778644E+03 0.25092721735382E+02 0.30059150831261E+02 0.24915840558870E+02 0.22962066340612E+03
 0.24686881144970E+02 0.30141088693123E+02 0.24514464601816E+02 0.22969536778644E+03 0.12655657217038E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34486606882861E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10760126520698E+00 0.00000000000000E+00 0.00000000000000E+00 0.10760126520698E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17257668210751E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17257668210751E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17473713715595E+00 0.19000245275147E+00 0.32373690093981E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    150.45405278
 0.69479823048406E+00 0.29456555776745E+03 0.43100202407564E+03 0.39940341895416E+03 0.38924678449108E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19841522876799E+00 0.00000000000000E+00 -.13509522668234E+02
 0.99166735668931E-03 0.46355178611564E+00 0.80000000000000E+04 0.30000000000000E+04 0.17258050210607E+02
 0.64717688289775E+01 0.30282775076117E+03 0.29215000000010E+03 0.30269875904372E+03 0.31652849459509E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30094059152963E+03 0.31647542778952E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30269875904372E+03 0.31652849459509E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30094059152963E+03 0.31647542778952E+03 0.29215000000006E+03 0.29215000000006E+03 0.34772384376208E+03
 0.29305036897014E+03 0.12967679420801E+04 0.12664481288021E+04 0.68644063676548E+03 0.15989374554162E+04
 0.90906461546693E+03 0.83685693215294E+03 0.91502681073435E+03 0.81161454838320E+03 0.18344248285464E+04
 0.71448146640974E+03 0.91219169205465E+03 0.69608757863999E+03 0.18321244261349E+04 0.83685693215294E+03
 0.91502681073435E+03 0.81161454838320E+03 0.18344248285464E+04 0.71448146640974E+03 0.91219169205465E+03
 0.69608757863999E+03 0.18321244261349E+04 0.16393724167157E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49064112168585E+03 0.12948040281721E+01
 0.12948040281721E+01 0.43710994921504E+00 0.42904455964159E+00 0.29221792653933E+03 0.32463003682023E+03
 0.31535921043188E+03 0.31464761232652E+03 0.22999999927606E+00 0.00000000000000E+00 0.22013666067855E+00
 0.00000000000000E+00 -.74430203928675E+01 0.99940028512351E-03 0.16785776283698E+00 0.80000000000000E+04
 0.30000000000000E+04 0.47659398438244E+02 0.17872274414341E+02 0.29304929859687E+03 0.34776305703322E+03
 0.29240734123751E+03 0.29514008329003E+03 0.29215000000006E+03 0.29215000000006E+03 0.29240329351932E+03
 0.29514102827710E+03 0.29215000000006E+03 0.29215000000006E+03 0.29240734123751E+03 0.29514008329003E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29240329351932E+03 0.29514102827710E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29460248557368E+03 0.29215000000010E+03 0.41508010412322E+02 0.40074445853614E+02
 0.38374529697996E+02 0.29834675359430E+03 0.25978035124782E+03 0.29338236259662E+02 0.36279276214985E+02
 0.29124610707880E+02 0.23990719165340E+03 0.28875253722913E+02 0.36361668586992E+02 0.28667409090057E+02
 0.23998172741632E+03 0.29338236259662E+02 0.36279276214982E+02 0.29124610707880E+02 0.23990719165339E+03
 0.28875253722913E+02 0.36361668586992E+02 0.28667409090057E+02 0.23998172741632E+03 0.13509691460826E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34603886656648E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12777891320265E+00 0.00000000000000E+00 0.00000000000000E+00 0.12777891320265E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17340289101662E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17340289101662E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17468927224845E+00 0.18942481796011E+00 0.32463003682023E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    160.73592810
 0.68412594879768E+00 0.29497433872701E+03 0.43314099675636E+03 0.40163319808418E+03 0.39133987577871E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19585392594831E+00 0.00000000000000E+00 -.15036980514476E+02
 0.99027815500825E-03 0.49869319026354E+00 0.80000000000000E+04 0.30000000000000E+04 0.16041927494082E+02
 0.60157228102806E+01 0.30377549637630E+03 0.29215000000010E+03 0.30353630054775E+03 0.31831690326354E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30169575652725E+03 0.31826444946180E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30353630054775E+03 0.31831690326354E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30169575652725E+03 0.31826444946180E+03 0.29215000000006E+03 0.29215000000006E+03 0.35101530371117E+03
 0.29325671830498E+03 0.13431281306610E+04 0.13101855817399E+04 0.70915474896262E+03 0.16051209642204E+04
 0.89242044151292E+03 0.86361432320511E+03 0.95742206656966E+03 0.83661233073702E+03 0.18773107523429E+04
 0.74111714757548E+03 0.95480241578809E+03 0.72140093604931E+03 0.18752182741057E+04 0.86361432320511E+03
 0.95742206656966E+03 0.83661233073702E+03 0.18773107523429E+04 0.74111714757548E+03 0.95480241578809E+03
 0.72140093604931E+03 0.18752182741057E+04 0.16769730617571E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49365517649301E+03 0.12948152816740E+01
 0.12948152816740E+01 0.47823745049898E+00 0.39747630544691E+00 0.29225311135663E+03 0.32536211629205E+03
 0.31658875298622E+03 0.31587949501967E+03 0.22999999926883E+00 0.00000000000000E+00 0.21905165948108E+00
 0.00000000000000E+00 -.88604666364963E+01 0.99926598074346E-03 0.19080391442813E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41927860987429E+02 0.15722947870286E+02 0.29325552634362E+03 0.35105611752009E+03
 0.29246095118864E+03 0.29540380502586E+03 0.29215000000006E+03 0.29215000000006E+03 0.29245612360927E+03
 0.29540481245640E+03 0.29215000000006E+03 0.29215000000006E+03 0.29246095118864E+03 0.29540380502586E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29245612360927E+03 0.29540481245640E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29483304065302E+03 0.29215000000010E+03 0.47580716640814E+02 0.45741341443703E+02
 0.44901165057929E+02 0.31034719290860E+03 0.26522152202539E+03 0.33655933851016E+02 0.42593392817638E+02
 0.33412061852933E+02 0.24910649567888E+03 0.33140268914035E+02 0.42674828864178E+02 0.32903561612914E+02
 0.24917956247774E+03 0.33655933851016E+02 0.42593392817636E+02 0.33412061852933E+02 0.24910649567887E+03
 0.33140268914035E+02 0.42674828864178E+02 0.32903561612914E+02 0.24917956247774E+03 0.14303678690212E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34701642202888E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14327077243787E+00 0.00000000000000E+00 0.00000000000000E+00 0.14327077243787E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17432574075708E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17432574075708E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17464708182825E+00 0.18895031278018E+00 0.32536211629205E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    170.55711471
 0.68084837902106E+00 0.29536150721645E+03 0.43466070115221E+03 0.40304682435551E+03 0.39263518324949E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19344258567278E+00 0.00000000000000E+00 -.16183100096406E+02
 0.98896888115122E-03 0.53206428490867E+00 0.80000000000000E+04 0.30000000000000E+04 0.15035777117371E+02
 0.56384164190140E+01 0.30464543889477E+03 0.29215000000010E+03 0.30430950064622E+03 0.31994809066738E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30239509636311E+03 0.31989632601114E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30430950064622E+03 0.31994809066738E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30239509636311E+03 0.31989632601114E+03 0.29215000000006E+03 0.29215000000006E+03 0.35391824389039E+03
 0.29347617898703E+03 0.13787493495430E+04 0.13434183296361E+04 0.72531310128430E+03 0.16018637693472E+04
 0.87292410255650E+03 0.88476363948296E+03 0.99077945592469E+03 0.85617779058130E+03 0.19076996584423E+04
 0.76225325623085E+03 0.98834419278203E+03 0.74134509201881E+03 0.19057833176455E+04 0.88476363948296E+03
 0.99077945592469E+03 0.85617779058130E+03 0.19076996584423E+04 0.76225325623085E+03 0.98834419278203E+03
 0.74134509201881E+03 0.19057833176456E+04 0.17016918716505E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49541161679398E+03 0.12948237258330E+01
 0.12948237258330E+01 0.51752219694767E+00 0.36900261013036E+00 0.29229858797324E+03 0.32593182881289E+03
 0.31765799304158E+03 0.31695992649848E+03 0.22999999926672E+00 0.00000000000000E+00 0.21796426762132E+00
 0.00000000000000E+00 -.99495746203610E+01 0.99909976837521E-03 0.21343672706741E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37481834124422E+02 0.14055687796658E+02 0.29347486824634E+03 0.35395996600274E+03
 0.29251619998362E+03 0.29565090234689E+03 0.29215000000006E+03 0.29215000000006E+03 0.29251061346817E+03
 0.29565195749833E+03 0.29215000000006E+03 0.29215000000006E+03 0.29251619998362E+03 0.29565090234689E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29251061346817E+03 0.29565195749833E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29504877397397E+03 0.29215000000010E+03 0.53370623035525E+02 0.51094511819799E+02
 0.51206343945052E+02 0.32057736733616E+03 0.26911499167138E+03 0.37843017640027E+02 0.48678933592348E+02
 0.37580748577350E+02 0.25698847284274E+03 0.37281694924120E+02 0.48758149470290E+02 0.37027928778715E+02
 0.25705893466207E+03 0.37843017640027E+02 0.48678933592347E+02 0.37580748577350E+02 0.25698847284274E+03
 0.37281694924120E+02 0.48758149470290E+02 0.37027928778715E+02 0.25705893466207E+03 0.15009544427772E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34778510066507E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15406045573591E+00 0.00000000000000E+00 0.00000000000000E+00 0.15406045573591E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17528743274194E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17528743274194E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17461483171063E+00 0.18858317881550E+00 0.32593182881289E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    180.40688475
 0.68116143712317E+00 0.29574451800402E+03 0.43574658677570E+03 0.40395858332078E+03 0.39345608705923E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19109285740449E+00 0.00000000000000E+00 -.17023510959856E+02
 0.98767990069545E-03 0.56487102389337E+00 0.80000000000000E+04 0.30000000000000E+04 0.14162525004133E+02
 0.53109468765498E+01 0.30547965001305E+03 0.29215000000010E+03 0.30505499864732E+03 0.32149899162319E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30307070606274E+03 0.32144798335363E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30505499864732E+03 0.32149899162319E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30307070606274E+03 0.32144798335363E+03 0.29215000000006E+03 0.29215000000006E+03 0.35659000670453E+03
 0.29371818715454E+03 0.14072143515510E+04 0.13696197290194E+04 0.73638455753787E+03 0.15911322744756E+04
 0.85106579415007E+03 0.90206438236267E+03 0.10175308367890E+04 0.87198795820399E+03 0.19286541358939E+04
 0.77962294781443E+03 0.10152624063004E+04 0.75759468923707E+03 0.19268956595019E+04 0.90206438236267E+03
 0.10175308367890E+04 0.87198795820399E+03 0.19286541358939E+04 0.77962294781443E+03 0.10152624063004E+04
 0.75759468923707E+03 0.19268956595019E+04 0.17172610794357E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49642087744928E+03 0.12948299177353E+01
 0.12948299177353E+01 0.55692127709216E+00 0.34209337241422E+00 0.29235849480469E+03 0.32638703600023E+03
 0.31862641038962E+03 0.31794716190274E+03 0.22999999926226E+00 0.00000000000000E+00 0.21682722721184E+00
 0.00000000000000E+00 -.10764599108000E+02 0.99888700563003E-03 0.23682337144746E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33780449755040E+02 0.12667668658140E+02 0.29371675268074E+03 0.35663211953901E+03
 0.29257512285935E+03 0.29589310761874E+03 0.29215000000006E+03 0.29215000000006E+03 0.29256877447323E+03
 0.29589419866095E+03 0.29215000000006E+03 0.29215000000006E+03 0.29257512285935E+03 0.29589310761874E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29256877447323E+03 0.29589419866095E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29525990721284E+03 0.29215000000010E+03 0.59075573352711E+02 0.56318952627426E+02
 0.57502429558237E+02 0.32964505118261E+03 0.27185510947658E+03 0.42049435073805E+02 0.54743671308968E+02
 0.41784692581143E+02 0.26401284088268E+03 0.41447531631907E+02 0.54819625268974E+02 0.41192566392290E+02
 0.26407975946459E+03 0.42049435073805E+02 0.54743671308967E+02 0.41784692581143E+02 0.26401284088268E+03
 0.41447531631907E+02 0.54819625268974E+02 0.41192566392290E+02 0.26407975946459E+03 0.15662065168162E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34840939137141E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16159716014525E+00 0.00000000000000E+00 0.00000000000000E+00 0.16159716014525E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17630551558864E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17630551558864E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17459085584790E+00 0.18829279167420E+00 0.32638703600023E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    190.34240505
 0.68313657972735E+00 0.29612464576568E+03 0.43650145849236E+03 0.40453594658598E+03 0.39397353669309E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18881291678474E+00 0.00000000000000E+00 -.17597543145271E+02
 0.98640645162537E-03 0.59695698635772E+00 0.80000000000000E+04 0.30000000000000E+04 0.13401300567418E+02
 0.50254877127819E+01 0.30628232301626E+03 0.29215000000011E+03 0.30577588709736E+03 0.32297522843869E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30372493024454E+03 0.32292502978662E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30577588709736E+03 0.32297522843869E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30372493024454E+03 0.32292502978662E+03 0.29215000000006E+03 0.29215000000006E+03 0.35905080319869E+03
 0.29398523104904E+03 0.14300398558082E+04 0.13902996394845E+04 0.74323420832830E+03 0.15749496351944E+04
 0.82799925582444E+03 0.91620856226038E+03 0.10386608199910E+04 0.88472497044344E+03 0.19419105063809E+04
 0.79390130245376E+03 0.10365444959202E+04 0.77081911427173E+03 0.19402946038925E+04 0.91620856226038E+03
 0.10386608199910E+04 0.88472497044345E+03 0.19419105063809E+04 0.79390130245376E+03 0.10365444959202E+04
 0.77081911427173E+03 0.19402946038925E+04 0.17258411409310E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49696056877263E+03 0.12948341470784E+01
 0.12948341470784E+01 0.59666335832402E+00 0.31659455485964E+00 0.29243615497988E+03 0.32674160834708E+03
 0.31950099518838E+03 0.31884707049689E+03 0.22999999932671E+00 0.00000000000000E+00 0.21563768945045E+00
 0.00000000000000E+00 -.11333066313607E+02 0.99861613318922E-03 0.26106857146047E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30643290210102E+02 0.11491233828788E+02 0.29398367821520E+03 0.35909295062913E+03
 0.29263789915007E+03 0.29613132205696E+03 0.29215000000006E+03 0.29215000000006E+03 0.29263079162431E+03
 0.29613243674852E+03 0.29215000000006E+03 0.29215000000006E+03 0.29263789915007E+03 0.29613132205696E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29263079162431E+03 0.29613243674852E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29546720546217E+03 0.29215000000010E+03 0.64645303978474E+02 0.61365975672421E+02
 0.63755144069433E+02 0.33765342314703E+03 0.27357950335725E+03 0.46270812695878E+02 0.60750073156468E+02
 0.46024941239523E+02 0.27024859830992E+03 0.45633979061027E+02 0.60821606720618E+02 0.45398991296625E+02
 0.27031092122900E+03 0.46270812695878E+02 0.60750073156466E+02 0.46024941239523E+02 0.27024859830992E+03
 0.45633979061027E+02 0.60821606720618E+02 0.45398991296625E+02 0.27031092122900E+03 0.16263168390486E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34891248359965E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16657663802357E+00 0.00000000000000E+00 0.00000000000000E+00 0.16657663802357E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17735923592058E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17735923592058E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17457431120309E+00 0.18806958140983E+00 0.32674160834708E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    200.27792536
 0.68559801736649E+00 0.29649923247552E+03 0.43702298021753E+03 0.40490871260258E+03 0.39431422681354E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18663282176053E+00 0.00000000000000E+00 -.17963491328060E+02
 0.98515670173144E-03 0.62782314905598E+00 0.80000000000000E+04 0.30000000000000E+04 0.12742441899489E+02
 0.47784157123083E+01 0.30704885453483E+03 0.29215000000012E+03 0.30646725654082E+03 0.32436772416272E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30435304558107E+03 0.32431836578011E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30646725654082E+03 0.32436772416272E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30435304558107E+03 0.32431836578011E+03 0.29215000000006E+03 0.29215000000006E+03 0.36129879434071E+03
 0.29427530961099E+03 0.14484482261818E+04 0.14066970049560E+04 0.74685623042463E+03 0.15556110351101E+04
 0.80502052353336E+03 0.92778226584376E+03 0.10551662566220E+04 0.89498323115302E+03 0.19493621024356E+04
 0.80565602975488E+03 0.10531875350224E+04 0.78159347618355E+03 0.19478739818284E+04 0.92778226584376E+03
 0.10551662566220E+04 0.89498323115302E+03 0.19493621024356E+04 0.80565602975488E+03 0.10531875350224E+04
 0.78159347618355E+03 0.19478739818284E+04 0.17295004662979E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49723571047710E+03 0.12948368433218E+01
 0.12948368433218E+01 0.63640543955588E+00 0.29269549591971E+00 0.29253444403103E+03 0.32700394025816E+03
 0.32027789606329E+03 0.31965434670803E+03 0.23000000000000E+00 0.00000000000000E+00 0.21441116901274E+00
 0.00000000000000E+00 -.11703200969318E+02 0.99827695842021E-03 0.28589674865590E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27982130043839E+02 0.10493298766439E+02 0.29427363788673E+03 0.36134071006084E+03
 0.29270381710656E+03 0.29636314110246E+03 0.29215000000006E+03 0.29215000000006E+03 0.29269596889864E+03
 0.29636426645844E+03 0.29215000000006E+03 0.29215000000006E+03 0.29270381710656E+03 0.29636314110246E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29269596889864E+03 0.29636426645844E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29566870561682E+03 0.29215000000010E+03 0.69952137093611E+02 0.66121726978739E+02
 0.69831402899384E+02 0.34454979631260E+03 0.27436923639872E+03 0.50429593257846E+02 0.66571174475596E+02
 0.50228989155260E+02 0.27567228841460E+03 0.49764169460797E+02 0.66637338347375E+02 0.49575191769915E+02
 0.27572917698463E+03 0.50429593257846E+02 0.66571174475594E+02 0.50228989155260E+02 0.27567228841460E+03
 0.49764169460796E+02 0.66637338347375E+02 0.49575191769914E+02 0.27572917698463E+03 0.16805578992075E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34926431017778E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16967004579534E+00 0.00000000000000E+00 0.00000000000000E+00 0.16967004579534E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17836927978589E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17836927978589E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17456375168351E+00 0.18790665319477E+00 0.32700394025816E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    210.01155388
 0.68791362062718E+00 0.29686118285495E+03 0.43739462990133E+03 0.40516967244237E+03 0.39456382782652E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18459514932528E+00 0.00000000000000E+00 -.18171938287172E+02
 0.98395351695362E-03 0.65678143222390E+00 0.80000000000000E+04 0.30000000000000E+04 0.12180612312549E+02
 0.45677296172059E+01 0.30776828353257E+03 0.29215000000015E+03 0.30711846350635E+03 0.32565748675989E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30494526610025E+03 0.32560897260525E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30711846350635E+03 0.32565748675989E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30494526610025E+03 0.32560897260525E+03 0.29215000000006E+03 0.29215000000006E+03 0.36331671686232E+03
 0.29458195094777E+03 0.14634009540786E+04 0.14197991887904E+04 0.74829887888589E+03 0.15354221398200E+04
 0.78338176653964E+03 0.93727253953433E+03 0.10680120514761E+04 0.90326498476713E+03 0.19528671521348E+04
 0.81535409810541E+03 0.10661555278804E+04 0.79039563577139E+03 0.19514914509488E+04 0.93727253953433E+03
 0.10680120514761E+04 0.90326498476713E+03 0.19528671521348E+04 0.81535409810542E+03 0.10661555278804E+04
 0.79039563577139E+03 0.19514914509488E+04 0.17301002765130E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49738511382872E+03 0.12948383791294E+01
 0.12948383791294E+01 0.67533995362622E+00 0.27033217428576E+00 0.29265411522691E+03 0.32717347204857E+03
 0.32095234352554E+03 0.32036331208550E+03 0.23000000000000E+00 0.00000000000000E+00 0.21318567873944E+00
 0.00000000000000E+00 -.11918612000257E+02 0.99786662375778E-03 0.31059929787129E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25756658353153E+02 0.96587468824323E+01 0.29458016890222E+03 0.36335824474168E+03
 0.29277135592317E+03 0.29658368600123E+03 0.29215000000006E+03 0.29215000000006E+03 0.29276280921602E+03
 0.29658480867853E+03 0.29215000000006E+03 0.29215000000006E+03 0.29277135592317E+03 0.29658368600123E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29276280921602E+03 0.29658480867853E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29586000682697E+03 0.29215000000010E+03 0.74816501740915E+02 0.70429535331259E+02
 0.75531883149019E+02 0.35014818854383E+03 0.27423864597906E+03 0.54411806882973E+02 0.72010167536959E+02
 0.54283067625016E+02 0.28013489011692E+03 0.53725743351642E+02 0.72070093294087E+02 0.53608646040234E+02
 0.28018558815204E+03 0.54411806882973E+02 0.72010167536956E+02 0.54283067625016E+02 0.28013489011691E+03
 0.53725743351642E+02 0.72070093294087E+02 0.53608646040234E+02 0.28018558815204E+03 0.17275851982811E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34941614926235E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17137708828122E+00 0.00000000000000E+00 0.00000000000000E+00 0.17137708828122E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17927098111774E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17927098111774E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455789709325E+00 0.18780258715544E+00 0.32717347204857E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    220.00414539
 0.68992593513339E+00 0.29722854373211E+03 0.43770167509449E+03 0.40539632292239E+03 0.39479225989929E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18260100068840E+00 0.00000000000000E+00 -.18308334443093E+02
 0.98273607231299E-03 0.68516315610053E+00 0.80000000000000E+04 0.30000000000000E+04 0.11676051067209E+02
 0.43785191502035E+01 0.30847893154354E+03 0.29215000000019E+03 0.30776356749527E+03 0.32691554358507E+03
 0.29215000000006E+03 0.29215000000006E+03 0.30553246607437E+03 0.32686790425112E+03 0.29215000000006E+03
 0.29215000000006E+03 0.30776356749527E+03 0.32691554358507E+03 0.29215000000006E+03 0.29215000000006E+03
 0.30553246607437E+03 0.32686790425112E+03 0.29215000000006E+03 0.29215000000006E+03 0.36523233323432E+03
 0.29491827671511E+03 0.14765673773257E+04 0.14311747202086E+04 0.74839062798352E+03 0.15145821195537E+04
 0.76244953843021E+03 0.94566643004196E+03 0.10787951779725E+04 0.91049110886798E+03 0.19540644551846E+04
 0.82398096904128E+03 0.10770523606985E+04 0.79816180461438E+03 0.19527924176689E+04 0.94566643004196E+03
 0.10787951779725E+04 0.91049110886798E+03 0.19540644551846E+04 0.82398096904128E+03 0.10770523606985E+04
 0.79816180461438E+03 0.19527924176689E+04 0.17289876591938E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49750457088643E+03 0.12948393840792E+01
 0.12948393840792E+01 0.71531031966509E+00 0.24832028869229E+00 0.29280529418031E+03 0.32727356729744E+03
 0.32156745294333E+03 0.32101765765149E+03 0.23000000000000E+00 0.00000000000000E+00 0.21191024953660E+00
 0.00000000000000E+00 -.12061368910259E+02 0.99735000718914E-03 0.33622965744918E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23793261013000E+02 0.89224728798752E+01 0.29491637798524E+03 0.36527325954233E+03
 0.29284342747055E+03 0.29680282370799E+03 0.29215000000006E+03 0.29215000000006E+03 0.29283419848075E+03
 0.29680393039825E+03 0.29215000000006E+03 0.29215000000006E+03 0.29284342747055E+03 0.29680282370799E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29283419848075E+03 0.29680393039825E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29604995420362E+03 0.29215000000010E+03 0.79426975857242E+02 0.74471634774595E+02
 0.81069987023340E+02 0.35489079591933E+03 0.27341545896087E+03 0.58368280863965E+02 0.77277174974150E+02
 0.58368280863965E+02 0.28390675256929E+03 0.57668663941415E+02 0.77330074856470E+02 0.57668663941415E+02
 0.28395058452110E+03 0.58368280863965E+02 0.77277174974146E+02 0.58368280863965E+02 0.28390675256928E+03
 0.57668663941415E+02 0.77330074856470E+02 0.57668663941415E+02 0.28395058452110E+03 0.17700834421540E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34951428862950E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17244919121025E+00 0.00000000000000E+00 0.00000000000000E+00 0.17244919121025E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18010297018049E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18010297018049E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455432969815E+00 0.18774104698838E+00 0.32727356729744E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    230.00620117
 0.69153130279214E+00 0.29759078498713E+03 0.43797436143980E+03 0.40561448226815E+03 0.39502006870454E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18069858415188E+00 0.00000000000000E+00 -.18389224851322E+02
 0.98153905682335E-03 0.71223585187262E+00 0.80000000000000E+04 0.30000000000000E+04 0.11232234349010E+02
 0.42120878808787E+01 0.30916221469416E+03 0.29215000000029E+03 0.30838537573463E+03 0.32811348151499E+03
 0.29215000000006E+03 0.29215000000007E+03 0.30609872255027E+03 0.32806669571062E+03 0.29215000000006E+03
 0.29215000000007E+03 0.30838537573463E+03 0.32811348151499E+03 0.29215000000006E+03 0.29215000000007E+03
 0.30609872255027E+03 0.32806669571062E+03 0.29215000000006E+03 0.29215000000007E+03 0.36702108601876E+03
 0.29527567763814E+03 0.14881841222966E+04 0.14411051086558E+04 0.74755603892150E+03 0.14941775503740E+04
 0.74288373125793E+03 0.95308041114552E+03 0.10878751663896E+04 0.91680715200953E+03 0.19538536828105E+04
 0.83163962817785E+03 0.10862354909389E+04 0.80501699510391E+03 0.19526750471236E+04 0.95308041114552E+03
 0.10878751663896E+04 0.91680715200953E+03 0.19538536828105E+04 0.83163962817784E+03 0.10862354909389E+04
 0.80501699510391E+03 0.19526750471236E+04 0.17269478412387E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49763112082383E+03 0.12948399800705E+01
 0.12948399800705E+01 0.75531854280603E+00 0.22726834934963E+00 0.29298926244454E+03 0.32731726389448E+03
 0.32211615174382E+03 0.32160811315253E+03 0.23000000000000E+00 0.00000000000000E+00 0.21062028629791E+00
 0.00000000000000E+00 -.12145988878251E+02 0.99672293728623E-03 0.36208638690362E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22094175007273E+02 0.82853156277276E+01 0.29527368794345E+03 0.36706140764483E+03
 0.29291673947250E+03 0.29701387835396E+03 0.29215000000006E+03 0.29215000000006E+03 0.29290690309824E+03
 0.29701495734438E+03 0.29215000000006E+03 0.29215000000006E+03 0.29291673947250E+03 0.29701387835396E+03
 0.29215000000006E+03 0.29215000000006E+03 0.29290690309824E+03 0.29701495734438E+03 0.29215000000006E+03
 0.29215000000006E+03 0.29623261118291E+03 0.29215000000012E+03 0.83638391284113E+02 0.78136254240267E+02
 0.86294070228268E+02 0.35881908604460E+03 0.27209354546520E+03 0.62203141082336E+02 0.82226581595800E+02
 0.62203141082336E+02 0.28701730534082E+03 0.61497354505048E+02 0.82271810462822E+02 0.61583939226497E+02
 0.28705372243112E+03 0.62203141082336E+02 0.82226581595800E+02 0.62203141082336E+02 0.28701730534082E+03
 0.61497354505048E+02 0.82271810462822E+02 0.61583939226497E+02 0.28705372243112E+03 0.18074829440231E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34957314427444E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17303134564848E+00 0.00000000000000E+00 0.00000000000000E+00 0.17303134564848E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18083384314150E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18083384314150E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455256570407E+00 0.18771392986420E+00 0.32731726389448E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    240.03276540
 0.69275561901627E+00 0.29794811091888E+03 0.43824149712612E+03 0.40584515325749E+03 0.39526386564890E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17887929548339E+00 0.00000000000000E+00 -.18441891335957E+02
 0.98036139803564E-03 0.73808384573994E+00 0.80000000000000E+04 0.30000000000000E+04 0.10838876973361E+02
 0.40645788650102E+01 0.30982302960455E+03 0.29215000000048E+03 0.30898799786488E+03 0.32926122839764E+03
 0.29215000000007E+03 0.29215000000009E+03 0.30664786179521E+03 0.32921527801636E+03 0.29215000000007E+03
 0.29215000000009E+03 0.30898799786488E+03 0.32926122839764E+03 0.29215000000007E+03 0.29215000000009E+03
 0.30664786179521E+03 0.32921527801636E+03 0.29215000000007E+03 0.29215000000009E+03 0.36870242636742E+03
 0.29565546180805E+03 0.14987324199147E+04 0.14500493285337E+04 0.74622995332405E+03 0.14747134412476E+04
 0.72475233815691E+03 0.95980415434663E+03 0.10957995256450E+04 0.92248727815652E+03 0.19529369130988E+04
 0.83861426231633E+03 0.10942536859999E+04 0.81123190337790E+03 0.19518426471107E+04 0.95980415434663E+03
 0.10957995256450E+04 0.92248727815652E+03 0.19529369130988E+04 0.83861426231633E+03 0.10942536859999E+04
 0.81123190337790E+03 0.19518426471107E+04 0.17245674556568E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49778875674557E+03 0.12948403681116E+01
 0.12948403681116E+01 0.79542479970599E+00 0.20716114399254E+00 0.29321017585439E+03 0.32732072830633E+03
 0.32260980759423E+03 0.32214492163721E+03 0.23000000000000E+00 0.00000000000000E+00 0.20931736162677E+00
 0.00000000000000E+00 -.12198942829968E+02 0.99597145533996E-03 0.38814578710344E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20610812395261E+02 0.77290546482230E+01 0.29565339000372E+03 0.36874202574370E+03
 0.29299240718844E+03 0.29721772455849E+03 0.29215000000007E+03 0.29215000000007E+03 0.29298202053137E+03
 0.29721876406612E+03 0.29215000000007E+03 0.29215000000007E+03 0.29299240718844E+03 0.29721772455849E+03
 0.29215000000007E+03 0.29215000000007E+03 0.29298202053137E+03 0.29721876406612E+03 0.29215000000007E+03
 0.29215000000007E+03 0.29640888969144E+03 0.29215000000016E+03 0.87448248422878E+02 0.81434122675302E+02
 0.91213662496959E+02 0.36210020863566E+03 0.27043047782622E+03 0.65920983516011E+02 0.86869935040394E+02
 0.66219089345208E+02 0.28960050006369E+03 0.65216170567855E+02 0.86906967378606E+02 0.65532168628422E+02
 0.28962907187611E+03 0.65920983516011E+02 0.86869935040394E+02 0.66219089345208E+02 0.28960050006369E+03
 0.65216170567855E+02 0.86906967378606E+02 0.65532168628422E+02 0.28962907187611E+03 0.18406477065294E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34960912223317E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17335712939814E+00 0.00000000000000E+00 0.00000000000000E+00 0.17335712939814E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18147135992568E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18147135992568E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455178934904E+00 0.18771101004094E+00 0.32732072830633E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    250.00223559
 0.69365486809684E+00 0.29829742299141E+03 0.43851690919023E+03 0.40609559945562E+03 0.39552783379213E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17715130786599E+00 0.00000000000000E+00 -.18479938440047E+02
 0.97921300805152E-03 0.76256812128699E+00 0.80000000000000E+04 0.30000000000000E+04 0.10490866031088E+02
 0.39340747616578E+01 0.31045925835933E+03 0.29215000000080E+03 0.30956922231688E+03 0.33035709562516E+03
 0.29215000000008E+03 0.29215000000011E+03 0.30717787997535E+03 0.33031195333317E+03 0.29215000000008E+03
 0.29215000000011E+03 0.30956922231688E+03 0.33035709562516E+03 0.29215000000008E+03 0.29215000000011E+03
 0.30717787997535E+03 0.33031195333317E+03 0.29215000000008E+03 0.29215000000011E+03 0.37028026956136E+03
 0.29605442746582E+03 0.15084415775731E+04 0.14582333200229E+04 0.74468027614733E+03 0.14565453141979E+04
 0.70814163666981E+03 0.96597883363142E+03 0.11028747131220E+04 0.92766986925855E+03 0.19517585050357E+04
 0.84504040232437E+03 0.11014137092646E+04 0.81693845178328E+03 0.19507399815294E+04 0.96597883363142E+03
 0.11028747131220E+04 0.92766986925855E+03 0.19517585050357E+04 0.84504040232437E+03 0.11014137092646E+04
 0.81693845178328E+03 0.19507399815294E+04 0.17221964854394E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49798451492102E+03 0.12948406484387E+01
 0.12948406484387E+01 0.83530268047259E+00 0.18816133979993E+00 0.29346999147744E+03 0.32729775911405E+03
 0.32305437372676E+03 0.32263269991199E+03 0.23000000000000E+00 0.00000000000000E+00 0.20801503857330E+00
 0.00000000000000E+00 -.12233619125834E+02 0.99508935856820E-03 0.41414132378691E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19317077385198E+02 0.72439040194492E+01 0.29605227717784E+03 0.37031898789828E+03
 0.29306958857921E+03 0.29741329172952E+03 0.29215000000007E+03 0.29215000000007E+03 0.29305873725461E+03
 0.29741428101977E+03 0.29215000000007E+03 0.29215000000007E+03 0.29306958857921E+03 0.29741329172952E+03
 0.29215000000007E+03 0.29215000000007E+03 0.29305873725461E+03 0.29741428101977E+03 0.29215000000007E+03
 0.29215000000007E+03 0.29657794836541E+03 0.29215000000022E+03 0.90833192481745E+02 0.84362909670244E+02
 0.95803442680363E+02 0.36485742022988E+03 0.26857496033611E+03 0.69501818284811E+02 0.91186149761024E+02
 0.70155927910374E+02 0.29175595844594E+03 0.68804451866713E+02 0.91214660409912E+02 0.69480870005153E+02
 0.29177644452222E+03 0.69501818284811E+02 0.91186149761024E+02 0.70155927910374E+02 0.29175595844594E+03
 0.68804451866713E+02 0.91214660409912E+02 0.69480870005153E+02 0.29177644452222E+03 0.18699234335091E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34963462427917E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17354422197145E+00 0.00000000000000E+00 0.00000000000000E+00 0.17354422197145E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18201631314448E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18201631314448E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455156754235E+00 0.18772388045592E+00 0.32729775911405E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    260.00372729
 0.69431228291646E+00 0.29864156331552E+03 0.43881029882937E+03 0.40637000724666E+03 0.39581448425084E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17549339004811E+00 0.00000000000000E+00 -.18513740601143E+02
 0.97808428323548E-03 0.78597866266930E+00 0.80000000000000E+04 0.30000000000000E+04 0.10178393358454E+02
 0.38168975094204E+01 0.31107909174022E+03 0.29215000000134E+03 0.31013632761645E+03 0.33141715170454E+03
 0.29215000000010E+03 0.29215000000015E+03 0.30769540205933E+03 0.33137279453632E+03 0.29215000000010E+03
 0.29215000000015E+03 0.31013632761645E+03 0.33141715170454E+03 0.29215000000010E+03 0.29215000000015E+03
 0.30769540205933E+03 0.33137279453632E+03 0.29215000000010E+03 0.29215000000015E+03 0.37178349516592E+03
 0.29647603191275E+03 0.15175934644604E+04 0.14659118315282E+04 0.74302897775598E+03 0.14395218198893E+04
 0.69277769724451E+03 0.97178547121539E+03 0.11093980681507E+04 0.93251779674048E+03 0.19505451423182E+04
 0.85109928885612E+03 0.11080145912757E+04 0.82230341411406E+03 0.19495953664018E+04 0.97178547121539E+03
 0.11093980681507E+04 0.93251779674048E+03 0.19505451423182E+04 0.85109928885612E+03 0.11080145912757E+04
 0.82230341411406E+03 0.19495953664018E+04 0.17199683367532E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49821954746728E+03 0.12948408974897E+01
 0.12948408974897E+01 0.87530864727896E+00 0.17009698821959E+00 0.29377465462991E+03 0.32725919593154E+03
 0.32346211618000E+03 0.32308329774362E+03 0.23000000000000E+00 0.00000000000000E+00 0.20670385381833E+00
 0.00000000000000E+00 -.12260671120516E+02 0.10625851936206E-02 0.44026419780566E+00 0.75288080880753E+04
 0.28233030330282E+04 0.18170907468455E+02 0.68140903006705E+01 0.29647380764617E+03 0.37182117652618E+03
 0.29314918465599E+03 0.29760294971724E+03 0.29215000000007E+03 0.29215000000008E+03 0.29313794210187E+03
 0.29760387860802E+03 0.29215000000007E+03 0.29215000000008E+03 0.29314918465599E+03 0.29760294971724E+03
 0.29215000000007E+03 0.29215000000008E+03 0.29313794210187E+03 0.29760387860802E+03 0.29215000000007E+03
 0.29215000000008E+03 0.29674184968801E+03 0.29215000000033E+03 0.93834604972433E+02 0.86976439314204E+02
 0.10012374500871E+03 0.36724155394606E+03 0.26661719021230E+03 0.72985154968806E+02 0.95234937053035E+02
 0.74157066321820E+02 0.29360488115905E+03 0.72301252230063E+02 0.95254675821416E+02 0.73499671724458E+02
 0.29361711046179E+03 0.72985154968806E+02 0.95234937053035E+02 0.74157066321820E+02 0.29360488115905E+03
 0.72301252230063E+02 0.95254675821416E+02 0.73499671724457E+02 0.29361711046180E+03 0.18962065569078E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34965954578767E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17368201620803E+00 0.00000000000000E+00 0.00000000000000E+00 0.17368201620803E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18248601847799E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18248601847799E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455156806622E+00 0.18774595165563E+00 0.32725919593154E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    270.01719784
 0.69479155584133E+00 0.29897958819497E+03 0.43912347646182E+03 0.40666654640496E+03 0.39612097318601E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17390447951756E+00 0.00000000000000E+00 -.18547077337220E+02
 0.97697814437290E-03 0.80832664589472E+00 0.80000000000000E+04 0.30000000000000E+04 0.98969890954726E+01
 0.37113709108022E+01 0.31168320248893E+03 0.29215000000223E+03 0.31068978462724E+03 0.33244400848008E+03
 0.29215000000013E+03 0.29215000000023E+03 0.30820085091377E+03 0.33240040997847E+03 0.29215000000012E+03
 0.29215000000023E+03 0.31068978462724E+03 0.33244400848008E+03 0.29215000000013E+03 0.29215000000023E+03
 0.30820085091377E+03 0.33240040997847E+03 0.29215000000012E+03 0.29215000000023E+03 0.37321963878563E+03
 0.29691952503029E+03 0.15262858898337E+04 0.14731749223142E+04 0.74135254930615E+03 0.14236268270321E+04
 0.67856751497944E+03 0.97729015803275E+03 0.11154979718889E+04 0.93709168605556E+03 0.19494163194324E+04
 0.85685541995800E+03 0.11141853197576E+04 0.82738624984389E+03 0.19485289326074E+04 0.97729015803275E+03
 0.11154979718889E+04 0.93709168605556E+03 0.19494163194324E+04 0.85685541995800E+03 0.11141853197576E+04
 0.82738624984389E+03 0.19485289326074E+04 0.17179498483095E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49848934314426E+03 0.12948411431116E+01
 0.12948411431116E+01 0.91536252945545E+00 0.15300810046870E+00 0.29412748253790E+03 0.32721370555537E+03
 0.32383873213165E+03 0.32350167717376E+03 0.23000000000000E+00 0.00000000000000E+00 0.20538807134699E+00
 0.00000000000000E+00 -.12284295416982E+02 0.11812610300150E-02 0.46643027928087E+00 0.67724235344481E+04
 0.25396588254180E+04 0.17151545161978E+02 0.64318294357419E+01 0.29691723265257E+03 0.37325614346887E+03
 0.29323128931923E+03 0.29778688807964E+03 0.29215000000008E+03 0.29215000000008E+03 0.29321973059289E+03
 0.29778774744215E+03 0.29215000000008E+03 0.29215000000008E+03 0.29323128931923E+03 0.29778688807964E+03
 0.29215000000008E+03 0.29215000000008E+03 0.29321973059289E+03 0.29778774744215E+03 0.29215000000008E+03
 0.29215000000008E+03 0.29690078322841E+03 0.29215000000049E+03 0.96460809367085E+02 0.89300420614710E+02
 0.10418673613552E+03 0.36935025976831E+03 0.26464258995212E+03 0.76375340443565E+02 0.99030228245100E+02
 0.78261472029542E+02 0.29522676286573E+03 0.75710364852033E+02 0.99041071804662E+02 0.77627099630940E+02
 0.29523068161798E+03 0.76375340443565E+02 0.99030228245103E+02 0.78261472029542E+02 0.29522676286574E+03
 0.75710364852033E+02 0.99041071804662E+02 0.77627099630940E+02 0.29523068161798E+03 0.19199735990090E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34969100107588E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17380514237683E+00 0.00000000000000E+00 0.00000000000000E+00 0.17380514237683E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18289010877704E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18289010877704E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455164505109E+00 0.18777209175956E+00 0.32721370555537E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    280.00009321
 0.69514489331599E+00 0.29930996799061E+03 0.43945451603999E+03 0.40698092707246E+03 0.39644254325073E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17238693138385E+00 0.00000000000000E+00 -.18581693621992E+02
 0.97589941769081E-03 0.82958097145842E+00 0.80000000000000E+04 0.30000000000000E+04 0.96434227341736E+01
 0.36162835253151E+01 0.31227077509289E+03 0.29215000000365E+03 0.31122872872724E+03 0.33343750570075E+03
 0.29215000000019E+03 0.29215000000035E+03 0.30869342506228E+03 0.33339463545600E+03 0.29215000000016E+03
 0.29215000000035E+03 0.31122872872724E+03 0.33343750570075E+03 0.29215000000019E+03 0.29215000000035E+03
 0.30869342506228E+03 0.33339463545600E+03 0.29215000000016E+03 0.29215000000035E+03 0.37459152418731E+03
 0.29738289090712E+03 0.15345577664772E+04 0.14800583979625E+04 0.73969138309096E+03 0.14088260713452E+04
 0.66543623133877E+03 0.98252206589312E+03 0.11212379239294E+04 0.94141835780096E+03 0.19484203755853E+04
 0.86233640325335E+03 0.11199898263003E+04 0.83221199197414E+03 0.19475894861334E+04 0.98252206589312E+03
 0.11212379239294E+04 0.94141835780096E+03 0.19484203755853E+04 0.86233640325335E+03 0.11199898263003E+04
 0.83221199197414E+03 0.19475894861334E+04 0.17161556897552E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49878702154883E+03 0.12948413981612E+01
 0.12948413981612E+01 0.95529411092950E+00 0.13695881066593E+00 0.29452988929388E+03 0.32716819446746E+03
 0.32418812549498E+03 0.32389104774337E+03 0.23000000000000E+00 0.00000000000000E+00 0.20407451879880E+00
 0.00000000000000E+00 -.12306762459484E+02 0.13196848939111E-02 0.49250305110029E+00 0.60620531741414E+04
 0.22732699403030E+04 0.16243554191446E+02 0.60913328217922E+01 0.29738053714148E+03 0.37462673795531E+03
 0.29331588099730E+03 0.29796496877114E+03 0.29215000000008E+03 0.29215000000009E+03 0.29330408233315E+03
 0.29796575079527E+03 0.29215000000008E+03 0.29215000000009E+03 0.29331588099730E+03 0.29796496877114E+03
 0.29215000000008E+03 0.29215000000009E+03 0.29330408233315E+03 0.29796575079527E+03 0.29215000000008E+03
 0.29215000000009E+03 0.29705464433305E+03 0.29215000000074E+03 0.98720650795565E+02 0.91358410948310E+02
 0.10799944138422E+03 0.37126055246539E+03 0.26272111387425E+03 0.79670129245040E+02 0.10258110609805E+03
 0.82502318958597E+02 0.29668485388024E+03 0.79028887358054E+02 0.10258306895157E+03 0.81895682280539E+02
 0.29668053470610E+03 0.79670129245040E+02 0.10258110609806E+03 0.82502318958597E+02 0.29668485388025E+03
 0.79028887358054E+02 0.10258306895157E+03 0.81895682280539E+02 0.29668053470610E+03 0.19416005261769E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34973405459080E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17393145993978E+00 0.00000000000000E+00 0.00000000000000E+00 0.17393145993978E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18323788908418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18323788908418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455171255715E+00 0.18779824296915E+00 0.32716819446746E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    290.00969980
 0.69541515370233E+00 0.29963458464962E+03 0.43980334628854E+03 0.40731151931541E+03 0.39677751717730E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17092837717770E+00 0.00000000000000E+00 -.18618308191490E+02
 0.97484180019472E-03 0.84991967281483E+00 0.80000000000000E+04 0.30000000000000E+04 0.94126542259046E+01
 0.35297453347142E+01 0.31284661316483E+03 0.29215000000589E+03 0.31175747456128E+03 0.33440672717440E+03
 0.29215000000028E+03 0.29215000000056E+03 0.30917705470750E+03 0.33436455874785E+03 0.29215000000023E+03
 0.29215000000056E+03 0.31175747456128E+03 0.33440672717440E+03 0.29215000000028E+03 0.29215000000056E+03
 0.30917705470750E+03 0.33436455874785E+03 0.29215000000023E+03 0.29215000000056E+03 0.37591400811339E+03
 0.29786861758044E+03 0.15425046347683E+04 0.14866425843057E+04 0.73804660892524E+03 0.13949220135522E+04
 0.65318517158237E+03 0.98754556692352E+03 0.11267045170245E+04 0.94555202562338E+03 0.19475549911073E+04
 0.86760786835101E+03 0.11255157082650E+04 0.83683825695273E+03 0.19467756597017E+04 0.98754556692352E+03
 0.11267045170245E+04 0.94555202562339E+03 0.19475549911073E+04 0.86760786835101E+03 0.11255157082650E+04
 0.83683825695273E+03 0.19467756597017E+04 0.17145551099000E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49910858481275E+03 0.12948416679341E+01
 0.12948416679341E+01 0.99533253732598E+00 0.12184839162853E+00 0.29498656982547E+03 0.32712744711171E+03
 0.32451657097647E+03 0.32425749894368E+03 0.23000000000000E+00 0.00000000000000E+00 0.20275654985060E+00
 0.00000000000000E+00 -.12329175030431E+02 0.14833387620921E-02 0.51861295043874E+00 0.53932386885898E+04
 0.20224645082212E+04 0.15425762108779E+02 0.57846607907922E+01 0.29786620959858E+03 0.37594783512061E+03
 0.29340385730196E+03 0.29813882642786E+03 0.29215000000008E+03 0.29215000000011E+03 0.29339189285876E+03
 0.29813952388019E+03 0.29215000000008E+03 0.29215000000011E+03 0.29340385730196E+03 0.29813882642785E+03
 0.29215000000008E+03 0.29215000000011E+03 0.29339189285876E+03 0.29813952388019E+03 0.29215000000008E+03
 0.29215000000011E+03 0.29720485348907E+03 0.29215000000115E+03 0.10064591954694E+03 0.93191877982605E+02
 0.11160711683153E+03 0.37305169869944E+03 0.26088654628375E+03 0.82898834131895E+02 0.10593205308603E+03
 0.86957632688701E+02 0.29804350636492E+03 0.82285730692936E+02 0.10592518736200E+03 0.86383066164554E+02
 0.29803105456249E+03 0.82898834131895E+02 0.10593205308604E+03 0.86957632688701E+02 0.29804350636493E+03
 0.82285730692936E+02 0.10592518736200E+03 0.86383066164554E+02 0.29803105456249E+03 0.19616044183431E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34979262298289E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17406817718454E+00 0.00000000000000E+00 0.00000000000000E+00 0.17406817718454E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18354110295061E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18354110295061E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455173039440E+00 0.18782161295391E+00 0.32712744711171E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    300.01359996
 0.69563042715568E+00 0.29995243430751E+03 0.44016639435456E+03 0.40765402871432E+03 0.39712168478515E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16953060048806E+00 0.00000000000000E+00 -.18656666580602E+02
 0.97380842391291E-03 0.86932384341495E+00 0.80000000000000E+04 0.30000000000000E+04 0.92025544457330E+01
 0.34509579171499E+01 0.31341000482647E+03 0.29215000000931E+03 0.31227529641981E+03 0.33535120784926E+03
 0.29215000000042E+03 0.29215000000090E+03 0.30965106321266E+03 0.33530971266426E+03 0.29215000000034E+03
 0.29215000000090E+03 0.31227529641981E+03 0.33535120784926E+03 0.29215000000042E+03 0.29215000000090E+03
 0.30965106321266E+03 0.33530971266426E+03 0.29215000000034E+03 0.29215000000090E+03 0.37718821105204E+03
 0.29837493868689E+03 0.15501323729681E+04 0.14929322030446E+04 0.73642314040155E+03 0.13818532826572E+04
 0.64174802655363E+03 0.99236740732693E+03 0.11319121763446E+04 0.94949852107760E+03 0.19468073683765E+04
 0.87267574465615E+03 0.11307777364430E+04 0.84126983231849E+03 0.19460750177356E+04 0.99236740732693E+03
 0.11319121763446E+04 0.94949852107761E+03 0.19468073683765E+04 0.87267574465615E+03 0.11307777364430E+04
 0.84126983231849E+03 0.19460750177356E+04 0.17131232796618E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49944798788089E+03 0.12948419505555E+01
 0.12948419505555E+01 0.10353481379488E+01 0.10771888806984E+00 0.29549781030033E+03 0.32709528839597E+03
 0.32482619159178E+03 0.32460269819923E+03 0.23000000000000E+00 0.00000000000000E+00 0.20143906327598E+00
 0.00000000000000E+00 -.12351714286425E+02 0.16779082693700E-02 0.54466105285377E+00 0.47678410947959E+04
 0.17879404105485E+04 0.14688033884714E+02 0.55080127067676E+01 0.29837248459211E+03 0.37722058612012E+03
 0.29349543642585E+03 0.29830845957496E+03 0.29215000000008E+03 0.29215000000015E+03 0.29348337982274E+03
 0.29830906647192E+03 0.29215000000008E+03 0.29215000000015E+03 0.29349543642585E+03 0.29830845957495E+03
 0.29215000000008E+03 0.29215000000015E+03 0.29348337982274E+03 0.29830906647192E+03 0.29215000000008E+03
 0.29215000000015E+03 0.29735141340169E+03 0.29215000000178E+03 0.10224775639235E+03 0.94819810939928E+02
 0.11501944921592E+03 0.37477090577768E+03 0.25917635931568E+03 0.86059790135649E+02 0.10909412767091E+03
 0.91670346758515E+02 0.29934195634667E+03 0.85478639669666E+02 0.10907858941748E+03 0.91131571640726E+02
 0.29932157083286E+03 0.86059790135649E+02 0.10909412767092E+03 0.91670346758515E+02 0.29934195634669E+03
 0.85478639669666E+02 0.10907858941748E+03 0.91131571640726E+02 0.29932157083286E+03 0.19802341445273E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34986901419337E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17421538744602E+00 0.00000000000000E+00 0.00000000000000E+00 0.17421538744602E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18380695730355E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18380695730355E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455168227148E+00 0.18783998524207E+00 0.32709528839597E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    310.00006301
 0.69581072742530E+00 0.30026330487117E+03 0.44054064804922E+03 0.40800515464988E+03 0.39747196083871E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16819228116874E+00 0.00000000000000E+00 -.18696361642835E+02
 0.97279983304833E-03 0.88781894735553E+00 0.80000000000000E+04 0.30000000000000E+04 0.90108462134413E+01
 0.33790673300405E+01 0.31396133853516E+03 0.29215000001444E+03 0.31278248805756E+03 0.33627219386070E+03
 0.29215000000066E+03 0.29215000000145E+03 0.31011570339193E+03 0.33623134317394E+03 0.29215000000053E+03
 0.29215000000145E+03 0.31278248805756E+03 0.33627219386070E+03 0.29215000000066E+03 0.29215000000145E+03
 0.31011570339193E+03 0.33623134317394E+03 0.29215000000053E+03 0.29215000000145E+03 0.37841733960326E+03
 0.29890080980180E+03 0.15574575445091E+04 0.14989409432124E+04 0.73481761891587E+03 0.13695329731650E+04
 0.63104126615460E+03 0.99700019967772E+03 0.11368773162048E+04 0.95326850019393E+03 0.19461547759708E+04
 0.87755242491670E+03 0.11357927649320E+04 0.84551737376537E+03 0.19454652641340E+04 0.99700019967772E+03
 0.11368773162048E+04 0.95326850019394E+03 0.19461547759708E+04 0.87755242491670E+03 0.11357927649320E+04
 0.84551737376537E+03 0.19454652641340E+04 0.17118293687182E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49980053278583E+03 0.12948422430255E+01
 0.12948422430255E+01 0.10752939901562E+01 0.94571921460286E-01 0.29606341780279E+03 0.32707429872954E+03
 0.32511912633259E+03 0.32492850426686E+03 0.23000000000000E+00 0.00000000000000E+00 0.20012404811230E+00
 0.00000000000000E+00 -.12368410490422E+02 0.19111632999964E-02 0.57060655935859E+00 0.41859322016151E+04
 0.15697245756056E+04 0.14020168308252E+02 0.52575631155945E+01 0.29889831833763E+03 0.37844822448181E+03
 0.29359106486553E+03 0.29847421551158E+03 0.29215000000008E+03 0.29215000000020E+03 0.29357898768650E+03
 0.29847472696107E+03 0.29215000000008E+03 0.29215000000020E+03 0.29359106486553E+03 0.29847421551158E+03
 0.29215000000008E+03 0.29215000000020E+03 0.29357898768650E+03 0.29847472696107E+03 0.29215000000008E+03
 0.29215000000020E+03 0.29749462493517E+03 0.29215000000273E+03 0.10354312524071E+03 0.96261442928433E+02
 0.11825336669971E+03 0.37645726278763E+03 0.25761262925442E+03 0.89156859273355E+02 0.11208488082438E+03
 0.96693496699470E+02 0.30061269094503E+03 0.88610933838696E+02 0.11206089843148E+03 0.96193676439118E+02
 0.30058463423519E+03 0.89156859273355E+02 0.11208488082439E+03 0.96693496699470E+02 0.30061269094505E+03
 0.88610933838696E+02 0.11206089843148E+03 0.96193676439118E+02 0.30058463423519E+03 0.19977235459886E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34996457284696E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17437122654125E+00 0.00000000000000E+00 0.00000000000000E+00 0.17437122654125E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18404245932276E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18404245932276E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455173646140E+00 0.18785206703584E+00 0.32707429872954E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    320.09161398
 0.69597149290864E+00 0.30057110531802E+03 0.44092843366870E+03 0.40836686721774E+03 0.39783052791006E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16689527752444E+00 0.00000000000000E+00 -.18737983856613E+02
 0.97180323614601E-03 0.90566159701019E+00 0.80000000000000E+04 0.30000000000000E+04 0.88333214375104E+01
 0.33124955390664E+01 0.31450805781769E+03 0.29215000002210E+03 0.31328584993032E+03 0.33718263681016E+03
 0.29215000000104E+03 0.29215000000233E+03 0.31057718562101E+03 0.33714240975384E+03 0.29215000000082E+03
 0.29215000000232E+03 0.31328584993032E+03 0.33718263681016E+03 0.29215000000104E+03 0.29215000000233E+03
 0.31057718562101E+03 0.33714240975384E+03 0.29215000000082E+03 0.29215000000232E+03 0.37962012191464E+03
 0.29945241094776E+03 0.15645864038546E+04 0.15047559670326E+04 0.73319913203818E+03 0.13577209812694E+04
 0.62085585357108E+03 0.10015125834509E+04 0.11416729942288E+04 0.95691805440546E+03 0.19455649354636E+04
 0.88230963344598E+03 0.11406348389567E+04 0.84964327736125E+03 0.19449150163528E+04 0.10015125834509E+04
 0.11416729942288E+04 0.95691805440546E+03 0.19455649354636E+04 0.88230963344598E+03 0.11406348389567E+04
 0.84964327736125E+03 0.19449150163528E+04 0.17106268102056E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50016720416904E+03 0.12948425496948E+01
 0.12948425496948E+01 0.11156601940359E+01 0.82246159899940E-01 0.29669044216450E+03 0.32706607860158E+03
 0.32540055896727E+03 0.32524028979433E+03 0.23000000000000E+00 0.00000000000000E+00 0.19879567978904E+00
 0.00000000000000E+00 -.12378163509667E+02 0.21975780951962E-02 0.59675947555397E+00 0.36403711965857E+04
 0.13651391987196E+04 0.13405736025513E+02 0.50271510095673E+01 0.29944989098276E+03 0.37964948161351E+03
 0.29369254402534E+03 0.29863856609667E+03 0.29215000000008E+03 0.29215000000029E+03 0.29368051561224E+03
 0.29863897699175E+03 0.29215000000008E+03 0.29215000000029E+03 0.29369254402534E+03 0.29863856609667E+03
 0.29215000000008E+03 0.29215000000029E+03 0.29368051561224E+03 0.29863897699175E+03 0.29215000000008E+03
 0.29215000000029E+03 0.29763661433085E+03 0.29215000000418E+03 0.10456090125633E+03 0.97546515024953E+02
 0.12136448230602E+03 0.37816162479576E+03 0.25619032007821E+03 0.92232398044235E+02 0.11495732921172E+03
 0.10215677679503E+03 0.30189636125837E+03 0.91724951871260E+02 0.11492508970667E+03 0.10169909612606E+03
 0.30186085643945E+03 0.92232398044235E+02 0.11495732921173E+03 0.10215677679503E+03 0.30189636125839E+03
 0.91724951871260E+02 0.11492508970667E+03 0.10169909612606E+03 0.30186085643945E+03 0.20144626080839E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35008141672266E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17453962243346E+00 0.00000000000000E+00 0.00000000000000E+00 0.17453962243346E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18425528690409E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18425528690409E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455192729535E+00 0.18785697559755E+00 0.32706607860158E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    330.04106107
 0.69611716088421E+00 0.30086852242398E+03 0.44131807175039E+03 0.40872829124220E+03 0.39818686474137E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16566872550854E+00 0.00000000000000E+00 -.18778584810166E+02
 0.97084219187224E-03 0.92245831745100E+00 0.80000000000000E+04 0.30000000000000E+04 0.86724785810443E+01
 0.32521794678916E+01 0.31503751784491E+03 0.29215000003306E+03 0.31377369332370E+03 0.33806182725324E+03
 0.29215000000162E+03 0.29215000000364E+03 0.31102477738129E+03 0.33802218866361E+03 0.29215000000127E+03
 0.29215000000363E+03 0.31377369332370E+03 0.33806182725324E+03 0.29215000000162E+03 0.29215000000364E+03
 0.31102477738129E+03 0.33802218866361E+03 0.29215000000127E+03 0.29215000000363E+03 0.38077016216373E+03
 0.30001555931595E+03 0.15713631187383E+04 0.15102510872102E+04 0.73160423320843E+03 0.13466346836953E+04
 0.61137242932084E+03 0.10058066532229E+04 0.11461954247842E+04 0.96036876550422E+03 0.19450306292418E+04
 0.88684352683907E+03 0.11451994447723E+04 0.85355785170847E+03 0.19444164801501E+04 0.10058066532229E+04
 0.11461954247842E+04 0.96036876550422E+03 0.19450306292418E+04 0.88684352683907E+03 0.11451994447723E+04
 0.85355785170847E+03 0.19444164801501E+04 0.17095215313499E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50053622693251E+03 0.12948428488397E+01
 0.12948428488397E+01 0.11554579824022E+01 0.71022531722045E-01 0.29736169817304E+03 0.32707166523599E+03
 0.32566494718387E+03 0.32553168446547E+03 0.23000000000000E+00 0.00000000000000E+00 0.19748676175137E+00
 0.00000000000000E+00 -.12387498561937E+02 0.25448590892326E-02 0.62247327483639E+00 0.31435925210352E+04
 0.11788471953882E+04 0.12851957382592E+02 0.48194840184722E+01 0.30001302155313E+03 0.38079802489458E+03
 0.29379808237050E+03 0.29879791213478E+03 0.29215000000008E+03 0.29215000000043E+03 0.29378616775790E+03
 0.29879822070439E+03 0.29215000000008E+03 0.29215000000043E+03 0.29379808237050E+03 0.29879791213478E+03
 0.29215000000008E+03 0.29215000000043E+03 0.29378616775790E+03 0.29879822070439E+03 0.29215000000008E+03
 0.29215000000043E+03 0.29777427328694E+03 0.29215000000627E+03 0.10529259714933E+03 0.98652308780313E+02
 0.12429377685082E+03 0.37986340399998E+03 0.25494815826491E+03 0.95216033783420E+02 0.11765815998483E+03
 0.10799531243679E+03 0.30317915159163E+03 0.94748909442088E+02 0.11761807856343E+03 0.10758145735859E+03
 0.30313662344103E+03 0.95216033783420E+02 0.11765815998483E+03 0.10799531243679E+03 0.30317915159164E+03
 0.94748909442088E+02 0.11761807856343E+03 0.10758145735859E+03 0.30313662344103E+03 0.20301781327881E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35021657790859E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17470070514607E+00 0.00000000000000E+00 0.00000000000000E+00 0.17470070514607E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18444379192182E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18444379192182E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455205123749E+00 0.18785388292867E+00 0.32707166523599E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    340.01386150
 0.69625072597642E+00 0.30116105141322E+03 0.44171463903743E+03 0.40909445989678E+03 0.39854621212937E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16448928828405E+00 0.00000000000000E+00 -.18811052251067E+02
 0.96989886571163E-03 0.93853544480349E+00 0.80000000000000E+04 0.30000000000000E+04 0.85239188826534E+01
 0.31964695809950E+01 0.31555965847212E+03 0.29215000004868E+03 0.31425512449172E+03 0.33892615655342E+03
 0.29215000000247E+03 0.29215000000560E+03 0.31146685633900E+03 0.33888708459039E+03 0.29215000000193E+03
 0.29215000000558E+03 0.31425512449172E+03 0.33892615655342E+03 0.29215000000247E+03 0.29215000000560E+03
 0.31146685633900E+03 0.33888708459039E+03 0.29215000000193E+03 0.29215000000558E+03 0.38188881672761E+03
 0.30059846843206E+03 0.15779265296580E+04 0.15155409657369E+04 0.73002146188339E+03 0.13360559713499E+04
 0.60238440215706E+03 0.10099700270453E+04 0.11505402923359E+04 0.96369243246812E+03 0.19445327805186E+04
 0.89124565712768E+03 0.11495833624948E+04 0.85734085787030E+03 0.19439515282147E+04 0.10099700270453E+04
 0.11505402923359E+04 0.96369243246812E+03 0.19445327805186E+04 0.89124565712768E+03 0.11495833624948E+04
 0.85734085787030E+03 0.19439515282147E+04 0.17084898586849E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50091242497302E+03 0.12948430880576E+01
 0.12948430880576E+01 0.11953491841211E+01 0.60686863473201E-01 0.29809582201849E+03 0.32709132217769E+03
 0.32591822487136E+03 0.32580916447873E+03 0.23000000000000E+00 0.00000000000000E+00 0.19617570475421E+00
 0.00000000000000E+00 -.12387901877408E+02 0.29782773837404E-02 0.64817235826734E+00 0.26861164925991E+04
 0.10072936847247E+04 0.12342396120355E+02 0.46283985451330E+01 0.30059592136020E+03 0.38191518161084E+03
 0.29388635077559E+03 0.29895539641621E+03 0.29215000000008E+03 0.29215000000064E+03 0.29387578932585E+03
 0.29895559969997E+03 0.29215000000008E+03 0.29215000000064E+03 0.29388635077559E+03 0.29895539641620E+03
 0.29215000000008E+03 0.29215000000064E+03 0.29387578932585E+03 0.29895559969997E+03 0.29215000000008E+03
 0.29215000000064E+03 0.29791034489821E+03 0.29215000000927E+03 0.10576669721577E+03 0.99636533860651E+02
 0.12710490378315E+03 0.38159909420625E+03 0.25385866590419E+03 0.98280137965155E+02 0.12024751624733E+03
 0.98280137965155E+02 0.30448912643201E+03 0.97848451026431E+02 0.12019992249855E+03 0.97848451026431E+02
 0.30443992409251E+03 0.98280137965155E+02 0.12024751624734E+03 0.98280137965155E+02 0.30448912643203E+03
 0.97848451026431E+02 0.12019992249855E+03 0.97848451026431E+02 0.30443992409251E+03 0.20433269209566E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35036984427052E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17479038434856E+00 0.00000000000000E+00 0.00000000000000E+00 0.17479038434856E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18457084850129E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18457084850129E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455236576893E+00 0.18784293135843E+00 0.32709132217769E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    350.00267383
 0.69636581851364E+00 0.30144884585478E+03 0.44211652132395E+03 0.40946446766845E+03 0.39890798043881E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16335601479358E+00 0.00000000000000E+00 -.18853735482377E+02
 0.96897249105578E-03 0.95391108781509E+00 0.80000000000000E+04 0.30000000000000E+04 0.83865258536032E+01
 0.31449471951012E+01 0.31607448459746E+03 0.29215000007059E+03 0.31473010678637E+03 0.33977614922922E+03
 0.29215000000372E+03 0.29215000000847E+03 0.31190334606782E+03 0.33973762107549E+03 0.29215000000291E+03
 0.29215000000844E+03 0.31473010678637E+03 0.33977614922922E+03 0.29215000000372E+03 0.29215000000847E+03
 0.31190334606782E+03 0.33973762107549E+03 0.29215000000291E+03 0.29215000000844E+03 0.38297885648152E+03
 0.30120209782868E+03 0.15842934486064E+04 0.15206435268652E+04 0.72843528475474E+03 0.13259134817895E+04
 0.59383602061097E+03 0.10140119050542E+04 0.11547158861078E+04 0.96689920545027E+03 0.19440592263194E+04
 0.89552512534386E+03 0.11537951370046E+04 0.86100240954218E+03 0.19435082589726E+04 0.10140119050542E+04
 0.11547158861078E+04 0.96689920545027E+03 0.19440592263194E+04 0.89552512534386E+03 0.11537951370047E+04
 0.86100240954218E+03 0.19435082589726E+04 0.17075160794246E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50129491624926E+03 0.12948434025449E+01
 0.12948434025449E+01 0.12353044334306E+01 0.51250548548457E-01 0.29896033690072E+03 0.32712492624735E+03
 0.32616262581158E+03 0.32607534738970E+03 0.23000000000000E+00 0.00000000000000E+00 0.19486377854408E+00
 0.00000000000000E+00 -.12400308176072E+02 0.35266411204714E-02 0.67383140635619E+00 0.22684474338944E+04
 0.85066778771041E+03 0.11872405952790E+02 0.44521522322962E+01 0.30119954380646E+03 0.38300374973060E+03
 0.29393989441885E+03 0.29911117419890E+03 0.29215000000009E+03 0.29215000000094E+03 0.29392844531024E+03
 0.29911127033329E+03 0.29215000000009E+03 0.29215000000094E+03 0.29393989441885E+03 0.29911117419890E+03
 0.29215000000009E+03 0.29215000000094E+03 0.29392844531024E+03 0.29911127033329E+03 0.29215000000009E+03
 0.29215000000094E+03 0.29804493878549E+03 0.29215000001353E+03 0.10597929846478E+03 0.10069672664097E+03
 0.12980706314805E+03 0.38338480832532E+03 0.25292870986153E+03 0.10151884049931E+03 0.12273404609204E+03
 0.10151884049931E+03 0.30584050139476E+03 0.10113511253182E+03 0.12267926665484E+03 0.10113511253182E+03
 0.30578496264594E+03 0.10151884049931E+03 0.12273404609204E+03 0.10151884049931E+03 0.30584050139478E+03
 0.10113511253182E+03 0.12267926665484E+03 0.10113511253182E+03 0.30578496264594E+03 0.20582938913004E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35054755349532E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17496717446121E+00 0.00000000000000E+00 0.00000000000000E+00 0.17496717446121E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18473672005530E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18473672005530E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455226287127E+00 0.18782350137134E+00 0.32712492624735E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    360.00362772
 0.69648559532026E+00 0.30173173877335E+03 0.44252224337247E+03 0.40983605723542E+03 0.39926988355792E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16226768108068E+00 0.00000000000000E+00 -.18895063996772E+02
 0.96806362207343E-03 0.96860744867685E+00 0.80000000000000E+04 0.30000000000000E+04 0.82592798671209E+01
 0.30972299501703E+01 0.31658221148760E+03 0.29215000010092E+03 0.31519881658043E+03 0.34061237436766E+03
 0.29215000000552E+03 0.29215000001263E+03 0.31233439526561E+03 0.34057436777438E+03 0.29215000000432E+03
 0.29215000001258E+03 0.31519881658043E+03 0.34061237436766E+03 0.29215000000552E+03 0.29215000001263E+03
 0.31233439526561E+03 0.34057436777438E+03 0.29215000000432E+03 0.29215000001258E+03 0.38404178453480E+03
 0.30182371082753E+03 0.15904622540423E+04 0.15255551471927E+04 0.72684136053592E+03 0.13161629062497E+04
 0.58568733891114E+03 0.10179344694799E+04 0.11587269739772E+04 0.96998990189888E+03 0.19435969301863E+04
 0.89968414344849E+03 0.11578397594540E+04 0.86454350408913E+03 0.19430738509419E+04 0.10179344694799E+04
 0.11587269739772E+04 0.96998990189888E+03 0.19435969301863E+04 0.89968414344849E+03 0.11578397594540E+04
 0.86454350408913E+03 0.19430738509419E+04 0.17065780913788E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50168013721140E+03 0.12948437070509E+01
 0.12948437070509E+01 0.12753082490050E+01 0.42680065147711E-01 0.29984500427488E+03 0.32717340161520E+03
 0.32639581642929E+03 0.32632714055818E+03 0.23000000000000E+00 0.00000000000000E+00 0.19355135313948E+00
 0.00000000000000E+00 -.12412199380671E+02 0.42348171696058E-02 0.69944061019228E+00 0.18891016257839E+04
 0.70841310966897E+03 0.11437711627583E+02 0.42891418603436E+01 0.30182115549686E+03 0.38406524624466E+03
 0.29401462307897E+03 0.29926551728789E+03 0.29215000000009E+03 0.29215000000138E+03 0.29400324709045E+03
 0.29926550498720E+03 0.29215000000009E+03 0.29215000000138E+03 0.29401462307897E+03 0.29926551728789E+03
 0.29215000000009E+03 0.29215000000138E+03 0.29400324709045E+03 0.29926550498720E+03 0.29215000000009E+03
 0.29215000000138E+03 0.29817828067404E+03 0.29215000001948E+03 0.10596428790649E+03 0.10150101851746E+03
 0.13241509932876E+03 0.38523388058931E+03 0.25215670576390E+03 0.10465057525398E+03 0.12513268815076E+03
 0.10465057525398E+03 0.30724489545873E+03 0.10431170915713E+03 0.12507107930977E+03 0.10431170915713E+03
 0.30718338361187E+03 0.10465057525398E+03 0.12513268815078E+03 0.10465057525398E+03 0.30724489545876E+03
 0.10431170915713E+03 0.12507107930977E+03 0.10431170915713E+03 0.30718338361187E+03 0.20725216824485E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35074276317273E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17513251578026E+00 0.00000000000000E+00 0.00000000000000E+00 0.17513251578026E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18488402031247E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18488402031247E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455210110459E+00 0.18779547657292E+00 0.32717340161520E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    370.01415024
 0.69660503256743E+00 0.30200997921486E+03 0.44293063930847E+03 0.41020862563716E+03 0.39963157395793E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16122301799327E+00 0.00000000000000E+00 -.18934506807054E+02
 0.96717137283177E-03 0.98264674586365E+00 0.80000000000000E+04 0.30000000000000E+04 0.81412776602326E+01
 0.30529791225872E+01 0.31708314612718E+03 0.29215000014240E+03 0.31566151111751E+03 0.34143542923281E+03
 0.29215000000809E+03 0.29215000001855E+03 0.31276023348047E+03 0.34139792295295E+03 0.29215000000634E+03
 0.29215000001848E+03 0.31566151111751E+03 0.34143542923281E+03 0.29215000000809E+03 0.29215000001855E+03
 0.31276023348047E+03 0.34139792295295E+03 0.29215000000634E+03 0.29215000001848E+03 0.38507894022683E+03
 0.30246108136087E+03 0.15964459977178E+04 0.15302887774666E+04 0.72523950471796E+03 0.13067717781240E+04
 0.57790607588245E+03 0.10217449856195E+04 0.11625805790435E+04 0.97297182880011E+03 0.19431359719787E+04
 0.90372991069015E+03 0.11617244752673E+04 0.86797145900761E+03 0.19426385935173E+04 0.10217449856195E+04
 0.11625805790435E+04 0.97297182880011E+03 0.19431359719787E+04 0.90372991069015E+03 0.11617244752673E+04
 0.86797145900761E+03 0.19426385935173E+04 0.17056687160129E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50206750017552E+03 0.12948439976633E+01
 0.12948439976633E+01 0.13153503390511E+01 0.34964117522576E-01 0.30074481201155E+03 0.32723571937516E+03
 0.32661823190960E+03 0.32656521760586E+03 0.23000000000000E+00 0.00000000000000E+00 0.19223903989507E+00
 0.00000000000000E+00 -.12423116796636E+02 0.51693641338138E-02 0.72498925793136E+00 0.15475791205480E+04
 0.58034217020551E+03 0.11034646255072E+02 0.41379923456522E+01 0.30245854439136E+03 0.38510101172596E+03
 0.29409425167029E+03 0.29941862438135E+03 0.29215000000011E+03 0.29215000000201E+03 0.29408306445524E+03
 0.29941850307908E+03 0.29215000000011E+03 0.29215000000201E+03 0.29409425167029E+03 0.29941862438135E+03
 0.29215000000011E+03 0.29215000000201E+03 0.29408306445524E+03 0.29941850307908E+03 0.29215000000011E+03
 0.29215000000201E+03 0.29831054108721E+03 0.29215000002767E+03 0.10573309856540E+03 0.10204917933030E+03
 0.13493250799273E+03 0.38713748418344E+03 0.25153031365075E+03 0.10775434238621E+03 0.12744735901801E+03
 0.10775434238621E+03 0.30869555117746E+03 0.10746065579448E+03 0.12737932116582E+03 0.10746065579448E+03
 0.30862846838841E+03 0.10775434238621E+03 0.12744735901802E+03 0.10775434238621E+03 0.30869555117747E+03
 0.10746065579448E+03 0.12737932116582E+03 0.10746065579448E+03 0.30862846838841E+03 0.20859963441383E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35095413683696E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17528203309030E+00 0.00000000000000E+00 0.00000000000000E+00 0.17528203309030E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18501315843022E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18501315843022E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455189864138E+00 0.18775947535946E+00 0.32723571937516E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    380.00802386
 0.69671959025412E+00 0.30228317964386E+03 0.44333989520569E+03 0.41058090284941E+03 0.39999201409695E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16022301345127E+00 0.00000000000000E+00 -.18971820536729E+02
 0.96629689737858E-03 0.99602050976788E+00 0.80000000000000E+04 0.30000000000000E+04 0.80319631187759E+01
 0.30119861695410E+01 0.31757640157192E+03 0.29215000019825E+03 0.31611734367871E+03 0.34224394942351E+03
 0.29215000001170E+03 0.29215000002687E+03 0.31318007110795E+03 0.34220692201362E+03 0.29215000000918E+03
 0.29215000002676E+03 0.31611734367871E+03 0.34224394942351E+03 0.29215000001170E+03 0.29215000002687E+03
 0.31318007110795E+03 0.34220692201362E+03 0.29215000000918E+03 0.29215000002676E+03 0.38608931199717E+03
 0.30311082045061E+03 0.16022450497927E+04 0.15348478314037E+04 0.72363462755303E+03 0.12977338059673E+04
 0.57048100527646E+03 0.10254426703174E+04 0.11662773873885E+04 0.97584636114019E+03 0.19426733195305E+04
 0.90766100946236E+03 0.11654501057887E+04 0.87128659272622E+03 0.19421995902804E+04 0.10254426703174E+04
 0.11662773873885E+04 0.97584636114019E+03 0.19426733195305E+04 0.90766100946236E+03 0.11654501057887E+04
 0.87128659272622E+03 0.19421995902804E+04 0.17047869306034E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50245572600711E+03 0.12948442725889E+01
 0.12948442725889E+01 0.13553258335345E+01 0.28121939424517E-01 0.30165156526677E+03 0.32731096047960E+03
 0.32682989917437E+03 0.32678980975301E+03 0.23000000000000E+00 0.00000000000000E+00 0.19093007738782E+00
 0.00000000000000E+00 -.12432927497347E+02 0.64270901237912E-02 0.75041499968559E+00 0.12447312618795E+04
 0.46677422320482E+03 0.10660767713001E+02 0.39977878923755E+01 0.30310831807867E+03 0.38611004187233E+03
 0.29417654225010E+03 0.29957027868630E+03 0.29215000000014E+03 0.29215000000291E+03 0.29416561401892E+03
 0.29957004873673E+03 0.29215000000014E+03 0.29215000000291E+03 0.29417654225010E+03 0.29957027868630E+03
 0.29215000000014E+03 0.29215000000291E+03 0.29416561401892E+03 0.29957004873673E+03 0.29215000000014E+03
 0.29215000000291E+03 0.29844155261442E+03 0.29215000003880E+03 0.10529875175746E+03 0.10233809731632E+03
 0.13735918035247E+03 0.38910020567780E+03 0.25105422942356E+03 0.11083024732270E+03 0.12967863608490E+03
 0.11083024732270E+03 0.31017894688119E+03 0.11058138128782E+03 0.12960460463041E+03 0.11058138128782E+03
 0.31010672342171E+03 0.11083024732270E+03 0.12967863608490E+03 0.11083024732270E+03 0.31017894688119E+03
 0.11058138128782E+03 0.12960460463041E+03 0.11058138128782E+03 0.31010672342171E+03 0.20986859815459E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35120973425315E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17541419678710E+00 0.00000000000000E+00 0.00000000000000E+00 0.17541419678710E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18512413048845E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18512413048845E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455166310468E+00 0.18771604238016E+00 0.32731096047960E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    390.19846917
 0.69683049632998E+00 0.30255727720389E+03 0.44375800894645E+03 0.41096035028568E+03 0.40035854721530E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15924578557165E+00 0.00000000000000E+00 -.19010419002775E+02
 0.96542112630113E-03 0.10090244934126E+01 0.80000000000000E+04 0.30000000000000E+04 0.79284497573920E+01
 0.29731686590220E+01 0.31807242989096E+03 0.29215000027461E+03 0.31657595498293E+03 0.34305561919331E+03
 0.29215000001684E+03 0.29215000003870E+03 0.31360273059546E+03 0.34301905818086E+03 0.29215000001323E+03
 0.29215000003855E+03 0.31657595498293E+03 0.34305561919331E+03 0.29215000001684E+03 0.29215000003870E+03
 0.31360273059546E+03 0.34301905818086E+03 0.29215000001323E+03 0.29215000003855E+03 0.38709727553492E+03
 0.30378562372354E+03 0.16079923708010E+04 0.15393399596699E+04 0.72196816672743E+03 0.12887878876715E+04
 0.56320988011048E+03 0.10291117196464E+04 0.11699001947193E+04 0.97868105726475E+03 0.19421920177468E+04
 0.91156675704183E+03 0.11691001922173E+04 0.87456613394597E+03 0.19417405311258E+04 0.10291117196464E+04
 0.11699001947193E+04 0.97868105726475E+03 0.19421920177468E+04 0.91156675704183E+03 0.11691001922173E+04
 0.87456613394597E+03 0.19417405311258E+04 0.17038979055350E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50285254136459E+03 0.12948445569806E+01
 0.12948445569806E+01 0.13960876148072E+01 0.22376992487205E-01 0.30257361819058E+03 0.32740608644195E+03
 0.32703563580495E+03 0.32700572279150E+03 0.23000000000000E+00 0.00000000000000E+00 0.18958631179267E+00
 0.00000000000000E+00 -.12444756234883E+02 0.80771455247901E-02 0.77644041680222E+00 0.99044891235977E+03
 0.37141834213491E+03 0.10303430664967E+02 0.38637864993628E+01 0.30378316930185E+03 0.38711668954561E+03
 0.29426228800036E+03 0.29972390933946E+03 0.29215000000020E+03 0.29215000000421E+03 0.29425166493424E+03
 0.29972356988955E+03 0.29215000000020E+03 0.29215000000421E+03 0.29426228800036E+03 0.29972390933946E+03
 0.29215000000020E+03 0.29215000000421E+03 0.29425166493424E+03 0.29972356988955E+03 0.29215000000020E+03
 0.29215000000421E+03 0.29857437720744E+03 0.29215000005414E+03 0.10467324274751E+03 0.10236732519099E+03
 0.13978241515949E+03 0.39126322221160E+03 0.25078189497631E+03 0.11394719854579E+03 0.13191144545992E+03
 0.11394719854579E+03 0.31179820100200E+03 0.11373972008992E+03 0.13183181575831E+03 0.11373972008992E+03
 0.31172123687517E+03 0.11394719854579E+03 0.13191144545992E+03 0.11394719854579E+03 0.31179820100200E+03
 0.11373972008992E+03 0.13183181575831E+03 0.11373972008992E+03 0.31172123687516E+03 0.21111586026144E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35155609029304E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17555451209672E+00 0.00000000000000E+00 0.00000000000000E+00 0.17555451209672E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18521396200029E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18521396200029E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455128533434E+00 0.18766107449853E+00 0.32740608644195E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    400.13543299
 0.69693715972968E+00 0.30281996164582E+03 0.44416511902815E+03 0.41132888818562E+03 0.40071378785358E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15833243730684E+00 0.00000000000000E+00 -.19045081913911E+02
 0.96458333128965E-03 0.10211187903068E+01 0.80000000000000E+04 0.30000000000000E+04 0.78345439100148E+01
 0.29379539662555E+01 0.31854937162047E+03 0.29215000037464E+03 0.31701711808042E+03 0.34383498455377E+03
 0.29215000002383E+03 0.29215000005483E+03 0.31400953734964E+03 0.34379885716905E+03 0.29215000001875E+03
 0.29215000005460E+03 0.31701711808042E+03 0.34383498455377E+03 0.29215000002383E+03 0.29215000005483E+03
 0.31400953734964E+03 0.34379885716905E+03 0.29215000001875E+03 0.29215000005460E+03 0.38805942009407E+03
 0.30445448053845E+03 0.16134361344841E+04 0.15435674864750E+04 0.72030624714525E+03 0.12802900181369E+04
 0.55638223975594E+03 0.10325918393771E+04 0.11732883760205E+04 0.98135150335480E+03 0.19416964385582E+04
 0.91527648278645E+03 0.11725131098142E+04 0.87766628447024E+03 0.19412650050978E+04 0.10325918393771E+04
 0.11732883760205E+04 0.98135150335480E+03 0.19416964385582E+04 0.91527648278645E+03 0.11725131098142E+04
 0.87766628447025E+03 0.19412650050978E+04 0.17030231939154E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50323870729173E+03 0.12948448123753E+01
 0.12948448123753E+01 0.14358354700872E+01 0.17898654577143E-01 0.30345878141240E+03 0.32751322749405E+03
 0.32722619934641E+03 0.32720374004745E+03 0.23000000000000E+00 0.00000000000000E+00 0.18826876608114E+00
 0.00000000000000E+00 -.12455330153024E+02 0.10098089303629E-01 0.80189008290594E+00 0.79222908012163E+03
 0.29708590504561E+03 0.99764296510678E+01 0.37411611191504E+01 0.30445208325899E+03 0.38807763168591E+03
 0.29434718854982E+03 0.29987301762170E+03 0.29215000000026E+03 0.29215000000597E+03 0.29433687929980E+03
 0.29987257336424E+03 0.29215000000026E+03 0.29215000000597E+03 0.29434718854982E+03 0.29987301762170E+03
 0.29215000000026E+03 0.29215000000597E+03 0.29433687929980E+03 0.29987257336424E+03 0.29215000000026E+03
 0.29215000000597E+03 0.29870317663427E+03 0.29215000007438E+03 0.10388219104250E+03 0.10211226152249E+03
 0.14208988654239E+03 0.39344613699050E+03 0.25064580101540E+03 0.11696872268103E+03 0.13403896622597E+03
 0.11696872268103E+03 0.31347420735049E+03 0.11679644254007E+03 0.13395431222486E+03 0.11679644254007E+03
 0.31339303852356E+03 0.11696872268103E+03 0.13403896622597E+03 0.11696872268103E+03 0.31347420735049E+03
 0.11679644254007E+03 0.13395431222485E+03 0.11679644254007E+03 0.31339303852355E+03 0.21226297036892E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35186895723068E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17566722760082E+00 0.00000000000000E+00 0.00000000000000E+00 0.17566722760082E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18531139419515E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18531139419515E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455087611098E+00 0.18759922458488E+00 0.32751322749405E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    410.18408066
 0.69703198427762E+00 0.30308167811306E+03 0.44457678066386E+03 0.41170124329834E+03 0.40107213218136E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15744700441004E+00 0.00000000000000E+00 -.19081003916832E+02
 0.96375005454005E-03 0.10327828077682E+01 0.80000000000000E+04 0.30000000000000E+04 0.77460623277490E+01
 0.29047733729059E+01 0.31902504921877E+03 0.29215000050909E+03 0.31745728705432E+03 0.34461153353634E+03
 0.29215000003360E+03 0.29215000007737E+03 0.31441560223713E+03 0.34457582329996E+03 0.29215000002647E+03
 0.29215000007707E+03 0.31745728705432E+03 0.34461153353634E+03 0.29215000003360E+03 0.29215000007737E+03
 0.31441560223713E+03 0.34457582329996E+03 0.29215000002647E+03 0.29215000007707E+03 0.38901482542249E+03
 0.30514028315247E+03 0.16188097884062E+04 0.15477224841088E+04 0.71858079533221E+03 0.12718816617921E+04
 0.54970796248325E+03 0.10360293036596E+04 0.11765944185605E+04 0.98397694024707E+03 0.19411920471245E+04
 0.91894488207089E+03 0.11758424380450E+04 0.88072218586916E+03 0.19407793915803E+04 0.10360293036596E+04
 0.11765944185605E+04 0.98397694024707E+03 0.19411920471245E+04 0.91894488207089E+03 0.11758424380450E+04
 0.88072218586916E+03 0.19407793915803E+04 0.17021445383175E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50363011523130E+03 0.12948450770472E+01
 0.12948450770472E+01 0.14760300607565E+01 0.14275862593812E-01 0.30432717387175E+03 0.32763094952881E+03
 0.32740916186268E+03 0.32739235241365E+03 0.23000000000000E+00 0.00000000000000E+00 0.18693549495582E+00
 0.00000000000000E+00 -.12467117667199E+02 0.12660685125182E-01 0.82758718106121E+00 0.63187733688190E+03
 0.23695400133071E+03 0.96666552878957E+01 0.36249957329609E+01 0.30513795187925E+03 0.38903185601669E+03
 0.29443379019735E+03 0.30002287290017E+03 0.29215000000036E+03 0.29215000000844E+03 0.29442380041998E+03
 0.30002232523386E+03 0.29215000000036E+03 0.29215000000844E+03 0.29443379019735E+03 0.30002287290017E+03
 0.29215000000036E+03 0.29215000000844E+03 0.29442380041998E+03 0.30002232523386E+03 0.29215000000036E+03
 0.29215000000844E+03 0.29883251642827E+03 0.29215000010180E+03 0.10289484125796E+03 0.10154723411788E+03
 0.14434476391459E+03 0.39565659109351E+03 0.25059010335934E+03 0.11998608431669E+03 0.13611855158391E+03
 0.11998608431669E+03 0.31519740737491E+03 0.11984471789419E+03 0.13602929436616E+03 0.11984471789419E+03
 0.31511244306475E+03 0.11998608431669E+03 0.13611855158391E+03 0.11998608431669E+03 0.31519740737491E+03
 0.11984471789419E+03 0.13602929436616E+03 0.11984471789419E+03 0.31511244306475E+03 0.21332051812594E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35216063111505E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17578941290885E+00 0.00000000000000E+00 0.00000000000000E+00 0.17578941290885E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18536508670601E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18536508670601E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455038049561E+00 0.18753126345908E+00 0.32763094952881E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    420.70010934
 0.69711060964896E+00 0.30335206762474E+03 0.44500859665975E+03 0.41209184022097E+03 0.40144749749985E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15655973442108E+00 0.00000000000000E+00 -.19103840559637E+02
 0.96289080952659E-03 0.10444042308593E+01 0.80000000000000E+04 0.30000000000000E+04 0.76598693912011E+01
 0.28724510217004E+01 0.31951843208720E+03 0.29215000069232E+03 0.31791400250013E+03 0.34541200732812E+03
 0.29215000004740E+03 0.29215000010926E+03 0.31483758934536E+03 0.34537672570717E+03 0.29215000003741E+03
 0.29215000010882E+03 0.31791400250013E+03 0.34541200732812E+03 0.29215000004740E+03 0.29215000010926E+03
 0.31483758934536E+03 0.34537672570717E+03 0.29215000003741E+03 0.29215000010882E+03 0.38998169778990E+03
 0.30586473967049E+03 0.16243053189451E+04 0.15519433263662E+04 0.71693529753320E+03 0.12636670596067E+04
 0.54314708558580E+03 0.10395472653310E+04 0.11799541574815E+04 0.98664419937936E+03 0.19406954910549E+04
 0.92270147089375E+03 0.11792248818316E+04 0.88383316327553E+03 0.19403009242727E+04 0.10395472653310E+04
 0.11799541574815E+04 0.98664419937936E+03 0.19406954910549E+04 0.92270147089375E+03 0.11792248818316E+04
 0.88383316327553E+03 0.19403009242727E+04 0.17013415871341E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50404233441984E+03 0.12948452453067E+01
 0.12948452453067E+01 0.15180941754528E+01 0.11263506969177E-01 0.30520380725330E+03 0.32776107923401E+03
 0.32759169657390E+03 0.32757927858708E+03 0.23000000000000E+00 0.00000000000000E+00 0.18554495755540E+00
 0.00000000000000E+00 -.12465585020074E+02 0.16046706519818E-01 0.85433520214855E+00 0.49854466959434E+03
 0.18695425109788E+03 0.93640060480722E+01 0.35115022680271E+01 0.30586248755847E+03 0.38999752538159E+03
 0.29452724482601E+03 0.30017988148069E+03 0.29215000000051E+03 0.29215000001196E+03 0.29451760436643E+03
 0.30017922579921E+03 0.29215000000050E+03 0.29215000001196E+03 0.29452724482601E+03 0.30017988148069E+03
 0.29215000000051E+03 0.29215000001196E+03 0.29451760436643E+03 0.30017922579921E+03 0.29215000000050E+03
 0.29215000001196E+03 0.29896802230756E+03 0.29215000013943E+03 0.10167729011928E+03 0.10065769898660E+03
 0.14661080734556E+03 0.39792809171027E+03 0.25058423032797E+03 0.12310892157938E+03 0.13820532474617E+03
 0.12310892157938E+03 0.31698374148532E+03 0.12299538305007E+03 0.13811163005921E+03 0.12299538305007E+03
 0.31689518275757E+03 0.12310892157938E+03 0.13820532474617E+03 0.12310892157938E+03 0.31698374148532E+03
 0.12299538305007E+03 0.13811163005921E+03 0.12299538305007E+03 0.31689518275757E+03 0.21430832057967E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35244559360139E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17579092460784E+00 0.00000000000000E+00 0.00000000000000E+00 0.17579092460784E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18541227269524E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18541227269524E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17455022813263E+00 0.18745664785965E+00 0.32776107923401E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    430.76979950
 0.69716473219096E+00 0.30360812878997E+03 0.44542233561914E+03 0.41246638111083E+03 0.40180711059039E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15574589255295E+00 0.00000000000000E+00 -.19128446048324E+02
 0.96207847986804E-03 0.10550022282709E+01 0.80000000000000E+04 0.30000000000000E+04 0.75829223726968E+01
 0.28435958897613E+01 0.31998540488734E+03 0.29215000091733E+03 0.31834640151484E+03 0.34616855671109E+03
 0.29215000006493E+03 0.29215000014977E+03 0.31523732764308E+03 0.34613366767014E+03 0.29215000005133E+03
 0.29215000014918E+03 0.31834640151484E+03 0.34616855671109E+03 0.29215000006493E+03 0.29215000014977E+03
 0.31523732764308E+03 0.34613366767014E+03 0.29215000005133E+03 0.29215000014918E+03 0.39089266073489E+03
 0.30656432400708E+03 0.16294652700885E+04 0.15558938107622E+04 0.71532271600973E+03 0.12559624354447E+04
 0.53706310585491E+03 0.10428509779125E+04 0.11830732263906E+04 0.98914042632028E+03 0.19402246286272E+04
 0.92623240043194E+03 0.11823641833399E+04 0.88675028537059E+03 0.19398460655083E+04 0.10428509779125E+04
 0.11830732263906E+04 0.98914042632028E+03 0.19402246286272E+04 0.92623240043194E+03 0.11823641833399E+04
 0.88675028537059E+03 0.19398460655083E+04 0.17005874385319E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50443907043814E+03 0.12948454265991E+01
 0.12948454265991E+01 0.15583729361085E+01 0.89731577352863E-02 0.30602162862873E+03 0.32789003847819E+03
 0.32775921935086E+03 0.32774993086511E+03 0.23000000000000E+00 0.00000000000000E+00 0.18422280495686E+00
 0.00000000000000E+00 -.12466818305868E+02 0.20142539218158E-01 0.87972279881135E+00 0.39716938928873E+03
 0.14893852098327E+03 0.90937736418896E+01 0.34101651157086E+01 0.30656215860997E+03 0.39090737402503E+03
 0.29461738133147E+03 0.30032931564773E+03 0.29215000000070E+03 0.29215000001643E+03 0.29460806502367E+03
 0.30032855948032E+03 0.29215000000069E+03 0.29215000001644E+03 0.29461738133147E+03 0.30032931564773E+03
 0.29215000000070E+03 0.29215000001643E+03 0.29460806502367E+03 0.30032855948032E+03 0.29215000000069E+03
 0.29215000001644E+03 0.29909687281994E+03 0.29215000018592E+03 0.10033500756713E+03 0.99553072827859E+02
 0.14868364011901E+03 0.40004516694428E+03 0.25061810862468E+03 0.12604976772466E+03 0.14011230663312E+03
 0.12604976772466E+03 0.31865741507087E+03 0.12595906614851E+03 0.14001479870739E+03 0.12595906614851E+03
 0.31856582682979E+03 0.12604976772466E+03 0.14011230663312E+03 0.12604976772466E+03 0.31865741507087E+03
 0.12595906614851E+03 0.14001479870739E+03 0.12595906614851E+03 0.31856582682978E+03 0.21514415235401E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35270089436045E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17581609142922E+00 0.00000000000000E+00 0.00000000000000E+00 0.17581609142922E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18542342604897E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18542342604897E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454997613260E+00 0.18738264829886E+00 0.32789003847819E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    440.83948966
 0.69720026349543E+00 0.30386137978221E+03 0.44583576740707E+03 0.41284090725319E+03 0.40216643000668E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15496538974885E+00 0.00000000000000E+00 -.19150944642780E+02
 0.96127642927532E-03 0.10651078594327E+01 0.80000000000000E+04 0.30000000000000E+04 0.75109764040808E+01
 0.28166161515303E+01 0.32044737082508E+03 0.29215000120393E+03 0.31877428385579E+03 0.34691523162211E+03
 0.29215000008800E+03 0.29215000020311E+03 0.31563317806307E+03 0.34688072003806E+03 0.29215000006969E+03
 0.29215000020232E+03 0.31877428385579E+03 0.34691523162211E+03 0.29215000008800E+03 0.29215000020311E+03
 0.31563317806307E+03 0.34688072003806E+03 0.29215000006969E+03 0.29215000020232E+03 0.39178639521872E+03
 0.30726891704312E+03 0.16345272107623E+04 0.15597551859774E+04 0.71371450033814E+03 0.12484800929919E+04
 0.53119702015210E+03 0.10460928708838E+04 0.11861026532569E+04 0.99158034548749E+03 0.19397608455685E+04
 0.92969986511088E+03 0.11854125097258E+04 0.88960666647319E+03 0.19393971168412E+04 0.10460928708838E+04
 0.11861026532569E+04 0.99158034548749E+03 0.19397608455685E+04 0.92969986511088E+03 0.11854125097258E+04
 0.88960666647319E+03 0.19393971168412E+04 0.16998639417705E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50483706704707E+03 0.12948455923680E+01
 0.12948455923680E+01 0.15986516967642E+01 0.71465433196334E-02 0.30682280634359E+03 0.32802221904745E+03
 0.32792121736663E+03 0.32791427435693E+03 0.23000000000000E+00 0.00000000000000E+00 0.18291350753904E+00
 0.00000000000000E+00 -.12466091997061E+02 0.25290852523832E-01 0.90482222722196E+00 0.31631990232284E+03
 0.11861996337107E+03 0.88415157799142E+01 0.33155684174678E+01 0.30726684191851E+03 0.39180008161371E+03
 0.29470840297067E+03 0.30047791598874E+03 0.29215000000097E+03 0.29215000002235E+03 0.29469940052684E+03
 0.30047706187495E+03 0.29215000000096E+03 0.29215000002236E+03 0.29470840297067E+03 0.30047791598874E+03
 0.29215000000097E+03 0.29215000002235E+03 0.29469940052684E+03 0.30047706187495E+03 0.29215000000096E+03
 0.29215000002236E+03 0.29922492661908E+03 0.29215000024550E+03 0.98829014900389E+02 0.98228489099536E+02
 0.15066464853023E+03 0.40210071408792E+03 0.25068274231504E+03 0.12894986266451E+03 0.14193190945858E+03
 0.12894986266451E+03 0.32028634019902E+03 0.12887870033193E+03 0.14183093037101E+03 0.12887870033193E+03
 0.32019204432364E+03 0.12894986266451E+03 0.14193190945858E+03 0.12894986266451E+03 0.32028634019902E+03
 0.12887870033193E+03 0.14183093037101E+03 0.12887870033193E+03 0.32019204432364E+03 0.21588012966057E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35294275425849E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17582368057044E+00 0.00000000000000E+00 0.00000000000000E+00 0.17582368057044E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18541425297770E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18541425297770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454976127635E+00 0.18730691095057E+00 0.32802221904745E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    451.94911132
 0.69722268906730E+00 0.30413726146950E+03 0.44629025468064E+03 0.41325282395209E+03 0.40256133992218E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15414107048961E+00 0.00000000000000E+00 -.19180494095463E+02
 0.96040417915442E-03 0.10757174024119E+01 0.80000000000000E+04 0.30000000000000E+04 0.74368974435694E+01
 0.27888365413385E+01 0.32094998434031E+03 0.29215000161650E+03 0.31923994580953E+03 0.34772802426299E+03
 0.29215000012247E+03 0.29215000028280E+03 0.31606402905217E+03 0.34769390560587E+03 0.29215000009716E+03
 0.29215000028172E+03 0.31923994580953E+03 0.34772802426299E+03 0.29215000012247E+03 0.29215000028280E+03
 0.31606402905217E+03 0.34769390560587E+03 0.29215000009716E+03 0.29215000028172E+03 0.39276165471573E+03
 0.30805337965008E+03 0.16400033561619E+04 0.15639231832454E+04 0.71182956048674E+03 0.12402423774384E+04
 0.52485366914925E+03 0.10496007125735E+04 0.11893349362269E+04 0.99421420572928E+03 0.19392357392992E+04
 0.93345564165595E+03 0.11886642074528E+04 0.89269639315179E+03 0.19388871845807E+04 0.10496007125735E+04
 0.11893349362269E+04 0.99421420572928E+03 0.19392357392992E+04 0.93345564165595E+03 0.11886642074528E+04
 0.89269639315179E+03 0.19388871845807E+04 0.16990364338775E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50527597045393E+03 0.12948458100874E+01
 0.12948458100874E+01 0.16430901833992E+01 0.55581310187507E-02 0.30769515154166E+03 0.32817107417636E+03
 0.32809520226921E+03 0.32809017323581E+03 0.23000000000000E+00 0.00000000000000E+00 0.18148752666955E+00
 0.00000000000000E+00 -.12470055364027E+02 0.32518514556196E-01 0.93211230164548E+00 0.24601369740229E+03
 0.92255136525858E+02 0.85826568170782E+01 0.32184963064043E+01 0.30805140511264E+03 0.39277429156725E+03
 0.29480841302664E+03 0.30064003407934E+03 0.29215000000140E+03 0.29215000003124E+03 0.29479973380200E+03
 0.30063907611988E+03 0.29215000000138E+03 0.29215000003125E+03 0.29480841302664E+03 0.30064003407934E+03
 0.29215000000140E+03 0.29215000003124E+03 0.29479973380200E+03 0.30063907611988E+03 0.29215000000138E+03
 0.29215000003125E+03 0.29936451158525E+03 0.29215000033182E+03 0.96975723902314E+02 0.96528889228494E+02
 0.15275044413444E+03 0.40430684016443E+03 0.25079264380932E+03 0.13210649428802E+03 0.14384504595518E+03
 0.13210649428802E+03 0.32203656992674E+03 0.13205356159717E+03 0.14374060013784E+03 0.13205356159717E+03
 0.32193961650994E+03 0.13210649428802E+03 0.14384504595518E+03 0.13210649428802E+03 0.32203656992675E+03
 0.13205356159717E+03 0.14374060013784E+03 0.13205356159717E+03 0.32193961650994E+03 0.21658879994719E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35319688137272E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17587373110240E+00 0.00000000000000E+00 0.00000000000000E+00 0.17587373110240E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18537933404237E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18537933404237E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454936672977E+00 0.18722151988523E+00 0.32817107417636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    460.14873815
 0.69721533500314E+00 0.30433976888852E+03 0.44662716171020E+03 0.41355884429257E+03 0.40285462231301E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15355623187752E+00 0.00000000000000E+00 -.19186871583885E+02
 0.95976506662424E-03 0.10831966636788E+01 0.80000000000000E+04 0.30000000000000E+04 0.73855471201602E+01
 0.27695801700601E+01 0.32131957223284E+03 0.29215000198500E+03 0.31958242881879E+03 0.34832047383449E+03
 0.29215000015406E+03 0.29215000035586E+03 0.31638156869822E+03 0.34828664502001E+03 0.29215000012238E+03
 0.29215000035452E+03 0.31958242881879E+03 0.34832047383449E+03 0.29215000015406E+03 0.29215000035586E+03
 0.31638156869822E+03 0.34828664502001E+03 0.29215000012238E+03 0.29215000035452E+03 0.39345589215308E+03
 0.30863188139956E+03 0.16439938609352E+04 0.15669468857714E+04 0.71064254529671E+03 0.12346968394593E+04
 0.52050108143611E+03 0.10521567053236E+04 0.11916924209666E+04 0.99612373385854E+03 0.19389065783610E+04
 0.93619153077173E+03 0.11910351839790E+04 0.89493630089380E+03 0.19385684024604E+04 0.10521567053236E+04
 0.11916924209666E+04 0.99612373385854E+03 0.19389065783610E+04 0.93619153077173E+03 0.11910351839790E+04
 0.89493630089380E+03 0.19385684024605E+04 0.16985676556189E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50560345304002E+03 0.12948458570766E+01
 0.12948458570766E+01 0.16758886907224E+01 0.46161619874994E-02 0.30832535177379E+03 0.32828296356846E+03
 0.32822154518918E+03 0.32821758265877E+03 0.23000000000000E+00 0.00000000000000E+00 0.18044942119196E+00
 0.00000000000000E+00 -.12457134100791E+02 0.39154206198785E-01 0.95194826733642E+00 0.20432032153542E+03
 0.76620120575784E+02 0.84038180166914E+01 0.31514317562593E+01 0.30862999140688E+03 0.39346774493016E+03
 0.29488491795051E+03 0.30076016056067E+03 0.29215000000180E+03 0.29215000003941E+03 0.29487647903546E+03
 0.30075912602328E+03 0.29215000000178E+03 0.29215000003942E+03 0.29488491795051E+03 0.30076016056067E+03
 0.29215000000180E+03 0.29215000003941E+03 0.29487647903546E+03 0.30075912602328E+03 0.29215000000178E+03
 0.29215000003942E+03 0.29946798314274E+03 0.29215000040927E+03 0.95518443758638E+02 0.95156277541042E+02
 0.15422276629094E+03 0.40587754230242E+03 0.25088366218003E+03 0.13440717321778E+03 0.14519174672876E+03
 0.13440717321778E+03 0.32327950798086E+03 0.13436568841258E+03 0.14508494175785E+03 0.13436568841258E+03
 0.32318079192149E+03 0.13440717321778E+03 0.14519174672876E+03 0.13440717321778E+03 0.32327950798086E+03
 0.13436568841258E+03 0.14508494175785E+03 0.13436568841258E+03 0.32318079192149E+03 0.21704577130622E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35337776609960E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17577771426361E+00 0.00000000000000E+00 0.00000000000000E+00 0.17577771426361E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18532922537227E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18532922537227E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454952861008E+00 0.18715790622170E+00 0.32828296356846E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    471.63147320
 0.69720897417207E+00 0.30461974030414E+03 0.44709586820771E+03 0.41398398988115E+03 0.40326170175205E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15276920109600E+00 0.00000000000000E+00 -.19212737569091E+02
 0.95888271617842E-03 0.10932062304119E+01 0.80000000000000E+04 0.30000000000000E+04 0.73179238989388E+01
 0.27442214621021E+01 0.32183048570517E+03 0.29215000263327E+03 0.32005598049613E+03 0.34914110074745E+03
 0.29215000021143E+03 0.29215000048852E+03 0.31682054908877E+03 0.34910765503121E+03 0.29215000016825E+03
 0.29215000048671E+03 0.32005598049613E+03 0.34914110074745E+03 0.29215000021143E+03 0.29215000048852E+03
 0.31682054908877E+03 0.34910765503121E+03 0.29215000016825E+03 0.29215000048671E+03 0.39442426283176E+03
 0.30944808236618E+03 0.16494717567746E+04 0.15710847623703E+04 0.70879873056514E+03 0.12267749531072E+04
 0.51443222888927E+03 0.10556690603357E+04 0.11948776779198E+04 0.99873987964031E+03 0.19383951352398E+04
 0.93995582249597E+03 0.11942381367340E+04 0.89801323629606E+03 0.19380705285952E+04 0.10556690603357E+04
 0.11948776779198E+04 0.99873987964031E+03 0.19383951352398E+04 0.93995582249597E+03 0.11942381367340E+04
 0.89801323629606E+03 0.19380705285952E+04 0.16978181958164E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50605854795821E+03 0.12948460476565E+01
 0.12948460476565E+01 0.17218196308928E+01 0.35577692438364E-02 0.30920585670555E+03 0.32844295792031E+03
 0.32839733047562E+03 0.32839449893574E+03 0.23000000000000E+00 0.00000000000000E+00 0.17901825829879E+00
 0.00000000000000E+00 -.12457779351500E+02 0.50802100306946E-01 0.97924855574863E+00 0.15747380426526E+03
 0.59052676599471E+02 0.81695295367416E+01 0.30635735762781E+01 0.30944630911055E+03 0.39443511439182E+03
 0.29499099060950E+03 0.30092635683306E+03 0.29215000000256E+03 0.29215000005431E+03 0.29498286085968E+03
 0.30092521948672E+03 0.29215000000253E+03 0.29215000005432E+03 0.29499099060950E+03 0.30092635683306E+03
 0.29215000000256E+03 0.29215000005431E+03 0.29498286085968E+03 0.30092521948672E+03 0.29215000000253E+03
 0.29215000005432E+03 0.29961101762949E+03 0.29215000054625E+03 0.93318304326454E+02 0.93054642021896E+02
 0.15620717959360E+03 0.40805150379192E+03 0.25106328830036E+03 0.13760011118290E+03 0.14700467926334E+03
 0.13760011118290E+03 0.32499923826296E+03 0.13757225712706E+03 0.14689484816309E+03 0.13757225712706E+03
 0.32489829703441E+03 0.13760011118290E+03 0.14700467926334E+03 0.13760011118290E+03 0.32499923826296E+03
 0.13757225712706E+03 0.14689484816309E+03 0.13757225712706E+03 0.32489829703441E+03 0.21761980975883E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35362789862923E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17579066654731E+00 0.00000000000000E+00 0.00000000000000E+00 0.17579066654731E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18530719917668E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18530719917668E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454919362145E+00 0.18706637582560E+00 0.32844295792031E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    481.20041907
 0.69718459033889E+00 0.30485101988491E+03 0.44748563116608E+03 0.41433808015467E+03 0.40360067405927E+03
 0.22999999970306E+00 0.00000000000000E+00 0.15214068055759E+00 0.00000000000000E+00 -.19219841997607E+02
 0.95815517891495E-03 0.11011468907405E+01 0.80000000000000E+04 0.30000000000000E+04 0.72651524217808E+01
 0.27244321581678E+01 0.32225222262387E+03 0.29215000330669E+03 0.32044696086847E+03 0.34981642546393E+03
 0.29215000027286E+03 0.29215000063058E+03 0.31718329592315E+03 0.34978328767970E+03 0.29215000021746E+03
 0.29215000062827E+03 0.32044696086847E+03 0.34981642546393E+03 0.29215000027286E+03 0.29215000063058E+03
 0.31718329592315E+03 0.34978328767970E+03 0.29215000021746E+03 0.29215000062827E+03 0.39521599526661E+03
 0.31013036074372E+03 0.16539673449273E+04 0.15744721245596E+04 0.70728901715003E+03 0.12203839239665E+04
 0.50955846173071E+03 0.10585515727835E+04 0.11974698105447E+04 0.10008810390604E+04 0.19379847690043E+04
 0.94304644579736E+03 0.11968440699100E+04 0.90053389685249E+03 0.19376706470386E+04 0.10585515727835E+04
 0.11974698105447E+04 0.10008810390604E+04 0.19379847690043E+04 0.94304644579736E+03 0.11968440699100E+04
 0.90053389685248E+03 0.19376706470386E+04 0.16972347493448E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50643869725613E+03 0.12948461000017E+01
 0.12948461000017E+01 0.17600954143680E+01 0.28630976721424E-02 0.30993133944032E+03 0.32857874496111E+03
 0.32854315206555E+03 0.32854101490757E+03 0.23000000000000E+00 0.00000000000000E+00 0.17784738155000E+00
 0.00000000000000E+00 -.12441813805137E+02 0.63128179320121E-01 0.10015438911491E+01 0.12672629063848E+03
 0.47522358989432E+02 0.79876679102117E+01 0.29953754663294E+01 0.31012868810030E+03 0.39522605356679E+03
 0.29508024766669E+03 0.30106405585211E+03 0.29215000000342E+03 0.29215000007033E+03 0.29507236152210E+03
 0.30106283511291E+03 0.29215000000337E+03 0.29215000007034E+03 0.29508024766669E+03 0.30106405585211E+03
 0.29215000000342E+03 0.29215000007033E+03 0.29507236152210E+03 0.30106283511291E+03 0.29215000000337E+03
 0.29215000007034E+03 0.29972952232463E+03 0.29215000068928E+03 0.91367798386421E+02 0.91165518103727E+02
 0.15779131306120E+03 0.40981476534475E+03 0.25123449571824E+03 0.14023143639643E+03 0.14844906668893E+03
 0.14023143639643E+03 0.32638898130174E+03 0.14021304879347E+03 0.14833693919734E+03 0.14021304879347E+03
 0.32628639308086E+03 0.14023143639643E+03 0.14844906668893E+03 0.14023143639643E+03 0.32638898130174E+03
 0.14021304879347E+03 0.14833693919734E+03 0.14021304879347E+03 0.32628639308086E+03 0.21803307367092E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35382759302951E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17567798077369E+00 0.00000000000000E+00 0.00000000000000E+00 0.17567798077369E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18519936932470E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18519936932470E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454938743228E+00 0.18698930667957E+00 0.32857874496111E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    490.11217519
 0.69713333585324E+00 0.30506548947097E+03 0.44784881545537E+03 0.41466914333946E+03 0.40391773072205E+03
 0.22999999992084E+00 0.00000000000000E+00 0.15157634889091E+00 0.00000000000000E+00 -.19230543783034E+02
 0.95748146783284E-03 0.11082320256814E+01 0.80000000000000E+04 0.30000000000000E+04 0.72187049413967E+01
 0.27070143530237E+01 0.32264227045081E+03 0.29215000405739E+03 0.32080861275720E+03 0.35043867460023E+03
 0.29215000034307E+03 0.29215000079292E+03 0.31751915072965E+03 0.35040581613903E+03 0.29215000027379E+03
 0.29215000079005E+03 0.32080861275720E+03 0.35043867460023E+03 0.29215000034307E+03 0.29215000079292E+03
 0.31751915072965E+03 0.35040581613903E+03 0.29215000027379E+03 0.29215000079005E+03 0.39593992859413E+03
 0.31076628235795E+03 0.16581187203574E+04 0.15775995654279E+04 0.70594143043740E+03 0.12146611398505E+04
 0.50519000226087E+03 0.10612109599987E+04 0.11998529573058E+04 0.10028556259378E+04 0.19376392937110E+04
 0.94589774434282E+03 0.11992393693878E+04 0.90285763193282E+03 0.19373343193983E+04 0.10612109599987E+04
 0.11998529573058E+04 0.10028556259378E+04 0.19376392937110E+04 0.94589774434282E+03 0.11992393693878E+04
 0.90285763193282E+03 0.19373343193983E+04 0.16967592341448E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50679557206457E+03 0.12948461788522E+01
 0.12948461788522E+01 0.17957424388742E+01 0.23385489667582E-02 0.31060027511809E+03 0.32870659583691E+03
 0.32867836749184E+03 0.32867672464637E+03 0.23000000000000E+00 0.00000000000000E+00 0.17677593847277E+00
 0.00000000000000E+00 -.12431470341278E+02 0.77288155895721E-01 0.10219146002626E+01 0.10350874473954E+03
 0.38815779277328E+02 0.78284428052444E+01 0.29356660519667E+01 0.31076471026843E+03 0.39594926639196E+03
 0.29516437007270E+03 0.30119179219953E+03 0.29215000000443E+03 0.29215000008869E+03 0.29515670050206E+03
 0.30119049541830E+03 0.29215000000437E+03 0.29215000008871E+03 0.29516437007270E+03 0.30119179219953E+03
 0.29215000000443E+03 0.29215000008869E+03 0.29515670050206E+03 0.30119049541830E+03 0.29215000000437E+03
 0.29215000008871E+03 0.29983947669853E+03 0.29215000084937E+03 0.89464770955007E+02 0.89306407868052E+02
 0.15920994990432E+03 0.41141749511173E+03 0.25141149545788E+03 0.14265551425535E+03 0.14973997354102E+03
 0.14265551425535E+03 0.32764818435910E+03 0.14264460362920E+03 0.14962587994086E+03 0.14264460362920E+03
 0.32754422221903E+03 0.14265551425535E+03 0.14973997354102E+03 0.14265551425535E+03 0.32764818435910E+03
 0.14264460362920E+03 0.14962587994086E+03 0.14264460362920E+03 0.32754422221903E+03 0.21836921109101E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35401180340748E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17560755437015E+00 0.00000000000000E+00 0.00000000000000E+00 0.17560755437015E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18511283815711E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18511283815711E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454942981789E+00 0.18691664141401E+00 0.32870659583691E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    500.00000000
 0.69708343842434E+00 0.30530119404095E+03 0.44825001172868E+03 0.41503426061119E+03 0.40426710026461E+03
 0.22999999966526E+00 0.00000000000000E+00 0.15097285966426E+00 0.00000000000000E+00 -.19241810584657E+02
 0.95674214794948E-03 0.11157641752965E+01 0.80000000000000E+04 0.30000000000000E+04 0.71699738861704E+01
 0.26887402073139E+01 0.32307110971014E+03 0.29215000506338E+03 0.32120629797275E+03 0.35112225810554E+03
 0.29215000043966E+03 0.29215000101621E+03 0.31788860981804E+03 0.35108969778935E+03 0.29215000035140E+03
 0.29215000101258E+03 0.32120629797275E+03 0.35112225810554E+03 0.29215000043966E+03 0.29215000101621E+03
 0.31788860981804E+03 0.35108969778935E+03 0.29215000035140E+03 0.29215000101258E+03 0.39673496997647E+03
 0.31147399816540E+03 0.16626516119425E+04 0.15810033511530E+04 0.70439917020355E+03 0.12083509934402E+04
 0.50042982738563E+03 0.10641183148363E+04 0.12024311039541E+04 0.10050076921177E+04 0.19372443553295E+04
 0.94901716457607E+03 0.12018302312263E+04 0.90539436143570E+03 0.19369488857519E+04 0.10641183148363E+04
 0.12024311039541E+04 0.10050076921177E+04 0.19372443553295E+04 0.94901716457607E+03 0.12018302312263E+04
 0.90539436143570E+03 0.19369488857519E+04 0.16962127685888E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50718907438897E+03 0.12948462618657E+01
 0.12948462618657E+01 0.18352937381077E+01 0.18678941729902E-02 0.31134131419294E+03 0.32885167275630E+03
 0.32882986775848E+03 0.32882864302168E+03 0.23000000000000E+00 0.00000000000000E+00 0.17560904662211E+00
 0.00000000000000E+00 -.12420111497147E+02 0.96762510926009E-01 0.10440595338307E+01 0.82676647427197E+02
 0.31003742785199E+02 0.76623983027551E+01 0.28733993635332E+01 0.31147253799581E+03 0.39674356283665E+03
 0.29525782083563E+03 0.30133249885176E+03 0.29215000000587E+03 0.29215000011404E+03 0.29525037604845E+03
 0.30133112011739E+03 0.29215000000579E+03 0.29215000011407E+03 0.29525782083563E+03 0.30133249885176E+03
 0.29215000000587E+03 0.29215000011404E+03 0.29525037604845E+03 0.30133112011739E+03 0.29215000000579E+03
 0.29215000011407E+03 0.29996057366446E+03 0.29215000106482E+03 0.87261318035836E+02 0.87144288525927E+02
 0.16073893613391E+03 0.41319185632835E+03 0.25164922551377E+03 0.14533605756557E+03 0.15112920764826E+03
 0.14533605756557E+03 0.32903901658257E+03 0.14533215684280E+03 0.15101306446644E+03 0.14533215684280E+03
 0.32893364331859E+03 0.14533605756557E+03 0.15112920764826E+03 0.14533605756557E+03 0.32903901658257E+03
 0.14533215684280E+03 0.15101306446644E+03 0.14533215684280E+03 0.32893364331859E+03 0.21871112257398E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35421625255160E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17552572814505E+00 0.00000000000000E+00 0.00000000000000E+00 0.17552572814505E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18503371591164E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18503371591164E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17454946417648E+00 0.18683423859322E+00 0.32885167275630E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
