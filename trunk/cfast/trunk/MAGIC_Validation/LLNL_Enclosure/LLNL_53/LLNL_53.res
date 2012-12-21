#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-53 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL53 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.172000                                                                                   
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
#CONDINIT 1000.000000 10.000000 33.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-53           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-53           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-53           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-53           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-53           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-53           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-53           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-53           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-53           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-53           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-53           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-53           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-53           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-53           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-53           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-53           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-53           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-53           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL53     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL53     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL53     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL53     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL53     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL53     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL53     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL53     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL53     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL53     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL53     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL53     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL53     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL53     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL53     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL53     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL53     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL53     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.71678472154450E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.54435082205078E-03 0.54435082205078E-03 0.77101856587816E-03 0.77487365870755E-03
 0.00000000000000E+00 0.51583807036801E-03 0.60705595658254E-03 0.51583807036801E-03 0.60705595658254E-03
 0.51322686785616E-03 0.60704869313451E-03 0.51322686785616E-03 0.60704869313451E-03 0.51583807036801E-03
 0.60705595658254E-03 0.51583807036801E-03 0.60705595658254E-03 0.51322686785616E-03 0.60704869313451E-03
 0.51322686785616E-03 0.60704869313451E-03 0.62549002899996E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.60228330579320E-07 0.99966377844935E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.63391242294677E-03 0.63391242294677E-03
 0.77858963682099E-03 0.78248258500509E-03 0.00000000000000E+00 0.55410271104509E-03 0.61124221923079E-03
 0.55410271104509E-03 0.61124221923079E-03 0.54927996787464E-03 0.61123382457871E-03 0.54927996787464E-03
 0.61123382457871E-03 0.55410271098504E-03 0.61124221929084E-03 0.55410271098504E-03 0.61124221929084E-03
 0.54927996787464E-03 0.61123382457871E-03 0.54927996787464E-03 0.61123382457871E-03 0.50558688603612E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.45680635246408E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.45680635246408E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17199975321449E+00 0.19778430159363E+00 0.30615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     10.04934616
 0.30000000000000E+01 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.13531814861498E+02
 0.99986645137072E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30615000173537E+03 0.30615000000000E+03 0.30615000239386E+03 0.30615000239386E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000238155E+03 0.30615000238155E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000239386E+03 0.30615000239386E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000238155E+03 0.30615000238155E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000843824E+03
 0.30615000696484E+03 0.55985552707202E-03 0.55985552707202E-03 0.73593366325560E-03 0.73961333157188E-03
 .00000000000000E+00 0.52434033844579E-03 0.62306474303574E-03 0.52434033844579E-03 0.62306474303574E-03
 0.52162888936272E-03 0.62312920125078E-03 0.52162888936272E-03 0.62312920125078E-03 0.52434033844579E-03
 0.62306474309578E-03 0.52434033844579E-03 0.62306474309578E-03 0.52162888948281E-03 0.62312920119074E-03
 0.52162888948281E-03 0.62312920119074E-03 0.62582598137648E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.22999999928483E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.14956305442950E+02 0.99953251444253E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615000693896E+03 0.30615000846974E+03
 0.30615000257072E+03 0.30615000257072E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000254821E+03
 0.30615000254821E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000257072E+03 0.30615000257072E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000254821E+03 0.30615000254821E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000247636E+03 0.30615000000000E+03 0.60938060749801E-03 0.60938060749801E-03
 0.79294747378115E-03 0.79691221115006E-03 .00000000000000E+00 0.56304482132704E-03 0.61862565583279E-03
 0.56304482132704E-03 0.61862565583279E-03 0.55810376030200E-03 0.61874891468146E-03 0.55810376030200E-03
 0.61874891468146E-03 0.56304482126699E-03 0.61862565583279E-03 0.56304482126699E-03 0.61862565583279E-03
 0.55810376030200E-03 0.61874891474151E-03 0.55810376030200E-03 0.61874891474151E-03 0.50583079554524E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19726164953335E+00 0.00000000000000E+00 0.00000000000000E+00 0.19726164953335E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150905350E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150905350E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17157036085305E+00 0.19726140755010E+00 0.30615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     30.00000000
 0.30000000000000E+01 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.13531814826793E+02
 0.99986645137107E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30615000303853E+03 0.30615000000000E+03 0.30615000416118E+03 0.30615000416118E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000413954E+03 0.30615000413954E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000416118E+03 0.30615000416118E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000413954E+03 0.30615000413954E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615001412975E+03
 0.30615001169590E+03 0.56991063507376E-03 0.56991063507376E-03 0.71277377855969E-03 0.71633764745249E-03
 .00000000000000E+00 0.52956430226015E-03 0.63324710185532E-03 0.52956430226015E-03 0.63324710185532E-03
 0.52679619942330E-03 0.63336596114348E-03 0.52679619942330E-03 0.63336596114348E-03 0.52956430232019E-03
 0.63324710185532E-03 0.52956430232019E-03 0.63324710185532E-03 0.52679619930320E-03 0.63336596120352E-03
 0.52679619930320E-03 0.63336596120352E-03 0.62577382447794E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.22999999934819E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.14956305406983E+02 0.99956218436369E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615001167426E+03 0.30615001415589E+03
 0.30615000446818E+03 0.30615000446818E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000442890E+03
 0.30615000442890E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000446818E+03 0.30615000446818E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000442890E+03 0.30615000442890E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000430956E+03 0.30615000000000E+03 0.59326038192440E-03 0.59326038192440E-03
 0.80194641973433E-03 0.80595615183301E-03 .00000000000000E+00 0.56860918568915E-03 0.62322328237266E-03
 0.56860918568915E-03 0.62322328237266E-03 0.56360166800260E-03 0.62344467171623E-03 0.56360166800260E-03
 0.62344467171623E-03 0.56860918568915E-03 0.62322328231261E-03 0.56860918568915E-03 0.62322328231261E-03
 0.56360166788251E-03 0.62344467165618E-03 0.56360166788251E-03 0.62344467165618E-03 0.50579382167893E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19726164928039E+00 0.00000000000000E+00 0.00000000000000E+00 0.19726164928039E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150896636E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150896636E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17157036085408E+00 0.19726140755136E+00 0.30615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     40.00000000
 0.30000000000000E+01 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.13531814825799E+02
 0.99986645137108E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30615000354929E+03 0.30615000000000E+03 0.30615000484846E+03 0.30615000484846E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000482317E+03 0.30615000482317E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000484846E+03 0.30615000484846E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000482317E+03 0.30615000482317E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615001624306E+03
 0.30615001345819E+03 0.57361494763462E-03 0.57361494763462E-03 0.70435135022103E-03 0.70787310697214E-03
 .00000000000000E+00 0.53145710127467E-03 0.63694395197615E-03 0.53145710127467E-03 0.63694395197615E-03
 0.52867047789129E-03 0.63708408587447E-03 0.52867047789129E-03 0.63708408587447E-03 0.53145710127467E-03
 0.63694395191611E-03 0.53145710127467E-03 0.63694395191611E-03 0.52867047795134E-03 0.63708408581442E-03
 0.52867047795134E-03 0.63708408581442E-03 0.62576260426562E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.22999999937778E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.14956305405973E+02 0.99957603705299E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615001344189E+03 0.30615001626267E+03
 0.30615000520609E+03 0.30615000520609E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000516029E+03
 0.30615000516029E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000520609E+03 0.30615000520609E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000516029E+03 0.30615000516029E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000502400E+03 0.30615000000000E+03 0.58741872954169E-03 0.58741872954169E-03
 0.80522996911757E-03 0.80925611896316E-03 .00000000000000E+00 0.57063953957206E-03 0.62490073385852E-03
 0.57063953957206E-03 0.62490073385852E-03 0.56561099574513E-03 0.62516028655236E-03 0.56561099574513E-03
 0.62516028655236E-03 0.57063953963210E-03 0.62490073385852E-03 0.57063953963210E-03 0.62490073385852E-03
 0.56561099568508E-03 0.62516028649232E-03 0.56561099568508E-03 0.62516028649232E-03 0.50579040025535E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19726164927307E+00 0.00000000000000E+00 0.00000000000000E+00 0.19726164927307E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150896533E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150896533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17157036085411E+00 0.19726140755139E+00 0.30615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     40.00025000
 0.30000000000000E+01 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.13531814825795E+02
 0.99986645137108E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30615000354931E+03 0.30615000000000E+03 0.30615000484847E+03 0.30615000484847E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000482319E+03 0.30615000482319E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000484847E+03 0.30615000484847E+03 0.30615000000000E+03 0.30615000000000E+03
 0.30615000482319E+03 0.30615000482319E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615001624312E+03
 0.30615001345824E+03 0.57362227845411E-03 0.57362227845411E-03 0.70436151948114E-03 0.70788332707855E-03
 .00000000000000E+00 0.53146508046941E-03 0.63694978068829E-03 0.53146508046941E-03 0.63694978068829E-03
 0.52867852007414E-03 0.63708991488684E-03 0.52867852007414E-03 0.63708991488684E-03 0.53146508058950E-03
 0.63694978068829E-03 0.53146508058950E-03 0.63694978068829E-03 0.52867852001410E-03 0.63708991488684E-03
 0.52867852001410E-03 0.63708991488684E-03 0.62577161912903E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.30615000000000E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000000000E+03 0.22999999937778E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.14956305405819E+02 0.99957603739902E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615001344193E+03 0.30615001626272E+03
 0.30615000520611E+03 0.30615000520611E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000516031E+03
 0.30615000516031E+03 0.30615000000000E+03 0.30615000000000E+03 0.30615000520611E+03 0.30615000520611E+03
 0.30615000000000E+03 0.30615000000000E+03 0.30615000516031E+03 0.30615000516031E+03 0.30615000000000E+03
 0.30615000000000E+03 0.30615000502401E+03 0.30615000000000E+03 0.58742815037760E-03 0.58742815037760E-03
 0.80524929534218E-03 0.80927554181889E-03 .00000000000000E+00 0.57065281643331E-03 0.62491141289548E-03
 0.57065281643331E-03 0.62491141289548E-03 0.56562449291466E-03 0.62517096612973E-03 0.56562449291466E-03
 0.62517096612973E-03 0.57065281643331E-03 0.62491141295553E-03 0.57065281643331E-03 0.62491141295553E-03
 0.56562449285461E-03 0.62517096606969E-03 0.56562449285461E-03 0.62517096606969E-03 0.50580133223131E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19726164927307E+00 0.00000000000000E+00 0.00000000000000E+00 0.19726164927307E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150895528E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19726150895528E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17157036085412E+00 0.19726140755140E+00 0.30615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     50.00142610
 0.22196585035967E+01 0.30618358500346E+03 0.34201500068317E+03 0.31550383184671E+03 0.31476113712487E+03
 0.22999999998898E+00 0.00000000000000E+00 0.22593453020000E+00 0.00000000000000E+00 0.23208402523394E+02
 0.10001193348950E-02 0.71359764304234E-01 0.79990454347532E+04 0.29996420380324E+04 0.11210799360117E+03
 0.42040497600438E+02 0.30748060365849E+03 0.30615000000005E+03 0.30730468674359E+03 0.30772079688382E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30702970084288E+03 0.30770861225533E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30730468674359E+03 0.30772079688382E+03 0.30615000000004E+03 0.30615000000004E+03
 0.30702970084288E+03 0.30770861225533E+03 0.30615000000004E+03 0.30615000000004E+03 0.31043910021658E+03
 0.30615459730279E+03 0.61210753240072E+03 0.60959767780982E+03 0.28595500324178E+03 0.58770425839561E+03
 0.30031948013762E+03 0.39201393625834E+03 0.19376100472841E+03 0.39032130366716E+03 0.50424026557661E+03
 0.29504756535942E+03 0.18903788018721E+03 0.29387612500451E+03 0.49964525922878E+03 0.39201393625834E+03
 0.19376100472841E+03 0.39032130366716E+03 0.50424026557661E+03 0.29504756535942E+03 0.18903788018721E+03
 0.29387612500451E+03 0.49964525922877E+03 0.52191150272431E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.36742312086757E+03 0.12945335813775E+01
 0.12945335813775E+01 0.20022907327555E-01 0.13195270745702E+01 0.30616870515871E+03 0.31212375958544E+03
 0.30688518922103E+03 0.30687313634857E+03 0.22999999934729E+00 0.00000000000000E+00 0.22922650803967E+00
 0.00000000000000E+00 0.21959580351251E+02 0.99987904029418E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30615458693101E+03 0.31044957000896E+03
 0.30615224189878E+03 0.30632541360562E+03 0.30615000000004E+03 0.30615000000004E+03 0.30615225910838E+03
 0.30632542363675E+03 0.30615000000004E+03 0.30615000000004E+03 0.30615224189878E+03 0.30632541360562E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30615225910838E+03 0.30632542363675E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30622066718831E+03 0.30615000000005E+03 0.64811537656953E+00 0.65134844052185E+00
 0.52485667873783E+00 0.34131478172789E+02 0.33603997210657E+02 0.76603138088655E+00 -.30942472862517E+00
 0.77365581331360E+00 0.49012629122298E+02 0.77135694826592E+00 -.30575889089655E+00 0.77897121185009E+00
 0.49016200539041E+02 0.76603138088649E+00 -.30942472862523E+00 0.77365581331354E+00 0.49012629122298E+02
 0.77135694826592E+00 -.30575889089667E+00 0.77897121185009E+00 0.49016200539041E+02 0.10321753526254E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31818468729308E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24805134866356E+00 0.00000000000000E+00 0.00000000000000E+00 0.24805134866356E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18251903178941E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18251903178941E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17262822812460E+00 0.19853789688418E+00 0.30616870515871E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     60.03359730
 0.16608317431174E+01 0.30623102897907E+03 0.36737010889119E+03 0.33352286733685E+03 0.33080657075045E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22268641149348E+00 0.00000000000000E+00 0.12767280814874E+02
 0.99986136911184E-03 0.12444085381600E+00 0.80000000000000E+04 0.30000000000000E+04 0.64287569191940E+02
 0.24107838446978E+02 0.30862243777196E+03 0.30615000000005E+03 0.30851874113887E+03 0.30982123002333E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30793683468430E+03 0.30979369235231E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30851874113887E+03 0.30982123002333E+03 0.30615000000004E+03 0.30615000000004E+03
 0.30793683468430E+03 0.30979369235231E+03 0.30615000000004E+03 0.30615000000004E+03 0.31625479156151E+03
 0.30616318591790E+03 0.65765497407316E+03 0.65198482309958E+03 0.30782675901039E+03 0.82880166816279E+03
 0.51943577535735E+03 0.44930333084261E+03 0.26519687552096E+03 0.44490802215503E+03 0.74680310826352E+03
 0.33804211066812E+03 0.25969232859511E+03 0.33504699711758E+03 0.74157556173282E+03 0.44930333084261E+03
 0.26519687552096E+03 0.44490802215503E+03 0.74680310826352E+03 0.33804211066812E+03 0.25969232859511E+03
 0.33504699711759E+03 0.74157556173282E+03 0.68803433507394E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39301677494301E+03 0.12946104717610E+01
 0.12946104717610E+01 0.60151592158060E-01 0.11358296999420E+01 0.30616103827049E+03 0.31654129183447E+03
 0.30868115830721E+03 0.30861807822926E+03 0.22999999937202E+00 0.00000000000000E+00 0.22856400590114E+00
 0.00000000000000E+00 0.13068288206217E+02 0.99981632697652E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30616313988270E+03 0.31629256343242E+03
 0.30615579792753E+03 0.30651805764072E+03 0.30615000000004E+03 0.30615000000004E+03 0.30615580821644E+03
 0.30651811156848E+03 0.30615000000004E+03 0.30615000000004E+03 0.30615579792753E+03 0.30651805764072E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30615580821644E+03 0.30651811156848E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30635213339549E+03 0.30615000000005E+03 0.12249379419231E+01 0.12243400640749E+01
 0.20926162696590E+00 0.66820244354632E+02 0.66609936419531E+02 0.12665547313044E+01 -.82892967122990E+00
 0.12683896468975E+01 0.72433629036458E+02 0.12652217567402E+01 -.81375688806655E+00 0.12670521257238E+01
 0.72448352615337E+02 0.12665547313045E+01 -.82892967122990E+00 0.12683896468976E+01 0.72433629036458E+02
 0.12652217567401E+01 -.81375688806661E+00 0.12670521257237E+01 0.72448352615337E+02 0.21894777886507E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32644791245475E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.18906942761919E+00 0.00000000000000E+00 0.00000000000000E+00 0.18906942761919E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17079953734293E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17079953734293E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17237403898531E+00 0.19823312250657E+00 0.30616103827049E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     70.01048875
 0.12751222585214E+01 0.30632207680681E+03 0.38179036418528E+03 0.34971326650235E+03 0.34560018486573E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22041489050706E+00 0.00000000000000E+00 0.53550856848063E+01
 0.99949106965664E-03 0.15798843686486E+00 0.80000000000000E+04 0.30000000000000E+04 0.50636617202834E+02
 0.18988731451063E+02 0.30951320571808E+03 0.30615000000006E+03 0.30946719142578E+03 0.31180405056472E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30866525002778E+03 0.31176740435722E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30946719142578E+03 0.31180405056472E+03 0.30615000000004E+03 0.30615000000004E+03
 0.30866525002778E+03 0.31176740435722E+03 0.30615000000004E+03 0.30615000000004E+03 0.32133608743770E+03
 0.30617511171388E+03 0.71961089197695E+03 0.71128651869505E+03 0.34155024540773E+03 0.96936816341615E+03
 0.62611016678138E+03 0.49769596354567E+03 0.33836629724943E+03 0.49089719106317E+03 0.90980421022820E+03
 0.37930769441065E+03 0.33291822496480E+03 0.37467671137025E+03 0.90472063190458E+03 0.49769596354567E+03
 0.33836629724943E+03 0.49089719106317E+03 0.90980421022820E+03 0.37930769441065E+03 0.33291822496480E+03
 0.37467671137025E+03 0.90472063190458E+03 0.82985100723397E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40565264585244E+03 0.20472425348453E+01
 0.20472425348453E+01 0.10005915793637E+00 0.97565479679063E+00 0.30615590033191E+03 0.31987871225070E+03
 0.31095289406802E+03 0.31081700203939E+03 0.22999999945903E+00 0.00000000000000E+00 0.22796402359542E+00
 0.00000000000000E+00 0.69503386064113E+01 0.99977272408787E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30617504021654E+03 0.32137581496882E+03
 0.30616036803411E+03 0.30671015046760E+03 0.30615000000004E+03 0.30615000000004E+03 0.30616034794473E+03
 0.30671027368912E+03 0.30615000000004E+03 0.30615000000004E+03 0.30616036803411E+03 0.30671015046760E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30616034794473E+03 0.30671027368912E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30650550436535E+03 0.30615000000006E+03 0.19521680674338E+01 0.19427090213302E+01
 -.59898501488849E-01 0.93469047570662E+02 0.93529245564658E+02 0.19022631211337E+01 -.12012816012052E+01
 0.19007106984912E+01 0.89176748765508E+02 0.18937775386384E+01 -.11746594353233E+01 0.18922338814730E+01
 0.89202390888402E+02 0.19022631211337E+01 -.12012816012052E+01 0.19007106984912E+01 0.89176748765508E+02
 0.18937775386384E+01 -.11746594353232E+01 0.18922338814729E+01 0.89202390888402E+02 0.32622003164894E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33247870689272E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.14223497831216E+00 0.00000000000000E+00 0.00000000000000E+00 0.14223497831216E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16699579924295E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16699579924295E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17219891743860E+00 0.19654448238365E+00 0.30845912181907E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     80.18368630
 0.96747133676317E+00 0.30647084571289E+03 0.39215295606426E+03 0.36452129411815E+03 0.35972028146669E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21842013773111E+00 0.00000000000000E+00 0.51703062010765E-01
 0.99895360521776E-03 0.18629631479618E+00 0.80000000000000E+04 0.30000000000000E+04 0.42942341660126E+02
 0.16103378122547E+02 0.31048029329496E+03 0.30615000000007E+03 0.31037339391482E+03 0.31373427456803E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30938567364533E+03 0.31369241644634E+03 0.30615000000004E+03
 0.30615000000004E+03 0.31037339391482E+03 0.31373427456803E+03 0.30615000000004E+03 0.30615000000004E+03
 0.30938567364533E+03 0.31369241644634E+03 0.30615000000004E+03 0.30615000000004E+03 0.32599028703350E+03
 0.30619045403154E+03 0.82931608269030E+03 0.81803943777847E+03 0.36688475287618E+03 0.10628106617200E+04
 0.69409148507947E+03 0.54554527321606E+03 0.40153976544022E+03 0.53632683497642E+03 0.10336932511501E+04
 0.42329605334574E+03 0.39666366681404E+03 0.41699075828079E+03 0.10292313187522E+04 0.54554527321606E+03
 0.40153976544022E+03 0.53632683497642E+03 0.10336932511501E+04 0.42329605334574E+03 0.39666366681404E+03
 0.41699075828079E+03 0.10292313187522E+04 0.96542056513541E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41650298409071E+03 0.17890419331185E+01
 0.17890419331185E+01 0.14075194813546E+00 0.86005133403837E+00 0.30615264350675E+03 0.32271535815885E+03
 0.31321883493763E+03 0.31300624368733E+03 0.22999999944619E+00 0.00000000000000E+00 0.22736054012590E+00
 0.00000000000000E+00 0.27878554800157E+01 0.99974227697348E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30619035982262E+03 0.32602393552143E+03
 0.30616606180990E+03 0.30690907340129E+03 0.30615000000004E+03 0.30615000000004E+03 0.30616599401834E+03
 0.30690928238689E+03 0.30615000000004E+03 0.30615000000004E+03 0.30616606180990E+03 0.30690907340129E+03
 0.30615000000004E+03 0.30615000000004E+03 0.30616599401834E+03 0.30690928238689E+03 0.30615000000004E+03
 0.30615000000004E+03 0.30667348741675E+03 0.30615000000007E+03 0.27434951046831E+01 0.27214107206094E+01
 -.36770984000780E+00 0.11687318869903E+03 0.11724273708824E+03 0.25772209860658E+01 -.15595461779943E+01
 0.25708408703489E+01 0.10455417572280E+03 0.25607369218258E+01 -.15217454162634E+01 0.25543974993599E+01
 0.10459033425254E+03 0.25772209860658E+01 -.15595461779943E+01 0.25708408703490E+01 0.10455417572280E+03
 0.25607369218257E+01 -.15217454162634E+01 0.25543974993598E+01 0.10459033425254E+03 0.42527495736424E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33732916815285E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10160489757474E+00 0.00000000000000E+00 0.00000000000000E+00 0.10160489757474E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16498888991372E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16498888991372E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17207966714926E+00 0.19277978698882E+00 0.31425216065535E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
     90.00190132
 0.73624786756816E+00 0.30668781798726E+03 0.40025664926117E+03 0.37729336509575E+03 0.37237502800748E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21654870342229E+00 0.00000000000000E+00 -.34707052138915E+01
 0.99821217351506E-03 0.21227007738535E+00 0.80000000000000E+04 0.30000000000000E+04 0.37687836639721E+02
 0.14132938739895E+02 0.31147501781984E+03 0.30615000000009E+03 0.31123755943640E+03 0.31556749600564E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31009410271952E+03 0.31552285472557E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31123755943640E+03 0.31556749600564E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31009410271952E+03 0.31552285472557E+03 0.30615000000006E+03 0.30615000000006E+03 0.33023140647841E+03
 0.30620829125000E+03 0.93513540758031E+03 0.92086528520784E+03 0.39817208578311E+03 0.11417822516553E+04
 0.74161930544331E+03 0.59256422906957E+03 0.46413495706136E+03 0.58102271548715E+03 0.11399301936639E+04
 0.46799509101398E+03 0.45977565240089E+03 0.46006866143687E+03 0.11360104704819E+04 0.59256422906957E+03
 0.46413495706136E+03 0.58102271548715E+03 0.11399301936639E+04 0.46799509101398E+03 0.45977565240089E+03
 0.46006866143687E+03 0.11360104704819E+04 0.10888606015640E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42699027531534E+03 0.16103450036954E+01
 0.16103450036954E+01 0.18002480822002E+00 0.76846858098883E+00 0.30615099527377E+03 0.32514796568729E+03
 0.31541558242278E+03 0.31513014001255E+03 0.22999999944700E+00 0.00000000000000E+00 0.22675637371828E+00
 0.00000000000000E+00 0.21324064250152E+00 0.99972224840611E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30620817832055E+03 0.33026015558240E+03
 0.30617260030911E+03 0.30710592716710E+03 0.30615000000006E+03 0.30615000000006E+03 0.30617247500849E+03
 0.30710622889324E+03 0.30615000000006E+03 0.30615000000006E+03 0.30617260030911E+03 0.30710592716710E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30617247500849E+03 0.30710622889324E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30684498450280E+03 0.30615000000009E+03 0.35723541602847E+01 0.35351999915034E+01
 -.63702469813730E+00 0.13745029236909E+03 0.13809050219071E+03 0.32939148026472E+01 -.18686868524473E+01
 0.32819896278295E+01 0.11806709844361E+03 0.32700880896063E+01 -.18209557513281E+01 0.32582503061265E+01
 0.11811246559185E+03 0.32939148026471E+01 -.18686868524473E+01 0.32819896278295E+01 0.11806709844361E+03
 0.32700880896063E+01 -.18209557513283E+01 0.32582503061265E+01 0.11811246559185E+03 0.51616093327279E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34141150763254E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.66620143869163E-01 0.00000000000000E+00 0.00000000000000E+00 0.66620143869163E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16384059064729E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16384059064729E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17200586517416E+00 0.19183921694878E+00 0.31564944885063E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    100.12134653
 0.54734806839015E+00 0.30701625233934E+03 0.40723161963276E+03 0.38894739039574E+03 0.38434227053052E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21461526810237E+00 0.00000000000000E+00 -.57803316215239E+01
 0.99712159403403E-03 0.23877972677636E+00 0.80000000000000E+04 0.30000000000000E+04 0.33503681857769E+02
 0.12563880696663E+02 0.31254666938938E+03 0.30615000000009E+03 0.31213239863866E+03 0.31743043717127E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31084741945708E+03 0.31738422011565E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31213239863866E+03 0.31743043717127E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31084741945708E+03 0.31738422011565E+03 0.30615000000006E+03 0.30615000000006E+03 0.33442021292842E+03
 0.30624060474668E+03 0.10435323561027E+04 0.10262548551379E+04 0.43394535640933E+03 0.12111264270358E+04
 0.77501134384439E+03 0.64166396214911E+03 0.52892079332013E+03 0.62781076643341E+03 0.12385413075214E+04
 0.51577941122298E+03 0.52503105960458E+03 0.50625106911598E+03 0.12351042146505E+04 0.64166396214911E+03
 0.52892079332013E+03 0.62781076643341E+03 0.12385413075214E+04 0.51577941122298E+03 0.52503105960458E+03
 0.50625106911598E+03 0.12351042146505E+04 0.12072426192011E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43632151023083E+03 0.14558361775248E+01
 0.14558361775248E+01 0.22050258907589E+00 0.68494069919828E+00 0.30615072780801E+03 0.32741607243336E+03
 0.31770573908911E+03 0.31735051008137E+03 0.22999999944642E+00 0.00000000000000E+00 0.22609799403801E+00
 0.00000000000000E+00 -.12670114266992E+01 0.99970851201567E-03 0.15046998295645E-01 0.80000000000000E+04
 0.30000000000000E+04 0.53166750223635E+03 0.19937531333863E+03 0.30624045678559E+03 0.33444714341831E+03
 0.30618312028660E+03 0.30731711650139E+03 0.30615000000006E+03 0.30615000000006E+03 0.30618285649991E+03
 0.30731751997622E+03 0.30615000000006E+03 0.30615000000006E+03 0.30618312028660E+03 0.30731711650139E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30618285649991E+03 0.30731751997622E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30703193546717E+03 0.30615000000009E+03 0.62917907231494E+01 0.62204545032176E+01
 0.14865465308157E+01 0.15935161858060E+03 0.15785763931713E+03 0.51955396506693E+01 0.23059757392604E+00
 0.51751596281157E+01 0.13309755419825E+03 0.51360626371455E+01 0.28768307153399E+00 0.51158923463555E+01
 0.13315147883602E+03 0.51955396506693E+01 0.23059757392592E+00 0.51751596281157E+01 0.13309755419825E+03
 0.51360626371454E+01 0.28768307153406E+00 0.51158923463554E+01 0.13315147883602E+03 0.62142981499927E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34518432083757E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34033757947667E-01 0.00000000000000E+00 0.00000000000000E+00 0.34033757947667E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16350373384699E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16350373384699E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17190135212993E+00 0.19101978855676E+00 0.31680626278951E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    110.22904001
 0.39083576502328E+00 0.30749661751396E+03 0.41277563985752E+03 0.39906003744467E+03 0.39515032484527E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21270896202960E+00 0.00000000000000E+00 -.69921445920329E+01
 0.99555200298058E-03 0.26486756431714E+00 0.80000000000000E+04 0.30000000000000E+04 0.30203773801541E+02
 0.11326415175578E+02 0.31366008442933E+03 0.30615000000009E+03 0.31303601235971E+03 0.31925764304085E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31162579656978E+03 0.31921071845725E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31303601235971E+03 0.31925764304085E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31162579656978E+03 0.31921071845725E+03 0.30615000000006E+03 0.30615000000006E+03 0.33838273638622E+03
 0.30630149215795E+03 0.11493031893541E+04 0.11293688549905E+04 0.46888519868109E+03 0.12649888638616E+04
 0.79375923918707E+03 0.68996231603661E+03 0.59012939206285E+03 0.67399716366664E+03 0.13237572967347E+04
 0.56362462683219E+03 0.58664955902723E+03 0.55269062060683E+03 0.13207350369241E+04 0.68996231603661E+03
 0.59012939206285E+03 0.67399716366664E+03 0.13237572967347E+04 0.56362462683219E+03 0.58664955902723E+03
 0.55269062060683E+03 0.13207350369241E+04 0.13136866102062E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44376858107338E+03 0.13317689954244E+01
 0.13317689954244E+01 0.26093336297097E+00 0.61129348087969E+00 0.30615225021011E+03 0.32940868313191E+03
 0.31993101257617E+03 0.31951726672453E+03 0.22999999966536E+00 0.00000000000000E+00 0.22539902181619E+00
 0.00000000000000E+00 -.17841878763320E+01 0.99969843636748E-03 0.35489493609018E-01 0.80000000000000E+04
 0.30000000000000E+04 0.22541882643170E+03 0.84532059911886E+02 0.30630118195392E+03 0.33840978352670E+03
 0.30620115691717E+03 0.30754128038704E+03 0.30615000000006E+03 0.30615000000006E+03 0.30620059573892E+03
 0.30754178742726E+03 0.30615000000006E+03 0.30615000000006E+03 0.30620115691717E+03 0.30754128038704E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30620059573892E+03 0.30754178742726E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30723102181265E+03 0.30615000000009E+03 0.10427163148884E+02 0.10286978885710E+02
 0.54092309124722E+01 0.18054550143162E+03 0.17510922436458E+03 0.80242248729559E+01 0.41030895170127E+01
 0.79890944845480E+01 0.14826662286846E+03 0.79113872229783E+01 0.41680584366666E+01 0.78767662636546E+01
 0.14832761393212E+03 0.80242248729559E+01 0.41030895170127E+01 0.79890944845480E+01 0.14826662286846E+03
 0.79113872229787E+01 0.41680584366666E+01 0.78767662636551E+01 0.14832761393212E+03 0.73128477425435E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34801272003388E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10605014689724E-01 0.72170847214808E-02 0.00000000000000E+00 0.10605014689724E-01 0.72170847214808E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16394516590896E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16394516590896E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17188279091301E+00 0.18969349896227E+00 0.31898521774296E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    120.17273769
 0.27002218986853E+00 0.30816805203432E+03 0.41735622646312E+03 0.40752848314079E+03 0.40445772300002E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21080001718901E+00 0.00000000000000E+00 -.79164160100820E+01
 0.12473261199857E-02 0.29095531893593E+00 0.64137196133533E+04 0.24051448550075E+04 0.27495630701158E+02
 0.10310861512934E+02 0.31478785038811E+03 0.30615000000009E+03 0.31393568784995E+03 0.32102106968952E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31241617423435E+03 0.32097399488359E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31393568784995E+03 0.32102106968952E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31241617423435E+03 0.32097399488359E+03 0.30615000000006E+03 0.30615000000006E+03 0.34209385478050E+03
 0.30638339297580E+03 0.12481375139179E+04 0.12262505768204E+04 0.50255633113761E+03 0.13093535217104E+04
 0.80428440891714E+03 0.73618110410320E+03 0.64757162894978E+03 0.71850605662537E+03 0.13987358868507E+04
 0.61010518310639E+03 0.64443616607297E+03 0.59815946763323E+03 0.13960576654160E+04 0.73618110410320E+03
 0.64757162894978E+03 0.71850605662537E+03 0.13987358868507E+04 0.61010518310639E+03 0.64443616607297E+03
 0.59815946763323E+03 0.13960576654160E+04 0.14078664673163E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45072353150367E+03 0.12947628231468E+01
 0.12947628231468E+01 0.30070815370454E+00 0.55264103090626E+00 0.30615558449029E+03 0.33104669559955E+03
 0.32187612939700E+03 0.32141893199206E+03 0.22999999974515E+00 0.00000000000000E+00 0.22468560448677E+00
 0.00000000000000E+00 -.21430353418265E+01 0.99968400715294E-03 0.54520246606386E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14673447935327E+03 0.55025429757476E+02 0.30638290885250E+03 0.34212204890164E+03
 0.30622478253694E+03 0.30776789715943E+03 0.30615000000006E+03 0.30615000000006E+03 0.30622382789439E+03
 0.30776850442448E+03 0.30615000000006E+03 0.30615000000006E+03 0.30622478253694E+03 0.30776789715943E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30622382789439E+03 0.30776850442448E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30743139551155E+03 0.30615000000009E+03 0.14972360701249E+02 0.14726009973710E+02
 0.98631001859258E+01 0.19889654626048E+03 0.18898413057362E+03 0.11146778692998E+02 0.84708719840681E+01
 0.11091143979147E+02 0.16187827289982E+03 0.10979226694565E+02 0.85421393317934E+01 0.10924561945828E+02
 0.16194476425801E+03 0.11146778692998E+02 0.84708719840681E+01 0.11091143979147E+02 0.16187827289982E+03
 0.10979226694565E+02 0.85421393317933E+01 0.10924561945828E+02 0.16194476425801E+03 0.83164338980762E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34986475425729E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.77497424802005E-04 0.26820181045981E-01 0.00000000000000E+00 0.77497424802005E-04 0.26820181045981E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16447241659179E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16447241659179E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17186963715428E+00 0.18522725942990E+00 0.32665050093782E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    130.17727757
 0.18264106466510E+00 0.30905193074174E+03 0.42138665991996E+03 0.41454768174130E+03 0.41226371876974E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20880834452243E+00 0.00000000000000E+00 -.92295167499460E+01
 0.18440791272659E-02 0.31813155084031E+00 0.43382086385096E+04 0.16268282394411E+04 0.25146829916331E+02
 0.94300612186242E+01 0.31593985782047E+03 0.30615000000009E+03 0.31484873905981E+03 0.32275944106573E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31323155681067E+03 0.32271254265503E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31484873905981E+03 0.32275944106573E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31323155681067E+03 0.32271254265503E+03 0.30615000000006E+03 0.30615000000006E+03 0.34567249077042E+03
 0.30648459963429E+03 0.13396295516436E+04 0.13166022785402E+04 0.53576530809555E+03 0.13483614359669E+04
 0.80991730133088E+03 0.78067239942133E+03 0.70314344489490E+03 0.76174735069063E+03 0.14673327589009E+04
 0.65531811171297E+03 0.70029709745459E+03 0.64283582377479E+03 0.14649399166151E+04 0.78067239942133E+03
 0.70314344489490E+03 0.76174735069063E+03 0.14673327589009E+04 0.65531811171297E+03 0.70029709745459E+03
 0.64283582377479E+03 0.14649399166151E+04 0.14919404029110E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45815119844381E+03 0.12947724966167E+01
 0.12947724966167E+01 0.34072631321380E+00 0.51582585019363E+00 0.30616108014615E+03 0.33243416779532E+03
 0.32339927594609E+03 0.32290515403920E+03 0.22999999981433E+00 0.00000000000000E+00 0.22393373462054E+00
 0.00000000000000E+00 -.30047904989457E+01 0.99965755755298E-03 0.73310903822139E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10912428551432E+03 0.40921607067870E+02 0.30648397364549E+03 0.34570249917008E+03
 0.30625353594173E+03 0.30799883284184E+03 0.30615000000006E+03 0.30615000000006E+03 0.30625210423559E+03
 0.30799953820077E+03 0.30615000000006E+03 0.30615000000006E+03 0.30625353594173E+03 0.30799883284184E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30625210423559E+03 0.30799953820077E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30763390915878E+03 0.30615000000009E+03 0.19722115792380E+02 0.19328800065147E+02
 0.14652177361398E+02 0.21530244553560E+03 0.20057700728740E+03 0.14444543309255E+02 0.13151927095925E+02
 0.14363125112177E+02 0.17456318358282E+03 0.14220583458476E+02 0.13228623888955E+02 0.14140759127582E+02
 0.17463430927104E+03 0.14444543309255E+02 0.13151927095925E+02 0.14363125112177E+02 0.17456318358282E+03
 0.14220583458476E+02 0.13228623888955E+02 0.14140759127582E+02 0.17463430927104E+03 0.92149811581942E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35151872741321E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.56857951879188E-01 0.00000000000000E+00 0.00000000000000E+00 0.56857951879188E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16497797452078E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16497797452078E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17184232724058E+00 0.18197421967539E+00 0.33243416779532E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    140.06474575
 0.12621026595779E+00 0.31001657173783E+03 0.42489717914085E+03 0.42006414180294E+03 0.41837487810453E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20675867832435E+00 0.00000000000000E+00 -.10861454129614E+02
 0.26685921154525E-02 0.34608149197896E+00 0.29978354330270E+04 0.11241882873851E+04 0.23115942878813E+02
 0.86684785795548E+01 0.31708551475491E+03 0.30615000000009E+03 0.31575717956758E+03 0.32445684051752E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31405391384684E+03 0.32441032850274E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31575717956758E+03 0.32445684051752E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31405391384684E+03 0.32441032850274E+03 0.30615000000006E+03 0.30615000000006E+03 0.34907459813420E+03
 0.30660382144791E+03 0.14204708431082E+04 0.13966873390648E+04 0.56730000460687E+03 0.13808383103476E+04
 0.81070180571764E+03 0.82181709916285E+03 0.75551036145957E+03 0.80179480438742E+03 0.15309836568252E+04
 0.69734751870680E+03 0.75290198744887E+03 0.68450982434860E+03 0.15288239363145E+04 0.82181709916285E+03
 0.75551036145957E+03 0.80179480438742E+03 0.15309836568252E+04 0.69734751870680E+03 0.75290198744887E+03
 0.68450982434860E+03 0.15288239363145E+04 0.15651093940617E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46401841212615E+03 0.12947845191661E+01
 0.12947845191661E+01 0.38027618595021E+00 0.48313907770596E+00 0.30616967542540E+03 0.33359745155633E+03
 0.32476316458072E+03 0.32424171012718E+03 0.22999999981059E+00 0.00000000000000E+00 0.22314933669155E+00
 0.00000000000000E+00 -.43000666047502E+01 0.99961671026330E-03 0.91946587604676E-01 0.80000000000000E+04
 0.30000000000000E+04 0.87007035371404E+02 0.32627638264277E+02 0.30660308525462E+03 0.34910667095868E+03
 0.30628692488097E+03 0.30822910346751E+03 0.30615000000006E+03 0.30615000000006E+03 0.30628494932566E+03
 0.30822990099351E+03 0.30615000000006E+03 0.30615000000006E+03 0.30628692488097E+03 0.30822910346751E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30628494932566E+03 0.30822990099351E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30783492014140E+03 0.30615000000009E+03 0.24678010856446E+02 0.24095697615605E+02
 0.19776640246043E+02 0.23000835099148E+03 0.21013282754421E+03 0.17928014031656E+02 0.18132070965814E+02
 0.17816559350805E+02 0.18595901655321E+03 0.17648325771214E+02 0.18212575602000E+02 0.17539245339772E+02
 0.18603320323285E+03 0.17928014031656E+02 0.18132070965814E+02 0.17816559350805E+02 0.18595901655321E+03
 0.17648325771214E+02 0.18212575602000E+02 0.17539245339772E+02 0.18603320323285E+03 0.10052075479646E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35295444520474E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.83182452289723E-01 0.00000000000000E+00 0.00000000000000E+00 0.83182452289723E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16548742328934E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16548742328934E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17180312888309E+00 0.18129597629356E+00 0.33359745155633E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    151.09152479
 0.90386854156299E-01 0.31091815439003E+03 0.42824352260700E+03 0.42470863229171E+03 0.42342946878835E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20439155731882E+00 0.00000000000000E+00 -.12812156516317E+02
 0.37262395104282E-02 0.37842027813169E+00 0.21469366039438E+04 0.80510122647893E+03 0.21140516146484E+02
 0.79276935549316E+01 0.31833862409811E+03 0.30615000000009E+03 0.31675603284847E+03 0.32630513971061E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31496628606591E+03 0.32625916702544E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31675603284847E+03 0.32630513971061E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31496628606591E+03 0.32625916702544E+03 0.30615000000006E+03 0.30615000000006E+03 0.35267966832466E+03
 0.30675758557101E+03 0.14982643932396E+04 0.14729474191941E+04 0.59965349174426E+03 0.14088338061403E+04
 0.80618204693729E+03 0.86322608375874E+03 0.80965640380065E+03 0.84125414101558E+03 0.15948739456513E+04
 0.73968825237018E+03 0.80727165577596E+03 0.72580188940793E+03 0.15929318843671E+04 0.86322608375874E+03
 0.80965640380065E+03 0.84125414101559E+03 0.15948739456513E+04 0.73968825237018E+03 0.80727165577596E+03
 0.72580188940793E+03 0.15929318843671E+04 0.16336415157672E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46894701331869E+03 0.12947988904260E+01
 0.12947988904260E+01 0.42438330209945E+00 0.44822908811942E+00 0.30618467445954E+03 0.33468502399777E+03
 0.32616856687470E+03 0.32562776181928E+03 0.22999999980365E+00 0.00000000000000E+00 0.22222255469682E+00
 0.00000000000000E+00 -.59881570475546E+01 0.99955108288813E-03 0.11302790588159E+00 0.80000000000000E+04
 0.30000000000000E+04 0.70778980974669E+02 0.26542117865501E+02 0.30675673328012E+03 0.35271392058804E+03
 0.30632896454432E+03 0.30848315585135E+03 0.30615000000006E+03 0.30615000000006E+03 0.30632632460111E+03
 0.30848404815686E+03 0.30615000000006E+03 0.30615000000006E+03 0.30632896454432E+03 0.30848315585135E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30632632460111E+03 0.30848404815686E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30805590813293E+03 0.30615000000009E+03 0.30454062295742E+02 0.29611060833675E+02
 0.25858591842534E+02 0.24485708903499E+03 0.21886920423324E+03 0.22005147524908E+02 0.24028577326353E+02
 0.21857998692514E+02 0.19746288644751E+03 0.21664202967191E+02 0.24112019203321E+02 0.21520456822329E+02
 0.19753924599627E+03 0.22005147524908E+02 0.24028577326353E+02 0.21857998692514E+02 0.19746288644751E+03
 0.21664202967191E+02 0.24112019203321E+02 0.21520456822329E+02 0.19753924599627E+03 0.10936329949459E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35433848496540E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10825479515615E+00 0.00000000000000E+00 0.00000000000000E+00 0.10825479515615E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16616680221472E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16616680221472E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17175297612866E+00 0.18065108493660E+00 0.33468502399777E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    160.19940450
 0.75978778387895E-01 0.31138673302673E+03 0.43051259635186E+03 0.42749558382858E+03 0.42638140544524E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20240002511689E+00 0.00000000000000E+00 -.14327403704339E+02
 0.44328525276150E-02 0.40575741948408E+00 0.18047070030331E+04 0.67676512613743E+03 0.19716213717477E+02
 0.73935801440537E+01 0.31935528082664E+03 0.30615000000009E+03 0.31756923626317E+03 0.32780237706920E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31571466788041E+03 0.32775695919657E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31756923626317E+03 0.32780237706920E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31571466788041E+03 0.32775695919657E+03 0.30615000000006E+03 0.30615000000006E+03 0.35550428789780E+03
 0.30690530950851E+03 0.15522337049196E+04 0.15244313176660E+04 0.62268738540681E+03 0.14235912423412E+04
 0.79779042000734E+03 0.89309187946829E+03 0.84954944919297E+03 0.86849251094928E+03 0.16395302567273E+04
 0.77016847833998E+03 0.84732461519647E+03 0.75440933544358E+03 0.16377421779554E+04 0.89309187946829E+03
 0.84954944919297E+03 0.86849251094928E+03 0.16395302567273E+04 0.77016847833998E+03 0.84732461519648E+03
 0.75440933544358E+03 0.16377421779554E+04 0.16784153647883E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47186551342969E+03 0.12948100538576E+01
 0.12948100538576E+01 0.46081482093006E+00 0.42069402692691E+00 0.30620307490145E+03 0.33543389211575E+03
 0.32723573864625E+03 0.32668730247714E+03 0.22999999979623E+00 0.00000000000000E+00 0.22141422320444E+00
 0.00000000000000E+00 -.73611078562949E+01 0.99947746915106E-03 0.13080219381152E+00 0.80000000000000E+04
 0.30000000000000E+04 0.61161053701651E+02 0.22935395138119E+02 0.30690436121697E+03 0.35554000510514E+03
 0.30636860083698E+03 0.30869333950432E+03 0.30615000000006E+03 0.30615000000006E+03 0.30636536228085E+03
 0.30869430054110E+03 0.30615000000006E+03 0.30615000000006E+03 0.30636860083698E+03 0.30869333950432E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30636536228085E+03 0.30869430054110E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30823887769268E+03 0.30615000000009E+03 0.35368768630286E+02 0.34262868085951E+02
 0.31143258410110E+02 0.25594976003904E+03 0.22465078533688E+03 0.25547135185796E+02 0.29128031890016E+02
 0.25369807232314E+02 0.20606916703568E+03 0.25157994660678E+02 0.29212019415828E+02 0.24985049306636E+02
 0.20614552204108E+03 0.25547135185796E+02 0.29128031890016E+02 0.25369807232314E+02 0.20606916703568E+03
 0.25157994660678E+02 0.29212019415829E+02 0.24985049306636E+02 0.20614552204108E+03 0.11626159619712E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35531109114630E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12496083250481E+00 0.00000000000000E+00 0.00000000000000E+00 0.12496083250481E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16684282995388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16684282995388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17171253657829E+00 0.18020289254273E+00 0.33543389211575E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    171.07809669
 0.69814387973645E-01 0.31171788038754E+03 0.43261520726640E+03 0.42980174963851E+03 0.42874549888533E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20002239867277E+00 0.00000000000000E+00 -.15870805186568E+02
 0.48242558585929E-02 0.43863665458819E+00 0.16582868393579E+04 0.62185756475923E+03 0.18238329871248E+02
 0.68393737017179E+01 0.32051638538333E+03 0.30615000000009E+03 0.31850295869944E+03 0.32951446802776E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31657587159050E+03 0.32946974159287E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31850295869944E+03 0.32951446802776E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31657587159050E+03 0.32946974159287E+03 0.30615000000006E+03 0.30615000000006E+03 0.35864637053133E+03
 0.30710190400370E+03 0.16049349091859E+04 0.15732447181601E+04 0.64472571879869E+03 0.14308743411375E+04
 0.78292499374481E+03 0.92318069265801E+03 0.89022318210450E+03 0.89497043896596E+03 0.16817778429806E+04
 0.80080308145158E+03 0.88816825739461E+03 0.78220464040887E+03 0.16801522062417E+04 0.92318069265801E+03
 0.89022318210450E+03 0.89497043896596E+03 0.16817778429806E+04 0.80080308145158E+03 0.88816825739461E+03
 0.78220464040887E+03 0.16801522062417E+04 0.17180582971394E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47422119632775E+03 0.12948214249535E+01
 0.12948214249535E+01 0.50432958970763E+00 0.38940741178115E+00 0.30623427630150E+03 0.33616852989369E+03
 0.32839744975040E+03 0.32784895019914E+03 0.22999999978732E+00 0.00000000000000E+00 0.22039797205080E+00
 0.00000000000000E+00 -.88014125398811E+01 0.99936142339899E-03 0.15255876341534E+00 0.80000000000000E+04
 0.30000000000000E+04 0.52438809943812E+02 0.19664553728930E+02 0.30710083274502E+03 0.35868327806639E+03
 0.30641973495150E+03 0.30893916388974E+03 0.30615000000006E+03 0.30615000000006E+03 0.30641575610487E+03
 0.30894019645115E+03 0.30615000000006E+03 0.30615000000006E+03 0.30641973495150E+03 0.30893916388974E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30641575610487E+03 0.30894019645115E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30845219588735E+03 0.30615000000009E+03 0.41305667526546E+02 0.39839520505916E+02
 0.37613356517642E+02 0.26783797592304E+03 0.23003655262281E+03 0.29858007320816E+02 0.35365020359518E+02
 0.29650014460022E+02 0.21532767601724E+03 0.29414246087760E+02 0.35448703025464E+02 0.29211883810123E+02
 0.21540316006441E+03 0.29858007320815E+02 0.35365020359517E+02 0.29650014460022E+02 0.21532767601724E+03
 0.29414246087761E+02 0.35448703025464E+02 0.29211883810123E+02 0.21540316006441E+03 0.12397310808168E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35627185243173E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14032695115113E+00 0.00000000000000E+00 0.00000000000000E+00 0.14032695115113E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16772486799239E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16772486799239E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17167034255643E+00 0.17976235060818E+00 0.33616852989369E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    181.16990786
 0.69881877894818E-01 0.31197300191778E+03 0.43402133710045E+03 0.43117834814828E+03 0.43010184793164E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19785444789554E+00 0.00000000000000E+00 -.16932841834922E+02
 0.48195949489168E-02 0.46889209358768E+00 0.16598905270655E+04 0.62245894764958E+03 0.17061494764795E+02
 0.63980605367982E+01 0.32154638419711E+03 0.30615000000009E+03 0.31933623026463E+03 0.33102698382075E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31734575383644E+03 0.33098295658853E+03 0.30615000000006E+03
 0.30615000000006E+03 0.31933623026463E+03 0.33102698382075E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31734575383644E+03 0.33098295658853E+03 0.30615000000006E+03 0.30615000000006E+03 0.36132428331238E+03
 0.30730750443094E+03 0.16435942396760E+04 0.16081652780159E+04 0.65955483123288E+03 0.14282868551381E+04
 0.76543424974905E+03 0.94589786899008E+03 0.92075147183566E+03 0.91467031939198E+03 0.17099795391146E+04
 0.82390024474446E+03 0.91883827427484E+03 0.80282603813235E+03 0.17084879894351E+04 0.94589786899008E+03
 0.92075147183566E+03 0.91467031939198E+03 0.17099795391146E+04 0.82390024474446E+03 0.91883827427484E+03
 0.80282603813235E+03 0.17084879894351E+04 0.17426457906573E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47558021208347E+03 0.12948292497082E+01
 0.12948292497082E+01 0.54469683438129E+00 0.36199030962363E+00 0.30627501229960E+03 0.33670370068045E+03
 0.32936044046150E+03 0.32881990337832E+03 0.22999999973358E+00 0.00000000000000E+00 0.21940868011439E+00
 0.00000000000000E+00 -.98107582427981E+01 0.99921854569609E-03 0.17327825256594E+00 0.80000000000000E+04
 0.30000000000000E+04 0.46168517292471E+02 0.17313193984677E+02 0.30730632430965E+03 0.36136175913941E+03
 0.30647167762957E+03 0.30916360659510E+03 0.30615000000006E+03 0.30615000000006E+03 0.30646698786680E+03
 0.30916469259586E+03 0.30615000000006E+03 0.30615000000006E+03 0.30647167762957E+03 0.30916360659510E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30646698786680E+03 0.30916469259586E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30864679267291E+03 0.30615000000009E+03 0.46782905974120E+02 0.44934125648739E+02
 0.43702671069858E+02 0.27751524332419E+03 0.23359405889899E+03 0.33931780005015E+02 0.41214449544213E+02
 0.33704391329924E+02 0.22289049657986E+03 0.33441951182914E+02 0.41296187322798E+02 0.33221394517723E+02
 0.22296362128319E+03 0.33931780005015E+02 0.41214449544213E+02 0.33704391329925E+02 0.22289049657986E+03
 0.33441951182915E+02 0.41296187322798E+02 0.33221394517724E+02 0.22296362128319E+03 0.13055096028071E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35696952519810E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15013990454969E+00 0.00000000000000E+00 0.00000000000000E+00 0.15013990454969E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16843949017583E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16843949017583E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17164091584108E+00 0.17944407577021E+00 0.33670370068045E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    190.00024263
 0.72086986108087E-01 0.31220617146424E+03 0.43490058199191E+03 0.43195235856950E+03 0.43083215044517E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19600647569970E+00 0.00000000000000E+00 -.17666870966617E+02
 0.46721649468173E-02 0.49489077256110E+00 0.17122683148097E+04 0.64210061805366E+03 0.16165183195070E+02
 0.60619436981513E+01 0.32240849761525E+03 0.30615000000010E+03 0.32003801106927E+03 0.33228350727434E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31799439475125E+03 0.33224012119032E+03 0.30615000000006E+03
 0.30615000000006E+03 0.32003801106927E+03 0.33228350727434E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31799439475125E+03 0.33224012119032E+03 0.30615000000006E+03 0.30615000000006E+03 0.36347909171124E+03
 0.30750526130187E+03 0.16706999357682E+04 0.16321680500066E+04 0.66853795704253E+03 0.14202190642692E+04
 0.74833841744149E+03 0.96220545610385E+03 0.94222279975639E+03 0.92874088621278E+03 0.17271037774997E+04
 0.84048873206678E+03 0.94042323289676E+03 0.81755219272714E+03 0.17257187999251E+04 0.96220545610385E+03
 0.94222279975639E+03 0.92874088621278E+03 0.17271037774997E+04 0.84048873206678E+03 0.94042323289676E+03
 0.81755219272713E+03 0.17257187999251E+04 0.17563592308619E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47631714405712E+03 0.12948346578729E+01
 0.12948346578729E+01 0.58001817347074E+00 0.33925863090026E+00 0.30632121121773E+03 0.33707241720576E+03
 0.33011734251107E+03 0.32958904248317E+03 0.22999999972076E+00 0.00000000000000E+00 0.21850783854718E+00
 0.00000000000000E+00 -.10533701094684E+02 0.99906071368439E-03 0.19185284426126E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41698626000592E+02 0.15636984750222E+02 0.30750398019099E+03 0.36351672780093E+03
 0.30652027420638E+03 0.30935624892157E+03 0.30615000000006E+03 0.30615000000006E+03 0.30651495497646E+03
 0.30935737101775E+03 0.30615000000006E+03 0.30615000000006E+03 0.30652027420638E+03 0.30935624892157E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30651495497646E+03 0.30935737101775E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30881360158970E+03 0.30615000000009E+03 0.51490383980478E+02 0.49271438323978E+02
 0.49030582112167E+02 0.28505667426784E+03 0.23578093924512E+03 0.37513931674985E+02 0.46319552010823E+02
 0.37279879873683E+02 0.22882650839266E+03 0.36987795838338E+02 0.46398542525185E+02 0.36761610576446E+02
 0.22889661134460E+03 0.37513931674985E+02 0.46319552010823E+02 0.37279879873683E+02 0.22882650839266E+03
 0.36987795838338E+02 0.46398542525185E+02 0.36761610576446E+02 0.22889661134460E+03 0.13588237367599E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35747755390188E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15666839309255E+00 0.00000000000000E+00 0.00000000000000E+00 0.15666839309255E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16953983658275E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16953983658275E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17161998351079E+00 0.17922464628643E+00 0.33707241720576E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    200.37234768
 0.75510817829980E-01 0.31250552543624E+03 0.43562069782481E+03 0.43252185537337E+03 0.43134344424882E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19390611960532E+00 0.00000000000000E+00 -.18231362008475E+02
 0.44603179928333E-02 0.52464890380891E+00 0.17935940919132E+04 0.67259778446745E+03 0.15248292604675E+02
 0.57181097267529E+01 0.32337395106644E+03 0.30615000000011E+03 0.32082851699559E+03 0.33367709576271E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31872493116091E+03 0.33363447742416E+03 0.30615000000006E+03
 0.30615000000006E+03 0.32082851699559E+03 0.33367709576271E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31872493116091E+03 0.33363447742416E+03 0.30615000000006E+03 0.30615000000006E+03 0.36579977079571E+03
 0.30775798150301E+03 0.16962876252624E+04 0.16544094534227E+04 0.67521457488604E+03 0.14059134331818E+04
 0.72732278542132E+03 0.97792470902304E+03 0.96219976424756E+03 0.94222821862099E+03 0.17401154843595E+04
 0.85651726862702E+03 0.96052293444327E+03 0.83172550415201E+03 0.17388447578039E+04 0.97792470902304E+03
 0.96219976424756E+03 0.94222821862099E+03 0.17401154843595E+04 0.85651726862701E+03 0.96052293444326E+03
 0.83172550415201E+03 0.17388447578039E+04 0.17655321292938E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47683883515639E+03 0.12948388169557E+01
 0.12948388169557E+01 0.62150659367497E+00 0.31405355621701E+00 0.30639099414779E+03 0.33739710466482E+03
 0.33090538514993E+03 0.33039675829547E+03 0.22999999971220E+00 0.00000000000000E+00 0.21741233982707E+00
 0.00000000000000E+00 -.11096461699100E+02 0.99882761986482E-03 0.21416385115313E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37354576680076E+02 0.14007966255028E+02 0.30775658624014E+03 0.36583732289903E+03
 0.30658042298029E+03 0.30957679704168E+03 0.30615000000006E+03 0.30615000000006E+03 0.30657436946314E+03
 0.30957794881684E+03 0.30615000000006E+03 0.30615000000006E+03 0.30658042298029E+03 0.30957679704168E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30657436946314E+03 0.30957794881684E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30900421250633E+03 0.30615000000009E+03 0.56833822460995E+02 0.54146881208068E+02
 0.55201197458612E+02 0.29276843184523E+03 0.23729122839932E+03 0.41694678458207E+02 0.52216208981880E+02
 0.41471129993091E+02 0.23491757803329E+03 0.41131254876128E+02 0.52290869761051E+02 0.40916667010759E+02
 0.23498313634592E+03 0.41694678458207E+02 0.52216208981880E+02 0.41471129993091E+02 0.23491757803329E+03
 0.41131254876128E+02 0.52290869761051E+02 0.40916667010759E+02 0.23498313634592E+03 0.14157672895997E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35793155502245E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16153666024496E+00 0.00000000000000E+00 0.00000000000000E+00 0.16153666024496E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17054228008130E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17054228008130E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17160384718476E+00 0.17903434304766E+00 0.33739710466482E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    211.02474662
 0.78973491831994E-01 0.31283639894255E+03 0.43612195181022E+03 0.43287652160942E+03 0.43164398685579E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19183241801570E+00 0.00000000000000E+00 -.18588645483371E+02
 0.42647506210827E-02 0.55419910029355E+00 0.18758423905145E+04 0.70344089644293E+03 0.14435245376188E+02
 0.54132170160705E+01 0.32431797054678E+03 0.30615000000014E+03 0.32160584738095E+03 0.33502230266499E+03
 0.30615000000006E+03 0.30615000000006E+03 0.31944334460384E+03 0.33498049119712E+03 0.30615000000006E+03
 0.30615000000006E+03 0.32160584738095E+03 0.33502230266499E+03 0.30615000000006E+03 0.30615000000006E+03
 0.31944334460384E+03 0.33498049119712E+03 0.30615000000006E+03 0.30615000000006E+03 0.36796679971719E+03
 0.30804138035493E+03 0.17173291665613E+04 0.16723206006566E+04 0.67889233600525E+03 0.13881149103779E+04
 0.70582811269266E+03 0.99111146208029E+03 0.97810787269565E+03 0.95341485202633E+03 0.17476681632184E+04
 0.87002205545686E+03 0.97654624573835E+03 0.84358433626372E+03 0.17465037422377E+04 0.99111146208029E+03
 0.97810787269565E+03 0.95341485202633E+03 0.17476681632184E+04 0.87002205545686E+03 0.97654624573835E+03
 0.84358433626371E+03 0.17465037422377E+04 0.17695944287913E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47715041436890E+03 0.12948414493819E+01
 0.12948414493819E+01 0.66411618940978E+00 0.28981540986028E+00 0.30648328513503E+03 0.33762505757725E+03
 0.33160814721451E+03 0.33112439057951E+03 0.22999999969277E+00 0.00000000000000E+00 0.21625024546732E+00
 0.00000000000000E+00 -.11462310392808E+02 0.99852323702633E-03 0.23758296788683E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33672447445015E+02 0.12627167791881E+02 0.30803987450893E+03 0.36800408228013E+03
 0.30664570638441E+03 0.30979688456261E+03 0.30615000000006E+03 0.30615000000006E+03 0.30663891201796E+03
 0.30979805135214E+03 0.30615000000006E+03 0.30615000000006E+03 0.30664570638441E+03 0.30979688456261E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30663891201796E+03 0.30979805135214E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30919416463291E+03 0.30615000000009E+03 0.62034066134794E+02 0.58835750585085E+02
 0.61374223542737E+02 0.29952503218074E+03 0.23784393752029E+03 0.45933192308418E+02 0.58093142339932E+02
 0.45745607384938E+02 0.24027933925389E+03 0.45338050066109E+02 0.58162029511843E+02 0.45160265152485E+02
 0.24033903057387E+03 0.45933192308418E+02 0.58093142339932E+02 0.45745607384938E+02 0.24027933925389E+03
 0.45338050066109E+02 0.58162029511843E+02 0.45160265152485E+02 0.24033903057387E+03 0.14680682269864E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35828015300920E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16455641150902E+00 0.00000000000000E+00 0.00000000000000E+00 0.16455641150902E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17152639447751E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17152639447751E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17159357415734E+00 0.17890210831275E+00 0.33762505757725E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    221.67714555
 0.81918313979743E-01 0.31318285261654E+03 0.43647961569587E+03 0.43311286137900E+03 0.43183731130444E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18984215547875E+00 0.00000000000000E+00 -.18797071996584E+02
 0.41114401385074E-02 0.58266086862545E+00 0.19457902171730E+04 0.72967133143986E+03 0.13730113743304E+02
 0.51487926537391E+01 0.32521884365850E+03 0.30615000000020E+03 0.32235113678187E+03 0.33628895263726E+03
 0.30615000000006E+03 0.30615000000006E+03 0.32013225402660E+03 0.33624795473938E+03 0.30615000000006E+03
 0.30615000000006E+03 0.32235113678187E+03 0.33628895263726E+03 0.30615000000006E+03 0.30615000000006E+03
 0.32013225402660E+03 0.33624795473938E+03 0.30615000000006E+03 0.30615000000006E+03 0.36994666039718E+03
 0.30834809931460E+03 0.17346401282597E+04 0.16867626944047E+04 0.68040511514147E+03 0.13689867829552E+04
 0.68517964223802E+03 0.10021353306238E+04 0.99062887608661E+03 0.96262712145434E+03 0.17513618183250E+04
 0.88137462652269E+03 0.98917242447676E+03 0.85346600439926E+03 0.17502936514312E+04 0.10021353306238E+04
 0.99062887608661E+03 0.96262712145434E+03 0.17513618183250E+04 0.88137462652269E+03 0.98917242447676E+03
 0.85346600439926E+03 0.17502936514312E+04 0.17703098588194E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47735321571349E+03 0.12948429850520E+01
 0.12948429850520E+01 0.70672578514460E+00 0.26706293845257E+00 0.30660021354527E+03 0.33776169807840E+03
 0.33221364632776E+03 0.33175839904181E+03 0.22999999754199E+00 0.00000000000000E+00 0.21505753327484E+00
 0.00000000000000E+00 -.11682641323824E+02 0.99814025785171E-03 0.26142777190460E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30601186483429E+02 0.11475444931286E+02 0.30834648492278E+03 0.36998351808956E+03
 0.30671409275604E+03 0.31001000957792E+03 0.30615000000006E+03 0.30615000000006E+03 0.30670658161002E+03
 0.31001117563467E+03 0.30615000000006E+03 0.30615000000006E+03 0.30671409275604E+03 0.31001000957792E+03
 0.30615000000006E+03 0.30615000000006E+03 0.30670658161002E+03 0.31001117563467E+03 0.30615000000006E+03
 0.30615000000006E+03 0.30937781894862E+03 0.30615000000010E+03 0.66875059727681E+02 0.63148226641847E+02
 0.67307611948470E+02 0.30514316898809E+03 0.23749901897988E+03 0.50077739091016E+02 0.63719555076240E+02
 0.49955129415664E+02 0.24480936709276E+03 0.49457844815542E+02 0.63781550762551E+02 0.49345254628860E+02
 0.24486220065755E+03 0.50077739091015E+02 0.63719555076240E+02 0.49955129415664E+02 0.24480936709276E+03
 0.49457844815542E+02 0.63781550762551E+02 0.49345254628860E+02 0.24486220065755E+03 0.15141725726281E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35844083503399E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16628616075518E+00 0.00000000000000E+00 0.00000000000000E+00 0.16628616075518E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17242798652686E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17242798652686E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158765240596E+00 0.17882317349141E+00 0.33776169807840E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    230.55414466
 0.83880656776604E-01 0.31347727430229E+03 0.43671861352822E+03 0.43327275870279E+03 0.43197024143583E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18824436836727E+00 0.00000000000000E+00 -.18897224819558E+02
 0.40152551687930E-02 0.60554283847935E+00 0.19924013951036E+04 0.74715052316385E+03 0.13211286620266E+02
 0.49542324825997E+01 0.32594006556974E+03 0.30615000000031E+03 0.32294988983922E+03 0.33729123478174E+03
 0.30615000000006E+03 0.30615000000006E+03 0.32068585321838E+03 0.33725091264211E+03 0.30615000000006E+03
 0.30615000000006E+03 0.32294988983922E+03 0.33729123478174E+03 0.30615000000006E+03 0.30615000000006E+03
 0.32068585321838E+03 0.33725091264211E+03 0.30615000000006E+03 0.30615000000006E+03 0.37147542090888E+03
 0.30862086613882E+03 0.17470652557342E+04 0.16969558038838E+04 0.68060390640008E+03 0.13529317099373E+04
 0.66892478400523E+03 0.10101336182957E+04 0.99922199404092E+03 0.96920679846006E+03 0.17526040436288E+04
 0.88965609959306E+03 0.99784615673616E+03 0.86060852460922E+03 0.17516091296930E+04 0.10101336182957E+04
 0.99922199404092E+03 0.96920679846006E+03 0.17526040436288E+04 0.88965609959306E+03 0.99784615673616E+03
 0.86060852460922E+03 0.17516091296930E+04 0.17694322078213E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47749224318689E+03 0.12948437229717E+01
 0.12948437229717E+01 0.74223378159027E+00 0.24883175460486E+00 0.30671931964690E+03 0.33781095854364E+03
 0.33265323383683E+03 0.33222434690083E+03 0.22999998953653E+00 0.00000000000000E+00 0.21404880935877E+00
 0.00000000000000E+00 -.11792003485133E+02 0.99775157986157E-03 0.28149596627434E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28419590184121E+02 0.10657346319045E+02 0.30861916779562E+03 0.37151183739658E+03
 0.30677312920390E+03 0.31018178423671E+03 0.30615000000005E+03 0.30615000000006E+03 0.30676503968522E+03
 0.31018293770830E+03 0.30615000000005E+03 0.30615000000006E+03 0.30677312920390E+03 0.31018178423671E+03
 0.30615000000005E+03 0.30615000000006E+03 0.30676503968522E+03 0.31018293770830E+03 0.30615000000005E+03
 0.30615000000006E+03 0.30952550951955E+03 0.30615000000011E+03 0.70586788292119E+02 0.66418172845930E+02
 0.72014287662197E+02 0.30898735128752E+03 0.23661299218701E+03 0.53435376401103E+02 0.68162538785075E+02
 0.53435376401103E+02 0.24792275481827E+03 0.52800961099425E+02 0.68218008627132E+02 0.52800961099425E+02
 0.24796918696464E+03 0.53435376401103E+02 0.68162538785075E+02 0.53435376401103E+02 0.24792275481827E+03
 0.52800961099424E+02 0.68218008627132E+02 0.52800961099424E+02 0.24796918696464E+03 0.15478724365385E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35848671761124E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16709393847297E+00 0.00000000000000E+00 0.00000000000000E+00 0.16709393847297E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17309559284156E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17309559284156E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158497686000E+00 0.17879411600950E+00 0.33781095854364E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    240.00029592
 0.85477575741347E-01 0.31379899874771E+03 0.43694983297951E+03 0.43344095472596E+03 0.43211792089045E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18660106869038E+00 0.00000000000000E+00 -.18949904861033E+02
 0.39402408285538E-02 0.62907138101948E+00 0.20303327507360E+04 0.76137478152600E+03 0.12717157768384E+02
 0.47689341631440E+01 0.32668506445695E+03 0.30615000000049E+03 0.32357035447251E+03 0.33831300591101E+03
 0.30615000000006E+03 0.30615000000007E+03 0.32126009748491E+03 0.33827341893063E+03 0.30615000000006E+03
 0.30615000000007E+03 0.32357035447251E+03 0.33831300591101E+03 0.30615000000006E+03 0.30615000000007E+03
 0.32126009748491E+03 0.33827341893063E+03 0.30615000000006E+03 0.30615000000007E+03 0.37298939267971E+03
 0.30892900848682E+03 0.17588887131886E+04 0.17065472637249E+04 0.68032002138195E+03 0.13365965075815E+04
 0.65287488609260E+03 0.10177960883810E+04 0.10070901838793E+04 0.97543696122595E+03 0.17528806805924E+04
 0.89762785955487E+03 0.10057936564907E+04 0.86744187048349E+03 0.17519571551100E+04 0.10177960883810E+04
 0.10070901838793E+04 0.97543696122595E+03 0.17528806805924E+04 0.89762785955487E+03 0.10057936564907E+04
 0.86744187048348E+03 0.17519571551100E+04 0.17678382890455E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47764443968413E+03 0.12948441111154E+01
 0.12948441111154E+01 0.78001838661471E+00 0.23017520066640E+00 0.30687135844841E+03 0.33781518039612E+03
 0.33306684677866E+03 0.33266769062444E+03 0.22999998717431E+00 0.00000000000000E+00 0.21296350445732E+00
 0.00000000000000E+00 -.11851721560786E+02 0.99725665781330E-03 0.30300446209514E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26402251454264E+02 0.99008442953491E+01 0.30892721805196E+03 0.37302518972083E+03
 0.30683874470615E+03 0.31035945833810E+03 0.30615000000005E+03 0.30615000000006E+03 0.30683004455130E+03
 0.31036058419712E+03 0.30615000000005E+03 0.30615000000006E+03 0.30683874470615E+03 0.31035945833810E+03
 0.30615000000005E+03 0.30615000000006E+03 0.30683004455130E+03 0.31036058419712E+03 0.30615000000005E+03
 0.30615000000006E+03 0.30967842180882E+03 0.30615000000013E+03 0.74202629372611E+02 0.69573487517510E+02
 0.76781301957320E+02 0.31240704310458E+03 0.23524183463747E+03 0.56917919614244E+02 0.72643177165603E+02
 0.56917919614244E+02 0.25068753121690E+03 0.56274213196145E+02 0.72690892938962E+02 0.56274213196145E+02
 0.25072645397538E+03 0.56917919614244E+02 0.72643177165603E+02 0.56917919614244E+02 0.25068753121690E+03
 0.56274213196145E+02 0.72690892938962E+02 0.56274213196145E+02 0.25072645397538E+03 0.15795349565403E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35850056587193E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16748348230762E+00 0.00000000000000E+00 0.00000000000000E+00 0.16748348230762E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17371024744848E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17371024744848E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158384365256E+00 0.17879059529995E+00 0.33781518039612E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    250.00793412
 0.86723976412659E-01 0.31413446319500E+03 0.43718773093572E+03 0.43363050803937E+03 0.43229249988444E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18491987900926E+00 0.00000000000000E+00 -.18990243758454E+02
 0.38836113922537E-02 0.65311444436512E+00 0.20599383388248E+04 0.77247687705929E+03 0.12249001792904E+02
 0.45933756723391E+01 0.32744694489850E+03 0.30615000000084E+03 0.32420617198549E+03 0.33935007595281E+03
 0.30615000000008E+03 0.30615000000010E+03 0.32184863523472E+03 0.33931124035885E+03 0.30615000000008E+03
 0.30615000000010E+03 0.32420617198549E+03 0.33935007595281E+03 0.30615000000008E+03 0.30615000000010E+03
 0.32184863523472E+03 0.33931124035885E+03 0.30615000000008E+03 0.30615000000010E+03 0.37450672957191E+03
 0.30927200533132E+03 0.17702675199429E+04 0.17156577874633E+04 0.67947355737440E+03 0.13198410835773E+04
 0.63697015841600E+03 0.10252056808423E+04 0.10143888597542E+04 0.98136212050597E+03 0.17525113946059E+04
 0.90537146572585E+03 0.10131696412631E+04 0.87401148936410E+03 0.17516571215327E+04 0.10252056808423E+04
 0.10143888597542E+04 0.98136212050597E+03 0.17525113946059E+04 0.90537146572585E+03 0.10131696412631E+04
 0.87401148936410E+03 0.17516571215327E+04 0.17657072509726E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47782209308441E+03 0.12948444083303E+01
 0.12948444083303E+01 0.82004893940187E+00 0.21125444856454E+00 0.30706390011124E+03 0.33777961869980E+03
 0.33345373057134E+03 0.33308711786080E+03 0.22999998729702E+00 0.00000000000000E+00 0.21180283029232E+00
 0.00000000000000E+00 -.11896575165483E+02 0.99663089559639E-03 0.32592556512494E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24545481717377E+02 0.92045556440164E+01 0.30927013358733E+03 0.37454182903894E+03
 0.30690926721154E+03 0.31054065268585E+03 0.30615000000007E+03 0.30615000000007E+03 0.30690007431092E+03
 0.31054173820101E+03 0.30615000000007E+03 0.30615000000007E+03 0.30690926721154E+03 0.31054065268585E+03
 0.30615000000007E+03 0.30615000000007E+03 0.30690007431092E+03 0.31054173820101E+03 0.30615000000007E+03
 0.30615000000007E+03 0.30983410557196E+03 0.30615000000018E+03 0.77662315645068E+02 0.72578981732382E+02
 0.81546185855831E+02 0.31541856140700E+03 0.23346464462189E+03 0.60486210594934E+02 0.77105453811224E+02
 0.60676869982298E+02 0.25311527253267E+03 0.59838117217427E+02 0.77144535646977E+02 0.60042999501708E+02
 0.25314590635093E+03 0.60486210594934E+02 0.77105453811224E+02 0.60676869982298E+02 0.25311527253267E+03
 0.59838117217427E+02 0.77144535646977E+02 0.60042999501708E+02 0.25314590635093E+03 0.16091574408962E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35849049000322E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16775382555053E+00 0.00000000000000E+00 0.00000000000000E+00 0.16775382555053E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17428425334183E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17428425334183E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158323440579E+00 0.17880870441835E+00 0.33777961869980E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    260.01505932
 0.87611059995796E-01 0.31446363838590E+03 0.43743168942577E+03 0.43384056899337E+03 0.43249269752540E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18329616156290E+00 0.00000000000000E+00 -.19014797327697E+02
 0.38442886964477E-02 0.67628871163349E+00 0.20810091623432E+04 0.78037843587870E+03 0.11829267385932E+02
 0.44359752697245E+01 0.32818404264863E+03 0.30615000000143E+03 0.32482254588033E+03 0.34034561553153E+03
 0.30615000000012E+03 0.30615000000015E+03 0.32241944061495E+03 0.34030751016286E+03 0.30615000000011E+03
 0.30615000000015E+03 0.32482254588033E+03 0.34034561553153E+03 0.30615000000012E+03 0.30615000000015E+03
 0.32241944061495E+03 0.34030751016286E+03 0.30615000000011E+03 0.30615000000015E+03 0.37594094143197E+03
 0.30963321041292E+03 0.17807545942225E+04 0.17239568378807E+04 0.67835851496752E+03 0.13040043579780E+04
 0.62225405043559E+03 0.10320588168623E+04 0.10209230358225E+04 0.98675668813302E+03 0.17518281439563E+04
 0.91256161145936E+03 0.10197748499392E+04 0.88005109252980E+03 0.17510371255760E+04 0.10320588168623E+04
 0.10209230358225E+04 0.98675668813302E+03 0.17518281439563E+04 0.91256161145936E+03 0.10197748499392E+04
 0.88005109252980E+03 0.17510371255760E+04 0.17634696416548E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47802401716227E+03 0.12948445892399E+01
 0.12948445892399E+01 0.86007744023214E+00 0.19320777180748E+00 0.30729081821497E+03 0.33771585021687E+03
 0.33379694845670E+03 0.33346317057705E+03 0.22999998744498E+00 0.00000000000000E+00 0.21063325108938E+00
 0.00000000000000E+00 -.11922223587329E+02 0.99589468390380E-03 0.34894870049667E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22926005996335E+02 0.85972522486256E+01 0.30963127266831E+03 0.37597528338267E+03
 0.30698170344531E+03 0.31071521583419E+03 0.30615000000008E+03 0.30615000000009E+03 0.30697202344702E+03
 0.31071624904790E+03 0.30615000000008E+03 0.30615000000009E+03 0.30698170344531E+03 0.31071521583419E+03
 0.30615000000008E+03 0.30615000000009E+03 0.30697202344702E+03 0.31071624904790E+03 0.30615000000008E+03
 0.30615000000009E+03 0.30998397041010E+03 0.30615000000026E+03 0.80740056596323E+02 0.75243532534658E+02
 0.86035108981510E+02 0.31793833968217E+03 0.23147305515576E+03 0.63944740132308E+02 0.81291431322966E+02
 0.64407621876401E+02 0.25513698650445E+03 0.63298585332433E+02 0.81321385671256E+02 0.63779659344837E+02
 0.25515892868062E+03 0.63944740132308E+02 0.81291431322966E+02 0.64407621876401E+02 0.25513698650445E+03
 0.63298585332434E+02 0.81321385671256E+02 0.63779659344837E+02 0.25515892868062E+03 0.16352725390167E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35846626089265E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16787666194795E+00 0.00000000000000E+00 0.00000000000000E+00 0.16787666194795E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17477654335091E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17477654335091E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158321021193E+00 0.17884239708631E+00 0.33771585021687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    270.01110519
 0.88226873110898E-01 0.31478641302531E+03 0.43768896494129E+03 0.43407452899032E+03 0.43272038921258E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18172784464334E+00 0.00000000000000E+00 -.19032550897790E+02
 0.38174558025686E-02 0.69861314188348E+00 0.20956365741333E+04 0.78586371529998E+03 0.11451258959188E+02
 0.42942221096956E+01 0.32889857764671E+03 0.30615000000237E+03 0.32542113544629E+03 0.34130408838920E+03
 0.30615000000017E+03 0.30615000000021E+03 0.32297410419777E+03 0.34126669184950E+03 0.30615000000015E+03
 0.30615000000021E+03 0.32542113544629E+03 0.34130408838920E+03 0.30615000000017E+03 0.30615000000021E+03
 0.32297410419777E+03 0.34126669184950E+03 0.30615000000015E+03 0.30615000000021E+03 0.37730168156237E+03
 0.31001235253997E+03 0.17905472497550E+04 0.17316319434163E+04 0.67712227829327E+03 0.12891794082216E+04
 0.60867151853691E+03 0.10384756484508E+04 0.10268988409678E+04 0.99174055734259E+03 0.17510569200504E+04
 0.91931566646267E+03 0.10258159133252E+04 0.88567630801125E+03 0.17503236599174E+04 0.10384756484508E+04
 0.10268988409678E+04 0.99174055734259E+03 0.17510569200504E+04 0.91931566646266E+03 0.10258159133252E+04
 0.88567630801125E+03 0.17503236599174E+04 0.17613110304092E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47825246991364E+03 0.12948447200474E+01
 0.12948447200474E+01 0.90006162369254E+00 0.17605479389026E+00 0.30755508331033E+03 0.33763486873934E+03
 0.33410440845669E+03 0.33380310439454E+03 0.22999998759896E+00 0.00000000000000E+00 0.20945778290732E+00
 0.00000000000000E+00 -.11937775973084E+02 0.99503881381192E-03 0.37202054457141E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21504188724890E+02 0.80640707718337E+01 0.31001035283205E+03 0.37733512888763E+03
 0.30705594749284E+03 0.31088354506965E+03 0.30615000000009E+03 0.30615000000010E+03 0.30704585420426E+03
 0.31088451465141E+03 0.30615000000009E+03 0.30615000000010E+03 0.30705594749284E+03 0.31088354506965E+03
 0.30615000000009E+03 0.30615000000010E+03 0.30704585420426E+03 0.31088451465141E+03 0.30615000000009E+03
 0.30615000000010E+03 0.31012841733651E+03 0.30615000000036E+03 0.83442602398838E+02 0.77587564798273E+02
 0.90258528574313E+02 0.32007998280151E+03 0.22937016158433E+03 0.67299743263065E+02 0.85213741188363E+02
 0.68162053796991E+02 0.25684513482494E+03 0.66660955782419E+02 0.85234218270208E+02 0.67545156661999E+02
 0.25685812000137E+03 0.67299743263065E+02 0.85213741188363E+02 0.68162053796991E+02 0.25684513482494E+03
 0.66660955782419E+02 0.85234218270208E+02 0.67545156661999E+02 0.25685812000137E+03 0.16584272640037E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35843748101838E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16792864719192E+00 0.00000000000000E+00 0.00000000000000E+00 0.16792864719192E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17519608497804E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17519608497804E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158348337499E+00 0.17888554962354E+00 0.33763486873934E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    280.00030608
 0.88647999999723E-01 0.31510266735706E+03 0.43796265990173E+03 0.43433222902871E+03 0.43297418154612E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18021095475836E+00 0.00000000000000E+00 -.19048405829570E+02
 0.37993205320495E-02 0.72013965834521E+00 0.21056396617541E+04 0.78961487315778E+03 0.11108956307701E+02
 0.41658586153881E+01 0.32959326529087E+03 0.30615000000389E+03 0.32600405026504E+03 0.34223054961087E+03
 0.30615000000024E+03 0.30615000000031E+03 0.32351459452836E+03 0.34219384018658E+03 0.30615000000021E+03
 0.30615000000031E+03 0.32600405026504E+03 0.34223054961087E+03 0.30615000000024E+03 0.30615000000031E+03
 0.32351459452836E+03 0.34219384018658E+03 0.30615000000021E+03 0.30615000000031E+03 0.37859944113038E+03
 0.31040945991396E+03 0.17997854620883E+04 0.17388098124834E+04 0.67584055924183E+03 0.12753327646623E+04
 0.59611300262424E+03 0.10445444466716E+04 0.10324576248902E+04 0.99639804773765E+03 0.17503179423620E+04
 0.92572020566576E+03 0.10314347428252E+04 0.89096914927969E+03 0.17496375027183E+04 0.10445444466716E+04
 0.10324576248902E+04 0.99639804773765E+03 0.17503179423620E+04 0.92572020566576E+03 0.10314347428252E+04
 0.89096914927969E+03 0.17496375027183E+04 0.17593121209438E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47850639243289E+03 0.12948448368658E+01
 0.12948448368658E+01 0.94001842726602E+00 0.15978457892752E+00 0.30785947841412E+03 0.33754535031899E+03
 0.33438312062405E+03 0.33411344861458E+03 0.22999998776585E+00 0.00000000000000E+00 0.20827730341262E+00
 0.00000000000000E+00 -.11948454999614E+02 0.10794116762472E-02 0.39512832226950E+00 0.74114447490633E+04
 0.27792917808987E+04 0.20246587118965E+02 0.75924701696120E+01 0.31040740230930E+03 0.37863186380306E+03
 0.30713223989223E+03 0.31104628387900E+03 0.30615000000011E+03 0.30615000000013E+03 0.30712179891242E+03
 0.31104717938044E+03 0.30615000000011E+03 0.30615000000013E+03 0.30713223989223E+03 0.31104628387900E+03
 0.30615000000011E+03 0.30615000000013E+03 0.30712179891242E+03 0.31104717938044E+03 0.30615000000011E+03
 0.30615000000013E+03 0.31026802834912E+03 0.30615000000052E+03 0.85784207117098E+02 0.79637317418353E+02
 0.94236893173414E+02 0.32194242145449E+03 0.22723434381521E+03 0.70560840245642E+02 0.88894322344275E+02
 0.71974922629123E+02 0.25832059112616E+03 0.69934436764691E+02 0.88905100667565E+02 0.71374004560780E+02
 0.25832447419272E+03 0.70560840245642E+02 0.88894322344275E+02 0.71974922629123E+02 0.25832059112616E+03
 0.69934436764691E+02 0.88905100667564E+02 0.71374004560780E+02 0.25832447419272E+03 0.16791301676859E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35841138763964E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16795261450078E+00 0.00000000000000E+00 0.00000000000000E+00 0.16795261450078E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17555319918240E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17555319918240E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158388438542E+00 0.17893339012981E+00 0.33754535031899E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    290.00219019
 0.88934962030604E-01 0.31541289335634E+03 0.43825369825737E+03 0.43461208415081E+03 0.43325157224913E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17873988854324E+00 0.00000000000000E+00 -.19064864689180E+02
 0.37870611730685E-02 0.74094687563650E+00 0.21124559742767E+04 0.79217099035378E+03 0.10796995389349E+02
 0.40488732710060E+01 0.33027140053891E+03 0.30615000000627E+03 0.32657393016030E+03 0.34313054965452E+03
 0.30615000000034E+03 0.30615000000047E+03 0.32404336050785E+03 0.34309450646885E+03 0.30615000000029E+03
 0.30615000000047E+03 0.32657393016030E+03 0.34313054965452E+03 0.30615000000034E+03 0.30615000000047E+03
 0.32404336050785E+03 0.34309450646885E+03 0.30615000000029E+03 0.30615000000047E+03 0.37984459878160E+03
 0.31082519386795E+03 0.18085740057202E+04 0.17455815502934E+04 0.67454923043296E+03 0.12623711914826E+04
 0.58444921489752E+03 0.10503333151101E+04 0.10376977614446E+04 0.10007921459127E+04 0.17496652434436E+04
 0.93184267295620E+03 0.10367302996035E+04 0.89599198150393E+03 0.17490332583464E+04 0.10503333151101E+04
 0.10376977614446E+04 0.10007921459127E+04 0.17496652434436E+04 0.93184267295620E+03 0.10367302996035E+04
 0.89599198150393E+03 0.17490332583464E+04 0.17574947921244E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47878353742550E+03 0.12948449581340E+01
 0.12948449581340E+01 0.98002596368960E+00 0.14436226950224E+00 0.30820710701202E+03 0.33745387619216E+03
 0.33463912287585E+03 0.33439991266654E+03 0.22999998795477E+00 0.00000000000000E+00 0.20709052876426E+00
 0.00000000000000E+00 -.11957137081763E+02 0.11947257231581E-02 0.41830026982264E+00 0.66960975602441E+04
 0.25110365850915E+04 0.19125017546348E+02 0.71718815798804E+01 0.31082308384218E+03 0.37987588725199E+03
 0.30721097428705E+03 0.31120430306802E+03 0.30615000000012E+03 0.30615000000016E+03 0.30720025074725E+03
 0.31120511485212E+03 0.30615000000012E+03 0.30615000000016E+03 0.30721097428705E+03 0.31120430306802E+03
 0.30615000000012E+03 0.30615000000016E+03 0.30720025074725E+03 0.31120511485212E+03 0.30615000000012E+03
 0.30615000000016E+03 0.31040356853604E+03 0.30615000000078E+03 0.87787714970328E+02 0.81426225325621E+02
 0.97997775002709E+02 0.32360942222983E+03 0.22512165835211E+03 0.73745457857429E+02 0.92361333203107E+02
 0.75894072862378E+02 0.25963197409288E+03 0.73136001044183E+02 0.92362282820858E+02 0.75313640660161E+02
 0.25962669434116E+03 0.73745457857429E+02 0.92361333203107E+02 0.75894072862378E+02 0.25963197409288E+03
 0.73136001044182E+02 0.92362282820858E+02 0.75313640660161E+02 0.25962669434116E+03 0.16978590756288E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35839333386162E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16797125561971E+00 0.00000000000000E+00 0.00000000000000E+00 0.16797125561971E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17585821507309E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17585821507309E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158431681841E+00 0.17898232960604E+00 0.33745387619216E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    300.01043521
 0.89132111150376E-01 0.31571697571284E+03 0.43856087371414E+03 0.43491109505721E+03 0.43354892398943E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17731344575504E+00 0.00000000000000E+00 -.19082957911684E+02
 0.37786843623438E-02 0.76105322312837E+00 0.21171389914764E+04 0.79392712180364E+03 0.10511748399298E+02
 0.39419056497367E+01 0.33093414570274E+03 0.30615000000993E+03 0.32713165590125E+03 0.34400655292826E+03
 0.30615000000049E+03 0.30615000000071E+03 0.32456121389569E+03 0.34397115443112E+03 0.30615000000041E+03
 0.30615000000071E+03 0.32713165590126E+03 0.34400655292826E+03 0.30615000000049E+03 0.30615000000071E+03
 0.32456121389569E+03 0.34397115443112E+03 0.30615000000041E+03 0.30615000000071E+03 0.38104258267019E+03
 0.31125915920143E+03 0.18169643516709E+04 0.17519929264396E+04 0.67326594502916E+03 0.12502217166090E+04
 0.57358944185468E+03 0.10558765082149E+04 0.10426710456602E+04 0.10049566950348E+04 0.17491168127405E+04
 0.93771623156605E+03 0.10417547614559E+04 0.90077697876531E+03 0.17485292996749E+04 0.10558765082149E+04
 0.10426710456602E+04 0.10049566950348E+04 0.17491168127405E+04 0.93771623156605E+03 0.10417547614559E+04
 0.90077697876531E+03 0.17485292996749E+04 0.17558562150336E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47908044866066E+03 0.12948450914441E+01
 0.12948450914441E+01 0.10200589437798E+01 0.12979413975685E+00 0.30859972044005E+03 0.33736567155182E+03
 0.33487657029926E+03 0.33466632250076E+03 0.22999998818194E+00 0.00000000000000E+00 0.20589896448108E+00
 0.00000000000000E+00 -.11965244287388E+02 0.13288218951745E-02 0.44150826049123E+00 0.60203703965531E+04
 0.22576388987074E+04 0.18119706279332E+02 0.67948898547496E+01 0.31125700341647E+03 0.38107265321771E+03
 0.30729241173289E+03 0.31135803370963E+03 0.30615000000013E+03 0.30615000000020E+03 0.30728147096992E+03
 0.31135875316100E+03 0.30615000000013E+03 0.30615000000020E+03 0.30729241173289E+03 0.31135803370963E+03
 0.30615000000013E+03 0.30615000000020E+03 0.30728147096992E+03 0.31135875316100E+03 0.30615000000013E+03
 0.30615000000020E+03 0.31053541877381E+03 0.30615000000118E+03 0.89469005123526E+02 0.82979165925070E+02
 0.10155836075262E+03 0.32514445068049E+03 0.22307829812411E+03 0.76861401209939E+02 0.95633053032877E+02
 0.79959259396289E+02 0.26083159034557E+03 0.76273002959133E+02 0.95624147161154E+02 0.79403370954375E+02
 0.26081718169952E+03 0.76861401209939E+02 0.95633053032877E+02 0.79959259396289E+02 0.26083159034557E+03
 0.76273002959133E+02 0.95624147161154E+02 0.79403370954375E+02 0.26081718169952E+03 0.17149655985886E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35838728362515E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16799511376816E+00 0.00000000000000E+00 0.00000000000000E+00 0.16799511376816E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17611973977708E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17611973977708E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158472789003E+00 0.17902953931710E+00 0.33736567155182E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    310.01655733
 0.89271358017324E-01 0.31601491499561E+03 0.43888218952020E+03 0.43522601336930E+03 0.43386255953545E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17593093844675E+00 0.00000000000000E+00 -.19102927941366E+02
 0.37727899894747E-02 0.78047145834202E+00 0.21204466780071E+04 0.79516750425265E+03 0.10250214680489E+02
 0.38438305051833E+01 0.33158231781371E+03 0.30615000001544E+03 0.32767783670432E+03 0.34486037410237E+03
 0.30615000000072E+03 0.30615000000109E+03 0.32506871006459E+03 0.34482559826534E+03 0.30615000000059E+03
 0.30615000000109E+03 0.32767783670432E+03 0.34486037410237E+03 0.30615000000072E+03 0.30615000000109E+03
 0.32506871006459E+03 0.34482559826534E+03 0.30615000000059E+03 0.30615000000109E+03 0.38219750886682E+03
 0.31171073184439E+03 0.18249902272856E+04 0.17580743167763E+04 0.67199688176178E+03 0.12388089390208E+04
 0.56345207285025E+03 0.10611967844861E+04 0.10474093059602E+04 0.10089149659277E+04 0.17486699043413E+04
 0.94336283347133E+03 0.10465402998854E+04 0.90534612971188E+03 0.17481232229474E+04 0.10611967844861E+04
 0.10474093059602E+04 0.10089149659277E+04 0.17486699043413E+04 0.94336283347132E+03 0.10465402998854E+04
 0.90534612971188E+03 0.17481232229474E+04 0.17543791806578E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47939357052185E+03 0.12948452385826E+01
 0.12948452385826E+01 0.10600834322673E+01 0.11608543657255E+00 0.30903823441498E+03 0.33728465833014E+03
 0.33509865936227E+03 0.33491561429082E+03 0.22999998846654E+00 0.00000000000000E+00 0.20470418657030E+00
 0.00000000000000E+00 -.11973381593661E+02 0.14857442851750E-02 0.46472238288050E+00 0.53845066609545E+04
 0.20191899978579E+04 0.17214578627380E+02 0.64554669852677E+01 0.31170853795379E+03 0.38222630339831E+03
 0.30737684530719E+03 0.31150786161617E+03 0.30615000000014E+03 0.30615000000026E+03 0.30736575221483E+03
 0.31150848120804E+03 0.30615000000014E+03 0.30615000000026E+03 0.30737684530719E+03 0.31150786161617E+03
 0.30615000000014E+03 0.30615000000026E+03 0.30736575221483E+03 0.31150848120804E+03 0.30615000000014E+03
 0.30615000000026E+03 0.31066391644004E+03 0.30615000000180E+03 0.90844228772641E+02 0.84318480464903E+02
 0.10493476821379E+03 0.32659753324567E+03 0.22113809119081E+03 0.79914442796850E+02 0.98726614597316E+02
 0.84210134432104E+02 0.26196092788055E+03 0.79350767152762E+02 0.98707924443909E+02 0.83682389028609E+02
 0.26193751350058E+03 0.79914442796850E+02 0.98726614597316E+02 0.84210134432104E+02 0.26196092788055E+03
 0.79350767152762E+02 0.98707924443909E+02 0.83682389028609E+02 0.26193751350058E+03 0.17307377259843E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35839598502675E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16802796983590E+00 0.00000000000000E+00 0.00000000000000E+00 0.16802796983590E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17634506233242E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17634506233242E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158509132645E+00 0.17907290576597E+00 0.33728465833014E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    320.00670087
 0.89374686392996E-01 0.31630666627656E+03 0.43921520766945E+03 0.43555357022211E+03 0.43418893840113E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17459250707633E+00 0.00000000000000E+00 -.19124614083505E+02
 0.37684278347841E-02 0.79920363320966E+00 0.21229012072771E+04 0.79608795272893E+03 0.10009964504130E+02
 0.37537366890486E+01 0.33221624109933E+03 0.30615000002355E+03 0.32821267893658E+03 0.34569302053602E+03
 0.30615000000106E+03 0.30615000000168E+03 0.32556602734386E+03 0.34565884482758E+03 0.30615000000086E+03
 0.30615000000167E+03 0.32821267893658E+03 0.34569302053602E+03 0.30615000000106E+03 0.30615000000168E+03
 0.32556602734386E+03 0.34565884482758E+03 0.30615000000086E+03 0.30615000000167E+03 0.38331211190125E+03
 0.31217888067921E+03 0.18326725189147E+04 0.17638456160655E+04 0.67074307213073E+03 0.12280656509836E+04
 0.55396886349217E+03 0.10663081541013E+04 0.10519309232151E+04 0.10126824677663E+04 0.17483120089012E+04
 0.94879577990540E+03 0.10511055897977E+04 0.90971375933365E+03 0.17478028127666E+04 0.10663081541013E+04
 0.10519309232151E+04 0.10126824677663E+04 0.17483120089012E+04 0.94879577990540E+03 0.10511055897977E+04
 0.90971375933365E+03 0.17478028127666E+04 0.17530417875098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47971943908897E+03 0.12948453983653E+01
 0.12948453983653E+01 0.11000440064315E+01 0.10324378000236E+00 0.30952238816313E+03 0.33721368481333E+03
 0.33530771538711E+03 0.33514990687630E+03 0.22999998883757E+00 0.00000000000000E+00 0.20350827625120E+00
 0.00000000000000E+00 -.11981718477767E+02 0.16705437792508E-02 0.48790282397535E+00 0.47888598307720E+04
 0.17958224365395E+04 0.16396707719003E+02 0.61487653946262E+01 0.31217665704266E+03 0.38333959655740E+03
 0.30746456438590E+03 0.31165407260123E+03 0.30615000000016E+03 0.30615000000034E+03 0.30745338274622E+03
 0.31165458597056E+03 0.30615000000016E+03 0.30615000000034E+03 0.30746456438590E+03 0.31165407260123E+03
 0.30615000000016E+03 0.30615000000034E+03 0.30745338274622E+03 0.31165458597056E+03 0.30615000000016E+03
 0.30615000000034E+03 0.31078930997482E+03 0.30615000000273E+03 0.91929611287911E+02 0.85463071131667E+02
 0.10814076897392E+03 0.32800672384002E+03 0.21932525102122E+03 0.82907655632509E+02 0.10165674069564E+03
 0.88684513142172E+02 0.26305176953781E+03 0.82371911855112E+02 0.10162842781392E+03 0.88188039493701E+02
 0.26301955385993E+03 0.82907655632509E+02 0.10165674069564E+03 0.88684513142172E+02 0.26305176953781E+03
 0.82371911855112E+02 0.10162842781392E+03 0.88188039493701E+02 0.26301955385993E+03 0.17454011430160E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35842117865665E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16807006555196E+00 0.00000000000000E+00 0.00000000000000E+00 0.16807006555196E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17654031747307E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17654031747307E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158539576527E+00 0.17911089836887E+00 0.33721368481333E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    330.01584440
 0.89457149240694E-01 0.31659362507407E+03 0.43955918206064E+03 0.43589246599970E+03 0.43452659292742E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17329202575948E+00 0.00000000000000E+00 -.19147878202758E+02
 0.37649536991700E-02 0.81733957809518E+00 0.21248601282305E+04 0.79682254808642E+03 0.97878534386456E+01
 0.36704450394921E+01 0.33283915029417E+03 0.30615000003534E+03 0.32873885680479E+03 0.34650918302204E+03
 0.30615000000158E+03 0.30615000000257E+03 0.32605563934378E+03 0.34647558746650E+03 0.30615000000127E+03
 0.30615000000256E+03 0.32873885680479E+03 0.34650918302204E+03 0.30615000000158E+03 0.30615000000257E+03
 0.32605563934378E+03 0.34647558746650E+03 0.30615000000127E+03 0.30615000000256E+03 0.38439379117058E+03
 0.31266479342714E+03 0.18400632828371E+04 0.17693495210247E+04 0.66949711135456E+03 0.12178815650592E+04
 0.54503696814787E+03 0.10712451103776E+04 0.10562691709653E+04 0.10162887062073E+04 0.17480258291884E+04
 0.95405059329308E+03 0.10554843698989E+04 0.91391138272746E+03 0.17475512062100E+04 0.10712451103776E+04
 0.10562691709653E+04 0.10162887062073E+04 0.17480258291884E+04 0.95405059329308E+03 0.10554843698989E+04
 0.91391138272746E+03 0.17475512062100E+04 0.17518159512483E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48005662932291E+03 0.12948455697745E+01
 0.12948455697745E+01 0.11400805805338E+01 0.91214312173551E-01 0.31005352571160E+03 0.33715441538928E+03
 0.33550642271512E+03 0.33537185106460E+03 0.22999998934060E+00 0.00000000000000E+00 0.20230735387433E+00
 0.00000000000000E+00 -.11980862619097E+02 0.18908571683158E-02 0.51112508983831E+00 0.42308854069213E+04
 0.15865820275955E+04 0.15651745842746E+02 0.58694046910298E+01 0.31266254875221E+03 0.38441994691939E+03
 0.30755634657582E+03 0.31179762151502E+03 0.30615000000017E+03 0.30615000000044E+03 0.30754513831945E+03
 0.31179802290772E+03 0.30615000000017E+03 0.30615000000044E+03 0.30755634657582E+03 0.31179762151502E+03
 0.30615000000017E+03 0.30615000000044E+03 0.30754513831945E+03 0.31179802290772E+03 0.30615000000017E+03
 0.30615000000044E+03 0.31091241940526E+03 0.30615000000410E+03 0.92745468633847E+02 0.86432880019624E+02
 0.11120394235476E+03 0.32940744591897E+03 0.21764748385244E+03 0.85857093713783E+02 0.10445048199864E+03
 0.93443877818255E+02 0.26413346120792E+03 0.85352194873186E+02 0.10441274079285E+03 0.92981509836214E+02
 0.26409267692970E+03 0.85857093713787E+02 0.10445048199864E+03 0.93443877818261E+02 0.26413346120792E+03
 0.85352194873186E+02 0.10441274079285E+03 0.92981509836214E+02 0.26409267692970E+03 0.17591987952803E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35846404926899E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16812090371810E+00 0.00000000000000E+00 0.00000000000000E+00 0.16812090371810E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17671185477096E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17671185477096E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158590923976E+00 0.17914292243403E+00 0.33715441538928E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    340.01896760
 0.89527687241845E-01 0.31687546413470E+03 0.43991152220754E+03 0.43623981096533E+03 0.43487255070587E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17203151373707E+00 0.00000000000000E+00 -.19172184839012E+02
 0.37619869744371E-02 0.83485515039842E+00 0.21265358052434E+04 0.79745092696629E+03 0.95825006244282E+01
 0.35934377341606E+01 0.33345035246969E+03 0.30615000005217E+03 0.32925573803359E+03 0.34730829017003E+03
 0.30615000000236E+03 0.30615000000391E+03 0.32653694533996E+03 0.34727525388192E+03 0.30615000000187E+03
 0.30615000000389E+03 0.32925573803359E+03 0.34730829017003E+03 0.30615000000236E+03 0.30615000000391E+03
 0.32653694533996E+03 0.34727525388192E+03 0.30615000000187E+03 0.30615000000389E+03 0.38544274283125E+03
 0.31316672055764E+03 0.18471655678656E+04 0.17745919498398E+04 0.66825864440240E+03 0.12082194817766E+04
 0.53661954415222E+03 0.10760091607189E+04 0.10604265797176E+04 0.10197383467308E+04 0.17477964016463E+04
 0.95912800429576E+03 0.10596793449011E+04 0.91794227530553E+03 0.17473536194536E+04 0.10760091607189E+04
 0.10604265797176E+04 0.10197383467309E+04 0.17477964016463E+04 0.95912800429576E+03 0.10596793449011E+04
 0.91794227530553E+03 0.17473536194536E+04 0.17506841320363E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48040219194255E+03 0.12948457488651E+01
 0.12948457488651E+01 0.11800930733196E+01 0.80017448611916E-01 0.31062909269805E+03 0.33710821599039E+03
 0.33569568806550E+03 0.33558221837557E+03 0.22999999003680E+00 0.00000000000000E+00 0.20110465437476E+00
 0.00000000000000E+00 -.11977448915744E+02 0.21554451159199E-02 0.53432677145539E+00 0.37115303660079E+04
 0.13918238872530E+04 0.14972111500627E+02 0.56145418127350E+01 0.31316446390352E+03 0.38546757022326E+03
 0.30765249275029E+03 0.31193854525555E+03 0.30615000000019E+03 0.30615000000059E+03 0.30764131805466E+03
 0.31193883013674E+03 0.30615000000019E+03 0.30615000000059E+03 0.30765249275029E+03 0.31193854525555E+03
 0.30615000000019E+03 0.30615000000059E+03 0.30764131805466E+03 0.31193883013674E+03 0.30615000000019E+03
 0.30615000000059E+03 0.31103327617191E+03 0.30615000000608E+03 0.93304849133061E+02 0.87236341643153E+02
 0.11413087431264E+03 0.33081819836640E+03 0.21611666968220E+03 0.88759101618436E+02 0.10711543080716E+03
 0.98524460969340E+02 0.26522183286738E+03 0.88287504766781E+02 0.10706853643672E+03 0.98098561800711E+02
 0.26517278430470E+03 0.88759101618437E+02 0.10711543080716E+03 0.98524460969342E+02 0.26522183286738E+03
 0.88287504766781E+02 0.10706853643672E+03 0.98098561800712E+02 0.26517278430470E+03 0.17722360158715E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35852494529440E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16817723445860E+00 0.00000000000000E+00 0.00000000000000E+00 0.16817723445860E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17686244765695E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17686244765695E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158643759542E+00 0.17916803103043E+00 0.33710821599039E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    350.00101025
 0.89591542585115E-01 0.31715217159727E+03 0.44027014012633E+03 0.43659336388618E+03 0.43522453934318E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17081150881267E+00 0.00000000000000E+00 -.19194555013066E+02
 0.37593053021876E-02 0.85174715831690E+00 0.21280527536150E+04 0.79801978260564E+03 0.93924586913897E+01
 0.35221720092711E+01 0.33404974979781E+03 0.30615000007577E+03 0.32976319455760E+03 0.34809048070469E+03
 0.30615000000348E+03 0.30615000000586E+03 0.32700981213069E+03 0.34805798269862E+03 0.30615000000274E+03
 0.30615000000583E+03 0.32976319455760E+03 0.34809048070469E+03 0.30615000000348E+03 0.30615000000586E+03
 0.32700981213069E+03 0.34805798269862E+03 0.30615000000274E+03 0.30615000000583E+03 0.38646005577113E+03
 0.31368316390389E+03 0.18539900798900E+04 0.17795848998234E+04 0.66702654398251E+03 0.11990372863214E+04
 0.52867560961898E+03 0.10806064671644E+04 0.10644093735849E+04 0.10230392466884E+04 0.17476101436294E+04
 0.96403382264502E+03 0.10636969466724E+04 0.92181369612711E+03 0.17471966752826E+04 0.10806064671645E+04
 0.10644093735849E+04 0.10230392466884E+04 0.17476101436294E+04 0.96403382264502E+03 0.10636969466724E+04
 0.92181369612711E+03 0.17471966752826E+04 0.17496305407788E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48075386202418E+03 0.12948459136879E+01
 0.12948459136879E+01 0.12200212439287E+01 0.69654731208545E-01 0.31124647028340E+03 0.33707575022862E+03
 0.33587632919402E+03 0.33578177979626E+03 0.22999999097084E+00 0.00000000000000E+00 0.19990219531352E+00
 0.00000000000000E+00 -.11971807352573E+02 0.24761161195450E-02 0.55746950963087E+00 0.32308662493058E+04
 0.12115748434897E+04 0.14350560634782E+02 0.53814602380433E+01 0.31368090448669E+03 0.38648357054317E+03
 0.30774965700649E+03 0.31207699773085E+03 0.30615000000020E+03 0.30615000000079E+03 0.30774233615771E+03
 0.31207716266597E+03 0.30615000000020E+03 0.30615000000079E+03 0.30774965700649E+03 0.31207699773085E+03
 0.30615000000020E+03 0.30615000000079E+03 0.30774233615771E+03 0.31207716266597E+03 0.30615000000020E+03
 0.30615000000079E+03 0.31115201129276E+03 0.30615000000890E+03 0.93622171921133E+02 0.87881263804762E+02
 0.11693027630297E+03 0.33225057446590E+03 0.21473564678141E+03 0.91633738770744E+02 0.10966092212423E+03
 0.91633738770744E+02 0.26632667181628E+03 0.91174588957634E+02 0.10960521213315E+03 0.91174588957634E+02
 0.26626971762345E+03 0.91633738770744E+02 0.10966092212423E+03 0.91633738770744E+02 0.26632667181628E+03
 0.91174588957634E+02 0.10960521213315E+03 0.91174588957634E+02 0.26626971762345E+03 0.17828692918141E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35860273797667E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16821460279878E+00 0.00000000000000E+00 0.00000000000000E+00 0.16821460279878E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17697446920288E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17697446920288E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158697020401E+00 0.17918585394142E+00 0.33707575022862E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    360.02087559
 0.89638058201185E-01 0.31742686149810E+03 0.44063599643951E+03 0.43695458723658E+03 0.43558423801269E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16962368556603E+00 0.00000000000000E+00 -.19218074065521E+02
 0.37573541299801E-02 0.86813430033718E+00 0.21291578390676E+04 0.79843418965034E+03 0.92151640557144E+01
 0.34556865208929E+01 0.33464158227572E+03 0.30615000010862E+03 0.33026477624163E+03 0.34886144103579E+03
 0.30615000000512E+03 0.30615000000872E+03 0.32747753999723E+03 0.34882946419242E+03 0.30615000000402E+03
 0.30615000000868E+03 0.33026477624163E+03 0.34886144103579E+03 0.30615000000512E+03 0.30615000000872E+03
 0.32747753999723E+03 0.34882946419242E+03 0.30615000000402E+03 0.30615000000868E+03 0.38745391334604E+03
 0.31421820687995E+03 0.18606058024150E+04 0.17843920320744E+04 0.66579371467845E+03 0.11902392247912E+04
 0.52111654153933E+03 0.10850797539564E+04 0.10682540659211E+04 0.10262306713090E+04 0.17474581561273E+04
 0.96881312816059E+03 0.10675741254237E+04 0.92556901734895E+03 0.17470718692901E+04 0.10850797539564E+04
 0.10682540659211E+04 0.10262306713090E+04 0.17474581561273E+04 0.96881312816059E+03 0.10675741254237E+04
 0.92556901734895E+03 0.17470718692901E+04 0.17486447210956E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48111324439595E+03 0.12948460869756E+01
 0.12948460869756E+01 0.12601007053016E+01 0.60072709568033E-01 0.31198773305313E+03 0.33705663234168E+03
 0.33605266120425E+03 0.33597546908393E+03 0.22999999218308E+00 0.00000000000000E+00 0.19869323497156E+00
 0.00000000000000E+00 -.11967593632078E+02 0.28710738828942E-02 0.58068413499050E+00 0.27864138389694E+04
 0.10449051896135E+04 0.13776853056492E+02 0.51663198961843E+01 0.31421594916609E+03 0.38747613405076E+03
 0.30779419957333E+03 0.31221407423859E+03 0.30615000000022E+03 0.30615000000111E+03 0.30778337432971E+03
 0.31221411597286E+03 0.30615000000022E+03 0.30615000000111E+03 0.30779419957333E+03 0.31221407423859E+03
 0.30615000000022E+03 0.30615000000111E+03 0.30778337432971E+03 0.31221411597286E+03 0.30615000000022E+03
 0.30615000000111E+03 0.31126956711169E+03 0.30615000001288E+03 0.93689774091165E+02 0.88595796439473E+02
 0.11962677540297E+03 0.33372086375084E+03 0.21349595447085E+03 0.94819752522335E+02 0.11211013491397E+03
 0.94819752522335E+02 0.26746107125110E+03 0.94417700601074E+02 0.11204592721754E+03 0.94417700601074E+02
 0.26739654942656E+03 0.94819752522335E+02 0.11211013491397E+03 0.94819752522335E+02 0.26746107125110E+03
 0.94417700601074E+02 0.11204592721754E+03 0.94417700601074E+02 0.26739654942656E+03 0.17951074873437E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35870163354935E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16825865057989E+00 0.00000000000000E+00 0.00000000000000E+00 0.16825865057989E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17708300435130E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17708300435130E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158740979368E+00 0.17919648392035E+00 0.33705663234168E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    370.06547091
 0.89691825312533E-01 0.31769689762754E+03 0.44100743477140E+03 0.43732078571917E+03 0.43594855836586E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16846890942038E+00 0.00000000000000E+00 -.19245752824051E+02
 0.37551013577152E-02 0.88400787432341E+00 0.21304351701621E+04 0.79891318881079E+03 0.90496931445582E+01
 0.33936349292093E+01 0.33522551419093E+03 0.30615000015376E+03 0.33076016033242E+03 0.34962099592093E+03
 0.30615000000747E+03 0.30615000001283E+03 0.32793980822272E+03 0.34958952271224E+03 0.30615000000585E+03
 0.30615000001277E+03 0.33076016033242E+03 0.34962099592093E+03 0.30615000000747E+03 0.30615000001283E+03
 0.32793980822272E+03 0.34958952271224E+03 0.30615000000585E+03 0.30615000001277E+03 0.38842498672719E+03
 0.31476963715270E+03 0.18670043876493E+04 0.17889916567586E+04 0.66455437074616E+03 0.11817888635151E+04
 0.51391172091519E+03 0.10894264233166E+04 0.10719614375151E+04 0.10293001673873E+04 0.17473288881960E+04
 0.97346238563825E+03 0.10713117880278E+04 0.92919484521751E+03 0.17469677784336E+04 0.10894264233166E+04
 0.10719614375151E+04 0.10293001673873E+04 0.17473288881960E+04 0.97346238563825E+03 0.10713117880278E+04
 0.92919484521751E+03 0.17469677784336E+04 0.17477020308831E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48147737415924E+03 0.12948462909121E+01
 0.12948462909121E+01 0.13002790865955E+01 0.51255121337522E-01 0.31275260268688E+03 0.33705222260165E+03
 0.33622190262353E+03 0.33615975947988E+03 0.22999999287544E+00 0.00000000000000E+00 0.19747914691354E+00
 0.00000000000000E+00 -.11968539948174E+02 0.33649939547206E-02 0.60394203597836E+00 0.23774188327373E+04
 0.89153206227651E+03 0.13246304319653E+02 0.49673641198698E+01 0.31476738411773E+03 0.38844594222681E+03
 0.30786652517645E+03 0.31234991575118E+03 0.30615000000024E+03 0.30615000000155E+03 0.30785568304825E+03
 0.31234983181004E+03 0.30615000000024E+03 0.30615000000155E+03 0.30786652517645E+03 0.31234991575118E+03
 0.30615000000024E+03 0.30615000000155E+03 0.30785568304825E+03 0.31234983181004E+03 0.30615000000024E+03
 0.30615000000155E+03 0.31138605010677E+03 0.30615000001842E+03 0.93544202333967E+02 0.89097251217029E+02
 0.12223453465586E+03 0.33525222437221E+03 0.21240651704307E+03 0.97851802819074E+02 0.11447693797778E+03
 0.97851802819074E+02 0.26864545566475E+03 0.97488533351478E+02 0.11440455894298E+03 0.97488533351478E+02
 0.26857370716103E+03 0.97851802819074E+02 0.11447693797778E+03 0.97851802819074E+02 0.26864545566475E+03
 0.97488533351478E+02 0.11440455894298E+03 0.97488533351478E+02 0.26857370716103E+03 0.18067689578495E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35881850001669E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16833650078074E+00 0.00000000000000E+00 0.00000000000000E+00 0.16833650078074E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17719453241483E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17719453241483E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158763998295E+00 0.17919906711939E+00 0.33705222260165E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    380.01184698
 0.89748019658473E-01 0.31796066340886E+03 0.44137887751669E+03 0.43768669741470E+03 0.43631238940106E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16735989773546E+00 0.00000000000000E+00 -.19271520834832E+02
 0.37527497904954E-02 0.89919727295000E+00 0.21317701543176E+04 0.79941380786912E+03 0.88968241348802E+01
 0.33363090505801E+01 0.33579511517674E+03 0.30615000021419E+03 0.33124386838660E+03 0.35036076448174E+03
 0.30615000001072E+03 0.30615000001857E+03 0.32839151087902E+03 0.35032977279002E+03 0.30615000000839E+03
 0.30615000001848E+03 0.33124386838660E+03 0.35036076448174E+03 0.30615000001072E+03 0.30615000001857E+03
 0.32839151087902E+03 0.35032977279002E+03 0.30615000000839E+03 0.30615000001848E+03 0.38936264882113E+03
 0.31532880481395E+03 0.18731275218549E+04 0.17933540701171E+04 0.66333093904718E+03 0.11737678052543E+04
 0.50712021151188E+03 0.10936046444624E+04 0.10754965633249E+04 0.10322277926301E+04 0.17472164152399E+04
 0.97793630812847E+03 0.10748748512107E+04 0.93266449715487E+03 0.17468783649592E+04 0.10936046444625E+04
 0.10754965633249E+04 0.10322277926301E+04 0.17472164152399E+04 0.97793630812847E+03 0.10748748512107E+04
 0.93266449715487E+03 0.17468783649592E+04 0.17468117830337E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48184109842748E+03 0.12948464807702E+01
 0.12948464807702E+01 0.13400645908518E+01 0.43286078181335E-01 0.31352790410885E+03 0.33706169371669E+03
 0.33638257007878E+03 0.33633317225338E+03 0.22999999433615E+00 0.00000000000000E+00 0.19627507417429E+00
 0.00000000000000E+00 -.11968507211533E+02 0.39844949670256E-02 0.62695514624599E+00 0.20077826841809E+04
 0.75291850656784E+03 0.12760083473118E+02 0.47850313024194E+01 0.31532657201829E+03 0.38938238923489E+03
 0.30794245914061E+03 0.31248312877390E+03 0.30615000000027E+03 0.30615000000216E+03 0.30793171994322E+03
 0.31248291887674E+03 0.30615000000027E+03 0.30615000000216E+03 0.30794245914061E+03 0.31248312877390E+03
 0.30615000000027E+03 0.30615000000216E+03 0.30793171994322E+03 0.31248291887674E+03 0.30615000000027E+03
 0.30615000000216E+03 0.31150027789535E+03 0.30615000002593E+03 0.93200775002450E+02 0.89384294534164E+02
 0.12472720118956E+03 0.33681902455541E+03 0.21146818735990E+03 0.10082749155989E+03 0.11673846780509E+03
 0.10082749155989E+03 0.26985998521944E+03 0.10050330913869E+03 0.11665839639098E+03 0.10050330913869E+03
 0.26978148757074E+03 0.10082749155989E+03 0.11673846780509E+03 0.10082749155989E+03 0.26985998521944E+03
 0.10050330913869E+03 0.11665839639098E+03 0.10050330913869E+03 0.26978148757074E+03 0.18177317090392E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35894987457850E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16839849076564E+00 0.00000000000000E+00 0.00000000000000E+00 0.16839849076564E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17728967010126E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17728967010126E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158783803814E+00 0.17919423868777E+00 0.33706169371669E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    390.00497884
 0.89803976727443E-01 0.31822261241142E+03 0.44175498963240E+03 0.43805709005605E+03 0.43668055191197E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16627921169376E+00 0.00000000000000E+00 -.19297245793522E+02
 0.37504110718852E-02 0.91394489044097E+00 0.21330995047374E+04 0.79991231427653E+03 0.87532630070726E+01
 0.32824736276522E+01 0.33635923641015E+03 0.30615000029514E+03 0.33172337847193E+03 0.35109242238957E+03
 0.30615000001527E+03 0.30615000002661E+03 0.32883960332148E+03 0.35106189759182E+03 0.30615000001194E+03
 0.30615000002647E+03 0.33172337847193E+03 0.35109242238957E+03 0.30615000001527E+03 0.30615000002661E+03
 0.32883960332148E+03 0.35106189759182E+03 0.30615000001194E+03 0.30615000002647E+03 0.39028288420780E+03
 0.31590231583091E+03 0.18790846275363E+04 0.17975640968953E+04 0.66209915599389E+03 0.11660147455725E+04
 0.50060509379865E+03 0.10976867754968E+04 0.10789214359705E+04 0.10350686057141E+04 0.17471143000097E+04
 0.98231195300853E+03 0.10783258982265E+04 0.93604161639963E+03 0.17467977040959E+04 0.10976867754968E+04
 0.10789214359705E+04 0.10350686057142E+04 0.17471143000097E+04 0.98231195300853E+03 0.10783258982265E+04
 0.93604161639963E+03 0.17467977040959E+04 0.17459534453172E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48220920934390E+03 0.12948466703112E+01
 0.12948466703112E+01 0.13800371182881E+01 0.36028165958102E-01 0.31432026284203E+03 0.33708430539431E+03
 0.33653754092567E+03 0.33649896077217E+03 0.22999999706331E+00 0.00000000000000E+00 0.19506373826736E+00
 0.00000000000000E+00 -.11969033822648E+02 0.47871753750393E-02 0.65005549291747E+00 0.16711315908150E+04
 0.62667434655564E+03 0.12306641643924E+02 0.46149906164717E+01 0.31590011696499E+03 0.39030144145120E+03
 0.30802122599265E+03 0.31261584760955E+03 0.30615000000031E+03 0.30615000000301E+03 0.30801066040039E+03
 0.31261551055734E+03 0.30615000000031E+03 0.30615000000302E+03 0.30802122599265E+03 0.31261584760955E+03
 0.30615000000031E+03 0.30615000000301E+03 0.30801066040039E+03 0.31261551055734E+03 0.30615000000031E+03
 0.30615000000302E+03 0.31161407378484E+03 0.30615000003611E+03 0.92668129258993E+02 0.89463039863325E+02
 0.12714604600466E+03 0.33843897036535E+03 0.21065719413066E+03 0.10379817910244E+03 0.11893294515525E+03
 0.10379817910244E+03 0.27111853544054E+03 0.10351375689745E+03 0.11884558838089E+03 0.10351375689745E+03
 0.27103370157085E+03 0.10379817910244E+03 0.11893294515525E+03 0.10379817910244E+03 0.27111853544054E+03
 0.10351375689745E+03 0.11884558838089E+03 0.10351375689745E+03 0.27103370157085E+03 0.18281563864528E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35909661108978E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16845848456229E+00 0.00000000000000E+00 0.00000000000000E+00 0.16845848456229E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17737100464101E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17737100464101E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158796650810E+00 0.17918235152934E+00 0.33708430539431E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    400.00428510
 0.89857597565949E-01 0.31848161809217E+03 0.44213354805560E+03 0.43842985960196E+03 0.43705099188798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16523059277573E+00 0.00000000000000E+00 -.19321918793196E+02
 0.37481727115888E-02 0.92820239875282E+00 0.21343733641903E+04 0.80039001157136E+03 0.86188098746019E+01
 0.32320537029757E+01 0.33691594325528E+03 0.30615000040216E+03 0.33219702205785E+03 0.35181352628981E+03
 0.30615000002151E+03 0.30615000003768E+03 0.32928252052194E+03 0.35178345245631E+03 0.30615000001684E+03
 0.30615000003750E+03 0.33219702205785E+03 0.35181352628981E+03 0.30615000002151E+03 0.30615000003768E+03
 0.32928252052194E+03 0.35178345245631E+03 0.30615000001684E+03 0.30615000003750E+03 0.39118311992246E+03
 0.31648668913998E+03 0.18848641081378E+04 0.18016151840719E+04 0.66086344315055E+03 0.11585373134308E+04
 0.49436955306450E+03 0.11016634349853E+04 0.10822285733969E+04 0.10378162214743E+04 0.17470181416378E+04
 0.98657889800573E+03 0.10816574766642E+04 0.93931807361846E+03 0.17467214364204E+04 0.11016634349853E+04
 0.10822285733969E+04 0.10378162214743E+04 0.17470181416378E+04 0.98657889800573E+03 0.10816574766642E+04
 0.93931807361846E+03 0.17467214364204E+04 0.17451249073271E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48257964088721E+03 0.12948468521015E+01
 0.12948468521015E+01 0.14200343433292E+01 0.29502537806414E-01 0.31512121112740E+03 0.33711926157142E+03
 0.33668659602816E+03 0.33665702707128E+03 0.23000000000000E+00 0.00000000000000E+00 0.19385029359394E+00
 0.00000000000000E+00 -.11969225575378E+02 0.58460441354468E-02 0.67314578075775E+00 0.13684467333206E+04
 0.51316752499521E+03 0.11884498467768E+02 0.44566869254130E+01 0.31648453538603E+03 0.39120053931989E+03
 0.30810187476826E+03 0.31274768407007E+03 0.30615000000037E+03 0.30615000000421E+03 0.30809153737572E+03
 0.31274721985579E+03 0.30615000000037E+03 0.30615000000421E+03 0.30810187476826E+03 0.31274768407007E+03
 0.30615000000037E+03 0.30615000000421E+03 0.30809153737572E+03 0.31274721985579E+03 0.30615000000037E+03
 0.30615000000421E+03 0.31172709840706E+03 0.30615000004972E+03 0.91957425648439E+02 0.89328580755827E+02
 0.12948530979769E+03 0.34010137785786E+03 0.20996864151118E+03 0.10675434474248E+03 0.12105545759884E+03
 0.10675434474248E+03 0.27241279901735E+03 0.10650980599759E+03 0.12096126805220E+03 0.10650980599759E+03
 0.27232207892065E+03 0.10675434474248E+03 0.12105545759884E+03 0.10675434474248E+03 0.27241279901735E+03
 0.10650980599759E+03 0.12096126805220E+03 0.10650980599759E+03 0.27232207892065E+03 0.18379831135561E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35925716578980E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16850876733498E+00 0.00000000000000E+00 0.00000000000000E+00 0.16850876733498E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17743783438324E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17743783438324E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158805363266E+00 0.17916386259401E+00 0.33711926157142E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    410.00456049
 0.89906920963792E-01 0.31873786144604E+03 0.44251385759171E+03 0.43880441802415E+03 0.43742317019137E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16421365424768E+00 0.00000000000000E+00 -.19345867743215E+02
 0.37461160711628E-02 0.94197783752470E+00 0.21355451481023E+04 0.80082943053838E+03 0.84927688118673E+01
 0.31847883044503E+01 0.33746534609635E+03 0.30615000054216E+03 0.33266486707365E+03 0.35252425628694E+03
 0.30615000002999E+03 0.30615000005277E+03 0.32972031908906E+03 0.35249461796969E+03 0.30615000002350E+03
 0.30615000005252E+03 0.33266486707364E+03 0.35252425628694E+03 0.30615000002999E+03 0.30615000005277E+03
 0.32972031908906E+03 0.35249461796969E+03 0.30615000002350E+03 0.30615000005252E+03 0.39206409362886E+03
 0.31708041205728E+03 0.18904776332480E+04 0.18055199131314E+04 0.65962593163489E+03 0.11513194717448E+04
 0.48839541045173E+03 0.11055409428325E+04 0.10854249982540E+04 0.10404775635749E+04 0.17469269261794E+04
 0.99074348022778E+03 0.10848767330521E+04 0.94250086035794E+03 0.17466486658051E+04 0.11055409428325E+04
 0.10854249982540E+04 0.10404775635749E+04 0.17469269261794E+04 0.99074348022778E+03 0.10848767330521E+04
 0.94250086035794E+03 0.17466486658051E+04 0.17443251868920E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48295183831617E+03 0.12948470285570E+01
 0.12948470285570E+01 0.14600354448864E+01 0.23831355446845E-01 0.31592423051725E+03 0.33716771686254E+03
 0.33683020947985E+03 0.33680789909135E+03 0.23000000000000E+00 0.00000000000000E+00 0.19263208431582E+00
 0.00000000000000E+00 -.11969604219909E+02 0.72372353634102E-02 0.69627185673492E+00 0.11053944770742E+04
 0.41452292890284E+03 0.11489764985641E+02 0.43086618696153E+01 0.31707831251063E+03 0.39208042065230E+03
 0.30818404104473E+03 0.31287870877806E+03 0.30615000000046E+03 0.30615000000584E+03 0.30817397413116E+03
 0.31287811810590E+03 0.30615000000046E+03 0.30615000000584E+03 0.30818404104473E+03 0.31287870877806E+03
 0.30615000000046E+03 0.30615000000584E+03 0.30817397413116E+03 0.31287811810590E+03 0.30615000000046E+03
 0.30615000000584E+03 0.31183949125896E+03 0.30615000006773E+03 0.91084576387468E+02 0.88982921388070E+02
 0.13175922688918E+03 0.34185451322417E+03 0.20943649020054E+03 0.10969601899937E+03 0.12312108076791E+03
 0.10969601899937E+03 0.27374883521378E+03 0.10948982857710E+03 0.12302054184551E+03 0.10948982857710E+03
 0.27365270448495E+03 0.10969601899937E+03 0.12312108076791E+03 0.10969601899937E+03 0.27374883521378E+03
 0.10948982857710E+03 0.12302054184551E+03 0.10948982857710E+03 0.27365270448495E+03 0.18473196915883E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35949806195354E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16855237810542E+00 0.00000000000000E+00 0.00000000000000E+00 0.16855237810542E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17749284870352E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17749284870352E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158808136410E+00 0.17913814274915E+00 0.33716771686254E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    420.01855117
 0.89952266954660E-01 0.31899171873461E+03 0.44289597183178E+03 0.43918081568131E+03 0.43779714121543E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16322621598947E+00 0.00000000000000E+00 -.19369368786592E+02
 0.37442272355412E-02 0.95530338201721E+00 0.21366224581836E+04 0.80123342181885E+03 0.83743030230954E+01
 0.31403636336608E+01 0.33800849812762E+03 0.30615000072380E+03 0.33312778641576E+03 0.35322601602520E+03
 0.30615000004138E+03 0.30615000007311E+03 0.33015380833064E+03 0.35319679899170E+03 0.30615000003246E+03
 0.30615000007277E+03 0.33312778641576E+03 0.35322601602520E+03 0.30615000004138E+03 0.30615000007311E+03
 0.33015380833064E+03 0.35319679899170E+03 0.30615000003246E+03 0.30615000007277E+03 0.39292805459593E+03
 0.31768303630476E+03 0.18959443116894E+04 0.18092938270463E+04 0.65838572771157E+03 0.11443331117352E+04
 0.48265545538504E+03 0.11093312967204E+04 0.10885225416783E+04 0.10430619854809E+04 0.17468397010332E+04
 0.99481815771225E+03 0.10879956491188E+04 0.94560026659351E+03 0.17465785769575E+04 0.11093312967204E+04
 0.10885225416783E+04 0.10430619854809E+04 0.17468397010332E+04 0.99481815771225E+03 0.10879956491188E+04
 0.94560026659352E+03 0.17465785769575E+04 0.17435508628519E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48332585341557E+03 0.12948472017125E+01
 0.12948472017125E+01 0.15000914076347E+01 0.19227571151455E-01 0.31671793684448E+03 0.33723161083679E+03
 0.33696865875261E+03 0.33695185972870E+03 0.23000000000000E+00 0.00000000000000E+00 0.19140139535088E+00
 0.00000000000000E+00 -.11970661805250E+02 0.89700939557360E-02 0.71957163716874E+00 0.89185241977141E+03
 0.33444465741428E+03 0.11117725583900E+02 0.41691470939627E+01 0.31768099877620E+03 0.39294333390498E+03
 0.30826764572698E+03 0.31300943506996E+03 0.30615000000057E+03 0.30615000000803E+03 0.30825787146754E+03
 0.31300871923165E+03 0.30615000000057E+03 0.30615000000803E+03 0.30826764572698E+03 0.31300943506996E+03
 0.30615000000057E+03 0.30615000000803E+03 0.30825787146754E+03 0.31300871923165E+03 0.30615000000057E+03
 0.30615000000803E+03 0.31195166097552E+03 0.30615000009133E+03 0.90063561287630E+02 0.88415130609655E+02
 0.13399031065969E+03 0.34370719523575E+03 0.20904693302275E+03 0.11262506626117E+03 0.12515199658357E+03
 0.11262506626117E+03 0.27518192547967E+03 0.11245271099215E+03 0.12504562783029E+03 0.11245271099215E+03
 0.27508089248454E+03 0.11262506626117E+03 0.12515199658356E+03 0.11262506626117E+03 0.27518192547967E+03
 0.11245271099215E+03 0.12504562783029E+03 0.11245271099215E+03 0.27508089248454E+03 0.18562658746370E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35974951214226E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16859165720974E+00 0.00000000000000E+00 0.00000000000000E+00 0.16859165720974E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17753832981726E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17753832981726E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158802961629E+00 0.17910414625052E+00 0.33723161083679E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    430.01130961
 0.89992018477023E-01 0.31924261172451E+03 0.44327817936136E+03 0.43955744232984E+03 0.43817134125242E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16227075869214E+00 0.00000000000000E+00 -.19391328501838E+02
 0.37425729509629E-02 0.96814829184189E+00 0.21375668837509E+04 0.80158758140657E+03 0.82631969372999E+01
 0.30986988514874E+01 0.33854384971492E+03 0.30615000095651E+03 0.33358443655628E+03 0.35391683693252E+03
 0.30615000005648E+03 0.30615000010016E+03 0.33058172190214E+03 0.35388802616059E+03 0.30615000004436E+03
 0.30615000009969E+03 0.33358443655628E+03 0.35391683693252E+03 0.30615000005648E+03 0.30615000010016E+03
 0.33058172190214E+03 0.35388802616059E+03 0.30615000004436E+03 0.30615000009969E+03 0.39377307845477E+03
 0.31829109928078E+03 0.19012574191901E+04 0.18129363487662E+04 0.65714771660170E+03 0.11375848257193E+04
 0.47715137053460E+03 0.11130283492721E+04 0.10915178566531E+04 0.10455677479044E+04 0.17467562271984E+04
 0.99879594189177E+03 0.10910109193305E+04 0.94861305117749E+03 0.17465109793519E+04 0.11130283492721E+04
 0.10915178566531E+04 0.10455677479044E+04 0.17467562271984E+04 0.99879594189177E+03 0.10910109193305E+04
 0.94861305117749E+03 0.17465109793519E+04 0.17428034391147E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48370010950070E+03 0.12948473635115E+01
 0.12948473635115E+01 0.15400624413920E+01 0.15516818110088E-01 0.31749042085829E+03 0.33730652999546E+03
 0.33710154135470E+03 0.33708888754601E+03 0.23000000000000E+00 0.00000000000000E+00 0.19016733017889E+00
 0.00000000000000E+00 -.11970999305526E+02 0.11115237084047E-01 0.74288203027889E+00 0.71973273619881E+03
 0.26989977607455E+03 0.10768869987334E+02 0.40383262452502E+01 0.31828913166127E+03 0.39378735837733E+03
 0.30835224069335E+03 0.31313952445100E+03 0.30615000000072E+03 0.30615000001094E+03 0.30834276754264E+03
 0.31313868572458E+03 0.30615000000071E+03 0.30615000001094E+03 0.30835224069335E+03 0.31313952445100E+03
 0.30615000000072E+03 0.30615000001094E+03 0.30834276754264E+03 0.31313868572458E+03 0.30615000000071E+03
 0.30615000001094E+03 0.31206321714516E+03 0.30615000012188E+03 0.88895660495437E+02 0.87613273866796E+02
 0.13615439040585E+03 0.34558875369807E+03 0.20875359134019E+03 0.11552226033817E+03 0.12712344611947E+03
 0.11552226033817E+03 0.27666091218202E+03 0.11537943773748E+03 0.12701176458498E+03 0.11537943773748E+03
 0.27655547602185E+03 0.11552226033817E+03 0.12712344611947E+03 0.11552226033817E+03 0.27666091218202E+03
 0.11537943773748E+03 0.12701176458498E+03 0.11537943773748E+03 0.27655547602186E+03 0.18645398969123E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35998383974269E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16861795447703E+00 0.00000000000000E+00 0.00000000000000E+00 0.16861795447703E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17756872012418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17756872012418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158795316054E+00 0.17906428504204E+00 0.33730652999546E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    440.01796446
 0.90023536722508E-01 0.31949177153017E+03 0.44366152920373E+03 0.43993546229048E+03 0.43854698307407E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16134295259758E+00 0.00000000000000E+00 -.19411418972255E+02
 0.37412622592012E-02 0.98057337428442E+00 0.21383157463300E+04 0.80186840487373E+03 0.81584919699029E+01
 0.30594344887136E+01 0.33907360801999E+03 0.30615000125310E+03 0.33403667850342E+03 0.35459958723704E+03
 0.30615000007637E+03 0.30615000013589E+03 0.33100579637182E+03 0.35457116978887E+03 0.30615000006006E+03
 0.30615000013527E+03 0.33403667850342E+03 0.35459958723704E+03 0.30615000007637E+03 0.30615000013589E+03
 0.33100579637182E+03 0.35457116978887E+03 0.30615000006006E+03 0.30615000013527E+03 0.39460314644339E+03
 0.31890554524181E+03 0.19064483165963E+04 0.18164735764144E+04 0.65590851744204E+03 0.11310356068378E+04
 0.47184754680851E+03 0.11166522024701E+04 0.10944289340736E+04 0.10480110809971E+04 0.17466767367045E+04
 0.10026981040240E+04 0.10939407117401E+04 0.95155757516199E+03 0.17464462604172E+04 0.11166522024701E+04
 0.10944289340736E+04 0.10480110809972E+04 0.17466767367045E+04 0.10026981040240E+04 0.10939407117401E+04
 0.95155757516199E+03 0.17464462604172E+04 0.17420806740369E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48407580081937E+03 0.12948475115379E+01
 0.12948475115379E+01 0.15800890607694E+01 0.12515739796111E-01 0.31824238492016E+03 0.33738976340705E+03
 0.33723000100244E+03 0.33722047371995E+03 0.23000000000000E+00 0.00000000000000E+00 0.18893009399361E+00
 0.00000000000000E+00 -.11969918900720E+02 0.13780496234361E-01 0.76620623912969E+00 0.58053061834249E+03
 0.21769898187843E+03 0.10441053063059E+02 0.39153948986471E+01 0.31890365548265E+03 0.39461647131516E+03
 0.30843799300482E+03 0.31326939498375E+03 0.30615000000090E+03 0.30615000001478E+03 0.30842882289499E+03
 0.31326843559202E+03 0.30615000000090E+03 0.30615000001479E+03 0.30843799300482E+03 0.31326939498375E+03
 0.30615000000090E+03 0.30615000001478E+03 0.30842882289499E+03 0.31326843559202E+03 0.30615000000090E+03
 0.30615000001479E+03 0.31217450414889E+03 0.30615000016119E+03 0.87578735517445E+02 0.86583358828895E+02
 0.13824878374571E+03 0.34747044909576E+03 0.20853042143133E+03 0.11839227434319E+03 0.12903144988986E+03
 0.11839227434319E+03 0.27815450453194E+03 0.11827522411441E+03 0.12891492109109E+03 0.11827522411441E+03
 0.27804511345556E+03 0.11839227434319E+03 0.12903144988986E+03 0.11839227434319E+03 0.27815450453194E+03
 0.11827522411441E+03 0.12891492109109E+03 0.11827522411441E+03 0.27804511345556E+03 0.18720622216308E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36020425037892E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16862795321993E+00 0.00000000000000E+00 0.00000000000000E+00 0.16862795321993E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17758184632643E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17758184632643E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158788298259E+00 0.17902003891628E+00 0.33738976340705E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    451.51280360
 0.90053578403077E-01 0.31977164555267E+03 0.44410111201619E+03 0.44036900756420E+03 0.43897773481494E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16031177162732E+00 0.00000000000000E+00 -.19434495831967E+02
 0.37400137528465E-02 0.99432745465529E+00 0.21390295674478E+04 0.80213608779291E+03 0.80456392534926E+01
 0.30171147200597E+01 0.33967360331140E+03 0.30615000169817E+03 0.33454918850716E+03 0.35537308688983E+03
 0.30615000010735E+03 0.30615000019172E+03 0.33148652879027E+03 0.35534510078215E+03 0.30615000008456E+03
 0.30615000019086E+03 0.33454918850716E+03 0.35537308688983E+03 0.30615000010735E+03 0.30615000019172E+03
 0.33148652879027E+03 0.35534510078215E+03 0.30615000008456E+03 0.30615000019086E+03 0.39554250020566E+03
 0.31961830143314E+03 0.19122528636398E+04 0.18203834228452E+04 0.65440943844740E+03 0.11236084707624E+04
 0.46592698512275E+03 0.11207193490647E+04 0.10976553473358E+04 0.10507200838052E+04 0.17465655096516E+04
 0.10070823807538E+04 0.10971871925886E+04 0.95483708075895E+03 0.17463507777765E+04 0.11207193490648E+04
 0.10976553473358E+04 0.10507200838052E+04 0.17465655096516E+04 0.10070823807538E+04 0.10971871925886E+04
 0.95483708075894E+03 0.17463507777765E+04 0.17412239288715E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48450665142637E+03 0.12948476815681E+01
 0.12948476815681E+01 0.16260684173233E+01 0.97747391040421E-02 0.31908662505567E+03 0.33749297646634E+03
 0.33737303161108E+03 0.33736616032272E+03 0.23000000000000E+00 0.00000000000000E+00 0.18751202934294E+00
 0.00000000000000E+00 -.11969145268402E+02 0.17644777627209E-01 0.79288893768870E+00 0.45339194230840E+03
 0.17002197836565E+03 0.10089685477666E+02 0.37836320541249E+01 0.31961650560889E+03 0.39555480859365E+03
 0.30853674966641E+03 0.31341753468149E+03 0.30615000000118E+03 0.30615000002077E+03 0.30852791562344E+03
 0.31341644114964E+03 0.30615000000117E+03 0.30615000002078E+03 0.30853674966641E+03 0.31341753468149E+03
 0.30615000000118E+03 0.30615000002077E+03 0.30852791562344E+03 0.31341644114964E+03 0.30615000000117E+03
 0.30615000002078E+03 0.31230128740097E+03 0.30615000022082E+03 0.85880222551409E+02 0.85140707577606E+02
 0.14056080487202E+03 0.34960893555258E+03 0.20834532665620E+03 0.12165141097333E+03 0.13113678192714E+03
 0.12165141097333E+03 0.27986249418283E+03 0.12155981426293E+03 0.13101521136290E+03 0.12155981426293E+03
 0.27974905256388E+03 0.12165141097333E+03 0.13113678192714E+03 0.12165141097333E+03 0.27986249418283E+03
 0.12155981426293E+03 0.13101521136290E+03 0.12155981426293E+03 0.27974905256389E+03 0.18797577228173E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36044282942882E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16864008961667E+00 0.00000000000000E+00 0.00000000000000E+00 0.16864008961667E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17757714083734E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17757714083734E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158775682210E+00 0.17896516028253E+00 0.33749297646634E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    460.85855976
 0.90062931818898E-01 0.31999976862370E+03 0.44445879839763E+03 0.44072241669337E+03 0.43932910336374E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15949969630425E+00 0.00000000000000E+00 -.19448641751322E+02
 0.37396249862945E-02 0.10051139839496E+01 0.21392519381808E+04 0.80221947681781E+03 0.79592962865406E+01
 0.29847361074527E+01 0.34015603601350E+03 0.30615000215751E+03 0.33496164624428E+03 0.35599364466280E+03
 0.30615000014043E+03 0.30615000025148E+03 0.33187378544042E+03 0.35596599934893E+03 0.30615000011077E+03
 0.30615000025037E+03 0.33496164624428E+03 0.35599364466280E+03 0.30615000014043E+03 0.30615000025148E+03
 0.33187378544042E+03 0.35596599934893E+03 0.30615000011077E+03 0.30615000025037E+03 0.39628923635714E+03
 0.32020119600974E+03 0.19168691565960E+04 0.18234902554648E+04 0.65323343897190E+03 0.11178246116627E+04
 0.46132500549597E+03 0.11239625221460E+04 0.11002100547073E+04 0.10528819271836E+04 0.17464890413958E+04
 0.10105804611460E+04 0.10997571698364E+04 0.95745545492117E+03 0.17462861703516E+04 0.11239625221460E+04
 0.11002100547073E+04 0.10528819271836E+04 0.17464890413958E+04 0.10105804611460E+04 0.10997571698364E+04
 0.95745545492117E+03 0.17462861703516E+04 0.17405805565139E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48485802938603E+03 0.12948477857952E+01
 0.12948477857952E+01 0.16634514419967E+01 0.79929626931552E-02 0.31975524238039E+03 0.33758183570002E+03
 0.33748684416978E+03 0.33748157810520E+03 0.23000000000000E+00 0.00000000000000E+00 0.18636478105407E+00
 0.00000000000000E+00 -.11963988351544E+02 0.21578118025165E-01 0.81443895102317E+00 0.37074595618905E+03
 0.13902973357089E+03 0.98227129116918E+01 0.36835173418844E+01 0.32019948256723E+03 0.39630074354254E+03
 0.30861827683240E+03 0.31353762927886E+03 0.30615000000148E+03 0.30615000002720E+03 0.30860971262104E+03
 0.31353642878810E+03 0.30615000000147E+03 0.30615000002721E+03 0.30861827683240E+03 0.31353762927886E+03
 0.30615000000148E+03 0.30615000002720E+03 0.30860971262104E+03 0.31353642878810E+03 0.30615000000147E+03
 0.30615000002721E+03 0.31240403463537E+03 0.30615000028295E+03 0.84370074056468E+02 0.83786290280580E+02
 0.14236493061759E+03 0.35131459601781E+03 0.20823784074713E+03 0.12426909073824E+03 0.13277805105400E+03
 0.12426909073824E+03 0.28122886765537E+03 0.12419526082620E+03 0.13265275804159E+03 0.12419526082620E+03
 0.28111249353100E+03 0.12426909073824E+03 0.13277805105400E+03 0.12426909073824E+03 0.28122886765537E+03
 0.12419526082620E+03 0.13265275804159E+03 0.12419526082620E+03 0.28111249353100E+03 0.18852802058940E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36062768702734E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16861033518322E+00 0.00000000000000E+00 0.00000000000000E+00 0.16861033518322E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17755609771832E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17755609771832E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158776423931E+00 0.17891806941739E+00 0.33758183570002E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    472.83507140
 0.90068024046386E-01 0.32028465951718E+03 0.44491589064569E+03 0.44117412773828E+03 0.43977813131089E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15849225602256E+00 0.00000000000000E+00 -.19476996957004E+02
 0.37394131101121E-02 0.10184407659015E+01 0.21393731487881E+04 0.80226493079554E+03 0.78551451079419E+01
 0.29456794154782E+01 0.34076568768553E+03 0.30615000292147E+03 0.33548310413745E+03 0.35677851057260E+03
 0.30615000019761E+03 0.30615000035506E+03 0.33236342739061E+03 0.35675128107984E+03 0.30615000015619E+03
 0.30615000035351E+03 0.33548310413745E+03 0.35677851057260E+03 0.30615000019761E+03 0.30615000035506E+03
 0.33236342739061E+03 0.35675128107984E+03 0.30615000015619E+03 0.30615000035351E+03 0.39723578638739E+03
 0.32095450742402E+03 0.19226498525626E+04 0.18273351655609E+04 0.65162331962477E+03 0.11104304186146E+04
 0.45554898239167E+03 0.11280360082223E+04 0.11033809984996E+04 0.10555615391615E+04 0.17463772214603E+04
 0.10149785911627E+04 0.11029464277624E+04 0.96071637284518E+03 0.17461885040790E+04 0.11280360082223E+04
 0.11033809984996E+04 0.10555615391615E+04 0.17463772214602E+04 0.10149785911627E+04 0.11029464277624E+04
 0.96071637284518E+03 0.17461885040790E+04 0.17397186586643E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48530711621446E+03 0.12948479947164E+01
 0.12948479947164E+01 0.17113574885543E+01 0.61754717746427E-02 0.32059935920950E+03 0.33770140364990E+03
 0.33763099485475E+03 0.33762725525387E+03 0.23000000000000E+00 0.00000000000000E+00 0.18490569863507E+00
 0.00000000000000E+00 -.11968793999698E+02 0.27928730308257E-01 0.84180001215187E+00 0.28644338327242E+03
 0.10741626872716E+03 0.95034448616244E+01 0.35637918231091E+01 0.32095290060953E+03 0.39724634175458E+03
 0.30872236958628E+03 0.31369003272119E+03 0.30615000000202E+03 0.30615000003840E+03 0.30871413001460E+03
 0.31368870030249E+03 0.30615000000201E+03 0.30615000003841E+03 0.30872236958628E+03 0.31369003272119E+03
 0.30615000000202E+03 0.30615000003840E+03 0.30871413001460E+03 0.31368870030249E+03 0.30615000000201E+03
 0.30615000003841E+03 0.31253430361668E+03 0.30615000038735E+03 0.82257605584340E+02 0.81827835101435E+02
 0.14458535058112E+03 0.35347191875429E+03 0.20816364142026E+03 0.12758929436809E+03 0.13479672618519E+03
 0.12758929436809E+03 0.28296062035811E+03 0.12753488225118E+03 0.13466710979797E+03 0.12753488225118E+03
 0.28284089700035E+03 0.12758929436809E+03 0.13479672618519E+03 0.12758929436809E+03 0.28296062035811E+03
 0.12753488225118E+03 0.13466710979797E+03 0.12753488225118E+03 0.28284089700035E+03 0.18915389424845E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36085760564627E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866030756066E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866030756066E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17755146878517E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17755146878517E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158742415958E+00 0.17885435797972E+00 0.33770140364990E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    480.24109121
 0.90044593669762E-01 0.32046985993726E+03 0.44520061614291E+03 0.44145683938935E+03 0.44005977217843E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15788730068619E+00 0.00000000000000E+00 -.19474090792915E+02
 0.37403858558240E-02 0.10264068006498E+01 0.21388167714150E+04 0.80205628928064E+03 0.77941806259809E+01
 0.29228177347428E+01 0.34114097737890E+03 0.30615000348344E+03 0.33580458239353E+03 0.35725824038748E+03
 0.30615000024074E+03 0.30615000043331E+03 0.33266598659701E+03 0.35723126932912E+03 0.30615000019050E+03
 0.30615000043144E+03 0.33580458239353E+03 0.35725824038748E+03 0.30615000024074E+03 0.30615000043331E+03
 0.33266598659701E+03 0.35723126932912E+03 0.30615000019050E+03 0.30615000043144E+03 0.39780004770586E+03
 0.32141860228126E+03 0.19261848554667E+04 0.18297373984572E+04 0.65081826546030E+03 0.11063219500403E+04
 0.45224959325268E+03 0.11305280644021E+04 0.11053314031679E+04 0.10572457624521E+04 0.17463684627799E+04
 0.10176677806523E+04 0.11049074885478E+04 0.96274969262587E+03 0.17461878285981E+04 0.11305280644021E+04
 0.11053314031679E+04 0.10572457624521E+04 0.17463684627799E+04 0.10176677806523E+04 0.11049074885478E+04
 0.96274969262587E+03 0.17461878285981E+04 0.17393422067894E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48558858360451E+03 0.12948479733037E+01
 0.12948479733037E+01 0.17409815677967E+01 0.52641906553665E-02 0.32110667153021E+03 0.33777827442197E+03
 0.33771976609120E+03 0.33771673959738E+03 0.23000000000000E+00 0.00000000000000E+00 0.18401131405239E+00
 0.00000000000000E+00 -.11949405771935E+02 0.32763456543357E-01 0.85854649800911E+00 0.24417448108423E+03
 0.91565430406587E+02 0.93180742319156E+01 0.34942778369683E+01 0.32141707440971E+03 0.39780999350888E+03
 0.30878941196298E+03 0.31378494902138E+03 0.30615000000245E+03 0.30615000004688E+03 0.30878137811341E+03
 0.31378353481665E+03 0.30615000000243E+03 0.30615000004689E+03 0.30878941196298E+03 0.31378494902138E+03
 0.30615000000245E+03 0.30615000004688E+03 0.30878137811341E+03 0.31378353481665E+03 0.30615000000243E+03
 0.30615000004689E+03 0.31261550175907E+03 0.30615000046465E+03 0.80893011740485E+02 0.80532089185139E+02
 0.14590246054383E+03 0.35476293001956E+03 0.20813095717302E+03 0.12961152269739E+03 0.13599204138806E+03
 0.12961152269739E+03 0.28399432644414E+03 0.12956740074323E+03 0.13586000900217E+03 0.12956740074323E+03
 0.28387279056258E+03 0.12961152269739E+03 0.13599204138806E+03 0.12961152269739E+03 0.28399432644414E+03
 0.12956740074323E+03 0.13586000900217E+03 0.12956740074323E+03 0.28387279056259E+03 0.18949201073575E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36099262618627E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16851530085817E+00 0.00000000000000E+00 0.00000000000000E+00 0.16851530085817E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17746826316046E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17746826316046E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158784619618E+00 0.17881412877389E+00 0.33777827442197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    491.35901554
 0.90012520433247E-01 0.32073696324846E+03 0.44562646397230E+03 0.44187925772703E+03 0.44048028296921E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15700395833021E+00 0.00000000000000E+00 -.19488562762941E+02
 0.37417182096910E-02 0.10379967001958E+01 0.21380551799117E+04 0.80177069246690E+03 0.77071535954696E+01
 0.28901825983011E+01 0.34169796702852E+03 0.30615000451262E+03 0.33628180868372E+03 0.35797158202394E+03
 0.30615000032205E+03 0.30615000058108E+03 0.33311502370851E+03 0.35794498155401E+03 0.30615000025531E+03
 0.30615000057863E+03 0.33628180868372E+03 0.35797158202394E+03 0.30615000032205E+03 0.30615000058108E+03
 0.33311502370851E+03 0.35794498155401E+03 0.30615000025531E+03 0.30615000057863E+03 0.39864520131509E+03
 0.32211978410844E+03 0.19313902041907E+04 0.18332072464256E+04 0.64945985307564E+03 0.11000195107916E+04
 0.44731235845055E+03 0.11342086922598E+04 0.11081757519622E+04 0.10596766732497E+04 0.17463246210830E+04
 0.10216436609394E+04 0.11077669525033E+04 0.96570520130592E+03 0.17461554094417E+04 0.11342086922598E+04
 0.11081757519622E+04 0.10596766732497E+04 0.17463246210830E+04 0.10216436609394E+04 0.11077669525033E+04
 0.96570520130592E+03 0.17461554094417E+04 0.17386917308713E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48600896236848E+03 0.12948480799332E+01
 0.12948480799332E+01 0.17854532650989E+01 0.41417979455152E-02 0.32186673007563E+03 0.33789787258398E+03
 0.33785360741524E+03 0.33785140910008E+03 0.23000000000000E+00 0.00000000000000E+00 0.18268179129622E+00
 0.00000000000000E+00 -.11941015657212E+02 0.41642078695100E-01 0.88340378269235E+00 0.19211336827288E+03
 0.72042513102329E+02 0.90558815308877E+01 0.33959555740829E+01 0.32211837214594E+03 0.39865431465922E+03
 0.30888908343316E+03 0.31392600000113E+03 0.30615000000329E+03 0.30615000006294E+03 0.30888133562248E+03
 0.31392446783377E+03 0.30615000000326E+03 0.30615000006296E+03 0.30888908343316E+03 0.31392600000113E+03
 0.30615000000329E+03 0.30615000006294E+03 0.30888133562248E+03 0.31392446783377E+03 0.30615000000326E+03
 0.30615000006296E+03 0.31273605760836E+03 0.30615000060732E+03 0.78717829315010E+02 0.78445442124760E+02
 0.14781570123235E+03 0.35669121879111E+03 0.20813643905259E+03 0.13262373112124E+03 0.13772765060910E+03
 0.13262373112124E+03 0.28553862777441E+03 0.13259298021556E+03 0.13759230700732E+03 0.13259298021556E+03
 0.28541465128905E+03 0.13262373112124E+03 0.13772765060910E+03 0.13262373112124E+03 0.28553862777441E+03
 0.13259298021556E+03 0.13759230700733E+03 0.13259298021556E+03 0.28541465128905E+03 0.18995287214265E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36119523665211E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16845953225158E+00 0.00000000000000E+00 0.00000000000000E+00 0.16845953225158E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17740336811604E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17740336811604E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158787112968E+00 0.17875087871344E+00 0.33789787258398E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    500.88866496
 0.89974745485117E-01 0.32096455918870E+03 0.44599108212046E+03 0.44224133892724E+03 0.44084085617676E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15626977803332E+00 0.00000000000000E+00 -.19499638302037E+02
 0.37432887710189E-02 0.10475876688168E+01 0.21371581220068E+04 0.80143429575255E+03 0.76365923713437E+01
 0.28637221392539E+01 0.34217064267451E+03 0.30615000559499E+03 0.33668706650690E+03 0.35857607865468E+03
 0.30615000041014E+03 0.30615000074145E+03 0.33349661838760E+03 0.35854978591786E+03 0.30615000032566E+03
 0.30615000073837E+03 0.33668706650690E+03 0.35857607865468E+03 0.30615000041014E+03 0.30615000074145E+03
 0.33349661838760E+03 0.35854978591786E+03 0.30615000032566E+03 0.30615000073837E+03 0.39935821209376E+03
 0.32272222900610E+03 0.19357752493620E+04 0.18361208762471E+04 0.64830171185607E+03 0.10947599076244E+04
 0.44321668720903E+03 0.11373161092192E+04 0.11105613822229E+04 0.10617226811638E+04 0.17462979181642E+04
 0.10250018400464E+04 0.11101647564446E+04 0.96819559836101E+03 0.17461378173374E+04 0.11373161092192E+04
 0.11105613822229E+04 0.10617226811638E+04 0.17462979181642E+04 0.10250018400464E+04 0.11101647564446E+04
 0.96819559836101E+03 0.17461378173374E+04 0.17381607646511E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48636938733585E+03 0.12948481615378E+01
 0.12948481615378E+01 0.18235718627864E+01 0.33718753142096E-02 0.32251109082868E+03 0.33800415299893E+03
 0.33796932588302E+03 0.33796765676952E+03 0.23000000000000E+00 0.00000000000000E+00 0.18155621554608E+00
 0.00000000000000E+00 -.11932387450120E+02 0.51150488983023E-01 0.90441344795351E+00 0.15640124188559E+03
 0.58650465707096E+02 0.88455119924436E+01 0.33170669971663E+01 0.32272091459806E+03 0.39936666699176E+03
 0.30897519787241E+03 0.31404631843418E+03 0.30615000000424E+03 0.30615000008044E+03 0.30896768250318E+03
 0.31404468805398E+03 0.30615000000420E+03 0.30615000008047E+03 0.30897519787241E+03 0.31404631843418E+03
 0.30615000000424E+03 0.30615000008044E+03 0.30896768250318E+03 0.31404468805398E+03 0.30615000000420E+03
 0.30615000008047E+03 0.31283888062426E+03 0.30615000075853E+03 0.76757365218529E+02 0.76543888243251E+02
 0.14939535090314E+03 0.35832117650049E+03 0.20817884884283E+03 0.13518024949787E+03 0.13915917719001E+03
 0.13518024949787E+03 0.28684148104982E+03 0.13515918772240E+03 0.13902126949922E+03 0.13515918772240E+03
 0.28671566532090E+03 0.13518024949787E+03 0.13915917719002E+03 0.13518024949787E+03 0.28684148104982E+03
 0.13515918772240E+03 0.13902126949923E+03 0.13515918772240E+03 0.28671566532091E+03 0.19030271367080E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36136628739539E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16840056588614E+00 0.00000000000000E+00 0.00000000000000E+00 0.16840056588614E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17733471539740E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17733471539740E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158792263407E+00 0.17869474200009E+00 0.33800415299893E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    511.84296642
 0.89927293150574E-01 0.32122253490994E+03 0.44640905686793E+03 0.44265649518173E+03 0.44125426949892E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15545109256238E+00 0.00000000000000E+00 -.19514222185920E+02
 0.37452635969821E-02 0.10582371440907E+01 0.21360312279345E+04 0.80101171047543E+03 0.75597422039784E+01
 0.28349033264919E+01 0.34270837146764E+03 0.30615000711999E+03 0.33714835360671E+03 0.35926331711022E+03
 0.30615000053802E+03 0.30615000097466E+03 0.33393119012471E+03 0.35923736595402E+03 0.30615000042800E+03
 0.30615000097069E+03 0.33714835360671E+03 0.35926331711022E+03 0.30615000053802E+03 0.30615000097466E+03
 0.33393119012471E+03 0.35923736595402E+03 0.30615000042800E+03 0.30615000097069E+03 0.40016730805917E+03
 0.32341664716511E+03 0.19407250201713E+04 0.18393856595064E+04 0.64694413292921E+03 0.10888058055319E+04
 0.43862695193802E+03 0.11408329023930E+04 0.11132393018444E+04 0.10640204150660E+04 0.17462665151805E+04
 0.10288045202325E+04 0.11128558438648E+04 0.97099928766535E+03 0.17461161845011E+04 0.11408329023930E+04
 0.11132393018444E+04 0.10640204150660E+04 0.17462665151805E+04 0.10288045202325E+04 0.11128558438648E+04
 0.97099928766535E+03 0.17461161845011E+04 0.17375511427963E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48678264415710E+03 0.12948482689920E+01
 0.12948482689920E+01 0.18673890686142E+01 0.26612050147548E-02 0.32324686054452E+03 0.33813074524664E+03
 0.33810433920090E+03 0.33810312559216E+03 0.23000000000000E+00 0.00000000000000E+00 0.18027980610683E+00
 0.00000000000000E+00 -.11924636551313E+02 0.64810138483863E-01 0.92819839234385E+00 0.12343747733222E+03
 0.46289053999583E+02 0.86188470762147E+01 0.32320676535805E+01 0.32341544576279E+03 0.40017505172994E+03
 0.30907455630564E+03 0.31418378368361E+03 0.30615000000568E+03 0.30615000010599E+03 0.30906729175845E+03
 0.31418204394968E+03 0.30615000000562E+03 0.30615000010602E+03 0.30907455630564E+03 0.31418378368361E+03
 0.30615000000568E+03 0.30615000010599E+03 0.30906729175845E+03 0.31418204394968E+03 0.30615000000562E+03
 0.30615000010602E+03 0.31295633527751E+03 0.30615000097321E+03 0.74400311858666E+02 0.74241107497698E+02
 0.15115103230505E+03 0.36018199642381E+03 0.20827520895724E+03 0.13809656176190E+03 0.14074890356871E+03
 0.13809656176190E+03 0.28832584541106E+03 0.13808490864746E+03 0.14060832269010E+03 0.13808490864746E+03
 0.28819816377716E+03 0.13809656176190E+03 0.14074890356871E+03 0.13809656176190E+03 0.28832584541106E+03
 0.13808490864746E+03 0.14060832269011E+03 0.13808490864746E+03 0.28819816377716E+03 0.19066404328586E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36156209872761E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16834934496761E+00 0.00000000000000E+00 0.00000000000000E+00 0.16834934496761E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17725852312141E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17725852312141E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158790761993E+00 0.17862783879178E+00 0.33813074524664E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    520.60640758
 0.89883883613197E-01 0.32142850798120E+03 0.44674282442615E+03 0.44298824528135E+03 0.44158471086464E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15481499422010E+00 0.00000000000000E+00 -.19521682311711E+02
 0.37470720477659E-02 0.10664763538840E+01 0.21350003143841E+04 0.80062511789402E+03 0.75013383755435E+01
 0.28130018908288E+01 0.34313478232136E+03 0.30615000858615E+03 0.33751439374108E+03 0.35980738205262E+03
 0.30615000066433E+03 0.30615000120533E+03 0.33427629756181E+03 0.35978169677854E+03 0.30615000052927E+03
 0.30615000120049E+03 0.33751439374108E+03 0.35980738205262E+03 0.30615000066433E+03 0.30615000120533E+03
 0.33427629756181E+03 0.35978169677854E+03 0.30615000052927E+03 0.30615000120049E+03 0.40080454574291E+03
 0.32397246787532E+03 0.19446222109240E+04 0.18419519998620E+04 0.64587461941613E+03 0.10841747487316E+04
 0.43507075621835E+03 0.11436079709976E+04 0.11153406880375E+04 0.10658323156462E+04 0.17462502318802E+04
 0.10318060875292E+04 0.11149671690589E+04 0.97321079765938E+03 0.17461071986694E+04 0.11436079709976E+04
 0.11153406880375E+04 0.10658323156462E+04 0.17462502318803E+04 0.10318060875292E+04 0.11149671690589E+04
 0.97321079765938E+03 0.17461071986694E+04 0.17370895238328E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48711293417344E+03 0.12948483239582E+01
 0.12948483239582E+01 0.19024428332764E+01 0.22018660210934E-02 0.32383004997295E+03 0.33823542993231E+03
 0.33821428412121E+03 0.33821334490498E+03 0.23000000000000E+00 0.00000000000000E+00 0.17927293674349E+00
 0.00000000000000E+00 -.11913802963205E+02 0.78330406765505E-01 0.94692939899372E+00 0.10213147525136E+03
 0.38299303219259E+02 0.84483595170891E+01 0.31681348189084E+01 0.32397136098200E+03 0.40081174151533E+03
 0.30915476801000E+03 0.31429337375516E+03 0.30615000000717E+03 0.30615000013134E+03 0.30914769364653E+03
 0.31429154885186E+03 0.30615000000708E+03 0.30615000013138E+03 0.30915476801000E+03 0.31429337375516E+03
 0.30615000000717E+03 0.30615000013134E+03 0.30914769364653E+03 0.31429154885186E+03 0.30615000000708E+03
 0.30615000013138E+03 0.31304997696657E+03 0.30615000118103E+03 0.72448322478906E+02 0.72322655136828E+02
 0.15251257774502E+03 0.36166024191470E+03 0.20838510128095E+03 0.14041197196414E+03 0.14198064949923E+03
 0.14041197196414E+03 0.28950173166082E+03 0.14040667682717E+03 0.14183812867811E+03 0.14040667682717E+03
 0.28937273892830E+03 0.14041197196414E+03 0.14198064949923E+03 0.14041197196414E+03 0.28950173166082E+03
 0.14040667682717E+03 0.14183812867811E+03 0.14040667682717E+03 0.28937273892830E+03 0.19092530231676E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36171755312222E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16827275741452E+00 0.00000000000000E+00 0.00000000000000E+00 0.16827275741452E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17717494330534E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17717494330534E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158802013761E+00 0.17857268924190E+00 0.33823542993231E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    531.56070904
 0.89814097695178E-01 0.32168564070970E+03 0.44715922887780E+03 0.44340279650917E+03 0.44199786979045E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15404252860298E+00 0.00000000000000E+00 -.19532010375241E+02
 0.37499831260127E-02 0.10764387291938E+01 0.21333429328004E+04 0.80000359980016E+03 0.74319139427391E+01
 0.27869677285272E+01 0.34366318816999E+03 0.30615001077870E+03 0.33796827253986E+03 0.36048063037566E+03
 0.30615000085831E+03 0.30615000156005E+03 0.33470451013612E+03 0.36045526806050E+03 0.30615000068509E+03
 0.30615000155390E+03 0.33796827253985E+03 0.36048063037566E+03 0.30615000085831E+03 0.30615000156005E+03
 0.33470451013612E+03 0.36045526806050E+03 0.30615000068509E+03 0.30615000155390E+03 0.40159023886637E+03
 0.32466749732297E+03 0.19494281488435E+04 0.18451165758759E+04 0.64454476939665E+03 0.10785175172967E+04
 0.43075002405312E+03 0.11470351824321E+04 0.11179205572502E+04 0.10680698978710E+04 0.17462415145076E+04
 0.10355142530217E+04 0.11175587736881E+04 0.97594263134734E+03 0.17461070111725E+04 0.11470351824321E+04
 0.11179205572502E+04 0.10680698978710E+04 0.17462415145076E+04 0.10355142530217E+04 0.11175587736881E+04
 0.97594263134734E+03 0.17461070111725E+04 0.17365419561378E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48752582834563E+03 0.12948484000554E+01
 0.12948484000554E+01 0.19462600391042E+01 0.17373666008376E-02 0.32455421851571E+03 0.33836992442197E+03
 0.33835392245797E+03 0.33835324210483E+03 0.23000000000000E+00 0.00000000000000E+00 0.17803324320171E+00
 0.00000000000000E+00 -.11901378814072E+02 0.99272689708544E-01 0.96995374218277E+00 0.80586111079364E+02
 0.30219791654762E+02 0.82478160061498E+01 0.30929310023062E+01 0.32466651228994E+03 0.40159678061359E+03
 0.30925573820220E+03 0.31442979518380E+03 0.30615000000953E+03 0.30615000017046E+03 0.30924888806604E+03
 0.31442786677988E+03 0.30615000000942E+03 0.30615000017052E+03 0.30925573820220E+03 0.31442979518380E+03
 0.30615000000953E+03 0.30615000017046E+03 0.30924888806604E+03 0.31442786677988E+03 0.30615000000942E+03
 0.30615000017052E+03 0.31316655626157E+03 0.30615000149390E+03 0.69927930168478E+02 0.69835575939478E+02
 0.15416270552873E+03 0.36349413971085E+03 0.20856062065447E+03 0.14328473181083E+03 0.14347203654187E+03
 0.14328473181083E+03 0.29095635068154E+03 0.14328612901361E+03 0.14332731663590E+03 0.14328612901361E+03
 0.29082592334783E+03 0.14328473181083E+03 0.14347203654187E+03 0.14328473181083E+03 0.29095635068154E+03
 0.14328612901361E+03 0.14332731663590E+03 0.14328612901361E+03 0.29082592334784E+03 0.19121909205499E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36191206876854E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16818577248202E+00 0.00000000000000E+00 0.00000000000000E+00 0.16818577248202E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17707057522936E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17707057522936E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158811969678E+00 0.17850183605309E+00 0.33836992442197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    540.32415021
 0.89758443518201E-01 0.32188924099994E+03 0.44749154640800E+03 0.44373359059609E+03 0.44232751160571E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15344209465409E+00 0.00000000000000E+00 -.19540404845605E+02
 0.37523079520151E-02 0.10841487380798E+01 0.21320211726502E+04 0.79950793974383E+03 0.73790613031286E+01
 0.27671479886732E+01 0.34408230770770E+03 0.30615001286411E+03 0.33832849201466E+03 0.36101393679866E+03
 0.30615000104759E+03 0.30615000190659E+03 0.33504458349418E+03 0.36098882556950E+03 0.30615000083740E+03
 0.30615000189918E+03 0.33832849201466E+03 0.36101393679866E+03 0.30615000104759E+03 0.30615000190659E+03
 0.33504458349418E+03 0.36098882556950E+03 0.30615000083740E+03 0.30615000189918E+03 0.40221056532124E+03
 0.32522350304913E+03 0.19532147305807E+04 0.18475955680441E+04 0.64348337807392E+03 0.10740859085140E+04
 0.42738511354966E+03 0.11497419303923E+04 0.11199473104039E+04 0.10698270457322E+04 0.17462414689642E+04
 0.10384436784336E+04 0.11195943950533E+04 0.97809087603238E+03 0.17461133439466E+04 0.11497419303923E+04
 0.11199473104039E+04 0.10698270457322E+04 0.17462414689642E+04 0.10384436784336E+04 0.11195943950533E+04
 0.97809087603238E+03 0.17461133439466E+04 0.17361170969474E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48785527574804E+03 0.12948484619060E+01
 0.12948484619060E+01 0.19813138037664E+01 0.14372604570529E-02 0.32513004172143E+03 0.33848098610796E+03
 0.33846819358500E+03 0.33846766880452E+03 0.23000000000000E+00 0.00000000000000E+00 0.17705698376854E+00
 0.00000000000000E+00 -.11891664651020E+02 0.12000124966340E+00 0.98805315864547E+00 0.66665972416450E+02
 0.24999739656169E+02 0.80967303530179E+01 0.30362738823817E+01 0.32522261707537E+03 0.40221661120160E+03
 0.30933706094116E+03 0.31453852233690E+03 0.30615000001194E+03 0.30615000020882E+03 0.30933037968970E+03
 0.31453651345029E+03 0.30615000001179E+03 0.30615000020888E+03 0.30933706094116E+03 0.31453852233690E+03
 0.30615000001194E+03 0.30615000020882E+03 0.30933037968970E+03 0.31453651345029E+03 0.30615000001179E+03
 0.30615000020888E+03 0.31325948006831E+03 0.30615000179337E+03 0.67856663364219E+02 0.67785360160462E+02
 0.15544888259583E+03 0.36496272521019E+03 0.20873659820138E+03 0.14556970123440E+03 0.14463378006492E+03
 0.14556970123440E+03 0.29211827731587E+03 0.14557558331869E+03 0.14448746549325E+03 0.14557558331869E+03
 0.29198685013118E+03 0.14556970123440E+03 0.14463378006492E+03 0.14556970123440E+03 0.29211827731587E+03
 0.14557558331869E+03 0.14448746549325E+03 0.14557558331869E+03 0.29198685013118E+03 0.19143714251480E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36206867123789E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16811789477579E+00 0.00000000000000E+00 0.00000000000000E+00 0.16811789477579E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17698602507307E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17698602507307E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158818496096E+00 0.17844335138418E+00 0.33848098610796E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    551.27845166
 0.89685392497056E-01 0.32214230720871E+03 0.44790571250184E+03 0.44414599904668E+03 0.44273852412263E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15271277862780E+00 0.00000000000000E+00 -.19550749365242E+02
 0.37553638927142E-02 0.10934726977972E+01 0.21302862328524E+04 0.79885733731964E+03 0.73161406005987E+01
 0.27435527252245E+01 0.34460183198390E+03 0.30615001595057E+03 0.33877526999577E+03 0.36167415312073E+03
 0.30615000133488E+03 0.30615000243317E+03 0.33546664606824E+03 0.36164934698491E+03 0.30615000106900E+03
 0.30615000242388E+03 0.33877526999577E+03 0.36167415312072E+03 0.30615000133488E+03 0.30615000243317E+03
 0.33546664606824E+03 0.36164934698491E+03 0.30615000106900E+03 0.30615000242388E+03 0.40297615324355E+03
 0.32591819047240E+03 0.19578795447172E+04 0.18506399125117E+04 0.64215709289004E+03 0.10686533858377E+04
 0.42328550748321E+03 0.11530836731835E+04 0.11224348238561E+04 0.10719906013489E+04 0.17462466793157E+04
 0.10420613684043E+04 0.11220923887914E+04 0.98073794682945E+03 0.17461260116407E+04 0.11530836731836E+04
 0.11224348238561E+04 0.10719906013489E+04 0.17462466793157E+04 0.10420613684043E+04 0.11220923887914E+04
 0.98073794682945E+03 0.17461260116407E+04 0.17356013981114E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48826603422438E+03 0.12948485381245E+01
 0.12948485381245E+01 0.20251310095941E+01 0.11338224363167E-02 0.32584255930372E+03 0.33862382423663E+03
 0.33861416311334E+03 0.33861378445077E+03 0.23000000000000E+00 0.00000000000000E+00 0.17585663547355E+00
 0.00000000000000E+00 -.11879414451140E+02 0.15211645109869E+00 0.10102670623808E+01 0.52591287413151E+02
 0.19721732779932E+02 0.79186982312848E+01 0.29695118367318E+01 0.32591742952634E+03 0.40298161192886E+03
 0.30943938505396E+03 0.31467395264199E+03 0.30615000001572E+03 0.30615000026730E+03 0.30943290236903E+03
 0.31467184595993E+03 0.30615000001553E+03 0.30615000026739E+03 0.30943938505396E+03 0.31467395264199E+03
 0.30615000001572E+03 0.30615000026730E+03 0.30943290236903E+03 0.31467184595993E+03 0.30615000001553E+03
 0.30615000026739E+03 0.31337524538674E+03 0.30615000223934E+03 0.65204157420336E+02 0.65153164404464E+02
 0.15701720835686E+03 0.36679993443590E+03 0.20899764003726E+03 0.14841052927189E+03 0.14604946189558E+03
 0.14841052927189E+03 0.29356800192214E+03 0.14842109134202E+03 0.14590133950156E+03 0.14842109134202E+03
 0.29343549014625E+03 0.14841052927189E+03 0.14604946189558E+03 0.14841052927189E+03 0.29356800192214E+03
 0.14842109134202E+03 0.14590133950156E+03 0.14842109134202E+03 0.29343549014625E+03 0.19169033930299E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36226570048236E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16803246455229E+00 0.00000000000000E+00 0.00000000000000E+00 0.16803246455229E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17687690877233E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17687690877233E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158826074280E+00 0.17836818085149E+00 0.33862382423663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    560.75346018
 0.89620269495357E-01 0.32235938699919E+03 0.44826262889854E+03 0.44450146807542E+03 0.44309279875586E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15210037882014E+00 0.00000000000000E+00 -.19559859525436E+02
 0.37580923937239E-02 0.11012662280650E+01 0.21287395736624E+04 0.79827734012342E+03 0.72643651427105E+01
 0.27241369285164E+01 0.34504728641095E+03 0.30615001911749E+03 0.33915856879027E+03 0.36223961633949E+03
 0.30615000163727E+03 0.30615000298799E+03 0.33582895537420E+03 0.36221506623762E+03 0.30615000131322E+03
 0.30615000297676E+03 0.33915856879027E+03 0.36223961633949E+03 0.30615000163727E+03 0.30615000298799E+03
 0.33582895537420E+03 0.36221506623762E+03 0.30615000131322E+03 0.30615000297676E+03 0.40363041449277E+03
 0.32651877427531E+03 0.19618540840501E+04 0.18532220428025E+04 0.64100078127499E+03 0.10640282709621E+04
 0.41982248578071E+03 0.11559374882478E+04 0.11245449533019E+04 0.10738303217736E+04 0.17462522913976E+04
 0.10451518587654E+04 0.11242110752862E+04 0.98299153624697E+03 0.17461376456582E+04 0.11559374882478E+04
 0.11245449533019E+04 0.10738303217736E+04 0.17462522913976E+04 0.10451518587654E+04 0.11242110752862E+04
 0.98299153624697E+03 0.17461376456582E+04 0.17351609497895E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48862008914121E+03 0.12948486052482E+01
 0.12948486052482E+01 0.20630310436599E+01 0.92347251128055E-03 0.32646075928398E+03 0.33875087054509E+03
 0.33874330415848E+03 0.33874301949233E+03 0.23000000000000E+00 0.00000000000000E+00 0.17483664264793E+00
 0.00000000000000E+00 -.11869036418414E+02 0.18676575428928E+00 0.10291071746861E+01 0.42834405217612E+02
 0.16062901956605E+02 0.77737287201790E+01 0.29151482700671E+01 0.32651812155480E+03 0.40363539479382E+03
 0.30952837443954E+03 0.31479063579060E+03 0.30615000001986E+03 0.30615000032915E+03 0.30952205260838E+03
 0.31478844699712E+03 0.30615000001962E+03 0.30615000032925E+03 0.30952837443954E+03 0.31479063579060E+03
 0.30615000001986E+03 0.30615000032915E+03 0.30952205260838E+03 0.31478844699712E+03 0.30615000001962E+03
 0.30615000032925E+03 0.31347500396352E+03 0.30615000269977E+03 0.62856545376912E+02 0.62820016143400E+02
 0.15834220942572E+03 0.36839365743374E+03 0.20925973696089E+03 0.15085613422102E+03 0.14724481305515E+03
 0.15085613422102E+03 0.29482234530657E+03 0.15087003396129E+03 0.14709527592851E+03 0.15087003396129E+03
 0.29468902651891E+03 0.15085613422102E+03 0.14724481305515E+03 0.15085613422102E+03 0.29482234530657E+03
 0.15087003396128E+03 0.14709527592851E+03 0.15087003396128E+03 0.29468902651892E+03 0.19189540810445E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36243750794676E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16796070744255E+00 0.00000000000000E+00 0.00000000000000E+00 0.16796070744255E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17678015058927E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17678015058927E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158831251368E+00 0.17830135709336E+00 0.33875087054509E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    570.46565781
 0.89551552662626E-01 0.32258077006971E+03 0.44862721331689E+03 0.44486466175009E+03 0.44345479852406E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15148982770050E+00 0.00000000000000E+00 -.19568577504646E+02
 0.37609757830122E-02 0.11090023387608E+01 0.21271075544105E+04 0.79766533290393E+03 0.72136908285868E+01
 0.27051340607201E+01 0.34550034957737E+03 0.30615002290929E+03 0.33954863927146E+03 0.36281396376879E+03
 0.30615000200786E+03 0.30615000366856E+03 0.33619790295951E+03 0.36278966923477E+03 0.30615000161303E+03
 0.30615000365499E+03 0.33954863927146E+03 0.36281396376879E+03 0.30615000200786E+03 0.30615000366856E+03
 0.33619790295951E+03 0.36278966923477E+03 0.30615000161303E+03 0.30615000365499E+03 0.40429287293236E+03
 0.32713351542883E+03 0.19658727766470E+04 0.18558251046645E+04 0.63981906983211E+03 0.10593764252080E+04
 0.41635826002670E+03 0.11588290959058E+04 0.11266710606421E+04 0.10756898804407E+04 0.17462610459517E+04
 0.10482839853345E+04 0.11263454967751E+04 0.98527071312286E+03 0.17461521858276E+04 0.11588290959058E+04
 0.11266710606421E+04 0.10756898804407E+04 0.17462610459517E+04 0.10482839853345E+04 0.11263454967751E+04
 0.98527071312286E+03 0.17461521858276E+04 0.17347217362437E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48898185605309E+03 0.12948486694824E+01
 0.12948486694824E+01 0.21018798341934E+01 0.74822759389059E-03 0.32709059895927E+03 0.33888436381773E+03
 0.33887848087086E+03 0.33887826886160E+03 0.23000000000000E+00 0.00000000000000E+00 0.17380894580721E+00
 0.00000000000000E+00 -.11857832551731E+02 0.23050878673629E+00 0.10480546425490E+01 0.34705835353479E+02
 0.13014688257555E+02 0.76331897929890E+01 0.28624461723709E+01 0.32713297444976E+03 0.40429738684774E+03
 0.30962021655882E+03 0.31490991697158E+03 0.30615000002512E+03 0.30615000040527E+03 0.30961405005956E+03
 0.31490764623677E+03 0.30615000002482E+03 0.30615000040540E+03 0.30962021655882E+03 0.31490991697158E+03
 0.30615000002512E+03 0.30615000040527E+03 0.30961405005956E+03 0.31490764623677E+03 0.30615000002482E+03
 0.30615000040540E+03 0.31357700870081E+03 0.30615000325414E+03 0.60406356087453E+02 0.60381353798796E+02
 0.15967271694950E+03 0.37003254718440E+03 0.20956146665016E+03 0.15335217649412E+03 0.14844444494508E+03
 0.15335217649412E+03 0.29610907803091E+03 0.15336891530761E+03 0.14829358838064E+03 0.15336891530761E+03
 0.29597504676351E+03 0.15335217649412E+03 0.14844444494508E+03 0.15335217649412E+03 0.29610907803091E+03
 0.15336891530761E+03 0.14829358838064E+03 0.15336891530761E+03 0.29597504676351E+03 0.19209462198739E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36261514019291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16788253679821E+00 0.00000000000000E+00 0.00000000000000E+00 0.16788253679821E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17667919154506E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17667919154506E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158837500333E+00 0.17823120527766E+00 0.33888436381773E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    580.17785545
 0.89481213397763E-01 0.32280098880909E+03 0.44899048993215E+03 0.44522662670596E+03 0.44381559501180E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15089613583375E+00 0.00000000000000E+00 -.19577277892166E+02
 0.37639318471391E-02 0.11164910656955E+01 0.21254369964431E+04 0.79703887366618E+03 0.71653058817957E+01
 0.26869897056734E+01 0.34594991545644E+03 0.30615002732597E+03 0.33993591917110E+03 0.36338314163276E+03
 0.30615000244956E+03 0.30615000448038E+03 0.33656443787391E+03 0.36335909587061E+03 0.30615000197096E+03
 0.30615000446406E+03 0.33993591917109E+03 0.36338314163276E+03 0.30615000244956E+03 0.30615000448038E+03
 0.33656443787391E+03 0.36335909587061E+03 0.30615000197096E+03 0.30615000446406E+03 0.40494762499534E+03
 0.32774723999211E+03 0.19698383549535E+04 0.18583862909140E+04 0.63863747050369E+03 0.10548040985384E+04
 0.41297344068219E+03 0.11616883841641E+04 0.11287617701871E+04 0.10775242534816E+04 0.17462729190581E+04
 0.10513817831989E+04 0.11284440850552E+04 0.98752024596573E+03 0.17461694781336E+04 0.11616883841641E+04
 0.11287617701871E+04 0.10775242534816E+04 0.17462729190581E+04 0.10513817831989E+04 0.11284440850552E+04
 0.98752024596573E+03 0.17461694781336E+04 0.17342929777281E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48934241361403E+03 0.12948487335869E+01
 0.12948487335869E+01 0.21407286247270E+01 0.60619515847149E-03 0.32771721187138E+03 0.33902105214192E+03
 0.33901648391976E+03 0.33901632641547E+03 0.23000000000000E+00 0.00000000000000E+00 0.17279946332195E+00
 0.00000000000000E+00 -.11846673765587E+02 0.28451733258992E+00 0.10666307916810E+01 0.28117794888547E+02
 0.10544173083205E+02 0.75002522544768E+01 0.28125945954288E+01 0.32774681186785E+03 0.40495169445290E+03
 0.30971264620019E+03 0.31502888219563E+03 0.30615000003161E+03 0.30615000049639E+03 0.30970662591705E+03
 0.31502653170580E+03 0.30615000003122E+03 0.30615000049655E+03 0.30971264620019E+03 0.31502888219563E+03
 0.30615000003161E+03 0.30615000049639E+03 0.30970662591705E+03 0.31502653170580E+03 0.30615000003122E+03
 0.30615000049655E+03 0.31367877074166E+03 0.30615000390340E+03 0.57915479203770E+02 0.57899522932219E+02
 0.16097792574218E+03 0.37167844466172E+03 0.20989562929084E+03 0.15583812016123E+03 0.14962066747432E+03
 0.15583812016123E+03 0.29739831927042E+03 0.15585719480843E+03 0.14946861269693E+03 0.15585719480843E+03
 0.29726368109770E+03 0.15583812016123E+03 0.14962066747432E+03 0.15583812016123E+03 0.29739831927041E+03
 0.15585719480843E+03 0.14946861269693E+03 0.15585719480843E+03 0.29726368109770E+03 0.19228489625588E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36279443952568E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16780493275186E+00 0.00000000000000E+00 0.00000000000000E+00 0.16780493275186E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17657695980750E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17657695980750E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158842963998E+00 0.17815942142371E+00 0.33902105214192E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    594.23226683
 0.89385455218435E-01 0.32311220730411E+03 0.44951254471183E+03 0.44574642747885E+03 0.44433350158170E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15006587143771E+00 0.00000000000000E+00 -.19598139734343E+02
 0.37679636145988E-02 0.11269084487764E+01 0.21231627526880E+04 0.79618603225801E+03 0.70990682594370E+01
 0.26621505972889E+01 0.34659281869297E+03 0.30615003512895E+03 0.34048992329210E+03 0.36419759864592E+03
 0.30615000325432E+03 0.30615000596097E+03 0.33708879385939E+03 0.36417389731705E+03 0.30615000262461E+03
 0.30615000593977E+03 0.34048992329210E+03 0.36419759864592E+03 0.30615000325432E+03 0.30615000596097E+03
 0.33708879385939E+03 0.36417389731705E+03 0.30615000262461E+03 0.30615000593977E+03 0.40588958893784E+03
 0.32863758729178E+03 0.19754771217012E+04 0.18619865881935E+04 0.63680304255252E+03 0.10480973994084E+04
 0.40811034164315E+03 0.11657658221613E+04 0.11317107498285E+04 0.10801082616498E+04 0.17462711316970E+04
 0.10558024382101E+04 0.11314037437376E+04 0.99070158794095E+03 0.17461749630616E+04 0.11657658221613E+04
 0.11317107498285E+04 0.10801082616499E+04 0.17462711316970E+04 0.10558024382101E+04 0.11314037437376E+04
 0.99070158794095E+03 0.17461749630616E+04 0.17336120564803E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48986005845146E+03 0.12948488872972E+01
 0.12948488872972E+01 0.21969462702720E+01 0.44702405182839E-03 0.32862256179406E+03 0.33922433942803E+03
 0.33922117992830E+03 0.33922107803022E+03 0.23000000000000E+00 0.00000000000000E+00 0.17137106651508E+00
 0.00000000000000E+00 -.11839262685040E+02 0.38582492575607E+00 0.10928524895869E+01 0.20734793078293E+02
 0.77755474043600E+01 0.73202926069411E+01 0.27451097276029E+01 0.32863731026494E+03 0.40589308086259E+03
 0.30984566368483E+03 0.31519952251561E+03 0.30615000004400E+03 0.30615000066335E+03 0.30983983782255E+03
 0.31519706078411E+03 0.30615000004346E+03 0.30615000066356E+03 0.30984566368483E+03 0.31519952251561E+03
 0.30615000004400E+03 0.30615000066335E+03 0.30983983782255E+03 0.31519706078411E+03 0.30615000004346E+03
 0.30615000066356E+03 0.31382474993078E+03 0.30615000505874E+03 0.54214418941725E+02 0.54207745314597E+02
 0.16283311135803E+03 0.37409157023306E+03 0.21044429331824E+03 0.15943280880713E+03 0.15129259143972E+03
 0.15943280880713E+03 0.29928590416360E+03 0.15945453151597E+03 0.15113896549262E+03 0.15945453151597E+03
 0.29915052085561E+03 0.15943280880713E+03 0.15129259143972E+03 0.15943280880713E+03 0.29928590416360E+03
 0.15945453151597E+03 0.15113896549262E+03 0.15945453151597E+03 0.29915052085562E+03 0.19254886306250E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36305738007660E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16776571060322E+00 0.00000000000000E+00 0.00000000000000E+00 0.16776571060322E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17643756020785E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17643756020785E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158824656683E+00 0.17805247872008E+00 0.33922433942803E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    600.73679321
 0.89329240600360E-01 0.32326445184703E+03 0.44975485928406E+03 0.44598842860420E+03 0.44457501055743E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14969281028021E+00 0.00000000000000E+00 -.19592823601427E+02
 0.37703345424093E-02 0.11315621330556E+01 0.21218276282953E+04 0.79568536061072E+03 0.70698724942283E+01
 0.26512021853356E+01 0.34689004204495E+03 0.30615003920546E+03 0.34074649860288E+03 0.36457092370021E+03
 0.30615000368245E+03 0.30615000674906E+03 0.33733234121703E+03 0.36454738259854E+03 0.30615000297282E+03
 0.30615000672529E+03 0.34074649860288E+03 0.36457092370021E+03 0.30615000368245E+03 0.30615000674906E+03
 0.33733234121703E+03 0.36454738259854E+03 0.30615000297282E+03 0.30615000672529E+03 0.40630818716293E+03
 0.32904210726892E+03 0.19780674743989E+04 0.18636877727192E+04 0.63615042197974E+03 0.10453964363222E+04
 0.40606526223257E+03 0.11676398715719E+04 0.11330809621014E+04 0.10813378388094E+04 0.17463157322962E+04
 0.10578312843055E+04 0.11327786418497E+04 0.99219716764978E+03 0.17462226717778E+04 0.11676398715719E+04
 0.11330809621014E+04 0.10813378388094E+04 0.17463157322962E+04 0.10578312843055E+04 0.11327786418497E+04
 0.99219716764978E+03 0.17462226717778E+04 0.17334305565401E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49010129834573E+03 0.12948488481279E+01
 0.12948488481279E+01 0.22229643757652E+01 0.38824955200711E-03 0.32903381962837E+03 0.33932043416289E+03
 0.33931777164723E+03 0.33931768843098E+03 0.23000000000000E+00 0.00000000000000E+00 0.17072300452895E+00
 0.00000000000000E+00 -.11820221979107E+02 0.44423236805931E+00 0.11047243466838E+01 0.18008593194028E+02
 0.67532224477606E+01 0.72416255005284E+01 0.27156095626982E+01 0.32904191517212E+03 0.40631139160463E+03
 0.30991026299376E+03 0.31527977701094E+03 0.30615000005078E+03 0.30615000075247E+03 0.30990452434906E+03
 0.31527726456173E+03 0.30615000005016E+03 0.30615000075271E+03 0.30991026299376E+03 0.31527977701094E+03
 0.30615000005078E+03 0.30615000075247E+03 0.30990452434906E+03 0.31527726456173E+03 0.30615000005016E+03
 0.30615000075271E+03 0.31389349213096E+03 0.30615000566487E+03 0.52526491534867E+02 0.52523339728516E+02
 0.16366886233951E+03 0.37519113526501E+03 0.21070392861380E+03 0.16107279860756E+03 0.15204388701840E+03
 0.16107279860756E+03 0.30014070116955E+03 0.16109548694560E+03 0.15188963527171E+03 0.16109548694560E+03
 0.30000506867215E+03 0.16107279860756E+03 0.15204388701840E+03 0.16107279860756E+03 0.30014070116955E+03
 0.16109548694560E+03 0.15188963527171E+03 0.16109548694560E+03 0.30000506867216E+03 0.19266661475689E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36317932753013E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16761853272628E+00 0.00000000000000E+00 0.00000000000000E+00 0.16761853272628E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17634996271499E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17634996271499E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158860654089E+00 0.17800246157866E+00 0.33932043416289E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    611.32895292
 0.89249997463978E-01 0.32350056571018E+03 0.45014657599215E+03 0.44637885729332E+03 0.44496420497614E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14909987362887E+00 0.00000000000000E+00 -.19604842429149E+02
 0.37736817549250E-02 0.11389314849523E+01 0.21199455914795E+04 0.79497959680482E+03 0.70241275315480E+01
 0.26340478243305E+01 0.34736929899924E+03 0.30615004676596E+03 0.34116012134457E+03 0.36517505184581E+03
 0.30615000449225E+03 0.30615000824053E+03 0.33772459209079E+03 0.36515176092829E+03 0.30615000363243E+03
 0.30615000821197E+03 0.34116012134457E+03 0.36517505184581E+03 0.30615000449225E+03 0.30615000824053E+03
 0.33772459209079E+03 0.36515176092829E+03 0.30615000363243E+03 0.30615000821197E+03 0.40699733426423E+03
 0.32970598970200E+03 0.19822259573199E+04 0.18663476062949E+04 0.63487044972537E+03 0.10406715577327E+04
 0.40262675575872E+03 0.11706572040101E+04 0.11352525575429E+04 0.10832574546393E+04 0.17463416795279E+04
 0.10611019430457E+04 0.11349575025477E+04 0.99455470816975E+03 0.17462534163706E+04 0.11706572040101E+04
 0.11352525575429E+04 0.10832574546393E+04 0.17463416795279E+04 0.10611019430458E+04 0.11349575025477E+04
 0.99455470816975E+03 0.17462534163706E+04 0.17329996634548E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49049022607134E+03 0.12948489366828E+01
 0.12948489366828E+01 0.22653330146125E+01 0.30858614948892E-03 0.32971247185539E+03 0.33947965501701E+03
 0.33947764567205E+03 0.33947758616116E+03 0.23000000000000E+00 0.00000000000000E+00 0.16968527205371E+00
 0.00000000000000E+00 -.11811124909647E+02 0.55891365190233E+00 0.11236999770380E+01 0.14313481112460E+02
 0.53675554171724E+01 0.71193380470537E+01 0.26697517676452E+01 0.32970593328208E+03 0.40700010574125E+03
 0.31001327403366E+03 0.31540877268662E+03 0.30615000006399E+03 0.30615000092166E+03 0.31000766808530E+03
 0.31540618012636E+03 0.30615000006320E+03 0.30615000092195E+03 0.31001327403366E+03 0.31540877268662E+03
 0.30615000006399E+03 0.30615000092166E+03 0.31000766808530E+03 0.31540618012636E+03 0.30615000006320E+03
 0.30615000092195E+03 0.31400394640744E+03 0.30615000679413E+03 0.49700466046560E+02 0.49701671875351E+02
 0.16502037961861E+03 0.37701589055947E+03 0.21117040904276E+03 0.16375139534955E+03 0.15325999370364E+03
 0.16375139534955E+03 0.30156072345754E+03 0.16377538308974E+03 0.15310478452868E+03 0.16377538308974E+03
 0.30142472958183E+03 0.16375139534955E+03 0.15325999370364E+03 0.16375139534955E+03 0.30156072345754E+03
 0.16377538308974E+03 0.15310478452869E+03 0.16377538308974E+03 0.30142472958184E+03 0.19285561149600E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36338163954046E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16755812559812E+00 0.00000000000000E+00 0.00000000000000E+00 0.16755812559812E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17624834777588E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17624834777588E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158855768238E+00 0.17791894117702E+00 0.33947965501701E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    620.40794696
 0.89180220555513E-01 0.32370188366319E+03 0.45048068491766E+03 0.44671196443178E+03 0.44529629351204E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14860569316362E+00 0.00000000000000E+00 -.19612569363058E+02
 0.37766340520538E-02 0.11450443263535E+01 0.21182883725918E+04 0.79435813972194E+03 0.69866290901393E+01
 0.26199859088022E+01 0.34777704434063E+03 0.30615005420137E+03 0.34151220749397E+03 0.36568842458891E+03
 0.30615000530536E+03 0.30615000973887E+03 0.33805867592406E+03 0.36566534236619E+03 0.30615000429577E+03
 0.30615000970559E+03 0.34151220749397E+03 0.36568842458891E+03 0.30615000530536E+03 0.30615000973887E+03
 0.33805867592406E+03 0.36566534236619E+03 0.30615000429577E+03 0.30615000970559E+03 0.40758184690228E+03
 0.33027351199482E+03 0.19857449602668E+04 0.18685917561518E+04 0.63376672399088E+03 0.10366720765253E+04
 0.39973651891449E+03 0.11732158500595E+04 0.11370817783547E+04 0.10848811917131E+04 0.17463604445496E+04
 0.10638760348953E+04 0.11367926330665E+04 0.99655006109170E+03 0.17462760334042E+04 0.11732158500595E+04
 0.11370817783547E+04 0.10848811917131E+04 0.17463604445496E+04 0.10638760348953E+04 0.11367926330665E+04
 0.99655006109170E+03 0.17462760334042E+04 0.17326315658127E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49082207515313E+03 0.12948489936149E+01
 0.12948489936149E+01 0.23016489907673E+01 0.25343466684724E-03 0.33029092747141E+03 0.33961843201620E+03
 0.33961685607419E+03 0.33961681157690E+03 0.23000000000000E+00 0.00000000000000E+00 0.16881313274022E+00
 0.00000000000000E+00 -.11800514058412E+02 0.68054228152490E+00 0.11396147442811E+01 0.11755331325005E+02
 0.44082492468768E+01 0.70199161954917E+01 0.26324685733094E+01 0.33027355465969E+03 0.40758429578238E+03
 0.31010206790326E+03 0.31551911352685E+03 0.30615000007768E+03 0.30615000109219E+03 0.31009656920439E+03
 0.31551645406129E+03 0.30615000007672E+03 0.30615000109254E+03 0.31010206790326E+03 0.31551911352685E+03
 0.30615000007768E+03 0.30615000109219E+03 0.31009656920439E+03 0.31551645406129E+03 0.30615000007672E+03
 0.30615000109254E+03 0.31409846211082E+03 0.30615000790998E+03 0.47250561819905E+02 0.47254649930771E+02
 0.16616166585340E+03 0.37858510981258E+03 0.21159263562991E+03 0.16603999214798E+03 0.15428634759991E+03
 0.16603999214798E+03 0.30277931597317E+03 0.16606485641105E+03 0.15413039014382E+03 0.16606485641105E+03
 0.30264307209377E+03 0.16603999214798E+03 0.15428634759991E+03 0.16603999214798E+03 0.30277931597317E+03
 0.16606485641105E+03 0.15413039014382E+03 0.16606485641105E+03 0.30264307209378E+03 0.19301206023426E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36355572940949E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16748507843793E+00 0.00000000000000E+00 0.00000000000000E+00 0.16748507843793E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17614794575108E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17614794575108E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158859223252E+00 0.17784629327384E+00 0.33961843201620E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    631.00010667
 0.89102431969281E-01 0.32393478134670E+03 0.45086861761555E+03 0.44709857977863E+03 0.44568165774074E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14804505047022E+00 0.00000000000000E+00 -.19621969345237E+02
 0.37799307640110E-02 0.11519456652697E+01 0.21164408819782E+04 0.79366533074183E+03 0.69447719985362E+01
 0.26042894994511E+01 0.34824929020843E+03 0.30615006412370E+03 0.34192020231931E+03 0.36628233820567E+03
 0.30615000641287E+03 0.30615001178065E+03 0.33844602122190E+03 0.36625949301566E+03 0.30615000520069E+03
 0.30615001174103E+03 0.34192020231931E+03 0.36628233820567E+03 0.30615000641287E+03 0.30615001178065E+03
 0.33844602122190E+03 0.36625949301566E+03 0.30615000520069E+03 0.30615001174103E+03 0.40825694384089E+03
 0.33093368187446E+03 0.19897971271633E+04 0.18711625394685E+04 0.63247063147111E+03 0.10320600085165E+04
 0.39642702388800E+03 0.11761691462558E+04 0.11391806016012E+04 0.10867468546622E+04 0.17463795317536E+04
 0.10670786087600E+04 0.11388979991745E+04 0.99884492115488E+03 0.17462993274155E+04 0.11761691462559E+04
 0.11391806016012E+04 0.10867468546623E+04 0.17463795317536E+04 0.10670786087600E+04 0.11388979991745E+04
 0.99884492115488E+03 0.17462993274155E+04 0.17322008960084E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49120718376412E+03 0.12948490628741E+01
 0.12948490628741E+01 0.23440176296145E+01 0.20140490493548E-03 0.33095925095510E+03 0.33978321317339E+03
 0.33978202838055E+03 0.33978199679622E+03 0.23000000000000E+00 0.00000000000000E+00 0.16781569253947E+00
 0.00000000000000E+00 -.11788741936089E+02 0.85634955176388E+00 0.11577766569446E+01 0.93419795497316E+01
 0.35032423311493E+01 0.69097955568666E+01 0.25911733338250E+01 0.33093384026622E+03 0.40825903132516E+03
 0.31020622028080E+03 0.31564760380100E+03 0.30615000009692E+03 0.30615000132535E+03 0.31020083974786E+03
 0.31564486823191E+03 0.30615000009572E+03 0.30615000132578E+03 0.31020622028080E+03 0.31564760380100E+03
 0.30615000009692E+03 0.30615000132535E+03 0.31020083974786E+03 0.31564486823191E+03 0.30615000009572E+03
 0.30615000132578E+03 0.31420856199443E+03 0.30615000940594E+03 0.44365335228422E+02 0.44371906821370E+02
 0.16747725493231E+03 0.38042915080561E+03 0.21211450959863E+03 0.16870332427145E+03 0.15546900047087E+03
 0.16870332427145E+03 0.30420914887362E+03 0.16872898004066E+03 0.15531224523370E+03 0.16872898004066E+03
 0.30407267477130E+03 0.16870332427145E+03 0.15546900047087E+03 0.16870332427145E+03 0.30420914887362E+03
 0.16872898004066E+03 0.15531224523370E+03 0.16872898004066E+03 0.30407267477130E+03 0.19319316590504E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36376094299260E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16740426331915E+00 0.00000000000000E+00 0.00000000000000E+00 0.16740426331915E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17603493354086E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17603493354086E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158860960778E+00 0.17776008365498E+00 0.33978321317339E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    640.84448725
 0.89028511953646E-01 0.32415148175239E+03 0.45122768914980E+03 0.44745655393337E+03 0.44603853615721E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14753891711124E+00 0.00000000000000E+00 -.19630248843908E+02
 0.37830688723881E-02 0.11581431595822E+01 0.21146852647570E+04 0.79300697428387E+03 0.69076089029323E+01
 0.25903533385996E+01 0.34868516838673E+03 0.30615007465468E+03 0.34229701609622E+03 0.36682956474793E+03
 0.30615000761232E+03 0.30615001399279E+03 0.33880402019220E+03 0.36680693426714E+03 0.30615000618224E+03
 0.30615001394642E+03 0.34229701609622E+03 0.36682956474793E+03 0.30615000761232E+03 0.30615001399279E+03
 0.33880402019220E+03 0.36680693426714E+03 0.30615000618224E+03 0.30615001394642E+03 0.40887684289768E+03
 0.33154452287290E+03 0.19935202484588E+04 0.18735271107317E+04 0.63127981459884E+03 0.10278613948250E+04
 0.39342518115319E+03 0.11788875640849E+04 0.11411043743398E+04 0.10884682506740E+04 0.17464033971320E+04
 0.10700265998330E+04 0.11408275343176E+04 0.10009602168713E+04 0.17463268406199E+04 0.11788875640849E+04
 0.11411043743398E+04 0.10884682506740E+04 0.17464033971320E+04 0.10700265998330E+04 0.11408275343176E+04
 0.10009602168714E+04 0.17463268406199E+04 0.17318191970940E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49156380068838E+03 0.12948491238776E+01
 0.12948491238776E+01 0.23833951519313E+01 0.16267728758379E-03 0.33157479989238E+03 0.33993881034606E+03
 0.33993790325637E+03 0.33993788037748E+03 0.23000000000000E+00 0.00000000000000E+00 0.16690795292228E+00
 0.00000000000000E+00 -.11777352539044E+02 0.10602155756497E+01 0.11742680113643E+01 0.75456352309272E+01
 0.28296132115977E+01 0.68127547736782E+01 0.25547830401293E+01 0.33154479186312E+03 0.40887860361552E+03
 0.31030382839674E+03 0.31576696809161E+03 0.30615000011840E+03 0.30615000157882E+03 0.31029855168776E+03
 0.31576416356597E+03 0.30615000011693E+03 0.30615000157933E+03 0.31030382839674E+03 0.31576696809161E+03
 0.30615000011840E+03 0.30615000157882E+03 0.31029855168776E+03 0.31576416356597E+03 0.30615000011693E+03
 0.30615000157933E+03 0.31431088994555E+03 0.30615001100086E+03 0.41664349951451E+02 0.41672435042239E+02
 0.16868363197677E+03 0.38214966387551E+03 0.21262261373885E+03 0.17116836342739E+03 0.15655283417963E+03
 0.17116836342739E+03 0.30554063962017E+03 0.17119456684721E+03 0.15639541040881E+03 0.17119456684721E+03
 0.30540401188814E+03 0.17116836342739E+03 0.15655283417963E+03 0.17116836342739E+03 0.30554063962017E+03
 0.17119456684721E+03 0.15639541040881E+03 0.17119456684721E+03 0.30540401188814E+03 0.19335873055410E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36395315866753E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16732597266250E+00 0.00000000000000E+00 0.00000000000000E+00 0.16732597266250E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17592704477203E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17592704477203E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158863403489E+00 0.17767876438562E+00 0.33993881034606E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    650.08712630
 0.89085008418515E-01 0.32433623511130E+03 0.45156210659547E+03 0.44778413398473E+03 0.44636273018746E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14707739975520E+00 0.00000000000000E+00 -.19681126134361E+02
 0.37806693905287E-02 0.11637672634876E+01 0.21160273945248E+04 0.79351027294679E+03 0.68742267040797E+01
 0.25778350140299E+01 0.34909125614742E+03 0.30615008580873E+03 0.34264824234454E+03 0.36733935890768E+03
 0.30615000890628E+03 0.30615001638001E+03 0.33913784687946E+03 0.36731692485915E+03 0.30615000724262E+03
 0.30615001632645E+03 0.34264824234454E+03 0.36733935890768E+03 0.30615000890628E+03 0.30615001638001E+03
 0.33913784687946E+03 0.36731692485915E+03 0.30615000724262E+03 0.30615001632645E+03 0.40945357722421E+03
 0.33221128020103E+03 0.19968765552942E+04 0.18755000029663E+04 0.63012920596669E+03 0.10239164171582E+04
 0.39063656516171E+03 0.11813733914089E+04 0.11428589326260E+04 0.10899393349238E+04 0.17463929415939E+04
 0.10727222108764E+04 0.11425871859236E+04 0.10027941912183E+04 0.17463195448448E+04 0.11813733914090E+04
 0.11428589326260E+04 0.10899393349238E+04 0.17463929415939E+04 0.10727222108764E+04 0.11425871859236E+04
 0.10027941912183E+04 0.17463195448448E+04 0.17313615125663E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49188854969235E+03 0.12948494987425E+01
 0.12948494987425E+01 0.24203657081492E+01 0.00000000000000E+00 0.34006011139435E+03 0.34006011139435E+03
 0.34006011139435E+03 0.34006011139435E+03 0.00000000000000E+00 0.00000000000000E+00 0.16607654790126E+00
 0.00000000000000E+00 -.11817326719612E+02 0.10000000000000E-02 0.11895360070827E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67253113418733E+01 0.25219917532025E+01 0.33221125536302E+03 0.40945504307123E+03
 0.31587770718766E+03 0.31587770718766E+03 0.30615000187668E+03 0.30615000185320E+03 0.31587494018894E+03
 0.31587494018894E+03 0.30615000187728E+03 0.30615000185379E+03 0.31587770718766E+03 0.31587770718766E+03
 0.30615000187668E+03 0.30615000185320E+03 0.31587494018894E+03 0.31587494018894E+03 0.30615000187728E+03
 0.30615000185379E+03 0.31440474821855E+03 0.30615001269703E+03 0.38863754226405E+02 0.47305961501999E+02
 0.16920977024596E+03 0.38296115989509E+03 0.21290534079790E+03 0.13488524016990E+03 0.15735738863152E+03
 0.13488524016990E+03 0.30642624655186E+03 0.13489977662462E+03 0.15722205493020E+03 0.13489977662462E+03
 0.30631143213107E+03 0.13488524016990E+03 0.15735738863152E+03 0.13488524016990E+03 0.30642624655186E+03
 0.13489977662462E+03 0.15722205493020E+03 0.13489977662462E+03 0.30631143213108E+03 0.19517820537037E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36412920903408E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16762764039891E+00 0.00000000000000E+00 0.00000000000000E+00 0.16762764039891E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17610449647553E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17610449647553E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158725003951E+00 0.17761388282215E+00 0.34006011139435E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    660.21744942
 0.89057258741370E-01 0.32455686553732E+03 0.45192563614985E+03 0.44814459829651E+03 0.44672140014928E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14658660304757E+00 0.00000000000000E+00 -.19670712363835E+02
 0.37818470719777E-02 0.11697193873960E+01 0.21153684556093E+04 0.79326317085349E+03 0.68392471615005E+01
 0.25647176855627E+01 0.34953310499048E+03 0.30615009967018E+03 0.34303066062886E+03 0.36789347853497E+03
 0.30615001054560E+03 0.30615001940523E+03 0.33950145819888E+03 0.36787125385428E+03 0.30615000858801E+03
 0.30615001934273E+03 0.34303066062886E+03 0.36789347853497E+03 0.30615001054560E+03 0.30615001940523E+03
 0.33950145819888E+03 0.36787125385428E+03 0.30615000858801E+03 0.30615001934273E+03 0.41008007114740E+03
 0.33286680416665E+03 0.20005487705437E+04 0.18777959598471E+04 0.62883528001323E+03 0.10195995706694E+04
 0.38762011425608E+03 0.11840818205040E+04 0.11447324044833E+04 0.10916464907977E+04 0.17463489941598E+04
 0.10756623251058E+04 0.11444659749997E+04 0.10048959050955E+04 0.17462788530884E+04 0.11840818205041E+04
 0.11447324044833E+04 0.10916464907978E+04 0.17463489941598E+04 0.10756623251058E+04 0.11444659749997E+04
 0.10048959050955E+04 0.17462788530884E+04 0.17308887260918E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49224716528823E+03 0.12948494220136E+01
 0.12948494220136E+01 0.24608870006305E+01 0.00000000000000E+00 0.34020630988371E+03 0.34020630988371E+03
 0.34020630988371E+03 0.34020630988371E+03 0.00000000000000E+00 0.00000000000000E+00 0.16517707270952E+00
 0.00000000000000E+00 -.11784978288407E+02 0.10000000000000E-02 0.12058625257074E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66342554225301E+01 0.24878457834488E+01 0.33286689948207E+03 0.41008124504827E+03
 0.31599835992853E+03 0.31599835992853E+03 0.30615000222995E+03 0.30615000220204E+03 0.31599556338697E+03
 0.31599556338697E+03 0.30615000223065E+03 0.30615000220274E+03 0.31599835992853E+03 0.31599835992853E+03
 0.30615000222995E+03 0.30615000220204E+03 0.31599556338697E+03 0.31599556338697E+03 0.30615000223065E+03
 0.30615000220274E+03 0.31450787381306E+03 0.30615001481387E+03 0.35659187583466E+02 0.43420569040854E+02
 0.17033671338677E+03 0.38449704230781E+03 0.21330864535410E+03 0.13726726398488E+03 0.15834330084140E+03
 0.13726726398488E+03 0.30757733827263E+03 0.13728195346265E+03 0.15820623123871E+03 0.13728195346265E+03
 0.30746100824502E+03 0.13726726398488E+03 0.15834330084140E+03 0.13726726398488E+03 0.30757733827263E+03
 0.13728195346265E+03 0.15820623123871E+03 0.13728195346265E+03 0.30746100824502E+03 0.19495946860215E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36430810066654E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16738871420381E+00 0.00000000000000E+00 0.00000000000000E+00 0.16738871420381E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17590340690005E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17590340690005E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158789527956E+00 0.17753828027755E+00 0.34020630988371E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    670.94843933
 0.88978629678152E-01 0.32478842033701E+03 0.45230855088584E+03 0.44852636206164E+03 0.44710200989114E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14608212468978E+00 0.00000000000000E+00 -.19674912479854E+02
 0.37851886578861E-02 0.11758033107405E+01 0.21135009963989E+04 0.79256287364958E+03 0.68038590527204E+01
 0.25514471447701E+01 0.34999713572042E+03 0.30615011657400E+03 0.34343240326301E+03 0.36847538675805E+03
 0.30615001258898E+03 0.30615002317714E+03 0.33988347621716E+03 0.36845337649651E+03 0.30615001026784E+03
 0.30615002310368E+03 0.34343240326301E+03 0.36847538675805E+03 0.30615001258898E+03 0.30615002317714E+03
 0.33988347621716E+03 0.36845337649651E+03 0.30615001026784E+03 0.30615002310368E+03 0.41074091152548E+03
 0.33354662646743E+03 0.20044272715721E+04 0.18802251963209E+04 0.62740235062287E+03 0.10149804470163E+04
 0.38444108464027E+03 0.11869364354769E+04 0.11466846680771E+04 0.10934348797013E+04 0.17462987153631E+04
 0.10787631297102E+04 0.11464235813884E+04 0.10071039309146E+04 0.17462318047734E+04 0.11869364354769E+04
 0.11466846680771E+04 0.10934348797013E+04 0.17462987153631E+04 0.10787631297102E+04 0.11464235813884E+04
 0.10071039309146E+04 0.17462318047734E+04 0.17303802867118E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49262748631805E+03 0.12948494529601E+01
 0.12948494529601E+01 0.25038109602783E+01 0.00000000000000E+00 0.34037163280369E+03 0.34037163280369E+03
 0.34037163280369E+03 0.34037163280369E+03 0.00000000000000E+00 0.00000000000000E+00 0.16424596078757E+00
 0.00000000000000E+00 -.11767559837281E+02 0.10000000000000E-02 0.12226962254347E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65429170660570E+01 0.24535938997714E+01 0.33354701536845E+03 0.41074180105488E+03
 0.31612549127074E+03 0.31612549127074E+03 0.30615000270504E+03 0.30615000263865E+03 0.31612265232504E+03
 0.31612265232504E+03 0.30615000270589E+03 0.30615000263948E+03 0.31612549127074E+03 0.31612549127074E+03
 0.30615000270504E+03 0.30615000263865E+03 0.31612265232504E+03 0.31612265232504E+03 0.30615000270589E+03
 0.30615000263948E+03 0.31461668362379E+03 0.30615001740763E+03 0.32385522766260E+02 0.39470962614270E+02
 0.17156720956388E+03 0.38627047021763E+03 0.21384542460593E+03 0.13978311927555E+03 0.15942914977716E+03
 0.13978311927555E+03 0.30892302138056E+03 0.13979804304780E+03 0.15929052573048E+03 0.13979804304780E+03
 0.30880545512505E+03 0.13978311927555E+03 0.15942914977716E+03 0.13978311927555E+03 0.30892302138056E+03
 0.13979804304780E+03 0.15929052573048E+03 0.13979804304780E+03 0.30880545512505E+03 0.19484074234316E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36451001548423E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16726615403774E+00 0.00000000000000E+00 0.00000000000000E+00 0.16726615403774E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17575824710564E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17575824710564E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158807447573E+00 0.17745226346981E+00 0.34037163280369E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    680.24428142
 0.88881915154692E-01 0.32499481331247E+03 0.45264056954394E+03 0.44885876978554E+03 0.44743400700728E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14565709590381E+00 0.00000000000000E+00 -.19665650143330E+02
 0.37893070829690E-02 0.11808967568832E+01 0.21112039285377E+04 0.79170147320164E+03 0.67745126348851E+01
 0.25404422380819E+01 0.35039753824165E+03 0.30615013294222E+03 0.34377940043183E+03 0.36897516295205E+03
 0.30615001460223E+03 0.30615002689402E+03 0.34021394686619E+03 0.36895333547720E+03 0.30615001192513E+03
 0.30615002680993E+03 0.34377940043183E+03 0.36897516295205E+03 0.30615001460223E+03 0.30615002689402E+03
 0.34021394686619E+03 0.36895333547720E+03 0.30615001192513E+03 0.30615002680993E+03 0.41130073372932E+03
 0.33412160341791E+03 0.20077906054520E+04 0.18823803044936E+04 0.62629454757655E+03 0.10112614626924E+04
 0.38183544237800E+03 0.11894066192626E+04 0.11483876440725E+04 0.10950170468577E+04 0.17463094040007E+04
 0.10814439052218E+04 0.11481309762544E+04 0.10090425862426E+04 0.17462451148482E+04 0.11894066192626E+04
 0.11483876440725E+04 0.10950170468578E+04 0.17463094040007E+04 0.10814439052218E+04 0.11481309762544E+04
 0.10090425862426E+04 0.17462451148482E+04 0.17300557458771E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49295902677121E+03 0.12948493847151E+01
 0.12948493847151E+01 0.25409943286222E+01 0.00000000000000E+00 0.34052156721709E+03 0.34052156721709E+03
 0.34052156721709E+03 0.34052156721709E+03 0.00000000000000E+00 0.00000000000000E+00 0.16345704079374E+00
 0.00000000000000E+00 -.11738130453147E+02 0.10000000000000E-02 0.12369093254979E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64677335962194E+01 0.24254000985823E+01 0.33412220314023E+03 0.41130136133170E+03
 0.31623657393096E+03 0.31623657393096E+03 0.30615000309943E+03 0.30615000307020E+03 0.31623369412168E+03
 0.31623369412168E+03 0.30615000310041E+03 0.30615000307116E+03 0.31623657393096E+03 0.31623657393096E+03
 0.30615000309943E+03 0.30615000307020E+03 0.31623369412168E+03 0.31623369412168E+03 0.30615000310041E+03
 0.30615000307116E+03 0.31471189871248E+03 0.30615001992861E+03 0.29658262763440E+02 0.36194940970272E+02
 0.17264488803323E+03 0.38786957454271E+03 0.21436146206932E+03 0.14193437219996E+03 0.16038468951994E+03
 0.14193437219996E+03 0.31014094684642E+03 0.14194952699386E+03 0.16024496829066E+03 0.14194952699386E+03
 0.31002259026611E+03 0.14193437219996E+03 0.16038468951994E+03 0.14193437219996E+03 0.31014094684642E+03
 0.14194952699386E+03 0.16024496829066E+03 0.14194952699386E+03 0.31002259026611E+03 0.19480356498740E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36468787620241E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16704809463486E+00 0.00000000000000E+00 0.00000000000000E+00 0.16704809463486E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17557948660255E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17557948660255E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158862907973E+00 0.17737475464266E+00 0.34052156721709E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    690.13505232
 0.88768969949471E-01 0.32520971392168E+03 0.45299308641429E+03 0.44921202029668E+03 0.44778687118130E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14521624638631E+00 0.00000000000000E+00 -.19671042604543E+02
 0.37941280586024E-02 0.11861505066635E+01 0.21085213457310E+04 0.79069550464912E+03 0.67445066667828E+01
 0.25291900000435E+01 0.35082084021648E+03 0.30615015238755E+03 0.34414634074968E+03 0.36950342671533E+03
 0.30615001703526E+03 0.30615003138650E+03 0.34056347518609E+03 0.36948178857409E+03 0.30615001393064E+03
 0.30615003128973E+03 0.34414634074968E+03 0.36950342671533E+03 0.30615001703526E+03 0.30615003138650E+03
 0.34056347518609E+03 0.36948178857409E+03 0.30615001393064E+03 0.30615003128973E+03 0.41189480907728E+03
 0.33472911328948E+03 0.20113575384912E+04 0.18846453831735E+04 0.62507689679296E+03 0.10072800703372E+04
 0.37907778906030E+03 0.11920259207542E+04 0.11501882878878E+04 0.10966713604324E+04 0.17463361789714E+04
 0.10842868164010E+04 0.11499360963674E+04 0.10110763101007E+04 0.17462745043773E+04 0.11920259207542E+04
 0.11501882878878E+04 0.10966713604324E+04 0.17463361789714E+04 0.10842868164010E+04 0.11499360963674E+04
 0.10110763101007E+04 0.17462745043773E+04 0.17297075821991E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49331140182197E+03 0.12948494244468E+01
 0.12948494244468E+01 0.25805574122139E+01 0.00000000000000E+00 0.34068609373113E+03 0.34068609373113E+03
 0.34068609373113E+03 0.34068609373113E+03 0.00000000000000E+00 0.00000000000000E+00 0.16263534317230E+00
 0.00000000000000E+00 -.11723526120599E+02 0.10000000000000E-02 0.12516673095971E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63914747462529E+01 0.23968030298448E+01 0.33472987618288E+03 0.41189517301555E+03
 0.31635455627185E+03 0.31635455627185E+03 0.30615000363486E+03 0.30615000359339E+03 0.31635163090869E+03
 0.31635163090869E+03 0.30615000363600E+03 0.30615000359451E+03 0.31635455627185E+03 0.31635455627185E+03
 0.30615000363486E+03 0.30615000359339E+03 0.31635163090869E+03 0.31635163090869E+03 0.30615000363600E+03
 0.30615000359451E+03 0.31481307083177E+03 0.30615002293451E+03 0.26779068370867E+02 0.32753678420018E+02
 0.17380369516377E+03 0.38963701571108E+03 0.21496430207149E+03 0.14422797860392E+03 0.16141467279091E+03
 0.14422797860392E+03 0.31149177387499E+03 0.14424339309015E+03 0.16127386669896E+03 0.14424339309015E+03
 0.31137267583744E+03 0.14422797860392E+03 0.16141467279091E+03 0.14422797860392E+03 0.31149177387499E+03
 0.14424339309015E+03 0.16127386669896E+03 0.14424339309015E+03 0.31137267583744E+03 0.19480700698945E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36488452689494E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16694753702508E+00 0.00000000000000E+00 0.00000000000000E+00 0.16694753702508E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17544746669583E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17544746669583E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158872953142E+00 0.17728922493783E+00 0.34068609373113E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    700.81853399
 0.88657095254048E-01 0.32543465647915E+03 0.45337163025131E+03 0.44959079009456E+03 0.44816493225836E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14475289871765E+00 0.00000000000000E+00 -.19682422884286E+02
 0.37989154227985E-02 0.11916424599723E+01 0.21058642032380E+04 0.78969907621423E+03 0.67134230851305E+01
 0.25175336569239E+01 0.35127430432363E+03 0.30615017623816E+03 0.34453948705960E+03 0.37006961575567E+03
 0.30615002007964E+03 0.30615003700833E+03 0.34093798445300E+03 0.37004817573205E+03 0.30615001644397E+03
 0.30615003689599E+03 0.34453948705960E+03 0.37006961575567E+03 0.30615002007964E+03 0.30615003700833E+03
 0.34093798445300E+03 0.37004817573205E+03 0.30615001644397E+03 0.30615003689599E+03 0.41253476626697E+03
 0.33538218524804E+03 0.20151663616446E+04 0.18870211614036E+04 0.62369225410722E+03 0.10029107733911E+04
 0.37610005801332E+03 0.11948296082068E+04 0.11521010784370E+04 0.10984065598309E+04 0.17463564990218E+04
 0.10873308766077E+04 0.11518534750833E+04 0.10132204036737E+04 0.17462974637471E+04 0.11948296082068E+04
 0.11521010784370E+04 0.10984065598309E+04 0.17463564990218E+04 0.10873308766077E+04 0.11518534750833E+04
 0.10132204036737E+04 0.17462974637471E+04 0.17292905535409E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49368904687255E+03 0.12948495082970E+01
 0.12948495082970E+01 0.26232913389101E+01 0.00000000000000E+00 0.34086883278357E+03 0.34086883278357E+03
 0.34086883278357E+03 0.34086883278357E+03 0.00000000000000E+00 0.00000000000000E+00 0.16176780368984E+00
 0.00000000000000E+00 -.11713955344758E+02 0.10000000000000E-02 0.12671979640779E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63131414560164E+01 0.23674280460062E+01 0.33538304905366E+03 0.41253489723510E+03
 0.31648156352563E+03 0.31648156352563E+03 0.30615000436063E+03 0.30615000425046E+03 0.31647858739314E+03
 0.31647858739314E+03 0.30615000436199E+03 0.30615000425178E+03 0.31648156352563E+03 0.31648156352563E+03
 0.30615000436063E+03 0.30615000425046E+03 0.31647858739314E+03 0.31647858739314E+03 0.30615000436199E+03
 0.30615000425178E+03 0.31492201230083E+03 0.30615002663701E+03 0.23678868351109E+02 0.29068340340269E+02
 0.17506862054651E+03 0.39161252111217E+03 0.21566855746293E+03 0.14671640376071E+03 0.16254143512639E+03
 0.14671640376071E+03 0.31300575567339E+03 0.14673210928858E+03 0.16239952319362E+03 0.14673210928858E+03
 0.31288593563316E+03 0.14671640376071E+03 0.16254143512639E+03 0.14671640376071E+03 0.31300575567339E+03
 0.14673210928858E+03 0.16239952319362E+03 0.14673210928858E+03 0.31288593563315E+03 0.19485075504577E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36510202078614E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16688830082931E+00 0.00000000000000E+00 0.00000000000000E+00 0.16688830082931E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17532493180148E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17532493180148E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158865032434E+00 0.17719411551345E+00 0.34086883278357E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    710.90929397
 0.88562552744972E-01 0.32564569650689E+03 0.45372712007126E+03 0.44994604746124E+03 0.44851936117928E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14432735516292E+00 0.00000000000000E+00 -.19684440138790E+02
 0.38029705006671E-02 0.11966577791320E+01 0.21036187366157E+04 0.78885702623087E+03 0.66852864198175E+01
 0.25069824074316E+01 0.35169978647039E+03 0.30615020168681E+03 0.34490857683477E+03 0.37060000586144E+03
 0.30615002339136E+03 0.30615004312409E+03 0.34128983980774E+03 0.37057874834402E+03 0.30615001918213E+03
 0.30615004299510E+03 0.34490857683476E+03 0.37060000586144E+03 0.30615002339136E+03 0.30615004312409E+03
 0.34128983980774E+03 0.37057874834402E+03 0.30615001918213E+03 0.30615004299510E+03 0.41313163602774E+03
 0.33599233795439E+03 0.20187153304257E+04 0.18892228531510E+04 0.62240548641360E+03 0.99887129287429E+03
 0.37335377902862E+03 0.11974498690002E+04 0.11538802816203E+04 0.11000229626490E+04 0.17463729915842E+04
 0.10901755214838E+04 0.11536368002899E+04 0.10152175262493E+04 0.17463162838534E+04 0.11974498690002E+04
 0.11538802816203E+04 0.11000229626490E+04 0.17463729915842E+04 0.10901755214838E+04 0.11536368002899E+04
 0.10152175262493E+04 0.17463162838534E+04 0.17289038533351E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49404313541818E+03 0.12948495231601E+01
 0.12948495231601E+01 0.26636543787993E+01 0.00000000000000E+00 0.34104547510909E+03 0.34104547510909E+03
 0.34104547510909E+03 0.34104547510909E+03 0.00000000000000E+00 0.00000000000000E+00 0.16096705260905E+00
 0.00000000000000E+00 -.11695683154480E+02 0.10000000000000E-02 0.12814869743938E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62427478077054E+01 0.23410304278895E+01 0.33599328069255E+03 0.41313155499886E+03
 0.31660190306205E+03 0.31660190306205E+03 0.30615000509655E+03 0.30615000496779E+03 0.31659887794783E+03
 0.31659887794783E+03 0.30615000509813E+03 0.30615000496933E+03 0.31660190306205E+03 0.31660190306205E+03
 0.30615000509655E+03 0.30615000496779E+03 0.31659887794783E+03 0.31659887794783E+03 0.30615000509813E+03
 0.30615000496933E+03 0.31502529150638E+03 0.30615003060364E+03 0.20786622281646E+02 0.25647937771601E+02
 0.17626728650680E+03 0.39351534074336E+03 0.21636671780402E+03 0.14905826912012E+03 0.16361078316041E+03
 0.14905826912012E+03 0.31446508071754E+03 0.14907425722842E+03 0.16346793994792E+03 0.14907425722842E+03
 0.31434470015997E+03 0.14905826912012E+03 0.16361078316041E+03 0.14905826912012E+03 0.31446508071754E+03
 0.14907425722842E+03 0.16346793994791E+03 0.14907425722842E+03 0.31434470015997E+03 0.19492426539194E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36530917626571E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16675841204861E+00 0.00000000000000E+00 0.00000000000000E+00 0.16675841204861E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17518368444245E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17518368444245E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158883339338E+00 0.17710255985649E+00 0.34104547510909E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    721.00005394
 0.88463995592756E-01 0.32585831136698E+03 0.45408125304042E+03 0.45030021512472E+03 0.44887283833420E+03
 0.22999992216431E+00 0.00000000000000E+00 0.14391318715158E+00 0.00000000000000E+00 -.19691049398213E+02
 0.38072070162342E-02 0.12015100946921E+01 0.21012779094721E+04 0.78797921605202E+03 0.66582877957844E+01
 0.24968579234191E+01 0.35212272263978E+03 0.30615023012650E+03 0.34527566663408E+03 0.37112635754034E+03
 0.30615002715852E+03 0.30615005008085E+03 0.34164001289994E+03 0.37110527817182E+03 0.30615002230117E+03
 0.30615004993321E+03 0.34527566663408E+03 0.37112635754034E+03 0.30615002715852E+03 0.30615005008085E+03
 0.34164001289994E+03 0.37110527817182E+03 0.30615002230117E+03 0.30615004993321E+03 0.41372240206453E+03
 0.33659704293517E+03 0.20222380906664E+04 0.18914234387584E+04 0.62113509139953E+03 0.99490591568054E+03
 0.37066514882401E+03 0.12000532148744E+04 0.11556440177695E+04 0.11016424200737E+04 0.17464008888599E+04
 0.10930014917153E+04 0.11554044625359E+04 0.10172131032752E+04 0.17463463563697E+04 0.12000532148744E+04
 0.11556440177695E+04 0.11016424200738E+04 0.17464008888599E+04 0.10930014917153E+04 0.11554044625359E+04
 0.10172131032752E+04 0.17463463563697E+04 0.17285417780716E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49439622135174E+03 0.12948495718573E+01
 0.12948495718573E+01 0.27040174186885E+01 0.00000000000000E+00 0.34122461473695E+03 0.34122461473695E+03
 0.34122461473695E+03 0.34122461473695E+03 0.00000000000000E+00 0.00000000000000E+00 0.16018420236368E+00
 0.00000000000000E+00 -.11682604893064E+02 0.10000000000000E-02 0.12954164450206E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61756202268009E+01 0.23158575850504E+01 0.33659807280595E+03 0.41372210313989E+03
 0.31672249532698E+03 0.31672249532698E+03 0.30615000593643E+03 0.30615000578644E+03 0.31671942081550E+03
 0.31671942081550E+03 0.30615000593826E+03 0.30615000578823E+03 0.31672249532698E+03 0.31672249532698E+03
 0.30615000593643E+03 0.30615000578644E+03 0.31671942081550E+03 0.31671942081550E+03 0.30615000593826E+03
 0.30615000578823E+03 0.31512884415904E+03 0.30615003505285E+03 0.17912289884990E+02 0.22266202005645E+02
 0.17746218360652E+03 0.39543734408634E+03 0.21708784956179E+03 0.15139085859927E+03 0.16467715156501E+03
 0.15139085859927E+03 0.31593909236769E+03 0.15140713299565E+03 0.16453345591941E+03 0.15140713299565E+03
 0.31581823401952E+03 0.15139085859927E+03 0.16467715156501E+03 0.15139085859927E+03 0.31593909236769E+03
 0.15140713299565E+03 0.16453345591941E+03 0.15140713299565E+03 0.31581823401952E+03 0.19501607577060E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36551947989536E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16666866556591E+00 0.00000000000000E+00 0.00000000000000E+00 0.16666866556591E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17506447500501E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17506447500501E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158886270410E+00 0.17700963581674E+00 0.34122461473695E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    731.09081391
 0.88371126464616E-01 0.32606934124128E+03 0.45443382253075E+03 0.45065258459422E+03 0.44922442616201E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14351002519613E+00 0.00000000000000E+00 -.19698344002490E+02
 0.38112076599027E-02 0.12062047269008E+01 0.20990721875817E+04 0.78715207034313E+03 0.66323732792482E+01
 0.24871399797181E+01 0.35254310383553E+03 0.30615026182614E+03 0.34564072974965E+03 0.37164882781326E+03
 0.30615003143107E+03 0.30615005797052E+03 0.34198845027912E+03 0.37162792229330E+03 0.30615002584347E+03
 0.30615005780207E+03 0.34564072974965E+03 0.37164882781325E+03 0.30615003143107E+03 0.30615005797052E+03
 0.34198845027912E+03 0.37162792229330E+03 0.30615002584347E+03 0.30615005780207E+03 0.41430779037397E+03
 0.33719699677764E+03 0.20257263310782E+04 0.18935931874884E+04 0.61986877045469E+03 0.99099153905221E+03
 0.36802342474525E+03 0.12026361355605E+04 0.11573883805012E+04 0.11032433940995E+04 0.17464333110735E+04
 0.10958051259632E+04 0.11571525613361E+04 0.10191862808087E+04 0.17463808066156E+04 0.12026361355605E+04
 0.11573883805012E+04 0.11032433940995E+04 0.17464333110735E+04 0.10958051259633E+04 0.11571525613361E+04
 0.10191862808087E+04 0.17463808066156E+04 0.17281859498924E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49474744977460E+03 0.12948496256041E+01
 0.12948496256041E+01 0.27443804585777E+01 0.00000000000000E+00 0.34140611444144E+03 0.34140611444144E+03
 0.34140611444144E+03 0.34140611444144E+03 0.00000000000000E+00 0.00000000000000E+00 0.15941884744180E+00
 0.00000000000000E+00 -.11670420199545E+02 0.10000000000000E-02 0.13089948508349E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61115595641169E+01 0.22918348365438E+01 0.33719811416007E+03 0.41430728459160E+03
 0.31684327903638E+03 0.31684327903638E+03 0.30615000689203E+03 0.30615000671791E+03 0.31684015497256E+03
 0.31684015497256E+03 0.30615000689415E+03 0.30615000671997E+03 0.31684327903638E+03 0.31684327903638E+03
 0.30615000689203E+03 0.30615000671791E+03 0.31684015497256E+03 0.31684015497256E+03 0.30615000689415E+03
 0.30615000671997E+03 0.31523260937898E+03 0.30615004002988E+03 0.15051352623520E+02 0.18917767567921E+02
 0.17865457379405E+03 0.39737773190405E+03 0.21782988524103E+03 0.15371687634316E+03 0.16574159133833E+03
 0.15371687634316E+03 0.31742699675383E+03 0.15373343897946E+03 0.16559710627509E+03 0.15373343897946E+03
 0.31730572542102E+03 0.15371687634316E+03 0.16574159133833E+03 0.15371687634316E+03 0.31742699675383E+03
 0.15373343897946E+03 0.16559710627509E+03 0.15373343897946E+03 0.31730572542102E+03 0.19512446633949E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36573179276021E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16658581122191E+00 0.00000000000000E+00 0.00000000000000E+00 0.16658581122191E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17494954508184E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17494954508184E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158886213678E+00 0.17691555393499E+00 0.34140611444144E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    741.18157388
 0.88284160497486E-01 0.32627896804842E+03 0.45478464579234E+03 0.45100297383274E+03 0.44957394771393E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14311768393231E+00 0.00000000000000E+00 -.19706028677702E+02
 0.38149616176093E-02 0.12107460061057E+01 0.20970066810301E+04 0.78637750538628E+03 0.66074965018730E+01
 0.24778111882024E+01 0.35296094524786E+03 0.30615029707078E+03 0.34600377469348E+03 0.37216749904533E+03
 0.30615003626298E+03 0.30615006689232E+03 0.34233514812535E+03 0.37214676317526E+03 0.30615002985488E+03
 0.30615006670070E+03 0.34600377469348E+03 0.37216749904533E+03 0.30615003626298E+03 0.30615006689232E+03
 0.34233514812535E+03 0.37214676317526E+03 0.30615002985488E+03 0.30615006670070E+03 0.41488803359991E+03
 0.33779250744484E+03 0.20291778732795E+04 0.18957311208534E+04 0.61860161855054E+03 0.98711859658641E+03
 0.36542396994311E+03 0.12051974451225E+04 0.11591105359113E+04 0.11048259494315E+04 0.17464651521967E+04
 0.10985853115164E+04 0.11588782718767E+04 0.10211371809531E+04 0.17464145364802E+04 0.12051974451226E+04
 0.11591105359113E+04 0.11048259494315E+04 0.17464651521967E+04 0.10985853115164E+04 0.11588782718767E+04
 0.10211371809531E+04 0.17464145364802E+04 0.17278308461317E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49509664165628E+03 0.12948496822250E+01
 0.12948496822250E+01 0.27847434984669E+01 0.00000000000000E+00 0.34158952592303E+03 0.34158952592303E+03
 0.34158952592303E+03 0.34158952592303E+03 0.00000000000000E+00 0.00000000000000E+00 0.15867065425318E+00
 0.00000000000000E+00 -.11658827890929E+02 0.10000000000000E-02 0.13222305045620E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60503822687484E+01 0.22688933507806E+01 0.33779371151677E+03 0.41488733468680E+03
 0.31696422336082E+03 0.31696422336082E+03 0.30615000797613E+03 0.30615000777462E+03 0.31696104976200E+03
 0.31696104976200E+03 0.30615000797857E+03 0.30615000777699E+03 0.31696422336082E+03 0.31696422336082E+03
 0.30615000797613E+03 0.30615000777462E+03 0.31696104976200E+03 0.31696104976200E+03 0.30615000797857E+03
 0.30615000777699E+03 0.31533655856549E+03 0.30615004558282E+03 0.12199826090474E+02 0.15598028864853E+02
 0.17984294317123E+03 0.39933090605833E+03 0.21858874817124E+03 0.15603623456784E+03 0.16680245929199E+03
 0.15603623456784E+03 0.31892411158697E+03 0.15605308627734E+03 0.16665723765613E+03 0.15605308627734E+03
 0.31880248055753E+03 0.15603623456784E+03 0.16680245929199E+03 0.15603623456784E+03 0.31892411158697E+03
 0.15605308627734E+03 0.16665723765613E+03 0.15605308627734E+03 0.31880248055753E+03 0.19524558213412E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36594566027004E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16650737791124E+00 0.00000000000000E+00 0.00000000000000E+00 0.16650737791124E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17483827045788E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17483827045788E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158884122699E+00 0.17682056041668E+00 0.34158952592303E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    755.04738534
 0.88191402528132E-01 0.32655580732707E+03 0.45526181556463E+03 0.45147822777121E+03 0.45004741161032E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14259597568658E+00 0.00000000000000E+00 -.19728184951687E+02
 0.38189736549717E-02 0.12167457663055E+01 0.20948036626504E+04 0.78555137349389E+03 0.65749150081623E+01
 0.24655931280609E+01 0.35352929212979E+03 0.30615035291038E+03 0.34649766040941E+03 0.37287382006610E+03
 0.30615004409951E+03 0.30615008135909E+03 0.34280671740450E+03 0.37285330882416E+03 0.30615003637266E+03
 0.30615008113072E+03 0.34649766040941E+03 0.37287382006610E+03 0.30615004409951E+03 0.30615008135909E+03
 0.34280671740450E+03 0.37285330882416E+03 0.30615003637266E+03 0.30615008113072E+03 0.41568441552938E+03
 0.33861010695650E+03 0.20338316392870E+04 0.18985374092557E+04 0.61671538233539E+03 0.98161684665078E+03
 0.36181788740371E+03 0.12086665002286E+04 0.11614084966654E+04 0.11069119337534E+04 0.17464608044347E+04
 0.11023536606107E+04 0.11611808187360E+04 0.10237282444839E+04 0.17464125663984E+04 0.12086665002286E+04
 0.11614084966654E+04 0.11069119337534E+04 0.17464608044347E+04 0.11023536606107E+04 0.11611808187360E+04
 0.10237282444839E+04 0.17464125663984E+04 0.17272348380080E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49556987429007E+03 0.12948498454730E+01
 0.12948498454730E+01 0.28402067442944E+01 0.00000000000000E+00 0.34184469543167E+03 0.34184469543167E+03
 0.34184469543167E+03 0.34184469543167E+03 0.00000000000000E+00 0.00000000000000E+00 0.15766988581010E+00
 0.00000000000000E+00 -.11656285108219E+02 0.10000000000000E-02 0.13398715272025E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59707216979996E+01 0.22390206367498E+01 0.33861140851292E+03 0.41568349828859E+03
 0.31712928296380E+03 0.31712928296380E+03 0.30615000986840E+03 0.30615000949582E+03 0.31712604147466E+03
 0.31712604147466E+03 0.30615000987138E+03 0.30615000949869E+03 0.31712928296380E+03 0.31712928296380E+03
 0.30615000986840E+03 0.30615000949582E+03 0.31712604147466E+03 0.31712604147466E+03 0.30615000987138E+03
 0.30615000949869E+03 0.31547842650527E+03 0.30615005442233E+03 0.82464800668826E+01 0.11026455211855E+02
 0.18148203084240E+03 0.40206523828281E+03 0.21967579728620E+03 0.15924466173508E+03 0.16826652832899E+03
 0.15924466173508E+03 0.32102282594304E+03 0.15926191000416E+03 0.16812028511068E+03 0.15926191000416E+03
 0.32090069166390E+03 0.15924466173508E+03 0.16826652832899E+03 0.15924466173508E+03 0.32102282594304E+03
 0.15926191000416E+03 0.16812028511069E+03 0.15926191000416E+03 0.32090069166391E+03 0.19543419810813E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36624467459990E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16650297612699E+00 0.00000000000000E+00 0.00000000000000E+00 0.16650297612699E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17474352555078E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17474352555078E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158842231229E+00 0.17668814602043E+00 0.34184469543167E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    761.98029107
 0.88160223536962E-01 0.32669349558685E+03 0.45549884338961E+03 0.45171367397126E+03 0.45028174324166E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14234266937130E+00 0.00000000000000E+00 -.19738447932824E+02
 0.38203240554383E-02 0.12196401252340E+01 0.20940631956632E+04 0.78527369837369E+03 0.65593119105236E+01
 0.24597419664464E+01 0.35381193931495E+03 0.30615038390004E+03 0.34674346075756E+03 0.37322428638170E+03
 0.30615004852367E+03 0.30615008952482E+03 0.34304163459521E+03 0.37320388509499E+03 0.30615004005726E+03
 0.30615008927604E+03 0.34674346075756E+03 0.37322428638170E+03 0.30615004852367E+03 0.30615008952482E+03
 0.34304163459521E+03 0.37320388509499E+03 0.30615004005726E+03 0.30615008927604E+03 0.41607641748908E+03
 0.33901408672560E+03 0.20361176621695E+04 0.18999031230940E+04 0.61580435491300E+03 0.97895176887681E+03
 0.36006839218924E+03 0.12103785895356E+04 0.11625365631786E+04 0.11079373373715E+04 0.17464506926105E+04
 0.11042131138959E+04 0.11623110645054E+04 0.10250014182864E+04 0.17464035534542E+04 0.12103785895356E+04
 0.11625365631786E+04 0.11079373373715E+04 0.17464506926105E+04 0.11042131138960E+04 0.11623110645054E+04
 0.10250014182864E+04 0.17464035534542E+04 0.17269413807466E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49580416173974E+03 0.12948499210909E+01
 0.12948499210909E+01 0.28679383672081E+01 0.00000000000000E+00 0.34197397853532E+03 0.34197397853532E+03
 0.34197397853532E+03 0.34197397853532E+03 0.00000000000000E+00 0.00000000000000E+00 0.15718102904131E+00
 0.00000000000000E+00 -.11654686381528E+02 0.10000000000000E-02 0.13484602660913E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59326924205109E+01 0.22247596576916E+01 0.33901544631611E+03 0.41607538579949E+03
 0.31721229456936E+03 0.31721229456936E+03 0.30615001088141E+03 0.30615001047058E+03 0.31720901920703E+03
 0.31720901920703E+03 0.30615001088468E+03 0.30615001047373E+03 0.31721229456935E+03 0.31721229456935E+03
 0.30615001088141E+03 0.30615001047058E+03 0.31720901920703E+03 0.31720901920703E+03 0.30615001088468E+03
 0.30615001047373E+03 0.31554981291105E+03 0.30615005934494E+03 0.62948920582839E+01 0.87823996369392E+01
 0.18230060650228E+03 0.40344437488163E+03 0.22023226534684E+03 0.16083823360212E+03 0.16899789714226E+03
 0.16083823360212E+03 0.32208123662340E+03 0.16085568056717E+03 0.16885119794016E+03 0.16085568056717E+03
 0.32195890552546E+03 0.16083823360212E+03 0.16899789714226E+03 0.16083823360212E+03 0.32208123662340E+03
 0.16085568056717E+03 0.16885119794017E+03 0.16085568056717E+03 0.32195890552546E+03 0.19554411713972E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36639621343647E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16649490388239E+00 0.00000000000000E+00 0.00000000000000E+00 0.16649490388239E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17471080577770E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17471080577770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158821926840E+00 0.17662114293752E+00 0.34197397853532E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    775.84610252
 0.88107411201864E-01 0.32697195818204E+03 0.45597033607946E+03 0.45218176503754E+03 0.45074759679040E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14185102103051E+00 0.00000000000000E+00 -.19753327249767E+02
 0.38226135368621E-02 0.12252186690544E+01 0.20928089964770E+04 0.78480337367889E+03 0.65294467037254E+01
 0.24485425138970E+01 0.35437377604300E+03 0.30615045258477E+03 0.34723236611275E+03 0.37391988288699E+03
 0.30615005849854E+03 0.30615010793127E+03 0.34350917045924E+03 0.37389969615122E+03 0.30615004837594E+03
 0.30615010763722E+03 0.34723236611275E+03 0.37391988288700E+03 0.30615005849854E+03 0.30615010793127E+03
 0.34350917045924E+03 0.37389969615122E+03 0.30615004837594E+03 0.30615010763722E+03 0.41685226456642E+03
 0.33981565343658E+03 0.20406371240381E+04 0.19026231389636E+04 0.61399761703662E+03 0.97370474038301E+03
 0.35663713526120E+03 0.12137715045924E+04 0.11647627324061E+04 0.11099943467322E+04 0.17464299178259E+04
 0.11078979887973E+04 0.11645413663782E+04 0.10275463656978E+04 0.17463848036942E+04 0.12137715045924E+04
 0.11647627324061E+04 0.11099943467322E+04 0.17464299178259E+04 0.11078979887973E+04 0.11645413663783E+04
 0.10275463656978E+04 0.17463848036942E+04 0.17263700519717E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49626991899460E+03 0.12948500307221E+01
 0.12948500307221E+01 0.29234016130357E+01 0.00000000000000E+00 0.34223466272253E+03 0.34223466272253E+03
 0.34223466272253E+03 0.34223466272253E+03 0.00000000000000E+00 0.00000000000000E+00 0.15622593736388E+00
 0.00000000000000E+00 -.11645148205026E+02 0.10000000000000E-02 0.13651880134133E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58599987118246E+01 0.21974995169342E+01 0.33981713704479E+03 0.41685100577712E+03
 0.31737874020723E+03 0.31737874020723E+03 0.30615001317256E+03 0.30615001267522E+03 0.31737539758688E+03
 0.31737539758688E+03 0.30615001317648E+03 0.30615001267900E+03 0.31737874020724E+03 0.31737874020724E+03
 0.30615001317256E+03 0.30615001267522E+03 0.31737539758688E+03 0.31737539758688E+03 0.30615001317648E+03
 0.30615001267900E+03 0.31569301468161E+03 0.30615007029253E+03 0.24082159196747E+01 0.43401815472016E+01
 0.18393222253936E+03 0.40621059792063E+03 0.22135871426857E+03 0.16401377434156E+03 0.17045528772164E+03
 0.16401377434156E+03 0.32420224990370E+03 0.16403161694574E+03 0.17030773032739E+03 0.16403161694574E+03
 0.32407957564428E+03 0.16401377434156E+03 0.17045528772164E+03 0.16401377434156E+03 0.32420224990370E+03
 0.16403161694574E+03 0.17030773032740E+03 0.16403161694574E+03 0.32407957564429E+03 0.19577814113226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36669878054930E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16643143954503E+00 0.00000000000000E+00 0.00000000000000E+00 0.16643143954503E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17461135389888E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17461135389888E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158799191794E+00 0.17648639130584E+00 0.34223466272253E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    782.77900825
 0.88077063440221E-01 0.32711214921850E+03 0.45620483647262E+03 0.45241480153764E+03 0.45097962989938E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14161248478679E+00 0.00000000000000E+00 -.19758291546076E+02
 0.38239304275945E-02 0.12279056979969E+01 0.20920882718655E+04 0.78453310194956E+03 0.65151583000639E+01
 0.24431843625239E+01 0.35465303447135E+03 0.30615049051798E+03 0.34747553658757E+03 0.37426512378558E+03
 0.30615006409847E+03 0.30615011826204E+03 0.34374183849652E+03 0.37424504170289E+03 0.30615005305210E+03
 0.30615011794298E+03 0.34747553658757E+03 0.37426512378558E+03 0.30615006409847E+03 0.30615011826204E+03
 0.34374183849651E+03 0.37424504170289E+03 0.30615005305210E+03 0.30615011794298E+03 0.41723656239499E+03
 0.34021347017851E+03 0.20428772601169E+04 0.19039797695475E+04 0.61309718135415E+03 0.97111367459560E+03
 0.35495100733468E+03 0.12154551324132E+04 0.11658613182364E+04 0.11110224795193E+04 0.17464187450482E+04
 0.11097266563068E+04 0.11656419122884E+04 0.10288158804887E+04 0.17463745630410E+04 0.12154551324132E+04
 0.11658613182364E+04 0.11110224795193E+04 0.17464187450481E+04 0.11097266563068E+04 0.11656419122884E+04
 0.10288158804888E+04 0.17463745630410E+04 0.17260913725504E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49650186334138E+03 0.12948500672992E+01
 0.12948500672992E+01 0.29511332359494E+01 0.00000000000000E+00 0.34236539677065E+03 0.34236539677065E+03
 0.34236539677065E+03 0.34236539677065E+03 0.00000000000000E+00 0.00000000000000E+00 0.15575959574605E+00
 0.00000000000000E+00 -.11637581036014E+02 0.10000000000000E-02 0.13733319628099E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58252485317764E+01 0.21844681994162E+01 0.34021501492353E+03 0.41723519709207E+03
 0.31746210240204E+03 0.31746210240204E+03 0.30615001446267E+03 0.30615001391663E+03 0.31745872641647E+03
 0.31745872641647E+03 0.30615001446695E+03 0.30615001392075E+03 0.31746210240204E+03 0.31746210240204E+03
 0.30615001446267E+03 0.30615001391663E+03 0.31745872641647E+03 0.31745872641647E+03 0.30615001446695E+03
 0.30615001392075E+03 0.31576476747968E+03 0.30615007635833E+03 0.46718809081149E+00 0.21358306086346E+01
 0.18474145152009E+03 0.40758819976672E+03 0.22192304098903E+03 0.16559444136124E+03 0.17117746826139E+03
 0.16559444136124E+03 0.32525688268098E+03 0.16561248079431E+03 0.17102950335186E+03 0.16561248079431E+03
 0.32513405652836E+03 0.16559444136124E+03 0.17117746826139E+03 0.16559444136124E+03 0.32525688268098E+03
 0.16561248079431E+03 0.17102950335187E+03 0.16561248079431E+03 0.32513405652837E+03 0.19589609295607E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36684933796696E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16637881016513E+00 0.00000000000000E+00 0.00000000000000E+00 0.16637881016513E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17454654339805E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17454654339805E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158795819853E+00 0.17641897755093E+00 0.34236539677065E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    796.64481971
 0.87994403322790E-01 0.32739379724956E+03 0.45667166246997E+03 0.45287975293233E+03 0.45144300194205E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14114918322512E+00 0.00000000000000E+00 -.19762371766551E+02
 0.38275220963864E-02 0.12330862146908E+01 0.20901250988343E+04 0.78379691206285E+03 0.64877864213299E+01
 0.24329199079987E+01 0.35520838609622E+03 0.30615057419157E+03 0.34795942149686E+03 0.37495069045566E+03
 0.30615007665494E+03 0.30615014141925E+03 0.34420506853539E+03 0.37493081260751E+03 0.30615006355080E+03
 0.30615014104505E+03 0.34795942149686E+03 0.37495069045566E+03 0.30615007665494E+03 0.30615014141925E+03
 0.34420506853539E+03 0.37493081260751E+03 0.30615006355080E+03 0.30615014104505E+03 0.41799842596483E+03
 0.34100335388704E+03 0.20473343999394E+04 0.19066970220477E+04 0.61130353629304E+03 0.96599566515251E+03
 0.35163561117800E+03 0.12188044788716E+04 0.11680357326556E+04 0.11130773602012E+04 0.17464013709828E+04
 0.11133647960177E+04 0.11678200539220E+04 0.10313500339418E+04 0.17463589101641E+04 0.12188044788716E+04
 0.11680357326556E+04 0.11130773602012E+04 0.17464013709828E+04 0.11133647960177E+04 0.11678200539220E+04
 0.10313500339419E+04 0.17463589101641E+04 0.17255560583539E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49696491478522E+03 0.12948500973624E+01
 0.12948500973624E+01 0.30065964817769E+01 0.00000000000000E+00 0.34262634165226E+03 0.34262634165226E+03
 0.34262634165226E+03 0.34262634165226E+03 0.00000000000000E+00 0.00000000000000E+00 0.15484907238819E+00
 0.00000000000000E+00 -.11615722694135E+02 0.10000000000000E-02 0.13891908037368E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57587481708639E+01 0.21595305640740E+01 0.34100501634797E+03 0.41799686408695E+03
 0.31762895263199E+03 0.31762895263199E+03 0.30615001736407E+03 0.30615001670849E+03 0.31762551046436E+03
 0.31762551046436E+03 0.30615001736916E+03 0.30615001671338E+03 0.31762895263199E+03 0.31762895263199E+03
 0.30615001736407E+03 0.30615001670849E+03 0.31762551046436E+03 0.31762551046436E+03 0.30615001736916E+03
 0.30615001671338E+03 0.31590844846856E+03 0.30615008978152E+03 -.34196975986649E+01 -.22477081763050E+01
 0.18633925432213E+03 0.41031296459476E+03 0.22304201400102E+03 0.16873812026672E+03 0.17260131556708E+03
 0.16873812026672E+03 0.32733807573887E+03 0.16875655128222E+03 0.17245256996049E+03 0.16875655128222E+03
 0.32721497587656E+03 0.16873812026672E+03 0.17260131556708E+03 0.16873812026672E+03 0.32733807573887E+03
 0.16875655128222E+03 0.17245256996049E+03 0.16875655128222E+03 0.32721497587656E+03 0.19612231227861E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36714749432139E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16622344831105E+00 0.00000000000000E+00 0.00000000000000E+00 0.16622344831105E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17437946545906E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17437946545906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158808559941E+00 0.17628478545215E+00 0.34262634165226E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    803.57772544
 0.87937019433958E-01 0.32753569714411E+03 0.45690418702165E+03 0.45311209388548E+03 0.45167485437187E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14092400323203E+00 0.00000000000000E+00 -.19760517738242E+02
 0.38300195445050E-02 0.12355851590239E+01 0.20887621869914E+04 0.78328582012177E+03 0.64746650132316E+01
 0.24279993799618E+01 0.35548454235362E+03 0.30615062019432E+03 0.34820017716450E+03 0.37529108622912E+03
 0.30615008366768E+03 0.30615015434831E+03 0.34443566932832E+03 0.37527130803545E+03 0.30615006942154E+03
 0.30615015394379E+03 0.34820017716450E+03 0.37529108622912E+03 0.30615008366768E+03 0.30615015434831E+03
 0.34443566932831E+03 0.37527130803545E+03 0.30615006942154E+03 0.30615015394379E+03 0.41837614809790E+03
 0.34139544418924E+03 0.20495588489261E+04 0.19080678438453E+04 0.61041301095166E+03 0.96347187222458E+03
 0.35000679621816E+03 0.12204739537040E+04 0.11691155644485E+04 0.11141092472964E+04 0.17464000053481E+04
 0.11151782103389E+04 0.11689016613818E+04 0.10326201334561E+04 0.17463583414106E+04 0.12204739537040E+04
 0.11691155644485E+04 0.11141092472965E+04 0.17464000053481E+04 0.11151782103389E+04 0.11689016613818E+04
 0.10326201334561E+04 0.17463583414107E+04 0.17253062031061E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49719650561922E+03 0.12948500837019E+01
 0.12948500837019E+01 0.30343281046907E+01 0.00000000000000E+00 0.34275604981993E+03 0.34275604981993E+03
 0.34275604981993E+03 0.34275604981993E+03 0.00000000000000E+00 0.00000000000000E+00 0.15440480649561E+00
 0.00000000000000E+00 -.11600257991575E+02 0.10000000000000E-02 0.13969097163090E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57269270208372E+01 0.21475976328139E+01 0.34139716339744E+03 0.41837449529551E+03
 0.31771238120344E+03 0.31771238120344E+03 0.30615001898912E+03 0.30615001827219E+03 0.31770890622488E+03
 0.31770890622488E+03 0.30615001899465E+03 0.30615001827750E+03 0.31771238120344E+03 0.31771238120344E+03
 0.30615001898912E+03 0.30615001827219E+03 0.31770890622487E+03 0.31770890622487E+03 0.30615001899465E+03
 0.30615001827750E+03 0.31598032786537E+03 0.30615009718416E+03 -.53688885730453E+01 -.44291674468211E+01
 0.18712484827258E+03 0.41165220064534E+03 0.22359172813140E+03 0.17029960585190E+03 0.17330002461793E+03
 0.17029960585190E+03 0.32835793931441E+03 0.17031823160184E+03 0.17315090298219E+03 0.17031823160184E+03
 0.32823471456992E+03 0.17029960585190E+03 0.17330002461793E+03 0.17029960585190E+03 0.32835793931442E+03
 0.17031823160185E+03 0.17315090298219E+03 0.17031823160185E+03 0.32823471456993E+03 0.19622612279263E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36729427287320E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16611205073318E+00 0.00000000000000E+00 0.00000000000000E+00 0.16611205073318E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17426993368149E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17426993368149E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158828141042E+00 0.17621830251932E+00 0.34275604981993E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    810.51063117
 0.87864891808062E-01 0.32767876984586E+03 0.45713627598814E+03 0.45334468606450E+03 0.45190723137923E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14070281040071E+00 0.00000000000000E+00 -.19755333024956E+02
 0.38331633503320E-02 0.12380274843148E+01 0.20870490685734E+04 0.78264340071501E+03 0.64618920834604E+01
 0.24232095312976E+01 0.35575973756711E+03 0.30615066915614E+03 0.34844017948388E+03 0.37562994129252E+03
 0.30615009120957E+03 0.30615016824971E+03 0.34466563058218E+03 0.37561026116592E+03 0.30615007574043E+03
 0.30615016781293E+03 0.34844017948388E+03 0.37562994129253E+03 0.30615009120957E+03 0.30615016824971E+03
 0.34466563058218E+03 0.37561026116592E+03 0.30615007574043E+03 0.30615016781293E+03 0.41875183903837E+03
 0.34178563694578E+03 0.20517863915894E+04 0.19094555362079E+04 0.60952892315948E+03 0.96097433604056E+03
 0.34839776826528E+03 0.12221428863946E+04 0.11701936245695E+04 0.11151487530564E+04 0.17464073648581E+04
 0.11169909253093E+04 0.11699814448088E+04 0.10338971218569E+04 0.17463664610715E+04 0.12221428863946E+04
 0.11701936245695E+04 0.11151487530565E+04 0.17464073648581E+04 0.11169909253094E+04 0.11699814448088E+04
 0.10338971218569E+04 0.17463664610715E+04 0.17250738316777E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49742852857779E+03 0.12948500455008E+01
 0.12948500455008E+01 0.30620597276044E+01 0.00000000000000E+00 0.34288486518629E+03 0.34288486518629E+03
 0.34288486518629E+03 0.34288486518629E+03 0.00000000000000E+00 0.00000000000000E+00 0.15396783129612E+00
 0.00000000000000E+00 -.11580876179473E+02 0.10000000000000E-02 0.14044908980172E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56960141295998E+01 0.21360052985999E+01 0.34178741167818E+03 0.41875009963907E+03
 0.31779576859250E+03 0.31779576859250E+03 0.30615002074010E+03 0.30615001995705E+03 0.31779226099118E+03
 0.31779226099118E+03 0.30615002074609E+03 0.30615001996282E+03 0.31779576859250E+03 0.31779576859250E+03
 0.30615002074010E+03 0.30615001995705E+03 0.31779226099118E+03 0.31779226099118E+03 0.30615002074609E+03
 0.30615001996282E+03 0.31605219784923E+03 0.30615010507894E+03 -.73244444219205E+01 -.66050994611774E+01
 0.18789918990007E+03 0.41296981778098E+03 0.22413113193141E+03 0.17185293118014E+03 0.17398756669152E+03
 0.17185293118014E+03 0.32935874508038E+03 0.17187175096804E+03 0.17383807688373E+03 0.17187175096804E+03
 0.32923540159755E+03 0.17185293118014E+03 0.17398756669152E+03 0.17185293118014E+03 0.32935874508038E+03
 0.17187175096804E+03 0.17383807688373E+03 0.17187175096804E+03 0.32923540159756E+03 0.19632035470293E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36743888535304E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16597157562166E+00 0.00000000000000E+00 0.00000000000000E+00 0.16597157562166E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17413742040358E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17413742040358E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158859170413E+00 0.17615245288904E+00 0.34288486518629E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    820.18569889
 0.87743685309882E-01 0.32788292911208E+03 0.45746068435841E+03 0.45367080776524E+03 0.45223348601679E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14040026432483E+00 0.00000000000000E+00 -.19759277142435E+02
 0.38384580455186E-02 0.12413451091507E+01 0.20841702332373E+04 0.78156383746398E+03 0.64446219999796E+01
 0.24167332499924E+01 0.35614331772878E+03 0.30615074206638E+03 0.34877500509501E+03 0.37609995686161E+03
 0.30615010256276E+03 0.30615018917075E+03 0.34498692739038E+03 0.37608041164899E+03 0.30615008526070E+03
 0.30615018868595E+03 0.34877500509501E+03 0.37609995686161E+03 0.30615010256276E+03 0.30615018917075E+03
 0.34498692739037E+03 0.37608041164899E+03 0.30615008526070E+03 0.30615018868595E+03 0.41926543509724E+03
 0.34232115790810E+03 0.20549086983296E+04 0.19114390324799E+04 0.60843927169739E+03 0.95775758202493E+03
 0.34627611396906E+03 0.12244766993677E+04 0.11717209239200E+04 0.11166281331951E+04 0.17464708149482E+04
 0.11195227446470E+04 0.11715110901589E+04 0.10357017696176E+04 0.17464309335961E+04 0.12244766993677E+04
 0.11717209239200E+04 0.11166281331951E+04 0.17464708149481E+04 0.11195227446470E+04 0.11715110901589E+04
 0.10357017696176E+04 0.17464309335961E+04 0.17248593790697E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49775413482139E+03 0.12948500745611E+01
 0.12948500745611E+01 0.31007599985022E+01 0.00000000000000E+00 0.34306343031496E+03 0.34306343031496E+03
 0.34306343031496E+03 0.34306343031496E+03 0.00000000000000E+00 0.00000000000000E+00 0.15336999971918E+00
 0.00000000000000E+00 -.11567521828664E+02 0.10000000000000E-02 0.14148432233084E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56543367266466E+01 0.21203762724925E+01 0.34232301066233E+03 0.41926357789860E+03
 0.31791323871497E+03 0.31791323871497E+03 0.30615002259600E+03 0.30615002249833E+03 0.31790968597772E+03
 0.31790968597772E+03 0.30615002260247E+03 0.30615002250477E+03 0.31791323871497E+03 0.31791323871497E+03
 0.30615002259600E+03 0.30615002249833E+03 0.31790968597772E+03 0.31790968597772E+03 0.30615002260247E+03
 0.30615002250477E+03 0.31615356539492E+03 0.30615011685998E+03 -.10011799491247E+02 -.95720437898944E+01
 0.18895505129118E+03 0.41476982063529E+03 0.22486999408766E+03 0.17398011731942E+03 0.17492299107823E+03
 0.17398011731942E+03 0.33072254014011E+03 0.17399920713595E+03 0.17477307425488E+03 0.17399920713595E+03
 0.33059911561805E+03 0.17398011731942E+03 0.17492299107823E+03 0.17398011731942E+03 0.33072254014011E+03
 0.17399920713595E+03 0.17477307425488E+03 0.17399920713595E+03 0.33059911561806E+03 0.19644941754228E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36764561623182E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16587161098493E+00 0.00000000000000E+00 0.00000000000000E+00 0.16587161098493E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17405638051902E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17405638051902E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158863375592E+00 0.17606083158504E+00 0.34306343031496E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    830.00706641
 0.87655740100398E-01 0.32808293791118E+03 0.45778988049770E+03 0.45400002781484E+03 0.45256210571181E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14010044118342E+00 0.00000000000000E+00 -.19771158485974E+02
 0.38423088576235E-02 0.12446072518223E+01 0.20820814506172E+04 0.78078054398145E+03 0.64277305055765E+01
 0.24103989395912E+01 0.35653123139162E+03 0.30615082102269E+03 0.34911372793614E+03 0.37657506073636E+03
 0.30615011498749E+03 0.30615021206037E+03 0.34531205528078E+03 0.37655564994419E+03 0.30615009568810E+03
 0.30615021152359E+03 0.34911372793614E+03 0.37657506073636E+03 0.30615011498749E+03 0.30615021206037E+03
 0.34531205528078E+03 0.37655564994419E+03 0.30615009568810E+03 0.30615021152359E+03 0.41978637588498E+03
 0.34286281090790E+03 0.20580526434760E+04 0.19133846523605E+04 0.60730731774100E+03 0.95447175966712E+03
 0.34412790533742E+03 0.12268350615348E+04 0.11732716161884E+04 0.11180861499143E+04 0.17465547373234E+04
 0.11220802080928E+04 0.11730640320537E+04 0.10374877248050E+04 0.17465157875645E+04 0.12268350615348E+04
 0.11732716161884E+04 0.11180861499143E+04 0.17465547373234E+04 0.11220802080928E+04 0.11730640320538E+04
 0.10374877248050E+04 0.17465157875645E+04 0.17246263374720E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49808235323430E+03 0.12948501621032E+01
 0.12948501621032E+01 0.31400454685938E+01 0.00000000000000E+00 0.34324635468499E+03 0.34324635468499E+03
 0.34324635468499E+03 0.34324635468499E+03 0.00000000000000E+00 0.00000000000000E+00 0.15277655575401E+00
 0.00000000000000E+00 -.11561431758068E+02 0.10000000000000E-02 0.14250846306325E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56137016904389E+01 0.21051381339146E+01 0.34286478873641E+03 0.41978434974647E+03
 0.31803235535695E+03 0.31803235535695E+03 0.30615002558155E+03 0.30615002528473E+03 0.31802875732887E+03
 0.31802875732887E+03 0.30615002558881E+03 0.30615002529191E+03 0.31803235535695E+03 0.31803235535695E+03
 0.30615002558155E+03 0.30615002528473E+03 0.31802875732887E+03 0.31802875732887E+03 0.30615002558881E+03
 0.30615002529191E+03 0.31625639135846E+03 0.30615012964405E+03 -.12741664749154E+02 -.12549694569641E+02
 0.19003152455186E+03 0.41661692439488E+03 0.22563524222026E+03 0.17614367672800E+03 0.17587677598545E+03
 0.17614367672800E+03 0.33212247353500E+03 0.17616303775186E+03 0.17572639970780E+03 0.17616303775186E+03
 0.33199893717156E+03 0.17614367672800E+03 0.17587677598545E+03 0.17614367672800E+03 0.33212247353500E+03
 0.17616303775186E+03 0.17572639970780E+03 0.17616303775186E+03 0.33199893717157E+03 0.19659805285495E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36785562987075E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16583972238673E+00 0.00000000000000E+00 0.00000000000000E+00 0.16583972238673E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17395189890280E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17395189890280E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158845904359E+00 0.17596683587637E+00 0.34324635468499E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    840.13105929
 0.87583808103128E-01 0.32828271848638E+03 0.45812612317837E+03 0.45433539656503E+03 0.45289646415859E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13979934223897E+00 0.00000000000000E+00 -.19782949620956E+02
 0.38454641941516E-02 0.12478632445432E+01 0.20803730306908E+04 0.78013988650903E+03 0.64109589211665E+01
 0.24041095954374E+01 0.35692850118791E+03 0.30615090966212E+03 0.34946068500794E+03 0.37706189046918E+03
 0.30615012913116E+03 0.30615023810716E+03 0.34564510142716E+03 0.37704261475657E+03 0.30615010757105E+03
 0.30615023751207E+03 0.34946068500794E+03 0.37706189046918E+03 0.30615012913116E+03 0.30615023810716E+03
 0.34564510142716E+03 0.37704261475657E+03 0.30615010757105E+03 0.30615023751207E+03 0.42032223656013E+03
 0.34342040020912E+03 0.20612352577060E+04 0.19153029410607E+04 0.60606915450560E+03 0.95099611750850E+03
 0.34189661723036E+03 0.12292343469921E+04 0.11748243737582E+04 0.11195324566856E+04 0.17465980904197E+04
 0.11246836418679E+04 0.11746189928253E+04 0.10392710276071E+04 0.17465600183430E+04 0.12292343469921E+04
 0.11748243737582E+04 0.11195324566856E+04 0.17465980904197E+04 0.11246836418679E+04 0.11746189928253E+04
 0.10392710276071E+04 0.17465600183430E+04 0.17243152304150E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49841644161903E+03 0.12948502489807E+01
 0.12948502489807E+01 0.31805414400981E+01 0.00000000000000E+00 0.34343516585445E+03 0.34343516585445E+03
 0.34343516585445E+03 0.34343516585445E+03 0.00000000000000E+00 0.00000000000000E+00 0.15217890020516E+00
 0.00000000000000E+00 -.11555496448320E+02 0.10000000000000E-02 0.14353678650151E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55734841185927E+01 0.20900565444723E+01 0.34342243202095E+03 0.42032013078373E+03
 0.31815462809381E+03 0.31815462809381E+03 0.30615002879869E+03 0.30615002846455E+03 0.31815098368559E+03
 0.31815098368559E+03 0.30615002880679E+03 0.30615002847255E+03 0.31815462809381E+03 0.31815462809381E+03
 0.30615002879869E+03 0.30615002846455E+03 0.31815098368559E+03 0.31815098368559E+03 0.30615002880679E+03
 0.30615002847255E+03 0.31636195814182E+03 0.30615014403453E+03 -.15576530240652E+02 -.15573792605355E+02
 0.19113881215159E+03 0.41852698339219E+03 0.22643247717984E+03 0.17838014206913E+03 0.17685747153730E+03
 0.17838014206913E+03 0.33357036134614E+03 0.17839978109688E+03 0.17670660285373E+03 0.17839978109688E+03
 0.33344668924843E+03 0.17838014206913E+03 0.17685747153730E+03 0.17838014206913E+03 0.33357036134614E+03
 0.17839978109689E+03 0.17670660285374E+03 0.17839978109689E+03 0.33344668924844E+03 0.19676503354096E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36807310360193E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16580492656897E+00 0.00000000000000E+00 0.00000000000000E+00 0.16580492656897E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17386725196259E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17386725196259E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158826901806E+00 0.17586990970906E+00 0.34343516585445E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    850.13305151
 0.87522219735854E-01 0.32847828226759E+03 0.45845569122428E+03 0.45466372077515E+03 0.45322365369114E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13950980808690E+00 0.00000000000000E+00 -.19792948040944E+02
 0.38481698841493E-02 0.12509727245319E+01 0.20789102978411E+04 0.77959136169041E+03 0.63950235229897E+01
 0.23981338211211E+01 0.35731857531574E+03 0.30615100496833E+03 0.34980147452281E+03 0.37753980051866E+03
 0.30615014455093E+03 0.30615026649275E+03 0.34597229378077E+03 0.37752065497467E+03 0.30615012054020E+03
 0.30615026583500E+03 0.34980147452280E+03 0.37753980051866E+03 0.30615014455093E+03 0.30615026649275E+03
 0.34597229378077E+03 0.37752065497467E+03 0.30615012054020E+03 0.30615026583500E+03 0.42084909622800E+03
 0.34396831777610E+03 0.20643340942305E+04 0.19171544138737E+04 0.60480488452892E+03 0.94752466783174E+03
 0.33969575888017E+03 0.12315788685050E+04 0.11763231046306E+04 0.11209372934173E+04 0.17466130705484E+04
 0.11272287436057E+04 0.11761197928572E+04 0.10410063406928E+04 0.17465757895973E+04 0.12315788685050E+04
 0.11763231046306E+04 0.11209372934173E+04 0.17466130705484E+04 0.11272287436057E+04 0.11761197928572E+04
 0.10410063406928E+04 0.17465757895973E+04 0.17239703702204E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49874341260021E+03 0.12948503226494E+01
 0.12948503226494E+01 0.32205494089861E+01 0.00000000000000E+00 0.34362209041754E+03 0.34362209041754E+03
 0.34362209041754E+03 0.34362209041754E+03 0.00000000000000E+00 0.00000000000000E+00 0.15160218666436E+00
 0.00000000000000E+00 -.11548016149932E+02 0.10000000000000E-02 0.14452595816261E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55353378048522E+01 0.20757516768196E+01 0.34397040128715E+03 0.42084691045093E+03
 0.31827510628730E+03 0.31827510628730E+03 0.30615003246246E+03 0.30615003193987E+03 0.31827141638360E+03
 0.31827141638360E+03 0.30615003247150E+03 0.30615003194877E+03 0.31827510628730E+03 0.31827510628730E+03
 0.30615003246246E+03 0.30615003193987E+03 0.31827141638360E+03 0.31827141638360E+03 0.30615003247150E+03
 0.30615003194877E+03 0.31646600304234E+03 0.30615015954867E+03 -.18378972146483E+02 -.18780328162008E+02
 0.19222996137168E+03 0.42041697347384E+03 0.22722586229530E+03 0.18058741732605E+03 0.17782357540230E+03
 0.18058741732605E+03 0.33500274395685E+03 0.18060732955557E+03 0.17767221662975E+03 0.18060732955557E+03
 0.33487893202064E+03 0.18058741732605E+03 0.17782357540230E+03 0.18058741732605E+03 0.33500274395685E+03
 0.18060732955558E+03 0.17767221662975E+03 0.18060732955558E+03 0.33487893202065E+03 0.19687876062955E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36828754418286E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16575772095512E+00 0.00000000000000E+00 0.00000000000000E+00 0.16575772095512E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17377846267043E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17377846267043E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158812739196E+00 0.17577410737557E+00 0.34362209041754E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    860.23557089
 0.87462166254327E-01 0.32867523556470E+03 0.45878623890571E+03 0.45499297550380E+03 0.45355176690385E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13922523473833E+00 0.00000000000000E+00 -.19800360412778E+02
 0.38508118047404E-02 0.12540065514797E+01 0.20774840230187E+04 0.77905650863200E+03 0.63795519972049E+01
 0.23923319989518E+01 0.35771025840849E+03 0.30615110963599E+03 0.35014381257566E+03 0.37801938983442E+03
 0.30615016172067E+03 0.30615029808639E+03 0.34630106989516E+03 0.37800037258358E+03 0.30615013499683E+03
 0.30615029735991E+03 0.35014381257566E+03 0.37801938983442E+03 0.30615016172067E+03 0.30615029808639E+03
 0.34630106989515E+03 0.37800037258358E+03 0.30615013499683E+03 0.30615029735991E+03 0.42137788301806E+03
 0.34451590289631E+03 0.20674279955550E+04 0.19189981860409E+04 0.60350510600283E+03 0.94401099632586E+03
 0.33748836479302E+03 0.12339254091303E+04 0.11778079863775E+04 0.11223421573031E+04 0.17466095256809E+04
 0.11297768212727E+04 0.11776066643924E+04 0.10427424018861E+04 0.17465729743140E+04 0.12339254091303E+04
 0.11778079863775E+04 0.11223421573031E+04 0.17466095256810E+04 0.11297768212727E+04 0.11776066643924E+04
 0.10427424018861E+04 0.17465729743140E+04 0.17236024899620E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49907130420333E+03 0.12948503772640E+01
 0.12948503772640E+01 0.32609594864870E+01 0.00000000000000E+00 0.34381252344607E+03 0.34381252344607E+03
 0.34381252344607E+03 0.34381252344607E+03 0.00000000000000E+00 0.00000000000000E+00 0.15103327343725E+00
 0.00000000000000E+00 -.11537651390231E+02 0.10000000000000E-02 0.14549808847057E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54983540224433E+01 0.20618827584163E+01 0.34451805635096E+03 0.42137561143890E+03
 0.31839664464910E+03 0.31839664464910E+03 0.30615003655004E+03 0.30615003581917E+03 0.31839290918937E+03
 0.31839290918937E+03 0.30615003656011E+03 0.30615003582904E+03 0.31839664464910E+03 0.31839664464910E+03
 0.30615003655004E+03 0.30615003581917E+03 0.31839290918937E+03 0.31839290918937E+03 0.30615003656011E+03
 0.30615003582904E+03 0.31657099134204E+03 0.30615017663171E+03 -.21177374117048E+02 -.22205419746658E+02
 0.19333716966350E+03 0.42234702278256E+03 0.22804316727074E+03 0.18280816116810E+03 0.17880476643080E+03
 0.18280816116810E+03 0.33646697259491E+03 0.18282834761893E+03 0.17865293185922E+03 0.18282834761893E+03
 0.33634303614501E+03 0.18280816116810E+03 0.17880476643080E+03 0.18280816116810E+03 0.33646697259491E+03
 0.18282834761894E+03 0.17865293185923E+03 0.18282834761894E+03 0.33634303614501E+03 0.19695920413310E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36850459671618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16568787435365E+00 0.00000000000000E+00 0.00000000000000E+00 0.16568787435365E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17367905436884E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17367905436884E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158806237588E+00 0.17567669992795E+00 0.34381252344607E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    870.23835691
 0.87397431695317E-01 0.32887090742753E+03 0.45911166346472E+03 0.45531742760482E+03 0.45387523286065E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13895099170820E+00 0.00000000000000E+00 -.19804909536052E+02
 0.38536637547199E-02 0.12569073801717E+01 0.20759465561057E+04 0.77847995853963E+03 0.63648285674852E+01
 0.23868107128070E+01 0.35809598708886E+03 0.30615122205922E+03 0.35048111248206E+03 0.37849117019797E+03
 0.30615018041341E+03 0.30615033246725E+03 0.34662514325275E+03 0.37847227698901E+03 0.30615015075246E+03
 0.30615033166703E+03 0.35048111248206E+03 0.37849117019797E+03 0.30615018041341E+03 0.30615033246725E+03
 0.34662514325275E+03 0.37847227698901E+03 0.30615015075246E+03 0.30615033166703E+03 0.42189730757437E+03
 0.34505190172548E+03 0.20704673529041E+04 0.19208168479553E+04 0.60221722100830E+03 0.94055989945618E+03
 0.33533159234284E+03 0.12362332217198E+04 0.11792583928175E+04 0.11237299969586E+04 0.17465995926224E+04
 0.11322831462510E+04 0.11790589510832E+04 0.10444554265609E+04 0.17465637026789E+04 0.12362332217198E+04
 0.11792583928175E+04 0.11237299969587E+04 0.17465995926224E+04 0.11322831462510E+04 0.11790589510832E+04
 0.10444554265609E+04 0.17465637026789E+04 0.17232401002043E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49939450104388E+03 0.12948504107821E+01
 0.12948504107821E+01 0.33009706305982E+01 0.00000000000000E+00 0.34400263177705E+03 0.34400263177705E+03
 0.34400263177705E+03 0.34400263177705E+03 0.00000000000000E+00 0.00000000000000E+00 0.15048320018574E+00
 0.00000000000000E+00 -.11524378533236E+02 0.10000000000000E-02 0.14643440671224E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54631969218279E+01 0.20486988456854E+01 0.34505413437324E+03 0.42189494777350E+03
 0.31851703210055E+03 0.31851703210055E+03 0.30615004093413E+03 0.30615004005277E+03 0.31851325196061E+03
 0.31851325196061E+03 0.30615004094529E+03 0.30615004006369E+03 0.31851703210055E+03 0.31851703210055E+03
 0.30615004093413E+03 0.30615004005277E+03 0.31851325196061E+03 0.31851325196061E+03 0.30615004094529E+03
 0.30615004006369E+03 0.31667501803595E+03 0.30615019502781E+03 -.23912003526548E+02 -.25661132737272E+02
 0.19443744074626E+03 0.42427536344238E+03 0.22886573549239E+03 0.18499536767135E+03 0.17978043368033E+03
 0.18499536767135E+03 0.33793082498480E+03 0.18501582381685E+03 0.17962815020391E+03 0.18501582381685E+03
 0.33780678465484E+03 0.18499536767135E+03 0.17978043368033E+03 0.18499536767135E+03 0.33793082498480E+03
 0.18501582381686E+03 0.17962815020391E+03 0.18501582381686E+03 0.33780678465485E+03 0.19703177802507E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36871985764528E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16559521285396E+00 0.00000000000000E+00 0.00000000000000E+00 0.16559521285396E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17356881501084E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17356881501084E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158808196368E+00 0.17557965766545E+00 0.34400263177705E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    880.53213166
 0.87320214173593E-01 0.32907344324608E+03 0.45944508204904E+03 0.45565038890822E+03 0.45420741310194E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13867618948581E+00 0.00000000000000E+00 -.19807473395825E+02
 0.38570712372533E-02 0.12597899895013E+01 0.20741125864445E+04 0.77779221991668E+03 0.63502647795818E+01
 0.23813492923432E+01 0.35849095781425E+03 0.30615134728275E+03 0.35082667474213E+03 0.37897358316685E+03
 0.30615020151107E+03 0.30615037125360E+03 0.34695731604266E+03 0.37895481466883E+03 0.30615016855354E+03
 0.30615037037135E+03 0.35082667474213E+03 0.37897358316685E+03 0.30615020151107E+03 0.30615037125360E+03
 0.34695731604265E+03 0.37895481466883E+03 0.30615016855354E+03 0.30615037037135E+03 0.42242737274198E+03
 0.34559697679530E+03 0.20735810520731E+04 0.19226943448227E+04 0.60090333586072E+03 0.93705641775893E+03
 0.33314856521890E+03 0.12385976563677E+04 0.11807387990054E+04 0.11251613551415E+04 0.17465937345397E+04
 0.11348508745260E+04 0.11805412063912E+04 0.10462187461872E+04 0.17465584680773E+04 0.12385976563677E+04
 0.11807387990054E+04 0.11251613551415E+04 0.17465937345397E+04 0.11348508745260E+04 0.11805412063912E+04
 0.10462187461872E+04 0.17465584680772E+04 0.17228840216160E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49972632385185E+03 0.12948504296727E+01
 0.12948504296727E+01 0.33421457295647E+01 0.00000000000000E+00 0.34419946850110E+03 0.34419946850110E+03
 0.34419946850110E+03 0.34419946850110E+03 0.00000000000000E+00 0.00000000000000E+00 0.14993061637924E+00
 0.00000000000000E+00 -.11508340789739E+02 0.10000000000000E-02 0.14737139839331E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54284617552786E+01 0.20356731582295E+01 0.34559929391612E+03 0.42242492092241E+03
 0.31864106755968E+03 0.31864106755968E+03 0.30615004582903E+03 0.30615004484228E+03 0.31863724190725E+03
 0.31863724190725E+03 0.30615004584139E+03 0.30615004485437E+03 0.31864106755968E+03 0.31864106755968E+03
 0.30615004582903E+03 0.30615004484228E+03 0.31863724190725E+03 0.31863724190725E+03 0.30615004584139E+03
 0.30615004485437E+03 0.31678223515292E+03 0.30615021556977E+03 -.26689702330366E+02 -.29249775630755E+02
 0.19557071036267E+03 0.42626979880400E+03 0.22972123488952E+03 0.18723136030871E+03 0.18078556307438E+03
 0.18723136030871E+03 0.33944495284300E+03 0.18725209202118E+03 0.18063283994476E+03 0.18725209202118E+03
 0.33932082472437E+03 0.18723136030871E+03 0.18078556307438E+03 0.18723136030871E+03 0.33944495284300E+03
 0.18725209202119E+03 0.18063283994476E+03 0.18725209202119E+03 0.33932082472437E+03 0.19710339604024E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36894146176899E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16548185794274E+00 0.00000000000000E+00 0.00000000000000E+00 0.16548185794274E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17344317983498E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17344317983498E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158816863230E+00 0.17547936566739E+00 0.34419946850110E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    890.82590640
 0.87233191981426E-01 0.32927601237017E+03 0.45977718713407E+03 0.45598250912341E+03 0.45453894062302E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13840847699966E+00 0.00000000000000E+00 -.19810215074885E+02
 0.38609186555702E-02 0.12625742566028E+01 0.20720457263347E+04 0.77701714737550E+03 0.63362609827999E+01
 0.23760978685500E+01 0.35888397413752E+03 0.30615148272377E+03 0.35117069122172E+03 0.37945298273833E+03
 0.30615022463157E+03 0.30615041373828E+03 0.34728815461054E+03 0.37943433606693E+03 0.30615018808143E+03
 0.30615041276745E+03 0.35117069122172E+03 0.37945298273833E+03 0.30615022463157E+03 0.30615041373828E+03
 0.34728815461054E+03 0.37943433606693E+03 0.30615018808143E+03 0.30615041276745E+03 0.42295339858273E+03
 0.34613594191615E+03 0.20766836470712E+04 0.19245726097044E+04 0.59959820120690E+03 0.93359513527036E+03
 0.33099894305743E+03 0.12409534813106E+04 0.11822098853238E+04 0.11265903064104E+04 0.17465956796234E+04
 0.11374090402685E+04 0.11820140594354E+04 0.10479774573196E+04 0.17465609823612E+04 0.12409534813106E+04
 0.11822098853238E+04 0.11265903064104E+04 0.17465956796234E+04 0.11374090402685E+04 0.11820140594354E+04
 0.10479774573196E+04 0.17465609823612E+04 0.17225439598345E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50005743122012E+03 0.12948504498735E+01
 0.12948504498735E+01 0.33833208285311E+01 0.00000000000000E+00 0.34439722230099E+03 0.34439722230099E+03
 0.34439722230099E+03 0.34439722230099E+03 0.00000000000000E+00 0.00000000000000E+00 0.14939144465807E+00
 0.00000000000000E+00 -.11492488113711E+02 0.10000000000000E-02 0.14828211300817E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53951213923954E+01 0.20231705221483E+01 0.34613834005415E+03 0.42295085970183E+03
 0.31876520530454E+03 0.31876520530454E+03 0.30615005120578E+03 0.30615005010326E+03 0.31876133461238E+03
 0.31876133461238E+03 0.30615005121942E+03 0.30615005011661E+03 0.31876520530454E+03 0.31876520530454E+03
 0.30615005120578E+03 0.30615005010326E+03 0.31876133461238E+03 0.31876133461238E+03 0.30615005121942E+03
 0.30615005011661E+03 0.31688957707653E+03 0.30615023784295E+03 -.29435728719878E+02 -.32858187909706E+02
 0.19670367707113E+03 0.42827151622448E+03 0.23058432076800E+03 0.18945312565965E+03 0.18179039554244E+03
 0.18945312565965E+03 0.34096454735335E+03 0.18947413090730E+03 0.18163724860526E+03 0.18947413090730E+03
 0.34084034413401E+03 0.18945312565965E+03 0.18179039554244E+03 0.18945312565965E+03 0.34096454735335E+03
 0.18947413090730E+03 0.18163724860526E+03 0.18947413090730E+03 0.34084034413402E+03 0.19717365861523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36916354498053E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16537041766136E+00 0.00000000000000E+00 0.00000000000000E+00 0.16537041766136E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17331612531192E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17331612531192E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158824866857E+00 0.17537871420479E+00 0.34439722230099E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    902.55617291
 0.87131407880021E-01 0.32950435193450E+03 0.46015385416076E+03 0.45635929580482E+03 0.45491507119924E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13811161147437E+00 0.00000000000000E+00 -.19817030729620E+02
 0.38654284893630E-02 0.12656339604037E+01 0.20696282500154E+04 0.77611059375577E+03 0.63209429031504E+01
 0.23703535886814E+01 0.35932923193996E+03 0.30615165061321E+03 0.35156057454444E+03 0.37999571194846E+03
 0.30615025369994E+03 0.30615046712302E+03 0.34766321565056E+03 0.37997720059370E+03 0.30615021266021E+03
 0.30615046604256E+03 0.35156057454444E+03 0.37999571194846E+03 0.30615025369994E+03 0.30615046712302E+03
 0.34766321565056E+03 0.37997720059370E+03 0.30615021266021E+03 0.30615046604256E+03 0.42354956668976E+03
 0.34674450583606E+03 0.20801993263967E+04 0.19266911529470E+04 0.59809383177679E+03 0.92965441996053E+03
 0.32857011902487E+03 0.12436251385466E+04 0.11838714762856E+04 0.11282004607252E+04 0.17466017414734E+04
 0.11403102187302E+04 0.11836775623955E+04 0.10499615328694E+04 0.17465676255982E+04 0.12436251385466E+04
 0.11838714762856E+04 0.11282004607252E+04 0.17466017414734E+04 0.11403102187302E+04 0.11836775623955E+04
 0.10499615328694E+04 0.17465676255982E+04 0.17221542320968E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50043308203172E+03 0.12948505000915E+01
 0.12948505000915E+01 0.34302418945906E+01 0.00000000000000E+00 0.34462371488885E+03 0.34462371488885E+03
 0.34462371488885E+03 0.34462371488885E+03 0.00000000000000E+00 0.00000000000000E+00 0.14879296666749E+00
 0.00000000000000E+00 -.11478480605834E+02 0.10000000000000E-02 0.14928868762379E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53587449439975E+01 0.20095293539991E+01 0.34674698595741E+03 0.42354694070848E+03
 0.31890651116979E+03 0.31890651116979E+03 0.30615005836629E+03 0.30615005673429E+03 0.31890258970122E+03
 0.31890258970122E+03 0.30615005838163E+03 0.30615005674920E+03 0.31890651116979E+03 0.31890651116979E+03
 0.30615005836629E+03 0.30615005673429E+03 0.31890258970122E+03 0.31890258970122E+03 0.30615005838163E+03
 0.30615005674920E+03 0.31701179350897E+03 0.30615026552589E+03 -.32540680739720E+02 -.36996768127629E+02
 0.19799736595721E+03 0.43056832127978E+03 0.23158096849278E+03 0.19197628409414E+03 0.18293792015285E+03
 0.19197628409414E+03 0.34270895865808E+03 0.19199759846486E+03 0.18278428823487E+03 0.19199759846486E+03
 0.34258466441308E+03 0.19197628409414E+03 0.18293792015285E+03 0.19197628409414E+03 0.34270895865808E+03
 0.19199759846486E+03 0.18278428823487E+03 0.19199759846486E+03 0.34258466441308E+03 0.19725494504454E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36941792340556E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16527637193715E+00 0.00000000000000E+00 0.00000000000000E+00 0.16527637193715E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17318227726920E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17318227726920E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158822161613E+00 0.17526344892828E+00 0.34462371488885E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    910.57604383
 0.87070637843050E-01 0.32965685464468E+03 0.46040990822797E+03 0.45661499096949E+03 0.45517011748781E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13791357888417E+00 0.00000000000000E+00 -.19825792138007E+02
 0.38681260765287E-02 0.12676590524319E+01 0.20681849147946E+04 0.77556934304796E+03 0.63108451634945E+01
 0.23665669363105E+01 0.35963209803184E+03 0.30615177442449E+03 0.35182587635361E+03 0.38036460449606E+03
 0.30615027541224E+03 0.30615050697735E+03 0.34791852658806E+03 0.38034618359429E+03 0.30615023103741E+03
 0.30615050581620E+03 0.35182587635360E+03 0.38036460449606E+03 0.30615027541224E+03 0.30615050697735E+03
 0.34791852658806E+03 0.38034618359429E+03 0.30615023103741E+03 0.30615050581620E+03 0.42395433329544E+03
 0.34715655276228E+03 0.20825764836092E+04 0.19280968249621E+04 0.59706081317142E+03 0.92697283012821E+03
 0.32692671289093E+03 0.12454369048445E+04 0.11849911367978E+04 0.11292717435747E+04 0.17465979185925E+04
 0.11422776943768E+04 0.11847984719409E+04 0.10512868005664E+04 0.17465641619443E+04 0.12454369048445E+04
 0.11849911367978E+04 0.11292717435747E+04 0.17465979185925E+04 0.11422776943768E+04 0.11847984719409E+04
 0.10512868005664E+04 0.17465641619442E+04 0.17218741892570E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50068787468147E+03 0.12948505646458E+01
 0.12948505646458E+01 0.34623213782600E+01 0.00000000000000E+00 0.34477961854726E+03 0.34477961854726E+03
 0.34477961854726E+03 0.34477961854726E+03 0.00000000000000E+00 0.00000000000000E+00 0.14839328238404E+00
 0.00000000000000E+00 -.11474022059928E+02 0.10000000000000E-02 0.14995811539685E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53348229796225E+01 0.20005586173584E+01 0.34715907998132E+03 0.42395165707977E+03
 0.31900315650668E+03 0.31900315650668E+03 0.30615006347328E+03 0.30615006169849E+03 0.31899920059753E+03
 0.31899920059753E+03 0.30615006348980E+03 0.30615006171454E+03 0.31900315650668E+03 0.31900315650668E+03
 0.30615006347328E+03 0.30615006169849E+03 0.31899920059753E+03 0.31899920059753E+03 0.30615006348980E+03
 0.30615006171454E+03 0.31709539851440E+03 0.30615028598982E+03 -.34639902430538E+02 -.39824580047964E+02
 0.19888572877782E+03 0.43215527675042E+03 0.23227511932871E+03 0.19369463600524E+03 0.18372627061232E+03
 0.19369463600524E+03 0.34391561461492E+03 0.19371616060283E+03 0.18357231473055E+03 0.19371616060283E+03
 0.34379126452165E+03 0.19369463600524E+03 0.18372627061232E+03 0.19369463600524E+03 0.34391561461492E+03
 0.19371616060283E+03 0.18357231473055E+03 0.19371616060283E+03 0.34379126452166E+03 0.19731912958187E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36959459858014E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16524876611840E+00 0.00000000000000E+00 0.00000000000000E+00 0.16524876611840E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17312721532178E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17312721532178E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158805434289E+00 0.17518403461982E+00 0.34477961854726E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    922.60585021
 0.86993809389438E-01 0.32988579737806E+03 0.46079211300032E+03 0.45699609997728E+03 0.45555007460524E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13762411893146E+00 0.00000000000000E+00 -.19829505470908E+02
 0.38715418376561E-02 0.12705933360215E+01 0.20663602087904E+04 0.77488507829641E+03 0.62962710201596E+01
 0.23611016325598E+01 0.36008419188454E+03 0.30615197405650E+03 0.35222208388975E+03 0.38091455902278E+03
 0.30615031085503E+03 0.30615057200046E+03 0.34830000614519E+03 0.38089627077183E+03 0.30615026106500E+03
 0.30615057070944E+03 0.35222208388975E+03 0.38091455902278E+03 0.30615031085503E+03 0.30615057200046E+03
 0.34830000614519E+03 0.38089627077184E+03 0.30615026106500E+03 0.30615057070944E+03 0.42455606765682E+03
 0.34776752333617E+03 0.20861060831134E+04 0.19301837266018E+04 0.59552886784709E+03 0.92300967499528E+03
 0.32450316280896E+03 0.12481343406456E+04 0.11866534339130E+04 0.11308735042525E+04 0.17465915663060E+04
 0.11452064618415E+04 0.11864625570897E+04 0.10532643078864E+04 0.17465582924825E+04 0.12481343406456E+04
 0.11866534339130E+04 0.11308735042525E+04 0.17465915663060E+04 0.11452064618415E+04 0.11864625570897E+04
 0.10532643078864E+04 0.17465582924825E+04 0.17214607485022E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50106750799770E+03 0.12948505920058E+01
 0.12948505920058E+01 0.35104406037641E+01 0.00000000000000E+00 0.34501525059844E+03 0.34501525059844E+03
 0.34501525059844E+03 0.34501525059844E+03 0.00000000000000E+00 0.00000000000000E+00 0.14780778751202E+00
 0.00000000000000E+00 -.11456645533031E+02 0.10000000000000E-02 0.15093445957982E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53003138065826E+01 0.19876176774685E+01 0.34777012810986E+03 0.42455331060971E+03
 0.31914849704011E+03 0.31914849704011E+03 0.30615007182804E+03 0.30615006981964E+03 0.31914448997771E+03
 0.31914448997771E+03 0.30615007184645E+03 0.30615006983754E+03 0.31914849704011E+03 0.31914849704011E+03
 0.30615007182804E+03 0.30615006981964E+03 0.31914448997771E+03 0.31914448997771E+03 0.30615007184645E+03
 0.30615006983754E+03 0.31722116887481E+03 0.30615031906165E+03 -.37742172404086E+02 -.44040998518009E+02
 0.20022380274239E+03 0.43455026221700E+03 0.23332534046089E+03 0.19625682229047E+03 0.18491408178989E+03
 0.19625682229047E+03 0.34573652534920E+03 0.19627866012943E+03 0.18475965636044E+03 0.19627866012943E+03
 0.34561210394709E+03 0.19625682229047E+03 0.18491408178989E+03 0.19625682229047E+03 0.34573652534920E+03
 0.19627866012943E+03 0.18475965636044E+03 0.19627866012943E+03 0.34561210394710E+03 0.19742825032427E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36985759955685E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16512655173095E+00 0.00000000000000E+00 0.00000000000000E+00 0.16512655173095E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17298861133418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17298861133418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158810778729E+00 0.17506447527333E+00 0.34501525059844E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    930.62572112
 0.86928994954039E-01 0.33004179139916E+03 0.46104579155015E+03 0.45724977619412E+03 0.45580331490687E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13743601600138E+00 0.00000000000000E+00 -.19833477707820E+02
 0.38744282206254E-02 0.12724829278183E+01 0.20648208056642E+04 0.77430780212409E+03 0.62869212820924E+01
 0.23575954807846E+01 0.36038421100509E+03 0.30615211688531E+03 0.35248516306461E+03 0.38127903622579E+03
 0.30615033651943E+03 0.30615061905895E+03 0.34855341656388E+03 0.38126083446244E+03 0.30615028282864E+03
 0.30615061767520E+03 0.35248516306461E+03 0.38127903622579E+03 0.30615033651943E+03 0.30615061905895E+03
 0.34855341656388E+03 0.38126083446244E+03 0.30615028282864E+03 0.30615061767520E+03 0.42495416298533E+03
 0.34817057512495E+03 0.20884530839691E+04 0.19316011489369E+04 0.59451438040113E+03 0.92039836445713E+03
 0.32291141215400E+03 0.12499266058807E+04 0.11877524710555E+04 0.11319601526087E+04 0.17465888489306E+04
 0.11471525333578E+04 0.11875627367370E+04 0.10545993304561E+04 0.17465558663235E+04 0.12499266058807E+04
 0.11877524710555E+04 0.11319601526087E+04 0.17465888489306E+04 0.11471525333578E+04 0.11875627367370E+04
 0.10545993304561E+04 0.17465558663235E+04 0.17212017032691E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50132041398876E+03 0.12948506212734E+01
 0.12948506212734E+01 0.35425200874335E+01 0.00000000000000E+00 0.34517205619230E+03 0.34517205619230E+03
 0.34517205619230E+03 0.34517205619230E+03 0.00000000000000E+00 0.00000000000000E+00 0.14742675397365E+00
 0.00000000000000E+00 -.11446786314357E+02 0.10000000000000E-02 0.15156758589165E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52781733989740E+01 0.19793150246152E+01 0.34817323172536E+03 0.42495135382812E+03
 0.31924550944350E+03 0.31924550944350E+03 0.30615007789063E+03 0.30615007571271E+03 0.31924146859970E+03
 0.31924146859970E+03 0.30615007791038E+03 0.30615007573191E+03 0.31924550944350E+03 0.31924550944350E+03
 0.30615007789063E+03 0.30615007571271E+03 0.31924146859970E+03 0.31924146859970E+03 0.30615007791038E+03
 0.30615007573191E+03 0.31730514715495E+03 0.30615034277629E+03 -.39793970598362E+02 -.46851466993397E+02
 0.20110937986204E+03 0.43613712314997E+03 0.23402219638862E+03 0.19795107619440E+03 0.18569934809980E+03
 0.19795107619440E+03 0.34694162728251E+03 0.19797312162983E+03 0.18554461632941E+03 0.19797312162983E+03
 0.34681716280984E+03 0.19795107619440E+03 0.18569934809980E+03 0.19795107619440E+03 0.34694162728251E+03
 0.19797312162983E+03 0.18554461632942E+03 0.19797312162983E+03 0.34681716280984E+03 0.19749610992561E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37003298486316E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16505820567714E+00 0.00000000000000E+00 0.00000000000000E+00 0.16505820567714E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17290483375937E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17290483375937E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158809470520E+00 0.17498495025457E+00 0.34517205619230E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    942.65552750
 0.86832605158731E-01 0.33027246531076E+03 0.46142479709504E+03 0.45762869754788E+03 0.45618151877568E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13716068885707E+00 0.00000000000000E+00 -.19840454438500E+02
 0.38787287180844E-02 0.12752237763546E+01 0.20625314584906E+04 0.77344929693397E+03 0.62734087525164E+01
 0.23525282821936E+01 0.36083222285162E+03 0.30615234655868E+03 0.35287819026481E+03 0.38182262143712E+03
 0.30615037828391E+03 0.30615069559614E+03 0.34893216187424E+03 0.38180454658489E+03 0.30615031827836E+03
 0.30615069406358E+03 0.35287819026481E+03 0.38182262143712E+03 0.30615037828391E+03 0.30615069559614E+03
 0.34893216187424E+03 0.38180454658489E+03 0.30615031827836E+03 0.30615069406358E+03 0.42554700232685E+03
 0.34876900075930E+03 0.20919534297205E+04 0.19336955999643E+04 0.59300256858812E+03 0.91652495082566E+03
 0.32055736939460E+03 0.12526024924995E+04 0.11893899627236E+04 0.11335643357550E+04 0.17465904382582E+04
 0.11500577037891E+04 0.11892018673200E+04 0.10565739358010E+04 0.17465578450366E+04 0.12526024924995E+04
 0.11893899627236E+04 0.11335643357551E+04 0.17465904382582E+04 0.11500577037891E+04 0.11892018673200E+04
 0.10565739358010E+04 0.17465578450366E+04 0.17208186463215E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50169814486991E+03 0.12948506726782E+01
 0.12948506726782E+01 0.35906393129375E+01 0.00000000000000E+00 0.34540772303655E+03 0.34540772303655E+03
 0.34540772303655E+03 0.34540772303655E+03 0.00000000000000E+00 0.00000000000000E+00 0.14686872862627E+00
 0.00000000000000E+00 -.11433196974914E+02 0.10000000000000E-02 0.15249106088611E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52462091571224E+01 0.19673284339209E+01 0.34877173152596E+03 0.42554412214438E+03
 0.31939115690357E+03 0.31939115690357E+03 0.30615008777724E+03 0.30615008532288E+03 0.31938706584261E+03
 0.31938706584261E+03 0.30615008779915E+03 0.30615008534417E+03 0.31939115690357E+03 0.31939115690357E+03
 0.30615008777724E+03 0.30615008532288E+03 0.31938706584261E+03 0.31938706584261E+03 0.30615008779915E+03
 0.30615008534417E+03 0.31743126655447E+03 0.30615038099484E+03 -.42841324799544E+02 -.51053306504914E+02
 0.20243558679431E+03 0.43851856088309E+03 0.23507079615481E+03 0.20047650589921E+03 0.18687489322922E+03
 0.20047650589921E+03 0.34874959304755E+03 0.20049886091766E+03 0.18671970815792E+03 0.20049886091766E+03
 0.34862506713184E+03 0.20047650589921E+03 0.18687489322922E+03 0.20047650589921E+03 0.34874959304755E+03
 0.20049886091767E+03 0.18671970815792E+03 0.20049886091767E+03 0.34862506713184E+03 0.19760159585514E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37029639288372E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16496540293334E+00 0.00000000000000E+00 0.00000000000000E+00 0.16496540293334E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17278261726492E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17278261726492E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158804024584E+00 0.17486552844089E+00 0.34540772303655E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    950.67539842
 0.86771264885000E-01 0.33042509689659E+03 0.46167632801299E+03 0.45788004956576E+03 0.45643234296916E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13698159337560E+00 0.00000000000000E+00 -.19845196957918E+02
 0.38814704222461E-02 0.12769904643431E+01 0.20610745747666E+04 0.77290296553747E+03 0.62647296306285E+01
 0.23492736114857E+01 0.36112952390382E+03 0.30615251045231E+03 0.35313911234714E+03 0.38218295845480E+03
 0.30615040843569E+03 0.30615075082095E+03 0.34918370695469E+03 0.38216496637139E+03 0.30615034389448E+03
 0.30615074918243E+03 0.35313911234714E+03 0.38218295845480E+03 0.30615040843569E+03 0.30615075082095E+03
 0.34918370695469E+03 0.38216496637139E+03 0.30615034389448E+03 0.30615074918243E+03 0.42593941699165E+03
 0.34916398325570E+03 0.20942700394054E+04 0.19350750212998E+04 0.59199839065847E+03 0.91396672211585E+03
 0.31900833950409E+03 0.12543763904652E+04 0.11904716506441E+04 0.11346229844744E+04 0.17465914051495E+04
 0.11519834299606E+04 0.11902845995193E+04 0.10578775209181E+04 0.17465590410052E+04 0.12543763904652E+04
 0.11904716506441E+04 0.11346229844744E+04 0.17465914051495E+04 0.11519834299606E+04 0.11902845995193E+04
 0.10578775209181E+04 0.17465590410052E+04 0.17205643199149E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50194867225292E+03 0.12948507076213E+01
 0.12948507076213E+01 0.36227187966069E+01 0.00000000000000E+00 0.34556514681042E+03 0.34556514681042E+03
 0.34556514681042E+03 0.34556514681042E+03 0.00000000000000E+00 0.00000000000000E+00 0.14650551815077E+00
 0.00000000000000E+00 -.11424332386027E+02 0.10000000000000E-02 0.15308964713680E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52256962829440E+01 0.19596361061040E+01 0.34916676092592E+03 0.42593649388520E+03
 0.31948834771521E+03 0.31948834771521E+03 0.30615009492948E+03 0.30615009227514E+03 0.31948422346585E+03
 0.31948422346585E+03 0.30615009495291E+03 0.30615009229791E+03 0.31948834771521E+03 0.31948834771521E+03
 0.30615009492948E+03 0.30615009227514E+03 0.31948422346585E+03 0.31948422346585E+03 0.30615009495291E+03
 0.30615009229791E+03 0.31751545175807E+03 0.30615040832635E+03 -.44853162950232E+02 -.53843489350899E+02
 0.20331858715618E+03 0.44010717064985E+03 0.23577199055789E+03 0.20214994321829E+03 0.18765728575153E+03
 0.20214994321829E+03 0.34995528525245E+03 0.20217250349366E+03 0.18750180125678E+03 0.20217250349366E+03
 0.34983071920458E+03 0.20214994321829E+03 0.18765728575153E+03 0.20214994321829E+03 0.34995528525245E+03
 0.20217250349366E+03 0.18750180125678E+03 0.20217250349366E+03 0.34983071920459E+03 0.19767492116174E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37047214071641E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16490498076192E+00 0.00000000000000E+00 0.00000000000000E+00 0.16490498076192E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17270237656045E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17270237656045E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158799808365E+00 0.17478584000466E+00 0.34556514681042E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    968.78316513
 0.86650885454116E-01 0.33075972270095E+03 0.46223927473384E+03 0.45844166819959E+03 0.45699233276987E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13659011694238E+00 0.00000000000000E+00 -.19865230741307E+02
 0.38868621984941E-02 0.12808090983770E+01 0.20582154940042E+04 0.77183081025159E+03 0.62460518200076E+01
 0.23422694325029E+01 0.36179501185030E+03 0.30615291904724E+03 0.35372321129787E+03 0.38299033194046E+03
 0.30615048492347E+03 0.30615089078844E+03 0.34974669787173E+03 0.38297252087670E+03 0.30615040896487E+03
 0.30615088888672E+03 0.35372321129787E+03 0.38299033194047E+03 0.30615048492347E+03 0.30615089078844E+03
 0.34974669787173E+03 0.38297252087670E+03 0.30615040896487E+03 0.30615088888672E+03 0.42682568862518E+03
 0.35005399709304E+03 0.20994328651164E+04 0.19380864893549E+04 0.58957769807736E+03 0.90798974685805E+03
 0.31546416029030E+03 0.12583425822341E+04 0.11928577418450E+04 0.11369418441722E+04 0.17465549124902E+04
 0.11562916902412E+04 0.11926728807146E+04 0.10607492685567E+04 0.17465229533691E+04 0.12583425822341E+04
 0.11928577418450E+04 0.11369418441722E+04 0.17465549124902E+04 0.11562916902412E+04 0.11926728807146E+04
 0.10607492685567E+04 0.17465229533691E+04 0.17198909207769E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50250816007205E+03 0.12948508552310E+01
 0.12948508552310E+01 0.36951498634705E+01 0.00000000000000E+00 0.34592133833077E+03 0.34592133833077E+03
 0.34592133833077E+03 0.34592133833077E+03 0.00000000000000E+00 0.00000000000000E+00 0.14571055939332E+00
 0.00000000000000E+00 -.11414697923210E+02 0.10000000000000E-02 0.15439258844152E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51815958788917E+01 0.19430984545844E+01 0.35005684496999E+03 0.42682270464697E+03
 0.31970615635335E+03 0.31970615635335E+03 0.30615011524972E+03 0.30615010996543E+03 0.31970195784694E+03
 0.31970195784694E+03 0.30615011527742E+03 0.30615010999186E+03 0.31970615635335E+03 0.31970615635335E+03
 0.30615011524972E+03 0.30615010996543E+03 0.31970195784694E+03 0.31970195784694E+03 0.30615011527742E+03
 0.30615010999186E+03 0.31770410407252E+03 0.30615047668423E+03 -.49424479110806E+02 -.60223464935112E+02
 0.20532236831408E+03 0.44372841147329E+03 0.23737943131765E+03 0.20594798706369E+03 0.18943320242992E+03
 0.20594798706369E+03 0.35270695596433E+03 0.20597100689398E+03 0.18927695180601E+03 0.20597100689398E+03
 0.35258220561432E+03 0.20594798706369E+03 0.18943320242992E+03 0.20594798706369E+03 0.35270695596433E+03
 0.20597100689398E+03 0.18927695180601E+03 0.20597100689398E+03 0.35258220561433E+03 0.19783830391646E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37087044019350E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16485286730571E+00 0.00000000000000E+00 0.00000000000000E+00 0.16485286730571E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17255044543533E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17255044543533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158760411161E+00 0.17460548059410E+00 0.34592133833077E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    978.87106093
 0.86612190746784E-01 0.33093909524067E+03 0.46254942614690E+03 0.45874973978534E+03 0.45729893578643E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13637995439891E+00 0.00000000000000E+00 -.19881269488842E+02
 0.38885983911580E-02 0.12828332798422E+01 0.20572965359937E+04 0.77148620099763E+03 0.62361961805233E+01
 0.23385735676962E+01 0.36216320094532E+03 0.30615317073389E+03 0.35404659627250E+03 0.38343645277957E+03
 0.30615053287430E+03 0.30615097845237E+03 0.35005860459495E+03 0.38341873945751E+03 0.30615044981414E+03
 0.30615097638917E+03 0.35404659627250E+03 0.38343645277957E+03 0.30615053287430E+03 0.30615097845237E+03
 0.35005860459495E+03 0.38341873945751E+03 0.30615044981414E+03 0.30615097638917E+03 0.42731260170864E+03
 0.35054193618722E+03 0.21022370849181E+04 0.19396570697525E+04 0.58823774144512E+03 0.90470826849761E+03
 0.31352933834527E+03 0.12605130156327E+04 0.11941419195266E+04 0.11381663834867E+04 0.17464959879678E+04
 0.11586497817940E+04 0.11939582016389E+04 0.10622774127904E+04 0.17464642073203E+04 0.12605130156327E+04
 0.11941419195266E+04 0.11381663834867E+04 0.17464959879678E+04 0.11586497817940E+04 0.11939582016389E+04
 0.10622774127904E+04 0.17464642073203E+04 0.17194742820641E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50281468580827E+03 0.12948509734051E+01
 0.12948509734051E+01 0.37355014466646E+01 0.00000000000000E+00 0.34612131252262E+03 0.34612131252262E+03
 0.34612131252262E+03 0.34612131252262E+03 0.00000000000000E+00 0.00000000000000E+00 0.14528225159711E+00
 0.00000000000000E+00 -.11416174429694E+02 0.10000000000000E-02 0.15508980973652E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51583015116152E+01 0.19343630668557E+01 0.35054482477816E+03 0.42730958232561E+03
 0.31982781212147E+03 0.31982781212147E+03 0.30615012690881E+03 0.30615012108994E+03 0.31982357249551E+03
 0.31982357249551E+03 0.30615012693883E+03 0.30615012111859E+03 0.31982781212147E+03 0.31982781212147E+03
 0.30615012690881E+03 0.30615012108994E+03 0.31982357249551E+03 0.31982357249551E+03 0.30615012693883E+03
 0.30615012111859E+03 0.31780949968041E+03 0.30615051892821E+03 -.51918293977331E+02 -.63724943292453E+02
 0.20644455267758E+03 0.44576936728689E+03 0.23829259184593E+03 0.20804599107478E+03 0.19042812113742E+03
 0.20804599107478E+03 0.35425974880939E+03 0.20806926668805E+03 0.19027146260532E+03 0.20806926668805E+03
 0.35413491425446E+03 0.20804599107478E+03 0.19042812113742E+03 0.20804599107478E+03 0.35425974880939E+03
 0.20806926668805E+03 0.19027146260532E+03 0.20806926668805E+03 0.35413491425447E+03 0.19795360625489E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37109704325946E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16486884800586E+00 0.00000000000000E+00 0.00000000000000E+00 0.16486884800586E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17253393891497E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17253393891497E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158718562749E+00 0.17450417282192E+00 0.34612131252262E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    980.03116895
 0.86602258868167E-01 0.33096513688103E+03 0.46258563100622E+03 0.45878608697136E+03 0.45733533258736E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13635615473677E+00 0.00000000000000E+00 -.19851910652179E+02
 0.38890443149753E-02 0.12830597321814E+01 0.20570606432009E+04 0.77139774120035E+03 0.62350955293398E+01
 0.23381608235024E+01 0.36220611707447E+03 0.30615319978852E+03 0.35408446179069E+03 0.38348715803831E+03
 0.30615053841333E+03 0.30615098857850E+03 0.35009540787138E+03 0.38346945576622E+03 0.30615045453308E+03
 0.30615098649666E+03 0.35408446179068E+03 0.38348715803831E+03 0.30615053841333E+03 0.30615098857850E+03
 0.35009540787138E+03 0.38346945576622E+03 0.30615045453308E+03 0.30615098649666E+03 0.42736169670533E+03
 0.35059086816408E+03 0.21025659296007E+04 0.19398789547434E+04 0.58820193926359E+03 0.90452787456275E+03
 0.31338492560285E+03 0.12607652200566E+04 0.11943048150469E+04 0.11383404963335E+04 0.17465139851599E+04
 0.11589217254770E+04 0.11941212539364E+04 0.10624815689521E+04 0.17464822525697E+04 0.12607652200566E+04
 0.11943048150469E+04 0.11383404963335E+04 0.17465139851599E+04 0.11589217254770E+04 0.11941212539364E+04
 0.10624815689521E+04 0.17464822525697E+04 0.17194992302164E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50285098747413E+03 0.12948507570880E+01
 0.12948507570880E+01 0.37401418787319E+01 0.00000000000000E+00 0.34614428785938E+03 0.34614428785938E+03
 0.34614428785938E+03 0.34614428785938E+03 0.00000000000000E+00 0.00000000000000E+00 0.14523366383427E+00
 0.00000000000000E+00 -.11382236781804E+02 0.10000000000000E-02 0.15516877292302E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51556765251788E+01 0.19333786969421E+01 0.35059376144590E+03 0.42735867324081E+03
 0.31984305232602E+03 0.31984305232602E+03 0.30615012247807E+03 0.30615012237513E+03 0.31983880805780E+03
 0.31983880805780E+03 0.30615012250700E+03 0.30615012240404E+03 0.31984305232602E+03 0.31984305232602E+03
 0.30615012247807E+03 0.30615012237513E+03 0.31983880805780E+03 0.31983880805780E+03 0.30615012250700E+03
 0.30615012240404E+03 0.31782276862295E+03 0.30615052380543E+03 -.52141394979789E+02 -.64039810986306E+02
 0.20656432144534E+03 0.44597320861605E+03 0.23837606556349E+03 0.20825186537493E+03 0.19053366053171E+03
 0.20825186537493E+03 0.35441065491393E+03 0.20827517105658E+03 0.19037703220399E+03 0.20827517105658E+03
 0.35428588529688E+03 0.20825186537493E+03 0.19053366053171E+03 0.20825186537493E+03 0.35441065491392E+03
 0.20827517105658E+03 0.19037703220399E+03 0.20827517105658E+03 0.35428588529689E+03 0.19797223955127E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37111650437182E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16459781143047E+00 0.00000000000000E+00 0.00000000000000E+00 0.16459781143047E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17241756387408E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17241756387408E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158811788602E+00 0.17449359661746E+00 0.34614428785938E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
    992.14716322
 0.86486666263525E-01 0.33121071538661E+03 0.46296069198724E+03 0.45916248656841E+03 0.45771181740568E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13611112520152E+00 0.00000000000000E+00 -.19867791040395E+02
 0.38942417982909E-02 0.12853787167145E+01 0.20543151695180E+04 0.77036818856924E+03 0.62238466344367E+01
 0.23339424879138E+01 0.36264868570467E+03 0.30615351718434E+03 0.35447385816142E+03 0.38401952988789E+03
 0.30615059939302E+03 0.30615110001019E+03 0.35047171657260E+03 0.38400194267107E+03 0.30615050651563E+03
 0.30615109772511E+03 0.35447385816142E+03 0.38401952988789E+03 0.30615059939302E+03 0.30615110001019E+03
 0.35047171657260E+03 0.38400194267107E+03 0.30615050651563E+03 0.30615109772511E+03 0.42793138347705E+03
 0.35115727136033E+03 0.21060130240431E+04 0.19420570688874E+04 0.58685147159759E+03 0.90103804853864E+03
 0.31125231958307E+03 0.12634071402977E+04 0.11959157907064E+04 0.11400231062344E+04 0.17465631943243E+04
 0.11617867140843E+04 0.11957335727885E+04 0.10645156601737E+04 0.17465316708384E+04 0.12634071402977E+04
 0.11959157907064E+04 0.11400231062344E+04 0.17465631943243E+04 0.11617867140843E+04 0.11957335727885E+04
 0.10645156601737E+04 0.17465316708384E+04 0.17192594402313E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50322669010265E+03 0.12948508740954E+01
 0.12948508740954E+01 0.37886058558047E+01 0.00000000000000E+00 0.34638191014022E+03 0.34638191014022E+03
 0.34638191014022E+03 0.34638191014022E+03 0.00000000000000E+00 0.00000000000000E+00 0.14473434955447E+00
 0.00000000000000E+00 -.11377887765244E+02 0.10000000000000E-02 0.15597840397146E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51289151551158E+01 0.19233431831684E+01 0.35116032188472E+03 0.42792822534024E+03
 0.31999189018364E+03 0.31999189018364E+03 0.30615013922533E+03 0.30615013654312E+03 0.31998759752626E+03
 0.31998759752626E+03 0.30615013925763E+03 0.30615013657480E+03 0.31999189018364E+03 0.31999189018364E+03
 0.30615013922533E+03 0.30615013654312E+03 0.31998759752626E+03 0.31998759752626E+03 0.30615013925763E+03
 0.30615013657480E+03 0.31795192307421E+03 0.30615057716102E+03 -.55012045304704E+02 -.68088982191665E+02
 0.20786773821566E+03 0.44832150063101E+03 0.23941442372428E+03 0.21067383259243E+03 0.19168394678340E+03
 0.21067383259243E+03 0.35618458873908E+03 0.21069744286701E+03 0.19152693355519E+03 0.21069744286701E+03
 0.35605981322960E+03 0.21067383259243E+03 0.19168394678340E+03 0.21067383259243E+03 0.35618458873907E+03
 0.21069744286701E+03 0.19152693355519E+03 0.21069744286701E+03 0.35605981322960E+03 0.19809620798603E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37138129521076E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16458423076405E+00 0.00000000000000E+00 0.00000000000000E+00 0.16458423076405E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17230217181238E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17230217181238E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158779666761E+00 0.17437357290307E+00 0.34638191014022E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
   1000.00000000
 0.86425804165044E-01 0.33135754329665E+03 0.46320185547645E+03 0.45940360524154E+03 0.45795248610755E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13595590595201E+00 0.00000000000000E+00 -.19871984469060E+02
 0.38969839372253E-02 0.12868359778802E+01 0.20528696368444E+04 0.76982611381664E+03 0.62167985178487E+01
 0.23312994441933E+01 0.36293387263703E+03 0.30615373663446E+03 0.35472476706427E+03 0.38436297477849E+03
 0.30615064202500E+03 0.30615117786527E+03 0.35071414645707E+03 0.38434546053193E+03 0.30615054288892E+03
 0.30615117544006E+03 0.35472476706426E+03 0.38436297477849E+03 0.30615064202500E+03 0.30615117786527E+03
 0.35071414645707E+03 0.38434546053193E+03 0.30615054288892E+03 0.30615117544006E+03 0.42830139338769E+03
 0.35152448408246E+03 0.21082130710694E+04 0.19433563051823E+04 0.58591126477304E+03 0.89868460810516E+03
 0.30984378700826E+03 0.12651015234648E+04 0.11969322848216E+04 0.11410245874366E+04 0.17465669400215E+04
 0.11636253143354E+04 0.11967508885841E+04 0.10657476987251E+04 0.17465355181581E+04 0.12651015234648E+04
 0.11969322848216E+04 0.11410245874367E+04 0.17465669400215E+04 0.11636253143354E+04 0.11967508885841E+04
 0.10657476987251E+04 0.17465355181581E+04 0.17190352930004E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50346704982006E+03 0.12948509049927E+01
 0.12948509049927E+01 0.38200172029388E+01 0.00000000000000E+00 0.34653589750973E+03 0.34653589750973E+03
 0.34653589750973E+03 0.34653589750973E+03 0.00000000000000E+00 0.00000000000000E+00 0.14441837646962E+00
 0.00000000000000E+00 -.11369079993896E+02 0.10000000000000E-02 0.15648853255082E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51121956795155E+01 0.19170733798183E+01 0.35152755442308E+03 0.42829822792598E+03
 0.32008773721754E+03 0.32008773721754E+03 0.30615014863166E+03 0.30615014646742E+03 0.32008341332762E+03
 0.32008341332762E+03 0.30615014866574E+03 0.30615014650099E+03 0.32008773721754E+03 0.32008773721754E+03
 0.30615014863166E+03 0.30615014646742E+03 0.32008341332762E+03 0.32008341332762E+03 0.30615014866574E+03
 0.30615014650099E+03 0.31803508810920E+03 0.30615061412719E+03 -.56887413290556E+02 -.70742341698585E+02
 0.20871529143215E+03 0.44985132586752E+03 0.24009245797821E+03 0.21225202468562E+03 0.19243210436439E+03
 0.21225202468562E+03 0.35734118809247E+03 0.21227583163604E+03 0.19227481382853E+03 0.21227583163604E+03
 0.35721638021783E+03 0.21225202468562E+03 0.19243210436439E+03 0.21225202468562E+03 0.35734118809246E+03
 0.21227583163604E+03 0.19227481382853E+03 0.21227583163604E+03 0.35721638021783E+03 0.19817353568333E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37155241879515E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16452364586455E+00 0.00000000000000E+00 0.00000000000000E+00 0.16452364586455E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17222499333226E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17222499333226E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17158776098239E+00 0.17429606683018E+00 0.34653589750973E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30615000000000E+03
