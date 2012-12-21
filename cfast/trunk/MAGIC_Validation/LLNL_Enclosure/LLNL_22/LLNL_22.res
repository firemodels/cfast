#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-22 0 MONOZONE(1=OUI,0=NON)                                                                     
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
85.200000 0.004000                                                                                  
1000.000000 0.004000                                                                                
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
Plenum-LLNL22 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.183000                                                                                   
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
#CONDINIT 500.000000 10.000000 30.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-22           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-22           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-22           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-22           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-22           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-22           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-22           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-22           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-22           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-22           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-22           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-22           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-22           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-22           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-22           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-22           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-22           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-22           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL22     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL22     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL22     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL22     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL22     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL22     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL22     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL22     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL22     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL22     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL22     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL22     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL22     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL22     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL22     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL22     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL22     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL22     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.75682115215768E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.52332576667151E-03 0.52332576667151E-03 0.74123867489846E-03 0.74494486827295E-03
 0.00000000000000E+00 0.49591429408475E-03 0.58360897238787E-03 0.49591429408475E-03 0.58360897238787E-03
 0.49340394697718E-03 0.58360198928660E-03 0.49340394697718E-03 0.58360198928660E-03 0.49591429408475E-03
 0.58360897238787E-03 0.49591429408475E-03 0.58360897238787E-03 0.49340394691773E-03 0.58360198946497E-03
 0.49340394691773E-03 0.58360198946497E-03 0.60133104552457E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.64079984336615E-07 0.99966045117024E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.60942813574386E-03 0.60942813574386E-03
 0.74851732268090E-03 0.75225990929431E-03 0.00000000000000E+00 0.53270100094083E-03 0.58763354584929E-03
 0.53270100094083E-03 0.58763354584929E-03 0.52806453182567E-03 0.58762547538984E-03 0.52806453182567E-03
 0.58762547538984E-03 0.53270100094083E-03 0.58763354578984E-03 0.53270100094083E-03 0.58763354578984E-03
 0.52806453176621E-03 0.58762547544930E-03 0.52806453176621E-03 0.58762547544930E-03 0.48605905479402E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.48842620380140E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.48842620380140E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18299973220910E+00 0.21251505129577E+00 0.30315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     10.22352612
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15457817808124E+02
 0.99984744319953E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000168230E+03 0.30315000000000E+03 0.30315000232106E+03 0.30315000232106E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000230912E+03 0.30315000230912E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000232106E+03 0.30315000232106E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000230912E+03 0.30315000230912E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000818980E+03
 0.30315000675922E+03 0.53794458182128E-03 0.53794458182128E-03 0.70817479744640E-03 0.71171567143363E-03
 .00000000000000E+00 0.50393683783366E-03 0.59870557907167E-03 0.50393683783366E-03 0.59870557907167E-03
 0.50133189359683E-03 0.59876609381262E-03 0.50133189359683E-03 0.59876609381262E-03 0.50393683783366E-03
 0.59870557907167E-03 0.50393683783366E-03 0.59870557907167E-03 0.50133189282388E-03 0.59876609298021E-03
 0.50133189282388E-03 0.59876609298021E-03 0.60165537677178E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.22999999930921E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17085032996240E+02 0.99950975034833E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315000673395E+03 0.30315000822056E+03
 0.30315000249255E+03 0.30315000249255E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000247073E+03
 0.30315000247073E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000249255E+03 0.30315000249255E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000247073E+03 0.30315000247073E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000240098E+03 0.30315000000000E+03 0.58630699604264E-03 0.58630699604264E-03
 0.76206182186151E-03 0.76587213097082E-03 .00000000000000E+00 0.54113744750933E-03 0.59459933625884E-03
 0.54113744750933E-03 0.59459933625884E-03 0.53638933546279E-03 0.59471518651959E-03 0.53638933546279E-03
 0.59471518651959E-03 0.54113744750933E-03 0.59459933619939E-03 0.54113744750933E-03 0.59459933619939E-03
 0.53638933552225E-03 0.59471518657905E-03 0.53638933552225E-03 0.59471518657905E-03 0.48629445129401E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21187338454029E+00 0.00000000000000E+00 0.00000000000000E+00 0.21187338454029E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324804685E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324804685E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18247776054897E+00 0.21187314944371E+00 0.30315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     30.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15457817749674E+02
 0.99984744320010E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000292006E+03 0.30315000000000E+03 0.30315000400077E+03 0.30315000400077E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000397998E+03 0.30315000397998E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000400077E+03 0.30315000400077E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000397998E+03 0.30315000397998E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001361788E+03
 0.30315001126873E+03 0.54726220822527E-03 0.54726220822527E-03 0.68669963734262E-03 0.69013313552933E-03
 .00000000000000E+00 0.50878400631958E-03 0.60815030473265E-03 0.50878400631958E-03 0.60815030473265E-03
 0.50612614374686E-03 0.60826098266932E-03 0.50612614374686E-03 0.60826098266932E-03 0.50878400626013E-03
 0.60815030473265E-03 0.50878400626013E-03 0.60815030473265E-03 0.50612614249826E-03 0.60826098159909E-03
 0.50612614249826E-03 0.60826098159909E-03 0.60160739911487E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.22999999937352E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17085032935681E+02 0.99954128383713E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001124798E+03 0.30315001364296E+03
 0.30315000429595E+03 0.30315000429595E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000425819E+03
 0.30315000425819E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000429595E+03 0.30315000429595E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000425819E+03 0.30315000425819E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000414309E+03 0.30315000000000E+03 0.57135830301066E-03 0.57135830301066E-03
 0.77040731641657E-03 0.77425935299865E-03 .00000000000000E+00 0.54629839790841E-03 0.59886368504161E-03
 0.54629839790841E-03 0.59886368504161E-03 0.54148813247310E-03 0.59907007243786E-03 0.54148813247310E-03
 0.59907007243786E-03 0.54629839790841E-03 0.59886368504161E-03 0.54629839790841E-03 0.59886368504161E-03
 0.54148813241364E-03 0.59907007243786E-03 0.54148813241364E-03 0.59907007243786E-03 0.48626052495895E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21187338413968E+00 0.00000000000000E+00 0.00000000000000E+00 0.21187338413968E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324790870E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324790870E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18247776055082E+00 0.21187314944599E+00 0.30315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     40.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15457817748838E+02
 0.99984744320011E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000340967E+03 0.30315000000000E+03 0.30315000466013E+03 0.30315000466013E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000463584E+03 0.30315000463584E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000466013E+03 0.30315000466013E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000463584E+03 0.30315000463584E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001565427E+03
 0.30315001296582E+03 0.55073106721633E-03 0.55073106721633E-03 0.67880452908653E-03 0.68219855173196E-03
 .00000000000000E+00 0.51055943147191E-03 0.61161675617191E-03 0.51055943147191E-03 0.61161675617191E-03
 0.50788401865991E-03 0.61174723921288E-03 0.50788401865991E-03 0.61174723921288E-03 0.51055943141245E-03
 0.61161675617191E-03 0.51055943141245E-03 0.61161675617191E-03 0.50788401782751E-03 0.61174723849939E-03
 0.50788401782751E-03 0.61174723849939E-03 0.60159666824527E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.22999999940367E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17085032934825E+02 0.99955606657689E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001295022E+03 0.30315001567303E+03
 0.30315000500389E+03 0.30315000500389E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000495987E+03
 0.30315000495987E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000500389E+03 0.30315000500389E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000495987E+03 0.30315000495987E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000482841E+03 0.30315000000000E+03 0.56588170468524E-03 0.56588170468524E-03
 0.77348485988002E-03 0.77735228417942E-03 .00000000000000E+00 0.54820161119196E-03 0.60043612804581E-03
 0.54820161119196E-03 0.60043612804581E-03 0.54337136679871E-03 0.60067806405155E-03 0.54337136679871E-03
 0.60067806405155E-03 0.54820161113250E-03 0.60043612804581E-03 0.54820161113250E-03 0.60043612804581E-03
 0.54337136679871E-03 0.60067806405155E-03 0.54337136679871E-03 0.60067806405155E-03 0.48625701135257E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21187338413400E+00 0.00000000000000E+00 0.00000000000000E+00 0.21187338413400E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324790679E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324790679E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18247776055085E+00 0.21187314944602E+00 0.30315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     40.00025000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15457817748832E+02
 0.99984744320011E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000340969E+03 0.30315000000000E+03 0.30315000466015E+03 0.30315000466015E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000463586E+03 0.30315000463586E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000466015E+03 0.30315000466015E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000463586E+03 0.30315000463586E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001565432E+03
 0.30315001296587E+03 0.55073790274416E-03 0.55073790274416E-03 0.67881400932840E-03 0.68220807937505E-03
 .00000000000000E+00 0.51056687144437E-03 0.61162219147637E-03 0.51056687144437E-03 0.61162219147637E-03
 0.50789151731689E-03 0.61175267487408E-03 0.50789151731689E-03 0.61175267487408E-03 0.51056687150383E-03
 0.61162219147637E-03 0.51056687150383E-03 0.61162219147637E-03 0.50789151648449E-03 0.61175267416060E-03
 0.50789151648449E-03 0.61175267416060E-03 0.60160507349545E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.22999999940367E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17085032934648E+02 0.99955606694614E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001295027E+03 0.30315001567308E+03
 0.30315000500390E+03 0.30315000500390E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000495989E+03
 0.30315000495989E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000500390E+03 0.30315000500390E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000495989E+03 0.30315000495989E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000482843E+03 0.30315000000000E+03 0.56589050070418E-03 0.56589050070418E-03
 0.77350290383134E-03 0.77737041835049E-03 .00000000000000E+00 0.54821400759843E-03 0.60044609882540E-03
 0.54821400759843E-03 0.60044609882540E-03 0.54338396928477E-03 0.60068803548517E-03 0.54338396928477E-03
 0.60068803548517E-03 0.54821400771734E-03 0.60044609888486E-03 0.54821400771734E-03 0.60044609888486E-03
 0.54338396910639E-03 0.60068803548517E-03 0.54338396910639E-03 0.60068803548517E-03 0.48626721825501E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21187338413390E+00 0.00000000000000E+00 0.00000000000000E+00 0.21187338413390E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324789641E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21187324789641E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18247776055086E+00 0.21187314944603E+00 0.30315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     50.01173286
 0.22255888243539E+01 0.30318166740130E+03 0.33899943273254E+03 0.31242755998770E+03 0.31168251664075E+03
 0.22999999998816E+00 0.00000000000000E+00 0.22593640500458E+00 0.00000000000000E+00 0.21366860371346E+02
 0.10001064022393E-02 0.71893801596730E-01 0.79991488726472E+04 0.29996808272427E+04 0.11127523962182E+03
 0.41728214858184E+02 0.30448180924196E+03 0.30315000000002E+03 0.30430483880806E+03 0.30472572987516E+03
 0.30315000000002E+03 0.30315000000002E+03 0.30402976633171E+03 0.30471359105281E+03 0.30315000000002E+03
 0.30315000000002E+03 0.30430483880806E+03 0.30472572987516E+03 0.30315000000002E+03 0.30315000000002E+03
 0.30402976633171E+03 0.30471359105281E+03 0.30315000000002E+03 0.30315000000002E+03 0.30745042628558E+03
 0.30315463553863E+03 0.61173637796447E+03 0.60920106037124E+03 0.28573546452032E+03 0.58824178722165E+03
 0.30107764537873E+03 0.39140597184023E+03 0.19289051275207E+03 0.38969969381030E+03 0.50470539268876E+03
 0.29456810288985E+03 0.18818879796407E+03 0.29338651867849E+03 0.50013196799626E+03 0.39140597184023E+03
 0.19289051275207E+03 0.38969969381030E+03 0.50470539268876E+03 0.29456810288985E+03 0.18818879796407E+03
 0.29338651867849E+03 0.50013196799626E+03 0.52158682041103E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.36430393361485E+03 0.12945471420347E+01
 0.12945471420347E+01 0.20062266300037E-01 0.13143907282980E+01 0.30316666898661E+03 0.30934838011796E+03
 0.30393159092058E+03 0.30391816369123E+03 0.22999999935075E+00 0.00000000000000E+00 0.22919919050104E+00
 0.00000000000000E+00 0.19877400063347E+02 0.99986567016594E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315462499610E+03 0.30746101457512E+03
 0.30315225147772E+03 0.30333219934880E+03 0.30315000000002E+03 0.30315000000002E+03 0.30315226844790E+03
 0.30333220967395E+03 0.30315000000002E+03 0.30315000000002E+03 0.30315225147772E+03 0.30333219934880E+03
 0.30315000000002E+03 0.30315000000002E+03 0.30315226844790E+03 0.30333220967395E+03 0.30315000000002E+03
 0.30315000000002E+03 0.30322440120921E+03 0.30315000000002E+03 0.65336497429779E+00 0.65602412381382E+00
 0.51546186955202E+00 0.35932738663540E+02 0.35414699484640E+02 0.76890385409423E+00 -.31389012080898E+00
 0.77537340908664E+00 0.51001358533847E+02 0.77410811547880E+00 -.31009917718232E+00 0.78056796088259E+00
 0.51005051911443E+02 0.76890385409417E+00 -.31389012080898E+00 0.77537340908658E+00 0.51001358533847E+02
 0.77410811547892E+00 -.31009917718243E+00 0.78056796088271E+00 0.51005051911443E+02 0.10878997851263E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31562317278657E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23938697437617E+00 0.00000000000000E+00 0.00000000000000E+00 0.23938697437617E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19552116159304E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19552116159304E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18360510386008E+00 0.21324817754221E+00 0.30316666898661E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     60.00647734
 0.16702038089384E+01 0.30322882613687E+03 0.36413934778621E+03 0.33022835269849E+03 0.32751262244368E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22272043374174E+00 0.00000000000000E+00 0.11321740861335E+02
 0.99985175190037E-03 0.12491919753776E+00 0.80000000000000E+04 0.30000000000000E+04 0.64041397620904E+02
 0.24015524107839E+02 0.30561558562428E+03 0.30315000000004E+03 0.30550985520890E+03 0.30681008877701E+03
 0.30315000000003E+03 0.30315000000003E+03 0.30492929736969E+03 0.30678268474999E+03 0.30315000000003E+03
 0.30315000000003E+03 0.30550985520890E+03 0.30681008877701E+03 0.30315000000003E+03 0.30315000000003E+03
 0.30492929736969E+03 0.30678268474999E+03 0.30315000000003E+03 0.30315000000003E+03 0.31323289112372E+03
 0.30316331387827E+03 0.65557434904649E+03 0.64987981862414E+03 0.30622519004749E+03 0.82749277973287E+03
 0.51973646373515E+03 0.44755959630735E+03 0.26186798652783E+03 0.44315754991726E+03 0.74367479537150E+03
 0.33644769807330E+03 0.25637920396076E+03 0.33344899649728E+03 0.73846289433810E+03 0.44755959630735E+03
 0.26186798652783E+03 0.44315754991726E+03 0.74367479537150E+03 0.33644769807330E+03 0.25637920396076E+03
 0.33344899649729E+03 0.73846289433810E+03 0.68517597843171E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39061700617273E+03 0.12946211178631E+01
 0.12946211178631E+01 0.60041244226086E-01 0.11272674449632E+01 0.30315944432219E+03 0.31393496627922E+03
 0.30583703620945E+03 0.30576737375760E+03 0.22999999936955E+00 0.00000000000000E+00 0.22851391489082E+00
 0.00000000000000E+00 0.11399247424348E+02 0.99980582258932E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30316326735530E+03 0.31327067724253E+03
 0.30315581386703E+03 0.30353332203387E+03 0.30315000000003E+03 0.30315000000003E+03 0.30315582261024E+03
 0.30353337756254E+03 0.30315000000003E+03 0.30315000000003E+03 0.30315581386703E+03 0.30353332203387E+03
 0.30315000000003E+03 0.30315000000003E+03 0.30315582261024E+03 0.30353337756254E+03 0.30315000000003E+03
 0.30315000000003E+03 0.30336269618741E+03 0.30315000000004E+03 0.12403177855398E+01 0.12390506521561E+01
 0.18261545592895E+00 0.70382490849448E+02 0.70198962316239E+02 0.12724309352320E+01 -.84237468541843E+00
 0.12735923104279E+01 0.75682689585705E+02 0.12706772919857E+01 -.82673840817005E+00 0.12718351467700E+01
 0.75697859738933E+02 0.12724309352320E+01 -.84237468541837E+00 0.12735923104279E+01 0.75682689585706E+02
 0.12706772919858E+01 -.82673840817005E+00 0.12718351467701E+01 0.75697859738933E+02 0.23118480131919E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32417270499877E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.18056289871798E+00 0.00000000000000E+00 0.00000000000000E+00 0.18056289871798E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18325542600520E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18325542600520E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18334714416014E+00 0.21293582403898E+00 0.30315944432219E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     70.02529453
 0.12844845805736E+01 0.30331950877340E+03 0.37863010812649E+03 0.34638500691889E+03 0.34224675364381E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22044913945273E+00 0.00000000000000E+00 0.42483845529137E+01
 0.99948305926241E-03 0.15871182776036E+00 0.80000000000000E+04 0.30000000000000E+04 0.50405821121784E+02
 0.18902182920669E+02 0.30650208278204E+03 0.30315000000005E+03 0.30645549585263E+03 0.30878910141537E+03
 0.30315000000004E+03 0.30315000000004E+03 0.30565399251403E+03 0.30875254455124E+03 0.30315000000004E+03
 0.30315000000004E+03 0.30645549585263E+03 0.30878910141537E+03 0.30315000000004E+03 0.30315000000004E+03
 0.30565399251403E+03 0.30875254455124E+03 0.30315000000004E+03 0.30315000000004E+03 0.31831859029296E+03
 0.30317550761358E+03 0.71467128231426E+03 0.70631937375112E+03 0.33926617416682E+03 0.96755978004979E+03
 0.62659727501213E+03 0.49514728277948E+03 0.33359951301348E+03 0.48833962500522E+03 0.90615886629243E+03
 0.37680577950363E+03 0.32815136041417E+03 0.37217398017737E+03 0.90107591190423E+03 0.49514728277948E+03
 0.33359951301348E+03 0.48833962500522E+03 0.90615886629243E+03 0.37680577950363E+03 0.32815136041417E+03
 0.37217398017737E+03 0.90107591190423E+03 0.82545521281136E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40246260995259E+03 0.20546147716542E+01
 0.20546147716542E+01 0.10011651297709E+00 0.96488286108187E+00 0.30315468151251E+03 0.31743715348819E+03
 0.30824987853938E+03 0.30810000762055E+03 0.22999999936042E+00 0.00000000000000E+00 0.22788788308184E+00
 0.00000000000000E+00 0.56420424766583E+01 0.99976470840617E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30317543483626E+03 0.31835819550444E+03
 0.30316043849281E+03 0.30373630768454E+03 0.30315000000004E+03 0.30315000000004E+03 0.30316041455411E+03
 0.30373643516760E+03 0.30315000000004E+03 0.30315000000004E+03 0.30316043849281E+03 0.30373630768454E+03
 0.30315000000004E+03 0.30315000000004E+03 0.30316041455411E+03 0.30373643516760E+03 0.30315000000004E+03
 0.30315000000004E+03 0.30352538303745E+03 0.30315000000005E+03 0.19878779442895E+01 0.19773783349048E+01
 -.10699513153102E+00 0.98748590167032E+02 0.98856120274221E+02 0.19177755115625E+01 -.12295917071309E+01
 0.19156270253051E+01 0.93651187124043E+02 0.19084763677393E+01 -.12020408147563E+01 0.19063391231007E+01
 0.93677713674429E+02 0.19177755115625E+01 -.12295917071318E+01 0.19156270253050E+01 0.93651187124041E+02
 0.19084763677393E+01 -.12020408147563E+01 0.19063391231007E+01 0.93677713674429E+02 0.34567193285402E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33043802130582E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.13380856637605E+00 0.00000000000000E+00 0.00000000000000E+00 0.13380856637605E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17911219109111E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17911219109111E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18317176563545E+00 0.21015979965369E+00 0.30685265740482E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     80.16509813
 0.97830722527838E+00 0.30346570850999E+03 0.38896386686717E+03 0.36108271151090E+03 0.35623453340205E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21847548500771E+00 0.00000000000000E+00 -.72631866879154E+00
 0.99895249597132E-03 0.18690441745024E+00 0.80000000000000E+04 0.30000000000000E+04 0.42802626653432E+02
 0.16050984995037E+02 0.30745194935541E+03 0.30315000000006E+03 0.30734956869725E+03 0.31069985550973E+03
 0.30315000000004E+03 0.30315000000004E+03 0.30636285742585E+03 0.31065805615260E+03 0.30315000000004E+03
 0.30315000000004E+03 0.30734956869725E+03 0.31069985550973E+03 0.30315000000004E+03 0.30315000000004E+03
 0.30636285742585E+03 0.31065805615260E+03 0.30315000000004E+03 0.30315000000004E+03 0.32294031957338E+03
 0.30319111755747E+03 0.82176096211722E+03 0.81049391665126E+03 0.36320195819373E+03 0.10597533225144E+04
 0.69473535452967E+03 0.54166413746105E+03 0.39462433117250E+03 0.53246104396766E+03 0.10280481569572E+04
 0.41944645594820E+03 0.38974188528427E+03 0.41316274868733E+03 0.10235810953767E+04 0.54166413746105E+03
 0.39462433117250E+03 0.53246104396766E+03 0.10280481569572E+04 0.41944645594820E+03 0.38974188528427E+03
 0.41316274868733E+03 0.10235810953767E+04 0.95866162806695E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41324671844308E+03 0.17969522482408E+01
 0.17969522482408E+01 0.14067572737000E+00 0.85035004563144E+00 0.30315176759241E+03 0.32040118602154E+03
 0.31062249018932E+03 0.31038903920752E+03 0.22999999935852E+00 0.00000000000000E+00 0.22726172226651E+00
 0.00000000000000E+00 0.18195508763334E+01 0.99973659097342E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30319102154514E+03 0.32297379821408E+03
 0.30316616100460E+03 0.30394615792992E+03 0.30315000000004E+03 0.30315000000004E+03 0.30316608660293E+03
 0.30394637422974E+03 0.30315000000004E+03 0.30315000000004E+03 0.30316616100460E+03 0.30394615792992E+03
 0.30315000000004E+03 0.30315000000004E+03 0.30316608660293E+03 0.30394637422974E+03 0.30315000000004E+03
 0.30315000000004E+03 0.30370248313052E+03 0.30315000000006E+03 0.27971491208077E+01 0.27738591913582E+01
 -.43622319751121E+00 0.12353112746882E+03 0.12396953178232E+03 0.25980655426579E+01 -.16062970339204E+01
 0.25910508041166E+01 0.11009482129848E+03 0.25803832785758E+01 -.15670585925650E+01 0.25734143428644E+01
 0.11013233921896E+03 0.25980655426580E+01 -.16062970339221E+01 0.25910508041167E+01 0.11009482129848E+03
 0.25803832785758E+01 -.15670585925650E+01 0.25734143428644E+01 0.11013233921896E+03 0.45059226066875E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33541101404127E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.93535449271062E-01 0.00000000000000E+00 0.00000000000000E+00 0.93535449271062E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17693134325206E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17693134325206E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18305522974421E+00 0.20677479022631E+00 0.31166581254603E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
     90.04387077
 0.74545717656193E+00 0.30368011132641E+03 0.39713156408103E+03 0.37391021204234E+03 0.36892143690610E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21660932103508E+00 0.00000000000000E+00 -.40134145982175E+01
 0.99821483570277E-03 0.21295981656579E+00 0.80000000000000E+04 0.30000000000000E+04 0.37565772402553E+02
 0.14087164650957E+02 0.30843890342295E+03 0.30315000000007E+03 0.30820883532360E+03 0.31253125303215E+03
 0.30315000000005E+03 0.30315000000005E+03 0.30706527471216E+03 0.31248662888278E+03 0.30315000000005E+03
 0.30315000000005E+03 0.30820883532360E+03 0.31253125303215E+03 0.30315000000005E+03 0.30315000000005E+03
 0.30706527471216E+03 0.31248662888278E+03 0.30315000000005E+03 0.30315000000005E+03 0.32718905864017E+03
 0.30320990735899E+03 0.92601032560827E+03 0.91175515026215E+03 0.39345228879772E+03 0.11383934406910E+04
 0.74297389044933E+03 0.58779621095052E+03 0.45579575643504E+03 0.57627547703358E+03 0.11337967716042E+04
 0.46320486784109E+03 0.45143112664821E+03 0.45530986160661E+03 0.11298736075799E+04 0.58779621095052E+03
 0.45579575643504E+03 0.57627547703358E+03 0.11337967716042E+04 0.46320486784109E+03 0.45143112664821E+03
 0.45530986160661E+03 0.11298736075799E+04 0.10812705045719E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42362255166860E+03 0.16165787277493E+01
 0.16165787277493E+01 0.18019081793138E+00 0.75715290408798E+00 0.30315038558604E+03 0.32296596286597E+03
 0.31296368161018E+03 0.31265023913540E+03 0.22999999935910E+00 0.00000000000000E+00 0.22662955773008E+00
 0.00000000000000E+00 -.50120497962493E+00 0.99971824307011E-03 0.20794993547529E-02 0.80000000000000E+04
 0.30000000000000E+04 0.38470798183781E+04 0.14426549318918E+04 0.30320979171988E+03 0.32721758171055E+03
 0.30317291450292E+03 0.30415609854199E+03 0.30315000000005E+03 0.30315000000005E+03 0.30317277621815E+03
 0.30415641230550E+03 0.30315000000005E+03 0.30315000000005E+03 0.30317291450292E+03 0.30415609854199E+03
 0.30315000000005E+03 0.30315000000005E+03 0.30317277621815E+03 0.30415641230550E+03 0.30315000000005E+03
 0.30315000000005E+03 0.30388548847709E+03 0.30315000000007E+03 0.38679568058606E+01 0.38288634816189E+01
 -.45120014116950E+00 0.14579854684449E+03 0.14625200298636E+03 0.34576667460527E+01 -.16544645079199E+01
 0.34450058408092E+01 0.12485414253700E+03 0.34287998631067E+01 -.16046950700599E+01 0.34162371809359E+01
 0.12490142205238E+03 0.34576667460527E+01 -.16544645079240E+01 0.34450058408092E+01 0.12485414253699E+03
 0.34287998631068E+01 -.16046950700573E+01 0.34162371809359E+01 0.12490142205239E+03 0.54982832287005E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33965217236554E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.58425025998877E-01 0.00000000000000E+00 0.00000000000000E+00 0.58425025998877E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17571361192636E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17571361192636E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18298443989396E+00 0.20577081462008E+00 0.31305817395922E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    100.27090362
 0.55476540299686E+00 0.30400545505163E+03 0.40422526002507E+03 0.38569243319361E+03 0.38099875586809E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21467042429523E+00 0.00000000000000E+00 -.60964239540301E+01
 0.99712605589413E-03 0.23968031009728E+00 0.80000000000000E+04 0.30000000000000E+04 0.33377793932063E+02
 0.12516672724524E+02 0.30950535030290E+03 0.30315000000007E+03 0.30910058259559E+03 0.31439840519635E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30781393815378E+03 0.31435217423336E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30910058259559E+03 0.31439840519635E+03 0.30315000000006E+03 0.30315000000006E+03
 0.30781393815378E+03 0.31435217423336E+03 0.30315000000006E+03 0.30315000000006E+03 0.33140229788996E+03
 0.30325490193548E+03 0.10333443295825E+04 0.10160753711495E+04 0.42863277191607E+03 0.12084507403844E+04
 0.77767480460874E+03 0.63626799668906E+03 0.51970705975229E+03 0.62243642134808E+03 0.12324386367106E+04
 0.51030497299483E+03 0.51581534643984E+03 0.50081506596764E+03 0.12290017683894E+04 0.63626799668906E+03
 0.51970705975229E+03 0.62243642134808E+03 0.12324386367106E+04 0.51030497299484E+03 0.51581534643984E+03
 0.50081506596765E+03 0.12290017683894E+04 0.11995978022632E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43320804980322E+03 0.14613244919071E+01
 0.14613244919071E+01 0.22109894935191E+00 0.67177538880497E+00 0.30315051557561E+03 0.32533790614000E+03
 0.31540127752469E+03 0.31501246797438E+03 0.22999999935873E+00 0.00000000000000E+00 0.22593742187456E+00
 0.00000000000000E+00 -.17379539652549E+01 0.99970560787116E-03 0.24536086289065E-01 0.80000000000000E+04
 0.30000000000000E+04 0.32605036947418E+03 0.12226888855282E+03 0.30325470211141E+03 0.33142898815879E+03
 0.30318663122447E+03 0.30438722653024E+03 0.30315000000006E+03 0.30315000000006E+03 0.30318627360314E+03
 0.30438764730913E+03 0.30315000000006E+03 0.30315000000006E+03 0.30318663122447E+03 0.30438722653024E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30318627360314E+03 0.30438764730913E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30408901563608E+03 0.30315000000007E+03 0.76310923101004E+01 0.75434315711701E+01
 0.29810039750143E+01 0.17017764710163E+03 0.16718173810674E+03 0.60153125339126E+01 0.17495496971452E+01
 0.59917399713383E+01 0.14191452630949E+03 0.59349403421630E+01 0.18090710570665E+01 0.59116628815004E+01
 0.14197070786591E+03 0.60153125339127E+01 0.17495496971435E+01 0.59917399713384E+01 0.14191452630948E+03
 0.59349403421629E+01 0.18090710570664E+01 0.59116628815004E+01 0.14197070786591E+03 0.66964919216730E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34357540536984E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.25370834172205E-01 0.00000000000000E+00 0.00000000000000E+00 0.25370834172205E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17547401309529E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17547401309529E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18287698435902E+00 0.20483492533825E+00 0.31430001701358E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    110.15940267
 0.40241809428043E+00 0.30445967982204E+03 0.40970958296072E+03 0.39559142781263E+03 0.39155281615854E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21282094538324E+00 0.00000000000000E+00 -.71769027811588E+01
 0.99562782140486E-03 0.26510807397522E+00 0.80000000000000E+04 0.30000000000000E+04 0.30176372526277E+02
 0.11316139697354E+02 0.31057894066894E+03 0.30315000000007E+03 0.30997282406315E+03 0.31617529919712E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30856332517024E+03 0.31612836603864E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30997282406315E+03 0.31617529919712E+03 0.30315000000006E+03 0.30315000000006E+03
 0.30856332517024E+03 0.31612836603864E+03 0.30315000000006E+03 0.30315000000006E+03 0.33527072059291E+03
 0.30332319391528E+03 0.11345673780623E+04 0.11146859912048E+04 0.46201324903339E+03 0.12611180743455E+04
 0.79679475906698E+03 0.68240301756920E+03 0.57830049575304E+03 0.66651358926296E+03 0.13152424546015E+04
 0.55594063535668E+03 0.57480760698296E+03 0.54508127706254E+03 0.13122094108968E+04 0.68240301756920E+03
 0.57830049575304E+03 0.66651358926296E+03 0.13152424546015E+04 0.55594063535668E+03 0.57480760698296E+03
 0.54508127706255E+03 0.13122094108968E+04 0.13025867906256E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44049689839262E+03 0.13398926689773E+01
 0.13398926689773E+01 0.26065294555666E+00 0.59853973833362E+00 0.30315248039320E+03 0.32733035972963E+03
 0.31768274534864E+03 0.31723459205366E+03 0.22999999872152E+00 0.00000000000000E+00 0.22523267827262E+00
 0.00000000000000E+00 -.21299728776919E+01 0.99969525935236E-03 0.44430618158660E-01 0.80000000000000E+04
 0.30000000000000E+04 0.18005601388286E+03 0.67521005206074E+02 0.30332281701133E+03 0.33529758093990E+03
 0.30320651416080E+03 0.30462140189511E+03 0.30315000000006E+03 0.30315000000006E+03 0.30320581662418E+03
 0.30462192739343E+03 0.30315000000006E+03 0.30315000000006E+03 0.30320651416080E+03 0.30462140189511E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30320581662418E+03 0.30462192739343E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30429619799185E+03 0.30315000000007E+03 0.11955175034431E+02 0.11786345786498E+02
 0.71101401416707E+01 0.19160586100608E+03 0.18446017016370E+03 0.89625220524809E+01 0.58152479029253E+01
 0.89223385194707E+01 0.15739999409500E+03 0.88276013744284E+01 0.58825615140353E+01 0.87880736511769E+01
 0.15746312977465E+03 0.89625220524809E+01 0.58152479029244E+01 0.89223385194706E+01 0.15739999409500E+03
 0.88276013744285E+01 0.58825615140354E+01 0.87880736511770E+01 0.15746312977465E+03 0.78258296551178E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34603679739179E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.64133295987726E-02 0.12309521010453E-01 0.00000000000000E+00 0.64133295987726E-02 0.12309521010453E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17583247463107E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17583247463107E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18286109924850E+00 0.20304298303851E+00 0.31704508202186E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    120.26444907
 0.28111529768669E+00 0.30510320439255E+03 0.41441475664880E+03 0.40417170679776E+03 0.40095377224123E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21090267959882E+00 0.00000000000000E+00 -.82430450633666E+01
 0.12184007024611E-02 0.29144200131787E+00 0.65659843956429E+04 0.24622441483661E+04 0.27449715428198E+02
 0.10293643285574E+02 0.31170595538742E+03 0.30315000000007E+03 0.31087315553398E+03 0.31795613354408E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30935202942059E+03 0.31790902492145E+03 0.30315000000006E+03
 0.30315000000006E+03 0.31087315553398E+03 0.31795613354408E+03 0.30315000000006E+03 0.30315000000006E+03
 0.30935202942059E+03 0.31790902492145E+03 0.30315000000006E+03 0.30315000000006E+03 0.33903273310218E+03
 0.30341473665159E+03 0.12323775690142E+04 0.12104132439304E+04 0.49538958534324E+03 0.13058960400213E+04
 0.80802950675135E+03 0.72809526593393E+03 0.63534342054862E+03 0.71043119093583E+03 0.13910533881563E+04
 0.60182006529688E+03 0.63219726514367E+03 0.58988948264933E+03 0.13883671724863E+04 0.72809526593393E+03
 0.63534342054862E+03 0.71043119093583E+03 0.13910533881563E+04 0.60182006529689E+03 0.63219726514367E+03
 0.58988948264933E+03 0.13883671724863E+04 0.13969844284134E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44731884035032E+03 0.12947652293710E+01
 0.12947652293710E+01 0.30107313112952E+00 0.54078644134155E+00 0.30315645310418E+03 0.32902207559954E+03
 0.31969689030464E+03 0.31920328484820E+03 0.22999999869808E+00 0.00000000000000E+00 0.22448977686150E+00
 0.00000000000000E+00 -.26169178538569E+01 0.99967735287625E-03 0.63750957392539E-01 0.80000000000000E+04
 0.30000000000000E+04 0.12548831150473E+03 0.47058116814275E+02 0.30341418935397E+03 0.33906078443300E+03
 0.30323268564954E+03 0.30486519220394E+03 0.30315000000006E+03 0.30315000000006E+03 0.30323154648071E+03
 0.30486582234179E+03 0.30315000000006E+03 0.30315000000006E+03 0.30323268564954E+03 0.30486519220394E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30323154648071E+03 0.30486582234179E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30451122257811E+03 0.30315000000007E+03 0.16796059685604E+02 0.16502805600542E+02
 0.11857966942722E+02 0.21084298865880E+03 0.19892573188137E+03 0.12274118712398E+02 0.10467981139896E+02
 0.12210703658872E+02 0.17170250606228E+03 0.12082044694600E+02 0.10541787590087E+02 0.12019827661221E+02
 0.17177128899285E+03 0.12274118712398E+02 0.10467981139896E+02 0.12210703658872E+02 0.17170250606228E+03
 0.12082044694600E+02 0.10541787590087E+02 0.12019827661221E+02 0.17177128899285E+03 0.88820561941396E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34798385461939E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.38008634984950E-01 0.00000000000000E+00 0.00000000000000E+00 0.38008634984950E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17631310242618E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17631310242618E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18284306522746E+00 0.19715986608299E+00 0.32647172271880E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    130.20937709
 0.19592909587458E+00 0.30590666798014E+03 0.41844115016027E+03 0.41109155704418E+03 0.40862371766604E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20895329410784E+00 0.00000000000000E+00 -.97846643833557E+01
 0.17481319375613E-02 0.31816669593947E+00 0.45763136226206E+04 0.17161176084827E+04 0.25144052165415E+02
 0.94290195620308E+01 0.31283000195196E+03 0.30315000000007E+03 0.31176523928234E+03 0.31967288933258E+03
 0.30315000000006E+03 0.30315000000006E+03 0.31014636610496E+03 0.31962593244286E+03 0.30315000000006E+03
 0.30315000000006E+03 0.31176523928234E+03 0.31967288933258E+03 0.30315000000006E+03 0.30315000000006E+03
 0.31014636610496E+03 0.31962593244286E+03 0.30315000000006E+03 0.30315000000006E+03 0.34257630122643E+03
 0.30352325805342E+03 0.13204601220429E+04 0.12971097337702E+04 0.52741182126573E+03 0.13439398061357E+04
 0.81389092576365E+03 0.77089048567555E+03 0.68908691588166E+03 0.75185293708681E+03 0.14580366322682E+04
 0.64523946186511E+03 0.68622443475350E+03 0.63263262223718E+03 0.14556305908608E+04 0.77089048567555E+03
 0.68908691588166E+03 0.75185293708681E+03 0.14580366322682E+04 0.64523946186511E+03 0.68622443475351E+03
 0.63263262223719E+03 0.14556305908608E+04 0.14785930472184E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45461452806732E+03 0.12947765863816E+01
 0.12947765863816E+01 0.34085284323897E+00 0.50531517182944E+00 0.30316276924503E+03 0.33043087370828E+03
 0.32124488244674E+03 0.32071309825271E+03 0.22999999804206E+00 0.00000000000000E+00 0.22372325422678E+00
 0.00000000000000E+00 -.37097700483151E+01 0.99964573959207E-03 0.82504372276518E-01 0.80000000000000E+04
 0.30000000000000E+04 0.96964558110782E+02 0.36361709291543E+02 0.30352257886391E+03 0.34260613339536E+03
 0.30326336299764E+03 0.30510774291121E+03 0.30315000000006E+03 0.30315000000006E+03 0.30326171131423E+03
 0.30510847326516E+03 0.30315000000006E+03 0.30315000000006E+03 0.30326336299764E+03 0.30510774291121E+03
 0.30315000000006E+03 0.30315000000006E+03 0.30326171131423E+03 0.30510847326516E+03 0.30315000000006E+03
 0.30315000000006E+03 0.30472336711829E+03 0.30315000000007E+03 0.21708928243847E+02 0.21251411178526E+02
 0.16810569289028E+02 0.22769241027786E+03 0.21079778814239E+03 0.15674652100878E+02 0.15303978871776E+02
 0.15583316412576E+02 0.18474104265399E+03 0.15425360285008E+02 0.15383206377980E+02 0.15335921086514E+02
 0.18481442272033E+03 0.15674652100878E+02 0.15303978871776E+02 0.15583316412576E+02 0.18474104265399E+03
 0.15425360285008E+02 0.15383206377980E+02 0.15335921086514E+02 0.18481442272033E+03 0.98069075039497E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34967862760208E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.67758818580937E-01 0.00000000000000E+00 0.00000000000000E+00 0.67758818580937E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17679634291455E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17679634291455E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18280687686225E+00 0.19475688361084E+00 0.33043087370828E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    140.33962607
 0.13879582830067E+00 0.30679519009498E+03 0.42203301293518E+03 0.41670150324429E+03 0.41482416761730E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20689499574497E+00 0.00000000000000E+00 -.11636875337936E+02
 0.24677183828678E-02 0.34637324942750E+00 0.32418610063208E+04 0.12156978773703E+04 0.23096471835578E+02
 0.86611769383419E+01 0.31397555499756E+03 0.30315000000007E+03 0.31267492253666E+03 0.32139192838807E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31096690615532E+03 0.32134532613122E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31267492253666E+03 0.32139192838807E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31096690615532E+03 0.32134532613122E+03 0.30315000000007E+03 0.30315000000007E+03 0.34603415706003E+03
 0.30365298463968E+03 0.14006232022963E+04 0.13761630076477E+04 0.55862770162669E+03 0.13762770834613E+04
 0.81485624332649E+03 0.81158387131785E+03 0.74109460104085E+03 0.79124839094827E+03 0.15216951033028E+04
 0.68674543531103E+03 0.73847319173715E+03 0.67357541224444E+03 0.15195255038734E+04 0.81158387131785E+03
 0.74109460104085E+03 0.79124839094827E+03 0.15216951033028E+04 0.68674543531103E+03 0.73847319173715E+03
 0.67357541224445E+03 0.15195255038734E+04 0.15515228777148E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46055463456132E+03 0.12947902318209E+01
 0.12947902318209E+01 0.38137383913915E+00 0.47135386192730E+00 0.30317297790762E+03 0.33165088499483E+03
 0.32270210400472E+03 0.32214218830711E+03 0.22999999803620E+00 0.00000000000000E+00 0.22289987427897E+00
 0.00000000000000E+00 -.52160342840758E+01 0.99959721329115E-03 0.10170895948419E+00 0.80000000000000E+04
 0.30000000000000E+04 0.78655804174692E+02 0.29495926565510E+02 0.30365219610453E+03 0.34606606283682E+03
 0.30329951011278E+03 0.30535523167718E+03 0.30315000000007E+03 0.30315000000007E+03 0.30329726937494E+03
 0.30535605886054E+03 0.30315000000007E+03 0.30315000000007E+03 0.30329951011278E+03 0.30535523167718E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30329726937494E+03 0.30535605886054E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30493899900901E+03 0.30315000000007E+03 0.26996769225424E+02 0.26326401491463E+02
 0.22255361799970E+02 0.24327566531879E+03 0.22090902670982E+03 0.19362926661372E+02 0.20595937124994E+02
 0.19239080822962E+02 0.19677914077736E+03 0.19055754409811E+02 0.20679177120692E+02 0.18934681934839E+02
 0.19685573898396E+03 0.19362926661372E+02 0.20595937124994E+02 0.19239080822962E+02 0.19677914077736E+03
 0.19055754409811E+02 0.20679177120694E+02 0.18934681934839E+02 0.19685573898396E+03 0.10699622471504E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35119051299843E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94431265044451E-01 0.00000000000000E+00 0.00000000000000E+00 0.94431265044451E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17727639920112E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17727639920112E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18275863390295E+00 0.19398635835963E+00 0.33165088499483E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    150.46987504
 0.10495685744431E+00 0.30756090445192E+03 0.42511265480489E+03 0.42100003403684E+03 0.41950317247342E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20477468708545E+00 0.00000000000000E+00 -.13567919539286E+02
 0.32633255036222E-02 0.37548218974130E+00 0.24514869850158E+04 0.91930761938093E+03 0.21305937321586E+02
 0.79897264955947E+01 0.31510786114304E+03 0.30315000000007E+03 0.31357745663168E+03 0.32307914127639E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31178894699668E+03 0.32303302529673E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31357745663168E+03 0.32307914127639E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31178894699668E+03 0.32303302529673E+03 0.30315000000007E+03 0.30315000000007E+03 0.34933260404613E+03
 0.30380314647221E+03 0.14702526366850E+04 0.14441642816187E+04 0.58729605170339E+03 0.14011027758428E+04
 0.81087024388093E+03 0.84846477061752E+03 0.78932076117423E+03 0.82631134716710E+03 0.15788971233532E+04
 0.72441324964187E+03 0.78690351502009E+03 0.71020156898440E+03 0.15769262027641E+04 0.84846477061752E+03
 0.78932076117423E+03 0.82631134716710E+03 0.15788971233532E+04 0.72441324964188E+03 0.78690351502009E+03
 0.71020156898441E+03 0.15769262027641E+04 0.16129731667917E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46509457092295E+03 0.12948044584051E+01
 0.12948044584051E+01 0.42189483503934E+00 0.43886803379960E+00 0.30318866333545E+03 0.33268184184101E+03
 0.32405276633352E+03 0.32347538442820E+03 0.22999999803184E+00 0.00000000000000E+00 0.22203088804682E+00
 0.00000000000000E+00 -.69016040358266E+01 0.99952886502978E-03 0.12119713385057E+00 0.80000000000000E+04
 0.30000000000000E+04 0.66008161627514E+02 0.24753060610318E+02 0.30380224790953E+03 0.34936649484251E+03
 0.30334060939183E+03 0.30560182533110E+03 0.30315000000007E+03 0.30315000000007E+03 0.30333772438736E+03
 0.30560274116708E+03 0.30315000000007E+03 0.30315000000007E+03 0.30334060939183E+03 0.30560182533110E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30333772438736E+03 0.30560274116708E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30515361820492E+03 0.30315000000007E+03 0.32509914733450E+02 0.31577983994020E+02
 0.28032736988401E+02 0.25742101379867E+03 0.22924811312533E+03 0.23245554110234E+02 0.26192510263081E+02
 0.23087157454267E+02 0.20770298721303E+03 0.22882127225653E+02 0.26278171325362E+02 0.22727532160648E+02
 0.20778128242679E+03 0.23245554110234E+02 0.26192510263081E+02 0.23087157454267E+02 0.20770298721303E+03
 0.22882127225653E+02 0.26278171325363E+02 0.22727532160648E+02 0.20778128242679E+03 0.11546027867194E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35250672386416E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11727443769159E+00 0.00000000000000E+00 0.00000000000000E+00 0.11727443769159E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17790223673533E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17790223673533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18270544639098E+00 0.19332571127504E+00 0.33268184184101E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    160.21551107
 0.88116674527088E-01 0.30806100480098E+03 0.42756158240063E+03 0.42405158456659E+03 0.42274489466186E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20270293991284E+00 0.00000000000000E+00 -.15311664906070E+02
 0.38869827151817E-02 0.40405209155364E+00 0.20581516786153E+04 0.77180687948074E+03 0.19799427269981E+02
 0.74247852262428E+01 0.31616949391585E+03 0.30315000000007E+03 0.31442751335212E+03 0.32465917247586E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31256801867228E+03 0.32461360367593E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31442751335212E+03 0.32465917247586E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31256801867228E+03 0.32461360367593E+03 0.30315000000007E+03 0.30315000000007E+03 0.35233281277549E+03
 0.30396723942237E+03 0.15269263516483E+04 0.14982601573430E+04 0.61125747696658E+03 0.14167160743519E+04
 0.80240231000047E+03 0.87961625236334E+03 0.83089702606289E+03 0.85489397643619E+03 0.16258510021756E+04
 0.75619106940678E+03 0.82865029012385E+03 0.74015867846902E+03 0.16240446394491E+04 0.87961625236334E+03
 0.83089702606289E+03 0.85489397643619E+03 0.16258510021756E+04 0.75619106940678E+03 0.82865029012385E+03
 0.74015867846903E+03 0.16240446394491E+04 0.16602884622170E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46828122546534E+03 0.12948173054285E+01
 0.12948173054285E+01 0.46087737914281E+00 0.40907739387523E+00 0.30321071801065E+03 0.33351531249611E+03
 0.32525069613974E+03 0.32466589232498E+03 0.22999999802666E+00 0.00000000000000E+00 0.22115035566738E+00
 0.00000000000000E+00 -.84872364712594E+01 0.99944051533252E-03 0.14034127531696E+00 0.80000000000000E+04
 0.30000000000000E+04 0.57003899828698E+02 0.21376462435762E+02 0.30396623169866E+03 0.35236826532989E+03
 0.30338456255716E+03 0.30583684927545E+03 0.30315000000007E+03 0.30315000000007E+03 0.30338101879389E+03
 0.30583784098268E+03 0.30315000000007E+03 0.30315000000007E+03 0.30338456255716E+03 0.30583684927545E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30338101879389E+03 0.30583784098268E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30535796397171E+03 0.30315000000007E+03 0.37955838991697E+02 0.36726094606990E+02
 0.33828697278593E+02 0.26975289195317E+03 0.23575505118819E+03 0.27123932789822E+02 0.31792203616838E+02
 0.26933435787565E+02 0.21724362035965E+03 0.26708910509861E+02 0.31878754923054E+02 0.26523318502342E+02
 0.21732219048975E+03 0.27123932789822E+02 0.31792203616838E+02 0.26933435787565E+02 0.21724362035965E+03
 0.26708910509861E+02 0.31878754923056E+02 0.26523318502342E+02 0.21732219048976E+03 0.12315115955347E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35359118277626E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13526152186212E+00 0.00000000000000E+00 0.00000000000000E+00 0.13526152186212E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17861236724214E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17861236724214E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18265579587641E+00 0.19278715761499E+00 0.33351531249611E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    170.54209935
 0.81045874688848E-01 0.30839634873501E+03 0.42959623173749E+03 0.42632198156078E+03 0.42508311583053E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20051014742186E+00 0.00000000000000E+00 -.16891329551903E+02
 0.42260970381785E-02 0.43450269023674E+00 0.18929995993296E+04 0.70987484974860E+03 0.18411853780793E+02
 0.69044451677974E+01 0.31725284584238E+03 0.30315000000007E+03 0.31529931828462E+03 0.32627070123082E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31336978371906E+03 0.32622576895798E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31529931828462E+03 0.32627070123082E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31336978371906E+03 0.32622576895798E+03 0.30315000000007E+03 0.30315000000007E+03 0.35530129503217E+03
 0.30416237670808E+03 0.15762703027861E+04 0.15440635377794E+04 0.63178170393965E+03 0.14238522329074E+04
 0.78891162044800E+03 0.90762294668765E+03 0.86870599961495E+03 0.87973031269335E+03 0.16655406693103E+04
 0.78470411888144E+03 0.86661932490724E+03 0.76620228687178E+03 0.16638874353455E+04 0.90762294668765E+03
 0.86870599961495E+03 0.87973031269335E+03 0.16655406693103E+04 0.78470411888145E+03 0.86661932490724E+03
 0.76620228687179E+03 0.16638874353455E+04 0.16977647136322E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47060310513462E+03 0.12948289438564E+01
 0.12948289438564E+01 0.50218373225289E+00 0.37912654299501E+00 0.30324370302892E+03 0.33424479637177E+03
 0.32640923813968E+03 0.32582573765623E+03 0.22999999802353E+00 0.00000000000000E+00 0.22016982434266E+00
 0.00000000000000E+00 -.99648336153135E+01 0.99931722305300E-03 0.16113647105421E+00 0.80000000000000E+04
 0.30000000000000E+04 0.49647357594846E+02 0.18617759098067E+02 0.30416125078434E+03 0.35533789650706E+03
 0.30343549969648E+03 0.30608226800925E+03 0.30315000000007E+03 0.30315000000007E+03 0.30343123028392E+03
 0.30608332881932E+03 0.30315000000007E+03 0.30315000000007E+03 0.30343549969648E+03 0.30608226800925E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30343123028392E+03 0.30608332881932E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30557106130779E+03 0.30315000000007E+03 0.43793648576524E+02 0.42200506766515E+02
 0.40135178601423E+02 0.28149492858323E+03 0.24115907408880E+03 0.31338580594897E+02 0.37870754564753E+02
 0.31119520449976E+02 0.22635961754467E+03 0.30872564121103E+02 0.37956828240255E+02 0.30659656149900E+02
 0.22643715554236E+03 0.31338580594897E+02 0.37870754564753E+02 0.31119520449976E+02 0.22635961754467E+03
 0.30872564121102E+02 0.37956828240255E+02 0.30659656149900E+02 0.22643715554236E+03 0.13078365421593E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35455105329173E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15004924607511E+00 0.00000000000000E+00 0.00000000000000E+00 0.15004924607511E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17948402924253E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17948402924253E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18260976102580E+00 0.19231511482119E+00 0.33424479637177E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    180.34702741
 0.80190610634529E-01 0.30865036958735E+03 0.43103494011895E+03 0.42776357563789E+03 0.42651434331792E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19846189511056E+00 0.00000000000000E+00 -.18064272276894E+02
 0.42711682574004E-02 0.46318391128167E+00 0.18730238468454E+04 0.70238394256703E+03 0.17271757082113E+02
 0.64769089057924E+01 0.31823840881570E+03 0.30315000000007E+03 0.31609702195102E+03 0.32773145336111E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31410472873838E+03 0.32768717247517E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31609702195102E+03 0.32773145336111E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31410472873838E+03 0.32768717247517E+03 0.30315000000007E+03 0.30315000000007E+03 0.35790501578058E+03
 0.30436887190918E+03 0.16139292469247E+04 0.15781632928815E+04 0.64637264266503E+03 0.14223266396841E+04
 0.77272213380572E+03 0.92961033660122E+03 0.89831076923203E+03 0.89886850960066E+03 0.16935381734854E+04
 0.80706123310345E+03 0.89636087778353E+03 0.78624646799304E+03 0.16920145636129E+04 0.92961033660122E+03
 0.89831076923203E+03 0.89886850960065E+03 0.16935381734854E+04 0.80706123310346E+03 0.89636087778353E+03
 0.78624646799305E+03 0.16920145636129E+04 0.17224170883311E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47203686176335E+03 0.12948375858610E+01
 0.12948375858610E+01 0.54140344452430E+00 0.35225436151300E+00 0.30328650543593E+03 0.33480291877160E+03
 0.32740172273378E+03 0.32682725052304E+03 0.22999999801288E+00 0.00000000000000E+00 0.21919552533330E+00
 0.00000000000000E+00 -.11086285232844E+02 0.99916512719516E-03 0.18140028546352E+00 0.80000000000000E+04
 0.30000000000000E+04 0.44101363895643E+02 0.16538011460866E+02 0.30436763142460E+03 0.35794223578694E+03
 0.30348797398758E+03 0.30631145162682E+03 0.30315000000007E+03 0.30315000000007E+03 0.30348299898208E+03
 0.30631256604232E+03 0.30315000000007E+03 0.30315000000007E+03 0.30348797398758E+03 0.30631145162682E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30348299898208E+03 0.30631256604232E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30576986877982E+03 0.30315000000007E+03 0.49313573298958E+02 0.47331290875517E+02
 0.46196393970529E+02 0.29139797385520E+03 0.24497059791481E+03 0.35402181024794E+02 0.43697187881968E+02
 0.35165518707764E+02 0.23408113858403E+03 0.34892228051440E+02 0.43781419990922E+02 0.34662927447571E+02
 0.23415641179549E+03 0.35402181024794E+02 0.43697187881968E+02 0.35165518707764E+02 0.23408113858403E+03
 0.34892228051440E+02 0.43781419990923E+02 0.34662927447571E+02 0.23415641179549E+03 0.13749833453862E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35529265894686E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16031132649232E+00 0.00000000000000E+00 0.00000000000000E+00 0.16031132649232E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18039352192306E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18039352192306E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18257497750333E+00 0.19195582548774E+00 0.33480291877160E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    191.06124476
 0.82418133038742E-01 0.30892786235283E+03 0.43215280870360E+03 0.42876748536292E+03 0.42746848201789E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19628655774861E+00 0.00000000000000E+00 -.19001783418181E+02
 0.41557300455369E-02 0.49390459034520E+00 0.19250528577023E+04 0.72189482163835E+03 0.16197460311937E+02
 0.60740476169764E+01 0.31926323983591E+03 0.30315000000008E+03 0.31693181353431E+03 0.32923992534456E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31487418060105E+03 0.32919638239336E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31693181353431E+03 0.32923992534456E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31487418060104E+03 0.32919638239336E+03 0.30315000000007E+03 0.30315000000007E+03 0.36051017942506E+03
 0.30461718804669E+03 0.16465474469530E+04 0.16070588861931E+04 0.65731849089997E+03 0.14132470745691E+04
 0.75264199121459E+03 0.94914591059387E+03 0.92413437789055E+03 0.91571576416488E+03 0.17146379827250E+04
 0.82693378993135E+03 0.92232020495758E+03 0.80390277233975E+03 0.17132419074394E+04 0.94914591059387E+03
 0.92413437789056E+03 0.91571576416488E+03 0.17146379827250E+04 0.82693378993136E+03 0.92232020495758E+03
 0.80390277233976E+03 0.17132419074394E+04 0.17394239981206E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47300333809396E+03 0.12948444933540E+01
 0.12948444933540E+01 0.58426031390021E+00 0.32463555628497E+00 0.30334888064703E+03 0.33527746363564E+03
 0.32836736143571E+03 0.32781014869186E+03 0.22999999799337E+00 0.00000000000000E+00 0.21808632406554E+00
 0.00000000000000E+00 -.12002173282264E+02 0.99895064307400E-03 0.20411886818145E+00 0.80000000000000E+04
 0.30000000000000E+04 0.39192849104417E+02 0.14697318414156E+02 0.30461582319822E+03 0.36054764812775E+03
 0.30354909177448E+03 0.30655588535997E+03 0.30315000000007E+03 0.30315000000007E+03 0.30354334482099E+03
 0.30655704475297E+03 0.30315000000007E+03 0.30315000000007E+03 0.30354909177448E+03 0.30655588535997E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30354334482099E+03 0.30655704475297E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30598149986084E+03 0.30315000000007E+03 0.55220289418438E+02 0.52771817005740E+02
 0.52795619821609E+02 0.30088178390824E+03 0.24782218598752E+03 0.39849764196133E+02 0.50024856763306E+02
 0.39610893815324E+02 0.24151310030691E+03 0.39297492957830E+02 0.50105790568407E+02 0.39067239382437E+02
 0.24158472894749E+03 0.39849764196133E+02 0.50024856763306E+02 0.39610893815324E+02 0.24151310030691E+03
 0.39297492957830E+02 0.50105790568408E+02 0.39067239382437E+02 0.24158472894749E+03 0.14421448330336E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35593671125183E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16815859599052E+00 0.00000000000000E+00 0.00000000000000E+00 0.16815859599052E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18146887985819E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18146887985819E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18254674003257E+00 0.19165275565990E+00 0.33527746363564E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    200.34160767
 0.85372935173053E-01 0.30918569845383E+03 0.43283116226319E+03 0.42931250354111E+03 0.42796079474404E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19446689259151E+00 0.00000000000000E+00 -.19548655894308E+02
 0.40118976747246E-02 0.51978887409580E+00 0.19940688044964E+04 0.74777580168615E+03 0.15390864250252E+02
 0.57715740938447E+01 0.32011026965514E+03 0.30315000000009E+03 0.31762591539479E+03 0.33047423785336E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31551415642480E+03 0.33043135850493E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31762591539479E+03 0.33047423785336E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31551415642480E+03 0.33043135850493E+03 0.30315000000007E+03 0.30315000000007E+03 0.36257285543807E+03
 0.30485242995326E+03 0.16691009598846E+04 0.16266219114542E+04 0.66335792193937E+03 0.14010225026509E+04
 0.73434779110180E+03 0.96296362373883E+03 0.94180276243708E+03 0.92752669162907E+03 0.17264558280257E+04
 0.84102082872239E+03 0.94009612972373E+03 0.81633387971433E+03 0.17251599170187E+04 0.96296362373883E+03
 0.94180276243707E+03 0.92752669162907E+03 0.17264558280257E+04 0.84102082872240E+03 0.94009612972373E+03
 0.81633387971434E+03 0.17251599170187E+04 0.17478051748053E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47350976694982E+03 0.12948485226997E+01
 0.12948485226997E+01 0.62138176553481E+00 0.30216627968078E+00 0.30341865621309E+03 0.33558726447933E+03
 0.32910708535778E+03 0.32856992418378E+03 0.22999999798656E+00 0.00000000000000E+00 0.21709167017433E+00
 0.00000000000000E+00 -.12546686959725E+02 0.99871555021804E-03 0.22425745795004E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35673284059887E+02 0.13377481522458E+02 0.30485095985021E+03 0.36261028922764E+03
 0.30360532254430E+03 0.30676263685327E+03 0.30315000000007E+03 0.30315000000007E+03 0.30359891276985E+03
 0.30676382263678E+03 0.30315000000007E+03 0.30315000000007E+03 0.30360532254430E+03 0.30676263685327E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30359891276985E+03 0.30676382263678E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30616030871709E+03 0.30315000000007E+03 0.60156474380930E+02 0.57273640351802E+02
 0.58427059874225E+02 0.30802378037651E+03 0.24930458520291E+03 0.43682986935033E+02 0.55407475352770E+02
 0.43460855195978E+02 0.24713465859441E+03 0.43099487120137E+02 0.55484377980135E+02 0.42886913439967E+02
 0.24720206241334E+03 0.43682986935033E+02 0.55407475352770E+02 0.43460855195978E+02 0.24713465859441E+03
 0.43099487120137E+02 0.55484377980137E+02 0.42886913439967E+02 0.24720206241335E+03 0.14949569248261E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35637176149319E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17260359841992E+00 0.00000000000000E+00 0.00000000000000E+00 0.17260359841992E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18235404383789E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18235404383789E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18253010805685E+00 0.19145735457442E+00 0.33558726447933E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    210.94773671
 0.88864534084837E-01 0.30949940067268E+03 0.43338058316021E+03 0.42971103530566E+03 0.42830245904664E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19246576270705E+00 0.00000000000000E+00 -.19959695080332E+02
 0.38542650174881E-02 0.54840949476754E+00 0.20756227098296E+04 0.77835851618609E+03 0.14587639485328E+02
 0.54703648069982E+01 0.32103498286491E+03 0.30315000000012E+03 0.31838763756000E+03 0.33180641606521E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31621656401648E+03 0.33176430937290E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31838763756000E+03 0.33180641606521E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31621656401648E+03 0.33176430937290E+03 0.30315000000007E+03 0.30315000000007E+03 0.36473280764302E+03
 0.30514339571535E+03 0.16900485785306E+04 0.16444208975025E+04 0.66733244254989E+03 0.13840831970881E+04
 0.71341409232549E+03 0.97604641517145E+03 0.95779783896692E+03 0.93858059434529E+03 0.17345965405348E+04
 0.85440900789805E+03 0.95620396522651E+03 0.82806009595393E+03 0.17334048127412E+04 0.97604641517145E+03
 0.95779783896692E+03 0.93858059434529E+03 0.17345965405348E+04 0.85440900789806E+03 0.95620396522651E+03
 0.82806009595393E+03 0.17334048127412E+04 0.17523881390948E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47386772067985E+03 0.12948515512485E+01
 0.12948515512485E+01 0.66380628168865E+00 0.27802495727064E+00 0.30351964597402E+03 0.33583994774065E+03
 0.32984938072889E+03 0.32933976898158E+03 0.23000000000000E+00 0.00000000000000E+00 0.21592222818337E+00
 0.00000000000000E+00 -.12966023008228E+02 0.99837911493221E-03 0.24773048801156E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32293158844570E+02 0.12109934566714E+02 0.30514180533138E+03 0.36476998999117E+03
 0.30367284231757E+03 0.30699252154153E+03 0.30315000000007E+03 0.30315000000007E+03 0.30366569339706E+03
 0.30699372305763E+03 0.30315000000007E+03 0.30315000000007E+03 0.30367284231757E+03 0.30699252154153E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30366569339706E+03 0.30699372305763E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30635885178522E+03 0.30315000000008E+03 0.65515295711478E+02 0.62113197763564E+02
 0.64685009478184E+02 0.31499964651363E+03 0.24999121198806E+03 0.47997552467656E+02 0.61369585919208E+02
 0.47820940250177E+02 0.25269241980669E+03 0.47384912483602E+02 0.61440757704804E+02 0.47218575327178E+02
 0.25275399123512E+03 0.47997552467656E+02 0.61369585919208E+02 0.47820940250177E+02 0.25269241980669E+03
 0.47384912483602E+02 0.61440757704806E+02 0.47218575327178E+02 0.25275399123512E+03 0.15491555299995E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35668489909210E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17588532047747E+00 0.00000000000000E+00 0.00000000000000E+00 0.17588532047747E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18335242158640E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18335242158640E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18251751657969E+00 0.19129931417304E+00 0.33583994774065E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    221.58151064
 0.91974500789697E-01 0.30982573155680E+03 0.43377805661365E+03 0.42997790554071E+03 0.42852204223933E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19054328301897E+00 0.00000000000000E+00 -.20210483891825E+02
 0.37239393115412E-02 0.57600988286542E+00 0.21482627214699E+04 0.80559852055120E+03 0.13888650590860E+02
 0.52082439715725E+01 0.32191858733141E+03 0.30315000000018E+03 0.31911881357728E+03 0.33306425875992E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31689079283386E+03 0.33302292380647E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31911881357728E+03 0.33306425875992E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31689079283386E+03 0.33302292380647E+03 0.30315000000007E+03 0.30315000000007E+03 0.36671734114492E+03
 0.30545742613108E+03 0.17073107686198E+04 0.16587718938509E+04 0.66908604230659E+03 0.13655040040819E+04
 0.69307253156373E+03 0.98700773009851E+03 0.97046905652906E+03 0.94769529176614E+03 0.17388295883632E+04
 0.86568432344800E+03 0.96897836908508E+03 0.83783869898498E+03 0.17377325082078E+04 0.98700773009851E+03
 0.97046905652906E+03 0.94769529176614E+03 0.17388295883632E+04 0.86568432344800E+03 0.96897836908508E+03
 0.83783869898499E+03 0.17377325082079E+04 0.17535075847367E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47410150691853E+03 0.12948533990763E+01
 0.12948533990763E+01 0.70634137743822E+00 0.25490209097204E+00 0.30364733539105E+03 0.33599013964993E+03
 0.33049397402761E+03 0.33001667603536E+03 0.23000000000000E+00 0.00000000000000E+00 0.21472657224922E+00
 0.00000000000000E+00 -.13228999850773E+02 0.99795668643727E-03 0.27158315428779E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29456908036066E+02 0.11046340513525E+02 0.30545572465794E+03 0.36675413006984E+03
 0.30374317988827E+03 0.30721490708682E+03 0.30315000000007E+03 0.30315000000007E+03 0.30373532429919E+03
 0.30721610939754E+03 0.30315000000007E+03 0.30315000000007E+03 0.30374317988827E+03 0.30721490708682E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30373532429919E+03 0.30721610939754E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30655037661484E+03 0.30315000000008E+03 0.70502941696064E+02 0.66571954435877E+02
 0.70673789601296E+02 0.32071049728570E+03 0.24968333873640E+03 0.52205351528050E+02 0.67050834848211E+02
 0.52104995218103E+02 0.25728025678362E+03 0.51571611783668E+02 0.67115205205948E+02 0.51481402525134E+02
 0.25733504908652E+03 0.52205351528050E+02 0.67050834848211E+02 0.52104995218103E+02 0.25728025678362E+03
 0.51571611783668E+02 0.67115205205949E+02 0.51481402525135E+02 0.25733504908652E+03 0.15966468614654E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35682991592332E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17785469949638E+00 0.00000000000000E+00 0.00000000000000E+00 0.17785469949638E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18426309469592E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18426309469592E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250992934601E+00 0.19120535570058E+00 0.33599013964993E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    230.00709743
 0.94007101773213E-01 0.31009505881191E+03 0.43403285750065E+03 0.43014917974902E+03 0.42866420174199E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18907746748842E+00 0.00000000000000E+00 -.20314644819505E+02
 0.36434210674433E-02 0.59708651959889E+00 0.21957385248403E+04 0.82340194681511E+03 0.13398393260284E+02
 0.50243974726063E+01 0.32259569399039E+03 0.30315000000028E+03 0.31968143142315E+03 0.33401409452892E+03
 0.30315000000007E+03 0.30315000000007E+03 0.31741013764985E+03 0.33397339707595E+03 0.30315000000007E+03
 0.30315000000007E+03 0.31968143142315E+03 0.33401409452892E+03 0.30315000000007E+03 0.30315000000007E+03
 0.31741013764985E+03 0.33397339707595E+03 0.30315000000007E+03 0.30315000000007E+03 0.36816383325738E+03
 0.30572436423529E+03 0.17190922043365E+04 0.16684013269491E+04 0.66961423807555E+03 0.13509080772667E+04
 0.67794576800082E+03 0.99457388252308E+03 0.97877678866235E+03 0.95389852367833E+03 0.17403848938318E+04
 0.87350678948219E+03 0.97736145518219E+03 0.84456748978211E+03 0.17393561951540E+04 0.99457388252308E+03
 0.97877678866235E+03 0.95389852367833E+03 0.17403848938318E+04 0.87350678948219E+03 0.97736145518219E+03
 0.84456748978212E+03 0.17393561951540E+04 0.17530167496878E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47425254889991E+03 0.12948541665425E+01
 0.12948541665425E+01 0.74004372460506E+00 0.23731517550115E+00 0.30377049239428E+03 0.33605108642685E+03
 0.33094396986810E+03 0.33049467178775E+03 0.23000000000000E+00 0.00000000000000E+00 0.21376646378495E+00
 0.00000000000000E+00 -.13342247442016E+02 0.99755097159551E-03 0.29065403990324E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27524131447350E+02 0.10321549292756E+02 0.30572257463905E+03 0.36820018797104E+03
 0.30380226857173E+03 0.30738647131510E+03 0.30315000000007E+03 0.30315000000007E+03 0.30379386378670E+03
 0.30738766053909E+03 0.30315000000007E+03 0.30315000000007E+03 0.30380226857173E+03 0.30738647131510E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30379386378670E+03 0.30738766053909E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30669829508018E+03 0.30315000000010E+03 0.74151427985198E+02 0.69800159064980E+02
 0.75204962663952E+02 0.32448190166519E+03 0.24890091418792E+03 0.55463122899517E+02 0.71329090712949E+02
 0.55463122899517E+02 0.26030443668252E+03 0.54818836789005E+02 0.71387106264002E+02 0.54818836789005E+02
 0.26035300336309E+03 0.55463122899517E+02 0.71329090712949E+02 0.55463122899517E+02 0.26030443668252E+03
 0.54818836789005E+02 0.71387106264002E+02 0.54818836789005E+02 0.26035300336309E+03 0.16298930915707E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35689764506050E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17864559097083E+00 0.00000000000000E+00 0.00000000000000E+00 0.17864559097083E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18490382021752E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18490382021752E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250694457142E+00 0.19116733820832E+00 0.33605108642685E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    240.07390113
 0.95939836576328E-01 0.31041654284124E+03 0.43430167534012E+03 0.43033983555140E+03 0.42882854565710E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18738896280950E+00 0.00000000000000E+00 -.20402232069056E+02
 0.35700232598580E-02 0.62136962667797E+00 0.22408817583777E+04 0.84033065939166E+03 0.12874784438323E+02
 0.48280441643711E+01 0.32337545996721E+03 0.30315000000048E+03 0.32033088949573E+03 0.33509932201138E+03
 0.30315000000007E+03 0.30315000000008E+03 0.31800959741569E+03 0.33505936034734E+03 0.30315000000007E+03
 0.30315000000008E+03 0.32033088949573E+03 0.33509932201138E+03 0.30315000000007E+03 0.30315000000008E+03
 0.31800959741568E+03 0.33505936034734E+03 0.30315000000007E+03 0.30315000000008E+03 0.36979453568389E+03
 0.30605889016482E+03 0.17316221377353E+04 0.16784906819989E+04 0.66936179298799E+03 0.13335035998923E+04
 0.66079499793935E+03 0.10026780393098E+04 0.98726732484118E+03 0.96044196852045E+03 0.17409772765560E+04
 0.88192576495921E+03 0.98593513375705E+03 0.85174356600234E+03 0.17400236976271E+04 0.10026780393098E+04
 0.98726732484118E+03 0.96044196852045E+03 0.17409772765560E+04 0.88192576495922E+03 0.98593513375705E+03
 0.85174356600236E+03 0.17400236976271E+04 0.17514322870042E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47442488423912E+03 0.12948548118933E+01
 0.12948548118933E+01 0.78031093938482E+00 0.21717090202997E+00 0.30394695181051E+03 0.33607266978065E+03
 0.33142148901402E+03 0.33100739370520E+03 0.23000000000000E+00 0.00000000000000E+00 0.21260760725383E+00
 0.00000000000000E+00 -.13438764424466E+02 0.99697088337736E-03 0.31359230831617E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25510829787108E+02 0.95665611701654E+01 0.30605700043648E+03 0.36983026097930E+03
 0.30387338296291E+03 0.30758404754426E+03 0.30315000000007E+03 0.30315000000007E+03 0.30386435911507E+03
 0.30758521044115E+03 0.30315000000007E+03 0.30315000000007E+03 0.30387338296291E+03 0.30758404754426E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30386435911507E+03 0.30758521044115E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30686834222634E+03 0.30315000000014E+03 0.78157848249254E+02 0.73333289822630E+02
 0.80337130100640E+02 0.32826646677272E+03 0.24752765102157E+03 0.59232004987076E+02 0.76160149073804E+02
 0.59232004987076E+02 0.26333267571834E+03 0.58581464891725E+02 0.76210163331015E+02 0.58581464891725E+02
 0.26337348101447E+03 0.59232004987076E+02 0.76160149073804E+02 0.59232004987076E+02 0.26333267571834E+03
 0.58581464891725E+02 0.76210163331015E+02 0.58581464891725E+02 0.26337348101447E+03 0.16651168437221E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35694158254017E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17928912403495E+00 0.00000000000000E+00 0.00000000000000E+00 0.17928912403495E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18559018699548E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18559018699548E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250465852095E+00 0.19115248446228E+00 0.33607266978065E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    250.01695810
 0.97382349463160E-01 0.31073065731612E+03 0.43455247780990E+03 0.43053312454507E+03 0.42900324870201E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18578425726161E+00 0.00000000000000E+00 -.20451616897080E+02
 0.35171408325777E-02 0.64442658079096E+00 0.22745748267740E+04 0.85296556004024E+03 0.12414137216657E+02
 0.46553014562463E+01 0.32411829223187E+03 0.30315000000080E+03 0.32095110855011E+03 0.33612371024017E+03
 0.30315000000008E+03 0.30315000000009E+03 0.31858227166756E+03 0.33608445816595E+03 0.30315000000008E+03
 0.30315000000009E+03 0.32095110855011E+03 0.33612371024017E+03 0.30315000000008E+03 0.30315000000009E+03
 0.31858227166756E+03 0.33608445816595E+03 0.30315000000008E+03 0.30315000000009E+03 0.37130587953132E+03
 0.30640778479700E+03 0.17427676880394E+04 0.16873323339913E+04 0.66859427872635E+03 0.13169673033851E+04
 0.64503005326511E+03 0.10099251849131E+04 0.99453783618094E+03 0.96619354677315E+03 0.17407753861067E+04
 0.88948990128597E+03 0.99328106651336E+03 0.85812238464305E+03 0.17398895397882E+04 0.10099251849131E+04
 0.99453783618094E+03 0.96619354677315E+03 0.17407753861067E+04 0.88948990128598E+03 0.99328106651336E+03
 0.85812238464306E+03 0.17398895397882E+04 0.17493633850142E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47460506568612E+03 0.12948551757655E+01
 0.12948551757655E+01 0.82008316725232E+00 0.19821055273493E+00 0.30415520832742E+03 0.33605377303530E+03
 0.33183868494050E+03 0.33146028178674E+03 0.23000000000000E+00 0.00000000000000E+00 0.21145302789891E+00
 0.00000000000000E+00 -.13493952742015E+02 0.99628770977350E-03 0.33637156953653E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23783222853890E+02 0.89187085702088E+01 0.30640581810979E+03 0.37134096427474E+03
 0.30394566780198E+03 0.30777197372238E+03 0.30315000000007E+03 0.30315000000007E+03 0.30393619290341E+03
 0.30777309814616E+03 0.30315000000007E+03 0.30315000000007E+03 0.30394566780198E+03 0.30777197372238E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30393619290341E+03 0.30777309814616E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30702995724424E+03 0.30315000000018E+03 0.81729159515422E+02 0.76474353250325E+02
 0.85121361481946E+02 0.33138629256115E+03 0.24583932427180E+03 0.62838402363258E+02 0.80645063110217E+02
 0.63122347969858E+02 0.26581913550712E+03 0.62187685863246E+02 0.80686559725730E+02 0.62487736387253E+02
 0.26585175806827E+03 0.62838402363258E+02 0.80645063110217E+02 0.63122347969858E+02 0.26581913550712E+03
 0.62187685863246E+02 0.80686559725730E+02 0.62487736387253E+02 0.26585175806827E+03 0.16959030166728E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35695954418963E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17961654532598E+00 0.00000000000000E+00 0.00000000000000E+00 0.17961654532598E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18618216587018E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18618216587018E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250369079857E+00 0.19116211541484E+00 0.33605377303530E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    260.00558940
 0.98446438822858E-01 0.31104223414682E+03 0.43480581872012E+03 0.43074445733439E+03 0.42920161874437E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18423107968761E+00 0.00000000000000E+00 -.20482662226524E+02
 0.34791245592009E-02 0.66670150614504E+00 0.22994290270071E+04 0.86228588512767E+03 0.11999372922160E+02
 0.44997648458100E+01 0.32484060105218E+03 0.30315000000136E+03 0.32155547332544E+03 0.33711170233318E+03
 0.30315000000009E+03 0.30315000000011E+03 0.31914058923646E+03 0.33707314570368E+03 0.30315000000008E+03
 0.30315000000011E+03 0.32155547332544E+03 0.33711170233318E+03 0.30315000000009E+03 0.30315000000011E+03
 0.31914058923646E+03 0.33707314570368E+03 0.30315000000008E+03 0.30315000000011E+03 0.37273917556351E+03
 0.30677689109933E+03 0.17530624931150E+04 0.16954016411413E+04 0.66754026403418E+03 0.13012333715639E+04
 0.63035540620959E+03 0.10166417085840E+04 0.10010499902590E+04 0.97144459707780E+03 0.17401869648280E+04
 0.89652844347668E+03 0.99986287626405E+03 0.86400321800918E+03 0.17393632884913E+04 0.10166417085840E+04
 0.10010499902590E+04 0.97144459707780E+03 0.17401869648280E+04 0.89652844347668E+03 0.99986287626405E+03
 0.86400321800918E+03 0.17393632884913E+04 0.17471312192779E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47480701238269E+03 0.12948554045107E+01
 0.12948554045107E+01 0.86003769248055E+00 0.18010433454916E+00 0.30440138785755E+03 0.33600762326275E+03
 0.33221267659925E+03 0.33187022490338E+03 0.23000000000000E+00 0.00000000000000E+00 0.21028538754153E+00
 0.00000000000000E+00 -.13527417486245E+02 0.99548164982033E-03 0.35934233949272E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22262892848345E+02 0.83485848181294E+01 0.30677485103682E+03 0.37277348301805E+03
 0.30402026069895E+03 0.30795410757725E+03 0.30315000000007E+03 0.30315000000007E+03 0.30401034148680E+03
 0.30795518137308E+03 0.30315000000007E+03 0.30315000000007E+03 0.30402026069895E+03 0.30795410757725E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30401034148680E+03 0.30795518137308E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30718651525288E+03 0.30315000000026E+03 0.84934457323410E+02 0.79297359033547E+02
 0.89651662590715E+02 0.33403535054916E+03 0.24393542964549E+03 0.66351152068593E+02 0.84875036321581E+02
 0.66965089453777E+02 0.26791967665167E+03 0.65706682427589E+02 0.84907519915240E+02 0.66340667177897E+02
 0.26794371495313E+03 0.66351152068593E+02 0.84875036321581E+02 0.66965089453777E+02 0.26791967665167E+03
 0.65706682427589E+02 0.84907519915240E+02 0.66340667177897E+02 0.26794371495313E+03 0.17233073620629E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35696436878172E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17978467091492E+00 0.00000000000000E+00 0.00000000000000E+00 0.17978467091492E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18669620018209E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18669620018209E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250342013105E+00 0.19118802431246E+00 0.33600762326275E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    270.00209193
 0.99211358444875E-01 0.31134901797390E+03 0.43506946820701E+03 0.43097797689533E+03 0.42942630286601E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18273199242223E+00 0.00000000000000E+00 -.20504781751066E+02
 0.34523003203579E-02 0.68814576716226E+00 0.23172955008650E+04 0.86898581282436E+03 0.11625443883772E+02
 0.43595414564144E+01 0.32554214507087E+03 0.30315000000227E+03 0.32214355598029E+03 0.33806456423923E+03
 0.30315000000012E+03 0.30315000000016E+03 0.31968418329879E+03 0.33802668450277E+03 0.30315000000011E+03
 0.30315000000016E+03 0.32214355598029E+03 0.33806456423923E+03 0.30315000000012E+03 0.30315000000016E+03
 0.31968418329879E+03 0.33802668450277E+03 0.30315000000011E+03 0.30315000000016E+03 0.37410065095838E+03
 0.30716491288839E+03 0.17626770058260E+04 0.17028606567389E+04 0.66634208890097E+03 0.12864388475027E+04
 0.61676504815723E+03 0.10229292514541E+04 0.10069926479310E+04 0.97629406378685E+03 0.17394555834033E+04
 0.90313959158571E+03 0.10058696627734E+04 0.86948008681574E+03 0.17386887965727E+04 0.10229292514541E+04
 0.10069926479310E+04 0.97629406378685E+03 0.17394555834033E+04 0.90313959158572E+03 0.10058696627734E+04
 0.86948008681577E+03 0.17386887965727E+04 0.17449366571424E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47503386035196E+03 0.12948555674896E+01
 0.12948555674896E+01 0.90002370256923E+00 0.16292923189514E+00 0.30468825032479E+03 0.33594478348729E+03
 0.33254971486071E+03 0.33224268238502E+03 0.23000000000000E+00 0.00000000000000E+00 0.20911072642732E+00
 0.00000000000000E+00 -.13548613463370E+02 0.10691424984640E-02 0.38239047249926E+00 0.74826321201273E+04
 0.28059870450477E+04 0.20921023339606E+02 0.78453837523523E+01 0.30716280341507E+03 0.37413404731852E+03
 0.30409699250349E+03 0.30813024167514E+03 0.30315000000007E+03 0.30315000000007E+03 0.30408670104532E+03
 0.30813125361339E+03 0.30315000000007E+03 0.30315000000007E+03 0.30409699250349E+03 0.30813024167514E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30408670104532E+03 0.30813125361339E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30733786992166E+03 0.30315000000038E+03 0.87767053657275E+02 0.81811859954881E+02
 0.93924462380635E+02 0.33631551114151E+03 0.24192142644897E+03 0.69763717352207E+02 0.88849303625942E+02
 0.70853484165882E+02 0.26971694537977E+03 0.69131090203437E+02 0.88872441234395E+02 0.70244657314572E+02
 0.26973215142604E+03 0.69763717352207E+02 0.88849303625942E+02 0.70853484165882E+02 0.26971694537977E+03
 0.69131090203437E+02 0.88872441234397E+02 0.70244657314572E+02 0.26973215142604E+03 0.17477920934904E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35696540983986E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17986902695978E+00 0.00000000000000E+00 0.00000000000000E+00 0.17986902695978E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18713894415155E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18713894415155E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250352842335E+00 0.19122386022039E+00 0.33594478348729E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    280.01581703
 0.99754489568391E-01 0.31165065992057E+03 0.43534778215433E+03 0.43123466772450E+03 0.42967697571538E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18128244342952E+00 0.00000000000000E+00 -.20523905031546E+02
 0.34335034343349E-02 0.70881857909978E+00 0.23299816508119E+04 0.87374311905445E+03 0.11286385876285E+02
 0.42323947036068E+01 0.32622579623018E+03 0.30315000000374E+03 0.32271759000283E+03 0.33898758981247E+03
 0.30315000000017E+03 0.30315000000024E+03 0.32021513263939E+03 0.33895036793015E+03 0.30315000000014E+03
 0.30315000000024E+03 0.32271759000283E+03 0.33898758981247E+03 0.30315000000017E+03 0.30315000000024E+03
 0.32021513263939E+03 0.33895036793015E+03 0.30315000000014E+03 0.30315000000024E+03 0.37540138896728E+03
 0.30757219967916E+03 0.17717597308256E+04 0.17098422989723E+04 0.66508078496861E+03 0.12725628915036E+04
 0.60415670261011E+03 0.10288811991060E+04 0.10125157534297E+04 0.98082845103992E+03 0.17387188778032E+04
 0.90941520863379E+03 0.10114519195976E+04 0.87463787420368E+03 0.17380042489349E+04 0.10288811991060E+04
 0.10125157534297E+04 0.98082845103992E+03 0.17387188778032E+04 0.90941520863381E+03 0.10114519195976E+04
 0.87463787420371E+03 0.17380042489349E+04 0.17428747309855E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47528573285131E+03 0.12948557083920E+01
 0.12948557083920E+01 0.94007860298383E+00 0.14666984619731E+00 0.30501919980949E+03 0.33587366144634E+03
 0.33285671535115E+03 0.33258407110167E+03 0.23000000000000E+00 0.00000000000000E+00 0.20792924373100E+00
 0.00000000000000E+00 -.13563743272811E+02 0.11876642913673E-02 0.40551572679873E+00 0.67359101878782E+04
 0.25259663204543E+04 0.19727964839131E+02 0.73979868146741E+01 0.30757002584934E+03 0.37543375135061E+03
 0.30417616499370E+03 0.30830109411699E+03 0.30315000000007E+03 0.30315000000007E+03 0.30416557094814E+03
 0.30830203379645E+03 0.30315000000007E+03 0.30315000000007E+03 0.30417616499370E+03 0.30830109411699E+03
 0.30315000000007E+03 0.30315000000007E+03 0.30416557094814E+03 0.30830203379645E+03 0.30315000000007E+03
 0.30315000000007E+03 0.30748465898282E+03 0.30315000000055E+03 0.90244035296227E+02 0.84047834430147E+02
 0.97962067709502E+02 0.33832552161518E+03 0.23987364356713E+03 0.73089426874194E+02 0.92591193868880E+02
 0.74832137951304E+02 0.27129118765048E+03 0.72473761768158E+02 0.92604761144189E+02 0.74243942966137E+02
 0.27129741440418E+03 0.73089426874194E+02 0.92591193868880E+02 0.74832137951304E+02 0.27129118765048E+03
 0.72473761768157E+02 0.92604761144189E+02 0.74243942966136E+02 0.27129741440418E+03 0.17698781053618E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35696987623625E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17991791204515E+00 0.00000000000000E+00 0.00000000000000E+00 0.17991791204515E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18752029607887E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18752029607887E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250380589517E+00 0.19126461454273E+00 0.33587366144634E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    290.00774979
 0.10013772894779E+00 0.31194571113815E+03 0.43564079990260E+03 0.43151195147898E+03 0.42995007037209E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17988516213150E+00 0.00000000000000E+00 -.20543069223234E+02
 0.34203627472559E-02 0.72867955793126E+00 0.23389332042100E+04 0.87709995157877E+03 0.10978762767426E+02
 0.41170360377846E+01 0.32689082214343E+03 0.30315000000604E+03 0.32327682518103E+03 0.33988096818563E+03
 0.30315000000025E+03 0.30315000000037E+03 0.32073274511813E+03 0.33984438192463E+03 0.30315000000020E+03
 0.30315000000037E+03 0.32327682518103E+03 0.33988096818563E+03 0.30315000000025E+03 0.30315000000037E+03
 0.32073274511813E+03 0.33984438192463E+03 0.30315000000020E+03 0.30315000000037E+03 0.37664447193010E+03
 0.30799703876797E+03 0.17803676562701E+04 0.17164012710147E+04 0.66380551302370E+03 0.12596033352691E+04
 0.59247879468033E+03 0.10345342201351E+04 0.10176928212028E+04 0.98508640149390E+03 0.17380487934428E+04
 0.91538933814484E+03 0.10166833992854E+04 0.87951109836053E+03 0.17373818568135E+04 0.10345342201351E+04
 0.10176928212028E+04 0.98508640149390E+03 0.17380487934428E+04 0.91538933814485E+03 0.10166833992854E+04
 0.87951109836056E+03 0.17373818568135E+04 0.17409899051547E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47555941438626E+03 0.12948558495958E+01
 0.12948558495958E+01 0.98004633401451E+00 0.13138318939890E+00 0.30539528791536E+03 0.33580117431188E+03
 0.33313795942436E+03 0.33289811960462E+03 0.23000000000000E+00 0.00000000000000E+00 0.20674653504450E+00
 0.00000000000000E+00 -.13576239316406E+02 0.13258508512790E-02 0.42861030455069E+00 0.60338611935744E+04
 0.22626979475904E+04 0.18664973555375E+02 0.69993650832658E+01 0.30799480695133E+03 0.37667570154821E+03
 0.30425775366719E+03 0.30846658039132E+03 0.30315000000007E+03 0.30315000000009E+03 0.30424692768556E+03
 0.30846743866563E+03 0.30315000000007E+03 0.30315000000009E+03 0.30425775366719E+03 0.30846658039132E+03
 0.30315000000007E+03 0.30315000000009E+03 0.30424692768556E+03 0.30846743866563E+03 0.30315000000007E+03
 0.30315000000009E+03 0.30762682256946E+03 0.30315000000083E+03 0.92373381616696E+02 0.86024626116812E+02
 0.10176933041275E+03 0.34013792059764E+03 0.23785974353282E+03 0.76325811084977E+02 0.96107733949768E+02
 0.78929781065975E+02 0.27270190575826E+03 0.75731643323786E+02 0.96111650555881E+02 0.78366678656945E+02
 0.27269913941378E+03 0.76325811084978E+02 0.96107733949767E+02 0.78929781065976E+02 0.27270190575826E+03
 0.75731643323786E+02 0.96111650555881E+02 0.78366678656945E+02 0.27269913941378E+03 0.17899168152673E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35698306589865E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17995752159972E+00 0.00000000000000E+00 0.00000000000000E+00 0.17995752159972E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18784830038710E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18784830038710E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250412877701E+00 0.19130621639500E+00 0.33580117431188E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    300.01030539
 0.10041201384190E+00 0.31223514202932E+03 0.43594890781288E+03 0.43180812502545E+03 0.43024313315662E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17853306886828E+00 0.00000000000000E+00 -.20563737577160E+02
 0.34110194330736E-02 0.74783065696094E+00 0.23453399070176E+04 0.87950246513160E+03 0.10697609044955E+02
 0.40116033918581E+01 0.32754102429647E+03 0.30315000000959E+03 0.32382435347364E+03 0.34075076207164E+03
 0.30315000000037E+03 0.30315000000059E+03 0.32123987716661E+03 0.34071479102516E+03 0.30315000000030E+03
 0.30315000000059E+03 0.32382435347364E+03 0.34075076207164E+03 0.30315000000037E+03 0.30315000000059E+03
 0.32123987716661E+03 0.34071479102516E+03 0.30315000000030E+03 0.30315000000059E+03 0.37784054796469E+03
 0.30844059026206E+03 0.17885903302632E+04 0.17226128379087E+04 0.66253317723675E+03 0.12474407710844E+04
 0.58159492796142E+03 0.10399477234008E+04 0.10226020869271E+04 0.98912075500786E+03 0.17374698800288E+04
 0.92112148624675E+03 0.10216429439265E+04 0.88415339861446E+03 0.17368467509202E+04 0.10399477234008E+04
 0.10226020869271E+04 0.98912075500786E+03 0.17374698800288E+04 0.92112148624677E+03 0.10216429439265E+04
 0.88415339861450E+03 0.17368467509202E+04 0.17392784015300E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47585271495420E+03 0.12948560018826E+01
 0.12948560018826E+01 0.10200565564434E+01 0.11701020603364E+00 0.30581940173735E+03 0.33573213652881E+03
 0.33339873968816E+03 0.33318990322671E+03 0.23000000000000E+00 0.00000000000000E+00 0.20555950214892E+00
 0.00000000000000E+00 -.13587936194313E+02 0.14887119293515E-02 0.45173560267486E+00 0.53737730196634E+04
 0.20151648823738E+04 0.17709474198247E+02 0.66410528243426E+01 0.30843830762864E+03 0.37787056491274E+03
 0.30434237132171E+03 0.30862780685200E+03 0.30315000000007E+03 0.30315000000011E+03 0.30433138258935E+03
 0.30862857532776E+03 0.30315000000007E+03 0.30315000000011E+03 0.30434237132171E+03 0.30862780685200E+03
 0.30315000000007E+03 0.30315000000011E+03 0.30433138258935E+03 0.30862857532776E+03 0.30315000000007E+03
 0.30315000000011E+03 0.30776531852916E+03 0.30315000000125E+03 0.94180003594577E+02 0.87774325046510E+02
 0.10537926275937E+03 0.34182331511698E+03 0.23591715604381E+03 0.79492701152378E+02 0.99431824273390E+02
 0.83206037720389E+02 0.27400678066165E+03 0.78924133388493E+02 0.99426075187540E+02 0.82672066327716E+02
 0.27399506634898E+03 0.79492701152378E+02 0.99431824273390E+02 0.83206037720389E+02 0.27400678066165E+03
 0.78924133388495E+02 0.99426075187541E+02 0.82672066327718E+02 0.27399506634898E+03 0.18083404651894E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35700880761383E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18000082438259E+00 0.00000000000000E+00 0.00000000000000E+00 0.18000082438259E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18813261393209E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18813261393209E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250443141981E+00 0.19134585057192E+00 0.33573213652881E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    310.00208730
 0.10061316151782E+00 0.31251852491459E+03 0.43627006001711E+03 0.43211971562066E+03 0.43055219137132E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17722694665437E+00 0.00000000000000E+00 -.20586304142084E+02
 0.34041997730599E-02 0.76626361805041E+00 0.23500383447852E+04 0.88126437929447E+03 0.10440271222004E+02
 0.39151017082513E+01 0.32817637774861E+03 0.30315000001492E+03 0.32436007274556E+03 0.34159768866754E+03
 0.30315000000057E+03 0.30315000000093E+03 0.32173642530624E+03 0.34156231114507E+03 0.30315000000046E+03
 0.30315000000093E+03 0.32436007274556E+03 0.34159768866754E+03 0.30315000000057E+03 0.30315000000093E+03
 0.32173642530624E+03 0.34156231114507E+03 0.30315000000046E+03 0.30315000000093E+03 0.37899232787377E+03
 0.30890163083559E+03 0.17964520959692E+04 0.17285002774151E+04 0.66127424015996E+03 0.12360199803801E+04
 0.57143936901935E+03 0.10451386321706E+04 0.10272723513298E+04 0.99295031614605E+03 0.17369863205256E+04
 0.92662728897136E+03 0.10263596270726E+04 0.88858150775749E+03 0.17364033949929E+04 0.10451386321706E+04
 0.10272723513298E+04 0.99295031614605E+03 0.17369863205256E+04 0.92662728897137E+03 0.10263596270726E+04
 0.88858150775750E+03 0.17364033949929E+04 0.17377294110580E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47616184131546E+03 0.12948561681556E+01
 0.12948561681556E+01 0.10600236840846E+01 0.10357193846368E+00 0.30629160110874E+03 0.33567041744139E+03
 0.33364187013649E+03 0.33346192304315E+03 0.23000000000000E+00 0.00000000000000E+00 0.20437122035291E+00
 0.00000000000000E+00 -.13599609833575E+02 0.16818693338922E-02 0.47483203396053E+00 0.47566120856050E+04
 0.17837295321019E+04 0.16848062952436E+02 0.63180236071634E+01 0.30889930546146E+03 0.37902107716304E+03
 0.30443025579080E+03 0.30878495935774E+03 0.30315000000007E+03 0.30315000000015E+03 0.30441917267425E+03
 0.30878563083597E+03 0.30315000000007E+03 0.30315000000015E+03 0.30443025579079E+03 0.30878495935774E+03
 0.30315000000007E+03 0.30315000000015E+03 0.30441917267425E+03 0.30878563083597E+03 0.30315000000007E+03
 0.30315000000015E+03 0.30790031028952E+03 0.30315000000190E+03 0.95677460051489E+02 0.89315103623510E+02
 0.10880348448855E+03 0.34342872123667E+03 0.23408121932567E+03 0.82591325717819E+02 0.10257634071572E+03
 0.87700471964988E+02 0.27524473112370E+03 0.82051930135468E+02 0.10256101572822E+03 0.87199122829419E+02
 0.27522420911674E+03 0.82591325717823E+02 0.10257634071572E+03 0.87700471964995E+02 0.27524473112370E+03
 0.82051930135468E+02 0.10256101572822E+03 0.87199122829419E+02 0.27522420911674E+03 0.18254062615313E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35704963699254E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18005276259678E+00 0.00000000000000E+00 0.00000000000000E+00 0.18005276259678E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18838004006604E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18838004006604E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250468013667E+00 0.19138127172749E+00 0.33567041744139E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    320.00926538
 0.10076762679187E+00 0.31279688913046E+03 0.43660337541482E+03 0.43244481348004E+03 0.43087499254116E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17596157466957E+00 0.00000000000000E+00 -.20610847115048E+02
 0.33989812221047E-02 0.78405578715380E+00 0.23536464243972E+04 0.88261740914896E+03 0.10203355591623E+02
 0.38262583468586E+01 0.32879970473520E+03 0.30315000002282E+03 0.32488629727693E+03 0.34242609721046E+03
 0.30315000000089E+03 0.30315000000148E+03 0.32222452370323E+03 0.34239129332470E+03 0.30315000000070E+03
 0.30315000000148E+03 0.32488629727693E+03 0.34242609721046E+03 0.30315000000089E+03 0.30315000000148E+03
 0.32222452370322E+03 0.34239129332470E+03 0.30315000000070E+03 0.30315000000148E+03 0.38010711701642E+03
 0.30938100193406E+03 0.18040042020331E+04 0.17341058124246E+04 0.66002622492813E+03 0.12252328889098E+04
 0.56190653285702E+03 0.10501414952100E+04 0.10317419005844E+04 0.99660547698620E+03 0.17365877837648E+04
 0.93194176671505E+03 0.10308721965867E+04 0.89282695676842E+03 0.17360418957346E+04 0.10501414952100E+04
 0.10317419005844E+04 0.99660547698620E+03 0.17365877837648E+04 0.93194176671506E+03 0.10308721965867E+04
 0.89282695676845E+03 0.17360418957346E+04 0.17363174926792E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47648465974456E+03 0.12948563489910E+01
 0.12948563489910E+01 0.11000523963895E+01 0.91022583036283E-01 0.30681321640311E+03 0.33561853222588E+03
 0.33387057605830E+03 0.33371729987552E+03 0.23000000000000E+00 0.00000000000000E+00 0.20317896868838E+00
 0.00000000000000E+00 -.13601303159508E+02 0.19137497566590E-02 0.49795256261025E+00 0.41802748620417E+04
 0.15676030732656E+04 0.16065787387586E+02 0.60246702703449E+01 0.30937864239124E+03 0.38013456120488E+03
 0.30452212533267E+03 0.30893894100492E+03 0.30315000000007E+03 0.30315000000021E+03 0.30451101430791E+03
 0.30893950901663E+03 0.30315000000007E+03 0.30315000000021E+03 0.30452212533267E+03 0.30893894100492E+03
 0.30315000000007E+03 0.30315000000021E+03 0.30451101430791E+03 0.30893950901663E+03 0.30315000000007E+03
 0.30315000000021E+03 0.30803257489908E+03 0.30315000000287E+03 0.96886489785837E+02 0.90668533906707E+02
 0.11206881869028E+03 0.34499757307785E+03 0.23236841029412E+03 0.85636278857217E+02 0.10556786791578E+03
 0.92476111001590E+02 0.27645148411706E+03 0.85129246058212E+02 0.10554310487820E+03 0.92010492964846E+02
 0.27642233699808E+03 0.85636278857220E+02 0.10556786791578E+03 0.92476111001596E+02 0.27645148411706E+03
 0.85129246058212E+02 0.10554310487820E+03 0.92010492964846E+02 0.27642233699808E+03 0.18413945222510E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35710734572797E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18011497652749E+00 0.00000000000000E+00 0.00000000000000E+00 0.18011497652749E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18859789437737E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18859789437737E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250517517460E+00 0.19141137446360E+00 0.33561853222588E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    330.00817568
 0.10089254457534E+00 0.31306993330824E+03 0.43694630186488E+03 0.43278023451940E+03 0.43120815198222E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17473837974800E+00 0.00000000000000E+00 -.20636841395858E+02
 0.33947725408453E-02 0.80119112689685E+00 0.23565643658729E+04 0.88371163720235E+03 0.99851330493204E+01
 0.37444248934951E+01 0.32941052217147E+03 0.30315000003428E+03 0.32540256491766E+03 0.34323580007867E+03
 0.30315000000138E+03 0.30315000000234E+03 0.32270373192574E+03 0.34320154899665E+03 0.30315000000108E+03
 0.30315000000232E+03 0.32540256491766E+03 0.34323580007867E+03 0.30315000000138E+03 0.30315000000234E+03
 0.32270373192574E+03 0.34320154899665E+03 0.30315000000108E+03 0.30315000000232E+03 0.38118581673518E+03
 0.30987709242353E+03 0.18112531480251E+04 0.17394378927836E+04 0.65878978686564E+03 0.12150339039068E+04
 0.55295016810686E+03 0.10549608908657E+04 0.10360183961780E+04 0.10000937414302E+04 0.17362612795999E+04
 0.93706859866627E+03 0.10351885162490E+04 0.89689569313607E+03 0.17357494708646E+04 0.10549608908657E+04
 0.10360183961780E+04 0.10000937414302E+04 0.17362612795999E+04 0.93706859866629E+03 0.10351885162490E+04
 0.89689569313609E+03 0.17357494708646E+04 0.17350235795571E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47681785700089E+03 0.12948565405199E+01
 0.12948565405199E+01 0.11400480375939E+01 0.79380279121381E-01 0.30738207145485E+03 0.33557839539455E+03
 0.33408624068486E+03 0.33395723428514E+03 0.23000000000000E+00 0.00000000000000E+00 0.20198586073651E+00
 0.00000000000000E+00 -.13600011300541E+02 0.21944295040762E-02 0.52103675996141E+00 0.36455944404411E+04
 0.13670979151654E+04 0.15354003046911E+02 0.57577511425916E+01 0.30987470782410E+03 0.38121194000496E+03
 0.30461831848433E+03 0.30908983411149E+03 0.30315000000008E+03 0.30315000000030E+03 0.30460724401186E+03
 0.30909029338651E+03 0.30315000000008E+03 0.30315000000030E+03 0.30461831848433E+03 0.30908983411149E+03
 0.30315000000008E+03 0.30315000000030E+03 0.30460724401186E+03 0.30909029338651E+03 0.30315000000008E+03
 0.30315000000030E+03 0.30816218283915E+03 0.30315000000429E+03 0.97820042416455E+02 0.91843677674951E+02
 0.11518343724480E+03 0.34655505734947E+03 0.23079570291844E+03 0.88624978772175E+02 0.10841559756604E+03
 0.97575708721137E+02 0.27764822361254E+03 0.88152977935734E+02 0.10838161964564E+03 0.97148394938211E+02
 0.27761071001752E+03 0.88624978772184E+02 0.10841559756604E+03 0.97575708721151E+02 0.27764822361254E+03
 0.88152977935734E+02 0.10838161964564E+03 0.97148394938211E+02 0.27761071001752E+03 0.18564434516357E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35718265344755E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18018469538055E+00 0.00000000000000E+00 0.00000000000000E+00 0.18018469538055E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18878974911672E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18878974911672E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250569756982E+00 0.19143481860350E+00 0.33557839539455E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    340.00308006
 0.10099577706127E+00 0.31333849952154E+03 0.43729732080954E+03 0.43312421498301E+03 0.43154985216065E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17355531168583E+00 0.00000000000000E+00 -.20658023510423E+02
 0.33913022672175E-02 0.81770267280242E+00 0.23589758062362E+04 0.88461592733857E+03 0.97835072161163E+01
 0.36688152060436E+01 0.33000998707069E+03 0.30315000005067E+03 0.32590980477323E+03 0.34402868420770E+03
 0.30315000000213E+03 0.30315000000363E+03 0.32317489939298E+03 0.34399496591139E+03 0.30315000000166E+03
 0.30315000000361E+03 0.32590980477323E+03 0.34402868420770E+03 0.30315000000213E+03 0.30315000000363E+03
 0.32317489939297E+03 0.34399496591139E+03 0.30315000000166E+03 0.30315000000361E+03 0.38223197545355E+03
 0.31038961841403E+03 0.18182261577591E+04 0.17445229412306E+04 0.65756137215188E+03 0.12053546947853E+04
 0.54450551577263E+03 0.10596140455774E+04 0.10401184688934E+04 0.10034330455838E+04 0.17359921384481E+04
 0.94202521961447E+03 0.10393255183608E+04 0.90080603537761E+03 0.17355117400582E+04 0.10596140455774E+04
 0.10401184688934E+04 0.10034330455838E+04 0.17359921384481E+04 0.94202521961449E+03 0.10393255183608E+04
 0.90080603537765E+03 0.17355117400582E+04 0.17338282350189E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47715962993184E+03 0.12948566965923E+01
 0.12948566965923E+01 0.11800276551047E+01 0.68628971484827E-01 0.30801333799976E+03 0.33555089328067E+03
 0.33429097721658E+03 0.33418392748195E+03 0.23000000000000E+00 0.00000000000000E+00 0.20079167528898E+00
 0.00000000000000E+00 -.13592983049982E+02 0.25382051416064E-02 0.54408939685376E+00 0.31518335018961E+04
 0.11819375632110E+04 0.14703466096308E+02 0.55137997861156E+01 0.31038721818675E+03 0.38225677730392E+03
 0.30469493835175E+03 0.30923810192617E+03 0.30315000000009E+03 0.30315000000046E+03 0.30468461516070E+03
 0.30923844811210E+03 0.30315000000009E+03 0.30315000000046E+03 0.30469493835175E+03 0.30923810192617E+03
 0.30315000000009E+03 0.30315000000046E+03 0.30468461516070E+03 0.30923844811210E+03 0.30315000000009E+03
 0.30315000000046E+03 0.30828953200234E+03 0.30315000000635E+03 0.98488576723987E+02 0.92896431327951E+02
 0.11816141088193E+03 0.34811743575916E+03 0.22936521782282E+03 0.91699833810599E+02 0.11113401407566E+03
 0.91699833810599E+02 0.27884801556341E+03 0.91260403349663E+02 0.11109110887452E+03 0.91260403349663E+02
 0.27880245171057E+03 0.91699833810601E+02 0.11113401407566E+03 0.91699833810601E+02 0.27884801556341E+03
 0.91260403349663E+02 0.11109110887452E+03 0.91260403349663E+02 0.27880245171057E+03 0.18692134520898E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35727487346815E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18021223249143E+00 0.00000000000000E+00 0.00000000000000E+00 0.18021223249143E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18892634224883E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18892634224883E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250633153525E+00 0.19145118713094E+00 0.33555089328067E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    350.00903073
 0.10108045170490E+00 0.31360329819886E+03 0.43765562145727E+03 0.43347586650060E+03 0.43189919978866E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17240921398582E+00 0.00000000000000E+00 -.20687494056949E+02
 0.33884610630637E-02 0.83363818391922E+00 0.23609537932145E+04 0.88535767245546E+03 0.95964894054987E+01
 0.35986835270620E+01 0.33059979141229E+03 0.30315000007375E+03 0.32640940388317E+03 0.34480721965505E+03
 0.30315000000324E+03 0.30315000000554E+03 0.32363930668295E+03 0.34477401561170E+03 0.30315000000252E+03
 0.30315000000552E+03 0.32640940388317E+03 0.34480721965505E+03 0.30315000000324E+03 0.30315000000554E+03
 0.32363930668294E+03 0.34477401561170E+03 0.30315000000252E+03 0.30315000000552E+03 0.38324971969461E+03
 0.31092022309213E+03 0.18249569989308E+04 0.17493899038760E+04 0.65633832910200E+03 0.11961301249962E+04
 0.53651010424869E+03 0.10641220644695E+04 0.10440620755130E+04 0.10066413553871E+04 0.17357696550787E+04
 0.94683333712287E+03 0.10433034646446E+04 0.90457721081067E+03 0.17353182845920E+04 0.10641220644695E+04
 0.10440620755130E+04 0.10066413553871E+04 0.17357696550787E+04 0.94683333712290E+03 0.10433034646446E+04
 0.90457721081071E+03 0.17353182845920E+04 0.17327164622747E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47750908304412E+03 0.12948569137349E+01
 0.12948569137349E+01 0.12200514577861E+01 0.58746623370424E-01 0.30874161516688E+03 0.33553667625930E+03
 0.33448726335118E+03 0.33440005031696E+03 0.23000000000000E+00 0.00000000000000E+00 0.19959485051069E+00
 0.00000000000000E+00 -.13595375037798E+02 0.29651813366620E-02 0.56714052514518E+00 0.26979800193285E+04
 0.10117425072482E+04 0.14105851451811E+02 0.52896942944291E+01 0.31091781024722E+03 0.38327321079700E+03
 0.30475938280972E+03 0.30938431993359E+03 0.30315000000011E+03 0.30315000000067E+03 0.30474859922922E+03
 0.30938454930843E+03 0.30315000000011E+03 0.30315000000067E+03 0.30475938280972E+03 0.30938431993359E+03
 0.30315000000011E+03 0.30315000000067E+03 0.30474859922922E+03 0.30938454930843E+03 0.30315000000011E+03
 0.30315000000067E+03 0.30841511674672E+03 0.30315000000929E+03 0.98899444644043E+02 0.93927059348657E+02
 0.12102221964579E+03 0.34971156966672E+03 0.22808423892270E+03 0.94834642042110E+02 0.11374143556012E+03
 0.94834642042110E+02 0.28007367877304E+03 0.94436604343191E+02 0.11368986690100E+03 0.94436604343191E+02
 0.28002035244568E+03 0.94834642042113E+02 0.11374143556012E+03 0.94834642042113E+02 0.28007367877304E+03
 0.94436604343191E+02 0.11368986690100E+03 0.94436604343191E+02 0.28002035244568E+03 0.18831092180112E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35738856848938E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18030355179249E+00 0.00000000000000E+00 0.00000000000000E+00 0.18030355179249E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18908475884183E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18908475884183E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250661497648E+00 0.19145959193108E+00 0.33553667625930E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    360.04178078
 0.10116592251381E+00 0.31386386322031E+03 0.43802033566025E+03 0.43383353430343E+03 0.43225426018893E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17129733983172E+00 0.00000000000000E+00 -.20716956767923E+02
 0.33855979662410E-02 0.84903979063721E+00 0.23629503797471E+04 0.88610639240515E+03 0.94224088060654E+01
 0.35334033022745E+01 0.33118136929895E+03 0.30315000010594E+03 0.32690253720741E+03 0.34557363618567E+03
 0.30315000000483E+03 0.30315000000833E+03 0.32409802112676E+03 0.34554092879624E+03 0.30315000000376E+03
 0.30315000000829E+03 0.32690253720741E+03 0.34557363618567E+03 0.30315000000483E+03 0.30315000000833E+03
 0.32409802112675E+03 0.34554092879624E+03 0.30315000000376E+03 0.30315000000829E+03 0.38424303494957E+03
 0.31146788937248E+03 0.18314618500080E+04 0.17540443469836E+04 0.65510919486014E+03 0.11872873547804E+04
 0.52890261394591E+03 0.10684977219565E+04 0.10478615966346E+04 0.10097245865571E+04 0.17355780469148E+04
 0.95150591798190E+03 0.10471349885243E+04 0.90821590623460E+03 0.17351535620777E+04 0.10684977219565E+04
 0.10478615966346E+04 0.10097245865571E+04 0.17355780469148E+04 0.95150591798192E+03 0.10471349885243E+04
 0.90821590623464E+03 0.17351535620777E+04 0.17316601311549E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47786438549961E+03 0.12948571308199E+01
 0.12948571308199E+01 0.12601824579938E+01 0.49692095167286E-01 0.30949975190145E+03 0.33553665686676E+03
 0.33467410462716E+03 0.33460414060758E+03 0.23000000000000E+00 0.00000000000000E+00 0.19839346755595E+00
 0.00000000000000E+00 -.13598250263264E+02 0.35054745611615E-02 0.59022596887930E+00 0.22821446455881E+04
 0.85580424209554E+03 0.13554130827537E+02 0.50827990603265E+01 0.31146547137771E+03 0.38426523557699E+03
 0.30483486376043E+03 0.30952905251469E+03 0.30315000000013E+03 0.30315000000099E+03 0.30482411136286E+03
 0.30952916196303E+03 0.30315000000013E+03 0.30315000000099E+03 0.30483486376043E+03 0.30952905251469E+03
 0.30315000000013E+03 0.30315000000099E+03 0.30482411136286E+03 0.30952916196303E+03 0.30315000000013E+03
 0.30315000000099E+03 0.30853940754754E+03 0.30315000001343E+03 0.99079877783821E+02 0.94745040463584E+02
 0.12378373387114E+03 0.35135409420693E+03 0.22695144166644E+03 0.97907210654509E+02 0.11625569286176E+03
 0.97907210654509E+02 0.28133927679402E+03 0.97549868513113E+02 0.11619576658038E+03 0.97549868513113E+02
 0.28127851352998E+03 0.97907210654510E+02 0.11625569286176E+03 0.97907210654510E+02 0.28133927679402E+03
 0.97549868513113E+02 0.11619576658038E+03 0.97549868513113E+02 0.28127851352998E+03 0.18963408001991E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35751956399367E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18039263192164E+00 0.00000000000000E+00 0.00000000000000E+00 0.18039263192164E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18922521032847E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18922521032847E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250681602536E+00 0.19145980847258E+00 0.33553665686676E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    370.01657453
 0.10124905826517E+00 0.31411943946599E+03 0.43838736204316E+03 0.43419335866532E+03 0.43261130037016E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17022771566081E+00 0.00000000000000E+00 -.20745599686730E+02
 0.33828177165757E-02 0.86380013944117E+00 0.23648924270440E+04 0.88683466014149E+03 0.92614016075241E+01
 0.34730256028216E+01 0.33175061042177E+03 0.30315000014976E+03 0.32738570034020E+03 0.34632246335787E+03
 0.30315000000709E+03 0.30315000001231E+03 0.32454780104053E+03 0.34629023266949E+03 0.30315000000551E+03
 0.30315000001225E+03 0.32738570034020E+03 0.34632246335787E+03 0.30315000000709E+03 0.30315000001231E+03
 0.32454780104053E+03 0.34629023266949E+03 0.30315000000551E+03 0.30315000001225E+03 0.38520472578913E+03
 0.31202641503135E+03 0.18377060613150E+04 0.17584719399785E+04 0.65389437513713E+03 0.11788823913160E+04
 0.52171854430318E+03 0.10727158504057E+04 0.10514965094599E+04 0.10126730970108E+04 0.17354117319497E+04
 0.95601544333630E+03 0.10507995112041E+04 0.91170779016024E+03 0.17350119575691E+04 0.10727158504057E+04
 0.10514965094599E+04 0.10126730970108E+04 0.17354117319497E+04 0.95601544333633E+03 0.10507995112041E+04
 0.91170779016028E+03 0.17350119575691E+04 0.17306662383608E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47822175710599E+03 0.12948573418646E+01
 0.12948573418646E+01 0.13000816329674E+01 0.41520362386658E-01 0.31027615681737E+03 0.33555028528650E+03
 0.33485069130447E+03 0.33479540661943E+03 0.23000000000000E+00 0.00000000000000E+00 0.19719795664134E+00
 0.00000000000000E+00 -.13601115377452E+02 0.41953959746811E-02 0.61314668517619E+00 0.19068521894666E+04
 0.71506957104997E+03 0.13047448829804E+02 0.48927933111763E+01 0.31202400736764E+03 0.38522567191382E+03
 0.30491325150730E+03 0.30967138930690E+03 0.30315000000015E+03 0.30315000000143E+03 0.30490263153672E+03
 0.30967137730055E+03 0.30315000000015E+03 0.30315000000143E+03 0.30491325150730E+03 0.30967138930690E+03
 0.30315000000015E+03 0.30315000000143E+03 0.30490263153672E+03 0.30967137730055E+03 0.30315000000015E+03
 0.30315000000143E+03 0.30866164044036E+03 0.30315000001916E+03 0.99042664246675E+02 0.95342294314234E+02
 0.12642942399099E+03 0.35302939613643E+03 0.22596782502549E+03 0.10093405935642E+03 0.11866279343430E+03
 0.10093405935642E+03 0.28263323214546E+03 0.10061792037561E+03 0.11859493503620E+03 0.10061792037561E+03
 0.28256546539372E+03 0.10093405935642E+03 0.11866279343430E+03 0.10093405935642E+03 0.28263323214546E+03
 0.10061792037561E+03 0.11859493503620E+03 0.10061792037561E+03 0.28256546539372E+03 0.19088137524128E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35766578381766E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18047449868364E+00 0.00000000000000E+00 0.00000000000000E+00 0.18047449868364E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18934710880397E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18934710880397E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250695066113E+00 0.19145216812536E+00 0.33555028528650E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    380.01844618
 0.10132938582086E+00 0.31437238257610E+03 0.43875885099904E+03 0.43455751618247E+03 0.43297252043835E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16918989474636E+00 0.00000000000000E+00 -.20773583315576E+02
 0.33801356986517E-02 0.87806727807509E+00 0.23667688854004E+04 0.88753833202516E+03 0.91109191741408E+01
 0.34165946903028E+01 0.33231285021134E+03 0.30315000020916E+03 0.32786337666309E+03 0.34706102066891E+03
 0.30315000001029E+03 0.30315000001794E+03 0.32499278078114E+03 0.34702925072850E+03 0.30315000000800E+03
 0.30315000001786E+03 0.32786337666309E+03 0.34706102066891E+03 0.30315000001029E+03 0.30315000001794E+03
 0.32499278078114E+03 0.34702925072850E+03 0.30315000000800E+03 0.30315000001786E+03 0.38614569934403E+03
 0.31259908188401E+03 0.18437615282008E+04 0.17627279531271E+04 0.65267208507101E+03 0.11707863627840E+04
 0.51485091728768E+03 0.10768232070204E+04 0.10550069190338E+04 0.10155219485346E+04 0.17352607309908E+04
 0.96041150033190E+03 0.10543375846771E+04 0.91509328972188E+03 0.17348838947486E+04 0.10768232070204E+04
 0.10550069190338E+04 0.10155219485346E+04 0.17352607309908E+04 0.96041150033192E+03 0.10543375846771E+04
 0.91509328972192E+03 0.17348838947486E+04 0.17297122407810E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47858338187819E+03 0.12948575480517E+01
 0.12948575480517E+01 0.13400891195727E+01 0.34139784253267E-01 0.31107177134648E+03 0.33557695469864E+03
 0.33501922025015E+03 0.33497636064617E+03 0.23000000000000E+00 0.00000000000000E+00 0.19599841127013E+00
 0.00000000000000E+00 -.13603987191912E+02 0.51023857356055E-02 0.63609415399460E+00 0.15678940038136E+04
 0.58796025143011E+03 0.12576754478501E+02 0.47162829294379E+01 0.31259669821852E+03 0.38616542348114E+03
 0.30499389918973E+03 0.30981274347023E+03 0.30315000000018E+03 0.30315000000207E+03 0.30498347895091E+03
 0.30981260849687E+03 0.30315000000018E+03 0.30315000000207E+03 0.30499389918973E+03 0.30981274347023E+03
 0.30315000000018E+03 0.30315000000207E+03 0.30498347895091E+03 0.30981260849687E+03 0.30315000000018E+03
 0.30315000000207E+03 0.30878301370352E+03 0.30315000002702E+03 0.98800953455092E+02 0.95721188170968E+02
 0.12898809915849E+03 0.35474920759299E+03 0.22511616793871E+03 0.10394721728498E+03 0.12098963885767E+03
 0.10394721728498E+03 0.28396465131088E+03 0.10367280375388E+03 0.12091423887118E+03 0.10367280375388E+03
 0.28389027986073E+03 0.10394721728498E+03 0.12098963885767E+03 0.10394721728498E+03 0.28396465131088E+03
 0.10367280375388E+03 0.12091423887118E+03 0.10367280375388E+03 0.28389027986073E+03 0.19206366817922E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35782738976992E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18054953985268E+00 0.00000000000000E+00 0.00000000000000E+00 0.18054953985268E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18945231121452E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18945231121452E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250702413554E+00 0.19143702442849E+00 0.33557695469864E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    390.01141772
 0.10140501188623E+00 0.31462200921979E+03 0.43913270287535E+03 0.43492403341865E+03 0.43333600726970E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16818668181399E+00 0.00000000000000E+00 -.20800374379000E+02
 0.33776145188684E-02 0.89180579169316E+00 0.23685355316036E+04 0.88820082435134E+03 0.89705629572235E+01
 0.33639611089588E+01 0.33286653555050E+03 0.30315000028849E+03 0.32833422144106E+03 0.34778731472807E+03
 0.30315000001472E+03 0.30315000002581E+03 0.32543170615179E+03 0.34775598879466E+03 0.30315000001146E+03
 0.30315000002568E+03 0.32833422144106E+03 0.34778731472807E+03 0.30315000001472E+03 0.30315000002581E+03
 0.32543170615179E+03 0.34775598879466E+03 0.30315000001146E+03 0.30315000002568E+03 0.38706382998886E+03
 0.31318257808978E+03 0.18496211780802E+04 0.17668109107915E+04 0.65144841998496E+03 0.11630044342511E+04
 0.50829877216627E+03 0.10808135059908E+04 0.10583882347110E+04 0.10182686203652E+04 0.17351204188328E+04
 0.96468690551587E+03 0.10577446760802E+04 0.91836829024508E+03 0.17347648128852E+04 0.10808135059908E+04
 0.10583882347110E+04 0.10182686203652E+04 0.17351204188328E+04 0.96468690551588E+03 0.10577446760802E+04
 0.91836829024509E+03 0.17347648128852E+04 0.17287965689386E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47894732527998E+03 0.12948577454518E+01
 0.12948577454518E+01 0.13800610057355E+01 0.27571922197947E-01 0.31187695584697E+03 0.33561604660998E+03
 0.33517969170094E+03 0.33514713380848E+03 0.23000000000000E+00 0.00000000000000E+00 0.19479917729544E+00
 0.00000000000000E+00 -.13606465169426E+02 0.63178161875635E-02 0.65898597320750E+00 0.12662603283312E+04
 0.47484762312418E+03 0.12139863859410E+02 0.45524489472788E+01 0.31318023002080E+03 0.38708237493808E+03
 0.30507606547824E+03 0.30995278826790E+03 0.30315000000022E+03 0.30315000000295E+03 0.30506589816546E+03
 0.30995252990256E+03 0.30315000000022E+03 0.30315000000295E+03 0.30507606547824E+03 0.30995278826790E+03
 0.30315000000022E+03 0.30315000000295E+03 0.30506589816546E+03 0.30995252990256E+03 0.30315000000022E+03
 0.30315000000295E+03 0.30890325712956E+03 0.30315000003766E+03 0.98366691126408E+02 0.95873025422223E+02
 0.13145647250834E+03 0.35651421849333E+03 0.22440046362245E+03 0.10693743523143E+03 0.12323389479364E+03
 0.10693743523143E+03 0.28532328361804E+03 0.10670456264250E+03 0.12315139305476E+03 0.10670456264250E+03
 0.28524274764550E+03 0.10693743523143E+03 0.12323389479364E+03 0.10693743523143E+03 0.28532328361805E+03
 0.10670456264250E+03 0.12315139305476E+03 0.10670456264250E+03 0.28524274764549E+03 0.19317623514479E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35802224126635E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18061427274943E+00 0.00000000000000E+00 0.00000000000000E+00 0.18061427274943E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18954056704720E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18954056704720E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250705114095E+00 0.19141474985230E+00 0.33561604660998E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    400.00205736
 0.10147547714844E+00 0.31486868339155E+03 0.43950856707912E+03 0.43529260318948E+03 0.43370148026445E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16721629079000E+00 0.00000000000000E+00 -.20826928660953E+02
 0.33752687449685E-02 0.90504328924404E+00 0.23701816372179E+04 0.88881811395673E+03 0.88393561888981E+01
 0.33147585708368E+01 0.33341248272833E+03 0.30315000039336E+03 0.32879890031551E+03 0.34850248377208E+03
 0.30315000002082E+03 0.30315000003665E+03 0.32586518875470E+03 0.34847158614081E+03 0.30315000001623E+03
 0.30315000003647E+03 0.32879890031551E+03 0.34850248377208E+03 0.30315000002082E+03 0.30315000003665E+03
 0.32586518875470E+03 0.34847158614081E+03 0.30315000001623E+03 0.30315000003647E+03 0.38796115345532E+03
 0.31377601389474E+03 0.18553038964491E+04 0.17707375794145E+04 0.65022315860434E+03 0.11555064263742E+04
 0.50203215197681E+03 0.10846981471063E+04 0.10616517976583E+04 0.10209229989757E+04 0.17349881325667E+04
 0.96885339090064E+03 0.10610322951991E+04 0.92154337324397E+03 0.17346522057251E+04 0.10846981471063E+04
 0.10616517976583E+04 0.10209229989757E+04 0.17349881325667E+04 0.96885339090065E+03 0.10610322951991E+04
 0.92154337324399E+03 0.17346522057251E+04 0.17279147579298E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47931329503736E+03 0.12948579411074E+01
 0.12948579411074E+01 0.14200235643029E+01 0.22113389998430E-01 0.31268198894772E+03 0.33567177156394E+03
 0.33533285021129E+03 0.33530832513873E+03 0.23000000000000E+00 0.00000000000000E+00 0.19359145147414E+00
 0.00000000000000E+00 -.13610006577066E+02 0.78773234655574E-02 0.68197694682610E+00 0.10155733778077E+04
 0.38084001667789E+03 0.11730601800005E+02 0.43989756750018E+01 0.31377371125862E+03 0.38797856273691E+03
 0.30515956297315E+03 0.31009189810250E+03 0.30315000000027E+03 0.30315000000417E+03 0.30514968408964E+03
 0.31009151660570E+03 0.30315000000027E+03 0.30315000000417E+03 0.30515956297315E+03 0.31009189810250E+03
 0.30315000000027E+03 0.30315000000417E+03 0.30514968408964E+03 0.31009151660570E+03 0.30315000000027E+03
 0.30315000000417E+03 0.30902281093920E+03 0.30315000005188E+03 0.97764596044492E+02 0.95798773358607E+02
 0.13386925178924E+03 0.35841305111844E+03 0.22387445307025E+03 0.10991071062322E+03 0.12543143031090E+03
 0.10991071062322E+03 0.28676515476288E+03 0.10971577490981E+03 0.12534231881977E+03 0.10971577490981E+03
 0.28667894174910E+03 0.10991071062322E+03 0.12543143031090E+03 0.10991071062322E+03 0.28676515476288E+03
 0.10971577490981E+03 0.12534231881977E+03 0.10971577490981E+03 0.28667894174910E+03 0.19424767614426E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35830008878341E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18067650954609E+00 0.00000000000000E+00 0.00000000000000E+00 0.18067650954609E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18961851008658E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18961851008658E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250697314567E+00 0.19138288456162E+00 0.33567177156394E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    410.01004552
 0.10154103495600E+00 0.31511304857434E+03 0.43988667358494E+03 0.43566345924535E+03 0.43406918298513E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16627584484875E+00 0.00000000000000E+00 -.20852347616886E+02
 0.33730892378740E-02 0.91782187088687E+00 0.23717131198825E+04 0.88939241995592E+03 0.87162882621982E+01
 0.32686080983243E+01 0.33395214508188E+03 0.30315000053085E+03 0.32925862581806E+03 0.34920848847852E+03
 0.30315000002910E+03 0.30315000005144E+03 0.32629435174116E+03 0.34917800494356E+03 0.30315000002272E+03
 0.30315000005119E+03 0.32925862581806E+03 0.34920848847852E+03 0.30315000002910E+03 0.30315000005144E+03
 0.32629435174116E+03 0.34917800494356E+03 0.30315000002272E+03 0.30315000005119E+03 0.38884067496258E+03
 0.31437916382674E+03 0.18608334239411E+04 0.17745273997895E+04 0.64899412403804E+03 0.11482557272377E+04
 0.49601663257942E+03 0.10884921663873E+04 0.10648118388626E+04 0.10234971307382E+04 0.17348615642787E+04
 0.97292665309998E+03 0.10642148510710E+04 0.92463187386390E+03 0.17345439269393E+04 0.10884921663873E+04
 0.10648118388626E+04 0.10234971307382E+04 0.17348615642787E+04 0.97292665309999E+03 0.10642148510710E+04
 0.92463187386392E+03 0.17345439269393E+04 0.17270615897474E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47968152862169E+03 0.12948581283978E+01
 0.12948581283978E+01 0.14600555169283E+01 0.17724642215353E-01 0.31347380131683E+03 0.33574196739288E+03
 0.33547883720855E+03 0.33546038165369E+03 0.23000000000000E+00 0.00000000000000E+00 0.19237325582376E+00
 0.00000000000000E+00 -.13613496865777E+02 0.98278043997877E-02 0.70510742151304E+00 0.81401701484544E+03
 0.30525638056704E+03 0.11345788961962E+02 0.42546708607357E+01 0.31437691581094E+03 0.38885699157735E+03
 0.30524439922706E+03 0.31023070681245E+03 0.30315000000034E+03 0.30315000000583E+03 0.30523482515756E+03
 0.31023020296215E+03 0.30315000000034E+03 0.30315000000584E+03 0.30524439922706E+03 0.31023070681245E+03
 0.30315000000034E+03 0.30315000000583E+03 0.30523482515756E+03 0.31023020296215E+03 0.30315000000034E+03
 0.30315000000584E+03 0.30914206000008E+03 0.30315000007074E+03 0.96996358800727E+02 0.95472312463683E+02
 0.13622722251147E+03 0.36038013931288E+03 0.22347178068885E+03 0.11286515577473E+03 0.12758123616374E+03
 0.11286515577473E+03 0.28829474603323E+03 0.11270340417357E+03 0.12748601120021E+03 0.11270340417357E+03
 0.28820334463224E+03 0.11286515577473E+03 0.12758123616374E+03 0.11286515577473E+03 0.28829474603323E+03
 0.11270340417357E+03 0.12748601120021E+03 0.11270340417357E+03 0.28820334463224E+03 0.19526005896939E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35855767383924E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18072893496004E+00 0.00000000000000E+00 0.00000000000000E+00 0.18072893496004E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18968211119760E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18968211119760E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250683260456E+00 0.19134271691659E+00 0.33574196739288E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    420.00064108
 0.10159858212951E+00 0.31535465339677E+03 0.44026529378434E+03 0.43603504579892E+03 0.43443763620719E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16536758331244E+00 0.00000000000000E+00 -.20875546063866E+02
 0.33711783256504E-02 0.93011393295881E+00 0.23730574971754E+04 0.88989656144079E+03 0.86010968296658E+01
 0.32254113111247E+01 0.33448401578245E+03 0.30315000070876E+03 0.32971209076022E+03 0.34990339610689E+03
 0.30315000004022E+03 0.30315000007135E+03 0.32671796706388E+03 0.34987331169463E+03 0.30315000003143E+03
 0.30315000007101E+03 0.32971209076022E+03 0.34990339610689E+03 0.30315000004022E+03 0.30315000007135E+03
 0.32671796706388E+03 0.34987331169463E+03 0.30315000003143E+03 0.30315000007101E+03 0.38970051616484E+03
 0.31498850181365E+03 0.18662044905295E+04 0.17781818473921E+04 0.64776625117775E+03 0.11412582211961E+04
 0.49025313876243E+03 0.10921902101253E+04 0.10678653298862E+04 0.10259903940670E+04 0.17347402948641E+04
 0.97690055459285E+03 0.10672893609559E+04 0.92763172344268E+03 0.17344396099352E+04 0.10921902101253E+04
 0.10678653298862E+04 0.10259903940670E+04 0.17347402948641E+04 0.97690055459286E+03 0.10672893609559E+04
 0.92763172344270E+03 0.17344396099352E+04 0.17262390278105E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48005051836414E+03 0.12948582993273E+01
 0.12948582993273E+01 0.15000178991820E+01 0.14208913399612E-01 0.31424269232475E+03 0.33582199370976E+03
 0.33561758142669E+03 0.33560368627607E+03 0.23000000000000E+00 0.00000000000000E+00 0.19115416848619E+00
 0.00000000000000E+00 -.13615496216454E+02 0.12259509486792E-01 0.72820522404857E+00 0.65255465633590E+03
 0.24470799612596E+03 0.10985914047036E+02 0.41197177676384E+01 0.31498631799550E+03 0.38971578772683E+03
 0.30533013605197E+03 0.31036874540337E+03 0.30315000000043E+03 0.30315000000807E+03 0.30532087174794E+03
 0.31036812092336E+03 0.30315000000042E+03 0.30315000000807E+03 0.30533013605197E+03 0.31036874540337E+03
 0.30315000000043E+03 0.30315000000807E+03 0.30532087174794E+03 0.31036812092336E+03 0.30315000000042E+03
 0.30315000000807E+03 0.30926055633016E+03 0.30315000009539E+03 0.96062613158490E+02 0.94888320314112E+02
 0.13850494427881E+03 0.36235300007116E+03 0.22315553107096E+03 0.11578166308990E+03 0.12965775712100E+03
 0.11578166308990E+03 0.28985034513570E+03 0.11564873202421E+03 0.12955690993381E+03 0.11564873202421E+03
 0.28975423223647E+03 0.11578166308990E+03 0.12965775712100E+03 0.11578166308990E+03 0.28985034513570E+03
 0.11564873202421E+03 0.12955690993381E+03 0.11564873202421E+03 0.28975423223647E+03 0.19618677966782E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35879655502900E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18076344642161E+00 0.00000000000000E+00 0.00000000000000E+00 0.18076344642161E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18972614955550E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18972614955550E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250669057573E+00 0.19129696733773E+00 0.33582199370976E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    430.51520951
 0.10164846056070E+00 0.31560528520182E+03 0.44066421733608E+03 0.43642686802581E+03 0.43482619570949E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16444355095304E+00 0.00000000000000E+00 -.20900599520411E+02
 0.33695237492724E-02 0.94256842095240E+00 0.23742227671574E+04 0.89033353768401E+03 0.84874475127403E+01
 0.31827928172776E+01 0.33503621119491E+03 0.30315000095253E+03 0.33018320778858E+03 0.35062465271824E+03
 0.30315000005601E+03 0.30315000009974E+03 0.32715826110096E+03 0.35059497073873E+03 0.30315000004385E+03
 0.30315000009927E+03 0.33018320778858E+03 0.35062465271824E+03 0.30315000005601E+03 0.30315000009974E+03
 0.32715826110096E+03 0.35059497073873E+03 0.30315000004385E+03 0.30315000009927E+03 0.39059030249558E+03
 0.31563682662199E+03 0.18717091105343E+04 0.17818959197453E+04 0.64642744990356E+03 0.11340455816402E+04
 0.48438599448710E+03 0.10959925482564E+04 0.10709718962246E+04 0.10285334201043E+04 0.17346076123890E+04
 0.98099083178270E+03 0.10704166030161E+04 0.93070224488385E+03 0.17343235053023E+04 0.10959925482564E+04
 0.10709718962246E+04 0.10285334201043E+04 0.17346076123890E+04 0.98099083178271E+03 0.10704166030161E+04
 0.93070224488386E+03 0.17343235053023E+04 0.17253748303830E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48043964587402E+03 0.12948584839248E+01
 0.12948584839248E+01 0.15420761729011E+01 0.11256858856527E-01 0.31503007154325E+03 0.33591377508771E+03
 0.33575705181891E+03 0.33574674554925E+03 0.23000000000000E+00 0.00000000000000E+00 0.18987286466535E+00
 0.00000000000000E+00 -.13618599609943E+02 0.15474503428051E-01 0.75243588568638E+00 0.51697943247072E+03
 0.19386728717652E+03 0.10632135112352E+02 0.39870506671321E+01 0.31563471944656E+03 0.39060452975814E+03
 0.30542082940758E+03 0.31051309967613E+03 0.30315000000056E+03 0.30315000001127E+03 0.30541188498477E+03
 0.31051235110190E+03 0.30315000000055E+03 0.30315000001127E+03 0.30542082940758E+03 0.31051309967613E+03
 0.30315000000056E+03 0.30315000001127E+03 0.30541188498477E+03 0.31051235110190E+03 0.30315000000055E+03
 0.30315000001127E+03 0.30938433564700E+03 0.30315000012954E+03 0.94898629327331E+02 0.94008918728234E+02
 0.14080978867545E+03 0.36440493084919E+03 0.22289109323037E+03 0.11880969904400E+03 0.13175778143082E+03
 0.11880969904400E+03 0.29148222636989E+03 0.11870295887142E+03 0.13165151445641E+03 0.11870295887142E+03
 0.29138162942446E+03 0.11880969904400E+03 0.13175778143082E+03 0.11880969904400E+03 0.29148222636989E+03
 0.11870295887142E+03 0.13165151445641E+03 0.11870295887142E+03 0.29138162942446E+03 0.19706316459012E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35903097564470E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18080483413660E+00 0.00000000000000E+00 0.00000000000000E+00 0.18080483413660E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18974639530008E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18974639530008E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250647292651E+00 0.19124446553257E+00 0.33591377508771E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    440.08922450
 0.10168641238021E+00 0.31583008859336E+03 0.44102723231443E+03 0.43678361618602E+03 0.43517998448211E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16362955263456E+00 0.00000000000000E+00 -.20917145755552E+02
 0.33682658393418E-02 0.95349609791857E+00 0.23751094425383E+04 0.89066604095187E+03 0.83901759194019E+01
 0.31463159697757E+01 0.33553267149437E+03 0.30315000123956E+03 0.33060708706073E+03 0.35127238774104E+03
 0.30315000007531E+03 0.30315000013452E+03 0.32755467593910E+03 0.35124305934189E+03 0.30315000005904E+03
 0.30315000013390E+03 0.33060708706073E+03 0.35127238774104E+03 0.30315000007531E+03 0.30315000013452E+03
 0.32755467593909E+03 0.35124305934189E+03 0.30315000005904E+03 0.30315000013390E+03 0.39138413871881E+03
 0.31623253606142E+03 0.18765907169967E+04 0.17851583055325E+04 0.64520786492395E+03 0.11276793143467E+04
 0.47924541009812E+03 0.10993760572369E+04 0.10737089146584E+04 0.10307752465136E+04 0.17344806021798E+04
 0.98463378822733E+03 0.10731712663578E+04 0.93341842971699E+03 0.17342105435098E+04 0.10993760572369E+04
 0.10737089146584E+04 0.10307752465136E+04 0.17344806021798E+04 0.98463378822734E+03 0.10731712663578E+04
 0.93341842971700E+03 0.17342105435098E+04 0.17246033290701E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48079396202978E+03 0.12948586058399E+01
 0.12948586058399E+01 0.15803722328703E+01 0.91024455565596E-02 0.31573109289088E+03 0.33600255501523E+03
 0.33587954176167E+03 0.33587169470127E+03 0.23000000000000E+00 0.00000000000000E+00 0.18871131216612E+00
 0.00000000000000E+00 -.13615632103949E+02 0.19137087229171E-01 0.77436495531372E+00 0.41803644954940E+03
 0.15676366858103E+03 0.10331046033403E+02 0.38741422625261E+01 0.31623050246873E+03 0.39139747953865E+03
 0.30550416729803E+03 0.31064382989381E+03 0.30315000000072E+03 0.30315000001519E+03 0.30549550849532E+03
 0.31064297035231E+03 0.30315000000071E+03 0.30315000001520E+03 0.30550416729803E+03 0.31064382989381E+03
 0.30315000000072E+03 0.30315000001519E+03 0.30549550849532E+03 0.31064297035231E+03 0.30315000000071E+03
 0.30315000001520E+03 0.30949634113331E+03 0.30315000017016E+03 0.93684323944715E+02 0.92995874109057E+02
 0.14282575027697E+03 0.36623837416731E+03 0.22269849513896E+03 0.12153538501950E+03 0.13359202466493E+03
 0.12153538501950E+03 0.29294694586411E+03 0.12144922517747E+03 0.13348118054454E+03 0.12144922517747E+03
 0.29284260776229E+03 0.12153538501950E+03 0.13359202466493E+03 0.12153538501950E+03 0.29294694586411E+03
 0.12144922517747E+03 0.13348118054454E+03 0.12144922517747E+03 0.29284260776229E+03 0.19777574313060E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35923260675932E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18079243068884E+00 0.00000000000000E+00 0.00000000000000E+00 0.18079243068884E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18975383442193E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18975383442193E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250642614157E+00 0.19119389075170E+00 0.33600255501523E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    454.15986590
 0.10171058934527E+00 0.31615890299715E+03 0.44156132555423E+03 0.43730974078636E+03 0.43570216838712E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16247873153234E+00 0.00000000000000E+00 -.20943691959793E+02
 0.33674647143955E-02 0.96886923288647E+00 0.23756744846653E+04 0.89087793174949E+03 0.82570482459911E+01
 0.30963930922467E+01 0.33625175381469E+03 0.30315000180488E+03 0.33122150489169E+03 0.35220982413705E+03
 0.30315000011498E+03 0.30315000020628E+03 0.32812961266910E+03 0.35218099197856E+03 0.30315000009037E+03
 0.30315000020534E+03 0.33122150489169E+03 0.35220982413705E+03 0.30315000011498E+03 0.30315000020628E+03
 0.32812961266910E+03 0.35218099197856E+03 0.30315000009037E+03 0.30315000020534E+03 0.39252884522604E+03
 0.31711621993674E+03 0.18835920999183E+04 0.17898247979022E+04 0.64338179023901E+03 0.11185744205710E+04
 0.47197572138078E+03 0.11042406439344E+04 0.10776060229624E+04 0.10339924361339E+04 0.17343136893359E+04
 0.98987635782867E+03 0.10770924279217E+04 0.93732377798463E+03 0.17340626400425E+04 0.11042406439344E+04
 0.10776060229624E+04 0.10339924361339E+04 0.17343136893359E+04 0.98987635782867E+03 0.10770924279217E+04
 0.93732377798464E+03 0.17340626400425E+04 0.17235132213802E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48131677486995E+03 0.12948588014363E+01
 0.12948588014363E+01 0.16366547984502E+01 0.66595377904024E-02 0.31673461201653E+03 0.33614009123388E+03
 0.33605393688576E+03 0.33604868330387E+03 0.23000000000000E+00 0.00000000000000E+00 0.18701902289508E+00
 0.00000000000000E+00 -.13613131134296E+02 0.26157113536450E-01 0.80625485643177E+00 0.30584414403569E+03
 0.11469155401338E+03 0.99224208526389E+01 0.37209078197396E+01 0.31711430491508E+03 0.39254092753578E+03
 0.30562722999295E+03 0.31083430088474E+03 0.30315000000108E+03 0.30315000002333E+03 0.30561897196550E+03
 0.31083328279944E+03 0.30315000000107E+03 0.30315000002333E+03 0.30562722999295E+03 0.31083430088474E+03
 0.30315000000108E+03 0.30315000002333E+03 0.30561897196550E+03 0.31083328279944E+03 0.30315000000107E+03
 0.30315000002333E+03 0.30965942526319E+03 0.30315000025116E+03 0.91644345831920E+02 0.91167652736829E+02
 0.14564294036149E+03 0.36885590469041E+03 0.22248474962711E+03 0.12547578698032E+03 0.13615188671296E+03
 0.12547578698032E+03 0.29504362103774E+03 0.12541489946479E+03 0.13603496439087E+03 0.12541489946479E+03
 0.29493439805778E+03 0.12547578698032E+03 0.13615188671296E+03 0.12547578698032E+03 0.29504362103774E+03
 0.12541489946479E+03 0.13603496439087E+03 0.12541489946479E+03 0.29493439805778E+03 0.19867981040783E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35951171043522E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18079156987813E+00 0.00000000000000E+00 0.00000000000000E+00 0.18079156987813E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18971659396589E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18971659396589E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250626635293E+00 0.19111549860480E+00 0.33614009123388E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    460.01911005
 0.10171634314037E+00 0.31629432386672E+03 0.44178360334873E+03 0.43752883314465E+03 0.43591964133184E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16201493335265E+00 0.00000000000000E+00 -.20939374200282E+02
 0.33672740286523E-02 0.97503898903540E+00 0.23758090170054E+04 0.89092838137701E+03 0.82048001053930E+01
 0.30768000395224E+01 0.33654835847555E+03 0.30315000210285E+03 0.33147520927547E+03 0.35259495936972E+03
 0.30315000013663E+03 0.30315000024551E+03 0.32836738035292E+03 0.35256633081193E+03 0.30315000010749E+03
 0.30315000024441E+03 0.33147520927547E+03 0.35259495936972E+03 0.30315000013663E+03 0.30315000024551E+03
 0.32836738035292E+03 0.35256633081193E+03 0.30315000010749E+03 0.30315000024441E+03 0.39299115405270E+03
 0.31748539361995E+03 0.18864367081293E+04 0.17917016638982E+04 0.64269430141428E+03 0.11150280114992E+04
 0.46912023857785E+03 0.11062238547532E+04 0.10791858456562E+04 0.10352906682649E+04 0.17342505680617E+04
 0.99201433962208E+03 0.10786816604891E+04 0.93890330781507E+03 0.17340068656182E+04 0.11062238547532E+04
 0.10791858456562E+04 0.10352906682649E+04 0.17342505680617E+04 0.99201433962209E+03 0.10786816604891E+04
 0.93890330781508E+03 0.17340068656182E+04 0.17231054948395E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48153450756229E+03 0.12948587696224E+01
 0.12948587696224E+01 0.16600917750612E+01 0.58469985019194E-02 0.31714534932713E+03 0.33619984605557E+03
 0.33612557164635E+03 0.33612112758081E+03 0.23000000000000E+00 0.00000000000000E+00 0.18632091954663E+00
 0.00000000000000E+00 -.13597051038850E+02 0.29792086088575E-01 0.81938953455118E+00 0.26852768806505E+03
 0.10069788302439E+03 0.97633660947134E+01 0.36612622855175E+01 0.31748352772808E+03 0.39300275406197E+03
 0.30567974305655E+03 0.31091357000838E+03 0.30315000000129E+03 0.30315000002779E+03 0.30567165039488E+03
 0.31091248619094E+03 0.30315000000127E+03 0.30315000002780E+03 0.30567974305655E+03 0.31091357000838E+03
 0.30315000000129E+03 0.30315000002779E+03 0.30567165039488E+03 0.31091248619094E+03 0.30315000000127E+03
 0.30315000002780E+03 0.30972728474589E+03 0.30315000029426E+03 0.90722047480927E+02 0.90313909873832E+02
 0.14677064301622E+03 0.36992187865709E+03 0.22241738242579E+03 0.12710509878968E+03 0.13717371381824E+03
 0.12710509878968E+03 0.29589664424425E+03 0.12705318065072E+03 0.13705439204393E+03 0.12705318065072E+03
 0.29578551633165E+03 0.12710509878968E+03 0.13717371381824E+03 0.12710509878968E+03 0.29589664424425E+03
 0.12705318065072E+03 0.13705439204393E+03 0.12705318065072E+03 0.29578551633165E+03 0.19901489008446E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35962426578095E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18066782213242E+00 0.00000000000000E+00 0.00000000000000E+00 0.18066782213242E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18970314063372E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18970314063372E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250664937541E+00 0.19108196185518E+00 0.33619984605557E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    470.23169530
 0.10169049905720E+00 0.31653775173873E+03 0.44217365518340E+03 0.43791499594314E+03 0.43630368015251E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16122718943620E+00 0.00000000000000E+00 -.20956662289350E+02
 0.33681294513397E-02 0.98547578059631E+00 0.23752056194924E+04 0.89070210730967E+03 0.81179062514953E+01
 0.30442148443107E+01 0.33706202655881E+03 0.30315000270335E+03 0.33191486025605E+03 0.35326083113964E+03
 0.30315000018117E+03 0.30315000032638E+03 0.32877967731703E+03 0.35323255012234E+03 0.30315000014278E+03
 0.30315000032495E+03 0.33191486025605E+03 0.35326083113964E+03 0.30315000018117E+03 0.30315000032638E+03
 0.32877967731703E+03 0.35323255012234E+03 0.30315000014278E+03 0.30315000032495E+03 0.39378803963827E+03
 0.31813015544955E+03 0.18913654494182E+04 0.17950070099150E+04 0.64151666859141E+03 0.11090105952972E+04
 0.46428634336287E+03 0.11096580767753E+04 0.10819196933072E+04 0.10375826417742E+04 0.17341950796299E+04
 0.99571739076861E+03 0.10814310889474E+04 0.94168006603990E+03 0.17339634416178E+04 0.11096580767753E+04
 0.10819196933072E+04 0.10375826417742E+04 0.17341950796299E+04 0.99571739076862E+03 0.10814310889474E+04
 0.94168006603991E+03 0.17339634416178E+04 0.17224796005828E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48191870727031E+03 0.12948588970036E+01
 0.12948588970036E+01 0.17009421160478E+01 0.46593670462771E-02 0.31784864343458E+03 0.33630683539309E+03
 0.33624949973219E+03 0.33624618106149E+03 0.23000000000000E+00 0.00000000000000E+00 0.18511543153916E+00
 0.00000000000000E+00 -.13593161078978E+02 0.37385824160377E-01 0.84204289685543E+00 0.21398485066644E+03
 0.80244318999915E+02 0.95007036219599E+01 0.35627638582350E+01 0.31812839855501E+03 0.39379873030701E+03
 0.30577195832588E+03 0.31105125968197E+03 0.30315000000173E+03 0.30315000003703E+03 0.30576414494958E+03
 0.31105006319892E+03 0.30315000000171E+03 0.30315000003704E+03 0.30577195832588E+03 0.31105125968197E+03
 0.30315000000173E+03 0.30315000003703E+03 0.30576414494958E+03 0.31105006319892E+03 0.30315000000171E+03
 0.30315000003704E+03 0.30984516189794E+03 0.30315000038163E+03 0.89015973123447E+02 0.88699509165454E+02
 0.14866183634795E+03 0.37173172285923E+03 0.22232657732954E+03 0.12988935811424E+03 0.13888715493168E+03
 0.12988935811424E+03 0.29734448843670E+03 0.12985115763379E+03 0.13876408526740E+03 0.12985115763379E+03
 0.29723045987131E+03 0.12988935811424E+03 0.13888715493168E+03 0.12988935811424E+03 0.29734448843670E+03
 0.12985115763379E+03 0.13876408526740E+03 0.12985115763379E+03 0.29723045987131E+03 0.19953714007077E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35981549164115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18064962583164E+00 0.00000000000000E+00 0.00000000000000E+00 0.18064962583164E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18965193869230E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18965193869230E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250656701118E+00 0.19102109405583E+00 0.33630683539309E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    480.27767173
 0.10166665268999E+00 0.31676657489524E+03 0.44255545685935E+03 0.43829261200105E+03 0.43667890742897E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16047708564064E+00 0.00000000000000E+00 -.20979543072615E+02
 0.33689191187538E-02 0.99537341191875E+00 0.23746488763908E+04 0.89049332864653E+03 0.80371847431394E+01
 0.30139442786773E+01 0.33756029675061E+03 0.30315000345371E+03 0.33234140429680E+03 0.35390824426527E+03
 0.30315000023882E+03 0.30315000043128E+03 0.32917954570346E+03 0.35388028616756E+03 0.30315000018854E+03
 0.30315000042941E+03 0.33234140429680E+03 0.35390824426527E+03 0.30315000023882E+03 0.30315000043128E+03
 0.32917954570345E+03 0.35388028616756E+03 0.30315000018854E+03 0.30315000042941E+03 0.39456914815310E+03
 0.31876992971319E+03 0.18961067907947E+04 0.17981215547211E+04 0.64020819313193E+03 0.11029639145059E+04
 0.45955468040832E+03 0.11129720000278E+04 0.10845210898770E+04 0.10397410121409E+04 0.17341104451307E+04
 0.99929500645563E+03 0.10840468921536E+04 0.94431547409698E+03 0.17338899406716E+04 0.11129720000278E+04
 0.10845210898770E+04 0.10397410121409E+04 0.17341104451307E+04 0.99929500645563E+03 0.10840468921536E+04
 0.94431547409699E+03 0.17338899406716E+04 0.17217806812595E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48229424561498E+03 0.12948590655927E+01
 0.12948590655927E+01 0.17411260217737E+01 0.37263039451263E-02 0.31854046580278E+03 0.33641577118484E+03
 0.33637136530420E+03 0.33636887994829E+03 0.23000000000000E+00 0.00000000000000E+00 0.18394495241766E+00
 0.00000000000000E+00 -.13596028559877E+02 0.46747198908792E-01 0.86400375931488E+00 0.17113324833877E+03
 0.64174968127038E+02 0.92592189718522E+01 0.34722071144446E+01 0.31876826617017E+03 0.39457908694907E+03
 0.30586145439736E+03 0.31118490776233E+03 0.30315000000234E+03 0.30315000004905E+03 0.30585389215919E+03
 0.31118360504489E+03 0.30315000000231E+03 0.30315000004907E+03 0.30586145439736E+03 0.31118490776233E+03
 0.30315000000234E+03 0.30315000004905E+03 0.30585389215919E+03 0.31118360504489E+03 0.30315000000231E+03
 0.30315000004907E+03 0.30995948471331E+03 0.30315000049185E+03 0.87196592005605E+02 0.86956315348181E+02
 0.15045335566100E+03 0.37349271149516E+03 0.22228708905585E+03 0.13260919798359E+03 0.14050830973534E+03
 0.13260919798359E+03 0.29875212947925E+03 0.13258244582962E+03 0.14038180489376E+03 0.13258244582962E+03
 0.29863546443954E+03 0.13260919798359E+03 0.14050830973534E+03 0.13260919798359E+03 0.29875212947925E+03
 0.13258244582963E+03 0.14038180489376E+03 0.13258244582963E+03 0.29863546443954E+03 0.19999561247261E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36000059622050E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18068028032661E+00 0.00000000000000E+00 0.00000000000000E+00 0.18068028032661E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18961637939537E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18961637939537E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250626790729E+00 0.19095892058369E+00 0.33641577118484E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    490.40562078
 0.10161938226686E+00 0.31700291076628E+03 0.44294171908736E+03 0.43867577778569E+03 0.43706015490358E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15974489787680E+00 0.00000000000000E+00 -.20982026901734E+02
 0.33704858945265E-02 0.10049876285355E+01 0.23735450170528E+04 0.89007938139479E+03 0.79602969955537E+01
 0.29851113733326E+01 0.33805947893567E+03 0.30315000436522E+03 0.33276922945202E+03 0.35455338348827E+03
 0.30315000031077E+03 0.30315000056243E+03 0.32958136948476E+03 0.35452574903548E+03 0.30315000024575E+03
 0.30315000056004E+03 0.33276922945202E+03 0.35455338348827E+03 0.30315000031077E+03 0.30315000056243E+03
 0.32958136948475E+03 0.35452574903548E+03 0.30315000024575E+03 0.30315000056004E+03 0.39533285956414E+03
 0.31941335529336E+03 0.19008169180908E+04 0.18012425429091E+04 0.63906267743258E+03 0.10973414606152E+04
 0.45508346979549E+03 0.11162688135540E+04 0.10871105147331E+04 0.10419133200003E+04 0.17340758467038E+04
 0.10028533686961E+04 0.10866499803867E+04 0.94695765349475E+03 0.17338657379372E+04 0.11162688135540E+04
 0.10871105147331E+04 0.10419133200003E+04 0.17340758467038E+04 0.10028533686961E+04 0.10866499803867E+04
 0.94695765349476E+03 0.17338657379372E+04 0.17212175782818E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48267559843281E+03 0.12948590838940E+01
 0.12948590838940E+01 0.17816378179688E+01 0.29743718272557E-02 0.31922500899827E+03 0.33652943410749E+03
 0.33649512091114E+03 0.33649326476255E+03 0.23000000000000E+00 0.00000000000000E+00 0.18278189864571E+00
 0.00000000000000E+00 -.13576775625397E+02 0.58565060630389E-01 0.88579017266376E+00 0.13660021715830E+03
 0.51225081434361E+02 0.90314842576570E+01 0.33868065966214E+01 0.31941179949956E+03 0.39534203268971E+03
 0.30595436481528E+03 0.31131993665454E+03 0.30315000000313E+03 0.30315000006414E+03 0.30594705124169E+03
 0.31131852728645E+03 0.30315000000309E+03 0.30315000006416E+03 0.30595436481528E+03 0.31131993665454E+03
 0.30315000000313E+03 0.30315000006414E+03 0.30594705124169E+03 0.31131852728645E+03 0.30315000000309E+03
 0.30315000006416E+03 0.31007504366881E+03 0.30315000062671E+03 0.85278196700742E+02 0.85094061645333E+02
 0.15218740944878E+03 0.37522275370648E+03 0.22227440721046E+03 0.13531583990503E+03 0.14207399512479E+03
 0.13531583990503E+03 0.30012898923320E+03 0.13529878484413E+03 0.14194431000352E+03 0.13529878484413E+03
 0.30000994537918E+03 0.13531583990503E+03 0.14207399512478E+03 0.13531583990503E+03 0.30012898923320E+03
 0.13529878484413E+03 0.14194431000352E+03 0.13529878484413E+03 0.30000994537918E+03 0.20040242834250E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36018237610930E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18054469610376E+00 0.00000000000000E+00 0.00000000000000E+00 0.18054469610376E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18951734038995E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18951734038995E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250663132396E+00 0.19089484055702E+00 0.33652943410749E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
    500.00000000
 0.10157804457811E+00 0.31722095129995E+03 0.44330632599875E+03 0.43903715739482E+03 0.43741952160972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15907264892391E+00 0.00000000000000E+00 -.20986912163902E+02
 0.33718572025285E-02 0.10137772988522E+01 0.23725797148233E+04 0.88971739305874E+03 0.78912794842196E+01
 0.29592298065823E+01 0.33852721349462E+03 0.30315000542234E+03 0.33317026321235E+03 0.35515807870254E+03
 0.30315000039670E+03 0.30315000071935E+03 0.32995811306545E+03 0.35513073857641E+03 0.30315000031422E+03
 0.30315000071635E+03 0.33317026321235E+03 0.35515807870254E+03 0.30315000039670E+03 0.30315000071935E+03
 0.32995811306545E+03 0.35513073857641E+03 0.30315000031422E+03 0.30315000071635E+03 0.39604984069254E+03
 0.32002555476407E+03 0.19051914099326E+04 0.18041015331018E+04 0.63791287427986E+03 0.10920201117719E+04
 0.45091767312064E+03 0.11193400345045E+04 0.10894988707524E+04 0.10439067533390E+04 0.17340301606295E+04
 0.10061707620434E+04 0.10890505360618E+04 0.94939358364547E+03 0.17338292789461E+04 0.11193400345045E+04
 0.10894988707524E+04 0.10439067533390E+04 0.17340301606295E+04 0.10061707620434E+04 0.10890505360618E+04
 0.94939358364548E+03 0.17338292789461E+04 0.17206523976737E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48303515985757E+03 0.12948591198893E+01
 0.12948591198893E+01 0.18200153348664E+01 0.24019121344154E-02 0.31987162905456E+03 0.33664095368258E+03
 0.33661410138638E+03 0.33661269602232E+03 0.23000000000000E+00 0.00000000000000E+00 0.18169675576070E+00
 0.00000000000000E+00 -.13561646808693E+02 0.72523161399838E-01 0.90608336596113E+00 0.11030958724888E+03
 0.41366095218330E+02 0.88292096517123E+01 0.33109536193921E+01 0.32002409737624E+03 0.39605835248816E+03
 0.30604218310230E+03 0.31144676943165E+03 0.30315000000412E+03 0.30315000008226E+03 0.30603508839665E+03
 0.31144526217019E+03 0.30315000000407E+03 0.30315000008229E+03 0.30604218310230E+03 0.31144676943165E+03
 0.30315000000412E+03 0.30315000008226E+03 0.30603508839665E+03 0.31144526217019E+03 0.30315000000407E+03
 0.30315000008229E+03 0.31018355573209E+03 0.30315000078434E+03 0.83362194478799E+02 0.83221925042845E+02
 0.15377728996119E+03 0.37685438851155E+03 0.22230821210055E+03 0.13786439360124E+03 0.14350783718926E+03
 0.13786439360124E+03 0.30142470575268E+03 0.13785509606990E+03 0.14337534597493E+03 0.13785509606990E+03
 0.30130359150423E+03 0.13786439360124E+03 0.14350783718926E+03 0.13786439360124E+03 0.30142470575268E+03
 0.13785509606990E+03 0.14337534597493E+03 0.13785509606990E+03 0.30130359150423E+03 0.20075264325527E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36035441884672E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18043753999517E+00 0.00000000000000E+00 0.00000000000000E+00 0.18043753999517E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18943554527028E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18943554527028E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18250686932928E+00 0.19083187989313E+00 0.33664095368258E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30315000000000E+03
