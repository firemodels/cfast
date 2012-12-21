#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-56 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL56 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.180000                                                                                   
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
#CONDINIT 1000.000000 10.000000 20.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-56           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-56           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-56           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-56           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-56           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-56           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-56           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-56           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-56           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-56           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-56           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-56           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-56           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-56           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-56           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-56           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-56           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-56           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL56     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL56     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL56     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL56     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL56     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL56     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL56     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL56     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL56     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL56     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL56     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL56     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL56     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL56     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL56     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL56     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL56     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL56     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.73013404273805E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.45761623340350E-03 0.45761623340350E-03 0.64816768459935E-03 0.65140852302234E-03
 0.00000000000000E+00 0.43364658456045E-03 0.51033019341656E-03 0.43364658456045E-03 0.51033019341656E-03
 0.43145144016006E-03 0.51032408715331E-03 0.43145144016006E-03 0.51032408715331E-03 0.43364658456045E-03
 0.51033019341656E-03 0.43364658456045E-03 0.51033019341656E-03 0.43145144021755E-03 0.51032408721081E-03
 0.43145144021755E-03 0.51032408721081E-03 0.52582705762928E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.63028958290084E-07 0.99964886840276E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.53290747688068E-03 0.53290747688068E-03
 0.65453242277559E-03 0.65780508488947E-03 0.00000000000000E+00 0.46581430984023E-03 0.51384943997599E-03
 0.46581430984023E-03 0.51384943997599E-03 0.46176000224395E-03 0.51384238284147E-03 0.46176000224395E-03
 0.51384238284147E-03 0.46581430978273E-03 0.51384944003349E-03 0.46581430978273E-03 0.51384944003349E-03
 0.46176000230144E-03 0.51384238284147E-03 0.46176000230144E-03 0.51384238284147E-03 0.42502879086363E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.48843329357824E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.48843329357824E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17999971832238E+00 0.21615919158933E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     12.46901258
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15465069129076E+02
 0.99984737163455E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000163458E+03 0.29315000000000E+03 0.29315000225499E+03 0.29315000225499E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000224339E+03 0.29315000224339E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000225499E+03 0.29315000225499E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000224339E+03 0.29315000224339E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000795698E+03
 0.29315000656919E+03 0.47045891438885E-03 0.47045891438885E-03 0.61911889614018E-03 0.62221449062088E-03
 .00000000000000E+00 0.44069520528054E-03 0.52359383146119E-03 0.44069520528054E-03 0.52359383146119E-03
 0.43841696877162E-03 0.52364704828451E-03 0.43841696877162E-03 0.52364704828451E-03 0.44069520533804E-03
 0.52359383146119E-03 0.44069520533804E-03 0.52359383146119E-03 0.43841696877162E-03 0.52364704828451E-03
 0.43841696877162E-03 0.52364704828451E-03 0.52611196361887E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.22999999931733E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17092851325193E+02 0.99950226344203E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000654342E+03 0.29315000798834E+03
 0.29315000242159E+03 0.29315000242159E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000240039E+03
 0.29315000240039E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000242159E+03 0.29315000242159E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000240039E+03 0.29315000240039E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000233244E+03 0.29315000000000E+03 0.51258944734273E-03 0.51258944734273E-03
 0.66643710540709E-03 0.66976929093413E-03 .00000000000000E+00 0.47322866498571E-03 0.51997113306277E-03
 0.47322866498571E-03 0.51997113306277E-03 0.46907626229392E-03 0.52007296466035E-03 0.46907626229392E-03
 0.52007296466035E-03 0.47322866510070E-03 0.51997113306277E-03 0.47322866510070E-03 0.51997113306277E-03
 0.46907626286888E-03 0.52007296517782E-03 0.46907626286888E-03 0.52007296517782E-03 0.42523640840399E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21550618977269E+00 0.00000000000000E+00 0.00000000000000E+00 0.21550618977269E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606658665E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606658665E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17948606376432E+00 0.21550597755156E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     30.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.22999999999998E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15465069130373E+02
 0.99984737163454E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000253888E+03 0.29315000000000E+03 0.29315000348304E+03 0.29315000348304E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000346497E+03 0.29315000346497E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000348304E+03 0.29315000348304E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000346497E+03 0.29315000346497E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001193791E+03
 0.29315000987041E+03 0.47664017181603E-03 0.47664017181603E-03 0.60485466187600E-03 0.60787893518538E-03
 .00000000000000E+00 0.44391258505407E-03 0.52986490862101E-03 0.44391258505407E-03 0.52986490862101E-03
 0.44159894046326E-03 0.52995125248160E-03 0.44159894046326E-03 0.52995125248160E-03 0.44391258505407E-03
 0.52986490862101E-03 0.44391258505407E-03 0.52986490862101E-03 0.44159894046326E-03 0.52995125248160E-03
 0.44159894046326E-03 0.52995125248160E-03 0.52607686201099E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.22999999937256E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17092851326587E+02 0.99953031338600E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000985199E+03 0.29315001196019E+03
 0.29315000374008E+03 0.29315000374008E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000370722E+03
 0.29315000370722E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000374008E+03 0.29315000374008E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000370722E+03 0.29315000370722E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000360615E+03 0.29315000000000E+03 0.50266214923122E-03 0.50266214923122E-03
 0.67196931162475E-03 0.67532915818287E-03 .00000000000000E+00 0.47665045540431E-03 0.52279876227051E-03
 0.47665045540431E-03 0.52279876227051E-03 0.47245639134973E-03 0.52296043223513E-03 0.47245639134973E-03
 0.52296043223513E-03 0.47665045540431E-03 0.52279876227051E-03 0.47665045540431E-03 0.52279876227051E-03
 0.47245639129224E-03 0.52296043217763E-03 0.47245639129224E-03 0.52296043217763E-03 0.42520976271068E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21550618978171E+00 0.00000000000000E+00 0.00000000000000E+00 0.21550618978171E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606659244E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606659244E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17948606376428E+00 0.21550597755151E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.22999999999998E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15465069129447E+02
 0.99984737163455E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296607E+03 0.29315000000000E+03 0.29315000406004E+03 0.29315000406004E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000403892E+03 0.29315000403892E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000406004E+03 0.29315000406004E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000403892E+03 0.29315000403892E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001374982E+03
 0.29315001137705E+03 0.47944240365344E-03 0.47944240365344E-03 0.59845777700353E-03 0.60145006588855E-03
 .00000000000000E+00 0.44535685795408E-03 0.53267908482289E-03 0.44535685795408E-03 0.53267908482289E-03
 0.44302842529688E-03 0.53278106430056E-03 0.44302842529688E-03 0.53278106430056E-03 0.44535685795408E-03
 0.53267908488038E-03 0.44535685795408E-03 0.53267908488038E-03 0.44302842518189E-03 0.53278106424306E-03
 0.44302842518189E-03 0.53278106424306E-03 0.52606948361100E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.22999999940229E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17092851325621E+02 0.99954537090027E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001136325E+03 0.29315001376645E+03
 0.29315000435958E+03 0.29315000435958E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000432125E+03
 0.29315000432125E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000435958E+03 0.29315000435958E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000432125E+03 0.29315000432125E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000420554E+03 0.29315000000000E+03 0.49822253741865E-03 0.49822253741865E-03
 0.67446706889019E-03 0.67783940423464E-03 .00000000000000E+00 0.47819622464187E-03 0.52407593475235E-03
 0.47819622464187E-03 0.52407593475235E-03 0.47398518944972E-03 0.52426573384421E-03 0.47398518944972E-03
 0.52426573384421E-03 0.47819622469936E-03 0.52407593475235E-03 0.47819622469936E-03 0.52407593475235E-03
 0.47398518939223E-03 0.52426573384421E-03 0.47398518939223E-03 0.52426573384421E-03 0.42520840916802E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21550618977533E+00 0.00000000000000E+00 0.00000000000000E+00 0.21550618977533E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606658953E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606658953E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17948606376431E+00 0.21550597755155E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00025000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.22999999999998E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.15465069129442E+02
 0.99984737163455E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296608E+03 0.29315000000000E+03 0.29315000406006E+03 0.29315000406006E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000403894E+03 0.29315000403894E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000406006E+03 0.29315000406006E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000403894E+03 0.29315000403894E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001374987E+03
 0.29315001137709E+03 0.47944783525691E-03 0.47944783525691E-03 0.59846530499402E-03 0.60145763151899E-03
 .00000000000000E+00 0.44536276999534E-03 0.53268340479586E-03 0.44536276999534E-03 0.53268340479586E-03
 0.44303438379503E-03 0.53278538450351E-03 0.44303438379503E-03 0.53278538450351E-03 0.44536276988035E-03
 0.53268340485335E-03 0.44536276988035E-03 0.53268340485335E-03 0.44303438373754E-03 0.53278538456101E-03
 0.44303438373754E-03 0.53278538456101E-03 0.52607616130775E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.22999999940229E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.17092851325436E+02 0.99954537127671E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001136329E+03 0.29315001376649E+03
 0.29315000435959E+03 0.29315000435959E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000432127E+03
 0.29315000432127E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000435959E+03 0.29315000435959E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000432127E+03 0.29315000432127E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000420556E+03 0.29315000000000E+03 0.49822955907805E-03 0.49822955907805E-03
 0.67448147936965E-03 0.67785388676650E-03 .00000000000000E+00 0.47820612450086E-03 0.52408389756617E-03
 0.47820612450086E-03 0.52408389756617E-03 0.47399525363271E-03 0.52427369711800E-03 0.47399525363271E-03
 0.52427369711800E-03 0.47820612455836E-03 0.52408389756617E-03 0.47820612455836E-03 0.52408389756617E-03
 0.47399525369020E-03 0.52427369711800E-03 0.47399525369020E-03 0.52427369711800E-03 0.42521655985740E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21550618977522E+00 0.00000000000000E+00 0.00000000000000E+00 0.21550618977522E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606657801E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21550606657801E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17948606376432E+00 0.21550597755155E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     50.00237438
 0.22455109766474E+01 0.29318168552888E+03 0.32871326120950E+03 0.30211774680670E+03 0.30137454451939E+03
 0.22999999999319E+00 0.00000000000000E+00 0.22597086319362E+00 0.00000000000000E+00 0.22588839412975E+02
 0.10001148356972E-02 0.73129366167069E-01 0.79990814199082E+04 0.29996555324656E+04 0.10939517760517E+03
 0.41023191601938E+02 0.29447803860859E+03 0.29315000000004E+03 0.29429779690582E+03 0.29472797272710E+03
 0.29315000000007E+03 0.29315000000007E+03 0.29402427351007E+03 0.29471605427409E+03 0.29315000000007E+03
 0.29315000000007E+03 0.29429779690582E+03 0.29472797272710E+03 0.29315000000007E+03 0.29315000000007E+03
 0.29402427351007E+03 0.29471605427409E+03 0.29315000000007E+03 0.29315000000007E+03 0.29745288037821E+03
 0.29315448263414E+03 0.61040270178908E+03 0.60781729944404E+03 0.28508015168346E+03 0.58813797024293E+03
 0.30163241780105E+03 0.38928944348380E+03 0.18989713434526E+03 0.38756437680978E+03 0.50415345907042E+03
 0.29288364097427E+03 0.18526531275554E+03 0.29168959115077E+03 0.49964972791753E+03 0.38928944348380E+03
 0.18989713434526E+03 0.38756437680978E+03 0.50415345907043E+03 0.29288364097427E+03 0.18526531275554E+03
 0.29168959115077E+03 0.49964972791753E+03 0.51966605542842E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.35380590330263E+03 0.12945381436473E+01
 0.12945381436473E+01 0.20024819833876E-01 0.13190037831090E+01 0.29316715519580E+03 0.29934973189088E+03
 0.29391317052410E+03 0.29389958811407E+03 0.22999999964955E+00 0.00000000000000E+00 0.22919877070229E+00
 0.00000000000000E+00 0.21104003122052E+02 0.99986365905726E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315447228552E+03 0.29746357119777E+03
 0.29315219794369E+03 0.29333488498540E+03 0.29315000000007E+03 0.29315000000007E+03 0.29315221583675E+03
 0.29333489332305E+03 0.29315000000007E+03 0.29315000000007E+03 0.29315219794369E+03 0.29333488498540E+03
 0.29315000000007E+03 0.29315000000007E+03 0.29315221583675E+03 0.29333489332305E+03 0.29315000000007E+03
 0.29315000000007E+03 0.29322556878823E+03 0.29315000000004E+03 0.62969745681276E+00 0.63256518149455E+00
 0.53133021653118E+00 0.36483345567746E+02 0.35949358700133E+02 0.74831428331557E+00 -.25538263784388E+00
 0.75519165593105E+00 0.51751698634746E+02 0.75385997493746E+00 -.25234520632547E+00 0.76072684445799E+00
 0.51754656140877E+02 0.74831428331557E+00 -.25538263784388E+00 0.75519165593105E+00 0.51751698634746E+02
 0.75385997493734E+00 -.25234520632564E+00 0.76072684445788E+00 0.51754656140877E+02 0.10981037942724E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30559268298327E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24982575182126E+00 0.00000000000000E+00 0.00000000000000E+00 0.24982575182126E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19868816567671E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19868816567671E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18063184958117E+00 0.21695080902313E+00 0.29316715519580E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     60.02167035
 0.16949034257759E+01 0.29322746127371E+03 0.35350612687732E+03 0.31945062126640E+03 0.31672196422455E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22281408718350E+00 0.00000000000000E+00 0.12447987738793E+02
 0.99985865176419E-03 0.12684194537691E+00 0.80000000000000E+04 0.30000000000000E+04 0.63070618920484E+02
 0.23651482095181E+02 0.29560201457154E+03 0.29315000000005E+03 0.29549040153888E+03 0.29678765023467E+03
 0.29315000000008E+03 0.29315000000008E+03 0.29491185091701E+03 0.29676055707265E+03 0.29315000000008E+03
 0.29315000000008E+03 0.29549040153888E+03 0.29678765023467E+03 0.29315000000008E+03 0.29315000000008E+03
 0.29491185091701E+03 0.29676055707265E+03 0.29315000000008E+03 0.29315000000008E+03 0.30319907243981E+03
 0.29316258963573E+03 0.64947439881363E+03 0.64368455246268E+03 0.30146546236756E+03 0.82179443014531E+03
 0.51882164046592E+03 0.44250425892415E+03 0.25193763521774E+03 0.43806575253110E+03 0.73523787212869E+03
 0.33175687357013E+03 0.24649735320516E+03 0.32874002915870E+03 0.73007537875154E+03 0.44250425892415E+03
 0.25193763521775E+03 0.43806575253110E+03 0.73523787212870E+03 0.33175687357013E+03 0.24649735320516E+03
 0.32874002915870E+03 0.73007537875154E+03 0.67675934275370E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.37959691714137E+03 0.12946128232701E+01
 0.12946128232701E+01 0.60102003713734E-01 0.11351251420443E+01 0.29316013343631E+03 0.30389295992687E+03
 0.29577089246378E+03 0.29570051073898E+03 0.22999999966324E+00 0.00000000000000E+00 0.22852095972375E+00
 0.00000000000000E+00 0.12590681800325E+02 0.99980358524218E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29316254530291E+03 0.30323681436781E+03
 0.29315554911002E+03 0.29353809318482E+03 0.29315000000008E+03 0.29315000000008E+03 0.29315556123338E+03
 0.29353814126024E+03 0.29315000000008E+03 0.29315000000008E+03 0.29315554911002E+03 0.29353809318482E+03
 0.29315000000008E+03 0.29315000000008E+03 0.29315556123338E+03 0.29353814126024E+03 0.29315000000008E+03
 0.29315000000008E+03 0.29336534710726E+03 0.29315000000005E+03 0.11600206978573E+01 0.11593004400970E+01
 0.20915563656480E+00 0.71089909405815E+02 0.70879707991067E+02 0.11990076194706E+01 -.74772342617452E+00
 0.12005835234544E+01 0.76424599798584E+02 0.11980923881190E+01 -.73398213637053E+00 0.11996630308462E+01
 0.76437931898846E+02 0.11990076194706E+01 -.74772342617447E+00 0.12005835234544E+01 0.76424599798584E+02
 0.11980923881189E+01 -.73398213637060E+00 0.11996630308461E+01 0.76437931898846E+02 0.23189761444615E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31399690663425E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.19119844356655E+00 0.00000000000000E+00 0.00000000000000E+00 0.19119844356655E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18583120757349E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18583120757349E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18037711569174E+00 0.21663184030102E+00 0.29316013343631E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     70.00704438
 0.13168421905622E+01 0.29331411879565E+03 0.36797108061188E+03 0.33520060156562E+03 0.33099114145919E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22058207223582E+00 0.00000000000000E+00 0.51576920213185E+01
 0.99949134143504E-03 0.16090734372272E+00 0.80000000000000E+04 0.30000000000000E+04 0.49718053973880E+02
 0.18644270240205E+02 0.29646233938420E+03 0.29315000000006E+03 0.29641165263894E+03 0.29872315828414E+03
 0.29315000000008E+03 0.29315000000008E+03 0.29561298889398E+03 0.29868696120109E+03 0.29315000000008E+03
 0.29315000000008E+03 0.29641165263894E+03 0.29872315828414E+03 0.29315000000008E+03 0.29315000000008E+03
 0.29561298889398E+03 0.29868696120109E+03 0.29315000000008E+03 0.29315000000008E+03 0.30821753398229E+03
 0.29317375785580E+03 0.70093853080844E+03 0.69251140752206E+03 0.33074937460231E+03 0.95986413668891E+03
 0.62746101521359E+03 0.48675989275943E+03 0.31716044725487E+03 0.47994280875644E+03 0.89270358584422E+03
 0.36861127866979E+03 0.31173255849807E+03 0.36399254775478E+03 0.88764268939545E+03 0.48675989275943E+03
 0.31716044725487E+03 0.47994280875644E+03 0.89270358584423E+03 0.36861127866979E+03 0.31173255849807E+03
 0.36399254775478E+03 0.88764268939545E+03 0.81058760229555E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39173048749082E+03 0.12946665171914E+01
 0.12946665171914E+01 0.10004349981291E+00 0.97424901348190E+00 0.29315537305594E+03 0.30733535508415E+03
 0.29812546608270E+03 0.29797407962781E+03 0.22999999972677E+00 0.00000000000000E+00 0.22790927439510E+00
 0.00000000000000E+00 0.66646605770764E+01 0.99976133246010E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29317369066784E+03 0.30825662293983E+03
 0.29315979029826E+03 0.29374127053925E+03 0.29315000000008E+03 0.29315000000008E+03 0.29315977374401E+03
 0.29374138299636E+03 0.29315000000008E+03 0.29315000000008E+03 0.29315979029826E+03 0.29374127053925E+03
 0.29315000000008E+03 0.29315000000008E+03 0.29315977374401E+03 0.29374138299636E+03 0.29315000000008E+03
 0.29315000000008E+03 0.29352835888266E+03 0.29315000000006E+03 0.18375976542468E+01 0.18285166513002E+01
 -.62074564965065E-01 0.99382472044599E+02 0.99444856982389E+02 0.17809550744298E+01 -.11020309950993E+01
 0.17794001982728E+01 0.94219146226175E+02 0.17731855308278E+01 -.10774856230286E+01 0.17716379826767E+01
 0.94242775781410E+02 0.17809550744298E+01 -.11020309950993E+01 0.17794001982728E+01 0.94219146226175E+02
 0.17731855308279E+01 -.10774856230287E+01 0.17716379826768E+01 0.94242775781410E+02 0.34544903754515E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32016918027384E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.14445116776297E+00 0.00000000000000E+00 0.00000000000000E+00 0.14445116776297E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18145766543378E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18145766543378E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18019958555636E+00 0.21450227597106E+00 0.29576190750742E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     80.00054696
 0.10166207129321E+01 0.29345022766517E+03 0.37819537332757E+03 0.34947748319396E+03 0.34448324422408E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21868118679028E+00 0.00000000000000E+00 -.13755229718597E-02
 0.99897689074166E-03 0.18868989015928E+00 0.80000000000000E+04 0.30000000000000E+04 0.42397608018356E+02
 0.15899103006884E+02 0.29735404347390E+03 0.29315000000007E+03 0.29726595482692E+03 0.30057022894500E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29628420998537E+03 0.30052867472542E+03 0.29315000000009E+03
 0.29315000000009E+03 0.29726595482692E+03 0.30057022894500E+03 0.29315000000009E+03 0.29315000000009E+03
 0.29628420998537E+03 0.30052867472542E+03 0.29315000000009E+03 0.29315000000009E+03 0.31273382122256E+03
 0.29318787967979E+03 0.79662426407585E+03 0.78540665068020E+03 0.35105314976350E+03 0.10487069110315E+04
 0.69589849551919E+03 0.52883275442583E+03 0.37181297811595E+03 0.51969658299823E+03 0.10086357954672E+04
 0.40677299070862E+03 0.36690539716193E+03 0.40057212670354E+03 0.10041472783953E+04 0.52883275442583E+03
 0.37181297811595E+03 0.51969658299823E+03 0.10086357954672E+04 0.40677299070862E+03 0.36690539716193E+03
 0.40057212670354E+03 0.10041472783953E+04 0.93555504022091E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40226459362987E+03 0.18254539210036E+01
 0.18254539210036E+01 0.14001751014160E+00 0.86090373857416E+00 0.29315239759187E+03 0.31021924864201E+03
 0.30042397139217E+03 0.30018885791439E+03 0.22999999968304E+00 0.00000000000000E+00 0.22730766889898E+00
 0.00000000000000E+00 0.26959074784835E+01 0.99973230915131E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29318779217588E+03 0.31276672896593E+03
 0.29316496736270E+03 0.29394851007445E+03 0.29315000000009E+03 0.29315000000009E+03 0.29316490648221E+03
 0.29394870149099E+03 0.29315000000009E+03 0.29315000000009E+03 0.29316496736270E+03 0.29394851007445E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29316490648221E+03 0.29394870149099E+03 0.29315000000009E+03
 0.29315000000009E+03 0.29370390368879E+03 0.29315000000007E+03 0.25655371504648E+01 0.25448491151222E+01
 -.36425640488994E+00 0.12383263920657E+03 0.12419871689348E+03 0.23894379447826E+01 -.14395356950753E+01
 0.23834650223671E+01 0.11035609028123E+03 0.23741445978798E+01 -.14045094301079E+01 0.23682082334987E+01
 0.11038957397307E+03 0.23894379447826E+01 -.14395356950752E+01 0.23834650223671E+01 0.11035609028123E+03
 0.23741445978798E+01 -.14045094301078E+01 0.23682082334987E+01 0.11038957397307E+03 0.44847278105826E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32501915037112E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10501103828765E+00 0.00000000000000E+00 0.00000000000000E+00 0.10501103828765E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17915477559807E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17915477559807E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18008059229171E+00 0.21027606458547E+00 0.30149520500428E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     90.00308089
 0.77984785373281E+00 0.29365273493600E+03 0.38652580734744E+03 0.36238351861756E+03 0.35716217057651E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21684652486740E+00 0.00000000000000E+00 -.36376352757248E+01
 0.99825215584253E-03 0.21484313855850E+00 0.80000000000000E+04 0.30000000000000E+04 0.37236469610695E+02
 0.13963676104011E+02 0.29830602086179E+03 0.29315000000008E+03 0.29810191950507E+03 0.30237962465841E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29696075748080E+03 0.30233509803845E+03 0.29315000000009E+03
 0.29315000000009E+03 0.29810191950507E+03 0.30237962465841E+03 0.29315000000009E+03 0.29315000000009E+03
 0.29696075748080E+03 0.30233509803845E+03 0.29315000000009E+03 0.29315000000009E+03 0.31697158030571E+03
 0.29320485347896E+03 0.89488885643501E+03 0.88072493110330E+03 0.37756108736851E+03 0.11257680773776E+04
 0.74631918457227E+03 0.57169119175208E+03 0.42778000994111E+03 0.56027037589288E+03 0.11122110514300E+04
 0.44709036943797E+03 0.42338977157446E+03 0.43932071693381E+03 0.11082683770661E+04 0.57169119175208E+03
 0.42778000994110E+03 0.56027037589288E+03 0.11122110514300E+04 0.44709036943797E+03 0.42338977157446E+03
 0.43932071693381E+03 0.11082683770661E+04 0.10545529491403E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41214300670327E+03 0.16403443137235E+01
 0.16403443137235E+01 0.18002764586885E+00 0.76754679322092E+00 0.29315078165552E+03 0.31278790358400E+03
 0.30273963027447E+03 0.30242184771523E+03 0.22999999968461E+00 0.00000000000000E+00 0.22668609764996E+00
 0.00000000000000E+00 0.94830299175980E-01 0.99971214774329E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29320474840950E+03 0.31699929339426E+03
 0.29317108273289E+03 0.29416115418233E+03 0.29315000000009E+03 0.29315000000009E+03 0.29317096585195E+03
 0.29416143517768E+03 0.29315000000009E+03 0.29315000000009E+03 0.29317108273289E+03 0.29416115418233E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29317096585195E+03 0.29416143517768E+03 0.29315000000009E+03
 0.29315000000009E+03 0.29388926423985E+03 0.29315000000008E+03 0.33543934438944E+01 0.33170597319156E+01
 -.64662857305919E+00 0.14614243733775E+03 0.14679229905368E+03 0.30532412449547E+01 -.17487702384211E+01
 0.30420268033781E+01 0.12506227331150E+03 0.30306610407849E+01 -.17038528941073E+01 0.30195282593714E+01
 0.12510493114171E+03 0.30532412449547E+01 -.17487702384212E+01 0.30420268033782E+01 0.12506227331150E+03
 0.30306610407847E+01 -.17038528941073E+01 0.30195282593713E+01 0.12510493114171E+03 0.54658829601442E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32925305327903E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.69614961221518E-01 0.00000000000000E+00 0.00000000000000E+00 0.69614961221518E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17774584956021E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17774584956021E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18000256182779E+00 0.20915691341972E+00 0.30296931778465E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    100.36612403
 0.58638995525752E+00 0.29395773279865E+03 0.39388858253315E+03 0.37435576703159E+03 0.36934636570235E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21493247319218E+00 0.00000000000000E+00 -.60915430675980E+01
 0.99719226082818E-03 0.24169386465235E+00 0.80000000000000E+04 0.30000000000000E+04 0.33099723120846E+02
 0.12412396170317E+02 0.29933296111163E+03 0.29315000000009E+03 0.29896554706998E+03 0.30422343130469E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29767867682305E+03 0.30417720217694E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29896554706998E+03 0.30422343130469E+03 0.29315000000010E+03 0.29315000000010E+03
 0.29767867682305E+03 0.30417720217694E+03 0.29315000000010E+03 0.29315000000010E+03 0.32118317674117E+03
 0.29323888515267E+03 0.99633580107119E+03 0.97918866136637E+03 0.40979138846891E+03 0.11968058213622E+04
 0.78496547595093E+03 0.61700342516504E+03 0.48730125379956E+03 0.60331321345825E+03 0.12088558860656E+04
 0.49085645602677E+03 0.48338602229120E+03 0.48154009312911E+03 0.12054026029140E+04 0.61700342516504E+03
 0.48730125379956E+03 0.60331321345825E+03 0.12088558860656E+04 0.49085645602677E+03 0.48338602229120E+03
 0.48154009312911E+03 0.12054026029140E+04 0.11701560662762E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42246659239229E+03 0.14856094775615E+01
 0.14856094775615E+01 0.22147981843213E+00 0.68192162222165E+00 0.29315049764397E+03 0.31519248585473E+03
 0.30517188028295E+03 0.30477454184081E+03 0.22999999970251E+00 0.00000000000000E+00 0.22600729655407E+00
 0.00000000000000E+00 -.14421832745727E+01 0.99969794618419E-03 0.18720301724827E-01 0.80000000000000E+04
 0.30000000000000E+04 0.42734353952162E+03 0.16025382732061E+03 0.29323873480844E+03 0.32120898025057E+03
 0.29318177839507E+03 0.29439125786600E+03 0.29315000000010E+03 0.29315000000010E+03 0.29318150778936E+03
 0.29439163824142E+03 0.29315000000010E+03 0.29315000000010E+03 0.29318177839507E+03 0.29439125786600E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29318150778936E+03 0.29439163824142E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29409280383121E+03 0.29315000000009E+03 0.62981121375926E+01 0.62262016072656E+01
 0.17953347529850E+01 0.17012007400366E+03 0.16831576257691E+03 0.50564054635064E+01 0.68324445215007E+00
 0.50365509666562E+01 0.14159641042347E+03 0.49941446976210E+01 0.73762687501845E+00 0.49745074658524E+01
 0.14164773230768E+03 0.50564054635065E+01 0.68324445215002E+00 0.50365509666562E+01 0.14159641042347E+03
 0.49941446976210E+01 0.73762687501845E+00 0.49745074658525E+01 0.14164773230768E+03 0.66177379216957E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33320793972193E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36247435465957E-01 0.00000000000000E+00 0.00000000000000E+00 0.36247435465957E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17720701296532E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17720701296532E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17988347037709E+00 0.20816915795642E+00 0.30420088200555E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    110.23218069
 0.43280665447877E+00 0.29437400036417E+03 0.39957058096803E+03 0.38439398759678E+03 0.37998050311584E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21313527612933E+00 0.00000000000000E+00 -.73655168729944E+01
 0.99576963311178E-03 0.26679633116588E+00 0.80000000000000E+04 0.30000000000000E+04 0.29985419833326E+02
 0.11244532437497E+02 0.30035284904489E+03 0.29315000000009E+03 0.29979705767387E+03 0.30595860980396E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29838657232452E+03 0.30591164438441E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29979705767387E+03 0.30595860980396E+03 0.29315000000010E+03 0.29315000000010E+03
 0.29838657232452E+03 0.30591164438441E+03 0.29315000000010E+03 0.29315000000010E+03 0.32501071997529E+03
 0.29329768914456E+03 0.10913405502473E+04 0.10716059034412E+04 0.44059207114240E+03 0.12497510811224E+04
 0.80695604962424E+03 0.65983044867662E+03 0.54174856214379E+03 0.64414254904516E+03 0.12898232367398E+04
 0.53299744028944E+03 0.53823024427151E+03 0.52236450165695E+03 0.12867723841425E+04 0.65983044867662E+03
 0.54174856214379E+03 0.64414254904516E+03 0.12898232367397E+04 0.53299744028944E+03 0.53823024427151E+03
 0.52236450165694E+03 0.12867723841425E+04 0.12702855604574E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42983092786206E+03 0.13613131608905E+01
 0.13613131608905E+01 0.26094404506793E+00 0.60926016448020E+00 0.29315189511064E+03 0.31720783335395E+03
 0.30743695009339E+03 0.30697618050268E+03 0.23000000000000E+00 0.00000000000000E+00 0.22532585342789E+00
 0.00000000000000E+00 -.19735949829170E+01 0.99968793565163E-03 0.39380398249356E-01 0.80000000000000E+04
 0.30000000000000E+04 0.20314675208067E+03 0.76180032030251E+02 0.29329738078645E+03 0.32503683149607E+03
 0.29319887720159E+03 0.29462358006143E+03 0.29315000000010E+03 0.29315000000010E+03 0.29319831705583E+03
 0.29462405725335E+03 0.29315000000010E+03 0.29315000000010E+03 0.29319887720159E+03 0.29462358006143E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29319831705583E+03 0.29462405725335E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29429932766615E+03 0.29315000000009E+03 0.10203953176491E+02 0.10063886844592E+02
 0.54909589718995E+01 0.19162986113115E+03 0.18611144736439E+03 0.76993635012241E+01 0.43355028855479E+01
 0.76653689936982E+01 0.15702375732556E+03 0.75873505434965E+01 0.43971927815608E+01 0.75538687314577E+01
 0.15708159995003E+03 0.76993635012241E+01 0.43355028855479E+01 0.76653689936982E+01 0.15702375732556E+03
 0.75873505434965E+01 0.43971927815608E+01 0.75538687314577E+01 0.15708159995003E+03 0.77312690668469E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33579411045599E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12680612499867E-01 0.62683145035113E-02 0.00000000000000E+00 0.12680612499867E-01 0.62683145035113E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17740656334367E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17740656334367E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17986329603169E+00 0.20658202581905E+00 0.30650201503588E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    120.03522922
 0.30994152497991E+00 0.29494722408190E+03 0.40434371849087E+03 0.39304154638935E+03 0.38942136320950E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21133155892274E+00 0.00000000000000E+00 -.83433931227854E+01
 0.11469725832883E-02 0.29193079465220E+00 0.69748833725954E+04 0.26155812647233E+04 0.27403755090418E+02
 0.10276408158907E+02 0.30139444082745E+03 0.29315000000009E+03 0.30063046573639E+03 0.30765011998848E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29911004717386E+03 0.30760295599328E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30063046573639E+03 0.30765011998848E+03 0.29315000000010E+03 0.29315000000010E+03
 0.29911004717386E+03 0.30760295599328E+03 0.29315000000010E+03 0.29315000000010E+03 0.32863739964667E+03
 0.29337608483478E+03 0.11819204136789E+04 0.11601338421259E+04 0.47093445730725E+03 0.12939354402249E+04
 0.82064631063111E+03 0.70150520241871E+03 0.59372519346345E+03 0.68413377125351E+03 0.13622329457836E+04
 0.57464214600844E+03 0.59054733662553E+03 0.56299899289062E+03 0.13595230088926E+04 0.70150520241871E+03
 0.59372519346345E+03 0.68413377125351E+03 0.13622329457836E+04 0.57464214600844E+03 0.59054733662553E+03
 0.56299899289062E+03 0.13595230088926E+04 0.13606954098553E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43652486758578E+03 0.12947659686213E+01
 0.12947659686213E+01 0.30015623919634E+00 0.55087028692196E+00 0.29315500860634E+03 0.31889476664058E+03
 0.30944192137816E+03 0.30893315181298E+03 0.23000000000000E+00 0.00000000000000E+00 0.22462912946671E+00
 0.00000000000000E+00 -.23305836177955E+01 0.99967379494668E-03 0.58726103864539E-01 0.80000000000000E+04
 0.30000000000000E+04 0.13622562154733E+03 0.51084608080249E+02 0.29337561513567E+03 0.32866475994302E+03
 0.29322112095857E+03 0.29485904137294E+03 0.29315000000010E+03 0.29315000000010E+03 0.29322018447893E+03
 0.29485961332249E+03 0.29315000000010E+03 0.29315000000010E+03 0.29322112095857E+03 0.29485904137294E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29322018447893E+03 0.29485961332249E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29450802898449E+03 0.29315000000009E+03 0.14510804613168E+02 0.14266813335648E+02
 0.96730534406279E+01 0.21052228072129E+03 0.20080086201346E+03 0.10614148934304E+02 0.84482685935033E+01
 0.10560683177951E+02 0.17092914046614E+03 0.10450914564481E+02 0.85160249132293E+01 0.10398406785180E+02
 0.17099227289750E+03 0.10614148934304E+02 0.84482685935033E+01 0.10560683177951E+02 0.17092914046614E+03
 0.10450914564482E+02 0.85160249132293E+01 0.10398406785180E+02 0.17099227289750E+03 0.87557323307465E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33772602197253E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.98292140948841E-03 0.24474810109383E-01 0.00000000000000E+00 0.98292140948841E-03 0.24474810109383E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17781948052322E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17781948052322E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17984926761064E+00 0.20124221798960E+00 0.31460916204809E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    130.23796588
 0.21480170219551E+00 0.29572929237532E+03 0.40869756731549E+03 0.40060897473174E+03 0.39781674602207E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20939645785474E+00 0.00000000000000E+00 -.96468248021379E+01
 0.16549823725792E-02 0.31883556763936E+00 0.48338883437970E+04 0.18127081289239E+04 0.25091303518085E+02
 0.94092388192819E+01 0.30249656795901E+03 0.29315000000009E+03 0.30150437536882E+03 0.30937765795584E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29988170402314E+03 0.30933064176717E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30150437536882E+03 0.30937765795584E+03 0.29315000000010E+03 0.29315000000010E+03
 0.29988170402314E+03 0.30933064176717E+03 0.29315000000010E+03 0.29315000000010E+03 0.33225697384198E+03
 0.29347583680087E+03 0.12697058770838E+04 0.12465127845358E+04 0.50224301801512E+03 0.13343350245786E+04
 0.82958079147345E+03 0.74335112565206E+03 0.64604199718104E+03 0.72465557203139E+03 0.14305209885900E+04
 0.61695291970048E+03 0.64316266905944E+03 0.60468631693697E+03 0.14281061615552E+04 0.74335112565206E+03
 0.64604199718104E+03 0.72465557203139E+03 0.14305209885900E+04 0.61695291970048E+03 0.64316266905944E+03
 0.60468631693697E+03 0.14281061615552E+04 0.14447922189537E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44402289285177E+03 0.12947755709164E+01
 0.12947755709164E+01 0.34096718584502E+00 0.51347552995967E+00 0.29316040960685E+03 0.32039511778717E+03
 0.31107221364304E+03 0.31052015002669E+03 0.22999999999826E+00 0.00000000000000E+00 0.22387160014992E+00
 0.00000000000000E+00 -.31081645184460E+01 0.99964770324055E-03 0.78403821044228E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10203584332308E+03 0.38263441246157E+02 0.29347523296925E+03 0.33228623432269E+03
 0.29324898209573E+03 0.29510690241727E+03 0.29315000000010E+03 0.29315000000010E+03 0.29324757724600E+03
 0.29510757082296E+03 0.29315000000010E+03 0.29315000000010E+03 0.29324898209573E+03 0.29510690241727E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29324757724600E+03 0.29510757082296E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29472598015760E+03 0.29315000000009E+03 0.19157499238770E+02 0.18765509606920E+02
 0.14299825245945E+02 0.22807255027249E+03 0.21370122590031E+03 0.13785191265819E+02 0.12986228231759E+02
 0.13706649209133E+02 0.18441244243899E+03 0.13567376390753E+02 0.13059542711259E+02 0.13490410621915E+02
 0.18448032752858E+03 0.13785191265819E+02 0.12986228231759E+02 0.13706649209133E+02 0.18441244243899E+03
 0.13567376390753E+02 0.13059542711259E+02 0.13490410621915E+02 0.18448032752858E+03 0.97030692539887E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33952174110421E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.54799798013445E-01 0.00000000000000E+00 0.00000000000000E+00 0.54799798013445E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17818636272390E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17818636272390E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17982274740732E+00 0.19757736658283E+00 0.32039511778717E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    140.55151576
 0.14995921725578E+00 0.29663170937992E+03 0.41260039674693E+03 0.40680353888568E+03 0.40469179910599E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20736411443609E+00 0.00000000000000E+00 -.11378763895187E+02
 0.23705908123380E-02 0.34704386741551E+00 0.33746861577136E+04 0.12655073091426E+04 0.23051840851064E+02
 0.86444403191490E+01 0.30361772320298E+03 0.29315000000009E+03 0.30239230057502E+03 0.31109677151965E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30067715602236E+03 0.31105013150326E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30239230057502E+03 0.31109677151965E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30067715602236E+03 0.31105013150326E+03 0.29315000000010E+03 0.29315000000010E+03 0.33577685012572E+03
 0.29359591030727E+03 0.13499283425094E+04 0.13257848212613E+04 0.53314512118597E+03 0.13695120932120E+04
 0.83370124642007E+03 0.78335575592049E+03 0.69695584070013E+03 0.76355690446646E+03 0.14953451039871E+04
 0.65767483305928E+03 0.69432726356297E+03 0.64503136509075E+03 0.14931760238507E+04 0.78335575592049E+03
 0.69695584070013E+03 0.76355690446646E+03 0.14953451039871E+04 0.65767483305929E+03 0.69432726356297E+03
 0.64503136509076E+03 0.14931760238507E+04 0.15202141676297E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45056641441122E+03 0.12947883302649E+01
 0.12947883302649E+01 0.38222138535139E+00 0.47929500220740E+00 0.29316902173569E+03 0.32169752953441E+03
 0.31258181539551E+03 0.31199640218525E+03 0.23000000000000E+00 0.00000000000000E+00 0.22306446380602E+00
 0.00000000000000E+00 -.44307142339589E+01 0.99960528505167E-03 0.98282314424945E-01 0.80000000000000E+04
 0.30000000000000E+04 0.81398164530500E+02 0.30524311698938E+02 0.29359520240467E+03 0.33580830535427E+03
 0.29328193062695E+03 0.29535877898028E+03 0.29315000000010E+03 0.29315000000010E+03 0.29327998285323E+03
 0.29535954069923E+03 0.29315000000010E+03 0.29315000000010E+03 0.29328193062695E+03 0.29535877898028E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29327998285323E+03 0.29535954069923E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29494643978321E+03 0.29315000000009E+03 0.24135400239984E+02 0.23548445670980E+02
 0.19369540420407E+02 0.24421289225135E+03 0.22474650412884E+03 0.17212598402296E+02 0.17932048950566E+02
 0.17104293782936E+02 0.19682533715134E+03 0.16939616243933E+02 0.18009550948471E+02 0.16833675800189E+02
 0.19689662989524E+03 0.17212598402296E+02 0.17932048950566E+02 0.17104293782936E+02 0.19682533715134E+03
 0.16939616243933E+02 0.18009550948471E+02 0.16833675800188E+02 0.19689662989524E+03 0.10607788771224E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34113028134088E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.83298645841741E-01 0.00000000000000E+00 0.00000000000000E+00 0.83298645841741E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17859132203933E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17859132203933E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17978044882637E+00 0.19672860720422E+00 0.32169752953441E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    150.14224056
 0.11241150878908E+00 0.29740875838403E+03 0.41578948405144E+03 0.41135369872350E+03 0.40967920996735E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20541007066600E+00 0.00000000000000E+00 -.13142290037000E+02
 0.31624093398040E-02 0.37417506998149E+00 0.25297167888126E+04 0.94864379580473E+03 0.21380366148914E+02
 0.80176373058429E+01 0.30465127742872E+03 0.29315000000009E+03 0.30321363747676E+03 0.31266933384485E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30142060397303E+03 0.31262316026353E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30321363747676E+03 0.31266933384485E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30142060397303E+03 0.31262316026353E+03 0.29315000000010E+03 0.29315000000010E+03 0.33891661855457E+03
 0.29372533583842E+03 0.14159520766632E+04 0.13906694295687E+04 0.56033754832686E+03 0.13962758346043E+04
 0.83313659853580E+03 0.81772278805064E+03 0.74175737559508E+03 0.79658711437194E+03 0.15510134814422E+04
 0.69273975849041E+03 0.73932626040694E+03 0.67947662628204E+03 0.15490365637504E+04 0.81772278805064E+03
 0.74175737559508E+03 0.79658711437194E+03 0.15510134814422E+04 0.69273975849041E+03 0.73932626040694E+03
 0.67947662628204E+03 0.15490365637504E+04 0.15810038637134E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45536988695763E+03 0.12948013226317E+01
 0.12948013226317E+01 0.42058428454043E+00 0.44878579228108E+00 0.29318123239812E+03 0.32274101246876E+03
 0.31389700625632E+03 0.31329039988945E+03 0.22999999999628E+00 0.00000000000000E+00 0.22227384580596E+00
 0.00000000000000E+00 -.59116951628355E+01 0.99954903709809E-03 0.11694222565595E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68409849009855E+02 0.25653693378696E+02 0.29372453424492E+03 0.33895006293585E+03
 0.29331665686974E+03 0.29559193925254E+03 0.29315000000010E+03 0.29315000000010E+03 0.29331415527972E+03
 0.29559278163835E+03 0.29315000000010E+03 0.29315000000010E+03 0.29331665686974E+03 0.29559193925254E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29331415527972E+03 0.29559278163835E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29515014700526E+03 0.29315000000009E+03 0.28982246247608E+02 0.28170992660380E+02
 0.24388354955035E+02 0.25794565401979E+03 0.23343535728998E+03 0.20572484432071E+02 0.22812837536788E+02
 0.20434329614722E+02 0.20737252091858E+03 0.20249107341709E+02 0.22892997704900E+02 0.20114180908557E+02
 0.20744579956318E+03 0.20572484432071E+02 0.22812837536788E+02 0.20434329614722E+02 0.20737252091858E+03
 0.20249107341709E+02 0.22892997704900E+02 0.20114180908557E+02 0.20744579956318E+03 0.11409537888936E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34245347338102E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10649831146653E+00 0.00000000000000E+00 0.00000000000000E+00 0.10649831146653E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17902922745757E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17902922745757E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17973407930485E+00 0.19603910299009E+00 0.32274101246876E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    161.21300212
 0.89117700396748E-01 0.29804319509234E+03 0.41890085993834E+03 0.41531067421620E+03 0.41391490022035E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20310422843439E+00 0.00000000000000E+00 -.15142705058155E+02
 0.39890013886403E-02 0.40629542139479E+00 0.20055144685540E+04 0.75206792570774E+03 0.19690106210246E+02
 0.73837898288422E+01 0.30582566881537E+03 0.29315000000009E+03 0.30415048960108E+03 0.31445233043557E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30227562182613E+03 0.31440680854723E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30415048960108E+03 0.31445233043557E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30227562182613E+03 0.31440680854723E+03 0.29315000000010E+03 0.29315000000010E+03 0.34237165317707E+03
 0.29389850915701E+03 0.14811849481585E+04 0.14534086921401E+04 0.58835297911891E+03 0.14181942501529E+04
 0.82689950613835E+03 0.85306558740191E+03 0.78894663976609E+03 0.82938489826801E+03 0.16071335833536E+04
 0.72877152978670E+03 0.78671120202120E+03 0.71382617566870E+03 0.16053452889269E+04 0.85306558740191E+03
 0.78894663976609E+03 0.82938489826801E+03 0.16071335833536E+04 0.72877152978670E+03 0.78671120202120E+03
 0.71382617566870E+03 0.16053452889269E+04 0.16383623139252E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45950972954269E+03 0.12948160606053E+01
 0.12948160606053E+01 0.46486733079028E+00 0.41518696440237E+00 0.29320249283029E+03 0.32376697294824E+03
 0.31530699046910E+03 0.31468708021164E+03 0.22999999998033E+00 0.00000000000000E+00 0.22131092101949E+00
 0.00000000000000E+00 -.76877655353168E+01 0.99945903225521E-03 0.13886708076174E+00 0.80000000000000E+04
 0.30000000000000E+04 0.57609045686832E+02 0.21603392132562E+02 0.29389758623943E+03 0.34240706022317E+03
 0.29336209190550E+03 0.29586058910089E+03 0.29315000000010E+03 0.29315000000010E+03 0.29335889867572E+03
 0.29586151442014E+03 0.29315000000010E+03 0.29315000000010E+03 0.29336209190550E+03 0.29586058910089E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29335889867572E+03 0.29586151442014E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29538501064018E+03 0.29315000000009E+03 0.34769017274100E+02 0.33642435450423E+02
 0.30471896071302E+02 0.27235029246865E+03 0.24172603691699E+03 0.24631605131257E+02 0.28712903702959E+02
 0.24458852195341E+02 0.21845074284898E+03 0.24252317440340E+02 0.28794564605670E+02 0.24083933930685E+02
 0.21852483435422E+03 0.24631605131257E+02 0.28712903702959E+02 0.24458852195340E+02 0.21845074284898E+03
 0.24252317440340E+02 0.28794564605670E+02 0.24083933930685E+02 0.21852483435422E+03 0.12285887385580E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34378292331615E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12888638378314E+00 0.00000000000000E+00 0.00000000000000E+00 0.12888638378314E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17972400244589E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17972400244589E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17967906274426E+00 0.19535464652225E+00 0.32376697294824E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    170.93131707
 0.80529229961003E-01 0.29836794004189E+03 0.42109212440491E+03 0.41779782971679E+03 0.41649359867339E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20106968498029E+00 0.00000000000000E+00 -.16694022207860E+02
 0.44144258605643E-02 0.43481723449361E+00 0.18122401989955E+04 0.67959007462333E+03 0.18398534752922E+02
 0.68994505323456E+01 0.30681692806173E+03 0.29315000000009E+03 0.30494553388253E+03 0.31596022952220E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30300402863379E+03 0.31591531530535E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30494553388253E+03 0.31596022952220E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30300402863379E+03 0.31591531530535E+03 0.29315000000010E+03 0.29315000000010E+03 0.34521342582222E+03
 0.29407013705855E+03 0.15286841028359E+04 0.14977275843720E+04 0.60887102934008E+03 0.14286961614775E+04
 0.81678077699070E+03 0.87969569504571E+03 0.82515129637494E+03 0.85309949948943E+03 0.16475047844700E+04
 0.75586133616233E+03 0.82306656855541E+03 0.73870218299340E+03 0.16458606183404E+04 0.87969569504571E+03
 0.82515129637494E+03 0.85309949948943E+03 0.16475047844700E+04 0.75586133616233E+03 0.82306656855542E+03
 0.73870218299340E+03 0.16458606183404E+04 0.16769290066895E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46206226321046E+03 0.12948274901496E+01
 0.12948274901496E+01 0.50374059058862E+00 0.38717783743181E+00 0.29322920444346E+03 0.32452526366623E+03
 0.31644716997956E+03 0.31582469285411E+03 0.22999999995982E+00 0.00000000000000E+00 0.22042068699688E+00
 0.00000000000000E+00 -.91128225730463E+01 0.99935392554472E-03 0.15855736414710E+00 0.80000000000000E+04
 0.30000000000000E+04 0.50454925528264E+02 0.18920597073099E+02 0.29406911940788E+03 0.34525001187847E+03
 0.29340559048945E+03 0.29609222445096E+03 0.29315000000010E+03 0.29315000000010E+03 0.29340176470827E+03
 0.29609321371875E+03 0.29315000000010E+03 0.29315000000010E+03 0.29340559048945E+03 0.29609222445096E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29340176470827E+03 0.29609321371875E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29558689449882E+03 0.29315000000009E+03 0.39949814071437E+02 0.38500090091711E+02
 0.36008193008932E+02 0.28380246467075E+03 0.24761423069678E+03 0.28315785940734E+02 0.34064771797260E+02
 0.28116963567359E+02 0.22727892300367E+03 0.27890443220353E+02 0.34146316405715E+02 0.27697066987601E+02
 0.22735237216449E+03 0.28315785940733E+02 0.34064771797260E+02 0.28116963567358E+02 0.22727892300367E+03
 0.27890443220353E+02 0.34146316405715E+02 0.27697066987601E+02 0.22735237216449E+03 0.13010760358979E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34477152896371E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14433313938903E+00 0.00000000000000E+00 0.00000000000000E+00 0.14433313938903E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18040437736649E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18040437736649E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17963517823305E+00 0.19484783465907E+00 0.32452526366623E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    180.06553007
 0.78463614055468E-01 0.29859707790874E+03 0.42270696359376E+03 0.41946092687014E+03 0.41816115068421E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19917732010592E+00 0.00000000000000E+00 -.17904478397155E+02
 0.45306367401082E-02 0.46154388570555E+00 0.17657562190274E+04 0.66215858213526E+03 0.17333129628119E+02
 0.64999236105447E+01 0.30772183608907E+03 0.29315000000009E+03 0.30567459737484E+03 0.31733323888692E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30367411497779E+03 0.31728896087153E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30567459737484E+03 0.31733323888692E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30367411497779E+03 0.31728896087153E+03 0.29315000000010E+03 0.29315000000010E+03 0.34771359695987E+03
 0.29425198072232E+03 0.15655568229064E+04 0.15312880606596E+04 0.62416869140428E+03 0.14311141573482E+04
 0.80382462248686E+03 0.90093725058328E+03 0.85411318084839E+03 0.87161157538547E+03 0.16771332324504E+04
 0.77743099666090E+03 0.85215623903081E+03 0.75808842317212E+03 0.16756099067387E+04 0.90093725058328E+03
 0.85411318084840E+03 0.87161157538547E+03 0.16771332324504E+04 0.77743099666090E+03 0.85215623903081E+03
 0.75808842317212E+03 0.16756099067387E+04 0.17035470126458E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46372854051592E+03 0.12948364085238E+01
 0.12948364085238E+01 0.54027744260525E+00 0.36215331621310E+00 0.29326350474877E+03 0.32512351223369E+03
 0.31743137399020E+03 0.31681366637918E+03 0.22999999999262E+00 0.00000000000000E+00 0.21954693101457E+00
 0.00000000000000E+00 -.10253561184256E+02 0.99922578581905E-03 0.17748445746168E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45074369409091E+02 0.16902888528409E+02 0.29425085919170E+03 0.34775089945543E+03
 0.29345074646782E+03 0.29630904580179E+03 0.29315000000010E+03 0.29315000000010E+03 0.29344630032338E+03
 0.29631008466694E+03 0.29315000000010E+03 0.29315000000010E+03 0.29345074646782E+03 0.29630904580179E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29344630032338E+03 0.29631008466694E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29577620577345E+03 0.29315000000009E+03 0.44835400267752E+02 0.43037108409250E+02
 0.41308589924575E+02 0.29350870990205E+03 0.25199357702785E+03 0.31862923854165E+02 0.39174270761096E+02
 0.31645584418108E+02 0.23479159291316E+03 0.31397699507987E+02 0.39254396929663E+02 0.31186869387083E+02
 0.23486321996096E+03 0.31862923854165E+02 0.39174270761096E+02 0.31645584418108E+02 0.23479159291316E+03
 0.31397699507987E+02 0.39254396929663E+02 0.31186869387083E+02 0.23486321996096E+03 0.13648770417260E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34556059342809E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15552766517423E+00 0.00000000000000E+00 0.00000000000000E+00 0.15552766517423E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18125367496151E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18125367496151E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17960019581356E+00 0.19444923659979E+00 0.32512351223369E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    190.07097699
 0.79856692361816E-01 0.29883626531466E+03 0.42404001658099E+03 0.42070723076518E+03 0.41936305000447E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19715041492703E+00 0.00000000000000E+00 -.18927005223070E+02
 0.44515995787991E-02 0.49040408864905E+00 0.17971068283186E+04 0.67391506061948E+03 0.16313077694842E+02
 0.61174041355656E+01 0.30867039837256E+03 0.29315000000010E+03 0.30644356677632E+03 0.31876598287943E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30438116751273E+03 0.31872241540913E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30644356677632E+03 0.31876598287943E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30438116751273E+03 0.31872241540913E+03 0.29315000000010E+03 0.29315000000010E+03 0.35025002076203E+03
 0.29447034007265E+03 0.15983987941174E+04 0.15605698850363E+04 0.63659128423917E+03 0.14266545538259E+04
 0.78688031316555E+03 0.92031331730981E+03 0.88031607071931E+03 0.88834778873567E+03 0.17010741428616E+04
 0.79709535906729E+03 0.87848662175403E+03 0.77559348596263E+03 0.16996704558664E+04 0.92031331730981E+03
 0.88031607071931E+03 0.88834778873567E+03 0.17010741428616E+04 0.79709535906729E+03 0.87848662175403E+03
 0.77559348596263E+03 0.16996704558664E+04 0.17235497711284E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46494264491524E+03 0.12948439423921E+01
 0.12948439423921E+01 0.58029923026023E+00 0.33618831398822E+00 0.29331310033306E+03 0.32566504255563E+03
 0.31841414594893E+03 0.31780860074613E+03 0.23000000000000E+00 0.00000000000000E+00 0.21855032214142E+00
 0.00000000000000E+00 -.11234196465287E+02 0.99904715584532E-03 0.19871101749865E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40259468753685E+02 0.15097300782632E+02 0.29446909542390E+03 0.35028765404323E+03
 0.29350335751220E+03 0.29654178697411E+03 0.29315000000010E+03 0.29315000000010E+03 0.29349822662377E+03
 0.29654286995919E+03 0.29315000000010E+03 0.29315000000010E+03 0.29350335751220E+03 0.29654178697411E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29349822662377E+03 0.29654286995919E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29597905179989E+03 0.29315000000009E+03 0.50135679166664E+02 0.47917106482348E+02
 0.47125688081432E+02 0.30301485410640E+03 0.25565353758456E+03 0.35760125805092E+02 0.44777000206031E+02
 0.35534932946751E+02 0.24218159519073E+03 0.35254997018900E+02 0.44854926829344E+02 0.35037449359913E+02
 0.24225066977422E+03 0.35760125805092E+02 0.44777000206031E+02 0.35534932946751E+02 0.24218159519073E+03
 0.35254997018900E+02 0.44854926829344E+02 0.35037449359913E+02 0.24225066977422E+03 0.14297676183543E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34627353346900E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16450638041348E+00 0.00000000000000E+00 0.00000000000000E+00 0.16450638041348E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18215099285263E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18215099285263E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17957023941506E+00 0.19409163923628E+00 0.32566504255563E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    200.39483625
 0.83083297954478E-01 0.29909890545760E+03 0.42503052956309E+03 0.42154292468061E+03 0.42013163784302E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19512600721136E+00 0.00000000000000E+00 -.19681015692714E+02
 0.42787174683616E-02 0.51946463924260E+00 0.18697191527964E+04 0.70114468229863E+03 0.15400470784045E+02
 0.57751765440168E+01 0.30960399556328E+03 0.29315000000011E+03 0.30720526903047E+03 0.32016435880521E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30508167408419E+03 0.32012154200973E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30720526903047E+03 0.32016435880521E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30508167408419E+03 0.32012154200973E+03 0.29315000000010E+03 0.29315000000010E+03 0.35264809853875E+03
 0.29471796667692E+03 0.16254862619017E+04 0.15842193796070E+04 0.64527295447128E+03 0.14161283673935E+04
 0.76762904814990E+03 0.93666276341525E+03 0.90190991302415E+03 0.90236396275090E+03 0.17178214817575E+04
 0.81370576335972E+03 0.90020042879043E+03 0.79028283351564E+03 0.17165293977275E+04 0.93666276341525E+03
 0.90190991302415E+03 0.90236396275090E+03 0.17178214817575E+04 0.81370576335972E+03 0.90020042879043E+03
 0.79028283351564E+03 0.17165293977275E+04 0.17361796906230E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46573123533542E+03 0.12948494979287E+01
 0.12948494979287E+01 0.62159466730505E+00 0.31098077389007E+00 0.29338030484825E+03 0.32610854863156E+03
 0.31932331224502E+03 0.31873687105314E+03 0.23000000000000E+00 0.00000000000000E+00 0.21748282525621E+00
 0.00000000000000E+00 -.11972949196299E+02 0.99881101881919E-03 0.22112998177631E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36177816937066E+02 0.13566681351400E+02 0.29471660955416E+03 0.35268573336425E+03
 0.29356112890385E+03 0.29677654092442E+03 0.29315000000010E+03 0.29315000000010E+03 0.29355529325406E+03
 0.29677765721336E+03 0.29315000000010E+03 0.29315000000010E+03 0.29356112890385E+03 0.29677654092442E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29355529325406E+03 0.29677765721336E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29618333442370E+03 0.29315000000009E+03 0.55471440236034E+02 0.52780599869513E+02
 0.53089455752501E+02 0.31164663273249E+03 0.25829172970122E+03 0.39788369812683E+02 0.50503687710938E+02
 0.39573655195179E+02 0.24892312842601E+03 0.39247412475919E+02 0.50578093937239E+02 0.39041396720986E+02
 0.24898842377248E+03 0.39788369812683E+02 0.50503687710937E+02 0.39573655195179E+02 0.24892312842601E+03
 0.39247412475919E+02 0.50578093937239E+02 0.39041396720986E+02 0.24898842377248E+03 0.14911171133413E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34686866255707E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17088819330533E+00 0.00000000000000E+00 0.00000000000000E+00 0.17088819330533E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18315846308387E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18315846308387E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17954781015944E+00 0.19380205178695E+00 0.32610854863156E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    211.14246720
 0.86944818276339E-01 0.29938971330966E+03 0.42575697961958E+03 0.42209465328445E+03 0.42061178739235E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19309987311025E+00 0.00000000000000E+00 -.20205562377295E+02
 0.40886845034802E-02 0.54875579067885E+00 0.19566195418577E+04 0.73373232819662E+03 0.14578433860540E+02
 0.54669126977026E+01 0.31052630707313E+03 0.29315000000013E+03 0.30796203661118E+03 0.32153223660983E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30577737976075E+03 0.32149019428948E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30796203661118E+03 0.32153223660983E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30577737976075E+03 0.32149019428948E+03 0.29315000000010E+03 0.29315000000010E+03 0.35492965839242E+03
 0.29499867505815E+03 0.16480311132797E+04 0.16034708865509E+04 0.65073515706915E+03 0.14008211132113E+04
 0.74683228035678E+03 0.95055395003555E+03 0.91956864904286E+03 0.91414148541643E+03 0.17286694802218E+04
 0.82785910029044E+03 0.91797308809925E+03 0.80270394496195E+03 0.17274826460198E+04 0.95055395003555E+03
 0.91956864904286E+03 0.91414148541643E+03 0.17286694802218E+04 0.82785910029044E+03 0.91797308809925E+03
 0.80270394496195E+03 0.17274826460198E+04 0.17430162719601E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46623450983408E+03 0.12948533628142E+01
 0.12948533628142E+01 0.66458519109033E+00 0.28640335650413E+00 0.29347061503739E+03 0.32645965322850E+03
 0.32016087238465E+03 0.31960005435583E+03 0.22999999976894E+00 0.00000000000000E+00 0.21633430749427E+00
 0.00000000000000E+00 -.12497993565799E+02 0.99849847662111E-03 0.24498103075304E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32655589599770E+02 0.12245846099914E+02 0.29499721391759E+03 0.35496710914633E+03
 0.29362398932256E+03 0.29701337913775E+03 0.29315000000010E+03 0.29315000000010E+03 0.29361743799734E+03
 0.29701451739570E+03 0.29315000000010E+03 0.29315000000010E+03 0.29362398932256E+03 0.29701337913775E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29361743799734E+03 0.29701451739570E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29638887853574E+03 0.29315000000009E+03 0.60798264291173E+02 0.57585820291447E+02
 0.59167279750414E+02 0.31940399434521E+03 0.25994087819604E+03 0.43933731180516E+02 0.56324028378696E+02
 0.43755993321924E+02 0.25503341205319E+03 0.43361472040986E+02 0.56393763027269E+02 0.43193222020665E+02
 0.25509387493067E+03 0.43933731180516E+02 0.56324028378696E+02 0.43755993321924E+02 0.25503341205319E+03
 0.43361472040986E+02 0.56393763027269E+02 0.43193222020665E+02 0.25509387493067E+03 0.15487844258957E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34731785454345E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17522009626486E+00 0.00000000000000E+00 0.00000000000000E+00 0.17522009626486E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18417095946663E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18417095946663E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17953204388632E+00 0.19357561612570E+00 0.32645965322850E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    220.79185396
 0.90211724736560E-01 0.29966734902595E+03 0.42623163375743E+03 0.42242577295221E+03 0.42088625823065E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19135343109815E+00 0.00000000000000E+00 -.20501543403164E+02
 0.39406177213409E-02 0.57412732714637E+00 0.20301385634732E+04 0.76130196130244E+03 0.13934191287781E+02
 0.52253217329179E+01 0.31131872661624E+03 0.29315000000019E+03 0.30861549073135E+03 0.32269311496358E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30637824888540E+03 0.32265177894209E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30861549073135E+03 0.32269311496358E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30637824888540E+03 0.32265177894209E+03 0.29315000000010E+03 0.29315000000010E+03 0.35680575201589E+03
 0.29527245986406E+03 0.16646275965784E+04 0.16173450774306E+04 0.65343743339096E+03 0.13850989390012E+04
 0.72839431844332E+03 0.96095536824084E+03 0.93217310166502E+03 0.92284302343008E+03 0.17342800996884E+04
 0.83850082998545E+03 0.93067124123984E+03 0.81196681928083E+03 0.17331790760227E+04 0.96095536824084E+03
 0.93217310166502E+03 0.92284302343008E+03 0.17342800996884E+04 0.83850082998546E+03 0.93067124123984E+03
 0.81196681928083E+03 0.17331790760227E+04 0.17454579097748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46652837191715E+03 0.12948555436291E+01
 0.12948555436291E+01 0.70318273815555E+00 0.26533339182291E+00 0.29357265393446E+03 0.32668197808817E+03
 0.32082530523573E+03 0.32029225837373E+03 0.22999999696937E+00 0.00000000000000E+00 0.21528020989102E+00
 0.00000000000000E+00 -.12801560585414E+02 0.99814843035942E-03 0.26670858644277E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29995284766420E+02 0.11248231787407E+02 0.29527090257802E+03 0.35684288797636E+03
 0.29368341004423E+03 0.29722035043702E+03 0.29315000000010E+03 0.29315000000010E+03 0.29367623436353E+03
 0.29722149568755E+03 0.29315000000010E+03 0.29315000000010E+03 0.29368341004423E+03 0.29722035043702E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29367623436353E+03 0.29722149568755E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29656815185014E+03 0.29315000000010E+03 0.65306894227661E+02 0.61605329207562E+02
 0.64439908534656E+02 0.32523017892365E+03 0.26046807084632E+03 0.47592903161522E+02 0.61352254146242E+02
 0.47472594099756E+02 0.25968155705775E+03 0.46998909850274E+02 0.61416749636995E+02 0.46888270944191E+02
 0.25973674384807E+03 0.47592903161522E+02 0.61352254146242E+02 0.47472594099756E+02 0.25968155705775E+03
 0.46998909850274E+02 0.61416749636995E+02 0.46888270944191E+02 0.25973674384807E+03 0.15947681203284E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34753028005483E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17761831132261E+00 0.00000000000000E+00 0.00000000000000E+00 0.17761831132261E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18503458205664E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18503458205664E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17952314822229E+00 0.19343371260419E+00 0.32668197808817E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    230.44124073
 0.93014209671568E-01 0.29995582212855E+03 0.42660161578109E+03 0.42267499631282E+03 0.42108927949433E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18967412475025E+00 0.00000000000000E+00 -.20685941964421E+02
 0.38218882551105E-02 0.59859144700041E+00 0.20932061499451E+04 0.78495230622940E+03 0.13364708166294E+02
 0.50117655623602E+01 0.31208022656761E+03 0.29315000000029E+03 0.30924587883896E+03 0.32379605068215E+03
 0.29315000000010E+03 0.29315000000011E+03 0.30695799729000E+03 0.32375541917972E+03 0.29315000000010E+03
 0.29315000000011E+03 0.30924587883896E+03 0.32379605068215E+03 0.29315000000010E+03 0.29315000000011E+03
 0.30695799729000E+03 0.32375541917972E+03 0.29315000000010E+03 0.29315000000011E+03 0.35854317808645E+03
 0.29556558536084E+03 0.16787495523434E+04 0.16289377700054E+04 0.65468835929193E+03 0.13685830148685E+04
 0.71062121378008E+03 0.96991263552463E+03 0.94250006405689E+03 0.93023170896717E+03 0.17372693314018E+04
 0.84770823794275E+03 0.94108449520642E+03 0.81991665123930E+03 0.17362467584782E+04 0.96991263552463E+03
 0.94250006405689E+03 0.93023170896717E+03 0.17372693314018E+04 0.84770823794275E+03 0.94108449520642E+03
 0.81991665123930E+03 0.17362467584782E+04 0.17456947160562E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46674775578473E+03 0.12948569022989E+01
 0.12948569022989E+01 0.74178028522076E+00 0.24507400168358E+00 0.29369795682394E+03 0.32682867934770E+03
 0.32141569351599E+03 0.32091408794225E+03 0.22999999695334E+00 0.00000000000000E+00 0.21421022996828E+00
 0.00000000000000E+00 -.12995182746632E+02 0.99772067423469E-03 0.28864711179542E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27715503371016E+02 0.10393313764131E+02 0.29556393188278E+03 0.35857986836870E+03
 0.29374512486388E+03 0.29742095322014E+03 0.29315000000010E+03 0.29315000000010E+03 0.29373734489823E+03
 0.29742209369078E+03 0.29315000000010E+03 0.29315000000010E+03 0.29374512486388E+03 0.29742095322014E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29373734489823E+03 0.29742209369078E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29674169788502E+03 0.29315000000011E+03 0.69513789354186E+02 0.65318615835605E+02
 0.69488998686840E+02 0.33012229083365E+03 0.26028584715338E+03 0.51164737959886E+02 0.66149483854898E+02
 0.51164737959886E+02 0.26357876371826E+03 0.50555348111297E+02 0.66207936432983E+02 0.50555348111297E+02
 0.26362796964526E+03 0.51164737959886E+02 0.66149483854897E+02 0.51164737959886E+02 0.26357876371826E+03
 0.50555348111297E+02 0.66207936432983E+02 0.50555348111297E+02 0.26362796964526E+03 0.16356100798990E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34767697568539E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17908291010702E+00 0.00000000000000E+00 0.00000000000000E+00 0.17908291010702E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18581829993588E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18581829993588E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951772734909E+00 0.19334067946595E+00 0.32682867934770E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    240.93148771
 0.95467761418802E-01 0.30027439010484E+03 0.42693573529185E+03 0.42290504359742E+03 0.42128073172726E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18792090138044E+00 0.00000000000000E+00 -.20807969373149E+02
 0.37236644437339E-02 0.62416322662232E+00 0.21484212986652E+04 0.80565798699944E+03 0.12817160093350E+02
 0.48064350350062E+01 0.31287606515354E+03 0.29315000000049E+03 0.30990682727920E+03 0.32493630069698E+03
 0.29315000000011E+03 0.29315000000011E+03 0.30756594578910E+03 0.32489642256959E+03 0.29315000000011E+03
 0.29315000000011E+03 0.30990682727920E+03 0.32493630069698E+03 0.29315000000011E+03 0.29315000000011E+03
 0.30756594578910E+03 0.32489642256959E+03 0.29315000000011E+03 0.29315000000011E+03 0.36029964858666E+03
 0.29590541885231E+03 0.16920912985598E+04 0.16397016024853E+04 0.65494146452597E+03 0.13505363036814E+04
 0.69232013183282E+03 0.97844872595360E+03 0.95185106351999E+03 0.93715921294310E+03 0.17386224954812E+04
 0.85652748612145E+03 0.95052160356590E+03 0.82745953443953E+03 0.17376776849775E+04 0.97844872595360E+03
 0.95185106351999E+03 0.93715921294310E+03 0.17386224954812E+04 0.85652748612145E+03 0.95052160356590E+03
 0.82745953443953E+03 0.17376776849775E+04 0.17444704524559E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46695310092341E+03 0.12948578014128E+01
 0.12948578014128E+01 0.78374127314018E+00 0.22399490028220E+00 0.29386459373041E+03 0.32692124082625E+03
 0.32198489391299E+03 0.32152033193167E+03 0.22999999699473E+00 0.00000000000000E+00 0.21303249227551E+00
 0.00000000000000E+00 -.13126258790504E+02 0.99715362281172E-03 0.31268510928663E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25584844824403E+02 0.95943168091512E+01 0.29590366911908E+03 0.36033574827728E+03
 0.29381405294633E+03 0.29763145251430E+03 0.29315000000010E+03 0.29315000000010E+03 0.29380563268347E+03
 0.29763257503648E+03 0.29315000000010E+03 0.29315000000010E+03 0.29381405294633E+03 0.29763145251430E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29380563268347E+03 0.29763257503648E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29692361403524E+03 0.29315000000013E+03 0.73723816402329E+02 0.69004262351308E+02
 0.74706156684716E+02 0.33455971806810E+03 0.25948003059996E+03 0.54940283886165E+02 0.71087813243647E+02
 0.54940283886165E+02 0.26710561742282E+03 0.54321222614224E+02 0.71138945104640E+02 0.54321222614224E+02
 0.26714767538383E+03 0.54940283886165E+02 0.71087813243647E+02 0.54940283886165E+02 0.26710561742282E+03
 0.54321222614224E+02 0.71138945104640E+02 0.54321222614224E+02 0.26714767538383E+03 0.16747544995012E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34778353427410E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18002060508249E+00 0.00000000000000E+00 0.00000000000000E+00 0.18002060508249E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18657834321853E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18657834321853E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951435985163E+00 0.19328206283777E+00 0.32692124082625E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    250.00037774
 0.97101615922861E-01 0.30055612296739E+03 0.42719901286428E+03 0.42309993644624E+03 0.42145113328443E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18646236506756E+00 0.00000000000000E+00 -.20859526135431E+02
 0.36610090994753E-02 0.64542849978536E+00 0.21851898705050E+04 0.81944620143937E+03 0.12394866360349E+02
 0.46480748851308E+01 0.31354364913447E+03 0.29315000000079E+03 0.31046301260795E+03 0.32588008176330E+03
 0.29315000000011E+03 0.29315000000012E+03 0.30807807312154E+03 0.32584086602220E+03 0.29315000000011E+03
 0.29315000000012E+03 0.31046301260795E+03 0.32588008176330E+03 0.29315000000011E+03 0.29315000000012E+03
 0.30807807312154E+03 0.32584086602220E+03 0.29315000000011E+03 0.29315000000012E+03 0.36171004587722E+03
 0.29621835805738E+03 0.17024227969311E+04 0.16479313961015E+04 0.65473701132399E+03 0.13356818444986E+04
 0.67767114811797E+03 0.98509381154314E+03 0.95882233970218E+03 0.94248094974101E+03 0.17388640072638E+04
 0.86342512093357E+03 0.95756140264701E+03 0.83331641439151E+03 0.17379804539563E+04 0.98509381154314E+03
 0.95882233970218E+03 0.94248094974100E+03 0.17388640072638E+04 0.86342512093357E+03 0.95756140264701E+03
 0.83331641439151E+03 0.17379804539563E+04 0.17428663722863E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46713226752915E+03 0.12948581812902E+01
 0.12948581812902E+01 0.82001683326508E+00 0.20657970221919E+00 0.29403811794926E+03 0.32695930026197E+03
 0.32242540156939E+03 0.32199433281238E+03 0.22999999707898E+00 0.00000000000000E+00 0.21200460661965E+00
 0.00000000000000E+00 -.13183095178760E+02 0.99656460138669E-03 0.33358502227924E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23981892068593E+02 0.89932095257225E+01 0.29621652390299E+03 0.36174549627925E+03
 0.29387664050370E+03 0.29780821516583E+03 0.29315000000010E+03 0.29315000000010E+03 0.29386779691736E+03
 0.29780930909687E+03 0.29315000000010E+03 0.29315000000010E+03 0.29387664050370E+03 0.29780821516583E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29386779691736E+03 0.29780930909687E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29707659540915E+03 0.29315000000016E+03 0.77049214092877E+02 0.71897422578778E+02
 0.78991689770046E+02 0.33779079172756E+03 0.25840414350867E+03 0.58123286375237E+02 0.75127792172623E+02
 0.58328428022847E+02 0.26966634351941E+03 0.57501139646845E+02 0.75171893421326E+02 0.57720392359663E+02
 0.26970162615842E+03 0.58123286375237E+02 0.75127792172623E+02 0.58328428022847E+02 0.26966634351941E+03
 0.57501139646845E+02 0.75171893421326E+02 0.57720392359663E+02 0.26970162615842E+03 0.17048705195558E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34784639915697E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18037568395897E+00 0.00000000000000E+00 0.00000000000000E+00 0.18037568395897E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18715247477303E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18715247477303E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951322371374E+00 0.19325823240644E+00 0.32695930026197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    260.02183858
 0.98477654469074E-01 0.30086329363355E+03 0.42748000094720E+03 0.42332369549625E+03 0.42165500654211E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18490816224815E+00 0.00000000000000E+00 -.20905495875380E+02
 0.36098532925125E-02 0.66805887904027E+00 0.22161565448085E+04 0.83105870430318E+03 0.11974992401108E+02
 0.44906221504155E+01 0.31425768471594E+03 0.29315000000134E+03 0.31105897795514E+03 0.32688274304369E+03
 0.29315000000012E+03 0.29315000000014E+03 0.30862686806133E+03 0.32684422993413E+03 0.29315000000012E+03
 0.29315000000014E+03 0.31105897795514E+03 0.32688274304369E+03 0.29315000000012E+03 0.29315000000014E+03
 0.30862686806133E+03 0.32684422993413E+03 0.29315000000012E+03 0.29315000000014E+03 0.36319194045537E+03
 0.29658120723539E+03 0.17128996228132E+04 0.16561742233842E+04 0.65404004673336E+03 0.13197411020940E+04
 0.66243085512695E+03 0.99185224112847E+03 0.96565977589618E+03 0.94781115570041E+03 0.17385690613206E+04
 0.87046904018136E+03 0.96446867449980E+03 0.83924235787860E+03 0.17377476624906E+04 0.99185224112846E+03
 0.96565977589618E+03 0.94781115570040E+03 0.17385690613206E+04 0.87046904018136E+03 0.96446867449980E+03
 0.83924235787860E+03 0.17377476624906E+04 0.17407407255380E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46734311603198E+03 0.12948585200018E+01
 0.12948585200018E+01 0.86010267660993E+00 0.18821451252381E+00 0.29426410931571E+03 0.32696841106764E+03
 0.32286479492652E+03 0.32247143389882E+03 0.22999999722127E+00 0.00000000000000E+00 0.21086044372482E+00
 0.00000000000000E+00 -.13232122799468E+02 0.99579876945838E-03 0.35677386371106E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22423167203972E+02 0.84086877014893E+01 0.29657928980541E+03 0.36322660065431E+03
 0.29394709210452E+03 0.29799682707369E+03 0.29315000000010E+03 0.29315000000010E+03 0.29393779049103E+03
 0.29799788016441E+03 0.29315000000010E+03 0.29315000000010E+03 0.29394709210452E+03 0.29799682707369E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29393779049103E+03 0.29799788016441E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29723960154422E+03 0.29315000000023E+03 0.80391770606710E+02 0.74807715622823E+02
 0.83474678208754E+02 0.34083954344997E+03 0.25694749185017E+03 0.61533677229819E+02 0.79341122560369E+02
 0.62021404111852E+02 0.27207323882766E+03 0.60913914763834E+02 0.79377173502030E+02 0.61419618960581E+02
 0.27210082660200E+03 0.61533677229820E+02 0.79341122560369E+02 0.62021404111853E+02 0.27207323882766E+03
 0.60913914763834E+02 0.79377173502030E+02 0.61419618960581E+02 0.27210082660200E+03 0.17346418705406E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34789676676364E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18066802323848E+00 0.00000000000000E+00 0.00000000000000E+00 0.18066802323848E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18771761191958E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18771761191958E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951243110356E+00 0.19325190053769E+00 0.32696841106764E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    270.01107528
 0.99491507022977E-01 0.30116432231029E+03 0.42776154093181E+03 0.42356309157662E+03 0.42188022424116E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18341505567167E+00 0.00000000000000E+00 -.20936467558422E+02
 0.35730674652668E-02 0.68975287042957E+00 0.22389725572681E+04 0.83961470897554E+03 0.11598356952132E+02
 0.43493838570496E+01 0.31494779346851E+03 0.29315000000225E+03 0.31163605349178E+03 0.32784450162889E+03
 0.29315000000015E+03 0.29315000000019E+03 0.30915851029635E+03 0.32780666729399E+03 0.29315000000014E+03
 0.29315000000019E+03 0.31163605349178E+03 0.32784450162889E+03 0.29315000000015E+03 0.29315000000019E+03
 0.30915851029635E+03 0.32780666729399E+03 0.29315000000014E+03 0.29315000000019E+03 0.36459173389820E+03
 0.29696207796380E+03 0.17225885569189E+04 0.16637099875221E+04 0.65311368528100E+03 0.13046932998886E+04
 0.64831404618118E+03 0.99811520838913E+03 0.97181453453124E+03 0.95267640056897E+03 0.17379936725165E+04
 0.87702021944279E+03 0.97068747502064E+03 0.84470197589319E+03 0.17372289353983E+04 0.99811520838912E+03
 0.97181453453124E+03 0.95267640056896E+03 0.17379936725165E+04 0.87702021944279E+03 0.97068747502064E+03
 0.84470197589319E+03 0.17372289353983E+04 0.17385496595340E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46757310205374E+03 0.12948587482058E+01
 0.12948587482058E+01 0.90005962344119E+00 0.17082719026381E+00 0.29452839465088E+03 0.32695388420901E+03
 0.32326111402624E+03 0.32290532663792E+03 0.22999999736155E+00 0.00000000000000E+00 0.20971317312627E+00
 0.00000000000000E+00 -.13262929413413E+02 0.10531415563212E-02 0.37995492238276E+00 0.75963197463646E+04
 0.28486199048867E+04 0.21055129250151E+02 0.78956734688065E+01 0.29696009277260E+03 0.36462555533877E+03
 0.29401926809434E+03 0.29817851337139E+03 0.29315000000010E+03 0.29315000000011E+03 0.29400957626861E+03
 0.29817951538461E+03 0.29315000000010E+03 0.29315000000011E+03 0.29401926809434E+03 0.29817851337139E+03
 0.29315000000010E+03 0.29315000000011E+03 0.29400957626861E+03 0.29817951538461E+03 0.29315000000010E+03
 0.29315000000011E+03 0.29739655257696E+03 0.29315000000033E+03 0.83375675887615E+02 0.77417970332792E+02
 0.87697464946287E+02 0.34345230846104E+03 0.25531635619002E+03 0.64837784543322E+02 0.83295665699837E+02
 0.65741328921263E+02 0.27412558289833E+03 0.64225898314718E+02 0.83323327634818E+02 0.65151054198679E+02
 0.27414521703844E+03 0.64837784543322E+02 0.83295665699837E+02 0.65741328921263E+02 0.27412558289833E+03
 0.64225898314718E+02 0.83323327634818E+02 0.65151054198680E+02 0.27414521703844E+03 0.17612102343502E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34793665476068E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18082515210583E+00 0.00000000000000E+00 0.00000000000000E+00 0.18082515210583E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18820762505816E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18820762505816E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951220662481E+00 0.19326018643822E+00 0.32695388420901E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    280.01629807
 0.10023168665277E+00 0.30146063322769E+03 0.42805212596244E+03 0.42382263301821E+03 0.42212964665572E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18197218589608E+00 0.00000000000000E+00 -.20961526976468E+02
 0.35466812576919E-02 0.71065902591890E+00 0.22556298180588E+04 0.84586118177205E+03 0.11257156678839E+02
 0.42214337545645E+01 0.31561987261688E+03 0.29315000000371E+03 0.31219898893965E+03 0.32877493864458E+03
 0.29315000000020E+03 0.29315000000027E+03 0.30967742108859E+03 0.32873776315726E+03 0.29315000000017E+03
 0.29315000000027E+03 0.31219898893965E+03 0.32877493864459E+03 0.29315000000020E+03 0.29315000000027E+03
 0.30967742108859E+03 0.32873776315726E+03 0.29315000000017E+03 0.29315000000027E+03 0.36592662937383E+03
 0.29736283235788E+03 0.17317075177701E+04 0.16707341913059E+04 0.65207171144378E+03 0.12905195163705E+04
 0.63518744636953E+03 0.10040184578308E+04 0.97749350204896E+03 0.95720231182917E+03 0.17373272246687E+04
 0.88321370281762E+03 0.97642551428650E+03 0.84982098699859E+03 0.17366144349870E+04 0.10040184578308E+04
 0.97749350204896E+03 0.95720231182916E+03 0.17373272246687E+04 0.88321370281762E+03 0.97642551428650E+03
 0.84982098699859E+03 0.17366144349870E+04 0.17364321825820E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46782569772882E+03 0.12948589328474E+01
 0.12948589328474E+01 0.94008051458618E+00 0.15433176247589E+00 0.29483541865679E+03 0.32692488937297E+03
 0.32362327299127E+03 0.32330447176802E+03 0.22999999767951E+00 0.00000000000000E+00 0.20855869317217E+00
 0.00000000000000E+00 -.13284739935538E+02 0.11657041789683E-02 0.40321676630051E+00 0.68628045985735E+04
 0.25735517244651E+04 0.19840444814335E+02 0.74401668053757E+01 0.29736078289669E+03 0.36595948249169E+03
 0.29409377826390E+03 0.29835476458566E+03 0.29315000000010E+03 0.29315000000011E+03 0.29408376038271E+03
 0.29835570570014E+03 0.29315000000010E+03 0.29315000000011E+03 0.29409377826390E+03 0.29835476458566E+03
 0.29315000000010E+03 0.29315000000011E+03 0.29408376038271E+03 0.29835570570014E+03 0.29315000000010E+03
 0.29315000000011E+03 0.29754876539359E+03 0.29315000000050E+03 0.86025967468688E+02 0.79765257107231E+02
 0.91695372350752E+02 0.34574401024466E+03 0.25359016103215E+03 0.68059109416096E+02 0.87026861825866E+02
 0.69542167493059E+02 0.27591578768554E+03 0.67460167736806E+02 0.87045875004234E+02 0.68968393261285E+02
 0.27592728401841E+03 0.68059109416096E+02 0.87026861825866E+02 0.69542167493059E+02 0.27591578768554E+03
 0.67460167736806E+02 0.87045875004234E+02 0.68968393261285E+02 0.27592728401841E+03 0.17852111305647E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34797442838073E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18092177665290E+00 0.00000000000000E+00 0.00000000000000E+00 0.18092177665290E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18863386434589E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18863386434589E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951225229044E+00 0.19327733414958E+00 0.32692488937297E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    290.00000669
 0.10076799817401E+00 0.30175073791834E+03 0.42835380160877E+03 0.42410128917851E+03 0.42240098946114E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18058185724061E+00 0.00000000000000E+00 -.20984840465940E+02
 0.35278047129396E-02 0.73074004392606E+00 0.22676992211777E+04 0.85038720794162E+03 0.10947805675214E+02
 0.41054271282053E+01 0.31627336089144E+03 0.29315000000600E+03 0.31274716927416E+03 0.32967454181654E+03
 0.29315000000027E+03 0.29315000000040E+03 0.31018304503808E+03 0.32963800231887E+03 0.29315000000023E+03
 0.29315000000039E+03 0.31274716927416E+03 0.32967454181654E+03 0.29315000000027E+03 0.29315000000040E+03
 0.31018304503808E+03 0.32963800231887E+03 0.29315000000023E+03 0.29315000000039E+03 0.36720039238200E+03
 0.29778188915513E+03 0.17403323610775E+04 0.16773192606932E+04 0.65098240398848E+03 0.12772498487280E+04
 0.62301253271955E+03 0.10096099987275E+04 0.98279117776231E+03 0.96143831962321E+03 0.17366774956582E+04
 0.88909475596039E+03 0.98177753433049E+03 0.85464447264760E+03 0.17360122085170E+04 0.10096099987275E+04
 0.98279117776231E+03 0.96143831962320E+03 0.17366774956582E+04 0.88909475596039E+03 0.98177753433049E+03
 0.85464447264760E+03 0.17360122085170E+04 0.17344640057652E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46809911162833E+03 0.12948591046247E+01
 0.12948591046247E+01 0.98001534908641E+00 0.13878609336132E+00 0.29518674081558E+03 0.32688885397126E+03
 0.32395564568048E+03 0.32367258783574E+03 0.22999999804892E+00 0.00000000000000E+00 0.20740241817161E+00
 0.00000000000000E+00 -.13302033444599E+02 0.12962764889627E-02 0.42645334566660E+00 0.61715228719467E+04
 0.23143210769800E+04 0.18759379147313E+02 0.70347671802425E+01 0.29777977993784E+03 0.36723216813296E+03
 0.29417059916104E+03 0.29852546235305E+03 0.29315000000010E+03 0.29315000000012E+03 0.29416032114554E+03
 0.29852633380422E+03 0.29315000000010E+03 0.29315000000012E+03 0.29417059916104E+03 0.29852546235305E+03
 0.29315000000010E+03 0.29315000000012E+03 0.29416032114554E+03 0.29852633380422E+03 0.29315000000010E+03
 0.29315000000012E+03 0.29769615588649E+03 0.29315000000077E+03 0.88346606160130E+02 0.81866865835632E+02
 0.95470561472092E+02 0.34779154374885E+03 0.25184362946940E+03 0.71194804611681E+02 0.90538905484830E+02
 0.73451490065688E+02 0.27750635957249E+03 0.70613357494824E+02 0.90549140058521E+02 0.72898705923529E+02
 0.27750965439767E+03 0.71194804611681E+02 0.90538905484830E+02 0.73451490065688E+02 0.27750635957249E+03
 0.70613357494824E+02 0.90549140058521E+02 0.72898705923529E+02 0.27750965439767E+03 0.18070015183480E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34801605566241E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18099377330755E+00 0.00000000000000E+00 0.00000000000000E+00 0.18099377330755E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18900353467568E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18900353467568E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951241206706E+00 0.19329877959516E+00 0.32688885397126E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    300.00632012
 0.10116099717871E+00 0.30203583727984E+03 0.42866873694879E+03 0.42439863348008E+03 0.42269281674267E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17923527960848E+00 0.00000000000000E+00 -.21008833372726E+02
 0.35140993110510E-02 0.75012235722069E+00 0.22765435156719E+04 0.85370381837695E+03 0.10664926758937E+02
 0.39993475346014E+01 0.31691281854484E+03 0.29315000000954E+03 0.31328431052584E+03 0.33055061787113E+03
 0.29315000000039E+03 0.29315000000061E+03 0.31067881456673E+03 0.33051469427591E+03 0.29315000000032E+03
 0.29315000000061E+03 0.31328431052584E+03 0.33055061787113E+03 0.29315000000039E+03 0.29315000000061E+03
 0.31067881456673E+03 0.33051469427591E+03 0.29315000000032E+03 0.29315000000061E+03 0.36842574229802E+03
 0.29822095524439E+03 0.17485736803651E+04 0.16835583516275E+04 0.64987589552770E+03 0.12647701315796E+04
 0.61164485657427E+03 0.10149622173776E+04 0.98780670431215E+03 0.96544860246464E+03 0.17360929161309E+04
 0.89473582474177E+03 0.98684333676511E+03 0.85923759914403E+03 0.17354713106688E+04 0.10149622173776E+04
 0.98780670431215E+03 0.96544860246464E+03 0.17360929161309E+04 0.89473582474177E+03 0.98684333676511E+03
 0.85923759914403E+03 0.17354713106688E+04 0.17326590485869E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46839230718389E+03 0.12948592814082E+01
 0.12948592814082E+01 0.10200406027964E+01 0.12411559379227E+00 0.29558614834516E+03 0.32685136918120E+03
 0.32426436821513E+03 0.32401555110104E+03 0.22999999849335E+00 0.00000000000000E+00 0.20624006241853E+00
 0.00000000000000E+00 -.13317566973105E+02 0.14494965166198E-02 0.44975252482392E+00 0.55191577960158E+04
 0.20696841735059E+04 0.17787559954515E+02 0.66703349829432E+01 0.29821879154028E+03 0.36845634667511E+03
 0.29425040323531E+03 0.29869190388874E+03 0.29315000000010E+03 0.29315000000014E+03 0.29423992992216E+03
 0.29869269746765E+03 0.29315000000010E+03 0.29315000000014E+03 0.29425040323531E+03 0.29869190388874E+03
 0.29315000000010E+03 0.29315000000014E+03 0.29423992992216E+03 0.29869269746765E+03 0.29315000000010E+03
 0.29315000000014E+03 0.29783985599575E+03 0.29315000000119E+03 0.90361999536500E+02 0.83756371668556E+02
 0.99057236281061E+02 0.34967385680742E+03 0.25012133434495E+03 0.74266792195338E+02 0.93865752721574E+02
 0.77531068022463E+02 0.27896117411253E+03 0.73707013875475E+02 0.93867132693532E+02 0.77003403885286E+02
 0.27895625259743E+03 0.74266792195338E+02 0.93865752721574E+02 0.77531068022463E+02 0.27896117411253E+03
 0.73707013875475E+02 0.93867132693532E+02 0.77003403885287E+02 0.27895625259743E+03 0.18270570260030E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34806627615445E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18106208727613E+00 0.00000000000000E+00 0.00000000000000E+00 0.18106208727613E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18932673449509E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18932673449509E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951259156748E+00 0.19332111162836E+00 0.32685136918120E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    310.01705180
 0.10145411209901E+00 0.30231547091402E+03 0.42899588299628E+03 0.42471180008690E+03 0.42300156921591E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17793281678735E+00 0.00000000000000E+00 -.21034433286997E+02
 0.35039462925168E-02 0.76880222443254E+00 0.22831400176096E+04 0.85617750660361E+03 0.10405797155315E+02
 0.39021739332431E+01 0.31753843150473E+03 0.29315000001486E+03 0.31381048643570E+03 0.33140424691910E+03
 0.29315000000059E+03 0.29315000000096E+03 0.31116479647135E+03 0.33136891807768E+03 0.29315000000047E+03
 0.29315000000096E+03 0.31381048643570E+03 0.33140424691910E+03 0.29315000000059E+03 0.29315000000096E+03
 0.31116479647135E+03 0.33136891807768E+03 0.29315000000047E+03 0.29315000000096E+03 0.36960604751419E+03
 0.29867907484921E+03 0.17564636282177E+04 0.16894811089582E+04 0.64877030543521E+03 0.12530346135286E+04
 0.60102045656622E+03 0.10200973213026E+04 0.99257891871474E+03 0.96925651094633E+03 0.17355928588684E+04
 0.90015773720242E+03 0.99166203054984E+03 0.86362151221501E+03 0.17350113925896E+04 0.10200973213026E+04
 0.99257891871474E+03 0.96925651094633E+03 0.17355928588684E+04 0.90015773720242E+03 0.99166203054984E+03
 0.86362151221501E+03 0.17350113925896E+04 0.17310176930328E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46870200683726E+03 0.12948594700324E+01
 0.12948594700324E+01 0.10600835294987E+01 0.11034256156106E+00 0.29603465244049E+03 0.32681690076754E+03
 0.32455250601353E+03 0.32433602634780E+03 0.22999999903829E+00 0.00000000000000E+00 0.20507433357192E+00
 0.00000000000000E+00 -.13332633737657E+02 0.16304233927694E-02 0.47306080130180E+00 0.49067009437415E+04
 0.18400128539031E+04 0.16911145412989E+02 0.63416795298710E+01 0.29867686315482E+03 0.36963541152568E+03
 0.29433345270830E+03 0.29885429008258E+03 0.29315000000010E+03 0.29315000000018E+03 0.29432284898004E+03
 0.29885499858430E+03 0.29315000000010E+03 0.29315000000018E+03 0.29433345270830E+03 0.29885429008258E+03
 0.29315000000010E+03 0.29315000000018E+03 0.29432284898004E+03 0.29885499858430E+03 0.29315000000010E+03
 0.29315000000018E+03 0.29798004537301E+03 0.29315000000184E+03 0.92082112238535E+02 0.85451520676900E+02
 0.10246549141906E+03 0.35144317783201E+03 0.24846535895585E+03 0.77276601302639E+02 0.97018709070804E+02
 0.81821453125874E+02 0.28032301296372E+03 0.76742200433950E+02 0.97011256344898E+02 0.81322556955306E+02
 0.28030994971814E+03 0.77276601302639E+02 0.97018709070804E+02 0.81821453125874E+02 0.28032301296372E+03
 0.76742200433950E+02 0.97011256344898E+02 0.81322556955306E+02 0.28030994971814E+03 0.18456514118056E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34812829322231E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18113612929435E+00 0.00000000000000E+00 0.00000000000000E+00 0.18113612929435E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18961026160298E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18961026160298E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951273970336E+00 0.19334163141964E+00 0.32681690076754E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    320.01280935
 0.10167978248257E+00 0.30258931727170E+03 0.42933337138856E+03 0.42503760210404E+03 0.42332357426766E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17667496961168E+00 0.00000000000000E+00 -.21061875873439E+02
 0.34961692396164E-02 0.78677569541008E+00 0.22882187479224E+04 0.85808203047091E+03 0.10168082271314E+02
 0.38130308517428E+01 0.31815020529245E+03 0.29315000002271E+03 0.31432563287861E+03 0.33223611373519E+03
 0.29315000000092E+03 0.29315000000152E+03 0.31164092422676E+03 0.33220135771295E+03 0.29315000000072E+03
 0.29315000000152E+03 0.31432563287861E+03 0.33223611373519E+03 0.29315000000092E+03 0.29315000000152E+03
 0.31164092422676E+03 0.33220135771295E+03 0.29315000000072E+03 0.29315000000152E+03 0.37074382746117E+03
 0.29915499920552E+03 0.17640217367095E+04 0.16951062666666E+04 0.64767339593619E+03 0.12419898777599E+04
 0.59107811484408E+03 0.10250292228133E+04 0.99713106965521E+03 0.97287777330582E+03 0.17351775807678E+04
 0.90537330292454E+03 0.99625712259113E+03 0.86781011710803E+03 0.17346329730499E+04 0.10250292228133E+04
 0.99713106965521E+03 0.97287777330582E+03 0.17351775807678E+04 0.90537330292454E+03 0.99625712259113E+03
 0.86781011710803E+03 0.17346329730499E+04 0.17295265910822E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46902473646059E+03 0.12948596722338E+01
 0.12948596722338E+01 0.11000665596931E+01 0.97482539070245E-01 0.29653201228120E+03 0.32678878124086E+03
 0.32482244346603E+03 0.32463608215618E+03 0.22999999972495E+00 0.00000000000000E+00 0.20390793964307E+00
 0.00000000000000E+00 -.13344852421735E+02 0.18455106906648E-02 0.49632470768673E+00 0.43348434882911E+04
 0.16255663081092E+04 0.16118480253152E+02 0.60444300949320E+01 0.29915274695815E+03 0.37077190688799E+03
 0.29442004181829E+03 0.29901280309531E+03 0.29315000000010E+03 0.29315000000025E+03 0.29440937157313E+03
 0.29901342038429E+03 0.29315000000010E+03 0.29315000000025E+03 0.29442004181829E+03 0.29901280309531E+03
 0.29315000000010E+03 0.29315000000025E+03 0.29440937157313E+03 0.29901342038429E+03 0.29315000000010E+03
 0.29315000000025E+03 0.29811688425165E+03 0.29315000000282E+03 0.93518944480511E+02 0.86967711847863E+02
 0.10570553928362E+03 0.35314062541566E+03 0.24690655843562E+03 0.80224786150858E+02 0.10000903630438E+03
 0.86363654468771E+02 0.28162575849012E+03 0.79718986893145E+02 0.99992861415445E+02 0.85896670901826E+02
 0.28160470802392E+03 0.80224786150858E+02 0.10000903630438E+03 0.86363654468771E+02 0.28162575849012E+03
 0.79718986893145E+02 0.99992861415445E+02 0.85896670901826E+02 0.28160470802392E+03 0.18630079564222E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34820420285343E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18121958130878E+00 0.00000000000000E+00 0.00000000000000E+00 0.18121958130878E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18986058279507E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18986058279507E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951291915974E+00 0.19335843805307E+00 0.32678878124086E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    330.01002264
 0.10186186812608E+00 0.30285812541708E+03 0.42968024714740E+03 0.42537413440101E+03 0.42365659152858E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17545788778090E+00 0.00000000000000E+00 -.21090939459732E+02
 0.34899192500428E-02 0.80410175472175E+00 0.22923166488456E+04 0.85961874331712E+03 0.99489896061331E+01
 0.37308711022999E+01 0.31875019290546E+03 0.29315000003410E+03 0.31483142221572E+03 0.33304954496140E+03
 0.29315000000142E+03 0.29315000000239E+03 0.31210873373627E+03 0.33301534128784E+03 0.29315000000111E+03
 0.29315000000238E+03 0.31483142221572E+03 0.33304954496140E+03 0.29315000000142E+03 0.29315000000239E+03
 0.31210873373627E+03 0.33301534128784E+03 0.29315000000111E+03 0.29315000000238E+03 0.37184493691025E+03
 0.29964903205638E+03 0.17712868256148E+04 0.17004661356267E+04 0.64658351754979E+03 0.12315447821078E+04
 0.58172834697027E+03 0.10297840900626E+04 0.10014927608943E+04 0.97633599755484E+03 0.17348364448883E+04
 0.91040883469927E+03 0.10006586016884E+04 0.87182756687075E+03 0.17343257803789E+04 0.10297840900626E+04
 0.10014927608943E+04 0.97633599755484E+03 0.17348364448883E+04 0.91040883469927E+03 0.10006586016884E+04
 0.87182756687075E+03 0.17343257803789E+04 0.17281620006595E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46935839000786E+03 0.12948598863791E+01
 0.12948598863791E+01 0.11400554128598E+01 0.85501461792111E-01 0.29707867383166E+03 0.32676930346603E+03
 0.32507690864252E+03 0.32491831206931E+03 0.23000000000000E+00 0.00000000000000E+00 0.20273929496843E+00
 0.00000000000000E+00 -.13345929360033E+02 0.21041165878565E-02 0.51957637477657E+00 0.38020706866580E+04
 0.14257765074967E+04 0.15397158893994E+02 0.57739345852476E+01 0.29964674723542E+03 0.37187170546238E+03
 0.29451083074122E+03 0.29916816382223E+03 0.29315000000012E+03 0.29315000000035E+03 0.29450015601359E+03
 0.29916868450214E+03 0.29315000000012E+03 0.29315000000035E+03 0.29451083074122E+03 0.29916816382223E+03
 0.29315000000012E+03 0.29315000000035E+03 0.29450015601359E+03 0.29916868450214E+03 0.29315000000012E+03
 0.29315000000035E+03 0.29825099578278E+03 0.29315000000426E+03 0.94689913163801E+02 0.88321601993657E+02
 0.10879829990861E+03 0.35480327463457E+03 0.24546098322642E+03 0.83121217645575E+02 0.10285766511231E+03
 0.91216832435802E+02 0.28289977142116E+03 0.82646865645350E+02 0.10283292604321E+03 0.90784522777009E+02
 0.28287092981890E+03 0.83121217645575E+02 0.10285766511231E+03 0.91216832435802E+02 0.28289977142116E+03
 0.82646865645350E+02 0.10283292604321E+03 0.90784522777009E+02 0.28287092981889E+03 0.18793584675939E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34829546869114E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18131166325836E+00 0.00000000000000E+00 0.00000000000000E+00 0.18131166325836E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19008309537214E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19008309537214E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951337505900E+00 0.19337045262636E+00 0.32676930346603E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    340.11204921
 0.10201754020331E+00 0.30312490371074E+03 0.43003868820870E+03 0.42572287750457E+03 0.42400186391465E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17426813311789E+00 0.00000000000000E+00 -.21122079828218E+02
 0.34845935222510E-02 0.82097483066573E+00 0.22958201434158E+04 0.86093255378093E+03 0.97445131095100E+01
 0.36541924160663E+01 0.31934530481535E+03 0.29315000005056E+03 0.31533364216459E+03 0.33385429905526E+03
 0.29315000000218E+03 0.29315000000370E+03 0.31257357283821E+03 0.33382063324947E+03 0.29315000000170E+03
 0.29315000000368E+03 0.31533364216459E+03 0.33385429905526E+03 0.29315000000218E+03 0.29315000000370E+03
 0.31257357283821E+03 0.33382063324947E+03 0.29315000000170E+03 0.29315000000368E+03 0.37292353028147E+03
 0.30016599918464E+03 0.17783506075234E+04 0.17056306472265E+04 0.64548662329891E+03 0.12215334052417E+04
 0.57281934882635E+03 0.10344225686184E+04 0.10057216220120E+04 0.97967828018664E+03 0.17345531344368E+04
 0.91532767381480E+03 0.10049247387310E+04 0.87572647011761E+03 0.17340740723382E+04 0.10344225686184E+04
 0.10057216220120E+04 0.97967828018664E+03 0.17345531344368E+04 0.91532767381480E+03 0.10049247387310E+04
 0.87572647011761E+03 0.17340740723382E+04 0.17268897003743E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46970429834081E+03 0.12948601158265E+01
 0.12948601158265E+01 0.11804635191343E+01 0.74277491516387E-01 0.29767962296390E+03 0.32676000054026E+03
 0.32531998887444E+03 0.32518692355625E+03 0.23000000000000E+00 0.00000000000000E+00 0.20155653242409E+00
 0.00000000000000E+00 -.13348563314965E+02 0.24220664669133E-02 0.54305126870146E+00 0.33029646829615E+04
 0.12386117561105E+04 0.14731574090840E+02 0.55243402840649E+01 0.30016369004016E+03 0.37294896522271E+03
 0.29460741969146E+03 0.29932234957936E+03 0.29315000000013E+03 0.29315000000051E+03 0.29459680135866E+03
 0.29932276803590E+03 0.29315000000013E+03 0.29315000000051E+03 0.29460741969146E+03 0.29932234957936E+03
 0.29315000000013E+03 0.29315000000051E+03 0.29459680135866E+03 0.29932276803590E+03 0.29315000000013E+03
 0.29315000000051E+03 0.29838408255712E+03 0.29315000000637E+03 0.95616771324341E+02 0.89532972101132E+02
 0.11178705976888E+03 0.35647311014902E+03 0.24412711508130E+03 0.85997987114363E+02 0.10560581876473E+03
 0.96492249922555E+02 0.28417874162037E+03 0.85557872805988E+02 0.10557264560221E+03 0.96097346710625E+02
 0.28414227890842E+03 0.85997987114363E+02 0.10560581876473E+03 0.96492249922555E+02 0.28417874162037E+03
 0.85557872805988E+02 0.10557264560221E+03 0.96097346710625E+02 0.28414227890842E+03 0.18950040041685E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34840399014613E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18141563209206E+00 0.00000000000000E+00 0.00000000000000E+00 0.18141563209206E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19028411350895E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19028411350895E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951372602055E+00 0.19337633095835E+00 0.32676000054026E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    350.07849753
 0.10215352137909E+00 0.30338334162034E+03 0.43039851498948E+03 0.42607349924673E+03 0.42434901939262E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17313245736722E+00 0.00000000000000E+00 -.21145559598993E+02
 0.34799546809126E-02 0.83702044314835E+00 0.22988805124043E+04 0.86208019215161E+03 0.95577116012949E+01
 0.35841418504856E+01 0.31992198748518E+03 0.29315000007349E+03 0.31582079765222E+03 0.33463259147491E+03
 0.29315000000326E+03 0.29315000000562E+03 0.31302475789131E+03 0.33459943577118E+03 0.29315000000253E+03
 0.29315000000559E+03 0.31582079765222E+03 0.33463259147491E+03 0.29315000000326E+03 0.29315000000562E+03
 0.31302475789131E+03 0.33459943577118E+03 0.29315000000253E+03 0.29315000000559E+03 0.37395756424739E+03
 0.30069286693071E+03 0.17850639881951E+04 0.17104933416888E+04 0.64439586462920E+03 0.12121132397418E+04
 0.56449539578949E+03 0.10388465071858E+04 0.10097281824689E+04 0.98283630764418E+03 0.17343177794146E+04
 0.92002505101257E+03 0.10089654436090E+04 0.87942556233714E+03 0.17338674948298E+04 0.10388465071858E+04
 0.10097281824689E+04 0.98283630764418E+03 0.17343177794146E+04 0.92002505101257E+03 0.10089654436090E+04
 0.87942556233714E+03 0.17338674948298E+04 0.17257134612832E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47005212388926E+03 0.12948602888295E+01
 0.12948602888295E+01 0.12203293124414E+01 0.64061350237269E-01 0.29832459724840E+03 0.32676191089706E+03
 0.32554742242409E+03 0.32543704682053E+03 0.23000000000000E+00 0.00000000000000E+00 0.20038804181717E+00
 0.00000000000000E+00 -.13343012068567E+02 0.28083235842055E-02 0.56618684917441E+00 0.28486745775997E+04
 0.10682529665999E+04 0.14129611119837E+02 0.52986041699387E+01 0.30069054314907E+03 0.37398169340012E+03
 0.29469317643671E+03 0.29947201044370E+03 0.29315000000013E+03 0.29315000000072E+03 0.29468261598525E+03
 0.29947232461899E+03 0.29315000000013E+03 0.29315000000072E+03 0.29469317643671E+03 0.29947201044370E+03
 0.29315000000013E+03 0.29315000000072E+03 0.29468261598525E+03 0.29947232461899E+03 0.29315000000013E+03
 0.29315000000072E+03 0.29851324085338E+03 0.29315000000937E+03 0.96288970353166E+02 0.90596007014598E+02
 0.11461306640190E+03 0.35812486479055E+03 0.24293873305664E+03 0.88865485991100E+02 0.10820064965587E+03
 0.88865485991100E+02 0.28544353640504E+03 0.88461008815709E+02 0.10815941495091E+03 0.88461008815709E+02
 0.28539983712719E+03 0.88865485991100E+02 0.10820064965587E+03 0.88865485991100E+02 0.28544353640504E+03
 0.88461008815709E+02 0.10815941495091E+03 0.88461008815709E+02 0.28539983712719E+03 0.19080036622056E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34852564253646E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18145554429590E+00 0.00000000000000E+00 0.00000000000000E+00 0.18145554429590E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19042294319003E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19042294319003E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951425429504E+00 0.19337578008284E+00 0.32676191089706E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    360.00488617
 0.10226302341600E+00 0.30363840539327E+03 0.43076239255569E+03 0.42642903146371E+03 0.42470128827626E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17203769277753E+00 0.00000000000000E+00 -.21178923707054E+02
 0.34762280401806E-02 0.85242893535961E+00 0.23013449944971E+04 0.86300437293641E+03 0.93849465546651E+01
 0.35193549579994E+01 0.32048723809575E+03 0.29315000010502E+03 0.31629878558222E+03 0.33539350877085E+03
 0.29315000000483E+03 0.29315000000838E+03 0.31346782137599E+03 0.33536084472703E+03 0.29315000000374E+03
 0.29315000000833E+03 0.31629878558222E+03 0.33539350877085E+03 0.29315000000483E+03 0.29315000000838E+03
 0.31346782137599E+03 0.33536084472703E+03 0.29315000000374E+03 0.29315000000833E+03 0.37495789221044E+03
 0.30123540116683E+03 0.17915272315171E+04 0.17151442436005E+04 0.64333102781021E+03 0.12031997997794E+04
 0.55665211683010E+03 0.10431186853972E+04 0.10135732872328E+04 0.98586758948368E+03 0.17341271389330E+04
 0.92456658551273E+03 0.10128421799977E+04 0.88298749796025E+03 0.17337033281670E+04 0.10431186853972E+04
 0.10135732872328E+04 0.98586758948368E+03 0.17341271389330E+04 0.92456658551273E+03 0.10128421799977E+04
 0.88298749796024E+03 0.17337033281670E+04 0.17246350832988E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47040499574192E+03 0.12948605346620E+01
 0.12948605346620E+01 0.12600348670058E+01 0.54734267224852E-01 0.29908339422226E+03 0.32677510441909E+03
 0.32576464744188E+03 0.32567480801875E+03 0.23000000000000E+00 0.00000000000000E+00 0.19922296141623E+00
 0.00000000000000E+00 -.13348793514913E+02 0.32868802391546E-02 0.58920028796178E+00 0.24339189194364E+04
 0.91271959478864E+03 0.13577725882780E+02 0.50916472060425E+01 0.30123306496687E+03 0.37498072900804E+03
 0.29474153596278E+03 0.29961906594035E+03 0.29315000000014E+03 0.29315000000103E+03 0.29473095294998E+03
 0.29961927287066E+03 0.29315000000014E+03 0.29315000000103E+03 0.29474153596278E+03 0.29961906594035E+03
 0.29315000000014E+03 0.29315000000103E+03 0.29473095294998E+03 0.29961927287066E+03 0.29315000000014E+03
 0.29315000000103E+03 0.29864017403741E+03 0.29315000001354E+03 0.96716635760228E+02 0.91714970877426E+02
 0.11731635886097E+03 0.35979116861430E+03 0.24188822795903E+03 0.91899952343749E+02 0.11067968619325E+03
 0.91899952343749E+02 0.28672139373268E+03 0.91532078673721E+02 0.11063069219136E+03 0.91532078673721E+02
 0.28667077956121E+03 0.91899952343749E+02 0.11067968619325E+03 0.91899952343749E+02 0.28672139373268E+03
 0.91532078673721E+02 0.11063069219136E+03 0.91532078673721E+02 0.28667077956121E+03 0.19223774693313E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34866710783446E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18157516642960E+00 0.00000000000000E+00 0.00000000000000E+00 0.18157516642960E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19059126022277E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19059126022277E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951438149400E+00 0.19336809852593E+00 0.32677510441909E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    370.01698582
 0.10237393833660E+00 0.30389089977425E+03 0.43113360595788E+03 0.42679149363901E+03 0.42506017946538E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17096893690505E+00 0.00000000000000E+00 -.21212189681131E+02
 0.34724614415173E-02 0.86741414679457E+00 0.23038412764936E+04 0.86394047868511E+03 0.92228147645079E+01
 0.34585555366905E+01 0.32104839092289E+03 0.29315000014847E+03 0.31677374742525E+03 0.33614766047589E+03
 0.29315000000709E+03 0.29315000001237E+03 0.31390837392101E+03 0.33611547392402E+03 0.29315000000549E+03
 0.29315000001231E+03 0.31677374742525E+03 0.33614766047589E+03 0.29315000000709E+03 0.29315000001237E+03
 0.31390837392101E+03 0.33611547392402E+03 0.29315000000549E+03 0.29315000001231E+03 0.37594123413285E+03
 0.30179843981128E+03 0.17978234346364E+04 0.17196281068167E+04 0.64224926071907E+03 0.11945824426898E+04
 0.54912193566713E+03 0.10472969330028E+04 0.10173097396211E+04 0.98880285775964E+03 0.17339625564592E+04
 0.92901324084364E+03 0.10166083289657E+04 0.88645014530587E+03 0.17335634438908E+04 0.10472969330028E+04
 0.10173097396211E+04 0.98880285775964E+03 0.17339625564592E+04 0.92901324084364E+03 0.10166083289657E+04
 0.88645014530587E+03 0.17335634438908E+04 0.17236003008077E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47076463565553E+03 0.12948607797715E+01
 0.12948607797715E+01 0.13000832655975E+01 0.46144857256080E-01 0.29987130971714E+03 0.32680099625443E+03
 0.32597255189362E+03 0.32590064153587E+03 0.23000000000000E+00 0.00000000000000E+00 0.19804647250095E+00
 0.00000000000000E+00 -.13354856632604E+02 0.38987001828522E-02 0.61238285561952E+00 0.20519659437232E+04
 0.76948722889620E+03 0.13063723006920E+02 0.48988961275949E+01 0.30179609501175E+03 0.37596279641539E+03
 0.29481128503377E+03 0.29976562278555E+03 0.29315000000016E+03 0.29315000000148E+03 0.29480081053321E+03
 0.29976571932668E+03 0.29315000000016E+03 0.29315000000148E+03 0.29481128503377E+03 0.29976562278555E+03
 0.29315000000016E+03 0.29315000000148E+03 0.29480081053321E+03 0.29976571932668E+03 0.29315000000016E+03
 0.29315000000148E+03 0.29876665689211E+03 0.29315000001938E+03 0.96935315843545E+02 0.92626186203849E+02
 0.11994546476757E+03 0.36151102684057E+03 0.24096583474916E+03 0.94845537832788E+02 0.11308850496054E+03
 0.94845537832788E+02 0.28804320340419E+03 0.94516008955345E+02 0.11303198184998E+03 0.94516008955345E+02
 0.28798592575920E+03 0.94845537832788E+02 0.11308850496054E+03 0.94845537832788E+02 0.28804320340419E+03
 0.94516008955345E+02 0.11303198184998E+03 0.94516008955345E+02 0.28798592575920E+03 0.19361499805498E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34882482870808E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18169124211074E+00 0.00000000000000E+00 0.00000000000000E+00 0.18169124211074E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19074224295325E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19074224295325E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951443566492E+00 0.19335282510235E+00 0.32680099625443E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    380.00024265
 0.10248320527193E+00 0.30413893346874E+03 0.43150688130748E+03 0.42715585612634E+03 0.42542079242919E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16993773309368E+00 0.00000000000000E+00 -.21244376657495E+02
 0.34687587734873E-02 0.88181755755829E+00 0.23063004729952E+04 0.86486267737319E+03 0.90721713708578E+01
 0.34020642640717E+01 0.32159950793365E+03 0.29315000020700E+03 0.31724064176383E+03 0.33688710467887E+03
 0.29315000001024E+03 0.29315000001799E+03 0.31434174702569E+03 0.33685537707486E+03 0.29315000000794E+03
 0.29315000001790E+03 0.31724064176383E+03 0.33688710467887E+03 0.29315000001024E+03 0.29315000001799E+03
 0.31434174702569E+03 0.33685537707486E+03 0.29315000000794E+03 0.29315000001790E+03 0.37689736265385E+03
 0.30237393737304E+03 0.18038928191263E+04 0.17239096046353E+04 0.64116518020180E+03 0.11863355325960E+04
 0.54196452649320E+03 0.10513404898007E+04 0.10209000043579E+04 0.99161912005523E+03 0.17338152255576E+04
 0.93332129192536E+03 0.10202261769524E+04 0.88978444744144E+03 0.17334389017368E+04 0.10513404898007E+04
 0.10209000043579E+04 0.99161912005523E+03 0.17338152255576E+04 0.93332129192536E+03 0.10202261769524E+04
 0.88978444744144E+03 0.17334389017368E+04 0.17226133012438E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47112609339634E+03 0.12948610169310E+01
 0.12948610169310E+01 0.13400162929188E+01 0.38384865218189E-01 0.30067473423181E+03 0.32683886447067E+03
 0.32616932672882E+03 0.32611268190934E+03 0.23000000000000E+00 0.00000000000000E+00 0.19687223979608E+00
 0.00000000000000E+00 -.13360646077045E+02 0.46868719237808E-02 0.63546625132762E+00 0.17068953728837E+04
 0.64008576483138E+03 0.12589181539203E+02 0.47209430772010E+01 0.30237160078887E+03 0.37691769012302E+03
 0.29488509586267E+03 0.29991024649843E+03 0.29315000000017E+03 0.29315000000211E+03 0.29487479200225E+03
 0.29991023136191E+03 0.29315000000017E+03 0.29315000000211E+03 0.29488509586267E+03 0.29991024649843E+03
 0.29315000000017E+03 0.29315000000211E+03 0.29487479200225E+03 0.29991023136191E+03 0.29315000000017E+03
 0.29315000000211E+03 0.29889145431032E+03 0.29315000002734E+03 0.96951553912915E+02 0.93315266425443E+02
 0.12247397190987E+03 0.36326172437292E+03 0.24017538260350E+03 0.97751632028706E+02 0.11540381127743E+03
 0.97751632028706E+02 0.28939184668039E+03 0.97461157336096E+02 0.11534013173105E+03 0.97461157336096E+02
 0.28932828524571E+03 0.97751632028706E+02 0.11540381127743E+03 0.97751632028706E+02 0.28939184668039E+03
 0.97461157336096E+02 0.11534013173106E+03 0.97461157336096E+02 0.28932828524572E+03 0.19491649085882E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34899622663331E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18179784646948E+00 0.00000000000000E+00 0.00000000000000E+00 0.18179784646948E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19087467402245E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19087467402245E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951443352591E+00 0.19333040949502E+00 0.32683886447067E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    390.00042424
 0.10258877789564E+00 0.30438425890289E+03 0.43188315269452E+03 0.42752316746214E+03 0.42578424007143E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16893824992636E+00 0.00000000000000E+00 -.21275324190377E+02
 0.34651887704472E-02 0.89572415417272E+00 0.23086765339389E+04 0.86575370022710E+03 0.89313210576405E+01
 0.33492453966152E+01 0.32214361346680E+03 0.29315000028528E+03 0.31770200501417E+03 0.33761599752783E+03
 0.29315000001463E+03 0.29315000002584E+03 0.31477028640839E+03 0.33758471322885E+03 0.29315000001134E+03
 0.29315000002571E+03 0.31770200501417E+03 0.33761599752783E+03 0.29315000001463E+03 0.29315000002584E+03
 0.31477028640839E+03 0.33758471322885E+03 0.29315000001134E+03 0.29315000002571E+03 0.37783227736852E+03
 0.30296305140554E+03 0.18097795640625E+04 0.17280260884406E+04 0.64007352539105E+03 0.11783903448202E+04
 0.53511645180218E+03 0.10552771635156E+04 0.10243689853670E+04 0.99433984034269E+03 0.17336783608266E+04
 0.93751996065565E+03 0.10237209233943E+04 0.89301657134946E+03 0.17333231798644E+04 0.10552771635156E+04
 0.10243689853670E+04 0.99433984034269E+03 0.17336783608266E+04 0.93751996065565E+03 0.10237209233943E+04
 0.89301657134946E+03 0.17333231798644E+04 0.17216634945735E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47149044875850E+03 0.12948612449580E+01
 0.12948612449580E+01 0.13800170192663E+01 0.31401256665527E-01 0.30149201480333E+03 0.32688821652299E+03
 0.32635656809064E+03 0.32631279933214E+03 0.23000000000000E+00 0.00000000000000E+00 0.19569515656564E+00
 0.00000000000000E+00 -.13365929513015E+02 0.57292271487491E-02 0.65855257888690E+00 0.13963488952863E+04
 0.52363083573235E+03 0.12147853119825E+02 0.45554449199343E+01 0.30296073741194E+03 0.37785140564277E+03
 0.29496150179249E+03 0.30005380006203E+03 0.29315000000019E+03 0.29315000000299E+03 0.29495141969864E+03
 0.30005367213485E+03 0.29315000000019E+03 0.29315000000299E+03 0.29496150179249E+03 0.30005380006203E+03
 0.29315000000019E+03 0.29315000000299E+03 0.29495141969864E+03 0.30005367213485E+03 0.29315000000019E+03
 0.29315000000299E+03 0.29901531031365E+03 0.29315000003814E+03 0.96776417224232E+02 0.93782196883983E+02
 0.12491826461045E+03 0.36504752032704E+03 0.23950466439354E+03 0.10063942265029E+03 0.11764116320019E+03
 0.10063942265029E+03 0.29077068147955E+03 0.10038841412983E+03 0.11757069446837E+03 0.10038841412983E+03
 0.29070120951359E+03 0.10063942265029E+03 0.11764116320018E+03 0.10063942265029E+03 0.29077068147953E+03
 0.10038841412983E+03 0.11757069446838E+03 0.10038841412983E+03 0.29070120951361E+03 0.19614747180879E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34918109260837E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18189317769819E+00 0.00000000000000E+00 0.00000000000000E+00 0.18189317769819E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19098939476562E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19098939476562E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951438671890E+00 0.19330116088898E+00 0.32688821652299E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    400.01255749
 0.10268844330972E+00 0.30462707143375E+03 0.43226164801113E+03 0.42789278268406E+03 0.42614994005142E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16797008803916E+00 0.00000000000000E+00 -.21305275424330E+02
 0.34618252378108E-02 0.90914242511038E+00 0.23109196595548E+04 0.86659487233306E+03 0.87995013531887E+01
 0.32998130074457E+01 0.32268084184336E+03 0.29315000038885E+03 0.31815792766406E+03 0.33833459223971E+03
 0.29315000002067E+03 0.29315000003667E+03 0.31519406951361E+03 0.33830373604526E+03 0.29315000001603E+03
 0.29315000003649E+03 0.31815792766406E+03 0.33833459223971E+03 0.29315000002067E+03 0.29315000003667E+03
 0.31519406951361E+03 0.33830373604526E+03 0.29315000001603E+03 0.29315000003649E+03 0.37874688055082E+03
 0.30356417789888E+03 0.18154953312013E+04 0.17319901087690E+04 0.63897517258331E+03 0.11707261370320E+04
 0.52855608858574E+03 0.10591132565064E+04 0.10277229893051E+04 0.99697190707874E+03 0.17335486906277E+04
 0.94161557682721E+03 0.10270990041424E+04 0.89615348378048E+03 0.17332131298776E+04 0.10591132565064E+04
 0.10277229893051E+04 0.99697190707874E+03 0.17335486906277E+04 0.94161557682721E+03 0.10270990041424E+04
 0.89615348378048E+03 0.17332131298776E+04 0.17207475198339E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47185709178352E+03 0.12948614656443E+01
 0.12948614656443E+01 0.14200655522532E+01 0.25292511724374E-01 0.30231640735141E+03 0.32695027087083E+03
 0.32653490268291E+03 0.32650167329197E+03 0.23000000000000E+00 0.00000000000000E+00 0.19451330949429E+00
 0.00000000000000E+00 -.13371180941764E+02 0.71129716457577E-02 0.68167525167730E+00 0.11247057345957E+04
 0.42176465047337E+03 0.11735793517977E+02 0.44009225692415E+01 0.30356189785246E+03 0.37876484946817E+03
 0.29503979830851E+03 0.30019637720612E+03 0.29315000000022E+03 0.29315000000421E+03 0.29502997997444E+03
 0.30019613608210E+03 0.29315000000022E+03 0.29315000000421E+03 0.29503979830851E+03 0.30019637720613E+03
 0.29315000000022E+03 0.29315000000421E+03 0.29502997997444E+03 0.30019613608210E+03 0.29315000000022E+03
 0.29315000000421E+03 0.29913838144120E+03 0.29315000005259E+03 0.96424334756624E+02 0.94025239320165E+02
 0.12729089915902E+03 0.36691887560291E+03 0.23899152194809E+03 0.10351109105155E+03 0.11981395783382E+03
 0.10351109105155E+03 0.29218515912650E+03 0.10329854387378E+03 0.11973710238518E+03 0.10329854387378E+03
 0.29211017983174E+03 0.10351109105155E+03 0.11981395783381E+03 0.10351109105155E+03 0.29218515912648E+03
 0.10329854387378E+03 0.11973710238519E+03 0.10329854387378E+03 0.29211017983176E+03 0.19731845123336E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34944392597131E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18197951661032E+00 0.00000000000000E+00 0.00000000000000E+00 0.18197951661032E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19108915121337E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19108915121337E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951427761568E+00 0.19326434532829E+00 0.32695027087083E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    410.00575022
 0.10278227033762E+00 0.30486669096049E+03 0.43264070827096E+03 0.42826307374118E+03 0.42651628577586E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16703517931853E+00 0.00000000000000E+00 -.21334630702362E+02
 0.34586646806413E-02 0.92204866115687E+00 0.23130313975730E+04 0.86738677408986E+03 0.86763316699171E+01
 0.32536243762189E+01 0.32320993674905E+03 0.29315000052404E+03 0.31860731902292E+03 0.33904127893888E+03
 0.29315000002887E+03 0.29315000005142E+03 0.31561207427013E+03 0.33901083504807E+03 0.29315000002241E+03
 0.29315000005117E+03 0.31860731902292E+03 0.33904127893888E+03 0.29315000002887E+03 0.29315000005142E+03
 0.31561207427013E+03 0.33901083504807E+03 0.29315000002241E+03 0.29315000005117E+03 0.37963971190838E+03
 0.30417411904434E+03 0.18210356159652E+04 0.17358010371904E+04 0.63787396073693E+03 0.11633431670628E+04
 0.52227983652223E+03 0.10628446409860E+04 0.10309598940078E+04 0.99951374765227E+03 0.17334244630724E+04
 0.94560330601780E+03 0.10303583590918E+04 0.89919235574684E+03 0.17331070670370E+04 0.10628446409860E+04
 0.10309598940078E+04 0.99951374765227E+03 0.17334244630724E+04 0.94560330601781E+03 0.10303583590918E+04
 0.89919235574684E+03 0.17331070670370E+04 0.17198642188854E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47222440837966E+03 0.12948616819395E+01
 0.12948616819395E+01 0.14600383231733E+01 0.20347722962101E-01 0.30313221659362E+03 0.32702825914874E+03
 0.32670410577954E+03 0.32667892618848E+03 0.23000000000000E+00 0.00000000000000E+00 0.19332305578625E+00
 0.00000000000000E+00 -.13377304185451E+02 0.88415252786139E-02 0.70489344567634E+00 0.90482125514594E+03
 0.33930797067973E+03 0.11349233063621E+02 0.42559623988580E+01 0.30417188198126E+03 0.37965656734657E+03
 0.29511941473427E+03 0.30033795197828E+03 0.29315000000027E+03 0.29315000000588E+03 0.29510988221010E+03
 0.30033759835364E+03 0.29315000000027E+03 0.29315000000588E+03 0.29511941473427E+03 0.30033795197828E+03
 0.29315000000027E+03 0.29315000000588E+03 0.29510988221010E+03 0.30033759835364E+03 0.29315000000027E+03
 0.29315000000588E+03 0.29926064315173E+03 0.29315000007167E+03 0.95915950526702E+02 0.94033657113768E+02
 0.12961102752298E+03 0.36889947033085E+03 0.23864038767025E+03 0.10635935172486E+03 0.12194228852706E+03
 0.10635935172486E+03 0.29370055243762E+03 0.10618080993676E+03 0.12185952168315E+03 0.10618080993676E+03
 0.29362053385220E+03 0.10635935172486E+03 0.12194228852705E+03 0.10635935172486E+03 0.29370055243760E+03
 0.10618080993676E+03 0.12185952168316E+03 0.10618080993676E+03 0.29362053385220E+03 0.19844196033962E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34972834639811E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18206088760479E+00 0.00000000000000E+00 0.00000000000000E+00 0.18206088760479E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19117760088044E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19117760088044E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951406675105E+00 0.19321801784317E+00 0.32702825914874E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    420.68576120
 0.10287519307870E+00 0.30511943816282E+03 0.43304653868114E+03 0.42865969695920E+03 0.42690867736170E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16606963049366E+00 0.00000000000000E+00 -.21365793188402E+02
 0.34555402382830E-02 0.93532320398851E+00 0.23151228023248E+04 0.86817105087181E+03 0.85531931271302E+01
 0.32074474226738E+01 0.32376761340305E+03 0.29315000071349E+03 0.31908134160817E+03 0.33978552622974E+03
 0.29315000004081E+03 0.29315000007296E+03 0.31605323074791E+03 0.33975550530533E+03 0.29315000003173E+03
 0.29315000007261E+03 0.31908134160817E+03 0.33978552622974E+03 0.29315000004081E+03 0.29315000007296E+03
 0.31605323074791E+03 0.33975550530533E+03 0.29315000003173E+03 0.29315000007261E+03 0.38057528346019E+03
 0.30483542255171E+03 0.18267886181858E+04 0.17397248645663E+04 0.63666473955241E+03 0.11556690347139E+04
 0.51582097146370E+03 0.10667325900185E+04 0.10343022640044E+04 0.10021420211068E+04 0.17332902502260E+04
 0.94976269512595E+03 0.10337231144399E+04 0.90234528878783E+03 0.17329908473916E+04 0.10667325900185E+04
 0.10343022640044E+04 0.10021420211068E+04 0.17332902502260E+04 0.94976269512595E+03 0.10337231144399E+04
 0.90234528878783E+03 0.17329908473916E+04 0.17189345616761E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47261785980492E+03 0.12948619115507E+01
 0.12948619115507E+01 0.15027583671054E+01 0.16122723977202E-01 0.30398417811754E+03 0.32712387804886E+03
 0.32687516138559E+03 0.32685644753487E+03 0.23000000000000E+00 0.00000000000000E+00 0.19204446417376E+00
 0.00000000000000E+00 -.13384326055917E+02 0.11158467767642E-01 0.72977134369340E+00 0.71694431230053E+03
 0.26885411711270E+03 0.10962337818736E+02 0.41108766820260E+01 0.30483324149242E+03 0.38059100062792E+03
 0.29520555334314E+03 0.30048847744014E+03 0.29315000000035E+03 0.29315000000835E+03 0.29519633329536E+03
 0.30048800530879E+03 0.29315000000035E+03 0.29315000000835E+03 0.29520555334314E+03 0.30048847744014E+03
 0.29315000000035E+03 0.29315000000835E+03 0.29519633329536E+03 0.30048800530879E+03 0.29315000000035E+03
 0.29315000000835E+03 0.29939051778799E+03 0.29315000009873E+03 0.95192863513169E+02 0.93758651328466E+02
 0.13201669010926E+03 0.37104882735010E+03 0.23837205379030E+03 0.10936985939645E+03 0.12415037258182E+03
 0.10936985939645E+03 0.29537719673310E+03 0.10922299137219E+03 0.12406180974932E+03 0.10922299137219E+03
 0.29529229932099E+03 0.10936985939645E+03 0.12415037258181E+03 0.10936985939645E+03 0.29537719673309E+03
 0.10922299137219E+03 0.12406180974932E+03 0.10922299137219E+03 0.29529229932099E+03 0.19955689729100E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35000873919574E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18214599623429E+00 0.00000000000000E+00 0.00000000000000E+00 0.18214599623429E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19123695518228E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19123695518228E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951376459001E+00 0.19316120134241E+00 0.32712387804886E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    431.39410469
 0.10296373776262E+00 0.30536758898396E+03 0.43345281448601E+03 0.42905676996272E+03 0.42730142140720E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16513513914591E+00 0.00000000000000E+00 -.21392778346876E+02
 0.34525682338733E-02 0.94811762448703E+00 0.23171156826132E+04 0.86891838097996E+03 0.84377716365397E+01
 0.31641643637024E+01 0.32431871726926E+03 0.29315000096590E+03 0.31955011459072E+03 0.34052050922817E+03
 0.29315000005738E+03 0.29315000010297E+03 0.31648974142392E+03 0.34049089461122E+03 0.29315000004469E+03
 0.29315000010248E+03 0.31955011459072E+03 0.34052050922817E+03 0.29315000005738E+03 0.29315000010297E+03
 0.31648974142392E+03 0.34049089461122E+03 0.29315000004469E+03 0.29315000010248E+03 0.38149418888432E+03
 0.30550726785203E+03 0.18323828119693E+04 0.17434935936503E+04 0.63541795834017E+03 0.11481785307833E+04
 0.50958348265147E+03 0.10705276941692E+04 0.10375293130354E+04 0.10046761191291E+04 0.17331375049919E+04
 0.95382723382667E+03 0.10369710879894E+04 0.90539919891703E+03 0.17328548105953E+04 0.10705276941692E+04
 0.10375293130354E+04 0.10046761191291E+04 0.17331375049919E+04 0.95382723382667E+03 0.10369710879894E+04
 0.90539919891703E+03 0.17328548105953E+04 0.17179995195427E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47301172553110E+03 0.12948621103826E+01
 0.12948621103826E+01 0.15455917410633E+01 0.12760884183974E-01 0.30481787721909E+03 0.32722811648684E+03
 0.32703746684162E+03 0.32702357819095E+03 0.23000000000000E+00 0.00000000000000E+00 0.19076145266545E+00
 0.00000000000000E+00 -.13388705100693E+02 0.14098152085242E-01 0.75468040625495E+00 0.56745025529795E+03
 0.21279384573673E+03 0.10600513719045E+02 0.39751926446418E+01 0.30550515178614E+03 0.38150884563022E+03
 0.29529279269144E+03 0.30063848551197E+03 0.29315000000048E+03 0.29315000001180E+03 0.29528388495095E+03
 0.30063789675512E+03 0.29315000000048E+03 0.29315000001180E+03 0.29529279269144E+03 0.30063848551197E+03
 0.29315000000048E+03 0.29315000001180E+03 0.29528388495095E+03 0.30063789675512E+03 0.29315000000048E+03
 0.29315000001180E+03 0.29951978450144E+03 0.29315000013522E+03 0.94277846078714E+02 0.93197879065414E+02
 0.13434053785188E+03 0.37319243433130E+03 0.23818019379016E+03 0.11235641234589E+03 0.12628175117424E+03
 0.11235641234589E+03 0.29706946227885E+03 0.11223701678992E+03 0.12618780959692E+03 0.11223701678992E+03
 0.29698008759940E+03 0.11235641234589E+03 0.12628175117424E+03 0.11235641234589E+03 0.29706946227885E+03
 0.11223701678992E+03 0.12618780959692E+03 0.11223701678992E+03 0.29698008759940E+03 0.20057233792241E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35027070538421E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18219679843226E+00 0.00000000000000E+00 0.00000000000000E+00 0.18219679843226E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19130453313001E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19130453313001E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951349521714E+00 0.19309937177924E+00 0.32722811648684E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    440.57268482
 0.10302141657924E+00 0.30558095241568E+03 0.43380172254886E+03 0.42939856075752E+03 0.42763979655911E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16436005979020E+00 0.00000000000000E+00 -.21411940249952E+02
 0.34506349109566E-02 0.95868499435386E+00 0.23184139169861E+04 0.86940521886980E+03 0.83447639705594E+01
 0.31292864889598E+01 0.32478578323833E+03 0.29315000124117E+03 0.31994773140069E+03 0.34114204629799E+03
 0.29315000007608E+03 0.29315000013694E+03 0.31686032434567E+03 0.34111276997450E+03 0.29315000005934E+03
 0.29315000013630E+03 0.31994773140069E+03 0.34114204629799E+03 0.29315000007608E+03 0.29315000013694E+03
 0.31686032434567E+03 0.34111276997450E+03 0.29315000005934E+03 0.29315000013630E+03 0.38226437445467E+03
 0.30608844876070E+03 0.18370703739316E+04 0.17466478666175E+04 0.63437920323537E+03 0.11420164244400E+04
 0.50446532518844E+03 0.10737150631225E+04 0.10402222520679E+04 0.10068056410652E+04 0.17330225105907E+04
 0.95724332853200E+03 0.10396808377183E+04 0.90796774237261E+03 0.17327531228334E+04 0.10737150631225E+04
 0.10402222520679E+04 0.10068056410652E+04 0.17330225105907E+04 0.95724332853200E+03 0.10396808377183E+04
 0.90796774237261E+03 0.17327531228334E+04 0.17172527316079E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47335093579260E+03 0.12948622515713E+01
 0.12948622515713E+01 0.15823060615987E+01 0.10440569252350E-01 0.30551062774919E+03 0.32732213089904E+03
 0.32717031455962E+03 0.32715955663228E+03 0.23000000000000E+00 0.00000000000000E+00 0.18966463293773E+00
 0.00000000000000E+00 -.13388356261150E+02 0.17231328483069E-01 0.77593627100772E+00 0.46427064563599E+03
 0.17410149211349E+03 0.10310125069434E+02 0.38662969010379E+01 0.30608639504642E+03 0.38227813359813E+03
 0.29536871901834E+03 0.30076656419647E+03 0.29315000000064E+03 0.29315000001571E+03 0.29536007891719E+03
 0.30076587655592E+03 0.29315000000063E+03 0.29315000001571E+03 0.29536871901834E+03 0.30076656419647E+03
 0.29315000000064E+03 0.29315000001571E+03 0.29536007891719E+03 0.30076587655592E+03 0.29315000000063E+03
 0.29315000001571E+03 0.29963010895367E+03 0.29315000017543E+03 0.93350217848347E+02 0.92498611742933E+02
 0.13625342443034E+03 0.37498906279710E+03 0.23805437124461E+03 0.11488024054026E+03 0.12803475011139E+03
 0.11488024054026E+03 0.29849740291532E+03 0.11478128508631E+03 0.12793656592559E+03 0.11478128508631E+03
 0.29840454922566E+03 0.11488024054026E+03 0.12803475011139E+03 0.11488024054026E+03 0.29849740291531E+03
 0.11478128508631E+03 0.12793656592559E+03 0.11478128508631E+03 0.29840454922566E+03 0.20135170168503E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35048070728914E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18220708596555E+00 0.00000000000000E+00 0.00000000000000E+00 0.18220708596555E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19132748827833E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19132748827833E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951336010651E+00 0.19304376458802E+00 0.32732213089904E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    450.01277540
 0.10307613914282E+00 0.30579465452194E+03 0.43415934179937E+03 0.42974889634376E+03 0.42798653517502E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16358699719688E+00 0.00000000000000E+00 -.21422195397817E+02
 0.34488026540151E-02 0.96918659792389E+00 0.23196456285158E+04 0.86986711069344E+03 0.82543444339170E+01
 0.30953791627189E+01 0.32525974188322E+03 0.29315000160539E+03 0.32035138854978E+03 0.34177309022426E+03
 0.29315000010176E+03 0.29315000018373E+03 0.31723659082186E+03 0.34174414615806E+03 0.29315000007950E+03
 0.29315000018289E+03 0.32035138854978E+03 0.34177309022426E+03 0.29315000010176E+03 0.29315000018373E+03
 0.31723659082186E+03 0.34174414615806E+03 0.29315000007950E+03 0.29315000018289E+03 0.38304631259044E+03
 0.30669196483430E+03 0.18417775446134E+04 0.17497757672421E+04 0.63324928353448E+03 0.11357362701137E+04
 0.49932074016152E+03 0.10769247648745E+04 0.10429067307844E+04 0.10089205527143E+04 0.17328893696723E+04
 0.96068671306184E+03 0.10423816156075E+04 0.91053112351605E+03 0.17326328377738E+04 0.10769247648745E+04
 0.10429067307844E+04 0.10089205527143E+04 0.17328893696723E+04 0.96068671306184E+03 0.10423816156075E+04
 0.91053112351605E+03 0.17326328377738E+04 0.17164609481966E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47369859159460E+03 0.12948623271332E+01
 0.12948623271332E+01 0.16200664239018E+01 0.84927761148315E-02 0.30621129820639E+03 0.32742218921888E+03
 0.32730209631984E+03 0.32729382797006E+03 0.23000000000000E+00 0.00000000000000E+00 0.18854219080205E+00
 0.00000000000000E+00 -.13378798958414E+02 0.21183281965667E-01 0.79765498096270E+00 0.37765630523948E+03
 0.14162111446480E+03 0.10029398914233E+02 0.37610245928375E+01 0.30668997721552E+03 0.38305922416029E+03
 0.29544671243825E+03 0.30089708198261E+03 0.29315000000086E+03 0.29315000002112E+03 0.29543833489443E+03
 0.30089629547704E+03 0.29315000000085E+03 0.29315000002113E+03 0.29544671243825E+03 0.30089708198261E+03
 0.29315000000086E+03 0.29315000002112E+03 0.29543833489443E+03 0.30089629547704E+03 0.29315000000085E+03
 0.29315000002113E+03 0.29974241344104E+03 0.29315000022921E+03 0.92254651647821E+02 0.91589943356816E+02
 0.13814930735129E+03 0.37680138793741E+03 0.23796133404936E+03 0.11745395360877E+03 0.12976982228225E+03
 0.11745395360877E+03 0.29994312919071E+03 0.11737336951307E+03 0.12966753091167E+03 0.11737336951307E+03
 0.29984693119718E+03 0.11745395360877E+03 0.12976982228225E+03 0.11745395360877E+03 0.29994312919070E+03
 0.11737336951307E+03 0.12966753091167E+03 0.11737336951307E+03 0.29984693119718E+03 0.20207535717471E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35068542514419E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18214054193619E+00 0.00000000000000E+00 0.00000000000000E+00 0.18214054193619E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19133283988783E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19133283988783E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951347754210E+00 0.19298491599468E+00 0.32742218921888E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    460.22690195
 0.10309253644387E+00 0.30603491565532E+03 0.43454938538913E+03 0.43013309117093E+03 0.42836774435446E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16277698062869E+00 0.00000000000000E+00 -.21445352826637E+02
 0.34482537386672E-02 0.98013700895195E+00 0.23200148847202E+04 0.87000558177009E+03 0.81621241999160E+01
 0.30607965749685E+01 0.32576939474065E+03 0.29315000208054E+03 0.32078594375693E+03 0.34244812122844E+03
 0.29315000013618E+03 0.29315000024659E+03 0.31764235713101E+03 0.34241953496089E+03 0.29315000010656E+03
 0.29315000024549E+03 0.32078594375693E+03 0.34244812122844E+03 0.29315000013618E+03 0.29315000024659E+03
 0.31764235713101E+03 0.34241953496089E+03 0.29315000010656E+03 0.29315000024549E+03 0.38386844386369E+03
 0.30734666919860E+03 0.18468074402907E+04 0.17531693648346E+04 0.63217871860957E+03 0.11294248321084E+04
 0.49408521990575E+03 0.10803557264837E+04 0.10457776208077E+04 0.10112253396129E+04 0.17328140964714E+04
 0.96436773979610E+03 0.10452690618312E+04 0.91331099581889E+03 0.17325704280931E+04 0.10803557264837E+04
 0.10457776208077E+04 0.10112253396129E+04 0.17328140964714E+04 0.96436773979609E+03 0.10452690618312E+04
 0.91331099581889E+03 0.17325704280931E+04 0.17157689722824E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47408037258365E+03 0.12948624977618E+01
 0.12948624977618E+01 0.16609229300905E+01 0.67903881486715E-02 0.30694731718637E+03 0.32753359323632E+03
 0.32744040069971E+03 0.32743418066563E+03 0.23000000000000E+00 0.00000000000000E+00 0.18733710173063E+00
 0.00000000000000E+00 -.13380181591814E+02 0.26494047160944E-01 0.82093647593538E+00 0.30195462216106E+03
 0.11323298331040E+03 0.97449683800257E+01 0.36543631425096E+01 0.30734476887133E+03 0.38388038862494E+03
 0.29553364266051E+03 0.30103853998589E+03 0.29315000000116E+03 0.29315000002841E+03 0.29552555344184E+03
 0.30103764618496E+03 0.29315000000115E+03 0.29315000002842E+03 0.29553364266051E+03 0.30103853998589E+03
 0.29315000000116E+03 0.29315000002841E+03 0.29552555344184E+03 0.30103764618496E+03 0.29315000000115E+03
 0.29315000002842E+03 0.29986417403131E+03 0.29315000029992E+03 0.90941927203710E+02 0.90424031209543E+02
 0.14010583105693E+03 0.37868741985360E+03 0.23788105964139E+03 0.12017876959198E+03 0.13155862628169E+03
 0.12017876959198E+03 0.30144997417377E+03 0.12011528986231E+03 0.13145235446083E+03 0.12011528986231E+03
 0.30135062314226E+03 0.12017876959198E+03 0.13155862628168E+03 0.12017876959198E+03 0.30144997417377E+03
 0.12011528986231E+03 0.13145235446083E+03 0.12011528986231E+03 0.30135062314226E+03 0.20276365750205E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35089699911235E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18216553520881E+00 0.00000000000000E+00 0.00000000000000E+00 0.18216553520881E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19131453206289E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19131453206289E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951323330745E+00 0.19291901091807E+00 0.32753359323632E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    470.06338294
 0.10309946924477E+00 0.30626030058942E+03 0.43492432912905E+03 0.43050259811128E+03 0.42873434000562E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16202183153981E+00 0.00000000000000E+00 -.21462960947470E+02
 0.34480215108182E-02 0.99030230195829E+00 0.23201711401451E+04 0.87006417755442E+03 0.80783413147483E+01
 0.30293779930306E+01 0.32625439085066E+03 0.29315000265286E+03 0.32119966104548E+03 0.34309063261907E+03
 0.29315000017900E+03 0.29315000032498E+03 0.31802876124494E+03 0.34306237625638E+03 0.29315000014028E+03
 0.29315000032354E+03 0.32119966104548E+03 0.34309063261907E+03 0.29315000017900E+03 0.29315000032498E+03
 0.31802876124494E+03 0.34306237625638E+03 0.29315000014028E+03 0.29315000032354E+03 0.38465156745371E+03
 0.30798145158141E+03 0.18515525617831E+04 0.17563310804894E+04 0.63108308271107E+03 0.11233882957327E+04
 0.48914979760811E+03 0.10836002190561E+04 0.10484671972968E+04 0.10133732152207E+04 0.17327353029851E+04
 0.96785173684463E+03 0.10479736151149E+04 0.91591402948272E+03 0.17325032058776E+04 0.10836002190561E+04
 0.10484671972968E+04 0.10133732152207E+04 0.17327353029851E+04 0.96785173684463E+03 0.10479736151149E+04
 0.91591402948272E+03 0.17325032058776E+04 0.17150830339912E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47444755622925E+03 0.12948626275020E+01
 0.12948626275020E+01 0.17002688540805E+01 0.54731548651206E-02 0.30764995314533E+03 0.32764369293575E+03
 0.32757074037962E+03 0.32756601771412E+03 0.23000000000000E+00 0.00000000000000E+00 0.18618800140150E+00
 0.00000000000000E+00 -.13376817862866E+02 0.32870412349460E-01 0.84310168438370E+00 0.24337997086707E+03
 0.91267489075150E+02 0.94887724080968E+01 0.35582896530363E+01 0.30797963471587E+03 0.38466268471812E+03
 0.29561728302790E+03 0.30117352129382E+03 0.29315000000156E+03 0.29315000003753E+03 0.29560945496514E+03
 0.30117252734981E+03 0.29315000000154E+03 0.29315000003754E+03 0.29561728302790E+03 0.30117352129382E+03
 0.29315000000156E+03 0.29315000003753E+03 0.29560945496514E+03 0.30117252734981E+03 0.29315000000154E+03
 0.29315000003754E+03 0.29998026677181E+03 0.29315000038587E+03 0.89541376290002E+02 0.89138369548602E+02
 0.14191261395285E+03 0.38045922242600E+03 0.23783704540338E+03 0.12277409904177E+03 0.13320802545847E+03
 0.12277409904177E+03 0.30286586638697E+03 0.12272475203634E+03 0.13309819843114E+03 0.12272475203634E+03
 0.30276373196649E+03 0.12277409904177E+03 0.13320802545847E+03 0.12277409904177E+03 0.30286586638697E+03
 0.12272475203634E+03 0.13309819843114E+03 0.12272475203634E+03 0.30276373196649E+03 0.20335381494611E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35109275309469E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18215140995483E+00 0.00000000000000E+00 0.00000000000000E+00 0.18215140995483E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19127770362397E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19127770362397E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951312437649E+00 0.19285407278597E+00 0.32764369293575E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    480.62146523
 0.10309977992605E+00 0.30649801412320E+03 0.43532573559907E+03 0.43089836568823E+03 0.42912698772522E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16123753082434E+00 0.00000000000000E+00 -.21480545377365E+02
 0.34480107417501E-02 0.10008139811256E+01 0.23201783866658E+04 0.87006689499967E+03 0.79934934472063E+01
 0.29975600427024E+01 0.32676917295546E+03 0.29315000342215E+03 0.32163903831576E+03 0.34377195327835E+03
 0.29315000023846E+03 0.29315000043411E+03 0.31843938157380E+03 0.34374403857242E+03 0.29315000018721E+03
 0.29315000043222E+03 0.32163903831576E+03 0.34377195327835E+03 0.29315000023846E+03 0.29315000043411E+03
 0.31843938157380E+03 0.34374403857242E+03 0.29315000018721E+03 0.29315000043222E+03 0.38547864313481E+03
 0.30866642244993E+03 0.18565382676138E+04 0.17596206978299E+04 0.62989038968071E+03 0.11170518700682E+04
 0.48401202843908E+03 0.10870183492674E+04 0.10512770053359E+04 0.10156123874385E+04 0.17326468203860E+04
 0.97152472490498E+03 0.10507985248885E+04 0.91863694346575E+03 0.17324263003182E+04 0.10870183492674E+04
 0.10512770053359E+04 0.10156123874385E+04 0.17326468203860E+04 0.97152472490498E+03 0.10507985248885E+04
 0.91863694346575E+03 0.17324263003182E+04 0.17143533323547E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47484084779494E+03 0.12948627570677E+01
 0.12948627570677E+01 0.17425011832439E+01 0.43409335688205E-02 0.30839668544730E+03 0.32776498076339E+03
 0.32770892977451E+03 0.32770542041874E+03 0.23000000000000E+00 0.00000000000000E+00 0.18496919590786E+00
 0.00000000000000E+00 -.13372626330712E+02 0.41443815637388E-01 0.86657372634902E+00 0.19303241936013E+03
 0.72387157260047E+02 0.92317592338103E+01 0.34619097126789E+01 0.30866469468167E+03 0.38548894198530E+03
 0.29570757791068E+03 0.30131735322731E+03 0.29315000000214E+03 0.29315000005028E+03 0.29570001562744E+03
 0.30131625442682E+03 0.29315000000211E+03 0.29315000005029E+03 0.29570757791068E+03 0.30131735322731E+03
 0.29315000000214E+03 0.29315000005028E+03 0.29570001562744E+03 0.30131625442682E+03 0.29315000000211E+03
 0.29315000005029E+03 0.30010391181828E+03 0.29315000050248E+03 0.87903779718396E+02 0.87598523241556E+02
 0.14377283819028E+03 0.38231863433162E+03 0.23782693195038E+03 0.12553058330299E+03 0.13490332706060E+03
 0.12553058330299E+03 0.30435040175322E+03 0.12549415909608E+03 0.13478996090681E+03 0.12549415909608E+03
 0.30424553609679E+03 0.12553058330299E+03 0.13490332706060E+03 0.12553058330299E+03 0.30435040175322E+03
 0.12549415909608E+03 0.13478996090681E+03 0.12549415909608E+03 0.30424553609679E+03 0.20391742924220E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35129780281786E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18212606223283E+00 0.00000000000000E+00 0.00000000000000E+00 0.18212606223283E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19124923128447E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19124923128447E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951301067232E+00 0.19278259394939E+00 0.32776498076339E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    490.03196856
 0.10307800431205E+00 0.30671135728029E+03 0.43568351966712E+03 0.43125212196358E+03 0.42947837133054E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16056054785880E+00 0.00000000000000E+00 -.21484395717672E+02
 0.34487388072024E-02 0.10098457078350E+01 0.23196885723247E+04 0.86988321462176E+03 0.79220022800823E+01
 0.29707508550309E+01 0.32722382825861E+03 0.29315000426194E+03 0.32202738578790E+03 0.34437210932733E+03
 0.29315000030532E+03 0.29315000055707E+03 0.31880268741781E+03 0.34434449148150E+03 0.29315000024007E+03
 0.29315000055469E+03 0.32202738578790E+03 0.34437210932733E+03 0.29315000030532E+03 0.29315000055707E+03
 0.31880268741781E+03 0.34434449148150E+03 0.29315000024007E+03 0.29315000055469E+03 0.38620076615750E+03
 0.30927848715203E+03 0.18609128580818E+04 0.17625148207810E+04 0.62887483499649E+03 0.11116393284484E+04
 0.47962011927696E+03 0.10900212900839E+04 0.10537344366561E+04 0.10175870287694E+04 0.17325905412244E+04
 0.97475269009291E+03 0.10532686320070E+04 0.92103655828452E+03 0.17323796394213E+04 0.10900212900839E+04
 0.10537344366561E+04 0.10175870287694E+04 0.17325905412244E+04 0.97475269009291E+03 0.10532686320070E+04
 0.92103655828452E+03 0.17323796394213E+04 0.17137660378509E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47519262301642E+03 0.12948627854378E+01
 0.12948627854378E+01 0.17801431965314E+01 0.35302958219439E-02 0.30905354946243E+03 0.32787538291985E+03
 0.32783108515984E+03 0.32782839409364E+03 0.23000000000000E+00 0.00000000000000E+00 0.18389720335686E+00
 0.00000000000000E+00 -.13356079490266E+02 0.50960274659814E-01 0.88718668692927E+00 0.15698502516723E+03
 0.58869384437712E+02 0.90172678624040E+01 0.33814754484015E+01 0.30927684485425E+03 0.38621035301841E+03
 0.29578914250001E+03 0.30144498548166E+03 0.29315000000282E+03 0.29315000006469E+03 0.29578180740725E+03
 0.30144379484766E+03 0.29315000000278E+03 0.29315000006471E+03 0.29578914250001E+03 0.30144498548166E+03
 0.29315000000282E+03 0.29315000006469E+03 0.29578180740725E+03 0.30144379484766E+03 0.29315000000278E+03
 0.29315000006471E+03 0.30021363107620E+03 0.29315000063083E+03 0.86340719017920E+02 0.86101752863516E+02
 0.14536277922311E+03 0.38392883513479E+03 0.23783924201557E+03 0.12795976594417E+03 0.13634955914254E+03
 0.12795976594417E+03 0.30563234641273E+03 0.12793312602984E+03 0.13623327033801E+03 0.12793312602984E+03
 0.30552526531987E+03 0.12795976594417E+03 0.13634955914254E+03 0.12795976594417E+03 0.30563234641273E+03
 0.12793312602984E+03 0.13623327033801E+03 0.12793312602984E+03 0.30552526531987E+03 0.20435877067706E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35147462955210E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18200554242405E+00 0.00000000000000E+00 0.00000000000000E+00 0.18200554242405E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19117793075421E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19117793075421E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951328469982E+00 0.19271800589076E+00 0.32787538291985E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    500.22232973
 0.10304199847854E+00 0.30694084710439E+03 0.43607097825589E+03 0.43163570266334E+03 0.42985953548460E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15984990217597E+00 0.00000000000000E+00 -.21505024842783E+02
 0.34499435270189E-02 0.10192827083051E+01 0.23188785373866E+04 0.86957945151998E+03 0.78486566433590E+01
 0.29432462412596E+01 0.32771150259378E+03 0.29315000536133E+03 0.32244414675498E+03 0.34501539325014E+03
 0.29315000039530E+03 0.29315000072289E+03 0.31919275617115E+03 0.34498808613569E+03 0.29315000031133E+03
 0.29315000071984E+03 0.32244414675498E+03 0.34501539325014E+03 0.29315000039530E+03 0.29315000072289E+03
 0.31919275617115E+03 0.34498808613569E+03 0.29315000031133E+03 0.29315000071984E+03 0.38697412525628E+03
 0.30994359529830E+03 0.18655834831233E+04 0.17655970497396E+04 0.62774923013386E+03 0.11058644404922E+04
 0.47497646420769E+03 0.10932319575114E+04 0.10563479743189E+04 0.10196926969278E+04 0.17325473726347E+04
 0.97820564536468E+03 0.10558951055343E+04 0.92359860428894E+03 0.17323462077892E+04 0.10932319575114E+04
 0.10563479743189E+04 0.10196926969278E+04 0.17325473726347E+04 0.97820564536468E+03 0.10558951055343E+04
 0.92359860428894E+03 0.17323462077892E+04 0.17131496276780E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47557414965390E+03 0.12948629374374E+01
 0.12948629374374E+01 0.18209046412286E+01 0.28220703520989E-02 0.30975967654024E+03 0.32799791699877E+03
 0.32796360393366E+03 0.32796158765004E+03 0.23000000000000E+00 0.00000000000000E+00 0.18275284149475E+00
 0.00000000000000E+00 -.13355360032140E+02 0.63749239428960E-01 0.90915575811924E+00 0.12549169326036E+03
 0.47059384972633E+02 0.87993723061817E+01 0.32997646148181E+01 0.30994205358664E+03 0.38698296037782E+03
 0.29587769562532E+03 0.30158218482007E+03 0.29315000000377E+03 0.29315000008419E+03 0.29587059226153E+03
 0.30158089733549E+03 0.29315000000372E+03 0.29315000008421E+03 0.29587769562532E+03 0.30158218482007E+03
 0.29315000000377E+03 0.29315000008419E+03 0.29587059226153E+03 0.30158089733549E+03 0.29315000000372E+03
 0.29315000008421E+03 0.30033155172197E+03 0.29315000080014E+03 0.84542749475119E+02 0.84360594742210E+02
 0.14701928650810E+03 0.38564486119621E+03 0.23789047825557E+03 0.13055707693423E+03 0.13785496253937E+03
 0.13055707693423E+03 0.30699617323189E+03 0.13053943149038E+03 0.13773579192752E+03 0.13053943149038E+03
 0.30688695615978E+03 0.13055707693423E+03 0.13785496253937E+03 0.13055707693423E+03 0.30699617323189E+03
 0.13053943149038E+03 0.13773579192752E+03 0.13053943149038E+03 0.30688695615978E+03 0.20478749131134E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35166494625690E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18201078733708E+00 0.00000000000000E+00 0.00000000000000E+00 0.18201078733708E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19112153396178E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19112153396178E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951305329767E+00 0.19264576297316E+00 0.32799791699877E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    511.33908738
 0.10300318347445E+00 0.30718715313900E+03 0.43649215162969E+03 0.43205254280179E+03 0.43027364075003E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15910041191570E+00 0.00000000000000E+00 -.21515594553659E+02
 0.34512431801452E-02 0.10291880336860E+01 0.23180053048779E+04 0.86925198932920E+03 0.77731179708225E+01
 0.29149192390584E+01 0.32823805199186E+03 0.29315000684002E+03 0.32289439867170E+03 0.34570897864784E+03
 0.29315000052008E+03 0.29315000095332E+03 0.31961448591737E+03 0.34568199939240E+03 0.29315000041036E+03
 0.29315000094938E+03 0.32289439867170E+03 0.34570897864784E+03 0.29315000052008E+03 0.29315000095332E+03
 0.31961448591737E+03 0.34568199939240E+03 0.29315000041036E+03 0.29315000094938E+03 0.38780392604968E+03
 0.31067101223042E+03 0.18705761685801E+04 0.17688595894421E+04 0.62651951656535E+03 0.10997194781757E+04
 0.47006736402751E+03 0.10966740625066E+04 0.10591293084155E+04 0.10219275976777E+04 0.17324982910604E+04
 0.98190938659183E+03 0.10586896919364E+04 0.92632567917305E+03 0.17323070058709E+04 0.10966740625066E+04
 0.10591293084155E+04 0.10219275976777E+04 0.17324982910604E+04 0.98190938659183E+03 0.10586896919364E+04
 0.92632567917305E+03 0.17323070058709E+04 0.17124876258011E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47598870590713E+03 0.12948630153173E+01
 0.12948630153173E+01 0.18653716718074E+01 0.22099575434016E-02 0.31052496489705E+03 0.32813554602630E+03
 0.32810960026856E+03 0.32810813111593E+03 0.23000000000000E+00 0.00000000000000E+00 0.18152513766911E+00
 0.00000000000000E+00 -.13342014809558E+02 0.81406465229081E-01 0.93268167006967E+00 0.98272292962085E+02
 0.36852109860782E+02 0.85774174155288E+01 0.32165315308233E+01 0.31066957755430E+03 0.38781201419780E+03
 0.29597500961982E+03 0.30173091079120E+03 0.29315000000515E+03 0.29315000011140E+03 0.29596814343098E+03
 0.30172952029095E+03 0.29315000000509E+03 0.29315000011143E+03 0.29597500961982E+03 0.30173091079120E+03
 0.29315000000515E+03 0.29315000011140E+03 0.29596814343098E+03 0.30172952029095E+03 0.29315000000509E+03
 0.29315000011143E+03 0.30045936724970E+03 0.29315000102976E+03 0.82471938585530E+02 0.82338479458894E+02
 0.14876114250817E+03 0.38749388125567E+03 0.23798893303496E+03 0.13336942285700E+03 0.13943537515154E+03
 0.13336942285700E+03 0.30846059706237E+03 0.13335996702287E+03 0.13931328698774E+03 0.13335996702287E+03
 0.30834925561805E+03 0.13336942285700E+03 0.13943537515154E+03 0.13336942285700E+03 0.30846059706237E+03
 0.13335996702287E+03 0.13931328698774E+03 0.13335996702287E+03 0.30834925561805E+03 0.20520721391767E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35186948384631E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18191908487634E+00 0.00000000000000E+00 0.00000000000000E+00 0.18191908487634E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19102421542376E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19102421542376E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951316639644E+00 0.19256510876469E+00 0.32813554602630E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    520.60305208
 0.10294720588203E+00 0.30739324701281E+03 0.43684244392683E+03 0.43240029955151E+03 0.43061955435502E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15849554053331E+00 0.00000000000000E+00 -.21523300425599E+02
 0.34531194605450E-02 0.10371435189545E+01 0.23167457979393E+04 0.86877967422723E+03 0.77134937005292E+01
 0.28925601376984E+01 0.32867282802323E+03 0.29315000832928E+03 0.32326639645232E+03 0.34628075937972E+03
 0.29315000064928E+03 0.29315000119235E+03 0.31996317332148E+03 0.34625404507369E+03 0.29315000051308E+03
 0.29315000118749E+03 0.32326639645232E+03 0.34628075937972E+03 0.29315000064928E+03 0.29315000119235E+03
 0.31996317332148E+03 0.34625404507369E+03 0.29315000051308E+03 0.29315000118749E+03 0.38848515541241E+03
 0.31127812699269E+03 0.18746820481702E+04 0.17715506986665E+04 0.62550031530612E+03 0.10947262965177E+04
 0.46609847963503E+03 0.10995070165476E+04 0.10614039537697E+04 0.10237729391614E+04 0.17324688962114E+04
 0.98495898663932E+03 0.10609747504743E+04 0.92857676246362E+03 0.17322853007856E+04 0.10995070165476E+04
 0.10614039537697E+04 0.10237729391614E+04 0.17324688962114E+04 0.98495898663932E+03 0.10609747504743E+04
 0.92857676246362E+03 0.17322853007856E+04 0.17119695975679E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47633480127582E+03 0.12948630720957E+01
 0.12948630720957E+01 0.19024275306231E+01 0.18024133614176E-02 0.31115872619072E+03 0.32825234221185E+03
 0.32823180237058E+03 0.32823067543729E+03 0.23000000000000E+00 0.00000000000000E+00 0.18051961149822E+00
 0.00000000000000E+00 -.13329549212705E+02 0.99813300480808E-01 0.95191819716759E+00 0.80149638990630E+02
 0.30056114621486E+02 0.84040834851185E+01 0.31515313069195E+01 0.31127678639366E+03 0.38849264389040E+03
 0.29605666767075E+03 0.30185409673029E+03 0.29315000000664E+03 0.29315000013973E+03 0.29604998729297E+03
 0.30185262242302E+03 0.29315000000656E+03 0.29315000013976E+03 0.29605666767075E+03 0.30185409673029E+03
 0.29315000000664E+03 0.29315000013973E+03 0.29604998729297E+03 0.30185262242302E+03 0.29315000000656E+03
 0.29315000013976E+03 0.30056524325799E+03 0.29315000126275E+03 0.80661301401255E+02 0.80559614515480E+02
 0.15015832665856E+03 0.38900506832053E+03 0.23809595002868E+03 0.13569076633819E+03 0.14070094049682E+03
 0.13569076633819E+03 0.30965284952626E+03 0.13568701867903E+03 0.14057660628728E+03 0.13568701867903E+03
 0.30953990725241E+03 0.13569076633819E+03 0.14070094049682E+03 0.13569076633819E+03 0.30965284952626E+03
 0.13568701867903E+03 0.14057660628728E+03 0.13568701867903E+03 0.30953990725241E+03 0.20551513109414E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35203816942601E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18183338851011E+00 0.00000000000000E+00 0.00000000000000E+00 0.18183338851011E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19093025262012E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19093025262012E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951329424229E+00 0.19249675253707E+00 0.32825234221185E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    531.71980972
 0.10287702972473E+00 0.30763787365533E+03 0.43726183761361E+03 0.43281672814922E+03 0.43103376098680E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15779235882175E+00 0.00000000000000E+00 -.21534090368941E+02
 0.34554745582842E-02 0.10463469388183E+01 0.23151668070658E+04 0.86818755264969E+03 0.76456476367532E+01
 0.28671178637824E+01 0.32918988004290E+03 0.29315001047847E+03 0.32370903458759E+03 0.34695974782239E+03
 0.29315000084089E+03 0.29315000154742E+03 0.32037836708340E+03 0.34693334187386E+03 0.29315000066569E+03
 0.29315000154124E+03 0.32370903458759E+03 0.34695974782239E+03 0.29315000084089E+03 0.29315000154742E+03
 0.32037836708340E+03 0.34693334187386E+03 0.29315000066569E+03 0.29315000154124E+03 0.38929116046416E+03
 0.31200740588763E+03 0.18795336182907E+04 0.17747119310825E+04 0.62427977951728E+03 0.10888700604587E+04
 0.46146888204384E+03 0.11028614176724E+04 0.10640840390687E+04 0.10259447595146E+04 0.17324463099537E+04
 0.98857107117324E+03 0.10636666194541E+04 0.93123031648629E+03 0.17322713309299E+04 0.11028614176724E+04
 0.10640840390687E+04 0.10259447595146E+04 0.17324463099537E+04 0.98857107117324E+03 0.10636666194541E+04
 0.93123031648629E+03 0.17322713309299E+04 0.17113708376355E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47674923778382E+03 0.12948631515983E+01
 0.12948631515983E+01 0.19468945612019E+01 0.14111066445532E-02 0.31191508356631E+03 0.32839628724549E+03
 0.32838078275481E+03 0.32837996432825E+03 0.23000000000000E+00 0.00000000000000E+00 0.17933453849702E+00
 0.00000000000000E+00 -.13316428024173E+02 0.12749200815358E+00 0.97454748440466E+00 0.62749031220553E+02
 0.23530886707708E+02 0.82089381256647E+01 0.30783517971242E+01 0.31200618184793E+03 0.38929796561683E+03
 0.29615529395078E+03 0.30200108511367E+03 0.29315000000895E+03 0.29315000018196E+03 0.29614882277192E+03
 0.30199951265130E+03 0.29315000000884E+03 0.29315000018201E+03 0.29615529395078E+03 0.30200108511367E+03
 0.29315000000895E+03 0.29315000018196E+03 0.29614882277192E+03 0.30199951265130E+03 0.29315000000884E+03
 0.29315000018201E+03 0.30069159062068E+03 0.29315000160142E+03 0.78402464170792E+02 0.78330617049473E+02
 0.15178274648665E+03 0.39080980784172E+03 0.23826814762264E+03 0.13845669465354E+03 0.14217060551467E+03
 0.13845669465354E+03 0.31107221293856E+03 0.13845865615386E+03 0.14204377724467E+03 0.13845865615386E+03
 0.31095753245297E+03 0.13845669465354E+03 0.14217060551467E+03 0.13845669465354E+03 0.31107221293856E+03
 0.13845865615386E+03 0.14204377724467E+03 0.13845865615386E+03 0.31095753245297E+03 0.20585290158377E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35224127212261E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18174394801703E+00 0.00000000000000E+00 0.00000000000000E+00 0.18174394801703E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19082210132995E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19082210132995E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951338206597E+00 0.19241249503251E+00 0.32839628724549E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    543.47627607
 0.10280273601491E+00 0.30789042604035E+03 0.43770267429286E+03 0.43325432286332E+03 0.43146888118950E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15707465897875E+00 0.00000000000000E+00 -.21554691440236E+02
 0.34579713484454E-02 0.10556913401657E+01 0.23134951663485E+04 0.86756068738068E+03 0.75779725527957E+01
 0.28417397072984E+01 0.32972962591908E+03 0.29315001331735E+03 0.32417121397088E+03 0.34766931495746E+03
 0.29315000110260E+03 0.29315000203336E+03 0.32081186387391E+03 0.34764321900261E+03 0.29315000087462E+03
 0.29315000202539E+03 0.32417121397088E+03 0.34766931495746E+03 0.29315000110260E+03 0.29315000203336E+03
 0.32081186387391E+03 0.34764321900261E+03 0.29315000087462E+03 0.29315000202539E+03 0.39013868449415E+03
 0.31278266865018E+03 0.18845710583381E+04 0.17779606242199E+04 0.62287116772534E+03 0.10825980157059E+04
 0.45661249214193E+03 0.11063525926318E+04 0.10668449044202E+04 0.10281795335914E+04 0.17324095113155E+04
 0.99233352793641E+03 0.10664391587619E+04 0.93397202670548E+03 0.17322430184640E+04 0.11063525926318E+04
 0.10668449044202E+04 0.10281795335914E+04 0.17324095113155E+04 0.99233352793641E+03 0.10664391587619E+04
 0.93397202670548E+03 0.17322430184640E+04 0.17106870545051E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47718467400974E+03 0.12948633033914E+01
 0.12948633033914E+01 0.19939204266000E+01 0.10892427734657E-02 0.31271448493367E+03 0.32855271771198E+03
 0.32854121659158E+03 0.32854063453203E+03 0.23000000000000E+00 0.00000000000000E+00 0.17810756406568E+00
 0.00000000000000E+00 -.13311957805923E+02 0.16516502765284E+00 0.99792710896022E+00 0.48436403963286E+02
 0.18163651486232E+02 0.80166175747401E+01 0.30062315905275E+01 0.31278156190091E+03 0.39014483760442E+03
 0.29625870012879E+03 0.30215466540733E+03 0.29315000001226E+03 0.29315000024003E+03 0.29625243080435E+03
 0.30215299295951E+03 0.29315000001211E+03 0.29315000024009E+03 0.29625870012879E+03 0.30215466540733E+03
 0.29315000001226E+03 0.29315000024003E+03 0.29625243080435E+03 0.30215299295951E+03 0.29315000001211E+03
 0.29315000024009E+03 0.30082359426608E+03 0.29315000205269E+03 0.75896222848001E+02 0.75851177934987E+02
 0.15344756145512E+03 0.39272178421781E+03 0.23850698495541E+03 0.14136785076225E+03 0.14367574084243E+03
 0.14136785076225E+03 0.31257207268305E+03 0.14137471682355E+03 0.14354647600324E+03 0.14137471682355E+03
 0.31245572588629E+03 0.14136785076225E+03 0.14367574084243E+03 0.14136785076225E+03 0.31257207268305E+03
 0.14137471682355E+03 0.14354647600324E+03 0.14137471682355E+03 0.31245572588629E+03 0.20617788525037E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35245666333243E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18172691806728E+00 0.00000000000000E+00 0.00000000000000E+00 0.18172691806728E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19070577878527E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19070577878527E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951318100370E+00 0.19232067670833E+00 0.32855271771198E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    550.48101964
 0.10274295987700E+00 0.30804757225751E+03 0.43796638118428E+03 0.43351696686000E+03 0.43173050060409E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15665927024437E+00 0.00000000000000E+00 -.21548440105448E+02
 0.34599829523646E-02 0.10610705698635E+01 0.23121501204313E+04 0.86705629516174E+03 0.75395550750492E+01
 0.28273331531434E+01 0.33005088239020E+03 0.29315001523835E+03 0.32444670476048E+03 0.34808804487304E+03
 0.29315000128303E+03 0.29315000236875E+03 0.32107099808480E+03 0.34806213599203E+03 0.29315000101884E+03
 0.29315000235956E+03 0.32444670476048E+03 0.34808804487304E+03 0.29315000128303E+03 0.29315000236875E+03
 0.32107099808480E+03 0.34806213599203E+03 0.29315000101884E+03 0.29315000235956E+03 0.39062374240109E+03
 0.31323909914170E+03 0.18875479358792E+04 0.17799159203941E+04 0.62222731002171E+03 0.10793037756781E+04
 0.45396532910625E+03 0.11084170303648E+04 0.10684897456814E+04 0.10295318245264E+04 0.17324356239438E+04
 0.99455610582471E+03 0.10680906128349E+04 0.93561682164940E+03 0.17322738303436E+04 0.11084170303648E+04
 0.10684897456814E+04 0.10295318245264E+04 0.17324356239438E+04 0.99455610582471E+03 0.10680906128349E+04
 0.93561682164940E+03 0.17322738303436E+04 0.17104209890098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47744626105619E+03 0.12948632573302E+01
 0.12948632573302E+01 0.20219394008538E+01 0.93348908699387E-03 0.31318069559334E+03 0.32864801964647E+03
 0.32863839392766E+03 0.32863791884447E+03 0.23000000000000E+00 0.00000000000000E+00 0.17738958306115E+00
 0.00000000000000E+00 -.13290319384753E+02 0.19272299063439E+00 0.10115834452255E+01 0.41510356256231E+02
 0.15566383596087E+02 0.79083935564172E+01 0.29656475836564E+01 0.31323807606136E+03 0.39062948494812E+03
 0.29632323973910E+03 0.30224714042753E+03 0.29315000001461E+03 0.29315000028021E+03 0.29631708886637E+03
 0.30224540813627E+03 0.29315000001443E+03 0.29315000028029E+03 0.29632323973910E+03 0.30224714042753E+03
 0.29315000001461E+03 0.29315000028021E+03 0.29631708886637E+03 0.30224540813627E+03 0.29315000001443E+03
 0.29315000028029E+03 0.30090315441402E+03 0.29315000235954E+03 0.74396898476018E+02 0.74359858190470E+02
 0.15440924125861E+03 0.39384047252965E+03 0.23865918506475E+03 0.14308505302795E+03 0.14454281185537E+03
 0.14308505302795E+03 0.31344412406270E+03 0.14309434801215E+03 0.14441219734562E+03 0.14309434801215E+03
 0.31332688858184E+03 0.14308505302795E+03 0.14454281185537E+03 0.14308505302795E+03 0.31344412406269E+03
 0.14309434801215E+03 0.14441219734562E+03 0.14309434801215E+03 0.31332688858184E+03 0.20635545211638E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35258530422346E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18156271544226E+00 0.00000000000000E+00 0.00000000000000E+00 0.18156271544226E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19062193498504E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19062193498504E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951362692124E+00 0.19226542589035E+00 0.32864801964647E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    560.65454981
 0.10266098644781E+00 0.30826896112030E+03 0.43834739060491E+03 0.43389606397609E+03 0.43210786125899E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15607159083495E+00 0.00000000000000E+00 -.21559489557648E+02
 0.34627453379871E-02 0.10686504179281E+01 0.23103056156738E+04 0.86636460587768E+03 0.74860776412838E+01
 0.28072791154814E+01 0.33051293515336E+03 0.29315001846168E+03 0.32484292675138E+03 0.34869186202336E+03
 0.29315000159230E+03 0.29315000294427E+03 0.32144346853709E+03 0.34866621316578E+03 0.29315000126643E+03
 0.29315000293304E+03 0.32484292675138E+03 0.34869186202336E+03 0.29315000159230E+03 0.29315000294427E+03
 0.32144346853709E+03 0.34866621316578E+03 0.29315000126643E+03 0.29315000293304E+03 0.39133232375866E+03
 0.31390572694879E+03 0.18918154183270E+04 0.17826798912811E+04 0.62113470548382E+03 0.10742997770974E+04
 0.45005939808617E+03 0.11113823312562E+04 0.10708271720310E+04 0.10314425933299E+04 0.17324482417339E+04
 0.99775151880192E+03 0.10704371765958E+04 0.93795416415871E+03 0.17322929208965E+04 0.11113823312562E+04
 0.10708271720310E+04 0.10314425933299E+04 0.17324482417339E+04 0.99775151880192E+03 0.10704371765958E+04
 0.93795416415871E+03 0.17322929208965E+04 0.17099470686585E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47782370475382E+03 0.12948633387449E+01
 0.12948633387449E+01 0.20626335215307E+01 0.74601442384115E-03 0.31386501012329E+03 0.32878918516067E+03
 0.32878176272745E+03 0.32878140997713E+03 0.23000000000000E+00 0.00000000000000E+00 0.17636436519481E+00
 0.00000000000000E+00 -.13279585640046E+02 0.24115458979263E+00 0.10310502036390E+01 0.33173741403302E+02
 0.12440153026238E+02 0.77590790164872E+01 0.29096546311827E+01 0.31390482784861E+03 0.39133750464600E+03
 0.29641551383101E+03 0.30237993466898E+03 0.29315000001877E+03 0.29315000034938E+03 0.29640952250396E+03
 0.30237811834496E+03 0.29315000001854E+03 0.29315000034948E+03 0.29641551383101E+03 0.30237993466898E+03
 0.29315000001877E+03 0.29315000034938E+03 0.29640952250396E+03 0.30237811834496E+03 0.29315000001854E+03
 0.29315000034948E+03 0.30101738120564E+03 0.29315000287723E+03 0.72143236638884E+02 0.72119793939397E+02
 0.15577878766131E+03 0.39547936469546E+03 0.23892168309585E+03 0.14556979732839E+03 0.14577791400232E+03
 0.14556979732839E+03 0.31472066592803E+03 0.14558208823587E+03 0.14564547390253E+03 0.14558208823587E+03
 0.31460225258348E+03 0.14556979732839E+03 0.14577791400232E+03 0.14556979732839E+03 0.31472066592803E+03
 0.14558208823587E+03 0.14564547390253E+03 0.14558208823587E+03 0.31460225258348E+03 0.20660055825245E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35277389900094E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18149199309230E+00 0.00000000000000E+00 0.00000000000000E+00 0.18149199309230E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19051488142906E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19051488142906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951364534306E+00 0.19218291689179E+00 0.32878918516067E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    570.36042182
 0.10259492764522E+00 0.30847253823575E+03 0.43870779638074E+03 0.43425397075199E+03 0.43246374585422E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15552775615093E+00 0.00000000000000E+00 -.21578233186835E+02
 0.34649745872128E-02 0.10756338215933E+01 0.23088192420006E+04 0.86580721575023E+03 0.74374753186450E+01
 0.27890532444919E+01 0.33094885495594E+03 0.29315002214079E+03 0.32521678585156E+03 0.34926243170153E+03
 0.29315000195488E+03 0.29315000361994E+03 0.32179484570289E+03 0.34923702025291E+03 0.29315000155724E+03
 0.29315000360635E+03 0.32521678585156E+03 0.34926243170153E+03 0.29315000195488E+03 0.29315000361994E+03
 0.32179484570289E+03 0.34923702025291E+03 0.29315000155724E+03 0.29315000360635E+03 0.39200660480656E+03
 0.31454430088872E+03 0.18958080303762E+04 0.17852148724306E+04 0.61998240052854E+03 0.10694091811703E+04
 0.44632686863916E+03 0.11141659703580E+04 0.10729937657663E+04 0.10331973707293E+04 0.17324249137053E+04
 0.10007538415919E+04 0.10726120169840E+04 0.94011535298498E+03 0.17322754031846E+04 0.11141659703580E+04
 0.10729937657663E+04 0.10331973707293E+04 0.17324249137053E+04 0.10007538415919E+04 0.10726120169840E+04
 0.94011535298498E+03 0.17322754031846E+04 0.17094186464183E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47817985239161E+03 0.12948634768520E+01
 0.12948634768520E+01 0.21014570095933E+01 0.60214274053294E-03 0.31451902750093E+03 0.32892704368579E+03
 0.32892125989756E+03 0.32892099505530E+03 0.23000000000000E+00 0.00000000000000E+00 0.17540571147490E+00
 0.00000000000000E+00 -.13278607916943E+02 0.29877433412140E+00 0.10492155386972E+01 0.26776061683897E+02
 0.10041023131461E+02 0.76247441111421E+01 0.28592790416783E+01 0.31454349743855E+03 0.39201133903197E+03
 0.29650273494899E+03 0.30250532313795E+03 0.29315000002386E+03 0.29315000043092E+03 0.29649688399697E+03
 0.30250342913180E+03 0.29315000002356E+03 0.29315000043104E+03 0.29650273494899E+03 0.30250532313795E+03
 0.29315000002386E+03 0.29315000043092E+03 0.29649688399697E+03 0.30250342913180E+03 0.29315000002356E+03
 0.29315000043104E+03 0.30112522660710E+03 0.29315000347214E+03 0.69923200234561E+02 0.69910453171558E+02
 0.15706064538703E+03 0.39706289990717E+03 0.23921695129321E+03 0.14793995856290E+03 0.14693351454563E+03
 0.14793995856290E+03 0.31595258704701E+03 0.14795460765735E+03 0.14679942408078E+03 0.14795460765735E+03
 0.31583312269705E+03 0.14793995856290E+03 0.14693351454563E+03 0.14793995856290E+03 0.31595258704701E+03
 0.14795460765735E+03 0.14679942408078E+03 0.14795460765735E+03 0.31583312269705E+03 0.20682481807453E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35295668265287E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18149444747595E+00 0.00000000000000E+00 0.00000000000000E+00 0.18149444747595E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19044358936726E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19044358936726E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951337682139E+00 0.19210208449477E+00 0.32892704368579E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    580.16239381
 0.10251496087047E+00 0.30868463426108E+03 0.43907164842143E+03 0.43461610853654E+03 0.43282428253425E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15499482436981E+00 0.00000000000000E+00 -.21575950438114E+02
 0.34676770920888E-02 0.10824400672988E+01 0.23070198832098E+04 0.86513245620368E+03 0.73907094181796E+01
 0.27715160318174E+01 0.33138699048163E+03 0.29315002641209E+03 0.32559291098755E+03 0.34983304587829E+03
 0.29315000238472E+03 0.29315000442177E+03 0.32214896174153E+03 0.34980787165581E+03 0.29315000190252E+03
 0.29315000440542E+03 0.32559291098755E+03 0.34983304587829E+03 0.29315000238472E+03 0.29315000442177E+03
 0.32214896174153E+03 0.34980787165581E+03 0.29315000190252E+03 0.29315000440542E+03 0.39267025579793E+03
 0.31518458134672E+03 0.18998011602941E+04 0.17877900004740E+04 0.61895118147100E+03 0.10648078949565E+04
 0.44276195757817E+03 0.11169525811336E+04 0.10751670632469E+04 0.10349897205990E+04 0.17324400478447E+04
 0.10037580269309E+04 0.10747932222194E+04 0.94230849849280E+03 0.17322960170620E+04 0.11169525811336E+04
 0.10751670632469E+04 0.10349897205990E+04 0.17324400478447E+04 0.10037580269309E+04 0.10747932222194E+04
 0.94230849849280E+03 0.17322960170620E+04 0.17089942218972E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47854043728235E+03 0.12948634600322E+01
 0.12948634600322E+01 0.21406648975522E+01 0.48507439543055E-03 0.31517168365295E+03 0.32906915096076E+03
 0.32906465675706E+03 0.32906445865246E+03 0.23000000000000E+00 0.00000000000000E+00 0.17445694845260E+00
 0.00000000000000E+00 -.13254398433894E+02 0.37088082134897E+00 0.10671569793631E+01 0.21570271471311E+02
 0.80888518017416E+01 0.74965540728361E+01 0.28112077773136E+01 0.31518389128345E+03 0.39267451542550E+03
 0.29659307981720E+03 0.30263241488776E+03 0.29315000003009E+03 0.29315000052798E+03 0.29658736455184E+03
 0.30263044332599E+03 0.29315000002971E+03 0.29315000052813E+03 0.29659307981720E+03 0.30263241488776E+03
 0.29315000003009E+03 0.29315000052798E+03 0.29658736455184E+03 0.30263044332599E+03 0.29315000002971E+03
 0.29315000052813E+03 0.30123462223179E+03 0.29315000416644E+03 0.67660349596637E+02 0.67655008673555E+02
 0.15832385252106E+03 0.39864803143885E+03 0.23953255965518E+03 0.15031551332172E+03 0.14807051890358E+03
 0.15031551332172E+03 0.31717959212247E+03 0.15033209980687E+03 0.14793488335118E+03 0.15033209980687E+03
 0.31705918090097E+03 0.15031551332172E+03 0.14807051890358E+03 0.15031551332172E+03 0.31717959212247E+03
 0.15033209980687E+03 0.14793488335118E+03 0.15033209980687E+03 0.31705918090097E+03 0.20703597493504E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35313917596603E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18132104967740E+00 0.00000000000000E+00 0.00000000000000E+00 0.18132104967740E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19029526866367E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19029526866367E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951379777280E+00 0.19201962211027E+00 0.32906915096076E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    590.36487491
 0.10242539104811E+00 0.30890235254274E+03 0.43944886493335E+03 0.43499177240616E+03 0.43319832970043E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15445661413105E+00 0.00000000000000E+00 -.21585259132649E+02
 0.34707091748629E-02 0.10892780591603E+01 0.23050044232865E+04 0.86437665873244E+03 0.73443139083947E+01
 0.27541177156480E+01 0.33183931484820E+03 0.29315003157186E+03 0.32598137091763E+03 0.35042197183798E+03
 0.29315000291564E+03 0.29315000541316E+03 0.32251479487105E+03 0.35039703659441E+03 0.29315000232967E+03
 0.29315000539345E+03 0.32598137091763E+03 0.35042197183798E+03 0.29315000291564E+03 0.29315000541316E+03
 0.32251479487105E+03 0.35039703659441E+03 0.29315000232967E+03 0.29315000539345E+03 0.39335678222791E+03
 0.31585121952501E+03 0.19039098214859E+04 0.17904228765555E+04 0.61782874214839E+03 0.10600095400169E+04
 0.43909165415773E+03 0.11198238350012E+04 0.10773906821487E+04 0.10368222157102E+04 0.17324536407467E+04
 0.10068548587792E+04 0.10770246330051E+04 0.94455622008762E+03 0.17323149554243E+04 0.11198238350012E+04
 0.10773906821487E+04 0.10368222157102E+04 0.17324536407467E+04 0.10068548587792E+04 0.10770246330051E+04
 0.94455622008762E+03 0.17323149554243E+04 0.17085377217384E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47891452066968E+03 0.12948635286207E+01
 0.12948635286207E+01 0.21814748219617E+01 0.38730235569195E-03 0.31585407186814E+03 0.32921969914468E+03
 0.32921624811872E+03 0.32921610212500E+03 0.23000000000000E+00 0.00000000000000E+00 0.17349009444447E+00
 0.00000000000000E+00 -.13241968628055E+02 0.46450733144746E+00 0.10854022695796E+01 0.17222548404287E+02
 0.64584556516076E+01 0.73705392223828E+01 0.27639522083936E+01 0.31585064945010E+03 0.39336057420562E+03
 0.29668709279526E+03 0.30276390738513E+03 0.29315000003805E+03 0.29315000064840E+03 0.29668150971116E+03
 0.30276185711733E+03 0.29315000003757E+03 0.29315000064858E+03 0.29668709279526E+03 0.30276390738513E+03
 0.29315000003805E+03 0.29315000064840E+03 0.29668150971116E+03 0.30276185711733E+03 0.29315000003757E+03
 0.29315000064858E+03 0.30134782686282E+03 0.29315000500979E+03 0.65250048814499E+02 0.65250593203030E+02
 0.15961170358500E+03 0.40030626541499E+03 0.23989650331207E+03 0.15277904038761E+03 0.14922890879886E+03
 0.15277904038761E+03 0.31846077624636E+03 0.15279725653937E+03 0.14909176963880E+03 0.15279725653937E+03
 0.31833946911156E+03 0.15277904038761E+03 0.14922890879886E+03 0.15277904038761E+03 0.31846077624636E+03
 0.15279725653937E+03 0.14909176963880E+03 0.15279725653937E+03 0.31833946911156E+03 0.20724449262375E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35333243722831E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18123803200276E+00 0.00000000000000E+00 0.00000000000000E+00 0.18123803200276E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19017874354192E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19017874354192E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951384582620E+00 0.19193188879475E+00 0.32921969914468E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    600.56735602
 0.10233686670141E+00 0.30911777718310E+03 0.43982431896827E+03 0.43536561965371E+03 0.43357051953978E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15393473426506E+00 0.00000000000000E+00 -.21594353133854E+02
 0.34737110799631E-02 0.10958732956934E+01 0.23030124889042E+04 0.86362968333909E+03 0.73001140108428E+01
 0.27375427540660E+01 0.33228806712390E+03 0.29315003757013E+03 0.32636695066479E+03 0.35100551491904E+03
 0.29315000354688E+03 0.29315000659307E+03 0.32287813747718E+03 0.35098081174240E+03 0.29315000283839E+03
 0.29315000656943E+03 0.32636695066479E+03 0.35100551491904E+03 0.29315000354688E+03 0.29315000659307E+03
 0.32287813747718E+03 0.35098081174240E+03 0.29315000283839E+03 0.29315000656943E+03 0.39403569570989E+03
 0.31651672295663E+03 0.19079627391533E+04 0.17930043262026E+04 0.61669653181617E+03 0.10552766570689E+04
 0.43549664259362E+03 0.11226621356315E+04 0.10795761135962E+04 0.10386227377029E+04 0.17324668698971E+04
 0.10099169214199E+04 0.10792174415537E+04 0.94676796200402E+03 0.17323331866915E+04 0.11226621356315E+04
 0.10795761135962E+04 0.10386227377029E+04 0.17324668698971E+04 0.10099169214199E+04 0.10792174415537E+04
 0.94676796200402E+03 0.17323331866915E+04 0.17080833911232E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47928676988841E+03 0.12948635956272E+01
 0.12948635956272E+01 0.22222847463713E+01 0.30921581334420E-03 0.31653734292059E+03 0.32937319771088E+03
 0.32937055167803E+03 0.32937044440219E+03 0.23000000000000E+00 0.00000000000000E+00 0.17254422192996E+00
 0.00000000000000E+00 -.13229417556337E+02 0.58180975667318E+00 0.11032115169793E+01 0.13750199112755E+02
 0.51563246672833E+01 0.72515559136882E+01 0.27193334676331E+01 0.31651626556687E+03 0.39403906246197E+03
 0.29678161929261E+03 0.30289490384613E+03 0.29315000004784E+03 0.29315000079222E+03 0.29677616023737E+03
 0.30289277662186E+03 0.29315000004724E+03 0.29315000079244E+03 0.29678161929261E+03 0.30289490384613E+03
 0.29315000004784E+03 0.29315000079222E+03 0.29677616023737E+03 0.30289277662186E+03 0.29315000004724E+03
 0.29315000079244E+03 0.30146064375656E+03 0.29315000599564E+03 0.62797329919943E+02 0.62802607789544E+02
 0.16087603109224E+03 0.40197364256872E+03 0.24029323132101E+03 0.15523493471134E+03 0.15036528403931E+03
 0.15523493471134E+03 0.31974604425430E+03 0.15525444921028E+03 0.15022673282129E+03 0.15525444921028E+03
 0.31962391968913E+03 0.15523493471134E+03 0.15036528403931E+03 0.15523493471134E+03 0.31974604425430E+03
 0.15525444921028E+03 0.15022673282129E+03 0.15525444921028E+03 0.31962391968913E+03 0.20744528803091E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35352738631888E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18115426979458E+00 0.00000000000000E+00 0.00000000000000E+00 0.18115426979458E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19006102441382E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19006102441382E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951389105371E+00 0.19184251443359E+00 0.32937319771088E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    610.85702782
 0.10225111096249E+00 0.30933171209585E+03 0.44020065920334E+03 0.43574016079260E+03 0.43394328800342E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15342441546844E+00 0.00000000000000E+00 -.21606422008925E+02
 0.34766240458042E-02 0.11022885361125E+01 0.23010828592913E+04 0.86290607223424E+03 0.72576278695720E+01
 0.27216104510895E+01 0.33273671200253E+03 0.29315004462054E+03 0.32675258171006E+03 0.35158873812265E+03
 0.29315000430616E+03 0.29315000801366E+03 0.32324164566024E+03 0.35156426133343E+03 0.29315000345133E+03
 0.29315000798539E+03 0.32675258171006E+03 0.35158873812265E+03 0.29315000430616E+03 0.29315000801366E+03
 0.32324164566024E+03 0.35156426133343E+03 0.29315000345133E+03 0.29315000798539E+03 0.39471517850535E+03
 0.31718767541130E+03 0.19119904971995E+04 0.17955477546669E+04 0.61550904704731E+03 0.10504994288630E+04
 0.43191283658050E+03 0.11254895198550E+04 0.10817352938824E+04 0.10404005884727E+04 0.17324676884740E+04
 0.10129685954957E+04 0.10813836639929E+04 0.94895763723749E+03 0.17323387298715E+04 0.11254895198550E+04
 0.10817352938824E+04 0.10404005884727E+04 0.17324676884740E+04 0.10129685954957E+04 0.10813836639929E+04
 0.94895763723749E+03 0.17323387298715E+04 0.17076011045244E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47965965139664E+03 0.12948636845533E+01
 0.12948636845533E+01 0.22634434335789E+01 0.24635534958017E-03 0.31722146457328E+03 0.32953086764878E+03
 0.32952884599059E+03 0.32952876755592E+03 0.23000000000000E+00 0.00000000000000E+00 0.17161137634522E+00
 0.00000000000000E+00 -.13220037774897E+02 0.73026532887976E+00 0.11207348579443E+01 0.10954922387281E+02
 0.41080958952304E+01 0.71381736217911E+01 0.26768151081717E+01 0.31718732838336E+03 0.39471814538435E+03
 0.29687700318235E+03 0.30302628396393E+03 0.29315000006006E+03 0.29315000096602E+03 0.29687166121801E+03
 0.30302408096147E+03 0.29315000005930E+03 0.29315000096630E+03 0.29687700318235E+03 0.30302628396393E+03
 0.29315000006006E+03 0.29315000096602E+03 0.29687166121801E+03 0.30302408096147E+03 0.29315000005930E+03
 0.29315000096630E+03 0.30157381593081E+03 0.29315000716093E+03 0.60276874808014E+02 0.60286510952487E+02
 0.16213104595372E+03 0.40366997270494E+03 0.24072827152145E+03 0.15770772574805E+03 0.15149269877498E+03
 0.15770772574805E+03 0.32105148809664E+03 0.15772827229414E+03 0.15135280106039E+03 0.15772827229414E+03
 0.32092860313139E+03 0.15770772574805E+03 0.15149269877498E+03 0.15770772574805E+03 0.32105148809664E+03
 0.15772827229414E+03 0.15135280106039E+03 0.15772827229414E+03 0.32092860313139E+03 0.20764221101582E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35372610738102E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18109517698341E+00 0.00000000000000E+00 0.00000000000000E+00 0.18109517698341E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18995086982038E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18995086982038E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951383184397E+00 0.19175067846438E+00 0.32953086764878E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    620.04215417
 0.10218206483926E+00 0.30952116109917E+03 0.44053483896987E+03 0.43607242292753E+03 0.43427384356837E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15298212023676E+00 0.00000000000000E+00 -.21616248989612E+02
 0.34789729412852E-02 0.11078197234363E+01 0.22995292389497E+04 0.86232346460612E+03 0.72213915592558E+01
 0.27080218347209E+01 0.33313430755318E+03 0.29315005185533E+03 0.32709451248673E+03 0.35210477932891E+03
 0.29315000510196E+03 0.29315000950378E+03 0.32356418969101E+03 0.35208049949160E+03 0.29315000409477E+03
 0.29315000947072E+03 0.32709451248673E+03 0.35210477932891E+03 0.29315000510196E+03 0.29315000950378E+03
 0.32356418969101E+03 0.35208049949161E+03 0.29315000409477E+03 0.29315000947072E+03 0.39531407110852E+03
 0.31778474657562E+03 0.19155336870891E+04 0.17977716743070E+04 0.61445631070001E+03 0.10463160474915E+04
 0.42878745523799E+03 0.11279833884079E+04 0.10836299917098E+04 0.10419613478984E+04 0.17324657152934E+04
 0.10156607060878E+04 0.10832843331420E+04 0.95088161979226E+03 0.17323407126914E+04 0.11279833884079E+04
 0.10836299917098E+04 0.10419613478984E+04 0.17324657152934E+04 0.10156607060878E+04 0.10832843331420E+04
 0.95088161979226E+03 0.17323407126914E+04 0.17071755340423E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47999036575836E+03 0.12948637569607E+01
 0.12948637569607E+01 0.23001839390070E+01 0.20108980587841E-03 0.31782531206825E+03 0.32967432698845E+03
 0.32967273851104E+03 0.32967267929841E+03 0.23000000000000E+00 0.00000000000000E+00 0.17079634750485E+00
 0.00000000000000E+00 -.13211097276310E+02 0.89464885344324E+00 0.11360093066203E+01 0.89420558347673E+01
 0.33532709380377E+01 0.70421958283077E+01 0.26408234356154E+01 0.31778449863431E+03 0.39531669582995E+03
 0.29696281539224E+03 0.30314332553617E+03 0.29315000007328E+03 0.29315000114895E+03 0.29695757214943E+03
 0.30314105615805E+03 0.29315000007236E+03 0.29315000114928E+03 0.29696281539224E+03 0.30314332553617E+03
 0.29315000007328E+03 0.29315000114895E+03 0.29695757214943E+03 0.30314105615805E+03 0.29315000007236E+03
 0.29315000114928E+03 0.30167467479502E+03 0.29315000836279E+03 0.58002529704256E+02 0.58014569533625E+02
 0.16323686773291E+03 0.40519874295638E+03 0.24114569088481E+03 0.15991079400901E+03 0.15248553286420E+03
 0.15991079400901E+03 0.32222614009783E+03 0.15993206359091E+03 0.15234449569098E+03 0.15993206359091E+03
 0.32210262956972E+03 0.15991079400901E+03 0.15248553286420E+03 0.15991079400901E+03 0.32222614009783E+03
 0.15993206359091E+03 0.15234449569098E+03 0.15993206359091E+03 0.32210262956972E+03 0.20781769010040E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35390575424082E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18103574152142E+00 0.00000000000000E+00 0.00000000000000E+00 0.18103574152142E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18986202485731E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18986202485731E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951379026917E+00 0.19166720978205E+00 0.32967432698845E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    630.00399683
 0.10211393082594E+00 0.30972586413684E+03 0.44089570561595E+03 0.43643094957620E+03 0.43463042702985E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15251615026591E+00 0.00000000000000E+00 -.21724393732320E+02
 0.34812938929903E-02 0.11136146714780E+01 0.22979961605966E+04 0.86174856022372E+03 0.71838134005384E+01
 0.26939300252019E+01 0.33356274738947E+03 0.29315006079208E+03 0.32746319080947E+03 0.35265968855375E+03
 0.29315000610470E+03 0.29315001138273E+03 0.32391225086680E+03 0.35263561737443E+03 0.29315000490673E+03
 0.29315001134373E+03 0.32746319080947E+03 0.35265968855375E+03 0.29315000610470E+03 0.29315001138273E+03
 0.32391225086680E+03 0.35263561737443E+03 0.29315000490673E+03 0.29315001134373E+03 0.39595479988223E+03
 0.31845845047881E+03 0.19193269942209E+04 0.18001442130853E+04 0.61333904433578E+03 0.10418924666086E+04
 0.42548672705113E+03 0.11306595601826E+04 0.10856567247038E+04 0.10436330101862E+04 0.17324688569291E+04
 0.10185496543573E+04 0.10853172304289E+04 0.95294206350829E+03 0.17323478849170E+04 0.11306595601826E+04
 0.10856567247038E+04 0.10436330101862E+04 0.17324688569291E+04 0.10185496543573E+04 0.10853172304289E+04
 0.95294206350829E+03 0.17323478849170E+04 0.17067332603099E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48034716291482E+03 0.12948645537954E+01
 0.12948645537954E+01 0.23400313096101E+01 0.00000000000000E+00 0.32982824216829E+03 0.32982824216829E+03
 0.32982824216829E+03 0.32982824216829E+03 0.00000000000000E+00 0.00000000000000E+00 0.16993771249666E+00
 0.00000000000000E+00 -.13313875991476E+02 0.10000000000000E-02 0.11521936969328E+01 0.80000000000000E+04
 0.30000000000000E+04 0.69432770039418E+01 0.26037288764782E+01 0.31845831182349E+03 0.39595706067733E+03
 0.30327012475007E+03 0.30327012475007E+03 0.29315000138070E+03 0.29315000138038E+03 0.30326780466094E+03
 0.30326780466094E+03 0.29315000138110E+03 0.29315000138078E+03 0.30327012475007E+03 0.30327012475007E+03
 0.29315000138070E+03 0.29315000138038E+03 0.30326780466094E+03 0.30326780466094E+03 0.29315000138110E+03
 0.29315000138078E+03 0.30178376806038E+03 0.29315000985442E+03 0.55942398997932E+02 0.69498837090640E+02
 0.16394089175032E+03 0.40639046549535E+03 0.24162986928628E+03 0.12395206157305E+03 0.15347296092027E+03
 0.12395206157305E+03 0.32343780701950E+03 0.12396227054318E+03 0.15335437961099E+03 0.12396227054318E+03
 0.32333714861434E+03 0.12395206157305E+03 0.15347296092027E+03 0.12395206157305E+03 0.32343780701950E+03
 0.12396227054318E+03 0.15335437961099E+03 0.12396227054318E+03 0.32333714861434E+03 0.21123269571787E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35413105778699E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18177955941884E+00 0.00000000000000E+00 0.00000000000000E+00 0.18177955941884E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19033652739563E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19033652739563E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951036654768E+00 0.19157391957168E+00 0.32982824216829E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    640.21173095
 0.10223390995692E+00 0.30991719349898E+03 0.44126240382196E+03 0.43678642568682E+03 0.43498020354038E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15205474992605E+00 0.00000000000000E+00 -.21696500887971E+02
 0.34772080112032E-02 0.11193211953970E+01 0.23006964133940E+04 0.86276115502274E+03 0.71471888792053E+01
 0.26801958297020E+01 0.33399826462402E+03 0.29315007116747E+03 0.32783818842861E+03 0.35322359710807E+03
 0.29315000729117E+03 0.29315001360740E+03 0.32426647530448E+03 0.35319973420832E+03 0.29315000586886E+03
 0.29315001356147E+03 0.32783818842861E+03 0.35322359710807E+03 0.29315000729117E+03 0.29315001360740E+03
 0.32426647530448E+03 0.35319973420832E+03 0.29315000586886E+03 0.29315001356147E+03 0.39660406005508E+03
 0.31923675032705E+03 0.19230272583094E+04 0.18022818024578E+04 0.61216525986744E+03 0.10373786312857E+04
 0.42215254511889E+03 0.11333159810305E+04 0.10876714005071E+04 0.10451898852659E+04 0.17324358131586E+04
 0.10214168141667E+04 0.10873377912422E+04 0.95488725766743E+03 0.17323185980220E+04 0.11333159810305E+04
 0.10876714005071E+04 0.10451898852659E+04 0.17324358131586E+04 0.10214168141667E+04 0.10873377912422E+04
 0.95488725766742E+03 0.17323185980220E+04 0.17061622921118E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48069886013931E+03 0.12948643482746E+01
 0.12948643482746E+01 0.23808622461060E+01 0.00000000000000E+00 0.32994766792317E+03 0.32994766792317E+03
 0.32994766792317E+03 0.32994766792317E+03 0.00000000000000E+00 0.00000000000000E+00 0.16906755894196E+00
 0.00000000000000E+00 -.13261095857509E+02 0.10000000000000E-02 0.11685735402347E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68459533992127E+01 0.25672325247048E+01 0.31923608779901E+03 0.39660594553370E+03
 0.30339791000775E+03 0.30339791000775E+03 0.29315000167314E+03 0.29315000165526E+03 0.30339560117199E+03
 0.30339560117199E+03 0.29315000167362E+03 0.29315000165574E+03 0.30339791000775E+03 0.30339791000775E+03
 0.29315000167314E+03 0.29315000165526E+03 0.30339560117199E+03 0.30339560117199E+03 0.29315000167362E+03
 0.29315000165574E+03 0.30189312353294E+03 0.29315001159397E+03 0.52277344568967E+02 0.64857352443593E+02
 0.16493850044020E+03 0.40747082926421E+03 0.24170763632181E+03 0.12635436594670E+03 0.15431208676411E+03
 0.12635436594670E+03 0.32418475524483E+03 0.12636446930900E+03 0.15419010748598E+03 0.12636446930900E+03
 0.32408060740663E+03 0.12635436594670E+03 0.15431208676411E+03 0.12635436594670E+03 0.32418475524482E+03
 0.12636446930900E+03 0.15419010748598E+03 0.12636446930900E+03 0.32408060740663E+03 0.21060169244274E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35428379898279E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18140101703697E+00 0.00000000000000E+00 0.00000000000000E+00 0.18140101703697E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19003749671418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19003749671418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951169599090E+00 0.19150609677196E+00 0.32994766792317E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    651.85332266
 0.10220984784482E+00 0.31015827836343E+03 0.44167632771230E+03 0.43719551444137E+03 0.43538636183301E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15154763606422E+00 0.00000000000000E+00 -.21679170568486E+02
 0.34780262260289E-02 0.11255543103452E+01 0.23001551685061E+04 0.86255818818978E+03 0.71076090478001E+01
 0.26653533929250E+01 0.33449077871827E+03 0.29315008485152E+03 0.32826256196104E+03 0.35386065922616E+03
 0.29315000889111E+03 0.29315001660938E+03 0.32466746712872E+03 0.35383702584733E+03 0.29315000716845E+03
 0.29315001655426E+03 0.32826256196104E+03 0.35386065922616E+03 0.29315000889111E+03 0.29315001660938E+03
 0.32466746712872E+03 0.35383702584733E+03 0.29315000716845E+03 0.29315001655426E+03 0.39733833186471E+03
 0.32003446105844E+03 0.19272595068282E+04 0.18049130226845E+04 0.61076160734153E+03 0.10322046365996E+04
 0.41838922122141E+03 0.11363317805704E+04 0.10899004288457E+04 0.10470880509667E+04 0.17323480048330E+04
 0.10246766435605E+04 0.10895731877916E+04 0.95722620053344E+03 0.17322348099418E+04 0.11363317805704E+04
 0.10899004288457E+04 0.10470880509667E+04 0.17323480048330E+04 0.10246766435605E+04 0.10895731877916E+04
 0.95722620053344E+03 0.17322348099418E+04 0.17055479555162E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48110555351110E+03 0.12948642205808E+01
 0.12948642205808E+01 0.24274286129432E+01 0.00000000000000E+00 0.33010406099507E+03 0.33010406099507E+03
 0.33010406099507E+03 0.33010406099507E+03 0.00000000000000E+00 0.00000000000000E+00 0.16810120261556E+00
 0.00000000000000E+00 -.13216907690258E+02 0.10000000000000E-02 0.11866626305622E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67415959633025E+01 0.25280984862384E+01 0.32003409203189E+03 0.39733984911333E+03
 0.30354209641302E+03 0.30354209641302E+03 0.29315000206204E+03 0.29315000202759E+03 0.30353974560851E+03
 0.30353974560851E+03 0.29315000206263E+03 0.29315000202817E+03 0.30354209641302E+03 0.30354209641302E+03
 0.29315000206204E+03 0.29315000202759E+03 0.30353974560851E+03 0.30353974560851E+03 0.29315000206263E+03
 0.29315000202817E+03 0.30201722180555E+03 0.29315001390018E+03 0.48735120993835E+02 0.60377717603939E+02
 0.16612023273244E+03 0.40895974827645E+03 0.24200891438034E+03 0.12890275996583E+03 0.15533583227886E+03
 0.12890275996583E+03 0.32527111410862E+03 0.12891306000368E+03 0.15521132950488E+03 0.12891306000368E+03
 0.32516476502495E+03 0.12890275996583E+03 0.15533583227886E+03 0.12890275996583E+03 0.32527111410862E+03
 0.12891306000368E+03 0.15521132950488E+03 0.12891306000368E+03 0.32516476502495E+03 0.21020956415384E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35447864622462E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18108534680485E+00 0.00000000000000E+00 0.00000000000000E+00 0.18108534680485E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18977343106560E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18977343106560E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951268712679E+00 0.19141650749649E+00 0.33010406099507E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    661.30715936
 0.10212443398390E+00 0.31035534225432E+03 0.44201132850576E+03 0.43752956414692E+03 0.43571921561090E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15114895605523E+00 0.00000000000000E+00 -.21669045822125E+02
 0.34809348255887E-02 0.11304219429411E+01 0.22982332048251E+04 0.86183745180942E+03 0.70770034587139E+01
 0.26538762970177E+01 0.33488789540891E+03 0.29315009759462E+03 0.32860491706662E+03 0.35437335478985E+03
 0.29315001041263E+03 0.29315001946590E+03 0.32499116096399E+03 0.35434990239782E+03 0.29315000840633E+03
 0.29315001940220E+03 0.32860491706662E+03 0.35437335478985E+03 0.29315001041263E+03 0.29315001946590E+03
 0.32499116096399E+03 0.35434990239782E+03 0.29315000840633E+03 0.29315001940220E+03 0.39792821055515E+03
 0.32066393996886E+03 0.19307044924491E+04 0.18070829425910E+04 0.60962993499150E+03 0.10280799727477E+04
 0.41540188808120E+03 0.11387764883594E+04 0.10916945846794E+04 0.10486345141545E+04 0.17322941052628E+04
 0.10273199215601E+04 0.10913722600953E+04 0.95913041408734E+03 0.17321839751616E+04 0.11387764883594E+04
 0.10916945846794E+04 0.10486345141545E+04 0.17322941052628E+04 0.10273199215601E+04 0.10913722600953E+04
 0.95913041408734E+03 0.17321839751616E+04 0.17050951171720E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48143831006594E+03 0.12948641459794E+01
 0.12948641459794E+01 0.24652439597629E+01 0.00000000000000E+00 0.33024285912256E+03 0.33024285912256E+03
 0.33024285912256E+03 0.33024285912256E+03 0.00000000000000E+00 0.00000000000000E+00 0.16733627155962E+00
 0.00000000000000E+00 -.13185347246863E+02 0.10000000000000E-02 0.12009129033864E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66615988365527E+01 0.24980995637073E+01 0.32066389805990E+03 0.39792945051658E+03
 0.30365887311013E+03 0.30365887311013E+03 0.29315000242365E+03 0.29315000238316E+03 0.30365648018626E+03
 0.30365648018626E+03 0.29315000242434E+03 0.29315000238384E+03 0.30365887311013E+03 0.30365887311013E+03
 0.29315000242365E+03 0.29315000238316E+03 0.30365648018626E+03 0.30365648018626E+03 0.29315000242434E+03
 0.29315000238384E+03 0.30211786655080E+03 0.29315001605827E+03 0.45999984237450E+02 0.56935230520265E+02
 0.16711742040831E+03 0.41032683612907E+03 0.24237382861872E+03 0.13095760705949E+03 0.15620959713286E+03
 0.13095760705949E+03 0.32628748834807E+03 0.13096811357959E+03 0.15608334027317E+03 0.13096811357959E+03
 0.32617971064562E+03 0.13095760705949E+03 0.15620959713286E+03 0.13095760705949E+03 0.32628748834807E+03
 0.13096811357959E+03 0.15608334027317E+03 0.13096811357959E+03 0.32617971064562E+03 0.21001620917597E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35464801571264E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18086021932970E+00 0.00000000000000E+00 0.00000000000000E+00 0.18086021932970E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18958105430237E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18958105430237E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951333665193E+00 0.19133680877712E+00 0.33024285912256E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    670.76099607
 0.10200299684128E+00 0.31055219701708E+03 0.44234615771556E+03 0.43786503139661E+03 0.43605409632843E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15076098566133E+00 0.00000000000000E+00 -.21664740425515E+02
 0.34850786437903E-02 0.11351290856960E+01 0.22955005661793E+04 0.86081271231724E+03 0.70476566064684E+01
 0.26428712274256E+01 0.33528270216773E+03 0.29315011191515E+03 0.32894541775763E+03 0.35488213272802E+03
 0.29315001215361E+03 0.29315002273598E+03 0.32531331021002E+03 0.35485885693494E+03 0.29315000982474E+03
 0.29315002266261E+03 0.32894541775763E+03 0.35488213272802E+03 0.29315001215361E+03 0.29315002273598E+03
 0.32531331021002E+03 0.35485885693494E+03 0.29315000982474E+03 0.29315002266261E+03 0.39851239504551E+03
 0.32128336447195E+03 0.19341572803867E+04 0.18092723340154E+04 0.60852188535084E+03 0.10240480239990E+04
 0.41248352922143E+03 0.11412204959371E+04 0.10934885402628E+04 0.10501826214371E+04 0.17322753740692E+04
 0.10299620038216E+04 0.10931709166182E+04 0.96103490415924E+03 0.17321681386502E+04 0.11412204959371E+04
 0.10934885402629E+04 0.10501826214371E+04 0.17322753740692E+04 0.10299620038216E+04 0.10931709166182E+04
 0.96103490415924E+03 0.17321681386502E+04 0.17046946812068E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48177282105351E+03 0.12948641142563E+01
 0.12948641142563E+01 0.25030593065825E+01 0.00000000000000E+00 0.33038957411493E+03 0.33038957411493E+03
 0.33038957411493E+03 0.33038957411493E+03 0.00000000000000E+00 0.00000000000000E+00 0.16658872204937E+00
 0.00000000000000E+00 -.13160004626955E+02 0.10000000000000E-02 0.12147856593592E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65855239056907E+01 0.24695714646340E+01 0.32128355249919E+03 0.39851336194319E+03
 0.30377564043385E+03 0.30377564043385E+03 0.29315000283892E+03 0.29315000279150E+03 0.30377320200043E+03
 0.30377320200043E+03 0.29315000283973E+03 0.29315000279229E+03 0.30377564043385E+03 0.30377564043385E+03
 0.29315000283892E+03 0.29315000279150E+03 0.30377320200043E+03 0.30377320200043E+03 0.29315000283973E+03
 0.29315000279229E+03 0.30221858274561E+03 0.29315001849356E+03 0.43337428519668E+02 0.53599968282587E+02
 0.16813646024149E+03 0.41179545165666E+03 0.24281830911396E+03 0.13300791055268E+03 0.15710772805894E+03
 0.13300791055268E+03 0.32738870394651E+03 0.13301864363036E+03 0.15697988333489E+03 0.13301864363036E+03
 0.32727969124232E+03 0.13300791055268E+03 0.15710772805894E+03 0.13300791055268E+03 0.32738870394651E+03
 0.13301864363036E+03 0.15697988333489E+03 0.13301864363036E+03 0.32727969124232E+03 0.20989843044487E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35482523443581E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18068248135287E+00 0.00000000000000E+00 0.00000000000000E+00 0.18068248135287E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18940869725261E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18940869725261E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951378224896E+00 0.19125236507336E+00 0.33038957411493E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    683.77921377
 0.10182840056567E+00 0.31081489119493E+03 0.44280534932346E+03 0.43832522357640E+03 0.43651338928654E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15024320068310E+00 0.00000000000000E+00 -.21674121445483E+02
 0.34910537617524E-02 0.11413682995751E+01 0.22915716989658E+04 0.85933938711219E+03 0.70091310604809E+01
 0.26284241476803E+01 0.33582149208710E+03 0.29315013478135E+03 0.32941016735601E+03 0.35557658772018E+03
 0.29315001499904E+03 0.29315002808335E+03 0.32575306919765E+03 0.35555354640565E+03 0.29315001214713E+03
 0.29315002799446E+03 0.32941016735601E+03 0.35557658772018E+03 0.29315001499904E+03 0.29315002808335E+03
 0.32575306919765E+03 0.35555354640565E+03 0.29315001214713E+03 0.29315002799446E+03 0.39931402478717E+03
 0.32212852139089E+03 0.19388812809446E+04 0.18122272280737E+04 0.60692808127563E+03 0.10184480024397E+04
 0.40848528075767E+03 0.11445657883507E+04 0.10959340046454E+04 0.10522622463561E+04 0.17322681811781E+04
 0.10335790675742E+04 0.10956225016801E+04 0.96360499620815E+03 0.17321646664762E+04 0.11445657883507E+04
 0.10959340046454E+04 0.10522622463561E+04 0.17322681811781E+04 0.10335790675742E+04 0.10956225016801E+04
 0.96360499620815E+03 0.17321646664762E+04 0.17041279543385E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48223166851387E+03 0.12948641833778E+01
 0.12948641833778E+01 0.25551321773866E+01 0.00000000000000E+00 0.33060220676874E+03 0.33060220676874E+03
 0.33060220676874E+03 0.33060220676874E+03 0.00000000000000E+00 0.00000000000000E+00 0.16558694065530E+00
 0.00000000000000E+00 -.13141967182525E+02 0.10000000000000E-02 0.12332949868207E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64866881690837E+01 0.24325080634064E+01 0.32212888508103E+03 0.39931465916798E+03
 0.30393582879622E+03 0.30393582879622E+03 0.29315000356360E+03 0.29315000346200E+03 0.30393332482364E+03
 0.30393332482364E+03 0.29315000356461E+03 0.29315000346298E+03 0.30393582879622E+03 0.30393582879622E+03
 0.29315000356360E+03 0.29315000346200E+03 0.30393332482364E+03 0.30393332482364E+03 0.29315000356461E+03
 0.29315000346298E+03 0.30235679779874E+03 0.29315002240267E+03 0.39720377944451E+02 0.49096827788823E+02
 0.16957169940563E+03 0.41396458200370E+03 0.24354502410104E+03 0.13584608680354E+03 0.15837864633437E+03
 0.13584608680354E+03 0.32902697835341E+03 0.13585714887113E+03 0.15824875483780E+03 0.13585714887113E+03
 0.32891642899805E+03 0.13584608680354E+03 0.15837864633437E+03 0.13584608680354E+03 0.32902697835341E+03
 0.13585714887113E+03 0.15824875483780E+03 0.13585714887113E+03 0.32891642899805E+03 0.20982956593359E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35508102747302E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18056596746092E+00 0.00000000000000E+00 0.00000000000000E+00 0.18056596746092E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18922770816132E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18922770816132E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951386615506E+00 0.19112948110945E+00 0.33060220676874E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    693.01589679
 0.10172872084009E+00 0.31099672672970E+03 0.44312956305795E+03 0.43864899491774E+03 0.43683601282625E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14988713150265E+00 0.00000000000000E+00 -.21685376307703E+02
 0.34944741904919E-02 0.11456293009829E+01 0.22893286840598E+04 0.85849825652244E+03 0.69830616178692E+01
 0.26186481067009E+01 0.33620114251120E+03 0.29315015336146E+03 0.32973780633082E+03 0.35606495424346E+03
 0.29315001736106E+03 0.29315003252415E+03 0.32606337073946E+03 0.35604207496228E+03 0.29315001407819E+03
 0.29315003242262E+03 0.32973780633082E+03 0.35606495424346E+03 0.29315001736106E+03 0.29315003252415E+03
 0.32606337073946E+03 0.35604207496228E+03 0.29315001407819E+03 0.29315003242262E+03 0.39987475935652E+03
 0.32272000303103E+03 0.19421828795439E+04 0.18142568730035E+04 0.60582454263721E+03 0.10145799299662E+04
 0.40572626461576E+03 0.11469127268953E+04 0.10976473093208E+04 0.10536972953829E+04 0.17322708818027E+04
 0.10361160333898E+04 0.10973399193332E+04 0.96538317980940E+03 0.17321698224859E+04 0.11469127268953E+04
 0.10976473093208E+04 0.10536972953829E+04 0.17322708818027E+04 0.10361160333898E+04 0.10973399193332E+04
 0.96538317980940E+03 0.17321698224859E+04 0.17037351188599E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48255421497106E+03 0.12948642663061E+01
 0.12948642663061E+01 0.25920789094629E+01 0.00000000000000E+00 0.33076019953083E+03 0.33076019953083E+03
 0.33076019953083E+03 0.33076019953083E+03 0.00000000000000E+00 0.00000000000000E+00 0.16489486036009E+00
 0.00000000000000E+00 -.13135212416023E+02 0.10000000000000E-02 0.12460249778446E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64204170399848E+01 0.24076563899943E+01 0.32272045469852E+03 0.39987516522576E+03
 0.30404984765302E+03 0.30404984765302E+03 0.29315000413899E+03 0.29315000402098E+03 0.30404729564747E+03
 0.30404729564747E+03 0.29315000414016E+03 0.29315000402211E+03 0.30404984765302E+03 0.30404984765302E+03
 0.29315000413899E+03 0.29315000402098E+03 0.30404729564747E+03 0.30404729564747E+03 0.29315000414016E+03
 0.29315000402211E+03 0.30245523025948E+03 0.29315002559431E+03 0.37209111168889E+02 0.45987905837610E+02
 0.17060873392051E+03 0.41559322920406E+03 0.24413145161394E+03 0.13785837021278E+03 0.15930066249701E+03
 0.13785837021278E+03 0.33026352819872E+03 0.13786967531545E+03 0.15916945122732E+03 0.13786967531545E+03
 0.33015203425067E+03 0.13785837021278E+03 0.15930066249701E+03 0.13785837021278E+03 0.33026352819872E+03
 0.13786967531545E+03 0.15916945122732E+03 0.13786967531545E+03 0.33015203425067E+03 0.20984515159585E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35527095610979E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18052318983678E+00 0.00000000000000E+00 0.00000000000000E+00 0.18052318983678E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18914833698767E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18914833698767E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951372886624E+00 0.19103805177442E+00 0.33076019953083E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    700.25423103
 0.10161742174644E+00 0.31115073383394E+03 0.44338491363899E+03 0.43890581483281E+03 0.43709284415764E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14961442688455E+00 0.00000000000000E+00 -.21668233665785E+02
 0.34983013548588E-02 0.11488707747121E+01 0.22868241436344E+04 0.85755905386290E+03 0.69633593055799E+01
 0.26112597395925E+01 0.33649845447213E+03 0.29315016901005E+03 0.32999464445554E+03 0.35644507283361E+03
 0.29315001937296E+03 0.29315003630745E+03 0.32630708077401E+03 0.35642231971485E+03 0.29315001572445E+03
 0.29315003619525E+03 0.32999464445554E+03 0.35644507283361E+03 0.29315001937296E+03 0.29315003630745E+03
 0.32630708077401E+03 0.35642231971485E+03 0.29315001572445E+03 0.29315003619525E+03 0.40030346965449E+03
 0.32317448657111E+03 0.19447951450628E+04 0.18159526279448E+04 0.60509320385148E+03 0.10118208396903E+04
 0.40370216981956E+03 0.11487620185334E+04 0.10990150259218E+04 0.10548982530769E+04 0.17323329953367E+04
 0.10381125830915E+04 0.10987107594254E+04 0.96684561938292E+03 0.17322337720216E+04 0.11487620185334E+04
 0.10990150259218E+04 0.10548982530769E+04 0.17323329953367E+04 0.10381125830915E+04 0.10987107594254E+04
 0.96684561938292E+03 0.17322337720216E+04 0.17035587735531E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48281054230549E+03 0.12948641399953E+01
 0.12948641399953E+01 0.26210322464406E+01 0.00000000000000E+00 0.33088548030043E+03 0.33088548030043E+03
 0.33088548030043E+03 0.33088548030043E+03 0.00000000000000E+00 0.00000000000000E+00 0.16436342760251E+00
 0.00000000000000E+00 -.13100211764939E+02 0.10000000000000E-02 0.12557763567995E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63705610928914E+01 0.23889604098343E+01 0.32317502768525E+03 0.40030366257879E+03
 0.30414031026000E+03 0.30414031026000E+03 0.29315000455545E+03 0.29315000449817E+03 0.30413771995083E+03
 0.30413771995083E+03 0.29315000455674E+03 0.29315000449944E+03 0.30414031026000E+03 0.30414031026000E+03
 0.29315000455545E+03 0.29315000449817E+03 0.30413771995083E+03 0.30413771995083E+03 0.29315000455674E+03
 0.29315000449944E+03 0.30253342569586E+03 0.29315002828916E+03 0.35287756793288E+02 0.43618080823161E+02
 0.17141016586117E+03 0.41685140466444E+03 0.24458418797396E+03 0.13940754090001E+03 0.16001322365173E+03
 0.13940754090001E+03 0.33121464050990E+03 0.13941904153907E+03 0.15988110440659E+03 0.13941904153907E+03
 0.33110253682980E+03 0.13940754090001E+03 0.16001322365173E+03 0.13940754090001E+03 0.33121464050990E+03
 0.13941904153907E+03 0.15988110440659E+03 0.13941904153907E+03 0.33110253682980E+03 0.20986361622838E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35541426146988E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18027186100414E+00 0.00000000000000E+00 0.00000000000000E+00 0.18027186100414E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18895066577286E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18895066577286E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951451218288E+00 0.19096661970758E+00 0.33088548030043E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    710.14190321
 0.10147962261497E+00 0.31135128827498E+03 0.44373178523827E+03 0.43925381094713E+03 0.43744034944694E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14925039581463E+00 0.00000000000000E+00 -.21674123635792E+02
 0.35030513618052E-02 0.11531761033806E+01 0.22837232954179E+04 0.85639623578173E+03 0.69373619315799E+01
 0.26015107243425E+01 0.33690179389864E+03 0.29315019260299E+03 0.33034308073547E+03 0.35696168580649E+03
 0.29315002245336E+03 0.29315004210145E+03 0.32663757289820E+03 0.35693910008532E+03 0.29315001824806E+03
 0.29315004197314E+03 0.33034308073548E+03 0.35696168580649E+03 0.29315002245336E+03 0.29315004210145E+03
 0.32663757289820E+03 0.35693910008532E+03 0.29315001824806E+03 0.29315004197314E+03 0.40089242339035E+03
 0.32379604796416E+03 0.19483264053394E+04 0.18181801659122E+04 0.60398172543212E+03 0.10078848957521E+04
 0.40088326169281E+03 0.11512678909940E+04 0.11008515248309E+04 0.10564714563143E+04 0.17323959464732E+04
 0.10408196879687E+04 0.11005513322501E+04 0.96877870020503E+03 0.17322990866930E+04 0.11512678909940E+04
 0.11008515248309E+04 0.10564714563143E+04 0.17323959464733E+04 0.10408196879687E+04 0.11005513322501E+04
 0.96877870020503E+03 0.17322990866930E+04 0.17032402880204E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48315761353352E+03 0.12948641833939E+01
 0.12948641833939E+01 0.26605829351323E+01 0.00000000000000E+00 0.33105970224622E+03 0.33105970224622E+03
 0.33105970224622E+03 0.33105970224622E+03 0.00000000000000E+00 0.00000000000000E+00 0.16365242140417E+00
 0.00000000000000E+00 -.13085276437475E+02 0.10000000000000E-02 0.12687844760778E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63052473850646E+01 0.23644677693992E+01 0.32379667978407E+03 0.40089237584929E+03
 0.30426312917030E+03 0.30426312917030E+03 0.29315000529767E+03 0.29315000523105E+03 0.30426048654577E+03
 0.30426048654577E+03 0.29315000529915E+03 0.29315000523252E+03 0.30426312917030E+03 0.30426312917030E+03
 0.29315000529767E+03 0.29315000523105E+03 0.30426048654577E+03 0.30426048654577E+03 0.29315000529915E+03
 0.29315000523252E+03 0.30263957053326E+03 0.29315003236603E+03 0.32640956044027E+02 0.40369755267073E+02
 0.17251320778477E+03 0.41862614227529E+03 0.24525036845160E+03 0.14154131219719E+03 0.16099478714112E+03
 0.14154131219719E+03 0.33256068126096E+03 0.14155308000452E+03 0.16086141564648E+03 0.14155308000452E+03
 0.33244773433086E+03 0.14154131219719E+03 0.16099478714111E+03 0.14154131219719E+03 0.33256068126096E+03
 0.14155308000452E+03 0.16086141564648E+03 0.14155308000452E+03 0.33244773433085E+03 0.20991366615684E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35561906942880E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18017409051439E+00 0.00000000000000E+00 0.00000000000000E+00 0.18017409051439E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18881035565598E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18881035565598E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951458671406E+00 0.19086622989005E+00 0.33105970224622E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    720.79188461
 0.10135128705026E+00 0.31156023289722E+03 0.44410259130581E+03 0.43962481176801E+03 0.43781033082515E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14886916110819E+00 0.00000000000000E+00 -.21686830659017E+02
 0.35074867281865E-02 0.11576580268286E+01 0.22808354300278E+04 0.85531328626043E+03 0.69105036328523E+01
 0.25914388623196E+01 0.33733271721874E+03 0.29315022127938E+03 0.33071539945248E+03 0.35751400404455E+03
 0.29315002626951E+03 0.29315004928120E+03 0.32699071799911E+03 0.35749159290624E+03 0.29315002137910E+03
 0.29315004913327E+03 0.33071539945249E+03 0.35751400404455E+03 0.29315002626951E+03 0.29315004928120E+03
 0.32699071799911E+03 0.35749159290624E+03 0.29315002137910E+03 0.29315004913327E+03 0.40152570915160E+03
 0.32446358945728E+03 0.19520746319200E+04 0.18204956720235E+04 0.60270444819975E+03 0.10035521092327E+04
 0.39783413879192E+03 0.11539366490303E+04 0.11027897453825E+04 0.10581103301065E+04 0.17324409127076E+04
 0.10437040568276E+04 0.11024937183626E+04 0.97080421551985E+03 0.17323464329573E+04 0.11539366490303E+04
 0.11027897453825E+04 0.10581103301065E+04 0.17324409127076E+04 0.10437040568276E+04 0.11024937183626E+04
 0.97080421551985E+03 0.17323464329573E+04 0.17028373645822E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48352736577074E+03 0.12948642770221E+01
 0.12948642770221E+01 0.27031828607598E+01 0.00000000000000E+00 0.33125086929014E+03 0.33125086929014E+03
 0.33125086929014E+03 0.33125086929014E+03 0.00000000000000E+00 0.00000000000000E+00 0.16290545869576E+00
 0.00000000000000E+00 -.13076388720147E+02 0.10000000000000E-02 0.12824028537084E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62382892995488E+01 0.23393584873308E+01 0.32446428842964E+03 0.40152545426956E+03
 0.30439491254491E+03 0.30439491254491E+03 0.29315000629494E+03 0.29315000614244E+03 0.30439221346269E+03
 0.30439221346269E+03 0.29315000629670E+03 0.29315000614415E+03 0.30439491254491E+03 0.30439491254491E+03
 0.29315000629494E+03 0.29315000614244E+03 0.30439221346269E+03 0.30439221346269E+03 0.29315000629670E+03
 0.29315000614415E+03 0.30275347259709E+03 0.29315003734214E+03 0.29780887213078E+02 0.36879503689340E+02
 0.17370734250029E+03 0.42058481576038E+03 0.24600893654759E+03 0.14385048822617E+03 0.16205843509186E+03
 0.14385048822617E+03 0.33404857816865E+03 0.14386254491741E+03 0.16192374002314E+03 0.14386254491741E+03
 0.33393475000641E+03 0.14385048822616E+03 0.16205843509185E+03 0.14385048822616E+03 0.33404857816864E+03
 0.14386254491741E+03 0.16192374002314E+03 0.14386254491741E+03 0.33393475000641E+03 0.20999235073906E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35584342919996E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18012391061150E+00 0.00000000000000E+00 0.00000000000000E+00 0.18012391061150E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18868254873872E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18868254873872E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951444327932E+00 0.19075594406335E+00 0.33125086929014E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    731.20504113
 0.10125644659752E+00 0.31176081569493E+03 0.44446247100172E+03 0.43998350497707E+03 0.43816746189178E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14850732775829E+00 0.00000000000000E+00 -.21702142367998E+02
 0.35107716264129E-02 0.11618842077852E+01 0.22787013372824E+04 0.85451300148089E+03 0.68853677039382E+01
 0.25820128889768E+01 0.33775130306049E+03 0.29315025281048E+03 0.33107723902093E+03 0.35804967396859E+03
 0.29315003054465E+03 0.29315005732613E+03 0.32733417725415E+03 0.35802742918853E+03 0.29315002489189E+03
 0.29315005715657E+03 0.33107723902093E+03 0.35804967396859E+03 0.29315003054465E+03 0.29315005732613E+03
 0.32733417725415E+03 0.35802742918853E+03 0.29315002489189E+03 0.29315005715657E+03 0.40213737937072E+03
 0.32511005911108E+03 0.19556725610126E+04 0.18226825800230E+04 0.60146280213966E+03 0.99938267103360E+03
 0.39491255488325E+03 0.11565105663686E+04 0.11046484300624E+04 0.10596705939214E+04 0.17324691195208E+04
 0.10464859610386E+04 0.11043562736746E+04 0.97273710047984E+03 0.17323768090467E+04 0.11565105663686E+04
 0.11046484300624E+04 0.10596705939214E+04 0.17324691195208E+04 0.10464859610386E+04 0.11043562736746E+04
 0.97273710047983E+03 0.17323768090467E+04 0.17024264814882E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48388453462619E+03 0.12948643898423E+01
 0.12948643898423E+01 0.27448354868119E+01 0.00000000000000E+00 0.33144167534133E+03 0.33144167534133E+03
 0.33144167534133E+03 0.33144167534133E+03 0.00000000000000E+00 0.00000000000000E+00 0.16219342321295E+00
 0.00000000000000E+00 -.13072087647166E+02 0.10000000000000E-02 0.12953350532257E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61760082691179E+01 0.23160031009192E+01 0.32511083135805E+03 0.40213692485447E+03
 0.30452411070532E+03 0.30452411070532E+03 0.29315000734518E+03 0.29315000716723E+03 0.30452135614013E+03
 0.30452135614013E+03 0.29315000734722E+03 0.29315000716923E+03 0.30452411070532E+03 0.30452411070532E+03
 0.29315000734518E+03 0.29315000716723E+03 0.30452135614013E+03 0.30452135614013E+03 0.29315000734722E+03
 0.29315000716923E+03 0.30286518608421E+03 0.29315004283593E+03 0.27010537336890E+02 0.33516818862643E+02
 0.17487965619132E+03 0.42254344723263E+03 0.24678939276035E+03 0.14610388904858E+03 0.16310390201502E+03
 0.14610388904858E+03 0.33553873203563E+03 0.14611623081227E+03 0.16296799344149E+03 0.14611623081227E+03
 0.33542412615159E+03 0.14610388904857E+03 0.16310390201502E+03 0.14610388904857E+03 0.33553873203562E+03
 0.14611623081227E+03 0.16296799344149E+03 0.14611623081227E+03 0.33542412615159E+03 0.21010233076747E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35606821365209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18010117027762E+00 0.00000000000000E+00 0.00000000000000E+00 0.18010117027762E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18860568369269E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18860568369269E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951416308895E+00 0.19064583923024E+00 0.33144167534133E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    741.14712800
 0.10113794257835E+00 0.31196281336444E+03 0.44480595210503E+03 0.44032745819241E+03 0.43851075390242E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14817162230762E+00 0.00000000000000E+00 -.21694395920770E+02
 0.35148848919992E-02 0.11657742884836E+01 0.22760347054921E+04 0.85351301455954E+03 0.68623918703906E+01
 0.25733969513965E+01 0.33814969984006E+03 0.29315028579417E+03 0.33142194883927E+03 0.35855726464262E+03
 0.29315003508184E+03 0.29315006586528E+03 0.32766184185260E+03 0.35853517603771E+03 0.29315002862425E+03
 0.29315006567308E+03 0.33142194883927E+03 0.35855726464262E+03 0.29315003508184E+03 0.29315006586528E+03
 0.32766184185260E+03 0.35853517603771E+03 0.29315002862425E+03 0.29315006567308E+03 0.40271028648765E+03
 0.32571799839202E+03 0.19591096577350E+04 0.18248525513630E+04 0.60038134694383E+03 0.99563459441327E+03
 0.39225134073472E+03 0.11589651881298E+04 0.11064286387066E+04 0.10612230804599E+04 0.17325371993877E+04
 0.10491372423370E+04 0.11061400238311E+04 0.97463735542099E+03 0.17324468352709E+04 0.11589651881298E+04
 0.11064286387066E+04 0.10612230804598E+04 0.17325371993877E+04 0.10491372423370E+04 0.11061400238311E+04
 0.97463735542098E+03 0.17324468352709E+04 0.17021386444407E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48422745760060E+03 0.12948643327647E+01
 0.12948643327647E+01 0.27846038343113E+01 0.00000000000000E+00 0.33162459984212E+03 0.33162459984212E+03
 0.33162459984212E+03 0.33162459984212E+03 0.00000000000000E+00 0.00000000000000E+00 0.16153046394587E+00
 0.00000000000000E+00 -.13042338899278E+02 0.10000000000000E-02 0.13073410508364E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61192907503991E+01 0.22947340313996E+01 0.32571888032809E+03 0.40270960192491E+03
 0.30464851197409E+03 0.30464851197409E+03 0.29315000836182E+03 0.29315000825797E+03 0.30464570432320E+03
 0.30464570432320E+03 0.29315000836414E+03 0.29315000826026E+03 0.30464851197409E+03 0.30464851197409E+03
 0.29315000836182E+03 0.29315000825797E+03 0.30464570432320E+03 0.30464570432320E+03 0.29315000836414E+03
 0.29315000826026E+03 0.30297285621096E+03 0.29315004860084E+03 0.24397839035894E+02 0.30361291138526E+02
 0.17598087286844E+03 0.42438272749251E+03 0.24752195025973E+03 0.14822622791722E+03 0.16408486357943E+03
 0.14822622791722E+03 0.33693238795593E+03 0.14823884415541E+03 0.16394790268968E+03 0.14823884415541E+03
 0.33681714586382E+03 0.14822622791722E+03 0.16408486357943E+03 0.14822622791722E+03 0.33693238795593E+03
 0.14823884415541E+03 0.16394790268968E+03 0.14823884415541E+03 0.33681714586382E+03 0.21020445180718E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35627711486011E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17989201221390E+00 0.00000000000000E+00 0.00000000000000E+00 0.17989201221390E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18841106796628E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18841106796628E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951466593440E+00 0.19054126847334E+00 0.33162459984212E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    750.58207158
 0.10104133716693E+00 0.31214631373361E+03 0.44512934564616E+03 0.44065041785784E+03 0.43883262026183E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14786148340931E+00 0.00000000000000E+00 -.21703450169817E+02
 0.35182451584617E-02 0.11693466985433E+01 0.22738608708831E+04 0.85269782658117E+03 0.68414269351989E+01
 0.25655351006996E+01 0.33852492407336E+03 0.29315032053696E+03 0.33174664177346E+03 0.35903596165653E+03
 0.29315003994054E+03 0.29315007501052E+03 0.32797040388003E+03 0.35901401683409E+03 0.29315003262629E+03
 0.29315007479443E+03 0.33174664177347E+03 0.35903596165653E+03 0.29315003994054E+03 0.29315007501052E+03
 0.32797040388003E+03 0.35901401683409E+03 0.29315003262629E+03 0.29315007479443E+03 0.40325516057210E+03
 0.32629504354771E+03 0.19623248886956E+04 0.18268248496090E+04 0.59926045544252E+03 0.99194602218126E+03
 0.38968926446152E+03 0.11612690428667E+04 0.11080802201199E+04 0.10626344847308E+04 0.17325720901215E+04
 0.10516273559826E+04 0.11077947965269E+04 0.97637993969217E+03 0.17324834468570E+04 0.11612690428667E+04
 0.11080802201199E+04 0.10626344847308E+04 0.17325720901215E+04 0.10516273559826E+04 0.11077947965269E+04
 0.97637993969217E+03 0.17324834468570E+04 0.17017922051910E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48454920527431E+03 0.12948643994785E+01
 0.12948643994785E+01 0.28223436086184E+01 0.00000000000000E+00 0.33179985431917E+03 0.33179985431917E+03
 0.33179985431917E+03 0.33179985431917E+03 0.00000000000000E+00 0.00000000000000E+00 0.16091605383856E+00
 0.00000000000000E+00 -.13032720524280E+02 0.10000000000000E-02 0.13184321499856E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60678131977343E+01 0.22754299491504E+01 0.32629600325015E+03 0.40325431107341E+03
 0.30476585974359E+03 0.30476585974359E+03 0.29315000959223E+03 0.29315000942981E+03 0.30476300201127E+03
 0.30476300201127E+03 0.29315000959488E+03 0.29315000943241E+03 0.30476585974359E+03 0.30476585974359E+03
 0.29315000959223E+03 0.29315000942981E+03 0.30476300201127E+03 0.30476300201127E+03 0.29315000959488E+03
 0.29315000943241E+03 0.30307441232607E+03 0.29315005469483E+03 0.21894002655661E+02 0.27353409171070E+02
 0.17702833664569E+03 0.42615953091349E+03 0.24824605258457E+03 0.15025300907116E+03 0.16501810973535E+03
 0.15025300907116E+03 0.33828089160071E+03 0.15026588435682E+03 0.16488013551446E+03 0.15026588435682E+03
 0.33816502994841E+03 0.15025300907116E+03 0.16501810973535E+03 0.15025300907116E+03 0.33828089160071E+03
 0.15026588435682E+03 0.16488013551446E+03 0.15026588435682E+03 0.33816502994841E+03 0.21031346970223E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35648033652212E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17983225783386E+00 0.00000000000000E+00 0.00000000000000E+00 0.17983225783386E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18830275382801E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18830275382801E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951457994608E+00 0.19044055270669E+00 0.33179985431917E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    760.23528357
 0.10094659706595E+00 0.31233345840463E+03 0.44545828590088E+03 0.44097878646063E+03 0.43915983726444E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14755256933780E+00 0.00000000000000E+00 -.21706266474744E+02
 0.35215467790374E-02 0.11728820048440E+01 0.22717290162441E+04 0.85189838109153E+03 0.68208054748562E+01
 0.25578020530711E+01 0.33890665563733E+03 0.29315035970295E+03 0.33207711228979E+03 0.35952232462572E+03
 0.29315004550321E+03 0.29315008548150E+03 0.32828463233257E+03 0.35950052345648E+03 0.29315003721379E+03
 0.29315008523845E+03 0.33207711228979E+03 0.35952232462572E+03 0.29315004550321E+03 0.29315008548150E+03
 0.32828463233257E+03 0.35950052345648E+03 0.29315003721379E+03 0.29315008523845E+03 0.40380754791406E+03
 0.32688116032179E+03 0.19655791857325E+04 0.18288159535784E+04 0.59811353636069E+03 0.98821219734348E+03
 0.38710809330099E+03 0.11636057363492E+04 0.11097459816012E+04 0.10640643205709E+04 0.17326006138463E+04
 0.10541531672828E+04 0.11094636806759E+04 0.97814516077256E+03 0.17325136238282E+04 0.11636057363492E+04
 0.11097459816012E+04 0.10640643205709E+04 0.17326006138464E+04 0.10541531672828E+04 0.11094636806759E+04
 0.97814516077256E+03 0.17325136238281E+04 0.17014357969238E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48487631589960E+03 0.12948644202296E+01
 0.12948644202296E+01 0.28609564565983E+01 0.00000000000000E+00 0.33198030933285E+03 0.33198030933285E+03
 0.33198030933285E+03 0.33198030933285E+03 0.00000000000000E+00 0.00000000000000E+00 0.16030199414765E+00
 0.00000000000000E+00 -.13015892219641E+02 0.10000000000000E-02 0.13294832452716E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60173755693818E+01 0.22565158385182E+01 0.32688219920743E+03 0.40380653936250E+03
 0.30488604024257E+03 0.30488604024257E+03 0.29315001096115E+03 0.29315001077555E+03 0.30488313138192E+03
 0.30488313138192E+03 0.29315001096416E+03 0.29315001077851E+03 0.30488604024257E+03 0.30488604024257E+03
 0.29315001096115E+03 0.29315001077555E+03 0.30488313138192E+03 0.30488313138192E+03 0.29315001096416E+03
 0.29315001077851E+03 0.30317846079172E+03 0.29315006158741E+03 0.19337301149024E+02 0.24297243752941E+02
 0.17809436682872E+03 0.42797953954282E+03 0.24899470087996E+03 0.15231994090730E+03 0.16596761235211E+03
 0.15231994090730E+03 0.33966082738209E+03 0.15233308147679E+03 0.16582864094048E+03 0.15233308147679E+03
 0.33954437069621E+03 0.15231994090730E+03 0.16596761235211E+03 0.15231994090730E+03 0.33966082738208E+03
 0.15233308147680E+03 0.16582864094048E+03 0.15233308147680E+03 0.33954437069621E+03 0.21043070140330E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35668759072434E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17971831872240E+00 0.00000000000000E+00 0.00000000000000E+00 0.17971831872240E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18816682570861E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18816682570861E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951470016576E+00 0.19033719375611E+00 0.33198030933285E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    771.81913797
 0.10083668035209E+00 0.31255711649178E+03 0.44585080989167E+03 0.44137051204023E+03 0.43955015156162E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14719272667789E+00 0.00000000000000E+00 -.21716938910510E+02
 0.35253850579430E-02 0.11769688734786E+01 0.22692556610165E+04 0.85097087288118E+03 0.67971211306174E+01
 0.25489204239815E+01 0.33936194464656E+03 0.29315041186143E+03 0.33247145853673E+03 0.36010159680497E+03
 0.29315005303607E+03 0.29315009966157E+03 0.32865980871433E+03 0.36007996354967E+03 0.29315004343432E+03
 0.29315009938260E+03 0.33247145853673E+03 0.36010159680497E+03 0.29315005303607E+03 0.29315009966157E+03
 0.32865980871433E+03 0.36007996354967E+03 0.29315004343432E+03 0.29315009938260E+03 0.40446446358852E+03
 0.32757924619611E+03 0.19694448707813E+04 0.18311756800798E+04 0.59673391614985E+03 0.98377640671887E+03
 0.38405882098828E+03 0.11663866087942E+04 0.11117188401419E+04 0.10657637096173E+04 0.17326326049392E+04
 0.10571592754688E+04 0.11114401022556E+04 0.98024293698302E+03 0.17325474599745E+04 0.11663866087942E+04
 0.11117188401419E+04 0.10657637096173E+04 0.17326326049392E+04 0.10571592754688E+04 0.11114401022556E+04
 0.98024293698302E+03 0.17325474599745E+04 0.17010094562706E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48526651645353E+03 0.12948644988666E+01
 0.12948644988666E+01 0.29072918741742E+01 0.00000000000000E+00 0.33219812026627E+03 0.33219812026627E+03
 0.33219812026627E+03 0.33219812026627E+03 0.00000000000000E+00 0.00000000000000E+00 0.15958413888560E+00
 0.00000000000000E+00 -.13004237059570E+02 0.10000000000000E-02 0.13423582436421E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59596609458698E+01 0.22348728547012E+01 0.32758039079801E+03 0.40446326279470E+03
 0.30503031901484E+03 0.30503031901484E+03 0.29315001282103E+03 0.29315001260394E+03 0.30502734905986E+03
 0.30502734905986E+03 0.29315001282452E+03 0.29315001260737E+03 0.30503031901484E+03 0.30503031901484E+03
 0.29315001282103E+03 0.29315001260394E+03 0.30502734905986E+03 0.30502734905986E+03 0.29315001282452E+03
 0.29315001260737E+03 0.30330342522732E+03 0.29315007079907E+03 0.16272232846469E+02 0.20653848490814E+02
 0.17936603818014E+03 0.43017074461347E+03 0.24990787624244E+03 0.15479251059853E+03 0.16709977030426E+03
 0.15479251059853E+03 0.34132166329913E+03 0.15480596914640E+03 0.16695964207563E+03 0.15480596914640E+03
 0.34120453118581E+03 0.15479251059853E+03 0.16709977030426E+03 0.15479251059853E+03 0.34132166329912E+03
 0.15480596914640E+03 0.16695964207563E+03 0.15480596914640E+03 0.34120453118581E+03 0.21058034548117E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35693922584022E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17964373665019E+00 0.00000000000000E+00 0.00000000000000E+00 0.17964373665019E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18804612855142E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18804612855142E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951458536487E+00 0.19021229642306E+00 0.33219812026627E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    782.13118118
 0.10075205593869E+00 0.31275190951523E+03 0.44619740619249E+03 0.44171577014383E+03 0.43989386607709E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14688217334208E+00 0.00000000000000E+00 -.21729168078670E+02
 0.35283457986618E-02 0.11804694582590E+01 0.22673514605723E+04 0.85025679771463E+03 0.67769648287203E+01
 0.25413618107701E+01 0.33976412488561E+03 0.29315046380358E+03 0.33281990844283E+03 0.36061334486073E+03
 0.29315006067475E+03 0.29315011404070E+03 0.32899136577533E+03 0.36059185652764E+03 0.29315004975132E+03
 0.29315011372592E+03 0.33281990844283E+03 0.36061334486073E+03 0.29315006067475E+03 0.29315011404070E+03
 0.32899136577533E+03 0.36059185652764E+03 0.29315004975132E+03 0.29315011372592E+03 0.40504694652564E+03
 0.32819839518493E+03 0.19728364746754E+04 0.18332149498157E+04 0.59544768836879E+03 0.97976963206941E+03
 0.38134470525878E+03 0.11688342027176E+04 0.11134380589692E+04 0.10672376565438E+04 0.17326386257971E+04
 0.10598062638605E+04 0.11131623204335E+04 0.98206969981912E+03 0.17325549959929E+04 0.11688342027176E+04
 0.11134380589692E+04 0.10672376565438E+04 0.17326386257971E+04 0.10598062638605E+04 0.11131623204335E+04
 0.98206969981912E+03 0.17325549959929E+04 0.17005855071733E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48561026981840E+03 0.12948645889739E+01
 0.12948645889739E+01 0.29485400470210E+01 0.00000000000000E+00 0.33239332836941E+03 0.33239332836941E+03
 0.33239332836941E+03 0.33239332836941E+03 0.00000000000000E+00 0.00000000000000E+00 0.15896209045045E+00
 0.00000000000000E+00 -.12997080648164E+02 0.10000000000000E-02 0.13534739772527E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59107157835707E+01 0.22165184188390E+01 0.32819962641221E+03 0.40504559517472E+03
 0.30515830479699E+03 0.30515830479699E+03 0.29315001482167E+03 0.29315001446463E+03 0.30515528080309E+03
 0.30515528080309E+03 0.29315001482568E+03 0.29315001446854E+03 0.30515830479699E+03 0.30515830479699E+03
 0.29315001482167E+03 0.29315001446463E+03 0.30515528080309E+03 0.30515528080309E+03 0.29315001482568E+03
 0.29315001446854E+03 0.30341429417899E+03 0.29315008000756E+03 0.13530142241315E+02 0.17413806329531E+02
 0.18049662255481E+03 0.43213760573076E+03 0.25073850006318E+03 0.15699813733029E+03 0.16810619655793E+03
 0.15699813733029E+03 0.34281272322845E+03 0.15701187760474E+03 0.16796504127842E+03 0.15701187760474E+03
 0.34269499055439E+03 0.15699813733028E+03 0.16810619655793E+03 0.15699813733028E+03 0.34281272322844E+03
 0.15701187760474E+03 0.16796504127842E+03 0.15701187760474E+03 0.34269499055439E+03 0.21072202794212E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35716445188197E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17960197577705E+00 0.00000000000000E+00 0.00000000000000E+00 0.17960197577705E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18794968403521E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18794968403521E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951438402763E+00 0.19010038868298E+00 0.33239332836941E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    792.44322439
 0.10067970975657E+00 0.31294631895955E+03 0.44654202492180E+03 0.44205856595480E+03 0.44023495172011E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14658072028592E+00 0.00000000000000E+00 -.21740161175083E+02
 0.35308808653638E-02 0.11838406528019E+01 0.22657235701369E+04 0.84964633880132E+03 0.67576662290367E+01
 0.25341248358888E+01 0.34016410892234E+03 0.29315052102493E+03 0.33316666082435E+03 0.36112131493504E+03
 0.29315006922348E+03 0.29315013013213E+03 0.32932155749307E+03 0.36109996810782E+03 0.29315005682977E+03
 0.29315012977789E+03 0.33316666082435E+03 0.36112131493504E+03 0.29315006922348E+03 0.29315013013213E+03
 0.32932155749307E+03 0.36109996810783E+03 0.29315005682977E+03 0.29315012977789E+03 0.40562237396485E+03
 0.32881176431690E+03 0.19761861353727E+04 0.18352223242631E+04 0.59418509644965E+03 0.97584784952534E+03
 0.37869182759344E+03 0.11712583650050E+04 0.11151345497594E+04 0.10686971278262E+04 0.17326421365579E+04
 0.10624276632359E+04 0.11148616676805E+04 0.98387696261921E+03 0.17325599139520E+04 0.11712583650050E+04
 0.11151345497594E+04 0.10686971278262E+04 0.17326421365580E+04 0.10624276632359E+04 0.11148616676805E+04
 0.98387696261921E+03 0.17325599139520E+04 0.17001707207466E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48595146902308E+03 0.12948646699736E+01
 0.12948646699736E+01 0.29897882198678E+01 0.00000000000000E+00 0.33258992310126E+03 0.33258992310126E+03
 0.33258992310126E+03 0.33258992310126E+03 0.00000000000000E+00 0.00000000000000E+00 0.15835562596351E+00
 0.00000000000000E+00 -.12989080740305E+02 0.10000000000000E-02 0.13642721224448E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58639327656009E+01 0.21989747871003E+01 0.32881308895817E+03 0.40562087002935E+03
 0.30528663741489E+03 0.30528663741489E+03 0.29315001696205E+03 0.29315001655345E+03 0.30528355957461E+03
 0.30528355957461E+03 0.29315001696661E+03 0.29315001655790E+03 0.30528663741489E+03 0.30528663741489E+03
 0.29315001696205E+03 0.29315001655345E+03 0.30528355957461E+03 0.30528355957461E+03 0.29315001696661E+03
 0.29315001655790E+03 0.30352551742091E+03 0.29315009018543E+03 0.10804492487179E+02 0.14210930447984E+02
 0.18162283372730E+03 0.43411088187089E+03 0.25157993397495E+03 0.15919325751927E+03 0.16910851073414E+03
 0.15919325751927E+03 0.34430799528710E+03 0.15920727952440E+03 0.16896637269080E+03 0.15920727952440E+03
 0.34418970533356E+03 0.15919325751927E+03 0.16910851073414E+03 0.15919325751927E+03 0.34430799528709E+03
 0.15920727952440E+03 0.16896637269080E+03 0.15920727952440E+03 0.34418970533356E+03 0.21087543874823E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35739102889188E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17955120990600E+00 0.00000000000000E+00 0.00000000000000E+00 0.17955120990600E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18786261759758E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18786261759758E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951420560401E+00 0.18998784604347E+00 0.33258992310126E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    802.75526760
 0.10061273999479E+00 0.31314070297629E+03 0.44688468888939E+03 0.44239923926255E+03 0.44057387743827E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14628820014178E+00 0.00000000000000E+00 -.21749747692857E+02
 0.35332307661603E-02 0.11870851154545E+01 0.22642166700858E+04 0.84908125128216E+03 0.67391966219179E+01
 0.25271987332192E+01 0.34056179308536E+03 0.29315058392759E+03 0.33351160308591E+03 0.36162567834129E+03
 0.29315007876743E+03 0.29315014809538E+03 0.32965020313491E+03 0.36160446950030E+03 0.29315006474201E+03
 0.29315014769776E+03 0.33351160308591E+03 0.36162567834129E+03 0.29315007876743E+03 0.29315014809538E+03
 0.32965020313491E+03 0.36160446950030E+03 0.29315006474201E+03 0.29315014769776E+03 0.40619266495052E+03
 0.32942066998954E+03 0.19795006004519E+04 0.18372079096384E+04 0.59292207448811E+03 0.97196610921117E+03
 0.37607942435061E+03 0.11736620397299E+04 0.11168083514610E+04 0.10701469065156E+04 0.17326415567527E+04
 0.10650270734532E+04 0.11165381843874E+04 0.98567067243799E+03 0.17325606354653E+04 0.11736620397299E+04
 0.11168083514610E+04 0.10701469065156E+04 0.17326415567527E+04 0.10650270734532E+04 0.11165381843874E+04
 0.98567067243799E+03 0.17325606354653E+04 0.16997568975751E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48629052417208E+03 0.12948647406093E+01
 0.12948647406093E+01 0.30310363927146E+01 0.00000000000000E+00 0.33278740037623E+03 0.33278740037623E+03
 0.33278740037623E+03 0.33278740037623E+03 0.00000000000000E+00 0.00000000000000E+00 0.15776442563390E+00
 0.00000000000000E+00 -.12979633463816E+02 0.10000000000000E-02 0.13747613777979E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58191917006096E+01 0.21821968877286E+01 0.32942209041542E+03 0.40619101210539E+03
 0.30541506844235E+03 0.30541506844235E+03 0.29315001935885E+03 0.29315001889251E+03 0.30541193704708E+03
 0.30541193704708E+03 0.29315001936401E+03 0.29315001889755E+03 0.30541506844235E+03 0.30541506844235E+03
 0.29315001935885E+03 0.29315001889251E+03 0.30541193704708E+03 0.30541193704708E+03 0.29315001936401E+03
 0.29315001889755E+03 0.30363686958582E+03 0.29315010140980E+03 0.80832245579637E+01 0.11031498208703E+02
 0.18274372739197E+03 0.43608569871198E+03 0.25242825268305E+03 0.16138163885787E+03 0.17010559620405E+03
 0.16138163885787E+03 0.34580330134669E+03 0.16139594188759E+03 0.16996249911197E+03 0.16139594188759E+03
 0.34568447589022E+03 0.16138163885787E+03 0.17010559620404E+03 0.16138163885787E+03 0.34580330134669E+03
 0.16139594188759E+03 0.16996249911197E+03 0.16139594188759E+03 0.34568447589022E+03 0.21103485861019E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35761769783375E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17948979008241E+00 0.00000000000000E+00 0.00000000000000E+00 0.17948979008241E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18776929325946E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18776929325946E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951406934590E+00 0.18987498014202E+00 0.33278740037623E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    813.06731081
 0.10051816912028E+00 0.31333893832535E+03 0.44722574518583E+03 0.44273972628750E+03 0.44091326688882E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14600414022446E+00 0.00000000000000E+00 -.21744530688027E+02
 0.35365546255419E-02 0.11902091185549E+01 0.22620886277910E+04 0.84828323542162E+03 0.67215079058654E+01
 0.25205654646995E+01 0.34095731644201E+03 0.29315065293295E+03 0.33385485749230E+03 0.36212654782549E+03
 0.29315008939740E+03 0.29315016810038E+03 0.32997741258628E+03 0.36210547356122E+03 0.29315007356527E+03
 0.29315016765517E+03 0.33385485749230E+03 0.36212654782549E+03 0.29315008939740E+03 0.29315016810038E+03
 0.32997741258628E+03 0.36210547356122E+03 0.29315007356527E+03 0.29315016765517E+03 0.40675818982216E+03
 0.33002521690489E+03 0.19828071186780E+04 0.18392258860931E+04 0.59166190041513E+03 0.96812843272046E+03
 0.37350822280325E+03 0.11760568907454E+04 0.11184662845493E+04 0.10716168850632E+04 0.17326445857268E+04
 0.10676172830038E+04 0.11181987106912E+04 0.98748186830348E+03 0.17325648782610E+04 0.11760568907454E+04
 0.11184662845493E+04 0.10716168850632E+04 0.17326445857268E+04 0.10676172830038E+04 0.11181987106912E+04
 0.98748186830348E+03 0.17325648782610E+04 0.16993676724542E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48662973807476E+03 0.12948647021692E+01
 0.12948647021692E+01 0.30722845655614E+01 0.00000000000000E+00 0.33298369307951E+03 0.33298369307951E+03
 0.33298369307951E+03 0.33298369307951E+03 0.00000000000000E+00 0.00000000000000E+00 0.15718841163165E+00
 0.00000000000000E+00 -.12953129718647E+02 0.10000000000000E-02 0.13849525705377E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57763710975995E+01 0.21661391615998E+01 0.33002673020853E+03 0.40675639834682E+03
 0.30554346772701E+03 0.30554346772701E+03 0.29315002203631E+03 0.29315002150547E+03 0.30554028307552E+03
 0.30554028307552E+03 0.29315002204214E+03 0.29315002151117E+03 0.30554346772701E+03 0.30554346772701E+03
 0.29315002203631E+03 0.29315002150547E+03 0.30554028307552E+03 0.30554028307552E+03 0.29315002204214E+03
 0.29315002151117E+03 0.30374824377694E+03 0.29315011376176E+03 0.53526021662952E+01 0.78600990231734E+01
 0.18384665106340E+03 0.43802335205977E+03 0.25325746774106E+03 0.16355676133363E+03 0.17108483798752E+03
 0.16355676133363E+03 0.34726531482828E+03 0.16357134476849E+03 0.17094080170313E+03 0.16357134476849E+03
 0.34714597066875E+03 0.16355676133363E+03 0.17108483798752E+03 0.16355676133363E+03 0.34726531482827E+03
 0.16357134476849E+03 0.17094080170313E+03 0.16357134476849E+03 0.34714597066875E+03 0.21117730140925E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35783850465391E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17930435960066E+00 0.00000000000000E+00 0.00000000000000E+00 0.17930435960066E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18759022548653E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18759022548653E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951444930236E+00 0.18976350082606E+00 0.33298369307951E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    823.37935403
 0.10042192832441E+00 0.31353460328003E+03 0.44756538707397E+03 0.44307884381950E+03 0.44125127403402E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14572801261430E+00 0.00000000000000E+00 -.21751578025794E+02
 0.35399436119315E-02 0.11932196213556E+01 0.22599230035856E+04 0.84747112634461E+03 0.67045494868005E+01
 0.25142060575502E+01 0.34135072975631E+03 0.29315072848197E+03 0.33419643445742E+03 0.36262403361028E+03
 0.29315010121002E+03 0.29315019032782E+03 0.33030319368544E+03 0.36260309061641E+03 0.29315008338182E+03
 0.29315018983052E+03 0.33419643445742E+03 0.36262403361028E+03 0.29315010121002E+03 0.29315019032782E+03
 0.33030319368545E+03 0.36260309061641E+03 0.29315008338182E+03 0.29315018983052E+03 0.40731920307923E+03
 0.33062547318483E+03 0.19860929713878E+04 0.18412173502447E+04 0.59040543214354E+03 0.96433359608738E+03
 0.37097613678311E+03 0.11784389262023E+04 0.11201113254660E+04 0.10730659793197E+04 0.17326553355549E+04
 0.10701934580276E+04 0.11198462230030E+04 0.98927001654517E+03 0.17325767535021E+04 0.11784389262023E+04
 0.11201113254660E+04 0.10730659793197E+04 0.17326553355549E+04 0.10701934580276E+04 0.11198462230030E+04
 0.98927001654517E+03 0.17325767535021E+04 0.16989865568844E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48696758129792E+03 0.12948647540956E+01
 0.12948647540956E+01 0.31135327384082E+01 0.00000000000000E+00 0.33318002316696E+03 0.33318002316696E+03
 0.33318002316696E+03 0.33318002316696E+03 0.00000000000000E+00 0.00000000000000E+00 0.15662711163344E+00
 0.00000000000000E+00 -.12940721689675E+02 0.10000000000000E-02 0.13948498736477E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57353842525569E+01 0.21507690947088E+01 0.33062707587791E+03 0.40731728333384E+03
 0.30567180259615E+03 0.30567180259615E+03 0.29315002502030E+03 0.29315002441758E+03 0.30566856498832E+03
 0.30566856498832E+03 0.29315002502687E+03 0.29315002442399E+03 0.30567180259615E+03 0.30567180259615E+03
 0.29315002502030E+03 0.29315002441758E+03 0.30566856498832E+03 0.30566856498832E+03 0.29315002502687E+03
 0.29315002442399E+03 0.30385960978926E+03 0.29315012732644E+03 0.26213452053943E+01 0.47076716158823E+01
 0.18494068687597E+03 0.43995793117624E+03 0.25409254086590E+03 0.16572367194366E+03 0.17205531744370E+03
 0.16572367194366E+03 0.34872456282883E+03 0.16573853502356E+03 0.17191035766647E+03 0.16573853502356E+03
 0.34860471419951E+03 0.16572367194366E+03 0.17205531744370E+03 0.16572367194366E+03 0.34872456282883E+03
 0.16573853502356E+03 0.17191035766647E+03 0.16573853502356E+03 0.34860471419951E+03 0.21132273168324E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35806260776516E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17922233658760E+00 0.00000000000000E+00 0.00000000000000E+00 0.17922233658760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18747766321496E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18747766321496E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951440555703E+00 0.18965165756216E+00 0.33318002316696E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    830.25404950
 0.10036033830010E+00 0.31366391370376E+03 0.44779094130727E+03 0.44330393001878E+03 0.44147556438803E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14554821615319E+00 0.00000000000000E+00 -.21756315121741E+02
 0.35421158229596E-02 0.11951656965156E+01 0.22585371003807E+04 0.84695141264278E+03 0.66936325426031E+01
 0.25101122034762E+01 0.34161182064774E+03 0.29315078271003E+03 0.33442320448078E+03 0.36295384968305E+03
 0.29315010979257E+03 0.29315020647502E+03 0.33051956856991E+03 0.36293299240607E+03 0.29315009052102E+03
 0.29315020594034E+03 0.33442320448078E+03 0.36295384968304E+03 0.29315010979257E+03 0.29315020647502E+03
 0.33051956856991E+03 0.36293299240607E+03 0.29315009052102E+03 0.29315020594034E+03 0.40769076682902E+03
 0.33102331525108E+03 0.19882688434498E+04 0.18425292461540E+04 0.58956802068741E+03 0.96182388592555E+03
 0.36930802513471E+03 0.11800184257398E+04 0.11211993343895E+04 0.10740217747290E+04 0.17326642906362E+04
 0.10719016055885E+04 0.11209358140152E+04 0.99045029029237E+03 0.17325864114701E+04 0.11800184257398E+04
 0.11211993343895E+04 0.10740217747290E+04 0.17326642906362E+04 0.10719016055885E+04 0.11209358140152E+04
 0.99045029029237E+03 0.17325864114701E+04 0.16987339622202E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48719178987957E+03 0.12948647889996E+01
 0.12948647889996E+01 0.31410315203061E+01 0.00000000000000E+00 0.33331104071785E+03 0.33331104071785E+03
 0.33331104071785E+03 0.33331104071785E+03 0.00000000000000E+00 0.00000000000000E+00 0.15626089575395E+00
 0.00000000000000E+00 -.12932571388359E+02 0.10000000000000E-02 0.14012883598385E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57090319375251E+01 0.21408869765719E+01 0.33102497548232E+03 0.40768876660064E+03
 0.30575733224870E+03 0.30575733224870E+03 0.29315002719346E+03 0.29315002653840E+03 0.30575405950076E+03
 0.30575405950076E+03 0.29315002720056E+03 0.29315002654533E+03 0.30575733224871E+03 0.30575733224871E+03
 0.29315002719346E+03 0.29315002653840E+03 0.30575405950076E+03 0.30575405950076E+03 0.29315002720056E+03
 0.29315002654533E+03 0.30393385540778E+03 0.29315013708718E+03 0.80099037096114E+00 0.26180621804385E+01
 0.18566615725656E+03 0.44124507611673E+03 0.25465058807389E+03 0.16716431873433E+03 0.17269845315064E+03
 0.16716431873433E+03 0.34969476036443E+03 0.16717936782056E+03 0.17255288493588E+03 0.16717936782056E+03
 0.34957458153781E+03 0.16716431873432E+03 0.17269845315064E+03 0.16716431873432E+03 0.34969476036442E+03
 0.16717936782056E+03 0.17255288493588E+03 0.16717936782056E+03 0.34957458153781E+03 0.21142115984145E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35821200715390E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17916852152749E+00 0.00000000000000E+00 0.00000000000000E+00 0.17916852152749E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18740338637915E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18740338637915E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951437272657E+00 0.18957709007505E+00 0.33331104071785E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    840.56609271
 0.10027141604780E+00 0.31385688815222E+03 0.44812786168727E+03 0.44364001480377E+03 0.44181040452168E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14528481184882E+00 0.00000000000000E+00 -.21763436439979E+02
 0.35452567086593E-02 0.11979959105047E+01 0.22565361714033E+04 0.84620106427624E+03 0.66778191226293E+01
 0.25041821709860E+01 0.34200169794677E+03 0.29315087019870E+03 0.33476195609146E+03 0.36344585467071E+03
 0.29315012380745E+03 0.29315023283825E+03 0.33084292530843E+03 0.36342512333702E+03 0.29315010219024E+03
 0.29315023224329E+03 0.33476195609146E+03 0.36344585467071E+03 0.29315012380745E+03 0.29315023283825E+03
 0.33084292530843E+03 0.36342512333702E+03 0.29315010219024E+03 0.29315023224329E+03 0.40824449565665E+03
 0.33161664399930E+03 0.19915095378368E+04 0.18444773824703E+04 0.58831062327605E+03 0.95808627469122E+03
 0.36683409829879E+03 0.11823742258100E+04 0.11228167257961E+04 0.10754438960050E+04 0.17326775783838E+04
 0.10744492563842E+04 0.11225554845543E+04 0.99220661514884E+03 0.17326006860216E+04 0.11823742258100E+04
 0.11228167257961E+04 0.10754438960050E+04 0.17326775783839E+04 0.10744492563842E+04 0.11225554845543E+04
 0.99220661514884E+03 0.17326006860216E+04 0.16983559313878E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48752653206615E+03 0.12948648414711E+01
 0.12948648414711E+01 0.31822796931529E+01 0.00000000000000E+00 0.33350768067387E+03 0.33350768067387E+03
 0.33350768067387E+03 0.33350768067387E+03 0.00000000000000E+00 0.00000000000000E+00 0.15572329575117E+00
 0.00000000000000E+00 -.12920487128643E+02 0.10000000000000E-02 0.14107120783871E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56708949491284E+01 0.21265856059232E+01 0.33161838747308E+03 0.40824238111271E+03
 0.30588558228485E+03 0.30588558228485E+03 0.29315003075046E+03 0.29315003000971E+03 0.30588225706575E+03
 0.30588225706575E+03 0.29315003075842E+03 0.29315003001748E+03 0.30588558228485E+03 0.30588558228485E+03
 0.29315003075046E+03 0.29315003000971E+03 0.30588225706575E+03 0.30588225706575E+03 0.29315003075842E+03
 0.29315003001748E+03 0.30404522243031E+03 0.29315015287333E+03 -.19291219503981E+01 -.49765023703207E+00
 0.18674845556537E+03 0.44317105516133E+03 0.25548885731813E+03 0.16931933366692E+03 0.17365727807211E+03
 0.16931933366692E+03 0.35114541163029E+03 0.16933466116403E+03 0.17351080611015E+03 0.16933466116403E+03
 0.35102474479402E+03 0.16931933366692E+03 0.17365727807210E+03 0.16931933366692E+03 0.35114541163028E+03
 0.16933466116403E+03 0.17351080611015E+03 0.16933466116403E+03 0.35102474479402E+03 0.21157076065056E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35843601445797E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17908874804375E+00 0.00000000000000E+00 0.00000000000000E+00 0.17908874804375E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18729314222416E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18729314222416E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951431940143E+00 0.18946527957287E+00 0.33350768067387E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    850.87813592
 0.10018580346737E+00 0.31404893843624E+03 0.44846303800414E+03 0.44397424314996E+03 0.44214335161569E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14502878209263E+00 0.00000000000000E+00 -.21770537388874E+02
 0.35482859497925E-02 0.12007222826215E+01 0.22546097223274E+04 0.84547864587279E+03 0.66626563992250E+01
 0.24984961497094E+01 0.34238949537195E+03 0.29315096550377E+03 0.33509905263624E+03 0.36393464347236E+03
 0.29315013929260E+03 0.29315026196071E+03 0.33116485883372E+03 0.36391403499873E+03 0.29315011509827E+03
 0.29315026130013E+03 0.33509905263624E+03 0.36393464347235E+03 0.29315013929260E+03 0.29315026196071E+03
 0.33116485883372E+03 0.36391403499873E+03 0.29315011509827E+03 0.29315026130013E+03 0.40879393580307E+03
 0.33220591422335E+03 0.19947224864318E+04 0.18464035981652E+04 0.58705101395550E+03 0.95437914719868E+03
 0.36439287817340E+03 0.11847137778826E+04 0.11244158599612E+04 0.10768534560914E+04 0.17326890811611E+04
 0.10769793728718E+04 0.11241567899424E+04 0.99394739194204E+03 0.17326130986439E+04 0.11847137778826E+04
 0.11244158599612E+04 0.10768534560914E+04 0.17326890811611E+04 0.10769793728718E+04 0.11241567899424E+04
 0.99394739194204E+03 0.17326130986439E+04 0.16979781161095E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48785939918405E+03 0.12948648937926E+01
 0.12948648937926E+01 0.32235278659997E+01 0.00000000000000E+00 0.33370435280733E+03 0.33370435280733E+03
 0.33370435280733E+03 0.33370435280733E+03 0.00000000000000E+00 0.00000000000000E+00 0.15519946929163E+00
 0.00000000000000E+00 -.12908530786798E+02 0.10000000000000E-02 0.14198615263269E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56343522601780E+01 0.21128820975668E+01 0.33220773739493E+03 0.40879171370696E+03
 0.30601377128658E+03 0.30601377128658E+03 0.29315003469141E+03 0.29315003385573E+03 0.30601039387240E+03
 0.30601039387240E+03 0.29315003470031E+03 0.29315003386442E+03 0.30601377128658E+03 0.30601377128658E+03
 0.29315003469141E+03 0.29315003385573E+03 0.30601039387240E+03 0.30601039387240E+03 0.29315003470031E+03
 0.29315003386442E+03 0.30415658082471E+03 0.29315017011929E+03 -.46591185574855E+01 -.35895863739405E+01
 0.18782350657411E+03 0.44509020802115E+03 0.25632758391416E+03 0.17146714098397E+03 0.17460886559503E+03
 0.17146714098397E+03 0.35258958098189E+03 0.17148274623119E+03 0.17446149850040E+03 0.17148274623119E+03
 0.35246843286662E+03 0.17146714098397E+03 0.17460886559503E+03 0.17146714098397E+03 0.35258958098189E+03
 0.17148274623119E+03 0.17446149850040E+03 0.17148274623119E+03 0.35246843286662E+03 0.21172248571606E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35865981061147E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17900977145610E+00 0.00000000000000E+00 0.00000000000000E+00 0.17900977145610E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18718426792426E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18718426792426E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951426265061E+00 0.18935357873147E+00 0.33370435280733E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    862.88022895
 0.10009823441781E+00 0.31426582257465E+03 0.44884969597332E+03 0.44435915993721E+03 0.44252644725963E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14473987984480E+00 0.00000000000000E+00 -.21787096428132E+02
 0.35513897387208E-02 0.12037711293072E+01 0.22526392732332E+04 0.84473972746247E+03 0.66457815819228E+01
 0.24921680932210E+01 0.34283678807464E+03 0.29315108885503E+03 0.33548785071900E+03 0.36449935831903E+03
 0.29315015969728E+03 0.29315030032251E+03 0.33153601071939E+03 0.36447888803289E+03 0.29315013213153E+03
 0.29315029957710E+03 0.33548785071900E+03 0.36449935831903E+03 0.29315015969728E+03 0.29315030032251E+03
 0.33153601071939E+03 0.36447888803289E+03 0.29315013213153E+03 0.29315029957710E+03 0.40943582393659E+03
 0.33289292142545E+03 0.19984149649793E+04 0.18485761952826E+04 0.58545264376142E+03 0.94987434828427E+03
 0.36149444130404E+03 0.11874097744782E+04 0.11262356461188E+04 0.10784465489046E+04 0.17326726312690E+04
 0.10798970964985E+04 0.11259789452890E+04 0.99592678012817E+03 0.17325975965034E+04 0.11874097744782E+04
 0.11262356461188E+04 0.10784465489046E+04 0.17326726312690E+04 0.10798970964985E+04 0.11259789452890E+04
 0.99592678012817E+03 0.17325975965033E+04 0.16974561199378E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48824256847989E+03 0.12948650158035E+01
 0.12948650158035E+01 0.32715362381155E+01 0.00000000000000E+00 0.33393340052166E+03 0.33393340052166E+03
 0.33393340052166E+03 0.33393340052166E+03 0.00000000000000E+00 0.00000000000000E+00 0.15460669738755E+00
 0.00000000000000E+00 -.12903559453927E+02 0.10000000000000E-02 0.14301736178211E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55937264541268E+01 0.20976474202976E+01 0.33289481368682E+03 0.40943350851542E+03
 0.30616149700338E+03 0.30616149700338E+03 0.29315004055234E+03 0.29315003894121E+03 0.30615805944641E+03
 0.30615805944641E+03 0.29315004056262E+03 0.29315003895109E+03 0.30616149700338E+03 0.30616149700338E+03
 0.29315004055234E+03 0.29315003894121E+03 0.30615805944641E+03 0.30615805944641E+03 0.29315004056262E+03
 0.29315003895109E+03 0.30428489759819E+03 0.29315019252088E+03 -.78831430640272E+01 -.72059066051770E+01
 0.18907588436323E+03 0.44734110224892E+03 0.25731983846387E+03 0.17398688615892E+03 0.17571723384851E+03
 0.17398688615892E+03 0.35428484474370E+03 0.17400281134283E+03 0.17556876242779E+03 0.17400281134283E+03
 0.35416307006243E+03 0.17398688615892E+03 0.17571723384851E+03 0.17398688615892E+03 0.35428484474370E+03
 0.17400281134283E+03 0.17556876242779E+03 0.17400281134283E+03 0.35416307006243E+03 0.21190328741586E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35892063899184E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17898954468841E+00 0.00000000000000E+00 0.00000000000000E+00 0.17898954468841E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18707326629693E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18707326629693E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951392778961E+00 0.18922335584262E+00 0.33393340052166E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    871.44497424
 0.10005597801647E+00 0.31441615356090E+03 0.44912323653430E+03 0.44463048689009E+03 0.44279604327984E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14453972995899E+00 0.00000000000000E+00 -.21802605774361E+02
 0.35528893353633E-02 0.12058650589422E+01 0.22516884836161E+04 0.84438318135603E+03 0.66342414855419E+01
 0.24878405570782E+01 0.34315442041970E+03 0.29315118498369E+03 0.33576411693132E+03 0.36489970470258E+03
 0.29315017583862E+03 0.29315033065949E+03 0.33179994479275E+03 0.36487933081305E+03 0.29315014562204E+03
 0.29315032984805E+03 0.33576411693132E+03 0.36489970470258E+03 0.29315017583862E+03 0.29315033065949E+03
 0.33179994479275E+03 0.36487933081305E+03 0.29315014562204E+03 0.29315032984805E+03 0.40988773391359E+03
 0.33337807005058E+03 0.20009985514899E+04 0.18500522501190E+04 0.58433060901964E+03 0.94672295165409E+03
 0.35947068958935E+03 0.11893061973038E+04 0.11275025374777E+04 0.10795370649509E+04 0.17326352333083E+04
 0.10819496854409E+04 0.11272474508513E+04 0.99728974891357E+03 0.17325608205783E+04 0.11893061973038E+04
 0.11275025374777E+04 0.10795370649509E+04 0.17326352333083E+04 0.10819496854409E+04 0.11272474508513E+04
 0.99728974891357E+03 0.17325608205783E+04 0.16970623029436E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48851242428001E+03 0.12948651300800E+01
 0.12948651300800E+01 0.33057952192823E+01 0.00000000000000E+00 0.33409773134982E+03 0.33409773134982E+03
 0.33409773134982E+03 0.33409773134982E+03 0.00000000000000E+00 0.00000000000000E+00 0.15419446696401E+00
 0.00000000000000E+00 -.12905575104903E+02 0.10000000000000E-02 0.14373143994867E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55659360282321E+01 0.20872260105870E+01 0.33338001281952E+03 0.40988535058654E+03
 0.30626718314974E+03 0.30626718314974E+03 0.29315004475382E+03 0.29315004297578E+03 0.30626370264711E+03
 0.30626370264711E+03 0.29315004476507E+03 0.29315004298658E+03 0.30626718314974E+03 0.30626718314974E+03
 0.29315004475382E+03 0.29315004297578E+03 0.30626370264711E+03 0.30626370264711E+03 0.29315004476507E+03
 0.29315004298658E+03 0.30437673132845E+03 0.29315021003089E+03 -.10162819680785E+02 -.97361965638825E+01
 0.18996849538228E+03 0.44895739251860E+03 0.25803905465941E+03 0.17577519367612E+03 0.17650717476607E+03
 0.17577519367612E+03 0.35550312374717E+03 0.17579134826103E+03 0.17635794493272E+03 0.17579134826103E+03
 0.35538093272721E+03 0.17577519367612E+03 0.17650717476606E+03 0.17577519367612E+03 0.35550312374717E+03
 0.17579134826103E+03 0.17635794493272E+03 0.17579134826103E+03 0.35538093272721E+03 0.21204936123089E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35911052185879E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17900787267962E+00 0.00000000000000E+00 0.00000000000000E+00 0.17900787267962E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18705770448915E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18705770448915E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951351998859E+00 0.18912985017245E+00 0.33409773134982E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    880.73457314
 0.99963558819193E-01 0.31460169739286E+03 0.44942256915971E+03 0.44493017777807E+03 0.44309533795289E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14432792151843E+00 0.00000000000000E+00 -.21786874458817E+02
 0.35561737994055E-02 0.12080521104201E+01 0.22496088355798E+04 0.84360331334242E+03 0.66222308880517E+01
 0.24833365830194E+01 0.34349993519338E+03 0.29315129331066E+03 0.33606516002841E+03 0.36533107331179E+03
 0.29315019414405E+03 0.29315036505898E+03 0.33208835826506E+03 0.36531080308822E+03 0.29315016092901E+03
 0.29315036417317E+03 0.33606516002841E+03 0.36533107331179E+03 0.29315019414405E+03 0.29315036505898E+03
 0.33208835826506E+03 0.36531080308822E+03 0.29315016092901E+03 0.29315036417317E+03 0.41035894202557E+03
 0.33388940944526E+03 0.20038543076997E+04 0.18518615471664E+04 0.58340286098146E+03 0.94383383706339E+03
 0.35751396177702E+03 0.11913875175183E+04 0.11289346726203E+04 0.10808737487725E+04 0.17327006725401E+04
 0.10841971721180E+04 0.11286813279602E+04 0.99890815543307E+03 0.17326269376395E+04 0.11913875175183E+04
 0.11289346726203E+04 0.10808737487725E+04 0.17327006725401E+04 0.10841971721180E+04 0.11286813279602E+04
 0.99890815543307E+03 0.17326269376395E+04 0.16968829154276E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48881126603745E+03 0.12948650141680E+01
 0.12948650141680E+01 0.33429536148535E+01 0.00000000000000E+00 0.33427401417674E+03 0.33427401417674E+03
 0.33427401417674E+03 0.33427401417674E+03 0.00000000000000E+00 0.00000000000000E+00 0.15375746098513E+00
 0.00000000000000E+00 -.12870709157168E+02 0.10000000000000E-02 0.14448678645196E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55368384863758E+01 0.20763144323909E+01 0.33389146819274E+03 0.41035640351931E+03
 0.30638446157418E+03 0.30638446157418E+03 0.29315004798189E+03 0.29315004755683E+03 0.30638093437967E+03
 0.30638093437967E+03 0.29315004799385E+03 0.29315004756868E+03 0.30638446157418E+03 0.30638446157418E+03
 0.29315004798189E+03 0.29315004755683E+03 0.30638093437967E+03 0.30638093437967E+03 0.29315004799385E+03
 0.29315004756868E+03 0.30447884277389E+03 0.29315022978788E+03 -.12556295718484E+02 -.12362043750114E+02
 0.19090110191871E+03 0.45061601351974E+03 0.25876040609144E+03 0.17764770963199E+03 0.17732898625675E+03
 0.17764770963199E+03 0.35674170894648E+03 0.17766411645608E+03 0.17717906478075E+03 0.17766411645608E+03
 0.35661919579020E+03 0.17764770963199E+03 0.17732898625675E+03 0.17764770963199E+03 0.35674170894648E+03
 0.17766411645608E+03 0.17717906478075E+03 0.17766411645608E+03 0.35661919579020E+03 0.21218876384990E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35930669331742E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17875162373719E+00 0.00000000000000E+00 0.00000000000000E+00 0.17875162373719E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18688144426707E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18688144426707E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951419701952E+00 0.18903088860702E+00 0.33427401417674E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    891.24788559
 0.99875327982044E-01 0.31479525486947E+03 0.44975823680935E+03 0.44526507944713E+03 0.44342905461694E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14409437333448E+00 0.00000000000000E+00 -.21799698726828E+02
 0.35593150422609E-02 0.12104448600932E+01 0.22476234626644E+04 0.84285879849915E+03 0.66091403778474E+01
 0.24784276416928E+01 0.34388766809910E+03 0.29315142624098E+03 0.33640283026081E+03 0.36581736529823E+03
 0.29315021691124E+03 0.29315040782947E+03 0.33241143436246E+03 0.36579720906724E+03 0.29315017998733E+03
 0.29315040685252E+03 0.33640283026081E+03 0.36581736529823E+03 0.29315021691124E+03 0.29315040782947E+03
 0.33241143436246E+03 0.36579720906724E+03 0.29315017998733E+03 0.29315040685252E+03 0.41090324023721E+03
 0.33447516468667E+03 0.20070438250739E+04 0.18537688971256E+04 0.58211787175015E+03 0.94017920552378E+03
 0.35515074441488E+03 0.11937203018497E+04 0.11305067488569E+04 0.10822763440142E+04 0.17327193181671E+04
 0.10867199028535E+04 0.11302552311717E+04 0.10006378831589E+04 0.17326462372921E+04 0.11937203018497E+04
 0.11305067488569E+04 0.10822763440142E+04 0.17327193181671E+04 0.10867199028535E+04 0.11302552311717E+04
 0.10006378831589E+04 0.17326462372921E+04 0.16965172455245E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48914484825778E+03 0.12948651086602E+01
 0.12948651086602E+01 0.33850068646498E+01 0.00000000000000E+00 0.33447315384426E+03 0.33447315384426E+03
 0.33447315384426E+03 0.33447315384426E+03 0.00000000000000E+00 0.00000000000000E+00 0.15327516875207E+00
 0.00000000000000E+00 -.12864729117910E+02 0.10000000000000E-02 0.14531739975981E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55051907157869E+01 0.20644465184201E+01 0.33447729145524E+03 0.41090062066710E+03
 0.30651485863709E+03 0.30651485863709E+03 0.29315005447271E+03 0.29315005326917E+03 0.30651127922368E+03
 0.30651127922368E+03 0.29315005448614E+03 0.29315005328230E+03 0.30651485863709E+03 0.30651485863709E+03
 0.29315005447271E+03 0.29315005326917E+03 0.30651127922368E+03 0.30651127922368E+03 0.29315005448614E+03
 0.29315005328230E+03 0.30459229906454E+03 0.29315025409718E+03 -.15350509894168E+02 -.15351864693670E+02
 0.19196306907831E+03 0.45252613259492E+03 0.25960324817122E+03 0.17980760231727E+03 0.17826476466760E+03
 0.17980760231727E+03 0.35817232273054E+03 0.17982429025738E+03 0.17811393804516E+03 0.17982429025738E+03
 0.35804931942916E+03 0.17980760231727E+03 0.17826476466760E+03 0.17980760231727E+03 0.35817232273054E+03
 0.17982429025738E+03 0.17811393804516E+03 0.17982429025738E+03 0.35804931942916E+03 0.21235876071930E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35953312191851E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17872107770488E+00 0.00000000000000E+00 0.00000000000000E+00 0.17872107770488E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18678055137230E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18678055137230E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951395716066E+00 0.18891810153884E+00 0.33447315384426E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    900.15458019
 0.99813126357942E-01 0.31495538434228E+03 0.45004048539343E+03 0.44554606330666E+03 0.44370874921208E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14390169469377E+00 0.00000000000000E+00 -.21807075662757E+02
 0.35615328754459E-02 0.12124019552748E+01 0.22462238254641E+04 0.84233393454902E+03 0.65984717075014E+01
 0.24744268903130E+01 0.34421441237875E+03 0.29315154777990E+03 0.33668748003999E+03 0.36622685323484E+03
 0.29315023799454E+03 0.29315044742365E+03 0.33268389386347E+03 0.36620679127376E+03 0.29315019765405E+03
 0.29315044636349E+03 0.33668748003999E+03 0.36622685323484E+03 0.29315023799454E+03 0.29315044742365E+03
 0.33268389386347E+03 0.36620679127376E+03 0.29315019765405E+03 0.29315044636349E+03 0.41136085876748E+03
 0.33496788072005E+03 0.20097076029945E+04 0.18553291437642E+04 0.58101894378894E+03 0.93709111872181E+03
 0.35316708021393E+03 0.11956757359630E+04 0.11318125036182E+04 0.10834288175041E+04 0.17327155340940E+04
 0.10888349397711E+04 0.11315624655633E+04 0.10020655738228E+04 0.17326429618261E+04 0.11956757359630E+04
 0.11318125036182E+04 0.10834288175041E+04 0.17327155340940E+04 0.10888349397711E+04 0.11315624655633E+04
 0.10020655738228E+04 0.17326429618261E+04 0.16961821886071E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48942456842609E+03 0.12948651630152E+01
 0.12948651630152E+01 0.34206336430831E+01 0.00000000000000E+00 0.33464206952678E+03 0.33464206952678E+03
 0.33464206952678E+03 0.33464206952678E+03 0.00000000000000E+00 0.00000000000000E+00 0.15287650895383E+00
 0.00000000000000E+00 -.12856497840974E+02 0.10000000000000E-02 0.14600132354958E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54794023817758E+01 0.20547758931659E+01 0.33497004207174E+03 0.41135819763400E+03
 0.30662520944781E+03 0.30662520944781E+03 0.29315005989527E+03 0.29315005857192E+03 0.30662158590142E+03
 0.30662158590142E+03 0.29315005990990E+03 0.29315005858623E+03 0.30662520944781E+03 0.30662520944781E+03
 0.29315005989527E+03 0.29315005857192E+03 0.30662158590142E+03 0.30662158590142E+03 0.29315005990990E+03
 0.29315005858623E+03 0.30468834445363E+03 0.29315027637961E+03 -.17711130671147E+02 -.18088479878215E+02
 0.19286028372775E+03 0.45414506223263E+03 0.26032047708624E+03 0.18163215428050E+03 0.17905515234379E+03
 0.18163215428050E+03 0.35938479073275E+03 0.18164908048719E+03 0.17890356616413E+03 0.18164908048719E+03
 0.35926137892847E+03 0.18163215428050E+03 0.17905515234379E+03 0.18163215428050E+03 0.35938479073274E+03
 0.18164908048719E+03 0.17890356616413E+03 0.18164908048719E+03 0.35926137892847E+03 0.21245420756837E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35972508065629E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17866732149492E+00 0.00000000000000E+00 0.00000000000000E+00 0.17866732149492E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18670114399535E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18670114399535E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951384914117E+00 0.18882264397482E+00 0.33464206952678E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    912.03017301
 0.99711631444068E-01 0.31517362399128E+03 0.45041492778893E+03 0.44591988410884E+03 0.44408138628383E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14365193312366E+00 0.00000000000000E+00 -.21800538768818E+02
 0.35651577517830E-02 0.12149123368878E+01 0.22439399760078E+04 0.84147749100294E+03 0.65848372405972E+01
 0.24693139652240E+01 0.34464791577126E+03 0.29315172271376E+03 0.33706531825450E+03 0.36676936202083E+03
 0.29315026873438E+03 0.29315050513208E+03 0.33304572960019E+03 0.36674942268935E+03 0.29315022343906E+03
 0.29315050395233E+03 0.33706531825450E+03 0.36676936202083E+03 0.29315026873438E+03 0.29315050513208E+03
 0.33304572960019E+03 0.36674942268935E+03 0.29315022343906E+03 0.29315050395233E+03 0.41196595205114E+03
 0.33561611044588E+03 0.20132455896485E+04 0.18574433482285E+04 0.57956151242330E+03 0.93302189609924E+03
 0.35056257611382E+03 0.11982720034719E+04 0.11335366651279E+04 0.10849914813144E+04 0.17327107684277E+04
 0.10916432963765E+04 0.11332885154001E+04 0.10039914452560E+04 0.17326388202746E+04 0.11982720034719E+04
 0.11335366651279E+04 0.10849914813144E+04 0.17327107684277E+04 0.10916432963765E+04 0.11332885154001E+04
 0.10039914452560E+04 0.17326388202746E+04 0.16957582650374E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48979698294657E+03 0.12948651148498E+01
 0.12948651148498E+01 0.34681360143274E+01 0.00000000000000E+00 0.33486783341959E+03 0.33486783341959E+03
 0.33486783341959E+03 0.33486783341959E+03 0.00000000000000E+00 0.00000000000000E+00 0.15235892673360E+00
 0.00000000000000E+00 -.12826721797210E+02 0.10000000000000E-02 0.14688538350001E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54464234693572E+01 0.20424088010089E+01 0.33561834857337E+03 0.41196321299833E+03
 0.30677230718560E+03 0.30677230718560E+03 0.29315006782097E+03 0.29315006632250E+03 0.30676862509606E+03
 0.30676862509606E+03 0.29315006783733E+03 0.29315006633850E+03 0.30677230718561E+03 0.30677230718561E+03
 0.29315006782097E+03 0.29315006632250E+03 0.30676862509606E+03 0.30676862509606E+03 0.29315006783733E+03
 0.29315006633850E+03 0.30481642641141E+03 0.29315030853312E+03 -.20819014362166E+02 -.21956119756387E+02
 0.19405308083654E+03 0.45629737523814E+03 0.26127402899742E+03 0.18404466992065E+03 0.18010591521300E+03
 0.18404466992065E+03 0.36099496742177E+03 0.18406191342319E+03 0.17995334621958E+03 0.18406191342319E+03
 0.36087103789278E+03 0.18404466992065E+03 0.18010591521299E+03 0.18404466992065E+03 0.36099496742177E+03
 0.18406191342319E+03 0.17995334621957E+03 0.18406191342319E+03 0.36087103789278E+03 0.21252551999867E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35997612554572E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17845818240656E+00 0.00000000000000E+00 0.00000000000000E+00 0.17845818240656E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18650176970410E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18650176970410E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951427021987E+00 0.18869584009732E+00 0.33486783341959E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    920.08254014
 0.99633595189513E-01 0.31532428050747E+03 0.45066873939486E+03 0.44617378771889E+03 0.44433474193129E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14348685912223E+00 0.00000000000000E+00 -.21806893505329E+02
 0.35679498585535E-02 0.12165520461378E+01 0.22421839759944E+04 0.84081899099788E+03 0.65759619782790E+01
 0.24659857418546E+01 0.34494116340563E+03 0.29315184903520E+03 0.33732110874119E+03 0.36713497294582E+03
 0.29315029116720E+03 0.29315054723256E+03 0.33329096107646E+03 0.36711511508371E+03 0.29315024227185E+03
 0.29315054596659E+03 0.33732110874119E+03 0.36713497294582E+03 0.29315029116720E+03 0.29315054723256E+03
 0.33329096107646E+03 0.36711511508371E+03 0.29315024227185E+03 0.29315054596659E+03 0.41236981485266E+03
 0.33604704857054E+03 0.20156471615334E+04 0.18589027559976E+04 0.57864386443651E+03 0.93040527502060E+03
 0.34886819126192E+03 0.12000324114913E+04 0.11347147220809E+04 0.10860685622976E+04 0.17327369025964E+04
 0.10935460900094E+04 0.11344678113151E+04 0.10053108820866E+04 0.17326653536400E+04 0.12000324114913E+04
 0.11347147220809E+04 0.10860685622976E+04 0.17327369025964E+04 0.10935460900094E+04 0.11344678113151E+04
 0.10053108820865E+04 0.17326653536400E+04 0.16955297949777E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49005006365456E+03 0.12948651616731E+01
 0.12948651616731E+01 0.35003454828552E+01 0.00000000000000E+00 0.33502197160054E+03 0.33502197160054E+03
 0.33502197160054E+03 0.33502197160054E+03 0.00000000000000E+00 0.00000000000000E+00 0.15201682675522E+00
 0.00000000000000E+00 -.12818977019853E+02 0.10000000000000E-02 0.14746684499660E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54249482317088E+01 0.20343555868908E+01 0.33604936476569E+03 0.41236700228786E+03
 0.30687267492639E+03 0.30687267492639E+03 0.29315007283174E+03 0.29315007198995E+03 0.30686895332608E+03
 0.30686895332608E+03 0.29315007284916E+03 0.29315007200717E+03 0.30687267492639E+03 0.30687267492639E+03
 0.29315007283174E+03 0.29315007198995E+03 0.30686895332608E+03 0.30686895332608E+03 0.29315007284916E+03
 0.29315007200717E+03 0.30490389130924E+03 0.29315033179982E+03 -.22871679438578E+02 -.24599232768551E+02
 0.19486059931809E+03 0.45776510476056E+03 0.26193020244588E+03 0.18565622567408E+03 0.18081750829761E+03
 0.18565622567408E+03 0.36209418343965E+03 0.18567368453460E+03 0.18066432011875E+03 0.18567368453460E+03
 0.36196994954726E+03 0.18565622567408E+03 0.18081750829760E+03 0.18565622567408E+03 0.36209418343964E+03
 0.18567368453460E+03 0.18066432011875E+03 0.18567368453460E+03 0.36196994954726E+03 0.21257315624289E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36015069942127E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17840690926130E+00 0.00000000000000E+00 0.00000000000000E+00 0.17840690926130E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18643068497520E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18643068497520E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951417942943E+00 0.18860894323167E+00 0.33502197160054E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    931.04465593
 0.99531160656843E-01 0.31552392142612E+03 0.45101218246753E+03 0.44651708117525E+03 0.44467710454928E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14326746304040E+00 0.00000000000000E+00 -.21813335428644E+02
 0.35716215632837E-02 0.12187120658138E+01 0.22398789620491E+04 0.83995461076840E+03 0.65643068813453E+01
 0.24616150805045E+01 0.34533815394342E+03 0.29315203329426E+03 0.33766742470158E+03 0.36763030267093E+03
 0.29315032427102E+03 0.29315060933730E+03 0.33362294270749E+03 0.36761055318105E+03 0.29315027008894E+03
 0.29315060794575E+03 0.33766742470158E+03 0.36763030267093E+03 0.29315032427102E+03 0.29315060933730E+03
 0.33362294270749E+03 0.36761055318105E+03 0.29315027008894E+03 0.29315060794575E+03 0.41292029324529E+03
 0.33663118554763E+03 0.20188898708942E+04 0.18608370291677E+04 0.57732420617988E+03 0.92674676356137E+03
 0.34653593635060E+03 0.12024136561532E+04 0.11362924176074E+04 0.10874949640654E+04 0.17327522859490E+04
 0.10961210946712E+04 0.11360471096022E+04 0.10070681924797E+04 0.17326812212450E+04 0.12024136561532E+04
 0.11362924176074E+04 0.10874949640654E+04 0.17327522859490E+04 0.10961210946712E+04 0.11360471096022E+04
 0.10070681924797E+04 0.17326812212450E+04 0.16951699525198E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49039214977614E+03 0.12948652091387E+01
 0.12948652091387E+01 0.35441939460326E+01 0.00000000000000E+00 0.33523313799806E+03 0.33523313799806E+03
 0.33523313799806E+03 0.33523313799806E+03 0.00000000000000E+00 0.00000000000000E+00 0.15156228837295E+00
 0.00000000000000E+00 -.12805737470829E+02 0.10000000000000E-02 0.14823574245273E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53968090742698E+01 0.20238034028512E+01 0.33663358064363E+03 0.41291741414653E+03
 0.30700866919849E+03 0.30700866919849E+03 0.29315008131160E+03 0.29315008037181E+03 0.30700489417583E+03
 0.30700489417583E+03 0.29315008133081E+03 0.29315008039079E+03 0.30700866919849E+03 0.30700866919849E+03
 0.29315008131160E+03 0.29315008037181E+03 0.30700489417583E+03 0.30700489417583E+03 0.29315008133081E+03
 0.29315008039079E+03 0.30502239190894E+03 0.29315036581542E+03 -.25662040673740E+02 -.28271335535999E+02
 0.19596859501107E+03 0.45978914301110E+03 0.26284070502497E+03 0.18785600459977E+03 0.18179469903705E+03
 0.18785600459977E+03 0.36361198055138E+03 0.18787375495373E+03 0.18164064410435E+03 0.18787375495373E+03
 0.36348730600655E+03 0.18785600459977E+03 0.18179469903705E+03 0.18785600459977E+03 0.36361198055137E+03
 0.18787375495373E+03 0.18164064410435E+03 0.18787375495373E+03 0.36348730600655E+03 0.21263290701406E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36038786720407E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17831973490940E+00 0.00000000000000E+00 0.00000000000000E+00 0.17831973490940E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18631022727285E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18631022727285E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951413464377E+00 0.18849011436145E+00 0.33523313799806E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    940.16323102
 0.99448643667097E-01 0.31568814580763E+03 0.45129621905382E+03 0.44680087273560E+03 0.44496006229796E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14308955592063E+00 0.00000000000000E+00 -.21818488053254E+02
 0.35745848300931E-02 0.12204462667932E+01 0.22380221425020E+04 0.83925830343824E+03 0.65549792872245E+01
 0.24581172327092E+01 0.34566663653246E+03 0.29315219791674E+03 0.33795405421951E+03 0.36803999901778E+03
 0.29315035420532E+03 0.29315066547355E+03 0.33389776735308E+03 0.36802033752053E+03 0.29315029526687E+03
 0.29315066397002E+03 0.33795405421951E+03 0.36803999901778E+03 0.29315035420532E+03 0.29315066547355E+03
 0.33389776735308E+03 0.36802033752053E+03 0.29315029526687E+03 0.29315066397002E+03 0.41337629837439E+03
 0.33711306793541E+03 0.20215651405251E+04 0.18624213091547E+04 0.57620342750141E+03 0.92368670312713E+03
 0.34460225848822E+03 0.12043816084941E+04 0.11375871130681E+04 0.10886651548621E+04 0.17327561240213E+04
 0.10982495814392E+04 0.11373430764334E+04 0.10085123934594E+04 0.17326854214991E+04 0.12043816084941E+04
 0.11375871130681E+04 0.10886651548621E+04 0.17327561240213E+04 0.10982495814392E+04 0.11373430764334E+04
 0.10085123934594E+04 0.17326854214991E+04 0.16948547099278E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49067490881570E+03 0.12948652471045E+01
 0.12948652471045E+01 0.35806682463720E+01 0.00000000000000E+00 0.33540992028243E+03 0.33540992028243E+03
 0.33540992028243E+03 0.33540992028243E+03 0.00000000000000E+00 0.00000000000000E+00 0.15119376606410E+00
 0.00000000000000E+00 -.12794514167405E+02 0.10000000000000E-02 0.14885592096962E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53743243452388E+01 0.20153716294646E+01 0.33711551596130E+03 0.41337337308175E+03
 0.30712160482026E+03 0.30712160482026E+03 0.29315008949010E+03 0.29315008796846E+03 0.30711778562686E+03
 0.30711778562686E+03 0.29315008951102E+03 0.29315008798903E+03 0.30712160482026E+03 0.30712160482026E+03
 0.29315008949010E+03 0.29315008796846E+03 0.30711778562686E+03 0.30711778562686E+03 0.29315008951102E+03
 0.29315008798903E+03 0.30512081298748E+03 0.29315039627785E+03 -.27964991688792E+02 -.31356856006099E+02
 0.19689546321711E+03 0.46149052241941E+03 0.26361058188621E+03 0.18968263572555E+03 0.18261267519650E+03
 0.18968263572555E+03 0.36488917210452E+03 0.18970062766515E+03 0.18245789688704E+03 0.18970062766515E+03
 0.36476412695121E+03 0.18968263572555E+03 0.18261267519649E+03 0.18968263572555E+03 0.36488917210452E+03
 0.18970062766515E+03 0.18245789688704E+03 0.18970062766515E+03 0.36476412695121E+03 0.21268382615215E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36058562722281E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17824637877189E+00 0.00000000000000E+00 0.00000000000000E+00 0.17824637877189E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18620612015017E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18620612015017E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951410177813E+00 0.18839075451210E+00 0.33540992028243E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    951.13972810
 0.99355860629488E-01 0.31588409059908E+03 0.45163622749681E+03 0.44714030403222E+03 0.44529837300050E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14288081370356E+00 0.00000000000000E+00 -.21826168474238E+02
 0.35779226213212E-02 0.12224600484056E+01 0.22359343246629E+04 0.83847537174860E+03 0.65441811455792E+01
 0.24540679295922E+01 0.34606012526332E+03 0.29315241054231E+03 0.33829753633815E+03 0.36853027714887E+03
 0.29315039333404E+03 0.29315073882149E+03 0.33422723840479E+03 0.36851071907828E+03 0.29315032820977E+03
 0.29315073717362E+03 0.33829753633815E+03 0.36853027714887E+03 0.29315039333404E+03 0.29315073882149E+03
 0.33422723840479E+03 0.36851071907828E+03 0.29315032820977E+03 0.29315073717362E+03 0.41392117260747E+03
 0.33768694774074E+03 0.20247564483694E+04 0.18642984650198E+04 0.57485358815244E+03 0.92002966197463E+03
 0.34230180588143E+03 0.12067340496435E+04 0.11391265992337E+04 0.10900557799270E+04 0.17327537623788E+04
 0.11007939895094E+04 0.11388840273103E+04 0.10102302260913E+04 0.17326834548337E+04 0.12067340496435E+04
 0.11391265992337E+04 0.10900557799270E+04 0.17327537623788E+04 0.11007939895094E+04 0.11388840273103E+04
 0.10102302260913E+04 0.17326834548337E+04 0.16944690397701E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49101303737243E+03 0.12948653036956E+01
 0.12948653036956E+01 0.36245742347203E+01 0.00000000000000E+00 0.33562414729407E+03 0.33562414729407E+03
 0.33562414729407E+03 0.33562414729407E+03 0.00000000000000E+00 0.00000000000000E+00 0.15076135933767E+00
 0.00000000000000E+00 -.12783066406137E+02 0.10000000000000E-02 0.14957971051600E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53483189480731E+01 0.20056196055274E+01 0.33768945519075E+03 0.41391819253225E+03
 0.30725761270079E+03 0.30725761270079E+03 0.29315009978676E+03 0.29315009792100E+03 0.30725374059508E+03
 0.30725374059508E+03 0.29315009980979E+03 0.29315009794360E+03 0.30725761270079E+03 0.30725761270079E+03
 0.29315009978676E+03 0.29315009792100E+03 0.30725374059508E+03 0.30725374059508E+03 0.29315009980979E+03
 0.29315009794360E+03 0.30523936802737E+03 0.29315043571516E+03 -.30703109085410E+02 -.35078039511774E+02
 0.19801652744933E+03 0.46355880078465E+03 0.26455219069807E+03 0.19187161833010E+03 0.18360261316569E+03
 0.19187161833010E+03 0.36644346701478E+03 0.19188990058978E+03 0.18344697583050E+03 0.19188990058978E+03
 0.36631798615863E+03 0.19187161833010E+03 0.18360261316569E+03 0.19187161833010E+03 0.36644346701478E+03
 0.19188990058978E+03 0.18344697583050E+03 0.19188990058978E+03 0.36631798615863E+03 0.21275336510719E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36082553387847E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17817165128186E+00 0.00000000000000E+00 0.00000000000000E+00 0.17817165128186E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18609834809621E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18609834809621E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951399770099E+00 0.18827041787687E+00 0.33562414729407E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    961.71767852
 0.99275924788634E-01 0.31607137028676E+03 0.45196198858935E+03 0.44746509965532E+03 0.44562192588311E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14268519239791E+00 0.00000000000000E+00 -.21835914249691E+02
 0.35808032191386E-02 0.12243252263879E+01 0.22341356143900E+04 0.83780085539625E+03 0.65342115212331E+01
 0.24503293204624E+01 0.34643735405243E+03 0.29315263108782E+03 0.33862696118301E+03 0.36899982615124E+03
 0.29315043443288E+03 0.29315081582673E+03 0.33454335326688E+03 0.36898036525421E+03 0.29315036284605E+03
 0.29315081402949E+03 0.33862696118301E+03 0.36899982615124E+03 0.29315043443288E+03 0.29315081582673E+03
 0.33454335326688E+03 0.36898036525421E+03 0.29315036284605E+03 0.29315081402949E+03 0.41444240631258E+03
 0.33823404543840E+03 0.20278011748647E+04 0.18660775880897E+04 0.57354897286069E+03 0.91652675018441E+03
 0.34011003245941E+03 0.12089839148688E+04 0.11405912503677E+04 0.10913794194671E+04 0.17327449761158E+04
 0.11032275247582E+04 0.11403500210458E+04 0.10118663225237E+04 0.17326750050313E+04 0.12089839148688E+04
 0.11405912503677E+04 0.10913794194671E+04 0.17327449761158E+04 0.11032275247582E+04 0.11403500210458E+04
 0.10118663225237E+04 0.17326750050313E+04 0.16940898588592E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49133649229937E+03 0.12948653755049E+01
 0.12948653755049E+01 0.36668860363734E+01 0.00000000000000E+00 0.33583208934273E+03 0.33583208934273E+03
 0.33583208934273E+03 0.33583208934273E+03 0.00000000000000E+00 0.00000000000000E+00 0.15035590932641E+00
 0.00000000000000E+00 -.12774920701447E+02 0.10000000000000E-02 0.15025434020679E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53243054336997E+01 0.19966145376374E+01 0.33823661259544E+03 0.41443937041692E+03
 0.30738874817513E+03 0.30738874817513E+03 0.29315011068027E+03 0.29315010839949E+03 0.30738482535748E+03
 0.30738482535748E+03 0.29315011070548E+03 0.29315010842418E+03 0.30738874817513E+03 0.30738874817513E+03
 0.29315011068027E+03 0.29315010839949E+03 0.30738482535748E+03 0.30738482535748E+03 0.29315011070548E+03
 0.29315010842418E+03 0.30535370093856E+03 0.29315047672169E+03 -.33309167102060E+02 -.38664197058141E+02
 0.19910304740419E+03 0.46557357258952E+03 0.26547500994832E+03 0.19397249610735E+03 0.18456261275281E+03
 0.19397249610735E+03 0.36795924743271E+03 0.19399105752714E+03 0.18440615408788E+03 0.19399105752714E+03
 0.36783335172490E+03 0.19397249610735E+03 0.18456261275281E+03 0.19397249610735E+03 0.36795924743271E+03
 0.19399105752714E+03 0.18440615408788E+03 0.19399105752714E+03 0.36783335172490E+03 0.21283091687241E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36105854036558E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17812061019729E+00 0.00000000000000E+00 0.00000000000000E+00 0.17812061019729E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18601008637781E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18601008637781E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951380797673E+00 0.18815366001639E+00 0.33583208934273E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    972.29562893
 0.99175330527829E-01 0.31626296314283E+03 0.45228630488487E+03 0.44778958492595E+03 0.44594571733142E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14249474974603E+00 0.00000000000000E+00 -.21830214930906E+02
 0.35844349478262E-02 0.12261192252879E+01 0.22318720011509E+04 0.83695200043159E+03 0.65246509760270E+01
 0.24467441160101E+01 0.34681290347620E+03 0.29315286748474E+03 0.33895508875032E+03 0.36946655142266E+03
 0.29315047901279E+03 0.29315089931558E+03 0.33485838296108E+03 0.36944718538253E+03 0.29315040045173E+03
 0.29315089735861E+03 0.33895508875032E+03 0.36946655142266E+03 0.29315047901279E+03 0.29315089931558E+03
 0.33485838296108E+03 0.36944718538253E+03 0.29315040045173E+03 0.29315089735861E+03 0.41495920707670E+03
 0.33877464618527E+03 0.20308412063660E+04 0.18678940722809E+04 0.57225903598803E+03 0.91307563840193E+03
 0.33795530723396E+03 0.12112278608855E+04 0.11420453488518E+04 0.10927286985788E+04 0.17327400954115E+04
 0.11056546926482E+04 0.11418054104067E+04 0.10135252267585E+04 0.17326704320480E+04 0.12112278608855E+04
 0.11420453488518E+04 0.10927286985788E+04 0.17327400954115E+04 0.11056546926482E+04 0.11418054104067E+04
 0.10135252267585E+04 0.17326704320480E+04 0.16937375505103E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49165992369522E+03 0.12948653335109E+01
 0.12948653335109E+01 0.37091978380264E+01 0.00000000000000E+00 0.33603964453606E+03 0.33603964453606E+03
 0.33603964453606E+03 0.33603964453606E+03 0.00000000000000E+00 0.00000000000000E+00 0.14996132961839E+00
 0.00000000000000E+00 -.12749064486630E+02 0.10000000000000E-02 0.15090774834528E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53012519819034E+01 0.19879694932138E+01 0.33877727490344E+03 0.41495611523640E+03
 0.30752003512168E+03 0.30752003512168E+03 0.29315012231152E+03 0.29315011979105E+03 0.30751606184614E+03
 0.30751606184614E+03 0.29315012233901E+03 0.29315011981798E+03 0.30752003512168E+03 0.30752003512168E+03
 0.29315012231152E+03 0.29315011979105E+03 0.30751606184614E+03 0.30751606184614E+03 0.29315012233901E+03
 0.29315011981798E+03 0.30546820481968E+03 0.29315052077732E+03 -.35889998982658E+02 -.42252437147381E+02
 0.20018212627931E+03 0.46756772722927E+03 0.26638469031856E+03 0.19605477901232E+03 0.18551509551454E+03
 0.19605477901232E+03 0.36945622507563E+03 0.19607361936396E+03 0.18535782933336E+03 0.19607361936396E+03
 0.36932992546516E+03 0.19605477901232E+03 0.18551509551454E+03 0.19605477901232E+03 0.36945622507563E+03
 0.19607361936396E+03 0.18535782933336E+03 0.19607361936396E+03 0.36932992546516E+03 0.21289738118909E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36128625865711E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17793928481672E+00 0.00000000000000E+00 0.00000000000000E+00 0.17793928481672E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18583548756575E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18583548756575E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951415247448E+00 0.18803785559817E+00 0.33603964453606E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    980.22909174
 0.99098524669149E-01 0.31640526515453E+03 0.45252871864282E+03 0.44803217417163E+03 0.44618778990128E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14235505821644E+00 0.00000000000000E+00 -.21833621561147E+02
 0.35872128189632E-02 0.12274211801115E+01 0.22301436808291E+04 0.83630388031093E+03 0.65177301236347E+01
 0.24441487963630E+01 0.34709347473215E+03 0.29315305566875E+03 0.33920032112735E+03 0.36981481422176E+03
 0.29315051486540E+03 0.29315096643208E+03 0.33509392097700E+03 0.36979551785269E+03 0.29315043072014E+03
 0.29315096434822E+03 0.33920032112735E+03 0.36981481422176E+03 0.29315051486540E+03 0.29315096643208E+03
 0.33509392097700E+03 0.36979551785269E+03 0.29315043072014E+03 0.29315096434822E+03 0.41534429916281E+03
 0.33917618386662E+03 0.20331125574939E+04 0.18692442399127E+04 0.57129803768172E+03 0.91051447251834E+03
 0.33635994464821E+03 0.12129053126891E+04 0.11431307698564E+04 0.10937302273777E+04 0.17327413455664E+04
 0.11074689129012E+04 0.11428917634042E+04 0.10147578501915E+04 0.17326718914986E+04 0.12129053126891E+04
 0.11431307698564E+04 0.10937302273777E+04 0.17327413455664E+04 0.11074689129012E+04 0.11428917634042E+04
 0.10147578501915E+04 0.17326718914986E+04 0.16934796844732E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49190172744878E+03 0.12948653586118E+01
 0.12948653586118E+01 0.37409316892662E+01 0.00000000000000E+00 0.33619556729925E+03 0.33619556729925E+03
 0.33619556729925E+03 0.33619556729925E+03 0.00000000000000E+00 0.00000000000000E+00 0.14967231454432E+00
 0.00000000000000E+00 -.12738487950204E+02 0.10000000000000E-02 0.15138404873169E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52845726263928E+01 0.19817147348973E+01 0.33917885794731E+03 0.41534116830363E+03
 0.30761854170473E+03 0.30761854170473E+03 0.29315013168379E+03 0.29315012897019E+03 0.30761453075635E+03
 0.30761453075635E+03 0.29315013171309E+03 0.29315012899888E+03 0.30761854170473E+03 0.30761854170473E+03
 0.29315013168379E+03 0.29315012897019E+03 0.30761453075635E+03 0.30761453075635E+03 0.29315013171309E+03
 0.29315012899888E+03 0.30555414150439E+03 0.29315055591756E+03 -.37807741636673E+02 -.44939324365851E+02
 0.20099072626735E+03 0.46906815207570E+03 0.26707247217701E+03 0.19760773205204E+03 0.18622868268081E+03
 0.19760773205204E+03 0.37058320528982E+03 0.19762678133105E+03 0.18607081563829E+03 0.19762678133105E+03
 0.37045660686523E+03 0.19760773205204E+03 0.18622868268080E+03 0.19760773205204E+03 0.37058320528981E+03
 0.19762678133105E+03 0.18607081563829E+03 0.19762678133105E+03 0.37045660686523E+03 0.21295068842636E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36145936897799E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17786848256438E+00 0.00000000000000E+00 0.00000000000000E+00 0.17786848256438E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18574591979367E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18574591979367E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951414541851E+00 0.18795065853290E+00 0.33619556729925E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    995.27635405
 0.98963404437010E-01 0.31666883211294E+03 0.45298546928344E+03 0.44848868311818E+03 0.44664301331863E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14209731079976E+00 0.00000000000000E+00 -.21846287314781E+02
 0.35921102104436E-02 0.12297940867843E+01 0.22271031597920E+04 0.83516368492199E+03 0.65051540627574E+01
 0.24394327735340E+01 0.34762199340191E+03 0.29315344224788E+03 0.33966231672116E+03 0.37047119805038E+03
 0.29315058954829E+03 0.29315110615545E+03 0.33553761398364E+03 0.37045203015664E+03 0.29315049384109E+03
 0.29315110381173E+03 0.33966231672116E+03 0.37047119805038E+03 0.29315058954829E+03 0.29315110615545E+03
 0.33553761398364E+03 0.37045203015664E+03 0.29315049384109E+03 0.29315110381173E+03 0.41607453659595E+03
 0.33993482301739E+03 0.20373828856709E+04 0.18717439573310E+04 0.56938777055039E+03 0.90554430520520E+03
 0.33330959580206E+03 0.12160654339529E+04 0.11451588748315E+04 0.10955864641024E+04 0.17327278624171E+04
 0.11108878641528E+04 0.11449215293661E+04 0.10170521967877E+04 0.17326587344823E+04 0.12160654339529E+04
 0.11451588748315E+04 0.10955864641024E+04 0.17327278624171E+04 0.11108878641528E+04 0.11449215293661E+04
 0.10170521967877E+04 0.17326587344823E+04 0.16929373591918E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49235659563802E+03 0.12948654519361E+01
 0.12948654519361E+01 0.38011207384935E+01 0.00000000000000E+00 0.33649199059137E+03 0.33649199059137E+03
 0.33649199059137E+03 0.33649199059137E+03 0.00000000000000E+00 0.00000000000000E+00 0.14913992250151E+00
 0.00000000000000E+00 -.12725100298317E+02 0.10000000000000E-02 0.15225607014166E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52543061124307E+01 0.19703647921615E+01 0.33993756049974E+03 0.41607135703353E+03
 0.30780437693082E+03 0.30780437693082E+03 0.29315015368189E+03 0.29315014814107E+03 0.30780029506695E+03
 0.30780029506695E+03 0.29315015371539E+03 0.29315014817335E+03 0.30780437693082E+03 0.30780437693082E+03
 0.29315015368189E+03 0.29315014814107E+03 0.30780029506695E+03 0.30780029506695E+03 0.29315015371539E+03
 0.29315014817335E+03 0.30571626008732E+03 0.29315062829951E+03 -.41452050495914E+02 -.50086415864479E+02
 0.20253119769017E+03 0.47193771543145E+03 0.26839386175283E+03 0.20056116572548E+03 0.18758846547357E+03
 0.20056116572548E+03 0.37274072309492E+03 0.20058060886301E+03 0.18742940965432E+03 0.20058060886301E+03
 0.37261350544784E+03 0.20056116572548E+03 0.18758846547357E+03 0.20056116572548E+03 0.37274072309491E+03
 0.20058060886301E+03 0.18742940965432E+03 0.20058060886301E+03 0.37261350544784E+03 0.21305126270690E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36178829411239E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17778846491045E+00 0.00000000000000E+00 0.00000000000000E+00 0.17778846491045E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18558577346849E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18558577346849E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951393066439E+00 0.18778488878430E+00 0.33649199059137E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
   1000.00000000
 0.98928923980059E-01 0.31674968362889E+03 0.45312791647511E+03 0.44863066586519E+03 0.44678441687543E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14201836006759E+00 0.00000000000000E+00 -.21851377970953E+02
 0.35933620634064E-02 0.12305130870412E+01 0.22263272831506E+04 0.83487273118148E+03 0.65013530406540E+01
 0.24380073902453E+01 0.34778751013811E+03 0.29315357125178E+03 0.33980712028819E+03 0.37067619111399E+03
 0.29315061473533E+03 0.29315115325529E+03 0.33567683351121E+03 0.37065706275936E+03 0.29315051514681E+03
 0.29315115082507E+03 0.33980712028819E+03 0.37067619111399E+03 0.29315061473533E+03 0.29315115325529E+03
 0.33567683351121E+03 0.37065706275936E+03 0.29315051514681E+03 0.29315115082507E+03 0.41629967360565E+03
 0.34016822842604E+03 0.20387020755241E+04 0.18724955278494E+04 0.56882175499692E+03 0.90405305323673E+03
 0.33238718946483E+03 0.12170461051627E+04 0.11457837841609E+04 0.10961477860539E+04 0.17327144710019E+04
 0.11119485775978E+04 0.11455469484540E+04 0.10177491012199E+04 0.17326454416625E+04 0.12170461051627E+04
 0.11457837841609E+04 0.10961477860539E+04 0.17327144710019E+04 0.11119485775978E+04 0.11455469484540E+04
 0.10177491012199E+04 0.17326454416625E+04 0.16927688781009E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49249797252008E+03 0.12948654894453E+01
 0.12948654894453E+01 0.38200153223071E+01 0.00000000000000E+00 0.33658550827292E+03 0.33658550827292E+03
 0.33658550827292E+03 0.33658550827292E+03 0.00000000000000E+00 0.00000000000000E+00 0.14897693014675E+00
 0.00000000000000E+00 -.12723123046278E+02 0.10000000000000E-02 0.15252146579778E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52451633336691E+01 0.19669362501259E+01 0.34017098598322E+03 0.41629647866070E+03
 0.30786316976491E+03 0.30786316976491E+03 0.29315015921648E+03 0.29315015461936E+03 0.30785906556114E+03
 0.30785906556114E+03 0.29315015925096E+03 0.29315015465284E+03 0.30786316976491E+03 0.30786316976491E+03
 0.29315015921648E+03 0.29315015461936E+03 0.30785906556114E+03 0.30785906556114E+03 0.29315015925096E+03
 0.29315015465284E+03 0.30576757176782E+03 0.29315065250329E+03 -.42563147877631E+02 -.51665658501808E+02
 0.20301406135362E+03 0.47284139722036E+03 0.26881226555997E+03 0.20147354550390E+03 0.18801465433295E+03
 0.20147354550390E+03 0.37342052883005E+03 0.20149311353880E+03 0.18785525437872E+03 0.20149311353880E+03
 0.37329314699247E+03 0.20147354550390E+03 0.18801465433295E+03 0.20147354550390E+03 0.37342052883005E+03
 0.20149311353880E+03 0.18785525437872E+03 0.20149311353880E+03 0.37329314699247E+03 0.21309305012096E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36189393251254E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17777346560151E+00 0.00000000000000E+00 0.00000000000000E+00 0.17777346560151E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18557475617770E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18557475617770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17951379554852E+00 0.18773257656779E+00 0.33658550827292E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
