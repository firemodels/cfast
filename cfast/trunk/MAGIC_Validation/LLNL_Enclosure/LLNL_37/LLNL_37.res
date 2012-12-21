#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-37 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL37 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.089000                                                                                   
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
#CONDINIT 500.000000 10.000000 20.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-37           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-37           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-37           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-37           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-37           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-37           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-37           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-37           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-37           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-37           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-37           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-37           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-37           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-37           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-37           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-37           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-37           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-37           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL37     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL37     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL37     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL37     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL37     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL37     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL37     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL37     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL37     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL37     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL37     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL37     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL37     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL37     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL37     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL37     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL37     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL37     #HEAT_POWER#W#Total sprinkling power
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.37001938920237E-12
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
 0.00000000000000E+00 -.31164331504305E-07 0.99964886840307E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.53290747682319E-03 0.53290747682319E-03
 0.65453242277559E-03 0.65780508488947E-03 0.00000000000000E+00 0.46581430978273E-03 0.51384943997599E-03
 0.46581430978273E-03 0.51384943997599E-03 0.46176000224395E-03 0.51384238284147E-03 0.46176000224395E-03
 0.51384238284147E-03 0.46581430978273E-03 0.51384944003349E-03 0.46581430978273E-03 0.51384944003349E-03
 0.46176000230144E-03 0.51384238284147E-03 0.46176000230144E-03 0.51384238284147E-03 0.42502879084465E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24155772451965E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24155772451965E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88999860726537E-01 0.10687871139755E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     20.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.37981509456035E+01
 0.99996251516461E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000203920E+03 0.29315000000000E+03 0.29315000280443E+03 0.29315000280443E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000278994E+03 0.29315000278994E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000280443E+03 0.29315000280443E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000278994E+03 0.29315000278994E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000973749E+03
 0.29315000804578E+03 0.47321948549593E-03 0.47321948549593E-03 0.61273457014241E-03 0.61579824299313E-03
 .00000000000000E+00 0.44212973177877E-03 0.52639516302394E-03 0.44212973177877E-03 0.52639516302394E-03
 0.43983565370157E-03 0.52646321391056E-03 0.43983565370157E-03 0.52646321391056E-03 0.44212973183626E-03
 0.52639516296644E-03 0.44212973183626E-03 0.52639516296644E-03 0.43983565404655E-03 0.52646321425554E-03
 0.43983565404655E-03 0.52646321425554E-03 0.52609151521785E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.41978802280188E+01 0.99962440773974E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000802338E+03 0.29315000976468E+03
 0.29315000301148E+03 0.29315000301148E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000298506E+03
 0.29315000298506E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000301148E+03 0.29315000301148E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000298506E+03 0.29315000298506E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000290225E+03 0.29315000000000E+03 0.50814409982957E-03 0.50814409982957E-03
 0.66890143321154E-03 0.67224594037759E-03 .00000000000000E+00 0.47475190614064E-03 0.52123002125255E-03
 0.47475190614064E-03 0.52123002125255E-03 0.47058078327850E-03 0.52135863949985E-03 0.47058078327850E-03
 0.52135863949985E-03 0.47475190619813E-03 0.52123002131005E-03 0.47475190619813E-03 0.52123002131005E-03
 0.47058078327850E-03 0.52135863949985E-03 0.47058078327850E-03 0.52135863949985E-03 0.42521864675363E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10679967371476E+00 0.00000000000000E+00 0.00000000000000E+00 0.10679967371476E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955052166E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955052166E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88937553830209E-01 0.10679946147679E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     30.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.37981509450992E+01
 0.99996251516462E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000253563E+03 0.29315000000000E+03 0.29315000347813E+03 0.29315000347813E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000346008E+03 0.29315000346008E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000347813E+03 0.29315000347813E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000346008E+03 0.29315000346008E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001191226E+03
 0.29315000984967E+03 0.47660018336112E-03 0.47660018336112E-03 0.60496488324749E-03 0.60798970766373E-03
 .00000000000000E+00 0.44389030845958E-03 0.52981917170137E-03 0.44389030845958E-03 0.52981917170137E-03
 0.44157713033501E-03 0.52990539551001E-03 0.44157713033501E-03 0.52990539551001E-03 0.44389030845958E-03
 0.52981917170137E-03 0.44389030845958E-03 0.52981917170137E-03 0.44157713062249E-03 0.52990539591248E-03
 0.44157713062249E-03 0.52990539591248E-03 0.52608059620631E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.41978802274916E+01 0.99963256071301E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000983155E+03 0.29315001193417E+03
 0.29315000373480E+03 0.29315000373480E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000370200E+03
 0.29315000370200E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000373480E+03 0.29315000373480E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000370200E+03 0.29315000370200E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000360117E+03 0.29315000000000E+03 0.50274143549733E-03 0.50274143549733E-03
 0.67193516989806E-03 0.67529484574755E-03 .00000000000000E+00 0.47662958809787E-03 0.52278141424570E-03
 0.47662958809787E-03 0.52278141424570E-03 0.47243617995933E-03 0.52294284565881E-03 0.47243617995933E-03
 0.52294284565881E-03 0.47662958809787E-03 0.52278141424570E-03 0.47662958809787E-03 0.52278141424570E-03
 0.47243618036180E-03 0.52294284600378E-03 0.47243618036180E-03 0.52294284600378E-03 0.42521437497566E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10679967370780E+00 0.00000000000000E+00 0.00000000000000E+00 0.10679967370780E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955051776E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955051776E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88937553830217E-01 0.10679946147680E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.37981509445845E+01
 0.99996251516462E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296433E+03 0.29315000000000E+03 0.29315000405736E+03 0.29315000405736E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000403625E+03 0.29315000403625E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000405736E+03 0.29315000405736E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000403625E+03 0.29315000403625E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001373506E+03
 0.29315001136536E+03 0.47941737063620E-03 0.47941737063620E-03 0.59851931447123E-03 0.60151191104359E-03
 .00000000000000E+00 0.44534185611702E-03 0.53265100634975E-03 0.44534185611702E-03 0.53265100634975E-03
 0.44301368115756E-03 0.53275291683205E-03 0.44301368115756E-03 0.53275291683205E-03 0.44534185611702E-03
 0.53265100634975E-03 0.44534185611702E-03 0.53265100634975E-03 0.44301368156003E-03 0.53275291723452E-03
 0.44301368156003E-03 0.53275291723452E-03 0.52606937933139E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.41978802269601E+01 0.99964051709258E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001135152E+03 0.29315001375174E+03
 0.29315000435670E+03 0.29315000435670E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000431840E+03
 0.29315000431840E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000435670E+03 0.29315000435670E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000431840E+03 0.29315000431840E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000420283E+03 0.29315000000000E+03 0.49826634700996E-03 0.49826634700996E-03
 0.67444235841946E-03 0.67781457021156E-03 .00000000000000E+00 0.47818071327396E-03 0.52406313662760E-03
 0.47818071327396E-03 0.52406313662760E-03 0.47397000966210E-03 0.52425279979857E-03 0.47397000966210E-03
 0.52425279979857E-03 0.47818071327396E-03 0.52406313662760E-03 0.47818071327396E-03 0.52406313662760E-03
 0.47397000966210E-03 0.52425279979857E-03 0.47397000966210E-03 0.52425279979857E-03 0.42520834968193E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10679967370043E+00 0.00000000000000E+00 0.00000000000000E+00 0.10679967370043E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955051580E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955051580E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88937553830225E-01 0.10679946147681E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00025000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.37981509445825E+01
 0.99996251516462E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296434E+03 0.29315000000000E+03 0.29315000405737E+03 0.29315000405737E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000403626E+03 0.29315000403626E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000405737E+03 0.29315000405737E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000403626E+03 0.29315000403626E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001373510E+03
 0.29315001136540E+03 0.47942282529562E-03 0.47942282529562E-03 0.59852687392727E-03 0.60151950829691E-03
 .00000000000000E+00 0.44534779265165E-03 0.53265534437651E-03 0.44534779265165E-03 0.53265534437651E-03
 0.44301966489652E-03 0.53275725554876E-03 0.44301966489652E-03 0.53275725554876E-03 0.44534779270914E-03
 0.53265534443401E-03 0.44534779270914E-03 0.53265534443401E-03 0.44301966518400E-03 0.53275725577874E-03
 0.44301966518400E-03 0.53275725577874E-03 0.52607608503681E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.41978802269130E+01 0.99964051729147E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001135156E+03 0.29315001375178E+03
 0.29315000435671E+03 0.29315000435671E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000431841E+03
 0.29315000431841E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000435671E+03 0.29315000435671E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000431841E+03 0.29315000431841E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000420284E+03 0.29315000000000E+03 0.49827340368451E-03 0.49827340368451E-03
 0.67445684075480E-03 0.67782912495858E-03 .00000000000000E+00 0.47819066217717E-03 0.52407113882628E-03
 0.47819066217717E-03 0.52407113882628E-03 0.47398012375173E-03 0.52426080239972E-03 0.47398012375173E-03
 0.52426080239972E-03 0.47819066217717E-03 0.52407113876879E-03 0.47819066217717E-03 0.52407113876879E-03
 0.47398012375173E-03 0.52426080245722E-03 0.47398012375173E-03 0.52426080245722E-03 0.42521654091644E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10679967370043E+00 0.00000000000000E+00 0.00000000000000E+00 0.10679967370043E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955050999E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10679955050999E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88937553830225E-01 0.10679946147681E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     50.00021699
 0.22395779857937E+01 0.29319967851770E+03 0.32856432102621E+03 0.30216369608037E+03 0.30142322223502E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22599104547092E+00 0.00000000000000E+00 0.44056294982131E+02
 0.10002652923696E-02 0.72606519362748E-01 0.79978782239341E+04 0.29992043339753E+04 0.11018294321521E+03
 0.41318603705705E+02 0.29447598755243E+03 0.29315000000001E+03 0.29429664801236E+03 0.29471620471020E+03
 0.29315000000001E+03 0.29315000000001E+03 0.29402314145269E+03 0.29470423455601E+03 0.29315000000001E+03
 0.29315000000001E+03 0.29429664801236E+03 0.29471620471020E+03 0.29315000000001E+03 0.29315000000001E+03
 0.29402314145269E+03 0.29470423455601E+03 0.29315000000001E+03 0.29315000000001E+03 0.29743203526246E+03
 0.29315389112102E+03 0.61036774270803E+03 0.60783557973446E+03 0.28523116963433E+03 0.58705314738752E+03
 0.30039582190501E+03 0.38956256750639E+03 0.19002519605294E+03 0.38787587327909E+03 0.50229935001923E+03
 0.29298695347940E+03 0.18537287313613E+03 0.29182804868528E+03 0.49777539268941E+03 0.38956256750639E+03
 0.19002519605294E+03 0.38787587327909E+03 0.50229935001923E+03 0.29298695347940E+03 0.18537287313613E+03
 0.29182804868528E+03 0.49777539268941E+03 0.51941710778468E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.35386345842052E+03 0.12943800867511E+01
 0.12943800867511E+01 0.20050060356423E-01 0.13695815390218E+01 0.29318644593335E+03 0.29732397836693E+03
 0.29354618634150E+03 0.29354160995057E+03 0.23000000000000E+00 0.00000000000000E+00 0.22944879134251E+00
 0.00000000000000E+00 0.44082431309597E+02 0.99999269878955E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315388186489E+03 0.29744268004566E+03
 0.29315203133914E+03 0.29327779729278E+03 0.29315000000001E+03 0.29315000000001E+03 0.29315205344403E+03
 0.29327779920567E+03 0.29315000000001E+03 0.29315000000001E+03 0.29315203133914E+03 0.29327779729278E+03
 0.29315000000001E+03 0.29315000000001E+03 0.29315205344403E+03 0.29327779920567E+03 0.29315000000001E+03
 0.29315000000001E+03 0.29319600776108E+03 0.29315000000001E+03 0.53460689274078E+00 0.54392717242217E+00
 0.64266944725093E+00 0.22124243005766E+02 0.21478360211279E+02 0.67691445512727E+00 -.56963803302459E-01
 0.69678381869528E+00 0.35007751420782E+02 0.68411430441443E+00 -.56422177384234E-01 0.70396728444084E+00
 0.35008274826872E+02 0.67691445512733E+00 -.56963803300791E-01 0.69678381869534E+00 0.35007751420787E+02
 0.68411430441438E+00 -.56422177384177E-01 0.70396728444079E+00 0.35008274826872E+02 0.65451452345522E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30155739179655E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34643500364772E+00 0.00000000000000E+00 0.00000000000000E+00 0.34643500364772E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10074426148635E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10074426148635E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89651488894757E-01 0.10769470908395E+00 0.29318644593335E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     60.00391346
 0.16922770981267E+01 0.29324197329408E+03 0.35341503513539E+03 0.31947187030932E+03 0.31675070234040E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22282871193077E+00 0.00000000000000E+00 0.30381510756769E+02
 0.99998610514871E-03 0.12652643769930E+00 0.80000000000000E+04 0.30000000000000E+04 0.63227892490049E+02
 0.23710459683768E+02 0.29560017841917E+03 0.29315000000002E+03 0.29548967668037E+03 0.29677630713559E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29491092447223E+03 0.29674916600768E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29548967668037E+03 0.29677630713559E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29491092447223E+03 0.29674916600768E+03 0.29315000000002E+03 0.29315000000002E+03 0.30317842184529E+03
 0.29315979357923E+03 0.64937403441508E+03 0.64363741565331E+03 0.30145644331313E+03 0.82110389196425E+03
 0.51814016643455E+03 0.44251960386362E+03 0.25185671433793E+03 0.43811978208324E+03 0.73425611077727E+03
 0.33171211374998E+03 0.24641089045049E+03 0.32873110011533E+03 0.72908843885197E+03 0.44251960386362E+03
 0.25185671433793E+03 0.43811978208324E+03 0.73425611077727E+03 0.33171211374998E+03 0.24641089045049E+03
 0.32873110011533E+03 0.72908843885197E+03 0.67651272736296E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.37960415769275E+03 0.12944807636860E+01
 0.12944807636860E+01 0.60064846200458E-01 0.12302056888430E+01 0.29317630941660E+03 0.30009855692156E+03
 0.29442136474810E+03 0.29439771296438E+03 0.23000000000000E+00 0.00000000000000E+00 0.22902062195636E+00
 0.00000000000000E+00 0.31969067319669E+02 0.99990773051731E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315975814724E+03 0.30321604685080E+03
 0.29315475967685E+03 0.29340240401670E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315479190988E+03
 0.29340242157207E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315475967685E+03 0.29340240401670E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315479190988E+03 0.29340242157207E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29327596393544E+03 0.29315000000002E+03 0.84854622455848E+00 0.85254612097237E+00
 0.50493550736157E+00 0.40881271851623E+02 0.40373811666724E+02 0.97470216056003E+00 -.35145143084303E+00
 0.98579600702964E+00 0.48058156604559E+02 0.97917998278735E+00 -.34605520423725E+00 0.99025262630153E+00
 0.48063409388874E+02 0.97470216056003E+00 -.35145143083998E+00 0.98579600702964E+00 0.48058156604567E+02
 0.97917998278729E+00 -.34605520423719E+00 0.99025262630147E+00 0.48063409388874E+02 0.12852378114297E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30670880616872E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.28507607936325E+00 0.00000000000000E+00 0.00000000000000E+00 0.28507607936325E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.92322374011311E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.92322374011311E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89472901727832E-01 0.10747104706860E+00 0.29317630941660E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     70.00272129
 0.13143361618755E+01 0.29332550125671E+03 0.36792639474496E+03 0.33524284407168E+03 0.33104048346276E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22059085686630E+00 0.00000000000000E+00 0.19693271026111E+02
 0.99959592547264E-03 0.16072128522895E+00 0.80000000000000E+04 0.30000000000000E+04 0.49775609923749E+02
 0.18665853721406E+02 0.29646175732987E+03 0.29315000000002E+03 0.29641198710527E+03 0.29871452625783E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29561287201611E+03 0.29867827838530E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29641198710527E+03 0.29871452625783E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29561287201611E+03 0.29867827838530E+03 0.29315000000002E+03 0.29315000000002E+03 0.30820339317231E+03
 0.29316704327086E+03 0.70090866935631E+03 0.69252438573395E+03 0.33074454537064E+03 0.95958226347446E+03
 0.62718399537697E+03 0.48676970405417E+03 0.31712770866568E+03 0.47998350446386E+03 0.89223239677471E+03
 0.36858809295021E+03 0.31169747481034E+03 0.36399888433039E+03 0.88716958566949E+03 0.48676970405417E+03
 0.31712770866568E+03 0.47998350446386E+03 0.89223239677471E+03 0.36858809295021E+03 0.31169747481034E+03
 0.36399888433039E+03 0.88716958566949E+03 0.81060015025842E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39171627635384E+03 0.12945594662285E+01
 0.12945594662285E+01 0.10006007754162E+00 0.11036558157808E+01 0.29316858989094E+03 0.30214490767748E+03
 0.29554039745788E+03 0.29548814251135E+03 0.23000000000000E+00 0.00000000000000E+00 0.22864374625707E+00
 0.00000000000000E+00 0.22638852258228E+02 0.99984198016949E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29316699706262E+03 0.30824248796197E+03
 0.29315786767257E+03 0.29352088656505E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315789739779E+03
 0.29352093178454E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315786767257E+03 0.29352088656505E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315789739779E+03 0.29352093178454E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29336631019750E+03 0.29315000000002E+03 0.12410391841439E+01 0.12412535836144E+01
 0.42412971135615E+00 0.55867289986580E+02 0.55441039626667E+02 0.13570883837474E+01 -.53226844896587E+00
 0.13617364676215E+01 0.57042120915495E+02 0.13590913216919E+01 -.52206287786188E+00 0.13637230503381E+01
 0.57051979643169E+02 0.13570883837474E+01 -.53226844896281E+00 0.13617364676215E+01 0.57042120915503E+02
 0.13590913216919E+01 -.52206287786194E+00 0.13637230503381E+01 0.57051979643169E+02 0.18528778703569E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31056526625030E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23594944776421E+00 0.00000000000000E+00 0.00000000000000E+00 0.23594944776421E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.90141857526632E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.90141857526632E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89335102976464E-01 0.10729847303781E+00 0.29316858989094E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     80.03110422
 0.10137551098086E+01 0.29345944991303E+03 0.37820139609751E+03 0.34956553577763E+03 0.34457742826140E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21868095940525E+00 0.00000000000000E+00 0.11611674688569E+02
 0.99905998796138E-03 0.18865105163336E+00 0.80000000000000E+04 0.30000000000000E+04 0.42406336623809E+02
 0.15902376233928E+02 0.29735779005654E+03 0.29315000000002E+03 0.29726933259383E+03 0.30056893828574E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29628658818881E+03 0.30052733927376E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29726933259383E+03 0.30056893828574E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29628658818881E+03 0.30052733927376E+03 0.29315000000003E+03 0.29315000000003E+03 0.31273687936414E+03
 0.29317581738781E+03 0.79721442769373E+03 0.78601799589486E+03 0.35105360936621E+03 0.10488166077749E+04
 0.69600773036181E+03 0.52899820917156E+03 0.37192188370922E+03 0.51987824905589E+03 0.10087006195771E+04
 0.40690791154479E+03 0.36701715568342E+03 0.40072510555571E+03 0.10042153517853E+04 0.52899820917156E+03
 0.37192188370922E+03 0.51987824905589E+03 0.10087006195771E+04 0.40690791154479E+03 0.36701715568342E+03
 0.40072510555571E+03 0.10042153517853E+04 0.93609536558162E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40229631670524E+03 0.18235221346401E+01
 0.18235221346401E+01 0.14017360925720E+00 0.99073644822043E+00 0.29316299585662E+03 0.30382274104114E+03
 0.29678207565244E+03 0.29669719750755E+03 0.23000000000000E+00 0.00000000000000E+00 0.22828096805190E+00
 0.00000000000000E+00 0.15755599697495E+02 0.99979312714888E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29317576229611E+03 0.31276977968736E+03
 0.29316152422646E+03 0.29363630873723E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316154391766E+03
 0.29363638827858E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316152422646E+03 0.29363630873723E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29316154391766E+03 0.29363638827858E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29346237990271E+03 0.29315000000002E+03 0.16546794385851E+01 0.16488967257319E+01
 0.33259580283834E+00 0.68684041203440E+02 0.68349782421587E+02 0.17532526973122E+01 -.67746211899262E+00
 0.17536433914230E+01 0.64428311671541E+02 0.17523650429696E+01 -.66267502589222E+00 0.17527492265337E+01
 0.64442508156538E+02 0.17532526973122E+01 -.67746211898808E+00 0.17536433914230E+01 0.64428311671552E+02
 0.17523650429697E+01 -.66267502589228E+00 0.17527492265338E+01 0.64442508156538E+02 0.23790090465066E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31386714720379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.19515649390762E+00 0.00000000000000E+00 0.00000000000000E+00 0.19515649390762E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88991672678687E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88991672678687E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89233307086722E-01 0.10717097147247E+00 0.29316299585662E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     90.00702311
 0.77817866422100E+00 0.29365918519909E+03 0.38651923808488E+03 0.36243200078016E+03 0.35721857582336E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21684912203817E+00 0.00000000000000E+00 0.53426929390025E+01
 0.99831870437302E-03 0.21477047622851E+00 0.80000000000000E+04 0.30000000000000E+04 0.37249067658108E+02
 0.13968400371791E+02 0.29830741542783E+03 0.29315000000002E+03 0.29810290845479E+03 0.30237389769373E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29696127964501E+03 0.30232934483203E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29810290845479E+03 0.30237389769373E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29696127964501E+03 0.30232934483203E+03 0.29315000000003E+03 0.29315000000003E+03 0.31696416261405E+03
 0.29318599734567E+03 0.89520271421251E+03 0.88105947585198E+03 0.37755031161828E+03 0.11258361806938E+04
 0.74639811751744E+03 0.57175336893908E+03 0.42778273099551E+03 0.56034862684817E+03 0.11120961748046E+04
 0.44714000078024E+03 0.42339400682645E+03 0.43938690221469E+03 0.11081552191568E+04 0.57175336893908E+03
 0.42778273099551E+03 0.56034862684817E+03 0.11120961748046E+04 0.44714000078024E+03 0.42339400682645E+03
 0.43938690221469E+03 0.11081552191568E+04 0.10548101538715E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41218622592623E+03 0.16392765969919E+01
 0.16392765969919E+01 0.18007728481655E+00 0.89531682789426E+00 0.29315889729016E+03 0.30529388242761E+03
 0.29805077816107E+03 0.29793282820393E+03 0.23000000000000E+00 0.00000000000000E+00 0.22791438442028E+00
 0.00000000000000E+00 0.10528291382400E+02 0.99975551529278E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29318593396290E+03 0.31699190063964E+03
 0.29316573018698E+03 0.29375040607056E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316573502370E+03
 0.29375052271218E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316573018698E+03 0.29375040607056E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29316573502370E+03 0.29375052271218E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29356216442083E+03 0.29315000000002E+03 0.20953509556042E+01 0.20805773287964E+01
 0.26525770707132E+00 0.80259576511208E+02 0.79992992515601E+02 0.21821230188649E+01 -.78575706401812E+00
 0.21793788892464E+01 0.71277645596577E+02 0.21786336185021E+01 -.76719480325513E+00 0.21758870412525E+01
 0.71295355263384E+02 0.21821230188648E+01 -.78575706401220E+00 0.21793788892463E+01 0.71277645596591E+02
 0.21786336185020E+01 -.76719480325507E+00 0.21758870412525E+01 0.71295355263384E+02 0.28772569002473E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31680285356954E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.15912067187987E+00 0.00000000000000E+00 0.00000000000000E+00 0.15912067187987E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88291980946976E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88291980946976E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89155922976900E-01 0.10544658791580E+00 0.29768338057436E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    100.03399991
 0.59077951420998E+00 0.29395078042600E+03 0.39367304080009E+03 0.37403508462018E+03 0.36901994257941E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21499522656974E+00 0.00000000000000E+00 0.51502250727545E+00
 0.99728087001687E-03 0.24078309477882E+00 0.80000000000000E+04 0.30000000000000E+04 0.33224923898203E+02
 0.12459346461826E+02 0.29930443026578E+03 0.29315000000003E+03 0.29894104302093E+03 0.30416415863349E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29765809868473E+03 0.30411796709968E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29894104302093E+03 0.30416415863349E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29765809868473E+03 0.30411796709968E+03 0.29315000000003E+03 0.29315000000003E+03 0.32105494289458E+03
 0.29319768572526E+03 0.99336960877617E+03 0.97631368041764E+03 0.40869159235794E+03 0.11946931055611E+04
 0.78395805524141E+03 0.61561477354132E+03 0.48542139088234E+03 0.60199718454988E+03 0.12058369347587E+04
 0.48949870541356E+03 0.48149278294797E+03 0.48023394306366E+03 0.12023699260631E+04 0.61561477354132E+03
 0.48542139088234E+03 0.60199718454988E+03 0.12058369347586E+04 0.48949870541356E+03 0.48149278294797E+03
 0.48023394306366E+03 0.12023699260631E+04 0.11667735172701E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42222063685715E+03 0.14894040331569E+01
 0.14894040331569E+01 0.22018519200493E+00 0.82146491783830E+00 0.29315604579612E+03 0.30665461611686E+03
 0.29926221481055E+03 0.29911202520806E+03 0.23000000000000E+00 0.00000000000000E+00 0.22752733123248E+00
 0.00000000000000E+00 0.66106216631295E+01 0.99972657491988E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29319761354955E+03 0.32108079127462E+03
 0.29317051334428E+03 0.29386706303480E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317049991488E+03
 0.29386721823680E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317051334428E+03 0.29386706303480E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29317049991488E+03 0.29386721823680E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29366607638800E+03 0.29315000000003E+03 0.25553270032226E+01 0.25300344992984E+01
 0.20552155166842E+00 0.91128085797118E+02 0.90921536637691E+02 0.26345652054250E+01 -.88367610082813E+00
 0.26273606028751E+01 0.78134269234339E+02 0.26286018840085E+01 -.86178757219500E+00 0.26214057371229E+01
 0.78155028562784E+02 0.26345652054250E+01 -.88367610082461E+00 0.26273606028750E+01 0.78134269234347E+02
 0.26286018840084E+01 -.86178757219500E+00 0.26214057371228E+01 0.78155028562784E+02 0.33513709879954E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31938650493995E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12674182542454E+00 0.00000000000000E+00 0.00000000000000E+00 0.12674182542454E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87828696367332E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87828696367332E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89097882156284E-01 0.10461226619084E+00 0.29985058087493E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    110.04958009
 0.43385411314578E+00 0.29437014322913E+03 0.39947509372590E+03 0.38427502203090E+03 0.37986064190763E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21316945975959E+00 0.00000000000000E+00 -.30544799757024E+01
 0.99582505096508E-03 0.26628615523067E+00 0.80000000000000E+04 0.30000000000000E+04 0.30042868706674E+02
 0.11266075765003E+02 0.30033896259075E+03 0.29315000000004E+03 0.29978507117350E+03 0.30592674427331E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29837625062567E+03 0.30587978155139E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29978507117350E+03 0.30592674427331E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29837625062567E+03 0.30587978155139E+03 0.29315000000003E+03 0.29315000000003E+03 0.32494589945540E+03
 0.29321094384899E+03 0.10900250410625E+04 0.10703342194502E+04 0.44000434893011E+03 0.12488318345460E+04
 0.80662746387125E+03 0.65915906154198E+03 0.54080002352178E+03 0.64350591215468E+03 0.12883827086549E+04
 0.53233090466939E+03 0.53727635085643E+03 0.52172317133737E+03 0.12853264700586E+04 0.65915906154198E+03
 0.54080002352178E+03 0.64350591215468E+03 0.12883827086549E+04 0.53233090466939E+03 0.53727635085643E+03
 0.52172317133737E+03 0.12853264700586E+04 0.12688186922559E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42972804154262E+03 0.13622242724985E+01
 0.13622242724985E+01 0.26024751272671E+00 0.75648649502572E+00 0.29315433885162E+03 0.30792005325083E+03
 0.30047334422922E+03 0.30029203247479E+03 0.23000000000000E+00 0.00000000000000E+00 0.22711701077824E+00
 0.00000000000000E+00 0.38278979717753E+01 0.99970493215996E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29321086228669E+03 0.32497201075435E+03
 0.29317581916662E+03 0.29398516767167E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317578458878E+03
 0.29398536242040E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317581916662E+03 0.29398516767167E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29317578458878E+03 0.29398536242040E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29377295208585E+03 0.29315000000004E+03 0.30266972535962E+01 0.29869872624260E+01
 0.14888834977785E+00 0.10134659365670E+03 0.10119696086517E+03 0.31016221895036E+01 -.97335361046751E+00
 0.30895414904801E+01 0.84611899102125E+02 0.30932153775954E+01 -.94840527048648E+00 0.30811592903539E+01
 0.84635432731083E+02 0.31016221895036E+01 -.97335361046758E+00 0.30895414904801E+01 0.84611899102125E+02
 0.30932153775953E+01 -.94840527048648E+00 0.30811592903539E+01 0.84635432731083E+02 0.38090409160035E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32169992128680E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.98345354125794E-01 0.00000000000000E+00 0.00000000000000E+00 0.98345354125794E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87648595250239E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87648595250239E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89056632311383E-01 0.10432207854286E+00 0.30053719564895E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    120.07127057
 0.30362391885935E+00 0.29498032528654E+03 0.40437593044490E+03 0.39330422299684E+03 0.38974726759657E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21132181703025E+00 0.00000000000000E+00 -.56661011330953E+01
 0.11771299706426E-02 0.29202868148881E+00 0.67961909045890E+04 0.25485715892209E+04 0.27394569462201E+02
 0.10272963548325E+02 0.30140524167620E+03 0.29315000000004E+03 0.30063746443929E+03 0.30765492383623E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29911622459136E+03 0.30760776393677E+03 0.29315000000003E+03
 0.29315000000003E+03 0.30063746443929E+03 0.30765492383623E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29911622459136E+03 0.30760776393677E+03 0.29315000000003E+03 0.29315000000003E+03 0.32865413130060E+03
 0.29322610044677E+03 0.11843414267943E+04 0.11626566709249E+04 0.47129401114720E+03 0.12946854783954E+04
 0.82103499719246E+03 0.70232204627038E+03 0.59429485849826E+03 0.68500524427543E+03 0.13627733545879E+04
 0.57546780292130E+03 0.59112607506995E+03 0.56388778859373E+03 0.13600723333322E+04 0.70232204627038E+03
 0.59429485849826E+03 0.68500524427543E+03 0.13627733545879E+04 0.57546780292130E+03 0.59112607506995E+03
 0.56388778859373E+03 0.13600723333322E+04 0.13627442365918E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43669379593975E+03 0.12947462457306E+01
 0.12947462457306E+01 0.30033427466668E+00 0.69777517731362E+00 0.29315361129822E+03 0.30910076788565E+03
 0.30168241454202E+03 0.30147191694576E+03 0.23000000000000E+00 0.00000000000000E+00 0.22668073639297E+00
 0.00000000000000E+00 0.18958742791738E+01 0.99968834525647E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29322600947307E+03 0.32868147449407E+03
 0.29318159141660E+03 0.29410396240223E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318153302344E+03
 0.29410419743004E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318159141660E+03 0.29410396240223E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29318153302344E+03 0.29410419743004E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29388180773077E+03 0.29315000000004E+03 0.35016428016960E+01 0.34464060248140E+01
 0.93643613331260E-01 0.11096695682723E+03 0.11087284499583E+03 0.35774346878163E+01 -.10567008631376E+01
 0.35602743046208E+01 0.90700806121048E+02 0.35665764130878E+01 -.10289069488342E+01 0.35494613584280E+01
 0.90726892781897E+02 0.35774346878163E+01 -.10567008631376E+01 0.35602743046208E+01 0.90700806121048E+02
 0.35665764130878E+01 -.10289069488343E+01 0.35494613584279E+01 0.90726892781897E+02 0.42509619501837E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32380343167533E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.72981336208168E-01 0.00000000000000E+00 0.00000000000000E+00 0.72981336208168E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87602058752679E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87602058752679E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.89001238071784E-01 0.10404516630191E+00 0.30114388484637E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    130.40737045
 0.19607555105861E+00 0.29592996157633E+03 0.40885820391962E+03 0.40147738147044E+03 0.39890897043055E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20934147927340E+00 0.00000000000000E+00 -.75719379254543E+01
 0.18227833745036E-02 0.31953293199466E+00 0.43888923455749E+04 0.16458346295906E+04 0.25036543025662E+02
 0.93887036346231E+01 0.30253777473404E+03 0.29315000000004E+03 0.30153161499705E+03 0.30940933636979E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29990727163195E+03 0.30936239801015E+03 0.29315000000003E+03
 0.29315000000003E+03 0.30153161499705E+03 0.30940933636979E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29990727163195E+03 0.30936239801015E+03 0.29315000000003E+03 0.29315000000003E+03 0.33234025397005E+03
 0.29324459379295E+03 0.12784766197296E+04 0.12560177473713E+04 0.50402703406650E+03 0.13378594144997E+04
 0.83131224526284E+03 0.74661029416399E+03 0.64859882512113E+03 0.72836527527806E+03 0.14330597774003E+04
 0.62031929561097E+03 0.64574800519256E+03 0.60853009325330E+03 0.14306722188861E+04 0.74661029416399E+03
 0.64859882512114E+03 0.72836527527806E+03 0.14330597774003E+04 0.62031929561097E+03 0.64574800519256E+03
 0.60853009325330E+03 0.14306722188861E+04 0.14523164100794E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44499425290320E+03 0.12947602854433E+01
 0.12947602854433E+01 0.34167867418159E+00 0.64334683249396E+00 0.29315375966907E+03 0.31024278612628E+03
 0.30291333876518E+03 0.30267528113141E+03 0.23000000000000E+00 0.00000000000000E+00 0.22620056701323E+00
 0.00000000000000E+00 0.58837429698282E+00 0.99967493501160E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29324449244770E+03 0.33236958332500E+03
 0.29318807257490E+03 0.29422726368277E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318798753656E+03
 0.29422753999308E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318807257490E+03 0.29422726368277E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29318798753656E+03 0.29422753999308E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29399620280225E+03 0.29315000000004E+03 0.39951043561961E+01 0.39204700233699E+01
 0.51561150426061E-01 0.12035841424948E+03 0.12030659529330E+03 0.40890648544905E+01 -.11277633332540E+01
 0.40664793196564E+01 0.96665023524992E+02 0.40758462675065E+01 -.10975040832113E+01 0.40533315556972E+01
 0.96693273169679E+02 0.40890648544905E+01 -.11277633332540E+01 0.40664793196564E+01 0.96665023524992E+02
 0.40758462675065E+01 -.10975040832113E+01 0.40533315556972E+01 0.96693273169679E+02 0.46913102587394E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32580472646602E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.49284116671746E-01 0.00000000000000E+00 0.00000000000000E+00 0.49284116671746E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87711106283365E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87711106283365E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88980550645525E-01 0.10359455377251E+00 0.30237958607289E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    140.46620123
 0.11606386280594E+00 0.29735419801080E+03 0.41288670551059E+03 0.40841698914389E+03 0.40677225805530E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20729833445470E+00 0.00000000000000E+00 -.88020777052777E+01
 0.30793561198611E-02 0.34778943489185E+00 0.25979457031300E+04 0.97422963867374E+03 0.23002423873191E+02
 0.86259089524467E+01 0.30366852914224E+03 0.29315000000004E+03 0.30242113939423E+03 0.31110233344335E+03
 0.29315000000003E+03 0.29315000000003E+03 0.30070774071428E+03 0.31105588537649E+03 0.29315000000003E+03
 0.29315000000003E+03 0.30242113939423E+03 0.31110233344335E+03 0.29315000000003E+03 0.29315000000003E+03
 0.30070774071428E+03 0.31105588537649E+03 0.29315000000003E+03 0.29315000000003E+03 0.33582981648146E+03
 0.29326643567397E+03 0.13666259891599E+04 0.13455539138277E+04 0.53688144345749E+03 0.13779043428813E+04
 0.83833849220654E+03 0.78966562900898E+03 0.70154732715213E+03 0.77198111615547E+03 0.15007727423655E+04
 0.66434256975867E+03 0.69896499990603E+03 0.65383291527457E+03 0.14986474527469E+04 0.78966562900898E+03
 0.70154732715213E+03 0.77198111615547E+03 0.15007727423655E+04 0.66434256975867E+03 0.69896499990603E+03
 0.65383291527457E+03 0.14986474527469E+04 0.15355889978769E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45244436567410E+03 0.12947693477006E+01
 0.12947693477006E+01 0.38191399728281E+00 0.59895232023025E+00 0.29315477685153E+03 0.31129091712417E+03
 0.30404912825996E+03 0.30378649345745E+03 0.23000000000000E+00 0.00000000000000E+00 0.22570039234896E+00
 0.00000000000000E+00 -.14988819117281E+00 0.99966418015325E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29326632419627E+03 0.33586148011107E+03
 0.29319486932609E+03 0.29434791201718E+03 0.29315000000003E+03 0.29315000000003E+03 0.29319475645641E+03
 0.29434822762277E+03 0.29315000000003E+03 0.29315000000003E+03 0.29319486932609E+03 0.29434791201718E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29319475645641E+03 0.29434822762277E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29410896237308E+03 0.29315000000004E+03 0.44654320985244E+01 0.43671384815233E+01
 0.19581473423214E-01 0.12901230340993E+03 0.12899262402914E+03 0.46001764986328E+01 -.11881465135772E+01
 0.45722700652258E+01 0.10227308881060E+03 0.45846552806992E+01 -.11558356688108E+01 0.45568482054201E+01
 0.10230309820040E+03 0.46001764986328E+01 -.11881465135772E+01 0.45722700652258E+01 0.10227308881060E+03
 0.45846552806991E+01 -.11558356688108E+01 0.45568482054201E+01 0.10230309820040E+03 0.50990790516528E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32757260357444E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.27799648911610E-01 0.00000000000000E+00 0.00000000000000E+00 0.27799648911610E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87788339934939E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87788339934939E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88968426792473E-01 0.10215513449622E+00 0.30659625731407E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    150.00001717
 0.62040064846766E-01 0.29935273210056E+03 0.41651148998178E+03 0.41408864433634E+03 0.41316747169735E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20522172859268E+00 0.00000000000000E+00 -.94750740675921E+01
 0.57608092379961E-02 0.37638316330425E+00 0.13886937875386E+04 0.52076017032696E+03 0.21254935873774E+02
 0.79706009526651E+01 0.30476948636415E+03 0.29315000000005E+03 0.30330257125321E+03 0.31271406639818E+03
 0.29315000000005E+03 0.29315000000005E+03 0.30151860867223E+03 0.31266826839887E+03 0.29315000000005E+03
 0.29315000000005E+03 0.30330257125321E+03 0.31271406639818E+03 0.29315000000005E+03 0.29315000000005E+03
 0.30151860867223E+03 0.31266826839887E+03 0.29315000000005E+03 0.29315000000005E+03 0.33908373569877E+03
 0.29329183493786E+03 0.14460426143393E+04 0.14289371763818E+04 0.56911230420963E+03 0.14152026423415E+04
 0.84324477661079E+03 0.83010323669502E+03 0.75228987959045E+03 0.83010323669502E+03 0.15655619012599E+04
 0.70589780572272E+03 0.74992976647363E+03 0.70589780572272E+03 0.15636518680450E+04 0.83010323669502E+03
 0.75228987959045E+03 0.83010323669502E+03 0.15655619012599E+04 0.70589780572273E+03 0.74992976647362E+03
 0.70589780572273E+03 0.15636518680450E+04 0.16117397314770E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45898116834509E+03 0.12947743056302E+01
 0.12947743056302E+01 0.42004926105946E+00 0.56949462285766E+00 0.29315656681872E+03 0.31223477072016E+03
 0.30499148102973E+03 0.30470611567341E+03 0.22999999989572E+00 0.00000000000000E+00 0.22519221409019E+00
 0.00000000000000E+00 -.43743039644989E+00 0.99965523851212E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29329171373401E+03 0.33911775331193E+03
 0.29320179118751E+03 0.29446384710687E+03 0.29315000000005E+03 0.29315000000005E+03 0.29320164970665E+03
 0.29446419895201E+03 0.29315000000005E+03 0.29315000000005E+03 0.29320179118751E+03 0.29446384710687E+03
 0.29315000000005E+03 0.29315000000005E+03 0.29320164970665E+03 0.29446419895201E+03 0.29315000000005E+03
 0.29315000000005E+03 0.29421738896902E+03 0.29315000000005E+03 0.48948233014553E+01 0.47678693409395E+01
 0.55358744163140E-03 0.13679595691653E+03 0.13679540056115E+03 0.51009491280895E+01 -.12389845373833E+01
 0.50681120927182E+01 0.10756632702966E+03 0.50831659532330E+01 -.12050587359408E+01 0.50504588028605E+01
 0.10759767551519E+03 0.51009491280895E+01 -.12389845373833E+01 0.50681120927183E+01 0.10756632702966E+03
 0.50831659532330E+01 -.12050587359408E+01 0.50504588028605E+01 0.10759767551519E+03 0.54572309277328E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32906667433068E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.13163064652533E-01 0.65515485221704E-02 0.00000000000000E+00 0.13163064652533E-01 0.65515485221704E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87857859384758E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87857859384758E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88963046951945E-01 0.10030400844488E+00 0.31223477072016E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    160.01879219
 0.33048861049760E-01 0.30163017066123E+03 0.42036032756776E+03 0.41905236208175E+03 0.41854538010972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20281159571258E+00 0.00000000000000E+00 -.10056458859492E+02
 0.10814298509688E-01 0.40929780376916E+00 0.73976134400521E+03 0.27741050400195E+03 0.19545670478388E+02
 0.73296264293955E+01 0.30594291868919E+03 0.29315000000015E+03 0.30428845504023E+03 0.31443342014788E+03
 0.29315000000016E+03 0.29315000000016E+03 0.30239260879962E+03 0.31438835807476E+03 0.29315000000016E+03
 0.29315000000016E+03 0.30428845504023E+03 0.31443342014788E+03 0.29315000000016E+03 0.29315000000016E+03
 0.30239260879962E+03 0.31438835807476E+03 0.29315000000016E+03 0.29315000000016E+03 0.34249380941477E+03
 0.29332469516214E+03 0.15183909864440E+04 0.15058263502983E+04 0.60576693635259E+03 0.14573186419761E+04
 0.84852287094179E+03 0.86854187198982E+03 0.80859294033024E+03 0.86854187198982E+03 0.16390401054078E+04
 0.74689127582053E+03 0.80617954978182E+03 0.74689127582053E+03 0.16370700038263E+04 0.86854187198982E+03
 0.80859294033023E+03 0.86854187198982E+03 0.16390401054078E+04 0.74689127582053E+03 0.80617954978180E+03
 0.74689127582053E+03 0.16370700038263E+04 0.16886967948771E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.19825065038431E+06 0.50050000000000E+08 0.46493202449900E+03 0.12874896265420E+01
 0.12874896265420E+01 0.46012436114731E+00 0.54413348054829E+00 0.29315912724828E+03 0.31318139832055E+03
 0.30591820628920E+03 0.30560973506296E+03 0.22999999989939E+00 0.00000000000000E+00 0.22461725907230E+00
 0.00000000000000E+00 -.70482243702113E+00 0.99964386863971E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29332456403772E+03 0.34253037009406E+03
 0.29320954665389E+03 0.29458698412237E+03 0.29315000000016E+03 0.29315000000016E+03 0.29320937241874E+03
 0.29458737269672E+03 0.29315000000016E+03 0.29315000000016E+03 0.29320954665389E+03 0.29458698412237E+03
 0.29315000000016E+03 0.29315000000016E+03 0.29320937241874E+03 0.29458737269672E+03 0.29315000000016E+03
 0.29315000000016E+03 0.29433229751285E+03 0.29315000000015E+03 0.53199831982175E+01 0.51537427696127E+01
 -.84173578372419E-02 0.14463985289445E+03 0.14464831233907E+03 0.56393175192591E+01 -.12875650640352E+01
 0.56012845447285E+01 0.11296538612174E+03 0.56188927081627E+01 -.12521372772064E+01 0.55810258687419E+01
 0.11299795275120E+03 0.56393175192591E+01 -.12875650640352E+01 0.56012845447285E+01 0.11296538612174E+03
 0.56188927081627E+01 -.12521372772064E+01 0.55810258687419E+01 0.11299795275120E+03 0.58157606814640E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33055602831716E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.33048631511227E-02 0.19874334921619E-01 0.00000000000000E+00 0.33048631511227E-02 0.19874334921619E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87874049100862E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87874049100862E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88957953976508E-01 0.99994838940584E-01 0.31318139832055E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    170.16766762
 0.21763353024701E-01 0.30282346968772E+03 0.42422159183612E+03 0.42334091510650E+03 0.42299144042548E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20020972071328E+00 0.00000000000000E+00 -.10445996317166E+02
 0.16422089335867E-01 0.44397136868539E+00 0.48714873219739E+03 0.18268077457402E+03 0.18019179983809E+02
 0.67571924939283E+01 0.30667702947564E+03 0.29315000000017E+03 0.30486540319063E+03 0.31622982058870E+03
 0.29315000000019E+03 0.29315000000019E+03 0.30304309222557E+03 0.31615640711781E+03 0.29315000000019E+03
 0.29315000000019E+03 0.30486540319063E+03 0.31622982058870E+03 0.29315000000019E+03 0.29315000000019E+03
 0.30304309222557E+03 0.31615640711781E+03 0.29315000000019E+03 0.29315000000019E+03 0.34589736059789E+03
 0.29338222925736E+03 0.14032899547773E+04 0.13925033527089E+04 0.64137854739621E+03 0.14966808404594E+04
 0.85209540032621E+03 0.79712291900626E+03 0.87613250372237E+03 0.79712291900626E+03 0.17232785038320E+04
 0.72142259671907E+03 0.86559779264115E+03 0.72142259671907E+03 0.17134666845585E+04 0.79712291900626E+03
 0.87613250372237E+03 0.79712291900626E+03 0.17232785038320E+04 0.72142259671907E+03 0.86559779264114E+03
 0.72142259671907E+03 0.17134666845585E+04 0.17208705054642E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.12843976685832E+06 0.50050000000000E+08 0.46847684454375E+03 0.11915427371076E+01
 0.11915427371076E+01 0.50071986284097E+00 0.51996129043043E+00 0.29316287229162E+03 0.31407007433099E+03
 0.30682278382987E+03 0.30649324322373E+03 0.23000000000000E+00 0.00000000000000E+00 0.22398833025677E+00
 0.00000000000000E+00 -.84658802803296E+00 0.99962969946994E-03 0.18048266768186E-01 0.80000000000000E+04
 0.30000000000000E+04 0.44325585956552E+03 0.16622094733707E+03 0.29338205037446E+03 0.34593619463408E+03
 0.29322181838642E+03 0.29471961882736E+03 0.29315000000019E+03 0.29315000000019E+03 0.29322151956799E+03
 0.29472004153995E+03 0.29315000000019E+03 0.29315000000019E+03 0.29322181838642E+03 0.29471961882736E+03
 0.29315000000019E+03 0.29315000000019E+03 0.29322151956799E+03 0.29472004153995E+03 0.29315000000019E+03
 0.29315000000019E+03 0.29445372012451E+03 0.29315000000017E+03 0.79553064781795E+01 0.77134237354210E+01
 0.27846885052352E+01 0.15471766260055E+03 0.15191905065279E+03 0.75642112010660E+01 0.14562322001763E+01
 0.75174333622166E+01 0.12084169397333E+03 0.75108289445505E+01 0.14924042908221E+01 0.74643508293741E+01
 0.12087473680933E+03 0.75642112010660E+01 0.14562322001763E+01 0.75174333622166E+01 0.12084169397333E+03
 0.75108289445504E+01 0.14924042908221E+01 0.74643508293740E+01 0.12087473680933E+03 0.63451505747433E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33171197819686E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39468567800740E-03 0.28356171980233E-01 0.00000000000000E+00 0.39468567800740E-03 0.28356171980233E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88277218170021E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88277218170021E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88954829476460E-01 0.99708257084174E-01 0.31407007433099E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    180.32505920
 0.20281141340791E-01 0.30240042863538E+03 0.42757741703599E+03 0.42673117297120E+03 0.42638421399708E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19754558059330E+00 0.00000000000000E+00 -.11086715438767E+02
 0.17622251733714E-01 0.47908625415970E+00 0.45397149699631E+03 0.17023931137361E+03 0.16698454465223E+02
 0.62619204244587E+01 0.30742244324661E+03 0.29315000000019E+03 0.30546514213916E+03 0.31802256369985E+03
 0.29315000000020E+03 0.29315000000020E+03 0.30370384129333E+03 0.31792828141223E+03 0.29315000000020E+03
 0.29315000000020E+03 0.30546514213916E+03 0.31802256369985E+03 0.29315000000020E+03 0.29315000000020E+03
 0.30370384129333E+03 0.31792828141223E+03 0.29315000000020E+03 0.29315000000020E+03 0.34924009196890E+03
 0.29346759596205E+03 0.14379546841147E+04 0.14225958826616E+04 0.67542416483431E+03 0.15280773821785E+04
 0.84927609652002E+03 0.81593840603689E+03 0.92942283287949E+03 0.81593840603689E+03 0.17885762389527E+04
 0.74598362515549E+03 0.91886286000276E+03 0.74598362515549E+03 0.17789445961373E+04 0.81593840603689E+03
 0.92942283287948E+03 0.81593840603689E+03 0.17885762389527E+04 0.74598362515549E+03 0.91886286000276E+03
 0.74598362515549E+03 0.17789445961373E+04 0.17749526178179E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.11927097206446E+06 0.50050000000000E+08 0.47176771408356E+03 0.11985070530593E+01
 0.11985070530593E+01 0.54134942916318E+00 0.49616229074901E+00 0.29316831948309E+03 0.31484406864433E+03
 0.30767427573930E+03 0.30732798368393E+03 0.23000000000000E+00 0.00000000000000E+00 0.22331588235314E+00
 0.00000000000000E+00 -.13043655045082E+01 0.99960660814739E-03 0.37439233559985E-01 0.80000000000000E+04
 0.30000000000000E+04 0.21367958794302E+03 0.80129845478632E+02 0.29346728252932E+03 0.34928074656010E+03
 0.29323920106364E+03 0.29485974341334E+03 0.29315000000020E+03 0.29315000000020E+03 0.29323867900604E+03
 0.29486019551619E+03 0.29315000000020E+03 0.29315000000020E+03 0.29323920106364E+03 0.29485974341334E+03
 0.29315000000020E+03 0.29315000000020E+03 0.29323867900604E+03 0.29486019551619E+03 0.29315000000020E+03
 0.29315000000020E+03 0.29458004380965E+03 0.29315000000019E+03 0.10883590420030E+02 0.10517710905370E+02
 0.60664318267395E+01 0.16421534788933E+03 0.15811858390345E+03 0.97589010821594E+01 0.46569190277244E+01
 0.96992653262423E+01 0.12831173263771E+03 0.96684987595403E+01 0.46930984081569E+01 0.96094181639530E+01
 0.12834455185631E+03 0.97589010821594E+01 0.46569190277244E+01 0.96992653262423E+01 0.12831173263771E+03
 0.96684987595404E+01 0.46930984081569E+01 0.96094181639530E+01 0.12834455185631E+03 0.68658316515139E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33268895580173E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.47861525271365E-01 0.00000000000000E+00 0.00000000000000E+00 0.47861525271365E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88754538939014E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88754538939014E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88947202967154E-01 0.99454163190421E-01 0.31484406864433E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    190.00429292
 0.24535562818730E-01 0.30182311033427E+03 0.42976347838146E+03 0.42871711540237E+03 0.42827871800391E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19502996284901E+00 0.00000000000000E+00 -.11922385232103E+02
 0.14566579074067E-01 0.51301851816381E+00 0.54920238714405E+03 0.20595089517902E+03 0.15593979002227E+02
 0.58477421258351E+01 0.30836644691318E+03 0.29315000000020E+03 0.30625797052686E+03 0.31964736705876E+03
 0.29315000000020E+03 0.29315000000020E+03 0.30445784781604E+03 0.31955189069411E+03 0.29315000000020E+03
 0.29315000000020E+03 0.30625797052686E+03 0.31964736705876E+03 0.29315000000020E+03 0.29315000000020E+03
 0.30445784781604E+03 0.31955189069411E+03 0.29315000000020E+03 0.29315000000020E+03 0.35222124316539E+03
 0.29357126393948E+03 0.15557239192735E+04 0.15338580560835E+04 0.69924254281717E+03 0.15390874254054E+04
 0.83634866987413E+03 0.88500708582101E+03 0.96485837333616E+03 0.88500708582101E+03 0.18271851719634E+04
 0.79773260962345E+03 0.95791698190823E+03 0.79773260962345E+03 0.18211824669365E+04 0.88500708582101E+03
 0.96485837333616E+03 0.88500708582101E+03 0.18271851719634E+04 0.79773260962345E+03 0.95791698190823E+03
 0.79773260962345E+03 0.18211824669365E+04 0.18275944717134E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.14558834489405E+06 0.50050000000000E+08 0.47399294180169E+03 0.11755082308500E+01
 0.11755082308500E+01 0.58006636404473E+00 0.47405164699832E+00 0.29317600381716E+03 0.31547590641082E+03
 0.30842836937587E+03 0.30807033079288E+03 0.23000000000000E+00 0.00000000000000E+00 0.22263496541139E+00
 0.00000000000000E+00 -.20437406847953E+01 0.99957311114701E-03 0.56157040835062E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14245764878347E+03 0.53421618293800E+02 0.29357084943349E+03 0.35226271721339E+03
 0.29325922978300E+03 0.29499570243147E+03 0.29315000000020E+03 0.29315000000020E+03 0.29325843147976E+03
 0.29499617886092E+03 0.29315000000020E+03 0.29315000000020E+03 0.29325922978300E+03 0.29499570243147E+03
 0.29315000000020E+03 0.29315000000020E+03 0.29325843147976E+03 0.29499617886092E+03 0.29315000000020E+03
 0.29315000000020E+03 0.29470167949975E+03 0.29315000000019E+03 0.13773535365534E+02 0.13243403569943E+02
 0.94188784288065E+01 0.17248327093990E+03 0.16301729811895E+03 0.11974880149902E+02 0.79166112057968E+01
 0.11901129505559E+02 0.13483875961825E+03 0.11847663060654E+02 0.79523471153765E+01 0.11774806423244E+02
 0.13487094581212E+03 0.11974880149902E+02 0.79166112057968E+01 0.11901129505559E+02 0.13483875961825E+03
 0.11847663060654E+02 0.79523471153764E+01 0.11774806423244E+02 0.13487094581212E+03 0.73368123577828E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33343908256882E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.67267113644929E-01 0.00000000000000E+00 0.00000000000000E+00 0.67267113644929E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.89362504596236E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.89362504596236E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88935599573272E-01 0.99241303057160E-01 0.31547590641082E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    200.00201081
 0.31003215097830E-01 0.30169292733326E+03 0.43091073422440E+03 0.42957534507056E+03 0.42901179115721E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19251287321365E+00 0.00000000000000E+00 -.12739113960381E+02
 0.11527810307418E-01 0.54831387171763E+00 0.69397394532528E+03 0.26024022949698E+03 0.14590183492786E+02
 0.54713188097947E+01 0.30956168265824E+03 0.29315000000026E+03 0.30728237635817E+03 0.32120502920970E+03
 0.29315000000031E+03 0.29315000000031E+03 0.30535274627924E+03 0.32112184227867E+03 0.29315000000031E+03
 0.29315000000031E+03 0.30728237635817E+03 0.32120502920970E+03 0.29315000000031E+03 0.29315000000031E+03
 0.30535274627924E+03 0.32112184227867E+03 0.29315000000031E+03 0.29315000000031E+03 0.35498491038349E+03
 0.29370093526369E+03 0.17015777096182E+04 0.16736290864801E+04 0.71267417583828E+03 0.15297542560484E+04
 0.81351670933093E+03 0.97158225736547E+03 0.98719746880106E+03 0.97158225736547E+03 0.18441357043270E+04
 0.85844959090431E+03 0.98457077740899E+03 0.85844959090431E+03 0.18423244890312E+04 0.97158225736547E+03
 0.98719746880106E+03 0.97158225736547E+03 0.18441357043270E+04 0.85844959090431E+03 0.98457077740899E+03
 0.85844959090431E+03 0.18423244890312E+04 0.18691722348603E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.18559651456973E+06 0.50050000000000E+08 0.47525809144625E+03 0.12391125116630E+01
 0.12391125116630E+01 0.62005723561250E+00 0.45188510651125E+00 0.29318796783573E+03 0.31601654020986E+03
 0.30913927897067E+03 0.30877368734874E+03 0.23000000000000E+00 0.00000000000000E+00 0.22189145228615E+00
 0.00000000000000E+00 -.28306264797977E+01 0.99952455670739E-03 0.75799729881830E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10554127320073E+03 0.39577977450275E+02 0.29370043396479E+03 0.35502602449185E+03
 0.29328346044167E+03 0.29513797162452E+03 0.29315000000031E+03 0.29315000000031E+03 0.29328231742069E+03
 0.29513846869592E+03 0.29315000000031E+03 0.29315000000031E+03 0.29328346044167E+03 0.29513797162452E+03
 0.29315000000031E+03 0.29315000000031E+03 0.29328231742069E+03 0.29513846869592E+03 0.29315000000031E+03
 0.29315000000031E+03 0.29482840654384E+03 0.29315000000024E+03 0.16808388026996E+02 0.16057935318723E+02
 0.13033423464376E+02 0.18008154749705E+03 0.16698295691536E+03 0.14344648455402E+02 0.11430193716726E+02
 0.14256023840420E+02 0.14087622901959E+03 0.14178313694311E+02 0.11465349606075E+02 0.14091029195814E+02
 0.14090767681352E+03 0.14344648455402E+02 0.11430193716726E+02 0.14256023840420E+02 0.14087622901959E+03
 0.14178313694311E+02 0.11465349606075E+02 0.14091029195814E+02 0.14090767681352E+03 0.77889459783182E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33401666702420E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.82911760801938E-01 0.00000000000000E+00 0.00000000000000E+00 0.82911760801938E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.90146469814103E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.90146469814103E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923436150225E-01 0.99057204082551E-01 0.31601654020986E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    210.00212066
 0.36597785492708E-01 0.30195962483885E+03 0.43125146205357E+03 0.42967419707878E+03 0.42901055543151E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19010210215762E+00 0.00000000000000E+00 -.13009536866140E+02
 0.97655967638357E-02 0.58333054433733E+00 0.81920236862798E+03 0.30720088823549E+03 0.13714351284464E+02
 0.51428817316742E+01 0.31074947744951E+03 0.29315000000034E+03 0.30830935965787E+03 0.32262235668111E+03
 0.29315000000037E+03 0.29315000000037E+03 0.30623538167959E+03 0.32255209703313E+03 0.29315000000037E+03
 0.29315000000037E+03 0.30830935965787E+03 0.32262235668112E+03 0.29315000000037E+03 0.29315000000037E+03
 0.30623538167959E+03 0.32255209703313E+03 0.29315000000037E+03 0.29315000000037E+03 0.35738535415108E+03
 0.29385284757096E+03 0.17640198325461E+04 0.17316638367982E+04 0.71629105671268E+03 0.15058294802011E+04
 0.78595696820485E+03 0.10092332625448E+04 0.10017915242743E+04 0.10092332625448E+04 0.18476715555301E+04
 0.88717336369739E+03 0.10005792329564E+04 0.88717336369739E+03 0.18471454583509E+04 0.10092332625448E+04
 0.10017915242743E+04 0.10092332625448E+04 0.18476715555301E+04 0.88717336369739E+03 0.10005792329564E+04
 0.88717336369739E+03 0.18471454583509E+04 0.18789822396755E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47546605152941E+03 0.12948003445927E+01
 0.12948003445927E+01 0.66005767503232E+00 0.43047489742779E+00 0.29320571326097E+03 0.31644284776314E+03
 0.30977417903558E+03 0.30940572605590E+03 0.23000000000000E+00 0.00000000000000E+00 0.22110960637607E+00
 0.00000000000000E+00 -.31126109647638E+01 0.99946128084936E-03 0.95787297596615E-01 0.80000000000000E+04
 0.30000000000000E+04 0.83518380836779E+02 0.31319392813792E+02 0.29385228892453E+03 0.35742515602139E+03
 0.29331048513351E+03 0.29527878456411E+03 0.29315000000037E+03 0.29315000000037E+03 0.29330895206021E+03
 0.29527929797244E+03 0.29315000000037E+03 0.29315000000037E+03 0.29331048513351E+03 0.29527878456411E+03
 0.29315000000037E+03 0.29315000000037E+03 0.29330895206021E+03 0.29527929797244E+03 0.29315000000037E+03
 0.29315000000037E+03 0.29495317650275E+03 0.29315000000030E+03 0.19837721986932E+02 0.18814725503210E+02
 0.16765121524283E+02 0.18671825241298E+03 0.16986930528108E+03 0.16780134968291E+02 0.15046842119440E+02
 0.16679486039143E+02 0.14618574530426E+03 0.16574808134780E+02 0.15080757219789E+02 0.16476018328083E+02
 0.14621583007615E+03 0.16780134968291E+02 0.15046842119439E+02 0.16679486039143E+02 0.14618574530426E+03
 0.16574808134780E+02 0.15080757219789E+02 0.16476018328083E+02 0.14621583007615E+03 0.82029366650618E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33442590021855E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87705051325682E-01 0.00000000000000E+00 0.00000000000000E+00 0.87705051325682E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.91030338492029E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.91030338492029E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918933665277E-01 0.98918471321754E-01 0.31644284776314E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    220.00078256
 0.39475370174054E-01 0.30247090467077E+03 0.43138701593563E+03 0.42969067886444E+03 0.42898117570454E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18778094167595E+00 0.00000000000000E+00 -.13019248932534E+02
 0.90537272412148E-02 0.61730599202711E+00 0.88361398425855E+03 0.33135524409696E+03 0.12959537252716E+02
 0.48598264697684E+01 0.31175766225011E+03 0.29315000000045E+03 0.30918141909422E+03 0.32393589270016E+03
 0.29315000000039E+03 0.29315000000039E+03 0.30700989318353E+03 0.32387178341139E+03 0.29315000000039E+03
 0.29315000000039E+03 0.30918141909422E+03 0.32393589270017E+03 0.29315000000039E+03 0.29315000000039E+03
 0.30700989318353E+03 0.32387178341139E+03 0.29315000000039E+03 0.29315000000039E+03 0.35950676485954E+03
 0.29402731566852E+03 0.17805328834945E+04 0.17457665573734E+04 0.71666356165047E+03 0.14802880585527E+04
 0.76004117909393E+03 0.10194996568923E+04 0.10134994000964E+04 0.10194996568923E+04 0.18476710244429E+04
 0.89797574359223E+03 0.10123660164062E+04 0.89797574359223E+03 0.18471614809334E+04 0.10194996568923E+04
 0.10134994000964E+04 0.10194996568923E+04 0.18476710244429E+04 0.89797574359223E+03 0.10123660164062E+04
 0.89797574359223E+03 0.18471614809334E+04 0.18758127908219E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47544055691581E+03 0.12948004161448E+01
 0.12948004161448E+01 0.70005232260923E+00 0.40986224016885E+00 0.29323054984506E+03 0.31676410166475E+03
 0.31033375881946E+03 0.30996674563841E+03 0.23000000000000E+00 0.00000000000000E+00 0.22029332062233E+00
 0.00000000000000E+00 -.31345662965081E+01 0.99937640999714E-03 0.11610290933950E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68904388748838E+02 0.25839145780814E+02 0.29402671300654E+03 0.35954522480088E+03
 0.29334023218796E+03 0.29541739000768E+03 0.29315000000039E+03 0.29315000000039E+03 0.29333827189411E+03
 0.29541791290791E+03 0.29315000000039E+03 0.29315000000039E+03 0.29334023218796E+03 0.29541739000768E+03
 0.29315000000039E+03 0.29315000000039E+03 0.29333827189411E+03 0.29541791290791E+03 0.29315000000039E+03
 0.29315000000039E+03 0.29507549860714E+03 0.29315000000036E+03 0.22806777538943E+02 0.21456826123963E+02
 0.20565709874579E+02 0.19246648910454E+03 0.17179795068059E+03 0.19266487542688E+02 0.18713320840694E+02
 0.19158753977613E+02 0.15080824950739E+03 0.19023356197177E+02 0.18744995169059E+02 0.18918050401684E+02
 0.15083602575143E+03 0.19266487542688E+02 0.18713320840694E+02 0.19158753977613E+02 0.15080824950739E+03
 0.19023356197177E+02 0.18744995169059E+02 0.18918050401684E+02 0.15083602575143E+03 0.85786969046120E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33473142918708E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87908322854936E-01 0.00000000000000E+00 0.00000000000000E+00 0.87908322854936E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.91860696388826E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.91860696388826E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918438174780E-01 0.98817578711582E-01 0.31676410166475E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    230.00454616
 0.40809567746821E-01 0.30300523733857E+03 0.43148430060566E+03 0.42973657559351E+03 0.42900978518602E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18552193604413E+00 0.00000000000000E+00 -.13012524738508E+02
 0.87577315249437E-02 0.65040672169175E+00 0.91347856202425E+03 0.34255446075909E+03 0.12299995884408E+02
 0.46124984566531E+01 0.31269373648570E+03 0.29315000000054E+03 0.30999192306402E+03 0.32516708776188E+03
 0.29315000000041E+03 0.29315000000041E+03 0.30773686945988E+03 0.32510732198445E+03 0.29315000000041E+03
 0.29315000000041E+03 0.30999192306402E+03 0.32516708776189E+03 0.29315000000041E+03 0.29315000000041E+03
 0.30773686945988E+03 0.32510732198445E+03 0.29315000000041E+03 0.29315000000041E+03 0.36143192709175E+03
 0.29422439439290E+03 0.17952531231171E+04 0.17585210253475E+04 0.71609518092773E+03 0.14561978518123E+04
 0.73652219497989E+03 0.10285992783758E+04 0.10230662580818E+04 0.10285992783758E+04 0.18460942519454E+04
 0.90759240880775E+03 0.10220152502766E+04 0.90759240880775E+03 0.18456228174695E+04 0.10285992783758E+04
 0.10230662580818E+04 0.10285992783758E+04 0.18460942519454E+04 0.90759240880775E+03 0.10220152502766E+04
 0.90759240880775E+03 0.18456228174695E+04 0.18720100941897E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47546931606650E+03 0.12948003666054E+01
 0.12948003666054E+01 0.74006737703123E+00 0.39003654249006E+00 0.29326403514526E+03 0.31699593093663E+03
 0.31082505988250E+03 0.31046315545189E+03 0.23000000000000E+00 0.00000000000000E+00 0.21944554341180E+00
 0.00000000000000E+00 -.31358634737855E+01 0.99926228699477E-03 0.13673595357904E+00 0.80000000000000E+04
 0.30000000000000E+04 0.58506923677360E+02 0.21940096379010E+02 0.29422375264458E+03 0.36146960766980E+03
 0.29337253253311E+03 0.29555322810335E+03 0.29315000000041E+03 0.29315000000041E+03 0.29337011633842E+03
 0.29555375197544E+03 0.29315000000041E+03 0.29315000000041E+03 0.29337253253311E+03 0.29555322810335E+03
 0.29315000000041E+03 0.29315000000041E+03 0.29337011633842E+03 0.29555375197544E+03 0.29315000000041E+03
 0.29315000000041E+03 0.29519498213590E+03 0.29315000000037E+03 0.25669124337520E+02 0.23937604154419E+02
 0.24387138254091E+02 0.19743326326443E+03 0.17292418931907E+03 0.21783911831343E+02 0.22381914850457E+02
 0.21676537393770E+02 0.15482188123107E+03 0.21504792014131E+02 0.22410297741260E+02 0.21400427272298E+02
 0.15484636447789E+03 0.21783911831342E+02 0.22381914850457E+02 0.21676537393770E+02 0.15482188123107E+03
 0.21504792014131E+02 0.22410297741260E+02 0.21400427272298E+02 0.15484636447789E+03 0.89182821235827E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33496196169353E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87771671039714E-01 0.00000000000000E+00 0.00000000000000E+00 0.87771671039714E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.92616320879354E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.92616320879354E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918369478693E-01 0.98745232700241E-01 0.31699593093663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    240.00204007
 0.41383261707257E-01 0.30352897382872E+03 0.43160048996734E+03 0.42983381761080E+03 0.42910292190759E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18331243090873E+00 0.00000000000000E+00 -.13006639241173E+02
 0.86363233844456E-02 0.68273597782647E+00 0.92632010681864E+03 0.34737004005699E+03 0.11717560315876E+02
 0.43940851184534E+01 0.31357941366171E+03 0.29315000000070E+03 0.31076010171775E+03 0.32633092119784E+03
 0.29315000000043E+03 0.29315000000044E+03 0.30842964440920E+03 0.32627457708356E+03 0.29315000000043E+03
 0.29315000000044E+03 0.31076010171775E+03 0.32633092119784E+03 0.29315000000043E+03 0.29315000000044E+03
 0.30842964440920E+03 0.32627457708356E+03 0.29315000000043E+03 0.29315000000044E+03 0.36320681675755E+03
 0.29444369369970E+03 0.18087558891667E+04 0.17702373593989E+04 0.71524058865230E+03 0.14341819755511E+04
 0.71536518395549E+03 0.10369240342365E+04 0.10314566780965E+04 0.10369240342365E+04 0.18442233872456E+04
 0.91641939124131E+03 0.10304855093512E+04 0.91641939124131E+03 0.18437968572766E+04 0.10369240342365E+04
 0.10314566780965E+04 0.10369240342365E+04 0.18442233872456E+04 0.91641939124131E+03 0.10304855093512E+04
 0.91641939124131E+03 0.18437968572766E+04 0.18684130315443E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47556084018564E+03 0.12948003232449E+01
 0.12948003232449E+01 0.78005735267340E+00 0.37101183703379E+00 0.29330803468309E+03 0.31715351373324E+03
 0.31125554374167E+03 0.31090176116981E+03 0.23000000000000E+00 0.00000000000000E+00 0.21857005432377E+00
 0.00000000000000E+00 -.31333835050436E+01 0.99911241076091E-03 0.15764575838611E+00 0.80000000000000E+04
 0.30000000000000E+04 0.50746687268339E+02 0.19030007725627E+02 0.29444301504731E+03 0.36324408580449E+03
 0.29340716187138E+03 0.29568575615060E+03 0.29315000000043E+03 0.29315000000043E+03 0.29340426944748E+03
 0.29568627131187E+03 0.29315000000043E+03 0.29315000000043E+03 0.29340716187138E+03 0.29568575615060E+03
 0.29315000000043E+03 0.29315000000043E+03 0.29340426944748E+03 0.29568627131187E+03 0.29315000000043E+03
 0.29315000000043E+03 0.29531122609368E+03 0.29315000000038E+03 0.28383409870692E+02 0.26218394033334E+02
 0.28183712117907E+02 0.20171726680383E+03 0.17339263612533E+03 0.24312089203882E+02 0.26008642045125E+02
 0.24215242040703E+02 0.15830027095866E+03 0.23999269499352E+02 0.26032715052838E+02 0.23905969555758E+02
 0.15832051671980E+03 0.24312089203882E+02 0.26008642045125E+02 0.24215242040703E+02 0.15830027095866E+03
 0.23999269499352E+02 0.26032715052838E+02 0.23905969555758E+02 0.15832051671980E+03 0.92237862386752E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33513420591795E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87588011376626E-01 0.00000000000000E+00 0.00000000000000E+00 0.87588011376626E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93289536141196E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93289536141196E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918452885181E-01 0.98696264546541E-01 0.31715351373324E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    250.00266083
 0.41607843388056E-01 0.30403497817993E+03 0.43175480564616E+03 0.42998342345324E+03 0.42925386755744E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18114203229280E+00 0.00000000000000E+00 -.13006829899618E+02
 0.85897077776852E-02 0.71441029315693E+00 0.93134716652210E+03 0.34925518744579E+03 0.11198046943933E+02
 0.41992676039747E+01 0.31442665897981E+03 0.29315000000099E+03 0.31149642723568E+03 0.32744059310336E+03
 0.29315000000046E+03 0.29315000000047E+03 0.30909610729605E+03 0.32738710637799E+03 0.29315000000046E+03
 0.29315000000047E+03 0.31149642723567E+03 0.32744059310336E+03 0.29315000000046E+03 0.29315000000047E+03
 0.30909610729605E+03 0.32738710637799E+03 0.29315000000046E+03 0.29315000000047E+03 0.36486531955429E+03
 0.29468519255622E+03 0.18213499964452E+04 0.17811325899748E+04 0.71434780403377E+03 0.14142086977463E+04
 0.69628915469236E+03 0.10446857828961E+04 0.10391372231347E+04 0.10446857828961E+04 0.18425420773021E+04
 0.92466779607471E+03 0.10382415334176E+04 0.92466779607471E+03 0.18421618569612E+04 0.10446857828961E+04
 0.10391372231347E+04 0.10446857828961E+04 0.18425420773021E+04 0.92466779607471E+03 0.10382415334176E+04
 0.92466779607471E+03 0.18421618569612E+04 0.18652782570476E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47570957475557E+03 0.12948003246496E+01
 0.12948003246496E+01 0.82005983571735E+00 0.35275672338439E+00 0.29336466304813E+03 0.31725093536564E+03
 0.31163357326124E+03 0.31129033161029E+03 0.23000000000000E+00 0.00000000000000E+00 0.21766837720091E+00
 0.00000000000000E+00 -.31327831583538E+01 0.99891955738598E-03 0.17883828167682E+00 0.80000000000000E+04
 0.30000000000000E+04 0.44733151789373E+02 0.16774931921015E+02 0.29468447822178E+03 0.36490225944460E+03
 0.29344397514127E+03 0.29581489724479E+03 0.29315000000045E+03 0.29315000000045E+03 0.29344059239430E+03
 0.29581539328948E+03 0.29315000000045E+03 0.29315000000045E+03 0.29344397514127E+03 0.29581489724479E+03
 0.29315000000045E+03 0.29315000000045E+03 0.29344059239430E+03 0.29581539328948E+03 0.29315000000045E+03
 0.29315000000045E+03 0.29542422353303E+03 0.29315000000039E+03 0.30922761616763E+02 0.28275976110851E+02
 0.31925916444565E+02 0.20541882638855E+03 0.17333328036176E+03 0.26839520682008E+02 0.29566023186495E+02
 0.26765543205562E+02 0.16131982340763E+03 0.26495519889624E+02 0.29584842129470E+02 0.26425484636614E+02
 0.16133496583149E+03 0.26839520682008E+02 0.29566023186495E+02 0.26765543205562E+02 0.16131982340763E+03
 0.26495519889624E+02 0.29584842129473E+02 0.26425484636614E+02 0.16133496583149E+03 0.94981507349654E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33526063003087E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87454990871140E-01 0.00000000000000E+00 0.00000000000000E+00 0.87454990871140E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93882791692516E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93882791692516E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918583477293E-01 0.98666102323507E-01 0.31725093536564E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    260.00778874
 0.41685825435393E-01 0.30452212550758E+03 0.43194973038297E+03 0.43017908875214E+03 0.42945268096784E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17900616239548E+00 0.00000000000000E+00 -.13013298610379E+02
 0.85736384468331E-02 0.74548459798467E+00 0.93309276447913E+03 0.34990978667967E+03 0.10731274692498E+02
 0.40242280096868E+01 0.31524230555575E+03 0.29315000000148E+03 0.31220682683853E+03 0.32850505144783E+03
 0.29315000000049E+03 0.29315000000051E+03 0.30974083239955E+03 0.32845403460189E+03 0.29315000000048E+03
 0.29315000000051E+03 0.31220682683853E+03 0.32850505144783E+03 0.29315000000049E+03 0.29315000000051E+03
 0.30974083239955E+03 0.32845403460189E+03 0.29315000000048E+03 0.29315000000051E+03 0.36642946255367E+03
 0.29494869401504E+03 0.18331861400812E+04 0.17913194226085E+04 0.71348497202611E+03 0.13960491921976E+04
 0.67899679531134E+03 0.10519891397537E+04 0.10463156849728E+04 0.10519891397537E+04 0.18411796501951E+04
 0.93244220584443E+03 0.10454905306225E+04 0.93244220584443E+03 0.18408447814145E+04 0.10519891397537E+04
 0.10463156849728E+04 0.10519891397537E+04 0.18411796501951E+04 0.93244220584443E+03 0.10454905306225E+04
 0.93244220584443E+03 0.18408447814145E+04 0.18626187213255E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47590612812080E+03 0.12948003723068E+01
 0.12948003723068E+01 0.86008034733182E+00 0.33525393563726E+00 0.29343616996536E+03 0.31730020322724E+03
 0.31196652917976E+03 0.31163572304918E+03 0.23000000000000E+00 0.00000000000000E+00 0.21674233671650E+00
 0.00000000000000E+00 -.31348763450343E+01 0.99867611188894E-03 0.20030527215485E+00 0.80000000000000E+04
 0.30000000000000E+04 0.39939038618092E+02 0.14977139481784E+02 0.29494794518893E+03 0.36646596863347E+03
 0.29348281364044E+03 0.29594059216553E+03 0.29315000000046E+03 0.29315000000047E+03 0.29347893159210E+03
 0.29594105832154E+03 0.29315000000046E+03 0.29315000000047E+03 0.29348281364044E+03 0.29594059216553E+03
 0.29315000000046E+03 0.29315000000047E+03 0.29347893159210E+03 0.29594105832154E+03 0.29315000000046E+03
 0.29315000000047E+03 0.29553397119703E+03 0.29315000000043E+03 0.33265210088361E+02 0.30094000655431E+02
 0.35588643380527E+02 0.20862320102278E+03 0.17285661442535E+03 0.29356136167719E+02 0.33031330446646E+02
 0.29317635420335E+02 0.16394578220455E+03 0.28983612176842E+02 0.33044052568904E+02 0.28949071856666E+02
 0.16395505851121E+03 0.29356136167719E+02 0.33031330446646E+02 0.29317635420335E+02 0.16394578220455E+03
 0.28983612176842E+02 0.33044052568904E+02 0.28949071856666E+02 0.16395505851121E+03 0.97439919225258E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33535062516998E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87386207668764E-01 0.00000000000000E+00 0.00000000000000E+00 0.87386207668764E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.94401397153872E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.94401397153872E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918731637420E-01 0.98650944570493E-01 0.31730020322724E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    270.05812366
 0.41710874581975E-01 0.30499330366601E+03 0.43218240286350E+03 0.43041401334189E+03 0.42969100041782E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17689415527478E+00 0.00000000000000E+00 -.13024806046767E+02
 0.85684890282962E-02 0.77611263186068E+00 0.93365352672813E+03 0.35012007252305E+03 0.10307782236221E+02
 0.38654183385827E+01 0.31603395110233E+03 0.29315000000232E+03 0.31289784843275E+03 0.32953493941206E+03
 0.29315000000049E+03 0.29315000000053E+03 0.31036930459996E+03 0.32948611158763E+03 0.29315000000048E+03
 0.29315000000053E+03 0.31289784843275E+03 0.32953493941206E+03 0.29315000000049E+03 0.29315000000053E+03
 0.31036930459996E+03 0.32948611158763E+03 0.29315000000048E+03 0.29315000000053E+03 0.36792077197035E+03
 0.29523513638084E+03 0.18443950256456E+04 0.18009062107542E+04 0.71264418001920E+03 0.13793610096224E+04
 0.66315360870311E+03 0.10589203122567E+04 0.10531122322072E+04 0.10589203122567E+04 0.18401206913340E+04
 0.93983160612115E+03 0.10523528748899E+04 0.93983160612115E+03 0.18398293362531E+04 0.10589203122567E+04
 0.10531122322072E+04 0.10589203122567E+04 0.18401206913340E+04 0.93983160612115E+03 0.10523528748899E+04
 0.93983160612115E+03 0.18398293362531E+04 0.18603533009515E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47614234600466E+03 0.12948004570860E+01
 0.12948004570860E+01 0.90028168700595E+00 0.31842009991767E+00 0.29352529551477E+03 0.31731152135559E+03
 0.31226217974966E+03 0.31194531114339E+03 0.23000000000000E+00 0.00000000000000E+00 0.21578974607410E+00
 0.00000000000000E+00 -.31389773226053E+01 0.99837283505485E-03 0.22212495451257E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36015764269058E+02 0.13505911600897E+02 0.29523435469371E+03 0.36795665169291E+03
 0.29352366555933E+03 0.29606332337658E+03 0.29315000000045E+03 0.29315000000045E+03 0.29351926945852E+03
 0.29606374868438E+03 0.29315000000045E+03 0.29315000000045E+03 0.29352366555933E+03 0.29606332337658E+03
 0.29315000000045E+03 0.29315000000045E+03 0.29351926945852E+03 0.29606374868438E+03 0.29315000000045E+03
 0.29315000000045E+03 0.29564092391370E+03 0.29315000000050E+03 0.35402631003845E+02 0.31669219486737E+02
 0.39166882817237E+02 0.21141543442037E+03 0.17205271718905E+03 0.31864536549603E+02 0.36401149558192E+02
 0.31864536549603E+02 0.16624409902044E+03 0.31466139260188E+02 0.36407019120665E+02 0.31466139260188E+02
 0.16624683377869E+03 0.31864536549603E+02 0.36401149558192E+02 0.31864536549603E+02 0.16624409902044E+03
 0.31466139260187E+02 0.36407019120665E+02 0.31466139260187E+02 0.16624683377869E+03 0.99645825792521E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33541195948427E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87368499491492E-01 0.00000000000000E+00 0.00000000000000E+00 0.87368499491492E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.94853621122584E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.94853621122584E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88918893937204E-01 0.98647601870494E-01 0.31731152135559E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    280.01561185
 0.41720229429663E-01 0.30544510852957E+03 0.43244413100766E+03 0.43067798822262E+03 0.42995803038913E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17483362259462E+00 0.00000000000000E+00 -.13039476948575E+02
 0.85665670795326E-02 0.80589783048850E+00 0.93386299619526E+03 0.35019862357322E+03 0.99268166476521E+01
 0.37225562428696E+01 0.31679445920215E+03 0.29315000000368E+03 0.31356318513242E+03 0.33052144285005E+03
 0.29315000000052E+03 0.29315000000059E+03 0.31097550879132E+03 0.33047455762119E+03 0.29315000000050E+03
 0.29315000000059E+03 0.31356318513243E+03 0.33052144285005E+03 0.29315000000052E+03 0.29315000000059E+03
 0.31097550879132E+03 0.33047455762119E+03 0.29315000000050E+03 0.29315000000059E+03 0.36932967435663E+03
 0.29554009640418E+03 0.18548927849553E+04 0.18098296542329E+04 0.71182936077754E+03 0.13641430814479E+04
 0.64875457386647E+03 0.10654287312411E+04 0.10594869669128E+04 0.10654287312411E+04 0.18393287448051E+04
 0.94678080840975E+03 0.10587879823329E+04 0.94678080840975E+03 0.18390780243916E+04 0.10654287312411E+04
 0.10594869669128E+04 0.10654287312411E+04 0.18393287448051E+04 0.94678080840975E+03 0.10587879823329E+04
 0.94678080840975E+03 0.18390780243916E+04 0.18584379086470E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47640750717208E+03 0.12948005651716E+01
 0.12948005651716E+01 0.94011163977413E+00 0.30246292720916E+00 0.29363324860794E+03 0.31729361709566E+03
 0.31252269422123E+03 0.31222068742383E+03 0.23000000000000E+00 0.00000000000000E+00 0.21482536674237E+00
 0.00000000000000E+00 -.31438033681066E+01 0.99800573970988E-03 0.24398557351796E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32788823882700E+02 0.12295808956012E+02 0.29553928423874E+03 0.36936472505605E+03
 0.29356580727393E+03 0.29618154535518E+03 0.29315000000044E+03 0.29315000000044E+03 0.29356092486131E+03
 0.29618191955262E+03 0.29315000000044E+03 0.29315000000044E+03 0.29356580727393E+03 0.29618154535518E+03
 0.29315000000044E+03 0.29315000000044E+03 0.29356092486131E+03 0.29618191955262E+03 0.29315000000044E+03
 0.29315000000044E+03 0.29574378418619E+03 0.29315000000060E+03 0.37296865759443E+02 0.32980619481116E+02
 0.42598935743139E+02 0.21382499176370E+03 0.17101306134185E+03 0.34326203558207E+02 0.39619002194244E+02
 0.34388824816115E+02 0.16823571240613E+03 0.33904733400067E+02 0.39617475086133E+02 0.33973074865308E+02
 0.16823143549187E+03 0.34326203558207E+02 0.39619002194244E+02 0.34388824816115E+02 0.16823571240613E+03
 0.33904733400068E+02 0.39617475086133E+02 0.33973074865308E+02 0.16823143549187E+03 0.10159684648009E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33545034873298E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87379607363104E-01 0.00000000000000E+00 0.00000000000000E+00 0.87379607363104E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.95240462184349E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.95240462184349E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88919074083332E-01 0.98653363529041E-01 0.31729361709566E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    290.00696144
 0.41728293315872E-01 0.30588519236014E+03 0.43273287987226E+03 0.43096850070195E+03 0.43025114431918E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17279734141832E+00 0.00000000000000E+00 -.13056298088432E+02
 0.85649109002259E-02 0.83523908644232E+00 0.93404357537321E+03 0.35026634076495E+03 0.95780958169424E+01
 0.35917859313534E+01 0.31753637133008E+03 0.29315000000582E+03 0.31421368328791E+03 0.33148150430199E+03
 0.29315000000059E+03 0.29315000000071E+03 0.31156911979976E+03 0.33143638327968E+03 0.29315000000055E+03
 0.29315000000071E+03 0.31421368328791E+03 0.33148150430199E+03 0.29315000000059E+03 0.29315000000071E+03
 0.31156911979976E+03 0.33143638327968E+03 0.29315000000055E+03 0.29315000000071E+03 0.37068346145879E+03
 0.29586682036241E+03 0.18648742145433E+04 0.18182609835642E+04 0.71101132118200E+03 0.13499873596830E+04
 0.63542098189507E+03 0.10716346281287E+04 0.10655566507040E+04 0.10716346281287E+04 0.18387368292978E+04
 0.95341734675719E+03 0.10649137009646E+04 0.95341734675719E+03 0.18385242519262E+04 0.10716346281287E+04
 0.10655566507040E+04 0.10716346281287E+04 0.18387368292978E+04 0.95341734675719E+03 0.10649137009646E+04
 0.95341734675719E+03 0.18385242519262E+04 0.18567747774660E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47669895557832E+03 0.12948006890988E+01
 0.12948006890988E+01 0.98007703814875E+00 0.28715312087032E+00 0.29376333915604E+03 0.31725377664993E+03
 0.31275687502528E+03 0.31247050744881E+03 0.23000000000000E+00 0.00000000000000E+00 0.21383845646310E+00
 0.00000000000000E+00 -.31486272060824E+01 0.99756373401005E-03 0.26615331415495E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30057863549062E+02 0.11271698830898E+02 0.29586598034627E+03 0.37071749292219E+03
 0.29360986833939E+03 0.29629691940418E+03 0.29315000000043E+03 0.29315000000044E+03 0.29360450224644E+03
 0.29629723207119E+03 0.29315000000043E+03 0.29315000000044E+03 0.29360986833939E+03 0.29629691940418E+03
 0.29315000000043E+03 0.29315000000044E+03 0.29360450224644E+03 0.29629723207119E+03 0.29315000000043E+03
 0.29315000000044E+03 0.29584402399355E+03 0.29315000000075E+03 0.38968350785931E+02 0.34050626001260E+02
 0.45920584060844E+02 0.21594266710705E+03 0.16979248012590E+03 0.36768774738531E+02 0.42720218815345E+02
 0.36946492769864E+02 0.16999266905471E+03 0.36326786209555E+02 0.42710781067059E+02 0.36512350981031E+02
 0.16998094079172E+03 0.36768774738531E+02 0.42720218815345E+02 0.36946492769864E+02 0.16999266905471E+03
 0.36326786209555E+02 0.42710781067059E+02 0.36512350981031E+02 0.16998094079172E+03 0.10334415493386E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33547187694689E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87404580959683E-01 0.00000000000000E+00 0.00000000000000E+00 0.87404580959683E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.95574099977819E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.95574099977819E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88919275867565E-01 0.98665971533608E-01 0.31725377664993E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    300.00033740
 0.41738466631022E-01 0.30631383671553E+03 0.43304347783755E+03 0.43128031087184E+03 0.43056511059262E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17079144140041E+00 0.00000000000000E+00 -.13074667690633E+02
 0.85628225342367E-02 0.86405223680869E+00 0.93427137699206E+03 0.35035176637202E+03 0.92586994850536E+01
 0.34720123068951E+01 0.31825931824402E+03 0.29315000000910E+03 0.31484891409814E+03 0.33241517710578E+03
 0.29315000000069E+03 0.29315000000089E+03 0.31214960416829E+03 0.33237166479873E+03 0.29315000000062E+03
 0.29315000000089E+03 0.31484891409814E+03 0.33241517710578E+03 0.29315000000069E+03 0.29315000000089E+03
 0.31214960416829E+03 0.33237166479873E+03 0.29315000000062E+03 0.29315000000089E+03 0.37198440828305E+03
 0.29621389505263E+03 0.18743554177257E+04 0.18262207857290E+04 0.71017918277157E+03 0.13367826379693E+04
 0.62305255928387E+03 0.10775466999179E+04 0.10713265052759E+04 0.10775466999179E+04 0.18383021078105E+04
 0.95974993337219E+03 0.10707353592994E+04 0.95974993337219E+03 0.18381250492854E+04 0.10775466999179E+04
 0.10713265052759E+04 0.10775466999179E+04 0.18383021078105E+04 0.95974993337219E+03 0.10707353592994E+04
 0.95974993337219E+03 0.18381250492854E+04 0.18553162981859E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47701143979022E+03 0.12948008244340E+01
 0.12948008244340E+01 0.10200505419720E+01 0.27252276532104E+00 0.29391741696568E+03 0.31719805618622E+03
 0.31296838673367E+03 0.31269811977974E+03 0.23000000000000E+00 0.00000000000000E+00 0.21283326375612E+00
 0.00000000000000E+00 -.31531877792659E+01 0.99704074482612E-03 0.28854914929052E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27724912791011E+02 0.10396842296629E+02 0.29621303082987E+03 0.37201727247319E+03
 0.29365566957678E+03 0.29640920704981E+03 0.29315000000042E+03 0.29315000000044E+03 0.29364983188048E+03
 0.29640944848025E+03 0.29315000000042E+03 0.29315000000044E+03 0.29365566957678E+03 0.29640920704981E+03
 0.29315000000042E+03 0.29315000000044E+03 0.29364983188048E+03 0.29640944848025E+03 0.29315000000042E+03
 0.29315000000044E+03 0.29594145893916E+03 0.29315000000096E+03 0.40408249926906E+02 0.34882414780834E+02
 0.49115165784657E+02 0.21780936769066E+03 0.16844862607708E+03 0.39182618607362E+02 0.45690951772600E+02
 0.39534661148711E+02 0.17154653893747E+03 0.38722616237688E+02 0.45673230864950E+02 0.39084586212149E+02
 0.17152705494867E+03 0.39182618607362E+02 0.45690951772600E+02 0.39534661148711E+02 0.17154653893747E+03
 0.38722616237688E+02 0.45673230864950E+02 0.39084586212149E+02 0.17152705494867E+03 0.10490301533084E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33548099668323E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87437858572840E-01 0.00000000000000E+00 0.00000000000000E+00 0.87437858572840E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.95859611303877E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.95859611303877E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88919494297637E-01 0.98683541622191E-01 0.31719805618622E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    310.01092209
 0.41750471616470E-01 0.30673307771705E+03 0.43337282229434E+03 0.43161039927384E+03 0.43089697990233E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16881249157407E+00 0.00000000000000E+00 -.13093907518897E+02
 0.85603595711336E-02 0.89239072077214E+00 0.93454018298213E+03 0.35045256861830E+03 0.89646830853172E+01
 0.33617561569939E+01 0.31896612640492E+03 0.29315000001409E+03 0.31547126231066E+03 0.33332649855272E+03
 0.29315000000087E+03 0.29315000000120E+03 0.31271903397048E+03 0.33328446419973E+03 0.29315000000076E+03
 0.29315000000119E+03 0.31547126231066E+03 0.33332649855272E+03 0.29315000000087E+03 0.29315000000120E+03
 0.31271903397048E+03 0.33328446419973E+03 0.29315000000076E+03 0.29315000000119E+03 0.37323976350131E+03
 0.29658140301450E+03 0.18833962475236E+04 0.18337658742194E+04 0.70932608641786E+03 0.13243871198292E+04
 0.61151440297927E+03 0.10832005091084E+04 0.10768286631270E+04 0.10832005091084E+04 0.18379891900931E+04
 0.96581598482834E+03 0.10762854704966E+04 0.96581598482834E+03 0.18378451501836E+04 0.10832005091084E+04
 0.10768286631270E+04 0.10832005091084E+04 0.18379891900931E+04 0.96581598482834E+03 0.10762854704965E+04
 0.96581598482834E+03 0.18378451501836E+04 0.18540215187277E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47734198014278E+03 0.12948009661806E+01
 0.12948009661806E+01 0.10600928807488E+01 0.25853022470215E+00 0.29409775541745E+03 0.31713138523952E+03
 0.31316145891045E+03 0.31290755181800E+03 0.23000000000000E+00 0.00000000000000E+00 0.21180930425729E+00
 0.00000000000000E+00 -.31570307162238E+01 0.99642932940526E-03 0.31119821729383E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25707088136840E+02 0.96401580513150E+01 0.29658051884972E+03 0.37327135135424E+03
 0.29370330979463E+03 0.29651874377597E+03 0.29315000000042E+03 0.29315000000044E+03 0.29369701392654E+03
 0.29651890477895E+03 0.29315000000042E+03 0.29315000000044E+03 0.29370330979463E+03 0.29651874377597E+03
 0.29315000000042E+03 0.29315000000044E+03 0.29369701392654E+03 0.29651890477895E+03 0.29315000000042E+03
 0.29315000000044E+03 0.29603640042633E+03 0.29315000000128E+03 0.41618790418027E+02 0.35487620207023E+02
 0.52185076571176E+02 0.21947047368467E+03 0.16702447173064E+03 0.41571087365012E+02 0.48535079583212E+02
 0.42168325554011E+02 0.17293298451278E+03 0.41095440737627E+02 0.48508796106384E+02 0.41704744771556E+02
 0.17290552901012E+03 0.41571087365012E+02 0.48535079583212E+02 0.42168325554011E+02 0.17293298451278E+03
 0.41095440737627E+02 0.48508796106384E+02 0.41704744771556E+02 0.17290552901012E+03 0.10629619976945E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33548158054081E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87470484394414E-01 0.00000000000000E+00 0.00000000000000E+00 0.87470484394414E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96102659069605E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96102659069605E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88919729921664E-01 0.98704545799823E-01 0.31713138523952E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    320.00785366
 0.41762813890158E-01 0.30714288328678E+03 0.43371700715806E+03 0.43195497663187E+03 0.43124303354299E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16686614324677E+00 0.00000000000000E+00 -.13113658362739E+02
 0.85578288770234E-02 0.92017749610737E+00 0.93481654225161E+03 0.35055620334435E+03 0.86939748405525E+01
 0.32602405652072E+01 0.31965617669348E+03 0.29315000002150E+03 0.31608008852327E+03 0.33421493989994E+03
 0.29315000000115E+03 0.29315000000170E+03 0.31327674022996E+03 0.33417426646241E+03 0.29315000000097E+03
 0.29315000000169E+03 0.31608008852327E+03 0.33421493989994E+03 0.29315000000115E+03 0.29315000000170E+03
 0.31327674022996E+03 0.33417426646241E+03 0.29315000000097E+03 0.29315000000169E+03 0.37445012317804E+03
 0.29696764404950E+03 0.18920122302009E+04 0.18409158082635E+04 0.70845423756343E+03 0.13127421711437E+04
 0.60074566239246E+03 0.10886038517292E+04 0.10820695603404E+04 0.10886038517292E+04 0.18377739115037E+04
 0.97162308981452E+03 0.10815706207391E+04 0.97162308981452E+03 0.18376603961698E+04 0.10886038517292E+04
 0.10820695603404E+04 0.10886038517292E+04 0.18377739115037E+04 0.97162308981452E+03 0.10815706207391E+04
 0.97162308981452E+03 0.18376603961697E+04 0.18528670719186E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47768683588730E+03 0.12948011116920E+01
 0.12948011116920E+01 0.11000806070293E+01 0.24519713630757E+00 0.29430558366772E+03 0.31705816839420E+03
 0.31333892264851E+03 0.31310139322800E+03 0.23000000000000E+00 0.00000000000000E+00 0.21077071330516E+00
 0.00000000000000E+00 -.31600519187232E+01 0.99572565636383E-03 0.33402150427947E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23950553774246E+02 0.89814576653423E+01 0.29696674463908E+03 0.37448036378057E+03
 0.29375270301867E+03 0.29662538339672E+03 0.29315000000041E+03 0.29315000000046E+03 0.29374596574987E+03
 0.29662545574504E+03 0.29315000000041E+03 0.29315000000046E+03 0.29375270301867E+03 0.29662538339672E+03
 0.29315000000041E+03 0.29315000000046E+03 0.29374596574987E+03 0.29662545574504E+03 0.29315000000041E+03
 0.29315000000046E+03 0.29612874074559E+03 0.29315000000178E+03 0.42599472940735E+02 0.35878036253861E+02
 0.55121072549561E+02 0.22095840690016E+03 0.16556172898785E+03 0.43927488534940E+02 0.51245708778225E+02
 0.44852538406324E+02 0.17417732079835E+03 0.43438489151997E+02 0.51210709566302E+02 0.44377818534121E+02
 0.17414179657758E+03 0.43927488534940E+02 0.51245708778225E+02 0.44852538406324E+02 0.17417732079835E+03
 0.43438489151997E+02 0.51210709566302E+02 0.44377818534121E+02 0.17414179657758E+03 0.10753905126947E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33547696878906E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87499675915562E-01 0.00000000000000E+00 0.00000000000000E+00 0.87499675915562E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96307781026425E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96307781026425E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88919977934332E-01 0.98727611628443E-01 0.31705816839420E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    330.00799702
 0.41774410145938E-01 0.30754488864140E+03 0.43407420875873E+03 0.43231231285404E+03 0.43160160353648E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16494859341931E+00 0.00000000000000E+00 -.13133784484813E+02
 0.85554524337239E-02 0.94747063275204E+00 0.93507620572649E+03 0.35065357714743E+03 0.84435334705447E+01
 0.31663250514543E+01 0.32033201208691E+03 0.29315000003233E+03 0.31667754519446E+03 0.33508399014050E+03
 0.29315000000159E+03 0.29315000000248E+03 0.31382462437365E+03 0.33504457800802E+03 0.29315000000131E+03
 0.29315000000247E+03 0.31667754519446E+03 0.33508399014050E+03 0.29315000000159E+03 0.29315000000248E+03
 0.31382462437365E+03 0.33504457800802E+03 0.29315000000131E+03 0.29315000000247E+03 0.37562146150248E+03
 0.29737261165457E+03 0.19002579009813E+04 0.18477212342141E+04 0.70756346790215E+03 0.13017465658579E+04
 0.59064528061627E+03 0.10937890885863E+04 0.10870803846148E+04 0.10937890885863E+04 0.18376369931383E+04
 0.97720518540058E+03 0.10866223606209E+04 0.97720518540058E+03 0.18375516909771E+04 0.10937890885863E+04
 0.10870803846148E+04 0.10937890885863E+04 0.18376369931383E+04 0.97720518540058E+03 0.10866223606209E+04
 0.97720518540058E+03 0.18375516909771E+04 0.18518299010641E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47804431655211E+03 0.12948012599683E+01
 0.12948012599683E+01 0.11400811804760E+01 0.23247774723012E+00 0.29454271644833E+03 0.31698177094004E+03
 0.31350405038123E+03 0.31328278293561E+03 0.23000000000000E+00 0.00000000000000E+00 0.20971665877192E+00
 0.00000000000000E+00 -.31622666462662E+01 0.99492398791909E-03 0.35704813181516E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22405942748754E+02 0.84022285307827E+01 0.29737170203033E+03 0.37565031256900E+03
 0.29380401418030E+03 0.29672950850591E+03 0.29315000000041E+03 0.29315000000051E+03 0.29379685304939E+03
 0.29672948462934E+03 0.29315000000041E+03 0.29315000000051E+03 0.29380401418030E+03 0.29672950850591E+03
 0.29315000000041E+03 0.29315000000051E+03 0.29379685304939E+03 0.29672948462934E+03 0.29315000000041E+03
 0.29315000000051E+03 0.29621882701065E+03 0.29315000000252E+03 0.43357596971731E+02 0.36069981182506E+02
 0.57930316099309E+02 0.22230854544055E+03 0.16408857776074E+03 0.46256977715538E+02 0.53831146418947E+02
 0.47605879221174E+02 0.17530773890881E+03 0.45756753791487E+02 0.53787359039112E+02 0.47122230358128E+02
 0.17526412425827E+03 0.46256977715538E+02 0.53831146418947E+02 0.47605879221174E+02 0.17530773890881E+03
 0.45756753791487E+02 0.53787359039112E+02 0.47122230358128E+02 0.17526412425827E+03 0.10865218231966E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33546997238005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87524644978454E-01 0.00000000000000E+00 0.00000000000000E+00 0.87524644978454E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96480133451085E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96480133451085E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88920234159601E-01 0.98751688888097E-01 0.31698177094004E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    340.00234806
 0.41784423660760E-01 0.30793953365424E+03 0.43444211621603E+03 0.43268017038138E+03 0.43197049910162E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16306108180602E+00 0.00000000000000E+00 -.13154010320942E+02
 0.85534012669655E-02 0.97425595430940E+00 0.93530044368399E+03 0.35073766638150E+03 0.82113945155929E+01
 0.30792729433474E+01 0.32099420886392E+03 0.29315000004784E+03 0.31726405906475E+03 0.33593454935937E+03
 0.29315000000227E+03 0.29315000000368E+03 0.31436302018564E+03 0.33589631021604E+03 0.29315000000182E+03
 0.29315000000366E+03 0.31726405906475E+03 0.33593454935937E+03 0.29315000000227E+03 0.29315000000368E+03
 0.31436302018564E+03 0.33589631021604E+03 0.29315000000182E+03 0.29315000000366E+03 0.37675607324023E+03
 0.29779525195944E+03 0.19081610883325E+04 0.18542100245438E+04 0.70665678127746E+03 0.12913426951027E+04
 0.58115262991882E+03 0.10987720356013E+04 0.10918767568594E+04 0.10987720356013E+04 0.18375642192648E+04
 0.98257847073563E+03 0.10914565384887E+04 0.98257847073563E+03 0.18375049288786E+04 0.10987720356013E+04
 0.10918767568594E+04 0.10987720356013E+04 0.18375642192648E+04 0.98257847073563E+03 0.10914565384887E+04
 0.98257847073563E+03 0.18375049288786E+04 0.18508947451634E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47841221569273E+03 0.12948014089792E+01
 0.12948014089792E+01 0.11800585846348E+01 0.22028301644653E+00 0.29481014760797E+03 0.31690373269159E+03
 0.31365917164736E+03 0.31345398512508E+03 0.23000000000000E+00 0.00000000000000E+00 0.20865030092177E+00
 0.00000000000000E+00 -.31635427421769E+01 0.99402144989560E-03 0.38022539615353E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21040151659858E+02 0.78900568724469E+01 0.29779433745420E+03 0.37678351875106E+03
 0.29385730179822E+03 0.29683122382604E+03 0.29315000000040E+03 0.29315000000058E+03 0.29384973620088E+03
 0.29683109706686E+03 0.29315000000040E+03 0.29315000000058E+03 0.29385730179822E+03 0.29683122382604E+03
 0.29315000000040E+03 0.29315000000058E+03 0.29384973620088E+03 0.29683109706686E+03 0.29315000000040E+03
 0.29315000000058E+03 0.29630670375941E+03 0.29315000000362E+03 0.43896464594732E+02 0.36076655182104E+02
 0.60609483261442E+02 0.22351148020294E+03 0.16259894952519E+03 0.48557511571227E+02 0.56289104184778E+02
 0.50439149078985E+02 0.17634122510723E+03 0.48048214247066E+02 0.56236543945330E+02 0.49948797485594E+02
 0.17628957915958E+03 0.48557511571227E+02 0.56289104184778E+02 0.50439149078985E+02 0.17634122510723E+03
 0.48048214247066E+02 0.56236543945330E+02 0.49948797485593E+02 0.17628957915958E+03 0.10964277354175E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33540934988823E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87542661471071E-01 0.00000000000000E+00 0.00000000000000E+00 0.87542661471071E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96621912544364E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96621912544364E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88920499412456E-01 0.98776300119519E-01 0.31690373269159E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    350.00084140
 0.41791569813205E-01 0.30832804830594E+03 0.43481943345541E+03 0.43305734227099E+03 0.43234856646246E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16120123528643E+00 0.00000000000000E+00 -.13174182347743E+02
 0.85519377803881E-02 0.10005704032063E+01 0.93546050093420E+03 0.35079768785033E+03 0.79954393757444E+01
 0.29982897659042E+01 0.32164446762677E+03 0.29315000006975E+03 0.31784105738686E+03 0.33676891286629E+03
 0.29315000000329E+03 0.29315000000547E+03 0.31489318982515E+03 0.33673176998768E+03 0.29315000000261E+03
 0.29315000000544E+03 0.31784105738686E+03 0.33676891286629E+03 0.29315000000329E+03 0.29315000000547E+03
 0.31489318982515E+03 0.33673176998768E+03 0.29315000000261E+03 0.29315000000544E+03 0.37785800681511E+03
 0.29823522028127E+03 0.19157616847778E+04 0.18604203182544E+04 0.70573609779288E+03 0.12814633616508E+04
 0.57219858336895E+03 0.11035762131134E+04 0.10964819665150E+04 0.11035762131134E+04 0.18375448435389E+04
 0.98776750675226E+03 0.10960967263495E+04 0.98776750675226E+03 0.18375095264715E+04 0.11035762131134E+04
 0.10964819665150E+04 0.11035762131134E+04 0.18375448435389E+04 0.98776750675227E+03 0.10960967263495E+04
 0.98776750675227E+03 0.18375095264715E+04 0.18500487048181E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47878936240332E+03 0.12948015575938E+01
 0.12948015575938E+01 0.12200525579768E+01 0.20843191928481E+00 0.29510922363820E+03 0.31682415234429E+03
 0.31380676283273E+03 0.31361752189894E+03 0.23000000000000E+00 0.00000000000000E+00 0.20757417484309E+00
 0.00000000000000E+00 -.31638136767756E+01 0.99301406432690E-03 0.40351873701945E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19825597341752E+02 0.74345990031570E+01 0.29823430668947E+03 0.37788404878429E+03
 0.29391273642280E+03 0.29693068988605E+03 0.29315000000040E+03 0.29315000000070E+03 0.29390478782888E+03
 0.29693045425362E+03 0.29315000000040E+03 0.29315000000070E+03 0.29391273642279E+03 0.29693068988605E+03
 0.29315000000040E+03 0.29315000000070E+03 0.29390478782888E+03 0.29693045425362E+03 0.29315000000040E+03
 0.29315000000070E+03 0.29639253038948E+03 0.29315000000521E+03 0.44219829584194E+02 0.35910530292713E+02
 0.63157941157672E+02 0.22458620716270E+03 0.16111247629924E+03 0.50830656673478E+02 0.58619784038538E+02
 0.53368382207806E+02 0.17726372573889E+03 0.50314613579082E+02 0.58558514134419E+02 0.52873723871409E+02
 0.17720415043301E+03 0.50830656673480E+02 0.58619784038538E+02 0.53368382207809E+02 0.17726372573889E+03
 0.50314613579082E+02 0.58558514134419E+02 0.52873723871409E+02 0.17720415043301E+03 0.11051918201045E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33534442073725E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87552156107959E-01 0.00000000000000E+00 0.00000000000000E+00 0.87552156107959E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96735365460919E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96735365460919E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88920776494011E-01 0.98801418498813E-01 0.31682415234429E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    360.00105203
 0.41795821675877E-01 0.30871076032081E+03 0.43520466309885E+03 0.43344235756431E+03 0.43273435674773E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15936903193067E+00 0.00000000000000E+00 -.13194423159676E+02
 0.85510668838787E-02 0.10264173395382E+01 0.93555577434231E+03 0.35083341537836E+03 0.77941005981051E+01
 0.29227877242894E+01 0.32228353378675E+03 0.29315000010025E+03 0.31840913196295E+03 0.33758811748678E+03
 0.29315000000478E+03 0.29315000000809E+03 0.31541563697872E+03 0.33755200261611E+03 0.29315000000375E+03
 0.29315000000805E+03 0.31840913196295E+03 0.33758811748678E+03 0.29315000000478E+03 0.29315000000809E+03
 0.31541563697872E+03 0.33755200261611E+03 0.29315000000375E+03 0.29315000000805E+03 0.37892952933266E+03
 0.29869169577694E+03 0.19230854039590E+04 0.18663761742557E+04 0.70480439771177E+03 0.12720624200772E+04
 0.56373400037690E+03 0.11082165222350E+04 0.11009113692540E+04 0.11082165222350E+04 0.18375704944056E+04
 0.99278760801745E+03 0.11005584940746E+04 0.99278760801745E+03 0.18375572387850E+04 0.11082165222350E+04
 0.11009113692540E+04 0.11082165222350E+04 0.18375704944056E+04 0.99278760801745E+03 0.11005584940746E+04
 0.99278760801745E+03 0.18375572387850E+04 0.18492816768396E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47917429875386E+03 0.12948017067151E+01
 0.12948017067151E+01 0.12600534004886E+01 0.19693926397528E+00 0.29544074172054E+03 0.31674615921417E+03
 0.31394891038758E+03 0.31377531763707E+03 0.23000000000000E+00 0.00000000000000E+00 0.20648895798616E+00
 0.00000000000000E+00 -.31633442578136E+01 0.99189979431028E-03 0.42691596819608E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18739050764027E+02 0.70271440365101E+01 0.29869078893453E+03 0.37895418484531E+03
 0.29397045630552E+03 0.29702801788518E+03 0.29315000000040E+03 0.29315000000086E+03 0.29396214787391E+03
 0.29702766821103E+03 0.29315000000040E+03 0.29315000000086E+03 0.29397045630552E+03 0.29702801788518E+03
 0.29315000000040E+03 0.29315000000086E+03 0.29396214787391E+03 0.29702766821103E+03 0.29315000000040E+03
 0.29315000000086E+03 0.29647645841422E+03 0.29315000000744E+03 0.44338446069516E+02 0.35590730217897E+02
 0.65582949152022E+02 0.22557011068039E+03 0.15965924678260E+03 0.53079160234774E+02 0.60831894128933E+02
 0.56410113255670E+02 0.17810368555119E+03 0.52558569669782E+02 0.60762048650789E+02 0.55913402203528E+02
 0.17803634760417E+03 0.53079160234774E+02 0.60831894128933E+02 0.56410113255670E+02 0.17810368555119E+03
 0.52558569669782E+02 0.60762048650789E+02 0.55913402203528E+02 0.17803634760417E+03 0.11130161358176E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33528463291498E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87556564935668E-01 0.00000000000000E+00 0.00000000000000E+00 0.87556564935668E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96825504969617E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96825504969617E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88921056971077E-01 0.98826058775408E-01 0.31674615921417E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    370.00600772
 0.41797846526923E-01 0.30908808388712E+03 0.43559668036076E+03 0.43383408472751E+03 0.43312674589626E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15756346319433E+00 0.00000000000000E+00 -.13214813155287E+02
 0.85506517100846E-02 0.10518140049456E+01 0.93560119991378E+03 0.35085044996767E+03 0.76059074725988E+01
 0.28522153022245E+01 0.32291239377332E+03 0.29315000014212E+03 0.31896909828332E+03 0.33839348642263E+03
 0.29315000000692E+03 0.29315000001187E+03 0.31593107721263E+03 0.33835833903999E+03 0.29315000000540E+03
 0.29315000001181E+03 0.31896909828332E+03 0.33839348642263E+03 0.29315000000692E+03 0.29315000001187E+03
 0.31593107721263E+03 0.33835833903999E+03 0.29315000000540E+03 0.29315000001181E+03 0.37997320105616E+03
 0.29916410916485E+03 0.19301580573079E+04 0.18721008235810E+04 0.70386347545024E+03 0.12630939300726E+04
 0.55571113724506E+03 0.11127081909602E+04 0.11051805657778E+04 0.11127081909602E+04 0.18376336885714E+04
 0.99765452485879E+03 0.11048576568388E+04 0.99765452485879E+03 0.18376407171846E+04 0.11127081909602E+04
 0.11051805657778E+04 0.11127081909602E+04 0.18376336885714E+04 0.99765452485879E+03 0.11048576568387E+04
 0.99765452485879E+03 0.18376407171846E+04 0.18485838328076E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47956589426809E+03 0.12948018569356E+01
 0.12948018569356E+01 0.13000732232572E+01 0.18580148810770E+00 0.29580535589454E+03 0.31667205116345E+03
 0.31408734247488E+03 0.31392898060173E+03 0.23000000000000E+00 0.00000000000000E+00 0.20539482427989E+00
 0.00000000000000E+00 -.31623291378947E+01 0.99067717348891E-03 0.45041619363165E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17761350753171E+02 0.66605065324393E+01 0.29916321461464E+03 0.37999649767652E+03
 0.29403065307304E+03 0.29712341626481E+03 0.29315000000040E+03 0.29315000000110E+03 0.29402200902382E+03
 0.29712294815868E+03 0.29315000000040E+03 0.29315000000110E+03 0.29403065307304E+03 0.29712341626481E+03
 0.29315000000040E+03 0.29315000000110E+03 0.29402200902382E+03 0.29712294815868E+03 0.29315000000040E+03
 0.29315000000110E+03 0.29655868725451E+03 0.29315000001054E+03 0.44263111057924E+02 0.35134831566347E+02
 0.67892945611104E+02 0.22649093259599E+03 0.15825852225683E+03 0.55306792803075E+02 0.62934783432386E+02
 0.59582033676564E+02 0.17888461629795E+03 0.54783746264008E+02 0.62856555786811E+02 0.59085405096526E+02
 0.17880973609046E+03 0.55306792803075E+02 0.62934783432386E+02 0.59582033676564E+02 0.17888461629795E+03
 0.54783746264008E+02 0.62856555786811E+02 0.59085405096526E+02 0.17880973609046E+03 0.11200699426820E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33523194210992E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87558302986285E-01 0.00000000000000E+00 0.00000000000000E+00 0.87558302986285E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96896492122572E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96896492122572E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88921334789372E-01 0.98849496020094E-01 0.31667205116345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    380.01403296
 0.41798470630661E-01 0.30946021360134E+03 0.43599431182748E+03 0.43423133456465E+03 0.43352454710224E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15578441011743E+00 0.00000000000000E+00 -.13235392200866E+02
 0.85505231008351E-02 0.10767650519977E+01 0.93561527238242E+03 0.35085572714341E+03 0.74296616380311E+01
 0.27861231142617E+01 0.32353163914283E+03 0.29315000019891E+03 0.31952142834490E+03 0.33918581755398E+03
 0.29315000000994E+03 0.29315000001725E+03 0.31643991521134E+03 0.33915158331142E+03 0.29315000000775E+03
 0.29315000001716E+03 0.31952142834490E+03 0.33918581755398E+03 0.29315000000994E+03 0.29315000001725E+03
 0.31643991521134E+03 0.33915158331143E+03 0.29315000000775E+03 0.29315000001716E+03 0.38099082241260E+03
 0.29965167304668E+03 0.19369987272287E+04 0.18776116333880E+04 0.70291486960949E+03 0.12545208521157E+04
 0.54809140815815E+03 0.11170623718647E+04 0.11093010612729E+04 0.11170623718647E+04 0.18377275071340E+04
 0.10023796875281E+04 0.11090059024431E+04 0.10023796875281E+04 0.18377531616285E+04 0.11170623718647E+04
 0.11093010612729E+04 0.11170623718647E+04 0.18377275071340E+04 0.10023796875281E+04 0.11090059024431E+04
 0.10023796875281E+04 0.18377531616284E+04 0.18479463471312E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47996295734871E+03 0.12948020085489E+01
 0.12948020085489E+01 0.13401053242006E+01 0.17501974024160E+00 0.29620326018661E+03 0.31660367686521E+03
 0.31422335977994E+03 0.31407970301444E+03 0.23000000000000E+00 0.00000000000000E+00 0.20429236582148E+00
 0.00000000000000E+00 -.31609186471129E+01 0.10292315379268E-02 0.47400911362120E+00 0.77727894115205E+04
 0.29147960293202E+04 0.16877312629886E+02 0.63289922362072E+01 0.29965079608767E+03 0.38101279579128E+03
 0.29409350683917E+03 0.29721705733237E+03 0.29315000000039E+03 0.29315000000145E+03 0.29408455236773E+03
 0.29721646723665E+03 0.29315000000039E+03 0.29315000000145E+03 0.29409350683917E+03 0.29721705733237E+03
 0.29315000000039E+03 0.29315000000145E+03 0.29408455236773E+03 0.29721646723665E+03 0.29315000000039E+03
 0.29315000000145E+03 0.29663937806772E+03 0.29315000001480E+03 0.44003943234772E+02 0.34558565193445E+02
 0.70095262047434E+02 0.22737140371427E+03 0.15692566535660E+03 0.57515543861663E+02 0.64936644813172E+02
 0.62899274025403E+02 0.17962589426066E+03 0.56992040415372E+02 0.64850286893456E+02 0.62404757069769E+02
 0.17954374475544E+03 0.57515543861663E+02 0.64936644813172E+02 0.62899274025403E+02 0.17962589426066E+03
 0.56992040415372E+02 0.64850286893456E+02 0.62404757069769E+02 0.17954374475544E+03 0.11264943652933E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33518789435867E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87559137863769E-01 0.00000000000000E+00 0.00000000000000E+00 0.87559137863769E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96951819606903E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96951819606903E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88921605081287E-01 0.98871145646694E-01 0.31660367686521E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    390.00419182
 0.41798468282785E-01 0.30982662115331E+03 0.43639572135377E+03 0.43463225651367E+03 0.43392591070968E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15403511159857E+00 0.00000000000000E+00 -.13256149993696E+02
 0.85505226377161E-02 0.11012282359934E+01 0.93561532305783E+03 0.35085574614669E+03 0.72646157613126E+01
 0.27242309104922E+01 0.32414062146382E+03 0.29315000027482E+03 0.32006549131427E+03 0.33996430934637E+03
 0.29315000001416E+03 0.29315000002477E+03 0.31694154241778E+03 0.33993093758507E+03 0.29315000001102E+03
 0.29315000002465E+03 0.32006549131427E+03 0.33996430934637E+03 0.29315000001416E+03 0.29315000002477E+03
 0.31694154241778E+03 0.33993093758507E+03 0.29315000001102E+03 0.29315000002465E+03 0.38198210435704E+03
 0.30015261247337E+03 0.19436114505878E+04 0.18829136695684E+04 0.70196143654492E+03 0.12463253050864E+04
 0.54085406135874E+03 0.11212807483531E+04 0.11132752691197E+04 0.11212807483531E+04 0.18378453594877E+04
 0.10069643096354E+04 0.11130057654488E+04 0.10069643096354E+04 0.18378880640288E+04 0.11212807483531E+04
 0.11132752691197E+04 0.11212807483531E+04 0.18378453594877E+04 0.10069643096354E+04 0.11130057654488E+04
 0.10069643096354E+04 0.18378880640288E+04 0.18473621551036E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48036363537043E+03 0.12948021614792E+01
 0.12948021614792E+01 0.13800659596641E+01 0.16461408258925E+00 0.29663348282500E+03 0.31654257647392E+03
 0.31435769834979E+03 0.31422810373370E+03 0.23000000000000E+00 0.00000000000000E+00 0.20318424561500E+00
 0.00000000000000E+00 -.31592466123487E+01 0.10942916839190E-02 0.49764028832401E+00 0.73106650791217E+04
 0.27414994046706E+04 0.16075868830763E+02 0.60284508115362E+01 0.30015175816911E+03 0.38200279828092E+03
 0.29415907998415E+03 0.29730893929815E+03 0.29315000000039E+03 0.29315000000195E+03 0.29414984156162E+03
 0.29730822471455E+03 0.29315000000039E+03 0.29315000000195E+03 0.29415907998415E+03 0.29730893929815E+03
 0.29315000000039E+03 0.29315000000195E+03 0.29414984156162E+03 0.29730822471455E+03 0.29315000000039E+03
 0.29315000000195E+03 0.29671853765827E+03 0.29315000002057E+03 0.43573137042727E+02 0.33878714640745E+02
 0.72193477482277E+02 0.22822893801818E+03 0.15567449314849E+03 0.59703823563225E+02 0.66842042188277E+02
 0.66370147933877E+02 0.18034245937147E+03 0.59181760902632E+02 0.66747867607485E+02 0.65879651840718E+02
 0.18025336788060E+03 0.59703823563225E+02 0.66842042188277E+02 0.66370147933877E+02 0.18034245937147E+03
 0.59181760902632E+02 0.66747867607485E+02 0.65879651840718E+02 0.18025336788060E+03 0.11324030782507E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33515372454552E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87560553716196E-01 0.00000000000000E+00 0.00000000000000E+00 0.87560553716196E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96994541390458E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96994541390458E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88921863363800E-01 0.98890519044724E-01 0.31654257647392E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    400.01200575
 0.41798510277248E-01 0.31018882472618E+03 0.43680145227475E+03 0.43503737920348E+03 0.43433136885866E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15230895191560E+00 0.00000000000000E+00 -.13277173378409E+02
 0.85505130963634E-02 0.11252990383859E+01 0.93561636709293E+03 0.35085613765985E+03 0.71092213954748E+01
 0.26659580233030E+01 0.32474206190361E+03 0.29315000037553E+03 0.32060367245122E+03 0.34073245616587E+03
 0.29315000001999E+03 0.29315000003520E+03 0.31743813470505E+03 0.34069990402090E+03 0.29315000001555E+03
 0.29315000003502E+03 0.32060367245122E+03 0.34073245616587E+03 0.29315000001999E+03 0.29315000003520E+03
 0.31743813470505E+03 0.34069990402090E+03 0.29315000001555E+03 0.29315000003502E+03 0.38295216694738E+03
 0.30066799368882E+03 0.19500354165410E+04 0.18880399514507E+04 0.70100027162265E+03 0.12384467396832E+04
 0.53394146670248E+03 0.11253875394555E+04 0.11171266923009E+04 0.11253875394555E+04 0.18379819794688E+04
 0.10114341707002E+04 0.11168810003313E+04 0.10114341707002E+04 0.18380403303761E+04 0.11253875394555E+04
 0.11171266923009E+04 0.11253875394555E+04 0.18379819794688E+04 0.10114341707002E+04 0.11168810003313E+04
 0.10114341707002E+04 0.18380403303761E+04 0.18468216789634E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48076845406530E+03 0.12948023163662E+01
 0.12948023163662E+01 0.14200972153872E+01 0.15454529477298E+00 0.29709710538651E+03 0.31648959742447E+03
 0.31449158515888E+03 0.31437538672604E+03 0.23000000000000E+00 0.00000000000000E+00 0.20206679663610E+00
 0.00000000000000E+00 -.31574054924159E+01 0.11655858394350E-02 0.52138962153289E+00 0.68635013650114E+04
 0.25738130118793E+04 0.15343611897145E+02 0.57538544614294E+01 0.30066716689663E+03 0.38297162431148E+03
 0.29422781701419E+03 0.29739957443168E+03 0.29315000000040E+03 0.29315000000265E+03 0.29421832059122E+03
 0.29739873317536E+03 0.29315000000040E+03 0.29315000000265E+03 0.29422781701419E+03 0.29739957443168E+03
 0.29315000000040E+03 0.29315000000265E+03 0.29421832059122E+03 0.29739873317536E+03 0.29315000000040E+03
 0.29315000000265E+03 0.29679661287014E+03 0.29315000002831E+03 0.42980130256466E+02 0.33106064883207E+02
 0.74202759098753E+02 0.22908234902052E+03 0.15450857612627E+03 0.61882134569434E+02 0.68665888924638E+02
 0.70021364953716E+02 0.18105043262308E+03 0.61363323796141E+02 0.68564221367290E+02 0.69536708508471E+02
 0.18095473450233E+03 0.61882134569434E+02 0.68665888924638E+02 0.70021364953716E+02 0.18105043262308E+03
 0.61363323796141E+02 0.68564221367290E+02 0.69536708508471E+02 0.18095473450233E+03 0.11379225241675E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33513015000650E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87563589368550E-01 0.00000000000000E+00 0.00000000000000E+00 0.87563589368550E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97027431219959E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97027431219959E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88922107555080E-01 0.98907346318425E-01 0.31648959742447E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    410.01110682
 0.41799104125185E-01 0.31054607039307E+03 0.43720965929606E+03 0.43544485111559E+03 0.43473907233850E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15061014487120E+00 0.00000000000000E+00 -.13298403004101E+02
 0.85503906638907E-02 0.11489214849277E+01 0.93562976412118E+03 0.35086116154544E+03 0.69630519621658E+01
 0.26111444858122E+01 0.32533485867466E+03 0.29315000050742E+03 0.32113494514749E+03 0.34148885761692E+03
 0.29315000002792E+03 0.29315000004944E+03 0.31792872466771E+03 0.34145708408655E+03 0.29315000002173E+03
 0.29315000004919E+03 0.32113494514749E+03 0.34148885761692E+03 0.29315000002792E+03 0.29315000004944E+03
 0.31792872466771E+03 0.34145708408655E+03 0.29315000002173E+03 0.29315000004919E+03 0.38389985449458E+03
 0.30119575944707E+03 0.19562673414616E+04 0.18929893754078E+04 0.70003415736198E+03 0.12308782426922E+04
 0.52734391454342E+03 0.11293799373752E+04 0.11208533039764E+04 0.11293799373752E+04 0.18381320579655E+04
 0.10157857291214E+04 0.11206296618757E+04 0.10157857291214E+04 0.18382047132289E+04 0.11293799373752E+04
 0.11208533039764E+04 0.11293799373752E+04 0.18381320579655E+04 0.10157857291214E+04 0.11206296618757E+04
 0.10157857291214E+04 0.18382047132288E+04 0.18463197658039E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48117556217428E+03 0.12948024727728E+01
 0.12948024727728E+01 0.14600936196411E+01 0.14483781613581E+00 0.29759219657261E+03 0.31644565854959E+03
 0.31462519571003E+03 0.31452163314845E+03 0.23000000000000E+00 0.00000000000000E+00 0.20094317139231E+00
 0.00000000000000E+00 -.31554695431738E+01 0.12437069115152E-02 0.54519143661156E+00 0.64323836475696E+04
 0.24121438678386E+04 0.14673744785357E+02 0.55026542945088E+01 0.30119496471755E+03 0.38391812379656E+03
 0.29429975623060E+03 0.29748889846811E+03 0.29315000000042E+03 0.29315000000361E+03 0.29429002884554E+03
 0.29748792941044E+03 0.29315000000042E+03 0.29315000000361E+03 0.29429975623060E+03 0.29748889846811E+03
 0.29315000000042E+03 0.29315000000361E+03 0.29429002884554E+03 0.29748792941044E+03 0.29315000000042E+03
 0.29315000000361E+03 0.29687355176462E+03 0.29315000003855E+03 0.42236390197374E+02 0.32253333912465E+02
 0.76124933732119E+02 0.22994202090552E+03 0.15343646250474E+03 0.64046725570132E+02 0.70410846332458E+02
 0.73856363802925E+02 0.18175892035361E+03 0.63532889916297E+02 0.70302062302106E+02 0.73379258524996E+02
 0.18165699661628E+03 0.64046725570132E+02 0.70410846332458E+02 0.73856363802925E+02 0.18175892035361E+03
 0.63532889916297E+02 0.70302062302106E+02 0.73379258524996E+02 0.18165699661628E+03 0.11431230904667E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33511780901111E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87568872590161E-01 0.00000000000000E+00 0.00000000000000E+00 0.87568872590161E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97052519156252E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97052519156252E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88922334829437E-01 0.98921334448271E-01 0.31644565854959E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    420.00079253
 0.41800602507048E-01 0.31089854312740E+03 0.43761962040888E+03 0.43585394794864E+03 0.43514830174402E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14893839216582E+00 0.00000000000000E+00 -.13319823724676E+02
 0.85500832121998E-02 0.11721026897951E+01 0.93566340834965E+03 0.35087377813112E+03 0.68253405351355E+01
 0.25595027006758E+01 0.32591943357876E+03 0.29315000067841E+03 0.32165964667149E+03 0.34223405608096E+03
 0.29315000003858E+03 0.29315000006866E+03 0.31841360434768E+03 0.34220302375490E+03 0.29315000003006E+03
 0.29315000006833E+03 0.32165964667149E+03 0.34223405608096E+03 0.29315000003858E+03 0.29315000006866E+03
 0.31841360434768E+03 0.34220302375490E+03 0.29315000003006E+03 0.29315000006833E+03 0.38482644816785E+03
 0.30173512113870E+03 0.19623197618070E+04 0.18977734761117E+04 0.69906331762669E+03 0.12235943967650E+04
 0.52103576255017E+03 0.11332652747924E+04 0.11244626152302E+04 0.11332652747924E+04 0.18382913080632E+04
 0.10200265046505E+04 0.11242593989164E+04 0.10200265046505E+04 0.18383770263955E+04 0.11332652747924E+04
 0.11244626152302E+04 0.11332652747924E+04 0.18382913080632E+04 0.10200265046505E+04 0.11242593989164E+04
 0.10200265046505E+04 0.18383770263955E+04 0.18458505815816E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48158423659198E+03 0.12948026305872E+01
 0.12948026305872E+01 0.15000523624808E+01 0.13548855504495E+00 0.29811760561273E+03 0.31641125740336E+03
 0.31475887043829E+03 0.31466714074632E+03 0.23000000000000E+00 0.00000000000000E+00 0.19981364211586E+00
 0.00000000000000E+00 -.31534901218172E+01 0.13295276423774E-02 0.56904143945800E+00 0.60171746303029E+04
 0.22564404863636E+04 0.14058730077057E+02 0.52720237788964E+01 0.30173436276366E+03 0.38484357909358E+03
 0.29437510096338E+03 0.29757707363865E+03 0.29315000000046E+03 0.29315000000491E+03 0.29436516986752E+03
 0.29757597634832E+03 0.29315000000046E+03 0.29315000000491E+03 0.29437510096338E+03 0.29757707363865E+03
 0.29315000000046E+03 0.29315000000491E+03 0.29436516986752E+03 0.29757597634832E+03 0.29315000000046E+03
 0.29315000000491E+03 0.29694949605808E+03 0.29315000005197E+03 0.41351971926729E+02 0.31329628481356E+02
 0.77966872936382E+02 0.23081784941833E+03 0.15246114211726E+03 0.66199065684029E+02 0.72084003998491E+02
 0.77885971001853E+02 0.18247662341085E+03 0.65691847184689E+02 0.71968505827187E+02 0.77418035155386E+02
 0.18236887632561E+03 0.66199065684029E+02 0.72084003998491E+02 0.77885971001853E+02 0.18247662341085E+03
 0.65691847184689E+02 0.71968505827187E+02 0.77418035155386E+02 0.18236887632561E+03 0.11480738067935E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33511700446198E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87576784348770E-01 0.00000000000000E+00 0.00000000000000E+00 0.87576784348770E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97071559249960E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97071559249960E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88922543683142E-01 0.98932323758842E-01 0.31641125740336E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    430.00440343
 0.41803231004019E-01 0.31124725364663E+03 0.43803169422170E+03 0.43626502780268E+03 0.43555942115318E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14728947441738E+00 0.00000000000000E+00 -.13341462996321E+02
 0.85495446444640E-02 0.11949038010773E+01 0.93572234928092E+03 0.35089588098035E+03 0.66950996329472E+01
 0.25106623623552E+01 0.32649754436643E+03 0.29315000089856E+03 0.32217931961029E+03 0.34297029046219E+03
 0.29315000005281E+03 0.29315000009439E+03 0.31889418301185E+03 0.34293996672378E+03 0.29315000004119E+03
 0.29315000009394E+03 0.32217931961029E+03 0.34297029046219E+03 0.29315000005281E+03 0.29315000009439E+03
 0.31889418301185E+03 0.34293996672378E+03 0.29315000004119E+03 0.29315000009394E+03 0.38573527029399E+03
 0.30228662971274E+03 0.19682180148052E+04 0.19024137597167E+04 0.69808557340378E+03 0.12165558497739E+04
 0.51497984850311E+03 0.11370591814720E+04 0.11279697658581E+04 0.11370591814720E+04 0.18384565678131E+04
 0.10241731368958E+04 0.11277855216083E+04 0.10241731368958E+04 0.18385542295270E+04 0.11370591814720E+04
 0.11279697658581E+04 0.11370591814720E+04 0.18384565678131E+04 0.10241731368958E+04 0.11277855216083E+04
 0.10241731368958E+04 0.18385542295269E+04 0.18454081036657E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48199483666134E+03 0.12948027900119E+01
 0.12948027900119E+01 0.15400668061023E+01 0.12647291212090E+00 0.29867330143797E+03 0.31638663277750E+03
 0.31489312837891E+03 0.31481242530846E+03 0.23000000000000E+00 0.00000000000000E+00 0.19867570414717E+00
 0.00000000000000E+00 -.31514947425446E+01 0.14243031384483E-02 0.59299360851878E+00 0.56167818381102E+04
 0.21062931892913E+04 0.13490870533972E+02 0.50590764502396E+01 0.30228591180039E+03 0.38575131033528E+03
 0.29445424766754E+03 0.29766445843639E+03 0.29315000000051E+03 0.29315000000665E+03 0.29444413983815E+03
 0.29766323282545E+03 0.29315000000051E+03 0.29315000000665E+03 0.29445424766754E+03 0.29766445843639E+03
 0.29315000000051E+03 0.29315000000665E+03 0.29444413983815E+03 0.29766323282545E+03 0.29315000000051E+03
 0.29315000000665E+03 0.29702475542482E+03 0.29315000006941E+03 0.40333760968669E+02 0.30339706296471E+02
 0.79739108395898E+02 0.23171958666799E+03 0.15158178273011E+03 0.68345340459650E+02 0.73695638801421E+02
 0.82130242049187E+02 0.18321206053847E+03 0.67846325798931E+02 0.73573834024539E+02 0.81673033384417E+02
 0.18309889550008E+03 0.68345340459649E+02 0.73695638801421E+02 0.82130242049187E+02 0.18321206053847E+03
 0.67846325798930E+02 0.73573834024539E+02 0.81673033384417E+02 0.18309889550008E+03 0.11528396530310E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33512788253378E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87587481424473E-01 0.00000000000000E+00 0.00000000000000E+00 0.87587481424473E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97085951900003E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97085951900003E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88922733636754E-01 0.98940237042905E-01 0.31638663277750E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    440.01300547
 0.41807098892919E-01 0.31159206294154E+03 0.43844499304974E+03 0.43667720871845E+03 0.43597155493827E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14566461795920E+00 0.00000000000000E+00 -.13363273012911E+02
 0.85487527048978E-02 0.12173102644238E+01 0.93580903275125E+03 0.35092838728172E+03 0.65718660507531E+01
 0.24644497690324E+01 0.32706901310039E+03 0.29315000117937E+03 0.32269377295791E+03 0.34369732867246E+03
 0.29315000007160E+03 0.29315000012848E+03 0.31937027013022E+03 0.34366768288289E+03 0.29315000005591E+03
 0.29315000012787E+03 0.32269377295791E+03 0.34369732867246E+03 0.29315000007160E+03 0.29315000012848E+03
 0.31937027013022E+03 0.34366768288289E+03 0.29315000005591E+03 0.29315000012787E+03 0.38662652408829E+03
 0.30284910729879E+03 0.19739667195613E+04 0.19069152839586E+04 0.69710186984081E+03 0.12097494084664E+04
 0.50916202927638E+03 0.11407640379758E+04 0.11313775672094E+04 0.11407640379758E+04 0.18386248177981E+04
 0.10282278462079E+04 0.11312109339743E+04 0.10282278462079E+04 0.18387333752430E+04 0.11407640379758E+04
 0.11313775672094E+04 0.11407640379758E+04 0.18386248177981E+04 0.10282278462079E+04 0.11312109339743E+04
 0.10282278462079E+04 0.18387333752429E+04 0.18449886157069E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48240648401147E+03 0.12948029506945E+01
 0.12948029506945E+01 0.15801012142740E+01 0.11779682227945E+00 0.29925737447682E+03 0.31637197635094E+03
 0.31502794587404E+03 0.31495743346709E+03 0.23000000000000E+00 0.00000000000000E+00 0.19753046663977E+00
 0.00000000000000E+00 -.31495030929380E+01 0.15292072318329E-02 0.61702545479292E+00 0.52314688509623E+04
 0.19618008191109E+04 0.12965429445184E+02 0.48620360419439E+01 0.30284843363858E+03 0.38664152166857E+03
 0.29453735391305E+03 0.29775111282234E+03 0.29315000000057E+03 0.29315000000895E+03 0.29452709667126E+03
 0.29774975951719E+03 0.29315000000057E+03 0.29315000000895E+03 0.29453735391305E+03 0.29775111282234E+03
 0.29315000000057E+03 0.29315000000895E+03 0.29452709667126E+03 0.29774975951719E+03 0.29315000000057E+03
 0.29315000000895E+03 0.29709938166554E+03 0.29315000009188E+03 0.39190774922298E+02 0.29289511463095E+02
 0.81445620370385E+02 0.23265222826966E+03 0.15079937979742E+03 0.70484342511985E+02 0.75249950715928E+02
 0.86595560317886E+02 0.18396979767422E+03 0.69995046582352E+02 0.75122269076872E+02 0.86150552263441E+02
 0.18385163766036E+03 0.70484342511985E+02 0.75249950715928E+02 0.86595560317886E+02 0.18396979767422E+03
 0.69995046582352E+02 0.75122269076872E+02 0.86150552263441E+02 0.18385163766036E+03 0.11574562235713E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33515044932005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87600915180203E-01 0.00000000000000E+00 0.00000000000000E+00 0.87600915180203E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97096713868844E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97096713868844E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88922903978419E-01 0.98945012089842E-01 0.31637197635094E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    450.14919742
 0.41812938677992E-01 0.31193669061471E+03 0.43886399908209E+03 0.43709493116025E+03 0.43638913146587E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14404412451634E+00 0.00000000000000E+00 -.13385777568474E+02
 0.85475577801962E-02 0.12395953666457E+01 0.93593985624001E+03 0.35097744609000E+03 0.64537188628316E+01
 0.24201445735619E+01 0.32764089409550E+03 0.29315000154011E+03 0.32320930480526E+03 0.34442429817358E+03
 0.29315000009657E+03 0.29315000017392E+03 0.31984766662490E+03 0.34439530942651E+03 0.29315000007552E+03
 0.29315000017311E+03 0.32320930480526E+03 0.34442429817358E+03 0.29315000009657E+03 0.29315000017392E+03
 0.31984766662490E+03 0.34439530942651E+03 0.29315000007552E+03 0.29315000017311E+03 0.38751232263197E+03
 0.30342906709684E+03 0.19796414779840E+04 0.19113352335953E+04 0.69609136710598E+03 0.12030609711666E+04
 0.50348914722513E+03 0.11444284169267E+04 0.11347295629932E+04 0.11444284169267E+04 0.18387926027474E+04
 0.10322435761730E+04 0.11345794764052E+04 0.10322435761730E+04 0.18389112074504E+04 0.11444284169267E+04
 0.11347295629932E+04 0.11444284169267E+04 0.18387926027474E+04 0.10322435761730E+04 0.11345794764052E+04
 0.10322435761730E+04 0.18389112074504E+04 0.18445767755294E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48282360310310E+03 0.12948031164941E+01
 0.12948031164941E+01 0.16206459820743E+01 0.10935785871039E+00 0.29987590471679E+03 0.31636729168044E+03
 0.31516498317010E+03 0.31510392863898E+03 0.23000000000000E+00 0.00000000000000E+00 0.19636384915241E+00
 0.00000000000000E+00 -.31477980935198E+01 0.16472134852003E-02 0.64143203209469E+00 0.48566868058557E+04
 0.18212575521959E+04 0.12472093066314E+02 0.46770348998678E+01 0.30342844155089E+03 0.38752631609577E+03
 0.29462553691091E+03 0.29783819396559E+03 0.29315000000067E+03 0.29315000001202E+03 0.29461515586206E+03
 0.29783671276934E+03 0.29315000000067E+03 0.29315000001202E+03 0.29462553691091E+03 0.29783819396559E+03
 0.29315000000067E+03 0.29315000001202E+03 0.29461515586206E+03 0.29783671276934E+03 0.29315000000067E+03
 0.29315000001202E+03 0.29717436328958E+03 0.29315000012103E+03 0.37913601968320E+02 0.28167526899428E+02
 0.83111233229795E+02 0.23363191387150E+03 0.15010512447555E+03 0.72642971658136E+02 0.76770079001429E+02
 0.91351918356915E+02 0.18476333202584E+03 0.72164973201390E+02 0.76636900230867E+02 0.90920671211829E+02
 0.18464055596795E+03 0.72642971658136E+02 0.76770079001429E+02 0.91351918356915E+02 0.18476333202584E+03
 0.72164973201390E+02 0.76636900230867E+02 0.90920671211829E+02 0.18464055596795E+03 0.11620037913123E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33518506085138E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87622215852643E-01 0.00000000000000E+00 0.00000000000000E+00 0.87622215852643E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97104945875611E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97104945875611E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923051861307E-01 0.98946643455612E-01 0.31636729168044E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    460.12468707
 0.41819449340285E-01 0.31227258953472E+03 0.43927648038920E+03 0.43750606946266E+03 0.43680006830707E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14247362884588E+00 0.00000000000000E+00 -.13407573374978E+02
 0.85462261039917E-02 0.12611340486875E+01 0.93608569474466E+03 0.35103213552925E+03 0.63434969568272E+01
 0.23788113588102E+01 0.32819738706701E+03 0.29315000198665E+03 0.32371169572038E+03 0.34513086935706E+03
 0.29315000012851E+03 0.29315000023223E+03 0.32031321961118E+03 0.34510250073530E+03 0.29315000010065E+03
 0.29315000023115E+03 0.32371169572038E+03 0.34513086935706E+03 0.29315000012851E+03 0.29315000023223E+03
 0.32031321961118E+03 0.34510250073530E+03 0.29315000010065E+03 0.29315000023115E+03 0.38836745049453E+03
 0.30400912161643E+03 0.19850913088364E+04 0.19155632803001E+04 0.69509622617392E+03 0.11966895327258E+04
 0.49811782542102E+03 0.11479537894202E+04 0.11379381572474E+04 0.11479537894202E+04 0.18389566531012E+04
 0.10361119360904E+04 0.11378031854550E+04 0.10361119360904E+04 0.18390842469287E+04 0.11479537894202E+04
 0.11379381572474E+04 0.11479537894202E+04 0.18389566531012E+04 0.10361119360904E+04 0.11378031854549E+04
 0.10361119360904E+04 0.18390842469287E+04 0.18441912176053E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48323411277662E+03 0.12948032770721E+01
 0.12948032770721E+01 0.16605479406742E+01 0.10139061215094E+00 0.30050945593603E+03 0.31637254152908E+03
 0.31530029622316E+03 0.31524770878499E+03 0.23000000000000E+00 0.00000000000000E+00 0.19520917716699E+00
 0.00000000000000E+00 -.31456821771792E+01 0.17766509483402E-02 0.66551762187626E+00 0.45028540960586E+04
 0.16885702860220E+04 0.12020718515982E+02 0.45077694434931E+01 0.30400854634096E+03 0.38838050417869E+03
 0.29471677424992E+03 0.29792336152524E+03 0.29315000000080E+03 0.29315000001595E+03 0.29470629905930E+03
 0.29792175613300E+03 0.29315000000080E+03 0.29315000001595E+03 0.29471677424992E+03 0.29792336152524E+03
 0.29315000000080E+03 0.29315000001595E+03 0.29470629905930E+03 0.29792175613300E+03 0.29315000000080E+03
 0.29315000001595E+03 0.29724769560080E+03 0.29315000015744E+03 0.36547453957208E+02 0.27010135024987E+02
 0.84693993915485E+02 0.23463323271662E+03 0.14951576883156E+03 0.74759604472049E+02 0.78217940570632E+02
 0.96270705683514E+02 0.18557247964034E+03 0.74294116248537E+02 0.78079794910009E+02 0.95854332532724E+02
 0.18544559054458E+03 0.74759604472049E+02 0.78217940570632E+02 0.96270705683514E+02 0.18557247964034E+03
 0.74294116248537E+02 0.78079794910009E+02 0.95854332532724E+02 0.18544559054458E+03 0.11663770507582E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33523038345315E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87637837780282E-01 0.00000000000000E+00 0.00000000000000E+00 0.87637837780282E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97110393590669E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97110393590669E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923184459708E-01 0.98945151156411E-01 0.31637254152908E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    470.00357365
 0.41826019723754E-01 0.31260238652665E+03 0.43968502739548E+03 0.43791324038097E+03 0.43720699870362E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14094184499235E+00 0.00000000000000E+00 -.13429099673579E+02
 0.85448826527712E-02 0.12820848034784E+01 0.93623286885110E+03 0.35108732581916E+03 0.62398368487760E+01
 0.23399388182910E+01 0.32874277739446E+03 0.29315000253533E+03 0.32420477457735E+03 0.34582237280115E+03
 0.29315000016898E+03 0.29315000030632E+03 0.32077047113073E+03 0.34579459469468E+03 0.29315000013257E+03
 0.29315000030492E+03 0.32420477457735E+03 0.34582237280115E+03 0.29315000016898E+03 0.29315000030632E+03
 0.32077047113073E+03 0.34579459469469E+03 0.29315000013257E+03 0.29315000030492E+03 0.38919872566049E+03
 0.30459173072668E+03 0.19903676426783E+04 0.19196420665788E+04 0.69411634058960E+03 0.11905824097519E+04
 0.49299548745939E+03 0.11513723761054E+04 0.11410358971773E+04 0.11513723761054E+04 0.18391215683532E+04
 0.10398676164801E+04 0.11409148324991E+04 0.10398676164801E+04 0.18392572443736E+04 0.11513723761054E+04
 0.11410358971773E+04 0.11513723761054E+04 0.18391215683532E+04 0.10398676164801E+04 0.11409148324991E+04
 0.10398676164801E+04 0.18392572443736E+04 0.18438326573151E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48364063889207E+03 0.12948034356646E+01
 0.12948034356646E+01 0.17000634869601E+01 0.93828611487332E-01 0.30115875319249E+03 0.31638725789799E+03
 0.31543467826363E+03 0.31538966520385E+03 0.23000000000000E+00 0.00000000000000E+00 0.19405929844165E+00
 0.00000000000000E+00 -.31399240135595E+01 0.19198378035119E-02 0.68943484005853E+00 0.41670186853108E+04
 0.15626320069915E+04 0.11603707174591E+02 0.43513901904715E+01 0.30459120933552E+03 0.38921088385170E+03
 0.29481186337615E+03 0.29800731617981E+03 0.29315000000095E+03 0.29315000002095E+03 0.29480132235460E+03
 0.29800558977751E+03 0.29315000000095E+03 0.29315000002095E+03 0.29481186337615E+03 0.29800731617981E+03
 0.29315000000095E+03 0.29315000002095E+03 0.29480132235460E+03 0.29800558977751E+03 0.29315000000095E+03
 0.29315000002095E+03 0.29731998514840E+03 0.29315000020254E+03 0.35096013475670E+02 0.25813702586452E+02
 0.86210267208014E+02 0.23566288147982E+03 0.14902156293576E+03 0.76846180608517E+02 0.79608725744158E+02
 0.10137811593012E+03 0.18640316265422E+03 0.76394376417019E+02 0.79466105625880E+02 0.10097769836318E+03
 0.18627262962542E+03 0.76846180608517E+02 0.79608725744158E+02 0.10137811593012E+03 0.18640316265422E+03
 0.76394376417019E+02 0.79466105625880E+02 0.10097769836318E+03 0.18627262962542E+03 0.11706208276224E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33528606339523E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87652995308886E-01 0.00000000000000E+00 0.00000000000000E+00 0.87652995308886E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97113810271949E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97113810271949E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923351358451E-01 0.98940740167938E-01 0.31638725789799E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    480.01110692
 0.41833914686169E-01 0.31293282092462E+03 0.44009848238585E+03 0.43832520324166E+03 0.43761865291945E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13941360125596E+00 0.00000000000000E+00 -.13451256241339E+02
 0.85432691024207E-02 0.13029311413739E+01 0.93640969330268E+03 0.35115363498850E+03 0.61400021428335E+01
 0.23025008035626E+01 0.32928949964360E+03 0.29315000322131E+03 0.32469970448770E+03 0.34651484818356E+03
 0.29315000022114E+03 0.29315000040204E+03 0.32122972879794E+03 0.34648764536734E+03 0.29315000017377E+03
 0.29315000040023E+03 0.32469970448770E+03 0.34651484818356E+03 0.29315000022114E+03 0.29315000040204E+03
 0.32122972879794E+03 0.34648764536734E+03 0.29315000017377E+03 0.29315000040023E+03 0.39002672927913E+03
 0.30518975303746E+03 0.19955957135160E+04 0.19236650593806E+04 0.69311536977034E+03 0.11845607970593E+04
 0.48797985044008E+03 0.11547653124791E+04 0.11440948416823E+04 0.11547653124791E+04 0.18392856611072E+04
 0.10435995446058E+04 0.11439868717460E+04 0.10435995446058E+04 0.18394287646309E+04 0.11547653124791E+04
 0.11440948416823E+04 0.11547653124791E+04 0.18392856611072E+04 0.10435995446058E+04 0.11439868717459E+04
 0.10435995446058E+04 0.18394287646308E+04 0.18434801950161E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48405190768648E+03 0.12948035989006E+01
 0.12948035989006E+01 0.17400936200736E+01 0.86497140293408E-01 0.30183709274259E+03 0.31641149505424E+03
 0.31557106563995E+03 0.31553293120989E+03 0.23000000000000E+00 0.00000000000000E+00 0.19288807277684E+00
 0.00000000000000E+00 -.31340528962094E+01 0.20825625352229E-02 0.71372827121319E+00 0.38414212609196E+04
 0.14405329728449E+04 0.11208747534130E+02 0.42032803252989E+01 0.30518928803273E+03 0.39003803047648E+03
 0.29491285816159E+03 0.29809201929226E+03 0.29315000000115E+03 0.29315000002740E+03 0.29490227717819E+03
 0.29809017298123E+03 0.29315000000115E+03 0.29315000002741E+03 0.29491285816159E+03 0.29809201929226E+03
 0.29315000000115E+03 0.29315000002740E+03 0.29490227717819E+03 0.29809017298123E+03 0.29315000000115E+03
 0.29315000002741E+03 0.29739291089577E+03 0.29315000025937E+03 0.33530162339525E+02 0.24549791017009E+02
 0.87697831389300E+02 0.23674456576939E+03 0.14860824522314E+03 0.78950319120986E+02 0.80977270003781E+02
 0.10680519484412E+03 0.18727474963117E+03 0.78513601847222E+02 0.80830569766939E+02 0.10642209600801E+03
 0.18714095825113E+03 0.78950319120986E+02 0.80977270003781E+02 0.10680519484412E+03 0.18727474963117E+03
 0.78513601847222E+02 0.80830569766939E+02 0.10642209600801E+03 0.18714095825113E+03 0.11748321312797E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33535299285698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87675534771639E-01 0.00000000000000E+00 0.00000000000000E+00 0.87675534771639E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97115551021903E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97115551021903E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923502472958E-01 0.98933335155766E-01 0.31641149505424E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    490.01165440
 0.41842888364043E-01 0.31325959785897E+03 0.44051091678470E+03 0.43873606254070E+03 0.43802914799731E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13790978531851E+00 0.00000000000000E+00 -.13473325919367E+02
 0.85414359606550E-02 0.13233893776762E+01 0.93661066322465E+03 0.35122899870925E+03 0.60450840356958E+01
 0.22669065133859E+01 0.32983027526295E+03 0.29315000406326E+03 0.32518989888471E+03 0.34719902043925E+03
 0.29315000028708E+03 0.29315000052337E+03 0.32168488362785E+03 0.34717237098697E+03 0.29315000022597E+03
 0.29315000052103E+03 0.32518989888471E+03 0.34719902043925E+03 0.29315000028708E+03 0.29315000052337E+03
 0.32168488362785E+03 0.34717237098697E+03 0.29315000022597E+03 0.29315000052103E+03 0.39084042828872E+03
 0.30579453517433E+03 0.20007082094619E+04 0.19275817066423E+04 0.69210880674592E+03 0.11787016102413E+04
 0.48313225946163E+03 0.11580885649781E+04 0.11470755321278E+04 0.11580885649781E+04 0.18394447524169E+04
 0.10472591202511E+04 0.11469797235282E+04 0.10472591202511E+04 0.18395945732028E+04 0.11580885649781E+04
 0.11470755321278E+04 0.11580885649781E+04 0.18394447524169E+04 0.10472591202511E+04 0.11469797235281E+04
 0.10472591202511E+04 0.18395945732027E+04 0.18431373268720E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48446203856614E+03 0.12948037614965E+01
 0.12948037614965E+01 0.17800958099837E+01 0.79498859168277E-01 0.30253364390826E+03 0.31644476798099E+03
 0.31570748898531E+03 0.31567546217847E+03 0.23000000000000E+00 0.00000000000000E+00 0.19171129663164E+00
 0.00000000000000E+00 -.31285900295775E+01 0.22658902840595E-02 0.73807000427853E+00 0.35306210791758E+04
 0.13239829046909E+04 0.10839080241203E+02 0.40646550904511E+01 0.30579412844914E+03 0.39085091816530E+03
 0.29501876684839E+03 0.29817640102014E+03 0.29315000000142E+03 0.29315000003559E+03 0.29500817279675E+03
 0.29817443789405E+03 0.29315000000142E+03 0.29315000003560E+03 0.29501876684839E+03 0.29817640102014E+03
 0.29315000000142E+03 0.29315000003559E+03 0.29500817279675E+03 0.29817443789405E+03 0.29315000000142E+03
 0.29315000003560E+03 0.29746555099842E+03 0.29315000032967E+03 0.31875266606577E+02 0.23234412202303E+02
 0.89139266351062E+02 0.23786418163101E+03 0.14827921894820E+03 0.81042401391011E+02 0.82307602786846E+02
 0.11249354816555E+03 0.18817604562103E+03 0.80621926238824E+02 0.82157271144369E+02 0.11212884741526E+03
 0.18803942246874E+03 0.81042401391011E+02 0.82307602786846E+02 0.11249354816555E+03 0.18817604562103E+03
 0.80621926238824E+02 0.82157271144368E+02 0.11212884741526E+03 0.18803942246874E+03 0.11789529643398E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33543001932789E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87698262516097E-01 0.00000000000000E+00 0.00000000000000E+00 0.87698262516097E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97115532510348E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97115532510348E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923629427438E-01 0.98923079269329E-01 0.31644476798099E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    500.00000000
 0.41852416170774E-01 0.31358292006276E+03 0.44092191641044E+03 0.43914543485380E+03 0.43843811650342E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13643082615710E+00 0.00000000000000E+00 -.13495285511155E+02
 0.85394905455340E-02 0.13434558263199E+01 0.93682403620481E+03 0.35130901357680E+03 0.59547919948467E+01
 0.22330469980675E+01 0.33036505905483E+03 0.29315000508941E+03 0.32567529352170E+03 0.34787481445230E+03
 0.29315000036979E+03 0.29315000067590E+03 0.32213586800947E+03 0.34784869755962E+03 0.29315000029159E+03
 0.29315000067293E+03 0.32567529352170E+03 0.34787481445230E+03 0.29315000036979E+03 0.29315000067590E+03
 0.32213586800947E+03 0.34784869755963E+03 0.29315000029159E+03 0.29315000067293E+03 0.39164002568221E+03
 0.30640505924579E+03 0.20057095884162E+04 0.19313981172204E+04 0.69109840739888E+03 0.11729986463184E+04
 0.47844474688252E+03 0.11613444886855E+04 0.11499810269795E+04 0.11613444886855E+04 0.18395988219695E+04
 0.10508486342492E+04 0.11498965112923E+04 0.10508486342492E+04 0.18397547030829E+04 0.11613444886855E+04
 0.11499810269795E+04 0.11613444886855E+04 0.18395988219695E+04 0.10508486342492E+04 0.11498965112923E+04
 0.10508486342492E+04 0.18397547030828E+04 0.18428041279741E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48487066000302E+03 0.12948039232814E+01
 0.12948039232814E+01 0.18200491923793E+01 0.72833434600500E-01 0.30324599122392E+03 0.31648666544453E+03
 0.31584375625792E+03 0.31581710435040E+03 0.23000000000000E+00 0.00000000000000E+00 0.19052966130778E+00
 0.00000000000000E+00 -.31235400050787E+01 0.24732553502435E-02 0.76244696867135E+00 0.32346033333001E+04
 0.12129762499875E+04 0.10492533026842E+02 0.39346998850659E+01 0.30640471271837E+03 0.39164974607140E+03
 0.29512978376641E+03 0.29826048719145E+03 0.29315000000177E+03 0.29315000004590E+03 0.29511920323134E+03
 0.29825841070419E+03 0.29315000000176E+03 0.29315000004591E+03 0.29512978376641E+03 0.29826048719145E+03
 0.29315000000177E+03 0.29315000004590E+03 0.29511920323134E+03 0.29825841070419E+03 0.29315000000176E+03
 0.29315000004591E+03 0.29753792780079E+03 0.29315000041604E+03 0.30137828923176E+02 0.21867758305875E+02
 0.90536948833573E+02 0.23902003159386E+03 0.14803039801611E+03 0.83119585169308E+02 0.83601896815412E+02
 0.11845180718823E+03 0.18910588323558E+03 0.82716420128266E+02 0.83448376468913E+02 0.11810649385657E+03
 0.18896684668851E+03 0.83119585169308E+02 0.83601896815412E+02 0.11845180718823E+03 0.18910588323558E+03
 0.82716420128266E+02 0.83448376468913E+02 0.11810649385657E+03 0.18896684668851E+03 0.11829775244781E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33551661641057E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.87721087846843E-01 0.00000000000000E+00 0.00000000000000E+00 0.87721087846843E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97113650767621E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97113650767621E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.88923732841268E-01 0.98910103491439E-01 0.31648666544453E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
