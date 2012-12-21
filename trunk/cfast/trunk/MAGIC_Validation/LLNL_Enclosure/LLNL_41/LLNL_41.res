#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-41 0 MONOZONE(1=OUI,0=NON)                                                                     
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
150kW                                                                                               
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
1.000000 0.000300                                                                                   
4.000000 0.001200                                                                                   
8.000000 0.002400                                                                                   
10.000000 0.003000                                                                                  
53.300000 0.003000                                                                                  
63.900000 0.003000                                                                                  
74.600000 0.003000                                                                                  
100.000000 0.003000                                                                                 
500.000000 0.003000                                                                                 
501.000000 0.000000                                                                                 
550.000000 0.000000                                                                                 
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL41 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.098000                                                                                   
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
#ROOM#LOC_1 #LLNL-41           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-41           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-41           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-41           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-41           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-41           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-41           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-41           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-41           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-41           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-41           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-41           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-41           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-41           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-41           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-41           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-41           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-41           #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_1 #150kW       #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #150kW       #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #150kW       #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #150kW       #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #150kW       #TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_1 #150kW       #HEIGHT#m#Flame height
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #150kW       #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #150kW       #MASS#kg#Consumed combustible mass
#ROOM#LOC_2 #Plenum-LLNL41     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL41     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL41     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL41     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL41     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL41     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL41     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL41     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL41     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL41     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL41     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL41     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL41     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL41     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL41     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL41     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL41     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL41     #HEAT_POWER#W#Total sprinkling power
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.40570198272641E-12
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
 0.00000000000000E+00 -.34315777975801E-07 0.99964886840304E-03 0.00000000000000E+00 0.80000000000000E+04
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
 0.00000000000000E+00 0.00000000000000E+00 0.26591821922804E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.26591821922804E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97999846642652E-01 0.11768667097701E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     20.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.46037045173841E+01
 0.99995456496899E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000205120E+03 0.29315000000000E+03 0.29315000282130E+03 0.29315000282130E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000280672E+03 0.29315000280672E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000282130E+03 0.29315000282130E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000280672E+03 0.29315000280672E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000980315E+03
 0.29315000809984E+03 0.47332611445699E-03 0.47332611445699E-03 0.61248516959396E-03 0.61554759544193E-03
 .00000000000000E+00 0.44218948568396E-03 0.52650822850611E-03 0.44218948568396E-03 0.52650822850611E-03
 0.43989459208143E-03 0.52657671693840E-03 0.43989459208143E-03 0.52657671693840E-03 0.44218948568396E-03
 0.52650822856361E-03 0.44218948568396E-03 0.52650822856361E-03 0.43989459173645E-03 0.52657671665092E-03
 0.43989459173645E-03 0.52657671665092E-03 0.52609260728011E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.50882167444683E+01 0.99961735140224E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000807696E+03 0.29315000983093E+03
 0.29315000302959E+03 0.29315000302959E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000300302E+03
 0.29315000300302E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000302959E+03 0.29315000302959E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000300302E+03 0.29315000300302E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000291959E+03 0.29315000000000E+03 0.50796917470333E-03 0.50796917470333E-03
 0.66900355952607E-03 0.67234857732370E-03 .00000000000000E+00 0.47481547807744E-03 0.52128249275333E-03
 0.47481547807744E-03 0.52128249275333E-03 0.47064337047881E-03 0.52141192537605E-03 0.47064337047881E-03
 0.52141192537605E-03 0.47481547807744E-03 0.52128249275333E-03 0.47481547807744E-03 0.52128249275333E-03
 0.47064337042131E-03 0.52141192531855E-03 0.47064337042131E-03 0.52141192531855E-03 0.42522025099444E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11758110473381E+00 0.00000000000000E+00 0.00000000000000E+00 0.11758110473381E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153852E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153852E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97916681752985E-01 0.11758089249603E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     30.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.46037045167997E+01
 0.99995456496899E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000254331E+03 0.29315000000000E+03 0.29315000348890E+03 0.29315000348890E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000347080E+03 0.29315000347080E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000348890E+03 0.29315000348890E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000347080E+03 0.29315000347080E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001195325E+03
 0.29315000988300E+03 0.47666598729834E-03 0.47666598729834E-03 0.60480931354965E-03 0.60783336011740E-03
 .00000000000000E+00 0.44392645484950E-03 0.52988857650725E-03 0.44392645484950E-03 0.52988857650725E-03
 0.44161278248804E-03 0.52997508354190E-03 0.44161278248804E-03 0.52997508354190E-03 0.44392645479200E-03
 0.52988857656474E-03 0.44392645479200E-03 0.52988857656474E-03 0.44161278248804E-03 0.52997508348441E-03
 0.44161278248804E-03 0.52997508348441E-03 0.52608043491237E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.50882167438606E+01 0.99962628135373E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315000986501E+03 0.29315001197501E+03
 0.29315000374637E+03 0.29315000374637E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000371346E+03
 0.29315000371346E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000374637E+03 0.29315000374637E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000371346E+03 0.29315000371346E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000361226E+03 0.29315000000000E+03 0.50263249030401E-03 0.50263249030401E-03
 0.67199643104099E-03 0.67535641319620E-03 .00000000000000E+00 0.47666756775241E-03 0.52281278345328E-03
 0.47666756775241E-03 0.52281278345328E-03 0.47247355843417E-03 0.52297474233604E-03 0.47247355843417E-03
 0.52297474233604E-03 0.47666756780991E-03 0.52281278351078E-03 0.47666756780991E-03 0.52281278351078E-03
 0.47247355837667E-03 0.52297474227854E-03 0.47247355837667E-03 0.52297474227854E-03 0.42521432604931E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11758110472637E+00 0.00000000000000E+00 0.00000000000000E+00 0.11758110472637E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153498E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153498E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97916681752995E-01 0.11758089249604E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00000000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.46037045162813E+01
 0.99995456496900E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296976E+03 0.29315000000000E+03 0.29315000406498E+03 0.29315000406498E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000404383E+03 0.29315000404383E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000406498E+03 0.29315000406498E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000404383E+03 0.29315000404383E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001376328E+03
 0.29315001138817E+03 0.47946259060798E-03 0.47946259060798E-03 0.59841313476418E-03 0.60140520043800E-03
 .00000000000000E+00 0.44536637546370E-03 0.53269823046472E-03 0.44536637546370E-03 0.53269823046472E-03
 0.44303787795085E-03 0.53280034442587E-03 0.44303787795085E-03 0.53280034442587E-03 0.44536637540620E-03
 0.53269823046472E-03 0.44536637540620E-03 0.53269823046472E-03 0.44303787795085E-03 0.53280034442587E-03
 0.44303787795085E-03 0.53280034442587E-03 0.52606925571582E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.50882167433116E+01 0.99963497482604E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001137456E+03 0.29315001377968E+03
 0.29315000436487E+03 0.29315000436487E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000432650E+03
 0.29315000432650E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000436487E+03 0.29315000436487E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000432650E+03 0.29315000432650E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000421067E+03 0.29315000000000E+03 0.49819239161407E-03 0.49819239161407E-03
 0.67448392904876E-03 0.67785634869400E-03 .00000000000000E+00 0.47820643963724E-03 0.52408437863643E-03
 0.47820643963724E-03 0.52408437863643E-03 0.47399534982376E-03 0.52427441892462E-03 0.47399534982376E-03
 0.52427441892462E-03 0.47820643957974E-03 0.52408437863643E-03 0.47820643957974E-03 0.52408437863643E-03
 0.47399534982376E-03 0.52427441886713E-03 0.47399534982376E-03 0.52427441886713E-03 0.42520829878232E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11758110471968E+00 0.00000000000000E+00 0.00000000000000E+00 0.11758110471968E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153319E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098153319E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97916681753003E-01 0.11758089249605E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     40.00025000
 0.30000000000000E+01 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.46037045162791E+01
 0.99995456496900E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29315000296977E+03 0.29315000000000E+03 0.29315000406499E+03 0.29315000406499E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000404384E+03 0.29315000404384E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000406499E+03 0.29315000406499E+03 0.29315000000000E+03 0.29315000000000E+03
 0.29315000404384E+03 0.29315000404384E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315001376332E+03
 0.29315001138821E+03 0.47946801329954E-03 0.47946801329954E-03 0.59842065028287E-03 0.60141275353428E-03
 .00000000000000E+00 0.44537227750063E-03 0.53270254325067E-03 0.44537227750063E-03 0.53270254325067E-03
 0.44304382644467E-03 0.53280465738431E-03 0.44304382644467E-03 0.53280465738431E-03 0.44537227738564E-03
 0.53270254325067E-03 0.44537227738564E-03 0.53270254325067E-03 0.44304382638718E-03 0.53280465732682E-03
 0.44304382638718E-03 0.53280465732682E-03 0.52607592225947E-04 0.00000000000000E+00 0.75000000000358E-07
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.18750000000179E-10 0.15000000000000E+01 0.29315000000000E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.50882167432708E+01 0.99963497504337E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315001137459E+03 0.29315001377972E+03
 0.29315000436489E+03 0.29315000436489E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000432652E+03
 0.29315000432652E+03 0.29315000000000E+03 0.29315000000000E+03 0.29315000436489E+03 0.29315000436489E+03
 0.29315000000000E+03 0.29315000000000E+03 0.29315000432652E+03 0.29315000432652E+03 0.29315000000000E+03
 0.29315000000000E+03 0.29315000421068E+03 0.29315000000000E+03 0.49819940367161E-03 0.49819940367161E-03
 0.67449832007678E-03 0.67787081167717E-03 .00000000000000E+00 0.47821632563967E-03 0.52409233035349E-03
 0.47821632563967E-03 0.52409233035349E-03 0.47400540015017E-03 0.52428237110166E-03 0.47400540015017E-03
 0.52428237110166E-03 0.47821632569716E-03 0.52409233029600E-03 0.47821632569716E-03 0.52409233029600E-03
 0.47400540009267E-03 0.52428237110166E-03 0.47400540009267E-03 0.52428237110166E-03 0.42521643831859E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11758110471968E+00 0.00000000000000E+00 0.00000000000000E+00 0.11758110471968E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098152791E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11758098152791E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97916681753004E-01 0.11758089249605E+00 0.29315000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     50.00790832
 0.22802929065205E+01 0.29317563907444E+03 0.32101476727864E+03 0.29985431175606E+03 0.29940471732939E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22684685909606E+00 0.00000000000000E+00 0.19508116959418E+02
 0.10001050603581E-02 0.54932527239843E-01 0.79991596054277E+04 0.29996848520354E+04 0.14563320498747E+03
 0.54612451870302E+02 0.29418137610001E+03 0.29315000000001E+03 0.29399044699233E+03 0.29432513893376E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29379505749985E+03 0.29431661267427E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29399044699233E+03 0.29432513893376E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29379505749985E+03 0.29431661267427E+03 0.29315000000002E+03 0.29315000000002E+03 0.29631867827767E+03
 0.29315292726282E+03 0.47577684409879E+03 0.47393340829903E+03 0.20630647212371E+03 0.43231975399017E+03
 0.22498174950585E+03 0.28500461934236E+03 0.13599652684522E+03 0.28386309456538E+03 0.37281641444062E+03
 0.21603430576967E+03 0.13267747104957E+03 0.21523675254413E+03 0.36958486733441E+03 0.28500461934236E+03
 0.13599652684522E+03 0.28386309456538E+03 0.37281641444062E+03 0.21603430576967E+03 0.13267747104957E+03
 0.21523675254413E+03 0.36958486733441E+03 0.38791756848625E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.34110498554115E+03 0.10917549188161E+01
 0.10917549188161E+01 0.15053534319877E-01 0.13691263198005E+01 0.29316590534394E+03 0.29678674219030E+03
 0.29348182017293E+03 0.29347829849802E+03 0.22999999998969E+00 0.00000000000000E+00 0.22952079413201E+00
 0.00000000000000E+00 0.19280018049424E+02 0.99982123302149E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315292030519E+03 0.29632660031801E+03
 0.29315149621908E+03 0.29325886779431E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315151151493E+03
 0.29325887076884E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315149621908E+03 0.29325886779431E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315151151493E+03 0.29325887076884E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29318858219239E+03 0.29315000000001E+03 0.40444077138002E+00 0.40739414563733E+00
 0.44608777245796E+00 0.18487388206199E+02 0.18039069994879E+02 0.50254074230350E+00 -.10791980396289E+00
 0.50905679226612E+00 0.29745317554144E+02 0.50743699221261E+00 -.10689061869049E+00 0.51394420755401E+00
 0.29746319118165E+02 0.50254074230350E+00 -.10791980396289E+00 0.50905679226612E+00 0.29745317554144E+02
 0.50743699221267E+00 -.10689061869038E+00 0.51394420755407E+00 0.29746319118165E+02 0.54532620157384E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30043603387204E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23446726245518E+00 0.00000000000000E+00 0.00000000000000E+00 0.23446726245518E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.11084374457238E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11084374457238E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98314308986305E-01 0.11808037088888E+00 0.29316590534394E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     60.02334127
 0.17447580806421E+01 0.29320769172895E+03 0.33985538863882E+03 0.31272573992987E+03 0.31107273940720E+03
 0.22999999995607E+00 0.00000000000000E+00 0.22446884878852E+00 0.00000000000000E+00 0.12610851299669E+02
 0.99992767431097E-03 0.99745359126089E-01 0.80000000000000E+04 0.30000000000000E+04 0.80204232759212E+02
 0.30076587284704E+02 0.29504530389701E+03 0.29315000000002E+03 0.29486080210676E+03 0.29581270305279E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29444393998734E+03 0.29579284443557E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29486080210676E+03 0.29581270305279E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29444393998734E+03 0.29579284443557E+03 0.29315000000002E+03 0.29315000000002E+03 0.30050162906827E+03
 0.29315747272233E+03 0.49890676928625E+03 0.49479152749414E+03 0.21378543610887E+03 0.59900296628911E+03
 0.38414860299970E+03 0.32273967607647E+03 0.17482681067410E+03 0.31980218218215E+03 0.53336739144768E+03
 0.24251562696175E+03 0.17079116413418E+03 0.24050639927074E+03 0.52952518516890E+03 0.32273967607647E+03
 0.17482681067410E+03 0.31980218218215E+03 0.53336739144768E+03 0.24251562696175E+03 0.17079116413418E+03
 0.24050639927074E+03 0.52952518516890E+03 0.49655319994422E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.36051386260874E+03 0.10918001916725E+01
 0.10918001916725E+01 0.45099833173303E-01 0.12291074914128E+01 0.29316116739891E+03 0.29925071840504E+03
 0.29426090989772E+03 0.29424250477777E+03 0.23000000000000E+00 0.00000000000000E+00 0.22914693585212E+00
 0.00000000000000E+00 0.13570681547256E+02 0.99978104503200E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29315744583328E+03 0.30052923187910E+03
 0.29315351078042E+03 0.29336585808591E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315352976061E+03
 0.29336587821377E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315351078042E+03 0.29336585808591E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315352976061E+03 0.29336587821377E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29325620299083E+03 0.29315000000002E+03 0.65584653667490E+00 0.65646586834971E+00
 0.29322205570928E+00 0.34557868922314E+02 0.34263180756326E+02 0.72626137507744E+00 -.38615077065773E+00
 0.72923072936976E+00 0.41149338160945E+02 0.72827145520628E+00 -.38021147870643E+00 0.73123146738908E+00
 0.41155117190931E+02 0.72626137507744E+00 -.38615077065779E+00 0.72923072936976E+00 0.41149338160945E+02
 0.72827145520623E+00 -.38021147870643E+00 0.73123146738902E+00 0.41155117190931E+02 0.10860432397913E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30498870866286E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.19264114840590E+00 0.00000000000000E+00 0.00000000000000E+00 0.19264114840590E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10468614088590E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10468614088590E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98221293119735E-01 0.11796391243994E+00 0.29316116739891E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     70.00213177
 0.14032423578021E+01 0.29326624603259E+03 0.35207781085782E+03 0.32456885122736E+03 0.32188441026238E+03
 0.22999999978603E+00 0.00000000000000E+00 0.22265689366940E+00 0.00000000000000E+00 0.71079326080847E+01
 0.99967373808584E-03 0.12982483293944E+00 0.80000000000000E+04 0.30000000000000E+04 0.61621492736540E+02
 0.23108059776203E+02 0.29568494515869E+03 0.29315000000002E+03 0.29552782013286E+03 0.29722334710437E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29494730338475E+03 0.29719634687112E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29552782013286E+03 0.29722334710437E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29494730338475E+03 0.29719634687112E+03 0.29315000000002E+03 0.29315000000002E+03 0.30424375984345E+03
 0.29316319079590E+03 0.53015831907188E+03 0.52422522072908E+03 0.23023534875051E+03 0.71290456699816E+03
 0.48151804150390E+03 0.35362717919851E+03 0.21643555325772E+03 0.34912859585472E+03 0.65420838775145E+03
 0.26705234552392E+03 0.21232651069278E+03 0.26399814547301E+03 0.65036077721160E+03 0.35362717919851E+03
 0.21643555325772E+03 0.34912859585472E+03 0.65420838775145E+03 0.26705234552392E+03 0.21232651069278E+03
 0.26399814547301E+03 0.65036077721160E+03 0.59376781596977E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.37385987698405E+03 0.10918363152903E+01
 0.10918363152903E+01 0.75036204679321E-01 0.11018863255524E+01 0.29315753177996E+03 0.30108243299197E+03
 0.29526087280739E+03 0.29521991768140E+03 0.23000000000000E+00 0.00000000000000E+00 0.22881958562595E+00
 0.00000000000000E+00 0.91237425154716E+01 0.99974955556151E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29316315482142E+03 0.30427238012239E+03
 0.29315583930409E+03 0.29346782484663E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315585026923E+03
 0.29346787422185E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315583930409E+03 0.29346782484663E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315585026923E+03 0.29346787422185E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29333315580675E+03 0.29315000000002E+03 0.97441469180654E+00 0.97233972323648E+00
 0.18164448615045E+00 0.47585270522842E+02 0.47402717814260E+02 0.10175394328776E+01 -.57079764563435E+00
 0.10179925271089E+01 0.49098242922765E+02 0.10167147386337E+01 -.55983035538969E+00 0.10171641598001E+01
 0.49108840952734E+02 0.10175394328775E+01 -.57079764563441E+00 0.10179925271088E+01 0.49098242922765E+02
 0.10167147386337E+01 -.55983035538957E+00 0.10171641598001E+01 0.49108840952734E+02 0.15787769764190E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30854319129884E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.15710479326360E+00 0.00000000000000E+00 0.00000000000000E+00 0.15710479326360E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10238530006806E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10238530006806E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98148782939295E-01 0.11787311491852E+00 0.29315753177996E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     80.00105320
 0.11329700007090E+01 0.29335495746627E+03 0.36089010470191E+03 0.33538500609810E+03 0.33202309069267E+03
 0.22999999981806E+00 0.00000000000000E+00 0.22111275211445E+00 0.00000000000000E+00 0.30161715676733E+01
 0.99933107937130E-03 0.15422842695674E+00 0.80000000000000E+04 0.30000000000000E+04 0.51871111946462E+02
 0.19451666979923E+02 0.29627929628123E+03 0.29315000000002E+03 0.29614127038913E+03 0.29861828259789E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29542185303848E+03 0.29858619635186E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29614127038913E+03 0.29861828259789E+03 0.29315000000002E+03 0.29315000000002E+03
 0.29542185303848E+03 0.29858619635186E+03 0.29315000000002E+03 0.29315000000002E+03 0.30774403465242E+03
 0.29317033056528E+03 0.56662092484557E+03 0.55898290167312E+03 0.24935630517797E+03 0.79273233194274E+03
 0.54212924523889E+03 0.38141454041461E+03 0.25766671813189E+03 0.37540482842876E+03 0.74961251970583E+03
 0.29121947988756E+03 0.25366307560994E+03 0.28714766227011E+03 0.74591887123792E+03 0.38141454041461E+03
 0.25766671813189E+03 0.37540482842876E+03 0.74961251970583E+03 0.29121947988756E+03 0.25366307560994E+03
 0.28714766227011E+03 0.74591887123792E+03 0.68083484031287E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.38199271518236E+03 0.10918631772188E+01
 0.10918631772188E+01 0.10503296896202E+00 0.98777438077496E+00 0.29315504207500E+03 0.30263512517024E+03
 0.29639233636418E+03 0.29632483505834E+03 0.23000000000000E+00 0.00000000000000E+00 0.22849963876730E+00
 0.00000000000000E+00 0.59754871453209E+01 0.99972697476770E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29317028634875E+03 0.30776904851679E+03
 0.29315867985968E+03 0.29356886768779E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315867637753E+03
 0.29356895280787E+03 0.29315000000002E+03 0.29315000000002E+03 0.29315867985968E+03 0.29356886768779E+03
 0.29315000000002E+03 0.29315000000002E+03 0.29315867637753E+03 0.29356895280787E+03 0.29315000000002E+03
 0.29315000000002E+03 0.29341646857481E+03 0.29315000000002E+03 0.13305610044777E+01 0.13233427613904E+01
 0.84478103685225E-01 0.59125552673788E+02 0.59040652179584E+02 0.13446575902708E+01 -.71306361748273E+00
 0.13434433299244E+01 0.55939960936963E+02 0.13409394486474E+01 -.69745975963160E+00 0.13397266487947E+01
 0.55954948113990E+02 0.13446575902708E+01 -.71306361748267E+00 0.13434433299244E+01 0.55939960936963E+02
 0.13409394486475E+01 -.69745975963160E+00 0.13397266487948E+01 0.55954948113990E+02 0.20504235357070E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31159474075396E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12797657839618E+00 0.00000000000000E+00 0.00000000000000E+00 0.12797657839618E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10113750864505E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10113750864505E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98097416095909E-01 0.11780876426785E+00 0.29315504207500E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
     90.00158212
 0.91089988934532E+00 0.29347994631174E+03 0.36777902018876E+03 0.34521934746442E+03 0.34152606381688E+03
 0.22999999982911E+00 0.00000000000000E+00 0.21969264987947E+00 0.00000000000000E+00 -.16010843822715E+00
 0.99887416657895E-03 0.17607488061434E+00 0.80000000000000E+04 0.30000000000000E+04 0.45435214677343E+02
 0.17038205504004E+02 0.29689668220111E+03 0.29315000000003E+03 0.29672840530873E+03 0.29997293133560E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29588907459965E+03 0.29993752610365E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29672840530873E+03 0.29997293133560E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29588907459965E+03 0.29993752610365E+03 0.29315000000003E+03 0.29315000000003E+03 0.31099310489304E+03
 0.29317875895481E+03 0.62489052747353E+03 0.61549695980779E+03 0.26363661234121E+03 0.84970417514733E+03
 0.58474937974441E+03 0.40826854178767E+03 0.29339966475706E+03 0.40078852317641E+03 0.82574718582000E+03
 0.31612824570075E+03 0.28970551176171E+03 0.31105815571736E+03 0.82239474971983E+03 0.40826854178767E+03
 0.29339966475706E+03 0.40078852317641E+03 0.82574718581999E+03 0.31612824570075E+03 0.28970551176171E+03
 0.31105815571736E+03 0.82239474971983E+03 0.76257850761218E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.38908306799234E+03 0.16095643417605E+01
 0.16095643417605E+01 0.13503455571145E+00 0.89320558295341E+00 0.29315331858183E+03 0.30400900158050E+03
 0.29754476380637E+03 0.29745001853694E+03 0.23000000000000E+00 0.00000000000000E+00 0.22817593855807E+00
 0.00000000000000E+00 0.36390036329083E+01 0.99970979243912E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29317870664206E+03 0.31101471280855E+03
 0.29316197068856E+03 0.29367028328875E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316194715554E+03
 0.29367040848362E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316197068856E+03 0.29367028328875E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29316194715554E+03 0.29367040848362E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29350419832582E+03 0.29315000000003E+03 0.17020546508084E+01 0.16883982817038E+01
 -.24538759565963E-01 0.69589263616311E+02 0.69613925069674E+02 0.16802312206285E+01 -.84435547922046E+00
 0.16765337233945E+01 0.62334107362590E+02 0.16733762000831E+01 -.82437462196918E+00 0.16696913911311E+01
 0.62353194236538E+02 0.16802312206286E+01 -.84435547922052E+00 0.16765337233945E+01 0.62334107362590E+02
 0.16733762000831E+01 -.82437462196918E+00 0.16696913911310E+01 0.62353194236538E+02 0.24971371995647E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31424196626376E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10227467341753E+00 0.00000000000000E+00 0.00000000000000E+00 0.10227467341753E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10041063600360E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10041063600360E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98059276518532E-01 0.11599030335175E+00 0.29762840946237E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    100.01720908
 0.72893434513704E+00 0.29365191626698E+03 0.37355470553799E+03 0.35414007641403E+03 0.35038894039220E+03
 0.22999999983902E+00 0.00000000000000E+00 0.21830381833759E+00 0.00000000000000E+00 -.26149718043088E+01
 0.99826501451168E-03 0.19707492461095E+00 0.80000000000000E+04 0.30000000000000E+04 0.40593698136854E+02
 0.15222636801320E+02 0.29755532993198E+03 0.29315000000003E+03 0.29730767141682E+03 0.30129852919672E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29636179151121E+03 0.30126111141348E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29730767141682E+03 0.30129852919672E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29636179151121E+03 0.30126111141348E+03 0.29315000000003E+03 0.29315000000003E+03 0.31406003034941E+03
 0.29318841973800E+03 0.68658963661697E+03 0.67537893551162E+03 0.27963092439490E+03 0.89820642440785E+03
 0.61717734539097E+03 0.43547190047417E+03 0.32866208035133E+03 0.42656594712141E+03 0.89287971462774E+03
 0.34202720580341E+03 0.32528467598766E+03 0.33598930744512E+03 0.88986293419412E+03 0.43547190047417E+03
 0.32866208035133E+03 0.42656594712141E+03 0.89287971462774E+03 0.34202720580341E+03 0.32528467598766E+03
 0.33598930744512E+03 0.88986293419412E+03 0.83957583261154E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.39592166011989E+03 0.14618021590668E+01
 0.14618021590668E+01 0.16508143660631E+00 0.81938907856031E+00 0.29315223181761E+03 0.30526821115245E+03
 0.29864974372376E+03 0.29852836727309E+03 0.23000000000000E+00 0.00000000000000E+00 0.22783896675975E+00
 0.00000000000000E+00 0.19288559085238E+01 0.99969662019946E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29318835983383E+03 0.31407973397738E+03
 0.29316567430060E+03 0.29377388880637E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316562590545E+03
 0.29377405741774E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316567430060E+03 0.29377388880637E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29316562590545E+03 0.29377405741774E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29359548443343E+03 0.29315000000003E+03 0.20887033430064E+01 0.20674780635434E+01
 -.13441876606855E+00 0.79325912941678E+02 0.79461003801577E+02 0.20298805932409E+01 -.97127920811556E+00
 0.20233116753991E+01 0.68579009277054E+02 0.20198894595376E+01 -.94717673175428E+00 0.20133504148219E+01
 0.68601912840642E+02 0.20298805932409E+01 -.97127920811550E+00 0.20233116753991E+01 0.68579009277054E+02
 0.20198894595376E+01 -.94717673175428E+00 0.20133504148218E+01 0.68601912840642E+02 0.29201708379609E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31655020245444E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.79066303158253E-01 0.00000000000000E+00 0.00000000000000E+00 0.79066303158253E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99951900733250E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99951900733250E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98031351265200E-01 0.11535447902743E+00 0.29917863581538E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    110.00010502
 0.58098072681102E+00 0.29388325391333E+03 0.37856969158953E+03 0.36216929555214E+03 0.35855995897614E+03
 0.22999999980665E+00 0.00000000000000E+00 0.21691049796241E+00 0.00000000000000E+00 -.44616494887448E+01
 0.99746102510429E-03 0.21788713922915E+00 0.80000000000000E+04 0.30000000000000E+04 0.36716256077814E+02
 0.13768596029180E+02 0.29823651077803E+03 0.29315000000004E+03 0.29788437597648E+03 0.30259395432321E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29684301636615E+03 0.30255529422056E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29788437597648E+03 0.30259395432321E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29684301636615E+03 0.30255529422056E+03 0.29315000000003E+03 0.29315000000003E+03 0.31699972822337E+03
 0.29319942102572E+03 0.74737793735193E+03 0.73442307515351E+03 0.29767548838467E+03 0.94157433221746E+03
 0.64241046639087E+03 0.46285895365144E+03 0.36424732275767E+03 0.45261217045528E+03 0.95303453956893E+03
 0.36861046640242E+03 0.36115323087697E+03 0.36167544274017E+03 0.95031161071320E+03 0.46285895365144E+03
 0.36424732275767E+03 0.45261217045528E+03 0.95303453956893E+03 0.36861046640242E+03 0.36115323087697E+03
 0.36167544274017E+03 0.95031161071320E+03 0.91115940688290E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.40326891543732E+03 0.13436384931679E+01
 0.13436384931679E+01 0.19503012442209E+00 0.75403208629239E+00 0.29315171855550E+03 0.30643621693393E+03
 0.29975825824884E+03 0.29961111404507E+03 0.23000000000000E+00 0.00000000000000E+00 0.22748571952564E+00
 0.00000000000000E+00 0.73683980785188E+00 0.99968660586672E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29319935321779E+03 0.31701934656524E+03
 0.29316979357746E+03 0.29387868753552E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316971695586E+03
 0.29387890162797E+03 0.29315000000003E+03 0.29315000000003E+03 0.29316979357746E+03 0.29387868753552E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29316971695586E+03 0.29387890162797E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29368945543039E+03 0.29315000000004E+03 0.24911895189635E+01 0.24612012260544E+01
 -.23315948122658E+00 0.88501839523666E+02 0.88736164802298E+02 0.23989545228463E+01 -.10855650075735E+01
 0.23892370715068E+01 0.74462078309725E+02 0.23859992032176E+01 -.10576845053655E+01 0.23763337991263E+01
 0.74488438360923E+02 0.23989545228462E+01 -.10855650075735E+01 0.23892370715067E+01 0.74462078309725E+02
 0.23859992032177E+01 -.10576845053654E+01 0.23763337991264E+01 0.74488438360923E+02 0.33301501045756E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31866150565645E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.58139925826691E-01 0.00000000000000E+00 0.00000000000000E+00 0.58139925826691E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99678364868268E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99678364868268E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98011881589829E-01 0.11509349451977E+00 0.29979396771964E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    120.08435891
 0.45363881576852E+00 0.29419566795680E+03 0.38280964576474E+03 0.36941006578030E+03 0.36613354786800E+03
 0.22999999980764E+00 0.00000000000000E+00 0.21551499476399E+00 0.00000000000000E+00 -.58183799610110E+01
 0.99638845296165E-03 0.23864107961516E+00 0.80000000000000E+04 0.30000000000000E+04 0.33523147032778E+02
 0.12571180137292E+02 0.29894410496081E+03 0.29315000000004E+03 0.29846834610271E+03 0.30388021717038E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29733977147990E+03 0.30384082950782E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29846834610271E+03 0.30388021717038E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29733977147990E+03 0.30384082950782E+03 0.29315000000003E+03 0.29315000000003E+03 0.31983148012235E+03
 0.29321214401020E+03 0.80763891322636E+03 0.79310709052230E+03 0.31617541087108E+03 0.97627594695510E+03
 0.65851965902967E+03 0.49026863859864E+03 0.39899604490730E+03 0.47879986230737E+03 0.10077819081635E+04
 0.39559903952193E+03 0.39615923410315E+03 0.38788022300189E+03 0.10053224133367E+04 0.49026863859864E+03
 0.39899604490730E+03 0.47879986230737E+03 0.10077819081635E+04 0.39559903952193E+03 0.39615923410315E+03
 0.38788022300189E+03 0.10053224133367E+04 0.97752712095054E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.40836211275173E+03 0.12345810744222E+01
 0.12345810744222E+01 0.22528288609296E+00 0.69424119531084E+00 0.29315175202953E+03 0.30754078932477E+03
 0.30088114769062E+03 0.30070945639990E+03 0.23000000000000E+00 0.00000000000000E+00 0.22710895477525E+00
 0.00000000000000E+00 -.38573620919527E-01 0.99967883874792E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29321206772914E+03 0.31985211406907E+03
 0.29317434962631E+03 0.29398540584605E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317424161634E+03
 0.29398566746523E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317434962631E+03 0.29398540584605E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29317424161634E+03 0.29398566746523E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29378645486672E+03 0.29315000000004E+03 0.29047284582198E+01 0.28614268652582E+01
 -.33133571148247E+00 0.97258440444634E+02 0.97591432834674E+02 0.27808761121010E+01 -.11951664342101E+01
 0.27677661721716E+01 0.80063090911405E+02 0.27649544334905E+01 -.11636523917404E+01 0.27519236463328E+01
 0.80092746519725E+02 0.27808761121010E+01 -.11951664342102E+01 0.27677661721716E+01 0.80063090911404E+02
 0.27649544334904E+01 -.11636523917404E+01 0.27519236463326E+01 0.80092746519725E+02 0.37316407438690E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32059073670640E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39225875869577E-01 0.00000000000000E+00 0.00000000000000E+00 0.39225875869577E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99569706484189E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99569706484189E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97972532859889E-01 0.11482890034287E+00 0.30036183243256E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    130.03267827
 0.34516872181464E+00 0.29460840193297E+03 0.38631233978413E+03 0.37576122944631E+03 0.37295531187570E+03
 0.22999999981504E+00 0.00000000000000E+00 0.21414409572438E+00 0.00000000000000E+00 -.67346296538968E+01
 0.10419530463894E-02 0.25900836319461E+00 0.76778891599017E+04 0.28792084349631E+04 0.30887033535627E+02
 0.11582637575860E+02 0.29966101778568E+03 0.29315000000004E+03 0.29904838057431E+03 0.30512108264337E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29784177767260E+03 0.30508136819027E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29904838057431E+03 0.30512108264337E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29784177767260E+03 0.30508136819027E+03 0.29315000000003E+03 0.29315000000003E+03 0.32249101461699E+03
 0.29322688320295E+03 0.86546463090521E+03 0.84969693330919E+03 0.33397810130711E+03 0.10038851890005E+04
 0.66823719718684E+03 0.51679794879234E+03 0.43161817038563E+03 0.50432914509468E+03 0.10552020376557E+04
 0.42204801920466E+03 0.42900937942258E+03 0.41375525015846E+03 0.10529728064128E+04 0.51679794879234E+03
 0.43161817038563E+03 0.50432914509468E+03 0.10552020376557E+04 0.42204801920466E+03 0.42900937942258E+03
 0.41375525015846E+03 0.10529728064128E+04 0.10369775928362E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.41292778169854E+03 0.11465697603297E+01
 0.11465697603297E+01 0.25512784416280E+00 0.64098122945873E+00 0.29315232842747E+03 0.30856026887608E+03
 0.30197613513463E+03 0.30178230778694E+03 0.23000000000000E+00 0.00000000000000E+00 0.22671669251355E+00
 0.00000000000000E+00 -.45182583239109E+00 0.99967279457512E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29322679839971E+03 0.32251296034796E+03
 0.29317925273461E+03 0.29409147838716E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317911130512E+03
 0.29409178791985E+03 0.29315000000003E+03 0.29315000000003E+03 0.29317925273461E+03 0.29409147838716E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29317911130512E+03 0.29409178791985E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29388416305526E+03 0.29315000000004E+03 0.33176328653442E+01 0.32602784052669E+01
 -.42079065611430E+00 0.10538879331108E+03 0.10581168792047E+03 0.31718072845189E+01 -.12936853974977E+01
 0.31551736742818E+01 0.85267140148898E+02 0.31530714654901E+01 -.12588964071053E+01 0.31365481653587E+01
 0.85299727702292E+02 0.31718072845189E+01 -.12936853974975E+01 0.31551736742818E+01 0.85267140148899E+02
 0.31530714654900E+01 -.12588964071054E+01 0.31365481653585E+01 0.85299727702292E+02 0.41130757789233E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32230742813318E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.22720659393600E-01 0.18693779569118E-03 0.00000000000000E+00 0.22720659393600E-01 0.18693779569118E-03
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99632062506026E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99632062506026E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97964495106215E-01 0.11443634879968E+00 0.30136620899791E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    140.54344851
 0.24943606205604E+00 0.29519840935502E+03 0.38951620785180E+03 0.38167412110552E+03 0.37943630127049E+03
 0.22999999984109E+00 0.00000000000000E+00 0.21267388276674E+00 0.00000000000000E+00 -.73898495043507E+01
 0.14418460459508E-02 0.28083073261294E+00 0.55484425833580E+04 0.20806659687593E+04 0.28486910693732E+02
 0.10682591510149E+02 0.30043186582071E+03 0.29315000000004E+03 0.29966423102018E+03 0.30639964596149E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29838297044587E+03 0.30635987355792E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29966423102018E+03 0.30639964596149E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29838297044587E+03 0.30635987355792E+03 0.29315000000003E+03 0.29315000000003E+03 0.32516920027592E+03
 0.29324552455041E+03 0.92408617809815E+03 0.90758642186153E+03 0.35250237937171E+03 0.10287078331081E+04
 0.67444294183951E+03 0.54416523219028E+03 0.46470663327465E+03 0.53101164211398E+03 0.11002540967921E+04
 0.44964980454386E+03 0.46231231204445E+03 0.44111540213874E+03 0.10982388362591E+04 0.54416523219028E+03
 0.46470663327465E+03 0.53101164211398E+03 0.11002540967921E+04 0.44964980454386E+03 0.46231231204445E+03
 0.44111540213874E+03 0.10982388362591E+04 0.10942278426547E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.41768710426944E+03 0.10919314983622E+01
 0.10919314983622E+01 0.28666015488040E+00 0.59350313694567E+00 0.29315347255655E+03 0.30956731854169E+03
 0.30307287248734E+03 0.30285788348278E+03 0.23000000000000E+00 0.00000000000000E+00 0.22627875194047E+00
 0.00000000000000E+00 -.64623432913830E+00 0.99966697430936E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29324543020171E+03 0.32519269085438E+03
 0.29318479743860E+03 0.29420372258144E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318461808028E+03
 0.29420408348106E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318479743860E+03 0.29420372258144E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29318461808028E+03 0.29420408348106E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29398822634752E+03 0.29315000000004E+03 0.37425610935012E+01 0.36665803637303E+01
 -.51673894447796E+00 0.11343363247072E+03 0.11395295510992E+03 0.35864405716619E+01 -.13960744936384E+01
 0.35659727368878E+01 0.90501859017795E+02 0.35646469111306E+01 -.13579733741111E+01 0.35443269385547E+01
 0.90537386715155E+02 0.35864405716619E+01 -.13960744936380E+01 0.35659727368878E+01 0.90501859017796E+02
 0.35646469111306E+01 -.13579733741111E+01 0.35443269385547E+01 0.90537386715155E+02 0.44936813253193E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32392322233102E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11756997882403E-01 0.65918915990375E-02 0.00000000000000E+00 0.11756997882403E-01 0.65918915990375E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99761813991696E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99761813991696E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97960071613395E-01 0.11310914431453E+00 0.30488803880300E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    150.01711785
 0.18027383761995E+00 0.29589396078229E+03 0.39210369212087E+03 0.38632232629261E+03 0.38458933914997E+03
 0.22999999872733E+00 0.00000000000000E+00 0.21131130710614E+00 0.00000000000000E+00 -.79015647485484E+01
 0.19950064092048E-02 0.30102588865739E+00 0.40100121799552E+04 0.15037545674832E+04 0.26575787337365E+02
 0.99659202515117E+01 0.30113752206084E+03 0.29315000000004E+03 0.30022427543679E+03 0.30752607235913E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29888196813154E+03 0.30748643356295E+03 0.29315000000003E+03
 0.29315000000003E+03 0.30022427543679E+03 0.30752607235913E+03 0.29315000000003E+03 0.29315000000003E+03
 0.29888196813154E+03 0.30748643356295E+03 0.29315000000003E+03 0.29315000000003E+03 0.32749137900329E+03
 0.29326596975427E+03 0.97420421249564E+03 0.95768917226644E+03 0.36905668760492E+03 0.10486762938851E+04
 0.67777432284215E+03 0.56820938618112E+03 0.49365256381791E+03 0.55487512110598E+03 0.11374531505414E+04
 0.47410971562514E+03 0.49142837009084E+03 0.46579036066489E+03 0.11356055685107E+04 0.56820938618112E+03
 0.49365256381791E+03 0.55487512110598E+03 0.11374531505414E+04 0.47410971562514E+03 0.49142837009084E+03
 0.46579036066489E+03 0.11356055685107E+04 0.11412933406232E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.42233549708554E+03 0.10919348583023E+01
 0.10919348583023E+01 0.31508116290526E+00 0.56359722243187E+00 0.29315489131813E+03 0.31041832045565E+03
 0.30393190664795E+03 0.30369861377462E+03 0.22999999909625E+00 0.00000000000000E+00 0.22586164876173E+00
 0.00000000000000E+00 -.80298019928060E+00 0.99966058929792E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29326586752488E+03 0.32751629835097E+03
 0.29319012039354E+03 0.29430557381240E+03 0.29315000000003E+03 0.29315000000003E+03 0.29318990414699E+03
 0.29430598148042E+03 0.29315000000003E+03 0.29315000000003E+03 0.29319012039354E+03 0.29430557381240E+03
 0.29315000000003E+03 0.29315000000003E+03 0.29318990414699E+03 0.29430598148042E+03 0.29315000000003E+03
 0.29315000000003E+03 0.29408277951142E+03 0.29315000000004E+03 0.41089397979977E+01 0.40113166355945E+01
 -.59718758732639E+00 0.12020623461712E+03 0.12080640814238E+03 0.39690425989833E+01 -.14844312838857E+01
 0.39450407518323E+01 0.95121189717030E+02 0.39444291266270E+01 -.14436200301768E+01 0.39206131032076E+01
 0.95159081738031E+02 0.39690425989834E+01 -.14844312838857E+01 0.39450407518324E+01 0.95121189717030E+02
 0.39444291266270E+01 -.14436200301768E+01 0.39206131032076E+01 0.95159081738031E+02 0.48072981541101E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32519035339745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.50695983816459E-02 0.14732472802137E-01 0.00000000000000E+00 0.50695983816459E-02 0.14732472802137E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99884583746256E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99884583746256E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97956399902543E-01 0.11108970367780E+00 0.31041832045565E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    160.00009309
 0.12425604937742E+00 0.29679517216039E+03 0.39460127339027E+03 0.39055027347566E+03 0.38928782793812E+03
 0.22999999948668E+00 0.00000000000000E+00 0.20982414783047E+00 0.00000000000000E+00 -.83835900595020E+01
 0.28943993818769E-02 0.32303110364723E+00 0.27639585781048E+04 0.10364844667893E+04 0.24765417043978E+02
 0.92870313914916E+01 0.30189235813137E+03 0.29315000000006E+03 0.30082249561215E+03 0.30869906754366E+03
 0.29315000000004E+03 0.29315000000004E+03 0.29942158825499E+03 0.30865971400151E+03 0.29315000000004E+03
 0.29315000000004E+03 0.30082249561215E+03 0.30869906754366E+03 0.29315000000004E+03 0.29315000000004E+03
 0.29942158825499E+03 0.30865971400151E+03 0.29315000000004E+03 0.29315000000004E+03 0.32986369402426E+03
 0.29329225559161E+03 0.10237145839356E+04 0.10078436016034E+04 0.38635015799504E+03 0.10675291480126E+04
 0.67924723922755E+03 0.59274525876764E+03 0.52338276930060E+03 0.57968998052822E+03 0.11752121099698E+04
 0.49921543501856E+03 0.52131652229157E+03 0.49159992195229E+03 0.11735189727389E+04 0.59274525876764E+03
 0.52338276930060E+03 0.57968998052822E+03 0.11752121099698E+04 0.49921543501856E+03 0.52131652229157E+03
 0.49159992195229E+03 0.11735189727389E+04 0.11871521003201E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.42661762606623E+03 0.10919380233190E+01
 0.10919380233190E+01 0.34503008862582E+00 0.53934257833092E+00 0.29315687153801E+03 0.31126055937695E+03
 0.30475116625938E+03 0.30449931334444E+03 0.22999999909691E+00 0.00000000000000E+00 0.22539730045842E+00
 0.00000000000000E+00 -.97007369965969E+00 0.99965218767158E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29329214549959E+03 0.32989009527256E+03
 0.29319609984518E+03 0.29441415459502E+03 0.29315000000004E+03 0.29315000000004E+03 0.29319584108895E+03
 0.29441461219404E+03 0.29315000000004E+03 0.29315000000004E+03 0.29319609984518E+03 0.29441415459502E+03
 0.29315000000004E+03 0.29315000000004E+03 0.29319584108895E+03 0.29441461219404E+03 0.29315000000004E+03
 0.29315000000004E+03 0.29418336567123E+03 0.29315000000006E+03 0.44709393871997E+01 0.43438263784483E+01
 -.68597849951158E+00 0.12688177762550E+03 0.12757118601751E+03 0.43723242634047E+01 -.15816723073635E+01
 0.43445742923853E+01 0.99763330414530E+02 0.43443684548259E+01 -.15379687613262E+01 0.43168492950030E+01
 0.99803740822779E+02 0.43723242634047E+01 -.15816723073636E+01 0.43445742923853E+01 0.99763330414530E+02
 0.43443684548260E+01 -.15379687613261E+01 0.43168492950030E+01 0.99803740822779E+02 0.51132373878659E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32639686747663E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.52332158198518E-03 0.25227958323066E-01 0.00000000000000E+00 0.52332158198518E-03 0.25227958323066E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10000378188668E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10000378188668E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97952547646106E-01 0.11078456673739E+00 0.31126055937695E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    170.81600511
 0.82138009119514E-01 0.29783951049329E+03 0.39707346944370E+03 0.39435650950196E+03 0.39348401848679E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20814670096634E+00 0.00000000000000E+00 -.89693349694127E+01
 0.43785566940900E-02 0.34782350802819E+00 0.18270860831374E+04 0.68515728117653E+03 0.23000170532901E+02
 0.86250639498377E+01 0.30270830772974E+03 0.29315000000010E+03 0.30147172215581E+03 0.30994862405830E+03
 0.29315000000009E+03 0.29315000000009E+03 0.30001291923105E+03 0.30990966258949E+03 0.29315000000009E+03
 0.29315000000009E+03 0.30147172215581E+03 0.30994862405830E+03 0.29315000000009E+03 0.29315000000009E+03
 0.30001291923105E+03 0.30990966258949E+03 0.29315000000009E+03 0.29315000000009E+03 0.33234171114236E+03
 0.29332742857577E+03 0.10727194340407E+04 0.10578238950834E+04 0.40474945094308E+03 0.10856020807289E+04
 0.67882888253111E+03 0.61797793745929E+03 0.55457909939466E+03 0.60538578878718E+03 0.12143712529126E+04
 0.52511243272079E+03 0.55266233429937E+03 0.51847587139149E+03 0.12128234045644E+04 0.61797793745929E+03
 0.55457909939466E+03 0.60538578878718E+03 0.12143712529126E+04 0.52511243272079E+03 0.55266233429937E+03
 0.51847587139149E+03 0.12128234045644E+04 0.12321084139853E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43053069799959E+03 0.10919418693952E+01
 0.10919418693952E+01 0.37747782468093E+00 0.51506436642284E+00 0.29315964932147E+03 0.31211581671375E+03
 0.30560671914861E+03 0.30533635664753E+03 0.22999999909878E+00 0.00000000000000E+00 0.22486389227577E+00
 0.00000000000000E+00 -.12819618255080E+01 0.99963963753347E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29332731077063E+03 0.33236954224915E+03
 0.29320289668774E+03 0.29453113789272E+03 0.29315000000009E+03 0.29315000000009E+03 0.29320258803539E+03
 0.29453164951498E+03 0.29315000000009E+03 0.29315000000009E+03 0.29320289668774E+03 0.29453113789272E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29320258803539E+03 0.29453164951498E+03 0.29315000000009E+03
 0.29315000000009E+03 0.29429168722187E+03 0.29315000000010E+03 0.48275988268837E+01 0.46583715623628E+01
 -.77135370123993E+00 0.13367607191200E+03 0.13445128238175E+03 0.48128951804638E+01 -.16807511478559E+01
 0.47811994027744E+01 0.10445710812281E+03 0.47810352852955E+01 -.16341033697689E+01 0.47496246435236E+01
 0.10450005601013E+03 0.48128951804638E+01 -.16807511478558E+01 0.47811994027744E+01 0.10445710812281E+03
 0.47810352852955E+01 -.16341033697684E+01 0.47496246435235E+01 0.10450005601013E+03 0.54271580145889E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32761222617315E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.40806986358516E-01 0.00000000000000E+00 0.00000000000000E+00 0.40806986358516E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10013514325249E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10013514325249E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97946339158231E-01 0.11047365298217E+00 0.31211581671375E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    180.01806992
 0.59655191333589E-01 0.29853967003213E+03 0.39897303080993E+03 0.39697590702544E+03 0.39632177963006E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20666926511288E+00 0.00000000000000E+00 -.95247431571688E+01
 0.60287374517405E-02 0.36966650363865E+00 0.13269776738561E+04 0.49761662769606E+03 0.21641127668467E+02
 0.81154228756753E+01 0.30339907278104E+03 0.29315000000011E+03 0.30205502481068E+03 0.31099965770847E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30054499984783E+03 0.31096109347180E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30205502481068E+03 0.31099965770847E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30054499984783E+03 0.31096109347180E+03 0.29315000000010E+03 0.29315000000010E+03 0.33438009980986E+03
 0.29336409350929E+03 0.11100472038378E+04 0.10952141857578E+04 0.41967733401647E+03 0.10983503509322E+04
 0.67657463024564E+03 0.63775231361313E+03 0.57988859451688E+03 0.63775231361313E+03 0.12454261169441E+04
 0.54544379254227E+03 0.57808388103338E+03 0.54544379254227E+03 0.12439863103531E+04 0.63775231361313E+03
 0.57988859451688E+03 0.63775231361313E+03 0.12454261169441E+04 0.54544379254227E+03 0.57808388103338E+03
 0.54544379254227E+03 0.12439863103531E+04 0.12656817627451E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43328786341673E+03 0.10919455163054E+01
 0.10919455163054E+01 0.40508401911504E+00 0.49514882038622E+00 0.29316272881496E+03 0.31278572095071E+03
 0.30630818667840E+03 0.30602401207196E+03 0.22999999906214E+00 0.00000000000000E+00 0.22438846160198E+00
 0.00000000000000E+00 -.16565900445733E+01 0.99962543966930E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29336397057215E+03 0.33440891106831E+03
 0.29320905859461E+03 0.29463066779259E+03 0.29315000000009E+03 0.29315000000009E+03 0.29320870429123E+03
 0.29463122429344E+03 0.29315000000009E+03 0.29315000000010E+03 0.29320905859461E+03 0.29463066779259E+03
 0.29315000000009E+03 0.29315000000009E+03 0.29320870429123E+03 0.29463122429344E+03 0.29315000000009E+03
 0.29315000000010E+03 0.29438370899611E+03 0.29315000000011E+03 0.51064624445403E+01 0.48906029254463E+01
 -.80848000406486E+00 0.13887725853875E+03 0.13968978094284E+03 0.52148045064481E+01 -.17441266172271E+01
 0.51799378775606E+01 0.10815440488546E+03 0.51797237528457E+01 -.16955838314550E+01 0.51451930750396E+01
 0.10819890718217E+03 0.52148045064482E+01 -.17441266172271E+01 0.51799378775607E+01 0.10815440488546E+03
 0.51797237528456E+01 -.16955838314550E+01 0.51451930750396E+01 0.10819890718217E+03 0.56755272190262E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32823412522049E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.53004156756082E-01 0.00000000000000E+00 0.00000000000000E+00 0.53004156756082E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10026041189061E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10026041189061E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97939378515782E-01 0.11022880586339E+00 0.31278572095071E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    190.22700784
 0.47449209781213E-01 0.29897509289137E+03 0.40090310785926E+03 0.39929097327100E+03 0.39875295074134E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20496968133444E+00 0.00000000000000E+00 -.10152692123742E+02
 0.75795813259178E-02 0.39478769100764E+00 0.10554672687058E+04 0.39580022576468E+03 0.20264056307280E+02
 0.75990211152301E+01 0.30414854692388E+03 0.29315000000011E+03 0.30269536048756E+03 0.31214577507083E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30111494284479E+03 0.31210767974869E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30269536048756E+03 0.31214577507083E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30111494284479E+03 0.31210767974869E+03 0.29315000000010E+03 0.29315000000010E+03 0.33656362425871E+03
 0.29342123100592E+03 0.11467307741230E+04 0.11306254679483E+04 0.43552668002800E+03 0.11105813734259E+04
 0.67287705999779E+03 0.65785633993700E+03 0.60676578884191E+03 0.65785633993700E+03 0.12777336240430E+04
 0.56612244505091E+03 0.60507219504700E+03 0.56612244505091E+03 0.12764003059745E+04 0.65785633993700E+03
 0.60676578884191E+03 0.65785633993700E+03 0.12777336240430E+04 0.56612244505091E+03 0.60507219504700E+03
 0.56612244505091E+03 0.12764003059745E+04 0.12979841276861E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43616732279018E+03 0.10919496395652E+01
 0.10919496395652E+01 0.43571083288060E+00 0.47354497630156E+00 0.29316730943410E+03 0.31345783589848E+03
 0.30705218464933E+03 0.30675528980253E+03 0.22999999906195E+00 0.00000000000000E+00 0.22383797592968E+00
 0.00000000000000E+00 -.21364491839018E+01 0.99960508519726E-03 0.11164875882133E-01 0.80000000000000E+04
 0.30000000000000E+04 0.71653281993060E+03 0.26869980747398E+03 0.29342109197941E+03 0.33659326798705E+03
 0.29321820933874E+03 0.29474337217725E+03 0.29315000000010E+03 0.29315000000010E+03 0.29321775835692E+03
 0.29474397579798E+03 0.29315000000010E+03 0.29315000000010E+03 0.29321820933874E+03 0.29474337217725E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29321775835692E+03 0.29474397579798E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29448705790124E+03 0.29315000000011E+03 0.67871973541061E+01 0.64931095211711E+01
 0.89225200531411E+00 0.14585063038898E+03 0.14495391712364E+03 0.65109043584008E+01 -.76364309726731E-01
 0.64712646399994E+01 0.11349290587104E+03 0.64539167261924E+01 -.26409943815536E-01 0.64147193144372E+01
 0.11353846197116E+03 0.65109043584008E+01 -.76364309726666E-01 0.64712646399994E+01 0.11349290587104E+03
 0.64539167261928E+01 -.26409943815536E-01 0.64147193144376E+01 0.11353846197116E+03 0.60476055990286E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32898288678789E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.65192699962414E-01 0.00000000000000E+00 0.00000000000000E+00 0.65192699962414E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10066799331436E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10066799331436E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97930734909146E-01 0.10998222533144E+00 0.31345783589848E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    201.61686590
 0.43962375509777E-01 0.29917837169247E+03 0.40271575504617E+03 0.40119850527074E+03 0.40068373214566E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20303790608271E+00 0.00000000000000E+00 -.10800130831687E+02
 0.81807432529346E-02 0.42344533333249E+00 0.97790625529413E+03 0.36671484573530E+03 0.18892639427716E+02
 0.70847397853937E+01 0.30495652940953E+03 0.29315000000012E+03 0.30338128022975E+03 0.31339197318616E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30173523037293E+03 0.31335442296152E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30338128022975E+03 0.31339197318616E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30173523037293E+03 0.31335442296152E+03 0.29315000000010E+03 0.29315000000010E+03 0.33888299691145E+03
 0.29351948612058E+03 0.11815220227794E+04 0.11628747405724E+04 0.45097730775873E+03 0.11185446599001E+04
 0.66531246560261E+03 0.67753720484405E+03 0.63382851412765E+03 0.67753720484405E+03 0.13085395347143E+04
 0.58622155356416E+03 0.63224681441301E+03 0.58622155356416E+03 0.13073126819991E+04 0.67753720484404E+03
 0.63382851412765E+03 0.67753720484404E+03 0.13085395347143E+04 0.58622155356416E+03 0.63224681441301E+03
 0.58622155356416E+03 0.13073126819991E+04 0.13265050186083E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43820489759024E+03 0.10919538908366E+01
 0.10919538908366E+01 0.46988040706206E+00 0.45012047169198E+00 0.29317506268872E+03 0.31408587435568E+03
 0.30781095141502E+03 0.30750425888427E+03 0.22999999905936E+00 0.00000000000000E+00 0.22319147401865E+00
 0.00000000000000E+00 -.26728098475342E+01 0.99957335661419E-03 0.31316818754537E-01 0.80000000000000E+04
 0.30000000000000E+04 0.25545378867197E+03 0.95795170751989E+02 0.29351921983732E+03 0.33891321656876E+03
 0.29323447693106E+03 0.29487639926550E+03 0.29315000000010E+03 0.29315000000010E+03 0.29323379843854E+03
 0.29487704863306E+03 0.29315000000010E+03 0.29315000000010E+03 0.29323447693106E+03 0.29487639926550E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29323379843854E+03 0.29487704863306E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29460679383846E+03 0.29315000000011E+03 0.96732813253492E+01 0.92318756795380E+01
 0.41879313570256E+01 0.15393524427872E+03 0.14972637326491E+03 0.86631726949961E+01 0.31500021270524E+01
 0.86144633401563E+01 0.11992482842357E+03 0.85683649332288E+01 0.32003495813653E+01 0.85203558629488E+01
 0.11997043157313E+03 0.86631726949961E+01 0.31500021270524E+01 0.86144633401563E+01 0.11992482842357E+03
 0.85683649332286E+01 0.32003495813652E+01 0.85203558629486E+01 0.11997043157313E+03 0.65117383958832E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32970364440054E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76460717816775E-01 0.00000000000000E+00 0.00000000000000E+00 0.76460717816775E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10133125818230E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10133125818230E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97921280722610E-01 0.10975112997050E+00 0.31408587435568E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    211.36749839
 0.44746567746152E-01 0.29930003310781E+03 0.40391296523109E+03 0.40235260867963E+03 0.40181814562977E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20139460117260E+00 0.00000000000000E+00 -.11256356488287E+02
 0.80373713776522E-02 0.44800308562150E+00 0.99535029851226E+03 0.37325636194210E+03 0.17857019866062E+02
 0.66963824497731E+01 0.30562278816582E+03 0.29315000000014E+03 0.30394996849124E+03 0.31442050505107E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30225176995189E+03 0.31438344167057E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30394996849124E+03 0.31442050505107E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30225176995189E+03 0.31438344167057E+03 0.29315000000010E+03 0.29315000000010E+03 0.34074612166121E+03
 0.29362618078206E+03 0.12063479704809E+04 0.11853341464036E+04 0.46143412259252E+03 0.11196436797925E+04
 0.65590238658705E+03 0.69185022883138E+03 0.65351995161283E+03 0.69185022883138E+03 0.13288151407654E+04
 0.60080527497211E+03 0.65202665929561E+03 0.60080527497211E+03 0.13276716979273E+04 0.69185022883138E+03
 0.65351995161283E+03 0.69185022883138E+03 0.13288151407654E+04 0.60080527497211E+03 0.65202665929561E+03
 0.60080527497211E+03 0.13276716979273E+04 0.13441871150241E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43932167401469E+03 0.10919568865701E+01
 0.10919568865701E+01 0.49913230452823E+00 0.43067082186229E+00 0.29318501862500E+03 0.31452798138636E+03
 0.30840012051075E+03 0.30808862029159E+03 0.22999999902536E+00 0.00000000000000E+00 0.22261110716378E+00
 0.00000000000000E+00 -.30676265458663E+01 0.99953551702647E-03 0.48561102602543E-01 0.80000000000000E+04
 0.30000000000000E+04 0.16474090519479E+03 0.61777839448046E+02 0.29362582177006E+03 0.34077645376538E+03
 0.29325176416997E+03 0.29499184810389E+03 0.29315000000010E+03 0.29315000000010E+03 0.29325083256177E+03
 0.29499253017994E+03 0.29315000000010E+03 0.29315000000010E+03 0.29325176416997E+03 0.29499184810389E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29325083256177E+03 0.29499253017994E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29470980827570E+03 0.29315000000011E+03 0.12162598500849E+02 0.11548668973160E+02
 0.71713837251495E+01 0.16014877976845E+03 0.15294153912468E+03 0.10607530457240E+02 0.60452722737968E+01
 0.10550580840974E+02 0.12487557515336E+03 0.10480394688341E+02 0.60948547812878E+01 0.10424445318847E+02
 0.12492016807965E+03 0.10607530457240E+02 0.60452722737969E+01 0.10550580840974E+02 0.12487557515336E+03
 0.10480394688341E+02 0.60948547812878E+01 0.10424445318847E+02 0.12492016807965E+03 0.68822241322271E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33020954699834E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.83752084743186E-01 0.00000000000000E+00 0.00000000000000E+00 0.83752084743186E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10194525525774E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10194525525774E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97914395177201E-01 0.10958872779556E+00 0.31452798138636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    221.11813088
 0.46747106154025E-01 0.29945717479890E+03 0.40481320761592E+03 0.40317151106589E+03 0.40260602416362E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19977984120039E+00 0.00000000000000E+00 -.11609804764387E+02
 0.76934110835678E-02 0.47231575401879E+00 0.10398508428969E+04 0.38994406608632E+03 0.16937821641414E+02
 0.63516831155301E+01 0.30626749052671E+03 0.29315000000018E+03 0.30450322946410E+03 0.31540936887242E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30275541390886E+03 0.31537281616702E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30450322946410E+03 0.31540936887242E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30275541390886E+03 0.31537281616702E+03 0.29315000000010E+03 0.29315000000010E+03 0.34248590529081E+03
 0.29375240592687E+03 0.12271920619100E+04 0.12040056960093E+04 0.46935601774098E+03 0.11161932533844E+04
 0.64449045555469E+03 0.70401863175970E+03 0.66995427745915E+03 0.70401863175970E+03 0.13436839538891E+04
 0.61321158086294E+03 0.66854384390726E+03 0.61321158086294E+03 0.13426180172099E+04 0.70401863175970E+03
 0.66995427745915E+03 0.70401863175970E+03 0.13436839538891E+04 0.61321158086294E+03 0.66854384390726E+03
 0.61321158086294E+03 0.13426180172099E+04 0.13564861838502E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44005446247431E+03 0.10919592074453E+01
 0.10919592074453E+01 0.52838420199440E+00 0.41181729054498E+00 0.29319930678364E+03 0.31488754372790E+03
 0.30893314974451E+03 0.30861998129758E+03 0.22999999902609E+00 0.00000000000000E+00 0.22200750052690E+00
 0.00000000000000E+00 -.33811800618311E+01 0.99948371363923E-03 0.65823220775394E-01 0.80000000000000E+04
 0.30000000000000E+04 0.12153765655585E+03 0.45576621208446E+02 0.29375198121523E+03 0.34251604811755E+03
 0.29327189448839E+03 0.29510778585661E+03 0.29315000000010E+03 0.29315000000010E+03 0.29327066689031E+03
 0.29510849332127E+03 0.29315000000010E+03 0.29315000000010E+03 0.29327189448839E+03 0.29510778585661E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29327066689031E+03 0.29510849332127E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29481282715673E+03 0.29315000000011E+03 0.14630864116143E+02 0.13800937655801E+02
 0.10244091617043E+02 0.16569055077941E+03 0.15539523870428E+03 0.12612382502370E+02 0.90117703395499E+01
 0.12548458517825E+02 0.12930384038962E+03 0.12453387030055E+02 0.90597872416230E+01 0.12390822374072E+02
 0.12934667990487E+03 0.12612382502370E+02 0.90117703395499E+01 0.12548458517825E+02 0.12930384038962E+03
 0.12453387030055E+02 0.90597872416231E+01 0.12390822374072E+02 0.12934667990487E+03 0.72251268331953E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33061656193703E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.89089517307686E-01 0.00000000000000E+00 0.00000000000000E+00 0.89089517307686E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10254826101908E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10254826101908E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97908968694155E-01 0.10945718572608E+00 0.31488754372790E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    230.86876337
 0.48969732722746E-01 0.29964900882773E+03 0.40547508173436E+03 0.40374765689925E+03 0.40315098585967E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19820125609990E+00 0.00000000000000E+00 -.11872033122704E+02
 0.73442233574963E-02 0.49624755396761E+00 0.10892914894581E+04 0.40848430854678E+03 0.16120986261873E+02
 0.60453698482025E+01 0.30688992348612E+03 0.29315000000026E+03 0.30503975069537E+03 0.31635424967488E+03
 0.29315000000010E+03 0.29315000000010E+03 0.30324439977956E+03 0.31631822270498E+03 0.29315000000010E+03
 0.29315000000010E+03 0.30503975069537E+03 0.31635424967488E+03 0.29315000000010E+03 0.29315000000010E+03
 0.30324439977956E+03 0.31631822270498E+03 0.29315000000010E+03 0.29315000000010E+03 0.34410239625389E+03
 0.29389721866789E+03 0.12447366260993E+04 0.12195930160559E+04 0.47510151585336E+03 0.11093442818914E+04
 0.63186725845880E+03 0.71436125330321E+03 0.68350361883103E+03 0.71436125330321E+03 0.13540547214626E+04
 0.62378222375732E+03 0.68217108160577E+03 0.62378222375732E+03 0.13530611006147E+04 0.71436125330321E+03
 0.68350361883103E+03 0.71436125330321E+03 0.13540547214626E+04 0.62378222375732E+03 0.68217108160577E+03
 0.62378222375732E+03 0.13530611006147E+04 0.13645232338678E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44053232697375E+03 0.10919609293433E+01
 0.10919609293433E+01 0.55763609946057E+00 0.39357924936579E+00 0.29321911341232E+03 0.31517274361680E+03
 0.30941241475231E+03 0.30910042426758E+03 0.22999999902577E+00 0.00000000000000E+00 0.22138206888220E+00
 0.00000000000000E+00 -.36184273726117E+01 0.99941385863135E-03 0.83125291892537E-01 0.80000000000000E+04
 0.30000000000000E+04 0.96240263557117E+02 0.36090098833919E+02 0.29389674801223E+03 0.34413213081713E+03
 0.29329448238249E+03 0.29522294165260E+03 0.29315000000010E+03 0.29315000000010E+03 0.29329292580850E+03
 0.29522366678281E+03 0.29315000000010E+03 0.29315000000010E+03 0.29329448238249E+03 0.29522294165260E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29329292580850E+03 0.29522366678281E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29491476774676E+03 0.29315000000011E+03 0.17045596346867E+02 0.15956615796626E+02
 0.13368430416502E+02 0.17060606701702E+03 0.15717079444844E+03 0.14657669359346E+02 0.12013545594407E+02
 0.14589973076453E+02 0.13324536903191E+03 0.14467749850267E+02 0.12059193939146E+02 0.14401809341362E+02
 0.13328571234540E+03 0.14657669359346E+02 0.12013545594407E+02 0.14589973076453E+02 0.13324536903191E+03
 0.14467749850267E+02 0.12059193939146E+02 0.14401809341362E+02 0.13328571234540E+03 0.75406240597184E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33093841377026E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.92892834965786E-01 0.00000000000000E+00 0.00000000000000E+00 0.92892834965786E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10314646763528E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10314646763528E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97904904043258E-01 0.10935334179046E+00 0.31517274361680E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    240.61939586
 0.50968692227335E-01 0.29986815294384E+03 0.40596027112923E+03 0.40415781228939E+03 0.40353468954873E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19666096180984E+00 0.00000000000000E+00 -.12059646942871E+02
 0.70561869697962E-02 0.51973041547721E+00 0.11337568057995E+04 0.42515880217480E+03 0.15392595395162E+02
 0.57722232731857E+01 0.30749088731403E+03 0.29315000000040E+03 0.30555972654553E+03 0.31725533159421E+03
 0.29315000000011E+03 0.29315000000011E+03 0.30371865676019E+03 0.31721984076220E+03 0.29315000000010E+03
 0.29315000000011E+03 0.30555972654553E+03 0.31725533159421E+03 0.29315000000011E+03 0.29315000000011E+03
 0.30371865676019E+03 0.31721984076220E+03 0.29315000000010E+03 0.29315000000011E+03 0.34560305216445E+03
 0.29406006165127E+03 0.12596483692165E+04 0.12327415036114E+04 0.47912384200781E+03 0.11002292262092E+04
 0.61870976499131E+03 0.72321948874120E+03 0.69466917964717E+03 0.72321948874120E+03 0.13609518830344E+04
 0.63286940875675E+03 0.69340991600341E+03 0.63286940875675E+03 0.13600258117786E+04 0.72321948874120E+03
 0.69466917964717E+03 0.72321948874120E+03 0.13609518830344E+04 0.63286940875675E+03 0.69340991600340E+03
 0.63286940875675E+03 0.13600258117786E+04 0.13693963446683E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44085094359767E+03 0.10919621612957E+01
 0.10919621612957E+01 0.58688799692674E+00 0.37596963665952E+00 0.29324576799833E+03 0.31539146316869E+03
 0.30984072385751E+03 0.30953243955163E+03 0.22999999902572E+00 0.00000000000000E+00 0.22073648527308E+00
 0.00000000000000E+00 -.37906106422427E+01 0.99932131804449E-03 0.10047767860278E+00 0.80000000000000E+04
 0.30000000000000E+04 0.79619673854398E+02 0.29857377695399E+02 0.29405955696773E+03 0.34563225458324E+03
 0.29331929387144E+03 0.29533660023768E+03 0.29315000000010E+03 0.29315000000010E+03 0.29331738258529E+03
 0.29533733473416E+03 0.29315000000010E+03 0.29315000000010E+03 0.29331929387144E+03 0.29533660023768E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29331738258529E+03 0.29533733473416E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29501506691596E+03 0.29315000000012E+03 0.19376988509268E+02 0.17987108782277E+02
 0.16510222126978E+02 0.17493774341762E+03 0.15834497018001E+03 0.16726706355583E+02 0.15017946198472E+02
 0.16660411603743E+02 0.13673169674643E+03 0.16507186888654E+02 0.15060434055011E+02 0.16443052086877E+02
 0.13676881590415E+03 0.16726706355583E+02 0.15017946198472E+02 0.16660411603743E+02 0.13673169674643E+03
 0.16507186888654E+02 0.15060434055011E+02 0.16443052086877E+02 0.13676881590415E+03 0.78289642260777E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33118753650658E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.95528500845475E-01 0.00000000000000E+00 0.00000000000000E+00 0.95528500845475E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10372303109909E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10372303109909E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97902002439980E-01 0.10927408236710E+00 0.31539146316869E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    250.00385322
 0.52527816186283E-01 0.30010317021652E+03 0.40631249741365E+03 0.40445284940822E+03 0.40381020845687E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19521391033670E+00 0.00000000000000E+00 -.12182793942179E+02
 0.68467456837896E-02 0.54187976094496E+00 0.11684383164605E+04 0.43816436867267E+03 0.14763422767533E+02
 0.55362835378248E+01 0.30805281622624E+03 0.29315000000063E+03 0.30604781436822E+03 0.31808510422796E+03
 0.29315000000011E+03 0.29315000000012E+03 0.30416429114950E+03 0.31805015108696E+03 0.29315000000011E+03
 0.29315000000012E+03 0.30604781436822E+03 0.31808510422796E+03 0.29315000000011E+03 0.29315000000012E+03
 0.30416429114950E+03 0.31805015108696E+03 0.29315000000011E+03 0.29315000000012E+03 0.34694215010202E+03
 0.29423382008391E+03 0.12720594153929E+04 0.12436241758678E+04 0.48184537495279E+03 0.10903669205655E+04
 0.60611231873797E+03 0.73063555695735E+03 0.70363758655576E+03 0.73063555695735E+03 0.13651902347714E+04
 0.64051076311995E+03 0.70244470400488E+03 0.64051076311995E+03 0.13643248183471E+04 0.73063555695735E+03
 0.70363758655576E+03 0.73063555695735E+03 0.13651902347714E+04 0.64051076311995E+03 0.70244470400488E+03
 0.64051076311995E+03 0.13643248183472E+04 0.13720228617562E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44107254965931E+03 0.10919629699332E+01
 0.10919629699332E+01 0.61504136901586E+00 0.35962136691898E+00 0.29327943635309E+03 0.31554670004568E+03
 0.31020817750790E+03 0.30990553507297E+03 0.22999999902667E+00 0.00000000000000E+00 0.22009760836573E+00
 0.00000000000000E+00 -.39041130341037E+01 0.99920547665370E-03 0.11723168883007E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68240934510430E+02 0.25590350441411E+02 0.29423328532028E+03 0.34697075682930E+03
 0.29334562494222E+03 0.29544487682566E+03 0.29315000000010E+03 0.29315000000010E+03 0.29334334746654E+03
 0.29544561080427E+03 0.29315000000010E+03 0.29315000000010E+03 0.29334562494222E+03 0.29544487682566E+03
 0.29315000000010E+03 0.29315000000010E+03 0.29334334746654E+03 0.29544561080427E+03 0.29315000000010E+03
 0.29315000000010E+03 0.29511051286000E+03 0.29315000000014E+03 0.21521315571097E+02 0.19803955092705E+02
 0.19528082630998E+02 0.17859797200451E+03 0.15897224896036E+03 0.18734551674618E+02 0.17889095651533E+02
 0.18675865424712E+02 0.13968839106861E+03 0.18488220621868E+02 0.17927709341120E+02 0.18432057828658E+02
 0.13972164705788E+03 0.18734551674618E+02 0.17889095651533E+02 0.18675865424712E+02 0.13968839106861E+03
 0.18488220621868E+02 0.17927709341120E+02 0.18432057828658E+02 0.13972164705788E+03 0.80815261526529E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33136986902035E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97198803507424E-01 0.00000000000000E+00 0.00000000000000E+00 0.97198803507424E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10424372620527E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10424372620527E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900143708032E-01 0.10921812769930E+00 0.31554670004568E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    260.01095993
 0.53804181723943E-01 0.30036450188288E+03 0.40660712520539E+03 0.40470169273470E+03 0.40404397772982E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19370658098825E+00 0.00000000000000E+00 -.12277951329510E+02
 0.66843239147178E-02 0.56501218002925E+00 0.11968300911309E+04 0.44881128417408E+03 0.14158986801994E+02
 0.53096200507477E+01 0.30863251835214E+03 0.29315000000103E+03 0.30655252927604E+03 0.31893130777306E+03
 0.29315000000012E+03 0.29315000000014E+03 0.30462517822372E+03 0.31889692218840E+03 0.29315000000012E+03
 0.29315000000014E+03 0.30655252927604E+03 0.31893130777306E+03 0.29315000000012E+03 0.29315000000014E+03
 0.30462517822372E+03 0.31889692218840E+03 0.29315000000012E+03 0.29315000000014E+03 0.34828236731064E+03
 0.29443522791669E+03 0.12836958045636E+04 0.12537560059945E+04 0.48375695086409E+03 0.10790998416059E+04
 0.59292410598746E+03 0.73762182975971E+03 0.71173981588489E+03 0.73762182975971E+03 0.13679258206942E+04
 0.64774255938375E+03 0.71061342351586E+03 0.64774255938375E+03 0.13671208535635E+04 0.73762182975971E+03
 0.71173981588489E+03 0.73762182975971E+03 0.13679258206942E+04 0.64774255938375E+03 0.71061342351586E+03
 0.64774255938375E+03 0.13671208535635E+04 0.13732817000354E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44126004926319E+03 0.10919635947795E+01
 0.10919635947795E+01 0.64506268915549E+00 0.34283285621914E+00 0.29332546692359E+03 0.31566116353421E+03
 0.31055622309111E+03 0.31026147435956E+03 0.22999999903708E+00 0.00000000000000E+00 0.21939910980966E+00
 0.00000000000000E+00 -.39920266930902E+01 0.99904780757492E-03 0.13515547746988E+00 0.80000000000000E+04
 0.30000000000000E+04 0.59191089771282E+02 0.22196658664231E+02 0.29443466236493E+03 0.34831029161767E+03
 0.29337530730277E+03 0.29555758054024E+03 0.29315000000010E+03 0.29315000000011E+03 0.29337262862654E+03
 0.29555830557029E+03 0.29315000000010E+03 0.29315000000011E+03 0.29337530730277E+03 0.29555758054024E+03
 0.29315000000010E+03 0.29315000000011E+03 0.29337262862654E+03 0.29555830557029E+03 0.29315000000010E+03
 0.29315000000011E+03 0.29520957468199E+03 0.29315000000016E+03 0.23678496826326E+02 0.21578865786127E+02
 0.22700733275060E+02 0.18200741881051E+03 0.15919318186907E+03 0.20864698438058E+02 0.20896649857836E+02
 0.20823097625490E+02 0.14245438337139E+03 0.20591544527461E+02 0.20930572923377E+02 0.20552721123010E+02
 0.14248302375054E+03 0.20864698438058E+02 0.20896649857836E+02 0.20823097625490E+02 0.14245438337139E+03
 0.20591544527461E+02 0.20930572923377E+02 0.20552721123010E+02 0.14248302375054E+03 0.83252418915605E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33151385544876E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.98451419144469E-01 0.00000000000000E+00 0.00000000000000E+00 0.98451419144469E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10475856847489E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10475856847489E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97898774935850E-01 0.10917690234049E+00 0.31566116353421E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    270.00231217
 0.54738696296516E-01 0.30063101646687E+03 0.40684955098470E+03 0.40491146295069E+03 0.40424350088019E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19223575266046E+00 0.00000000000000E+00 -.12343926632418E+02
 0.65702069063543E-02 0.58761789485242E+00 0.12176176662356E+04 0.45660662483834E+03 0.13614289268725E+02
 0.51053584757719E+01 0.30919288804675E+03 0.29315000000169E+03 0.30704165217070E+03 0.31973968466192E+03
 0.29315000000013E+03 0.29315000000017E+03 0.30507196171324E+03 0.31970586205271E+03 0.29315000000012E+03
 0.29315000000017E+03 0.30704165217070E+03 0.31973968466192E+03 0.29315000000013E+03 0.29315000000017E+03
 0.30507196171324E+03 0.31970586205271E+03 0.29315000000012E+03 0.29315000000017E+03 0.34953704632141E+03
 0.29465341578820E+03 0.12940317971627E+04 0.12626897340065E+04 0.48497445395008E+03 0.10676780457963E+04
 0.58027871957645E+03 0.74385185141772E+03 0.71868336457355E+03 0.74385185141772E+03 0.13694180628151E+04
 0.65422155307180E+03 0.71761919033770E+03 0.65422155307180E+03 0.13686693441742E+04 0.74385185141772E+03
 0.71868336457355E+03 0.74385185141772E+03 0.13694180628151E+04 0.65422155307180E+03 0.71761919033770E+03
 0.65422155307180E+03 0.13686693441742E+04 0.13735216556523E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44142433840866E+03 0.10919640280035E+01
 0.10919640280035E+01 0.67503674586950E+00 0.32672853922333E+00 0.29338339527234E+03 0.31573087110268E+03
 0.31086316568043E+03 0.31057788132440E+03 0.22999999905824E+00 0.00000000000000E+00 0.21868544430633E+00
 0.00000000000000E+00 -.40521319619078E+01 0.99884995353287E-03 0.15311235289849E+00 0.80000000000000E+04
 0.30000000000000E+04 0.52249213394975E+02 0.19593455023116E+02 0.29465282832452E+03 0.34956432185544E+03
 0.29340665874726E+03 0.29566728358511E+03 0.29315000000011E+03 0.29315000000011E+03 0.29340357688641E+03
 0.29566799015745E+03 0.29315000000011E+03 0.29315000000011E+03 0.29340665874726E+03 0.29566728358511E+03
 0.29315000000011E+03 0.29315000000011E+03 0.29340357688641E+03 0.29566799015745E+03 0.29315000000011E+03
 0.29315000000011E+03 0.29530577162179E+03 0.29315000000019E+03 0.25680510204235E+02 0.23167609959350E+02
 0.25810573523819E+02 0.18496581875655E+03 0.15902619236511E+03 0.22980563420939E+02 0.23830817652943E+02
 0.22980563420939E+02 0.14486367505706E+03 0.22682674363626E+02 0.23859359652276E+02 0.22682674363626E+02
 0.14488707996514E+03 0.22980563420939E+02 0.23830817652943E+02 0.22980563420939E+02 0.14486367505706E+03
 0.22682674363626E+02 0.23859359652276E+02 0.22682674363626E+02 0.14488707996514E+03 0.85440997721094E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33161528406747E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.99277165521692E-01 0.00000000000000E+00 0.00000000000000E+00 0.99277165521692E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10522638805064E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10522638805064E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897921217550E-01 0.10915178145728E+00 0.31573087110268E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    280.00519263
 0.55402746248051E-01 0.30095520459989E+03 0.40706422626034E+03 0.40510464919311E+03 0.40443089909724E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19079472194273E+00 0.00000000000000E+00 -.12388102033496E+02
 0.64914569469122E-02 0.60977426425885E+00 0.12323889791498E+04 0.46214586718117E+03 0.13119609122441E+02
 0.49198534209153E+01 0.30973784568381E+03 0.29315000000276E+03 0.30740974396926E+03 0.32051667888880E+03
 0.29315000000017E+03 0.29315000000023E+03 0.30550720746584E+03 0.32048341518149E+03 0.29315000000015E+03
 0.29315000000023E+03 0.30740974396926E+03 0.32051667888880E+03 0.29315000000017E+03 0.29315000000023E+03
 0.30550720746584E+03 0.32048341518149E+03 0.29315000000015E+03 0.29315000000023E+03 0.35072067943469E+03
 0.29488863917393E+03 0.13033810061800E+04 0.12709853588679E+04 0.48572965154846E+03 0.10564263808450E+04
 0.56826808103881E+03 0.75015251515990E+03 0.72477857181347E+03 0.72168568741967E+03 0.13701272313007E+04
 0.66011987908907E+03 0.72377275888836E+03 0.66011987908907E+03 0.13694309793123E+04 0.75015251515990E+03
 0.72477857181347E+03 0.72168568741967E+03 0.13701272313007E+04 0.66011987908907E+03 0.72377275888837E+03
 0.66011987908907E+03 0.13694309793123E+04 0.13730331801251E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44158548587215E+03 0.10919643180797E+01
 0.10919643180797E+01 0.70504538724664E+00 0.31125413458029E+00 0.29345518964362E+03 0.31576374057320E+03
 0.31113465476432E+03 0.31086010233791E+03 0.22999999907802E+00 0.00000000000000E+00 0.21795605010412E+00
 0.00000000000000E+00 -.40904396982148E+01 0.99860520530008E-03 0.17115134172637E+00 0.80000000000000E+04
 0.30000000000000E+04 0.46742256994923E+02 0.17528346373096E+02 0.29488802950812E+03 0.35074726841318E+03
 0.29343956281487E+03 0.29577426501703E+03 0.29315000000011E+03 0.29315000000011E+03 0.29343604899795E+03
 0.29577494340292E+03 0.29315000000011E+03 0.29315000000011E+03 0.29343956281487E+03 0.29577426501703E+03
 0.29315000000011E+03 0.29315000000011E+03 0.29343604899795E+03 0.29577494340292E+03 0.29315000000011E+03
 0.29315000000011E+03 0.29539939627124E+03 0.29315000000027E+03 0.27519795750855E+02 0.24567404882472E+02
 0.28850453152820E+02 0.18753709039395E+03 0.15854238497536E+03 0.25082151681406E+02 0.26685541282918E+02
 0.25082151681406E+02 0.14696534454227E+03 0.24761773442120E+02 0.26708061759326E+02 0.24761773442120E+02
 0.14698294477284E+03 0.25082151681410E+02 0.26685541282918E+02 0.25082151681410E+02 0.14696534454227E+03
 0.24761773442120E+02 0.26708061759326E+02 0.24761773442120E+02 0.14698294477284E+03 0.87401048938240E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33168303959141E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.99778240937894E-01 0.00000000000000E+00 0.00000000000000E+00 0.99778240937894E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10564587818593E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10564587818593E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897472136089E-01 0.10913987735542E+00 0.31576374057320E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    290.14032930
 0.55868328866311E-01 0.30125561086852E+03 0.40726936193553E+03 0.40529509156588E+03 0.40461771088302E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18936429459678E+00 0.00000000000000E+00 -.12424189867708E+02
 0.64373595915675E-02 0.63175721860453E+00 0.12427455521483E+04 0.46602958205563E+03 0.12663092346884E+02
 0.47486596300816E+01 0.31027497020446E+03 0.29315000000451E+03 0.30781850556031E+03 0.32127488744915E+03
 0.29315000000023E+03 0.29315000000033E+03 0.30593704973021E+03 0.32124218272840E+03 0.29315000000019E+03
 0.29315000000033E+03 0.30781850556031E+03 0.32127488744915E+03 0.29315000000023E+03 0.29315000000033E+03
 0.30593704973021E+03 0.32124218272840E+03 0.29315000000019E+03 0.29315000000033E+03 0.35185662757340E+03
 0.29514364066515E+03 0.13120704802208E+04 0.12785334399384E+04 0.48618047570840E+03 0.10454110701104E+04
 0.55679969202341E+03 0.75579655102421E+03 0.73030243874304E+03 0.72680886380066E+03 0.13703660719036E+04
 0.66564363666989E+03 0.72935197558852E+03 0.66564363666989E+03 0.13697193080124E+04 0.75579655102421E+03
 0.73030243874304E+03 0.72680886380066E+03 0.13703660719036E+04 0.66564363666989E+03 0.72935197558852E+03
 0.66564363666989E+03 0.13697193080124E+04 0.13723497238381E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44175339670063E+03 0.10919645550492E+01
 0.10919645550492E+01 0.73545079724808E+00 0.29622278257009E+00 0.29354386643819E+03 0.31576657536478E+03
 0.31137799358179E+03 0.31111529468489E+03 0.22999999907972E+00 0.00000000000000E+00 0.21720312184262E+00
 0.00000000000000E+00 -.41200424160565E+01 0.99830324444359E-03 0.18948969603926E+00 0.80000000000000E+04
 0.30000000000000E+04 0.42218654455715E+02 0.15831995420893E+02 0.29514300785700E+03 0.35188246721410E+03
 0.29347440703359E+03 0.29587973215742E+03 0.29315000000011E+03 0.29315000000012E+03 0.29347048806997E+03
 0.29588037224435E+03 0.29315000000011E+03 0.29315000000012E+03 0.29347440703358E+03 0.29587973215742E+03
 0.29315000000011E+03 0.29315000000012E+03 0.29347048806997E+03 0.29588037224435E+03 0.29315000000011E+03
 0.29315000000012E+03 0.29549153543118E+03 0.29315000000041E+03 0.29206006425900E+02 0.25789181318089E+02
 0.31843261720733E+02 0.18979845794891E+03 0.15779597991958E+03 0.27188898976431E+02 0.29483112976554E+02
 0.27254217021480E+02 0.14882013368866E+03 0.26847757935916E+02 0.29498969562200E+02 0.26917715500246E+02
 0.14883136007134E+03 0.27188898976435E+02 0.29483112976554E+02 0.27254217021484E+02 0.14882013368866E+03
 0.26847757935916E+02 0.29498969562200E+02 0.26917715500246E+02 0.14883136007134E+03 0.89178061482251E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33172480562818E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10015563723230E+00 0.00000000000000E+00 0.00000000000000E+00 0.10015563723230E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10602311188385E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10602311188385E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897205433760E-01 0.10913856832779E+00 0.31576657536478E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    300.03046755
 0.56180025603863E-01 0.30153032280749E+03 0.40746682771602E+03 0.40548298919663E+03 0.40480353060771E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18799530535892E+00 0.00000000000000E+00 -.12450665929585E+02
 0.64016436255870E-02 0.65277371143075E+00 0.12496790617998E+04 0.46862964817491E+03 0.12255395491442E+02
 0.45957733092906E+01 0.31078580599281E+03 0.29315000000712E+03 0.30822117746423E+03 0.32198994393928E+03
 0.29315000000033E+03 0.29315000000050E+03 0.30634705521689E+03 0.32195777858161E+03 0.29315000000027E+03
 0.29315000000050E+03 0.30822117746423E+03 0.32198994393928E+03 0.29315000000033E+03 0.29315000000050E+03
 0.30634705521689E+03 0.32195777858161E+03 0.29315000000027E+03 0.29315000000050E+03 0.35291065859891E+03
 0.29540820550918E+03 0.13199359523000E+04 0.12852500576048E+04 0.48642564782650E+03 0.10351552250933E+04
 0.54629744902766E+03 0.76085983243482E+03 0.73520579532383E+03 0.73122039938210E+03 0.13703271886222E+04
 0.67067342080411E+03 0.73430591179818E+03 0.67067342080411E+03 0.13697253843738E+04 0.76085983243482E+03
 0.73520579532383E+03 0.73122039938210E+03 0.13703271886222E+04 0.67067342080411E+03 0.73430591179818E+03
 0.67067342080411E+03 0.13697253843738E+04 0.13714987231303E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44192622270404E+03 0.10919647289035E+01
 0.10919647289035E+01 0.76512121200466E+00 0.28216842763768E+00 0.29364743254165E+03 0.31574597854428E+03
 0.31158897055852E+03 0.31133852729219E+03 0.22999999908328E+00 0.00000000000000E+00 0.21645599730598E+00
 0.00000000000000E+00 -.41392845694786E+01 0.99795096467406E-03 0.20744254581126E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38564895010875E+02 0.14461835629078E+02 0.29540754988866E+03 0.35293570327846E+03
 0.29350995147166E+03 0.29597994319962E+03 0.29315000000011E+03 0.29315000000014E+03 0.29350563213469E+03
 0.29598053636194E+03 0.29315000000011E+03 0.29315000000014E+03 0.29350995147166E+03 0.29597994319962E+03
 0.29315000000011E+03 0.29315000000014E+03 0.29350563213469E+03 0.29598053636194E+03 0.29315000000011E+03
 0.29315000000014E+03 0.29557896908308E+03 0.29315000000062E+03 0.30675124032791E+02 0.26793514824029E+02
 0.34672110710129E+02 0.19172021152482E+03 0.15687474026114E+03 0.29220459072637E+02 0.32115350660162E+02
 0.29375886299962E+02 0.15040127614792E+03 0.28861231989111E+02 0.32124221962595E+02 0.29022814749283E+02
 0.15040586832898E+03 0.29220459072642E+02 0.32115350660162E+02 0.29375886299968E+02 0.15040127614792E+03
 0.28861231989110E+02 0.32124221962595E+02 0.29022814749283E+02 0.15040586832898E+03 0.90726245725275E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33174538015967E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10038843624989E+00 0.00000000000000E+00 0.00000000000000E+00 0.10038843624989E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10634598121657E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10634598121657E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897129897611E-01 0.10914558273135E+00 0.31574597854428E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    310.01164789
 0.56395047270840E-01 0.30180120865411E+03 0.40766899127894E+03 0.40567885174375E+03 0.40499834847870E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18663904677420E+00 0.00000000000000E+00 -.12472748696493E+02
 0.63772352153958E-02 0.67356409719569E+00 0.12544621187386E+04 0.47042329452699E+03 0.11877117609604E+02
 0.44539191036015E+01 0.31128918220736E+03 0.29315000001105E+03 0.30862420010476E+03 0.32268959195354E+03
 0.29315000000047E+03 0.29315000000075E+03 0.30675212973212E+03 0.32265796242691E+03 0.29315000000038E+03
 0.29315000000075E+03 0.30862420010476E+03 0.32268959195354E+03 0.29315000000047E+03 0.29315000000075E+03
 0.30675212973212E+03 0.32265796242691E+03 0.29315000000038E+03 0.29315000000075E+03 0.35392757994938E+03
 0.29569039587591E+03 0.13273685378688E+04 0.12915434303331E+04 0.48653648843953E+03 0.10253173744864E+04
 0.53634820360465E+03 0.76563653065754E+03 0.73977197051679E+03 0.73530680750934E+03 0.13701419001475E+04
 0.67545174536247E+03 0.73891992298417E+03 0.67545174536247E+03 0.13695823819282E+04 0.76563653065754E+03
 0.73977197051679E+03 0.73530680750934E+03 0.13701419001475E+04 0.67545174536247E+03 0.73891992298417E+03
 0.67545174536247E+03 0.13695823819282E+04 0.13705696198606E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44211177233860E+03 0.10919648739094E+01
 0.10919648739094E+01 0.79506475302768E+00 0.26858245449744E+00 0.29377059166780E+03 0.31570720636542E+03
 0.31177934648618E+03 0.31154173737146E+03 0.22999999911185E+00 0.00000000000000E+00 0.21569057663881E+00
 0.00000000000000E+00 -.41529841958192E+01 0.99753245305662E-03 0.22561637081386E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35458419844011E+02 0.13296907441504E+02 0.29568971846623E+03 0.35395176607328E+03
 0.29354727106800E+03 0.29607841926749E+03 0.29315000000011E+03 0.29315000000015E+03 0.29354255567867E+03
 0.29607895601396E+03 0.29315000000011E+03 0.29315000000015E+03 0.29354727106800E+03 0.29607841926749E+03
 0.29315000000011E+03 0.29315000000015E+03 0.29354255567867E+03 0.29607895601396E+03 0.29315000000011E+03
 0.29315000000015E+03 0.29566478685402E+03 0.29315000000092E+03 0.31979331634840E+02 0.27624971958451E+02
 0.37429659435191E+02 0.19341816262408E+03 0.15580135489171E+03 0.31243783628251E+02 0.34670223362841E+02
 0.31532994020418E+02 0.15180217433244E+03 0.30868348502360E+02 0.34671660419525E+02 0.31165266942031E+02
 0.15179975019981E+03 0.31243783628253E+02 0.34670223362841E+02 0.31532994020420E+02 0.15180217433244E+03
 0.30868348502360E+02 0.34671660419525E+02 0.31165266942031E+02 0.15179975019981E+03 0.92122426256960E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33175101046306E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10054699504066E+00 0.00000000000000E+00 0.00000000000000E+00 0.10054699504066E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10663076608777E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10663076608777E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897166066320E-01 0.10915901252637E+00 0.31570720636542E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    320.00914043
 0.56541403502339E-01 0.30206842226314E+03 0.40787747557533E+03 0.40588327811615E+03 0.40520240743907E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18530484464785E+00 0.00000000000000E+00 -.12491923613064E+02
 0.63607274749196E-02 0.69398007505304E+00 0.12577177738779E+04 0.47164416520422E+03 0.11527708485562E+02
 0.43228906820858E+01 0.31178218392259E+03 0.29315000001683E+03 0.30902277676250E+03 0.32337063123587E+03
 0.29315000000070E+03 0.29315000000115E+03 0.30714983216642E+03 0.32333952866701E+03 0.29315000000055E+03
 0.29315000000115E+03 0.30902277676250E+03 0.32337063123587E+03 0.29315000000070E+03 0.29315000000115E+03
 0.30714983216642E+03 0.32333952866701E+03 0.29315000000055E+03 0.29315000000115E+03 0.35490480080613E+03
 0.29598787293707E+03 0.13343848407134E+04 0.12974462187334E+04 0.48655170244185E+03 0.10159777897892E+04
 0.52699332883517E+03 0.77014785260221E+03 0.74403554588128E+03 0.73912000562544E+03 0.13698877793954E+04
 0.67998480548111E+03 0.74322840079456E+03 0.67998480548111E+03 0.13693677364907E+04 0.77014785260221E+03
 0.74403554588128E+03 0.73912000562544E+03 0.13698877793954E+04 0.67998480548111E+03 0.74322840079456E+03
 0.67998480548111E+03 0.13693677364907E+04 0.13696315856824E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44230925946387E+03 0.10919649998209E+01
 0.10919649998209E+01 0.82505723065271E+00 0.25555867604955E+00 0.29391413187102E+03 0.31565490814809E+03
 0.31195087881365E+03 0.31172640111544E+03 0.22999999914255E+00 0.00000000000000E+00 0.21491328070796E+00
 0.00000000000000E+00 -.41626192619762E+01 0.99704518867442E-03 0.24387390204272E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32803838102359E+02 0.12301439288384E+02 0.29598717604607E+03 0.35492808110379E+03
 0.29358609293836E+03 0.29617448925004E+03 0.29315000000012E+03 0.29315000000018E+03 0.29358099078724E+03
 0.29617496082863E+03 0.29315000000012E+03 0.29315000000018E+03 0.29358609293836E+03 0.29617448925004E+03
 0.29315000000012E+03 0.29315000000018E+03 0.29358099078724E+03 0.29617496082863E+03 0.29315000000012E+03
 0.29315000000018E+03 0.29574841914180E+03 0.29315000000136E+03 0.33106498480091E+02 0.28282494299181E+02
 0.40091715644706E+02 0.19491548894503E+03 0.15462331472210E+03 0.33242918243921E+02 0.37126296205954E+02
 0.33717474963287E+02 0.15304035471385E+03 0.32853228300227E+02 0.37119973140105E+02 0.33337111644505E+02
 0.15303064902710E+03 0.33242918243924E+02 0.37126296205954E+02 0.33717474963292E+02 0.15304035471385E+03
 0.32853228300227E+02 0.37119973140105E+02 0.33337111644505E+02 0.15303064902710E+03 0.93372080294164E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33174555533026E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10065374150521E+00 0.00000000000000E+00 0.00000000000000E+00 0.10065374150521E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10687818048358E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10687818048358E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897281222162E-01 0.10917721620722E+00 0.31565490814809E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    330.00988789
 0.56640409818757E-01 0.30233207714868E+03 0.40809357516303E+03 0.40609678363284E+03 0.40541595276740E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18399350933537E+00 0.00000000000000E+00 -.12509323083640E+02
 0.63496086261738E-02 0.71400636715503E+00 0.12599201731935E+04 0.47247006494757E+03 0.11204381876700E+02
 0.42016432037624E+01 0.31226510914946E+03 0.29315000002523E+03 0.30941590582455E+03 0.32403417715808E+03
 0.29315000000105E+03 0.29315000000177E+03 0.30754033671347E+03 0.32400359151929E+03 0.29315000000082E+03
 0.29315000000176E+03 0.30941590582455E+03 0.32403417715808E+03 0.29315000000105E+03 0.29315000000177E+03
 0.30754033671347E+03 0.32400359151929E+03 0.29315000000082E+03 0.29315000000176E+03 0.35584548246326E+03
 0.29629979832355E+03 0.13410353266616E+04 0.13030087973321E+04 0.48650045121340E+03 0.10071349919095E+04
 0.51820203844007E+03 0.77443030223571E+03 0.74804433638459E+03 0.74270546550335E+03 0.13696126781372E+04
 0.68430143572268E+03 0.74727930157931E+03 0.68430143572268E+03 0.13691294503132E+04 0.77443030223571E+03
 0.74804433638459E+03 0.74270546550335E+03 0.13696126781372E+04 0.68430143572268E+03 0.74727930157931E+03
 0.68430143572268E+03 0.13691294503132E+04 0.13687220326974E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44251812791328E+03 0.10919651140741E+01
 0.10919651140741E+01 0.85505947302727E+00 0.24309744608352E+00 0.29407918747790E+03 0.31559310506636E+03
 0.31210645278569E+03 0.31189522287519E+03 0.22999999917361E+00 0.00000000000000E+00 0.21412588441295E+00
 0.00000000000000E+00 -.41693494635665E+01 0.99648551842911E-03 0.26218907257166E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30512331889093E+02 0.11442124458410E+02 0.29629908485654E+03 0.35586781877393E+03
 0.29362640149550E+03 0.29626816553679E+03 0.29315000000012E+03 0.29315000000024E+03 0.29362092413645E+03
 0.29626856371041E+03 0.29315000000012E+03 0.29315000000024E+03 0.29362640149550E+03 0.29626816553679E+03
 0.29315000000012E+03 0.29315000000024E+03 0.29362092413645E+03 0.29626856371041E+03 0.29315000000012E+03
 0.29315000000024E+03 0.29582989471976E+03 0.29315000000203E+03 0.34057172996948E+02 0.28774786731822E+02
 0.42654279204303E+02 0.19624475463875E+03 0.15337720403842E+03 0.35215375633711E+02 0.39481103852938E+02
 0.35935603873051E+02 0.15414142071854E+03 0.34813313199897E+02 0.39466778354823E+02 0.35544559896409E+02
 0.15412424740669E+03 0.35215375633713E+02 0.39481103852938E+02 0.35935603873054E+02 0.15414142071854E+03
 0.34813313199897E+02 0.39466778354823E+02 0.35544559896409E+02 0.15412424740669E+03 0.94490677949304E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33173247333209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10072561175245E+00 0.00000000000000E+00 0.00000000000000E+00 0.10072561175245E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10709159504186E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10709159504186E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897450247751E-01 0.10919877783085E+00 0.31559310506636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    340.01107692
 0.56707114548365E-01 0.30259215368658E+03 0.40831776761888E+03 0.40631930278557E+03 0.40563873336835E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18270460254954E+00 0.00000000000000E+00 -.12525690588561E+02
 0.63421391221494E-02 0.73364840101027E+00 0.12614040540455E+04 0.47302652026707E+03 0.10904405964742E+02
 0.40891522367784E+01 0.31273864497021E+03 0.29315000003719E+03 0.30980343668469E+03 0.32468179470378E+03
 0.29315000000157E+03 0.29315000000272E+03 0.30792413817228E+03 0.32465171538554E+03 0.29315000000122E+03
 0.29315000000270E+03 0.30980343668469E+03 0.32468179470378E+03 0.29315000000157E+03 0.29315000000272E+03
 0.30792413817228E+03 0.32465171538554E+03 0.29315000000122E+03 0.29315000000270E+03 0.35675324849289E+03
 0.29662557968191E+03 0.13473646249955E+04 0.13082729467191E+04 0.48640062408093E+03 0.99876264221178E+03
 0.50993001501045E+03 0.77851415819357E+03 0.75183641865709E+03 0.74609626859118E+03 0.13693435844801E+04
 0.68842759935607E+03 0.75111088464924E+03 0.68842759935607E+03 0.13688946970038E+04 0.77851415819357E+03
 0.75183641865709E+03 0.74609626859118E+03 0.13693435844801E+04 0.68842759935607E+03 0.75111088464924E+03
 0.68842759935607E+03 0.13688946970038E+04 0.13678588661529E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44273755150786E+03 0.10919652215510E+01
 0.10919652215510E+01 0.88506304011297E+00 0.23118377561177E+00 0.29426678709275E+03 0.31552517986732E+03
 0.31224878286394E+03 0.31205077410350E+03 0.22999999920486E+00 0.00000000000000E+00 0.21332931484049E+00
 0.00000000000000E+00 -.41739809780826E+01 0.99585019800598E-03 0.28055423529774E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28514985672948E+02 0.10693119627356E+02 0.29662485304225E+03 0.35677461364361E+03
 0.29366822862103E+03 0.29635957440249E+03 0.29315000000012E+03 0.29315000000031E+03 0.29366238917600E+03
 0.29635989149442E+03 0.29315000000012E+03 0.29315000000031E+03 0.29366822862103E+03 0.29635957440249E+03
 0.29315000000012E+03 0.29315000000031E+03 0.29366238917600E+03 0.29635989149442E+03 0.29315000000012E+03
 0.29315000000031E+03 0.29590933725420E+03 0.29315000000302E+03 0.34835046315572E+02 0.29112840271186E+02
 0.45117778264819E+02 0.19743624020742E+03 0.15209287305127E+03 0.37161311506232E+02 0.41736305934062E+02
 0.38196642976141E+02 0.15512937407963E+03 0.36748658248222E+02 0.41713809248436E+02 0.37796771627972E+02
 0.15510461603700E+03 0.37161311506237E+02 0.41736305934062E+02 0.38196642976148E+02 0.15512937407963E+03
 0.36748658248222E+02 0.41713809248436E+02 0.37796771627972E+02 0.15510461603700E+03 0.95494092910998E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33171465218747E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10077406053943E+00 0.00000000000000E+00 0.00000000000000E+00 0.10077406053943E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10727474366266E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10727474366266E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897655032814E-01 0.10922250925656E+00 0.31552517986732E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    350.00502786
 0.56751826719243E-01 0.30284849411682E+03 0.40854980103976E+03 0.40655022028827E+03 0.40587001269424E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18143840921874E+00 0.00000000000000E+00 -.12541477190129E+02
 0.63371419807182E-02 0.75290166225557E+00 0.12623987318481E+04 0.47339952444304E+03 0.10625557627318E+02
 0.39845841102442E+01 0.31320315621179E+03 0.29315000005397E+03 0.31018521801173E+03 0.32531449758248E+03
 0.29315000000235E+03 0.29315000000412E+03 0.30830147867499E+03 0.32528491331356E+03 0.29315000000182E+03
 0.29315000000409E+03 0.31018521801173E+03 0.32531449758248E+03 0.29315000000235E+03 0.29315000000412E+03
 0.30830147867499E+03 0.32528491331356E+03 0.29315000000182E+03 0.29315000000409E+03 0.35763070670865E+03
 0.29696443117014E+03 0.13534047084804E+04 0.13132685918515E+04 0.48626360454268E+03 0.99083285324188E+03
 0.50213793067649E+03 0.78242051013148E+03 0.75543813684030E+03 0.74931496077735E+03 0.13690946638969E+04
 0.69238169260461E+03 0.75474964908373E+03 0.69238169260461E+03 0.13686778006455E+04 0.78242051013148E+03
 0.75543813684030E+03 0.74931496077735E+03 0.13690946638969E+04 0.69238169260461E+03 0.75474964908373E+03
 0.69238169260461E+03 0.13686778006455E+04 0.13670497791796E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44296639450399E+03 0.10919653252134E+01
 0.10919653252134E+01 0.91504489292418E+00 0.21980723288003E+00 0.29447759608199E+03 0.31545400332687E+03
 0.31238015930536E+03 0.31219521754566E+03 0.22999999923663E+00 0.00000000000000E+00 0.21252483426985E+00
 0.00000000000000E+00 -.41770561347399E+01 0.99513726407237E-03 0.29895259859688E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26760095204215E+02 0.10035035701581E+02 0.29696369516865E+03 0.35765108578556E+03
 0.29371160127193E+03 0.29644880813855E+03 0.29315000000012E+03 0.29315000000042E+03 0.29370541426485E+03
 0.29644903712704E+03 0.29315000000012E+03 0.29315000000042E+03 0.29371160127193E+03 0.29644880813855E+03
 0.29315000000012E+03 0.29315000000042E+03 0.29370541426485E+03 0.29644903712704E+03 0.29315000000012E+03
 0.29315000000042E+03 0.29598683834059E+03 0.29315000000443E+03 0.35444871919317E+02 0.29308263491006E+02
 0.47482733623378E+02 0.19851582630436E+03 0.15079567901287E+03 0.39080369871414E+02 0.43893634832874E+02
 0.40509050777588E+02 0.15602482179760E+03 0.38658805519717E+02 0.43862871102353E+02 0.40102096420102E+02
 0.15599242967685E+03 0.39080369871422E+02 0.43893634832874E+02 0.40509050777600E+02 0.15602482179760E+03
 0.38658805519717E+02 0.43862871102353E+02 0.40102096420102E+02 0.15599242967685E+03 0.96396514120193E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33169448800120E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10080654875583E+00 0.00000000000000E+00 0.00000000000000E+00 0.10080654875583E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10743119067084E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10743119067084E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97897882499359E-01 0.10924740387802E+00 0.31545400332687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    360.00194499
 0.56781310841283E-01 0.30310142798598E+03 0.40878945735888E+03 0.40678908907621E+03 0.40610927168055E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18019304060308E+00 0.00000000000000E+00 -.12556683710211E+02
 0.63338508965585E-02 0.77179543033010E+00 0.12630546772654E+04 0.47364550397453E+03 0.10365441003685E+02
 0.38870403763817E+01 0.31365971708185E+03 0.29315000007721E+03 0.31056182283697E+03 0.32593419684048E+03
 0.29315000000349E+03 0.29315000000616E+03 0.30867317698842E+03 0.32590509653420E+03 0.29315000000269E+03
 0.29315000000613E+03 0.31056182283697E+03 0.32593419684048E+03 0.29315000000349E+03 0.29315000000616E+03
 0.30867317698842E+03 0.32590509653420E+03 0.29315000000269E+03 0.29315000000613E+03 0.35848158969525E+03
 0.29731616247596E+03 0.13591911516781E+04 0.13180280558565E+04 0.48609542119356E+03 0.98330039909574E+03
 0.49477450079621E+03 0.78617228690546E+03 0.75887499801874E+03 0.75238415501235E+03 0.13688713300404E+04
 0.69618496779131E+03 0.75822131184258E+03 0.69618496779131E+03 0.13684843798715E+04 0.78617228690546E+03
 0.75887499801874E+03 0.75238415501235E+03 0.13688713300404E+04 0.69618496779131E+03 0.75822131184258E+03
 0.69618496779131E+03 0.13684843798715E+04 0.13662942418308E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44320387124158E+03 0.10919654250667E+01
 0.10919654250667E+01 0.94503564433737E+00 0.20879961393970E+00 0.29471244393570E+03 0.31537960348723E+03
 0.31250274019684E+03 0.31233075737018E+03 0.22999999924414E+00 0.00000000000000E+00 0.21171427601814E+00
 0.00000000000000E+00 -.41786113311292E+01 0.99434425259616E-03 0.31736660838611E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25207440822719E+02 0.94527903085198E+01 0.29731542123770E+03 0.35850097904794E+03
 0.29375662212220E+03 0.29653609155017E+03 0.29315000000012E+03 0.29315000000058E+03 0.29375010320856E+03
 0.29653622594286E+03 0.29315000000012E+03 0.29315000000058E+03 0.29375662212220E+03 0.29653609155017E+03
 0.29315000000012E+03 0.29315000000058E+03 0.29375010320856E+03 0.29653622594286E+03 0.29315000000012E+03
 0.29315000000058E+03 0.29606252640937E+03 0.29315000000643E+03 0.35889236485068E+02 0.29369084023343E+02
 0.49748817765963E+02 0.19946040529110E+03 0.14946284343631E+03 0.40973834810860E+02 0.45952794206806E+02
 0.42883763979732E+02 0.15683465512985E+03 0.40545151034035E+02 0.45913708087233E+02 0.42471577753625E+02
 0.15679461594101E+03 0.40973834810869E+02 0.45952794206806E+02 0.42883763979746E+02 0.15683465512985E+03
 0.40545151034035E+02 0.45913708087233E+02 0.42471577753625E+02 0.15679461594101E+03 0.97199872523099E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33161726374170E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10082360220347E+00 0.00000000000000E+00 0.00000000000000E+00 0.10082360220347E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10756149602886E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10756149602886E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97898133942141E-01 0.10927345494251E+00 0.31537960348723E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    370.00933677
 0.56798957125909E-01 0.30335157414723E+03 0.40903647285435E+03 0.40703554217751E+03 0.40635611617297E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17896708072333E+00 0.00000000000000E+00 -.12571644363071E+02
 0.63318826071098E-02 0.79035175076798E+00 0.12634473025474E+04 0.47379273845529E+03 0.10122075382545E+02
 0.37957782684544E+01 0.31410929137243E+03 0.29315000010896E+03 0.31093382463621E+03 0.32654245701929E+03
 0.29315000000511E+03 0.29315000000909E+03 0.30903998985477E+03 0.32651383028851E+03 0.29315000000394E+03
 0.29315000000904E+03 0.31093382463621E+03 0.32654245701929E+03 0.29315000000511E+03 0.29315000000909E+03
 0.30903998985477E+03 0.32651383028851E+03 0.29315000000394E+03 0.29315000000904E+03 0.35930868098656E+03
 0.29768043557309E+03 0.13647534244432E+04 0.13225795518258E+04 0.48590352873956E+03 0.97613318052667E+03
 0.48780013414341E+03 0.78978820678452E+03 0.76216774749620E+03 0.75532340657994E+03 0.13686770915790E+04
 0.69985493316577E+03 0.76154679624931E+03 0.69985493316577E+03 0.13683181121785E+04 0.78978820678452E+03
 0.76216774749620E+03 0.75532340657994E+03 0.13686770915790E+04 0.69985493316577E+03 0.76154679624931E+03
 0.69985493316577E+03 0.13683181121785E+04 0.13655931727814E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44344945003750E+03 0.10919655233055E+01
 0.10919655233055E+01 0.97505781965907E+00 0.19808927470759E+00 0.29497212664627E+03 0.31530359412718E+03
 0.31261863036249E+03 0.31245945315967E+03 0.22999999926607E+00 0.00000000000000E+00 0.21089842068466E+00
 0.00000000000000E+00 -.41790528242826E+01 0.99346886382284E-03 0.33579117667433E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23824330583167E+02 0.89341239686876E+01 0.29767969362753E+03 0.35932708048147E+03
 0.29380344410526E+03 0.29662154601714E+03 0.29315000000012E+03 0.29315000000080E+03 0.29379661040189E+03
 0.29662157971179E+03 0.29315000000012E+03 0.29315000000081E+03 0.29380344410526E+03 0.29662154601714E+03
 0.29315000000012E+03 0.29315000000080E+03 0.29379661040189E+03 0.29662157971179E+03 0.29315000000012E+03
 0.29315000000081E+03 0.29613657990508E+03 0.29315000000919E+03 0.36174407584335E+02 0.29307293227672E+02
 0.51919391678778E+02 0.20031523018791E+03 0.14813624155074E+03 0.42843934072571E+02 0.47918255461358E+02
 0.45333018704345E+02 0.15755835061697E+03 0.42409934531592E+02 0.47870832436837E+02 0.44917456071633E+02
 0.15751068929757E+03 0.42843934072582E+02 0.47918255461358E+02 0.45333018704362E+02 0.15755835061697E+03
 0.42409934531592E+02 0.47870832436837E+02 0.44917456071633E+02 0.15751068929757E+03 0.97919234341680E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33155275623222E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10083058734370E+00 0.00000000000000E+00 0.00000000000000E+00 0.10083058734370E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10766938291053E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10766938291053E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97898401079268E-01 0.10930009496292E+00 0.31530359412718E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    380.01074954
 0.56808435027067E-01 0.30359841591226E+03 0.40928976294266E+03 0.40728837626909E+03 0.40660930611563E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17776208389203E+00 0.00000000000000E+00 -.12586486020982E+02
 0.63308256954439E-02 0.80854826651849E+00 0.12636582311463E+04 0.47387183667985E+03 0.98942763608252E+01
 0.37103536353094E+01 0.31455157919120E+03 0.29315000015176E+03 0.31130079716547E+03 0.32713919818563E+03
 0.29315000000739E+03 0.29315000001323E+03 0.30940162318732E+03 0.32711103360088E+03 0.29315000000570E+03
 0.29315000001316E+03 0.31130079716547E+03 0.32713919818563E+03 0.29315000000739E+03 0.29315000001323E+03
 0.30940162318732E+03 0.32711103360088E+03 0.29315000000570E+03 0.29315000001316E+03 0.36011277818275E+03
 0.29805614662548E+03 0.13701024446490E+04 0.13269332423345E+04 0.48568962124267E+03 0.96930795416301E+03
 0.48118988481412E+03 0.79327499834867E+03 0.76532459993737E+03 0.75813955505736E+03 0.13685111651455E+04
 0.70339734314213E+03 0.76473440822185E+03 0.70339734314213E+03 0.13681783133759E+04 0.79327499834867E+03
 0.76532459993737E+03 0.75813955505736E+03 0.13685111651455E+04 0.70339734314213E+03 0.76473440822185E+03
 0.70339734314213E+03 0.13681783133759E+04 0.13649425777831E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44370174543349E+03 0.10919656207630E+01
 0.10919656207630E+01 0.10050620579676E+01 0.18769969937336E+00 0.29525654725441E+03 0.31522846589963E+03
 0.31272931714922E+03 0.31258266319218E+03 0.22999999929008E+00 0.00000000000000E+00 0.21007892203614E+00
 0.00000000000000E+00 -.41786308289837E+01 0.99251185948170E-03 0.35419400483602E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22586491839984E+02 0.84699344399941E+01 0.29805540839376E+03 0.36013020299176E+03
 0.29385207537916E+03 0.29670516110750E+03 0.29315000000013E+03 0.29315000000113E+03 0.29384494565603E+03
 0.29670508881386E+03 0.29315000000013E+03 0.29315000000113E+03 0.29385207537916E+03 0.29670516110750E+03
 0.29315000000013E+03 0.29315000000113E+03 0.29384494565603E+03 0.29670508881386E+03 0.29315000000013E+03
 0.29315000000113E+03 0.29620900798161E+03 0.29315000001298E+03 0.36307899841862E+02 0.29135568525483E+02
 0.53996534552618E+02 0.20110013957296E+03 0.14683362234757E+03 0.44690060181992E+02 0.49793277953240E+02
 0.47864495404157E+02 0.15821879194740E+03 0.44252460583231E+02 0.49737566669145E+02 0.47447310702191E+02
 0.15816359055684E+03 0.44690060182003E+02 0.49793277953240E+02 0.47864495404175E+02 0.15821879194740E+03
 0.44252460583231E+02 0.49737566669145E+02 0.47447310702191E+02 0.15816359055684E+03 0.98567454171135E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33149201277341E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10083051374138E+00 0.00000000000000E+00 0.00000000000000E+00 0.10083051374138E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10775828404747E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10775828404747E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97898675681698E-01 0.10932645150711E+00 0.31522846589963E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    390.00938759
 0.56812228984898E-01 0.30384216908406E+03 0.40954874893521E+03 0.40754694012863E+03 0.40686817206725E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17657720796809E+00 0.00000000000000E+00 -.12601354996576E+02
 0.63304024058325E-02 0.82639902102777E+00 0.12637427271020E+04 0.47390352266326E+03 0.96805535781621E+01
 0.36302075918108E+01 0.31498717042473E+03 0.29315000020881E+03 0.31166308707362E+03 0.32772543613514E+03
 0.29315000001056E+03 0.29315000001900E+03 0.30975852380668E+03 0.32769772255192E+03 0.29315000000815E+03
 0.29315000001890E+03 0.31166308707362E+03 0.32772543613514E+03 0.29315000001056E+03 0.29315000001900E+03
 0.30975852380668E+03 0.32769772255192E+03 0.29315000000815E+03 0.29315000001890E+03 0.36089590589429E+03
 0.29844286268373E+03 0.13752576684549E+04 0.13311070271152E+04 0.48545616807335E+03 0.96279399704556E+03
 0.47491054813184E+03 0.79664490828939E+03 0.76835844414016E+03 0.76084458551267E+03 0.13683717299339E+04
 0.70682374246526E+03 0.76779718319910E+03 0.70682374246526E+03 0.13680633043867E+04 0.79664490828939E+03
 0.76835844414016E+03 0.76084458551267E+03 0.13683717299339E+04 0.70682374246526E+03 0.76779718319910E+03
 0.70682374246526E+03 0.13680633043867E+04 0.13643381456060E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44395998624447E+03 0.10919657183999E+01
 0.10919657183999E+01 0.10350579721174E+01 0.17762620303999E+00 0.29556587002109E+03 0.31515601806823E+03
 0.31283620232516E+03 0.31270170781709E+03 0.22999999931701E+00 0.00000000000000E+00 0.20925586178558E+00
 0.00000000000000E+00 -.41775765358210E+01 0.10140394514687E-02 0.37257844274036E+00 0.78892394062311E+04
 0.29584647773367E+04 0.21471988398360E+02 0.80519956493849E+01 0.29844213242783E+03 0.36091237527238E+03
 0.29390264950820E+03 0.29678711631656E+03 0.29315000000013E+03 0.29315000000157E+03 0.29389524323859E+03
 0.29678693330912E+03 0.29315000000013E+03 0.29315000000157E+03 0.29390264950819E+03 0.29678711631656E+03
 0.29315000000013E+03 0.29315000000157E+03 0.29389524323859E+03 0.29678693330912E+03 0.29315000000013E+03
 0.29315000000157E+03 0.29627997923963E+03 0.29315000001810E+03 0.36297851954269E+02 0.28865637040296E+02
 0.55986501167581E+02 0.20183603966305E+03 0.14556960598963E+03 0.46514745468130E+02 0.51584784689928E+02
 0.50489596798828E+02 0.15883365367458E+03 0.46075179175405E+02 0.51520878397698E+02 0.50072452575027E+02
 0.15877103474939E+03 0.46514745468141E+02 0.51584784689928E+02 0.50489596798845E+02 0.15883365367458E+03
 0.46075179175405E+02 0.51520878397698E+02 0.50072452575027E+02 0.15877103474939E+03 0.99156997712583E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33143649486039E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10082617208264E+00 0.00000000000000E+00 0.00000000000000E+00 0.10082617208264E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10783141470798E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10783141470798E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97898951265666E-01 0.10935189235267E+00 0.31515601806823E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    400.00347963
 0.56812239635775E-01 0.30408289793935E+03 0.40981273715464E+03 0.40781048750059E+03 0.40713195562165E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17541224757707E+00 0.00000000000000E+00 -.12616340033377E+02
 0.63304006972053E-02 0.84390852994342E+00 0.12637430681966E+04 0.47390365057372E+03 0.94797003657924E+01
 0.35548876371722E+01 0.31541638275716E+03 0.29315000028403E+03 0.31202084125592E+03 0.32830178185492E+03
 0.29315000001490E+03 0.29315000002697E+03 0.31011091836031E+03 0.32827450820486E+03 0.29315000001151E+03
 0.29315000002683E+03 0.31202084125592E+03 0.32830178185492E+03 0.29315000001490E+03 0.29315000002697E+03
 0.31011091836031E+03 0.32827450820486E+03 0.29315000001151E+03 0.29315000002683E+03 0.36165948402784E+03
 0.29883996677391E+03 0.13802334081819E+04 0.13351142290542E+04 0.48520481987409E+03 0.95656601353698E+03
 0.46893516956352E+03 0.79990684751422E+03 0.77127866922644E+03 0.76344733297287E+03 0.13682562649809E+04
 0.71014246667222E+03 0.77074463084795E+03 0.71014246667222E+03 0.13679706821834E+04 0.79990684751422E+03
 0.77127866922644E+03 0.76344733297287E+03 0.13682562649809E+04 0.71014246667222E+03 0.77074463084795E+03
 0.71014246667222E+03 0.13679706821834E+04 0.13637753958040E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44422333565303E+03 0.10919658167989E+01
 0.10919658167989E+01 0.10650402482540E+01 0.16786885500601E+00 0.29589993728947E+03 0.31508770199056E+03
 0.31294034993023E+03 0.31281757825439E+03 0.22999999934759E+00 0.00000000000000E+00 0.20842967321083E+00
 0.00000000000000E+00 -.41760634768450E+01 0.10729802447298E-02 0.39093968093124E+00 0.74558688655213E+04
 0.27959508245705E+04 0.20463514936482E+02 0.76738181011808E+01 0.29883924862301E+03 0.36167502219945E+03
 0.29395528199127E+03 0.29686755180244E+03 0.29315000000014E+03 0.29315000000220E+03 0.29394761932766E+03
 0.29686725398381E+03 0.29315000000014E+03 0.29315000000220E+03 0.29395528199127E+03 0.29686755180244E+03
 0.29315000000014E+03 0.29315000000220E+03 0.29394761932766E+03 0.29686725398381E+03 0.29315000000014E+03
 0.29315000000220E+03 0.29634962339776E+03 0.29315000002495E+03 0.36152468417792E+02 0.28508569291904E+02
 0.57894541983343E+02 0.20253995911460E+03 0.14435594442134E+03 0.48319616379642E+02 0.53298658072932E+02
 0.53218135915466E+02 0.15941740243738E+03 0.47879638871051E+02 0.53226693251953E+02 0.52802606560748E+02
 0.15934752727182E+03 0.48319616379658E+02 0.53298658072932E+02 0.53218135915492E+02 0.15941740243738E+03
 0.47879638871051E+02 0.53226693251953E+02 0.52802606560747E+02 0.15934752727182E+03 0.99698330708368E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33138735853943E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10081956784063E+00 0.00000000000000E+00 0.00000000000000E+00 0.10081956784063E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10789149027074E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10789149027074E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97899222740553E-01 0.10937590653405E+00 0.31508770199056E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    410.00039898
 0.56809929935139E-01 0.30432087472272E+03 0.41008130807470E+03 0.40807856047182E+03 0.40740019144877E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17426597466825E+00 0.00000000000000E+00 -.12631517243754E+02
 0.63306575402781E-02 0.86109669532894E+00 0.12636917964842E+04 0.47388442368156E+03 0.92904781116876E+01
 0.34839292918828E+01 0.31583988009266E+03 0.29315000038226E+03 0.31237451581218E+03 0.32886927666232E+03
 0.29315000002079E+03 0.29315000003781E+03 0.31045932130040E+03 0.32884243238104E+03 0.29315000001607E+03
 0.29315000003762E+03 0.31237451581218E+03 0.32886927666232E+03 0.29315000002079E+03 0.29315000003781E+03
 0.31045932130040E+03 0.32884243238104E+03 0.29315000001607E+03 0.29315000003762E+03 0.36240543674052E+03
 0.29924722440534E+03 0.13850464558033E+04 0.13389698989408E+04 0.48493648225734E+03 0.95059551220557E+03
 0.46323434753695E+03 0.80307138380912E+03 0.77409581852419E+03 0.76595765257213E+03 0.13681618486621E+04
 0.71336367171012E+03 0.77358742912029E+03 0.71336367171012E+03 0.13678976531098E+04 0.80307138380912E+03
 0.77409581852419E+03 0.76595765257213E+03 0.13681618486621E+04 0.71336367171012E+03 0.77358742912029E+03
 0.71336367171012E+03 0.13678976531098E+04 0.13632493371876E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44449126693410E+03 0.10919659164599E+01
 0.10919659164599E+01 0.10950310062961E+01 0.15841860230195E+00 0.29625865474211E+03 0.31502458890101E+03
 0.31304267352745E+03 0.31293113771309E+03 0.22999999938310E+00 0.00000000000000E+00 0.20759998402533E+00
 0.00000000000000E+00 -.41742312146560E+01 0.11369873893309E-02 0.40929005709777E+00 0.70361378455638E+04
 0.26385516920864E+04 0.19546040421131E+02 0.73297651579242E+01 0.29924652229910E+03 0.36242007103414E+03
 0.29401014736968E+03 0.29694667806765E+03 0.29315000000016E+03 0.29315000000306E+03 0.29400224874015E+03
 0.29694626183640E+03 0.29315000000016E+03 0.29315000000306E+03 0.29401014736968E+03 0.29694667806765E+03
 0.29315000000016E+03 0.29315000000306E+03 0.29400224874015E+03 0.29694626183640E+03 0.29315000000016E+03
 0.29315000000306E+03 0.29641812826050E+03 0.29315000003401E+03 0.35880200284271E+02 0.28074313189836E+02
 0.59727583485835E+02 0.20322664894394E+03 0.14320042754068E+03 0.50108296520941E+02 0.54942124899691E+02
 0.56062182817514E+02 0.15998259806506E+03 0.49669384686630E+02 0.54862266422507E+02 0.55649755865437E+02
 0.15990565289196E+03 0.50108296520950E+02 0.54942124899690E+02 0.56062182817528E+02 0.15998259806506E+03
 0.49669384686630E+02 0.54862266422507E+02 0.55649755865437E+02 0.15990565289195E+03 0.10020093979966E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33134544191347E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10081225907236E+00 0.00000000000000E+00 0.00000000000000E+00 0.10081225907236E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10794097169265E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10794097169265E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97899486268132E-01 0.10939811573119E+00 0.31502458890101E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    420.00812953
 0.56806046399133E-01 0.30456896858351E+03 0.41035417114585E+03 0.40835109143749E+03 0.40767300005395E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17313716803436E+00 0.00000000000000E+00 -.12645864325723E+02
 0.63310897960287E-02 0.87798308947702E+00 0.12636055178080E+04 0.47385206917801E+03 0.91117928077240E+01
 0.34169223028965E+01 0.31625840373263E+03 0.29315000050945E+03 0.31272472022531E+03 0.32942889687657E+03
 0.29315000002872E+03 0.29315000005246E+03 0.31076371158906E+03 0.32940247192002E+03 0.29315000002223E+03
 0.29315000005220E+03 0.31272472022531E+03 0.32942889687657E+03 0.29315000002872E+03 0.29315000005246E+03
 0.31076371158906E+03 0.32940247192002E+03 0.29315000002223E+03 0.29315000005220E+03 0.36313553861794E+03
 0.29966445340401E+03 0.13897100905224E+04 0.13427535927991E+04 0.48465228029497E+03 0.94485821227758E+03
 0.45778267058114E+03 0.80614397585867E+03 0.77681989535398E+03 0.76845224478117E+03 0.13680870999565E+04
 0.71674511334799E+03 0.77633570562321E+03 0.71674511334799E+03 0.13678429539721E+04 0.80614397585867E+03
 0.77681989535398E+03 0.76845224478117E+03 0.13680870999565E+04 0.71674511334798E+03 0.77633570562321E+03
 0.71674511334798E+03 0.13678429539721E+04 0.13627740483758E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44476378606577E+03 0.10919660106698E+01
 0.10919660106698E+01 0.11250541979413E+01 0.14926637541054E+00 0.29664181095362E+03 0.31496749406063E+03
 0.31314388853109E+03 0.31304306184144E+03 0.22999999939961E+00 0.00000000000000E+00 0.20676632602993E+00
 0.00000000000000E+00 -.41711219475494E+01 0.12067013804365E-02 0.42764334627077E+00 0.66296435304533E+04
 0.24861163239200E+04 0.18707177534184E+02 0.70151915753191E+01 0.29966377110477E+03 0.36314929850336E+03
 0.29406743327894E+03 0.29702470007738E+03 0.29315000000020E+03 0.29315000000423E+03 0.29405931932433E+03
 0.29702416230271E+03 0.29315000000020E+03 0.29315000000423E+03 0.29406743327893E+03 0.29702470007738E+03
 0.29315000000020E+03 0.29315000000423E+03 0.29405931932433E+03 0.29702416230271E+03 0.29315000000020E+03
 0.29315000000423E+03 0.29648567504913E+03 0.29315000004589E+03 0.35488323579167E+02 0.27570767925427E+02
 0.61492118266797E+02 0.20390839764896E+03 0.14210881879083E+03 0.51883829749848E+02 0.56521930003611E+02
 0.59033114893995E+02 0.16053981416190E+03 0.51447395928658E+02 0.56434368706559E+02 0.58625205509043E+02
 0.16045600797634E+03 0.51883829749857E+02 0.56521930003611E+02 0.59033114894011E+02 0.16053981416190E+03
 0.51447395928658E+02 0.56434368706559E+02 0.58625205509043E+02 0.16045600797634E+03 0.10067270961145E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33131156422538E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078888760696E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078888760696E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10798490185340E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10798490185340E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97899756338551E-01 0.10941825176952E+00 0.31496749406063E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    430.00526221
 0.56800547231378E-01 0.30482161572436E+03 0.41063043263644E+03 0.40862709973560E+03 0.40794932909687E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17202791194872E+00 0.00000000000000E+00 -.12661397466277E+02
 0.63317021979235E-02 0.89453750046515E+00 0.12634833019506E+04 0.47380623823146E+03 0.89431689513744E+01
 0.33536883567654E+01 0.31667149074209E+03 0.29315000067232E+03 0.31307109375136E+03 0.32997994106561E+03
 0.29315000003926E+03 0.29315000007199E+03 0.31104228141267E+03 0.32995392471794E+03 0.29315000003042E+03
 0.29315000007163E+03 0.31307109375136E+03 0.32997994106561E+03 0.29315000003926E+03 0.29315000007199E+03
 0.31104228141267E+03 0.32995392471794E+03 0.29315000003042E+03 0.29315000007163E+03 0.36384935950385E+03
 0.30009023539118E+03 0.13942276407591E+04 0.13464418638189E+04 0.48435568870439E+03 0.93935078852020E+03
 0.45257332137229E+03 0.80912678230636E+03 0.77945416630682E+03 0.77090374548458E+03 0.13680336184259E+04
 0.72017560883975E+03 0.77899277054864E+03 0.69326843027552E+03 0.13678082333191E+04 0.80912678230636E+03
 0.77945416630682E+03 0.77090374548458E+03 0.13680336184259E+04 0.72017560883975E+03 0.77899277054864E+03
 0.69326843027552E+03 0.13678082333191E+04 0.13622211003697E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44503987488757E+03 0.10919661126680E+01
 0.10919661126680E+01 0.11550455959889E+01 0.14042949461858E+00 0.29704783849589E+03 0.31491716610739E+03
 0.31324424567695E+03 0.31315354123254E+03 0.22999999940796E+00 0.00000000000000E+00 0.20593064701558E+00
 0.00000000000000E+00 -.41687304618252E+01 0.12826360321565E-02 0.44595989362744E+00 0.62371552018150E+04
 0.23389332006806E+04 0.17938832873351E+02 0.67270623275065E+01 0.30008957644791E+03 0.36386227819520E+03
 0.29412715820636E+03 0.29710158841523E+03 0.29315000000026E+03 0.29315000000581E+03 0.29411885029265E+03
 0.29710092677798E+03 0.29315000000026E+03 0.29315000000581E+03 0.29412715820636E+03 0.29710158841523E+03
 0.29315000000026E+03 0.29315000000581E+03 0.29411885029265E+03 0.29710092677798E+03 0.29315000000026E+03
 0.29315000000581E+03 0.29655224011174E+03 0.29315000006128E+03 0.34985083958438E+02 0.27006082428767E+02
 0.63189233785250E+02 0.20459231726901E+03 0.14108713731484E+03 0.53643753819604E+02 0.58039879900183E+02
 0.62132521503175E+02 0.16109520842935E+03 0.53211139982655E+02 0.57944851495212E+02 0.61730461081876E+02
 0.16100478958608E+03 0.53643753819609E+02 0.58039879900183E+02 0.62132521503184E+02 0.16109520842935E+03
 0.53211139982655E+02 0.57944851495212E+02 0.61730461081877E+02 0.16100478958608E+03 0.10111837107931E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33128574224775E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078045274983E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078045274983E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10801671144857E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10801671144857E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97899998876245E-01 0.10943601196297E+00 0.31491716610739E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    440.18711471
 0.56795579985100E-01 0.30507020509165E+03 0.41091491895074E+03 0.40891108164674E+03 0.40823345933618E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17091670930368E+00 0.00000000000000E+00 -.12678247985315E+02
 0.63322553990243E-02 0.91108152723177E+00 0.12633729210026E+04 0.47376484537598E+03 0.87807729175535E+01
 0.32927898440826E+01 0.31708707536108E+03 0.29315000088384E+03 0.31342010692439E+03 0.33053340779872E+03
 0.29315000005341E+03 0.29315000009833E+03 0.31132525408780E+03 0.33050779685354E+03 0.29315000004144E+03
 0.29315000009784E+03 0.31342010692439E+03 0.33053340779872E+03 0.29315000005341E+03 0.29315000009833E+03
 0.31132525408780E+03 0.33050779685354E+03 0.29315000004144E+03 0.29315000009784E+03 0.36456180277054E+03
 0.30053265806394E+03 0.13986935403283E+04 0.13500400473871E+04 0.48403539395012E+03 0.93394090410002E+03
 0.44748533318015E+03 0.81208418542838E+03 0.78205170494497E+03 0.77328975909164E+03 0.13679945077643E+04
 0.72357278846915E+03 0.78161220033739E+03 0.69648653461349E+03 0.13677870221248E+04 0.81208418542838E+03
 0.78205170494497E+03 0.77328975909164E+03 0.13679945077643E+04 0.72357278846915E+03 0.78161220033739E+03
 0.69648653461349E+03 0.13677870221248E+04 0.13617989922849E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44532380594918E+03 0.10919662233168E+01
 0.10919662233168E+01 0.11855911534881E+01 0.13174095389451E+00 0.29748434804919E+03 0.31487323786609E+03
 0.31334601857833E+03 0.31326500415677E+03 0.22999999940811E+00 0.00000000000000E+00 0.20507667888224E+00
 0.00000000000000E+00 -.41670089392157E+01 0.13672279808097E-02 0.46459718659574E+00 0.58512553226581E+04
 0.21942207459968E+04 0.17219217487343E+02 0.64572065577538E+01 0.30053202629539E+03 0.36457390028272E+03
 0.29419060037778E+03 0.29717894199117E+03 0.29315000000031E+03 0.29315000000789E+03 0.29418211664056E+03
 0.29717815239367E+03 0.29315000000031E+03 0.29315000000790E+03 0.29419060037778E+03 0.29717894199117E+03
 0.29315000000031E+03 0.29315000000789E+03 0.29418211664056E+03 0.29717815239367E+03 0.29315000000031E+03
 0.29315000000790E+03 0.29661920311914E+03 0.29315000008152E+03 0.34364454349967E+02 0.26372895387252E+02
 0.64854959674660E+02 0.20529972434001E+03 0.14012048986698E+03 0.55423426090481E+02 0.59528927327982E+02
 0.65432472537009E+02 0.16166655483564E+03 0.54996010502690E+02 0.59426554610797E+02 0.65037660445149E+02
 0.16156967112933E+03 0.55423426090484E+02 0.59528927327982E+02 0.65432472537014E+02 0.16166655483564E+03
 0.54996010502690E+02 0.59426554610797E+02 0.65037660445149E+02 0.16156967112933E+03 0.10155103523107E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33126831618175E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078525019193E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078525019193E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10804416053840E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10804416053840E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900217536051E-01 0.10945152579682E+00 0.31487323786609E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    450.00713704
 0.56791542651086E-01 0.30529869465104E+03 0.41119178166615E+03 0.40918717107693E+03 0.40850948249893E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16986244387194E+00 0.00000000000000E+00 -.12694081947353E+02
 0.63327050202718E-02 0.92674106916233E+00 0.12632832216866E+04 0.47373120813248E+03 0.86324004257534E+01
 0.32371501596575E+01 0.31748323798654E+03 0.29315000114099E+03 0.31375322227339E+03 0.33106009453735E+03
 0.29315000007120E+03 0.29315000013155E+03 0.31161181869415E+03 0.33103486558941E+03 0.29315000005530E+03
 0.29315000013091E+03 0.31375322227339E+03 0.33106009453735E+03 0.29315000007120E+03 0.29315000013155E+03
 0.31161181869415E+03 0.33103486558941E+03 0.29315000005530E+03 0.29315000013091E+03 0.36523483506494E+03
 0.30096709677523E+03 0.14028792951062E+04 0.13533491402039E+04 0.48371874552654E+03 0.92891851937775E+03
 0.44278118012358E+03 0.81486564056516E+03 0.78447977262848E+03 0.77547334683541E+03 0.13679686264706E+04
 0.72667346882819E+03 0.78406019596919E+03 0.69927990518331E+03 0.13677773169154E+04 0.81486564056516E+03
 0.78447977262848E+03 0.77547334683541E+03 0.13679686264706E+04 0.72667346882819E+03 0.78406019596919E+03
 0.69927990518331E+03 0.13677773169154E+04 0.13614044534338E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44559969120157E+03 0.10919663272904E+01
 0.10919663272904E+01 0.12150512204650E+01 0.12365628407695E+00 0.29792627605494E+03 0.31483816317040E+03
 0.31344398909211E+03 0.31337171072536E+03 0.22999999944774E+00 0.00000000000000E+00 0.20425046487454E+00
 0.00000000000000E+00 -.41646785199878E+01 0.14566175025868E-02 0.48255517494980E+00 0.54921762135858E+04
 0.20595660800947E+04 0.16578415102133E+02 0.62169056632997E+01 0.30096649425159E+03 0.36524617370375E+03
 0.29425468205914E+03 0.29725276244865E+03 0.29315000000038E+03 0.29315000001053E+03 0.29424605013959E+03
 0.29725184780013E+03 0.29315000000037E+03 0.29315000001053E+03 0.29425468205913E+03 0.29725276244865E+03
 0.29315000000038E+03 0.29315000001053E+03 0.29424605013959E+03 0.29725184780013E+03 0.29315000000037E+03
 0.29315000001053E+03 0.29668311661137E+03 0.29315000010638E+03 0.33670140011805E+02 0.25712866868918E+02
 0.66406156336351E+02 0.20599820235815E+03 0.13926001524012E+03 0.57128279473066E+02 0.60915308910255E+02
 0.68757353554499E+02 0.16222823485919E+03 0.56707098096296E+02 0.60806111900981E+02 0.68370726875401E+02
 0.16212538346077E+03 0.57128279473068E+02 0.60915308910255E+02 0.68757353554503E+02 0.16222823485919E+03
 0.56707098096296E+02 0.60806111900982E+02 0.68370726875401E+02 0.16212538346077E+03 0.10195232528640E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33126009403011E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078227931905E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078227931905E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10806654929174E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10806654929174E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900423000905E-01 0.10946395154821E+00 0.31483816317040E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    460.01682795
 0.56788866858848E-01 0.30552590742166E+03 0.41147602992905E+03 0.40947043412880E+03 0.40879255006151E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16880518615259E+00 0.00000000000000E+00 -.12710646056296E+02
 0.63330028524097E-02 0.94240882297189E+00 0.12632238112692E+04 0.47370892922597E+03 0.84888848714001E+01
 0.31833318267750E+01 0.31788245941505E+03 0.29315000146807E+03 0.31408926816773E+03 0.33159017732510E+03
 0.29315000009457E+03 0.29315000017534E+03 0.31190795001696E+03 0.33156532818308E+03 0.29315000007355E+03
 0.29315000017450E+03 0.31408926816773E+03 0.33159017732510E+03 0.29315000009457E+03 0.29315000017534E+03
 0.31190795001696E+03 0.33156532818308E+03 0.29315000007355E+03 0.29315000017450E+03 0.36590824381566E+03
 0.30141727629189E+03 0.14070300077491E+04 0.13565964872302E+04 0.48337925587249E+03 0.92396247004728E+03
 0.43816631789543E+03 0.81763279524279E+03 0.78688001857291E+03 0.77761673624736E+03 0.13679496757299E+04
 0.72972045276019E+03 0.78647963457272E+03 0.70196055148878E+03 0.13677738366545E+04 0.81763279524279E+03
 0.78688001857291E+03 0.77761673624736E+03 0.13679496757299E+04 0.72972045276019E+03 0.78647963457272E+03
 0.70196055148878E+03 0.13677738366545E+04 0.13610139706018E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44588260704895E+03 0.10919664360585E+01
 0.10919664360585E+01 0.12450802932072E+01 0.11571113805644E+00 0.29839665800337E+03 0.31480993500026E+03
 0.31354380235990E+03 0.31347980277611E+03 0.22999999952934E+00 0.00000000000000E+00 0.20340576954430E+00
 0.00000000000000E+00 -.41625054904496E+01 0.15566340468401E-02 0.50084322247575E+00 0.51392939890012E+04
 0.19272352458755E+04 0.15973062309708E+02 0.59898983661404E+01 0.30141670676921E+03 0.36591883908070E+03
 0.29432287145573E+03 0.29732731841889E+03 0.29315000000046E+03 0.29315000001400E+03 0.29431410972764E+03
 0.29732627544242E+03 0.29315000000046E+03 0.29315000001400E+03 0.29432287145573E+03 0.29732731841889E+03
 0.29315000000046E+03 0.29315000001400E+03 0.29431410972764E+03 0.29732627544242E+03 0.29315000000046E+03
 0.29315000001400E+03 0.29674766672983E+03 0.29315000013831E+03 0.32871407751638E+02 0.24993028296465E+02
 0.67934600748717E+02 0.20673021653974E+03 0.13845594278727E+03 0.58854524199459E+02 0.62281754288695E+02
 0.72296110285689E+02 0.16281491451875E+03 0.58440851163402E+02 0.62165893582968E+02 0.71918949496607E+02
 0.16270627580997E+03 0.58854524199463E+02 0.62281754288695E+02 0.72296110285695E+02 0.16281491451875E+03
 0.58440851163406E+02 0.62165893582968E+02 0.71918949496614E+02 0.16270627580997E+03 0.10234772402096E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33126039074031E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078444937663E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078444937663E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10808593979853E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10808593979853E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900612337887E-01 0.10947398095806E+00 0.31480993500026E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    470.01044423
 0.56787642761710E-01 0.30574969755811E+03 0.41176145281385E+03 0.40975473358518E+03 0.40907657094002E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16776686852941E+00 0.00000000000000E+00 -.12727269411139E+02
 0.63331388101268E-02 0.95776031605131E+00 0.12631966928007E+04 0.47369875980026E+03 0.83528204979119E+01
 0.31323076867170E+01 0.31827661845545E+03 0.29315000187368E+03 0.31442140540099E+03 0.33211284126106E+03
 0.29315000012450E+03 0.29315000023157E+03 0.31220455927865E+03 0.33208836197276E+03 0.29315000009695E+03
 0.29315000023047E+03 0.31442140540099E+03 0.33211284126106E+03 0.29315000012450E+03 0.29315000023157E+03
 0.31220455927865E+03 0.33208836197276E+03 0.29315000009695E+03 0.29315000023047E+03 0.36656839514959E+03
 0.30187364607618E+03 0.14110631829863E+04 0.13597315716149E+04 0.48302464731897E+03 0.91916824069219E+03
 0.43372847013663E+03 0.82033021572425E+03 0.78920481309564E+03 0.77969162376217E+03 0.13679358534818E+04
 0.73267150559118E+03 0.78882253450276E+03 0.70452339508239E+03 0.13677745028142E+04 0.82033021572425E+03
 0.78920481309564E+03 0.77969162376217E+03 0.13679358534818E+04 0.73267150559118E+03 0.78882253450276E+03
 0.70452339508239E+03 0.13677745028142E+04 0.13606369851443E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44616644612356E+03 0.10919665452157E+01
 0.10919665452157E+01 0.12750611420533E+01 0.10807377515926E+00 0.29888510483416E+03 0.31478934822437E+03
 0.31364346047488E+03 0.31358709490149E+03 0.22999999962378E+00 0.00000000000000E+00 0.20255998036205E+00
 0.00000000000000E+00 -.41602265051453E+01 0.16666382438028E-02 0.51908545137330E+00 0.48000818592440E+04
 0.18000306972165E+04 0.15411720707708E+02 0.57793952653906E+01 0.30187311184935E+03 0.36657828466420E+03
 0.29439400554853E+03 0.29740115352478E+03 0.29315000000057E+03 0.29315000001847E+03 0.29438513569282E+03
 0.29739998196853E+03 0.29315000000057E+03 0.29315000001847E+03 0.29439400554853E+03 0.29740115352478E+03
 0.29315000000057E+03 0.29315000001847E+03 0.29438513569282E+03 0.29739998196853E+03 0.29315000000057E+03
 0.29315000001847E+03 0.29681159274728E+03 0.29315000017830E+03 0.31987993561389E+02 0.24229578351861E+02
 0.69411249120230E+02 0.20748420424314E+03 0.13772589887731E+03 0.60567021870952E+02 0.63602687654965E+02
 0.75985754615512E+02 0.16341761207841E+03 0.60161958442022E+02 0.63480475786811E+02 0.75619119656898E+02
 0.16330349788896E+03 0.60567021870956E+02 0.63602687654965E+02 0.75985754615520E+02 0.16341761207841E+03
 0.60161958442022E+02 0.63480475786812E+02 0.75619119656898E+02 0.16330349788896E+03 0.10273089629272E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33126930050112E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10078703185877E+00 0.00000000000000E+00 0.00000000000000E+00 0.10078703185877E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10810208224911E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10810208224911E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900786254294E-01 0.10948133735293E+00 0.31478934822437E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    480.01434201
 0.56787454371007E-01 0.30597139741919E+03 0.41204851076052E+03 0.41004056101596E+03 0.40936205561491E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16674448523691E+00 0.00000000000000E+00 -.12744010781814E+02
 0.63331592637858E-02 0.97284128518169E+00 0.12631926131632E+04 0.47369722993618E+03 0.82233352159863E+01
 0.30837507059949E+01 0.31866695459694E+03 0.29315000237457E+03 0.31475065939047E+03 0.33262973832061E+03
 0.29315000016262E+03 0.29315000030338E+03 0.31250124317884E+03 0.33260562020084E+03 0.29315000012680E+03
 0.29315000030197E+03 0.31475065939047E+03 0.33262973832061E+03 0.29315000016262E+03 0.29315000030338E+03
 0.31250124317884E+03 0.33260562020084E+03 0.29315000012680E+03 0.29315000030197E+03 0.36721765821969E+03
 0.30233688894984E+03 0.14149963366608E+04 0.13627727010706E+04 0.48265506683613E+03 0.91451287929782E+03
 0.42944453712751E+03 0.82296894008681E+03 0.79146470496532E+03 0.78171086954107E+03 0.13679263892564E+04
 0.73554651863278E+03 0.79109955964651E+03 0.70699758548880E+03 0.13677786469278E+04 0.82296894008681E+03
 0.79146470496532E+03 0.78171086954107E+03 0.13679263892564E+04 0.73554651863278E+03 0.79109955964651E+03
 0.70699758548880E+03 0.13677786469278E+04 0.13602722262841E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44645172861002E+03 0.10919666551478E+01
 0.10919666551478E+01 0.13050728354001E+01 0.10072102693242E+00 0.29939153095909E+03 0.31477629062494E+03
 0.31374324476317E+03 0.31369389467766E+03 0.22999999973221E+00 0.00000000000000E+00 0.20171095102034E+00
 0.00000000000000E+00 -.41578776729894E+01 0.17883046108721E-02 0.53733049327634E+00 0.44735108053535E+04
 0.16775665520076E+04 0.14888416161198E+02 0.55831560604493E+01 0.30233639243033E+03 0.36722687429250E+03
 0.29446841714464E+03 0.29747455082097E+03 0.29315000000073E+03 0.29315000002418E+03 0.29445946050737E+03
 0.29747325050722E+03 0.29315000000073E+03 0.29315000002419E+03 0.29446841714464E+03 0.29747455082097E+03
 0.29315000000073E+03 0.29315000002418E+03 0.29445946050737E+03 0.29747325050722E+03 0.29315000000073E+03
 0.29315000002419E+03 0.29687513930284E+03 0.29315000022815E+03 0.31023121788256E+02 0.23422505911111E+02
 0.70843225765185E+02 0.20826401471968E+03 0.13706657282567E+03 0.62270242902571E+02 0.64884867554177E+02
 0.79841054752935E+02 0.16403975128699E+03 0.61874858171984E+02 0.64756610912475E+02 0.79485972841873E+02
 0.16392046704519E+03 0.62270242902572E+02 0.64884867554177E+02 0.79841054752938E+02 0.16403975128699E+03
 0.61874858171984E+02 0.64756610912475E+02 0.79485972841873E+02 0.16392046704519E+03 0.10310436040954E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33128668977214E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10079046263153E+00 0.00000000000000E+00 0.00000000000000E+00 0.10079046263153E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10811546953152E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10811546953152E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97900944660463E-01 0.10948605856511E+00 0.31477629062494E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    490.10533523
 0.56788502781078E-01 0.30619263265284E+03 0.41233909834888E+03 0.41032979872808E+03 0.40965088420356E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16573018848798E+00 0.00000000000000E+00 -.12761101728252E+02
 0.63330417814004E-02 0.98776837449539E+00 0.12632160462758E+04 0.47370601735342E+03 0.80990647266743E+01
 0.30371492725029E+01 0.31905650810757E+03 0.29315000299511E+03 0.31507957064442E+03 0.33314499280748E+03
 0.29315000021131E+03 0.29315000039533E+03 0.31279959474604E+03 0.33312122975109E+03 0.29315000016500E+03
 0.29315000039352E+03 0.31507957064442E+03 0.33314499280748E+03 0.29315000021131E+03 0.29315000039533E+03
 0.31279959474604E+03 0.33312122975109E+03 0.29315000016500E+03 0.29315000039352E+03 0.36786173028821E+03
 0.30281021690046E+03 0.14188640296146E+04 0.13657470245274E+04 0.48226454355998E+03 0.90994515980501E+03
 0.42526929352723E+03 0.82557174012030E+03 0.79367937252984E+03 0.78369204616947E+03 0.13679193802678E+04
 0.73837431508632E+03 0.79333056903099E+03 0.70941211674720E+03 0.13677845208627E+04 0.82557174012030E+03
 0.79367937252984E+03 0.78369204616947E+03 0.13679193802678E+04 0.73837431508632E+03 0.79333056903099E+03
 0.70941211674720E+03 0.13677845208627E+04 0.13599130600246E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44674032420284E+03 0.10919667673755E+01
 0.10919667673755E+01 0.13353458150368E+01 0.93597995941388E-01 0.29991873777802E+03 0.31477064492902E+03
 0.31384390576553E+03 0.31380100999504E+03 0.22999999974116E+00 0.00000000000000E+00 0.20085219972112E+00
 0.00000000000000E+00 -.41509747882771E+01 0.19243987581684E-02 0.55571887331694E+00 0.41571425703965E+04
 0.15589284638987E+04 0.14395768047700E+02 0.53984130178876E+01 0.30280976045392E+03 0.36787030105302E+03
 0.29454674220987E+03 0.29754813400596E+03 0.29315000000094E+03 0.29315000003151E+03 0.29453771937828E+03
 0.29754670428864E+03 0.29315000000094E+03 0.29315000003152E+03 0.29454674220987E+03 0.29754813400596E+03
 0.29315000000094E+03 0.29315000003151E+03 0.29453771937828E+03 0.29754670428864E+03 0.29315000000094E+03
 0.29315000003152E+03 0.29693884124118E+03 0.29315000029048E+03 0.29972869055009E+02 0.22565823425007E+02
 0.72243802029818E+02 0.20907720257697E+03 0.13647218153701E+03 0.63977387712502E+02 0.66140505615464E+02
 0.83900930695539E+02 0.16468762078050E+03 0.63592778412320E+02 0.66006476721957E+02 0.83558467595549E+02
 0.16456344144094E+03 0.63977387712506E+02 0.66140505615464E+02 0.83900930695545E+02 0.16468762078050E+03
 0.63592778412320E+02 0.66006476721957E+02 0.83558467595549E+02 0.16456344144094E+03 0.10347178202194E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33131261606036E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10079658117050E+00 0.00000000000000E+00 0.00000000000000E+00 0.10079658117050E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10812676765506E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10812676765506E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97901161834085E-01 0.10948827263567E+00 0.31477064492902E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
    500.00000000
 0.56790029021972E-01 0.30640767141598E+03 0.41262487399583E+03 0.41061418132345E+03 0.40993482078503E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16475193740798E+00 0.00000000000000E+00 -.12777833582459E+02
 0.63328710286720E-02 0.10021317277666E+01 0.12632501062757E+04 0.47371878985338E+03 0.79829824546408E+01
 0.29936184204903E+01 0.31943463293425E+03 0.29315000373652E+03 0.31539913337408E+03 0.33364444325282E+03
 0.29315000027121E+03 0.29315000050873E+03 0.31309099179381E+03 0.33362101999298E+03 0.29315000021206E+03
 0.29315000050643E+03 0.31539913337408E+03 0.33364444325282E+03 0.29315000027121E+03 0.29315000050873E+03
 0.31309099179381E+03 0.33362101999298E+03 0.29315000021206E+03 0.29315000050643E+03 0.36848283542198E+03
 0.30327967182509E+03 0.14225654674623E+04 0.13685801050708E+04 0.48186995047680E+03 0.90559281110598E+03
 0.42131351087680E+03 0.82807007126014E+03 0.79579223313348E+03 0.78558550518207E+03 0.13679160748987E+04
 0.74108291527201E+03 0.79545859214324E+03 0.71171045215809E+03 0.13677930740229E+04 0.82807007126014E+03
 0.79579223313348E+03 0.78558550518207E+03 0.13679160748987E+04 0.74108291527201E+03 0.79545859214324E+03
 0.71171045215809E+03 0.13677930740229E+04 0.13595724401260E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44702402841682E+03 0.10919668772452E+01
 0.10919668772452E+01 0.13650298093572E+01 0.86897090767190E-01 0.30045020940224E+03 0.31477227940819E+03
 0.31394258193001E+03 0.31390542516248E+03 0.22999999974123E+00 0.00000000000000E+00 0.20000796065946E+00
 0.00000000000000E+00 -.41440132399338E+01 0.20727950309655E-02 0.57373475776641E+00 0.38595229535424E+04
 0.14473211075784E+04 0.13943725548622E+02 0.52288970807333E+01 0.30327925663300E+03 0.36849080342961E+03
 0.29462702569818E+03 0.29761992435529E+03 0.29315000000121E+03 0.29315000004056E+03 0.29461795901925E+03
 0.29761836841293E+03 0.29315000000121E+03 0.29315000004057E+03 0.29462702569818E+03 0.29761992435529E+03
 0.29315000000121E+03 0.29315000004056E+03 0.29461795901925E+03 0.29761836841293E+03 0.29315000000121E+03
 0.29315000004057E+03 0.29700098998540E+03 0.29315000036563E+03 0.28873176363603E+02 0.21685705800604E+02
 0.73576961580042E+02 0.20990077300939E+03 0.13595592662145E+03 0.65639638375969E+02 0.67337521284863E+02
 0.88052909073105E+02 0.16534318160517E+03 0.65266537337989E+02 0.67198153921140E+02 0.87723709878445E+02
 0.16521451774904E+03 0.65639638375970E+02 0.67337521284863E+02 0.88052909073108E+02 0.16534318160517E+03
 0.65266537337989E+02 0.67198153921140E+02 0.87723709878445E+02 0.16521451774904E+03 0.10382361569269E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33134595181542E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10080190582205E+00 0.00000000000000E+00 0.00000000000000E+00 0.10080190582205E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10813544666660E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10813544666660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97901361946855E-01 0.10948793543075E+00 0.31477227940819E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29315000000000E+03
