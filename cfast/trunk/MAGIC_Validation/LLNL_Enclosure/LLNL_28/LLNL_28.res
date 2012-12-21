#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-28 0 MONOZONE(1=OUI,0=NON)                                                                     
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
85.200000 0.003000                                                                                  
1000.000000 0.003000                                                                                
2000.000000 0.003000                                                                                
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
Plenum-LLNL28 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.146000                                                                                   
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
#CONDINIT 1000.000000 10.000000 31.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-28           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-28           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-28           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-28           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-28           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-28           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-28           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-28           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-28           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-28           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-28           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-28           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-28           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-28           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-28           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-28           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-28           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-28           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL28     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL28     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL28     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL28     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL28     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL28     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL28     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL28     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL28     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL28     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL28     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL28     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL28     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL28     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL28     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL28     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL28     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL28     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.60917853803112E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.53026518157340E-03 0.53026518157340E-03 0.75106766297337E-03 0.75482300128823E-03
 0.00000000000000E+00 0.50249022678299E-03 0.59134775583368E-03 0.50249022678299E-03 0.59134775583368E-03
 0.49994659189231E-03 0.59134068020083E-03 0.49994659189231E-03 0.59134068020083E-03 0.50249022678299E-03
 0.59134775589333E-03 0.50249022678299E-03 0.59134775589333E-03 0.49994659183265E-03 0.59134068020083E-03
 0.49994659183265E-03 0.59134068020083E-03 0.60930482757566E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.51123969818382E-07 0.99966156755646E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.61750928576219E-03 0.61750928576219E-03
 0.75844282634781E-03 0.76223504047955E-03 0.00000000000000E+00 0.53976473247483E-03 0.59542569558434E-03
 0.53976473247483E-03 0.59542569558434E-03 0.53506678272995E-03 0.59541751803018E-03 0.53506678272995E-03
 0.59541751803018E-03 0.53976473241518E-03 0.59542569558434E-03 0.53976473241518E-03 0.59542569558434E-03
 0.53506678261065E-03 0.59541751803018E-03 0.53506678261065E-03 0.59541751803018E-03 0.49250430405855E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.38892958014586E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.38892958014586E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14599978775549E+00 0.16899027053632E+00 0.30415000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     12.06358153
 0.30000000000000E+01 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.22999999999996E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.98283544759055E+01
 0.99990300168294E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30415000186815E+03 0.30415000000000E+03 0.30415000257492E+03 0.30415000257492E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000256165E+03 0.30415000256165E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000257492E+03 0.30415000257492E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000256165E+03 0.30415000256165E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000904263E+03
 0.30415000746985E+03 0.54652223786653E-03 0.54652223786653E-03 0.71422022922486E-03 0.71779133037099E-03
 .00000000000000E+00 0.51138289458428E-03 0.60812422485439E-03 0.51138289458428E-03 0.60812422485439E-03
 0.50873456362808E-03 0.60819292361094E-03 0.50873456362808E-03 0.60819292361094E-03 0.51138289464394E-03
 0.60812422485439E-03 0.51138289464394E-03 0.60812422485439E-03 0.50873456356842E-03 0.60819292361094E-03
 0.50873456356842E-03 0.60819292361094E-03 0.60962939370166E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.10862920745545E+02 0.99957095074120E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415000744031E+03 0.30415000907857E+03
 0.30415000276511E+03 0.30415000276511E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000274089E+03
 0.30415000274089E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000276511E+03 0.30415000276511E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000274089E+03 0.30415000274089E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000266378E+03 0.30415000000000E+03 0.59174730855050E-03 0.59174730855050E-03
 0.77347946675470E-03 0.77734686408848E-03 .00000000000000E+00 0.54912550046797E-03 0.60315502243344E-03
 0.54912550046797E-03 0.60315502243344E-03 0.54430391547458E-03 0.60328579338598E-03 0.54430391547458E-03
 0.60328579338598E-03 0.54912550046797E-03 0.60315502249309E-03 0.54912550046797E-03 0.60315502249309E-03
 0.54430391553423E-03 0.60328579332632E-03 0.54430391553423E-03 0.60328579332632E-03 0.49274166137812E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866610609370E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866610609370E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596850495E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596850495E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573514922521E+00 0.16866586905577E+00 0.30415000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     27.06358153
 0.30000000000000E+01 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.98283544745373E+01
 0.99990300168296E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30415000278364E+03 0.30415000000000E+03 0.30415000381536E+03 0.30415000381536E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000379554E+03 0.30415000379554E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000381536E+03 0.30415000381536E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000379554E+03 0.30415000379554E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415001301543E+03
 0.30415001077072E+03 0.55339122480150E-03 0.55339122480150E-03 0.69839688906637E-03 0.70188887351170E-03
 .00000000000000E+00 0.51494053032292E-03 0.61506970739078E-03 0.51494053032292E-03 0.61506970739078E-03
 0.51225390951021E-03 0.61517587546855E-03 0.51225390951021E-03 0.61517587546855E-03 0.51494053026327E-03
 0.61506970739078E-03 0.51494053026327E-03 0.61506970739078E-03 0.51225391004710E-03 0.61517587588613E-03
 0.51225391004710E-03 0.61517587588613E-03 0.60958675284756E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.10862920744130E+02 0.99958996055618E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415001074756E+03 0.30415001304345E+03
 0.30415000409688E+03 0.30415000409688E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000406088E+03
 0.30415000406088E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000409688E+03 0.30415000409688E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000406088E+03 0.30415000406088E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000395079E+03 0.30415000000000E+03 0.58073637659396E-03 0.58073637659396E-03
 0.77960676839596E-03 0.78350480223794E-03 .00000000000000E+00 0.55291301141547E-03 0.60628477291808E-03
 0.55291301141547E-03 0.60628477291808E-03 0.54804652867661E-03 0.60648309113371E-03 0.54804652867661E-03
 0.60648309113371E-03 0.55291301141547E-03 0.60628477291808E-03 0.55291301141547E-03 0.60628477291808E-03
 0.54804652861696E-03 0.60648309125302E-03 0.54804652861696E-03 0.60648309125302E-03 0.49270780841206E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866610608196E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866610608196E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596850136E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596850136E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573514922524E+00 0.16866586905581E+00 0.30415000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     40.00000000
 0.30000000000000E+01 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.98283544733245E+01
 0.99990300168297E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30415000343609E+03 0.30415000000000E+03 0.30415000469444E+03 0.30415000469444E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000466996E+03 0.30415000466996E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000469444E+03 0.30415000469444E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000466996E+03 0.30415000466996E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415001574111E+03
 0.30415001304188E+03 0.55808244794674E-03 0.55808244794674E-03 0.68770453178514E-03 0.69114305444406E-03
 .00000000000000E+00 0.51734520507445E-03 0.61976443721327E-03 0.51734520507445E-03 0.61976443721327E-03
 0.51463457547718E-03 0.61989725857936E-03 0.51463457547718E-03 0.61989725857936E-03 0.51734520501480E-03
 0.61976443733258E-03 0.51734520501480E-03 0.61976443733258E-03 0.51463457541753E-03 0.61989725863902E-03
 0.51463457541753E-03 0.61989725863902E-03 0.60957069249722E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.22999999999998E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.10862920742854E+02 0.99960554568807E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415001302521E+03 0.30415001576117E+03
 0.30415000504072E+03 0.30415000504072E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000499638E+03
 0.30415000499638E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000504072E+03 0.30415000504072E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000499638E+03 0.30415000499638E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000486434E+03 0.30415000000000E+03 0.57331549536703E-03 0.57331549536703E-03
 0.78377259050475E-03 0.78769145345727E-03 .00000000000000E+00 0.55548931953756E-03 0.60841339830718E-03
 0.55548931953756E-03 0.60841339830718E-03 0.55059539250364E-03 0.60865957708366E-03 0.55059539250364E-03
 0.60865957708366E-03 0.55548931959721E-03 0.60841339830718E-03 0.55548931959721E-03 0.60841339830718E-03
 0.55059539244398E-03 0.60865957702400E-03 0.55059539244398E-03 0.60865957702400E-03 0.49270119233682E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866610607160E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866610607160E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596849660E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596849660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573514922527E+00 0.16866586905585E+00 0.30415000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     40.00025000
 0.30000000000000E+01 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.98283544733220E+01
 0.99990300168297E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30415000343610E+03 0.30415000000000E+03 0.30415000469446E+03 0.30415000469446E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000466998E+03 0.30415000466998E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000469446E+03 0.30415000469446E+03 0.30415000000000E+03 0.30415000000000E+03
 0.30415000466998E+03 0.30415000466998E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415001574116E+03
 0.30415001304192E+03 0.55809163412398E-03 0.55809163412398E-03 0.68771736751860E-03 0.69115595435619E-03
 .00000000000000E+00 0.51735521870678E-03 0.61977173630851E-03 0.51735521870678E-03 0.61977173630851E-03
 0.51464466886637E-03 0.61990455845010E-03 0.51464466886637E-03 0.61990455845010E-03 0.51735521870678E-03
 0.61977173642782E-03 0.51735521870678E-03 0.61977173642782E-03 0.51464466898568E-03 0.61990455839045E-03
 0.51464466898568E-03 0.61990455839045E-03 0.60958202051193E-04 0.00000000000000E+00 0.75000000000358E-07
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.18750000000179E-10 0.15000000000000E+01 0.30415000000000E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000000000E+03 0.22999999999998E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.10862920742766E+02 0.99960554598913E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415001302526E+03 0.30415001576122E+03
 0.30415000504074E+03 0.30415000504074E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000499640E+03
 0.30415000499640E+03 0.30415000000000E+03 0.30415000000000E+03 0.30415000504074E+03 0.30415000504074E+03
 0.30415000000000E+03 0.30415000000000E+03 0.30415000499640E+03 0.30415000499640E+03 0.30415000000000E+03
 0.30415000000000E+03 0.30415000486436E+03 0.30415000000000E+03 0.57332740491009E-03 0.57332740491009E-03
 0.78379691581844E-03 0.78771590039753E-03 .00000000000000E+00 0.55550603244817E-03 0.60842684070915E-03
 0.55550603244817E-03 0.60842684070915E-03 0.55061238316141E-03 0.60867301984355E-03 0.55061238316141E-03
 0.60867301984355E-03 0.55550603238852E-03 0.60842684070915E-03 0.55550603238852E-03 0.60842684070915E-03
 0.55061238304211E-03 0.60867301990320E-03 0.55061238304211E-03 0.60867301990320E-03 0.49271496508134E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866610607160E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866610607160E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596848951E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16866596848951E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573514922528E+00 0.16866586905585E+00 0.30415000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     50.00171015
 0.22637840886484E+01 0.30416919596935E+03 0.33227193031513E+03 0.31106575602863E+03 0.31061627623402E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22681543290824E+00 0.00000000000000E+00 0.11150946802662E+02
 0.10000469348318E-02 0.54297935590639E-01 0.79996245389682E+04 0.29998592021131E+04 0.14733525157040E+03
 0.55250719338899E+02 0.30518248998442E+03 0.30415000000001E+03 0.30499381801732E+03 0.30532359342114E+03
 0.30415000000003E+03 0.30415000000003E+03 0.30479785953319E+03 0.30531494884369E+03 0.30415000000003E+03
 0.30415000000003E+03 0.30499381801732E+03 0.30532359342114E+03 0.30415000000003E+03 0.30415000000003E+03
 0.30479785953319E+03 0.30531494884369E+03 0.30415000000003E+03 0.30415000000003E+03 0.30731284649487E+03
 0.30415326983600E+03 0.47664461135275E+03 0.47483001734955E+03 0.20640906223405E+03 0.43199597524622E+03
 0.22455486770101E+03 0.28644959990123E+03 0.13791963205699E+03 0.28531423859539E+03 0.37314887629073E+03
 0.21722274290428E+03 0.13455610311778E+03 0.21642639637460E+03 0.36987256782595E+03 0.28644959990123E+03
 0.13791963205699E+03 0.28531423859539E+03 0.37314887629073E+03 0.21722274290428E+03 0.13455610311778E+03
 0.21642639637460E+03 0.36987256782595E+03 0.38909109122460E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.35245495388084E+03 0.10918097748705E+01
 0.10918097748705E+01 0.15025991652311E-01 0.13394942351767E+01 0.30415857839027E+03 0.30865509687507E+03
 0.30463972314923E+03 0.30463345404047E+03 0.22999999998636E+00 0.00000000000000E+00 0.22941426644301E+00
 0.00000000000000E+00 0.10232537749104E+02 0.99978534869466E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415326239278E+03 0.30732061509147E+03
 0.30415159969668E+03 0.30428072493482E+03 0.30415000000003E+03 0.30415000000003E+03 0.30415161251266E+03
 0.30428073211115E+03 0.30415000000003E+03 0.30415000000003E+03 0.30415159969668E+03 0.30428072493482E+03
 0.30415000000003E+03 0.30415000000003E+03 0.30415161251266E+03 0.30428073211115E+03 0.30415000000003E+03
 0.30415000000003E+03 0.30419958343291E+03 0.30415000000001E+03 0.45934751290165E+00 0.46030316385454E+00
 0.38858636382351E+00 0.23856615522813E+02 0.23466086227170E+02 0.54540104528347E+00 -.24146968862488E+00
 0.54795841840404E+00 0.36176243808734E+02 0.54936592886172E+00 -.23882463364344E+00 0.55191734085301E+00
 0.36178823061218E+02 0.54540104528347E+00 -.24146968862416E+00 0.54795841840404E+00 0.36176243808736E+02
 0.54936592886190E+00 -.23882463364344E+00 0.55191734085318E+00 0.36178823061218E+02 0.71446144046000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31317956938545E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17583708996629E+00 0.00000000000000E+00 0.00000000000000E+00 0.17583708996629E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15807530687512E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15807530687512E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14624861348469E+00 0.16929060466715E+00 0.30415857839027E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     60.00179395
 0.17201279645286E+01 0.30420360864769E+03 0.35142003586427E+03 0.32434727025081E+03 0.32270107791248E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22438851490635E+00 0.00000000000000E+00 0.56797262970399E+01
 0.99987981845643E-03 0.98216376385666E-01 0.80000000000000E+04 0.30000000000000E+04 0.81452811581914E+02
 0.30544804343218E+02 0.30605399426204E+03 0.30415000000002E+03 0.30587465639834E+03 0.30682819327217E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30545629768548E+03 0.30680810848973E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30587465639834E+03 0.30682819327217E+03 0.30415000000004E+03 0.30415000000004E+03
 0.30545629768548E+03 0.30680810848973E+03 0.30415000000004E+03 0.30415000000004E+03 0.31152069037899E+03
 0.30415909793793E+03 0.50292142562394E+03 0.49887353392517E+03 0.21632782210513E+03 0.60245892039452E+03
 0.38504945917886E+03 0.32639754298588E+03 0.18147143263230E+03 0.32347862444213E+03 0.53902984889781E+03
 0.24585160576152E+03 0.17739669591636E+03 0.24384832232206E+03 0.53514764166147E+03 0.32639754298588E+03
 0.18147143263230E+03 0.32347862444213E+03 0.53902984889781E+03 0.24585160576152E+03 0.17739669591636E+03
 0.24384832232206E+03 0.53514764166147E+03 0.50222711754223E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.37235373329935E+03 0.10918456911237E+01
 0.10918456911237E+01 0.45026243044520E-01 0.11732742746101E+01 0.30415491028794E+03 0.31188823076221E+03
 0.30583936011569E+03 0.30580651390380E+03 0.22999999999246E+00 0.00000000000000E+00 0.22893165932014E+00
 0.00000000000000E+00 0.59235231731606E+01 0.99975487793672E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30415906593155E+03 0.31154831840843E+03
 0.30415401292625E+03 0.30441800934466E+03 0.30415000000004E+03 0.30415000000004E+03 0.30415402116043E+03
 0.30441804790083E+03 0.30415000000004E+03 0.30415000000004E+03 0.30415401292625E+03 0.30441800934466E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30415402116043E+03 0.30441804790083E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30428985482681E+03 0.30415000000002E+03 0.83533501333654E+00 0.83393010691319E+00
 0.15174719584326E+00 0.46007984187423E+02 0.45855478255601E+02 0.86543230270839E+00 -.63235433186310E+00
 0.86563181054180E+00 0.52133393517628E+02 0.86462365502683E+00 -.62148722470442E+00 0.86482088467629E+00
 0.52143952015432E+02 0.86543230270833E+00 -.63235433186310E+00 0.86563181054174E+00 0.52133393517628E+02
 0.86462365502683E+00 -.62148722470448E+00 0.86482088467629E+00 0.52143952015432E+02 0.14852048163928E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31917888071784E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.13554495340372E+00 0.00000000000000E+00 0.00000000000000E+00 0.13554495340372E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15040850459416E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15040850459416E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14614388246404E+00 0.16916421707457E+00 0.30415491028794E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     70.00985419
 0.13735101775666E+01 0.30426605488026E+03 0.36380581769586E+03 0.33654632763015E+03 0.33389210146943E+03
 0.22999999997633E+00 0.00000000000000E+00 0.22252883811480E+00 0.00000000000000E+00 0.15548064992348E+01
 0.99963391323600E-03 0.12808941768826E+00 0.80000000000000E+04 0.30000000000000E+04 0.62456369498610E+02
 0.23421138561979E+02 0.30671141360905E+03 0.30415000000002E+03 0.30655949272087E+03 0.30827431915917E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30597660626030E+03 0.30824704328578E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30655949272087E+03 0.30827431915917E+03 0.30415000000004E+03 0.30415000000004E+03
 0.30597660626030E+03 0.30824704328578E+03 0.30415000000004E+03 0.30415000000004E+03 0.31532158753042E+03
 0.30416707475134E+03 0.53851489654081E+03 0.53264741291575E+03 0.23578507859056E+03 0.71918998200143E+03
 0.48222597801792E+03 0.35976505945256E+03 0.22820134444505E+03 0.35527234148024E+03 0.66489064232840E+03
 0.27293510375196E+03 0.22406389807643E+03 0.26987052912479E+03 0.66101315931128E+03 0.35976505945256E+03
 0.22820134444505E+03 0.35527234148024E+03 0.66489064232840E+03 0.27293510375196E+03 0.22406389807643E+03
 0.26987052912479E+03 0.66101315931128E+03 0.60443402709265E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.38551777584137E+03 0.10918727712762E+01
 0.10918727712762E+01 0.75050423769625E-01 0.10243330311834E+01 0.30415235160659E+03 0.31435746561251E+03
 0.30738850870367E+03 0.30731602045313E+03 0.22999999999500E+00 0.00000000000000E+00 0.22849659994669E+00
 0.00000000000000E+00 0.28178784406988E+01 0.99973263671687E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30416702666559E+03 0.31535060208980E+03
 0.30415705603964E+03 0.30455382586919E+03 0.30415000000004E+03 0.30415000000004E+03 0.30415704271834E+03
 0.30455391483958E+03 0.30415000000004E+03 0.30415000000004E+03 0.30415705603964E+03 0.30455382586919E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30415704271834E+03 0.30455391483958E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30439539267684E+03 0.30415000000002E+03 0.13166108716451E+01 0.13098098372197E+01
 -.48039024523248E-01 0.64440782446221E+02 0.64489061665867E+02 0.12812427651051E+01 -.91299184310704E+00
 0.12796005701896E+01 0.63688534754687E+02 0.12753352381314E+01 -.89373504488409E+00 0.12736988939163E+01
 0.63707116131360E+02 0.12812427651051E+01 -.91299184310704E+00 0.12796005701896E+01 0.63688534754687E+02
 0.12753352381313E+01 -.89373504488415E+00 0.12736988939162E+01 0.63707116131360E+02 0.22101419490420E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32387792277191E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10102898477950E+00 0.00000000000000E+00 0.00000000000000E+00 0.10102898477950E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14720279976181E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14720279976181E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14606835272549E+00 0.16907302862463E+00 0.30415235160659E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     80.02128351
 0.10991879501833E+01 0.30436056499426E+03 0.37258870952763E+03 0.34759019141615E+03 0.34430904128399E+03
 0.22999999998236E+00 0.00000000000000E+00 0.22095182608030E+00 0.00000000000000E+00 -.13353068577930E+01
 0.99929500321088E-03 0.15229929732681E+00 0.80000000000000E+04 0.30000000000000E+04 0.52528147801190E+02
 0.19698055425446E+02 0.30732850560965E+03 0.30415000000003E+03 0.30719304088023E+03 0.30970236355570E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30647132177011E+03 0.30967000023492E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30719304088023E+03 0.30970236355570E+03 0.30415000000004E+03 0.30415000000004E+03
 0.30647132177011E+03 0.30967000023492E+03 0.30415000000004E+03 0.30415000000004E+03 0.31886707465048E+03
 0.30417733579298E+03 0.57973152972463E+03 0.57213925805292E+03 0.25768776386576E+03 0.80049879860829E+03
 0.54152259592320E+03 0.38994221037870E+03 0.27403644400805E+03 0.38391412628089E+03 0.76355601075300E+03
 0.29966824804129E+03 0.27001561014658E+03 0.29555835908770E+03 0.75984289079872E+03 0.38994221037870E+03
 0.27403644400805E+03 0.38391412628089E+03 0.76355601075300E+03 0.29966824804129E+03 0.27001561014658E+03
 0.29555835908770E+03 0.75984289079872E+03 0.69577298006881E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.39374294065678E+03 0.10918917458277E+01
 0.10918917458277E+01 0.10508471172598E+00 0.90543277683100E+00 0.30415086265586E+03 0.31646355381934E+03
 0.30903134438573E+03 0.30891492996659E+03 0.23000000000000E+00 0.00000000000000E+00 0.22806618110439E+00
 0.00000000000000E+00 0.82706383988661E+00 0.99971788208394E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30417727277126E+03 0.31889260715186E+03
 0.30416087012477E+03 0.30469144535319E+03 0.30415000000004E+03 0.30415000000004E+03 0.30416082463080E+03
 0.30469159508387E+03 0.30415000000004E+03 0.30415000000004E+03 0.30416087012477E+03 0.30469144535319E+03
 0.30415000000004E+03 0.30415000000004E+03 0.30416082463080E+03 0.30469159508387E+03 0.30415000000004E+03
 0.30415000000004E+03 0.30451059451056E+03 0.30415000000003E+03 0.18537680153294E+01 0.18395917089974E+01
 -.24230534749241E+00 0.80782036146212E+02 0.81025553020442E+02 0.17447229186580E+01 -.11514827650231E+01
 0.17403542849951E+01 0.74084789665007E+02 0.17335266967337E+01 -.11244838693072E+01 0.17291831168415E+01
 0.74110674414595E+02 0.17447229186579E+01 -.11514827650231E+01 0.17403542849950E+01 0.74084789665007E+02
 0.17335266967337E+01 -.11244838693074E+01 0.17291831168415E+01 0.74110674414595E+02 0.28954445020424E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32772435772589E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.72447121894556E-01 0.00000000000000E+00 0.00000000000000E+00 0.72447121894556E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14551275725169E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14551275725169E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14601991495960E+00 0.16608940705905E+00 0.30950737376672E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
     90.00461145
 0.87623105920412E+00 0.30449426324989E+03 0.37940985178491E+03 0.35752872995393E+03 0.35397315045793E+03
 0.22999999998388E+00 0.00000000000000E+00 0.21949642953950E+00 0.00000000000000E+00 -.33914849667484E+01
 0.99883595980269E-03 0.17410260620428E+00 0.80000000000000E+04 0.30000000000000E+04 0.45949915250627E+02
 0.17231218218985E+02 0.30798226394594E+03 0.30415000000004E+03 0.30780243412575E+03 0.31108400786345E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30696146621479E+03 0.31104846017776E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30780243412575E+03 0.31108400786345E+03 0.30415000000005E+03 0.30415000000005E+03
 0.30696146621479E+03 0.31104846017776E+03 0.30415000000005E+03 0.30415000000005E+03 0.32213989791819E+03
 0.30418954370808E+03 0.64636109030400E+03 0.63695089933681E+03 0.27355580749760E+03 0.85773743353389E+03
 0.58281384699881E+03 0.41951134470147E+03 0.31314523022287E+03 0.41198084222308E+03 0.84174339592063E+03
 0.32737349864901E+03 0.30946652234212E+03 0.32223000593690E+03 0.83840230844885E+03 0.41951134470147E+03
 0.31314523022287E+03 0.41198084222308E+03 0.84174339592063E+03 0.32737349864901E+03 0.30946652234212E+03
 0.32223000593690E+03 0.83840230844885E+03 0.78159792446325E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.40085569953488E+03 0.15825201482491E+01
 0.15825201482491E+01 0.13503469554412E+00 0.81562950221650E+00 0.30415014136604E+03 0.31833616029094E+03
 0.31062246992151E+03 0.31046238230196E+03 0.23000000000000E+00 0.00000000000000E+00 0.22762506897648E+00
 0.00000000000000E+00 -.43896346663386E+00 0.99970775755802E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30418946732967E+03 0.32216203400290E+03
 0.30416531795530E+03 0.30483266857429E+03 0.30415000000005E+03 0.30415000000005E+03 0.30416523095449E+03
 0.30483288689250E+03 0.30415000000005E+03 0.30415000000005E+03 0.30416531795530E+03 0.30483266857429E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30416523095449E+03 0.30483288689250E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30463204553118E+03 0.30415000000004E+03 0.24148155935531E+01 0.23914971759753E+01
 -.45821896013226E+00 0.95608100225687E+02 0.96068610280620E+02 0.22196408974681E+01 -.13917141718343E+01
 0.22120843901322E+01 0.83865217899374E+02 0.22028129161751E+01 -.13571087299010E+01 0.21953110884499E+01
 0.83898207027586E+02 0.22196408974681E+01 -.13917141718342E+01 0.22120843901322E+01 0.83865217899374E+02
 0.22028129161752E+01 -.13571087299010E+01 0.21953110884500E+01 0.83898207027586E+02 0.35360439497411E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33094304711069E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46973147841374E-01 0.00000000000000E+00 0.00000000000000E+00 0.46973147841374E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14454032470227E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14454032470227E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14598910289758E+00 0.16513374377407E+00 0.31122897923238E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    100.12945315
 0.69292798301099E+00 0.30468250499021E+03 0.38516989905902E+03 0.36657924318238E+03 0.36301971180486E+03
 0.22999999972186E+00 0.00000000000000E+00 0.21804784513910E+00 0.00000000000000E+00 -.47816057039986E+01
 0.99820515435986E-03 0.19547334449223E+00 0.80000000000000E+04 0.30000000000000E+04 0.40926296221006E+02
 0.15347361082877E+02 0.30868477561614E+03 0.30415000000005E+03 0.30841461319825E+03 0.31245451983101E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30746654244337E+03 0.31241700359255E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30841461319825E+03 0.31245451983101E+03 0.30415000000005E+03 0.30415000000005E+03
 0.30746654244337E+03 0.31241700359255E+03 0.30415000000005E+03 0.30415000000005E+03 0.32527616496443E+03
 0.30420392438187E+03 0.71390162438002E+03 0.70260581389426E+03 0.29244577826274E+03 0.90811925293967E+03
 0.61421124578562E+03 0.44975554313907E+03 0.35290287589118E+03 0.44074351012876E+03 0.91156134338439E+03
 0.35639251398769E+03 0.34954817204878E+03 0.35022963189165E+03 0.90856196616885E+03 0.44975554313907E+03
 0.35290287589118E+03 0.44074351012876E+03 0.91156134338439E+03 0.35639251398769E+03 0.34954817204878E+03
 0.35022963189164E+03 0.90856196616885E+03 0.86269110149219E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.40824635120871E+03 0.14349637051882E+01
 0.14349637051882E+01 0.16540922065902E+00 0.73549288240024E+00 0.30415011466333E+03 0.32006571986099E+03
 0.31226184363301E+03 0.31205891970601E+03 0.23000000000000E+00 0.00000000000000E+00 0.22715778867174E+00
 0.00000000000000E+00 -.11269695566915E+01 0.99970105489093E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30420383556725E+03 0.32529641795935E+03
 0.30417048459397E+03 0.30497838624742E+03 0.30415000000005E+03 0.30415000000005E+03 0.30417034821895E+03
 0.30497868029262E+03 0.30415000000005E+03 0.30415000000005E+03 0.30417048459397E+03 0.30497838624742E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30417034821895E+03 0.30497868029262E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30476039869918E+03 0.30415000000005E+03 0.30176652843568E+01 0.29832280520787E+01
 -.66656319259767E+00 0.10956757693077E+03 0.11023747293933E+03 0.27322296371056E+01 -.16172415572824E+01
 0.27210294023183E+01 0.92974816577163E+02 0.27098711888757E+01 -.15753406973269E+01 0.26987655821649E+01
 0.93014543460462E+02 0.27322296371056E+01 -.16172415572824E+01 0.27210294023183E+01 0.92974816577163E+02
 0.27098711888757E+01 -.15753406973265E+01 0.26987655821650E+01 0.93014543460463E+02 0.41629554281983E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33386301406304E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23733094652443E-01 0.00000000000000E+00 0.00000000000000E+00 0.23733094652443E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14393612630162E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14393612630162E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14593106223767E+00 0.16460211452363E+00 0.31210792712262E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    110.25203218
 0.54529737668566E+00 0.30493820304835E+03 0.39010663753699E+03 0.37462592956930E+03 0.37125903920761E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21659084626129E+00 0.00000000000000E+00 -.56403016740713E+01
 0.99735968243147E-03 0.21675349047439E+00 0.80000000000000E+04 0.30000000000000E+04 0.36908286840000E+02
 0.13840607565000E+02 0.30941299104007E+03 0.30415000000005E+03 0.30902746063602E+03 0.31379766851976E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30798353082859E+03 0.31375893877017E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30902746063602E+03 0.31379766851976E+03 0.30415000000005E+03 0.30415000000005E+03
 0.30798353082859E+03 0.31375893877017E+03 0.30415000000005E+03 0.30415000000005E+03 0.32828157238048E+03
 0.30422035698679E+03 0.78025001601316E+03 0.76715331023699E+03 0.31313313786368E+03 0.95184685091383E+03
 0.63714804736083E+03 0.48009311352501E+03 0.39260845924489E+03 0.46968476517758E+03 0.97481215852720E+03
 0.38603316726129E+03 0.38954146963957E+03 0.37892354117516E+03 0.97211071227582E+03 0.48009311352501E+03
 0.39260845924489E+03 0.46968476517758E+03 0.97481215852720E+03 0.38603316726129E+03 0.38954146963957E+03
 0.37892354117516E+03 0.97211071227582E+03 0.93786855693113E+02 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.41508438001457E+03 0.13140312569800E+01
 0.13140312569800E+01 0.19577695772775E+00 0.66377416579694E+00 0.30415072314840E+03 0.32166118583757E+03
 0.31391252399476E+03 0.31367000702494E+03 0.23000000000000E+00 0.00000000000000E+00 0.22666548830225E+00
 0.00000000000000E+00 -.13774924861103E+01 0.99969658229596E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30422025540736E+03 0.32830179884818E+03
 0.30417629195250E+03 0.30512564003851E+03 0.30415000000005E+03 0.30415000000005E+03 0.30417610107845E+03
 0.30512601390546E+03 0.30415000000005E+03 0.30415000000005E+03 0.30417629195250E+03 0.30512564003851E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30417610107845E+03 0.30512601390546E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30489252984256E+03 0.30415000000005E+03 0.36468419933271E+01 0.35958331008323E+01
 -.86075484202765E+00 0.12263474054701E+03 0.12349979916325E+03 0.32723363152788E+01 -.18244272520875E+01
 0.32570844984610E+01 0.10144568465799E+03 0.32447978976801E+01 -.17757448836778E+01 0.32296896642557E+01
 0.10149160106589E+03 0.32723363152788E+01 -.18244272520874E+01 0.32570844984610E+01 0.10144568465799E+03
 0.32447978976800E+01 -.17757448836778E+01 0.32296896642557E+01 0.10149160106589E+03 0.47695648840811E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33650208719959E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.89006071732697E-02 0.79113688487136E-02 0.00000000000000E+00 0.89006071732697E-02 0.79113688487136E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14363094973178E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14363094973178E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14592212659753E+00 0.16412541289726E+00 0.31299450276940E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    120.56694828
 0.42077542437118E+00 0.30528162704656E+03 0.39419270343082E+03 0.38172217146519E+03 0.37872217846560E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21512729047760E+00 0.00000000000000E+00 -.63959583527311E+01
 0.99623028093991E-03 0.23809170032030E+00 0.80000000000000E+04 0.30000000000000E+04 0.33600499258218E+02
 0.12600187221832E+02 0.31017543132993E+03 0.30415000000005E+03 0.30965422232086E+03 0.31513891776609E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30852243688449E+03 0.31509948193162E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30965422232086E+03 0.31513891776609E+03 0.30415000000005E+03 0.30415000000005E+03
 0.30852243688449E+03 0.31509948193162E+03 0.30415000000005E+03 0.30415000000005E+03 0.33118486694063E+03
 0.30423949790846E+03 0.84519420170107E+03 0.83045259162331E+03 0.33365845588640E+03 0.98585035297019E+03
 0.65052360480436E+03 0.51012180376977E+03 0.43077474446592E+03 0.49841980955082E+03 0.10310210630128E+04
 0.41578036669917E+03 0.42796614299812E+03 0.40782608298829E+03 0.10285837583199E+04 0.51012180376977E+03
 0.43077474446592E+03 0.49841980955082E+03 0.10310210630128E+04 0.41578036669917E+03 0.42796614299812E+03
 0.40782608298829E+03 0.10285837583199E+04 0.10061517632287E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.42009562334180E+03 0.12087165859189E+01
 0.12087165859189E+01 0.22672170604313E+00 0.59988441697271E+00 0.30415172803214E+03 0.32316570254458E+03
 0.31556157786810E+03 0.31528327701203E+03 0.23000000000000E+00 0.00000000000000E+00 0.22613525761430E+00
 0.00000000000000E+00 -.16068002419300E+01 0.99969101621462E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30423938334218E+03 0.33120604896033E+03
 0.30418283763674E+03 0.30527682686987E+03 0.30415000000005E+03 0.30415000000005E+03 0.30418258723577E+03
 0.30527728517928E+03 0.30415000000005E+03 0.30415000000005E+03 0.30418283763674E+03 0.30527682686987E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30418258723577E+03 0.30527728517928E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30503013319793E+03 0.30415000000005E+03 0.43015499650234E+01 0.42320855042260E+01
 -.10542513537741E+01 0.13502198958461E+03 0.13608151219516E+03 0.38393120386626E+01 -.20247889158748E+01
 0.38193486152011E+01 0.10950484632967E+03 0.38067636316474E+01 -.19696001975705E+01 0.37870026701188E+01
 0.10955663738463E+03 0.38393120386626E+01 -.20247889158747E+01 0.38193486152011E+01 0.10950484632967E+03
 0.38067636316474E+01 -.19696001975706E+01 0.37870026701188E+01 0.10955663738463E+03 0.53594792413017E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33885695042421E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.80349912806058E-03 0.21779552483007E-01 0.00000000000000E+00 0.80349912806058E-03 0.21779552483007E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14360222496675E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14360222496675E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14591401303273E+00 0.16290104606970E+00 0.31532872658208E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    130.15138845
 0.32707677325397E+00 0.30568414787897E+03 0.39734499952943E+03 0.38735162099891E+03 0.38476628025008E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21377100579612E+00 0.00000000000000E+00 -.71895663672805E+01
 0.10612915265291E-02 0.25786697221354E+00 0.75379853697353E+04 0.28267445136507E+04 0.31023748141639E+02
 0.11633905553115E+02 0.31090238606261E+03 0.30415000000005E+03 0.31024214750948E+03 0.31636065115514E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30903648253630E+03 0.31632091505538E+03 0.30415000000005E+03
 0.30415000000005E+03 0.31024214750948E+03 0.31636065115514E+03 0.30415000000005E+03 0.30415000000005E+03
 0.30903648253630E+03 0.31632091505538E+03 0.30415000000005E+03 0.30415000000005E+03 0.33375337623526E+03
 0.30426026010210E+03 0.90196462538401E+03 0.88592360700082E+03 0.35178306702348E+03 0.10105756694780E+04
 0.65703368711944E+03 0.53683881346034E+03 0.46399651766192E+03 0.52407997842075E+03 0.10765740299968E+04
 0.44255190918076E+03 0.46139710734666E+03 0.43395243880292E+03 0.10743479234908E+04 0.53683881346034E+03
 0.46399651766192E+03 0.52407997842075E+03 0.10765740299968E+04 0.44255190918076E+03 0.46139710734666E+03
 0.43395243880292E+03 0.10743479234908E+04 0.10621283430967E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.42421806948655E+03 0.11332954806268E+01
 0.11332954806268E+01 0.25547502655504E+00 0.55497941738223E+00 0.30415295419368E+03 0.32445118754410E+03
 0.31694111973163E+03 0.31663296642628E+03 0.23000000000000E+00 0.00000000000000E+00 0.22561688392048E+00
 0.00000000000000E+00 -.19905136084888E+01 0.99968319894964E-03 0.10409238988365E-02 0.80000000000000E+04
 0.30000000000000E+04 0.76854801863442E+04 0.28820550698791E+04 0.30426013190195E+03 0.33377577237660E+03
 0.30418960086785E+03 0.30541952746036E+03 0.30415000000005E+03 0.30415000000005E+03 0.30418929038377E+03
 0.30542006682696E+03 0.30415000000005E+03 0.30415000000005E+03 0.30418960086785E+03 0.30541952746036E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30418929038377E+03 0.30542006682696E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30516114764052E+03 0.30415000000005E+03 0.50499711976381E+01 0.49591491443078E+01
 -.10666687774692E+01 0.14562000401652E+03 0.14669200613788E+03 0.44612384484394E+01 -.20362560707964E+01
 0.44362768089725E+01 0.11680851735200E+03 0.44221961089391E+01 -.19753579038415E+01 0.43975007836561E+01
 0.11686540003604E+03 0.44612384484394E+01 -.20362560707964E+01 0.44362768089725E+01 0.11680851735200E+03
 0.44221961089390E+01 -.19753579038415E+01 0.43975007836560E+01 0.11686540003604E+03 0.58711274679799E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34046776482157E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.40640929806770E-01 0.00000000000000E+00 0.00000000000000E+00 0.40640929806770E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14371259375800E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14371259375800E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14590248769932E+00 0.15983733347394E+00 0.32134624844345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    140.08891834
 0.25150523480432E+00 0.30618221372780E+03 0.40013954113206E+03 0.39226262123527E+03 0.39010363777634E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21235102738165E+00 0.00000000000000E+00 -.81895773042981E+01
 0.13801816934928E-02 0.27858049300263E+00 0.57963382920653E+04 0.21736268595245E+04 0.28717014295486E+02
 0.10768880360807E+02 0.31165839329656E+03 0.30415000000005E+03 0.31084936589880E+03 0.31759140690637E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30957407084963E+03 0.31755156693865E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31084936589880E+03 0.31759140690637E+03 0.30415000000006E+03 0.30415000000006E+03
 0.30957407084964E+03 0.31755156693865E+03 0.30415000000006E+03 0.30415000000006E+03 0.33627889395941E+03
 0.30430473624605E+03 0.95628299101330E+03 0.93919814450904E+03 0.36970656822640E+03 0.10314592941598E+04
 0.65990419309226E+03 0.56304041918232E+03 0.49639307606938E+03 0.54937496651754E+03 0.11184563741974E+04
 0.46904357033306E+03 0.49398194012440E+03 0.45995739972817E+03 0.11164186034104E+04 0.56304041918232E+03
 0.49639307606938E+03 0.54937496651754E+03 0.11184563741974E+04 0.46904357033306E+03 0.49398194012440E+03
 0.45995739972817E+03 0.11164186034104E+04 0.11131590361090E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.42827556628948E+03 0.10919367494132E+01
 0.10919367494132E+01 0.28528761622017E+00 0.52510658782061E+00 0.30415472050377E+03 0.32560781994836E+03
 0.31809771071668E+03 0.31776172997864E+03 0.23000000000000E+00 0.00000000000000E+00 0.22505860124083E+00
 0.00000000000000E+00 -.26478876215936E+01 0.99967090550713E-03 0.18999305986831E-01 0.80000000000000E+04
 0.30000000000000E+04 0.42106801193397E+03 0.15790050447524E+03 0.30430453930792E+03 0.33630264911475E+03
 0.30420195299508E+03 0.30557680275157E+03 0.30415000000006E+03 0.30415000000006E+03 0.30420146314663E+03
 0.30557742703206E+03 0.30415000000006E+03 0.30415000000006E+03 0.30420195299508E+03 0.30557680275157E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30420146314663E+03 0.30557742703206E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30530186993100E+03 0.30415000000005E+03 0.81983792265433E+01 0.80564317949869E+01
 0.19393414861305E+01 0.15800853342478E+03 0.15605949523121E+03 0.65582455394595E+01 0.95917873927122E+00
 0.65234550661007E+01 0.12655236703840E+03 0.64791860292127E+01 0.10254290917555E+01 0.64448500922771E+01
 0.12661394249794E+03 0.65582455394595E+01 0.95917873927129E+00 0.65234550661007E+01 0.12655236703840E+03
 0.64791860292127E+01 0.10254290917550E+01 0.64448500922771E+01 0.12661394249794E+03 0.65217207564766E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34172695840911E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.59417474234309E-01 0.00000000000000E+00 0.00000000000000E+00 0.59417474234309E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14428994841537E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14428994841537E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14588442116542E+00 0.15772481777490E+00 0.32560781994836E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    150.77538677
 0.19146565158835E+00 0.30677766279341E+03 0.40272975094784E+03 0.39660590792126E+03 0.39484786565465E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21079829993087E+00 0.00000000000000E+00 -.93386891021519E+01
 0.18129734978483E-02 0.30124592239100E+00 0.44126403444366E+04 0.16547401291637E+04 0.26556376054832E+02
 0.99586410205618E+01 0.31246325243882E+03 0.30415000000005E+03 0.31149465716538E+03 0.31886769787180E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31015135303608E+03 0.31882789357021E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31149465716538E+03 0.31886769787180E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31015135303608E+03 0.31882789357021E+03 0.30415000000006E+03 0.30415000000006E+03 0.33884807384194E+03
 0.30437460299104E+03 0.10093314879665E+04 0.99139655494517E+03 0.38797240674181E+03 0.10497874948612E+04
 0.65987522608567E+03 0.58941963492222E+03 0.52911172242304E+03 0.57493752060935E+03 0.11583764204036E+04
 0.49587914598902E+03 0.52687504736951E+03 0.48642489970484E+03 0.11565116334148E+04 0.58941963492222E+03
 0.52911172242304E+03 0.57493752060935E+03 0.11583764204036E+04 0.49587914598902E+03 0.52687504736951E+03
 0.48642489970484E+03 0.11565116334148E+04 0.11607935667778E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43261505853541E+03 0.10919442946379E+01
 0.10919442946379E+01 0.31734702150473E+00 0.49584522191596E+00 0.30415797509229E+03 0.32665305811956E+03
 0.31921700516245E+03 0.31885762810447E+03 0.23000000000000E+00 0.00000000000000E+00 0.22443105281992E+00
 0.00000000000000E+00 -.35054372725747E+01 0.99965174513896E-03 0.37833334569216E-01 0.80000000000000E+04
 0.30000000000000E+04 0.21145373758594E+03 0.79295151594728E+02 0.30437425483590E+03 0.33887323936973E+03
 0.30422012839774E+03 0.30574954294423E+03 0.30415000000006E+03 0.30415000000006E+03 0.30421934750122E+03
 0.30575025665561E+03 0.30415000000006E+03 0.30415000000006E+03 0.30422012839774E+03 0.30574954294423E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30421934750122E+03 0.30575025665561E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30545424809473E+03 0.30415000000005E+03 0.11771829415330E+02 0.11540246639620E+02
 0.55187902789036E+01 0.16996310105433E+03 0.16441671682403E+03 0.89927692923171E+01 0.44881506298815E+01
 0.89429673144075E+01 0.13594991628681E+03 0.88691522784607E+01 0.45592753955316E+01 0.88201389064862E+01
 0.13601567200536E+03 0.89927692923171E+01 0.44881506298810E+01 0.89429673144076E+01 0.13594991628681E+03
 0.88691522784606E+01 0.45592753955316E+01 0.88201389064862E+01 0.13601567200536E+03 0.71751030576156E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34289966546270E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77268614926643E-01 0.00000000000000E+00 0.00000000000000E+00 0.77268614926643E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14503832985745E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14503832985745E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14586178042091E+00 0.15719439238630E+00 0.32665305811956E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    160.39655846
 0.15313508689957E+00 0.30731373243182E+03 0.40475175225822E+03 0.39977802571375E+03 0.39830536936238E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20937563696110E+00 0.00000000000000E+00 -.10350446412007E+02
 0.22667673267885E-02 0.32203629590587E+00 0.35292550344522E+04 0.13234706379196E+04 0.24841920310555E+02
 0.93157201164582E+01 0.31318609145950E+03 0.30415000000005E+03 0.31207398939504E+03 0.31999049806187E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31067482047597E+03 0.31995085461023E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31207398939504E+03 0.31999049806187E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31067482047597E+03 0.31995085461023E+03 0.30415000000006E+03 0.30415000000006E+03 0.34105494360853E+03
 0.30445579827512E+03 0.10521613142014E+04 0.10334613951781E+04 0.40327461972048E+03 0.10624950706508E+04
 0.65720407783176E+03 0.61141220985753E+03 0.55660464685709E+03 0.59615712577221E+03 0.11909001631258E+04
 0.51832986073824E+03 0.55450444427189E+03 0.50851364703925E+03 0.11891695994318E+04 0.61141220985753E+03
 0.55660464685709E+03 0.59615712577220E+03 0.11909001631258E+04 0.51832986073824E+03 0.55450444427189E+03
 0.50851364703925E+03 0.11891695994318E+04 0.11978866065094E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43580683926853E+03 0.10919509380735E+01
 0.10919509380735E+01 0.34621053659011E+00 0.47034186916667E+00 0.30416285807798E+03 0.32744026288428E+03
 0.32014137082698E+03 0.31976692536220E+03 0.23000000000000E+00 0.00000000000000E+00 0.22384150299287E+00
 0.00000000000000E+00 -.43078000392061E+01 0.99962777810325E-03 0.54512168714784E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14675622321793E+03 0.55033583706722E+02 0.30445533968753E+03 0.34108131941090E+03
 0.30424053071108E+03 0.30590756978082E+03 0.30415000000006E+03 0.30415000000006E+03 0.30423942029370E+03
 0.30590836013620E+03 0.30415000000006E+03 0.30415000000006E+03 0.30424053071108E+03 0.30590756978082E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30423942029370E+03 0.30590836013620E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30559309383310E+03 0.30415000000005E+03 0.15119360764066E+02 0.14773056056745E+02
 0.89967078030312E+01 0.17959365887687E+03 0.17055196753482E+03 0.11336734731792E+02 0.78869570450154E+01
 0.11270002050801E+02 0.14348597158575E+03 0.11173100733790E+02 0.79611298240181E+01 0.11107570707422E+02
 0.14355418387891E+03 0.11336734731792E+02 0.78869570450153E+01 0.11270002050801E+02 0.14348597158575E+03
 0.11173100733790E+02 0.79611298240181E+01 0.11107570707422E+02 0.14355418387891E+03 0.77255968777292E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34379578591054E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.90921201047324E-01 0.00000000000000E+00 0.00000000000000E+00 0.90921201047324E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14545927723702E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14545927723702E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14584102370222E+00 0.15679292095901E+00 0.32744026288428E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    170.12789446
 0.12650189156944E+00 0.30780127025509E+03 0.40652656981170E+03 0.40236359076514E+03 0.40110171232952E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20791673460250E+00 0.00000000000000E+00 -.11339685631082E+02
 0.27440000189680E-02 0.34339377625472E+00 0.29154518748906E+04 0.10932944530840E+04 0.23296869521787E+02
 0.87363260706701E+01 0.31391446713983E+03 0.30415000000006E+03 0.31265842415163E+03 0.32110612511267E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31120719276286E+03 0.32106674582696E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31265842415163E+03 0.32110612511267E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31120719276286E+03 0.32106674582696E+03 0.30415000000006E+03 0.30415000000006E+03 0.34319027002276E+03
 0.30455554990569E+03 0.10907073863625E+04 0.10710025476589E+04 0.41741951468534E+03 0.10717453495036E+04
 0.65223873724483E+03 0.63180884640227E+03 0.58234493807476E+03 0.61556035723168E+03 0.12202533526756E+04
 0.53917883088336E+03 0.58036688794239E+03 0.52879598472794E+03 0.12186418668142E+04 0.63180884640227E+03
 0.58234493807476E+03 0.61556035723168E+03 0.12202533526756E+04 0.53917883088335E+03 0.58036688794239E+03
 0.52879598472794E+03 0.12186418668142E+04 0.12298640286705E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.43842102708596E+03 0.10919574337398E+01
 0.10919574337398E+01 0.37540454459131E+00 0.44542547934680E+00 0.30417096297125E+03 0.32810401938514E+03
 0.32099709063488E+03 0.32061292557328E+03 0.23000000000000E+00 0.00000000000000E+00 0.22322180688808E+00
 0.00000000000000E+00 -.51374956101893E+01 0.99959295386143E-03 0.71213048815901E-01 0.80000000000000E+04
 0.30000000000000E+04 0.11233896221297E+03 0.42127110829864E+02 0.30455499126868E+03 0.34321773125438E+03
 0.30426508424971E+03 0.30606893420583E+03 0.30415000000006E+03 0.30415000000006E+03 0.30426358475357E+03
 0.30606979604663E+03 0.30415000000006E+03 0.30415000000006E+03 0.30426508424971E+03 0.30606893420583E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30426358475357E+03 0.30606979604663E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30573488882337E+03 0.30415000000005E+03 0.18575987432199E+02 0.18078121322348E+02
 0.12692546458686E+02 0.18832880591621E+03 0.17557279672523E+03 0.13822428599747E+02 0.11476263940097E+02
 0.13736392177775E+02 0.15031654721409E+03 0.13619266104424E+02 0.11552193371931E+02 0.13534957571239E+02
 0.15038596719659E+03 0.13822428599747E+02 0.11476263940097E+02 0.13736392177775E+02 0.15031654721409E+03
 0.13619266104424E+02 0.11552193371931E+02 0.13534957571239E+02 0.15038596719659E+03 0.82464049251308E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34458478253875E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10309211697124E+00 0.00000000000000E+00 0.00000000000000E+00 0.10309211697124E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14613898334275E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14613898334275E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14581988859033E+00 0.15645177001242E+00 0.32810401938514E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    180.62355154
 0.10838362154368E+00 0.30821812000634E+03 0.40814480931081E+03 0.40453467048558E+03 0.40341958194389E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20633018999572E+00 0.00000000000000E+00 -.12314662169008E+02
 0.32027058971009E-02 0.36669004170047E+00 0.24978878039478E+04 0.93670792648043E+03 0.21816790995744E+02
 0.81812966234041E+01 0.31467095412936E+03 0.30415000000006E+03 0.31326827844617E+03 0.32226069914742E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31176451814783E+03 0.32222161839913E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31326827844617E+03 0.32226069914742E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31176451814783E+03 0.32222161839913E+03 0.30415000000006E+03 0.30415000000006E+03 0.34535990694167E+03
 0.30467837240148E+03 0.11269829368243E+04 0.11058292432202E+04 0.43097197571481E+03 0.10779125249372E+04
 0.64478568934377E+03 0.65154691370056E+03 0.60757644558784E+03 0.63391614161536E+03 0.12478075368270E+04
 0.55935605388183E+03 0.60571628623031E+03 0.54805324566460E+03 0.12463105870657E+04 0.65154691370056E+03
 0.60757644558784E+03 0.63391614161536E+03 0.12478075368270E+04 0.55935605388182E+03 0.60571628623031E+03
 0.54805324566460E+03 0.12463105870657E+04 0.12583846130391E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44062076836774E+03 0.10919638358396E+01
 0.10919638358396E+01 0.40689151582914E+00 0.41957169751382E+00 0.30418390135037E+03 0.32869438859622E+03
 0.32183845076913E+03 0.32144930932184E+03 0.23000000000000E+00 0.00000000000000E+00 0.22252637857724E+00
 0.00000000000000E+00 -.59853428493266E+01 0.99954206935247E-03 0.89153401659692E-01 0.80000000000000E+04
 0.30000000000000E+04 0.89732975422933E+02 0.33649865783600E+02 0.30467774285471E+03 0.34538819793164E+03
 0.30429390348722E+03 0.30623819351468E+03 0.30415000000006E+03 0.30415000000006E+03 0.30429195649429E+03
 0.30623912529933E+03 0.30415000000006E+03 0.30415000000006E+03 0.30429390348722E+03 0.30623819351468E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30429195649429E+03 0.30623912529933E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30588281900093E+03 0.30415000000005E+03 0.22322822215225E+02 0.21626671304317E+02
 0.16795603601468E+02 0.19676305076785E+03 0.17988346914838E+03 0.16552127269666E+02 0.15449625037646E+02
 0.16446037681242E+02 0.15691283904081E+03 0.16307508595625E+02 0.15526659113527E+02 0.16203785097916E+02
 0.15698283080749E+03 0.16552127269666E+02 0.15449625037646E+02 0.16446037681242E+02 0.15691283904081E+03
 0.16307508595625E+02 0.15526659113527E+02 0.16203785097916E+02 0.15698283080749E+03 0.87698124452054E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34530355558094E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11419664664958E+00 0.00000000000000E+00 0.00000000000000E+00 0.11419664664958E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14683453313506E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14683453313506E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14579851409050E+00 0.15614656756874E+00 0.32869438859622E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    191.46383309
 0.97940926665984E-01 0.30854328254706E+03 0.40951015077893E+03 0.40621388789987E+03 0.40518147236591E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20469125227677E+00 0.00000000000000E+00 -.13149462137763E+02
 0.35441836344585E-02 0.39085593816316E+00 0.22572193839562E+04 0.84645726898356E+03 0.20467899343160E+02
 0.76754622536852E+01 0.31543249518922E+03 0.30415000000006E+03 0.31388413076698E+03 0.32341463617010E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31232987800496E+03 0.32337593014437E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31388413076698E+03 0.32341463617010E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31232987800496E+03 0.32337593014437E+03 0.30415000000006E+03 0.30415000000006E+03 0.34746712132097E+03
 0.30482620186771E+03 0.11589470673596E+04 0.11358876615653E+04 0.44280602131476E+03 0.10799555612055E+04
 0.63493550978417E+03 0.66943002587881E+03 0.63058452134655E+03 0.65008780083609E+03 0.12713136002077E+04
 0.57762231339793E+03 0.62883393869893E+03 0.56506170705444E+03 0.12699221411526E+04 0.66943002587880E+03
 0.63058452134655E+03 0.65008780083608E+03 0.12713136002077E+04 0.57762231339793E+03 0.62883393869893E+03
 0.56506170705444E+03 0.12699221411526E+04 0.12814325970344E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44231952966732E+03 0.10919693175506E+01
 0.10919693175506E+01 0.43941236048014E+00 0.39402138895446E+00 0.30420366874295E+03 0.32918211134649E+03
 0.32262075091411E+03 0.32223190048729E+03 0.23000000000000E+00 0.00000000000000E+00 0.22177938651818E+00
 0.00000000000000E+00 -.67247632424984E+01 0.99946982172960E-03 0.10768549220142E+00 0.80000000000000E+04
 0.30000000000000E+04 0.74290415881061E+02 0.27858905955398E+02 0.30482550704408E+03 0.34749596421003E+03
 0.30432748007844E+03 0.30641137452611E+03 0.30415000000006E+03 0.30415000000006E+03 0.30432503108503E+03
 0.30641236796002E+03 0.30415000000006E+03 0.30415000000006E+03 0.30432748007844E+03 0.30641137452611E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30432503108503E+03 0.30641236796002E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30603408061347E+03 0.30415000000005E+03 0.26168137722929E+02 0.25221675346917E+02
 0.21139034999854E+02 0.20444321942435E+03 0.18319848924950E+03 0.19450761731007E+02 0.19630648436695E+02
 0.19325939762783E+02 0.16291616347167E+03 0.19165891668485E+02 0.19707188785491E+02 0.19044178314744E+02
 0.16298519826384E+03 0.19450761731006E+02 0.19630648436696E+02 0.19325939762783E+02 0.16291616347167E+03
 0.19165891668485E+02 0.19707188785491E+02 0.19044178314744E+02 0.16298519826384E+03 0.92676126386710E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34590404201113E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12308887056270E+00 0.00000000000000E+00 0.00000000000000E+00 0.12308887056270E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14733464290773E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14733464290773E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14578001536749E+00 0.15589429729659E+00 0.32918211134649E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    200.49740105
 0.93620768530991E-01 0.30876386583855E+03 0.41043001125867E+03 0.40725732370274E+03 0.40625555278264E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20333587884085E+00 0.00000000000000E+00 -.13760402318460E+02
 0.37077296159164E-02 0.41092941461431E+00 0.21576546373980E+04 0.80912048902426E+03 0.19468063651536E+02
 0.73005238693261E+01 0.31604738155392E+03 0.30415000000007E+03 0.31438326317108E+03 0.32434117913361E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31278916064419E+03 0.32430281615827E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31438326317108E+03 0.32434117913361E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31278916064419E+03 0.32430281615827E+03 0.30415000000006E+03 0.30415000000006E+03 0.34911653759760E+03
 0.30496482958355E+03 0.11816850313885E+04 0.11568674068276E+04 0.45093097064416E+03 0.10785778606819E+04
 0.62539223518452E+03 0.68245190181996E+03 0.64734466867164E+03 0.66162578299064E+03 0.12871321135382E+04
 0.59091366325773E+03 0.64567780634887E+03 0.57720970027596E+03 0.12858206565023E+04 0.68245190181996E+03
 0.64734466867164E+03 0.66162578299063E+03 0.12871321135382E+04 0.59091366325773E+03 0.64567780634887E+03
 0.57720970027596E+03 0.12858206565023E+04 0.12961368804453E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44337033939119E+03 0.10919733293271E+01
 0.10919733293271E+01 0.46651306435597E+00 0.37363548519154E+00 0.30422619344767E+03 0.32950655857809E+03
 0.32320946425054E+03 0.32282450295586E+03 0.23000000000000E+00 0.00000000000000E+00 0.22113507897353E+00
 0.00000000000000E+00 -.72868193269396E+01 0.99939027569386E-03 0.12318474419230E+00 0.80000000000000E+04
 0.30000000000000E+04 0.64943106814523E+02 0.24353665055446E+02 0.30496407740367E+03 0.34914560463069E+03
 0.30435795935013E+03 0.30655320597666E+03 0.30415000000006E+03 0.30415000000006E+03 0.30435507250105E+03
 0.30655424187642E+03 0.30415000000006E+03 0.30415000000006E+03 0.30435795935013E+03 0.30655320597666E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30435507250105E+03 0.30655424187642E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30615781670087E+03 0.30415000000006E+03 0.29315016730632E+02 0.28126155074946E+02
 0.24791531296578E+02 0.21015171170809E+03 0.18523622275502E+03 0.21895781602284E+02 0.23132150802445E+02
 0.21759215638996E+02 0.16739561720309E+03 0.21579563524280E+02 0.23207273915206E+02 0.21446760924242E+02
 0.16746291939821E+03 0.21895781602284E+02 0.23132150802445E+02 0.21759215638996E+02 0.16739561720309E+03
 0.21579563524280E+02 0.23207273915206E+02 0.21446760924242E+02 0.16746291939821E+03 0.96519112687696E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34632881692299E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12936333012127E+00 0.00000000000000E+00 0.00000000000000E+00 0.12936333012127E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14807975953516E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14807975953516E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14576608455746E+00 0.15572504980139E+00 0.32950655857809E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    211.33768260
 0.91775105957161E-01 0.30900426000223E+03 0.41130490775158E+03 0.40817535682269E+03 0.40718103647896E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20173311221905E+00 0.00000000000000E+00 -.14328111053361E+02
 0.37822935356141E-02 0.43477671667056E+00 0.21151187565619E+04 0.79316953371070E+03 0.18400249353881E+02
 0.69000935077053E+01 0.31676010604969E+03 0.30415000000010E+03 0.31496430944486E+03 0.32540839160664E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31332452372722E+03 0.32537046731569E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31496430944486E+03 0.32540839160664E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31332452372722E+03 0.32537046731569E+03 0.30415000000006E+03 0.30415000000006E+03 0.35096872073099E+03
 0.30514955871190E+03 0.12048579069761E+04 0.11778936179995E+04 0.45867403651226E+03 0.10737547116811E+04
 0.61278730498626E+03 0.69600164492217E+03 0.66465788356097E+03 0.67348132750049E+03 0.13019135318360E+04
 0.60474168683326E+03 0.66308394409299E+03 0.58970048024690E+03 0.13006902445306E+04 0.69600164492217E+03
 0.66465788356097E+03 0.67348132750049E+03 0.13019135318360E+04 0.60474168683326E+03 0.66308394409299E+03
 0.58970048024690E+03 0.13006902445306E+04 0.13091091045387E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44428903942206E+03 0.10919770572520E+01
 0.10919770572520E+01 0.49903390900697E+00 0.35026567071976E+00 0.30426229598900E+03 0.32980961900636E+03
 0.32384405218518E+03 0.32346750227117E+03 0.23000000000000E+00 0.00000000000000E+00 0.22033708504039E+00
 0.00000000000000E+00 -.78140419147120E+01 0.99926649043265E-03 0.14188901173582E+00 0.80000000000000E+04
 0.30000000000000E+04 0.56382096838441E+02 0.21143286314416E+02 0.30514873830584E+03 0.35099779836217E+03
 0.30439726858044E+03 0.30671991214238E+03 0.30415000000006E+03 0.30415000000006E+03 0.30439384169749E+03
 0.30672098757779E+03 0.30415000000006E+03 0.30415000000006E+03 0.30439726858044E+03 0.30671991214238E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30439384169749E+03 0.30672098757779E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30630307345425E+03 0.30415000000006E+03 0.32980313462028E+02 0.31462309181987E+02
 0.29170450192734E+02 0.21619403511654E+03 0.18687773267284E+03 0.24843649045843E+02 0.27313204699153E+02
 0.24700892627242E+02 0.17214352983732E+03 0.24492819634312E+02 0.27385490253896E+02 0.24354614240593E+02
 0.17220770823059E+03 0.24843649045843E+02 0.27313204699153E+02 0.24700892627242E+02 0.17214352983732E+03
 0.24492819634312E+02 0.27385490253896E+02 0.24354614240593E+02 0.17220770823059E+03 0.10074796263347E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34673184233509E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13498515527251E+00 0.00000000000000E+00 0.00000000000000E+00 0.13498515527251E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14877057813705E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14877057813705E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14575312409502E+00 0.15556731180750E+00 0.32980961900636E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    220.30870885
 0.91921892361723E-01 0.30919735291460E+03 0.41186981463445E+03 0.40872386564287E+03 0.40772142592208E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20043272763985E+00 0.00000000000000E+00 -.14695590410825E+02
 0.37762531278386E-02 0.45421060940873E+00 0.21185020519477E+04 0.79443826948039E+03 0.17612974761673E+02
 0.66048655356273E+01 0.31732648454522E+03 0.30415000000014E+03 0.31542812012952E+03 0.32625140619556E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31375194451235E+03 0.32621384755349E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31542812012952E+03 0.32625140619556E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31375194451235E+03 0.32621384755349E+03 0.30415000000006E+03 0.30415000000006E+03 0.35240210788932E+03
 0.30531655646843E+03 0.12211347793047E+04 0.11924451221458E+04 0.46355305227378E+03 0.10676011179336E+04
 0.60173030039841E+03 0.70569734378111E+03 0.67687203190575E+03 0.68189887378392E+03 0.13111226982773E+04
 0.61464293922962E+03 0.67536962705478E+03 0.59858192027292E+03 0.13099669627143E+04 0.70569734378111E+03
 0.67687203190575E+03 0.68189887378392E+03 0.13111226982773E+04 0.61464293922962E+03 0.67536962705478E+03
 0.59858192027292E+03 0.13099669627143E+04 0.13166352579464E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44483402907147E+03 0.10919794703632E+01
 0.10919794703632E+01 0.52594698776548E+00 0.33182646461429E+00 0.30430110482380E+03 0.32999747839521E+03
 0.32431298719814E+03 0.32394601089132E+03 0.23000000000000E+00 0.00000000000000E+00 0.21965766531582E+00
 0.00000000000000E+00 -.81608637134223E+01 0.99913562831938E-03 0.15746866456106E+00 0.80000000000000E+04
 0.30000000000000E+04 0.50803758463944E+02 0.19051409423979E+02 0.30531568297151E+03 0.35243102149960E+03
 0.30443134892684E+03 0.30685385766687E+03 0.30415000000006E+03 0.30415000000006E+03 0.30442747445618E+03
 0.30685495673082E+03 0.30415000000006E+03 0.30415000000006E+03 0.30443134892684E+03 0.30685385766687E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30442747445618E+03 0.30685495673082E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30641955095181E+03 0.30415000000006E+03 0.35890346617544E+02 0.34073238326971E+02
 0.32751221263919E+02 0.22058473279268E+03 0.18766975542244E+03 0.27267778232432E+02 0.30721808663208E+02
 0.27129639830081E+02 0.17560458904880E+03 0.26890829119100E+02 0.30791047610277E+02 0.26757839735722E+02
 0.17566555629963E+03 0.27267778232432E+02 0.30721808663208E+02 0.27129639830081E+02 0.17560458904880E+03
 0.26890829119100E+02 0.30791047610277E+02 0.26757839735722E+02 0.17566555629963E+03 0.10394153406420E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34699373372997E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13854204934673E+00 0.00000000000000E+00 0.00000000000000E+00 0.13854204934673E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14931054451327E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14931054451327E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14574469724390E+00 0.15546922967057E+00 0.32999747839521E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    230.31842790
 0.93032272669918E-01 0.30942289217443E+03 0.41236907697419E+03 0.40917663779599E+03 0.40815794890342E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19901433018082E+00 0.00000000000000E+00 -.14992070688813E+02
 0.37311814738542E-02 0.47548253133682E+00 0.21440929786072E+04 0.80403486697769E+03 0.16825013481585E+02
 0.63093800555943E+01 0.31794569178184E+03 0.30415000000023E+03 0.31593748953373E+03 0.32716136682054E+03
 0.30415000000006E+03 0.30415000000006E+03 0.31422232724649E+03 0.32712427608694E+03 0.30415000000006E+03
 0.30415000000006E+03 0.31593748953373E+03 0.32716136682054E+03 0.30415000000006E+03 0.30415000000006E+03
 0.31422232724649E+03 0.32712427608694E+03 0.30415000000006E+03 0.30415000000006E+03 0.35389140092598E+03
 0.30552148029210E+03 0.12367065704105E+04 0.12061721696874E+04 0.46775120112225E+03 0.10593480777516E+04
 0.58925812062378E+03 0.71513237821890E+03 0.68848787562586E+03 0.69003903883539E+03 0.13186042972503E+04
 0.62429146662308E+03 0.68706017445186E+03 0.60719343595555E+03 0.13175182430593E+04 0.71513237821890E+03
 0.68848787562586E+03 0.69003903883539E+03 0.13186042972503E+04 0.62429146662308E+03 0.68706017445186E+03
 0.60719343595555E+03 0.13175182430593E+04 0.13223229116972E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44528107098144E+03 0.10919814172567E+01
 0.10919814172567E+01 0.55597614489431E+00 0.31220418972195E+00 0.30435627802360E+03 0.33014948241615E+03
 0.32478098476435E+03 0.32442696080892E+03 0.23000000000000E+00 0.00000000000000E+00 0.21888089023405E+00
 0.00000000000000E+00 -.84441683451620E+01 0.99895171242784E-03 0.17496740663075E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45722801486584E+02 0.17146050557469E+02 0.30552053484999E+03 0.35391993668274E+03
 0.30447317745968E+03 0.30700216616879E+03 0.30415000000006E+03 0.30415000000006E+03 0.30446878601204E+03
 0.30700327773810E+03 0.30415000000006E+03 0.30415000000006E+03 0.30447317745968E+03 0.30700216616879E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30446878601204E+03 0.30700327773810E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30654881386528E+03 0.30415000000007E+03 0.38983926489164E+02 0.36801342110788E+02
 0.36704920414605E+02 0.22489957357709E+03 0.18801112856041E+03 0.29987273708509E+02 0.34462928839483E+02
 0.29864984597979E+02 0.17901288367464E+03 0.29584736827401E+02 0.34527392195913E+02 0.29468185039682E+02
 0.17906899939111E+03 0.29987273708509E+02 0.34462928839483E+02 0.29864984597979E+02 0.17901288367464E+03
 0.29584736827401E+02 0.34527392195913E+02 0.29468185039682E+02 0.17906899939111E+03 0.10719458679376E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34722391423745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14134855364336E+00 0.00000000000000E+00 0.00000000000000E+00 0.14134855364336E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14991693757669E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14991693757669E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573792484558E+00 0.15538999452971E+00 0.33014948241615E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    240.52392744
 0.94663307642738E-01 0.30965185493308E+03 0.41276589179312E+03 0.40951218652859E+03 0.40847380265832E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19760635907701E+00 0.00000000000000E+00 -.15220673208419E+02
 0.36668934831530E-02 0.49666610839910E+00 0.21816832249845E+04 0.81813120936919E+03 0.16107400655516E+02
 0.60402752458184E+01 0.31854999192272E+03 0.30415000000038E+03 0.31643663594840E+03 0.32804398341941E+03
 0.30415000000006E+03 0.30415000000007E+03 0.31468290128623E+03 0.32800734980414E+03 0.30415000000006E+03
 0.30415000000007E+03 0.31643663594840E+03 0.32804398341941E+03 0.30415000000006E+03 0.30415000000007E+03
 0.31468290128623E+03 0.32800734980414E+03 0.30415000000006E+03 0.30415000000007E+03 0.35531597754600E+03
 0.30574488415766E+03 0.12503198221834E+04 0.12180031446578E+04 0.47071957093015E+03 0.10495207469893E+04
 0.57644757820451E+03 0.72350196971418E+03 0.69851298132257E+03 0.69720340537723E+03 0.13239130798015E+04
 0.63287073761381E+03 0.69715639446082E+03 0.61480928509058E+03 0.13228932037951E+04 0.72350196971418E+03
 0.69851298132257E+03 0.69720340537723E+03 0.13239130798015E+04 0.63287073761381E+03 0.69715639446082E+03
 0.61480928509058E+03 0.13228932037951E+04 0.13258272770825E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44561028748920E+03 0.10919829184235E+01
 0.10919829184235E+01 0.58659264352747E+00 0.29321158595600E+00 0.30442673922659E+03 0.33025075803750E+03
 0.32520282369663E+03 0.32486393874344E+03 0.23000000000000E+00 0.00000000000000E+00 0.21807051481424E+00
 0.00000000000000E+00 -.86658842373455E+01 0.99871831340344E-03 0.19293272103129E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41465231803281E+02 0.15549461926230E+02 0.30574388043229E+03 0.35534404726269E+03
 0.30451670961385E+03 0.30714779990472E+03 0.30415000000006E+03 0.30415000000006E+03 0.30451180634182E+03
 0.30714891458999E+03 0.30415000000006E+03 0.30415000000006E+03 0.30451670961385E+03 0.30714779990472E+03
 0.30415000000006E+03 0.30415000000006E+03 0.30451180634182E+03 0.30714891458999E+03 0.30415000000006E+03
 0.30415000000006E+03 0.30667529851906E+03 0.30415000000008E+03 0.41944293445168E+02 0.39372088543419E+02
 0.40623292407983E+02 0.22872064090916E+03 0.18789423203913E+03 0.32705816787061E+02 0.38160938285992E+02
 0.32616390880303E+02 0.18204167485349E+03 0.32280232600692E+02 0.38220066198642E+02 0.32196857931748E+02
 0.18209245263314E+03 0.32705816787061E+02 0.38160938285993E+02 0.32616390880303E+02 0.18204167485349E+03
 0.32280232600692E+02 0.38220066198641E+02 0.32196857931748E+02 0.18209245263313E+03 0.11018717391472E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34739934449293E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14348886628406E+00 0.00000000000000E+00 0.00000000000000E+00 0.14348886628406E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15048608509020E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15048608509020E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573274754047E+00 0.15533648365339E+00 0.33025075803750E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    250.01524429
 0.96295439121706E-01 0.30986969805309E+03 0.41306296328970E+03 0.40975061635958E+03 0.40869423315233E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19633268117540E+00 0.00000000000000E+00 -.15368724031037E+02
 0.36047423701109E-02 0.51587149591186E+00 0.22192986845143E+04 0.83223700669285E+03 0.15507737999478E+02
 0.58154017498043E+01 0.31909478066277E+03 0.30415000000065E+03 0.31688840159254E+03 0.32883129163434E+03
 0.30415000000007E+03 0.30415000000007E+03 0.31510000111832E+03 0.32879509834419E+03 0.30415000000006E+03
 0.30415000000007E+03 0.31688840159254E+03 0.32883129163434E+03 0.30415000000007E+03 0.30415000000007E+03
 0.31510000111832E+03 0.32879509834419E+03 0.30415000000006E+03 0.30415000000007E+03 0.35655366426739E+03
 0.30596800902338E+03 0.12613278403922E+04 0.12274413494531E+04 0.47265376762862E+03 0.10398418474768E+04
 0.56482481101004E+03 0.73035785191350E+03 0.70646471346623E+03 0.70302486844410E+03 0.13271968362820E+04
 0.63991853196077E+03 0.70517006941074E+03 0.62103383653460E+03 0.13262341802777E+04 0.73035785191349E+03
 0.70646471346624E+03 0.70302486844409E+03 0.13271968362820E+04 0.63991853196077E+03 0.70517006941074E+03
 0.62103383653460E+03 0.13262341802777E+04 0.13275898358021E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44584328302433E+03 0.10919838906332E+01
 0.10919838906332E+01 0.61506659406095E+00 0.27644445108534E+00 0.30450731311264E+03 0.33030443301061E+03
 0.32555011924409E+03 0.32522662581917E+03 0.23000000000000E+00 0.00000000000000E+00 0.21730185978405E+00
 0.00000000000000E+00 -.88108855707776E+01 0.99845261898912E-03 0.20974824883317E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38140962055722E+02 0.14302860770896E+02 0.30596695168298E+03 0.35658119771718E+03
 0.30455931385196E+03 0.30727988169594E+03 0.30415000000005E+03 0.30415000000006E+03 0.30455399357902E+03
 0.30728098754262E+03 0.30415000000005E+03 0.30415000000006E+03 0.30455931385196E+03 0.30727988169594E+03
 0.30415000000005E+03 0.30415000000006E+03 0.30455399357902E+03 0.30728098754262E+03 0.30415000000005E+03
 0.30415000000006E+03 0.30679001544742E+03 0.30415000000012E+03 0.44505547834171E+02 0.41556566532117E+02
 0.44167504906073E+02 0.23181312026675E+03 0.18742477783614E+03 0.35202896898564E+02 0.41491310996484E+02
 0.35157057953002E+02 0.18449992843060E+03 0.34758707326638E+02 0.41544702986767E+02 0.34758707326638E+02
 0.18454505743666E+03 0.35202896898564E+02 0.41491310996484E+02 0.35157057953002E+02 0.18449992843060E+03
 0.34758707326638E+02 0.41544702986767E+02 0.34758707326638E+02 0.18454505743666E+03 0.11269730947241E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34751936783212E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14485004558297E+00 0.00000000000000E+00 0.00000000000000E+00 0.14485004558297E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15097303316945E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15097303316945E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572948919879E+00 0.15530754638976E+00 0.33030443301061E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    260.01070294
 0.97949102622272E-01 0.31009978803849E+03 0.41332350179482E+03 0.40995327841756E+03 0.40887971936783E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19502863545033E+00 0.00000000000000E+00 -.15481011585760E+02
 0.35438837859880E-02 0.53556271667643E+00 0.22574103675834E+04 0.84652888784378E+03 0.14937559600202E+02
 0.56015848500756E+01 0.31964946684561E+03 0.30415000000110E+03 0.31734982583407E+03 0.32962663468040E+03
 0.30415000000007E+03 0.30415000000009E+03 0.31552600194496E+03 0.32959090029408E+03 0.30415000000006E+03
 0.30415000000008E+03 0.31734982583407E+03 0.32962663468040E+03 0.30415000000007E+03 0.30415000000009E+03
 0.31552600194496E+03 0.32959090029408E+03 0.30415000000006E+03 0.30415000000008E+03 0.35778267282241E+03
 0.30621757670123E+03 0.12715667060546E+04 0.12361061740309E+04 0.47396453026807E+03 0.10292835722009E+04
 0.55294921928147E+03 0.73680112318673E+03 0.71368777557837E+03 0.70844376006099E+03 0.13293844265825E+04
 0.64656369421996E+03 0.71245428893062E+03 0.62686807344338E+03 0.13284780177176E+04 0.73680112318673E+03
 0.71368777557837E+03 0.70844376006099E+03 0.13293844265825E+04 0.64656369421996E+03 0.71245428893062E+03
 0.62686807344338E+03 0.13284780177176E+04 0.13283104220349E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44604101165523E+03 0.10919846279964E+01
 0.10919846279964E+01 0.64505297000797E+00 0.25969169573112E+00 0.30460954389013E+03 0.33032625634973E+03
 0.32587397857155E+03 0.32556764772329E+03 0.23000000000000E+00 0.00000000000000E+00 0.21647852672328E+00
 0.00000000000000E+00 -.89217359749458E+01 0.99811643337307E-03 0.22755901422207E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35155715660612E+02 0.13183393372729E+02 0.30621646770564E+03 0.35780957819805E+03
 0.30460547789317E+03 0.30741479027457E+03 0.30415000000005E+03 0.30415000000005E+03 0.30459960299086E+03
 0.30741587657695E+03 0.30415000000005E+03 0.30415000000005E+03 0.30460547789317E+03 0.30741479027457E+03
 0.30415000000005E+03 0.30415000000005E+03 0.30459960299086E+03 0.30741587657695E+03 0.30415000000005E+03
 0.30415000000005E+03 0.30690698581944E+03 0.30415000000019E+03 0.46990568334200E+02 0.43639384066028E+02
 0.47775987877452E+02 0.23464954932243E+03 0.18663468150559E+03 0.37785153491249E+02 0.44869148368978E+02
 0.37785153491249E+02 0.18676038508155E+03 0.37325204219630E+02 0.44915942736244E+02 0.37325204219630E+02
 0.18679908530550E+03 0.37785153491249E+02 0.44869148368978E+02 0.37785153491249E+02 0.18676038508155E+03
 0.37325204219630E+02 0.44915942736244E+02 0.37325204219630E+02 0.18679908530550E+03 0.11507421785120E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34760983771421E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14586417829898E+00 0.00000000000000E+00 0.00000000000000E+00 0.14586417829898E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15144405175175E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15144405175175E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572712942949E+00 0.15529460117435E+00 0.33032625634973E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    270.03749053
 0.99437974697244E-01 0.31033061647095E+03 0.41354985009565E+03 0.41012854624851E+03 0.40904030098537E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19375826981108E+00 0.00000000000000E+00 -.15558614975523E+02
 0.34908215564821E-02 0.55475835886071E+00 0.22917241315715E+04 0.85939654933930E+03 0.14420693031880E+02
 0.54077598869552E+01 0.32018838914075E+03 0.30415000000183E+03 0.31779944159801E+03 0.33039297623523E+03
 0.30415000000009E+03 0.30415000000013E+03 0.31594117487655E+03 0.33035769967040E+03 0.30415000000008E+03
 0.30415000000012E+03 0.31779944159801E+03 0.33039297623523E+03 0.30415000000009E+03 0.30415000000013E+03
 0.31594117487655E+03 0.33035769967040E+03 0.30415000000008E+03 0.30415000000012E+03 0.35894530776240E+03
 0.30648319559537E+03 0.12807335178000E+04 0.12437651976178E+04 0.47473662738129E+03 0.10186565110850E+04
 0.54154620056681E+03 0.74262016674531E+03 0.71998357900379E+03 0.71328578597821E+03 0.13306302268193E+04
 0.65258617998030E+03 0.71880753433814E+03 0.63212122981672E+03 0.13297763841621E+04 0.74262016674531E+03
 0.71998357900380E+03 0.71328578597821E+03 0.13306302268194E+04 0.65258617998030E+03 0.71880753433814E+03
 0.63212122981672E+03 0.13297763841621E+04 0.13282380529892E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44621223061182E+03 0.10919851375984E+01
 0.10919851375984E+01 0.67513333278960E+00 0.24376820061358E+00 0.30473248170232E+03 0.33031898272104E+03
 0.32616086584549E+03 0.32587240861781E+03 0.23000000000000E+00 0.00000000000000E+00 0.21564025824793E+00
 0.00000000000000E+00 -.89983200264796E+01 0.99771301019320E-03 0.24551520227704E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32584540288356E+02 0.12219202608133E+02 0.30648203915319E+03 0.35897152209421E+03
 0.30465290198370E+03 0.30754609212714E+03 0.30415000000005E+03 0.30415000000006E+03 0.30464662356325E+03
 0.30754714803713E+03 0.30415000000005E+03 0.30415000000006E+03 0.30465290198370E+03 0.30754609212714E+03
 0.30415000000005E+03 0.30415000000006E+03 0.30464662356325E+03 0.30754714803713E+03 0.30415000000005E+03
 0.30415000000006E+03 0.30702066663619E+03 0.30415000000026E+03 0.49254200975187E+02 0.45502030706691E+02
 0.51261632528207E+02 0.23710002852907E+03 0.18558208783822E+03 0.40328720750512E+02 0.48117920134299E+02
 0.40409193578621E+02 0.18873522861305E+03 0.39855223984453E+02 0.48157536943083E+02 0.39943667729002E+02
 0.18876700048938E+03 0.40328720750512E+02 0.48117920134299E+02 0.40409193578621E+02 0.18873522861305E+03
 0.39855223984453E+02 0.48157536943083E+02 0.39943667729002E+02 0.18876700048938E+03 0.11720774326301E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34763956662523E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14654302912560E+00 0.00000000000000E+00 0.00000000000000E+00 0.14654302912560E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15186980931331E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15186980931331E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572564585246E+00 0.15529632232291E+00 0.33031898272104E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    280.01155956
 0.10070404373934E+00 0.31056010005872E+03 0.41375502428130E+03 0.41029097556044E+03 0.40919082892186E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19253089637217E+00 0.00000000000000E+00 -.15610084979197E+02
 0.34469342212340E-02 0.57330316669490E+00 0.23209030072921E+04 0.87033862773454E+03 0.13954222590676E+02
 0.52328334715034E+01 0.32070904796278E+03 0.30415000000299E+03 0.31823494255506E+03 0.33112731588083E+03
 0.30415000000013E+03 0.30415000000019E+03 0.31634345086696E+03 0.33109249287137E+03 0.30415000000010E+03
 0.30415000000019E+03 0.31823494255506E+03 0.33112731588083E+03 0.30415000000013E+03 0.30415000000019E+03
 0.31634345086696E+03 0.33109249287137E+03 0.30415000000010E+03 0.30415000000019E+03 0.36003939271268E+03
 0.30676229282035E+03 0.12889934558105E+04 0.12505882881947E+04 0.47512706709779E+03 0.10082861668759E+04
 0.53078346444260E+03 0.74789919326538E+03 0.72550461750895E+03 0.71763308626887E+03 0.13312204553729E+04
 0.65806888614581E+03 0.72438209510112E+03 0.63687401851927E+03 0.13304153333740E+04 0.74789919326538E+03
 0.72550461750896E+03 0.71763308626887E+03 0.13312204553729E+04 0.65806888614581E+03 0.72438209510112E+03
 0.63687401851927E+03 0.13304153333740E+04 0.13276666324577E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44637147659221E+03 0.10919854755893E+01
 0.10919854755893E+01 0.70505553989038E+00 0.22849790827343E+00 0.30487626072162E+03 0.33028403680992E+03
 0.32641362101655E+03 0.32614363093804E+03 0.23000000000000E+00 0.00000000000000E+00 0.21479922757956E+00
 0.00000000000000E+00 -.90481273414904E+01 0.99724200036974E-03 0.26339594848378E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30372524885259E+02 0.11389696831972E+02 0.30676109164446E+03 0.36006484690007E+03
 0.30470189719703E+03 0.30767280440809E+03 0.30415000000005E+03 0.30415000000006E+03 0.30469515363492E+03
 0.30767381935072E+03 0.30415000000005E+03 0.30415000000006E+03 0.30470189719703E+03 0.30767280440809E+03
 0.30415000000005E+03 0.30415000000006E+03 0.30469515363492E+03 0.30767381935072E+03 0.30415000000005E+03
 0.30415000000006E+03 0.30713017669172E+03 0.30415000000038E+03 0.51265460178148E+02 0.47123273049589E+02
 0.54577270933397E+02 0.23913915398723E+03 0.18428899669916E+03 0.42802313888276E+02 0.51192700093983E+02
 0.43019851669720E+02 0.19039513223045E+03 0.42319301914192E+02 0.51224671330165E+02 0.42547405440274E+02
 0.19041958533583E+03 0.42802313888277E+02 0.51192700093983E+02 0.43019851669721E+02 0.19039513223045E+03
 0.42319301914192E+02 0.51224671330165E+02 0.42547405440274E+02 0.19041958533583E+03 0.11908038462606E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34760661657476E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14696691882842E+00 0.00000000000000E+00 0.00000000000000E+00 0.14696691882842E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15224047998631E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15224047998631E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572485520453E+00 0.15531183450696E+00 0.33028403680992E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    290.00341673
 0.10175022797349E+00 0.31078882883499E+03 0.41395091450456E+03 0.41045199259286E+03 0.40934245642216E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19133627205647E+00 0.00000000000000E+00 -.15644923717546E+02
 0.34114931174724E-02 0.59134095054906E+00 0.23450142575481E+04 0.87938034658054E+03 0.13528574323446E+02
 0.50732153712922E+01 0.32121648007858E+03 0.30415000000481E+03 0.31866031864405E+03 0.33183786101776E+03
 0.30415000000018E+03 0.30415000000029E+03 0.31673650928011E+03 0.33180348733929E+03 0.30415000000015E+03
 0.30415000000029E+03 0.31866031864405E+03 0.33183786101776E+03 0.30415000000018E+03 0.30415000000029E+03
 0.31673650928011E+03 0.33180348733929E+03 0.30415000000015E+03 0.30415000000029E+03 0.36108142442557E+03
 0.30705627905669E+03 0.12965924385424E+04 0.12568014442967E+04 0.47524359668742E+03 0.99821042236448E+03
 0.52059060769362E+03 0.75278070329614E+03 0.73045317998340E+03 0.72161187278096E+03 0.13313811272230E+04
 0.66315567741989E+03 0.72938089894615E+03 0.64125661236360E+03 0.13306215167157E+04 0.75278070329613E+03
 0.73045317998342E+03 0.72161187278096E+03 0.13313811272231E+04 0.66315567741989E+03 0.72938089894615E+03
 0.64125661236360E+03 0.13306215167157E+04 0.13267924756387E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44653002764728E+03 0.10919857043669E+01
 0.10919857043669E+01 0.73503111138252E+00 0.21373449899918E+00 0.30504379800054E+03 0.33022808670285E+03
 0.32663958581722E+03 0.32638848927294E+03 0.23000000000000E+00 0.00000000000000E+00 0.21395130169291E+00
 0.00000000000000E+00 -.90801557441470E+01 0.99669397625750E-03 0.28130581568645E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28438800600259E+02 0.10664550225097E+02 0.30705503734595E+03 0.36110605714590E+03
 0.30475233452598E+03 0.30779582689160E+03 0.30415000000005E+03 0.30415000000006E+03 0.30474519045080E+03
 0.30779679066534E+03 0.30415000000005E+03 0.30415000000006E+03 0.30475233452598E+03 0.30779582689160E+03
 0.30415000000005E+03 0.30415000000006E+03 0.30474519045080E+03 0.30779679066534E+03 0.30415000000005E+03
 0.30415000000006E+03 0.30723639792091E+03 0.30415000000058E+03 0.53040862482345E+02 0.48526269942804E+02
 0.57745842299535E+02 0.24086667039920E+03 0.18283209888816E+03 0.45225605994947E+02 0.54117161843940E+02
 0.45646133477612E+02 0.19179592271089E+03 0.44736134673705E+02 0.54141058203075E+02 0.45169617439002E+02
 0.19181270191658E+03 0.45225605994947E+02 0.54117161843940E+02 0.45646133477612E+02 0.19179592271089E+03
 0.44736134673705E+02 0.54141058203075E+02 0.45169617439002E+02 0.19181270191658E+03 0.12073921086696E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34755950598937E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14722518066044E+00 0.00000000000000E+00 0.00000000000000E+00 0.14722518066044E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15256367679739E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15256367679739E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572452743949E+00 0.15533775027531E+00 0.33022808670285E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    300.01552417
 0.10258998927214E+00 0.31101620646146E+03 0.41414477489831E+03 0.41061812198845E+03 0.40950138582752E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19017279355306E+00 0.00000000000000E+00 -.15668803905527E+02
 0.33835678418454E-02 0.60888753908498E+00 0.23643681385850E+04 0.88663805196937E+03 0.13138715257701E+02
 0.49270182216380E+01 0.32171209495948E+03 0.30415000000758E+03 0.31907659725873E+03 0.33252729487985E+03
 0.30415000000028E+03 0.30415000000045E+03 0.31712132364255E+03 0.33249336555311E+03 0.30415000000022E+03
 0.30415000000045E+03 0.31907659725873E+03 0.33252729487985E+03 0.30415000000028E+03 0.30415000000045E+03
 0.31712132364255E+03 0.33249336555311E+03 0.30415000000022E+03 0.30415000000045E+03 0.36207767002051E+03
 0.30736497957946E+03 0.13036663249398E+04 0.12625320341266E+04 0.47517170213227E+03 0.98851507096168E+03
 0.51096751031875E+03 0.75734266103368E+03 0.73495221286698E+03 0.72529301313660E+03 0.13312708028306E+04
 0.66792414253271E+03 0.73392711715737E+03 0.64534012284305E+03 0.13305537275463E+04 0.75734266103368E+03
 0.73495221286699E+03 0.72529301313660E+03 0.13312708028306E+04 0.66792414253272E+03 0.73392711715737E+03
 0.64534012284305E+03 0.13305537275463E+04 0.13257579337496E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44669427398192E+03 0.10919858611824E+01
 0.10919858611824E+01 0.76506743371366E+00 0.19948052120635E+00 0.30523693613640E+03 0.33015757128687E+03
 0.32684345042780E+03 0.32661137668155E+03 0.23000000000000E+00 0.00000000000000E+00 0.21309734959468E+00
 0.00000000000000E+00 -.90998001201047E+01 0.99606312669815E-03 0.29923672106362E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26734686744208E+02 0.10025507529078E+02 0.30736370225612E+03 0.36210142989449E+03
 0.30480437504098E+03 0.30791537715193E+03 0.30415000000004E+03 0.30415000000007E+03 0.30479686132424E+03
 0.30791627990526E+03 0.30415000000004E+03 0.30415000000007E+03 0.30480437504098E+03 0.30791537715193E+03
 0.30415000000004E+03 0.30415000000007E+03 0.30479686132424E+03 0.30791627990526E+03 0.30415000000004E+03
 0.30415000000007E+03 0.30733956077855E+03 0.30415000000088E+03 0.54585171331281E+02 0.49724035686264E+02
 0.60772302446494E+02 0.24234598306795E+03 0.18126981910922E+03 0.47601581394003E+02 0.56897623128857E+02
 0.48302779059480E+02 0.19298926611960E+03 0.47108774828642E+02 0.56913090606862E+02 0.47825290530125E+02
 0.19299808787726E+03 0.47601581394003E+02 0.56897623128857E+02 0.48302779059480E+02 0.19298926611960E+03
 0.47108774828643E+02 0.56913090606862E+02 0.47825290530125E+02 0.19299808787726E+03 0.12221445502339E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34750436274689E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14737093890451E+00 0.00000000000000E+00 0.00000000000000E+00 0.14737093890451E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15284366538877E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15284366538877E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572451766741E+00 0.15537088692790E+00 0.33015757128687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    310.00825079
 0.10324527772040E+00 0.31124082233058E+03 0.41434059867527E+03 0.41079241032806E+03 0.40967030068479E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18904357588540E+00 0.00000000000000E+00 -.15685902840100E+02
 0.33620924906183E-02 0.62589015078420E+00 0.23794705298333E+04 0.89230144868749E+03 0.12781795639341E+02
 0.47931733647529E+01 0.32219510867108E+03 0.30415000001171E+03 0.31948298725086E+03 0.33319525121817E+03
 0.30415000000043E+03 0.30415000000072E+03 0.31749717591973E+03 0.33316175853830E+03 0.30415000000034E+03
 0.30415000000071E+03 0.31948298725086E+03 0.33319525121816E+03 0.30415000000043E+03 0.30415000000072E+03
 0.31749717591973E+03 0.33316175853830E+03 0.30415000000034E+03 0.30415000000071E+03 0.36302984092740E+03
 0.30768681604469E+03 0.13102897518027E+04 0.12678527714964E+04 0.47497243514439E+03 0.97927868457553E+03
 0.50193138725542E+03 0.76162722277989E+03 0.73907922690011E+03 0.72871699262636E+03 0.13310032352207E+04
 0.67241509158449E+03 0.73809829043557E+03 0.64916322456923E+03 0.13303257759573E+04 0.76162722277989E+03
 0.73907922690011E+03 0.72871699262636E+03 0.13310032352207E+04 0.67241509158449E+03 0.73809829043558E+03
 0.64916322456923E+03 0.13303257759573E+04 0.13246627697772E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44686713776904E+03 0.10919859734671E+01
 0.10919859734671E+01 0.79504561355805E+00 0.18579363886778E+00 0.30545640173890E+03 0.33007823875010E+03
 0.32702851828749E+03 0.32681526830466E+03 0.23000000000000E+00 0.00000000000000E+00 0.21224167267891E+00
 0.00000000000000E+00 -.91111605680074E+01 0.99534735942110E-03 0.31710731780079E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25228052305704E+02 0.94605196146392E+01 0.30768550834720E+03 0.36305268906137E+03
 0.30485789115625E+03 0.30803123326125E+03 0.30415000000004E+03 0.30415000000009E+03 0.30485004120523E+03
 0.30803206594519E+03 0.30415000000004E+03 0.30415000000009E+03 0.30485789115625E+03 0.30803123326125E+03
 0.30415000000004E+03 0.30415000000009E+03 0.30485004120523E+03 0.30803206594519E+03 0.30415000000004E+03
 0.30415000000009E+03 0.30743949954633E+03 0.30415000000132E+03 0.55900179741591E+02 0.50727592052570E+02
 0.63651513438083E+02 0.24362703815771E+03 0.17965726715244E+03 0.49924575027977E+02 0.59531197886290E+02
 0.50996082901002E+02 0.19401606675324E+03 0.49431388170990E+02 0.59537997216640E+02 0.50520576539815E+02
 0.19401675528736E+03 0.49924575027977E+02 0.59531197886290E+02 0.50996082901002E+02 0.19401606675324E+03
 0.49431388170990E+02 0.59537997216640E+02 0.50520576539816E+02 0.19401675528736E+03 0.12352849533580E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34744642796930E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14744408483420E+00 0.00000000000000E+00 0.00000000000000E+00 0.14744408483420E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15308428526893E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15308428526893E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572471284615E+00 0.15540842020551E+00 0.33007823875010E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    320.00572838
 0.10374722459421E+00 0.31146285879431E+03 0.41454176903172E+03 0.41097705208128E+03 0.40985099214579E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18794439745912E+00 0.00000000000000E+00 -.15699200977569E+02
 0.33458259410177E-02 0.64240868305536E+00 0.23910389066942E+04 0.89663959001032E+03 0.12453131800696E+02
 0.46699244252610E+01 0.32266774742542E+03 0.30415000001779E+03 0.31988125298551E+03 0.33384546315222E+03
 0.30415000000066E+03 0.30415000000114E+03 0.31786570657448E+03 0.33381239972850E+03 0.30415000000052E+03
 0.30415000000113E+03 0.31988125298551E+03 0.33384546315222E+03 0.30415000000066E+03 0.30415000000114E+03
 0.31786570657448E+03 0.33381239972850E+03 0.30415000000052E+03 0.30415000000113E+03 0.36394516542808E+03
 0.30802217063102E+03 0.13165554664783E+04 0.12728470939664E+04 0.47468802688510E+03 0.97048942574534E+03
 0.49342795872582E+03 0.76569080090372E+03 0.74291670654001E+03 0.73193434242404E+03 0.13306552166924E+04
 0.67668489822461E+03 0.74197722041234E+03 0.65277702101465E+03 0.13300147661339E+04 0.76569080090372E+03
 0.74291670654002E+03 0.73193434242404E+03 0.13306552166924E+04 0.67668489822461E+03 0.74197722041234E+03
 0.65277702101465E+03 0.13300147661339E+04 0.13235653882150E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44705069345650E+03 0.10919860607928E+01
 0.10919860607928E+01 0.82503804633411E+00 0.17263916347963E+00 0.30570392803530E+03 0.32999440976550E+03
 0.32719875080122E+03 0.32700396222942E+03 0.23000000000000E+00 0.00000000000000E+00 0.21138297049562E+00
 0.00000000000000E+00 -.91172233650222E+01 0.10056637366117E-02 0.33495240860223E+00 0.79549452851442E+04
 0.29831044819291E+04 0.23883990067080E+02 0.89564962751549E+01 0.30802083796533E+03 0.36396707115442E+03
 0.30491311606018E+03 0.30814395935839E+03 0.30415000000004E+03 0.30415000000013E+03 0.30490496353593E+03
 0.30814471339534E+03 0.30415000000004E+03 0.30415000000013E+03 0.30491311606018E+03 0.30814395935839E+03
 0.30415000000004E+03 0.30415000000013E+03 0.30490496353593E+03 0.30814471339534E+03 0.30415000000004E+03
 0.30415000000013E+03 0.30753671811722E+03 0.30415000000199E+03 0.56998723576949E+02 0.51556073943921E+02
 0.66398860676572E+02 0.24476094792750E+03 0.17803009294754E+03 0.52204332786650E+02 0.62033772651255E+02
 0.53750524131901E+02 0.19491821968389E+03 0.51713526177214E+02 0.62031721269575E+02 0.53279772841351E+02
 0.19491065172410E+03 0.52204332786650E+02 0.62033772651255E+02 0.53750524131901E+02 0.19491821968389E+03
 0.51713526177214E+02 0.62031721269575E+02 0.53279772841351E+02 0.19491065172410E+03 0.12471003133842E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34738955981403E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14747304767597E+00 0.00000000000000E+00 0.00000000000000E+00 0.14747304767597E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15329108546883E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15329108546883E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572503217234E+00 0.15544823017150E+00 0.32999440976550E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    330.00833839
 0.10412444028652E+00 0.31168209186874E+03 0.41474989878657E+03 0.41117260621761E+03 0.41004367336031E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18687393894320E+00 0.00000000000000E+00 -.15710644617274E+02
 0.33337047006411E-02 0.65846082455182E+00 0.23997326453244E+04 0.89989974199667E+03 0.12149545882924E+02
 0.45560797060963E+01 0.32313091717563E+03 0.30415000002656E+03 0.32027207267478E+03 0.33447973356955E+03
 0.30415000000103E+03 0.30415000000178E+03 0.31822754900621E+03 0.33444709145594E+03 0.30415000000079E+03
 0.30415000000177E+03 0.32027207267478E+03 0.33447973356955E+03 0.30415000000103E+03 0.30415000000178E+03
 0.31822754900621E+03 0.33444709145594E+03 0.30415000000079E+03 0.30415000000177E+03 0.36482772746188E+03
 0.30837068988061E+03 0.13225190255755E+04 0.12775656113252E+04 0.47434716293631E+03 0.96214009581252E+03
 0.48542119706153E+03 0.76956747124935E+03 0.74651778773974E+03 0.73497646483753E+03 0.13302769457057E+04
 0.68076715862471E+03 0.74561723184880E+03 0.65621247897645E+03 0.13296710888843E+04 0.76956747124935E+03
 0.74651778773974E+03 0.73497646483753E+03 0.13302769457057E+04 0.68076715862471E+03 0.74561723184880E+03
 0.65621247897645E+03 0.13296710888843E+04 0.13225020119096E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44724540447338E+03 0.10919861359405E+01
 0.10919861359405E+01 0.85504587635850E+00 0.16001566585670E+00 0.30598063135853E+03 0.32990970634497E+03
 0.32735702176678E+03 0.32718016183200E+03 0.23000000000000E+00 0.00000000000000E+00 0.21052182442577E+00
 0.00000000000000E+00 -.91200175170619E+01 0.10849995805477E-02 0.35276641624798E+00 0.73732747398499E+04
 0.27649780274437E+04 0.22677895716627E+02 0.85042108937352E+01 0.30836933789215E+03 0.36484867021520E+03
 0.30497018809647E+03 0.30825384076541E+03 0.30415000000004E+03 0.30415000000019E+03 0.30496176754884E+03
 0.30825450823473E+03 0.30415000000004E+03 0.30415000000019E+03 0.30497018809647E+03 0.30825384076541E+03
 0.30415000000004E+03 0.30415000000019E+03 0.30496176754884E+03 0.30825450823473E+03 0.30415000000004E+03
 0.30415000000019E+03 0.30763147405807E+03 0.30415000000297E+03 0.57889771065068E+02 0.52224916075434E+02
 0.69022962352549E+02 0.24578749320102E+03 0.17641941603670E+03 0.54444383443688E+02 0.64414939386034E+02
 0.56584710754512E+02 0.19572853977124E+03 0.53958542909812E+02 0.64403928217167E+02 0.56121309312564E+02
 0.19571266010344E+03 0.54444383443688E+02 0.64414939386034E+02 0.56584710754512E+02 0.19572853977124E+03
 0.53958542909812E+02 0.64403928217167E+02 0.56121309312564E+02 0.19571266010344E+03 0.12578086726802E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34733688596100E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14747691121116E+00 0.00000000000000E+00 0.00000000000000E+00 0.14747691121116E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15346880549184E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15346880549184E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572541888070E+00 0.15548854940911E+00 0.32990970634497E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    340.00670372
 0.10440286977102E+00 0.31189819490814E+03 0.41496548015127E+03 0.41137864003164E+03 0.41024761823323E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18583200087316E+00 0.00000000000000E+00 -.15721475956195E+02
 0.33248139243117E-02 0.67404872154068E+00 0.24061496920181E+04 0.90230613450679E+03 0.11868578256056E+02
 0.44507168460210E+01 0.32358496905903E+03 0.30415000003902E+03 0.32065567354057E+03 0.33509902295948E+03
 0.30415000000157E+03 0.30415000000276E+03 0.31858291616449E+03 0.33506679341983E+03 0.30415000000121E+03
 0.30415000000274E+03 0.32065567354057E+03 0.33509902295948E+03 0.30415000000157E+03 0.30415000000276E+03
 0.31858291616449E+03 0.33506679341983E+03 0.30415000000121E+03 0.30415000000274E+03 0.36568016575950E+03
 0.30873166444263E+03 0.13282160590442E+04 0.12820411597005E+04 0.47396911192786E+03 0.95421926734587E+03
 0.47788030985837E+03 0.77327940943526E+03 0.74991860396958E+03 0.73786446236371E+03 0.13298998932257E+04
 0.68468344221116E+03 0.74905459970472E+03 0.65948990863146E+03 0.13293263624550E+04 0.77327940943526E+03
 0.74991860396959E+03 0.73786446236371E+03 0.13298998932257E+04 0.68468344221116E+03 0.74905459970472E+03
 0.65948990863146E+03 0.13293263624550E+04 0.13214929394385E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44745076188587E+03 0.10919862070674E+01
 0.10919862070674E+01 0.88504097237536E+00 0.14793222530406E+00 0.30628697792408E+03 0.32982709691763E+03
 0.32750553545987E+03 0.32734591516724E+03 0.23000000000000E+00 0.00000000000000E+00 0.20965953857320E+00
 0.00000000000000E+00 -.91209143538676E+01 0.11736247107719E-02 0.37052777558210E+00 0.68164890587026E+04
 0.25561833970135E+04 0.21590824027786E+02 0.80965590104197E+01 0.30873029889725E+03 0.36570013551163E+03
 0.30502921975249E+03 0.30836105909549E+03 0.30415000000003E+03 0.30415000000029E+03 0.30502056647206E+03
 0.30836163282500E+03 0.30415000000003E+03 0.30415000000029E+03 0.30502921975249E+03 0.30836105909549E+03
 0.30415000000003E+03 0.30415000000029E+03 0.30502056647206E+03 0.30836163282500E+03 0.30415000000003E+03
 0.30415000000029E+03 0.30772393178630E+03 0.30415000000437E+03 0.58584427055980E+02 0.52750495115785E+02
 0.71530639005092E+02 0.24673922905226E+03 0.17485093685214E+03 0.56647728678675E+02 0.66682432768870E+02
 0.59516386283475E+02 0.19647396543684E+03 0.56169245901323E+02 0.66662420630461E+02 0.59062724644867E+02
 0.19644978080547E+03 0.56647728678675E+02 0.66682432768870E+02 0.59516386283475E+02 0.19647396543684E+03
 0.56169245901329E+02 0.66662420630461E+02 0.59062724644876E+02 0.19644978080547E+03 0.12675965070518E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34729085061447E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14746834926014E+00 0.00000000000000E+00 0.00000000000000E+00 0.14746834926014E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15362168269864E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15362168269864E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572583333858E+00 0.15552793446966E+00 0.32982709691763E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    350.00691667
 0.10460545806938E+00 0.31211126816705E+03 0.41518872479133E+03 0.41159456993573E+03 0.41046200024883E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18481684680600E+00 0.00000000000000E+00 -.15732452476622E+02
 0.33183745792011E-02 0.68919844251684E+00 0.24108188539480E+04 0.90405707023052E+03 0.11607687287837E+02
 0.43528827329390E+01 0.32403086760590E+03 0.30415000005647E+03 0.32103281457407E+03 0.33570505264589E+03
 0.30415000000238E+03 0.30415000000420E+03 0.31893251123012E+03 0.33567322694137E+03 0.30415000000183E+03
 0.30415000000418E+03 0.32103281457407E+03 0.33570505264589E+03 0.30415000000238E+03 0.30415000000420E+03
 0.31893251123012E+03 0.33567322694137E+03 0.30415000000183E+03 0.30415000000418E+03 0.36650594742332E+03
 0.30910490003997E+03 0.13336809893361E+04 0.12863042973703E+04 0.47356567988835E+03 0.94669602100085E+03
 0.47076251271306E+03 0.77684860561710E+03 0.75315057252349E+03 0.74061856853868E+03 0.13295416230026E+04
 0.68845553271088E+03 0.75232093012122E+03 0.66262941986441E+03 0.13289983371376E+04 0.77684860561710E+03
 0.75315057252349E+03 0.74061856853868E+03 0.13295416230026E+04 0.68845553271088E+03 0.75232093012122E+03
 0.66262941986441E+03 0.13289983371376E+04 0.13205456476307E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44766612169103E+03 0.10919862791477E+01
 0.10919862791477E+01 0.91504161121782E+00 0.13637755714641E+00 0.30662348068577E+03 0.32974878403489E+03
 0.32764626911554E+03 0.32750309773720E+03 0.23000000000000E+00 0.00000000000000E+00 0.20879598567491E+00
 0.00000000000000E+00 -.91207986382074E+01 0.12730606429756E-02 0.38824350929520E+00 0.62840682760416E+04
 0.23565256035156E+04 0.20605624584743E+02 0.77271092192785E+01 0.30910352668499E+03 0.36652494238329E+03
 0.30509043406514E+03 0.30846595654784E+03 0.30415000000003E+03 0.30415000000042E+03 0.30508158340964E+03
 0.30846642998905E+03 0.30415000000003E+03 0.30415000000042E+03 0.30509043406514E+03 0.30846595654784E+03
 0.30415000000003E+03 0.30415000000042E+03 0.30508158340964E+03 0.30846642998905E+03 0.30415000000003E+03
 0.30415000000042E+03 0.30781439142288E+03 0.30415000000636E+03 0.59093878467283E+02 0.53147429561230E+02
 0.73932363086608E+02 0.24764398435913E+03 0.17334195945708E+03 0.58819497034921E+02 0.68847220477331E+02
 0.62566516111546E+02 0.19717761005488E+03 0.58350595669916E+02 0.68818218470660E+02 0.62124807053943E+02
 0.19714517465279E+03 0.58819497034921E+02 0.68847220477331E+02 0.62566516111546E+02 0.19717761005488E+03
 0.58350595669919E+02 0.68818218470659E+02 0.62124807053947E+02 0.19714517465279E+03 0.12766327841843E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34725319853653E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14745532277776E+00 0.00000000000000E+00 0.00000000000000E+00 0.14745532277776E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15375360371171E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15375360371171E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572624978046E+00 0.15556531593362E+00 0.32974878403489E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    360.00205597
 0.10475097299111E+00 0.31232119727665E+03 0.41541917856164E+03 0.41181930727730E+03 0.41068554183813E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18382818409122E+00 0.00000000000000E+00 -.15743986210632E+02
 0.33137646323894E-02 0.70391509626252E+00 0.24141726668836E+04 0.90531475008133E+03 0.11365007005073E+02
 0.42618776269023E+01 0.32446890413685E+03 0.30415000008057E+03 0.32140369683314E+03 0.33629855711221E+03
 0.30415000000354E+03 0.30415000000631E+03 0.31927651871603E+03 0.33626712608252E+03 0.30415000000273E+03
 0.30415000000627E+03 0.32140369683314E+03 0.33629855711221E+03 0.30415000000354E+03 0.30415000000631E+03
 0.31927651871603E+03 0.33626712608252E+03 0.30415000000273E+03 0.30415000000627E+03 0.36730700402348E+03
 0.30948969551437E+03 0.13389336516102E+04 0.12903732652321E+04 0.47314443828179E+03 0.93954641120386E+03
 0.46403625073065E+03 0.78028784628867E+03 0.75623332151340E+03 0.74325130468285E+03 0.13292112280894E+04
 0.69209582453110E+03 0.75543598188616E+03 0.66564298366037E+03 0.13286962387220E+04 0.78028784628867E+03
 0.75623332151340E+03 0.74325130468285E+03 0.13292112280894E+04 0.69209582453110E+03 0.75543598188616E+03
 0.66564298366037E+03 0.13286962387220E+04 0.13196618392078E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44789035024733E+03 0.10919863548871E+01
 0.10919863548871E+01 0.94502702911259E+00 0.12535496490730E+00 0.30698986515691E+03 0.32967653342263E+03
 0.32778060908642E+03 0.32765299662018E+03 0.23000000000000E+00 0.00000000000000E+00 0.20793208034755E+00
 0.00000000000000E+00 -.91202283017099E+01 0.13850020763401E-02 0.40589865145553E+00 0.57761646257889E+04
 0.21660617346708E+04 0.19709353483468E+02 0.73910075563004E+01 0.30948832010269E+03 0.36732503090538E+03
 0.30515399414199E+03 0.30856872326614E+03 0.30415000000003E+03 0.30415000000063E+03 0.30514498158882E+03
 0.30856909062074E+03 0.30415000000003E+03 0.30415000000063E+03 0.30515399414199E+03 0.30856872326614E+03
 0.30415000000003E+03 0.30415000000063E+03 0.30514498158882E+03 0.30856909062074E+03 0.30415000000003E+03
 0.30415000000063E+03 0.30790301989400E+03 0.30415000000914E+03 0.59428371932665E+02 0.53428070844148E+02
 0.76235023297789E+02 0.24852305393785E+03 0.17190685552358E+03 0.60961475578441E+02 0.70916890067179E+02
 0.65751769339570E+02 0.19785731979266E+03 0.60504208387519E+02 0.70878967252452E+02 0.65324042051614E+02
 0.19781674014845E+03 0.60961475578441E+02 0.70916890067179E+02 0.65751769339569E+02 0.19785731979266E+03
 0.60504208387519E+02 0.70878967252452E+02 0.65324042051614E+02 0.19781674014845E+03 0.12850469652961E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34722523030109E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14744264754345E+00 0.00000000000000E+00 0.00000000000000E+00 0.14744264754345E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15386795259569E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15386795259569E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572665097201E+00 0.15559983826539E+00 0.32967653342263E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    370.00961968
 0.10485483050749E+00 0.31252842208271E+03 0.41565673586989E+03 0.41205223524900E+03 0.41091747951624E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18286342369575E+00 0.00000000000000E+00 -.15756296702646E+02
 0.33104821535720E-02 0.71823833880579E+00 0.24165664180875E+04 0.90621240678280E+03 0.11138363921510E+02
 0.41768864705664E+01 0.32490035864336E+03 0.30415000011347E+03 0.32176936750211E+03 0.33688154834758E+03
 0.30415000000524E+03 0.30415000000935E+03 0.31961590698811E+03 0.33685050345517E+03 0.30415000000405E+03
 0.30415000000930E+03 0.32176936750211E+03 0.33688154834758E+03 0.30415000000524E+03 0.30415000000935E+03
 0.31961590698811E+03 0.33685050345517E+03 0.30415000000405E+03 0.30415000000930E+03 0.36808683868996E+03
 0.30988627078722E+03 0.13440014536666E+04 0.12942715457184E+04 0.47270879146780E+03 0.93272864567056E+03
 0.45765631024542E+03 0.78361501821613E+03 0.75918855572230E+03 0.74577861622492E+03 0.13289112912990E+04
 0.69562237162161E+03 0.75842165614939E+03 0.66854699943895E+03 0.13284228380005E+04 0.78361501821613E+03
 0.75918855572230E+03 0.74577861622492E+03 0.13289112912990E+04 0.69562237162161E+03 0.75842165614939E+03
 0.66854699943895E+03 0.13284228380005E+04 0.13188369850474E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44812279648485E+03 0.10919864357272E+01
 0.10919864357272E+01 0.97504972025027E+00 0.11484070774397E+00 0.30738640147190E+03 0.32961149170693E+03
 0.32790992831209E+03 0.32779694402938E+03 0.23000000000000E+00 0.00000000000000E+00 0.20706656184017E+00
 0.00000000000000E+00 -.91195280104895E+01 0.15118060204425E-02 0.42352218862912E+00 0.52916841789387E+04
 0.19843815671020E+04 0.18889211037313E+02 0.70834541389923E+01 0.30988489901964E+03 0.36810390927390E+03
 0.30522023711699E+03 0.30866978169506E+03 0.30415000000004E+03 0.30415000000095E+03 0.30521109771036E+03
 0.30867003764239E+03 0.30415000000004E+03 0.30415000000095E+03 0.30522023711699E+03 0.30866978169506E+03
 0.30415000000004E+03 0.30415000000095E+03 0.30521109771036E+03 0.30867003764239E+03 0.30415000000004E+03
 0.30415000000095E+03 0.30799018296921E+03 0.30415000001300E+03 0.59598486652964E+02 0.53603457357633E+02
 0.78450503860032E+02 0.24939527266735E+03 0.17055251628802E+03 0.63080084752297E+02 0.72903367836994E+02
 0.69096254080190E+02 0.19852883650343E+03 0.62636371196757E+02 0.72856623866987E+02 0.68684397830075E+02
 0.19848024623309E+03 0.63080084752297E+02 0.72903367836994E+02 0.69096254080190E+02 0.19852883650343E+03
 0.62636371196757E+02 0.72856623866987E+02 0.68684397830075E+02 0.19848024623309E+03 0.12929635203268E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34720776767354E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14743295219021E+00 0.00000000000000E+00 0.00000000000000E+00 0.14743295219021E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15396769261808E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15396769261808E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572702721392E+00 0.15563094542420E+00 0.32961149170693E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    380.01579018
 0.10492873335411E+00 0.31273275779131E+03 0.41590045119139E+03 0.41229203272754E+03 0.41115638693467E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18192313604799E+00 0.00000000000000E+00 -.15769412350152E+02
 0.33081502923285E-02 0.73216107463427E+00 0.24182698163840E+04 0.90685118114400E+03 0.10926557389023E+02
 0.40974590208835E+01 0.32532508127854E+03 0.30415000015779E+03 0.32212966950165E+03 0.33745405126093E+03
 0.30415000000762E+03 0.30415000001364E+03 0.31995052641573E+03 0.33742338343102E+03 0.30415000000590E+03
 0.30415000001357E+03 0.32212966950165E+03 0.33745405126093E+03 0.30415000000762E+03 0.30415000001364E+03
 0.31995052641573E+03 0.33742338343102E+03 0.30415000000590E+03 0.30415000001357E+03 0.36884616491360E+03
 0.31029359987755E+03 0.13488920217679E+04 0.12980069707673E+04 0.47226149441760E+03 0.92622383484069E+03
 0.45160103295100E+03 0.78683506407559E+03 0.76202467561651E+03 0.74820622186448E+03 0.13286419294461E+04
 0.69903971485991E+03 0.76128643943547E+03 0.67134652287043E+03 0.13281783413847E+04 0.78683506407559E+03
 0.76202467561651E+03 0.74820622186448E+03 0.13286419294461E+04 0.69903971485991E+03 0.76128643943547E+03
 0.67134652287043E+03 0.13281783413847E+04 0.13180671213886E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44836211620724E+03 0.10919865218547E+01
 0.10919865218547E+01 0.10050682317376E+01 0.10484466509150E+00 0.30781180741819E+03 0.32955465404710E+03
 0.32803490639847E+03 0.32793555020082E+03 0.23000000000000E+00 0.00000000000000E+00 0.20620082720041E+00
 0.00000000000000E+00 -.91188596993902E+01 0.16559436810066E-02 0.44108849927576E+00 0.48310821749306E+04
 0.18116558155990E+04 0.18136949870911E+02 0.68013562015918E+01 0.31029223734708E+03 0.36886229796177E+03
 0.30528931326065E+03 0.30876921497736E+03 0.30415000000006E+03 0.30415000000139E+03 0.30528008192713E+03
 0.30876935499720E+03 0.30415000000006E+03 0.30415000000139E+03 0.30528931326065E+03 0.30876921497736E+03
 0.30415000000006E+03 0.30415000000139E+03 0.30528008192713E+03 0.30876935499720E+03 0.30415000000006E+03
 0.30415000000139E+03 0.30807595272172E+03 0.30415000001826E+03 0.59613458597488E+02 0.53681832072156E+02
 0.80582910717923E+02 0.25027280629583E+03 0.16928698102432E+03 0.65174432030203E+02 0.74811449612395E+02
 0.72613509459785E+02 0.19920256821772E+03 0.64746026177561E+02 0.74756037283069E+02 0.72219235131919E+02
 0.19914614832542E+03 0.65174432030203E+02 0.74811449612395E+02 0.72613509459785E+02 0.19920256821772E+03
 0.64746026177561E+02 0.74756037283069E+02 0.72219235131919E+02 0.19914614832542E+03 0.13004584201380E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34720137875413E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14742735382975E+00 0.00000000000000E+00 0.00000000000000E+00 0.14742735382975E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15405499672645E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15405499672645E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572737236060E+00 0.15565815648948E+00 0.32955465404710E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    390.29896503
 0.10498403333573E+00 0.31293953455559E+03 0.41615652664079E+03 0.41254448126150E+03 0.41140792845207E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18098140125913E+00 0.00000000000000E+00 -.15784600191381E+02
 0.33064074826291E-02 0.74606742301590E+00 0.24195444880977E+04 0.90732918303662E+03 0.10722891461553E+02
 0.40210842980823E+01 0.32575487296291E+03 0.30415000021885E+03 0.32249458747571E+03 0.33803234479658E+03
 0.30415000001102E+03 0.30415000001982E+03 0.32028962332018E+03 0.33800205390914E+03 0.30415000000854E+03
 0.30415000001971E+03 0.32249458747571E+03 0.33803234479658E+03 0.30415000001102E+03 0.30415000001982E+03
 0.32028962332018E+03 0.33800205390914E+03 0.30415000000854E+03 0.30415000001971E+03 0.36960778807420E+03
 0.31072297907204E+03 0.13537468032647E+04 0.13016871408911E+04 0.47178003541100E+03 0.91981331207656E+03
 0.44567437648850E+03 0.79004137534401E+03 0.76482476396334E+03 0.75060447788483E+03 0.13283918493696E+04
 0.70244666633768E+03 0.76411426676684E+03 0.67412223777932E+03 0.13279522150569E+04 0.79004137534401E+03
 0.76482476396334E+03 0.75060447788483E+03 0.13283918493696E+04 0.70244666633768E+03 0.76411426676684E+03
 0.67412223777932E+03 0.13279522150569E+04 0.13173193556082E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44861405665123E+03 0.10919866215898E+01
 0.10919866215898E+01 0.10359177562863E+01 0.95103971507806E-01 0.30827802450637E+03 0.32950536211812E+03
 0.32815949271051E+03 0.32807307218431E+03 0.23000000000000E+00 0.00000000000000E+00 0.20531094961991E+00
 0.00000000000000E+00 -.91141373135524E+01 0.18255478235690E-02 0.45908336554895E+00 0.43822461930138E+04
 0.16433423223802E+04 0.17426028909660E+02 0.65347608411224E+01 0.31072163174177E+03 0.36962298338427E+03
 0.30536328088539E+03 0.30886991810416E+03 0.30415000000008E+03 0.30415000000202E+03 0.30535398989792E+03
 0.30886993532975E+03 0.30415000000008E+03 0.30415000000202E+03 0.30536328088539E+03 0.30886991810416E+03
 0.30415000000008E+03 0.30415000000202E+03 0.30535398989792E+03 0.30886993532975E+03 0.30415000000008E+03
 0.30415000000202E+03 0.30816280940894E+03 0.30415000002561E+03 0.59476208181582E+02 0.53667716315066E+02
 0.82694700186511E+02 0.25119054337628E+03 0.16808236968883E+03 0.67303783776467E+02 0.76697909463749E+02
 0.76429005214310E+02 0.19990604652346E+03 0.66892772906640E+02 0.76633790697289E+02 0.76054414930027E+02
 0.19984180409981E+03 0.67303783776467E+02 0.76697909463749E+02 0.76429005214310E+02 0.19990604652346E+03
 0.66892772906640E+02 0.76633790697289E+02 0.76054414930027E+02 0.19984180409981E+03 0.13077909456329E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34720663266722E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14743514334528E+00 0.00000000000000E+00 0.00000000000000E+00 0.14743514334528E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15413510509162E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15413510509162E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572779295854E+00 0.15568189855653E+00 0.32950536211812E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    400.03817591
 0.10502342116641E+00 0.31313268694367E+03 0.41640364308890E+03 0.41278835338173E+03 0.41165090616283E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18011180761141E+00 0.00000000000000E+00 -.15797859985343E+02
 0.33051672083632E-02 0.75887390607798E+00 0.24204524296856E+04 0.90766966113212E+03 0.10541935802412E+02
 0.39532259259047E+01 0.32615615354737E+03 0.30415000029537E+03 0.32283559440101E+03 0.33857110212827E+03
 0.30415000001545E+03 0.30415000002791E+03 0.32060672032136E+03 0.33854116018353E+03 0.30415000001199E+03
 0.30415000002776E+03 0.32283559440101E+03 0.33857110212827E+03 0.30415000001545E+03 0.30415000002791E+03
 0.32060672032136E+03 0.33854116018353E+03 0.30415000001199E+03 0.30415000002776E+03 0.37031132885649E+03
 0.31113922724376E+03 0.13581934389677E+04 0.13050325074871E+04 0.47132043575298E+03 0.91400911377919E+03
 0.44033207584745E+03 0.79298763726658E+03 0.76737801578116E+03 0.75279150663674E+03 0.13281785398885E+04
 0.70558073213205E+03 0.76669229504157E+03 0.67666190015675E+03 0.13277601896938E+04 0.79298763726658E+03
 0.76737801578116E+03 0.75279150663674E+03 0.13281785398885E+04 0.70558073213205E+03 0.76669229504157E+03
 0.67666190015675E+03 0.13277601896938E+04 0.13166547696324E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44885742364893E+03 0.10919867086639E+01
 0.10919867086639E+01 0.10651353889293E+01 0.86369669964657E-01 0.30874536602653E+03 0.32946751318372E+03
 0.32827433650977E+03 0.32819915543727E+03 0.23000000000000E+00 0.00000000000000E+00 0.20446811498668E+00
 0.00000000000000E+00 -.91038515346312E+01 0.20101597824172E-02 0.47607205783109E+00 0.39797831346422E+04
 0.14924186754908E+04 0.16804178838907E+02 0.63015670645900E+01 0.31113789944292E+03 0.37032566848807E+03
 0.30543673061950E+03 0.30896409676500E+03 0.30415000000009E+03 0.30415000000285E+03 0.30542741672323E+03
 0.30896399423789E+03 0.30415000000009E+03 0.30415000000285E+03 0.30543673061950E+03 0.30896409676500E+03
 0.30415000000009E+03 0.30415000000285E+03 0.30542741672323E+03 0.30896399423789E+03 0.30415000000009E+03
 0.30415000000285E+03 0.30824405250785E+03 0.30415000003493E+03 0.59212015707956E+02 0.53570630525845E+02
 0.84627284611754E+02 0.25208218935537E+03 0.16703176832056E+03 0.69300347641987E+02 0.78421813584936E+02
 0.80247345427336E+02 0.20058921833617E+03 0.68907254008968E+02 0.78349641520806E+02 0.79892782054565E+02
 0.20051778071635E+03 0.69300347641987E+02 0.78421813584936E+02 0.80247345427336E+02 0.20058921833617E+03
 0.68907254008968E+02 0.78349641520806E+02 0.79892782054565E+02 0.20051778071635E+03 0.13144413591052E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34722254145968E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14742896840642E+00 0.00000000000000E+00 0.00000000000000E+00 0.14742896840642E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15420030781535E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15420030781535E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572830226378E+00 0.15570034312311E+00 0.32946751318372E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    410.00991194
 0.10505179449819E+00 0.31332904376014E+03 0.41666118103644E+03 0.41304277221970E+03 0.41190439728949E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17924335717389E+00 0.00000000000000E+00 -.15812831787607E+02
 0.33042742646844E-02 0.77162850867349E+00 0.24211065302608E+04 0.90791494884780E+03 0.10367683295881E+02
 0.38878812359555E+01 0.32656179778457E+03 0.30415000039672E+03 0.32318060564617E+03 0.33911447779214E+03
 0.30415000002154E+03 0.30415000003907E+03 0.32092777988762E+03 0.33908488608354E+03 0.30415000001673E+03
 0.30415000003886E+03 0.32318060564617E+03 0.33911447779214E+03 0.30415000002154E+03 0.30415000003907E+03
 0.32092777988762E+03 0.33908488608354E+03 0.30415000001673E+03 0.30415000003886E+03 0.37101502239475E+03
 0.31157418317690E+03 0.13626088177959E+04 0.13083358627619E+04 0.47085170731508E+03 0.90832344876858E+03
 0.43511748291693E+03 0.79592216295718E+03 0.76990418299277E+03 0.75495921796697E+03 0.13279872267156E+04
 0.70870536986353E+03 0.76924241579442E+03 0.67918558731046E+03 0.13275893366089E+04 0.79592216295718E+03
 0.76990418299277E+03 0.75495921796695E+03 0.13279872267156E+04 0.70870536986353E+03 0.76924241579442E+03
 0.67918558731046E+03 0.13275893366089E+04 0.13160220845032E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44911130899653E+03 0.10919868069803E+01
 0.10919868069803E+01 0.10950505970170E+01 0.77916020912122E-01 0.30924746448868E+03 0.32943795935770E+03
 0.32838918401074E+03 0.32832448659918E+03 0.23000000000000E+00 0.00000000000000E+00 0.20360524099732E+00
 0.00000000000000E+00 -.90949577292374E+01 0.22282558071933E-02 0.49341125099305E+00 0.35902520591102E+04
 0.13463445221663E+04 0.16213655411989E+02 0.60801207794961E+01 0.31157288017574E+03 0.37102849470012E+03
 0.30551580834103E+03 0.30905953096766E+03 0.30415000000012E+03 0.30415000000399E+03 0.30550650454863E+03
 0.30905930274087E+03 0.30415000000012E+03 0.30415000000399E+03 0.30551580834103E+03 0.30905953096766E+03
 0.30415000000012E+03 0.30415000000399E+03 0.30550650454863E+03 0.30905930274087E+03 0.30415000000012E+03
 0.30415000000399E+03 0.30832640583658E+03 0.30415000004746E+03 0.58816746935856E+02 0.53391326188529E+02
 0.86541948513751E+02 0.25302246614404E+03 0.16604780788772E+03 0.71321025052122E+02 0.80128385384046E+02
 0.84374214716909E+02 0.20131037651371E+03 0.70947578786645E+02 0.80048222116188E+02 0.84041391521996E+02
 0.20123184478693E+03 0.71321025052122E+02 0.80128385384046E+02 0.84374214716909E+02 0.20131037651371E+03
 0.70947578786645E+02 0.80048222116188E+02 0.84041391521995E+02 0.20123184478693E+03 0.13209917415078E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34725002348240E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14743386333427E+00 0.00000000000000E+00 0.00000000000000E+00 0.14743386333427E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15425967949974E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15425967949974E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572875240372E+00 0.15571480563514E+00 0.32943795935770E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    420.00272954
 0.10507490112129E+00 0.31352316075126E+03 0.41692287674121E+03 0.41330130509667E+03 0.41216190828066E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17839470478330E+00 0.00000000000000E+00 -.15828472577692E+02
 0.33035473762323E-02 0.78405828598769E+00 0.24216392528700E+04 0.90811471982626E+03 0.10203323073006E+02
 0.38262461523774E+01 0.32696295611579E+03 0.30415000052790E+03 0.32352206469184E+03 0.33965107837191E+03
 0.30415000002973E+03 0.30415000005411E+03 0.32124572087887E+03 0.33962182848267E+03 0.30415000002312E+03
 0.30415000005383E+03 0.32352206469185E+03 0.33965107837191E+03 0.30415000002973E+03 0.30415000005411E+03
 0.32124572087887E+03 0.33962182848267E+03 0.30415000002312E+03 0.30415000005383E+03 0.37170567276755E+03
 0.31201856021403E+03 0.13668995441873E+04 0.13115217530363E+04 0.47036520026428E+03 0.90282844662544E+03
 0.43011142035984E+03 0.79878322787156E+03 0.77234806579508E+03 0.75705716099660E+03 0.13278125071669E+04
 0.71175487060378E+03 0.77170896622426E+03 0.68163575253187E+03 0.13274338837442E+04 0.79878322787155E+03
 0.77234806579508E+03 0.75705716099657E+03 0.13278125071669E+04 0.71175487060378E+03 0.77170896622426E+03
 0.68163575253187E+03 0.13274338837442E+04 0.13154154612274E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44936927239887E+03 0.10919869096899E+01
 0.10919869096899E+01 0.11250290498243E+01 0.69934113824929E-01 0.30977293799567E+03 0.32941754649329E+03
 0.32850166096881E+03 0.32844645020819E+03 0.23000000000000E+00 0.00000000000000E+00 0.20274073897833E+00
 0.00000000000000E+00 -.90870482730845E+01 0.24825769029303E-02 0.51073121791684E+00 0.32224580799722E+04
 0.12084217799896E+04 0.15663816346747E+02 0.58739311300303E+01 0.31201728687363E+03 0.37171831900675E+03
 0.30559881336487E+03 0.30915425632084E+03 0.30415000000016E+03 0.30415000000553E+03 0.30558955110150E+03
 0.30915390022863E+03 0.30415000000016E+03 0.30415000000554E+03 0.30559881336487E+03 0.30915425632084E+03
 0.30415000000016E+03 0.30415000000553E+03 0.30558955110150E+03 0.30915390022863E+03 0.30415000000016E+03
 0.30415000000554E+03 0.30840814299954E+03 0.30415000006387E+03 0.58299733309004E+02 0.53130905163367E+02
 0.88400404957914E+02 0.25399524915494E+03 0.16515284217223E+03 0.73324384819179E+02 0.81783801847946E+02
 0.88756152814027E+02 0.20205720556231E+03 0.72971798825544E+02 0.81695899746071E+02 0.88446228140750E+02
 0.20197184420809E+03 0.73324384819177E+02 0.81783801847946E+02 0.88756152814023E+02 0.20205720556231E+03
 0.72971798825544E+02 0.81695899746071E+02 0.88446228140749E+02 0.20197184420809E+03 0.13273121722777E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34728852394714E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14744325459005E+00 0.00000000000000E+00 0.00000000000000E+00 0.14744325459005E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15431224234812E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15431224234812E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572914777284E+00 0.15572488939139E+00 0.32941754649329E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    430.02191831
 0.10509193091132E+00 0.31371570618150E+03 0.41718839525986E+03 0.41356368036258E+03 0.41242320024900E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17756500080873E+00 0.00000000000000E+00 -.15837443024462E+02
 0.33030117861600E-02 0.79617709457554E+00 0.24220319265953E+04 0.90826197247323E+03 0.10048015767478E+02
 0.37680059128043E+01 0.32736010529703E+03 0.30415000069639E+03 0.32386036933614E+03 0.34018151873746E+03
 0.30415000004062E+03 0.30415000007420E+03 0.32156092253715E+03 0.34015260304476E+03 0.30415000003163E+03
 0.30415000007383E+03 0.32386036933615E+03 0.34018151873746E+03 0.30415000004062E+03 0.30415000007420E+03
 0.32156092253715E+03 0.34015260304476E+03 0.30415000003163E+03 0.30415000007383E+03 0.37238405254335E+03
 0.31247211880812E+03 0.13710761632628E+04 0.13146020764933E+04 0.46986534508651E+03 0.89751484205823E+03
 0.42530017024629E+03 0.80157712621071E+03 0.77471623985526E+03 0.75909311169471E+03 0.13276523844106E+04
 0.71473564138076E+03 0.77409861378316E+03 0.68402026646901E+03 0.13272919176272E+04 0.80157712621070E+03
 0.77471623985526E+03 0.75909311169468E+03 0.13276523844106E+04 0.71473564138076E+03 0.77409861378316E+03
 0.68402026646901E+03 0.13272919176272E+04 0.13148346472602E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44963105169644E+03 0.10919869685968E+01
 0.10919869685968E+01 0.11550866161388E+01 0.62419468909199E-01 0.31033456382222E+03 0.32940606712146E+03
 0.32861244504997E+03 0.32856582204819E+03 0.23000000000000E+00 0.00000000000000E+00 0.20187428998542E+00
 0.00000000000000E+00 -.90717373974348E+01 0.27814527882708E-02 0.52804023962480E+00 0.28761947834367E+04
 0.10785730437888E+04 0.15150360521169E+02 0.56813851954382E+01 0.31247087973515E+03 0.37239590499949E+03
 0.30565881148695E+03 0.30924844192682E+03 0.30415000000022E+03 0.30415000000760E+03 0.30565094145884E+03
 0.30924795608475E+03 0.30415000000022E+03 0.30415000000761E+03 0.30565881148695E+03 0.30924844192682E+03
 0.30415000000022E+03 0.30415000000760E+03 0.30565094145884E+03 0.30924795608475E+03 0.30415000000022E+03
 0.30415000000761E+03 0.30848941626641E+03 0.30415000008519E+03 0.57662224082033E+02 0.52828258451155E+02
 0.90205770528860E+02 0.25499700724625E+03 0.16434020786474E+03 0.75466008168725E+02 0.83391552537834E+02
 0.75466008168725E+02 0.20282648957743E+03 0.75126662903487E+02 0.83296198738594E+02 0.75126662903487E+02
 0.20273459601996E+03 0.75466008168725E+02 0.83391552537834E+02 0.75466008168725E+02 0.20282648957743E+03
 0.75126662903487E+02 0.83296198738594E+02 0.75126662903487E+02 0.20273459601996E+03 0.13312379302746E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34733573527033E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14738521207140E+00 0.00000000000000E+00 0.00000000000000E+00 0.14738521207140E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15430657927341E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15430657927341E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572969436492E+00 0.15573092384659E+00 0.32940606712146E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    440.12203362
 0.10509243057300E+00 0.31390824605763E+03 0.41745879273890E+03 0.41383133319293E+03 0.41268983335561E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17674948823106E+00 0.00000000000000E+00 -.15856072073777E+02
 0.33029958150552E-02 0.80805532026829E+00 0.24220436379409E+04 0.90826636422786E+03 0.99003122674124E+01
 0.37126171002796E+01 0.32775555727849E+03 0.30415000091273E+03 0.32419747480042E+03 0.34070893297999E+03
 0.30415000005511E+03 0.30415000010102E+03 0.32187520264576E+03 0.34068034560479E+03 0.30415000004297E+03
 0.30415000010053E+03 0.32419747480042E+03 0.34070893297999E+03 0.30415000005511E+03 0.30415000010102E+03
 0.32187520264576E+03 0.34068034560479E+03 0.30415000004297E+03 0.30415000010053E+03 0.37305458527945E+03
 0.31293936283397E+03 0.13751731116758E+04 0.13176075880962E+04 0.46934914079238E+03 0.89234013102830E+03
 0.42064424453196E+03 0.80432491214535E+03 0.77702634809901E+03 0.76108535352178E+03 0.13275047627510E+04
 0.71767000191692E+03 0.77642918775826E+03 0.68635967470378E+03 0.13271615105032E+04 0.80432491214534E+03
 0.77702634809901E+03 0.76108535352175E+03 0.13275047627510E+04 0.71767000191692E+03 0.77642918775826E+03
 0.68635967470378E+03 0.13271615105032E+04 0.13142772888131E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.44989811070193E+03 0.10919870909297E+01
 0.10919870909297E+01 0.11853869620590E+01 0.55350045157406E-01 0.31101534825428E+03 0.32940314232233E+03
 0.32872463216766E+03 0.32868608174826E+03 0.23000000000000E+00 0.00000000000000E+00 0.20100138763179E+00
 0.00000000000000E+00 -.90674211634444E+01 0.31367055923312E-02 0.54542967828738E+00 0.25504465639233E+04
 0.95641746147124E+03 0.14667335347647E+02 0.55002507553675E+01 0.31293815419150E+03 0.37306567185095E+03
 0.30568210066263E+03 0.30934267602643E+03 0.30415000000031E+03 0.30415000001037E+03 0.30567316006576E+03
 0.30934205832747E+03 0.30415000000031E+03 0.30415000001037E+03 0.30568210066263E+03 0.30934267602643E+03
 0.30415000000031E+03 0.30415000001037E+03 0.30567316006576E+03 0.30934205832747E+03 0.30415000000031E+03
 0.30415000001037E+03 0.30857073115240E+03 0.30415000011288E+03 0.56887204249296E+02 0.52692322700445E+02
 0.91972660734801E+02 0.25604363244019E+03 0.16361110840172E+03 0.77845378799097E+02 0.84964002678776E+02
 0.77845378799097E+02 0.20363151433377E+03 0.77533850126500E+02 0.84861387567675E+02 0.77533850126500E+02
 0.20353329125812E+03 0.77845378799097E+02 0.84964002678776E+02 0.77845378799097E+02 0.20363151433377E+03
 0.77533850126500E+02 0.84861387567675E+02 0.77533850126500E+02 0.20353329125812E+03 0.13378089859045E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34739932424760E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14741920002537E+00 0.00000000000000E+00 0.00000000000000E+00 0.14741920002537E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15436192669066E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15436192669066E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572994878331E+00 0.15573258511291E+00 0.32940314232233E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    450.01505210
 0.10510110137778E+00 0.31409412194977E+03 0.41772600831253E+03 0.41409539984767E+03 0.41295269532618E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17597048024952E+00 0.00000000000000E+00 -.15873547785934E+02
 0.33027230568118E-02 0.81937001690987E+00 0.24222436645120E+04 0.90834137419198E+03 0.97635986610430E+01
 0.36613494978911E+01 0.32813841075001E+03 0.30415000117947E+03 0.32452407825254E+03 0.34121875777814E+03
 0.30415000007359E+03 0.30415000013532E+03 0.32217989630016E+03 0.34119048448225E+03 0.30415000005746E+03
 0.30415000013466E+03 0.32452407825254E+03 0.34121875777814E+03 0.30415000007359E+03 0.30415000013532E+03
 0.32217989630016E+03 0.34119048448225E+03 0.30415000005746E+03 0.30415000013466E+03 0.37369866156598E+03
 0.31340445218509E+03 0.13790729480580E+04 0.13204436425180E+04 0.46883775708716E+03 0.88744656083271E+03
 0.41626461496011E+03 0.80695018665761E+03 0.77921912689876E+03 0.76297335784874E+03 0.13273737172377E+04
 0.72047576165786E+03 0.77864092533240E+03 0.68858312630242E+03 0.13270463209882E+04 0.80695018665760E+03
 0.77921912689876E+03 0.76297335784871E+03 0.13273737172377E+04 0.72047576165786E+03 0.77864092533240E+03
 0.68858312630242E+03 0.13270463209882E+04 0.13137527573241E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45016153751957E+03 0.10919872056888E+01
 0.10919872056888E+01 0.12150660174983E+01 0.48883292406480E-01 0.31167152126488E+03 0.32940956911820E+03
 0.32883150633157E+03 0.32879973831876E+03 0.23000000000000E+00 0.00000000000000E+00 0.20014673420709E+00
 0.00000000000000E+00 -.90630103341825E+01 0.35516590398984E-02 0.56240833969880E+00 0.22524684689972E+04
 0.84467567587394E+03 0.14224540134459E+02 0.53342025504221E+01 0.31340327211031E+03 0.37370902790043E+03
 0.30572890901509E+03 0.30943446031980E+03 0.30415000000042E+03 0.30415000001391E+03 0.30571989802335E+03
 0.30943371240070E+03 0.30415000000042E+03 0.30415000001391E+03 0.30572890901509E+03 0.30943446031980E+03
 0.30415000000042E+03 0.30415000001391E+03 0.30571989802335E+03 0.30943371240070E+03 0.30415000000042E+03
 0.30415000001391E+03 0.30864993992956E+03 0.30415000014740E+03 0.56030827532357E+02 0.52387244854515E+02
 0.93660094391026E+02 0.25711220558798E+03 0.16298381072500E+03 0.80046660883451E+02 0.86465866192021E+02
 0.80046660883451E+02 0.20445565406450E+03 0.79758344982677E+02 0.86356406441593E+02 0.79758344982677E+02
 0.20435150426626E+03 0.80046660883451E+02 0.86465866192021E+02 0.80046660883451E+02 0.20445565406450E+03
 0.79758344982677E+02 0.86356406441593E+02 0.79758344982677E+02 0.20435150426626E+03 0.13438704189979E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34747063175881E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14744417653523E+00 0.00000000000000E+00 0.00000000000000E+00 0.14744417653523E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15440601434964E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15440601434964E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573017034717E+00 0.15572979031203E+00 0.32940956911820E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    460.02167880
 0.10511450771226E+00 0.31428006325879E+03 0.41799817885908E+03 0.41436408597169E+03 0.41322002041798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17520197922699E+00 0.00000000000000E+00 -.15890939954640E+02
 0.33023015606475E-02 0.83050095434465E+00 0.24225528326466E+04 0.90845731224249E+03 0.96327402854255E+01
 0.36122776070345E+01 0.32852127545435E+03 0.30415000151624E+03 0.32485091909210E+03 0.34172795648995E+03
 0.30415000009767E+03 0.30415000018016E+03 0.32248499774791E+03 0.34169999303942E+03 0.30415000007637E+03
 0.30415000017931E+03 0.32485091909210E+03 0.34172795648995E+03 0.30415000009767E+03 0.30415000018016E+03
 0.32248499774791E+03 0.34169999303942E+03 0.30415000007637E+03 0.30415000017931E+03 0.37433849990301E+03
 0.31388083932808E+03 0.13829111408042E+04 0.13232143418279E+04 0.46830841621964E+03 0.88264733778862E+03
 0.41199737948788E+03 0.80954298329619E+03 0.78136918698051E+03 0.76482605928885E+03 0.13272499154851E+04
 0.72324905159599E+03 0.78080913839020E+03 0.69077069222998E+03 0.13269376174974E+04 0.80954298329618E+03
 0.78136918698051E+03 0.76482605928883E+03 0.13272499154851E+04 0.72324905159599E+03 0.78080913839020E+03
 0.69077069222998E+03 0.13269376174974E+04 0.13132379022158E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45042953530819E+03 0.10919873198994E+01
 0.10919873198994E+01 0.12450858976117E+01 0.42796505339098E-01 0.31232732742963E+03 0.32942466067606E+03
 0.32893685660035E+03 0.32891095569209E+03 0.23000000000000E+00 0.00000000000000E+00 0.19928278940063E+00
 0.00000000000000E+00 -.90588553762300E+01 0.40567980422511E-02 0.57952645751660E+00 0.19719985852588E+04
 0.73949946947206E+03 0.13804374064787E+02 0.51766402742951E+01 0.31387970130276E+03 0.37434816963721E+03
 0.30578183930665E+03 0.30952684735412E+03 0.30415000000057E+03 0.30415000001855E+03 0.30577285190265E+03
 0.30952596738692E+03 0.30415000000056E+03 0.30415000001856E+03 0.30578183930665E+03 0.30952684735412E+03
 0.30415000000057E+03 0.30415000001855E+03 0.30577285190265E+03 0.30952596738692E+03 0.30415000000056E+03
 0.30415000001856E+03 0.30872966584336E+03 0.30415000019145E+03 0.55073380370100E+02 0.51928404050271E+02
 0.95322222324251E+02 0.25822831722107E+03 0.16242948378520E+03 0.82244372490233E+02 0.87945917909252E+02
 0.82244372490233E+02 0.20531852722730E+03 0.81979953520723E+02 0.87829857144160E+02 0.81979953520723E+02
 0.20520870515838E+03 0.82244372490233E+02 0.87945917909252E+02 0.82244372490233E+02 0.20531852722730E+03
 0.81979953520723E+02 0.87829857144160E+02 0.81979953520723E+02 0.20520870515838E+03 0.13496603458441E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34755138683606E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14746604740490E+00 0.00000000000000E+00 0.00000000000000E+00 0.14746604740490E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15444189940889E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15444189940889E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573035739873E+00 0.15572286230574E+00 0.32942466067606E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    470.02185589
 0.10512985608578E+00 0.31446405723004E+03 0.41827168013157E+03 0.41463391997949E+03 0.41348839387672E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17445310711739E+00 0.00000000000000E+00 -.15907746177844E+02
 0.33018191766750E-02 0.84131702424916E+00 0.24229067589511E+04 0.90859003460665E+03 0.95089006514989E+01
 0.35658377443121E+01 0.32889965890265E+03 0.30415000193403E+03 0.32517415779649E+03 0.34223056611894E+03
 0.30415000012851E+03 0.30415000023775E+03 0.32278692259802E+03 0.34220290468417E+03 0.30415000010062E+03
 0.30415000023663E+03 0.32517415779650E+03 0.34223056611894E+03 0.30415000012851E+03 0.30415000023775E+03
 0.32278692259802E+03 0.34220290468417E+03 0.30415000010062E+03 0.30415000023663E+03 0.37496678200828E+03
 0.31436198618044E+03 0.13866463921897E+04 0.13258920061110E+04 0.46776745449757E+03 0.87799142323576E+03
 0.40788513146571E+03 0.81207471542107E+03 0.78345277714022E+03 0.76662433937466E+03 0.13271323795692E+04
 0.72595918340075E+03 0.78290990574230E+03 0.69289931539459E+03 0.13268342869655E+04 0.81207471542106E+03
 0.78345277714022E+03 0.76662433937464E+03 0.13271323795692E+04 0.72595918340075E+03 0.78290990574230E+03
 0.69289931539459E+03 0.13268342869655E+04 0.13127371336215E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45069865140947E+03 0.10919874302623E+01
 0.10919874302623E+01 0.12750864288709E+01 0.37164123883943E-01 0.31297615594940E+03 0.32944773057521E+03
 0.32903962948191E+03 0.32901871100575E+03 0.23000000000000E+00 0.00000000000000E+00 0.19842007421774E+00
 0.00000000000000E+00 -.90547023041110E+01 0.46716228770513E-02 0.59657695725627E+00 0.17124669971326E+04
 0.64217512392473E+03 0.13409837411074E+02 0.50286890291528E+01 0.31436089941455E+03 0.37497578926772E+03
 0.30583789641727E+03 0.30961878482377E+03 0.30415000000076E+03 0.30415000002453E+03 0.30582898478851E+03
 0.30961777300360E+03 0.30415000000076E+03 0.30415000002454E+03 0.30583789641727E+03 0.30961878482377E+03
 0.30415000000076E+03 0.30415000002453E+03 0.30582898478851E+03 0.30961777300360E+03 0.30415000000076E+03
 0.30415000002454E+03 0.30880899875907E+03 0.30415000024666E+03 0.54030601058388E+02 0.51333727894707E+02
 0.96939215847173E+02 0.25937368531609E+03 0.16194977338968E+03 0.84423646288848E+02 0.89386677485371E+02
 0.84423646288848E+02 0.20620589663297E+03 0.84183521789978E+02 0.89264356441856E+02 0.84183521789978E+02
 0.20609073985589E+03 0.84423646288848E+02 0.89386677485372E+02 0.84423646288848E+02 0.20620589663297E+03
 0.84183521789977E+02 0.89264356441856E+02 0.84183521789977E+02 0.20609073985589E+03 0.13551217167423E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34764029284594E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14748178389017E+00 0.00000000000000E+00 0.00000000000000E+00 0.14748178389017E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15446949324426E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15446949324426E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573051695766E+00 0.15571213453290E+00 0.32944773057521E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    480.00733259
 0.10514478469142E+00 0.31464618299718E+03 0.41854601064568E+03 0.41490450230982E+03 0.41375745353772E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17372396705075E+00 0.00000000000000E+00 -.15923976281820E+02
 0.33013501131930E-02 0.85181819690778E+00 0.24232510111636E+04 0.90871912918636E+03 0.93916753939293E+01
 0.35218782727235E+01 0.32927343176714E+03 0.30415000244826E+03 0.32549367395646E+03 0.34272643620908E+03
 0.30415000016765E+03 0.30415000031102E+03 0.32308555359162E+03 0.34269906895506E+03 0.30415000013146E+03
 0.30415000030958E+03 0.32549367395646E+03 0.34272643620908E+03 0.30415000016765E+03 0.30415000031102E+03
 0.32308555359162E+03 0.34269906895506E+03 0.30415000013146E+03 0.30415000030958E+03 0.37558358291414E+03
 0.31484672640842E+03 0.13902828955841E+04 0.13284822095829E+04 0.46721607987434E+03 0.87347294273895E+03
 0.40392078246524E+03 0.81454732081216E+03 0.78547232225927E+03 0.76837111590768E+03 0.13270204432642E+04
 0.72860804801528E+03 0.78494570155139E+03 0.69497180998682E+03 0.13267357118569E+04 0.81454732081216E+03
 0.78547232225927E+03 0.76837111590768E+03 0.13270204432642E+04 0.72860804801528E+03 0.78494570155139E+03
 0.69497180998682E+03 0.13267357118569E+04 0.13122503450338E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45096849627266E+03 0.10919875368419E+01
 0.10919875368419E+01 0.13050428589748E+01 0.31982047702716E-01 0.31361834300949E+03 0.32947816414698E+03
 0.32914001110953E+03 0.32912329319071E+03 0.23000000000000E+00 0.00000000000000E+00 0.19755946064088E+00
 0.00000000000000E+00 -.90505488641871E+01 0.54285693570951E-02 0.61354474064158E+00 0.14736847728664E+04
 0.55263178982489E+03 0.13038983907896E+02 0.48896189654612E+01 0.31484569584671E+03 0.37559196008707E+03
 0.30589604868320E+03 0.30971024687626E+03 0.30415000000103E+03 0.30415000003215E+03 0.30588725108392E+03
 0.30970910391440E+03 0.30415000000102E+03 0.30415000003216E+03 0.30589604868320E+03 0.30971024687626E+03
 0.30415000000103E+03 0.30415000003215E+03 0.30588725108392E+03 0.30970910391440E+03 0.30415000000102E+03
 0.30415000003216E+03 0.30888791610094E+03 0.30415000031527E+03 0.52908667386857E+02 0.50615628348163E+02
 0.98510983862614E+02 0.26054285659882E+03 0.16153931781689E+03 0.86586478429852E+02 0.90788180075407E+02
 0.86586478429852E+02 0.20711343846470E+03 0.86370886199017E+02 0.90659949908355E+02 0.86370886199017E+02
 0.20699329159607E+03 0.86586478429852E+02 0.90788180075407E+02 0.86586478429852E+02 0.20711343846470E+03
 0.86370886199017E+02 0.90659949908355E+02 0.86370886199017E+02 0.20699329159607E+03 0.13602646630234E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34773681714751E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14749168209725E+00 0.00000000000000E+00 0.00000000000000E+00 0.14749168209725E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15448923499585E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15448923499585E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573065123021E+00 0.15569790139831E+00 0.32947816414698E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    490.00289762
 0.10515763630715E+00 0.31482701449724E+03 0.41882162612520E+03 0.41517635027604E+03 0.41402773854032E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17301227827843E+00 0.00000000000000E+00 -.15939687522087E+02
 0.33009463784348E-02 0.86203863971554E+00 0.24235473960632E+04 0.90883027352370E+03 0.92803264626745E+01
 0.34801224235029E+01 0.32964368268553E+03 0.30415000307863E+03 0.32581038849543E+03 0.34321703192856E+03
 0.30415000021707E+03 0.30415000040379E+03 0.32338174690831E+03 0.34318995196326E+03 0.30415000017046E+03
 0.30415000040195E+03 0.32581038849543E+03 0.34321703192856E+03 0.30415000021707E+03 0.30415000040379E+03
 0.32338174690832E+03 0.34318995196326E+03 0.30415000017046E+03 0.30415000040195E+03 0.37619098265973E+03
 0.31533560135193E+03 0.13938362209613E+04 0.13309979190531E+04 0.46665357389737E+03 0.86907199148391E+03
 0.40008514971705E+03 0.81697059316052E+03 0.78743674235999E+03 0.77007430603692E+03 0.13269135560826E+04
 0.73120595767598E+03 0.78692554278488E+03 0.69699706666441E+03 0.13266414288004E+04 0.81697059316052E+03
 0.78743674235999E+03 0.77007430603692E+03 0.13269135560826E+04 0.73120595767598E+03 0.78692554278488E+03
 0.69699706666440E+03 0.13266414288004E+04 0.13117759406832E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45123959199467E+03 0.10919876400142E+01
 0.10919876400142E+01 0.13350295540750E+01 0.27229214112952E-01 0.31425576087826E+03 0.32951552417597E+03
 0.32923851660121E+03 0.32922532135657E+03 0.23000000000000E+00 0.00000000000000E+00 0.19669897805084E+00
 0.00000000000000E+00 -.90463702766318E+01 0.63761207604649E-02 0.63047054423920E+00 0.12546813808176E+04
 0.47050551780661E+03 0.12688935388177E+02 0.47583507705664E+01 0.31533462979728E+03 0.37619875967069E+03
 0.30595594380333E+03 0.30980150086525E+03 0.30415000000138E+03 0.30415000004183E+03 0.30594729167041E+03
 0.30980022751262E+03 0.30415000000137E+03 0.30415000004185E+03 0.30595594380333E+03 0.30980150086525E+03
 0.30415000000138E+03 0.30415000004183E+03 0.30594729167041E+03 0.30980022751262E+03 0.30415000000137E+03
 0.30415000004185E+03 0.30896664785433E+03 0.30415000040019E+03 0.51709469060344E+02 0.49781212976322E+02
 0.10004277413840E+03 0.26173515101117E+03 0.16119216300208E+03 0.88739464481487E+02 0.92155170328277E+02
 0.88739464481487E+02 0.20804054498908E+03 0.88548529649282E+02 0.92021368457427E+02 0.88548529649282E+02
 0.20791573887796E+03 0.88739464481488E+02 0.92155170328277E+02 0.88739464481488E+02 0.20804054498908E+03
 0.88548529649282E+02 0.92021368457427E+02 0.88548529649282E+02 0.20791573887796E+03 0.13651127513673E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34784077602670E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14749600382186E+00 0.00000000000000E+00 0.00000000000000E+00 0.14749600382186E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15450150867750E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15450150867750E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573076283861E+00 0.15568037422996E+00 0.32951552417597E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    500.01375285
 0.10516721351974E+00 0.31500672034611E+03 0.41909848133711E+03 0.41544946785252E+03 0.41429927062972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17231728040119E+00 0.00000000000000E+00 -.15954929666325E+02
 0.33006455045256E-02 0.87199041713162E+00 0.24237683171461E+04 0.90891311892979E+03 0.91744127490709E+01
 0.34404047809016E+01 0.33001073728352E+03 0.30415000384721E+03 0.32612457130791E+03 0.34370280155769E+03
 0.30415000027910E+03 0.30415000052048E+03 0.32367575131527E+03 0.34367600231452E+03 0.30415000021949E+03
 0.30415000051815E+03 0.32612457130791E+03 0.34370280155769E+03 0.30415000027910E+03 0.30415000052048E+03
 0.32367575131527E+03 0.34367600231452E+03 0.30415000021949E+03 0.30415000051815E+03 0.37678976890205E+03
 0.31582828110810E+03 0.13973138545570E+04 0.13334459338244E+04 0.46608024498362E+03 0.86477890997699E+03
 0.39636826376846E+03 0.81934896180349E+03 0.78935054417321E+03 0.77173777771596E+03 0.13268116789815E+04
 0.73375749278756E+03 0.78885399311333E+03 0.69897925496478E+03 0.13265514520155E+04 0.81934896180349E+03
 0.78935054417321E+03 0.77173777771595E+03 0.13268116789815E+04 0.73375749278756E+03 0.78885399311333E+03
 0.69897925496478E+03 0.13265514520155E+04 0.13113134448305E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45151194694738E+03 0.10919877401062E+01
 0.10919877401062E+01 0.13650621197521E+01 0.22916360549141E-01 0.31488849937937E+03 0.32955960239262E+03
 0.32933546353508E+03 0.32932518744441E+03 0.23000000000000E+00 0.00000000000000E+00 0.19583787327963E+00
 0.00000000000000E+00 -.90421951713181E+01 0.75761048843891E-02 0.64736992957258E+00 0.10559515901746E+04
 0.39598184631546E+03 0.12357694780914E+02 0.46341355428429E+01 0.31582737044418E+03 0.37679697460427E+03
 0.30601733072133E+03 0.30989262092564E+03 0.30415000000184E+03 0.30415000005405E+03 0.30600885077135E+03
 0.30989121819532E+03 0.30415000000183E+03 0.30415000005406E+03 0.30601733072133E+03 0.30989262092564E+03
 0.30415000000184E+03 0.30415000005405E+03 0.30600885077135E+03 0.30989121819532E+03 0.30415000000183E+03
 0.30415000005406E+03 0.30904527227955E+03 0.30415000050466E+03 0.50436879806789E+02 0.48836933242050E+02
 0.10153771538856E+03 0.26295709055280E+03 0.16091168658729E+03 0.90883852326911E+02 0.93490725840710E+02
 0.90883852326911E+02 0.20898294545102E+03 0.90717370117572E+02 0.93351688200326E+02 0.90717370117572E+02
 0.20885380751270E+03 0.90883852326912E+02 0.93490725840710E+02 0.90883852326912E+02 0.20898294545102E+03
 0.90717370117572E+02 0.93351688200326E+02 0.90717370117572E+02 0.20885380751270E+03 0.13696860405018E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34797008309317E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14749522563028E+00 0.00000000000000E+00 0.00000000000000E+00 0.14749522563028E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15450680960084E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15450680960084E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573085216897E+00 0.15565965399274E+00 0.32955960239262E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    511.47547612
 0.10517433187088E+00 0.31521026566074E+03 0.41941608179231E+03 0.41576282276274E+03 0.41461077961972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17154284506060E+00 0.00000000000000E+00 -.15973363531017E+02
 0.33004218048169E-02 0.88304505865661E+00 0.24239325980468E+04 0.90897472426754E+03 0.90595603492426E+01
 0.33973351309660E+01 0.33042629459862E+03 0.30415000493050E+03 0.32648048041255E+03 0.34425241108517E+03
 0.30415000036928E+03 0.30415000069052E+03 0.32400895964957E+03 0.34422592342221E+03 0.30415000029088E+03
 0.30415000068748E+03 0.32648048041255E+03 0.34425241108517E+03 0.30415000036928E+03 0.30415000069052E+03
 0.32400895964957E+03 0.34422592342221E+03 0.30415000029088E+03 0.30415000068748E+03 0.37746573214861E+03
 0.31639576944313E+03 0.14012023928290E+04 0.13361652377100E+04 0.46539348896926E+03 0.85995759284572E+03
 0.39223713643161E+03 0.82201602980897E+03 0.79147812879315E+03 0.77359209850043E+03 0.13266977574882E+04
 0.73662100378163E+03 0.79099743303548E+03 0.70119448708510E+03 0.13264503389524E+04 0.82201602980898E+03
 0.79147812879315E+03 0.77359209850043E+03 0.13266977574882E+04 0.73662100378163E+03 0.79099743303548E+03
 0.70119448708510E+03 0.13264503389524E+04 0.13107871818837E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45182441607486E+03 0.10919878611575E+01
 0.10919878611575E+01 0.13994472895608E+01 0.18753542242560E-01 0.31560246835440E+03 0.32961982167384E+03
 0.32944457165543E+03 0.32943688957808E+03 0.23000000000000E+00 0.00000000000000E+00 0.19484729826838E+00
 0.00000000000000E+00 -.90390398002445E+01 0.92578107226232E-02 0.66675518378157E+00 0.86413518699951E+03
 0.32405069512481E+03 0.11998406903456E+02 0.44994025887961E+01 0.31639493016584E+03 0.37747232052552E+03
 0.30608881353922E+03 0.30999659542325E+03 0.30415000000253E+03 0.30415000007189E+03 0.30608054890765E+03
 0.30999504707347E+03 0.30415000000251E+03 0.30415000007192E+03 0.30608881353922E+03 0.30999659542325E+03
 0.30415000000253E+03 0.30415000007189E+03 0.30608054890765E+03 0.30999504707347E+03 0.30415000000251E+03
 0.30415000007192E+03 0.30913502313070E+03 0.30415000065333E+03 0.48898325477417E+02 0.47623988644338E+02
 0.10321408821142E+03 0.26441682283250E+03 0.16068666418002E+03 0.93322197453072E+02 0.94992259287501E+02
 0.93322197453072E+02 0.21010510118386E+03 0.93181216117663E+02 0.94847691408062E+02 0.93181216117663E+02
 0.20997144917695E+03 0.93322197453072E+02 0.94992259287501E+02 0.93322197453072E+02 0.21010510118386E+03
 0.93181216117663E+02 0.94847691408062E+02 0.93181216117663E+02 0.20997144917695E+03 0.13746725724903E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34815201383538E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14750387523149E+00 0.00000000000000E+00 0.00000000000000E+00 0.14750387523149E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15449838374165E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15449838374165E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573088323077E+00 0.15563125406193E+00 0.32961982167384E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    520.04075641
 0.10518055603716E+00 0.31535939935247E+03 0.41965318228945E+03 0.41599662292927E+03 0.41484311956431E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17097853195621E+00 0.00000000000000E+00 -.15986114934382E+02
 0.33002262703581E-02 0.89107776871914E+00 0.24240762131537E+04 0.90902857993262E+03 0.89778920323637E+01
 0.33667095121364E+01 0.33073344990661E+03 0.30415000591764E+03 0.32674367562440E+03 0.34465855556720E+03
 0.30415000045391E+03 0.30415000085047E+03 0.32425546003550E+03 0.34463229366127E+03 0.30415000035801E+03
 0.30415000084676E+03 0.32674367562440E+03 0.34465855556720E+03 0.30415000045391E+03 0.30415000085047E+03
 0.32425546003550E+03 0.34463229366127E+03 0.30415000035801E+03 0.30415000084676E+03 0.37796441307067E+03
 0.31682198720722E+03 0.14040406163178E+04 0.13381288122312E+04 0.46485388208698E+03 0.85640444820385E+03
 0.38922629670643E+03 0.82396876028118E+03 0.79302060788773E+03 0.77493461970122E+03 0.13266058207059E+04
 0.73871922884889E+03 0.79255116651842E+03 0.70280433653566E+03 0.13263674504805E+04 0.82396876028118E+03
 0.79302060788773E+03 0.77493461970122E+03 0.13266058207059E+04 0.73871922884889E+03 0.79255116651842E+03
 0.70280433653566E+03 0.13263674504805E+04 0.13103843188294E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45205753124066E+03 0.10919879448933E+01
 0.10919879448933E+01 0.14251431304283E+01 0.16142310503287E-01 0.31612811665925E+03 0.32967053954351E+03
 0.32952480221340E+03 0.32951862909109E+03 0.23000000000000E+00 0.00000000000000E+00 0.19410464201714E+00
 0.00000000000000E+00 -.90364972999051E+01 0.10755383489715E-01 0.68125349327521E+00 0.74381355231544E+03
 0.27893008211829E+03 0.11743059050661E+02 0.44036471439979E+01 0.31682119942316E+03 0.37797058043222E+03
 0.30614289134726E+03 0.31007408682313E+03 0.30415000000321E+03 0.30415000008873E+03 0.30613479309122E+03
 0.31007243173616E+03 0.30415000000318E+03 0.30415000008876E+03 0.30614289134726E+03 0.31007408682313E+03
 0.30415000000321E+03 0.30415000008873E+03 0.30613479309122E+03 0.31007243173616E+03 0.30415000000318E+03
 0.30415000008876E+03 0.30920186807069E+03 0.30415000079004E+03 0.47690892746590E+02 0.46626228517854E+02
 0.10444204527251E+03 0.26553245752258E+03 0.16056820202371E+03 0.95135882228672E+02 0.96093449722686E+02
 0.95135882228672E+02 0.21097351856477E+03 0.95012030069289E+02 0.95945019906883E+02 0.95012030069289E+02
 0.21083674944128E+03 0.95135882228672E+02 0.96093449722686E+02 0.95135882228672E+02 0.21097351856477E+03
 0.95012030069289E+02 0.95945019906883E+02 0.95012030069289E+02 0.21083674944128E+03 0.13781976718635E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34828344091550E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14750071316184E+00 0.00000000000000E+00 0.00000000000000E+00 0.14750071316184E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15450284345211E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15450284345211E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573089222033E+00 0.15560732461571E+00 0.32967053954351E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    530.31909276
 0.10518087495424E+00 0.31553878999392E+03 0.41993827966278E+03 0.41627800310673E+03 0.41512281996658E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17031725863488E+00 0.00000000000000E+00 -.15999793167182E+02
 0.33002159885888E-02 0.90046359324560E+00 0.24240837653237E+04 0.90903141199640E+03 0.88843125474569E+01
 0.33316172052963E+01 0.33109896735407E+03 0.30415000732325E+03 0.32705708641927E+03 0.34514092383310E+03
 0.30415000057758E+03 0.30415000108462E+03 0.32454921837644E+03 0.34511492821611E+03 0.30415000045625E+03
 0.30415000107996E+03 0.32705708641927E+03 0.34514092383310E+03 0.30415000057758E+03 0.30415000108462E+03
 0.32454921837644E+03 0.34511492821611E+03 0.30415000045625E+03 0.30415000107996E+03 0.37855303468079E+03
 0.31733441116436E+03 0.14073842703306E+04 0.13404397636975E+04 0.46422163425057E+03 0.85227893894962E+03
 0.38573619652780E+03 0.82627413327889E+03 0.79483290060896E+03 0.77652079026247E+03 0.13265079423031E+04
 0.74119737726012E+03 0.79437632693897E+03 0.70470691854320E+03 0.13262798426707E+04 0.82627413327889E+03
 0.79483290060896E+03 0.77652079026247E+03 0.13265079423031E+04 0.74119737726012E+03 0.79437632693897E+03
 0.70470691854320E+03 0.13262798426707E+04 0.13099305278009E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45233810484729E+03 0.10919880347154E+01
 0.10919880347154E+01 0.14559781394694E+01 0.13483446299331E-01 0.31674257443935E+03 0.32973668335739E+03
 0.32961987977752E+03 0.32961513282834E+03 0.23000000000000E+00 0.00000000000000E+00 0.19321286699107E+00
 0.00000000000000E+00 -.90319519448971E+01 0.12876287771878E-01 0.69862725153322E+00 0.62129708047316E+03
 0.23298640517743E+03 0.11451027686714E+02 0.42941353825179E+01 0.31733369063139E+03 0.37855870119123E+03
 0.30620899691176E+03 0.31016708924730E+03 0.30415000000423E+03 0.30415000011346E+03 0.30620110543410E+03
 0.31016530753153E+03 0.30415000000419E+03 0.30415000011349E+03 0.30620899691176E+03 0.31016708924730E+03
 0.30415000000423E+03 0.30415000011346E+03 0.30620110543410E+03 0.31016530753153E+03 0.30415000000419E+03
 0.30415000011349E+03 0.30928209509889E+03 0.30415000098625E+03 0.46184972592991E+02 0.45324972246816E+02
 0.10588088038913E+03 0.26687841825101E+03 0.16046813345994E+03 0.97292527640867E+02 0.97385211501008E+02
 0.97292527640867E+02 0.21202983072348E+03 0.97187193416209E+02 0.97232490007436E+02 0.97187193416209E+02
 0.21188965351320E+03 0.97292527640867E+02 0.97385211501008E+02 0.97292527640867E+02 0.21202983072348E+03
 0.97187193416209E+02 0.97232490007436E+02 0.97187193416209E+02 0.21188965351320E+03 0.13821156840264E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34843549743821E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14748066930967E+00 0.00000000000000E+00 0.00000000000000E+00 0.14748066930967E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15448959492660E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15448959492660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573092218784E+00 0.15557614940450E+00 0.32973668335739E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    540.07480152
 0.10517619508622E+00 0.31570751420810E+03 0.42020894535405E+03 0.41654525771772E+03 0.41538849094589E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16970525099270E+00 0.00000000000000E+00 -.16009446133343E+02
 0.33003625721774E-02 0.90912414180104E+00 0.24239761011233E+04 0.90899103792125E+03 0.87996783191253E+01
 0.32998793696720E+01 0.33144236342499E+03 0.30415000893622E+03 0.32735168514473E+03 0.34559371726766E+03
 0.30415000072367E+03 0.30415000136177E+03 0.32482547834885E+03 0.34556796723874E+03 0.30415000057251E+03
 0.30415000135601E+03 0.32735168514473E+03 0.34559371726766E+03 0.30415000072367E+03 0.30415000136177E+03
 0.32482547834885E+03 0.34556796723874E+03 0.30415000057251E+03 0.30415000135601E+03 0.37910425439762E+03
 0.31782204790401E+03 0.14104975255972E+04 0.13425809861156E+04 0.46360938729824E+03 0.84844198143769E+03
 0.38251454720296E+03 0.82842528314997E+03 0.79651305356695E+03 0.77799421424229E+03 0.13264222540800E+04
 0.74351102510818E+03 0.79606809272963E+03 0.70647745416569E+03 0.13262033742794E+04 0.82842528314996E+03
 0.79651305356695E+03 0.77799421424229E+03 0.13264222540800E+04 0.74351102510818E+03 0.79606809272963E+03
 0.70647745416569E+03 0.13262033742794E+04 0.13095101114257E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45260459302354E+03 0.10919880981044E+01
 0.10919880981044E+01 0.14852452657631E+01 0.11366079197940E-01 0.31731248284183E+03 0.32980387059681E+03
 0.32970921852846E+03 0.32970552177264E+03 0.23000000000000E+00 0.00000000000000E+00 0.19236762928815E+00
 0.00000000000000E+00 -.90246005913273E+01 0.15274988591483E-01 0.71506161176532E+00 0.52373197872373E+03
 0.19639949202140E+03 0.11187847128655E+02 0.41954426732456E+01 0.31782139027550E+03 0.37910948104558E+03
 0.30627241384074E+03 0.31025513067768E+03 0.30415000000548E+03 0.30415000014282E+03 0.30626471828189E+03
 0.31025323126193E+03 0.30415000000543E+03 0.30415000014287E+03 0.30627241384074E+03 0.31025513067768E+03
 0.30415000000548E+03 0.30415000014282E+03 0.30626471828189E+03 0.31025323126193E+03 0.30415000000543E+03
 0.30415000014287E+03 0.30935800904816E+03 0.30415000121339E+03 0.44694932419449E+02 0.43991437573271E+02
 0.10721239198362E+03 0.26815954279095E+03 0.16041108884742E+03 0.99325921210207E+02 0.98581184228569E+02
 0.99325921210207E+02 0.21304130424306E+03 0.99236288594738E+02 0.98424665965597E+02 0.99236288594738E+02
 0.21289815160208E+03 0.99325921210207E+02 0.98581184228569E+02 0.99325921210207E+02 0.21304130424306E+03
 0.99236288594739E+02 0.98424665965597E+02 0.99236288594739E+02 0.21289815160208E+03 0.13855299729853E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34857535622380E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14742903221198E+00 0.00000000000000E+00 0.00000000000000E+00 0.14742903221198E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15446921722998E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15446921722998E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573101044172E+00 0.15554456110928E+00 0.32980387059681E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    550.19946669
 0.10515738003893E+00 0.31588495848916E+03 0.42049103279166E+03 0.41682433255506E+03 0.41566610221369E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16908572530901E+00 0.00000000000000E+00 -.16024130651734E+02
 0.33009528088261E-02 0.91786203920979E+00 0.24235426748936E+04 0.90882850308511E+03 0.87159068119730E+01
 0.32684650544899E+01 0.33179654007224E+03 0.30415001088051E+03 0.32765578839266E+03 0.34605930700803E+03
 0.30415000090379E+03 0.30415000170400E+03 0.32511096173003E+03 0.34603380980746E+03 0.30415000071607E+03
 0.30415000169690E+03 0.32765578839266E+03 0.34605930700803E+03 0.30415000090379E+03 0.30415000170400E+03
 0.32511096173003E+03 0.34603380980746E+03 0.30415000071607E+03 0.30415000169690E+03 0.37966621321724E+03
 0.31832714274856E+03 0.14136794981291E+04 0.13447776084734E+04 0.46300983287827E+03 0.84462815762962E+03
 0.37930327558696E+03 0.83062735701907E+03 0.79822828931499E+03 0.77951171561190E+03 0.13263546701566E+04
 0.74587990293392E+03 0.79779478012909E+03 0.70829846694040E+03 0.13261447854761E+04 0.83062735701907E+03
 0.79822828931499E+03 0.77951171561191E+03 0.13263546701566E+04 0.74587990293392E+03 0.79779478012909E+03
 0.70829846694040E+03 0.13261447854761E+04 0.13091215769383E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45288291339816E+03 0.10919881945346E+01
 0.10919881945346E+01 0.15156192612768E+01 0.95192882224659E-02 0.31788799528906E+03 0.32987755391513E+03
 0.32980146587231E+03 0.32979861500737E+03 0.23000000000000E+00 0.00000000000000E+00 0.19149338014050E+00
 0.00000000000000E+00 -.90217729414798E+01 0.18238414620458E-01 0.73202869628290E+00 0.43863461635675E+03
 0.16448798113378E+03 0.10928533322016E+02 0.40981999957562E+01 0.31832656476125E+03 0.37967096452652E+03
 0.30633973100673E+03 0.31034675706581E+03 0.30415000000708E+03 0.30415000017918E+03 0.30633224305041E+03
 0.31034473672086E+03 0.30415000000701E+03 0.30415000017924E+03 0.30633973100673E+03 0.31034675706581E+03
 0.30415000000708E+03 0.30415000017918E+03 0.30633224305041E+03 0.31034473672086E+03 0.30415000000701E+03
 0.30415000017924E+03 0.30943700760747E+03 0.30415000148904E+03 0.43102383792237E+02 0.42526156248887E+02
 0.10855188667534E+03 0.26947695250049E+03 0.16038230639177E+03 0.10140431282497E+03 0.99785391273024E+02
 0.10140431282497E+03 0.21408555400285E+03 0.10132917649241E+03 0.99625314473992E+02 0.10132917649241E+03
 0.21393968683920E+03 0.10140431282497E+03 0.99785391273024E+02 0.10140431282497E+03 0.21408555400285E+03
 0.10132917649241E+03 0.99625314473992E+02 0.10132917649241E+03 0.21393968683920E+03 0.13887443306323E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34871703406273E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14742038036585E+00 0.00000000000000E+00 0.00000000000000E+00 0.14742038036585E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15444091021629E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15444091021629E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573097261032E+00 0.15550978176704E+00 0.32987755391513E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    561.96563197
 0.10513540059542E+00 0.31608570801011E+03 0.42081798839113E+03 0.41714763164009E+03 0.41598758073082E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16838514508669E+00 0.00000000000000E+00 -.16040366495377E+02
 0.33016425850781E-02 0.92771152171539E+00 0.24230363504991E+04 0.90863863143717E+03 0.86233703179708E+01
 0.32337638692391E+01 0.33220337675942E+03 0.30415001362124E+03 0.32800521483119E+03 0.34659461140594E+03
 0.30415000116526E+03 0.30415000220169E+03 0.32543900751074E+03 0.34656939619450E+03 0.30415000092485E+03
 0.30415000219268E+03 0.32800521483119E+03 0.34659461140594E+03 0.30415000116526E+03 0.30415000220169E+03
 0.32543900751074E+03 0.34656939619450E+03 0.30415000092485E+03 0.30415000219268E+03 0.38031492051439E+03
 0.31891607206475E+03 0.14173007274109E+04 0.13472453167567E+04 0.46223862115126E+03 0.84016865401241E+03
 0.37561883975540E+03 0.83314021775066E+03 0.80016393421318E+03 0.78121831533366E+03 0.13262648915343E+04
 0.74858549665888E+03 0.79974303152757E+03 0.71035621357474E+03 0.13260648725060E+04 0.83314021775066E+03
 0.80016393421318E+03 0.78121831533366E+03 0.13262648915343E+04 0.74858549665888E+03 0.79974303152757E+03
 0.71035621357475E+03 0.13260648725060E+04 0.13086352184020E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45320530039885E+03 0.10919883011521E+01
 0.10919883011521E+01 0.15509177571080E+01 0.77455306228171E-02 0.31854902751455E+03 0.32996756201231E+03
 0.32990860027323E+03 0.32990649806554E+03 0.23000000000000E+00 0.00000000000000E+00 0.19048311131133E+00
 0.00000000000000E+00 -.90179670536884E+01 0.22415084107427E-01 0.75159824741010E+00 0.35690251982365E+03
 0.13383844493387E+03 0.10643984372724E+02 0.39914941397716E+01 0.31891557439471E+03 0.38031920208320E+03
 0.30641789592352E+03 0.31045254891895E+03 0.30415000000951E+03 0.30415000023226E+03 0.30641063771333E+03
 0.31045039257829E+03 0.30415000000941E+03 0.30415000023234E+03 0.30641789592352E+03 0.31045254891895E+03
 0.30415000000951E+03 0.30415000023226E+03 0.30641063771333E+03 0.31045039257829E+03 0.30415000000941E+03
 0.30415000023234E+03 0.30952813170191E+03 0.30415000188101E+03 0.41171596059188E+02 0.40718591860404E+02
 0.11006455019332E+03 0.27100619023728E+03 0.16039131729299E+03 0.10380377174879E+03 0.10114555925654E+03
 0.10380377174879E+03 0.21530118443160E+03 0.10374346941912E+03 0.10098165641754E+03 0.10374346941912E+03
 0.21515244064459E+03 0.10380377174879E+03 0.10114555925654E+03 0.10380377174879E+03 0.21530118443160E+03
 0.10374346941912E+03 0.10098165641754E+03 0.10374346941912E+03 0.21515244064459E+03 0.13921378725570E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34887799996281E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14740307416170E+00 0.00000000000000E+00 0.00000000000000E+00 0.14740307416170E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15440076739865E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15440076739865E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573092825196E+00 0.15546732054298E+00 0.32996756201231E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    570.16881967
 0.10511508879718E+00 0.31622495181350E+03 0.42104571690688E+03 0.41737296889668E+03 0.41621168771622E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16790873848447E+00 0.00000000000000E+00 -.16049121427632E+02
 0.33022803542547E-02 0.93438868003677E+00 0.24225683896562E+04 0.90846314612107E+03 0.85617475585055E+01
 0.32106553344396E+01 0.33248464494003E+03 0.30415001587484E+03 0.32824693731196E+03 0.34696406144796E+03
 0.30415000138573E+03 0.30415000262197E+03 0.32566610985120E+03 0.34693903896372E+03 0.30415000110119E+03
 0.30415000261136E+03 0.32824693731196E+03 0.34696406144796E+03 0.30415000138573E+03 0.30415000262197E+03
 0.32566610985120E+03 0.34693903896372E+03 0.30415000110119E+03 0.30415000261136E+03 0.38076032037252E+03
 0.31932626953260E+03 0.14197806609153E+04 0.13489282186331E+04 0.46170757715026E+03 0.83714518159694E+03
 0.37312906656092E+03 0.83486482919204E+03 0.80148429236157E+03 0.78238534651764E+03 0.13262059884522E+04
 0.75044307688463E+03 0.80107175855988E+03 0.71176506519481E+03 0.13260124678810E+04 0.83486482919204E+03
 0.80148429236157E+03 0.78238534651764E+03 0.13262059884522E+04 0.75044307688463E+03 0.80107175855988E+03
 0.71176506519481E+03 0.13260124678810E+04 0.13083097799593E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45343000920013E+03 0.10919883586440E+01
 0.10919883586440E+01 0.15755273202133E+01 0.67079062769682E-02 0.31900217942910E+03 0.33003282804432E+03
 0.32998349967293E+03 0.32998180185728E+03 0.23000000000000E+00 0.00000000000000E+00 0.18978341875657E+00
 0.00000000000000E+00 -.90128365802717E+01 0.25882400516716E-01 0.76512913941461E+00 0.30909034093779E+03
 0.11590887785167E+03 0.10455751307708E+02 0.39209067403906E+01 0.31932582775247E+03 0.38076429203781E+03
 0.30647312876385E+03 0.31052624380139E+03 0.30415000001164E+03 0.30415000027724E+03 0.30646602823728E+03
 0.31052399439625E+03 0.30415000001152E+03 0.30415000027733E+03 0.30647312876385E+03 0.31052624380139E+03
 0.30415000001164E+03 0.30415000027724E+03 0.30646602823728E+03 0.31052399439625E+03 0.30415000001152E+03
 0.30415000027733E+03 0.30959159163029E+03 0.30415000220569E+03 0.39787172475750E+02 0.39403813166482E+02
 0.11108977666344E+03 0.27206426459805E+03 0.16041903905129E+03 0.10546182857770E+03 0.10206733328569E+03
 0.10546182857770E+03 0.21614320450844E+03 0.10541068762434E+03 0.10190096454331E+03 0.10541068762434E+03
 0.21599264471812E+03 0.10546182857770E+03 0.10206733328569E+03 0.10546182857770E+03 0.21614320450844E+03
 0.10541068762434E+03 0.10190096454331E+03 0.10541068762434E+03 0.21599264471812E+03 0.13942879813329E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34898838425685E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14736598520561E+00 0.00000000000000E+00 0.00000000000000E+00 0.14736598520561E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15436693104301E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15436693104301E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573095054391E+00 0.15543660756954E+00 0.33003282804432E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    581.10640327
 0.10507946801326E+00 0.31641057966411E+03 0.42134940009529E+03 0.41767376162034E+03 0.41651092852538E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16728844415317E+00 0.00000000000000E+00 -.16060804203358E+02
 0.33033994970218E-02 0.94305567862769E+00 0.24217476594074E+04 0.90815537227777E+03 0.84830622213540E+01
 0.31811483330078E+01 0.33285677307775E+03 0.30415001936913E+03 0.32856692042364E+03 0.34745216089345E+03
 0.30415000173563E+03 0.30415000328985E+03 0.32596692734519E+03 0.34742739011458E+03 0.30415000138150E+03
 0.30415000327675E+03 0.32856692042364E+03 0.34745216089345E+03 0.30415000173563E+03 0.30415000328985E+03
 0.32596692734519E+03 0.34742739011458E+03 0.30415000138150E+03 0.30415000327675E+03 0.38134683297062E+03
 0.31987256409677E+03 0.14230397728672E+04 0.13511384169566E+04 0.46100166730410E+03 0.83320430760017E+03
 0.36989763195955E+03 0.83713477482736E+03 0.80321432370209E+03 0.78392184830595E+03 0.13261388341407E+04
 0.75288879088650E+03 0.80281243965032E+03 0.71362038922604E+03 0.13259535235522E+04 0.83713477482736E+03
 0.80321432370209E+03 0.78392184830595E+03 0.13261388341407E+04 0.75288879088650E+03 0.80281243965032E+03
 0.71362038922604E+03 0.13259535235522E+04 0.13078964839463E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45372998266333E+03 0.10919884353624E+01
 0.10919884353624E+01 0.16083400710204E+01 0.55371996477969E-02 0.31959697888037E+03 0.33012288872350E+03
 0.33008403268065E+03 0.33008275783794E+03 0.23000000000000E+00 0.00000000000000E+00 0.18885756243858E+00
 0.00000000000000E+00 -.90060825630229E+01 0.31354605825161E-01 0.78300570725676E+00 0.25514592798932E+03
 0.95679722995993E+02 0.10217039193785E+02 0.38313896976695E+01 0.31987220216081E+03 0.38135039824768E+03
 0.30654751982265E+03 0.31062435304261E+03 0.30415000001515E+03 0.30415000034894E+03 0.30654062431411E+03
 0.31062198201179E+03 0.30415000001500E+03 0.30415000034906E+03 0.30654751982265E+03 0.31062435304261E+03
 0.30415000001515E+03 0.30415000034894E+03 0.30654062431411E+03 0.31062198201179E+03 0.30415000001500E+03
 0.30415000034906E+03 0.30967605819711E+03 0.30415000271253E+03 0.37895453051501E+02 0.37586991538022E+02
 0.11242054953697E+03 0.27346560874765E+03 0.16048295646299E+03 0.10765276482140E+03 0.10326392559886E+03
 0.10765276482140E+03 0.21725900043605E+03 0.10761247316826E+03 0.10309452197890E+03 0.10761247316826E+03
 0.21710625545255E+03 0.10765276482140E+03 0.10326392559886E+03 0.10765276482140E+03 0.21725900043605E+03
 0.10761247316826E+03 0.10309452197890E+03 0.10761247316826E+03 0.21710625545255E+03 0.13969066229363E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34913405230628E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14731656677279E+00 0.00000000000000E+00 0.00000000000000E+00 0.14731656677279E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15431639699602E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15431639699602E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573096999920E+00 0.15539423408530E+00 0.33012288872350E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    592.04398688
 0.10503581743856E+00 0.31659539004171E+03 0.42165297263410E+03 0.41797470294553E+03 0.41681038473711E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16668470059729E+00 0.00000000000000E+00 -.16072249981640E+02
 0.33047720230162E-02 0.95146144027162E+00 0.24207418679061E+04 0.90777820046478E+03 0.84081179345704E+01
 0.31530442254639E+01 0.33322566478163E+03 0.30415002349882E+03 0.32888430909980E+03 0.34793532045470E+03
 0.30415000215994E+03 0.30415000410086E+03 0.32626550415151E+03 0.34791079528264E+03 0.30415000172201E+03
 0.30415000408479E+03 0.32888430909980E+03 0.34793532045470E+03 0.30415000215994E+03 0.30415000410086E+03
 0.32626550415151E+03 0.34791079528264E+03 0.30415000172201E+03 0.30415000408479E+03 0.38192570378304E+03
 0.32041798415308E+03 0.14262466494605E+04 0.13533072506201E+04 0.46029271974609E+03 0.82935058856965E+03
 0.36675640522483E+03 0.83937233124398E+03 0.80491047624681E+03 0.78543298850218E+03 0.13260823989599E+04
 0.75530045686887E+03 0.80451869309370E+03 0.71544667362987E+03 0.13259048115757E+04 0.83937233124398E+03
 0.80491047624681E+03 0.78543298850218E+03 0.13260823989599E+04 0.75530045686887E+03 0.80451869309370E+03
 0.71544667362987E+03 0.13259048115757E+04 0.13075005959138E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45403011724509E+03 0.10919885105245E+01
 0.10919885105245E+01 0.16411528218275E+01 0.45707236957240E-02 0.32018348787613E+03 0.33021626804050E+03
 0.33018569666314E+03 0.33018474173537E+03 0.23000000000000E+00 0.00000000000000E+00 0.18794085485789E+00
 0.00000000000000E+00 -.89991608225749E+01 0.37984511728511E-01 0.80067450196472E+00 0.21061215837600E+03
 0.78979559390999E+02 0.99915758280917E+01 0.37468409355344E+01 0.32041770352934E+03 0.38192888516839E+03
 0.30662265484226E+03 0.31072225707230E+03 0.30415000001960E+03 0.30415000043633E+03 0.30661595706174E+03
 0.31071976731519E+03 0.30415000001939E+03 0.30415000043648E+03 0.30662265484226E+03 0.31072225707230E+03
 0.30415000001960E+03 0.30415000043633E+03 0.30661595706174E+03 0.31071976731519E+03 0.30415000001939E+03
 0.30415000043648E+03 0.30976032855981E+03 0.30415000331598E+03 0.35955082796200E+02 0.35706638946496E+02
 0.11371251042688E+03 0.27485801457323E+03 0.16057694159421E+03 0.10982284180379E+03 0.10442565957565E+03
 0.10982284180379E+03 0.21836758716118E+03 0.10979199472174E+03 0.10425348514993E+03 0.10979199472174E+03
 0.21821290125042E+03 0.10982284180379E+03 0.10442565957565E+03 0.10982284180379E+03 0.21836758716118E+03
 0.10979199472174E+03 0.10425348514993E+03 0.10979199472174E+03 0.21821290125042E+03 0.13992805035851E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34927863302480E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14726490731962E+00 0.00000000000000E+00 0.00000000000000E+00 0.14726490731962E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15426022225821E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15426022225821E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573098514972E+00 0.15535031811324E+00 0.33021626804050E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    600.06829848
 0.10500143385602E+00 0.31672966059041E+03 0.42187530822046E+03 0.41819516029884E+03 0.41702975263044E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16625201762980E+00 0.00000000000000E+00 -.16076706668957E+02
 0.33058539824508E-02 0.95746735321608E+00 0.24199495931969E+04 0.90748109744883E+03 0.83553762675338E+01
 0.31332661003252E+01 0.33349399878680E+03 0.30415002703813E+03 0.32911528580779E+03 0.34828636235285E+03
 0.30415000253249E+03 0.30415000481381E+03 0.32648291055994E+03 0.34826201292617E+03 0.30415000202149E+03
 0.30415000479518E+03 0.32911528580779E+03 0.34828636235285E+03 0.30415000253249E+03 0.30415000481381E+03
 0.32648291055994E+03 0.34826201292617E+03 0.30415000202149E+03 0.30415000479518E+03 0.38234520247320E+03
 0.32081760342678E+03 0.14285653571524E+04 0.13548680725048E+04 0.45977298835862E+03 0.82658102921395E+03
 0.36450917591354E+03 0.84099284090194E+03 0.80613395144446E+03 0.78652213466422E+03 0.13260484112415E+04
 0.75704748692052E+03 0.80574925569672E+03 0.71676463183503E+03 0.13258762111988E+04 0.84099284090194E+03
 0.80613395144445E+03 0.78652213466422E+03 0.13260484112415E+04 0.75704748692052E+03 0.80574925569672E+03
 0.71676463183502E+03 0.13258762111988E+04 0.13072207720930E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45424998059263E+03 0.10919885397907E+01
 0.10919885397907E+01 0.16652257566563E+01 0.39710901522954E-02 0.32060940719831E+03 0.33028683739617E+03
 0.33026121743100E+03 0.33026044621444E+03 0.23000000000000E+00 0.00000000000000E+00 0.18727468733955E+00
 0.00000000000000E+00 -.89901986138621E+01 0.43720161940095E-01 0.81349485438867E+00 0.18298193888123E+03
 0.68618227080462E+02 0.98341126029764E+01 0.36877922261161E+01 0.32081737492237E+03 0.38234813514739E+03
 0.30667817766043E+03 0.31079389324212E+03 0.30415000002366E+03 0.30415000051343E+03 0.30667161928917E+03
 0.31079131832753E+03 0.30415000002341E+03 0.30415000051361E+03 0.30667817766043E+03 0.31079389324212E+03
 0.30415000002366E+03 0.30415000051343E+03 0.30667161928917E+03 0.31079131832753E+03 0.30415000002341E+03
 0.30415000051361E+03 0.30982198313349E+03 0.30415000383672E+03 0.34502308887772E+02 0.34290470786362E+02
 0.11463910941679E+03 0.27587749429845E+03 0.16066518933458E+03 0.11140782304371E+03 0.10525857372648E+03
 0.11140782304371E+03 0.21917880257926E+03 0.11138310375904E+03 0.10508448683149E+03 0.11138310375904E+03
 0.21902279977885E+03 0.11140782304371E+03 0.10525857372648E+03 0.11140782304371E+03 0.21917880257926E+03
 0.11138310375904E+03 0.10508448683149E+03 0.11138310375904E+03 0.21902279977885E+03 0.14008968011007E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34938434005893E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14718857392261E+00 0.00000000000000E+00 0.00000000000000E+00 0.14718857392261E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15421586523520E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15421586523520E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573108605239E+00 0.15531724712100E+00 0.33028683739617E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    610.61677391
 0.10494226867016E+00 0.31690935353570E+03 0.42216854774318E+03 0.41848650153033E+03 0.41731987088030E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16569598662942E+00 0.00000000000000E+00 -.16089834367368E+02
 0.33077174977678E-02 0.96515889770762E+00 0.24185862321673E+04 0.90696983706273E+03 0.82887906012171E+01
 0.31082964754564E+01 0.33384530818869E+03 0.30415003221500E+03 0.32941791499313E+03 0.34874469937459E+03
 0.30415000308642E+03 0.30415000587468E+03 0.32676803900368E+03 0.34872057936885E+03 0.30415000246729E+03
 0.30415000585226E+03 0.32941791499313E+03 0.34874469937459E+03 0.30415000308642E+03 0.30415000587468E+03
 0.32676803900368E+03 0.34872057936885E+03 0.30415000246729E+03 0.30415000585226E+03 0.38288940118096E+03
 0.32134022289376E+03 0.14315862891535E+04 0.13569178487654E+04 0.45912143706296E+03 0.82306771672066E+03
 0.36165067247239E+03 0.84310557440113E+03 0.80772802930866E+03 0.78795691069530E+03 0.13260262781019E+04
 0.75932508001560E+03 0.80735223643022E+03 0.71849598690345E+03 0.13258607667400E+04 0.84310557440113E+03
 0.80772802930866E+03 0.78795691069530E+03 0.13260262781019E+04 0.75932508001560E+03 0.80735223643021E+03
 0.71849598690345E+03 0.13258607667400E+04 0.13068975744043E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45454058952891E+03 0.10919886259977E+01
 0.10919886259977E+01 0.16968711829193E+01 0.33007661511837E-02 0.32116074780789E+03 0.33038211040890E+03
 0.33036181870120E+03 0.33036123739216E+03 0.23000000000000E+00 0.00000000000000E+00 0.18640786374826E+00
 0.00000000000000E+00 -.89856203522050E+01 0.52598909543774E-01 0.83015221366198E+00 0.15209440783829E+03
 0.57035402939358E+02 0.96367869269543E+01 0.36137950976079E+01 0.32134009077546E+03 0.38289196031815E+03
 0.30675238576620E+03 0.31088831764183E+03 0.30415000002987E+03 0.30415000062844E+03 0.30674600749634E+03
 0.31088563245694E+03 0.30415000002955E+03 0.30415000062866E+03 0.30675238576620E+03 0.31088831764183E+03
 0.30415000002987E+03 0.30415000062844E+03 0.30674600749634E+03 0.31088563245694E+03 0.30415000002955E+03
 0.30415000062866E+03 0.30990325447566E+03 0.30415000460193E+03 0.32570592723595E+02 0.32396916592860E+02
 0.11582345850462E+03 0.27720348261994E+03 0.16080090682280E+03 0.11346003510229E+03 0.10632368582894E+03
 0.11346003510229E+03 0.22023280004489E+03 0.11344242478484E+03 0.10614736038767E+03 0.11344242478484E+03
 0.22007533031717E+03 0.11346003510229E+03 0.10632368582894E+03 0.11346003510229E+03 0.22023280004489E+03
 0.11344242478484E+03 0.10614736038767E+03 0.11344242478484E+03 0.22007533031717E+03 0.14028459000299E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34952338643258E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14715889834683E+00 0.00000000000000E+00 0.00000000000000E+00 0.14715889834683E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15415387732747E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15415387732747E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573103731750E+00 0.15527241303943E+00 0.33038211040890E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    620.89187808
 0.10488828933228E+00 0.31707900593955E+03 0.42245286753274E+03 0.41876870617179E+03 0.41760073770597E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16516794849028E+00 0.00000000000000E+00 -.16104892150835E+02
 0.33094194942119E-02 0.97243989573696E+00 0.24173423810405E+04 0.90650339289018E+03 0.82267295234090E+01
 0.30850235712784E+01 0.33418375475353E+03 0.30415003813131E+03 0.32970947583780E+03 0.34918710807230E+03
 0.30415000373525E+03 0.30415000711864E+03 0.32704264759103E+03 0.34916320184112E+03 0.30415000299038E+03
 0.30415000709186E+03 0.32970947583780E+03 0.34918710807231E+03 0.30415000373525E+03 0.30415000711864E+03
 0.32704264759103E+03 0.34916320184112E+03 0.30415000299038E+03 0.30415000709186E+03 0.38341959242135E+03
 0.32185110715101E+03 0.14344815544680E+04 0.13588543563847E+04 0.45839991473787E+03 0.81954858379697E+03
 0.35885666948541E+03 0.84513493125862E+03 0.80924271765509E+03 0.78931227367647E+03 0.13259932748453E+04
 0.76151466949647E+03 0.80887518143755E+03 0.72014032274810E+03 0.13258339543411E+04 0.84513493125862E+03
 0.80924271765508E+03 0.78931227367647E+03 0.13259932748453E+04 0.76151466949647E+03 0.80887518143755E+03
 0.72014032274810E+03 0.13258339543411E+04 0.13065370338640E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45482204066095E+03 0.10919887248791E+01
 0.10919887248791E+01 0.17276964954439E+01 0.27567957596162E-02 0.32169687661185E+03 0.33047764961232E+03
 0.33046151174714E+03 0.33046107209287E+03 0.23000000000000E+00 0.00000000000000E+00 0.18557373920443E+00
 0.00000000000000E+00 -.89835695180605E+01 0.62977714388479E-01 0.84615449704603E+00 0.12702906222750E+03
 0.47635898335314E+02 0.94545381817723E+01 0.35454518181646E+01 0.32185104500983E+03 0.38342186452414E+03
 0.30682387162940E+03 0.31097938629175E+03 0.30415000003745E+03 0.30415000076381E+03 0.30681765591391E+03
 0.31097659732966E+03 0.30415000003704E+03 0.30415000076409E+03 0.30682387162940E+03 0.31097938629175E+03
 0.30415000003745E+03 0.30415000076381E+03 0.30681765591391E+03 0.31097659732966E+03 0.30415000003704E+03
 0.30415000076409E+03 0.30998160525455E+03 0.30415000548248E+03 0.30634283847127E+02 0.30492491090438E+02
 0.11695357842769E+03 0.27850297827724E+03 0.16096463195742E+03 0.11545496294825E+03 0.10734042312613E+03
 0.11545496294825E+03 0.22126561292261E+03 0.11544339051885E+03 0.10716205630254E+03 0.11544339051885E+03
 0.22110682870245E+03 0.11545496294825E+03 0.10734042312613E+03 0.11545496294825E+03 0.22126561292261E+03
 0.11544339051885E+03 0.10716205630254E+03 0.11544339051885E+03 0.22110682870245E+03 0.14046123521500E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34965899376407E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14715349675664E+00 0.00000000000000E+00 0.00000000000000E+00 0.14715349675664E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15409094490018E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15409094490018E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573092516311E+00 0.15522740835080E+00 0.33047764961232E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    630.49494693
 0.10483182116863E+00 0.31723904680459E+03 0.42271862579862E+03 0.41903275367794E+03 0.41786363727688E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16468625106414E+00 0.00000000000000E+00 -.16111417536326E+02
 0.33112018708221E-02 0.97905867145856E+00 0.24160411572894E+04 0.90601543398353E+03 0.81711139824562E+01
 0.30641677434211E+01 0.33449880564446E+03 0.30415004438916E+03 0.32998115953311E+03 0.34959700381190E+03
 0.30415000443469E+03 0.30415000846071E+03 0.32729895647780E+03 0.34957329747672E+03 0.30415000355505E+03
 0.30415000842930E+03 0.32998115953312E+03 0.34959700381190E+03 0.30415000443469E+03 0.30415000846071E+03
 0.32729895647780E+03 0.34957329747672E+03 0.30415000355505E+03 0.30415000842930E+03 0.38390323241444E+03
 0.32232388881124E+03 0.14371490014015E+04 0.13606399411100E+04 0.45780511797121E+03 0.81647479187944E+03
 0.35638064831837E+03 0.84700817970542E+03 0.81064064644534E+03 0.79056644822223E+03 0.13259778582030E+04
 0.76353510452012E+03 0.81028048251076E+03 0.72165903808704E+03 0.13258239845552E+04 0.84700817970541E+03
 0.81064064644534E+03 0.79056644822223E+03 0.13259778582030E+04 0.76353510452012E+03 0.81028048251076E+03
 0.72165903808704E+03 0.13258239845552E+04 0.13062550858519E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45508540825416E+03 0.10919887677301E+01
 0.10919887677301E+01 0.17565057019773E+01 0.23296914400937E-02 0.32219052643585E+03 0.33056930874630E+03
 0.33055629542801E+03 0.33055595754714E+03 0.23000000000000E+00 0.00000000000000E+00 0.18480374565349E+00
 0.00000000000000E+00 -.89742352701985E+01 0.74523470791351E-01 0.86090292072480E+00 0.10734873074281E+03
 0.40255774028552E+02 0.92925692402864E+01 0.34847134651074E+01 0.32232389849998E+03 0.38390523365901E+03
 0.30689264193398E+03 0.31106513040721E+03 0.30415000004588E+03 0.30415000091031E+03 0.30688657615190E+03
 0.31106224560211E+03 0.30415000004538E+03 0.30415000091064E+03 0.30689264193398E+03 0.31106513040721E+03
 0.30415000004588E+03 0.30415000091031E+03 0.30688657615190E+03 0.31106224560211E+03 0.30415000004538E+03
 0.30415000091064E+03 0.31005539371687E+03 0.30415000641877E+03 0.28822325641938E+02 0.28705567874931E+02
 0.11798330636330E+03 0.27970682983678E+03 0.16113360694166E+03 0.11730042978514E+03 0.10826589534253E+03
 0.11730042978514E+03 0.22221965534458E+03 0.11729377064959E+03 0.10808578160883E+03 0.11729377064959E+03
 0.22205979598988E+03 0.11730042978514E+03 0.10826589534253E+03 0.11730042978514E+03 0.22221965534458E+03
 0.11729377064959E+03 0.10808578160883E+03 0.11729377064959E+03 0.22205979598988E+03 0.14061683160002E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34978661182532E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14707413192937E+00 0.00000000000000E+00 0.00000000000000E+00 0.14707413192937E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15403607481564E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15403607481564E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573099624423E+00 0.15518445741687E+00 0.33056930874630E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    640.16397716
 0.10477581957450E+00 0.31739799647415E+03 0.42298532081676E+03 0.41929765466853E+03 0.41812733794692E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16421247243338E+00 0.00000000000000E+00 -.16119924792470E+02
 0.33129714154777E-02 0.98554819939261E+00 0.24147506865363E+04 0.90553150745112E+03 0.81173097418578E+01
 0.30439911531967E+01 0.33481335107004E+03 0.30415005156878E+03 0.33025246777190E+03 0.35000646745447E+03
 0.30415000525344E+03 0.30415001003291E+03 0.32755492097920E+03 0.34998295609801E+03 0.30415000421700E+03
 0.30415000999615E+03 0.33025246777190E+03 0.35000646745447E+03 0.30415000525344E+03 0.30415001003291E+03
 0.32755492097920E+03 0.34998295609801E+03 0.30415000421700E+03 0.30415000999615E+03 0.38438859056523E+03
 0.32280009582329E+03 0.14397984292281E+04 0.13624018970644E+04 0.45715913301987E+03 0.81334515516492E+03
 0.35390022647995E+03 0.84887225741914E+03 0.81202077241332E+03 0.79180607210853E+03 0.13259576720252E+04
 0.76554677208016E+03 0.81166770435991E+03 0.72316383969351E+03 0.13258090156123E+04 0.84887225741914E+03
 0.81202077241333E+03 0.79180607210853E+03 0.13259576720252E+04 0.76554677208016E+03 0.81166770435991E+03
 0.72316383969351E+03 0.13258090156123E+04 0.13059508662345E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45534961209767E+03 0.10919888235955E+01
 0.10919888235955E+01 0.17855127926919E+01 0.19664542833580E-02 0.32268479979658E+03 0.33066386675907E+03
 0.33065340644546E+03 0.33065314813945E+03 0.23000000000000E+00 0.00000000000000E+00 0.18403814498623E+00
 0.00000000000000E+00 -.89665863200555E+01 0.88289206315614E-01 0.87554427955588E+00 0.90611302715779E+02
 0.33979238518417E+02 0.91371735122957E+01 0.34264400671109E+01 0.32280017776706E+03 0.38439033416733E+03
 0.30696166007301E+03 0.31115096906528E+03 0.30415000005611E+03 0.30415000108248E+03 0.30695573648314E+03
 0.31114799037161E+03 0.30415000005548E+03 0.30415000108287E+03 0.30696166007301E+03 0.31115096906528E+03
 0.30415000005611E+03 0.30415000108248E+03 0.30695573648314E+03 0.31114799037161E+03 0.30415000005548E+03
 0.30415000108287E+03 0.31012925179611E+03 0.30415000749893E+03 0.26963753901613E+02 0.26867614455904E+02
 0.11900043209301E+03 0.28092250041439E+03 0.16132706616091E+03 0.11914913439954E+03 0.10918048090138E+03
 0.11914913439954E+03 0.22318227427902E+03 0.11914680468195E+03 0.10899874311890E+03 0.11914680468195E+03
 0.22302144948882E+03 0.11914913439954E+03 0.10918048090138E+03 0.11914913439954E+03 0.22318227427902E+03
 0.11914680468195E+03 0.10899874311890E+03 0.11914680468195E+03 0.22302144948882E+03 0.14076412052891E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34991507619090E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14701398872301E+00 0.00000000000000E+00 0.00000000000000E+00 0.14701398872301E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15396623969082E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15396623969082E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573102056578E+00 0.15514011783315E+00 0.33066386675907E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    651.21429744
 0.10470762098051E+00 0.31757878184260E+03 0.42328936322563E+03 0.41959979539594E+03 0.41842814730193E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16368442236017E+00 0.00000000000000E+00 -.16130681213915E+02
 0.33151289385992E-02 0.99275552704120E+00 0.24131791396869E+04 0.90494217738258E+03 0.80583787066320E+01
 0.30218920149870E+01 0.33517012548699E+03 0.30415006095686E+03 0.33056034847778E+03 0.35047039256705E+03
 0.30415000634656E+03 0.30415001213355E+03 0.32784554631643E+03 0.35044709874016E+03 0.30415000510213E+03
 0.30415001208975E+03 0.33056034847778E+03 0.35047039256705E+03 0.30415000634656E+03 0.30415001213355E+03
 0.32784554631643E+03 0.35044709874016E+03 0.30415000510213E+03 0.30415001208975E+03 0.38493774815999E+03
 0.32334263682837E+03 0.14427862907162E+04 0.13643837512331E+04 0.45641188984952E+03 0.80981740209995E+03
 0.35112345280118E+03 0.85097784734188E+03 0.81357147712024E+03 0.79320321469158E+03 0.13259389082319E+04
 0.76781965349714E+03 0.81322614370295E+03 0.72486102920033E+03 0.13257958920066E+04 0.85097784734188E+03
 0.81357147712024E+03 0.79320321469158E+03 0.13259389082319E+04 0.76781965349713E+03 0.81322614370295E+03
 0.72486102920032E+03 0.13257958920066E+04 0.13056097226073E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45565096401592E+03 0.10919888942309E+01
 0.10919888942309E+01 0.18186637535086E+01 0.16201436973899E-02 0.32324510099318E+03 0.33077450146483E+03
 0.33076636899102E+03 0.33076617976935E+03 0.23000000000000E+00 0.00000000000000E+00 0.18317545685648E+00
 0.00000000000000E+00 -.89590856852071E+01 0.10716128669775E+00 0.89201472579914E+00 0.74653825523430E+02
 0.27995184571286E+02 0.89684618074359E+01 0.33631731777885E+01 0.32334279930822E+03 0.38493921533910E+03
 0.30704102231086E+03 0.31124886946643E+03 0.30415000007025E+03 0.30415000131332E+03 0.30703525310400E+03
 0.31124578605370E+03 0.30415000006947E+03 0.30415000131380E+03 0.30704102231086E+03 0.31124886946643E+03
 0.30415000007025E+03 0.30415000131332E+03 0.30703525310400E+03 0.31124578605370E+03 0.30415000006947E+03
 0.30415000131380E+03 0.31021349136069E+03 0.30415000891936E+03 0.24812497048789E+02 0.24735572638191E+02
 0.12013847709348E+03 0.28231221601807E+03 0.16157304653912E+03 0.12124845386336E+03 0.11020372909960E+03
 0.12124845386336E+03 0.22428101204753E+03 0.12125039504945E+03 0.11002028621191E+03 0.12125039504945E+03
 0.22411921755093E+03 0.12124845386336E+03 0.11020372909960E+03 0.12124845386336E+03 0.22428101204753E+03
 0.12125039504945E+03 0.11002028621191E+03 0.12125039504945E+03 0.22411921755093E+03 0.14092299326424E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35006316418433E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14695592967238E+00 0.00000000000000E+00 0.00000000000000E+00 0.14695592967238E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15389231103890E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15389231103890E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573101302220E+00 0.15508823131354E+00 0.33077450146483E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    664.24213209
 0.10463551936479E+00 0.31778569380561E+03 0.42364554981242E+03 0.41995331614137E+03 0.41877990203409E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16307974404161E+00 0.00000000000000E+00 -.16152028395919E+02
 0.33174129637807E-02 0.10009770516956E+01 0.24115176757743E+04 0.90431912841537E+03 0.79921912160212E+01
 0.29970717060079E+01 0.33558549884819E+03 0.30415007416099E+03 0.33091880301612E+03 0.35101166131515E+03
 0.30415000792762E+03 0.30415001517457E+03 0.32818377481082E+03 0.35098861164895E+03 0.30415000638504E+03
 0.30415001512079E+03 0.33091880301612E+03 0.35101166131515E+03 0.30415000792762E+03 0.30415001517457E+03
 0.32818377481082E+03 0.35098861164895E+03 0.30415000638504E+03 0.30415001512079E+03 0.38558552394820E+03
 0.32398443459752E+03 0.14462489353843E+04 0.13666502768259E+04 0.45541280107053E+03 0.80551913958241E+03
 0.34782927450653E+03 0.85342394840399E+03 0.81535269117672E+03 0.79480275457071E+03 0.13259035705870E+04
 0.77046242626695E+03 0.81501598597366E+03 0.72681384877770E+03 0.13257668282727E+04 0.85342394840399E+03
 0.81535269117672E+03 0.79480275457071E+03 0.13259035705870E+04 0.77046242626694E+03 0.81501598597366E+03
 0.72681384877770E+03 0.13257668282727E+04 0.13051476024817E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45600350428867E+03 0.10919890344137E+01
 0.10919890344137E+01 0.18577472574616E+01 0.12893945822738E-02 0.32390448276009E+03 0.33090864675867E+03
 0.33090262600459E+03 0.33090249592515E+03 0.23000000000000E+00 0.00000000000000E+00 0.18217556621081E+00
 0.00000000000000E+00 -.89595630317448E+01 0.13464976510407E+00 0.91106621289499E+00 0.59413397370704E+02
 0.22280024014014E+02 0.87809205157321E+01 0.32928451933996E+01 0.32398467871271E+03 0.38558670955500E+03
 0.30713333032651E+03 0.31136295716611E+03 0.30415000009173E+03 0.30415000164910E+03 0.30712772807995E+03
 0.31135975463708E+03 0.30415000009070E+03 0.30415000164970E+03 0.30713333032651E+03 0.31136295716611E+03
 0.30415000009173E+03 0.30415000164910E+03 0.30712772807995E+03 0.31135975463708E+03 0.30415000009070E+03
 0.30415000164970E+03 0.31031163567949E+03 0.30415001093220E+03 0.22211456572312E+02 0.22155782038162E+02
 0.12145766169358E+03 0.28397361811388E+03 0.16190866811183E+03 0.12372187172487E+03 0.11139078430155E+03
 0.12372187172487E+03 0.22559447304555E+03 0.12372805578147E+03 0.11120547233454E+03 0.12372805578147E+03
 0.22543164762915E+03 0.12372187172487E+03 0.11139078430155E+03 0.12372187172487E+03 0.22559447304555E+03
 0.12372805578147E+03 0.11120547233454E+03 0.12372805578147E+03 0.22543164762915E+03 0.14110360553773E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35024044054781E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14697472658804E+00 0.00000000000000E+00 0.00000000000000E+00 0.14697472658804E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15382649306228E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15382649306228E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573076979440E+00 0.15502510145060E+00 0.33090864675867E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    670.19558895
 0.10459185276407E+00 0.31788531898837E+03 0.42380902236678E+03 0.42011610357079E+03 0.41894211248073E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16280969871547E+00 0.00000000000000E+00 -.16146964769982E+02
 0.33187978087138E-02 0.10046339953771E+01 0.24105114144029E+04 0.90394178040108E+03 0.79630990358802E+01
 0.29861621384551E+01 0.33577568056797E+03 0.30415008074491E+03 0.33108323016627E+03 0.35125696343146E+03
 0.30415000872667E+03 0.30415001671208E+03 0.32833943956066E+03 0.35123402891013E+03 0.30415000703406E+03
 0.30415001665330E+03 0.33108323016627E+03 0.35125696343146E+03 0.30415000872667E+03 0.30415001671208E+03
 0.32833943956066E+03 0.35123402891013E+03 0.30415000703406E+03 0.30415001665330E+03 0.38586827692303E+03
 0.32427082127452E+03 0.14478167772634E+04 0.13676991180038E+04 0.45509287723095E+03 0.80384660591386E+03
 0.34647826429675E+03 0.85453227022524E+03 0.81616934281914E+03 0.79554769923204E+03 0.13259143327876E+04
 0.77165789413174E+03 0.81583642954446E+03 0.72771370078769E+03 0.13257802791593E+04 0.85453227022524E+03
 0.81616934281914E+03 0.79554769923204E+03 0.13259143327876E+04 0.77165789413174E+03 0.81583642954446E+03
 0.72771370078768E+03 0.13257802791593E+04 0.13050265257596E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45616590134809E+03 0.10919890011618E+01
 0.10919890011618E+01 0.18756076280396E+01 0.11616100234502E-02 0.32419472632214E+03 0.33097103923493E+03
 0.33096579161293E+03 0.33096568201393E+03 0.23000000000000E+00 0.00000000000000E+00 0.18172496786232E+00
 0.00000000000000E+00 -.89442148661672E+01 0.14946210133419E+00 0.91963862592361E+00 0.53525274491575E+02
 0.20071977934340E+02 0.86990691500865E+01 0.32621509312824E+01 0.32427111307944E+03 0.38586931780914E+03
 0.30717813684134E+03 0.31141633247483E+03 0.30415000010284E+03 0.30415000181927E+03 0.30717261203150E+03
 0.31141307559181E+03 0.30415000010169E+03 0.30415000181993E+03 0.30717813684134E+03 0.31141633247483E+03
 0.30415000010284E+03 0.30415000181927E+03 0.30717261203150E+03 0.31141307559181E+03 0.30415000010169E+03
 0.30415000181993E+03 0.31035759572759E+03 0.30415001193947E+03 0.21054311691500E+02 0.21001938095318E+02
 0.12204291317716E+03 0.28471233669884E+03 0.16205920895579E+03 0.12483107669611E+03 0.11191607093370E+03
 0.12483107669611E+03 0.22617461846488E+03 0.12483892832078E+03 0.11173000577168E+03 0.12483892832078E+03
 0.22601142200208E+03 0.12483107669611E+03 0.11191607093370E+03 0.12483107669611E+03 0.22617461846488E+03
 0.12483892832078E+03 0.11173000577168E+03 0.12483892832078E+03 0.22601142200208E+03 0.14118069947503E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35032040982246E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14683657622395E+00 0.00000000000000E+00 0.00000000000000E+00 0.14683657622395E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15376268555820E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15376268555820E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573103614343E+00 0.15499618391215E+00 0.33097103923493E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    680.58121749
 0.10452383913328E+00 0.31805272472548E+03 0.42409261034104E+03 0.42039804502578E+03 0.41922283242177E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16234772663150E+00 0.00000000000000E+00 -.16160828437586E+02
 0.33209570746853E-02 0.10108739053391E+01 0.24089441146294E+04 0.90335404298603E+03 0.79139445164692E+01
 0.29677291936760E+01 0.33610407440906E+03 0.30415009352053E+03 0.33136700987694E+03 0.35168271176616E+03
 0.30415001030316E+03 0.30415001974685E+03 0.32860771436239E+03 0.35165996836852E+03 0.30415000831616E+03
 0.30415001967835E+03 0.33136700987694E+03 0.35168271176616E+03 0.30415001030316E+03 0.30415001974685E+03
 0.32860771436239E+03 0.35165996836852E+03 0.30415000831616E+03 0.30415001967835E+03 0.38637058960078E+03
 0.32477548939175E+03 0.14505237145582E+04 0.13694801167529E+04 0.45436662517769E+03 0.80064825884725E+03
 0.34400980054367E+03 0.85644902865303E+03 0.81756141743867E+03 0.79681110736044E+03 0.13259080401140E+04
 0.77372820266664E+03 0.81723485892844E+03 0.72925118927307E+03 0.13257785045564E+04 0.85644902865303E+03
 0.81756141743867E+03 0.79681110736044E+03 0.13259080401140E+04 0.77372820266664E+03 0.81723485892844E+03
 0.72925118927307E+03 0.13257785045564E+04 0.13047200505451E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45644710565258E+03 0.10919890922018E+01
 0.10919890922018E+01 0.19067645136634E+01 0.96825789649113E-03 0.32471139598886E+03 0.33108174882863E+03
 0.33107763673234E+03 0.33107755611225E+03 0.23000000000000E+00 0.00000000000000E+00 0.18094849736569E+00
 0.00000000000000E+00 -.89414116646263E+01 0.17930829284693E+00 0.93438985596079E+00 0.44615895188013E+02
 0.16730960695505E+02 0.85617367835976E+01 0.32106512938491E+01 0.32477587249917E+03 0.38637138081794E+03
 0.30725395529513E+03 0.31150787718415E+03 0.30415000012542E+03 0.30415000215613E+03 0.30724855552043E+03
 0.31150452835467E+03 0.30415000012400E+03 0.30415000215692E+03 0.30725395529513E+03 0.31150787718415E+03
 0.30415000012542E+03 0.30415000215613E+03 0.30724855552043E+03 0.31150452835467E+03 0.30415000012400E+03
 0.30415000215692E+03 0.31043638262481E+03 0.30415001390260E+03 0.18974740921204E+02 0.18932389543190E+02
 0.12305686772641E+03 0.28602749926941E+03 0.16235534720437E+03 0.12677000482356E+03 0.11282781782514E+03
 0.12677000482356E+03 0.22720982155037E+03 0.12678043829585E+03 0.11264050827024E+03 0.12678043829585E+03
 0.22704602876668E+03 0.12677000482356E+03 0.11282781782514E+03 0.12677000482356E+03 0.22720982155037E+03
 0.12678043829585E+03 0.11264050827024E+03 0.12678043829585E+03 0.22704602876668E+03 0.14131422781263E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35046365431374E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14682063602877E+00 0.00000000000000E+00 0.00000000000000E+00 0.14682063602877E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15370676892068E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15370676892068E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573091285655E+00 0.15494422833503E+00 0.33108174882863E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    690.20880447
 0.10445908098163E+00 0.31820691881241E+03 0.42435461253730E+03 0.42065858235902E+03 0.41948225128946E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16192963831671E+00 0.00000000000000E+00 -.16167535745101E+02
 0.33230156082994E-02 0.10165011363538E+01 0.24074518277975E+04 0.90279443542405E+03 0.78701338482475E+01
 0.29513001930928E+01 0.33640645792133E+03 0.30415010685186E+03 0.33162843744302E+03 0.35207422695790E+03
 0.30415001197891E+03 0.30415002297417E+03 0.32885500407573E+03 0.35205165694614E+03 0.30415000968090E+03
 0.30415002289548E+03 0.33162843744302E+03 0.35207422695790E+03 0.30415001197891E+03 0.30415002297417E+03
 0.32885500407573E+03 0.35205165694614E+03 0.30415000968090E+03 0.30415002289548E+03 0.38683149045701E+03
 0.32524125595107E+03 0.14530018960993E+04 0.13711041930080E+04 0.45369239258972E+03 0.79772861309829E+03
 0.34176775854562E+03 0.85820674289988E+03 0.81883173895003E+03 0.79796528021434E+03 0.13259037367357E+04
 0.77562701103360E+03 0.81851081735745E+03 0.73065690823640E+03 0.13257781729525E+04 0.85820674289988E+03
 0.81883173895003E+03 0.79796528021435E+03 0.13259037367357E+04 0.77562701103360E+03 0.81851081735746E+03
 0.73065690823640E+03 0.13257781729525E+04 0.13044409586502E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45670696204754E+03 0.10919891362474E+01
 0.10919891362474E+01 0.19356472746237E+01 0.81788258866533E-03 0.32518666824090E+03 0.33118621907005E+03
 0.33118294778461E+03 0.33118288746435E+03 0.23000000000000E+00 0.00000000000000E+00 0.18023970645294E+00
 0.00000000000000E+00 -.89320728669181E+01 0.21227578207591E+00 0.94783278945002E+00 0.37686823818363E+02
 0.14132558931886E+02 0.84403072873666E+01 0.31651152327625E+01 0.32524170466287E+03 0.38683209353309E+03
 0.30732473117467E+03 0.31159267771950E+03 0.30415000015021E+03 0.30415000251556E+03 0.30731944169480E+03
 0.31158924553062E+03 0.30415000014851E+03 0.30415000251648E+03 0.30732473117468E+03 0.31159267771950E+03
 0.30415000015021E+03 0.30415000251556E+03 0.30731944169480E+03 0.31158924553062E+03 0.30415000014851E+03
 0.30415000251648E+03 0.31050937937890E+03 0.30415001596108E+03 0.17034079393752E+02 0.16999347983536E+02
 0.12398138422261E+03 0.28724596563622E+03 0.16264467449249E+03 0.12855849037797E+03 0.11365890455082E+03
 0.12855849037797E+03 0.22816679894498E+03 0.12857097253906E+03 0.11347052671289E+03 0.12857097253906E+03
 0.22800252577452E+03 0.12855849037797E+03 0.11365890455082E+03 0.12855849037797E+03 0.22816679894498E+03
 0.12857097253906E+03 0.11347052671289E+03 0.12857097253906E+03 0.22800252577452E+03 0.14143292015674E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35059577563130E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14674587734932E+00 0.00000000000000E+00 0.00000000000000E+00 0.14674587734932E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15362240864824E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15362240864824E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573095957665E+00 0.15489541626552E+00 0.33118621907005E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    700.00763877
 0.10439537205196E+00 0.31836141643402E+03 0.42462010156013E+03 0.42092246323764E+03 0.41974493491960E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16151388703445E+00 0.00000000000000E+00 -.16179435408400E+02
 0.33250432745517E-02 0.10220781021260E+01 0.24059837239498E+04 0.90224389648117E+03 0.78271904890237E+01
 0.29351964333839E+01 0.33671182851168E+03 0.30415012212946E+03 0.33189250899292E+03 0.35246971916134E+03
 0.30415001393557E+03 0.30415002674400E+03 0.32910482032565E+03 0.35244732064174E+03 0.30415001127670E+03
 0.30415002665357E+03 0.33189250899292E+03 0.35246971916134E+03 0.30415001393557E+03 0.30415002674400E+03
 0.32910482032565E+03 0.35244732064174E+03 0.30415001127670E+03 0.30415002665357E+03 0.38729877214949E+03
 0.32571475052839E+03 0.14554929724028E+04 0.13727234977833E+04 0.45296892862296E+03 0.79473084402303E+03
 0.33949707075695E+03 0.85997682439423E+03 0.82010160765971E+03 0.79911745736291E+03 0.13258941896151E+04
 0.77754001125303E+03 0.81978617975460E+03 0.73206396382038E+03 0.13257724721299E+04 0.85997682439423E+03
 0.82010160765971E+03 0.79911745736291E+03 0.13258941896151E+04 0.77754001125303E+03 0.81978617975460E+03
 0.73206396382037E+03 0.13257724721299E+04 0.13041387436027E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45697013442884E+03 0.10919892143902E+01
 0.10919892143902E+01 0.19650437775227E+01 0.68874138889141E-03 0.32566866231721E+03 0.33129439308626E+03
 0.33129180997051E+03 0.33129176536958E+03 0.23000000000000E+00 0.00000000000000E+00 0.17952923425707E+00
 0.00000000000000E+00 -.89282783987743E+01 0.25207815955431E+00 0.96128509832201E+00 0.31736188546221E+02
 0.11901070704833E+02 0.83221928790580E+01 0.31208223296468E+01 0.32571526252748E+03 0.38729919715714E+03
 0.30739660861325E+03 0.31167857017731E+03 0.30415000018013E+03 0.30415000293685E+03 0.30739142522529E+03
 0.31167505510982E+03 0.30415000017808E+03 0.30415000293792E+03 0.30739660861325E+03 0.31167857017730E+03
 0.30415000018013E+03 0.30415000293685E+03 0.30739142522529E+03 0.31167505510982E+03 0.30415000017808E+03
 0.30415000293792E+03 0.31058331637862E+03 0.30415001833155E+03 0.15036929081441E+02 0.15008733185639E+02
 0.12491058499845E+03 0.28849606670672E+03 0.16296092878328E+03 0.13037433196530E+03 0.11449437570691E+03
 0.13037433196530E+03 0.22914802448868E+03 0.13038860429344E+03 0.11430498032567E+03 0.13038860429344E+03
 0.22898331940730E+03 0.13037433196530E+03 0.11449437570691E+03 0.13037433196530E+03 0.22914802448868E+03
 0.13038860429344E+03 0.11430498032567E+03 0.13038860429344E+03 0.22898331940730E+03 0.14155219866650E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35073265646302E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14672072936143E+00 0.00000000000000E+00 0.00000000000000E+00 0.14672072936143E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15356318633423E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15356318633423E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573086459079E+00 0.15484474479626E+00 0.33129439308626E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    713.18691605
 0.10432364085399E+00 0.31856695594258E+03 0.42497559887674E+03 0.42127528652700E+03 0.42009597686240E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16096994688538E+00 0.00000000000000E+00 -.16194952218269E+02
 0.33273291768152E-02 0.10293444076568E+01 0.24043307935217E+04 0.90162404757065E+03 0.77719371091851E+01
 0.29144764159444E+01 0.33711949205755E+03 0.30415014560117E+03 0.33224524401081E+03 0.35299682735827E+03
 0.30415001700613E+03 0.30415003266225E+03 0.32943876233784E+03 0.35297465426662E+03 0.30415001378510E+03
 0.30415003255370E+03 0.33224524401081E+03 0.35299682735827E+03 0.30415001700613E+03 0.30415003266225E+03
 0.32943876233784E+03 0.35297465426663E+03 0.30415001378510E+03 0.30415003255370E+03 0.38791915842640E+03
 0.32634757753121E+03 0.14587879832814E+04 0.13748475786033E+04 0.45200463125992E+03 0.79078436669478E+03
 0.33651971227856E+03 0.86232526926414E+03 0.82177852256935E+03 0.80063536689783E+03 0.13258819375926E+04
 0.78007823234584E+03 0.82147012025200E+03 0.73391998792634E+03 0.13257650821214E+04 0.86232526926414E+03
 0.82177852256936E+03 0.80063536689783E+03 0.13258819375926E+04 0.78007823234585E+03 0.82147012025199E+03
 0.73391998792635E+03 0.13257650821214E+04 0.13037378331581E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45732196668163E+03 0.10919893162862E+01
 0.10919893162862E+01 0.20045816093471E+01 0.54653940403737E-03 0.32631060918536E+03 0.33144332747170E+03
 0.33144145731650E+03 0.33144142791069E+03 0.23000000000000E+00 0.00000000000000E+00 0.17859094438048E+00
 0.00000000000000E+00 -.89231855338515E+01 0.31766539495026E+00 0.97901373928202E+00 0.25183731458230E+02
 0.94438992968362E+01 0.81714889985783E+01 0.30643083744669E+01 0.32634817637550E+03 0.38791935198922E+03
 0.30749420863577E+03 0.31179414612998E+03 0.30415000022890E+03 0.30415000360085E+03 0.30748916032162E+03
 0.31179052223689E+03 0.30415000022629E+03 0.30415000360217E+03 0.30749420863577E+03 0.31179414612998E+03
 0.30415000022890E+03 0.30415000360085E+03 0.30748916032162E+03 0.31179052223689E+03 0.30415000022629E+03
 0.30415000360217E+03 0.31068282633503E+03 0.30415002199330E+03 0.12344219154675E+02 0.12322694586353E+02
 0.12614348084508E+03 0.29019216931306E+03 0.16341797106375E+03 0.13280427973137E+03 0.11560292656249E+03
 0.13280427973137E+03 0.23047737981956E+03 0.13282054804127E+03 0.11541228294481E+03 0.13282054804127E+03
 0.23031219699388E+03 0.13280427973137E+03 0.11560292656249E+03 0.13280427973137E+03 0.23047737981956E+03
 0.13282054804127E+03 0.11541228294481E+03 0.13282054804127E+03 0.23031219699388E+03 0.14171629083668E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35091989015650E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14668361597942E+00 0.00000000000000E+00 0.00000000000000E+00 0.14668361597942E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15350057087906E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15350057087906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573073053279E+00 0.15477503056264E+00 0.33144332747170E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    723.07137400
 0.10426994983634E+00 0.31872143877729E+03 0.42524125329413E+03 0.42153898138872E+03 0.42035836567473E+03
 0.22999999247691E+00 0.00000000000000E+00 0.16057320982798E+00 0.00000000000000E+00 -.16203462098312E+02
 0.33290422403148E-02 0.10346212215941E+01 0.24030935694116E+04 0.90116008852935E+03 0.77322983842084E+01
 0.28996118940781E+01 0.33742299998547E+03 0.30415016557615E+03 0.33250800849816E+03 0.35338870170236E+03
 0.30415001967231E+03 0.30415003780262E+03 0.32968768529009E+03 0.35336669362234E+03 0.30415001596654E+03
 0.30415003767860E+03 0.33250800849816E+03 0.35338870170236E+03 0.30415001967231E+03 0.30415003780262E+03
 0.32968768529009E+03 0.35336669362235E+03 0.30415001596654E+03 0.30415003767860E+03 0.38837936408322E+03
 0.32681949076877E+03 0.14612272319155E+04 0.13764220373589E+04 0.45128160221437E+03 0.78787390154717E+03
 0.33433589132173E+03 0.86406690261927E+03 0.82301670474235E+03 0.80176477258605E+03 0.13258770305325E+04
 0.78196079907963E+03 0.82271331164936E+03 0.73529962271112E+03 0.13257636016851E+04 0.86406690261926E+03
 0.82301670474236E+03 0.80176477258605E+03 0.13258770305325E+04 0.78196079907964E+03 0.82271331164935E+03
 0.73529962271114E+03 0.13257636016851E+04 0.13034467097148E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45758492724811E+03 0.10919893721690E+01
 0.10919893721690E+01 0.20342349832154E+01 0.45950753597346E-03 0.32678806587222E+03 0.33155708275964E+03
 0.33155562182684E+03 0.33155560051319E+03 0.22999992039849E+00 0.00000000000000E+00 0.17790032084731E+00
 0.00000000000000E+00 -.89158255694064E+01 0.37783199929853E+00 0.99203617605991E+00 0.21173431617366E+02
 0.79400368565122E+01 0.80642220445768E+01 0.30240832667163E+01 0.32682015824861E+03 0.38837938644016E+03
 0.30756795164986E+03 0.31188080753778E+03 0.30415000027279E+03 0.30415000417979E+03 0.30756299921159E+03
 0.31187710399397E+03 0.30415000026968E+03 0.30415000418131E+03 0.30756795164986E+03 0.31188080753778E+03
 0.30415000027279E+03 0.30415000417979E+03 0.30756299921159E+03 0.31187710399397E+03 0.30415000026968E+03
 0.30415000418131E+03 0.31075746016636E+03 0.30415002512548E+03 0.10316042286426E+02 0.10298372967564E+02
 0.12705398457910E+03 0.29146707880459E+03 0.16377782430259E+03 0.13461561778943E+03 0.11642150979154E+03
 0.13461561778943E+03 0.23147458622859E+03 0.13463311419545E+03 0.11623001815954E+03 0.13463311419545E+03
 0.23130911996937E+03 0.13461561778943E+03 0.11642150979154E+03 0.13461561778943E+03 0.23147458622859E+03
 0.13463311419545E+03 0.11623001815954E+03 0.13463311419545E+03 0.23130911996937E+03 0.14183733358342E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35106043801874E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14662558745282E+00 0.00000000000000E+00 0.00000000000000E+00 0.14662558745282E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15343018142490E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15343018142490E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573071275013E+00 0.15472192052216E+00 0.33155708275964E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    732.95583196
 0.10420737774300E+00 0.31887634908199E+03 0.42550609019600E+03 0.42180222162570E+03 0.42062042595590E+03
 0.22999997631431E+00 0.00000000000000E+00 0.16018577678109E+00 0.00000000000000E+00 -.16209439659891E+02
 0.33310409329600E-02 0.10397547504770E+01 0.24016516641515E+04 0.90061937405681E+03 0.76941220959363E+01
 0.28852957859761E+01 0.33772464742618E+03 0.30415018778274E+03 0.33276928782863E+03 0.35377771776364E+03
 0.30415002268702E+03 0.30415004361617E+03 0.32993532679004E+03 0.35375587123634E+03 0.30415001843643E+03
 0.30415004347489E+03 0.33276928782863E+03 0.35377771776364E+03 0.30415002268702E+03 0.30415004361617E+03
 0.32993532679004E+03 0.35375587123634E+03 0.30415001843643E+03 0.30415004347489E+03 0.38883561222933E+03
 0.32728912450577E+03 0.14636449549721E+04 0.13779871267622E+04 0.45055632521655E+03 0.78499921591046E+03
 0.33219010906783E+03 0.86579426949433E+03 0.82423934089751E+03 0.80288863618563E+03 0.13258758496270E+04
 0.78382818770315E+03 0.82394074902783E+03 0.73667140261635E+03 0.13257656732989E+04 0.86579426949433E+03
 0.82423934089751E+03 0.80288863618563E+03 0.13258758496270E+04 0.78382818770316E+03 0.82394074902782E+03
 0.73667140261637E+03 0.13257656732989E+04 0.13031648570309E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45784746235412E+03 0.10919894114225E+01
 0.10919894114225E+01 0.20638883570836E+01 0.38633956013179E-03 0.32726231535196E+03 0.33167203622434E+03
 0.33167090045792E+03 0.33167088515795E+03 0.22999915466975E+00 0.00000000000000E+00 0.17722102325433E+00
 0.00000000000000E+00 -.89055681080811E+01 0.44938873534521E+00 0.10048237491576E+01 0.17801959352307E+02
 0.66757347571151E+01 0.79615952615642E+01 0.29855982230866E+01 0.32728985925240E+03 0.38883547316515E+03
 0.30764209829921E+03 0.31196740733586E+03 0.30415000032396E+03 0.30415000483668E+03 0.30763723732001E+03
 0.31196362577261E+03 0.30415000032026E+03 0.30415000483844E+03 0.30764209829921E+03 0.31196740733586E+03
 0.30415000032396E+03 0.30415000483668E+03 0.30763723732001E+03 0.31196362577261E+03 0.30415000032026E+03
 0.30415000483844E+03 0.31083205881475E+03 0.30415002862247E+03 0.82781490288255E+01 0.82635593812301E+01
 0.12795063721223E+03 0.29273796573871E+03 0.16414757534042E+03 0.13641664548105E+03 0.11722727437663E+03
 0.13641664548105E+03 0.23246658157168E+03 0.13643517207811E+03 0.11703500067634E+03 0.13643517207811E+03
 0.23230088669999E+03 0.13641664548105E+03 0.11722727437663E+03 0.13641664548105E+03 0.23246658157168E+03
 0.13643517207811E+03 0.11703500067634E+03 0.13643517207811E+03 0.23230088669999E+03 0.14195304123144E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35120067784223E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14654285939326E+00 0.00000000000000E+00 0.00000000000000E+00 0.14654285939326E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15334036298331E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15334036298331E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573076357553E+00 0.15466836540666E+00 0.33167203622434E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    740.09490796
 0.10415336891535E+00 0.31898882915591E+03 0.42569699267778E+03 0.42199232110392E+03 0.42080979307728E+03
 0.22999998078079E+00 0.00000000000000E+00 0.15991154839103E+00 0.00000000000000E+00 -.16210385043712E+02
 0.33327680624564E-02 0.10433760362979E+01 0.24004070640618E+04 0.90015264902318E+03 0.76674178068966E+01
 0.28752816775862E+01 0.33794153484088E+03 0.30415020529343E+03 0.33295725043537E+03 0.35405687002462E+03
 0.30415002509784E+03 0.30415004826581E+03 0.33011360611868E+03 0.35403513847878E+03 0.30415002041375E+03
 0.30415004811089E+03 0.33295725043537E+03 0.35405687002462E+03 0.30415002509784E+03 0.30415004826581E+03
 0.33011360611868E+03 0.35403513847878E+03 0.30415002041375E+03 0.30415004811089E+03 0.38916148361999E+03
 0.32762612872802E+03 0.14653803843134E+04 0.13791153288579E+04 0.45004835970307E+03 0.78297675012577E+03
 0.33067814862418E+03 0.86703423148691E+03 0.82511561396678E+03 0.80369866826146E+03 0.13258814770662E+04
 0.78516851104471E+03 0.82482037140972E+03 0.73765866912818E+03 0.13257735486912E+04 0.86703423148691E+03
 0.82511561396678E+03 0.80369866826146E+03 0.13258814770662E+04 0.78516851104472E+03 0.82482037140971E+03
 0.73765866912819E+03 0.13257735486912E+04 0.13029782988573E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45803707968833E+03 0.10919894176306E+01
 0.10919894176306E+01 0.20853055850965E+01 0.34086113616016E-03 0.32760240918354E+03 0.33175559491617E+03
 0.33175465114310E+03 0.33175463918115E+03 0.22999906123749E+00 0.00000000000000E+00 0.17673748060187E+00
 0.00000000000000E+00 -.88947748587708E+01 0.50934713446574E+00 0.10139136592322E+01 0.15706380695341E+02
 0.58898927607530E+01 0.78902181927979E+01 0.29588318222992E+01 0.32762691081722E+03 0.38916123434982E+03
 0.30769616555376E+03 0.31203006077154E+03 0.30415000036593E+03 0.30415000536350E+03 0.30769136819692E+03
 0.31202622381072E+03 0.30415000036175E+03 0.30415000536545E+03 0.30769616555376E+03 0.31203006077154E+03
 0.30415000036593E+03 0.30415000536350E+03 0.30769136819692E+03 0.31202622381072E+03 0.30415000036175E+03
 0.30415000536545E+03 0.31088604972154E+03 0.30415003138965E+03 0.68052902786035E+01 0.67926905742685E+01
 0.12858881285681E+03 0.29365010753848E+03 0.16441835061739E+03 0.13771168611767E+03 0.11780012605617E+03
 0.13771168611767E+03 0.23317690998184E+03 0.13773084789291E+03 0.11760731508525E+03 0.13773084789291E+03
 0.23301107112962E+03 0.13771168611767E+03 0.11780012605617E+03 0.13771168611767E+03 0.23317690998184E+03
 0.13773084789291E+03 0.11760731508525E+03 0.13773084789291E+03 0.23301107112962E+03 0.14203294084717E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35130221199414E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14644979475471E+00 0.00000000000000E+00 0.00000000000000E+00 0.14644979475471E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15327465086300E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15327465086300E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573088192835E+00 0.15462955141849E+00 0.33175559491617E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    751.17820436
 0.10406747502186E+00 0.31916348943664E+03 0.42599320515184E+03 0.42228737222791E+03 0.42110373583503E+03
 0.22999998846853E+00 0.00000000000000E+00 0.15949480395337E+00 0.00000000000000E+00 -.16225108177774E+02
 0.33355185332721E-02 0.10488579684740E+01 0.23984276867897E+04 0.89941038254615E+03 0.76273434921218E+01
 0.28602538095457E+01 0.33827665500233E+03 0.30415023488442E+03 0.33324780240127E+03 0.35448770808224E+03
 0.30415002922752E+03 0.30415005623134E+03 0.33038931489993E+03 0.35446615188476E+03 0.30415002380448E+03
 0.30415005605333E+03 0.33324780240127E+03 0.35448770808224E+03 0.30415002922752E+03 0.30415005623134E+03
 0.33038931489992E+03 0.35446615188476E+03 0.30415002380448E+03 0.30415005605333E+03 0.38966429511036E+03
 0.32814695963631E+03 0.14680590728856E+04 0.13808595698840E+04 0.44925858089407E+03 0.77987096669778E+03
 0.32836609289924E+03 0.86894920675518E+03 0.82646888754176E+03 0.80495219701116E+03 0.13259050484769E+04
 0.78723832596298E+03 0.82617864046063E+03 0.73918508488914E+03 0.13258004370668E+04 0.86894920675518E+03
 0.82646888754176E+03 0.80495219701115E+03 0.13259050484769E+04 0.78723832596298E+03 0.82617864046062E+03
 0.73918508488915E+03 0.13258004370668E+04 0.13027037351036E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45833138775350E+03 0.10919895143147E+01
 0.10919895143147E+01 0.21185554742850E+01 0.28068048030860E-03 0.32812648633964E+03 0.33188676630645E+03
 0.33188606268165E+03 0.33188605461975E+03 0.22999906685264E+00 0.00000000000000E+00 0.17599838412166E+00
 0.00000000000000E+00 -.88919851371523E+01 0.61855615129956E+00 0.10277848189180E+01 0.12933344827616E+02
 0.48500043103559E+01 0.77837304586989E+01 0.29188989220121E+01 0.32814782344606E+03 0.38966386837536E+03
 0.30778049062285E+03 0.31212728212586E+03 0.30415000043960E+03 0.30415000626844E+03 0.30777578822612E+03
 0.31212336075534E+03 0.30415000043458E+03 0.30415000627071E+03 0.30778049062285E+03 0.31212728212586E+03
 0.30415000043960E+03 0.30415000626844E+03 0.30777578822612E+03 0.31212336075534E+03 0.30415000043458E+03
 0.30415000627071E+03 0.31096985226234E+03 0.30415003608162E+03 0.45122266844684E+01 0.45016326715399E+01
 0.12956653371594E+03 0.29506880320870E+03 0.16485443682418E+03 0.13970282960223E+03 0.11867814416218E+03
 0.13970282960223E+03 0.23428099799233E+03 0.13972281543410E+03 0.11848460334169E+03 0.13972281543410E+03
 0.23411502975624E+03 0.13970282960223E+03 0.11867814416218E+03 0.13970282960223E+03 0.23428099799233E+03
 0.13972281543410E+03 0.11848460334169E+03 0.13972281543410E+03 0.23411502975624E+03 0.14215598274697E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35146269094314E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14643649626601E+00 0.00000000000000E+00 0.00000000000000E+00 0.14643649626601E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15320671573040E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15320671573040E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573072267306E+00 0.15456827262614E+00 0.33188676630645E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    760.57207983
 0.10400519034675E+00 0.31930790777369E+03 0.42624295072562E+03 0.42253568422663E+03 0.42135093221531E+03
 0.22999996509717E+00 0.00000000000000E+00 0.15915001364410E+00 0.00000000000000E+00 -.16233996792945E+02
 0.33375158053046E-02 0.10533761747059E+01 0.23969923939491E+04 0.89887214773091E+03 0.75946278187218E+01
 0.28479854320207E+01 0.33855875188905E+03 0.30415026274499E+03 0.33349245908895E+03 0.35485027758918E+03
 0.30415003318138E+03 0.30415006385842E+03 0.33062154257320E+03 0.35482886633033E+03 0.30415002705516E+03
 0.30415006365861E+03 0.33349245908895E+03 0.35485027758918E+03 0.30415003318138E+03 0.30415006385842E+03
 0.33062154257320E+03 0.35482886633033E+03 0.30415002705516E+03 0.30415006365861E+03 0.39008794866606E+03
 0.32858673945392E+03 0.14702971918577E+04 0.13822933863437E+04 0.44856707863465E+03 0.77723267672808E+03
 0.32642276270025E+03 0.87055379487235E+03 0.82759490300785E+03 0.80598452138992E+03 0.13259173367389E+04
 0.78897312472170E+03 0.82730870687149E+03 0.74044741329800E+03 0.13258153888097E+04 0.87055379487234E+03
 0.82759490300785E+03 0.80598452138992E+03 0.13259173367389E+04 0.78897312472171E+03 0.82730870687148E+03
 0.74044741329801E+03 0.13258153888097E+04 0.13024525996889E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45857902758046E+03 0.10919895726846E+01
 0.10919895726846E+01 0.21467371006955E+01 0.23806481351940E-03 0.32856869854331E+03 0.33199959761509E+03
 0.33199905309752E+03 0.33199904741261E+03 0.22999872405001E+00 0.00000000000000E+00 0.17538286873161E+00
 0.00000000000000E+00 -.88859968582611E+01 0.72928304909347E+00 0.10393142407717E+01 0.10969677699138E+02
 0.41136291371768E+01 0.76973832226718E+01 0.28865187085019E+01 0.32858765783554E+03 0.39008740153812E+03
 0.30785207427885E+03 0.31220949321952E+03 0.30415000051229E+03 0.30415000713783E+03 0.30784744840246E+03
 0.31220550178257E+03 0.30415000050645E+03 0.30415000714040E+03 0.30785207427885E+03 0.31220949321952E+03
 0.30415000051229E+03 0.30415000713783E+03 0.30784744840246E+03 0.31220550178257E+03 0.30415000050645E+03
 0.30415000714040E+03 0.31104072607130E+03 0.30415004051746E+03 0.25611168485762E+01 0.25519814232450E+01
 0.13038934637469E+03 0.29628135211127E+03 0.16524005900470E+03 0.14138913739583E+03 0.11941694328076E+03
 0.14138913739583E+03 0.23522368861268E+03 0.14140969164126E+03 0.11922281072387E+03 0.14140969164126E+03
 0.23505762785740E+03 0.14138913739583E+03 0.11941694328076E+03 0.14138913739583E+03 0.23522368861268E+03
 0.14140969164126E+03 0.11922281072387E+03 0.14140969164126E+03 0.23505762785739E+03 0.14226271515458E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35159942280150E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14639096957471E+00 0.00000000000000E+00 0.00000000000000E+00 0.14639096957471E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15314174707672E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15314174707672E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573067327696E+00 0.15451569878079E+00 0.33199959761509E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    771.84473040
 0.10392444638849E+00 0.31948145543053E+03 0.42654145217843E+03 0.42283273521430E+03 0.42164674418692E+03
 0.22999996659343E+00 0.00000000000000E+00 0.15874622318063E+00 0.00000000000000E+00 -.16242875569052E+02
 0.33401085955375E-02 0.10586458993534E+01 0.23951317064027E+04 0.89817438990101E+03 0.75568233012438E+01
 0.28338087379664E+01 0.33889517554079E+03 0.30415029974616E+03 0.33378436048586E+03 0.35528222324668E+03
 0.30415003851873E+03 0.30415007415469E+03 0.33089874749636E+03 0.35526098204855E+03 0.30415003144897E+03
 0.30415007392586E+03 0.33378436048586E+03 0.35528222324668E+03 0.30415003851873E+03 0.30415007415469E+03
 0.33089874749636E+03 0.35526098204855E+03 0.30415003144897E+03 0.30415007392586E+03 0.39059221168715E+03
 0.32911166193286E+03 0.14729574100093E+04 0.13840008362725E+04 0.44773102748033E+03 0.77409636813966E+03
 0.32412668552194E+03 0.87246293731458E+03 0.82892778486975E+03 0.80721601630041E+03 0.13259325579095E+04
 0.79103745692206E+03 0.82864624515403E+03 0.74195229321309E+03 0.13258336388922E+04 0.87246293731458E+03
 0.82892778486976E+03 0.80721601630041E+03 0.13259325579095E+04 0.79103745692206E+03 0.82864624515402E+03
 0.74195229321309E+03 0.13258336388922E+04 0.13021558800616E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45887529590031E+03 0.10919896309899E+01
 0.10919896309899E+01 0.21805550523882E+01 0.19538114350270E-03 0.32909578838362E+03 0.33213613745398E+03
 0.33213574143606E+03 0.33213573777793E+03 0.22999845544778E+00 0.00000000000000E+00 0.17465752160047E+00
 0.00000000000000E+00 -.88768268719551E+01 0.88860482881079E+00 0.10528763522327E+01 0.90028770276956E+01
 0.33760788853858E+01 0.75982331477343E+01 0.28493374304004E+01 0.32911264626277E+03 0.39059152296440E+03
 0.30793837473859E+03 0.31230806219146E+03 0.30415000061336E+03 0.30415000831534E+03 0.30793383647295E+03
 0.31230398837001E+03 0.30415000060637E+03 0.30415000831833E+03 0.30793837473859E+03 0.31230806219146E+03
 0.30415000061336E+03 0.30415000831534E+03 0.30793383647295E+03 0.31230398837001E+03 0.30415000060637E+03
 0.30415000831833E+03 0.31112572673472E+03 0.30415004643209E+03 0.21236000327250E+00 0.20447632935532E+00
 0.13136372267572E+03 0.29773335747571E+03 0.16571281618661E+03 0.14340186113844E+03 0.12029135324407E+03
 0.14340186113844E+03 0.23635049119336E+03 0.14342296080660E+03 0.12009656330017E+03 0.14342296080660E+03
 0.23618436067409E+03 0.14340186113844E+03 0.12029135324407E+03 0.14340186113844E+03 0.23635049119336E+03
 0.14342296080660E+03 0.12009656330017E+03 0.14342296080660E+03 0.23618436067408E+03 0.14238666265348E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35176352330520E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14631927148168E+00 0.00000000000000E+00 0.00000000000000E+00 0.14631927148168E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15305132860625E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15305132860625E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573066052838E+00 0.15445217846691E+00 0.33213613745398E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    781.23860587
 0.10385980093499E+00 0.31962491117259E+03 0.42678923437465E+03 0.42307921261629E+03 0.42189215340844E+03
 0.22999997286258E+00 0.00000000000000E+00 0.15841780143869E+00 0.00000000000000E+00 -.16251549186248E+02
 0.33421873404728E-02 0.10629141476819E+01 0.23936420029849E+04 0.89761575111934E+03 0.75264780485301E+01
 0.28224292681988E+01 0.33917382108676E+03 0.30415033378729E+03 0.33402623391293E+03 0.35563962304193E+03
 0.30415004350778E+03 0.30415008377902E+03 0.33112855153310E+03 0.35561852042656E+03 0.30415003556126E+03
 0.30415008352345E+03 0.33402623391293E+03 0.35563962304193E+03 0.30415004350778E+03 0.30415008377902E+03
 0.33112855153310E+03 0.35561852042656E+03 0.30415003556126E+03 0.30415008352345E+03 0.39100909267766E+03
 0.32954671824749E+03 0.14751505141064E+04 0.13854015648502E+04 0.44702941753907E+03 0.77150666426553E+03
 0.32224209963877E+03 0.87403954417328E+03 0.83002399460369E+03 0.80822828929565E+03 0.13259462171537E+04
 0.79274234620306E+03 0.82974617024502E+03 0.74319035083680E+03 0.13258496876219E+04 0.87403954417328E+03
 0.83002399460370E+03 0.80822828929565E+03 0.13259462171537E+04 0.79274234620307E+03 0.82974617024501E+03
 0.74319035083681E+03 0.13258496876219E+04 0.13019089378625E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45912111286918E+03 0.10919896879479E+01
 0.10919896879479E+01 0.22087366787987E+01 0.16571998218984E-03 0.32953217337762E+03 0.33225116015316E+03
 0.33225085975953E+03 0.33225085728125E+03 0.22999813705387E+00 0.00000000000000E+00 0.17406397720306E+00
 0.00000000000000E+00 -.88707354524233E+01 0.10476505044548E+01 0.10639526736434E+01 0.76361343463133E+01
 0.28635503798675E+01 0.75191314408795E+01 0.28196742903298E+01 0.32954775740272E+03 0.39100829018494E+03
 0.30801061617538E+03 0.31239014316195E+03 0.30415000071059E+03 0.30415000941959E+03 0.30800614763793E+03
 0.31238600204309E+03 0.30415000070251E+03 0.30415000942296E+03 0.30801061617538E+03 0.31239014316195E+03
 0.30415000071059E+03 0.30415000941959E+03 0.30800614763793E+03 0.31238600204309E+03 0.30415000070251E+03
 0.30415000942296E+03 0.31119652856603E+03 0.30415005189451E+03 -.17480342076996E+01 -.17551773213202E+01
 0.13216760533440E+03 0.29894751862333E+03 0.16611907526225E+03 0.14507144274982E+03 0.12101256649823E+03
 0.14507144274982E+03 0.23729166621582E+03 0.14509289858768E+03 0.12081726859155E+03 0.14509289858768E+03
 0.23712550863522E+03 0.14507144274982E+03 0.12101256649823E+03 0.14507144274982E+03 0.23729166621582E+03
 0.14509289858768E+03 0.12081726859155E+03 0.14509289858768E+03 0.23712550863522E+03 0.14249066880810E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35190162825530E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14627288052532E+00 0.00000000000000E+00 0.00000000000000E+00 0.14627288052532E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15298600662019E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15298600662019E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573061007435E+00 0.15439866418677E+00 0.33225116015316E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    791.50966066
 0.10379365378434E+00 0.31978218576540E+03 0.42705974825503E+03 0.42334817152841E+03 0.42215991646140E+03
 0.22999998332947E+00 0.00000000000000E+00 0.15806693805258E+00 0.00000000000000E+00 -.16261134782467E+02
 0.33443170386787E-02 0.10674541110800E+01 0.23921177051924E+04 0.89704413944715E+03 0.74944673658205E+01
 0.28104252621827E+01 0.33947704394551E+03 0.30415037434746E+03 0.33428958916616E+03 0.35602782958172E+03
 0.30415004953505E+03 0.30415009540577E+03 0.33137894157922E+03 0.35600687591415E+03 0.30415004053483E+03
 0.30415009511828E+03 0.33428958916616E+03 0.35602782958172E+03 0.30415004953505E+03 0.30415009540577E+03
 0.33137894157922E+03 0.35600687591415E+03 0.30415004053483E+03 0.30415009511828E+03 0.39146018613143E+03
 0.33004537412560E+03 0.14775305300904E+04 0.13869251533220E+04 0.44628305165526E+03 0.76874531340391E+03
 0.32023084649037E+03 0.87575249175731E+03 0.83121699201069E+03 0.80933254781873E+03 0.13259758043003E+04
 0.79459409350343E+03 0.83094307491445E+03 0.74453819551660E+03 0.13258817582233E+04 0.87575249175731E+03
 0.83121699201069E+03 0.80933254781873E+03 0.13259758043003E+04 0.79459409350344E+03 0.83094307491444E+03
 0.74453819551661E+03 0.13258817582233E+04 0.13016617267825E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45938934511415E+03 0.10919897508947E+01
 0.10919897508947E+01 0.22395498431720E+01 0.00000000000000E+00 0.33237652815421E+03 0.33237652815421E+03
 0.33237652815421E+03 0.33237652815421E+03 0.00000000000000E+00 0.00000000000000E+00 0.17343125693470E+00
 0.00000000000000E+00 -.88641918688885E+01 0.10000000000000E-02 0.10758476943820E+01 0.80000000000000E+04
 0.30000000000000E+04 0.74359967881841E+01 0.27884987955690E+01 0.33004636752615E+03 0.39145925356551E+03
 0.31247987014178E+03 0.31247987014178E+03 0.30415001095160E+03 0.30415001075740E+03 0.31247574748612E+03
 0.31247574748612E+03 0.30415001095550E+03 0.30415001076123E+03 0.31247987014178E+03 0.31247987014178E+03
 0.30415001095160E+03 0.30415001075740E+03 0.31247574748612E+03 0.31247574748612E+03 0.30415001095550E+03
 0.30415001076123E+03 0.31127292729178E+03 0.30415005842465E+03 -.35204921651582E+01 -.16577943113090E+01
 0.13264619664603E+03 0.29987906126387E+03 0.16656963363461E+03 0.11703971550060E+03 0.12175638427529E+03
 0.11703971550060E+03 0.23827044058948E+03 0.11706255893984E+03 0.12158112973026E+03 0.11706255893984E+03
 0.23812420379608E+03 0.11703971550060E+03 0.12175638427529E+03 0.11703971550060E+03 0.23827044058949E+03
 0.11706255893984E+03 0.12158112973026E+03 0.11706255893984E+03 0.23812420379607E+03 0.14303668783025E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35205354509825E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14622363552925E+00 0.00000000000000E+00 0.00000000000000E+00 0.14622363552925E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15291365867527E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15291365867527E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573055313102E+00 0.15434037673036E+00 0.33237652815421E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    801.08830171
 0.10372942970754E+00 0.31992643292635E+03 0.42731039521280E+03 0.42359743615690E+03 0.42240806860828E+03
 0.22999998656330E+00 0.00000000000000E+00 0.15774728385189E+00 0.00000000000000E+00 -.16267554098303E+02
 0.33463874315071E-02 0.10715747827126E+01 0.23906377141744E+04 0.89648914281540E+03 0.74656478755018E+01
 0.27996179533132E+01 0.33975785624314E+03 0.30415041603628E+03 0.33453352917134E+03 0.35638752419321E+03
 0.30415005582800E+03 0.30415010754407E+03 0.33161087846823E+03 0.35636670575113E+03 0.30415004573418E+03
 0.30415010722373E+03 0.33453352917134E+03 0.35638752419321E+03 0.30415005582800E+03 0.30415010754407E+03
 0.33161087846823E+03 0.35636670575113E+03 0.30415004573418E+03 0.30415010722373E+03 0.39187989862501E+03
 0.33049197363604E+03 0.14797235669937E+04 0.13883148914758E+04 0.44554588459871E+03 0.76612535794788E+03
 0.31835174392618E+03 0.87733363252832E+03 0.83230553246979E+03 0.81034008124847E+03 0.13259874386872E+04
 0.79630436423544E+03 0.83203510173268E+03 0.74577256495237E+03 0.13258955854877E+04 0.87733363252832E+03
 0.83230553246979E+03 0.81034008124846E+03 0.13259874386872E+04 0.79630436423545E+03 0.83203510173268E+03
 0.74577256495238E+03 0.13258955854877E+04 0.13014034751416E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45963793101458E+03 0.10919897930492E+01
 0.10919897930492E+01 0.22682857663391E+01 0.00000000000000E+00 0.33249672571281E+03 0.33249672571281E+03
 0.33249672571281E+03 0.33249672571281E+03 0.00000000000000E+00 0.00000000000000E+00 0.17284609592577E+00
 0.00000000000000E+00 -.88554071222034E+01 0.10000000000000E-02 0.10867153963464E+01 0.80000000000000E+04
 0.30000000000000E+04 0.73616330705323E+01 0.27606124014496E+01 0.33049301980547E+03 0.39187887351147E+03
 0.31256329640164E+03 0.31256329640164E+03 0.30415001237814E+03 0.30415001215865E+03 0.31255914194416E+03
 0.31255914194416E+03 0.30415001238253E+03 0.30415001216296E+03 0.31256329640164E+03 0.31256329640164E+03
 0.30415001237814E+03 0.30415001215865E+03 0.31255914194416E+03 0.31255914194416E+03 0.30415001238253E+03
 0.30415001216296E+03 0.31134452188918E+03 0.30415006516161E+03 -.55866720531133E+01 -.40444907080405E+01
 0.13347070345938E+03 0.30115328441879E+03 0.16701522744211E+03 0.11866787545763E+03 0.12248770786880E+03
 0.11866787545763E+03 0.23924754957414E+03 0.11869089827529E+03 0.12231134626177E+03 0.11869089827529E+03
 0.23910043799687E+03 0.11866787545763E+03 0.12248770786880E+03 0.11866787545763E+03 0.23924754957414E+03
 0.11869089827529E+03 0.12231134626177E+03 0.11869089827529E+03 0.23910043799687E+03 0.14306272197355E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35219547070563E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14615373790351E+00 0.00000000000000E+00 0.00000000000000E+00 0.14615373790351E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15283326424176E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15283326424176E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573055958655E+00 0.15428460290420E+00 0.33249672571281E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    810.66694277
 0.10365731412436E+00 0.32007142976565E+03 0.42756018475193E+03 0.42384618620511E+03 0.42265583215916E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15743463713010E+00 0.00000000000000E+00 -.16274351821713E+02
 0.33487153106577E-02 0.10755880740883E+01 0.23889758483019E+04 0.89586594311320E+03 0.74377916534463E+01
 0.27891718700424E+01 0.34003721718231E+03 0.30415046148030E+03 0.33477631740953E+03 0.35674486199655E+03
 0.30415006278465E+03 0.30415012096113E+03 0.33184184595991E+03 0.35672417619297E+03 0.30415005148839E+03
 0.30415012060493E+03 0.33477631740953E+03 0.35674486199655E+03 0.30415006278465E+03 0.30415012096113E+03
 0.33184184595991E+03 0.35672417619298E+03 0.30415005148839E+03 0.30415012060493E+03 0.39229587878110E+03
 0.33093185049302E+03 0.14819013198194E+04 0.13897016225748E+04 0.44481531952145E+03 0.76354604434290E+03
 0.31650664822384E+03 0.87890440337397E+03 0.83338334574573E+03 0.81134633625962E+03 0.13260026246778E+04
 0.79800339626856E+03 0.83311627147301E+03 0.74700344431986E+03 0.13259128596145E+04 0.87890440337397E+03
 0.83338334574574E+03 0.81134633625961E+03 0.13260026246778E+04 0.79800339626856E+03 0.83311627147301E+03
 0.74700344431987E+03 0.13259128596145E+04 0.13011570690163E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.45988603203724E+03 0.10919898376886E+01
 0.10919898376886E+01 0.22970216895061E+01 0.00000000000000E+00 0.33261909780083E+03 0.33261909780083E+03
 0.33261909780083E+03 0.33261909780083E+03 0.00000000000000E+00 0.00000000000000E+00 0.17227119227587E+00
 0.00000000000000E+00 -.88470306566634E+01 0.10000000000000E-02 0.10973689751819E+01 0.80000000000000E+04
 0.30000000000000E+04 0.72901641844518E+01 0.27338115691694E+01 0.33093300116055E+03 0.39229475670411E+03
 0.31264687024931E+03 0.31264687024931E+03 0.30415001395965E+03 0.30415001371211E+03 0.31264267577049E+03
 0.31264267577049E+03 0.30415001396457E+03 0.30415001371695E+03 0.31264687024931E+03 0.31264687024931E+03
 0.30415001395965E+03 0.30415001371211E+03 0.31264267577049E+03 0.31264267577049E+03 0.30415001396457E+03
 0.30415001371695E+03 0.31141636955922E+03 0.30415007252974E+03 -.76139759154887E+01 -.63712410504875E+01
 0.13429172961626E+03 0.30244158700468E+03 0.16747839874034E+03 0.12028007276981E+03 0.12321773415064E+03
 0.12028007276981E+03 0.24023721563317E+03 0.12030332699208E+03 0.12304042365340E+03 0.12030332699208E+03
 0.24008944601130E+03 0.12028007276981E+03 0.12321773415064E+03 0.12028007276981E+03 0.24023721563317E+03
 0.12030332699208E+03 0.12304042365340E+03 0.12030332699208E+03 0.24008944601130E+03 0.14311031536433E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35233916076553E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14608790477333E+00 0.00000000000000E+00 0.00000000000000E+00 0.14608790477333E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15275330952393E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15275330952393E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573055248227E+00 0.15422784612966E+00 0.33261909780083E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    821.93912247
 0.10357314790570E+00 0.32023940947900E+03 0.42785264203142E+03 0.42413736161417E+03 0.42294580600778E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15707553114436E+00 0.00000000000000E+00 -.16286159389499E+02
 0.33514362815064E-02 0.10801788097360E+01 0.23870362817711E+04 0.89513860566417E+03 0.74061812061979E+01
 0.27773179523242E+01 0.34036347436892E+03 0.30415052059328E+03 0.33505989555002E+03 0.35716243969746E+03
 0.30415007198407E+03 0.30415013870069E+03 0.33211159793299E+03 0.35714190543328E+03 0.30415005910786E+03
 0.30415013829779E+03 0.33505989555002E+03 0.35716243969746E+03 0.30415007198407E+03 0.30415013870069E+03
 0.33211159793299E+03 0.35714190543328E+03 0.30415005910786E+03 0.30415013829779E+03 0.39278494380082E+03
 0.33144655645741E+03 0.14844409898605E+04 0.13913073209276E+04 0.44390619353733E+03 0.76045415679545E+03
 0.31432843229044E+03 0.88073841657440E+03 0.83463321391420E+03 0.81251165971995E+03 0.13260165579004E+04
 0.79998796439321E+03 0.83436991774566E+03 0.74843257122740E+03 0.13259291208979E+04 0.88073841657439E+03
 0.83463321391420E+03 0.81251165971994E+03 0.13260165579004E+04 0.79998796439322E+03 0.83436991774565E+03
 0.74843257122741E+03 0.13259291208979E+04 0.13008444154165E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46017643229659E+03 0.10919899152268E+01
 0.10919899152268E+01 0.23308382286162E+01 0.00000000000000E+00 0.33276543945437E+03 0.33276543945437E+03
 0.33276543945437E+03 0.33276543945437E+03 0.00000000000000E+00 0.00000000000000E+00 0.17160755771512E+00
 0.00000000000000E+00 -.88412062475172E+01 0.10000000000000E-02 0.11096378760223E+01 0.80000000000000E+04
 0.30000000000000E+04 0.72095592380801E+01 0.27035847142800E+01 0.33144779806879E+03 0.39278371904140E+03
 0.31274474640082E+03 0.31274474640082E+03 0.30415001631348E+03 0.30415001577328E+03 0.31274049982084E+03
 0.31274049982084E+03 0.30415001631920E+03 0.30415001577881E+03 0.31274474640082E+03 0.31274474640082E+03
 0.30415001631348E+03 0.30415001577328E+03 0.31274049982084E+03 0.31274049982084E+03 0.30415001631920E+03
 0.30415001577881E+03 0.31150058504660E+03 0.30415008215107E+03 -.99938005945002E+01 -.90808616329354E+01
 0.13525866225280E+03 0.30398346205320E+03 0.16804850648914E+03 0.12217750933587E+03 0.12407880817643E+03
 0.12217750933587E+03 0.24142354604353E+03 0.12220106752217E+03 0.12390045046561E+03 0.12220106752217E+03
 0.24127510783739E+03 0.12217750933587E+03 0.12407880817643E+03 0.12217750933587E+03 0.24142354604353E+03
 0.12220106752217E+03 0.12390045046561E+03 0.12220106752217E+03 0.24127510783739E+03 0.14318587532919E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35251037679333E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14604920372328E+00 0.00000000000000E+00 0.00000000000000E+00 0.14604920372328E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15266583154605E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15266583154605E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573044195345E+00 0.15415991264580E+00 0.33276543945437E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    832.98911261
 0.10351149367533E+00 0.32039901803696E+03 0.42813731244297E+03 0.42441992851631E+03 0.42322688287496E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15673256774313E+00 0.00000000000000E+00 -.16302886088983E+02
 0.33534322151941E-02 0.10845437360179E+01 0.23856155385377E+04 0.89460582695165E+03 0.73763738006302E+01
 0.27661401752363E+01 0.34068124918429E+03 0.30415058494370E+03 0.33533623160847E+03 0.35756874639154E+03
 0.30415008217425E+03 0.30415015834617E+03 0.33237461261351E+03 0.35754835739874E+03 0.30415006755987E+03
 0.30415015789239E+03 0.33533623160847E+03 0.35756874639154E+03 0.30415008217425E+03 0.30415015834617E+03
 0.33237461261351E+03 0.35754835739874E+03 0.30415006755987E+03 0.30415015789239E+03 0.39325937124705E+03
 0.33194548059554E+03 0.14868854392735E+04 0.13928150582207E+04 0.44301270197469E+03 0.75745630858601E+03
 0.31222854310145E+03 0.88251118404328E+03 0.83583156722872E+03 0.81361010392675E+03 0.13260150909783E+04
 0.80190650520139E+03 0.83557180991028E+03 0.74978726562909E+03 0.13259298029933E+04 0.88251118404328E+03
 0.83583156722872E+03 0.81361010392675E+03 0.13260150909783E+04 0.80190650520140E+03 0.83557180991028E+03
 0.74978726562909E+03 0.13259298029933E+04 0.13005179468660E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46045816771148E+03 0.10919900250680E+01
 0.10919900250680E+01 0.23639881990359E+01 0.00000000000000E+00 0.33291166041476E+03 0.33291166041476E+03
 0.33291166041476E+03 0.33291166041476E+03 0.00000000000000E+00 0.00000000000000E+00 0.17097020034604E+00
 0.00000000000000E+00 -.88422560116720E+01 0.10000000000000E-02 0.11213894254176E+01 0.80000000000000E+04
 0.30000000000000E+04 0.71340069904985E+01 0.26752526214369E+01 0.33194678336692E+03 0.39325805573764E+03
 0.31284087614448E+03 0.31284087614448E+03 0.30415001868319E+03 0.30415001806452E+03 0.31283657535077E+03
 0.31283657535077E+03 0.30415001868969E+03 0.30415001807080E+03 0.31284087614448E+03 0.31284087614448E+03
 0.30415001868319E+03 0.30415001806452E+03 0.31283657535077E+03 0.31283657535077E+03 0.30415001868969E+03
 0.30415001807080E+03 0.31158335716211E+03 0.30415009266705E+03 -.12296134433919E+02 -.11677825025402E+02
 0.13620649084626E+03 0.30552295628067E+03 0.16863543298017E+03 0.12402767899563E+03 0.12492433419085E+03
 0.12402767899563E+03 0.24261009941058E+03 0.12405155566973E+03 0.12474506638831E+03 0.12405155566973E+03
 0.24246114636074E+03 0.12402767899563E+03 0.12492433419085E+03 0.12402767899563E+03 0.24261009941058E+03
 0.12405155566973E+03 0.12474506638831E+03 0.12405155566973E+03 0.24246114636074E+03 0.14328798695846E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35268396382587E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14606428090250E+00 0.00000000000000E+00 0.00000000000000E+00 0.14606428090250E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15264988535989E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15264988535989E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573016423445E+00 0.15409190750892E+00 0.33291166041476E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    840.50075926
 0.10344669742290E+00 0.32051661082985E+03 0.42833160159586E+03 0.42461390002006E+03 0.42342031056560E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15650441119154E+00 0.00000000000000E+00 -.16297516732259E+02
 0.33555325272429E-02 0.10874320181380E+01 0.23841223218817E+04 0.89404587070565E+03 0.73567817266390E+01
 0.27587931474896E+01 0.34089716793136E+03 0.30415063132630E+03 0.33552422574653E+03 0.35784310984072E+03
 0.30415008959016E+03 0.30415017264107E+03 0.33255389094685E+03 0.35782281938914E+03 0.30415007371565E+03
 0.30415017215060E+03 0.33552422574653E+03 0.35784310984072E+03 0.30415008959016E+03 0.30415017264107E+03
 0.33255389094685E+03 0.35782281938914E+03 0.30415007371565E+03 0.30415017215060E+03 0.39357323928476E+03
 0.33227729586903E+03 0.14885561313827E+04 0.13939062886989E+04 0.44250557147032E+03 0.75562154418388E+03
 0.31090344485621E+03 0.88371863780331E+03 0.83666020229405E+03 0.81440836406690E+03 0.13260504686599E+04
 0.80321137539468E+03 0.83640278343370E+03 0.75075371349293E+03 0.13259665788886E+04 0.88371863780331E+03
 0.83666020229405E+03 0.81440836406690E+03 0.13260504686599E+04 0.80321137539469E+03 0.83640278343370E+03
 0.75075371349294E+03 0.13259665788886E+04 0.13003851955256E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46065168988561E+03 0.10919899898084E+01
 0.10919899898084E+01 0.23865231389918E+01 0.00000000000000E+00 0.33301107661835E+03 0.33301107661835E+03
 0.33301107661835E+03 0.33301107661835E+03 0.00000000000000E+00 0.00000000000000E+00 0.17054446550223E+00
 0.00000000000000E+00 -.88238415096264E+01 0.10000000000000E-02 0.11292264438893E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70844958009006E+01 0.26566859253377E+01 0.33227866037353E+03 0.39357183012428E+03
 0.31290734173908E+03 0.31290734173908E+03 0.30415002005683E+03 0.30415001973522E+03 0.31290300341907E+03
 0.31290300341907E+03 0.30415002006377E+03 0.30415001974205E+03 0.31290734173908E+03 0.31290734173908E+03
 0.30415002005683E+03 0.30415001973522E+03 0.31290300341907E+03 0.31290300341907E+03 0.30415002006377E+03
 0.30415001974205E+03 0.31164067460922E+03 0.30415010026358E+03 -.13821106580905E+02 -.13383070580360E+02
 0.13683389360200E+03 0.30653522191974E+03 0.16901715884973E+03 0.12525488515693E+03 0.12548342432675E+03
 0.12525488515693E+03 0.24338605228733E+03 0.12527898393525E+03 0.12530366354052E+03 0.12527898393525E+03
 0.24323687789428E+03 0.12525488515693E+03 0.12548342432675E+03 0.12525488515693E+03 0.24338605228733E+03
 0.12527898393525E+03 0.12530366354052E+03 0.12527898393525E+03 0.24323687789428E+03 0.14335443108048E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35279625484354E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14590733139998E+00 0.00000000000000E+00 0.00000000000000E+00 0.14590733139998E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15252824806096E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15252824806096E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573044196225E+00 0.15404622692153E+00 0.33301107661835E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    850.34807912
 0.10337138858758E+00 0.32066398917523E+03 0.42858486354781E+03 0.42486622000065E+03 0.42367167098918E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15621115840971E+00 0.00000000000000E+00 -.16306667509302E+02
 0.33579768787601E-02 0.10911313680878E+01 0.23823868623401E+04 0.89339507337755E+03 0.73318394411295E+01
 0.27494397904236E+01 0.34117840167691E+03 0.30415069688511E+03 0.33576906517213E+03 0.35820123731094E+03
 0.30415010020247E+03 0.30415019309296E+03 0.33278724764126E+03 0.35818107262999E+03 0.30415008253353E+03
 0.30415019255061E+03 0.33576906517213E+03 0.35820123731094E+03 0.30415010020247E+03 0.30415019309296E+03
 0.33278724764126E+03 0.35818107262999E+03 0.30415008253353E+03 0.30415019255061E+03 0.39398792836398E+03
 0.33271371421567E+03 0.14907234338673E+04 0.13952811334830E+04 0.44175815851016E+03 0.75308628140024E+03
 0.30911933209753E+03 0.88528866901723E+03 0.83772519631778E+03 0.81541171122236E+03 0.13260782969379E+04
 0.80490949033879E+03 0.83747071886526E+03 0.75197988958398E+03 0.13259961470364E+04 0.88528866901722E+03
 0.83772519631778E+03 0.81541171122236E+03 0.13260782969379E+04 0.80490949033880E+03 0.83747071886526E+03
 0.75197988958399E+03 0.13259961470364E+04 0.13001538954615E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46090335699775E+03 0.10919900498999E+01
 0.10919900498999E+01 0.24160650985649E+01 0.00000000000000E+00 0.33314233155663E+03 0.33314233155663E+03
 0.33314233155663E+03 0.33314233155663E+03 0.00000000000000E+00 0.00000000000000E+00 0.16999532914257E+00
 0.00000000000000E+00 -.88178706196093E+01 0.10000000000000E-02 0.11393162200865E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70217555573751E+01 0.26331583340156E+01 0.33271513340413E+03 0.39398642968155E+03
 0.31299366693115E+03 0.31299366693115E+03 0.30415002249266E+03 0.30415002213199E+03 0.31298927870688E+03
 0.31298927870688E+03 0.30415002250039E+03 0.30415002213960E+03 0.31299366693115E+03 0.31299366693115E+03
 0.30415002249266E+03 0.30415002213199E+03 0.31298927870688E+03 0.31298927870688E+03 0.30415002250039E+03
 0.30415002213960E+03 0.31171511363532E+03 0.30415011103120E+03 -.15845932260580E+02 -.15622688935298E+02
 0.13765859157839E+03 0.30788584997497E+03 0.16953896543870E+03 0.12687602962595E+03 0.12621839989191E+03
 0.12687602962595E+03 0.24442328412289E+03 0.12690042396314E+03 0.12603795679779E+03 0.12690042396314E+03
 0.24427379054091E+03 0.12687602962595E+03 0.12621839989191E+03 0.12687602962595E+03 0.24442328412289E+03
 0.12690042396314E+03 0.12603795679779E+03 0.12690042396314E+03 0.24427379054091E+03 0.14345220465655E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35294880081257E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14586344610987E+00 0.00000000000000E+00 0.00000000000000E+00 0.14586344610987E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15245927343355E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15245927343355E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573036156378E+00 0.15398545828542E+00 0.33314233155663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    860.19539898
 0.10329870237013E+00 0.32080973466610E+03 0.42883703674638E+03 0.42511734337123E+03 0.42392178978640E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15592443764425E+00 0.00000000000000E+00 -.16315901350712E+02
 0.33603394793086E-02 0.10947325004881E+01 0.23807118445205E+04 0.89276694169520E+03 0.73077212893863E+01
 0.27403954835199E+01 0.34145813990135E+03 0.30415076791076E+03 0.33601268637842E+03 0.35855717384571E+03
 0.30415011185238E+03 0.30415021553875E+03 0.33301953286595E+03 0.35853713231616E+03 0.30415009222400E+03
 0.30415021494017E+03 0.33601268637842E+03 0.35855717384571E+03 0.30415011185238E+03 0.30415021553875E+03
 0.33301953286595E+03 0.35853713231616E+03 0.30415009222400E+03 0.30415021494017E+03 0.39439993770121E+03
 0.33314748727806E+03 0.14928706811606E+04 0.13966335464914E+04 0.44100407616737E+03 0.75056409948977E+03
 0.30735500294157E+03 0.88684664283067E+03 0.83877730045955E+03 0.81639985296071E+03 0.13261043676865E+04
 0.80659466088141E+03 0.83852565060898E+03 0.75318933120686E+03 0.13260238688552E+04 0.88684664283067E+03
 0.83877730045955E+03 0.81639985296071E+03 0.13261043676865E+04 0.80659466088141E+03 0.83852565060898E+03
 0.75318933120686E+03 0.13260238688552E+04 0.12999186729793E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46115381669757E+03 0.10919901105369E+01
 0.10919901105369E+01 0.24456070581380E+01 0.00000000000000E+00 0.33327418636027E+03 0.33327418636027E+03
 0.33327418636027E+03 0.33327418636027E+03 0.00000000000000E+00 0.00000000000000E+00 0.16945627079757E+00
 0.00000000000000E+00 -.88121213770064E+01 0.10000000000000E-02 0.11492011910512E+01 0.80000000000000E+04
 0.30000000000000E+04 0.69613572125541E+01 0.26105089547078E+01 0.33314894354895E+03 0.39439837013732E+03
 0.31307999144558E+03 0.31307999144558E+03 0.30415002517376E+03 0.30415002477010E+03 0.31307555276195E+03
 0.31307555276195E+03 0.30415002518236E+03 0.30415002477856E+03 0.31307999144558E+03 0.31307999144558E+03
 0.30415002517376E+03 0.30415002477010E+03 0.31307555276195E+03 0.31307555276195E+03 0.30415002518236E+03
 0.30415002477856E+03 0.31178959063118E+03 0.30415012273174E+03 -.17868454318908E+02 -.17820302559323E+02
 0.13847672304906E+03 0.30923527586716E+03 0.17006616920286E+03 0.12849081450403E+03 0.12694730015640E+03
 0.12849081450403E+03 0.24545875398796E+03 0.12851550856029E+03 0.12676621097249E+03 0.12851550856029E+03
 0.24530898178783E+03 0.12849081450404E+03 0.12694730015640E+03 0.12849081450404E+03 0.24545875398796E+03
 0.12851550856029E+03 0.12676621097249E+03 0.12851550856029E+03 0.24530898178783E+03 0.14355924712625E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35310184011305E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14582120245034E+00 0.00000000000000E+00 0.00000000000000E+00 0.14582120245034E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15239327063338E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15239327063338E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573027490698E+00 0.15392445351639E+00 0.33327418636027E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    870.04271884
 0.10323194635287E+00 0.32095418667725E+03 0.42908810672473E+03 0.42536714838030E+03 0.42417051757234E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15564413624211E+00 0.00000000000000E+00 -.16324677395607E+02
 0.33625122370379E-02 0.10982373234597E+01 0.23791734976844E+04 0.89219006163163E+03 0.72844000373233E+01
 0.27316500139962E+01 0.34173640735100E+03 0.30415084474047E+03 0.33625511510119E+03 0.35891095596479E+03
 0.30415012461891E+03 0.30415024012896E+03 0.33325077051032E+03 0.35889103505364E+03 0.30415010285453E+03
 0.30415023946955E+03 0.33625511510119E+03 0.35891095596479E+03 0.30415012461891E+03 0.30415024012896E+03
 0.33325077051032E+03 0.35889103505364E+03 0.30415010285453E+03 0.30415023946955E+03 0.39480921903947E+03
 0.33357864140554E+03 0.14949963710352E+04 0.13979640347891E+04 0.44024432058429E+03 0.74805622470039E+03
 0.30561068251318E+03 0.88839197489123E+03 0.83981642757046E+03 0.81737452691369E+03 0.13261282428968E+04
 0.80826623251234E+03 0.83956749500800E+03 0.75438349810098E+03 0.13260493090222E+04 0.88839197489123E+03
 0.83981642757046E+03 0.81737452691369E+03 0.13261282428968E+04 0.80826623251234E+03 0.83956749500800E+03
 0.75438349810099E+03 0.13260493090221E+04 0.12996793735818E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46140294252116E+03 0.10919901681676E+01
 0.10919901681676E+01 0.24751490177111E+01 0.00000000000000E+00 0.33340660272863E+03 0.33340660272863E+03
 0.33340660272863E+03 0.33340660272863E+03 0.00000000000000E+00 0.00000000000000E+00 0.16892711115392E+00
 0.00000000000000E+00 -.88059722211069E+01 0.10000000000000E-02 0.11588852185751E+01 0.80000000000000E+04
 0.30000000000000E+04 0.69031858132046E+01 0.25886946799517E+01 0.33358013336371E+03 0.39480758415475E+03
 0.31316632019659E+03 0.31316632019659E+03 0.30415002811951E+03 0.30415002766862E+03 0.31316183072784E+03
 0.31316183072784E+03 0.30415002812905E+03 0.30415002767800E+03 0.31316632019659E+03 0.31316632019659E+03
 0.30415002811951E+03 0.30415002766862E+03 0.31316183072784E+03 0.31316183072784E+03 0.30415002812905E+03
 0.30415002767800E+03 0.31186410634703E+03 0.30415013542555E+03 -.19888567361761E+02 -.20050483204891E+02
 0.13928863708634E+03 0.31058319283753E+03 0.17059811256575E+03 0.13009938720496E+03 0.12767040152916E+03
 0.13009938720496E+03 0.24649215522074E+03 0.13012438370960E+03 0.12748869801516E+03 0.13012438370960E+03
 0.24634213882784E+03 0.13009938720496E+03 0.12767040152916E+03 0.13009938720496E+03 0.24649215522074E+03
 0.13012438370960E+03 0.12748869801516E+03 0.13012438370960E+03 0.24634213882783E+03 0.14365749491091E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35325510662177E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14577531317328E+00 0.00000000000000E+00 0.00000000000000E+00 0.14577531317328E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15232588392458E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15232588392458E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573019721604E+00 0.15386324791725E+00 0.33340660272863E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    882.35881537
 0.10314812917968E+00 0.32113376267956E+03 0.42940042359042E+03 0.42567792241526E+03 0.42447995949820E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15530239016051E+00 0.00000000000000E+00 -.16334775138295E+02
 0.33652442823805E-02 0.11024889979346E+01 0.23772419856371E+04 0.89146574461390E+03 0.72563082397984E+01
 0.27211155899244E+01 0.34208222988506E+03 0.30415094977925E+03 0.33655649839158E+03 0.35935045426951E+03
 0.30415014233330E+03 0.30415027423719E+03 0.33353831432909E+03 0.35933068046966E+03 0.30415011762298E+03
 0.30415027349461E+03 0.33655649839158E+03 0.35935045426951E+03 0.30415014233330E+03 0.30415027423719E+03
 0.33353831432909E+03 0.35933068046966E+03 0.30415011762298E+03 0.30415027349461E+03 0.39531848617571E+03
 0.33411216805277E+03 0.14976290988285E+04 0.13996072148445E+04 0.43926990360840E+03 0.74490930966359E+03
 0.30344305653715E+03 0.89030866143380E+03 0.84109639632298E+03 0.81858035880899E+03 0.13261517348444E+04
 0.81033997941714E+03 0.84085071291198E+03 0.75586203223699E+03 0.13260746442392E+04 0.89030866143380E+03
 0.84109639632298E+03 0.81858035880899E+03 0.13261517348444E+04 0.81033997941715E+03 0.84085071291197E+03
 0.75586203223699E+03 0.13260746442392E+04 0.12993685913667E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46171287167150E+03 0.10919902344777E+01
 0.10919902344777E+01 0.25120973073135E+01 0.00000000000000E+00 0.33357392582718E+03 0.33357392582718E+03
 0.33357392582718E+03 0.33357392582718E+03 0.00000000000000E+00 0.00000000000000E+00 0.16827906537394E+00
 0.00000000000000E+00 -.87972840886113E+01 0.10000000000000E-02 0.11707144413314E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68334341130208E+01 0.25625377923828E+01 0.33411371395155E+03 0.39531677292001E+03
 0.31327410367826E+03 0.31327410367826E+03 0.30415003244251E+03 0.30415003170247E+03 0.31326955057538E+03
 0.31326955057538E+03 0.30415003245340E+03 0.30415003171311E+03 0.31327410367826E+03 0.31327410367826E+03
 0.30415003244251E+03 0.30415003170247E+03 0.31326955057538E+03 0.31326955057538E+03 0.30415003245340E+03
 0.30415003171311E+03 0.31195717607347E+03 0.30415015283770E+03 -.22389877852367E+02 -.23124694565134E+02
 0.14030195391674E+03 0.31228312981826E+03 0.17127966613194E+03 0.13210003758526E+03 0.12857360429115E+03
 0.13210003758526E+03 0.24779579873791E+03 0.13212541407607E+03 0.12839118900613E+03 0.13212541407607E+03
 0.24764553507703E+03 0.13210003758526E+03 0.12857360429115E+03 0.13210003758526E+03 0.24779579873791E+03
 0.13212541407607E+03 0.12839118900612E+03 0.13212541407607E+03 0.24764553507703E+03 0.14371873244282E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35344743720893E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14571026676040E+00 0.00000000000000E+00 0.00000000000000E+00 0.14571026676040E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15223107081583E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15223107081583E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573012168708E+00 0.15378600248495E+00 0.33357392582718E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    890.98564031
 0.10309259199021E+00 0.32125828170417E+03 0.42961805403785E+03 0.42589435743873E+03 0.42469542018876E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15506871238298E+00 0.00000000000000E+00 -.16343233619023E+02
 0.33670569706544E-02 0.11053821580521E+01 0.23759621740066E+04 0.89098581525247E+03 0.72373160193735E+01
 0.27139935072650E+01 0.34232312842836E+03 0.30415102969272E+03 0.33676652721125E+03 0.35965630630195E+03
 0.30415015599718E+03 0.30415030053668E+03 0.33373878904652E+03 0.35963663333198E+03 0.30415012902740E+03
 0.30415029973085E+03 0.33676652721125E+03 0.35965630630194E+03 0.30415015599718E+03 0.30415030053668E+03
 0.33373878904652E+03 0.35963663333198E+03 0.30415012902740E+03 0.30415029973085E+03 0.39567231758199E+03
 0.33448111684121E+03 0.14994534729980E+04 0.14007375343929E+04 0.43858620774762E+03 0.74272500954723E+03
 0.30194587076087E+03 0.89163941709107E+03 0.84198024497284E+03 0.81941156596713E+03 0.13261644738860E+04
 0.81177986621308E+03 0.84173674615797E+03 0.75688264836866E+03 0.13260886042261E+04 0.89163941709107E+03
 0.84198024497284E+03 0.81941156596714E+03 0.13261644738860E+04 0.81177986621308E+03 0.84173674615798E+03
 0.75688264836866E+03 0.13260886042262E+04 0.12991479162765E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46192870612215E+03 0.10919902900230E+01
 0.10919902900230E+01 0.25379777821440E+01 0.00000000000000E+00 0.33369290163323E+03 0.33369290163323E+03
 0.33369290163323E+03 0.33369290163323E+03 0.00000000000000E+00 0.00000000000000E+00 0.16783409502062E+00
 0.00000000000000E+00 -.87930186854015E+01 0.10000000000000E-02 0.11788142300284E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67864806822081E+01 0.25449302558280E+01 0.33448270600212E+03 0.39567055332527E+03
 0.31334969142008E+03 0.31334969142008E+03 0.30415003563540E+03 0.30415003482253E+03 0.31334509381236E+03
 0.31334509381236E+03 0.30415003564728E+03 0.30415003483414E+03 0.31334969142008E+03 0.31334969142008E+03
 0.30415003563540E+03 0.30415003482253E+03 0.31334509381236E+03 0.31334509381236E+03 0.30415003564728E+03
 0.30415003483414E+03 0.31202246880526E+03 0.30415016612545E+03 -.24112339243905E+02 -.25332468970749E+02
 0.14101361077816E+03 0.31349236422914E+03 0.17177368539709E+03 0.13349194691278E+03 0.12920882765442E+03
 0.13349194691278E+03 0.24872422905508E+03 0.13351758989644E+03 0.12902596496221E+03 0.13351758989644E+03
 0.24857384386398E+03 0.13349194691278E+03 0.12920882765442E+03 0.13349194691278E+03 0.24872422905508E+03
 0.13351758989644E+03 0.12902596496221E+03 0.13351758989644E+03 0.24857384386398E+03 0.14375759735561E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35358444995249E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14567929344860E+00 0.00000000000000E+00 0.00000000000000E+00 0.14567929344860E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15218235309691E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15218235309691E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573002152678E+00 0.15373107200574E+00 0.33369290163323E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    902.48807357
 0.10300726030591E+00 0.32142615772539E+03 0.42990719086242E+03 0.42618241285622E+03 0.42498237388280E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15476422252737E+00 0.00000000000000E+00 -.16347476753852E+02
 0.33698459780379E-02 0.11091334200693E+01 0.23739957410926E+04 0.89024840290972E+03 0.72128382891031E+01
 0.27048143584136E+01 0.34264274761313E+03 0.30415114463215E+03 0.33704531212411E+03 0.36006161073606E+03
 0.30415017590196E+03 0.30415033883396E+03 0.33400501623313E+03 0.36004206950976E+03 0.30415014565811E+03
 0.30415033793718E+03 0.33704531212411E+03 0.36006161073606E+03 0.30415017590196E+03 0.30415033883396E+03
 0.33400501623313E+03 0.36004206950976E+03 0.30415014565811E+03 0.30415033793718E+03 0.39614041328521E+03
 0.33496718210736E+03 0.15018739109849E+04 0.14022528690953E+04 0.43768032415818E+03 0.73984982335437E+03
 0.29998109757540E+03 0.89340477128140E+03 0.84314906370525E+03 0.82052666148828E+03 0.13261857822536E+04
 0.81368993775766E+03 0.84290836948933E+03 0.75824783803607E+03 0.13261114581305E+04 0.89340477128140E+03
 0.84314906370525E+03 0.82052666148828E+03 0.13261857822536E+04 0.81368993775767E+03 0.84290836948933E+03
 0.75824783803607E+03 0.13261114581305E+04 0.12988683644104E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46221600800480E+03 0.10919903178870E+01
 0.10919903178870E+01 0.25724850819181E+01 0.00000000000000E+00 0.33385310744822E+03 0.33385310744822E+03
 0.33385310744822E+03 0.33385310744822E+03 0.00000000000000E+00 0.00000000000000E+00 0.16725218076748E+00
 0.00000000000000E+00 -.87792452370313E+01 0.10000000000000E-02 0.11893802469164E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67261920825917E+01 0.25223220309719E+01 0.33496883592119E+03 0.39613857700069E+03
 0.31345066083709E+03 0.31345066083709E+03 0.30415004029847E+03 0.30415003937923E+03 0.31344600422071E+03
 0.31344600422071E+03 0.30415004031177E+03 0.30415003939223E+03 0.31345066083709E+03 0.31345066083709E+03
 0.30415004029847E+03 0.30415003937923E+03 0.31344600422071E+03 0.31344600422071E+03 0.30415004031177E+03
 0.30415003939223E+03 0.31210971993239E+03 0.30415018529098E+03 -.26376470386065E+02 -.28309482351310E+02
 0.14196073433058E+03 0.31511195567633E+03 0.17244141767409E+03 0.13533355068770E+03 0.13005472192917E+03
 0.13533355068770E+03 0.24996706340578E+03 0.13535954795611E+03 0.12987131800713E+03 0.13535954795611E+03
 0.24981656905686E+03 0.13533355068770E+03 0.13005472192917E+03 0.13533355068770E+03 0.24996706340578E+03
 0.13535954795611E+03 0.12987131800713E+03 0.13535954795611E+03 0.24981656905686E+03 0.14380465329787E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35376552898211E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14556837267735E+00 0.00000000000000E+00 0.00000000000000E+00 0.14556837267735E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15206498202581E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15206498202581E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573008268838E+00 0.15365738658783E+00 0.33385310744822E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    911.11489852
 0.10294016173701E+00 0.32155155856273E+03 0.43012328692330E+03 0.42639782316413E+03 0.42519699761467E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15454096227331E+00 0.00000000000000E+00 -.16353419243422E+02
 0.33720423067995E-02 0.11118701599235E+01 0.23724494748683E+04 0.88966855307560E+03 0.71950847215385E+01
 0.26981567705769E+01 0.34288130865632E+03 0.30415123746973E+03 0.33725347955806E+03 0.36036378694825E+03
 0.30415019218057E+03 0.30415037014216E+03 0.33420389272073E+03 0.36034434254749E+03 0.30415015927305E+03
 0.30415036917195E+03 0.33725347955806E+03 0.36036378694825E+03 0.30415019218057E+03 0.30415037014216E+03
 0.33420389272073E+03 0.36034434254749E+03 0.30415015927305E+03 0.30415036917195E+03 0.39648900275852E+03
 0.33532768170613E+03 0.15036782711410E+04 0.14033813488091E+04 0.43700264851583E+03 0.73771582052878E+03
 0.29852815877038E+03 0.89472149781110E+03 0.84401862122613E+03 0.82135688417840E+03 0.13262048108913E+04
 0.81511453942803E+03 0.84377995109365E+03 0.75926428345440E+03 0.13261315855074E+04 0.89472149781110E+03
 0.84401862122613E+03 0.82135688417840E+03 0.13262048108913E+04 0.81511453942803E+03 0.84377995109365E+03
 0.75926428345441E+03 0.13261315855074E+04 0.12986640648758E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46243086155742E+03 0.10919903569103E+01
 0.10919903569103E+01 0.25983655567487E+01 0.00000000000000E+00 0.33397452411494E+03 0.33397452411494E+03
 0.33397452411494E+03 0.33397452411494E+03 0.00000000000000E+00 0.00000000000000E+00 0.16682413149152E+00
 0.00000000000000E+00 -.87720997225562E+01 0.10000000000000E-02 0.11971320961425E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66826376352104E+01 0.25059891132039E+01 0.33532938265148E+03 0.39648711528678E+03
 0.31352650162769E+03 0.31352650162769E+03 0.30415004412150E+03 0.30415004311505E+03 0.31352180106192E+03
 0.31352180106192E+03 0.30415004413594E+03 0.30415004312917E+03 0.31352650162769E+03 0.31352650162769E+03
 0.30415004412150E+03 0.30415004311505E+03 0.31352180106192E+03 0.31352180106192E+03 0.30415004413594E+03
 0.30415004312917E+03 0.31217527848859E+03 0.30415020081345E+03 -.28051613254305E+02 -.30554922143268E+02
 0.14267111511430E+03 0.31633858200844E+03 0.17295411131856E+03 0.13670573766121E+03 0.13068958753969E+03
 0.13670573766121E+03 0.25090879869165E+03 0.13673199944396E+03 0.13050581189408E+03 0.13673199944396E+03
 0.25075825536045E+03 0.13670573766121E+03 0.13068958753969E+03 0.13670573766121E+03 0.25090879869165E+03
 0.13673199944396E+03 0.13050581189408E+03 0.13673199944396E+03 0.25075825536045E+03 0.14384265277539E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35390331687058E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14551303343026E+00 0.00000000000000E+00 0.00000000000000E+00 0.14551303343026E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15199591076926E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15199591076926E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14573004891795E+00 0.15360149958201E+00 0.33397452411494E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    923.00435430
 0.10284767079002E+00 0.32172330980732E+03 0.43041995724001E+03 0.42669355823629E+03 0.42549164894409E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15424022773978E+00 0.00000000000000E+00 -.16362022004746E+02
 0.33750745002671E-02 0.11155377966017E+01 0.23703180476066E+04 0.88886926785247E+03 0.71714289057443E+01
 0.26892858396541E+01 0.34320843121135E+03 0.30415137534217E+03 0.33753902898958E+03 0.36077777270959E+03
 0.30415021666332E+03 0.30415041720902E+03 0.33447680158941E+03 0.36075845897133E+03 0.30415017977102E+03
 0.30415041612984E+03 0.33753902898958E+03 0.36077777270959E+03 0.30415021666332E+03 0.30415041720902E+03
 0.33447680158941E+03 0.36075845897133E+03 0.30415017977102E+03 0.30415041612984E+03 0.39696634192004E+03
 0.33581942928928E+03 0.15061473402736E+04 0.14049210054088E+04 0.43606552052932E+03 0.73479522948104E+03
 0.29654938134907E+03 0.89652509046069E+03 0.84520593848153E+03 0.82249039564652E+03 0.13262329829549E+04
 0.81706586150485E+03 0.84496994974504E+03 0.76065259685266E+03 0.13261611901405E+04 0.89652509046069E+03
 0.84520593848153E+03 0.82249039564652E+03 0.13262329829549E+04 0.81706586150485E+03 0.84496994974503E+03
 0.76065259685266E+03 0.13261611901405E+04 0.12983845063266E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46272582918229E+03 0.10919904134031E+01
 0.10919904134031E+01 0.26340339240935E+01 0.00000000000000E+00 0.33414352824905E+03 0.33414352824905E+03
 0.33414352824905E+03 0.33414352824905E+03 0.00000000000000E+00 0.00000000000000E+00 0.16624575229477E+00
 0.00000000000000E+00 -.87627508002349E+01 0.10000000000000E-02 0.12075781027979E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66248302958329E+01 0.24843113609374E+01 0.33582119050488E+03 0.39696439015081E+03
 0.31363114670562E+03 0.31363114670562E+03 0.30415004997649E+03 0.30415004874779E+03 0.31362638606835E+03
 0.31362638606835E+03 0.30415004999266E+03 0.30415004876356E+03 0.31363114670562E+03 0.31363114670562E+03
 0.30415004997649E+03 0.30415004874779E+03 0.31362638606835E+03 0.31362638606835E+03 0.30415004999266E+03
 0.30415004876356E+03 0.31226576205558E+03 0.30415022392930E+03 -.30332372126165E+02 -.33659208976386E+02
 0.14365071916764E+03 0.31804398017932E+03 0.17367500741584E+03 0.13858617747205E+03 0.13156550771464E+03
 0.13858617747205E+03 0.25221834876846E+03 0.13861280163121E+03 0.13138125601023E+03 0.13861280163121E+03
 0.25206777145123E+03 0.13858617747205E+03 0.13156550771464E+03 0.13858617747205E+03 0.25221834876846E+03
 0.13861280163121E+03 0.13138125601023E+03 0.13861280163121E+03 0.25206777145123E+03 0.14389978044117E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35409451635220E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14544163741817E+00 0.00000000000000E+00 0.00000000000000E+00 0.14544163741817E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15190158101697E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15190158101697E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572998760183E+00 0.15352376012229E+00 0.33414352824905E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    932.79224681
 0.10277323988553E+00 0.32186363877431E+03 0.43066309475104E+03 0.42693587055487E+03 0.42573304882620E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15399855594339E+00 0.00000000000000E+00 -.16369524914379E+02
 0.33775185785093E-02 0.11184692050966E+01 0.23686028112186E+04 0.88822605420696E+03 0.71526332272232E+01
 0.26822374602087E+01 0.34347630807627E+03 0.30415149802861E+03 0.33777295441239E+03 0.36111645478111E+03
 0.30415023873810E+03 0.30415045962652E+03 0.33470046575524E+03 0.36109724625863E+03 0.30415019827301E+03
 0.30415045845045E+03 0.33777295441239E+03 0.36111645478111E+03 0.30415023873810E+03 0.30415045962652E+03
 0.33470046575524E+03 0.36109724625863E+03 0.30415019827301E+03 0.30415045845045E+03 0.39735647933444E+03
 0.33621984165203E+03 0.15081627303591E+04 0.14061721377284E+04 0.43529239307847E+03 0.73241026629149E+03
 0.29494141124763E+03 0.89799931921409E+03 0.84617260736535E+03 0.82341271628650E+03 0.13262558482242E+04
 0.81866082677056E+03 0.84593873655443E+03 0.76178299472785E+03 0.13261851675925E+04 0.89799931921409E+03
 0.84617260736535E+03 0.82341271628650E+03 0.13262558482242E+04 0.81866082677056E+03 0.84593873655442E+03
 0.76178299472786E+03 0.13261851675925E+04 0.12981544292166E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46296750542351E+03 0.10919904626734E+01
 0.10919904626734E+01 0.26633976016363E+01 0.00000000000000E+00 0.33428395683304E+03 0.33428395683304E+03
 0.33428395683304E+03 0.33428395683304E+03 0.00000000000000E+00 0.00000000000000E+00 0.16577948104570E+00
 0.00000000000000E+00 -.87556704403209E+01 0.10000000000000E-02 0.12159752358752E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65790813529536E+01 0.24671555073576E+01 0.33622164804852E+03 0.39735448033913E+03
 0.31371742675999E+03 0.31371742675999E+03 0.30415005519681E+03 0.30415005383976E+03 0.31371261713333E+03
 0.31371261713333E+03 0.30415005521450E+03 0.30415005385702E+03 0.31371742675999E+03 0.31371742675999E+03
 0.30415005519681E+03 0.30415005383976E+03 0.31371261713333E+03 0.31371261713333E+03 0.30415005521450E+03
 0.30415005385702E+03 0.31234038763282E+03 0.30415024455774E+03 -.32185911969219E+02 -.36215700131312E+02
 0.14445709384242E+03 0.31945906063168E+03 0.17427968132005E+03 0.14012439832057E+03 0.13228682517229E+03
 0.14012439832057E+03 0.25330508605969E+03 0.14015131873850E+03 0.13210221224568E+03 0.14015131873850E+03
 0.25315450890068E+03 0.14012439832057E+03 0.13228682517229E+03 0.14012439832057E+03 0.25330508605969E+03
 0.14015131873850E+03 0.13210221224568E+03 0.14015131873850E+03 0.25315450890068E+03 0.14395186307892E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35425319072910E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14538781308803E+00 0.00000000000000E+00 0.00000000000000E+00 0.14538781308803E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15183004819605E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15183004819605E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572992010355E+00 0.15345920632617E+00 0.33428395683304E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    942.58013933
 0.10270106658438E+00 0.32200344191340E+03 0.43090529990202E+03 0.42717718757920E+03 0.42597343703015E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15376209878108E+00 0.00000000000000E+00 -.16377267517177E+02
 0.33798919014044E-02 0.11213229601248E+01 0.23669396043927E+04 0.88760235164725E+03 0.71344298516008E+01
 0.26754111943503E+01 0.34374295373743E+03 0.30415162936260E+03 0.33800589318307E+03 0.36145324187011E+03
 0.30415026264406E+03 0.30415050554226E+03 0.33492327879290E+03 0.36143413653898E+03 0.30415021832896E+03
 0.30415050426257E+03 0.33800589318307E+03 0.36145324187011E+03 0.30415026264406E+03 0.30415050554226E+03
 0.33492327879290E+03 0.36143413653898E+03 0.30415021832896E+03 0.30415050426257E+03 0.39774390010240E+03
 0.33661619995589E+03 0.15101629122882E+04 0.14074113328984E+04 0.43452124911458E+03 0.73004848996041E+03
 0.29335463460027E+03 0.89946428905698E+03 0.84713049769885E+03 0.82432795710911E+03 0.13262795958733E+04
 0.82024568309665E+03 0.84689866737263E+03 0.76290454646558E+03 0.13262099692826E+04 0.89946428905698E+03
 0.84713049769885E+03 0.82432795710911E+03 0.13262795958733E+04 0.82024568309665E+03 0.84689866737263E+03
 0.76290454646559E+03 0.13262099692826E+04 0.12979271229685E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46320818387402E+03 0.10919905135177E+01
 0.10919905135177E+01 0.26927612791791E+01 0.00000000000000E+00 0.33442544557898E+03 0.33442544557898E+03
 0.33442544557898E+03 0.33442544557898E+03 0.00000000000000E+00 0.00000000000000E+00 0.16532196528190E+00
 0.00000000000000E+00 -.87489309976167E+01 0.10000000000000E-02 0.12241932625594E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65349158867896E+01 0.24505934575461E+01 0.33661805171680E+03 0.39774185517845E+03
 0.31380386945882E+03 0.31380386945882E+03 0.30415006086312E+03 0.30415005936676E+03 0.31379901130905E+03
 0.31379901130905E+03 0.30415006088243E+03 0.30415005938559E+03 0.31380386945882E+03 0.31380386945882E+03
 0.30415006086312E+03 0.30415005936676E+03 0.31379901130905E+03 0.31379901130905E+03 0.30415006088243E+03
 0.30415005938559E+03 0.31241517532198E+03 0.30415026669527E+03 -.34017277269856E+02 -.38766924291515E+02
 0.14526282747165E+03 0.32088197373092E+03 0.17489283212191E+03 0.14165287967240E+03 0.13300771868687E+03
 0.14165287967240E+03 0.25439771623550E+03 0.14168009421458E+03 0.13282277054698E+03 0.14168009421458E+03
 0.25424716250016E+03 0.14165287967240E+03 0.13300771868687E+03 0.14165287967240E+03 0.25439771623550E+03
 0.14168009421458E+03 0.13282277054698E+03 0.14168009421458E+03 0.25424716250016E+03 0.14400886028787E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35441273449626E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14533704319743E+00 0.00000000000000E+00 0.00000000000000E+00 0.14533704319743E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15176046810053E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15176046810053E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572984268324E+00 0.15339420950711E+00 0.33442544557898E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    952.36803184
 0.10263246156564E+00 0.32214250508947E+03 0.43114653200076E+03 0.42741741479994E+03 0.42621269966133E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15353075551437E+00 0.00000000000000E+00 -.16385810979063E+02
 0.33821509705507E-02 0.11241007927497E+01 0.23653586340935E+04 0.88700948778508E+03 0.71167995357704E+01
 0.26687998259139E+01 0.34400837424720E+03 0.30415176978076E+03 0.33823784866363E+03 0.36178817240376E+03
 0.30415028849662E+03 0.30415055517388E+03 0.33514523738661E+03 0.36176916826889E+03 0.30415024003846E+03
 0.30415055378351E+03 0.33823784866363E+03 0.36178817240376E+03 0.30415028849662E+03 0.30415055517388E+03
 0.33514523738661E+03 0.36176916826889E+03 0.30415024003846E+03 0.30415055378351E+03 0.39812875419568E+03
 0.33700872863999E+03 0.15121469958871E+04 0.14086363010940E+04 0.43375006429867E+03 0.72770578032821E+03
 0.29178696570804E+03 0.90091961708865E+03 0.84807901600242E+03 0.82523463802961E+03 0.13263029903302E+04
 0.82182006067200E+03 0.84784915084075E+03 0.76401585345242E+03 0.13262343614269E+04 0.90091961708865E+03
 0.84807901600242E+03 0.82523463802961E+03 0.13263029903302E+04 0.82182006067200E+03 0.84784915084075E+03
 0.76401585345243E+03 0.13262343614269E+04 0.12977001678407E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46344776572874E+03 0.10919905696212E+01
 0.10919905696212E+01 0.27221249567219E+01 0.00000000000000E+00 0.33456790094140E+03 0.33456790094140E+03
 0.33456790094140E+03 0.33456790094140E+03 0.00000000000000E+00 0.00000000000000E+00 0.16487305028486E+00
 0.00000000000000E+00 -.87431933949613E+01 0.10000000000000E-02 0.12322356840063E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64922645106251E+01 0.24345991914844E+01 0.33701062494163E+03 0.39812666592240E+03
 0.31389045758343E+03 0.31389045758343E+03 0.30415006700460E+03 0.30415006535724E+03 0.31388555139205E+03
 0.31388555139205E+03 0.30415006702562E+03 0.30415006537776E+03 0.31389045758343E+03 0.31389045758343E+03
 0.30415006700460E+03 0.30415006535724E+03 0.31388555139205E+03 0.31388555139205E+03 0.30415006702562E+03
 0.30415006537776E+03 0.31249010895786E+03 0.30415029042183E+03 -.35828212035701E+02 -.41311290594402E+02
 0.14606790000336E+03 0.32231232237504E+03 0.17551408287167E+03 0.14317220540415E+03 0.13372810543575E+03
 0.14317220540415E+03 0.25549594998959E+03 0.14319971181531E+03 0.13354284361916E+03 0.14319971181531E+03
 0.25534543841398E+03 0.14317220540415E+03 0.13372810543575E+03 0.14317220540415E+03 0.25549594998959E+03
 0.14319971181531E+03 0.13354284361916E+03 0.14319971181531E+03 0.25534543841398E+03 0.14407097123516E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35457330455324E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14529498337784E+00 0.00000000000000E+00 0.00000000000000E+00 0.14529498337784E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15169738656642E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15169738656642E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572973938885E+00 0.15332879593147E+00 0.33456790094140E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    962.15592435
 0.10256667064884E+00 0.32228108197530E+03 0.43138675457562E+03 0.42765655271312E+03 0.42645085279813E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15330444123576E+00 0.00000000000000E+00 -.16390956172454E+02
 0.33843202058194E-02 0.11268042390642E+01 0.23638425188739E+04 0.88644094457772E+03 0.70997247992642E+01
 0.26623967997241E+01 0.34427257867194E+03 0.30415191973160E+03 0.33846882858852E+03 0.36212127310494E+03
 0.30415031641593E+03 0.30415060874761E+03 0.33536634575584E+03 0.36210236820864E+03 0.30415026350529E+03
 0.30415060723920E+03 0.33846882858852E+03 0.36212127310494E+03 0.30415031641593E+03 0.30415060874761E+03
 0.33536634575584E+03 0.36210236820864E+03 0.30415026350529E+03 0.30415060723920E+03 0.39851108619211E+03
 0.33739755417481E+03 0.15141150884923E+04 0.14098487557540E+04 0.43297821245056E+03 0.72538063586482E+03
 0.29023753235200E+03 0.90236529640653E+03 0.84901772003464E+03 0.82613414852700E+03 0.13263250385447E+04
 0.82338399246724E+03 0.84878974758498E+03 0.76511829741386E+03 0.13262573534406E+04 0.90236529640653E+03
 0.84901772003464E+03 0.82613414852700E+03 0.13263250385447E+04 0.82338399246725E+03 0.84878974758498E+03
 0.76511829741388E+03 0.13262573534406E+04 0.12974731497972E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46368625546832E+03 0.10919906034088E+01
 0.10919906034088E+01 0.27514886342647E+01 0.00000000000000E+00 0.33471114574798E+03 0.33471114574798E+03
 0.33471114574798E+03 0.33471114574798E+03 0.00000000000000E+00 0.00000000000000E+00 0.16443259487249E+00
 0.00000000000000E+00 -.87336377713543E+01 0.10000000000000E-02 0.12401061233809E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64510607996918E+01 0.24191477998844E+01 0.33739949372491E+03 0.39850895757800E+03
 0.31397718800009E+03 0.31397718800009E+03 0.30415007365168E+03 0.30415007184091E+03 0.31397223425730E+03
 0.31397223425730E+03 0.30415007367455E+03 0.30415007186321E+03 0.31397718800009E+03 0.31397718800009E+03
 0.30415007365168E+03 0.30415007184091E+03 0.31397223425730E+03 0.31397223425730E+03 0.30415007367455E+03
 0.30415007186321E+03 0.31256518520497E+03 0.30415031581976E+03 -.37620296660980E+02 -.43847855265905E+02
 0.14687157323229E+03 0.32374559084050E+03 0.17613965974205E+03 0.14468222299719E+03 0.13444719745577E+03
 0.14468222299719E+03 0.25659573600984E+03 0.14471001896414E+03 0.13426164059887E+03 0.14471001896414E+03
 0.25644528181685E+03 0.14468222299719E+03 0.13444719745577E+03 0.14468222299719E+03 0.25659573600984E+03
 0.14471001896414E+03 0.13426164059887E+03 0.14471001896414E+03 0.25644528181685E+03 0.14413612283770E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35473311164609E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14521977501898E+00 0.00000000000000E+00 0.00000000000000E+00 0.14521977501898E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15161053120003E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15161053120003E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572972805438E+00 0.15326317904358E+00 0.33471114574798E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    971.94381687
 0.10249366533188E+00 0.32242026850087E+03 0.43162603310747E+03 0.42789506674417E+03 0.42668850094782E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15308299092759E+00 0.00000000000000E+00 -.16398138446602E+02
 0.33867305970589E-02 0.11294356365301E+01 0.23621601337134E+04 0.88581005014254E+03 0.70831836195447E+01
 0.26561938573293E+01 0.34453561577940E+03 0.30415207967548E+03 0.33869887812013E+03 0.36245257351525E+03
 0.30415034652690E+03 0.30415066649837E+03 0.33558664632999E+03 0.36243376595057E+03 0.30415028883743E+03
 0.30415066486420E+03 0.33869887812013E+03 0.36245257351525E+03 0.30415034652690E+03 0.30415066649837E+03
 0.33558664632999E+03 0.36243376595057E+03 0.30415028883743E+03 0.30415066486420E+03 0.39889094115856E+03
 0.33778273291111E+03 0.15160732113237E+04 0.14110615580247E+04 0.43220688036452E+03 0.72307464823151E+03
 0.28870673346517E+03 0.90380387030965E+03 0.84994794286630E+03 0.82703392604415E+03 0.13263470842584E+04
 0.82494028515008E+03 0.84972179672243E+03 0.76621954164705E+03 0.13262802945771E+04 0.90380387030965E+03
 0.84994794286629E+03 0.82703392604415E+03 0.13263470842584E+04 0.82494028515009E+03 0.84972179672243E+03
 0.76621954164706E+03 0.13262802945771E+04 0.12972515238948E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46392414916987E+03 0.10919906505735E+01
 0.10919906505735E+01 0.27808523118075E+01 0.00000000000000E+00 0.33485457288133E+03 0.33485457288133E+03
 0.33485457288133E+03 0.33485457288133E+03 0.00000000000000E+00 0.00000000000000E+00 0.16400053728779E+00
 0.00000000000000E+00 -.87264699455515E+01 0.10000000000000E-02 0.12478081680431E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64112418918899E+01 0.24042157094587E+01 0.33778471464856E+03 0.39888877503659E+03
 0.31406402070874E+03 0.31406402070874E+03 0.30415008083611E+03 0.30415007884870E+03 0.31405901990572E+03
 0.31405901990572E+03 0.30415008086092E+03 0.30415007887290E+03 0.31406402070874E+03 0.31406402070874E+03
 0.30415008083611E+03 0.30415007884870E+03 0.31405901990572E+03 0.31405901990572E+03 0.30415008086092E+03
 0.30415007887290E+03 0.31264037238872E+03 0.30415034297381E+03 -.39397548386448E+02 -.46379896044643E+02
 0.14767030013085E+03 0.32517573253149E+03 0.17676708089999E+03 0.14618114195794E+03 0.13516145314619E+03
 0.14618114195794E+03 0.25769231776709E+03 0.14620922518654E+03 0.13497561757396E+03 0.14620922518654E+03
 0.25754193431120E+03 0.14618114195794E+03 0.13516145314619E+03 0.14618114195794E+03 0.25769231776709E+03
 0.14620922518654E+03 0.13497561757396E+03 0.14620922518654E+03 0.25754193431120E+03 0.14420065007226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35489374182618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14516523677610E+00 0.00000000000000E+00 0.00000000000000E+00 0.14516523677610E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15153901551738E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15153901551738E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572965839400E+00 0.15319746995328E+00 0.33485457288133E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    981.73170938
 0.10242204980575E+00 0.32255843085296E+03 0.43186438651137E+03 0.42813260649987E+03 0.42692515501867E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15286625981771E+00 0.00000000000000E+00 -.16405575846539E+02
 0.33890984376670E-02 0.11319972191226E+01 0.23605097777883E+04 0.88519116667062E+03 0.70671551703995E+01
 0.26501831888998E+01 0.34479749405096E+03 0.30415225008443E+03 0.33892799687851E+03 0.36278210440225E+03
 0.30415037895928E+03 0.30415072866981E+03 0.33580613736194E+03 0.36276339230356E+03 0.30415031614717E+03
 0.30415072690183E+03 0.33892799687851E+03 0.36278210440225E+03 0.30415037895928E+03 0.30415072866981E+03
 0.33580613736193E+03 0.36276339230356E+03 0.30415031614717E+03 0.30415072690183E+03 0.39926836104545E+03
 0.33816434830076E+03 0.15180176633881E+04 0.14122603787745E+04 0.43143617845909E+03 0.72078736781771E+03
 0.28719400846632E+03 0.90523404243834E+03 0.85086992804362E+03 0.82792408351724E+03 0.13263694239647E+04
 0.82648743248981E+03 0.85064554236658E+03 0.76730979570435E+03 0.13263034815422E+04 0.90523404243835E+03
 0.85086992804362E+03 0.82792408351724E+03 0.13263694239647E+04 0.82648743248981E+03 0.85064554236658E+03
 0.76730979570436E+03 0.13263034815422E+04 0.12970308098308E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46416106413585E+03 0.10919906994137E+01
 0.10919906994137E+01 0.28102159893503E+01 0.00000000000000E+00 0.33499848619932E+03 0.33499848619932E+03
 0.33499848619932E+03 0.33499848619932E+03 0.00000000000000E+00 0.00000000000000E+00 0.16357668586977E+00
 0.00000000000000E+00 -.87196539452631E+01 0.10000000000000E-02 0.12553446147583E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63727520761621E+01 0.23897820285608E+01 0.33816637131051E+03 0.39926616006695E+03
 0.31415095882290E+03 0.31415095882290E+03 0.30415008859091E+03 0.30415008641284E+03 0.31414591145239E+03
 0.31414591145239E+03 0.30415008861778E+03 0.30415008643906E+03 0.31415095882290E+03 0.31415095882290E+03
 0.30415008859091E+03 0.30415008641284E+03 0.31414591145239E+03 0.31414591145239E+03 0.30415008861778E+03
 0.30415008643906E+03 0.31271567166991E+03 0.30415037197105E+03 -.41158060580446E+02 -.48902487336721E+02
 0.14846645255301E+03 0.32660694902145E+03 0.17739816420568E+03 0.14767037863994E+03 0.13587320748764E+03
 0.14767037863994E+03 0.25878916870971E+03 0.14769874678645E+03 0.13568710727903E+03 0.14769874678645E+03
 0.25863886696176E+03 0.14767037863994E+03 0.13587320748764E+03 0.14767037863994E+03 0.25878916870971E+03
 0.14769874678645E+03 0.13568710727903E+03 0.14769874678645E+03 0.25863886696176E+03 0.14426778520749E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35505472799801E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14511389352800E+00 0.00000000000000E+00 0.00000000000000E+00 0.14511389352800E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15146916402593E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15146916402593E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572957953638E+00 0.15313158469161E+00 0.33499848619932E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
    991.51960190
 0.10235246398933E+00 0.32269583610952E+03 0.43210178328379E+03 0.42836912719433E+03 0.42716077050095E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15265414746931E+00 0.00000000000000E+00 -.16413038469115E+02
 0.33914023383663E-02 0.11344906783044E+01 0.23589061992137E+04 0.88458982470513E+03 0.70516225060191E+01
 0.26443584397572E+01 0.34505821907222E+03 0.30415243144208E+03 0.33915618799808E+03 0.36310989180560E+03
 0.30415041384767E+03 0.30415079551438E+03 0.33602481970400E+03 0.36309127334596E+03 0.30415034555113E+03
 0.30415079360421E+03 0.33915618799808E+03 0.36310989180560E+03 0.30415041384767E+03 0.30415079551438E+03
 0.33602481970400E+03 0.36309127334596E+03 0.30415034555113E+03 0.30415079360421E+03 0.39964337141161E+03
 0.33854248245101E+03 0.15199479335081E+04 0.14134464180875E+04 0.43066574321405E+03 0.71851778158801E+03
 0.28569870965789E+03 0.90665558612548E+03 0.85178326469750E+03 0.82880609780529E+03 0.13263911834799E+04
 0.82802520359092E+03 0.85156057606778E+03 0.76839042800751E+03 0.13263260421782E+04 0.90665558612548E+03
 0.85178326469750E+03 0.82880609780529E+03 0.13263911834799E+04 0.82802520359093E+03 0.85156057606778E+03
 0.76839042800752E+03 0.13263260421782E+04 0.12968105101614E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46439695648834E+03 0.10919907484194E+01
 0.10919907484194E+01 0.28395796668931E+01 0.00000000000000E+00 0.33514281683384E+03 0.33514281683384E+03
 0.33514281683384E+03 0.33514281683384E+03 0.00000000000000E+00 0.00000000000000E+00 0.16316089674524E+00
 0.00000000000000E+00 -.87129483658649E+01 0.10000000000000E-02 0.12627186990932E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63355361774125E+01 0.23758260665297E+01 0.33854454572299E+03 0.39964113800640E+03
 0.31423799853052E+03 0.31423799853052E+03 0.30415009695042E+03 0.30415009456683E+03 0.31423290508070E+03
 0.31423290508070E+03 0.30415009697947E+03 0.30415009459517E+03 0.31423799853052E+03 0.31423799853052E+03
 0.30415009695042E+03 0.30415009456683E+03 0.31423290508070E+03 0.31423290508070E+03 0.30415009697947E+03
 0.30415009459517E+03 0.31279107979533E+03 0.30415040290095E+03 -.42902445769026E+02 -.51414683291562E+02
 0.14925990751758E+03 0.32803846109717E+03 0.17803225404200E+03 0.14915000548100E+03 0.13658230979869E+03
 0.14915000548100E+03 0.25988564432795E+03 0.14917865622123E+03 0.13639595705926E+03 0.14917865622123E+03
 0.25973543328104E+03 0.14915000548100E+03 0.13658230979869E+03 0.14915000548100E+03 0.25988564432795E+03
 0.14917865622123E+03 0.13639595705926E+03 0.14917865622123E+03 0.25973543328104E+03 0.14433737464610E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35521596828745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14506347647802E+00 0.00000000000000E+00 0.00000000000000E+00 0.14506347647802E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15140029045205E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15140029045205E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572949748692E+00 0.15306556189809E+00 0.33514281683384E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
   1000.00000000
 0.10229358996023E+00 0.32281463025076E+03 0.43230672903670E+03 0.42857328241767E+03 0.42736413647460E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15247403110862E+00 0.00000000000000E+00 -.16419120287001E+02
 0.33933540291895E-02 0.11365970582211E+01 0.23575494720516E+04 0.88408105201936E+03 0.70385542018921E+01
 0.26394578257095E+01 0.34528328975253E+03 0.30415259764952E+03 0.33935325743263E+03 0.36339249280313E+03
 0.30415044613269E+03 0.30415085734073E+03 0.33621376835405E+03 0.36337395411479E+03 0.30415037278283E+03
 0.30415085530044E+03 0.33935325743263E+03 0.36339249280313E+03 0.30415044613269E+03 0.30415085734073E+03
 0.33621376835405E+03 0.36337395411479E+03 0.30415037278283E+03 0.30415085530044E+03 0.39996568935665E+03
 0.33886662870291E+03 0.15216087079157E+04 0.14144651740265E+04 0.43000776860965E+03 0.71658211115412E+03
 0.28442430370142E+03 0.90788014322343E+03 0.85256815901746E+03 0.82956506722787E+03 0.13264103032918E+04
 0.82934971174494E+03 0.85234689393139E+03 0.76931993591262E+03 0.13263458222390E+04 0.90788014322343E+03
 0.85256815901745E+03 0.82956506722787E+03 0.13264103032918E+04 0.82934971174494E+03 0.85234689393138E+03
 0.76931993591263E+03 0.13263458222390E+04 0.12966247508145E+03 0.00000000000000E+00 0.30000000000000E-02
 0.15015000000000E+06 0.15015000000000E+06 0.50050000000000E+08 0.46460056746996E+03 0.10919907883577E+01
 0.10919907883577E+01 0.28650208612018E+01 0.00000000000000E+00 0.33526815163331E+03 0.33526815163331E+03
 0.33526815163331E+03 0.33526815163331E+03 0.00000000000000E+00 0.00000000000000E+00 0.16280706211587E+00
 0.00000000000000E+00 -.87069034442391E+01 0.10000000000000E-02 0.12689788673133E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63042815022900E+01 0.23641055633587E+01 0.33886872660569E+03 0.39996342908684E+03
 0.31431362078290E+03 0.31431362078290E+03 0.30415010399488E+03 0.30415010212659E+03 0.31430848782123E+03
 0.31430848782123E+03 0.30415010402572E+03 0.30415010215687E+03 0.31431362078290E+03 0.31431362078290E+03
 0.30415010399488E+03 0.30415010212659E+03 0.31430848782123E+03 0.31430848782123E+03 0.30415010402572E+03
 0.30415010215687E+03 0.31285661806962E+03 0.30415043130451E+03 -.44395237479277E+02 -.53573904208407E+02
 0.14994411636680E+03 0.32927660816363E+03 0.17858277121500E+03 0.15042100265130E+03 0.13719354794778E+03
 0.15042100265130E+03 0.26083336419972E+03 0.15044989635873E+03 0.13700699614927E+03 0.15044989635873E+03
 0.26068324883352E+03 0.15042100265130E+03 0.13719354794778E+03 0.15042100265130E+03 0.26083336419972E+03
 0.15044989635873E+03 0.13700699614927E+03 0.15044989635873E+03 0.26068324883352E+03 0.14440064436523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35535605396054E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14501647096853E+00 0.00000000000000E+00 0.00000000000000E+00 0.14501647096853E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15134545860253E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15134545860253E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14572943181822E+00 0.15300828089482E+00 0.33526815163331E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30415000000000E+03
