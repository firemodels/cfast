#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-44 0 MONOZONE(1=OUI,0=NON)                                                                     
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
3.000000 2.000000 0.900000 0.560000                                                                 
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
Plenum-LLNL44 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.173000                                                                                   
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
#CONDINIT 2000.000000 10.000000 19.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-44           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-44           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-44           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-44           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-44           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-44           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-44           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-44           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-44           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-44           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-44           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-44           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-44           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-44           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-44           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-44           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-44           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-44           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL44     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL44     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL44     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL44     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL44     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL44     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL44     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL44     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL44     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL44     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL44     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL44     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL44     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL44     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL44     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL44     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL44     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL44     #HEAT_POWER#W#Total sprinkling power
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.70103560273725E-12
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
 0.00000000000000E+00 -.60577779711941E-07 0.99964766651472E-03 0.00000000000000E+00 0.80000000000000E+04
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
 0.00000000000000E+00 0.00000000000000E+00 0.47035961068911E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47035961068911E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17299972742010E+00 0.20846386550914E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     10.60734796
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14340824065114E+02
 0.99985846707066E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000148909E+03 0.29215000000000E+03 0.29215000205565E+03 0.29215000205565E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000204510E+03 0.29215000204510E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000205565E+03 0.29215000205565E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000204510E+03 0.29215000204510E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000727678E+03
 0.29215000600555E+03 0.46305704717551E-03 0.46305704717551E-03 0.61307330357129E-03 0.61613867008914E-03
 .00000000000000E+00 0.43417297930117E-03 0.51544143315711E-03 0.43417297930117E-03 0.51544143315711E-03
 0.43193209738815E-03 0.51548885992058E-03 0.43193209738815E-03 0.51548885992058E-03 0.43417297935847E-03
 0.51544143327171E-03 0.43417297935847E-03 0.51544143327171E-03 0.43193209744545E-03 0.51548885992058E-03
 0.43193209744545E-03 0.51548885992058E-03 0.51897294808229E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999929755E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.15850237605588E+02 0.99950939825220E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000598124E+03 0.29215000730638E+03
 0.29215000220756E+03 0.29215000220756E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000218825E+03
 0.29215000218825E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000220756E+03 0.29215000220756E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000218825E+03 0.29215000218825E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000212616E+03 0.29215000000000E+03 0.50727730677205E-03 0.50727730677205E-03
 0.65646532193898E-03 0.65974764854867E-03 .00000000000000E+00 0.46623167763085E-03 0.51243917031291E-03
 0.46623167763085E-03 0.51243917031291E-03 0.46214332172210E-03 0.51253040196325E-03 0.46214332172210E-03
 0.51253040196325E-03 0.46623167757355E-03 0.51243917031291E-03 0.46623167757355E-03 0.51243917031291E-03
 0.46214332166480E-03 0.51253040184865E-03 0.46214332166480E-03 0.51253040184865E-03 0.41946488984858E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20787996326326E+00 0.00000000000000E+00 0.00000000000000E+00 0.20787996326326E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984106320E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984106320E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17254198492385E+00 0.20787975281752E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     20.69209944
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999990E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14340824013765E+02
 0.99985846707117E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000209551E+03 0.29215000000000E+03 0.29215000288400E+03 0.29215000288400E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000286910E+03 0.29215000286910E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000288400E+03 0.29215000288400E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000286910E+03 0.29215000286910E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001005516E+03
 0.29215000830520E+03 0.46736097840153E-03 0.46736097840153E-03 0.60309348681743E-03 0.60610895425152E-03
 .00000000000000E+00 0.43644586109479E-03 0.51985013952214E-03 0.43644586109479E-03 0.51985013952214E-03
 0.43417854072596E-03 0.51991954445102E-03 0.43417854072596E-03 0.51991954445102E-03 0.43644586115209E-03
 0.51985013957944E-03 0.43644586115209E-03 0.51985013957944E-03 0.43417854072596E-03 0.51991954445102E-03
 0.43417854072596E-03 0.51991954445102E-03 0.51895614361540E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999933069E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.15850237552650E+02 0.99952587105206E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000828184E+03 0.29215001008350E+03
 0.29215000309694E+03 0.29215000309694E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000306978E+03
 0.29215000306978E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000309694E+03 0.29215000309694E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000306978E+03 0.29215000306978E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000298401E+03 0.29215000000000E+03 0.50031854824461E-03 0.50031854824461E-03
 0.66036368770612E-03 0.66366550614465E-03 .00000000000000E+00 0.46864533879831E-03 0.51443345496740E-03
 0.46864533879831E-03 0.51443345496740E-03 0.46452553818879E-03 0.51456457350152E-03 0.46452553818879E-03
 0.51456457350152E-03 0.46864533879831E-03 0.51443345502470E-03 0.46864533879831E-03 0.51443345502470E-03
 0.46452553813149E-03 0.51456457338692E-03 0.46452553813149E-03 0.51456457338692E-03 0.41945539861633E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20787996289105E+00 0.00000000000000E+00 0.00000000000000E+00 0.20787996289105E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984095315E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984095315E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17254198492538E+00 0.20787975281948E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     35.69209944
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.22999999999989E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14340824012485E+02
 0.99985846707118E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000275856E+03 0.29215000000000E+03 0.29215000378054E+03 0.29215000378054E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000376090E+03 0.29215000376090E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000378054E+03 0.29215000378054E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000376090E+03 0.29215000376090E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001288535E+03
 0.29215001065573E+03 0.47168673600520E-03 0.47168673600520E-03 0.59316392964510E-03 0.59612974929333E-03
 .00000000000000E+00 0.43867269143989E-03 0.52420325157967E-03 0.43867269143989E-03 0.52420325157967E-03
 0.43638209087743E-03 0.52429669701606E-03 0.43638209087743E-03 0.52429669701606E-03 0.43867269143989E-03
 0.52420325157967E-03 0.43867269143989E-03 0.52420325157967E-03 0.43638209082013E-03 0.52429669701606E-03
 0.43638209082013E-03 0.52429669701606E-03 0.51892982719371E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.22999999937535E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.15850237551326E+02 0.99954817937026E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215001064076E+03 0.29215001290342E+03
 0.29215000405949E+03 0.29215000405949E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000402381E+03
 0.29215000402381E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000405949E+03 0.29215000405949E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000402381E+03 0.29215000402381E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000391502E+03 0.29215000000000E+03 0.49342146899971E-03 0.49342146899971E-03
 0.66420288599158E-03 0.66752390042154E-03 .00000000000000E+00 0.47101890336394E-03 0.51639499340859E-03
 0.47101890336394E-03 0.51639499340859E-03 0.46687207221755E-03 0.51656939167494E-03 0.46687207221755E-03
 0.51656939167494E-03 0.47101890342124E-03 0.51639499340859E-03 0.47101890342124E-03 0.51639499340859E-03
 0.46687207216025E-03 0.51656939156034E-03 0.46687207216025E-03 0.51656939156034E-03 0.41943493993443E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20787996288177E+00 0.00000000000000E+00 0.00000000000000E+00 0.20787996288177E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984095012E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20787984095012E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17254198492542E+00 0.20787975281953E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     40.00119103
 0.28386276757121E+01 0.29215139568327E+03 0.32111774798386E+03 0.29370951821557E+03 0.29357587999293E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22659951154740E+00 0.00000000000000E+00 -.26819726019424E-03
 0.99999522009278E-03 0.40569565676390E-01 0.80000000000000E+04 0.30000000000000E+04 0.19719215295065E+03
 0.73947057356493E+02 0.29242800780491E+03 0.29215000000001E+03 0.29247002316157E+03 0.29316178030823E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29239176978665E+03 0.29316023251137E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29247002316157E+03 0.29316178030823E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29239176978665E+03 0.29316023251137E+03 0.29215000000001E+03 0.29215000000001E+03 0.29406725907412E+03
 0.29215145452703E+03 0.19928502482603E+03 0.19895434691859E+03 0.15637958079443E+03 0.39822490569003E+03
 0.24106342699163E+03 0.16250944385534E+03 0.92524913303648E+02 0.16217993974244E+03 0.44816674758417E+03
 0.12258102040868E+03 0.91521298472911E+02 0.12235304330399E+03 0.44718518893950E+03 0.16250944385534E+03
 0.92524913303648E+02 0.16217993974244E+03 0.44816674758417E+03 0.12258102040868E+03 0.91521298472911E+02
 0.12235304330399E+03 0.44718518893950E+03 0.24080408550153E+02 0.00000000000000E+00 0.17236366335166E-02
 0.86268013507508E+05 0.86268013507508E+05 0.50050000000000E+08 0.34269945444238E+03 0.76122785401640E+00
 0.76122785401640E+00 0.37200952903593E-02 0.14268779333288E+01 0.29215000000000E+03 0.29683973417204E+03
 0.29237861536987E+03 0.29237517692720E+03 0.23000000000000E+00 0.00000000000000E+00 0.22938954605938E+00
 0.00000000000000E+00 -.20319184964395E+01 0.99968463294515E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215145421772E+03 0.29406800352496E+03
 0.29215074992625E+03 0.29225923036456E+03 0.29215000000001E+03 0.29215000000001E+03 0.29215075840507E+03
 0.29225923184997E+03 0.29215000000001E+03 0.29215000000001E+03 0.29215074992625E+03 0.29225923036456E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29215075840507E+03 0.29225923184997E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29218032045599E+03 0.29215000000001E+03 0.33003853104340E+00 0.32965542379555E+00
 0.36812828524356E+00 0.25943811941615E+02 0.25573843014945E+02 0.40507979457845E+00 -.19376000528221E+00
 0.40492608185626E+00 0.54140602752783E+02 0.40911940139463E+00 -.19253598432666E+00 0.40896350947300E+00
 0.54141807738198E+02 0.40507979457840E+00 -.19376000528221E+00 0.40492608185620E+00 0.54140602752783E+02
 0.40911940139457E+00 -.19253598432660E+00 0.40896350947294E+00 0.54141807738199E+02 0.72134306608778E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30183596329654E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.27343981452200E-02 0.40231203856125E-03 0.00000000000000E+00 0.27343981452200E-02 0.40231203856125E-03
 0.16931383785603E+00 0.69313814014172E-01 0.10000002384186E+00 0.52000010490417E-01 0.19176097475975E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19176097475975E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17294111490311E+00 0.20838905721757E+00 0.29215000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     50.00204467
 0.24159140714442E+01 0.29218821156539E+03 0.37380596332478E+03 0.30807880500639E+03 0.30516067508308E+03
 0.22999999997128E+00 0.00000000000000E+00 0.21995593253082E+00 0.00000000000000E+00 0.12852627157967E+02
 0.99999605174780E-03 0.16560377493547E+00 0.80000000000000E+04 0.30000000000000E+04 0.48308077536984E+02
 0.18115529076369E+02 0.29350512649436E+03 0.29215000000004E+03 0.29393172115531E+03 0.29675083571966E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29350659928355E+03 0.29673093270034E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29393172115531E+03 0.29675083571966E+03 0.29215000000003E+03 0.29215000000003E+03
 0.29350659928355E+03 0.29673093270034E+03 0.29215000000003E+03 0.29215000000003E+03 0.30384272811462E+03
 0.29216219684718E+03 0.45919258424937E+03 0.45654612133266E+03 0.41488420567353E+03 0.11682276262213E+04
 0.75126899951944E+03 0.41677405972911E+03 0.31225430548583E+03 0.41368832615817E+03 0.10985433726764E+04
 0.31958933356956E+03 0.30683437943881E+03 0.31745015288038E+03 0.10933654603209E+04 0.41677405972911E+03
 0.31225430548583E+03 0.41368832615817E+03 0.10985433726764E+04 0.31958933356956E+03 0.30683437943881E+03
 0.31745015288038E+03 0.10933654603209E+04 0.69931933403996E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41063105120733E+03 0.12946098432096E+01
 0.12946098432096E+01 0.37255923362306E-01 0.12271092276095E+01 0.29215965606193E+03 0.30481861698678E+03
 0.29446266514489E+03 0.29438382672951E+03 0.23000000000000E+00 0.00000000000000E+00 0.22820190251680E+00
 0.00000000000000E+00 0.12003762667202E+02 0.99979011745999E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29216215603722E+03 0.30388526764747E+03
 0.29215560726377E+03 0.29258949905645E+03 0.29215000000003E+03 0.29215000000003E+03 0.29215563372001E+03
 0.29258953356974E+03 0.29215000000003E+03 0.29215000000003E+03 0.29215560726377E+03 0.29258949905645E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29215563372001E+03 0.29258953356974E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29237383374845E+03 0.29215000000004E+03 0.13405824128158E+01 0.13398282788553E+01
 0.51560838953067E+00 0.88146489103805E+02 0.87628302672327E+02 0.14565142781412E+01 -.78472398936423E+00
 0.14578412032544E+01 0.10289320946675E+03 0.14598470900702E+01 -.77256773539964E+00 0.14611630648998E+01
 0.10290503384997E+03 0.14565142781412E+01 -.78472398936423E+00 0.14578412032545E+01 0.10289320946675E+03
 0.14598470900702E+01 -.77256773539964E+00 0.14611630648997E+01 0.10290503384997E+03 0.27450807975615E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31652981446366E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.18060731173721E+00 0.00000000000000E+00 0.00000000000000E+00 0.18060731173721E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17015134142872E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17015134142872E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17334555688585E+00 0.20889843832217E+00 0.29215965606193E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     60.00268661
 0.20748706795593E+01 0.29223812123314E+03 0.39241532976758E+03 0.32313041218485E+03 0.31720998775065E+03
 0.22999999996925E+00 0.00000000000000E+00 0.21673660466284E+00 0.00000000000000E+00 0.51538051311270E+01
 0.99974930961969E-03 0.21190597508349E+00 0.80000000000000E+04 0.30000000000000E+04 0.37752592850900E+02
 0.14157222319087E+02 0.29429546108365E+03 0.29215000000005E+03 0.29492990262548E+03 0.29969732194978E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29430565076834E+03 0.29966359499766E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29492990262548E+03 0.29969732194978E+03 0.29215000000003E+03 0.29215000000003E+03
 0.29430565076834E+03 0.29966359499766E+03 0.29215000000003E+03 0.29215000000003E+03 0.31149588776178E+03
 0.29217677386480E+03 0.53545751549658E+03 0.53066300920581E+03 0.44557570514825E+03 0.13261374996041E+04
 0.87833391593014E+03 0.46422657530180E+03 0.40629746164144E+03 0.45872101083607E+03 0.12737631425427E+04
 0.36473299882067E+03 0.40019209640373E+03 0.36083671027036E+03 0.12680417216667E+04 0.46422657530180E+03
 0.40629746164144E+03 0.45872101083607E+03 0.12737631425427E+04 0.36473299882067E+03 0.40019209640373E+03
 0.36083671027036E+03 0.12680417216667E+04 0.85499737774810E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42683681246776E+03 0.18390620811890E+01
 0.18390620811890E+01 0.77258491112390E-01 0.10490881382687E+01 0.29215451100194E+03 0.30967088690506E+03
 0.29742007211478E+03 0.29720815921939E+03 0.23000000000000E+00 0.00000000000000E+00 0.22728912788735E+00
 0.00000000000000E+00 0.55502139083992E+01 0.99974403028978E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29217668637402E+03 0.31155954487771E+03
 0.29216127444050E+03 0.29288710608241E+03 0.29215000000003E+03 0.29215000000003E+03 0.29216127042610E+03
 0.29288722120066E+03 0.29215000000003E+03 0.29215000000003E+03 0.29216127444050E+03 0.29288710608241E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29216127042610E+03 0.29288722120066E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29260458511259E+03 0.29215000000005E+03 0.22425764628722E+01 0.22310321287961E+01
 0.46475496066771E-01 0.12929326252042E+03 0.12924655464687E+03 0.22193346447972E+01 -.13773409759299E+01
 0.22167123395403E+01 0.12667880535178E+03 0.22122707300844E+01 -.13489382340280E+01 0.22096503927080E+01
 0.12670618913124E+03 0.22193346447972E+01 -.13773409759299E+01 0.22167123395402E+01 0.12667880535178E+03
 0.22122707300844E+01 -.13489382340281E+01 0.22096503927081E+01 0.12670618913124E+03 0.43455526358005E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32423979486759E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12843018797055E+00 0.00000000000000E+00 0.00000000000000E+00 0.12843018797055E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16603664759509E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16603664759509E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17315971529961E+00 0.20866486170471E+00 0.29215451100194E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     70.00403350
 0.18051400585924E+01 0.29231929367522E+03 0.40313341013420E+03 0.33645507657499E+03 0.32825752809324E+03
 0.22999999996709E+00 0.00000000000000E+00 0.21427400096635E+00 0.00000000000000E+00 0.32167898225050E+00
 0.99942403329842E-03 0.24569201025979E+00 0.80000000000000E+04 0.30000000000000E+04 0.32561091390563E+02
 0.12210409271461E+02 0.29510539674988E+03 0.29215000000007E+03 0.29583985640201E+03 0.30223415870000E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29505153497056E+03 0.30219168839584E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29583985640201E+03 0.30223415870000E+03 0.29215000000003E+03 0.29215000000003E+03
 0.29505153497056E+03 0.30219168839584E+03 0.29215000000003E+03 0.29215000000003E+03 0.31765856351296E+03
 0.29220036260466E+03 0.62955886208932E+03 0.62238009059887E+03 0.47448148633943E+03 0.14124306664371E+04
 0.93557677266593E+03 0.51470123776913E+03 0.48758996691102E+03 0.50680565579263E+03 0.13955214557593E+04
 0.41121331839136E+03 0.48167253869147E+03 0.40554806503525E+03 0.13900735367360E+04 0.51470123776913E+03
 0.48758996691102E+03 0.50680565579263E+03 0.13955214557593E+04 0.41121331839136E+03 0.48167253869147E+03
 0.40554806503525E+03 0.13900735367360E+04 0.98967266621802E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44013314441208E+03 0.16435164197399E+01
 0.16435164197399E+01 0.11726387867205E+00 0.90670625397226E+00 0.29215179790664E+03 0.31309430547096E+03
 0.30043517041601E+03 0.30009115279732E+03 0.23000000000000E+00 0.00000000000000E+00 0.22648053471512E+00
 0.00000000000000E+00 0.17755008583630E+01 0.99971605909022E-03 0.14183570486891E-01 0.80000000000000E+04
 0.30000000000000E+04 0.56403287221606E+03 0.21151232708102E+03 0.29220024487127E+03 0.31770829757303E+03
 0.29216942638225E+03 0.29316228161379E+03 0.29215000000003E+03 0.29215000000003E+03 0.29216932937983E+03
 0.29316249896810E+03 0.29215000000003E+03 0.29215000000003E+03 0.29216942638225E+03 0.29316228161379E+03
 0.29215000000003E+03 0.29215000000003E+03 0.29216932937983E+03 0.29316249896810E+03 0.29215000000003E+03
 0.29215000000003E+03 0.29284003744866E+03 0.29215000000007E+03 0.42833498363819E+01 0.42526869309778E+01
 0.11555842175063E+01 0.16107349333009E+03 0.15991213119149E+03 0.37052662978467E+01 -.28159746944965E+00
 0.36961803858848E+01 0.14503855994531E+03 0.36690600378812E+01 -.23910179060475E+00 0.36600375253828E+01
 0.14507919406086E+03 0.37052662978467E+01 -.28159746944969E+00 0.36961803858848E+01 0.14503855994531E+03
 0.36690600378811E+01 -.23910179060469E+00 0.36600375253827E+01 0.14507919406086E+03 0.57557954307396E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33010200563894E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.88312851203901E-01 0.00000000000000E+00 0.00000000000000E+00 0.88312851203901E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16500901782425E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16500901782425E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17305092233260E+00 0.20247305923935E+00 0.30088846902997E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     80.01487344
 0.15928795054058E+01 0.29243497551015E+03 0.41158263713356E+03 0.34832001436124E+03 0.33838057662389E+03
 0.22999999996376E+00 0.00000000000000E+00 0.21187827565365E+00 0.00000000000000E+00 -.28651308438525E+01
 0.99899725902874E-03 0.27784210741049E+00 0.80000000000000E+04 0.30000000000000E+04 0.28793331847936E+02
 0.10797499442976E+02 0.29595281715336E+03 0.29215000000008E+03 0.29672904177345E+03 0.30463775345265E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29579599161064E+03 0.30458959618046E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29672904177345E+03 0.30463775345265E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29579599161064E+03 0.30458959618046E+03 0.29215000000004E+03 0.29215000000004E+03 0.32320819954313E+03
 0.29225214503401E+03 0.72424465667112E+03 0.71445492488667E+03 0.51303531478630E+03 0.14895030035600E+04
 0.97390251219972E+03 0.56492333445431E+03 0.56941326828052E+03 0.55459977358775E+03 0.15087414229633E+04
 0.45824862160220E+03 0.56388669870321E+03 0.45075650558750E+03 0.15037377568867E+04 0.56492333445431E+03
 0.56941326828052E+03 0.55459977358775E+03 0.15087414229633E+04 0.45824862160220E+03 0.56388669870321E+03
 0.45075650558750E+03 0.15037377568867E+04 0.11164321749398E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45327476093553E+03 0.15045470575880E+01
 0.15045470575880E+01 0.15730723843504E+00 0.80546830371055E+00 0.29215085501504E+03 0.31588337787849E+03
 0.30313951458943E+03 0.30268021514297E+03 0.23000000000000E+00 0.00000000000000E+00 0.22566708754214E+00
 0.00000000000000E+00 -.51097626949451E+00 0.99969671859139E-03 0.40759737575272E-01 0.80000000000000E+04
 0.30000000000000E+04 0.19627211743516E+03 0.73602044038185E+02 0.29225189407892E+03 0.32324553316741E+03
 0.29218512066658E+03 0.29344412676278E+03 0.29215000000004E+03 0.29215000000004E+03 0.29218474466353E+03
 0.29344445520122E+03 0.29215000000004E+03 0.29215000000004E+03 0.29218512066658E+03 0.29344412676278E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29218474466353E+03 0.29344445520122E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29308568832542E+03 0.29215000000008E+03 0.82482611565404E+01 0.81619081000309E+01
 0.48697692989883E+01 0.19017525098446E+03 0.18528113283898E+03 0.64369526798572E+01 0.34144486150994E+01
 0.64153371461614E+01 0.16411255749283E+03 0.63418290310341E+01 0.34686841105297E+01 0.63205146668758E+01
 0.16416400929619E+03 0.64369526798572E+01 0.34144486150994E+01 0.64153371461614E+01 0.16411255749283E+03
 0.63418290310340E+01 0.34686841105297E+01 0.63205146668758E+01 0.16416400929619E+03 0.71427411586165E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33486261040410E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.52506064547255E-01 0.00000000000000E+00 0.00000000000000E+00 0.52506064547255E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16490846771369E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16490846771369E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17298498745684E+00 0.20031662058774E+00 0.30400483929466E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
     90.14537801
 0.14190392326372E+01 0.29259153836316E+03 0.41831772325134E+03 0.35884759360930E+03 0.34765564471755E+03
 0.22999999997651E+00 0.00000000000000E+00 0.20947320445271E+00 0.00000000000000E+00 -.46717985547398E+01
 0.99844490190899E-03 0.30995868981334E+00 0.80000000000000E+04 0.30000000000000E+04 0.25809891004565E+02
 0.96787091267118E+01 0.29684949731385E+03 0.29215000000009E+03 0.29762629837268E+03 0.30696880256369E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29656103984368E+03 0.30691692939260E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29762629837268E+03 0.30696880256369E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29656103984368E+03 0.30691692939260E+03 0.29215000000004E+03 0.29215000000004E+03 0.32841306020441E+03
 0.29232940331343E+03 0.81886124075834E+03 0.80624367929313E+03 0.55475043968265E+03 0.15501481050619E+04
 0.99262391318078E+03 0.61457861535068E+03 0.64933156300665E+03 0.60176874773317E+03 0.16061516699869E+04
 0.50530784571349E+03 0.64424973481181E+03 0.49592032461778E+03 0.16016237183353E+04 0.61457861535068E+03
 0.64933156300665E+03 0.60176874773317E+03 0.16061516699869E+04 0.50530784571349E+03 0.64424973481181E+03
 0.49592032461778E+03 0.16016237183353E+04 0.12315843496611E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46492237932881E+03 0.13716167001794E+01
 0.13716167001794E+01 0.19782925669011E+00 0.71692059454525E+00 0.29215208372319E+03 0.31830251152965E+03
 0.30580399136257E+03 0.30524391158341E+03 0.23000000000000E+00 0.00000000000000E+00 0.22480354318038E+00
 0.00000000000000E+00 -.15464542206109E+01 0.99968229426989E-03 0.64509266731774E-01 0.80000000000000E+04
 0.30000000000000E+04 0.12401319074457E+03 0.46504946529215E+02 0.29232896872921E+03 0.32844491356411E+03
 0.29220777730898E+03 0.29373381247705E+03 0.29215000000004E+03 0.29215000000004E+03 0.29220697911339E+03
 0.29373425513839E+03 0.29215000000004E+03 0.29215000000004E+03 0.29220777730898E+03 0.29373381247705E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29220697911339E+03 0.29373425513839E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29334174642494E+03 0.29215000000009E+03 0.13084423754216E+02 0.12902233678990E+02
 0.96118280261627E+01 0.21710787654000E+03 0.20744798937370E+03 0.97429254583234E+01 0.81089012609413E+01
 0.97019609507192E+01 0.18209851286251E+03 0.95810111070032E+01 0.81727961128690E+01 0.95407923634177E+01
 0.18215867702571E+03 0.97429254583233E+01 0.81089012609413E+01 0.97019609507191E+01 0.18209851286251E+03
 0.95810111070031E+01 0.81727961128690E+01 0.95407923634177E+01 0.18215867702571E+03 0.85107578071497E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33899975342291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.21788060042318E-01 0.10875900396036E-02 0.00000000000000E+00 0.21788060042318E-01 0.10875900396036E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16540626404820E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16540626404820E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17287146324901E+00 0.19938056939344E+00 0.30522851498766E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    100.02736400
 0.12788890912547E+01 0.29278531009079E+03 0.42330218389915E+03 0.36766331518642E+03 0.35570625896463E+03
 0.22999999996802E+00 0.00000000000000E+00 0.20716750626977E+00 0.00000000000000E+00 -.57088483761214E+01
 0.99777389653832E-03 0.34090862304199E+00 0.80000000000000E+04 0.30000000000000E+04 0.23466698872603E+02
 0.88000120772261E+01 0.29775313638778E+03 0.29215000000009E+03 0.29849867066755E+03 0.30914248167761E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29731538258153E+03 0.30908834151593E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29849867066755E+03 0.30914248167761E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29731538258153E+03 0.30908834151593E+03 0.29215000000004E+03 0.29215000000004E+03 0.33308491255214E+03
 0.29242843609546E+03 0.90698007480947E+03 0.89149654522750E+03 0.59311044408812E+03 0.15928544191621E+04
 0.99677842285356E+03 0.66021677446115E+03 0.72052271428647E+03 0.64499907850545E+03 0.16860497383433E+04
 0.54893687847737E+03 0.71586775656963E+03 0.53769949673586E+03 0.16819656616190E+04 0.66021677446114E+03
 0.72052271428647E+03 0.64499907850545E+03 0.16860497383433E+04 0.54893687847737E+03 0.71586775656964E+03
 0.53769949673586E+03 0.16819656616190E+04 0.13300561349076E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47488292233173E+03 0.12947465606323E+01
 0.12947465606323E+01 0.23735720068152E+00 0.63933034769695E+00 0.29215565080444E+03 0.32029394630573E+03
 0.30830083547477E+03 0.30766420296607E+03 0.23000000000000E+00 0.00000000000000E+00 0.22392334712210E+00
 0.00000000000000E+00 -.19569989468155E+01 0.99966603671139E-03 0.86325620827391E-01 0.80000000000000E+04
 0.30000000000000E+04 0.92672371461957E+02 0.34752139298234E+02 0.29242783651035E+03 0.33311548070275E+03
 0.29223633387274E+03 0.29401911396665E+03 0.29215000000004E+03 0.29215000000004E+03 0.29223500975440E+03
 0.29401966560759E+03 0.29215000000004E+03 0.29215000000004E+03 0.29223633387274E+03 0.29401911396665E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29223500975440E+03 0.29401966560759E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29359508228636E+03 0.29215000000009E+03 0.18501470247583E+02 0.18177831308984E+02
 0.15040695573877E+02 0.24028450158562E+03 0.22516860253387E+03 0.13424289424635E+02 0.13453686370590E+02
 0.13357250077876E+02 0.19824126110374E+03 0.13194044362658E+02 0.13524830639833E+02 0.13128402377811E+02
 0.19830776988975E+03 0.13424289424635E+02 0.13453686370590E+02 0.13357250077876E+02 0.19824126110374E+03
 0.13194044362658E+02 0.13524830639833E+02 0.13128402377811E+02 0.19830776988975E+03 0.97855821040399E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34141549894010E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.56600271479945E-02 0.15982880806913E-01 0.00000000000000E+00 0.56600271479945E-02 0.15982880806913E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16644945899600E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16644945899600E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17285612952759E+00 0.19833078622403E+00 0.30681565698910E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    110.29884876
 0.11661159553460E+01 0.29302926013925E+03 0.42764804026363E+03 0.37532100446621E+03 0.36285248108357E+03
 0.22999999997174E+00 0.00000000000000E+00 0.20470418102231E+00 0.00000000000000E+00 -.67877959430091E+01
 0.99693262247633E-03 0.37398466052320E+00 0.80000000000000E+04 0.30000000000000E+04 0.21391251686120E+02
 0.80217193822950E+01 0.29871536329951E+03 0.29215000000009E+03 0.29940449681749E+03 0.31132624791960E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29810821327026E+03 0.31127075779957E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29940449681749E+03 0.31132624791960E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29810821327026E+03 0.31127075779957E+03 0.29215000000004E+03 0.29215000000004E+03 0.33762591686957E+03
 0.29255544164730E+03 0.99274832376846E+03 0.97422912224091E+03 0.63178854950571E+03 0.16296552650041E+04
 0.99470777275084E+03 0.70491188781376E+03 0.78995555568147E+03 0.68724197987996E+03 0.17620144683763E+04
 0.59205091858710E+03 0.78571291701827E+03 0.57891869684283E+03 0.17583513095124E+04 0.70491188781376E+03
 0.78995555568147E+03 0.68724197987996E+03 0.17620144683763E+04 0.59205091858710E+03 0.78571291701827E+03
 0.57891869684283E+03 0.17583513095124E+04 0.14208362391418E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48448455779905E+03 0.12947545088661E+01
 0.12947545088661E+01 0.27844313970499E+00 0.56984966391463E+00 0.29216244699783E+03 0.32194277283886E+03
 0.31062923372434E+03 0.30994079469288E+03 0.23000000000000E+00 0.00000000000000E+00 0.22298788203115E+00
 0.00000000000000E+00 -.25061967329980E+01 0.99963736254712E-03 0.10819415772198E+00 0.80000000000000E+04
 0.30000000000000E+04 0.73941145884766E+02 0.27727929706787E+02 0.29255468292327E+03 0.33765763531648E+03
 0.29227257985539E+03 0.29431380368632E+03 0.29215000000004E+03 0.29215000000004E+03 0.29227061761890E+03
 0.29431446114441E+03 0.29215000000004E+03 0.29215000000004E+03 0.29227257985539E+03 0.29431380368632E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29227061761890E+03 0.29431446114441E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29385708851828E+03 0.29215000000009E+03 0.24555655230464E+02 0.24028914289854E+02
 0.21173817292360E+02 0.26074772267250E+03 0.23946803629368E+03 0.17545956869267E+02 0.19461370553080E+02
 0.17444239902773E+02 0.21274465444560E+03 0.17246706238085E+02 0.19537633754320E+02 0.17147290069214E+02
 0.21281540561913E+03 0.17545956869267E+02 0.19461370553080E+02 0.17444239902773E+02 0.21274465444560E+03
 0.17246706238086E+02 0.19537633754320E+02 0.17147290069214E+02 0.21281540561913E+03 0.10990314148392E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34324395298001E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.42625429817355E-01 0.00000000000000E+00 0.00000000000000E+00 0.42625429817355E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16745713464837E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16745713464837E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17283783936961E+00 0.19339046435685E+00 0.31461852081150E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    120.51858970
 0.10870197002140E+01 0.29330902715682E+03 0.43145617275667E+03 0.38139994982488E+03 0.36855794306327E+03
 0.22999999996735E+00 0.00000000000000E+00 0.20213522486523E+00 0.00000000000000E+00 -.83517265292338E+01
 0.99596634419785E-03 0.40841928737280E+00 0.80000000000000E+04 0.30000000000000E+04 0.19587713527098E+02
 0.73453925726618E+01 0.29967828759009E+03 0.29215000000009E+03 0.30029845637488E+03 0.31342976354391E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29889859480154E+03 0.31337360695898E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30029845637488E+03 0.31342976354391E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29889859480154E+03 0.31337360695898E+03 0.29215000000004E+03 0.29215000000004E+03 0.34189345549430E+03
 0.29270069467904E+03 0.10702801256225E+04 0.10487597881353E+04 0.66914405984140E+03 0.16622834845321E+04
 0.98979370439149E+03 0.74625462739976E+03 0.85544887010744E+03 0.72623729221738E+03 0.18302651132684E+04
 0.63225914878364E+03 0.85157269110085E+03 0.61730553731947E+03 0.18269697896368E+04 0.74625462739976E+03
 0.85544887010744E+03 0.72623729221738E+03 0.18302651132684E+04 0.63225914878364E+03 0.85157269110085E+03
 0.61730553731946E+03 0.18269697896368E+04 0.14988122669685E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49626920469898E+03 0.12947660300125E+01
 0.12947660300125E+01 0.31932210345535E+00 0.52621043248524E+00 0.29217321173570E+03 0.32328159025128E+03
 0.31236855470955E+03 0.31164139371946E+03 0.23000000000000E+00 0.00000000000000E+00 0.22201699483790E+00
 0.00000000000000E+00 -.36657964271699E+01 0.99958908811998E-03 0.12993496142249E+00 0.80000000000000E+04
 0.30000000000000E+04 0.61569264441365E+02 0.23088474165512E+02 0.29269980541520E+03 0.34192777017978E+03
 0.29231368752052E+03 0.29460300166443E+03 0.29215000000004E+03 0.29215000000004E+03 0.29231102463513E+03
 0.29460375671572E+03 0.29215000000004E+03 0.29215000000004E+03 0.29231368752052E+03 0.29460300166443E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29231102463513E+03 0.29460375671572E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29411240766007E+03 0.29215000000009E+03 0.30496857077375E+02 0.29716810878267E+02
 0.27308169020413E+02 0.27822120579562E+03 0.25077649593011E+03 0.21638981385085E+02 0.25456433010972E+02
 0.21498317746494E+02 0.22584936362576E+03 0.21273142064695E+02 0.25536643507069E+02 0.21135863140480E+02
 0.22592323952857E+03 0.21638981385085E+02 0.25456433010972E+02 0.21498317746494E+02 0.22584936362576E+03
 0.21273142064695E+02 0.25536643507069E+02 0.21135863140480E+02 0.22592323952857E+03 0.12014409961835E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34483712265457E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.72661953039818E-01 0.00000000000000E+00 0.00000000000000E+00 0.72661953039818E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16831246372771E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16831246372771E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17280216139425E+00 0.18816711958096E+00 0.32328159025128E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    130.25427892
 0.10374099986732E+01 0.29360365159737E+03 0.43466746373777E+03 0.38588712734930E+03 0.37273926077774E+03
 0.22999999994514E+00 0.00000000000000E+00 0.19957332849331E+00 0.00000000000000E+00 -.10044922611978E+02
 0.99495028730828E-03 0.44273426947396E+00 0.80000000000000E+04 0.30000000000000E+04 0.18069529628925E+02
 0.67760736108468E+01 0.30059323333761E+03 0.29215000000009E+03 0.30114250976389E+03 0.31538698877119E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29965118709157E+03 0.31533065875015E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30114250976389E+03 0.31538698877119E+03 0.29215000000004E+03 0.29215000000004E+03
 0.29965118709157E+03 0.31533065875015E+03 0.29215000000004E+03 0.29215000000004E+03 0.34575974581273E+03
 0.29285715547970E+03 0.11360003010513E+04 0.11116673103467E+04 0.70285006128229E+03 0.16876114056753E+04
 0.98124709408664E+03 0.78236838464233E+03 0.91417349852513E+03 0.76020866855118E+03 0.18923601222770E+04
 0.66760235926152E+03 0.91060710562573E+03 0.65097989936756E+03 0.18893729091251E+04 0.78236838464233E+03
 0.91417349852513E+03 0.76020866855118E+03 0.18923601222770E+04 0.66760235926152E+03 0.91060710562573E+03
 0.65097989936756E+03 0.18893729091251E+04 0.15628077022334E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50504056132607E+03 0.12947785037112E+01
 0.12947785037112E+01 0.35826486032758E+00 0.49062553259312E+00 0.29218859055120E+03 0.32434512169812E+03
 0.31382724488458E+03 0.31307538549198E+03 0.23000000000000E+00 0.00000000000000E+00 0.22104210219990E+00
 0.00000000000000E+00 -.50750438252049E+01 0.99952256941800E-03 0.15110630660372E+00 0.80000000000000E+04
 0.30000000000000E+04 0.52942859764155E+02 0.19853572411558E+02 0.29285616861059E+03 0.34579700994853E+03
 0.29235744258512E+03 0.29487530999180E+03 0.29215000000004E+03 0.29215000000004E+03 0.29235405674583E+03
 0.29487615018264E+03 0.29215000000004E+03 0.29215000000004E+03 0.29235744258512E+03 0.29487530999180E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29235405674583E+03 0.29487615018264E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29435153646553E+03 0.29215000000009E+03 0.36314327671526E+02 0.35244202686913E+02
 0.33409081070560E+02 0.29307792989375E+03 0.25950180341784E+03 0.25680813044292E+02 0.31394206497949E+02
 0.25500762123630E+02 0.23710240427706E+03 0.25253474239456E+02 0.31476654077935E+02 0.25078004748854E+02
 0.23717780067191E+03 0.25680813044292E+02 0.31394206497949E+02 0.25500762123630E+02 0.23710240427706E+03
 0.25253474239457E+02 0.31476654077935E+02 0.25078004748855E+02 0.23717780067191E+03 0.12915409443736E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34616498581354E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97345428090920E-01 0.00000000000000E+00 0.00000000000000E+00 0.97345428090920E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16902046702606E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16902046702606E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17275985375871E+00 0.18750159011902E+00 0.32434512169812E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    140.04236078
 0.10071197122335E+01 0.29392061715835E+03 0.43743338927740E+03 0.38925520869128E+03 0.37582912477266E+03
 0.22999999996051E+00 0.00000000000000E+00 0.19691040501373E+00 0.00000000000000E+00 -.11766887061855E+02
 0.99386043552166E-03 0.47848085573761E+00 0.80000000000000E+04 0.30000000000000E+04 0.16719582202861E+02
 0.62698433260727E+01 0.30149734692345E+03 0.29215000000009E+03 0.30197615757414E+03 0.31730549175351E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30039917588558E+03 0.31724929816774E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30197615757414E+03 0.31730549175351E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30039917588558E+03 0.31724929816774E+03 0.29215000000004E+03 0.29215000000004E+03 0.34943995635890E+03
 0.29303340166651E+03 0.11933062741196E+04 0.11662405366886E+04 0.73339942416304E+03 0.17055209099452E+04
 0.96845448866138E+03 0.81485881889730E+03 0.96822510547325E+03 0.79065846970753E+03 0.19487987153499E+04
 0.69955455579582E+03 0.96493636798043E+03 0.68133942063180E+03 0.19460853702786E+04 0.81485881889730E+03
 0.96822510547325E+03 0.79065846970753E+03 0.19487987153499E+04 0.69955455579582E+03 0.96493636798043E+03
 0.68133942063180E+03 0.19460853702786E+04 0.16159844229263E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51134623074371E+03 0.12947911896445E+01
 0.12947911896445E+01 0.39741718779208E+00 0.45654372671717E+00 0.29221075831765E+03 0.32524080031998E+03
 0.31518769467373E+03 0.31442349624926E+03 0.23000000000000E+00 0.00000000000000E+00 0.22000795744700E+00
 0.00000000000000E+00 -.66009005466998E+01 0.99943168662142E-03 0.17303821275312E+00 0.80000000000000E+04
 0.30000000000000E+04 0.46232562580925E+02 0.17337210967847E+02 0.29303230801341E+03 0.34947999116214E+03
 0.29240574052389E+03 0.29514428852735E+03 0.29215000000004E+03 0.29215000000004E+03 0.29240158896049E+03
 0.29514520460797E+03 0.29215000000004E+03 0.29215000000004E+03 0.29240574052389E+03 0.29514428852735E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29240158896049E+03 0.29514520460797E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29458716899817E+03 0.29215000000009E+03 0.42344000576698E+02 0.40932941270722E+02
 0.39798431983844E+02 0.30659816020979E+03 0.26660073606603E+03 0.29890943493924E+02 0.37597938392900E+02
 0.29671860480505E+02 0.24735822450262E+03 0.29404446478065E+02 0.37681238075628E+02 0.29191278938000E+02
 0.24743383915973E+03 0.29890943493924E+02 0.37597938392900E+02 0.29671860480505E+02 0.24735822450262E+03
 0.29404446478065E+02 0.37681238075628E+02 0.29191278938000E+02 0.24743383915973E+03 0.13772702245687E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34733222749204E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11836538331766E+00 0.00000000000000E+00 0.00000000000000E+00 0.11836538331766E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16986331593645E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16986331593645E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17271463836775E+00 0.18693347592723E+00 0.32524080031998E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    150.13300330
 0.99048789927197E+00 0.29426300559942E+03 0.43973608604215E+03 0.39170631075937E+03 0.37803327904196E+03
 0.22999999999327E+00 0.00000000000000E+00 0.19412388273810E+00 0.00000000000000E+00 -.13352727549919E+02
 0.99268849488815E-03 0.51609170309955E+00 0.80000000000000E+04 0.30000000000000E+04 0.15501121122377E+02
 0.58129204208914E+01 0.30241075589594E+03 0.29215000000009E+03 0.30281998143462E+03 0.31922855449850E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30116048900813E+03 0.31917278473478E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30281998143462E+03 0.31922855449850E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30116048900813E+03 0.31917278473478E+03 0.29215000000004E+03 0.29215000000004E+03 0.35300123884408E+03
 0.29323885901647E+03 0.12427827453937E+04 0.12129982412613E+04 0.75969454050222E+03 0.17140952941947E+04
 0.95060228098993E+03 0.84378262967258E+03 0.10169780392306E+04 0.81758400910796E+03 0.19972577711721E+04
 0.72812527933550E+03 0.10139446295208E+04 0.70834675628931E+03 0.19947934788108E+04 0.84378262967258E+03
 0.10169780392306E+04 0.81758400910796E+03 0.19972577711721E+04 0.72812527933550E+03 0.10139446295208E+04
 0.70834675628931E+03 0.19947934788108E+04 0.16581145777391E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51563225003143E+03 0.12948028730021E+01
 0.12948028730021E+01 0.43777975788296E+00 0.42322842720440E+00 0.29224295802883E+03 0.32600474592176E+03
 0.31647878032874E+03 0.31571372603959E+03 0.23000000000000E+00 0.00000000000000E+00 0.21888279065729E+00
 0.00000000000000E+00 -.80579961680971E+01 0.99930719127004E-03 0.19645486573394E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40721821626117E+02 0.15270683109794E+02 0.29323763753225E+03 0.35304354632817E+03
 0.29246085922533E+03 0.29541824049387E+03 0.29215000000004E+03 0.29215000000004E+03 0.29245587961542E+03
 0.29541922195763E+03 0.29215000000004E+03 0.29215000000004E+03 0.29246085922533E+03 0.29541824049387E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29245587961541E+03 0.29541922195763E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29482727471790E+03 0.29215000000009E+03 0.48676382475135E+02 0.46855855412532E+02
 0.46591675306242E+02 0.31913888546878E+03 0.27231425178601E+03 0.34377734765294E+02 0.44174085131005E+02
 0.34121840967217E+02 0.25690423230500E+03 0.33834469563369E+02 0.44256418676674E+02 0.33585984292214E+02
 0.25697834145783E+03 0.34377734765294E+02 0.44174085131005E+02 0.34121840967216E+02 0.25690423230500E+03
 0.33834469563370E+02 0.44256418676674E+02 0.33585984292216E+02 0.25697834145783E+03 0.14604067999217E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34835346419055E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13539410096391E+00 0.00000000000000E+00 0.00000000000000E+00 0.13539410096391E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17077590332720E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17077590332720E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17267177703474E+00 0.18644646045531E+00 0.32600474592176E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    160.28571893
 0.98332115860583E+00 0.29461792802809E+03 0.44147289093560E+03 0.39333769351119E+03 0.37947379250388E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19133853753538E+00 0.00000000000000E+00 -.14650620758018E+02
 0.99147991391475E-03 0.55399978825734E+00 0.80000000000000E+04 0.30000000000000E+04 0.14440438732233E+02
 0.54151645245873E+01 0.30329394417503E+03 0.29215000000009E+03 0.30363962405351E+03 0.32107764242865E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30190246132377E+03 0.32102246163318E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30363962405351E+03 0.32107764242865E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30190246132377E+03 0.32102246163318E+03 0.29215000000004E+03 0.29215000000004E+03 0.35631234836491E+03
 0.29346784668225E+03 0.12830155298520E+04 0.12506663972108E+04 0.77977168938773E+03 0.17121512620327E+04
 0.92848071419802E+03 0.86796206117991E+03 0.10577355244314E+04 0.83989733149269E+03 0.20342860173077E+04
 0.75211968606592E+03 0.10549316325729E+04 0.73088457732664E+03 0.20320434714623E+04 0.86796206117991E+03
 0.10577355244314E+04 0.83989733149269E+03 0.20342860173077E+04 0.75211968606592E+03 0.10549316325729E+04
 0.73088457732664E+03 0.20320434714623E+04 0.16878169519319E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51820614172149E+03 0.12948124351571E+01
 0.12948124351571E+01 0.47839062037754E+00 0.39158927347905E+00 0.29228733222544E+03 0.32662893569147E+03
 0.31766373332389E+03 0.31690853778166E+03 0.23000000000000E+00 0.00000000000000E+00 0.21769035317249E+00
 0.00000000000000E+00 -.92844709658105E+01 0.99914338004565E-03 0.22090971676162E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36213889172800E+02 0.13580208439800E+02 0.29346648564379E+03 0.35635615569593E+03
 0.29252047226262E+03 0.29568762194828E+03 0.29215000000004E+03 0.29215000000004E+03 0.29251464475104E+03
 0.29568865679241E+03 0.29215000000004E+03 0.29215000000004E+03 0.29252047226262E+03 0.29568762194828E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29251464475104E+03 0.29568865679241E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29506286206901E+03 0.29215000000009E+03 0.55069839802276E+02 0.52786255443180E+02
 0.53515012662666E+02 0.33040386965280E+03 0.27662128192682E+03 0.38953140279781E+02 0.50867280117862E+02
 0.38670193102053E+02 0.26552898923815E+03 0.38357459479080E+02 0.50947538881021E+02 0.38083459747140E+02
 0.26560059084053E+03 0.38953140279781E+02 0.50867280117862E+02 0.38670193102053E+02 0.26552898923815E+03
 0.38357459479082E+02 0.50947538881021E+02 0.38083459747142E+02 0.26560059084053E+03 0.15384520100190E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34920403325983E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14815132417167E+00 0.00000000000000E+00 0.00000000000000E+00 0.14815132417167E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17181941343586E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17181941343586E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17263591745754E+00 0.18604926095418E+00 0.32662893569147E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    170.14952628
 0.98157794700538E+00 0.29496862708553E+03 0.44265686327719E+03 0.39433435805124E+03 0.38034739304710E+03
 0.22999999999179E+00 0.00000000000000E+00 0.18870078879271E+00 0.00000000000000E+00 -.15578552426992E+02
 0.99029203648958E-03 0.59023849732508E+00 0.80000000000000E+04 0.30000000000000E+04 0.13553843126559E+02
 0.50826911724596E+01 0.30411319374815E+03 0.29215000000009E+03 0.30440360086883E+03 0.32277877683071E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30259565628640E+03 0.32272428234263E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30440360086883E+03 0.32277877683071E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30259565628640E+03 0.32272428234263E+03 0.29215000000004E+03 0.29215000000004E+03 0.35925569591268E+03
 0.29371289780065E+03 0.13139368834441E+04 0.12792762756940E+04 0.79311170626791E+03 0.17012468555289E+04
 0.90416959072968E+03 0.88698243722708E+03 0.10892694867310E+04 0.85724661848519E+03 0.20590991697854E+04
 0.77109205463278E+03 0.10866654032341E+04 0.74855842670806E+03 0.20570477665024E+04 0.88698243722708E+03
 0.10892694867310E+04 0.85724661848519E+03 0.20590991697854E+04 0.77109205463278E+03 0.10866654032341E+04
 0.74855842670806E+03 0.20570477665024E+04 0.17059765390011E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51955420745696E+03 0.12948192717465E+01
 0.12948192717465E+01 0.51784584980275E+00 0.36265841896049E+00 0.29234449132498E+03 0.32710950954876E+03
 0.31870429185265E+03 0.31796759999488E+03 0.23000000000000E+00 0.00000000000000E+00 0.21647703309245E+00
 0.00000000000000E+00 -.10180992298710E+02 0.99893918526244E-03 0.24552436505159E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32583324259159E+02 0.12218746597185E+02 0.29371140950785E+03 0.35930027364863E+03
 0.29258213213784E+03 0.29594287088459E+03 0.29215000000004E+03 0.29215000000004E+03 0.29257547864427E+03
 0.29594394507403E+03 0.29215000000004E+03 0.29215000000004E+03 0.29258213213784E+03 0.29594287088459E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29257547864427E+03 0.29594394507403E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29528561329476E+03 0.29215000000009E+03 0.61204615285572E+02 0.58424456552548E+02
 0.60237632218841E+02 0.34008135015078E+03 0.27954252977085E+03 0.43419875748756E+02 0.57353068363594E+02
 0.43125655164920E+02 0.27298292393669E+03 0.42778919070255E+02 0.57430135884238E+02 0.42495100714254E+02
 0.27305102405452E+03 0.43419875748756E+02 0.57353068363595E+02 0.43125655164920E+02 0.27298292393669E+03
 0.42778919070256E+02 0.57430135884238E+02 0.42495100714255E+02 0.27305102405452E+03 0.16085223823118E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34987113831577E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15674597266932E+00 0.00000000000000E+00 0.00000000000000E+00 0.15674597266932E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17290640486693E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17290640486693E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17260988677419E+00 0.18574626995250E+00 0.32710950954876E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    180.01333364
 0.98236643019309E+00 0.29532307867863E+03 0.44345365706266E+03 0.39494748789947E+03 0.38089284165637E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18616364852025E+00 0.00000000000000E+00 -.16204699483238E+02
 0.98909735858275E-03 0.62539543520300E+00 0.80000000000000E+04 0.30000000000000E+04 0.12791906607702E+02
 0.47969649778883E+01 0.30489533456982E+03 0.29215000000009E+03 0.30513615666357E+03 0.32438449056067E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30326153536671E+03 0.32433077252254E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30513615666357E+03 0.32438449056067E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30326153536671E+03 0.32433077252254E+03 0.29215000000004E+03 0.29215000000004E+03 0.36193630685939E+03
 0.29398157832061E+03 0.13383233236213E+04 0.13015249422300E+04 0.80126920550002E+03 0.16838496685064E+04
 0.87857411697889E+03 0.90226179666679E+03 0.11137842934994E+04 0.87098683177298E+03 0.20746136747609E+04
 0.78642211339959E+03 0.11113596922015E+04 0.76269951533805E+03 0.20727321880882E+04 0.90226179666680E+03
 0.11137842934994E+04 0.87098683177298E+03 0.20746136747609E+04 0.78642211339959E+03 0.11113596922015E+04
 0.76269951533805E+03 0.20727321880882E+04 0.17159636724149E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52020441266727E+03 0.12948238849701E+01
 0.12948238849701E+01 0.55730107922797E+00 0.33548868621551E+00 0.29241850030069E+03 0.32747877491697E+03
 0.31963722460405E+03 0.31892638413213E+03 0.23000000000000E+00 0.00000000000000E+00 0.21521487445910E+00
 0.00000000000000E+00 -.10798195824719E+02 0.99868027500886E-03 0.27092894786924E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29528037010874E+02 0.11073013879078E+02 0.29397995961344E+03 0.36198114161856E+03
 0.29264755029835E+03 0.29619191498553E+03 0.29215000000004E+03 0.29215000000004E+03 0.29264007718570E+03
 0.29619301529454E+03 0.29215000000004E+03 0.29215000000004E+03 0.29264755029835E+03 0.29619191498553E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29264007718570E+03 0.29619301529454E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29550269008923E+03 0.29215000000009E+03 0.67164991018278E+02 0.63846117004945E+02
 0.66863766891304E+02 0.34854254852284E+03 0.28134446279708E+03 0.47867215797141E+02 0.63730342653656E+02
 0.47582011950326E+02 0.27953733257961E+03 0.47187382106125E+02 0.63803067587153E+02 0.46913899101772E+02
 0.27960089504558E+03 0.47867215797141E+02 0.63730342653656E+02 0.47582011950326E+02 0.27953733257961E+03
 0.47187382106127E+02 0.63803067587153E+02 0.46913899101774E+02 0.27960089504558E+03 0.16725879233754E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35040102260823E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16231881046995E+00 0.00000000000000E+00 0.00000000000000E+00 0.16231881046995E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17400906508309E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17400906508309E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17259216418863E+00 0.18551664187783E+00 0.32747877491697E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    190.33545150
 0.98426351099567E+00 0.29569605890725E+03 0.44400125808791E+03 0.39534412607309E+03 0.38126384661894E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18363440275178E+00 0.00000000000000E+00 -.16620610826370E+02
 0.98784569173283E-03 0.66068573299374E+00 0.80000000000000E+04 0.30000000000000E+04 0.12108631381746E+02
 0.45407367681548E+01 0.30567336010308E+03 0.29215000000009E+03 0.30586782214495E+03 0.32596296259274E+03
 0.29215000000004E+03 0.29215000000004E+03 0.30392732510753E+03 0.32591010329118E+03 0.29215000000004E+03
 0.29215000000004E+03 0.30586782214495E+03 0.32596296259274E+03 0.29215000000004E+03 0.29215000000004E+03
 0.30392732510753E+03 0.32591010329118E+03 0.29215000000004E+03 0.29215000000004E+03 0.36448793091233E+03
 0.29428709040441E+03 0.13585609327596E+04 0.13197155592558E+04 0.80556289053873E+03 0.16613492816125E+04
 0.85175857662106E+03 0.91511864478088E+03 0.11334626401116E+04 0.88237403871163E+03 0.20834566479863E+04
 0.79940647336046E+03 0.11312063434952E+04 0.77455863715136E+03 0.20817330312338E+04 0.91511864478088E+03
 0.11334626401116E+04 0.88237403871163E+03 0.20834566479863E+04 0.79940647336046E+03 0.11312063434952E+04
 0.77455863715136E+03 0.20817330312338E+04 0.17203257435712E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52047721563913E+03 0.12948269492756E+01
 0.12948269492756E+01 0.59858955068243E+00 0.30889503506984E+00 0.29251723543320E+03 0.32776223225522E+03
 0.32050422923564E+03 0.31982662261751E+03 0.23000000000000E+00 0.00000000000000E+00 0.21384834092364E+00
 0.00000000000000E+00 -.11217387450117E+02 0.99833905220278E-03 0.29827222576132E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26821136227420E+02 0.10057926085282E+02 0.29428534154688E+03 0.36453268586431E+03
 0.29271914512640E+03 0.29644471566356E+03 0.29215000000004E+03 0.29215000000004E+03 0.29271083729358E+03
 0.29644582976499E+03 0.29215000000004E+03 0.29215000000004E+03 0.29271914512640E+03 0.29644471566356E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29271083729358E+03 0.29644582976499E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29572257096512E+03 0.29215000000009E+03 0.73125100715687E+02 0.69209463957078E+02
 0.73603651824359E+02 0.35618949279610E+03 0.28221782271262E+03 0.52449478754198E+02 0.70200774113143E+02
 0.52202082377171E+02 0.28549228195425E+03 0.51736188272035E+02 0.70267965319982E+02 0.51501564735318E+02
 0.28555022309901E+03 0.52449478754199E+02 0.70200774113143E+02 0.52202082377172E+02 0.28549228195425E+03
 0.51736188272036E+02 0.70267965319982E+02 0.51501564735319E+02 0.28555022309901E+03 0.17331920753617E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35083554066043E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16591247531845E+00 0.00000000000000E+00 0.00000000000000E+00 0.16591247531845E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17514073084957E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17514073084957E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17258037359040E+00 0.18534277253339E+00 0.32776223225522E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    200.51143628
 0.98631051928247E+00 0.29606456560651E+03 0.44436977948618E+03 0.39561144864820E+03 0.38153541162373E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18127212940954E+00 0.00000000000000E+00 -.16858273271369E+02
 0.98661382190151E-03 0.69379223255849E+00 0.80000000000000E+04 0.30000000000000E+04 0.11530829583517E+02
 0.43240610938190E+01 0.30640470677579E+03 0.29215000000011E+03 0.30655793034483E+03 0.32742685815961E+03
 0.29215000000004E+03 0.29215000000005E+03 0.30455596210061E+03 0.32737488133548E+03 0.29215000000004E+03
 0.29215000000005E+03 0.30655793034483E+03 0.32742685815961E+03 0.29215000000004E+03 0.29215000000005E+03
 0.30455596210061E+03 0.32737488133548E+03 0.29215000000004E+03 0.29215000000005E+03 0.36677805260829E+03
 0.29461356472295E+03 0.13747590189016E+04 0.13340629421422E+04 0.80695962068760E+03 0.16373681015954E+04
 0.82637368280441E+03 0.92549860075608E+03 0.11484729912270E+04 0.89142798528115E+03 0.20872326874620E+04
 0.80996045099670E+03 0.11463652293044E+04 0.78410677312798E+03 0.20856470308167E+04 0.92549860075608E+03
 0.11484729912270E+04 0.89142798528115E+03 0.20872326874620E+04 0.80996045099670E+03 0.11463652293044E+04
 0.78410677312798E+03 0.20856470308167E+04 0.17208725067729E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52056666165653E+03 0.12948287003065E+01
 0.12948287003065E+01 0.63929348980042E+00 0.28425846992865E+00 0.29263968628847E+03 0.32794910349933E+03
 0.32125776955904E+03 0.32061803248218E+03 0.23000000000000E+00 0.00000000000000E+00 0.21246576782121E+00
 0.00000000000000E+00 -.11462218538246E+02 0.99791889923860E-03 0.32582580418475E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24552997022494E+02 0.92073738834354E+01 0.29461169328848E+03 0.36682250028377E+03
 0.29279290907620E+03 0.29668628591261E+03 0.29215000000004E+03 0.29215000000004E+03 0.29278380912860E+03
 0.29668739940325E+03 0.29215000000004E+03 0.29215000000004E+03 0.29279290907620E+03 0.29668628591261E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29278380912860E+03 0.29668739940325E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29593229509414E+03 0.29215000000009E+03 0.78632935701044E+02 0.74105834189155E+02
 0.79969881604297E+02 0.36247303319498E+03 0.28210330218266E+03 0.56861679622127E+02 0.76291575100637E+02
 0.56683676478562E+02 0.29047901028201E+03 0.56123419076762E+02 0.76352313792182E+02 0.55958596370797E+02
 0.29053053256707E+03 0.56861679622127E+02 0.76291575100637E+02 0.56683676478562E+02 0.29047901028201E+03
 0.56123419076764E+02 0.76352313792182E+02 0.55958596370799E+02 0.29053053256707E+03 0.17861949269368E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35103104645048E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16789530330912E+00 0.00000000000000E+00 0.00000000000000E+00 0.16789530330912E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17616910787171E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17616910787171E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17257379683123E+00 0.18522965405151E+00 0.32794910349933E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    210.68742106
 0.98813104383195E+00 0.29643277483427E+03 0.44465379174000E+03 0.39583319568903E+03 0.38177746675754E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17903712817107E+00 0.00000000000000E+00 -.16994359713086E+02
 0.98538699166591E-03 0.72517106771097E+00 0.80000000000000E+04 0.30000000000000E+04 0.11031879726328E+02
 0.41369548973728E+01 0.30710433895830E+03 0.29215000000013E+03 0.30721993444453E+03 0.32880984863744E+03
 0.29215000000004E+03 0.29215000000005E+03 0.30515950869332E+03 0.32875876550584E+03 0.29215000000004E+03
 0.29215000000005E+03 0.30721993444453E+03 0.32880984863744E+03 0.29215000000004E+03 0.29215000000005E+03
 0.30515950869332E+03 0.32875876550584E+03 0.29215000000004E+03 0.29215000000005E+03 0.36888184147987E+03
 0.29496415362376E+03 0.13884267400460E+04 0.13460273672206E+04 0.80661216061839E+03 0.16132244983101E+04
 0.80257927688857E+03 0.93428748957444E+03 0.11604788599246E+04 0.89899855811709E+03 0.20880812293564E+04
 0.81895185265170E+03 0.11585042603740E+04 0.79218319705204E+03 0.20866181485664E+04 0.93428748957445E+03
 0.11604788599246E+04 0.89899855811710E+03 0.20880812293564E+04 0.81895185265170E+03 0.11585042603740E+04
 0.79218319705205E+03 0.20866181485664E+04 0.17193959970305E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52061097789762E+03 0.12948297029562E+01
 0.12948297029562E+01 0.67999742891841E+00 0.26066595563589E+00 0.29279132220862E+03 0.32805016949021E+03
 0.32192298207603E+03 0.32132584561927E+03 0.22999999957540E+00 0.00000000000000E+00 0.21106291134401E+00
 0.00000000000000E+00 -.11604450939288E+02 0.99740067871578E-03 0.35372514606228E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22616429985420E+02 0.84811612445324E+01 0.29496216676840E+03 0.36892580753795E+03
 0.29286923241927E+03 0.29691933049756E+03 0.29215000000004E+03 0.29215000000004E+03 0.29285938399132E+03
 0.29692042961337E+03 0.29215000000004E+03 0.29215000000004E+03 0.29286923241927E+03 0.29691933049756E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29285938399132E+03 0.29692042961337E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29613415883473E+03 0.29215000000009E+03 0.83696920547928E+02 0.78553687793796E+02
 0.85970046139558E+02 0.36756600052728E+03 0.28116610415703E+03 0.61127294530491E+02 0.82008749316200E+02
 0.61049354566949E+02 0.29452537524313E+03 0.60373092787775E+02 0.82062193815561E+02 0.60307235606519E+02
 0.29456975316520E+03 0.61127294530491E+02 0.82008749316200E+02 0.61049354566949E+02 0.29452537524313E+03
 0.60373092787777E+02 0.82062193815561E+02 0.60307235606521E+02 0.29456975316520E+03 0.18324341207420E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35112002613806E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16896766660381E+00 0.00000000000000E+00 0.00000000000000E+00 0.16896766660381E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17708315002814E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17708315002814E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17257035989890E+00 0.18516864043972E+00 0.32805016949021E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    220.86340584
 0.98959500678734E+00 0.29679960096770E+03 0.44490857360530E+03 0.39605260701111E+03 0.38202389750172E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17692245252165E+00 0.00000000000000E+00 -.17073758844510E+02
 0.98416834234787E-03 0.75484813422774E+00 0.80000000000000E+04 0.30000000000000E+04 0.10598158274822E+02
 0.39743093530584E+01 0.30777613736585E+03 0.29215000000019E+03 0.30785702140849E+03 0.33012281081002E+03
 0.29215000000005E+03 0.29215000000005E+03 0.30574079657756E+03 0.33007261721073E+03 0.29215000000005E+03
 0.29215000000005E+03 0.30785702140849E+03 0.33012281081002E+03 0.29215000000005E+03 0.29215000000005E+03
 0.30574079657756E+03 0.33007261721073E+03 0.29215000000005E+03 0.29215000000005E+03 0.37083135042230E+03
 0.29533843513468E+03 0.14003958627646E+04 0.13564191336936E+04 0.80527949512980E+03 0.15899060488435E+04
 0.78060015623804E+03 0.94198118120967E+03 0.11704745435437E+04 0.90556568561209E+03 0.20873671723878E+04
 0.82686349923287E+03 0.11686194869962E+04 0.79925695735090E+03 0.20860132051883E+04 0.94198118120967E+03
 0.11704745435437E+04 0.90556568561209E+03 0.20873671723878E+04 0.82686349923287E+03 0.11686194869962E+04
 0.79925695735090E+03 0.20860132051883E+04 0.17170034084495E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52067779738633E+03 0.12948302879493E+01
 0.12948302879493E+01 0.72070136803640E+00 0.23814068959309E+00 0.29297678024278E+03 0.32808819961412E+03
 0.32251389453300E+03 0.32196240135691E+03 0.22999999967296E+00 0.00000000000000E+00 0.20964555063437E+00
 0.00000000000000E+00 -.11686619939692E+02 0.99676850267752E-03 0.38186656887881E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20949726035166E+02 0.78561472631874E+01 0.29533634382348E+03 0.37087469356293E+03
 0.29294764243269E+03 0.29714361986946E+03 0.29215000000004E+03 0.29215000000004E+03 0.29293705732537E+03
 0.29714469130166E+03 0.29215000000004E+03 0.29215000000004E+03 0.29294764243269E+03 0.29714361986946E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29293705732537E+03 0.29714469130166E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29632823115224E+03 0.29215000000011E+03 0.88295908474580E+02 0.82551258527071E+02
 0.91591573854545E+02 0.37171470716106E+03 0.27966517543724E+03 0.65241961000541E+02 0.87344076233456E+02
 0.65241961000541E+02 0.29780344757364E+03 0.64480747275779E+02 0.87389536957287E+02 0.64480747275779E+02
 0.29784010175347E+03 0.65241961000541E+02 0.87344076233455E+02 0.65241961000541E+02 0.29780344757364E+03
 0.64480747275779E+02 0.87389536957287E+02 0.64480747275779E+02 0.29784010175347E+03 0.18727759359672E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35116642502866E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16952661143697E+00 0.00000000000000E+00 0.00000000000000E+00 0.16952661143697E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17787744023699E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17787744023699E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256877252588E+00 0.18514532346812E+00 0.32808819961412E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    231.03939062
 0.99070737035654E+00 0.29716404124493E+03 0.44516638133024E+03 0.39629071161273E+03 0.38228976823077E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17491975247118E+00 0.00000000000000E+00 -.17125353575920E+02
 0.98296086332411E-03 0.78289392376885E+00 0.80000000000000E+04 0.30000000000000E+04 0.10218498007352E+02
 0.38319367527570E+01 0.30842369359271E+03 0.29215000000030E+03 0.30847223108313E+03 0.33137576819283E+03
 0.29215000000005E+03 0.29215000000006E+03 0.30630256297993E+03 0.33132644936660E+03 0.29215000000005E+03
 0.29215000000006E+03 0.30847223108313E+03 0.33137576819283E+03 0.29215000000005E+03 0.29215000000006E+03
 0.30630256297993E+03 0.33132644936660E+03 0.29215000000005E+03 0.29215000000006E+03 0.37265339115344E+03
 0.29573621949303E+03 0.14112129365227E+04 0.13657638500040E+04 0.80344718957575E+03 0.15679085188839E+04
 0.76044409336026E+03 0.94891672798966E+03 0.11791398447655E+04 0.91145099749445E+03 0.20859534965164E+04
 0.83402436388151E+03 0.11773922983353E+04 0.80564353985080E+03 0.20846968774087E+04 0.94891672798966E+03
 0.11791398447655E+04 0.91145099749446E+03 0.20859534965164E+04 0.83402436388151E+03 0.11773922983353E+04
 0.80564353985081E+03 0.20846968774087E+04 0.17143478784953E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52079525269344E+03 0.12948306680869E+01
 0.12948306680869E+01 0.76140530715439E+00 0.21669660096351E+00 0.29320112511609E+03 0.32808259209981E+03
 0.32304346187848E+03 0.32253923946597E+03 0.22999999967594E+00 0.00000000000000E+00 0.20821839100694E+00
 0.00000000000000E+00 -.11736818183279E+02 0.99600532467800E-03 0.41016229132601E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19504474616954E+02 0.73141779813579E+01 0.29573403332144E+03 0.37269596116797E+03
 0.29302786355763E+03 0.29735945058516E+03 0.29215000000004E+03 0.29215000000004E+03 0.29301668789400E+03
 0.29736048177255E+03 0.29215000000004E+03 0.29215000000004E+03 0.29302786355763E+03 0.29735945058516E+03
 0.29215000000004E+03 0.29215000000004E+03 0.29301668789400E+03 0.29736048177255E+03 0.29215000000004E+03
 0.29215000000004E+03 0.29651486400873E+03 0.29215000000013E+03 0.92425786441765E+02 0.86115706873621E+02
 0.96839941082202E+02 0.37512204691411E+03 0.27779790612650E+03 0.69210223019205E+02 0.92305945362781E+02
 0.69431716368294E+02 0.30047581384408E+03 0.68449725077789E+02 0.92342901713868E+02 0.68689219841870E+02
 0.30050432550288E+03 0.69210223019205E+02 0.92305945362781E+02 0.69431716368294E+02 0.30047581384408E+03
 0.68449725077790E+02 0.92342901713868E+02 0.68689219841871E+02 0.30050432550288E+03 0.19081656635395E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35118949135943E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16982523023878E+00 0.00000000000000E+00 0.00000000000000E+00 0.16982523023878E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17855933015769E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17855933015769E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256817244619E+00 0.18514775233848E+00 0.32808259209981E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    240.00557961
 0.99144147580643E+00 0.29748247968932E+03 0.44540928141708E+03 0.39652235921165E+03 0.38254377201845E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17324133559035E+00 0.00000000000000E+00 -.17156828447851E+02
 0.98190835334124E-03 0.80632518661287E+00 0.80000000000000E+04 0.30000000000000E+04 0.99215553883485E+01
 0.37205832706307E+01 0.30897671299546E+03 0.29215000000047E+03 0.30899836577956E+03 0.33243689597250E+03
 0.29215000000006E+03 0.29215000000008E+03 0.30678336124067E+03 0.33238833062291E+03 0.29215000000005E+03
 0.29215000000008E+03 0.30899836577956E+03 0.33243689597250E+03 0.29215000000006E+03 0.29215000000008E+03
 0.30678336124067E+03 0.33238833062291E+03 0.29215000000005E+03 0.29215000000008E+03 0.37416978283129E+03
 0.29610616331087E+03 0.14200652885397E+04 0.13733906288060E+04 0.80165462169038E+03 0.15497770946818E+04
 0.74411419988301E+03 0.95457516161857E+03 0.11860248179899E+04 0.91623607247853E+03 0.20845297710208E+04
 0.83988366324198E+03 0.11843631690937E+04 0.81086340965040E+03 0.20833503653623E+04 0.95457516161857E+03
 0.11860248179899E+04 0.91623607247853E+03 0.20845297710208E+04 0.83988366324198E+03 0.11843631690937E+04
 0.81086340965040E+03 0.20833503653623E+04 0.17120719718273E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52094624211931E+03 0.12948308999863E+01
 0.12948308999863E+01 0.79727006310746E+00 0.19870286027970E+00 0.29343416944413E+03 0.32805420221034E+03
 0.32346813585459E+03 0.32300595998082E+03 0.22999999961341E+00 0.00000000000000E+00 0.20695576209197E+00
 0.00000000000000E+00 -.11763657920668E+02 0.99521403720324E-03 0.43516425379580E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18383862944207E+02 0.68939486040776E+01 0.29610390037389E+03 0.37421153670341E+03
 0.29310038772921E+03 0.29754303814625E+03 0.29215000000004E+03 0.29215000000005E+03 0.29308876139349E+03
 0.29754402412535E+03 0.29215000000004E+03 0.29215000000005E+03 0.29310038772921E+03 0.29754303814625E+03
 0.29215000000004E+03 0.29215000000005E+03 0.29308876139349E+03 0.29754402412535E+03 0.29215000000004E+03
 0.29215000000005E+03 0.29667356999490E+03 0.29215000000016E+03 0.95683043789405E+02 0.88918773739342E+02
 0.10117254646537E+03 0.37765392687146E+03 0.27597551767377E+03 0.72597664247579E+02 0.96386375371893E+02
 0.73111615082779E+02 0.30244403071425E+03 0.71844105817811E+02 0.96415459256746E+02 0.72380604210604E+02
 0.30246506618825E+03 0.72597664247579E+02 0.96386375371893E+02 0.73111615082779E+02 0.30244403071425E+03
 0.71844105817813E+02 0.96415459256746E+02 0.72380604210607E+02 0.30246506618825E+03 0.19358742552878E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35120218744483E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16995221945471E+00 0.00000000000000E+00 0.00000000000000E+00 0.16995221945471E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17907795736998E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17907795736998E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256816640760E+00 0.18516371951954E+00 0.32805420221034E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    250.01137643
 0.99203586118257E+00 0.29783484755132E+03 0.44570682054492E+03 0.39680872052043E+03 0.38285090551009E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17145689886810E+00 0.00000000000000E+00 -.17193220895141E+02
 0.98074630705886E-03 0.83113539230766E+00 0.80000000000000E+04 0.30000000000000E+04 0.96253872402039E+01
 0.36095202150765E+01 0.30957902880526E+03 0.29215000000079E+03 0.30957197763093E+03 0.33358112267774E+03
 0.29215000000006E+03 0.29215000000011E+03 0.30730817637513E+03 0.33353339345741E+03 0.29215000000006E+03
 0.29215000000011E+03 0.30957197763093E+03 0.33358112267774E+03 0.29215000000006E+03 0.29215000000011E+03
 0.30730817637513E+03 0.33353339345741E+03 0.29215000000006E+03 0.29215000000011E+03 0.37577058555495E+03
 0.29654058802600E+03 0.14294039739065E+04 0.13814177814133E+04 0.79972632567219E+03 0.15311889124060E+04
 0.72746395510541E+03 0.96052608784555E+03 0.11931534244937E+04 0.92125304526841E+03 0.20830250759180E+04
 0.84605864699544E+03 0.11915789821259E+04 0.81635675917487E+03 0.20819233353244E+04 0.96052608784555E+03
 0.11931534244937E+04 0.92125304526841E+03 0.20830250759180E+04 0.84605864699544E+03 0.11915789821259E+04
 0.81635675917487E+03 0.20819233353244E+04 0.17098348980338E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52116889538526E+03 0.12948311681174E+01
 0.12948311681174E+01 0.83729325038188E+00 0.17962224321976E+00 0.29373781942168E+03 0.32800852851346E+03
 0.32390467408426E+03 0.32348901572953E+03 0.22999999986134E+00 0.00000000000000E+00 0.20554360939823E+00
 0.00000000000000E+00 -.11790887498703E+02 0.10062166021335E-02 0.46309036069918E+00 0.79505744419614E+04
 0.29814654157355E+04 0.17275246213118E+02 0.64782173299193E+01 0.29653823769209E+03 0.37581121442554E+03
 0.29318442462989E+03 0.29774165857605E+03 0.29215000000005E+03 0.29215000000005E+03 0.29317236978471E+03
 0.29774258242035E+03 0.29215000000005E+03 0.29215000000005E+03 0.29318442462989E+03 0.29774165857605E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29317236978471E+03 0.29774258242035E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29684551329286E+03 0.29215000000024E+03 0.98909740151513E+02 0.91706201758820E+02
 0.10569743332554E+03 0.38008036231988E+03 0.27385444182771E+03 0.76251496999221E+02 0.10063635614616E+03
 0.77243762356244E+02 0.30431542800800E+03 0.75511931673254E+02 0.10065655799902E+03 0.76531419995017E+02
 0.30432811536178E+03 0.76251496999221E+02 0.10063635614616E+03 0.77243762356244E+02 0.30431542800800E+03
 0.75511931673254E+02 0.10065655799902E+03 0.76531419995017E+02 0.30432811536178E+03 0.19635708335543E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35121785731168E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17009127367528E+00 0.00000000000000E+00 0.00000000000000E+00 0.17009127367528E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17956162381343E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17956162381343E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256823098318E+00 0.18518952223425E+00 0.32800852851346E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    260.00274936
 0.99247807306400E+00 0.29818231304673E+03 0.44602788468067E+03 0.39711672199857E+03 0.38317524136158E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16976204166898E+00 0.00000000000000E+00 -.17232157825433E+02
 0.97960308778246E-03 0.85459586913933E+00 0.80000000000000E+04 0.30000000000000E+04 0.93611498591222E+01
 0.35104311971708E+01 0.31016328267510E+03 0.29215000000133E+03 0.31012902729551E+03 0.33468636552216E+03
 0.29215000000008E+03 0.29215000000017E+03 0.30781808612379E+03 0.33463942803253E+03 0.29215000000007E+03
 0.29215000000016E+03 0.31012902729551E+03 0.33468636552216E+03 0.29215000000008E+03 0.29215000000017E+03
 0.30781808612379E+03 0.33463942803253E+03 0.29215000000007E+03 0.29215000000016E+03 0.37730335979988E+03
 0.29699566729302E+03 0.14382481452346E+04 0.13890144549014E+04 0.79767877229436E+03 0.15137058711385E+04
 0.71203870498265E+03 0.96614874500430E+03 0.11997867623137E+04 0.92598788688336E+03 0.20816315775178E+04
 0.85190488095679E+03 0.11982912317854E+04 0.82155890317250E+03 0.20805997476761E+04 0.96614874500430E+03
 0.11997867623137E+04 0.92598788688336E+03 0.20816315775178E+04 0.85190488095679E+03 0.11982912317854E+04
 0.82155890317251E+03 0.20805997476761E+04 0.17077844283098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52143294554995E+03 0.12948314549957E+01
 0.12948314549957E+01 0.87725874209665E+00 0.16161641248682E+00 0.29408999754277E+03 0.32795659886170E+03
 0.32430766645618E+03 0.32393733453036E+03 0.23000000000000E+00 0.00000000000000E+00 0.20413214655841E+00
 0.00000000000000E+00 -.11817916136618E+02 0.11183198748979E-02 0.49096408958839E+00 0.71535883243874E+04
 0.26825956216453E+04 0.16294470755910E+02 0.61104265334662E+01 0.29699324722620E+03 0.37734280504483E+03
 0.29326998276288E+03 0.29793336852244E+03 0.29215000000005E+03 0.29215000000005E+03 0.29325757880227E+03
 0.29793422274599E+03 0.29215000000005E+03 0.29215000000005E+03 0.29326998276288E+03 0.29793336852244E+03
 0.29215000000005E+03 0.29215000000005E+03 0.29325757880227E+03 0.29793422274599E+03 0.29215000000005E+03
 0.29215000000005E+03 0.29701129005170E+03 0.29215000000036E+03 0.10172155474727E+03 0.94169992908589E+02
 0.10992043500244E+03 0.38220681058498E+03 0.27173677340753E+03 0.79788955167457E+02 0.10458945167857E+03
 0.81454738350436E+02 0.30593734595972E+03 0.79069401027183E+02 0.10460072585345E+03 0.80766911103090E+02
 0.30594169568886E+03 0.79788955167457E+02 0.10458945167857E+03 0.81454738350436E+02 0.30593734595972E+03
 0.79069401027183E+02 0.10460072585345E+03 0.80766911103090E+02 0.30594169568886E+03 0.19884719447959E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35124241973913E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17024300560699E+00 0.00000000000000E+00 0.00000000000000E+00 0.17024300560699E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17998315446741E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17998315446741E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256827167248E+00 0.18521883995156E+00 0.32795659886170E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    270.01573421
 0.99281341872338E+00 0.29852567255304E+03 0.44637237549449E+03 0.39744431196307E+03 0.38351469494977E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16814529153348E+00 0.00000000000000E+00 -.17272432812893E+02
 0.97847597481274E-03 0.87686870520790E+00 0.80000000000000E+04 0.30000000000000E+04 0.91233726924982E+01
 0.34212647596868E+01 0.31073406929314E+03 0.29215000000222E+03 0.31067376348188E+03 0.33576047013580E+03
 0.29215000000012E+03 0.29215000000026E+03 0.30831710093132E+03 0.33571429304192E+03 0.29215000000010E+03
 0.29215000000026E+03 0.31067376348188E+03 0.33576047013580E+03 0.29215000000012E+03 0.29215000000026E+03
 0.30831710093132E+03 0.33571429304192E+03 0.29215000000010E+03 0.29215000000026E+03 0.37877470361999E+03
 0.29747404725395E+03 0.14467039765343E+04 0.13962674036923E+04 0.79563266146810E+03 0.14973474913391E+04
 0.69773666656367E+03 0.97151541107740E+03 0.12060497428161E+04 0.93049861006984E+03 0.20803960338680E+04
 0.85749458307816E+03 0.12046260868131E+04 0.82652951816520E+03 0.20794274750967E+04 0.97151541107740E+03
 0.12060497428161E+04 0.93049861006984E+03 0.20803960338680E+04 0.85749458307816E+03 0.12046260868131E+04
 0.82652951816521E+03 0.20794274750967E+04 0.17059651976083E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52173221096539E+03 0.12948317517328E+01
 0.12948317517328E+01 0.91731068151291E+00 0.14461598566397E+00 0.29449555544785E+03 0.32790549890698E+03
 0.32468442430410E+03 0.32435780298387E+03 0.23000000000000E+00 0.00000000000000E+00 0.20271778762851E+00
 0.00000000000000E+00 -.11843739814475E+02 0.12497844560631E-02 0.51885373501068E+00 0.64011037752866E+04
 0.24004139157325E+04 0.15418603471815E+02 0.57819763019307E+01 0.29747156610334E+03 0.37881283408679E+03
 0.29335830698321E+03 0.29811967065952E+03 0.29215000000005E+03 0.29215000000006E+03 0.29334563504803E+03
 0.29812044730484E+03 0.29215000000005E+03 0.29215000000006E+03 0.29335830698321E+03 0.29811967065952E+03
 0.29215000000005E+03 0.29215000000006E+03 0.29334563504803E+03 0.29812044730484E+03 0.29215000000005E+03
 0.29215000000006E+03 0.29717236129823E+03 0.29215000000056E+03 0.10414780846797E+03 0.96350713897714E+02
 0.11388535842285E+03 0.38413310969926E+03 0.26967832448429E+03 0.83238002898300E+02 0.10828968553962E+03
 0.85814448316745E+02 0.30739228118060E+03 0.82543889964255E+02 0.10829203256659E+03 0.85156462495261E+02
 0.30738835326115E+03 0.83238002898300E+02 0.10828968553962E+03 0.85814448316745E+02 0.30739228118060E+03
 0.82543889964254E+02 0.10829203256659E+03 0.85156462495261E+02 0.30738835326115E+03 0.20111702434869E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35128108841130E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17039805877494E+00 0.00000000000000E+00 0.00000000000000E+00 0.17039805877494E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18034568192849E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18034568192849E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256830295336E+00 0.18524769032696E+00 0.32790549890698E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    280.00867133
 0.99307526976014E+00 0.29886317883924E+03 0.44673595656755E+03 0.39778635702331E+03 0.38386435570910E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16660864756505E+00 0.00000000000000E+00 -.17314420949416E+02
 0.97737057648995E-03 0.89793349064753E+00 0.80000000000000E+04 0.30000000000000E+04 0.89093458294232E+01
 0.33410046860337E+01 0.31129053897713E+03 0.29215000000364E+03 0.31120528984927E+03 0.33680266995454E+03
 0.29215000000018E+03 0.29215000000042E+03 0.30880439360109E+03 0.33675721930986E+03 0.29215000000015E+03
 0.29215000000042E+03 0.31120528984927E+03 0.33680266995454E+03 0.29215000000018E+03 0.29215000000042E+03
 0.30880439360109E+03 0.33675721930986E+03 0.29215000000015E+03 0.29215000000042E+03 0.38018541328651E+03
 0.29797376710662E+03 0.14547863793419E+04 0.14031872737209E+04 0.79361254966867E+03 0.14820623709193E+04
 0.68448175850231E+03 0.97663940339765E+03 0.12119736946031E+04 0.93479522351485E+03 0.20793185321001E+04
 0.86283997614765E+03 0.12106153793132E+04 0.83127735161731E+03 0.20784071288818E+04 0.97663940339765E+03
 0.12119736946031E+04 0.93479522351485E+03 0.20793185321001E+04 0.86283997614765E+03 0.12106153793132E+04
 0.83127735161731E+03 0.20784071288818E+04 0.17043610627923E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52205807544470E+03 0.12948320610921E+01
 0.12948320610921E+01 0.95728242999144E+00 0.12868388442924E+00 0.29495571110110E+03 0.32786084488687E+03
 0.32503793793139E+03 0.32475276189051E+03 0.23000000000000E+00 0.00000000000000E+00 0.20130748721088E+00
 0.00000000000000E+00 -.11869285790134E+02 0.14045175780195E-02 0.54661870653543E+00 0.56959059289816E+04
 0.21359647233681E+04 0.14635430336268E+02 0.54882863761003E+01 0.29797123063610E+03 0.38022210159655E+03
 0.29344950616646E+03 0.29830048335140E+03 0.29215000000005E+03 0.29215000000008E+03 0.29343664810797E+03
 0.29830117575481E+03 0.29215000000005E+03 0.29215000000008E+03 0.29344950616646E+03 0.29830048335140E+03
 0.29215000000005E+03 0.29215000000008E+03 0.29343664810797E+03 0.29830117575481E+03 0.29215000000005E+03
 0.29215000000008E+03 0.29732867812334E+03 0.29215000000087E+03 0.10619934635662E+03 0.98272769407368E+02
 0.11760342347044E+03 0.38592683217391E+03 0.26773539158612E+03 0.86596940329511E+02 0.11175010147579E+03
 0.90362559754102E+02 0.30873643431516E+03 0.85932986202081E+02 0.11174364852772E+03 0.89739022996440E+02
 0.30872440438964E+03 0.86596940329511E+02 0.11175010147579E+03 0.90362559754102E+02 0.30873643431516E+03
 0.85932986202081E+02 0.11174364852772E+03 0.89739022996440E+02 0.30872440438964E+03 0.20320085610976E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35133761328321E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17056270393174E+00 0.00000000000000E+00 0.00000000000000E+00 0.17056270393174E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18065865468185E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18065865468185E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256828199640E+00 0.18527285147553E+00 0.32786084488687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    290.01669206
 0.99328979286711E+00 0.29919581924782E+03 0.44711658538282E+03 0.39814052299783E+03 0.38422233419752E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16514247822658E+00 0.00000000000000E+00 -.17358079800317E+02
 0.97628353312778E-03 0.91793103562767E+00 0.80000000000000E+04 0.30000000000000E+04 0.87152516795880E+01
 0.32682193798455E+01 0.31183590751916E+03 0.29215000000585E+03 0.31172660887892E+03 0.33781970729211E+03
 0.29215000000028E+03 0.29215000000069E+03 0.30928270271513E+03 0.33777495222227E+03 0.29215000000023E+03
 0.29215000000069E+03 0.31172660887892E+03 0.33781970729211E+03 0.29215000000028E+03 0.29215000000069E+03
 0.30928270271513E+03 0.33777495222227E+03 0.29215000000023E+03 0.29215000000069E+03 0.38154635913856E+03
 0.29849641801872E+03 0.14625583675116E+04 0.14098256137075E+04 0.79161078318357E+03 0.14676715315693E+04
 0.67210269446983E+03 0.98156361726749E+03 0.12176147578596E+04 0.93891225944518E+03 0.20783703548774E+04
 0.86798475783019E+03 0.12163161814355E+04 0.83583924222658E+03 0.20775108806789E+04 0.98156361726749E+03
 0.12176147578596E+04 0.93891225944518E+03 0.20783703548774E+04 0.86798475783019E+03 0.12163161814355E+04
 0.83583924222658E+03 0.20775108806789E+04 0.17029306213487E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52240524772923E+03 0.12948323827610E+01
 0.12948323827610E+01 0.99731451290659E+00 0.11375314855743E+00 0.29547416776837E+03 0.32782636738707E+03
 0.32537292434081E+03 0.32512670716006E+03 0.23000000000000E+00 0.00000000000000E+00 0.19989702950614E+00
 0.00000000000000E+00 -.11894882592789E+02 0.15888680749073E-02 0.57433859528667E+00 0.50350309924044E+04
 0.18881366221517E+04 0.13929065651607E+02 0.52233996193527E+01 0.29849383301700E+03 0.38158150822854E+03
 0.29354443129217E+03 0.29847709487664E+03 0.29215000000005E+03 0.29215000000010E+03 0.29353146688758E+03
 0.29847769709919E+03 0.29215000000005E+03 0.29215000000010E+03 0.29354443129217E+03 0.29847709487664E+03
 0.29215000000005E+03 0.29215000000010E+03 0.29353146688758E+03 0.29847769709919E+03 0.29215000000005E+03
 0.29215000000010E+03 0.29748136351826E+03 0.29215000000137E+03 0.10790466956987E+03 0.99973310106252E+02
 0.12111417132559E+03 0.38765361251562E+03 0.26593387033340E+03 0.89888801612103E+02 0.11500980324341E+03
 0.95176195462805E+02 0.31002357407001E+03 0.89259249484205E+02 0.11499472191646E+03 0.94591250925574E+02
 0.31000365625272E+03 0.89888801612104E+02 0.11500980324341E+03 0.95176195462806E+02 0.31002357407001E+03
 0.89259249484205E+02 0.11499472191646E+03 0.94591250925574E+02 0.31000365625272E+03 0.20514208847153E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35141475532789E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17073767841674E+00 0.00000000000000E+00 0.00000000000000E+00 0.17073767841674E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18093259175881E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18093259175881E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256819286341E+00 0.18529219408601E+00 0.32782636738707E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    300.04184707
 0.99347410323079E+00 0.29952352916365E+03 0.44751124110865E+03 0.39850392130407E+03 0.38458617558266E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16374311564266E+00 0.00000000000000E+00 -.17403103967780E+02
 0.97521494391418E-03 0.93691998457044E+00 0.80000000000000E+04 0.30000000000000E+04 0.85386160309814E+01
 0.32019810116180E+01 0.31237127934343E+03 0.29215000000925E+03 0.31223872074760E+03 0.33881419334026E+03
 0.29215000000044E+03 0.29215000000113E+03 0.30975293206993E+03 0.33877010368319E+03 0.29215000000035E+03
 0.29215000000112E+03 0.31223872074760E+03 0.33881419334026E+03 0.29215000000044E+03 0.29215000000113E+03
 0.30975293206993E+03 0.33877010368319E+03 0.29215000000035E+03 0.29215000000112E+03 0.38286244672875E+03
 0.29904191529348E+03 0.14700459959650E+04 0.14162025091524E+04 0.78962114907760E+03 0.14540630359376E+04
 0.66049378111457E+03 0.98630662523828E+03 0.12229961112489E+04 0.94286389256460E+03 0.20775215969020E+04
 0.87294762406233E+03 0.12217522647504E+04 0.84023008742849E+03 0.20767094030711E+04 0.98630662523828E+03
 0.12229961112489E+04 0.94286389256460E+03 0.20775215969020E+04 0.87294762406233E+03 0.12217522647504E+04
 0.84023008742849E+03 0.20767094030711E+04 0.17016385826668E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52276867842826E+03 0.12948327144895E+01
 0.12948327144895E+01 0.10374151329627E+01 0.99813685829259E-01 0.29605244754662E+03 0.32780479245080E+03
 0.32569191339839E+03 0.32548188023580E+03 0.23000000000000E+00 0.00000000000000E+00 0.19848663772955E+00
 0.00000000000000E+00 -.11920377330372E+02 0.18107608680005E-02 0.60200558382748E+00 0.44180322986734E+04
 0.16567621120025E+04 0.13288913284054E+02 0.49833424815204E+01 0.29903928989701E+03 0.38289599364459E+03
 0.29364372960756E+03 0.29865011973611E+03 0.29215000000005E+03 0.29215000000015E+03 0.29363073694192E+03
 0.29865062678627E+03 0.29215000000005E+03 0.29215000000015E+03 0.29364372960756E+03 0.29865011973611E+03
 0.29215000000005E+03 0.29215000000015E+03 0.29363073694192E+03 0.29865062678627E+03 0.29215000000005E+03
 0.29215000000015E+03 0.29763095201715E+03 0.29215000000214E+03 0.10928263188590E+03 0.10147651708822E+03
 0.12444141209041E+03 0.38935873265858E+03 0.26429511350772E+03 0.93123018872325E+02 0.11809288460515E+03
 0.10032211963079E+03 0.31129128246188E+03 0.92531587437407E+02 0.11806940903007E+03 0.99779376414229E+02
 0.31126374578708E+03 0.93123018872325E+02 0.11809288460515E+03 0.10032211963079E+03 0.31129128246188E+03
 0.92531587437407E+02 0.11806940903007E+03 0.99779376414229E+02 0.31126374578708E+03 0.20696890435601E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35151416530162E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17092170810125E+00 0.00000000000000E+00 0.00000000000000E+00 0.17092170810125E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18117525655862E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18117525655862E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256803378631E+00 0.18530417188940E+00 0.32780479245080E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    310.19600773
 0.99364201944197E+00 0.29984978062906E+03 0.44792158283783E+03 0.39887812798145E+03 0.38495784581799E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16239303979020E+00 0.00000000000000E+00 -.17449903469083E+02
 0.97415341156722E-03 0.95514532453677E+00 0.80000000000000E+04 0.30000000000000E+04 0.83756888030414E+01
 0.31408833011405E+01 0.31290316959010E+03 0.29215000001442E+03 0.31274782141889E+03 0.33979884188651E+03
 0.29215000000070E+03 0.29215000000184E+03 0.31022074465305E+03 0.33975539414414E+03 0.29215000000056E+03
 0.29215000000183E+03 0.31274782141889E+03 0.33979884188651E+03 0.29215000000070E+03 0.29215000000184E+03
 0.31022074465305E+03 0.33975539414414E+03 0.29215000000056E+03 0.29215000000183E+03 0.38415219167099E+03
 0.29961636481505E+03 0.14773473291625E+04 0.14223999050069E+04 0.78760456871168E+03 0.14409817037531E+04
 0.64943911219786E+03 0.99093198063455E+03 0.12281878996603E+04 0.94670207781860E+03 0.20767321856789E+04
 0.87779470244952E+03 0.12269948130918E+04 0.84450725774765E+03 0.20759635807681E+04 0.99093198063455E+03
 0.12281878996603E+04 0.94670207781860E+03 0.20767321856789E+04 0.87779470244952E+03 0.12269948130918E+04
 0.84450725774765E+03 0.20759635807681E+04 0.17004348447098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52314833586680E+03 0.12948330592985E+01
 0.12948330592985E+01 0.10780317755945E+01 0.86718521134105E-01 0.29669830119552E+03 0.32779791159077E+03
 0.32599997010989E+03 0.32582347647791E+03 0.23000000000000E+00 0.00000000000000E+00 0.19706095977207E+00
 0.00000000000000E+00 -.11932979797912E+02 0.20841993810601E-02 0.62991693382236E+00 0.38384043641405E+04
 0.14394016365527E+04 0.12700087218573E+02 0.47625327069649E+01 0.29961370823414E+03 0.38418408755449E+03
 0.29374915721812E+03 0.29882193842743E+03 0.29215000000005E+03 0.29215000000022E+03 0.29373621211717E+03
 0.29882234528264E+03 0.29215000000005E+03 0.29215000000022E+03 0.29374915721812E+03 0.29882193842743E+03
 0.29215000000005E+03 0.29215000000022E+03 0.29373621211717E+03 0.29882234528264E+03 0.29215000000005E+03
 0.29215000000022E+03 0.29777948549334E+03 0.29215000000332E+03 0.11036062723734E+03 0.10281360211142E+03
 0.12764027865615E+03 0.39109527805795E+03 0.26281679800853E+03 0.96341831132638E+02 0.12105202042579E+03
 0.10593621383116E+03 0.31258198672097E+03 0.95792198079490E+02 0.12102035005557E+03 0.10543928017240E+03
 0.31254706601093E+03 0.96341831132638E+02 0.12105202042579E+03 0.10593621383116E+03 0.31258198672097E+03
 0.95792198079490E+02 0.12102035005557E+03 0.10543928017240E+03 0.31254706601093E+03 0.20872132840064E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35163814070693E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17111723111308E+00 0.00000000000000E+00 0.00000000000000E+00 0.17111723111308E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18139541209859E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18139541209859E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256817577401E+00 0.18530819105108E+00 0.32779791159077E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    320.08525611
 0.99379279976277E+00 0.30016210752624E+03 0.44832939705071E+03 0.39924686855412E+03 0.38532158494171E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16114026268129E+00 0.00000000000000E+00 -.17494652150017E+02
 0.97313934840437E-03 0.97196872059948E+00 0.80000000000000E+04 0.30000000000000E+04 0.82307175431179E+01
 0.30865190786692E+01 0.31341217729081E+03 0.29215000002183E+03 0.31323527871825E+03 0.34073759799704E+03
 0.29215000000110E+03 0.29215000000292E+03 0.31066903949544E+03 0.34069474785753E+03 0.29215000000087E+03
 0.29215000000291E+03 0.31323527871825E+03 0.34073759799704E+03 0.29215000000110E+03 0.29215000000292E+03
 0.31066903949544E+03 0.34069474785753E+03 0.29215000000087E+03 0.29215000000291E+03 0.38536802453091E+03
 0.30019650946794E+03 0.14842052461201E+04 0.14281981350768E+04 0.78565894251804E+03 0.14288901782630E+04
 0.63930294103239E+03 0.99527754935102E+03 0.12330124330960E+04 0.95029134358151E+03 0.20760138175988E+04
 0.88235519531418E+03 0.12318647261932E+04 0.84851865490898E+03 0.20752839307797E+04 0.99527754935102E+03
 0.12330124330960E+04 0.95029134358152E+03 0.20760138175988E+04 0.88235519531418E+03 0.12318647261932E+04
 0.84851865490898E+03 0.20752839307797E+04 0.16993540863664E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52352665583311E+03 0.12948333889977E+01
 0.12948333889977E+01 0.11175887690860E+01 0.74940883480457E-01 0.29738389951243E+03 0.32780665586649E+03
 0.32628671704044E+03 0.32613974497481E+03 0.23000000000000E+00 0.00000000000000E+00 0.19567546313060E+00
 0.00000000000000E+00 -.11944539270323E+02 0.24117498204234E-02 0.65698547546483E+00 0.33170936438985E+04
 0.12439101164620E+04 0.12176829319308E+02 0.45663109947407E+01 0.30019383205520E+03 0.38539830318527E+03
 0.29385766092041E+03 0.29898647856936E+03 0.29215000000005E+03 0.29215000000033E+03 0.29384483521619E+03
 0.29898678463677E+03 0.29215000000005E+03 0.29215000000033E+03 0.29385766092041E+03 0.29898647856936E+03
 0.29215000000005E+03 0.29215000000033E+03 0.29384483521619E+03 0.29898678463677E+03 0.29215000000005E+03
 0.29215000000033E+03 0.29792174811684E+03 0.29215000000502E+03 0.11112042784592E+03 0.10394634675084E+03
 0.13060922593963E+03 0.39281872951689E+03 0.26155645744757E+03 0.99426020945348E+02 0.12379476180056E+03
 0.11186319295195E+03 0.31386512851489E+03 0.98919676056171E+02 0.12375543472466E+03 0.11141332337725E+03
 0.31382336183386E+03 0.99426020945348E+02 0.12379476180056E+03 0.11186319295195E+03 0.31386512851489E+03
 0.98919676056171E+02 0.12375543472466E+03 0.11141332337725E+03 0.31382336183386E+03 0.21034805746652E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35178137312897E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17129970304747E+00 0.00000000000000E+00 0.00000000000000E+00 0.17129970304747E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18158746340660E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18158746340660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256825655500E+00 0.18530331352881E+00 0.32780665586649E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    330.00246160
 0.99393552156171E+00 0.30047006182396E+03 0.44874455585392E+03 0.39961946033460E+03 0.38568699139100E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15994259295999E+00 0.00000000000000E+00 -.17539367281467E+02
 0.97214154060358E-03 0.98796783900204E+00 0.80000000000000E+04 0.30000000000000E+04 0.80974295763321E+01
 0.30365360911245E+01 0.31391422077504E+03 0.29215000003251E+03 0.31371629761713E+03 0.34166058477109E+03
 0.29215000000170E+03 0.29215000000453E+03 0.31111174319578E+03 0.34161830624193E+03 0.29215000000135E+03
 0.29215000000452E+03 0.31371629761713E+03 0.34166058477109E+03 0.29215000000170E+03 0.29215000000453E+03
 0.31111174319578E+03 0.34161830624193E+03 0.29215000000135E+03 0.29215000000452E+03 0.38655177724482E+03
 0.30079784088827E+03 0.14908510141594E+04 0.14337944946800E+04 0.78370581308787E+03 0.14173054649025E+04
 0.62968112274922E+03 0.99949018279833E+03 0.12376345109149E+04 0.95375431990148E+03 0.20753235507364E+04
 0.88678247302969E+03 0.12365286838577E+04 0.85240009946503E+03 0.20746291808124E+04 0.99949018279833E+03
 0.12376345109149E+04 0.95375431990148E+03 0.20753235507364E+04 0.88678247302969E+03 0.12365286838577E+04
 0.85240009946503E+03 0.20746291808124E+04 0.16983340201948E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52391225575674E+03 0.12948337184499E+01
 0.12948337184499E+01 0.11572575910542E+01 0.64080571046270E-01 0.29812472167626E+03 0.32783107326582E+03
 0.32656200661678E+03 0.32644146700870E+03 0.23000000000000E+00 0.00000000000000E+00 0.19428918136050E+00
 0.00000000000000E+00 -.11956416958987E+02 0.28204904421716E-02 0.68401196425165E+00 0.28363861406460E+04
 0.10636448027423E+04 0.11695701856257E+02 0.43858881960964E+01 0.30079515243908E+03 0.38658044306361E+03
 0.29397287402830E+03 0.29914910558784E+03 0.29215000000006E+03 0.29215000000049E+03 0.29396023517603E+03
 0.29914930850292E+03 0.29215000000006E+03 0.29215000000049E+03 0.29397287402830E+03 0.29914910558783E+03
 0.29215000000006E+03 0.29215000000049E+03 0.29396023517603E+03 0.29914930850292E+03 0.29215000000006E+03
 0.29215000000049E+03 0.29806236055385E+03 0.29215000000750E+03 0.11161144400656E+03 0.10491933886506E+03
 0.13345680126981E+03 0.39459398109592E+03 0.26046989581976E+03 0.10247042518717E+03 0.12642279825643E+03
 0.11834272454672E+03 0.31519053394544E+03 0.10200954211071E+03 0.12637615588002E+03 0.11794199439514E+03
 0.31514227752953E+03 0.10247042518717E+03 0.12642279825643E+03 0.11834272454672E+03 0.31519053394545E+03
 0.10200954211071E+03 0.12637615588002E+03 0.11794199439514E+03 0.31514227752953E+03 0.21190993525980E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35194686151037E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17148128485671E+00 0.00000000000000E+00 0.00000000000000E+00 0.17148128485671E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18176197980080E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18176197980080E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256825197271E+00 0.18528948518162E+00 0.32783107326582E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    340.00621965
 0.99406090598337E+00 0.30077560153048E+03 0.44916809528278E+03 0.39999770302260E+03 0.38605615278967E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15879126253182E+00 0.00000000000000E+00 -.17578139284646E+02
 0.97115362924814E-03 0.10032655197955E+01 0.80000000000000E+04 0.30000000000000E+04 0.79739608729208E+01
 0.29902353273453E+01 0.31441272162949E+03 0.29215000004782E+03 0.31419412191560E+03 0.34257421245091E+03
 0.29215000000260E+03 0.29215000000695E+03 0.31155183740097E+03 0.34253248392087E+03 0.29215000000206E+03
 0.29215000000692E+03 0.31419412191560E+03 0.34257421245091E+03 0.29215000000260E+03 0.29215000000695E+03
 0.31155183740097E+03 0.34253248392087E+03 0.29215000000206E+03 0.29215000000692E+03 0.38771228238670E+03
 0.30142468860125E+03 0.14973432555136E+04 0.14392398059468E+04 0.78173465967628E+03 0.14061180560514E+04
 0.62047472307671E+03 0.10036055797714E+04 0.12420926049281E+04 0.95712115382106E+03 0.20746449148143E+04
 0.89111335899141E+03 0.12410257087954E+04 0.85618425628333E+03 0.20739833597684E+04 0.10036055797714E+04
 0.12420926049281E+04 0.95712115382107E+03 0.20746449148143E+04 0.89111335899141E+03 0.12410257087954E+04
 0.85618425628333E+03 0.20739833597684E+04 0.16973623422705E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52430750801219E+03 0.12948340041145E+01
 0.12948340041145E+01 0.11972726232524E+01 0.54089089471381E-01 0.29899539052295E+03 0.32787091697224E+03
 0.32682968301648E+03 0.32673308810734E+03 0.23000000000000E+00 0.00000000000000E+00 0.19289427761999E+00
 0.00000000000000E+00 -.11961612957472E+02 0.33414985750953E-02 0.71114836007561E+00 0.23941353917147E+04
 0.89780077189302E+03 0.11249410740607E+02 0.42185290277278E+01 0.30142199820855E+03 0.38773935167408E+03
 0.29402504080615E+03 0.29931108336110E+03 0.29215000000006E+03 0.29215000000074E+03 0.29401278912781E+03
 0.29931118085965E+03 0.29215000000006E+03 0.29215000000074E+03 0.29402504080615E+03 0.29931108336110E+03
 0.29215000000006E+03 0.29215000000074E+03 0.29401278912781E+03 0.29931118085965E+03 0.29215000000006E+03
 0.29215000000074E+03 0.29820241026634E+03 0.29215000001107E+03 0.11182724175818E+03 0.10594828066214E+03
 0.13620741484278E+03 0.39643064058321E+03 0.25954218866622E+03 0.10585658225464E+03 0.12895934755355E+03
 0.10585658225464E+03 0.31656445219110E+03 0.10544041975148E+03 0.12890572469787E+03 0.10544041975148E+03
 0.31651005528885E+03 0.10585658225464E+03 0.12895934755355E+03 0.10585658225464E+03 0.31656445219110E+03
 0.10544041975148E+03 0.12890572469787E+03 0.10544041975148E+03 0.31651005528885E+03 0.21325278435632E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35213517551633E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17160885082439E+00 0.00000000000000E+00 0.00000000000000E+00 0.17160885082439E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18188487902196E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18188487902196E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256837164912E+00 0.18526708731079E+00 0.32787091697224E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    350.00054329
 0.99418454979834E+00 0.30107575049575E+03 0.44959484933062E+03 0.40037638486008E+03 0.38642404269870E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15769528450039E+00 0.00000000000000E+00 -.17624443993780E+02
 0.97018502162481E-03 0.10177475156839E+01 0.80000000000000E+04 0.30000000000000E+04 0.78604957287708E+01
 0.29476858982890E+01 0.31490332204861E+03 0.29215000006925E+03 0.31466454681932E+03 0.34347063067919E+03
 0.29215000000391E+03 0.29215000001047E+03 0.31198544151100E+03 0.34342942623830E+03 0.29215000000310E+03
 0.29215000001043E+03 0.31466454681932E+03 0.34347063067919E+03 0.29215000000391E+03 0.29215000001047E+03
 0.31198544151100E+03 0.34342942623830E+03 0.29215000000310E+03 0.29215000001043E+03 0.38884030292571E+03
 0.30207090578470E+03 0.15036266566605E+04 0.14444862956900E+04 0.77976480015581E+03 0.13953950676825E+04
 0.61173144352596E+03 0.10075909300021E+04 0.12463556786291E+04 0.96036443293712E+03 0.20739779744939E+04
 0.89531306206323E+03 0.12453246562642E+04 0.85984002512043E+03 0.20733464700825E+04 0.10075909300021E+04
 0.12463556786291E+04 0.96036443293712E+03 0.20739779744939E+04 0.89531306206323E+03 0.12453246562642E+04
 0.85984002512043E+03 0.20733464700825E+04 0.16964343168586E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52470535685238E+03 0.12948343452788E+01
 0.12948343452788E+01 0.12372499178229E+01 0.45041392295385E-01 0.29992711688521E+03 0.32792682489026E+03
 0.32708606100198E+03 0.32701014112849E+03 0.23000000000000E+00 0.00000000000000E+00 0.19150403425240E+00
 0.00000000000000E+00 -.11976492092463E+02 0.40127221918027E-02 0.73813221919622E+00 0.19936590717251E+04
 0.74762215189691E+03 0.10838166648126E+02 0.40643124930474E+01 0.30206821075921E+03 0.38886581595049E+03
 0.29409817781237E+03 0.29947119738361E+03 0.29215000000007E+03 0.29215000000110E+03 0.29408594095897E+03
 0.29947118867322E+03 0.29215000000007E+03 0.29215000000110E+03 0.29409817781237E+03 0.29947119738361E+03
 0.29215000000007E+03 0.29215000000110E+03 0.29408594095897E+03 0.29947118867323E+03 0.29215000000007E+03
 0.29215000000110E+03 0.29834084294532E+03 0.29215000001610E+03 0.11179040474995E+03 0.10682508663782E+03
 0.13885448076278E+03 0.39834300561940E+03 0.25879425245281E+03 0.10913348176711E+03 0.13139873569978E+03
 0.10913348176711E+03 0.31800218093829E+03 0.10876597877295E+03 0.13133849394006E+03 0.10876597877295E+03
 0.31794200821345E+03 0.10913348176711E+03 0.13139873569978E+03 0.10913348176711E+03 0.31800218093829E+03
 0.10876597877295E+03 0.13133849394006E+03 0.10876597877295E+03 0.31794200821345E+03 0.21475112088606E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35234665091787E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17180368754823E+00 0.00000000000000E+00 0.00000000000000E+00 0.17180368754823E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18204030614675E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18204030614675E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256813407130E+00 0.18523521907879E+00 0.32792682489026E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    360.00486901
 0.99431005524107E+00 0.30137122487830E+03 0.45002434983096E+03 0.40075525086981E+03 0.38679062364692E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15665025513578E+00 0.00000000000000E+00 -.17668715347449E+02
 0.96923339637337E-03 0.10314789629479E+01 0.80000000000000E+04 0.30000000000000E+04 0.77558537666503E+01
 0.29084451624938E+01 0.31538741501101E+03 0.29215000009888E+03 0.31512888794555E+03 0.34435255506152E+03
 0.29215000000580E+03 0.29215000001554E+03 0.31241375567176E+03 0.34431185105548E+03 0.29215000000460E+03
 0.29215000001549E+03 0.31512888794555E+03 0.34435255506152E+03 0.29215000000580E+03 0.29215000001554E+03
 0.31241375567176E+03 0.34431185105548E+03 0.29215000000460E+03 0.29215000001549E+03 0.38994006885358E+03
 0.30273461829146E+03 0.15097235904450E+04 0.14495527627263E+04 0.77778780383227E+03 0.13850675929432E+04
 0.60339085009180E+03 0.10114608192886E+04 0.12504396861908E+04 0.96349619391948E+03 0.20733075736696E+04
 0.89939664139864E+03 0.12494418231087E+04 0.86338051537194E+03 0.20727036711717E+04 0.10114608192886E+04
 0.12504396861908E+04 0.96349619391948E+03 0.20733075736696E+04 0.89939664139864E+03 0.12494418231087E+04
 0.86338051537194E+03 0.20727036711717E+04 0.16955335253384E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52510496099247E+03 0.12948346714619E+01
 0.12948346714619E+01 0.12772672206989E+01 0.36893827288669E-01 0.30087487526228E+03 0.32799815328107E+03
 0.32733103225791E+03 0.32727250157648E+03 0.23000000000000E+00 0.00000000000000E+00 0.19011583190307E+00
 0.00000000000000E+00 -.11990319489169E+02 0.48988838653202E-02 0.76501450526299E+00 0.16330250358930E+04
 0.61238438845986E+03 0.10457318057322E+02 0.39214942714958E+01 0.30273193710839E+03 0.38996406919171E+03
 0.29417942204572E+03 0.29963005433537E+03 0.29215000000008E+03 0.29215000000162E+03 0.29416737804701E+03
 0.29962993908026E+03 0.29215000000008E+03 0.29215000000162E+03 0.29417942204572E+03 0.29963005433537E+03
 0.29215000000008E+03 0.29215000000162E+03 0.29416737804701E+03 0.29962993908026E+03 0.29215000000008E+03
 0.29215000000162E+03 0.29847817760274E+03 0.29215000002310E+03 0.11152238805169E+03 0.10742363090438E+03
 0.14141037007697E+03 0.40032593716682E+03 0.25820851523946E+03 0.11237000510895E+03 0.13375355451897E+03
 0.11237000510895E+03 0.31949951651008E+03 0.11205134866949E+03 0.13368710224023E+03 0.11205134866949E+03
 0.31943397630749E+03 0.11237000510895E+03 0.13375355451897E+03 0.11237000510895E+03 0.31949951651008E+03
 0.11205134866949E+03 0.13368710224023E+03 0.11205134866949E+03 0.31943397630749E+03 0.21617277930766E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35257619623198E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17198176525694E+00 0.00000000000000E+00 0.00000000000000E+00 0.17198176525694E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18217742991367E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18217742991367E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256785184840E+00 0.18519460859027E+00 0.32799815328107E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    370.01652500
 0.99443208644118E+00 0.30166215758473E+03 0.45045560968358E+03 0.40113394867710E+03 0.38715577162137E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15565432747860E+00 0.00000000000000E+00 -.17710657957266E+02
 0.96829823556166E-03 0.10444900100844E+01 0.80000000000000E+04 0.30000000000000E+04 0.76592403208850E+01
 0.28722151203319E+01 0.31586526568808E+03 0.29215000013934E+03 0.31558737563221E+03 0.34522057018277E+03
 0.29215000000849E+03 0.29215000002274E+03 0.31283698201483E+03 0.34518034403131E+03 0.29215000000675E+03
 0.29215000002266E+03 0.31558737563221E+03 0.34522057018277E+03 0.29215000000849E+03 0.29215000002274E+03
 0.31283698201483E+03 0.34518034403131E+03 0.29215000000675E+03 0.29215000002266E+03 0.39101306092558E+03
 0.30341370718079E+03 0.15156478838188E+04 0.14544524313762E+04 0.77580439451404E+03 0.13751043691890E+04
 0.59542095270242E+03 0.10152231894609E+04 0.12543546011602E+04 0.96652399807927E+03 0.20726281325186E+04
 0.90337190931742E+03 0.12533874188611E+04 0.86681326939383E+03 0.20720496055643E+04 0.10152231894609E+04
 0.12543546011602E+04 0.96652399807927E+03 0.20726281325186E+04 0.90337190931742E+03 0.12533874188611E+04
 0.86681326939383E+03 0.20720496055643E+04 0.16946561573554E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52550615924775E+03 0.12948349804875E+01
 0.12948349804875E+01 0.13173138446492E+01 0.29634027004370E-01 0.30183269051810E+03 0.32808360269426E+03
 0.32756498920071E+03 0.32752085141417E+03 0.23000000000000E+00 0.00000000000000E+00 0.18873028417068E+00
 0.00000000000000E+00 -.12002823197505E+02 0.60990211607014E-02 0.79178471042547E+00 0.13116858901142E+04
 0.49188220879282E+03 0.10103756607906E+02 0.37889087279646E+01 0.30341105980037E+03 0.39103559745316E+03
 0.29426443127381E+03 0.29978781866372E+03 0.29215000000011E+03 0.29215000000238E+03 0.29425266322084E+03
 0.29978759719146E+03 0.29215000000011E+03 0.29215000000238E+03 0.29426443127381E+03 0.29978781866372E+03
 0.29215000000011E+03 0.29215000000238E+03 0.29425266322084E+03 0.29978759719146E+03 0.29215000000011E+03
 0.29215000000238E+03 0.29861455603716E+03 0.29215000003271E+03 0.11103493510383E+03 0.10774184353970E+03
 0.14387899748138E+03 0.40237204885395E+03 0.25777365638516E+03 0.11558430691116E+03 0.13602777913113E+03
 0.11558430691116E+03 0.32104567341222E+03 0.11531440860692E+03 0.13595554426938E+03 0.11531440860692E+03
 0.32097518879328E+03 0.11558430691116E+03 0.13602777913113E+03 0.11558430691116E+03 0.32104567341222E+03
 0.11531440860692E+03 0.13595554426938E+03 0.11531440860692E+03 0.32097518879328E+03 0.21751437575278E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35283043418313E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17214058959322E+00 0.00000000000000E+00 0.00000000000000E+00 0.17214058959322E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18229605136901E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18229605136901E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256753850703E+00 0.18514601559345E+00 0.32808360269426E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    381.03683184
 0.99456381876723E+00 0.30197688135019E+03 0.45093018185075E+03 0.40154899406281E+03 0.38755475391809E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15461308147281E+00 0.00000000000000E+00 -.17758318091143E+02
 0.96728860898459E-03 0.10580109039805E+01 0.80000000000000E+04 0.30000000000000E+04 0.75613587439427E+01
 0.28355095289785E+01 0.31638320407347E+03 0.29215000020103E+03 0.31608450061491E+03 0.34616020041565E+03
 0.29215000001278E+03 0.29215000003419E+03 0.31329609034073E+03 0.34612047007781E+03 0.29215000001017E+03
 0.29215000003406E+03 0.31608450061491E+03 0.34616020041565E+03 0.29215000001278E+03 0.29215000003419E+03
 0.31329609034073E+03 0.34612047007780E+03 0.29215000001017E+03 0.29215000003406E+03 0.39216905837716E+03
 0.30417675061239E+03 0.15219736439087E+04 0.14596604586478E+04 0.77354150883166E+03 0.13643875600183E+04
 0.58697834364245E+03 0.10192426544528E+04 0.12584635199520E+04 0.96974143224426E+03 0.20718427062916E+04
 0.90762507592815E+03 0.12575274629700E+04 0.87047253866744E+03 0.20712897921875E+04 0.10192426544528E+04
 0.12584635199520E+04 0.96974143224426E+03 0.20718427062916E+04 0.90762507592815E+03 0.12575274629700E+04
 0.87047253866744E+03 0.20712897921876E+04 0.16936680911915E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52594744137647E+03 0.12948353316390E+01
 0.12948353316390E+01 0.13613950720420E+01 0.23030555652611E-01 0.30288915080473E+03 0.32819887538293E+03
 0.32781027736930E+03 0.32777834525322E+03 0.23000000000000E+00 0.00000000000000E+00 0.18719812246943E+00
 0.00000000000000E+00 -.12019977990792E+02 0.78477716073769E-02 0.82129971147245E+00 0.10193976583722E+04
 0.38227412188958E+03 0.97406584810037E+01 0.36527469303764E+01 0.30417415837168E+03 0.39219006492472E+03
 0.29436019422007E+03 0.29996011389461E+03 0.29215000000015E+03 0.29215000000359E+03 0.29434878055241E+03
 0.29995977772311E+03 0.29215000000015E+03 0.29215000000359E+03 0.29436019422007E+03 0.29996011389461E+03
 0.29215000000015E+03 0.29215000000359E+03 0.29434878055241E+03 0.29995977772311E+03 0.29215000000015E+03
 0.29215000000359E+03 0.29876355183080E+03 0.29215000004746E+03 0.11027371524080E+03 0.10777596276417E+03
 0.14653658970079E+03 0.40482943143384E+03 0.25756015878454E+03 0.11910326762714E+03 0.13848074281979E+03
 0.11910326762714E+03 0.32286246260261E+03 0.11888255559632E+03 0.13840270780651E+03 0.11888255559632E+03
 0.32278708060938E+03 0.11910326762714E+03 0.13848074281979E+03 0.11910326762714E+03 0.32286246260261E+03
 0.11888255559632E+03 0.13840270780651E+03 0.11888255559632E+03 0.32278708060938E+03 0.21893618229067E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35324560152606E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17233052487056E+00 0.00000000000000E+00 0.00000000000000E+00 0.17233052487056E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18241585183473E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18241585183473E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256699889459E+00 0.18508037700690E+00 0.32819887538293E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    390.98461999
 0.99467368020052E+00 0.30225629964502E+03 0.45135879850424E+03 0.40192268808177E+03 0.38791294990396E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15372035722796E+00 0.00000000000000E+00 -.17794988918114E+02
 0.96639405735108E-03 0.10695291215442E+01 0.80000000000000E+04 0.30000000000000E+04 0.74799272304519E+01
 0.28049727114195E+01 0.31684471280527E+03 0.29215000027692E+03 0.31652755150548E+03 0.34699430939966E+03
 0.29215000001826E+03 0.29215000004882E+03 0.31370562001733E+03 0.34695500704219E+03 0.29215000001456E+03
 0.29215000004865E+03 0.31652755150548E+03 0.34699430939966E+03 0.29215000001826E+03 0.29215000004882E+03
 0.31370562001733E+03 0.34695500704219E+03 0.29215000001456E+03 0.29215000004865E+03 0.39318423419868E+03
 0.30487774775902E+03 0.15275269273825E+04 0.14642087226820E+04 0.77153362173177E+03 0.13551161704232E+04
 0.57972488058275E+03 0.10227726896949E+04 0.12620207855673E+04 0.97254973448872E+03 0.20711286755374E+04
 0.91136468033853E+03 0.12611106970383E+04 0.87367521368031E+03 0.20705969732938E+04 0.10227726896949E+04
 0.12620207855673E+04 0.97254973448872E+03 0.20711286755374E+04 0.91136468033853E+03 0.12611106970383E+04
 0.87367521368031E+03 0.20705969732938E+04 0.16928165384114E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52634672871109E+03 0.12948356018233E+01
 0.12948356018233E+01 0.14011862246411E+01 0.18333947253256E-01 0.30382304772069E+03 0.32831977471418E+03
 0.32802036024713E+03 0.32799653752328E+03 0.23000000000000E+00 0.00000000000000E+00 0.18580761292482E+00
 0.00000000000000E+00 -.12030510903825E+02 0.98581350098048E-02 0.84800869940769E+00 0.81151252159189E+03
 0.30431719559696E+03 0.94338654846204E+01 0.35376995567327E+01 0.30487521606958E+03 0.39320391712083E+03
 0.29444882051699E+03 0.30011519930359E+03 0.29215000000022E+03 0.29215000000515E+03 0.29443775396291E+03
 0.30011476094847E+03 0.29215000000022E+03 0.29215000000515E+03 0.29444882051699E+03 0.30011519930359E+03
 0.29215000000022E+03 0.29215000000515E+03 0.29443775396291E+03 0.30011476094847E+03 0.29215000000022E+03
 0.29215000000515E+03 0.29889767654772E+03 0.29215000006570E+03 0.10938918143397E+03 0.10747970266639E+03
 0.14888187107764E+03 0.40713899830017E+03 0.25751271786714E+03 0.12225965448759E+03 0.14064795067887E+03
 0.12225965448759E+03 0.32462217258428E+03 0.12207726720435E+03 0.14056515653953E+03 0.12207726720435E+03
 0.32454283453144E+03 0.12225965448759E+03 0.14064795067887E+03 0.12225965448759E+03 0.32462217258428E+03
 0.12207726720435E+03 0.14056515653953E+03 0.12207726720435E+03 0.32454283453144E+03 0.22014344755042E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35358917696892E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17244788815032E+00 0.00000000000000E+00 0.00000000000000E+00 0.17244788815032E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18250471746703E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18250471746703E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256657612830E+00 0.18501175115617E+00 0.32831977471418E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    400.01102747
 0.99477058967187E+00 0.30250589925160E+03 0.45174647885903E+03 0.40225976573268E+03 0.38823534930972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15294728934868E+00 0.00000000000000E+00 -.17820184131083E+02
 0.96559643911493E-03 0.10794464663112E+01 0.80000000000000E+04 0.30000000000000E+04 0.74112058816017E+01
 0.27792022056006E+01 0.31725758141043E+03 0.29215000036923E+03 0.31692402182181E+03 0.34774015314635E+03
 0.29215000002521E+03 0.29215000006732E+03 0.31407219871525E+03 0.34770121717882E+03 0.29215000002014E+03
 0.29215000006708E+03 0.31692402182181E+03 0.34774015314635E+03 0.29215000002521E+03 0.29215000006732E+03
 0.31407219871525E+03 0.34770121717882E+03 0.29215000002014E+03 0.29215000006708E+03 0.39408991591420E+03
 0.30552298775193E+03 0.15324407570887E+04 0.14682188130300E+04 0.76964263835758E+03 0.13468245207519E+04
 0.57333366920256E+03 0.10258973827740E+04 0.12651178797979E+04 0.97502497355929E+03 0.20704526299819E+04
 0.91467898197886E+03 0.12642297328340E+04 0.87650551935761E+03 0.20699387874165E+04 0.10258973827740E+04
 0.12651178797979E+04 0.97502497355929E+03 0.20704526299819E+04 0.91467898197886E+03 0.12642297328340E+04
 0.87650551935761E+03 0.20699387874165E+04 0.16920193647303E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52670788892647E+03 0.12948357874574E+01
 0.12948357874574E+01 0.14372918545341E+01 0.14902695177457E-01 0.30464889519148E+03 0.32843842834766E+03
 0.32820207624030E+03 0.32818381742317E+03 0.23000000000000E+00 0.00000000000000E+00 0.18454587574341E+00
 0.00000000000000E+00 -.12032868498733E+02 0.12127908019102E-01 0.87219200244464E+00 0.65963560965334E+03
 0.24736335362000E+03 0.91722923135927E+01 0.34396096175973E+01 0.30552051867989E+03 0.39410846210456E+03
 0.29452979213969E+03 0.30025505842956E+03 0.29215000000030E+03 0.29215000000712E+03 0.29451903959989E+03
 0.30025453005115E+03 0.29215000000030E+03 0.29215000000713E+03 0.29452979213969E+03 0.30025505842956E+03
 0.29215000000030E+03 0.29215000000712E+03 0.29451903959989E+03 0.30025453005115E+03 0.29215000000030E+03
 0.29215000000713E+03 0.29901847221183E+03 0.29215000008800E+03 0.10841537210347E+03 0.10693241340673E+03
 0.15094942289997E+03 0.40925504008615E+03 0.25755087007167E+03 0.12510509471485E+03 0.14255788902593E+03
 0.12510509471485E+03 0.32625990218475E+03 0.12495305125094E+03 0.14247111127195E+03 0.12495305125094E+03
 0.32617728732984E+03 0.12510509471485E+03 0.14255788902593E+03 0.12510509471485E+03 0.32625990218475E+03
 0.12495305125094E+03 0.14247111127195E+03 0.12495305125094E+03 0.32617728732984E+03 0.22115341318141E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35387815516408E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17248559920114E+00 0.00000000000000E+00 0.00000000000000E+00 0.17248559920114E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18256758180934E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18256758180934E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256635450161E+00 0.18494467087361E+00 0.32843842834766E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    410.50798248
 0.99484591907841E+00 0.30279319306092E+03 0.45220094797388E+03 0.40265504955594E+03 0.38861248227519E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15209019865694E+00 0.00000000000000E+00 -.17857763326002E+02
 0.96467991180331E-03 0.10903622965145E+01 0.80000000000000E+04 0.30000000000000E+04 0.73370108500386E+01
 0.27513790687645E+01 0.31773509904329E+03 0.29215000050380E+03 0.31738249721260E+03 0.34859558690755E+03
 0.29215000003564E+03 0.29215000009507E+03 0.31449680786128E+03 0.34855707321549E+03 0.29215000002852E+03
 0.29215000009473E+03 0.31738249721260E+03 0.34859558690755E+03 0.29215000003564E+03 0.29215000009507E+03
 0.31449680786128E+03 0.34855707321549E+03 0.29215000002852E+03 0.29215000009473E+03 0.39510826280666E+03
 0.30627894637856E+03 0.15380680916756E+04 0.14727897805071E+04 0.76767523834694E+03 0.13378948737780E+04
 0.56638125923928E+03 0.10294738172480E+04 0.12686522157317E+04 0.97784214600267E+03 0.20697512105126E+04
 0.91847288032087E+03 0.12677878655179E+04 0.87972938756767E+03 0.20692564814720E+04 0.10294738172480E+04
 0.12686522157317E+04 0.97784214600267E+03 0.20697512105126E+04 0.91847288032087E+03 0.12677878655179E+04
 0.87972938756767E+03 0.20692564814720E+04 0.16912708611460E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52713590713213E+03 0.12948360643347E+01
 0.12948360643347E+01 0.14792796745759E+01 0.11706166729943E-01 0.30556804378779E+03 0.32858327800840E+03
 0.32840366456232E+03 0.32839024962865E+03 0.23000000000000E+00 0.00000000000000E+00 0.18308489655510E+00
 0.00000000000000E+00 -.12044107864470E+02 0.15439597697468E-01 0.90014058442667E+00 0.51814821582507E+03
 0.19430558093440E+03 0.88875006175791E+01 0.33328127315921E+01 0.30627656502233E+03 0.39512543765821E+03
 0.29462730993466E+03 0.30041830917294E+03 0.29215000000044E+03 0.29215000001010E+03 0.29461694292105E+03
 0.30041767596331E+03 0.29215000000043E+03 0.29215000001010E+03 0.29462730993466E+03 0.30041830917294E+03
 0.29215000000044E+03 0.29215000001010E+03 0.29461694292105E+03 0.30041767596331E+03 0.29215000000043E+03
 0.29215000001010E+03 0.29915957293027E+03 0.29215000012065E+03 0.10710317331039E+03 0.10598040982084E+03
 0.15325312097411E+03 0.41165937934391E+03 0.25763999276494E+03 0.12835016426125E+03 0.14468513024765E+03
 0.12835016426125E+03 0.32813911428398E+03 0.12822856875475E+03 0.14459424930459E+03 0.12822856875475E+03
 0.32805321874704E+03 0.12835016426125E+03 0.14468513024765E+03 0.12835016426125E+03 0.32813911428398E+03
 0.12822856875474E+03 0.14459424930459E+03 0.12822856875474E+03 0.32805321874704E+03 0.22219719023556E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35419105401756E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17260090933077E+00 0.00000000000000E+00 0.00000000000000E+00 0.17260090933077E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18261087470519E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18261087470519E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256581138788E+00 0.18486253921826E+00 0.32858327800840E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    420.10856459
 0.99491346598587E+00 0.30305196856254E+03 0.45261393369199E+03 0.40301352932312E+03 0.38895397885462E+03
 0.22999999999399E+00 0.00000000000000E+00 0.15134372207723E+00 0.00000000000000E+00 -.17892790521856E+02
 0.96385584019919E-03 0.10998097631807E+01 0.80000000000000E+04 0.30000000000000E+04 0.72739852543801E+01
 0.27277444703925E+01 0.31816534251394E+03 0.29215000066630E+03 0.31779571402398E+03 0.34936762611041E+03
 0.29215000004870E+03 0.29215000012978E+03 0.31487947740945E+03 0.34932947279950E+03 0.29215000003903E+03
 0.29215000012933E+03 0.31779571402398E+03 0.34936762611041E+03 0.29215000004870E+03 0.29215000012978E+03
 0.31487947740945E+03 0.34932947279950E+03 0.29215000003903E+03 0.29215000012933E+03 0.39603114634385E+03
 0.30697815865590E+03 0.15430895905367E+04 0.14768563868948E+04 0.76570894221616E+03 0.13296502542151E+04
 0.56011276728785E+03 0.10326662510838E+04 0.12717395236771E+04 0.98034786585353E+03 0.20690481836630E+04
 0.92186431261393E+03 0.12708953718000E+04 0.88260521550085E+03 0.20685696388323E+04 0.10326662510838E+04
 0.12717395236771E+04 0.98034786585353E+03 0.20690481836630E+04 0.92186431261393E+03 0.12708953718000E+04
 0.88260521550085E+03 0.20685696388323E+04 0.16905015825152E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52752473936285E+03 0.12948363224094E+01
 0.12948363224094E+01 0.15176820030341E+01 0.93826125253798E-02 0.30639469239748E+03 0.32872028987904E+03
 0.32858064159199E+03 0.32857053430746E+03 0.23000000000000E+00 0.00000000000000E+00 0.18175922485790E+00
 0.00000000000000E+00 -.12056312383136E+02 0.19263131120741E-01 0.92545489921493E+00 0.41530112367799E+03
 0.15573792137925E+03 0.86443974814833E+01 0.32416490555563E+01 0.30697586233577E+03 0.39604721755508E+03
 0.29471605328010E+03 0.30056613017651E+03 0.29215000000061E+03 0.29215000001384E+03 0.29470601752714E+03
 0.30056540524143E+03 0.29215000000061E+03 0.29215000001384E+03 0.29471605328010E+03 0.30056613017651E+03
 0.29215000000061E+03 0.29215000001384E+03 0.29470601752714E+03 0.30056540524143E+03 0.29215000000061E+03
 0.29215000001384E+03 0.29928709085921E+03 0.29215000016025E+03 0.10571723320621E+03 0.10485670016000E+03
 0.15527528564213E+03 0.41382414035307E+03 0.25777247828274E+03 0.13128574110301E+03 0.14654981495000E+03
 0.13128574110301E+03 0.32984147864795E+03 0.13118807528424E+03 0.14645551569421E+03 0.13118807528424E+03
 0.32975288412533E+03 0.13128574110301E+03 0.14654981495000E+03 0.13128574110301E+03 0.32984147864795E+03
 0.13118807528424E+03 0.14645551569421E+03 0.13118807528424E+03 0.32975288412532E+03 0.22305261285580E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35446044944794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17271437184671E+00 0.00000000000000E+00 0.00000000000000E+00 0.17271437184671E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18265910227397E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18265910227397E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256523147883E+00 0.18478484456573E+00 0.32872028987904E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    431.62926313
 0.99496925472258E+00 0.30335843654546E+03 0.45310915402589E+03 0.40344336743733E+03 0.38936275627055E+03
 0.22999999978693E+00 0.00000000000000E+00 0.15049277187918E+00 0.00000000000000E+00 -.17913855573173E+02
 0.96288190423313E-03 0.11104987840957E+01 0.80000000000000E+04 0.30000000000000E+04 0.72039700669410E+01
 0.27014887751029E+01 0.31867644258203E+03 0.29215000092038E+03 0.31828659025102E+03 0.35028052576895E+03
 0.29215000006987E+03 0.29215000018593E+03 0.31533452353073E+03 0.35024278884262E+03 0.29215000005611E+03
 0.29215000018529E+03 0.31828659025102E+03 0.35028052576895E+03 0.29215000006987E+03 0.29215000018593E+03
 0.31533452353073E+03 0.35024278884262E+03 0.29215000005611E+03 0.29215000018529E+03 0.39711058666351E+03
 0.30782353834291E+03 0.15489967731716E+04 0.14816203816274E+04 0.76342974855647E+03 0.13201895684384E+04
 0.55294267113919E+03 0.10364209546076E+04 0.12753292167905E+04 0.98328022213507E+03 0.20682236984206E+04
 0.92585560196854E+03 0.12745075880786E+04 0.88597625242598E+03 0.20677630409801E+04 0.10364209546076E+04
 0.12753292167905E+04 0.98328022213507E+03 0.20682236984206E+04 0.92585560196854E+03 0.12745075880786E+04
 0.88597625242598E+03 0.20677630409801E+04 0.16896524794206E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52799401903931E+03 0.12948364776133E+01
 0.12948364776133E+01 0.15637647971838E+01 0.71916317250644E-02 0.30736050687915E+03 0.32888873293777E+03
 0.32878551755543E+03 0.32877832519253E+03 0.23000000000000E+00 0.00000000000000E+00 0.18018694995835E+00
 0.00000000000000E+00 -.12048177852756E+02 0.25131776998402E-01 0.95542461649386E+00 0.31832209877195E+03
 0.11937078703948E+03 0.83732404021133E+01 0.31399651507925E+01 0.30782135104817E+03 0.39712538401837E+03
 0.29482435412013E+03 0.30074289781487E+03 0.29215000000091E+03 0.29215000001991E+03 0.29481470818455E+03
 0.30074206528408E+03 0.29215000000090E+03 0.29215000001992E+03 0.29482435412013E+03 0.30074289781487E+03
 0.29215000000091E+03 0.29215000001991E+03 0.29481470818455E+03 0.30074206528409E+03 0.29215000000090E+03
 0.29215000001992E+03 0.29943952543022E+03 0.29215000022244E+03 0.10384636164057E+03 0.10321611530204E+03
 0.15758896706030E+03 0.41633521954889E+03 0.25795830765329E+03 0.13476010881461E+03 0.14867906325412E+03
 0.13476010881461E+03 0.33182049830031E+03 0.13468673981077E+03 0.14858104957101E+03 0.13468673981077E+03
 0.33172903262303E+03 0.13476010881461E+03 0.14867906325412E+03 0.13476010881461E+03 0.33182049830030E+03
 0.13468673981077E+03 0.14858104957101E+03 0.13468673981077E+03 0.33172903262303E+03 0.22394406723485E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35476057074142E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17266856368403E+00 0.00000000000000E+00 0.00000000000000E+00 0.17266856368403E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18261252413150E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18261252413150E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256516852569E+00 0.18469015295166E+00 0.32888873293777E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    441.95408789
 0.99497740941185E+00 0.30363021202279E+03 0.45355391979246E+03 0.40383035233710E+03 0.38973029532536E+03
 0.22999999984163E+00 0.00000000000000E+00 0.14976890758717E+00 0.00000000000000E+00 -.17938829437398E+02
 0.96201980393542E-03 0.11195149748588E+01 0.80000000000000E+04 0.30000000000000E+04 0.71459517555887E+01
 0.26797319083458E+01 0.31913018967599E+03 0.29215000121265E+03 0.31872235264204E+03 0.35108710223221E+03
 0.29215000009504E+03 0.29215000025261E+03 0.31573889202261E+03 0.35104972451116E+03 0.29215000007644E+03
 0.29215000025176E+03 0.31872235264204E+03 0.35108710223221E+03 0.29215000009504E+03 0.29215000025261E+03
 0.31573889202261E+03 0.35104972451116E+03 0.29215000007644E+03 0.29215000025176E+03 0.39805515480323E+03
 0.30858534386654E+03 0.15542182421270E+04 0.14858222057864E+04 0.76146761243870E+03 0.13120942493700E+04
 0.54681929886907E+03 0.10397360194547E+04 0.12784737925512E+04 0.98586192376339E+03 0.20675364626948E+04
 0.92938030990002E+03 0.12776709045676E+04 0.88894576241565E+03 0.20670905485108E+04 0.10397360194546E+04
 0.12784737925512E+04 0.98586192376339E+03 0.20675364626948E+04 0.92938030990002E+03 0.12776709045676E+04
 0.88894576241565E+03 0.20670905485108E+04 0.16889876040070E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52842082595742E+03 0.12948366616168E+01
 0.12948366616168E+01 0.16050640962304E+01 0.56654867826791E-02 0.30820608346534E+03 0.32904176726894E+03
 0.32896307107481E+03 0.32895777241580E+03 0.23000000000000E+00 0.00000000000000E+00 0.17879931031514E+00
 0.00000000000000E+00 -.12047766664180E+02 0.31901667572324E-01 0.98182911682942E+00 0.25077059002835E+03
 0.94038971260631E+02 0.81480573990656E+01 0.30555215246496E+01 0.30858326561192E+03 0.39806882892087E+03
 0.29492292125183E+03 0.30090059978102E+03 0.29215000000128E+03 0.29215000002715E+03 0.29491361307844E+03
 0.30089967328210E+03 0.29215000000126E+03 0.29215000002716E+03 0.29492292125183E+03 0.30090059978102E+03
 0.29215000000128E+03 0.29215000002715E+03 0.29491361307844E+03 0.30089967328210E+03 0.29215000000126E+03
 0.29215000002716E+03 0.29957549672598E+03 0.29215000029426E+03 0.10198864204627E+03 0.10150703470639E+03
 0.15955506045927E+03 0.41849861184300E+03 0.25814577608143E+03 0.13782123165217E+03 0.15048436184276E+03
 0.13782123165217E+03 0.33352672503854E+03 0.13776604959942E+03 0.15038337359396E+03 0.13776604959942E+03
 0.33343302107533E+03 0.13782123165217E+03 0.15048436184276E+03 0.13782123165217E+03 0.33352672503854E+03
 0.13776604959942E+03 0.15038337359395E+03 0.13776604959942E+03 0.33343302107532E+03 0.22462639229024E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35501718738862E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17268042967181E+00 0.00000000000000E+00 0.00000000000000E+00 0.17268042967181E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18258108534810E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18258108534810E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256489683123E+00 0.18460396534263E+00 0.32904176726894E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    450.24278223
 0.99498255162471E+00 0.30384582520115E+03 0.45390942654483E+03 0.40413920488785E+03 0.39002326252018E+03
 0.22999999985130E+00 0.00000000000000E+00 0.14921281930523E+00 0.00000000000000E+00 -.17951696210910E+02
 0.96133701934834E-03 0.11263951610718E+01 0.80000000000000E+04 0.30000000000000E+04 0.71023032382242E+01
 0.26633637143341E+01 0.31949094103054E+03 0.29215000150578E+03 0.31906881794320E+03 0.35172700673858E+03
 0.29215000012105E+03 0.29215000032141E+03 0.31606057948411E+03 0.35168990538167E+03 0.29215000009749E+03
 0.29215000032033E+03 0.31906881794320E+03 0.35172700673858E+03 0.29215000012105E+03 0.29215000032141E+03
 0.31606057948411E+03 0.35168990538167E+03 0.29215000009749E+03 0.29215000032033E+03 0.39880110136762E+03
 0.30920014059765E+03 0.15583328777652E+04 0.14891214499177E+04 0.75988031122669E+03 0.13057240608927E+04
 0.54204434810982E+03 0.10423495631711E+04 0.12809196424537E+04 0.98788891601374E+03 0.20669708345825E+04
 0.93216132785187E+03 0.12801308832395E+04 0.89128152080083E+03 0.20665359694809E+04 0.10423495631711E+04
 0.12809196424537E+04 0.98788891601374E+03 0.20669708345825E+04 0.93216132785187E+03 0.12801308832395E+04
 0.89128152080083E+03 0.20665359694809E+04 0.16884523204172E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52876200454538E+03 0.12948367564172E+01
 0.12948367564172E+01 0.16382188735889E+01 0.46768673395299E-02 0.30887999807176E+03 0.32916680556292E+03
 0.32910355309134E+03 0.32909941255797E+03 0.23000000000000E+00 0.00000000000000E+00 0.17770182966161E+00
 0.00000000000000E+00 -.12040545199855E+02 0.38645199040562E-01 0.10026786484778E+01 0.20701148392594E+03
 0.77629306472227E+02 0.79786280600923E+01 0.29919855225346E+01 0.30919814592276E+03 0.39881396512133E+03
 0.29500254749545E+03 0.30102641827283E+03 0.29215000000167E+03 0.29215000003466E+03 0.29499349681234E+03
 0.30102541849165E+03 0.29215000000165E+03 0.29215000003466E+03 0.29500254749545E+03 0.30102641827283E+03
 0.29215000000167E+03 0.29215000003466E+03 0.29499349681234E+03 0.30102541849165E+03 0.29215000000165E+03
 0.29215000003466E+03 0.29968391210723E+03 0.29215000036654E+03 0.10038294218016E+03 0.99999692618145E+02
 0.16107376709690E+03 0.42020050473478E+03 0.25832136880239E+03 0.14026056428561E+03 0.15187585069578E+03
 0.14026056428561E+03 0.33486796589843E+03 0.14021788235682E+03 0.15177263899133E+03 0.14021788235682E+03
 0.33477261262334E+03 0.14026056428561E+03 0.15187585069578E+03 0.14026056428561E+03 0.33486796589843E+03
 0.14021788235682E+03 0.15177263899133E+03 0.14021788235682E+03 0.33477261262334E+03 0.22511498384804E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35521654612973E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17262874608473E+00 0.00000000000000E+00 0.00000000000000E+00 0.17262874608473E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18254978098364E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18254978098364E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256486720260E+00 0.18453382259880E+00 0.32916680556292E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    461.61729815
 0.99496405037284E+00 0.30413888312800E+03 0.45439645196396E+03 0.40456282553456E+03 0.39042475239234E+03
 0.22999999986843E+00 0.00000000000000E+00 0.14848390015775E+00 0.00000000000000E+00 -.17982908939652E+02
 0.96041041165340E-03 0.11353446768291E+01 0.80000000000000E+04 0.30000000000000E+04 0.70463183236503E+01
 0.26423693713689E+01 0.31998101681362E+03 0.29215000200617E+03 0.31953951213205E+03 0.35259522694014E+03
 0.29215000016678E+03 0.29215000044223E+03 0.31649778179816E+03 0.35255848753885E+03 0.29215000013457E+03
 0.29215000044076E+03 0.31953951213205E+03 0.35259522694014E+03 0.29215000016678E+03 0.29215000044223E+03
 0.31649778179816E+03 0.35255848753884E+03 0.29215000013457E+03 0.29215000044076E+03 0.39981267124923E+03
 0.31004830209892E+03 0.15639076768252E+04 0.14935867098324E+04 0.75764997220165E+03 0.12970819552213E+04
 0.53564373315863E+03 0.10458882126187E+04 0.12841927339126E+04 0.99062950308958E+03 0.20662045129567E+04
 0.93592858127179E+03 0.12834221124760E+04 0.89444228728277E+03 0.20657837440857E+04 0.10458882126187E+04
 0.12841927339126E+04 0.99062950308958E+03 0.20662045129567E+04 0.93592858127179E+03 0.12834221124760E+04
 0.89444228728277E+03 0.20657837440857E+04 0.16877277754438E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52923264400930E+03 0.12948369863879E+01
 0.12948369863879E+01 0.16837169372770E+01 0.35939039795041E-02 0.30979511391005E+03 0.32934052960197E+03
 0.32929370003715E+03 0.32929075301748E+03 0.23000000000000E+00 0.00000000000000E+00 0.17622227465780E+00
 0.00000000000000E+00 -.12044648832588E+02 0.50290286368827E-01 0.10307400799640E+01 0.15907644552525E+03
 0.59653667071969E+02 0.77614135275301E+01 0.29105300728238E+01 0.31004643359546E+03 0.39982443678214E+03
 0.29511201197375E+03 0.30119769886951E+03 0.29215000000239E+03 0.29215000004788E+03 0.29510329360616E+03
 0.30119660173871E+03 0.29215000000236E+03 0.29215000004789E+03 0.29511201197375E+03 0.30119769886951E+03
 0.29215000000239E+03 0.29215000004788E+03 0.29510329360616E+03 0.30119660173871E+03 0.29215000000236E+03
 0.29215000004789E+03 0.29983144962489E+03 0.29215000049034E+03 0.98017523967254E+02 0.97738229017641E+02
 0.16306899008016E+03 0.42247687936871E+03 0.25859254433815E+03 0.14356266052522E+03 0.15370093223041E+03
 0.14356266052522E+03 0.33665963548776E+03 0.14353446718046E+03 0.15359497657862E+03 0.14353446718046E+03
 0.33656230098382E+03 0.14356266052522E+03 0.15370093223041E+03 0.14356266052522E+03 0.33665963548776E+03
 0.14353446718046E+03 0.15359497657862E+03 0.14353446718046E+03 0.33656230098382E+03 0.22569906200016E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35548288032031E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17267684379644E+00 0.00000000000000E+00 0.00000000000000E+00 0.17267684379644E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18250933295427E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18250933295427E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256441004227E+00 0.18443598665509E+00 0.32934052960197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    471.03183415
 0.99492934408347E+00 0.30437916301585E+03 0.45479928050771E+03 0.40491348423000E+03 0.39075675058914E+03
 0.22999999983604E+00 0.00000000000000E+00 0.14790888732761E+00 0.00000000000000E+00 -.17992907642766E+02
 0.95965215956759E-03 0.11423443827125E+01 0.80000000000000E+04 0.30000000000000E+04 0.70031420656213E+01
 0.26261782746080E+01 0.32038375918177E+03 0.29215000252001E+03 0.31992625944171E+03 0.35330452434127E+03
 0.29215000021513E+03 0.29215000056974E+03 0.31685744339113E+03 0.35326807594509E+03 0.29215000017382E+03
 0.29215000056787E+03 0.31992625944171E+03 0.35330452434127E+03 0.29215000021513E+03 0.29215000056974E+03
 0.31685744339113E+03 0.35326807594509E+03 0.29215000017382E+03 0.29215000056787E+03 0.40062893568658E+03
 0.31075121510798E+03 0.15684591375722E+04 0.14972206660751E+04 0.75591339547316E+03 0.12902976979539E+04
 0.53060473550333E+03 0.10487766103597E+04 0.12868485376215E+04 0.99285815898114E+03 0.20656084178624E+04
 0.93900397206960E+03 0.12860919881480E+04 0.89701395841406E+03 0.20651984752347E+04 0.10487766103596E+04
 0.12868485376215E+04 0.99285815898114E+03 0.20656084178624E+04 0.93900397206960E+03 0.12860919881480E+04
 0.89701395841406E+03 0.20651984752347E+04 0.16872129020745E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.52962435603507E+03 0.12948370600568E+01
 0.12948370600568E+01 0.17213750812732E+01 0.28895008641303E-02 0.31054212222272E+03 0.32948650487337E+03
 0.32945001166667E+03 0.32944778997631E+03 0.23000000000000E+00 0.00000000000000E+00 0.17502239229203E+00
 0.00000000000000E+00 -.12030880796037E+02 0.62550060251974E-01 0.10534553218077E+01 0.12789755865579E+03
 0.47961584495921E+02 0.75940572270992E+01 0.28477714601622E+01 0.31074945502575E+03 0.40063983846248E+03
 0.29520424751255E+03 0.30133906143205E+03 0.29215000000319E+03 0.29215000006189E+03 0.29519579222180E+03
 0.30133788550612E+03 0.29215000000315E+03 0.29215000006191E+03 0.29520424751255E+03 0.30133906143205E+03
 0.29215000000319E+03 0.29215000006189E+03 0.29519579222180E+03 0.30133788550612E+03 0.29215000000315E+03
 0.29215000006191E+03 0.29995323613831E+03 0.29215000061787E+03 0.95946596273796E+02 0.95730856099075E+02
 0.16465039307591E+03 0.42430768280689E+03 0.25883403776559E+03 0.14626824134655E+03 0.15514366378220E+03
 0.14626824134655E+03 0.33809513358636E+03 0.14625001003479E+03 0.15503562515259E+03 0.14625001003479E+03
 0.33799633328769E+03 0.14626824134655E+03 0.15514366378220E+03 0.14626824134655E+03 0.33809513358636E+03
 0.14625001003479E+03 0.15503562515259E+03 0.14625001003479E+03 0.33799633328769E+03 0.22611509520765E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35569524449118E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17257893841617E+00 0.00000000000000E+00 0.00000000000000E+00 0.17257893841617E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18241401056099E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18241401056099E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256451876718E+00 0.18435441559114E+00 0.32948650487337E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    480.43472087
 0.99488546855029E+00 0.30461691128507E+03 0.45519995333380E+03 0.40526232655234E+03 0.39108681151792E+03
 0.22999999985196E+00 0.00000000000000E+00 0.14735868050353E+00 0.00000000000000E+00 -.18007580154343E+02
 0.95890302865355E-03 0.11489920266777E+01 0.80000000000000E+04 0.30000000000000E+04 0.69626244693202E+01
 0.26109841759951E+01 0.32078230751657E+03 0.29215000314556E+03 0.32030898891774E+03 0.35400549583213E+03
 0.29215000027558E+03 0.29215000072896E+03 0.31721351019250E+03 0.35396932567082E+03 0.29215000022297E+03
 0.29215000072660E+03 0.32030898891774E+03 0.35400549583213E+03 0.29215000027558E+03 0.29215000072896E+03
 0.31721351019250E+03 0.35396932567081E+03 0.29215000022297E+03 0.29215000072660E+03 0.40143492680454E+03
 0.31145584129126E+03 0.15729431742308E+04 0.15007945077316E+04 0.75413828731670E+03 0.12835838978315E+04
 0.52567491907825E+03 0.10516219834882E+04 0.12894325808399E+04 0.99504912328857E+03 0.20650061237027E+04
 0.94203526978189E+03 0.12886892548479E+04 0.89954492560079E+03 0.20646062948672E+04 0.10516219834882E+04
 0.12894325808399E+04 0.99504912328857E+03 0.20650061237027E+04 0.94203526978189E+03 0.12886892548479E+04
 0.89954492560078E+03 0.20646062948672E+04 0.16866921619338E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53001513186217E+03 0.12948371681617E+01
 0.12948371681617E+01 0.17589866281465E+01 0.23233566091805E-02 0.31128520334255E+03 0.32963440345804E+03
 0.32960598230113E+03 0.32960430971859E+03 0.23000000000000E+00 0.00000000000000E+00 0.17384748700892E+00
 0.00000000000000E+00 -.12022349133584E+02 0.77791952129004E-01 0.10756587502334E+01 0.10283840141630E+03
 0.38564400531112E+02 0.74373029534361E+01 0.27889886075386E+01 0.31145419170336E+03 0.40144501606275E+03
 0.29529653833591E+03 0.30147920055048E+03 0.29215000000421E+03 0.29215000007946E+03 0.29528832898652E+03
 0.30147794828607E+03 0.29215000000416E+03 0.29215000007947E+03 0.29529653833591E+03 0.30147920055048E+03
 0.29215000000421E+03 0.29215000007946E+03 0.29528832898652E+03 0.30147794828607E+03 0.29215000000416E+03
 0.29215000007947E+03 0.30007394467006E+03 0.29215000077357E+03 0.93771859465194E+02 0.93607876242747E+02
 0.16617502221142E+03 0.42611067506536E+03 0.25910477774289E+03 0.14894925686868E+03 0.15653215984569E+03
 0.14894925686868E+03 0.33950552858323E+03 0.14893942191297E+03 0.15642220388191E+03 0.14893942191297E+03
 0.33940540902994E+03 0.14894925686868E+03 0.15653215984569E+03 0.14894925686868E+03 0.33950552858323E+03
 0.14893942191297E+03 0.15642220388191E+03 0.14893942191297E+03 0.33940540902994E+03 0.22648356728892E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35590502058046E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17252320732092E+00 0.00000000000000E+00 0.00000000000000E+00 0.17252320732092E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18232888396874E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18232888396874E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256446896316E+00 0.18427166278748E+00 0.32963440345804E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    491.05066744
 0.99482621700129E+00 0.30488282184196E+03 0.45565051463997E+03 0.40565463014922E+03 0.39145773527176E+03
 0.22999999987529E+00 0.00000000000000E+00 0.14676488953219E+00 0.00000000000000E+00 -.18025027881052E+02
 0.95806653434477E-03 0.11561070497436E+01 0.80000000000000E+04 0.30000000000000E+04 0.69197744289977E+01
 0.25949154108741E+01 0.32122813624093E+03 0.29215000401267E+03 0.32073711895354E+03 0.35478799629998E+03
 0.29215000036175E+03 0.29215000095559E+03 0.31761202189936E+03 0.35475212697895E+03 0.29215000029315E+03
 0.29215000095252E+03 0.32073711895354E+03 0.35478799629998E+03 0.29215000036175E+03 0.29215000095559E+03
 0.31761202189936E+03 0.35475212697895E+03 0.29215000029315E+03 0.29215000095252E+03 0.40233263666367E+03
 0.31225376271879E+03 0.15779374455762E+04 0.15047679428177E+04 0.75211347338890E+03 0.12761212439661E+04
 0.52024720321020E+03 0.10547910592336E+04 0.12922789490282E+04 0.99748439595778E+03 0.20643300117744E+04
 0.94541302032408E+03 0.12915496384435E+04 0.90236066047381E+03 0.20639408300162E+04 0.10547910592336E+04
 0.12922789490282E+04 0.99748439595778E+03 0.20643300117744E+04 0.94541302032407E+03 0.12915496384435E+04
 0.90236066047381E+03 0.20639408300162E+04 0.16861132572331E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53045578683188E+03 0.12948372967139E+01
 0.12948372967139E+01 0.18014504144143E+01 0.18159634208326E-02 0.31211953171827E+03 0.32980414429100E+03
 0.32978273455130E+03 0.32978152303264E+03 0.23000000000000E+00 0.00000000000000E+00 0.17255016892943E+00
 0.00000000000000E+00 -.12013534902426E+02 0.99527576487956E-01 0.11001275001584E+01 0.80379732756460E+02
 0.30142399783672E+02 0.72718843941705E+01 0.27269566478139E+01 0.31225224025813E+03 0.40234185871216E+03
 0.29540112930624E+03 0.30163630537916E+03 0.29215000000573E+03 0.29215000010455E+03 0.29539317935835E+03
 0.30163496946269E+03 0.29215000000565E+03 0.29215000010457E+03 0.29540112930624E+03 0.30163630537916E+03
 0.29215000000573E+03 0.29215000010455E+03 0.29539317935835E+03 0.30163496946269E+03 0.29215000000565E+03
 0.29215000010457E+03 0.30020926365127E+03 0.29215000099003E+03 0.91201678354260E+02 0.91083087510652E+02
 0.16783782324985E+03 0.42812215214524E+03 0.25944513977914E+03 0.15195418871589E+03 0.15804380457703E+03
 0.15195418871589E+03 0.34107438099182E+03 0.15195223207552E+03 0.15793185685461E+03 0.15195223207552E+03
 0.34097292520558E+03 0.15195418871589E+03 0.15804380457703E+03 0.15195418871589E+03 0.34107438099182E+03
 0.15195223207552E+03 0.15793185685461E+03 0.15195223207552E+03 0.34097292520558E+03 0.22685072450529E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35613895564415E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17246930967874E+00 0.00000000000000E+00 0.00000000000000E+00 0.17246930967874E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18221962800790E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18221962800790E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256438052336E+00 0.18417674503028E+00 0.32980414429100E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    500.08196760
 0.99477797799200E+00 0.30510665489112E+03 0.45603147968616E+03 0.40598591567336E+03 0.39177071657191E+03
 0.22999999992623E+00 0.00000000000000E+00 0.14628150578679E+00 0.00000000000000E+00 -.18044887108680E+02
 0.95736348768785E-03 0.11618525411060E+01 0.80000000000000E+04 0.30000000000000E+04 0.68855553669354E+01
 0.25820832626008E+01 0.32160404614955E+03 0.29215000491548E+03 0.32109810065637E+03 0.35544635645425E+03
 0.29215000045400E+03 0.29215000119780E+03 0.31794821510910E+03 0.35541073278465E+03 0.29215000036840E+03
 0.29215000119399E+03 0.32109810065637E+03 0.35544635645425E+03 0.29215000045400E+03 0.29215000119780E+03
 0.31794821510910E+03 0.35541073278465E+03 0.29215000036840E+03 0.29215000119399E+03 0.40308551077209E+03
 0.31293412417364E+03 0.15821179254753E+04 0.15080834402267E+04 0.75037441386843E+03 0.12698676093946E+04
 0.51574132345680E+03 0.10574453544527E+04 0.12946316866345E+04 0.99951694628487E+03 0.20637409805736E+04
 0.94824394564442E+03 0.12939135968653E+04 0.90471424875336E+03 0.20633602645229E+04 0.10574453544527E+04
 0.12946316866345E+04 0.99951694628487E+03 0.20637409805736E+04 0.94824394564442E+03 0.12939135968653E+04
 0.90471424875336E+03 0.20633602645229E+04 0.16856138291838E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53082803637783E+03 0.12948374430338E+01
 0.12948374430338E+01 0.18375756150469E+01 0.14723903731984E-02 0.31282677400282E+03 0.32995135053443E+03
 0.32993454116001E+03 0.32993362194389E+03 0.23000000000000E+00 0.00000000000000E+00 0.17147131318837E+00
 0.00000000000000E+00 -.12012554967361E+02 0.12275170694320E+00 0.11204334365998E+01 0.65172209814580E+02
 0.24439578680468E+02 0.71400939481757E+01 0.26775352305659E+01 0.31293270797666E+03 0.40309404781885E+03
 0.29549052394110E+03 0.30176910541841E+03 0.29215000000742E+03 0.29215000013148E+03 0.29548278009717E+03
 0.30176770028146E+03 0.29215000000731E+03 0.29215000013151E+03 0.29549052394110E+03 0.30176910541841E+03
 0.29215000000742E+03 0.29215000013148E+03 0.29548278009717E+03 0.30176770028146E+03 0.29215000000731E+03
 0.29215000013151E+03 0.30032364304461E+03 0.29215000121603E+03 0.88930542029569E+02 0.88842865134890E+02
 0.16921255476089E+03 0.42983194850233E+03 0.25977333096763E+03 0.15450209416823E+03 0.15929137183906E+03
 0.15450209416823E+03 0.34240505404312E+03 0.15450569931625E+03 0.15917783430764E+03 0.15450569931625E+03
 0.34230255131910E+03 0.15450209416823E+03 0.15929137183906E+03 0.15450209416823E+03 0.34240505404312E+03
 0.15450569931624E+03 0.15917783430764E+03 0.15450569931624E+03 0.34230255131911E+03 0.22713585035572E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35634001342209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17246903750274E+00 0.00000000000000E+00 0.00000000000000E+00 0.17246903750274E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18217399485756E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18217399485756E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256410923832E+00 0.18409428781141E+00 0.32995135053443E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    511.37109279
 0.99469553022917E+00 0.30538422947422E+03 0.45650616129618E+03 0.40639939126188E+03 0.39216121508113E+03
 0.22999999992754E+00 0.00000000000000E+00 0.14570394192699E+00 0.00000000000000E+00 -.18052637729870E+02
 0.95649323280022E-03 0.11686551403475E+01 0.80000000000000E+04 0.30000000000000E+04 0.68454753877360E+01
 0.25670532704010E+01 0.32207032730610E+03 0.29215000628637E+03 0.32154582081761E+03 0.35625991920606E+03
 0.29215000059794E+03 0.29215000157510E+03 0.31836552360383E+03 0.35622459189446E+03 0.29215000048600E+03
 0.29215000157016E+03 0.32154582081761E+03 0.35625991920606E+03 0.29215000059794E+03 0.29215000157510E+03
 0.31836552360383E+03 0.35622459189446E+03 0.29215000048600E+03 0.29215000157016E+03 0.40401036835518E+03
 0.31378480320868E+03 0.15872874444659E+04 0.15121771370291E+04 0.74824640182088E+03 0.12622838750960E+04
 0.51029624126601E+03 0.10607260671184E+04 0.12975174600944E+04 0.10020246448192E+04 0.20630304285755E+04
 0.95174345658409E+03 0.12968125819492E+04 0.90761873192876E+03 0.20626595978939E+04 0.10607260671184E+04
 0.12975174600944E+04 0.10020246448192E+04 0.20630304285755E+04 0.95174345658409E+03 0.12968125819492E+04
 0.90761873192876E+03 0.20626595978939E+04 0.16850414564852E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53129479700127E+03 0.12948375001393E+01
 0.12948375001393E+01 0.18827321158377E+01 0.11326103806653E-02 0.31370522249955E+03 0.33013796929827E+03
 0.33012556136520E+03 0.33012491191982E+03 0.23000000000000E+00 0.00000000000000E+00 0.17015556928666E+00
 0.00000000000000E+00 -.11991669517225E+02 0.15957687335905E+00 0.11451461276593E+01 0.50132577682481E+02
 0.18799716630931E+02 0.69860079921436E+01 0.26197529970538E+01 0.31378353058153E+03 0.40401806799047E+03
 0.29560331600641E+03 0.30193435375808E+03 0.29215000001014E+03 0.29215000017359E+03 0.29559581380927E+03
 0.30193286423045E+03 0.29215000001000E+03 0.29215000017363E+03 0.29560331600641E+03 0.30193435375808E+03
 0.29215000001014E+03 0.29215000017359E+03 0.29559581380927E+03 0.30193286423045E+03 0.29215000001000E+03
 0.29215000017363E+03 0.30046600976598E+03 0.29215000156015E+03 0.85993840578650E+02 0.85935352387278E+02
 0.17087643712979E+03 0.43193653973832E+03 0.26020572042288E+03 0.15766529622611E+03 0.16079836696454E+03
 0.15766529622611E+03 0.34403534823312E+03 0.15767460584154E+03 0.16068299236374E+03 0.15767460584154E+03
 0.34393167011366E+03 0.15766529622611E+03 0.16079836696454E+03 0.15766529622611E+03 0.34403534823312E+03
 0.15767460584154E+03 0.16068299236374E+03 0.15767460584154E+03 0.34393167011366E+03 0.22744521051232E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35658430321164E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17232021914708E+00 0.00000000000000E+00 0.00000000000000E+00 0.17232021914708E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18201103697698E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18201103697698E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256433153236E+00 0.18399049885545E+00 0.33013796929827E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    520.40239295
 0.99462155218377E+00 0.30560450796024E+03 0.45688448776047E+03 0.40672904498606E+03 0.39247239265796E+03
 0.22999999992363E+00 0.00000000000000E+00 0.14526190683182E+00 0.00000000000000E+00 -.18064292974513E+02
 0.95580368640529E-03 0.11738132097612E+01 0.80000000000000E+04 0.30000000000000E+04 0.68153944200607E+01
 0.25557729075228E+01 0.32244048150941E+03 0.29215000760907E+03 0.32190120833700E+03 0.35690371212474E+03
 0.29215000074048E+03 0.29215000194815E+03 0.31869700284291E+03 0.35686861337028E+03 0.29215000060263E+03
 0.29215000194211E+03 0.32190120833700E+03 0.35690371212474E+03 0.29215000074048E+03 0.29215000194815E+03
 0.31869700284291E+03 0.35686861337028E+03 0.29215000060263E+03 0.29215000194211E+03 0.40473917428991E+03
 0.31446545363185E+03 0.15913759963199E+04 0.15154092871775E+04 0.74655940362726E+03 0.12563564938582E+04
 0.50606429321285E+03 0.10633208441122E+04 0.12997811110244E+04 0.10040043054009E+04 0.20624762031069E+04
 0.95451194880841E+03 0.12990861888039E+04 0.90991262945228E+03 0.20621127603564E+04 0.10633208441122E+04
 0.12997811110244E+04 0.10040043054009E+04 0.20624762031069E+04 0.95451194880841E+03 0.12990861888039E+04
 0.90991262945228E+03 0.20621127603564E+04 0.16846085610218E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53166786522216E+03 0.12948375860135E+01
 0.12948375860135E+01 0.19188573164703E+01 0.91803785304290E-03 0.31440202338119E+03 0.33028973217275E+03
 0.33028000849404E+03 0.33027951744233E+03 0.23000000000000E+00 0.00000000000000E+00 0.16912939796627E+00
 0.00000000000000E+00 -.11981261950851E+02 0.19687468877959E+00 0.11643764306561E+01 0.40634984870789E+02
 0.15238119326546E+02 0.68706303128207E+01 0.25764863673078E+01 0.31446430011000E+03 0.40474623553180E+03
 0.29569421968485E+03 0.30206589703832E+03 0.29215000001293E+03 0.29215000021539E+03 0.29568689830662E+03
 0.30206434168224E+03 0.29215000001275E+03 0.29215000021544E+03 0.29569421968485E+03 0.30206589703832E+03
 0.29215000001293E+03 0.29215000021539E+03 0.29568689830662E+03 0.30206434168224E+03 0.29215000001275E+03
 0.29215000021544E+03 0.30057936726925E+03 0.29215000189301E+03 0.83573797094333E+02 0.83532816631996E+02
 0.17217129325320E+03 0.43361572767286E+03 0.26058357795339E+03 0.16018341120404E+03 0.16196915778108E+03
 0.16018341120404E+03 0.34533250266743E+03 0.16019644029976E+03 0.16185241512706E+03 0.16019644029976E+03
 0.34522797301609E+03 0.16018341120404E+03 0.16196915778108E+03 0.16018341120404E+03 0.34533250266743E+03
 0.16019644029976E+03 0.16185241512706E+03 0.16019644029976E+03 0.34522797301609E+03 0.22766887173422E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35678144569813E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17224979062366E+00 0.00000000000000E+00 0.00000000000000E+00 0.17224979062366E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18190737211443E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18190737211443E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256432114743E+00 0.18390596598121E+00 0.33028973217275E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    531.69151815
 0.99452435198271E+00 0.30587768182101E+03 0.45735524716727E+03 0.40713920466197E+03 0.39285938845697E+03
 0.22999999994631E+00 0.00000000000000E+00 0.14473301875599E+00 0.00000000000000E+00 -.18078549716151E+02
 0.95494994092112E-03 0.11799268167259E+01 0.80000000000000E+04 0.30000000000000E+04 0.67800815157323E+01
 0.25425305683996E+01 0.32289967514040E+03 0.29215000959371E+03 0.32234204791590E+03 0.35769993524684E+03
 0.29215000095993E+03 0.29215000252152E+03 0.31910846302556E+03 0.35766511196790E+03 0.29215000078246E+03
 0.29215000251379E+03 0.32234204791590E+03 0.35769993524684E+03 0.29215000095993E+03 0.29215000252152E+03
 0.31910846302556E+03 0.35766511196790E+03 0.29215000078246E+03 0.29215000251379E+03 0.40563717253241E+03
 0.31531602665851E+03 0.15964254333133E+04 0.15193934122459E+04 0.74446091100085E+03 0.12490976113587E+04
 0.50091439580281E+03 0.10665262803381E+04 0.13025524159976E+04 0.10064448604817E+04 0.20617931195941E+04
 0.95793302450677E+03 0.13018692275488E+04 0.91274221764337E+03 0.20614383117609E+04 0.10665262803381E+04
 0.13025524159976E+04 0.10064448604817E+04 0.20617931195941E+04 0.95793302450677E+03 0.13018692275488E+04
 0.91274221764337E+03 0.20614383117609E+04 0.16840877192302E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53213272101234E+03 0.12948376910551E+01
 0.12948376910551E+01 0.19640138172611E+01 0.70593980658584E-03 0.31527504110892E+03 0.33048269844064E+03
 0.33047554131352E+03 0.33047519625150E+03 0.23000000000000E+00 0.00000000000000E+00 0.16787975082467E+00
 0.00000000000000E+00 -.11968033226894E+02 0.25602523971304E+00 0.11877390082379E+01 0.31246919284077E+02
 0.11717594731529E+02 0.67354864532643E+01 0.25258074199741E+01 0.31531502368644E+03 0.40564348204478E+03
 0.29580865505518E+03 0.30222956939280E+03 0.29215000001737E+03 0.29215000027988E+03 0.29580154501768E+03
 0.30222793374117E+03 0.29215000001712E+03 0.29215000027995E+03 0.29580865505518E+03 0.30222956939280E+03
 0.29215000001737E+03 0.29215000027988E+03 0.29580154501768E+03 0.30222793374117E+03 0.29215000001712E+03
 0.29215000027995E+03 0.30072045148009E+03 0.29215000239367E+03 0.80469847202149E+02 0.80446317421720E+02
 0.17375124314063E+03 0.43571399294301E+03 0.26109399358668E+03 0.16331889019294E+03 0.16339557823091E+03
 0.16331889019294E+03 0.34694872579753E+03 0.16333568985104E+03 0.16327723527292E+03 0.16333568985104E+03
 0.34684322626224E+03 0.16331889019294E+03 0.16339557823091E+03 0.16331889019294E+03 0.34694872579753E+03
 0.16333568985104E+03 0.16327723527293E+03 0.16333568985104E+03 0.34684322626224E+03 0.22792451286730E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35702840829997E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17216046344574E+00 0.00000000000000E+00 0.00000000000000E+00 0.17216046344574E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18177366851486E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18177366851486E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256430698454E+00 0.18379859364587E+00 0.33048269844064E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    540.72281831
 0.99444384351563E+00 0.30609454456948E+03 0.45772995592550E+03 0.40746565549818E+03 0.39316729291813E+03
 0.22999999995131E+00 0.00000000000000E+00 0.14432790053324E+00 0.00000000000000E+00 -.18089707777052E+02
 0.95427327011954E-03 0.11845646562160E+01 0.80000000000000E+04 0.30000000000000E+04 0.67535359577208E+01
 0.25325759841453E+01 0.32326430542687E+03 0.29215001148720E+03 0.32269207684507E+03 0.35833028943717E+03
 0.29215000117454E+03 0.29215000308132E+03 0.31943538089777E+03 0.35829567872165E+03 0.29215000095857E+03
 0.29215000307197E+03 0.32269207684507E+03 0.35833028943717E+03 0.29215000117454E+03 0.29215000308132E+03
 0.31943538089777E+03 0.35829567872165E+03 0.29215000095857E+03 0.29215000307197E+03 0.40634562343287E+03
 0.31599601626126E+03 0.16004163441807E+04 0.15225362259927E+04 0.74278699329210E+03 0.12433994694173E+04
 0.49689854115876E+03 0.10690605627664E+04 0.13047228768601E+04 0.10083704499939E+04 0.20612509566905E+04
 0.96063867054165E+03 0.13040485457669E+04 0.91497611988555E+03 0.20609026119710E+04 0.10690605627664E+04
 0.13047228768601E+04 0.10083704499939E+04 0.20612509566905E+04 0.96063867054164E+03 0.13040485457669E+04
 0.91497611988555E+03 0.20609026119710E+04 0.16836831100249E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53250312577438E+03 0.12948377732661E+01
 0.12948377732661E+01 0.20001390178937E+01 0.57205145517569E-03 0.31597038714103E+03 0.33063965881391E+03
 0.33063406442844E+03 0.33063380480661E+03 0.23000000000000E+00 0.00000000000000E+00 0.16690641935180E+00
 0.00000000000000E+00 -.11957299951856E+02 0.31594780467664E+00 0.12058907590743E+01 0.25320638034462E+02
 0.94952392629232E+01 0.66341000955520E+01 0.24877875358320E+01 0.31599513445773E+03 0.40635136601525E+03
 0.29590083929717E+03 0.30235994411939E+03 0.29215000002185E+03 0.29215000034309E+03 0.29589388735397E+03
 0.30235824573093E+03 0.29215000002154E+03 0.29215000034317E+03 0.29590083929717E+03 0.30235994411939E+03
 0.29215000002185E+03 0.29215000034309E+03 0.29589388735397E+03 0.30235824573093E+03 0.29215000002154E+03
 0.29215000034317E+03 0.30083286804938E+03 0.29215000287244E+03 0.77929222514872E+02 0.77916313908214E+02
 0.17498798518099E+03 0.43739529067472E+03 0.26153236556783E+03 0.16581917416014E+03 0.16451057468444E+03
 0.16581917416014E+03 0.34824032559410E+03 0.16583839957547E+03 0.16439102808314E+03 0.16583839957547E+03
 0.34813411531611E+03 0.16581917416014E+03 0.16451057468444E+03 0.16581917416014E+03 0.34824032559410E+03
 0.16583839957547E+03 0.16439102808314E+03 0.16583839957547E+03 0.34813411531611E+03 0.22811327242820E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35722670493349E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17208804401263E+00 0.00000000000000E+00 0.00000000000000E+00 0.17208804401263E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18166442046135E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18166442046135E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256429425497E+00 0.18371134718745E+00 0.33063965881391E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    550.92489093
 0.99435163910281E+00 0.30633771086637E+03 0.45815082845485E+03 0.40783228768438E+03 0.39351301253314E+03
 0.22999999992583E+00 0.00000000000000E+00 0.14388853905985E+00 0.00000000000000E+00 -.18102880724782E+02
 0.95351565827290E-03 0.11895482734460E+01 0.80000000000000E+04 0.30000000000000E+04 0.67252419919243E+01
 0.25219657469716E+01 0.32367312207598E+03 0.29215001401113E+03 0.32308450881182E+03 0.35903552816041E+03
 0.29215000146739E+03 0.29215000384401E+03 0.31980208344602E+03 0.35900114893654E+03 0.29215000119923E+03
 0.29215000383249E+03 0.32308450881182E+03 0.35903552816041E+03 0.29215000146739E+03 0.29215000384401E+03
 0.31980208344602E+03 0.35900114893654E+03 0.29215000119923E+03 0.29215000383249E+03 0.40713711458857E+03
 0.31676398745033E+03 0.16048719059614E+04 0.15260391639584E+04 0.74087554117538E+03 0.12370272615555E+04
 0.49244734267420E+03 0.10718908502965E+04 0.13071212782126E+04 0.10105172375975E+04 0.20606337432157E+04
 0.96366155314992E+03 0.13064564174409E+04 0.91746844935783E+03 0.20602922565973E+04 0.10718908502965E+04
 0.13071212782126E+04 0.10105172375975E+04 0.20606337432157E+04 0.96366155314992E+03 0.13064564174409E+04
 0.91746844935783E+03 0.20602922565973E+04 0.16832225249563E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53291939050927E+03 0.12948378703226E+01
 0.12948378703226E+01 0.20409473083689E+01 0.45103134140900E-03 0.31675414266744E+03 0.33081967158475E+03
 0.33081544225517E+03 0.33081525450995E+03 0.23000000000000E+00 0.00000000000000E+00 0.16583494120978E+00
 0.00000000000000E+00 -.11945867722097E+02 0.40072247115159E+00 0.12258243663516E+01 0.19963941570359E+02
 0.74864780888845E+01 0.65262204110121E+01 0.24473326541295E+01 0.31676324137200E+03 0.40714225501007E+03
 0.29600539688220E+03 0.30250652022845E+03 0.29215000002816E+03 0.29215000042952E+03 0.29599861231107E+03
 0.30250475254494E+03 0.29215000002777E+03 0.29215000042963E+03 0.29600539688220E+03 0.30250652022845E+03
 0.29215000002816E+03 0.29215000042952E+03 0.29599861231107E+03 0.30250475254494E+03 0.29215000002777E+03
 0.29215000042963E+03 0.30095928602213E+03 0.29215000351198E+03 0.74999286035450E+02 0.74995593328788E+02
 0.17636017994341E+03 0.43930212208249E+03 0.26206014123936E+03 0.16863774734121E+03 0.16574628624202E+03
 0.16863774734121E+03 0.34970207157974E+03 0.16865918866667E+03 0.16562544878175E+03 0.16865918866667E+03
 0.34959511527362E+03 0.16863774734121E+03 0.16574628624202E+03 0.16863774734121E+03 0.34970207157974E+03
 0.16865918866666E+03 0.16562544878175E+03 0.16865918866666E+03 0.34959511527363E+03 0.22831290991179E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35745165148287E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17201266149702E+00 0.00000000000000E+00 0.00000000000000E+00 0.17201266149702E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18153903834435E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18153903834435E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256425404166E+00 0.18361136012541E+00 0.33081967158475E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    562.29773601
 0.99424663551102E+00 0.30660658696711E+03 0.45861707580090E+03 0.40823843677244E+03 0.39389589847591E+03
 0.22999999993314E+00 0.00000000000000E+00 0.14342049636115E+00 0.00000000000000E+00 -.18115701739041E+02
 0.95267936006577E-03 0.11948004956764E+01 0.80000000000000E+04 0.30000000000000E+04 0.66956785077925E+01
 0.25108794404222E+01 0.32412544372676E+03 0.29215001737799E+03 0.32351866380811E+03 0.35981317715488E+03
 0.29215000186820E+03 0.29215000488598E+03 0.32020806793903E+03 0.35977904669961E+03 0.29215000152911E+03
 0.29215000487152E+03 0.32351866380811E+03 0.35981317715488E+03 0.29215000186820E+03 0.29215000488598E+03
 0.32020806793903E+03 0.35977904669961E+03 0.29215000152911E+03 0.29215000487152E+03 0.40800623875020E+03
 0.31761845404056E+03 0.16097748388568E+04 0.15298847459307E+04 0.73875756358045E+03 0.12300688176754E+04
 0.48761746627707E+03 0.10750066530870E+04 0.13097341800646E+04 0.10128746701632E+04 0.20599460987033E+04
 0.96699052884287E+03 0.13090792713089E+04 0.92020726605946E+03 0.20596117530190E+04 0.10750066530871E+04
 0.13097341800646E+04 0.10128746701632E+04 0.20599460987033E+04 0.96699052884287E+03 0.13090792713089E+04
 0.92020726605946E+03 0.20596117530190E+04 0.16827226759157E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53338090562793E+03 0.12948379647860E+01
 0.12948379647860E+01 0.20864386886866E+01 0.34600340035595E-03 0.31763604379393E+03 0.33102361274321E+03
 0.33102052464696E+03 0.33102039452267E+03 0.23000000000000E+00 0.00000000000000E+00 0.16467521197510E+00
 0.00000000000000E+00 -.11931425101365E+02 0.52236014097248E+00 0.12473390090958E+01 0.15315104221211E+02
 0.57431640829541E+01 0.64136533385573E+01 0.24051200019590E+01 0.31761785935298E+03 0.40801074707096E+03
 0.29612289951353E+03 0.30266932172910E+03 0.29215000003710E+03 0.29215000054808E+03 0.29611628904470E+03
 0.30266747850490E+03 0.29215000003658E+03 0.29215000054821E+03 0.29612289951353E+03 0.30266932172910E+03
 0.29215000003710E+03 0.29215000054808E+03 0.29611628904470E+03 0.30266747850490E+03 0.29215000003658E+03
 0.29215000054821E+03 0.30109975097394E+03 0.29215000436709E+03 0.71671508777084E+02 0.71675892310011E+02
 0.17786161113513E+03 0.44143653155841E+03 0.26268561236760E+03 0.17177276729134E+03 0.16709652882466E+03
 0.17177276729134E+03 0.35133427387339E+03 0.17179613352502E+03 0.16697432603339E+03 0.17179613352502E+03
 0.35122654668321E+03 0.17177276729134E+03 0.16709652882466E+03 0.17177276729134E+03 0.35133427387339E+03
 0.17179613352502E+03 0.16697432603340E+03 0.17179613352502E+03 0.35122654668321E+03 0.22852191447795E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35770381840241E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17191403120798E+00 0.00000000000000E+00 0.00000000000000E+00 0.17191403120798E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18139777463040E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18139777463040E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256425148302E+00 0.18349826199417E+00 0.33102361274321E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    570.82736982
 0.99416610033174E+00 0.30680687628994E+03 0.45896493530061E+03 0.40854147391371E+03 0.39418152176532E+03
 0.22999999993660E+00 0.00000000000000E+00 0.14308375361451E+00 0.00000000000000E+00 -.18125391930939E+02
 0.95205734192229E-03 0.11985402761452E+01 0.80000000000000E+04 0.30000000000000E+04 0.66747861204380E+01
 0.25030447951642E+01 0.32446240834327E+03 0.29215002033592E+03 0.32384206881345E+03 0.36039071928367E+03
 0.29215000222837E+03 0.29215000582076E+03 0.32051068538188E+03 0.36035676926397E+03 0.29215000182594E+03
 0.29215000580371E+03 0.32384206881345E+03 0.36039071928367E+03 0.29215000222837E+03 0.29215000582076E+03
 0.32051068538188E+03 0.36035676926397E+03 0.29215000182594E+03 0.29215000580371E+03 0.40864961281660E+03
 0.31825797935644E+03 0.16134124708059E+04 0.15327330315452E+04 0.73717691782049E+03 0.12249433687775E+04
 0.48408056636786E+03 0.10773191657546E+04 0.13116571602386E+04 0.10146212600425E+04 0.20594355804481E+04
 0.96946186311946E+03 0.13110093216829E+04 0.92223727697358E+03 0.20591062633717E+04 0.10773191657547E+04
 0.13116571602386E+04 0.10146212600425E+04 0.20594355804481E+04 0.96946186311945E+03 0.13110093216829E+04
 0.92223727697358E+03 0.20591062633717E+04 0.16823594422857E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53372552597168E+03 0.12948380361820E+01
 0.12948380361820E+01 0.21205572239250E+01 0.28358182335353E-03 0.31828734436433E+03 0.33117875146664E+03
 0.33117631428749E+03 0.33117621559526E+03 0.23000000000000E+00 0.00000000000000E+00 0.16382913812252E+00
 0.00000000000000E+00 -.11920774898654E+02 0.63734119574593E+00 0.12629926128535E+01 0.12552146406662E+02
 0.47070549024983E+01 0.63341621467804E+01 0.23753108050427E+01 0.31825750054309E+03 0.40865366657809E+03
 0.29621165911783E+03 0.30279104423970E+03 0.29215000004537E+03 0.29215000065482E+03 0.29620517133935E+03
 0.30278914547267E+03 0.29215000004474E+03 0.29215000065499E+03 0.29621165911783E+03 0.30279104423970E+03
 0.29215000004537E+03 0.29215000065482E+03 0.29620517133936E+03 0.30278914547267E+03 0.29215000004474E+03
 0.29215000065499E+03 0.30120481324880E+03 0.29215000511982E+03 0.69136745621606E+02 0.69144882893835E+02
 0.17897027201471E+03 0.44304450721743E+03 0.26317938384265E+03 0.17411882807307E+03 0.16809242499036E+03
 0.17411882807307E+03 0.35256143063625E+03 0.17414332542842E+03 0.16796924558656E+03 0.17414332542842E+03
 0.35245316428293E+03 0.17411882807307E+03 0.16809242499036E+03 0.17411882807307E+03 0.35256143063625E+03
 0.17414332542842E+03 0.16796924558656E+03 0.17414332542842E+03 0.35245316428293E+03 0.22867077325141E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35789402968503E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17184179165199E+00 0.00000000000000E+00 0.00000000000000E+00 0.17184179165199E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18129114852851E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18129114852851E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256423984351E+00 0.18341231023701E+00 0.33117875146664E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    582.20021489
 0.99405766497137E+00 0.30707215593341E+03 0.45942613425011E+03 0.40894325427191E+03 0.39456016379870E+03
 0.22999999994602E+00 0.00000000000000E+00 0.14265284969148E+00 0.00000000000000E+00 -.18138184411527E+02
 0.95123473944316E-03 0.12032753976240E+01 0.80000000000000E+04 0.30000000000000E+04 0.66485195457306E+01
 0.24931948296490E+01 0.32490872023512E+03 0.29215002493970E+03 0.32427038948667E+03 0.36115343591000E+03
 0.29215000280161E+03 0.29215000730609E+03 0.32091172455847E+03 0.36111971858609E+03 0.29215000229900E+03
 0.29215000728495E+03 0.32427038948667E+03 0.36115343591000E+03 0.29215000280161E+03 0.29215000730609E+03
 0.32091172455847E+03 0.36111971858609E+03 0.29215000229900E+03 0.29215000728495E+03 0.40949685971780E+03
 0.31910868929230E+03 0.16182098776722E+04 0.15364828799877E+04 0.73507269868097E+03 0.12182160170904E+04
 0.47946795491598E+03 0.10803702164305E+04 0.13141713225825E+04 0.10169215533289E+04 0.20587569142632E+04
 0.97272336674376E+03 0.13135324206506E+04 0.92491214543875E+03 0.20584338970779E+04 0.10803702164305E+04
 0.13141713225825E+04 0.10169215533289E+04 0.20587569142632E+04 0.97272336674375E+03 0.13135324206506E+04
 0.92491214543875E+03 0.20584338970779E+04 0.16818841497409E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53418266074861E+03 0.12948381304353E+01
 0.12948381304353E+01 0.21660486042428E+01 0.21747632188829E-03 0.31915041741328E+03 0.33138838263446E+03
 0.33138660832269E+03 0.33138654029615E+03 0.23000000000000E+00 0.00000000000000E+00 0.16273219584681E+00
 0.00000000000000E+00 -.11906605791878E+02 0.83107148369777E+00 0.12832313041615E+01 0.96261274233653E+01
 0.36097977837620E+01 0.62342618778517E+01 0.23378482041944E+01 0.31910836466475E+03 0.40950033992832E+03
 0.29633080810389E+03 0.30295286257135E+03 0.29215000005894E+03 0.29215000082505E+03 0.29632447425089E+03
 0.30295089113327E+03 0.29215000005811E+03 0.29215000082526E+03 0.29633080810389E+03 0.30295286257135E+03
 0.29215000005894E+03 0.29215000082505E+03 0.29632447425089E+03 0.30295089113327E+03 0.29215000005811E+03
 0.29215000082526E+03 0.30134453642959E+03 0.29215000629366E+03 0.65709623874401E+02 0.65722105529986E+02
 0.18042813304565E+03 0.44519945354859E+03 0.26386917983772E+03 0.17724118816321E+03 0.16940061311321E+03
 0.17724118816321E+03 0.35420304256067E+03 0.17726685169433E+03 0.16927618463367E+03 0.17726685169433E+03
 0.35409409999251E+03 0.17724118816321E+03 0.16940061311321E+03 0.17724118816321E+03 0.35420304256067E+03
 0.17726685169433E+03 0.16927618463367E+03 0.17726685169433E+03 0.35409409999251E+03 0.22886131917813E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35814912378179E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17174586095327E+00 0.00000000000000E+00 0.00000000000000E+00 0.17174586095327E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18114856320535E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18114856320535E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256421785989E+00 0.18329628873804E+00 0.33138838263446E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    593.51484803
 0.99395308673533E+00 0.30733381955262E+03 0.45988111570085E+03 0.40933949707430E+03 0.39493355718282E+03
 0.22999999994304E+00 0.00000000000000E+00 0.14224367761131E+00 0.00000000000000E+00 -.18156045311716E+02
 0.95042469184321E-03 0.12077180565194E+01 0.80000000000000E+04 0.30000000000000E+04 0.66240625921050E+01
 0.24840234720394E+01 0.32534838367791E+03 0.29215003043374E+03 0.32469235191382E+03 0.36190425233754E+03
 0.29215000350406E+03 0.29215000912256E+03 0.32130690237221E+03 0.36187075559318E+03 0.29215000287962E+03
 0.29215000909652E+03 0.32469235191382E+03 0.36190425233754E+03 0.29215000350406E+03 0.29215000912256E+03
 0.32130690237221E+03 0.36187075559318E+03 0.29215000287962E+03 0.29215000909652E+03 0.41033387409172E+03
 0.31995547732246E+03 0.16229196464450E+04 0.15401605584130E+04 0.73288698240480E+03 0.12114672996918E+04
 0.47491588237498E+03 0.10833669077139E+04 0.13166034749543E+04 0.10191787901966E+04 0.20580607263418E+04
 0.97592877329408E+03 0.13159729045547E+04 0.92753950607191E+03 0.20577435317144E+04 0.10833669077139E+04
 0.13166034749543E+04 0.10191787901966E+04 0.20580607263418E+04 0.97592877329408E+03 0.13159729045547E+04
 0.92753950607191E+03 0.20577435317144E+04 0.16813694237832E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53463333053052E+03 0.12948382620320E+01
 0.12948382620320E+01 0.22113071368029E+01 0.16698698082207E-03 0.32000452909184E+03 0.33159999512207E+03
 0.33159870426082E+03 0.33159865749145E+03 0.23000000000000E+00 0.00000000000000E+00 0.16167559062779E+00
 0.00000000000000E+00 -.11898051839295E+02 0.10823500111653E+01 0.13026617138964E+01 0.73913243566997E+01
 0.27717466337624E+01 0.61412720698386E+01 0.23029770261895E+01 0.31995529681362E+03 0.41033683692294E+03
 0.29644913800236E+03 0.30311270562872E+03 0.29215000007616E+03 0.29215000103413E+03 0.29644294648229E+03
 0.30311066360329E+03 0.29215000007509E+03 0.29215000103440E+03 0.29644913800236E+03 0.30311270562872E+03
 0.29215000007616E+03 0.29215000103413E+03 0.29644294648229E+03 0.30311066360329E+03 0.29215000007509E+03
 0.29215000103440E+03 0.30148258342376E+03 0.29215000769763E+03 0.62231277225797E+02 0.62246463972793E+02
 0.18186179896455E+03 0.44736768661904E+03 0.26459657865967E+03 0.18034875371618E+03 0.17068627179338E+03
 0.18034875371618E+03 0.35585328308418E+03 0.18037526268094E+03 0.17056064179252E+03 0.18037526268094E+03
 0.35574369616059E+03 0.18034875371618E+03 0.17068627179338E+03 0.18034875371618E+03 0.35585328308418E+03
 0.18037526268094E+03 0.17056064179252E+03 0.18037526268094E+03 0.35574369616059E+03 0.22904437106027E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35840490760360E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17169765231570E+00 0.00000000000000E+00 0.00000000000000E+00 0.17169765231570E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18101331512001E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18101331512001E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256402973223E+00 0.18317913290439E+00 0.33159999512207E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    600.67690893
 0.99403759132331E+00 0.30749645160509E+03 0.46016837904048E+03 0.40958116737029E+03 0.39515945073159E+03
 0.22999999994958E+00 0.00000000000000E+00 0.14199561503648E+00 0.00000000000000E+00 -.18236315102398E+02
 0.94992126827804E-03 0.12103762682144E+01 0.80000000000000E+04 0.30000000000000E+04 0.66095149170447E+01
 0.24785680938917E+01 0.32562711554400E+03 0.29215003430062E+03 0.32495977609389E+03 0.36237457017361E+03
 0.29215000400603E+03 0.29215001041907E+03 0.32155791981198E+03 0.36234121342167E+03 0.29215000329491E+03
 0.29215001038957E+03 0.32495977609389E+03 0.36237457017361E+03 0.29215000400603E+03 0.29215001041907E+03
 0.32155791981198E+03 0.36234121342167E+03 0.29215000329491E+03 0.29215001038957E+03 0.41084408076715E+03
 0.32060486603013E+03 0.16257820441589E+04 0.15423470970813E+04 0.73170089678733E+03 0.12076063848505E+04
 0.47224698357918E+03 0.10852134515686E+04 0.13181222746549E+04 0.10205408369491E+04 0.20576529609819E+04
 0.97790636459720E+03 0.13174966904995E+04 0.92913336386663E+03 0.20573391721184E+04 0.10852134515686E+04
 0.13181222746549E+04 0.10205408369491E+04 0.20576529609819E+04 0.97790636459720E+03 0.13174966904995E+04
 0.92913336386662E+03 0.20573391721184E+04 0.16810755716687E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53489688073306E+03 0.12948388534495E+01
 0.12948388534495E+01 0.22399553803930E+01 0.00000000000000E+00 0.33170442355873E+03 0.33170442355873E+03
 0.33170442355873E+03 0.33170442355873E+03 0.00000000000000E+00 0.00000000000000E+00 0.16102920032292E+00
 0.00000000000000E+00 -.11974273050311E+02 0.10000000000000E-02 0.13147475967176E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60848181202026E+01 0.22818067950760E+01 0.32060441357836E+03 0.41084668526012E+03
 0.30321408893024E+03 0.30321408893024E+03 0.29215000119932E+03 0.29215000118375E+03 0.30321208124993E+03
 0.30321208124993E+03 0.29215000119963E+03 0.29215000118405E+03 0.30321408893024E+03 0.30321408893024E+03
 0.29215000119932E+03 0.29215000118375E+03 0.30321208124993E+03 0.30321208124993E+03 0.29215000119963E+03
 0.29215000118405E+03 0.30156940076989E+03 0.29215000868705E+03 0.59637967688834E+02 0.72768587514863E+02
 0.18216408627569E+03 0.44788009127518E+03 0.26480518456811E+03 0.14059447397530E+03 0.17125871309006E+03
 0.14059447397530E+03 0.35646589899468E+03 0.14060249495982E+03 0.17115344167552E+03 0.14060249495982E+03
 0.35637640175510E+03 0.14059447397530E+03 0.17125871309006E+03 0.14059447397530E+03 0.35646589899468E+03
 0.14060249495983E+03 0.17115344167552E+03 0.14060249495983E+03 0.35637640175510E+03 0.23188471293927E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35857140776032E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17227774611795E+00 0.00000000000000E+00 0.00000000000000E+00 0.17227774611795E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18138449450486E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18138449450486E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256161273462E+00 0.18311876107541E+00 0.33170442355873E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    611.10674995
 0.99405616834812E+00 0.30773295197847E+03 0.46057943290845E+03 0.40993343718220E+03 0.39549049802520E+03
 0.22999999996522E+00 0.00000000000000E+00 0.14165075258725E+00 0.00000000000000E+00 -.18219835699871E+02
 0.94919138472745E-03 0.12140369902166E+01 0.80000000000000E+04 0.30000000000000E+04 0.65895850492765E+01
 0.24710943934787E+01 0.32602864746573E+03 0.29215004072262E+03 0.32534516617172E+03 0.36305495165741E+03
 0.29215000485559E+03 0.29215001261007E+03 0.32191944045021E+03 0.36302178853477E+03 0.29215000399856E+03
 0.29215001257477E+03 0.32534516617172E+03 0.36305495165741E+03 0.29215000485559E+03 0.29215001261007E+03
 0.32191944045021E+03 0.36302178853477E+03 0.29215000399856E+03 0.29215001257477E+03 0.41159254303221E+03
 0.32143553086595E+03 0.16299137870038E+04 0.15455175165577E+04 0.72969621734340E+03 0.12015773851820E+04
 0.46823268675193E+03 0.10878636016493E+04 0.13201982807404E+04 0.10225017791497E+04 0.20569000381552E+04
 0.98074824180538E+03 0.13195795319679E+04 0.93143181644075E+03 0.20565908806997E+04 0.10878636016493E+04
 0.13201982807404E+04 0.10225017791497E+04 0.20569000381552E+04 0.98074824180538E+03 0.13195795319679E+04
 0.93143181644075E+03 0.20565908806997E+04 0.16805009004782E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53528846666881E+03 0.12948387320313E+01
 0.12948387320313E+01 0.22816747444517E+01 0.00000000000000E+00 0.33186983236392E+03 0.33186983236392E+03
 0.33186983236392E+03 0.33186983236392E+03 0.00000000000000E+00 0.00000000000000E+00 0.16010144799062E+00
 0.00000000000000E+00 -.11931299224496E+02 0.10000000000000E-02 0.13318448866407E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60067054956964E+01 0.22525145608862E+01 0.32143497281634E+03 0.41159468528409E+03
 0.30335900296043E+03 0.30335900296043E+03 0.29215000145629E+03 0.29215000143738E+03 0.30335697257487E+03
 0.30335697257487E+03 0.29215000145666E+03 0.29215000143774E+03 0.30335900296043E+03 0.30335900296043E+03
 0.29215000145629E+03 0.29215000143738E+03 0.30335697257487E+03 0.30335697257487E+03 0.29215000145666E+03
 0.29215000143774E+03 0.30169433026208E+03 0.29215001033282E+03 0.55756005100315E+02 0.67907660721118E+02
 0.18327933881409E+03 0.44938295215331E+03 0.26518721664515E+03 0.14324261496100E+03 0.17222033790535E+03
 0.14324261496100E+03 0.35754891660689E+03 0.14325071761152E+03 0.17211280244911E+03 0.14325071761152E+03
 0.35745733265050E+03 0.14324261496100E+03 0.17222033790535E+03 0.14324261496100E+03 0.35754891660689E+03
 0.14325071761152E+03 0.17211280244911E+03 0.14325071761152E+03 0.35745733265050E+03 0.23140343199716E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35877106427664E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17195289959454E+00 0.00000000000000E+00 0.00000000000000E+00 0.17195289959454E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18113791572309E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18113791572309E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256251493435E+00 0.18302852657236E+00 0.33186983236392E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    620.23286083
 0.99398081943691E+00 0.30793999048917E+03 0.46093744423519E+03 0.41024526608645E+03 0.39578455199092E+03
 0.22999999993338E+00 0.00000000000000E+00 0.14136194978396E+00 0.00000000000000E+00 -.18207399296220E+02
 0.94855332757705E-03 0.12170612122334E+01 0.80000000000000E+04 0.30000000000000E+04 0.65732108784566E+01
 0.24649540794212E+01 0.32637779702178E+03 0.29215004715474E+03 0.32568023561383E+03 0.36364471216236E+03
 0.29215000572319E+03 0.29215001484411E+03 0.32223393322749E+03 0.36361171285497E+03 0.29215000471801E+03
 0.29215001480297E+03 0.32568023561383E+03 0.36364471216236E+03 0.29215000572319E+03 0.29215001484411E+03
 0.32223393322749E+03 0.36361171285497E+03 0.29215000471801E+03 0.29215001480297E+03 0.41223973884387E+03
 0.32213733808662E+03 0.16335458688256E+04 0.15483209406331E+04 0.72795985848415E+03 0.11964019617486E+04
 0.46480230397205E+03 0.10901802093890E+04 0.13219938551313E+04 0.10242248080576E+04 0.20562556297500E+04
 0.98323072796295E+03 0.13213808312370E+04 0.93344644500171E+03 0.20559503270048E+04 0.10901802093890E+04
 0.13219938551313E+04 0.10242248080576E+04 0.20562556297500E+04 0.98323072796294E+03 0.13213808312370E+04
 0.93344644500171E+03 0.20559503270048E+04 0.16800555544668E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53564226207068E+03 0.12948386404015E+01
 0.12948386404015E+01 0.23181791880030E+01 0.00000000000000E+00 0.33202609343616E+03 0.33202609343616E+03
 0.33202609343616E+03 0.33202609343616E+03 0.00000000000000E+00 0.00000000000000E+00 0.15931374904660E+00
 0.00000000000000E+00 -.11895309789078E+02 0.10000000000000E-02 0.13462828759442E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59422875704257E+01 0.22283578389097E+01 0.32213715505481E+03 0.41224152026714E+03
 0.30348534226026E+03 0.30348534226026E+03 0.29215000171917E+03 0.29215000169684E+03 0.30348328069315E+03
 0.30348328069315E+03 0.29215000171960E+03 0.29215000169727E+03 0.30348534226026E+03 0.30348534226026E+03
 0.29215000171917E+03 0.29215000169684E+03 0.30348328069316E+03 0.30348328069316E+03 0.29215000171960E+03
 0.29215000169727E+03 0.30180343143841E+03 0.29215001198380E+03 0.52543562613356E+02 0.63903355155222E+02
 0.18429808552136E+03 0.45085983651393E+03 0.26564026056497E+03 0.14553095900825E+03 0.17310969201345E+03
 0.14553095900825E+03 0.35863495490528E+03 0.14553919863474E+03 0.17300047598663E+03 0.14553919863474E+03
 0.35854193569563E+03 0.14553095900825E+03 0.17310969201345E+03 0.14553095900825E+03 0.35863495490528E+03
 0.14553919863473E+03 0.17300047598662E+03 0.14553919863473E+03 0.35854193569562E+03 0.23112249016702E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35895549854116E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17168321386063E+00 0.00000000000000E+00 0.00000000000000E+00 0.17168321386063E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18092025955229E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18092025955229E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256323432111E+00 0.18294321572848E+00 0.33202609343616E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    630.66270185
 0.99384405122010E+00 0.30817617038402E+03 0.46134632638538E+03 0.41060391026324E+03 0.39612310265171E+03
 0.22999999992333E+00 0.00000000000000E+00 0.14104408631130E+00 0.00000000000000E+00 -.18203076255828E+02
 0.94782641621240E-03 0.12203449003757E+01 0.80000000000000E+04 0.30000000000000E+04 0.65555237683518E+01
 0.24583214131319E+01 0.32677451412109E+03 0.29215005554357E+03 0.32606088938448E+03 0.36431288296148E+03
 0.29215000687662E+03 0.29215001780945E+03 0.32259140461760E+03 0.36428006490736E+03 0.29215000567559E+03
 0.29215001776066E+03 0.32606088938448E+03 0.36431288296148E+03 0.29215000687662E+03 0.29215001780945E+03
 0.32259140461760E+03 0.36428006490736E+03 0.29215000567559E+03 0.29215001776066E+03 0.41297189877852E+03
 0.32292476884988E+03 0.16377116491170E+04 0.15515494304873E+04 0.72601131225537E+03 0.11906162536092E+04
 0.46097488479258E+03 0.10928301978005E+04 0.13240508804311E+04 0.10262040102408E+04 0.20555771049728E+04
 0.98606833000897E+03 0.13234441221386E+04 0.93575509846747E+03 0.20552759944055E+04 0.10928301978005E+04
 0.13240508804311E+04 0.10262040102408E+04 0.20555771049728E+04 0.98606833000897E+03 0.13234441221386E+04
 0.93575509846746E+03 0.20552759944055E+04 0.16796190668169E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53605333603178E+03 0.12948386085499E+01
 0.12948386085499E+01 0.23598985520618E+01 0.00000000000000E+00 0.33221490671480E+03 0.33221490671480E+03
 0.33221490671480E+03 0.33221490671480E+03 0.00000000000000E+00 0.00000000000000E+00 0.15844018238099E+00
 0.00000000000000E+00 -.11864918726807E+02 0.10000000000000E-02 0.13622142992999E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58727910902943E+01 0.22022966588604E+01 0.32292493149836E+03 0.41297328578500E+03
 0.30362956803771E+03 0.30362956803771E+03 0.29215000206924E+03 0.29215000204236E+03 0.30362746527488E+03
 0.30362746527488E+03 0.29215000206976E+03 0.29215000204288E+03 0.30362956803771E+03 0.30362956803771E+03
 0.29215000206924E+03 0.29215000204236E+03 0.30362746527488E+03 0.30362746527488E+03 0.29215000206976E+03
 0.29215000204288E+03 0.30192808532271E+03 0.29215001414035E+03 0.48978841046194E+02 0.59482513997370E+02
 0.18549968938389E+03 0.45269280752451E+03 0.26626561969369E+03 0.14814188931476E+03 0.17416610815276E+03
 0.14814188931476E+03 0.35999808667550E+03 0.14815031789709E+03 0.17405515065265E+03 0.14815031789709E+03
 0.35990365144521E+03 0.14814188931476E+03 0.17416610815276E+03 0.14814188931476E+03 0.35999808667550E+03
 0.14815031789709E+03 0.17405515065265E+03 0.14815031789709E+03 0.35990365144521E+03 0.23091017094027E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35917760123318E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17146026988799E+00 0.00000000000000E+00 0.00000000000000E+00 0.17146026988799E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18071088259001E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18071088259001E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256372533471E+00 0.18283981565667E+00 0.33221490671480E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    640.78137967
 0.99369819407940E+00 0.30840417282662E+03 0.46174194545229E+03 0.41095145620486E+03 0.39645116640740E+03
 0.22999999991661E+00 0.00000000000000E+00 0.14074679995594E+00 0.00000000000000E+00 -.18210107027304E+02
 0.94712562475465E-03 0.12233755846497E+01 0.80000000000000E+04 0.30000000000000E+04 0.65392836839150E+01
 0.24522313814681E+01 0.32715669589848E+03 0.29215006490969E+03 0.32642756293380E+03 0.36495556749790E+03
 0.29215000819110E+03 0.29215002118304E+03 0.32293586706686E+03 0.36492291880516E+03 0.29215000676824E+03
 0.29215002112565E+03 0.32642756293379E+03 0.36495556749790E+03 0.29215000819110E+03 0.29215002118304E+03
 0.32293586706686E+03 0.36492291880516E+03 0.29215000676824E+03 0.29215002112565E+03 0.41367731122612E+03
 0.32367996701856E+03 0.16417433235654E+04 0.15546795598934E+04 0.72410709645070E+03 0.11850421368595E+04
 0.45731450492658E+03 0.10953933613648E+04 0.13260367713104E+04 0.10281223890804E+04 0.20549551593516E+04
 0.98881229489789E+03 0.13254358044439E+04 0.93799037115058E+03 0.20546578974495E+04 0.10953933613648E+04
 0.13260367713104E+04 0.10281223890804E+04 0.20549551593516E+04 0.98881229489789E+03 0.13254358044439E+04
 0.93799037115058E+03 0.20546578974496E+04 0.16792210953007E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53645293403185E+03 0.12948386603517E+01
 0.12948386603517E+01 0.24003732633482E+01 0.00000000000000E+00 0.33240660426410E+03 0.33240660426410E+03
 0.33240660426410E+03 0.33240660426410E+03 0.00000000000000E+00 0.00000000000000E+00 0.15761893895423E+00
 0.00000000000000E+00 -.11847918619855E+02 0.10000000000000E-02 0.13771152313288E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58092451655486E+01 0.21784669370807E+01 0.32368033847237E+03 0.41367834618157E+03
 0.30376924869884E+03 0.30376924869884E+03 0.29215000248235E+03 0.29215000243684E+03 0.30376710278031E+03
 0.30376710278031E+03 0.29215000248297E+03 0.29215000243746E+03 0.30376924869884E+03 0.30376924869884E+03
 0.29215000248235E+03 0.29215000243684E+03 0.30376710278031E+03 0.30376710278031E+03 0.29215000248297E+03
 0.29215000243746E+03 0.30204886650486E+03 0.29215001655206E+03 0.45578451394022E+02 0.55289113818476E+02
 0.18669677883118E+03 0.45459481560993E+03 0.26696455288459E+03 0.15068357106907E+03 0.17522376277283E+03
 0.15068357106907E+03 0.36142362341290E+03 0.15069220163471E+03 0.17511122135580E+03 0.15069220163471E+03
 0.36132794642452E+03 0.15068357106907E+03 0.17522376277283E+03 0.15068357106907E+03 0.36142362341290E+03
 0.15069220163471E+03 0.17511122135580E+03 0.15069220163471E+03 0.36132794642452E+03 0.23078774884808E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35940328229591E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17134295241571E+00 0.00000000000000E+00 0.00000000000000E+00 0.17134295241571E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18055546024375E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18055546024375E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256382370363E+00 0.18273450751157E+00 0.33240660426410E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    651.80006429
 0.99353152813682E+00 0.30865130534947E+03 0.46217159417462E+03 0.41132917845580E+03 0.39680767000196E+03
 0.22999999991716E+00 0.00000000000000E+00 0.14043469313366E+00 0.00000000000000E+00 -.18209563788318E+02
 0.94636728038763E-03 0.12265138470287E+01 0.80000000000000E+04 0.30000000000000E+04 0.65225517179285E+01
 0.24459568942232E+01 0.32757060268668E+03 0.29215007660260E+03 0.32682460665816E+03 0.36564901290680E+03
 0.29215000986552E+03 0.29215002547295E+03 0.32330913211736E+03 0.36561654298764E+03 0.29215000816179E+03
 0.29215002540477E+03 0.32682460665816E+03 0.36564901290680E+03 0.29215000986552E+03 0.29215002547295E+03
 0.32330913211736E+03 0.36561654298764E+03 0.29215000816179E+03 0.29215002540477E+03 0.41443566846841E+03
 0.32449202434675E+03 0.16461157190761E+04 0.15580751178106E+04 0.72208406451344E+03 0.11791303750829E+04
 0.45343589024694E+03 0.10981730317568E+04 0.13281921220513E+04 0.10302039213086E+04 0.20543246447907E+04
 0.99178701188892E+03 0.13275971935514E+04 0.94041317388417E+03 0.20540313678008E+04 0.10981730317568E+04
 0.13281921220513E+04 0.10302039213086E+04 0.20543246447907E+04 0.99178701188892E+03 0.13275971935514E+04
 0.94041317388416E+03 0.20540313678008E+04 0.16788409885265E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53688804722161E+03 0.12948386563492E+01
 0.12948386563492E+01 0.24444480018221E+01 0.00000000000000E+00 0.33262243778333E+03 0.33262243778333E+03
 0.33262243778333E+03 0.33262243778333E+03 0.00000000000000E+00 0.00000000000000E+00 0.15675316285664E+00
 0.00000000000000E+00 -.11820071687751E+02 0.10000000000000E-02 0.13927473306700E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57440426011454E+01 0.21540159754295E+01 0.32449255311994E+03 0.41443633176241E+03
 0.30392162292990E+03 0.30392162292990E+03 0.29215000299514E+03 0.29215000294023E+03 0.30391942765587E+03
 0.30391942765587E+03 0.29215000299589E+03 0.29215000294097E+03 0.30392162292990E+03 0.30392162292990E+03
 0.29215000299514E+03 0.29215000294023E+03 0.30391942765587E+03 0.30391942765587E+03 0.29215000299589E+03
 0.29215000294097E+03 0.30218070123580E+03 0.29215001956765E+03 0.41932083829981E+02 0.50817899872092E+02
 0.18801967051079E+03 0.45674987091484E+03 0.26779010205150E+03 0.15344881714560E+03 0.17639604136392E+03
 0.15344881714560E+03 0.36304395734640E+03 0.15345768207776E+03 0.17628189065931E+03 0.15345768207776E+03
 0.36294706281805E+03 0.15344881714560E+03 0.17639604136392E+03 0.15344881714560E+03 0.36304395734640E+03
 0.15345768207776E+03 0.17628189065931E+03 0.15345768207776E+03 0.36294706281805E+03 0.23071782484938E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35965229973923E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17114365396554E+00 0.00000000000000E+00 0.00000000000000E+00 0.17114365396554E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18034072866802E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18034072866802E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256418672723E+00 0.18261636831767E+00 0.33262243778333E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    660.61501198
 0.99340182411804E+00 0.30884805610215E+03 0.46251400414282E+03 0.41162999311334E+03 0.39709151379953E+03
 0.22999999992395E+00 0.00000000000000E+00 0.14019325201448E+00 0.00000000000000E+00 -.18215289338976E+02
 0.94576434642219E-03 0.12289107679661E+01 0.80000000000000E+04 0.30000000000000E+04 0.65098298497622E+01
 0.24411861936608E+01 0.32789996076972E+03 0.29215008719197E+03 0.32714050934344E+03 0.36619921871705E+03
 0.29215001140989E+03 0.29215002942336E+03 0.32360628884111E+03 0.36616688760799E+03 0.29215000944855E+03
 0.29215002934538E+03 0.32714050934344E+03 0.36619921871705E+03 0.29215001140989E+03 0.29215002942336E+03
 0.32360628884111E+03 0.36616688760799E+03 0.29215000944855E+03 0.29215002934538E+03 0.41503628729215E+03
 0.32513559254917E+03 0.16495914744106E+04 0.15607729300223E+04 0.72047927135099E+03 0.11744751017897E+04
 0.45039343408192E+03 0.11003840104891E+04 0.13299024480530E+04 0.10318592348588E+04 0.20538411689831E+04
 0.99415298541331E+03 0.13293121490034E+04 0.94233911147686E+03 0.20535509245606E+04 0.11003840104891E+04
 0.13299024480530E+04 0.10318592348588E+04 0.20538411689831E+04 0.99415298541330E+03 0.13293121490034E+04
 0.94233911147686E+03 0.20535509245606E+04 0.16785555368864E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53723439487258E+03 0.12948386985343E+01
 0.12948386985343E+01 0.24797077926012E+01 0.00000000000000E+00 0.33279944003420E+03 0.33279944003420E+03
 0.33279944003420E+03 0.33279944003420E+03 0.00000000000000E+00 0.00000000000000E+00 0.15608127520157E+00
 0.00000000000000E+00 -.11804877208585E+02 0.10000000000000E-02 0.14048238695649E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56946640595436E+01 0.21354990223289E+01 0.32513622356883E+03 0.41503666340904E+03
 0.30404366307472E+03 0.30404366307472E+03 0.29215000346887E+03 0.29215000340528E+03 0.30404142718105E+03
 0.30404142718105E+03 0.29215000346973E+03 0.29215000340613E+03 0.30404366307472E+03 0.30404366307472E+03
 0.29215000346887E+03 0.29215000340528E+03 0.30404142718105E+03 0.30404142718105E+03 0.29215000346973E+03
 0.29215000340613E+03 0.30228633673541E+03 0.29215002230250E+03 0.39041338813722E+02 0.47292352150729E+02
 0.18908869675840E+03 0.45852968183481E+03 0.26849554159262E+03 0.15566156203922E+03 0.17734510291430E+03
 0.15566156203922E+03 0.36438581693487E+03 0.15567062160844E+03 0.17722972565003E+03 0.15567062160844E+03
 0.36428801897762E+03 0.15566156203922E+03 0.17734510291430E+03 0.15566156203922E+03 0.36438581693487E+03
 0.15567062160844E+03 0.17722972565003E+03 0.15567062160844E+03 0.36428801897762E+03 0.23070129748212E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35985704589135E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17103933601424E+00 0.00000000000000E+00 0.00000000000000E+00 0.17103933601424E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18020087868507E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18020087868507E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256426390112E+00 0.18251935123707E+00 0.33279944003420E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    672.82012861
 0.99324528020200E+00 0.30911818594396E+03 0.46298389351426E+03 0.41204176423788E+03 0.39747990887809E+03
 0.22999999995229E+00 0.00000000000000E+00 0.13987079528617E+00 0.00000000000000E+00 -.18235163913996E+02
 0.94493768371130E-03 0.12320727564907E+01 0.80000000000000E+04 0.30000000000000E+04 0.64931230382745E+01
 0.24349211393529E+01 0.32835197172836E+03 0.29215010411879E+03 0.32757408817150E+03 0.36695470472398E+03
 0.29215001393260E+03 0.29215003586376E+03 0.32401414258271E+03 0.36692255770242E+03 0.29215001155324E+03
 0.29215003577002E+03 0.32757408817150E+03 0.36695470472398E+03 0.29215001393260E+03 0.29215003586376E+03
 0.32401414258271E+03 0.36692255770242E+03 0.29215001155324E+03 0.29215003577002E+03 0.41586626491647E+03
 0.32602400318899E+03 0.16543437561221E+04 0.15644564367305E+04 0.71813102995096E+03 0.11678836209607E+04
 0.44616193586000E+03 0.11034109817676E+04 0.13322081394077E+04 0.10341229163315E+04 0.20531388035263E+04
 0.99739452601283E+03 0.13316239080251E+04 0.94497616722201E+03 0.20528524995752E+04 0.11034109817676E+04
 0.13322081394077E+04 0.10341229163315E+04 0.20531388035263E+04 0.99739452601283E+03 0.13316239080251E+04
 0.94497616722201E+03 0.20528524995752E+04 0.16780888215120E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53770677029711E+03 0.12948388449676E+01
 0.12948388449676E+01 0.25285282590919E+01 0.00000000000000E+00 0.33305000099354E+03 0.33305000099354E+03
 0.33305000099354E+03 0.33305000099354E+03 0.00000000000000E+00 0.00000000000000E+00 0.15518035843004E+00
 0.00000000000000E+00 -.11797516379664E+02 0.10000000000000E-02 0.14209393925075E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56300782722919E+01 0.21112793521095E+01 0.32602474179869E+03 0.41586629354035E+03
 0.30421189284749E+03 0.30421189284749E+03 0.29215000429376E+03 0.29215000416638E+03 0.30420959982210E+03
 0.30420959982210E+03 0.29215000429483E+03 0.29215000416741E+03 0.30421189284749E+03 0.30421189284749E+03
 0.29215000429376E+03 0.29215000416638E+03 0.30420959982210E+03 0.30420959982210E+03 0.29215000429483E+03
 0.29215000416741E+03 0.30243195726373E+03 0.29215002668131E+03 0.35029793226443E+02 0.42428968349339E+02
 0.19058796545561E+03 0.46108033123751E+03 0.26953942595462E+03 0.15874841194365E+03 0.17867838376300E+03
 0.15874841194365E+03 0.36631484999293E+03 0.15875774641182E+03 0.17856132361931E+03 0.15875774641182E+03
 0.36621582489555E+03 0.15874841194365E+03 0.17867838376300E+03 0.15874841194365E+03 0.36631484999293E+03
 0.15875774641182E+03 0.17856132361931E+03 0.15875774641182E+03 0.36621582489555E+03 0.23072480388704E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36014802784528E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17100302912306E+00 0.00000000000000E+00 0.00000000000000E+00 0.17100302912306E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18006238810194E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18006238810194E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256396474032E+00 0.18238173492425E+00 0.33305000099354E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    682.82150831
 0.99315413512104E+00 0.30933731923431E+03 0.46336580827426E+03 0.41237446466877E+03 0.39779332520135E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13961708423441E+00 0.00000000000000E+00 -.18254921597804E+02
 0.94426810956622E-03 0.12345235166322E+01 0.80000000000000E+04 0.30000000000000E+04 0.64802329742768E+01
 0.24300873653538E+01 0.32872048336997E+03 0.29215012000962E+03 0.32792753179658E+03 0.36756778827736E+03
 0.29215001635017E+03 0.29215004202412E+03 0.32434691953085E+03 0.36753578781458E+03 0.29215001357276E+03
 0.29215004191553E+03 0.32792753179658E+03 0.36756778827736E+03 0.29215001635017E+03 0.29215004202412E+03
 0.32434691953085E+03 0.36753578781458E+03 0.29215001357276E+03 0.29215004191553E+03 0.41653494897864E+03
 0.32674339387740E+03 0.16581702103614E+04 0.15674030017839E+04 0.71625207017185E+03 0.11626282454595E+04
 0.44279491493676E+03 0.11058556535541E+04 0.13340534614849E+04 0.10359391724665E+04 0.20525548926426E+04
 0.10000139362627E+04 0.13334739706762E+04 0.94709568700390E+03 0.20522716342870E+04 0.11058556535541E+04
 0.13340534614849E+04 0.10359391724665E+04 0.20525548926426E+04 0.10000139362627E+04 0.13334739706762E+04
 0.94709568700390E+03 0.20522716342870E+04 0.16777049151935E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53808568305287E+03 0.12948389905398E+01
 0.12948389905398E+01 0.25685337778878E+01 0.00000000000000E+00 0.33326073974573E+03 0.33326073974573E+03
 0.33326073974573E+03 0.33326073974573E+03 0.00000000000000E+00 0.00000000000000E+00 0.15446651025804E+00
 0.00000000000000E+00 -.11796525333105E+02 0.10000000000000E-02 0.14336381653461E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55802085863615E+01 0.20925782198855E+01 0.32674422930238E+03 0.41653469450997E+03
 0.30435036881208E+03 0.30435036881208E+03 0.29215000504680E+03 0.29215000489708E+03 0.30434802799764E+03
 0.30434802799764E+03 0.29215000504805E+03 0.29215000489830E+03 0.30435036881208E+03 0.30435036881208E+03
 0.29215000504680E+03 0.29215000489708E+03 0.30434802799764E+03 0.30434802799764E+03 0.29215000504805E+03
 0.29215000489830E+03 0.30255187754795E+03 0.29215003079848E+03 0.31787578589428E+02 0.38521369340927E+02
 0.19183184579490E+03 0.46323870937552E+03 0.27044770435164E+03 0.16127493649819E+03 0.17978661179392E+03
 0.16127493649819E+03 0.36795143393201E+03 0.16128450281328E+03 0.17966825056207E+03 0.16128450281328E+03
 0.36785148996366E+03 0.16127493649819E+03 0.17978661179392E+03 0.16127493649819E+03 0.36795143393201E+03
 0.16128450281328E+03 0.17966825056207E+03 0.16128450281328E+03 0.36785148996366E+03 0.23079585579535E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36039368238600E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17100688895879E+00 0.00000000000000E+00 0.00000000000000E+00 0.17100688895879E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17999714876561E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17999714876561E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256356337666E+00 0.18226598296307E+00 0.33326073974573E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    691.08892990
 0.99303583525540E+00 0.30951946309939E+03 0.46368257948797E+03 0.41265274647180E+03 0.39805592355298E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13941409676893E+00 0.00000000000000E+00 -.18240958320449E+02
 0.94371256329730E-03 0.12364518430349E+01 0.80000000000000E+04 0.30000000000000E+04 0.64701266329658E+01
 0.24262974873622E+01 0.32902555173953E+03 0.29215013425518E+03 0.32822000108044E+03 0.36807006925432E+03
 0.29215001854387E+03 0.29215004760775E+03 0.32462278365185E+03 0.36803818939994E+03 0.29215001540664E+03
 0.29215004748582E+03 0.32822000108044E+03 0.36807006925432E+03 0.29215001854387E+03 0.29215004760775E+03
 0.32462278365185E+03 0.36803818939994E+03 0.29215001540664E+03 0.29215004748582E+03 0.41707228389395E+03
 0.32732621032177E+03 0.16613545156872E+04 0.15698625602923E+04 0.71490758816634E+03 0.11586853068719E+04
 0.44020318076470E+03 0.11078850377501E+04 0.13356145962756E+04 0.10374517526884E+04 0.20521587068364E+04
 0.10021846894673E+04 0.13350389704169E+04 0.94885357559550E+03 0.20518779233732E+04 0.11078850377501E+04
 0.13356145962756E+04 0.10374517526884E+04 0.20521587068364E+04 0.10021846894673E+04 0.13350389704169E+04
 0.94885357559549E+03 0.20518779233732E+04 0.16775350636997E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53840596185002E+03 0.12948388876601E+01
 0.12948388876601E+01 0.26016034642825E+01 0.00000000000000E+00 0.33343457086940E+03 0.33343457086940E+03
 0.33343457086940E+03 0.33343457086940E+03 0.00000000000000E+00 0.00000000000000E+00 0.15389299183416E+00
 0.00000000000000E+00 -.11760980690273E+02 0.10000000000000E-02 0.14438094254067E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55408974752653E+01 0.20778365532245E+01 0.32732716922835E+03 0.41707174463833E+03
 0.30446640577491E+03 0.30446640577491E+03 0.29215000563570E+03 0.29215000556083E+03 0.30446402483802E+03
 0.30446402483802E+03 0.29215000563709E+03 0.29215000556221E+03 0.30446640577491E+03 0.30446640577491E+03
 0.29215000563570E+03 0.29215000556083E+03 0.30446402483802E+03 0.30446402483802E+03 0.29215000563709E+03
 0.29215000556221E+03 0.30265249752152E+03 0.29215003449272E+03 0.29155592238282E+02 0.35363816617644E+02
 0.19283620730111E+03 0.46496468972778E+03 0.27116430139017E+03 0.16332104667683E+03 0.18067937868565E+03
 0.16332104667683E+03 0.36925172567789E+03 0.16333081002486E+03 0.18056004808491E+03 0.16333081002486E+03
 0.36915113188510E+03 0.16332104667683E+03 0.18067937868565E+03 0.16332104667683E+03 0.36925172567789E+03
 0.16333081002486E+03 0.18056004808491E+03 0.16333081002486E+03 0.36915113188510E+03 0.23084228797113E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36058662291395E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17074108489978E+00 0.00000000000000E+00 0.00000000000000E+00 0.17074108489978E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17978087718446E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17978087718446E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256423677822E+00 0.18217173614884E+00 0.33343457086940E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    700.52052683
 0.99291987622367E+00 0.30972590131303E+03 0.46404047686316E+03 0.41296647377824E+03 0.39835197623458E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13918931858854E+00 0.00000000000000E+00 -.18248033004527E+02
 0.94308349503619E-03 0.12385647914373E+01 0.80000000000000E+04 0.30000000000000E+04 0.64590888222461E+01
 0.24221583083423E+01 0.32937082043998E+03 0.29215015228213E+03 0.32855106459355E+03 0.36864013450043E+03
 0.29215002136313E+03 0.29215005477320E+03 0.32493493071588E+03 0.36860838720545E+03 0.29215001776570E+03
 0.29215005463434E+03 0.32855106459355E+03 0.36864013450043E+03 0.29215002136313E+03 0.29215005477320E+03
 0.32493493071588E+03 0.36860838720545E+03 0.29215001776570E+03 0.29215005463434E+03 0.41768932042625E+03
 0.32799288717652E+03 0.16649411511448E+04 0.15726296956530E+04 0.71320839866150E+03 0.11539427183972E+04
 0.43716827774244E+03 0.11101738718453E+04 0.13373350393024E+04 0.10391562499564E+04 0.20516482310716E+04
 0.10046357458589E+04 0.13367635900472E+04 0.95083849155268E+03 0.20513700889044E+04 0.11101738718453E+04
 0.13373350393024E+04 0.10391562499564E+04 0.20516482310716E+04 0.10046357458589E+04 0.13367635900472E+04
 0.95083849155268E+03 0.20513700889044E+04 0.16772415785931E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53876544943534E+03 0.12948389397855E+01
 0.12948389397855E+01 0.26393298519790E+01 0.00000000000000E+00 0.33363471517005E+03 0.33363471517005E+03
 0.33363471517005E+03 0.33363471517005E+03 0.00000000000000E+00 0.00000000000000E+00 0.15325622640971E+00
 0.00000000000000E+00 -.11746437955296E+02 0.10000000000000E-02 0.14550579593365E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54980627738347E+01 0.20617735401880E+01 0.32799395215318E+03 0.41768853134836E+03
 0.30459778449648E+03 0.30459778449648E+03 0.29215000650139E+03 0.29215000641502E+03 0.30459535788188E+03
 0.30459535788188E+03 0.29215000650298E+03 0.29215000641660E+03 0.30459778449648E+03 0.30459778449648E+03
 0.29215000650139E+03 0.29215000641502E+03 0.30459535788188E+03 0.30459535788188E+03 0.29215000650298E+03
 0.29215000641660E+03 0.30276639315302E+03 0.29215003917294E+03 0.26114318822986E+02 0.31733994628264E+02
 0.19398984312059E+03 0.46697915127847E+03 0.27201935894228E+03 0.16567871712313E+03 0.18170527506788E+03
 0.16567871712313E+03 0.37077336618332E+03 0.16568870465864E+03 0.18158479791935E+03 0.16568870465864E+03
 0.37067199093106E+03 0.16567871712313E+03 0.18170527506788E+03 0.16567871712313E+03 0.37077336618332E+03
 0.16568870465864E+03 0.18158479791935E+03 0.16568870465864E+03 0.37067199093106E+03 0.23091099984234E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36081429771959E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17064279852394E+00 0.00000000000000E+00 0.00000000000000E+00 0.17064279852394E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17964073444418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17964073444418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256424977986E+00 0.18206249291262E+00 0.33363471517005E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    711.13688839
 0.99279690451367E+00 0.30995698863700E+03 0.46444024329991E+03 0.41331674429039E+03 0.39868257671127E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13894482644025E+00 0.00000000000000E+00 -.18259450426884E+02
 0.94238027630278E-03 0.12408312756232E+01 0.80000000000000E+04 0.30000000000000E+04 0.64472907454576E+01
 0.24177340295466E+01 0.32975710402892E+03 0.29215017499557E+03 0.32892145328599E+03 0.36927684219805E+03
 0.29215002497598E+03 0.29215006394085E+03 0.32528427867888E+03 0.36924523939124E+03 0.29215002079196E+03
 0.29215006378058E+03 0.32892145328599E+03 0.36927684219805E+03 0.29215002497598E+03 0.29215006394085E+03
 0.32528427867887E+03 0.36924523939124E+03 0.29215002079196E+03 0.29215006378058E+03 0.41837923932364E+03
 0.32873939722216E+03 0.16689362063646E+04 0.15757073170157E+04 0.71125459632495E+03 0.11485880177012E+04
 0.43377714839460E+03 0.11127254377077E+04 0.13392293354331E+04 0.10410538428673E+04 0.20510553878535E+04
 0.10073693821840E+04 0.13386623810322E+04 0.95304989474168E+03 0.20507800622475E+04 0.11127254377077E+04
 0.13392293354331E+04 0.10410538428673E+04 0.20510553878535E+04 0.10073693821840E+04 0.13386623810322E+04
 0.95304989474168E+03 0.20507800622475E+04 0.16768850170255E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53916615905774E+03 0.12948390239077E+01
 0.12948390239077E+01 0.26817952982361E+01 0.00000000000000E+00 0.33386151663133E+03 0.33386151663133E+03
 0.33386151663133E+03 0.33386151663133E+03 0.00000000000000E+00 0.00000000000000E+00 0.15256124204768E+00
 0.00000000000000E+00 -.11734108412554E+02 0.10000000000000E-02 0.14672821758516E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54522573310461E+01 0.20445964991423E+01 0.32874056394407E+03 0.41837820936104E+03
 0.30474548076625E+03 0.30474548076625E+03 0.29215000766440E+03 0.29215000751129E+03 0.30474300255179E+03
 0.30474300255179E+03 0.29215000766627E+03 0.29215000751312E+03 0.30474548076625E+03 0.30474548076625E+03
 0.29215000766440E+03 0.29215000751129E+03 0.30474300255179E+03 0.30474300255179E+03 0.29215000766627E+03
 0.29215000751312E+03 0.30289447013318E+03 0.29215004507723E+03 0.22682823325402E+02 0.27661406079218E+02
 0.19528678389969E+03 0.46926299221891E+03 0.27299977439972E+03 0.16833449092398E+03 0.18285833876393E+03
 0.16833449092398E+03 0.37249858994418E+03 0.16834473263445E+03 0.18273658216687E+03 0.16834473263445E+03
 0.37239634819448E+03 0.16833449092398E+03 0.18285833876393E+03 0.16833449092398E+03 0.37249858994418E+03
 0.16834473263445E+03 0.18273658216687E+03 0.16834473263445E+03 0.37239634819448E+03 0.23099916356484E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36107234338640E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17056433986464E+00 0.00000000000000E+00 0.00000000000000E+00 0.17056433986464E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17949862926929E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17949862926929E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256414511131E+00 0.18193872456623E+00 0.33386151663133E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    721.26543091
 0.99269536166362E+00 0.31017580581172E+03 0.46481833469738E+03 0.41364736098381E+03 0.39899456613458E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13871989932486E+00 0.00000000000000E+00 -.18273309845931E+02
 0.94171533420698E-03 0.12428852881230E+01 0.80000000000000E+04 0.30000000000000E+04 0.64366358476106E+01
 0.24137384428540E+01 0.33012339961533E+03 0.29215019936332E+03 0.32927267411993E+03 0.36987930403691E+03
 0.29215002892133E+03 0.29215007393500E+03 0.32561569532192E+03 0.36984783467161E+03 0.29215002410033E+03
 0.29215007375170E+03 0.32927267411993E+03 0.36987930403691E+03 0.29215002892133E+03 0.29215007393500E+03
 0.32561569532192E+03 0.36984783467161E+03 0.29215002410033E+03 0.29215007375170E+03 0.41903159054726E+03
 0.32944705088326E+03 0.16726973547746E+04 0.15785951443485E+04 0.70936483754031E+03 0.11434893056067E+04
 0.43057764387871E+03 0.11151311611412E+04 0.13409906977001E+04 0.10428370668395E+04 0.20504666719359E+04
 0.10099482691611E+04 0.13404278412005E+04 0.95513072354569E+03 0.20501938898751E+04 0.11151311611412E+04
 0.13409906977001E+04 0.10428370668395E+04 0.20504666719359E+04 0.10099482691611E+04 0.13404278412005E+04
 0.95513072354569E+03 0.20501938898751E+04 0.16765173651502E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53954313677274E+03 0.12948391260222E+01
 0.12948391260222E+01 0.27223094683050E+01 0.00000000000000E+00 0.33407971025580E+03 0.33407971025580E+03
 0.33407971025580E+03 0.33407971025580E+03 0.00000000000000E+00 0.00000000000000E+00 0.15191896500956E+00
 0.00000000000000E+00 -.11726501236136E+02 0.10000000000000E-02 0.14785259213861E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54107945517113E+01 0.20290479568917E+01 0.32944831319529E+03 0.41903034683878E+03
 0.30488632802257E+03 0.30488632802257E+03 0.29215000888787E+03 0.29215000871031E+03 0.30488380036171E+03
 0.30488380036171E+03 0.29215000889003E+03 0.29215000871243E+03 0.30488632802257E+03 0.30488632802257E+03
 0.29215000888787E+03 0.29215000871031E+03 0.30488380036171E+03 0.30488380036171E+03 0.29215000889003E+03
 0.29215000871243E+03 0.30301664067732E+03 0.29215005141974E+03 0.19410007835593E+02 0.23799768833197E+02
 0.19652531325239E+03 0.47146490377664E+03 0.27395696395799E+03 0.17086911964522E+03 0.18395956063600E+03
 0.17086911964522E+03 0.37416299839046E+03 0.17087960590610E+03 0.18383660271562E+03 0.17087960590610E+03
 0.37405995175287E+03 0.17086911964522E+03 0.18395956063600E+03 0.17086911964522E+03 0.37416299839046E+03
 0.17087960590610E+03 0.18383660271562E+03 0.17087960590610E+03 0.37405995175287E+03 0.23110005534699E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36132185252227E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17051782589603E+00 0.00000000000000E+00 0.00000000000000E+00 0.17051782589603E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17939999757422E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17939999757422E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256392199847E+00 0.18181967560441E+00 0.33407971025580E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    731.39397343
 0.99260298664645E+00 0.31039335281749E+03 0.46519388743197E+03 0.41397539643437E+03 0.39930409609052E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13850309061045E+00 0.00000000000000E+00 -.18278140979537E+02
 0.94105526439885E-03 0.12448323564756E+01 0.80000000000000E+04 0.30000000000000E+04 0.64265681707134E+01
 0.24099630640175E+01 0.33048785883899E+03 0.29215022649438E+03 0.32962212024326E+03 0.37047684887770E+03
 0.29215003338652E+03 0.29215008522773E+03 0.32594563584366E+03 0.37044550920683E+03 0.29215002784838E+03
 0.29215008501873E+03 0.32962212024326E+03 0.37047684887770E+03 0.29215003338652E+03 0.29215008522773E+03
 0.32594563584366E+03 0.37044550920683E+03 0.29215002784838E+03 0.29215008501873E+03 0.41967676985124E+03
 0.33014914047318E+03 0.16764192175418E+04 0.15814456525163E+04 0.70748550457481E+03 0.11384596018799E+04
 0.42743666978222E+03 0.11175143295064E+04 0.13427207010509E+04 0.10445992529676E+04 0.20498745711923E+04
 0.10125036777395E+04 0.13421617712695E+04 0.95718819520757E+03 0.20496042021494E+04 0.11175143295064E+04
 0.13427207010509E+04 0.10445992529676E+04 0.20498745711924E+04 0.10125036777395E+04 0.13421617712695E+04
 0.95718819520758E+03 0.20496042021494E+04 0.16761495699155E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53991644146205E+03 0.12948391616174E+01
 0.12948391616174E+01 0.27628236383740E+01 0.00000000000000E+00 0.33429933841969E+03 0.33429933841969E+03
 0.33429933841969E+03 0.33429933841969E+03 0.00000000000000E+00 0.00000000000000E+00 0.15129636296758E+00
 0.00000000000000E+00 -.11708734506795E+02 0.10000000000000E-02 0.14893746956083E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53713817104519E+01 0.20142681414194E+01 0.33015051276468E+03 0.41967530507029E+03
 0.30502738454682E+03 0.30502738454682E+03 0.29215001027454E+03 0.29215001006928E+03 0.30502480727840E+03
 0.30502480727840E+03 0.29215001027702E+03 0.29215001007171E+03 0.30502738454682E+03 0.30502738454682E+03
 0.29215001027454E+03 0.29215001006928E+03 0.30502480727840E+03 0.30502480727840E+03 0.29215001027702E+03
 0.29215001007171E+03 0.30313904094616E+03 0.29215005848982E+03 0.16145253795983E+02 0.19969895555209E+02
 0.19776191134045E+03 0.47367226056445E+03 0.27492153966729E+03 0.17339836679803E+03 0.18505872565812E+03
 0.17339836679803E+03 0.37583004823528E+03 0.17340909951541E+03 0.18493458861272E+03 0.17340909951541E+03
 0.37572622017568E+03 0.17339836679803E+03 0.18505872565812E+03 0.17339836679803E+03 0.37583004823528E+03
 0.17340909951541E+03 0.18493458861272E+03 0.17340909951541E+03 0.37572622017568E+03 0.23121067257215E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36156911401408E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17039307119246E+00 0.00000000000000E+00 0.00000000000000E+00 0.17039307119246E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17925150991071E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17925150991071E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256399029854E+00 0.18170032740472E+00 0.33429933841969E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    741.52251594
 0.99249691649287E+00 0.31061013579917E+03 0.46556715965667E+03 0.41430237020084E+03 0.39961287337798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13829387948758E+00 0.00000000000000E+00 -.18286633234714E+02
 0.94039839835260E-03 0.12466790049731E+01 0.80000000000000E+04 0.30000000000000E+04 0.64170487896944E+01
 0.24063932961354E+01 0.33085047790862E+03 0.29215025662820E+03 0.32996978312221E+03 0.37106969857326E+03
 0.29215003842609E+03 0.29215009795266E+03 0.32627406734239E+03 0.37103848493571E+03 0.29215003208275E+03
 0.29215009771505E+03 0.32996978312221E+03 0.37106969857326E+03 0.29215003842609E+03 0.29215009795266E+03
 0.32627406734239E+03 0.37103848493571E+03 0.29215003208275E+03 0.29215009771505E+03 0.42031576885829E+03
 0.33084614619532E+03 0.16801192878515E+04 0.15842796592553E+04 0.70561040562854E+03 0.11334853860303E+04
 0.42434692837365E+03 0.11198823813982E+04 0.13444251945025E+04 0.10463503513396E+04 0.20492834440626E+04
 0.10150429098245E+04 0.13438700418777E+04 0.95923208194671E+03 0.20490153776016E+04 0.11198823813982E+04
 0.13444251945025E+04 0.10463503513396E+04 0.20492834440626E+04 0.10150429098245E+04 0.13438700418777E+04
 0.95923208194671E+03 0.20490153776016E+04 0.16757918562717E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54028954896908E+03 0.12948392241873E+01
 0.12948392241873E+01 0.28033378084430E+01 0.00000000000000E+00 0.33451870044144E+03 0.33451870044144E+03
 0.33451870044144E+03 0.33451870044144E+03 0.00000000000000E+00 0.00000000000000E+00 0.15069304784234E+00
 0.00000000000000E+00 -.11695301554112E+02 0.10000000000000E-02 0.14998439529344E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53338882250704E+01 0.20002080844014E+01 0.33084763264371E+03 0.42031409063337E+03
 0.30516845834974E+03 0.30516845834974E+03 0.29215001184179E+03 0.29215001160522E+03 0.30516583137974E+03
 0.30516583137974E+03 0.29215001184463E+03 0.29215001160800E+03 0.30516845834974E+03 0.30516845834974E+03
 0.29215001184179E+03 0.29215001160522E+03 0.30516583137974E+03 0.30516583137974E+03 0.29215001184463E+03
 0.29215001160800E+03 0.30326150698541E+03 0.29215006635141E+03 0.12874564662124E+02 0.16155960862242E+02
 0.19898665087668E+03 0.47586632597594E+03 0.27588474184487E+03 0.17591847738365E+03 0.18614594489761E+03
 0.17591847738365E+03 0.37748496274412E+03 0.17592945814310E+03 0.18602064382626E+03 0.17592945814310E+03
 0.37738036947323E+03 0.17591847738365E+03 0.18614594489761E+03 0.17591847738365E+03 0.37748496274412E+03
 0.17592945814310E+03 0.18602064382626E+03 0.17592945814310E+03 0.37738036947323E+03 0.23131828647189E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36181719079896E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17030125419985E+00 0.00000000000000E+00 0.00000000000000E+00 0.17030125419985E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17912635068381E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17912635068381E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256393442000E+00 0.18158114194890E+00 0.33451870044144E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    752.55356037
 0.99238609815119E+00 0.31084496682919E+03 0.46597107944041E+03 0.41465608023521E+03 0.39994692488529E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13807413273136E+00 0.00000000000000E+00 -.18296937305079E+02
 0.93968786912492E-03 0.12485836612491E+01 0.80000000000000E+04 0.30000000000000E+04 0.64072598803646E+01
 0.24027224551367E+01 0.33124322634741E+03 0.29215029320464E+03 0.33034631994657E+03 0.37171022653627E+03
 0.29215004464589E+03 0.29215011363104E+03 0.32662994503178E+03 0.37167914599361E+03 0.29215003731413E+03
 0.29215011335864E+03 0.33034631994657E+03 0.37171022653627E+03 0.29215004464589E+03 0.29215011363104E+03
 0.32662994503178E+03 0.37167914599361E+03 0.29215003731413E+03 0.29215011335864E+03 0.42100569795717E+03
 0.33160013775071E+03 0.16841144182318E+04 0.15873355922364E+04 0.70355835973255E+03 0.11281034236024E+04
 0.42102727207119E+03 0.11224410318884E+04 0.13462511562923E+04 0.10482401400722E+04 0.20486386680254E+04
 0.10177871014296E+04 0.13456999405740E+04 0.96143842293242E+03 0.20483729771557E+04 0.11224410318884E+04
 0.13462511562923E+04 0.10482401400722E+04 0.20486386680254E+04 0.10177871014296E+04 0.13456999405740E+04
 0.96143842293242E+03 0.20483729771557E+04 0.16753983085007E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54069278908004E+03 0.12948393001064E+01
 0.12948393001064E+01 0.28474619861253E+01 0.00000000000000E+00 0.33475821281721E+03 0.33475821281721E+03
 0.33475821281721E+03 0.33475821281721E+03 0.00000000000000E+00 0.00000000000000E+00 0.15005714058320E+00
 0.00000000000000E+00 -.11681989197242E+02 0.10000000000000E-02 0.15108259255418E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52951169719510E+01 0.19856688644816E+01 0.33160174543708E+03 0.42100380508966E+03
 0.30532201134853E+03 0.30532201134853E+03 0.29215001379874E+03 0.29215001350365E+03 0.30531933020161E+03
 0.30531933020161E+03 0.29215001380202E+03 0.29215001350686E+03 0.30532201134853E+03 0.30532201134853E+03
 0.29215001379874E+03 0.29215001350365E+03 0.30531933020161E+03 0.30531933020161E+03 0.29215001380202E+03
 0.29215001350686E+03 0.30339485290989E+03 0.29215007590512E+03 0.93085504210967E+01 0.12024636196963E+02
 0.20031505532534E+03 0.47825713571147E+03 0.27694050510950E+03 0.17865916635710E+03 0.18732440395186E+03
 0.17865916635710E+03 0.37928724118130E+03 0.17867041854589E+03 0.18719784269536E+03 0.17867041854589E+03
 0.37918182283373E+03 0.17865916635710E+03 0.18732440395186E+03 0.17865916635710E+03 0.37928724118130E+03
 0.17867041854589E+03 0.18719784269536E+03 0.17867041854589E+03 0.37918182283373E+03 0.23144114741070E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36208770473098E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17021204598726E+00 0.00000000000000E+00 0.00000000000000E+00 0.17021204598726E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17899400325933E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17899400325933E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256383490985E+00 0.18145114372252E+00 0.33475821281721E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    761.05246916
 0.99230369694663E+00 0.31102502382942E+03 0.46628045820125E+03 0.41492694436851E+03 0.40020275913371E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13791040661388E+00 0.00000000000000E+00 -.18304591163004E+02
 0.93914379892401E-03 0.12499779886293E+01 0.80000000000000E+04 0.30000000000000E+04 0.64001127002027E+01
 0.24000422625760E+01 0.33154440918211E+03 0.29215032425272E+03 0.33063505779826E+03 0.37220006295255E+03
 0.29215005000490E+03 0.29215012711884E+03 0.32690298356933E+03 0.37216908221341E+03 0.29215004182567E+03
 0.29215012681685E+03 0.33063505779826E+03 0.37220006295255E+03 0.29215005000490E+03 0.29215012711884E+03
 0.32690298356933E+03 0.37216908221341E+03 0.29215004182567E+03 0.29215012681685E+03 0.42153227378998E+03
 0.33217690543148E+03 0.16871678124155E+04 0.15896676079712E+04 0.70198290473937E+03 0.11240028314498E+04
 0.41851001218670E+03 0.11243978432381E+04 0.13476375356566E+04 0.10496833394157E+04 0.20481425875497E+04
 0.10198861313168E+04 0.13470892399529E+04 0.96312371737896E+03 0.20478786433553E+04 0.11243978432381E+04
 0.13476375356566E+04 0.10496833394157E+04 0.20481425875498E+04 0.10198861313168E+04 0.13470892399529E+04
 0.96312371737896E+03 0.20478786433553E+04 0.16750976291372E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54100133784210E+03 0.12948393564992E+01
 0.12948393564992E+01 0.28814576212905E+01 0.00000000000000E+00 0.33494306171127E+03 0.33494306171127E+03
 0.33494306171127E+03 0.33494306171127E+03 0.00000000000000E+00 0.00000000000000E+00 0.14958181603631E+00
 0.00000000000000E+00 -.11671601852577E+02 0.10000000000000E-02 0.15189976696144E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52666308579860E+01 0.19749865717447E+01 0.33217860517505E+03 0.42153022462742E+03
 0.30544035306645E+03 0.30544035306645E+03 0.29215001547237E+03 0.29215001514149E+03 0.30543763012670E+03
 0.30543763012670E+03 0.29215001547603E+03 0.29215001514506E+03 0.30544035306645E+03 0.30544035306645E+03
 0.29215001547237E+03 0.29215001514149E+03 0.30543763012670E+03 0.30543763012670E+03 0.29215001547603E+03
 0.29215001514506E+03 0.30349766015456E+03 0.29215008402337E+03 0.65623669588598E+01 0.88630506245010E+01
 0.20133380635771E+03 0.48009692736279E+03 0.27775645197329E+03 0.18076537112374E+03 0.18822751400125E+03
 0.18076537112374E+03 0.38067315908295E+03 0.18077683353821E+03 0.18809999071744E+03 0.18077683353821E+03
 0.38056711458651E+03 0.18076537112374E+03 0.18822751400125E+03 0.18076537112374E+03 0.38067315908295E+03
 0.18077683353821E+03 0.18809999071744E+03 0.18077683353821E+03 0.38056711458651E+03 0.23153961710226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36229619566249E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17014179414332E+00 0.00000000000000E+00 0.00000000000000E+00 0.17014179414332E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17889373054703E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17889373054703E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256376185845E+00 0.18135094601379E+00 0.33494306171127E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    772.38434754
 0.99219698187487E+00 0.31126398692675E+03 0.46669049399158E+03 0.41528592358722E+03 0.40054187462414E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13769938295590E+00 0.00000000000000E+00 -.18314841986683E+02
 0.93842270597199E-03 0.12517420882536E+01 0.80000000000000E+04 0.30000000000000E+04 0.63910929216746E+01
 0.23966598456280E+01 0.33194408318225E+03 0.29215036983413E+03 0.33101820308625E+03 0.37284835899050E+03
 0.29215005799090E+03 0.29215014718686E+03 0.32726547800148E+03 0.37281750771628E+03 0.29215004855495E+03
 0.29215014684138E+03 0.33101820308625E+03 0.37284835899050E+03 0.29215005799090E+03 0.29215014718686E+03
 0.32726547800148E+03 0.37281750771628E+03 0.29215004855495E+03 0.29215014684138E+03 0.42222810826109E+03
 0.33294058507792E+03 0.16912066782234E+04 0.15927480528939E+04 0.69988425879993E+03 0.11185865049154E+04
 0.41520282482144E+03 0.11269877783786E+04 0.13494582053436E+04 0.10515910677055E+04 0.20474804836909E+04
 0.10226647588263E+04 0.13489136550123E+04 0.96535190633249E+03 0.20472187592273E+04 0.11269877783786E+04
 0.13494582053436E+04 0.10515910677055E+04 0.20474804836909E+04 0.10226647588263E+04 0.13489136550123E+04
 0.96535190633248E+03 0.20472187592273E+04 0.16746975513803E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54140998007954E+03 0.12948394320260E+01
 0.12948394320260E+01 0.29267851348440E+01 0.00000000000000E+00 0.33518975863432E+03 0.33518975863432E+03
 0.33518975863432E+03 0.33518975863432E+03 0.00000000000000E+00 0.00000000000000E+00 0.14896724005750E+00
 0.00000000000000E+00 -.11657994577639E+02 0.10000000000000E-02 0.15295144816524E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52304179502486E+01 0.19614067313432E+01 0.33294240597164E+03 0.42222586088144E+03
 0.30559814186520E+03 0.30559814186520E+03 0.29215001796969E+03 0.29215001758540E+03 0.30559536316425E+03
 0.30559536316425E+03 0.29215001797390E+03 0.29215001758952E+03 0.30559814186520E+03 0.30559814186520E+03
 0.29215001796969E+03 0.29215001758540E+03 0.30559536316425E+03 0.30559536316425E+03 0.29215001797390E+03
 0.29215001758952E+03 0.30363478775245E+03 0.29215009595421E+03 0.29003301678595E+01 0.46757640295979E+01
 0.20268542770126E+03 0.48254511665640E+03 0.27884626181664E+03 0.18356686723922E+03 0.18942475305010E+03
 0.18356686723922E+03 0.38251598203782E+03 0.18357861123795E+03 0.18929595388615E+03 0.18357861123795E+03
 0.38240911009600E+03 0.18356686723922E+03 0.18942475305010E+03 0.18356686723922E+03 0.38251598203782E+03
 0.18357861123795E+03 0.18929595388615E+03 0.18357861123795E+03 0.38240911009600E+03 0.23167498134297E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36257409388934E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17004983257133E+00 0.00000000000000E+00 0.00000000000000E+00 0.17004983257133E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17876207867620E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17876207867620E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256365761799E+00 0.18121738807937E+00 0.33518975863432E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    780.88325634
 0.99211893745046E+00 0.31144239327551E+03 0.46699615324020E+03 0.41555354288934E+03 0.40079473707139E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13754638243060E+00 0.00000000000000E+00 -.18322510435842E+02
 0.93788506985504E-03 0.12529967196778E+01 0.80000000000000E+04 0.30000000000000E+04 0.63846934907040E+01
 0.23942600590140E+01 0.33224242924188E+03 0.29215040738541E+03 0.33130420150820E+03 0.37333103606426E+03
 0.29215006466639E+03 0.29215016393584E+03 0.32753619413668E+03 0.37330027926262E+03 0.29215005418500E+03
 0.29215016355449E+03 0.33130420150820E+03 0.37333103606426E+03 0.29215006466639E+03 0.29215016393584E+03
 0.32753619413668E+03 0.37330027926262E+03 0.29215005418500E+03 0.29215016355449E+03 0.42274542009999E+03
 0.33350942158771E+03 0.16942118525916E+04 0.15950370062167E+04 0.69831051424246E+03 0.11145595672508E+04
 0.41275750043709E+03 0.11289159737048E+04 0.13508025995421E+04 0.10530095818557E+04 0.20469823140190E+04
 0.10247337977995E+04 0.13502607525413E+04 0.96700906000847E+03 0.20467221768632E+04 0.11289159737048E+04
 0.13508025995421E+04 0.10530095818557E+04 0.20469823140190E+04 0.10247337977995E+04 0.13502607525413E+04
 0.96700906000846E+03 0.20467221768632E+04 0.16743971262000E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54171443359498E+03 0.12948394885263E+01
 0.12948394885263E+01 0.29607807700091E+01 0.00000000000000E+00 0.33537483652360E+03 0.33537483652360E+03
 0.33537483652360E+03 0.33537483652360E+03 0.00000000000000E+00 0.00000000000000E+00 0.14852030226942E+00
 0.00000000000000E+00 -.11647913184543E+02 0.10000000000000E-02 0.15371265711396E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52045161083051E+01 0.19516935406144E+01 0.33351133058284E+03 0.42274303285880E+03
 0.30571646519886E+03 0.30571646519886E+03 0.29215002005987E+03 0.29215001963088E+03 0.30571364465444E+03
 0.30571364465444E+03 0.29215002006454E+03 0.29215001963544E+03 0.30571646519886E+03 0.30571646519886E+03
 0.29215002005987E+03 0.29215001963088E+03 0.30571364465444E+03 0.30571364465444E+03 0.29215002006454E+03
 0.29215001963544E+03 0.30373765575493E+03 0.29215010579316E+03 0.15283664973096E+00 0.15573706883153E+01
 0.20369386162631E+03 0.48437631347427E+03 0.27966398253983E+03 0.18566288013476E+03 0.19031724191825E+03
 0.18566288013476E+03 0.38389327348377E+03 0.18567483625131E+03 0.19018748897548E+03 0.18567483625131E+03
 0.38378578447897E+03 0.18566288013476E+03 0.19031724191825E+03 0.18566288013476E+03 0.38389327348377E+03
 0.18567483625131E+03 0.19018748897548E+03 0.18567483625131E+03 0.38378578447897E+03 0.23177916756358E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36278233771745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16998164796246E+00 0.00000000000000E+00 0.00000000000000E+00 0.16998164796246E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17866478252606E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17866478252606E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256357621982E+00 0.18111731516486E+00 0.33537483652360E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    791.83055308
 0.99203019267135E+00 0.31167035967979E+03 0.46738603195024E+03 0.41589448249544E+03 0.40111688209364E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13735581287934E+00 0.00000000000000E+00 -.18341866116590E+02
 0.93719888944511E-03 0.12545323820042E+01 0.80000000000000E+04 0.30000000000000E+04 0.63768780421751E+01
 0.23913292658157E+01 0.33262298189358E+03 0.29215046135330E+03 0.33166908172364E+03 0.37394865933776E+03
 0.29215007442872E+03 0.29215018838364E+03 0.32788140446875E+03 0.37391801896616E+03 0.29215006242734E+03
 0.29215018795067E+03 0.33166908172364E+03 0.37394865933776E+03 0.29215007442872E+03 0.29215018838364E+03
 0.32788140446875E+03 0.37391801896616E+03 0.29215006242734E+03 0.29215018795067E+03 0.42341619636770E+03
 0.33424489374067E+03 0.16980425412764E+04 0.15979555724553E+04 0.69609899498168E+03 0.11091029495502E+04
 0.40952345959362E+03 0.11313754682981E+04 0.13524794927811E+04 0.10548199223761E+04 0.20462951255973E+04
 0.10273754818904E+04 0.13519409083138E+04 0.96912711762873E+03 0.20460368658389E+04 0.11313754682981E+04
 0.13524794927811E+04 0.10548199223761E+04 0.20462951255973E+04 0.10273754818904E+04 0.13519409083138E+04
 0.96912711762872E+03 0.20460368658389E+04 0.16739141790715E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54210131695404E+03 0.12948396311367E+01
 0.12948396311367E+01 0.30045699569700E+01 0.00000000000000E+00 0.33561356363054E+03 0.33561356363054E+03
 0.33561356363054E+03 0.33561356363054E+03 0.00000000000000E+00 0.00000000000000E+00 0.14796173242986E+00
 0.00000000000000E+00 -.11645322059254E+02 0.10000000000000E-02 0.15465933789041E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51726588960757E+01 0.19397470860284E+01 0.33424688855431E+03 0.42341367120832E+03
 0.30586723787417E+03 0.30586723787417E+03 0.29215002353176E+03 0.29215002262680E+03 0.30586436388376E+03
 0.30586436388376E+03 0.29215002353718E+03 0.29215002263201E+03 0.30586723787417E+03 0.30586723787417E+03
 0.29215002353176E+03 0.29215002262680E+03 0.30586436388376E+03 0.30586436388376E+03 0.29215002353718E+03
 0.29215002263201E+03 0.30386870411679E+03 0.29215011995055E+03 -.34445761301183E+01 -.24916138347618E+01
 0.20499864814943E+03 0.48676207176626E+03 0.28073843037608E+03 0.18839232598634E+03 0.19147228759724E+03
 0.18839232598634E+03 0.38569040134862E+03 0.18840455255920E+03 0.19134123462947E+03 0.18840455255920E+03
 0.38558204327347E+03 0.18839232598634E+03 0.19147228759724E+03 0.18839232598634E+03 0.38569040134862E+03
 0.18840455255920E+03 0.19134123462947E+03 0.18840455255920E+03 0.38558204327347E+03 0.23191902733913E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36305186147634E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16998001686203E+00 0.00000000000000E+00 0.00000000000000E+00 0.16998001686203E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17856570655857E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17856570655857E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256317077458E+00 0.18098806301418E+00 0.33561356363054E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    800.02602349
 0.99198757436159E+00 0.31183922211483E+03 0.46767533351970E+03 0.41614617146955E+03 0.40135443412947E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13721813969631E+00 0.00000000000000E+00 -.18361417722689E+02
 0.93669121097351E-03 0.12556195991855E+01 0.80000000000000E+04 0.30000000000000E+04 0.63713564245013E+01
 0.23892586591880E+01 0.33290709502857E+03 0.29215050572425E+03 0.33194146692880E+03 0.37440752368348E+03
 0.29215008257623E+03 0.29215020875382E+03 0.32813934669486E+03 0.37437696868033E+03 0.29215006931267E+03
 0.29215020827837E+03 0.33194146692880E+03 0.37440752368348E+03 0.29215008257623E+03 0.29215020875382E+03
 0.32813934669486E+03 0.37437696868033E+03 0.29215006931267E+03 0.29215020827837E+03 0.42390933421723E+03
 0.33478876848906E+03 0.17008598415240E+04 0.16000823328518E+04 0.69450054239019E+03 0.11051505124693E+04
 0.40717746736713E+03 0.11331897463138E+04 0.13536991604050E+04 0.10561424292956E+04 0.20457593060315E+04
 0.10293253857302E+04 0.13531629600229E+04 0.97067830687327E+03 0.20455024137341E+04 0.11331897463138E+04
 0.13536991604050E+04 0.10561424292956E+04 0.20457593060315E+04 0.10293253857302E+04 0.13531629600229E+04
 0.97067830687327E+03 0.20455024137341E+04 0.16735503222421E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54238511462737E+03 0.12948397751907E+01
 0.12948397751907E+01 0.30373518386283E+01 0.00000000000000E+00 0.33579368218685E+03 0.33579368218685E+03
 0.33579368218685E+03 0.33579368218685E+03 0.00000000000000E+00 0.00000000000000E+00 0.14755573812988E+00
 0.00000000000000E+00 -.11652517158439E+02 0.10000000000000E-02 0.15534348840933E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51498779137237E+01 0.19312042176464E+01 0.33479082881592E+03 0.42390670561571E+03
 0.30598065952472E+03 0.30598065952472E+03 0.29215002514648E+03 0.29215002513046E+03 0.30597774513630E+03
 0.30597774513630E+03 0.29215002515223E+03 0.29215002513620E+03 0.30598065952472E+03 0.30598065952472E+03
 0.29215002514648E+03 0.29215002513046E+03 0.30597774513630E+03 0.30597774513630E+03 0.29215002515223E+03
 0.29215002513620E+03 0.30396732802703E+03 0.29215013160240E+03 -.61022278874339E+01 -.54551635262146E+01
 0.20597821521052E+03 0.48857341770820E+03 0.28156531142163E+03 0.19042386544872E+03 0.19233996591565E+03
 0.19042386544872E+03 0.38705828254061E+03 0.19043629775245E+03 0.19220799061726E+03 0.19043629775245E+03
 0.38694932940595E+03 0.19042386544872E+03 0.19233996591565E+03 0.19042386544872E+03 0.38705828254061E+03
 0.19043629775245E+03 0.19220799061726E+03 0.19043629775245E+03 0.38694932940595E+03 0.23205279138970E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36326364380292E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17002583703412E+00 0.00000000000000E+00 0.00000000000000E+00 0.17002583703412E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17863854308889E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17863854308889E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256260076225E+00 0.18089037127850E+00 0.33579368218685E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    810.41261941
 0.99187341372254E+00 0.31205557335575E+03 0.46804515207089E+03 0.41647118008912E+03 0.40166180157416E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13704901171572E+00 0.00000000000000E+00 -.18346153586134E+02
 0.93604193465185E-03 0.12569151197825E+01 0.80000000000000E+04 0.30000000000000E+04 0.63647893752640E+01
 0.23867960157240E+01 0.33326894897904E+03 0.29215056480698E+03 0.33228821072431E+03 0.37498389775448E+03
 0.29215009350918E+03 0.29215023606482E+03 0.32846845270431E+03 0.37495345131494E+03 0.29215007855634E+03
 0.29215023553279E+03 0.33228821072431E+03 0.37498389775448E+03 0.29215009350918E+03 0.29215023606482E+03
 0.32846845270431E+03 0.37495345131494E+03 0.29215007855634E+03 0.29215023553279E+03 0.42451163699269E+03
 0.33546053897338E+03 0.17044833451285E+04 0.16028336853019E+04 0.69285296931059E+03 0.11008121275579E+04
 0.40449489340071E+03 0.11355154877971E+04 0.13553320939410E+04 0.10578482710260E+04 0.20452403336898E+04
 0.10318181606481E+04 0.13547989756312E+04 0.97266580637320E+03 0.20449852383748E+04 0.11355154877971E+04
 0.13553320939410E+04 0.10578482710260E+04 0.20452403336898E+04 0.10318181606481E+04 0.13547989756312E+04
 0.97266580637320E+03 0.20449852383748E+04 0.16733454092698E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54275635608280E+03 0.12948396627263E+01
 0.12948396627263E+01 0.30788982222993E+01 0.00000000000000E+00 0.33601864735296E+03 0.33601864735296E+03
 0.33601864735296E+03 0.33601864735296E+03 0.00000000000000E+00 0.00000000000000E+00 0.14705610291192E+00
 0.00000000000000E+00 -.11611069866246E+02 0.10000000000000E-02 0.15618293744704E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51221984493105E+01 0.19208244184914E+01 0.33546276361938E+03 0.42450876675796E+03
 0.30612720795471E+03 0.30612720795471E+03 0.29215002875010E+03 0.29215002849234E+03 0.30612424178078E+03
 0.30612424178078E+03 0.29215002875662E+03 0.29215002849879E+03 0.30612720795471E+03 0.30612720795471E+03
 0.29215002875010E+03 0.29215002849234E+03 0.30612424178078E+03 0.30612424178078E+03 0.29215002875662E+03
 0.29215002849879E+03 0.30409498151683E+03 0.29215014712584E+03 -.93941419922563E+01 -.90847462099007E+01
 0.20717188970840E+03 0.49072222931630E+03 0.28251448015936E+03 0.19291788731960E+03 0.19339131363115E+03
 0.19291788731960E+03 0.38866123354082E+03 0.19293058699908E+03 0.19325825160577E+03 0.19293058699908E+03
 0.38855160885272E+03 0.19291788731960E+03 0.19339131363115E+03 0.19291788731960E+03 0.38866123354082E+03
 0.19293058699908E+03 0.19325825160577E+03 0.19293058699908E+03 0.38855160885272E+03 0.23217821104521E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36350524581073E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16972541393090E+00 0.00000000000000E+00 0.00000000000000E+00 0.16972541393090E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17834124784554E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17834124784554E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256334756610E+00 0.18077012110709E+00 0.33601864735296E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    821.32910583
 0.99177487894234E+00 0.31228121432851E+03 0.46842888424351E+03 0.41680777210115E+03 0.40198022974118E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13687701109844E+00 0.00000000000000E+00 -.18361818046302E+02
 0.93536544638338E-03 0.12582096313473E+01 0.80000000000000E+04 0.30000000000000E+04 0.63582409486354E+01
 0.23843403557383E+01 0.33364527844165E+03 0.29215063338779E+03 0.33264892912036E+03 0.37558699827824E+03
 0.29215010639808E+03 0.29215026820615E+03 0.32881050343033E+03 0.37555666095824E+03 0.29215008946416E+03
 0.29215026760842E+03 0.33264892912036E+03 0.37558699827824E+03 0.29215010639808E+03 0.29215026820615E+03
 0.32881050343033E+03 0.37555666095824E+03 0.29215008946416E+03 0.29215026760842E+03 0.42515575979456E+03
 0.33617321900535E+03 0.17082373553908E+04 0.16056835914698E+04 0.69081406419612E+03 0.10957740757712E+04
 0.40150594125410E+03 0.11379281405092E+04 0.13569611956417E+04 0.10596182841179E+04 0.20445953020979E+04
 0.10344085156932E+04 0.13564310604743E+04 0.97473412204823E+03 0.20443418874539E+04 0.11379281405092E+04
 0.13569611956417E+04 0.10596182841179E+04 0.20445953020979E+04 0.10344085156932E+04 0.13564310604743E+04
 0.97473412204823E+03 0.20443418874539E+04 0.16729563373858E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54313889672367E+03 0.12948397781402E+01
 0.12948397781402E+01 0.31225641679884E+01 0.00000000000000E+00 0.33625480236854E+03 0.33625480236854E+03
 0.33625480236854E+03 0.33625480236854E+03 0.00000000000000E+00 0.00000000000000E+00 0.14654815449384E+00
 0.00000000000000E+00 -.11604886310945E+02 0.10000000000000E-02 0.15703163111350E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50945149988399E+01 0.19104431245650E+01 0.33617553131986E+03 0.42515276869677E+03
 0.30627881895650E+03 0.30627881895650E+03 0.29215003321345E+03 0.29215003246106E+03 0.30627579898973E+03
 0.30627579898973E+03 0.29215003322089E+03 0.29215003246834E+03 0.30627881895650E+03 0.30627881895650E+03
 0.29215003321345E+03 0.29215003246106E+03 0.30627579898973E+03 0.30627579898973E+03 0.29215003322089E+03
 0.29215003246834E+03 0.30422697716064E+03 0.29215016516406E+03 -.12946605947934E+02 -.12919500573585E+02
 0.20843430153209E+03 0.49302803228005E+03 0.28355155924029E+03 0.19558539158229E+03 0.19450372755033E+03
 0.19558539158229E+03 0.39038867676770E+03 0.19559836798240E+03 0.19436942391125E+03 0.19559836798240E+03
 0.39027824428160E+03 0.19558539158229E+03 0.19450372755033E+03 0.19558539158229E+03 0.39038867676770E+03
 0.19559836798240E+03 0.19436942391125E+03 0.19559836798240E+03 0.39027824428160E+03 0.23232939874153E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36377098039295E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16969369283588E+00 0.00000000000000E+00 0.00000000000000E+00 0.16969369283588E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17823464858292E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17823464858292E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256305297850E+00 0.18064286714258E+00 0.33625480236854E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    830.68907735
 0.99170682729039E+00 0.31247319620248E+03 0.46875513948994E+03 0.41709318277647E+03 0.40225014119022E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13673447034432E+00 0.00000000000000E+00 -.18374335436117E+02
 0.93479064731336E-03 0.12592586452460E+01 0.80000000000000E+04 0.30000000000000E+04 0.63529442741584E+01
 0.23823541028094E+01 0.33396641070437E+03 0.29215069777445E+03 0.33295673776383E+03 0.37610026427013E+03
 0.29215011867278E+03 0.29215029876639E+03 0.32910252717829E+03 0.37607001785788E+03 0.29215009986135E+03
 0.29215029810694E+03 0.33295673776383E+03 0.37610026427013E+03 0.29215011867278E+03 0.29215029876639E+03
 0.32910252717829E+03 0.37607001785788E+03 0.29215009986135E+03 0.29215029810694E+03 0.42570247360414E+03
 0.33677976055612E+03 0.17114132987450E+04 0.16080833566044E+04 0.68906510859498E+03 0.10914892695440E+04
 0.39897883540607E+03 0.11399730604260E+04 0.13583228312789E+04 0.10611113819380E+04 0.20440226613019E+04
 0.10366053095802E+04 0.13577951647533E+04 0.97648155599186E+03 0.20437706268349E+04 0.11399730604260E+04
 0.13583228312789E+04 0.10611113819380E+04 0.20440226613019E+04 0.10366053095802E+04 0.13577951647533E+04
 0.97648155599185E+03 0.20437706268349E+04 0.16726043309610E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54346195373934E+03 0.12948398703670E+01
 0.12948398703670E+01 0.31600040540667E+01 0.00000000000000E+00 0.33645766276664E+03 0.33645766276664E+03
 0.33645766276664E+03 0.33645766276664E+03 0.00000000000000E+00 0.00000000000000E+00 0.14612614779341E+00
 0.00000000000000E+00 -.11599380684244E+02 0.10000000000000E-02 0.15773267273321E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50718724671149E+01 0.19019521751681E+01 0.33678212765109E+03 0.42569940115810E+03
 0.30640875401627E+03 0.30640875401627E+03 0.29215003708550E+03 0.29215003624540E+03 0.30640568779536E+03
 0.30640568779536E+03 0.29215003709374E+03 0.29215003625345E+03 0.30640875401627E+03 0.30640875401627E+03
 0.29215003708550E+03 0.29215003624540E+03 0.30640568779536E+03 0.30640568779536E+03 0.29215003709374E+03
 0.29215003625345E+03 0.30434014007457E+03 0.29215018211573E+03 -.15985842657672E+02 -.16356415328458E+02
 0.20951552285551E+03 0.49500949156280E+03 0.28444639109302E+03 0.19786810726928E+03 0.19545621286964E+03
 0.19786810726928E+03 0.39187345948366E+03 0.19788132256903E+03 0.19532084764873E+03 0.19788132256903E+03
 0.39176233886383E+03 0.19786810726928E+03 0.19545621286964E+03 0.19786810726928E+03 0.39187345948366E+03
 0.19788132256903E+03 0.19532084764872E+03 0.19788132256903E+03 0.39176233886383E+03 0.23242544166696E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36399990805189E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16966028479382E+00 0.00000000000000E+00 0.00000000000000E+00 0.16966028479382E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17816253927035E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17816253927035E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256280605915E+00 0.18053370363897E+00 0.33645766276664E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    840.04904887
 0.99163552586407E+00 0.31266448565051E+03 0.46907960163950E+03 0.41737733970722E+03 0.40251898217589E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13659645727516E+00 0.00000000000000E+00 -.18377685384575E+02
 0.93421870756946E-03 0.12602500170811E+01 0.80000000000000E+04 0.30000000000000E+04 0.63479467499070E+01
 0.23804800312151E+01 0.33428624580676E+03 0.29215076741173E+03 0.33326329705671E+03 0.37661007996184E+03
 0.29215013211450E+03 0.29215033218444E+03 0.32939350504371E+03 0.37657992222148E+03 0.29215011125581E+03
 0.29215033145824E+03 0.33326329705671E+03 0.37661007996184E+03 0.29215013211450E+03 0.29215033218444E+03
 0.32939350504371E+03 0.37657992222148E+03 0.29215011125581E+03 0.29215033145824E+03 0.42624429662393E+03
 0.33737878533652E+03 0.17145696311860E+04 0.16104662993033E+04 0.68732773770048E+03 0.10872551044596E+04
 0.39649072807058E+03 0.11420056449048E+04 0.13596667009525E+04 0.10625942297397E+04 0.20434508907029E+04
 0.10387889915886E+04 0.13591414276412E+04 0.97821691978524E+03 0.20432001849728E+04 0.11420056449048E+04
 0.13596667009525E+04 0.10625942297397E+04 0.20434508907029E+04 0.10387889915886E+04 0.13591414276412E+04
 0.97821691978523E+03 0.20432001849728E+04 0.16722592054378E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54378379499018E+03 0.12948398950490E+01
 0.12948398950490E+01 0.31974439401451E+01 0.00000000000000E+00 0.33666152415073E+03 0.33666152415073E+03
 0.33666152415073E+03 0.33666152415073E+03 0.00000000000000E+00 0.00000000000000E+00 0.14571631443643E+00
 0.00000000000000E+00 -.11583448276387E+02 0.10000000000000E-02 0.15840944878435E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50502037986957E+01 0.18938264245109E+01 0.33738123284909E+03 0.42624112257396E+03
 0.30653873674830E+03 0.30653873674830E+03 0.29215004133031E+03 0.29215004039405E+03 0.30653562426279E+03
 0.30653562426279E+03 0.29215004133941E+03 0.29215004040294E+03 0.30653873674830E+03 0.30653873674830E+03
 0.29215004133031E+03 0.29215004039405E+03 0.30653562426279E+03 0.30653562426279E+03 0.29215004133941E+03
 0.29215004040294E+03 0.30445338456196E+03 0.29215020046537E+03 -.18987860788816E+02 -.20059169470984E+02
 0.21059909533386E+03 0.49699764709417E+03 0.28534555628364E+03 0.20013687661747E+03 0.19641115946106E+03
 0.20013687661747E+03 0.39336317467330E+03 0.20015033193494E+03 0.19627475055878E+03 0.20015033193494E+03
 0.39325138375773E+03 0.20013687661747E+03 0.19641115946106E+03 0.20013687661747E+03 0.39336317467330E+03
 0.20015033193494E+03 0.19627475055877E+03 0.20015033193494E+03 0.39325138375773E+03 0.23246077212169E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36422619726044E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16954615291071E+00 0.00000000000000E+00 0.00000000000000E+00 0.16954615291071E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17803928658723E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17803928658723E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256285909854E+00 0.18042446743977E+00 0.33666152415073E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    852.52901090
 0.99149559044188E+00 0.31291953319295E+03 0.46951021846363E+03 0.41775722714691E+03 0.40287904494177E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13641840145404E+00 0.00000000000000E+00 -.18365245478808E+02
 0.93345737984056E-03 0.12614948886600E+01 0.80000000000000E+04 0.30000000000000E+04 0.63416824530281E+01
 0.23781309198855E+01 0.33471075140551E+03 0.29215086897832E+03 0.33367015256779E+03 0.37728476513781E+03
 0.29215015200193E+03 0.29215038154526E+03 0.32977987650698E+03 0.37725472235707E+03 0.29215012812917E+03
 0.29215038072170E+03 0.33367015256779E+03 0.37728476513781E+03 0.29215015200193E+03 0.29215038154526E+03
 0.32977987650698E+03 0.37725472235707E+03 0.29215012812917E+03 0.29215038072170E+03 0.42696016523254E+03
 0.33816630108050E+03 0.17187827306040E+04 0.16136591777613E+04 0.68503426285499E+03 0.10816920107145E+04
 0.39323257654527E+03 0.11447123914248E+04 0.13614482830003E+04 0.10645763138302E+04 0.20427085287010E+04
 0.10416955884537E+04 0.13609261240785E+04 0.98053227192525E+03 0.20424595526324E+04 0.11447123914248E+04
 0.13614482830003E+04 0.10645763138302E+04 0.20427085287010E+04 0.10416955884537E+04 0.13609261240785E+04
 0.98053227192524E+03 0.20424595526324E+04 0.16718402975534E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54421746875116E+03 0.12948398033932E+01
 0.12948398033932E+01 0.32473637882495E+01 0.00000000000000E+00 0.33693312065565E+03 0.33693312065565E+03
 0.33693312065565E+03 0.33693312065565E+03 0.00000000000000E+00 0.00000000000000E+00 0.14518846438005E+00
 0.00000000000000E+00 -.11542560326805E+02 0.10000000000000E-02 0.15927562805634E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50227395726671E+01 0.18835273397502E+01 0.33816887482187E+03 0.42695685502630E+03
 0.30671199865870E+03 0.30671199865870E+03 0.29215004761855E+03 0.29215004653984E+03 0.30670882455311E+03
 0.30670882455311E+03 0.29215004762889E+03 0.29215004654995E+03 0.30671199865870E+03 0.30671199865870E+03
 0.29215004761855E+03 0.29215004653984E+03 0.30670882455311E+03 0.30670882455311E+03 0.29215004762889E+03
 0.29215004654995E+03 0.30460439519233E+03 0.29215022725465E+03 -.22947398658918E+02 -.25148203923021E+02
 0.21203601519418E+03 0.49962827527878E+03 0.28653208000863E+03 0.20313520485555E+03 0.19767672358847E+03
 0.20313520485555E+03 0.39533100323783E+03 0.20314898155231E+03 0.19753894596444E+03 0.20314898155231E+03
 0.39521834019885E+03 0.20313520485555E+03 0.19767672358847E+03 0.20313520485555E+03 0.39533100323783E+03
 0.20314898155231E+03 0.19753894596444E+03 0.20314898155231E+03 0.39521834019885E+03 0.23246099853205E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36452093927194E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16924270259479E+00 0.00000000000000E+00 0.00000000000000E+00 0.16924270259479E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17777310900243E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17777310900243E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256349900845E+00 0.18027977141219E+00 0.33693312065565E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    860.46284602
 0.99140402421961E+00 0.31308154845556E+03 0.46978393526612E+03 0.41799880963652E+03 0.40310801818621E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13630804674198E+00 0.00000000000000E+00 -.18378464817203E+02
 0.93297420713829E-03 0.12622466489375E+01 0.80000000000000E+04 0.30000000000000E+04 0.63379055169084E+01
 0.23767145688406E+01 0.33498029558731E+03 0.29215093856484E+03 0.33392842880507E+03 0.37771034667078E+03
 0.29215016578999E+03 0.29215041571977E+03 0.33002541509179E+03 0.37768037570832E+03 0.29215013983613E+03
 0.29215041482952E+03 0.33392842880507E+03 0.37771034667078E+03 0.29215016578999E+03 0.29215041571977E+03
 0.33002541509179E+03 0.37768037570832E+03 0.29215013983613E+03 0.29215041482952E+03 0.42740703760801E+03
 0.33865639046630E+03 0.17214600806661E+04 0.16156884098360E+04 0.68368101250716E+03 0.10783537667342E+04
 0.39125434916446E+03 0.11464330333184E+04 0.13625991311395E+04 0.10658369656302E+04 0.20422869028816E+04
 0.10435416470721E+04 0.13620789390432E+04 0.98200179111042E+03 0.20420390290143E+04 0.11464330333184E+04
 0.13625991311395E+04 0.10658369656302E+04 0.20422869028816E+04 0.10435416470721E+04 0.13620789390432E+04
 0.98200179111042E+03 0.20420390290143E+04 0.16716419920304E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54449350956582E+03 0.12948399007918E+01
 0.12948399007918E+01 0.32790991287639E+01 0.00000000000000E+00 0.33710722483307E+03 0.33710722483307E+03
 0.33710722483307E+03 0.33710722483307E+03 0.00000000000000E+00 0.00000000000000E+00 0.14486350490353E+00
 0.00000000000000E+00 -.11541020737202E+02 0.10000000000000E-02 0.15980484705733E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50061059769544E+01 0.18772897413579E+01 0.33865905376617E+03 0.42740363534962E+03
 0.30682287845000E+03 0.30682287845000E+03 0.29215005111378E+03 0.29215005080518E+03 0.30681966507274E+03
 0.30681966507274E+03 0.29215005112479E+03 0.29215005081612E+03 0.30682287845000E+03 0.30682287845000E+03
 0.29215005111378E+03 0.29215005080518E+03 0.30681966507274E+03 0.30681966507274E+03 0.29215005112479E+03
 0.29215005081612E+03 0.30470111161903E+03 0.29215024562372E+03 -.25394836138223E+02 -.28374948014051E+02
 0.21295166962750E+03 0.50132231548427E+03 0.28730588750863E+03 0.20501342677882E+03 0.19848365966866E+03
 0.20501342677882E+03 0.39660133651739E+03 0.20502740983261E+03 0.19834505742379E+03 0.20502740983261E+03
 0.39648816682982E+03 0.20501342677881E+03 0.19848365966865E+03 0.20501342677881E+03 0.39660133651738E+03
 0.20502740983261E+03 0.19834505742378E+03 0.20502740983261E+03 0.39648816682982E+03 0.23247418878556E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36471760122976E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16923789527632E+00 0.00000000000000E+00 0.00000000000000E+00 0.16923789527632E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17773101005289E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17773101005289E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256319635815E+00 0.18018634988844E+00 0.33710722483307E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    871.20388432
 0.99129229991904E+00 0.31329983906180E+03 0.47015157171844E+03 0.41832293344794E+03 0.40341529324207E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13616219934981E+00 0.00000000000000E+00 -.18391191087789E+02
 0.93232404338623E-03 0.12632230688594E+01 0.80000000000000E+04 0.30000000000000E+04 0.63330065743838E+01
 0.23748774653939E+01 0.33534260169785E+03 0.29215104025341E+03 0.33427564665892E+03 0.37828407630781E+03
 0.29215018618592E+03 0.29215046619909E+03 0.33035537270353E+03 0.37825419955649E+03 0.29215015716666E+03
 0.29215046521141E+03 0.33427564665892E+03 0.37828407630781E+03 0.29215018618592E+03 0.29215046619909E+03
 0.33035537270353E+03 0.37825419955649E+03 0.29215015716666E+03 0.29215046521141E+03 0.42801678778362E+03
 0.33932069910686E+03 0.17250544585571E+04 0.16184141141536E+04 0.68168677080104E+03 0.10735926256290E+04
 0.38849742097397E+03 0.11487448987324E+04 0.13641110817767E+04 0.10675323858709E+04 0.20416710247489E+04
 0.10460242149742E+04 0.13635934062002E+04 0.98398062151990E+03 0.20414245268692E+04 0.11487448987324E+04
 0.13641110817767E+04 0.10675323858709E+04 0.20416710247489E+04 0.10460242149742E+04 0.13635934062002E+04
 0.98398062151989E+03 0.20414245268692E+04 0.16712874570157E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54486276462090E+03 0.12948399945575E+01
 0.12948399945575E+01 0.33220632819589E+01 0.00000000000000E+00 0.33734460562900E+03 0.33734460562900E+03
 0.33734460562900E+03 0.33734460562900E+03 0.00000000000000E+00 0.00000000000000E+00 0.14443617860196E+00
 0.00000000000000E+00 -.11532259670853E+02 0.10000000000000E-02 0.16049591822367E+01 0.80000000000000E+04
 0.30000000000000E+04 0.49845504412462E+01 0.18692064154673E+01 0.33932346833487E+03 0.42801328107481E+03
 0.30697180555480E+03 0.30697180555480E+03 0.29215005847864E+03 0.29215005712143E+03 0.30696853944381E+03
 0.30696853944381E+03 0.29215005849109E+03 0.29215005713359E+03 0.30697180555480E+03 0.30697180555480E+03
 0.29215005847864E+03 0.29215005712143E+03 0.30696853944381E+03 0.30696853944381E+03 0.29215005849109E+03
 0.29215005713359E+03 0.30483097093960E+03 0.29215027248924E+03 -.28733254892318E+02 -.32851201666044E+02
 0.21420785948935E+03 0.50365463974158E+03 0.28837574095479E+03 0.20758157690500E+03 0.19959205826244E+03
 0.20758157690500E+03 0.39835311802040E+03 0.20759583675818E+03 0.19945227686100E+03 0.20759583675818E+03
 0.39823919632056E+03 0.20758157690500E+03 0.19959205826244E+03 0.20758157690500E+03 0.39835311802039E+03
 0.20759583675818E+03 0.19945227686100E+03 0.20759583675818E+03 0.39823919632056E+03 0.23248508994027E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36498076813344E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16918701564950E+00 0.00000000000000E+00 0.00000000000000E+00 0.16918701564950E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17760968219518E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17760968219518E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256297690506E+00 0.18005934393886E+00 0.33734460562900E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    880.95612569
 0.99121099836071E+00 0.31349648058259E+03 0.47048267986522E+03 0.41861386409196E+03 0.40369095199769E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13603359648730E+00 0.00000000000000E+00 -.18404904006613E+02
 0.93173911447144E-03 0.12640640755008E+01 0.80000000000000E+04 0.30000000000000E+04 0.63287931008009E+01
 0.23732974128003E+01 0.33567016662548E+03 0.29215114060175E+03 0.33458956394228E+03 0.37880127462459E+03
 0.29215020658341E+03 0.29215051660134E+03 0.33065384344296E+03 0.37877148118636E+03 0.29215017451282E+03
 0.29215051551757E+03 0.33458956394228E+03 0.37880127462459E+03 0.29215020658341E+03 0.29215051660134E+03
 0.33065384344297E+03 0.37877148118636E+03 0.29215017451282E+03 0.29215051551757E+03 0.42856442217792E+03
 0.33991603453066E+03 0.17282746521027E+04 0.16208432543970E+04 0.67988785690548E+03 0.10693219174513E+04
 0.38603462126127E+03 0.11508205684590E+04 0.13654514971811E+04 0.10690465390990E+04 0.20410946439727E+04
 0.10482543451430E+04 0.13649360476577E+04 0.98575065009082E+03 0.20408493606681E+04 0.11508205684590E+04
 0.13654514971811E+04 0.10690465390989E+04 0.20410946439727E+04 0.10482543451430E+04 0.13649360476577E+04
 0.98575065009080E+03 0.20408493606681E+04 0.16709509512333E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54519262030086E+03 0.12948400955928E+01
 0.12948400955928E+01 0.33610722474074E+01 0.00000000000000E+00 0.33756249740164E+03 0.33756249740164E+03
 0.33756249740164E+03 0.33756249740164E+03 0.00000000000000E+00 0.00000000000000E+00 0.14406029547665E+00
 0.00000000000000E+00 -.11527696257950E+02 0.10000000000000E-02 0.16109862406905E+01 0.80000000000000E+04
 0.30000000000000E+04 0.49659021274886E+01 0.18622132978082E+01 0.33991886550699E+03 0.42856085065996E+03
 0.30710727740320E+03 0.30710727740320E+03 0.29215006495306E+03 0.29215006344558E+03 0.30710396335066E+03
 0.30710396335066E+03 0.29215006496673E+03 0.29215006345894E+03 0.30710727740320E+03 0.30710727740320E+03
 0.29215006495306E+03 0.29215006344558E+03 0.30710396335066E+03 0.30710396335066E+03 0.29215006496673E+03
 0.29215006345894E+03 0.30494912244841E+03 0.29215029902482E+03 -.31712851822976E+02 -.36906819877284E+02
 0.21536113226888E+03 0.50581020245713E+03 0.28937226452691E+03 0.20990301508042E+03 0.20061098653080E+03
 0.20990301508042E+03 0.39997569545768E+03 0.20991752772658E+03 0.20047015221180E+03 0.20991752772658E+03
 0.39986111010604E+03 0.20990301508042E+03 0.20061098653080E+03 0.20990301508042E+03 0.39997569545768E+03
 0.20991752772658E+03 0.20047015221180E+03 0.20991752772658E+03 0.39986111010604E+03 0.23251577021427E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36522382561752E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16916206398759E+00 0.00000000000000E+00 0.00000000000000E+00 0.16916206398759E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17753864784000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17753864784000E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256267548396E+00 0.17994281199879E+00 0.33756249740164E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    890.75888578
 0.99109814687423E+00 0.31369484847123E+03 0.47081528199913E+03 0.41890802516394E+03 0.40397012599495E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13590766100445E+00 0.00000000000000E+00 -.18397863754662E+02
 0.93114998518653E-03 0.12648642630698E+01 0.80000000000000E+04 0.30000000000000E+04 0.63247893339828E+01
 0.23717960002436E+01 0.33599922197017E+03 0.29215124813028E+03 0.33490484626051E+03 0.37931735521506E+03
 0.29215022866532E+03 0.29215057109798E+03 0.33095393892025E+03 0.37928764436305E+03 0.29215019330333E+03
 0.29215056991129E+03 0.33490484626051E+03 0.37931735521506E+03 0.29215022866532E+03 0.29215057109798E+03
 0.33095393892026E+03 0.37928764436305E+03 0.29215019330333E+03 0.29215056991129E+03 0.42910494022097E+03
 0.34050214101479E+03 0.17315232127828E+04 0.16233018059549E+04 0.67820855439827E+03 0.10652700485271E+04
 0.38367045135685E+03 0.11529105977168E+04 0.13668180195833E+04 0.10705763840364E+04 0.20405675339506E+04
 0.10504974751778E+04 0.13663048169893E+04 0.98753369154070E+03 0.20403234953147E+04 0.11529105977168E+04
 0.13668180195833E+04 0.10705763840364E+04 0.20405675339506E+04 0.10504974751778E+04 0.13663048169893E+04
 0.98753369154069E+03 0.20403234953147E+04 0.16707029825482E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54552848169333E+03 0.12948400437210E+01
 0.12948400437210E+01 0.34002832878009E+01 0.00000000000000E+00 0.33778102289234E+03 0.33778102289234E+03
 0.33778102289234E+03 0.33778102289234E+03 0.00000000000000E+00 0.00000000000000E+00 0.14369389871536E+00
 0.00000000000000E+00 -.11499078941244E+02 0.10000000000000E-02 0.16168244956927E+01 0.80000000000000E+04
 0.30000000000000E+04 0.49479705566760E+01 0.18554889587535E+01 0.34050507865418E+03 0.42910125684739E+03
 0.30724455953211E+03 0.30724455953211E+03 0.29215007123188E+03 0.29215007029814E+03 0.30724119719772E+03
 0.30724119719772E+03 0.29215007124671E+03 0.29215007031278E+03 0.30724455953211E+03 0.30724455953211E+03
 0.29215007123188E+03 0.29215007029814E+03 0.30724119719772E+03 0.30724119719772E+03 0.29215007124671E+03
 0.29215007031278E+03 0.30506894732746E+03 0.29215032747897E+03 -.34639000371415E+02 -.40936677034379E+02
 0.21650680178836E+03 0.50793529282837E+03 0.29034595703106E+03 0.21219314433645E+03 0.20162158659088E+03
 0.21219314433645E+03 0.40156951010233E+03 0.21220791367318E+03 0.20147974555642E+03 0.21220791367318E+03
 0.40145430874852E+03 0.21219314433645E+03 0.20162158659088E+03 0.21219314433645E+03 0.40156951010233E+03
 0.21220791367318E+03 0.20147974555642E+03 0.21220791367318E+03 0.40145430874852E+03 0.23253669665880E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36546012755473E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16895033690768E+00 0.00000000000000E+00 0.00000000000000E+00 0.16895033690768E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17734860414448E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17734860414448E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256306883439E+00 0.17982685975132E+00 0.33778102289234E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    901.44163250
 0.99099112580780E+00 0.31390959861353E+03 0.47117455490835E+03 0.41922516287878E+03 0.40427109676381E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13577380425939E+00 0.00000000000000E+00 -.18409295539708E+02
 0.93051286687626E-03 0.12656978358431E+01 0.80000000000000E+04 0.30000000000000E+04 0.63206239067884E+01
 0.23702339650457E+01 0.33635538624190E+03 0.29215137515876E+03 0.33524614088144E+03 0.37987680380711E+03
 0.29215025509166E+03 0.29215063621278E+03 0.33127873102028E+03 0.37984718016477E+03 0.29215021580872E+03
 0.29215063490461E+03 0.33524614088144E+03 0.37987680380711E+03 0.29215025509166E+03 0.29215063621278E+03
 0.33127873102028E+03 0.37984718016477E+03 0.29215021580872E+03 0.29215063490461E+03 0.42969576086338E+03
 0.34113987859168E+03 0.17350256735428E+04 0.16259484699198E+04 0.67625721471877E+03 0.10606836764761E+04
 0.38104517568375E+03 0.11551669318638E+04 0.13682604492335E+04 0.10722258523007E+04 0.20399503049292E+04
 0.10529212540288E+04 0.13677495780056E+04 0.98945928050354E+03 0.20397075336589E+04 0.11551669318638E+04
 0.13682604492335E+04 0.10722258523007E+04 0.20399503049292E+04 0.10529212540288E+04 0.13677495780056E+04
 0.98945928050353E+03 0.20397075336589E+04 0.16703599357121E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54588923644761E+03 0.12948401279492E+01
 0.12948401279492E+01 0.34430142746756E+01 0.00000000000000E+00 0.33802036938752E+03 0.33802036938752E+03
 0.33802036938752E+03 0.33802036938752E+03 0.00000000000000E+00 0.00000000000000E+00 0.14330706087149E+00
 0.00000000000000E+00 -.11489774070831E+02 0.10000000000000E-02 0.16229382415844E+01 0.80000000000000E+04
 0.30000000000000E+04 0.49293311322740E+01 0.18484991746027E+01 0.34114288624810E+03 0.42969200847636E+03
 0.30739324259382E+03 0.30739324259382E+03 0.29215007998933E+03 0.29215007850825E+03 0.30738982792193E+03
 0.30738982792193E+03 0.29215008000577E+03 0.29215007852439E+03 0.30739324259382E+03 0.30739324259382E+03
 0.29215007998933E+03 0.29215007850825E+03 0.30738982792193E+03 0.30738982792193E+03 0.29215008000577E+03
 0.29215007852439E+03 0.30519869563253E+03 0.29215036112295E+03 -.37840944716260E+02 -.45390541430260E+02
 0.21776777377831E+03 0.51029438871552E+03 0.29143777606832E+03 0.21470531410362E+03 0.20273485571611E+03
 0.21470531410362E+03 0.40334385547090E+03 0.21472036184095E+03 0.20259187164826E+03 0.21472036184095E+03
 0.40322793629918E+03 0.21470531410362E+03 0.20273485571611E+03 0.21470531410362E+03 0.40334385547090E+03
 0.21472036184095E+03 0.20259187164826E+03 0.21472036184095E+03 0.40322793629918E+03 0.23256616968913E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36572441511887E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16889185557285E+00 0.00000000000000E+00 0.00000000000000E+00 0.16889185557285E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17723990985235E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17723990985235E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256286309092E+00 0.17969932965712E+00 0.33802036938752E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    910.39953410
 0.99090847496945E+00 0.31408879951934E+03 0.47147400260877E+03 0.41948922541673E+03 0.40452169428421E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13566445602779E+00 0.00000000000000E+00 -.18410783228297E+02
 0.92998195638751E-03 0.12663619252478E+01 0.80000000000000E+04 0.30000000000000E+04 0.63173093256372E+01
 0.23689909971139E+01 0.33665292373436E+03 0.29215148970660E+03 0.33553124995669E+03 0.38034284333343E+03
 0.29215027920247E+03 0.29215069553566E+03 0.33155018747000E+03 0.38031329113954E+03 0.29215023635705E+03
 0.29215069411805E+03 0.33553124995669E+03 0.38034284333343E+03 0.29215027920247E+03 0.29215069553566E+03
 0.33155018747000E+03 0.38031329113954E+03 0.29215023635705E+03 0.29215069411805E+03 0.43018647450948E+03
 0.34166833175878E+03 0.17379381016836E+04 0.16281438718872E+04 0.67463636232337E+03 0.10568886629827E+04
 0.37887911884769E+03 0.11570451726423E+04 0.13694522566240E+04 0.10735956478948E+04 0.20394303567439E+04
 0.10549392862459E+04 0.13689432990733E+04 0.99105911458935E+03 0.20391886252943E+04 0.11570451726423E+04
 0.13694522566240E+04 0.10735956478948E+04 0.20394303567439E+04 0.10549392862459E+04 0.13689432990733E+04
 0.99105911458935E+03 0.20391886252944E+04 0.16700735178552E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54618902799949E+03 0.12948401389103E+01
 0.12948401389103E+01 0.34788458810680E+01 0.00000000000000E+00 0.33822197709224E+03 0.33822197709224E+03
 0.33822197709224E+03 0.33822197709224E+03 0.00000000000000E+00 0.00000000000000E+00 0.14299235231112E+00
 0.00000000000000E+00 -.11472976133318E+02 0.10000000000000E-02 0.16278724266891E+01 0.80000000000000E+04
 0.30000000000000E+04 0.49143900153595E+01 0.18428962557598E+01 0.34167139584750E+03 0.43018266548171E+03
 0.30751814029888E+03 0.30751814029888E+03 0.29215008762925E+03 0.29215008600671E+03 0.30751468173463E+03
 0.30751468173463E+03 0.29215008764707E+03 0.29215008602420E+03 0.30751814029888E+03 0.30751814029888E+03
 0.29215008762925E+03 0.29215008600671E+03 0.30751468173463E+03 0.30751468173463E+03 0.29215008764707E+03
 0.29215008602420E+03 0.30530771496065E+03 0.29215039148572E+03 -.40491112026912E+02 -.49107419283352E+02
 0.21882866115809E+03 0.51227907159438E+03 0.29235626713050E+03 0.21679961302875E+03 0.20367165623702E+03
 0.21679961302875E+03 0.40483641744479E+03 0.21681489530426E+03 0.20352772280352E+03 0.21681489530426E+03
 0.40471990540079E+03 0.21679961302875E+03 0.20367165623702E+03 0.21679961302875E+03 0.40483641744478E+03
 0.21681489530426E+03 0.20352772280352E+03 0.21681489530426E+03 0.40471990540079E+03 0.23259814034280E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36594404242867E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16877163917712E+00 0.00000000000000E+00 0.00000000000000E+00 0.16877163917712E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17710982596356E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17710982596356E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256294937540E+00 0.17959233388195E+00 0.33822197709224E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    921.59691110
 0.99079039197953E+00 0.31431241830196E+03 0.47184646469390E+03 0.41981872483564E+03 0.40483468732987E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13553112112261E+00 0.00000000000000E+00 -.18417108708458E+02
 0.92932025911096E-03 0.12671507909487E+01 0.80000000000000E+04 0.30000000000000E+04 0.63133764798508E+01
 0.23675161799441E+01 0.33702343511141E+03 0.29215164359467E+03 0.33588626977887E+03 0.38092165234939E+03
 0.29215031197516E+03 0.29215077605196E+03 0.33188835829683E+03 0.38089218745634E+03 0.29215026430770E+03
 0.29215077448745E+03 0.33588626977887E+03 0.38092165234939E+03 0.29215031197516E+03 0.29215077605196E+03
 0.33188835829683E+03 0.38089218745634E+03 0.29215026430770E+03 0.29215077448745E+03 0.43079478854271E+03
 0.34232146721743E+03 0.17415683127345E+04 0.16308833489843E+04 0.67262698944037E+03 0.10522026056592E+04
 0.37621248127168E+03 0.11593847278712E+04 0.13709290243993E+04 0.10753038477279E+04 0.20387876840010E+04
 0.10574525916582E+04 0.13704224186614E+04 0.99305262420368E+03 0.20385472337558E+04 0.11593847278712E+04
 0.13709290243993E+04 0.10753038477279E+04 0.20387876840010E+04 0.10574525916582E+04 0.13704224186614E+04
 0.99305262420368E+03 0.20385472337558E+04 0.16697329869985E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54656417995194E+03 0.12948401855158E+01
 0.12948401855158E+01 0.35236353890586E+01 0.00000000000000E+00 0.33847347453955E+03 0.33847347453955E+03
 0.33847347453955E+03 0.33847347453955E+03 0.00000000000000E+00 0.00000000000000E+00 0.14261104746077E+00
 0.00000000000000E+00 -.11457158821908E+02 0.10000000000000E-02 0.16338076606718E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48965372072688E+01 0.18362014527258E+01 0.34232461218253E+03 0.43079090130979E+03
 0.30767437400252E+03 0.30767437400252E+03 0.29215009802458E+03 0.29215009620956E+03 0.30767086061532E+03
 0.30767086061532E+03 0.29215009804423E+03 0.29215009622885E+03 0.30767437400252E+03 0.30767437400252E+03
 0.29215009802458E+03 0.29215009620956E+03 0.30767086061532E+03 0.30767086061532E+03 0.29215009804423E+03
 0.29215009622885E+03 0.30544412718042E+03 0.29215043230897E+03 -.43776035471756E+02 -.53748273663955E+02
 0.22014817646667E+03 0.51474909160055E+03 0.29350017425155E+03 0.21939815768423E+03 0.20483581335082E+03
 0.21939815768423E+03 0.40669288703309E+03 0.21941373429200E+03 0.20469069916083E+03 0.21941373429200E+03
 0.40657563992713E+03 0.21939815768423E+03 0.20483581335082E+03 0.21939815768423E+03 0.40669288703309E+03
 0.21941373429200E+03 0.20469069916083E+03 0.21941373429200E+03 0.40657563992713E+03 0.23263399276199E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36621946686485E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16866148285194E+00 0.00000000000000E+00 0.00000000000000E+00 0.16866148285194E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17697332768969E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17697332768969E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256290914005E+00 0.17945887677760E+00 0.33847347453955E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    931.97212233
 0.99068534574008E+00 0.31451874244639E+03 0.47218953359091E+03 0.42012215284487E+03 0.40512296812556E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13541063857523E+00 0.00000000000000E+00 -.18424996872708E+02
 0.92871055315263E-03 0.12678450191988E+01 0.80000000000000E+04 0.30000000000000E+04 0.63099194924120E+01
 0.23662198096545E+01 0.33736514346797E+03 0.29215179759739E+03 0.33621368920901E+03 0.38145446206054E+03
 0.29215034518569E+03 0.29215085751411E+03 0.33220034310720E+03 0.38142507600031E+03 0.29215029265377E+03
 0.29215085580279E+03 0.33621368920901E+03 0.38145446206054E+03 0.29215034518569E+03 0.29215085751411E+03
 0.33220034310720E+03 0.38142507600031E+03 0.29215029265377E+03 0.29215085580279E+03 0.43135486055650E+03
 0.34292080602301E+03 0.17449098044534E+04 0.16334030118188E+04 0.67075382180347E+03 0.10478707730054E+04
 0.37376318209288E+03 0.11615396578972E+04 0.13722782484138E+04 0.10768763864341E+04 0.20381906414939E+04
 0.10597679674465E+04 0.13717737633326E+04 0.99488800458323E+03 0.20379513420753E+04 0.11615396578972E+04
 0.13722782484138E+04 0.10768763864341E+04 0.20381906414939E+04 0.10597679674465E+04 0.13717737633326E+04
 0.99488800458322E+03 0.20379513420753E+04 0.16694116629265E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54690925588545E+03 0.12948402436349E+01
 0.12948402436349E+01 0.35651362339761E+01 0.00000000000000E+00 0.33870684521128E+03 0.33870684521128E+03
 0.33870684521128E+03 0.33870684521128E+03 0.00000000000000E+00 0.00000000000000E+00 0.14226924611526E+00
 0.00000000000000E+00 -.11444830645281E+02 0.10000000000000E-02 0.16390818664132E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48807812251053E+01 0.18302929594145E+01 0.34292402216475E+03 0.43135090902416E+03
 0.30781903769447E+03 0.30781903769447E+03 0.29215010894638E+03 0.29215010656019E+03 0.30781547359552E+03
 0.30781547359552E+03 0.29215010896794E+03 0.29215010658127E+03 0.30781903769447E+03 0.30781903769447E+03
 0.29215010894638E+03 0.29215010656019E+03 0.30781547359552E+03 0.30781547359552E+03 0.29215010896794E+03
 0.29215010658127E+03 0.30557046103028E+03 0.29215047319781E+03 -.46797531245310E+02 -.58045529459729E+02
 0.22137223023575E+03 0.51704393075232E+03 0.29456483936539E+03 0.22179667775190E+03 0.20591558994148E+03
 0.22179667775190E+03 0.40841809084862E+03 0.22181252768758E+03 0.20576937512029E+03 0.22181252768758E+03
 0.40830015580062E+03 0.22179667775190E+03 0.20591558994148E+03 0.22179667775190E+03 0.40841809084862E+03
 0.22181252768758E+03 0.20576937512029E+03 0.22181252768758E+03 0.40830015580062E+03 0.23267089135905E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36647505678853E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16857862453723E+00 0.00000000000000E+00 0.00000000000000E+00 0.16857862453723E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17685372578365E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17685372578365E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256280454611E+00 0.17933514186543E+00 0.33870684521128E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    940.81651297
 0.99060241363532E+00 0.31469384382316E+03 0.47248035094558E+03 0.42037911934740E+03 0.40536710477691E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13531028365279E+00 0.00000000000000E+00 -.18432637530198E+02
 0.92819373170871E-03 0.12684088846953E+01 0.80000000000000E+04 0.30000000000000E+04 0.63071144459238E+01
 0.23651679172214E+01 0.33765539562045E+03 0.29215193799565E+03 0.33649179865564E+03 0.38190589625031E+03
 0.29215037579633E+03 0.29215093249319E+03 0.33246545836416E+03 0.38187657597298E+03 0.29215031879854E+03
 0.29215093064819E+03 0.33649179865564E+03 0.38190589625031E+03 0.29215037579633E+03 0.29215093249319E+03
 0.33246545836416E+03 0.38187657597298E+03 0.29215031879854E+03 0.29215093064819E+03 0.43182826092917E+03
 0.34342618393210E+03 0.17477367895917E+04 0.16355301982454E+04 0.66916842267327E+03 0.10442190257565E+04
 0.37170476096985E+03 0.11633646379970E+04 0.13734129005121E+04 0.10782054783058E+04 0.20376799461600E+04
 0.10617291832175E+04 0.13729101895780E+04 0.99643982645633E+03 0.20374416097413E+04 0.11633646379970E+04
 0.13734129005121E+04 0.10782054783058E+04 0.20376799461600E+04 0.10617291832175E+04 0.13729101895780E+04
 0.99643982645632E+03 0.20374416097413E+04 0.16691381435450E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54720094115774E+03 0.12948402999305E+01
 0.12948402999305E+01 0.36005137965581E+01 0.00000000000000E+00 0.33890619525488E+03 0.33890619525488E+03
 0.33890619525488E+03 0.33890619525488E+03 0.00000000000000E+00 0.00000000000000E+00 0.14198629409550E+00
 0.00000000000000E+00 -.11435640130931E+02 0.10000000000000E-02 0.16434124305224E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48679198546996E+01 0.18254699455124E+01 0.34342945743783E+03 0.43182425979497E+03
 0.30794249037645E+03 0.30794249037645E+03 0.29215011870983E+03 0.29215011610979E+03 0.30793888304048E+03
 0.30793888304048E+03 0.29215011873305E+03 0.29215011613250E+03 0.30794249037645E+03 0.30794249037645E+03
 0.29215011870983E+03 0.29215011610979E+03 0.30793888304048E+03 0.30793888304048E+03 0.29215011873305E+03
 0.29215011613250E+03 0.30567829720784E+03 0.29215051050274E+03 -.49345221639920E+02 -.61687596851095E+02
 0.22241678354362E+03 0.51900501854250E+03 0.29547615108116E+03 0.22382961732293E+03 0.20683693370219E+03
 0.22382961732293E+03 0.40989272345894E+03 0.22384570130851E+03 0.20668978569980E+03 0.22384570130851E+03
 0.40977420744193E+03 0.22382961732293E+03 0.20683693370219E+03 0.22382961732293E+03 0.40989272345894E+03
 0.22384570130851E+03 0.20668978569980E+03 0.22384570130851E+03 0.40977420744193E+03 0.23270977632158E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36669373878799E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16851718990875E+00 0.00000000000000E+00 0.00000000000000E+00 0.16851718990875E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17676298358182E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17676298358182E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256267694554E+00 0.17922953780113E+00 0.33890619525488E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    952.60903383
 0.99050003561484E+00 0.31492631721059E+03 0.47286578057366E+03 0.42071943254496E+03 0.40569046108049E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13517992655097E+00 0.00000000000000E+00 -.18442978833536E+02
 0.92750845980144E-03 0.12691196780265E+01 0.80000000000000E+04 0.30000000000000E+04 0.63035820328940E+01
 0.23638432623352E+01 0.33804092336021E+03 0.29215213872980E+03 0.33686119474197E+03 0.38250393595849E+03
 0.29215042006707E+03 0.29215104077022E+03 0.33281775462686E+03 0.38247470144362E+03 0.29215035663749E+03
 0.29215103873436E+03 0.33686119474197E+03 0.38250393595849E+03 0.29215042006707E+03 0.29215104077022E+03
 0.33281775462686E+03 0.38247470144362E+03 0.29215035663749E+03 0.29215103873436E+03 0.43245404068841E+03
 0.34409247073539E+03 0.17514760360891E+04 0.16383382442288E+04 0.66706580085183E+03 0.10393997341328E+04
 0.36899860427672E+03 0.11657809654421E+04 0.13749030823981E+04 0.10799620043690E+04 0.20369952391036E+04
 0.10643264273117E+04 0.13744026843144E+04 0.99849145711043E+03 0.20367581554150E+04 0.11657809654421E+04
 0.13749030823981E+04 0.10799620043690E+04 0.20369952391036E+04 0.10643264273116E+04 0.13744026843144E+04
 0.99849145711043E+03 0.20367581554150E+04 0.16687721276811E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54758650924920E+03 0.12948403761241E+01
 0.12948403761241E+01 0.36476838800007E+01 0.00000000000000E+00 0.33917247613639E+03 0.33917247613639E+03
 0.33917247613639E+03 0.33917247613639E+03 0.00000000000000E+00 0.00000000000000E+00 0.14162065182064E+00
 0.00000000000000E+00 -.11423765927804E+02 0.10000000000000E-02 0.16489586144512E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48515468671496E+01 0.18193300751811E+01 0.34409582415408E+03 0.43244997265945E+03
 0.30810728266138E+03 0.30810728266138E+03 0.29215013284468E+03 0.29215012993505E+03 0.30810361769700E+03
 0.30810361769700E+03 0.29215013287024E+03 0.29215012996005E+03 0.30810728266138E+03 0.30810728266138E+03
 0.29215013284468E+03 0.29215012993505E+03 0.30810361769700E+03 0.30810361769700E+03 0.29215013287024E+03
 0.29215012996005E+03 0.30582228124236E+03 0.29215056388171E+03 -.52704933746089E+02 -.66513414638530E+02
 0.22381059719050E+03 0.52162382596022E+03 0.29669417578377E+03 0.22652411207281E+03 0.20806610143584E+03
 0.22652411207281E+03 0.41186199171140E+03 0.22654050942613E+03 0.20791771271042E+03 0.22654050942613E+03
 0.41174270476792E+03 0.22652411207281E+03 0.20806610143584E+03 0.22652411207281E+03 0.41186199171140E+03
 0.22654050942613E+03 0.20791771271042E+03 0.22654050942613E+03 0.41174270476792E+03 0.23277078179741E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36698553795917E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16843801322708E+00 0.00000000000000E+00 0.00000000000000E+00 0.16843801322708E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17664500112775E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17664500112775E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256249560214E+00 0.17908865929868E+00 0.33917247613639E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    961.45342448
 0.99042845503497E+00 0.31509993730428E+03 0.47315300792626E+03 0.42097292174304E+03 0.40593137006802E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13508477396376E+00 0.00000000000000E+00 -.18451104177875E+02
 0.92699732820667E-03 0.12696219615000E+01 0.80000000000000E+04 0.30000000000000E+04 0.63010882314513E+01
 0.23629080867943E+01 0.33832895009947E+03 0.29215229992916E+03 0.33713717005174E+03 0.38294961622125E+03
 0.29215045601993E+03 0.29215112857395E+03 0.33308106752967E+03 0.38292044459655E+03 0.29215038738844E+03
 0.29215112638507E+03 0.33713717005174E+03 0.38294961622125E+03 0.29215045601993E+03 0.29215112857395E+03
 0.33308106752967E+03 0.38292044459655E+03 0.29215038738844E+03 0.29215112638507E+03 0.43291950179732E+03
 0.34458681061080E+03 0.17542571312844E+04 0.16404225080796E+04 0.66549261972330E+03 0.10358147133034E+04
 0.36699463048150E+03 0.11675798205483E+04 0.13760013260204E+04 0.10812671554296E+04 0.20364750645297E+04
 0.10662604774042E+04 0.13755026223753E+04 0.10000166153296E+04 0.20362388958159E+04 0.11675798205483E+04
 0.13760013260204E+04 0.10812671554296E+04 0.20364750645297E+04 0.10662604774042E+04 0.13755026223753E+04
 0.10000166153296E+04 0.20362388958159E+04 0.16684931575340E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54787321946143E+03 0.12948404359908E+01
 0.12948404359908E+01 0.36830614425827E+01 0.00000000000000E+00 0.33937240771562E+03 0.33937240771562E+03
 0.33937240771562E+03 0.33937240771562E+03 0.00000000000000E+00 0.00000000000000E+00 0.14135485532941E+00
 0.00000000000000E+00 -.11415454950881E+02 0.10000000000000E-02 0.16529536612852E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48398210956379E+01 0.18149329108642E+01 0.34459022190251E+03 0.43291538753618E+03
 0.30823098837564E+03 0.30823098837564E+03 0.29215014433525E+03 0.29215014117395E+03 0.30822728020502E+03
 0.30822728020502E+03 0.29215014436268E+03 0.29215014120078E+03 0.30823098837564E+03 0.30823098837564E+03
 0.29215014433525E+03 0.29215014117395E+03 0.30822728020502E+03 0.30822728020502E+03 0.29215014436268E+03
 0.29215014120078E+03 0.30593039338011E+03 0.29215060678105E+03 -.55199430218836E+02 -.70111443728267E+02
 0.22485611905836E+03 0.52358937366542E+03 0.29760897401176E+03 0.22853339846236E+03 0.20898785479040E+03
 0.22853339846236E+03 0.41333998261022E+03 0.22855003180502E+03 0.20883853658267E+03 0.22855003180502E+03
 0.41322011867809E+03 0.22853339846236E+03 0.20898785479040E+03 0.22853339846236E+03 0.41333998261022E+03
 0.22855003180502E+03 0.20883853658267E+03 0.22855003180502E+03 0.41322011867809E+03 0.23282231429613E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36720455501166E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16838294626004E+00 0.00000000000000E+00 0.00000000000000E+00 0.16838294626004E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17656094806310E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17656094806310E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256234250639E+00 0.17898301021593E+00 0.33937240771562E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    970.29781512
 0.99034090709463E+00 0.31527348400885E+03 0.47343890121142E+03 0.42122634029696E+03 0.40617251705815E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13499163492123E+00 0.00000000000000E+00 -.18448126988209E+02
 0.92648707677261E-03 0.12701004228538E+01 0.80000000000000E+04 0.30000000000000E+04 0.62987145394574E+01
 0.23620179522965E+01 0.33861605034291E+03 0.29215247069228E+03 0.33741225150473E+03 0.38339289958951E+03
 0.29215049446978E+03 0.29215122235738E+03 0.33334362182916E+03 0.38336378969598E+03 0.29215042029456E+03
 0.29215122000662E+03 0.33741225150473E+03 0.38339289958951E+03 0.29215049446978E+03 0.29215122235738E+03
 0.33334362182916E+03 0.38336378969598E+03 0.29215042029456E+03 0.29215122000662E+03 0.43338174217068E+03
 0.34507658646935E+03 0.17570338055742E+04 0.16425072387748E+04 0.66392821218972E+03 0.10322628913455E+04
 0.36501503809478E+03 0.11693737262445E+04 0.13770891309629E+04 0.10825710230040E+04 0.20359557180056E+04
 0.10681889291127E+04 0.13765921044623E+04 0.10015389944229E+04 0.20357204593742E+04 0.11693737262445E+04
 0.13770891309629E+04 0.10825710230040E+04 0.20359557180056E+04 0.10681889291127E+04 0.13765921044623E+04
 0.10015389944229E+04 0.20357204593741E+04 0.16682253195137E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54816100584498E+03 0.12948404140552E+01
 0.12948404140552E+01 0.37184390051646E+01 0.00000000000000E+00 0.33957112540937E+03 0.33957112540937E+03
 0.33957112540937E+03 0.33957112540937E+03 0.00000000000000E+00 0.00000000000000E+00 0.14109613973979E+00
 0.00000000000000E+00 -.11394390954153E+02 0.10000000000000E-02 0.16568177595983E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48285334664325E+01 0.18107000499122E+01 0.34508005314390E+03 0.43337758572358E+03
 0.30835471920535E+03 0.30835471920535E+03 0.29215015663430E+03 0.29215015320362E+03 0.30835096783933E+03
 0.30835096783933E+03 0.29215015666370E+03 0.29215015323238E+03 0.30835471920535E+03 0.30835471920535E+03
 0.29215015663430E+03 0.29215015320362E+03 0.30835096783933E+03 0.30835096783933E+03 0.29215015666370E+03
 0.29215015323238E+03 0.30603855549002E+03 0.29215065225567E+03 -.57682450923354E+02 -.73704913784648E+02
 0.22589175234586E+03 0.52552554469851E+03 0.29850433359092E+03 0.23052742646081E+03 0.20989954492627E+03
 0.23052742646081E+03 0.41479234515781E+03 0.23054429674596E+03 0.20974929919935E+03 0.23054429674596E+03
 0.41467190532905E+03 0.23052742646081E+03 0.20989954492627E+03 0.23052742646081E+03 0.41479234515781E+03
 0.23054429674596E+03 0.20974929919935E+03 0.23054429674596E+03 0.41467190532905E+03 0.23286020218722E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36741821219456E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16822840725770E+00 0.00000000000000E+00 0.00000000000000E+00 0.16822840725770E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17641372148663E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17641372148663E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256256098654E+00 0.17887853268484E+00 0.33957112540937E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    982.09033599
 0.99022418524560E+00 0.31550419835521E+03 0.47381827408507E+03 0.42156279853423E+03 0.40649278683407E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13487010206731E+00 0.00000000000000E+00 -.18455799604195E+02
 0.92580950739676E-03 0.12707067510402E+01 0.80000000000000E+04 0.30000000000000E+04 0.62957090559653E+01
 0.23608908959870E+01 0.33899745305900E+03 0.29215271394983E+03 0.33777767585582E+03 0.38398031326385E+03
 0.29215054984684E+03 0.29215135722972E+03 0.33369254894360E+03 0.38395128396789E+03 0.29215046771967E+03
 0.29215135464877E+03 0.33777767585582E+03 0.38398031326385E+03 0.29215054984684E+03 0.29215135722972E+03
 0.33369254894360E+03 0.38395128396789E+03 0.29215046771967E+03 0.29215135464877E+03 0.43399324462284E+03
 0.34572262001221E+03 0.17607184483823E+04 0.16452728289528E+04 0.66185782136155E+03 0.10275798809900E+04
 0.36241277052165E+03 0.11717551109399E+04 0.13785270655111E+04 0.10843017143507E+04 0.20352710715266E+04
 0.10707488915743E+04 0.13780322392400E+04 0.10035590906904E+04 0.20350370093440E+04 0.11717551109399E+04
 0.13785270655111E+04 0.10843017143507E+04 0.20352710715266E+04 0.10707488915743E+04 0.13780322392400E+04
 0.10035590906904E+04 0.20350370093440E+04 0.16678788250222E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54854303338368E+03 0.12948404705862E+01
 0.12948404705862E+01 0.37656090886072E+01 0.00000000000000E+00 0.33983557666206E+03 0.33983557666206E+03
 0.33983557666206E+03 0.33983557666206E+03 0.00000000000000E+00 0.00000000000000E+00 0.14076176250798E+00
 0.00000000000000E+00 -.11379702879714E+02 0.10000000000000E-02 0.16617675425102E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48141510742927E+01 0.18053066528598E+01 0.34572615733766E+03 0.43398903807167E+03
 0.30851970940258E+03 0.30851970940258E+03 0.29215017436530E+03 0.29215017054627E+03 0.30851590045768E+03
 0.30851590045768E+03 0.29215017439747E+03 0.29215017057773E+03 0.30851970940258E+03 0.30851970940258E+03
 0.29215017436530E+03 0.29215017054627E+03 0.30851590045768E+03 0.30851590045768E+03 0.29215017439747E+03
 0.29215017057773E+03 0.30618282773438E+03 0.29215071708544E+03 -.60964972575221E+02 -.78471216626370E+02
 0.22726777947774E+03 0.52810283584217E+03 0.29969871746705E+03 0.23316828869530E+03 0.21111007395998E+03
 0.23316828869530E+03 0.41672582148328E+03 0.23318547625134E+03 0.21095859255959E+03 0.23318547625134E+03
 0.41660461558040E+03 0.23316828869530E+03 0.21111007395998E+03 0.23316828869530E+03 0.41672582148328E+03
 0.23318547625134E+03 0.21095859255959E+03 0.23318547625134E+03 0.41660461558040E+03 0.23291479034843E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36770640788882E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16812742275836E+00 0.00000000000000E+00 0.00000000000000E+00 0.16812742275836E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17628095860517E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17628095860517E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256246664021E+00 0.17873926223778E+00 0.33983557666206E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
    990.93472663
 0.99014019764061E+00 0.31567663310780E+03 0.47410128458740E+03 0.42181374601168E+03 0.40673171136715E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13478090876974E+00 0.00000000000000E+00 -.18461627354451E+02
 0.92530374130661E-03 0.12711388012372E+01 0.80000000000000E+04 0.30000000000000E+04 0.62935691933988E+01
 0.23600884475245E+01 0.33928245876259E+03 0.29215290860766E+03 0.33805073759983E+03 0.38441819535069E+03
 0.29215059463842E+03 0.29215146616254E+03 0.33395338963414E+03 0.38438922525889E+03 0.29215050610498E+03
 0.29215146339774E+03 0.33805073759983E+03 0.38441819535069E+03 0.29215059463842E+03 0.29215146616254E+03
 0.33395338963414E+03 0.38438922525889E+03 0.29215050610498E+03 0.29215146339774E+03 0.43444830362965E+03
 0.34620202541912E+03 0.17634645296132E+04 0.16473316715598E+04 0.66031312691243E+03 0.10241012774045E+04
 0.36048658485748E+03 0.11735311983133E+04 0.13795926435057E+04 0.10855913235345E+04 0.20347584030457E+04
 0.10726583804280E+04 0.13790994397932E+04 0.10050644131485E+04 0.20345252243765E+04 0.11735311983133E+04
 0.13795926435057E+04 0.10855913235345E+04 0.20347584030457E+04 0.10726583804280E+04 0.13790994397932E+04
 0.10050644131485E+04 0.20345252243765E+04 0.16676211266467E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54882763173684E+03 0.12948405135245E+01
 0.12948405135245E+01 0.38009866511892E+01 0.00000000000000E+00 0.34003367792423E+03 0.34003367792423E+03
 0.34003367792423E+03 0.34003367792423E+03 0.00000000000000E+00 0.00000000000000E+00 0.14051862826817E+00
 0.00000000000000E+00 -.11368882782432E+02 0.10000000000000E-02 0.16653331976036E+01 0.80000000000000E+04
 0.30000000000000E+04 0.48038434659875E+01 0.18014412997453E+01 0.34620561321057E+03 0.43444406349415E+03
 0.30864347808694E+03 0.30864347808694E+03 0.29215018872080E+03 0.29215018458734E+03 0.30863962596452E+03
 0.30863962596452E+03 0.29215018875516E+03 0.29215018462095E+03 0.30864347808694E+03 0.30864347808694E+03
 0.29215018872080E+03 0.29215018458734E+03 0.30863962596452E+03 0.30863962596452E+03 0.29215018875516E+03
 0.29215018462095E+03 0.30629108451980E+03 0.29215076900225E+03 -.63405093926666E+02 -.82024635530629E+02
 0.22829732799358E+03 0.53003036706363E+03 0.30059155243009E+03 0.23513637030599E+03 0.21201529416202E+03
 0.23513637030599E+03 0.41817129464635E+03 0.23515379683773E+03 0.21186288629541E+03 0.23515379683773E+03
 0.41804951457636E+03 0.23513637030599E+03 0.21201529416202E+03 0.23513637030599E+03 0.41817129464635E+03
 0.23515379683773E+03 0.21186288629541E+03 0.23515379683773E+03 0.41804951457636E+03 0.23295815035343E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36792216144217E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16805315333658E+00 0.00000000000000E+00 0.00000000000000E+00 0.16805315333658E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17618257772452E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17618257772452E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256239120896E+00 0.17863507098224E+00 0.34003367792423E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1005.05406813
 0.99001927792611E+00 0.31595006683995E+03 0.47454903969633E+03 0.42221035950063E+03 0.40710937278889E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13464207665532E+00 0.00000000000000E+00 -.18478905406985E+02
 0.92450279489239E-03 0.12717907427059E+01 0.80000000000000E+04 0.30000000000000E+04 0.62903430032669E+01
 0.23588786262251E+01 0.33973392882206E+03 0.29215324549730E+03 0.33848334657638E+03 0.38511296273769E+03
 0.29215067322030E+03 0.29215165691610E+03 0.33436653886971E+03 0.38508408393129E+03 0.29215057350508E+03
 0.29215165383393E+03 0.33848334657638E+03 0.38511296273769E+03 0.29215067322030E+03 0.29215165691610E+03
 0.33436653886971E+03 0.38508408393130E+03 0.29215057350508E+03 0.29215165383393E+03 0.43517675979089E+03
 0.34696723562454E+03 0.17678074354375E+04 0.16505866751789E+04 0.65769545564043E+03 0.10183374970869E+04
 0.35735356416830E+03 0.11763425025618E+04 0.13812412717127E+04 0.10876325028343E+04 0.20338965944496E+04
 0.10756832468666E+04 0.13807505285711E+04 0.10074499750030E+04 0.20336647291525E+04 0.11763425025618E+04
 0.13812412717127E+04 0.10876325028343E+04 0.20338965944496E+04 0.10756832468666E+04 0.13807505285711E+04
 0.10074499750030E+04 0.20336647291525E+04 0.16671279560252E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54927627685556E+03 0.12948406408274E+01
 0.12948406408274E+01 0.38574640171769E+01 0.00000000000000E+00 0.34034952996167E+03 0.34034952996167E+03
 0.34034952996167E+03 0.34034952996167E+03 0.00000000000000E+00 0.00000000000000E+00 0.14014352212300E+00
 0.00000000000000E+00 -.11360483119928E+02 0.10000000000000E-02 0.16707755322858E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47881955687101E+01 0.17955733382663E+01 0.34697087261818E+03 0.43517249663844E+03
 0.30883949213139E+03 0.30883949213139E+03 0.29215021768069E+03 0.29215020925114E+03 0.30883557150245E+03
 0.30883557150245E+03 0.29215021771945E+03 0.29215020928840E+03 0.30883949213139E+03 0.30883949213139E+03
 0.29215021768069E+03 0.29215020925114E+03 0.30883557150245E+03 0.30883557150245E+03 0.29215021771945E+03
 0.29215020928840E+03 0.30646251118847E+03 0.29215085893997E+03 -.67337796622345E+02 -.87768508698397E+02
 0.22994748060012E+03 0.53312736881172E+03 0.30203015080860E+03 0.23829905187578E+03 0.21346649881568E+03
 0.23829905187578E+03 0.42049664600874E+03 0.23831685784466E+03 0.21331254116735E+03 0.23831685784466E+03
 0.42037387613192E+03 0.23829905187578E+03 0.21346649881568E+03 0.23829905187578E+03 0.42049664600874E+03
 0.23831685784466E+03 0.21331254116735E+03 0.23831685784466E+03 0.42037387613192E+03 0.23302038784460E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36826716969872E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16800837848640E+00 0.00000000000000E+00 0.00000000000000E+00 0.16800837848640E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17605052366333E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17605052366333E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256201594159E+00 0.17846892029747E+00 0.34034952996167E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1013.27714919
 0.98997097172440E+00 0.31610755661581E+03 0.47480725960997E+03 0.42243789321480E+03 0.40732581425265E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13456356470647E+00 0.00000000000000E+00 -.18495254474096E+02
 0.92404204387867E-03 0.12721453744564E+01 0.80000000000000E+04 0.30000000000000E+04 0.62885894651924E+01
 0.23582210494472E+01 0.33999587652790E+03 0.29215345685010E+03 0.33873435371729E+03 0.38551483793290E+03
 0.29215072314277E+03 0.29215177788888E+03 0.33460639060265E+03 0.38548601111059E+03 0.29215061635748E+03
 0.29215177460812E+03 0.33873435371729E+03 0.38551483793290E+03 0.29215072314277E+03 0.29215177788888E+03
 0.33460639060265E+03 0.38548601111059E+03 0.29215061635748E+03 0.29215177460812E+03 0.43559551442629E+03
 0.34740668966800E+03 0.17702922199467E+04 0.16524317416763E+04 0.65618894052980E+03 0.10150332706170E+04
 0.35556338538452E+03 0.11779560215379E+04 0.13821665315142E+04 0.10887927441759E+04 0.20333666409259E+04
 0.10774208570393E+04 0.13816772014052E+04 0.10088099418783E+04 0.20331355335749E+04 0.11779560215379E+04
 0.13821665315142E+04 0.10887927441759E+04 0.20333666409259E+04 0.10774208570393E+04 0.13816772014052E+04
 0.10088099418783E+04 0.20331355335749E+04 0.16668198250200E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54953193841886E+03 0.12948407612857E+01
 0.12948407612857E+01 0.38903563414432E+01 0.00000000000000E+00 0.34053433853761E+03 0.34053433853761E+03
 0.34053433853761E+03 0.34053433853761E+03 0.00000000000000E+00 0.00000000000000E+00 0.13993214432418E+00
 0.00000000000000E+00 -.11363786740478E+02 0.10000000000000E-02 0.16738041071867E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47795318255291E+01 0.17923244345734E+01 0.34741035356289E+03 0.43559123781733E+03
 0.30895385061444E+03 0.30895385061444E+03 0.29215023399919E+03 0.29215022493772E+03 0.30894988985657E+03
 0.30894988985657E+03 0.29215023404030E+03 0.29215022497724E+03 0.30895385061444E+03 0.30895385061444E+03
 0.29215023399919E+03 0.29215022493772E+03 0.30894988985657E+03 0.30894988985657E+03 0.29215023404030E+03
 0.29215022497724E+03 0.30656254166951E+03 0.29215091541431E+03 -.69587614731464E+02 -.91061700531667E+02
 0.23091362280532E+03 0.53494933600362E+03 0.30288114508427E+03 0.24012784771867E+03 0.21431657089380E+03
 0.24012784771867E+03 0.42186694593517E+03 0.24014587717412E+03 0.21416172383677E+03 0.24014587717412E+03
 0.42174361570493E+03 0.24012784771867E+03 0.21431657089380E+03 0.24012784771867E+03 0.42186694593516E+03
 0.24014587717412E+03 0.21416172383677E+03 0.24014587717412E+03 0.42174361570492E+03 0.23307831666619E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36847260788026E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16804075301890E+00 0.00000000000000E+00 0.00000000000000E+00 0.16804075301890E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17603824839901E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17603824839901E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256155928124E+00 0.17837158705082E+00 0.34053433853761E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1021.50023026
 0.98994342818721E+00 0.31626389095600E+03 0.47506409494226E+03 0.42266302216526E+03 0.40753972820901E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13448719805477E+00 0.00000000000000E+00 -.18513313274109E+02
 0.92358511036131E-03 0.12724747015145E+01 0.80000000000000E+04 0.30000000000000E+04 0.62869619258273E+01
 0.23576107221852E+01 0.34025717210003E+03 0.29215367885120E+03 0.33898474124637E+03 0.38591451743041E+03
 0.29215077602176E+03 0.29215190587479E+03 0.33484576768692E+03 0.38588574177750E+03 0.29215066177156E+03
 0.29215190238582E+03 0.33898474124637E+03 0.38591451743041E+03 0.29215077602176E+03 0.29215190587479E+03
 0.33484576768692E+03 0.38588574177750E+03 0.29215066177156E+03 0.29215190238582E+03 0.43600985500652E+03
 0.34784115184801E+03 0.17727468474019E+04 0.16542425752042E+04 0.65471051913942E+03 0.10117913667818E+04
 0.35380729504664E+03 0.11795543465708E+04 0.13830763953602E+04 0.10899346553225E+04 0.20328321841733E+04
 0.10791429019505E+04 0.13825884494512E+04 0.10101506981229E+04 0.20326018154860E+04 0.11795543465708E+04
 0.13830763953602E+04 0.10899346553225E+04 0.20328321841733E+04 0.10791429019505E+04 0.13825884494512E+04
 0.10101506981229E+04 0.20326018154860E+04 0.16665088414968E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.54978326979478E+03 0.12948408943412E+01
 0.12948408943412E+01 0.39232486657096E+01 0.00000000000000E+00 0.34072045566473E+03 0.34072045566473E+03
 0.34072045566473E+03 0.34072045566473E+03 0.00000000000000E+00 0.00000000000000E+00 0.13972576756508E+00
 0.00000000000000E+00 -.11369262926269E+02 0.10000000000000E-02 0.16767297568771E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47711922372631E+01 0.17891970889737E+01 0.34784485616541E+03 0.43600555135039E+03
 0.30906855414032E+03 0.30906855414032E+03 0.29215025129731E+03 0.29215024156599E+03 0.30906455316862E+03
 0.30906455316862E+03 0.29215025134086E+03 0.29215024160785E+03 0.30906855414032E+03 0.30906855414032E+03
 0.29215025129731E+03 0.29215024156599E+03 0.30906455316862E+03 0.30906455316862E+03 0.29215025134086E+03
 0.29215024160785E+03 0.30666289784422E+03 0.29215097476963E+03 -.71798519546529E+02 -.94301532529816E+02
 0.23188745935812E+03 0.53679009577532E+03 0.30374319912040E+03 0.24194670809913E+03 0.21517408242448E+03
 0.24194670809913E+03 0.42325304989263E+03 0.24196496243008E+03 0.21501835429328E+03 0.24196496243008E+03
 0.42312916878691E+03 0.24194670809913E+03 0.21517408242447E+03 0.24194670809913E+03 0.42325304989263E+03
 0.24196496243008E+03 0.21501835429328E+03 0.24196496243008E+03 0.42312916878691E+03 0.23315830441402E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36867972774310E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16808994934085E+00 0.00000000000000E+00 0.00000000000000E+00 0.16808994934085E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17603758928306E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17603758928306E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256103764264E+00 0.17827360376904E+00 0.34072045566473E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1030.14783626
 0.98983437149849E+00 0.31643168642562E+03 0.47533672783521E+03 0.42290683723801E+03 0.40777248642416E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13440839310348E+00 0.00000000000000E+00 -.18482216914722E+02
 0.92309564061491E-03 0.12727944653520E+01 0.80000000000000E+04 0.30000000000000E+04 0.62853824539437E+01
 0.23570184202289E+01 0.34053379403990E+03 0.29215391859956E+03 0.33924969685198E+03 0.38633120413808E+03
 0.29215083338175E+03 0.29215204461965E+03 0.33509965369184E+03 0.38630248297580E+03 0.29215071104770E+03
 0.29215204090605E+03 0.33924969685198E+03 0.38633120413808E+03 0.29215083338175E+03 0.29215204461965E+03
 0.33509965369184E+03 0.38630248297580E+03 0.29215071104770E+03 0.29215204090605E+03 0.43642843144117E+03
 0.34827826831202E+03 0.17753939798308E+04 0.16562231487747E+04 0.65348471029893E+03 0.10089275648741E+04
 0.35217543102368E+03 0.11812655800541E+04 0.13841147357643E+04 0.10911745867850E+04 0.20324018271983E+04
 0.10809799020302E+04 0.13836283942066E+04 0.10115924082020E+04 0.20321723812620E+04 0.11812655800541E+04
 0.13841147357643E+04 0.10911745867850E+04 0.20324018271983E+04 0.10809799020302E+04 0.13836283942066E+04
 0.10115924082020E+04 0.20321723812620E+04 0.16664067008000E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55006168047228E+03 0.12948406652263E+01
 0.12948406652263E+01 0.39578390896940E+01 0.00000000000000E+00 0.34091189939957E+03 0.34091189939957E+03
 0.34091189939957E+03 0.34091189939957E+03 0.00000000000000E+00 0.00000000000000E+00 0.13951427403376E+00
 0.00000000000000E+00 -.11317395737748E+02 0.10000000000000E-02 0.16797219581439E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47626929928571E+01 0.17860098723214E+01 0.34828209215695E+03 0.43642402783946E+03
 0.30919199407725E+03 0.30919199407725E+03 0.29215026148774E+03 0.29215025961062E+03 0.30918795035486E+03
 0.30918795035486E+03 0.29215026153243E+03 0.29215025965500E+03 0.30919199407725E+03 0.30919199407725E+03
 0.29215026148774E+03 0.29215025961062E+03 0.30918795035486E+03 0.30918795035486E+03 0.29215026153243E+03
 0.29215025965500E+03 0.30677109297823E+03 0.29215103889047E+03 -.73998688176286E+02 -.97530465082867E+02
 0.23286023923933E+03 0.53857388464089E+03 0.30454934420537E+03 0.24375555440919E+03 0.21602477162484E+03
 0.24375555440919E+03 0.42457817083734E+03 0.24377405179080E+03 0.21586824361040E+03 0.24377405179080E+03
 0.42445383605164E+03 0.24375555440919E+03 0.21602477162483E+03 0.24375555440919E+03 0.42457817083734E+03
 0.24377405179080E+03 0.21586824361040E+03 0.24377405179080E+03 0.42445383605164E+03 0.23320895177085E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36887658587821E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16768794376581E+00 0.00000000000000E+00 0.00000000000000E+00 0.16768794376581E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17576519631918E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17576519631918E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256216399855E+00 0.17817474602929E+00 0.34091189939957E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1040.84364821
 0.98972107144740E+00 0.31663807313958E+03 0.47567074502871E+03 0.42320474955629E+03 0.40805691663368E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13431203397063E+00 0.00000000000000E+00 -.18490937668332E+02
 0.92249388158271E-03 0.12731826045289E+01 0.80000000000000E+04 0.30000000000000E+04 0.62834663084011E+01
 0.23562998656504E+01 0.34087314882870E+03 0.29215423261895E+03 0.33957480364329E+03 0.38684578681676E+03
 0.29215090923255E+03 0.29215222784256E+03 0.33541087620573E+03 0.38681713114285E+03 0.29215077624765E+03
 0.29215222383541E+03 0.33957480364329E+03 0.38684578681676E+03 0.29215090923255E+03 0.29215222784256E+03
 0.33541087620573E+03 0.38681713114285E+03 0.29215077624765E+03 0.29215222383541E+03 0.43695674371953E+03
 0.34882762829378E+03 0.17786342999290E+04 0.16586486898837E+04 0.65170668251865E+03 0.10049788124382E+04
 0.35001359650691E+03 0.11833635459527E+04 0.13853370449681E+04 0.10926965525036E+04 0.20317917413869E+04
 0.10832356184397E+04 0.13848525395476E+04 0.10133666742977E+04 0.20315633062833E+04 0.11833635459527E+04
 0.13853370449681E+04 0.10926965525036E+04 0.20317917413869E+04 0.10832356184397E+04 0.13848525395476E+04
 0.10133666742977E+04 0.20315633062833E+04 0.16661392924564E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55039996435696E+03 0.12948407294799E+01
 0.12948407294799E+01 0.40006223375133E+01 0.00000000000000E+00 0.34114801431086E+03 0.34114801431086E+03
 0.34114801431086E+03 0.34114801431086E+03 0.00000000000000E+00 0.00000000000000E+00 0.13926003712511E+00
 0.00000000000000E+00 -.11306338714672E+02 0.10000000000000E-02 0.16832839369777E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47526147100077E+01 0.17822305162529E+01 0.34883151062498E+03 0.43695231575159E+03
 0.30934224742347E+03 0.30934224742347E+03 0.29215028725773E+03 0.29215028349299E+03 0.30933815142601E+03
 0.30933815142601E+03 0.29215028730598E+03 0.29215028354061E+03 0.30934224742347E+03 0.30934224742347E+03
 0.29215028725773E+03 0.29215028349299E+03 0.30933815142601E+03 0.30933815142601E+03 0.29215028730598E+03
 0.29215028354061E+03 0.30690271484205E+03 0.29215112293337E+03 -.76820672127793E+02 -.10167945248021E+03
 0.23407416826976E+03 0.54083008658767E+03 0.30558554747657E+03 0.24604520161628E+03 0.21708716965360E+03
 0.24604520161628E+03 0.42626262485846E+03 0.24606399502005E+03 0.21692953778172E+03 0.24606399502005E+03
 0.42613761297088E+03 0.24604520161628E+03 0.21708716965360E+03 0.24604520161628E+03 0.42626262485845E+03
 0.24606399502005E+03 0.21692953778172E+03 0.24606399502005E+03 0.42613761297088E+03 0.23325993821682E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36913303961950E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16761620857220E+00 0.00000000000000E+00 0.00000000000000E+00 0.16761620857220E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17564625949418E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17564625949418E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256202387900E+00 0.17805130282227E+00 0.34114801431086E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1050.09101554
 0.98963181355899E+00 0.31681574157561E+03 0.47595764093270E+03 0.42346034544137E+03 0.40830097461016E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13423000607911E+00 0.00000000000000E+00 -.18498252888574E+02
 0.92197648577302E-03 0.12735057358230E+01 0.80000000000000E+04 0.30000000000000E+04 0.62818719813853E+01
 0.23557019930195E+01 0.34116544526640E+03 0.29215452008885E+03 0.33985483139444E+03 0.38728825321859E+03
 0.29215097933722E+03 0.29215239695392E+03 0.33567901896710E+03 0.38725965307744E+03 0.29215083654439E+03
 0.29215239267866E+03 0.33985483139444E+03 0.38728825321859E+03 0.29215097933722E+03 0.29215239695392E+03
 0.33567901896710E+03 0.38725965307744E+03 0.29215083654439E+03 0.29215239267866E+03 0.43741081665879E+03
 0.34929871605118E+03 0.17814115568100E+04 0.16607228652065E+04 0.65015911902824E+03 0.10015660634925E+04
 0.34815614886911E+03 0.11851639276325E+04 0.13863729386811E+04 0.10939999105071E+04 0.20312518922524E+04
 0.10851721683224E+04 0.13858900001306E+04 0.10148873191788E+04 0.20310243223642E+04 0.11851639276325E+04
 0.13863729386811E+04 0.10939999105071E+04 0.20312518922524E+04 0.10851721683224E+04 0.13858900001306E+04
 0.10148873191788E+04 0.20310243223642E+04 0.16658936156283E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55068942425986E+03 0.12948407833778E+01
 0.12948407833778E+01 0.40376118068303E+01 0.00000000000000E+00 0.34135166889197E+03 0.34135166889197E+03
 0.34135166889197E+03 0.34135166889197E+03 0.00000000000000E+00 0.00000000000000E+00 0.13904654418713E+00
 0.00000000000000E+00 -.11296896073501E+02 0.10000000000000E-02 0.16862445612092E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47442703057634E+01 0.17791013646613E+01 0.34930261118511E+03 0.43740639800370E+03
 0.30947194947036E+03 0.30947194947036E+03 0.29215030964357E+03 0.29215030558545E+03 0.30946780827959E+03
 0.30946780827959E+03 0.29215030969478E+03 0.29215030563600E+03 0.30947194947036E+03 0.30947194947036E+03
 0.29215030964357E+03 0.29215030558545E+03 0.30946780827959E+03 0.30946780827959E+03 0.29215030969478E+03
 0.29215030563600E+03 0.30701636093763E+03 0.29215119992442E+03 -.79247461799916E+02 -.10525209447282E+03
 0.23512129473071E+03 0.54277568092944E+03 0.30647877972508E+03 0.24801618878825E+03 0.21800319247393E+03
 0.24801618878825E+03 0.42771508949665E+03 0.24803523903935E+03 0.21784460116370E+03 0.24803523903935E+03
 0.42758948733836E+03 0.24801618878825E+03 0.21800319247393E+03 0.24801618878825E+03 0.42771508949665E+03
 0.24803523903935E+03 0.21784460116370E+03 0.24803523903935E+03 0.42758948733836E+03 0.23330511533959E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36935455146398E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16755329716705E+00 0.00000000000000E+00 0.00000000000000E+00 0.16755329716705E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17555171464424E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17555171464424E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256190080698E+00 0.17794496491651E+00 0.34135166889197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1060.89731944
 0.98953732598915E+00 0.31702237013023E+03 0.47629057870855E+03 0.42375663296466E+03 0.40858391981674E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13413587424663E+00 0.00000000000000E+00 -.18508066479411E+02
 0.92137547151633E-03 0.12738661183017E+01 0.80000000000000E+04 0.30000000000000E+04 0.62800948114278E+01
 0.23550355542854E+01 0.34150553271051E+03 0.29215487591495E+03 0.34018066121400E+03 0.38780258932643E+03
 0.29215106695568E+03 0.29215260801777E+03 0.33599107083106E+03 0.38777405275159E+03 0.29215091195042E+03
 0.29215260341149E+03 0.34018066121400E+03 0.38780258932643E+03 0.29215106695568E+03 0.29215260801777E+03
 0.33599107083106E+03 0.38777405275159E+03 0.29215091195042E+03 0.29215260341149E+03 0.43793928847674E+03
 0.34984580199425E+03 0.17846283101667E+04 0.16631202606343E+04 0.64831877242439E+03 0.99754772073164E+03
 0.34598735444513E+03 0.11872516167130E+04 0.13875551849561E+04 0.10955083375818E+04 0.20306006176810E+04
 0.10874189133299E+04 0.13870740397087E+04 0.10166489792176E+04 0.20303740353634E+04 0.11872516167130E+04
 0.13875551849561E+04 0.10955083375818E+04 0.20306006176810E+04 0.10874189133299E+04 0.13870740397087E+04
 0.10166489792176E+04 0.20303740353634E+04 0.16655797581442E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55102409244783E+03 0.12948408556833E+01
 0.12948408556833E+01 0.40808370224088E+01 0.00000000000000E+00 0.34158924815728E+03 0.34158924815728E+03
 0.34158924815728E+03 0.34158924815728E+03 0.00000000000000E+00 0.00000000000000E+00 0.13880419400613E+00
 0.00000000000000E+00 -.11287475731146E+02 0.10000000000000E-02 0.16895692688728E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47349346057515E+01 0.17756004771568E+01 0.34984971969832E+03 0.43793486972227E+03
 0.30962311387503E+03 0.30962311387503E+03 0.29215033958199E+03 0.29215033322172E+03 0.30961891992672E+03
 0.30961891992672E+03 0.29215033963713E+03 0.29215033327582E+03 0.30962311387503E+03 0.30962311387503E+03
 0.29215033958199E+03 0.29215033322172E+03 0.30961891992672E+03 0.30961891992672E+03 0.29215033963713E+03
 0.29215033327582E+03 0.30714883009588E+03 0.29215129529077E+03 -.82076889304477E+02 -.10942228218356E+03
 0.23634455434912E+03 0.54504891480223E+03 0.30752263768136E+03 0.25031540157840E+03 0.21907304313169E+03
 0.25031540157840E+03 0.42941253706888E+03 0.25033475254948E+03 0.21891331442827E+03 0.25033475254948E+03
 0.42928622879587E+03 0.25031540157840E+03 0.21907304313169E+03 0.25031540157840E+03 0.42941253706888E+03
 0.25033475254948E+03 0.21891331442827E+03 0.25033475254948E+03 0.42928622879587E+03 0.23335899595638E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36961314826408E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16749252655734E+00 0.00000000000000E+00 0.00000000000000E+00 0.16749252655734E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17544885964543E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17544885964543E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256171170671E+00 0.17782102381560E+00 0.34158924815728E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1070.02151473
 0.98946466773484E+00 0.31719600904730E+03 0.47656993171934E+03 0.42400497657191E+03 0.40882108986959E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13405800782081E+00 0.00000000000000E+00 -.18511489287798E+02
 0.92087106255939E-03 0.12741533496069E+01 0.80000000000000E+04 0.30000000000000E+04 0.62786790949991E+01
 0.23545046606247E+01 0.34179172486009E+03 0.29215519350143E+03 0.34045485988358E+03 0.38823433440603E+03
 0.29215114589342E+03 0.29215279791110E+03 0.33625377995855E+03 0.38820585056062E+03 0.29215097992559E+03
 0.29215279301013E+03 0.34045485988358E+03 0.38823433440603E+03 0.29215114589342E+03 0.29215279791110E+03
 0.33625377995855E+03 0.38820585056062E+03 0.29215097992559E+03 0.29215279301013E+03 0.43838155613651E+03
 0.35030280361020E+03 0.17873206208871E+04 0.16651208028188E+04 0.64677816894122E+03 0.99419433271851E+03
 0.34418227293258E+03 0.11890010021905E+04 0.13885352916535E+04 0.10967685870685E+04 0.20300428672799E+04
 0.10893021714369E+04 0.13880556459899E+04 0.10181219309324E+04 0.20298171148456E+04 0.11890010021905E+04
 0.13885352916535E+04 0.10967685870685E+04 0.20300428672799E+04 0.10893021714369E+04 0.13880556459898E+04
 0.10181219309324E+04 0.20298171148456E+04 0.16653112101237E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55130398494314E+03 0.12948408809022E+01
 0.12948408809022E+01 0.41173338035704E+01 0.00000000000000E+00 0.34178963570896E+03 0.34178963570896E+03
 0.34178963570896E+03 0.34178963570896E+03 0.00000000000000E+00 0.00000000000000E+00 0.13860534457584E+00
 0.00000000000000E+00 -.11274291890674E+02 0.10000000000000E-02 0.16922667232919E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47273871724181E+01 0.17727701896568E+01 0.35030674602986E+03 0.43837713022869E+03
 0.30975077855181E+03 0.30975077855181E+03 0.29215036497740E+03 0.29215035814148E+03 0.30974653999849E+03
 0.30974653999849E+03 0.29215036503572E+03 0.29215035819871E+03 0.30975077855181E+03 0.30975077855181E+03
 0.29215036497740E+03 0.29215035814148E+03 0.30974653999849E+03 0.30974653999849E+03 0.29215036503572E+03
 0.29215035819871E+03 0.30726073600796E+03 0.29215138046753E+03 -.84440748008612E+02 -.11290924330399E+03
 0.23737572950977E+03 0.54696175013313E+03 0.30839914197581E+03 0.25224361653471E+03 0.21997456019687E+03
 0.25224361653471E+03 0.43084018453539E+03 0.25226322285986E+03 0.21981387482987E+03 0.25226322285986E+03
 0.43071328408859E+03 0.25224361653471E+03 0.21997456019687E+03 0.25224361653471E+03 0.43084018453539E+03
 0.25226322285986E+03 0.21981387482987E+03 0.25226322285986E+03 0.43071328408859E+03 0.23340942894599E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36982980417369E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16739849900222E+00 0.00000000000000E+00 0.00000000000000E+00 0.16739849900222E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17534352550698E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17534352550698E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256170416030E+00 0.17771678464282E+00 0.34178963570896E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1081.42675884
 0.98936550695514E+00 0.31741264439462E+03 0.47691730462684E+03 0.42431450161606E+03 0.40911692709224E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13396256822389E+00 0.00000000000000E+00 -.18518727245417E+02
 0.92024249877917E-03 0.12744915997736E+01 0.80000000000000E+04 0.30000000000000E+04 0.62770127330940E+01
 0.23538797749103E+01 0.34214826965997E+03 0.29215561306723E+03 0.34079646202563E+03 0.38877089150224E+03
 0.29215125116138E+03 0.29215305079477E+03 0.33658119341043E+03 0.38874247245176E+03 0.29215107062784E+03
 0.29215304550552E+03 0.34079646202563E+03 0.38877089150224E+03 0.29215125116138E+03 0.29215305079477E+03
 0.33658119341043E+03 0.38874247245176E+03 0.29215107062784E+03 0.29215304550552E+03 0.43892995120578E+03
 0.35086809767540E+03 0.17906724092300E+04 0.16676121443345E+04 0.64486973183532E+03 0.99005218671992E+03
 0.34195810622543E+03 0.11911783292274E+04 0.13897462642591E+04 0.10983376474868E+04 0.20293461513987E+04
 0.10916461049602E+04 0.13892684808511E+04 0.10199551187298E+04 0.20291214366268E+04 0.11911783292274E+04
 0.13897462642591E+04 0.10983376474868E+04 0.20293461513987E+04 0.10916461049602E+04 0.13892684808511E+04
 0.10199551187298E+04 0.20291214366268E+04 0.16649855270361E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55165338088980E+03 0.12948409342308E+01
 0.12948409342308E+01 0.41629547800225E+01 0.00000000000000E+00 0.34203878201965E+03 0.34203878201965E+03
 0.34203878201965E+03 0.34203878201965E+03 0.00000000000000E+00 0.00000000000000E+00 0.13836399985930E+00
 0.00000000000000E+00 -.11261190110308E+02 0.10000000000000E-02 0.16955067701100E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47183533212793E+01 0.17693824954797E+01 0.35087208137554E+03 0.43892550730272E+03
 0.30991029694446E+03 0.30991029694446E+03 0.29215039887314E+03 0.29215039140236E+03 0.30990600260669E+03
 0.30990600260669E+03 0.29215039893559E+03 0.29215039146364E+03 0.30991029694446E+03 0.30991029694446E+03
 0.29215039887314E+03 0.29215039140236E+03 0.30990600260669E+03 0.30990600260669E+03 0.29215039893559E+03
 0.29215039146364E+03 0.30740060827551E+03 0.29215149307420E+03 -.87374136791171E+02 -.11724029104041E+03
 0.23865495838474E+03 0.54933067392405E+03 0.30948244074739E+03 0.25463434286760E+03 0.22109160416322E+03
 0.25463434286760E+03 0.43260643987130E+03 0.25465426999184E+03 0.22092972494217E+03 0.25465426999184E+03
 0.43247880154052E+03 0.25463434286760E+03 0.22109160416322E+03 0.25463434286760E+03 0.43260643987130E+03
 0.25465426999184E+03 0.22092972494217E+03 0.25465426999184E+03 0.43247880154052E+03 0.23346830725377E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37010003398015E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16730805402419E+00 0.00000000000000E+00 0.00000000000000E+00 0.16730805402419E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17522575282713E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17522575282713E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256160028912E+00 0.17758724908011E+00 0.34203878201965E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1092.00275118
 0.98927635232821E+00 0.31761276782348E+03 0.47723745277057E+03 0.42459981074857E+03 0.40938971099788E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13387581136903E+00 0.00000000000000E+00 -.18526455363087E+02
 0.91966259650400E-03 0.12747869273436E+01 0.80000000000000E+04 0.30000000000000E+04 0.62755585489652E+01
 0.23533344558619E+01 0.34247743210602E+03 0.29215602599174E+03 0.34111184049179E+03 0.38926554780814E+03
 0.29215135581613E+03 0.29215330182713E+03 0.33688354032428E+03 0.38923718760904E+03 0.29215116085917E+03
 0.29215329615690E+03 0.34111184049179E+03 0.38926554780814E+03 0.29215135581613E+03 0.29215330182713E+03
 0.33688354032428E+03 0.38923718760904E+03 0.29215116085917E+03 0.29215329615690E+03 0.43943580357848E+03
 0.35138825254673E+03 0.17937604798455E+04 0.16699059748071E+04 0.64308410060781E+03 0.98620649138844E+03
 0.33990697027759E+03 0.11931854881289E+04 0.13908497669771E+04 0.10997833958017E+04 0.20286925763550E+04
 0.10938074365539E+04 0.13903736785352E+04 0.10216447144787E+04 0.20284688053866E+04 0.11931854881289E+04
 0.13908497669771E+04 0.10997833958017E+04 0.20286925763550E+04 0.10938074365539E+04 0.13903736785352E+04
 0.10216447144787E+04 0.20284688053866E+04 0.16646729052465E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55197513387471E+03 0.12948409911708E+01
 0.12948409911708E+01 0.42052587493668E+01 0.00000000000000E+00 0.34226902592403E+03 0.34226902592403E+03
 0.34226902592403E+03 0.34226902592403E+03 0.00000000000000E+00 0.00000000000000E+00 0.13814709100922E+00
 0.00000000000000E+00 -.11250170900529E+02 0.10000000000000E-02 0.16983832012660E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47103621809476E+01 0.17663858178553E+01 0.35139227067175E+03 0.43943134800092E+03
 0.31005786038059E+03 0.31005786038059E+03 0.29215043545438E+03 0.29215042450073E+03 0.31005351437370E+03
 0.31005351437370E+03 0.29215043552124E+03 0.29215042456591E+03 0.31005786038059E+03 0.31005786038059E+03
 0.29215043545438E+03 0.29215042450073E+03 0.31005351437370E+03 0.31005351437370E+03 0.29215043552124E+03
 0.29215042456591E+03 0.30753002396548E+03 0.29215160398317E+03 -.90084916955975E+02 -.12124591482337E+03
 0.23983769296036E+03 0.55151872621943E+03 0.31048184479427E+03 0.25684294116548E+03 0.22212377255978E+03
 0.25684294116548E+03 0.43423734717758E+03 0.25686316648499E+03 0.22196077313553E+03 0.25686316648499E+03
 0.43410901111925E+03 0.25684294116548E+03 0.22212377255978E+03 0.25684294116548E+03 0.43423734717758E+03
 0.25686316648499E+03 0.22196077313553E+03 0.25686316648499E+03 0.43410901111925E+03 0.23352127856133E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37034959536439E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16723430932436E+00 0.00000000000000E+00 0.00000000000000E+00 0.16723430932436E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17511671374506E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17511671374506E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256147344955E+00 0.17746767524413E+00 0.34226902592403E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1103.20128878
 0.98919239443375E+00 0.31782342456985E+03 0.47757405448690E+03 0.42489935178025E+03 0.40967608284147E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13378586937248E+00 0.00000000000000E+00 -.18538065183116E+02
 0.91905292902857E-03 0.12750800732598E+01 0.80000000000000E+04 0.30000000000000E+04 0.62741157734099E+01
 0.23527934150287E+01 0.34282465826880E+03 0.29215648984778E+03 0.34144453453051E+03 0.38978618201863E+03
 0.29215147457292E+03 0.29215358625288E+03 0.33720260517805E+03 0.38975788299073E+03 0.29215126331432E+03
 0.29215358015603E+03 0.34144453453051E+03 0.38978618201863E+03 0.29215147457292E+03 0.29215358625288E+03
 0.33720260517805E+03 0.38975788299073E+03 0.29215126331432E+03 0.29215358015603E+03 0.43996677046213E+03
 0.35193323156529E+03 0.17969979242520E+04 0.16723017632179E+04 0.64120175075836E+03 0.98217058981892E+03
 0.33776283030676E+03 0.11952926470072E+04 0.13919913650063E+04 0.11012954579901E+04 0.20279848228133E+04
 0.10960774265619E+04 0.13915170532335E+04 0.10234137825198E+04 0.20277620459735E+04 0.11952926470072E+04
 0.13919913650063E+04 0.11012954579900E+04 0.20279848228133E+04 0.10960774265619E+04 0.13915170532335E+04
 0.10234137825198E+04 0.20277620459735E+04 0.16643304102873E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55231205526532E+03 0.12948410767109E+01
 0.12948410767109E+01 0.42500528997859E+01 0.00000000000000E+00 0.34251244832043E+03 0.34251244832043E+03
 0.34251244832043E+03 0.34251244832043E+03 0.00000000000000E+00 0.00000000000000E+00 0.13792434588185E+00
 0.00000000000000E+00 -.11242975262076E+02 0.10000000000000E-02 0.17012980942538E+01 0.80000000000000E+04
 0.30000000000000E+04 0.47022917541732E+01 0.17633594078150E+01 0.35193727654685E+03 0.43996231087880E+03
 0.31021400464443E+03 0.31021400464443E+03 0.29215047401842E+03 0.29215046209471E+03 0.31020960382448E+03
 0.31020960382448E+03 0.29215047408968E+03 0.29215046216418E+03 0.31021400464443E+03 0.31021400464443E+03
 0.29215047401842E+03 0.29215046209471E+03 0.31020960382448E+03 0.31020960382448E+03 0.29215047408968E+03
 0.29215046216418E+03 0.30766699657038E+03 0.29215172866816E+03 -.92927131955128E+02 -.12544781806882E+03
 0.24108827164111E+03 0.55383395342267E+03 0.31154024042336E+03 0.25916692596275E+03 0.22321479156916E+03
 0.25916692596275E+03 0.43596356466974E+03 0.25918746899918E+03 0.22305060823279E+03 0.25918746899918E+03
 0.43583449347367E+03 0.25916692596275E+03 0.22321479156916E+03 0.25916692596275E+03 0.43596356466974E+03
 0.25918746899918E+03 0.22305060823279E+03 0.25918746899918E+03 0.43583449347367E+03 0.23358625334232E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37061531605708E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16718859990683E+00 0.00000000000000E+00 0.00000000000000E+00 0.16718859990683E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17503500828819E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17503500828819E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256121121379E+00 0.17734129268935E+00 0.34251244832043E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1110.66698052
 0.98914366169519E+00 0.31796325456564E+03 0.47779732127460E+03 0.42509770327189E+03 0.40986568264185E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13372714760514E+00 0.00000000000000E+00 -.18546093195811E+02
 0.91864868636059E-03 0.12752618749716E+01 0.80000000000000E+04 0.30000000000000E+04 0.62732213336010E+01
 0.23524580001004E+01 0.34305547824473E+03 0.29215681420361E+03 0.34166569823744E+03 0.39013138532950E+03
 0.29215155829581E+03 0.29215378652287E+03 0.33741479349954E+03 0.39010312648754E+03 0.29215133558194E+03
 0.29215378012850E+03 0.34166569823744E+03 0.39013138532950E+03 0.29215155829581E+03 0.29215378652287E+03
 0.33741479349954E+03 0.39010312648754E+03 0.29215133558194E+03 0.29215378012850E+03 0.44031768827220E+03
 0.35229275035517E+03 0.17991391659346E+04 0.16738816759139E+04 0.63996290312904E+03 0.97951840260925E+03
 0.33635568496457E+03 0.11966881913978E+04 0.13927423305160E+04 0.11022940539098E+04 0.20275120518306E+04
 0.10975811557441E+04 0.13922691901811E+04 0.10245828194013E+04 0.20272899317367E+04 0.11966881913978E+04
 0.13927423305160E+04 0.11022940539098E+04 0.20275120518306E+04 0.10975811557441E+04 0.13922691901811E+04
 0.10245828194013E+04 0.20272899317367E+04 0.16641034081456E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55253454976639E+03 0.12948411358605E+01
 0.12948411358605E+01 0.42799156667320E+01 0.00000000000000E+00 0.34267475996976E+03 0.34267475996976E+03
 0.34267475996976E+03 0.34267475996976E+03 0.00000000000000E+00 0.00000000000000E+00 0.13777967054526E+00
 0.00000000000000E+00 -.11238614419626E+02 0.10000000000000E-02 0.17031681348557E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46971287427697E+01 0.17614232785386E+01 0.35229681860205E+03 0.44031322069798E+03
 0.31031817869361E+03 0.31031817869361E+03 0.29215050122685E+03 0.29215048861872E+03 0.31031374129805E+03
 0.31031374129805E+03 0.29215050130113E+03 0.29215048869113E+03 0.31031817869361E+03 0.31031817869361E+03
 0.29215050122685E+03 0.29215048861872E+03 0.31031374129805E+03 0.31031374129805E+03 0.29215050130113E+03
 0.29215048869113E+03 0.30775840581749E+03 0.29215181591029E+03 -.94799408084318E+02 -.12821616006563E+03
 0.24192197168542E+03 0.55537686630883E+03 0.31224528476498E+03 0.26070600915758E+03 0.22394200999211E+03
 0.26070600915758E+03 0.43711403883070E+03 0.26072676506120E+03 0.22377704067914E+03 0.26072676506120E+03
 0.43698448124820E+03 0.26070600915758E+03 0.22394200999211E+03 0.26070600915758E+03 0.43711403883070E+03
 0.26072676506120E+03 0.22377704067914E+03 0.26072676506120E+03 0.43698448124820E+03 0.23363723423768E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37079245092569E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16716153505613E+00 0.00000000000000E+00 0.00000000000000E+00 0.16716153505613E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17498285569564E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17498285569564E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256102408954E+00 0.17725710846145E+00 0.34267475996976E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1121.86551812
 0.98907165967119E+00 0.31817236336343E+03 0.47813029248804E+03 0.42539367434242E+03 0.41014872311162E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13364107774473E+00 0.00000000000000E+00 -.18543975640509E+02
 0.91804495253903E-03 0.12755129337205E+01 0.80000000000000E+04 0.30000000000000E+04 0.62719865777175E+01
 0.23519949666441E+01 0.34340065756296E+03 0.29215732420879E+03 0.34199644767872E+03 0.39064649788934E+03
 0.29215169101061E+03 0.29215410358981E+03 0.33773222947326E+03 0.39061829842796E+03 0.29215145019708E+03
 0.29215409672892E+03 0.34199644767872E+03 0.39064649788934E+03 0.29215169101061E+03 0.29215410358981E+03
 0.33773222947326E+03 0.39061829842796E+03 0.29215145019708E+03 0.29215409672892E+03 0.44084009233446E+03
 0.35282704312947E+03 0.18023284646731E+04 0.16762308978465E+04 0.63811429297299E+03 0.97557477013354E+03
 0.33426990569569E+03 0.11987679400515E+04 0.13938480252403E+04 0.11037797139839E+04 0.20267921718390E+04
 0.10998227036437E+04 0.13933766221425E+04 0.10263228581427E+04 0.20265710270550E+04 0.11987679400515E+04
 0.13938480252403E+04 0.11037797139839E+04 0.20267921718390E+04 0.10998227036437E+04 0.13933766221425E+04
 0.10263228581427E+04 0.20265710270550E+04 0.16637591637192E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55286633690579E+03 0.12948411202586E+01
 0.12948411202586E+01 0.43247098171511E+01 0.00000000000000E+00 0.34291758560390E+03 0.34291758560390E+03
 0.34291758560390E+03 0.34291758560390E+03 0.00000000000000E+00 0.00000000000000E+00 0.13756822818898E+00
 0.00000000000000E+00 -.11215947659484E+02 0.10000000000000E-02 0.17058702623828E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46896884109026E+01 0.17586331540885E+01 0.35283114536504E+03 0.44083561380351E+03
 0.31047446837169E+03 0.31047446837169E+03 0.29215054439004E+03 0.29215053069616E+03 0.31046997607873E+03
 0.31046997607873E+03 0.29215054446895E+03 0.29215053077308E+03 0.31047446837169E+03 0.31047446837169E+03
 0.29215054439004E+03 0.29215053069616E+03 0.31046997607873E+03 0.31046997607873E+03 0.29215054446895E+03
 0.29215053077308E+03 0.30789557738969E+03 0.29215195317323E+03 -.97585016116241E+02 -.13233590276995E+03
 0.24316790799205E+03 0.55767077597125E+03 0.31328702843924E+03 0.26299952421778E+03 0.22502803089934E+03
 0.26299952421778E+03 0.43882176240378E+03 0.26302060088009E+03 0.22486188434262E+03 0.26302060088009E+03
 0.43869147616678E+03 0.26299952421778E+03 0.22502803089934E+03 0.26299952421778E+03 0.43882176240378E+03
 0.26302060088009E+03 0.22486188434261E+03 0.26302060088009E+03 0.43869147616678E+03 0.23371126394101E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37105209631002E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16699406279270E+00 0.00000000000000E+00 0.00000000000000E+00 0.16699406279270E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17482577490649E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17482577490649E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256121143477E+00 0.17713182171156E+00 0.34291758560390E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1133.06405573
 0.98896568179957E+00 0.31838186393995E+03 0.47846175836488E+03 0.42569058438743E+03 0.41043324690797E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13355665398401E+00 0.00000000000000E+00 -.18549568351591E+02
 0.91744081300076E-03 0.12757457792314E+01 0.80000000000000E+04 0.30000000000000E+04 0.62708418324689E+01
 0.23515656871758E+01 0.34374466512313E+03 0.29215786334574E+03 0.34232606531516E+03 0.39115848018410E+03
 0.29215183265460E+03 0.29215444148907E+03 0.33804870455569E+03 0.39113033915090E+03 0.29215157259788E+03
 0.29215443413666E+03 0.34232606531516E+03 0.39115848018410E+03 0.29215183265460E+03 0.29215444148907E+03
 0.33804870455569E+03 0.39113033915089E+03 0.29215157259788E+03 0.29215443413666E+03 0.44135801756723E+03
 0.35335531293509E+03 0.18055241763269E+04 0.16785962015922E+04 0.63629342815261E+03 0.97169568792871E+03
 0.33222079263534E+03 0.12008466717345E+04 0.13949479552138E+04 0.11052715954491E+04 0.20260822252560E+04
 0.11020621876548E+04 0.13944782980081E+04 0.10280668279170E+04 0.20258620750527E+04 0.12008466717345E+04
 0.13949479552138E+04 0.11052715954491E+04 0.20260822252560E+04 0.11020621876548E+04 0.13944782980081E+04
 0.10280668279170E+04 0.20258620750527E+04 0.16634457185616E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55320171677327E+03 0.12948411614652E+01
 0.12948411614652E+01 0.43695039675702E+01 0.00000000000000E+00 0.34315721828907E+03 0.34315721828907E+03
 0.34315721828907E+03 0.34315721828907E+03 0.00000000000000E+00 0.00000000000000E+00 0.13736336044499E+00
 0.00000000000000E+00 -.11202019435769E+02 0.10000000000000E-02 0.17084627507468E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46825720938328E+01 0.17559645351873E+01 0.35335944488849E+03 0.44135353308107E+03
 0.31063058261210E+03 0.31063058261210E+03 0.29215059049943E+03 0.29215057564568E+03 0.31062603538725E+03
 0.31062603538725E+03 0.29215059058311E+03 0.29215057572726E+03 0.31063058261210E+03 0.31063058261210E+03
 0.29215059049943E+03 0.29215057564568E+03 0.31062603538725E+03 0.31062603538725E+03 0.29215059058311E+03
 0.29215057572726E+03 0.30803264986780E+03 0.29215209838592E+03 -.10036100301674E+03 -.13644457692361E+03
 0.24439050046857E+03 0.55991376698587E+03 0.31430131401496E+03 0.26526544535213E+03 0.22609073461802E+03
 0.26526544535213E+03 0.44048724718441E+03 0.26528684465958E+03 0.22592341571605E+03 0.26528684465958E+03
 0.44035623762184E+03 0.26526544535213E+03 0.22609073461802E+03 0.26526544535213E+03 0.44048724718441E+03
 0.26528684465958E+03 0.22592341571605E+03 0.26528684465958E+03 0.44035623762183E+03 0.23376305862276E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37131139799646E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16689540278752E+00 0.00000000000000E+00 0.00000000000000E+00 0.16689540278752E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17471055376077E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17471055376077E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256115292437E+00 0.17700809178829E+00 0.34315721828907E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1140.52974746
 0.98889777467726E+00 0.31852118634924E+03 0.47868202739910E+03 0.42588779429755E+03 0.41062223453241E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13350097195522E+00 0.00000000000000E+00 -.18554186049112E+02
 0.91703947901070E-03 0.12758947615294E+01 0.80000000000000E+04 0.30000000000000E+04 0.62701096055999E+01
 0.23512911021000E+01 0.34397337501686E+03 0.29215823949805E+03 0.34254520343102E+03 0.39149813191922E+03
 0.29215193225618E+03 0.29215467880363E+03 0.33825917331511E+03 0.39147002934746E+03 0.29215165871084E+03
 0.29215467110928E+03 0.34254520343102E+03 0.39149813191922E+03 0.29215193225618E+03 0.29215467880363E+03
 0.33825917331511E+03 0.39147002934746E+03 0.29215165871084E+03 0.29215467110928E+03 0.44170097378514E+03
 0.35370421516813E+03 0.18076472677139E+04 0.16801670633951E+04 0.63509427201084E+03 0.96914338385648E+03
 0.33087364048558E+03 0.12022285681034E+04 0.13956794028706E+04 0.11062633074396E+04 0.20256175073288E+04
 0.11035508552524E+04 0.13952109052661E+04 0.10292256869572E+04 0.20253980218262E+04 0.12022285681034E+04
 0.13956794028706E+04 0.11062633074396E+04 0.20256175073288E+04 0.11035508552524E+04 0.13952109052661E+04
 0.10292256869572E+04 0.20253980218262E+04 0.16632456481160E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55342424424585E+03 0.12948411954879E+01
 0.12948411954879E+01 0.43993667345162E+01 0.00000000000000E+00 0.34331646388654E+03 0.34331646388654E+03
 0.34331646388654E+03 0.34331646388654E+03 0.00000000000000E+00 0.00000000000000E+00 0.13723026496890E+00
 0.00000000000000E+00 -.11193730161799E+02 0.10000000000000E-02 0.17101272163117E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46780145498497E+01 0.17542554561936E+01 0.35370836480405E+03 0.44169648829746E+03
 0.31073457068091E+03 0.31073457068091E+03 0.29215062294718E+03 0.29215060727723E+03 0.31072998682090E+03
 0.31072998682090E+03 0.29215062303413E+03 0.29215060736199E+03 0.31073457068091E+03 0.31073457068091E+03
 0.29215062294718E+03 0.29215060727723E+03 0.31072998682090E+03 0.31072998682090E+03 0.29215062303413E+03
 0.29215060736199E+03 0.30812398058967E+03 0.29215219976299E+03 -.10219685818546E+03 -.13916204913983E+03
 0.24520235492586E+03 0.56140135052070E+03 0.31497298382021E+03 0.26676616705298E+03 0.22679599539550E+03
 0.26676616705298E+03 0.44159133330506E+03 0.26678778234426E+03 0.22662789592979E+03 0.26678778234426E+03
 0.44145984268948E+03 0.26676616705298E+03 0.22679599539550E+03 0.26676616705298E+03 0.44159133330506E+03
 0.26678778234426E+03 0.22662789592979E+03 0.26678778234426E+03 0.44145984268947E+03 0.23379905344906E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37148382770793E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16683811154084E+00 0.00000000000000E+00 0.00000000000000E+00 0.16683811154084E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17463580080950E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17463580080950E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256108645058E+00 0.17692593380983E+00 0.34331646388654E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1151.72828507
 0.98880142923199E+00 0.31872957211600E+03 0.47901093695706E+03 0.42618212274570E+03 0.41090433319056E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13341847438276E+00 0.00000000000000E+00 -.18561226492264E+02
 0.91643985383819E-03 0.12761083982651E+01 0.80000000000000E+04 0.30000000000000E+04 0.62690599097037E+01
 0.23508974661389E+01 0.34431547681917E+03 0.29215882964368E+03 0.34287298988562E+03 0.39200513627114E+03
 0.29215208974183E+03 0.29215505357563E+03 0.33857409260670E+03 0.39197709067013E+03 0.29215179493633E+03
 0.29215504534637E+03 0.34287298988562E+03 0.39200513627114E+03 0.29215208974183E+03 0.29215505357563E+03
 0.33857409260670E+03 0.39197709067013E+03 0.29215179493633E+03 0.29215504534637E+03 0.44221193048559E+03
 0.35422282793273E+03 0.18108137750912E+04 0.16825068783861E+04 0.63330872550025E+03 0.96535187728853E+03
 0.32887660816078E+03 0.12042913314466E+04 0.13967644592029E+04 0.11077420037919E+04 0.20249201520115E+04
 0.11057733071912E+04 0.13962976931352E+04 0.10309538917122E+04 0.20247016642896E+04 0.12042913314466E+04
 0.13967644592029E+04 0.11077420037919E+04 0.20249201520115E+04 0.11057733071912E+04 0.13962976931352E+04
 0.10309538917122E+04 0.20247016642896E+04 0.16629473704315E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55375584430267E+03 0.12948412473613E+01
 0.12948412473613E+01 0.44441608849353E+01 0.00000000000000E+00 0.34355463595712E+03 0.34355463595712E+03
 0.34355463595712E+03 0.34355463595712E+03 0.00000000000000E+00 0.00000000000000E+00 0.13703564367592E+00
 0.00000000000000E+00 -.11181541077358E+02 0.10000000000000E-02 0.17125317426327E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46714462575167E+01 0.17517923465688E+01 0.35422700072894E+03 0.44220744673127E+03
 0.31089045298524E+03 0.31089045298524E+03 0.29215067429081E+03 0.29215065732933E+03 0.31088581414914E+03
 0.31088581414914E+03 0.29215067438274E+03 0.29215065741895E+03 0.31089045298524E+03 0.31089045298524E+03
 0.29215067429081E+03 0.29215065732933E+03 0.31088581414914E+03 0.31088581414914E+03 0.29215067438274E+03
 0.29215065741895E+03 0.30826092549453E+03 0.29215235891297E+03 -.10492897068402E+03 -.14320621277111E+03
 0.24641576872541E+03 0.56362171277931E+03 0.31597386521026E+03 0.26900303571394E+03 0.22784946748266E+03
 0.26900303571394E+03 0.44323854221970E+03 0.26902497632083E+03 0.22768019836318E+03 0.26902497632083E+03
 0.44310633145391E+03 0.26900303571394E+03 0.22784946748266E+03 0.26900303571394E+03 0.44323854221970E+03
 0.26902497632083E+03 0.22768019836318E+03 0.26902497632083E+03 0.44310633145391E+03 0.23385541737598E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37174168426896E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16675406462847E+00 0.00000000000000E+00 0.00000000000000E+00 0.16675406462847E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17452493844463E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17452493844463E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256098152936E+00 0.17680319223252E+00 0.34355463595712E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1162.92682267
 0.98870927401846E+00 0.31893728105842E+03 0.47933787366010E+03 0.42647468917232E+03 0.41118484859584E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13333730082168E+00 0.00000000000000E+00 -.18568183948438E+02
 0.91584295650272E-03 0.12763095039347E+01 0.80000000000000E+04 0.30000000000000E+04 0.62680721058151E+01
 0.23505270396807E+01 0.34465641715265E+03 0.29215945189299E+03 0.34319966876668E+03 0.39250918031937E+03
 0.29215225732622E+03 0.29215545179887E+03 0.33888806632141E+03 0.39248119085665E+03 0.29215193998215E+03
 0.29215544300762E+03 0.34319966876668E+03 0.39250918031937E+03 0.29215225732622E+03 0.29215545179887E+03
 0.33888806632141E+03 0.39248119085665E+03 0.29215193998215E+03 0.29215544300762E+03 0.44271868615684E+03
 0.35473585892022E+03 0.18139572273493E+04 0.16848259509773E+04 0.63153620131853E+03 0.96160036928942E+03
 0.32690648696430E+03 0.12063407947733E+04 0.13978312519590E+04 0.11092090755058E+04 0.20242170030680E+04
 0.11079819305606E+04 0.13973662086290E+04 0.10326690486236E+04 0.20239995141733E+04 0.12063407947733E+04
 0.13978312519590E+04 0.11092090755058E+04 0.20242170030680E+04 0.11079819305606E+04 0.13973662086290E+04
 0.10326690486236E+04 0.20239995141733E+04 0.16626480925239E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55408500472147E+03 0.12948412986232E+01
 0.12948412986232E+01 0.44889550353544E+01 0.00000000000000E+00 0.34379186508475E+03 0.34379186508475E+03
 0.34379186508475E+03 0.34379186508475E+03 0.00000000000000E+00 0.00000000000000E+00 0.13684683217000E+00
 0.00000000000000E+00 -.11169412761763E+02 0.10000000000000E-02 0.17148304197090E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46651843284641E+01 0.17494441231740E+01 0.35474005129783E+03 0.44271420648761E+03
 0.31104621155757E+03 0.31104621155757E+03 0.29215072897572E+03 0.29215071063866E+03 0.31104151771711E+03
 0.31104151771711E+03 0.29215072907275E+03 0.29215071073325E+03 0.31104621155757E+03 0.31104621155757E+03
 0.29215072897572E+03 0.29215071063866E+03 0.31104151771711E+03 0.31104151771711E+03 0.29215072907275E+03
 0.29215071073325E+03 0.30839780522094E+03 0.29215252684583E+03 -.10763629314327E+03 -.14721348121423E+03
 0.24762316785054E+03 0.56582695039175E+03 0.31696566670195E+03 0.27122263435921E+03 0.22889686867525E+03
 0.27122263435921E+03 0.44487344102338E+03 0.27124490189632E+03 0.22872643153411E+03 0.27124490189632E+03
 0.44474051199358E+03 0.27122263435921E+03 0.22889686867525E+03 0.27122263435921E+03 0.44487344102338E+03
 0.27124490189632E+03 0.22872643153411E+03 0.27124490189632E+03 0.44474051199359E+03 0.23391331342604E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37199845128582E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16667025177872E+00 0.00000000000000E+00 0.00000000000000E+00 0.16667025177872E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17441530110729E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17441530110729E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256087729153E+00 0.17668110590898E+00 0.34379186508475E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1170.39251441
 0.98864943319427E+00 0.31907539048248E+03 0.47955471545094E+03 0.42666878422778E+03 0.41137101940754E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13328393924791E+00 0.00000000000000E+00 -.18572755020921E+02
 0.91544649934314E-03 0.12764364383871E+01 0.80000000000000E+04 0.30000000000000E+04 0.62674487811618E+01
 0.23502932929357E+01 0.34488306701969E+03 0.29215988512453E+03 0.34341684156373E+03 0.39284357407094E+03
 0.29215237488560E+03 0.29215573081350E+03 0.33909685772694E+03 0.39281562159001E+03 0.29215204177976E+03
 0.29215572163217E+03 0.34341684156373E+03 0.39284357407094E+03 0.29215237488560E+03 0.29215573081350E+03
 0.33909685772694E+03 0.39281562159001E+03 0.29215204177976E+03 0.29215572163217E+03 0.44305419106882E+03
 0.35507482725934E+03 0.18160401135087E+04 0.16863606205972E+04 0.63036154351936E+03 0.95912113611904E+03
 0.32560778488208E+03 0.12076996307873E+04 0.13985318368609E+04 0.11101806383761E+04 0.20237442459235E+04
 0.11094465843329E+04 0.13980679373411E+04 0.10338052182645E+04 0.20235274234887E+04 0.12076996307873E+04
 0.13985318368609E+04 0.11101806383761E+04 0.20237442459235E+04 0.11094465843329E+04 0.13980679373411E+04
 0.10338052182645E+04 0.20235274234887E+04 0.16624477708698E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55430317789729E+03 0.12948413323024E+01
 0.12948413323024E+01 0.45188178023005E+01 0.00000000000000E+00 0.34394946703463E+03 0.34394946703463E+03
 0.34394946703463E+03 0.34394946703463E+03 0.00000000000000E+00 0.00000000000000E+00 0.13672407124825E+00
 0.00000000000000E+00 -.11161335279085E+02 0.10000000000000E-02 0.17163064272221E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46611723134710E+01 0.17479396175516E+01 0.35507903105481E+03 0.44304971505295E+03
 0.31114997884631E+03 0.31114997884631E+03 0.29215076736513E+03 0.29215074806241E+03 0.31114524832106E+03
 0.31114524832106E+03 0.29215076746563E+03 0.29215074816037E+03 0.31114997884631E+03 0.31114997884631E+03
 0.29215076736513E+03 0.29215074806241E+03 0.31114524832106E+03 0.31114524832106E+03 0.29215076746563E+03
 0.29215074816037E+03 0.30848901930850E+03 0.29215264383882E+03 -.10942774121722E+03 -.14986484646460E+03
 0.24842458133603E+03 0.56728826589931E+03 0.31762156165659E+03 0.27269275570729E+03 0.22959159003587E+03
 0.27269275570729E+03 0.44595616803135E+03 0.27271524209188E+03 0.22942037522701E+03 0.27271524209188E+03
 0.44582276130014E+03 0.27269275570729E+03 0.22959159003587E+03 0.27269275570729E+03 0.44595616803135E+03
 0.27271524209188E+03 0.22942037522701E+03 0.27271524209188E+03 0.44582276130015E+03 0.23395246226599E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37216899220223E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16661429099720E+00 0.00000000000000E+00 0.00000000000000E+00 0.16661429099720E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17434281740703E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17434281740703E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256080896962E+00 0.17660009257896E+00 0.34394946703463E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1181.59105201
 0.98856129992839E+00 0.31928202674369E+03 0.47987831801672E+03 0.42695855852854E+03 0.41164907755516E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13320502784741E+00 0.00000000000000E+00 -.18579519168800E+02
 0.91485397007696E-03 0.12766161211779E+01 0.80000000000000E+04 0.30000000000000E+04 0.62665666423034E+01
 0.23499624908638E+01 0.34522208228503E+03 0.29216056342428E+03 0.34374168735835E+03 0.39334273081032E+03
 0.29215256032567E+03 0.29215617040473E+03 0.33940926261692E+03 0.39331483316367E+03 0.29215220243379E+03
 0.29215616061454E+03 0.34374168735835E+03 0.39334273081032E+03 0.29215256032567E+03 0.29215617040473E+03
 0.33940926261692E+03 0.39331483316367E+03 0.29215220243379E+03 0.29215616061454E+03 0.44355396877470E+03
 0.35557876741695E+03 0.18191458584276E+04 0.16886461836432E+04 0.62861042765048E+03 0.95543518531525E+03
 0.32368170552652E+03 0.12097268815436E+04 0.13995669687945E+04 0.11116285215870E+04 0.20230291588567E+04
 0.11116321275915E+04 0.13991047783778E+04 0.10354988409251E+04 0.20228133370967E+04 0.12097268815436E+04
 0.13995669687945E+04 0.11116285215870E+04 0.20230291588567E+04 0.11116321275915E+04 0.13991047783778E+04
 0.10354988409251E+04 0.20228133370967E+04 0.16621465066516E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55462866130747E+03 0.12948413821400E+01
 0.12948413821400E+01 0.45636119527196E+01 0.00000000000000E+00 0.34418501189609E+03 0.34418501189609E+03
 0.34418501189609E+03 0.34418501189609E+03 0.00000000000000E+00 0.00000000000000E+00 0.13654443742688E+00
 0.00000000000000E+00 -.11149233885120E+02 0.10000000000000E-02 0.17184390997029E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46553875557087E+01 0.17457703333908E+01 0.35558298624387E+03 0.44354949953784E+03
 0.31130551709955E+03 0.31130551709955E+03 0.29215082796591E+03 0.29215080713880E+03 0.31130073152533E+03
 0.31130073152533E+03 0.29215082807168E+03 0.29215080724190E+03 0.31130551709955E+03 0.31130551709955E+03
 0.29215082796591E+03 0.29215080713880E+03 0.31130073152533E+03 0.31130073152533E+03 0.29215082807168E+03
 0.29215080724190E+03 0.30862577722607E+03 0.29215282712552E+03 -.11209515889314E+03 -.15381205102477E+03
 0.24962121790915E+03 0.56946647937761E+03 0.31859715537892E+03 0.27488350936570E+03 0.23062815630265E+03
 0.27488350936570E+03 0.44756904080350E+03 0.27490632535572E+03 0.23045577668091E+03 0.27490632535572E+03
 0.44743491938130E+03 0.27488350936570E+03 0.23062815630265E+03 0.27488350936570E+03 0.44756904080350E+03
 0.27490632535572E+03 0.23045577668091E+03 0.27490632535572E+03 0.44743491938129E+03 0.23401168047507E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37242381766902E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16653026110994E+00 0.00000000000000E+00 0.00000000000000E+00 0.16653026110994E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17423493418125E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17423493418125E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256070821668E+00 0.17647915340323E+00 0.34418501189609E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1194.72939221
 0.98849177341808E+00 0.31952107705029E+03 0.48025288399298E+03 0.42729219436316E+03 0.41196894043334E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13311480093096E+00 0.00000000000000E+00 -.18607173879702E+02
 0.91416927077392E-03 0.12768088889846E+01 0.80000000000000E+04 0.30000000000000E+04 0.62656205396267E+01
 0.23496077023600E+01 0.34561495742200E+03 0.29216142256290E+03 0.34411825476417E+03 0.39392554398856E+03
 0.29215279842521E+03 0.29215673356611E+03 0.33977100660838E+03 0.39389770750854E+03 0.29215240888974E+03
 0.29215672300930E+03 0.34411825476417E+03 0.39392554398856E+03 0.29215279842521E+03 0.29215673356611E+03
 0.33977100660838E+03 0.39389770750854E+03 0.29215240888974E+03 0.29215672300930E+03 0.44414950812050E+03
 0.35617994886555E+03 0.18227308999681E+04 0.16912770204420E+04 0.62625844918605E+03 0.95066666374105E+03
 0.32127692230907E+03 0.12120723358745E+04 0.14006958675642E+04 0.11132993952875E+04 0.20220923602858E+04
 0.11141658601854E+04 0.14002355121632E+04 0.10374614596591E+04 0.20218775719319E+04 0.12120723358745E+04
 0.14006958675642E+04 0.11132993952875E+04 0.20220923602858E+04 0.11141658601854E+04 0.14002355121632E+04
 0.10374614596591E+04 0.20218775719319E+04 0.16616232485254E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55500070185420E+03 0.12948415858975E+01
 0.12948415858975E+01 0.46161653135077E+01 0.00000000000000E+00 0.34446156335197E+03 0.34446156335197E+03
 0.34446156335197E+03 0.34446156335197E+03 0.00000000000000E+00 0.00000000000000E+00 0.13634030059561E+00
 0.00000000000000E+00 -.11157463424502E+02 0.10000000000000E-02 0.17208142689808E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46489619154182E+01 0.17433607182818E+01 0.35618413329414E+03 0.44414509083298E+03
 0.31148465708949E+03 0.31148465708949E+03 0.29215092871301E+03 0.29215088309470E+03 0.31147980778604E+03
 0.31147980778604E+03 0.29215092882791E+03 0.29215088320396E+03 0.31148465708949E+03 0.31148465708949E+03
 0.29215092871301E+03 0.29215088309470E+03 0.31147980778604E+03 0.31147980778604E+03 0.29215092882791E+03
 0.29215088320396E+03 0.30878317263782E+03 0.29215305954491E+03 -.11533875251245E+03 -.15861566592343E+03
 0.25104996401213E+03 0.57209140656413E+03 0.31978619273194E+03 0.27752610320119E+03 0.23186886350666E+03
 0.27752610320119E+03 0.44952292964995E+03 0.27754929925185E+03 0.23169498591168E+03 0.27754929925185E+03
 0.44938783404219E+03 0.27752610320119E+03 0.23186886350666E+03 0.27752610320119E+03 0.44952292964995E+03
 0.27754929925185E+03 0.23169498591168E+03 0.27754929925185E+03 0.44938783404219E+03 0.23407484517582E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37272798307324E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16661371812799E+00 0.00000000000000E+00 0.00000000000000E+00 0.16661371812799E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17419723702603E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17419723702603E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255994196194E+00 0.17633666951004E+00 0.34446156335197E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1200.68021966
 0.98843500770674E+00 0.31963094210957E+03 0.48042456480597E+03 0.42744654957626E+03 0.41211718944720E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13307469801477E+00 0.00000000000000E+00 -.18585777194638E+02
 0.91385524120130E-03 0.12768810752914E+01 0.80000000000000E+04 0.30000000000000E+04 0.62652663233924E+01
 0.23494748712722E+01 0.34579596091619E+03 0.29216181721121E+03 0.34429163709475E+03 0.39418606910637E+03
 0.29215290806468E+03 0.29215699278483E+03 0.33993829880909E+03 0.39415826208865E+03 0.29215250397298E+03
 0.29215698187626E+03 0.34429163709475E+03 0.39418606910637E+03 0.29215290806468E+03 0.29215699278483E+03
 0.33993829880909E+03 0.39415826208865E+03 0.29215250397298E+03 0.29215698187626E+03 0.44439700533626E+03
 0.35642782005942E+03 0.18243718300425E+04 0.16924761625374E+04 0.62562287389628E+03 0.94919355521987E+03
 0.32044256695411E+03 0.12131439755372E+04 0.14012785250558E+04 0.11140589779580E+04 0.20217859890926E+04
 0.11153182862110E+04 0.14008191942433E+04 0.10383459476855E+04 0.20215718503543E+04 0.12131439755372E+04
 0.14012785250558E+04 0.11140589779580E+04 0.20217859890926E+04 0.11153182862110E+04 0.14008191942433E+04
 0.10383459476855E+04 0.20215718503543E+04 0.16616050415760E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55517486989431E+03 0.12948414282486E+01
 0.12948414282486E+01 0.46399686233165E+01 0.00000000000000E+00 0.34458592323987E+03 0.34458592323987E+03
 0.34458592323987E+03 0.34458592323987E+03 0.00000000000000E+00 0.00000000000000E+00 0.13625015087761E+00
 0.00000000000000E+00 -.11123685740216E+02 0.10000000000000E-02 0.17218513997683E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46461616844965E+01 0.17423106316862E+01 0.35643204732350E+03 0.44439255597818E+03
 0.31156962972709E+03 0.31156962972709E+03 0.29215092706398E+03 0.29215091807926E+03 0.31156475031969E+03
 0.31156475031969E+03 0.29215092717713E+03 0.29215091819131E+03 0.31156962972709E+03 0.31156962972709E+03
 0.29215092706398E+03 0.29215091807926E+03 0.31156475031969E+03 0.31156475031969E+03 0.29215092717713E+03
 0.29215091819131E+03 0.30885805130625E+03 0.29215316632998E+03 -.11659502061201E+03 -.16046735028044E+03
 0.25166448690428E+03 0.57318523415986E+03 0.32026242482105E+03 0.27859980354963E+03 0.23239866448065E+03
 0.27859980354963E+03 0.45032497865187E+03 0.27862318395159E+03 0.23222428826480E+03 0.27862318395159E+03
 0.45018962840550E+03 0.27859980354962E+03 0.23239866448065E+03 0.27859980354962E+03 0.45032497865187E+03
 0.27862318395159E+03 0.23222428826480E+03 0.27862318395159E+03 0.45018962840550E+03 0.23413354369706E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37285668063916E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16634271442248E+00 0.00000000000000E+00 0.00000000000000E+00 0.16634271442248E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17404994049145E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17404994049145E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256068129073E+00 0.17627384423288E+00 0.34458592323987E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1210.19591914
 0.98835007385202E+00 0.31980611976845E+03 0.48069633324827E+03 0.42769104845665E+03 0.41235227045819E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13301087631993E+00 0.00000000000000E+00 -.18594579925370E+02
 0.91335458660875E-03 0.12769991623822E+01 0.80000000000000E+04 0.30000000000000E+04 0.62646869596034E+01
 0.23492576098513E+01 0.34608234313790E+03 0.29216247059447E+03 0.34456604027572E+03 0.39460354254570E+03
 0.29215309068441E+03 0.29215742411893E+03 0.34020258386584E+03 0.39457578106631E+03 0.29215266240878E+03
 0.29215741262955E+03 0.34456604027572E+03 0.39460354254570E+03 0.29215309068441E+03 0.29215742411893E+03
 0.34020258386584E+03 0.39457578106631E+03 0.29215266240878E+03 0.29215741262955E+03 0.44480913376935E+03
 0.35683989995552E+03 0.18269801475962E+04 0.16943944573368E+04 0.62424913727672E+03 0.94628159176661E+03
 0.31891120880351E+03 0.12148471674624E+04 0.14021428711812E+04 0.11152750908363E+04 0.20211999268847E+04
 0.11171538119824E+04 0.14016850039998E+04 0.10397661422561E+04 0.20209866647270E+04 0.12148471674624E+04
 0.14021428711811E+04 0.11152750908363E+04 0.20211999268847E+04 0.11171538119824E+04 0.14016850039998E+04
 0.10397661422561E+04 0.20209866647270E+04 0.16614033694963E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55544998977360E+03 0.12948414931063E+01
 0.12948414931063E+01 0.46780314212112E+01 0.00000000000000E+00 0.34478332421318E+03 0.34478332421318E+03
 0.34478332421318E+03 0.34478332421318E+03 0.00000000000000E+00 0.00000000000000E+00 0.13610887664901E+00
 0.00000000000000E+00 -.11116766956010E+02 0.10000000000000E-02 0.17234617668822E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46418204068851E+01 0.17406826525819E+01 0.35684420599084E+03 0.44480463450474E+03
 0.31170231787639E+03 0.31170231787639E+03 0.29215098594164E+03 0.29215097638630E+03 0.31169739148077E+03
 0.31169739148077E+03 0.29215098605934E+03 0.29215097650285E+03 0.31170231787639E+03 0.31170231787639E+03
 0.29215098594164E+03 0.29215097638630E+03 0.31169739148077E+03 0.31169739148077E+03 0.29215098605934E+03
 0.29215097650285E+03 0.30897485739398E+03 0.29215334321605E+03 -.11877292895987E+03 -.16368603544729E+03
 0.25265752226291E+03 0.57497784701794E+03 0.32105703714371E+03 0.28039836370754E+03 0.23325559841859E+03
 0.28039836370754E+03 0.45164692877474E+03 0.28042202953905E+03 0.23308026828102E+03 0.28042202953905E+03
 0.45151100851029E+03 0.28039836370754E+03 0.23325559841859E+03 0.28039836370754E+03 0.45164692877474E+03
 0.28042202953905E+03 0.23308026828102E+03 0.28042202953905E+03 0.45151100851029E+03 0.23418780297764E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37307073054041E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16629989435069E+00 0.00000000000000E+00 0.00000000000000E+00 0.16629989435069E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17396580171178E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17396580171178E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256050501987E+00 0.17617275313039E+00 0.34478332421318E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1222.73698166
 0.98825762034871E+00 0.32003523207673E+03 0.48105069556935E+03 0.42800910930583E+03 0.41265809996483E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13292790194023E+00 0.00000000000000E+00 -.18608320822606E+02
 0.91270059487371E-03 0.12771496953427E+01 0.80000000000000E+04 0.30000000000000E+04 0.62639485638787E+01
 0.23489807114545E+01 0.34645726549159E+03 0.29216337829737E+03 0.34492533265884E+03 0.39515144768740E+03
 0.29215334673478E+03 0.29215802796164E+03 0.34054850642946E+03 0.39512374471862E+03 0.29215288468299E+03
 0.29215801566885E+03 0.34492533265884E+03 0.39515144768740E+03 0.29215334673478E+03 0.29215802796164E+03
 0.34054850642946E+03 0.39512374471862E+03 0.29215288468299E+03 0.29215801566885E+03 0.44535457862716E+03
 0.35738529640074E+03 0.18303700333823E+04 0.16968787261611E+04 0.62229844103700E+03 0.94224276605563E+03
 0.31683283281344E+03 0.12170648360683E+04 0.14032194725224E+04 0.11168532611812E+04 0.20203574101285E+04
 0.11195471166513E+04 0.14027634680388E+04 0.10416145485243E+04 0.20201452515711E+04 0.12170648360683E+04
 0.14032194725224E+04 0.11168532611812E+04 0.20203574101285E+04 0.11195471166513E+04 0.14027634680388E+04
 0.10416145485243E+04 0.20201452515711E+04 0.16610420690072E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55580614934706E+03 0.12948415943481E+01
 0.12948415943481E+01 0.47281956712860E+01 0.00000000000000E+00 0.34504258134783E+03 0.34504258134783E+03
 0.34504258134783E+03 0.34504258134783E+03 0.00000000000000E+00 0.00000000000000E+00 0.13592787353094E+00
 0.00000000000000E+00 -.11110502519230E+02 0.10000000000000E-02 0.17254904192898E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46363630365984E+01 0.17386361387244E+01 0.35738956611096E+03 0.44535013801257E+03
 0.31187562095471E+03 0.31187562095471E+03 0.29215108495675E+03 0.29215105821486E+03 0.31187063294399E+03
 0.31187063294399E+03 0.29215108508240E+03 0.29215105833741E+03 0.31187562095471E+03 0.31187562095471E+03
 0.29215108495675E+03 0.29215105821486E+03 0.31187063294399E+03 0.31187063294399E+03 0.29215108508240E+03
 0.29215105833741E+03 0.30912738814826E+03 0.29215358914965E+03 -.12168913793919E+03 -.16799730294212E+03
 0.25397060729595E+03 0.57735227534272E+03 0.32211181501029E+03 0.28279203053041E+03 0.23438919529697E+03
 0.28279203053041E+03 0.45340057832686E+03 0.28281607075881E+03 0.23421255196522E+03 0.28281607075881E+03
 0.45326384900899E+03 0.28279203053041E+03 0.23438919529697E+03 0.28279203053041E+03 0.45340057832686E+03
 0.28281607075881E+03 0.23421255196522E+03 0.28281607075881E+03 0.45326384900899E+03 0.23424895935606E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37335257662896E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16626515171838E+00 0.00000000000000E+00 0.00000000000000E+00 0.16626515171838E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17387274734354E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17387274734354E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256019256381E+00 0.17604007306903E+00 0.34504258134783E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1233.54791700
 0.98818717716692E+00 0.32023164488531E+03 0.48135407751888E+03 0.42828103689140E+03 0.41291956409382E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13285780817154E+00 0.00000000000000E+00 -.18614739616360E+02
 0.91214073579241E-03 0.12772660547113E+01 0.80000000000000E+04 0.30000000000000E+04 0.62633779160509E+01
 0.23487667185191E+01 0.34677950543221E+03 0.29216420114720E+03 0.34523414811057E+03 0.39562063988535E+03
 0.29215358090726E+03 0.29215857938857E+03 0.34084598854991E+03 0.39559298679547E+03 0.29215308808132E+03
 0.29215856637057E+03 0.34523414811057E+03 0.39562063988535E+03 0.29215358090726E+03 0.29215857938857E+03
 0.34084598854991E+03 0.39559298679547E+03 0.29215308808132E+03 0.29215856637057E+03 0.44581881663228E+03
 0.35784863562688E+03 0.18332613609970E+04 0.16989875390456E+04 0.62066073026444E+03 0.93884837162056E+03
 0.31508433770480E+03 0.12189593046802E+04 0.14041281505911E+04 0.11181949404035E+04 0.20196247004268E+04
 0.11215923100510E+04 0.14036737564363E+04 0.10431876339309E+04 0.20194135052411E+04 0.12189593046802E+04
 0.14041281505911E+04 0.11181949404035E+04 0.20196247004268E+04 0.11215923100510E+04 0.14036737564363E+04
 0.10431876339309E+04 0.20194135052411E+04 0.16607354813310E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55610984295747E+03 0.12948416416412E+01
 0.12948416416412E+01 0.47714394126526E+01 0.00000000000000E+00 0.34526560566884E+03 0.34526560566884E+03
 0.34526560566884E+03 0.34526560566884E+03 0.00000000000000E+00 0.00000000000000E+00 0.13577639377351E+00
 0.00000000000000E+00 -.11099427114330E+02 0.10000000000000E-02 0.17271555434531E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46318931901209E+01 0.17369599462953E+01 0.35785289156245E+03 0.44581440489870E+03
 0.31202513975993E+03 0.31202513975993E+03 0.29215116175421E+03 0.29215113311943E+03 0.31202009849574E+03
 0.31202009849574E+03 0.29215116188519E+03 0.29215113324717E+03 0.31202513975993E+03 0.31202513975993E+03
 0.29215116175421E+03 0.29215113311943E+03 0.31202009849574E+03 0.31202009849574E+03 0.29215116188519E+03
 0.29215113324717E+03 0.30925903870910E+03 0.29215381226728E+03 -.12415927764636E+03 -.17164576994032E+03
 0.25509872536014E+03 0.57938680055823E+03 0.32301258157129E+03 0.28483241026056E+03 0.23536264916765E+03
 0.28483241026056E+03 0.45490226942554E+03 0.28485677575692E+03 0.23518489113442E+03 0.28485677575692E+03
 0.45476486088255E+03 0.28483241026056E+03 0.23536264916765E+03 0.28483241026056E+03 0.45490226942554E+03
 0.28485677575692E+03 0.23518489113442E+03 0.28485677575692E+03 0.45476486088255E+03 0.23431203915080E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37359377890794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16618732260553E+00 0.00000000000000E+00 0.00000000000000E+00 0.16618732260553E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17377752088416E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17377752088416E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256008878262E+00 0.17592627341525E+00 0.34526560566884E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1240.75520722
 0.98813304252845E+00 0.32036250963844E+03 0.48155547001174E+03 0.42846210655579E+03 0.41309383522291E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13281175749206E+00 0.00000000000000E+00 -.18614027837346E+02
 0.91176814219500E-03 0.12773369766397E+01 0.80000000000000E+04 0.30000000000000E+04 0.62630301528151E+01
 0.23486363073057E+01 0.34699377144861E+03 0.29216477073457E+03 0.34543949177332E+03 0.39593196801627E+03
 0.29215374407959E+03 0.29215896319341E+03 0.34104385470940E+03 0.39590434787278E+03 0.29215322987083E+03
 0.29215894967505E+03 0.34543949177332E+03 0.39593196801627E+03 0.29215374407959E+03 0.29215896319341E+03
 0.34104385470940E+03 0.39590434787278E+03 0.29215322987083E+03 0.29215894967505E+03 0.44612612732680E+03
 0.35815480293111E+03 0.18351843121613E+04 0.17003916912942E+04 0.61958133606099E+03 0.93661357158254E+03
 0.31393432884124E+03 0.12202184223297E+04 0.14047278744577E+04 0.11190876371722E+04 0.20191366546749E+04
 0.11229515321549E+04 0.14042745555387E+04 0.10442337154881E+04 0.20189261071559E+04 0.12202184223297E+04
 0.14047278744577E+04 0.11190876371722E+04 0.20191366546749E+04 0.11229515321549E+04 0.14042745555387E+04
 0.10442337154881E+04 0.20189261071560E+04 0.16605384829194E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55631256592884E+03 0.12948416363968E+01
 0.12948416363968E+01 0.48002685735637E+01 0.00000000000000E+00 0.34541330612443E+03 0.34541330612443E+03
 0.34541330612443E+03 0.34541330612443E+03 0.00000000000000E+00 0.00000000000000E+00 0.13567769180701E+00
 0.00000000000000E+00 -.11086315869692E+02 0.10000000000000E-02 0.17282274907538E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46290202203129E+01 0.17358825826174E+01 0.35815905931697E+03 0.44612172602636E+03
 0.31212474093583E+03 0.31212474093583E+03 0.29215121530357E+03 0.29215118534891E+03 0.31211966416087E+03
 0.31211966416087E+03 0.29215121543810E+03 0.29215118548012E+03 0.31212474093583E+03 0.31212474093583E+03
 0.29215121530357E+03 0.29215118534891E+03 0.31211966416087E+03 0.31211966416087E+03 0.29215121543810E+03
 0.29215118548012E+03 0.30934676164576E+03 0.29215396680379E+03 -.12579652425927E+03 -.17406348589019E+03
 0.25584386046828E+03 0.58072324309734E+03 0.32360016332672E+03 0.28618151972981E+03 0.23600470451754E+03
 0.28618151972981E+03 0.45588653037739E+03 0.28620610292140E+03 0.23582620642839E+03 0.28620610292140E+03
 0.45574867158228E+03 0.28618151972981E+03 0.23600470451754E+03 0.28618151972981E+03 0.45588653037739E+03
 0.28620610292140E+03 0.23582620642839E+03 0.28620610292140E+03 0.45574867158228E+03 0.23434775778752E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37375166356927E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16609022662316E+00 0.00000000000000E+00 0.00000000000000E+00 0.16609022662316E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17368541181060E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17368541181060E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256018736928E+00 0.17585116969384E+00 0.34541330612443E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1250.18834308
 0.98804470860690E+00 0.32053422889566E+03 0.48181877139578E+03 0.42869999179667E+03 0.41332306938715E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13275187488724E+00 0.00000000000000E+00 -.18616365855064E+02
 0.91127966116797E-03 0.12774253034888E+01 0.80000000000000E+04 0.30000000000000E+04 0.62625970991422E+01
 0.23484739121783E+01 0.34727403301781E+03 0.29216554096535E+03 0.34570806231867E+03 0.39633731826166E+03
 0.29215396600511E+03 0.29215948467958E+03 0.34130280737548E+03 0.39630974118299E+03 0.29215342278637E+03
 0.29215947048656E+03 0.34570806231867E+03 0.39633731826166E+03 0.29215396600511E+03 0.29215948467958E+03
 0.34130280737548E+03 0.39630974118299E+03 0.29215342278637E+03 0.29215947048656E+03 0.44652316057382E+03
 0.35854916413411E+03 0.18377083208832E+04 0.17022405350170E+04 0.61824714741776E+03 0.93382080417499E+03
 0.31248242102014E+03 0.12218686118384E+04 0.14055260994805E+04 0.11202612388060E+04 0.20185277776205E+04
 0.11247315442617E+04 0.14050742237151E+04 0.10456058596726E+04 0.20183181158633E+04 0.12218686118384E+04
 0.14055260994805E+04 0.11202612388060E+04 0.20185277776205E+04 0.11247315442617E+04 0.14050742237151E+04
 0.10456058596726E+04 0.20183181158633E+04 0.16603311132672E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55658022948829E+03 0.12948416536232E+01
 0.12948416536232E+01 0.48380011169906E+01 0.00000000000000E+00 0.34560491242074E+03 0.34560491242074E+03
 0.34560491242074E+03 0.34560491242074E+03 0.00000000000000E+00 0.00000000000000E+00 0.13555120213513E+00
 0.00000000000000E+00 -.11073108494098E+02 0.10000000000000E-02 0.17295881331475E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46253786359194E+01 0.17345169884698E+01 0.35855342440419E+03 0.44651877102490E+03
 0.31225544083996E+03 0.31225544083996E+03 0.29215126126050E+03 0.29215125642716E+03 0.31225031744935E+03
 0.31225031744935E+03 0.29215126139677E+03 0.29215125656290E+03 0.31225544083996E+03 0.31225544083996E+03
 0.29215126126050E+03 0.29215125642716E+03 0.31225031744935E+03 0.31225031744935E+03 0.29215126139677E+03
 0.29215125656290E+03 0.30946194240107E+03 0.29215417588663E+03 -.12790236321580E+03 -.17717164773282E+03
 0.25680340771118E+03 0.58243808168486E+03 0.32435065693512E+03 0.28791596478483E+03 0.23682962218894E+03
 0.28791596478483E+03 0.45714641331958E+03 0.28794083528890E+03 0.23665018570429E+03 0.28794083528890E+03
 0.45700799657996E+03 0.28791596478483E+03 0.23682962218894E+03 0.28791596478483E+03 0.45714641331958E+03
 0.28794083528890E+03 0.23665018570429E+03 0.28794083528890E+03 0.45700799657996E+03 0.23439125813561E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37395860953969E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16599222627447E+00 0.00000000000000E+00 0.00000000000000E+00 0.16599222627447E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17359297045222E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17359297045222E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17256020570956E+00 0.17575371791224E+00 0.34560491242074E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1260.03093282
 0.98797013375386E+00 0.32071276230389E+03 0.48209230478425E+03 0.42894624872776E+03 0.41356027238006E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13268988494918E+00 0.00000000000000E+00 -.18626575163426E+02
 0.91077228098436E-03 0.12775139738698E+01 0.80000000000000E+04 0.30000000000000E+04 0.62621624214149E+01
 0.23483109080306E+01 0.34756540109879E+03 0.29216637482654E+03 0.34598729510043E+03 0.39675884062656E+03
 0.29215420782913E+03 0.29216005228837E+03 0.34157203768017E+03 0.39673130797281E+03 0.29215363308806E+03
 0.29216003736741E+03 0.34598729510043E+03 0.39675884062656E+03 0.29215420782913E+03 0.29216005228837E+03
 0.34157203768017E+03 0.39673130797281E+03 0.29215363308806E+03 0.29216003736741E+03 0.44693771641203E+03
 0.35895994168645E+03 0.18403223324781E+04 0.17041522364098E+04 0.61681171553412E+03 0.93084937102732E+03
 0.31095359691553E+03 0.12235810864350E+04 0.14063434020400E+04 0.11214780066020E+04 0.20178821391112E+04
 0.11265797409055E+04 0.14058929938555E+04 0.10470297827735E+04 0.20176733693465E+04 0.12235810864350E+04
 0.14063434020400E+04 0.11214780066020E+04 0.20178821391112E+04 0.11265797409055E+04 0.14058929938555E+04
 0.10470297827735E+04 0.20176733693465E+04 0.16600877533007E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55685581848567E+03 0.12948417288445E+01
 0.12948417288445E+01 0.48773714759572E+01 0.00000000000000E+00 0.34580493267352E+03 0.34580493267352E+03
 0.34580493267352E+03 0.34580493267352E+03 0.00000000000000E+00 0.00000000000000E+00 0.13542230864321E+00
 0.00000000000000E+00 -.11067650401004E+02 0.10000000000000E-02 0.17309489822475E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46217422246683E+01 0.17331533342506E+01 0.35896422411047E+03 0.44693332584619E+03
 0.31239127294685E+03 0.31239127294685E+03 0.29215135932487E+03 0.29215133393119E+03 0.31238610110084E+03
 0.31238610110084E+03 0.29215135946799E+03 0.29215133407164E+03 0.31239127294685E+03 0.31239127294685E+03
 0.29215135932487E+03 0.29215133393119E+03 0.31238610110084E+03 0.31238610110084E+03 0.29215135946799E+03
 0.29215133407164E+03 0.30958165185433E+03 0.29215440237701E+03 -.13010448791922E+03 -.18042031463272E+03
 0.25780957836440E+03 0.58424015618406E+03 0.32514152992784E+03 0.28973189236988E+03 0.23769511960215E+03
 0.28973189236988E+03 0.45847243416519E+03 0.28975706200867E+03 0.23751467706573E+03 0.28975706200867E+03
 0.45833340741835E+03 0.28973189236988E+03 0.23769511960215E+03 0.28973189236988E+03 0.45847243416519E+03
 0.28975706200867E+03 0.23751467706573E+03 0.28975706200867E+03 0.45833340741835E+03 0.23444180901226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37417550254243E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16596168504582E+00 0.00000000000000E+00 0.00000000000000E+00 0.16596168504582E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17351279199829E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17351279199829E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255998445862E+00 0.17565184281789E+00 0.34580493267352E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1271.38867385
 0.98789090800882E+00 0.32091778057129E+03 0.48240551296953E+03 0.42922809210580E+03 0.41383184270656E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13261943090046E+00 0.00000000000000E+00 -.18631216590815E+02
 0.91019039257257E-03 0.12776085497241E+01 0.80000000000000E+04 0.30000000000000E+04 0.62616988605217E+01
 0.23481370726957E+01 0.34790051490688E+03 0.29216738004675E+03 0.34630846950670E+03 0.39724266243742E+03
 0.29215450160492E+03 0.29216074091477E+03 0.34188179964994E+03 0.39721518049018E+03 0.29215388869797E+03
 0.29216072511987E+03 0.34630846950670E+03 0.39724266243742E+03 0.29215450160492E+03 0.29216074091477E+03
 0.34188179964994E+03 0.39721518049018E+03 0.29215388869797E+03 0.29216072511987E+03 0.44741219920782E+03
 0.35942951470180E+03 0.18433063066347E+04 0.17063256551777E+04 0.61515850987854E+03 0.92744161575818E+03
 0.30920731333025E+03 0.12255383927084E+04 0.14072559860921E+04 0.11228630121176E+04 0.20171116996431E+04
 0.11286934417577E+04 0.14068072703922E+04 0.10486529803522E+04 0.20169039648209E+04 0.12255383927084E+04
 0.14072559860921E+04 0.11228630121176E+04 0.20171116996431E+04 0.11286934417577E+04 0.14068072703922E+04
 0.10486529803522E+04 0.20169039648209E+04 0.16597887831663E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55717055070731E+03 0.12948417630421E+01
 0.12948417630421E+01 0.49228024400591E+01 0.00000000000000E+00 0.34603488137833E+03 0.34603488137833E+03
 0.34603488137833E+03 0.34603488137833E+03 0.00000000000000E+00 0.00000000000000E+00 0.13527736055612E+00
 0.00000000000000E+00 -.11053820304993E+02 0.10000000000000E-02 0.17324523905360E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46177315138368E+01 0.17316493176888E+01 0.35943378474642E+03 0.44740783869747E+03
 0.31254780550022E+03 0.31254780550022E+03 0.29215145534992E+03 0.29215142816238E+03 0.31254257768725E+03
 0.31254257768725E+03 0.29215145549854E+03 0.29215142830823E+03 0.31254780550022E+03 0.31254780550022E+03
 0.29215145534992E+03 0.29215142816238E+03 0.31254257768725E+03 0.31254257768725E+03 0.29215145549854E+03
 0.29215142830823E+03 0.30971963810738E+03 0.29215467560702E+03 -.13262417322052E+03 -.18413510503968E+03
 0.25896595454848E+03 0.58630507233398E+03 0.32604428801276E+03 0.29181319786024E+03 0.23868924524720E+03
 0.29181319786024E+03 0.45999071171808E+03 0.29183871430907E+03 0.23850764740385E+03 0.29183871430907E+03
 0.45985098674284E+03 0.29181319786024E+03 0.23868924524720E+03 0.29181319786024E+03 0.45999071171808E+03
 0.29183871430907E+03 0.23850764740385E+03 0.29183871430907E+03 0.45985098674284E+03 0.23450123866903E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37442325531195E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16586286434200E+00 0.00000000000000E+00 0.00000000000000E+00 0.16586286434200E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17339992644012E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17339992644012E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255994909330E+00 0.17553510583558E+00 0.34603488137833E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1280.02516300
 0.98782212200551E+00 0.32107373730921E+03 0.48264287363507E+03 0.42944235126973E+03 0.41403849508224E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13256648507487E+00 0.00000000000000E+00 -.18635398239999E+02
 0.90974824372963E-03 0.12776742682809E+01 0.80000000000000E+04 0.30000000000000E+04 0.62613767832735E+01
 0.23480162937275E+01 0.34815493472177E+03 0.29216817381146E+03 0.34655230095474E+03 0.39760844890408E+03
 0.29215473513228E+03 0.29216128767764E+03 0.34211710103142E+03 0.39758100538098E+03 0.29215409197501E+03
 0.29216127119512E+03 0.34655230095474E+03 0.39760844890408E+03 0.29215473513228E+03 0.29216128767764E+03
 0.34211710103142E+03 0.39758100538098E+03 0.29215409197501E+03 0.29216127119512E+03 0.44776862045889E+03
 0.35978122368123E+03 0.18455709726888E+04 0.17079766792792E+04 0.61395356203334E+03 0.92494199402799E+03
 0.30791866418448E+03 0.12270230800051E+04 0.14079512931754E+04 0.11239145660079E+04 0.20165400628039E+04
 0.11302961929231E+04 0.14075038825560E+04 0.10498839364958E+04 0.20163331358933E+04 0.12270230800051E+04
 0.14079512931754E+04 0.11239145660079E+04 0.20165400628039E+04 0.11302961929231E+04 0.14075038825560E+04
 0.10498839364958E+04 0.20163331358933E+04 0.16595898394126E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55741040496993E+03 0.12948417938521E+01
 0.12948417938521E+01 0.49573483966628E+01 0.00000000000000E+00 0.34620853957483E+03 0.34620853957483E+03
 0.34620853957483E+03 0.34620853957483E+03 0.00000000000000E+00 0.00000000000000E+00 0.13516978914424E+00
 0.00000000000000E+00 -.11044094073449E+02 0.10000000000000E-02 0.17335517174738E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46148031924066E+01 0.17305511971525E+01 0.35978549358249E+03 0.44776427329458E+03
 0.31266702603778E+03 0.31266702603778E+03 0.29215152169252E+03 0.29215150312150E+03 0.31266175558896E+03
 0.31266175558896E+03 0.29215152184428E+03 0.29215150327141E+03 0.31266702603778E+03 0.31266702603778E+03
 0.29215152169252E+03 0.29215150312150E+03 0.31266175558896E+03 0.31266175558896E+03 0.29215152184428E+03
 0.29215150327141E+03 0.30982478909165E+03 0.29215489149646E+03 -.13450872790593E+03 -.18691126963893E+03
 0.25983471323404E+03 0.58785028402961E+03 0.32671639722940E+03 0.29337177423182E+03 0.23943479076761E+03
 0.29337177423182E+03 0.46112457145193E+03 0.29339755626251E+03 0.23925233349669E+03 0.29339755626251E+03
 0.46098433507311E+03 0.29337177423182E+03 0.23943479076761E+03 0.29337177423182E+03 0.46112457145193E+03
 0.29339755626251E+03 0.23925233349669E+03 0.29339755626251E+03 0.46098433507311E+03 0.23454590652388E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37461079078959E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16579363126153E+00 0.00000000000000E+00 0.00000000000000E+00 0.16579363126153E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17331920598271E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17331920598271E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255990199955E+00 0.17544702639735E+00 0.34620853957483E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1291.22544133
 0.98774139897359E+00 0.32127521619099E+03 0.48294877451779E+03 0.42971821895821E+03 0.41430462130692E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13249859849874E+00 0.00000000000000E+00 -.18642178292036E+02
 0.90917765938791E-03 0.12777542888129E+01 0.80000000000000E+04 0.30000000000000E+04 0.62609846588207E+01
 0.23478692470577E+01 0.34848356499475E+03 0.29216924381901E+03 0.34686727934232E+03 0.39808103021825E+03
 0.29215505209361E+03 0.29216202888964E+03 0.34242105676413E+03 0.39805363589516E+03 0.29215436800205E+03
 0.29216201148373E+03 0.34686727934232E+03 0.39808103021825E+03 0.29215505209361E+03 0.29216202888964E+03
 0.34242105676413E+03 0.39805363589516E+03 0.29215436800205E+03 0.29216201148373E+03 0.44823039960534E+03
 0.36023630453264E+03 0.18484846254222E+04 0.17100974160753E+04 0.61234441733786E+03 0.92164069451347E+03
 0.30623455508893E+03 0.12289353477207E+04 0.14088256836854E+04 0.11252671206967E+04 0.20157736115051E+04
 0.11323618521610E+04 0.14083799373880E+04 0.10514691119009E+04 0.20155677110342E+04 0.12289353477207E+04
 0.14088256836854E+04 0.11252671206967E+04 0.20157736115051E+04 0.11323618521610E+04 0.14083799373880E+04
 0.10514691119009E+04 0.20155677110342E+04 0.16592986741637E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55771842195908E+03 0.12948418438070E+01
 0.12948418438070E+01 0.50021495099939E+01 0.00000000000000E+00 0.34643307036734E+03 0.34643307036734E+03
 0.34643307036734E+03 0.34643307036734E+03 0.00000000000000E+00 0.00000000000000E+00 0.13503352818348E+00
 0.00000000000000E+00 -.11033005511307E+02 0.10000000000000E-02 0.17349197316296E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46111643404306E+01 0.17291866276615E+01 0.36024057583199E+03 0.44822606978426E+03
 0.31282097807395E+03 0.31282097807395E+03 0.29215163367021E+03 0.29215160493670E+03 0.31281565247743E+03
 0.31281565247743E+03 0.29215163382810E+03 0.29215160509180E+03 0.31282097807395E+03 0.31282097807395E+03
 0.29215163367021E+03 0.29215160493670E+03 0.31281565247743E+03 0.31281565247743E+03 0.29215163382810E+03
 0.29215160509180E+03 0.30996057675958E+03 0.29215518270918E+03 -.13696124040414E+03 -.19052311672850E+03
 0.26096115812445E+03 0.58985322130836E+03 0.32758725739329E+03 0.29539593908086E+03 0.24040136403027E+03
 0.29539593908086E+03 0.46259483101512E+03 0.29542206529574E+03 0.24021777075177E+03 0.29542206529574E+03
 0.46245390897735E+03 0.29539593908086E+03 0.24040136403027E+03 0.29539593908086E+03 0.46259483101512E+03
 0.29542206529574E+03 0.24021777075177E+03 0.29542206529574E+03 0.46245390897735E+03 0.23460076375917E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37485318822478E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16571728144987E+00 0.00000000000000E+00 0.00000000000000E+00 0.16571728144987E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17321627997340E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17321627997340E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255979860533E+00 0.17533322951189E+00 0.34643307036734E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1301.54469750
 0.98766250889542E+00 0.32146036176378E+03 0.48322895551891E+03 0.42997136379602E+03 0.41454900521965E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13243679470325E+00 0.00000000000000E+00 -.18648903040099E+02
 0.90865395684793E-03 0.12778225700282E+01 0.80000000000000E+04 0.30000000000000E+04 0.62606500993508E+01
 0.23477437872566E+01 0.34878536498028E+03 0.29217027237595E+03 0.34715655147124E+03 0.39851413107470E+03
 0.29215535907587E+03 0.29216274580504E+03 0.34270028583210E+03 0.39848678165891E+03 0.29215463547083E+03
 0.29216272751531E+03 0.34715655147124E+03 0.39851413107470E+03 0.29215535907587E+03 0.29216274580504E+03
 0.34270028583210E+03 0.39848678165891E+03 0.29215463547083E+03 0.29216272751531E+03 0.44865253091556E+03
 0.36065161251853E+03 0.18511542987955E+04 0.17120392229985E+04 0.61087433314557E+03 0.91863158774180E+03
 0.30470288293051E+03 0.12306874971684E+04 0.14096157214237E+04 0.11265054642589E+04 0.20150599924650E+04
 0.11342549141807E+04 0.14091715139849E+04 0.10529206956202E+04 0.20148550484547E+04 0.12306874971684E+04
 0.14096157214237E+04 0.11265054642589E+04 0.20150599924650E+04 0.11342549141807E+04 0.14091715139849E+04
 0.10529206956202E+04 0.20148550484547E+04 0.16590314444349E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55800133253302E+03 0.12948418933544E+01
 0.12948418933544E+01 0.50434265346614E+01 0.00000000000000E+00 0.34663856856206E+03 0.34663856856206E+03
 0.34663856856206E+03 0.34663856856206E+03 0.00000000000000E+00 0.00000000000000E+00 0.13491113561182E+00
 0.00000000000000E+00 -.11023543942957E+02 0.10000000000000E-02 0.17361277846962E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46079557452621E+01 0.17279834044733E+01 0.36065587072004E+03 0.44864822838204E+03
 0.31296255651683E+03 0.31296255651683E+03 0.29215173412727E+03 0.29215170362687E+03 0.31295718009309E+03
 0.31295718009309E+03 0.29215173428993E+03 0.29215170378667E+03 0.31296255651683E+03 0.31296255651683E+03
 0.29215173412727E+03 0.29215170362687E+03 0.31295718009309E+03 0.31295718009309E+03 0.29215173428993E+03
 0.29215170378667E+03 0.31008548610905E+03 0.29215546284477E+03 -.13920603874098E+03 -.19382738373690E+03
 0.26199032146153E+03 0.59167857051955E+03 0.32837829745072E+03 0.29724576983134E+03 0.24128337670163E+03
 0.29724576983134E+03 0.46393322561005E+03 0.29727221449013E+03 0.24109874134051E+03 0.29727221449013E+03
 0.46379167654708E+03 0.29724576983134E+03 0.24128337670163E+03 0.29724576983134E+03 0.46393322561005E+03
 0.29727221449013E+03 0.24109874134051E+03 0.29727221449013E+03 0.46379167654708E+03 0.23464726662330E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37507557350891E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16565181171894E+00 0.00000000000000E+00 0.00000000000000E+00 0.16565181171894E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17312966562050E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17312966562050E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255968459151E+00 0.17522918707731E+00 0.34663856856206E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1311.86395367
 0.98759077162389E+00 0.32164480662730E+03 0.48350776451941E+03 0.43022297669208E+03 0.41479190685883E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13237573463452E+00 0.00000000000000E+00 -.18654555795687E+02
 0.90813284520497E-03 0.12778846753672E+01 0.80000000000000E+04 0.30000000000000E+04 0.62603458310520E+01
 0.23476296866445E+01 0.34908632889513E+03 0.29217134181640E+03 0.34744503222759E+03 0.39894500491581E+03
 0.29215568048022E+03 0.29216349546698E+03 0.34297884404641E+03 0.39891770008156E+03 0.29215491563324E+03
 0.29216347626201E+03 0.34744503222759E+03 0.39894500491581E+03 0.29215568048022E+03 0.29216349546698E+03
 0.34297884404641E+03 0.39891770008156E+03 0.29215491563324E+03 0.29216347626201E+03 0.44907122240911E+03
 0.36106270038688E+03 0.18538048556110E+04 0.17139621416088E+04 0.60942473095426E+03 0.91566642727187E+03
 0.30319457266284E+03 0.12324291876815E+04 0.14103947585539E+04 0.11277334259808E+04 0.20143456821816E+04
 0.11361370997751E+04 0.14099520878297E+04 0.10543608639968E+04 0.20141416972007E+04 0.12324291876815E+04
 0.14103947585539E+04 0.11277334259808E+04 0.20143456821816E+04 0.11361370997751E+04 0.14099520878297E+04
 0.10543608639968E+04 0.20141416972007E+04 0.16587664440847E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55828190064026E+03 0.12948419350034E+01
 0.12948419350034E+01 0.50847035593289E+01 0.00000000000000E+00 0.34684359092671E+03 0.34684359092671E+03
 0.34684359092671E+03 0.34684359092671E+03 0.00000000000000E+00 0.00000000000000E+00 0.13479163494297E+00
 0.00000000000000E+00 -.11012984978520E+02 0.10000000000000E-02 0.17372839270325E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46048892040722E+01 0.17268334515271E+01 0.36106695336130E+03 0.44906693993409E+03
 0.31310402711478E+03 0.31310402711478E+03 0.29215183938377E+03 0.29215180703208E+03 0.31309859985361E+03
 0.31309859985361E+03 0.29215183955108E+03 0.29215180719645E+03 0.31310402711478E+03 0.31310402711478E+03
 0.29215183938377E+03 0.29215180703208E+03 0.31309859985361E+03 0.31309859985361E+03 0.29215183955108E+03
 0.29215180719645E+03 0.31021033690646E+03 0.29215575431403E+03 -.14142593615741E+03 -.19709175449959E+03
 0.26301695381740E+03 0.59349636755251E+03 0.32916432896601E+03 0.29908203439750E+03 0.24216292956825E+03
 0.29908203439750E+03 0.46526574920922E+03 0.29910879876184E+03 0.24197725761695E+03 0.29910879876184E+03
 0.46512357863214E+03 0.29908203439750E+03 0.24216292956825E+03 0.29908203439750E+03 0.46526574920922E+03
 0.29910879876184E+03 0.24197725761695E+03 0.29910879876184E+03 0.46512357863214E+03 0.23469977142622E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37529698413128E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16557748403533E+00 0.00000000000000E+00 0.00000000000000E+00 0.16557748403533E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17303826305551E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17303826305551E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255960366208E+00 0.17512554365478E+00 0.34684359092671E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1327.61626332
 0.98749373915034E+00 0.32192417911236E+03 0.48392916333118E+03 0.43060286078877E+03 0.41515869071488E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13228423206338E+00 0.00000000000000E+00 -.18674025321069E+02
 0.90734457429334E-03 0.12779679957294E+01 0.80000000000000E+04 0.30000000000000E+04 0.62599376719400E+01
 0.23474766269775E+01 0.34954205279772E+03 0.29217307021298E+03 0.34788192419270E+03 0.39959946701666E+03
 0.29215620529068E+03 0.29216471727595E+03 0.34340052288822E+03 0.39957222820903E+03 0.29215537341276E+03
 0.29216469660094E+03 0.34788192419270E+03 0.39959946701666E+03 0.29215620529068E+03 0.29216471727595E+03
 0.34340052288822E+03 0.39957222820903E+03 0.29215537341276E+03 0.29216469660094E+03 0.44971317929483E+03
 0.36169324411404E+03 0.18578087159371E+04 0.17168638961647E+04 0.60704422881340E+03 0.91090375059755E+03
 0.30082430064008E+03 0.12350627169362E+04 0.14115201354595E+04 0.11295886268317E+04 0.20131895251969E+04
 0.11389864167474E+04 0.14110797242368E+04 0.10565413375898E+04 0.20129869375808E+04 0.12350627169362E+04
 0.14115201354595E+04 0.11295886268317E+04 0.20131895251969E+04 0.11389864167474E+04 0.14110797242368E+04
 0.10565413375898E+04 0.20129869375808E+04 0.16582645633617E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55870436867415E+03 0.12948420784533E+01
 0.12948420784533E+01 0.51477127979455E+01 0.00000000000000E+00 0.34715515546855E+03 0.34715515546855E+03
 0.34715515546855E+03 0.34715515546855E+03 0.00000000000000E+00 0.00000000000000E+00 0.13461457572061E+00
 0.00000000000000E+00 -.11009020530903E+02 0.10000000000000E-02 0.17389550648336E+01 0.80000000000000E+04
 0.30000000000000E+04 0.46004639002937E+01 0.17251739626101E+01 0.36169745788286E+03 0.44970895393452E+03
 0.31331769938954E+03 0.31331769938954E+03 0.29215206239255E+03 0.29215197607245E+03 0.31331219513324E+03
 0.31331219513324E+03 0.29215206257105E+03 0.29215197624348E+03 0.31331769938954E+03 0.31331769938954E+03
 0.29215206239255E+03 0.29215197607245E+03 0.31331219513324E+03 0.31331219513324E+03 0.29215206257105E+03
 0.29215197624348E+03 0.31039886214279E+03 0.29215622586406E+03 -.14487823171002E+03 -.20216944720664E+03
 0.26458955625572E+03 0.59628873888384E+03 0.33037623484684E+03 0.30191698643522E+03 0.24351095278823E+03
 0.30191698643522E+03 0.46731671693582E+03 0.30194423522955E+03 0.24332362117131E+03 0.30194423522955E+03
 0.46717351668053E+03 0.30191698643522E+03 0.24351095278823E+03 0.30191698643522E+03 0.46731671693582E+03
 0.30194423522955E+03 0.24332362117131E+03 0.30194423522955E+03 0.46717351668053E+03 0.23476646105264E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37563573716625E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16556503662821E+00 0.00000000000000E+00 0.00000000000000E+00 0.16556503662821E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17293928894543E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17293928894543E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255913222960E+00 0.17496790095233E+00 0.34715515546855E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1330.17968876
 0.98747321790219E+00 0.32196992405836E+03 0.48399824240787E+03 0.43066536743724E+03 0.41521906512721E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13226956378368E+00 0.00000000000000E+00 -.18661663120787E+02
 0.90721577100595E-03 0.12779775014845E+01 0.80000000000000E+04 0.30000000000000E+04 0.62598911097473E+01
 0.23474591661552E+01 0.34961727978582E+03 0.29217335760885E+03 0.34795401246221E+03 0.39970440477243E+03
 0.29215629289134E+03 0.29216492107515E+03 0.34347037863733E+03 0.39967717743650E+03 0.29215544984415E+03
 0.29216490015627E+03 0.34795401246221E+03 0.39970440477243E+03 0.29215629289134E+03 0.29216492107515E+03
 0.34347037863733E+03 0.39967717743650E+03 0.29215544984415E+03 0.29216490015627E+03 0.44980916121166E+03
 0.36178660911561E+03 0.18584604707050E+04 0.17173310112705E+04 0.60682132258336E+03 0.91038687069575E+03
 0.30053144149947E+03 0.12354916030772E+04 0.14117288651530E+04 0.11298870567782E+04 0.20130448201256E+04
 0.11394486844782E+04 0.14112888931482E+04 0.10568899484510E+04 0.20128425265713E+04 0.12354916030772E+04
 0.14117288651530E+04 0.11298870567782E+04 0.20130448201256E+04 0.11394486844782E+04 0.14112888931482E+04
 0.10568899484510E+04 0.20128425265713E+04 0.16582610711974E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55877429514139E+03 0.12948419873696E+01
 0.12948419873696E+01 0.51579664997181E+01 0.00000000000000E+00 0.34720573609890E+03 0.34720573609890E+03
 0.34720573609890E+03 0.34720573609890E+03 0.00000000000000E+00 0.00000000000000E+00 0.13458635885897E+00
 0.00000000000000E+00 -.10991700133504E+02 0.10000000000000E-02 0.17392167279503E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45997717658961E+01 0.17249144122110E+01 0.36179081424175E+03 0.44980494681857E+03
 0.31335384269501E+03 0.31335384269501E+03 0.29215201081612E+03 0.29215200430060E+03 0.31334832538738E+03
 0.31334832538738E+03 0.29215201098878E+03 0.29215200447269E+03 0.31335384269501E+03 0.31335384269501E+03
 0.29215201081612E+03 0.29215200430060E+03 0.31334832538738E+03 0.31334832538738E+03 0.29215201098878E+03
 0.29215200447269E+03 0.31043084022493E+03 0.29215630430368E+03 -.14535271225237E+03 -.20286165700445E+03
 0.26483519074580E+03 0.59671260631172E+03 0.33055323961218E+03 0.30233109941749E+03 0.24372042829607E+03
 0.30233109941749E+03 0.46762417773173E+03 0.30235843211552E+03 0.24353290098709E+03 0.30235843211552E+03
 0.46748088731382E+03 0.30233109941749E+03 0.24372042829607E+03 0.30233109941749E+03 0.46762417773173E+03
 0.30235843211552E+03 0.24353290098710E+03 0.30235843211552E+03 0.46748088731382E+03 0.23479428630311E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37568798661316E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16542124172785E+00 0.00000000000000E+00 0.00000000000000E+00 0.16542124172785E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17288065948593E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17288065948593E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255953797748E+00 0.17494285305051E+00 0.34720573609890E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1341.12980505
 0.98739434175933E+00 0.32216457575435E+03 0.48429050901299E+03 0.43092976596231E+03 0.41547463929864E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13220720785110E+00 0.00000000000000E+00 -.18674925152636E+02
 0.90666751294124E-03 0.12780207254742E+01 0.80000000000000E+04 0.30000000000000E+04 0.62596793937218E+01
 0.23473797726457E+01 0.34993453473206E+03 0.29217461805089E+03 0.34825812897193E+03 0.40015505459734E+03
 0.29215667889919E+03 0.29216581832790E+03 0.34376435846783E+03 0.40012787394405E+03 0.29215578674061E+03
 0.29216579634267E+03 0.34825812897193E+03 0.40015505459734E+03 0.29215667889919E+03 0.29216581832790E+03
 0.34376435846784E+03 0.40012787394405E+03 0.29215578674061E+03 0.29216579634267E+03 0.45024226479880E+03
 0.36220897669252E+03 0.18612348683578E+04 0.17193385800109E+04 0.60537489495343E+03 0.90741757234721E+03
 0.29901580291902E+03 0.12373165365543E+04 0.14125333798576E+04 0.11311706111074E+04 0.20122973668368E+04
 0.11414209259332E+04 0.14120950489237E+04 0.10583947003241E+04 0.20120961120306E+04 0.12373165365543E+04
 0.14125333798576E+04 0.11311706111074E+04 0.20122973668368E+04 0.11414209259332E+04 0.14120950489237E+04
 0.10583947003241E+04 0.20120961120307E+04 0.16580148965452E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55906909529984E+03 0.12948420850832E+01
 0.12948420850832E+01 0.52017669648610E+01 0.00000000000000E+00 0.34742064075679E+03 0.34742064075679E+03
 0.34742064075679E+03 0.34742064075679E+03 0.00000000000000E+00 0.00000000000000E+00 0.13446764361256E+00
 0.00000000000000E+00 -.10988601690340E+02 0.10000000000000E-02 0.17403050149237E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45968953323683E+01 0.17238357496381E+01 0.36221323595956E+03 0.45023801882708E+03
 0.31350391355173E+03 0.31350391355173E+03 0.29215216392809E+03 0.29215212875264E+03 0.31349834219324E+03
 0.31349834219324E+03 0.29215216410750E+03 0.29215212892913E+03 0.31350391355173E+03 0.31350391355173E+03
 0.29215216392809E+03 0.29215212875264E+03 0.31349834219324E+03 0.31349834219324E+03 0.29215216410750E+03
 0.29215212892913E+03 0.31056340722271E+03 0.29215664848538E+03 -.14762995673055E+03 -.20620108104373E+03
 0.26590509726854E+03 0.59859604122386E+03 0.33136141846897E+03 0.30422571217955E+03 0.24463448583519E+03
 0.30422571217955E+03 0.46900135615620E+03 0.30425338877182E+03 0.24444588677001E+03 0.30425338877182E+03
 0.46885743537921E+03 0.30422571217955E+03 0.24463448583519E+03 0.30422571217955E+03 0.46900135615620E+03
 0.30425338877182E+03 0.24444588677001E+03 0.30425338877182E+03 0.46885743537921E+03 0.23485792679685E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37592170726262E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16540998802650E+00 0.00000000000000E+00 0.00000000000000E+00 0.16540998802650E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17280947166133E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17280947166133E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255922405487E+00 0.17483432559871E+00 0.34742064075679E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1350.25450934
 0.98733147182164E+00 0.32232616049787E+03 0.48453219086158E+03 0.43114848462913E+03 0.41568618380240E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13215589421427E+00 0.00000000000000E+00 -.18675391735126E+02
 0.90621298891933E-03 0.12780532947394E+01 0.80000000000000E+04 0.30000000000000E+04 0.62595198752111E+01
 0.23473199532042E+01 0.35019789291816E+03 0.29217570803383E+03 0.34851059942742E+03 0.40052896255371E+03
 0.29215701492285E+03 0.29216659843729E+03 0.34400842966261E+03 0.40050182035585E+03 0.29215608014166E+03
 0.29216657553378E+03 0.34851059942742E+03 0.40052896255371E+03 0.29215701492285E+03 0.29216659843729E+03
 0.34400842966261E+03 0.40050182035585E+03 0.29215608014166E+03 0.29216657553378E+03 0.45060192921935E+03
 0.36255950404975E+03 0.18635248041289E+04 0.17209915842600E+04 0.60414254465116E+03 0.90491160982673E+03
 0.29774835245231E+03 0.12388240929700E+04 0.14131773355748E+04 0.11322284269163E+04 0.20116484993193E+04
 0.11430513684224E+04 0.14127403618048E+04 0.10596366534506E+04 0.20114481045894E+04 0.12388240929700E+04
 0.14131773355748E+04 0.11322284269163E+04 0.20116484993193E+04 0.11430513684224E+04 0.14127403618048E+04
 0.10596366534506E+04 0.20114481045894E+04 0.16577840056290E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55931259450656E+03 0.12948420885209E+01
 0.12948420885209E+01 0.52382657820208E+01 0.00000000000000E+00 0.34759876745139E+03 0.34759876745139E+03
 0.34759876745139E+03 0.34759876745139E+03 0.00000000000000E+00 0.00000000000000E+00 0.13437090787642E+00
 0.00000000000000E+00 -.10974367779847E+02 0.10000000000000E-02 0.17411758831329E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45945961447648E+01 0.17229735542868E+01 0.36256374621581E+03 0.45059771378111E+03
 0.31362851481420E+03 0.31362851481420E+03 0.29215227413802E+03 0.29215223717105E+03 0.31362289850624E+03
 0.31362289850624E+03 0.29215227432096E+03 0.29215223735103E+03 0.31362851481420E+03 0.31362851481420E+03
 0.29215227413802E+03 0.29215223717105E+03 0.31362289850624E+03 0.31362289850624E+03 0.29215227432096E+03
 0.29215223735103E+03 0.31067348833973E+03 0.29215694632613E+03 -.14952990670912E+03 -.20898646300350E+03
 0.26679265668445E+03 0.60015054511456E+03 0.33202392514669E+03 0.30580144893808E+03 0.24539227328818E+03
 0.30580144893808E+03 0.47013648325181E+03 0.30582941222669E+03 0.24520277487365E+03 0.30582941222669E+03
 0.46999202940356E+03 0.30580144893808E+03 0.24539227328818E+03 0.30580144893808E+03 0.47013648325181E+03
 0.30582941222669E+03 0.24520277487365E+03 0.30582941222669E+03 0.46999202940356E+03 0.23490299022041E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37611233052382E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16530498622880E+00 0.00000000000000E+00 0.00000000000000E+00 0.16530498622880E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17270579327266E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17270579327266E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255930152960E+00 0.17474483491137E+00 0.34759876745139E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1361.20415449
 0.98725183555360E+00 0.32251977829822E+03 0.48482090406818E+03 0.43141020929191E+03 0.41593949332069E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13209488066741E+00 0.00000000000000E+00 -.18681443944280E+02
 0.90566890936924E-03 0.12780883563674E+01 0.80000000000000E+04 0.30000000000000E+04 0.62593481586341E+01
 0.23472555594878E+01 0.35051306050761E+03 0.29217706314231E+03 0.34881274458294E+03 0.40097550115857E+03
 0.29215743534238E+03 0.29216757332119E+03 0.34430060226202E+03 0.40094840483437E+03 0.29215644738921E+03
 0.29216754928076E+03 0.34881274458294E+03 0.40097550115857E+03 0.29215743534238E+03 0.29216757332119E+03
 0.34430060226202E+03 0.40094840483437E+03 0.29215644738921E+03 0.29216754928076E+03 0.45103050454914E+03
 0.36297638059630E+03 0.18662620408411E+04 0.17229678615499E+04 0.60268030142688E+03 0.90194109226653E+03
 0.29624738933252E+03 0.12406260720946E+04 0.14139408936671E+04 0.11334931534579E+04 0.20108701540466E+04
 0.11450003525304E+04 0.14135055526846E+04 0.10611212064556E+04 0.20106707995740E+04 0.12406260720946E+04
 0.14139408936671E+04 0.11334931534579E+04 0.20108701540466E+04 0.11450003525304E+04 0.14135055526846E+04
 0.10611212064556E+04 0.20106707995740E+04 0.16575137834536E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55960420758469E+03 0.12948421331131E+01
 0.12948421331131E+01 0.52820643626126E+01 0.00000000000000E+00 0.34781120703443E+03 0.34781120703443E+03
 0.34781120703443E+03 0.34781120703443E+03 0.00000000000000E+00 0.00000000000000E+00 0.13425736772679E+00
 0.00000000000000E+00 -.10963609342511E+02 0.10000000000000E-02 0.17421798522194E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45919484086610E+01 0.17219806532479E+01 0.36298060192918E+03 0.45102632381235E+03
 0.31377779097852E+03 0.31377779097852E+03 0.29215241212912E+03 0.29215237291905E+03 0.31377212076074E+03
 0.31377212076074E+03 0.29215241231608E+03 0.29215237310297E+03 0.31377779097852E+03 0.31377779097852E+03
 0.29215241212912E+03 0.29215237291905E+03 0.31377212076074E+03 0.31377212076074E+03 0.29215241231608E+03
 0.29215237310297E+03 0.31080540857736E+03 0.29215731686202E+03 -.15179476688800E+03 -.21230467044930E+03
 0.26784922444299E+03 0.60199898018985E+03 0.33281050962465E+03 0.30767732417281E+03 0.24629329685418E+03
 0.30767732417281E+03 0.47148527335546E+03 0.30770563252491E+03 0.24610272404943E+03 0.30770563252491E+03
 0.47134018477512E+03 0.30767732417281E+03 0.24629329685418E+03 0.30767732417281E+03 0.47148527335546E+03
 0.30770563252491E+03 0.24610272404943E+03 0.30770563252491E+03 0.47134018477512E+03 0.23495480571473E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37634170744170E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16522941168613E+00 0.00000000000000E+00 0.00000000000000E+00 0.16522941168613E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17261134909918E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17261134909918E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255921461011E+00 0.17463803311034E+00 0.34781120703443E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1371.82593283
 0.98717829085214E+00 0.32270684513217E+03 0.48509924657823E+03 0.43166249547593E+03 0.41618373676013E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13203628693862E+00 0.00000000000000E+00 -.18689563653663E+02
 0.90514383830374E-03 0.12781191104061E+01 0.80000000000000E+04 0.30000000000000E+04 0.62591975465089E+01
 0.23471990799408E+01 0.35081751793529E+03 0.29217843048823E+03 0.34910464289068E+03 0.40140675442511E+03
 0.29215786257546E+03 0.29216856268192E+03 0.34458287664633E+03 0.40137970207523E+03 0.29215682076581E+03
 0.29216853749969E+03 0.34910464289068E+03 0.40140675442511E+03 0.29215786257546E+03 0.29216856268192E+03
 0.34458287664633E+03 0.40137970207523E+03 0.29215682076581E+03 0.29216853749969E+03 0.45144497595911E+03
 0.36337911420278E+03 0.18688988702846E+04 0.17248693288606E+04 0.60123605851943E+03 0.89903115580491E+03
 0.29478891699288E+03 0.12423632011556E+04 0.14146603724539E+04 0.11347110433096E+04 0.20100983816282E+04
 0.11468801558034E+04 0.14142266012900E+04 0.10625520472726E+04 0.20099000281399E+04 0.12423632011556E+04
 0.14146603724539E+04 0.11347110433096E+04 0.20100983816282E+04 0.11468801558034E+04 0.14142266012900E+04
 0.10625520472726E+04 0.20099000281399E+04 0.16572318264349E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.55988492130554E+03 0.12948421929385E+01
 0.12948421929385E+01 0.53245514759990E+01 0.00000000000000E+00 0.34801631962607E+03 0.34801631962607E+03
 0.34801631962607E+03 0.34801631962607E+03 0.00000000000000E+00 0.00000000000000E+00 0.13414977161048E+00
 0.00000000000000E+00 -.10955752194613E+02 0.10000000000000E-02 0.17431110207669E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45894953934033E+01 0.17210607725262E+01 0.36338331268816E+03 0.45144083043996E+03
 0.31392197390997E+03 0.31392197390997E+03 0.29215257319366E+03 0.29215251098082E+03 0.31391625152703E+03
 0.31391625152703E+03 0.29215257338577E+03 0.29215251116828E+03 0.31392197390997E+03 0.31392197390997E+03
 0.29215257319366E+03 0.29215251098082E+03 0.31391625152703E+03 0.31391625152703E+03 0.29215257338577E+03
 0.29215251116828E+03 0.31093283944092E+03 0.29215769102897E+03 -.15399486126252E+03 -.21552659952259E+03
 0.26887117685011E+03 0.60378573899849E+03 0.33357020626413E+03 0.30949511280316E+03 0.24716443455593E+03
 0.30949511280316E+03 0.47278914340029E+03 0.30952375586750E+03 0.24697280644692E+03 0.30952375586750E+03
 0.47264342516265E+03 0.30949511280316E+03 0.24716443455593E+03 0.30949511280316E+03 0.47278914340029E+03
 0.30952375586750E+03 0.24697280644692E+03 0.30952375586750E+03 0.47264342516265E+03 0.23500145030579E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37656366509913E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16517750189404E+00 0.00000000000000E+00 0.00000000000000E+00 0.16517750189404E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17252857987259E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17252857987259E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255905799163E+00 0.17453496071063E+00 0.34801631962607E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1382.28377778
 0.98711353418123E+00 0.32289014585280E+03 0.48537179455157E+03 0.43190918305602E+03 0.41642253570772E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13197933163833E+00 0.00000000000000E+00 -.18697579343882E+02
 0.90462992782499E-03 0.12781437288289E+01 0.80000000000000E+04 0.30000000000000E+04 0.62590769876326E+01
 0.23471538703622E+01 0.35111650387456E+03 0.29217982760894E+03 0.34939130761226E+03 0.40182907871545E+03
 0.29215830204544E+03 0.29216957908380E+03 0.34486019650605E+03 0.40180206947784E+03 0.29215720500975E+03
 0.29216955274020E+03 0.34939130761226E+03 0.40182907871545E+03 0.29215830204544E+03 0.29216957908380E+03
 0.34486019650605E+03 0.40180206947784E+03 0.29215720500975E+03 0.29216955274020E+03 0.45184897259358E+03
 0.36377098157056E+03 0.18714725556019E+04 0.17267175708025E+04 0.59984538875415E+03 0.89622582965875E+03
 0.29338121396082E+03 0.12440611206196E+04 0.14153556451162E+04 0.11358965242123E+04 0.20093349982550E+04
 0.11487180768064E+04 0.14149234249402E+04 0.10639461234666E+04 0.20091376390822E+04 0.12440611206196E+04
 0.14153556451162E+04 0.11358965242123E+04 0.20093349982550E+04 0.11487180768064E+04 0.14149234249402E+04
 0.10639461234666E+04 0.20091376390822E+04 0.16569571891854E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56015874944219E+03 0.12948422519975E+01
 0.12948422519975E+01 0.53663828557826E+01 0.00000000000000E+00 0.34821781164767E+03 0.34821781164767E+03
 0.34821781164767E+03 0.34821781164767E+03 0.00000000000000E+00 0.00000000000000E+00 0.13404618675839E+00
 0.00000000000000E+00 -.10948394349898E+02 0.10000000000000E-02 0.17439862114017E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45871922310498E+01 0.17201970866437E+01 0.36377516069309E+03 0.45184485771190E+03
 0.31406387790699E+03 0.31406387790699E+03 0.29215271884332E+03 0.29215265310905E+03 0.31405810410454E+03
 0.31405810410454E+03 0.29215271903870E+03 0.29215265329970E+03 0.31406387790699E+03 0.31406387790699E+03
 0.29215271884332E+03 0.29215265310905E+03 0.31405810410454E+03 0.31405810410454E+03 0.29215271903870E+03
 0.29215265329970E+03 0.31105829570405E+03 0.29215807362385E+03 -.15612955530365E+03 -.21864848985050E+03
 0.26987458411944E+03 0.60553836748223E+03 0.33431441044220E+03 0.31126839957810E+03 0.24801952287142E+03
 0.31126839957810E+03 0.47406810118272E+03 0.31129737387546E+03 0.24782686887005E+03 0.31129737387546E+03
 0.47392177676166E+03 0.31126839957810E+03 0.24801952287142E+03 0.31126839957810E+03 0.47406810118272E+03
 0.31129737387546E+03 0.24782686887005E+03 0.31129737387546E+03 0.47392177676166E+03 0.23505644205590E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37678217771199E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16512759007915E+00 0.00000000000000E+00 0.00000000000000E+00 0.16512759007915E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17245653422678E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17245653422678E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255889415124E+00 0.17443381522032E+00 0.34821781164767E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1392.74162273
 0.98704936189668E+00 0.32307297410153E+03 0.48564296950539E+03 0.43215476609641E+03 0.41666036723934E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13192319928762E+00 0.00000000000000E+00 -.18702690433623E+02
 0.90411794862679E-03 0.12781612460910E+01 0.80000000000000E+04 0.30000000000000E+04 0.62589912066780E+01
 0.23471217025042E+01 0.35141468826108E+03 0.29218127497498E+03 0.34967721691798E+03 0.40224924773320E+03
 0.29215876024089E+03 0.29217063749393E+03 0.34513687676959E+03 0.40222228140318E+03 0.29215760579893E+03
 0.29217060995253E+03 0.34967721691798E+03 0.40224924773320E+03 0.29215876024089E+03 0.29217063749393E+03
 0.34513687676959E+03 0.40222228140318E+03 0.29215760579893E+03 0.29217060995253E+03 0.45224960086439E+03
 0.36415883345629E+03 0.18740303676274E+04 0.17285515815509E+04 0.59847612484468E+03 0.89346448527404E+03
 0.29199597980514E+03 0.12457495069773E+04 0.14160395790994E+04 0.11370735854439E+04 0.20085694252943E+04
 0.11505460002083E+04 0.14156089099559E+04 0.10653306594411E+04 0.20083730638778E+04 0.12457495069773E+04
 0.14160395790994E+04 0.11370735854439E+04 0.20085694252943E+04 0.11505460002083E+04 0.14156089099559E+04
 0.10653306594411E+04 0.20083730638778E+04 0.16566869026769E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56043121102673E+03 0.12948422896556E+01
 0.12948422896556E+01 0.54082142355662E+01 0.00000000000000E+00 0.34841846927613E+03 0.34841846927613E+03
 0.34841846927613E+03 0.34841846927613E+03 0.00000000000000E+00 0.00000000000000E+00 0.13394486440351E+00
 0.00000000000000E+00 -.10937783002747E+02 0.10000000000000E-02 0.17448234482274E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45849911107782E+01 0.17193716665418E+01 0.36416300125909E+03 0.45224550947735E+03
 0.31420567056934E+03 0.31420567056934E+03 0.29215287081475E+03 0.29215280140620E+03 0.31419984535930E+03
 0.31419984535930E+03 0.29215287101306E+03 0.29215280159971E+03 0.31420567056934E+03 0.31420567056934E+03
 0.29215287081475E+03 0.29215280140620E+03 0.31419984535930E+03 0.31419984535930E+03 0.29215287101306E+03
 0.29215280159971E+03 0.31118369162903E+03 0.29215847026015E+03 -.15824185573964E+03 -.22173431550321E+03
 0.27087251730017E+03 0.60727547843520E+03 0.33504859854853E+03 0.31302643036822E+03 0.24886925744148E+03
 0.31302643036822E+03 0.47533434746718E+03 0.31305573707943E+03 0.24867558533323E+03 0.31305573707943E+03
 0.47518742416323E+03 0.31302643036822E+03 0.24886925744148E+03 0.31302643036822E+03 0.47533434746718E+03
 0.31305573707943E+03 0.24867558533323E+03 0.31305573707943E+03 0.47518742416323E+03 0.23511255254736E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37699864359107E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16505177401663E+00 0.00000000000000E+00 0.00000000000000E+00 0.16505177401663E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17236852650213E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17236852650213E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255882637300E+00 0.17433330685832E+00 0.34841846927613E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1403.19946767
 0.98697551456943E+00 0.32325574157178E+03 0.48591291219955E+03 0.43240003063997E+03 0.41689814153554E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13186770785942E+00 0.00000000000000E+00 -.18703157684585E+02
 0.90360675987977E-03 0.12781736947237E+01 0.80000000000000E+04 0.30000000000000E+04 0.62589302479183E+01
 0.23470988429694E+01 0.35171207523501E+03 0.29218277364809E+03 0.34996237016514E+03 0.40266734970055E+03
 0.29215923768448E+03 0.29217173901926E+03 0.34541290771723E+03 0.40264042608928E+03 0.29215802360324E+03
 0.29217171024313E+03 0.34996237016514E+03 0.40266734970055E+03 0.29215923768448E+03 0.29217173901926E+03
 0.34541290771723E+03 0.40264042608928E+03 0.29215802360324E+03 0.29217171024313E+03 0.45264717278217E+03
 0.36454292988922E+03 0.18765812343472E+04 0.17303831832318E+04 0.59712696998129E+03 0.89074440074108E+03
 0.29063179590989E+03 0.12474322176720E+04 0.14167152612830E+04 0.11382482809669E+04 0.20078045378970E+04
 0.11523677155854E+04 0.14162861502464E+04 0.10667115666746E+04 0.20076091843748E+04 0.12474322176720E+04
 0.14167152612830E+04 0.11382482809669E+04 0.20078045378971E+04 0.11523677155854E+04 0.14162861502464E+04
 0.10667115666746E+04 0.20076091843749E+04 0.16564273371198E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56070397761582E+03 0.12948422930982E+01
 0.12948422930982E+01 0.54500456153499E+01 0.00000000000000E+00 0.34861754956637E+03 0.34861754956637E+03
 0.34861754956637E+03 0.34861754956637E+03 0.00000000000000E+00 0.00000000000000E+00 0.13384574582003E+00
 0.00000000000000E+00 -.10921834981004E+02 0.10000000000000E-02 0.17456276283405E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45828788855761E+01 0.17185795820911E+01 0.36454708593432E+03 0.45264310575516E+03
 0.31434726535869E+03 0.31434726535869E+03 0.29215302929084E+03 0.29215295605073E+03 0.31434138876728E+03
 0.31434138876728E+03 0.29215302949170E+03 0.29215295624674E+03 0.31434726535869E+03 0.31434726535869E+03
 0.29215302929084E+03 0.29215295605073E+03 0.31434138876728E+03 0.31434138876728E+03 0.29215302949170E+03
 0.29215295624674E+03 0.31130895181986E+03 0.29215888124991E+03 -.16033996096838E+03 -.22479749697063E+03
 0.27185940097020E+03 0.60898332330341E+03 0.33576462532835E+03 0.31476738179512E+03 0.24970812381985E+03
 0.31476738179512E+03 0.47657614341940E+03 0.31479702202320E+03 0.24951344110579E+03 0.31479702202320E+03
 0.47642862774986E+03 0.31476738179512E+03 0.24970812381985E+03 0.31476738179512E+03 0.47657614341940E+03
 0.31479702202320E+03 0.24951344110579E+03 0.31479702202320E+03 0.47642862774986E+03 0.23515973946534E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37721166957576E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16493363135537E+00 0.00000000000000E+00 0.00000000000000E+00 0.16493363135537E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17225336278852E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17225336278852E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255891626714E+00 0.17423387090680E+00 0.34861754956637E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1410.17136430
 0.98691473015998E+00 0.32337782874361E+03 0.48609239611772E+03 0.43256392833334E+03 0.41705725023263E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13183082522627E+00 0.00000000000000E+00 -.18699714889710E+02
 0.90326564537730E-03 0.12781818335818E+01 0.80000000000000E+04 0.30000000000000E+04 0.62588903940077E+01
 0.23470838977529E+01 0.35190991687646E+03 0.29218380179222E+03 0.35015207290322E+03 0.40294497527562E+03
 0.29215956693393E+03 0.29217249787249E+03 0.34559658583209E+03 0.40291808006994E+03 0.29215831182651E+03
 0.29217246825246E+03 0.35015207290322E+03 0.40294497527562E+03 0.29215956693393E+03 0.29217249787249E+03
 0.34559658583209E+03 0.40291808006994E+03 0.29215831182651E+03 0.29217246825246E+03 0.45291062633526E+03
 0.36479690401498E+03 0.18782851717822E+04 0.17316119131069E+04 0.59624238749915E+03 0.88895885975982E+03
 0.28973526032318E+03 0.12485543523151E+04 0.14171660295848E+04 0.11390349460767E+04 0.20073008786796E+04
 0.11535821301740E+04 0.14167379681664E+04 0.10676348474046E+04 0.20071062093527E+04 0.12485543523151E+04
 0.14171660295848E+04 0.11390349460767E+04 0.20073008786796E+04 0.11535821301740E+04 0.14167379681664E+04
 0.10676348474046E+04 0.20071062093527E+04 0.16562680093739E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56088711292272E+03 0.12948422677320E+01
 0.12948422677320E+01 0.54779332018723E+01 0.00000000000000E+00 0.34874893008072E+03 0.34874893008072E+03
 0.34874893008072E+03 0.34874893008072E+03 0.00000000000000E+00 0.00000000000000E+00 0.13378086133899E+00
 0.00000000000000E+00 -.10906811697408E+02 0.10000000000000E-02 0.17461483027553E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45815123419794E+01 0.17180671282423E+01 0.36480105126393E+03 0.45290657687560E+03
 0.31444150693610E+03 0.31444150693610E+03 0.29215313864650E+03 0.29215306276245E+03 0.31443559611064E+03
 0.31443559611064E+03 0.29215313884885E+03 0.29215306295991E+03 0.31444150693610E+03 0.31444150693610E+03
 0.29215313864650E+03 0.29215306276245E+03 0.31443559611064E+03 0.31443559611064E+03 0.29215313884885E+03
 0.29215306295991E+03 0.31139234635783E+03 0.29215916337183E+03 -.16173440180971E+03 -.22683308330847E+03
 0.27250758562970E+03 0.61009667742235E+03 0.33622655386450E+03 0.31591663455787E+03 0.25025779196664E+03
 0.31591663455787E+03 0.47738279198155E+03 0.31594649773669E+03 0.25006244053359E+03 0.31594649773669E+03
 0.47723488555827E+03 0.31591663455787E+03 0.25025779196664E+03 0.31591663455787E+03 0.47738279198155E+03
 0.31594649773669E+03 0.25006244053359E+03 0.31594649773669E+03 0.47723488555827E+03 0.23518035141100E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37735083439114E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16482023775643E+00 0.00000000000000E+00 0.00000000000000E+00 0.16482023775643E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17215317542969E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17215317542969E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255910589329E+00 0.17416845084436E+00 0.34874893008072E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1420.52043022
 0.98684127531809E+00 0.32355874117123E+03 0.48635861367858E+03 0.43280606907632E+03 0.41729215416307E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13177619714208E+00 0.00000000000000E+00 -.18713345477417E+02
 0.90276047812043E-03 0.12781926457843E+01 0.80000000000000E+04 0.30000000000000E+04 0.62588374501962E+01
 0.23470640438236E+01 0.35220364928020E+03 0.29218536497856E+03 0.35043371262678E+03 0.40335499868171E+03
 0.29216006970443E+03 0.29217365566892E+03 0.34586946970148E+03 0.40332814598818E+03 0.29215875207936E+03
 0.29217362476991E+03 0.35043371262678E+03 0.40335499868171E+03 0.29216006970443E+03 0.29217365566892E+03
 0.34586946970148E+03 0.40332814598818E+03 0.29215875207936E+03 0.29217362476991E+03 0.45329659157817E+03
 0.36516731039365E+03 0.18807997329897E+04 0.17334181366072E+04 0.59501151740070E+03 0.88644159203124E+03
 0.28845501704354E+03 0.12502143196393E+04 0.14178490954217E+04 0.11401948731561E+04 0.20065857338743E+04
 0.11553780525569E+04 0.14174226055952E+04 0.10689956777044E+04 0.20063920929940E+04 0.12502143196393E+04
 0.14178490954217E+04 0.11401948731561E+04 0.20065857338743E+04 0.11553780525569E+04 0.14174226055952E+04
 0.10689956777044E+04 0.20063920929940E+04 0.16560675520413E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56115628714060E+03 0.12948423681611E+01
 0.12948423681611E+01 0.55193294655517E+01 0.00000000000000E+00 0.34894438924063E+03 0.34894438924063E+03
 0.34894438924063E+03 0.34894438924063E+03 0.00000000000000E+00 0.00000000000000E+00 0.13368620002415E+00
 0.00000000000000E+00 -.10906149454630E+02 0.10000000000000E-02 0.17468878629670E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45795727187735E+01 0.17173397695401E+01 0.36517146194814E+03 0.45329255493918E+03
 0.31458192070640E+03 0.31458192070640E+03 0.29215326658560E+03 0.29215322580035E+03 0.31457595896959E+03
 0.31457595896959E+03 0.29215326678745E+03 0.29215322599968E+03 0.31458192070640E+03 0.31458192070640E+03
 0.29215326658560E+03 0.29215322580035E+03 0.31457595896959E+03 0.31457595896959E+03 0.29215326678745E+03
 0.29215322599968E+03 0.31151667691485E+03 0.29215959252566E+03 -.16374300568413E+03 -.22975788747807E+03
 0.27346999144306E+03 0.61176057439807E+03 0.33692323299779E+03 0.31759633226519E+03 0.25107423474522E+03
 0.31759633226519E+03 0.47859146055375E+03 0.31762652857323E+03 0.25087792223470E+03 0.31762652857323E+03
 0.47844300843339E+03 0.31759633226519E+03 0.25107423474522E+03 0.31759633226519E+03 0.47859146055375E+03
 0.31762652857323E+03 0.25087792223470E+03 0.31762652857323E+03 0.47844300843339E+03 0.23524026877248E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37756502687992E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16482403758513E+00 0.00000000000000E+00 0.00000000000000E+00 0.16482403758513E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17211193912497E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17211193912497E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255876126133E+00 0.17407054489164E+00 0.34894438924063E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1430.37214820
 0.98677497599438E+00 0.32373036161177E+03 0.48661010764270E+03 0.43303488848282E+03 0.41751427526977E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13172466334425E+00 0.00000000000000E+00 -.18718721820977E+02
 0.90228184629923E-03 0.12782019919102E+01 0.80000000000000E+04 0.30000000000000E+04 0.62587916860032E+01
 0.23470468822512E+01 0.35248216737645E+03 0.29218690282561E+03 0.35070079044355E+03 0.40374396856127E+03
 0.29216056728284E+03 0.29217480016229E+03 0.34612823252211E+03 0.40371715593951E+03 0.29215918796288E+03
 0.29217476801058E+03 0.35070079044355E+03 0.40374396856127E+03 0.29216056728284E+03 0.29217480016229E+03
 0.34612823252211E+03 0.40371715593951E+03 0.29215918796288E+03 0.29217476801058E+03 0.45366366187299E+03
 0.36551944859873E+03 0.18831708493130E+04 0.17351177933504E+04 0.59379532071307E+03 0.88398622936165E+03
 0.28722193204501E+03 0.12517809891612E+04 0.14184683698665E+04 0.11412874706247E+04 0.20058715736271E+04
 0.11570745040809E+04 0.14180433614280E+04 0.10702796671331E+04 0.20056789012503E+04 0.12517809891612E+04
 0.14184683698665E+04 0.11412874706247E+04 0.20058715736271E+04 0.11570745040809E+04 0.14180433614280E+04
 0.10702796671331E+04 0.20056789012503E+04 0.16558413888152E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56141019458180E+03 0.12948424077736E+01
 0.12948424077736E+01 0.55587363374803E+01 0.00000000000000E+00 0.34912950238138E+03 0.34912950238138E+03
 0.34912950238138E+03 0.34912950238138E+03 0.00000000000000E+00 0.00000000000000E+00 0.13359787535587E+00
 0.00000000000000E+00 -.10896896451314E+02 0.10000000000000E-02 0.17475635673951E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45778020034628E+01 0.17166757512985E+01 0.36552357957872E+03 0.45365965789616E+03
 0.31471498271268E+03 0.31471498271268E+03 0.29215343010052E+03 0.29215338727368E+03 0.31470897266068E+03
 0.31470897266068E+03 0.29215343030376E+03 0.29215338747438E+03 0.31471498271268E+03 0.31471498271268E+03
 0.29215343010052E+03 0.29215338727368E+03 0.31470897266068E+03 0.31470897266068E+03 0.29215343030376E+03
 0.29215338747438E+03 0.31163449887483E+03 0.29216001501904E+03 -.16566680392244E+03 -.23255883442999E+03
 0.27438311106782E+03 0.61333218864826E+03 0.33757716202510E+03 0.31919709593665E+03 0.25184840238468E+03
 0.31919709593665E+03 0.47973172741976E+03 0.31922760899681E+03 0.25165115909514E+03 0.31922760899681E+03
 0.47958273793761E+03 0.31919709593665E+03 0.25184840238468E+03 0.31919709593665E+03 0.47973172741976E+03
 0.31922760899681E+03 0.25165115909514E+03 0.31922760899681E+03 0.47958273793761E+03 0.23528724450045E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37776486532632E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16475904153178E+00 0.00000000000000E+00 0.00000000000000E+00 0.16475904153178E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17202991223456E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17202991223456E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255868464877E+00 0.17397818900830E+00 0.34912950238138E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1440.85352606
 0.98670935232191E+00 0.32391204605887E+03 0.48687571510397E+03 0.43327645632547E+03 0.41774883833914E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13167038579987E+00 0.00000000000000E+00 -.18727684634896E+02
 0.90177567055549E-03 0.12782098809394E+01 0.80000000000000E+04 0.30000000000000E+04 0.62587530571432E+01
 0.23470323964287E+01 0.35277718954793E+03 0.29218859687513E+03 0.35098372053603E+03 0.40415625378808E+03
 0.29216111888190E+03 0.29217606731277E+03 0.34640233150947E+03 0.40412948328959E+03 0.29215967137842E+03
 0.29217603378775E+03 0.35098372053603E+03 0.40415625378808E+03 0.29216111888190E+03 0.29217606731277E+03
 0.34640233150947E+03 0.40412948328959E+03 0.29215967137842E+03 0.29217603378775E+03 0.45405366378319E+03
 0.36589362039592E+03 0.18856706967165E+04 0.17369052536235E+04 0.59245850589154E+03 0.88131810347530E+03
 0.28589730505430E+03 0.12534342784171E+04 0.14190973017865E+04 0.11424376421097E+04 0.20050810364008E+04
 0.11588662390730E+04 0.14186738535694E+04 0.10716336825405E+04 0.20048893838930E+04 0.12534342784171E+04
 0.14190973017865E+04 0.11424376421097E+04 0.20050810364008E+04 0.11588662390730E+04 0.14186738535694E+04
 0.10716336825405E+04 0.20048893838930E+04 0.16555666640679E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56167775350605E+03 0.12948424738109E+01
 0.12948424738109E+01 0.56006618488940E+01 0.00000000000000E+00 0.34932561410616E+03 0.34932561410616E+03
 0.34932561410616E+03 0.34932561410616E+03 0.00000000000000E+00 0.00000000000000E+00 0.13350575016987E+00
 0.00000000000000E+00 -.10890817645266E+02 0.10000000000000E-02 0.17482522934688E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45759985729101E+01 0.17159994648413E+01 0.36589771772552E+03 0.45404970200307E+03
 0.31485582203166E+03 0.31485582203166E+03 0.29215364849287E+03 0.29215356641982E+03 0.31484976073635E+03
 0.31484976073635E+03 0.29215364869918E+03 0.29215356662149E+03 0.31485582203166E+03 0.31485582203166E+03
 0.29215364849287E+03 0.29215356641982E+03 0.31484976073635E+03 0.31484976073635E+03 0.29215364869918E+03
 0.29215356662149E+03 0.31175920463091E+03 0.29216048077860E+03 -.16772497806441E+03 -.23555473958144E+03
 0.27535334575603E+03 0.61500298210209E+03 0.33827286961727E+03 0.32090363558350E+03 0.25267086009942E+03
 0.32090363558350E+03 0.48094465133793E+03 0.32093448517160E+03 0.25247260933738E+03 0.32093448517160E+03
 0.48079507204009E+03 0.32090363558350E+03 0.25267086009942E+03 0.32090363558350E+03 0.48094465133793E+03
 0.32093448517160E+03 0.25247260933738E+03 0.32093448517160E+03 0.48079507204009E+03 0.23533302409059E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37797747668929E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16472057261794E+00 0.00000000000000E+00 0.00000000000000E+00 0.16472057261794E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17195838743856E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17195838743856E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255849620906E+00 0.17388033804435E+00 0.34932561410616E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1450.10343916
 0.98665687975026E+00 0.32407171438458E+03 0.48710895389770E+03 0.43348834922399E+03 0.41795457788313E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13162309900908E+00 0.00000000000000E+00 -.18734134480115E+02
 0.90133131332867E-03 0.12782119327359E+01 0.80000000000000E+04 0.30000000000000E+04 0.62587430105401E+01
 0.23470286289525E+01 0.35303702209978E+03 0.29219013922030E+03 0.35123291469388E+03 0.40451822045560E+03
 0.29216162394880E+03 0.29217722624430E+03 0.34664384823004E+03 0.40449148712444E+03 0.29216011418664E+03
 0.29217719147442E+03 0.35123291469388E+03 0.40451822045560E+03 0.29216162394880E+03 0.29217722624430E+03
 0.34664384823004E+03 0.40449148712445E+03 0.29216011418664E+03 0.29217719147442E+03 0.45439420105216E+03
 0.36621974069505E+03 0.18878586546877E+04 0.17384630120282E+04 0.59131311261932E+03 0.87902456975331E+03
 0.28475489157089E+03 0.12548831984100E+04 0.14196434361920E+04 0.11434413008948E+04 0.20043828315133E+04
 0.11604368577527E+04 0.14192213713419E+04 0.10728162258698E+04 0.20041920870137E+04 0.12548831984100E+04
 0.14196434361920E+04 0.11434413008948E+04 0.20043828315133E+04 0.11604368577527E+04 0.14192213713419E+04
 0.10728162258698E+04 0.20041920870137E+04 0.16553306016635E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56191196642127E+03 0.12948425213329E+01
 0.12948425213329E+01 0.56376615013168E+01 0.00000000000000E+00 0.34949835454783E+03 0.34949835454783E+03
 0.34949835454783E+03 0.34949835454783E+03 0.00000000000000E+00 0.00000000000000E+00 0.13342597592753E+00
 0.00000000000000E+00 -.10884070710955E+02 0.10000000000000E-02 0.17488334306895E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45744779689201E+01 0.17154292383450E+01 0.36622381648493E+03 0.45439026821317E+03
 0.31498015494611E+03 0.31498015494611E+03 0.29215381642260E+03 0.29215373057193E+03 0.31497404837899E+03
 0.31497404837899E+03 0.29215381662933E+03 0.29215373077401E+03 0.31498015494611E+03 0.31498015494611E+03
 0.29215381642260E+03 0.29215373057193E+03 0.31497404837899E+03 0.31497404837899E+03 0.29215381662933E+03
 0.29215373077401E+03 0.31186933326351E+03 0.29216090512255E+03 -.16951070754280E+03 -.23814982192225E+03
 0.27620699558637E+03 0.61647041954999E+03 0.33888238898569E+03 0.32239386716398E+03 0.25339427937515E+03
 0.32239386716398E+03 0.48200966651121E+03 0.32242501521839E+03 0.25319515488953E+03 0.32242501521839E+03
 0.48185958241552E+03 0.32239386716398E+03 0.25339427937515E+03 0.32239386716398E+03 0.48200966651121E+03
 0.32242501521839E+03 0.25319515488953E+03 0.32242501521839E+03 0.48185958241552E+03 0.23538183329254E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37816460753042E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16467402295856E+00 0.00000000000000E+00 0.00000000000000E+00 0.16467402295856E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17189508070454E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17189508070454E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255837082851E+00 0.17379428254871E+00 0.34949835454783E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1462.43665664
 0.98658246903581E+00 0.32428422400705E+03 0.48741835981383E+03 0.43376993365107E+03 0.41822818035964E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13156094783189E+00 0.00000000000000E+00 -.18738037578520E+02
 0.90074061906505E-03 0.12782072089265E+01 0.80000000000000E+04 0.30000000000000E+04 0.62587661406783E+01
 0.23470373027544E+01 0.35338252745020E+03 0.29219226529436E+03 0.35156429124203E+03 0.40499838925081E+03
 0.29216232442321E+03 0.29217883157808E+03 0.34696511523960E+03 0.40497170533272E+03 0.29216072857299E+03
 0.29217879510043E+03 0.35156429124203E+03 0.40499838925081E+03 0.29216232442321E+03 0.29217883157808E+03
 0.34696511523960E+03 0.40497170533272E+03 0.29216072857299E+03 0.29217879510043E+03 0.45484451280617E+03
 0.36665017987725E+03 0.18907607812211E+04 0.17405280546148E+04 0.58980994458125E+03 0.87601500957304E+03
 0.28325601526889E+03 0.12568051861926E+04 0.14203587901729E+04 0.11447718338691E+04 0.20034490100320E+04
 0.11625205534662E+04 0.14199385722674E+04 0.10743839348530E+04 0.20032594816297E+04 0.12568051861926E+04
 0.14203587901729E+04 0.11447718338691E+04 0.20034490100320E+04 0.11625205534662E+04 0.14199385722674E+04
 0.10743839348530E+04 0.20032594816297E+04 0.16550225663508E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56222344556789E+03 0.12948425500906E+01
 0.12948425500906E+01 0.56869943712137E+01 0.00000000000000E+00 0.34972737103931E+03 0.34972737103931E+03
 0.34972737103931E+03 0.34972737103931E+03 0.00000000000000E+00 0.00000000000000E+00 0.13332178383412E+00
 0.00000000000000E+00 -.10869747347305E+02 0.10000000000000E-02 0.17495746002532E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45725400899410E+01 0.17147025337279E+01 0.36665423619622E+03 0.45484061110180E+03
 0.31514574001076E+03 0.31514574001076E+03 0.29215404950595E+03 0.29215395841201E+03 0.31513957312656E+03
 0.31513957312656E+03 0.29215404971257E+03 0.29215395861398E+03 0.31514574001076E+03 0.31514574001076E+03
 0.29215404950595E+03 0.29215395841201E+03 0.31513957312656E+03 0.31513957312656E+03 0.29215404971257E+03
 0.29215395861398E+03 0.31201604629568E+03 0.29216149050885E+03 -.17186936375854E+03 -.24157395890593E+03
 0.27733635824400E+03 0.61840266317098E+03 0.33967962313576E+03 0.32436241698451E+03 0.25435021206296E+03
 0.32436241698451E+03 0.48340956141603E+03 0.32439396419905E+03 0.25414993262636E+03 0.32439396419905E+03
 0.48325881318143E+03 0.32436241698451E+03 0.25435021206296E+03 0.32436241698451E+03 0.48340956141603E+03
 0.32439396419906E+03 0.25414993262636E+03 0.32439396419906E+03 0.48325881318143E+03 0.23544366766027E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37841091162633E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16456943711415E+00 0.00000000000000E+00 0.00000000000000E+00 0.16456943711415E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17178442150713E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17178442150713E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255836059703E+00 0.17368048885879E+00 0.34972737103931E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1471.68656974
 0.98651308800120E+00 0.32444377283763E+03 0.48764949272070E+03 0.43398129982027E+03 0.41843383859019E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13151471428060E+00 0.00000000000000E+00 -.18735985158024E+02
 0.90029768809602E-03 0.12782012166312E+01 0.80000000000000E+04 0.30000000000000E+04 0.62587954822050E+01
 0.23470483058269E+01 0.35364097862402E+03 0.29219391301284E+03 0.35181217956623E+03 0.40535675429506E+03
 0.29216287056250E+03 0.29218008167799E+03 0.34720551016537E+03 0.40533010735626E+03 0.29216120779087E+03
 0.29218004388317E+03 0.35181217956623E+03 0.40535675429506E+03 0.29216287056250E+03 0.29218008167799E+03
 0.34720551016537E+03 0.40533010735626E+03 0.29216120779087E+03 0.29218004388317E+03 0.45517968574213E+03
 0.36696983777411E+03 0.18929368531367E+04 0.17420817595161E+04 0.58870338508222E+03 0.87379752751627E+03
 0.28215062550864E+03 0.12582442988278E+04 0.14208917486178E+04 0.11457713631196E+04 0.20027534206756E+04
 0.11640804130100E+04 0.14204729268902E+04 0.10755601333910E+04 0.20025648171859E+04 0.12582442988278E+04
 0.14208917486178E+04 0.11457713631196E+04 0.20027534206756E+04 0.11640804130100E+04 0.14204729268902E+04
 0.10755601333910E+04 0.20025648171859E+04 0.16548067542841E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56245824048086E+03 0.12948425349686E+01
 0.12948425349686E+01 0.57239940236364E+01 0.00000000000000E+00 0.34989739498105E+03 0.34989739498105E+03
 0.34989739498105E+03 0.34989739498105E+03 0.00000000000000E+00 0.00000000000000E+00 0.13324523326288E+00
 0.00000000000000E+00 -.10853222797718E+02 0.10000000000000E-02 0.17501097632138E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45711418610163E+01 0.17141781978811E+01 0.36697387932523E+03 0.45517580845342E+03
 0.31526970345316E+03 0.31526970345316E+03 0.29215423137613E+03 0.29215413619097E+03 0.31526349137639E+03
 0.31526349137639E+03 0.29215423158210E+03 0.29215413639231E+03 0.31526970345316E+03 0.31526970345316E+03
 0.29215423137613E+03 0.29215413619097E+03 0.31526349137639E+03 0.31526349137639E+03 0.29215423158210E+03
 0.29215413639231E+03 0.31212591841055E+03 0.29216194452928E+03 -.17362876575931E+03 -.24412698463665E+03
 0.27817088005090E+03 0.61981936905731E+03 0.34025763460615E+03 0.32582246362675E+03 0.25505490249830E+03
 0.32582246362675E+03 0.48443226623763E+03 0.32585431107273E+03 0.25485376496306E+03 0.32585431107273E+03
 0.48428102670961E+03 0.32582246362675E+03 0.25505490249830E+03 0.32582246362675E+03 0.48443226623763E+03
 0.32585431107273E+03 0.25485376496306E+03 0.32585431107273E+03 0.48428102670961E+03 0.23547770338889E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37859189086742E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16444516860796E+00 0.00000000000000E+00 0.00000000000000E+00 0.16444516860796E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17167125232322E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17167125232322E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255852368265E+00 0.17359628553267E+00 0.34989739498105E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1480.14082644
 0.98644717509577E+00 0.32458978583038E+03 0.48786070619067E+03 0.43417466013577E+03 0.41862204452309E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13147243125354E+00 0.00000000000000E+00 -.18744038534292E+02
 0.89989262796841E-03 0.12781960584672E+01 0.80000000000000E+04 0.30000000000000E+04 0.62588207395928E+01
 0.23470577773473E+01 0.35387713856761E+03 0.29219545583155E+03 0.35203867889071E+03 0.40568261551855E+03
 0.29216338420074E+03 0.29218125631490E+03 0.34742529634731E+03 0.40565600264769E+03 0.29216165862866E+03
 0.29218121729120E+03 0.35203867889071E+03 0.40568261551855E+03 0.29216338420074E+03 0.29218125631490E+03
 0.34742529634731E+03 0.40565600264769E+03 0.29216165862866E+03 0.29218121729120E+03 0.45548226609465E+03
 0.36725715529490E+03 0.18949268186883E+04 0.17435043850415E+04 0.58775722602742E+03 0.87187515807453E+03
 0.28117914591698E+03 0.12595603409946E+04 0.14213945447628E+04 0.11466868325177E+04 0.20021483663935E+04
 0.11655058660631E+04 0.14209770181463E+04 0.10766353518271E+04 0.20019606265619E+04 0.12595603409946E+04
 0.14213945447628E+04 0.11466868325177E+04 0.20021483663935E+04 0.11655058660631E+04 0.14209770181463E+04
 0.10766353518271E+04 0.20019606265619E+04 0.16546481787404E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56267319748973E+03 0.12948425943052E+01
 0.12948425943052E+01 0.57578110504432E+01 0.00000000000000E+00 0.35005209313156E+03 0.35005209313156E+03
 0.35005209313156E+03 0.35005209313156E+03 0.00000000000000E+00 0.00000000000000E+00 0.13317640939090E+00
 0.00000000000000E+00 -.10849484692418E+02 0.10000000000000E-02 0.17505810632541E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45699111957313E+01 0.17137166983992E+01 0.36726119042179E+03 0.45547840609447E+03
 0.31538325247438E+03 0.31538325247438E+03 0.29215436721996E+03 0.29215430348756E+03 0.31537699904548E+03
 0.31537699904548E+03 0.29215436742330E+03 0.29215430368793E+03 0.31538325247438E+03 0.31538325247438E+03
 0.29215436721996E+03 0.29215430348756E+03 0.31537699904548E+03 0.31537699904548E+03 0.29215436742330E+03
 0.29215430368793E+03 0.31222662228032E+03 0.29216236988584E+03 -.17519939931845E+03 -.24640194165393E+03
 0.27892611382712E+03 0.62110272716581E+03 0.34078198276956E+03 0.32713355648745E+03 0.25569180393329E+03
 0.32713355648745E+03 0.48535843098117E+03 0.32716567992914E+03 0.25548990668082E+03 0.32716567992914E+03
 0.48520676821015E+03 0.32713355648745E+03 0.25569180393329E+03 0.32713355648745E+03 0.48535843098117E+03
 0.32716567992914E+03 0.25548990668082E+03 0.32716567992914E+03 0.48520676821015E+03 0.23551751828468E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37876040997806E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16442250950466E+00 0.00000000000000E+00 0.00000000000000E+00 0.16442250950466E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17162276356103E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17162276356103E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255834569937E+00 0.17351939574011E+00 0.35005209313156E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1490.98138842
 0.98637763688566E+00 0.32477606599636E+03 0.48812970459423E+03 0.43442024592195E+03 0.41886105232921E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13141854318543E+00 0.00000000000000E+00 -.18750567119940E+02
 0.89937642314452E-03 0.12781900211482E+01 0.80000000000000E+04 0.30000000000000E+04 0.62588503020965E+01
 0.23470688632862E+01 0.35417886905028E+03 0.29219749176160E+03 0.35232809443071E+03 0.40609926363001E+03
 0.29216406558285E+03 0.29218281287002E+03 0.34770611520006E+03 0.40607269409167E+03 0.29216225692002E+03
 0.29218277223170E+03 0.35232809443071E+03 0.40609926363001E+03 0.29216406558285E+03 0.29218281287002E+03
 0.34770611520006E+03 0.40607269409167E+03 0.29216225692002E+03 0.29218277223170E+03 0.45587020476241E+03
 0.36762537461784E+03 0.18974500228018E+04 0.17453004198226E+04 0.58649550275474E+03 0.86934452911204E+03
 0.27991654884352E+03 0.12612325730448E+04 0.14220078285423E+04 0.11478454763604E+04 0.20013387026273E+04
 0.11673189086414E+04 0.14215919420983E+04 0.10779992769812E+04 0.20011520536055E+04 0.12612325730448E+04
 0.14220078285423E+04 0.11478454763604E+04 0.20013387026273E+04 0.11673189086414E+04 0.14215919420983E+04
 0.10779992769812E+04 0.20011520536055E+04 0.16544030374174E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56294487883467E+03 0.12948426424074E+01
 0.12948426424074E+01 0.58011732983284E+01 0.00000000000000E+00 0.35025018839719E+03 0.35025018839719E+03
 0.35025018839719E+03 0.35025018839719E+03 0.00000000000000E+00 0.00000000000000E+00 0.13308967217964E+00
 0.00000000000000E+00 -.10840469664604E+02 0.10000000000000E-02 0.17511588119046E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45684034740966E+01 0.17131513027862E+01 0.36762939276385E+03 0.45586637557936E+03
 0.31552823225821E+03 0.31552823225821E+03 0.29215459259643E+03 0.29215452557499E+03 0.31552192597756E+03
 0.31552192597756E+03 0.29215459279787E+03 0.29215452577350E+03 0.31552823225821E+03 0.31552823225821E+03
 0.29215459259643E+03 0.29215452557499E+03 0.31552192597756E+03 0.31552192597756E+03 0.29215459279787E+03
 0.29215452577350E+03 0.31235519637929E+03 0.29216293157644E+03 -.17722182764710E+03 -.24932986388393E+03
 0.27989729762041E+03 0.62275158159370E+03 0.34145479748519E+03 0.32882078136805E+03 0.25651122366536E+03
 0.32882078136805E+03 0.48654913766740E+03 0.32885325809974E+03 0.25630833466826E+03 0.32885325809974E+03
 0.48639691296633E+03 0.32882078136805E+03 0.25651122366536E+03 0.32882078136805E+03 0.48654913766740E+03
 0.32885325809974E+03 0.25630833466826E+03 0.32885325809974E+03 0.48639691296633E+03 0.23556768658644E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37897447844818E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16435996253209E+00 0.00000000000000E+00 0.00000000000000E+00 0.16435996253209E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17153896883548E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17153896883548E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255824041009E+00 0.17342116588255E+00 0.35025018839719E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1500.27329868
 0.98631547837279E+00 0.32493547270341E+03 0.48835906178570E+03 0.43462998997133E+03 0.41906533074654E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13137270157046E+00 0.00000000000000E+00 -.18755674940220E+02
 0.89893516211452E-03 0.12781835183890E+01 0.80000000000000E+04 0.30000000000000E+04 0.62588821440001E+01
 0.23470808040000E+01 0.35443684677303E+03 0.29219928904333E+03 0.35257555474005E+03 0.40645476027896E+03
 0.29216467035439E+03 0.29218419286075E+03 0.34794628726832E+03 0.40642822782117E+03 0.29216278814342E+03
 0.29218415080355E+03 0.35257555474005E+03 0.40645476027896E+03 0.29216467035439E+03 0.29218419286075E+03
 0.34794628726832E+03 0.40642822782117E+03 0.29216278814342E+03 0.29218415080355E+03 0.45620033957030E+03
 0.36793820317323E+03 0.18996004975434E+04 0.17468298437846E+04 0.58542561771104E+03 0.86720050320840E+03
 0.27884775740880E+03 0.12626580402459E+04 0.14225219601654E+04 0.11488322949435E+04 0.20006384741913E+04
 0.11688647689018E+04 0.14221074849104E+04 0.10791612445373E+04 0.20004527670904E+04 0.12626580402459E+04
 0.14225219601654E+04 0.11488322949435E+04 0.20006384741913E+04 0.11688647689018E+04 0.14221074849104E+04
 0.10791612445373E+04 0.20004527670904E+04 0.16541931166860E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56317700711356E+03 0.12948426800414E+01
 0.12948426800414E+01 0.58383409393728E+01 0.00000000000000E+00 0.35041896726971E+03 0.35041896726971E+03
 0.35041896726971E+03 0.35041896726971E+03 0.00000000000000E+00 0.00000000000000E+00 0.13301664685761E+00
 0.00000000000000E+00 -.10832255871946E+02 0.10000000000000E-02 0.17516347011609E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45671623168335E+01 0.17126858688125E+01 0.36794219758566E+03 0.45619654248806E+03
 0.31565230156873E+03 0.31565230156873E+03 0.29215479277819E+03 0.29215472283540E+03 0.31564595003582E+03
 0.31564595003582E+03 0.29215479297741E+03 0.29215472303171E+03 0.31565230156873E+03 0.31565230156873E+03
 0.29215479277819E+03 0.29215472283540E+03 0.31564595003582E+03 0.31564595003582E+03 0.29215479297741E+03
 0.29215472303171E+03 0.31246525818017E+03 0.29216342777953E+03 -.17894246846084E+03 -.25181874486172E+03
 0.28072298770835E+03 0.62414874962996E+03 0.34202214698306E+03 0.33025479909177E+03 0.25720703630292E+03
 0.33025479909177E+03 0.48755667669600E+03 0.33028757932924E+03 0.25700330370617E+03 0.33028757932924E+03
 0.48740397620374E+03 0.33025479909177E+03 0.25720703630292E+03 0.33025479909177E+03 0.48755667669600E+03
 0.33028757932924E+03 0.25700330370617E+03 0.33028757932924E+03 0.48740397620374E+03 0.23560788009411E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37915675076049E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16430225059258E+00 0.00000000000000E+00 0.00000000000000E+00 0.16430225059258E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17146553166076E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17146553166076E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255816643740E+00 0.17333757753843E+00 0.35041896726971E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1511.11386065
 0.98624708092448E+00 0.32512088396455E+03 0.48862530201192E+03 0.43487338367276E+03 0.41930243910782E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13131964342412E+00 0.00000000000000E+00 -.18761357475636E+02
 0.89842246331617E-03 0.12781739700381E+01 0.80000000000000E+04 0.30000000000000E+04 0.62589288997662E+01
 0.23470983374123E+01 0.35473704645153E+03 0.29220144781051E+03 0.35286353171123E+03 0.40686767267585E+03
 0.29216540066107E+03 0.29218585743079E+03 0.34822584910634E+03 0.40684118339868E+03 0.29216342987587E+03
 0.29218581367711E+03 0.35286353171123E+03 0.40686767267585E+03 0.29216540066107E+03 0.29218585743079E+03
 0.34822584910634E+03 0.40684118339868E+03 0.29216342987587E+03 0.29218581367711E+03 0.45658292170891E+03
 0.36830013920303E+03 0.19020917015697E+04 0.17485973875673E+04 0.58418814529960E+03 0.86472388401707E+03
 0.27761479799098E+03 0.12643109559572E+04 0.14231086148778E+04 0.11499739961916E+04 0.19998149214411E+04
 0.11706578803089E+04 0.14226957860493E+04 0.10805065321982E+04 0.19996303153455E+04 0.12643109559572E+04
 0.14231086148778E+04 0.11499739961916E+04 0.19998149214411E+04 0.11706578803089E+04 0.14226957860493E+04
 0.10805065321982E+04 0.19996303153455E+04 0.16539447249177E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56344595248100E+03 0.12948427219099E+01
 0.12948427219099E+01 0.58817031872580E+01 0.00000000000000E+00 0.35061518511693E+03 0.35061518511693E+03
 0.35061518511693E+03 0.35061518511693E+03 0.00000000000000E+00 0.00000000000000E+00 0.13293293046226E+00
 0.00000000000000E+00 -.10822481360542E+02 0.10000000000000E-02 0.17521660255538E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45657773768736E+01 0.17121665163276E+01 0.36830410856686E+03 0.45657915906954E+03
 0.31579681065424E+03 0.31579681065424E+03 0.29215503468864E+03 0.29215496121552E+03 0.31579040639468E+03
 0.31579040639468E+03 0.29215503488450E+03 0.29215496140852E+03 0.31579681065424E+03 0.31579681065424E+03
 0.29215503468864E+03 0.29215496121552E+03 0.31579040639468E+03 0.31579040639468E+03 0.29215503488450E+03
 0.29215496140852E+03 0.31259348390594E+03 0.29216402421046E+03 -.18093294082245E+03 -.25469467286549E+03
 0.28168262828920E+03 0.62576969146622E+03 0.34267865003557E+03 0.33191685849973E+03 0.25801534996897E+03
 0.33191685849973E+03 0.48872515392362E+03 0.33194999347805E+03 0.25781063860594E+03 0.33194999347805E+03
 0.48857190316469E+03 0.33191685849973E+03 0.25801534996897E+03 0.33191685849973E+03 0.48872515392362E+03
 0.33194999347805E+03 0.25781063860594E+03 0.33194999347805E+03 0.48857190316469E+03 0.23565716055019E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37936857285837E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16423310772957E+00 0.00000000000000E+00 0.00000000000000E+00 0.16423310772957E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17138000499947E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17138000499947E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255808733191E+00 0.17324050841292E+00 0.35061518511693E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1521.32020039
 0.98618295039537E+00 0.32529500297543E+03 0.48887463416937E+03 0.43510148639756E+03 0.41952476161147E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13127013054357E+00 0.00000000000000E+00 -.18766784586637E+02
 0.89794152116458E-03 0.12781627594763E+01 0.80000000000000E+04 0.30000000000000E+04 0.62589837958333E+01
 0.23471189234375E+01 0.35501887694691E+03 0.29220354274131E+03 0.35313390353433E+03 0.40725468133595E+03
 0.29216611333115E+03 0.29218747988864E+03 0.34848837516949E+03 0.40722823260395E+03 0.29216405635612E+03
 0.29218743449658E+03 0.35313390353433E+03 0.40725468133595E+03 0.29216611333115E+03 0.29218747988864E+03
 0.34848837516949E+03 0.40722823260395E+03 0.29216405635612E+03 0.29218743449658E+03 0.45694083622919E+03
 0.36863823109374E+03 0.19044223508694E+04 0.17502486869867E+04 0.58302900051468E+03 0.86240936271884E+03
 0.27646521720158E+03 0.12658581428983E+04 0.14236473097897E+04 0.11510412161325E+04 0.19990315531290E+04
 0.11723368297297E+04 0.14232360312686E+04 0.10817647189667E+04 0.19988479859946E+04 0.12658581428983E+04
 0.14236473097897E+04 0.11510412161325E+04 0.19990315531290E+04 0.11723368297297E+04 0.14232360312686E+04
 0.10817647189667E+04 0.19988479859946E+04 0.16537070236381E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56369788176739E+03 0.12948427618965E+01
 0.12948427618965E+01 0.59225285462389E+01 0.00000000000000E+00 0.35079901016135E+03 0.35079901016135E+03
 0.35079901016135E+03 0.35079901016136E+03 0.00000000000000E+00 0.00000000000000E+00 0.13285552349598E+00
 0.00000000000000E+00 -.10813434254565E+02 0.10000000000000E-02 0.17526447717546E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45645302053942E+01 0.17116988270228E+01 0.36864217719323E+03 0.45693710546541E+03
 0.31593257093227E+03 0.31593257093227E+03 0.29215528072322E+03 0.29215519401738E+03 0.31592611711169E+03
 0.31592611711169E+03 0.29215528091549E+03 0.29215519420649E+03 0.31593257093227E+03 0.31593257093227E+03
 0.29215528072322E+03 0.29215519401738E+03 0.31592611711169E+03 0.31592611711169E+03 0.29215528091549E+03
 0.29215519420649E+03 0.31271397497427E+03 0.29216460344330E+03 -.18279522165319E+03 -.25738289384863E+03
 0.28258097065590E+03 0.62728354842552E+03 0.34328967291635E+03 0.33347151760439E+03 0.25877141316756E+03
 0.33347151760439E+03 0.48981557998883E+03 0.33350498707658E+03 0.25856578376486E+03 0.33350498707658E+03
 0.48966181386724E+03 0.33347151760439E+03 0.25877141316756E+03 0.33347151760439E+03 0.48981557998883E+03
 0.33350498707658E+03 0.25856578376486E+03 0.33350498707658E+03 0.48966181386724E+03 0.23570237916051E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37956700809640E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16416927508842E+00 0.00000000000000E+00 0.00000000000000E+00 0.16416927508842E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17130005840382E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17130005840382E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255801040731E+00 0.17314966552306E+00 0.35079901016135E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1530.58750808
 0.98612466998221E+00 0.32545274187070E+03 0.48909999606939E+03 0.43530779788933E+03 0.41972592896935E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13122552921613E+00 0.00000000000000E+00 -.18771452998370E+02
 0.89750626975707E-03 0.12781506784009E+01 0.80000000000000E+04 0.30000000000000E+04 0.62590429557247E+01
 0.23471411083968E+01 0.35527416305765E+03 0.29220549821946E+03 0.35337882217480E+03 0.40760455745389E+03
 0.29216678195851E+03 0.29218900042658E+03 0.34872624510252E+03 0.40757814549945E+03 0.29216464433150E+03
 0.29218895351208E+03 0.35337882217480E+03 0.40760455745389E+03 0.29216678195851E+03 0.29218900042658E+03
 0.34872624510252E+03 0.40757814549945E+03 0.29216464433150E+03 0.29218895351208E+03 0.45726357006579E+03
 0.36894259406148E+03 0.19065270360862E+04 0.17517378563970E+04 0.58198999724792E+03 0.86033499259629E+03
 0.27543504536213E+03 0.12672559569119E+04 0.14241276450896E+04 0.11520041099111E+04 0.19983173881199E+04
 0.11738539895561E+04 0.14237177771488E+04 0.10829002733575E+04 0.19981347686915E+04 0.12672559569119E+04
 0.14241276450896E+04 0.11520041099111E+04 0.19983173881200E+04 0.11738539895561E+04 0.14237177771488E+04
 0.10829002733575E+04 0.19981347686915E+04 0.16534928145331E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56392567742778E+03 0.12948427962930E+01
 0.12948427962930E+01 0.59595977770034E+01 0.00000000000000E+00 0.35096516502313E+03 0.35096516502313E+03
 0.35096516502313E+03 0.35096516502313E+03 0.00000000000000E+00 0.00000000000000E+00 0.13278638598551E+00
 0.00000000000000E+00 -.10805017737529E+02 0.10000000000000E-02 0.17530620945929E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45634436022973E+01 0.17112913508615E+01 0.36894651908294E+03 0.45725986807026E+03
 0.31605565470260E+03 0.31605565470260E+03 0.29215550294195E+03 0.29215541258738E+03 0.31604915592879E+03
 0.31604915592879E+03 0.29215550312994E+03 0.29215541277228E+03 0.31605565470260E+03 0.31605565470260E+03
 0.29215550294195E+03 0.29215541258738E+03 0.31604915592879E+03 0.31604915592879E+03 0.29215550312994E+03
 0.29215541277228E+03 0.31282324366223E+03 0.29216514449962E+03 -.18447209369039E+03 -.25980100055868E+03
 0.28339194300443E+03 0.62864688428056E+03 0.34383798156110E+03 0.33487245300118E+03 0.25945339258138E+03
 0.33487245300118E+03 0.49079675441812E+03 0.33490622682027E+03 0.25924693615431E+03 0.33490622682027E+03
 0.49064252640370E+03 0.33487245300118E+03 0.25945339258138E+03 0.33487245300118E+03 0.49079675441813E+03
 0.33490622682027E+03 0.25924693615431E+03 0.33490622682027E+03 0.49064252640371E+03 0.23574353105696E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37974636187329E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16410936745701E+00 0.00000000000000E+00 0.00000000000000E+00 0.16410936745701E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17122776879410E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17122776879410E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255794809431E+00 0.17306764443328E+00 0.35096516502313E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1541.70827731
 0.98605469279286E+00 0.32564162553544E+03 0.48936920904155E+03 0.43555442502294E+03 0.41996651184933E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13117242680613E+00 0.00000000000000E+00 -.18777095444704E+02
 0.89698563440546E-03 0.12781339639940E+01 0.80000000000000E+04 0.30000000000000E+04 0.62591248064489E+01
 0.23471718024183E+01 0.35557974702741E+03 0.29220791233703E+03 0.35367201061931E+03 0.40802252864818E+03
 0.29216761174669E+03 0.29219088533735E+03 0.34901106751478E+03 0.40799616078924E+03 0.29216537429915E+03
 0.29219083655214E+03 0.35367201061931E+03 0.40802252864818E+03 0.29216761174669E+03 0.29219088533735E+03
 0.34901106751478E+03 0.40799616078924E+03 0.29216537429915E+03 0.29219083655214E+03 0.45764814875988E+03
 0.36930462109049E+03 0.19090392138513E+04 0.17535133612789E+04 0.58075958867840E+03 0.85787877263031E+03
 0.27421538600852E+03 0.12689251593775E+04 0.14246942600228E+04 0.11531527402032E+04 0.19974583051961E+04
 0.11756660351019E+04 0.14242860873859E+04 0.10842551657284E+04 0.19972768273378E+04 0.12689251593775E+04
 0.14246942600228E+04 0.11531527402032E+04 0.19974583051961E+04 0.11756660351019E+04 0.14242860873859E+04
 0.10842551657284E+04 0.19972768273378E+04 0.16532386213043E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56419790029945E+03 0.12948428378662E+01
 0.12948428378662E+01 0.60040808539208E+01 0.00000000000000E+00 0.35116360838116E+03 0.35116360838116E+03
 0.35116360838116E+03 0.35116360838116E+03 0.00000000000000E+00 0.00000000000000E+00 0.13270481549433E+00
 0.00000000000000E+00 -.10795029016879E+02 0.10000000000000E-02 0.17535418922044E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45621949698293E+01 0.17108231136860E+01 0.36930852285840E+03 0.45764447971868E+03
 0.31620311904933E+03 0.31620311904933E+03 0.29215577892693E+03 0.29215568404082E+03 0.31619656640484E+03
 0.31619656640484E+03 0.29215577910886E+03 0.29215568421976E+03 0.31620311904933E+03 0.31620311904933E+03
 0.29215577892693E+03 0.29215568404082E+03 0.31619656640484E+03 0.31619656640484E+03 0.29215577910886E+03
 0.29215568421976E+03 0.31295419215285E+03 0.29216581294852E+03 -.18646729397336E+03 -.26267507080196E+03
 0.28435925034653E+03 0.63026897386600E+03 0.34448792726774E+03 0.33654047714654E+03 0.26026614877600E+03
 0.33654047714654E+03 0.49196311501252E+03 0.33657461687572E+03 0.26005870742811E+03 0.33657461687572E+03
 0.49180833950847E+03 0.33654047714654E+03 0.26026614877600E+03 0.33654047714654E+03 0.49196311501252E+03
 0.33657461687572E+03 0.26005870742811E+03 0.33657461687572E+03 0.49180833950847E+03 0.23579289817864E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37996057592757E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16403835983647E+00 0.00000000000000E+00 0.00000000000000E+00 0.16403835983647E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17114147348045E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17114147348045E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255787221917E+00 0.17296978451480E+00 0.35116360838116E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1552.38610866
 0.98599236990838E+00 0.32582208413521E+03 0.48962599247193E+03 0.43578952454487E+03 0.42019588508521E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13112191001478E+00 0.00000000000000E+00 -.18785919645817E+02
 0.89648875521199E-03 0.12781162632685E+01 0.80000000000000E+04 0.30000000000000E+04 0.62592114895260E+01
 0.23472043085722E+01 0.35587194233785E+03 0.29221030635908E+03 0.35395237759558E+03 0.40842228766343E+03
 0.29216843956451E+03 0.29219276333595E+03 0.34928342647443E+03 0.40839596176787E+03 0.29216610284349E+03
 0.29219271270572E+03 0.35395237759558E+03 0.40842228766343E+03 0.29216843956451E+03 0.29219276333595E+03
 0.34928342647443E+03 0.40839596176787E+03 0.29216610284349E+03 0.29219271270572E+03 0.45801641986167E+03
 0.36965127815703E+03 0.19114311156604E+04 0.17551994312403E+04 0.57955021017160E+03 0.85548660051571E+03
 0.27303863929325E+03 0.12705160262123E+04 0.14252138020383E+04 0.11542446649600E+04 0.19966102870826E+04
 0.11773942895208E+04 0.14248072466008E+04 0.10855451887632E+04 0.19964298989918E+04 0.12705160262123E+04
 0.14252138020383E+04 0.11542446649600E+04 0.19966102870826E+04 0.11773942895208E+04 0.14248072466008E+04
 0.10855451887632E+04 0.19964298989918E+04 0.16529695676668E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56445693618062E+03 0.12948429028823E+01
 0.12948429028823E+01 0.60467921793156E+01 0.00000000000000E+00 0.35135335202973E+03 0.35135335202973E+03
 0.35135335202973E+03 0.35135335202973E+03 0.00000000000000E+00 0.00000000000000E+00 0.13262787413142E+00
 0.00000000000000E+00 -.10789396229270E+02 0.10000000000000E-02 0.17539810734846E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45610526367348E+01 0.17103947387756E+01 0.36965514865747E+03 0.45801278961301E+03
 0.31634404677688E+03 0.31634404677688E+03 0.29215610446893E+03 0.29215595508179E+03 0.31633744257659E+03
 0.31633744257659E+03 0.29215610464541E+03 0.29215595525395E+03 0.31634404677688E+03 0.31634404677688E+03
 0.29215610446893E+03 0.29215595508179E+03 0.31633744257659E+03 0.31633744257659E+03 0.29215610464541E+03
 0.29215595525395E+03 0.31307933898873E+03 0.29216647640086E+03 -.18838818885652E+03 -.26544085184913E+03
 0.28528649570470E+03 0.63182484407128E+03 0.34511191588806E+03 0.33814266397018E+03 0.26104512528529E+03
 0.33814266397018E+03 0.49308255184587E+03 0.33817715462605E+03 0.26083672685421E+03 0.33817715462605E+03
 0.49292723830936E+03 0.33814266397018E+03 0.26104512528529E+03 0.33814266397018E+03 0.49308255184587E+03
 0.33817715462605E+03 0.26083672685421E+03 0.33817715462605E+03 0.49292723830936E+03 0.23583828606270E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38016643927379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16400242591760E+00 0.00000000000000E+00 0.00000000000000E+00 0.16400242591760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17107574012407E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17107574012407E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255768677120E+00 0.17287619833539E+00 0.35135335202973E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1563.06394001
 0.98593637608804E+00 0.32600181781886E+03 0.48988155017976E+03 0.43602322036703E+03 0.42042387670899E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13107203780189E+00 0.00000000000000E+00 -.18793386608173E+02
 0.89599443049666E-03 0.12780934709465E+01 0.80000000000000E+04 0.30000000000000E+04 0.62593231104414E+01
 0.23472461664155E+01 0.35616356299970E+03 0.29221277068722E+03 0.35423220924684E+03 0.40881989826905E+03
 0.29216929628911E+03 0.29219470462691E+03 0.34955538381616E+03 0.40879361447256E+03 0.29216685711799E+03
 0.29219465210693E+03 0.35423220924684E+03 0.40881989826905E+03 0.29216929628911E+03 0.29219470462691E+03
 0.34955538381616E+03 0.40879361447256E+03 0.29216685711799E+03 0.29219465210693E+03 0.45838065592034E+03
 0.36999334745449E+03 0.19138030383263E+04 0.17568638997880E+04 0.57838226547714E+03 0.85316567397135E+03
 0.27189149716683E+03 0.12720958492837E+04 0.14257257160711E+04 0.11553241811585E+04 0.19957649169934E+04
 0.11791108939256E+04 0.14253207827463E+04 0.10868215704650E+04 0.19955856241027E+04 0.12720958492837E+04
 0.14257257160711E+04 0.11553241811585E+04 0.19957649169934E+04 0.11791108939256E+04 0.14253207827463E+04
 0.10868215704650E+04 0.19955856241027E+04 0.16527098033748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56471386338505E+03 0.12948429578983E+01
 0.12948429578983E+01 0.60895035047105E+01 0.00000000000000E+00 0.35154271266750E+03 0.35154271266750E+03
 0.35154271266750E+03 0.35154271266750E+03 0.00000000000000E+00 0.00000000000000E+00 0.13255224020764E+00
 0.00000000000000E+00 -.10782484103156E+02 0.10000000000000E-02 0.17543981404912E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45599683534550E+01 0.17099881325456E+01 0.36999719404560E+03 0.45837705755063E+03
 0.31648500970433E+03 0.31648500970433E+03 0.29215639223618E+03 0.29215623580677E+03 0.31647835393798E+03
 0.31647835393798E+03 0.29215639240466E+03 0.29215623597113E+03 0.31648500970433E+03 0.31648500970433E+03
 0.29215639223618E+03 0.29215623580677E+03 0.31647835393798E+03 0.31647835393798E+03 0.29215639240466E+03
 0.29215623597113E+03 0.31320456507172E+03 0.29216715987337E+03 -.19027263990420E+03 -.26814855394044E+03
 0.28621066287254E+03 0.63337250246570E+03 0.34573078627880E+03 0.33972614684429E+03 0.26182129833116E+03
 0.33972614684429E+03 0.49419578087207E+03 0.33976098990151E+03 0.26161196210962E+03 0.33976098990151E+03
 0.49403994858060E+03 0.33972614684429E+03 0.26182129833116E+03 0.33972614684429E+03 0.49419578087207E+03
 0.33976098990151E+03 0.26161196210962E+03 0.33976098990151E+03 0.49403994858060E+03 0.23589378203271E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38037172549544E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16395484828612E+00 0.00000000000000E+00 0.00000000000000E+00 0.16395484828612E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17100954696672E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17100954696672E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255753939126E+00 0.17278294173088E+00 0.35154271266750E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1570.18249424
 0.98589897611773E+00 0.32612140356898E+03 0.49005120142613E+03 0.43617846147195E+03 0.42057539136788E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13103918320789E+00 0.00000000000000E+00 -.18796934147706E+02
 0.89566584613048E-03 0.12780751055039E+01 0.80000000000000E+04 0.30000000000000E+04 0.62594130544824E+01
 0.23472798954309E+01 0.35635756438774E+03 0.29221445317601E+03 0.35441837805997E+03 0.40908390557983E+03
 0.29216988380272E+03 0.29219603460252E+03 0.34973635751727E+03 0.40905764983911E+03 0.29216737453898E+03
 0.29219598079771E+03 0.35441837805997E+03 0.40908390557983E+03 0.29216988380272E+03 0.29219603460252E+03
 0.34973635751726E+03 0.40905764983911E+03 0.29216737453898E+03 0.29219598079771E+03 0.45862187932472E+03
 0.37021955797260E+03 0.19153757828449E+04 0.17579659375690E+04 0.57761364861565E+03 0.85163817618121E+03
 0.27113645932249E+03 0.12731438473806E+04 0.14260605988119E+04 0.11560392536621E+04 0.19951990423276E+04
 0.11802498569232E+04 0.14256567461950E+04 0.10876673439981E+04 0.19950204797162E+04 0.12731438473806E+04
 0.14260605988119E+04 0.11560392536621E+04 0.19951990423276E+04 0.11802498569232E+04 0.14256567461950E+04
 0.10876673439981E+04 0.19950204797162E+04 0.16525376622980E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56488448606683E+03 0.12948429840364E+01
 0.12948429840364E+01 0.61179777216404E+01 0.00000000000000E+00 0.35166848714192E+03 0.35166848714192E+03
 0.35166848714192E+03 0.35166848714192E+03 0.00000000000000E+00 0.00000000000000E+00 0.13250253070333E+00
 0.00000000000000E+00 -.10776280051709E+02 0.10000000000000E-02 0.17546654355523E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45592737156084E+01 0.17097276433532E+01 0.37022339066719E+03 0.45861830054197E+03
 0.31657889299215E+03 0.31657889299215E+03 0.29215658970611E+03 0.29215642844418E+03 0.31657220288861E+03
 0.31657220288861E+03 0.29215658986865E+03 0.29215642860275E+03 0.31657889299215E+03 0.31657889299215E+03
 0.29215658970611E+03 0.29215642844418E+03 0.31657220288861E+03 0.31657220288861E+03 0.29215658986865E+03
 0.29215642860275E+03 0.31328798733598E+03 0.29216762681187E+03 -.19151810396854E+03 -.26993618077985E+03
 0.28682374036749E+03 0.63439603753372E+03 0.34613817846438E+03 0.34077413610880E+03 0.26233582835310E+03
 0.34077413610880E+03 0.49493125163289E+03 0.34080921451162E+03 0.26212587195342E+03 0.34080921451162E+03
 0.49477507795286E+03 0.34077413610880E+03 0.26233582835310E+03 0.34077413610880E+03 0.49493125163289E+03
 0.34080921451162E+03 0.26212587195342E+03 0.34080921451162E+03 0.49477507795286E+03 0.23593100026919E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38050752768904E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16391027921617E+00 0.00000000000000E+00 0.00000000000000E+00 0.16391027921617E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17095781627730E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17095781627730E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255748834420E+00 0.17272110527883E+00 0.35166848714192E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1580.86032559
 0.98583610227655E+00 0.32630072457197E+03 0.49030472411214E+03 0.43641103622399E+03 0.42080257230407E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13099036507971E+00 0.00000000000000E+00 -.18799011790296E+02
 0.89517360790901E-03 0.12780441238747E+01 0.80000000000000E+04 0.30000000000000E+04 0.62595647916645E+01
 0.23473367968742E+01 0.35664795584986E+03 0.29221703712711E+03 0.35469705684805E+03 0.40947836683617E+03
 0.29217079007715E+03 0.29219808416732E+03 0.35000732032853E+03 0.40945215316762E+03 0.29216817294411E+03
 0.29219802839756E+03 0.35469705684805E+03 0.40947836683617E+03 0.29217079007715E+03 0.29219808416732E+03
 0.35000732032853E+03 0.40945215316762E+03 0.29216817294411E+03 0.29219802839756E+03 0.45898146919916E+03
 0.37055623401329E+03 0.19177285275511E+04 0.17596161054963E+04 0.57647648725641E+03 0.84937726904704E+03
 0.27001839935435E+03 0.12747108996303E+04 0.14265562305654E+04 0.11571094239121E+04 0.19943499118363E+04
 0.11819529631652E+04 0.14261540028636E+04 0.10889326332337E+04 0.19941724496378E+04 0.12747108996303E+04
 0.14265562305654E+04 0.11571094239121E+04 0.19943499118363E+04 0.11819529631652E+04 0.14261540028636E+04
 0.10889326332337E+04 0.19941724496378E+04 0.16522864907722E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56514055646274E+03 0.12948429993443E+01
 0.12948429993443E+01 0.61606890470353E+01 0.00000000000000E+00 0.35185596556485E+03 0.35185596556485E+03
 0.35185596556485E+03 0.35185596556485E+03 0.00000000000000E+00 0.00000000000000E+00 0.13242901932409E+00
 0.00000000000000E+00 -.10763254496784E+02 0.10000000000000E-02 0.17550530210147E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45582668467614E+01 0.17093500675355E+01 0.37056004482661E+03 0.45897792095461E+03
 0.31671952859714E+03 0.31671952859714E+03 0.29215689451468E+03 0.29215672579343E+03 0.31671278705544E+03
 0.31671278705544E+03 0.29215689466738E+03 0.29215672594239E+03 0.31671952859714E+03 0.31671952859714E+03
 0.29215689451468E+03 0.29215672579343E+03 0.31671278705544E+03 0.31671278705544E+03 0.29215689466738E+03
 0.29215672594239E+03 0.31341298381703E+03 0.29216834440641E+03 -.19337506316517E+03 -.27259970799440E+03
 0.28773501019801E+03 0.63590976751475E+03 0.34673608226575E+03 0.34233313994832E+03 0.26309949510929E+03
 0.34233313994832E+03 0.49601655397328E+03 0.34236857195157E+03 0.26288861614024E+03 0.34236857195157E+03
 0.49585987471126E+03 0.34233313994832E+03 0.26309949510929E+03 0.34233313994832E+03 0.49601655397328E+03
 0.34236857195157E+03 0.26288861614024E+03 0.34236857195157E+03 0.49585987471126E+03 0.23598057896543E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38070873261800E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16381369015532E+00 0.00000000000000E+00 0.00000000000000E+00 0.16381369015532E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17086144885792E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17086144885792E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255752179081E+00 0.17262913044140E+00 0.35185596556485E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1591.53815694
 0.98575569348015E+00 0.32648044938992E+03 0.49055745330339E+03 0.43664417304444E+03 0.42103064620413E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13094169986385E+00 0.00000000000000E+00 -.18795593521427E+02
 0.89468085234543E-03 0.12780133356784E+01 0.80000000000000E+04 0.30000000000000E+04 0.62597155887686E+01
 0.23473933457882E+01 0.35693765931478E+03 0.29221969431093E+03 0.35497508111027E+03 0.40987103381690E+03
 0.29217172690771E+03 0.29220020036867E+03 0.35027771679625E+03 0.40984486224786E+03 0.29216899857931E+03
 0.29220014258844E+03 0.35497508111027E+03 0.40987103381690E+03 0.29217172690771E+03 0.29220020036867E+03
 0.35027771679625E+03 0.40984486224786E+03 0.29216899857931E+03 0.29220014258844E+03 0.45933853456147E+03
 0.37088972596829E+03 0.19200854679173E+04 0.17612777580586E+04 0.57536458249191E+03 0.84716121715772E+03
 0.26891981175334E+03 0.12762778186877E+04 0.14270522629307E+04 0.11581848547101E+04 0.19935105873733E+04
 0.11836553124959E+04 0.14266516753503E+04 0.10902018044137E+04 0.19933342417664E+04 0.12762778186877E+04
 0.14270522629307E+04 0.11581848547101E+04 0.19935105873733E+04 0.11836553124959E+04 0.14266516753503E+04
 0.10902018044137E+04 0.19933342417664E+04 0.16520566965373E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56539854812350E+03 0.12948429741587E+01
 0.12948429741587E+01 0.62034003724302E+01 0.00000000000000E+00 0.35204130923054E+03 0.35204130923054E+03
 0.35204130923054E+03 0.35204130923054E+03 0.00000000000000E+00 0.00000000000000E+00 0.13235674550783E+00
 0.00000000000000E+00 -.10743797563094E+02 0.10000000000000E-02 0.17554286696702E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45572914116202E+01 0.17089842793576E+01 0.37089351353838E+03 0.45933501873269E+03
 0.31685986456263E+03 0.31685986456263E+03 0.29215720984791E+03 0.29215703340976E+03 0.31685307167348E+03
 0.31685307167348E+03 0.29215720998956E+03 0.29215703354795E+03 0.31685986456263E+03 0.31685986456263E+03
 0.29215720984791E+03 0.29215703340976E+03 0.31685307167348E+03 0.31685307167348E+03 0.29215720998956E+03
 0.29215703354795E+03 0.31353775670487E+03 0.29216908292666E+03 -.19522417646452E+03 -.27525135075483E+03
 0.28863055225678E+03 0.63738373354629E+03 0.34731002852823E+03 0.34387354586926E+03 0.26384779902843E+03
 0.34387354586926E+03 0.49706853883003E+03 0.34390933214923E+03 0.26363600815974E+03 0.34390933214923E+03
 0.49691136269242E+03 0.34387354586926E+03 0.26384779902843E+03 0.34387354586926E+03 0.49706853883003E+03
 0.34390933214923E+03 0.26363600815974E+03 0.34390933214923E+03 0.49691136269242E+03 0.23601347238245E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38090554614459E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16366606397888E+00 0.00000000000000E+00 0.00000000000000E+00 0.16366606397888E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17073070032997E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17073070032997E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255774545849E+00 0.17253850097701E+00 0.35204130923054E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1601.01594499
 0.98568319635512E+00 0.32664019312513E+03 0.49078180992372E+03 0.43685126542341E+03 0.42123329338464E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13089822067977E+00 0.00000000000000E+00 -.18805856916390E+02
 0.89424321712298E-03 0.12779894359372E+01 0.80000000000000E+04 0.30000000000000E+04 0.62598326520071E+01
 0.23474372445027E+01 0.35719474014648E+03 0.29222210957793E+03 0.35522179065369E+03 0.41021764633372E+03
 0.29217258222877E+03 0.29220213052183E+03 0.35051780922123E+03 0.41019151257276E+03 0.29216975262180E+03
 0.29220207092212E+03 0.35522179065369E+03 0.41021764633372E+03 0.29217258222877E+03 0.29220213052183E+03
 0.35051780922123E+03 0.41019151257276E+03 0.29216975262180E+03 0.29220207092212E+03 0.45965146344611E+03
 0.37118039685534E+03 0.19221790062404E+04 0.17627558809868E+04 0.57445137328946E+03 0.84530983562095E+03
 0.26798620546505E+03 0.12776699044567E+04 0.14275133139744E+04 0.11591420719165E+04 0.19928041308606E+04
 0.11851664966947E+04 0.14271142018963E+04 0.10913289884507E+04 0.19926287941921E+04 0.12776699044567E+04
 0.14275133139744E+04 0.11591420719165E+04 0.19928041308606E+04 0.11851664966948E+04 0.14271142018963E+04
 0.10913289884507E+04 0.19926287941921E+04 0.16518972818772E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56562776811612E+03 0.12948430497787E+01
 0.12948430497787E+01 0.62413115246186E+01 0.00000000000000E+00 0.35220502469405E+03 0.35220502469405E+03
 0.35220502469405E+03 0.35220502469405E+03 0.00000000000000E+00 0.00000000000000E+00 0.13229357103815E+00
 0.00000000000000E+00 -.10741797889220E+02 0.10000000000000E-02 0.17557479063379E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45564627878078E+01 0.17086735454279E+01 0.37118417256136E+03 0.45964797036654E+03
 0.31698463432961E+03 0.31698463432961E+03 0.29215740383489E+03 0.29215731445167E+03 0.31697779585617E+03
 0.31697779585617E+03 0.29215740396415E+03 0.29215731457938E+03 0.31698463432961E+03 0.31698463432961E+03
 0.29215740383489E+03 0.29215731445167E+03 0.31697779585617E+03 0.31697779585617E+03 0.29215740396415E+03
 0.29215731457938E+03 0.31364876537115E+03 0.29216975467251E+03 -.19682230252824E+03 -.27753766799572E+03
 0.28941715626829E+03 0.63868160773814E+03 0.34781736568851E+03 0.34521440793073E+03 0.26450421385639E+03
 0.34521440793073E+03 0.49799509764871E+03 0.34525051023284E+03 0.26429164293552E+03 0.34525051023284E+03
 0.49783751124381E+03 0.34521440793073E+03 0.26450421385639E+03 0.34521440793073E+03 0.49799509764871E+03
 0.34525051023284E+03 0.26429164293552E+03 0.34525051023284E+03 0.49783751124381E+03 0.23605416446421E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38108471341209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16365742135266E+00 0.00000000000000E+00 0.00000000000000E+00 0.16365742135266E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17068992251329E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17068992251329E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255750421783E+00 0.17245806220812E+00 0.35220502469405E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1611.00165903
 0.98562161121717E+00 0.32680767007335E+03 0.49101660898772E+03 0.43706731600471E+03 0.42144467342765E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13085257943131E+00 0.00000000000000E+00 -.18812113494190E+02
 0.89378489506309E-03 0.12779662853334E+01 0.80000000000000E+04 0.30000000000000E+04 0.62599460500733E+01
 0.23474797687775E+01 0.35746468621224E+03 0.29222471720904E+03 0.35548087224131E+03 0.41058189071340E+03
 0.29217350988623E+03 0.29220422175535E+03 0.35076992429782E+03 0.41055579653100E+03 0.29217057070615E+03
 0.29220416020013E+03 0.35548087224131E+03 0.41058189071340E+03 0.29217350988623E+03 0.29220422175535E+03
 0.35076992429782E+03 0.41055579653100E+03 0.29217057070615E+03 0.29220416020013E+03 0.45998116996725E+03
 0.37148659226352E+03 0.19243583058442E+04 0.17642868129200E+04 0.57344677213768E+03 0.84330138682876E+03
 0.26698738083039E+03 0.12791225828286E+04 0.14279706974410E+04 0.11601364478554E+04 0.19920286933570E+04
 0.11867451574005E+04 0.14275731235015E+04 0.10925029026816E+04 0.19918544061245E+04 0.12791225828286E+04
 0.14279706974410E+04 0.11601364478554E+04 0.19920286933570E+04 0.11867451574005E+04 0.14275731235015E+04
 0.10925029026816E+04 0.19918544061245E+04 0.16516912157438E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56586557422129E+03 0.12948430958768E+01
 0.12948430958768E+01 0.62812543807825E+01 0.00000000000000E+00 0.35237738761185E+03 0.35237738761185E+03
 0.35237738761185E+03 0.35237738761185E+03 0.00000000000000E+00 0.00000000000000E+00 0.13222793911893E+00
 0.00000000000000E+00 -.10734448089105E+02 0.10000000000000E-02 0.17560679563248E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45556323553349E+01 0.17083621332506E+01 0.37149034875325E+03 0.45997770716015E+03
 0.31711556304163E+03 0.31711556304163E+03 0.29215774450364E+03 0.29215761947596E+03 0.31710867673085E+03
 0.31710867673085E+03 0.29215774462115E+03 0.29215761959157E+03 0.31711556304163E+03 0.31711556304163E+03
 0.29215774450364E+03 0.29215761947596E+03 0.31710867673085E+03 0.31710867673085E+03 0.29215774462115E+03
 0.29215761959157E+03 0.31376525009238E+03 0.29217048043947E+03 -.19851286444526E+03 -.27995486608972E+03
 0.29024907168522E+03 0.64005208114362E+03 0.34835176409998E+03 0.34663287911348E+03 0.26519893241975E+03
 0.34663287911348E+03 0.49897407824468E+03 0.34666931357656E+03 0.26498552393977E+03 0.34666931357656E+03
 0.49881604184869E+03 0.34663287911348E+03 0.26519893241975E+03 0.34663287911348E+03 0.49897407824468E+03
 0.34666931357656E+03 0.26498552393977E+03 0.34666931357656E+03 0.49881604184869E+03 0.23609659071882E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38127096605209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16360755099841E+00 0.00000000000000E+00 0.00000000000000E+00 0.16360755099841E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17061588326177E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17061588326177E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255740217249E+00 0.17237361613392E+00 0.35237738761185E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1620.02995953
 0.98556882914987E+00 0.32695858960282E+03 0.49122764580590E+03 0.43726149201002E+03 0.42163472189704E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13081159826853E+00 0.00000000000000E+00 -.18816573980752E+02
 0.89337229707359E-03 0.12779449817542E+01 0.80000000000000E+04 0.30000000000000E+04 0.62600504045318E+01
 0.23475189016994E+01 0.35770814693732E+03 0.29222713310339E+03 0.35571455052549E+03 0.41090990045430E+03
 0.29217437325981E+03 0.29220616605037E+03 0.35099736156542E+03 0.41088384203680E+03 0.29217133235298E+03
 0.29220610269174E+03 0.35571455052549E+03 0.41090990045430E+03 0.29217437325981E+03 0.29220616605037E+03
 0.35099736156542E+03 0.41088384203680E+03 0.29217133235298E+03 0.29220610269174E+03 0.46027744302830E+03
 0.37176147086206E+03 0.19263116261409E+04 0.17656542312334E+04 0.57254052513883E+03 0.84149429575012E+03
 0.26609106798560E+03 0.12804259898487E+04 0.14283685059786E+04 0.11610255805770E+04 0.19913141839144E+04
 0.11881623737251E+04 0.14279723241658E+04 0.10935539548407E+04 0.19911408479568E+04 0.12804259898487E+04
 0.14283685059786E+04 0.11610255805770E+04 0.19913141839144E+04 0.11881623737251E+04 0.14279723241658E+04
 0.10935539548407E+04 0.19911408479568E+04 0.16514950751324E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56607897401501E+03 0.12948431287413E+01
 0.12948431287413E+01 0.63173675827951E+01 0.00000000000000E+00 0.35253263337772E+03 0.35253263337772E+03
 0.35253263337772E+03 0.35253263337772E+03 0.00000000000000E+00 0.00000000000000E+00 0.13216939943360E+00
 0.00000000000000E+00 -.10726665152559E+02 0.10000000000000E-02 0.17563456420741E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45549120903974E+01 0.17080920338990E+01 0.37176519738956E+03 0.46027401543197E+03
 0.31723371417929E+03 0.31723371417929E+03 0.29215803325437E+03 0.29215790356499E+03 0.31722678468667E+03
 0.31722678468667E+03 0.29215803335975E+03 0.29215790366867E+03 0.31723371417929E+03 0.31723371417929E+03
 0.29215803325437E+03 0.29215790356499E+03 0.31722678468667E+03 0.31722678468667E+03 0.29215803335975E+03
 0.29215790366867E+03 0.31387038835238E+03 0.29217115333166E+03 -.20003086442573E+03 -.28212315376811E+03
 0.29099806758677E+03 0.64128353755154E+03 0.34883047962684E+03 0.34790780151036E+03 0.26582407936574E+03
 0.34790780151036E+03 0.49985332113770E+03 0.34794453662847E+03 0.26560991849759E+03 0.34794453662847E+03
 0.49969488211397E+03 0.34790780151036E+03 0.26582407936574E+03 0.34790780151036E+03 0.49985332113770E+03
 0.34794453662847E+03 0.26560991849759E+03 0.34794453662847E+03 0.49969488211397E+03 0.23613547576649E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38143860239026E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16355205801320E+00 0.00000000000000E+00 0.00000000000000E+00 0.16355205801320E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17054858787129E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17054858787129E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255734415195E+00 0.17229766279755E+00 0.35253263337772E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1630.86392014
 0.98550509485679E+00 0.32713931755327E+03 0.49147966336296E+03 0.43749358066764E+03 0.42186199492082E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13076280003252E+00 0.00000000000000E+00 -.18821967822540E+02
 0.89287870646062E-03 0.12779177488035E+01 0.80000000000000E+04 0.30000000000000E+04 0.62601838087703E+01
 0.23475689282889E+01 0.35799960546171E+03 0.29223010491517E+03 0.35599431520913E+03 0.41130183619395E+03
 0.29217544023774E+03 0.29220856629809E+03 0.35126971619621E+03 0.41127582071385E+03 0.29217227393436E+03
 0.29220850073157E+03 0.35599431520913E+03 0.41130183619394E+03 0.29217544023774E+03 0.29220856629809E+03
 0.35126971619621E+03 0.41127582071385E+03 0.29217227393436E+03 0.29220850073157E+03 0.46063059935179E+03
 0.37208862303204E+03 0.19286415109653E+04 0.17672828011841E+04 0.57146509143615E+03 0.83935070920558E+03
 0.26502829231225E+03 0.12819813952592E+04 0.14288342497449E+04 0.11620850374624E+04 0.19904508148503E+04
 0.11898540569320E+04 0.14284397404195E+04 0.10948069370366E+04 0.19902786233499E+04 0.12819813952592E+04
 0.14288342497449E+04 0.11620850374624E+04 0.19904508148503E+04 0.11898540569320E+04 0.14284397404195E+04
 0.10948069370366E+04 0.19902786233499E+04 0.16512588094282E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56633397026401E+03 0.12948431684828E+01
 0.12948431684828E+01 0.63607034252103E+01 0.00000000000000E+00 0.35271807054811E+03 0.35271807054811E+03
 0.35271807054811E+03 0.35271807054811E+03 0.00000000000000E+00 0.00000000000000E+00 0.13210012666729E+00
 0.00000000000000E+00 -.10717441479640E+02 0.10000000000000E-02 0.17566652958820E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45540832500953E+01 0.17077812187857E+01 0.37209231939414E+03 0.46062720799710E+03
 0.31737525709154E+03 0.31737525709154E+03 0.29215839036126E+03 0.29215825490662E+03 0.31736827587649E+03
 0.31736827587649E+03 0.29215839045078E+03 0.29215825499470E+03 0.31737525709154E+03 0.31737525709154E+03
 0.29215839036126E+03 0.29215825490662E+03 0.31736827587649E+03 0.31736827587649E+03 0.29215839045078E+03
 0.29215825499470E+03 0.31399637461514E+03 0.29217198168589E+03 -.20183784470167E+03 -.28470145263092E+03
 0.29189153448318E+03 0.64274887173703E+03 0.34939787958144E+03 0.34942625596858E+03 0.26656918448865E+03
 0.34942625596858E+03 0.50089861521598E+03 0.34946335230186E+03 0.26635412808858E+03 0.34946335230186E+03
 0.50073969946286E+03 0.34942625596858E+03 0.26656918448865E+03 0.34942625596858E+03 0.50089861521598E+03
 0.34946335230186E+03 0.26635412808858E+03 0.34946335230186E+03 0.50073969946286E+03 0.23618189525988E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38163885198379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16348632644883E+00 0.00000000000000E+00 0.00000000000000E+00 0.16348632644883E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17046859753035E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17046859753035E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255727306801E+00 0.17220702417242E+00 0.35271807054811E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1640.79428282
 0.98544689823641E+00 0.32730456835910E+03 0.49170951687889E+03 0.43770540135438E+03 0.42206951475889E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13071840979022E+00 0.00000000000000E+00 -.18827425549726E+02
 0.89242785830187E-03 0.12778914301119E+01 0.80000000000000E+04 0.30000000000000E+04 0.62603127397916E+01
 0.23476172774218E+01 0.35826604031955E+03 0.29223290012283E+03 0.35625007675767E+03 0.41165956865576E+03
 0.29217644867274E+03 0.29221083231088E+03 0.35151874973990E+03 0.41163359250542E+03 0.29217316416963E+03
 0.29221076467807E+03 0.35625007675767E+03 0.41165956865576E+03 0.29217644867274E+03 0.29221083231088E+03
 0.35151874973990E+03 0.41163359250542E+03 0.29217316416963E+03 0.29221076467807E+03 0.46095234961249E+03
 0.37238627772205E+03 0.19307641194510E+04 0.17687643096604E+04 0.57048539882659E+03 0.83740128586236E+03
 0.26406346004163E+03 0.12833991597115E+04 0.14292495994821E+04 0.11630493669214E+04 0.19896528158441E+04
 0.11913965315573E+04 0.14288566232480E+04 0.10959480454102E+04 0.19894816747360E+04 0.12833991597115E+04
 0.14292495994821E+04 0.11630493669214E+04 0.19896528158441E+04 0.11913965315573E+04 0.14288566232480E+04
 0.10959480454102E+04 0.19894816747360E+04 0.16510391065094E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56656659510803E+03 0.12948432086950E+01
 0.12948432086950E+01 0.64004248759452E+01 0.00000000000000E+00 0.35288723823831E+03 0.35288723823831E+03
 0.35288723823831E+03 0.35288723823831E+03 0.00000000000000E+00 0.00000000000000E+00 0.13203753843415E+00
 0.00000000000000E+00 -.10709632633627E+02 0.10000000000000E-02 0.17569456952406E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45533564421890E+01 0.17075086658209E+01 0.37238994809782E+03 0.46094899016220E+03
 0.31750470334820E+03 0.31750470334820E+03 0.29215874300119E+03 0.29215858722815E+03 0.31749767483119E+03
 0.31749767483119E+03 0.29215874307499E+03 0.29215858730064E+03 0.31750470334820E+03 0.31750470334820E+03
 0.29215874300119E+03 0.29215858722815E+03 0.31749767483119E+03 0.31749767483119E+03 0.29215874307499E+03
 0.29215858730064E+03 0.31411161943336E+03 0.29217276143639E+03 -.20348376949881E+03 -.28704766624750E+03
 0.29270598307929E+03 0.64408184735889E+03 0.34991233436420E+03 0.35080924735229E+03 0.26724787092243E+03
 0.35080924735229E+03 0.50184882511948E+03 0.35084667500535E+03 0.26703199780024E+03 0.35084667500535E+03
 0.50168947560308E+03 0.35080924735229E+03 0.26724787092243E+03 0.35080924735229E+03 0.50184882511948E+03
 0.35084667500535E+03 0.26703199780024E+03 0.35084667500535E+03 0.50168947560308E+03 0.23622372906061E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38182169814606E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16343129944081E+00 0.00000000000000E+00 0.00000000000000E+00 0.16343129944081E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17039821527660E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17039821527660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255719102069E+00 0.17212440270320E+00 0.35288723823831E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1651.32885127
 0.98538889831535E+00 0.32747921881001E+03 0.49195207386961E+03 0.43792883205296E+03 0.42228844495040E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13067171176972E+00 0.00000000000000E+00 -.18834900767119E+02
 0.89195184494843E-03 0.12778617887148E+01 0.80000000000000E+04 0.30000000000000E+04 0.62604579545697E+01
 0.23476717329636E+01 0.35854786591058E+03 0.29223594270613E+03 0.35652063184072E+03 0.41203753897517E+03
 0.29217755166891E+03 0.29221330801520E+03 0.35178222392469E+03 0.41201160446231E+03 0.29217413823003E+03
 0.29221323814465E+03 0.35652063184072E+03 0.41203753897517E+03 0.29217755166891E+03 0.29221330801520E+03
 0.35178222392469E+03 0.41201160446231E+03 0.29217413823003E+03 0.29221323814465E+03 0.46129183595772E+03
 0.37270006199088E+03 0.19329992020694E+04 0.17703199093879E+04 0.56944633020946E+03 0.83534022513656E+03
 0.26304666327605E+03 0.12848935102848E+04 0.14296753096444E+04 0.11640629878984E+04 0.19887955004324E+04
 0.11930230886282E+04 0.14292839568702E+04 0.10971487906716E+04 0.19886254725304E+04 0.12848935102848E+04
 0.14296753096444E+04 0.11640629878985E+04 0.19887955004324E+04 0.11930230886282E+04 0.14292839568702E+04
 0.10971487906716E+04 0.19886254725304E+04 0.16507969563843E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56681160068012E+03 0.12948432637719E+01
 0.12948432637719E+01 0.64425631497330E+01 0.00000000000000E+00 0.35306607330463E+03 0.35306607330463E+03
 0.35306607330463E+03 0.35306607330463E+03 0.00000000000000E+00 0.00000000000000E+00 0.13197205940898E+00
 0.00000000000000E+00 -.10703383625383E+02 0.10000000000000E-02 0.17572294738898E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45526211111696E+01 0.17072329166886E+01 0.37270370300775E+03 0.46128851178125E+03
 0.31764166925236E+03 0.31764166925236E+03 0.29215914560542E+03 0.29215895099698E+03 0.31763459067377E+03
 0.31763459067377E+03 0.29215914566111E+03 0.29215895105149E+03 0.31764166925236E+03 0.31764166925236E+03
 0.29215914560542E+03 0.29215895099698E+03 0.31763459067377E+03 0.31763459067377E+03 0.29215914566111E+03
 0.29215895105149E+03 0.31423357908908E+03 0.29217361088386E+03 -.20522055159382E+03 -.28952089915427E+03
 0.29356749423734E+03 0.64549121917104E+03 0.35045588746251E+03 0.35226996486023E+03 0.26796556718871E+03
 0.35226996486023E+03 0.50285365307865E+03 0.35230774409311E+03 0.26774882928511E+03 0.35230774409311E+03
 0.50269384427655E+03 0.35226996486023E+03 0.26796556718871E+03 0.35226996486023E+03 0.50285365307865E+03
 0.35230774409311E+03 0.26774882928511E+03 0.35230774409311E+03 0.50269384427655E+03 0.23626979567137E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38201561573866E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16338918405539E+00 0.00000000000000E+00 0.00000000000000E+00 0.16338918405539E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17033391063128E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17033391063128E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255704660193E+00 0.17203708486310E+00 0.35306607330463E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1662.56647920
 0.98533235691579E+00 0.32766480715856E+03 0.49220949172746E+03 0.43816575777271E+03 0.42252061669382E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13062249234327E+00 0.00000000000000E+00 -.18842443263004E+02
 0.89144657977034E-03 0.12778260020210E+01 0.80000000000000E+04 0.30000000000000E+04 0.62606332844592E+01
 0.23477374816722E+01 0.35884777715313E+03 0.29223927500082E+03 0.35680857087781E+03 0.41243876534180E+03
 0.29217876569183E+03 0.29221602974537E+03 0.35206271243089E+03 0.41241287533072E+03 0.29217521073297E+03
 0.29221595743695E+03 0.35680857087781E+03 0.41243876534180E+03 0.29217876569183E+03 0.29221602974537E+03
 0.35206271243089E+03 0.41241287533072E+03 0.29217521073297E+03 0.29221595743695E+03 0.46165089052967E+03
 0.37303134042224E+03 0.19353635722293E+04 0.17719589078243E+04 0.56836053255387E+03 0.83318314614389E+03
 0.26198081092724E+03 0.12864763292158E+04 0.14301178380722E+04 0.11651324042632E+04 0.19878771498774E+04
 0.11947465384699E+04 0.14297282158051E+04 0.10984169038897E+04 0.19877083089242E+04 0.12864763292158E+04
 0.14301178380722E+04 0.11651324042632E+04 0.19878771498774E+04 0.11947465384699E+04 0.14297282158051E+04
 0.10984169038897E+04 0.19877083089242E+04 0.16505388110411E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56707090105372E+03 0.12948433193445E+01
 0.12948433193445E+01 0.64875136614813E+01 0.00000000000000E+00 0.35325633572743E+03 0.35325633572743E+03
 0.35325633572743E+03 0.35325633572743E+03 0.00000000000000E+00 0.00000000000000E+00 0.13190322323740E+00
 0.00000000000000E+00 -.10696421645122E+02 0.10000000000000E-02 0.17575162376836E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45518782862250E+01 0.17069543573344E+01 0.37303495378304E+03 0.46164760069197E+03
 0.31778761506414E+03 0.31778761506414E+03 0.29215955503050E+03 0.29215935170976E+03 0.31778048316427E+03
 0.31778048316427E+03 0.29215955506491E+03 0.29215935174344E+03 0.31778761506414E+03 0.31778761506414E+03
 0.29215955503050E+03 0.29215935170976E+03 0.31778048316427E+03 0.31778048316427E+03 0.29215955506491E+03
 0.29215935174344E+03 0.31436357342412E+03 0.29217454200367E+03 -.20704802439902E+03 -.29211862391422E+03
 0.29448363894649E+03 0.64698762677274E+03 0.35103156963152E+03 0.35381455895205E+03 0.26872858038616E+03
 0.35381455895205E+03 0.50392041484264E+03 0.35385271394509E+03 0.26851093206110E+03 0.35385271394509E+03
 0.50376012748133E+03 0.35381455895205E+03 0.26872858038616E+03 0.35381455895205E+03 0.50392041484264E+03
 0.35385271394509E+03 0.26851093206110E+03 0.35385271394509E+03 0.50376012748133E+03 0.23632545065548E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38222192790028E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16334108958084E+00 0.00000000000000E+00 0.00000000000000E+00 0.16334108958084E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17026713160359E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17026713160359E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255690238547E+00 0.17194429450544E+00 0.35325633572743E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1670.99470016
 0.98529073508192E+00 0.32780365430613E+03 0.49240165510173E+03 0.43834269336943E+03 0.42269406626725E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13058603018541E+00 0.00000000000000E+00 -.18847202216276E+02
 0.89106894964127E-03 0.12777957205503E+01 0.80000000000000E+04 0.30000000000000E+04 0.62607816502586E+01
 0.23477931188470E+01 0.35907219112701E+03 0.29224183327078E+03 0.35702404114541E+03 0.41273841445565E+03
 0.29217970182590E+03 0.29221812628808E+03 0.35227265620500E+03 0.41271255784010E+03 0.29217603801167E+03
 0.29221805211695E+03 0.35702404114541E+03 0.41273841445565E+03 0.29217970182590E+03 0.29221812628808E+03
 0.35227265620500E+03 0.41271255784010E+03 0.29217603801167E+03 0.29221805211695E+03 0.46191835979805E+03
 0.37327776477050E+03 0.19371254401236E+04 0.17731776161827E+04 0.56755633302664E+03 0.83158564903483E+03
 0.26119153434306E+03 0.12876565863683E+04 0.14304411867472E+04 0.11659281397898E+04 0.19871844000867E+04
 0.11960320332388E+04 0.14300528602375E+04 0.10993610697961E+04 0.19870164479624E+04 0.12876565863683E+04
 0.14304411867472E+04 0.11659281397898E+04 0.19871844000868E+04 0.11960320332388E+04 0.14300528602375E+04
 0.10993610697961E+04 0.19870164479623E+04 0.16503444834624E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56726441457165E+03 0.12948433544082E+01
 0.12948433544082E+01 0.65212265452925E+01 0.00000000000000E+00 0.35339850821579E+03 0.35339850821579E+03
 0.35339850821579E+03 0.35339850821579E+03 0.00000000000000E+00 0.00000000000000E+00 0.13185227122542E+00
 0.00000000000000E+00 -.10690238714759E+02 0.10000000000000E-02 0.17577216272359E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45513463998166E+01 0.17067548999312E+01 0.37328135883709E+03 0.46191509455494E+03
 0.31789692517144E+03 0.31789692517144E+03 0.29215987097035E+03 0.29215966092661E+03 0.31788975335560E+03
 0.31788975335560E+03 0.29215987098761E+03 0.29215966094350E+03 0.31789692517144E+03 0.31789692517144E+03
 0.29215987097035E+03 0.29215966092661E+03 0.31788975335560E+03 0.31788975335560E+03 0.29215987098761E+03
 0.29215966094350E+03 0.31446095994099E+03 0.29217525738940E+03 -.20840614982583E+03 -.29404674494412E+03
 0.29516756315680E+03 0.64810181827292E+03 0.35145841730034E+03 0.35496453360401E+03 0.26929782660791E+03
 0.35496453360401E+03 0.50471407378277E+03 0.35500297073454E+03 0.26907950136067E+03 0.35500297073454E+03
 0.50455343260078E+03 0.35496453360401E+03 0.26929782660791E+03 0.35496453360401E+03 0.50471407378277E+03
 0.35500297073454E+03 0.26907950136067E+03 0.35500297073454E+03 0.50455343260078E+03 0.23636816279190E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38237575801299E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16329714718421E+00 0.00000000000000E+00 0.00000000000000E+00 0.16329714718421E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17021287916246E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17021287916246E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255682321701E+00 0.17187505282222E+00 0.35339850821579E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1682.23232809
 0.98523294302912E+00 0.32798848282712E+03 0.49265675975081E+03 0.43857788938512E+03 0.42292475877584E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13053796293173E+00 0.00000000000000E+00 -.18852474832090E+02
 0.89056676689035E-03 0.12777512326688E+01 0.80000000000000E+04 0.30000000000000E+04 0.62609996339355E+01
 0.23478748627258E+01 0.35937072221352E+03 0.29224532400139E+03 0.35731069220030E+03 0.41313628801462E+03
 0.29218098474798E+03 0.29222099649834E+03 0.35255201704556E+03 0.41311047594837E+03 0.29217717212300E+03
 0.29222091979767E+03 0.35731069220030E+03 0.41313628801462E+03 0.29218098474798E+03 0.29222099649834E+03
 0.35255201704556E+03 0.41311047594838E+03 0.29217717212300E+03 0.29222091979767E+03 0.46227265853478E+03
 0.37360370276503E+03 0.19394629673778E+04 0.17747932028337E+04 0.56649806028562E+03 0.82948312880160E+03
 0.26015257821455E+03 0.12892227796124E+04 0.14308627699191E+04 0.11669831917560E+04 0.19862574708448E+04
 0.11977381997469E+04 0.14304761703483E+04 0.11006131828126E+04 0.19860907040608E+04 0.12892227796125E+04
 0.14308627699191E+04 0.11669831917561E+04 0.19862574708448E+04 0.11977381997470E+04 0.14304761703482E+04
 0.11006131828126E+04 0.19860907040607E+04 0.16500875270354E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56752174029031E+03 0.12948433932565E+01
 0.12948433932565E+01 0.65661770570408E+01 0.00000000000000E+00 0.35358713017142E+03 0.35358713017142E+03
 0.35358713017142E+03 0.35358713017142E+03 0.00000000000000E+00 0.00000000000000E+00 0.13178522256650E+00
 0.00000000000000E+00 -.10680818038167E+02 0.10000000000000E-02 0.17579840222200E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45506670702828E+01 0.17065001513560E+01 0.37360727040867E+03 0.46226942678381E+03
 0.31804244415276E+03 0.31804244415276E+03 0.29216030426881E+03 0.29216008500472E+03 0.31803521922036E+03
 0.31803521922036E+03 0.29216030426157E+03 0.29216008499763E+03 0.31804244415276E+03 0.31804244415276E+03
 0.29216030426881E+03 0.29216008500472E+03 0.31803521922036E+03 0.31803521922036E+03 0.29216030426157E+03
 0.29216008499763E+03 0.31459063781267E+03 0.29217623427697E+03 -.21020310505613E+03 -.29659526634809E+03
 0.29607330268205E+03 0.64957243493255E+03 0.35201876573709E+03 0.35648589290800E+03 0.27005091315377E+03
 0.35648589290800E+03 0.50576021266316E+03 0.35652470661312E+03 0.26983169316204E+03 0.35652470661312E+03
 0.50559910637152E+03 0.35648589290800E+03 0.27005091315377E+03 0.35648589290800E+03 0.50576021266316E+03
 0.35652470661312E+03 0.26983169316204E+03 0.35652470661312E+03 0.50559910637152E+03 0.23642325150422E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38257945545538E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16322902484971E+00 0.00000000000000E+00 0.00000000000000E+00 0.16322902484971E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17013493664528E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17013493664528E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255675371179E+00 0.17178331242246E+00 0.35358713017142E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1691.62521613
 0.98517074890534E+00 0.32814344283010E+03 0.49286956162983E+03 0.43877511035577E+03 0.42311847494456E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13049807406043E+00 0.00000000000000E+00 -.18849889420828E+02
 0.89014623502431E-03 0.12777115780560E+01 0.80000000000000E+04 0.30000000000000E+04 0.62611939481458E+01
 0.23479477305547E+01 0.35961991965113E+03 0.29224830955945E+03 0.35754997882292E+03 0.41346708422133E+03
 0.29218208677622E+03 0.29222345943265E+03 0.35278532480111E+03 0.41344130963983E+03 0.29217814664038E+03
 0.29222338057893E+03 0.35754997882292E+03 0.41346708422133E+03 0.29218208677622E+03 0.29222345943265E+03
 0.35278532480111E+03 0.41344130963984E+03 0.29217814664038E+03 0.29222338057893E+03 0.46256568674778E+03
 0.37387230439851E+03 0.19414207332175E+04 0.17761529781513E+04 0.56566092453238E+03 0.82780160600805E+03
 0.25931237685301E+03 0.12905322622930E+04 0.14312241458904E+04 0.11678694715780E+04 0.19855036298517E+04
 0.11991637892282E+04 0.14308390022071E+04 0.11016624640221E+04 0.19853378654518E+04 0.12905322622930E+04
 0.14312241458904E+04 0.11678694715780E+04 0.19855036298517E+04 0.11991637892282E+04 0.14308390022071E+04
 0.11016624640221E+04 0.19853378654517E+04 0.16499046913676E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56773855172413E+03 0.12948433742074E+01
 0.12948433742074E+01 0.66037486091815E+01 0.00000000000000E+00 0.35374330143127E+03 0.35374330143127E+03
 0.35374330143127E+03 0.35374330143127E+03 0.00000000000000E+00 0.00000000000000E+00 0.13172995265808E+00
 0.00000000000000E+00 -.10664877381655E+02 0.10000000000000E-02 0.17581968273950E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45501162755783E+01 0.17062936033419E+01 0.37387585277190E+03 0.46256248087263E+03
 0.31816408482184E+03 0.31816408482184E+03 0.29216060957440E+03 0.29216044955818E+03 0.31815681554765E+03
 0.31815681554765E+03 0.29216060954543E+03 0.29216044952965E+03 0.31816408482184E+03 0.31816408482184E+03
 0.29216060957440E+03 0.29216044955818E+03 0.31815681554765E+03 0.31815681554765E+03 0.29216060954543E+03
 0.29216044952965E+03 0.31469909405511E+03 0.29217707044172E+03 -.21168196474426E+03 -.29869006671884E+03
 0.29681753755877E+03 0.65076767673382E+03 0.35246605148725E+03 0.35773527693102E+03 0.27066796990671E+03
 0.35773527693102E+03 0.50660615019513E+03 0.35777440635254E+03 0.27044802276742E+03 0.35777440635254E+03
 0.50644467404174E+03 0.35773527693102E+03 0.27066796990671E+03 0.35773527693102E+03 0.50660615019512E+03
 0.35777440635254E+03 0.27044802276742E+03 0.35777440635254E+03 0.50644467404174E+03 0.23646109432373E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38274558126847E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16310707067477E+00 0.00000000000000E+00 0.00000000000000E+00 0.16310707067477E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17003024404441E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17003024404441E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255693181862E+00 0.17170767745773E+00 0.35374330143127E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1701.38985732
 0.98510622550794E+00 0.32830434606663E+03 0.49309000359933E+03 0.43897954456270E+03 0.42331936165484E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13045648639386E+00 0.00000000000000E+00 -.18856651251289E+02
 0.88970991150842E-03 0.12776738398133E+01 0.80000000000000E+04 0.30000000000000E+04 0.62613788830245E+01
 0.23480170811342E+01 0.35987831734520E+03 0.29225148091363E+03 0.35779810902595E+03 0.41381005669856E+03
 0.29218326215495E+03 0.29222608371359E+03 0.35302726008070E+03 0.41378432100855E+03 0.29217918633980E+03
 0.29222600258327E+03 0.35779810902595E+03 0.41381005669856E+03 0.29218326215495E+03 0.29222608371359E+03
 0.35302726008070E+03 0.41378432100855E+03 0.29217918633980E+03 0.29222600258327E+03 0.46286978962856E+03
 0.37415068002337E+03 0.19434493878192E+04 0.17775629431329E+04 0.56477888423629E+03 0.82604006844232E+03
 0.25843728978485E+03 0.12918895521480E+04 0.14315903675828E+04 0.11687889680075E+04 0.19847130284693E+04
 0.12006418329438E+04 0.14312067372377E+04 0.11027512282570E+04 0.19845483073519E+04 0.12918895521481E+04
 0.14315903675828E+04 0.11687889680075E+04 0.19847130284693E+04 0.12006418329438E+04 0.14312067372377E+04
 0.11027512282570E+04 0.19845483073518E+04 0.16497072788310E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56796320253896E+03 0.12948434240281E+01
 0.12948434240281E+01 0.66428071739418E+01 0.00000000000000E+00 0.35390468454727E+03 0.35390468454727E+03
 0.35390468454727E+03 0.35390468454727E+03 0.00000000000000E+00 0.00000000000000E+00 0.13167320479291E+00
 0.00000000000000E+00 -.10659054176398E+02 0.10000000000000E-02 0.17584097814554E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45495652289757E+01 0.17060869608659E+01 0.37415420959189E+03 0.46286661164865E+03
 0.31829008758510E+03 0.31829008758510E+03 0.29216100462510E+03 0.29216083865048E+03 0.31828277236099E+03
 0.31828277236099E+03 0.29216100457207E+03 0.29216083859825E+03 0.31829008758510E+03 0.31829008758510E+03
 0.29216100462510E+03 0.29216083865048E+03 0.31828277236099E+03 0.31828277236099E+03 0.29216100457207E+03
 0.29216083859825E+03 0.31481145031597E+03 0.29217795929972E+03 -.21322357470987E+03 -.30087362269550E+03
 0.29758635382841E+03 0.65200531443087E+03 0.35293102883332E+03 0.35903119186872E+03 0.27130474344514E+03
 0.35903119186872E+03 0.50748241145510E+03 0.35907064915430E+03 0.27108403597348E+03 0.35907064915430E+03
 0.50732054620815E+03 0.35903119186872E+03 0.27130474344514E+03 0.35903119186872E+03 0.50748241145510E+03
 0.35907064915430E+03 0.27108403597348E+03 0.35907064915430E+03 0.50732054620815E+03 0.23649690562339E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38292063079225E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16306789739041E+00 0.00000000000000E+00 0.00000000000000E+00 0.16306789739041E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16996954386391E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16996954386391E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255680821694E+00 0.17162926443599E+00 0.35390468454727E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1711.15449851
 0.98504345252250E+00 0.32846491775016E+03 0.49330958230750E+03 0.43918319647234E+03 0.42351953601545E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13041493633638E+00 0.00000000000000E+00 -.18858770574166E+02
 0.88927495385765E-03 0.12776379469813E+01 0.80000000000000E+04 0.30000000000000E+04 0.62615547846724E+01
 0.23480830442522E+01 0.36013616280455E+03 0.29225472331575E+03 0.35804572178214E+03 0.41415165273213E+03
 0.29218446890478E+03 0.29222877528818E+03 0.35326874395314E+03 0.41412595599492E+03 0.29218025412710E+03
 0.29222869184133E+03 0.35804572178214E+03 0.41415165273213E+03 0.29218446890478E+03 0.29222877528818E+03
 0.35326874395314E+03 0.41412595599492E+03 0.29218025412710E+03 0.29222869184133E+03 0.46317198768273E+03
 0.37442679095941E+03 0.19454673913251E+04 0.17789634342179E+04 0.56391034865236E+03 0.82430336754111E+03
 0.25757346714549E+03 0.12932406569123E+04 0.14319507247957E+04 0.11697030608517E+04 0.19839224491965E+04
 0.12021134123837E+04 0.14315686117926E+04 0.11038339111618E+04 0.19837587759090E+04 0.12932406569124E+04
 0.14319507247958E+04 0.11697030608517E+04 0.19839224491965E+04 0.12021134123837E+04 0.14315686117926E+04
 0.11038339111618E+04 0.19837587759089E+04 0.16495121049239E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56818678426268E+03 0.12948434396431E+01
 0.12948434396431E+01 0.66818657387022E+01 0.00000000000000E+00 0.35406534600674E+03 0.35406534600674E+03
 0.35406534600674E+03 0.35406534600674E+03 0.00000000000000E+00 0.00000000000000E+00 0.13161714301047E+00
 0.00000000000000E+00 -.10647976350453E+02 0.10000000000000E-02 0.17586139393770E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45490370688374E+01 0.17058889008140E+01 0.37443029257318E+03 0.46316884416674E+03
 0.31841586648928E+03 0.31841586648928E+03 0.29216141051810E+03 0.29216123842153E+03 0.31840850541559E+03
 0.31840850541559E+03 0.29216141043945E+03 0.29216123834407E+03 0.31841586648928E+03 0.31841586648928E+03
 0.29216141051810E+03 0.29216123842153E+03 0.31840850541559E+03 0.31840850541559E+03 0.29216141043945E+03
 0.29216123834407E+03 0.31492363781518E+03 0.29217886877396E+03 -.21475198357855E+03 -.30303602445040E+03
 0.29835066044105E+03 0.65322952405586E+03 0.35338711031261E+03 0.36031713564961E+03 0.27193728628084E+03
 0.36031713564961E+03 0.50834772171061E+03 0.36035692104072E+03 0.27171582562274E+03 0.36035692104072E+03
 0.50818547297460E+03 0.36031713564960E+03 0.27193728628084E+03 0.36031713564960E+03 0.50834772171060E+03
 0.36035692104072E+03 0.27171582562274E+03 0.36035692104072E+03 0.50818547297460E+03 0.23653128657017E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38309313988741E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16298624376543E+00 0.00000000000000E+00 0.00000000000000E+00 0.16298624376543E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16988358275872E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16988358275872E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255683811572E+00 0.17155143405746E+00 0.35406534600674E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1720.71867754
 0.98498297709234E+00 0.32862186308044E+03 0.49352378147709E+03 0.43938192064023E+03 0.42371493014982E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13037424035214E+00 0.00000000000000E+00 -.18865290946601E+02
 0.88885019103088E-03 0.12776050044979E+01 0.80000000000000E+04 0.30000000000000E+04 0.62617162361099E+01
 0.23481435885412E+01 0.36038810600344E+03 0.29225796985325E+03 0.35828767878304E+03 0.41448503110385E+03
 0.29218568223539E+03 0.29223147877375E+03 0.35350474488480E+03 0.41445937251989E+03 0.29218132807711E+03
 0.29223139301853E+03 0.35828767878304E+03 0.41448503110385E+03 0.29218568223539E+03 0.29223147877375E+03
 0.35350474488480E+03 0.41445937251989E+03 0.29218132807711E+03 0.29223139301853E+03 0.46346654910641E+03
 0.37469553174481E+03 0.19474342802082E+04 0.17803270793956E+04 0.56306389324679E+03 0.82261306644332E+03
 0.25673385373029E+03 0.12945582599720E+04 0.14322957079366E+04 0.11705937132730E+04 0.19831444067652E+04
 0.12035488721608E+04 0.14319150840560E+04 0.11048892335030E+04 0.19829817636343E+04 0.12945582599720E+04
 0.14322957079366E+04 0.11705937132730E+04 0.19831444067653E+04 0.12035488721608E+04 0.14319150840560E+04
 0.11048892335030E+04 0.19829817636342E+04 0.16493189170160E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56840480924048E+03 0.12948434876849E+01
 0.12948434876849E+01 0.67201224548298E+01 0.00000000000000E+00 0.35422197432683E+03 0.35422197432683E+03
 0.35422197432683E+03 0.35422197432683E+03 0.00000000000000E+00 0.00000000000000E+00 0.13156286940291E+00
 0.00000000000000E+00 -.10642254274221E+02 0.10000000000000E-02 0.17588058871528E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45485406083957E+01 0.17057027281484E+01 0.37469900635828E+03 0.46346343831058E+03
 0.31853875057389E+03 0.31853875057389E+03 0.29216184383817E+03 0.29216164067064E+03 0.31853134470736E+03
 0.31853134470736E+03 0.29216184373264E+03 0.29216164056692E+03 0.31853875057389E+03 0.31853875057389E+03
 0.29216184383817E+03 0.29216164067064E+03 0.31853134470736E+03 0.31853134470736E+03 0.29216184373264E+03
 0.29216164056692E+03 0.31503326551074E+03 0.29217978011996E+03 -.21624184867266E+03 -.30514221333452E+03
 0.29909529139219E+03 0.65442260642662E+03 0.35383183857747E+03 0.36156980586515E+03 0.27255310577118E+03
 0.36156980586515E+03 0.50919106041823E+03 0.36160991262476E+03 0.27233091036430E+03 0.36160991262476E+03
 0.50902843857284E+03 0.36156980586515E+03 0.27255310577118E+03 0.36156980586515E+03 0.50919106041822E+03
 0.36160991262476E+03 0.27233091036430E+03 0.36160991262476E+03 0.50902843857284E+03 0.23656509784613E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38326300017368E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16294777099588E+00 0.00000000000000E+00 0.00000000000000E+00 0.16294777099588E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16982371318906E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16982371318906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255672070353E+00 0.17147547120785E+00 0.35422197432683E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1730.63960108
 0.98492427813560E+00 0.32878418978057E+03 0.49374500677359E+03 0.43958703557442E+03 0.42391663428997E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13033216987392E+00 0.00000000000000E+00 -.18870184863543E+02
 0.88841130670029E-03 0.12775715421996E+01 0.80000000000000E+04 0.30000000000000E+04 0.62618802436899E+01
 0.23482050913837E+01 0.36064889599655E+03 0.29226141132067E+03 0.35853814774370E+03 0.41482944802596E+03
 0.29218697371187E+03 0.29223435345561E+03 0.35374910469695E+03 0.41480382910929E+03 0.29218247155444E+03
 0.29223426526513E+03 0.35853814774370E+03 0.41482944802596E+03 0.29218697371187E+03 0.29223435345561E+03
 0.35374910469695E+03 0.41480382910929E+03 0.29218247155444E+03 0.29223426526513E+03 0.46377008364313E+03
 0.37497196291572E+03 0.19494605319464E+04 0.17817277296360E+04 0.56219905372689E+03 0.82088422652223E+03
 0.25587417752671E+03 0.12959171278963E+04 0.14326453282580E+04 0.11715096882201E+04 0.19823344732741E+04
 0.12050297368696E+04 0.14322662508077E+04 0.11059753917196E+04 0.19821729007848E+04 0.12959171278963E+04
 0.14326453282580E+04 0.11715096882201E+04 0.19823344732741E+04 0.12050297368696E+04 0.14322662508077E+04
 0.11059753917196E+04 0.19821729007848E+04 0.16491175981817E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56862945288250E+03 0.12948435237429E+01
 0.12948435237429E+01 0.67598061489975E+01 0.00000000000000E+00 0.35438395466199E+03 0.35438395466199E+03
 0.35438395466199E+03 0.35438395466199E+03 0.00000000000000E+00 0.00000000000000E+00 0.13150720714106E+00
 0.00000000000000E+00 -.10634300340623E+02 0.10000000000000E-02 0.17589959167970E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45480492158091E+01 0.17055184559284E+01 0.37497541130361E+03 0.46376700469921E+03
 0.31866602570745E+03 0.31866602570745E+03 0.29216227979037E+03 0.29216206914438E+03 0.31865857347235E+03
 0.31865857347235E+03 0.29216227965545E+03 0.29216206901176E+03 0.31866602570745E+03 0.31866602570745E+03
 0.29216227979037E+03 0.29216206914438E+03 0.31865857347235E+03 0.31865857347235E+03 0.29216227965545E+03
 0.29216206901176E+03 0.31514683986479E+03 0.29218074694222E+03 -.21777126177031E+03 -.30730121631520E+03
 0.29986498326170E+03 0.65565275886994E+03 0.35428845069193E+03 0.36285975868345E+03 0.27318944314937E+03
 0.36285975868345E+03 0.51006015230695E+03 0.36290019907888E+03 0.27296649358676E+03 0.36290019907888E+03
 0.50989715039153E+03 0.36285975868345E+03 0.27318944314937E+03 0.36285975868345E+03 0.51006015230695E+03
 0.36290019907888E+03 0.27296649358676E+03 0.36290019907888E+03 0.50989715039153E+03 0.23660285567537E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38343803557390E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16289114811816E+00 0.00000000000000E+00 0.00000000000000E+00 0.16289114811816E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16975350862120E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16975350862120E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255665842021E+00 0.17139704551290E+00 0.35438395466199E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1740.56052463
 0.98486663943892E+00 0.32894617089884E+03 0.49396525604179E+03 0.43979132543245E+03 0.42411760409430E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13029028880828E+00 0.00000000000000E+00 -.18875014789936E+02
 0.88797378896704E-03 0.12775382084752E+01 0.80000000000000E+04 0.30000000000000E+04 0.62620436296368E+01
 0.23482663611138E+01 0.36090910852117E+03 0.29226492853255E+03 0.35878807773269E+03 0.41517248622533E+03
 0.29218829907321E+03 0.29223730052667E+03 0.35399299020043E+03 0.41514690704825E+03 0.29218364540476E+03
 0.29223720985944E+03 0.35878807773269E+03 0.41517248622533E+03 0.29218829907321E+03 0.29223730052667E+03
 0.35399299020043E+03 0.41514690704825E+03 0.29218364540476E+03 0.29223720985944E+03 0.46407172768590E+03
 0.37524620867231E+03 0.19514747015550E+04 0.17831175491548E+04 0.56134477126493E+03 0.81917601041788E+03
 0.25502451529663E+03 0.12972687883244E+04 0.14329862565220E+04 0.11724192693463E+04 0.19815208136821E+04
 0.12065031458455E+04 0.14326087270577E+04 0.11070545145293E+04 0.19813603137822E+04 0.12972687883244E+04
 0.14329862565220E+04 0.11724192693463E+04 0.19815208136821E+04 0.12065031458455E+04 0.14326087270577E+04
 0.11070545145293E+04 0.19813603137822E+04 0.16489155991104E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56885302536522E+03 0.12948435593295E+01
 0.12948435593295E+01 0.67994898431653E+01 0.00000000000000E+00 0.35454526769420E+03 0.35454526769420E+03
 0.35454526769420E+03 0.35454526769420E+03 0.00000000000000E+00 0.00000000000000E+00 0.13145217217335E+00
 0.00000000000000E+00 -.10626345623140E+02 0.10000000000000E-02 0.17591778736000E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45475787980602E+01 0.17053420492726E+01 0.37524963241881E+03 0.46406867897720E+03
 0.31879307530053E+03 0.31879307530053E+03 0.29216272751836E+03 0.29216250919187E+03 0.31878557680537E+03
 0.31878557680537E+03 0.29216272735228E+03 0.29216250902864E+03 0.31879307530053E+03 0.31879307530053E+03
 0.29216272751836E+03 0.29216250919187E+03 0.31878557680537E+03 0.31878557680537E+03 0.29216272735228E+03
 0.29216250902864E+03 0.31526024138427E+03 0.29218173583354E+03 -.21928774339264E+03 -.30943943374067E+03
 0.30063066302757E+03 0.65687372635163E+03 0.35473991000892E+03 0.36414033250179E+03 0.27382202815612E+03
 0.36414033250179E+03 0.51092212235582E+03 0.36418110671825E+03 0.27359833127855E+03 0.36418110671825E+03
 0.51075874621892E+03 0.36414033250179E+03 0.27382202815611E+03 0.36414033250179E+03 0.51092212235582E+03
 0.36418110671825E+03 0.27359833127855E+03 0.36418110671825E+03 0.51075874621892E+03 0.23664126179695E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38361235033476E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16283437666458E+00 0.00000000000000E+00 0.00000000000000E+00 0.16283437666458E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16968381170237E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16968381170237E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255659762890E+00 0.17131901540613E+00 0.35454526769420E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1751.70032110
 0.98480373614942E+00 0.32912756643748E+03 0.49421135333641E+03 0.44001964329782E+03 0.42434229116975E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13024351826381E+00 0.00000000000000E+00 -.18881214711527E+02
 0.88748433631591E-03 0.12775006592126E+01 0.80000000000000E+04 0.30000000000000E+04 0.62622276883447E+01
 0.23483353831293E+01 0.36120053833217E+03 0.29226897022268E+03 0.35906801096303E+03 0.41555610352551E+03
 0.29218982877105E+03 0.29224069821052E+03 0.35426620130736E+03 0.41553056899806E+03 0.29218500069038E+03
 0.29224060471213E+03 0.35906801096303E+03 0.41555610352551E+03 0.29218982877105E+03 0.29224069821052E+03
 0.35426620130736E+03 0.41553056899806E+03 0.29218500069038E+03 0.29224060471213E+03 0.46440844174538E+03
 0.37555192139702E+03 0.19537212850690E+04 0.17846644713555E+04 0.56039172219994E+03 0.81727295135413E+03
 0.25407927054318E+03 0.12987775698004E+04 0.14333565776198E+04 0.11734325428265E+04 0.19805995881703E+04
 0.12081484386993E+04 0.14329807864240E+04 0.11082575638229E+04 0.19804402936261E+04 0.12987775698004E+04
 0.14333565776198E+04 0.11734325428266E+04 0.19805995881704E+04 0.12081484386994E+04 0.14329807864240E+04
 0.11082575638229E+04 0.19804402936261E+04 0.16486840641545E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56910265558138E+03 0.12948436050102E+01
 0.12948436050102E+01 0.68440490290695E+01 0.00000000000000E+00 0.35472563042757E+03 0.35472563042757E+03
 0.35472563042757E+03 0.35472563042757E+03 0.00000000000000E+00 0.00000000000000E+00 0.13139109828094E+00
 0.00000000000000E+00 -.10618381800966E+02 0.10000000000000E-02 0.17593728789506E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45470747535745E+01 0.17051530325904E+01 0.37555531633816E+03 0.46440542768348E+03
 0.31893540551227E+03 0.31893540551227E+03 0.29216326651439E+03 0.29216301749656E+03 0.31892785521591E+03
 0.31892785521591E+03 0.29216326631079E+03 0.29216301729679E+03 0.31893540551227E+03 0.31893540551227E+03
 0.29216326651439E+03 0.29216301749656E+03 0.31892785521591E+03 0.31892785521591E+03 0.29216326631079E+03
 0.29216301729679E+03 0.31538730984072E+03 0.29218287316774E+03 -.22097883949129E+03 -.31182124887986E+03
 0.30148630160198E+03 0.65823586975849E+03 0.35524213664850E+03 0.36556929172274E+03 0.27452850169458E+03
 0.36556929172274E+03 0.51188331151948E+03 0.36561044082082E+03 0.27430397088752E+03 0.36561044082082E+03
 0.51171951923006E+03 0.36556929172273E+03 0.27452850169458E+03 0.36556929172273E+03 0.51188331151947E+03
 0.36561044082082E+03 0.27430397088752E+03 0.36561044082082E+03 0.51171951923006E+03 0.23668484739198E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38380753364002E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16277839988585E+00 0.00000000000000E+00 0.00000000000000E+00 0.16277839988585E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16961039966185E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16961039966185E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255650307387E+00 0.17123182668711E+00 0.35472563042757E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1760.36928000
 0.98475812653680E+00 0.32926821471342E+03 0.49440190282448E+03 0.44019632238036E+03 0.42451618324286E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13020737017386E+00 0.00000000000000E+00 -.18887072986933E+02
 0.88710519242230E-03 0.12774707562301E+01 0.80000000000000E+04 0.30000000000000E+04 0.62623742743112E+01
 0.23483903528667E+01 0.36142672048589E+03 0.29227218521046E+03 0.35928528638201E+03 0.41585352763242E+03
 0.29219105066400E+03 0.29224340935170E+03 0.35447828508997E+03 0.41582802782277E+03 0.29218608361525E+03
 0.29224331361261E+03 0.35928528638201E+03 0.41585352763241E+03 0.29219105066400E+03 0.29224340935170E+03
 0.35447828508997E+03 0.41582802782277E+03 0.29218608361525E+03 0.29224331361261E+03 0.46466915243049E+03
 0.37578844506651E+03 0.19554563692349E+04 0.17858553556748E+04 0.55964986716375E+03 0.81579605941405E+03
 0.25334794291448E+03 0.12999440984153E+04 0.14336331430454E+04 0.11742135360813E+04 0.19798737980853E+04
 0.12094211477683E+04 0.14332587024808E+04 0.11091859679905E+04 0.19797154404898E+04 0.12999440984153E+04
 0.14336331430455E+04 0.11742135360813E+04 0.19798737980854E+04 0.12094211477683E+04 0.14332587024807E+04
 0.11091859679905E+04 0.19797154404898E+04 0.16484960760266E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56929550125928E+03 0.12948436481736E+01
 0.12948436481736E+01 0.68787248646512E+01 0.00000000000000E+00 0.35486555957802E+03 0.35486555957802E+03
 0.35486555957802E+03 0.35486555957802E+03 0.00000000000000E+00 0.00000000000000E+00 0.13134408341909E+00
 0.00000000000000E+00 -.10613447132887E+02 0.10000000000000E-02 0.17595174446794E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45467011561557E+01 0.17050129335584E+01 0.37579181597476E+03 0.46466616633691E+03
 0.31904589012696E+03 0.31904589012696E+03 0.29216372078582E+03 0.29216342383854E+03 0.31903829962855E+03
 0.31903829962855E+03 0.29216372055087E+03 0.29216342360868E+03 0.31904589012696E+03 0.31904589012696E+03
 0.29216372078582E+03 0.29216342383854E+03 0.31903829962855E+03 0.31903829962855E+03 0.29216372055087E+03
 0.29216342360868E+03 0.31548596256483E+03 0.29218377862159E+03 -.22228774696242E+03 -.31366285782626E+03
 0.30215059119528E+03 0.65929302273021E+03 0.35563167857896E+03 0.36667677208928E+03 0.27507688903989E+03
 0.36667677208928E+03 0.51262947893713E+03 0.36671821284888E+03 0.27485171095751E+03 0.36671821284888E+03
 0.51246536372479E+03 0.36667677208928E+03 0.27507688903989E+03 0.36667677208928E+03 0.51262947893712E+03
 0.36671821284888E+03 0.27485171095751E+03 0.36671821284888E+03 0.51246536372479E+03 0.23672038943260E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38395933264133E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16274496636424E+00 0.00000000000000E+00 0.00000000000000E+00 0.16274496636424E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16955971305388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16955971305388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255639397968E+00 0.17116420737234E+00 0.35486555957802E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1771.92789186
 0.98470131414504E+00 0.32945515694820E+03 0.49465481406910E+03 0.44043070758135E+03 0.42474690434116E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13015964191593E+00 0.00000000000000E+00 -.18894099043291E+02
 0.88660176219252E-03 0.12774279139520E+01 0.80000000000000E+04 0.30000000000000E+04 0.62625843013327E+01
 0.23484691129998E+01 0.36172768106442E+03 0.29227656479514E+03 0.35957441695216E+03 0.41624833733307E+03
 0.29219272199617E+03 0.29224711384240E+03 0.35476058667290E+03 0.41622288397435E+03 0.29218756533435E+03
 0.29224701506621E+03 0.35957441695216E+03 0.41624833733307E+03 0.29219272199617E+03 0.29224711384240E+03
 0.35476058667290E+03 0.41622288397435E+03 0.29218756533435E+03 0.29224701506621E+03 0.46501405772236E+03
 0.37610077738289E+03 0.19577524475157E+04 0.17874255408376E+04 0.55868229125059E+03 0.81386489193445E+03
 0.25238918922761E+03 0.13014895305740E+04 0.14339925681876E+04 0.11752445269209E+04 0.19789037778756E+04
 0.12111077539565E+04 0.14336199269659E+04 0.11104126245805E+04 0.19787466680001E+04 0.13014895305740E+04
 0.14339925681876E+04 0.11752445269209E+04 0.19789037778756E+04 0.12111077539565E+04 0.14336199269659E+04
 0.11104126245805E+04 0.19787466680001E+04 0.16482465718620E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56955093409361E+03 0.12948436999412E+01
 0.12948436999412E+01 0.69249593120934E+01 0.00000000000000E+00 0.35505164048364E+03 0.35505164048364E+03
 0.35505164048364E+03 0.35505164048364E+03 0.00000000000000E+00 0.00000000000000E+00 0.13128207925342E+00
 0.00000000000000E+00 -.10606144265009E+02 0.10000000000000E-02 0.17597002944667E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45462287101706E+01 0.17048357663140E+01 0.37610411928895E+03 0.46501110615204E+03
 0.31919305497754E+03 0.31919305497754E+03 0.29216428932748E+03 0.29216398007534E+03 0.31918541098076E+03
 0.31918541098076E+03 0.29216428904920E+03 0.29216397980309E+03 0.31919305497754E+03 0.31919305497754E+03
 0.29216428932748E+03 0.29216398007534E+03 0.31918541098076E+03 0.31918541098076E+03 0.29216428904920E+03
 0.29216397980309E+03 0.31561740529292E+03 0.29218501309260E+03 -.22400975163701E+03 -.31608135508233E+03
 0.30303328925897E+03 0.66069517996450E+03 0.35614672425924E+03 0.36814060023714E+03 0.27580535922126E+03
 0.36814060023714E+03 0.51361889308634E+03 0.36818243034333E+03 0.27557933059374E+03 0.36818243034333E+03
 0.51345435883720E+03 0.36814060023714E+03 0.27580535922126E+03 0.36814060023714E+03 0.51361889308634E+03
 0.36818243034333E+03 0.27557933059374E+03 0.36818243034333E+03 0.51345435883720E+03 0.23677324178773E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38416107174724E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16269379172432E+00 0.00000000000000E+00 0.00000000000000E+00 0.16269379172432E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16949169903924E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16949169903924E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255627062568E+00 0.17107439105205E+00 0.35505164048364E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1780.59685075
 0.98465914625482E+00 0.32959504367720E+03 0.49484362848294E+03 0.44060578500472E+03 0.42491931463179E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13012423020317E+00 0.00000000000000E+00 -.18898619772205E+02
 0.88622543112380E-03 0.12773931484157E+01 0.80000000000000E+04 0.30000000000000E+04 0.62627547438485E+01
 0.23485330289432E+01 0.36195289054579E+03 0.29227991985262E+03 0.35979078985215E+03 0.41654321317195E+03
 0.29219400753345E+03 0.29224996026613E+03 0.35497189589773E+03 0.41651779471028E+03 0.29218870538622E+03
 0.29224985917498E+03 0.35979078985215E+03 0.41654321317195E+03 0.29219400753345E+03 0.29224996026613E+03
 0.35497189589773E+03 0.41651779471028E+03 0.29218870538622E+03 0.29224985917498E+03 0.46527102688954E+03
 0.37633313484958E+03 0.19594635553791E+04 0.17885932398163E+04 0.55796641097562E+03 0.81243549100618E+03
 0.25167924797568E+03 0.13026419635098E+04 0.14342541031449E+04 0.11760117585658E+04 0.19781726145111E+04
 0.12123658308172E+04 0.14338828093643E+04 0.11113260209666E+04 0.19780164387393E+04 0.13026419635098E+04
 0.14342541031449E+04 0.11760117585658E+04 0.19781726145111E+04 0.12123658308172E+04 0.14338828093643E+04
 0.11113260209666E+04 0.19780164387393E+04 0.16480588073434E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56974162749004E+03 0.12948437332497E+01
 0.12948437332497E+01 0.69596351476750E+01 0.00000000000000E+00 0.35519065774540E+03 0.35519065774540E+03
 0.35519065774540E+03 0.35519065774540E+03 0.00000000000000E+00 0.00000000000000E+00 0.13123608253964E+00
 0.00000000000000E+00 -.10599871663379E+02 0.10000000000000E-02 0.17598309802533E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45458911053199E+01 0.17047091644950E+01 0.37633645597974E+03 0.46526810041837E+03
 0.31930325797036E+03 0.31930325797036E+03 0.29216472697328E+03 0.29216440824923E+03 0.31929557394852E+03
 0.31929557394852E+03 0.29216472666073E+03 0.29216440794345E+03 0.31930325797036E+03 0.31930325797036E+03
 0.29216472697328E+03 0.29216440824923E+03 0.31929557394852E+03 0.31929557394852E+03 0.29216472666073E+03
 0.29216440794345E+03 0.31571585913056E+03 0.29218595956921E+03 -.22528943545108E+03 -.31787628468007E+03
 0.30369196960893E+03 0.66173861827545E+03 0.35652818881847E+03 0.36923019213524E+03 0.27634857429406E+03
 0.36923019213524E+03 0.51435452486601E+03 0.36927231440093E+03 0.27612191405779E+03 0.36927231440093E+03
 0.51418968164447E+03 0.36923019213524E+03 0.27634857429406E+03 0.36923019213524E+03 0.51435452486600E+03
 0.36927231440093E+03 0.27612191405779E+03 0.36927231440093E+03 0.51418968164447E+03 0.23681353633611E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38431151122422E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16264885720879E+00 0.00000000000000E+00 0.00000000000000E+00 0.16264885720879E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16943728917360E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16943728917360E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255620231518E+00 0.17100737752999E+00 0.35519065774540E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1792.15546261
 0.98459122872077E+00 0.32978172482842E+03 0.49509446197465E+03 0.44083930497798E+03 0.42514954513500E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13007734176045E+00 0.00000000000000E+00 -.18898919431553E+02
 0.88572375854683E-03 0.12773450971782E+01 0.80000000000000E+04 0.30000000000000E+04 0.62629903364980E+01
 0.23486213761867E+01 0.36225251390245E+03 0.29228448805806E+03 0.36007867193644E+03 0.41693478409832E+03
 0.29219576493269E+03 0.29225384744690E+03 0.35525310017998E+03 0.41690941225625E+03 0.29219026438826E+03
 0.29225374321955E+03 0.36007867193644E+03 0.41693478409832E+03 0.29219576493269E+03 0.29225384744690E+03
 0.35525310017998E+03 0.41690941225625E+03 0.29219026438826E+03 0.29225374321955E+03 0.46561149251017E+03
 0.37664044214132E+03 0.19617425027984E+04 0.17901531461991E+04 0.55702954168184E+03 0.81056108998265E+03
 0.25074640059239E+03 0.13041750911358E+04 0.14345981534021E+04 0.11770352747359E+04 0.19771998120698E+04
 0.12140393494159E+04 0.14342286622900E+04 0.11125433492573E+04 0.19770448881332E+04 0.13041750911358E+04
 0.14345981534021E+04 0.11770352747359E+04 0.19771998120698E+04 0.12140393494159E+04 0.14342286622900E+04
 0.11125433492573E+04 0.19770448881331E+04 0.16478194733902E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.56999681621657E+03 0.12948437354575E+01
 0.12948437354575E+01 0.70058695951173E+01 0.00000000000000E+00 0.35537443633466E+03 0.35537443633466E+03
 0.35537443633466E+03 0.35537443633466E+03 0.00000000000000E+00 0.00000000000000E+00 0.13117542739079E+00
 0.00000000000000E+00 -.10584904568640E+02 0.10000000000000E-02 0.17600009864537E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45454519977965E+01 0.17045444991737E+01 0.37664373516274E+03 0.46560860011109E+03
 0.31944990939065E+03 0.31944990939065E+03 0.29216532572814E+03 0.29216499404527E+03 0.31944217213936E+03
 0.31944217213936E+03 0.29216532536744E+03 0.29216499369238E+03 0.31944990939065E+03 0.31944990939065E+03
 0.29216532572814E+03 0.29216499404527E+03 0.31944217213936E+03 0.31944217213936E+03 0.29216532536744E+03
 0.29216499369238E+03 0.31584691196104E+03 0.29218724936283E+03 -.22698731958242E+03 -.32025682662706E+03
 0.30455863281976E+03 0.66309997749612E+03 0.35701855151226E+03 0.37066828744478E+03 0.27706163916503E+03
 0.37066828744478E+03 0.51531039368776E+03 0.37071079947388E+03 0.27683414678386E+03 0.37071079947388E+03
 0.51514514616519E+03 0.37066828744478E+03 0.27706163916503E+03 0.37066828744478E+03 0.51531039368776E+03
 0.37071079947388E+03 0.27683414678386E+03 0.37071079947388E+03 0.51514514616519E+03 0.23685564461219E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38450822762698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16253571744307E+00 0.00000000000000E+00 0.00000000000000E+00 0.16253571744307E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16933161704906E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16933161704906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255630530370E+00 0.17091906992287E+00 0.35537443633466E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1800.82442151
 0.98453732713568E+00 0.32992167261003E+03 0.49528207420446E+03 0.44101424493784E+03 0.42532211197109E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13004217217906E+00 0.00000000000000E+00 -.18903044396597E+02
 0.88534801188053E-03 0.12773104048027E+01 0.80000000000000E+04 0.30000000000000E+04 0.62631604423794E+01
 0.23486851658923E+01 0.36247676542523E+03 0.29228798596398E+03 0.36029414397747E+03 0.41722730751488E+03
 0.29219711592408E+03 0.29225683261338E+03 0.35546361748454E+03 0.41720197072352E+03 0.29219146323612E+03
 0.29225672599668E+03 0.36029414397747E+03 0.41722730751488E+03 0.29219711592408E+03 0.29225683261338E+03
 0.35546361748454E+03 0.41720197072352E+03 0.29219146323612E+03 0.29225672599668E+03 0.46586532355228E+03
 0.37686903972975E+03 0.19634489167019E+04 0.17913228662542E+04 0.55634229127532E+03 0.80918153105594E+03
 0.25005752832424E+03 0.13053228091186E+04 0.14348563270907E+04 0.11778026532408E+04 0.19764767277189E+04
 0.12152920328785E+04 0.14344881921128E+04 0.11134553684027E+04 0.19763227470004E+04 0.13053228091186E+04
 0.14348563270907E+04 0.11778026532408E+04 0.19764767277190E+04 0.12152920328785E+04 0.14344881921128E+04
 0.11134553684027E+04 0.19763227470003E+04 0.16476491925809E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57018818319867E+03 0.12948437658500E+01
 0.12948437658500E+01 0.70405454306989E+01 0.00000000000000E+00 0.35551141315848E+03 0.35551141315848E+03
 0.35551141315848E+03 0.35551141315848E+03 0.00000000000000E+00 0.00000000000000E+00 0.13113042232070E+00
 0.00000000000000E+00 -.10578110970761E+02 0.10000000000000E-02 0.17601238772376E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45451346370889E+01 0.17044254889083E+01 0.37687231159127E+03 0.46586245737892E+03
 0.31955966368798E+03 0.31955966368798E+03 0.29216578637904E+03 0.29216544472631E+03 0.31955188662323E+03
 0.31955188662323E+03 0.29216578598035E+03 0.29216544433625E+03 0.31955966368798E+03 0.31955966368798E+03
 0.29216578637904E+03 0.29216544472631E+03 0.31955188662323E+03 0.31955188662323E+03 0.29216578598035E+03
 0.29216544433625E+03 0.31594502040176E+03 0.29218823779862E+03 -.22825151353322E+03 -.32202776051417E+03
 0.30520284569623E+03 0.66411061639086E+03 0.35738175646615E+03 0.37173741684854E+03 0.27759094632151E+03
 0.37173741684854E+03 0.51601929279921E+03 0.37178022127407E+03 0.27736283661600E+03 0.37178022127407E+03
 0.51585374825324E+03 0.37173741684854E+03 0.27759094632151E+03 0.37173741684854E+03 0.51601929279921E+03
 0.37178022127407E+03 0.27736283661600E+03 0.37178022127407E+03 0.51585374825324E+03 0.23688580790465E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38465629698463E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16248711868856E+00 0.00000000000000E+00 0.00000000000000E+00 0.16248711868856E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16927207064722E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16927207064722E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255625616694E+00 0.17085317844266E+00 0.35551141315848E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1812.38303337
 0.98446859237946E+00 0.33010786132827E+03 0.49553132637627E+03 0.44124659111549E+03 0.42555134470009E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12999531179460E+00 0.00000000000000E+00 -.18908627378459E+02
 0.88484860569173E-03 0.12772660871888E+01 0.80000000000000E+04 0.30000000000000E+04 0.62633777567895E+01
 0.23487666587960E+01 0.36277513317046E+03 0.29229274645952E+03 0.36058084667366E+03 0.41761582182244E+03
 0.29219896179724E+03 0.29226090707078E+03 0.35574378477044E+03 0.41759053187459E+03 0.29219310174237E+03
 0.29226079721866E+03 0.36058084667366E+03 0.41761582182244E+03 0.29219896179724E+03 0.29226090707078E+03
 0.35574378477044E+03 0.41759053187459E+03 0.29219310174237E+03 0.29226079721866E+03 0.46620177087719E+03
 0.37717142782615E+03 0.19657123615160E+04 0.17928718937672E+04 0.55544062404670E+03 0.80736854701379E+03
 0.24915071984686E+03 0.13068464614844E+04 0.14351951749118E+04 0.11788199467293E+04 0.19755141836735E+04
 0.12169553287614E+04 0.14348288497890E+04 0.11146647758629E+04 0.19753614625750E+04 0.13068464614844E+04
 0.14351951749118E+04 0.11788199467293E+04 0.19755141836735E+04 0.12169553287614E+04 0.14348288497890E+04
 0.11146647758629E+04 0.19753614625749E+04 0.16474247999971E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57044202432564E+03 0.12948438069851E+01
 0.12948438069851E+01 0.70867798781411E+01 0.00000000000000E+00 0.35569334058677E+03 0.35569334058677E+03
 0.35569334058677E+03 0.35569334058677E+03 0.00000000000000E+00 0.00000000000000E+00 0.13107102821751E+00
 0.00000000000000E+00 -.10569211055658E+02 0.10000000000000E-02 0.17602801759945E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45447310655988E+01 0.17042741495995E+01 0.37717467112304E+03 0.46619894015320E+03
 0.31970572199635E+03 0.31970572199635E+03 0.29216641626787E+03 0.29216606098243E+03 0.31969789199262E+03
 0.31969789199262E+03 0.29216641581592E+03 0.29216606054027E+03 0.31970572199635E+03 0.31970572199635E+03
 0.29216641626787E+03 0.29216606098243E+03 0.31969789199262E+03 0.31969789199262E+03 0.29216641581592E+03
 0.29216606054027E+03 0.31607561424603E+03 0.29218958415786E+03 -.22992202249570E+03 -.32436486998965E+03
 0.30605770205012E+03 0.66544895419198E+03 0.35786096363162E+03 0.37315260425802E+03 0.27829292880155E+03
 0.37315260425802E+03 0.51695750911743E+03 0.37319579859343E+03 0.27806400392834E+03 0.37319579859343E+03
 0.51679157517856E+03 0.37315260425802E+03 0.27829292880155E+03 0.37315260425802E+03 0.51695750911743E+03
 0.37319579859343E+03 0.27806400392834E+03 0.37319579859343E+03 0.51679157517856E+03 0.23692752087112E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38485298769746E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16242360967082E+00 0.00000000000000E+00 0.00000000000000E+00 0.16242360967082E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16919331207052E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16919331207052E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255618763887E+00 0.17076573889768E+00 0.35569334058677E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1821.05199226
 0.98441886312425E+00 0.33024719854725E+03 0.49571749645356E+03 0.44142013560497E+03 0.42572261256757E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12996025009960E+00 0.00000000000000E+00 -.18912793934003E+02
 0.88447523581034E-03 0.12772337927549E+01 0.80000000000000E+04 0.30000000000000E+04 0.62635361242243E+01
 0.23488260465841E+01 0.36299842707128E+03 0.29229638993959E+03 0.36079542417657E+03 0.41790606975300E+03
 0.29220038004304E+03 0.29226403439968E+03 0.35595351349102E+03 0.41788081501857E+03 0.29219436104891E+03
 0.29226392208379E+03 0.36079542417657E+03 0.41790606975300E+03 0.29220038004304E+03 0.29226403439968E+03
 0.35595351349102E+03 0.41788081501857E+03 0.29219436104891E+03 0.29226392208379E+03 0.46645260127020E+03
 0.37739645093062E+03 0.19673998812642E+04 0.17940245409096E+04 0.55477340212811E+03 0.80602581969102E+03
 0.24847855055227E+03 0.13079833585850E+04 0.14354430433228E+04 0.11795776772738E+04 0.19747904240431E+04
 0.12181967479460E+04 0.14350780765077E+04 0.11155660599231E+04 0.19746386487681E+04 0.13079833585850E+04
 0.14354430433228E+04 0.11795776772738E+04 0.19747904240431E+04 0.12181967479460E+04 0.14350780765076E+04
 0.11155660599231E+04 0.19746386487681E+04 0.16472562426299E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57063141053455E+03 0.12948438376841E+01
 0.12948438376841E+01 0.71214557137228E+01 0.00000000000000E+00 0.35582924126108E+03 0.35582924126108E+03
 0.35582924126108E+03 0.35582924126108E+03 0.00000000000000E+00 0.00000000000000E+00 0.13102692399517E+00
 0.00000000000000E+00 -.10562572322770E+02 0.10000000000000E-02 0.17603921330800E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45444420306531E+01 0.17041657614949E+01 0.37739967252539E+03 0.46644979704155E+03
 0.31981505936689E+03 0.31981505936689E+03 0.29216690061475E+03 0.29216653484654E+03 0.31980718976840E+03
 0.31980718976840E+03 0.29216690012087E+03 0.29216653436335E+03 0.31981505936689E+03 0.31981505936689E+03
 0.29216690061475E+03 0.29216653484654E+03 0.31980718976840E+03 0.31980718976840E+03 0.29216690012087E+03
 0.29216653436335E+03 0.31617339972788E+03 0.29219061548088E+03 -.23116402968642E+03 -.32610031632596E+03
 0.30669563069405E+03 0.66644548684115E+03 0.35821637799364E+03 0.37420630218319E+03 0.27881644506920E+03
 0.37420630218319E+03 0.51765563867592E+03 0.37424978898245E+03 0.27858691474309E+03 0.37424978898245E+03
 0.51748941763416E+03 0.37420630218319E+03 0.27881644506920E+03 0.37420630218319E+03 0.51765563867592E+03
 0.37424978898245E+03 0.27858691474309E+03 0.37424978898245E+03 0.51748941763416E+03 0.23695958600141E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38499992707692E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16237618002285E+00 0.00000000000000E+00 0.00000000000000E+00 0.16237618002285E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16913472929617E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16913472929617E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255613638940E+00 0.17070047940000E+00 0.35582924126108E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1830.77559116
 0.98437437306750E+00 0.33040205123420E+03 0.49592456775700E+03 0.44161252661344E+03 0.42591237932468E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12992121482960E+00 0.00000000000000E+00 -.18925425971629E+02
 0.88406059011365E-03 0.12771982748772E+01 0.80000000000000E+04 0.30000000000000E+04 0.62637103082287E+01
 0.23488913655858E+01 0.36324706341222E+03 0.29230058026490E+03 0.36103437647482E+03 0.41823153436026E+03
 0.29220201903956E+03 0.29226764385234E+03 0.35618687061446E+03 0.41820631789650E+03 0.29219581692814E+03
 0.29226752872085E+03 0.36103437647482E+03 0.41823153436025E+03 0.29220201903956E+03 0.29226764385234E+03
 0.35618687061446E+03 0.41820631789650E+03 0.29219581692814E+03 0.29226752872085E+03 0.46673668356278E+03
 0.37765310627559E+03 0.19692720985750E+04 0.17952988062899E+04 0.55392324906566E+03 0.80437379045319E+03
 0.24768092514220E+03 0.13092469840087E+04 0.14356779591056E+04 0.11804172509011E+04 0.19739238092730E+04
 0.12195791749963E+04 0.14353144971906E+04 0.11165689753592E+04 0.19737730864012E+04 0.13092469840087E+04
 0.14356779591056E+04 0.11804172509011E+04 0.19739238092730E+04 0.12195791749963E+04 0.14353144971906E+04
 0.11165689753592E+04 0.19737730864012E+04 0.16470018520304E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57084043346688E+03 0.12948439307562E+01
 0.12948439307562E+01 0.71603501093161E+01 0.00000000000000E+00 0.35598149490274E+03 0.35598149490274E+03
 0.35598149490274E+03 0.35598149490274E+03 0.00000000000000E+00 0.00000000000000E+00 0.13097788805624E+00
 0.00000000000000E+00 -.10564053511184E+02 0.10000000000000E-02 0.17605106251286E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45441361647084E+01 0.17040510617656E+01 0.37765628572845E+03 0.46673392149010E+03
 0.31993636186639E+03 0.31993636186639E+03 0.29216783602724E+03 0.29216708301544E+03 0.31992844824793E+03
 0.31992844824793E+03 0.29216783547175E+03 0.29216708248341E+03 0.31993636186639E+03 0.31993636186639E+03
 0.29216783602724E+03 0.29216708301544E+03 0.31992844824793E+03 0.31992844824793E+03 0.29216783547175E+03
 0.29216708248341E+03 0.31628181282823E+03 0.29219180286927E+03 -.23261004485637E+03 -.32812608766769E+03
 0.30741965647303E+03 0.66758755590763E+03 0.35863080115223E+03 0.37541898820524E+03 0.27941193932622E+03
 0.37541898820524E+03 0.51845987055234E+03 0.37546279971035E+03 0.27918168093162E+03 0.37546279971035E+03
 0.51829327666204E+03 0.37541898820524E+03 0.27941193932622E+03 0.37541898820524E+03 0.51845987055233E+03
 0.37546279971035E+03 0.27918168093162E+03 0.37546279971035E+03 0.51829327666204E+03 0.23698872227645E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38516652946831E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16239812153235E+00 0.00000000000000E+00 0.00000000000000E+00 0.16239812153235E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16910091531775E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16910091531775E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255582131813E+00 0.17062715658900E+00 0.35598149490274E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1840.82473611
 0.98431217255945E+00 0.33056360422983E+03 0.49613958076647E+03 0.44181343103699E+03 0.42611081831131E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12988105394815E+00 0.00000000000000E+00 -.18921564696337E+02
 0.88362856583265E-03 0.12771569026616E+01 0.80000000000000E+04 0.30000000000000E+04 0.62639132148352E+01
 0.23489674555632E+01 0.36350588820643E+03 0.29230495701596E+03 0.36128312584564E+03 0.41856414627298E+03
 0.29220373445392E+03 0.29227141949826E+03 0.35643031820016E+03 0.41853897192530E+03 0.29219734093692E+03
 0.29227130143393E+03 0.36128312584564E+03 0.41856414627298E+03 0.29220373445392E+03 0.29227141949826E+03
 0.35643031820016E+03 0.41853897192531E+03 0.29219734093692E+03 0.29227130143393E+03 0.46701943176421E+03
 0.37790404372307E+03 0.19712156583065E+04 0.17966223031177E+04 0.55328682938714E+03 0.80302773352457E+03
 0.24697446999049E+03 0.13105570795508E+04 0.14359894525097E+04 0.11812876086291E+04 0.19731366623416E+04
 0.12210081498353E+04 0.14356275823133E+04 0.11176017589689E+04 0.19729870452479E+04 0.13105570795508E+04
 0.14359894525098E+04 0.11812876086291E+04 0.19731366623416E+04 0.12210081498353E+04 0.14356275823133E+04
 0.11176017589689E+04 0.19729870452479E+04 0.16468736066478E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57105994041711E+03 0.12948439023066E+01
 0.12948439023066E+01 0.72005466890977E+01 0.00000000000000E+00 0.35613789198820E+03 0.35613789198820E+03
 0.35613789198820E+03 0.35613789198820E+03 0.00000000000000E+00 0.00000000000000E+00 0.13092769346305E+00
 0.00000000000000E+00 -.10546947833970E+02 0.10000000000000E-02 0.17606294258581E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45438295432902E+01 0.17039360787338E+01 0.37790722950444E+03 0.46701667479675E+03
 0.32006370087068E+03 0.32006370087068E+03 0.29216789190546E+03 0.29216765698936E+03 0.32005574132361E+03
 0.32005574132361E+03 0.29216789131389E+03 0.29216765640556E+03 0.32006370087068E+03 0.32006370087068E+03
 0.29216789190546E+03 0.29216765698936E+03 0.32005574132361E+03 0.32005574132361E+03 0.29216789131389E+03
 0.29216765640556E+03 0.31639584102714E+03 0.29219304366698E+03 -.23395810982722E+03 -.32999647085458E+03
 0.30814458720380E+03 0.66870509821580E+03 0.35901978807598E+03 0.37658752156696E+03 0.28000527141794E+03
 0.37658752156696E+03 0.51923867106328E+03 0.37663167534854E+03 0.27977437965463E+03 0.37663167534854E+03
 0.51907181168697E+03 0.37658752156696E+03 0.28000527141794E+03 0.37658752156696E+03 0.51923867106328E+03
 0.37663167534854E+03 0.27977437965463E+03 0.37663167534854E+03 0.51907181168697E+03 0.23704164876052E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38533372224107E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16226262682547E+00 0.00000000000000E+00 0.00000000000000E+00 0.16226262682547E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16900462299903E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16900462299903E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255603649745E+00 0.17055246757653E+00 0.35613789198820E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1850.94315877
 0.98425501816846E+00 0.33072559441982E+03 0.49635436926904E+03 0.44201405166956E+03 0.42630907773304E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12984067052224E+00 0.00000000000000E+00 -.18926818964887E+02
 0.88319571663292E-03 0.12771188646811E+01 0.80000000000000E+04 0.30000000000000E+04 0.62640997805619E+01
 0.23490374177107E+01 0.36376503086310E+03 0.29230945526713E+03 0.36153219902573E+03 0.41889909487356E+03
 0.29220550442990E+03 0.29227531111694E+03 0.35667392148202E+03 0.41887396199456E+03 0.29219891391529E+03
 0.29227519005419E+03 0.36153219902573E+03 0.41889909487356E+03 0.29220550442990E+03 0.29227531111694E+03
 0.35667392148202E+03 0.41887396199456E+03 0.29219891391529E+03 0.29227519005419E+03 0.46730704164772E+03
 0.37816054617845E+03 0.19731546028037E+04 0.17979419649800E+04 0.55254253381702E+03 0.80152365083339E+03
 0.24621840434729E+03 0.13118655078634E+04 0.14362593995121E+04 0.11821569102858E+04 0.19722876861812E+04
 0.12224377276990E+04 0.14358991143072E+04 0.11186365469984E+04 0.19721391724076E+04 0.13118655078634E+04
 0.14362593995121E+04 0.11821569102858E+04 0.19722876861812E+04 0.12224377276990E+04 0.14358991143072E+04
 0.11186365469984E+04 0.19721391724076E+04 0.16466807550898E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57127853290067E+03 0.12948439410197E+01
 0.12948439410197E+01 0.72410203797494E+01 0.00000000000000E+00 0.35629450223707E+03 0.35629450223707E+03
 0.35629450223707E+03 0.35629450223707E+03 0.00000000000000E+00 0.00000000000000E+00 0.13087762237702E+00
 0.00000000000000E+00 -.10539705030590E+02 0.10000000000000E-02 0.17607446940747E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45435320787402E+01 0.17038245295276E+01 0.37816370692405E+03 0.46730431643668E+03
 0.32019074094467E+03 0.32019074094467E+03 0.29216857627722E+03 0.29216824970986E+03 0.32018273555312E+03
 0.32018273555312E+03 0.29216857562733E+03 0.29216824907139E+03 0.32019074094467E+03 0.32019074094467E+03
 0.29216857627722E+03 0.29216824970986E+03 0.32018273555312E+03 0.32018273555312E+03 0.29216857562733E+03
 0.29216824907139E+03 0.31650954728495E+03 0.29219432005106E+03 -.23536996220020E+03 -.33196212139109E+03
 0.30887591666300E+03 0.66983823268610E+03 0.35941793643979E+03 0.37778854672152E+03 0.28060384344982E+03
 0.37778854672152E+03 0.52002993090422E+03 0.37783304206324E+03 0.28037226728488E+03 0.37783304206324E+03
 0.51986275522463E+03 0.37778854672151E+03 0.28060384344982E+03 0.37778854672151E+03 0.52002993090422E+03
 0.37783304206324E+03 0.28037226728488E+03 0.37783304206324E+03 0.51986275522463E+03 0.23707918490052E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38550301859998E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16221192220877E+00 0.00000000000000E+00 0.00000000000000E+00 0.16221192220877E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16893618473091E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16893618473091E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255596636840E+00 0.17047744366766E+00 0.35629450223707E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1861.25391087
 0.98419896774749E+00 0.33089017897985E+03 0.49657216478985E+03 0.44221748498700E+03 0.42651017589934E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12979968319608E+00 0.00000000000000E+00 -.18931530528873E+02
 0.88275637460189E-03 0.12770808263613E+01 0.80000000000000E+04 0.30000000000000E+04 0.62642863590659E+01
 0.23491073846497E+01 0.36402847541972E+03 0.29231413134575E+03 0.36178542515713E+03 0.41923907241758E+03
 0.29220735143960E+03 0.29227936788007E+03 0.35692162920738E+03 0.41921398185807E+03 0.29220055586090E+03
 0.29227924371656E+03 0.36178542515713E+03 0.41923907241758E+03 0.29220735143960E+03 0.29227936788007E+03
 0.35692162920738E+03 0.41921398185807E+03 0.29220055586090E+03 0.29227924371656E+03 0.46759837577408E+03
 0.37842007148182E+03 0.19751160137878E+04 0.17992729045788E+04 0.55179045711457E+03 0.80000472416710E+03
 0.24545531476695E+03 0.13131903261650E+04 0.14365231395965E+04 0.11830345360936E+04 0.19714150399983E+04
 0.12238858316391E+04 0.14361644705375E+04 0.11196823183500E+04 0.19712676519619E+04 0.13131903261650E+04
 0.14365231395966E+04 0.11830345360936E+04 0.19714150399983E+04 0.12238858316391E+04 0.14361644705375E+04
 0.11196823183500E+04 0.19712676519619E+04 0.16464791358898E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57149993527325E+03 0.12948439757343E+01
 0.12948439757343E+01 0.72822633881429E+01 0.00000000000000E+00 0.35645342243875E+03 0.35645342243875E+03
 0.35645342243875E+03 0.35645342243875E+03 0.00000000000000E+00 0.00000000000000E+00 0.13082706846815E+00
 0.00000000000000E+00 -.10531782589583E+02 0.10000000000000E-02 0.17608570202129E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45432422440710E+01 0.17037158415266E+01 0.37842319697726E+03 0.46759568847080E+03
 0.32031990808674E+03 0.32031990808674E+03 0.29216920637913E+03 0.29216886873421E+03 0.32031185611451E+03
 0.32031185611451E+03 0.29216920566986E+03 0.29216886803741E+03 0.32031990808674E+03 0.32031990808674E+03
 0.29216920637913E+03 0.29216886873421E+03 0.32031185611451E+03 0.32031185611451E+03 0.29216920566986E+03
 0.29216886803741E+03 0.31662518384242E+03 0.29219564807411E+03 -.23679806303436E+03 -.33394820528994E+03
 0.30961744326200E+03 0.67098478548983E+03 0.35981925501152E+03 0.37900443627997E+03 0.28121039726999E+03
 0.37900443627997E+03 0.52083008339334E+03 0.37904927961869E+03 0.28097813033270E+03 0.37904927961869E+03
 0.52066259085574E+03 0.37900443627997E+03 0.28121039726999E+03 0.37900443627997E+03 0.52083008339334E+03
 0.37904927961869E+03 0.28097813033270E+03 0.37904927961869E+03 0.52066259085575E+03 0.23711770516146E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38567486975852E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16215482488847E+00 0.00000000000000E+00 0.00000000000000E+00 0.16215482488847E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16886799537671E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16886799537671E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255591201808E+00 0.17040139813648E+00 0.35645342243875E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1871.56466297
 0.98414334141476E+00 0.33105443035830E+03 0.49678905747406E+03 0.44242018090145E+03 0.42671062043426E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12975888980452E+00 0.00000000000000E+00 -.18936281981919E+02
 0.88231835704975E-03 0.12770426718192E+01 0.80000000000000E+04 0.30000000000000E+04 0.62644735188087E+01
 0.23491775695533E+01 0.36429132896093E+03 0.29231889992009E+03 0.36203810084579E+03 0.41957769135202E+03
 0.29220924209405E+03 0.29228351621929E+03 0.35716884646411E+03 0.41955264319763E+03 0.29220223711889E+03
 0.29228338891002E+03 0.36203810084579E+03 0.41957769135202E+03 0.29220924209405E+03 0.29228351621929E+03
 0.35716884646411E+03 0.41955264319764E+03 0.29220223711889E+03 0.29228338891002E+03 0.46788794400597E+03
 0.37867761193869E+03 0.19770666163161E+04 0.18005945022099E+04 0.55104921807633E+03 0.79850598708981E+03
 0.24470152292309E+03 0.13145085885610E+04 0.14367797744778E+04 0.11839065646601E+04 0.19705405523016E+04
 0.12253271178479E+04 0.14364227215029E+04 0.11207218509081E+04 0.19703942902370E+04 0.13145085885610E+04
 0.14367797744778E+04 0.11839065646601E+04 0.19705405523016E+04 0.12253271178479E+04 0.14364227215029E+04
 0.11207218509081E+04 0.19703942902370E+04 0.16462781159053E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57172042767281E+03 0.12948440107427E+01
 0.12948440107427E+01 0.73235063965365E+01 0.00000000000000E+00 0.35661165926093E+03 0.35661165926093E+03
 0.35661165926093E+03 0.35661165926093E+03 0.00000000000000E+00 0.00000000000000E+00 0.13077697440439E+00
 0.00000000000000E+00 -.10523958525340E+02 0.10000000000000E-02 0.17609644516125E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45429650738687E+01 0.17036119027008E+01 0.37868070805853E+03 0.46788528939531E+03
 0.32044881694849E+03 0.32044881694849E+03 0.29216985189767E+03 0.29216950290414E+03 0.32044071854001E+03
 0.32044071854001E+03 0.29216985112630E+03 0.29216950214632E+03 0.32044881694849E+03 0.32044881694849E+03
 0.29216985189767E+03 0.29216950290414E+03 0.32044071854001E+03 0.32044071854001E+03 0.29216985112630E+03
 0.29216950214632E+03 0.31674061824777E+03 0.29219700356931E+03 -.23821441847282E+03 -.33591559964292E+03
 0.31035486857211E+03 0.67212222928809E+03 0.36021558637311E+03 0.38021144209410E+03 0.28181314965101E+03
 0.38021144209410E+03 0.52162321433538E+03 0.38025663335189E+03 0.28158019879595E+03 0.38025663335189E+03
 0.52145541049580E+03 0.38021144209410E+03 0.28181314965100E+03 0.38021144209410E+03 0.52162321433538E+03
 0.38025663335189E+03 0.28158019879595E+03 0.38025663335189E+03 0.52145541049580E+03 0.23715641895471E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38584599925430E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16209849635697E+00 0.00000000000000E+00 0.00000000000000E+00 0.16209849635697E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16880032015919E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16880032015919E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255585629296E+00 0.17032574525726E+00 0.35661165926093E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1880.13733030
 0.98409743633869E+00 0.33119073015259E+03 0.49696869297883E+03 0.44258813690591E+03 0.42687676746336E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12972512202244E+00 0.00000000000000E+00 -.18940375598345E+02
 0.88195520795553E-03 0.12770108619398E+01 0.80000000000000E+04 0.30000000000000E+04 0.62646295645819E+01
 0.23492360867182E+01 0.36450939945385E+03 0.29232293598426E+03 0.36224774092400E+03 0.41985824896920E+03
 0.29221084782286E+03 0.29228703606032E+03 0.35737398770062E+03 0.41983323610801E+03 0.29220366540665E+03
 0.29228690610117E+03 0.36224774092400E+03 0.41985824896919E+03 0.29221084782286E+03 0.29228703606032E+03
 0.35737398770062E+03 0.41983323610801E+03 0.29220366540665E+03 0.29228690610117E+03 0.46812749861301E+03
 0.37889041058018E+03 0.19786802499784E+04 0.18016863740950E+04 0.55043809951431E+03 0.79727066468575E+03
 0.24408037467387E+03 0.13155996713514E+04 0.14369868410967E+04 0.11846274353493E+04 0.19698106497641E+04
 0.12265203360790E+04 0.14366311313101E+04 0.11215815792234E+04 0.19696653238187E+04 0.13155996713514E+04
 0.14369868410967E+04 0.11846274353493E+04 0.19698106497641E+04 0.12265203360790E+04 0.14366311313101E+04
 0.11215815792234E+04 0.19696653238187E+04 0.16461098232270E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57190304717774E+03 0.12948440409043E+01
 0.12948440409043E+01 0.73577970658618E+01 0.00000000000000E+00 0.35674269819712E+03 0.35674269819712E+03
 0.35674269819712E+03 0.35674269819712E+03 0.00000000000000E+00 0.00000000000000E+00 0.13073566568441E+00
 0.00000000000000E+00 -.10517639319024E+02 0.10000000000000E-02 0.17610502297299E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45427437928486E+01 0.17035289223182E+01 0.37889348422000E+03 0.46812486967557E+03
 0.32055577278934E+03 0.32055577278934E+03 0.29217043553871E+03 0.29217004190949E+03 0.32054763588714E+03
 0.32054763588714E+03 0.29217043471214E+03 0.29217004109885E+03 0.32055577278934E+03 0.32055577278934E+03
 0.29217043553871E+03 0.29217004190949E+03 0.32054763588714E+03 0.32054763588714E+03 0.29217043471214E+03
 0.29217004109885E+03 0.31683641330508E+03 0.29219815178563E+03 -.23938506805450E+03 -.33754017252860E+03
 0.31096502800871E+03 0.67306140919067E+03 0.36054155604191E+03 0.38120921643311E+03 0.28231154551775E+03
 0.38120921643311E+03 0.52227761816425E+03 0.38125469682835E+03 0.28207802967758E+03 0.38125469682835E+03
 0.52210955812300E+03 0.38120921643311E+03 0.28231154551775E+03 0.38120921643311E+03 0.52227761816424E+03
 0.38125469682835E+03 0.28207802967758E+03 0.38125469682835E+03 0.52210955812301E+03 0.23718821504322E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38598772169158E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16205330247090E+00 0.00000000000000E+00 0.00000000000000E+00 0.16205330247090E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16874436157683E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16874436157683E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255580570753E+00 0.17026314181517E+00 0.35674269819712E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1890.36210647
 0.98404480929581E+00 0.33135284422131E+03 0.49718205257019E+03 0.44278759533507E+03 0.42707411456391E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12968504844196E+00 0.00000000000000E+00 -.18946392106900E+02
 0.88152365984281E-03 0.12769726669674E+01 0.80000000000000E+04 0.30000000000000E+04 0.62648169431837E+01
 0.23493063536939E+01 0.36476888911048E+03 0.29232783681732E+03 0.36249721570744E+03 0.42019175905080E+03
 0.29221280433824E+03 0.29229132073182E+03 0.35761813626243E+03 0.42016678828891E+03 0.29220540620857E+03
 0.29229118757057E+03 0.36249721570744E+03 0.42019175905080E+03 0.29221280433824E+03 0.29229132073182E+03
 0.35761813626243E+03 0.42016678828891E+03 0.29220540620857E+03 0.29229118757057E+03 0.46841190709610E+03
 0.37914284555873E+03 0.19805932356561E+04 0.18029777667463E+04 0.54971129630175E+03 0.79580413790702E+03
 0.24334428512376E+03 0.13168941992362E+04 0.14372239556809E+04 0.11854808025354E+04 0.19689334802453E+04
 0.12279365884210E+04 0.14368698464874E+04 0.11226002286203E+04 0.19687892701624E+04 0.13168941992362E+04
 0.14372239556809E+04 0.11854808025354E+04 0.19689334802453E+04 0.12279365884210E+04 0.14368698464874E+04
 0.11226002286203E+04 0.19687892701624E+04 0.16459037952236E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57211969155607E+03 0.12948440852336E+01
 0.12948440852336E+01 0.73986961705621E+01 0.00000000000000E+00 0.35689846861997E+03 0.35689846861997E+03
 0.35689846861997E+03 0.35689846861997E+03 0.00000000000000E+00 0.00000000000000E+00 0.13068678989958E+00
 0.00000000000000E+00 -.10511468239602E+02 0.10000000000000E-02 0.17611480193346E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45424915521993E+01 0.17034343320748E+01 0.37914589193542E+03 0.46840930904112E+03
 0.32068304455795E+03 0.32068304455795E+03 0.29217117392095E+03 0.29217069917032E+03 0.32067486188027E+03
 0.32067486188027E+03 0.29217117302460E+03 0.29217069829407E+03 0.32068304455795E+03 0.32068304455795E+03
 0.29217117392095E+03 0.29217069917032E+03 0.32067486188027E+03 0.32067486188027E+03 0.29217117302460E+03
 0.29217069829407E+03 0.31695042225570E+03 0.29219954718624E+03 -.24077435423161E+03 -.33946639477971E+03
 0.31169039880536E+03 0.67417718487962E+03 0.36092833408024E+03 0.38239408290612E+03 0.28290383998706E+03
 0.38239408290612E+03 0.52305505295389E+03 0.38243990793912E+03 0.28266965308637E+03 0.38243990793912E+03
 0.52288668914543E+03 0.38239408290612E+03 0.28290383998706E+03 0.38239408290612E+03 0.52305505295389E+03
 0.38243990793912E+03 0.28266965308637E+03 0.38243990793912E+03 0.52288668914543E+03 0.23722690185313E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38615662558194E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16201042759269E+00 0.00000000000000E+00 0.00000000000000E+00 0.16201042759269E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16868445726008E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16868445726008E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255570703077E+00 0.17018874251834E+00 0.35689846861997E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1902.62661831
 0.98398830641397E+00 0.33154644324596E+03 0.49743665755705E+03 0.44302531388018E+03 0.42730930723824E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12963742171192E+00 0.00000000000000E+00 -.18954877495905E+02
 0.88100884030508E-03 0.12769246515380E+01 0.80000000000000E+04 0.30000000000000E+04 0.62650525153262E+01
 0.23493946932473E+01 0.36507936465119E+03 0.29233384089187E+03 0.36279573555524E+03 0.42059004051388E+03
 0.29221521109828E+03 0.29229658539168E+03 0.35791034631180E+03 0.42056512036884E+03 0.29220754833251E+03
 0.29229644833017E+03 0.36279573555523E+03 0.42059004051388E+03 0.29221521109828E+03 0.29229658539168E+03
 0.35791034631180E+03 0.42056512036884E+03 0.29220754833251E+03 0.29229644833017E+03 0.46875063915370E+03
 0.37944307353106E+03 0.19828670066440E+04 0.18045048331762E+04 0.54885215461419E+03 0.79406932888826E+03
 0.24247291350101E+03 0.13184352645702E+04 0.14374950625339E+04 0.11864916396949E+04 0.19678739167338E+04
 0.12296233973805E+04 0.14371428687911E+04 0.11238086764363E+04 0.19677310410036E+04 0.13184352645702E+04
 0.14374950625340E+04 0.11864916396949E+04 0.19678739167339E+04 0.12296233973806E+04 0.14371428687911E+04
 0.11238086764363E+04 0.19677310410035E+04 0.16456505568866E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57237729118948E+03 0.12948441477535E+01
 0.12948441477535E+01 0.74477542178929E+01 0.00000000000000E+00 0.35708487823768E+03 0.35708487823768E+03
 0.35708487823768E+03 0.35708487823768E+03 0.00000000000000E+00 0.00000000000000E+00 0.13062871620839E+00
 0.00000000000000E+00 -.10505747443369E+02 0.10000000000000E-02 0.17612575270725E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45422091187865E+01 0.17033284195449E+01 0.37944608794797E+03 0.46874807712687E+03
 0.32083540961968E+03 0.32083540961968E+03 0.29217200174184E+03 0.29217150842933E+03 0.32082717220023E+03
 0.32082717220023E+03 0.29217200076110E+03 0.29217150747058E+03 0.32083540961968E+03 0.32083540961968E+03
 0.29217200174184E+03 0.29217150842933E+03 0.32082717220023E+03 0.32082717220023E+03 0.29217200076110E+03
 0.29217150747058E+03 0.31708694232532E+03 0.29220125842916E+03 -.24242120149552E+03 -.34174547504163E+03
 0.31255889117798E+03 0.67551326341102E+03 0.36139157777715E+03 0.38380554290168E+03 0.28361305812096E+03
 0.38380554290168E+03 0.52398665326182E+03 0.38385178132826E+03 0.28337807576952E+03 0.38385178132826E+03
 0.52381793349647E+03 0.38380554290168E+03 0.28361305812096E+03 0.38380554290168E+03 0.52398665326182E+03
 0.38385178132826E+03 0.28337807576952E+03 0.38385178132826E+03 0.52381793349647E+03 0.23728001881536E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38635947459871E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16197168191755E+00 0.00000000000000E+00 0.00000000000000E+00 0.16197168191755E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16862475594942E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16862475594942E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255554114690E+00 0.17009974468891E+00 0.35708487823768E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1911.82500218
 0.98394173826512E+00 0.33169151816518E+03 0.49762678856489E+03 0.44320324243269E+03 0.42748548358017E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12960207129602E+00 0.00000000000000E+00 -.18954283418627E+02
 0.88062351066463E-03 0.12768861720070E+01 0.80000000000000E+04 0.30000000000000E+04 0.62652413154617E+01
 0.23494654932981E+01 0.36531170094978E+03 0.29233843270260E+03 0.36301914274260E+03 0.42088746534312E+03
 0.29221705870222E+03 0.29230062262599E+03 0.35812908179359E+03 0.42086258326241E+03 0.29220919329469E+03
 0.29230048259774E+03 0.36301914274260E+03 0.42088746534312E+03 0.29221705870222E+03 0.29230062262599E+03
 0.35812908179359E+03 0.42086258326241E+03 0.29220919329469E+03 0.29230048259774E+03 0.46900293742755E+03
 0.37966631560305E+03 0.19845649680515E+04 0.18056453008400E+04 0.54822076490938E+03 0.79279123363773E+03
 0.24182936490380E+03 0.13195858339942E+04 0.14376924807799E+04 0.11872462922746E+04 0.19670782842332E+04
 0.12308829387667E+04 0.14373417232300E+04 0.11247107797406E+04 0.19669364089014E+04 0.13195858339942E+04
 0.14376924807799E+04 0.11872462922746E+04 0.19670782842332E+04 0.12308829387667E+04 0.14373417232300E+04
 0.11247107797406E+04 0.19669364089014E+04 0.16454645032828E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57257036335943E+03 0.12948441433763E+01
 0.12948441433763E+01 0.74845477533910E+01 0.00000000000000E+00 0.35722383355256E+03 0.35722383355256E+03
 0.35722383355256E+03 0.35722383355256E+03 0.00000000000000E+00 0.00000000000000E+00 0.13058555863907E+00
 0.00000000000000E+00 -.10493517106985E+02 0.10000000000000E-02 0.17613368707368E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45420045040296E+01 0.17032516890111E+01 0.37966930870038E+03 0.46900040024094E+03
 0.32094949240044E+03 0.32094949240044E+03 0.29217263778618E+03 0.29217213021187E+03 0.32094121405223E+03
 0.32094121405223E+03 0.29217263673934E+03 0.29217212918850E+03 0.32094949240044E+03 0.32094949240044E+03
 0.29217263778618E+03 0.29217213021187E+03 0.32094121405223E+03 0.32094121405223E+03 0.29217263673934E+03
 0.29217212918850E+03 0.31718918920506E+03 0.29220256839269E+03 -.24364595350246E+03 -.34343866007865E+03
 0.31320416224662E+03 0.67649693844319E+03 0.36172675538534E+03 0.38485387276544E+03 0.28413913908897E+03
 0.38485387276544E+03 0.52466996072489E+03 0.38490042126424E+03 0.28390356835729E+03 0.38490042126424E+03
 0.52450097991556E+03 0.38485387276544E+03 0.28413913908897E+03 0.38485387276544E+03 0.52466996072489E+03
 0.38490042126425E+03 0.28390356835729E+03 0.38490042126425E+03 0.52450097991556E+03 0.23731469732784E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38650801885398E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16187802243369E+00 0.00000000000000E+00 0.00000000000000E+00 0.16187802243369E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16854176385168E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16854176385168E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255564804136E+00 0.17003370401834E+00 0.35722383355256E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1921.02338606
 0.98388899297408E+00 0.33183668277873E+03 0.49781645918100E+03 0.44338123416150E+03 0.42766185972180E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12956678047218E+00 0.00000000000000E+00 -.18958416655580E+02
 0.88023823894636E-03 0.12768480804397E+01 0.80000000000000E+04 0.30000000000000E+04 0.62654282232584E+01
 0.23495355837219E+01 0.36554361483306E+03 0.29234310118009E+03 0.36324215271011E+03 0.42118385278925E+03
 0.29221894318446E+03 0.29230473669289E+03 0.35834746695341E+03 0.42115900886610E+03 0.29221087153644E+03
 0.29230459366236E+03 0.36324215271011E+03 0.42118385278925E+03 0.29221894318446E+03 0.29230473669289E+03
 0.35834746695341E+03 0.42115900886610E+03 0.29221087153644E+03 0.29230459366236E+03 0.46925390529061E+03
 0.37988795425204E+03 0.19862625290407E+04 0.18067888273849E+04 0.54760348620702E+03 0.79153663769232E+03
 0.24119513405426E+03 0.13207351730722E+04 0.14378903106279E+04 0.11880022282212E+04 0.19662885668847E+04
 0.12321409151776E+04 0.14375409927976E+04 0.11256134447979E+04 0.19661476954831E+04 0.13207351730722E+04
 0.14378903106279E+04 0.11880022282212E+04 0.19662885668848E+04 0.12321409151776E+04 0.14375409927976E+04
 0.11256134447979E+04 0.19661476954831E+04 0.16452883673765E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57276395498169E+03 0.12948441738298E+01
 0.12948441738298E+01 0.75213412888891E+01 0.00000000000000E+00 0.35736184280549E+03 0.35736184280549E+03
 0.35736184280549E+03 0.35736184280549E+03 0.00000000000000E+00 0.00000000000000E+00 0.13054273855244E+00
 0.00000000000000E+00 -.10486631891385E+02 0.10000000000000E-02 0.17614147419799E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45418037043381E+01 0.17031763891268E+01 0.37989092644997E+03 0.46925139309827E+03
 0.32106334927061E+03 0.32106334927061E+03 0.29217328700834E+03 0.29217276487670E+03 0.32105503010981E+03
 0.32105503010981E+03 0.29217328589293E+03 0.29217276378630E+03 0.32106334927061E+03 0.32106334927061E+03
 0.29217328700834E+03 0.29217276487670E+03 0.32105503010981E+03 0.32105503010981E+03 0.29217328589293E+03
 0.29217276378630E+03 0.31729126061452E+03 0.29220390131098E+03 -.24486417379483E+03 -.34512196997013E+03
 0.31384262632259E+03 0.67746877479326E+03 0.36205693533906E+03 0.38589296000513E+03 0.28465871114099E+03
 0.38589296000513E+03 0.52534400243179E+03 0.38593981852786E+03 0.28442255886285E+03 0.38593981852786E+03
 0.52517476685563E+03 0.38589296000513E+03 0.28465871114099E+03 0.38589296000513E+03 0.52534400243179E+03
 0.38593981852786E+03 0.28442255886285E+03 0.38593981852786E+03 0.52517476685563E+03 0.23734612561795E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38665731984513E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16182829927738E+00 0.00000000000000E+00 0.00000000000000E+00 0.16182829927738E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16848244723329E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16848244723329E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255560225751E+00 0.16996800531070E+00 0.35736184280549E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1930.22176993
 0.98383761339120E+00 0.33198159897899E+03 0.49800559825777E+03 0.44355871318574E+03 0.42783775429006E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12953151710325E+00 0.00000000000000E+00 -.18962615545920E+02
 0.87985396196250E-03 0.12768110360870E+01 0.80000000000000E+04 0.30000000000000E+04 0.62656100032761E+01
 0.23496037512285E+01 0.36577510192035E+03 0.29234784684965E+03 0.36346476380111E+03 0.42147922897364E+03
 0.29222086492161E+03 0.29230892827913E+03 0.35856549923764E+03 0.42145442329623E+03 0.29221258340871E+03
 0.29230878221083E+03 0.36346476380111E+03 0.42147922897364E+03 0.29222086492161E+03 0.29230892827913E+03
 0.35856549923764E+03 0.42145442329623E+03 0.29221258340871E+03 0.29230878221083E+03 0.46950359286973E+03
 0.38010805874816E+03 0.19879535583640E+04 0.18079268240772E+04 0.54699724216532E+03 0.79030097702569E+03
 0.24056874864955E+03 0.13218807725651E+04 0.14380862155294E+04 0.11887550883071E+04 0.19655020010688E+04
 0.12333949147966E+04 0.14377383375446E+04 0.11265125064102E+04 0.19653621338385E+04 0.13218807725651E+04
 0.14380862155294E+04 0.11887550883071E+04 0.19655020010688E+04 0.12333949147966E+04 0.14377383375446E+04
 0.11265125064102E+04 0.19653621338384E+04 0.16451158200720E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57295683964378E+03 0.12948442047670E+01
 0.12948442047670E+01 0.75581348243871E+01 0.00000000000000E+00 0.35749935735460E+03 0.35749935735460E+03
 0.35749935735460E+03 0.35749935735460E+03 0.00000000000000E+00 0.00000000000000E+00 0.13050023970088E+00
 0.00000000000000E+00 -.10479852867656E+02 0.10000000000000E-02 0.17614892983418E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45416114690740E+01 0.17031043009027E+01 0.38011100980669E+03 0.46950110626659E+03
 0.32117700252695E+03 0.32117700252695E+03 0.29217394955885E+03 0.29217341257098E+03 0.32116864267181E+03
 0.32116864267181E+03 0.29217394837238E+03 0.29217341141111E+03 0.32117700252695E+03 0.32117700252695E+03
 0.29217394955885E+03 0.29217341257098E+03 0.32116864267181E+03 0.32116864267181E+03 0.29217394837238E+03
 0.29217341141111E+03 0.31739317283373E+03 0.29220525737506E+03 -.24607260344365E+03 -.34678979428386E+03
 0.31447816087762E+03 0.67843420185243E+03 0.36238365017042E+03 0.38692513053021E+03 0.28517560405589E+03
 0.38692513053021E+03 0.52601316432781E+03 0.38697229896606E+03 0.28493887585718E+03 0.38697229896606E+03
 0.52584367855769E+03 0.38692513053021E+03 0.28517560405589E+03 0.38692513053021E+03 0.52601316432781E+03
 0.38697229896606E+03 0.28493887585719E+03 0.38697229896606E+03 0.52584367855769E+03 0.23737826882238E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38680611008064E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16177948584259E+00 0.00000000000000E+00 0.00000000000000E+00 0.16177948584259E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16842338117072E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16842338117072E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255555446896E+00 0.16990259028477E+00 0.35749935735460E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1942.52505882
 0.98377177637630E+00 0.33217480224382E+03 0.49825737597975E+03 0.44379492644997E+03 0.42807190609123E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12948446979693E+00 0.00000000000000E+00 -.18968505663888E+02
 0.87934216022488E-03 0.12767631027346E+01 0.80000000000000E+04 0.30000000000000E+04 0.62658452322641E+01
 0.23496919620990E+01 0.36608383334820E+03 0.29235432190740E+03 0.36376167860957E+03 0.42187296961536E+03
 0.29222349710737E+03 0.29231466307177E+03 0.35885632377263E+03 0.42184821501621E+03 0.29221492890207E+03
 0.29231451288230E+03 0.36376167860957E+03 0.42187296961536E+03 0.29222349710737E+03 0.29231466307177E+03
 0.35885632377263E+03 0.42184821501621E+03 0.29221492890207E+03 0.29231451288230E+03 0.46983620173297E+03
 0.38040110643457E+03 0.19902005345610E+04 0.18094354725891E+04 0.54618152813006E+03 0.78864571558146E+03
 0.23973327981074E+03 0.13234043266089E+04 0.14383331483255E+04 0.11897541818548E+04 0.19644378065775E+04
 0.12350634682714E+04 0.14379871963245E+04 0.11277069207449E+04 0.19642992842382E+04 0.13234043266089E+04
 0.14383331483255E+04 0.11897541818548E+04 0.19644378065775E+04 0.12350634682714E+04 0.14379871963245E+04
 0.11277069207449E+04 0.19642992842382E+04 0.16448742210283E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57321325186786E+03 0.12948442481651E+01
 0.12948442481651E+01 0.76073479799365E+01 0.00000000000000E+00 0.35768248956680E+03 0.35768248956680E+03
 0.35768248956680E+03 0.35768248956680E+03 0.00000000000000E+00 0.00000000000000E+00 0.13044387802866E+00
 0.00000000000000E+00 -.10471152181872E+02 0.10000000000000E-02 0.17615843215985E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45413664857896E+01 0.17030124321711E+01 0.38040402491347E+03 0.46983375221827E+03
 0.32132852059226E+03 0.32132852059226E+03 0.29217496323313E+03 0.29217430052673E+03 0.32132010650987E+03
 0.32132010650987E+03 0.29217496194194E+03 0.29217429926982E+03 0.32132852059226E+03 0.32132852059226E+03
 0.29217496323313E+03 0.29217430052673E+03 0.32132010650987E+03 0.32132010650987E+03 0.29217496194194E+03
 0.29217429926982E+03 0.31752905444694E+03 0.29220710946945E+03 -.24768558191120E+03 -.34901460794844E+03
 0.31532481884538E+03 0.67971839198417E+03 0.36281694904457E+03 0.38830110764014E+03 0.28586385869674E+03
 0.38830110764014E+03 0.52690290790966E+03 0.38834869004829E+03 0.28562636020012E+03 0.38834869004829E+03
 0.52673308579684E+03 0.38830110764014E+03 0.28586385869674E+03 0.38830110764014E+03 0.52690290790966E+03
 0.38834869004829E+03 0.28562636020012E+03 0.38834869004829E+03 0.52673308579684E+03 0.23741940188794E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38700426930753E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16171739575430E+00 0.00000000000000E+00 0.00000000000000E+00 0.16171739575430E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16834518304016E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16834518304016E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255548165959E+00 0.16981554367452E+00 0.35768248956680E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1950.72725141
 0.98372835144504E+00 0.33230335248827E+03 0.49842462803779E+03 0.44395189185904E+03 0.42822754485804E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12945321360947E+00 0.00000000000000E+00 -.18972113914895E+02
 0.87900195886228E-03 0.12767313346672E+01 0.80000000000000E+04 0.30000000000000E+04 0.62660011411759E+01
 0.23497504279410E+01 0.36628928705594E+03 0.29235871651022E+03 0.36395928232549E+03 0.42213434709766E+03
 0.29222528977955E+03 0.29231856485965E+03 0.35904992730537E+03 0.42210962672353E+03 0.29221652678653E+03
 0.29231841188767E+03 0.36395928232549E+03 0.42213434709766E+03 0.29222528977955E+03 0.29231856485965E+03
 0.35904992730537E+03 0.42210962672353E+03 0.29221652678653E+03 0.29231841188767E+03 0.47005631355515E+03
 0.38059456437514E+03 0.19916907183046E+04 0.18104340009319E+04 0.54565448650818E+03 0.78756965535132E+03
 0.23918689641060E+03 0.13244153723551E+04 0.14384956657146E+04 0.11904158997871E+04 0.19637315908417E+04
 0.12361708403823E+04 0.14381509982734E+04 0.11284981672543E+04 0.19635939650921E+04 0.13244153723551E+04
 0.14384956657146E+04 0.11904158997871E+04 0.19637315908418E+04 0.12361708403824E+04 0.14381509982734E+04
 0.11284981672543E+04 0.19635939650920E+04 0.16447183004437E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57338355847883E+03 0.12948442747505E+01
 0.12948442747505E+01 0.76401567503027E+01 0.00000000000000E+00 0.35780409011440E+03 0.35780409011440E+03
 0.35780409011440E+03 0.35780409011440E+03 0.00000000000000E+00 0.00000000000000E+00 0.13040660026916E+00
 0.00000000000000E+00 -.10465069148987E+02 0.10000000000000E-02 0.17616447711026E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45412106522433E+01 0.17029539945912E+01 0.38059746339635E+03 0.47005388671753E+03
 0.32142940308327E+03 0.32142940308327E+03 0.29217558499646E+03 0.29217490578299E+03 0.32142095294785E+03
 0.32142095294785E+03 0.29217558363620E+03 0.29217490445884E+03 0.32142940308327E+03 0.32142940308327E+03
 0.29217558499646E+03 0.29217490578299E+03 0.32142095294785E+03 0.32142095294785E+03 0.29217558363620E+03
 0.29217490445884E+03 0.31761955491342E+03 0.29220836763645E+03 -.24874665011459E+03 -.35047561166782E+03
 0.31588581094791E+03 0.68056684481927E+03 0.36310160481661E+03 0.38920912506798E+03 0.28631953312877E+03
 0.38920912506798E+03 0.52749014826431E+03 0.38925698356527E+03 0.28608153065835E+03 0.38925698356527E+03
 0.52732011054966E+03 0.38920912506798E+03 0.28631953312877E+03 0.38920912506798E+03 0.52749014826431E+03
 0.38925698356527E+03 0.28608153065835E+03 0.38925698356527E+03 0.52732011054966E+03 0.23744853590988E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38713585161094E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16167331945016E+00 0.00000000000000E+00 0.00000000000000E+00 0.16167331945016E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16829318706594E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16829318706594E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255544233742E+00 0.16975780298063E+00 0.35780409011440E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1963.03054030
 0.98366392975095E+00 0.33249584865287E+03 0.49867457154631E+03 0.44418656634552E+03 0.42846031744415E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12940650282956E+00 0.00000000000000E+00 -.18977601130767E+02
 0.87849301919368E-03 0.12766839128851E+01 0.80000000000000E+04 0.30000000000000E+04 0.62662338886385E+01
 0.23498377082394E+01 0.36659683779596E+03 0.29236542615949E+03 0.36425510224925E+03 0.42252488987531E+03
 0.29222803626712E+03 0.29232453661645E+03 0.35933981760745E+03 0.42250022098324E+03 0.29221897556002E+03
 0.29232437941820E+03 0.36425510224925E+03 0.42252488987531E+03 0.29222803626712E+03 0.29232453661645E+03
 0.35933981760744E+03 0.42250022098324E+03 0.29221897556002E+03 0.29232437941820E+03 0.47038452247781E+03
 0.38088248073496E+03 0.19939144937462E+04 0.18119220546805E+04 0.54487828853716E+03 0.78598081580560E+03
 0.23837813582575E+03 0.13259250308333E+04 0.14387334688132E+04 0.11914027359036E+04 0.19626727678433E+04
 0.12378246370462E+04 0.14383907270875E+04 0.11296784923707E+04 0.19625364858303E+04 0.13259250308333E+04
 0.14387334688132E+04 0.11914027359036E+04 0.19626727678433E+04 0.12378246370462E+04 0.14383907270875E+04
 0.11296784923707E+04 0.19625364858303E+04 0.16444869913358E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57363803603756E+03 0.12948443151801E+01
 0.12948443151801E+01 0.76893699058521E+01 0.00000000000000E+00 0.35798574715587E+03 0.35798574715587E+03
 0.35798574715587E+03 0.35798574715587E+03 0.00000000000000E+00 0.00000000000000E+00 0.13035111471915E+00
 0.00000000000000E+00 -.10456081052771E+02 0.10000000000000E-02 0.17617314008157E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45409873470472E+01 0.17028702551427E+01 0.38088535192903E+03 0.47038212854820E+03
 0.32158045212805E+03 0.32158045212805E+03 0.29217653838023E+03 0.29217583385548E+03 0.32157194809205E+03
 0.32157194809205E+03 0.29217653691233E+03 0.29217583242655E+03 0.32158045212805E+03 0.32158045212805E+03
 0.29217653838023E+03 0.29217583385548E+03 0.32157194809205E+03 0.32157194809205E+03 0.29217653691233E+03
 0.29217583242655E+03 0.31775509438642E+03 0.29221029037532E+03 -.25032355993975E+03 -.35264400400556E+03
 0.31672264865428E+03 0.68182932845177E+03 0.36352306655422E+03 0.39056052004892E+03 0.28699875770502E+03
 0.39056052004892E+03 0.52836316885437E+03 0.39060879250107E+03 0.28676000822289E+03 0.39060879250107E+03
 0.52819281507609E+03 0.39056052004892E+03 0.28699875770502E+03 0.39056052004892E+03 0.52836316885437E+03
 0.39060879250107E+03 0.28676000822289E+03 0.39060879250107E+03 0.52819281507609E+03 0.23749298607001E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38733244583051E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16160832362113E+00 0.00000000000000E+00 0.00000000000000E+00 0.16160832362113E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16821572425577E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16821572425577E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255538099740E+00 0.16967161546635E+00 0.35798574715587E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1971.23273289
 0.98362147858705E+00 0.33262395085443E+03 0.49884054835089E+03 0.44434247655250E+03 0.42861502143904E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12937548620271E+00 0.00000000000000E+00 -.18981265651399E+02
 0.87815465674137E-03 0.12766524224539E+01 0.80000000000000E+04 0.30000000000000E+04 0.62663884541284E+01
 0.23498956702981E+01 0.36680144855071E+03 0.29236997827120E+03 0.36445192235763E+03 0.42278425574553E+03
 0.29222990595883E+03 0.29232859785395E+03 0.35953272997058E+03 0.42275962126575E+03 0.29222064306256E+03
 0.29232843780329E+03 0.36445192235763E+03 0.42278425574553E+03 0.29222990595883E+03 0.29232859785395E+03
 0.35953272997058E+03 0.42275962126575E+03 0.29222064306256E+03 0.29232843780329E+03 0.47060205504759E+03
 0.38107296999982E+03 0.19953888718169E+04 0.18129070833472E+04 0.54436910890400E+03 0.78493651632746E+03
 0.23784556187893E+03 0.13269265692762E+04 0.14388870866614E+04 0.11920564702985E+04 0.19619658821067E+04
 0.12389220557602E+04 0.14385456279834E+04 0.11304607170927E+04 0.19618304951565E+04 0.13269265692762E+04
 0.14388870866614E+04 0.11920564702985E+04 0.19619658821067E+04 0.12389220557602E+04 0.14385456279834E+04
 0.11304607170927E+04 0.19618304951565E+04 0.16443333146072E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57380700244312E+03 0.12948443421801E+01
 0.12948443421801E+01 0.77221786762183E+01 0.00000000000000E+00 0.35810634919318E+03 0.35810634919318E+03
 0.35810634919318E+03 0.35810634919318E+03 0.00000000000000E+00 0.00000000000000E+00 0.13031440431811E+00
 0.00000000000000E+00 -.10450137658337E+02 0.10000000000000E-02 0.17617866267218E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45408450028286E+01 0.17028168760607E+01 0.38107582237243E+03 0.47059968318666E+03
 0.32168096311381E+03 0.32168096311381E+03 0.29217718795036E+03 0.29217646618017E+03 0.32167242326267E+03
 0.32167242326267E+03 0.29217718640797E+03 0.29217646467873E+03 0.32168096311381E+03 0.32168096311381E+03
 0.29217718795036E+03 0.29217646618017E+03 0.32167242326267E+03 0.32167242326267E+03 0.29217718640797E+03
 0.29217646467873E+03 0.31784530751589E+03 0.29221159605254E+03 -.25136567647697E+03 -.35407519418497E+03
 0.31727741946375E+03 0.68266415510415E+03 0.36380034854308E+03 0.39145461534683E+03 0.28744868975831E+03
 0.39145461534683E+03 0.52893991861712E+03 0.39150316364172E+03 0.28720944793686E+03 0.39150316364172E+03
 0.52876935872919E+03 0.39145461534683E+03 0.28744868975831E+03 0.39145461534683E+03 0.52893991861712E+03
 0.39150316364172E+03 0.28720944793686E+03 0.39150316364172E+03 0.52876935872920E+03 0.23752285901919E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38746297899920E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16156534303452E+00 0.00000000000000E+00 0.00000000000000E+00 0.16156534303452E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16816444575444E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16816444575444E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255533976561E+00 0.16961444336699E+00 0.35810634919318E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1983.53602178
 0.98355838720106E+00 0.33281576143051E+03 0.49908853323996E+03 0.44457554014784E+03 0.42884636958135E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12932915894060E+00 0.00000000000000E+00 -.18986718529291E+02
 0.87764850561398E-03 0.12766052277488E+01 0.80000000000000E+04 0.30000000000000E+04 0.62666201156857E+01
 0.23499825433821E+01 0.36710772792741E+03 0.29237692582869E+03 0.36474656089349E+03 0.42317182469940E+03
 0.29223276919450E+03 0.29233481096824E+03 0.35982157334163E+03 0.42314724196860E+03 0.29222319740314E+03
 0.29233464658681E+03 0.36474656089349E+03 0.42317182469940E+03 0.29223276919450E+03 0.29233481096824E+03
 0.35982157334163E+03 0.42314724196860E+03 0.29222319740314E+03 0.29233464658681E+03 0.47092647300843E+03
 0.38135657517963E+03 0.19975882790999E+04 0.18143741841462E+04 0.54361720926360E+03 0.78339157762199E+03
 0.23705628231207E+03 0.13284215216949E+04 0.14391099272662E+04 0.11930308450280E+04 0.19609036826031E+04
 0.12405605200901E+04 0.14387703918399E+04 0.11316270907622E+04 0.19607696369624E+04 0.13284215216949E+04
 0.14391099272662E+04 0.11930308450280E+04 0.19609036826031E+04 0.12405605200901E+04 0.14387703918399E+04
 0.11316270907622E+04 0.19607696369624E+04 0.16441032806283E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57405944607758E+03 0.12948443823566E+01
 0.12948443823566E+01 0.77713918317677E+01 0.00000000000000E+00 0.35828649488609E+03 0.35828649488609E+03
 0.35828649488609E+03 0.35828649488609E+03 0.00000000000000E+00 0.00000000000000E+00 0.13025974714102E+00
 0.00000000000000E+00 -.10441235457425E+02 0.10000000000000E-02 0.17618659017975E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45406406877152E+01 0.17027402578932E+01 0.38135939885697E+03 0.47092413439880E+03
 0.32183144314180E+03 0.32183144314180E+03 0.29217818354224E+03 0.29217743534000E+03 0.32182284974733E+03
 0.32182284974733E+03 0.29217818188392E+03 0.29217743372571E+03 0.32183144314180E+03 0.32183144314180E+03
 0.29217818354224E+03 0.29217743534000E+03 0.32182284974733E+03 0.32182284974733E+03 0.29217818188392E+03
 0.29217743372571E+03 0.31798040328664E+03 0.29221359067287E+03 -.25291563653789E+03 -.35620120601234E+03
 0.31810488261175E+03 0.68390612129739E+03 0.36421071427258E+03 0.39278572076094E+03 0.28811924750776E+03
 0.39278572076094E+03 0.52979711228514E+03 0.39283468260570E+03 0.28787927544834E+03 0.39283468260570E+03
 0.52962624988085E+03 0.39278572076094E+03 0.28811924750776E+03 0.39278572076094E+03 0.52979711228514E+03
 0.39283468260570E+03 0.28787927544834E+03 0.39283468260570E+03 0.52962624988085E+03 0.23756780986544E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38765796470370E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16150089283774E+00 0.00000000000000E+00 0.00000000000000E+00 0.16150089283774E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16808784302693E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16808784302693E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255527915041E+00 0.16952911693219E+00 0.35828649488609E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   1991.73821437
 0.98351659604530E+00 0.33294341024582E+03 0.49925321004796E+03 0.44473039398450E+03 0.42900014257465E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12929840839392E+00 0.00000000000000E+00 -.18990344724638E+02
 0.87731198822409E-03 0.12765737421528E+01 0.80000000000000E+04 0.30000000000000E+04 0.62667746764938E+01
 0.23500405036852E+01 0.36731149032888E+03 0.29238163762848E+03 0.36494259279091E+03 0.42342922522337E+03
 0.29223471752599E+03 0.29233903455188E+03 0.36001378542700E+03 0.42340467707696E+03 0.29222493603983E+03
 0.29233886724858E+03 0.36494259279091E+03 0.42342922522337E+03 0.29223471752599E+03 0.29233903455188E+03
 0.36001378542700E+03 0.42340467707696E+03 0.29222493603983E+03 0.29233886724858E+03 0.47114151349541E+03
 0.38154424918738E+03 0.19990466163428E+04 0.18153454863696E+04 0.54312375726502E+03 0.78237576660032E+03
 0.23653639054898E+03 0.13294133364060E+04 0.14392535157694E+04 0.11936763776620E+04 0.19601943650734E+04
 0.12416478049442E+04 0.14389152615707E+04 0.11324001355385E+04 0.19600612127332E+04 0.13294133364060E+04
 0.14392535157695E+04 0.11936763776620E+04 0.19601943650734E+04 0.12416478049442E+04 0.14389152615707E+04
 0.11324001355385E+04 0.19600612127332E+04 0.16439503113551E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57422709598427E+03 0.12948444090742E+01
 0.12948444090742E+01 0.78042006021340E+01 0.00000000000000E+00 0.35840608408195E+03 0.35840608408195E+03
 0.35840608408195E+03 0.35840608408195E+03 0.00000000000000E+00 0.00000000000000E+00 0.13022357517339E+00
 0.00000000000000E+00 -.10435330988987E+02 0.10000000000000E-02 0.17619165019916E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45405102857923E+01 0.17026913571721E+01 0.38154705359868E+03 0.47113919702907E+03
 0.32193157070456E+03 0.32193157070456E+03 0.29217886158231E+03 0.29217809537866E+03 0.32192294173480E+03
 0.32192294173480E+03 0.29217885984386E+03 0.29217809368636E+03 0.32193157070456E+03 0.32193157070456E+03
 0.29217886158231E+03 0.29217809537866E+03 0.32192294173480E+03 0.32192294173480E+03 0.29217885984386E+03
 0.29217809368636E+03 0.31807031628495E+03 0.29221494467918E+03 -.25394035730432E+03 -.35760506440261E+03
 0.31865337201555E+03 0.68472722390517E+03 0.36448058502954E+03 0.39366650746726E+03 0.28856336831387E+03
 0.39366650746726E+03 0.53036326318108E+03 0.39371574485784E+03 0.28832291486404E+03 0.39371574485784E+03
 0.53019220344750E+03 0.39366650746726E+03 0.28856336831387E+03 0.39366650746726E+03 0.53036326318108E+03
 0.39371574485784E+03 0.28832291486404E+03 0.39371574485784E+03 0.53019220344750E+03 0.23759777006691E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38778741717570E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16145812130507E+00 0.00000000000000E+00 0.00000000000000E+00 0.16145812130507E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16803707122694E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16803707122694E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255523893643E+00 0.16947252061302E+00 0.35840608408195E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
   2000.00000000
 0.98347589861521E+00 0.33307168201489E+03 0.49941849924794E+03 0.44488580406126E+03 0.42915449194568E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12926755769814E+00 0.00000000000000E+00 -.18994598561547E+02
 0.87697408310664E-03 0.12765419725237E+01 0.80000000000000E+04 0.30000000000000E+04 0.62669306393305E+01
 0.23500989897489E+01 0.36751639876390E+03 0.29238645063336E+03 0.36513973966389E+03 0.42368768841564E+03
 0.29223671315869E+03 0.29234335711082E+03 0.36020712321067E+03 0.42366317520580E+03 0.29222671730614E+03
 0.29234318683571E+03 0.36513973966389E+03 0.42368768841564E+03 0.29223671315869E+03 0.29234335711082E+03
 0.36020712321067E+03 0.42366317520580E+03 0.29222671730614E+03 0.29234318683571E+03 0.47135700033568E+03
 0.38173205793247E+03 0.20005072003776E+04 0.18163154769219E+04 0.54263400425417E+03 0.78136546568037E+03
 0.23601829140492E+03 0.13304074694323E+04 0.14393928365454E+04 0.11943215931723E+04 0.19594773446745E+04
 0.12427379363675E+04 0.14390558726320E+04 0.11331734169449E+04 0.19593450916326E+04 0.13304074694323E+04
 0.14393928365455E+04 0.11943215931723E+04 0.19594773446746E+04 0.12427379363675E+04 0.14390558726320E+04
 0.11331734169449E+04 0.19593450916326E+04 0.16437950290506E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.57439519744176E+03 0.12948444404163E+01
 0.12948444404163E+01 0.78372477446647E+01 0.00000000000000E+00 0.35852619616216E+03 0.35852619616216E+03
 0.35852619616216E+03 0.35852619616216E+03 0.00000000000000E+00 0.00000000000000E+00 0.13018735059826E+00
 0.00000000000000E+00 -.10430235915090E+02 0.10000000000000E-02 0.17619654090448E+01 0.80000000000000E+04
 0.30000000000000E+04 0.45403842543861E+01 0.17026440953948E+01 0.38173484128140E+03 0.47135470719982E+03
 0.32203228161889E+03 0.32203228161889E+03 0.29217939809420E+03 0.29217877191710E+03 0.32202361689496E+03
 0.32202361689496E+03 0.29217939628238E+03 0.29217877014388E+03 0.32203228161889E+03 0.32203228161889E+03
 0.29217939809420E+03 0.29217877191710E+03 0.32202361689496E+03 0.32202361689496E+03 0.29217939628238E+03
 0.29217877014388E+03 0.31816077084937E+03 0.29221632884078E+03 -.25496369983727E+03 -.35900533138480E+03
 0.31920380990747E+03 0.68555081910458E+03 0.36475099014757E+03 0.39454808801210E+03 0.28900889445706E+03
 0.39454808801210E+03 0.53093111331731E+03 0.39459760295583E+03 0.28876796255535E+03 0.39459760295583E+03
 0.53075986076077E+03 0.39454808801210E+03 0.28900889445706E+03 0.39454808801210E+03 0.53093111331731E+03
 0.39459760295583E+03 0.28876796255535E+03 0.39459760295583E+03 0.53075986076077E+03 0.23762970074116E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38791799219631E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16142096705902E+00 0.00000000000000E+00 0.00000000000000E+00 0.16142096705902E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16799418061412E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16799418061412E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17255517453902E+00 0.16941568987198E+00 0.35852619616216E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29215000000000E+03
