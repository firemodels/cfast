#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-25 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL25 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.197000                                                                                   
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
#CONDINIT 2000.000000 10.000000 25.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-25           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-25           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-25           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-25           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-25           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-25           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-25           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-25           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-25           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-25           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-25           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-25           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-25           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-25           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-25           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-25           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-25           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-25           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL25     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL25     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL25     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL25     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL25     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL25     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL25     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL25     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL25     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL25     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL25     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL25     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL25     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL25     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL25     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL25     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL25     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL25     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.80532846903523E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.48964472804235E-03 0.48964472804235E-03 0.69353284794257E-03 0.69700051218228E-03
 0.00000000000000E+00 0.46399744678626E-03 0.54604813057338E-03 0.46399744678626E-03 0.54604813057338E-03
 0.46164866459260E-03 0.54604159690122E-03 0.46164866459260E-03 0.54604159690122E-03 0.46399744672778E-03
 0.54604813051490E-03 0.46399744672778E-03 0.54604813051490E-03 0.46164866447564E-03 0.54604159695969E-03
 0.46164866447564E-03 0.54604159695969E-03 0.56262961797463E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.68981988274340E-07 0.99965475690843E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.57020558861763E-03 0.57020558861763E-03
 0.70034304895866E-03 0.70384476420346E-03 0.00000000000000E+00 0.49841658112229E-03 0.54981368628329E-03
 0.49841658112229E-03 0.54981368628329E-03 0.49407851325873E-03 0.54980613517315E-03 0.49407851325873E-03
 0.54980613517315E-03 0.49841658118077E-03 0.54981368634176E-03 0.49841658118077E-03 0.54981368634176E-03
 0.49407851331721E-03 0.54980613523163E-03 0.49407851331721E-03 0.54980613523163E-03 0.45477649087889E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.53010489291426E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.53010489291426E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19699970197231E+00 0.23260823942851E+00 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     10.36586771
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18194150360625E+02
 0.99982043769691E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000159342E+03 0.29815000000000E+03 0.29815000219888E+03 0.29815000219888E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000218758E+03 0.29815000218758E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000219888E+03 0.29815000219888E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000218758E+03 0.29815000218758E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000776788E+03
 0.29815000641146E+03 0.50284922241537E-03 0.50284922241537E-03 0.66370163745023E-03 0.66702014563749E-03
 .00000000000000E+00 0.47125275018042E-03 0.55968581973204E-03 0.47125275018042E-03 0.55968581973204E-03
 0.46881850477601E-03 0.55974009731988E-03 0.46881850477601E-03 0.55974009731988E-03 0.47125275018042E-03
 0.55968581979051E-03 0.47125275018042E-03 0.55968581979051E-03 0.46881850477601E-03 0.55974009720293E-03
 0.46881850477601E-03 0.55974009720293E-03 0.56293607954388E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999933784E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20109352562051E+02 0.99947625585577E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815000638629E+03 0.29815000779852E+03
 0.29815000236136E+03 0.29815000236136E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000234069E+03
 0.29815000234069E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000236136E+03 0.29815000236136E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000234069E+03 0.29815000234069E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000227448E+03 0.29815000000000E+03 0.54934138688702E-03 0.54934138688702E-03
 0.71258862982896E-03 0.71615157297810E-03 .00000000000000E+00 0.50604527495422E-03 0.55611225513460E-03
 0.50604527495422E-03 0.55611225513460E-03 0.50160639073566E-03 0.55621637028326E-03 0.50160639073566E-03
 0.55621637028326E-03 0.50604527489574E-03 0.55611225507613E-03 0.50604527489574E-03 0.55611225507613E-03
 0.50160639073566E-03 0.55621637022478E-03 0.50160639073566E-03 0.55621637022478E-03 0.45499922674241E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23178132121863E+00 0.00000000000000E+00 0.00000000000000E+00 0.23178132121863E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119151942E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119151942E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19633816182779E+00 0.23178109787775E+00 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     30.00000000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18194150350148E+02
 0.99982043769701E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000272602E+03 0.29815000000000E+03 0.29815000373729E+03 0.29815000373729E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000371788E+03 0.29815000371788E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000373729E+03 0.29815000373729E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000371788E+03 0.29815000371788E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001276420E+03
 0.29815001055808E+03 0.51101549979457E-03 0.51101549979457E-03 0.64485647884150E-03 0.64808076123571E-03
 .00000000000000E+00 0.47550824265189E-03 0.56797562498107E-03 0.47550824265189E-03 0.56797562498107E-03
 0.47302705692010E-03 0.56807355685059E-03 0.47302705692010E-03 0.56807355685059E-03 0.47550824259342E-03
 0.56797562503954E-03 0.47550824259342E-03 0.56797562503954E-03 0.47302705727096E-03 0.56807355720145E-03
 0.47302705727096E-03 0.56807355720145E-03 0.56289256369435E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999940309E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20109352552121E+02 0.99951032039852E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001053849E+03 0.29815001278788E+03
 0.29815000401305E+03 0.29815000401305E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000397779E+03
 0.29815000397779E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000401305E+03 0.29815000401305E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000397779E+03 0.29815000397779E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000386982E+03 0.29815000000000E+03 0.53622161634399E-03 0.53622161634399E-03
 0.71990828957702E-03 0.72350783102491E-03 .00000000000000E+00 0.51057256218806E-03 0.55985319199165E-03
 0.51057256218806E-03 0.55985319199165E-03 0.50607841592460E-03 0.56003616679909E-03 0.50607841592460E-03
 0.56003616679909E-03 0.51057256218806E-03 0.55985319199165E-03 0.51057256218806E-03 0.55985319199165E-03
 0.50607841598308E-03 0.56003616679909E-03 0.50607841598308E-03 0.56003616679909E-03 0.45496753847685E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23178132115189E+00 0.00000000000000E+00 0.00000000000000E+00 0.23178132115189E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119155169E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119155169E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19633816182812E+00 0.23178109787816E+00 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     40.00000000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18194150349059E+02
 0.99982043769703E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000318300E+03 0.29815000000000E+03 0.29815000435363E+03 0.29815000435363E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000433096E+03 0.29815000433096E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000435363E+03 0.29815000435363E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000433096E+03 0.29815000433096E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001468434E+03
 0.29815001215634E+03 0.51413368917790E-03 0.51413368917790E-03 0.63774834448495E-03 0.64093708620737E-03
 .00000000000000E+00 0.47711009216422E-03 0.57109981010842E-03 0.47711009216422E-03 0.57109981010842E-03
 0.47461276730140E-03 0.57121532753279E-03 0.47461276730140E-03 0.57121532753279E-03 0.47711009228118E-03
 0.57109981010842E-03 0.47711009228118E-03 0.57109981010842E-03 0.47461276730140E-03 0.57121532753279E-03
 0.47461276730140E-03 0.57121532753279E-03 0.56288368344497E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999943389E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20109352550988E+02 0.99952637817718E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001214169E+03 0.29815001470198E+03
 0.29815000467481E+03 0.29815000467481E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000463370E+03
 0.29815000463370E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000467481E+03 0.29815000467481E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000463370E+03 0.29815000463370E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000451025E+03 0.29815000000000E+03 0.53128966761486E-03 0.53128966761486E-03
 0.72268151166968E-03 0.72629491922803E-03 .00000000000000E+00 0.51228823201086E-03 0.56127071675943E-03
 0.51228823201086E-03 0.56127071675943E-03 0.50777563549600E-03 0.56148529793041E-03 0.50777563549600E-03
 0.56148529793041E-03 0.51228823201086E-03 0.56127071681791E-03 0.51228823201086E-03 0.56127071681791E-03
 0.50777563549600E-03 0.56148529793041E-03 0.50777563549600E-03 0.56148529793041E-03 0.45496525886377E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23178132114494E+00 0.00000000000000E+00 0.00000000000000E+00 0.23178132114494E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119154991E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119154991E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19633816182816E+00 0.23178109787821E+00 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     40.00025000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18194150349054E+02
 0.99982043769703E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000318301E+03 0.29815000000000E+03 0.29815000435365E+03 0.29815000435365E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000433098E+03 0.29815000433098E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000435365E+03 0.29815000435365E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000433098E+03 0.29815000433098E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001468439E+03
 0.29815001215638E+03 0.51413978099931E-03 0.51413978099931E-03 0.63775679051425E-03 0.64094557446682E-03
 .00000000000000E+00 0.47711672267398E-03 0.57110465432714E-03 0.47711672267398E-03 0.57110465432714E-03
 0.47461944997247E-03 0.57122017210237E-03 0.47461944997247E-03 0.57122017210237E-03 0.47711672261551E-03
 0.57110465438562E-03 0.47711672261551E-03 0.57110465438562E-03 0.47461944997247E-03 0.57122017210237E-03
 0.47461944997247E-03 0.57122017210237E-03 0.56289117345737E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999943389E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20109352550775E+02 0.99952637857813E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001214173E+03 0.29815001470203E+03
 0.29815000467483E+03 0.29815000467483E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000463372E+03
 0.29815000463372E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000467483E+03 0.29815000467483E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000463372E+03 0.29815000463372E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000451027E+03 0.29815000000000E+03 0.53129752531890E-03 0.53129752531890E-03
 0.72269763503466E-03 0.72631112320983E-03 .00000000000000E+00 0.51229930845380E-03 0.56127962575951E-03
 0.51229930845380E-03 0.56127962575951E-03 0.50778689602394E-03 0.56149420751526E-03 0.50778689602394E-03
 0.56149420751526E-03 0.51229930839533E-03 0.56127962575951E-03 0.51229930839533E-03 0.56127962575951E-03
 0.50778689596546E-03 0.56149420751526E-03 0.50778689596546E-03 0.56149420751526E-03 0.45497437868268E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23178132114494E+00 0.00000000000000E+00 0.00000000000000E+00 0.23178132114494E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119153674E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23178119153674E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19633816182816E+00 0.23178109787822E+00 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     50.01217547
 0.22365808375751E+01 0.29817917263472E+03 0.33390375634454E+03 0.30727011655930E+03 0.30652468744894E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22594779230849E+00 0.00000000000000E+00 0.19109665807566E+02
 0.10000907433629E-02 0.72647670391445E-01 0.79992741189657E+04 0.29997277946121E+04 0.11012053045740E+03
 0.41295198921524E+02 0.29948078335030E+03 0.29815000000002E+03 0.29930197777971E+03 0.29972981706808E+03
 0.29815000000003E+03 0.29815000000003E+03 0.29902756373402E+03 0.29971779428393E+03 0.29815000000003E+03
 0.29815000000003E+03 0.29930197777971E+03 0.29972981706808E+03 0.29815000000003E+03 0.29815000000003E+03
 0.29902756373402E+03 0.29971779428393E+03 0.29815000000003E+03 0.29815000000003E+03 0.30245710997082E+03
 0.29815465390899E+03 0.61107628125714E+03 0.60850743539634E+03 0.28537603758929E+03 0.58856223875179E+03
 0.30175932097455E+03 0.39030121913924E+03 0.19136442121217E+03 0.38857942960990E+03 0.50492402214217E+03
 0.29370806305507E+03 0.18670133201901E+03 0.29251480160050E+03 0.50038911356531E+03 0.39030121913924E+03
 0.19136442121217E+03 0.38857942960990E+03 0.50492402214217E+03 0.29370806305507E+03 0.18670133201901E+03
 0.29251480160050E+03 0.50038911356531E+03 0.52074285045566E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.35905425176569E+03 0.12945637639231E+01
 0.12945637639231E+01 0.20067951429979E-01 0.13088790643743E+01 0.29816409618011E+03 0.30463085704040E+03
 0.29898805177084E+03 0.29897274790207E+03 0.22999999996183E+00 0.00000000000000E+00 0.22916484283866E+00
 0.00000000000000E+00 0.17274492088467E+02 0.99984803441700E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815464332375E+03 0.30246773577130E+03
 0.29815225206470E+03 0.29834139940524E+03 0.29815000000003E+03 0.29815000000003E+03 0.29815226882927E+03
 0.29834140985187E+03 0.29815000000003E+03 0.29815000000003E+03 0.29815225206470E+03 0.29834139940524E+03
 0.29815000000003E+03 0.29815000000003E+03 0.29815226882927E+03 0.29834140985187E+03 0.29815000000003E+03
 0.29815000000003E+03 0.29822932796246E+03 0.29815000000002E+03 0.65666230634429E+00 0.65863737377957E+00
 0.50719449325190E+00 0.38318012324669E+02 0.37808281858951E+02 0.76989173548104E+00 -.31162996838826E+00
 0.77496750262641E+00 0.53658862502535E+02 0.77500695351114E+00 -.30778959242841E+00 0.78007356817952E+00
 0.53662603424312E+02 0.76989173548110E+00 -.31162996838826E+00 0.77496750262647E+00 0.53658862502535E+02
 0.77500695351114E+00 -.30778959242830E+00 0.78007356817952E+00 0.53662603424313E+02 0.11608921932060E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31115713342150E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.22857011576279E+00 0.00000000000000E+00 0.00000000000000E+00 0.22857011576279E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21335724441398E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21335724441398E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19756617391121E+00 0.23330585737539E+00 0.29816409618011E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     60.01376707
 0.16833253039436E+01 0.29822616104709E+03 0.35883567482772E+03 0.32482716542551E+03 0.32210420906972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22276534179596E+00 0.00000000000000E+00 0.96082097977556E+01
 0.99983942127347E-03 0.12591788600910E+00 0.80000000000000E+04 0.30000000000000E+04 0.63533468147820E+02
 0.23825050555432E+02 0.30060865479955E+03 0.29815000000004E+03 0.30049976589151E+03 0.30180048547980E+03
 0.29815000000004E+03 0.29815000000004E+03 0.29992034654103E+03 0.30177325208796E+03 0.29815000000004E+03
 0.29815000000004E+03 0.30049976589151E+03 0.30180048547980E+03 0.29815000000004E+03 0.29815000000004E+03
 0.29992034654103E+03 0.30177325208796E+03 0.29815000000004E+03 0.29815000000004E+03 0.30821840124306E+03
 0.29816339874008E+03 0.65246607950799E+03 0.64671869763445E+03 0.30379622552248E+03 0.82466238537206E+03
 0.51934717872197E+03 0.44497279803094E+03 0.25681291489506E+03 0.44054897014907E+03 0.73949818741873E+03
 0.33405754653808E+03 0.25134947345381E+03 0.33104606464674E+03 0.73431194386544E+03 0.44497279803094E+03
 0.25681291489506E+03 0.44054897014907E+03 0.73949818741873E+03 0.33405754653808E+03 0.25134947345381E+03
 0.33104606464674E+03 0.73431194386544E+03 0.68089200738064E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.38509600208103E+03 0.12946337379396E+01
 0.12946337379396E+01 0.60074317819557E-01 0.11167748694518E+01 0.29815749738184E+03 0.30944307493528E+03
 0.30104077533599E+03 0.30096174943162E+03 0.22999999999442E+00 0.00000000000000E+00 0.22844916410555E+00
 0.00000000000000E+00 0.93801775344197E+01 0.99979224864759E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29816335195175E+03 0.30825615081387E+03
 0.29815580836944E+03 0.29855447233216E+03 0.29815000000004E+03 0.29815000000004E+03 0.29815581564256E+03
 0.29855452905716E+03 0.29815000000004E+03 0.29815000000004E+03 0.29815580836944E+03 0.29855447233216E+03
 0.29815000000004E+03 0.29815000000004E+03 0.29815581564256E+03 0.29855452905716E+03 0.29815000000004E+03
 0.29815000000004E+03 0.29837718157521E+03 0.29815000000004E+03 0.12503546285052E+01 0.12481842817119E+01
 0.15360364326159E+00 0.75201049156011E+02 0.75046677494533E+02 0.12713810805969E+01 -.84782510321371E+00
 0.12718289972930E+01 0.80021677922519E+02 0.12692071891718E+01 -.83181959390393E+00 0.12696526925054E+01
 0.80037201923154E+02 0.12713810805969E+01 -.84782510321378E+00 0.12718289972930E+01 0.80021677922519E+02
 0.12692071891718E+01 -.83181959390393E+00 0.12696526925054E+01 0.80037201923154E+02 0.24765430450820E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32007641946037E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17016957417655E+00 0.00000000000000E+00 0.00000000000000E+00 0.17016957417655E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20013113259971E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20013113259971E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19730750268864E+00 0.23298739249588E+00 0.29815749738184E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     70.02540029
 0.13009413307971E+01 0.29831543309028E+03 0.37332449192466E+03 0.34079703031738E+03 0.33662036959724E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22051204137112E+00 0.00000000000000E+00 0.29907834829052E+01
 0.99947494277044E-03 0.15985991273355E+00 0.80000000000000E+04 0.30000000000000E+04 0.50043815633344E+02
 0.18766430862504E+02 0.30148248894733E+03 0.29815000000005E+03 0.30143394532727E+03 0.30375916937957E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30063376717069E+03 0.30372279404630E+03 0.29815000000005E+03
 0.29815000000005E+03 0.30143394532727E+03 0.30375916937957E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30063376717069E+03 0.30372279404630E+03 0.29815000000005E+03 0.29815000000005E+03 0.31327496735457E+03
 0.29817572554757E+03 0.70702333976849E+03 0.69862868651784E+03 0.33525157233693E+03 0.96410747254167E+03
 0.62717964234306E+03 0.49090306797963E+03 0.32550669310201E+03 0.48408638010342E+03 0.89972681647092E+03
 0.37265595052799E+03 0.32006305037367E+03 0.36802677487278E+03 0.89464925243818E+03 0.49090306797963E+03
 0.32550669310202E+03 0.48408638010342E+03 0.89972681647092E+03 0.37265595052799E+03 0.32006305037367E+03
 0.36802677487278E+03 0.89464925243818E+03 0.81797889825513E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39711162426173E+03 0.12946824777810E+01
 0.12946824777810E+01 0.10012085071892E+00 0.95352797038793E+00 0.29815323765288E+03 0.31312512988905E+03
 0.30360771787784E+03 0.30343899026061E+03 0.22999999999495E+00 0.00000000000000E+00 0.22779586898941E+00
 0.00000000000000E+00 0.41060341825490E+01 0.99975447789507E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29817565219510E+03 0.31331429804505E+03
 0.29816042565057E+03 0.29877046769519E+03 0.29815000000005E+03 0.29815000000005E+03 0.29816039814237E+03
 0.29877059823252E+03 0.29815000000005E+03 0.29815000000005E+03 0.29816042565057E+03 0.29877046769519E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29816039814237E+03 0.29877059823252E+03 0.29815000000005E+03
 0.29815000000005E+03 0.29855116500410E+03 0.29815000000005E+03 0.20093741175908E+01 0.19977509676068E+01
 -.15705126777306E+00 0.10560185576600E+03 0.10575969229011E+03 0.19164482429899E+01 -.12463392532495E+01
 0.19135811127625E+01 0.99481302381314E+02 0.19063878521104E+01 -.12180825715157E+01 0.19035345564500E+01
 0.99508494613138E+02 0.19164482429898E+01 -.12463392532495E+01 0.19135811127625E+01 0.99481302381314E+02
 0.19063878521104E+01 -.12180825715156E+01 0.19035345564500E+01 0.99508494613138E+02 0.37062008492877E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32657417989569E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12378316291063E+00 0.00000000000000E+00 0.00000000000000E+00 0.12378316291063E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19550320153666E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19550320153666E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19713449646200E+00 0.22896413814003E+00 0.30311477480825E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     80.01628852
 0.99962813443405E+00 0.29845567783913E+03 0.38353030144846E+03 0.35518263901970E+03 0.35026201383626E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21859000624445E+00 0.00000000000000E+00 -.15511911328638E+01
 0.99896050817505E-03 0.18762700251794E+00 0.80000000000000E+04 0.30000000000000E+04 0.42637786100297E+02
 0.15989169787611E+02 0.30239631283311E+03 0.29815000000007E+03 0.30230172185711E+03 0.30562447974707E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30131865671221E+03 0.30558283779299E+03 0.29815000000005E+03
 0.29815000000005E+03 0.30230172185711E+03 0.30562447974707E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30131865671221E+03 0.30558283779299E+03 0.29815000000005E+03 0.29815000000005E+03 0.31781216292874E+03
 0.29819135754235E+03 0.80825252937086E+03 0.79702711357793E+03 0.35686129702110E+03 0.10536130585908E+04
 0.69496745508462E+03 0.53483706717209E+03 0.38266558855381E+03 0.52568152613314E+03 0.10175007149003E+04
 0.41271821439435E+03 0.37776629778637E+03 0.40648491207623E+03 0.10130182943629E+04 0.53483706717209E+03
 0.38266558855381E+03 0.52568152613314E+03 0.10175007149003E+04 0.41271821439435E+03 0.37776629778636E+03
 0.40648491207623E+03 0.10130182943629E+04 0.94607730688159E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40769360914906E+03 0.18128181993578E+01
 0.18128181993578E+01 0.14008440364225E+00 0.84067153857358E+00 0.29815076038052E+03 0.31621380437407E+03
 0.30609041305049E+03 0.30582968340284E+03 0.22999999999215E+00 0.00000000000000E+00 0.22715054253900E+00
 0.00000000000000E+00 0.72594594038769E+00 0.99972942351625E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29819126093290E+03 0.31784536875748E+03
 0.29816607689448E+03 0.29899222331716E+03 0.29815000000005E+03 0.29815000000005E+03 0.29816599700592E+03
 0.29899244415718E+03 0.29815000000005E+03 0.29815000000005E+03 0.29816607689448E+03 0.29899222331716E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29816599700592E+03 0.29899244415718E+03 0.29815000000005E+03
 0.29815000000005E+03 0.29873856885600E+03 0.29815000000007E+03 0.28250293470162E+01 0.28006799692179E+01
 -.50296607492693E+00 0.13189372561483E+03 0.13239920652013E+03 0.25912466777831E+01 -.16335231158097E+01
 0.25835907974182E+01 0.11707896010959E+03 0.25725717759676E+01 -.15932888077267E+01 0.25649663732058E+01
 0.11711740820116E+03 0.25912466777831E+01 -.16335231158097E+01 0.25835907974182E+01 0.11707896010959E+03
 0.25725717759676E+01 -.15932888077266E+01 0.25649663732058E+01 0.11711740820116E+03 0.48200521149021E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33164726092739E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.84413858741774E-01 0.00000000000000E+00 0.00000000000000E+00 0.84413858741774E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19308977684800E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19308977684800E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19702353977733E+00 0.22584464002728E+00 0.30711836061440E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     90.03041373
 0.76274116523538E+00 0.29866567715926E+03 0.39184439546079E+03 0.36815398073664E+03 0.36304718495497E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21672635234556E+00 0.00000000000000E+00 -.45973920768431E+01
 0.99822810225682E-03 0.21391631900272E+00 0.80000000000000E+04 0.30000000000000E+04 0.37397801333231E+02
 0.14024175499962E+02 0.30337162336853E+03 0.29815000000008E+03 0.30315485233197E+03 0.30745676170070E+03
 0.29815000000006E+03 0.29815000000006E+03 0.30201243218681E+03 0.30741218599511E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30315485233197E+03 0.30745676170070E+03 0.29815000000006E+03 0.29815000000006E+03
 0.30201243218681E+03 0.30741218599511E+03 0.29815000000006E+03 0.29815000000006E+03 0.32208352913570E+03
 0.29821707912371E+03 0.91023891915385E+03 0.89602999757643E+03 0.38544138199577E+03 0.11321163943797E+04
 0.74474780547396E+03 0.57963847527985E+03 0.44164446351451E+03 0.56816827230692E+03 0.11229509521040E+04
 0.45503666369767E+03 0.43726698183718E+03 0.44720463972441E+03 0.11190179640516E+04 0.57963847527985E+03
 0.44164446351451E+03 0.56816827230692E+03 0.11229509521040E+04 0.45503666369767E+03 0.43726698183718E+03
 0.44720463972441E+03 0.11190179640516E+04 0.10678095434570E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41789143115721E+03 0.16284567708948E+01
 0.16284567708948E+01 0.18014090445811E+00 0.74476971969154E+00 0.29815000000000E+03 0.31895212164240E+03
 0.30862359477267E+03 0.30827291621449E+03 0.22999999999287E+00 0.00000000000000E+00 0.22648147518172E+00
 0.00000000000000E+00 -.13201984609090E+01 0.99971177786470E-03 0.12639706359073E-01 0.80000000000000E+04
 0.30000000000000E+04 0.63292609596565E+03 0.23734728598712E+03 0.29821695676679E+03 0.32211162843552E+03
 0.29817452217608E+03 0.29922328061996E+03 0.29815000000006E+03 0.29815000000006E+03 0.29817433131379E+03
 0.29922360382411E+03 0.29815000000006E+03 0.29815000000006E+03 0.29817452217608E+03 0.29922328061996E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29817433131379E+03 0.29922360382411E+03 0.29815000000006E+03
 0.29815000000006E+03 0.29893926617991E+03 0.29815000000008E+03 0.50023949956912E+01 0.49532082347809E+01
 0.88557341041289E+00 0.15750421603952E+03 0.15661421476205E+03 0.41238432804956E+01 -.26566656716515E+00
 0.41096080099552E+01 0.13455269325841E+03 0.40759113523251E+01 -.21418318798423E+00 0.40618161961591E+01
 0.13460156689563E+03 0.41238432804956E+01 -.26566656716515E+00 0.41096080099552E+01 0.13455269325841E+03
 0.40759113523250E+01 -.21418318798423E+00 0.40618161961591E+01 0.13460156689563E+03 0.59925683152513E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33611259375561E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.48749509886666E-01 0.00000000000000E+00 0.00000000000000E+00 0.48749509886666E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19190105747835E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19190105747835E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19688155535634E+00 0.22462943555509E+00 0.30855106087871E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    100.20027116
 0.57285432202979E+00 0.29897634812564E+03 0.39899715578485E+03 0.37989803846469E+03 0.37503902535166E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21482290353715E+00 0.00000000000000E+00 -.63591305362637E+01
 0.99717348898309E-03 0.24037893438519E+00 0.80000000000000E+04 0.30000000000000E+04 0.33280786523417E+02
 0.12480294946281E+02 0.30440644315939E+03 0.29815000000008E+03 0.30402252496847E+03 0.30929134621402E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30273732934999E+03 0.30924513783264E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30402252496847E+03 0.30929134621402E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30273732934999E+03 0.30924513783264E+03 0.29815000000006E+03 0.29815000000006E+03 0.32624969800248E+03
 0.29827181327728E+03 0.10133464037029E+04 0.99616986111655E+03 0.41874354451530E+03 0.12019002731067E+04
 0.78106301086881E+03 0.62595770874761E+03 0.50264290004486E+03 0.61222316468258E+03 0.12195045184694E+04
 0.49990784356513E+03 0.49873380008509E+03 0.49052210672999E+03 0.12160535983963E+04 0.62595770874761E+03
 0.50264290004486E+03 0.61222316468258E+03 0.12195045184694E+04 0.49990784356513E+03 0.49873380008509E+03
 0.49052210672999E+03 0.12160535983963E+04 0.11834153646315E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42775139759674E+03 0.14752816372293E+01
 0.14752816372293E+01 0.22082033421023E+00 0.65855959949135E+00 0.29815030845591E+03 0.32141643803397E+03
 0.31120168271621E+03 0.31076934906272E+03 0.22999999999903E+00 0.00000000000000E+00 0.22576384408417E+00
 0.00000000000000E+00 -.22176541766116E+01 0.99970188578698E-03 0.34802407439528E-01 0.80000000000000E+04
 0.30000000000000E+04 0.22986915528475E+03 0.86200933231781E+02 0.29827154737069E+03 0.32627593154199E+03
 0.29819064095832E+03 0.29947286833674E+03 0.29815000000005E+03 0.29815000000005E+03 0.29819016965101E+03
 0.29947330121007E+03 0.29815000000006E+03 0.29815000000006E+03 0.29819064095832E+03 0.29947286833674E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29819016965101E+03 0.29947330121007E+03 0.29815000000006E+03
 0.29815000000006E+03 0.29915865557455E+03 0.29815000000008E+03 0.90235260111431E+01 0.89149759248382E+01
 0.46017678438708E+01 0.18334463695483E+03 0.17871986027174E+03 0.68459083524086E+01 0.34131143307797E+01
 0.68185075189440E+01 0.15263044157304E+03 0.67437399761695E+01 0.34744579753281E+01 0.67167442070185E+01
 0.15268828823275E+03 0.68459083524086E+01 0.34131143307797E+01 0.68185075189440E+01 0.15263044157304E+03
 0.67437399761695E+01 0.34744579753281E+01 0.67167442070185E+01 0.15268828823275E+03 0.72724802369534E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34014266488156E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17576819846996E-01 0.22402457774831E-02 0.00000000000000E+00 0.17576819846996E-01 0.22402457774831E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19155589027294E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19155589027294E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19684621654812E+00 0.22360888123311E+00 0.30990091129320E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    110.02860170
 0.42235068828560E+00 0.29940077411198E+03 0.40456577985794E+03 0.38976027567115E+03 0.38550250594773E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21300992303028E+00 0.00000000000000E+00 -.74407405852886E+01
 0.99574928093580E-03 0.26550130946526E+00 0.80000000000000E+04 0.30000000000000E+04 0.30131678130373E+02
 0.11299379298890E+02 0.30544477765521E+03 0.29815000000008E+03 0.30486857412533E+03 0.31103763468578E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30346052270239E+03 0.31099068392844E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30486857412533E+03 0.31103763468578E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30346052270239E+03 0.31099068392844E+03 0.29815000000006E+03 0.29815000000006E+03 0.33007819357131E+03
 0.29834784887736E+03 0.11102721275947E+04 0.10904856789304E+04 0.45061857305385E+03 0.12544768428958E+04
 0.80160517697670E+03 0.66998491214000E+03 0.55879133649045E+03 0.65422204925749E+03 0.13010825247436E+04
 0.54333821549499E+03 0.55527595603327E+03 0.53260391987941E+03 0.12980308655274E+04 0.66998491214000E+03
 0.55879133649045E+03 0.65422204925749E+03 0.13010825247436E+04 0.54333821549499E+03 0.55527595603327E+03
 0.53260391987941E+03 0.12980308655274E+04 0.12840615980409E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43501077680464E+03 0.13540256204333E+01
 0.13540256204333E+01 0.26013365635823E+00 0.58381518867822E+00 0.29815272211659E+03 0.32344195547270E+03
 0.31359912977715E+03 0.31310549829674E+03 0.23000000000000E+00 0.00000000000000E+00 0.22504606763828E+00
 0.00000000000000E+00 -.25969817125368E+01 0.99969004891681E-03 0.54522026515320E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14672968910560E+03 0.55023633414599E+02 0.29834740638095E+03 0.33010466397206E+03
 0.29821242783539E+03 0.29972256629232E+03 0.29815000000005E+03 0.29815000000005E+03 0.29821157468299E+03
 0.29972310578603E+03 0.29815000000006E+03 0.29815000000006E+03 0.29821242783539E+03 0.29972256629232E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29821157468299E+03 0.29972310578603E+03 0.29815000000006E+03
 0.29815000000006E+03 0.29937860949495E+03 0.29815000000008E+03 0.13585398964211E+02 0.13381291046046E+02
 0.89703657238746E+01 0.20538416473021E+03 0.19636894717772E+03 0.99302885476338E+01 0.77059022179628E+01
 0.98841544144870E+01 0.16867752482089E+03 0.97722050261730E+01 0.77750657697928E+01 0.97269047253904E+01
 0.16874232227069E+03 0.99302885476338E+01 0.77059022179629E+01 0.98841544144870E+01 0.16867752482089E+03
 0.97722050261730E+01 0.77750657697918E+01 0.97269047253904E+01 0.16874232227068E+03 0.84563175466229E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34225051699235E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24785454145489E-02 0.19549491975164E-01 0.00000000000000E+00 0.24785454145489E-02 0.19549491975164E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19178051527361E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19178051527361E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19682950220560E+00 0.22098610530437E+00 0.31355117307115E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    120.01392776
 0.30366811907099E+00 0.29997878292437E+03 0.40931195676952E+03 0.39824495701831E+03 0.39474864944939E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21115001153412E+00 0.00000000000000E+00 -.87410721293364E+01
 0.11664336815332E-02 0.29123045078684E+00 0.68585125126741E+04 0.25719421922528E+04 0.27469654970439E+02
 0.10301120613915E+02 0.30652646129944E+03 0.29815000000009E+03 0.30573500745750E+03 0.31277809274303E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30421556958476E+03 0.31273092232172E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30573500745750E+03 0.31277809274303E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30421556958476E+03 0.31273092232172E+03 0.29815000000006E+03 0.29815000000006E+03 0.33378121922015E+03
 0.29844602471600E+03 0.12030194079468E+04 0.11810406847972E+04 0.48227056359017E+03 0.12985504942755E+04
 0.81386857786740E+03 0.71317989739158E+03 0.61307004225635E+03 0.69561799892038E+03 0.13751701518817E+04
 0.58658634921886E+03 0.60989402933177E+03 0.57473903041166E+03 0.13724585479579E+04 0.71317989739158E+03
 0.61307004225635E+03 0.69561799892038E+03 0.13751701518817E+04 0.58658634921886E+03 0.60989402933177E+03
 0.57473903041166E+03 0.13724585479579E+04 0.13754108314376E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44163447500258E+03 0.12947688982780E+01
 0.12947688982780E+01 0.30007496061492E+00 0.52942531260739E+00 0.29815720671937E+03 0.32516724400660E+03
 0.31563404571704E+03 0.31509255931082E+03 0.23000000000000E+00 0.00000000000000E+00 0.22429415820596E+00
 0.00000000000000E+00 -.33111707025940E+01 0.99966796369007E-03 0.73663279593588E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10860227842335E+03 0.40725854408757E+02 0.29844542439032E+03 0.33380887099546E+03
 0.29824021014079E+03 0.29997966670993E+03 0.29815000000005E+03 0.29815000000005E+03 0.29823888321787E+03
 0.29998031173797E+03 0.29815000000006E+03 0.29815000000006E+03 0.29824021014079E+03 0.29997966670993E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29823888321787E+03 0.29998031173797E+03 0.29815000000006E+03
 0.29815000000006E+03 0.29960491264870E+03 0.29815000000009E+03 0.18554424871478E+02 0.18210061735432E+02
 0.13836311692769E+02 0.22523532191776E+03 0.21132982866653E+03 0.13307892450216E+02 0.12473808246925E+02
 0.13236482312314E+02 0.18349372926703E+03 0.13092354103860E+02 0.12549463230670E+02 0.13022387937381E+02
 0.18356414749418E+03 0.13307892450216E+02 0.12473808246925E+02 0.13236482312314E+02 0.18349372926703E+03
 0.13092354103860E+02 0.12549463230670E+02 0.13022387937381E+02 0.18356414749418E+03 0.95443711551734E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34426856288084E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.50166281423910E-01 0.00000000000000E+00 0.00000000000000E+00 0.50166281423910E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19213335191964E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19213335191964E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19680243734184E+00 0.21342950089812E+00 0.32460571771992E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    130.10159768
 0.21765076967598E+00 0.30070039585369E+03 0.41345641221566E+03 0.40527593430007E+03 0.40250632971972E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20922169197498E+00 0.00000000000000E+00 -.10552987406189E+02
 0.16274120047701E-02 0.31787363257750E+00 0.49157803780182E+04 0.18434176417568E+04 0.25167233705833E+02
 0.94377126396874E+01 0.30763253458340E+03 0.29815000000009E+03 0.30661465763052E+03 0.31450162054564E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30499475109755E+03 0.31445456328375E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30661465763052E+03 0.31450162054564E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30499475109755E+03 0.31445456328375E+03 0.29815000000006E+03 0.29815000000006E+03 0.33735735747457E+03
 0.29856370385351E+03 0.12886148174800E+04 0.12649438849916E+04 0.51330957999707E+03 0.13363861092742E+04
 0.82050998137713E+03 0.75458037692214E+03 0.66536532826466E+03 0.73549395388806E+03 0.14419892361002E+04
 0.62847708421871E+03 0.66247460009459E+03 0.61579600298518E+03 0.14395599394710E+04 0.75458037692214E+03
 0.66536532826466E+03 0.73549395388806E+03 0.14419892361002E+04 0.62847708421871E+03 0.66247460009459E+03
 0.61579600298518E+03 0.14395599394710E+04 0.14561339716748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44867765300568E+03 0.12947822466584E+01
 0.12947822466584E+01 0.34042564028087E+00 0.49356995118909E+00 0.29816458687893E+03 0.32665276604605E+03
 0.31727882657873E+03 0.31669621428609E+03 0.23000000000000E+00 0.00000000000000E+00 0.22349803577971E+00
 0.00000000000000E+00 -.46557885573114E+01 0.99962994931603E-03 0.92813780332087E-01 0.80000000000000E+04
 0.30000000000000E+04 0.86194097162900E+02 0.32322786436088E+02 0.29856297781829E+03 0.33738680226427E+03
 0.29827323790862E+03 0.30024196200221E+03 0.29815000000005E+03 0.29815000000005E+03 0.29827135657399E+03
 0.30024271092028E+03 0.29815000000006E+03 0.29815000000006E+03 0.29827323790862E+03 0.30024196200221E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29827135657399E+03 0.30024271092028E+03 0.29815000000006E+03
 0.29815000000006E+03 0.29983396747555E+03 0.29815000000009E+03 0.23725563025959E+02 0.23195373032179E+02
 0.19028918047887E+02 0.24314253883282E+03 0.22401847619469E+03 0.16862895196861E+02 0.17542393597225E+02
 0.16761036486695E+02 0.19731560548476E+03 0.16588719958621E+02 0.17623571665420E+02 0.16489092617365E+02
 0.19739067701157E+03 0.16862895196861E+02 0.17542393597225E+02 0.16761036486695E+02 0.19731560548475E+03
 0.16588719958621E+02 0.17623571665417E+02 0.16489092617365E+02 0.19739067701157E+03 0.10528782500500E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34606352217390E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.80248450314010E-01 0.00000000000000E+00 0.00000000000000E+00 0.80248450314010E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19250085016460E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19250085016460E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19675498189426E+00 0.21203803629063E+00 0.32665276604605E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    140.24706066
 0.15972884642014E+00 0.30148310781644E+03 0.41708935980814E+03 0.41093414205161E+03 0.40874424926715E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20722279450576E+00 0.00000000000000E+00 -.12621321413999E+02
 0.22175488675645E-02 0.34548227068661E+00 0.36075867896369E+04 0.13528450461138E+04 0.23156036297032E+02
 0.86835136113868E+01 0.30873964277674E+03 0.29815000000009E+03 0.30749530120998E+03 0.31619545801896E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30578454058363E+03 0.31614869999666E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30749530120998E+03 0.31619545801896E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30578454058363E+03 0.31614869999666E+03 0.29815000000006E+03 0.29815000000006E+03 0.34078946820591E+03
 0.29870051059912E+03 0.13656746769973E+04 0.13405657680119E+04 0.54314193924226E+03 0.13680366169311E+04
 0.82217896799267E+03 0.79344045051257E+03 0.71523333759167E+03 0.77291843008904E+03 0.15039272289442E+04
 0.66802208078015E+03 0.71258241739252E+03 0.65460970260232E+03 0.15017335386021E+04 0.79344045051257E+03
 0.71523333759167E+03 0.77291843008904E+03 0.15039272289442E+04 0.66802208078016E+03 0.71258241739252E+03
 0.65460970260232E+03 0.15017335386021E+04 0.15270580319631E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45463618518201E+03 0.12947974844841E+01
 0.12947974844841E+01 0.38100749220319E+00 0.45904639302369E+00 0.29817634073291E+03 0.32792861091694E+03
 0.31882349604211E+03 0.31821169628167E+03 0.23000000000000E+00 0.00000000000000E+00 0.22265529241138E+00
 0.00000000000000E+00 -.63677813893526E+01 0.99957364901937E-03 0.11219674884131E+00 0.80000000000000E+04
 0.30000000000000E+04 0.71303313889381E+02 0.26738742708518E+02 0.29869967623431E+03 0.34082096361645E+03
 0.29831109249676E+03 0.30050446453019E+03 0.29815000000005E+03 0.29815000000005E+03 0.29830859519919E+03
 0.30050531229655E+03 0.29815000000006E+03 0.29815000000006E+03 0.29831109249676E+03 0.30050446453019E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29830859519919E+03 0.30050531229655E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30006247697224E+03 0.29815000000009E+03 0.29215110520459E+02 0.28452422011356E+02
 0.24628601077128E+02 0.25950337629517E+03 0.23475163221266E+03 0.20639808389798E+02 0.22993666687504E+02
 0.20504064710111E+02 0.20990730973795E+03 0.20307231764013E+02 0.23079246731245E+02 0.20174670180620E+02
 0.20998595761443E+03 0.20639808389798E+02 0.22993666687504E+02 0.20504064710111E+02 0.20990730973795E+03
 0.20307231764013E+02 0.23079246731244E+02 0.20174670180620E+02 0.20998595761442E+03 0.11468706213715E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34764898919533E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10693402070633E+00 0.00000000000000E+00 0.00000000000000E+00 0.10693402070633E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19295067589312E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19295067589312E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19669614416811E+00 0.21114634706477E+00 0.32792861091694E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    151.33053532
 0.12193898758900E+00 0.30222758948608E+03 0.42046181216839E+03 0.41565602503097E+03 0.41388060594607E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20498221988861E+00 0.00000000000000E+00 -.14905788035482E+02
 0.29047788333592E-02 0.37649053953921E+00 0.27540823102007E+04 0.10327808663253E+04 0.21248873902094E+02
 0.79683277132853E+01 0.30993019745837E+03 0.29815000000009E+03 0.30844573339640E+03 0.31800196745980E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30664542697598E+03 0.31795568545952E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30844573339640E+03 0.31800196745980E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30664542697598E+03 0.31795568545952E+03 0.29815000000006E+03 0.29815000000006E+03 0.34434812001529E+03
 0.29887308187126E+03 0.14385761114467E+04 0.14114422682186E+04 0.57293406503297E+03 0.13941663727723E+04
 0.81836763741420E+03 0.83179624925174E+03 0.76552015322098E+03 0.80926505179418E+03 0.15642930029599E+04
 0.70713037831152E+03 0.76308983773051E+03 0.69250114110519E+03 0.15623143572157E+04 0.83179624925174E+03
 0.76552015322098E+03 0.80926505179418E+03 0.15642930029599E+04 0.70713037831152E+03 0.76308983773051E+03
 0.69250114110519E+03 0.15623143572157E+04 0.15919137879221E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45959485586567E+03 0.12948143151078E+01
 0.12948143151078E+01 0.42534139082869E+00 0.42315563480716E+00 0.29819616006435E+03 0.32910496336644E+03
 0.32038547384483E+03 0.31975507428957E+03 0.23000000000000E+00 0.00000000000000E+00 0.22168384527570E+00
 0.00000000000000E+00 -.83739494027889E+01 0.99948741562186E-03 0.13371345392254E+00 0.80000000000000E+04
 0.30000000000000E+04 0.59829432007900E+02 0.22436037002962E+02 0.29887213775661E+03 0.34438169529305E+03
 0.29835781740905E+03 0.30078879376876E+03 0.29815000000005E+03 0.29815000000005E+03 0.29835459241501E+03
 0.30078974051510E+03 0.29815000000006E+03 0.29815000000006E+03 0.29835781740905E+03 0.30078879376876E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29835459241501E+03 0.30078974051510E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30030948561524E+03 0.29815000000009E+03 0.35472905714691E+02 0.34400178789496E+02
 0.31134582712393E+02 0.27569251204297E+03 0.24440225641701E+03 0.25001746144319E+02 0.29296520527305E+02
 0.24828011311375E+02 0.22235222946731E+03 0.24608293744241E+02 0.29384647192465E+02 0.24438940230038E+02
 0.22243260124466E+03 0.25001746144319E+02 0.29296520527305E+02 0.24828011311375E+02 0.22235222946731E+03
 0.24608293744241E+02 0.29384647192463E+02 0.24438940230038E+02 0.22243260124465E+03 0.12440497027728E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34914591405893E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13150283214236E+00 0.00000000000000E+00 0.00000000000000E+00 0.13150283214236E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19347655106229E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19347655106229E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19662806478538E+00 0.21031464031401E+00 0.32910496336644E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    160.33514621
 0.10542337174376E+00 0.30267050998733E+03 0.42273731423100E+03 0.41851803178504E+03 0.41692529906085E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20313956241837E+00 0.00000000000000E+00 -.16658691051632E+02
 0.33598374221816E-02 0.40209692435393E+00 0.23810675918972E+04 0.89290034696144E+03 0.19895700552433E+02
 0.74608877071623E+01 0.31088618949645E+03 0.29815000000009E+03 0.30921127978615E+03 0.31944504801439E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30734447213944E+03 0.31939927227076E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30921127978615E+03 0.31944504801439E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30734447213944E+03 0.31939927227076E+03 0.29815000000006E+03 0.29815000000006E+03 0.34709704601638E+03
 0.29903482218327E+03 0.14888770352781E+04 0.14593461306097E+04 0.59395058786925E+03 0.14078318377083E+04
 0.81091149689967E+03 0.85927004947578E+03 0.80216518982496E+03 0.83453121786732E+03 0.16060748653169E+04
 0.73512657408667E+03 0.79988982402877E+03 0.71892692490329E+03 0.16042455773737E+04 0.85927004947578E+03
 0.80216518982496E+03 0.83453121786732E+03 0.16060748653169E+04 0.73512657408667E+03 0.79988982402877E+03
 0.71892692490329E+03 0.16042455773737E+04 0.16341012923558E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46257084048564E+03 0.12948272298397E+01
 0.12948272298397E+01 0.46135983441379E+00 0.39545508068611E+00 0.29821961976493E+03 0.32991180114160E+03
 0.32155657837932E+03 0.32092056569735E+03 0.23000000000000E+00 0.00000000000000E+00 0.22085413997786E+00
 0.00000000000000E+00 -.99803388075104E+01 0.99939293885198E-03 0.15157633219186E+00 0.80000000000000E+04
 0.30000000000000E+04 0.52778688363258E+02 0.19792008136222E+02 0.29903375853788E+03 0.34713211688721E+03
 0.29840104335260E+03 0.30102077393973E+03 0.29815000000005E+03 0.29815000000005E+03 0.29839718335320E+03
 0.30102179088855E+03 0.29815000000006E+03 0.29815000000006E+03 0.29840104335260E+03 0.30102077393973E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29839718335320E+03 0.30102179088855E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30051164372045E+03 0.29815000000009E+03 0.40689281307439E+02 0.39314870573425E+02
 0.36636722297316E+02 0.28759954737728E+03 0.25077964146848E+03 0.28699404379773E+02 0.34611007021052E+02
 0.28495612132449E+02 0.23153129514700E+03 0.28259505632606E+02 0.34699599816235E+02 0.28061190106425E+02
 0.23161155259719E+03 0.28699404379773E+02 0.34611007021052E+02 0.28495612132449E+02 0.23153129514700E+03
 0.28259505632606E+02 0.34699599816235E+02 0.28061190106425E+02 0.23161155259719E+03 0.13185389865545E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35020466770519E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14817660694052E+00 0.00000000000000E+00 0.00000000000000E+00 0.14817660694052E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19429836684733E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19429836684733E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19657395601071E+00 0.20973923114098E+00 0.32991180114160E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    171.36348569
 0.96529763304204E-01 0.30303562502569E+03 0.42495042853382E+03 0.42102762615851E+03 0.41951973694293E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20089003355514E+00 0.00000000000000E+00 -.18483629681748E+02
 0.36693872065120E-02 0.43356409172871E+00 0.21802005484192E+04 0.81757520565722E+03 0.18451712567114E+02
 0.69193922126677E+01 0.31200249699777E+03 0.29815000000009E+03 0.31011068675514E+03 0.32113121958340E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30816757260247E+03 0.32108607188643E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31011068675514E+03 0.32113121958340E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30816757260247E+03 0.32108607188643E+03 0.29815000000006E+03 0.29815000000006E+03 0.35023176549186E+03
 0.29925212895340E+03 0.15399249275334E+04 0.15067827140910E+04 0.61503880232901E+03 0.14154765594232E+04
 0.79736256308253E+03 0.88802865320639E+03 0.84100646981884E+03 0.86020574218872E+03 0.16475007158942E+04
 0.76439195758691E+03 0.83889922896176E+03 0.74581542277268E+03 0.16458326003664E+04 0.88802865320639E+03
 0.84100646981884E+03 0.86020574218872E+03 0.16475007158942E+04 0.76439195758691E+03 0.83889922896176E+03
 0.74581542277268E+03 0.16458326003664E+04 0.16733723441480E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46513637790222E+03 0.12948406756354E+01
 0.12948406756354E+01 0.50547319232198E+00 0.36336089385528E+00 0.29825962827204E+03 0.33073634683242E+03
 0.32286916717536E+03 0.32223672447902E+03 0.23000000000000E+00 0.00000000000000E+00 0.21978921517594E+00
 0.00000000000000E+00 -.11688805741195E+02 0.99924202417401E-03 0.17400034470378E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45976920411389E+02 0.17241345154271E+02 0.29925093910108E+03 0.35026806907939E+03
 0.29845726843271E+03 0.30129702206075E+03 0.29815000000005E+03 0.29815000000005E+03 0.29845262247742E+03
 0.30129811424247E+03 0.29815000000006E+03 0.29815000000006E+03 0.29845726843271E+03 0.30129702206075E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29845262247742E+03 0.30129811424247E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30075139602755E+03 0.29815000000009E+03 0.47135996476346E+02 0.45349328811567E+02
 0.43509818666315E+02 0.30072439995970E+03 0.25699703220005E+03 0.33292863988378E+02 0.41242987203460E+02
 0.33061842338699E+02 0.24167678030792E+03 0.32800152842688E+02 0.41331183190131E+02 0.32575980481424E+02
 0.24175602558373E+03 0.33292863988378E+02 0.41242987203460E+02 0.33061842338699E+02 0.24167678030792E+03
 0.32800152842688E+02 0.41331183190131E+02 0.32575980481424E+02 0.24175602558373E+03 0.14039330933752E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35128928412865E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16406443802017E+00 0.00000000000000E+00 0.00000000000000E+00 0.16406443802017E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19516982055639E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19516982055639E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19651665150131E+00 0.20915182041079E+00 0.33073634683242E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    180.00261596
 0.94847371248314E-01 0.30326502668346E+03 0.42627673709834E+03 0.42238762464314E+03 0.42087932271786E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19915640089223E+00 0.00000000000000E+00 -.19604540510735E+02
 0.37344728485321E-02 0.45800039838613E+00 0.21422032839641E+04 0.80332623148652E+03 0.17467233714621E+02
 0.65502126429828E+01 0.31284557002385E+03 0.29815000000010E+03 0.31079346339760E+03 0.32239943913949E+03
 0.29815000000005E+03 0.29815000000005E+03 0.30879410887771E+03 0.32235483621659E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31079346339760E+03 0.32239943913949E+03 0.29815000000005E+03 0.29815000000005E+03
 0.30879410887771E+03 0.32235483621659E+03 0.29815000000006E+03 0.29815000000006E+03 0.35251235894149E+03
 0.29944192565820E+03 0.15725181354899E+04 0.15363499471932E+04 0.62772148657881E+03 0.14147638723142E+04
 0.78390377830253E+03 0.90691358625434E+03 0.86647872295252E+03 0.87671998461484E+03 0.16721739862671E+04
 0.78359086391980E+03 0.86448992272717E+03 0.76311399081783E+03 0.16706182624295E+04 0.90691358625434E+03
 0.86647872295253E+03 0.87671998461484E+03 0.16721739862671E+04 0.78359086391980E+03 0.86448992272717E+03
 0.76311399081783E+03 0.16706182624295E+04 0.16952587766356E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46649649832328E+03 0.12948489344582E+01
 0.12948489344582E+01 0.54002971341560E+00 0.33965163693310E+00 0.29830242127246E+03 0.33126306938537E+03
 0.32379964399475E+03 0.32317728338276E+03 0.23000000000000E+00 0.00000000000000E+00 0.21891971975272E+00
 0.00000000000000E+00 -.12759872744454E+02 0.99908811187175E-03 0.19200109655238E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41666428700930E+02 0.15624910762849E+02 0.29944063194708E+03 0.35254925993646E+03
 0.29850523637291E+03 0.30151067643510E+03 0.29815000000005E+03 0.29815000000005E+03 0.29849996148772E+03
 0.30151181668729E+03 0.29815000000006E+03 0.29815000000006E+03 0.29850523637291E+03 0.30151067643510E+03
 0.29815000000005E+03 0.29815000000005E+03 0.29849996148772E+03 0.30151181668729E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30093689071077E+03 0.29815000000009E+03 0.52160363532735E+02 0.50012641195590E+02
 0.48959412290109E+02 0.30988030650549E+03 0.26067609715393E+03 0.36963627433724E+02 0.46480725831416E+02
 0.36721596714717E+02 0.24877960572488E+03 0.36434226603382E+02 0.46566990276041E+02 0.36200122370670E+02
 0.24885653197505E+03 0.36963627433724E+02 0.46480725831416E+02 0.36721596714717E+02 0.24877960572488E+03
 0.36434226603382E+02 0.46566990276041E+02 0.36200122370670E+02 0.24885653197505E+03 0.14659599051942E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35199378074602E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17322459140099E+00 0.00000000000000E+00 0.00000000000000E+00 0.17322459140099E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19598426422654E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19598426422654E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19648086728797E+00 0.20877902761665E+00 0.33126306938537E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    190.02718714
 0.96112461724098E-01 0.30352374277084E+03 0.42744002353676E+03 0.42347005727273E+03 0.42192146356795E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19719582886379E+00 0.00000000000000E+00 -.20636186254543E+02
 0.36853164388697E-02 0.48583585854179E+00 0.21707769557106E+04 0.81404135839149E+03 0.16466466728108E+02
 0.61749250230403E+01 0.31378933525171E+03 0.29815000000010E+03 0.31156211319504E+03 0.32380908244619E+03
 0.29815000000006E+03 0.29815000000006E+03 0.30950051169765E+03 0.32376516255845E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31156211319504E+03 0.32380908244619E+03 0.29815000000006E+03 0.29815000000006E+03
 0.30950051169765E+03 0.32376516255845E+03 0.29815000000006E+03 0.29815000000006E+03 0.35496245362012E+03
 0.29968399246809E+03 0.16033665807605E+04 0.15637493006973E+04 0.63854124597542E+03 0.14079765082635E+04
 0.76624255605820E+03 0.92522984328042E+03 0.89087231001683E+03 0.89254212073101E+03 0.16931028186198E+04
 0.80221447951878E+03 0.88900888480554E+03 0.77970574418261E+03 0.16916648687617E+04 0.92522984328042E+03
 0.89087231001683E+03 0.89254212073101E+03 0.16931028186198E+04 0.80221447951879E+03 0.88900888480554E+03
 0.77970574418261E+03 0.16916648687617E+04 0.17124854633081E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46755294833391E+03 0.12948565356928E+01
 0.12948565356928E+01 0.58012799810588E+00 0.31368381938914E+00 0.29836617650447E+03 0.33176384850034E+03
 0.32477964229343E+03 0.32417549341347E+03 0.23000000000000E+00 0.00000000000000E+00 0.21787301928824E+00
 0.00000000000000E+00 -.13765074575962E+02 0.99886471148304E-03 0.21339704948809E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37488803238803E+02 0.14058301214551E+02 0.29968255948997E+03 0.35499965887446E+03
 0.29856531872623E+03 0.30175557777415E+03 0.29815000000006E+03 0.29815000000006E+03 0.29855930823168E+03
 0.30175676071838E+03 0.29815000000006E+03 0.29815000000006E+03 0.29856531872623E+03 0.30175557777415E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29855930823168E+03 0.30175676071838E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30114968745646E+03 0.29815000000010E+03 0.57889757168667E+02 0.55283131957324E+02
 0.55250716267091E+02 0.31935723088941E+03 0.26383026104099E+03 0.41222206737687E+02 0.52521554764493E+02
 0.40981686780477E+02 0.25617089434892E+03 0.40654953151193E+02 0.52604818413194E+02 0.40423551985200E+02
 0.25624448287544E+03 0.41222206737687E+02 0.52521554764493E+02 0.40981686780477E+02 0.25617089434892E+03
 0.40654953151193E+02 0.52604818413194E+02 0.40423551985200E+02 0.25624448287544E+03 0.15325765859605E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35267336008454E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18134733813743E+00 0.00000000000000E+00 0.00000000000000E+00 0.18134733813743E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19700577493847E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19700577493847E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19644743790576E+00 0.20842635037801E+00 0.33176384850034E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    200.06412715
 0.98970350199335E-01 0.30378867599455E+03 0.42828235716915E+03 0.42417529609467E+03 0.42256944339268E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19530050877017E+00 0.00000000000000E+00 -.21353840798623E+02
 0.35788979732256E-02 0.51293953317623E+00 0.22353249687053E+04 0.83824686326449E+03 0.15596380240888E+02
 0.58486425903330E+01 0.31468797960407E+03 0.29815000000011E+03 0.31229839436271E+03 0.32514068684506E+03
 0.29815000000006E+03 0.29815000000006E+03 0.31017710880381E+03 0.32509745294997E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31229839436271E+03 0.32514068684506E+03 0.29815000000006E+03 0.29815000000006E+03
 0.31017710880381E+03 0.32509745294997E+03 0.29815000000006E+03 0.29815000000006E+03 0.35721360663264E+03
 0.29994602718012E+03 0.16281801086694E+04 0.15853109671383E+04 0.64574024982698E+03 0.13963142220877E+04
 0.74734527101163E+03 0.94030638614488E+03 0.91044568471691E+03 0.90541448550414E+03 0.17072530397105E+04
 0.81757031531325E+03 0.90869677439734E+03 0.79325932335307E+03 0.17059219223415E+04 0.94030638614488E+03
 0.91044568471691E+03 0.90541448550415E+03 0.17072530397105E+04 0.81757031531325E+03 0.90869677439735E+03
 0.79325932335306E+03 0.17059219223415E+04 0.17228732890076E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46822188559216E+03 0.12948618234832E+01
 0.12948618234832E+01 0.62027575817386E+00 0.28929551958458E+00 0.29844860587592E+03 0.33215290995401E+03
 0.32565257384701E+03 0.32507269009956E+03 0.23000000000000E+00 0.00000000000000E+00 0.21678981394214E+00
 0.00000000000000E+00 -.14476767216924E+02 0.99858181486582E-03 0.23531025345456E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33997668535701E+02 0.12749125700888E+02 0.29994447144326E+03 0.35725082184909E+03
 0.29862834181738E+03 0.30199378580790E+03 0.29815000000006E+03 0.29815000000006E+03 0.29862161244725E+03
 0.30199499920273E+03 0.29815000000006E+03 0.29815000000006E+03 0.29862834181738E+03 0.30199378580790E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29862161244725E+03 0.30199499920273E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30135611800820E+03 0.29815000000010E+03 0.63440323683614E+02 0.60350308005730E+02
 0.61451311184515E+02 0.32761400155069E+03 0.26585543381025E+03 0.45455367244438E+02 0.58458058773070E+02
 0.45240196648422E+02 0.26267483900606E+03 0.44856400779463E+02 0.58537178702325E+02 0.44651313759904E+02
 0.26274405222524E+03 0.45455367244438E+02 0.58458058773070E+02 0.45240196648421E+02 0.26267483900606E+03
 0.44856400779463E+02 0.58537178702325E+02 0.44651313759904E+02 0.26274405222524E+03 0.15932638727392E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35315716930506E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18682716694625E+00 0.00000000000000E+00 0.00000000000000E+00 0.18682716694625E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19800218945328E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19800218945328E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19642393720015E+00 0.20815584655759E+00 0.33215290995401E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    210.76190573
 0.10251238739329E+00 0.30408140693563E+03 0.42892443105515E+03 0.42465844557118E+03 0.42299026251154E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19336187505460E+00 0.00000000000000E+00 -.21868898560351E+02
 0.34552386436287E-02 0.54082934589761E+00 0.23153248805988E+04 0.86824683022456E+03 0.14792096731960E+02
 0.55470362744850E+01 0.31559803604211E+03 0.29815000000014E+03 0.31304791074687E+03 0.32647666640252E+03
 0.29815000000006E+03 0.29815000000006E+03 0.31086570316825E+03 0.32643415613646E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31304791074687E+03 0.32647666640252E+03 0.29815000000006E+03 0.29815000000006E+03
 0.31086570316825E+03 0.32643415613646E+03 0.29815000000006E+03 0.29815000000006E+03 0.35941463064471E+03
 0.30024648455280E+03 0.16495061573277E+04 0.16034170658624E+04 0.65025027514788E+03 0.13803195956667E+04
 0.72681806914308E+03 0.95353797741416E+03 0.92696488595442E+03 0.91655330815062E+03 0.17166185244791E+04
 0.83109141853887E+03 0.92532733544558E+03 0.80507604842709E+03 0.17153906346857E+04 0.95353797741416E+03
 0.92696488595442E+03 0.91655330815062E+03 0.17166185244791E+04 0.83109141853887E+03 0.92532733544558E+03
 0.80507604842709E+03 0.17153906346857E+04 0.17284657467352E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46866641513916E+03 0.12948656185413E+01
 0.12948656185413E+01 0.66306687247312E+00 0.26445626853066E+00 0.29856046587058E+03 0.33244481954783E+03
 0.32647086639113E+03 0.32592335709537E+03 0.23000000000000E+00 0.00000000000000E+00 0.21560886034794E+00
 0.00000000000000E+00 -.14997746089723E+02 0.99820254680025E-03 0.25903375934366E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30884005313710E+02 0.11581501992641E+02 0.30024481177733E+03 0.35945166454535E+03
 0.29869802123556E+03 0.30223899539805E+03 0.29815000000006E+03 0.29815000000006E+03 0.29869055877680E+03
 0.30224022763766E+03 0.29815000000006E+03 0.29815000000006E+03 0.29869802123556E+03 0.30223899539805E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29869055877680E+03 0.30224022763766E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30156788643914E+03 0.29815000000010E+03 0.69042702254072E+02 0.65424314761880E+02
 0.67825630331808E+02 0.33494961743862E+03 0.26678485895515E+03 0.49867041124327E+02 0.64540332724426E+02
 0.49709994321232E+02 0.26850817769742E+03 0.49242338535220E+02 0.64614038573227E+02 0.49095887632504E+02
 0.26857184706595E+03 0.49867041124327E+02 0.64540332724426E+02 0.49709994321231E+02 0.26850817769742E+03
 0.49242338535220E+02 0.64614038573227E+02 0.49095887632504E+02 0.26857184706595E+03 0.16506298776402E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35345083192139E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19068436322010E+00 0.00000000000000E+00 0.00000000000000E+00 0.19068436322010E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19899448937703E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19899448937703E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19640701339744E+00 0.20795408266832E+00 0.33244481954783E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    220.02537927
 0.10544944364148E+00 0.30434336815520E+03 0.42933665890781E+03 0.42494316791821E+03 0.42322696812570E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19175240881893E+00 0.00000000000000E+00 -.22137653569542E+02
 0.33590006890691E-02 0.56407918055006E+00 0.23816607201164E+04 0.89312277004366E+03 0.14182406080293E+02
 0.53184022801100E+01 0.31635345959971E+03 0.29815000000019E+03 0.31367310749501E+03 0.32757173147483E+03
 0.29815000000006E+03 0.29815000000006E+03 0.31144044495167E+03 0.32752986893531E+03 0.29815000000006E+03
 0.29815000000006E+03 0.31367310749501E+03 0.32757173147483E+03 0.29815000000006E+03 0.29815000000006E+03
 0.31144044495167E+03 0.32752986893531E+03 0.29815000000006E+03 0.29815000000006E+03 0.36115805377314E+03
 0.30052786052883E+03 0.16647621219101E+04 0.16160646024578E+04 0.65237132475826E+03 0.13651630345068E+04
 0.70952985312472E+03 0.96316956309368E+03 0.93843794616737E+03 0.92452086958115E+03 0.17212623006219E+04
 0.84097603625204E+03 0.93688885754186E+03 0.81361317605166E+03 0.17201156401139E+04 0.96316956309368E+03
 0.93843794616737E+03 0.92452086958116E+03 0.17212623006219E+04 0.84097603625204E+03 0.93688885754186E+03
 0.81361317605166E+03 0.17201156401139E+04 0.17302476874076E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46892191557313E+03 0.12948675987976E+01
 0.12948675987976E+01 0.70012076662582E+00 0.24384831341263E+00 0.29868058957851E+03 0.33261191957744E+03
 0.32709585451605E+03 0.32658059424694E+03 0.23000000000000E+00 0.00000000000000E+00 0.21456908664635E+00
 0.00000000000000E+00 -.15277601693426E+02 0.99779833128878E-03 0.27981508030487E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28590310398152E+02 0.10721366399307E+02 0.30052609876952E+03 0.36119484230216E+03
 0.29876163339179E+03 0.30244515068437E+03 0.29815000000006E+03 0.29815000000006E+03 0.29875356570734E+03
 0.30244638510308E+03 0.29815000000006E+03 0.29815000000006E+03 0.29876163339179E+03 0.30244515068437E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29875356570734E+03 0.30244638510308E+03 0.29815000000006E+03
 0.29815000000006E+03 0.30174584451679E+03 0.29815000000010E+03 0.73577429874161E+02 0.69491759134502E+02
 0.73129206633727E+02 0.34027213054794E+03 0.26677727788104E+03 0.53617730982801E+02 0.69575654764246E+02
 0.53534927873361E+02 0.27273297548537E+03 0.52978176911136E+02 0.69643363118952E+02 0.52905499592181E+02
 0.27279065426714E+03 0.53617730982801E+02 0.69575654764246E+02 0.53534927873361E+02 0.27273297548537E+03
 0.52978176911136E+02 0.69643363118952E+02 0.52905499592181E+02 0.27279065426714E+03 0.16946652878863E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35362889163844E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19266111343402E+00 0.00000000000000E+00 0.00000000000000E+00 0.19266111343402E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19983378942927E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19983378942927E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639820030426E+00 0.20783970829194E+00 0.33261191957744E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    230.00962064
 0.10817882317981E+00 0.30463869998745E+03 0.42969334172599E+03 0.42518392040050E+03 0.42342557496626E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19008780152354E+00 0.00000000000000E+00 -.22329530090199E+02
 0.32742521800443E-02 0.58817394827524E+00 0.24433060009116E+04 0.91623975034184E+03 0.13601418463805E+02
 0.51005319239269E+01 0.31713823837204E+03 0.29815000000029E+03 0.31432500072394E+03 0.32869675182803E+03
 0.29815000000007E+03 0.29815000000007E+03 0.31203995630322E+03 0.32865558996402E+03 0.29815000000007E+03
 0.29815000000007E+03 0.31432500072394E+03 0.32869675182803E+03 0.29815000000007E+03 0.29815000000007E+03
 0.31203995630322E+03 0.32865558996402E+03 0.29815000000007E+03 0.29815000000007E+03 0.36290495581714E+03
 0.30084974123995E+03 0.16788653180139E+04 0.16275532191545E+04 0.65332942610259E+03 0.13482640884579E+04
 0.69166801522485E+03 0.97218086302988E+03 0.94866102369393E+03 0.93188068928944E+03 0.17239026755347E+04
 0.85026860411927E+03 0.94719964851245E+03 0.82158222177808E+03 0.17228359456051E+04 0.97218086302988E+03
 0.94866102369393E+03 0.93188068928944E+03 0.17239026755347E+04 0.85026860411927E+03 0.94719964851246E+03
 0.82158222177808E+03 0.17228359456052E+04 0.17302004178178E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46913723739518E+03 0.12948690125979E+01
 0.12948690125979E+01 0.74005773210301E+00 0.22260442289479E+00 0.29883890816873E+03 0.33272224272887E+03
 0.32769385597186E+03 0.32721636517299E+03 0.23000000000000E+00 0.00000000000000E+00 0.21343443827821E+00
 0.00000000000000E+00 -.15481290547878E+02 0.99726771288656E-03 0.30240340975627E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26454728160796E+02 0.99205230602987E+01 0.30084785606661E+03 0.36294119336905E+03
 0.29883275027154E+03 0.30266043804201E+03 0.29815000000006E+03 0.29815000000006E+03 0.29882404357852E+03
 0.30266166146379E+03 0.29815000000007E+03 0.29815000000007E+03 0.29883275027154E+03 0.30266043804201E+03
 0.29815000000006E+03 0.29815000000006E+03 0.29882404357852E+03 0.30266166146379E+03 0.29815000000007E+03
 0.29815000000007E+03 0.30193170601092E+03 0.29815000000012E+03 0.78114212386868E+02 0.73541269946839E+02
 0.78564663885499E+02 0.34507883953912E+03 0.26612135233420E+03 0.57536813702076E+02 0.74721508940952E+02
 0.57536813702076E+02 0.27654219603117E+03 0.56888550212072E+02 0.74782166984589E+02 0.56888550212072E+02
 0.27659294298500E+03 0.57536813702076E+02 0.74721508940952E+02 0.57536813702076E+02 0.27654219603118E+03
 0.56888550212072E+02 0.74782166984589E+02 0.56888550212072E+02 0.27659294298500E+03 0.17367481314628E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35376230100968E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19404888801402E+00 0.00000000000000E+00 0.00000000000000E+00 0.19404888801402E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20063249741004E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20063249741004E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639207797223E+00 0.20776389864074E+00 0.33272224272887E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    240.00621686
 0.11041686872619E+00 0.30493402834548E+03 0.42999655851377E+03 0.42539355418839E+03 0.42360224762418E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18849059025521E+00 0.00000000000000E+00 -.22445729333182E+02
 0.32078861215255E-02 0.61131106884725E+00 0.24938541135605E+04 0.93519529258520E+03 0.13086627099826E+02
 0.49074851624348E+01 0.31789330970501E+03 0.29815000000047E+03 0.31495412275127E+03 0.32976872989915E+03
 0.29815000000007E+03 0.29815000000008E+03 0.31261859780455E+03 0.32972825348943E+03 0.29815000000007E+03
 0.29815000000008E+03 0.31495412275127E+03 0.32976872989915E+03 0.29815000000007E+03 0.29815000000008E+03
 0.31261859780455E+03 0.32972825348943E+03 0.29815000000007E+03 0.29815000000008E+03 0.36453639947268E+03
 0.30119068630595E+03 0.16911945235327E+04 0.16374014145399E+04 0.65332778704211E+03 0.13313520889548E+04
 0.67475766297753E+03 0.98012982131122E+03 0.95724543006472E+03 0.93825347805655E+03 0.17249532906932E+04
 0.85850704233266E+03 0.95586466355821E+03 0.82856533062072E+03 0.17239595870451E+04 0.98012982131122E+03
 0.95724543006472E+03 0.93825347805655E+03 0.17249532906932E+04 0.85850704233266E+03 0.95586466355821E+03
 0.82856533062072E+03 0.17239595870451E+04 0.17289012043809E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46932703344970E+03 0.12948698687885E+01
 0.12948698687885E+01 0.78004411700078E+00 0.20235721429334E+00 0.29903150199897E+03 0.33277699702084E+03
 0.32822456744246E+03 0.32778679929736E+03 0.23000000000000E+00 0.00000000000000E+00 0.21228696657096E+00
 0.00000000000000E+00 -.15607914272249E+02 0.99662416789140E-03 0.32516994413438E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24602519834041E+02 0.92259449377653E+01 0.30118870673835E+03 0.36457209128131E+03
 0.29890518902181E+03 0.30286802332929E+03 0.29815000000007E+03 0.29815000000007E+03 0.29889603972514E+03
 0.30286922329043E+03 0.29815000000007E+03 0.29815000000007E+03 0.29890518902181E+03 0.30286802332929E+03
 0.29815000000007E+03 0.29815000000007E+03 0.29889603972514E+03 0.30286922329043E+03 0.29815000000007E+03
 0.29815000000007E+03 0.30211072549838E+03 0.29815000000015E+03 0.82277554985982E+02 0.77251917757192E+02
 0.83719158097715E+02 0.34911275677369E+03 0.26497500288549E+03 0.61344778010391E+02 0.79582859850279E+02
 0.61495915429580E+02 0.27972843772481E+03 0.60694218510479E+02 0.79635763912066E+02 0.60859088152732E+02
 0.27977165101426E+03 0.61344778010391E+02 0.79582859850279E+02 0.61495915429580E+02 0.27972843772481E+03
 0.60694218510479E+02 0.79635763912066E+02 0.60859088152732E+02 0.27977165101426E+03 0.17740804485444E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35385417993952E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19486232043792E+00 0.00000000000000E+00 0.00000000000000E+00 0.19486232043792E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20134745384314E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20134745384314E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638860191369E+00 0.20772577724004E+00 0.33277699702084E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    250.00847732
 0.11217711293672E+00 0.30522798843966E+03 0.43027483287379E+03 0.42559903487697E+03 0.42378288831006E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18695806950880E+00 0.00000000000000E+00 -.22516234486151E+02
 0.31575490006236E-02 0.63349903798863E+00 0.25336107209801E+04 0.95010402036754E+03 0.12628274899044E+02
 0.47356030871413E+01 0.31862202955605E+03 0.29815000000078E+03 0.31556288350903E+03 0.33079372997016E+03
 0.29815000000009E+03 0.29815000000009E+03 0.31317871288117E+03 0.33075392492066E+03 0.29815000000008E+03
 0.29815000000010E+03 0.31556288350903E+03 0.33079372997016E+03 0.29815000000009E+03 0.29815000000009E+03
 0.31317871288117E+03 0.33075392492066E+03 0.29815000000008E+03 0.29815000000010E+03 0.36606624345557E+03
 0.30155101836475E+03 0.17022356226557E+04 0.16460747936206E+04 0.65274092454405E+03 0.13149449743405E+04
 0.65894034517374E+03 0.98728971334546E+03 0.96464373971589E+03 0.94389472763420E+03 0.17250533412169E+04
 0.86596289646250E+03 0.96333704074225E+03 0.83481756531060E+03 0.17241263207743E+04 0.98728971334546E+03
 0.96464373971589E+03 0.94389472763420E+03 0.17250533412169E+04 0.86596289646250E+03 0.96333704074225E+03
 0.83481756531060E+03 0.17241263207743E+04 0.17269580828767E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46951747058126E+03 0.12948703882921E+01
 0.12948703882921E+01 0.82005315883922E+00 0.18313356013832E+00 0.29926095232613E+03 0.33279123103278E+03
 0.32869755149146E+03 0.32830031343702E+03 0.23000000000000E+00 0.00000000000000E+00 0.21113004129649E+00
 0.00000000000000E+00 -.15686117108235E+02 0.99585926398904E-03 0.34805756391119E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22984703765959E+02 0.86192639122345E+01 0.30154895246216E+03 0.36610128480139E+03
 0.29898023858653E+03 0.30306834800120E+03 0.29815000000007E+03 0.29815000000007E+03 0.29897055980443E+03
 0.30306951210643E+03 0.29815000000007E+03 0.29815000000008E+03 0.29898023858653E+03 0.30306834800120E+03
 0.29815000000007E+03 0.29815000000007E+03 0.29897055980443E+03 0.30306951210643E+03 0.29815000000007E+03
 0.29815000000008E+03 0.30228338744543E+03 0.29815000000019E+03 0.86057137340813E+02 0.80624509347050E+02
 0.88590797019793E+02 0.35252022675730E+03 0.26348647575241E+03 0.65035229523712E+02 0.84160001018352E+02
 0.65454741233125E+02 0.28240845413788E+03 0.64390063352249E+02 0.84204579005842E+02 0.64827714432775E+02
 0.28244366105698E+03 0.65035229523712E+02 0.84160001018352E+02 0.65454741233125E+02 0.28240845413788E+03
 0.64390063352248E+02 0.84204579005842E+02 0.64827714432773E+02 0.28244366105698E+03 0.18072549439331E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35391994296694E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19532486435129E+00 0.00000000000000E+00 0.00000000000000E+00 0.19532486435129E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20197888986818E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20197888986818E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638678459986E+00 0.20771480994019E+00 0.33279123103278E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    260.01581690
 0.11352132822569E+00 0.30551928696742E+03 0.43054666610281E+03 0.42581557472147E+03 0.42398114430682E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18548617920444E+00 0.00000000000000E+00 -.22560204293082E+02
 0.31201600831026E-02 0.65477520445990E+00 0.25639710101172E+04 0.96148912879396E+03 0.12217933644263E+02
 0.45817251165988E+01 0.31932744275108E+03 0.29815000000132E+03 0.31615349365108E+03 0.33177772885024E+03
 0.29815000000011E+03 0.29815000000012E+03 0.31372238307603E+03 0.33173857893340E+03 0.29815000000010E+03
 0.29815000000013E+03 0.31615349365108E+03 0.33177772885024E+03 0.29815000000011E+03 0.29815000000012E+03
 0.31372238307603E+03 0.33173857893340E+03 0.29815000000010E+03 0.29815000000013E+03 0.36750920403410E+03
 0.30193069988502E+03 0.17123371105028E+04 0.16539020804738E+04 0.65181381902732E+03 0.12993011044430E+04
 0.64422821632053E+03 0.99386281577028E+03 0.97119278462013E+03 0.94899330521124E+03 0.17246421214565E+04
 0.87283640668577E+03 0.96995419199589E+03 0.84052642177254E+03 0.17237760444183E+04 0.99386281577028E+03
 0.97119278462013E+03 0.94899330521124E+03 0.17246421214565E+04 0.87283640668577E+03 0.96995419199589E+03
 0.84052642177254E+03 0.17237760444183E+04 0.17247584430765E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46972276710390E+03 0.12948707122754E+01
 0.12948707122754E+01 0.86008251714131E+00 0.16493889476589E+00 0.29953143966048E+03 0.33277778034888E+03
 0.32912203722345E+03 0.32876524237934E+03 0.23000000000000E+00 0.00000000000000E+00 0.20996596009335E+00
 0.00000000000000E+00 -.15734468645644E+02 0.10726983799788E-02 0.37102769726753E+00 0.74578279871717E+04
 0.27966854951894E+04 0.21561732611653E+02 0.80856497293701E+01 0.30192855178473E+03 0.36754346388405E+03
 0.29905733305213E+03 0.30326189148058E+03 0.29815000000007E+03 0.29815000000008E+03 0.29904726023532E+03
 0.30326300802288E+03 0.29815000000008E+03 0.29815000000008E+03 0.29905733305213E+03 0.30326189148058E+03
 0.29815000000007E+03 0.29815000000008E+03 0.29904726023532E+03 0.30326300802288E+03 0.29815000000008E+03
 0.29815000000008E+03 0.30245016062070E+03 0.29815000000025E+03 0.89453613165254E+02 0.83676865794516E+02
 0.93189036534526E+02 0.35543605520767E+03 0.26178107349047E+03 0.68615654780725E+02 0.88464378774607E+02
 0.69444494496320E+02 0.28469038083175E+03 0.67981804700357E+02 0.88500197018666E+02 0.68832730147698E+02
 0.28471723987988E+03 0.68615654780725E+02 0.88464378774607E+02 0.69444494496320E+02 0.28469038083175E+03
 0.67981804700355E+02 0.88500197018666E+02 0.68832730147695E+02 0.28471723987988E+03 0.18368852794983E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35397204528781E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19557829400393E+00 0.00000000000000E+00 0.00000000000000E+00 0.19557829400393E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20253096386394E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20253096386394E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638597862036E+00 0.20772225397393E+00 0.33277778034888E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    270.01761254
 0.11452619906114E+00 0.30580642682102E+03 0.43082271171913E+03 0.42605016507576E+03 0.42420242618026E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18407250181528E+00 0.00000000000000E+00 -.22591350765963E+02
 0.30927831517924E-02 0.67516112745518E+00 0.25866669622032E+04 0.97000011082619E+03 0.11849023402982E+02
 0.44433837761182E+01 0.32001152928865E+03 0.29815000000220E+03 0.31672735283027E+03 0.33272509024100E+03
 0.29815000000014E+03 0.29815000000017E+03 0.31425092845794E+03 0.33268657712945E+03 0.29815000000012E+03
 0.29815000000018E+03 0.31672735283027E+03 0.33272509024100E+03 0.29815000000014E+03 0.29815000000017E+03
 0.31425092845794E+03 0.33268657712945E+03 0.29815000000012E+03 0.29815000000018E+03 0.36887661320057E+03
 0.30232927217821E+03 0.17217241839165E+04 0.16610922324725E+04 0.65070260137426E+03 0.12845415263446E+04
 0.63058541196350E+03 0.99998365130646E+03 0.97712135704384E+03 0.95367424835713E+03 0.17239991446787E+04
 0.87925976562804E+03 0.97594537799917E+03 0.84581458576969E+03 0.17231887597188E+04 0.99998365130646E+03
 0.97712135704384E+03 0.95367424835713E+03 0.17239991446787E+04 0.87925976562804E+03 0.97594537799917E+03
 0.84581458576969E+03 0.17231887597188E+04 0.17225318242991E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46994897349240E+03 0.12948709417725E+01
 0.12948709417725E+01 0.90008969972480E+00 0.14779073605713E+00 0.29984624836812E+03 0.33274723048806E+03
 0.32950559024505E+03 0.32918837230325E+03 0.23000000000000E+00 0.00000000000000E+00 0.20879767599245E+00
 0.00000000000000E+00 -.15766638968828E+02 0.11971633477525E-02 0.39402698242646E+00 0.66824631868480E+04
 0.25059236950680E+04 0.20303178098960E+02 0.76136917871099E+01 0.30232704627917E+03 0.36890996179148E+03
 0.29913671273141E+03 0.30344904972503E+03 0.29815000000008E+03 0.29815000000009E+03 0.29912632167703E+03
 0.30345010790232E+03 0.29815000000008E+03 0.29815000000009E+03 0.29913671273141E+03 0.30344904972503E+03
 0.29815000000008E+03 0.29815000000009E+03 0.29912632167703E+03 0.30345010790232E+03 0.29815000000008E+03
 0.29815000000009E+03 0.30261140012437E+03 0.29815000000036E+03 0.92474853802583E+02 0.86432400748773E+02
 0.97525058709511E+02 0.35797448874372E+03 0.25996180474066E+03 0.72090145885099E+02 0.92508916102898E+02
 0.73499953003919E+02 0.28666607714730E+03 0.71473309522054E+02 0.92535681059657E+02 0.72909005360269E+02
 0.28668437826512E+03 0.72090145885099E+02 0.92508916102898E+02 0.73499953003919E+02 0.28666607714730E+03
 0.71473309522054E+02 0.92535681059657E+02 0.72909005360269E+02 0.28668437826512E+03 0.18635167377278E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35402015160733E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19572486529899E+00 0.00000000000000E+00 0.00000000000000E+00 0.19572486529899E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20301202712312E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20301202712312E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638570403510E+00 0.20774096871253E+00 0.33274723048806E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    280.01347877
 0.11526922474154E+00 0.30608846715334E+03 0.43110858252285E+03 0.42630492526427E+03 0.42444743639189E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18271350796087E+00 0.00000000000000E+00 -.22616930979115E+02
 0.30728468763520E-02 0.69470079223593E+00 0.26034489585428E+04 0.97629335945355E+03 0.11515749066950E+02
 0.43184059001061E+01 0.32067648382837E+03 0.29815000000363E+03 0.31728610576486E+03 0.33364027445986E+03
 0.29815000000019E+03 0.29815000000025E+03 0.31476587553075E+03 0.33360237845677E+03 0.29815000000016E+03
 0.29815000000025E+03 0.31728610576486E+03 0.33364027445986E+03 0.29815000000019E+03 0.29815000000025E+03
 0.31476587553075E+03 0.33360237845677E+03 0.29815000000016E+03 0.29815000000025E+03 0.37017886058715E+03
 0.30274664333369E+03 0.17305512256375E+04 0.16677844503336E+04 0.64950159419294E+03 0.12706808362882E+04
 0.61793173412432E+03 0.10057480281287E+04 0.98258855427284E+03 0.95802545669489E+03 0.17232901127447E+04
 0.88532699928489E+03 0.98147019779309E+03 0.85076822452943E+03 0.17225306801756E+04 0.10057480281287E+04
 0.98258855427284E+03 0.95802545669489E+03 0.17232901127447E+04 0.88532699928490E+03 0.98147019779309E+03
 0.85076822452943E+03 0.17225306801756E+04 0.17203991063908E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47019739287041E+03 0.12948711302556E+01
 0.12948711302556E+01 0.94007316463517E+00 0.13168403967491E+00 0.30020825139428E+03 0.33270799993577E+03
 0.32985486781819E+03 0.32957576359014E+03 0.23000000000000E+00 0.00000000000000E+00 0.20762649714758E+00
 0.00000000000000E+00 -.15790097388955E+02 0.13435919949250E-02 0.41703182439164E+00 0.59541884963722E+04
 0.22328206861396E+04 0.19183188265477E+02 0.71936955995537E+01 0.30274434547409E+03 0.37021118582801E+03
 0.29921862648209E+03 0.30363040454012E+03 0.29815000000008E+03 0.29815000000010E+03 0.29920799233121E+03
 0.30363139449360E+03 0.29815000000008E+03 0.29815000000010E+03 0.29921862648209E+03 0.30363040454012E+03
 0.29815000000008E+03 0.29815000000010E+03 0.29920799233121E+03 0.30363139449360E+03 0.29815000000008E+03
 0.29815000000010E+03 0.30276761926385E+03 0.29815000000054E+03 0.95135749370378E+02 0.88918504615870E+02
 0.10161843367075E+03 0.36023422875599E+03 0.25810770291688E+03 0.75468668224718E+02 0.96314269052599E+02
 0.77667254302935E+02 0.28841530721651E+03 0.74873962903109E+02 0.96331797897581E+02 0.77102154552642E+02
 0.28842494243759E+03 0.75468668224717E+02 0.96314269052599E+02 0.77667254302934E+02 0.28841530721651E+03
 0.74873962903109E+02 0.96331797897581E+02 0.77102154552642E+02 0.28842494243759E+03 0.18876591873465E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35407147641149E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19581943179763E+00 0.00000000000000E+00 0.00000000000000E+00 0.19581943179763E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20343076199989E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20343076199989E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638569225079E+00 0.20776540345218E+00 0.33270799993577E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    290.00994490
 0.11581801657385E+00 0.30636522329355E+03 0.43140685273627E+03 0.42657949489920E+03 0.42471467760807E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18140514245362E+00 0.00000000000000E+00 -.22641241846500E+02
 0.30582862908417E-02 0.71344989899927E+00 0.26158440509499E+04 0.98094151910622E+03 0.11213120937043E+02
 0.42049203513912E+01 0.32132465209302E+03 0.29815000000589E+03 0.31783157757953E+03 0.33452767823296E+03
 0.29815000000027E+03 0.29815000000039E+03 0.31526891242652E+03 0.33449037941024E+03 0.29815000000023E+03
 0.29815000000039E+03 0.31783157757953E+03 0.33452767823296E+03 0.29815000000027E+03 0.29815000000039E+03
 0.31526891242652E+03 0.33449037941024E+03 0.29815000000023E+03 0.29815000000039E+03 0.37142518846824E+03
 0.30318296443622E+03 0.17389257164292E+04 0.16740738454951E+04 0.64826505061760E+03 0.12576754417670E+04
 0.60616906589635E+03 0.10112250423699E+04 0.98770404546636E+03 0.96211046568373E+03 0.17226056932008E+04
 0.89110601604528E+03 0.98663881544252E+03 0.85544979052506E+03 0.17218929655182E+04 0.10112250423699E+04
 0.98770404546636E+03 0.96211046568373E+03 0.17226056932008E+04 0.89110601604528E+03 0.98663881544252E+03
 0.85544979052506E+03 0.17218929655182E+04 0.17184147259557E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47046701953591E+03 0.12948713093859E+01
 0.12948713093859E+01 0.98005902916816E+00 0.11659950113732E+00 0.30062001473210E+03 0.33266665310501E+03
 0.33017557174007E+03 0.32993267301996E+03 0.23000000000000E+00 0.00000000000000E+00 0.20645263087493E+00
 0.00000000000000E+00 -.15809476248902E+02 0.15174129480243E-02 0.44003906385081E+00 0.52721311034126E+04
 0.19770491637797E+04 0.18180204116406E+02 0.68175765436523E+01 0.30318060124109E+03 0.37145639335019E+03
 0.29930347232982E+03 0.30380667358546E+03 0.29815000000009E+03 0.29815000000012E+03 0.29929266954358E+03
 0.30380758637444E+03 0.29815000000009E+03 0.29815000000012E+03 0.29930347232982E+03 0.30380667358546E+03
 0.29815000000009E+03 0.29815000000012E+03 0.29929266954358E+03 0.30380758637444E+03 0.29815000000009E+03
 0.29815000000012E+03 0.30291944523142E+03 0.29815000000081E+03 0.97454029633004E+02 0.91162718077229E+02
 0.10549226772133E+03 0.36229749316325E+03 0.25627776410331E+03 0.78762694718945E+02 0.99904276452312E+02
 0.81999391000614E+02 0.29000475192054E+03 0.78194683859160E+02 0.99912482938174E+02 0.81464628367558E+02
 0.29000570175760E+03 0.78762694718945E+02 0.99904276452312E+02 0.81999391000613E+02 0.29000475192054E+03
 0.78194683859160E+02 0.99912482938174E+02 0.81464628367558E+02 0.29000570175760E+03 0.19097632888635E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35413138576888E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19589521528682E+00 0.00000000000000E+00 0.00000000000000E+00 0.19589521528682E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20379628005996E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20379628005996E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638577389920E+00 0.20779127305147E+00 0.33266665310501E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    300.02115887
 0.11622749071306E+00 0.30663697535650E+03 0.43171834907405E+03 0.42687238434000E+03 0.42500179537116E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18014294395186E+00 0.00000000000000E+00 -.22666455997854E+02
 0.30475115883881E-02 0.73147260291479E+00 0.26250925609216E+04 0.98440971034561E+03 0.10936841609817E+02
 0.41013156036815E+01 0.32195851729446E+03 0.29815000000937E+03 0.31836574896263E+03 0.33539165017476E+03
 0.29815000000040E+03 0.29815000000061E+03 0.31576186562702E+03 0.33535492912006E+03 0.29815000000033E+03
 0.29815000000061E+03 0.31836574896263E+03 0.33539165017476E+03 0.29815000000040E+03 0.29815000000061E+03
 0.31576186562702E+03 0.33535492912006E+03 0.29815000000033E+03 0.29815000000061E+03 0.37262400578586E+03
 0.30363870940926E+03 0.17469253321291E+04 0.16800271180305E+04 0.64701997368887E+03 0.12454474515767E+04
 0.59519237801944E+03 0.10164662180566E+04 0.99254365049867E+03 0.96597599383506E+03 0.17219880044243E+04
 0.89664774392668E+03 0.99152752782453E+03 0.85990570511200E+03 0.17213182003883E+04 0.10164662180566E+04
 0.99254365049867E+03 0.96597599383506E+03 0.17219880044243E+04 0.89664774392668E+03 0.99152752782453E+03
 0.85990570511200E+03 0.17213182003883E+04 0.17165920358954E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47075587623420E+03 0.12948714951720E+01
 0.12948714951720E+01 0.10201038850454E+01 0.10250769671007E+00 0.30108379928944E+03 0.33262814961319E+03
 0.33047245714926E+03 0.33026353677445E+03 0.23000000000000E+00 0.00000000000000E+00 0.20527512706328E+00
 0.00000000000000E+00 -.15827310733475E+02 0.17260124994539E-02 0.46306737674797E+00 0.46349606405117E+04
 0.17381102401919E+04 0.17276103655115E+02 0.64785388706681E+01 0.30363628829015E+03 0.37265401137983E+03
 0.29939180535166E+03 0.30397867198011E+03 0.29815000000010E+03 0.29815000000015E+03 0.29938090718547E+03
 0.30397949951096E+03 0.29815000000010E+03 0.29815000000015E+03 0.29939180535166E+03 0.30397867198011E+03
 0.29815000000010E+03 0.29815000000015E+03 0.29938090718547E+03 0.30397949951096E+03 0.29815000000010E+03
 0.29815000000015E+03 0.30306758429806E+03 0.29815000000126E+03 0.99448814728353E+02 0.93190727391578E+02
 0.10917170835877E+03 0.36423152697094E+03 0.25451396007038E+03 0.81985156584720E+02 0.10330440676139E+03
 0.86557250690044E+02 0.29148907482914E+03 0.81447897029369E+02 0.10330328165805E+03 0.86056807183272E+02
 0.29148138956675E+03 0.81985156584718E+02 0.10330440676139E+03 0.86557250690040E+02 0.29148907482914E+03
 0.81447897029369E+02 0.10330328165805E+03 0.86056807183272E+02 0.29148138956675E+03 0.19302168248823E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35420376829115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19596968443709E+00 0.00000000000000E+00 0.00000000000000E+00 0.19596968443709E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20411716173943E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20411716173943E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638585350655E+00 0.20781537364943E+00 0.33262814961319E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    310.00046049
 0.11653843778909E+00 0.30690250327167E+03 0.43204079773894E+03 0.42717965729067E+03 0.42530424894790E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17893031233868E+00 0.00000000000000E+00 -.22693377056336E+02
 0.30393799860298E-02 0.74872287459392E+00 0.26321157725494E+04 0.98704341470602E+03 0.10684861210283E+02
 0.40068229538560E+01 0.32257648949372E+03 0.29815000001461E+03 0.31888719121914E+03 0.33623082430749E+03
 0.29815000000061E+03 0.29815000000096E+03 0.31624340735055E+03 0.33619465885079E+03 0.29815000000049E+03
 0.29815000000096E+03 0.31888719121914E+03 0.33623082430749E+03 0.29815000000061E+03 0.29815000000096E+03
 0.31624340735055E+03 0.33619465885079E+03 0.29815000000049E+03 0.29815000000096E+03 0.37377536857716E+03
 0.30411142366545E+03 0.17545583954122E+04 0.16856559115994E+04 0.64578418523890E+03 0.12339778356426E+04
 0.58496472947753E+03 0.10214783051142E+04 0.99713033861942E+03 0.96963326073051E+03 0.17214532840711E+04
 0.90195683183300E+03 0.99615945669620E+03 0.86414368551183E+03 0.17208227992505E+04 0.10214783051142E+04
 0.99713033861942E+03 0.96963326073051E+03 0.17214532840711E+04 0.90195683183300E+03 0.99615945669620E+03
 0.86414368551183E+03 0.17208227992505E+04 0.17149319580534E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47105969127173E+03 0.12948716935352E+01
 0.12948716935352E+01 0.10600210914996E+01 0.89459185608118E-01 0.30159789077670E+03 0.33259636077902E+03
 0.33074762885801E+03 0.33057003567605E+03 0.23000000000000E+00 0.00000000000000E+00 0.20410000188245E+00
 0.00000000000000E+00 -.15831671489955E+02 0.19777682927232E-02 0.48599909756372E+00 0.40449632191164E+04
 0.15168612071686E+04 0.16460935915526E+02 0.61728509683223E+01 0.30410895324298E+03 0.37380412370186E+03
 0.29948369774350E+03 0.30414614669229E+03 0.29815000000010E+03 0.29815000000020E+03 0.29947277583601E+03
 0.30414688227897E+03 0.29815000000010E+03 0.29815000000020E+03 0.29948369774350E+03 0.30414614669229E+03
 0.29815000000010E+03 0.29815000000020E+03 0.29947277583601E+03 0.30414688227897E+03 0.29815000000010E+03
 0.29815000000020E+03 0.30321181762909E+03 0.29815000000193E+03 0.10112824443047E+03 0.95012351591791E+02
 0.11265926844913E+03 0.36607847919442E+03 0.25285591440305E+03 0.85128462752796E+02 0.10651898723152E+03
 0.91377091181672E+02 0.29290319838818E+03 0.84625318161822E+02 0.10650864279034E+03 0.90914231098749E+02
 0.29288703824794E+03 0.85128462752796E+02 0.10651898723152E+03 0.91377091181672E+02 0.29290319838818E+03
 0.84625318161822E+02 0.10650864279034E+03 0.90914231098749E+02 0.29288703824794E+03 0.19492195048938E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35429079944415E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19605082282503E+00 0.00000000000000E+00 0.00000000000000E+00 0.19605082282503E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20439977867005E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20439977867005E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638631193235E+00 0.20783571239227E+00 0.33259636077902E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    320.00776621
 0.11678332450270E+00 0.30716378985024E+03 0.43237489215059E+03 0.42750070255349E+03 0.42562097967782E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17775788092901E+00 0.00000000000000E+00 -.22722163853840E+02
 0.30330063461385E-02 0.76533727047801E+00 0.26376469703683E+04 0.98911761388812E+03 0.10452907899028E+02
 0.39198404621354E+01 0.32318350728770E+03 0.29815000002237E+03 0.31940000574247E+03 0.33705246812520E+03
 0.29815000000093E+03 0.29815000000152E+03 0.31671732348867E+03 0.33701684002958E+03 0.29815000000074E+03
 0.29815000000151E+03 0.31940000574247E+03 0.33705246812520E+03 0.29815000000093E+03 0.29815000000152E+03
 0.31671732348867E+03 0.33701684002958E+03 0.29815000000074E+03 0.29815000000151E+03 0.37489056729147E+03
 0.30460351075686E+03 0.17619039301246E+04 0.16910235512310E+04 0.64455683041342E+03 0.12231289402480E+04
 0.57534932568251E+03 0.10263142870399E+04 0.10015225262586E+04 0.97312666124852E+03 0.17209973133838E+04
 0.90708760429284E+03 0.10005936256385E+04 0.86821110368159E+03 0.17204031044674E+04 0.10263142870399E+04
 0.10015225262586E+04 0.97312666124852E+03 0.17209973133838E+04 0.90708760429284E+03 0.10005936256385E+04
 0.86821110368159E+03 0.17204031044674E+04 0.17134099721215E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47137759640240E+03 0.12948719056458E+01
 0.12948719056458E+01 0.11000503144059E+01 0.77361056254220E-01 0.30216466757639E+03 0.33257372054642E+03
 0.33100540290811E+03 0.33085648449581E+03 0.23000000000000E+00 0.00000000000000E+00 0.20292058967458E+00
 0.00000000000000E+00 -.15833994417535E+02 0.22870618119743E-02 0.50896396297913E+00 0.34979378161599E+04
 0.13117266810600E+04 0.15718205181313E+02 0.58943269429922E+01 0.30460099932037E+03 0.37491802857398E+03
 0.29958033171476E+03 0.30431062187384E+03 0.29815000000011E+03 0.29815000000027E+03 0.29956945531095E+03
 0.30431125917480E+03 0.29815000000011E+03 0.29815000000027E+03 0.29958033171476E+03 0.30431062187384E+03
 0.29815000000011E+03 0.29815000000027E+03 0.29956945531095E+03 0.30431125917480E+03 0.29815000000011E+03
 0.29815000000027E+03 0.30335346274944E+03 0.29815000000295E+03 0.10251888713349E+03 0.96651899672348E+02
 0.11599343956660E+03 0.36788784110114E+03 0.25131443433671E+03 0.88216738086967E+02 0.10958530735244E+03
 0.96549418419978E+02 0.29428719391717E+03 0.87750743479099E+02 0.10956586943063E+03 0.96127091960374E+02
 0.29426273000831E+03 0.88216738086967E+02 0.10958530735244E+03 0.96549418419978E+02 0.29428719391717E+03
 0.87750743479099E+02 0.10956586943063E+03 0.96127091960374E+02 0.29426273000831E+03 0.19671085733159E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35439448740783E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19614031967640E+00 0.00000000000000E+00 0.00000000000000E+00 0.19614031967640E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20465060400883E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20465060400883E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638676992721E+00 0.20785034094291E+00 0.33257372054642E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    330.03079212
 0.11697732003530E+00 0.30742104848341E+03 0.43271873540243E+03 0.42783307286162E+03 0.42594937169158E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17662551849249E+00 0.00000000000000E+00 -.22747414737925E+02
 0.30279761224819E-02 0.78132131745014E+00 0.26420287599371E+04 0.99076078497640E+03 0.10239065313242E+02
 0.38396494924656E+01 0.32377967213565E+03 0.29815000003369E+03 0.31990421982617E+03 0.33785726354160E+03
 0.29815000000142E+03 0.29815000000236E+03 0.31718361969725E+03 0.33782215367677E+03 0.29815000000112E+03
 0.29815000000235E+03 0.31990421982617E+03 0.33785726354160E+03 0.29815000000142E+03 0.29815000000236E+03
 0.31718361969725E+03 0.33782215367677E+03 0.29815000000112E+03 0.29815000000235E+03 0.37597205542455E+03
 0.30511439153836E+03 0.17689794821594E+04 0.16961492100616E+04 0.64333497877553E+03 0.12128403876018E+04
 0.56628873393238E+03 0.10309854985536E+04 0.10057342110028E+04 0.97647002420954E+03 0.17206108381882E+04
 0.91205094555745E+03 0.10048442663268E+04 0.87212103271168E+03 0.17200501008344E+04 0.10309854985536E+04
 0.10057342110028E+04 0.97647002420954E+03 0.17206108381882E+04 0.91205094555745E+03 0.10048442663268E+04
 0.87212103271168E+03 0.17200501008344E+04 0.17120086400119E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47170704675425E+03 0.12948720917027E+01
 0.12948720917027E+01 0.11401424180400E+01 0.66226907145813E-01 0.30281460822461E+03 0.33256177027513E+03
 0.33124839524915E+03 0.33112560409599E+03 0.23000000000000E+00 0.00000000000000E+00 0.20173871807647E+00
 0.00000000000000E+00 -.15831578021796E+02 0.26715651301864E-02 0.53192631747623E+00 0.29944993328469E+04
 0.11229372498176E+04 0.15039676995033E+02 0.56398788731374E+01 0.30511184850251E+03 0.37599820437061E+03
 0.29964819035996E+03 0.30447229617748E+03 0.29815000000011E+03 0.29815000000037E+03 0.29963770820869E+03
 0.30447283001355E+03 0.29815000000011E+03 0.29815000000037E+03 0.29964819035996E+03 0.30447229617748E+03
 0.29815000000011E+03 0.29815000000037E+03 0.29963770820869E+03 0.30447283001355E+03 0.29815000000011E+03
 0.29815000000037E+03 0.30349268271740E+03 0.29815000000446E+03 0.10362330749443E+03 0.98206295959596E+02
 0.11918245362144E+03 0.36967967525515E+03 0.24990130936560E+03 0.91432165325759E+02 0.11251250151716E+03
 0.91432165325759E+02 0.29565698130343E+03 0.91003089561745E+02 0.11248418107797E+03 0.91003089561745E+02
 0.29562446071699E+03 0.91432165325759E+02 0.11251250151716E+03 0.91432165325759E+02 0.29565698130343E+03
 0.91003089561745E+02 0.11248418107797E+03 0.91003089561745E+02 0.29562446071699E+03 0.19827868841744E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35451541522455E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19619861192506E+00 0.00000000000000E+00 0.00000000000000E+00 0.19619861192506E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20484604078700E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20484604078700E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638731489721E+00 0.20785839159639E+00 0.33256177027513E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    340.09412418
 0.11714113939002E+00 0.30767448600433E+03 0.43307174745057E+03 0.42817535475651E+03 0.42628772025951E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17552908518495E+00 0.00000000000000E+00 -.22780547409510E+02
 0.30237412777591E-02 0.79673703272524E+00 0.26457290042780E+04 0.99214837660426E+03 0.10040954130921E+02
 0.37653577990953E+01 0.32436719646188E+03 0.29815000005000E+03 0.32040165865826E+03 0.33864854571961E+03
 0.29815000000216E+03 0.29815000000365E+03 0.31764397749179E+03 0.33861393663952E+03 0.29815000000169E+03
 0.29815000000363E+03 0.32040165865826E+03 0.33864854571961E+03 0.29815000000216E+03 0.29815000000365E+03
 0.31764397749179E+03 0.33861393663952E+03 0.29815000000169E+03 0.29815000000363E+03 0.37702521136494E+03
 0.30564580613052E+03 0.17758182338849E+04 0.17010551137816E+04 0.64211482338688E+03 0.12030315847003E+04
 0.55770618719648E+03 0.10355151108833E+04 0.10097899827402E+04 0.97967959019462E+03 0.17202819779233E+04
 0.91687045312858E+03 0.10089363102656E+04 0.87589097606016E+03 0.17197522283939E+04 0.10355151108833E+04
 0.10097899827402E+04 0.97967959019462E+03 0.17202819779233E+04 0.91687045312858E+03 0.10089363102656E+04
 0.87589097606016E+03 0.17197522283939E+04 0.17107041841117E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47204646645983E+03 0.12948723358354E+01
 0.12948723358354E+01 0.11803957462841E+01 0.56018068502926E-01 0.30354170409555E+03 0.33256218209885E+03
 0.33147840134900E+03 0.33137900961651E+03 0.23000000000000E+00 0.00000000000000E+00 0.20055165286990E+00
 0.00000000000000E+00 -.15837945660803E+02 0.31584358484431E-02 0.55493783098623E+00 0.25328993159521E+04
 0.94983724348203E+03 0.14416029243821E+02 0.54060109664328E+01 0.30564323389092E+03 0.37705004237683E+03
 0.29972093648406E+03 0.30463196379689E+03 0.29815000000012E+03 0.29815000000053E+03 0.29971030873047E+03
 0.30463238942858E+03 0.29815000000012E+03 0.29815000000053E+03 0.29972093648406E+03 0.30463196379689E+03
 0.29815000000012E+03 0.29815000000053E+03 0.29971030873047E+03 0.30463238942858E+03 0.29815000000012E+03
 0.29815000000053E+03 0.30363015704317E+03 0.29815000000667E+03 0.10445894341028E+03 0.99655838093236E+02
 0.12225214600949E+03 0.37149336359384E+03 0.24862995685431E+03 0.94613848845954E+02 0.11532497332134E+03
 0.94613848845954E+02 0.29704567927918E+03 0.94227253614623E+02 0.11528795153498E+03 0.94227253614623E+02
 0.29700531591989E+03 0.94613848845954E+02 0.11532497332134E+03 0.94613848845954E+02 0.29704567927918E+03
 0.94227253614622E+02 0.11528795153498E+03 0.94227253614622E+02 0.29700531591989E+03 0.19991514893740E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35465664162344E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19631236388448E+00 0.00000000000000E+00 0.00000000000000E+00 0.19631236388448E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20505228809035E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20505228809035E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638749748993E+00 0.20785831438653E+00 0.33256218209885E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    350.01973762
 0.11728714592982E+00 0.30792045279904E+03 0.43342634704300E+03 0.42851960433192E+03 0.42662798807783E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17448599557088E+00 0.00000000000000E+00 -.22812688949663E+02
 0.30199768446561E-02 0.81134433260763E+00 0.26490269334866E+04 0.99338510005748E+03 0.98601785684363E+01
 0.36975669631636E+01 0.32493678883311E+03 0.29815000007264E+03 0.32088442498936E+03 0.33941389025753E+03
 0.29815000000323E+03 0.29815000000552E+03 0.31809110706056E+03 0.33937975789747E+03 0.29815000000252E+03
 0.29815000000549E+03 0.32088442498936E+03 0.33941389025753E+03 0.29815000000323E+03 0.29815000000552E+03
 0.31809110706056E+03 0.33937975789747E+03 0.29815000000252E+03 0.29815000000549E+03 0.37803359213508E+03
 0.30618626343135E+03 0.17823178966005E+04 0.17056726348774E+04 0.64092660445204E+03 0.11938379955075E+04
 0.54970675803317E+03 0.10398360252265E+04 0.10136334295157E+04 0.98271299286186E+03 0.17200052524410E+04
 0.92147371126805E+03 0.10128129763945E+04 0.87946833208181E+03 0.17195037063081E+04 0.10398360252265E+04
 0.10136334295157E+04 0.98271299286186E+03 0.17200052524410E+04 0.92147371126805E+03 0.10128129763945E+04
 0.87946833208181E+03 0.17195037063081E+04 0.17095063149822E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47238786701754E+03 0.12948725726651E+01
 0.12948725726651E+01 0.12200982000520E+01 0.46875542009068E-01 0.30429173345373E+03 0.33257554969964E+03
 0.33169167022189E+03 0.33161231184339E+03 0.23000000000000E+00 0.00000000000000E+00 0.19938053500040E+00
 0.00000000000000E+00 -.15844111541950E+02 0.37744514527313E-02 0.57758907649084E+00 0.21195132856222E+04
 0.79481748210833E+03 0.13850677454990E+02 0.51940040456211E+01 0.30618367292486E+03 0.37805712662722E+03
 0.29979847374272E+03 0.30478727600128E+03 0.29815000000013E+03 0.29815000000075E+03 0.29978792613276E+03
 0.30478759112999E+03 0.29815000000013E+03 0.29815000000075E+03 0.29979847374272E+03 0.30478727600128E+03
 0.29815000000013E+03 0.29815000000075E+03 0.29978792613276E+03 0.30478759112999E+03 0.29815000000013E+03
 0.29815000000075E+03 0.30376388250107E+03 0.29815000000978E+03 0.10503638541102E+03 0.10086778638632E+03
 0.12516200614389E+03 0.37330789960341E+03 0.24752008342880E+03 0.97702819074076E+02 0.11798732188862E+03
 0.97702819074076E+02 0.29843826062988E+03 0.97358730312649E+02 0.11794199844721E+03 0.97358730312649E+02
 0.29839046799851E+03 0.97702819074076E+02 0.11798732188862E+03 0.97702819074076E+02 0.29843826062988E+03
 0.97358730312649E+02 0.11794199844721E+03 0.97358730312649E+02 0.29839046799851E+03 0.20144488006273E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35481241139258E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19641803581095E+00 0.00000000000000E+00 0.00000000000000E+00 0.19641803581095E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20523256556566E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20523256556566E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638760473306E+00 0.20785006054047E+00 0.33257554969964E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    360.01363277
 0.11742275761594E+00 0.30816436197265E+03 0.43378850003165E+03 0.42887145579365E+03 0.42697571204015E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17347269508748E+00 0.00000000000000E+00 -.22845284538319E+02
 0.30164887774921E-02 0.82547797234061E+00 0.26520900921936E+04 0.99453378457261E+03 0.96913549095881E+01
 0.36342580910956E+01 0.32550089009232E+03 0.29815000010420E+03 0.32136300691216E+03 0.34017050940631E+03
 0.29815000000479E+03 0.29815000000826E+03 0.31853466966358E+03 0.34013683907349E+03 0.29815000000372E+03
 0.29815000000822E+03 0.32136300691216E+03 0.34017050940631E+03 0.29815000000479E+03 0.29815000000826E+03
 0.31853466966358E+03 0.34013683907349E+03 0.29815000000372E+03 0.29815000000822E+03 0.37902191644390E+03
 0.30674527960147E+03 0.17886341883624E+04 0.17101174466738E+04 0.63972888907282E+03 0.11849867742786E+04
 0.54205924076046E+03 0.10440506229980E+04 0.10173551121622E+04 0.98564576738248E+03 0.17197606300595E+04
 0.92596925579761E+03 0.10165657650879E+04 0.88294056857038E+03 0.17192853394342E+04 0.10440506229980E+04
 0.10173551121622E+04 0.98564576738248E+03 0.17197606300595E+04 0.92596925579761E+03 0.10165657650879E+04
 0.88294056857038E+03 0.17192853394342E+04 0.17083666335793E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47273681216990E+03 0.12948728128406E+01
 0.12948728128406E+01 0.12600737806481E+01 0.38577641553884E-01 0.30507436693740E+03 0.33260177195747E+03
 0.33189381038162E+03 0.33183171674941E+03 0.23000000000000E+00 0.00000000000000E+00 0.19820138741140E+00
 0.00000000000000E+00 -.15851184339781E+02 0.45863208314277E-02 0.60034538043772E+00 0.17443175682739E+04
 0.65411908810273E+03 0.13325662628014E+02 0.49971234855054E+01 0.30674268627466E+03 0.37904416747511E+03
 0.29987898420068E+03 0.30494173718751E+03 0.29815000000014E+03 0.29815000000107E+03 0.29986860401958E+03
 0.30494193847538E+03 0.29815000000014E+03 0.29815000000107E+03 0.29987898420068E+03 0.30494173718751E+03
 0.29815000000014E+03 0.29815000000107E+03 0.29986860401958E+03 0.30494193847538E+03 0.29815000000014E+03
 0.29815000000107E+03 0.30389685528880E+03 0.29815000001419E+03 0.10538425853414E+03 0.10186516119570E+03
 0.12798097584640E+03 0.37516319459042E+03 0.24654231386478E+03 0.10078097354309E+03 0.12056376175113E+03
 0.10078097354309E+03 0.29986558253513E+03 0.10048040272216E+03 0.12051042024846E+03 0.10048040272216E+03
 0.29981066668039E+03 0.10078097354309E+03 0.12056376175113E+03 0.10078097354309E+03 0.29986558253513E+03
 0.10048040272216E+03 0.12051042024846E+03 0.10048040272216E+03 0.29981066668039E+03 0.20290308916658E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35498497863187E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19652430176295E+00 0.00000000000000E+00 0.00000000000000E+00 0.19652430176295E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20539290157008E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20539290157008E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638761025477E+00 0.20783366501168E+00 0.33260177195747E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    370.00681763
 0.11754871516520E+00 0.30840475303962E+03 0.43415464673954E+03 0.42922740059401E+03 0.42732742572389E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17249534109471E+00 0.00000000000000E+00 -.22876841562811E+02
 0.30132562096392E-02 0.83905536675309E+00 0.26549352074373E+04 0.99560070278898E+03 0.95345317091026E+01
 0.35754493909135E+01 0.32605609325548E+03 0.29815000014742E+03 0.32183448971841E+03 0.34091393989235E+03
 0.29815000000702E+03 0.29815000001220E+03 0.31897196484870E+03 0.34088071445586E+03 0.29815000000545E+03
 0.29815000001214E+03 0.32183448971841E+03 0.34091393989235E+03 0.29815000000702E+03 0.29815000001220E+03
 0.31897196484870E+03 0.34088071445586E+03 0.29815000000545E+03 0.29815000001214E+03 0.37998475297649E+03
 0.30731786892489E+03 0.17947362890450E+04 0.17143706652967E+04 0.63853015118354E+03 0.11765077332865E+04
 0.53478493134701E+03 0.10481375738840E+04 0.10209360721448E+04 0.98846490649711E+03 0.17195398102900E+04
 0.93033386820989E+03 0.10201756737457E+04 0.88629112264430E+03 0.17190888070805E+04 0.10481375738840E+04
 0.10209360721448E+04 0.98846490649711E+03 0.17195398102900E+04 0.93033386820989E+03 0.10201756737457E+04
 0.88629112264430E+03 0.17190888070805E+04 0.17072824873642E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47308981849081E+03 0.12948730453636E+01
 0.12948730453636E+01 0.13000465200608E+01 0.31172615039820E-01 0.30587660471505E+03 0.33264037403210E+03
 0.33208417624681E+03 0.33203660760166E+03 0.23000000000000E+00 0.00000000000000E+00 0.19702261755993E+00
 0.00000000000000E+00 -.15857960993836E+02 0.56757967179920E-02 0.62304502190026E+00 0.14094937499506E+04
 0.52856015623148E+03 0.12840163581759E+02 0.48150613431597E+01 0.30731528686770E+03 0.38000575191123E+03
 0.29996119389758E+03 0.30509452129363E+03 0.29815000000016E+03 0.29815000000153E+03 0.29995104534140E+03
 0.30509460686368E+03 0.29815000000016E+03 0.29815000000153E+03 0.29996119389758E+03 0.30509452129363E+03
 0.29815000000016E+03 0.29815000000153E+03 0.29995104534140E+03 0.30509460686368E+03 0.29815000000016E+03
 0.29815000000153E+03 0.30402836143848E+03 0.29815000002031E+03 0.10550990051906E+03 0.10262625929545E+03
 0.13069568027015E+03 0.37704705169129E+03 0.24569789301979E+03 0.10383121741028E+03 0.12304271213348E+03
 0.10383121741028E+03 0.30131854264059E+03 0.10357438148203E+03 0.12298172405797E+03 0.10357438148203E+03
 0.30125688638270E+03 0.10383121741028E+03 0.12304271213348E+03 0.10383121741028E+03 0.30131854264059E+03
 0.10357438148203E+03 0.12298172405797E+03 0.10357438148203E+03 0.30125688638270E+03 0.20427803266426E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35517224845627E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19662134535669E+00 0.00000000000000E+00 0.00000000000000E+00 0.19662134535669E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20553207474581E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20553207474581E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638755468306E+00 0.20780947373105E+00 0.33264037403210E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    380.01330908
 0.11766552095153E+00 0.30864232677506E+03 0.43452446435099E+03 0.42958713525221E+03 0.42768285252513E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17155137669098E+00 0.00000000000000E+00 -.22907925714872E+02
 0.30102646727197E-02 0.85211548667495E+00 0.26575736255019E+04 0.99659010956321E+03 0.93883987852596E+01
 0.35206495444724E+01 0.32660369182839E+03 0.29815000020595E+03 0.32229994243508E+03 0.34164602816762E+03
 0.29815000001016E+03 0.29815000001777E+03 0.31940397640669E+03 0.34161323182749E+03 0.29815000000790E+03
 0.29815000001769E+03 0.32229994243508E+03 0.34164602816762E+03 0.29815000001016E+03 0.29815000001777E+03
 0.31940397640669E+03 0.34161323182749E+03 0.29815000000790E+03 0.29815000001769E+03 0.38092513193873E+03
 0.30790351577744E+03 0.18006485936042E+04 0.17184538386187E+04 0.63732839484139E+03 0.11683565248457E+04
 0.52784148803014E+03 0.10521119635689E+04 0.10243904115571E+04 0.99118378740976E+03 0.17193363144096E+04
 0.93458315439686E+03 0.10236570193040E+04 0.88953439869018E+03 0.17189078297038E+04 0.10521119635689E+04
 0.10243904115571E+04 0.99118378740976E+03 0.17193363144096E+04 0.93458315439686E+03 0.10236570193040E+04
 0.88953439869018E+03 0.17189078297038E+04 0.17062449334313E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47344659346002E+03 0.12948732744025E+01
 0.12948732744025E+01 0.13400724858729E+01 0.24814261373201E-01 0.30669020322966E+03 0.33269418104527E+03
 0.33226400137710E+03 0.33222818047880E+03 0.23000000000000E+00 0.00000000000000E+00 0.19583835138858E+00
 0.00000000000000E+00 -.15865398589145E+02 0.71301502855945E-02 0.64579339091464E+00 0.11219959859981E+04
 0.42074849474928E+03 0.12387862917999E+02 0.46454485942495E+01 0.30790095713398E+03 0.38094491167298E+03
 0.30004488458605E+03 0.30524609343114E+03 0.29815000000018E+03 0.29815000000220E+03 0.30003501647379E+03
 0.30524606200721E+03 0.29815000000019E+03 0.29815000000220E+03 0.30004488458605E+03 0.30524609343114E+03
 0.29815000000018E+03 0.29815000000220E+03 0.30003501647379E+03 0.30524606200721E+03 0.29815000000019E+03
 0.29815000000220E+03 0.30415891153672E+03 0.29815000002870E+03 0.10543460470011E+03 0.10314734712598E+03
 0.13333367769317E+03 0.37904705240057E+03 0.24504670631893E+03 0.10686248478454E+03 0.12545256177869E+03
 0.10686248478454E+03 0.30282294983646E+03 0.10664750099486E+03 0.12538433623261E+03 0.10664750099486E+03
 0.30275496928801E+03 0.10686248478454E+03 0.12545256177869E+03 0.10686248478454E+03 0.30282294983646E+03
 0.10664750099486E+03 0.12538433623261E+03 0.10664750099486E+03 0.30275496928801E+03 0.20559256850963E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35546022522869E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19671354266320E+00 0.00000000000000E+00 0.00000000000000E+00 0.19671354266320E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20565541775756E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20565541775756E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638739751365E+00 0.20777568292287E+00 0.33269418104527E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    390.01130134
 0.11777506287910E+00 0.30887669185410E+03 0.43489643371496E+03 0.42994910604107E+03 0.42804044109518E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17064168065794E+00 0.00000000000000E+00 -.22938471478470E+02
 0.30074645439099E-02 0.86464952923626E+00 0.26600479850045E+04 0.99751799437670E+03 0.92523036554087E+01
 0.34696138707783E+01 0.32714295790542E+03 0.29815000028415E+03 0.32275871871368E+03 0.34236590257939E+03
 0.29815000001454E+03 0.29815000002556E+03 0.31983009560976E+03 0.34233351929617E+03 0.29815000001130E+03
 0.29815000002543E+03 0.32275871871368E+03 0.34236590257939E+03 0.29815000001454E+03 0.29815000002556E+03
 0.31983009560976E+03 0.34233351929617E+03 0.29815000001130E+03 0.29815000002543E+03 0.38184256570084E+03
 0.30849954295868E+03 0.18063717164219E+04 0.17223700967376E+04 0.63612570853563E+03 0.11605213029291E+04
 0.52121496585084E+03 0.10559733282351E+04 0.10277190575760E+04 0.99380378489289E+03 0.17191457291531E+04
 0.93871612963863E+03 0.10270108279294E+04 0.89267088341774E+03 0.17187380940755E+04 0.10559733282351E+04
 0.10277190575760E+04 0.99380378489289E+03 0.17191457291531E+04 0.93871612963863E+03 0.10270108279294E+04
 0.89267088341774E+03 0.17187380940755E+04 0.17052490684077E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47380558444054E+03 0.12948734994745E+01
 0.12948734994745E+01 0.13800644548988E+01 0.19741736153904E-01 0.30749627660557E+03 0.33276564012869E+03
 0.33243306605706E+03 0.33240612469148E+03 0.23000000000000E+00 0.00000000000000E+00 0.19464521837232E+00
 0.00000000000000E+00 -.15873859670788E+02 0.89622006848226E-02 0.66864568834285E+00 0.89263790014744E+03
 0.33473921255529E+03 0.11964483043070E+02 0.44866811411513E+01 0.30849701839868E+03 0.38186116621141E+03
 0.30012970230287E+03 0.30539665115549E+03 0.29815000000022E+03 0.29815000000313E+03 0.30012013850746E+03
 0.30539650260379E+03 0.29815000000022E+03 0.29815000000313E+03 0.30012970230287E+03 0.30539665115549E+03
 0.29815000000022E+03 0.29815000000313E+03 0.30012013850746E+03 0.30539650260379E+03 0.29815000000022E+03
 0.29815000000313E+03 0.30428860571833E+03 0.29815000004007E+03 0.10517690442516E+03 0.10340573372759E+03
 0.13591276518392E+03 0.38115650889552E+03 0.24456417988568E+03 0.10986771698594E+03 0.12781144191595E+03
 0.10986771698594E+03 0.30444407981076E+03 0.10968926561014E+03 0.12773646484107E+03 0.10968926561014E+03
 0.30437025942551E+03 0.10986771698594E+03 0.12781144191595E+03 0.10986771698594E+03 0.30444407981076E+03
 0.10968926561014E+03 0.12773646484107E+03 0.10968926561014E+03 0.30437025942551E+03 0.20685088343542E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35575000093318E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19680103259083E+00 0.00000000000000E+00 0.00000000000000E+00 0.19680103259083E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20576474435059E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20576474435059E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638711697945E+00 0.20773075046119E+00 0.33276564012869E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    400.01147982
 0.11787575343831E+00 0.30910848643626E+03 0.43527035481319E+03 0.43031321304982E+03 0.42840014470274E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16976408636892E+00 0.00000000000000E+00 -.22966613941762E+02
 0.30048952402096E-02 0.87669063752518E+00 0.26623224307287E+04 0.99837091152326E+03 0.91252257724381E+01
 0.34219596646643E+01 0.32767490423044E+03 0.29815000038757E+03 0.32321165398767E+03 0.34307499187957E+03
 0.29815000002055E+03 0.29815000003630E+03 0.32025108963204E+03 0.34304300676139E+03 0.29815000001598E+03
 0.29815000003613E+03 0.32321165398767E+03 0.34307499187957E+03 0.29815000002055E+03 0.29815000003630E+03
 0.32025108963204E+03 0.34304300676139E+03 0.29815000001598E+03 0.29815000003613E+03 0.38273948496465E+03
 0.30910493388477E+03 0.18119266085385E+04 0.17261387661203E+04 0.63492070085028E+03 0.11529676030656E+04
 0.51487229871111E+03 0.10597342891357E+04 0.10309339343638E+04 0.99633654148748E+03 0.17189644077985E+04
 0.94274586940146E+03 0.10302491985099E+04 0.89571308760928E+03 0.17185761149306E+04 0.10597342891357E+04
 0.10309339343638E+04 0.99633654148748E+03 0.17189644077985E+04 0.94274586940146E+03 0.10302491985099E+04
 0.89571308760928E+03 0.17185761149306E+04 0.17042896985460E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47416671708956E+03 0.12948737068381E+01
 0.12948737068381E+01 0.14200651688183E+01 0.15701029141684E-01 0.30828300950709E+03 0.33284882179811E+03
 0.33259168277499E+03 0.33257142379564E+03 0.23000000000000E+00 0.00000000000000E+00 0.19344782512131E+00
 0.00000000000000E+00 -.15880868605028E+02 0.11268649311024E-01 0.69152596973582E+00 0.70993424138009E+03
 0.26622534051753E+03 0.11568618316759E+02 0.43382318687845E+01 0.30910245475969E+03 0.38275694812314E+03
 0.30021557994074E+03 0.30554649451359E+03 0.29815000000027E+03 0.29815000000441E+03 0.30020633029250E+03
 0.30554622924382E+03 0.29815000000027E+03 0.29815000000441E+03 0.30021557994074E+03 0.30554649451359E+03
 0.29815000000027E+03 0.29815000000441E+03 0.30020633029250E+03 0.30554622924382E+03 0.29815000000027E+03
 0.29815000000441E+03 0.30441758082928E+03 0.29815000005528E+03 0.10473014603000E+03 0.10337559647602E+03
 0.13841178868674E+03 0.38328673397574E+03 0.24418288634556E+03 0.11283779547476E+03 0.13009661662497E+03
 0.11283779547476E+03 0.30611202126043E+03 0.11269105948648E+03 0.13001535248970E+03 0.11269105948648E+03
 0.30603282039689E+03 0.11283779547476E+03 0.13009661662497E+03 0.11283779547476E+03 0.30611202126042E+03
 0.11269105948648E+03 0.13001535248970E+03 0.11269105948648E+03 0.30603282039689E+03 0.20801605590297E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35601638651323E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19686985231335E+00 0.00000000000000E+00 0.00000000000000E+00 0.19686985231335E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20585180636665E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20585180636665E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638681924975E+00 0.20767850760984E+00 0.33284882179811E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    410.85332947
 0.11797561409622E+00 0.30935453533879E+03 0.43567629657305E+03 0.43070866745461E+03 0.42879075523425E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16884781298602E+00 0.00000000000000E+00 -.23000175432178E+02
 0.30023514169121E-02 0.88920867370519E+00 0.26645781552874E+04 0.99921680823276E+03 0.89967633431479E+01
 0.33737862536805E+01 0.32824273286043E+03 0.29815000053825E+03 0.32369546580418E+03 0.34383202359068E+03
 0.29815000002968E+03 0.29815000005269E+03 0.32070092290493E+03 0.34380044888255E+03 0.29815000002312E+03
 0.29815000005243E+03 0.32369546580418E+03 0.34383202359068E+03 0.29815000002968E+03 0.29815000005269E+03
 0.32070092290493E+03 0.34380044888255E+03 0.29815000002312E+03 0.29815000005243E+03 0.38369485507697E+03
 0.30977063887894E+03 0.18177664582857E+04 0.17300548601982E+04 0.63354154202958E+03 0.11449329509928E+04
 0.50822370125308E+03 0.10637022771519E+04 0.10342863387377E+04 0.99897819419942E+03 0.17187549675685E+04
 0.94700286094250E+03 0.10336253404732E+04 0.89890135679716E+03 0.17183861321953E+04 0.10637022771519E+04
 0.10342863387377E+04 0.99897819419942E+03 0.17187549675685E+04 0.94700286094249E+03 0.10336253404732E+04
 0.89890135679716E+03 0.17183861321953E+04 0.17032372643457E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47455893648275E+03 0.12948739541312E+01
 0.12948739541312E+01 0.14634325674388E+01 0.12245171776975E-01 0.30911368664406E+03 0.33294743138885E+03
 0.33275286585652E+03 0.33273799599950E+03 0.23000000000000E+00 0.00000000000000E+00 0.19215123327375E+00
 0.00000000000000E+00 -.15892605988631E+02 0.14448909733190E-01 0.71625198683574E+00 0.55367499331964E+03
 0.20762812249486E+03 0.11169253484856E+02 0.41884700568209E+01 0.30976822143302E+03 0.38371115509121E+03
 0.30030888710057E+03 0.30570758130458E+03 0.29815000000034E+03 0.29815000000638E+03 0.30029997166017E+03
 0.30570719192759E+03 0.29815000000034E+03 0.29815000000638E+03 0.30030888710057E+03 0.30570758130458E+03
 0.29815000000034E+03 0.29815000000638E+03 0.30029997166017E+03 0.30570719192759E+03 0.29815000000034E+03
 0.29815000000638E+03 0.30455602517170E+03 0.29815000007772E+03 0.10402704346198E+03 0.10302496076960E+03
 0.14101354156259E+03 0.38556696308611E+03 0.24384835381570E+03 0.11600931884340E+03 0.13247334354858E+03
 0.11600931884340E+03 0.30791807834828E+03 0.11589213921354E+03 0.13238576589633E+03 0.11589213921354E+03
 0.30783352627845E+03 0.11600931884340E+03 0.13247334354858E+03 0.11600931884340E+03 0.30791807834828E+03
 0.11589213921354E+03 0.13238576589633E+03 0.11589213921354E+03 0.30783352627845E+03 0.20915731587394E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35628276176567E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19696732338717E+00 0.00000000000000E+00 0.00000000000000E+00 0.19696732338717E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20593247112900E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20593247112900E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638630795885E+00 0.20761643453248E+00 0.33294743138885E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    420.58952054
 0.11805027384676E+00 0.30957401347418E+03 0.43604161043629E+03 0.43106509895159E+03 0.42914298554152E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16805497693030E+00 0.00000000000000E+00 -.23016819581493E+02
 0.30004523162721E-02 0.89999261474388E+00 0.26662646683682E+04 0.99984925063808E+03 0.88889618302886E+01
 0.33333606863582E+01 0.32874598247225E+03 0.29815000071669E+03 0.32412462776574E+03 0.34450157974708E+03
 0.29815000004094E+03 0.29815000007295E+03 0.32110031000974E+03 0.34447036182288E+03 0.29815000003192E+03
 0.29815000007260E+03 0.32412462776574E+03 0.34450157974708E+03 0.29815000004094E+03 0.29815000007295E+03
 0.32110031000974E+03 0.34447036182288E+03 0.29815000003192E+03 0.29815000007260E+03 0.38453143284460E+03
 0.31037538551013E+03 0.18228654347679E+04 0.17334507417822E+04 0.63233451184969E+03 0.11380264207747E+04
 0.50253023636572E+03 0.10671777298446E+04 0.10371977209143E+04 0.10012787827976E+04 0.17185748659260E+04
 0.95073483728500E+03 0.10365566336659E+04 0.90168533930842E+03 0.17182222271938E+04 0.10671777298446E+04
 0.10371977209143E+04 0.10012787827976E+04 0.17185748659260E+04 0.95073483728500E+03 0.10365566336659E+04
 0.90168533930842E+03 0.17182222271938E+04 0.17023421731307E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47491255313813E+03 0.12948740767713E+01
 0.12948740767713E+01 0.15023773317228E+01 0.97911297983426E-02 0.30983758742681E+03 0.33304122615773E+03
 0.33288976626532E+03 0.33287850302697E+03 0.23000000000000E+00 0.00000000000000E+00 0.19099264243237E+00
 0.00000000000000E+00 -.15889166339514E+02 0.18070373663821E-01 0.73830777427470E+00 0.44271359014657E+03
 0.16601759630496E+03 0.10835589545104E+02 0.40633460794140E+01 0.31037303111931E+03 0.38454673117964E+03
 0.30039377887692E+03 0.30585144206610E+03 0.29815000000044E+03 0.29815000000882E+03 0.30038516238650E+03
 0.30585094184879E+03 0.29815000000044E+03 0.29815000000882E+03 0.30039377887692E+03 0.30585144206610E+03
 0.29815000000044E+03 0.29815000000882E+03 0.30038516238650E+03 0.30585094184879E+03 0.29815000000044E+03
 0.29815000000882E+03 0.30467958757235E+03 0.29815000010462E+03 0.10321045892657E+03 0.10244498303514E+03
 0.14324946344917E+03 0.38755756124920E+03 0.24359185048278E+03 0.11881488819730E+03 0.13451215365223E+03
 0.11881488819730E+03 0.30950423611151E+03 0.11872044101113E+03 0.13441928250048E+03 0.11872044101113E+03
 0.30941524877953E+03 0.11881488819730E+03 0.13451215365223E+03 0.11881488819730E+03 0.30950423611151E+03
 0.11872044101113E+03 0.13441928250048E+03 0.11872044101113E+03 0.30941524877953E+03 0.21006778587389E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35650397821377E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19695331212485E+00 0.00000000000000E+00 0.00000000000000E+00 0.19695331212485E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20595269653161E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20595269653161E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638627510406E+00 0.20755793561206E+00 0.33304122615773E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    431.59887377
 0.11810844411760E+00 0.30981986328416E+03 0.43645529751474E+03 0.43146972614570E+03 0.42954315815618E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16719126951973E+00 0.00000000000000E+00 -.23042038211140E+02
 0.29989742135026E-02 0.91168715698767E+00 0.26675787887674E+04 0.10003420457878E+04 0.87749398888463E+01
 0.32906024583174E+01 0.32930687706088E+03 0.29815000098357E+03 0.32460322925640E+03 0.34524783428194E+03
 0.29815000005848E+03 0.29815000010462E+03 0.32154583645055E+03 0.34521700028059E+03 0.29815000004568E+03
 0.29815000010413E+03 0.32460322925640E+03 0.34524783428194E+03 0.29815000005848E+03 0.29815000010462E+03
 0.32154583645055E+03 0.34521700028059E+03 0.29815000004568E+03 0.29815000010413E+03 0.38546283614533E+03
 0.31106667187361E+03 0.18284959543621E+04 0.17371833062921E+04 0.63091028128778E+03 0.11303541259043E+04
 0.49628929321005E+03 0.10710233408581E+04 0.10403877401291E+04 0.10038135148773E+04 0.17183801385086E+04
 0.95486869295997E+03 0.10397676901420E+04 0.90476131125125E+03 0.17180445384795E+04 0.10710233408581E+04
 0.10403877401291E+04 0.10038135148773E+04 0.17183801385086E+04 0.95486869295997E+03 0.10397676901420E+04
 0.90476131125125E+03 0.17180445384795E+04 0.17013445112415E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47531416993716E+03 0.12948742625913E+01
 0.12948742625913E+01 0.15464147446519E+01 0.76016196454587E-02 0.31063651391411E+03 0.33315138041923E+03
 0.33303728078487E+03 0.33302905579931E+03 0.23000000000000E+00 0.00000000000000E+00 0.18969348363786E+00
 0.00000000000000E+00 -.15891792375918E+02 0.23275219561030E-01 0.76300012016860E+00 0.34371319157799E+03
 0.12889244684175E+03 0.10484926264798E+02 0.39318473492994E+01 0.31106439627406E+03 0.38547704472482E+03
 0.30048968572087E+03 0.30601242941684E+03 0.29815000000061E+03 0.29815000001266E+03 0.30048139136011E+03
 0.30601180684034E+03 0.29815000000060E+03 0.29815000001266E+03 0.30048968572087E+03 0.30601242941684E+03
 0.29815000000061E+03 0.29815000001266E+03 0.30048139136011E+03 0.30601180684034E+03 0.29815000000060E+03
 0.29815000001266E+03 0.30481774123954E+03 0.29815000014534E+03 0.10207875279668E+03 0.10151209095214E+03
 0.14566008838131E+03 0.38973464365997E+03 0.24334625483675E+03 0.12193284193454E+03 0.13670668778985E+03
 0.12193284193454E+03 0.31124531404480E+03 0.12186027485582E+03 0.13660827950382E+03 0.12186027485582E+03
 0.31115173589028E+03 0.12193284193454E+03 0.13670668778985E+03 0.12193284193454E+03 0.31124531404480E+03
 0.12186027485582E+03 0.13660827950382E+03 0.12186027485582E+03 0.31115173589028E+03 0.21097377763784E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35673854856350E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19698493031729E+00 0.00000000000000E+00 0.00000000000000E+00 0.19698493031729E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20595011543376E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20595011543376E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638599471439E+00 0.20748900635556E+00 0.33315138041923E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    440.51861939
 0.11813521686945E+00 0.31002027454524E+03 0.43679164003671E+03 0.43179958578499E+03 0.42986972821158E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16651609586720E+00 0.00000000000000E+00 -.23044161466879E+02
 0.29982942899501E-02 0.92078641929235E+00 0.26681837159264E+04 0.10005688934724E+04 0.86882254477083E+01
 0.32580845428906E+01 0.32975730454598E+03 0.29815000125832E+03 0.32498802126673E+03 0.34584405162120E+03
 0.29815000007718E+03 0.29815000013849E+03 0.32190469219260E+03 0.34581352688513E+03 0.29815000006036E+03
 0.29815000013785E+03 0.32498802126673E+03 0.34584405162120E+03 0.29815000007718E+03 0.29815000013849E+03
 0.32190469219260E+03 0.34581352688513E+03 0.29815000006036E+03 0.29815000013785E+03 0.38619180234280E+03
 0.31162959139179E+03 0.18329542282798E+04 0.17401308736170E+04 0.62990205782241E+03 0.11246107571623E+04
 0.49155918905081E+03 0.10740757410391E+04 0.10429110393822E+04 0.10058226515045E+04 0.17182527492414E+04
 0.95815079504604E+03 0.10423069780978E+04 0.90719983393260E+03 0.17179299311752E+04 0.10740757410391E+04
 0.10429110393822E+04 0.10058226515045E+04 0.17182527492414E+04 0.95815079504604E+03 0.10423069780978E+04
 0.90719983393260E+03 0.17179299311752E+04 0.17006505324351E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47564175913515E+03 0.12948742782362E+01
 0.12948742782362E+01 0.15820937271274E+01 0.61922111045405E-02 0.31126825804384E+03 0.33324371047143E+03
 0.33315299271106E+03 0.33314661636007E+03 0.23000000000000E+00 0.00000000000000E+00 0.18865192825718E+00
 0.00000000000000E+00 -.15875873395762E+02 0.28572888983201E-01 0.78276642062568E+00 0.27998568869614E+03
 0.10499463326105E+03 0.10220162476573E+02 0.38325609287149E+01 0.31162738451391E+03 0.38620516920501E+03
 0.30056967070619E+03 0.30614282306691E+03 0.29815000000079E+03 0.29815000001677E+03 0.30056163897397E+03
 0.30614210023318E+03 0.29815000000078E+03 0.29815000001677E+03 0.30056967070619E+03 0.30614282306691E+03
 0.29815000000079E+03 0.29815000001677E+03 0.30056163897397E+03 0.30614210023318E+03 0.29815000000078E+03
 0.29815000001677E+03 0.30492964225453E+03 0.29815000018769E+03 0.10102763327528E+03 0.10057962278570E+03
 0.14752634302032E+03 0.39143534379359E+03 0.24317136905817E+03 0.12442494443297E+03 0.13840047680059E+03
 0.12442494443297E+03 0.31260497488872E+03 0.12436743068990E+03 0.13829783250204E+03 0.12436743068990E+03
 0.31250793543511E+03 0.12442494443297E+03 0.13840047680059E+03 0.12442494443297E+03 0.31260497488872E+03
 0.12436743068990E+03 0.13829783250204E+03 0.12436743068990E+03 0.31250793543511E+03 0.21162040919649E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35692002164652E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19687252719290E+00 0.00000000000000E+00 0.00000000000000E+00 0.19687252719290E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20594119049597E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20594119049597E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638634342564E+00 0.20743191941627E+00 0.33324371047143E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    450.27797519
 0.11813578793505E+00 0.31024037849423E+03 0.43716136848198E+03 0.43216339809608E+03 0.43023036030580E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16580129526281E+00 0.00000000000000E+00 -.23060250524068E+02
 0.29982794952021E-02 0.93037535192814E+00 0.26681968818456E+04 0.10005738306921E+04 0.85986800740374E+01
 0.32245050277640E+01 0.33024530925806E+03 0.29815000162508E+03 0.32540514831234E+03 0.34648960262526E+03
 0.29815000010286E+03 0.29815000018511E+03 0.32229386920833E+03 0.34645940432763E+03 0.29815000008057E+03
 0.29815000018427E+03 0.32540514831234E+03 0.34648960262526E+03 0.29815000010286E+03 0.29815000018511E+03
 0.32229386920833E+03 0.34645940432763E+03 0.29815000008057E+03 0.29815000018427E+03 0.38698055540215E+03
 0.31224884178488E+03 0.18377643362908E+04 0.17433179450000E+04 0.62876781761533E+03 0.11184315990916E+04
 0.48651994238824E+03 0.10773713871125E+04 0.10456200059849E+04 0.10079978146429E+04 0.17181424291107E+04
 0.96169679882887E+03 0.10450324126552E+04 0.90984092110133E+03 0.17178326953128E+04 0.10773713871125E+04
 0.10456200059849E+04 0.10079978146429E+04 0.17181424291107E+04 0.96169679882887E+03 0.10450324126552E+04
 0.90984092110133E+03 0.17178326953128E+04 0.16999275731162E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47600331052228E+03 0.12948743967863E+01
 0.12948743967863E+01 0.16211311503302E+01 0.49460084222908E-02 0.31195103281832E+03 0.33334741289385E+03
 0.33327686177647E+03 0.33327203979896E+03 0.23000000000000E+00 0.00000000000000E+00 0.18752594629201E+00
 0.00000000000000E+00 -.15872045505907E+02 0.35772149853343E-01 0.80410535221352E+00 0.22363766317647E+03
 0.83864123691176E+02 0.99489450952886E+01 0.37308544107332E+01 0.31224672568008E+03 0.38699297129659E+03
 0.30065732142069E+03 0.30628438484895E+03 0.29815000000105E+03 0.29815000002245E+03 0.30064956257762E+03
 0.30628355464169E+03 0.29815000000104E+03 0.29815000002246E+03 0.30065732142069E+03 0.30628438484895E+03
 0.29815000000105E+03 0.29815000002245E+03 0.30064956257762E+03 0.30628355464169E+03 0.29815000000104E+03
 0.29815000002246E+03 0.30505107959044E+03 0.29815000024469E+03 0.99740648774891E+02 0.99395428457086E+02
 0.14947901117002E+03 0.39323738325793E+03 0.24301097703205E+03 0.12710353833500E+03 0.14017016536738E+03
 0.12710353833500E+03 0.31404507581347E+03 0.12706015035941E+03 0.14006325481924E+03 0.12706015035941E+03
 0.31394459918095E+03 0.12710353833500E+03 0.14017016536738E+03 0.12710353833500E+03 0.31404507581347E+03
 0.12706015035941E+03 0.14006325481924E+03 0.12706015035941E+03 0.31394459918095E+03 0.21224844006485E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35711155031548E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19685163110747E+00 0.00000000000000E+00 0.00000000000000E+00 0.19685163110747E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20591508206445E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20591508206445E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638626208999E+00 0.20736731051873E+00 0.33334741289385E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    461.91227768
 0.11812967772788E+00 0.31049463190507E+03 0.43760074925838E+03 0.43259574769831E+03 0.43065876834807E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16498062146734E+00 0.00000000000000E+00 -.23081345405976E+02
 0.29984342230564E-02 0.94133258328450E+00 0.26680591951907E+04 0.10005221981965E+04 0.84985903410316E+01
 0.31869713778869E+01 0.33081905908880E+03 0.29815000219229E+03 0.32589577984329E+03 0.34724904729434E+03
 0.29815000014411E+03 0.29815000026021E+03 0.32275171422303E+03 0.34721921854143E+03 0.29815000011309E+03
 0.29815000025904E+03 0.32589577984329E+03 0.34724904729434E+03 0.29815000014411E+03 0.29815000026021E+03
 0.32275171422303E+03 0.34721921854143E+03 0.29815000011309E+03 0.29815000025904E+03 0.38790951757577E+03
 0.31299303512482E+03 0.18433595807761E+04 0.17469686746752E+04 0.62732237128318E+03 0.11111047954281E+04
 0.48064581228848E+03 0.10812164880405E+04 0.10487428781931E+04 0.10104925422999E+04 0.17179917762071E+04
 0.96583837756354E+03 0.10481736455279E+04 0.91288758807616E+03 0.17176965684404E+04 0.10812164880405E+04
 0.10487428781931E+04 0.10104925422999E+04 0.17179917762071E+04 0.96583837756354E+03 0.10481736455279E+04
 0.91288758807616E+03 0.17176965684404E+04 0.16990269038671E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47643291826343E+03 0.12948745522211E+01
 0.12948745522211E+01 0.16676683602794E+01 0.37821486725260E-02 0.31276053770852E+03 0.33347484548599E+03
 0.33342261575821E+03 0.33341916585199E+03 0.23000000000000E+00 0.00000000000000E+00 0.18620454169145E+00
 0.00000000000000E+00 -.15869779301107E+02 0.46780114423553E-01 0.82910725912952E+00 0.17101283523095E+03
 0.64129813211606E+02 0.96489325258100E+01 0.36183496971787E+01 0.31299101901233E+03 0.38792096978934E+03
 0.30076135729927E+03 0.30645115794337E+03 0.29815000000149E+03 0.29815000003164E+03 0.30075389942284E+03
 0.30645020357256E+03 0.29815000000147E+03 0.29815000003165E+03 0.30076135729927E+03 0.30645115794337E+03
 0.29815000000149E+03 0.29815000003164E+03 0.30075389942284E+03 0.30645020357256E+03 0.29815000000147E+03
 0.29815000003165E+03 0.30519404744408E+03 0.29815000033381E+03 0.98021775820087E+02 0.97774573949845E+02
 0.15170220155434E+03 0.39533086279584E+03 0.24287015023372E+03 0.13025666751901E+03 0.14218066488902E+03
 0.13025666751901E+03 0.31571510614350E+03 0.13022736331613E+03 0.14206900570804E+03 0.13022736331613E+03
 0.31561083894554E+03 0.13025666751901E+03 0.14218066488902E+03 0.13025666751901E+03 0.31571510614350E+03
 0.13022736331613E+03 0.14206900570804E+03 0.13022736331613E+03 0.31561083894554E+03 0.21290706100246E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35733303346857E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19684198682485E+00 0.00000000000000E+00 0.00000000000000E+00 0.19684198682485E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20587763976376E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20587763976376E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638607124222E+00 0.20728787129465E+00 0.33347484548599E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    471.21971967
 0.11810672228464E+00 0.31069817930198E+03 0.43795235499443E+03 0.43294249713174E+03 0.43100264579837E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16434777782953E+00 0.00000000000000E+00 -.23083512953369E+02
 0.29990167175356E-02 0.94973943705076E+00 0.26675409820902E+04 0.10003278682838E+04 0.84233629645227E+01
 0.31587611116960E+01 0.33127326354111E+03 0.29815000276280E+03 0.32628448117124E+03 0.34784884327537E+03
 0.29815000018700E+03 0.29815000033849E+03 0.32311481019396E+03 0.34781930180851E+03 0.29815000014697E+03
 0.29815000033700E+03 0.32628448117124E+03 0.34784884327537E+03 0.29815000018700E+03 0.29815000033849E+03
 0.32311481019396E+03 0.34781930180851E+03 0.29815000014697E+03 0.29815000033700E+03 0.38863678871029E+03
 0.31359075650480E+03 0.18477493432823E+04 0.17498283756979E+04 0.62620574213745E+03 0.11054817465144E+04
 0.47614497566625E+03 0.10842389707605E+04 0.10511824116858E+04 0.10124523232994E+04 0.17178887802447E+04
 0.96909551265484E+03 0.10506269499890E+04 0.91528242434992E+03 0.17176043669680E+04 0.10842389707605E+04
 0.10511824116858E+04 0.10124523232994E+04 0.17178887802447E+04 0.96909551265484E+03 0.10506269499890E+04
 0.91528242434992E+03 0.17176043669680E+04 0.16983606545774E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47677762350156E+03 0.12948745681924E+01
 0.12948745681924E+01 0.17048981282388E+01 0.30510397718087E-02 0.31339984977518E+03 0.33357982547117E+03
 0.33353877886554E+03 0.33353614157510E+03 0.23000000000000E+00 0.00000000000000E+00 0.18516531953550E+00
 0.00000000000000E+00 -.15851627426036E+02 0.57989851215899E-01 0.84873820226583E+00 0.13795517374610E+03
 0.51733190154788E+02 0.94257569397052E+01 0.35346588523894E+01 0.31358882694164E+03 0.38864748931084E+03
 0.30084552647777E+03 0.30658375816790E+03 0.29815000000197E+03 0.29815000004125E+03 0.30083829753025E+03
 0.30658270576318E+03 0.29815000000194E+03 0.29815000004126E+03 0.30084552647777E+03 0.30658375816790E+03
 0.29815000000197E+03 0.29815000004125E+03 0.30083829753025E+03 0.30658270576318E+03 0.29815000000194E+03
 0.29815000004126E+03 0.30530771941472E+03 0.29815000042429E+03 0.96525451914065E+02 0.96336011280947E+02
 0.15340270511698E+03 0.39695652576653E+03 0.24278680712397E+03 0.13274648931201E+03 0.14371469783029E+03
 0.13274648931201E+03 0.31700666764688E+03 0.13272655946653E+03 0.14359949226999E+03 0.13272655946653E+03
 0.31689960996786E+03 0.13274648931201E+03 0.14371469783029E+03 0.13274648931201E+03 0.31700666764688E+03
 0.13272655946653E+03 0.14359949226999E+03 0.13272655946653E+03 0.31689960996786E+03 0.21336694650392E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35750391950143E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19672366182981E+00 0.00000000000000E+00 0.00000000000000E+00 0.19672366182981E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20578127966277E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20578127966277E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638644415432E+00 0.20722306693240E+00 0.33357982547117E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    480.47897543
 0.11805790567958E+00 0.31090340450465E+03 0.43830341731223E+03 0.43328989108036E+03 0.43134764677125E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16373818318960E+00 0.00000000000000E+00 -.23092479544305E+02
 0.30002565136901E-02 0.95779763192612E+00 0.26664386739921E+04 0.99991450274704E+03 0.83524950713357E+01
 0.31321856517509E+01 0.33172183047310E+03 0.29815000344190E+03 0.32666866729144E+03 0.34843922530948E+03
 0.29815000023939E+03 0.29815000043431E+03 0.32347412677347E+03 0.34840996475180E+03 0.29815000018843E+03
 0.29815000043243E+03 0.32666866729144E+03 0.34843922530948E+03 0.29815000023939E+03 0.29815000043431E+03
 0.32347412677347E+03 0.34840996475180E+03 0.29815000018843E+03 0.29815000043243E+03 0.38934521295903E+03
 0.31418588859805E+03 0.18520656476473E+04 0.17526564946528E+04 0.62517153632158E+03 0.11001644832391E+04
 0.47186708923588E+03 0.10872123782295E+04 0.10535795566800E+04 0.10143935728222E+04 0.17178283860469E+04
 0.97230003114748E+03 0.10530370416632E+04 0.91764992874217E+03 0.17175540157573E+04 0.10872123782295E+04
 0.10535795566800E+04 0.10143935728222E+04 0.17178283860469E+04 0.97230003114748E+03 0.10530370416632E+04
 0.91764992874217E+03 0.17175540157573E+04 0.16977878924523E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47712321994919E+03 0.12948746342615E+01
 0.12948746342615E+01 0.17419351512838E+01 0.24644409431351E-02 0.31402855843834E+03 0.33368693320830E+03
 0.33365463527253E+03 0.33365261693196E+03 0.23000000000000E+00 0.00000000000000E+00 0.18414843003300E+00
 0.00000000000000E+00 -.15841436640954E+02 0.71792889903704E-01 0.86791874017804E+00 0.11143164748947E+03
 0.41786867808552E+02 0.92174527748519E+01 0.34565447905695E+01 0.31418405614467E+03 0.38935516346288E+03
 0.30093055452714E+03 0.30671524133453E+03 0.29815000000257E+03 0.29815000005306E+03 0.30092354453467E+03
 0.30671409213228E+03 0.29815000000254E+03 0.29815000005307E+03 0.30093055452714E+03 0.30671524133453E+03
 0.29815000000257E+03 0.29815000005306E+03 0.30092354453467E+03 0.30671409213228E+03 0.29815000000254E+03
 0.29815000005307E+03 0.30542046920844E+03 0.29815000053277E+03 0.94946367300396E+02 0.94800499756354E+02
 0.15502879929627E+03 0.39853826993996E+03 0.24273432664722E+03 0.13519204708789E+03 0.14517843034976E+03
 0.13519204708789E+03 0.31825919076179E+03 0.13517998628283E+03 0.14505992323938E+03 0.13517998628283E+03
 0.31814957773375E+03 0.13519204708789E+03 0.14517843034976E+03 0.13519204708789E+03 0.31825919076179E+03
 0.13517998628283E+03 0.14505992323938E+03 0.13517998628283E+03 0.31814957773375E+03 0.21377569599238E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35767396206600E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19665605353423E+00 0.00000000000000E+00 0.00000000000000E+00 0.19665605353423E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20572102245393E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20572102245393E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638654597038E+00 0.20715668015525E+00 0.33368693320830E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    490.58181657
 0.11799926028650E+00 0.31112272353893E+03 0.43868553246897E+03 0.43366809343770E+03 0.43172318420177E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16309476611435E+00 0.00000000000000E+00 -.23101863201478E+02
 0.30017473212593E-02 0.96626349515160E+00 0.26651143963190E+04 0.99941789861961E+03 0.82793151558984E+01
 0.31047431834619E+01 0.33220607162147E+03 0.29815000434803E+03 0.32708356843842E+03 0.34907667303058E+03
 0.29815000031137E+03 0.29815000056620E+03 0.32386227104099E+03 0.34904770659058E+03 0.29815000024548E+03
 0.29815000056379E+03 0.32708356843842E+03 0.34907667303058E+03 0.29815000031137E+03 0.29815000056620E+03
 0.32386227104099E+03 0.34904770659058E+03 0.29815000024548E+03 0.29815000056379E+03 0.39011101062672E+03
 0.31483818389604E+03 0.18566923137198E+04 0.17556572093119E+04 0.62398381706997E+03 0.10943869644843E+04
 0.46728322832900E+03 0.10904065838845E+04 0.10561317540794E+04 0.10164554709531E+04 0.17177570203647E+04
 0.97574501681545E+03 0.10556025630713E+04 0.92017416560625E+03 0.17174929317714E+04 0.10904065838845E+04
 0.10561317540794E+04 0.10164554709531E+04 0.17177570203647E+04 0.97574501681545E+03 0.10556025630713E+04
 0.92017416560625E+03 0.17174929317714E+04 0.16971444230097E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47749944960335E+03 0.12948747034038E+01
 0.12948747034038E+01 0.17823465158320E+01 0.19517681177867E-02 0.31471360598987E+03 0.33380715641388E+03
 0.33378231229189E+03 0.33378080708887E+03 0.23000000000000E+00 0.00000000000000E+00 0.18305891526278E+00
 0.00000000000000E+00 -.15829311330214E+02 0.90650795122840E-01 0.88843572799603E+00 0.88250742744829E+02
 0.33094028529311E+02 0.90045905943528E+01 0.33767214728823E+01 0.31483645452393E+03 0.39012021824202E+03
 0.30102310988655E+03 0.30685738160731E+03 0.29815000000344E+03 0.29815000006937E+03 0.30101632174110E+03
 0.30685612949171E+03 0.29815000000340E+03 0.29815000006939E+03 0.30102310988655E+03 0.30685738160731E+03
 0.29815000000344E+03 0.29815000006937E+03 0.30101632174110E+03 0.30685612949171E+03 0.29815000000340E+03
 0.29815000006939E+03 0.30554233455729E+03 0.29815000067868E+03 0.93113152505614E+02 0.93006779228165E+02
 0.15674013958768E+03 0.40024191365002E+03 0.24271807336440E+03 0.13783637496067E+03 0.14671635738726E+03
 0.13783637496067E+03 0.31960338891158E+03 0.13783151947051E+03 0.14659446640404E+03 0.13783151947051E+03
 0.31949118507330E+03 0.13783637496067E+03 0.14671635738726E+03 0.13783637496067E+03 0.31960338891158E+03
 0.13783151947051E+03 0.14659446640404E+03 0.13783151947051E+03 0.31949118507330E+03 0.21417630185263E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35785718564621E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19657985068946E+00 0.00000000000000E+00 0.00000000000000E+00 0.19657985068946E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20562573914384E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20562573914384E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638667914031E+00 0.20708223630533E+00 0.33380715641388E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    500.08558318
 0.11793895405335E+00 0.31132638206357E+03 0.43904404887807E+03 0.43402308619866E+03 0.43207568381774E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16250948648043E+00 0.00000000000000E+00 -.23110837939953E+02
 0.30032819232941E-02 0.97392741327253E+00 0.26637525894424E+04 0.99890722104089E+03 0.82141645167568E+01
 0.30803116937838E+01 0.33265711071521E+03 0.29815000538743E+03 0.32747022265222E+03 0.34966982865979E+03
 0.29815000039636E+03 0.29815000072222E+03 0.32422420947156E+03 0.34964112966228E+03 0.29815000031296E+03
 0.29815000071919E+03 0.32747022265222E+03 0.34966982865979E+03 0.29815000039636E+03 0.29815000072222E+03
 0.32422420947156E+03 0.34964112966228E+03 0.29815000031296E+03 0.29815000071919E+03 0.39082134210279E+03
 0.31545350734691E+03 0.18609676372280E+04 0.17584094332272E+04 0.62285535349858E+03 0.10890566025886E+04
 0.46308697232256E+03 0.10933649213871E+04 0.10584778375013E+04 0.10183505700476E+04 0.17176904882036E+04
 0.97893731075176E+03 0.10579604725230E+04 0.92249990753451E+03 0.17174354597801E+04 0.10933649213871E+04
 0.10584778375013E+04 0.10183505700476E+04 0.17176904882036E+04 0.97893731075176E+03 0.10579604725230E+04
 0.92249990753451E+03 0.17174354597801E+04 0.16965470950736E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47785260277369E+03 0.12948747695330E+01
 0.12948747695330E+01 0.18203615822573E+01 0.15670240754515E-02 0.31535534659127E+03 0.33392359068001E+03
 0.33390419275632E+03 0.33390305186249E+03 0.23000000000000E+00 0.00000000000000E+00 0.18205373940851E+00
 0.00000000000000E+00 -.15818444026220E+02 0.11290785466893E+00 0.90733189538175E+00 0.70854237939937E+02
 0.26570339227476E+02 0.88170602628645E+01 0.33063975985742E+01 0.31545187563536E+03 0.39082989383394E+03
 0.30111051250537E+03 0.30699013708490E+03 0.29815000000450E+03 0.29815000008873E+03 0.30110391946185E+03
 0.30698879006365E+03 0.29815000000445E+03 0.29815000008876E+03 0.30111051250537E+03 0.30699013708490E+03
 0.29815000000450E+03 0.29815000008873E+03 0.30110391946185E+03 0.30698879006365E+03 0.29815000000445E+03
 0.29815000008876E+03 0.30565615959277E+03 0.29815000084736E+03 0.91297463118302E+02 0.91220443030918E+02
 0.15829605721390E+03 0.40183076003811E+03 0.24274322253814E+03 0.14030536816457E+03 0.14811200738029E+03
 0.14030536816457E+03 0.32085239239631E+03 0.14030615476634E+03 0.14798710824402E+03 0.14030615476634E+03
 0.32073791169069E+03 0.14030536816457E+03 0.14811200738029E+03 0.14030536816457E+03 0.32085239239631E+03
 0.14030615476634E+03 0.14798710824402E+03 0.14030615476634E+03 0.32073791169069E+03 0.21451769891417E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35802980369004E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19650977639301E+00 0.00000000000000E+00 0.00000000000000E+00 0.19650977639301E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20554383869137E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20554383869137E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638677651750E+00 0.20701015462012E+00 0.33392359068001E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    511.17331088
 0.11785537196861E+00 0.31156336780901E+03 0.43946152939870E+03 0.43443703426262E+03 0.43248693379539E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16185018598704E+00 0.00000000000000E+00 -.23117347339806E+02
 0.30054114831309E-02 0.98251526010860E+00 0.26618651206011E+04 0.99819942022541E+03 0.81423671721046E+01
 0.30533876895392E+01 0.33317830720002E+03 0.29815000686595E+03 0.32791728851369E+03 0.35035415339018E+03
 0.29815000052081E+03 0.29815000095114E+03 0.32464302257655E+03 0.35032575664997E+03 0.29815000041197E+03
 0.29815000094722E+03 0.32791728851369E+03 0.35035415339018E+03 0.29815000052081E+03 0.29815000095114E+03
 0.32464302257655E+03 0.35032575664997E+03 0.29815000041197E+03 0.29815000094722E+03 0.39163700828098E+03
 0.31617250905362E+03 0.18658758240624E+04 0.17615630911519E+04 0.62154938368299E+03 0.10830022063196E+04
 0.45834507571822E+03 0.10967670996186E+04 0.10611592218990E+04 0.10205271900477E+04 0.17176252080306E+04
 0.98261001852331E+03 0.10606548490534E+04 0.92517285997624E+03 0.17173800476957E+04 0.10967670996186E+04
 0.10611592218990E+04 0.10205271900477E+04 0.17176252080306E+04 0.98261001852330E+03 0.10606548490534E+04
 0.92517285997624E+03 0.17173800476957E+04 0.16958838783634E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47826451615206E+03 0.12948748174967E+01
 0.12948748174967E+01 0.18647124930867E+01 0.12127609924266E-02 0.31609865868042E+03 0.33406340599524E+03
 0.33404888136538E+03 0.33404805659643E+03 0.23000000000000E+00 0.00000000000000E+00 0.18090585252782E+00
 0.00000000000000E+00 -.15801205135188E+02 0.14588968608461E+00 0.92887074285412E+00 0.54835953210293E+02
 0.20563482453860E+02 0.86126084404581E+01 0.32297281651718E+01 0.31617099799369E+03 0.39164482100377E+03
 0.30121315808459E+03 0.30714405411239E+03 0.29815000000613E+03 0.29815000011726E+03 0.30120677779841E+03
 0.30714259839020E+03 0.29815000000605E+03 0.29815000011730E+03 0.30121315808459E+03 0.30714405411239E+03
 0.29815000000613E+03 0.29815000011726E+03 0.30120677779841E+03 0.30714259839020E+03 0.29815000000605E+03
 0.29815000011730E+03 0.30578815885740E+03 0.29815000108918E+03 0.89080105955290E+02 0.89030774098738E+02
 0.16004953981971E+03 0.40366657391298E+03 0.24281678639417E+03 0.14316146977762E+03 0.14968199471221E+03
 0.14316146977762E+03 0.32228917052709E+03 0.14316765558019E+03 0.14955380110865E+03 0.14316765558019E+03
 0.32217223281123E+03 0.14316146977762E+03 0.14968199471221E+03 0.14316146977762E+03 0.32228917052709E+03
 0.14316765558019E+03 0.14955380110865E+03 0.14316765558019E+03 0.32217223281123E+03 0.21487664628296E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35823059247747E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19639847411174E+00 0.00000000000000E+00 0.00000000000000E+00 0.19639847411174E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20542247148979E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20542247148979E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638702895814E+00 0.20692381601752E+00 0.33406340599524E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    520.67707749
 0.11777783987616E+00 0.31176504031853E+03 0.43981855469972E+03 0.43479126592893E+03 0.43283891953121E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16130442152613E+00 0.00000000000000E+00 -.23124108803845E+02
 0.30073896244841E-02 0.98958619938029E+00 0.26601142515321E+04 0.99754284432453E+03 0.80841871127648E+01
 0.30315701672868E+01 0.33362087091097E+03 0.29815000840028E+03 0.32829712361383E+03 0.35093440011479E+03
 0.29815000065363E+03 0.29815000119587E+03 0.32499911092524E+03 0.35090625422126E+03 0.29815000051783E+03
 0.29815000119101E+03 0.32829712361383E+03 0.35093440011479E+03 0.29815000065363E+03 0.29815000119587E+03
 0.32499911092524E+03 0.35090625422126E+03 0.29815000051783E+03 0.29815000119101E+03 0.39232598149587E+03
 0.31678950119152E+03 0.18700157499870E+04 0.17642116890985E+04 0.62043201185887E+03 0.10779323664113E+04
 0.45439819449315E+03 0.10996424735826E+04 0.10634118181272E+04 0.10223594795052E+04 0.17175781424350E+04
 0.98571521277694E+03 0.10629179357111E+04 0.92742575374041E+03 0.17173408809819E+04 0.10996424735826E+04
 0.10634118181272E+04 0.10223594795052E+04 0.17175781424350E+04 0.98571521277694E+03 0.10629179357111E+04
 0.92742575374041E+03 0.17173408809819E+04 0.16953359939615E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47861704248440E+03 0.12948748673177E+01
 0.12948748673177E+01 0.19027275595120E+01 0.97349302875127E-03 0.31673443399386E+03 0.33418687472896E+03
 0.33417554817603E+03 0.33417492449695E+03 0.23000000000000E+00 0.00000000000000E+00 0.17994352754700E+00
 0.00000000000000E+00 -.15787728912040E+02 0.18174687437403E+00 0.94689218629688E+00 0.44017263171946E+02
 0.16506473689480E+02 0.84486915361362E+01 0.31682593260511E+01 0.31678809736615E+03 0.39233319059115E+03
 0.30130164894625E+03 0.30727518177829E+03 0.29815000000794E+03 0.29815000014788E+03 0.30129543878508E+03
 0.30727363464268E+03 0.29815000000784E+03 0.29815000014792E+03 0.30130164894625E+03 0.30727518177829E+03
 0.29815000000794E+03 0.29815000014788E+03 0.30129543878508E+03 0.30727363464268E+03 0.29815000000784E+03
 0.29815000014792E+03 0.30590064240596E+03 0.29815000134198E+03 0.87103175857465E+02 0.87069234883714E+02
 0.16150637949773E+03 0.40523543988941E+03 0.24292152849418E+03 0.14559246085196E+03 0.15098428057380E+03
 0.14559246085196E+03 0.32351230072083E+03 0.14560241845197E+03 0.15085342895617E+03 0.14560241845197E+03
 0.32339340932021E+03 0.14559246085196E+03 0.15098428057380E+03 0.14559246085196E+03 0.32351230072083E+03
 0.14560241845197E+03 0.15085342895617E+03 0.14560241845197E+03 0.32339340932021E+03 0.21515791448365E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35840386633308E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19631234729423E+00 0.00000000000000E+00 0.00000000000000E+00 0.19631234729423E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20531867502111E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20531867502111E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638719273215E+00 0.20684756593464E+00 0.33418687472896E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    533.46493153
 0.11767408883255E+00 0.31202995157667E+03 0.44029605571350E+03 0.43526485673603E+03 0.43330933702716E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16059711120369E+00 0.00000000000000E+00 -.23143564683639E+02
 0.30100407914384E-02 0.99869948475280E+00 0.26577712909256E+04 0.99666423409711E+03 0.80104176703167E+01
 0.30039066263688E+01 0.33420858061877E+03 0.29815001098957E+03 0.32880164505902E+03 0.35170592540624E+03
 0.29815000088560E+03 0.29815000162412E+03 0.32547203309232E+03 0.35167809918501E+03 0.29815000070313E+03
 0.29815000161767E+03 0.32880164505902E+03 0.35170592540624E+03 0.29815000088560E+03 0.29815000162412E+03
 0.32547203309232E+03 0.35167809918502E+03 0.29815000070313E+03 0.29815000161767E+03 0.39324827345994E+03
 0.31762447432669E+03 0.18754833339908E+04 0.17676733487182E+04 0.61879093630672E+03 0.10710091611575E+04
 0.44912427016921E+03 0.11034490913867E+04 0.10663610671947E+04 0.10247583362008E+04 0.17174984384126E+04
 0.98982976060409E+03 0.10658804187794E+04 0.93038784140025E+03 0.17172711014155E+04 0.11034490913867E+04
 0.10663610671947E+04 0.10247583362008E+04 0.17174984384126E+04 0.98982976060409E+03 0.10658804187794E+04
 0.93038784140025E+03 0.17172711014155E+04 0.16945385356482E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47908827974122E+03 0.12948750106759E+01
 0.12948750106759E+01 0.19538789756732E+01 0.72425888775385E-03 0.31759312629462E+03 0.33435813411587E+03
 0.33435003931193E+03 0.33434961222312E+03 0.23000000000000E+00 0.00000000000000E+00 0.17868040957138E+00
 0.00000000000000E+00 -.15780128066648E+02 0.24429014400621E+00 0.97049440056884E+00 0.32747944181476E+02
 0.12280479068054E+02 0.82432211822252E+01 0.30912079433344E+01 0.31762320758563E+03 0.39325474928472E+03
 0.30141952382213E+03 0.30744945301704E+03 0.29815000001126E+03 0.29815000020171E+03 0.30141352228624E+03
 0.30744778687971E+03 0.29815000001112E+03 0.29815000020177E+03 0.30141952382213E+03 0.30744945301704E+03
 0.29815000001126E+03 0.29815000020171E+03 0.30141352228624E+03 0.30744778687971E+03 0.29815000001112E+03
 0.29815000020177E+03 0.30605013697299E+03 0.29815000177240E+03 0.84313717948786E+02 0.84297269819439E+02
 0.16341161716456E+03 0.40735949539398E+03 0.24313082014360E+03 0.14885189826650E+03 0.15268570131091E+03
 0.14885189826650E+03 0.32516380002256E+03 0.14886591421519E+03 0.15255146550798E+03 0.14886591421519E+03
 0.32504244158622E+03 0.14885189826650E+03 0.15268570131091E+03 0.14885189826650E+03 0.32516380002256E+03
 0.14886591421519E+03 0.15255146550798E+03 0.14886591421519E+03 0.32504244158622E+03 0.21550666939694E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35863874000737E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19627594218832E+00 0.00000000000000E+00 0.00000000000000E+00 0.19627594218832E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20517622818837E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20517622818837E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638705287583E+00 0.20674148618576E+00 0.33435813411587E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    540.92085477
 0.11759817161463E+00 0.31219016522733E+03 0.44057533818673E+03 0.43554271765258E+03 0.43358577566689E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16019850052291E+00 0.00000000000000E+00 -.23135133528260E+02
 0.30119837360248E-02 0.10038036856940E+01 0.26560568386595E+04 0.99602131449732E+03 0.79696858200604E+01
 0.29886321825226E+01 0.33455040956909E+03 0.29815001273780E+03 0.32909551633821E+03 0.35215105596171E+03
 0.29815000104563E+03 0.29815000191991E+03 0.32574827951293E+03 0.35212341851341E+03 0.29815000083116E+03
 0.29815000191238E+03 0.32909551633821E+03 0.35215105596171E+03 0.29815000104563E+03 0.29815000191991E+03
 0.32574827951293E+03 0.35212341851341E+03 0.29815000083116E+03 0.29815000191238E+03 0.39376469200466E+03
 0.31810563573902E+03 0.18786359445610E+04 0.17696969211136E+04 0.61803688137724E+03 0.10674363233360E+04
 0.44630925755190E+03 0.11056463273484E+04 0.10680730832103E+04 0.10261672520539E+04 0.17174971777171E+04
 0.99220245967865E+03 0.10675997440066E+04 0.93211499962302E+03 0.17172752071387E+04 0.11056463273484E+04
 0.10680730832103E+04 0.10261672520539E+04 0.17174971777171E+04 0.99220245967865E+03 0.10675997440066E+04
 0.93211499962302E+03 0.17172752071387E+04 0.16942144437442E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47936496981695E+03 0.12948749485519E+01
 0.12948749485519E+01 0.19837026686458E+01 0.60950803374693E-03 0.31808682885980E+03 0.33446072283932E+03
 0.33445406949271E+03 0.33445372715019E+03 0.23000000000000E+00 0.00000000000000E+00 0.17796083347885E+00
 0.00000000000000E+00 -.15755566955286E+02 0.29028215202470E+00 0.98391224136620E+00 0.27559393315092E+02
 0.10334772493159E+02 0.81308064516930E+01 0.30490524193849E+01 0.31810446426952E+03 0.39377071886348E+03
 0.30149132531837E+03 0.30755196373375E+03 0.29815000001363E+03 0.29815000023901E+03 0.30148544161204E+03
 0.30755022737024E+03 0.29815000001346E+03 0.29815000023908E+03 0.30149132531837E+03 0.30755196373375E+03
 0.29815000001363E+03 0.29815000023901E+03 0.30148544161204E+03 0.30755022737024E+03 0.29815000001346E+03
 0.29815000023908E+03 0.30613817153405E+03 0.29815000206462E+03 0.82678103315248E+02 0.82669669787787E+02
 0.16448826611533E+03 0.40858051527955E+03 0.24326980783364E+03 0.15073237562578E+03 0.15364425190527E+03
 0.15073237562578E+03 0.32610684275675E+03 0.15074828462515E+03 0.15350816170217E+03 0.15074828462515E+03
 0.32598417115503E+03 0.15073237562578E+03 0.15364425190527E+03 0.15073237562578E+03 0.32610684275675E+03
 0.15074828462515E+03 0.15350816170217E+03 0.15074828462515E+03 0.32598417115504E+03 0.21569600497893E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35877686885457E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19610517967609E+00 0.00000000000000E+00 0.00000000000000E+00 0.19610517967609E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20508286432996E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20508286432996E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638762761434E+00 0.20667872760532E+00 0.33446072283932E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    550.96327342
 0.11749967572531E+00 0.31240035459739E+03 0.44094959040156E+03 0.43591475922765E+03 0.43395570048831E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15967696446432E+00 0.00000000000000E+00 -.23142579454706E+02
 0.30145082683508E-02 0.10104517821955E+01 0.26538324953332E+04 0.99518718574994E+03 0.79172506209229E+01
 0.29689689828461E+01 0.33500629671012E+03 0.29815001547797E+03 0.32948745482176E+03 0.35274603700215E+03
 0.29815000130215E+03 0.29815000239461E+03 0.32611652339202E+03 0.35271864219937E+03 0.29815000103668E+03
 0.29815000238536E+03 0.32948745482176E+03 0.35274603700215E+03 0.29815000130215E+03 0.29815000239461E+03
 0.32611652339202E+03 0.35271864219937E+03 0.29815000103668E+03 0.29815000238536E+03 0.39446315109527E+03
 0.31875725253663E+03 0.18828250149600E+04 0.17723535241758E+04 0.61687377757550E+03 0.10624296340292E+04
 0.44247148756583E+03 0.11085718058847E+04 0.10703275709768E+04 0.10280180638779E+04 0.17174727949128E+04
 0.99536453124238E+03 0.10698635816857E+04 0.93439535995479E+03 0.17172576708272E+04 0.11085718058847E+04
 0.10703275709768E+04 0.10280180638779E+04 0.17174727949128E+04 0.99536453124238E+03 0.10698635816857E+04
 0.93439535995479E+03 0.17172576708272E+04 0.16936998348443E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47973534736421E+03 0.12948750034163E+01
 0.12948750034163E+01 0.20238723432214E+01 0.48310398463514E-03 0.31875503855719E+03 0.33460197383630E+03
 0.33459687002458E+03 0.33459661637391E+03 0.23000000000000E+00 0.00000000000000E+00 0.17701127222758E+00
 0.00000000000000E+00 -.15741635024049E+02 0.36623439983079E+00 0.10015855169187E+01 0.21843933840448E+02
 0.81914751901680E+01 0.79873359437258E+01 0.29952509788972E+01 0.31875621057581E+03 0.39446861082299E+03
 0.30158667281594E+03 0.30768855034321E+03 0.29815000001755E+03 0.29815000029907E+03 0.30158093651054E+03
 0.30768672225974E+03 0.29815000001732E+03 0.29815000029916E+03 0.30158667281594E+03 0.30768855034321E+03
 0.29815000001755E+03 0.29815000029907E+03 0.30158093651054E+03 0.30768672225974E+03 0.29815000001732E+03
 0.29815000029916E+03 0.30625545964513E+03 0.29815000252525E+03 0.80398848234420E+02 0.80398563853730E+02
 0.16591147404310E+03 0.41024165495427E+03 0.24350062354096E+03 0.15325719114944E+03 0.15491124816478E+03
 0.15325719114944E+03 0.32738846101508E+03 0.15327521277762E+03 0.15477278481762E+03 0.15327521277762E+03
 0.32726412347376E+03 0.15325719114944E+03 0.15491124816478E+03 0.15325719114944E+03 0.32738846101508E+03
 0.15327521277762E+03 0.15477278481762E+03 0.15327521277762E+03 0.32726412347376E+03 0.21593845111336E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35896475356341E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19601779677655E+00 0.00000000000000E+00 0.00000000000000E+00 0.19601779677655E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20496395588875E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20496395588875E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638776338559E+00 0.20659165018359E+00 0.33460197383630E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    560.95866368
 0.11740977163340E+00 0.31260395070916E+03 0.44131950915582E+03 0.43628202104819E+03 0.43432060565217E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15917485893213E+00 0.00000000000000E+00 -.23156425025450E+02
 0.30168162615937E-02 0.10168193973706E+01 0.26518022001691E+04 0.99442582506341E+03 0.78676705230910E+01
 0.29503764461591E+01 0.33545530574629E+03 0.29815001874091E+03 0.32987357686621E+03 0.35333247662320E+03
 0.29815000161590E+03 0.29815000297601E+03 0.32647931654422E+03 0.35330531322839E+03 0.29815000128853E+03
 0.29815000296470E+03 0.32987357686621E+03 0.35333247662320E+03 0.29815000161590E+03 0.29815000297601E+03
 0.32647931654422E+03 0.35330531322839E+03 0.29815000128853E+03 0.29815000296470E+03 0.39515461489991E+03
 0.31940771488492E+03 0.18869194065344E+04 0.17749116952217E+04 0.61563197067769E+03 0.10573843782709E+04
 0.43867424773981E+03 0.11114396861783E+04 0.10725130539765E+04 0.10298041709391E+04 0.17174245652404E+04
 0.99846671878269E+03 0.10720578588019E+04 0.93660715674020E+03 0.17172158441215E+04 0.11114396861783E+04
 0.10725130539765E+04 0.10298041709391E+04 0.17174245652404E+04 0.99846671878269E+03 0.10720578588019E+04
 0.93660715674020E+03 0.17172158441215E+04 0.16931356142804E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48010083951067E+03 0.12948751054357E+01
 0.12948751054357E+01 0.20638539042590E+01 0.38322490776288E-03 0.31943184009990E+03 0.33474612010455E+03
 0.33474220756218E+03 0.33474202003633E+03 0.23000000000000E+00 0.00000000000000E+00 0.17608841287974E+00
 0.00000000000000E+00 -.15734988546976E+02 0.46168526199544E+00 0.10187236783011E+01 0.17327821913620E+02
 0.64979332176075E+01 0.78529636351848E+01 0.29448613631943E+01 0.31940678285004E+03 0.39515959037546E+03
 0.30168106731819E+03 0.30782331869875E+03 0.29815000002254E+03 0.29815000037292E+03 0.30167546686585E+03
 0.30782140157525E+03 0.29815000002225E+03 0.29815000037303E+03 0.30168106731819E+03 0.30782331869875E+03
 0.29815000002254E+03 0.29815000037292E+03 0.30167546686585E+03 0.30782140157525E+03 0.29815000002225E+03
 0.29815000037303E+03 0.30637119699627E+03 0.29815000307746E+03 0.78062037730774E+02 0.78068570099523E+02
 0.16730160915342E+03 0.41191486601989E+03 0.24377674882071E+03 0.15576804990219E+03 0.15614788869426E+03
 0.15576804990219E+03 0.32867727822063E+03 0.15578775393801E+03 0.15600715598417E+03 0.15578775393801E+03
 0.32855135833325E+03 0.15576804990219E+03 0.15614788869426E+03 0.15576804990219E+03 0.32867727822063E+03
 0.15578775393801E+03 0.15600715598417E+03 0.15578775393801E+03 0.32855135833325E+03 0.21617084958829E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35915460988500E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19598080648784E+00 0.00000000000000E+00 0.00000000000000E+00 0.19598080648784E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20486644890138E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20486644890138E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638765217325E+00 0.20650258559905E+00 0.33474612010455E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    572.79808496
 0.11731786799076E+00 0.31284371586921E+03 0.44175554217287E+03 0.43671432196595E+03 0.43474990361318E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15860143807032E+00 0.00000000000000E+00 -.23168504061146E+02
 0.30191791973824E-02 0.10240470320377E+01 0.26497267889683E+04 0.99364754586312E+03 0.78121411905089E+01
 0.29295529464408E+01 0.33598259681479E+03 0.29815002335858E+03 0.33032732825116E+03 0.35401959814923E+03
 0.29815000207205E+03 0.29815000382236E+03 0.32690608034768E+03 0.35399270137503E+03 0.29815000165537E+03
 0.29815000380810E+03 0.33032732825117E+03 0.35401959814923E+03 0.29815000207205E+03 0.29815000382236E+03
 0.32690608034768E+03 0.35399270137503E+03 0.29815000165537E+03 0.29815000380810E+03 0.39595913151082E+03
 0.32017603768952E+03 0.18916780022366E+04 0.17778657317846E+04 0.61419849156794E+03 0.10515974770414E+04
 0.43432799301558E+03 0.11147843343057E+04 0.10750481960345E+04 0.10318788811641E+04 0.17173693501737E+04
 0.10020852106090E+04 0.10746028081460E+04 0.93917795234257E+03 0.17171676836422E+04 0.11147843343057E+04
 0.10750481960345E+04 0.10318788811641E+04 0.17173693501737E+04 0.10020852106090E+04 0.10746028081460E+04
 0.93917795234257E+03 0.17171676836423E+04 0.16924921788851E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48053093397882E+03 0.12948751944386E+01
 0.12948751944386E+01 0.21112115894091E+01 0.29120062645438E-03 0.32022027036789E+03 0.33492212042082E+03
 0.33491926629552E+03 0.33491913528444E+03 0.23000000000000E+00 0.00000000000000E+00 0.17502363086405E+00
 0.00000000000000E+00 -.15723146078714E+02 0.60758552241012E+00 0.10384456938710E+01 0.13166870678989E+02
 0.49375765046209E+01 0.77038212467119E+01 0.28889329675170E+01 0.32017524274620E+03 0.39596354647852E+03
 0.30179415556596E+03 0.30798257960967E+03 0.29815000003009E+03 0.29815000048088E+03 0.30178870552855E+03
 0.30798055852432E+03 0.29815000002970E+03 0.29815000048102E+03 0.30179415556596E+03 0.30798257960967E+03
 0.29815000003009E+03 0.29815000048088E+03 0.30178870552855E+03 0.30798055852432E+03 0.29815000002970E+03
 0.29815000048102E+03 0.30650803941339E+03 0.29815000386415E+03 0.75246875503439E+02 0.75260455690771E+02
 0.16891814767565E+03 0.41392097789073E+03 0.24415823947671E+03 0.15873253934632E+03 0.15758456373887E+03
 0.15873253934632E+03 0.33021877701188E+03 0.15875378013438E+03 0.15744127737717E+03 0.15875378013438E+03
 0.33009110619346E+03 0.15873253934632E+03 0.15758456373887E+03 0.15873253934632E+03 0.33021877701187E+03
 0.15875378013438E+03 0.15744127737717E+03 0.15875378013438E+03 0.33009110619346E+03 0.21644387275400E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35938318833305E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19590587757303E+00 0.00000000000000E+00 0.00000000000000E+00 0.19590587757303E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20475690146221E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20475690146221E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638763891564E+00 0.20639407946746E+00 0.33492212042082E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    581.23011373
 0.11722086817448E+00 0.31302077160593E+03 0.44206626296057E+03 0.43702398811704E+03 0.43505815679488E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15820658505341E+00 0.00000000000000E+00 -.23160458508604E+02
 0.30216772990783E-02 0.10289914679952E+01 0.26475361887387E+04 0.99282607077703E+03 0.77746028502905E+01
 0.29154760688589E+01 0.33635606455824E+03 0.29815002714367E+03 0.33064899754739E+03 0.35450431382208E+03
 0.29815000245388E+03 0.29815000453149E+03 0.32720904602710E+03 0.35447760387089E+03 0.29815000196291E+03
 0.29815000451481E+03 0.33064899754739E+03 0.35450431382208E+03 0.29815000245388E+03 0.29815000453149E+03
 0.32720904602710E+03 0.35447760387089E+03 0.29815000196291E+03 0.29815000451481E+03 0.39652036001104E+03
 0.32071991421574E+03 0.18950519934157E+04 0.17800072808215E+04 0.61326060210040E+03 0.10477023338560E+04
 0.43137542874506E+03 0.11171529834270E+04 0.10768448010462E+04 0.10333842731353E+04 0.17173667647255E+04
 0.10046469582513E+04 0.10764060357902E+04 0.94103005367784E+03 0.17171697958155E+04 0.11171529834270E+04
 0.10768448010462E+04 0.10333842731353E+04 0.17173667647255E+04 0.10046469582513E+04 0.10764060357902E+04
 0.94103005367784E+03 0.17171697958155E+04 0.16921234692900E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48083938311027E+03 0.12948751351559E+01
 0.12948751351559E+01 0.21449397044756E+01 0.23955840911150E-03 0.32076814736184E+03 0.33504908823204E+03
 0.33504680748572E+03 0.33504670596140E+03 0.23000000000000E+00 0.00000000000000E+00 0.17428424363777E+00
 0.00000000000000E+00 -.15695654643518E+02 0.73856426179415E+00 0.10521116731561E+01 0.10831826577373E+02
 0.40619349665150E+01 0.76037555747311E+01 0.28514083405242E+01 0.32071923241690E+03 0.39652435868961E+03
 0.30187620955660E+03 0.30809614824003E+03 0.29815000003661E+03 0.29815000057163E+03 0.30187086108087E+03
 0.30809405374331E+03 0.29815000003613E+03 0.29815000057180E+03 0.30187620955660E+03 0.30809614824003E+03
 0.29815000003661E+03 0.29815000057163E+03 0.30187086108087E+03 0.30809405374331E+03 0.29815000003613E+03
 0.29815000057180E+03 0.30660570829681E+03 0.29815000451231E+03 0.73214909561333E+02 0.73229962381377E+02
 0.17003793167988E+03 0.41532581850539E+03 0.24443769716711E+03 0.16082394957274E+03 0.15857762165131E+03
 0.16082394957274E+03 0.33129189734410E+03 0.16084602762997E+03 0.15843262250617E+03 0.16084602762997E+03
 0.33116308026813E+03 0.16082394957274E+03 0.15857762165131E+03 0.16082394957274E+03 0.33129189734410E+03
 0.16084602762997E+03 0.15843262250617E+03 0.16084602762997E+03 0.33116308026813E+03 0.21661733405272E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35954320659338E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19572610561679E+00 0.00000000000000E+00 0.00000000000000E+00 0.19572610561679E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20459723327904E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20459723327904E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638825352289E+00 0.20631656752410E+00 0.33504908823204E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    590.08083065
 0.11712638597509E+00 0.31320135085640E+03 0.44239074763292E+03 0.43734691858268E+03 0.43537935061781E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15780359130508E+00 0.00000000000000E+00 -.23164945164527E+02
 0.30241145242882E-02 0.10340134251828E+01 0.26454024593804E+04 0.99202592226765E+03 0.77368434540253E+01
 0.29013162952595E+01 0.33674500206852E+03 0.29815003168608E+03 0.33098406514641E+03 0.35500937227162E+03
 0.29815000292139E+03 0.29815000540047E+03 0.32752464377793E+03 0.35498285193542E+03 0.29815000233998E+03
 0.29815000538085E+03 0.33098406514641E+03 0.35500937227162E+03 0.29815000292139E+03 0.29815000540047E+03
 0.32752464377793E+03 0.35498285193542E+03 0.29815000233998E+03 0.29815000538085E+03 0.39710767551655E+03
 0.32129154897377E+03 0.18985461973887E+04 0.17821896067022E+04 0.61221410257917E+03 0.10435589498947E+04
 0.42828377680261E+03 0.11196119358453E+04 0.10786941915233E+04 0.10349193029557E+04 0.17173501115660E+04
 0.10073078522196E+04 0.10782620506962E+04 0.94292839187962E+03 0.17171578109785E+04 0.11196119358453E+04
 0.10786941915233E+04 0.10349193029557E+04 0.17173501115660E+04 0.10073078522196E+04 0.10782620506962E+04
 0.94292839187962E+03 0.17171578109785E+04 0.16916972743650E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48116092066926E+03 0.12948751682153E+01
 0.12948751682153E+01 0.21803425721524E+01 0.19513130884086E-03 0.32134600352907E+03 0.33518478636772E+03
 0.33518298611452E+03 0.33518290859704E+03 0.23000000000000E+00 0.00000000000000E+00 0.17352462719552E+00
 0.00000000000000E+00 -.15681295095380E+02 0.90671904537039E+00 0.10661221487445E+01 0.88230197003660E+01
 0.33086323876372E+01 0.75038305971047E+01 0.28139364739143E+01 0.32129097394650E+03 0.39711128968285E+03
 0.30196200110151E+03 0.30821459655954E+03 0.29815000004483E+03 0.29815000068318E+03 0.30195675312478E+03
 0.30821242647680E+03 0.29815000004424E+03 0.29815000068339E+03 0.30196200110151E+03 0.30821459655954E+03
 0.29815000004483E+03 0.29815000068318E+03 0.30195675312478E+03 0.30821242647680E+03 0.29815000004424E+03
 0.29815000068339E+03 0.30670758367353E+03 0.29815000529394E+03 0.71041090848854E+02 0.71058521252525E+02
 0.17119914761422E+03 0.41682020929425E+03 0.24476506594196E+03 0.16301828705854E+03 0.15960700866583E+03
 0.16301828705854E+03 0.33243306887105E+03 0.16304106292403E+03 0.15946026716401E+03 0.16304106292403E+03
 0.33230309338971E+03 0.16301828705854E+03 0.15960700866583E+03 0.16301828705854E+03 0.33243306887105E+03
 0.16304106292403E+03 0.15946026716401E+03 0.16304106292403E+03 0.33230309338972E+03 0.21679860969368E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35971505995577E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19563597381035E+00 0.00000000000000E+00 0.00000000000000E+00 0.19563597381035E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20448036144192E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20448036144192E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638841554049E+00 0.20623324051985E+00 0.33518478636772E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    600.01906415
 0.11708801120634E+00 0.31339443926836E+03 0.44275351378858E+03 0.43770471486623E+03 0.43573382175183E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15736478624119E+00 0.00000000000000E+00 -.23312313662363E+02
 0.30251053649809E-02 0.10394496749509E+01 0.26445359862864E+04 0.99170099485740E+03 0.76963802989093E+01
 0.28861426120910E+01 0.33717866591690E+03 0.29815003753334E+03 0.33135785951791E+03 0.35557161983524E+03
 0.29815000353569E+03 0.29815000654323E+03 0.32787698177878E+03 0.35554530708997E+03 0.29815000283618E+03
 0.29815000651981E+03 0.33135785951791E+03 0.35557161983524E+03 0.29815000353569E+03 0.29815000654323E+03
 0.32787698177878E+03 0.35554530708997E+03 0.29815000283618E+03 0.29815000651981E+03 0.39775877751537E+03
 0.32201286034097E+03 0.19023781634078E+04 0.17844994892365E+04 0.61105042961162E+03 0.10389992474011E+04
 0.42489356564144E+03 0.11223275296657E+04 0.10807359302226E+04 0.10365592481016E+04 0.17173325538151E+04
 0.10102462874660E+04 0.10803108401925E+04 0.94497104944264E+03 0.17171451636821E+04 0.11223275296657E+04
 0.10807359302226E+04 0.10365592481016E+04 0.17173325538151E+04 0.10102462874660E+04 0.10803108401925E+04
 0.94497104944264E+03 0.17171451636821E+04 0.16911960218491E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48151646694065E+03 0.12948762540830E+01
 0.12948762540830E+01 0.22200955061567E+01 0.00000000000000E+00 0.33532526492619E+03 0.33532526492619E+03
 0.33532526492619E+03 0.33532526492619E+03 0.00000000000000E+00 0.00000000000000E+00 0.17269719443828E+00
 0.00000000000000E+00 -.15828879728826E+02 0.00000000000000E+00 0.10815066138342E+01 0.80000000000000E+04
 0.30000000000000E+04 0.73970883743726E+01 0.27739081403897E+01 0.32201239525118E+03 0.39776198708295E+03
 0.30834708229209E+03 0.30834708229209E+03 0.29815000083264E+03 0.29815000083037E+03 0.30834487967861E+03
 0.30834487967861E+03 0.29815000083289E+03 0.29815000083062E+03 0.30834708229209E+03 0.30834708229209E+03
 0.29815000083264E+03 0.29815000083037E+03 0.30834487967861E+03 0.30834487967861E+03 0.29815000083289E+03
 0.29815000083062E+03 0.30682100995893E+03 0.29815000630506E+03 0.68643928515942E+02 0.85070302987219E+02
 0.17191570667600E+03 0.41785556524673E+03 0.24508028003735E+03 0.12512374798484E+03 0.16061650700285E+03
 0.12512374798484E+03 0.33353318660281E+03 0.12513390016742E+03 0.16049348656049E+03 0.12513390016742E+03
 0.33342718619319E+03 0.12512374798484E+03 0.16061650700285E+03 0.12512374798484E+03 0.33353318660281E+03
 0.12513390016743E+03 0.16049348656050E+03 0.12513390016743E+03 0.33342718619320E+03 0.22074944430784E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35993570582329E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19660933165374E+00 0.00000000000000E+00 0.00000000000000E+00 0.19660933165374E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20509410409679E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20509410409679E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638323389710E+00 0.20614110324894E+00 0.33532526492619E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    611.44654253
 0.11723683832225E+00 0.31360489420194E+03 0.44316471093501E+03 0.43810164983921E+03 0.43612362013920E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15688048430771E+00 0.00000000000000E+00 -.23269959794846E+02
 0.30212648133754E-02 0.10454158475258E+01 0.26478976502104E+04 0.99296161882889E+03 0.76524571718841E+01
 0.28696714394565E+01 0.33767196129420E+03 0.29815004537462E+03 0.33178330142084E+03 0.35621203160967E+03
 0.29815000437883E+03 0.29815000811305E+03 0.32827805978878E+03 0.35618594860632E+03 0.29815000351838E+03
 0.29815000808451E+03 0.33178330142084E+03 0.35621203160967E+03 0.29815000437883E+03 0.29815000811305E+03
 0.32827805978878E+03 0.35618594860632E+03 0.29815000351838E+03 0.29815000808451E+03 0.39850145402533E+03
 0.32286624196301E+03 0.19065471990604E+04 0.17868700022636E+04 0.60959587115246E+03 0.10336443006185E+04
 0.42100045011030E+03 0.11253350914315E+04 0.10829679024170E+04 0.10383080395251E+04 0.17172128739221E+04
 0.10135036793019E+04 0.10825503236591E+04 0.94717205206183E+03 0.17170306241901E+04 0.11253350914315E+04
 0.10829679024170E+04 0.10383080395251E+04 0.17172128739221E+04 0.10135036793019E+04 0.10825503236591E+04
 0.94717205206183E+03 0.17170306241901E+04 0.16904358355564E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48190912415907E+03 0.12948759420032E+01
 0.12948759420032E+01 0.22658054196612E+01 0.00000000000000E+00 0.33544751940660E+03 0.33544751940660E+03
 0.33544751940660E+03 0.33544751940660E+03 0.00000000000000E+00 0.00000000000000E+00 0.17176095181082E+00
 0.00000000000000E+00 -.15758109584543E+02 0.10000000000000E-02 0.10988721687529E+01 0.80000000000000E+04
 0.30000000000000E+04 0.72801916614914E+01 0.27300718730593E+01 0.32286508966601E+03 0.39850421423704E+03
 0.30849570411968E+03 0.30849570411968E+03 0.29815000104929E+03 0.29815000103334E+03 0.30849348011172E+03
 0.30849348011172E+03 0.29815000104961E+03 0.29815000103365E+03 0.30849570411968E+03 0.30849570411968E+03
 0.29815000104929E+03 0.29815000103334E+03 0.30849348011172E+03 0.30849348011172E+03 0.29815000104961E+03
 0.29815000103365E+03 0.30694822862744E+03 0.29815000766848E+03 0.64542647473992E+02 0.79847174391301E+02
 0.17308842515318E+03 0.41896009548473E+03 0.24500622820578E+03 0.12781260024516E+03 0.16158380197006E+03
 0.12781260024516E+03 0.33427019369082E+03 0.12782283461800E+03 0.16145617505269E+03 0.12782283461800E+03
 0.33415974520484E+03 0.12781260024516E+03 0.16158380197006E+03 0.12781260024516E+03 0.33427019369082E+03
 0.12782283461801E+03 0.16145617505269E+03 0.12782283461801E+03 0.33415974520485E+03 0.21998154905918E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36009979679972E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19614186536774E+00 0.00000000000000E+00 0.00000000000000E+00 0.19614186536774E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20474017015128E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20474017015128E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638528487500E+00 0.20606827083743E+00 0.33544751940660E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    620.92268485
 0.11721408100571E+00 0.31379602348409E+03 0.44350277679849E+03 0.43843495750183E+03 0.43645402548576E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15649416680661E+00 0.00000000000000E+00 -.23243029306001E+02
 0.30218511265727E-02 0.10501431593781E+01 0.26473838931547E+04 0.99276895993301E+03 0.76180089624521E+01
 0.28567533609195E+01 0.33807777905351E+03 0.29815005292711E+03 0.33213358104460E+03 0.35673777407359E+03
 0.29815000520952E+03 0.29815000966091E+03 0.32860849602936E+03 0.35671187566448E+03 0.29815000419161E+03
 0.29815000962740E+03 0.33213358104460E+03 0.35673777407359E+03 0.29815000520952E+03 0.29815000966091E+03
 0.32860849602936E+03 0.35671187566448E+03 0.29815000419161E+03 0.29815000962740E+03 0.39910900562774E+03
 0.32351843283120E+03 0.19100237237103E+04 0.17889862449035E+04 0.60838308636760E+03 0.10292721188914E+04
 0.41784711709198E+03 0.11278208820065E+04 0.10847703385162E+04 0.10398460469951E+04 0.17170873152346E+04
 0.10161989475685E+04 0.10843586873603E+04 0.94908273454603E+03 0.17169090858548E+04 0.11278208820065E+04
 0.10847703385162E+04 0.10398460469951E+04 0.17170873152346E+04 0.10161989475685E+04 0.10843586873603E+04
 0.94908273454603E+03 0.17169090858548E+04 0.16898657510440E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48224039478978E+03 0.12948757435689E+01
 0.12948757435689E+01 0.23037099889753E+01 0.00000000000000E+00 0.33556779337497E+03 0.33556779337497E+03
 0.33556779337497E+03 0.33556779337497E+03 0.00000000000000E+00 0.00000000000000E+00 0.17100644651079E+00
 0.00000000000000E+00 -.15708338939899E+02 0.10000000000000E-02 0.11127840369265E+01 0.80000000000000E+04
 0.30000000000000E+04 0.71891757380845E+01 0.26959409017817E+01 0.32351763799892E+03 0.39911142303271E+03
 0.30861788525522E+03 0.30861788525522E+03 0.29815000125327E+03 0.29815000123422E+03 0.30861561449261E+03
 0.30861561449261E+03 0.29815000125364E+03 0.29815000123459E+03 0.30861788525522E+03 0.30861788525522E+03
 0.29815000125327E+03 0.29815000123422E+03 0.30861561449261E+03 0.30861561449261E+03 0.29815000125364E+03
 0.29815000123459E+03 0.30705323443205E+03 0.29815000898867E+03 0.61569191394718E+02 0.76066184697686E+02
 0.17411459597072E+03 0.42012499626505E+03 0.24513982731447E+03 0.12994096404021E+03 0.16245584743755E+03
 0.12994096404021E+03 0.33510299698032E+03 0.12995144326384E+03 0.16232534007719E+03 0.12995144326384E+03
 0.33499002606395E+03 0.12994096404021E+03 0.16245584743755E+03 0.12994096404021E+03 0.33510299698032E+03
 0.12995144326384E+03 0.16232534007720E+03 0.12995144326384E+03 0.33499002606396E+03 0.21960322601519E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36025349589388E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19581233749451E+00 0.00000000000000E+00 0.00000000000000E+00 0.19581233749451E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20448923937881E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20448923937881E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638664877877E+00 0.20599594387025E+00 0.33556779337497E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    631.07159607
 0.11710382452220E+00 0.31400324005486E+03 0.44386447489598E+03 0.43879539247696E+03 0.43681282720470E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15609374614122E+00 0.00000000000000E+00 -.23226588500563E+02
 0.30246959852345E-02 0.10550085471883E+01 0.26448939129926E+04 0.99183521737223E+03 0.75828769551876E+01
 0.28435788581953E+01 0.33850946422377E+03 0.29815006218416E+03 0.33250636286468E+03 0.35729599212857E+03
 0.29815000624891E+03 0.29815001159893E+03 0.32896036599597E+03 0.35727028573807E+03 0.29815000503527E+03
 0.29815001155931E+03 0.33250636286468E+03 0.35729599212857E+03 0.29815000624891E+03 0.29815001159893E+03
 0.32896036599597E+03 0.35727028573807E+03 0.29815000503527E+03 0.29815001155931E+03 0.39975331360225E+03
 0.32419850535873E+03 0.19137732892481E+04 0.17913125904176E+04 0.60709851381953E+03 0.10246814290615E+04
 0.41454742267285E+03 0.11304863920005E+04 0.10866937596867E+04 0.10415126004074E+04 0.17169864781488E+04
 0.10190894302813E+04 0.10862881611767E+04 0.95114809728360E+03 0.17168123165386E+04 0.11304863920005E+04
 0.10866937596867E+04 0.10415126004073E+04 0.17169864781488E+04 0.10190894302813E+04 0.10862881611767E+04
 0.95114809728360E+03 0.17168123165386E+04 0.16893236071740E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48259940534317E+03 0.12948756224268E+01
 0.12948756224268E+01 0.23443056338507E+01 0.00000000000000E+00 0.33571050064033E+03 0.33571050064033E+03
 0.33571050064033E+03 0.33571050064033E+03 0.00000000000000E+00 0.00000000000000E+00 0.17021973930842E+00
 0.00000000000000E+00 -.15668209874172E+02 0.00000000000000E+00 0.11272195229150E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70971091587486E+01 0.26614159345307E+01 0.32419811947886E+03 0.39975537119053E+03
 0.30874838779139E+03 0.30874838779139E+03 0.29815000151555E+03 0.29815000148660E+03 0.30874605926456E+03
 0.30874605926456E+03 0.29815000151601E+03 0.29815000148705E+03 0.30874838779139E+03 0.30874838779139E+03
 0.29815000151555E+03 0.29815000148660E+03 0.30874605926456E+03 0.30874605926456E+03 0.29815000151601E+03
 0.29815000148705E+03 0.30716553070844E+03 0.29815001061457E+03 0.58540006302697E+02 0.72230110697180E+02
 0.17525916945062E+03 0.42156053191535E+03 0.24542506661748E+03 0.13220936036126E+03 0.16344097025691E+03
 0.13220936036126E+03 0.33615495890884E+03 0.13222014899321E+03 0.16330771539183E+03 0.13222014899321E+03
 0.33603968520424E+03 0.13220936036126E+03 0.16344097025691E+03 0.13220936036126E+03 0.33615495890885E+03
 0.13222014899322E+03 0.16330771539183E+03 0.13222014899322E+03 0.33603968520424E+03 0.21934240905511E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36043174152739E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19555064297254E+00 0.00000000000000E+00 0.00000000000000E+00 0.19555064297254E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20425892544331E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20425892544331E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638764387758E+00 0.20590950193568E+00 0.33571050064033E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    640.08009985
 0.11697563137701E+00 0.31418449148024E+03 0.44418521470008E+03 0.43911624247405E+03 0.43713262867520E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15574886492736E+00 0.00000000000000E+00 -.23218102178784E+02
 0.30280104716428E-02 0.10591707769722E+01 0.26419987892776E+04 0.99074954597909E+03 0.75530784779287E+01
 0.28324044292233E+01 0.33889008131448E+03 0.29815007155058E+03 0.33283515815862E+03 0.35778746645527E+03
 0.29815000732194E+03 0.29815001360085E+03 0.32927089659364E+03 0.35776192576908E+03 0.29815000590753E+03
 0.29815001355502E+03 0.33283515815862E+03 0.35778746645527E+03 0.29815000732194E+03 0.29815001360085E+03
 0.32927089659364E+03 0.35776192576908E+03 0.29815000590753E+03 0.29815001355502E+03 0.40031993698290E+03
 0.32479208960885E+03 0.19171010371642E+04 0.17933730633824E+04 0.60597047611039E+03 0.10206812502392E+04
 0.41168092174824E+03 0.11328473622107E+04 0.10883957768541E+04 0.10429768630280E+04 0.17169265994513E+04
 0.10216493607744E+04 0.10879953037543E+04 0.95296528322152E+03 0.17167558499215E+04 0.11328473622106E+04
 0.10883957768541E+04 0.10429768630280E+04 0.17169265994513E+04 0.10216493607744E+04 0.10879953037543E+04
 0.95296528322152E+03 0.17167558499215E+04 0.16888801408383E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48291920761374E+03 0.12948755598963E+01
 0.12948755598963E+01 0.23803396489717E+01 0.00000000000000E+00 0.33584691232750E+03 0.33584691232750E+03
 0.33584691232750E+03 0.33584691232750E+03 0.00000000000000E+00 0.00000000000000E+00 0.16953937548724E+00
 0.00000000000000E+00 -.15639397582068E+02 0.00000000000000E+00 0.11396485729698E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70197078202390E+01 0.26323904325896E+01 0.32479194005865E+03 0.40032169909588E+03
 0.30886414956894E+03 0.30886414956894E+03 0.29815000178224E+03 0.29815000174820E+03 0.30886176612126E+03
 0.30886176612126E+03 0.29815000178278E+03 0.29815000174872E+03 0.30886414956894E+03 0.30886414956894E+03
 0.29815000178224E+03 0.29815000174820E+03 0.30886176612125E+03 0.30886176612125E+03 0.29815000178278E+03
 0.29815000174872E+03 0.30726521619296E+03 0.29815001226728E+03 0.55936171630084E+02 0.68947368549488E+02
 0.17630579553091E+03 0.42296406245133E+03 0.24577673794277E+03 0.13422287091441E+03 0.16434910559063E+03
 0.13422287091441E+03 0.33719749656054E+03 0.13423395686384E+03 0.16421361092849E+03 0.13423395686384E+03
 0.33708040736104E+03 0.13422287091441E+03 0.16434910559063E+03 0.13422287091441E+03 0.33719749656054E+03
 0.13423395686384E+03 0.16421361092849E+03 0.13423395686384E+03 0.33708040736104E+03 0.21920394170442E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36059957487162E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19536386640536E+00 0.00000000000000E+00 0.00000000000000E+00 0.19536386640536E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20408274048825E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20408274048825E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638828084119E+00 0.20582659334808E+00 0.33584691232750E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    651.34072958
 0.11678990364527E+00 0.31441000894572E+03 0.44458589190347E+03 0.43951814896094E+03 0.43753361047628E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15533082504517E+00 0.00000000000000E+00 -.23211873448671E+02
 0.30328254974300E-02 0.10641794237831E+01 0.26378042544087E+04 0.98917659540325E+03 0.75175293011777E+01
 0.28190734879416E+01 0.33936273661728E+03 0.29815008491087E+03 0.33324362260313E+03 0.35839669431702E+03
 0.29815000888416E+03 0.29815001651705E+03 0.32965695063933E+03 0.35837135517107E+03 0.29815000717939E+03
 0.29815001646231E+03 0.33324362260313E+03 0.35839669431702E+03 0.29815000888416E+03 0.29815001651705E+03
 0.32965695063933E+03 0.35837135517107E+03 0.29815000717939E+03 0.29815001646231E+03 0.40102063980666E+03
 0.32552375215812E+03 0.19212552291709E+04 0.17959533413431E+04 0.60459085893061E+03 0.10158016851344E+04
 0.40818787190916E+03 0.11357910686568E+04 0.10905202829247E+04 0.10448024138847E+04 0.17168950104516E+04
 0.10248402015215E+04 0.10901259111753E+04 0.95522854451614E+03 0.17167282785584E+04 0.11357910686568E+04
 0.10905202829247E+04 0.10448024138847E+04 0.17168950104516E+04 0.10248402015215E+04 0.10901259111753E+04
 0.95522854451614E+03 0.17167282785584E+04 0.16883841229748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48332000540375E+03 0.12948755140006E+01
 0.12948755140006E+01 0.24253821678730E+01 0.00000000000000E+00 0.33602709440909E+03 0.33602709440909E+03
 0.33602709440909E+03 0.33602709440909E+03 0.00000000000000E+00 0.00000000000000E+00 0.16871206422979E+00
 0.00000000000000E+00 -.15608038047799E+02 0.00000000000000E+00 0.11546979734398E+01 0.80000000000000E+04
 0.30000000000000E+04 0.69282186199467E+01 0.25980819824800E+01 0.32552379202219E+03 0.40102204701630E+03
 0.30900898954031E+03 0.30900898954031E+03 0.29815000217211E+03 0.29815000213062E+03 0.30900653459391E+03
 0.30900653459391E+03 0.29815000217276E+03 0.29815000213126E+03 0.30900898954031E+03 0.30900898954031E+03
 0.29815000217211E+03 0.29815000213062E+03 0.30900653459391E+03 0.30900653459391E+03 0.29815000217276E+03
 0.29815000213126E+03 0.30739002616410E+03 0.29815001463569E+03 0.52756144807321E+02 0.64957957219880E+02
 0.17763821186305E+03 0.42483737645617E+03 0.24631097353381E+03 0.13673759601800E+03 0.16551119053442E+03
 0.13673759601800E+03 0.33859933430903E+03 0.13674907189922E+03 0.16537309576557E+03 0.13674907189922E+03
 0.33848019872548E+03 0.13673759601800E+03 0.16551119053442E+03 0.13673759601800E+03 0.33859933430903E+03
 0.13674907189922E+03 0.16537309576558E+03 0.13674907189922E+03 0.33848019872549E+03 0.21911638410494E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36081820130374E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19516302258142E+00 0.00000000000000E+00 0.00000000000000E+00 0.19516302258142E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20387530881413E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20387530881413E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638890192780E+00 0.20571694074876E+00 0.33602709440909E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    660.34923336
 0.11663970103757E+00 0.31458833309994E+03 0.44490584351525E+03 0.43983911169698E+03 0.43785380886643E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15500634370677E+00 0.00000000000000E+00 -.23210744457250E+02
 0.30367307467476E-02 0.10680394451772E+01 0.26344120263438E+04 0.98790450987894E+03 0.74903600575093E+01
 0.28088850215660E+01 0.33973837305240E+03 0.29815009706078E+03 0.33356836720309E+03 0.35888016156471E+03
 0.29815001033330E+03 0.29815001922347E+03 0.32996407950453E+03 0.35885497923830E+03 0.29815000836094E+03
 0.29815001916060E+03 0.33356836720309E+03 0.35888016156471E+03 0.29815001033330E+03 0.29815001922347E+03
 0.32996407950453E+03 0.35885497923830E+03 0.29815000836094E+03 0.29815001916060E+03 0.40157582504362E+03
 0.32610261720495E+03 0.19245589594663E+04 0.17979974842238E+04 0.60349852160296E+03 0.10119686814329E+04
 0.40545266722188E+03 0.11381336562406E+04 0.10922113928414E+04 0.10462474961728E+04 0.17168939597159E+04
 0.10273788922580E+04 0.10918216657151E+04 0.95702071383021E+03 0.17167302509958E+04 0.11381336562406E+04
 0.10922113928414E+04 0.10462474961728E+04 0.17168939597159E+04 0.10273788922580E+04 0.10918216657151E+04
 0.95702071383021E+03 0.17167302509958E+04 0.16880131016699E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48364007287056E+03 0.12948755056818E+01
 0.12948755056818E+01 0.24614161829940E+01 0.00000000000000E+00 0.33617759870718E+03 0.33617759870718E+03
 0.33617759870718E+03 0.33617759870718E+03 0.00000000000000E+00 0.00000000000000E+00 0.16806819801931E+00
 0.00000000000000E+00 -.15587199854918E+02 0.00000000000000E+00 0.11663625271536E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68589309187797E+01 0.25720990945424E+01 0.32610276057854E+03 0.40157696360580E+03
 0.30912499894549E+03 0.30912499894549E+03 0.29815000253519E+03 0.29815000248676E+03 0.30912248539391E+03
 0.30912248539391E+03 0.29815000253595E+03 0.29815000248751E+03 0.30912499894549E+03 0.30912499894549E+03
 0.29815000253519E+03 0.29815000248676E+03 0.30912248539391E+03 0.30912248539391E+03 0.29815000253595E+03
 0.29815000248751E+03 0.30749004482901E+03 0.29815001679916E+03 0.50253219843573E+02 0.61833793928865E+02
 0.17871804847368E+03 0.42641219554857E+03 0.24680055683253E+03 0.13874917940629E+03 0.16645634110593E+03
 0.13874917940629E+03 0.33978344421966E+03 0.13876097662275E+03 0.16631629021075E+03 0.13876097662275E+03
 0.33966280733721E+03 0.13874917940629E+03 0.16645634110593E+03 0.13874917940629E+03 0.33978344421966E+03
 0.13876097662276E+03 0.16631629021076E+03 0.13876097662276E+03 0.33966280733722E+03 0.21909959431459E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36099906676240E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19503180802029E+00 0.00000000000000E+00 0.00000000000000E+00 0.19503180802029E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20372262453337E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20372262453337E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638924460557E+00 0.20562524385377E+00 0.33617759870718E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    670.68599080
 0.11648541142532E+00 0.31478578396131E+03 0.44527047388494E+03 0.44020395295479E+03 0.43821730546622E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15464461674317E+00 0.00000000000000E+00 -.23222723526788E+02
 0.30407527163470E-02 0.10723167714152E+01 0.26309275190292E+04 0.98659781963594E+03 0.74604820266325E+01
 0.27976807599872E+01 0.34016545344392E+03 0.29815011307719E+03 0.33393759649030E+03 0.35943059264029E+03
 0.29815001228599E+03 0.29815002287204E+03 0.33031320012508E+03 0.35940558339434E+03 0.29815000995569E+03
 0.29815002279841E+03 0.33393759649030E+03 0.35943059264029E+03 0.29815001228599E+03 0.29815002287204E+03
 0.33031320012508E+03 0.35940558339434E+03 0.29815000995569E+03 0.29815002279841E+03 0.40221322842579E+03
 0.32676548858789E+03 0.19283000838079E+04 0.18002671133362E+04 0.60214635914563E+03 0.10074459872217E+04
 0.40228889628039E+03 0.11407937342852E+04 0.10941152252116E+04 0.10478539850090E+04 0.17168773758888E+04
 0.10302632246036E+04 0.10937305609202E+04 0.95902524029654E+03 0.17167169366585E+04 0.11407937342852E+04
 0.10941152252116E+04 0.10478539850090E+04 0.17168773758888E+04 0.10302632246036E+04 0.10937305609202E+04
 0.95902524029654E+03 0.17167169366585E+04 0.16875264861404E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48400366616916E+03 0.12948755939482E+01
 0.12948755939482E+01 0.25027632127687E+01 0.00000000000000E+00 0.33635604206912E+03 0.33635604206912E+03
 0.33635604206912E+03 0.33635604206912E+03 0.00000000000000E+00 0.00000000000000E+00 0.16734851606508E+00
 0.00000000000000E+00 -.15578119721868E+02 0.10000000000000E-02 0.11793512241158E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67833905934151E+01 0.25437714725307E+01 0.32676570921778E+03 0.40221409988584E+03
 0.30925739866424E+03 0.30925739866424E+03 0.29815000306390E+03 0.29815000296876E+03 0.30925481743027E+03
 0.30925481743027E+03 0.29815000306482E+03 0.29815000296965E+03 0.30925739866424E+03 0.30925739866424E+03
 0.29815000306390E+03 0.29815000296876E+03 0.30925481743027E+03 0.30925481743027E+03 0.29815000306482E+03
 0.29815000296965E+03 0.30760418946493E+03 0.29815001966503E+03 0.47378933217172E+02 0.58265116024037E+02
 0.17997403363167E+03 0.42830298003903E+03 0.24742907623920E+03 0.14107645737675E+03 0.16755839496846E+03
 0.14107645737675E+03 0.34121133481149E+03 0.14108862636668E+03 0.16741613490299E+03 0.14108862636668E+03
 0.34108901538297E+03 0.14107645737675E+03 0.16755839496846E+03 0.14107645737675E+03 0.34121133481149E+03
 0.14108862636669E+03 0.16741613490299E+03 0.14108862636669E+03 0.34108901538298E+03 0.21912506516716E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36121337527557E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19498344870652E+00 0.00000000000000E+00 0.00000000000000E+00 0.19498344870652E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20359577479964E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20359577479964E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638913651457E+00 0.20551606101140E+00 0.33635604206912E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    681.02274825
 0.11636459284492E+00 0.31498093835207E+03 0.44563321966395E+03 0.44056545315758E+03 0.43857690270201E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15429414057706E+00 0.00000000000000E+00 -.23234909083776E+02
 0.30439095598374E-02 0.10764313186494E+01 0.26281989798762E+04 0.98557461745356E+03 0.74319651067361E+01
 0.27869869150260E+01 0.34059002218931E+03 0.29815013124228E+03 0.33430488262749E+03 0.35997640512525E+03
 0.29815001454557E+03 0.29815002709560E+03 0.33066086542758E+03 0.35995156528106E+03 0.29815001180389E+03
 0.29815002700972E+03 0.33430488262749E+03 0.35997640512525E+03 0.29815001454557E+03 0.29815002709560E+03
 0.33066086542758E+03 0.35995156528106E+03 0.29815001180389E+03 0.29815002700972E+03 0.40284004798952E+03
 0.32741962063252E+03 0.19319758449461E+04 0.18024691353187E+04 0.60084738693015E+03 0.10030792727312E+04
 0.39922764886641E+03 0.11434195750060E+04 0.10959909793424E+04 0.10494262004058E+04 0.17168625388316E+04
 0.10331096677730E+04 0.10956111284338E+04 0.96098834895354E+03 0.17167051590181E+04 0.11434195750060E+04
 0.10959909793424E+04 0.10494262004058E+04 0.17168625388316E+04 0.10331096677730E+04 0.10956111284338E+04
 0.96098834895354E+03 0.17167051590181E+04 0.16870563608305E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48436363300643E+03 0.12948756837360E+01
 0.12948756837360E+01 0.25441102425434E+01 0.00000000000000E+00 0.33654069109412E+03 0.33654069109412E+03
 0.33654069109412E+03 0.33654069109412E+03 0.00000000000000E+00 0.00000000000000E+00 0.16664854181601E+00
 0.00000000000000E+00 -.15570431732521E+02 0.10000000000000E-02 0.11919306902672E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67117996585914E+01 0.25169248719718E+01 0.32741992623620E+03 0.40284064895142E+03
 0.30939051490098E+03 0.30939051490098E+03 0.29815000364183E+03 0.29815000352874E+03 0.30938786475888E+03
 0.30938786475888E+03 0.29815000364291E+03 0.29815000352979E+03 0.30939051490098E+03 0.30939051490098E+03
 0.29815000364183E+03 0.29815000352874E+03 0.30938786475888E+03 0.30938786475888E+03 0.29815000364291E+03
 0.29815000352979E+03 0.30771902073143E+03 0.29815002292973E+03 0.44559644178209E+02 0.54781313588407E+02
 0.18124063001011E+03 0.43026126877688E+03 0.24811443561673E+03 0.14339480137717E+03 0.16867263209029E+03
 0.14339480137717E+03 0.34269451360047E+03 0.14340735094463E+03 0.16852831682647E+03 0.14340735094463E+03
 0.34257067625682E+03 0.14339480137717E+03 0.16867263209029E+03 0.14339480137717E+03 0.34269451360047E+03
 0.14340735094463E+03 0.16852831682647E+03 0.14340735094463E+03 0.34257067625682E+03 0.21920371336657E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36143434062727E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19493841424547E+00 0.00000000000000E+00 0.00000000000000E+00 0.19493841424547E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20350396348105E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20350396348105E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638896885490E+00 0.20540314121335E+00 0.33654069109412E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    691.35950569
 0.11625793150054E+00 0.31517744686653E+03 0.44599417646292E+03 0.44092468233340E+03 0.43893410946479E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15395486892398E+00 0.00000000000000E+00 -.23238560145273E+02
 0.30467019130194E-02 0.10803844871748E+01 0.26257901916212E+04 0.98467132185796E+03 0.74047712596468E+01
 0.27767892223675E+01 0.34101173449489E+03 0.29815015177743E+03 0.33466989518519E+03 0.36051770925223E+03
 0.29815001715054E+03 0.29815003196629E+03 0.33100661668959E+03 0.36049303426600E+03 0.29815001393778E+03
 0.29815003186651E+03 0.33466989518519E+03 0.36051770925223E+03 0.29815001715054E+03 0.29815003196629E+03
 0.33100661668959E+03 0.36049303426600E+03 0.29815001393778E+03 0.29815003186651E+03 0.40345994578739E+03
 0.32806795693195E+03 0.19356046544014E+04 0.18046497354772E+04 0.59955723914340E+03 0.99878486496299E+03
 0.39622983962388E+03 0.11460189159434E+04 0.10978417173717E+04 0.10509942342745E+04 0.17168516516954E+04
 0.10359273089091E+04 0.10974664330494E+04 0.96294133815118E+03 0.17166971312456E+04 0.11460189159434E+04
 0.10978417173717E+04 0.10509942342745E+04 0.17168516516954E+04 0.10359273089091E+04 0.10974664330494E+04
 0.96294133815118E+03 0.17166971312456E+04 0.16865984583432E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48472125824219E+03 0.12948757106384E+01
 0.12948757106384E+01 0.25854572723182E+01 0.00000000000000E+00 0.33672969514344E+03 0.33672969514344E+03
 0.33672969514344E+03 0.33672969514344E+03 0.00000000000000E+00 0.00000000000000E+00 0.16596782105172E+00
 0.00000000000000E+00 -.15553072030840E+02 0.00000000000000E+00 0.12041163166348E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66438764174859E+01 0.24914536565572E+01 0.32806835718206E+03 0.40346027922726E+03
 0.30952398416032E+03 0.30952398416032E+03 0.29815000431068E+03 0.29815000417682E+03 0.30952126470161E+03
 0.30952126470161E+03 0.29815000431196E+03 0.29815000417806E+03 0.30952398416032E+03 0.30952398416032E+03
 0.29815000431068E+03 0.29815000417682E+03 0.30952126470161E+03 0.30952126470161E+03 0.29815000431196E+03
 0.29815000417806E+03 0.30783420845277E+03 0.29815002663617E+03 0.41763586639459E+02 0.51343508631944E+02
 0.18251071544146E+03 0.43225911961381E+03 0.24883585059515E+03 0.14570914101055E+03 0.16979121262704E+03
 0.14570914101055E+03 0.34420832840436E+03 0.14572207487428E+03 0.16964493532090E+03 0.14572207487428E+03
 0.34408306992934E+03 0.14570914101055E+03 0.16979121262704E+03 0.14570914101055E+03 0.34420832840436E+03
 0.14572207487429E+03 0.16964493532091E+03 0.14572207487429E+03 0.34408306992935E+03 0.21931209828329E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36165657695524E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19482984898510E+00 0.00000000000000E+00 0.00000000000000E+00 0.19482984898510E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20336768351241E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20336768351241E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638911036118E+00 0.20528803295781E+00 0.33672969514344E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    701.69626313
 0.11613174796625E+00 0.31537670060878E+03 0.44635337210366E+03 0.44128318883583E+03 0.43929106782595E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15362637109128E+00 0.00000000000000E+00 -.23226792776076E+02
 0.30500120266897E-02 0.10841828157533E+01 0.26229404769537E+04 0.98360267885762E+03 0.73788293669289E+01
 0.27670610125983E+01 0.34143073806732E+03 0.29815017491953E+03 0.33503277221208E+03 0.36105471272952E+03
 0.29815002014297E+03 0.29815003756291E+03 0.33135056126044E+03 0.36103019816681E+03 0.29815001639265E+03
 0.29815003744742E+03 0.33503277221208E+03 0.36105471272952E+03 0.29815002014297E+03 0.29815003756291E+03
 0.33135056126044E+03 0.36103019816682E+03 0.29815001639265E+03 0.29815003744742E+03 0.40407372944748E+03
 0.32871105638386E+03 0.19392092017013E+04 0.18068405688462E+04 0.59826907921749E+03 0.99454818240410E+03
 0.39328775779052E+03 0.11486006790888E+04 0.10996688657234E+04 0.10525701044056E+04 0.17168432529251E+04
 0.10387262440984E+04 0.10992979270195E+04 0.96489816778846E+03 0.17166914159672E+04 0.11486006790887E+04
 0.10996688657234E+04 0.10525701044056E+04 0.17168432529251E+04 0.10387262440984E+04 0.10992979270195E+04
 0.96489816778846E+03 0.17166914159672E+04 0.16861610618399E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48507839273125E+03 0.12948756239320E+01
 0.12948756239320E+01 0.26268043020929E+01 0.00000000000000E+00 0.33692037996089E+03 0.33692037996089E+03
 0.33692037996089E+03 0.33692037996089E+03 0.00000000000000E+00 0.00000000000000E+00 0.16530613737259E+00
 0.00000000000000E+00 -.15517996161256E+02 0.00000000000000E+00 0.12159222278912E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65793681672180E+01 0.24672630627068E+01 0.32871155449205E+03 0.40407381137677E+03
 0.30965764660880E+03 0.30965764660880E+03 0.29815000508193E+03 0.29815000492413E+03 0.30965485762603E+03
 0.30965485762603E+03 0.29815000508343E+03 0.29815000492558E+03 0.30965764660880E+03 0.30965764660880E+03
 0.29815000508193E+03 0.29815000492413E+03 0.30965485762603E+03 0.30965485762603E+03 0.29815000508343E+03
 0.29815000492558E+03 0.30794961589988E+03 0.29815003083041E+03 0.38970785974859E+02 0.47927108451500E+02
 0.18376916819188E+03 0.43425340065924E+03 0.24956538662640E+03 0.14801282404719E+03 0.17089887369622E+03
 0.14801282404719E+03 0.34571604574942E+03 0.14802614485785E+03 0.17075071159925E+03 0.14802614485785E+03
 0.34558944482693E+03 0.14801282404719E+03 0.17089887369622E+03 0.14801282404719E+03 0.34571604574942E+03
 0.14802614485785E+03 0.17075071159925E+03 0.14802614485785E+03 0.34558944482693E+03 0.21942294380070E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36187597500330E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19460454214109E+00 0.00000000000000E+00 0.00000000000000E+00 0.19460454214109E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20314956059444E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20314956059444E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638983215225E+00 0.20517267244529E+00 0.33692037996089E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    710.04635807
 0.11599462660431E+00 0.31554345205736E+03 0.44664367550791E+03 0.44157470168561E+03 0.43958211171221E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15336826382933E+00 0.00000000000000E+00 -.23216873463776E+02
 0.30536173112674E-02 0.10871433564191E+01 0.26198436753948E+04 0.98244137827305E+03 0.73587351224324E+01
 0.27595256709122E+01 0.34176825684162E+03 0.29815019546848E+03 0.33532534087339E+03 0.36148516244137E+03
 0.29815002284049E+03 0.29815004260880E+03 0.33162830901672E+03 0.36146077570180E+03 0.29815001860814E+03
 0.29815004247934E+03 0.33532534087339E+03 0.36148516244137E+03 0.29815002284049E+03 0.29815004260880E+03
 0.33162830901672E+03 0.36146077570180E+03 0.29815001860814E+03 0.29815004247934E+03 0.40455885844207E+03
 0.32922199432252E+03 0.19421310902749E+04 0.18086654963860E+04 0.59734013602652E+03 0.99136299703955E+03
 0.39103616033290E+03 0.11506871586121E+04 0.11011541738805E+04 0.10538772939946E+04 0.17168777898387E+04
 0.10409863160628E+04 0.11007866270667E+04 0.96650741749163E+03 0.17167280223849E+04 0.11506871586121E+04
 0.11011541738805E+04 0.10538772939946E+04 0.17168777898387E+04 0.10409863160628E+04 0.11007866270667E+04
 0.96650741749163E+03 0.17167280223849E+04 0.16859073040271E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48536917828319E+03 0.12948755508427E+01
 0.12948755508427E+01 0.26602046818531E+01 0.00000000000000E+00 0.33707435940575E+03 0.33707435940575E+03
 0.33707435940575E+03 0.33707435940575E+03 0.00000000000000E+00 0.00000000000000E+00 0.16478538329089E+00
 0.00000000000000E+00 -.15489931316441E+02 0.10000000000000E-02 0.12251894330724E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65296025121097E+01 0.24486009420411E+01 0.32922257688450E+03 0.40455874368903E+03
 0.30976655019024E+03 0.30976655019024E+03 0.29815000564314E+03 0.29815000559978E+03 0.30976370451098E+03
 0.30976370451098E+03 0.29815000564479E+03 0.29815000560143E+03 0.30976655019024E+03 0.30976655019024E+03
 0.29815000564314E+03 0.29815000559978E+03 0.30976370451098E+03 0.30976370451098E+03 0.29815000564479E+03
 0.29815000560143E+03 0.30804375458206E+03 0.29815003456674E+03 0.36745823025114E+02 0.45216276045786E+02
 0.18476360918084E+03 0.43583428005849E+03 0.25014685283174E+03 0.14984209612165E+03 0.17177280953479E+03
 0.14984209612165E+03 0.34690765298717E+03 0.14985573448595E+03 0.17162325176702E+03 0.14985573448595E+03
 0.34678009883890E+03 0.14984209612165E+03 0.17177280953479E+03 0.14984209612165E+03 0.34690765298717E+03
 0.14985573448596E+03 0.17162325176702E+03 0.14985573448596E+03 0.34678009883890E+03 0.21950912286338E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36205453380036E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19441827540768E+00 0.00000000000000E+00 0.00000000000000E+00 0.19441827540768E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20300181145933E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20300181145933E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639040682163E+00 0.20507960413447E+00 0.33707435940575E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    720.30037879
 0.11584466432354E+00 0.31574291479757E+03 0.44699958572647E+03 0.44193112406514E+03 0.43993749120884E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15305967620340E+00 0.00000000000000E+00 -.23228179506064E+02
 0.30575699594226E-02 0.10906561342829E+01 0.26164568942556E+04 0.98117133534585E+03 0.73350341583691E+01
 0.27506378093884E+01 0.34218048262280E+03 0.29815022304742E+03 0.33568274256744E+03 0.36201105629524E+03
 0.29815002651182E+03 0.29815004947724E+03 0.33196760908413E+03 0.36198682244855E+03 0.29815002162666E+03
 0.29815004932899E+03 0.33568274256744E+03 0.36201105629524E+03 0.29815002651182E+03 0.29815004947724E+03
 0.33196760908413E+03 0.36198682244855E+03 0.29815002162666E+03 0.29815004932899E+03 0.40515566320225E+03
 0.32984855604961E+03 0.19456954880018E+04 0.18108586054465E+04 0.59613456684604E+03 0.98737264921423E+03
 0.38825740953395E+03 0.11532374364552E+04 0.11029674209851E+04 0.10554495607257E+04 0.17169315579313E+04
 0.10437490809614E+04 0.11026038256379E+04 0.96845023866064E+03 0.17167841589869E+04 0.11532374364552E+04
 0.11029674209851E+04 0.10554495607257E+04 0.17169315579313E+04 0.10437490809614E+04 0.11026038256379E+04
 0.96845023866064E+03 0.17167841589869E+04 0.16855683215958E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48572448218502E+03 0.12948756341499E+01
 0.12948756341499E+01 0.27012207647193E+01 0.00000000000000E+00 0.33726587148386E+03 0.33726587148386E+03
 0.33726587148386E+03 0.33726587148386E+03 0.00000000000000E+00 0.00000000000000E+00 0.16416192522737E+00
 0.00000000000000E+00 -.15480270574511E+02 0.00000000000000E+00 0.12362459404883E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64712042628347E+01 0.24267015985630E+01 0.32984927526602E+03 0.40515528389629E+03
 0.30989982236834E+03 0.30989982236834E+03 0.29815000664915E+03 0.29815000652188E+03 0.30989690764287E+03
 0.30989690764287E+03 0.29815000665109E+03 0.29815000652378E+03 0.30989982236834E+03 0.30989982236834E+03
 0.29815000664915E+03 0.29815000652188E+03 0.30989690764287E+03 0.30989690764287E+03 0.29815000665109E+03
 0.29815000652378E+03 0.30815897137499E+03 0.29815003959627E+03 0.33995889929295E+02 0.41881775324902E+02
 0.18598757768870E+03 0.43780974998352E+03 0.25089223440638E+03 0.15209815251231E+03 0.17284850066823E+03
 0.15209815251231E+03 0.34839821480976E+03 0.15211217802241E+03 0.17269721013548E+03 0.15211217802241E+03
 0.34826946776621E+03 0.15209815251232E+03 0.17284850066823E+03 0.15209815251232E+03 0.34839821480976E+03
 0.15211217802241E+03 0.17269721013548E+03 0.15211217802241E+03 0.34826946776621E+03 0.21962998087172E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36227782154826E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19436881893567E+00 0.00000000000000E+00 0.00000000000000E+00 0.19436881893567E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20286288672802E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20286288672802E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639029037272E+00 0.20496305034420E+00 0.33726587148386E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    731.59783320
 0.11571069582872E+00 0.31595373181571E+03 0.44738793632199E+03 0.44231848856895E+03 0.44032299549257E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15273040949634E+00 0.00000000000000E+00 -.23238882921939E+02
 0.30611096596941E-02 0.10943772188650E+01 0.26134313661927E+04 0.98003676232227E+03 0.73100936880769E+01
 0.27412851330288E+01 0.34263105367587E+03 0.29815025737262E+03 0.33607349463576E+03 0.36258599641481E+03
 0.29815003117017E+03 0.29815005819345E+03 0.33233864509972E+03 0.36256192542856E+03 0.29815002546234E+03
 0.29815005802177E+03 0.33607349463576E+03 0.36258599641481E+03 0.29815003117017E+03 0.29815005819345E+03
 0.33233864509972E+03 0.36256192542856E+03 0.29815002546234E+03 0.29815005802177E+03 0.40581009683278E+03
 0.33053613309707E+03 0.19495441592801E+04 0.18131550422010E+04 0.59473332006342E+03 0.98290191530915E+03
 0.38519492864541E+03 0.11560050317681E+04 0.11049101066333E+04 0.10571040357372E+04 0.17169481454065E+04
 0.10467489403214E+04 0.11045506324386E+04 0.97051123010091E+03 0.17168031785617E+04 0.11560050317681E+04
 0.11049101066333E+04 0.10571040357372E+04 0.17169481454064E+04 0.10467489403214E+04 0.11045506324386E+04
 0.97051123010091E+03 0.17168031785617E+04 0.16851173256236E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48611027420709E+03 0.12948757130168E+01
 0.12948757130168E+01 0.27464105823618E+01 0.00000000000000E+00 0.33747935245676E+03 0.33747935245676E+03
 0.33747935245676E+03 0.33747935245676E+03 0.00000000000000E+00 0.00000000000000E+00 0.16349495920759E+00
 0.00000000000000E+00 -.15469089913963E+02 0.00000000000000E+00 0.12480274701898E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64101153148363E+01 0.24037932430636E+01 0.33053694082371E+03 0.40580952587632E+03
 0.31004625148688E+03 0.31004625148688E+03 0.29815000784647E+03 0.29815000769628E+03 0.31004326092576E+03
 0.31004326092576E+03 0.29815000784876E+03 0.29815000769852E+03 0.31004625148688E+03 0.31004625148688E+03
 0.29815000784647E+03 0.29815000769628E+03 0.31004326092576E+03 0.31004326092576E+03 0.29815000784876E+03
 0.29815000769852E+03 0.30828558342179E+03 0.29815004588170E+03 0.30956634456617E+02 0.38215519956619E+02
 0.18733659359291E+03 0.44001682729943E+03 0.25174355073856E+03 0.15458943825276E+03 0.17403451366667E+03
 0.15458943825276E+03 0.35006505056058E+03 0.15460388993206E+03 0.17388134506034E+03 0.15460388993206E+03
 0.34993501942772E+03 0.15458943825276E+03 0.17403451366667E+03 0.15458943825276E+03 0.35006505056058E+03
 0.15460388993206E+03 0.17388134506034E+03 0.15460388993206E+03 0.34993501942772E+03 0.21977967477368E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36252680426781E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19430359987017E+00 0.00000000000000E+00 0.00000000000000E+00 0.19430359987017E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20274175354809E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20274175354809E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639017470976E+00 0.20483329783795E+00 0.33747935245676E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    740.63579673
 0.11561150028061E+00 0.31612252201835E+03 0.44769663479536E+03 0.44262614126995E+03 0.44062909976744E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15247502257136E+00 0.00000000000000E+00 -.23240597743831E+02
 0.30637358625573E-02 0.10972407110385E+01 0.26111911597113E+04 0.97919668489172E+03 0.72910163827484E+01
 0.27341311435306E+01 0.34298925574288E+03 0.29815028790249E+03 0.33638429102331E+03 0.36304246658053E+03
 0.29815003538376E+03 0.29815006607813E+03 0.33263393620582E+03 0.36301852241749E+03 0.29815002893629E+03
 0.29815006588557E+03 0.33638429102331E+03 0.36304246658053E+03 0.29815003538376E+03 0.29815006607813E+03
 0.33263393620582E+03 0.36301852241749E+03 0.29815002893629E+03 0.29815006588557E+03 0.40632855495026E+03
 0.33108203451421E+03 0.19525834851458E+04 0.18149665484981E+04 0.59360997837768E+03 0.97936235869391E+03
 0.38278433042434E+03 0.11581964893632E+04 0.11064384850272E+04 0.10584165642560E+04 0.17169528163189E+04
 0.10491245747825E+04 0.11060821496601E+04 0.97214496427596E+03 0.17168096720724E+04 0.11581964893632E+04
 0.11064384850272E+04 0.10584165642560E+04 0.17169528163189E+04 0.10491245747825E+04 0.11060821496601E+04
 0.97214496427596E+03 0.17168096720724E+04 0.16847528048815E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48641663428161E+03 0.12948757256523E+01
 0.12948757256523E+01 0.27825624364758E+01 0.00000000000000E+00 0.33765142984428E+03 0.33765142984428E+03
 0.33765142984428E+03 0.33765142984428E+03 0.00000000000000E+00 0.00000000000000E+00 0.16297606905441E+00
 0.00000000000000E+00 -.15452514951188E+02 0.00000000000000E+00 0.12571602122219E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63635485137260E+01 0.23863306926472E+01 0.33108292540403E+03 0.40632782140615E+03
 0.31016350243044E+03 0.31016350243044E+03 0.29815000893304E+03 0.29815000876205E+03 0.31016045129076E+03
 0.31016045129076E+03 0.29815000893562E+03 0.29815000876458E+03 0.31016350243044E+03 0.31016350243044E+03
 0.29815000893304E+03 0.29815000876205E+03 0.31016045129076E+03 0.31016045129076E+03 0.29815000893562E+03
 0.29815000876458E+03 0.30838701252348E+03 0.29815005149195E+03 0.28530194463727E+02 0.35302316531123E+02
 0.18841002491565E+03 0.44178547783514E+03 0.25243340279492E+03 0.15657567831159E+03 0.17497788929051E+03
 0.15657567831159E+03 0.35139936129115E+03 0.15659047152060E+03 0.17482325944597E+03 0.15659047152060E+03
 0.35126834356601E+03 0.15657567831159E+03 0.17497788929051E+03 0.15657567831159E+03 0.35139936129115E+03
 0.15659047152060E+03 0.17482325944597E+03 0.15659047152060E+03 0.35126834356602E+03 0.21990515690823E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36272517625605E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19420041251374E+00 0.00000000000000E+00 0.00000000000000E+00 0.19420041251374E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20261283293603E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20261283293603E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639033106233E+00 0.20472910508819E+00 0.33765142984428E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    751.93325114
 0.11548189398107E+00 0.31633400450637E+03 0.44808032348096E+03 0.44300888533422E+03 0.44101008961514E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15216543298109E+00 0.00000000000000E+00 -.23246118926958E+02
 0.30671740044706E-02 0.11006834095123E+01 0.26082641507588E+04 0.97809905653454E+03 0.72682116681897E+01
 0.27255793755711E+01 0.34343428904689E+03 0.29815033024261E+03 0.33677061423331E+03 0.36360887118477E+03
 0.29815004132536E+03 0.29815007719682E+03 0.33300117972120E+03 0.36358508130766E+03 0.29815003384119E+03
 0.29815007697527E+03 0.33677061423332E+03 0.36360887118477E+03 0.29815004132536E+03 0.29815007719682E+03
 0.33300117972120E+03 0.36358508130766E+03 0.29815003384119E+03 0.29815007697527E+03 0.40697112332480E+03
 0.33175967774610E+03 0.19563485916231E+04 0.18172161167607E+04 0.59219847911947E+03 0.97497380840778E+03
 0.37981433689272E+03 0.11609144620688E+04 0.11083220252549E+04 0.10600495549583E+04 0.17169534724386E+04
 0.10520713841530E+04 0.11079694290899E+04 0.97417571367345E+03 0.17168124647757E+04 0.11609144620688E+04
 0.11083220252549E+04 0.10600495549583E+04 0.17169534724386E+04 0.10520713841530E+04 0.11079694290899E+04
 0.97417571367345E+03 0.17168124647757E+04 0.16842996747534E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48679785070471E+03 0.12948757663345E+01
 0.12948757663345E+01 0.28277522541183E+01 0.00000000000000E+00 0.33786711273027E+03 0.33786711273027E+03
 0.33786711273027E+03 0.33786711273027E+03 0.00000000000000E+00 0.00000000000000E+00 0.16234541763008E+00
 0.00000000000000E+00 -.15435793305169E+02 0.00000000000000E+00 0.12682221808813E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63080429601389E+01 0.23655161100521E+01 0.33176068132602E+03 0.40697018685387E+03
 0.31031004178519E+03 0.31031004178519E+03 0.29815001047016E+03 0.29815001026975E+03 0.31030691512068E+03
 0.31030691512068E+03 0.29815001047318E+03 0.29815001027271E+03 0.31031004178519E+03 0.31031004178519E+03
 0.29815001047016E+03 0.29815001026975E+03 0.31030691512068E+03 0.31030691512068E+03 0.29815001047318E+03
 0.29815001027271E+03 0.30851383383200E+03 0.29815005929946E+03 0.25494352580326E+02 0.31674704274347E+02
 0.18973975993761E+03 0.44399087957158E+03 0.25330242083428E+03 0.15904888199304E+03 0.17614543593137E+03
 0.15904888199304E+03 0.35306139134703E+03 0.15906410233112E+03 0.17598901967459E+03 0.15906410233112E+03
 0.35292917952857E+03 0.15904888199304E+03 0.17614543593137E+03 0.15904888199304E+03 0.35306139134703E+03
 0.15906410233112E+03 0.17598901967459E+03 0.15906410233112E+03 0.35292917952858E+03 0.22006229210892E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36297413163417E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19409788420600E+00 0.00000000000000E+00 0.00000000000000E+00 0.19409788420600E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20247021820388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20247021820388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639039406169E+00 0.20459851239708E+00 0.33786711273027E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    760.97121467
 0.11538825745739E+00 0.31650131748677E+03 0.44838553742390E+03 0.44331290731568E+03 0.44131253292761E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15192526174736E+00 0.00000000000000E+00 -.23250505210854E+02
 0.30696627392571E-02 0.11033315529389E+01 0.26061494957378E+04 0.97730606090168E+03 0.72507669872133E+01
 0.27190376202050E+01 0.34378814867896E+03 0.29815036772425E+03 0.33707793380131E+03 0.36405871779999E+03
 0.29815004667107E+03 0.29815008720048E+03 0.33329347202761E+03 0.36403504804499E+03 0.29815003825969E+03
 0.29815008695322E+03 0.33707793380131E+03 0.36405871779999E+03 0.29815004667107E+03 0.29815008720048E+03
 0.33329347202761E+03 0.36403504804499E+03 0.29815003825969E+03 0.29815008695322E+03 0.40748088992562E+03
 0.33229808973766E+03 0.19593250925383E+04 0.18189795705107E+04 0.59106284019479E+03 0.97148921063886E+03
 0.37747105624310E+03 0.11630687659356E+04 0.11098073576994E+04 0.10613347025873E+04 0.17169497305075E+04
 0.10544072411074E+04 0.11094576062290E+04 0.97577603287366E+03 0.17168103187583E+04 0.11630687659356E+04
 0.11098073576994E+04 0.10613347025873E+04 0.17169497305075E+04 0.10544072411074E+04 0.11094576062290E+04
 0.97577603287366E+03 0.17168103187583E+04 0.16839303214873E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48710056411800E+03 0.12948757986543E+01
 0.12948757986543E+01 0.28639041082323E+01 0.00000000000000E+00 0.33804080920136E+03 0.33804080920136E+03
 0.33804080920136E+03 0.33804080920136E+03 0.00000000000000E+00 0.00000000000000E+00 0.16185481642949E+00
 0.00000000000000E+00 -.15422608674081E+02 0.00000000000000E+00 0.12767949762545E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62656888136171E+01 0.23496333051064E+01 0.33229918517926E+03 0.40747979782595E+03
 0.31042727352368E+03 0.31042727352368E+03 0.29815001185748E+03 0.29815001163051E+03 0.31042408661906E+03
 0.31042408661906E+03 0.29815001186087E+03 0.29815001163384E+03 0.31042727352368E+03 0.31042727352368E+03
 0.29815001185748E+03 0.29815001163051E+03 0.31042408661906E+03 0.31042408661906E+03 0.29815001186087E+03
 0.29815001163384E+03 0.30861533189262E+03 0.29815006623428E+03 0.23068705782102E+02 0.28789836175225E+02
 0.19079945607006E+03 0.44576223218763E+03 0.25400877883722E+03 0.16102276240602E+03 0.17707560772645E+03
 0.16102276240602E+03 0.35439587671007E+03 0.16103832442095E+03 0.17691779015736E+03 0.16103832442095E+03
 0.35426273650134E+03 0.16102276240602E+03 0.17707560772646E+03 0.16102276240602E+03 0.35439587671007E+03
 0.16103832442095E+03 0.17691779015736E+03 0.16103832442095E+03 0.35426273650135E+03 0.22019528259506E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36317412202705E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19401667488818E+00 0.00000000000000E+00 0.00000000000000E+00 0.19401667488818E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20235950362275E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20235950362275E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639043596442E+00 0.20449345321515E+00 0.33804080920136E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    770.00917820
 0.11529725108592E+00 0.31666813611852E+03 0.44868919264315E+03 0.44361530434223E+03 0.44161334095915E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15169160264826E+00 0.00000000000000E+00 -.23254879162793E+02
 0.30720854364683E-02 0.11058879224440E+01 0.26040942432893E+04 0.97653534123349E+03 0.72340061209097E+01
 0.27127522953411E+01 0.34414011788132E+03 0.29815040866424E+03 0.33738374113430E+03 0.36450569995982E+03
 0.29815005259352E+03 0.29815009828316E+03 0.33358445739071E+03 0.36448214746972E+03 0.29815004316027E+03
 0.29815009800780E+03 0.33738374113430E+03 0.36450569995982E+03 0.29815005259352E+03 0.29815009828316E+03
 0.33358445739071E+03 0.36448214746972E+03 0.29815004316027E+03 0.29815009800780E+03 0.40798690006091E+03
 0.33283323848663E+03 0.19622741076918E+04 0.18207230692438E+04 0.58992160623900E+03 0.96802745041489E+03
 0.37515623614470E+03 0.11652068178960E+04 0.11112737087891E+04 0.10626088740058E+04 0.17169415484356E+04
 0.10567257055607E+04 0.11109266796036E+04 0.97736269582966E+03 0.17168036388816E+04 0.11652068178960E+04
 0.11112737087891E+04 0.10626088740058E+04 0.17169415484356E+04 0.10567257055607E+04 0.11109266796036E+04
 0.97736269582966E+03 0.17168036388816E+04 0.16835583838016E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48740164668955E+03 0.12948758308832E+01
 0.12948758308832E+01 0.29000559623463E+01 0.00000000000000E+00 0.33821505904003E+03 0.33821505904003E+03
 0.33821505904003E+03 0.33821505904003E+03 0.00000000000000E+00 0.00000000000000E+00 0.16137631085165E+00
 0.00000000000000E+00 -.15409546807345E+02 0.00000000000000E+00 0.12851291130058E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62250554586602E+01 0.23343957969976E+01 0.33283442535440E+03 0.40798565945346E+03
 0.31054449036382E+03 0.31054449036382E+03 0.29815001339869E+03 0.29815001314222E+03 0.31054124338835E+03
 0.31054124338835E+03 0.29815001340250E+03 0.29815001314596E+03 0.31054449036382E+03 0.31054449036382E+03
 0.29815001339869E+03 0.29815001314222E+03 0.31054124338835E+03 0.31054124338835E+03 0.29815001340250E+03
 0.29815001314596E+03 0.30871685468829E+03 0.29815007383110E+03 0.20643241743000E+02 0.25917237582874E+02
 0.19185307011887E+03 0.44753272775365E+03 0.25472039228418E+03 0.16299106897031E+03 0.17799987154349E+03
 0.16299106897031E+03 0.35572872499662E+03 0.16300697260942E+03 0.17784067451689E+03 0.16300697260942E+03
 0.35559467726727E+03 0.16299106897031E+03 0.17799987154349E+03 0.16299106897031E+03 0.35572872499663E+03
 0.16300697260942E+03 0.17784067451689E+03 0.16300697260942E+03 0.35559467726728E+03 0.22033052345223E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36337437076545E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19393611578539E+00 0.00000000000000E+00 0.00000000000000E+00 0.19393611578539E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20225034260811E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20225034260811E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639047297762E+00 0.20438816219865E+00 0.33821505904003E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    781.92830517
 0.11519631131138E+00 0.31687878528621E+03 0.44908525139982E+03 0.44400868565722E+03 0.44200411174652E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15139322461764E+00 0.00000000000000E+00 -.23275407848883E+02
 0.30747770046767E-02 0.11091267379522E+01 0.26018146967510E+04 0.97568051128163E+03 0.72128817440379E+01
 0.27048306540142E+01 0.34459899745537E+03 0.29815047019531E+03 0.33778231123256E+03 0.36509041114262E+03
 0.29815006169031E+03 0.29815011530416E+03 0.33396333151974E+03 0.36506700623161E+03 0.29815005070019E+03
 0.29815011498651E+03 0.33778231123256E+03 0.36509041114262E+03 0.29815006169031E+03 0.29815011530416E+03
 0.33396333151974E+03 0.36506700623161E+03 0.29815005070019E+03 0.29815011498651E+03 0.40866098568687E+03
 0.33354413325947E+03 0.19661024603982E+04 0.18229284203881E+04 0.58820088116949E+03 0.96313054940823E+03
 0.37198866383290E+03 0.11679916937337E+04 0.11131511950324E+04 0.10642250242901E+04 0.17168836661209E+04
 0.10597492963122E+04 0.11128075249651E+04 0.97939396521920E+03 0.17167475737561E+04 0.11679916937337E+04
 0.11131511950324E+04 0.10642250242900E+04 0.17168836661209E+04 0.10597492963122E+04 0.11128075249651E+04
 0.97939396521920E+03 0.17167475737561E+04 0.16829343928191E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48779305839052E+03 0.12948759821465E+01
 0.12948759821465E+01 0.29477324702366E+01 0.00000000000000E+00 0.33844590996203E+03 0.33844590996203E+03
 0.33844590996203E+03 0.33844590996203E+03 0.00000000000000E+00 0.00000000000000E+00 0.16076326779878E+00
 0.00000000000000E+00 -.15408231469596E+02 0.10000000000000E-02 0.12957639864606E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61739638418660E+01 0.23152364406997E+01 0.33354540730042E+03 0.40865960043797E+03
 0.31069681234895E+03 0.31069681234895E+03 0.29815001617319E+03 0.29815001547395E+03 0.31069348745620E+03
 0.31069348745620E+03 0.29815001617775E+03 0.29815001547831E+03 0.31069681234895E+03 0.31069681234895E+03
 0.29815001617319E+03 0.29815001547395E+03 0.31069348745620E+03 0.31069348745620E+03 0.29815001617775E+03
 0.29815001547831E+03 0.30884872503088E+03 0.29815008529926E+03 0.17368168207401E+02 0.22059314979258E+02
 0.19325119606137E+03 0.44990973077727E+03 0.25569227873560E+03 0.16562589029786E+03 0.17922675304526E+03
 0.16562589029786E+03 0.35752180358697E+03 0.16564223639434E+03 0.17906561983595E+03 0.16564223639434E+03
 0.35738643322402E+03 0.16562589029786E+03 0.17922675304526E+03 0.16562589029786E+03 0.35752180358697E+03
 0.16564223639434E+03 0.17906561983595E+03 0.16564223639434E+03 0.35738643322402E+03 0.22051286761410E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36364001285531E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19394475755266E+00 0.00000000000000E+00 0.00000000000000E+00 0.19394475755266E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20213635277547E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20213635277547E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638999590229E+00 0.20424825735290E+00 0.33844590996203E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    791.58794126
 0.11514996382082E+00 0.31704388343870E+03 0.44940300472765E+03 0.44432262205173E+03 0.44231524549962E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15115963701360E+00 0.00000000000000E+00 -.23298459922745E+02
 0.30760143396698E-02 0.11116392221942E+01 0.26007681098322E+04 0.97528804118709E+03 0.71965794659611E+01
 0.26987172997354E+01 0.34496896415700E+03 0.29815052579375E+03 0.33810392635031E+03 0.36556065334013E+03
 0.29815007006143E+03 0.29815013096512E+03 0.33426941044212E+03 0.36553736547830E+03 0.29815005764850E+03
 0.29815013060925E+03 0.33810392635031E+03 0.36556065334013E+03 0.29815007006143E+03 0.29815013096512E+03
 0.33426941044212E+03 0.36553736547830E+03 0.29815005764850E+03 0.29815013060925E+03 0.40919692209090E+03
 0.33411212604362E+03 0.19691244911101E+04 0.18246058173596E+04 0.58685552241749E+03 0.95929406647125E+03
 0.36950426644167E+03 0.11702060637661E+04 0.11146261901618E+04 0.10654688352445E+04 0.17167991578510E+04
 0.10621536754664E+04 0.11142851115142E+04 0.98096868804514E+03 0.17166644336308E+04 0.11702060637661E+04
 0.11146261901618E+04 0.10654688352445E+04 0.17167991578510E+04 0.10621536754664E+04 0.11142851115142E+04
 0.98096868804514E+03 0.17166644336308E+04 0.16824058263682E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48810506282732E+03 0.12948761520032E+01
 0.12948761520032E+01 0.29863710145984E+01 0.00000000000000E+00 0.33863524674009E+03 0.33863524674009E+03
 0.33863524674009E+03 0.33863524674009E+03 0.00000000000000E+00 0.00000000000000E+00 0.16028092796151E+00
 0.00000000000000E+00 -.15416820508975E+02 0.10000000000000E-02 0.13040909779349E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61345413282964E+01 0.23004529981112E+01 0.33411348049629E+03 0.40919541123111E+03
 0.31082098584651E+03 0.31082098584651E+03 0.29815001842378E+03 0.29815001762724E+03 0.31081759728487E+03
 0.31081759728487E+03 0.29815001842893E+03 0.29815001763216E+03 0.31082098584651E+03 0.31082098584651E+03
 0.29815001842378E+03 0.29815001762724E+03 0.31081759728487E+03 0.31081759728487E+03 0.29815001842893E+03
 0.29815001763216E+03 0.30895628050076E+03 0.29815009569983E+03 0.14758593090082E+02 0.19000058741667E+02
 0.19438414816095E+03 0.45185897453432E+03 0.25650290563257E+03 0.16774369541992E+03 0.18022149895075E+03
 0.16774369541992E+03 0.35899431727793E+03 0.16776040491996E+03 0.18005888616883E+03 0.16776040491996E+03
 0.35885797012515E+03 0.16774369541992E+03 0.18022149895075E+03 0.16774369541992E+03 0.35899431727793E+03
 0.16776040491996E+03 0.18005888616884E+03 0.16776040491996E+03 0.35885797012516E+03 0.22068810151128E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36386174823618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19400294383464E+00 0.00000000000000E+00 0.00000000000000E+00 0.19400294383464E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20214827313363E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20214827313363E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19638928666165E+00 0.20413330412705E+00 0.33863524674009E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    800.64495073
 0.11503340779544E+00 0.31722548863329E+03 0.44970430541150E+03 0.44462447549327E+03 0.44261654962995E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15094681205642E+00 0.00000000000000E+00 -.23263733464109E+02
 0.30791308196167E-02 0.11138967397209E+01 0.25981357950215E+04 0.97430092313305E+03 0.71819942681625E+01
 0.26932478505609E+01 0.34531692416076E+03 0.29815058026126E+03 0.33840700227364E+03 0.36599822170908E+03
 0.29815007832077E+03 0.29815014641600E+03 0.33455881028057E+03 0.36597504385163E+03 0.29815006450780E+03
 0.29815014602266E+03 0.33840700227364E+03 0.36599822170908E+03 0.29815007832077E+03 0.29815014641600E+03
 0.33455881028057E+03 0.36597504385163E+03 0.29815006450780E+03 0.29815014602266E+03 0.40967671645225E+03
 0.33462666696091E+03 0.19720311439935E+04 0.18264339298180E+04 0.58594028795667E+03 0.95633419129313E+03
 0.36746420189668E+03 0.11723142823249E+04 0.11160777028342E+04 0.10668166872383E+04 0.17168459283389E+04
 0.10644363023579E+04 0.11157390393948E+04 0.98261103176056E+03 0.17167124752254E+04 0.11723142823249E+04
 0.11160777028342E+04 0.10668166872383E+04 0.17168459283389E+04 0.10644363023579E+04 0.11157390393948E+04
 0.98261103176056E+03 0.17167124752254E+04 0.16822111726136E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48840607823323E+03 0.12948758961252E+01
 0.12948758961252E+01 0.30225990524704E+01 0.00000000000000E+00 0.33881033062231E+03 0.33881033062231E+03
 0.33881033062231E+03 0.33881033062231E+03 0.00000000000000E+00 0.00000000000000E+00 0.15984051837557E+00
 0.00000000000000E+00 -.15360554901251E+02 0.00000000000000E+00 0.13116778743854E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60990584321232E+01 0.22871469120462E+01 0.33462817322395E+03 0.40967498889328E+03
 0.31094056214358E+03 0.31094056214358E+03 0.29815001999005E+03 0.29815001975465E+03 0.31093711296712E+03
 0.31093711296712E+03 0.29815001999560E+03 0.29815001976014E+03 0.31094056214358E+03 0.31094056214358E+03
 0.29815001999005E+03 0.29815001975465E+03 0.31093711296712E+03 0.31093711296712E+03 0.29815001999560E+03
 0.29815001976014E+03 0.30906009945281E+03 0.29815010590343E+03 0.12408337727230E+02 0.16254625610630E+02
 0.19539853196394E+03 0.45356315355023E+03 0.25718762892647E+03 0.16964512212714E+03 0.18110774947019E+03
 0.16964512212714E+03 0.36026594838886E+03 0.16966218154310E+03 0.18094396575806E+03 0.16966218154310E+03
 0.36012890549212E+03 0.16964512212714E+03 0.18110774947019E+03 0.16964512212714E+03 0.36026594838886E+03
 0.16966218154310E+03 0.18094396575806E+03 0.16966218154310E+03 0.36012890549212E+03 0.22081960636765E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36405512740558E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19362467146244E+00 0.00000000000000E+00 0.00000000000000E+00 0.19362467146244E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20189367741438E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20189367741438E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639074643268E+00 0.20402944594883E+00 0.33881033062231E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    810.44792933
 0.11492198946725E+00 0.31740763593048E+03 0.45002797051999E+03 0.44494763962171E+03 0.44293843156383E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15072256841734E+00 0.00000000000000E+00 -.23270851062919E+02
 0.30821158109807E-02 0.11162563058348E+01 0.25956195323674E+04 0.97335732463778E+03 0.71668128172566E+01
 0.26875548064712E+01 0.34569065481505E+03 0.29815064452898E+03 0.33873241565828E+03 0.36646992734541E+03
 0.29815008820303E+03 0.29815016490027E+03 0.33486918244800E+03 0.36644686447199E+03 0.29815007272377E+03
 0.29815016446272E+03 0.33873241565828E+03 0.36646992734541E+03 0.29815008820303E+03 0.29815016490027E+03
 0.33486918244800E+03 0.36644686447199E+03 0.29815007272377E+03 0.29815016446272E+03 0.41020580796013E+03
 0.33518935560483E+03 0.19751416022109E+04 0.18282921968713E+04 0.58474188490394E+03 0.95279260151895E+03
 0.36512700719050E+03 0.11745771989918E+04 0.11176094397478E+04 0.10681812497504E+04 0.17168558188235E+04
 0.10668896766621E+04 0.11172732163671E+04 0.98430142824008E+03 0.17167236057442E+04 0.11745771989918E+04
 0.11176094397478E+04 0.10681812497504E+04 0.17168558188235E+04 0.10668896766621E+04 0.11172732163671E+04
 0.98430142824008E+03 0.17167236057442E+04 0.16818627620279E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48872802919950E+03 0.12948759485704E+01
 0.12948759485704E+01 0.30618109668794E+01 0.00000000000000E+00 0.33900007865431E+03 0.33900007865431E+03
 0.33900007865431E+03 0.33900007865431E+03 0.00000000000000E+00 0.00000000000000E+00 0.15937622558162E+00
 0.00000000000000E+00 -.15349154234631E+02 0.00000000000000E+00 0.13196465903743E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60622291288843E+01 0.22733359233316E+01 0.33519096530400E+03 0.41020394396146E+03
 0.31106798806800E+03 0.31106798806800E+03 0.29815002257268E+03 0.29815002230687E+03 0.31106447441186E+03
 0.31106447441186E+03 0.29815002257891E+03 0.29815002231303E+03 0.31106798806800E+03 0.31106798806800E+03
 0.29815002257268E+03 0.29815002230687E+03 0.31106447441186E+03 0.31106447441186E+03 0.29815002257891E+03
 0.29815002231303E+03 0.30917067322715E+03 0.29815011797670E+03 0.97911756321333E+01 0.13212734557440E+02
 0.19650322354155E+03 0.45544797036621E+03 0.25796223070695E+03 0.17173849583088E+03 0.18207281890453E+03
 0.17173849583088E+03 0.36167760513439E+03 0.17175592677128E+03 0.18190764224333E+03 0.17175592677128E+03
 0.36153967709777E+03 0.17173849583088E+03 0.18207281890453E+03 0.17173849583088E+03 0.36167760513440E+03
 0.17175592677128E+03 0.18190764224333E+03 0.17175592677128E+03 0.36153967709778E+03 0.22096529935760E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36427210183643E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19355757811677E+00 0.00000000000000E+00 0.00000000000000E+00 0.19355757811677E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20177952835942E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20177952835942E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639069570376E+00 0.20391521511971E+00 0.33900007865431E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    821.49331602
 0.11481502658532E+00 0.31760429793659E+03 0.45038936992806E+03 0.44530746273778E+03 0.44329629640618E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15047749121073E+00 0.00000000000000E+00 -.23283174805530E+02
 0.30849868489803E-02 0.11188121819293E+01 0.25932039232661E+04 0.97245147122478E+03 0.71504405558083E+01
 0.26814152084281E+01 0.34610844264546E+03 0.29815072457744E+03 0.33909620237213E+03 0.36699787066573E+03
 0.29815010071531E+03 0.29815018829904E+03 0.33521608453442E+03 0.36697493301389E+03 0.29815008313955E+03
 0.29815018780642E+03 0.33909620237213E+03 0.36699787066573E+03 0.29815010071531E+03 0.29815018829904E+03
 0.33521608453442E+03 0.36697493301389E+03 0.29815008313955E+03 0.29815018780642E+03 0.41080264329918E+03
 0.33582329780947E+03 0.19785922020717E+04 0.18302909892355E+04 0.58328822222494E+03 0.94866046533211E+03
 0.36245580199605E+03 0.11770972418414E+04 0.11192913931693E+04 0.10696530767790E+04 0.17168301474257E+04
 0.10696237371739E+04 0.11189577630631E+04 0.98614106543196E+03 0.17166992172819E+04 0.11770972418414E+04
 0.11192913931693E+04 0.10696530767790E+04 0.17168301474257E+04 0.10696237371739E+04 0.11189577630631E+04
 0.98614106543196E+03 0.17166992172819E+04 0.16813880117184E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48908624819217E+03 0.12948760393765E+01
 0.12948760393765E+01 0.31059925136186E+01 0.00000000000000E+00 0.33921437505129E+03 0.33921437505129E+03
 0.33921437505129E+03 0.33921437505129E+03 0.00000000000000E+00 0.00000000000000E+00 0.15886807356891E+00
 0.00000000000000E+00 -.15341456747316E+02 0.00000000000000E+00 0.13283313957763E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60225934773787E+01 0.22584725540170E+01 0.33582497474452E+03 0.41080068734716E+03
 0.31121052405411E+03 0.31121052405411E+03 0.29815002618068E+03 0.29815002554842E+03 0.31120693830871E+03
 0.31120693830871E+03 0.29815002618783E+03 0.29815002555541E+03 0.31121052405411E+03 0.31121052405411E+03
 0.29815002618068E+03 0.29815002554842E+03 0.31120693830871E+03 0.31120693830871E+03 0.29815002618783E+03
 0.29815002555541E+03 0.30929435670887E+03 0.29815013306375E+03 0.68108152294724E+01 0.97670448424704E+01
 0.19774854432151E+03 0.45758805694071E+03 0.25885076989759E+03 0.17411035907595E+03 0.18316055959497E+03
 0.17411035907595E+03 0.36328172776587E+03 0.17412820581886E+03 0.18299376994687E+03 0.17412820581886E+03
 0.36314275537341E+03 0.17411035907595E+03 0.18316055959497E+03 0.17411035907595E+03 0.36328172776587E+03
 0.17412820581886E+03 0.18299376994687E+03 0.17412820581886E+03 0.36314275537341E+03 0.22113306607063E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36451742586373E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19351686183208E+00 0.00000000000000E+00 0.00000000000000E+00 0.19351686183208E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20167183786633E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20167183786633E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639046840366E+00 0.20378617268503E+00 0.33921437505129E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    831.44948286
 0.11473238107424E+00 0.31778073444905E+03 0.45071306927214E+03 0.44562918817347E+03 0.44361606469668E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15026345654978E+00 0.00000000000000E+00 -.23290092658483E+02
 0.30872088051777E-02 0.11210209722087E+01 0.25913375171070E+04 0.97175156891512E+03 0.71363517706884E+01
 0.26761319140081E+01 0.34648295558348E+03 0.29815080361598E+03 0.33942248633316E+03 0.36747029424598E+03
 0.29815011325643E+03 0.29815021174650E+03 0.33552745435704E+03 0.36744746651681E+03 0.29815009359148E+03
 0.29815021119951E+03 0.33942248633316E+03 0.36747029424598E+03 0.29815011325643E+03 0.29815021174650E+03
 0.33552745435704E+03 0.36744746651681E+03 0.29815009359148E+03 0.29815021119951E+03 0.41133411392103E+03
 0.33638921233747E+03 0.19816609314634E+04 0.18320572907724E+04 0.58200047277679E+03 0.94501140583909E+03
 0.36010093069841E+03 0.11793457134775E+04 0.11207845041152E+04 0.10709622769991E+04 0.17168002508113E+04
 0.10720629532681E+04 0.11204531024756E+04 0.98777717096023E+03 0.17166703977136E+04 0.11793457134775E+04
 0.11207845041152E+04 0.10709622769991E+04 0.17168002508113E+04 0.10720629532681E+04 0.11204531024756E+04
 0.98777717096023E+03 0.17166703977136E+04 0.16809634370709E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48940642995858E+03 0.12948760903499E+01
 0.12948760903499E+01 0.31458171809789E+01 0.00000000000000E+00 0.33940823732294E+03 0.33940823732294E+03
 0.33940823732294E+03 0.33940823732294E+03 0.00000000000000E+00 0.00000000000000E+00 0.15842323154124E+00
 0.00000000000000E+00 -.15330481686471E+02 0.00000000000000E+00 0.13359001826276E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59884713723632E+01 0.22456767646362E+01 0.33639096074012E+03 0.41133205875421E+03
 0.31133922466630E+03 0.31133922466630E+03 0.29815002951959E+03 0.29815002880670E+03 0.31133557397114E+03
 0.31133557397114E+03 0.29815002952759E+03 0.29815002881451E+03 0.31133922466630E+03 0.31133922466630E+03
 0.29815002951959E+03 0.29815002880670E+03 0.31133557397114E+03 0.31133557397114E+03 0.29815002952759E+03
 0.29815002881451E+03 0.30940609578172E+03 0.29815014800479E+03 0.41414669079182E+01 0.66971371088047E+01
 0.19886587752040E+03 0.45951607136916E+03 0.25965586446116E+03 0.17623638156281E+03 0.18413607930003E+03
 0.17623638156281E+03 0.36472611192026E+03 0.17625460432133E+03 0.18396786957331E+03 0.17625460432133E+03
 0.36458623212737E+03 0.17623638156281E+03 0.18413607930003E+03 0.17623638156281E+03 0.36472611192026E+03
 0.17625460432133E+03 0.18396786957332E+03 0.17625460432133E+03 0.36458623212737E+03 0.22129221803564E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36473877887670E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19344942917561E+00 0.00000000000000E+00 0.00000000000000E+00 0.19344942917561E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20157483062740E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20157483062740E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639039545251E+00 0.20366972107092E+00 0.33940823732294E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    841.40564970
 0.11464649205680E+00 0.31795854879157E+03 0.45103509912884E+03 0.44594951257178E+03 0.44393458936594E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15005577223000E+00 0.00000000000000E+00 -.23290967777925E+02
 0.30895213701927E-02 0.11231415519866E+01 0.25893978521019E+04 0.97102419453823E+03 0.71228777760469E+01
 0.26710791660176E+01 0.34685547608843E+03 0.29815088956243E+03 0.33974719154429E+03 0.36793961620723E+03
 0.29815012708393E+03 0.29815023759296E+03 0.33583747044795E+03 0.36791689557789E+03 0.29815010512792E+03
 0.29815023698686E+03 0.33974719154429E+03 0.36793961620723E+03 0.29815012708393E+03 0.29815023759296E+03
 0.33583747044795E+03 0.36791689557789E+03 0.29815010512792E+03 0.29815023698686E+03 0.41186122245784E+03
 0.33695111481862E+03 0.19847067488104E+04 0.18338224024437E+04 0.58071502225189E+03 0.94139868263519E+03
 0.35778008527205E+03 0.11815796738937E+04 0.11222603914476E+04 0.10722738294407E+04 0.17167687510029E+04
 0.10744865304195E+04 0.11219311161258E+04 0.98941231132908E+03 0.17166399009590E+04 0.11815796738937E+04
 0.11222603914476E+04 0.10722738294407E+04 0.17167687510029E+04 0.10744865304195E+04 0.11219311161259E+04
 0.98941231132908E+03 0.17166399009590E+04 0.16805467698152E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48972528450725E+03 0.12948760967982E+01
 0.12948760967982E+01 0.31856418483393E+01 0.00000000000000E+00 0.33960189562473E+03 0.33960189562473E+03
 0.33960189562473E+03 0.33960189562473E+03 0.00000000000000E+00 0.00000000000000E+00 0.15799062942388E+00
 0.00000000000000E+00 -.15312660618615E+02 0.00000000000000E+00 0.13432318864521E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59557847611337E+01 0.22334192854251E+01 0.33695294310194E+03 0.41185905865979E+03
 0.31146788555901E+03 0.31146788555901E+03 0.29815003321065E+03 0.29815003240862E+03 0.31146417007840E+03
 0.31146417007840E+03 0.29815003321957E+03 0.29815003241733E+03 0.31146788555901E+03 0.31146788555901E+03
 0.29815003321065E+03 0.29815003240862E+03 0.31146417007840E+03 0.31146417007840E+03 0.29815003321957E+03
 0.29815003241733E+03 0.30951784892639E+03 0.29815016429610E+03 0.14732208752712E+01 0.36445731734265E+01
 0.19997432658749E+03 0.46143044994599E+03 0.26045625172557E+03 0.17835320539295E+03 0.18510276056844E+03
 0.17835320539295E+03 0.36615796921743E+03 0.17837180438282E+03 0.18493314215152E+03 0.17837180438282E+03
 0.36601719205576E+03 0.17835320539295E+03 0.18510276056844E+03 0.17835320539295E+03 0.36615796921743E+03
 0.17837180438282E+03 0.18493314215152E+03 0.17837180438282E+03 0.36601719205576E+03 0.22144850305449E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36495811424024E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19333645549910E+00 0.00000000000000E+00 0.00000000000000E+00 0.19333645549910E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20144685502226E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20144685502226E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639054897994E+00 0.20355377316617E+00 0.33960189562473E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    851.36181654
 0.11454317002942E+00 0.31813811136683E+03 0.45135571580294E+03 0.44626932689766E+03 0.44425300328189E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14985406793996E+00 0.00000000000000E+00 -.23285054963449E+02
 0.30923079675345E-02 0.11251787074407E+01 0.25870644463586E+04 0.97014916738449E+03 0.71099816829957E+01
 0.26662431311234E+01 0.34722610734284E+03 0.29815098286284E+03 0.34007040185233E+03 0.36840594059185E+03
 0.29815014230046E+03 0.29815026602841E+03 0.33614620645165E+03 0.36838332433623E+03 0.29815011783668E+03
 0.29815026535817E+03 0.34007040185233E+03 0.36840594059185E+03 0.29815014230046E+03 0.29815026602841E+03
 0.33614620645165E+03 0.36838332433623E+03 0.29815011783668E+03 0.29815026535817E+03 0.41238429436601E+03
 0.33750911506238E+03 0.19877433604710E+04 0.18356021369561E+04 0.57943339331623E+03 0.93782331131167E+03
 0.35549275102886E+03 0.11838052920544E+04 0.11237238384179E+04 0.10735933879778E+04 0.17167410847143E+04
 0.10769011236581E+04 0.11233965987385E+04 0.99105308332175E+03 0.17166131741675E+04 0.11838052920544E+04
 0.11237238384179E+04 0.10735933879778E+04 0.17167410847143E+04 0.10769011236581E+04 0.11233965987385E+04
 0.99105308332175E+03 0.17166131741675E+04 0.16801472425357E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49004382488242E+03 0.12948760532302E+01
 0.12948760532302E+01 0.32254665156996E+01 0.00000000000000E+00 0.33979446605575E+03 0.33979446605575E+03
 0.33979446605575E+03 0.33979446605575E+03 0.00000000000000E+00 0.00000000000000E+00 0.15757003604189E+00
 0.00000000000000E+00 -.15287018573753E+02 0.00000000000000E+00 0.13503348058536E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59244566349920E+01 0.22216712381220E+01 0.33751102262278E+03 0.41238202563834E+03
 0.31159641843081E+03 0.31159641843081E+03 0.29815003728294E+03 0.29815003638257E+03 0.31159263832176E+03
 0.31159263832176E+03 0.29815003729287E+03 0.29815003639225E+03 0.31159641843081E+03 0.31159641843081E+03
 0.29815003728294E+03 0.29815003638257E+03 0.31159263832176E+03 0.31159263832176E+03 0.29815003729287E+03
 0.29815003639225E+03 0.30962954169941E+03 0.29815018202867E+03 -.12002938807308E+01 0.60283897993359E+00
 0.20106848523780E+03 0.46331724276592E+03 0.26124341510193E+03 0.18045826763966E+03 0.18605523364563E+03
 0.18045826763966E+03 0.36756552482911E+03 0.18047724316074E+03 0.18588421531931E+03 0.18047724316074E+03
 0.36742385754283E+03 0.18045826763966E+03 0.18605523364563E+03 0.18045826763966E+03 0.36756552482911E+03
 0.18047724316074E+03 0.18588421531931E+03 0.18047724316074E+03 0.36742385754284E+03 0.22159347699339E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36517440266470E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19317171398984E+00 0.00000000000000E+00 0.00000000000000E+00 0.19317171398984E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20128170636349E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20128170636349E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639096307647E+00 0.20343889437361E+00 0.33979446605575E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    860.38019231
 0.11444351467155E+00 0.31830223722938E+03 0.45164586907823E+03 0.44655909778228E+03 0.44454167202083E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14967615208545E+00 0.00000000000000E+00 -.23293308468433E+02
 0.30950004567443E-02 0.11269547618896E+01 0.25848138350246E+04 0.96930518813422E+03 0.70987765175119E+01
 0.26620411940670E+01 0.34756080921536E+03 0.29815107347229E+03 0.34036248007409E+03 0.36882569124940E+03
 0.29815015724957E+03 0.29815029395747E+03 0.33642549967785E+03 0.36880316763545E+03 0.29815013033329E+03
 0.29815029322497E+03 0.34036248007409E+03 0.36882569124940E+03 0.29815015724957E+03 0.29815029395747E+03
 0.33642549967785E+03 0.36880316763545E+03 0.29815013033329E+03 0.29815029322497E+03 0.41285105460180E+03
 0.33800824424239E+03 0.19904907046479E+04 0.18372272302028E+04 0.57834409963235E+03 0.93473341406461E+03
 0.35349759393409E+03 0.11858180320521E+04 0.11250557671758E+04 0.10747970876587E+04 0.17167450168968E+04
 0.10790832023874E+04 0.11247303104312E+04 0.99254345525755E+03 0.17166179162719E+04 0.11858180320521E+04
 0.11250557671758E+04 0.10747970876587E+04 0.17167450168968E+04 0.10790832023874E+04 0.11247303104312E+04
 0.99254345525755E+03 0.17166179162719E+04 0.16798417972451E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49033251997910E+03 0.12948761140453E+01
 0.12948761140453E+01 0.32615400188064E+01 0.00000000000000E+00 0.33996854903126E+03 0.33996854903126E+03
 0.33996854903126E+03 0.33996854903126E+03 0.00000000000000E+00 0.00000000000000E+00 0.15719911902430E+00
 0.00000000000000E+00 -.15279643923480E+02 0.00000000000000E+00 0.13565746711536E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58972057860973E+01 0.22114521697865E+01 0.33801023115828E+03 0.41284868355784E+03
 0.31171336007545E+03 0.31171336007545E+03 0.29815004076311E+03 0.29815004029516E+03 0.31170952135071E+03
 0.31170952135071E+03 0.29815004077386E+03 0.29815004030580E+03 0.31171336007545E+03 0.31171336007545E+03
 0.29815004076311E+03 0.29815004029516E+03 0.31170952135071E+03 0.31170952135071E+03 0.29815004077386E+03
 0.29815004030580E+03 0.30973125014964E+03 0.29815019928869E+03 -.35977901523607E+01 -.21099512230049E+01
 0.20204736142476E+03 0.46501123050103E+03 0.26195363226915E+03 0.18234337698392E+03 0.18690614364646E+03
 0.18234337698392E+03 0.36882818002007E+03 0.18236269554749E+03 0.18673390217554E+03 0.18236269554749E+03
 0.36868575278571E+03 0.18234337698392E+03 0.18690614364646E+03 0.18234337698392E+03 0.36882818002007E+03
 0.18236269554749E+03 0.18673390217554E+03 0.18236269554749E+03 0.36868575278572E+03 0.22172766976463E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36537387192931E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19312649589396E+00 0.00000000000000E+00 0.00000000000000E+00 0.19312649589396E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20121104740488E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20121104740488E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639081715693E+00 0.20333458597076E+00 0.33996854903126E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    871.37347488
 0.11433588718525E+00 0.31849732839708E+03 0.45199742882872E+03 0.44690947802133E+03 0.44489036670092E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14946537162744E+00 0.00000000000000E+00 -.23294798934418E+02
 0.30979135795932E-02 0.11290365772744E+01 0.25823832054897E+04 0.96839370205864E+03 0.70856871788096E+01
 0.26571326920536E+01 0.34796640503774E+03 0.29815119292281E+03 0.34071649395103E+03 0.36933462036847E+03
 0.29815017721436E+03 0.29815033124629E+03 0.33676400759759E+03 0.36931220663580E+03 0.29815014703953E+03
 0.29815033043177E+03 0.34071649395103E+03 0.36933462036847E+03 0.29815017721436E+03 0.29815033124629E+03
 0.33676400759759E+03 0.36931220663580E+03 0.29815014703953E+03 0.29815033043177E+03 0.41342015128640E+03
 0.33861564171010E+03 0.19938039612754E+04 0.18391512353392E+04 0.57694682119964E+03 0.93087716563228E+03
 0.35104561032664E+03 0.11882520518749E+04 0.11266518470594E+04 0.10762261801544E+04 0.17167313356851E+04
 0.10817230808806E+04 0.11263284424807E+04 0.99432168124963E+03 0.17166051308635E+04 0.11882520518749E+04
 0.11266518470594E+04 0.10762261801544E+04 0.17167313356851E+04 0.10817230808805E+04 0.11263284424807E+04
 0.99432168124962E+03 0.17166051308635E+04 0.16794207679416E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49068142829574E+03 0.12948761250276E+01
 0.12948761250276E+01 0.33055131490612E+01 0.00000000000000E+00 0.34018096188210E+03 0.34018096188210E+03
 0.34018096188210E+03 0.34018096188210E+03 0.00000000000000E+00 0.00000000000000E+00 0.15675946976272E+00
 0.00000000000000E+00 -.15260434372229E+02 0.00000000000000E+00 0.13639380188552E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58653691659058E+01 0.21995134372147E+01 0.33861771426617E+03 0.41341767470591E+03
 0.31185520273043E+03 0.31185520273043E+03 0.29815004626066E+03 0.29815004553322E+03 0.31185129295631E+03
 0.31185129295631E+03 0.29815004627274E+03 0.29815004554511E+03 0.31185520273043E+03 0.31185520273043E+03
 0.29815004626066E+03 0.29815004553322E+03 0.31185129295631E+03 0.31185129295631E+03 0.29815004627274E+03
 0.29815004554511E+03 0.30985462497862E+03 0.29815022210017E+03 -.65411135230537E+01 -.54187605277251E+01
 0.20323997659208E+03 0.46707874713704E+03 0.26282257066200E+03 0.18464843615851E+03 0.18794238045701E+03
 0.18464843615851E+03 0.37036825926229E+03 0.18466817118784E+03 0.18776860922375E+03 0.18466817118784E+03
 0.37022486357477E+03 0.18464843615851E+03 0.18794238045701E+03 0.18464843615851E+03 0.37036825926229E+03
 0.18466817118784E+03 0.18776860922375E+03 0.18466817118784E+03 0.37022486357477E+03 0.22189166708242E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36561361621757E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19300721453171E+00 0.00000000000000E+00 0.00000000000000E+00 0.19300721453171E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20106067328969E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20106067328969E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639097602961E+00 0.20320782448030E+00 0.34018096188210E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    881.34836040
 0.11424675032532E+00 0.31867208023066E+03 0.45231441127154E+03 0.44722501059577E+03 0.44520421007051E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14927978627455E+00 0.00000000000000E+00 -.23305230812257E+02
 0.31003303573657E-02 0.11308482026648E+01 0.25803701792597E+04 0.96763881722239E+03 0.70743358667842E+01
 0.26528759500441E+01 0.34833238968978E+03 0.29815131058567E+03 0.34103606466241E+03 0.36979338682005E+03
 0.29815019715031E+03 0.29815036846885E+03 0.33706971625257E+03 0.36977107006118E+03 0.29815016373932E+03
 0.29815036757360E+03 0.34103606466241E+03 0.36979338682005E+03 0.29815019715031E+03 0.29815036846885E+03
 0.33706971625257E+03 0.36977107006118E+03 0.29815016373932E+03 0.29815036757360E+03 0.41393259038919E+03
 0.33916305608270E+03 0.19967766448587E+04 0.18408592546830E+04 0.57567173570340E+03 0.92739594381734E+03
 0.34884584943543E+03 0.11904415774105E+04 0.11280779009587E+04 0.10774995702584E+04 0.17167090267495E+04
 0.10840980175243E+04 0.11277562687336E+04 0.99590926865792E+03 0.17165835719937E+04 0.11904415774105E+04
 0.11280779009587E+04 0.10774995702583E+04 0.17167090267495E+04 0.10840980175243E+04 0.11277562687336E+04
 0.99590926865792E+03 0.17165835719937E+04 0.16790264663907E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49099554916854E+03 0.12948762018938E+01
 0.12948762018938E+01 0.33454126911478E+01 0.00000000000000E+00 0.34037370785799E+03 0.34037370785799E+03
 0.34037370785799E+03 0.34037370785799E+03 0.00000000000000E+00 0.00000000000000E+00 0.15637211552104E+00
 0.00000000000000E+00 -.15253829354676E+02 0.00000000000000E+00 0.13703949432295E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58377331582580E+01 0.21891499343468E+01 0.33916518929717E+03 0.41393003843395E+03
 0.31198374629012E+03 0.31198374629012E+03 0.29815005158828E+03 0.29815005077707E+03 0.31197977216139E+03
 0.31197977216139E+03 0.29815005160162E+03 0.29815005079020E+03 0.31198374629012E+03 0.31198374629012E+03
 0.29815005158828E+03 0.29815005077707E+03 0.31197977216139E+03 0.31197977216139E+03 0.29815005160162E+03
 0.29815005079020E+03 0.30996647745832E+03 0.29815024462977E+03 -.92096577327292E+01 -.83962385559278E+01
 0.20431697152945E+03 0.46895498975740E+03 0.26361643337030E+03 0.18673377028522E+03 0.18887748828454E+03
 0.18673377028522E+03 0.37176615448254E+03 0.18675388367661E+03 0.18870233158993E+03 0.18675388367661E+03
 0.37162188267578E+03 0.18673377028522E+03 0.18887748828454E+03 0.18673377028522E+03 0.37176615448254E+03
 0.18675388367661E+03 0.18870233158993E+03 0.18675388367661E+03 0.37162188267578E+03 0.22204663401630E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36583360018705E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19296955583533E+00 0.00000000000000E+00 0.00000000000000E+00 0.19296955583533E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20098086668260E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20098086668260E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639076406849E+00 0.20309254651058E+00 0.34037370785799E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    892.96356081
 0.11414623063133E+00 0.31887459481151E+03 0.45268115577851E+03 0.44758998425580E+03 0.44556720224186E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14907034371456E+00 0.00000000000000E+00 -.23311912585735E+02
 0.31030602806478E-02 0.11328672249794E+01 0.25781000936050E+04 0.96678753510186E+03 0.70617278208801E+01
 0.26481479328300E+01 0.34875598494081E+03 0.29815145957625E+03 0.34140607219740E+03 0.37032408900784E+03
 0.29815022275066E+03 0.29815041624963E+03 0.33742376870462E+03 0.37030188190735E+03 0.29815018520739E+03
 0.29815041525227E+03 0.34140607219740E+03 0.37032408900784E+03 0.29815022275066E+03 0.29815041624963E+03
 0.33742376870462E+03 0.37030188190735E+03 0.29815018520739E+03 0.29815041525227E+03 0.41452617069044E+03
 0.33979727131252E+03 0.20002050726617E+04 0.18428236958375E+04 0.57415702271068E+03 0.92332578185994E+03
 0.34629797403571E+03 0.11929715333169E+04 0.11297125648041E+04 0.10789686755716E+04 0.17166710379978E+04
 0.10868428516993E+04 0.11293928871347E+04 0.99774169937394E+03 0.17165463794251E+04 0.11929715333169E+04
 0.11297125648041E+04 0.10789686755716E+04 0.17166710379978E+04 0.10868428516993E+04 0.11293928871347E+04
 0.99774169937394E+03 0.17165463794251E+04 0.16785497132933E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49135887239759E+03 0.12948762511277E+01
 0.12948762511277E+01 0.33918734928044E+01 0.00000000000000E+00 0.34059777369762E+03 0.34059777369762E+03
 0.34059777369762E+03 0.34059777369762E+03 0.00000000000000E+00 0.00000000000000E+00 0.15593453465495E+00
 0.00000000000000E+00 -.15239751296142E+02 0.10000000000000E-02 0.13776545070575E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58069711665859E+01 0.21776141874697E+01 0.33979947487442E+03 0.41452353051022E+03
 0.31213299610975E+03 0.31213299610975E+03 0.29815005894807E+03 0.29815005752850E+03 0.31212894728631E+03
 0.31212894728631E+03 0.29815005896313E+03 0.29815005754320E+03 0.31213299610975E+03 0.31213299610975E+03
 0.29815005894807E+03 0.29815005752850E+03 0.31212894728631E+03 0.31212894728631E+03 0.29815005896313E+03
 0.29815005754320E+03 0.31009638694569E+03 0.29815027323524E+03 -.12326969750503E+02 -.11842694755485E+02
 0.20556423956188E+03 0.47112953211851E+03 0.26453747135881E+03 0.18915883351837E+03 0.18995935459758E+03
 0.18915883351837E+03 0.37338440682517E+03 0.18917938723204E+03 0.18978256767301E+03 0.18917938723204E+03
 0.37323909593489E+03 0.18915883351837E+03 0.18995935459758E+03 0.18915883351837E+03 0.37338440682517E+03
 0.18917938723204E+03 0.18978256767301E+03 0.18917938723204E+03 0.37323909593489E+03 0.22222672570784E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36608713905492E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19288498868520E+00 0.00000000000000E+00 0.00000000000000E+00 0.19288498868520E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20085061765111E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20085061765111E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639072910162E+00 0.20295893201241E+00 0.34059777369762E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    902.87194825
 0.11406719347685E+00 0.31904492428539E+03 0.45299178046050E+03 0.44789879980752E+03 0.44587419767028E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14889719080997E+00 0.00000000000000E+00 -.23319413497037E+02
 0.31052101410659E-02 0.11345153772113E+01 0.25763151724263E+04 0.96611818965986E+03 0.70514689890449E+01
 0.26443008708918E+01 0.34911521150394E+03 0.29815159787403E+03 0.34171998673541E+03 0.37077378136519E+03
 0.29815024685348E+03 0.29815046121702E+03 0.33772426426430E+03 0.37075166512815E+03 0.29815020544198E+03
 0.29815046012500E+03 0.34171998673541E+03 0.37077378136519E+03 0.29815024685348E+03 0.29815046121702E+03
 0.33772426426430E+03 0.37075166512815E+03 0.29815020544198E+03 0.29815046012500E+03 0.41502866271642E+03
 0.34033479593687E+03 0.20030948263138E+04 0.18444598990613E+04 0.57285358900722E+03 0.91986433508679E+03
 0.34414647813453E+03 0.11951096434397E+04 0.11310817470873E+04 0.10801966466486E+04 0.17166238084978E+04
 0.10891630351318E+04 0.11307636538894E+04 0.99927733898963E+03 0.17164997725307E+04 0.11951096434397E+04
 0.11310817470873E+04 0.10801966466486E+04 0.17166238084978E+04 0.10891630351318E+04 0.11307636538894E+04
 0.99927733898963E+03 0.17164997725307E+04 0.16781259534302E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49166621995655E+03 0.12948763063973E+01
 0.12948763063973E+01 0.34315070425579E+01 0.00000000000000E+00 0.34078872822078E+03 0.34078872822078E+03
 0.34078872822078E+03 0.34078872822078E+03 0.00000000000000E+00 0.00000000000000E+00 0.15557236725159E+00
 0.00000000000000E+00 -.15230470538383E+02 0.00000000000000E+00 0.13836330989907E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57818796079941E+01 0.21682048529978E+01 0.34033705157843E+03 0.41502595714602E+03
 0.31226007436992E+03 0.31226007436992E+03 0.29815006547865E+03 0.29815006390181E+03 0.31225596187250E+03
 0.31225596187250E+03 0.29815006549520E+03 0.29815006391797E+03 0.31226007436992E+03 0.31226007436992E+03
 0.29815006547865E+03 0.29815006390181E+03 0.31225596187250E+03 0.31225596187250E+03 0.29815006549520E+03
 0.29815006391797E+03 0.31020703479704E+03 0.29815029986035E+03 -.14986341172605E+02 -.14748753440195E+02
 0.20662300772899E+03 0.47298125801508E+03 0.26532513524745E+03 0.19122219959859E+03 0.19087701284055E+03
 0.19122219959859E+03 0.37476214611731E+03 0.19124312980235E+03 0.19069883538642E+03 0.19124312980235E+03
 0.37461594921989E+03 0.19122219959859E+03 0.19087701284055E+03 0.19122219959859E+03 0.37476214611731E+03
 0.19124312980236E+03 0.19069883538643E+03 0.19124312980236E+03 0.37461594921990E+03 0.22238690130548E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36630422531415E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19282778934392E+00 0.00000000000000E+00 0.00000000000000E+00 0.19282778934392E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20076712695596E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20076712695596E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639061031697E+00 0.20284510371068E+00 0.34078872822078E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    912.78033569
 0.11399025558297E+00 0.31921598381223E+03 0.45330095044140E+03 0.44820615723610E+03 0.44617977296003E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14872900167088E+00 0.00000000000000E+00 -.23323614840755E+02
 0.31073057586757E-02 0.11360955263695E+01 0.25745776635157E+04 0.96546662381838E+03 0.70416613870179E+01
 0.26406230201317E+01 0.34947272756082E+03 0.29815174655764E+03 0.34203255862771E+03 0.37122069238151E+03
 0.29815027308594E+03 0.29815051013908E+03 0.33802363495443E+03 0.37119866473848E+03 0.29815022748543E+03
 0.29815050894542E+03 0.34203255862771E+03 0.37122069238151E+03 0.29815027308594E+03 0.29815051013908E+03
 0.33802363495443E+03 0.37119866473848E+03 0.29815022748543E+03 0.29815050894542E+03 0.41552656087513E+03
 0.34086837348897E+03 0.20059629985476E+04 0.18460901276601E+04 0.57156595370496E+03 0.91645639658622E+03
 0.34203261311274E+03 0.11972347465301E+04 0.11324382720453E+04 0.10814240583317E+04 0.17165783012211E+04
 0.10914688819209E+04 0.11321216937841E+04 0.10008090324234E+04 0.17164548409444E+04 0.11972347465301E+04
 0.11324382720453E+04 0.10814240583317E+04 0.17165783012211E+04 0.10914688819209E+04 0.11321216937842E+04
 0.10008090324234E+04 0.17164548409444E+04 0.16777142168580E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49197212481884E+03 0.12948763373545E+01
 0.12948763373545E+01 0.34711405923115E+01 0.00000000000000E+00 0.34097933251111E+03 0.34097933251111E+03
 0.34097933251111E+03 0.34097933251111E+03 0.00000000000000E+00 0.00000000000000E+00 0.15522014225082E+00
 0.00000000000000E+00 -.15217461866871E+02 0.00000000000000E+00 0.13894210516779E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57577938597798E+01 0.21591726974174E+01 0.34087068733544E+03 0.41552378176622E+03
 0.31238716792607E+03 0.31238716792607E+03 0.29815007260246E+03 0.29815007085407E+03 0.31238299176272E+03
 0.31238299176272E+03 0.29815007262061E+03 0.29815007087179E+03 0.31238716792607E+03 0.31238716792607E+03
 0.29815007260246E+03 0.29815007085407E+03 0.31238299176272E+03 0.31238299176272E+03 0.29815007262061E+03
 0.29815007087179E+03 0.31031774782111E+03 0.29815032855224E+03 -.17640840177114E+02 -.17600826615833E+02
 0.20767429682704E+03 0.47481975264784E+03 0.26610708433666E+03 0.19327582309539E+03 0.19178714456069E+03
 0.19327582309539E+03 0.37612813046119E+03 0.19329713097019E+03 0.19160758176232E+03 0.19329713097019E+03
 0.37598105263730E+03 0.19327582309539E+03 0.19178714456069E+03 0.19327582309539E+03 0.37612813046119E+03
 0.19329713097019E+03 0.19160758176232E+03 0.19329713097019E+03 0.37598105263731E+03 0.22255333204717E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36651983823043E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19274598311394E+00 0.00000000000000E+00 0.00000000000000E+00 0.19274598311394E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20066558957359E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20066558957359E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639061559248E+00 0.20273174666209E+00 0.34097933251111E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    922.68872313
 0.11390630805303E+00 0.31938802010857E+03 0.45360873778982E+03 0.44851254231805E+03 0.44648457370269E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14856556546195E+00 0.00000000000000E+00 -.23323648648495E+02
 0.31095955560339E-02 0.11376106035830E+01 0.25726818346125E+04 0.96475568797968E+03 0.70322832565057E+01
 0.26371062211896E+01 0.34982856949152E+03 0.29815190618971E+03 0.34234381191513E+03 0.37166494941706E+03
 0.29815030159186E+03 0.29815056328004E+03 0.33832187827187E+03 0.37164300815534E+03 0.29815025146179E+03
 0.29815056197740E+03 0.34234381191513E+03 0.37166494941706E+03 0.29815030159186E+03 0.29815056328004E+03
 0.33832187827187E+03 0.37164300815534E+03 0.29815025146179E+03 0.29815056197740E+03 0.41602063548580E+03
 0.34139841181953E+03 0.20088172935876E+04 0.18477237323440E+04 0.57028539430983E+03 0.91308587353812E+03
 0.33994905225674E+03 0.11993501706310E+04 0.11337830927837E+04 0.10826540929964E+04 0.17165345997130E+04
 0.10937641924233E+04 0.11334679640035E+04 0.10023408478295E+04 0.17164116715894E+04 0.11993501706310E+04
 0.11337830927837E+04 0.10826540929964E+04 0.17165345997130E+04 0.10937641924233E+04 0.11334679640035E+04
 0.10023408478295E+04 0.17164116715894E+04 0.16773142302000E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49227715299879E+03 0.12948763376036E+01
 0.12948763376036E+01 0.35107741420650E+01 0.00000000000000E+00 0.34116895759670E+03 0.34116895759670E+03
 0.34116895759670E+03 0.34116895759670E+03 0.00000000000000E+00 0.00000000000000E+00 0.15487763287868E+00
 0.00000000000000E+00 -.15199696178429E+02 0.00000000000000E+00 0.13950255304823E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57346620726246E+01 0.21504982772342E+01 0.34140078234236E+03 0.41601778521620E+03
 0.31251414498182E+03 0.31251414498182E+03 0.29815008036099E+03 0.29815007842577E+03 0.31250990517922E+03
 0.31250990517922E+03 0.29815008038086E+03 0.29815007844516E+03 0.31251414498182E+03 0.31251414498182E+03
 0.29815008036099E+03 0.29815007842577E+03 0.31250990517922E+03 0.31250990517922E+03 0.29815008038086E+03
 0.29815007844516E+03 0.31042840803144E+03 0.29815035942795E+03 -.20297729145883E+02 -.20532147430999E+02
 0.20871463476535E+03 0.47663610027650E+03 0.26687789233733E+03 0.19531939532379E+03 0.19268629290894E+03
 0.19531939532379E+03 0.37747484659387E+03 0.19534108190995E+03 0.19250534512381E+03 0.19534108190995E+03
 0.37732688778247E+03 0.19531939532379E+03 0.19268629290894E+03 0.19531939532379E+03 0.37747484659387E+03
 0.19534108190995E+03 0.19250534512381E+03 0.19534108190995E+03 0.37732688778247E+03 0.22268964760371E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36673321487131E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19263262727587E+00 0.00000000000000E+00 0.00000000000000E+00 0.19263262727587E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20054140921460E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20054140921460E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639078022047E+00 0.20261927174213E+00 0.34116895759670E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    932.59711057
 0.11380261213927E+00 0.31956192560557E+03 0.45391536120788E+03 0.44881877056741E+03 0.44678965630339E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14840652925984E+00 0.00000000000000E+00 -.23315303268079E+02
 0.31124287420296E-02 0.11390651861747E+01 0.25703399701878E+04 0.96387748882044E+03 0.70233030533276E+01
 0.26337386449979E+01 0.35018282171291E+03 0.29815207734981E+03 0.34265381182788E+03 0.37210663330822E+03
 0.29815033252146E+03 0.29815062091557E+03 0.33861905328245E+03 0.37208477629742E+03 0.29815027750074E+03
 0.29815061949626E+03 0.34265381182788E+03 0.37210663330822E+03 0.29815033252146E+03 0.29815062091557E+03
 0.33861905328245E+03 0.37208477629742E+03 0.29815027750074E+03 0.29815061949626E+03 0.41651112164143E+03
 0.34192226139518E+03 0.20116700281321E+04 0.18493790791033E+04 0.56901381296894E+03 0.90975473739671E+03
 0.33789585536292E+03 0.12014614772174E+04 0.11351204170668E+04 0.10838954362903E+04 0.17164975483117E+04
 0.10960549532322E+04 0.11348066823518E+04 0.10038821096021E+04 0.17163751179882E+04 0.12014614772174E+04
 0.11351204170668E+04 0.10838954362903E+04 0.17164975483117E+04 0.10960549532322E+04 0.11348066823518E+04
 0.10038821096021E+04 0.17163751179882E+04 0.16769355636954E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49258223902281E+03 0.12948762761116E+01
 0.12948762761116E+01 0.35504076918185E+01 0.00000000000000E+00 0.34135811275294E+03 0.34135811275294E+03
 0.34135811275294E+03 0.34135811275294E+03 0.00000000000000E+00 0.00000000000000E+00 0.15454463959693E+00
 0.00000000000000E+00 -.15172233080892E+02 0.00000000000000E+00 0.14004485997685E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57124552813452E+01 0.21421707305045E+01 0.34192469346631E+03 0.41650820665791E+03
 0.31264097139617E+03 0.31264097139617E+03 0.29815008879770E+03 0.29815008665930E+03 0.31263666803650E+03
 0.31263666803650E+03 0.29815008881940E+03 0.29815008668048E+03 0.31264097139617E+03 0.31264097139617E+03
 0.29815008879770E+03 0.29815008665930E+03 0.31263666803650E+03 0.31263666803650E+03 0.29815008881940E+03
 0.29815008668048E+03 0.31053898505633E+03 0.29815039260836E+03 -.22931248988438E+02 -.23702532959026E+02
 0.20974705816487E+03 0.47843685476032E+03 0.26764106130463E+03 0.19734539260057E+03 0.19357791298840E+03
 0.19734539260057E+03 0.37880794492285E+03 0.19736745876465E+03 0.19339560188722E+03 0.19736745876465E+03
 0.37865912576381E+03 0.19734539260057E+03 0.19357791298840E+03 0.19734539260057E+03 0.37880794492285E+03
 0.19736745876465E+03 0.19339560188723E+03 0.19736745876465E+03 0.37865912576381E+03 0.22276076241008E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36694363825948E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19245493739555E+00 0.00000000000000E+00 0.00000000000000E+00 0.19245493739555E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20037087191247E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20037087191247E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639126581396E+00 0.20250755091469E+00 0.34135811275294E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    940.79120298
 0.11371815078107E+00 0.31970671807715E+03 0.45416901898161E+03 0.44907208424540E+03 0.44704203679128E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14827809415532E+00 0.00000000000000E+00 -.23324368554258E+02
 0.31147402156549E-02 0.11402231597506E+01 0.25684325003387E+04 0.96316218762700E+03 0.70161704150526E+01
 0.26310639056447E+01 0.35047523102003E+03 0.29815222663514E+03 0.34290987868536E+03 0.37246990661772E+03
 0.29815035974265E+03 0.29815067162426E+03 0.33886480273636E+03 0.37244811798684E+03 0.29815030043373E+03
 0.29815067010330E+03 0.34290987868536E+03 0.37246990661772E+03 0.29815035974265E+03 0.29815067162426E+03
 0.33886480273636E+03 0.37244811798684E+03 0.29815030043373E+03 0.29815067010330E+03 0.41691039538910E+03
 0.34234715316099E+03 0.20140274321873E+04 0.18507562188923E+04 0.56803989979659E+03 0.90714896735785E+03
 0.33626886806227E+03 0.12032062034720E+04 0.11362375685146E+04 0.10849283680219E+04 0.17164975535758E+04
 0.10979461834583E+04 0.11359249576653E+04 0.10051589437210E+04 0.17163755196057E+04 0.12032062034720E+04
 0.11362375685146E+04 0.10849283680219E+04 0.17164975535758E+04 0.10979461834583E+04 0.11359249576653E+04
 0.10051589437210E+04 0.17163755196057E+04 0.16766777244029E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49283461089787E+03 0.12948763429081E+01
 0.12948763429081E+01 0.35831840614787E+01 0.00000000000000E+00 0.34151584467138E+03 0.34151584467138E+03
 0.34151584467138E+03 0.34151584467138E+03 0.00000000000000E+00 0.00000000000000E+00 0.15427618504220E+00
 0.00000000000000E+00 -.15167979053043E+02 0.00000000000000E+00 0.14047943076432E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56947838957443E+01 0.21355439609041E+01 0.34234965976418E+03 0.41690741132505E+03
 0.31274652382479E+03 0.31274652382479E+03 0.29815009512618E+03 0.29815009391773E+03 0.31274216781177E+03
 0.31274216781177E+03 0.29815009514920E+03 0.29815009394046E+03 0.31274652382479E+03 0.31274652382479E+03
 0.29815009512618E+03 0.29815009391773E+03 0.31274216781177E+03 0.31274216781177E+03 0.29815009514920E+03
 0.29815009394046E+03 0.31063109476140E+03 0.29815042159794E+03 -.25048259566494E+02 -.26338752535136E+02
 0.21060167788817E+03 0.47994012115391E+03 0.26828543487630E+03 0.19899629729016E+03 0.19431664049592E+03
 0.19899629729016E+03 0.37992331052302E+03 0.19901867945573E+03 0.19413325949399E+03 0.19901867945573E+03
 0.37977383945945E+03 0.19899629729016E+03 0.19431664049592E+03 0.19899629729016E+03 0.37992331052302E+03
 0.19901867945573E+03 0.19413325949399E+03 0.19901867945573E+03 0.37977383945945E+03 0.22282338509778E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36712318204080E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19243051658143E+00 0.00000000000000E+00 0.00000000000000E+00 0.19243051658143E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20031742349864E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20031742349864E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639105681989E+00 0.20241381422586E+00 0.34151584467138E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    950.89260747
 0.11361905995878E+00 0.31987995548565E+03 0.45447950468443E+03 0.44938181326748E+03 0.44735041044583E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14812355418385E+00 0.00000000000000E+00 -.23328042908761E+02
 0.31174564288980E-02 0.11416016645192E+01 0.25661946469699E+04 0.96232299261371E+03 0.70076982616954E+01
 0.26278868481358E+01 0.35083350911459E+03 0.29815242329214E+03 0.34322360929182E+03 0.37291578223295E+03
 0.29815039600869E+03 0.29815073915342E+03 0.33916576387112E+03 0.37289407576182E+03 0.29815033101354E+03
 0.29815073749879E+03 0.34322360929182E+03 0.37291578223295E+03 0.29815039600869E+03 0.29815073915342E+03
 0.33916576387112E+03 0.37289407576182E+03 0.29815033101354E+03 0.29815073749879E+03 0.41740530655667E+03
 0.34287076133637E+03 0.20169062050404E+04 0.18524020139868E+04 0.56673965998744E+03 0.90378767909951E+03
 0.33421432081213E+03 0.12053414947412E+04 0.11375849168032E+04 0.10861636807777E+04 0.17164683169401E+04
 0.11002625732223E+04 0.11372736103033E+04 0.10066968515985E+04 0.17163467107179E+04 0.12053414947412E+04
 0.11375849168032E+04 0.10861636807777E+04 0.17164683169401E+04 0.11002625732223E+04 0.11372736103033E+04
 0.10066968515985E+04 0.17163467107179E+04 0.16762931965902E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49314308977540E+03 0.12948763699822E+01
 0.12948763699822E+01 0.36235896794360E+01 0.00000000000000E+00 0.34171126270454E+03 0.34171126270454E+03
 0.34171126270454E+03 0.34171126270454E+03 0.00000000000000E+00 0.00000000000000E+00 0.15395365704271E+00
 0.00000000000000E+00 -.15153644403551E+02 0.00000000000000E+00 0.14099856139117E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56738167546304E+01 0.21276812829864E+01 0.34287332957584E+03 0.41740227604955E+03
 0.31287566789747E+03 0.31287566789747E+03 0.29815010604361E+03 0.29815010360804E+03 0.31287124747086E+03
 0.31287124747086E+03 0.29815010606897E+03 0.29815010363282E+03 0.31287566789747E+03 0.31287566789747E+03
 0.29815010604361E+03 0.29815010360804E+03 0.31287124747086E+03 0.31287124747086E+03 0.29815010606897E+03
 0.29815010363282E+03 0.31074375750059E+03 0.29815045986849E+03 -.27671089773974E+02 -.29678890629215E+02
 0.21166533866173E+03 0.48181736601541E+03 0.26909370066037E+03 0.20104556847734E+03 0.19523700084836E+03
 0.20104556847734E+03 0.38131788296115E+03 0.20106833744144E+03 0.19505225024019E+03 0.20106833744144E+03
 0.38116755368808E+03 0.20104556847734E+03 0.19523700084836E+03 0.20104556847734E+03 0.38131788296115E+03
 0.20106833744144E+03 0.19505225024019E+03 0.20106833744144E+03 0.38116755368809E+03 0.22289055946826E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36734188709010E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19234401274513E+00 0.00000000000000E+00 0.00000000000000E+00 0.19234401274513E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20019067523140E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20019067523140E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639109693791E+00 0.20229812765590E+00 0.34171126270454E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    960.03701196
 0.11352782297399E+00 0.32003563093947E+03 0.45475886630392E+03 0.44966058776561E+03 0.44762799714324E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14798716291028E+00 0.00000000000000E+00 -.23327566079260E+02
 0.31199615560104E-02 0.11428033836547E+01 0.25641341588291E+04 0.96155030956092E+03 0.70003292906047E+01
 0.26251234839767E+01 0.35115628165498E+03 0.29815261384416E+03 0.34350634540316E+03 0.37331715607651E+03
 0.29815043156009E+03 0.29815080532153E+03 0.33943708803600E+03 0.37329552212299E+03 0.29815036101787E+03
 0.29815080353761E+03 0.34350634540316E+03 0.37331715607651E+03 0.29815043156009E+03 0.29815080532153E+03
 0.33943708803600E+03 0.37329552212299E+03 0.29815036101787E+03 0.29815080353761E+03 0.41785027814011E+03
 0.34333994957649E+03 0.20194911722118E+04 0.18538720353424E+04 0.56555786767082E+03 0.90075784464495E+03
 0.33237218763577E+03 0.12072616309552E+04 0.11387865024909E+04 0.10872682541789E+04 0.17164309514753E+04
 0.11023458803608E+04 0.11384763325767E+04 0.10080737738285E+04 0.17163097063840E+04 0.12072616309552E+04
 0.11387865024909E+04 0.10872682541789E+04 0.17164309514753E+04 0.11023458803608E+04 0.11384763325767E+04
 0.10080737738285E+04 0.17163097063840E+04 0.16759369880158E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49342075441619E+03 0.12948763664688E+01
 0.12948763664688E+01 0.36601672973763E+01 0.00000000000000E+00 0.34188884453028E+03 0.34188884453028E+03
 0.34188884453028E+03 0.34188884453028E+03 0.00000000000000E+00 0.00000000000000E+00 0.15366948935178E+00
 0.00000000000000E+00 -.15136901954832E+02 0.00000000000000E+00 0.14145321228710E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56555802944670E+01 0.21208426104251E+01 0.34334255741016E+03 0.41784721750294E+03
 0.31299249456008E+03 0.31299249456008E+03 0.29815011578711E+03 0.29815011312776E+03 0.31298801590239E+03
 0.31298801590239E+03 0.29815011581448E+03 0.29815011315451E+03 0.31299249456008E+03 0.31299249456008E+03
 0.29815011578711E+03 0.29815011312776E+03 0.31298801590239E+03 0.31298801590239E+03 0.29815011581448E+03
 0.29815011315451E+03 0.31084569745029E+03 0.29815049703277E+03 -.30019804199259E+02 -.32724748939846E+02
 0.21263022262300E+03 0.48352578008966E+03 0.26983240635355E+03 0.20289156389135E+03 0.19607229397563E+03
 0.20289156389135E+03 0.38258794683214E+03 0.20291468359126E+03 0.19588631787429E+03 0.20291468359126E+03
 0.38243685474639E+03 0.20289156389135E+03 0.19607229397563E+03 0.20289156389135E+03 0.38258794683214E+03
 0.20291468359127E+03 0.19588631787429E+03 0.20291468359127E+03 0.38243685474639E+03 0.22295050047531E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36754005510005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19223717697144E+00 0.00000000000000E+00 0.00000000000000E+00 0.19223717697144E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20007434679281E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20007434679281E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639125624216E+00 0.20219324862845E+00 0.34188884453028E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    972.22955127
 0.11339112272993E+00 0.32024565151011E+03 0.45512990766072E+03 0.45003168191289E+03 0.44799790003420E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14781013515790E+00 0.00000000000000E+00 -.23330340401435E+02
 0.31237225665923E-02 0.11443397252890E+01 0.25610469013986E+04 0.96039258802447E+03 0.69909309475208E+01
 0.26215991053203E+01 0.35158471362716E+03 0.29815288634306E+03 0.34388181113555E+03 0.37384902551100E+03
 0.29815048301802E+03 0.29815090104694E+03 0.33979760104276E+03 0.37382748574314E+03 0.29815040448767E+03
 0.29815089907852E+03 0.34388181113555E+03 0.37384902551100E+03 0.29815048301802E+03 0.29815090104694E+03
 0.33979760104276E+03 0.37382748574314E+03 0.29815040448767E+03 0.29815089907852E+03 0.41843811413843E+03
 0.34395741463692E+03 0.20229307131047E+04 0.18558547729689E+04 0.56400986796218E+03 0.89679381833766E+03
 0.32996390103567E+03 0.12098149773493E+04 0.11403814527426E+04 0.10887556901853E+04 0.17163927362967E+04
 0.11051156901238E+04 0.11400727441579E+04 0.10099208861700E+04 0.17162719434928E+04 0.12098149773493E+04
 0.11403814527425E+04 0.10887556901853E+04 0.17163927362967E+04 0.11051156901238E+04 0.11400727441579E+04
 0.10099208861700E+04 0.17162719434928E+04 0.16754945819451E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49379055729487E+03 0.12948763869111E+01
 0.12948763869111E+01 0.37089374546301E+01 0.00000000000000E+00 0.34212597943713E+03 0.34212597943713E+03
 0.34212597943713E+03 0.34212597943713E+03 0.00000000000000E+00 0.00000000000000E+00 0.15330181696451E+00
 0.00000000000000E+00 -.15118400827537E+02 0.00000000000000E+00 0.14203768279412E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56323081612054E+01 0.21121155604520E+01 0.34396008790616E+03 0.41843499681045E+03
 0.31314838851623E+03 0.31314838851623E+03 0.29815012992140E+03 0.29815012693743E+03 0.31314383236100E+03
 0.31314383236100E+03 0.29815012995165E+03 0.29815012696698E+03 0.31314838851623E+03 0.31314838851623E+03
 0.29815012992140E+03 0.29815012693743E+03 0.31314383236100E+03 0.31314383236100E+03 0.29815012995165E+03
 0.29815012696698E+03 0.31098178431099E+03 0.29815055030093E+03 -.33109419432817E+02 -.36794565461775E+02
 0.21391333362367E+03 0.48580315567284E+03 0.27082025538105E+03 0.20533132957749E+03 0.19718279952084E+03
 0.20533132957749E+03 0.38428090084971E+03 0.20535491788535E+03 0.19699521566860E+03 0.20535491788535E+03
 0.38412881732829E+03 0.20533132957749E+03 0.19718279952084E+03 0.20533132957749E+03 0.38428090084970E+03
 0.20535491788535E+03 0.19699521566860E+03 0.20535491788535E+03 0.38412881732830E+03 0.22302808188300E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36780500599706E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19212109548772E+00 0.00000000000000E+00 0.00000000000000E+00 0.19212109548772E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19993296407564E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19993296407564E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639134257821E+00 0.20205322985671E+00 0.34212597943713E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    981.37395575
 0.11330111560134E+00 0.32040040377570E+03 0.45540695090752E+03 0.45030815343968E+03 0.44827320741674E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14768086602441E+00 0.00000000000000E+00 -.23332754842169E+02
 0.31262038474064E-02 0.11454447019743E+01 0.25590141879702E+04 0.95963032048884E+03 0.69841870028391E+01
 0.26190701260647E+01 0.35190453028697E+03 0.29815310521177E+03 0.34416220165062E+03 0.37424557502937E+03
 0.29815052483937E+03 0.29815097880612E+03 0.34006695060833E+03 0.37422410406779E+03 0.29815043984929E+03
 0.29815097668983E+03 0.34416220165062E+03 0.37424557502937E+03 0.29815052483937E+03 0.29815097880612E+03
 0.34006695060833E+03 0.37422410406779E+03 0.29815043984929E+03 0.29815097668983E+03 0.41887559896410E+03
 0.34441518918978E+03 0.20254874889977E+04 0.18573078327702E+04 0.56285654177039E+03 0.89385433203997E+03
 0.32818350756072E+03 0.12117178165896E+04 0.11415682360501E+04 0.10898497468423E+04 0.17163669805881E+04
 0.11071794810871E+04 0.11412605733601E+04 0.10112824018940E+04 0.17162464951468E+04 0.12117178165896E+04
 0.11415682360501E+04 0.10898497468423E+04 0.17163669805881E+04 0.11071794810871E+04 0.11412605733601E+04
 0.10112824018940E+04 0.17162464951468E+04 0.16751621217602E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49406593037439E+03 0.12948764047016E+01
 0.12948764047016E+01 0.37455150725705E+01 0.00000000000000E+00 0.34230516316139E+03 0.34230516316139E+03
 0.34230516316139E+03 0.34230516316139E+03 0.00000000000000E+00 0.00000000000000E+00 0.15303414429064E+00
 0.00000000000000E+00 -.15104948638064E+02 0.00000000000000E+00 0.14245994266830E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56156136596424E+01 0.21058551223659E+01 0.34441791191168E+03 0.41887243894067E+03
 0.31326537817107E+03 0.31326537817107E+03 0.29815014143365E+03 0.29815013818527E+03 0.31326076403433E+03
 0.31326076403433E+03 0.29815014146618E+03 0.29815013821705E+03 0.31326537817107E+03 0.31326537817107E+03
 0.29815014143365E+03 0.29815013818527E+03 0.31326076403433E+03 0.31326076403433E+03 0.29815014146618E+03
 0.29815013821705E+03 0.31108393972857E+03 0.29815059318086E+03 -.35392718819228E+02 -.39840344597204E+02
 0.21488174863918E+03 0.48752956962800E+03 0.27157341224563E+03 0.20715213974343E+03 0.19802173817963E+03
 0.20715213974343E+03 0.38556600753716E+03 0.20717607983560E+03 0.19783295980360E+03 0.20717607983560E+03
 0.38541319113790E+03 0.20715213974343E+03 0.19802173817963E+03 0.20715213974343E+03 0.38556600753716E+03
 0.20717607983561E+03 0.19783295980360E+03 0.20717607983561E+03 0.38541319113791E+03 0.22309687830063E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36800459460758E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19203751232052E+00 0.00000000000000E+00 0.00000000000000E+00 0.19203751232052E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19982615375977E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19982615375977E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639139091486E+00 0.20194753920986E+00 0.34230516316139E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    990.51836024
 0.11321180570352E+00 0.32055539252350E+03 0.45568276777727E+03 0.45058342972645E+03 0.44854735637781E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14755458623976E+00 0.00000000000000E+00 -.23334568613888E+02
 0.31286698063501E-02 0.11465098370303E+01 0.25569972209157E+04 0.95887395784338E+03 0.69776985260955E+01
 0.26166369472858E+01 0.35222305548357E+03 0.29815333709474E+03 0.34444156426263E+03 0.37464013780391E+03
 0.29815056959280E+03 0.29815106197956E+03 0.34033541365969E+03 0.37461873411681E+03 0.29815047771953E+03
 0.29815105970690E+03 0.34444156426263E+03 0.37464013780392E+03 0.29815056959280E+03 0.29815106197956E+03
 0.34033541365969E+03 0.37461873411681E+03 0.29815047771953E+03 0.29815105970690E+03 0.41931026063086E+03
 0.34486862392258E+03 0.20280286343929E+04 0.18587549573681E+04 0.56170622425346E+03 0.89093774590016E+03
 0.32642299052543E+03 0.12136112273066E+04 0.11427440772004E+04 0.10909417099733E+04 0.17163389976145E+04
 0.11092330215487E+04 0.11424374221450E+04 0.10126397198228E+04 0.17162187963797E+04 0.12136112273066E+04
 0.11427440772004E+04 0.10909417099733E+04 0.17163389976145E+04 0.11092330215487E+04 0.11424374221450E+04
 0.10126397198228E+04 0.17162187963797E+04 0.16748322396938E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49434012481690E+03 0.12948764180662E+01
 0.12948764180662E+01 0.37820926905108E+01 0.00000000000000E+00 0.34248477268049E+03 0.34248477268049E+03
 0.34248477268049E+03 0.34248477268049E+03 0.00000000000000E+00 0.00000000000000E+00 0.15277321168682E+00
 0.00000000000000E+00 -.15090946953503E+02 0.00000000000000E+00 0.14286907587624E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55995322647217E+01 0.20998245992706E+01 0.34487139302959E+03 0.41930706141233E+03
 0.31338241299267E+03 0.31338241299267E+03 0.29815015377559E+03 0.29815015024374E+03 0.31337774099193E+03
 0.31337774099193E+03 0.29815015381053E+03 0.29815015027788E+03 0.31338241299267E+03 0.31338241299267E+03
 0.29815015377559E+03 0.29815015024374E+03 0.31337774099193E+03 0.31337774099193E+03 0.29815015381053E+03
 0.29815015027788E+03 0.31118616106157E+03 0.29815063869615E+03 -.37653406515549E+02 -.42883220123464E+02
 0.21585044694172E+03 0.48925972591185E+03 0.27233002673542E+03 0.20896314472375E+03 0.19886091808354E+03
 0.20896314472375E+03 0.38685410013264E+03 0.20898743695142E+03 0.19867095351226E+03 0.20898743695142E+03
 0.38670055854958E+03 0.20896314472375E+03 0.19886091808354E+03 0.20896314472375E+03 0.38685410013264E+03
 0.20898743695142E+03 0.19867095351226E+03 0.20898743695142E+03 0.38670055854958E+03 0.22316816643904E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36820429714192E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19194982514366E+00 0.00000000000000E+00 0.00000000000000E+00 0.19194982514366E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19971889940388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19971889940388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639145682954E+00 0.20184172737580E+00 0.34248477268049E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1003.38108418
 0.11309154824003E+00 0.32077058353841E+03 0.45606817549523E+03 0.45096783744605E+03 0.44893006738430E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14738187260179E+00 0.00000000000000E+00 -.23341021213262E+02
 0.31319964093565E-02 0.11479439894505E+01 0.25542813446723E+04 0.95785550425212E+03 0.69689811293225E+01
 0.26133679234959E+01 0.35266856405844E+03 0.29815368771995E+03 0.34483241490190E+03 0.37519182725202E+03
 0.29815063812301E+03 0.29815118926628E+03 0.34071108131564E+03 0.37517051553874E+03 0.29815053576663E+03
 0.29815118675780E+03 0.34483241490190E+03 0.37519182725202E+03 0.29815063812301E+03 0.29815118926628E+03
 0.34071108131564E+03 0.37517051553874E+03 0.29815053576663E+03 0.29815118675780E+03 0.41991904324118E+03
 0.34550155731001E+03 0.20315693956860E+04 0.18607519567691E+04 0.56005277422832E+03 0.88680518336745E+03
 0.32395214526799E+03 0.12162549329784E+04 0.11443710433162E+04 0.10924522877745E+04 0.17162829762976E+04
 0.11121010850838E+04 0.11440657325529E+04 0.10145219237511E+04 0.17161631275796E+04 0.12162549329784E+04
 0.11443710433162E+04 0.10924522877745E+04 0.17162829762976E+04 0.11121010850838E+04 0.11440657325529E+04
 0.10145219237511E+04 0.17161631275796E+04 0.16743407631064E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49472296121385E+03 0.12948764656115E+01
 0.12948764656115E+01 0.38335435862853E+01 0.00000000000000E+00 0.34273797826806E+03 0.34273797826806E+03
 0.34273797826806E+03 0.34273797826806E+03 0.00000000000000E+00 0.00000000000000E+00 0.15241722086079E+00
 0.00000000000000E+00 -.15075803681213E+02 0.00000000000000E+00 0.14342312760674E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55779009518853E+01 0.20917128569570E+01 0.34550437831953E+03 0.41991580274954E+03
 0.31354664762905E+03 0.31354664762905E+03 0.29815017360943E+03 0.29815016875140E+03 0.31354189453914E+03
 0.31354189453914E+03 0.29815017364818E+03 0.29815016878905E+03 0.31354664762905E+03 0.31354664762905E+03
 0.29815017360943E+03 0.29815016875140E+03 0.31354189453914E+03 0.31354189453914E+03 0.29815017364818E+03
 0.29815016878905E+03 0.31132962398952E+03 0.29815070768235E+03 -.40816290014011E+02 -.47179597500654E+02
 0.21721611032789E+03 0.49170679403920E+03 0.27340460315966E+03 0.21150515705752E+03 0.20004413975521E+03
 0.21150515705752E+03 0.38867730588788E+03 0.21152994413559E+03 0.19985249284473E+03 0.21152994413559E+03
 0.38852272888175E+03 0.21150515705752E+03 0.20004413975521E+03 0.21150515705752E+03 0.38867730588788E+03
 0.21152994413559E+03 0.19985249284473E+03 0.21152994413559E+03 0.38852272888175E+03 0.22327116969488E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36848609770022E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19185774189603E+00 0.00000000000000E+00 0.00000000000000E+00 0.19185774189603E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19958554806765E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19958554806765E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639139906335E+00 0.20169258297661E+00 0.34273797826806E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1011.95623348
 0.11301602205376E+00 0.32091385616327E+03 0.45632381407945E+03 0.45122264914940E+03 0.44918370430990E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14726987408023E+00 0.00000000000000E+00 -.23344738522127E+02
 0.31340892494236E-02 0.11488583435458E+01 0.25525756809482E+04 0.95721588035558E+03 0.69634346522729E+01
 0.26112879946024E+01 0.35296426592697E+03 0.29815393722396E+03 0.34509197127032E+03 0.37555741689895E+03
 0.29815068744713E+03 0.29815128082917E+03 0.34096070045464E+03 0.37553616492275E+03 0.29815057758263E+03
 0.29815127815330E+03 0.34509197127032E+03 0.37555741689895E+03 0.29815068744713E+03 0.29815128082917E+03
 0.34096070045464E+03 0.37553616492275E+03 0.29815057758263E+03 0.29815127815330E+03 0.42032080949750E+03
 0.34591800602281E+03 0.20339098917981E+04 0.18620698674171E+04 0.55897013968212E+03 0.88410237595165E+03
 0.32233738557113E+03 0.12180058819931E+04 0.11454448398515E+04 0.10934529499518E+04 0.17162449741069E+04
 0.11140003487411E+04 0.11451403901133E+04 0.10157675282435E+04 0.17161253408849E+04 0.12180058819932E+04
 0.11454448398515E+04 0.10934529499518E+04 0.17162449741069E+04 0.11140003487411E+04 0.11451403901133E+04
 0.10157675282435E+04 0.17161253408849E+04 0.16740212213230E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49497670271280E+03 0.12948764930021E+01
 0.12948764930021E+01 0.38678441834683E+01 0.00000000000000E+00 0.34290727636733E+03 0.34290727636733E+03
 0.34290727636733E+03 0.34290727636733E+03 0.00000000000000E+00 0.00000000000000E+00 0.15218683938096E+00
 0.00000000000000E+00 -.15065300098582E+02 0.10000000000000E-02 0.14377896190037E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55640963700541E+01 0.20865361387703E+01 0.34592086538165E+03 0.42031753865903E+03
 0.31365635220012E+03 0.31365635220012E+03 0.29815018734218E+03 0.29815018209987E+03 0.31365154509415E+03
 0.31365154509415E+03 0.29815018738348E+03 0.29815018214001E+03 0.31365635220012E+03 0.31365635220012E+03
 0.29815018734218E+03 0.29815018209987E+03 0.31365154509415E+03 0.31365154509415E+03 0.29815018738348E+03
 0.29815018214001E+03 0.31142548990460E+03 0.29815075687787E+03 -.42892202419697E+02 -.50021301691098E+02
 0.21812633852973E+03 0.49334075580978E+03 0.27412378558739E+03 0.21318531669721E+03 0.20083274410018E+03
 0.21318531669721E+03 0.38989481527128E+03 0.21321043473365E+03 0.20063999444153E+03 0.21321043473365E+03
 0.38973956678003E+03 0.21318531669721E+03 0.20083274410018E+03 0.21318531669721E+03 0.38989481527128E+03
 0.21321043473365E+03 0.20063999444154E+03 0.21321043473365E+03 0.38973956678004E+03 0.22334648375092E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36867431701556E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19179252479044E+00 0.00000000000000E+00 0.00000000000000E+00 0.19179252479044E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19949997567239E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19949997567239E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639137336701E+00 0.20159299906966E+00 0.34290727636733E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1020.53138277
 0.11294217879568E+00 0.32105709281074E+03 0.45657841376528E+03 0.45147638934465E+03 0.44943627333782E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14716033988611E+00 0.00000000000000E+00 -.23347955819884E+02
 0.31361381580412E-02 0.11497400751562E+01 0.25509080266402E+04 0.95659050999008E+03 0.69580944187869E+01
 0.26092854070451E+01 0.35325888768737E+03 0.29815419988079E+03 0.34535067590674E+03 0.37592130572674E+03
 0.29815073984145E+03 0.29815137804720E+03 0.34120958981843E+03 0.37590011224870E+03 0.29815062203264E+03
 0.29815137519549E+03 0.34535067590674E+03 0.37592130572674E+03 0.29815073984145E+03 0.29815137804720E+03
 0.34120958981843E+03 0.37590011224870E+03 0.29815062203264E+03 0.29815137519549E+03 0.42072001445703E+03
 0.34633076420887E+03 0.20362363923060E+04 0.18633801976092E+04 0.55789304646309E+03 0.88142362321069E+03
 0.32074111151529E+03 0.12197485038308E+04 0.11465097008241E+04 0.10944500674714E+04 0.17162055754544E+04
 0.11158905286973E+04 0.11462060819436E+04 0.10170077709838E+04 0.17160861397839E+04 0.12197485038308E+04
 0.11465097008241E+04 0.10944500674714E+04 0.17162055754544E+04 0.11158905286973E+04 0.11462060819436E+04
 0.10170077709838E+04 0.17160861397838E+04 0.16737044723688E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49522937370844E+03 0.12948765167084E+01
 0.12948765167084E+01 0.39021447806513E+01 0.00000000000000E+00 0.34307676532947E+03 0.34307676532947E+03
 0.34307676532947E+03 0.34307676532947E+03 0.00000000000000E+00 0.00000000000000E+00 0.15196185433665E+00
 0.00000000000000E+00 -.15054291109019E+02 0.00000000000000E+00 0.14412437007660E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55507614678544E+01 0.20815355504454E+01 0.34633366281073E+03 0.42071671365837E+03
 0.31376611979228E+03 0.31376611979228E+03 0.29815020195375E+03 0.29815019630256E+03 0.31376125875690E+03
 0.31376125875690E+03 0.29815020199771E+03 0.29815019634530E+03 0.31376611979228E+03 0.31376611979228E+03
 0.29815020195375E+03 0.29815019630256E+03 0.31376125875690E+03 0.31376125875690E+03 0.29815020199771E+03
 0.29815019634530E+03 0.31152143482065E+03 0.29815080875448E+03 -.44949120223547E+02 -.52852059746325E+02
 0.21903583046880E+03 0.49497481384545E+03 0.27484380422430E+03 0.21485622536992E+03 0.20162054729862E+03
 0.21485622536992E+03 0.39111223685018E+03 0.21488167479692E+03 0.20142670147501E+03 0.21488167479692E+03
 0.39095632295803E+03 0.21485622536992E+03 0.20162054729862E+03 0.21485622536992E+03 0.39111223685018E+03
 0.21488167479692E+03 0.20142670147501E+03 0.21488167479692E+03 0.39095632295804E+03 0.22342442237179E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36886239198824E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19172393598589E+00 0.00000000000000E+00 0.00000000000000E+00 0.19172393598589E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19941213441736E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19941213441736E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639136426838E+00 0.20149341938428E+00 0.34307676532947E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1033.39410672
 0.11283157636715E+00 0.32127168457652E+03 0.45695836951098E+03 0.45185512199325E+03 0.44981329972897E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14700051550475E+00 0.00000000000000E+00 -.23351814908569E+02
 0.31392120313194E-02 0.11510036213094E+01 0.25484102125582E+04 0.95565382970934E+03 0.69504559776267E+01
 0.26064209916100E+01 0.35369882625211E+03 0.29815461958129E+03 0.34573715904776E+03 0.37646401435827E+03
 0.29815082450101E+03 0.29815153504223E+03 0.34158156913137E+03 0.37644290636742E+03 0.29815069391804E+03
 0.29815153191031E+03 0.34573715904775E+03 0.37646401435827E+03 0.29815082450101E+03 0.29815153504223E+03
 0.34158156913137E+03 0.37644290636742E+03 0.29815069391804E+03 0.29815153191031E+03 0.42131423814319E+03
 0.34694326207561E+03 0.20397020716861E+04 0.18653320399250E+04 0.55628541337875E+03 0.87744589905363E+03
 0.31837905860799E+03 0.12223477287856E+04 0.11480901442613E+04 0.10959379900885E+04 0.17161430930399E+04
 0.11187098326227E+04 0.11477877180682E+04 0.10188573833710E+04 0.17160239223377E+04 0.12223477287856E+04
 0.11480901442613E+04 0.10959379900885E+04 0.17161430930399E+04 0.11187098326227E+04 0.11477877180682E+04
 0.10188573833710E+04 0.17160239223377E+04 0.16732335219113E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49560652706029E+03 0.12948765451437E+01
 0.12948765451437E+01 0.39535956764258E+01 0.00000000000000E+00 0.34333102127349E+03 0.34333102127349E+03
 0.34333102127349E+03 0.34333102127349E+03 0.00000000000000E+00 0.00000000000000E+00 0.15163420785358E+00
 0.00000000000000E+00 -.15036793773267E+02 0.00000000000000E+00 0.14462363986096E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55315991270109E+01 0.20743496726291E+01 0.34694621672282E+03 0.42131089750593E+03
 0.31393084468602E+03 0.31393084468602E+03 0.29815022561140E+03 0.29815021929821E+03 0.31392590291651E+03
 0.31392590291651E+03 0.29815022565958E+03 0.29815021934504E+03 0.31393084468602E+03 0.31393084468602E+03
 0.29815022561140E+03 0.29815021929821E+03 0.31392590291651E+03 0.31392590291651E+03 0.29815022565958E+03
 0.29815021934504E+03 0.31166546086956E+03 0.29815089182187E+03 -.48002851187320E+02 -.57079347709638E+02
 0.22039674079159E+03 0.49742117551132E+03 0.27592245101577E+03 0.21734502163082E+03 0.20279879943007E+03
 0.21734502163082E+03 0.39293408496339E+03 0.21737096896100E+03 0.20260331941635E+03 0.21737096896100E+03
 0.39277718203755E+03 0.21734502163082E+03 0.20279879943007E+03 0.21734502163082E+03 0.39293408496339E+03
 0.21737096896100E+03 0.20260331941635E+03 0.21737096896100E+03 0.39277718203755E+03 0.22354343581032E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36914394651366E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19161442443691E+00 0.00000000000000E+00 0.00000000000000E+00 0.19161442443691E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19927615307502E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19927615307502E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639138366977E+00 0.20134425684416E+00 0.34333102127349E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1041.96925601
 0.11275620092468E+00 0.32141458489217E+03 0.45721041340630E+03 0.45210647283141E+03 0.45006357609620E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14689683176160E+00 0.00000000000000E+00 -.23353499119285E+02
 0.31413103335934E-02 0.11518082619935E+01 0.25467079500066E+04 0.95501548125246E+03 0.69456004649195E+01
 0.26046001743448E+01 0.35399081274917E+03 0.29815491724117E+03 0.34599378215175E+03 0.37682377210684E+03
 0.29815088520025E+03 0.29815164753864E+03 0.34182866546604E+03 0.37680271964299E+03 0.29815074550212E+03
 0.29815164420852E+03 0.34599378215175E+03 0.37682377210684E+03 0.29815088520025E+03 0.29815164753864E+03
 0.34182866546604E+03 0.37680271964300E+03 0.29815074550212E+03 0.29815164420852E+03 0.42170741086325E+03
 0.34734727618477E+03 0.20419983281792E+04 0.18666259990264E+04 0.55521901503240E+03 0.87482054373835E+03
 0.31682543363079E+03 0.12240716085637E+04 0.11491331696551E+04 0.10969252942246E+04 0.17160997133810E+04
 0.11205796676543E+04 0.11488315053246E+04 0.10200839792582E+04 0.17159807004589E+04 0.12240716085637E+04
 0.11491331696551E+04 0.10969252942246E+04 0.17160997133810E+04 0.11205796676543E+04 0.11488315053246E+04
 0.10200839792582E+04 0.17159807004588E+04 0.16729232109237E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49585685608637E+03 0.12948765575536E+01
 0.12948765575536E+01 0.39878962736088E+01 0.00000000000000E+00 0.34350034370421E+03 0.34350034370421E+03
 0.34350034370421E+03 0.34350034370421E+03 0.00000000000000E+00 0.00000000000000E+00 0.15142214583613E+00
 0.00000000000000E+00 -.15024180994939E+02 0.00000000000000E+00 0.14494437273958E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55193588055838E+01 0.20697595520939E+01 0.34735026603220E+03 0.42170404712874E+03
 0.31404068742057E+03 0.31404068742057E+03 0.29815024260711E+03 0.29815023581834E+03 0.31403569193106E+03
 0.31403569193106E+03 0.29815024265825E+03 0.29815023586805E+03 0.31404068742057E+03 0.31404068742057E+03
 0.29815024260711E+03 0.29815023581834E+03 0.31403569193106E+03 0.31403569193106E+03 0.29815024265825E+03
 0.29815023586805E+03 0.31176153126821E+03 0.29815095085537E+03 -.50019416913318E+02 -.59885427258005E+02
 0.22130060688704E+03 0.49904584923159E+03 0.27663873931012E+03 0.21899213936342E+03 0.20358081941003E+03
 0.21899213936342E+03 0.39414319731332E+03 0.21901841919982E+03 0.20338425585080E+03 0.21901841919982E+03
 0.39398564025041E+03 0.21899213936342E+03 0.20358081941003E+03 0.21899213936342E+03 0.39414319731333E+03
 0.21901841919982E+03 0.20338425585080E+03 0.21901841919982E+03 0.39398564025041E+03 0.22362255362011E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36933103958806E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19153505352815E+00 0.00000000000000E+00 0.00000000000000E+00 0.19153505352815E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19918122204864E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19918122204864E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639142868481E+00 0.20124507893761E+00 0.34350034370421E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1050.54440531
 0.11267797626659E+00 0.32155745452105E+03 0.45746149621468E+03 0.45235703208651E+03 0.45031313987397E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14679532782467E+00 0.00000000000000E+00 -.23354198516496E+02
 0.31434909291539E-02 0.11525842382327E+01 0.25449413344270E+04 0.95435300041011E+03 0.69409243460303E+01
 0.26028466297614E+01 0.35428177711404E+03 0.29815522977216E+03 0.34624959614594E+03 0.37718191955908E+03
 0.29815094948362E+03 0.29815176662069E+03 0.34207506449796E+03 0.37716092148596E+03 0.29815080016878E+03
 0.29815176308297E+03 0.34624959614593E+03 0.37718191955908E+03 0.29815094948362E+03 0.29815176662069E+03
 0.34207506449796E+03 0.37716092148596E+03 0.29815080016878E+03 0.29815176308297E+03 0.42209825429182E+03
 0.34774789795777E+03 0.20442851179467E+04 0.18679166355132E+04 0.55415744979129E+03 0.87221682782138E+03
 0.31528859078113E+03 0.12257892519797E+04 0.11501685714668E+04 0.10979100436797E+04 0.17160558810045E+04
 0.11224427134068E+04 0.11498676446874E+04 0.10213066548240E+04 0.17159370127197E+04 0.12257892519797E+04
 0.11501685714669E+04 0.10979100436797E+04 0.17160558810046E+04 0.11224427134068E+04 0.11498676446874E+04
 0.10213066548240E+04 0.17159370127196E+04 0.16726172973377E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49610643240694E+03 0.12948765627071E+01
 0.12948765627071E+01 0.40221968707918E+01 0.00000000000000E+00 0.34366936271718E+03 0.34366936271718E+03
 0.34366936271718E+03 0.34366936271718E+03 0.00000000000000E+00 0.00000000000000E+00 0.15121503611592E+00
 0.00000000000000E+00 -.15010479498253E+02 0.00000000000000E+00 0.14525576387280E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55075267147442E+01 0.20653225180291E+01 0.34775092156114E+03 0.42209486981621E+03
 0.31415053276876E+03 0.31415053276876E+03 0.29815026063465E+03 0.29815025334142E+03 0.31414548363808E+03
 0.31414548363808E+03 0.29815026068886E+03 0.29815025339411E+03 0.31415053276876E+03 0.31415053276876E+03
 0.29815026063465E+03 0.29815025334142E+03 0.31414548363808E+03 0.31414548363808E+03 0.29815026068886E+03
 0.29815025339411E+03 0.31185762889207E+03 0.29815101293851E+03 -.52021879732165E+02 -.62682223816147E+02
 0.22220076005573E+03 0.50066297112179E+03 0.27735120726579E+03 0.22062919816144E+03 0.20435908108176E+03
 0.22062919816144E+03 0.39534578499985E+03 0.22065581098525E+03 0.20416143832489E+03 0.22065581098525E+03
 0.39518757755403E+03 0.22062919816144E+03 0.20435908108176E+03 0.22062919816144E+03 0.39534578499985E+03
 0.22065581098525E+03 0.20416143832490E+03 0.22065581098525E+03 0.39518757755404E+03 0.22370012995981E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36951742685123E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19144841312487E+00 0.00000000000000E+00 0.00000000000000E+00 0.19144841312487E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19908111833669E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19908111833669E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639151059281E+00 0.20114621629454E+00 0.34366936271718E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1066.00150484
 0.11254079535666E+00 0.32180838887021E+03 0.45791036240718E+03 0.45280468762336E+03 0.45075881510310E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14661765171515E+00 0.00000000000000E+00 -.23362023483293E+02
 0.31473223066881E-02 0.11539168908244E+01 0.25418432624457E+04 0.95319122341712E+03 0.69329083087471E+01
 0.25998406157802E+01 0.35480256216696E+03 0.29815583830545E+03 0.34670751135839E+03 0.37782344941365E+03
 0.29815107639580E+03 0.29815200153172E+03 0.34251605199768E+03 0.37780254628303E+03 0.29815090821171E+03
 0.29815199759135E+03 0.34670751135839E+03 0.37782344941365E+03 0.29815107639580E+03 0.29815200153172E+03
 0.34251605199768E+03 0.37780254628303E+03 0.29815090821171E+03 0.29815199759135E+03 0.42280268524627E+03
 0.34846809612228E+03 0.20483651322318E+04 0.18701757711127E+04 0.55214425887878E+03 0.86739114127139E+03
 0.31248616109822E+03 0.12288604751342E+04 0.11519935010601E+04 0.10996354809534E+04 0.17159430114351E+04
 0.11257759592691E+04 0.11516938149816E+04 0.10234618370874E+04 0.17158243447098E+04 0.12288604751342E+04
 0.11519935010601E+04 0.10996354809534E+04 0.17159430114351E+04 0.11257759592691E+04 0.11516938149816E+04
 0.10234618370874E+04 0.17158243447098E+04 0.16719966948767E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49655223971072E+03 0.12948766203645E+01
 0.12948766203645E+01 0.40840252689062E+01 0.00000000000000E+00 0.34397322225290E+03 0.34397322225290E+03
 0.34397322225290E+03 0.34397322225290E+03 0.00000000000000E+00 0.00000000000000E+00 0.15085385198420E+00
 0.00000000000000E+00 -.14993242950242E+02 0.10000000000000E-02 0.14579419435039E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54871869457117E+01 0.20576951046419E+01 0.34847115548609E+03 0.42279928747604E+03
 0.31434718922750E+03 0.31434718922750E+03 0.29815029956586E+03 0.29815028802399E+03 0.31434204392987E+03
 0.31434204392987E+03 0.29815029962661E+03 0.29815028808239E+03 0.31434718922750E+03 0.31434718922750E+03
 0.29815029956586E+03 0.29815028802399E+03 0.31434204392987E+03 0.31434204392987E+03 0.29815029962661E+03
 0.29815028808239E+03 0.31202965839266E+03 0.29815113413557E+03 -.55653197049457E+02 -.67777502122771E+02
 0.22382368190152E+03 0.50358445059156E+03 0.27864165028054E+03 0.22358783583011E+03 0.20576177690937E+03
 0.22358783583011E+03 0.39751955010476E+03 0.22361504680988E+03 0.20556212501135E+03 0.22361504680988E+03
 0.39736010335869E+03 0.22358783583011E+03 0.20576177690937E+03 0.22358783583011E+03 0.39751955010476E+03
 0.22361504680988E+03 0.20556212501135E+03 0.22361504680988E+03 0.39736010335870E+03 0.22383039905441E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36985323141483E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19134461287798E+00 0.00000000000000E+00 0.00000000000000E+00 0.19134461287798E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19892452180262E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19892452180262E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639141512342E+00 0.20096846398434E+00 0.34397322225290E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1073.73005460
 0.11247457838388E+00 0.32193413799856E+03 0.45813362960411E+03 0.45302730280596E+03 0.45098044740865E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14653129768871E+00 0.00000000000000E+00 -.23363319365472E+02
 0.31491750450287E-02 0.11545508284669E+01 0.25403478325630E+04 0.95263043721112E+03 0.69291016062264E+01
 0.25984131023349E+01 0.35506191566590E+03 0.29815616252301E+03 0.34693570220870E+03 0.37814222438106E+03
 0.29815114478592E+03 0.29815212803408E+03 0.34273598107110E+03 0.37812136744662E+03 0.29815096648526E+03
 0.29815212387991E+03 0.34693570220870E+03 0.37814222438106E+03 0.29815114478592E+03 0.29815212803408E+03
 0.34273598107110E+03 0.37812136744662E+03 0.29815096648526E+03 0.29815212387991E+03 0.42314998876161E+03
 0.34882216256301E+03 0.20503875909058E+04 0.18712962145868E+04 0.55117597002035E+03 0.86505876099562E+03
 0.31112691112517E+03 0.12303857785796E+04 0.11528968871084E+04 0.11004943053496E+04 0.17158866738835E+04
 0.11274309190354E+04 0.11525978049428E+04 0.10245324968005E+04 0.17157681041552E+04 0.12303857785796E+04
 0.11528968871084E+04 0.11004943053496E+04 0.17158866738835E+04 0.11274309190354E+04 0.11525978049428E+04
 0.10245324968005E+04 0.17157681041552E+04 0.16717031073920E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49677393266413E+03 0.12948766299131E+01
 0.12948766299131E+01 0.41149394679633E+01 0.00000000000000E+00 0.34412505550754E+03 0.34412505550754E+03
 0.34412505550754E+03 0.34412505550754E+03 0.00000000000000E+00 0.00000000000000E+00 0.15067890444082E+00
 0.00000000000000E+00 -.14982021603852E+02 0.10000000000000E-02 0.14605267737854E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54774757598352E+01 0.20540534099382E+01 0.34882524914864E+03 0.42314657580080E+03
 0.31444585601023E+03 0.31444585601023E+03 0.29815031904473E+03 0.29815030675237E+03 0.31444066251016E+03
 0.31444066251016E+03 0.29815031910860E+03 0.29815030681377E+03 0.31444585601023E+03 0.31444585601023E+03
 0.29815031904473E+03 0.29815030675237E+03 0.31444066251016E+03 0.31444066251016E+03 0.29815031910860E+03
 0.29815030681377E+03 0.31211601307915E+03 0.29815119884555E+03 -.57432035071942E+02 -.70283333001510E+02
 0.22463044973516E+03 0.50503567750682E+03 0.27928207552298E+03 0.22504665026479E+03 0.20645852397904E+03
 0.22504665026479E+03 0.39859825193633E+03 0.22507416256694E+03 0.20625789614021E+03 0.22507416256694E+03
 0.39843821514482E+03 0.22504665026479E+03 0.20645852397904E+03 0.22504665026479E+03 0.39859825193633E+03
 0.22507416256694E+03 0.20625789614021E+03 0.22507416256694E+03 0.39843821514483E+03 0.22390083384719E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37002073377394E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19127305843696E+00 0.00000000000000E+00 0.00000000000000E+00 0.19127305843696E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19884429132491E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19884429132491E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639145378986E+00 0.20087985542218E+00 0.34412505550754E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1081.45860437
 0.11240449456496E+00 0.32206135605398E+03 0.45835631340953E+03 0.45324959147843E+03 0.45120189219472E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14644652568167E+00 0.00000000000000E+00 -.23361720395133E+02
 0.31511383653368E-02 0.11551637686672E+01 0.25387650659843E+04 0.95203689974413E+03 0.69254249631027E+01
 0.25970343611635E+01 0.35532048651646E+03 0.29815650056116E+03 0.34716328081203E+03 0.37845968859590E+03
 0.29815121662962E+03 0.29815226086317E+03 0.34295539685347E+03 0.37843887700960E+03 0.29815102773745E+03
 0.29815225648661E+03 0.34716328081203E+03 0.37845968859590E+03 0.29815121662962E+03 0.29815226086317E+03
 0.34295539685347E+03 0.37843887700960E+03 0.29815102773745E+03 0.29815225648661E+03 0.42349510476483E+03
 0.34917323844824E+03 0.20524066569687E+04 0.18724291585848E+04 0.55022060404429E+03 0.86275806150080E+03
 0.30978635443629E+03 0.12319082147278E+04 0.11537981238527E+04 0.11013627406350E+04 0.17158355825597E+04
 0.11290825195142E+04 0.11534996308961E+04 0.10256111094645E+04 0.17157171035530E+04 0.12319082147278E+04
 0.11537981238527E+04 0.11013627406350E+04 0.17158355825597E+04 0.11290825195142E+04 0.11534996308961E+04
 0.10256111094645E+04 0.17157171035530E+04 0.16714238250513E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49699536396454E+03 0.12948766181312E+01
 0.12948766181312E+01 0.41458536670205E+01 0.00000000000000E+00 0.34427652876715E+03 0.34427652876715E+03
 0.34427652876715E+03 0.34427652876715E+03 0.00000000000000E+00 0.00000000000000E+00 0.15050760842713E+00
 0.00000000000000E+00 -.14967433714053E+02 0.10000000000000E-02 0.14630435248311E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54680533177736E+01 0.20505199941651E+01 0.34917635502754E+03 0.42349167474631E+03
 0.31454457293001E+03 0.31454457293001E+03 0.29815033953530E+03 0.29815032645346E+03 0.31453933127858E+03
 0.31453933127858E+03 0.29815033960237E+03 0.29815032651794E+03 0.31454457293001E+03 0.31454457293001E+03
 0.29815033953530E+03 0.29815032645346E+03 0.31453933127858E+03 0.31453933127858E+03 0.29815033960237E+03
 0.29815032651794E+03 0.31220243652097E+03 0.29815126640930E+03 -.59197268569113E+02 -.72775850186386E+02
 0.22543316973205E+03 0.50647701821970E+03 0.27991668263898E+03 0.22649541805920E+03 0.20715122235636E+03
 0.22649541805920E+03 0.39966836552963E+03 0.22652323221370E+03 0.20694962510397E+03 0.22652323221370E+03
 0.39950774459708E+03 0.22649541805920E+03 0.20715122235636E+03 0.22649541805920E+03 0.39966836552963E+03
 0.22652323221370E+03 0.20694962510397E+03 0.22652323221370E+03 0.39950774459708E+03 0.22396951547498E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37018693559681E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19117925790903E+00 0.00000000000000E+00 0.00000000000000E+00 0.19117925790903E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19874688328933E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19874688328933E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639160443411E+00 0.20079165613651E+00 0.34427652876715E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1096.91570389
 0.11223549984527E+00 0.32231955226795E+03 0.45880013099543E+03 0.45369414233788E+03 0.45164544092964E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14628123725873E+00 0.00000000000000E+00 -.23350888778788E+02
 0.31558827215764E-02 0.11563330552294E+01 0.25349484457407E+04 0.95060566715276E+03 0.69184219579478E+01
 0.25944082342304E+01 0.35583544517799E+03 0.29815721968769E+03 0.34761674927714E+03 0.37909089796944E+03
 0.29815137118607E+03 0.29815254641488E+03 0.34339281153967E+03 0.37907017466952E+03 0.29815115962372E+03
 0.29815254156690E+03 0.34761674927714E+03 0.37909089796944E+03 0.29815137118607E+03 0.29815254641488E+03
 0.34339281153967E+03 0.37907017466952E+03 0.29815115962372E+03 0.29815254156690E+03 0.42417961758528E+03
 0.34986703019178E+03 0.20564497459343E+04 0.18747418699246E+04 0.54834144814574E+03 0.85823797942595E+03
 0.30715482403948E+03 0.12349511185721E+04 0.11555971903163E+04 0.11031261663299E+04 0.17157513138914E+04
 0.11323830497676E+04 0.11552998415893E+04 0.10277918260737E+04 0.17156330059906E+04 0.12349511185721E+04
 0.11555971903163E+04 0.11031261663299E+04 0.17157513138914E+04 0.11323830497676E+04 0.11552998415893E+04
 0.10277918260737E+04 0.17156330059906E+04 0.16709105476329E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49743853068063E+03 0.12948765383196E+01
 0.12948765383196E+01 0.42076820651348E+01 0.00000000000000E+00 0.34457714218878E+03 0.34457714218878E+03
 0.34457714218878E+03 0.34457714218878E+03 0.00000000000000E+00 0.00000000000000E+00 0.15017569072299E+00
 0.00000000000000E+00 -.14929270459265E+02 0.00000000000000E+00 0.14678836779750E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54500231319666E+01 0.20437586744875E+01 0.34987020396410E+03 0.42417615901708E+03
 0.31474197896421E+03 0.31474197896421E+03 0.29815038370635E+03 0.29815036892266E+03 0.31473664117931E+03
 0.31473664117931E+03 0.29815038378013E+03 0.29815036899359E+03 0.31474197896421E+03 0.31474197896421E+03
 0.29815038370635E+03 0.29815036892266E+03 0.31473664117931E+03 0.31473664117931E+03 0.29815038378013E+03
 0.29815036899359E+03 0.31237533478342E+03 0.29815141044379E+03 -.62700727571265E+02 -.77739125291578E+02
 0.22701820569269E+03 0.50931111404873E+03 0.28115781732758E+03 0.22936073569663E+03 0.20851628543584E+03
 0.22936073569663E+03 0.40176692576137E+03 0.22938915503674E+03 0.20831276450771E+03 0.22938915503674E+03
 0.40160514945346E+03 0.22936073569663E+03 0.20851628543584E+03 0.22936073569663E+03 0.40176692576137E+03
 0.22938915503674E+03 0.20831276450771E+03 0.22938915503674E+03 0.40160514945346E+03 0.22408794187318E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37051459282794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19093243099691E+00 0.00000000000000E+00 0.00000000000000E+00 0.19093243099691E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19850462486270E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19850462486270E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639220773132E+00 0.20061717485047E+00 0.34457714218878E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1104.64425366
 0.11213103045516E+00 0.32245010147410E+03 0.45902141226011E+03 0.45391678499043E+03 0.45186800242421E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14620037617951E+00 0.00000000000000E+00 -.23341724258038E+02
 0.31588227883168E-02 0.11568938524403E+01 0.25325890485496E+04 0.94972089320608E+03 0.69150682952676E+01
 0.25931506107254E+01 0.35609190914468E+03 0.29815760158176E+03 0.34784269553530E+03 0.37940472781371E+03
 0.29815145415950E+03 0.29815269960522E+03 0.34361086228540E+03 0.37938404752203E+03 0.29815123048667E+03
 0.29815269450781E+03 0.34784269553530E+03 0.37940472781371E+03 0.29815145415950E+03 0.29815269960522E+03
 0.34361086228540E+03 0.37938404752203E+03 0.29815123048667E+03 0.29815269450781E+03 0.42451922792539E+03
 0.35020984976339E+03 0.20584802741774E+04 0.18759244674149E+04 0.54741818455200E+03 0.85601844601611E+03
 0.30586317054135E+03 0.12364747496415E+04 0.11564976847169E+04 0.11040198527007E+04 0.17157210361851E+04
 0.11340353573965E+04 0.11562008974109E+04 0.10288932726698E+04 0.17156028143876E+04 0.12364747496415E+04
 0.11564976847169E+04 0.11040198527007E+04 0.17157210361851E+04 0.11340353573965E+04 0.11562008974109E+04
 0.10288932726698E+04 0.17156028143876E+04 0.16706795497360E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49766068612866E+03 0.12948764707918E+01
 0.12948764707918E+01 0.42385962641919E+01 0.00000000000000E+00 0.34472581877756E+03 0.34472581877756E+03
 0.34472581877756E+03 0.34472581877756E+03 0.00000000000000E+00 0.00000000000000E+00 0.15001492541469E+00
 0.00000000000000E+00 -.14905729228488E+02 0.00000000000000E+00 0.14702119087683E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54413924634184E+01 0.20405221737819E+01 0.35021304941183E+03 0.42451575904662E+03
 0.31484059161919E+03 0.31484059161919E+03 0.29815040746640E+03 0.29815039176726E+03 0.31483520584741E+03
 0.31483520584741E+03 0.29815040754367E+03 0.29815039184155E+03 0.31484059161919E+03 0.31484059161919E+03
 0.29815040746640E+03 0.29815039176726E+03 0.31483520584741E+03 0.31483520584741E+03 0.29815040754367E+03
 0.29815039184155E+03 0.31246174510684E+03 0.29815148709027E+03 -.64442960272530E+02 -.80215009258240E+02
 0.22779743732339E+03 0.51069674455298E+03 0.28176032004297E+03 0.23077606764192E+03 0.20918563544067E+03
 0.23077606764192E+03 0.40278941722481E+03 0.23080479034903E+03 0.20898115974397E+03 0.23080479034903E+03
 0.40262706908583E+03 0.23077606764192E+03 0.20918563544067E+03 0.23077606764192E+03 0.40278941722481E+03
 0.23080479034903E+03 0.20898115974397E+03 0.23080479034903E+03 0.40262706908583E+03 0.22413295986150E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37067564389734E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19077967955294E+00 0.00000000000000E+00 0.00000000000000E+00 0.19077967955294E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19835927005072E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19835927005072E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639266025582E+00 0.20053115946450E+00 0.34472581877756E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1112.37280342
 0.11201054004126E+00 0.32258166469225E+03 0.45924240122983E+03 0.45413992026249E+03 0.45209138204655E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14612043078252E+00 0.00000000000000E+00 -.23329660844659E+02
 0.31622205693861E-02 0.11574422450580E+01 0.25298678015851E+04 0.94870042559442E+03 0.69117919569275E+01
 0.25919219838478E+01 0.35634774063938E+03 0.29815799890761E+03 0.34806814490414E+03 0.37971742171117E+03
 0.29815154110783E+03 0.29815286005769E+03 0.34382850291552E+03 0.37969678372015E+03 0.29815130478603E+03
 0.29815285470142E+03 0.34806814490414E+03 0.37971742171117E+03 0.29815154110783E+03 0.29815286005769E+03
 0.34382850291552E+03 0.37969678372015E+03 0.29815130478603E+03 0.29815285470142E+03 0.42485717652601E+03
 0.35054996972492E+03 0.20605210710488E+04 0.18771289238734E+04 0.54650731165202E+03 0.85382766964955E+03
 0.30458782143928E+03 0.12380020445923E+04 0.11574012467715E+04 0.11049232518313E+04 0.17157015940633E+04
 0.11356913255632E+04 0.11571050184169E+04 0.10300040459958E+04 0.17155834629510E+04 0.12380020445923E+04
 0.11574012467715E+04 0.11049232518313E+04 0.17157015940633E+04 0.11356913255632E+04 0.11571050184169E+04
 0.10300040459958E+04 0.17155834629510E+04 0.16704691843227E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49788349335666E+03 0.12948763819038E+01
 0.12948763819038E+01 0.42695104632491E+01 0.00000000000000E+00 0.34487316203724E+03 0.34487316203724E+03
 0.34487316203724E+03 0.34487316203724E+03 0.00000000000000E+00 0.00000000000000E+00 0.14985752177586E+00
 0.00000000000000E+00 -.14878721133290E+02 0.00000000000000E+00 0.14724819615152E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54330037372870E+01 0.20373764014826E+01 0.35055319370998E+03 0.42485369962555E+03
 0.31493909791667E+03 0.31493909791667E+03 0.29815043239727E+03 0.29815041573757E+03 0.31493366421074E+03
 0.31493366421074E+03 0.29815043247811E+03 0.29815041581530E+03 0.31493909791667E+03 0.31493909791667E+03
 0.29815043239727E+03 0.29815041573757E+03 0.31493366421074E+03 0.31493366421074E+03 0.29815043247811E+03
 0.29815041581530E+03 0.31254809135266E+03 0.29815156694198E+03 -.66180643414520E+02 -.82689284023614E+02
 0.22856616429831E+03 0.51205745272021E+03 0.28234845760040E+03 0.23217906640432E+03 0.20984460595033E+03
 0.23217906640432E+03 0.40379071769061E+03 0.23220809300031E+03 0.20963918029408E+03 0.23220809300031E+03
 0.40362780161445E+03 0.23217906640432E+03 0.20984460595033E+03 0.23217906640432E+03 0.40379071769061E+03
 0.23220809300031E+03 0.20963918029408E+03 0.23220809300031E+03 0.40362780161445E+03 0.22416602586523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37083450122753E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19060410039334E+00 0.00000000000000E+00 0.00000000000000E+00 0.19060410039334E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19819488735740E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19819488735740E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639323015637E+00 0.20044611985174E+00 0.34487316203724E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1120.94847683
 0.11193220090110E+00 0.32272314130586E+03 0.45948878648544E+03 0.45438595992790E+03 0.45233654165917E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14603295906606E+00 0.00000000000000E+00 -.23361580955304E+02
 0.31644335464428E-02 0.11580295982724E+01 0.25280985941364E+04 0.94803697280115E+03 0.69082862924529E+01
 0.25906073596699E+01 0.35663224454100E+03 0.29815845021520E+03 0.34831911247154E+03 0.38006272912502E+03
 0.29815164028190E+03 0.29815304302028E+03 0.34407129486476E+03 0.38004213737586E+03 0.29815138956009E+03
 0.29815303737041E+03 0.34831911247154E+03 0.38006272912502E+03 0.29815164028190E+03 0.29815304302028E+03
 0.34407129486477E+03 0.38004213737586E+03 0.29815138956009E+03 0.29815303737041E+03 0.42522121671410E+03
 0.35091379545576E+03 0.20627578759287E+04 0.18783986003883E+04 0.54567731388047E+03 0.85170396305714E+03
 0.30329826260727E+03 0.12396880603312E+04 0.11584348584113E+04 0.11058922812827E+04 0.17157372857507E+04
 0.11375147990412E+04 0.11581392608115E+04 0.10311943811064E+04 0.17156192742693E+04 0.12396880603312E+04
 0.11584348584113E+04 0.11058922812827E+04 0.17157372857507E+04 0.11375147990412E+04 0.11581392608115E+04
 0.10311943811064E+04 0.17156192742693E+04 0.16703185739233E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49812860380929E+03 0.12948766171038E+01
 0.12948766171038E+01 0.43038131568877E+01 0.00000000000000E+00 0.34504019473342E+03 0.34504019473342E+03
 0.34504019473342E+03 0.34504019473342E+03 0.00000000000000E+00 0.00000000000000E+00 0.14968643806668E+00
 0.00000000000000E+00 -.14901568044184E+02 0.10000000000000E-02 0.14749168980139E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54240344054453E+01 0.20340129020420E+01 0.35091709450822E+03 0.42521769006941E+03
 0.31505043295030E+03 0.31505043295030E+03 0.29815044838014E+03 0.29815044309904E+03 0.31504494568816E+03
 0.31504494568816E+03 0.29815044846272E+03 0.29815044318065E+03 0.31505043295030E+03 0.31505043295030E+03
 0.29815044838014E+03 0.29815044309904E+03 0.31504494568816E+03 0.31504494568816E+03 0.29815044846272E+03
 0.29815044318065E+03 0.31264583609098E+03 0.29815165771415E+03 -.67970949314620E+02 -.85239254811220E+02
 0.22943031811036E+03 0.51361615070968E+03 0.28303868100877E+03 0.23368887928591E+03 0.21058779106447E+03
 0.23368887928591E+03 0.40494629765013E+03 0.23371824793858E+03 0.21038142834312E+03 0.23371824793858E+03
 0.40478287567545E+03 0.23368887928591E+03 0.21058779106447E+03 0.23368887928591E+03 0.40494629765013E+03
 0.23371824793858E+03 0.21038142834312E+03 0.23371824793858E+03 0.40478287567545E+03 0.22427473367371E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37102739055417E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19076130461682E+00 0.00000000000000E+00 0.00000000000000E+00 0.19076130461682E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19827179127498E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19827179127498E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639211531605E+00 0.20034790212531E+00 0.34504019473342E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1130.75430094
 0.11185679119071E+00 0.32288102831811E+03 0.45976748565756E+03 0.45466359236575E+03 0.45261287570072E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14593509792880E+00 0.00000000000000E+00 -.23367398826386E+02
 0.31665666672439E-02 0.11586786559068E+01 0.25263955699259E+04 0.94739833872222E+03 0.69044164740735E+01
 0.25891561777775E+01 0.35695549469980E+03 0.29815899081090E+03 0.34860421776040E+03 0.38045655084662E+03
 0.29815176006449E+03 0.29815326387877E+03 0.34434685592327E+03 0.38043601098488E+03 0.29815149201634E+03
 0.29815325787826E+03 0.34860421776040E+03 0.38045655084662E+03 0.29815176006449E+03 0.29815326387877E+03
 0.34434685592327E+03 0.38043601098488E+03 0.29815149201634E+03 0.29815325787826E+03 0.42564307889335E+03
 0.35133528089818E+03 0.20652704071901E+04 0.18797898212342E+04 0.54457178118934E+03 0.84903096598271E+03
 0.30173632588742E+03 0.12415914699658E+04 0.11595661477307E+04 0.11069639526871E+04 0.17157134070870E+04
 0.11395766989054E+04 0.11592712047049E+04 0.10325218592847E+04 0.17155954744589E+04 0.12415914699658E+04
 0.11595661477307E+04 0.11069639526871E+04 0.17157134070870E+04 0.11395766989054E+04 0.11592712047049E+04
 0.10325218592847E+04 0.17155954744589E+04 0.16700330842613E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49840503820900E+03 0.12948766599722E+01
 0.12948766599722E+01 0.43430364533362E+01 0.00000000000000E+00 0.34523070662140E+03 0.34523070662140E+03
 0.34523070662140E+03 0.34523070662140E+03 0.00000000000000E+00 0.00000000000000E+00 0.14949547725509E+00
 0.00000000000000E+00 -.14891981614681E+02 0.10000000000000E-02 0.14776149978271E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54141302110253E+01 0.20302988291345E+01 0.35133860833882E+03 0.42563954825583E+03
 0.31517633696727E+03 0.31517633696727E+03 0.29815048187151E+03 0.29815047619595E+03 0.31517078900831E+03
 0.31517078900831E+03 0.29815048195871E+03 0.29815047628213E+03 0.31517633696727E+03 0.31517633696727E+03
 0.29815048187151E+03 0.29815047619595E+03 0.31517078900831E+03 0.31517078900831E+03 0.29815048195871E+03
 0.29815047628213E+03 0.31275629996252E+03 0.29815176661554E+03 -.70083640580051E+02 -.88253637212277E+02
 0.23042487151764E+03 0.51539769193891E+03 0.28382069606369E+03 0.23544739012863E+03 0.21144296036688E+03
 0.23544739012863E+03 0.40626476844374E+03 0.23547714611165E+03 0.21123542633350E+03 0.23547714611165E+03
 0.40610066163285E+03 0.23544739012863E+03 0.21144296036688E+03 0.23544739012863E+03 0.40626476844374E+03
 0.23547714611165E+03 0.21123542633350E+03 0.23547714611165E+03 0.40610066163285E+03 0.22437431085021E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37123750736209E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19070508446395E+00 0.00000000000000E+00 0.00000000000000E+00 0.19070508446395E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19817583859918E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19817583859918E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639201841582E+00 0.20023726242611E+00 0.34523070662140E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1143.12994584
 0.11177201041649E+00 0.32307520081777E+03 0.46011503265278E+03 0.45500929348233E+03 0.45295670302763E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14581493669184E+00 0.00000000000000E+00 -.23376803045456E+02
 0.31689682793262E-02 0.11594611090892E+01 0.25244809334920E+04 0.94668035005950E+03 0.68997570830852E+01
 0.25874089061569E+01 0.35736063102392E+03 0.29815971649192E+03 0.34896154532827E+03 0.38095100904018E+03
 0.29815192265163E+03 0.29815356342965E+03 0.34469206942386E+03 0.38093053286740E+03 0.29815163120597E+03
 0.29815355696041E+03 0.34896154532827E+03 0.38095100904018E+03 0.29815192265163E+03 0.29815356342965E+03
 0.34469206942386E+03 0.38093053286740E+03 0.29815163120597E+03 0.29815355696041E+03 0.42617707145192E+03
 0.35186874208771E+03 0.20683821026563E+04 0.18814666685920E+04 0.54305338049268E+03 0.84547782499664E+03
 0.29970917760150E+03 0.12439604025260E+04 0.11609340344108E+04 0.11082659268169E+04 0.17156138126979E+04
 0.11421461653694E+04 0.11606398555829E+04 0.10341482026908E+04 0.17154959341413E+04 0.12439604025260E+04
 0.11609340344108E+04 0.11082659268169E+04 0.17156138126979E+04 0.11421461653694E+04 0.11606398555829E+04
 0.10341482026908E+04 0.17154959341413E+04 0.16695680874602E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49874912359091E+03 0.12948767292662E+01
 0.12948767292662E+01 0.43925390328982E+01 0.00000000000000E+00 0.34547032251941E+03 0.34547032251941E+03
 0.34547032251941E+03 0.34547032251941E+03 0.00000000000000E+00 0.00000000000000E+00 0.14926136714502E+00
 0.00000000000000E+00 -.14882582190508E+02 0.10000000000000E-02 0.14808926604734E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54021471059506E+01 0.20258051647315E+01 0.35187205566461E+03 0.42617357111984E+03
 0.31533388743900E+03 0.31533388743900E+03 0.29815053588214E+03 0.29815052121084E+03 0.31532826325611E+03
 0.31532826325611E+03 0.29815053597689E+03 0.29815052130299E+03 0.31533388743900E+03 0.31533388743900E+03
 0.29815053588214E+03 0.29815052121084E+03 0.31532826325611E+03 0.31532826325611E+03 0.29815053597689E+03
 0.29815052130299E+03 0.31289449070848E+03 0.29815191310978E+03 -.72789477984368E+02 -.92121849946136E+02
 0.23168264024661E+03 0.51765406328313E+03 0.28481300983529E+03 0.23768452519058E+03 0.21252424658924E+03
 0.23768452519058E+03 0.40793601122808E+03 0.23771476806444E+03 0.21231516430438E+03 0.23771476806444E+03
 0.40777096785972E+03 0.23768452519058E+03 0.21252424658924E+03 0.23768452519058E+03 0.40793601122808E+03
 0.23771476806444E+03 0.21231516430438E+03 0.23771476806444E+03 0.40777096785972E+03 0.22448872936510E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37150192699600E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19065207406742E+00 0.00000000000000E+00 0.00000000000000E+00 0.19065207406742E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19806864988062E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19806864988062E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639180962878E+00 0.20009818504818E+00 0.34547032251941E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1151.38037576
 0.11171419259024E+00 0.32320667900090E+03 0.46034532730499E+03 0.45523854951559E+03 0.45318484927915E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14573688158751E+00 0.00000000000000E+00 -.23376439084503E+02
 0.31706081990871E-02 0.11599574287658E+01 0.25231752073004E+04 0.94619070273765E+03 0.68968048323224E+01
 0.25863018121209E+01 0.35762974312717E+03 0.29816022485284E+03 0.34919904526357E+03 0.38127881821511E+03
 0.29815203757165E+03 0.29815377502320E+03 0.34492166293612E+03 0.38125838346631E+03 0.29815172965682E+03
 0.29815376822675E+03 0.34919904526357E+03 0.38127881821511E+03 0.29815203757165E+03 0.29815377502320E+03
 0.34492166293612E+03 0.38125838346631E+03 0.29815172965682E+03 0.29815376822675E+03 0.42652865421645E+03
 0.35221923960323E+03 0.20704375093494E+04 0.18825869739245E+04 0.54207146123973E+03 0.84317328122228E+03
 0.29839146267635E+03 0.12455280005709E+04 0.11618323861003E+04 0.11091396776085E+04 0.17155394523207E+04
 0.11438464073039E+04 0.11615387082485E+04 0.10352352006028E+04 0.17154216114332E+04 0.12455280005709E+04
 0.11618323861003E+04 0.11091396776085E+04 0.17155394523207E+04 0.11438464073039E+04 0.11615387082485E+04
 0.10352352006028E+04 0.17154216114332E+04 0.16692697174126E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49897736634622E+03 0.12948767265844E+01
 0.12948767265844E+01 0.44255407526061E+01 0.00000000000000E+00 0.34562965345132E+03 0.34562965345132E+03
 0.34562965345132E+03 0.34562965345132E+03 0.00000000000000E+00 0.00000000000000E+00 0.14910945311412E+00
 0.00000000000000E+00 -.14869175265403E+02 0.10000000000000E-02 0.14830006939943E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53944681431354E+01 0.20229255536758E+01 0.35222256226502E+03 0.42652515587573E+03
 0.31543919921139E+03 0.31543919921139E+03 0.29815056864849E+03 0.29815055308011E+03 0.31543352411995E+03
 0.31543352411995E+03 0.29815056874745E+03 0.29815055317637E+03 0.31543919921139E+03 0.31543919921139E+03
 0.29815056864849E+03 0.29815055308011E+03 0.31543352411995E+03 0.31543352411995E+03 0.29815056874745E+03
 0.29815055317637E+03 0.31298690498724E+03 0.29815201590791E+03 -.74562264966562E+02 -.94659733056623E+02
 0.23251519614114E+03 0.51914325701076E+03 0.28546548488891E+03 0.23915670550165E+03 0.21323926503795E+03
 0.23915670550165E+03 0.40903728477252E+03 0.23918727496676E+03 0.21302917550122E+03 0.23918727496676E+03
 0.40887164231374E+03 0.23915670550165E+03 0.21323926503795E+03 0.23915670550165E+03 0.40903728477252E+03
 0.23918727496676E+03 0.21302917550122E+03 0.23918727496676E+03 0.40887164231374E+03 0.22456735408569E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37167656131903E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19056582082397E+00 0.00000000000000E+00 0.00000000000000E+00 0.19056582082397E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19797728008082E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19797728008082E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639190695850E+00 0.20000606789440E+00 0.34562965345132E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1163.75602065
 0.11159704125862E+00 0.32340924046229E+03 0.46068943440209E+03 0.45558274657972E+03 0.45352814655575E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14562247938336E+00 0.00000000000000E+00 -.23363422031476E+02
 0.31739363330546E-02 0.11606675929491E+01 0.25205294500349E+04 0.94519854376309E+03 0.68925849645487E+01
 0.25847193617058E+01 0.35803192759151E+03 0.29816102556653E+03 0.34955415755882E+03 0.38176801560351E+03
 0.29815222019407E+03 0.29815411105365E+03 0.34526508155974E+03 0.38174764154886E+03 0.29815188621614E+03
 0.29815410374365E+03 0.34955415755882E+03 0.38176801560351E+03 0.29815222019407E+03 0.29815411105365E+03
 0.34526508155974E+03 0.38174764154886E+03 0.29815188621614E+03 0.29815410374365E+03 0.42705204211113E+03
 0.35273969376823E+03 0.20735269549795E+04 0.18843236651749E+04 0.54061892568028E+03 0.83976870837134E+03
 0.29644668806265E+03 0.12478783136841E+04 0.11631732748083E+04 0.11104852529652E+04 0.17154330539607E+04
 0.11463956970110E+04 0.11628803349677E+04 0.10368986540506E+04 0.17153152702408E+04 0.12478783136841E+04
 0.11631732748083E+04 0.11104852529652E+04 0.17154330539607E+04 0.11463956970110E+04 0.11628803349677E+04
 0.10368986540506E+04 0.17153152702408E+04 0.16688557448013E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49932040997174E+03 0.12948766306696E+01
 0.12948766306696E+01 0.44750433321681E+01 0.00000000000000E+00 0.34586638843842E+03 0.34586638843842E+03
 0.34586638843842E+03 0.34586638843842E+03 0.00000000000000E+00 0.00000000000000E+00 0.14888769369537E+00
 0.00000000000000E+00 -.14834587631903E+02 0.10000000000000E-02 0.14860568147698E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53833742562792E+01 0.20187653461047E+01 0.35274304497042E+03 0.42704853540584E+03
 0.31559706312939E+03 0.31559706312939E+03 0.29815062080262E+03 0.29815060380637E+03 0.31559131174907E+03
 0.31559131174907E+03 0.29815062090807E+03 0.29815060390894E+03 0.31559706312939E+03 0.31559706312939E+03
 0.29815062080262E+03 0.29815060380637E+03 0.31559131174907E+03 0.31559131174907E+03 0.29815062090807E+03
 0.29815060390894E+03 0.31312548752065E+03 0.29815217809804E+03 -.77208816953728E+02 -.98454460218727E+02
 0.23374601769904E+03 0.52133046086917E+03 0.28641571308164E+03 0.24134154731660E+03 0.21429387927942E+03
 0.24134154731660E+03 0.41064912244552E+03 0.24137260799562E+03 0.21408228811183E+03 0.24137260799562E+03
 0.41048258854900E+03 0.24134154731660E+03 0.21429387927942E+03 0.24134154731660E+03 0.41064912244552E+03
 0.24137260799562E+03 0.21408228811183E+03 0.24137260799562E+03 0.41048258854900E+03 0.22466499282218E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37193271895123E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19033934314760E+00 0.00000000000000E+00 0.00000000000000E+00 0.19033934314760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19777090215534E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19777090215534E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639253533950E+00 0.19986987761184E+00 0.34586638843842E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1170.94018725
 0.11153295702375E+00 0.32352701318720E+03 0.46088938499605E+03 0.45578257449217E+03 0.45372738710433E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14555734977407E+00 0.00000000000000E+00 -.23376535751020E+02
 0.31757598415136E-02 0.11610610066944E+01 0.25190821722171E+04 0.94465581458142E+03 0.68902494820458E+01
 0.25838435557672E+01 0.35826516903708E+03 0.29816150831485E+03 0.34976026951081E+03 0.38205048886954E+03
 0.29815233105095E+03 0.29815431492999E+03 0.34546464704552E+03 0.38203014930358E+03 0.29815198130265E+03
 0.29815430731124E+03 0.34976026951081E+03 0.38205048886954E+03 0.29815233105095E+03 0.29815431492999E+03
 0.34546464704552E+03 0.38203014930358E+03 0.29815198130265E+03 0.29815430731124E+03 0.42735028874707E+03
 0.35303480665670E+03 0.20753199815738E+04 0.18853338260639E+04 0.53985776086728E+03 0.83793589372096E+03
 0.29537884404934E+03 0.12492425541897E+04 0.11639657050169E+04 0.11112679312816E+04 0.17154008489263E+04
 0.11478736151467E+04 0.11636731972359E+04 0.10378622688868E+04 0.17152831099513E+04 0.12492425541897E+04
 0.11639657050169E+04 0.11112679312816E+04 0.17154008489264E+04 0.11478736151467E+04 0.11636731972359E+04
 0.10378622688868E+04 0.17152831099513E+04 0.16686669170971E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49951953435684E+03 0.12948767272967E+01
 0.12948767272967E+01 0.45037799985710E+01 0.00000000000000E+00 0.34600403061299E+03 0.34600403061299E+03
 0.34600403061299E+03 0.34600403061299E+03 0.00000000000000E+00 0.00000000000000E+00 0.14876219044331E+00
 0.00000000000000E+00 -.14838401150020E+02 0.10000000000000E-02 0.14877684429396E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53771808630336E+01 0.20164428236376E+01 0.35303819396068E+03 0.42734676156077E+03
 0.31568945095048E+03 0.31568945095048E+03 0.29815064363847E+03 0.29815063463692E+03 0.31568365515699E+03
 0.31568365515699E+03 0.29815064374628E+03 0.29815063474322E+03 0.31568945095048E+03 0.31568945095048E+03
 0.29815064363847E+03 0.29815063463692E+03 0.31568365515699E+03 0.31568365515699E+03 0.29815064374628E+03
 0.29815063474322E+03 0.31320667118338E+03 0.29815227600977E+03 -.78689253896489E+02 -.10057831600404E+03
 0.23445592889475E+03 0.52259982904871E+03 0.28697162050949E+03 0.24258175904764E+03 0.21490187565558E+03
 0.24258175904764E+03 0.41158596447927E+03 0.24261310733854E+03 0.21468946178704E+03 0.24261310733854E+03
 0.41141896486054E+03 0.24258175904764E+03 0.21490187565558E+03 0.24258175904764E+03 0.41158596447927E+03
 0.24261310733854E+03 0.21468946178704E+03 0.24261310733854E+03 0.41141896486054E+03 0.22474091252025E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37208748440825E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19036826205402E+00 0.00000000000000E+00 0.00000000000000E+00 0.19036826205402E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19776392180318E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19776392180318E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639211292096E+00 0.19978993110140E+00 0.34600403061299E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1180.70630050
 0.11145106140830E+00 0.32368319945567E+03 0.46115947708748E+03 0.45605218473397E+03 0.45399602093607E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14547040668678E+00 0.00000000000000E+00 -.23379761912761E+02
 0.31780932119819E-02 0.11615778506205E+01 0.25172326506469E+04 0.94396224399257E+03 0.68871836663607E+01
 0.25826938748852E+01 0.35858081019978E+03 0.29816219139775E+03 0.35003919034584E+03 0.38243334495599E+03
 0.29815248905264E+03 0.29815460535109E+03 0.34573461181568E+03 0.38241305146182E+03 0.29815211690393E+03
 0.29815459729681E+03 0.35003919034584E+03 0.38243334495599E+03 0.29815248905264E+03 0.29815460535109E+03
 0.34573461181568E+03 0.38241305146182E+03 0.29815211690393E+03 0.29815459729681E+03 0.42775757678068E+03
 0.35343744698420E+03 0.20777350753590E+04 0.18866644897975E+04 0.53875055000982E+03 0.83533653149159E+03
 0.29389222873173E+03 0.12510847449287E+04 0.11650181529199E+04 0.11123016053877E+04 0.17153293019219E+04
 0.11498707962827E+04 0.11647261955729E+04 0.10391434840482E+04 0.17152115944221E+04 0.12510847449287E+04
 0.11650181529199E+04 0.11123016053877E+04 0.17153293019219E+04 0.11498707962827E+04 0.11647261955729E+04
 0.10391434840482E+04 0.17152115944221E+04 0.16683567265633E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49978810948160E+03 0.12948767510683E+01
 0.12948767510683E+01 0.45428444515724E+01 0.00000000000000E+00 0.34619054137184E+03 0.34619054137184E+03
 0.34619054137184E+03 0.34619054137184E+03 0.00000000000000E+00 0.00000000000000E+00 0.14859528035470E+00
 0.00000000000000E+00 -.14826597823145E+02 0.10000000000000E-02 0.14900280289175E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53690265181198E+01 0.20133849442949E+01 0.35344085970178E+03 0.42775404549042E+03
 0.31581421794809E+03 0.31581421794809E+03 0.29815068826267E+03 0.29815067863702E+03 0.31580836205529E+03
 0.31580836205529E+03 0.29815068837572E+03 0.29815067874850E+03 0.31581421794809E+03 0.31581421794809E+03
 0.29815068826267E+03 0.29815067863702E+03 0.31580836205529E+03 0.31580836205529E+03 0.29815068837572E+03
 0.29815067874850E+03 0.31331628241076E+03 0.29815241474602E+03 -.80730636272011E+02 -.10350997698425E+03
 0.23542204689553E+03 0.52432052869333E+03 0.28772137156331E+03 0.24427993299198E+03 0.21572899926392E+03
 0.24427993299198E+03 0.41285449111899E+03 0.24431167073073E+03 0.21551541805714E+03 0.24431167073073E+03
 0.41268680611887E+03 0.24427993299198E+03 0.21572899926392E+03 0.24427993299198E+03 0.41285449111899E+03
 0.24431167073073E+03 0.21551541805714E+03 0.24431167073073E+03 0.41268680611887E+03 0.22483075754586E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37229248037570E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19029522450415E+00 0.00000000000000E+00 0.00000000000000E+00 0.19029522450415E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19766650843412E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19766650843412E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639209996013E+00 0.19968230402147E+00 0.34619054137184E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1192.17697307
 0.11135877239201E+00 0.32386431113923E+03 0.46147462520494E+03 0.45636658665733E+03 0.45430918964011E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14537060244650E+00 0.00000000000000E+00 -.23384760621575E+02
 0.31807268163818E-02 0.11621575783743E+01 0.25151484116138E+04 0.94318065435516E+03 0.68837480810400E+01
 0.25814055303900E+01 0.35894976015364E+03 0.29816303465657E+03 0.35036528521397E+03 0.38288086356301E+03
 0.29815268587484E+03 0.29815496687566E+03 0.34605024977029E+03 0.38286062288535E+03 0.29815228594137E+03
 0.29815495828583E+03 0.35036528521397E+03 0.38288086356301E+03 0.29815268587484E+03 0.29815496687566E+03
 0.34605024977029E+03 0.38286062288535E+03 0.29815228594137E+03 0.29815495828583E+03 0.42823459025658E+03
 0.35390826998965E+03 0.20805450424716E+04 0.18881946916533E+04 0.53741623305531E+03 0.83224498030093E+03
 0.29214166608034E+03 0.12532331643961E+04 0.11662306336661E+04 0.11134941667697E+04 0.17152249714072E+04
 0.11522008918978E+04 0.11659392913179E+04 0.10406262153832E+04 0.17151072818052E+04 0.12532331643961E+04
 0.11662306336661E+04 0.11134941667697E+04 0.17152249714072E+04 0.11522008918978E+04 0.11659392913179E+04
 0.10406262153832E+04 0.17151072818052E+04 0.16679625474784E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50010125815111E+03 0.12948767879008E+01
 0.12948767879008E+01 0.45887271418326E+01 0.00000000000000E+00 0.34640883294243E+03 0.34640883294243E+03
 0.34640883294243E+03 0.34640883294243E+03 0.00000000000000E+00 0.00000000000000E+00 0.14840452316341E+00
 0.00000000000000E+00 -.14814174053063E+02 0.10000000000000E-02 0.14925853205808E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53598276022754E+01 0.20099353508533E+01 0.35391169068917E+03 0.42823107133745E+03
 0.31596021629710E+03 0.31596021629710E+03 0.29815075042398E+03 0.29815073353829E+03 0.31595429000628E+03
 0.31595429000628E+03 0.29815075054438E+03 0.29815073365598E+03 0.31596021629710E+03 0.31596021629710E+03
 0.29815075042398E+03 0.29815073353829E+03 0.31595429000628E+03 0.31595429000628E+03 0.29815075054438E+03
 0.29815073365598E+03 0.31344455949438E+03 0.29815258631202E+03 -.83130964790066E+02 -.10696070503420E+03
 0.23655402043745E+03 0.52633603517074E+03 0.28859924463110E+03 0.24627204491666E+03 0.21669754364021E+03
 0.24627204491666E+03 0.41434011226625E+03 0.24630423992418E+03 0.21648256823895E+03 0.24630423992418E+03
 0.41417159789924E+03 0.24627204491666E+03 0.21669754364021E+03 0.24627204491666E+03 0.41434011226625E+03
 0.24630423992418E+03 0.21648256823895E+03 0.24630423992418E+03 0.41417159789924E+03 0.22493189118860E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37253239786752E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19021980523802E+00 0.00000000000000E+00 0.00000000000000E+00 0.19021980523802E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19755583416533E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19755583416533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639203952389E+00 0.19955643610930E+00 0.34640883294243E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1201.01202770
 0.11129248099149E+00 0.32400269624510E+03 0.46171593935041E+03 0.45660712318688E+03 0.45454869735195E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14529543503019E+00 0.00000000000000E+00 -.23388679689169E+02
 0.31826212243351E-02 0.11625837305838E+01 0.25136513069259E+04 0.94261924009721E+03 0.68812248008862E+01
 0.25804593003323E+01 0.35923279724218E+03 0.29816371527906E+03 0.35061554179765E+03 0.38322384484190E+03
 0.29815284609562E+03 0.29815526097317E+03 0.34629256476014E+03 0.38320364388312E+03 0.29815242363602E+03
 0.29815525195274E+03 0.35061554179765E+03 0.38322384484190E+03 0.29815284609562E+03 0.29815526097317E+03
 0.34629256476014E+03 0.38320364388312E+03 0.29815242363602E+03 0.29815525195274E+03 0.42859909474916E+03
 0.35426739591683E+03 0.20826881867931E+04 0.18893510183693E+04 0.53639600083260E+03 0.82988872339368E+03
 0.29081074255692E+03 0.12548759060322E+04 0.11671500767591E+04 0.11143993691548E+04 0.17151346492526E+04
 0.11539827151777E+04 0.11668591951416E+04 0.10417532505990E+04 0.17150169698109E+04 0.12548759060322E+04
 0.11671500767591E+04 0.11143993691548E+04 0.17151346492526E+04 0.11539827151777E+04 0.11668591951416E+04
 0.10417532505990E+04 0.17150169698109E+04 0.16676528709659E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50034079260996E+03 0.12948768167781E+01
 0.12948768167781E+01 0.46240673603644E+01 0.00000000000000E+00 0.34657662350549E+03 0.34657662350549E+03
 0.34657662350549E+03 0.34657662350549E+03 0.00000000000000E+00 0.00000000000000E+00 0.14826136592096E+00
 0.00000000000000E+00 -.14805079207281E+02 0.10000000000000E-02 0.14944852402466E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53530137230930E+01 0.20073801461599E+01 0.35427082203431E+03 0.42859558441950E+03
 0.31607260883902E+03 0.31607260883902E+03 0.29815079621572E+03 0.29815077829965E+03 0.31606662830872E+03
 0.31606662830872E+03 0.29815079634111E+03 0.29815077842222E+03 0.31607260883902E+03 0.31607260883902E+03
 0.29815079621572E+03 0.29815077829965E+03 0.31606662830872E+03 0.31606662830872E+03 0.29815079634111E+03
 0.29815077842222E+03 0.31354333746307E+03 0.29815272501620E+03 -.84961562347047E+02 -.10959424270437E+03
 0.23742310761819E+03 0.52788324953967E+03 0.28927302638339E+03 0.24779561149746E+03 0.21744080455732E+03
 0.24779561149746E+03 0.41548036995646E+03 0.24782815994516E+03 0.21722476229105E+03 0.24782815994516E+03
 0.41531122416636E+03 0.24779561149746E+03 0.21744080455732E+03 0.24779561149746E+03 0.41548036995646E+03
 0.24782815994516E+03 0.21722476229106E+03 0.24782815994516E+03 0.41531122416637E+03 0.22501317513260E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37271720617772E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19016296528080E+00 0.00000000000000E+00 0.00000000000000E+00 0.19016296528080E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19748168147371E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19748168147371E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639197849168E+00 0.19945977938370E+00 0.34657662350549E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1212.79210055
 0.11120246792112E+00 0.32418854638226E+03 0.46203623531374E+03 0.45692656757827E+03 0.45486689292746E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14519747888844E+00 0.00000000000000E+00 -.23389980260040E+02
 0.31851971472926E-02 0.11631238386005E+01 0.25116184744796E+04 0.94185692792987E+03 0.68780294363374E+01
 0.25792610386265E+01 0.35960876437042E+03 0.29816466471045E+03 0.35094810825241E+03 0.38367888636761E+03
 0.29815307145150E+03 0.29815567435606E+03 0.34661469586981E+03 0.38365873713617E+03 0.29815261743371E+03
 0.29815566473725E+03 0.35094810825241E+03 0.38367888636761E+03 0.29815307145150E+03 0.29815567435606E+03
 0.34661469586981E+03 0.38365873713617E+03 0.29815261743371E+03 0.29815566473725E+03 0.42908133296672E+03
 0.35474144193589E+03 0.20855299379605E+04 0.18908962973872E+04 0.53505253506696E+03 0.82679078647538E+03
 0.28906298873309E+03 0.12570562253041E+04 0.11683649681919E+04 0.11156113928345E+04 0.17150124111156E+04
 0.11563475765242E+04 0.11680746839655E+04 0.10432584327744E+04 0.17148947404127E+04 0.12570562253041E+04
 0.11683649681919E+04 0.11156113928345E+04 0.17150124111156E+04 0.11563475765242E+04 0.11680746839655E+04
 0.10432584327744E+04 0.17148947404127E+04 0.16672505119304E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50065895806338E+03 0.12948768263612E+01
 0.12948768263612E+01 0.46711876517402E+01 0.00000000000000E+00 0.34679958694682E+03 0.34679958694682E+03
 0.34679958694682E+03 0.34679958694682E+03 0.00000000000000E+00 0.00000000000000E+00 0.14807544278570E+00
 0.00000000000000E+00 -.14788501732306E+02 0.10000000000000E-02 0.14969282556391E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53442775028549E+01 0.20041040635706E+01 0.35474488698976E+03 0.42907782346933E+03
 0.31622242973140E+03 0.31622242973140E+03 0.29815086072101E+03 0.29815084135347E+03 0.31621637692500E+03
 0.31621637692500E+03 0.29815086085316E+03 0.29815084148265E+03 0.31622242973140E+03 0.31622242973140E+03
 0.29815086072101E+03 0.29815084135347E+03 0.31621637692500E+03 0.31621637692500E+03 0.29815086085316E+03
 0.29815084148265E+03 0.31367505417002E+03 0.29815291881070E+03 -.87379513720443E+02 -.11307495975065E+03
 0.23857549630347E+03 0.52993003431601E+03 0.29016166053102E+03 0.24981052207500E+03 0.21842546466036E+03
 0.24981052207500E+03 0.41698705807254E+03 0.24984354308954E+03 0.21820800759891E+03 0.24984354308954E+03
 0.41681707739184E+03 0.24981052207500E+03 0.21842546466036E+03 0.24981052207500E+03 0.41698705807254E+03
 0.24984354308954E+03 0.21820800759891E+03 0.24984354308954E+03 0.41681707739184E+03 0.22512093763576E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37296160693278E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19005721336461E+00 0.00000000000000E+00 0.00000000000000E+00 0.19005721336461E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19736194308832E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19736194308832E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639204590972E+00 0.19933164430343E+00 0.34679958694682E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1221.62715518
 0.11112673405886E+00 0.32432911608003E+03 0.46227547949579E+03 0.45716563654856E+03 0.45510524345099E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14512557827797E+00 0.00000000000000E+00 -.23387730881706E+02
 0.31873676951321E-02 0.11635094402226E+01 0.25099081013521E+04 0.94121553800704E+03 0.68757499711130E+01
 0.25784062391674E+01 0.35988972643864E+03 0.29816540911932E+03 0.35119674058634E+03 0.38401853257199E+03
 0.29815324958610E+03 0.29815600090093E+03 0.34685560746465E+03 0.38399842125578E+03 0.29815277072050E+03
 0.29815599081478E+03 0.35119674058634E+03 0.38401853257199E+03 0.29815324958610E+03 0.29815600090093E+03
 0.34685560746465E+03 0.38399842125578E+03 0.29815277072050E+03 0.29815599081478E+03 0.42944047435700E+03
 0.35509363029919E+03 0.20876562475515E+04 0.18920657958636E+04 0.53405577580441E+03 0.82449665199316E+03
 0.28777059730973E+03 0.12586870434852E+04 0.11692697836892E+04 0.11165266897439E+04 0.17149214240579E+04
 0.11581164346528E+04 0.11689799377405E+04 0.10443922523543E+04 0.17148037587840E+04 0.12586870434852E+04
 0.11692697836892E+04 0.11165266897439E+04 0.17149214240579E+04 0.11581164346528E+04 0.11689799377405E+04
 0.10443922523543E+04 0.17148037587840E+04 0.16669598326595E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50089717277935E+03 0.12948768097869E+01
 0.12948768097869E+01 0.47065278702721E+01 0.00000000000000E+00 0.34696579592926E+03 0.34696579592926E+03
 0.34696579592926E+03 0.34696579592926E+03 0.00000000000000E+00 0.00000000000000E+00 0.14793962335260E+00
 0.00000000000000E+00 -.14772358797251E+02 0.10000000000000E-02 0.14986965931825E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53379716991360E+01 0.20017393871760E+01 0.35509709004349E+03 0.42943696565645E+03
 0.31633469898761E+03 0.31633469898761E+03 0.29815091178590E+03 0.29815089126932E+03 0.31632859201612E+03
 0.31632859201612E+03 0.29815091192319E+03 0.29815089140352E+03 0.31633469898761E+03 0.31633469898761E+03
 0.29815091178590E+03 0.29815089126932E+03 0.31632859201612E+03 0.31632859201612E+03 0.29815091192319E+03
 0.29815089140352E+03 0.31377379005402E+03 0.29815307099673E+03 -.89181165860167E+02 -.11567033426693E+03
 0.23943198662984E+03 0.53144593889641E+03 0.29081679233342E+03 0.25130870742173E+03 0.21915621875998E+03
 0.25130870742173E+03 0.41810084690606E+03 0.25134208377319E+03 0.21893770486115E+03 0.25134208377319E+03
 0.41793024357146E+03 0.25130870742173E+03 0.21915621875998E+03 0.25130870742173E+03 0.41810084690607E+03
 0.25134208377319E+03 0.21893770486115E+03 0.25134208377319E+03 0.41793024357146E+03 0.22519592678274E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37314291493328E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18995299026224E+00 0.00000000000000E+00 0.00000000000000E+00 0.18995299026224E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19725421435841E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19725421435841E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639222125977E+00 0.19923636698848E+00 0.34696579592926E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1230.11285174
 0.11104551548009E+00 0.32446579165105E+03 0.46250489197685E+03 0.45739535095949E+03 0.45533448969840E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14505761560207E+00 0.00000000000000E+00 -.23388184650135E+02
 0.31896987471075E-02 0.11638653160190E+01 0.25080738446709E+04 0.94052769175160E+03 0.68736475689163E+01
 0.25776178383436E+01 0.36015906925617E+03 0.29816614953703E+03 0.35143522125607E+03 0.38434329538736E+03
 0.29815342790653E+03 0.29815632761191E+03 0.34708684574595E+03 0.38432321977525E+03 0.29815292424448E+03
 0.29815631706237E+03 0.35143522125607E+03 0.38434329538736E+03 0.29815342790653E+03 0.29815632761191E+03
 0.34708684574595E+03 0.38432321977525E+03 0.29815292424448E+03 0.29815631706237E+03 0.42978127730603E+03
 0.35542672405514E+03 0.20897000274637E+04 0.18932063918421E+04 0.53314920100882E+03 0.82238477550419E+03
 0.28656982849033E+03 0.12602527934784E+04 0.11701429343829E+04 0.11174161797808E+04 0.17148482665592E+04
 0.11598138462325E+04 0.11698535156416E+04 0.10454892874830E+04 0.17147306199635E+04 0.12602527934784E+04
 0.11701429343829E+04 0.11174161797808E+04 0.17148482665592E+04 0.11598138462325E+04 0.11698535156416E+04
 0.10454892874830E+04 0.17147306199635E+04 0.16667162465311E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50112617210374E+03 0.12948768131304E+01
 0.12948768131304E+01 0.47404706565284E+01 0.00000000000000E+00 0.34712460220635E+03 0.34712460220635E+03
 0.34712460220635E+03 0.34712460220635E+03 0.00000000000000E+00 0.00000000000000E+00 0.14781202130468E+00
 0.00000000000000E+00 -.14760168832270E+02 0.10000000000000E-02 0.15003448841115E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53321073605936E+01 0.19995402602226E+01 0.35543019727382E+03 0.42977777046838E+03
 0.31644286649433E+03 0.31644286649433E+03 0.29815094365578E+03 0.29815094129619E+03 0.31643670740301E+03
 0.31643670740301E+03 0.29815094379520E+03 0.29815094143526E+03 0.31644286649433E+03 0.31644286649433E+03
 0.29815094365578E+03 0.29815094129619E+03 0.31643670740301E+03 0.31643670740301E+03 0.29815094379520E+03
 0.29815094143526E+03 0.31386897748673E+03 0.29815322255616E+03 -.90878723733070E+02 -.11811652138226E+03
 0.24024501477658E+03 0.53288228709489E+03 0.29143604724443E+03 0.25272445208883E+03 0.21984881936732E+03
 0.25272445208883E+03 0.41915453966658E+03 0.25275817176963E+03 0.21962932332214E+03 0.25275817176963E+03
 0.41898337213726E+03 0.25272445208883E+03 0.21984881936732E+03 0.25272445208883E+03 0.41915453966658E+03
 0.25275817176963E+03 0.21962932332215E+03 0.25275817176963E+03 0.41898337213727E+03 0.22526960556749E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37331755769487E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18987311823594E+00 0.00000000000000E+00 0.00000000000000E+00 0.18987311823594E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19717551105378E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19717551105378E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639228258373E+00 0.19914530435722E+00 0.34712460220635E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1240.88605236
 0.11095347088426E+00 0.32463506704927E+03 0.46279498783466E+03 0.45768521358525E+03 0.45562344907069E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14497283923668E+00 0.00000000000000E+00 -.23393648860689E+02
 0.31923446189275E-02 0.11642988326382E+01 0.25059951086006E+04 0.93974816572524E+03 0.68710882255825E+01
 0.25766580845934E+01 0.36049967271933E+03 0.29816712714794E+03 0.35173683181223E+03 0.38475416454035E+03
 0.29815366505436E+03 0.29815676183965E+03 0.34737927051391E+03 0.38473413342775E+03 0.29815312853105E+03
 0.29815675068046E+03 0.35173683181223E+03 0.38475416454035E+03 0.29815366505436E+03 0.29815676183965E+03
 0.34737927051391E+03 0.38473413342775E+03 0.29815312853105E+03 0.29815675068046E+03 0.43021427244491E+03
 0.35584914101529E+03 0.20922772176033E+04 0.18946133575740E+04 0.53195857042060E+03 0.81965035855272E+03
 0.28503199528002E+03 0.12622316745468E+04 0.11712394265892E+04 0.11185166705742E+04 0.17147477854419E+04
 0.11619596395934E+04 0.11709505175063E+04 0.10468536153926E+04 0.17146301380066E+04 0.12622316745468E+04
 0.11712394265892E+04 0.11185166705742E+04 0.17147477854419E+04 0.11619596395934E+04 0.11709505175063E+04
 0.10468536153926E+04 0.17146301380066E+04 0.16663778885171E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50141498339680E+03 0.12948768533929E+01
 0.12948768533929E+01 0.47835634589999E+01 0.00000000000000E+00 0.34732600765673E+03 0.34732600765673E+03
 0.34732600765673E+03 0.34732600765673E+03 0.00000000000000E+00 0.00000000000000E+00 0.14765387467799E+00
 0.00000000000000E+00 -.14749618165948E+02 0.10000000000000E-02 0.15023657384783E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53249350641496E+01 0.19968506490561E+01 0.35585263918456E+03 0.43021076264642E+03
 0.31657958805993E+03 0.31657958805993E+03 0.29815102398464E+03 0.29815100791502E+03 0.31657336307110E+03
 0.31657336307110E+03 0.29815102413226E+03 0.29815100806033E+03 0.31657958805993E+03 0.31657958805993E+03
 0.29815102398464E+03 0.29815100791502E+03 0.31657336307110E+03 0.31657336307110E+03 0.29815102413226E+03
 0.29815100806033E+03 0.31398929294760E+03 0.29815342294965E+03 -.93043940509460E+02 -.12123787395382E+03
 0.24127958630770E+03 0.53471141527953E+03 0.29222543104029E+03 0.25452732265175E+03 0.22073015409262E+03
 0.25452732265175E+03 0.42049716656620E+03 0.25456147712226E+03 0.22050937380293E+03 0.25456147712226E+03
 0.42032524356116E+03 0.25452732265175E+03 0.22073015409262E+03 0.25452732265175E+03 0.42049716656620E+03
 0.25456147712226E+03 0.22050937380293E+03 0.25456147712226E+03 0.42032524356116E+03 0.22536216912743E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37353874812825E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18981060060450E+00 0.00000000000000E+00 0.00000000000000E+00 0.18981060060450E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19707303103249E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19707303103249E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639219908446E+00 0.19902976121292E+00 0.34732600765673E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1250.83054524
 0.11086828681584E+00 0.32479109001376E+03 0.46306135447508E+03 0.45795142536228E+03 0.45588886770160E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14489611642864E+00 0.00000000000000E+00 -.23395633694070E+02
 0.31947971972915E-02 0.11646811306297E+01 0.25040713090591E+04 0.93902674089715E+03 0.68688328415473E+01
 0.25758123155802E+01 0.36081293192038E+03 0.29816806919007E+03 0.35201433424973E+03 0.38513163009357E+03
 0.29815389538722E+03 0.29815718330287E+03 0.34764841852149E+03 0.38511163912589E+03 0.29815332707018E+03
 0.29815717155858E+03 0.35201433424973E+03 0.38513163009357E+03 0.29815389538722E+03 0.29815718330287E+03
 0.34764841852149E+03 0.38511163912589E+03 0.29815332707018E+03 0.29815717155858E+03 0.43061090460466E+03
 0.35623521815919E+03 0.20946389784825E+04 0.18959005645547E+04 0.53086999552700E+03 0.81715597323197E+03
 0.28363162772733E+03 0.12640480243182E+04 0.11722383636133E+04 0.11195257123062E+04 0.17146470641594E+04
 0.11639292954735E+04 0.11719499172743E+04 0.10481044434796E+04 0.17145294173064E+04 0.12640480243182E+04
 0.11722383636133E+04 0.11195257123062E+04 0.17146470641594E+04 0.11639292954735E+04 0.11719499172743E+04
 0.10481044434796E+04 0.17145294173064E+04 0.16660651305412E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50168024508739E+03 0.12948768680179E+01
 0.12948768680179E+01 0.48233414305120E+01 0.00000000000000E+00 0.34751108009713E+03 0.34751108009713E+03
 0.34751108009713E+03 0.34751108009713E+03 0.00000000000000E+00 0.00000000000000E+00 0.14751161759663E+00
 0.00000000000000E+00 -.14736756822156E+02 0.10000000000000E-02 0.15041647057194E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53185664904787E+01 0.19944624339295E+01 0.35623872020084E+03 0.43060740754489E+03
 0.31670571451208E+03 0.31670571451208E+03 0.29815108981644E+03 0.29815107271371E+03 0.31669942870156E+03
 0.31669942870156E+03 0.29815108996996E+03 0.29815107286482E+03 0.31670571451208E+03 0.31670571451208E+03
 0.29815108981644E+03 0.29815107271371E+03 0.31669942870156E+03 0.31669942870156E+03 0.29815108996996E+03
 0.29815107286482E+03 0.31410032286474E+03 0.29815361635185E+03 -.95024911336723E+02 -.12409468880576E+03
 0.24222805087830E+03 0.53638454135645E+03 0.29294535022376E+03 0.25617734107211E+03 0.22153730069183E+03
 0.25617734107211E+03 0.42172388064191E+03 0.25621189817731E+03 0.22131534414180E+03 0.25621189817731E+03
 0.42155126926721E+03 0.25617734107211E+03 0.22153730069183E+03 0.25617734107211E+03 0.42172388064191E+03
 0.25621189817731E+03 0.22131534414180E+03 0.25621189817731E+03 0.42155126926721E+03 0.22544601543977E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37374168487150E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18972965896760E+00 0.00000000000000E+00 0.00000000000000E+00 0.18972965896760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19697363039719E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19697363039719E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639222710167E+00 0.19892381845290E+00 0.34751108009713E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1260.77503812
 0.11078687408048E+00 0.32494608980832E+03 0.46332649942325E+03 0.45821625508484E+03 0.45615284033674E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14482082402799E+00 0.00000000000000E+00 -.23397855808813E+02
 0.31971447101619E-02 0.11650464834510E+01 0.25022326873640E+04 0.93833725776151E+03 0.68666788095038E+01
 0.25750045535639E+01 0.36112508558554E+03 0.29816905010275E+03 0.35229095201789E+03 0.38550741802492E+03
 0.29815413701935E+03 0.29815762515364E+03 0.34791678595171E+03 0.38548746634953E+03 0.29815353547104E+03
 0.29815761280248E+03 0.35229095201789E+03 0.38550741802492E+03 0.29815413701935E+03 0.29815762515364E+03
 0.34791678595171E+03 0.38548746634953E+03 0.29815353547104E+03 0.29815761280248E+03 0.43100502079730E+03
 0.35661802591008E+03 0.20969840815849E+04 0.18971704682888E+04 0.52978742084757E+03 0.81468290472056E+03
 0.28224654676875E+03 0.12658548555829E+04 0.11732266965758E+04 0.11205241260638E+04 0.17145415136262E+04
 0.11658886736812E+04 0.11729387015366E+04 0.10493431341102E+04 0.17144238642874E+04 0.12658548555829E+04
 0.11732266965758E+04 0.11205241260638E+04 0.17145415136262E+04 0.11658886736812E+04 0.11729387015366E+04
 0.10493431341102E+04 0.17144238642874E+04 0.16657497851059E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50194409510797E+03 0.12948768843914E+01
 0.12948768843914E+01 0.48631194020241E+01 0.00000000000000E+00 0.34769564090936E+03 0.34769564090936E+03
 0.34769564090936E+03 0.34769564090936E+03 0.00000000000000E+00 0.00000000000000E+00 0.14737281681821E+00
 0.00000000000000E+00 -.14724298204475E+02 0.10000000000000E-02 0.15059008605338E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53124347091245E+01 0.19921630159217E+01 0.35662153339256E+03 0.43100153412988E+03
 0.31683171002193E+03 0.31683171002193E+03 0.29815115897291E+03 0.29815114078488E+03 0.31682536343988E+03
 0.31682536343988E+03 0.29815115913235E+03 0.29815114094182E+03 0.31683171002193E+03 0.31683171002193E+03
 0.29815115897291E+03 0.29815114078488E+03 0.31682536343988E+03 0.31682536343988E+03 0.29815115913235E+03
 0.29815114094182E+03 0.31421126892617E+03 0.29815381803038E+03 -.96990548192724E+02 -.12692993783482E+03
 0.24317300467260E+03 0.53805000273883E+03 0.29366113304287E+03 0.25781699711636E+03 0.22234098685483E+03
 0.25781699711636E+03 0.42294447767812E+03 0.25785195771129E+03 0.22211785620271E+03 0.25785195771129E+03
 0.42277117979533E+03 0.25781699711636E+03 0.22234098685483E+03 0.25781699711636E+03 0.42294447767812E+03
 0.25785195771129E+03 0.22211785620271E+03 0.25785195771129E+03 0.42277117979533E+03 0.22553136313446E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37394411172322E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18965117286595E+00 0.00000000000000E+00 0.00000000000000E+00 0.18965117286595E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19687726961526E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19687726961526E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639224336534E+00 0.19881826834368E+00 0.34769564090936E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1270.71953100
 0.11070588541537E+00 0.32510094170064E+03 0.46359043176324E+03 0.45847989789054E+03 0.45641565202406E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14474694391502E+00 0.00000000000000E+00 -.23400057995380E+02
 0.31994834209045E-02 0.11653951505356E+01 0.25004036425788E+04 0.93765136596705E+03 0.68646244120056E+01
 0.25742341545021E+01 0.36143615124895E+03 0.29817007089189E+03 0.35256670421656E+03 0.38588154268683E+03
 0.29815439033380E+03 0.29815808806397E+03 0.34818439047730E+03 0.38586162947456E+03 0.29815375407428E+03
 0.29815807508375E+03 0.35256670421656E+03 0.38588154268683E+03 0.29815439033380E+03 0.29815808806397E+03
 0.34818439047730E+03 0.38586162947456E+03 0.29815375407428E+03 0.29815807508375E+03 0.43139661651956E+03
 0.35699758757577E+03 0.20993148723844E+04 0.18984320143361E+04 0.52871141170670E+03 0.81223193418623E+03
 0.28087696542099E+03 0.12676530690595E+04 0.11742045739605E+04 0.11215179554931E+04 0.17144311459648E+04
 0.11678387973820E+04 0.11739170203543E+04 0.10505757086064E+04 0.17143134924056E+04 0.12676530690595E+04
 0.11742045739605E+04 0.11215179554931E+04 0.17144311459648E+04 0.11678387973820E+04 0.11739170203543E+04
 0.10505757086064E+04 0.17143134924056E+04 0.16654347464798E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50220677110325E+03 0.12948769006180E+01
 0.12948769006180E+01 0.49028973735363E+01 0.00000000000000E+00 0.34787951895378E+03 0.34787951895378E+03
 0.34787951895378E+03 0.34787951895378E+03 0.00000000000000E+00 0.00000000000000E+00 0.14723736977676E+00
 0.00000000000000E+00 -.14711902332886E+02 0.10000000000000E-02 0.15075769305911E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53065285344099E+01 0.19899482004037E+01 0.35700110068552E+03 0.43139313962756E+03
 0.31695757168515E+03 0.31695757168515E+03 0.29815123157187E+03 0.29815121224453E+03 0.31695116437873E+03
 0.31695116437873E+03 0.29815123173725E+03 0.29815121240731E+03 0.31695757168515E+03 0.31695757168515E+03
 0.29815123157187E+03 0.29815121224453E+03 0.31695116437873E+03 0.31695116437873E+03 0.29815123173725E+03
 0.29815121240731E+03 0.31432212928907E+03 0.29815402821391E+03 -.98942092906377E+02 -.12974538027273E+03
 0.24411313253786E+03 0.53970455284010E+03 0.29437085463955E+03 0.25944556510783E+03 0.22313991067724E+03
 0.25944556510783E+03 0.42415617754418E+03 0.25948093008189E+03 0.22291560862607E+03 0.25948093008189E+03
 0.42398219548573E+03 0.25944556510783E+03 0.22313991067724E+03 0.25944556510783E+03 0.42415617754418E+03
 0.25948093008189E+03 0.22291560862607E+03 0.25948093008189E+03 0.42398219548573E+03 0.22561617683241E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37414575588010E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18957302955902E+00 0.00000000000000E+00 0.00000000000000E+00 0.18957302955902E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19678151973655E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19678151973655E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639225943594E+00 0.19871321991589E+00 0.34787951895378E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1280.25242251
 0.11062879996974E+00 0.32524875175815E+03 0.46384227059629E+03 0.45873145903874E+03 0.45666642130756E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14467740650211E+00 0.00000000000000E+00 -.23402275598944E+02
 0.32017125987132E-02 0.11657143880747E+01 0.24986627479354E+04 0.93699853047578E+03 0.68627444954274E+01
 0.25735291857853E+01 0.36173328691990E+03 0.29817108830588E+03 0.35283019090422E+03 0.38623865065904E+03
 0.29815464464051E+03 0.29815855248497E+03 0.34844014956085E+03 0.38621877356069E+03 0.29815397365870E+03
 0.29815853888025E+03 0.35283019090422E+03 0.38623865065904E+03 0.29815464464051E+03 0.29815855248497E+03
 0.34844014956085E+03 0.38621877356069E+03 0.29815397365870E+03 0.29815853888025E+03 0.43176993294915E+03
 0.35735876433775E+03 0.21015355288756E+04 0.18996297412197E+04 0.52768094764518E+03 0.80989458731155E+03
 0.27957523492814E+03 0.12693686626645E+04 0.11751313939541E+04 0.11224631440776E+04 0.17143195283278E+04
 0.11696995095456E+04 0.11748442533006E+04 0.10517486535676E+04 0.17142018679157E+04 0.12693686626645E+04
 0.11751313939541E+04 0.11224631440776E+04 0.17143195283278E+04 0.11696995095456E+04 0.11748442533006E+04
 0.10517486535675E+04 0.17142018679157E+04 0.16651292416288E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50245740911665E+03 0.12948769169582E+01
 0.12948769169582E+01 0.49410289396050E+01 0.00000000000000E+00 0.34805513188648E+03 0.34805513188648E+03
 0.34805513188648E+03 0.34805513188648E+03 0.00000000000000E+00 0.00000000000000E+00 0.14711058214732E+00
 0.00000000000000E+00 -.14700207182681E+02 0.10000000000000E-02 0.15091291125799E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53010706196791E+01 0.19879014823797E+01 0.35736228088217E+03 0.43176646688877E+03
 0.31707802848678E+03 0.31707802848678E+03 0.29815130653978E+03 0.29815128408031E+03 0.31707156302952E+03
 0.31707156302952E+03 0.29815130671109E+03 0.29815128424868E+03 0.31707802848678E+03 0.31707802848678E+03
 0.29815130653978E+03 0.29815128408031E+03 0.31707156302952E+03 0.31707156302952E+03 0.29815130671109E+03
 0.29815128424868E+03 0.31442825445941E+03 0.29815423800349E+03 -.10080298236687E+03 -.13243047781992E+03
 0.24501025012428E+03 0.54128137413334E+03 0.29504607275844E+03 0.26099813744875E+03 0.22390170701550E+03
 0.26099813744875E+03 0.42531022634208E+03 0.26103389072306E+03 0.22367628089363E+03 0.26103389072306E+03
 0.42513558681582E+03 0.26099813744875E+03 0.22390170701550E+03 0.26099813744875E+03 0.42531022634208E+03
 0.26103389072306E+03 0.22367628089363E+03 0.26103389072306E+03 0.42513558681582E+03 0.22569632627469E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37433830235789E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18949944565880E+00 0.00000000000000E+00 0.00000000000000E+00 0.18949944565880E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19669031180229E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19669031180229E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639227044249E+00 0.19861299224771E+00 0.34805513188648E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1291.69189233
 0.11053621691088E+00 0.32542594875821E+03 0.46414311765219E+03 0.45903202729548E+03 0.45696607807104E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14459556331463E+00 0.00000000000000E+00 -.23404635669446E+02
 0.32043940517725E-02 0.11660785536895E+01 0.24965718543807E+04 0.93621444539275E+03 0.68606012645441E+01
 0.25727254742040E+01 0.36208859257342E+03 0.29817236009246E+03 0.35314538473836E+03 0.38666517675357E+03
 0.29815496495284E+03 0.29815913703956E+03 0.34874620329943E+03 0.38664534203666E+03 0.29815425040235E+03
 0.29815912265753E+03 0.35314538473836E+03 0.38666517675357E+03 0.29815496495284E+03 0.29815913703956E+03
 0.34874620329943E+03 0.38664534203666E+03 0.29815425040235E+03 0.29815912265753E+03 0.43221460459346E+03
 0.35778798808182E+03 0.21041848282157E+04 0.19010583750311E+04 0.52645922900866E+03 0.80712705353653E+03
 0.27803552838282E+03 0.12714179055233E+04 0.11762330565055E+04 0.11235923153468E+04 0.17141826155121E+04
 0.11719220601583E+04 0.11759464026350E+04 0.10531491973971E+04 0.17140649475335E+04 0.12714179055233E+04
 0.11762330565055E+04 0.11235923153468E+04 0.17141826155121E+04 0.11719220601583E+04 0.11759464026350E+04
 0.10531491973971E+04 0.17140649475335E+04 0.16647676276083E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50275688881480E+03 0.12948769343481E+01
 0.12948769343481E+01 0.49867868188875E+01 0.00000000000000E+00 0.34826503286871E+03 0.34826503286871E+03
 0.34826503286871E+03 0.34826503286871E+03 0.00000000000000E+00 0.00000000000000E+00 0.14696225758151E+00
 0.00000000000000E+00 -.14685957661417E+02 0.10000000000000E-02 0.15109236496282E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52947744923898E+01 0.19855404346462E+01 0.35779150937440E+03 0.43221115076288E+03
 0.31722246255141E+03 0.31722246255141E+03 0.29815139873348E+03 0.29815137468920E+03 0.31721592734963E+03
 0.31721592734963E+03 0.29815139891160E+03 0.29815137486425E+03 0.31722246255141E+03 0.31722246255141E+03
 0.29815139873348E+03 0.29815137468920E+03 0.31721592734963E+03 0.31721592734963E+03 0.29815139891160E+03
 0.29815137486425E+03 0.31455554670892E+03 0.29815450064277E+03 -.10301591851499E+03 -.13562366798115E+03
 0.24608054080869E+03 0.54315935516287E+03 0.29584841165013E+03 0.26284617670173E+03 0.22480973949370E+03
 0.26284617670173E+03 0.42668349998844E+03 0.26288239720326E+03 0.22458297193096E+03 0.26288239720326E+03
 0.42650807862211E+03 0.26284617670173E+03 0.22480973949370E+03 0.26284617670173E+03 0.42668349998843E+03
 0.26288239720326E+03 0.22458297193096E+03 0.26288239720326E+03 0.42650807862211E+03 0.22579256435618E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37456840908294E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18940934803833E+00 0.00000000000000E+00 0.00000000000000E+00 0.18940934803833E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19658132396451E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19658132396451E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639229301776E+00 0.19849333800301E+00 0.34826503286871E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1301.51941552
 0.11045782784241E+00 0.32557666528905E+03 0.46440018304897E+03 0.45928880164057E+03 0.45722203891767E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14452659946417E+00 0.00000000000000E+00 -.23408516903540E+02
 0.32066679215783E-02 0.11663761069451E+01 0.24948015184754E+04 0.93555056942827E+03 0.68588510621613E+01
 0.25720691483105E+01 0.36239249239535E+03 0.29817350014431E+03 0.35341502912333E+03 0.38702996874589E+03
 0.29815525437303E+03 0.29815966482695E+03 0.34900803523101E+03 0.38701016963783E+03 0.29815450061250E+03
 0.29815964975130E+03 0.35341502912333E+03 0.38702996874589E+03 0.29815525437303E+03 0.29815966482695E+03
 0.34900803523101E+03 0.38701016963783E+03 0.29815450061250E+03 0.29815964975130E+03 0.43259556062317E+03
 0.35815522141419E+03 0.21064456331697E+04 0.19022678104199E+04 0.52538899474288E+03 0.80472744336693E+03
 0.27671150365034E+03 0.12731693896693E+04 0.11771656164126E+04 0.11245499338156E+04 0.17140550835876E+04
 0.11738222400011E+04 0.11768793632918E+04 0.10543396197455E+04 0.17139373994333E+04 0.12731693896693E+04
 0.11771656164126E+04 0.11245499338156E+04 0.17140550835876E+04 0.11738222400011E+04 0.11768793632918E+04
 0.10543396197455E+04 0.17139373994333E+04 0.16644411928233E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50301271632192E+03 0.12948769629466E+01
 0.12948769629466E+01 0.50260969116514E+01 0.00000000000000E+00 0.34844458689902E+03 0.34844458689902E+03
 0.34844458689902E+03 0.34844458689902E+03 0.00000000000000E+00 0.00000000000000E+00 0.14683805546135E+00
 0.00000000000000E+00 -.14675640503335E+02 0.10000000000000E-02 0.15124082075925E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52895772185306E+01 0.19835914569490E+01 0.35815874209602E+03 0.43259212126847E+03
 0.31734605389678E+03 0.31734605389678E+03 0.29815149758304E+03 0.29815145668077E+03 0.31733945895320E+03
 0.31733945895320E+03 0.29815149776885E+03 0.29815145686151E+03 0.31734605389678E+03 0.31734605389678E+03
 0.29815149758304E+03 0.29815145668077E+03 0.31733945895320E+03 0.31733945895320E+03 0.29815149776885E+03
 0.29815145686151E+03 0.31466448195835E+03 0.29815473645266E+03 -.10492000934870E+03 -.13837184226459E+03
 0.24699717977395E+03 0.54476636631817E+03 0.29653420064534E+03 0.26443168467218E+03 0.22558690607602E+03
 0.26443168467218E+03 0.42785826126983E+03 0.26446830630755E+03 0.22535896745772E+03 0.26446830630755E+03
 0.42768214830882E+03 0.26443168467218E+03 0.22558690607602E+03 0.26443168467218E+03 0.42785826126982E+03
 0.26446830630755E+03 0.22535896745772E+03 0.26446830630755E+03 0.42768214830882E+03 0.22587103059848E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37476515343031E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18934701710651E+00 0.00000000000000E+00 0.00000000000000E+00 0.18934701710651E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19648738833814E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19648738833814E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639225108168E+00 0.19839103192904E+00 0.34844458689902E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1313.84278037
 0.11036981948661E+00 0.32576275608021E+03 0.46472052829563E+03 0.45960828021708E+03 0.45754027412719E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14444192064844E+00 0.00000000000000E+00 -.23415084554191E+02
 0.32092246475666E-02 0.11667292249578E+01 0.24928139593051E+04 0.93480523473942E+03 0.68567751873097E+01
 0.25712906952411E+01 0.36277203474182E+03 0.29817499445479E+03 0.35375192801899E+03 0.38748510909383E+03
 0.29815563689437E+03 0.29816036184272E+03 0.34933528790377E+03 0.38746535356283E+03 0.29815483152839E+03
 0.29816034586236E+03 0.35375192801899E+03 0.38748510909383E+03 0.29815563689437E+03 0.29816036184272E+03
 0.34933528790377E+03 0.38746535356283E+03 0.29815483152839E+03 0.29816034586236E+03 0.43306910478402E+03
 0.35861090069299E+03 0.21092481130329E+04 0.19037396447388E+04 0.52406191712342E+03 0.80175791137885E+03
 0.27507568466981E+03 0.12753476448929E+04 0.11783143031301E+04 0.11257221770327E+04 0.17138797093456E+04
 0.11761856417325E+04 0.11780285439242E+04 0.10558016661164E+04 0.17137620067467E+04 0.12753476448929E+04
 0.11783143031301E+04 0.11257221770327E+04 0.17138797093456E+04 0.11761856417325E+04 0.11780285439242E+04
 0.10558016661164E+04 0.17137620067467E+04 0.16640196924494E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50333090391339E+03 0.12948770113397E+01
 0.12948770113397E+01 0.50753903710194E+01 0.00000000000000E+00 0.34866927248825E+03 0.34866927248825E+03
 0.34866927248825E+03 0.34866927248825E+03 0.00000000000000E+00 0.00000000000000E+00 0.14668636105100E+00
 0.00000000000000E+00 -.14665412563951E+02 0.10000000000000E-02 0.15141958832035E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52833322879434E+01 0.19812496079788E+01 0.35861441416132E+03 0.43306568784314E+03
 0.31750089422798E+03 0.31750089422798E+03 0.29815160916647E+03 0.29815156521660E+03 0.31749422430506E+03
 0.31749422430506E+03 0.29815160935952E+03 0.29815156540437E+03 0.31750089422798E+03 0.31750089422798E+03
 0.29815160916647E+03 0.29815156521660E+03 0.31749422430506E+03 0.31749422430506E+03 0.29815160935952E+03
 0.29815156540437E+03 0.31480099952740E+03 0.29815504605763E+03 -.10727937464225E+03 -.14177636949252E+03
 0.24814355360094E+03 0.54677708092980E+03 0.29739280956086E+03 0.26640454832191E+03 0.22655858528520E+03
 0.26640454832191E+03 0.42932858070700E+03 0.26644167504670E+03 0.22632919213635E+03 0.26644167504670E+03
 0.42915161561362E+03 0.26640454832191E+03 0.22655858528520E+03 0.26640454832191E+03 0.42932858070700E+03
 0.26644167504670E+03 0.22632919213635E+03 0.26644167504670E+03 0.42915161561362E+03 0.22597750914584E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37501269428530E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18928324715000E+00 0.00000000000000E+00 0.00000000000000E+00 0.18928324715000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19640006926262E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19640006926262E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639211079185E+00 0.19826306537503E+00 0.34866927248825E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1322.05835693
 0.11031319408790E+00 0.32588772086412E+03 0.46493318322750E+03 0.45982033353525E+03 0.45775152345337E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14438661177753E+00 0.00000000000000E+00 -.23416990003786E+02
 0.32108718207681E-02 0.11669510759144E+01 0.24915351488825E+04 0.93432568083093E+03 0.68554716346881E+01
 0.25708018630081E+01 0.36302420548961E+03 0.29817602943343E+03 0.35397586482006E+03 0.38778710176113E+03
 0.29815590374156E+03 0.29816084774460E+03 0.34955289882601E+03 0.38776737464497E+03 0.29815506250689E+03
 0.29816083114034E+03 0.35397586482006E+03 0.38778710176112E+03 0.29815590374156E+03 0.29816084774460E+03
 0.34955289882601E+03 0.38776737464497E+03 0.29815506250689E+03 0.29816083114034E+03 0.43338208625700E+03
 0.35891140796742E+03 0.21111043629174E+04 0.19047213751140E+04 0.52319530482387E+03 0.79981643250940E+03
 0.27400515116141E+03 0.12767926390147E+04 0.11790737784637E+04 0.11265070778788E+04 0.17137624407628E+04
 0.11777532951209E+04 0.11787883433606E+04 0.10567777083762E+04 0.17136447270269E+04 0.12767926390147E+04
 0.11790737784637E+04 0.11265070778788E+04 0.17137624407628E+04 0.11777532951209E+04 0.11787883433606E+04
 0.10567777083762E+04 0.17136447270269E+04 0.16637475572982E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50354210659734E+03 0.12948770253798E+01
 0.12948770253798E+01 0.51082526772647E+01 0.00000000000000E+00 0.34881868405882E+03 0.34881868405882E+03
 0.34881868405882E+03 0.34881868405882E+03 0.00000000000000E+00 0.00000000000000E+00 0.14658766407034E+00
 0.00000000000000E+00 -.14655786832500E+02 0.10000000000000E-02 0.15153438091329E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52793299789688E+01 0.19797487421133E+01 0.35891492396731E+03 0.43337867764051E+03
 0.31760413828328E+03 0.31760413828328E+03 0.29815168711230E+03 0.29815164103355E+03 0.31759741838148E+03
 0.31759741838148E+03 0.29815168731010E+03 0.29815164122595E+03 0.31760413828328E+03 0.31760413828328E+03
 0.29815168711230E+03 0.29815164103355E+03 0.31759741838148E+03 0.31759741838148E+03 0.29815168731010E+03
 0.29815164122595E+03 0.31489205915736E+03 0.29815526080711E+03 -.10883266657996E+03 -.14401701889417E+03
 0.24890439791162E+03 0.54810846554945E+03 0.29795954564827E+03 0.26770785281034E+03 0.22720305857056E+03
 0.26770785281034E+03 0.43030123313167E+03 0.26774531734002E+03 0.22697270499644E+03 0.26774531734002E+03
 0.43012370888960E+03 0.26770785281034E+03 0.22720305857056E+03 0.26770785281034E+03 0.43030123313167E+03
 0.26774531734002E+03 0.22697270499644E+03 0.26774531734002E+03 0.43012370888960E+03 0.22605101422772E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37517651389127E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18922184037341E+00 0.00000000000000E+00 0.00000000000000E+00 0.18922184037341E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19632834158479E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19632834158479E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639211088257E+00 0.19817816111072E+00 0.34881868405882E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1330.27393349
 0.11025277382705E+00 0.32601364747773E+03 0.46514514889285E+03 0.46003193757360E+03 0.45796244722663E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14433217100881E+00 0.00000000000000E+00 -.23416627918957E+02
 0.32126312598809E-02 0.11671626446244E+01 0.24901706273930E+04 0.93381398527237E+03 0.68542289601589E+01
 0.25703358600596E+01 0.36327571078750E+03 0.29817709611106E+03 0.35419928852723E+03 0.38808800364206E+03
 0.29815618033093E+03 0.29816135110537E+03 0.34977006753444E+03 0.38806830445881E+03 0.29815530202620E+03
 0.29816133386035E+03 0.35419928852723E+03 0.38808800364206E+03 0.29815618033093E+03 0.29816135110537E+03
 0.34977006753444E+03 0.38806830445880E+03 0.29815530202620E+03 0.29816133386035E+03 0.43369325040254E+03
 0.35920964502772E+03 0.21129555019383E+04 0.19057097868377E+04 0.52233847201887E+03 0.79789767349985E+03
 0.27294750912088E+03 0.12782338289643E+04 0.11798285917184E+04 0.11272968687944E+04 0.17136449314571E+04
 0.11793168250610E+04 0.11795434772481E+04 0.10577575397212E+04 0.17135272078781E+04 0.12782338289643E+04
 0.11798285917184E+04 0.11272968687944E+04 0.17136449314571E+04 0.11793168250610E+04 0.11795434772481E+04
 0.10577575397213E+04 0.17135272078781E+04 0.16634828816634E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50375291878581E+03 0.12948770227118E+01
 0.12948770227118E+01 0.51411149835100E+01 0.00000000000000E+00 0.34896745062549E+03 0.34896745062549E+03
 0.34896745062549E+03 0.34896745062549E+03 0.00000000000000E+00 0.00000000000000E+00 0.14649086730531E+00
 0.00000000000000E+00 -.14643560210528E+02 0.10000000000000E-02 0.15164590314261E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52754474959185E+01 0.19782928109694E+01 0.35921316381885E+03 0.43368985015711E+03
 0.31770732408105E+03 0.31770732408105E+03 0.29815176799064E+03 0.29815171970292E+03 0.31770055422735E+03
 0.31770055422735E+03 0.29815176819310E+03 0.29815171989985E+03 0.31770732408105E+03 0.31770732408105E+03
 0.29815176799064E+03 0.29815171970292E+03 0.31770055422735E+03 0.31770055422735E+03 0.29815176819310E+03
 0.29815171989985E+03 0.31498309157395E+03 0.29815548239139E+03 -.11037607076553E+03 -.14624325899390E+03
 0.24966032916569E+03 0.54942746727718E+03 0.29851883646566E+03 0.26900196620530E+03 0.22784269533857E+03
 0.26900196620530E+03 0.43126348388866E+03 0.26903976922665E+03 0.22761138602036E+03 0.26903976922665E+03
 0.43108540463516E+03 0.26900196620530E+03 0.22784269533857E+03 0.26900196620530E+03 0.43126348388866E+03
 0.26903976922665E+03 0.22761138602036E+03 0.26903976922665E+03 0.43108540463516E+03 0.22612188564416E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37533898894107E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18914289021936E+00 0.00000000000000E+00 0.00000000000000E+00 0.18914289021936E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19624412324103E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19624412324103E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639219829060E+00 0.19809378885161E+00 0.34896745062549E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1342.59729833
 0.11014600130668E+00 0.32620462245548E+03 0.46546200974088E+03 0.46034912828691E+03 0.45827901442959E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14425190015555E+00 0.00000000000000E+00 -.23411009586137E+02
 0.32157452417270E-02 0.11674634840455E+01 0.24877592590959E+04 0.93290972216098E+03 0.68524627188151E+01
 0.25696735195557E+01 0.36365179183148E+03 0.29817875684504E+03 0.35453351567582E+03 0.38853737335445E+03
 0.29815661400517E+03 0.29816213979005E+03 0.35009504030780E+03 0.38851771521519E+03 0.29815567778750E+03
 0.29816212155184E+03 0.35453351567582E+03 0.38853737335445E+03 0.29815661400517E+03 0.29816213979005E+03
 0.35009504030780E+03 0.38851771521519E+03 0.29815567778750E+03 0.29816212155184E+03 0.43415680391171E+03
 0.35965285852216E+03 0.21157326231517E+04 0.19072166662388E+04 0.52107288335940E+03 0.79506349235189E+03
 0.27138524457569E+03 0.12803931738755E+04 0.11809559850562E+04 0.11284947826235E+04 0.17134729472830E+04
 0.11816594253975E+04 0.11806713503803E+04 0.10592391530971E+04 0.17133552162611E+04 0.12803931738755E+04
 0.11809559850562E+04 0.11284947826235E+04 0.17134729472830E+04 0.11816594253975E+04 0.11806713503803E+04
 0.10592391530971E+04 0.17133552162611E+04 0.16631069288038E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50406911259086E+03 0.12948769813137E+01
 0.12948769813137E+01 0.51904084428780E+01 0.00000000000000E+00 0.34918880370473E+03 0.34918880370473E+03
 0.34918880370473E+03 0.34918880370473E+03 0.00000000000000E+00 0.00000000000000E+00 0.14634915053559E+00
 0.00000000000000E+00 -.14619316746269E+02 0.10000000000000E-02 0.15180747568610E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52698327034579E+01 0.19761872637967E+01 0.35965637852458E+03 0.43415341947309E+03
 0.31786191802900E+03 0.31786191802900E+03 0.29815189497210E+03 0.29815184321622E+03 0.31785507330720E+03
 0.31785507330720E+03 0.29815189518139E+03 0.29815184341979E+03 0.31786191802900E+03 0.31786191802900E+03
 0.29815189497210E+03 0.29815184321622E+03 0.31785507330720E+03 0.31785507330720E+03 0.29815189518139E+03
 0.29815184341979E+03 0.31511952530695E+03 0.29815582788094E+03 -.11267805890119E+03 -.14956404034124E+03
 0.25078073236447E+03 0.55137267127515E+03 0.29933803524885E+03 0.27092414771869E+03 0.22878887646767E+03
 0.27092414771869E+03 0.43267879001129E+03 0.27096245971452E+03 0.22855614231819E+03 0.27096245971452E+03
 0.43249988564294E+03 0.27092414771869E+03 0.22878887646766E+03 0.27092414771869E+03 0.43267879001129E+03
 0.27096245971452E+03 0.22855614231819E+03 0.27096245971452E+03 0.43249988564294E+03 0.22621623990332E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37557939485387E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18898478540435E+00 0.00000000000000E+00 0.00000000000000E+00 0.18898478540435E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19608847332803E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19608847332803E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639252818405E+00 0.19796859582684E+00 0.34918880370473E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1350.81287489
 0.11005816159453E+00 0.32633369538387E+03 0.46567271737123E+03 0.46056091850512E+03 0.45849076646246E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14419907316992E+00 0.00000000000000E+00 -.23403073535472E+02
 0.32183116236248E-02 0.11676558508791E+01 0.24857754424009E+04 0.93216579090034E+03 0.68513338018022E+01
 0.25692501756758E+01 0.36390178581351E+03 0.29817990533604E+03 0.35475577054911E+03 0.38883567542736E+03
 0.29815691600096E+03 0.29816268861991E+03 0.35031121028626E+03 0.38881604411979E+03 0.29815593959903E+03
 0.29816266969789E+03 0.35475577054911E+03 0.38883567542736E+03 0.29815691600096E+03 0.29816268861991E+03
 0.35031121028626E+03 0.38881604411979E+03 0.29815593959903E+03 0.29816266969789E+03 0.43446382956853E+03
 0.35994557761342E+03 0.21175919811863E+04 0.19082475641556E+04 0.52024476461884E+03 0.79320667683633E+03
 0.27036068839440E+03 0.12818348537516E+04 0.11817081436390E+04 0.11293067334540E+04 0.17133658964806E+04
 0.11832233081735E+04 0.11814238328323E+04 0.10602396951634E+04 0.17132481698142E+04 0.12818348537516E+04
 0.11817081436390E+04 0.11293067334540E+04 0.17133658964806E+04 0.11832233081735E+04 0.11814238328323E+04
 0.10602396951634E+04 0.17132481698142E+04 0.16628771310190E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50428042020109E+03 0.12948769228377E+01
 0.12948769228377E+01 0.52232707491233E+01 0.00000000000000E+00 0.34933477672151E+03 0.34933477672151E+03
 0.34933477672151E+03 0.34933477672151E+03 0.00000000000000E+00 0.00000000000000E+00 0.14625693279645E+00
 0.00000000000000E+00 -.14598223443070E+02 0.10000000000000E-02 0.15191167184751E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52662181270906E+01 0.19748317976590E+01 0.35994909666043E+03 0.43446045775197E+03
 0.31796480655757E+03 0.31796480655757E+03 0.29815198351411E+03 0.29815192933994E+03 0.31795791196969E+03
 0.31795791196969E+03 0.29815198372781E+03 0.29815192954780E+03 0.31796480655757E+03 0.31796480655757E+03
 0.29815198351411E+03 0.29815192933994E+03 0.31795791196969E+03 0.31795791196969E+03 0.29815198372781E+03
 0.29815192954780E+03 0.31521036265617E+03 0.29815606714900E+03 -.11420700587042E+03 -.15177030990166E+03
 0.25151571682593E+03 0.55264022386124E+03 0.29986692845118E+03 0.27219144942538E+03 0.22940792996009E+03
 0.27219144942538E+03 0.43359762276710E+03 0.27223010153478E+03 0.22917425243191E+03 0.27223010153478E+03
 0.43341817362618E+03 0.27219144942538E+03 0.22940792996009E+03 0.27219144942538E+03 0.43359762276709E+03
 0.27223010153479E+03 0.22917425243191E+03 0.27223010153479E+03 0.43341817362618E+03 0.22626653228352E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37573684658205E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18884634586149E+00 0.00000000000000E+00 0.00000000000000E+00 0.18884634586149E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19595944579039E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19595944579039E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639291425414E+00 0.19788630289759E+00 0.34933477672151E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1360.96913243
 0.10997235004522E+00 0.32648977082998E+03 0.46593328078945E+03 0.46082163729318E+03 0.45875090845464E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14413449760167E+00 0.00000000000000E+00 -.23417300556378E+02
 0.32208226665362E-02 0.11678830009380E+01 0.24838374627447E+04 0.93143904852927E+03 0.68500012360613E+01
 0.25687504635230E+01 0.36421057084970E+03 0.29818136310872E+03 0.35503046020955E+03 0.38920286192109E+03
 0.29815730123960E+03 0.29816338837501E+03 0.35057863393785E+03 0.38918326320252E+03 0.29815627370962E+03
 0.29816336858788E+03 0.35503046020955E+03 0.38920286192109E+03 0.29815730123960E+03 0.29816338837501E+03
 0.35057863393785E+03 0.38918326320252E+03 0.29815627370962E+03 0.29816336858788E+03 0.43483789800737E+03
 0.36030021727211E+03 0.21198760710477E+04 0.19094817631831E+04 0.51930464045109E+03 0.79105230984327E+03
 0.26915114618993E+03 0.12836114097020E+04 0.11826517932917E+04 0.11302858853782E+04 0.17132629315897E+04
 0.11851482493013E+04 0.11823678884102E+04 0.10614477830003E+04 0.17131452222977E+04 0.12836114097020E+04
 0.11826517932917E+04 0.11302858853782E+04 0.17132629315897E+04 0.11851482493013E+04 0.11823678884102E+04
 0.10614477830003E+04 0.17131452222977E+04 0.16626308276413E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50454028764458E+03 0.12948770276681E+01
 0.12948770276681E+01 0.52638957792777E+01 0.00000000000000E+00 0.34951616727670E+03 0.34951616727670E+03
 0.34951616727670E+03 0.34951616727670E+03 0.00000000000000E+00 0.00000000000000E+00 0.14614527633116E+00
 0.00000000000000E+00 -.14599972778333E+02 0.10000000000000E-02 0.15203565405367E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52619236256096E+01 0.19732213596036E+01 0.36030375709422E+03 0.43483452468494E+03
 0.31809274428213E+03 0.31809274428213E+03 0.29815206752428E+03 0.29815203930748E+03 0.31808578792945E+03
 0.31808578792945E+03 0.29815206774025E+03 0.29815203952050E+03 0.31809274428213E+03 0.31809274428213E+03
 0.29815206752428E+03 0.29815203930748E+03 0.31808578792945E+03 0.31808578792945E+03 0.29815206774025E+03
 0.29815203952050E+03 0.31532340806742E+03 0.29815637116579E+03 -.11602866640721E+03 -.15439450251558E+03
 0.25242582686079E+03 0.55422211251881E+03 0.30053415652372E+03 0.27373058451805E+03 0.23017519703921E+03
 0.27373058451805E+03 0.43474821316251E+03 0.27376965977164E+03 0.22994040321553E+03 0.27376965977164E+03
 0.43456814409199E+03 0.27373058451805E+03 0.23017519703921E+03 0.27373058451805E+03 0.43474821316251E+03
 0.27376965977164E+03 0.22994040321553E+03 0.27376965977164E+03 0.43456814409199E+03 0.22636148206106E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37593919176714E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18886383062319E+00 0.00000000000000E+00 0.00000000000000E+00 0.18886383062319E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19592982128265E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19592982128265E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639247327617E+00 0.19778315710885E+00 0.34951616727670E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1371.52431664
 0.10988982195693E+00 0.32664892099721E+03 0.46620202700222E+03 0.46109020501141E+03 0.45901871244750E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14406846896552E+00 0.00000000000000E+00 -.23421171695184E+02
 0.32232413137360E-02 0.11681093356280E+01 0.24819736474299E+04 0.93074011778620E+03 0.68486739691185E+01
 0.25682527384195E+01 0.36452999459294E+03 0.29818293573606E+03 0.35531463012282E+03 0.38958317297029E+03
 0.29815771976932E+03 0.29816414804945E+03 0.35085521690276E+03 0.38956360750851E+03 0.29815663689688E+03
 0.29816412733339E+03 0.35531463012282E+03 0.38958317297029E+03 0.29815771976932E+03 0.29816414804945E+03
 0.35085521690276E+03 0.38956360750851E+03 0.29815663689688E+03 0.29816412733339E+03 0.43522732357590E+03
 0.36066946836154E+03 0.21222210285063E+04 0.19107220288741E+04 0.51826685219421E+03 0.78872661666086E+03
 0.26786843020569E+03 0.12854418380887E+04 0.11836028666427E+04 0.11312759268875E+04 0.17131221951782E+04
 0.11871331389590E+04 0.11833193617453E+04 0.10626765667601E+04 0.17130044877033E+04 0.12854418380887E+04
 0.11836028666427E+04 0.11312759268875E+04 0.17131221951782E+04 0.11871331389590E+04 0.11833193617453E+04
 0.10626765667601E+04 0.17130044877033E+04 0.16623229107462E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50480789690966E+03 0.12948770561922E+01
 0.12948770561922E+01 0.53061165161360E+01 0.00000000000000E+00 0.34970379994937E+03 0.34970379994937E+03
 0.34970379994937E+03 0.34970379994937E+03 0.00000000000000E+00 0.00000000000000E+00 0.14603192235363E+00
 0.00000000000000E+00 -.14589269208208E+02 0.10000000000000E-02 0.15215989386500E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52576272214660E+01 0.19716102080498E+01 0.36067299369149E+03 0.43522397825088E+03
 0.31822489774889E+03 0.31822489774889E+03 0.29815219162459E+03 0.29815215893897E+03 0.31821787745970E+03
 0.31821787745970E+03 0.29815219184605E+03 0.29815215915714E+03 0.31822489774889E+03 0.31822489774889E+03
 0.29815219162459E+03 0.29815215893897E+03 0.31821787745970E+03 0.31821787745970E+03 0.29815219184605E+03
 0.29815215915714E+03 0.31544016323432E+03 0.29815669961876E+03 -.11794524551480E+03 -.15715635558097E+03
 0.25337069848547E+03 0.55585812889110E+03 0.30122057691321E+03 0.27533835184424E+03 0.23097133961664E+03
 0.27533835184424E+03 0.43593686087469E+03 0.27537786572364E+03 0.23073534728764E+03 0.27537786572364E+03
 0.43575610612327E+03 0.27533835184424E+03 0.23097133961664E+03 0.27533835184424E+03 0.43593686087469E+03
 0.27537786572364E+03 0.23073534728764E+03 0.27537786572364E+03 0.43575610612327E+03 0.22644779328667E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37614511718632E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18879769514515E+00 0.00000000000000E+00 0.00000000000000E+00 0.18879769514515E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19583811185289E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19583811185289E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639242974998E+00 0.19767701414675E+00 0.34970379994937E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1380.73351347
 0.10982129054720E+00 0.32678764276577E+03 0.46643525966311E+03 0.46132316582661E+03 0.45925097913170E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14401180278586E+00 0.00000000000000E+00 -.23423230239342E+02
 0.32252525158626E-02 0.11682969102866E+01 0.24804259389471E+04 0.93015972710516E+03 0.68475743876076E+01
 0.25678403953529E+01 0.36480775783484E+03 0.29818435510533E+03 0.35556182193112E+03 0.38991360234487E+03
 0.29815809994295E+03 0.29816483764165E+03 0.35109587060442E+03 0.38989406534131E+03 0.29815696696888E+03
 0.29816481609084E+03 0.35556182193112E+03 0.38991360234487E+03 0.29815809994295E+03 0.29816483764165E+03
 0.35109587060442E+03 0.38989406534131E+03 0.29815696696888E+03 0.29816481609084E+03 0.43556492341122E+03
 0.36098903868773E+03 0.21242488790058E+04 0.19117911881816E+04 0.51736501604545E+03 0.78671202354778E+03
 0.26676018242211E+03 0.12870285547380E+04 0.11844197733112E+04 0.11321337001115E+04 0.17129894018140E+04
 0.11888539949316E+04 0.11841366127703E+04 0.10637411380439E+04 0.17128716967605E+04 0.12870285547380E+04
 0.11844197733112E+04 0.11321337001114E+04 0.17129894018140E+04 0.11888539949316E+04 0.11841366127703E+04
 0.10637411380439E+04 0.17128716967605E+04 0.16620482555013E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50504000818940E+03 0.12948770713604E+01
 0.12948770713604E+01 0.53429533034412E+01 0.00000000000000E+00 0.34986698106567E+03 0.34986698106567E+03
 0.34986698106567E+03 0.34986698106567E+03 0.00000000000000E+00 0.00000000000000E+00 0.14593518948870E+00
 0.00000000000000E+00 -.14578588856616E+02 0.10000000000000E-02 0.15226448261605E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52540158167897E+01 0.19702559312961E+01 0.36099255149944E+03 0.43556160007768E+03
 0.31834006366644E+03 0.31834006366644E+03 0.29815230207351E+03 0.29815226774066E+03 0.31833298764184E+03
 0.31833298764184E+03 0.29815230229931E+03 0.29815226796309E+03 0.31834006366644E+03 0.31834006366644E+03
 0.29815230207351E+03 0.29815226774066E+03 0.31833298764184E+03 0.31833298764184E+03 0.29815230229931E+03
 0.29815226796309E+03 0.31554193818868E+03 0.29815699646325E+03 -.11960460923661E+03 -.15954664530599E+03
 0.25419169791354E+03 0.55727730609311E+03 0.30181464969001E+03 0.27673211515863E+03 0.23166268237173E+03
 0.27673211515863E+03 0.43696735370619E+03 0.27677201246752E+03 0.23142564800913E+03 0.27677201246752E+03
 0.43678600388452E+03 0.27673211515863E+03 0.23166268237173E+03 0.27673211515863E+03 0.43696735370619E+03
 0.27677201246752E+03 0.23142564800913E+03 0.27677201246752E+03 0.43678600388452E+03 0.22652380272217E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37632392237838E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18873024911710E+00 0.00000000000000E+00 0.00000000000000E+00 0.18873024911710E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19575475144002E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19575475144002E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639243742578E+00 0.19758484436339E+00 0.34986698106567E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1393.72886248
 0.10973173729457E+00 0.32697891634658E+03 0.46676182005966E+03 0.46164894643682E+03 0.45957557134766E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14393335168261E+00 0.00000000000000E+00 -.23431975813739E+02
 0.32278844193907E-02 0.11685470818631E+01 0.24784034867984E+04 0.92940130754940E+03 0.68461084060432E+01
 0.25672906522662E+01 0.36519764843275E+03 0.29818644385589E+03 0.35590881373016E+03 0.39037788580975E+03
 0.29815866390708E+03 0.29816585973945E+03 0.35143359857856E+03 0.39035838809136E+03 0.29815745692439E+03
 0.29816583696702E+03 0.35590881373016E+03 0.39037788580975E+03 0.29815866390708E+03 0.29816585973945E+03
 0.35143359857856E+03 0.39035838809136E+03 0.29815745692439E+03 0.29816583696702E+03 0.43604140984600E+03
 0.36144034498872E+03 0.21270763956236E+04 0.19132451717983E+04 0.51602758353261E+03 0.78377968195952E+03
 0.26517196050925E+03 0.12892485048228E+04 0.11855390728348E+04 0.11333067492035E+04 0.17127666401749E+04
 0.11912634721766E+04 0.11852563714437E+04 0.10652071187157E+04 0.17126489199238E+04 0.12892485048228E+04
 0.11855390728348E+04 0.11333067492034E+04 0.17127666401749E+04 0.11912634721766E+04 0.11852563714437E+04
 0.10652071187157E+04 0.17126489199238E+04 0.16616038861916E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50536449769667E+03 0.12948771358013E+01
 0.12948771358013E+01 0.53949346994822E+01 0.00000000000000E+00 0.35009631283284E+03 0.35009631283284E+03
 0.35009631283284E+03 0.35009631283284E+03 0.00000000000000E+00 0.00000000000000E+00 0.14580200705801E+00
 0.00000000000000E+00 -.14570163479792E+02 0.10000000000000E-02 0.15240628467126E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52491273685046E+01 0.19684227631892E+01 0.36144383046667E+03 0.43603812411063E+03
 0.31850148200941E+03 0.31850148200941E+03 0.29815250546974E+03 0.29815242939091E+03 0.31849432765958E+03
 0.31849432765958E+03 0.29815250570491E+03 0.29815242961894E+03 0.31850148200941E+03 0.31850148200941E+03
 0.29815250546974E+03 0.29815242939091E+03 0.31849432765958E+03 0.31849432765958E+03 0.29815250570491E+03
 0.29815242961894E+03 0.31568457005973E+03 0.29815743404314E+03 -.12197012441020E+03 -.16295475760948E+03
 0.25535073679168E+03 0.55928322265199E+03 0.30265573217635E+03 0.27870871300949E+03 0.23263855630009E+03
 0.27870871300949E+03 0.43842516006619E+03 0.27874914993881E+03 0.23240000665580E+03 0.27874914993881E+03
 0.43824292387988E+03 0.27870871300949E+03 0.23263855630009E+03 0.27870871300949E+03 0.43842516006619E+03
 0.27874914993881E+03 0.23240000665580E+03 0.27874914993881E+03 0.43824292387988E+03 0.22662451128770E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37657616766530E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18868204628784E+00 0.00000000000000E+00 0.00000000000000E+00 0.18868204628784E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19565985173226E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19565985173226E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639223184431E+00 0.19745522547923E+00 0.35009631283284E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1403.65447922
 0.10967380688615E+00 0.32712430230770E+03 0.46700971619630E+03 0.46189579423996E+03 0.45982134632474E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14387473903353E+00 0.00000000000000E+00 -.23437337200535E+02
 0.32295892125287E-02 0.11687240045997E+01 0.24770952197157E+04 0.92891070739337E+03 0.68450720345560E+01
 0.25669020129585E+01 0.36549435511134E+03 0.29818810341373E+03 0.35617302053434E+03 0.39073060898714E+03
 0.29815911538500E+03 0.29816667730690E+03 0.35169088924819E+03 0.39071114055719E+03 0.29815784939280E+03
 0.29816665356905E+03 0.35617302053434E+03 0.39073060898714E+03 0.29815911538500E+03 0.29816667730690E+03
 0.35169088924819E+03 0.39071114055719E+03 0.29815784939280E+03 0.29816665356905E+03 0.43640089034207E+03
 0.36178008405441E+03 0.21292078187444E+04 0.19143291741260E+04 0.51504020370079E+03 0.78160559751948E+03
 0.26399019280019E+03 0.12909285126598E+04 0.11863794799965E+04 0.11341896182550E+04 0.17125873112433E+04
 0.11930867485521E+04 0.11860971297214E+04 0.10663106917941E+04 0.17124695869111E+04 0.12909285126598E+04
 0.11863794799965E+04 0.11341896182550E+04 0.17125873112433E+04 0.11930867485521E+04 0.11860971297214E+04
 0.10663106917941E+04 0.17124695869111E+04 0.16612669324046E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50561027977570E+03 0.12948771753062E+01
 0.12948771753062E+01 0.54346371664215E+01 0.00000000000000E+00 0.35027136568162E+03 0.35027136568162E+03
 0.35027136568162E+03 0.35027136568162E+03 0.00000000000000E+00 0.00000000000000E+00 0.14570281403741E+00
 0.00000000000000E+00 -.14562804746999E+02 0.10000000000000E-02 0.15250993065024E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52455600536248E+01 0.19670850201093E+01 0.36178355798687E+03 0.43639762442000E+03
 0.31862490380940E+03 0.31862490380940E+03 0.29815263912605E+03 0.29815255898871E+03 0.31861768951544E+03
 0.31861768951544E+03 0.29815263936526E+03 0.29815255922066E+03 0.31862490380940E+03 0.31862490380940E+03
 0.29815263912605E+03 0.29815255898871E+03 0.31861768951544E+03 0.31861768951544E+03 0.29815263936526E+03
 0.29815255922066E+03 0.31579367378548E+03 0.29815778227154E+03 -.12373914332312E+03 -.16550067450958E+03
 0.25623394130677E+03 0.56081112507811E+03 0.30329601406480E+03 0.28020029251647E+03 0.23338214149863E+03
 0.28020029251647E+03 0.43953575387039E+03 0.28024114400022E+03 0.23314246049316E+03 0.28024114400022E+03
 0.43935286786300E+03 0.28020029251647E+03 0.23338214149863E+03 0.28020029251647E+03 0.43953575387039E+03
 0.28024114400022E+03 0.23314246049316E+03 0.28024114400022E+03 0.43935286786300E+03 0.22671328797668E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37676898993063E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18863579711046E+00 0.00000000000000E+00 0.00000000000000E+00 0.18863579711046E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19559741226103E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19559741226103E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639210589993E+00 0.19735643233900E+00 0.35027136568162E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1413.58009595
 0.10961471509350E+00 0.32727167468753E+03 0.46725660934827E+03 0.46214180643820E+03 0.46006640870437E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14381728185469E+00 0.00000000000000E+00 -.23438813857907E+02
 0.32313300442184E-02 0.11688876087455E+01 0.24757607209805E+04 0.92841027036769E+03 0.68441139594131E+01
 0.25665427347799E+01 0.36579013134241E+03 0.29818981787907E+03 0.35643651687543E+03 0.39108178633172E+03
 0.29815958472656E+03 0.29816752664108E+03 0.35194757261979E+03 0.39106234661128E+03 0.29815825759523E+03
 0.29816750191038E+03 0.35643651687543E+03 0.39108178633172E+03 0.29815958472656E+03 0.29816752664108E+03
 0.35194757261979E+03 0.39106234661128E+03 0.29815825759523E+03 0.29816750191038E+03 0.43675755440430E+03
 0.36211652805592E+03 0.21313281736167E+04 0.19154237662380E+04 0.51407152475462E+03 0.77947033501413E+03
 0.26282845263574E+03 0.12926013554526E+04 0.11872128749514E+04 0.11350831132531E+04 0.17124070951688E+04
 0.11949022017912E+04 0.11869308718521E+04 0.10674227083564E+04 0.17122893686102E+04 0.12926013554526E+04
 0.11872128749514E+04 0.11350831132531E+04 0.17124070951688E+04 0.11949022017912E+04 0.11869308718521E+04
 0.10674227083564E+04 0.17122893686102E+04 0.16609416525534E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50585528255318E+03 0.12948771861868E+01
 0.12948771861868E+01 0.54743396333607E+01 0.00000000000000E+00 0.35044582620044E+03 0.35044582620044E+03
 0.35044582620044E+03 0.35044582620044E+03 0.00000000000000E+00 0.00000000000000E+00 0.14560575872046E+00
 0.00000000000000E+00 -.14550999368372E+02 0.10000000000000E-02 0.15260986661637E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52421250194199E+01 0.19657968822825E+01 0.36211999871925E+03 0.43675430171246E+03
 0.31874830414448E+03 0.31874830414448E+03 0.29815277824037E+03 0.29815269387878E+03 0.31874102994087E+03
 0.31874102994087E+03 0.29815277848328E+03 0.29815269411431E+03 0.31874830414448E+03 0.31874830414448E+03
 0.29815277824037E+03 0.29815269387878E+03 0.31874102994087E+03 0.31874102994087E+03 0.29815277848328E+03
 0.29815269411431E+03 0.31590279377236E+03 0.29815814250631E+03 -.12548915617724E+03 -.16801773547207E+03
 0.25711238055887E+03 0.56232597053995E+03 0.30392802807829E+03 0.28167891336087E+03 0.23412109163904E+03
 0.28167891336087E+03 0.44063542560489E+03 0.28172018043763E+03 0.23388028921076E+03 0.28172018043763E+03
 0.44045189892640E+03 0.28167891336087E+03 0.23412109163904E+03 0.28167891336087E+03 0.44063542560489E+03
 0.28172018043763E+03 0.23388028921076E+03 0.28172018043763E+03 0.44045189892640E+03 0.22680258089634E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37695998770039E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18855945889368E+00 0.00000000000000E+00 0.00000000000000E+00 0.18855945889368E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19551379525920E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19551379525920E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639212805044E+00 0.19725822871341E+00 0.35044582620044E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1423.50571269
 0.10954501247863E+00 0.32742112216885E+03 0.46750264579062E+03 0.46238756837290E+03 0.46031151222627E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14376084336659E+00 0.00000000000000E+00 -.23436149091875E+02
 0.32333859174336E-02 0.11690394107570E+01 0.24741865661213E+04 0.92781996229547E+03 0.68432252380782E+01
 0.25662094642793E+01 0.36608503935754E+03 0.29819158829569E+03 0.35669935475158E+03 0.39143149570274E+03
 0.29816007239037E+03 0.29816840852690E+03 0.35220368899101E+03 0.39141208414845E+03 0.29815868194433E+03
 0.29816838277561E+03 0.35669935475158E+03 0.39143149570274E+03 0.29816007239037E+03 0.29816840852690E+03
 0.35220368899101E+03 0.39141208414845E+03 0.29815868194433E+03 0.29816838277561E+03 0.43711176422646E+03
 0.36244997837827E+03 0.21334467380921E+04 0.19165385310558E+04 0.51311901784729E+03 0.77736933879811E+03
 0.26168472586158E+03 0.12942712205642E+04 0.11880417573135E+04 0.11359892550623E+04 0.17122286993504E+04
 0.11967144540613E+04 0.11877601006988E+04 0.10685461229272E+04 0.17121109752949E+04 0.12942712205642E+04
 0.11880417573135E+04 0.11359892550623E+04 0.17122286993505E+04 0.11967144540613E+04 0.11877601006988E+04
 0.10685461229272E+04 0.17121109752949E+04 0.16606320101654E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50610017400452E+03 0.12948771665517E+01
 0.12948771665517E+01 0.55140421002999E+01 0.00000000000000E+00 0.35061908411803E+03 0.35061908411803E+03
 0.35061908411803E+03 0.35061908411803E+03 0.00000000000000E+00 0.00000000000000E+00 0.14551079868767E+00
 0.00000000000000E+00 -.14534389036106E+02 0.10000000000000E-02 0.15270644175057E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52388097766478E+01 0.19645536662429E+01 0.36245344519377E+03 0.43710852611116E+03
 0.31887159235932E+03 0.31887159235932E+03 0.29815292296068E+03 0.29815283420461E+03 0.31886425829781E+03
 0.31886425829781E+03 0.29815292320690E+03 0.29815283444335E+03 0.31887159235932E+03 0.31887159235932E+03
 0.29815292296068E+03 0.29815283420461E+03 0.31886425829781E+03 0.31886425829781E+03 0.29815292320690E+03
 0.29815283444335E+03 0.31601184999826E+03 0.29815851499652E+03 -.12722770145664E+03 -.17051771660910E+03
 0.25798170566669E+03 0.56381785903971E+03 0.30454624484468E+03 0.28314362305089E+03 0.23485110034312E+03
 0.28314362305089E+03 0.44171575627235E+03 0.28318530662679E+03 0.23460918465227E+03 0.28318530662679E+03
 0.44153159592631E+03 0.28314362305089E+03 0.23485110034312E+03 0.28314362305089E+03 0.44171575627235E+03
 0.28318530662678E+03 0.23460918465227E+03 0.28318530662678E+03 0.44153159592631E+03 0.22688466182353E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37714853035600E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18845069738603E+00 0.00000000000000E+00 0.00000000000000E+00 0.18845069738603E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19540661046635E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19540661046635E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639231137593E+00 0.19716097022219E+00 0.35061908411803E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1433.43132942
 0.10945679342017E+00 0.32757283102289E+03 0.46774806393199E+03 0.46263368676161E+03 0.46055740567778E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14370515611366E+00 0.00000000000000E+00 -.23428712430382E+02
 0.32359917365958E-02 0.11691824706981E+01 0.24721941992398E+04 0.92707282471491E+03 0.68423879082137E+01
 0.25658954655801E+01 0.36637915160940E+03 0.29819341570677E+03 0.35696158682611E+03 0.39177979727591E+03
 0.29816057883869E+03 0.29816932375427E+03 0.35245928863056E+03 0.39176041338526E+03 0.29815912285640E+03
 0.29816929695436E+03 0.35696158682611E+03 0.39177979727591E+03 0.29816057883869E+03 0.29816932375427E+03
 0.35245928863056E+03 0.39176041338526E+03 0.29815912285640E+03 0.29816929695436E+03 0.43746370899406E+03
 0.36278048573819E+03 0.21355735027221E+04 0.19176844510273E+04 0.51218502659368E+03 0.77530556241360E+03
 0.26055961068696E+03 0.12959429594590E+04 0.11888707702456E+04 0.11369117659790E+04 0.17120579021271E+04
 0.11985286476921E+04 0.11885894648493E+04 0.10696852886393E+04 0.17119401903706E+04 0.12959429594589E+04
 0.11888707702456E+04 0.11369117659790E+04 0.17120579021271E+04 0.11985286476921E+04 0.11885894648493E+04
 0.10696852886393E+04 0.17119401903706E+04 0.16603457224951E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50634563013835E+03 0.12948771117554E+01
 0.12948771117554E+01 0.55537445672392E+01 0.00000000000000E+00 0.35079060356480E+03 0.35079060356480E+03
 0.35079060356480E+03 0.35079060356480E+03 0.00000000000000E+00 0.00000000000000E+00 0.14541788721409E+00
 0.00000000000000E+00 -.14512166579127E+02 0.10000000000000E-02 0.15279998120155E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52356027383586E+01 0.19633510268845E+01 0.36278394643458E+03 0.43746048830114E+03
 0.31899469729821E+03 0.31899469729821E+03 0.29815307343646E+03 0.29815298011115E+03 0.31898730343956E+03
 0.31898730343956E+03 0.29815307368558E+03 0.29815298035270E+03 0.31899469729821E+03 0.31899469729821E+03
 0.29815307343646E+03 0.29815298011115E+03 0.31898730343956E+03 0.31898730343956E+03 0.29815307368558E+03
 0.29815298035270E+03 0.31612078298159E+03 0.29815889999196E+03 -.12895939774701E+03 -.17300799800993E+03
 0.25883791882787E+03 0.56527753790451E+03 0.30514542948250E+03 0.28459260821841E+03 0.23556825444879E+03
 0.28459260821841E+03 0.44276889301583E+03 0.28463470914940E+03 0.23532523406724E+03 0.28463470914940E+03
 0.44258410607042E+03 0.28459260821841E+03 0.23556825444879E+03 0.28459260821841E+03 0.44276889301583E+03
 0.28463470914940E+03 0.23532523406723E+03 0.28463470914940E+03 0.44258410607042E+03 0.22695318304600E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37733392460945E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18830423119473E+00 0.00000000000000E+00 0.00000000000000E+00 0.18830423119473E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19527079892465E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19527079892465E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639268358150E+00 0.19706498487236E+00 0.35079060356480E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1440.11700256
 0.10938654946314E+00 0.32767692669149E+03 0.46791344438657E+03 0.46280011479344E+03 0.46072394404206E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14366789378700E+00 0.00000000000000E+00 -.23429249742190E+02
 0.32380696351150E-02 0.11692751542129E+01 0.24706077699024E+04 0.92647791371339E+03 0.68418455409542E+01
 0.25656920778578E+01 0.36657713579914E+03 0.29819467678897E+03 0.35713822011020E+03 0.39201337989948E+03
 0.29816092997117E+03 0.29816995796664E+03 0.35263160983709E+03 0.39199401433428E+03 0.29815942866642E+03
 0.29816993044567E+03 0.35713822011020E+03 0.39201337989948E+03 0.29816092997117E+03 0.29816995796664E+03
 0.35263160983709E+03 0.39199401433428E+03 0.29815942866642E+03 0.29816993044567E+03 0.43769719810871E+03
 0.36299857581010E+03 0.21370151801789E+04 0.19184809858433E+04 0.51161401006211E+03 0.77401153223073E+03
 0.25983945211831E+03 0.12970726166796E+04 0.11894400221883E+04 0.11375469499691E+04 0.17119631988780E+04
 0.11997534479113E+04 0.11891589707433E+04 0.10704644301114E+04 0.17118455161413E+04 0.12970726166796E+04
 0.11894400221883E+04 0.11375469499691E+04 0.17119631988780E+04 0.11997534479113E+04 0.11891589707433E+04
 0.10704644301114E+04 0.17118455161413E+04 0.16601951933256E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50651173633078E+03 0.12948771157145E+01
 0.12948771157145E+01 0.55804872597970E+01 0.00000000000000E+00 0.35090524665341E+03 0.35090524665341E+03
 0.35090524665341E+03 0.35090524665341E+03 0.00000000000000E+00 0.00000000000000E+00 0.14535642586257E+00
 0.00000000000000E+00 -.14504017888346E+02 0.10000000000000E-02 0.15286128389755E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52335030794074E+01 0.19625636547778E+01 0.36300203165630E+03 0.43769399029083E+03
 0.31907797919067E+03 0.31907797919067E+03 0.29815309178203E+03 0.29815308136501E+03 0.31907054493021E+03
 0.31907054493021E+03 0.29815309202608E+03 0.29815308160824E+03 0.31907797919067E+03 0.31907797919067E+03
 0.29815309178203E+03 0.29815308136501E+03 0.31907054493021E+03 0.31907054493021E+03 0.29815309202608E+03
 0.29815308160824E+03 0.31619454119066E+03 0.29815916594758E+03 -.13009231693513E+03 -.17463573342530E+03
 0.25940428016904E+03 0.56624154274716E+03 0.30554024117728E+03 0.28554481978029E+03 0.23604151143849E+03
 0.28554481978029E+03 0.44346303307210E+03 0.28558720393808E+03 0.23579778773967E+03 0.28558720393808E+03
 0.44327786637168E+03 0.28554481978029E+03 0.23604151143849E+03 0.28554481978029E+03 0.44346303307210E+03
 0.28558720393808E+03 0.23579778773967E+03 0.28558720393808E+03 0.44327786637168E+03 0.22700208415483E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37746049543509E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18824873036214E+00 0.00000000000000E+00 0.00000000000000E+00 0.18824873036214E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19522515345580E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19522515345580E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639271169024E+00 0.19700064647167E+00 0.35090524665341E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1451.16478865
 0.10929275417068E+00 0.32784269439179E+03 0.46818628518814E+03 0.46307343933203E+03 0.46099684390018E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14360683136952E+00 0.00000000000000E+00 -.23435932498583E+02
 0.32408483347901E-02 0.11694222762135E+01 0.24684894736113E+04 0.92568355260422E+03 0.68409847860122E+01
 0.25653692947546E+01 0.36690345134480E+03 0.29819681097262E+03 0.35742936977839E+03 0.39239850879308E+03
 0.29816152694205E+03 0.29817103564458E+03 0.35291563844170E+03 0.39237917318792E+03 0.29815994877616E+03
 0.29817100690763E+03 0.35742936977839E+03 0.39239850879308E+03 0.29816152694205E+03 0.29817103564458E+03
 0.35291563844170E+03 0.39237917318792E+03 0.29815994877616E+03 0.29817100690763E+03 0.43808361872128E+03
 0.36335882284292E+03 0.21393813422105E+04 0.19197378628291E+04 0.51064370021363E+03 0.77183647600162E+03
 0.25863955728692E+03 0.12989328855089E+04 0.11903778060775E+04 0.11385562431240E+04 0.17118088764618E+04
 0.12017702628244E+04 0.11900971501620E+04 0.10717117529279E+04 0.17116912210687E+04 0.12989328855089E+04
 0.11903778060775E+04 0.11385562431240E+04 0.17118088764618E+04 0.12017702628244E+04 0.11900971501620E+04
 0.10717117529279E+04 0.17116912210687E+04 0.16599210373113E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50678424312088E+03 0.12948771649557E+01
 0.12948771649557E+01 0.56246784041363E+01 0.00000000000000E+00 0.35109522383900E+03 0.35109522383900E+03
 0.35109522383900E+03 0.35109522383900E+03 0.00000000000000E+00 0.00000000000000E+00 0.14525672650707E+00
 0.00000000000000E+00 -.14495949369065E+02 0.10000000000000E-02 0.15295888833565E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52301635341681E+01 0.19613113253130E+01 0.36336229305761E+03 0.43808041599874E+03
 0.31921517024904E+03 0.31921517024904E+03 0.29815330551621E+03 0.29815325366644E+03 0.31920766953457E+03
 0.31920766953457E+03 0.29815330576572E+03 0.29815325391204E+03 0.31921517024904E+03 0.31921517024904E+03
 0.29815330551621E+03 0.29815325366644E+03 0.31920766953457E+03 0.31920766953457E+03 0.29815330576572E+03
 0.29815325391204E+03 0.31631604798711E+03 0.29815961649288E+03 -.13196752036330E+03 -.17732792879301E+03
 0.26034764510102E+03 0.56784971194485E+03 0.30620032861832E+03 0.28712578596788E+03 0.23683063956927E+03
 0.28712578596788E+03 0.44462290419723E+03 0.28716863668133E+03 0.23658571744417E+03 0.28716863668133E+03
 0.44443707140241E+03 0.28712578596788E+03 0.23683063956927E+03 0.28712578596788E+03 0.44462290419723E+03
 0.28716863668133E+03 0.23658571744417E+03 0.28716863668133E+03 0.44443707140241E+03 0.22708979928023E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37766906055692E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18820341829253E+00 0.00000000000000E+00 0.00000000000000E+00 0.18820341829253E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19513168043530E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19513168043530E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639257958043E+00 0.19689393290807E+00 0.35109522383900E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1461.10209121
 0.10921447026431E+00 0.32798975221651E+03 0.46843007593785E+03 0.46331737075153E+03 0.46124026143648E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14355261242296E+00 0.00000000000000E+00 -.23438355671194E+02
 0.32431711493705E-02 0.11695490101001E+01 0.24667214992810E+04 0.92502056223037E+03 0.68402434877998E+01
 0.25650913079249E+01 0.36719587817522E+03 0.29819879467135E+03 0.35769033863279E+03 0.39274358705822E+03
 0.29816208533406E+03 0.29817204294386E+03 0.35317025283474E+03 0.39272427791446E+03 0.29816043552278E+03
 0.29817201308224E+03 0.35769033863279E+03 0.39274358705822E+03 0.29816208533406E+03 0.29817204294386E+03
 0.35317025283474E+03 0.39272427791446E+03 0.29816043552278E+03 0.29817201308224E+03 0.43842971873168E+03
 0.36368113856712E+03 0.21414846615271E+04 0.19208347660524E+04 0.50975622287392E+03 0.76986526872383E+03
 0.25756026473554E+03 0.13005924250990E+04 0.11912007187712E+04 0.11394430165461E+04 0.17116489789760E+04
 0.12035701380088E+04 0.11909204144644E+04 0.10728119423675E+04 0.17115313486609E+04 0.13005924250990E+04
 0.11912007187712E+04 0.11394430165461E+04 0.17116489789760E+04 0.12035701380088E+04 0.11909204144644E+04
 0.10728119423675E+04 0.17115313486609E+04 0.16596493917995E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50702737898879E+03 0.12948771828107E+01
 0.12948771828107E+01 0.56644276143990E+01 0.00000000000000E+00 0.35126534743518E+03 0.35126534743518E+03
 0.35126534743518E+03 0.35126534743518E+03 0.00000000000000E+00 0.00000000000000E+00 0.14516898252271E+00
 0.00000000000000E+00 -.14485091896817E+02 0.10000000000000E-02 0.15304343362164E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52272742519473E+01 0.19602278444802E+01 0.36368458813726E+03 0.43842654662426E+03
 0.31933821863924E+03 0.31933821863924E+03 0.29815346945692E+03 0.29815341503559E+03 0.31933065825390E+03
 0.31933065825390E+03 0.29815346970806E+03 0.29815341528279E+03 0.31933821863924E+03 0.31933821863924E+03
 0.29815346945692E+03 0.29815341503559E+03 0.31933065825389E+03 0.31933065825389E+03 0.29815346970806E+03
 0.29815341528279E+03 0.31642504750444E+03 0.29816003585849E+03 -.13365082273717E+03 -.17974394041738E+03
 0.26119265274582E+03 0.56928751199770E+03 0.30678889598815E+03 0.28854279752777E+03 0.23753704114480E+03
 0.28854279752777E+03 0.44565931487548E+03 0.28858606818721E+03 0.23729103837490E+03 0.28858606818721E+03
 0.44547287932395E+03 0.28854279752777E+03 0.23753704114480E+03 0.28854279752777E+03 0.44565931487548E+03
 0.28858606818721E+03 0.23729103837490E+03 0.28858606818721E+03 0.44547287932395E+03 0.22716587937573E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37785548937993E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18813533541436E+00 0.00000000000000E+00 0.00000000000000E+00 0.18813533541436E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19504353745865E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19504353745865E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639258129290E+00 0.19679859674604E+00 0.35126534743518E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1472.51304287
 0.10913089855668E+00 0.32815663090236E+03 0.46870825636235E+03 0.46359541463567E+03 0.46151758536610E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14349124357763E+00 0.00000000000000E+00 -.23442878867229E+02
 0.32456545261626E-02 0.11696867074817E+01 0.24648341145102E+04 0.92431279294132E+03 0.68394382434452E+01
 0.25647893412919E+01 0.36753031955047E+03 0.29820115068679E+03 0.35798885823466E+03 0.39313826389720E+03
 0.29816275286742E+03 0.29817324620794E+03 0.35346150596635E+03 0.39311898457969E+03 0.29816101771506E+03
 0.29817321501752E+03 0.35798885823466E+03 0.39313826389720E+03 0.29816275286742E+03 0.29817324620794E+03
 0.35346150596635E+03 0.39311898457969E+03 0.29816101771506E+03 0.29817321501752E+03 0.43882590624394E+03
 0.36404990318118E+03 0.21438743930820E+04 0.19220624840721E+04 0.50871400350559E+03 0.76757492722757E+03
 0.25631735370445E+03 0.13024838983330E+04 0.11921242815549E+04 0.11404417745907E+04 0.17114445696514E+04
 0.12056224468509E+04 0.11918443711049E+04 0.10740552677474E+04 0.17113269636277E+04 0.13024838983330E+04
 0.11921242815549E+04 0.11404417745907E+04 0.17114445696514E+04 0.12056224468509E+04 0.11918443711049E+04
 0.10740552677474E+04 0.17113269636277E+04 0.16593101266610E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50730445022482E+03 0.12948772161394E+01
 0.12948772161394E+01 0.57100714210221E+01 0.00000000000000E+00 0.35145997562087E+03 0.35145997562087E+03
 0.35145997562087E+03 0.35145997562087E+03 0.00000000000000E+00 0.00000000000000E+00 0.14507040316130E+00
 0.00000000000000E+00 -.14474678633196E+02 0.10000000000000E-02 0.15313679809496E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52240872863485E+01 0.19590327323807E+01 0.36405332700185E+03 0.43882276823108E+03
 0.31947896964489E+03 0.31947896964489E+03 0.29815369590155E+03 0.29815360819737E+03 0.31947134092791E+03
 0.31947134092791E+03 0.29815369615596E+03 0.29815360844574E+03 0.31947896964489E+03 0.31947896964489E+03
 0.29815369590155E+03 0.29815360819737E+03 0.31947134092791E+03 0.31947134092791E+03 0.29815369615596E+03
 0.29815360844574E+03 0.31654974007510E+03 0.29816053466357E+03 -.13558559889464E+03 -.18252009594665E+03
 0.26216105833369E+03 0.57093470867297E+03 0.30746284504761E+03 0.29016846122515E+03 0.23834631441477E+03
 0.29016846122515E+03 0.44684683841889E+03 0.29021221381654E+03 0.23809905583626E+03 0.29021221381654E+03
 0.44665969456280E+03 0.29016846122515E+03 0.23834631441477E+03 0.29016846122515E+03 0.44684683841889E+03
 0.29021221381654E+03 0.23809905583626E+03 0.29021221381654E+03 0.44665969456280E+03 0.22725110131725E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37806899634169E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18807170085313E+00 0.00000000000000E+00 0.00000000000000E+00 0.18807170085313E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19494945181719E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19494945181719E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639251752287E+00 0.19668957183530E+00 0.35145997562087E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1482.17649343
 0.10906595597080E+00 0.32829715198689E+03 0.46894249582517E+03 0.46382928953232E+03 0.46175075849280E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14344008441324E+00 0.00000000000000E+00 -.23446246409234E+02
 0.32475869430122E-02 0.11697955618199E+01 0.24633674603273E+04 0.92376279762274E+03 0.68388018052950E+01
 0.25645506769856E+01 0.36781258034037E+03 0.29820321188937E+03 0.35824089957421E+03 0.39347102466376E+03
 0.29816334056489E+03 0.29817430477193E+03 0.35370748714724E+03 0.39345177012342E+03 0.29816153054094E+03
 0.29817427242495E+03 0.35824089957421E+03 0.39347102466376E+03 0.29816334056489E+03 0.29817430477193E+03
 0.35370748714724E+03 0.39345177012342E+03 0.29816153054094E+03 0.29817427242495E+03 0.43915871245343E+03
 0.36435916667976E+03 0.21458765797894E+04 0.19230808172079E+04 0.50784326895887E+03 0.76566237748579E+03
 0.25527989218212E+03 0.13040735944650E+04 0.11928925327875E+04 0.11412758786975E+04 0.17112605027306E+04
 0.12073475062548E+04 0.11926129557603E+04 0.10750947810030E+04 0.17111429219249E+04 0.13040735944650E+04
 0.11928925327875E+04 0.11412758786975E+04 0.17112605027306E+04 0.12073475062548E+04 0.11926129557603E+04
 0.10750947810030E+04 0.17111429219249E+04 0.16590168429398E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50753745823153E+03 0.12948772409528E+01
 0.12948772409528E+01 0.57487252232598E+01 0.00000000000000E+00 0.35162438562248E+03 0.35162438562248E+03
 0.35162438562248E+03 0.35162438562248E+03 0.00000000000000E+00 0.00000000000000E+00 0.14498868009744E+00
 0.00000000000000E+00 -.14465695469785E+02 0.10000000000000E-02 0.15321278823144E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52214962552053E+01 0.19580610957020E+01 0.36436257158329E+03 0.43915559967361E+03
 0.31959805030930E+03 0.31959805030930E+03 0.29815387031656E+03 0.29815377847346E+03 0.31959036373185E+03
 0.31959036373185E+03 0.29815387057135E+03 0.29815377872220E+03 0.31959805030930E+03 0.31959805030930E+03
 0.29815387031656E+03 0.29815377847346E+03 0.31959036373185E+03 0.31959036373185E+03 0.29815387057135E+03
 0.29815377872220E+03 0.31665526599953E+03 0.29816097167582E+03 -.13720479989007E+03 -.18484140130331E+03
 0.26297851376038E+03 0.57232406278201E+03 0.30803065645282E+03 0.29153433416586E+03 0.23902923259645E+03
 0.29153433416586E+03 0.44784842696464E+03 0.29157849603331E+03 0.23878092163607E+03 0.29157849603331E+03
 0.44766069447600E+03 0.29153433416586E+03 0.23902923259645E+03 0.29153433416586E+03 0.44784842696464E+03
 0.29157849603332E+03 0.23878092163607E+03 0.29157849603332E+03 0.44766069447600E+03 0.22732783894564E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37824960398790E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18801486038978E+00 0.00000000000000E+00 0.00000000000000E+00 0.18801486038978E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19487724568111E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19487724568111E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639247015410E+00 0.19659757515976E+00 0.35162438562248E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1491.83994399
 0.10899978647040E+00 0.32843893911386E+03 0.46917579039704E+03 0.46406236148425E+03 0.46198322550538E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14338968049360E+00 0.00000000000000E+00 -.23446640210655E+02
 0.32495582401072E-02 0.11698964883871E+01 0.24618730943983E+04 0.92320241039935E+03 0.68382118242181E+01
 0.25643294340818E+01 0.36809402026333E+03 0.29820533276250E+03 0.35849230396103E+03 0.39380245936206E+03
 0.29816394863958E+03 0.29817539931046E+03 0.35395291323972E+03 0.39378322918069E+03 0.29816206138797E+03
 0.29817536577888E+03 0.35849230396103E+03 0.39380245936207E+03 0.29816394863958E+03 0.29817539931046E+03
 0.35395291323972E+03 0.39378322918069E+03 0.29816206138797E+03 0.29817536577888E+03 0.43948925214873E+03
 0.36466577427464E+03 0.21478684373848E+04 0.19241042937298E+04 0.50698554612177E+03 0.76377755486465E+03
 0.25425708101227E+03 0.13056567110439E+04 0.11936534899690E+04 0.11421157751881E+04 0.17110736996575E+04
 0.12090654752954E+04 0.11933742458974E+04 0.10761384645335E+04 0.17109561478282E+04 0.13056567110439E+04
 0.11936534899690E+04 0.11421157751881E+04 0.17110736996575E+04 0.12090654752954E+04 0.11933742458974E+04
 0.10761384645335E+04 0.17109561478282E+04 0.16587300965906E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50776970840475E+03 0.12948772438545E+01
 0.12948772438545E+01 0.57873790254975E+01 0.00000000000000E+00 0.35178815190322E+03 0.35178815190322E+03
 0.35178815190322E+03 0.35178815190322E+03 0.00000000000000E+00 0.00000000000000E+00 0.14490852813687E+00
 0.00000000000000E+00 -.14453335275766E+02 0.10000000000000E-02 0.15328615909731E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52189969708362E+01 0.19571238640636E+01 0.36466916786973E+03 0.43948615835692E+03
 0.31971702783703E+03 0.31971702783703E+03 0.29815405098205E+03 0.29815395485169E+03 0.31970928345563E+03
 0.31970928345563E+03 0.29815405123663E+03 0.29815395510023E+03 0.31971702783703E+03 0.31971702783703E+03
 0.29815405098205E+03 0.29815395485169E+03 0.31970928345563E+03 0.31970928345563E+03 0.29815405123663E+03
 0.29815395510023E+03 0.31676073194531E+03 0.29816142190935E+03 -.13880964618321E+03 -.18714061445110E+03
 0.26379131592248E+03 0.57370124335679E+03 0.30859097085469E+03 0.29288946324370E+03 0.23970767744733E+03
 0.29288946324370E+03 0.44883998127640E+03 0.29293403512465E+03 0.23945832197896E+03 0.29293403512465E+03
 0.44865166713143E+03 0.29288946324370E+03 0.23970767744733E+03 0.29288946324370E+03 0.44883998127640E+03
 0.29293403512465E+03 0.23945832197896E+03 0.29293403512465E+03 0.44865166713143E+03 0.22740394762289E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37842864464693E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18793496844584E+00 0.00000000000000E+00 0.00000000000000E+00 0.18793496844584E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19478947953910E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19478947953910E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639253569500E+00 0.19650614357839E+00 0.35178815190322E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1501.50339454
 0.10892511880200E+00 0.32858228901495E+03 0.46940828154793E+03 0.46429511889224E+03 0.46221561265339E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14333992751427E+00 0.00000000000000E+00 -.23443665045592E+02
 0.32517856112464E-02 0.11699905620273E+01 0.24601867885545E+04 0.92257004570793E+03 0.68376619945876E+01
 0.25641232479703E+01 0.36837469042492E+03 0.29820751425810E+03 0.35874311442221E+03 0.39413262218436E+03
 0.29816457753731E+03 0.29817653057691E+03 0.35419782017974E+03 0.39411341597049E+03 0.29816261065953E+03
 0.29817649583247E+03 0.35874311442221E+03 0.39413262218436E+03 0.29816457753731E+03 0.29817653057691E+03
 0.35419782017974E+03 0.39411341597049E+03 0.29816261065953E+03 0.29817649583247E+03 0.43981774844290E+03
 0.36496990012054E+03 0.21498576631201E+04 0.19251426144943E+04 0.50614033395204E+03 0.76191926570100E+03
 0.25324823007920E+03 0.13072367845390E+04 0.11944097060916E+04 0.11429648434624E+04 0.17108871605850E+04
 0.12107802121404E+04 0.11941307971690E+04 0.10771903243830E+04 0.17107696438882E+04 0.13072367845390E+04
 0.11944097060916E+04 0.11429648434624E+04 0.17108871605850E+04 0.12107802121404E+04 0.11941307971690E+04
 0.10771903243830E+04 0.17107696438882E+04 0.16584546066753E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50800175616677E+03 0.12948772219323E+01
 0.12948772219323E+01 0.58260328277353E+01 0.00000000000000E+00 0.35195082844257E+03 0.35195082844257E+03
 0.35195082844257E+03 0.35195082844257E+03 0.00000000000000E+00 0.00000000000000E+00 0.14482991156808E+00
 0.00000000000000E+00 -.14437084355372E+02 0.10000000000000E-02 0.15335717426078E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52165802079765E+01 0.19562175779912E+01 0.36497328345498E+03 0.43981467340247E+03
 0.31983584219988E+03 0.31983584219988E+03 0.29815423804335E+03 0.29815413747396E+03 0.31982804008204E+03
 0.31982804008204E+03 0.29815423829709E+03 0.29815413772168E+03 0.31983584219988E+03 0.31983584219988E+03
 0.29815423804335E+03 0.29815413747396E+03 0.31982804008204E+03 0.31982804008204E+03 0.29815423829709E+03
 0.29815413772168E+03 0.31686608511903E+03 0.29816188559759E+03 -.14040527437975E+03 -.18942586220602E+03
 0.26459618389524E+03 0.57505871516279E+03 0.30913955034808E+03 0.29423290365495E+03 0.24037840502292E+03
 0.29423290365495E+03 0.44981507097924E+03 0.29427788620550E+03 0.24012801218692E+03 0.29427788620550E+03
 0.44962618112085E+03 0.29423290365495E+03 0.24037840502292E+03 0.29423290365495E+03 0.44981507097924E+03
 0.29427788620550E+03 0.24012801218692E+03 0.29427788620550E+03 0.44962618112086E+03 0.22747373922039E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37860558543269E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18782865214023E+00 0.00000000000000E+00 0.00000000000000E+00 0.18782865214023E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19468296772554E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19468296772554E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639273199258E+00 0.19641554355937E+00 0.35195082844257E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1511.16684510
 0.10884173149720E+00 0.32872590331236E+03 0.46964013006978E+03 0.46452768059216E+03 0.46244799226542E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14329064028096E+00 0.00000000000000E+00 -.23447757972228E+02
 0.32542767268852E-02 0.11700797751484E+01 0.24583035406633E+04 0.92186382774872E+03 0.68371406547776E+01
 0.25639277455416E+01 0.36865463765948E+03 0.29820975732532E+03 0.35899336088326E+03 0.39446155442525E+03
 0.29816522770614E+03 0.29817769932723E+03 0.35444223748462E+03 0.39444237181535E+03 0.29816317876136E+03
 0.29817766334149E+03 0.35899336088326E+03 0.39446155442525E+03 0.29816522770614E+03 0.29817769932723E+03
 0.35444223748462E+03 0.39444237181535E+03 0.29816317876136E+03 0.29817766334149E+03 0.44014431625926E+03
 0.36527156347063E+03 0.21518473318207E+04 0.19261883216035E+04 0.50530929290623E+03 0.76008952653673E+03
 0.25225368716597E+03 0.13088156637034E+04 0.11951642008984E+04 0.11438154400150E+04 0.17107047630654E+04
 0.12124935693834E+04 0.11948856310281E+04 0.10782431811519E+04 0.17105872891497E+04 0.13088156637034E+04
 0.11951642008984E+04 0.11438154400150E+04 0.17107047630654E+04 0.12124935693834E+04 0.11948856310281E+04
 0.10782431811519E+04 0.17105872891497E+04 0.16581911675852E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50823370001288E+03 0.12948772520906E+01
 0.12948772520906E+01 0.58646866299730E+01 0.00000000000000E+00 0.35211236803895E+03 0.35211236803895E+03
 0.35211236803895E+03 0.35211236803895E+03 0.00000000000000E+00 0.00000000000000E+00 0.14475278521929E+00
 0.00000000000000E+00 -.14428906991014E+02 0.10000000000000E-02 0.15342592152050E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52142427568414E+01 0.19553410338155E+01 0.36527493584705E+03 0.44014126133104E+03
 0.31995445267053E+03 0.31995445267053E+03 0.29815443164688E+03 0.29815432648320E+03 0.31994659288969E+03
 0.31994659288969E+03 0.29815443189910E+03 0.29815432672944E+03 0.31995445267053E+03 0.31995445267053E+03
 0.29815443164688E+03 0.29815432648320E+03 0.31994659288969E+03 0.31994659288969E+03 0.29815443189910E+03
 0.29815432672944E+03 0.31697129178479E+03 0.29816236297420E+03 -.14199210858464E+03 -.19169781418496E+03
 0.26539291526415E+03 0.57640127563188E+03 0.30968139579140E+03 0.29556469969557E+03 0.24104126781773E+03
 0.29556469969557E+03 0.45077842520485E+03 0.29561009350853E+03 0.24078984462488E+03 0.29561009350853E+03
 0.45058896659246E+03 0.29556469969557E+03 0.24104126781774E+03 0.29556469969557E+03 0.45077842520485E+03
 0.29561009350853E+03 0.24078984462488E+03 0.29561009350853E+03 0.45058896659247E+03 0.22753962327523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37878330318891E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18777793807071E+00 0.00000000000000E+00 0.00000000000000E+00 0.18777793807071E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19461134653348E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19461134653348E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639266500965E+00 0.19632538233174E+00 0.35211236803895E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1520.83029566
 0.10877188304730E+00 0.32886566444436E+03 0.46987110036216E+03 0.46475862476727E+03 0.46267840046550E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14324187539883E+00 0.00000000000000E+00 -.23450153199994E+02
 0.32563662941182E-02 0.11701640577623E+01 0.24567260797564E+04 0.92127227990866E+03 0.68366482006792E+01
 0.25637430752547E+01 0.36893376137750E+03 0.29821206291035E+03 0.35924293343827E+03 0.39478926912442E+03
 0.29816589959633E+03 0.29817890631978E+03 0.35468605521779E+03 0.39477010974974E+03 0.29816376610145E+03
 0.29817886906406E+03 0.35924293343827E+03 0.39478926912442E+03 0.29816589959633E+03 0.29817890631978E+03
 0.35468605521779E+03 0.39477010974974E+03 0.29816376610145E+03 0.29817886906406E+03 0.44046897706611E+03
 0.36557090205885E+03 0.21538176607185E+04 0.19271899115382E+04 0.50448686119276E+03 0.75827949102493E+03
 0.25127019552621E+03 0.13103848957727E+04 0.11959115967668E+04 0.11446367407804E+04 0.17105196478984E+04
 0.12141962738881E+04 0.11956333618483E+04 0.10792656777560E+04 0.17104022164768E+04 0.13103848957727E+04
 0.11959115967668E+04 0.11446367407804E+04 0.17105196478984E+04 0.12141962738881E+04 0.11956333618483E+04
 0.10792656777560E+04 0.17104022164768E+04 0.16579180111039E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50846386046626E+03 0.12948772697396E+01
 0.12948772697396E+01 0.59033404322107E+01 0.00000000000000E+00 0.35227398975672E+03 0.35227398975672E+03
 0.35227398975672E+03 0.35227398975672E+03 0.00000000000000E+00 0.00000000000000E+00 0.14467707679843E+00
 0.00000000000000E+00 -.14418880808965E+02 0.10000000000000E-02 0.15349201887833E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52119973784054E+01 0.19544990169020E+01 0.36557426230980E+03 0.44046594341773E+03
 0.32007292734601E+03 0.32007292734601E+03 0.29815463194009E+03 0.29815452202337E+03 0.32006500997893E+03
 0.32006500997893E+03 0.29815463219008E+03 0.29815452226743E+03 0.32007292734601E+03 0.32007292734601E+03
 0.29815463194009E+03 0.29815452202337E+03 0.32006500997893E+03 0.32006500997893E+03 0.29815463219008E+03
 0.29815452226743E+03 0.31707640316075E+03 0.29816285427298E+03 -.14356137087564E+03 -.19394205771402E+03
 0.26619125132266E+03 0.57774632019046E+03 0.31022411261118E+03 0.29689042634979E+03 0.24170587804570E+03
 0.29689042634979E+03 0.45174432925847E+03 0.29693623189158E+03 0.24145342879947E+03 0.29693623189158E+03
 0.45155430554921E+03 0.29689042634979E+03 0.24170587804570E+03 0.29689042634979E+03 0.45174432925847E+03
 0.29693623189158E+03 0.24145342879947E+03 0.29693623189158E+03 0.45155430554921E+03 0.22761351892677E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37896045597882E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18771466808891E+00 0.00000000000000E+00 0.00000000000000E+00 0.18771466808891E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19453095986897E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19453095986897E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639265906832E+00 0.19623532262227E+00 0.35227398975672E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1530.49374622
 0.10870343210417E+00 0.32900603476583E+03 0.47010107428717E+03 0.46498856927089E+03 0.46290783651587E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14319371660621E+00 0.00000000000000E+00 -.23451926992025E+02
 0.32584166564668E-02 0.11702425683402E+01 0.24551801821056E+04 0.92069256828959E+03 0.68361895357703E+01
 0.25635710759139E+01 0.36921206988898E+03 0.29821443195615E+03 0.35949185868226E+03 0.39511576033990E+03
 0.29816659366015E+03 0.29818015231504E+03 0.35492929605667E+03 0.39509662383897E+03 0.29816437308992E+03
 0.29818011376049E+03 0.35949185868226E+03 0.39511576033990E+03 0.29816659366015E+03 0.29818015231504E+03
 0.35492929605667E+03 0.39509662383897E+03 0.29816437308992E+03 0.29818011376049E+03 0.44079170836846E+03
 0.36586796043675E+03 0.21557750215239E+04 0.19281888683603E+04 0.50367142970567E+03 0.75648704938048E+03
 0.25029726252629E+03 0.13119464736855E+04 0.11966498084906E+04 0.11454589410205E+04 0.17103286510297E+04
 0.12158908031946E+04 0.11963719080021E+04 0.10802876103497E+04 0.17102112652320E+04 0.13119464736855E+04
 0.11966498084906E+04 0.11454589410205E+04 0.17103286510297E+04 0.12158908031946E+04 0.11963719080021E+04
 0.10802876103497E+04 0.17102112652320E+04 0.16576448560839E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50869303378208E+03 0.12948772828096E+01
 0.12948772828096E+01 0.59419942344484E+01 0.00000000000000E+00 0.35243501439726E+03 0.35243501439726E+03
 0.35243501439726E+03 0.35243501439726E+03 0.00000000000000E+00 0.00000000000000E+00 0.14460275228100E+00
 0.00000000000000E+00 -.14408264023878E+02 0.10000000000000E-02 0.15355582519415E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52098316621237E+01 0.19536868732964E+01 0.36587130734156E+03 0.44078869650700E+03
 0.32019125163256E+03 0.32019125163256E+03 0.29815483907143E+03 0.29815472423940E+03 0.32018327675276E+03
 0.32018327675276E+03 0.29815483931844E+03 0.29815472448054E+03 0.32019125163256E+03 0.32019125163256E+03
 0.29815483907143E+03 0.29815472423940E+03 0.32018327675276E+03 0.32018327675276E+03 0.29815483931844E+03
 0.29815472448054E+03 0.31718140894234E+03 0.29816335972783E+03 -.14511888458286E+03 -.19616800255867E+03
 0.26698568516022E+03 0.57908228718012E+03 0.31076167359410E+03 0.29820726708386E+03 0.24236675901222E+03
 0.29820726708386E+03 0.45270300645778E+03 0.29825348490101E+03 0.24211328923403E+03 0.29825348490101E+03
 0.45251242248636E+03 0.29820726708386E+03 0.24236675901222E+03 0.29820726708386E+03 0.45270300645778E+03
 0.29825348490101E+03 0.24211328923403E+03 0.29825348490101E+03 0.45251242248636E+03 0.22768746016991E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37913686795363E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18764691380235E+00 0.00000000000000E+00 0.00000000000000E+00 0.18764691380235E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19444982027092E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19444982027092E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639267413141E+00 0.19614569993407E+00 0.35243501439726E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1540.15719678
 0.10863493560515E+00 0.32914649529834E+03 0.47033011103385E+03 0.46521762003254E+03 0.46313641225876E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14314615202101E+00 0.00000000000000E+00 -.23453671265950E+02
 0.32604709707725E-02 0.11703153219958E+01 0.24536332547394E+04 0.92011247052728E+03 0.68357645581850E+01
 0.25634117093194E+01 0.36948959372609E+03 0.29821686540226E+03 0.35974016680545E+03 0.39544103368987E+03
 0.29816731035180E+03 0.29818143807540E+03 0.35517198884181E+03 0.39542191971325E+03 0.29816500013891E+03
 0.29818139819297E+03 0.35974016680545E+03 0.39544103368987E+03 0.29816731035180E+03 0.29818143807540E+03
 0.35517198884181E+03 0.39542191971325E+03 0.29816500013891E+03 0.29818139819297E+03 0.44111252304590E+03
 0.36616275003025E+03 0.21577212658632E+04 0.19291827441697E+04 0.50286411998483E+03 0.75471376933652E+03
 0.24933532875176E+03 0.13135012692444E+04 0.11973800056611E+04 0.11462786289062E+04 0.17101332912694E+04
 0.12175780948585E+04 0.11971024393917E+04 0.10813058919223E+04 0.17100159544312E+04 0.13135012692444E+04
 0.11973800056611E+04 0.11462786289062E+04 0.17101332912694E+04 0.12175780948585E+04 0.11971024393917E+04
 0.10813058919223E+04 0.17100159544312E+04 0.16573724691972E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50892132982206E+03 0.12948772956622E+01
 0.12948772956622E+01 0.59806480366861E+01 0.00000000000000E+00 0.35259538751844E+03 0.35259538751844E+03
 0.35259538751844E+03 0.35259538751844E+03 0.00000000000000E+00 0.00000000000000E+00 0.14452977273641E+00
 0.00000000000000E+00 -.14397675608058E+02 0.10000000000000E-02 0.15361743679339E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52077421463292E+01 0.19529033048735E+01 0.36616608284656E+03 0.44110953320164E+03
 0.32030941697806E+03 0.32030941697806E+03 0.29815505319037E+03 0.29815493327720E+03 0.32030138466220E+03
 0.32030138466220E+03 0.29815505343358E+03 0.29815493351464E+03 0.32030941697806E+03 0.32030941697806E+03
 0.29815505319037E+03 0.29815493327720E+03 0.32030138466220E+03 0.32030138466220E+03 0.29815505343358E+03
 0.29815493351464E+03 0.31728630177506E+03 0.29816387957270E+03 -.14666506450383E+03 -.19837631365578E+03
 0.26777580430376E+03 0.58040840519983E+03 0.31129372187455E+03 0.29951501835006E+03 0.24302351222368E+03
 0.29951501835006E+03 0.45365382138044E+03 0.29956164896622E+03 0.24276902765630E+03 0.29956164896622E+03
 0.45346268218524E+03 0.29951501835006E+03 0.24302351222368E+03 0.29951501835006E+03 0.45365382138044E+03
 0.29956164896622E+03 0.24276902765630E+03 0.29956164896622E+03 0.45346268218524E+03 0.22776091168128E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37931256355167E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18757928795704E+00 0.00000000000000E+00 0.00000000000000E+00 0.18757928795704E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19436905003381E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19436905003381E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639268991785E+00 0.19605652204039E+00 0.35259538751844E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1552.91512840
 0.10855303983346E+00 0.32932589300182E+03 0.47063017504497E+03 0.46551717192588E+03 0.46343503841208E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14308430169673E+00 0.00000000000000E+00 -.23463545492022E+02
 0.32629305331427E-02 0.11704041925918E+01 0.24517837320596E+04 0.91941889952236E+03 0.68352455080363E+01
 0.25632170655136E+01 0.36985379062840E+03 0.29822019996006E+03 0.36006593379133E+03 0.39586882474988E+03
 0.29816829969779E+03 0.29818321130367E+03 0.35549020241736E+03 0.39584973988397E+03 0.29816586626648E+03
 0.29818316961379E+03 0.36006593379133E+03 0.39586882474988E+03 0.29816829969779E+03 0.29818321130367E+03
 0.35549020241736E+03 0.39584973988397E+03 0.29816586626648E+03 0.29818316961379E+03 0.44153795634629E+03
 0.36655487130331E+03 0.21602620333233E+04 0.19304339407494E+04 0.50170887398347E+03 0.75224264697471E+03
 0.24802522862132E+03 0.13155382986758E+04 0.11983124247494E+04 0.11473177189802E+04 0.17098405999526E+04
 0.12197909525486E+04 0.11980352707796E+04 0.10826099717338E+04 0.17097233045363E+04 0.13155382986758E+04
 0.11983124247494E+04 0.11473177189802E+04 0.17098405999526E+04 0.12197909525486E+04 0.11980352707796E+04
 0.10826099717338E+04 0.17097233045363E+04 0.16569449571931E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50921975217426E+03 0.12948773684195E+01
 0.12948773684195E+01 0.60316797631510E+01 0.00000000000000E+00 0.35280627810089E+03 0.35280627810089E+03
 0.35280627810089E+03 0.35280627810089E+03 0.00000000000000E+00 0.00000000000000E+00 0.14443541712356E+00
 0.00000000000000E+00 -.14391985505922E+02 0.10000000000000E-02 0.15369545908558E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52050984769467E+01 0.19519119288550E+01 0.36655816591671E+03 0.44153501028962E+03
 0.32046390893315E+03 0.32046390893315E+03 0.29815545666933E+03 0.29815522228806E+03 0.32045580137596E+03
 0.32045580137596E+03 0.29815545691070E+03 0.29815522251906E+03 0.32046390893315E+03 0.32046390893315E+03
 0.29815545666933E+03 0.29815522228806E+03 0.32045580137596E+03 0.32045580137596E+03 0.29815545691070E+03
 0.29815522251906E+03 0.31742339242191E+03 0.29816459317887E+03 -.14875296512523E+03 -.20136017042115E+03
 0.26882350505295E+03 0.58217170298004E+03 0.31200408040182E+03 0.30126494973105E+03 0.24389482883041E+03
 0.30126494973105E+03 0.45492056232185E+03 0.30131212142564E+03 0.24363893668646E+03 0.30131212142564E+03
 0.45472861857899E+03 0.30126494973105E+03 0.24389482883041E+03 0.30126494973105E+03 0.45492056232185E+03
 0.30131212142564E+03 0.24363893668646E+03 0.30131212142564E+03 0.45472861857899E+03 0.22784790326154E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37954444350160E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18755085431802E+00 0.00000000000000E+00 0.00000000000000E+00 0.18755085431802E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19428222495447E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19428222495447E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639243990363E+00 0.19593909047603E+00 0.35280627810089E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1562.45190983
 0.10850918891090E+00 0.32945596755101E+03 0.47085275823187E+03 0.46573847454141E+03 0.46365524269336E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14303895411474E+00 0.00000000000000E+00 -.23473814232688E+02
 0.32642489800171E-02 0.11704628465792E+01 0.24507934440583E+04 0.91904754152186E+03 0.68349029816548E+01
 0.25630886181205E+01 0.37012523296458E+03 0.29822277431245E+03 0.36030889468588E+03 0.39618713605506E+03
 0.29816906840349E+03 0.29818458792208E+03 0.35572766649380E+03 0.39616807255814E+03 0.29816653959116E+03
 0.29818454484510E+03 0.36030889468588E+03 0.39618713605506E+03 0.29816906840349E+03 0.29818458792208E+03
 0.35572766649380E+03 0.39616807255814E+03 0.29816653959116E+03 0.29818454484510E+03 0.44185169977149E+03
 0.36684336418637E+03 0.21621214450915E+04 0.19313028136219E+04 0.50087748028064E+03 0.75045419820043E+03
 0.24707233051838E+03 0.13170397333385E+04 0.11989871352083E+04 0.11480524499786E+04 0.17095995886418E+04
 0.12214221286426E+04 0.11987102953191E+04 0.10835404160812E+04 0.17094823339261E+04 0.13170397333385E+04
 0.11989871352083E+04 0.11480524499786E+04 0.17095995886418E+04 0.12214221286426E+04 0.11987102953191E+04
 0.10835404160812E+04 0.17094823339261E+04 0.16566076959596E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50944002014350E+03 0.12948774440837E+01
 0.12948774440837E+01 0.60698268888700E+01 0.00000000000000E+00 0.35296415132908E+03 0.35296415132908E+03
 0.35296415132908E+03 0.35296415132908E+03 0.00000000000000E+00 0.00000000000000E+00 0.14436631684239E+00
 0.00000000000000E+00 -.14392046971056E+02 0.10000000000000E-02 0.15375104064117E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52032168150789E+01 0.19512063056546E+01 0.36684663631180E+03 0.44184878030781E+03
 0.32057957465119E+03 0.32057957465119E+03 0.29815569162400E+03 0.29815544715055E+03 0.32057141059238E+03
 0.32057141059238E+03 0.29815569185919E+03 0.29815544737564E+03 0.32057957465119E+03 0.32057957465119E+03
 0.29815569162400E+03 0.29815544715055E+03 0.32057141059238E+03 0.32057141059238E+03 0.29815569185919E+03
 0.29815544737564E+03 0.31752606873115E+03 0.29816514495764E+03 -.15027152005855E+03 -.20352584468263E+03
 0.26960716220238E+03 0.58349449551169E+03 0.31253929749830E+03 0.30255578268538E+03 0.24454701027046E+03
 0.30255578268538E+03 0.45587262992422E+03 0.30260336209153E+03 0.24429010032074E+03 0.30260336209153E+03
 0.45568012228530E+03 0.30255578268538E+03 0.24454701027046E+03 0.30255578268538E+03 0.45587262992422E+03
 0.30260336209153E+03 0.24429010032074E+03 0.30260336209153E+03 0.45568012228531E+03 0.22793116425957E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37972009436406E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18755352879650E+00 0.00000000000000E+00 0.00000000000000E+00 0.18755352879650E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19426228486261E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19426228486261E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639211073817E+00 0.19585112278259E+00 0.35296415132908E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1570.55764184
 0.10843659130776E+00 0.32958621314717E+03 0.47104359463470E+03 0.46593054254675E+03 0.46384767075549E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14300086155220E+00 0.00000000000000E+00 -.23455358725826E+02
 0.32664342189144E-02 0.11705027985914E+01 0.24491538674422E+04 0.91843270029084E+03 0.68346696903479E+01
 0.25630011338805E+01 0.37035665940521E+03 0.29822498755054E+03 0.36051641866022E+03 0.39645586355947E+03
 0.29816973077290E+03 0.29818577375850E+03 0.35593096549254E+03 0.39643681798122E+03 0.29816711988230E+03
 0.29818572949152E+03 0.36051641866022E+03 0.39645586355947E+03 0.29816973077290E+03 0.29818577375850E+03
 0.35593096549254E+03 0.39643681798122E+03 0.29816711988230E+03 0.29818572949152E+03 0.44210900131317E+03
 0.36707644852932E+03 0.21637482484443E+04 0.19322380176458E+04 0.50036341996263E+03 0.74923898589349E+03
 0.24637374883104E+03 0.13183364957899E+04 0.11996114675156E+04 0.11488207108381E+04 0.17094740806117E+04
 0.12228267858537E+04 0.11993349426392E+04 0.10844651346280E+04 0.17093569123724E+04 0.13183364957899E+04
 0.11996114675156E+04 0.11488207108381E+04 0.17094740806117E+04 0.12228267858537E+04 0.11993349426392E+04
 0.10844651346280E+04 0.17093569123724E+04 0.16564944772550E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50963180195559E+03 0.12948773080961E+01
 0.12948773080961E+01 0.61022498169121E+01 0.00000000000000E+00 0.35309687077467E+03 0.35309687077467E+03
 0.35309687077467E+03 0.35309687077467E+03 0.00000000000000E+00 0.00000000000000E+00 0.14430854852766E+00
 0.00000000000000E+00 -.14361094061548E+02 0.10000000000000E-02 0.15379719689634E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52016552716446E+01 0.19506207268667E+01 0.36707975063014E+03 0.44210606497634E+03
 0.32067973441516E+03 0.32067973441516E+03 0.29815568814371E+03 0.29815564100064E+03 0.32067152188861E+03
 0.32067152188861E+03 0.29815568836527E+03 0.29815564122036E+03 0.32067973441516E+03 0.32067973441516E+03
 0.29815568814371E+03 0.29815564100064E+03 0.32067152188861E+03 0.32067152188861E+03 0.29815568836527E+03
 0.29815564122036E+03 0.31761516682441E+03 0.29816561960012E+03 -.15145496846512E+03 -.20520665532290E+03
 0.27024616899591E+03 0.58454533139262E+03 0.31294793155173E+03 0.30358231401801E+03 0.24507556182792E+03
 0.30358231401801E+03 0.45661890071973E+03 0.30363024629477E+03 0.24481791394249E+03 0.30363024629477E+03
 0.45642604016230E+03 0.30358231401801E+03 0.24507556182792E+03 0.30358231401801E+03 0.45661890071973E+03
 0.30363024629477E+03 0.24481791394249E+03 0.30363024629477E+03 0.45642604016230E+03 0.22800380920576E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37986159846931E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18733975423081E+00 0.00000000000000E+00 0.00000000000000E+00 0.18733975423081E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19411936870000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19411936870000E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639285560143E+00 0.19577831007615E+00 0.35309687077467E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1582.93744835
 0.10833915096921E+00 0.32976576210164E+03 0.47133291525799E+03 0.46622049353196E+03 0.46413730577213E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14294306609254E+00 0.00000000000000E+00 -.23463367934790E+02
 0.32693718214572E-02 0.11705635088213E+01 0.24469532487848E+04 0.91760746829428E+03 0.68343152163147E+01
 0.25628682061180E+01 0.37070818193232E+03 0.29822846838356E+03 0.36083140530239E+03 0.39686614208956E+03
 0.29817077854085E+03 0.29818764814307E+03 0.35623912316513E+03 0.39684712360979E+03 0.29816803825557E+03
 0.29818760201476E+03 0.36083140530239E+03 0.39686614208956E+03 0.29817077854085E+03 0.29818764814307E+03
 0.35623912316513E+03 0.39684712360979E+03 0.29816803825557E+03 0.29818760201476E+03 0.44251020940350E+03
 0.36744211656108E+03 0.21662043894640E+04 0.19334981869031E+04 0.49938379784637E+03 0.74708519372622E+03
 0.24520447689062E+03 0.13203034911317E+04 0.12005213315544E+04 0.11498586766314E+04 0.17092238896950E+04
 0.12249614393529E+04 0.12002452310474E+04 0.10857526038152E+04 0.17091067985724E+04 0.13203034911317E+04
 0.12005213315544E+04 0.11498586766314E+04 0.17092238896950E+04 0.12249614393529E+04 0.12002452310474E+04
 0.10857526038152E+04 0.17091067985724E+04 0.16561714118897E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50992093088919E+03 0.12948773671112E+01
 0.12948773671112E+01 0.61517690429460E+01 0.00000000000000E+00 0.35329848086724E+03 0.35329848086724E+03
 0.35329848086724E+03 0.35329848086724E+03 0.00000000000000E+00 0.00000000000000E+00 0.14422194396181E+00
 0.00000000000000E+00 -.14353826707460E+02 0.10000000000000E-02 0.15386512126025E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51993589804337E+01 0.19497596176627E+01 0.36744540297375E+03 0.44250730351598E+03
 0.32083025541265E+03 0.32083025541265E+03 0.29815609941148E+03 0.29815594802099E+03 0.32082196982359E+03
 0.32082196982359E+03 0.29815609962693E+03 0.29815594823109E+03 0.32083025541265E+03 0.32083025541265E+03
 0.29815609941148E+03 0.29815594802099E+03 0.32082196982359E+03 0.32082196982359E+03 0.29815609962693E+03
 0.29815594823109E+03 0.31774893543860E+03 0.29816636714709E+03 -.15337876367876E+03 -.20794674218480E+03
 0.27123225610047E+03 0.58618595880343E+03 0.31359754142246E+03 0.30520751292882E+03 0.24589175835198E+03
 0.30520751292882E+03 0.45779017337471E+03 0.30525597623059E+03 0.24563283760625E+03 0.30525597623059E+03
 0.45759662416317E+03 0.30520751292882E+03 0.24589175835198E+03 0.30520751292882E+03 0.45779017337471E+03
 0.30525597623059E+03 0.24563283760625E+03 0.30525597623059E+03 0.45759662416317E+03 0.22809123809550E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38008322964171E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18730005405558E+00 0.00000000000000E+00 0.00000000000000E+00 0.18730005405558E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19402747193830E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19402747193830E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639267797180E+00 0.19566642607716E+00 0.35329848086724E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1593.37149696
 0.10827837782183E+00 0.32991093881926E+03 0.47157490593949E+03 0.46646185775430E+03 0.46437785345810E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14289499955537E+00 0.00000000000000E+00 -.23473065476507E+02
 0.32712066229667E-02 0.11706105177292E+01 0.24455807663854E+04 0.91709278739454E+03 0.68340407666236E+01
 0.25627652874839E+01 0.37100328831974E+03 0.29823149524274E+03 0.36109590024260E+03 0.39721043707420E+03
 0.29817169530270E+03 0.29818928681402E+03 0.35649794495269E+03 0.39719144102421E+03 0.29816884221488E+03
 0.29818923907677E+03 0.36109590024260E+03 0.39721043707420E+03 0.29817169530270E+03 0.29818928681402E+03
 0.35649794495269E+03 0.39719144102422E+03 0.29816884221488E+03 0.29818923907677E+03 0.44284584739484E+03
 0.36774770965043E+03 0.21682336273417E+04 0.19344786844088E+04 0.49855706515317E+03 0.74527618364864E+03
 0.24422633316970E+03 0.13219403867315E+04 0.12012639478403E+04 0.11506807060015E+04 0.17089881239278E+04
 0.12267382825385E+04 0.12009882024853E+04 0.10867837016909E+04 0.17088710992748E+04 0.13219403867315E+04
 0.12012639478403E+04 0.11506807060015E+04 0.17089881239278E+04 0.12267382825385E+04 0.12009882024853E+04
 0.10867837016908E+04 0.17088710992748E+04 0.16558629530472E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51016134563831E+03 0.12948774385666E+01
 0.12948774385666E+01 0.61935052373969E+01 0.00000000000000E+00 0.35346849903550E+03 0.35346849903550E+03
 0.35346849903550E+03 0.35346849903550E+03 0.00000000000000E+00 0.00000000000000E+00 0.14415040823530E+00
 0.00000000000000E+00 -.14351862623528E+02 0.10000000000000E-02 0.15391964653994E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51975171330219E+01 0.19490689248832E+01 0.36775095967735E+03 0.44284298202986E+03
 0.32095687614263E+03 0.32095687614263E+03 0.29815637524824E+03 0.29815621701124E+03 0.32094852900348E+03
 0.32094852900348E+03 0.29815637545397E+03 0.29815621721186E+03 0.32095687614263E+03 0.32095687614263E+03
 0.29815637524824E+03 0.29815621701124E+03 0.32094852900348E+03 0.32094852900348E+03 0.29815637545397E+03
 0.29815621721186E+03 0.31786148911367E+03 0.29816701819790E+03 -.15497959232456E+03 -.21022352782089E+03
 0.27206596347282E+03 0.58757755822572E+03 0.31415126493554E+03 0.30657106600953E+03 0.24658253916115E+03
 0.30657106600953E+03 0.45878607232529E+03 0.30661997783419E+03 0.24632255403550E+03 0.30661997783419E+03
 0.45859195196047E+03 0.30657106600953E+03 0.24658253916115E+03 0.30657106600953E+03 0.45878607232529E+03
 0.30661997783419E+03 0.24632255403550E+03 0.30661997783419E+03 0.45859195196047E+03 0.22817749455097E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38027175385269E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18729123073428E+00 0.00000000000000E+00 0.00000000000000E+00 0.18729123073428E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19398658650625E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19398658650625E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639239135890E+00 0.19557202900289E+00 0.35346849903550E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1600.32752937
 0.10823033663005E+00 0.33001185832056E+03 0.47173562822770E+03 0.46662269111919E+03 0.46453844487430E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14286336360500E+00 0.00000000000000E+00 -.23463403860219E+02
 0.32726585153724E-02 0.11706380253233E+01 0.24444958013255E+04 0.91668592549705E+03 0.68338801806736E+01
 0.25627050677526E+01 0.37119951081529E+03 0.29823355869798E+03 0.36127182862214E+03 0.39743915813074E+03
 0.29817232304378E+03 0.29819040820306E+03 0.35667013717537E+03 0.39742017683050E+03 0.29816939291925E+03
 0.29819035937371E+03 0.36127182862214E+03 0.39743915813074E+03 0.29817232304378E+03 0.29819040820306E+03
 0.35667013717537E+03 0.39742017683050E+03 0.29816939291925E+03 0.29819035937371E+03 0.44306827462363E+03
 0.36794993774845E+03 0.21695847040035E+04 0.19351669745869E+04 0.49801346338504E+03 0.74408601404905E+03
 0.24358248334708E+03 0.13230293270513E+04 0.12017543300195E+04 0.11512562670056E+04 0.17088286926269E+04
 0.12279204650232E+04 0.12014788243237E+04 0.10874971500397E+04 0.17087117172472E+04 0.13230293270513E+04
 0.12017543300195E+04 0.11512562670056E+04 0.17088286926270E+04 0.12279204650232E+04 0.12014788243237E+04
 0.10874971500397E+04 0.17087117172472E+04 0.16556695676609E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51032168645637E+03 0.12948773673759E+01
 0.12948773673759E+01 0.62213293670308E+01 0.00000000000000E+00 0.35358102837865E+03 0.35358102837865E+03
 0.35358102837865E+03 0.35358102837865E+03 0.00000000000000E+00 0.00000000000000E+00 0.14410344851373E+00
 0.00000000000000E+00 -.14332089158960E+02 0.10000000000000E-02 0.15395503972970E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51963222600867E+01 0.19486208475325E+01 0.36795317196310E+03 0.44306542841201E+03
 0.32104121498076E+03 0.32104121498076E+03 0.29815656430541E+03 0.29815640137584E+03 0.32103282687087E+03
 0.32103282687087E+03 0.29815656450395E+03 0.29815640156945E+03 0.32104121498076E+03 0.32104121498076E+03
 0.29815656430541E+03 0.29815640137584E+03 0.32103282687087E+03 0.32103282687087E+03 0.29815656450395E+03
 0.29815640156945E+03 0.31793647826500E+03 0.29816746252246E+03 -.15604188293145E+03 -.21173400081052E+03
 0.27261545676347E+03 0.58848316643127E+03 0.31450463238398E+03 0.30747187833804E+03 0.24703688908294E+03
 0.30747187833804E+03 0.45943060286144E+03 0.30752108948946E+03 0.24677620053658E+03 0.30752108948946E+03
 0.45923610519668E+03 0.30747187833804E+03 0.24703688908294E+03 0.30747187833804E+03 0.45943060286144E+03
 0.30752108948946E+03 0.24677620053658E+03 0.30752108948946E+03 0.45923610519668E+03 0.22822564088764E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38039217941356E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18715809919601E+00 0.00000000000000E+00 0.00000000000000E+00 0.18715809919601E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19387762730516E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19387762730516E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639281040099E+00 0.19551024232945E+00 0.35358102837865E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1610.76157798
 0.10814941974234E+00 0.33016326797811E+03 0.47197612662349E+03 0.46686380049865E+03 0.46477937289350E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14281628034210E+00 0.00000000000000E+00 -.23465602196986E+02
 0.32751069105480E-02 0.11706755304918E+01 0.24426683520574E+04 0.91600063202151E+03 0.68336612422739E+01
 0.25626229658527E+01 0.37149322738651E+03 0.29823672305022E+03 0.36153525866257E+03 0.39778112486603E+03
 0.29817328993811E+03 0.29819213441658E+03 0.35692802443590E+03 0.39776216542980E+03 0.29817024146797E+03
 0.29819208391976E+03 0.36153525866257E+03 0.39778112486603E+03 0.29817328993811E+03 0.29819213441658E+03
 0.35692802443590E+03 0.39776216542980E+03 0.29817024146797E+03 0.29819208391976E+03 0.44340019094870E+03
 0.36825114100266E+03 0.21716119406025E+04 0.19362044649991E+04 0.49721146353113E+03 0.74232673657528E+03
 0.24262921572650E+03 0.13246615601835E+04 0.12024884487276E+04 0.11521187377047E+04 0.17085933077858E+04
 0.12296924831365E+04 0.12022133064169E+04 0.10885662335726E+04 0.17084764129706E+04 0.13246615601835E+04
 0.12024884487276E+04 0.11521187377047E+04 0.17085933077858E+04 0.12296924831365E+04 0.12022133064169E+04
 0.10885662335725E+04 0.17084764129706E+04 0.16553900959755E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51056214596791E+03 0.12948773835741E+01
 0.12948773835741E+01 0.62630655614818E+01 0.00000000000000E+00 0.35374877526792E+03 0.35374877526792E+03
 0.35374877526792E+03 0.35374877526792E+03 0.00000000000000E+00 0.00000000000000E+00 0.14403407744869E+00
 0.00000000000000E+00 -.14321368223414E+02 0.10000000000000E-02 0.15400659799236E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51945826375548E+01 0.19479684890831E+01 0.36825435635090E+03 0.44339736992503E+03
 0.32116750529764E+03 0.32116750529764E+03 0.29815685578536E+03 0.29815668562099E+03 0.32115905582183E+03
 0.32115905582183E+03 0.29815685597200E+03 0.29815668580300E+03 0.32116750529764E+03 0.32116750529764E+03
 0.29815685578536E+03 0.29815668562099E+03 0.32115905582183E+03 0.32115905582183E+03 0.29815685597200E+03
 0.29815668580300E+03 0.31804880178093E+03 0.29816814466245E+03 -.15762747441720E+03 -.21398774331385E+03
 0.27343227688966E+03 0.58983272048583E+03 0.31503328221173E+03 0.30881264739982E+03 0.24771130367604E+03
 0.30881264739982E+03 0.46039131169752E+03 0.30886230792999E+03 0.24744956707410E+03 0.30886230792999E+03
 0.46019625607504E+03 0.30881264739982E+03 0.24771130367604E+03 0.30881264739982E+03 0.46039131169752E+03
 0.30886230792999E+03 0.24744956707410E+03 0.30886230792999E+03 0.46019625607504E+03 0.22829678135583E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38057607423850E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18709005443415E+00 0.00000000000000E+00 0.00000000000000E+00 0.18709005443415E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19379273750816E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19379273750816E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639281750164E+00 0.19541755955568E+00 0.35374877526792E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1621.19562659
 0.10807154877256E+00 0.33031241720143E+03 0.47221582239326E+03 0.46710391546820E+03 0.46501919663005E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14276959584402E+00 0.00000000000000E+00 -.23467495722599E+02
 0.32774665979056E-02 0.11707096365780E+01 0.24409096968714E+04 0.91534113632679E+03 0.68334621583743E+01
 0.25625483093903E+01 0.37178610977122E+03 0.29823997141440E+03 0.36179801392274E+03 0.39812178851142E+03
 0.29817428769208E+03 0.29819391444684E+03 0.35718531287655E+03 0.39810285062943E+03 0.29817111748234E+03
 0.29819386224723E+03 0.36179801392274E+03 0.39812178851142E+03 0.29817428769208E+03 0.29819391444684E+03
 0.35718531287655E+03 0.39810285062943E+03 0.29817111748234E+03 0.29819386224723E+03 0.44373011436849E+03
 0.36854989501878E+03 0.21736289730280E+04 0.19372194966603E+04 0.49642206616975E+03 0.74059294776490E+03
 0.24168877126430E+03 0.13262880166857E+04 0.12032182015409E+04 0.11529641059206E+04 0.17083590273974E+04
 0.12314580670371E+04 0.12029434237479E+04 0.10896174306566E+04 0.17082422175792E+04 0.13262880166857E+04
 0.12032182015409E+04 0.11529641059206E+04 0.17083590273974E+04 0.12314580670371E+04 0.12029434237479E+04
 0.10896174306566E+04 0.17082422175792E+04 0.16551112229162E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51080156171279E+03 0.12948773975264E+01
 0.12948773975264E+01 0.63048017559327E+01 0.00000000000000E+00 0.35391594931998E+03 0.35391594931998E+03
 0.35391594931998E+03 0.35391594931998E+03 0.00000000000000E+00 0.00000000000000E+00 0.14396593825157E+00
 0.00000000000000E+00 -.14310407781196E+02 0.10000000000000E-02 0.15405614883919E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51929118443373E+01 0.19473419416265E+01 0.36855309301396E+03 0.44372731802932E+03
 0.32129358109872E+03 0.32129358109872E+03 0.29815715691332E+03 0.29815697927467E+03 0.32128507037003E+03
 0.32128507037003E+03 0.29815715708668E+03 0.29815697944372E+03 0.32129358109872E+03 0.32129358109872E+03
 0.29815715691332E+03 0.29815697927467E+03 0.32128507037003E+03 0.32128507037003E+03 0.29815715708668E+03
 0.29815697944372E+03 0.31816096711431E+03 0.29816884584634E+03 -.15919897195187E+03 -.21621936517869E+03
 0.27424570578914E+03 0.59117462615968E+03 0.31555769184160E+03 0.31014413429372E+03 0.24838260023439E+03
 0.31014413429372E+03 0.46134618793759E+03 0.31019424457846E+03 0.24811982248249E+03 0.31019424457846E+03
 0.46115058030653E+03 0.31014413429372E+03 0.24838260023439E+03 0.31014413429372E+03 0.46134618793759E+03
 0.31019424457846E+03 0.24811982248249E+03 0.31019424457846E+03 0.46115058030653E+03 0.22836964610099E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38075933413881E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18702006505874E+00 0.00000000000000E+00 0.00000000000000E+00 0.18702006505874E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19370802182895E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19370802182895E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639283400021E+00 0.19532529059579E+00 0.35391594931998E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1633.59298259
 0.10798299643673E+00 0.33048788395625E+03 0.47249922734981E+03 0.46738762388727E+03 0.46530246818564E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14271469928608E+00 0.00000000000000E+00 -.23471349432778E+02
 0.32801540852947E-02 0.11707458671189E+01 0.24389098170311E+04 0.91459118138667E+03 0.68332506863227E+01
 0.25624690073710E+01 0.37213278973351E+03 0.29824394602968E+03 0.36210908981570E+03 0.39852494804193E+03
 0.29817551569723E+03 0.29819610346696E+03 0.35748993195866E+03 0.39850603535546E+03 0.29817219618627E+03
 0.29819604919628E+03 0.36210908981570E+03 0.39852494804193E+03 0.29817551569723E+03 0.29819610346696E+03
 0.35748993195866E+03 0.39850603535546E+03 0.29817219618627E+03 0.29819604919628E+03 0.44412060913016E+03
 0.36890321649610E+03 0.21760081116078E+04 0.19384033222190E+04 0.49547438701393E+03 0.73852572162614E+03
 0.24057396267714E+03 0.13282106410297E+04 0.12040716723760E+04 0.11539541911917E+04 0.17080707293289E+04
 0.12335456343679E+04 0.12037973234295E+04 0.10908514814818E+04 0.17079540204456E+04 0.13282106410297E+04
 0.12040716723760E+04 0.11539541911917E+04 0.17080707293289E+04 0.12335456343679E+04 0.12037973234296E+04
 0.10908514814818E+04 0.17079540204456E+04 0.16547662517873E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51108439875204E+03 0.12948774259221E+01
 0.12948774259221E+01 0.63543911799030E+01 0.00000000000000E+00 0.35411374476338E+03 0.35411374476338E+03
 0.35411374476338E+03 0.35411374476338E+03 0.00000000000000E+00 0.00000000000000E+00 0.14388651763218E+00
 0.00000000000000E+00 -.14299136808209E+02 0.10000000000000E-02 0.15411256428139E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51910108934355E+01 0.19466290850383E+01 0.36890638988518E+03 0.44411784514801E+03
 0.32144283608344E+03 0.32144283608344E+03 0.29815758338856E+03 0.29815734116484E+03 0.32143425282772E+03
 0.32143425282772E+03 0.29815758354531E+03 0.29815734131658E+03 0.32144283608344E+03 0.32144283608344E+03
 0.29815758338856E+03 0.29815734116484E+03 0.32143425282772E+03 0.32143425282772E+03 0.29815758354531E+03
 0.29815734131658E+03 0.31829377411872E+03 0.29816970509634E+03 -.16106438106409E+03 -.21886696323841E+03
 0.27520916698441E+03 0.59276245809198E+03 0.31617724527265E+03 0.31172225097356E+03 0.24917735510363E+03
 0.31172225097356E+03 0.46247590792700E+03 0.31177289507336E+03 0.24891333119742E+03 0.31177289507336E+03
 0.46227963338530E+03 0.31172225097356E+03 0.24917735510363E+03 0.31172225097356E+03 0.46247590792701E+03
 0.31177289507336E+03 0.24891333119742E+03 0.31177289507336E+03 0.46227963338530E+03 0.22845413819795E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38097621393246E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18695025618326E+00 0.00000000000000E+00 0.00000000000000E+00 0.18695025618326E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19360949033958E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19360949033958E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639279803494E+00 0.19521617475606E+00 0.35411374476338E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1644.47562975
 0.10791473113841E+00 0.33063913804004E+03 0.47274650465820E+03 0.46763467857107E+03 0.46554891426142E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14266711589134E+00 0.00000000000000E+00 -.23476991122255E+02
 0.32822288674986E-02 0.11707731971694E+01 0.24373681187250E+04 0.91401304452189E+03 0.68330911737147E+01
 0.25624091901430E+01 0.37243606321520E+03 0.29824754052093E+03 0.36238132376293E+03 0.39887733081430E+03
 0.29817663289990E+03 0.29819809330072E+03 0.35775658540806E+03 0.39885843989574E+03 0.29817317805328E+03
 0.29819803716861E+03 0.36238132376293E+03 0.39887733081430E+03 0.29817663289990E+03 0.29819809330072E+03
 0.35775658540806E+03 0.39885843989574E+03 0.29817317805328E+03 0.29819803716861E+03 0.44446073038643E+03
 0.36921045199549E+03 0.21780697982165E+04 0.19394008583572E+04 0.49465182485496E+03 0.73673307937884E+03
 0.23960799539961E+03 0.13298836912771E+04 0.12048040506059E+04 0.11547965009417E+04 0.17078029269308E+04
 0.12353625206311E+04 0.12045300795101E+04 0.10919065866634E+04 0.17076863116059E+04 0.13298836912771E+04
 0.12048040506059E+04 0.11547965009417E+04 0.17078029269308E+04 0.12353625206311E+04 0.12045300795101E+04
 0.10919065866634E+04 0.17076863116059E+04 0.16544484724871E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51133058435295E+03 0.12948774674924E+01
 0.12948774674924E+01 0.63979217685757E+01 0.00000000000000E+00 0.35428697961856E+03 0.35428697961856E+03
 0.35428697961856E+03 0.35428697961856E+03 0.00000000000000E+00 0.00000000000000E+00 0.14381812564875E+00
 0.00000000000000E+00 -.14292304265415E+02 0.10000000000000E-02 0.15415982905697E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51894193506428E+01 0.19460322564910E+01 0.36921360171516E+03 0.44445799556071E+03
 0.32157363322863E+03 0.32157363322863E+03 0.29815792394294E+03 0.29815767084126E+03 0.32156498634710E+03
 0.32156498634710E+03 0.29815792408210E+03 0.29815767097597E+03 0.32157363322863E+03 0.32157363322863E+03
 0.29815792394294E+03 0.29815767084126E+03 0.32156498634710E+03 0.32156498634710E+03 0.29815792408210E+03
 0.29815767097597E+03 0.31841018784887E+03 0.29817048338025E+03 -.16268192861763E+03 -.22115980444522E+03
 0.27605316718909E+03 0.59415442684541E+03 0.31672099382038E+03 0.31309745008087E+03 0.24987357673902E+03
 0.31309745008087E+03 0.46346705661112E+03 0.31314856381751E+03 0.24960847107758E+03 0.31314856381751E+03
 0.46327020906827E+03 0.31309745008087E+03 0.24987357673902E+03 0.31309745008087E+03 0.46346705661112E+03
 0.31314856381751E+03 0.24960847107758E+03 0.31314856381751E+03 0.46327020906827E+03 0.22853520465424E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38116730370635E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18690770332896E+00 0.00000000000000E+00 0.00000000000000E+00 0.18690770332896E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19354727997212E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19354727997212E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639266680156E+00 0.19512060315145E+00 0.35428697961856E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1655.35827692
 0.10785380260468E+00 0.33079091601396E+03 0.47299267843870E+03 0.46788034483384E+03 0.46579389019363E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14262023747969E+00 0.00000000000000E+00 -.23481339460095E+02
 0.32840828627225E-02 0.11707937893130E+01 0.24359921276067E+04 0.91349704785249E+03 0.68329709920088E+01
 0.25623641220033E+01 0.37273838060837E+03 0.29825123056073E+03 0.36265280495134E+03 0.39922819202795E+03
 0.29817778585697E+03 0.29820014527522E+03 0.35802258236804E+03 0.39920932254093E+03 0.29817419179564E+03
 0.29820008724278E+03 0.36265280495134E+03 0.39922819202795E+03 0.29817778585697E+03 0.29820014527522E+03
 0.35802258236804E+03 0.39920932254093E+03 0.29817419179564E+03 0.29820008724278E+03 0.44479817115639E+03
 0.36951467206211E+03 0.21801126269552E+04 0.19403902953864E+04 0.49384725081087E+03 0.73497520505625E+03
 0.23865871799132E+03 0.13315462473264E+04 0.12055277404567E+04 0.11556387981443E+04 0.17075316330200E+04
 0.12371679144632E+04 0.12052541459954E+04 0.10929592576965E+04 0.17074151136232E+04 0.13315462473264E+04
 0.12055277404567E+04 0.11556387981443E+04 0.17075316330200E+04 0.12371679144632E+04 0.12052541459954E+04
 0.10929592576965E+04 0.17074151136232E+04 0.16541341743109E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51157534103278E+03 0.12948774995327E+01
 0.12948774995327E+01 0.64414523572484E+01 0.00000000000000E+00 0.35445998156292E+03 0.35445998156292E+03
 0.35445998156292E+03 0.35445998156292E+03 0.00000000000000E+00 0.00000000000000E+00 0.14375093253265E+00
 0.00000000000000E+00 -.14284079585220E+02 0.10000000000000E-02 0.15420500540244E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51878990433039E+01 0.19454621412389E+01 0.36951780407250E+03 0.44479546032070E+03
 0.32170434289897E+03 0.32170434289897E+03 0.29815827581441E+03 0.29815801147326E+03 0.32169563249464E+03
 0.32169563249464E+03 0.29815827593420E+03 0.29815801158922E+03 0.32170434289897E+03 0.32170434289897E+03
 0.29815827581441E+03 0.29815801147326E+03 0.32169563249464E+03 0.32169563249464E+03 0.29815827593420E+03
 0.29815801158922E+03 0.31852656131855E+03 0.29817128346467E+03 -.16427672800917E+03 -.22341701575224E+03
 0.27689573756330E+03 0.59554223724519E+03 0.31726202099407E+03 0.31446155623454E+03 0.25056863092331E+03
 0.31446155623454E+03 0.46445528584633E+03 0.31451314024016E+03 0.25030245555366E+03 0.31451314024016E+03
 0.46425787638600E+03 0.31446155623454E+03 0.25056863092331E+03 0.31446155623454E+03 0.46445528584633E+03
 0.31451314024016E+03 0.25030245555366E+03 0.31451314024016E+03 0.46425787638600E+03 0.22862294977697E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38135765779794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18685554020495E+00 0.00000000000000E+00 0.00000000000000E+00 0.18685554020495E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19347899727819E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19347899727819E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639258225458E+00 0.19502530215451E+00 0.35445998156292E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1660.79960051
 0.10782434782123E+00 0.33086701552315E+03 0.47311534704048E+03 0.46800273584898E+03 0.46591593783284E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14259708412768E+00 0.00000000000000E+00 -.23483304199426E+02
 0.32849798912742E-02 0.11708012816707E+01 0.24353269319091E+04 0.91324759946593E+03 0.68329272654915E+01
 0.25623477245593E+01 0.37288918958012E+03 0.29825311180173E+03 0.36278827464218E+03 0.39940307752857E+03
 0.29817837595202E+03 0.29820119490740E+03 0.35815534019561E+03 0.39938421863447E+03 0.29817471081117E+03
 0.29820113591017E+03 0.36278827464218E+03 0.39940307752857E+03 0.29817837595202E+03 0.29820119490740E+03
 0.35815534019561E+03 0.39938421863447E+03 0.29817471081117E+03 0.29820113591017E+03 0.44496601424362E+03
 0.36966579938644E+03 0.21811280866112E+04 0.19408831500698E+04 0.49344941801181E+03 0.73410572973881E+03
 0.23818906463694E+03 0.13323739873992E+04 0.12058859668167E+04 0.11560598597318E+04 0.17073940170633E+04
 0.12380668213838E+04 0.12056125598539E+04 0.10934848434336E+04 0.17072775460978E+04 0.13323739873992E+04
 0.12058859668167E+04 0.11560598597318E+04 0.17073940170633E+04 0.12380668213838E+04 0.12056125598539E+04
 0.10934848434336E+04 0.17072775460978E+04 0.16539775750179E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51169727795357E+03 0.12948775140097E+01
 0.12948775140097E+01 0.64632176515848E+01 0.00000000000000E+00 0.35454628959643E+03 0.35454628959643E+03
 0.35454628959643E+03 0.35454628959643E+03 0.00000000000000E+00 0.00000000000000E+00 0.14371777749904E+00
 0.00000000000000E+00 -.14279756580252E+02 0.10000000000000E-02 0.15422687126180E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51871635173223E+01 0.19451863189959E+01 0.36966892283503E+03 0.44496331515720E+03
 0.32176964488372E+03 0.32176964488372E+03 0.29815845606558E+03 0.29815818596685E+03 0.32176090276448E+03
 0.32176090276448E+03 0.29815845617500E+03 0.29815818607277E+03 0.32176964488372E+03 0.32176964488372E+03
 0.29815845606558E+03 0.29815818596685E+03 0.32176090276448E+03 0.32176090276448E+03 0.29815845617500E+03
 0.29815818607277E+03 0.31858471349016E+03 0.29817169178488E+03 -.16506792291055E+03 -.22453587306809E+03
 0.27731577993707E+03 0.59623314927168E+03 0.31753079043492E+03 0.31513974608379E+03 0.25091500029059E+03
 0.31513974608379E+03 0.46494707748428E+03 0.31519156538849E+03 0.25064829341578E+03 0.31519156538849E+03
 0.46474938999340E+03 0.31513974608379E+03 0.25091500029059E+03 0.31513974608379E+03 0.46494707748428E+03
 0.31519156538849E+03 0.25064829341578E+03 0.31519156538849E+03 0.46474938999340E+03 0.22866774703524E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38145254494113E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18682798797039E+00 0.00000000000000E+00 0.00000000000000E+00 0.18682798797039E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19344400748929E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19344400748929E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639254744695E+00 0.19497780045436E+00 0.35454628959643E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1671.68224768
 0.10776642369526E+00 0.33101904283976E+03 0.47335985172130E+03 0.46824666508161E+03 0.46615918169530E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14255135165895E+00 0.00000000000000E+00 -.23487453378486E+02
 0.32867453652633E-02 0.11708106149702E+01 0.24340187969990E+04 0.91275704887461E+03 0.68328727957457E+01
 0.25623272984046E+01 0.37319012231522E+03 0.29825694750079E+03 0.36305868376722E+03 0.39975178241165E+03
 0.29817958379251E+03 0.29820334214477E+03 0.35842038309895E+03 0.39973294446340E+03 0.29817577351342E+03
 0.29820328118863E+03 0.36305868376722E+03 0.39975178241165E+03 0.29817958379251E+03 0.29820334214477E+03
 0.35842038309895E+03 0.39973294446340E+03 0.29817577351342E+03 0.29820328118863E+03 0.44530001039849E+03
 0.36996616617623E+03 0.21831477888863E+04 0.19418609968278E+04 0.49266172092940E+03 0.73238414695646E+03
 0.23725911742242E+03 0.13340226815950E+04 0.12065950111933E+04 0.11568974532658E+04 0.17071144504978E+04
 0.12398574118521E+04 0.12063219776498E+04 0.10945304940367E+04 0.17069980773122E+04 0.13340226815950E+04
 0.12065950111933E+04 0.11568974532658E+04 0.17071144504978E+04 0.12398574118521E+04 0.12063219776498E+04
 0.10945304940367E+04 0.17069980773122E+04 0.16536639097903E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51194030044515E+03 0.12948775445825E+01
 0.12948775445825E+01 0.65067482402575E+01 0.00000000000000E+00 0.35471844507880E+03 0.35471844507880E+03
 0.35471844507880E+03 0.35471844507880E+03 0.00000000000000E+00 0.00000000000000E+00 0.14365233573710E+00
 0.00000000000000E+00 -.14271425888153E+02 0.10000000000000E-02 0.15426921796763E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51857396474771E+01 0.19446523678039E+01 0.36996927151688E+03 0.44529733563048E+03
 0.32190012623615E+03 0.32190012623615E+03 0.29815882534230E+03 0.29815854344812E+03 0.32189132077797E+03
 0.32189132077797E+03 0.29815882542954E+03 0.29815854353257E+03 0.32190012623615E+03 0.32190012623615E+03
 0.29815882534230E+03 0.29815854344812E+03 0.32189132077797E+03 0.32189132077797E+03 0.29815882542954E+03
 0.29815854353257E+03 0.31870093297591E+03 0.29817252518646E+03 -.16663920756332E+03 -.22675613524070E+03
 0.27815287088679E+03 0.59760822787318E+03 0.31806459263196E+03 0.31648846778697E+03 0.25160491759641E+03
 0.31648846778697E+03 0.46592540296591E+03 0.31654075801978E+03 0.25133715392851E+03 0.31654075801978E+03
 0.46572716487007E+03 0.31648846778697E+03 0.25160491759641E+03 0.31648846778697E+03 0.46592540296591E+03
 0.31654075801978E+03 0.25133715392851E+03 0.31654075801978E+03 0.46572716487007E+03 0.22875833455007E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38164186584149E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18677497687200E+00 0.00000000000000E+00 0.00000000000000E+00 0.18677497687200E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19337580681910E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19337580681910E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639246864769E+00 0.19488310972673E+00 0.35471844507880E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1682.56489484
 0.10771081453926E+00 0.33117036126431E+03 0.47360325523445E+03 0.46848940089222E+03 0.46640119831094E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14250638434130E+00 0.00000000000000E+00 -.23492387620545E+02
 0.32884420611967E-02 0.11708124016633E+01 0.24327629470500E+04 0.91228610514374E+03 0.68328623685869E+01
 0.25623233882201E+01 0.37349013760121E+03 0.29826088184810E+03 0.36332837505167E+03 0.40009907894627E+03
 0.29818082905666E+03 0.29820555425532E+03 0.35868478358346E+03 0.40008026162763E+03 0.29817686962107E+03
 0.29820549130111E+03 0.36332837505167E+03 0.40009907894627E+03 0.29818082905666E+03 0.29820555425532E+03
 0.35868478358346E+03 0.40008026162763E+03 0.29817686962107E+03 0.29820549130111E+03 0.44563179423642E+03
 0.37026407156953E+03 0.21851519722705E+04 0.19428239955509E+04 0.49188418719938E+03 0.73068487242932E+03
 0.23634126429394E+03 0.13356621756024E+04 0.12072941905706E+04 0.11577257790075E+04 0.17068290790565E+04
 0.12416381830770E+04 0.12070215278238E+04 0.10955656305949E+04 0.17067128043157E+04 0.13356621756024E+04
 0.12072941905706E+04 0.11577257790075E+04 0.17068290790565E+04 0.12416381830770E+04 0.12070215278238E+04
 0.10955656305949E+04 0.17067128043157E+04 0.16533481733748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51218211489686E+03 0.12948775809401E+01
 0.12948775809401E+01 0.65502788289302E+01 0.00000000000000E+00 0.35489003650693E+03 0.35489003650693E+03
 0.35489003650693E+03 0.35489003650693E+03 0.00000000000000E+00 0.00000000000000E+00 0.14358803148464E+00
 0.00000000000000E+00 -.14264091891361E+02 0.10000000000000E-02 0.15430973394123E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51843780659015E+01 0.19441417747130E+01 0.37026715748512E+03 0.44562914481381E+03
 0.32203043645075E+03 0.32203043645075E+03 0.29815920651004E+03 0.29815891244056E+03 0.32202156777639E+03
 0.32202156777639E+03 0.29815920657313E+03 0.29815891250164E+03 0.32203043645075E+03 0.32203043645075E+03
 0.29815920651004E+03 0.29815891244056E+03 0.32202156777639E+03 0.32202156777639E+03 0.29815920657313E+03
 0.29815891250164E+03 0.31881703182890E+03 0.29817338120902E+03 -.16819570923294E+03 -.22895310830649E+03
 0.27898644627665E+03 0.59897565569265E+03 0.31859427718462E+03 0.31782745002013E+03 0.25229154701661E+03
 0.31782745002013E+03 0.46689789620212E+03 0.31788021159140E+03 0.25202273434485E+03 0.31788021159140E+03
 0.46669911435690E+03 0.31782745002013E+03 0.25229154701661E+03 0.31782745002013E+03 0.46689789620212E+03
 0.31788021159140E+03 0.25202273434485E+03 0.31788021159140E+03 0.46669911435690E+03 0.22885092641189E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38183078084413E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18672868456607E+00 0.00000000000000E+00 0.00000000000000E+00 0.18672868456607E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19331277910608E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19331277910608E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639235851345E+00 0.19478878746335E+00 0.35489003650693E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1693.44754201
 0.10762332288356E+00 0.33132915033296E+03 0.47384598718607E+03 0.46873327533641E+03 0.46664522454053E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14246192350133E+00 0.00000000000000E+00 -.23477594237983E+02
 0.32911151830201E-02 0.11708092944832E+01 0.24307869992744E+04 0.91154512472791E+03 0.68328805021412E+01
 0.25623301883029E+01 0.37378936744237E+03 0.29826491606480E+03 0.36359748442108E+03 0.40044500574736E+03
 0.29818211241215E+03 0.29820783232782E+03 0.35894867522766E+03 0.40042620878015E+03 0.29817799974696E+03
 0.29820776733629E+03 0.36359748442107E+03 0.40044500574736E+03 0.29818211241215E+03 0.29820783232782E+03
 0.35894867522766E+03 0.40042620878015E+03 0.29817799974696E+03 0.29820776733629E+03 0.44596146941387E+03
 0.37055943268624E+03 0.21871736407361E+04 0.19438696946486E+04 0.49112526911614E+03 0.72902057871849E+03
 0.23543968325677E+03 0.13373069420553E+04 0.12079930453701E+04 0.11586071359907E+04 0.17065497932653E+04
 0.12434249023879E+04 0.12077207656504E+04 0.10966529421840E+04 0.17064336321294E+04 0.13373069420553E+04
 0.12079930453701E+04 0.11586071359907E+04 0.17065497932653E+04 0.12434249023879E+04 0.12077207656504E+04
 0.10966529421840E+04 0.17064336321294E+04 0.16530689638250E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51242548037426E+03 0.12948774719364E+01
 0.12948774719364E+01 0.65938094176029E+01 0.00000000000000E+00 0.35505902224369E+03 0.35505902224369E+03
 0.35505902224369E+03 0.35505902224369E+03 0.00000000000000E+00 0.00000000000000E+00 0.14352486717613E+00
 0.00000000000000E+00 -.14233970117554E+02 0.10000000000000E-02 0.15434934121780E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51830477129872E+01 0.19436428923702E+01 0.37056249866194E+03 0.44595884609435E+03
 0.32216048852167E+03 0.32216048852167E+03 0.29815959979962E+03 0.29815929316763E+03 0.32215155676311E+03
 0.32215155676311E+03 0.29815959983651E+03 0.29815929320334E+03 0.32216048852167E+03 0.32216048852167E+03
 0.29815959979962E+03 0.29815929316763E+03 0.32215155676311E+03 0.32215155676311E+03 0.29815959983651E+03
 0.29815929320334E+03 0.31893294099979E+03 0.29817426017902E+03 -.16975352715841E+03 -.23115334411829E+03
 0.27979965554008E+03 0.60028782406147E+03 0.31908917024369E+03 0.31914741052105E+03 0.25295819789920E+03
 0.31914741052105E+03 0.46782280816473E+03 0.31920064394941E+03 0.25268834904265E+03 0.31920064394941E+03
 0.46762349080137E+03 0.31914741052105E+03 0.25295819789920E+03 0.31914741052105E+03 0.46782280816473E+03
 0.31920064394941E+03 0.25268834904265E+03 0.31920064394941E+03 0.46762349080137E+03 0.22891519955218E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38201159642413E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18652558023823E+00 0.00000000000000E+00 0.00000000000000E+00 0.18652558023823E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19314537020433E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19314537020433E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639300472868E+00 0.19469677880733E+00 0.35505902224369E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1704.33018918
 0.10752712314549E+00 0.33148476268765E+03 0.47408839439663E+03 0.46897714164070E+03 0.46688930260341E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14241752055228E+00 0.00000000000000E+00 -.23480240765243E+02
 0.32940594022288E-02 0.11708060214158E+01 0.24286143700345E+04 0.91073038876292E+03 0.68328996039206E+01
 0.25623373514702E+01 0.37408791275161E+03 0.29826905136114E+03 0.36386606216212E+03 0.40078963959783E+03
 0.29818343452635E+03 0.29821017744818E+03 0.35921211266419E+03 0.40077086274936E+03 0.29817916450419E+03
 0.29821011037998E+03 0.36386606216212E+03 0.40078963959783E+03 0.29818343452635E+03 0.29821017744818E+03
 0.35921211266419E+03 0.40077086274936E+03 0.29817916450419E+03 0.29821011037998E+03 0.44628922459618E+03
 0.37085218010755E+03 0.21892019806376E+04 0.19449013163288E+04 0.49038886706988E+03 0.72739572548271E+03
 0.23455491407748E+03 0.13389541347627E+04 0.12086978496346E+04 0.11594683364566E+04 0.17062852027392E+04
 0.12452138103986E+04 0.12084259607826E+04 0.10977206366107E+04 0.17061691657604E+04 0.13389541347627E+04
 0.12086978496346E+04 0.11594683364566E+04 0.17062852027392E+04 0.12452138103986E+04 0.12084259607826E+04
 0.10977206366107E+04 0.17061691657604E+04 0.16528048678674E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51266887257750E+03 0.12948774914371E+01
 0.12948774914371E+01 0.66373400062757E+01 0.00000000000000E+00 0.35522674138711E+03 0.35522674138711E+03
 0.35522674138711E+03 0.35522674138711E+03 0.00000000000000E+00 0.00000000000000E+00 0.14346278501738E+00
 0.00000000000000E+00 -.14223607802643E+02 0.10000000000000E-02 0.15438749850065E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51817667088935E+01 0.19431625158350E+01 0.37085522638274E+03 0.44628662827359E+03
 0.32229021214815E+03 0.32229021214815E+03 0.29816000544250E+03 0.29815968585339E+03 0.32228121744645E+03
 0.32228121744645E+03 0.29816000545105E+03 0.29815968586166E+03 0.32229021214815E+03 0.32229021214815E+03
 0.29816000544250E+03 0.29815968585339E+03 0.32228121744645E+03 0.32228121744645E+03 0.29816000545105E+03
 0.29815968586166E+03 0.31904860356182E+03 0.29817516242182E+03 -.17130066359840E+03 -.23333727644048E+03
 0.28060406116889E+03 0.60158882624005E+03 0.31958174476531E+03 0.32045454115979E+03 0.25361654399546E+03
 0.32045454115979E+03 0.46873998233552E+03 0.32050824670621E+03 0.25334566887510E+03 0.32050824670621E+03
 0.46854014002040E+03 0.32045454115979E+03 0.25361654399546E+03 0.32045454115979E+03 0.46873998233552E+03
 0.32050824670621E+03 0.25334566887510E+03 0.32050824670621E+03 0.46854014002040E+03 0.22897838557474E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38219568801436E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18646048655280E+00 0.00000000000000E+00 0.00000000000000E+00 0.18646048655280E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19305879307064E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19305879307064E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639300294619E+00 0.19460487150567E+00 0.35522674138711E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1715.21283635
 0.10744022329104E+00 0.33163661641236E+03 0.47433007445465E+03 0.46921973545657E+03 0.46713182829829E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14237318698543E+00 0.00000000000000E+00 -.23482466939154E+02
 0.32967235115713E-02 0.11708036271843E+01 0.24266517868182E+04 0.90999442005681E+03 0.68329135768393E+01
 0.25623425913147E+01 0.37438562837905E+03 0.29827328893614E+03 0.36413394089786E+03 0.40113301031208E+03
 0.29818479606611E+03 0.29821259069910E+03 0.35947493063972E+03 0.40111425335120E+03 0.29818036450589E+03
 0.29821252151484E+03 0.36413394089786E+03 0.40113301031208E+03 0.29818479606611E+03 0.29821259069910E+03
 0.35947493063972E+03 0.40111425335120E+03 0.29818036450589E+03 0.29821252151484E+03 0.44661510304539E+03
 0.37114247202982E+03 0.21912172740834E+04 0.19458955126498E+04 0.48966576463134E+03 0.72579625429064E+03
 0.23368216083614E+03 0.13405949579427E+04 0.12093997155064E+04 0.11603029770287E+04 0.17060234930735E+04
 0.12469953837639E+04 0.12091282205312E+04 0.10987608073698E+04 0.17059075859082E+04 0.13405949579427E+04
 0.12093997155064E+04 0.11603029770287E+04 0.17060234930735E+04 0.12469953837639E+04 0.12091282205312E+04
 0.10987608073698E+04 0.17059075859082E+04 0.16525378632618E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51291086238197E+03 0.12948775078404E+01
 0.12948775078404E+01 0.66808705949484E+01 0.00000000000000E+00 0.35539409846785E+03 0.35539409846785E+03
 0.35539409846785E+03 0.35539409846785E+03 0.00000000000000E+00 0.00000000000000E+00 0.14340171446722E+00
 0.00000000000000E+00 -.14212926666550E+02 0.10000000000000E-02 0.15442397182240E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51805428299700E+01 0.19427035612387E+01 0.37114549833661E+03 0.44661253428519E+03
 0.32241968603894E+03 0.32241968603894E+03 0.29816042367070E+03 0.29816009072243E+03 0.32241062853951E+03
 0.32241062853951E+03 0.29816042364869E+03 0.29816009070112E+03 0.32241968603894E+03 0.32241968603894E+03
 0.29816042367070E+03 0.29816009072243E+03 0.32241062853951E+03 0.32241062853951E+03 0.29816042364869E+03
 0.29816009070112E+03 0.31916407894895E+03 0.29817608826165E+03 -.17283096191325E+03 -.23549464629199E+03
 0.28140697244022E+03 0.60288658857289E+03 0.32007258127047E+03 0.32175316056375E+03 0.25427374391298E+03
 0.32175316056375E+03 0.46965522208981E+03 0.32180733839382E+03 0.25400185033049E+03 0.32180733839382E+03
 0.46945486150303E+03 0.32175316056375E+03 0.25427374391298E+03 0.32175316056375E+03 0.46965522208981E+03
 0.32180733839382E+03 0.25400185033049E+03 0.32180733839382E+03 0.46945486150303E+03 0.22904658950395E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38237936993153E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18639269642610E+00 0.00000000000000E+00 0.00000000000000E+00 0.18639269642610E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19297293465562E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19297293465562E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639301272980E+00 0.19451326118112E+00 0.35539409846785E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1720.65415993
 0.10739956432912E+00 0.33171232394246E+03 0.47445053213296E+03 0.46934052500870E+03 0.46725253670424E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14235109727935E+00 0.00000000000000E+00 -.23483550512403E+02
 0.32979714778452E-02 0.11708023450748E+01 0.24257335315789E+04 0.90965007434208E+03 0.68329210593520E+01
 0.25623453972570E+01 0.37453415688004E+03 0.29827544644971E+03 0.36426760936852E+03 0.40130421650386E+03
 0.29818549182879E+03 0.29821382321090E+03 0.35960609692995E+03 0.40128546940034E+03 0.29818097791499E+03
 0.29821375295382E+03 0.36426760936852E+03 0.40130421650386E+03 0.29818549182879E+03 0.29821382321090E+03
 0.35960609692995E+03 0.40128546940034E+03 0.29818097791499E+03 0.29821375295382E+03 0.44677731736962E+03
 0.37128673325316E+03 0.21922185071361E+04 0.19463867279276E+04 0.48930733827853E+03 0.72500336481712E+03
 0.23324948984720E+03 0.13414119539447E+04 0.12097473322588E+04 0.11607177801552E+04 0.17058907137738E+04
 0.12478824389014E+04 0.12094760352731E+04 0.10992777150491E+04 0.17057748735218E+04 0.13414119539447E+04
 0.12097473322588E+04 0.11607177801552E+04 0.17058907137738E+04 0.12478824389014E+04 0.12094760352731E+04
 0.10992777150491E+04 0.17057748735218E+04 0.16524030861490E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51303132748371E+03 0.12948775158246E+01
 0.12948775158246E+01 0.67026358892848E+01 0.00000000000000E+00 0.35547760412593E+03 0.35547760412593E+03
 0.35547760412593E+03 0.35547760412593E+03 0.00000000000000E+00 0.00000000000000E+00 0.14337154228507E+00
 0.00000000000000E+00 -.14207598561520E+02 0.10000000000000E-02 0.15444162272780E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51799507533665E+01 0.19424815325124E+01 0.37128974933956E+03 0.44677476235566E+03
 0.32248433938443E+03 0.32248433938443E+03 0.29816063757694E+03 0.29816029779602E+03 0.32247525053960E+03
 0.32247525053960E+03 0.29816063753879E+03 0.29816029775908E+03 0.32248433938443E+03 0.32248433938443E+03
 0.29816063757694E+03 0.29816029779602E+03 0.32247525053960E+03 0.32247525053960E+03 0.29816063753879E+03
 0.29816029775908E+03 0.31922175447956E+03 0.29817656013145E+03 -.17359042277555E+03 -.23656436098772E+03
 0.28180751443133E+03 0.60353343780708E+03 0.32031688580359E+03 0.32239918969323E+03 0.25460154385736E+03
 0.32239918969323E+03 0.47011139600998E+03 0.32245360374059E+03 0.25432914396910E+03 0.32245360374059E+03
 0.46991077878244E+03 0.32239918969323E+03 0.25460154385736E+03 0.32239918969323E+03 0.47011139600998E+03
 0.32245360374059E+03 0.25432914396910E+03 0.32245360374059E+03 0.46991077878244E+03 0.22908192317233E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38247101677704E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18635879082265E+00 0.00000000000000E+00 0.00000000000000E+00 0.18635879082265E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19293048343078E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19293048343078E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639301768484E+00 0.19446758298172E+00 0.35547760412593E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1731.53680710
 0.10732165267968E+00 0.33186367203116E+03 0.47469063689467E+03 0.46958116158921E+03 0.46749297112778E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14230713936473E+00 0.00000000000000E+00 -.23485631803709E+02
 0.33003654949025E-02 0.11707988240527E+01 0.24239739545078E+04 0.90899023294044E+03 0.68329416084551E+01
 0.25623531031706E+01 0.37483056671617E+03 0.29827983966580E+03 0.36453443016352E+03 0.40164565916302E+03
 0.29818691375569E+03 0.29821634067989E+03 0.35986796677506E+03 0.40162693160510E+03 0.29818223193268E+03
 0.29821626824758E+03 0.36453443016352E+03 0.40164565916302E+03 0.29818691375569E+03 0.29821634067989E+03
 0.35986796677506E+03 0.40162693160510E+03 0.29818223193268E+03 0.29821626824758E+03 0.44710026529224E+03
 0.37157351675043E+03 0.21942086016643E+04 0.19473607520900E+04 0.48859637716994E+03 0.72343083136869E+03
 0.23239147231290E+03 0.13430389844635E+04 0.12104350927085E+04 0.11615440897134E+04 0.17056201493450E+04
 0.12496490770652E+04 0.12101641936657E+04 0.11003070479660E+04 0.17055044467360E+04 0.13430389844635E+04
 0.12104350927085E+04 0.11615440897134E+04 0.17056201493450E+04 0.12496490770652E+04 0.12101641936657E+04
 0.11003070479660E+04 0.17055044467360E+04 0.16521313011799E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51327129624467E+03 0.12948775311604E+01
 0.12948775311604E+01 0.67461664779575E+01 0.00000000000000E+00 0.35564419064903E+03 0.35564419064903E+03
 0.35564419064903E+03 0.35564419064903E+03 0.00000000000000E+00 0.00000000000000E+00 0.14331189850165E+00
 0.00000000000000E+00 -.14196926774965E+02 0.10000000000000E-02 0.15447583456170E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51788035472984E+01 0.19420513302369E+01 0.37157651202077E+03 0.44709773739893E+03
 0.32261348153317E+03 0.32261348153317E+03 0.29816107511927E+03 0.29816072136222E+03 0.32260433010466E+03
 0.32260433010466E+03 0.29816107504706E+03 0.29816072129231E+03 0.32261348153317E+03 0.32261348153317E+03
 0.29816107511927E+03 0.29816072136222E+03 0.32260433010466E+03 0.32260433010466E+03 0.29816107504706E+03
 0.29816072129231E+03 0.31933698330762E+03 0.29817752197211E+03 -.17509884822171E+03 -.23868727476596E+03
 0.28260609200089E+03 0.60482153124271E+03 0.32080240878181E+03 0.32368440712010E+03 0.25525486189210E+03
 0.32368440712010E+03 0.47101952194015E+03 0.32373929375701E+03 0.25498145542291E+03 0.32373929375701E+03
 0.47081839655731E+03 0.32368440712010E+03 0.25525486189210E+03 0.32368440712010E+03 0.47101952194015E+03
 0.32373929375701E+03 0.25498145542291E+03 0.32373929375701E+03 0.47081839655731E+03 0.22915394309052E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38265383132170E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18629069607715E+00 0.00000000000000E+00 0.00000000000000E+00 0.18629069607715E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19284627318054E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19284627318054E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639302921492E+00 0.19437652473726E+00 0.35564419064903E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1742.41945427
 0.10724668485659E+00 0.33201473905788E+03 0.47492967447472E+03 0.46982062346140E+03 0.46773219981973E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14226352084909E+00 0.00000000000000E+00 -.23487598320391E+02
 0.33026723337635E-02 0.11707934063166E+01 0.24222808657750E+04 0.90835532466564E+03 0.68329732272482E+01
 0.25623649602181E+01 0.37512612516332E+03 0.29828433810760E+03 0.36480057830104E+03 0.40198580511641E+03
 0.29818837677185E+03 0.29821892897005E+03 0.36012923349336E+03 0.40196709688415E+03 0.29818352272672E+03
 0.29821885432299E+03 0.36480057830104E+03 0.40198580511641E+03 0.29818837677185E+03 0.29821892897005E+03
 0.36012923349336E+03 0.40196709688415E+03 0.29818352272672E+03 0.29821885432299E+03 0.44742123155835E+03
 0.37185800889963E+03 0.21961834150271E+04 0.19483231653060E+04 0.48789373848993E+03 0.72187664402886E+03
 0.23154343684648E+03 0.13446571800447E+04 0.12111131876529E+04 0.11623644798073E+04 0.17053432875036E+04
 0.12514062757387E+04 0.12108426889281E+04 0.11013291132829E+04 0.17052277273258E+04 0.13446571800447E+04
 0.12111131876529E+04 0.11623644798073E+04 0.17053432875036E+04 0.12514062757387E+04 0.12108426889281E+04
 0.11013291132830E+04 0.17052277273258E+04 0.16518569811035E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51351007625024E+03 0.12948775456505E+01
 0.12948775456505E+01 0.67896970666302E+01 0.00000000000000E+00 0.35581016528582E+03 0.35581016528582E+03
 0.35581016528582E+03 0.35581016528582E+03 0.00000000000000E+00 0.00000000000000E+00 0.14325315964320E+00
 0.00000000000000E+00 -.14186224859242E+02 0.10000000000000E-02 0.15450866674279E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51777030820656E+01 0.19416386557746E+01 0.37186098310342E+03 0.44741873029135E+03
 0.32274240393587E+03 0.32274240393587E+03 0.29816152582910E+03 0.29816115767528E+03 0.32273319006814E+03
 0.32273319006814E+03 0.29816152572038E+03 0.29816115757004E+03 0.32274240393587E+03 0.32274240393587E+03
 0.29816152582910E+03 0.29816115767528E+03 0.32273319006814E+03 0.32273319006814E+03 0.29816152572038E+03
 0.29816115757004E+03 0.31945204861674E+03 0.29817850821500E+03 -.17659382053122E+03 -.24078904766462E+03
 0.28340094220269E+03 0.60610129542453E+03 0.32128334851083E+03 0.32496036886462E+03 0.25590475376259E+03
 0.32496036886462E+03 0.47192128056059E+03 0.32501572828486E+03 0.25563034889510E+03 0.32501572828486E+03
 0.47171965396172E+03 0.32496036886462E+03 0.25590475376259E+03 0.32496036886462E+03 0.47192128056059E+03
 0.32501572828486E+03 0.25563034889510E+03 0.32501572828486E+03 0.47171965396171E+03 0.22922712843869E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38283596216385E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18622218045136E+00 0.00000000000000E+00 0.00000000000000E+00 0.18622218045136E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19276283693731E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19276283693731E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639304330299E+00 0.19428588845864E+00 0.35581016528582E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1753.30210144
 0.10717354989988E+00 0.33216548301555E+03 0.47516767255961E+03 0.47005898846065E+03 0.46797031868601E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14222026833559E+00 0.00000000000000E+00 -.23489470518694E+02
 0.33049258797445E-02 0.11707856707541E+01 0.24206291732686E+04 0.90773593997573E+03 0.68330183737618E+01
 0.25623818901607E+01 0.37542083793771E+03 0.29828894293313E+03 0.36506605862446E+03 0.40232465767950E+03
 0.29818988154086E+03 0.29822158915031E+03 0.36038990093801E+03 0.40230596855933E+03 0.29818485090902E+03
 0.29822151224898E+03 0.36506605862446E+03 0.40232465767950E+03 0.29818988154086E+03 0.29822158915031E+03
 0.36038990093801E+03 0.40230596855933E+03 0.29818485090902E+03 0.29822151224898E+03 0.44774022584630E+03
 0.37214024080714E+03 0.21981441656321E+04 0.19492748152184E+04 0.48719990587881E+03 0.72034146094944E+03
 0.23070555554124E+03 0.13462670851407E+04 0.12117821356702E+04 0.11631788299566E+04 0.17050607933907E+04
 0.12531546240789E+04 0.12115120394376E+04 0.11023439114458E+04 0.17049453801734E+04 0.13462670851407E+04
 0.12117821356702E+04 0.11631788299566E+04 0.17050607933907E+04 0.12531546240789E+04 0.12115120394377E+04
 0.11023439114458E+04 0.17049453801734E+04 0.16515809984185E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51374775423996E+03 0.12948775594457E+01
 0.12948775594457E+01 0.68332276553029E+01 0.00000000000000E+00 0.35597549696772E+03 0.35597549696772E+03
 0.35597549696772E+03 0.35597549696772E+03 0.00000000000000E+00 0.00000000000000E+00 0.14319529425892E+00
 0.00000000000000E+00 -.14175505952320E+02 0.10000000000000E-02 0.15454019089830E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51766468991000E+01 0.19412425871625E+01 0.37214319388588E+03 0.44773775089835E+03
 0.32287110615834E+03 0.32287110615834E+03 0.29816198994000E+03 0.29816160696133E+03 0.32286182999817E+03
 0.32286182999817E+03 0.29816198979225E+03 0.29816160681830E+03 0.32287110615834E+03 0.32287110615834E+03
 0.29816198994000E+03 0.29816160696133E+03 0.32286182999817E+03 0.32286182999817E+03 0.29816198979225E+03
 0.29816160681830E+03 0.31956694972134E+03 0.29817951917989E+03 -.17807578121837E+03 -.24287039493430E+03
 0.28419179971165E+03 0.60737215000974E+03 0.32175939129953E+03 0.32622701391181E+03 0.25655095565754E+03
 0.32622701391181E+03 0.47281616908355E+03 0.32628284628946E+03 0.25627556067292E+03 0.32628284628946E+03
 0.47261404827090E+03 0.32622701391181E+03 0.25655095565754E+03 0.32622701391181E+03 0.47281616908355E+03
 0.32628284628946E+03 0.25627556067292E+03 0.32628284628946E+03 0.47261404827090E+03 0.22930101744565E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38301738202023E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18615336897394E+00 0.00000000000000E+00 0.00000000000000E+00 0.18615336897394E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19268007214283E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19268007214283E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639305957561E+00 0.19419568964263E+00 0.35597549696772E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1764.18474860
 0.10710156339223E+00 0.33231592165762E+03 0.47540466150794E+03 0.47029631892734E+03 0.46820740235149E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14217739218691E+00 0.00000000000000E+00 -.23491272470076E+02
 0.33071470416006E-02 0.11707754072613E+01 0.24190034187679E+04 0.90712628203797E+03 0.68330782747765E+01
 0.25624043530412E+01 0.37571471113132E+03 0.29829365528823E+03 0.36533087559906E+03 0.40266222418883E+03
 0.29819142872495E+03 0.29822432228501E+03 0.36064997272264E+03 0.40264355397355E+03 0.29818621709075E+03
 0.29822424308988E+03 0.36533087559906E+03 0.40266222418883E+03 0.29819142872495E+03 0.29822432228501E+03
 0.36064997272264E+03 0.40264355397355E+03 0.29818621709075E+03 0.29822424308988E+03 0.44805726604713E+03
 0.37242024199073E+03 0.22000918397497E+04 0.19502167945766E+04 0.48651528916049E+03 0.71882580223294E+03
 0.22987793662665E+03 0.13478691717407E+04 0.12124424798564E+04 0.11639875103556E+04 0.17047733578389E+04
 0.12548946213150E+04 0.12121727881730E+04 0.11033518747711E+04 0.17046580959389E+04 0.13478691717407E+04
 0.12124424798564E+04 0.11639875103556E+04 0.17047733578389E+04 0.12548946213150E+04 0.12121727881730E+04
 0.11033518747711E+04 0.17046580959389E+04 0.16513042270054E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51398439851404E+03 0.12948775727232E+01
 0.12948775727232E+01 0.68767582439756E+01 0.00000000000000E+00 0.35614016481722E+03 0.35614016481722E+03
 0.35614016481722E+03 0.35614016481722E+03 0.00000000000000E+00 0.00000000000000E+00 0.14313827369931E+00
 0.00000000000000E+00 -.14164788060892E+02 0.10000000000000E-02 0.15457046909554E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51756328662334E+01 0.19408623248375E+01 0.37242317395955E+03 0.44805481727585E+03
 0.32299958745748E+03 0.32299958745748E+03 0.29816246768586E+03 0.29816206944677E+03 0.32299024915393E+03
 0.32299024915393E+03 0.29816246749646E+03 0.29816206926343E+03 0.32299958745748E+03 0.32299958745748E+03
 0.29816246768586E+03 0.29816206944677E+03 0.32299024915393E+03 0.32299024915393E+03 0.29816246749646E+03
 0.29816206926343E+03 0.31968168570816E+03 0.29818055518519E+03 -.17954507456471E+03 -.24493187436850E+03
 0.28497848512391E+03 0.60863370810379E+03 0.32223033055426E+03 0.32748432267816E+03 0.25719329002758E+03
 0.32748432267816E+03 0.47370385159940E+03 0.32754062816628E+03 0.25691691327055E+03 0.32754062816628E+03
 0.47350124360768E+03 0.32748432267816E+03 0.25719329002758E+03 0.32748432267816E+03 0.47370385159940E+03
 0.32754062816628E+03 0.25691691327055E+03 0.32754062816628E+03 0.47350124360768E+03 0.22937528261728E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38319807334810E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18608442374930E+00 0.00000000000000E+00 0.00000000000000E+00 0.18608442374930E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19259788509858E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19259788509858E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639307747797E+00 0.19410593798744E+00 0.35614016481722E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1777.80259448
 0.10703210117525E+00 0.33249379688110E+03 0.47569827938943E+03 0.47058912050257E+03 0.46849923669717E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14212450968575E+00 0.00000000000000E+00 -.23505558197449E+02
 0.33092930997709E-02 0.11707593917814E+01 0.24174347085043E+04 0.90653801568912E+03 0.68331717483189E+01
 0.25624394056196E+01 0.37607985452461E+03 0.29829975735385E+03 0.36565972959789E+03 0.40308318205430E+03
 0.29819344637503E+03 0.29822788249564E+03 0.36097265098989E+03 0.40306453492053E+03 0.29818799980391E+03
 0.29822780035590E+03 0.36565972959789E+03 0.40308318205430E+03 0.29819344637503E+03 0.29822788249564E+03
 0.36097265098989E+03 0.40306453492053E+03 0.29818799980391E+03 0.29822780035590E+03 0.44845647010878E+03
 0.37277543911551E+03 0.22024826460150E+04 0.19512843813243E+04 0.48554399354310E+03 0.71676206203437E+03
 0.22879034852355E+03 0.13498500346159E+04 0.12132213739105E+04 0.11649233595059E+04 0.17043568522287E+04
 0.12570492989665E+04 0.12129521590264E+04 0.11045414869959E+04 0.17042417550896E+04 0.13498500346159E+04
 0.12132213739105E+04 0.11649233595059E+04 0.17043568522287E+04 0.12570492989665E+04 0.12129521590264E+04
 0.11045414869959E+04 0.17042417550896E+04 0.16508542560279E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51427604057025E+03 0.12948776779863E+01
 0.12948776779863E+01 0.69312296274683E+01 0.00000000000000E+00 0.35634593957980E+03 0.35634593957980E+03
 0.35634593957980E+03 0.35634593957980E+03 0.00000000000000E+00 0.00000000000000E+00 0.14306807239986E+00
 0.00000000000000E+00 -.14164920440967E+02 0.10000000000000E-02 0.15460637057395E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51744310213746E+01 0.19404116330155E+01 0.37277832264574E+03 0.44845406908439E+03
 0.32315843463600E+03 0.32315843463600E+03 0.29816335991177E+03 0.29816267362670E+03 0.32314901929463E+03
 0.32314901929463E+03 0.29816335966002E+03 0.29816267338788E+03 0.32315843463600E+03 0.32315843463600E+03
 0.29816335991177E+03 0.29816267362670E+03 0.32314901929463E+03 0.32314901929463E+03 0.29816335966002E+03
 0.29816267338788E+03 0.31982344614483E+03 0.29818189953710E+03 -.18144564125840E+03 -.24760202451249E+03
 0.28597563246497E+03 0.61024576373036E+03 0.32284025310306E+03 0.32909545443630E+03 0.25800930095839E+03
 0.32909545443630E+03 0.47484398918130E+03 0.32915234640179E+03 0.25773160859703E+03 0.32915234640179E+03
 0.47464068088740E+03 0.32909545443630E+03 0.25800930095839E+03 0.32909545443630E+03 0.47484398918130E+03
 0.32915234640179E+03 0.25773160859703E+03 0.32915234640179E+03 0.47464068088740E+03 0.22946329717141E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38342589733814E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18609546745807E+00 0.00000000000000E+00 0.00000000000000E+00 0.18609546745807E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19254023002488E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19254023002488E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639265471684E+00 0.19399343213841E+00 0.35634593957980E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1780.33311941
 0.10701270752774E+00 0.33253413577083E+03 0.47575348532176E+03 0.47064472186649E+03 0.46855501609555E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14211478285423E+00 0.00000000000000E+00 -.23487190607849E+02
 0.33098927901070E-02 0.11707537405473E+01 0.24169967147913E+04 0.90637376804674E+03 0.68332047320730E+01
 0.25624517745274E+01 0.37614833043441E+03 0.29830089439036E+03 0.36572166020882E+03 0.40316036070584E+03
 0.29819382255671E+03 0.29822854621324E+03 0.36103375306779E+03 0.40314171792629E+03 0.29818833219977E+03
 0.29822846352521E+03 0.36572166020882E+03 0.40316036070584E+03 0.29819382255671E+03 0.29822854621324E+03
 0.36103375306779E+03 0.40314171792629E+03 0.29818833219977E+03 0.29822846352521E+03 0.44852451961565E+03
 0.37283339843900E+03 0.22029355690351E+04 0.19515453873503E+04 0.48548794963421E+03 0.71657025126594E+03
 0.22865486188355E+03 0.13502211225582E+04 0.12133908736240E+04 0.11651453778704E+04 0.17043180762563E+04
 0.12574501701349E+04 0.12131217775963E+04 0.11048041959794E+04 0.17042030403375E+04 0.13502211225582E+04
 0.12133908736240E+04 0.11651453778704E+04 0.17043180762563E+04 0.12574501701349E+04 0.12131217775963E+04
 0.11048041959794E+04 0.17042030403375E+04 0.16508557273097E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51433158769458E+03 0.12948775426463E+01
 0.12948775426463E+01 0.69413517272104E+01 0.00000000000000E+00 0.35638413110959E+03 0.35638413110959E+03
 0.35638413110959E+03 0.35638413110959E+03 0.00000000000000E+00 0.00000000000000E+00 0.14305516686850E+00
 0.00000000000000E+00 -.14142337140630E+02 0.10000000000000E-02 0.15461284357364E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51742143893689E+01 0.19403303960133E+01 0.37283627487411E+03 0.44852212593229E+03
 0.32318930032987E+03 0.32318930032987E+03 0.29816284630493E+03 0.29816278629033E+03 0.32317987018717E+03
 0.32317987018717E+03 0.29816284605456E+03 0.29816278604113E+03 0.32318930032987E+03 0.32318930032987E+03
 0.29816284630493E+03 0.29816278629033E+03 0.32317987018717E+03 0.32317987018717E+03 0.29816284605456E+03
 0.29816278604113E+03 0.31985111805335E+03 0.29818215008767E+03 -.18171282345352E+03 -.24796973235090E+03
 0.28614958633902E+03 0.61051394056715E+03 0.32293360629643E+03 0.32934876533848E+03 0.25815077635865E+03
 0.32934876533848E+03 0.47502966466651E+03 0.32940577174457E+03 0.25787294315838E+03 0.32940577174457E+03
 0.47482633153790E+03 0.32934876533848E+03 0.25815077635865E+03 0.32934876533848E+03 0.47502966466651E+03
 0.32940577174457E+03 0.25787294315838E+03 0.32940577174457E+03 0.47482633153790E+03 0.22949510939642E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38346536250959E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18593007922668E+00 0.00000000000000E+00 0.00000000000000E+00 0.18593007922668E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19247627571589E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19247627571589E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639332105377E+00 0.19397334441656E+00 0.35638413110959E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1791.15767943
 0.10692913139801E+00 0.33269160447555E+03 0.47598810670812E+03 0.47088058320275E+03 0.46879116556201E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14207320057553E+00 0.00000000000000E+00 -.23493692190880E+02
 0.33124796238598E-02 0.11707320239198E+01 0.24151091956539E+04 0.90566594837021E+03 0.68333314853853E+01
 0.25624993070195E+01 0.37643928708289E+03 0.29830583112084E+03 0.36598424601171E+03 0.40349273646734E+03
 0.29819546079248E+03 0.29823143524943E+03 0.36129193472503E+03 0.40347411210306E+03 0.29818978013693E+03
 0.29823135018983E+03 0.36598424601171E+03 0.40349273646734E+03 0.29819546079248E+03 0.29823143524943E+03
 0.36129193472503E+03 0.40347411210306E+03 0.29818978013693E+03 0.29823135018983E+03 0.44883285909422E+03
 0.37310279890437E+03 0.22048700803934E+04 0.19525518444389E+04 0.48489870149768E+03 0.71521531198584E+03
 0.22789211698067E+03 0.13518096513328E+04 0.12140573125798E+04 0.11660022167546E+04 0.17040632355481E+04
 0.12591738642468E+04 0.12137886319644E+04 0.11058528994112E+04 0.17039483684200E+04 0.13518096513328E+04
 0.12140573125798E+04 0.11660022167546E+04 0.17040632355481E+04 0.12591738642468E+04 0.12137886319644E+04
 0.11058528994112E+04 0.17039483684200E+04 0.16506540567551E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51456701353469E+03 0.12948775905527E+01
 0.12948775905527E+01 0.69846499672785E+01 0.00000000000000E+00 0.35654610532477E+03 0.35654610532477E+03
 0.35654610532477E+03 0.35654610532477E+03 0.00000000000000E+00 0.00000000000000E+00 0.14300044685901E+00
 0.00000000000000E+00 -.14136440703599E+02 0.10000000000000E-02 0.15464012055961E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51733017091875E+01 0.19399881409453E+01 0.37310571220043E+03 0.44883044955629E+03
 0.32331733305942E+03 0.32331733305942E+03 0.29816349961993E+03 0.29816327730046E+03 0.32330784133448E+03
 0.32330784133448E+03 0.29816349931954E+03 0.29816327700502E+03 0.32331733305942E+03 0.32331733305942E+03
 0.29816349961993E+03 0.29816327730046E+03 0.32330784133448E+03 0.32330784133448E+03 0.29816349931954E+03
 0.29816327700502E+03 0.31996559781494E+03 0.29818323889491E+03 -.18310619140384E+03 -.24991615669901E+03
 0.28691374562595E+03 0.61172801529369E+03 0.32337970093962E+03 0.33055303970966E+03 0.25877240239786E+03
 0.33055303970966E+03 0.47587972024133E+03 0.33061051904631E+03 0.25849365123956E+03 0.33061051904631E+03
 0.47567595850032E+03 0.33055303970966E+03 0.25877240239786E+03 0.33055303970966E+03 0.47587972024134E+03
 0.33061051904631E+03 0.25849365123956E+03 0.33061051904631E+03 0.47567595850032E+03 0.22957431072620E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38364340503734E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18589888775330E+00 0.00000000000000E+00 0.00000000000000E+00 0.18589888775330E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19239616661857E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19239616661857E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639318636355E+00 0.19388510319070E+00 0.35654610532477E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1800.07823708
 0.10686516994963E+00 0.33281497954161E+03 0.47618032577801E+03 0.47107340508120E+03 0.46898394353876E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14203906077273E+00 0.00000000000000E+00 -.23496297645327E+02
 0.33144620716361E-02 0.11707152537398E+01 0.24136646692871E+04 0.90512425098265E+03 0.68334293710143E+01
 0.25625360141303E+01 0.37667827494486E+03 0.29830998407989E+03 0.36619991645194E+03 0.40376595637923E+03
 0.29819684473573E+03 0.29823387418321E+03 0.36150395604879E+03 0.40374734703197E+03 0.29819100377252E+03
 0.29823378713907E+03 0.36619991645194E+03 0.40376595637923E+03 0.29819684473573E+03 0.29823387418321E+03
 0.36150395604879E+03 0.40374734703197E+03 0.29819100377252E+03 0.29823378713907E+03 0.44908694394405E+03
 0.37332506914547E+03 0.22064465698193E+04 0.19533175797603E+04 0.48438656087014E+03 0.71406162332564E+03
 0.22725312965115E+03 0.13531094325604E+04 0.12145895025119E+04 0.11666586125740E+04 0.17038336330908E+04
 0.12605850871276E+04 0.12143211615463E+04 0.11066691757696E+04 0.17037189037907E+04 0.13531094325604E+04
 0.12145895025119E+04 0.11666586125740E+04 0.17038336330908E+04 0.12605850871276E+04 0.12143211615463E+04
 0.11066691757696E+04 0.17037189037907E+04 0.16504507119276E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51475935111482E+03 0.12948776097507E+01
 0.12948776097507E+01 0.70203321978665E+01 0.00000000000000E+00 0.35667892666328E+03 0.35667892666328E+03
 0.35667892666328E+03 0.35667892666328E+03 0.00000000000000E+00 0.00000000000000E+00 0.14295591864440E+00
 0.00000000000000E+00 -.14128934124022E+02 0.10000000000000E-02 0.15466186422135E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51725744030541E+01 0.19397154011453E+01 0.37332795620915E+03 0.44908456508554E+03
 0.32342233759124E+03 0.32342233759124E+03 0.29816392180625E+03 0.29816369253375E+03 0.32341279533243E+03
 0.32341279533243E+03 0.29816392146494E+03 0.29816369219806E+03 0.32342233759124E+03 0.32342233759124E+03
 0.29816392180625E+03 0.29816369253375E+03 0.32341279533243E+03 0.32341279533243E+03 0.29816392146494E+03
 0.29816369219806E+03 0.32005948568129E+03 0.29818415600685E+03 -.18426577577752E+03 -.25153685450949E+03
 0.28754184530128E+03 0.61272489726302E+03 0.32374534273523E+03 0.33154875835777E+03 0.25928315043688E+03
 0.33154875835777E+03 0.47657764968860E+03 0.33160662636012E+03 0.25900363175042E+03 0.33160662636012E+03
 0.47637352198170E+03 0.33154875835777E+03 0.25928315043688E+03 0.33154875835777E+03 0.47657764968860E+03
 0.33160662636012E+03 0.25900363175042E+03 0.33160662636012E+03 0.47637352198170E+03 0.22963392984260E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38378938096839E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18585203145730E+00 0.00000000000000E+00 0.00000000000000E+00 0.18585203145730E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19233071227223E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19233071227223E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639316413979E+00 0.19381289601569E+00 0.35667892666328E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1810.78290625
 0.10679598246627E+00 0.33296090319548E+03 0.47640988722801E+03 0.47130329550017E+03 0.46921360001994E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14199834901267E+00 0.00000000000000E+00 -.23498393173322E+02
 0.33166091546618E-02 0.11706944573939E+01 0.24121021280892E+04 0.90453829803346E+03 0.68335507608100E+01
 0.25625815353038E+01 0.37696423261074E+03 0.29831506697310E+03 0.36645801817104E+03 0.40409276947877E+03
 0.29819854543104E+03 0.29823686935504E+03 0.36175771799049E+03 0.40407417798287E+03 0.29819250800614E+03
 0.29823677989445E+03 0.36645801817104E+03 0.40409276947877E+03 0.29819854543104E+03 0.29823686935504E+03
 0.36175771799049E+03 0.40407417798287E+03 0.29819250800614E+03 0.29823677989445E+03 0.44939054533265E+03
 0.37359045130261E+03 0.22083188105610E+04 0.19542056064730E+04 0.48376850673015E+03 0.71267628131539E+03
 0.22648893205159E+03 0.13546587460986E+04 0.12152152778678E+04 0.11674271550334E+04 0.17035462362613E+04
 0.12622675618631E+04 0.12149473439365E+04 0.11076288118169E+04 0.17034316736099E+04 0.13546587460986E+04
 0.12152152778678E+04 0.11674271550334E+04 0.17035462362613E+04 0.12622675618631E+04 0.12149473439365E+04
 0.11076288118169E+04 0.17034316736099E+04 0.16501909079513E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51498857633956E+03 0.12948776251914E+01
 0.12948776251914E+01 0.70631508745722E+01 0.00000000000000E+00 0.35683795767991E+03 0.35683795767991E+03
 0.35683795767991E+03 0.35683795767991E+03 0.00000000000000E+00 0.00000000000000E+00 0.14290313232775E+00
 0.00000000000000E+00 -.14118961950863E+02 0.10000000000000E-02 0.15468693934126E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51717359164700E+01 0.19394009686763E+01 0.37359330474635E+03 0.44938820245540E+03
 0.32354805639249E+03 0.32354805639249E+03 0.29816444115487E+03 0.29816420332915E+03 0.32353845366334E+03
 0.32353845366334E+03 0.29816444076184E+03 0.29816420294259E+03 0.32354805639249E+03 0.32354805639249E+03
 0.29816444115487E+03 0.29816420332915E+03 0.32353845366334E+03 0.32353845366334E+03 0.29816444076184E+03
 0.29816420294259E+03 0.32017191515909E+03 0.29818527987129E+03 -.18565026526506E+03 -.25347019261617E+03
 0.28829461935998E+03 0.61391887329923E+03 0.32418278084245E+03 0.33273978391747E+03 0.25989532199118E+03
 0.33273978391747E+03 0.47741384979658E+03 0.33279811800986E+03 0.25961488357679E+03 0.33279811800986E+03
 0.47720928287402E+03 0.33273978391747E+03 0.25989532199118E+03 0.33273978391747E+03 0.47741384979658E+03
 0.33279811800986E+03 0.25961488357679E+03 0.33279811800986E+03 0.47720928287402E+03 0.22970729027220E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38396401336420E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18578826786007E+00 0.00000000000000E+00 0.00000000000000E+00 0.18578826786007E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19225164972730E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19225164972730E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639317021979E+00 0.19372654501436E+00 0.35683795767991E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1821.48757543
 0.10672829288192E+00 0.33310697827155E+03 0.47663835404937E+03 0.47153206781211E+03 0.46944214939736E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14195797802419E+00 0.00000000000000E+00 -.23500220429745E+02
 0.33187124447495E-02 0.11706718370527E+01 0.24105734176086E+04 0.90396503160323E+03 0.68336828022968E+01
 0.25626310508613E+01 0.37724934857899E+03 0.29832025921320E+03 0.36671543513861E+03 0.40441840883180E+03
 0.29820029030977E+03 0.29823994013331E+03 0.36201084369964E+03 0.40439983501241E+03 0.29819405191569E+03
 0.29823984821812E+03 0.36671543513861E+03 0.40441840883180E+03 0.29820029030977E+03 0.29823994013331E+03
 0.36201084369964E+03 0.40439983501241E+03 0.29819405191569E+03 0.29823984821812E+03 0.44969253465431E+03
 0.37385412920952E+03 0.22101760646898E+04 0.19550858133278E+04 0.48315251699166E+03 0.71129897211894E+03
 0.22573069254232E+03 0.13561992728395E+04 0.12158295946870E+04 0.11681927242822E+04 0.17032494009519E+04
 0.12639408493785E+04 0.12155620691225E+04 0.11085843983544E+04 0.17031350081930E+04 0.13561992728395E+04
 0.12158295946870E+04 0.11681927242821E+04 0.17032494009519E+04 0.12639408493785E+04 0.12155620691225E+04
 0.11085843983544E+04 0.17031350081930E+04 0.16499254228031E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51521668942109E+03 0.12948776386554E+01
 0.12948776386554E+01 0.71059695512779E+01 0.00000000000000E+00 0.35699634310231E+03 0.35699634310231E+03
 0.35699634310231E+03 0.35699634310231E+03 0.00000000000000E+00 0.00000000000000E+00 0.14285103376080E+00
 0.00000000000000E+00 -.14108791845228E+02 0.10000000000000E-02 0.15471105016198E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51709299313941E+01 0.19390987242728E+01 0.37385695507819E+03 0.44969022188001E+03
 0.32367349732575E+03 0.32367349732575E+03 0.29816497459296E+03 0.29816472798195E+03 0.32366383429395E+03
 0.32366383429395E+03 0.29816497414528E+03 0.29816472754164E+03 0.32367349732575E+03 0.32367349732575E+03
 0.29816497459296E+03 0.29816472798195E+03 0.32366383429395E+03 0.32366383429395E+03 0.29816497414528E+03
 0.29816472754164E+03 0.32028412185551E+03 0.29818642946624E+03 -.18702673570921E+03 -.25539076300515E+03
 0.28904381253903E+03 0.61510492377585E+03 0.32461589217412E+03 0.33392403505926E+03 0.26050418824376E+03
 0.33392403505926E+03 0.47824397801552E+03 0.33398283510735E+03 0.26022283482709E+03 0.33398283510735E+03
 0.47803897516061E+03 0.33392403505926E+03 0.26050418824376E+03 0.33392403505926E+03 0.47824397801553E+03
 0.33398283510735E+03 0.26022283482709E+03 0.33398283510735E+03 0.47803897516061E+03 0.22977999164703E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38413791274419E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18572281566024E+00 0.00000000000000E+00 0.00000000000000E+00 0.18572281566024E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19217307357143E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19217307357143E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639318441441E+00 0.19364062950589E+00 0.35699634310231E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1831.17820257
 0.10666754721882E+00 0.33323887866649E+03 0.47684430501375E+03 0.47173829214849E+03 0.46964817698857E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14192172685986E+00 0.00000000000000E+00 -.23501993715036E+02
 0.33206022414128E-02 0.11706495893733E+01 0.24092015298394E+04 0.90345057368978E+03 0.68338126734256E+01
 0.25626797525346E+01 0.37750673000247E+03 0.29832505556300E+03 0.36694787362729E+03 0.40471221144067E+03
 0.29820190885017E+03 0.29824278660871E+03 0.36223943269773E+03 0.40469365347424E+03 0.29819548456620E+03
 0.29824269243830E+03 0.36694787362729E+03 0.40471221144067E+03 0.29820190885017E+03 0.29824278660871E+03
 0.36223943269773E+03 0.40469365347424E+03 0.29819548456620E+03 0.29824269243830E+03 0.44996463706032E+03
 0.37409150449312E+03 0.22118464193895E+04 0.19558740827801E+04 0.48259613880509E+03 0.71005812788342E+03
 0.22504900838431E+03 0.13575873036766E+04 0.12163770262714E+04 0.11688805943037E+04 0.17029740507555E+04
 0.12654488016325E+04 0.12161098709933E+04 0.11094436719903E+04 0.17028598140337E+04 0.13575873036766E+04
 0.12163770262714E+04 0.11688805943037E+04 0.17029740507555E+04 0.12654488016325E+04 0.12161098709933E+04
 0.11094436719903E+04 0.17028598140337E+04 0.16496804319729E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51542232068045E+03 0.12948776517217E+01
 0.12948776517217E+01 0.71447320598481E+01 0.00000000000000E+00 0.35713916145374E+03 0.35713916145374E+03
 0.35713916145374E+03 0.35713916145374E+03 0.00000000000000E+00 0.00000000000000E+00 0.14280444734906E+00
 0.00000000000000E+00 -.14099762070865E+02 0.10000000000000E-02 0.15473207767906E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51702272211410E+01 0.19388352079279E+01 0.37409430871757E+03 0.44996234880396E+03
 0.32378678923400E+03 0.32378678923400E+03 0.29816550446985E+03 0.29816521517025E+03 0.32377707176344E+03
 0.32377707176344E+03 0.29816550396894E+03 0.29816521467869E+03 0.32378678923400E+03 0.32378678923400E+03
 0.29816550446985E+03 0.29816521517025E+03 0.32377707176344E+03 0.32377707176344E+03 0.29816550396894E+03
 0.29816521467869E+03 0.32038548043930E+03 0.29818749278895E+03 -.18826745859782E+03 -.25712072813803E+03
 0.28971909049847E+03 0.61617216581951E+03 0.32500447986854E+03 0.33499103266141E+03 0.26105264608287E+03
 0.33499103266141E+03 0.47899052018504E+03 0.33505025432155E+03 0.26077046656795E+03 0.33505025432155E+03
 0.47878512363417E+03 0.33499103266142E+03 0.26105264608287E+03 0.33499103266142E+03 0.47899052018504E+03
 0.33505025432155E+03 0.26077046656795E+03 0.33505025432155E+03 0.47878512363417E+03 0.22984485351296E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38429471529455E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18566486310720E+00 0.00000000000000E+00 0.00000000000000E+00 0.18566486310720E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19210238516689E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19210238516689E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639319282117E+00 0.19356321902901E+00 0.35713916145374E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1842.02393396
 0.10659969392732E+00 0.33338627088396E+03 0.47707389012090E+03 0.47196820471030E+03 0.46987788692120E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14188148246781E+00 0.00000000000000E+00 -.23503451475203E+02
 0.33227157033181E-02 0.11706226036928E+01 0.24076691219809E+04 0.90287592074283E+03 0.68339702093259E+01
 0.25627388284972E+01 0.37779401166069E+03 0.29833053295835E+03 0.36720739526646E+03 0.40503987789554E+03
 0.29820376489762E+03 0.29824604851080E+03 0.36249470155274E+03 0.40502133751466E+03 0.29819712805384E+03
 0.29824595177899E+03 0.36720739526646E+03 0.40503987789554E+03 0.29820376489762E+03 0.29824604851080E+03
 0.36249470155274E+03 0.40502133751466E+03 0.29819712805384E+03 0.29824595177899E+03 0.45026743945621E+03
 0.37435529178272E+03 0.22137047032945E+04 0.19567487755149E+04 0.48198177187012E+03 0.70868658319695E+03
 0.22429490246748E+03 0.13591339403331E+04 0.12169818609545E+04 0.11696457992341E+04 0.17026612188124E+04
 0.12671292288659E+04 0.12167151223199E+04 0.11103997725882E+04 0.17025471607426E+04 0.13591339403330E+04
 0.12169818609545E+04 0.11696457992341E+04 0.17026612188124E+04 0.12671292288659E+04 0.12167151223199E+04
 0.11103997725882E+04 0.17025471607426E+04 0.16494057113443E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51565157910815E+03 0.12948776624631E+01
 0.12948776624631E+01 0.71881149853915E+01 0.00000000000000E+00 0.35729837501124E+03 0.35729837501124E+03
 0.35729837501124E+03 0.35729837501124E+03 0.00000000000000E+00 0.00000000000000E+00 0.14275294060423E+00
 0.00000000000000E+00 -.14089180652890E+02 0.10000000000000E-02 0.15475474139808E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51694700451350E+01 0.19385512669256E+01 0.37435807349260E+03 0.45026517736569E+03
 0.32391335251026E+03 0.32391335251026E+03 0.29816607438952E+03 0.29816577445533E+03 0.32390357425564E+03
 0.32390357425564E+03 0.29816607382711E+03 0.29816577390342E+03 0.32391335251026E+03 0.32391335251026E+03
 0.29816607438952E+03 0.29816577445533E+03 0.32390357425564E+03 0.32390357425564E+03 0.29816607382711E+03
 0.29816577390342E+03 0.32049874005656E+03 0.29818870869084E+03 -.18964558593986E+03 -.25904050492965E+03
 0.29047103042492E+03 0.61735816836943E+03 0.32543478279239E+03 0.33617720306038E+03 0.26166295795240E+03
 0.33617720306038E+03 0.47981952279628E+03 0.33623689663644E+03 0.26137986192743E+03 0.33623689663644E+03
 0.47961369239999E+03 0.33617720306038E+03 0.26166295795240E+03 0.33617720306038E+03 0.47981952279627E+03
 0.33623689663644E+03 0.26137986192743E+03 0.33623689663644E+03 0.47961369239999E+03 0.22991739641688E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38446950637777E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18559616906042E+00 0.00000000000000E+00 0.00000000000000E+00 0.18559616906042E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19202345572270E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19202345572270E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639321943198E+00 0.19347701292966E+00 0.35729837501124E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1850.70051907
 0.10654520609932E+00 0.33350414928417E+03 0.47725694415083E+03 0.47215155376535E+03 0.47006109545606E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14184952446754E+00 0.00000000000000E+00 -.23504689207602E+02
 0.33244148140107E-02 0.11705993819040E+01 0.24064385606405E+04 0.90241446024019E+03 0.68341057783475E+01
 0.25627896668803E+01 0.37802326707399E+03 0.29833499784336E+03 0.36741455970522E+03 0.40530113970361E+03
 0.29820528370881E+03 0.29824871599624E+03 0.36269850719553E+03 0.40528261327690E+03 0.29819847339121E+03
 0.29824861718725E+03 0.36741455970522E+03 0.40530113970361E+03 0.29820528370881E+03 0.29824871599624E+03
 0.36269850719553E+03 0.40528261327690E+03 0.29819847339121E+03 0.29824861718725E+03 0.45050838843072E+03
 0.37456487554919E+03 0.22151844423080E+04 0.19574453766320E+04 0.48149790890453E+03 0.70760425818734E+03
 0.22369885973828E+03 0.13603669253428E+04 0.12174613745646E+04 0.11702560836416E+04 0.17024095783910E+04
 0.12684689212478E+04 0.12171949707098E+04 0.11111620268307E+04 0.17022956660542E+04 0.13603669253428E+04
 0.12174613745646E+04 0.11702560836416E+04 0.17024095783910E+04 0.12684689212478E+04 0.12171949707098E+04
 0.11111620268307E+04 0.17022956660542E+04 0.16491879155436E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51583441586874E+03 0.12948776715832E+01
 0.12948776715832E+01 0.72228213258261E+01 0.00000000000000E+00 0.35742527455778E+03 0.35742527455778E+03
 0.35742527455778E+03 0.35742527455778E+03 0.00000000000000E+00 0.00000000000000E+00 0.14271220424689E+00
 0.00000000000000E+00 -.14080827999772E+02 0.10000000000000E-02 0.15477222932754E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51688859395246E+01 0.19383322273217E+01 0.37456764136548E+03 0.45050614586721E+03
 0.32401443652949E+03 0.32401443652949E+03 0.29816654123218E+03 0.29816623258678E+03 0.32400460976496E+03
 0.32400460976496E+03 0.29816654061820E+03 0.29816623198426E+03 0.32401443652949E+03 0.32401443652949E+03
 0.29816654123218E+03 0.29816623258678E+03 0.32400460976496E+03 0.32400460976496E+03 0.29816654061820E+03
 0.29816623198426E+03 0.32058922071250E+03 0.29818970105495E+03 -.19073986704809E+03 -.26056347676357E+03
 0.29106962610291E+03 0.61830049080412E+03 0.32577551657070E+03 0.33711988616126E+03 0.26214848239472E+03
 0.33711988616126E+03 0.48047771124696E+03 0.33717995725774E+03 0.26186465912331E+03 0.33717995725774E+03
 0.48027153870308E+03 0.33711988616126E+03 0.26214848239472E+03 0.33711988616126E+03 0.48047771124696E+03
 0.33717995725774E+03 0.26186465912331E+03 0.33717995725774E+03 0.48027153870308E+03 0.22997545603856E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38460883837344E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18554201537588E+00 0.00000000000000E+00 0.00000000000000E+00 0.18554201537588E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19196067428454E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19196067428454E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639323817371E+00 0.19340835565040E+00 0.35742527455778E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1861.68967303
 0.10647725081543E+00 0.33365199964680E+03 0.47748783412166E+03 0.47238275271378E+03 0.47029207527029E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14180934252102E+00 0.00000000000000E+00 -.23507939284691E+02
 0.33265363196029E-02 0.11705684103971E+01 0.24049038493453E+04 0.90183894350450E+03 0.68342865986669E+01
 0.25628574745001E+01 0.37831273524704E+03 0.29834076467090E+03 0.36767616490064E+03 0.40563107156087E+03
 0.29820725336048E+03 0.29825217290614E+03 0.36295585738498E+03 0.40561256263027E+03 0.29820021870956E+03
 0.29825207142886E+03 0.36767616490064E+03 0.40563107156087E+03 0.29820725336048E+03 0.29825217290614E+03
 0.36295585738498E+03 0.40561256263028E+03 0.29820021870956E+03 0.29825207142886E+03 0.45081283998806E+03
 0.37482981689706E+03 0.22170474961776E+04 0.19583112099712E+04 0.48087407415559E+03 0.70622087100419E+03
 0.22294242647783E+03 0.13619220661367E+04 0.12180578827782E+04 0.11710172369877E+04 0.17020813091599E+04
 0.12701592302681E+04 0.12177919006613E+04 0.11121158441491E+04 0.17019675809505E+04 0.13619220661367E+04
 0.12180578827782E+04 0.11710172369877E+04 0.17020813091599E+04 0.12701592302681E+04 0.12177919006613E+04
 0.11121158441491E+04 0.17019675809505E+04 0.16488991777645E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51606494904613E+03 0.12948776955311E+01
 0.12948776955311E+01 0.72667779416640E+01 0.00000000000000E+00 0.35758537639477E+03 0.35758537639477E+03
 0.35758537639477E+03 0.35758537639477E+03 0.00000000000000E+00 0.00000000000000E+00 0.14266119241472E+00
 0.00000000000000E+00 -.14072032494822E+02 0.10000000000000E-02 0.15479359386274E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51681725324461E+01 0.19380646996673E+01 0.37483256000004E+03 0.45081062414233E+03
 0.32414200092214E+03 0.32414200092214E+03 0.29816732437958E+03 0.29816682734902E+03 0.32413211294029E+03
 0.32413211294029E+03 0.29816732368998E+03 0.29816682667920E+03 0.32414200092214E+03 0.32414200092214E+03
 0.29816732437958E+03 0.29816682734902E+03 0.32413211294029E+03 0.32413211294029E+03 0.29816732368998E+03
 0.29816682667920E+03 0.32070340855723E+03 0.29819098446413E+03 -.19213010883910E+03 -.26249805951909E+03
 0.29182586501822E+03 0.61949013583054E+03 0.32620514148724E+03 0.33831370596477E+03 0.26276160770177E+03
 0.33831370596477E+03 0.48130852461406E+03 0.33837425433060E+03 0.26247685367830E+03 0.33837425433060E+03
 0.48110190732779E+03 0.33831370596477E+03 0.26276160770177E+03 0.33831370596477E+03 0.48130852461406E+03
 0.33837425433061E+03 0.26247685367830E+03 0.33837425433061E+03 0.48110190732780E+03 0.23004586162462E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38478468667655E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18548724426601E+00 0.00000000000000E+00 0.00000000000000E+00 0.18548724426601E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19188281101326E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19188281101326E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639320467555E+00 0.19332174466868E+00 0.35758537639477E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1870.65310328
 0.10642769667460E+00 0.33377046517447E+03 0.47767523007299E+03 0.47257007918344E+03 0.47047906720342E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14177686013348E+00 0.00000000000000E+00 -.23512048370189E+02
 0.33280850512056E-02 0.11705415521558E+01 0.24037847221188E+04 0.90141927079454E+03 0.68344434123389E+01
 0.25629162796271E+01 0.37854816807477E+03 0.29834556369189E+03 0.36788899958255E+03 0.40589926121137E+03
 0.29820889927870E+03 0.29825505957445E+03 0.36316526580202E+03 0.40588076643312E+03 0.29820167771123E+03
 0.29825495588921E+03 0.36788899958256E+03 0.40589926121137E+03 0.29820889927870E+03 0.29825505957445E+03
 0.36316526580202E+03 0.40588076643312E+03 0.29820167771123E+03 0.29825495588921E+03 0.45105965096517E+03
 0.37504438287213E+03 0.22185502190400E+04 0.19589880472582E+04 0.48037015925351E+03 0.70510365354183E+03
 0.22233164349206E+03 0.13631811918185E+04 0.12185337741675E+04 0.11716184575231E+04 0.17018039517819E+04
 0.12715280380770E+04 0.12182681371449E+04 0.11128735366390E+04 0.17016903761733E+04 0.13631811918185E+04
 0.12185337741675E+04 0.11716184575231E+04 0.17018039517819E+04 0.12715280380770E+04 0.12182681371449E+04
 0.11128735366390E+04 0.17016903761733E+04 0.16486526369998E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51625166074996E+03 0.12948777258085E+01
 0.12948777258085E+01 0.73026316626876E+01 0.00000000000000E+00 0.35771574449053E+03 0.35771574449053E+03
 0.35771574449053E+03 0.35771574449053E+03 0.00000000000000E+00 0.00000000000000E+00 0.14262005198969E+00
 0.00000000000000E+00 -.14066913155352E+02 0.10000000000000E-02 0.15481026854571E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51676158662808E+01 0.19378559498553E+01 0.37504710433705E+03 0.45105745873700E+03
 0.32424586563933E+03 0.32424586563933E+03 0.29816783663742E+03 0.29816732490977E+03 0.32423592779624E+03
 0.32423592779624E+03 0.29816783588845E+03 0.29816732418229E+03 0.32424586563933E+03 0.32424586563933E+03
 0.29816783663742E+03 0.29816732490977E+03 0.32423592779624E+03 0.32423592779624E+03 0.29816783588845E+03
 0.29816732418229E+03 0.32079639917154E+03 0.29819205393516E+03 -.19325256095467E+03 -.26405796940256E+03
 0.29244211769722E+03 0.62046093741230E+03 0.32655660912660E+03 0.33928219382801E+03 0.26326138896065E+03
 0.33928219382801E+03 0.48198733919820E+03 0.33934313178793E+03 0.26297588453493E+03 0.33934313178793E+03
 0.48178036789156E+03 0.33928219382801E+03 0.26326138896065E+03 0.33928219382801E+03 0.48198733919820E+03
 0.33934313178793E+03 0.26297588453493E+03 0.33934313178793E+03 0.48178036789157E+03 0.23010807752500E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38492878871590E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18545473028843E+00 0.00000000000000E+00 0.00000000000000E+00 0.18545473028843E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19183780354152E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19183780354152E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639311032234E+00 0.19325120624420E+00 0.35771574449053E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1884.09824867
 0.10635716975893E+00 0.33394971420266E+03 0.47795523739320E+03 0.47284989743447E+03 0.47075840035450E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14172874175980E+00 0.00000000000000E+00 -.23514820899134E+02
 0.33302917301871E-02 0.11704959689059E+01 0.24021919543819E+04 0.90082198289320E+03 0.68347095697199E+01
 0.25630160886449E+01 0.37890030642343E+03 0.29835291374126E+03 0.36820747049048E+03 0.40629988204313E+03
 0.29821143106486E+03 0.29825949656926E+03 0.36347870029215E+03 0.40628140829633E+03 0.29820392285717E+03
 0.29825938952248E+03 0.36820747049048E+03 0.40629988204313E+03 0.29821143106486E+03 0.29825949656926E+03
 0.36347870029215E+03 0.40628140829633E+03 0.29820392285717E+03 0.29825938952248E+03 0.45142703077595E+03
 0.37536306895684E+03 0.22207882652107E+04 0.19600069912385E+04 0.47963776811700E+03 0.70346945823859E+03
 0.22143350128100E+03 0.13650602994100E+04 0.12192409813334E+04 0.11725281356994E+04 0.17013876986817E+04
 0.12735706707527E+04 0.12189758626812E+04 0.11140152716948E+04 0.17012743551395E+04 0.13650602994100E+04
 0.12192409813334E+04 0.11725281356993E+04 0.17013876986816E+04 0.12735706707527E+04 0.12189758626812E+04
 0.11140152716948E+04 0.17012743551395E+04 0.16482932667527E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51653056325421E+03 0.12948777462377E+01
 0.12948777462377E+01 0.73564122442230E+01 0.00000000000000E+00 0.35791083336138E+03 0.35791083336138E+03
 0.35791083336138E+03 0.35791083336138E+03 0.00000000000000E+00 0.00000000000000E+00 0.14255911621879E+00
 0.00000000000000E+00 -.14055387133548E+02 0.10000000000000E-02 0.15483409080754E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51668207939710E+01 0.19375577977391E+01 0.37536576687708E+03 0.45142486715186E+03
 0.32440153454348E+03 0.32440153454348E+03 0.29816862552972E+03 0.29816809116805E+03 0.32439152209088E+03
 0.32439152209088E+03 0.29816862468707E+03 0.29816809034958E+03 0.32440153454348E+03 0.32440153454348E+03
 0.29816862552972E+03 0.29816809116805E+03 0.32439152209088E+03 0.32439152209088E+03 0.29816862468707E+03
 0.29816809034958E+03 0.32093581624162E+03 0.29819369425194E+03 -.19491165720419E+03 -.26635964049946E+03
 0.29336316289949E+03 0.62190777372212E+03 0.32707779500814E+03 0.34072142295396E+03 0.26400814478990E+03
 0.34072142295396E+03 0.48299828522035E+03 0.34078294560866E+03 0.26372153189205E+03 0.34078294560866E+03
 0.48279079791804E+03 0.34072142295396E+03 0.26400814478990E+03 0.34072142295396E+03 0.48299828522035E+03
 0.34078294560866E+03 0.26372153189205E+03 0.34078294560866E+03 0.48279079791804E+03 0.23020613906327E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38514333462772E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18537943869029E+00 0.00000000000000E+00 0.00000000000000E+00 0.18537943869029E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19175224860834E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19175224860834E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639309681283E+00 0.19314587820180E+00 0.35791083336138E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1893.06167892
 0.10630597416492E+00 0.33407066542068E+03 0.47814125815647E+03 0.47303606992003E+03 0.47094439817904E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14169703253370E+00 0.00000000000000E+00 -.23514321920492E+02
 0.33318954089913E-02 0.11704623237289E+01 0.24010357523263E+04 0.90038840712236E+03 0.68349060348338E+01
 0.25630897630627E+01 0.37913442985632E+03 0.29835791559569E+03 0.36841929877430E+03 0.40656593085959E+03
 0.29821316137854E+03 0.29826252670178E+03 0.36368722467182E+03 0.40654747100751E+03 0.29820545786545E+03
 0.29826241738102E+03 0.36841929877430E+03 0.40656593085959E+03 0.29821316137854E+03 0.29826252670178E+03
 0.36368722467182E+03 0.40654747100751E+03 0.29820545786545E+03 0.29826241738102E+03 0.45167037594455E+03
 0.37557375935408E+03 0.22222759641361E+04 0.19606982452270E+04 0.47916119703601E+03 0.70240154251522E+03
 0.22084453949402E+03 0.13663094651361E+04 0.12197088749979E+04 0.11731432594470E+04 0.17011106546880E+04
 0.12749285969748E+04 0.12194441036279E+04 0.11147840729702E+04 0.17009974690793E+04 0.13663094651361E+04
 0.12197088749979E+04 0.11731432594470E+04 0.17011106546880E+04 0.12749285969748E+04 0.12194441036279E+04
 0.11147840729702E+04 0.17009974690793E+04 0.16480626078401E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51671619592525E+03 0.12948777425610E+01
 0.12948777425610E+01 0.73922659652466E+01 0.00000000000000E+00 0.35804016223889E+03 0.35804016223889E+03
 0.35804016223889E+03 0.35804016223889E+03 0.00000000000000E+00 0.00000000000000E+00 0.14251900512346E+00
 0.00000000000000E+00 -.14044997231813E+02 0.10000000000000E-02 0.15484936945004E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51663109952677E+01 0.19373666232254E+01 0.37557644237062E+03 0.45166823100781E+03
 0.32450515381332E+03 0.32450515381332E+03 0.29816916531827E+03 0.29816861546955E+03 0.32449509176069E+03
 0.32449509176069E+03 0.29816916440999E+03 0.29816861458733E+03 0.32450515381332E+03 0.32450515381332E+03
 0.29816916531827E+03 0.29816861546955E+03 0.32449509176069E+03 0.32449509176069E+03 0.29816916440999E+03
 0.29816861458733E+03 0.32102864528332E+03 0.29819481211350E+03 -.19600850106200E+03 -.26787992049926E+03
 0.29397190005705E+03 0.62285937123784E+03 0.32741761168051E+03 0.34167199624221E+03 0.26450096492169E+03
 0.34167199624221E+03 0.48366161984844E+03 0.34173390871621E+03 0.26421362186439E+03 0.34173390871621E+03
 0.48345379564792E+03 0.34167199624221E+03 0.26450096492169E+03 0.34167199624221E+03 0.48366161984844E+03
 0.34173390871621E+03 0.26421362186439E+03 0.34173390871621E+03 0.48345379564792E+03 0.23026861522773E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38528488096894E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18531065477735E+00 0.00000000000000E+00 0.00000000000000E+00 0.18531065477735E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19168209384907E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19168209384907E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639317865176E+00 0.19307621160130E+00 0.35804016223889E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1902.02510918
 0.10624733461328E+00 0.33419280292279E+03 0.47832687264402E+03 0.47322225239907E+03 0.47113059919459E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14166551384630E+00 0.00000000000000E+00 -.23511845340981E+02
 0.33337341862004E-02 0.11704271947065E+01 0.23997114206391E+04 0.89989178273966E+03 0.68351111766553E+01
 0.25631666912457E+01 0.37936808518760E+03 0.29836299954492E+03 0.36863076976209E+03 0.40683118786750E+03
 0.29821492606853E+03 0.29826561518380E+03 0.36389543283453E+03 0.40681274182126E+03 0.29820702385308E+03
 0.29826550356275E+03 0.36863076976209E+03 0.40683118786750E+03 0.29821492606853E+03 0.29826561518380E+03
 0.36389543283453E+03 0.40681274182126E+03 0.29820702385308E+03 0.29826550356275E+03 0.45191255677276E+03
 0.37578307176405E+03 0.22237654019634E+04 0.19614041949232E+04 0.47869497820246E+03 0.70135211289939E+03
 0.22026365980591E+03 0.13675583047819E+04 0.12201761654883E+04 0.11737664520970E+04 0.17008367298467E+04
 0.12762861677470E+04 0.12199117450390E+04 0.11155605285057E+04 0.17007237069210E+04 0.13675583047819E+04
 0.12201761654883E+04 0.11737664520970E+04 0.17008367298467E+04 0.12762861677470E+04 0.12199117450390E+04
 0.11155605285057E+04 0.17007237069210E+04 0.16478426865819E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51690193117420E+03 0.12948777243125E+01
 0.12948777243125E+01 0.74281196862703E+01 0.00000000000000E+00 0.35816863502149E+03 0.35816863502149E+03
 0.35816863502149E+03 0.35816863502149E+03 0.00000000000000E+00 0.00000000000000E+00 0.14247929868012E+00
 0.00000000000000E+00 -.14032287389941E+02 0.10000000000000E-02 0.15486429404685E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51658131070417E+01 0.19371799151406E+01 0.37578573910450E+03 0.45191043139593E+03
 0.32460860999457E+03 0.32460860999457E+03 0.29816971634691E+03 0.29816915068860E+03 0.32459849845779E+03
 0.32459849845779E+03 0.29816971537039E+03 0.29816914974010E+03 0.32460860999457E+03 0.32460860999457E+03
 0.29816971634691E+03 0.29816915068860E+03 0.32459849845778E+03 0.32459849845778E+03 0.29816971537039E+03
 0.29816914974010E+03 0.32112135188968E+03 0.29819594962229E+03 -.19710061980547E+03 -.26939319805078E+03
 0.29457423373038E+03 0.62379585889262E+03 0.32774875399359E+03 0.34261451412469E+03 0.26498764288372E+03
 0.34261451412469E+03 0.48431241438145E+03 0.34267681640318E+03 0.26469957690519E+03 0.34267681640318E+03
 0.48410425896246E+03 0.34261451412468E+03 0.26498764288372E+03 0.34261451412468E+03 0.48431241438145E+03
 0.34267681640318E+03 0.26469957690519E+03 0.34267681640318E+03 0.48410425896246E+03 0.23032538426620E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38542494739005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18522604537805E+00 0.00000000000000E+00 0.00000000000000E+00 0.18522604537805E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19160006416932E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19160006416932E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639333882387E+00 0.19300713803996E+00 0.35816863502149E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1910.98853943
 0.10617846871411E+00 0.33431623275172E+03 0.47851220060197E+03 0.47340869824827E+03 0.47131730917562E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14163404476822E+00 0.00000000000000E+00 -.23507182133175E+02
 0.33358962520198E-02 0.11703921626631E+01 0.23981561162629E+04 0.89930854359861E+03 0.68353157644163E+01
 0.25632434116561E+01 0.37960130884011E+03 0.29836816612792E+03 0.36884190909257E+03 0.40709568148287E+03
 0.29821672548560E+03 0.29826876256020E+03 0.36410335058991E+03 0.40707724916904E+03 0.29820862114765E+03
 0.29826864861263E+03 0.36884190909257E+03 0.40709568148287E+03 0.29821672548560E+03 0.29826876256020E+03
 0.36410335058991E+03 0.40707724916904E+03 0.29820862114765E+03 0.29826864861263E+03 0.45215364276614E+03
 0.37599099565915E+03 0.22252609938010E+04 0.19621298662981E+04 0.47824072524494E+03 0.70032330837283E+03
 0.21969137950167E+03 0.13688090794378E+04 0.12206453174617E+04 0.11743996317715E+04 0.17005690049273E+04
 0.12776457466570E+04 0.12203812540099E+04 0.11163467812053E+04 0.17004561517554E+04 0.13688090794378E+04
 0.12206453174617E+04 0.11743996317715E+04 0.17005690049273E+04 0.12776457466570E+04 0.12203812540099E+04
 0.11163467812053E+04 0.17004561517554E+04 0.16476373500349E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51708804764556E+03 0.12948776899521E+01
 0.12948776899521E+01 0.74639734072939E+01 0.00000000000000E+00 0.35829605301337E+03 0.35829605301337E+03
 0.35829605301337E+03 0.35829605301337E+03 0.00000000000000E+00 0.00000000000000E+00 0.14243998717079E+00
 0.00000000000000E+00 -.14016980225956E+02 0.10000000000000E-02 0.15487896857536E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51653236547138E+01 0.19369963705177E+01 0.37599364676716E+03 0.45215153775352E+03
 0.32471187448742E+03 0.32471187448742E+03 0.29817027874555E+03 0.29816969695140E+03 0.32470171358670E+03
 0.32470171358670E+03 0.29817027769813E+03 0.29816969593404E+03 0.32471187448742E+03 0.32471187448742E+03
 0.29817027874555E+03 0.29816969695140E+03 0.32470171358670E+03 0.32470171358670E+03 0.29817027769813E+03
 0.29816969593404E+03 0.32121391253029E+03 0.29819710694325E+03 -.19818956979599E+03 -.27090207170548E+03
 0.29516856287106E+03 0.62471368179335E+03 0.32806927610794E+03 0.34354814083719E+03 0.26546662386055E+03
 0.34354814083719E+03 0.48494765327267E+03 0.34361083287312E+03 0.26517784269512E+03 0.34361083287312E+03
 0.48473917264083E+03 0.34354814083719E+03 0.26546662386055E+03 0.34354814083719E+03 0.48494765327267E+03
 0.34361083287312E+03 0.26517784269513E+03 0.34361083287312E+03 0.48473917264083E+03 0.23037406472707E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38556327330107E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18512381060392E+00 0.00000000000000E+00 0.00000000000000E+00 0.18512381060392E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19150420851120E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19150420851120E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639358686190E+00 0.19293877327382E+00 0.35829605301337E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1920.00563822
 0.10610748893441E+00 0.33444058112720E+03 0.47869884394293E+03 0.47359654993446E+03 0.47150546006663E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14160230402428E+00 0.00000000000000E+00 -.23512453860527E+02
 0.33381276242051E-02 0.11703574358912E+01 0.23965530682503E+04 0.89870740059387E+03 0.68355185814735E+01
 0.25633194680526E+01 0.37983580130835E+03 0.29837343768259E+03 0.36905431064411E+03 0.40736066696742E+03
 0.29821856689780E+03 0.29827198168327E+03 0.36431266505269E+03 0.40734224846220E+03 0.29821025616208E+03
 0.29827186537196E+03 0.36905431064411E+03 0.40736066696742E+03 0.29821856689780E+03 0.29827198168327E+03
 0.36431266505269E+03 0.40734224846220E+03 0.29821025616208E+03 0.29827186537196E+03 0.45239346878724E+03
 0.37619624928665E+03 0.22267712731301E+04 0.19628678208971E+04 0.47783610678384E+03 0.69936955579685E+03
 0.21914426847909E+03 0.13700700331985E+04 0.12211334303069E+04 0.11750396522403E+04 0.17003268478721E+04
 0.12790148708909E+04 0.12208697356610E+04 0.11171384681546E+04 0.17002141760024E+04 0.13700700331985E+04
 0.12211334303069E+04 0.11750396522403E+04 0.17003268478721E+04 0.12790148708909E+04 0.12208697356610E+04
 0.11171384681546E+04 0.17002141760024E+04 0.16474681133079E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51727558345111E+03 0.12948777287964E+01
 0.12948777287964E+01 0.75000418024217E+01 0.00000000000000E+00 0.35842387624419E+03 0.35842387624419E+03
 0.35842387624419E+03 0.35842387624419E+03 0.00000000000000E+00 0.00000000000000E+00 0.14240081554091E+00
 0.00000000000000E+00 -.14013219995935E+02 0.10000000000000E-02 0.15489319305893E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51648493016451E+01 0.19368184881169E+01 0.37619888863322E+03 0.45239138150968E+03
 0.32481602675732E+03 0.32481602675732E+03 0.29817037657159E+03 0.29817025642669E+03 0.32480581624766E+03
 0.32480581624766E+03 0.29817037547614E+03 0.29817025533770E+03 0.32481602675732E+03 0.32481602675732E+03
 0.29817037657159E+03 0.29817025642669E+03 0.32480581624766E+03 0.32480581624766E+03 0.29817037547614E+03
 0.29817025533770E+03 0.32130734425862E+03 0.29819828897905E+03 -.19924780940274E+03 -.27236387700896E+03
 0.29576058639192E+03 0.62563007827354E+03 0.32839068894966E+03 0.34446653123359E+03 0.26594333796213E+03
 0.34446653123359E+03 0.48558243535495E+03 0.34452961639642E+03 0.26565387561491E+03 0.34452961639642E+03
 0.48537366687369E+03 0.34446653123359E+03 0.26594333796213E+03 0.34446653123359E+03 0.48558243535495E+03
 0.34452961639642E+03 0.26565387561491E+03 0.34452961639642E+03 0.48537366687369E+03 0.23043356669223E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38570544713893E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18510034355979E+00 0.00000000000000E+00 0.00000000000000E+00 0.18510034355979E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19146657802282E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19146657802282E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639345389078E+00 0.19286984288931E+00 0.35842387624419E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1930.26747280
 0.10604543031492E+00 0.33457518156237E+03 0.47891017207208E+03 0.47380815001271E+03 0.47171684194478E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14156630710397E+00 0.00000000000000E+00 -.23518377056828E+02
 0.33400809557717E-02 0.11703189275765E+01 0.23951515265449E+04 0.89818182245432E+03 0.68357434981987E+01
 0.25634038118245E+01 0.38010171544627E+03 0.29837953973084E+03 0.36929512634821E+03 0.40766186876620E+03
 0.29822070597406E+03 0.29827571879870E+03 0.36454987524237E+03 0.40764346581450E+03 0.29821215609298E+03
 0.29827559976524E+03 0.36929512634821E+03 0.40766186876620E+03 0.29822070597406E+03 0.29827571879870E+03
 0.36454987524237E+03 0.40764346581450E+03 0.29821215609298E+03 0.29827559976524E+03 0.45266759747177E+03
 0.37643175669880E+03 0.22284645097387E+04 0.19636326880221E+04 0.47732677245703E+03 0.69821306517749E+03
 0.21849965885818E+03 0.13714930398414E+04 0.12216681116447E+04 0.11757173423869E+04 0.17000259703941E+04
 0.12805611409664E+04 0.12214048235527E+04 0.11179904751314E+04 0.16999134930410E+04 0.13714930398414E+04
 0.12216681116447E+04 0.11757173423869E+04 0.17000259703941E+04 0.12805611409664E+04 0.12214048235527E+04
 0.11179904751314E+04 0.16999134930410E+04 0.16472226822031E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51748656345734E+03 0.12948777724409E+01
 0.12948777724409E+01 0.75410891407604E+01 0.00000000000000E+00 0.35856958553354E+03 0.35856958553354E+03
 0.35856958553354E+03 0.35856958553354E+03 0.00000000000000E+00 0.00000000000000E+00 0.14235666151790E+00
 0.00000000000000E+00 -.14008412807842E+02 0.10000000000000E-02 0.15490851377360E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51643384892918E+01 0.19366269334844E+01 0.37643438230108E+03 0.45266553144343E+03
 0.32493385407877E+03 0.32493385407877E+03 0.29817142836435E+03 0.29817090699028E+03 0.32492358742328E+03
 0.32492358742328E+03 0.29817142716127E+03 0.29817090581647E+03 0.32493385407877E+03 0.32493385407877E+03
 0.29817142836435E+03 0.29817090699028E+03 0.32492358742328E+03 0.32492358742328E+03 0.29817142716127E+03
 0.29817090581647E+03 0.32141300463781E+03 0.29819965890716E+03 -.20047521591772E+03 -.27406015479882E+03
 0.29644221484752E+03 0.62668826968725E+03 0.32876384376550E+03 0.34552841033561E+03 0.26649329999641E+03
 0.34552841033561E+03 0.48631758623625E+03 0.34559194070890E+03 0.26620302111556E+03 0.34559194070890E+03
 0.48610844628365E+03 0.34552841033561E+03 0.26649329999641E+03 0.34552841033561E+03 0.48631758623625E+03
 0.34559194070890E+03 0.26620302111556E+03 0.34559194070890E+03 0.48610844628366E+03 0.23050092664979E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38586612489859E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18507408176960E+00 0.00000000000000E+00 0.00000000000000E+00 0.18507408176960E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19140355280432E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19140355280432E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639331969249E+00 0.19279134520165E+00 0.35856958553354E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1940.35080478
 0.10599324977123E+00 0.33470645070164E+03 0.47911645473701E+03 0.47401429286128E+03 0.47192261202993E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14153126963864E+00 0.00000000000000E+00 -.23521034809661E+02
 0.33417251176592E-02 0.11702800885284E+01 0.23939730882484E+04 0.89773990809316E+03 0.68359703616423E+01
 0.25634888856159E+01 0.38036223896327E+03 0.29838564844271E+03 0.36953113775539E+03 0.40795678139941E+03
 0.29822285575009E+03 0.29827947195135E+03 0.36478240315130E+03 0.40793839363118E+03 0.29821406621199E+03
 0.29827935020825E+03 0.36953113775539E+03 0.40795678139941E+03 0.29822285575009E+03 0.29827947195135E+03
 0.36478240315130E+03 0.40793839363118E+03 0.29821406621199E+03 0.29827935020825E+03 0.45293510976315E+03
 0.37666132489670E+03 0.22301010102451E+04 0.19643543682473E+04 0.47682828373096E+03 0.69708387307362E+03
 0.21787144792401E+03 0.13728763504097E+04 0.12221749253781E+04 0.11763678562963E+04 0.16997106362403E+04
 0.12820647562231E+04 0.12219120388580E+04 0.11188107008090E+04 0.16995983532067E+04 0.13728763504097E+04
 0.12221749253781E+04 0.11763678562963E+04 0.16997106362403E+04 0.12820647562231E+04 0.12219120388580E+04
 0.11188107008090E+04 0.16995983532067E+04 0.16469633498379E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51769202278367E+03 0.12948777920243E+01
 0.12948777920243E+01 0.75814224686924E+01 0.00000000000000E+00 0.35871252735231E+03 0.35871252735231E+03
 0.35871252735231E+03 0.35871252735231E+03 0.00000000000000E+00 0.00000000000000E+00 0.14231370913670E+00
 0.00000000000000E+00 -.14000648924779E+02 0.10000000000000E-02 0.15492289439720E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51638591127074E+01 0.19364471672653E+01 0.37666392063596E+03 0.45293307364602E+03
 0.32504949135707E+03 0.32504949135707E+03 0.29817209923178E+03 0.29817156153400E+03 0.32503916961332E+03
 0.32503916961332E+03 0.29817209793950E+03 0.29817156027316E+03 0.32504949135707E+03 0.32504949135707E+03
 0.29817209923178E+03 0.29817156153400E+03 0.32503916961332E+03 0.32503916961332E+03 0.29817209793950E+03
 0.29817156027316E+03 0.32151672438609E+03 0.29820103219879E+03 -.20166612244247E+03 -.27570343829374E+03
 0.29711096315824E+03 0.62772561032843E+03 0.32912909235440E+03 0.34656467073654E+03 0.26703299362569E+03
 0.34656467073654E+03 0.48703856287172E+03 0.34662863897615E+03 0.26674192598958E+03 0.34662863897615E+03
 0.48682907082248E+03 0.34656467073654E+03 0.26703299362569E+03 0.34656467073654E+03 0.48703856287172E+03
 0.34662863897615E+03 0.26674192598958E+03 0.34662863897615E+03 0.48682907082248E+03 0.23057175110393E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38602361841639E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18502378488283E+00 0.00000000000000E+00 0.00000000000000E+00 0.18502378488283E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19134275880419E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19134275880419E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639328868066E+00 0.19271450494685E+00 0.35871252735231E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1950.43413677
 0.10593330496712E+00 0.33484168543866E+03 0.47932197751524E+03 0.47422021923448E+03 0.47212847249810E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14149655736107E+00 0.00000000000000E+00 -.23516956801517E+02
 0.33436159472090E-02 0.11702389105943E+01 0.23926192859194E+04 0.89723223221978E+03 0.68362109032394E+01
 0.25635790887148E+01 0.38062213126854E+03 0.29839186446937E+03 0.36976667102987E+03 0.40825065509152E+03
 0.29822505129257E+03 0.29828330244449E+03 0.36501450567086E+03 0.40823228240931E+03 0.29821601765268E+03
 0.29828317795898E+03 0.36976667102987E+03 0.40825065509152E+03 0.29822505129257E+03 0.29828330244449E+03
 0.36501450567086E+03 0.40823228240931E+03 0.29821601765268E+03 0.29828317795898E+03 0.45320101520240E+03
 0.37688914561283E+03 0.22317337084295E+04 0.19651090973974E+04 0.47634047418049E+03 0.69597455725510E+03
 0.21725238070371E+03 0.13742559056203E+04 0.12226762400040E+04 0.11770441656425E+04 0.16993930125406E+04
 0.12835645050306E+04 0.12224137590210E+04 0.11196552539819E+04 0.16992809290219E+04 0.13742559056203E+04
 0.12226762400040E+04 0.11770441656425E+04 0.16993930125406E+04 0.12835645050306E+04 0.12224137590210E+04
 0.11196552539819E+04 0.16992809290219E+04 0.16467167547607E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51789740979450E+03 0.12948777619759E+01
 0.12948777619759E+01 0.76217557966244E+01 0.00000000000000E+00 0.35885452978681E+03 0.35885452978681E+03
 0.35885452978681E+03 0.35885452978681E+03 0.00000000000000E+00 0.00000000000000E+00 0.14227118746152E+00
 0.00000000000000E+00 -.13985131969111E+02 0.10000000000000E-02 0.15493691963319E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51633916686480E+01 0.19362718757430E+01 0.37689171955791E+03 0.45319900237665E+03
 0.32516496227260E+03 0.32516496227260E+03 0.29817278510056E+03 0.29817223071401E+03 0.32515458559057E+03
 0.32515458559057E+03 0.29817278371542E+03 0.29817222936257E+03 0.32516496227260E+03 0.32516496227260E+03
 0.29817278510056E+03 0.29817223071401E+03 0.32515458559057E+03 0.32515458559057E+03 0.29817278371542E+03
 0.29817222936257E+03 0.32162032440581E+03 0.29820243141672E+03 -.20284946565473E+03 -.27733539858224E+03
 0.29777253668967E+03 0.62874367947808E+03 0.32948228010497E+03 0.34759096306114E+03 0.26756582767257E+03
 0.34759096306114E+03 0.48774332211819E+03 0.34765536912324E+03 0.26727398133939E+03 0.34765536912324E+03
 0.48753348542456E+03 0.34759096306114E+03 0.26756582767257E+03 0.34759096306114E+03 0.48774332211819E+03
 0.34765536912324E+03 0.26727398133939E+03 0.34765536912324E+03 0.48753348542457E+03 0.23063580364722E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38617815880666E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18491970278440E+00 0.00000000000000E+00 0.00000000000000E+00 0.18491970278440E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19124633814395E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19124633814395E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639351518010E+00 0.19263849751560E+00 0.35885452978681E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1960.51746875
 0.10585886890182E+00 0.33497889424221E+03 0.47952711206874E+03 0.47442654178844E+03 0.47233508199468E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14146192212525E+00 0.00000000000000E+00 -.23520059121055E+02
 0.33459668909926E-02 0.11701976145168E+01 0.23909381833801E+04 0.89660181876755E+03 0.68364521519756E+01
 0.25636695569908E+01 0.38088151070101E+03 0.29839818853177E+03 0.37000182335993E+03 0.40854357447761E+03
 0.29822729308830E+03 0.29828721102588E+03 0.36524627494690E+03 0.40852521681094E+03 0.29821801087153E+03
 0.29828708376530E+03 0.37000182335993E+03 0.40854357447761E+03 0.29822729308830E+03 0.29828721102588E+03
 0.36524627494690E+03 0.40852521681094E+03 0.29821801087153E+03 0.29828708376530E+03 0.45346555596030E+03
 0.37711529594612E+03 0.22333744609199E+04 0.19658916286186E+04 0.47586729301212E+03 0.69489035009930E+03
 0.21664372062212E+03 0.13756378072820E+04 0.12231793546554E+04 0.11777346869765E+04 0.16990823248935E+04
 0.12850668045446E+04 0.12229172867397E+04 0.11205139164361E+04 0.16989704494595E+04 0.13756378072820E+04
 0.12231793546554E+04 0.11777346869765E+04 0.16990823248935E+04 0.12850668045446E+04 0.12229172867397E+04
 0.11205139164361E+04 0.16989704494595E+04 0.16464887268301E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51810336224110E+03 0.12948777848351E+01
 0.12948777848351E+01 0.76620891245564E+01 0.00000000000000E+00 0.35899520208345E+03 0.35899520208345E+03
 0.35899520208345E+03 0.35899520208345E+03 0.00000000000000E+00 0.00000000000000E+00 0.14222908999426E+00
 0.00000000000000E+00 -.13977740135849E+02 0.10000000000000E-02 0.15495074580785E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51629309418883E+01 0.19360991032081E+01 0.37711785102811E+03 0.45346356502828E+03
 0.32528017790832E+03 0.32528017790832E+03 0.29817348615339E+03 0.29817291470854E+03 0.32526974644511E+03
 0.32526974644511E+03 0.29817348467165E+03 0.29817291326286E+03 0.32528017790831E+03 0.32528017790831E+03
 0.29817348615339E+03 0.29817291470854E+03 0.32526974644511E+03 0.32526974644511E+03 0.29817348467165E+03
 0.29817291326286E+03 0.32172372876795E+03 0.29820385678900E+03 -.20402938553216E+03 -.27896274884675E+03
 0.29842401695342E+03 0.62974358902141E+03 0.32982745198323E+03 0.34860632801983E+03 0.26808898074296E+03
 0.34860632801983E+03 0.48843359759312E+03 0.34867117176911E+03 0.26779636538454E+03 0.34867117176911E+03
 0.48822342523382E+03 0.34860632801983E+03 0.26808898074296E+03 0.34860632801983E+03 0.48843359759312E+03
 0.34867117176911E+03 0.26779636538454E+03 0.34867117176911E+03 0.48822342523382E+03 0.23069226023674E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38633338256967E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18487270084673E+00 0.00000000000000E+00 0.00000000000000E+00 0.18487270084673E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19118381485787E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19118381485787E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639347691141E+00 0.19256298863832E+00 0.35899520208345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1970.60080073
 0.10579964832562E+00 0.33511049586491E+03 0.47973158905457E+03 0.47463130212126E+03 0.47253966898952E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14142741300565E+00 0.00000000000000E+00 -.23522130216552E+02
 0.33478396099492E-02 0.11701563019547E+01 0.23896007372114E+04 0.89610027645427E+03 0.68366935140513E+01
 0.25637600677692E+01 0.38114022547750E+03 0.29840462133871E+03 0.37023641560347E+03 0.40883556130568E+03
 0.29822958162082E+03 0.29829119843591E+03 0.36547753386588E+03 0.40881721856994E+03 0.29822004632241E+03
 0.29829106836776E+03 0.37023641560347E+03 0.40883556130568E+03 0.29822958162082E+03 0.29829119843591E+03
 0.36547753386588E+03 0.40881721856994E+03 0.29822004632241E+03 0.29829106836776E+03 0.45372877468846E+03
 0.37733993730612E+03 0.22349965634028E+04 0.19666144550841E+04 0.47540142511920E+03 0.69382000209646E+03
 0.21604156985167E+03 0.13770107424146E+04 0.12236775471380E+04 0.11783835300312E+04 0.16987701093700E+04
 0.12865591120541E+04 0.12234158897393E+04 0.11213303041257E+04 0.16986584406485E+04 0.13770107424146E+04
 0.12236775471380E+04 0.11783835300312E+04 0.16987701093700E+04 0.12865591120541E+04 0.12234158897393E+04
 0.11213303041257E+04 0.16986584406485E+04 0.16462462396967E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51830753713105E+03 0.12948778000957E+01
 0.12948778000957E+01 0.77024224524884E+01 0.00000000000000E+00 0.35913619198690E+03 0.35913619198690E+03
 0.35913619198690E+03 0.35913619198690E+03 0.00000000000000E+00 0.00000000000000E+00 0.14218738503084E+00
 0.00000000000000E+00 -.13969266933112E+02 0.10000000000000E-02 0.15496373256796E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51624982616443E+01 0.19359368481166E+01 0.37734247388355E+03 0.45372680593464E+03
 0.32539522398697E+03 0.32539522398697E+03 0.29817420257254E+03 0.29817361369545E+03 0.32538473790363E+03
 0.32538473790363E+03 0.29817420099037E+03 0.29817361215178E+03 0.32539522398697E+03 0.32539522398697E+03
 0.29817420257254E+03 0.29817361369545E+03 0.32538473790363E+03 0.32538473790363E+03 0.29817420099037E+03
 0.29817361215178E+03 0.32182700167553E+03 0.29820530854210E+03 -.20519324286429E+03 -.28056444185182E+03
 0.29907943430321E+03 0.63075142881698E+03 0.33017659734225E+03 0.34961876805290E+03 0.26861629210257E+03
 0.34961876805290E+03 0.48913120207203E+03 0.34968404918531E+03 0.26832291273121E+03 0.34968404918531E+03
 0.48892069785645E+03 0.34961876805290E+03 0.26861629210257E+03 0.34961876805290E+03 0.48913120207203E+03
 0.34968404918531E+03 0.26832291273121E+03 0.34968404918531E+03 0.48892069785645E+03 0.23075897698738E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38648847156026E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18481824467657E+00 0.00000000000000E+00 0.00000000000000E+00 0.18481824467657E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19111620014502E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19111620014502E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639347385027E+00 0.19248740524787E+00 0.35913619198690E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1980.68413272
 0.10574161548376E+00 0.33524326118018E+03 0.47993525603889E+03 0.47483526761091E+03 0.47274350402149E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14139314965856E+00 0.00000000000000E+00 -.23523502018828E+02
 0.33496768018498E-02 0.11701137577396E+01 0.23882901167008E+04 0.89560879376281E+03 0.68369420896772E+01
 0.25638532836289E+01 0.38139827998128E+03 0.29841116358816E+03 0.37047048040432E+03 0.40912660260117E+03
 0.29823191737057E+03 0.29829526540818E+03 0.36570831058919E+03 0.40910827471031E+03 0.29822212445665E+03
 0.29829513250006E+03 0.37047048040433E+03 0.40912660260117E+03 0.29823191737057E+03 0.29829526540818E+03
 0.36570831058919E+03 0.40910827471031E+03 0.29822212445665E+03 0.29829513250006E+03 0.45399063360573E+03
 0.37756311206421E+03 0.22366074859557E+04 0.19673403422024E+04 0.47494067100495E+03 0.69276073918945E+03
 0.21544536482948E+03 0.13783769867706E+04 0.12241679564605E+04 0.11790379839834E+04 0.16984522911524E+04
 0.12880443183990E+04 0.12239067108853E+04 0.11221509817190E+04 0.16983408315472E+04 0.13783769867706E+04
 0.12241679564605E+04 0.11790379839834E+04 0.16984522911524E+04 0.12880443183990E+04 0.12239067108853E+04
 0.11221509817190E+04 0.16983408315472E+04 0.16460038593231E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51851093706166E+03 0.12948778102038E+01
 0.12948778102038E+01 0.77427557804205E+01 0.00000000000000E+00 0.35927669065539E+03 0.35927669065539E+03
 0.35927669065539E+03 0.35927669065539E+03 0.00000000000000E+00 0.00000000000000E+00 0.14214606740021E+00
 0.00000000000000E+00 -.13960102152462E+02 0.10000000000000E-02 0.15497623823497E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51620816785285E+01 0.19357806294482E+01 0.37756562969479E+03 0.45398868701756E+03
 0.32551008690061E+03 0.32551008690061E+03 0.29817493453986E+03 0.29817432785214E+03 0.32549954635070E+03
 0.32549954635070E+03 0.29817493285336E+03 0.29817432620668E+03 0.32551008690061E+03 0.32551008690061E+03
 0.29817493453986E+03 0.29817432785214E+03 0.32549954635070E+03 0.32549954635070E+03 0.29817493285336E+03
 0.29817432620668E+03 0.32193013389379E+03 0.29820678690090E+03 -.20634841881913E+03 -.28215260517838E+03
 0.29973179076491E+03 0.63175243715904E+03 0.33052198744030E+03 0.35062469596553E+03 0.26914079843513E+03
 0.35062469596553E+03 0.48982353274174E+03 0.35069041430224E+03 0.26884666178561E+03 0.35069041430224E+03
 0.48961270201417E+03 0.35062469596553E+03 0.26914079843513E+03 0.35062469596553E+03 0.48982353274174E+03
 0.35069041430224E+03 0.26884666178561E+03 0.35069041430224E+03 0.48961270201417E+03 0.23082571981057E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38664292557746E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18475850415397E+00 0.00000000000000E+00 0.00000000000000E+00 0.18475850415397E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19104750552513E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19104750552513E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639349477442E+00 0.19241216907484E+00 0.35927669065539E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   1990.76746470
 0.10568309262342E+00 0.33537631005781E+03 0.48013819738543E+03 0.47503856940317E+03 0.47294671654002E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14135912755104E+00 0.00000000000000E+00 -.23524861860832E+02
 0.33515315518827E-02 0.11700698298060E+01 0.23869684280627E+04 0.89511316052352E+03 0.68371987690057E+01
 0.25639495383771E+01 0.38165571466619E+03 0.29841781596740E+03 0.37070405881540E+03 0.40941670141842E+03
 0.29823430081479E+03 0.29829941266938E+03 0.36593864512778E+03 0.40939838829129E+03 0.29822424572304E+03
 0.29829927688906E+03 0.37070405881540E+03 0.40941670141842E+03 0.29823430081479E+03 0.29829941266938E+03
 0.36593864512778E+03 0.40939838829129E+03 0.29822424572304E+03 0.29829927688906E+03 0.45425114339479E+03
 0.37778481539977E+03 0.22382097561798E+04 0.19680643298478E+04 0.47448671379984E+03 0.69171494979035E+03
 0.21485580242151E+03 0.13797377658128E+04 0.12246523771685E+04 0.11796919096418E+04 0.16981312108514E+04
 0.12895237319529E+04 0.12243915448791E+04 0.11229702767440E+04 0.16980199628914E+04 0.13797377658128E+04
 0.12246523771685E+04 0.11796919096418E+04 0.16981312108514E+04 0.12895237319529E+04 0.12243915448791E+04
 0.11229702767440E+04 0.16980199628914E+04 0.16457623795314E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51871369361434E+03 0.12948778202236E+01
 0.12948778202236E+01 0.77830891083525E+01 0.00000000000000E+00 0.35941663594698E+03 0.35941663594698E+03
 0.35941663594698E+03 0.35941663594698E+03 0.00000000000000E+00 0.00000000000000E+00 0.14210513002674E+00
 0.00000000000000E+00 -.13950965221988E+02 0.10000000000000E-02 0.15498830159711E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51616798929739E+01 0.19356299598652E+01 0.37778731394311E+03 0.45424921881189E+03
 0.32562475748420E+03 0.32562475748420E+03 0.29817568223676E+03 0.29817505735561E+03 0.32561416262396E+03
 0.32561416262396E+03 0.29817568044194E+03 0.29817505560447E+03 0.32562475748420E+03 0.32562475748420E+03
 0.29817568223676E+03 0.29817505735561E+03 0.32561416262396E+03 0.32561416262396E+03 0.29817568044194E+03
 0.29817505560447E+03 0.32203311773920E+03 0.29820829208873E+03 -.20749533765518E+03 -.28372796873763E+03
 0.30038057573433E+03 0.63274570260341E+03 0.33086322399041E+03 0.35162381622482E+03 0.26966200524741E+03
 0.35162381622482E+03 0.49050983269699E+03 0.35168997156444E+03 0.26936711827246E+03 0.35168997156444E+03
 0.49029868101570E+03 0.35162381622482E+03 0.26966200524741E+03 0.35162381622482E+03 0.49050983269699E+03
 0.35168997156444E+03 0.26936711827246E+03 0.35168997156444E+03 0.49029868101570E+03 0.23089186773513E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38679678005171E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18469892296266E+00 0.00000000000000E+00 0.00000000000000E+00 0.18469892296266E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19097903897393E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19097903897393E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639351610615E+00 0.19233728820100E+00 0.35941663594698E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
   2000.00000000
 0.10562891753101E+00 0.33549825077033E+03 0.48032345625847E+03 0.47522421302950E+03 0.47313231077093E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14132817229553E+00 0.00000000000000E+00 -.23525527906662E+02
 0.33532503431325E-02 0.11700284175015E+01 0.23857449284645E+04 0.89465434817417E+03 0.68374407666811E+01
 0.25640402875054E+01 0.38189092779253E+03 0.29842400302616E+03 0.37091755004624E+03 0.40968146379400E+03
 0.29823652487251E+03 0.29830328016478E+03 0.36614921909635E+03 0.40966316412883E+03 0.29822622574837E+03
 0.29830314172680E+03 0.37091755004624E+03 0.40968146379400E+03 0.29823652487251E+03 0.29830328016478E+03
 0.36614921909635E+03 0.40966316412883E+03 0.29822622574837E+03 0.29830314172680E+03 0.45448830042792E+03
 0.37798621307500E+03 0.22396703904584E+04 0.19687253190371E+04 0.47408232594547E+03 0.69077708562879E+03
 0.21432434805360E+03 0.13809795359029E+04 0.12250927451798E+04 0.11802894389913E+04 0.16978372801608E+04
 0.12908737129307E+04 0.12248322934135E+04 0.11237183118167E+04 0.16977262289524E+04 0.13809795359029E+04
 0.12250927451798E+04 0.11802894389913E+04 0.16978372801608E+04 0.12908737129307E+04 0.12248322934135E+04
 0.11237183118167E+04 0.16977262289524E+04 0.16455455839430E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51889885408864E+03 0.12948778251313E+01
 0.12948778251313E+01 0.78200192495563E+01 0.00000000000000E+00 0.35954430143493E+03 0.35954430143493E+03
 0.35954430143493E+03 0.35954430143493E+03 0.00000000000000E+00 0.00000000000000E+00 0.14206797368094E+00
 0.00000000000000E+00 -.13942045625606E+02 0.10000000000000E-02 0.15499896636020E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51613247416173E+01 0.19354967781065E+01 0.37798869457442E+03 0.45448639564907E+03
 0.32572964025133E+03 0.32572964025133E+03 0.29817627556251E+03 0.29817573875070E+03 0.32571899578663E+03
 0.32571899578663E+03 0.29817627367254E+03 0.29817573689935E+03 0.32572964025133E+03 0.32572964025133E+03
 0.29817627556251E+03 0.29817573875070E+03 0.32571899578663E+03 0.32571899578663E+03 0.29817627367254E+03
 0.29817573689935E+03 0.32212733873664E+03 0.29820969370533E+03 -.20853455624327E+03 -.28515371007947E+03
 0.30097112570226E+03 0.63364752421189E+03 0.33117154288112E+03 0.35253083163014E+03 0.27013604373082E+03
 0.35253083163014E+03 0.49113228123508E+03 0.35259738709872E+03 0.26984048035927E+03 0.35259738709872E+03
 0.49092084515079E+03 0.35253083163014E+03 0.27013604373082E+03 0.35253083163014E+03 0.49113228123508E+03
 0.35259738709872E+03 0.26984048035927E+03 0.35259738709872E+03 0.49092084515079E+03 0.23095282743219E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38693713986078E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18463993552491E+00 0.00000000000000E+00 0.00000000000000E+00 0.18463993552491E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19091631966282E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19091631966282E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19639355500729E+00 0.19226904889822E+00 0.35954430143493E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
