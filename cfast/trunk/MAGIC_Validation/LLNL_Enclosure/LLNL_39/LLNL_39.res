#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-39 0 MONOZONE(1=OUI,0=NON)                                                                     
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
250kW                                                                                               
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
1.000000 0.000500                                                                                   
4.000000 0.002000                                                                                   
8.000000 0.004000                                                                                   
10.000000 0.005000                                                                                  
53.300000 0.005000                                                                                  
63.900000 0.005000                                                                                  
74.600000 0.005000                                                                                  
85.200000 0.005000                                                                                  
400.000000 0.005000                                                                                 
500.000000 0.005000                                                                                 
501.000000 0.000000                                                                                 
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL39 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.083000                                                                                   
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
#CONDINIT 500.000000 10.000000 25.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-39           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-39           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-39           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-39           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-39           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-39           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-39           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-39           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-39           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-39           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-39           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-39           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-39           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-39           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-39           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-39           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-39           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-39           #HEAT_POWER#W#Total sprinkling power
#SECONDARYSOURCE#FOY_1 #250kW       #MASS_FLOW_RATE#kg/s#Pyrolysis rate
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #250kW       #HEAT_POWER#W#Heat Release Rate
#SECONDARYSOURCE#FOY_1 #250kW       #HEAT_POWER#W#Lower layer Heat Release Rate
#SECONDARYSOURCE#FOY_1 #250kW       #NET_HEAT_OF_COMBUSTION#J/kg#Net Heat of Combustion
#SECONDARYSOURCE#FOY_1 #250kW       #TEMPERATURE#K#Medium plume temperature
#SECONDARYSOURCE#FOY_1 #250kW       #HEIGHT#m#Flame height
#NOUVELLE LIGNE
#SECONDARYSOURCE#FOY_1 #250kW       #HEIGHT#m#Length of flame
#SECONDARYSOURCE#FOY_1 #250kW       #MASS#kg#Consumed combustible mass
#ROOM#LOC_2 #Plenum-LLNL39     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL39     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL39     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL39     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL39     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL39     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL39     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL39     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL39     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL39     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL39     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL39     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL39     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL39     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL39     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL39     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL39     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL39     #HEAT_POWER#W#Total sprinkling power
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.35045196282309E-12
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
 0.00000000000000E+00 -.29063493274102E-07 0.99965475690882E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.57020558861763E-03 0.57020558861763E-03
 0.70034304890048E-03 0.70384476414498E-03 0.00000000000000E+00 0.49841658112229E-03 0.54981368622481E-03
 0.49841658112229E-03 0.54981368622481E-03 0.49407851325873E-03 0.54980613517315E-03 0.49407851325873E-03
 0.54980613517315E-03 0.49841658112229E-03 0.54981368628329E-03 0.49841658112229E-03 0.54981368628329E-03
 0.49407851325873E-03 0.54980613523163E-03 0.49407851325873E-03 0.54980613523163E-03 0.45477649085609E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22335409399988E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22335409399988E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82999874435588E-01 0.98002456206611E-01 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     20.00000000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.32486265541185E+01
 0.99996793854869E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000217327E+03 0.29815000000000E+03 0.29815000298666E+03 0.29815000298666E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000297120E+03 0.29815000297120E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000298666E+03 0.29815000298666E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000297120E+03 0.29815000297120E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001033054E+03
 0.29815000853926E+03 0.50703384181497E-03 0.50703384181497E-03 0.65402227746585E-03 0.65729238885318E-03
 .00000000000000E+00 0.47343203091544E-03 0.56393724777302E-03 0.47343203091544E-03 0.56393724777302E-03
 0.47097358353933E-03 0.56401388490379E-03 0.47097358353933E-03 0.56401388490379E-03 0.47343203097392E-03
 0.56393724777302E-03 0.47343203097392E-03 0.56393724777302E-03 0.47097358353933E-03 0.56401388484531E-03
 0.47097358353933E-03 0.56401388484531E-03 0.56290748654988E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.35905411977657E+01 0.99963487166617E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815000851584E+03 0.29815001035896E+03
 0.29815000320712E+03 0.29815000320712E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000317898E+03
 0.29815000317898E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000320712E+03 0.29815000320712E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000317898E+03 0.29815000317898E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000309126E+03 0.29815000000000E+03 0.54259867223988E-03 0.54259867223988E-03
 0.71633334516026E-03 0.71991501188606E-03 .00000000000000E+00 0.50836020545038E-03 0.55802529815827E-03
 0.50836020545038E-03 0.55802529815827E-03 0.50389269882282E-03 0.55816981148301E-03 0.50389269882282E-03
 0.55816981148301E-03 0.50836020550886E-03 0.55802529815827E-03 0.50836020550886E-03 0.55802529815827E-03
 0.50389269876435E-03 0.55816981148301E-03 0.50389269876435E-03 0.55816981148301E-03 0.45497524503179E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97940527627990E-01 0.00000000000000E+00 0.00000000000000E+00 0.97940527627990E-01
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398015642E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398015642E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82950177441381E-01 0.97940304345295E-01 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     30.00000000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.32486265535373E+01
 0.99996793854869E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000271009E+03 0.29815000000000E+03 0.29815000371451E+03 0.29815000371451E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000369522E+03 0.29815000369522E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000371451E+03 0.29815000371451E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000369522E+03 0.29815000369522E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001266990E+03
 0.29815001048159E+03 0.51085698893492E-03 0.51085698893492E-03 0.64524699708553E-03 0.64847323207096E-03
 .00000000000000E+00 0.47542052609586E-03 0.56780454382995E-03 0.47542052609586E-03 0.56780454382995E-03
 0.47294070766877E-03 0.56790185227823E-03 0.47294070766877E-03 0.56790185227823E-03 0.47542052568652E-03
 0.56780454353757E-03 0.47542052568652E-03 0.56780454353757E-03 0.47294070772725E-03 0.56790185227823E-03
 0.47294070772725E-03 0.56790185227823E-03 0.56289641220976E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.35905411971990E+01 0.99964237490913E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001046210E+03 0.29815001269347E+03
 0.29815000398860E+03 0.29815000398860E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000395356E+03
 0.29815000395356E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000398860E+03 0.29815000398860E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000395356E+03 0.29815000395356E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000384647E+03 0.29815000000000E+03 0.53649715303725E-03 0.53649715303725E-03
 0.71976336391602E-03 0.72336218073560E-03 .00000000000000E+00 0.51048294671465E-03 0.55977907199505E-03
 0.51048294671465E-03 0.55977907199505E-03 0.50599056697707E-03 0.55996088352336E-03 0.50599056697707E-03
 0.55996088352336E-03 0.51048294665618E-03 0.55977907199505E-03 0.51048294665618E-03 0.55977907199505E-03
 0.50599056703555E-03 0.55996088352336E-03 0.50599056703555E-03 0.55996088352336E-03 0.45497203854962E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97940527619215E-01 0.00000000000000E+00 0.00000000000000E+00 0.97940527619215E-01
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398017692E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398017692E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82950177441389E-01 0.97940304345305E-01 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     40.00000000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.32486265530336E+01
 0.99996793854870E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000317232E+03 0.29815000000000E+03 0.29815000433835E+03 0.29815000433835E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000431575E+03 0.29815000431575E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000433835E+03 0.29815000433835E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000431575E+03 0.29815000431575E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001462133E+03
 0.29815001210579E+03 0.51402564655143E-03 0.51402564655143E-03 0.63800614557071E-03 0.64119617629856E-03
 .00000000000000E+00 0.47704944861570E-03 0.57098414924043E-03 0.47704944861570E-03 0.57098414924043E-03
 0.47455301640134E-03 0.57109924984212E-03 0.47455301640134E-03 0.57109924984212E-03 0.47704944867418E-03
 0.57098414929891E-03 0.47704944867418E-03 0.57098414929891E-03 0.47455301634287E-03 0.57109924978365E-03
 0.47455301634287E-03 0.57109924978365E-03 0.56288376292548E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.35905411966920E+01 0.99964970912800E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001209073E+03 0.29815001463947E+03
 0.29815000465839E+03 0.29815000465839E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000461743E+03
 0.29815000461743E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000465839E+03 0.29815000465839E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000461743E+03 0.29815000461743E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000449458E+03 0.29815000000000E+03 0.53147032609749E-03 0.53147032609749E-03
 0.72257987182159E-03 0.72619277118070E-03 .00000000000000E+00 0.51222502998505E-03 0.56121854182215E-03
 0.51222502998505E-03 0.56121854182215E-03 0.50771354230742E-03 0.56143234343881E-03 0.50771354230742E-03
 0.56143234343881E-03 0.51222502998505E-03 0.56121854182215E-03 0.51222502998505E-03 0.56121854182215E-03
 0.50771354236590E-03 0.56143234338033E-03 0.50771354236590E-03 0.56143234338033E-03 0.45496527382447E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97940527611538E-01 0.00000000000000E+00 0.00000000000000E+00 0.97940527611538E-01
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398017666E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398017666E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82950177441396E-01 0.97940304345313E-01 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     40.00025000
 0.30000000000000E+01 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000000000E+03
 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.32486265530314E+01
 0.99996793854870E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29815000317234E+03 0.29815000000000E+03 0.29815000433836E+03 0.29815000433836E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000431577E+03 0.29815000431577E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000433836E+03 0.29815000433836E+03 0.29815000000000E+03 0.29815000000000E+03
 0.29815000431577E+03 0.29815000431577E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815001462138E+03
 0.29815001210583E+03 0.51403181784282E-03 0.51403181784282E-03 0.63801470029124E-03 0.64120477379269E-03
 .00000000000000E+00 0.47705616537875E-03 0.57098905678954E-03 0.47705616537875E-03 0.57098905678954E-03
 0.47455978649524E-03 0.57110415803447E-03 0.47455978649524E-03 0.57110415803447E-03 0.47705616537875E-03
 0.57098905684802E-03 0.47705616537875E-03 0.57098905684802E-03 0.47455978643677E-03 0.57110415803447E-03
 0.47455978643677E-03 0.57110415803447E-03 0.56289135028071E-04 0.00000000000000E+00 0.12500000000060E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29815000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.31250000000298E-10 0.15000000000000E+01 0.29815000000000E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000000000E+03 0.22999999999999E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.35905411966421E+01 0.99964970931133E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815001209077E+03 0.29815001463951E+03
 0.29815000465841E+03 0.29815000465841E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000461745E+03
 0.29815000461745E+03 0.29815000000000E+03 0.29815000000000E+03 0.29815000465841E+03 0.29815000465841E+03
 0.29815000000000E+03 0.29815000000000E+03 0.29815000461745E+03 0.29815000461745E+03 0.29815000000000E+03
 0.29815000000000E+03 0.29815000449459E+03 0.29815000000000E+03 0.53147829625243E-03 0.53147829625243E-03
 0.72259622682455E-03 0.72620920795867E-03 .00000000000000E+00 0.51223626595273E-03 0.56122757923730E-03
 0.51223626595273E-03 0.56122757923730E-03 0.50772496475764E-03 0.56144138143873E-03 0.50772496475764E-03
 0.56144138143873E-03 0.51223626601121E-03 0.56122757923730E-03 0.51223626601121E-03 0.56122757923730E-03
 0.50772496475764E-03 0.56144138143873E-03 0.50772496475764E-03 0.56144138143873E-03 0.45497452465834E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29815000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.97940527611538E-01 0.00000000000000E+00 0.00000000000000E+00 0.97940527611538E-01
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398009357E-01 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97940398009357E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82950177441397E-01 0.97940304345314E-01 0.29815000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     50.01558018
 0.21955931140640E+01 0.29823061952914E+03 0.34115411446083E+03 0.30973993782630E+03 0.30864313374464E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22512907784364E+00 0.00000000000000E+00 0.75172793632219E+02
 0.10004713711065E-02 0.87402466835593E-01 0.79962308078360E+04 0.29985865529385E+04 0.91530597357719E+02
 0.34323974009145E+02 0.29976096894396E+03 0.29815000000001E+03 0.29961482854011E+03 0.30011542376404E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29925903855390E+03 0.30009967222330E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29961482854011E+03 0.30011542376404E+03 0.29815000000001E+03 0.29815000000001E+03
 0.29925903855390E+03 0.30009967222330E+03 0.29815000000001E+03 0.29815000000001E+03 0.30357566867477E+03
 0.29815494179792E+03 0.73912301081158E+03 0.73593609407148E+03 0.36832267629167E+03 0.74690096163403E+03
 0.37673667196089E+03 0.49722238378219E+03 0.24935637624443E+03 0.49495514325162E+03 0.63629458665810E+03
 0.37201365850725E+03 0.24326224936054E+03 0.37047686376777E+03 0.63037375886704E+03 0.49722238378219E+03
 0.24935637624443E+03 0.49495514325162E+03 0.63629458665810E+03 0.37201365850725E+03 0.24326224936054E+03
 0.37047686376777E+03 0.63037375886704E+03 0.65334755099194E+02 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.37149123100301E+03 0.14683041170173E+01
 0.14683041170173E+01 0.25178702988638E-01 0.13676334126449E+01 0.29821341997255E+03 0.30280985446899E+03
 0.29861902953808E+03 0.29861340845733E+03 0.23000000000000E+00 0.00000000000000E+00 0.22938481774407E+00
 0.00000000000000E+00 0.75418980675307E+02 0.10002168308392E-02 0.00000000000000E+00 0.79982657293297E+04
 0.29993496484986E+04 0.80000000000000E+04 0.30000000000000E+04 0.29815493010758E+03 0.30358904868162E+03
 0.29815261645360E+03 0.29829413881715E+03 0.29815000000001E+03 0.29815000000001E+03 0.29815264544696E+03
 0.29829414000763E+03 0.29815000000001E+03 0.29815000000001E+03 0.29815261645360E+03 0.29829413881715E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29815264544696E+03 0.29829414000763E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29820247223314E+03 0.29815000000001E+03 0.67119118884331E+00 0.69046458327283E+00
 0.83741009457968E+00 0.25347695665043E+02 0.24506098519991E+02 0.85810276562798E+00 -.22786413412313E-01
 0.89907241590246E+00 0.39547014377726E+02 0.86754083358950E+00 -.22567825281379E-01 0.90848533897784E+00
 0.39547221488923E+02 0.85810276562792E+00 -.22786413412197E-01 0.89907241590241E+00 0.39547014377726E+02
 0.86754083358950E+00 -.22567825281379E-01 0.90848533897784E+00 0.39547221488923E+02 0.75430514889406E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30756883287088E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44320388865633E+00 0.00000000000000E+00 0.00000000000000E+00 0.44320388865633E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.92817109864940E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.92817109864940E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.84036886953968E-01 0.99279678048860E-01 0.29821341997255E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     60.18374188
 0.16149163790855E+01 0.29828431495942E+03 0.37168890681937E+03 0.33217481425446E+03 0.32821046067438E+03
 0.22999999942132E+00 0.00000000000000E+00 0.22119079715945E+00 0.00000000000000E+00 0.52136716718060E+02
 0.10000640259631E-02 0.14898266589736E+00 0.79994878250874E+04 0.29998079344078E+04 0.53697522136645E+02
 0.20136570801242E+02 0.30116524521799E+03 0.29815000000001E+03 0.30116333290286E+03 0.30281645738503E+03
 0.29815000000001E+03 0.29815000000001E+03 0.30041446019324E+03 0.30278147946835E+03 0.29815000000001E+03
 0.29815000000001E+03 0.30116333290286E+03 0.30281645738503E+03 0.29815000000001E+03 0.29815000000001E+03
 0.30041446019324E+03 0.30278147946835E+03 0.29815000000001E+03 0.29815000000001E+03 0.31099888231886E+03
 0.29816253322886E+03 0.80217346896230E+03 0.79477265069763E+03 0.39999507077587E+03 0.10446860596920E+04
 0.64269101356220E+03 0.56941033987008E+03 0.34488738777654E+03 0.56336874085843E+03 0.94509512468065E+03
 0.42742187079660E+03 0.33798255683856E+03 0.42334726454371E+03 0.93855869151157E+03 0.56941033987008E+03
 0.34488738777654E+03 0.56336874085843E+03 0.94509512468065E+03 0.42742187079660E+03 0.33798255683856E+03
 0.42334726454371E+03 0.93855869151157E+03 0.86642180882549E+02 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.39843715142526E+03 0.14684894802001E+01
 0.14684894802001E+01 0.76019511501234E-01 0.12248308610792E+01 0.29819536519049E+03 0.30585828323243E+03
 0.29960109756330E+03 0.29957220659677E+03 0.23000000000000E+00 0.00000000000000E+00 0.22890509849705E+00
 0.00000000000000E+00 0.54300734708029E+02 0.10000689952612E-02 0.00000000000000E+00 0.79994480759902E+04
 0.29997930284963E+04 0.80000000000000E+04 0.30000000000000E+04 0.29816248809593E+03 0.31104674976811E+03
 0.29815623551897E+03 0.29843487025062E+03 0.29815000000001E+03 0.29815000000001E+03 0.29815628215553E+03
 0.29843488620656E+03 0.29815000000001E+03 0.29815000000001E+03 0.29815623551897E+03 0.29843487025062E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29815628215553E+03 0.29843488620656E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29829347527057E+03 0.29815000000001E+03 0.10767723648201E+01 0.10861529810999E+01
 0.74574673879269E+00 0.46359549186609E+02 0.45610073714122E+02 0.12678830084170E+01 -.32768924752013E+00
 0.12915198666172E+01 0.53742240568220E+02 0.12750620056921E+01 -.32259620699461E+00 0.12986627586230E+01
 0.53747202057950E+02 0.12678830084170E+01 -.32768924752013E+00 0.12915198666172E+01 0.53742240568220E+02
 0.12750620056922E+01 -.32259620699461E+00 0.12986627586230E+01 0.53747202057950E+02 0.14661797537156E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31316367272645E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36057558993987E+00 0.00000000000000E+00 0.00000000000000E+00 0.36057558993987E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.82639437903534E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.82639437903534E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83747805413909E-01 0.98923538868426E-01 0.29819536519049E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     70.04712498
 0.12032420002604E+01 0.29839686266805E+03 0.38748954654708E+03 0.35175619349403E+03 0.34604971225597E+03
 0.22999999996274E+00 0.00000000000000E+00 0.21863915040513E+00 0.00000000000000E+00 0.34980800217761E+02
 0.99951765158601E-03 0.18506682358994E+00 0.80000000000000E+04 0.30000000000000E+04 0.43227629052119E+02
 0.16210360894545E+02 0.30232606938721E+03 0.29815000000001E+03 0.30234741931347E+03 0.30521508895917E+03
 0.29815000000001E+03 0.29815000000001E+03 0.30132769970079E+03 0.30517021181675E+03 0.29815000000001E+03
 0.29815000000001E+03 0.30234741931347E+03 0.30521508895917E+03 0.29815000000001E+03 0.29815000000001E+03
 0.30132769970079E+03 0.30517021181675E+03 0.29815000000001E+03 0.29815000000001E+03 0.31707467565856E+03
 0.29817138991496E+03 0.92283949904015E+03 0.91165585336084E+03 0.43078842587721E+03 0.11914332594565E+04
 0.75849089144993E+03 0.63067514477188E+03 0.42837170030557E+03 0.62133699202345E+03 0.11256675249708E+04
 0.48090082322270E+03 0.42197646726988E+03 0.47457572936523E+03 0.11197379839153E+04 0.63067514477188E+03
 0.42837170030557E+03 0.62133699202345E+03 0.11256675249708E+04 0.48090082322270E+03 0.42197646726988E+03
 0.47457572936523E+03 0.11197379839153E+04 0.10437961049620E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.41354591510076E+03 0.20846546026918E+01
 0.20846546026918E+01 0.12533642699766E+00 0.10999466280148E+01 0.29818218599539E+03 0.30797899457724E+03
 0.30079501686730E+03 0.30073354797855E+03 0.23000000000000E+00 0.00000000000000E+00 0.22849981382669E+00
 0.00000000000000E+00 0.38731576092422E+02 0.99995955302848E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29817133190201E+03 0.31712376274239E+03
 0.29816017336941E+03 0.29856242959632E+03 0.29815000000001E+03 0.29815000000001E+03 0.29816022306366E+03
 0.29856247187957E+03 0.29815000000001E+03 0.29815000000001E+03 0.29816017336941E+03 0.29856242959632E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29816022306366E+03 0.29856247187957E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29839227391362E+03 0.29815000000001E+03 0.15348772825060E+01 0.15372248147180E+01
 0.68231004389558E+00 0.62093866428675E+02 0.61408144834560E+02 0.17310806538961E+01 -.50708460599390E+00
 0.17426120437120E+01 0.62866821920884E+02 0.17359213292332E+01 -.49731657954353E+00 0.17474195386816E+01
 0.62876262634520E+02 0.17310806538961E+01 -.50708460599384E+00 0.17426120437120E+01 0.62866821920884E+02
 0.17359213292332E+01 -.49731657954359E+00 0.17474195386816E+01 0.62876262634520E+02 0.20685559947963E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31712798293957E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.29871339531841E+00 0.00000000000000E+00 0.00000000000000E+00 0.29871339531841E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.80694164551535E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.80694164551535E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83534042919709E-01 0.98660242965365E-01 0.29818218599539E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     80.00581493
 0.88674569247463E+00 0.29859095681846E+03 0.39932359267028E+03 0.36954884902587E+03 0.36311470244161E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21631441970601E+00 0.00000000000000E+00 0.21719565682615E+02
 0.99873724661355E-03 0.21664150505783E+00 0.80000000000000E+04 0.30000000000000E+04 0.36927365316560E+02
 0.13847761993710E+02 0.30359328388147E+03 0.29815000000002E+03 0.30348282619540E+03 0.30754196382351E+03
 0.29815000000001E+03 0.29815000000001E+03 0.30223441542866E+03 0.30749208248684E+03 0.29815000000001E+03
 0.29815000000001E+03 0.30348282619540E+03 0.30754196382351E+03 0.29815000000001E+03 0.29815000000001E+03
 0.30223441542866E+03 0.30749208248684E+03 0.29815000000001E+03 0.29815000000001E+03 0.32263441421964E+03
 0.29818201817026E+03 0.10679367459917E+04 0.10525223926217E+04 0.46848031514843E+03 0.13071481037694E+04
 0.83632538704521E+03 0.69347348947337E+03 0.51080815438868E+03 0.68082159695550E+03 0.12783481168525E+04
 0.53885745956180E+03 0.50519313427105E+03 0.53023527482034E+03 0.12732476241621E+04 0.69347348947337E+03
 0.51080815438868E+03 0.68082159695550E+03 0.12783481168525E+04 0.53885745956180E+03 0.50519313427105E+03
 0.53023527482034E+03 0.12732476241621E+04 0.12158141428620E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.42678488600564E+03 0.18373667322543E+01
 0.18373667322543E+01 0.17512987673581E+00 0.98824506302124E+00 0.29817226863234E+03 0.30972443635921E+03
 0.30211352121036E+03 0.30201542379840E+03 0.23000000000000E+00 0.00000000000000E+00 0.22810487929120E+00
 0.00000000000000E+00 0.26854268945978E+02 0.99987559747037E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29818195076283E+03 0.32267490566938E+03
 0.29816479996190E+03 0.29868644473964E+03 0.29815000000001E+03 0.29815000000001E+03 0.29816484601422E+03
 0.29868651838718E+03 0.29815000000001E+03 0.29815000000001E+03 0.29816479996190E+03 0.29868644473964E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29816484601422E+03 0.29868651838718E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29849658821343E+03 0.29815000000002E+03 0.20329074235925E+01 0.20288372132751E+01
 0.65378288682036E+00 0.75667738779579E+02 0.75010686978325E+02 0.22434391066751E+01 -.62058123647394E+00
 0.22464253131960E+01 0.70515428760503E+02 0.22461933667204E+01 -.60702535020202E+00 0.22491563394047E+01
 0.70528432762690E+02 0.22434391066751E+01 -.62058123647388E+00 0.22464253131959E+01 0.70515428760503E+02
 0.22461933667202E+01 -.60702535020202E+00 0.22491563394045E+01 0.70528432762690E+02 0.26309254046439E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32064906562091E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24559933068978E+00 0.00000000000000E+00 0.00000000000000E+00 0.24559933068978E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.79583621535375E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.79583621535375E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83370600343564E-01 0.98458937286663E-01 0.29817226863234E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
     90.04162388
 0.64413132893638E+00 0.29890279876399E+03 0.40924255257016E+03 0.38555145515227E+03 0.37918803109521E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21397425203035E+00 0.00000000000000E+00 0.11464368135437E+02
 0.99759431919218E-03 0.24768865793250E+00 0.80000000000000E+04 0.30000000000000E+04 0.32298612567799E+02
 0.12111979712925E+02 0.30493960597119E+03 0.29815000000002E+03 0.30462193153278E+03 0.30986916807152E+03
 0.29815000000001E+03 0.29815000000001E+03 0.30317406531636E+03 0.30981673751252E+03 0.29815000000001E+03
 0.29815000000001E+03 0.30462193153278E+03 0.30986916807152E+03 0.29815000000001E+03 0.29815000000001E+03
 0.30317406531636E+03 0.30981673751252E+03 0.29815000000001E+03 0.29815000000001E+03 0.32800696563637E+03
 0.29819448769895E+03 0.12151901681576E+04 0.11954121266660E+04 0.51658439238090E+03 0.14122284047569E+04
 0.89306109041408E+03 0.75883882430239E+03 0.59798475658845E+03 0.74287576110150E+03 0.14171310446502E+04
 0.60139971536536E+03 0.59307379206955E+03 0.59046914653468E+03 0.14127564559303E+04 0.75883882430239E+03
 0.59798475658845E+03 0.74287576110150E+03 0.14171310446502E+04 0.60139971536536E+03 0.59307379206956E+03
 0.59046914653468E+03 0.14127564559303E+04 0.13805724974292E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.44059484245165E+03 0.16492146970032E+01
 0.16492146970032E+01 0.22530892149504E+00 0.89216530411346E+00 0.29816489940618E+03 0.31128830254138E+03
 0.30348280590863E+03 0.30334714277966E+03 0.23000000000000E+00 0.00000000000000E+00 0.22769457802715E+00
 0.00000000000000E+00 0.17792156196837E+02 0.99981087530933E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29819441035276E+03 0.32804176026657E+03
 0.29817025011713E+03 0.29881055553336E+03 0.29815000000001E+03 0.29815000000001E+03 0.29817028997631E+03
 0.29881066065309E+03 0.29815000000001E+03 0.29815000000001E+03 0.29817025011713E+03 0.29881055553336E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29817028997631E+03 0.29881066065309E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29860604195450E+03 0.29815000000002E+03 0.25676383372078E+01 0.25512462401402E+01
 0.66194407876424E+00 0.88210384282171E+02 0.87545130483013E+02 0.28070510657679E+01 -.68789938532710E+00
 0.28050444997507E+01 0.77829761862704E+02 0.28081909728207E+01 -.67165380807877E+00 0.28061656030484E+01
 0.77845232269354E+02 0.28070510657679E+01 -.68789938532715E+00 0.28050444997507E+01 0.77829761862704E+02
 0.28081909728208E+01 -.67165380807864E+00 0.28061656030484E+01 0.77845232269354E+02 0.31745662220488E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32386982512632E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.19824206221566E+00 0.00000000000000E+00 0.00000000000000E+00 0.19824206221566E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78882604391412E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78882604391412E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83245681345470E-01 0.96765954797696E-01 0.30290730312900E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    100.01841929
 0.44847732771354E+00 0.29938654338514E+03 0.41706781542640E+03 0.39947535462407E+03 0.39392038579266E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21168499562951E+00 0.00000000000000E+00 0.39625284279323E+01
 0.99590868853039E-03 0.27784137648754E+00 0.80000000000000E+04 0.30000000000000E+04 0.28793407595138E+02
 0.10797527848177E+02 0.30634522163321E+03 0.29815000000003E+03 0.30576769647292E+03 0.31215773400967E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30414610060365E+03 0.31210423226680E+03 0.29815000000002E+03
 0.29815000000002E+03 0.30576769647292E+03 0.31215773400967E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30414610060365E+03 0.31210423226680E+03 0.29815000000002E+03 0.29815000000002E+03 0.33309817762120E+03
 0.29820855231609E+03 0.13606914904577E+04 0.13368274314487E+04 0.56585440274787E+03 0.14949035318733E+04
 0.92621985711169E+03 0.82406396501712E+03 0.68212770194239E+03 0.80495574489342E+03 0.15381731649958E+04
 0.66531317597057E+03 0.67782149275991E+03 0.65223008225362E+03 0.15344109521670E+04 0.82406396501712E+03
 0.68212770194239E+03 0.80495574489342E+03 0.15381731649958E+04 0.66531317597057E+03 0.67782149275991E+03
 0.65223008225362E+03 0.15344109521670E+04 0.15305055828839E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.45110539128637E+03 0.14955177567934E+01
 0.14955177567934E+01 0.27519289854778E+00 0.81860215792329E+00 0.29815991326877E+03 0.31271810140429E+03
 0.30477319192216E+03 0.30460154516466E+03 0.23000000000000E+00 0.00000000000000E+00 0.22726185596474E+00
 0.00000000000000E+00 0.11299936709353E+02 0.99976352212939E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29820846451588E+03 0.33313104594837E+03
 0.29817637179724E+03 0.29893587312320E+03 0.29815000000002E+03 0.29815000000002E+03 0.29817640316681E+03
 0.29893600894243E+03 0.29815000000002E+03 0.29815000000002E+03 0.29817637179724E+03 0.29893587312320E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29817640316681E+03 0.29893600894243E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29871856627167E+03 0.29815000000003E+03 0.31108128394203E+01 0.30802701412857E+01
 0.66932035940256E+00 0.99794991593932E+02 0.99122324632732E+02 0.33833437832077E+01 -.74762031600367E+00
 0.33749130429365E+01 0.85056042919208E+02 0.33828836925416E+01 -.72913002405682E+00 0.33744326212931E+01
 0.85073536799534E+02 0.33833437832077E+01 -.74762031600372E+00 0.33749130429365E+01 0.85056042919208E+02
 0.33828836925416E+01 -.72913002405682E+00 0.33744326212931E+01 0.85073536799534E+02 0.36834928017998E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32660512146291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.15752434128308E+00 0.00000000000000E+00 0.00000000000000E+00 0.15752434128308E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78543150740093E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78543150740093E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83156071960228E-01 0.95872786749433E-01 0.30538056984238E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    110.00193976
 0.29169382445325E+00 0.30015228815519E+03 0.42362350592494E+03 0.41161824201789E+03 0.40733137410798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20934661643445E+00 0.00000000000000E+00 -.15530832930158E+01
 0.12132924866225E-02 0.30851370628456E+00 0.65936285670653E+04 0.24726107126495E+04 0.25930776613928E+02
 0.97240412302230E+01 0.30781269803826E+03 0.29815000000003E+03 0.30693499462217E+03 0.31442029322819E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30516083639774E+03 0.31436663522032E+03 0.29815000000002E+03
 0.29815000000002E+03 0.30693499462217E+03 0.31442029322819E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30516083639774E+03 0.31436663522032E+03 0.29815000000002E+03 0.29815000000002E+03 0.33797145354500E+03
 0.29822442326438E+03 0.15035211591067E+04 0.14764552776331E+04 0.61586276098048E+03 0.15661439611126E+04
 0.94720188632726E+03 0.88941977378168E+03 0.76423189987322E+03 0.86762023304373E+03 0.16483350443559E+04
 0.73061645390222E+03 0.76044264057234E+03 0.71584873422491E+03 0.16450886526995E+04 0.88941977378168E+03
 0.76423189987322E+03 0.86762023304373E+03 0.16483350443559E+04 0.73061645390222E+03 0.76044264057234E+03
 0.71584873422491E+03 0.16450886526995E+04 0.16694338635684E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.46103743339737E+03 0.14689217320477E+01
 0.14689217320477E+01 0.32511050091893E+00 0.75396019712441E+00 0.29815675847851E+03 0.31404546270745E+03
 0.30605916232578E+03 0.30585301370005E+03 0.23000000000000E+00 0.00000000000000E+00 0.22679871541358E+00
 0.00000000000000E+00 0.66398561960699E+01 0.99972810882709E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29822432476374E+03 0.33800453838467E+03
 0.29818314989981E+03 0.29906268438818E+03 0.29815000000002E+03 0.29815000000002E+03 0.29818317073463E+03
 0.29906285027337E+03 0.29815000000002E+03 0.29815000000002E+03 0.29818314989981E+03 0.29906268438818E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29818317073463E+03 0.29906285027337E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29883415656140E+03 0.29815000000003E+03 0.36652173948233E+01 0.36154335316290E+01
 0.68456377857231E+00 0.11068648427417E+03 0.10999849767670E+03 0.39796991739808E+01 -.79517966225259E+00
 0.39641145074556E+01 0.91891620699672E+02 0.39777285444188E+01 -.77485406075176E+00 0.39621282886102E+01
 0.91910731616190E+02 0.39796991739808E+01 -.79517966225252E+00 0.39641145074556E+01 0.91891620699672E+02
 0.39777285444188E+01 -.77485406075169E+00 0.39621282886101E+01 0.91910731616190E+02 0.41742199300979E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32908893131247E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12129170538561E+00 0.00000000000000E+00 0.00000000000000E+00 0.12129170538561E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78421821802008E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78421821802008E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83091690725778E-01 0.95568660249058E-01 0.30610111054848E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    120.34431568
 0.16809762773661E+00 0.30147735417086E+03 0.42969234205955E+03 0.42250813029143E+03 0.41969109434636E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20677799350537E+00 0.00000000000000E+00 -.56463318801491E+01
 0.21053718977527E-02 0.34203138509079E+00 0.37998037346938E+04 0.14249264005102E+04 0.23389666412854E+02
 0.87711249048202E+01 0.30938340036084E+03 0.29815000000003E+03 0.30816960832995E+03 0.31673687281915E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30625762431557E+03 0.31668363210454E+03 0.29815000000002E+03
 0.29815000000002E+03 0.30816960832995E+03 0.31673687281915E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30625762431557E+03 0.31668363210454E+03 0.29815000000002E+03 0.29815000000002E+03 0.34285378077782E+03
 0.29824317646182E+03 0.16473459925557E+04 0.16192072766054E+04 0.66973379032184E+03 0.16361490632599E+04
 0.96306660398645E+03 0.95767516801695E+03 0.84952681013717E+03 0.93436867043008E+03 0.17557002178001E+04
 0.79986834110807E+03 0.84619054567261E+03 0.78467708397742E+03 0.17528991847470E+04 0.95767516801695E+03
 0.84952681013717E+03 0.93436867043008E+03 0.17557002178001E+04 0.79986834110807E+03 0.84619054567261E+03
 0.78467708397742E+03 0.17528991847470E+04 0.18044452710903E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.47316794963787E+03 0.14689546995864E+01
 0.14689546995864E+01 0.37682238048582E+00 0.69374781075195E+00 0.29815510868253E+03 0.31533275397254E+03
 0.30738811809666E+03 0.30714848085704E+03 0.23000000000000E+00 0.00000000000000E+00 0.22628190479543E+00
 0.00000000000000E+00 0.32863399289993E+01 0.99970054359624E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29824306709532E+03 0.34288876138991E+03
 0.29819075649892E+03 0.29919454389190E+03 0.29815000000002E+03 0.29815000000002E+03 0.29819076444971E+03
 0.29919473996360E+03 0.29815000000002E+03 0.29815000000002E+03 0.29819075649892E+03 0.29919454389190E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29819076444971E+03 0.29919473996360E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29895563578257E+03 0.29815000000003E+03 0.42391932688204E+01 0.41684289025517E+01
 0.70572972974149E+00 0.12137648553637E+03 0.12066722715798E+03 0.46089868317640E+01 -.83522951416606E+00
 0.45857954370561E+01 0.98610090492485E+02 0.46054579224074E+01 -.81333178381781E+00 0.45822599719016E+01
 0.98630552602285E+02 0.46089868317640E+01 -.83522951416601E+00 0.45857954370561E+01 0.98610090492486E+02
 0.46054579224074E+01 -.81333178381774E+00 0.45822599719016E+01 0.98630552602285E+02 0.46669100434616E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33147320146456E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.87190194248335E-01 0.00000000000000E+00 0.00000000000000E+00 0.87190194248335E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78373149491919E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78373149491919E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.83019477397764E-01 0.95272489789321E-01 0.30677566725247E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    130.13989908
 0.84987354029454E-01 0.30364430291966E+03 0.43507177319051E+03 0.43134854887548E+03 0.42980163350041E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20413398227321E+00 0.00000000000000E+00 -.83254157620293E+01
 0.41642260662384E-02 0.37632079293084E+00 0.19211252878080E+04 0.72042198292801E+03 0.21258458608399E+02
 0.79719219781494E+01 0.31093031569224E+03 0.29815000000003E+03 0.30938328665878E+03 0.31895174369446E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30735797956813E+03 0.31889929121237E+03 0.29815000000002E+03
 0.29815000000002E+03 0.30938328665878E+03 0.31895174369446E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30735797956813E+03 0.31889929121237E+03 0.29815000000002E+03 0.29815000000002E+03 0.34741134485203E+03
 0.29826433660048E+03 0.17786210150621E+04 0.17535015902669E+04 0.72345595586158E+03 0.17009342732872E+04
 0.97386103764632E+03 0.10230455679413E+04 0.93214346567983E+03 0.10010467634961E+04 0.18597251210471E+04
 0.86685043726148E+03 0.92917062143986E+03 0.85409430110807E+03 0.18572781488002E+04 0.10230455679413E+04
 0.93214346567983E+03 0.10010467634961E+04 0.18597251210471E+04 0.86685043726148E+03 0.92917062143985E+03
 0.85409430110807E+03 0.18572781488002E+04 0.19287345550879E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.48318655617494E+03 0.14689762782743E+01
 0.14689762782743E+01 0.42580029750255E+00 0.64266868840887E+00 0.29815487689443E+03 0.31648787481401E+03
 0.30863317899563E+03 0.30836422049551E+03 0.23000000000000E+00 0.00000000000000E+00 0.22575157258528E+00
 0.00000000000000E+00 0.11795822152329E+01 0.99968052839588E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29826421577173E+03 0.34744922253675E+03
 0.29819869536730E+03 0.29932087937837E+03 0.29815000000002E+03 0.29815000000002E+03 0.29819869113203E+03
 0.29932110068356E+03 0.29815000000002E+03 0.29815000000002E+03 0.29819869536730E+03 0.29932087937837E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29819869113203E+03 0.29932110068356E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29907364751171E+03 0.29815000000003E+03 0.47941918263321E+01 0.46996104886457E+01
 0.76359715741928E+00 0.13109014628076E+03 0.13032273113755E+03 0.52498867491309E+01 -.84274442725678E+00
 0.52193032644702E+01 0.10476065323726E+03 0.52453452182783E+01 -.82027303123892E+00 0.52147654794100E+01
 0.10478150211026E+03 0.52498867491309E+01 -.84274442725715E+00 0.52193032644702E+01 0.10476065323726E+03
 0.52453452182783E+01 -.82027303123904E+00 0.52147654794100E+01 0.10478150211026E+03 0.51224512477884E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33359347386356E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.56689963754089E-01 0.00000000000000E+00 0.00000000000000E+00 0.56689963754089E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78402523274543E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78402523274543E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82989164195572E-01 0.94784457283827E-01 0.30823621230542E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    140.00311818
 0.36578881407002E-01 0.30725739813542E+03 0.44053632883236E+03 0.43891126409902E+03 0.43821861857749E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20113410611898E+00 0.00000000000000E+00 -.99796700225811E+01
 0.96751308770000E-02 0.41482549034148E+00 0.82686219976805E+03 0.31007332491302E+03 0.19285217968198E+02
 0.72319567380741E+01 0.31254234640559E+03 0.29815000000003E+03 0.31072327599287E+03 0.32123549045440E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30856584478046E+03 0.32118406335592E+03 0.29815000000002E+03
 0.29815000000002E+03 0.31072327599287E+03 0.32123549045440E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30856584478046E+03 0.32118406335592E+03 0.29815000000002E+03 0.29815000000002E+03 0.35201396326124E+03
 0.29829013907311E+03 0.19052746798545E+04 0.18890185602833E+04 0.78359977872205E+03 0.17731728209985E+04
 0.98565504338288E+03 0.10895658407149E+04 0.10213049316773E+04 0.10895658407149E+04 0.19742159804934E+04
 0.93533671227212E+03 0.10186446044069E+04 0.93533671227212E+03 0.19720715694346E+04 0.10895658407149E+04
 0.10213049316773E+04 0.10895658407149E+04 0.19742159804934E+04 0.93533671227212E+03 0.10186446044069E+04
 0.93533671227212E+03 0.19720715694346E+04 0.20560239115324E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49298988135356E+03 0.14689896028684E+01
 0.14689896028684E+01 0.47511639302279E+00 0.59990835571602E+00 0.29815595509316E+03 0.31760210267386E+03
 0.30982483172709E+03 0.30952819178248E+03 0.23000000000000E+00 0.00000000000000E+00 0.22517027551650E+00
 0.00000000000000E+00 -.19285945580958E-01 0.99966508129389E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29829000494302E+03 0.35205541830926E+03
 0.29820739173072E+03 0.29944960216568E+03 0.29815000000002E+03 0.29815000000002E+03 0.29820737532364E+03
 0.29944984481047E+03 0.29815000000002E+03 0.29815000000002E+03 0.29820739173072E+03 0.29944960216568E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29820737532364E+03 0.29944984481047E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29919469654082E+03 0.29815000000003E+03 0.53502841010178E+01 0.52261798327006E+01
 0.85579533588046E+00 0.14055512967101E+03 0.13969505535845E+03 0.59334966983444E+01 -.82622205122078E+00
 0.58955093394579E+01 0.11090604035131E+03 0.59282275105240E+01 -.80396248536908E+00 0.58902555004702E+01
 0.11092651544718E+03 0.59334966983444E+01 -.82622205122078E+00 0.58955093394579E+01 0.11090604035131E+03
 0.59282275105239E+01 -.80396248536920E+00 0.58902555004702E+01 0.11092651544718E+03 0.55661209599110E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33560060025551E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.26267721501558E-01 0.00000000000000E+00 0.00000000000000E+00 0.26267721501558E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78398067051229E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78398067051229E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82971449610937E-01 0.93206018415862E-01 0.31338555732787E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    150.04432132
 0.15952353033114E-01 0.31114587750565E+03 0.44657652810912E+03 0.44585638225914E+03 0.44554531531475E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19772686886022E+00 0.00000000000000E+00 -.10536524492231E+02
 0.22185107336345E-01 0.45666370901364E+00 0.36060226703947E+03 0.13522585013980E+03 0.17518361634822E+02
 0.65693856130582E+01 0.31333274103188E+03 0.29815000000003E+03 0.31130630388578E+03 0.32373671260725E+03
 0.29815000000002E+03 0.29815000000002E+03 0.30934363694609E+03 0.32362802943046E+03 0.29815000000002E+03
 0.29815000000002E+03 0.31130630388578E+03 0.32373671260725E+03 0.29815000000002E+03 0.29815000000002E+03
 0.30934363694609E+03 0.32362802943046E+03 0.29815000000002E+03 0.29815000000002E+03 0.35678948646224E+03
 0.29833118973891E+03 0.16325109293412E+04 0.16275331004027E+04 0.85065605914290E+03 0.18539902169121E+04
 0.99908087747352E+03 0.92220579561544E+03 0.11432020629466E+04 0.92220579561544E+03 0.21255915291972E+04
 0.86296254913240E+03 0.11233228925468E+04 0.86296254913240E+03 0.21068046749676E+04 0.92220579561544E+03
 0.11432020629466E+04 0.92220579561544E+03 0.21255915291972E+04 0.86296254913240E+03 0.11233228925468E+04
 0.86296254913240E+03 0.21068046749676E+04 0.21025206718752E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.10574444695668E+06 0.50050000000000E+08 0.49869860530430E+03 0.13934633006044E+01
 0.13934633006044E+01 0.52532240871030E+00 0.56944548809946E+00 0.29815865857739E+03 0.31869309922733E+03
 0.31089760284149E+03 0.31057302351365E+03 0.23000000000000E+00 0.00000000000000E+00 0.22451931707261E+00
 0.00000000000000E+00 -.19661393226246E+00 0.99965426696975E-03 0.12129474611775E-01 0.80000000000000E+04
 0.30000000000000E+04 0.65955041385170E+03 0.24733140519439E+03 0.29833103152852E+03 0.35683505286946E+03
 0.29821915950007E+03 0.29958739866114E+03 0.29815000000002E+03 0.29815000000002E+03 0.29821908297905E+03
 0.29958765700170E+03 0.29815000000002E+03 0.29815000000002E+03 0.29821915950007E+03 0.29958739866114E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29821908297905E+03 0.29958765700170E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29932294560351E+03 0.29815000000003E+03 0.73857316677948E+01 0.72122005174935E+01
 0.28288433639909E+01 0.15175808900540E+03 0.14891510142459E+03 0.75657602289329E+01 0.10658270543919E+01
 0.75186136534232E+01 0.11911042949159E+03 0.75389420583833E+01 0.10873363689348E+01 0.74918710107557E+01
 0.11913002818903E+03 0.75657602289329E+01 0.10658270543919E+01 0.75186136534232E+01 0.11911042949159E+03
 0.75389420583834E+01 0.10873363689348E+01 0.74918710107558E+01 0.11913002818903E+03 0.61177147859727E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33755593308992E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.16996488807858E-01 0.38909355370753E-02 0.00000000000000E+00 0.16996488807858E-01 0.38909355370753E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78518658997454E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78518658997454E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82967832720905E-01 0.91649601661454E-01 0.31869309922733E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    160.19757363
 0.96874722780481E-02 0.31129824916001E+03 0.45219020043231E+03 0.45173523814159E+03 0.45153028906052E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19409939949893E+00 0.00000000000000E+00 -.11157624817700E+02
 0.36532139718320E-01 0.50005649252660E+00 0.21898525686378E+03 0.82119471323919E+02 0.15998192443376E+02
 0.59993221662659E+01 0.31404013402118E+03 0.29815000000003E+03 0.31182594740840E+03 0.32632887520948E+03
 0.29815000000002E+03 0.29815000000002E+03 0.31008764863733E+03 0.32616971906418E+03 0.29815000000002E+03
 0.29815000000002E+03 0.31182594740840E+03 0.32632887520948E+03 0.29815000000002E+03 0.29815000000002E+03
 0.31008764863733E+03 0.32616971906418E+03 0.29815000000002E+03 0.29815000000002E+03 0.36166469873844E+03
 0.29840291443747E+03 0.16138860478285E+04 0.16071606179559E+04 0.91811206301303E+03 0.19267686483232E+04
 0.10040660249951E+04 0.90793554600640E+03 0.12458489417618E+04 0.90793554600640E+03 0.22524751302423E+04
 0.87699882043320E+03 0.12233086825371E+04 0.87699882043320E+03 0.22315353751571E+04 0.90793554600640E+03
 0.12458489417618E+04 0.90793554600640E+03 0.22524751302423E+04 0.87699882043320E+03 0.12233086825371E+04
 0.87699882043320E+03 0.22315353751571E+04 0.21919897752122E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.61438621029029E+05 0.50050000000000E+08 0.50428557427091E+03 0.14244101497934E+01
 0.14244101497934E+01 0.57608867023277E+00 0.54284903374159E+00 0.29816268012823E+03 0.31971690139579E+03
 0.31191644260369E+03 0.31156579364054E+03 0.23000000000000E+00 0.00000000000000E+00 0.22379251631025E+00
 0.00000000000000E+00 -.52010720780706E+00 0.99963759126697E-03 0.32242069988481E-01 0.80000000000000E+04
 0.30000000000000E+04 0.24812302692905E+03 0.93046135098392E+02 0.29840262402249E+03 0.36171436071474E+03
 0.29823732644726E+03 0.29973890888413E+03 0.29815000000002E+03 0.29815000000002E+03 0.29823706275746E+03
 0.29973917927149E+03 0.29815000000002E+03 0.29815000000002E+03 0.29823732644726E+03 0.29973890888413E+03
 0.29815000000002E+03 0.29815000000002E+03 0.29823706275746E+03 0.29973917927149E+03 0.29815000000002E+03
 0.29815000000002E+03 0.29946113451880E+03 0.29815000000003E+03 0.10577366158647E+02 0.10307669173929E+02
 0.63302206912715E+01 0.16380674072585E+03 0.15744486893112E+03 0.99842576117027E+01 0.44680783726319E+01
 0.99221431062497E+01 0.12843042402685E+03 0.99191129230712E+01 0.44886884595508E+01 0.98572763769433E+01
 0.12844902290208E+03 0.99842576117026E+01 0.44680783726320E+01 0.99221431062496E+01 0.12843042402685E+03
 0.99191129230711E+01 0.44886884595508E+01 0.98572763769432E+01 0.12844902290208E+03 0.67448771553583E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33918637359162E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.48899032790766E-02 0.17772504338199E-01 0.00000000000000E+00 0.48899032790766E-02 0.17772504338199E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.78873496409818E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.78873496409818E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82962280318419E-01 0.91349714443272E-01 0.31971690139579E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    170.00041780
 0.11714770118696E-01 0.30972487300651E+03 0.45612821258484E+03 0.45555651876225E+03 0.45528783692710E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19055404992378E+00 0.00000000000000E+00 -.11800695669592E+02
 0.30210048594663E-01 0.54320513890368E+00 0.26481254986836E+03 0.99304706200637E+02 0.14727401173240E+02
 0.55227754399650E+01 0.31502820598256E+03 0.29815000000006E+03 0.31262361084694E+03 0.32876506358405E+03
 0.29814999999999E+03 0.29814999999999E+03 0.31097883345326E+03 0.32858600954367E+03 0.29814999999999E+03
 0.29814999999999E+03 0.31262361084694E+03 0.32876506358405E+03 0.29814999999999E+03 0.29814999999999E+03
 0.31097883345326E+03 0.32858600954366E+03 0.29814999999999E+03 0.29814999999999E+03 0.36615245894455E+03
 0.29849651524698E+03 0.17372272861388E+04 0.17209866784565E+04 0.96750244619942E+03 0.19647342831416E+04
 0.99239432471115E+03 0.97871603853051E+03 0.13193447070420E+04 0.97871603853051E+03 0.23365763505816E+04
 0.93758332657634E+03 0.13007525990519E+04 0.93758332657634E+03 0.23197824531641E+04 0.97871603853051E+03
 0.13193447070420E+04 0.97871603853051E+03 0.23365763505816E+04 0.93758332657633E+03 0.13007525990519E+04
 0.93758332657633E+03 0.23197824531641E+04 0.22792840915811E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.75775862030564E+05 0.50050000000000E+08 0.50817323378938E+03 0.14136690421249E+01
 0.14136690421249E+01 0.62510289109169E+00 0.51777909817495E+00 0.29816865567909E+03 0.32058329563984E+03
 0.31284607426331E+03 0.31247484100060E+03 0.23000000000000E+00 0.00000000000000E+00 0.22303280827183E+00
 0.00000000000000E+00 -.98430115077206E+00 0.99961297659707E-03 0.52137134392420E-01 0.80000000000000E+04
 0.30000000000000E+04 0.15344149795013E+03 0.57540561731298E+02 0.29849608777029E+03 0.36620462168498E+03
 0.29825948656008E+03 0.29989243859011E+03 0.29814999999999E+03 0.29814999999999E+03 0.29825895733096E+03
 0.29989271758707E+03 0.29814999999999E+03 0.29814999999999E+03 0.29825948656008E+03 0.29989243859011E+03
 0.29814999999999E+03 0.29814999999999E+03 0.29825895733096E+03 0.29989271758707E+03 0.29814999999999E+03
 0.29814999999999E+03 0.29959915257193E+03 0.29815000000006E+03 0.13885994345421E+02 0.13477804651982E+02
 0.10074975049693E+02 0.17454496515125E+03 0.16441961522631E+03 0.12522171958299E+02 0.80992731327539E+01
 0.12441769372755E+02 0.13687802771612E+03 0.12416510285015E+02 0.81190018835294E+01 0.12336701181810E+02
 0.13689567240838E+03 0.12522171958299E+02 0.80992731327539E+01 0.12441769372755E+02 0.13687802771612E+03
 0.12416510285015E+02 0.81190018835293E+01 0.12336701181810E+02 0.13689567240838E+03 0.73299808652344E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34026814670919E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.41354315437419E-01 0.00000000000000E+00 0.00000000000000E+00 0.41354315437419E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.79419424883825E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.79419424883825E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82955000903814E-01 0.91094425572653E-01 0.32058329563984E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    180.00159517
 0.19097115662317E-01 0.30869438687969E+03 0.45830700143589E+03 0.45735461163432E+03 0.45689737295910E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18701706387950E+00 0.00000000000000E+00 -.12816306373498E+02
 0.18531787149058E-01 0.58797907364646E+00 0.43169069100853E+03 0.16188400912820E+03 0.13605926398684E+02
 0.51022223995064E+01 0.31641550685024E+03 0.29815000000012E+03 0.31379257253498E+03 0.33105601246841E+03
 0.29814999999997E+03 0.29814999999997E+03 0.31208736240303E+03 0.33088273803886E+03 0.29814999999997E+03
 0.29814999999997E+03 0.31379257253498E+03 0.33105601246841E+03 0.29814999999997E+03 0.29814999999997E+03
 0.31208736240303E+03 0.33088273803886E+03 0.29814999999997E+03 0.29814999999997E+03 0.37023840593405E+03
 0.29861566540925E+03 0.19477425039892E+04 0.19209198130329E+04 0.99514112447458E+03 0.19639008583753E+04
 0.96378402827831E+03 0.11023379228619E+04 0.13644436562457E+04 0.11023379228619E+04 0.23778601821237E+04
 0.10265873041704E+04 0.13526370099767E+04 0.10265873041704E+04 0.23677880089364E+04 0.11023379228619E+04
 0.13644436562457E+04 0.11023379228619E+04 0.23778601821237E+04 0.10265873041704E+04 0.13526370099767E+04
 0.10265873041704E+04 0.23677880089364E+04 0.23516836481146E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.12798450404326E+06 0.50050000000000E+08 0.51029647483043E+03 0.13747827343710E+01
 0.13747827343710E+01 0.67510877794371E+00 0.49286835582404E+00 0.29817760845020E+03 0.32132961611998E+03
 0.31372235481716E+03 0.31333563539708E+03 0.23000000000000E+00 0.00000000000000E+00 0.22220010383557E+00
 0.00000000000000E+00 -.19312103057786E+01 0.99957361856987E-03 0.72986955026141E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10960862796831E+03 0.41103235488116E+02 0.29861513416730E+03 0.37029064174572E+03
 0.29828578589502E+03 0.30005111930338E+03 0.29814999999997E+03 0.29814999999997E+03 0.29828491274303E+03
 0.30005140777633E+03 0.29814999999997E+03 0.29814999999997E+03 0.29828578589502E+03 0.30005111930338E+03
 0.29814999999997E+03 0.29814999999997E+03 0.29828491274303E+03 0.30005140777633E+03 0.29814999999997E+03
 0.29814999999997E+03 0.29974074035167E+03 0.29815000000012E+03 0.17404017861224E+02 0.16803305378539E+02
 0.14139229849238E+02 0.18441785133684E+03 0.17020792533836E+03 0.15219289759297E+02 0.12058364054901E+02
 0.15117967372616E+02 0.14469974434502E+03 0.15068872930368E+02 0.12077949818778E+02 0.14968586471826E+02
 0.14471716963853E+03 0.15219289759297E+02 0.12058364054901E+02 0.15117967372616E+02 0.14469974434502E+03
 0.15068872930368E+02 0.12077949818778E+02 0.14968586471826E+02 0.14471716963853E+03 0.78925163398611E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34110607481249E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.67372065603261E-01 0.00000000000000E+00 0.00000000000000E+00 0.67372065603261E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.80223809294495E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.80223809294495E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82941215785989E-01 0.90866897551187E-01 0.32132961611998E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    190.00432073
 0.26933519061305E-01 0.30864097537526E+03 0.45887708178587E+03 0.45752828610730E+03 0.45688046292066E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18364459106767E+00 0.00000000000000E+00 -.13573830787007E+02
 0.13139899507338E-01 0.63285686886098E+00 0.60883266234515E+03 0.22831224837943E+03 0.12641088994417E+02
 0.47404083729065E+01 0.31804461837639E+03 0.29815000000021E+03 0.31518877752176E+03 0.33309972561554E+03
 0.29815000000001E+03 0.29815000000001E+03 0.31331351403054E+03 0.33294584238896E+03 0.29815000000001E+03
 0.29815000000001E+03 0.31518877752176E+03 0.33309972561554E+03 0.29815000000001E+03 0.29815000000001E+03
 0.31331351403054E+03 0.33294584238896E+03 0.29815000000001E+03 0.29815000000001E+03 0.37370554931773E+03
 0.29875967614346E+03 0.21448297204804E+04 0.21099734474059E+04 0.10011630484136E+04 0.19300791069465E+04
 0.92391024329092E+03 0.12195078160867E+04 0.13846361563115E+04 0.12195078160867E+04 0.23824011921089E+04
 0.11078103529635E+04 0.13785419126884E+04 0.11078103529635E+04 0.23778401110785E+04 0.12195078160868E+04
 0.13846361563115E+04 0.12195078160868E+04 0.23824011921089E+04 0.11078103529635E+04 0.13785419126884E+04
 0.11078103529635E+04 0.23778401110785E+04 0.23914636618976E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.18340428591440E+06 0.50050000000000E+08 0.51088910959442E+03 0.13318431506313E+01
 0.13318431506313E+01 0.72512240576284E+00 0.46876071371174E+00 0.29819125633827E+03 0.32192869282943E+03
 0.31451057438191E+03 0.31411445225930E+03 0.23000000000000E+00 0.00000000000000E+00 0.22131134747703E+00
 0.00000000000000E+00 -.27019210518187E+01 0.99952026367404E-03 0.94446034466771E-01 0.80000000000000E+04
 0.30000000000000E+04 0.84704456308482E+02 0.31764171115681E+02 0.29875906697100E+03 0.37375594990852E+03
 0.29831559524632E+03 0.30021000775665E+03 0.29815000000001E+03 0.29815000000001E+03 0.29831431108417E+03
 0.30021030603025E+03 0.29815000000001E+03 0.29815000000001E+03 0.29831559524632E+03 0.30021000775665E+03
 0.29815000000001E+03 0.29815000000001E+03 0.29831431108417E+03 0.30021030603025E+03 0.29815000000001E+03
 0.29815000000001E+03 0.29988162022576E+03 0.29815000000020E+03 0.20995880374502E+02 0.20145621374358E+02
 0.18396512292921E+02 0.19306666585572E+03 0.17457817100133E+03 0.18006025848311E+02 0.16210036994792E+02
 0.17883946967587E+02 0.15160986277061E+03 0.17809038981319E+02 0.16229594161675E+02 0.17688556543709E+02
 0.15162718379782E+03 0.18006025848311E+02 0.16210036994793E+02 0.17883946967587E+02 0.15160986277061E+03
 0.17809038981319E+02 0.16229594161675E+02 0.17688556543709E+02 0.15162718379782E+03 0.84110918281220E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34169043915270E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.82418270464038E-01 0.00000000000000E+00 0.00000000000000E+00 0.82418270464038E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.81233187809044E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.81233187809044E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82930067409516E-01 0.90684922454950E-01 0.32192869282943E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    200.00262464
 0.32279296575545E-01 0.30917080294913E+03 0.45853925850634E+03 0.45693208894768E+03 0.45616795036061E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18044563600984E+00 0.00000000000000E+00 -.13800015265935E+02
 0.10963802357450E-01 0.67712207871782E+00 0.72967386123703E+03 0.27362769796388E+03 0.11814708531065E+02
 0.44305156991494E+01 0.31969014542518E+03 0.29815000000028E+03 0.31661016312108E+03 0.33490767346140E+03
 0.29815000000003E+03 0.29815000000003E+03 0.31453170390697E+03 0.33477457972118E+03 0.29815000000003E+03
 0.29815000000003E+03 0.31661016312108E+03 0.33490767346140E+03 0.29815000000003E+03 0.29815000000003E+03
 0.31453170390697E+03 0.33477457972118E+03 0.29815000000003E+03 0.29815000000003E+03 0.37659553115791E+03
 0.29892899693036E+03 0.22747416606239E+04 0.22343395426625E+04 0.99404235421296E+03 0.18803345659940E+04
 0.88132200001003E+03 0.12976828505367E+04 0.13906365368246E+04 0.12976828505367E+04 0.23672410189504E+04
 0.11621412529989E+04 0.13878549936192E+04 0.11621412529989E+04 0.23657786474502E+04 0.12976828505367E+04
 0.13906365368246E+04 0.12976828505367E+04 0.23672410189504E+04 0.11621412529989E+04 0.13878549936192E+04
 0.11621412529989E+04 0.23657786474502E+04 0.24002642725750E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22121012727897E+06 0.50050000000000E+08 0.51062258107145E+03 0.13708009954815E+01
 0.13708009954815E+01 0.77511392531319E+00 0.44555565802003E+00 0.29821135548927E+03 0.32238280870163E+03
 0.31520299020741E+03 0.31480351286953E+03 0.23000000000000E+00 0.00000000000000E+00 0.22037126739440E+00
 0.00000000000000E+00 -.29759637850460E+01 0.99945019287739E-03 0.11651224314260E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68662312081731E+02 0.25748367030649E+02 0.29892832384801E+03 0.37664351362977E+03
 0.29834863490471E+03 0.30036709885156E+03 0.29815000000003E+03 0.29815000000003E+03 0.29834688332710E+03
 0.30036740515494E+03 0.29815000000003E+03 0.29815000000003E+03 0.29834863490471E+03 0.30036709885156E+03
 0.29815000000003E+03 0.29815000000003E+03 0.29834688332710E+03 0.30036740515494E+03 0.29815000000003E+03
 0.29815000000003E+03 0.30002017947894E+03 0.29815000000027E+03 0.24580859876184E+02 0.23420606246220E+02
 0.22782667104027E+02 0.20048222623782E+03 0.17758564579828E+03 0.20860170593763E+02 0.20479071901709E+02
 0.20720380687781E+02 0.15758516988960E+03 0.20616554018830E+02 0.20498048079530E+02 0.20479020910750E+02
 0.15760185045787E+03 0.20860170593763E+02 0.20479071901709E+02 0.20720380687781E+02 0.15758516988960E+03
 0.20616554018829E+02 0.20498048079530E+02 0.20479020910749E+02 0.15760185045787E+03 0.88808858481857E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34208073626345E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.86970852817531E-01 0.00000000000000E+00 0.00000000000000E+00 0.86970852817531E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.82310520895839E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.82310520895839E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82925983549050E-01 0.90552477124009E-01 0.32238280870163E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    210.00314773
 0.34598452926085E-01 0.31004001799401E+03 0.45797588641846E+03 0.45626976902521E+03 0.45546948605992E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17737569759985E+00 0.00000000000000E+00 -.13655115772647E+02
 0.10228893803182E-01 0.72034887038928E+00 0.78209825558182E+03 0.29328684584318E+03 0.11105729916223E+02
 0.41646487185838E+01 0.32120507322879E+03 0.29815000000040E+03 0.31792262336103E+03 0.33653474942690E+03
 0.29815000000009E+03 0.29815000000009E+03 0.31566148067517E+03 0.33641799665816E+03 0.29815000000009E+03
 0.29815000000009E+03 0.31792262336103E+03 0.33653474942690E+03 0.29815000000009E+03 0.29815000000009E+03
 0.31566148067517E+03 0.33641799665816E+03 0.29815000000009E+03 0.29815000000009E+03 0.37905810411487E+03
 0.29912409979555E+03 0.23342709305241E+04 0.22906302508645E+04 0.98287770226403E+03 0.18301469828420E+04
 0.84235489206665E+03 0.13342392849442E+04 0.13923407023984E+04 0.13342392849442E+04 0.23478186407393E+04
 0.11890701769051E+04 0.13908696641777E+04 0.11890701769051E+04 0.23474990794547E+04 0.13342392849442E+04
 0.13923407023984E+04 0.13342392849442E+04 0.23478186407393E+04 0.11890701769051E+04 0.13908696641777E+04
 0.11890701769051E+04 0.23474990794547E+04 0.23901864446812E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.23761141911249E+06 0.50050000000000E+08 0.51012064060205E+03 0.14271618071561E+01
 0.14271618071561E+01 0.82511654075003E+00 0.42327012287231E+00 0.29823955773588E+03 0.32270688575157E+03
 0.31580269312786E+03 0.31540530904354E+03 0.23000000000000E+00 0.00000000000000E+00 0.21938428667011E+00
 0.00000000000000E+00 -.28775559844259E+01 0.99935665342177E-03 0.13917712063559E+00 0.80000000000000E+04
 0.30000000000000E+04 0.57480712084469E+02 0.21555267031676E+02 0.29912337056638E+03 0.37910428462216E+03
 0.29838471989957E+03 0.30052118484083E+03 0.29815000000009E+03 0.29815000000009E+03 0.29838245633286E+03
 0.30052149406436E+03 0.29815000000009E+03 0.29815000000009E+03 0.29838471989957E+03 0.30052118484083E+03
 0.29815000000009E+03 0.29815000000009E+03 0.29838245633286E+03 0.30052149406436E+03 0.29815000000009E+03
 0.29815000000009E+03 0.30015549795525E+03 0.29815000000036E+03 0.28088023182193E+02 0.26554724345152E+02
 0.27241786720136E+02 0.20678418269605E+03 0.17940618704231E+03 0.23768203330518E+02 0.24799814784196E+02
 0.23616634387128E+02 0.16270057747125E+03 0.23479420323231E+02 0.24817078517032E+02 0.23330838661782E+02
 0.16271552680243E+03 0.23768203330518E+02 0.24799814784196E+02 0.23616634387128E+02 0.16270057747125E+03
 0.23479420323233E+02 0.24817078517032E+02 0.23330838661784E+02 0.16271552680243E+03 0.93029105603486E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34234618016023E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.85045878489966E-01 0.00000000000000E+00 0.00000000000000E+00 0.85045878489966E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.83329107688980E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.83329107688980E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82927219708875E-01 0.90462976556976E-01 0.32270688575157E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    220.00299144
 0.34702854074082E-01 0.31105467177094E+03 0.45760627263475E+03 0.45591101969505E+03 0.45512582957908E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17438211718779E+00 0.00000000000000E+00 -.13405313394933E+02
 0.10198121690155E-01 0.76239804493150E+00 0.78445818191436E+03 0.29417181821788E+03 0.10493206341733E+02
 0.39349523781499E+01 0.32254038687550E+03 0.29815000000054E+03 0.31907933115939E+03 0.33804118079862E+03
 0.29815000000016E+03 0.29815000000016E+03 0.31667772350395E+03 0.33793534018295E+03 0.29815000000016E+03
 0.29815000000016E+03 0.31907933115939E+03 0.33804118079862E+03 0.29815000000016E+03 0.29815000000016E+03
 0.31667772350395E+03 0.33793534018295E+03 0.29815000000016E+03 0.29815000000016E+03 0.38125347158164E+03
 0.29934517077587E+03 0.23460039457436E+04 0.23007951417734E+04 0.97353974043108E+03 0.17881473672193E+04
 0.80973992808612E+03 0.13422340763705E+04 0.13957066230125E+04 0.13422340763705E+04 0.23331319852322E+04
 0.11976300656549E+04 0.13944193593919E+04 0.11976300656549E+04 0.23328840208007E+04 0.13422340763705E+04
 0.13957066230125E+04 0.13422340763705E+04 0.23331319852322E+04 0.11976300656549E+04 0.13944193593919E+04
 0.11976300656549E+04 0.23328840208007E+04 0.23738877193826E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.23834975385031E+06 0.50050000000000E+08 0.50977894331273E+03 0.14296413421434E+01
 0.14296413421434E+01 0.87511575930740E+00 0.40190527476192E+00 0.29827764918693E+03 0.32292283263168E+03
 0.31631947981573E+03 0.31592871564117E+03 0.23000000000000E+00 0.00000000000000E+00 0.21835547864669E+00
 0.00000000000000E+00 -.26552993832187E+01 0.99923122353268E-03 0.16239433430658E+00 0.80000000000000E+04
 0.30000000000000E+04 0.49262802388765E+02 0.18473550895787E+02 0.29934439019561E+03 0.38129892850514E+03
 0.29842366222215E+03 0.30067148776855E+03 0.29815000000016E+03 0.29815000000016E+03 0.29842085427103E+03
 0.30067179148814E+03 0.29815000000016E+03 0.29815000000016E+03 0.29842366222215E+03 0.30067148776855E+03
 0.29815000000016E+03 0.29815000000016E+03 0.29842085427103E+03 0.30067179148814E+03 0.29815000000016E+03
 0.29815000000016E+03 0.30028702433648E+03 0.29815000000044E+03 0.31454688755169E+02 0.29484667795500E+02
 0.31717726106662E+02 0.21214507269670E+03 0.18026875795951E+03 0.26714231457301E+02 0.29111722107327E+02
 0.26559939109405E+02 0.16707737797285E+03 0.26382850553725E+02 0.29125837534729E+02 0.26232302710663E+02
 0.16708922486955E+03 0.26714231457301E+02 0.29111722107327E+02 0.26559939109405E+02 0.16707737797285E+03
 0.26382850553725E+02 0.29125837534729E+02 0.26232302710663E+02 0.16708922486955E+03 0.96809917552367E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34254037874604E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.80873753891106E-01 0.00000000000000E+00 0.00000000000000E+00 0.80873753891106E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.84216102387235E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.84216102387235E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82930300913396E-01 0.90406038947795E-01 0.32292283263168E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    230.00277667
 0.33770504983364E-01 0.31205079761658E+03 0.45756899194354E+03 0.45593091764131E+03 0.45517957977659E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17142506820475E+00 0.00000000000000E+00 -.13215393197555E+02
 0.10479674987311E-01 0.80338173555337E+00 0.76338245314729E+03 0.28626841993023E+03 0.99579062430260E+01
 0.37342148411348E+01 0.32371810784317E+03 0.29815000000066E+03 0.32009827406239E+03 0.33947376032347E+03
 0.29815000000022E+03 0.29815000000022E+03 0.31759593443715E+03 0.33937476284151E+03 0.29815000000022E+03
 0.29815000000022E+03 0.32009827406239E+03 0.33947376032347E+03 0.29815000000022E+03 0.29815000000022E+03
 0.31759593443715E+03 0.33937476284151E+03 0.29815000000022E+03 0.29815000000022E+03 0.38330123245109E+03
 0.29959237002750E+03 0.23374758370434E+04 0.22914185500689E+04 0.96804551286399E+03 0.17562103049427E+04
 0.78332456451435E+03 0.13379532196128E+04 0.14026270151564E+04 0.13379532196128E+04 0.23257453766960E+04
 0.11985443828561E+04 0.14011319939625E+04 0.11985443828561E+04 0.23252188264877E+04 0.13379532196128E+04
 0.14026270151564E+04 0.13379532196128E+04 0.23257453766960E+04 0.11985443828561E+04 0.14011319939625E+04
 0.11985443828561E+04 0.23252188264877E+04 0.23597049211817E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.23175609339097E+06 0.50050000000000E+08 0.50974466028845E+03 0.14073130000303E+01
 0.14073130000303E+01 0.92511468543718E+00 0.38143486475208E+00 0.29832774451044E+03 0.32305435340468E+03
 0.31676662625844E+03 0.31638601966390E+03 0.23000000000000E+00 0.00000000000000E+00 0.21728875404354E+00
 0.00000000000000E+00 -.24716560913763E+01 0.99906524358849E-03 0.18612410475046E+00 0.80000000000000E+04
 0.30000000000000E+04 0.42982073765919E+02 0.16118277662220E+02 0.29959154154015E+03 0.38334680416651E+03
 0.29846529136263E+03 0.30081772503982E+03 0.29815000000022E+03 0.29815000000022E+03 0.29846191719571E+03
 0.30081801242144E+03 0.29815000000022E+03 0.29815000000022E+03 0.29846529136263E+03 0.30081772503982E+03
 0.29815000000022E+03 0.29815000000022E+03 0.29846191719571E+03 0.30081801242144E+03 0.29815000000022E+03
 0.29815000000022E+03 0.30041461728970E+03 0.29815000000047E+03 0.34633282049907E+02 0.32164259035371E+02
 0.36162008289216E+02 0.21674398630593E+03 0.18040116797527E+03 0.29682000524571E+02 0.33367256751133E+02
 0.29537384475175E+02 0.17084932373566E+03 0.29311171544329E+02 0.33376774598728E+02 0.29171022302512E+02
 0.17085670055443E+03 0.29682000524571E+02 0.33367256751133E+02 0.29537384475175E+02 0.17084932373566E+03
 0.29311171544329E+02 0.33376774598728E+02 0.29171022302512E+02 0.17085670055443E+03 0.10020199656639E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34269438924027E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77263631399134E-01 0.00000000000000E+00 0.00000000000000E+00 0.77263631399134E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.84954804096033E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.84954804096033E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82932944935927E-01 0.90372278200331E-01 0.32305435340468E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    240.00058166
 0.32730784286573E-01 0.31292169911960E+03 0.45781857260266E+03 0.45623770983273E+03 0.45551732477669E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16848743655481E+00 0.00000000000000E+00 -.13131511875615E+02
 0.10812569476993E-01 0.84348613935618E+00 0.73987963887976E+03 0.27745486457991E+03 0.94844474932407E+01
 0.35566678099653E+01 0.32478929594408E+03 0.29815000000087E+03 0.32102480730083E+03 0.34085865515882E+03
 0.29815000000027E+03 0.29815000000027E+03 0.31844781968093E+03 0.34076425712477E+03 0.29815000000027E+03
 0.29815000000027E+03 0.32102480730083E+03 0.34085865515882E+03 0.29815000000027E+03 0.29815000000027E+03
 0.31844781968093E+03 0.34076425712477E+03 0.29815000000027E+03 0.29815000000027E+03 0.38526483710783E+03
 0.29986574921434E+03 0.23288657589259E+04 0.22818476403004E+04 0.96579617424707E+03 0.17321848615289E+04
 0.76155970641060E+03 0.13334338308947E+04 0.14123693220692E+04 0.13334338308947E+04 0.23242069305021E+04
 0.11995502286588E+04 0.14106904929378E+04 0.11995502286588E+04 0.23234485788320E+04 0.13334338308947E+04
 0.14123693220692E+04 0.13334338308947E+04 0.23242069305021E+04 0.11995502286588E+04 0.14106904929378E+04
 0.11995502286588E+04 0.23234485788320E+04 0.23508756147107E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22440309083491E+06 0.50050000000000E+08 0.50998795456673E+03 0.13819600537076E+01
 0.13819600537076E+01 0.97510371042444E+00 0.36183393019020E+00 0.29839226763819E+03 0.32312235825020E+03
 0.31715690099680E+03 0.31678911135644E+03 0.23000000000000E+00 0.00000000000000E+00 0.21618730440795E+00
 0.00000000000000E+00 -.23785884379040E+01 0.99885012758083E-03 0.21032923426369E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38035606548021E+02 0.14263352455508E+02 0.29986487572161E+03 0.38531080442592E+03
 0.29850942533654E+03 0.30095984412833E+03 0.29815000000026E+03 0.29815000000026E+03 0.29850547176255E+03
 0.30096010306434E+03 0.29815000000026E+03 0.29815000000026E+03 0.29850942533654E+03 0.30095984412833E+03
 0.29815000000026E+03 0.29815000000026E+03 0.29850547176255E+03 0.30096010306434E+03 0.29815000000026E+03
 0.29815000000026E+03 0.30053830553560E+03 0.29815000000049E+03 0.37587779962952E+02 0.34561212605799E+02
 0.40533273939404E+02 0.22072893597651E+03 0.17999299566741E+03 0.32654659153330E+02 0.37529206220740E+02
 0.32535214268717E+02 0.17413109784144E+03 0.32247751561229E+02 0.37532849390000E+02 0.32133363684611E+02
 0.17413281746378E+03 0.32654659153330E+02 0.37529206220740E+02 0.32535214268717E+02 0.17413109784144E+03
 0.32247751561229E+02 0.37532849390000E+02 0.32133363684611E+02 0.17413281746378E+03 0.10325065032498E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34282061299968E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.75322261411702E-01 0.00000000000000E+00 0.00000000000000E+00 0.75322261411702E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.85563385241610E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.85563385241610E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934404736638E-01 0.90354931743500E-01 0.32312235825020E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    250.00250899
 0.32054237458712E-01 0.31364315305891E+03 0.45823919418736E+03 0.45669422224138E+03 0.45599302231919E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16556754308627E+00 0.00000000000000E+00 -.13134154802794E+02
 0.11040781521384E-01 0.88289911476025E+00 0.72458638770323E+03 0.27171989538871E+03 0.90610579014709E+01
 0.33978967130516E+01 0.32580357144860E+03 0.29815000000122E+03 0.32190358864793E+03 0.34220661661285E+03
 0.29815000000027E+03 0.29815000000028E+03 0.31926364497374E+03 0.34211594842873E+03 0.29815000000027E+03
 0.29815000000028E+03 0.32190358864793E+03 0.34220661661285E+03 0.29815000000027E+03 0.29815000000028E+03
 0.31926364497374E+03 0.34211594842873E+03 0.29815000000027E+03 0.29815000000028E+03 0.38716562123475E+03
 0.30016552664251E+03 0.23294001694507E+04 0.22809145056404E+04 0.96518541963004E+03 0.17127072306030E+04
 0.74269588387479E+03 0.13342375379966E+04 0.14232491971163E+04 0.13342375379966E+04 0.23256805650989E+04
 0.12040851221130E+04 0.14215496680064E+04 0.12040851221130E+04 0.23248625417396E+04 0.13342375379966E+04
 0.14232491971163E+04 0.13342375379966E+04 0.23256805650989E+04 0.12040851221130E+04 0.14215496680064E+04
 0.12040851221130E+04 0.23248625417396E+04 0.23469935828816E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.21961848803734E+06 0.50050000000000E+08 0.51040290292454E+03 0.13651946011265E+01
 0.13651946011265E+01 0.10251133470630E+01 0.34306528453163E+00 0.29847396418959E+03 0.32314321995211E+03
 0.31750110978721E+03 0.31714810887526E+03 0.23000000000000E+00 0.00000000000000E+00 0.21505268276991E+00
 0.00000000000000E+00 -.23641243116114E+01 0.99857687075304E-03 0.23500156892377E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34042325915684E+02 0.12765872218381E+02 0.30016461096793E+03 0.38721175171659E+03
 0.29855592210767E+03 0.30109802640906E+03 0.29815000000026E+03 0.29815000000026E+03 0.29855138190517E+03
 0.30109824440344E+03 0.29815000000026E+03 0.29815000000026E+03 0.29855592210767E+03 0.30109802640906E+03
 0.29815000000026E+03 0.29815000000026E+03 0.29855138190517E+03 0.30109824440344E+03 0.29815000000026E+03
 0.29815000000026E+03 0.30065830451538E+03 0.29815000000051E+03 0.40293400417817E+02 0.36656231362109E+02
 0.44801363048091E+02 0.22421121915712E+03 0.17918584929379E+03 0.35619071448243E+02 0.41572538017587E+02
 0.35541951166333E+02 0.17701113869074E+03 0.35179441469558E+02 0.41569273189492E+02 0.35107644539074E+02
 0.17700625950131E+03 0.35619071448243E+02 0.41572538017587E+02 0.35541951166333E+02 0.17701113869074E+03
 0.35179441469558E+02 0.41569273189492E+02 0.35107644539074E+02 0.17700625950131E+03 0.10599252508331E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34292135593202E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.74932947278734E-01 0.00000000000000E+00 0.00000000000000E+00 0.74932947278734E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.86071591209005E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.86071591209005E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934825281172E-01 0.90349569592938E-01 0.32314321995211E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    260.00077492
 0.31820268186889E-01 0.31425306613421E+03 0.45872335510532E+03 0.45719099399195E+03 0.45649738211700E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16267656176533E+00 0.00000000000000E+00 -.13182970397236E+02
 0.11121961152239E-01 0.92167763529794E+00 0.71929760322799E+03 0.26973660121050E+03 0.86798243698449E+01
 0.32549341386918E+01 0.32679116975449E+03 0.29815000000184E+03 0.32276186950443E+03 0.34351713233666E+03
 0.29815000000028E+03 0.29815000000031E+03 0.32006108313917E+03 0.34343001937464E+03 0.29815000000028E+03
 0.29815000000031E+03 0.32276186950443E+03 0.34351713233666E+03 0.29815000000028E+03 0.29815000000031E+03
 0.32006108313917E+03 0.34343001937464E+03 0.29815000000028E+03 0.29815000000031E+03 0.38900039598845E+03
 0.30049150527418E+03 0.23395672072690E+04 0.22891422088953E+04 0.96474664623069E+03 0.16950363135718E+04
 0.72546593410993E+03 0.13407290039372E+04 0.14337676844342E+04 0.13407290039372E+04 0.23277230449734E+04
 0.12122058140605E+04 0.14321904381227E+04 0.12122058140605E+04 0.23269903845478E+04 0.13407290039372E+04
 0.14337676844342E+04 0.13407290039372E+04 0.23277230449734E+04 0.12122058140605E+04 0.14321904381227E+04
 0.12122058140605E+04 0.23269903845478E+04 0.23460542445436E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.21796383534161E+06 0.50050000000000E+08 0.51088553709916E+03 0.13593460451469E+01
 0.13593460451469E+01 0.10751046767245E+01 0.32512050625643E+00 0.29857559235702E+03 0.32312888739744E+03
 0.31780703425490E+03 0.31747025317623E+03 0.23000000000000E+00 0.00000000000000E+00 0.21388771269236E+00
 0.00000000000000E+00 -.23935730544018E+01 0.99823668824676E-03 0.26010291122515E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30757056744647E+02 0.11533896279243E+02 0.30049055181000E+03 0.38904618019333E+03
 0.29860447916808E+03 0.30123221059579E+03 0.29815000000024E+03 0.29815000000025E+03 0.29859936877663E+03
 0.30123237561723E+03 0.29815000000024E+03 0.29815000000025E+03 0.29860447916808E+03 0.30123221059579E+03
 0.29815000000024E+03 0.29815000000025E+03 0.29859936877663E+03 0.30123237561723E+03 0.29815000000024E+03
 0.29815000000025E+03 0.30077458703723E+03 0.29815000000054E+03 0.42727191505948E+02 0.38434370519143E+02
 0.48938658710425E+02 0.22726701045679E+03 0.17808365845281E+03 0.38562542792570E+02 0.45474123129249E+02
 0.38562542792570E+02 0.17954980849926E+03 0.38093412425545E+02 0.45463137087378E+02 0.38093412425545E+02
 0.17953760439452E+03 0.38562542792570E+02 0.45474123129249E+02 0.38562542792570E+02 0.17954980849926E+03
 0.38093412425545E+02 0.45463137087378E+02 0.38093412425545E+02 0.17953760439452E+03 0.10845235476316E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34299624078161E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.75431021443956E-01 0.00000000000000E+00 0.00000000000000E+00 0.75431021443956E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.86506051311413E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.86506051311413E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934671679620E-01 0.90353383485114E-01 0.32312888739744E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    270.02230799
 0.31887096242530E-01 0.31481375994973E+03 0.45921130717780E+03 0.45767650101592E+03 0.45698338386989E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15981641892182E+00 0.00000000000000E+00 -.13243467949884E+02
 0.11098650737043E-01 0.95993772034525E+00 0.72080833873790E+03 0.27030312702671E+03 0.83338739904113E+01
 0.31252027464042E+01 0.32776784031005E+03 0.29815000000287E+03 0.32361362722326E+03 0.34479269045398E+03
 0.29815000000031E+03 0.29815000000036E+03 0.32085023173728E+03 0.34470916003119E+03 0.29815000000030E+03
 0.29815000000036E+03 0.32361362722326E+03 0.34479269045398E+03 0.29815000000031E+03 0.29815000000036E+03
 0.32085023173728E+03 0.34470916003119E+03 0.29815000000030E+03 0.29815000000036E+03 0.39076458108573E+03
 0.30084423582247E+03 0.23554381583914E+04 0.23028478113545E+04 0.96373930419038E+03 0.16777813589247E+04
 0.70922335821333E+03 0.13506262151138E+04 0.14431942794793E+04 0.13506262151138E+04 0.23290608181805E+04
 0.12223294376153E+04 0.14418079841238E+04 0.12223294376153E+04 0.23284821889192E+04 0.13506262151138E+04
 0.14431942794793E+04 0.13506262151138E+04 0.23290608181805E+04 0.12223294376153E+04 0.14418079841238E+04
 0.12223294376153E+04 0.23284821889192E+04 0.23460829282108E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.21843644963645E+06 0.50050000000000E+08 0.51137581223237E+03 0.13610198316731E+01
 0.13610198316731E+01 0.11252123420366E+01 0.30793623073923E+00 0.29870078681974E+03 0.32308804731497E+03
 0.31808156659834E+03 0.31776208296852E+03 0.23000000000000E+00 0.00000000000000E+00 0.21269074368912E+00
 0.00000000000000E+00 -.24349079982297E+01 0.99781789009275E-03 0.28568875308796E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28002502421007E+02 0.10500938407878E+02 0.30084324600988E+03 0.39080942734697E+03
 0.29865518828265E+03 0.30136307577701E+03 0.29815000000024E+03 0.29815000000025E+03 0.29864947535683E+03
 0.30136317578060E+03 0.29815000000024E+03 0.29815000000025E+03 0.29865518828265E+03 0.30136307577701E+03
 0.29815000000024E+03 0.29815000000025E+03 0.29864947535683E+03 0.30136317578060E+03 0.29815000000024E+03
 0.29815000000025E+03 0.30088781328693E+03 0.29815000000059E+03 0.44881937436705E+02 0.39898154963049E+02
 0.52935685008729E+02 0.22995769558818E+03 0.17675733215441E+03 0.41483747559345E+02 0.49228224007791E+02
 0.41521787154596E+02 0.18179607917268E+03 0.40988479238870E+02 0.49208907109794E+02 0.41032548547047E+02
 0.18177602663559E+03 0.41483747559345E+02 0.49228224007791E+02 0.41521787154596E+02 0.18179607917268E+03
 0.40988479238870E+02 0.49208907109794E+02 0.41032548547047E+02 0.18177602663559E+03 0.11065115587771E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34304683316384E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76172439150970E-01 0.00000000000000E+00 0.00000000000000E+00 0.76172439150970E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.86883598033173E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.86883598033173E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934377246402E-01 0.90364446954288E-01 0.32308804731497E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    280.00922268
 0.32075118577700E-01 0.31536233060801E+03 0.45968018703633E+03 0.45813718291706E+03 0.45744201574535E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15700949668266E+00 0.00000000000000E+00 -.13294172490865E+02
 0.11033589918579E-01 0.99744037390058E+00 0.72505866712786E+03 0.27189700017295E+03 0.80205295567847E+01
 0.30076985837943E+01 0.32872778749233E+03 0.29815000000456E+03 0.32445345705213E+03 0.34602537402025E+03
 0.29815000000040E+03 0.29815000000048E+03 0.32162602337416E+03 0.34594536899986E+03 0.29815000000037E+03
 0.29815000000048E+03 0.32445345705213E+03 0.34602537402025E+03 0.29815000000040E+03 0.29815000000048E+03
 0.32162602337416E+03 0.34594536899986E+03 0.29815000000037E+03 0.29815000000048E+03 0.39244627147930E+03
 0.30122135045953E+03 0.23726471866398E+04 0.23178972566539E+04 0.96199715216311E+03 0.16606623997478E+04
 0.69385526182391E+03 0.13613527845231E+04 0.14513585819650E+04 0.13613527845231E+04 0.23294055370414E+04
 0.12327264787101E+04 0.14501688801723E+04 0.12327264787101E+04 0.23289873854293E+04 0.13613527845231E+04
 0.14513585819650E+04 0.13613527845231E+04 0.23294055370414E+04 0.12327264787101E+04 0.14501688801723E+04
 0.12327264787101E+04 0.23289873854293E+04 0.23458582186691E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.21976616127475E+06 0.50050000000000E+08 0.51184909471129E+03 0.13657165387395E+01
 0.13657165387395E+01 0.11751469154999E+01 0.29159089405973E+00 0.29885162063336E+03 0.32302773327743E+03
 0.31832804374358E+03 0.31802650282189E+03 0.23000000000000E+00 0.00000000000000E+00 0.21147031665360E+00
 0.00000000000000E+00 -.24671443714166E+01 0.99731396263825E-03 0.31159584434621E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25674283355048E+02 0.96278562581431E+01 0.30122033026984E+03 0.39248974932424E+03
 0.29870771146050E+03 0.30148990359933E+03 0.29815000000026E+03 0.29815000000028E+03 0.29870143230815E+03
 0.30148992792753E+03 0.29815000000026E+03 0.29815000000028E+03 0.29870771146050E+03 0.30148990359933E+03
 0.29815000000026E+03 0.29815000000028E+03 0.29870143230815E+03 0.30148992792753E+03 0.29815000000026E+03
 0.29815000000028E+03 0.30099736930793E+03 0.29815000000069E+03 0.46735426329950E+02 0.41040629969637E+02
 0.56758112007048E+02 0.23231860365457E+03 0.17527670108749E+03 0.44361657398662E+02 0.52804449795668E+02
 0.44524006541106E+02 0.18377595270234E+03 0.43843339063191E+02 0.52776380889737E+02 0.44014636494623E+02
 0.18374770545436E+03 0.44361657398662E+02 0.52804449795668E+02 0.44524006541106E+02 0.18377595270234E+03
 0.43843339063192E+02 0.52776380889737E+02 0.44014636494623E+02 0.18374770545436E+03 0.11260346903245E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34307670037445E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76743638563503E-01 0.00000000000000E+00 0.00000000000000E+00 0.76743638563503E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87211312531619E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87211312531619E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934222636311E-01 0.90381122069781E-01 0.32302773327743E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    290.00824220
 0.32251356923057E-01 0.31592006491253E+03 0.46013893458701E+03 0.45858851650670E+03 0.45789177221490E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15424397541929E+00 0.00000000000000E+00 -.13329944327775E+02
 0.10973295397385E-01 0.10343381824194E+01 0.72904261758111E+03 0.27339098159292E+03 0.77344142718270E+01
 0.29004053519351E+01 0.32967089752905E+03 0.29815000000725E+03 0.32528076965311E+03 0.34722305565373E+03
 0.29815000000053E+03 0.29815000000067E+03 0.32238933341139E+03 0.34714641651380E+03 0.29815000000048E+03
 0.29815000000067E+03 0.32528076965311E+03 0.34722305565373E+03 0.29815000000053E+03 0.29815000000067E+03
 0.32238933341139E+03 0.34714641651380E+03 0.29815000000048E+03 0.29815000000067E+03 0.39405684775551E+03
 0.30162404315695E+03 0.23884277580977E+04 0.23316448814913E+04 0.95973728200058E+03 0.16438637770316E+04
 0.67932780862099E+03 0.13712633835671E+04 0.14585831356277E+04 0.13712633835671E+04 0.23290842677884E+04
 0.12423702025716E+04 0.14575636084070E+04 0.12423702025716E+04 0.23288019477761E+04 0.13712633835671E+04
 0.14585831356277E+04 0.13712633835671E+04 0.23290842677884E+04 0.12423702025716E+04 0.14575636084070E+04
 0.12423702025716E+04 0.23288019477761E+04 0.23449948472919E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22101253542878E+06 0.50050000000000E+08 0.51231244332412E+03 0.13701033439260E+01
 0.13701033439260E+01 0.12251420131161E+01 0.27598480222072E+00 0.29903140706515E+03 0.32295342544702E+03
 0.31855201643912E+03 0.31826886253689E+03 0.23000000000000E+00 0.00000000000000E+00 0.21022233060498E+00
 0.00000000000000E+00 -.24842738671059E+01 0.99671417974002E-03 0.33792815811364E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23673670891047E+02 0.88776265841427E+01 0.30162299807424E+03 0.39409866831656E+03
 0.29876229245744E+03 0.30161345831710E+03 0.29815000000031E+03 0.29815000000033E+03 0.29875546575201E+03
 0.30161339681548E+03 0.29815000000031E+03 0.29815000000033E+03 0.29876229245744E+03 0.30161345831710E+03
 0.29815000000031E+03 0.29815000000033E+03 0.29875546575201E+03 0.30161339681548E+03 0.29815000000031E+03
 0.29815000000033E+03 0.30110394586549E+03 0.29815000000085E+03 0.48292565611049E+02 0.41875898705019E+02
 0.60416700553156E+02 0.23440657129689E+03 0.17368778724097E+03 0.47207291599798E+02 0.56215007049477E+02
 0.47568538226002E+02 0.18553380507110E+03 0.46668885007586E+02 0.56177870668132E+02 0.47041721989623E+02
 0.18549711796349E+03 0.47207291599798E+02 0.56215007049477E+02 0.47568538226002E+02 0.18553380507110E+03
 0.46668885007587E+02 0.56177870668132E+02 0.47041721989623E+02 0.18549711796349E+03 0.11433744703691E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34309148461570E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77032489841853E-01 0.00000000000000E+00 0.00000000000000E+00 0.77032489841853E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87493895905705E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87493895905705E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934285505683E-01 0.90401970957522E-01 0.32295342544702E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    300.00033862
 0.32358601558829E-01 0.31648595325017E+03 0.46060125975048E+03 0.45904680315662E+03 0.45835002172161E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15152350996477E+00 0.00000000000000E+00 -.13354218000030E+02
 0.10936925882655E-01 0.10705494892094E+01 0.73146696666264E+03 0.27430011249849E+03 0.74727979235298E+01
 0.28022992213237E+01 0.33059136587846E+03 0.29815000001143E+03 0.32609006890719E+03 0.34838697975173E+03
 0.29815000000071E+03 0.29815000000096E+03 0.32313648843448E+03 0.34831346249238E+03 0.29815000000063E+03
 0.29815000000095E+03 0.32609006890719E+03 0.34838697975173E+03 0.29815000000071E+03 0.29815000000096E+03
 0.32313648843448E+03 0.34831346249238E+03 0.29815000000063E+03 0.29815000000095E+03 0.39560055222006E+03
 0.30205099873265E+03 0.24017970457849E+04 0.23431380653579E+04 0.95727676712551E+03 0.16278060590826E+04
 0.66574290812151E+03 0.13797581724213E+04 0.14652262052984E+04 0.13797581724213E+04 0.23285618460091E+04
 0.12509171227736E+04 0.14643430586628E+04 0.12509171227736E+04 0.23283841404656E+04 0.13797581724213E+04
 0.14652262052984E+04 0.13797581724213E+04 0.23285618460091E+04 0.12509171227736E+04 0.14643430586628E+04
 0.12509171227736E+04 0.23283841404656E+04 0.23436475868636E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22177097957958E+06 0.50050000000000E+08 0.51277835724203E+03 0.13727655641008E+01
 0.13727655641008E+01 0.12751024951777E+01 0.26112558537792E+00 0.29924235426950E+03 0.32287025689342E+03
 0.31875702362414E+03 0.31849242197998E+03 0.23000000000000E+00 0.00000000000000E+00 0.20895059062093E+00
 0.00000000000000E+00 -.24888876656565E+01 0.99601151301078E-03 0.36461873747300E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21940726511874E+02 0.82277724419529E+01 0.30204993489084E+03 0.39564057742268E+03
 0.29881888692487E+03 0.30173367982579E+03 0.29815000000034E+03 0.29815000000038E+03 0.29881153179145E+03
 0.30173352335728E+03 0.29815000000034E+03 0.29815000000038E+03 0.29881888692487E+03 0.30173367982579E+03
 0.29815000000034E+03 0.29815000000038E+03 0.29881153179145E+03 0.30173352335728E+03 0.29815000000034E+03
 0.29815000000038E+03 0.30120751900403E+03 0.29815000000110E+03 0.49550233778366E+02 0.42415253945464E+02
 0.63902151286001E+02 0.23626150057540E+03 0.17203983853297E+03 0.50015812408363E+02 0.59453049861158E+02
 0.50663384490320E+02 0.18710001603164E+03 0.49460216812990E+02 0.59406664815723E+02 0.50122030470698E+02
 0.18705477002489E+03 0.50015812408363E+02 0.59453049861158E+02 0.50663384490320E+02 0.18710001603164E+03
 0.49460216812990E+02 0.59406664815723E+02 0.50122030470698E+02 0.18705477002489E+03 0.11587121359842E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34309683552380E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77092554889150E-01 0.00000000000000E+00 0.00000000000000E+00 0.77092554889150E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87732372613228E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87732372613228E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934523509749E-01 0.90425513096946E-01 0.32287025689342E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    310.00776803
 0.32398192361369E-01 0.31705296709681E+03 0.46107934195708E+03 0.45952394389114E+03 0.45882842203424E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14883932712636E+00 0.00000000000000E+00 -.13373660632998E+02
 0.10923559603680E-01 0.11061601887969E+01 0.73236200380183E+03 0.27463575142569E+03 0.72322255682526E+01
 0.27120845880947E+01 0.33148998849200E+03 0.29815000001778E+03 0.32688178571753E+03 0.34952375900403E+03
 0.29815000000097E+03 0.29815000000138E+03 0.32386868851046E+03 0.34945310682386E+03 0.29815000000084E+03
 0.29815000000137E+03 0.32688178571753E+03 0.34952375900403E+03 0.29815000000097E+03 0.29815000000138E+03
 0.32386868851046E+03 0.34945310682386E+03 0.29815000000084E+03 0.29815000000137E+03 0.39708939802736E+03
 0.30250250209609E+03 0.24131200554901E+04 0.23527014291239E+04 0.95484031652619E+03 0.16126976318186E+04
 0.65308311370974E+03 0.13870406236638E+04 0.14715857183974E+04 0.13870406236638E+04 0.23281754216234E+04
 0.12585580354916E+04 0.14708118409233E+04 0.12585580354916E+04 0.23280778923305E+04 0.13870406236638E+04
 0.14715857183974E+04 0.13870406236638E+04 0.23281754216234E+04 0.12585580354916E+04 0.14708118409233E+04
 0.12585580354916E+04 0.23280778923305E+04 0.23421466518188E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22205096945877E+06 0.50050000000000E+08 0.51325866563156E+03 0.13737470585271E+01
 0.13737470585271E+01 0.13251396422404E+01 0.24695677108267E+00 0.29948726912937E+03 0.32278247689450E+03
 0.31894720403358E+03 0.31870114859793E+03 0.23000000000000E+00 0.00000000000000E+00 0.20765356593555E+00
 0.00000000000000E+00 -.24871215875702E+01 0.99519701153304E-03 0.39171017186030E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20423263358229E+02 0.76587237593358E+01 0.30250142588599E+03 0.39712758041259E+03
 0.29887771218872E+03 0.30185103696147E+03 0.29815000000036E+03 0.29815000000042E+03 0.29886984872501E+03
 0.30185077711794E+03 0.29815000000036E+03 0.29815000000042E+03 0.29887771218872E+03 0.30185103696147E+03
 0.29815000000036E+03 0.29815000000042E+03 0.29886984872501E+03 0.30185077711794E+03 0.29815000000036E+03
 0.29815000000042E+03 0.30130851749635E+03 0.29815000000148E+03 0.50515209595237E+02 0.42677278211970E+02
 0.67223285779269E+02 0.23792916198526E+03 0.17036975977709E+03 0.52795100132743E+02 0.62528796286451E+02
 0.53833028837668E+02 0.18851062198316E+03 0.52225025867441E+02 0.62473079410784E+02 0.53279922808547E+02
 0.18845679018435E+03 0.52795100132743E+02 0.62528796286451E+02 0.53833028837668E+02 0.18851062198316E+03
 0.52225025867441E+02 0.62473079410784E+02 0.53279922808547E+02 0.18845679018435E+03 0.11723049978620E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34309776761106E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77042439260678E-01 0.00000000000000E+00 0.00000000000000E+00 0.77042439260678E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.87929884903655E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.87929884903655E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82934847213656E-01 0.90450458735631E-01 0.32278247689450E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    320.00291400
 0.32397808911797E-01 0.31761089869435E+03 0.46157566805262E+03 0.46002095369006E+03 0.45932724813777E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14619643471340E+00 0.00000000000000E+00 -.13393393102051E+02
 0.10923687549579E-01 0.11410892825479E+01 0.73235342586378E+03 0.27463253469892E+03 0.70108449201598E+01
 0.26290668450599E+01 0.33236557971979E+03 0.29815000002724E+03 0.32765472781858E+03 0.35063384551927E+03
 0.29815000000134E+03 0.29815000000201E+03 0.32458499017364E+03 0.35056581434822E+03 0.29815000000112E+03
 0.29815000000200E+03 0.32765472781858E+03 0.35063384551927E+03 0.29815000000134E+03 0.29815000000201E+03
 0.32458499017364E+03 0.35056581434822E+03 0.29815000000112E+03 0.29815000000200E+03 0.39852681498131E+03
 0.30297654304241E+03 0.24231918058721E+04 0.23610809241376E+04 0.95253185728577E+03 0.15986166618277E+04
 0.64132214525554E+03 0.13935727798217E+04 0.14777814746369E+04 0.13935727798217E+04 0.23280582499636E+04
 0.12656211617130E+04 0.14770989677706E+04 0.12656211617130E+04 0.23280255255038E+04 0.13935727798217E+04
 0.14777814746369E+04 0.13935727798217E+04 0.23280582499636E+04 0.12656211617130E+04 0.14770989677706E+04
 0.12656211617130E+04 0.23280255255038E+04 0.23407679170512E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22204825766732E+06 0.50050000000000E+08 0.51375607331736E+03 0.13737377089732E+01
 0.13737377089732E+01 0.13751153720761E+01 0.23349239352536E+00 0.29976745857728E+03 0.32269415793107E+03
 0.31912535132590E+03 0.31889760217938E+03 0.23000000000000E+00 0.00000000000000E+00 0.20633601584988E+00
 0.00000000000000E+00 -.24841284096016E+01 0.99426684087736E-03 0.41911122333380E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19088011856052E+02 0.71580044460195E+01 0.30297546117037E+03 0.39856316115862E+03
 0.29893872965779E+03 0.30196542663936E+03 0.29815000000036E+03 0.29815000000045E+03 0.29893038107437E+03
 0.30196505632905E+03 0.29815000000036E+03 0.29815000000045E+03 0.29893872965779E+03 0.30196542663936E+03
 0.29815000000036E+03 0.29815000000045E+03 0.29893038107437E+03 0.30196505632905E+03 0.29815000000036E+03
 0.29815000000045E+03 0.30140687325057E+03 0.29815000000205E+03 0.51192293631764E+02 0.42681725914089E+02
 0.70374937819764E+02 0.23944323850026E+03 0.16871642599140E+03 0.55539875988558E+02 0.65439572057974E+02
 0.57087980656383E+02 0.18979216329344E+03 0.54957900185768E+02 0.65374580560733E+02 0.56525781151829E+02
 0.18972984926616E+03 0.55539875988558E+02 0.65439572057974E+02 0.57087980656383E+02 0.18979216329344E+03
 0.54957900185768E+02 0.65374580560733E+02 0.56525781151829E+02 0.18972984926616E+03 0.11843374176073E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34309814636335E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76980556092441E-01 0.00000000000000E+00 0.00000000000000E+00 0.76980556092441E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88089949253407E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88089949253407E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82935180076172E-01 0.90475580145507E-01 0.32269415793107E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    330.00645068
 0.32385327381533E-01 0.31815639938287E+03 0.46208917653671E+03 0.46053540650036E+03 0.45984345405022E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14358786057019E+00 0.00000000000000E+00 -.13415390334272E+02
 0.10927896228558E-01 0.11754305890843E+01 0.73207137336218E+03 0.27452676501082E+03 0.68060165136863E+01
 0.25522561926324E+01 0.33322252923689E+03 0.29815000004107E+03 0.32841268731185E+03 0.35172231194262E+03
 0.29815000000192E+03 0.29815000000301E+03 0.32528867768314E+03 0.35165669885605E+03 0.29815000000156E+03
 0.29815000000299E+03 0.32841268731185E+03 0.35172231194262E+03 0.29815000000192E+03 0.29815000000301E+03
 0.32528867768314E+03 0.35165669885605E+03 0.29815000000156E+03 0.29815000000299E+03 0.39992139504806E+03
 0.30347320775221E+03 0.24327448306836E+04 0.23689666847044E+04 0.95034320459586E+03 0.15854264131861E+04
 0.63033149256723E+03 0.13997885293425E+04 0.14838500723296E+04 0.13997885293425E+04 0.23281859342993E+04
 0.12724120324293E+04 0.14832483337091E+04 0.12724120324293E+04 0.23282095138641E+04 0.13997885293425E+04
 0.14838500723296E+04 0.13997885293425E+04 0.23281859342993E+04 0.12724120324293E+04 0.14832483337091E+04
 0.12724120324293E+04 0.23282095138641E+04 0.23396282009341E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22195998711138E+06 0.50050000000000E+08 0.51426993871845E+03 0.13734285735441E+01
 0.13734285735441E+01 0.14251330555035E+01 0.22050151566009E+00 0.30008514751254E+03 0.32260513698810E+03
 0.31929467578010E+03 0.31908507354352E+03 0.23000000000000E+00 0.00000000000000E+00 0.20499998908408E+00
 0.00000000000000E+00 -.24820441205349E+01 0.99321426815449E-03 0.44680104685399E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17905061002720E+02 0.67143978760201E+01 0.30347212717669E+03 0.39995592560163E+03
 0.29900220871218E+03 0.30207728286417E+03 0.29815000000036E+03 0.29815000000051E+03 0.29899339948165E+03
 0.30207679582178E+03 0.29815000000036E+03 0.29815000000051E+03 0.29900220871218E+03 0.30207728286417E+03
 0.29815000000036E+03 0.29815000000051E+03 0.29899339948165E+03 0.30207679582178E+03 0.29815000000036E+03
 0.29815000000051E+03 0.30150286579289E+03 0.29815000000289E+03 0.51585160349693E+02 0.42443451993996E+02
 0.73358127406425E+02 0.24077057956610E+03 0.16704566152264E+03 0.58254503277685E+02 0.68186516070415E+02
 0.60451273534992E+02 0.19095586976816E+03 0.57663396088313E+02 0.68112376086120E+02 0.59882836433125E+02
 0.19088524008353E+03 0.58254503277685E+02 0.68186516070415E+02 0.60451273534992E+02 0.19095586976816E+03
 0.57663396088314E+02 0.68112376086120E+02 0.59882836433126E+02 0.19088524008353E+03 0.11948557930551E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34301046680839E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76947844040173E-01 0.00000000000000E+00 0.00000000000000E+00 0.76947844040173E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88213899149243E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88213899149243E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82935495153469E-01 0.90500892017526E-01 0.32260513698810E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    340.00578810
 0.32376303992837E-01 0.31868849035776E+03 0.46261470365019E+03 0.46106143737216E+03 0.46037088689609E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14101622157042E+00 0.00000000000000E+00 -.13439626575209E+02
 0.10930940460066E-01 0.12091597827109E+01 0.73186749385625E+03 0.27445031019609E+03 0.66161644758513E+01
 0.24810616784442E+01 0.33406232140695E+03 0.29815000006095E+03 0.32915690980749E+03 0.35278975912251E+03
 0.29815000000281E+03 0.29815000000453E+03 0.32598059096606E+03 0.35272639085989E+03 0.29815000000225E+03
 0.29815000000450E+03 0.32915690980749E+03 0.35278975912251E+03 0.29815000000281E+03 0.29815000000453E+03
 0.32598059096606E+03 0.35272639085989E+03 0.29815000000225E+03 0.29815000000450E+03 0.40127519847857E+03
 0.30399094279643E+03 0.24421272664921E+04 0.23766934648399E+04 0.94823307771021E+03 0.15729921056571E+04
 0.62001786255839E+03 0.14058954039719E+04 0.14897573047984E+04 0.14058954039719E+04 0.23284715168643E+04
 0.12790672155312E+04 0.14892291402214E+04 0.12790672155312E+04 0.23285460013030E+04 0.14058954039719E+04
 0.14897573047984E+04 0.14058954039719E+04 0.23284715168643E+04 0.12790672155312E+04 0.14892291402214E+04
 0.12790672155312E+04 0.23285460013030E+04 0.23387227112045E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22189617285785E+06 0.50050000000000E+08 0.51479543135299E+03 0.13732051053706E+01
 0.13732051053706E+01 0.14751297426012E+01 0.20791325373019E+00 0.30044147281655E+03 0.32251770886932E+03
 0.31945774749076E+03 0.31926602165519E+03 0.23000000000000E+00 0.00000000000000E+00 0.20364985822095E+00
 0.00000000000000E+00 -.24812064782700E+01 0.99203631855957E-03 0.47469845516289E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16852803949520E+02 0.63198014810699E+01 0.30398987097560E+03 0.40130794073091E+03
 0.29906824300719E+03 0.30218651777864E+03 0.29815000000036E+03 0.29815000000060E+03 0.29905900103134E+03
 0.30218590888569E+03 0.29815000000036E+03 0.29815000000060E+03 0.29906824300719E+03 0.30218651777864E+03
 0.29815000000036E+03 0.29815000000060E+03 0.29905900103134E+03 0.30218590888569E+03 0.29815000000036E+03
 0.29815000000060E+03 0.30159652333294E+03 0.29815000000412E+03 0.51703182403891E+02 0.41985005564852E+02
 0.76173155255156E+02 0.24197185181869E+03 0.16541783078726E+03 0.60937774143439E+02 0.70772244891695E+02
 0.63939488789324E+02 0.19199595808589E+03 0.60340345721603E+02 0.70689173895713E+02 0.63367695800572E+02
 0.19191726183408E+03 0.60937774143439E+02 0.70772244891695E+02 0.63939488789324E+02 0.19199595808589E+03
 0.60340345721603E+02 0.70689173895713E+02 0.63367695800572E+02 0.19191726183408E+03 0.12040586050131E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34294475175997E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76949425313314E-01 0.00000000000000E+00 0.00000000000000E+00 0.76949425313314E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88307086609564E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88307086609564E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82935785547994E-01 0.90525742723388E-01 0.32251770886932E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    350.00019108
 0.32375704602109E-01 0.31920877665699E+03 0.46314773915163E+03 0.46159436404148E+03 0.46090482437394E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13848139177599E+00 0.00000000000000E+00 -.13465250321226E+02
 0.10931141408368E-01 0.12422907016206E+01 0.73185403986043E+03 0.27444526494766E+03 0.64397165571343E+01
 0.24148937089254E+01 0.33488695820166E+03 0.29815000008906E+03 0.32988909617870E+03 0.35383758794647E+03
 0.29815000000413E+03 0.29815000000679E+03 0.32666205295060E+03 0.35377631612023E+03 0.29815000000328E+03
 0.29815000000675E+03 0.32988909617870E+03 0.35383758794647E+03 0.29815000000413E+03 0.29815000000679E+03
 0.32666205295060E+03 0.35377631612023E+03 0.29815000000328E+03 0.29815000000675E+03 0.40259094561518E+03
 0.30452878623551E+03 0.24514359972305E+04 0.23843565168961E+04 0.94615775234023E+03 0.15611784842063E+04
 0.61028994310434E+03 0.14119534214437E+04 0.14954717779632E+04 0.14119534214437E+04 0.23288296574949E+04
 0.12856258997079E+04 0.14950112733330E+04 0.12856258997079E+04 0.23289506701948E+04 0.14119534214437E+04
 0.14954717779632E+04 0.14119534214437E+04 0.23288296574949E+04 0.12856258997079E+04 0.14950112733330E+04
 0.12856258997079E+04 0.23289506701948E+04 0.23379908261426E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22189193391024E+06 0.50050000000000E+08 0.51532823186804E+03 0.13731904441794E+01
 0.13731904441794E+01 0.15251017575074E+01 0.19573508206545E+00 0.30083748677753E+03 0.32243513575734E+03
 0.31961685736035E+03 0.31944256297673E+03 0.23000000000000E+00 0.00000000000000E+00 0.20228638684916E+00
 0.00000000000000E+00 -.24810671597960E+01 0.99073043136380E-03 0.50278826937707E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15911270185185E+02 0.59667263194443E+01 0.30452773032741E+03 0.40262193049179E+03
 0.29913703593439E+03 0.30229332694679E+03 0.29815000000036E+03 0.29815000000074E+03 0.29912739073199E+03
 0.30229259216322E+03 0.29815000000036E+03 0.29815000000074E+03 0.29913703593439E+03 0.30229332694679E+03
 0.29815000000036E+03 0.29815000000074E+03 0.29912739073199E+03 0.30229259216322E+03 0.29815000000036E+03
 0.29815000000074E+03 0.30168804710854E+03 0.29815000000587E+03 0.51561864234180E+02 0.41332581665096E+02
 0.78831823596093E+02 0.24308129670384E+03 0.16385531398976E+03 0.63594282755963E+02 0.73209980202968E+02
 0.67575349462478E+02 0.19295029481419E+03 0.62993175261980E+02 0.73118281099276E+02 0.67002899997104E+02
 0.19286385885619E+03 0.63594282755963E+02 0.73209980202968E+02 0.67575349462478E+02 0.19295029481419E+03
 0.62993175261980E+02 0.73118281099276E+02 0.67002899997104E+02 0.19286385885619E+03 0.12121838150287E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34288759531646E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.76973132595539E-01 0.00000000000000E+00 0.00000000000000E+00 0.76973132595539E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88375625291918E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88375625291918E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82936054697490E-01 0.90549219637614E-01 0.32243513575734E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    360.01076753
 0.32381864308211E-01 0.31972058190977E+03 0.46368617917633E+03 0.46213222103108E+03 0.46144339667995E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13597791053112E+00 0.00000000000000E+00 -.13491395063038E+02
 0.10929060647789E-01 0.12749045309335E+01 0.73199337599235E+03 0.27449751599713E+03 0.62749796599609E+01
 0.23531173724853E+01 0.33569958390960E+03 0.29815000012831E+03 0.33061196160457E+03 0.35486914105839E+03
 0.29815000000606E+03 0.29815000001012E+03 0.32733544253935E+03 0.35480983881014E+03 0.29815000000478E+03
 0.29815000001006E+03 0.33061196160457E+03 0.35486914105839E+03 0.29815000000606E+03 0.29815000001012E+03
 0.32733544253935E+03 0.35480983881014E+03 0.29815000000478E+03 0.29815000001006E+03 0.40387360508985E+03
 0.30508701300823E+03 0.24606414896596E+04 0.23919281687505E+04 0.94408715976424E+03 0.15498664713127E+04
 0.60105887574962E+03 0.14179482276777E+04 0.15009903750470E+04 0.14179482276777E+04 0.23292061966624E+04
 0.12920842640195E+04 0.15005919828461E+04 0.12920842640195E+04 0.23293695042157E+04 0.14179482276777E+04
 0.15009903750470E+04 0.14179482276777E+04 0.23292061966624E+04 0.12920842640195E+04 0.15005919828461E+04
 0.12920842640195E+04 0.23293695042157E+04 0.23373654992055E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22193549593114E+06 0.50050000000000E+08 0.51586630432801E+03 0.13733433256960E+01
 0.13733433256960E+01 0.15751546397544E+01 0.18394159741141E+00 0.30127481228145E+03 0.32235969220419E+03
 0.31977410120805E+03 0.31961668815137E+03 0.23000000000000E+00 0.00000000000000E+00 0.20090729249463E+00
 0.00000000000000E+00 -.24809120931026E+01 0.98929230507120E-03 0.53111790884424E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15062568719267E+02 0.56484632697253E+01 0.30508597981373E+03 0.40390286583167E+03
 0.29920897775202E+03 0.30239815781762E+03 0.29815000000037E+03 0.29815000000093E+03 0.29919895924616E+03
 0.30239729388844E+03 0.29815000000037E+03 0.29815000000093E+03 0.29920897775202E+03 0.30239815781762E+03
 0.29815000000037E+03 0.29815000000093E+03 0.29919895924616E+03 0.30239729388844E+03 0.29815000000037E+03
 0.29815000000093E+03 0.30177784054045E+03 0.29815000000834E+03 0.51175579512228E+02 0.40508540236053E+02
 0.81351191780756E+02 0.24413325611918E+03 0.16237530837952E+03 0.66234087891008E+02 0.75517378455517E+02
 0.71389397905682E+02 0.19384834626961E+03 0.65631799697085E+02 0.75417408508999E+02 0.70818838460603E+02
 0.19375454633519E+03 0.66234087891008E+02 0.75517378455517E+02 0.71389397905682E+02 0.19384834626961E+03
 0.65631799697085E+02 0.75417408508999E+02 0.70818838460603E+02 0.19375454633519E+03 0.12194520442565E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34284092811792E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77003932027379E-01 0.00000000000000E+00 0.00000000000000E+00 0.77003932027379E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88424722617761E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88424722617761E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82936309941026E-01 0.90570690230695E-01 0.32235969220419E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    370.00057724
 0.32391055836624E-01 0.32022379956309E+03 0.46422611809731E+03 0.46267132238390E+03 0.46198303297895E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13351474126301E+00 0.00000000000000E+00 -.13517402395474E+02
 0.10925957908348E-01 0.13068915358951E+01 0.73220124652753E+03 0.27457546744783E+03 0.61213955253914E+01
 0.22955233220218E+01 0.33649817327720E+03 0.29815000018216E+03 0.33132362205329E+03 0.35588164456801E+03
 0.29815000000883E+03 0.29815000001490E+03 0.32799894242089E+03 0.35582419025778E+03 0.29815000000694E+03
 0.29815000001482E+03 0.33132362205329E+03 0.35588164456801E+03 0.29815000000883E+03 0.29815000001490E+03
 0.32799894242089E+03 0.35582419025778E+03 0.29815000000694E+03 0.29815000001482E+03 0.40512054176032E+03
 0.30566265369132E+03 0.24696295471331E+04 0.23993055622580E+04 0.94202164454153E+03 0.15390392082981E+04
 0.59230745553387E+03 0.14238111444498E+04 0.15062964843766E+04 0.14238111444498E+04 0.23295762244110E+04
 0.12983901777130E+04 0.15059544855534E+04 0.12983901777130E+04 0.23297773632696E+04 0.14238111444498E+04
 0.15062964843766E+04 0.14238111444498E+04 0.23295762244110E+04 0.12983901777130E+04 0.15059544855534E+04
 0.12983901777130E+04 0.23297773632696E+04 0.23368034928392E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22200049928456E+06 0.50050000000000E+08 0.51640574789227E+03 0.13735713226298E+01
 0.13735713226298E+01 0.16251036883061E+01 0.17257479256092E+00 0.30175211585819E+03 0.32229350942760E+03
 0.31993022493817E+03 0.31978896263055E+03 0.23000000000000E+00 0.00000000000000E+00 0.19951816470642E+00
 0.00000000000000E+00 -.24803083224621E+01 0.10262371634624E-02 0.55957287044586E+00 0.77954690054382E+04
 0.29233008770393E+04 0.14296618764998E+02 0.53612320368742E+01 0.30566164961941E+03 0.40514812601180E+03
 0.29928406029620E+03 0.30250085816458E+03 0.29815000000037E+03 0.29815000000122E+03 0.29927370068088E+03
 0.30249986334389E+03 0.29815000000037E+03 0.29815000000122E+03 0.29928406029620E+03 0.30250085816458E+03
 0.29815000000037E+03 0.29815000000122E+03 0.29927370068088E+03 0.30249986334389E+03 0.29815000000037E+03
 0.29815000000122E+03 0.30186578597603E+03 0.29815000001176E+03 0.50560058319604E+02 0.39537197050057E+02
 0.83733598555595E+02 0.24515045695255E+03 0.16099819040417E+03 0.68850998409258E+02 0.77698365464739E+02
 0.75388511163947E+02 0.19470976104319E+03 0.68249909219364E+02 0.77590574407884E+02 0.74822241962370E+02
 0.19460905490826E+03 0.68850998409258E+02 0.77698365464739E+02 0.75388511163947E+02 0.19470976104319E+03
 0.68249909219364E+02 0.77590574407884E+02 0.74822241962370E+02 0.19460905490826E+03 0.12260108499221E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34280650476697E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77032273756801E-01 0.00000000000000E+00 0.00000000000000E+00 0.77032273756801E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88458346595416E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88458346595416E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82936553961811E-01 0.90589555944869E-01 0.32229350942760E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    380.01159073
 0.32400499516830E-01 0.32072152116554E+03 0.46476879144688E+03 0.46321305694316E+03 0.46252520511746E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13108116582441E+00 0.00000000000000E+00 -.13543230644849E+02
 0.10922771921071E-01 0.13383956160147E+01 0.73241481721020E+03 0.27465555645383E+03 0.59773058909302E+01
 0.22414897090988E+01 0.33728680702694E+03 0.29815000025540E+03 0.33202763650019E+03 0.35688030009117E+03
 0.29815000001275E+03 0.29815000002172E+03 0.32865585186847E+03 0.35682458981251E+03 0.29815000001001E+03
 0.29815000002160E+03 0.33202763650019E+03 0.35688030009117E+03 0.29815000001275E+03 0.29815000002172E+03
 0.32865585186847E+03 0.35682458981251E+03 0.29815000001001E+03 0.29815000002160E+03 0.40633901040252E+03
 0.30625723035901E+03 0.24783899663158E+04 0.24064737940557E+04 0.93995590016767E+03 0.15286149865593E+04
 0.58395930689083E+03 0.14295385724463E+04 0.15114270297762E+04 0.14295385724463E+04 0.23299371939429E+04
 0.13045555098754E+04 0.15111361802203E+04 0.13045555098754E+04 0.23301719920280E+04 0.14295385724463E+04
 0.15114270297762E+04 0.14295385724463E+04 0.23299371939429E+04 0.13045555098754E+04 0.15111361802203E+04
 0.13045555098754E+04 0.23301719920280E+04 0.23362807325349E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22206728587918E+06 0.50050000000000E+08 0.51694778443635E+03 0.13738055257344E+01
 0.13738055257344E+01 0.16751587557453E+01 0.16158497213829E+00 0.30227134332007E+03 0.32223769595408E+03
 0.32008685426470E+03 0.31996098188896E+03 0.23000000000000E+00 0.00000000000000E+00 0.19811355182659E+00
 0.00000000000000E+00 -.24791857214682E+01 0.10960341638532E-02 0.58826513396181E+00 0.72990425516258E+04
 0.27371409568597E+04 0.13599310137798E+02 0.50997413016743E+01 0.30625626172124E+03 0.40636496677756E+03
 0.29936287544415E+03 0.30260209121230E+03 0.29815000000039E+03 0.29815000000162E+03 0.29935220618069E+03
 0.30260096419284E+03 0.29815000000039E+03 0.29815000000162E+03 0.29936287544415E+03 0.30260209121230E+03
 0.29815000000039E+03 0.29815000000162E+03 0.29935220618069E+03 0.30260096419284E+03 0.29815000000039E+03
 0.29815000000162E+03 0.30195245951585E+03 0.29815000001647E+03 0.49727578305453E+02 0.38434482715082E+02
 0.86000506434944E+02 0.24615934578165E+03 0.15972883681453E+03 0.71460003322716E+02 0.79774070678600E+02
 0.79610488322427E+02 0.19555734977441E+03 0.70862378552399E+02 0.79658922605864E+02 0.79050792038134E+02
 0.19545020678156E+03 0.71460003322716E+02 0.79774070678600E+02 0.79610488322427E+02 0.19555734977441E+03
 0.70862378552399E+02 0.79658922605864E+02 0.79050792038134E+02 0.19545020678156E+03 0.12320396247207E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34278539975252E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77056064787376E-01 0.00000000000000E+00 0.00000000000000E+00 0.77056064787376E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88480129696906E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88480129696906E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82936786909613E-01 0.90605502082814E-01 0.32223769595408E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    390.01293516
 0.32409188287258E-01 0.32121253174258E+03 0.46531187423624E+03 0.46375515999526E+03 0.46306767770843E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12868430672172E+00 0.00000000000000E+00 -.13568892366037E+02
 0.10919842144143E-01 0.13693281430813E+01 0.73261132298430E+03 0.27472924611911E+03 0.58422811511040E+01
 0.21908554316640E+01 0.33806368216804E+03 0.29815000035355E+03 0.33272232852407E+03 0.35786288066059E+03
 0.29815000001822E+03 0.29815000003126E+03 0.32930459438971E+03 0.35780881391431E+03 0.29815000001429E+03
 0.29815000003110E+03 0.33272232852407E+03 0.35786288066059E+03 0.29815000001822E+03 0.29815000003126E+03
 0.32930459438971E+03 0.35780881391431E+03 0.29815000001429E+03 0.29815000003110E+03 0.40752713371722E+03
 0.30686805321575E+03 0.24868942506661E+04 0.24134080050983E+04 0.93790209940564E+03 0.15185985489993E+04
 0.57600693909664E+03 0.14351118031861E+04 0.15163827961962E+04 0.14351118031861E+04 0.23302901106087E+04
 0.13105650124849E+04 0.15161381327840E+04 0.13105650124849E+04 0.23305546382367E+04 0.14351118031861E+04
 0.15163827961962E+04 0.14351118031861E+04 0.23302901106087E+04 0.13105650124849E+04 0.15161381327840E+04
 0.13105650124849E+04 0.23305546382367E+04 0.23357931228349E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22212873368085E+06 0.50050000000000E+08 0.51749009172780E+03 0.13740209844150E+01
 0.13740209844150E+01 0.17251654779192E+01 0.15100410290876E+00 0.30283039581840E+03 0.32219344094324E+03
 0.32024417477080E+03 0.32013280221390E+03 0.23000000000000E+00 0.00000000000000E+00 0.19669805698418E+00
 0.00000000000000E+00 -.24777053086975E+01 0.11728332605314E-02 0.61710026446120E+00 0.68210889554540E+04
 0.25579083582953E+04 0.12963857675519E+02 0.48614466283196E+01 0.30686712587892E+03 0.40755152213716E+03
 0.29944547726172E+03 0.30270176658636E+03 0.29815000000041E+03 0.29815000000217E+03 0.29943453130356E+03
 0.30270050738066E+03 0.29815000000041E+03 0.29815000000217E+03 0.29944547726172E+03 0.30270176658636E+03
 0.29815000000041E+03 0.29815000000217E+03 0.29943453130356E+03 0.30270050738066E+03 0.29815000000041E+03
 0.29815000000217E+03 0.30203778896485E+03 0.29815000002283E+03 0.48694901902762E+02 0.37220765119914E+02
 0.88155714804946E+02 0.24717498165054E+03 0.15857848827157E+03 0.74056783869324E+02 0.81749196932667E+02
 0.84062186053173E+02 0.19640428899943E+03 0.73464770780957E+02 0.81627215085942E+02 0.83511201488356E+02
 0.19629122879372E+03 0.74056783869324E+02 0.81749196932666E+02 0.84062186053173E+02 0.19640428899943E+03
 0.73464770780957E+02 0.81627215085942E+02 0.83511201488354E+02 0.19629122879372E+03 0.12376458473520E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34277855783136E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77077499366735E-01 0.00000000000000E+00 0.00000000000000E+00 0.77077499366735E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88492845177972E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88492845177972E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937004512212E-01 0.90618186318581E-01 0.32219344094324E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    400.00266198
 0.32417486684673E-01 0.32169681696466E+03 0.46585465945948E+03 0.46429691447963E+03 0.46360973288288E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12632407556843E+00 0.00000000000000E+00 -.13594529017533E+02
 0.10917045404496E-01 0.13996942848677E+01 0.73279900408818E+03 0.27479962653307E+03 0.57155338036951E+01
 0.21433251763857E+01 0.33882932032114E+03 0.29815000048351E+03 0.33340810078077E+03 0.35883004023942E+03
 0.29815000002575E+03 0.29815000004446E+03 0.32994552016474E+03 0.35877752493607E+03 0.29815000002021E+03
 0.29815000004424E+03 0.33340810078077E+03 0.35883004023942E+03 0.29815000002575E+03 0.29815000004446E+03
 0.32994552016474E+03 0.35877752493607E+03 0.29815000002021E+03 0.29815000004424E+03 0.40868656340833E+03
 0.30749405742711E+03 0.24951696782553E+04 0.24201328090761E+04 0.93586411979819E+03 0.15089628371713E+04
 0.56841939677407E+03 0.14405467297284E+04 0.15211772990638E+04 0.14405467297284E+04 0.23306353997066E+04
 0.13164330617758E+04 0.15209743980993E+04 0.13164330617758E+04 0.23309261786215E+04 0.14405467297284E+04
 0.15211772990638E+04 0.14405467297284E+04 0.23306353997066E+04 0.13164330617758E+04 0.15209743980993E+04
 0.13164330617758E+04 0.23309261786215E+04 0.23353404639814E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22218742072785E+06 0.50050000000000E+08 0.51803198098536E+03 0.13742267382197E+01
 0.13742267382197E+01 0.17751141119957E+01 0.14082992167686E+00 0.30342805742523E+03 0.32216138628148E+03
 0.32040257745776E+03 0.32030475235808E+03 0.23000000000000E+00 0.00000000000000E+00 0.19527221221889E+00
 0.00000000000000E+00 -.24760763979593E+01 0.12575638552434E-02 0.64606724734089E+00 0.63615059916394E+04
 0.23855647468648E+04 0.12382611923026E+02 0.46434794711348E+01 0.30749317693093E+03 0.40870944763933E+03
 0.29953213255648E+03 0.30280008225999E+03 0.29815000000044E+03 0.29815000000295E+03 0.29952094316716E+03
 0.30279869176087E+03 0.29815000000044E+03 0.29815000000295E+03 0.29953213255648E+03 0.30280008225999E+03
 0.29815000000044E+03 0.29815000000295E+03 0.29952094316716E+03 0.30279869176087E+03 0.29815000000044E+03
 0.29815000000295E+03 0.30212194684857E+03 0.29815000003134E+03 0.47476114775521E+02 0.35910374016705E+02
 0.90209363772464E+02 0.24821159433289E+03 0.15755118374156E+03 0.76643510610225E+02 0.83634054327465E+02
 0.88759322502549E+02 0.19726309633161E+03 0.76059151788163E+02 0.83505789928259E+02 0.88219067659953E+02
 0.19714466084400E+03 0.76643510610225E+02 0.83634054327465E+02 0.88759322502549E+02 0.19726309633161E+03
 0.76059151788162E+02 0.83505789928259E+02 0.88219067659952E+02 0.19714466084400E+03 0.12429312106794E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34278645849411E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77099919137643E-01 0.00000000000000E+00 0.00000000000000E+00 0.77099919137643E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88498827198836E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88498827198836E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937202970856E-01 0.90627421034554E-01 0.32216138628148E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    410.00514632
 0.32426267592720E-01 0.32217562099924E+03 0.46639782639033E+03 0.46483896378206E+03 0.46415200406075E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12399431033313E+00 0.00000000000000E+00 -.13620293432822E+02
 0.10914087692859E-01 0.14295773317023E+01 0.73299759220684E+03 0.27487409707757E+03 0.55960596342652E+01
 0.20985223628495E+01 0.33958621514293E+03 0.29815000065423E+03 0.33408713417961E+03 0.35978484457123E+03
 0.29815000003602E+03 0.29815000006253E+03 0.33058063228928E+03 0.35973380022459E+03 0.29815000002829E+03
 0.29815000006222E+03 0.33408713417961E+03 0.35978484457123E+03 0.29815000003602E+03 0.29815000006253E+03
 0.33058063228928E+03 0.35973380022459E+03 0.29815000002829E+03 0.29815000006222E+03 0.40982174204689E+03
 0.30813587596781E+03 0.25032715759666E+04 0.24266968479081E+04 0.93383731674516E+03 0.14996553409871E+04
 0.56114883765824E+03 0.14458773961508E+04 0.15258320041671E+04 0.14458773961508E+04 0.23309707423385E+04
 0.13221911124305E+04 0.15256670501147E+04 0.13221911124305E+04 0.23312847832999E+04 0.14458773961508E+04
 0.15258320041671E+04 0.14458773961508E+04 0.23309707423385E+04 0.13221911124305E+04 0.15256670501147E+04
 0.13221911124305E+04 0.23312847832999E+04 0.23349206164143E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22224952013542E+06 0.50050000000000E+08 0.51857415585376E+03 0.13744444096934E+01
 0.13744444096934E+01 0.18251265337164E+01 0.13103432064482E+00 0.30406444373657E+03 0.32214185119613E+03
 0.32056267732581E+03 0.32047743563290E+03 0.23000000000000E+00 0.00000000000000E+00 0.19383269285420E+00
 0.00000000000000E+00 -.24744457908633E+01 0.13515741795540E-02 0.67523315060781E+00 0.59190239951461E+04
 0.22196339981798E+04 0.11847759537278E+02 0.44429098264793E+01 0.30813504766399E+03 0.40984318372757E+03
 0.29962335178858E+03 0.30289747691522E+03 0.29815000000049E+03 0.29815000000401E+03 0.29961195189931E+03
 0.30289595648400E+03 0.29815000000049E+03 0.29815000000401E+03 0.29962335178858E+03 0.30289747691522E+03
 0.29815000000049E+03 0.29815000000401E+03 0.29961195189931E+03 0.30289595648400E+03 0.29815000000049E+03
 0.29815000000401E+03 0.30220531017170E+03 0.29815000004263E+03 0.46080660973000E+02 0.34511069607633E+02
 0.92175896375377E+02 0.24928298492761E+03 0.15664620907036E+03 0.79228481805905E+02 0.85442708729557E+02
 0.93729149979145E+02 0.19814585402372E+03 0.78653749709903E+02 0.85308717714385E+02 0.93201565587300E+02
 0.19802258709311E+03 0.79228481805905E+02 0.85442708729557E+02 0.93729149979145E+02 0.19814585402372E+03
 0.78653749709902E+02 0.85308717714385E+02 0.93201565587300E+02 0.19802258709311E+03 0.12479886453110E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34280936055666E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77125708645379E-01 0.00000000000000E+00 0.00000000000000E+00 0.77125708645379E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88499935893695E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88499935893695E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937380107143E-01 0.90633111826664E-01 0.32214185119613E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    420.10247758
 0.32436871282402E-01 0.32265245676057E+03 0.46694501005711E+03 0.46538487706434E+03 0.46469803896770E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12167610721986E+00 0.00000000000000E+00 -.13646640389397E+02
 0.10910518428568E-01 0.14592230331491E+01 0.73323738485727E+03 0.27496401932148E+03 0.54823696023597E+01
 0.20558886008849E+01 0.34034094124780E+03 0.29815000087865E+03 0.33476527640230E+03 0.36073552150151E+03
 0.29815000005001E+03 0.29815000008727E+03 0.33121534985563E+03 0.36068588526273E+03 0.29815000003934E+03
 0.29815000008684E+03 0.33476527640230E+03 0.36073552150151E+03 0.29815000005001E+03 0.29815000008727E+03
 0.33121534985563E+03 0.36068588526273E+03 0.29815000003934E+03 0.29815000008684E+03 0.41094334568953E+03
 0.30879820794239E+03 0.25113029652970E+04 0.24331861261711E+04 0.93179629082744E+03 0.14905632594482E+04
 0.55410798716662E+03 0.14511693508304E+04 0.15303868695379E+04 0.14511693508304E+04 0.23312893793732E+04
 0.13279026548543E+04 0.15302567753232E+04 0.13279026548543E+04 0.23316242428290E+04 0.14511693508304E+04
 0.15303868695379E+04 0.14511693508304E+04 0.23312893793732E+04 0.13279026548543E+04 0.15302567753232E+04
 0.13279026548543E+04 0.23316242428290E+04 0.23345234395563E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22232451042616E+06 0.50050000000000E+08 0.51912027122807E+03 0.13747071812745E+01
 0.13747071812745E+01 0.18756131899810E+01 0.12153995755099E+00 0.30474370048480E+03 0.32213503888194E+03
 0.32072587719491E+03 0.32065231436445E+03 0.23000000000000E+00 0.00000000000000E+00 0.19236757981138E+00
 0.00000000000000E+00 -.24731082509815E+01 0.14571552712841E-02 0.70483773432409E+00 0.54901493050565E+04
 0.20588059893962E+04 0.11350130122746E+02 0.42562987960298E+01 0.30879743711733E+03 0.41096339791523E+03
 0.29972015850617E+03 0.30299490874545E+03 0.29815000000054E+03 0.29815000000546E+03 0.29970857980476E+03
 0.30299325950877E+03 0.29815000000054E+03 0.29815000000546E+03 0.29972015850617E+03 0.30299490874545E+03
 0.29815000000054E+03 0.29815000000546E+03 0.29970857980476E+03 0.30299325950877E+03 0.29815000000054E+03
 0.29815000000546E+03 0.30228869481345E+03 0.29815000005760E+03 0.44505272524159E+02 0.33017337642232E+02
 0.94078643046592E+02 0.25040701631558E+03 0.15585798005375E+03 0.81834193730594E+02 0.87197336681597E+02
 0.99031886493865E+02 0.19906784166192E+03 0.81271072045734E+02 0.87058153070318E+02 0.98518936419250E+02
 0.19894026722549E+03 0.81834193730594E+02 0.87197336681597E+02 0.99031886493865E+02 0.19906784166192E+03
 0.81271072045734E+02 0.87058153070318E+02 0.98518936419249E+02 0.19894026722549E+03 0.12529146828404E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34284767787985E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77160543527876E-01 0.00000000000000E+00 0.00000000000000E+00 0.77160543527876E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88497603162428E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88497603162428E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937532497355E-01 0.90635196210957E-01 0.32213503888194E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    430.02928186
 0.32448235218981E-01 0.32311616870961E+03 0.46748140364513E+03 0.46591993794492E+03 0.46523315461311E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11942964474498E+00 0.00000000000000E+00 -.13672212447289E+02
 0.10906695984453E-01 0.14878665749192E+01 0.73349436084067E+03 0.27506038531525E+03 0.53768262120106E+01
 0.20163098295040E+01 0.34107438825172E+03 0.29815000116275E+03 0.33542532884967E+03 0.36165777050905E+03
 0.29815000006836E+03 0.29815000011983E+03 0.33183355829068E+03 0.36160945001460E+03 0.29815000005387E+03
 0.29815000011925E+03 0.33542532884967E+03 0.36165777050905E+03 0.29815000006836E+03 0.29815000011983E+03
 0.33183355829068E+03 0.36160945001460E+03 0.29815000005387E+03 0.29815000011925E+03 0.41202292231820E+03
 0.30946246476509E+03 0.25190461905171E+04 0.24394278700851E+04 0.92980326330484E+03 0.14819148432202E+04
 0.54746256359886E+03 0.14562796920804E+04 0.15347327270449E+04 0.14562796920804E+04 0.23315827517962E+04
 0.13334178069651E+04 0.15346337806588E+04 0.13334178069651E+04 0.23319356652231E+04 0.14562796920804E+04
 0.15347327270449E+04 0.14562796920804E+04 0.23315827517962E+04 0.13334178069651E+04 0.15346337806588E+04
 0.13334178069651E+04 0.23319356652231E+04 0.23341563453583E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22240487725445E+06 0.50050000000000E+08 0.51965556709915E+03 0.13749887132527E+01
 0.13749887132527E+01 0.19252472114117E+01 0.11258789656402E+00 0.30544566087561E+03 0.32214095953922E+03
 0.32088783383319E+03 0.32082473940720E+03 0.23000000000000E+00 0.00000000000000E+00 0.19091562504726E+00
 0.00000000000000E+00 -.24714691565762E+01 0.15730161166179E-02 0.73409856158290E+00 0.50857711599298E+04
 0.19071641849737E+04 0.10897719214638E+02 0.40866447054892E+01 0.30946175528863E+03 0.41204166829217E+03
 0.29982039220031E+03 0.30309000426358E+03 0.29815000000061E+03 0.29815000000737E+03 0.29980867108805E+03
 0.30308823123353E+03 0.29815000000061E+03 0.29815000000737E+03 0.29982039220031E+03 0.30309000426358E+03
 0.29815000000061E+03 0.29815000000737E+03 0.29980867108805E+03 0.30308823123353E+03 0.29815000000061E+03
 0.29815000000737E+03 0.30237007630030E+03 0.29815000007672E+03 0.42804822949365E+02 0.31476841725005E+02
 0.95876685906686E+02 0.25155994714312E+03 0.15520387780690E+03 0.84393545563374E+02 0.88860435088998E+02
 0.10453847524246E+03 0.20001013608986E+03 0.83843640457432E+02 0.88716717536992E+02 0.10404165328138E+03
 0.19987887650791E+03 0.84393545563374E+02 0.88860435088999E+02 0.10453847524246E+03 0.20001013608986E+03
 0.83843640457433E+02 0.88716717536992E+02 0.10404165328138E+03 0.19987887650791E+03 0.12576303567827E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34290005831426E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77190986692993E-01 0.00000000000000E+00 0.00000000000000E+00 0.77190986692993E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88492959380287E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88492959380287E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937665110939E-01 0.90633676804487E-01 0.32214095953922E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    440.00676775
 0.32460456558813E-01 0.32357741754545E+03 0.46801863303873E+03 0.46645575710513E+03 0.46576897320658E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11720392937705E+00 0.00000000000000E+00 -.13697826152896E+02
 0.10902588232347E-01 0.15161631801386E+01 0.73377071843039E+03 0.27516401941140E+03 0.52764769022216E+01
 0.19786788383331E+01 0.34180353966496E+03 0.29815000152648E+03 0.33608252531397E+03 0.36257289068009E+03
 0.29815000009267E+03 0.29815000016314E+03 0.33244950218092E+03 0.36252582960901E+03 0.29815000007316E+03
 0.29815000016235E+03 0.33608252531397E+03 0.36257289068009E+03 0.29815000009267E+03 0.29815000016314E+03
 0.33244950218092E+03 0.36252582960901E+03 0.29815000007316E+03 0.29815000016235E+03 0.41308605138391E+03
 0.31014225412562E+03 0.25266836676069E+04 0.24455696286654E+04 0.92781592328523E+03 0.14734954579347E+04
 0.54104045503301E+03 0.14613281648431E+04 0.15389768348261E+04 0.14613281648431E+04 0.23318573053825E+04
 0.13388663650784E+04 0.15389063567442E+04 0.13388663650784E+04 0.23322261674930E+04 0.14613281648431E+04
 0.15389768348261E+04 0.14613281648431E+04 0.23318573053825E+04 0.13388663650784E+04 0.15389063567442E+04
 0.13388663650784E+04 0.23322261674930E+04 0.23338080051010E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22249130771919E+06 0.50050000000000E+08 0.52019165376800E+03 0.13752914041662E+01
 0.13752914041662E+01 0.19751346408728E+01 0.10396811464997E+00 0.30618311109640E+03 0.32215935057010E+03
 0.32105200423858E+03 0.32099842296433E+03 0.23000000000000E+00 0.00000000000000E+00 0.18944473139621E+00
 0.00000000000000E+00 -.24697663418971E+01 0.17034315064627E-02 0.76366359321630E+00 0.46964025084945E+04
 0.17611509406854E+04 0.10475816931781E+02 0.39284313494178E+01 0.31014161042193E+03 0.41310354572132E+03
 0.29992655683705E+03 0.30318505391520E+03 0.29815000000071E+03 0.29815000000990E+03 0.29991472613635E+03
 0.30318315985095E+03 0.29815000000071E+03 0.29815000000990E+03 0.29992655683705E+03 0.30318505391520E+03
 0.29815000000071E+03 0.29815000000990E+03 0.29991472613635E+03 0.30318315985095E+03 0.29815000000071E+03
 0.29815000000990E+03 0.30245141537308E+03 0.29815000010143E+03 0.40954304041263E+02 0.29860707579294E+02
 0.97617951628187E+02 0.25277015158425E+03 0.15466411019793E+03 0.86962342553864E+02 0.90476604297096E+02
 0.11037695089084E+03 0.20099654103966E+03 0.86427438799813E+02 0.90328907716996E+02 0.10989798365100E+03
 0.20086213083526E+03 0.86962342553864E+02 0.90476604297096E+02 0.11037695089084E+03 0.20099654103966E+03
 0.86427438799812E+02 0.90328907716996E+02 0.10989798365100E+03 0.20086213083526E+03 0.12622706386337E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34296706418723E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77222680000864E-01 0.00000000000000E+00 0.00000000000000E+00 0.77222680000864E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88486427928562E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88486427928562E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937777702381E-01 0.90628627377981E-01 0.32215935057010E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    450.00325607
 0.32473676432841E-01 0.32403445513659E+03 0.46855464811595E+03 0.46699028078768E+03 0.46630343732386E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11500605646646E+00 0.00000000000000E+00 -.13723675529303E+02
 0.10898148461414E-01 0.15440251559648E+01 0.73406964754840E+03 0.27527611783065E+03 0.51812627333788E+01
 0.19429735250171E+01 0.34252634878016E+03 0.29815000198699E+03 0.33673496819227E+03 0.36347836307586E+03
 0.29815000012447E+03 0.29815000022000E+03 0.33306137861915E+03 0.36343250506369E+03 0.29815000009846E+03
 0.29815000021895E+03 0.33673496819227E+03 0.36347836307586E+03 0.29815000012447E+03 0.29815000022000E+03
 0.33306137861915E+03 0.36343250506369E+03 0.29815000009846E+03 0.29815000021895E+03 0.41413068937337E+03
 0.31083467226967E+03 0.25342037311656E+04 0.24516013821828E+04 0.92583444938532E+03 0.14653045461897E+04
 0.53484092455750E+03 0.14663062778863E+04 0.15431109366839E+04 0.14663062778863E+04 0.23321104181547E+04
 0.13442379160959E+04 0.15430664042859E+04 0.13442379160959E+04 0.23324932830601E+04 0.14663062778863E+04
 0.15431109366839E+04 0.14663062778863E+04 0.23321104181547E+04 0.13442379160959E+04 0.15430664042859E+04
 0.13442379160959E+04 0.23324932830601E+04 0.23334742772573E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22258479991168E+06 0.50050000000000E+08 0.52072648864088E+03 0.13756187326075E+01
 0.13756187326075E+01 0.20251170824603E+01 0.95708699394666E-01 0.30695154005547E+03 0.32219000475000E+03
 0.32121770232554E+03 0.32117265539429E+03 0.23000000000000E+00 0.00000000000000E+00 0.18795945175563E+00
 0.00000000000000E+00 -.24663494914397E+01 0.18504331371592E-02 0.79344029411942E+00 0.43233121150660E+04
 0.16212420431498E+04 0.10082674221730E+02 0.37810028331488E+01 0.31083409878972E+03 0.41414698671259E+03
 0.30003856431639E+03 0.30327987769308E+03 0.29815000000085E+03 0.29815000001321E+03 0.30002665721736E+03
 0.30327786634088E+03 0.29815000000085E+03 0.29815000001321E+03 0.30003856431639E+03 0.30327987769308E+03
 0.29815000000085E+03 0.29815000001321E+03 0.30002665721736E+03 0.30327786634088E+03 0.29815000000085E+03
 0.29815000001321E+03 0.30253255390653E+03 0.29815000013299E+03 0.38966968159373E+02 0.28174624480476E+02
 0.99302323201557E+02 0.25403678076651E+03 0.15423794594895E+03 0.89531745688710E+02 0.92045959640896E+02
 0.11654414127901E+03 0.20202677181261E+03 0.89013507970702E+02 0.91894855794965E+02 0.11608461773761E+03
 0.20188975598496E+03 0.89531745688710E+02 0.92045959640896E+02 0.11654414127901E+03 0.20202677181261E+03
 0.89013507970702E+02 0.91894855794965E+02 0.11608461773761E+03 0.20188975598496E+03 0.12668357128239E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34304824568371E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77261103795292E-01 0.00000000000000E+00 0.00000000000000E+00 0.77261103795292E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88478840943833E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88478840943833E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82937892994836E-01 0.90620133711229E-01 0.32219000475000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    460.00331953
 0.32488163322001E-01 0.32448674792901E+03 0.46908817331327E+03 0.46752222840511E+03 0.46683526679658E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11283920945395E+00 0.00000000000000E+00 -.13749400776070E+02
 0.10893287467165E-01 0.15714157638711E+01 0.73439721701219E+03 0.27539895637957E+03 0.50909505834996E+01
 0.19091064688123E+01 0.34324201460602E+03 0.29815000256513E+03 0.33738190131358E+03 0.36437313124762E+03
 0.29815000016570E+03 0.29815000029396E+03 0.33366844828178E+03 0.36432842210819E+03 0.29815000013134E+03
 0.29815000029257E+03 0.33738190131358E+03 0.36437313124762E+03 0.29815000016570E+03 0.29815000029396E+03
 0.33366844828178E+03 0.36432842210819E+03 0.29815000013134E+03 0.29815000029257E+03 0.41515616119125E+03
 0.31153785063759E+03 0.25416069000104E+04 0.24575253256990E+04 0.92385883437782E+03 0.14573340735899E+04
 0.52885594504020E+03 0.14712136011013E+04 0.15471316936627E+04 0.14712136011013E+04 0.23323373007470E+04
 0.13495303284793E+04 0.15471107916674E+04 0.13495303284793E+04 0.23327324064397E+04 0.14712136011013E+04
 0.15471316936627E+04 0.14712136011013E+04 0.23323373007470E+04 0.13495303284793E+04 0.15471107916674E+04
 0.13495303284793E+04 0.23327324064397E+04 0.23331509079815E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22268725255435E+06 0.50050000000000E+08 0.52125881355109E+03 0.13759773181780E+01
 0.13759773181780E+01 0.20751173997597E+01 0.87820072012782E-01 0.30774748168238E+03 0.32223250580751E+03
 0.32138445523299E+03 0.32134697950167E+03 0.23000000000000E+00 0.00000000000000E+00 0.18646206861161E+00
 0.00000000000000E+00 -.24614629249459E+01 0.20166521382050E-02 0.82338268116380E+00 0.39669707275944E+04
 0.14876140228479E+04 0.97160168449165E+01 0.36435063168437E+01 0.31153735027829E+03 0.41517132332230E+03
 0.30015653579933E+03 0.30337443914302E+03 0.29815000000101E+03 0.29815000001752E+03 0.30014458553791E+03
 0.30337231489909E+03 0.29815000000101E+03 0.29815000001752E+03 0.30015653579933E+03 0.30337443914302E+03
 0.29815000000101E+03 0.29815000001752E+03 0.30014458553791E+03 0.30337231489909E+03 0.29815000000101E+03
 0.29815000001752E+03 0.30261345880704E+03 0.29815000017295E+03 0.36853317351452E+02 0.26421425866507E+02
 0.10093262499091E+03 0.25535889576406E+03 0.15392160764820E+03 0.92097267602011E+02 0.93571056841354E+02
 0.12304619662466E+03 0.20310033376102E+03 0.91597262414829E+02 0.93417109254545E+02 0.12260759672295E+03
 0.20296124499107E+03 0.92097267602011E+02 0.93571056841354E+02 0.12304619662466E+03 0.20310033376102E+03
 0.91597262414829E+02 0.93417109254545E+02 0.12260759672295E+03 0.20296124499107E+03 0.12713251040247E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34314293622117E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77300430593292E-01 0.00000000000000E+00 0.00000000000000E+00 0.77300430593292E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88470129043343E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88470129043343E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82938008021200E-01 0.90608311347744E-01 0.32223250580751E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    470.00304502
 0.32503208764577E-01 0.32493469842555E+03 0.46961880087746E+03 0.46805123501516E+03 0.46736412003426E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11070393099955E+00 0.00000000000000E+00 -.13774920593955E+02
 0.10888243704204E-01 0.15983312004887E+01 0.73473741195847E+03 0.27552652948443E+03 0.50052204433937E+01
 0.18769576662726E+01 0.34395057235407E+03 0.29815000328526E+03 0.33802332373261E+03 0.36525722182441E+03
 0.29815000021867E+03 0.29815000038932E+03 0.33427070104051E+03 0.36521361071399E+03 0.29815000017370E+03
 0.29815000038750E+03 0.33802332373261E+03 0.36525722182441E+03 0.29815000021867E+03 0.29815000038932E+03
 0.33427070104051E+03 0.36521361071399E+03 0.29815000017370E+03 0.29815000038750E+03 0.41616293798108E+03
 0.31225061389345E+03 0.25488811469810E+04 0.24633318813075E+04 0.92189136706873E+03 0.14495748409505E+04
 0.52307401704640E+03 0.14760428289655E+04 0.15510441600802E+04 0.14760428289655E+04 0.23325388944440E+04
 0.13547395869155E+04 0.15510447347078E+04 0.13547395869155E+04 0.23329446056751E+04 0.14760428289655E+04
 0.15510441600802E+04 0.14760428289655E+04 0.23325388944440E+04 0.13547395869155E+04 0.15510447347078E+04
 0.13547395869155E+04 0.23329446056751E+04 0.23328340720980E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22279365533931E+06 0.50050000000000E+08 0.52178822333234E+03 0.13763496154577E+01
 0.13763496154577E+01 0.21251160272237E+01 0.80302076840910E-01 0.30856805742387E+03 0.32228637474325E+03
 0.32155196849558E+03 0.32152113956102E+03 0.23000000000000E+00 0.00000000000000E+00 0.18495318050470E+00
 0.00000000000000E+00 -.24569420232975E+01 0.22054538378890E-02 0.85347885663002E+00 0.36273713203888E+04
 0.13602642451458E+04 0.93734015059121E+01 0.35150255647171E+01 0.31225018969314E+03 0.41617702117542E+03
 0.30028076318124E+03 0.30346880522744E+03 0.29815000000124E+03 0.29815000002308E+03 0.30026880286792E+03
 0.30346657289524E+03 0.29815000000124E+03 0.29815000002308E+03 0.30028076318124E+03 0.30346880522744E+03
 0.29815000000124E+03 0.29815000002308E+03 0.30026880286792E+03 0.30346657289524E+03 0.29815000000124E+03
 0.29815000002308E+03 0.30269418727827E+03 0.29815000022313E+03 0.34621782358342E+02 0.24601405069942E+02
 0.10251296208768E+03 0.25673556754257E+03 0.15371004064445E+03 0.94655923638670E+02 0.95055612225885E+02
 0.12989529594556E+03 0.20421673597213E+03 0.94175626218322E+02 0.94899371573869E+02 0.12947900223845E+03
 0.20407609165004E+03 0.94655923638670E+02 0.95055612225885E+02 0.12989529594556E+03 0.20421673597213E+03
 0.94175626218322E+02 0.94899371573869E+02 0.12947900223845E+03 0.20407609165004E+03 0.12757361288496E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34325051951530E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77339368762406E-01 0.00000000000000E+00 0.00000000000000E+00 0.77339368762406E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88460396407138E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88460396407138E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82938098036837E-01 0.90593268882440E-01 0.32228637474325E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    480.03179706
 0.32518491040962E-01 0.32537964931063E+03 0.47014794771002E+03 0.46857873217184E+03 0.46789143745993E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10859374117849E+00 0.00000000000000E+00 -.13800320559610E+02
 0.10883125352732E-01 0.16248556585605E+01 0.73508296015276E+03 0.27565611005729E+03 0.49235142566985E+01
 0.18463178462619E+01 0.34465431064208E+03 0.29815000417908E+03 0.33866126789283E+03 0.36613355733274E+03
 0.29815000028644E+03 0.29815000051172E+03 0.33487003642800E+03 0.36609099974308E+03 0.29815000022803E+03
 0.29815000050934E+03 0.33866126789283E+03 0.36613355733274E+03 0.29815000028644E+03 0.29815000051172E+03
 0.33487003642800E+03 0.36609099974308E+03 0.29815000022803E+03 0.29815000050934E+03 0.41715492225112E+03
 0.31297424188925E+03 0.25560470341598E+04 0.24690364183341E+04 0.91992535279616E+03 0.14419892723052E+04
 0.51746429274504E+03 0.14808076305205E+04 0.15548660863471E+04 0.14808076305205E+04 0.23327174577521E+04
 0.13598822629817E+04 0.15548862188388E+04 0.13598822629817E+04 0.23331323100600E+04 0.14808076305205E+04
 0.15548660863471E+04 0.14808076305205E+04 0.23327174577521E+04 0.13598822629817E+04 0.15548862188388E+04
 0.13598822629817E+04 0.23331323100600E+04 0.23325199764735E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22290173303524E+06 0.50050000000000E+08 0.52231612822536E+03 0.13767276600803E+01
 0.13767276600803E+01 0.21752597874185E+01 0.73130909132528E-01 0.30941320584276E+03 0.32235129790127E+03
 0.32172051494481E+03 0.32169547578553E+03 0.23000000000000E+00 0.00000000000000E+00 0.18342832308816E+00
 0.00000000000000E+00 -.24527930826877E+01 0.24217189915531E-02 0.88381778273818E+00 0.33034386020442E+04
 0.12387894757666E+04 0.90516395531384E+01 0.33943648324269E+01 0.31297389681836E+03 0.41716797773909E+03
 0.30041190612929E+03 0.30356333468712E+03 0.29815000000154E+03 0.29815000003019E+03 0.30039996852282E+03
 0.30356099909334E+03 0.29815000000154E+03 0.29815000003020E+03 0.30041190612929E+03 0.30356333468712E+03
 0.29815000000154E+03 0.29815000003019E+03 0.30039996852282E+03 0.30356099909334E+03 0.29815000000154E+03
 0.29815000003020E+03 0.30277504410683E+03 0.29815000028590E+03 0.32272051933788E+02 0.22707468244199E+02
 0.10405195899436E+03 0.25816974863790E+03 0.15359752984857E+03 0.97212879333316E+02 0.96507549771923E+02
 0.13713144607790E+03 0.20537856001489E+03 0.96753731950770E+02 0.96349545610850E+02 0.13673881427597E+03
 0.20523685654436E+03 0.97212879333316E+02 0.96507549771923E+02 0.13713144607790E+03 0.20537856001489E+03
 0.96753731950769E+02 0.96349545610850E+02 0.13673881427597E+03 0.20523685654436E+03 0.12800743289277E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34337068890892E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77378314405553E-01 0.00000000000000E+00 0.00000000000000E+00 0.77378314405553E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88449067885045E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88449067885045E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82938163754649E-01 0.90575098425797E-01 0.32235129790127E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    490.08049974
 0.32533642818126E-01 0.32582161727912E+03 0.47067499368285E+03 0.46910412434654E+03 0.46841663729054E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10651044197469E+00 0.00000000000000E+00 -.13825441168844E+02
 0.10878055452284E-01 0.16509687313536E+01 0.73542555791257E+03 0.27578458421721E+03 0.48456399252584E+01
 0.18171149719719E+01 0.34535279369770E+03 0.29815000528125E+03 0.33929532144195E+03 0.36700158806741E+03
 0.29815000037252E+03 0.29815000066767E+03 0.33546606513727E+03 0.36696004128876E+03 0.29815000029721E+03
 0.29815000066459E+03 0.33929532144195E+03 0.36700158806741E+03 0.29815000037252E+03 0.29815000066767E+03
 0.33546606513727E+03 0.36696004128876E+03 0.29815000029721E+03 0.29815000066459E+03 0.41813179084764E+03
 0.31370726911470E+03 0.25630958568359E+04 0.24746331023510E+04 0.91796500931560E+03 0.14345773146111E+04
 0.51202248024891E+03 0.14855022679780E+04 0.15585994150586E+04 0.14855022679780E+04 0.23328744671008E+04
 0.13649539838965E+04 0.15586373218950E+04 0.13649539838965E+04 0.23332971121938E+04 0.14855022679780E+04
 0.15585994150586E+04 0.14855022679780E+04 0.23328744671008E+04 0.13649539838965E+04 0.15586373218950E+04
 0.13649539838965E+04 0.23332971121938E+04 0.23322081442616E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22300888782841E+06 0.50050000000000E+08 0.52284191334031E+03 0.13771023675743E+01
 0.13771023675743E+01 0.22255033007763E+01 0.66312615756772E-01 0.31027975278773E+03 0.32242674012822E+03
 0.32188974112547E+03 0.32186968251498E+03 0.23000000000000E+00 0.00000000000000E+00 0.18188880321117E+00
 0.00000000000000E+00 -.24489090483461E+01 0.26707210666305E-02 0.91437325434829E+00 0.29954457243613E+04
 0.11232921466355E+04 0.87491622944526E+01 0.32809358604197E+01 0.31370700535203E+03 0.41814387117536E+03
 0.30055029382695E+03 0.30365801996956E+03 0.29815000000193E+03 0.29815000003926E+03 0.30053841161938E+03
 0.30365558631794E+03 0.29815000000192E+03 0.29815000003926E+03 0.30055029382695E+03 0.30365801996956E+03
 0.29815000000193E+03 0.29815000003926E+03 0.30053841161938E+03 0.30365558631794E+03 0.29815000000192E+03
 0.29815000003926E+03 0.30285602263699E+03 0.29815000036391E+03 0.29811936302475E+02 0.20739271065532E+02
 0.10555180883844E+03 0.25965882777054E+03 0.15357925988791E+03 0.99763236542266E+02 0.97928625154282E+02
 0.14477344932842E+03 0.20658379790007E+03 0.99326576344285E+02 0.97769367481003E+02 0.14440572711242E+03
 0.20644150982916E+03 0.99763236542266E+02 0.97928625154281E+02 0.14477344932842E+03 0.20658379790007E+03
 0.99326576344283E+02 0.97769367481002E+02 0.14440572711241E+03 0.20644150982916E+03 0.12843265141302E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34350290269056E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77414691530556E-01 0.00000000000000E+00 0.00000000000000E+00 0.77414691530556E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88437209268057E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88437209268057E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82938207396018E-01 0.90553956555540E-01 0.32242674012822E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
    500.00000000
 0.32547473973622E-01 0.32625478280068E+03 0.47119234207135E+03 0.46961989159197E+03 0.46893222897227E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10448408920922E+00 0.00000000000000E+00 -.13849862928398E+02
 0.10873431473553E-01 0.16762965310344E+01 0.73573830114789E+03 0.27590186293046E+03 0.47724253149074E+01
 0.17896594930903E+01 0.34603605182003E+03 0.29815000660864E+03 0.33991640766676E+03 0.36784895425290E+03
 0.29815000047920E+03 0.29815000086148E+03 0.33605026974134E+03 0.36780836417697E+03 0.29815000038316E+03
 0.29815000085754E+03 0.33991640766676E+03 0.36784895425290E+03 0.29815000047920E+03 0.29815000086148E+03
 0.33605026974134E+03 0.36780836417697E+03 0.29815000038316E+03 0.29815000085754E+03 0.41907990562247E+03
 0.31443770863673E+03 0.25699126357469E+04 0.24800309665496E+04 0.91604784705295E+03 0.14274486150648E+04
 0.50682052877661E+03 0.14900507235156E+04 0.15622007794752E+04 0.14900507235156E+04 0.23330162515132E+04
 0.13698776365244E+04 0.15622545625829E+04 0.13698776365244E+04 0.23334453378000E+04 0.14900507235156E+04
 0.15622007794752E+04 0.14900507235156E+04 0.23330162515132E+04 0.13698776365244E+04 0.15622545625829E+04
 0.13698776365244E+04 0.23334453378000E+04 0.23319065277999E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.22310670306093E+06 0.50050000000000E+08 0.52335799408100E+03 0.13774443330214E+01
 0.13774443330214E+01 0.22751008021000E+01 0.59939504852232E-01 0.31115154105015E+03 0.32251086932002E+03
 0.32205695431205E+03 0.32204106841964E+03 0.23000000000000E+00 0.00000000000000E+00 0.18035766652765E+00
 0.00000000000000E+00 -.24452619722524E+01 0.29546872178001E-02 0.94468945561760E+00 0.27075623950329E+04
 0.10153358981373E+04 0.84683913347693E+01 0.31756467505385E+01 0.31443752809765E+03 0.41909106846723E+03
 0.30069433691923E+03 0.30375154036330E+03 0.29815000000242E+03 0.29815000005052E+03 0.30068254138002E+03
 0.30374901539119E+03 0.29815000000241E+03 0.29815000005053E+03 0.30069433691923E+03 0.30375154036330E+03
 0.29815000000242E+03 0.29815000005052E+03 0.30068254138002E+03 0.30374901539119E+03 0.29815000000241E+03
 0.29815000005053E+03 0.30293599740870E+03 0.29815000045858E+03 0.27286488870959E+02 0.18725052288350E+02
 0.10699405092781E+03 0.26117716426328E+03 0.15364814308082E+03 0.10226450915645E+03 0.99300939024061E+02
 0.15272381695199E+03 0.20781183163222E+03 0.10185121286564E+03 0.99140927838852E+02 0.15238175729483E+03
 0.20766941325804E+03 0.10226450915645E+03 0.99300939024061E+02 0.15272381695199E+03 0.20781183163222E+03
 0.10185121286563E+03 0.99140927838852E+02 0.15238175729482E+03 0.20766941325804E+03 0.12884164428054E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34364427893574E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.77446772592012E-01 0.00000000000000E+00 0.00000000000000E+00 0.77446772592012E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.88423740992543E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.88423740992543E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82938230686949E-01 0.90530363612118E-01 0.32251086932002E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29815000000000E+03
