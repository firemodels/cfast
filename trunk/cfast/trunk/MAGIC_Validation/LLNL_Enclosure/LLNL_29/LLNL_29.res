#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-29 0 MONOZONE(1=OUI,0=NON)                                                                     
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
1000.000000 0.005000                                                                                
4000.000000 0.005000                                                                                
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
Plenum-LLNL29 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.233000                                                                                   
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
#CONDINIT 1000.000000 10.000000 28.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-29           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-29           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-29           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-29           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-29           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-29           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-29           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-29           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-29           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-29           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-29           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-29           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-29           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-29           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-29           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-29           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-29           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-29           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL29     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL29     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL29     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL29     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL29     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL29     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL29     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL29     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL29     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL29     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL29     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL29     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL29     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL29     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL29     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL29     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL29     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL29     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.95429704708514E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.50965148986073E-03 0.50965148986073E-03 0.72187042769320E-03 0.72547977983166E-03
 0.00000000000000E+00 0.48295626713728E-03 0.56835952133382E-03 0.48295626713728E-03 0.56835952133382E-03
 0.48051151428522E-03 0.56835272080250E-03 0.48051151428522E-03 0.56835272080250E-03 0.48295626719634E-03
 0.56835952139289E-03 0.48295626719634E-03 0.56835952139289E-03 0.48051151434429E-03 0.56835272074344E-03
 0.48051151434429E-03 0.56835272074344E-03 0.58561852434177E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30115000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.81588034292573E-07 0.99965819615543E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.59350404359522E-03 0.59350404359522E-03
 0.72895888930134E-03 0.73260368374785E-03 0.00000000000000E+00 0.51878175547145E-03 0.57227893546292E-03
 0.51878175547145E-03 0.57227893546292E-03 0.51426643515814E-03 0.57227107583344E-03 0.51426643515814E-03
 0.57227107583344E-03 0.51878175547145E-03 0.57227893546292E-03 0.51878175547145E-03 0.57227893546292E-03
 0.51426643521720E-03 0.57227107583344E-03 0.51426643521720E-03 0.57227107583344E-03 0.47335854276972E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30115000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.62387060820616E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.62387060820616E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23299965449775E+00 0.27237562664938E+00 0.30115000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     10.55184375
 0.30000000000000E+01 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.22999999999963E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.25129277316011E+02
 0.99975199331541E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30115000169111E+03 0.30115000000000E+03 0.30115000233225E+03 0.30115000233225E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000232025E+03 0.30115000232025E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000233225E+03 0.30115000233225E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000232025E+03 0.30115000232025E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000821387E+03
 0.30115000678464E+03 0.52402001716505E-03 0.52402001716505E-03 0.68938436231702E-03 0.69283128412860E-03
 .00000000000000E+00 0.49083733061925E-03 0.58319108244844E-03 0.49083733061925E-03 0.58319108244844E-03
 0.48829993553058E-03 0.58325082518929E-03 0.48829993553058E-03 0.58325082518929E-03 0.49083733067831E-03
 0.58319108244844E-03 0.49083733067831E-03 0.58319108244844E-03 0.48829993464460E-03 0.58325082442144E-03
 0.48829993464460E-03 0.58325082442144E-03 0.58593773576506E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30115000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.22999999939188E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.27774780675627E+02 0.99940818411455E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30115000675570E+03 0.30115000824909E+03
 0.30115000250456E+03 0.30115000250456E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000248262E+03
 0.30115000248262E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000250456E+03 0.30115000250456E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000248262E+03 0.30115000248262E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000241261E+03 0.30115000000000E+03 0.57078707935997E-03 0.57078707935997E-03
 0.74227267306341E-03 0.74598403642873E-03 .00000000000000E+00 0.52707392641663E-03 0.57912536741225E-03
 0.52707392641663E-03 0.57912536741225E-03 0.52244934941549E-03 0.57923961375131E-03 0.52244934941549E-03
 0.57923961375131E-03 0.52707392641663E-03 0.57912536747131E-03 0.52707392641663E-03 0.57912536747131E-03
 0.52244934941549E-03 0.57923961375131E-03 0.52244934941549E-03 0.57923961375131E-03 0.47359229647414E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30115000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27103739206539E+00 0.00000000000000E+00 0.00000000000000E+00 0.27103739206539E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725851341E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725851341E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23191827911865E+00 0.27103716198139E+00 0.30115000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     20.34380775
 0.30000000000000E+01 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.22999999999946E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.25129277760473E+02
 0.99975199331102E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30115000236413E+03 0.30115000000000E+03 0.30115000325025E+03 0.30115000325025E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000323343E+03 0.30115000323343E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000325025E+03 0.30115000325025E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000323343E+03 0.30115000323343E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115001126991E+03
 0.30115000931580E+03 0.52919390481515E-03 0.52919390481515E-03 0.67739143114156E-03 0.68077838829727E-03
 .00000000000000E+00 0.49356037603104E-03 0.58848094604396E-03 0.49356037603104E-03 0.58848094604396E-03
 0.49099164378031E-03 0.58856741897910E-03 0.49099164378031E-03 0.58856741897910E-03 0.49356037597197E-03
 0.58848094610303E-03 0.49356037597197E-03 0.58848094610303E-03 0.49099164378031E-03 0.58856741903817E-03
 0.49099164378031E-03 0.58856741903817E-03 0.58591308021480E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30115000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.22999999942903E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.27774781132694E+02 0.99942920235715E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30115000928876E+03 0.30115001130269E+03
 0.30115000349018E+03 0.30115000349018E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000345955E+03
 0.30115000345955E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000349018E+03 0.30115000349018E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000345955E+03 0.30115000345955E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000336351E+03 0.30115000000000E+03 0.56242415054500E-03 0.56242415054500E-03
 0.74694568901747E-03 0.75068041746255E-03 .00000000000000E+00 0.52996568691721E-03 0.58151472785291E-03
 0.52996568691721E-03 0.58151472785291E-03 0.52530384963630E-03 0.58167741882249E-03 0.52530384963630E-03
 0.58167741882249E-03 0.52996568697628E-03 0.58151472791198E-03 0.52996568697628E-03 0.58151472791198E-03
 0.52530384963630E-03 0.58167741870436E-03 0.52530384963630E-03 0.58167741870436E-03 0.47357538192858E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30115000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27103739446230E+00 0.00000000000000E+00 0.00000000000000E+00 0.27103739446230E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725916477E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725916477E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23191827910082E+00 0.27103716195932E+00 0.30115000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     32.14862075
 0.30000000000000E+01 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000000000E+03
 0.22999999999847E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.25129277314972E+02
 0.99975199331542E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30115000299921E+03 0.30115000000000E+03 0.30115000411030E+03 0.30115000411030E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000408894E+03 0.30115000408894E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000411030E+03 0.30115000411030E+03 0.30115000000000E+03 0.30115000000000E+03
 0.30115000408894E+03 0.30115000408894E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115001400569E+03
 0.30115001158340E+03 0.53378887597349E-03 0.53378887597349E-03 0.66685436779224E-03 0.67018863963120E-03
 .00000000000000E+00 0.49593640391949E-03 0.59311321097204E-03 0.49593640391949E-03 0.59311321097204E-03
 0.49334259187439E-03 0.59322491833424E-03 0.49334259187439E-03 0.59322491833424E-03 0.49593640391949E-03
 0.59311321103111E-03 0.49593640391949E-03 0.59311321103111E-03 0.49334259187439E-03 0.59322491833424E-03
 0.49334259187439E-03 0.59322491833424E-03 0.58589393120600E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30115000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30115000000000E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000000000E+03 0.22999999947070E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.27774780674756E+02 0.99945284005713E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30115001156668E+03 0.30115001402585E+03
 0.30115000441357E+03 0.30115000441357E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000437477E+03
 0.30115000437477E+03 0.30115000000000E+03 0.30115000000000E+03 0.30115000441357E+03 0.30115000441357E+03
 0.30115000000000E+03 0.30115000000000E+03 0.30115000437477E+03 0.30115000437477E+03 0.30115000000000E+03
 0.30115000000000E+03 0.30115000425627E+03 0.30115000000000E+03 0.55510556121959E-03 0.55510556121959E-03
 0.75104374846727E-03 0.75479896720961E-03 .00000000000000E+00 0.53250049748976E-03 0.58360914767453E-03
 0.53250049748976E-03 0.58360914767453E-03 0.52780958666166E-03 0.58381731329898E-03 0.52780958666166E-03
 0.58381731329898E-03 0.53250049748976E-03 0.58360914773359E-03 0.53250049748976E-03 0.58360914773359E-03
 0.52780958672073E-03 0.58381731329898E-03 0.52780958672073E-03 0.58381731329898E-03 0.47356432677411E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30115000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27103739205982E+00 0.00000000000000E+00 0.00000000000000E+00 0.27103739205982E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725852156E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27103725852156E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23191827911869E+00 0.27103716198143E+00 0.30115000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     40.00326969
 0.23681090890968E+01 0.30116940352755E+03 0.33467876646001E+03 0.30822749081661E+03 0.30765763847130E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22621921036508E+00 0.00000000000000E+00 0.12530668109860E+02
 0.10000592328246E-02 0.67667814309131E-01 0.79995261654697E+04 0.29998223120511E+04 0.11822459586847E+03
 0.44334223450678E+02 0.30230370265574E+03 0.30115000000004E+03 0.30212652094629E+03 0.30252748727882E+03
 0.30115000000001E+03 0.30115000000001E+03 0.30189769491751E+03 0.30251818063619E+03 0.30115000000001E+03
 0.30115000000001E+03 0.30212652094629E+03 0.30252748727882E+03 0.30115000000001E+03 0.30115000000001E+03
 0.30189769491751E+03 0.30251818063619E+03 0.30115000000001E+03 0.30115000000001E+03 0.30479906151214E+03
 0.30115396066483E+03 0.59935772659539E+03 0.59723408709226E+03 0.27903261477822E+03 0.56243597523964E+03
 0.28200819738753E+03 0.37409919584511E+03 0.18144341313853E+03 0.37271602552754E+03 0.48895246196849E+03
 0.28308114764700E+03 0.17728747093294E+03 0.28211370764693E+03 0.48489911027597E+03 0.37409919584511E+03
 0.18144341313853E+03 0.37271602552754E+03 0.48895246196849E+03 0.28308114764700E+03 0.17728747093294E+03
 0.28211370764693E+03 0.48489911027597E+03 0.49927078393198E+02 0.00000000000000E+00 0.39273244696868E-02
 0.19656258970782E+06 0.19656258970782E+06 0.50050000000000E+08 0.35807062131993E+03 0.12809776898334E+01
 0.12809776898334E+01 0.15447944862357E-01 0.13337419618625E+01 0.30115747838216E+03 0.30780663049308E+03
 0.30189446170565E+03 0.30188027218923E+03 0.22999999947414E+00 0.00000000000000E+00 0.22915505809560E+00
 0.00000000000000E+00 0.95902886998134E+01 0.99979683169645E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30115395447523E+03 0.30480548045052E+03
 0.30115192395813E+03 0.30133719760055E+03 0.30115000000001E+03 0.30115000000001E+03 0.30115193897526E+03
 0.30133720671467E+03 0.30115000000001E+03 0.30115000000001E+03 0.30115192395813E+03 0.30133719760055E+03
 0.30115000000001E+03 0.30115000000001E+03 0.30115193897526E+03 0.30133720671467E+03 0.30115000000001E+03
 0.30115000000001E+03 0.30122245220496E+03 0.30115000000004E+03 0.63421598894840E+00 0.63478944265810E+00
 0.50612958932282E+00 0.39578805534642E+02 0.39070145297373E+02 0.74417023064678E+00 -.34661960542058E+00
 0.74609394893838E+00 0.59175901759975E+02 0.74930870344296E+00 -.34272289198750E+00 0.75122582983309E+00
 0.59179705930518E+02 0.74417023064684E+00 -.34661960542058E+00 0.74609394893844E+00 0.59175901759975E+02
 0.74930870344308E+00 -.34272289198756E+00 0.75122582983321E+00 0.59179705930518E+02 0.11818574188623E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31427089043675E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.18513880909775E+00 0.00000000000000E+00 0.00000000000000E+00 0.18513880909775E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25077826026367E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25077826026367E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23337185123271E+00 0.27282977800472E+00 0.30115747838216E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     50.02648774
 0.17263544329472E+01 0.30123654495646E+03 0.37005763663117E+03 0.33045443773354E+03 0.32705952255746E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22184688170672E+00 0.00000000000000E+00 0.19132352475908E+02
 0.99990146839533E-03 0.13835995473177E+00 0.80000000000000E+04 0.30000000000000E+04 0.57820198160005E+02
 0.21682574310002E+02 0.30390375427791E+03 0.30115000000008E+03 0.30387408149568E+03 0.30526275845128E+03
 0.30115000000004E+03 0.30115000000004E+03 0.30319788445135E+03 0.30523106101745E+03 0.30115000000004E+03
 0.30115000000004E+03 0.30387408149568E+03 0.30526275845128E+03 0.30115000000004E+03 0.30115000000004E+03
 0.30319788445135E+03 0.30523106101745E+03 0.30115000000004E+03 0.30115000000004E+03 0.31251240283563E+03
 0.30116531114218E+03 0.78820658441608E+03 0.78157350482614E+03 0.39201243813145E+03 0.99794163644176E+03
 0.60396913611965E+03 0.55593707939215E+03 0.32614876651159E+03 0.55060650889222E+03 0.89493756668786E+03
 0.41680454153819E+03 0.31928545736856E+03 0.41318609698646E+03 0.88840882773141E+03 0.55593707939215E+03
 0.32614876651159E+03 0.55060650889222E+03 0.89493756668786E+03 0.41680454153819E+03 0.31928545736856E+03
 0.41318609698646E+03 0.88840882773141E+03 0.82551455337553E+02 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.39764526244003E+03 0.14687551574633E+01
 0.14687551574633E+01 0.64417983243679E-01 0.11166204862224E+01 0.30116483108159E+03 0.31444562209324E+03
 0.30455921988201E+03 0.30445133317605E+03 0.22999999950868E+00 0.00000000000000E+00 0.22820282413817E+00
 0.00000000000000E+00 0.18198804887078E+02 0.99985738356618E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30116525705308E+03 0.31255622861465E+03
 0.30115672589661E+03 0.30161517692995E+03 0.30115000000004E+03 0.30115000000004E+03 0.30115673875663E+03
 0.30161523670458E+03 0.30115000000004E+03 0.30115000000004E+03 0.30115672589661E+03 0.30161517692995E+03
 0.30115000000004E+03 0.30115000000004E+03 0.30115673875663E+03 0.30161523670458E+03 0.30115000000004E+03
 0.30115000000004E+03 0.30141143255689E+03 0.30115000000008E+03 0.15226061957726E+01 0.15226061957726E+01
 0.26432528815892E+00 0.92638887720010E+02 0.92373240805410E+02 0.15698992757252E+01 -.95747085024517E+00
 0.15731160784740E+01 0.98690057702142E+02 0.15684894260491E+01 -.93937788582925E+00 0.15716997659800E+01
 0.98707620006809E+02 0.15698992757253E+01 -.95747085024517E+00 0.15731160784741E+01 0.98690057702142E+02
 0.15684894260491E+01 -.93937788582918E+00 0.15716997659800E+01 0.98707620006810E+02 0.30517634658721E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32716684541109E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.22582559323091E+00 0.00000000000000E+00 0.00000000000000E+00 0.22582559323091E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23008749193645E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23008749193645E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23370543863260E+00 0.27323631485443E+00 0.30116483108159E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     60.02064794
 0.12795157090319E+01 0.30134412780536E+03 0.38777186495158E+03 0.35090998249336E+03 0.34550768136918E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21910657869477E+00 0.00000000000000E+00 0.78784976908415E+01
 0.99943349827113E-03 0.17730913361564E+00 0.80000000000000E+04 0.30000000000000E+04 0.45118939091665E+02
 0.16919602159374E+02 0.30507752262052E+03 0.30115000000009E+03 0.30511096475807E+03 0.30774149609696E+03
 0.30115000000004E+03 0.30115000000004E+03 0.30414599742706E+03 0.30769820687580E+03 0.30115000000004E+03
 0.30115000000004E+03 0.30511096475807E+03 0.30774149609696E+03 0.30115000000004E+03 0.30115000000004E+03
 0.30414599742706E+03 0.30769820687580E+03 0.30115000000004E+03 0.30115000000004E+03 0.31887410858912E+03
 0.30118272349308E+03 0.89690319143329E+03 0.88652657453760E+03 0.42782532080822E+03 0.11672525561854E+04
 0.73728810877315E+03 0.62031411583325E+03 0.41729523217257E+03 0.61163070325094E+03 0.10960729451908E+04
 0.47193459851418E+03 0.41072741470722E+03 0.46603366047900E+03 0.10899535268394E+04 0.62031411583325E+03
 0.41729523217257E+03 0.61163070325094E+03 0.10960729451908E+04 0.47193459851418E+03 0.41072741470722E+03
 0.46603366047900E+03 0.10899535268394E+04 0.10108347014272E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.41368332986872E+03 0.21480996664309E+01
 0.21480996664309E+01 0.11438878422566E+00 0.93963527374017E+00 0.30115687293127E+03 0.31938707868631E+03
 0.30796724910299E+03 0.30771843550091E+03 0.22999999944800E+00 0.00000000000000E+00 0.22734433912037E+00
 0.00000000000000E+00 0.86047861518627E+01 0.99978911510355E-03 0.61536934099842E-02 0.80000000000000E+04
 0.30000000000000E+04 0.13000322679418E+04 0.48751210047816E+03 0.30118262870988E+03 0.31892483673561E+03
 0.30116311376250E+03 0.30190311343992E+03 0.30115000000004E+03 0.30115000000004E+03 0.30116307352029E+03
 0.30190326685401E+03 0.30115000000004E+03 0.30115000000004E+03 0.30116311376250E+03 0.30190311343992E+03
 0.30115000000004E+03 0.30115000000004E+03 0.30116307352029E+03 0.30190326685401E+03 0.30115000000004E+03
 0.30115000000004E+03 0.30164274031123E+03 0.30115000000009E+03 0.29728867972063E+01 0.29591039202211E+01
 0.40449048754880E+00 0.13567861380088E+03 0.13527210086089E+03 0.26997869572265E+01 -.92458443466862E+00
 0.26973888021397E+01 0.12705044064966E+03 0.26798099867176E+01 -.88962680529914E+00 0.26774312886423E+01
 0.12708409092186E+03 0.26997869572258E+01 -.92458443466862E+00 0.26973888021389E+01 0.12705044064966E+03
 0.26798099867177E+01 -.88962680529921E+00 0.26774312886424E+01 0.12708409092186E+03 0.48018889956681E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33549825855947E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.16358072962226E+00 0.00000000000000E+00 0.00000000000000E+00 0.16358072962226E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22377595363739E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22377595363739E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23333363094638E+00 0.26634808743272E+00 0.30843274711650E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     70.00725519
 0.94071473693135E+00 0.30152802298226E+03 0.40020030492642E+03 0.36925948167592E+03 0.36295613290900E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21674225696325E+00 0.00000000000000E+00 0.24518721805709E+00
 0.99874872573092E-03 0.20942063783854E+00 0.80000000000000E+04 0.30000000000000E+04 0.38200628565404E+02
 0.14325235712026E+02 0.30634654291499E+03 0.30115000000009E+03 0.30626637298697E+03 0.31009565296432E+03
 0.30115000000005E+03 0.30115000000005E+03 0.30506419144977E+03 0.31004658447947E+03 0.30115000000005E+03
 0.30115000000005E+03 0.30626637298697E+03 0.31009565296432E+03 0.30115000000005E+03 0.30115000000005E+03
 0.30506419144977E+03 0.31004658447947E+03 0.30115000000005E+03 0.30115000000005E+03 0.32454131544641E+03
 0.30122821302592E+03 0.10452912788582E+04 0.10307255332481E+04 0.46443224058220E+03 0.12873082866489E+04
 0.82055388486383E+03 0.68449395982151E+03 0.50147049012570E+03 0.67248591571771E+03 0.12538114027218E+04
 0.53072499349299E+03 0.49569587077818E+03 0.52252157197736E+03 0.12485412603323E+04 0.68449395982151E+03
 0.50147049012570E+03 0.67248591571771E+03 0.12538114027218E+04 0.53072499349299E+03 0.49569587077818E+03
 0.52252157197736E+03 0.12485412603323E+04 0.11870885563144E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.42732632410713E+03 0.18779519904972E+01
 0.18779519904972E+01 0.16432182047624E+00 0.81858026696332E+00 0.30115218295857E+03 0.32338202611733E+03
 0.31125075215236E+03 0.31085970530058E+03 0.22999999945358E+00 0.00000000000000E+00 0.22649922271723E+00
 0.00000000000000E+00 0.23637877834112E+01 0.99974308759215E-03 0.33147364592343E-01 0.80000000000000E+04
 0.30000000000000E+04 0.24134648707029E+03 0.90504932651358E+02 0.30122801062251E+03 0.32458374986262E+03
 0.30117700239797E+03 0.30221096635397E+03 0.30115000000005E+03 0.30115000000005E+03 0.30117671896678E+03
 0.30221123772854E+03 0.30115000000005E+03 0.30115000000005E+03 0.30117700239797E+03 0.30221096635397E+03
 0.30115000000005E+03 0.30115000000005E+03 0.30117671896678E+03 0.30221123772854E+03 0.30115000000005E+03
 0.30115000000005E+03 0.30190079152553E+03 0.30115000000009E+03 0.69985345196361E+01 0.69408785226902E+01
 0.39196993265392E+01 0.17517982719787E+03 0.17124052937470E+03 0.54727013382092E+01 0.25352720475074E+01
 0.54585730341093E+01 0.15478809489067E+03 0.53918580767260E+01 0.25863011976032E+01 0.53779340588437E+01
 0.15483682865025E+03 0.54727013382092E+01 0.25352720475074E+01 0.54585730341093E+01 0.15478809489067E+03
 0.53918580767260E+01 0.25863011976028E+01 0.53779340588436E+01 0.15483682865025E+03 0.65695133760489E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34198013938543E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11037686912938E+00 0.00000000000000E+00 0.00000000000000E+00 0.11037686912938E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22101740180692E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22101740180692E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23309144657112E+00 0.26280241680805E+00 0.31225036169896E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     80.15485653
 0.68124208431262E+00 0.30182639767815E+03 0.41058410919497E+03 0.38588733250204E+03 0.37952933313510E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21435997995230E+00 0.00000000000000E+00 -.47180045032764E+01
 0.99771252562314E-03 0.24096026950808E+00 0.80000000000000E+04 0.30000000000000E+04 0.33200494074529E+02
 0.12450185277948E+02 0.30770891143906E+03 0.30115000000010E+03 0.30742874837341E+03 0.31246487722104E+03
 0.30115000000007E+03 0.30115000000007E+03 0.30601927685252E+03 0.31241282648322E+03 0.30115000000007E+03
 0.30115000000007E+03 0.30742874837341E+03 0.31246487722104E+03 0.30115000000007E+03 0.30115000000007E+03
 0.30601927685252E+03 0.31241282648322E+03 0.30115000000007E+03 0.30115000000007E+03 0.33002949421119E+03
 0.30130561651360E+03 0.11972373764907E+04 0.11782577584177E+04 0.51332399640102E+03 0.13978490907073E+04
 0.88195847432425E+03 0.75185217212135E+03 0.59159515105588E+03 0.73647914365704E+03 0.13990957478916E+04
 0.59488799909090E+03 0.58655635972400E+03 0.58433438175550E+03 0.13945878108324E+04 0.75185217212135E+03
 0.59159515105588E+03 0.73647914365704E+03 0.13990957478916E+04 0.59488799909090E+03 0.58655635972400E+03
 0.58433438175550E+03 0.13945878108324E+04 0.13583146108548E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.44158882400551E+03 0.16807449335168E+01
 0.16807449335168E+01 0.21505982718287E+00 0.71355897778589E+00 0.30115065352116E+03 0.32686930521227E+03
 0.31463478867177E+03 0.31410836977805E+03 0.22999999923517E+00 0.00000000000000E+00 0.22560352643140E+00
 0.00000000000000E+00 -.14135112816562E+01 0.99971088340450E-03 0.56580515382893E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14139143035128E+03 0.53021786381731E+02 0.30130520459161E+03 0.33006526172371E+03
 0.30119959021264E+03 0.30254049384649E+03 0.30115000000007E+03 0.30115000000007E+03 0.30119888021056E+03
 0.30254089673072E+03 0.30115000000007E+03 0.30115000000007E+03 0.30119959021264E+03 0.30254049384649E+03
 0.30115000000007E+03 0.30115000000007E+03 0.30119888021056E+03 0.30254089673072E+03 0.30115000000007E+03
 0.30115000000007E+03 0.30218401591202E+03 0.30115000000010E+03 0.12419546546355E+02 0.12270558642124E+02
 0.90465638792760E+01 0.21237933458862E+03 0.20328753788994E+03 0.91767376727695E+01 0.75723518285998E+01
 0.91423983987180E+01 0.18100834287837E+03 0.90207099413830E+01 0.76372427899759E+01 0.89870023540810E+01
 0.18106983129760E+03 0.91767376727695E+01 0.75723518286001E+01 0.91423983987180E+01 0.18100834287837E+03
 0.90207099413829E+01 0.76372427899758E+01 0.89870023540808E+01 0.18106983129760E+03 0.83686177528868E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34745882975523E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.61693720166211E-01 0.00000000000000E+00 0.00000000000000E+00 0.61693720166211E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21971728214058E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21971728214058E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23284018567602E+00 0.26103746660181E+00 0.31401100096102E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
     90.21890766
 0.47531442458347E+00 0.30228849178411E+03 0.41875167671507E+03 0.40029946613820E+03 0.39466089540392E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21203205230894E+00 0.00000000000000E+00 -.72647853430634E+01
 0.99616232961154E-03 0.27151153364946E+00 0.80000000000000E+04 0.30000000000000E+04 0.29464678323126E+02
 0.11049254371172E+02 0.30913196869509E+03 0.30115000000011E+03 0.30859494927223E+03 0.31479299381407E+03
 0.30115000000007E+03 0.30115000000007E+03 0.30700603611126E+03 0.31473963595368E+03 0.30115000000007E+03
 0.30115000000007E+03 0.30859494927223E+03 0.31479299381407E+03 0.30115000000007E+03 0.30115000000007E+03
 0.30700603611126E+03 0.31473963595368E+03 0.30115000000007E+03 0.30115000000007E+03 0.33522703726593E+03
 0.30141460596119E+03 0.13473184566618E+04 0.13241342232047E+04 0.56422230077655E+03 0.14848591235262E+04
 0.91781571124580E+03 0.81914871534751E+03 0.67903133303418E+03 0.80054843571422E+03 0.15253615706471E+04
 0.66064626256883E+03 0.67462321774329E+03 0.64786565287690E+03 0.15214941151401E+04 0.81914871534751E+03
 0.67903133303418E+03 0.80054843571422E+03 0.15253615706471E+04 0.66064626256883E+03 0.67462321774329E+03
 0.64786565287690E+03 0.15214941151401E+04 0.15138856029154E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.45237382963366E+03 0.15162122430711E+01
 0.15162122430711E+01 0.26538008285896E+00 0.61758041916620E+00 0.30115305720971E+03 0.32973770111534E+03
 0.31796882353870E+03 0.31733638882166E+03 0.22999999912023E+00 0.00000000000000E+00 0.22469106034307E+00
 0.00000000000000E+00 -.29754073412191E+01 0.99968748847434E-03 0.78016705889890E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10254214028584E+03 0.38453302607189E+02 0.30141397446330E+03 0.33526030928817E+03
 0.30123084846213E+03 0.30287864105043E+03 0.30115000000007E+03 0.30115000000007E+03 0.30122956276494E+03
 0.30287917648916E+03 0.30115000000007E+03 0.30115000000007E+03 0.30123084846213E+03 0.30287864105043E+03
 0.30115000000007E+03 0.30115000000007E+03 0.30122956276494E+03 0.30287917648916E+03 0.30115000000007E+03
 0.30115000000007E+03 0.30247735024036E+03 0.30115000000011E+03 0.18974915440096E+02 0.18675439789814E+02
 0.15447225677681E+02 0.24477685670588E+03 0.22925239489981E+03 0.13625087478134E+02 0.13826436211501E+02
 0.13561501620633E+02 0.20423700877004E+03 0.13388283197194E+02 0.13902153553382E+02 0.13326029679660E+02
 0.20430820587707E+03 0.13625087478134E+02 0.13826436211501E+02 0.13561501620633E+02 0.20423700877005E+03
 0.13388283197194E+02 0.13902153553382E+02 0.13326029679660E+02 0.20430820587707E+03 0.10094155409287E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35075992617039E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.20372482448331E-01 0.11544080675418E-02 0.00000000000000E+00 0.20372482448331E-01 0.11544080675418E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21933375634745E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21933375634745E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23277252463338E+00 0.25911381344482E+00 0.31624540664342E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    100.25774734
 0.31440397091362E+00 0.30299533438639E+03 0.42544666575321E+03 0.41261360414475E+03 0.40815948277041E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20967983566577E+00 0.00000000000000E+00 -.86305578580378E+01
 0.11121455771202E-02 0.30227257527801E+00 0.71933028953957E+04 0.26974885857734E+04 0.26466178721778E+02
 0.99248170206669E+01 0.31061278295237E+03 0.30115000000011E+03 0.30977752262018E+03 0.31708410001021E+03
 0.30115000000008E+03 0.30115000000008E+03 0.30803178975706E+03 0.31703043369993E+03 0.30115000000008E+03
 0.30115000000008E+03 0.30977752262018E+03 0.31708410001021E+03 0.30115000000008E+03 0.30115000000008E+03
 0.30803178975706E+03 0.31703043369993E+03 0.30115000000008E+03 0.30115000000008E+03 0.34017028947677E+03
 0.30155423644195E+03 0.14924707644457E+04 0.14657819355279E+04 0.61511829335788E+03 0.15575902485927E+04
 0.93939636376802E+03 0.88565665840482E+03 0.76316199170435E+03 0.86418115011576E+03 0.16383263404066E+04
 0.72695684870492E+03 0.75928423996429E+03 0.71230827453270E+03 0.16349896427867E+04 0.88565665840482E+03
 0.76316199170435E+03 0.86418115011576E+03 0.16383263404066E+04 0.72695684870492E+03 0.75928423996429E+03
 0.71230827453270E+03 0.16349896427867E+04 0.16553894274918E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.46221861932723E+03 0.14689787360928E+01
 0.14689787360928E+01 0.31557428124552E+00 0.53882762039968E+00 0.30115992273222E+03 0.33208666168318E+03
 0.32097720757940E+03 0.32027217777884E+03 0.22999999910209E+00 0.00000000000000E+00 0.22375541597244E+00
 0.00000000000000E+00 -.35374055772670E+01 0.99965915197550E-03 0.98581749814341E-01 0.80000000000000E+04
 0.30000000000000E+04 0.81150923117782E+02 0.30431596169168E+02 0.30155339040652E+03 0.34020344242987E+03
 0.30127058442763E+03 0.30321918688566E+03 0.30115000000008E+03 0.30115000000008E+03 0.30126861066264E+03
 0.30321984839200E+03 0.30115000000008E+03 0.30115000000008E+03 0.30127058442763E+03 0.30321918688566E+03
 0.30115000000008E+03 0.30115000000008E+03 0.30126861066264E+03 0.30321984839200E+03 0.30115000000008E+03
 0.30115000000008E+03 0.30277505638191E+03 0.30115000000011E+03 0.26264794400426E+02 0.25746955612029E+02
 0.22666231818204E+02 0.27321908622547E+03 0.25043952324817E+03 0.18572987147371E+02 0.20862276652897E+02
 0.18470895149056E+02 0.22486500858489E+03 0.18256478924439E+02 0.20945656961681E+02 0.18156694151778E+02
 0.22494280133435E+03 0.18572987147371E+02 0.20862276652897E+02 0.18470895149056E+02 0.22486500858489E+03
 0.18256478924439E+02 0.20945656961681E+02 0.18156694151778E+02 0.22494280133435E+03 0.11696162058036E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35360450617191E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.65224634918087E-03 0.25694264834794E-01 0.00000000000000E+00 0.65224634918087E-03 0.25694264834794E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21956499527101E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21956499527101E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23274563564775E+00 0.25111116958954E+00 0.32628430274351E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    110.06345791
 0.20432900544985E+00 0.30396346088336E+03 0.43114420503963E+03 0.42248196671769E+03 0.41919805389252E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20727875858250E+00 0.00000000000000E+00 -.10678034249577E+02
 0.17112663762697E-02 0.33356123413942E+00 0.46749004777612E+04 0.17530876791605E+04 0.23983602353072E+02
 0.89938508824021E+01 0.31209877749965E+03 0.30115000000011E+03 0.31095133541366E+03 0.31929820867372E+03
 0.30115000000008E+03 0.30115000000008E+03 0.30907133088214E+03 0.31924479159440E+03 0.30115000000008E+03
 0.30115000000008E+03 0.31095133541366E+03 0.31929820867372E+03 0.30115000000008E+03 0.30115000000008E+03
 0.30907133088214E+03 0.31924479159440E+03 0.30115000000008E+03 0.30115000000008E+03 0.34482301982815E+03
 0.30171484748261E+03 0.16238826049678E+04 0.15948210623751E+04 0.66494700438927E+03 0.16208392790782E+04
 0.95256753966697E+03 0.94857561794169E+03 0.84328421259004E+03 0.92483104030148E+03 0.17393212885874E+04
 0.79059972233526E+03 0.83982767141802E+03 0.77468479698173E+03 0.17364002885858E+04 0.94857561794169E+03
 0.84328421259004E+03 0.92483104030148E+03 0.17393212885874E+04 0.79059972233526E+03 0.83982767141802E+03
 0.77468479698173E+03 0.17364002885858E+04 0.17789588276211E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.47272488183924E+03 0.14689952281047E+01
 0.14689952281047E+01 0.36460283410377E+00 0.49414423731804E+00 0.30117103501006E+03 0.33403049194471E+03
 0.32320561775427E+03 0.32244111546751E+03 0.22999999894417E+00 0.00000000000000E+00 0.22279331369310E+00
 0.00000000000000E+00 -.49685463519998E+01 0.99960814340391E-03 0.11879256424587E+00 0.80000000000000E+04
 0.30000000000000E+04 0.67344282453925E+02 0.25254105920222E+02 0.30171381985363E+03 0.34485766329392E+03
 0.30131631898142E+03 0.30355523538062E+03 0.30115000000008E+03 0.30115000000008E+03 0.30131358364938E+03
 0.30355601392371E+03 0.30115000000008E+03 0.30115000000008E+03 0.30131631898142E+03 0.30355523538062E+03
 0.30115000000008E+03 0.30115000000008E+03 0.30131358364938E+03 0.30355601392371E+03 0.30115000000008E+03
 0.30115000000008E+03 0.30306725897276E+03 0.30115000000011E+03 0.33458299894107E+02 0.32663174799731E+02
 0.29950334529135E+02 0.29781175579903E+03 0.26771166959725E+03 0.23520422644346E+02 0.27949905880677E+02
 0.23373778018577E+02 0.24370564135904E+03 0.23126249794405E+02 0.28039492132131E+02 0.22983103255715E+02
 0.24378862483821E+03 0.23520422644346E+02 0.27949905880677E+02 0.23373778018577E+02 0.24370564135904E+03
 0.23126249794405E+02 0.28039492132131E+02 0.22983103255715E+02 0.24378862483821E+03 0.13064176521621E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35604788062650E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.69879367325626E-01 0.00000000000000E+00 0.00000000000000E+00 0.69879367325626E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21971214602100E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21971214602100E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23268534115667E+00 0.24522087638592E+00 0.33403049194471E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    120.09894533
 0.13498481538032E+00 0.30509593447270E+03 0.43627605932252E+03 0.43037361767772E+03 0.42799597113260E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20469338204650E+00 0.00000000000000E+00 -.13695185351099E+02
 0.25903664385022E-02 0.36718514713930E+00 0.30883661404391E+04 0.11581373026647E+04 0.21787373651486E+02
 0.81702651193074E+01 0.31363375588563E+03 0.30115000000011E+03 0.31216416086125E+03 0.32154887922275E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31016304179530E+03 0.32149602071845E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31216416086125E+03 0.32154887922275E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31016304179530E+03 0.32149602071845E+03 0.30115000000009E+03 0.30115000000009E+03 0.34942127677520E+03
 0.30190426997470E+03 0.17436581725429E+04 0.17127512480830E+04 0.71472055782668E+03 0.16765478598468E+04
 0.95825369923094E+03 0.10091123339555E+04 0.92237777230733E+03 0.98323538260597E+03 0.18372730703456E+04
 0.85225275484024E+03 0.91926806627781E+03 0.83527766860055E+03 0.18346916512754E+04 0.10091123339555E+04
 0.92237777230733E+03 0.98323538260597E+03 0.18372730703456E+04 0.85225275484024E+03 0.91926806627781E+03
 0.83527766860055E+03 0.18346916512754E+04 0.18905497762256E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.48141280898675E+03 0.14690195315025E+01
 0.14690195315025E+01 0.41478027119022E+00 0.45376038734444E+00 0.30118860375612E+03 0.33573763633801E+03
 0.32528631473352E+03 0.32447816113425E+03 0.22999999894728E+00 0.00000000000000E+00 0.22174449277392E+00
 0.00000000000000E+00 -.75281236613239E+01 0.99952457531943E-03 0.14012648360576E+00 0.80000000000000E+04
 0.30000000000000E+04 0.57091277780920E+02 0.21409229167845E+02 0.30190309151952E+03 0.34945837843903E+03
 0.30137014577825E+03 0.30390114358224E+03 0.30115000000009E+03 0.30115000000009E+03 0.30136655012827E+03
 0.30390203399432E+03 0.30115000000009E+03 0.30115000000009E+03 0.30137014577825E+03 0.30390114358224E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30136655012827E+03 0.30390203399432E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30336688168980E+03 0.30115000000011E+03 0.41305085909170E+02 0.40158178116053E+02
 0.38018645633875E+02 0.32087177693389E+03 0.28266303807184E+03 0.28940779622814E+02 0.35773678420062E+02
 0.28743005925374E+02 0.26144988965733E+03 0.28468289075658E+02 0.35867562370666E+02 0.28275476525134E+02
 0.26153619885106E+03 0.28940779622814E+02 0.35773678420062E+02 0.28743005925374E+02 0.26144988965733E+03
 0.28468289075658E+02 0.35867562370666E+02 0.28275476525134E+02 0.26153619885106E+03 0.14395377527125E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35826820954817E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11083043505024E+00 0.00000000000000E+00 0.00000000000000E+00 0.11083043505024E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21999839683816E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21999839683816E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23258198179563E+00 0.24385945123029E+00 0.33573763633801E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    130.35634362
 0.98139135204516E-01 0.30603181688214E+03 0.44071320691915E+03 0.43630736853703E+03 0.43445845293030E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20194367670854E+00 0.00000000000000E+00 -.17161701832628E+02
 0.35628938535851E-02 0.40302589593783E+00 0.22453657977911E+04 0.84201217417166E+03 0.19849841116002E+02
 0.74436904185006E+01 0.31517556517251E+03 0.30115000000011E+03 0.31338973243999E+03 0.32380936303659E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31127798440928E+03 0.32375721693960E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31338973243999E+03 0.32380936303659E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31127798440928E+03 0.32375721693960E+03 0.30115000000009E+03 0.30115000000009E+03 0.35389914172573E+03
 0.30212364103972E+03 0.18479374259039E+04 0.18141559261414E+04 0.76139861318050E+03 0.17206620562484E+04
 0.95545645000203E+03 0.10646002516645E+04 0.99732067834917E+03 0.10355806136133E+04 0.19272097684794E+04
 0.90882481161816E+03 0.99450136446252E+03 0.88986567651162E+03 0.19249106670866E+04 0.10646002516645E+04
 0.99732067834917E+03 0.10355806136133E+04 0.19272097684794E+04 0.90882481161816E+03 0.99450136446252E+03
 0.88986567651162E+03 0.19249106670866E+04 0.19855036732033E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.48784429800148E+03 0.14690474558259E+01
 0.14690474558259E+01 0.46606726262181E+00 0.41473251968002E+00 0.30121606139629E+03 0.33722694196872E+03
 0.32727035314492E+03 0.32643669417826E+03 0.22999999892653E+00 0.00000000000000E+00 0.22059846110176E+00
 0.00000000000000E+00 -.10687638933897E+02 0.99940228539567E-03 0.16286278663074E+00 0.80000000000000E+04
 0.30000000000000E+04 0.49121104737931E+02 0.18420414276724E+02 0.30212229908059E+03 0.35393892727641E+03
 0.30143185035799E+03 0.30425211608297E+03 0.30115000000009E+03 0.30115000000009E+03 0.30142731888603E+03
 0.30425310979387E+03 0.30115000000009E+03 0.30115000000009E+03 0.30143185035799E+03 0.30425211608297E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30142731888603E+03 0.30425310979387E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30367014264749E+03 0.30115000000011E+03 0.49808189480377E+02 0.48233795981322E+02
 0.46843898208754E+02 0.34252245607221E+03 0.29544433837241E+03 0.34808709963079E+02 0.44321087906411E+02
 0.34558156282596E+02 0.27814064239453E+03 0.34259010021849E+02 0.44417590372786E+02 0.34015114682208E+02
 0.27822867561332E+03 0.34808709963079E+02 0.44321087906411E+02 0.34558156282596E+02 0.27814064239453E+03
 0.34259010021849E+02 0.44417590372786E+02 0.34015114682208E+02 0.27822867561332E+03 0.15700900337445E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36025569731557E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14633704553092E+00 0.00000000000000E+00 0.00000000000000E+00 0.14633704553092E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22057829397801E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22057829397801E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23245607073868E+00 0.24264348458086E+00 0.33722694196872E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    140.17255574
 0.84054928236991E-01 0.30653650520423E+03 0.44407457546832E+03 0.44022099125968E+03 0.43856125906470E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19927243185790E+00 0.00000000000000E+00 -.20272315553968E+02
 0.41598853372428E-02 0.43809901867901E+00 0.19231299306202E+04 0.72117372398259E+03 0.18260711982698E+02
 0.68477669935118E+01 0.31661884859065E+03 0.30115000000011E+03 0.31454298729392E+03 0.32593169100155E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31233541604719E+03 0.32588037811674E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31454298729392E+03 0.32593169100155E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31233541604719E+03 0.32588037811674E+03 0.30115000000009E+03 0.30115000000009E+03 0.35794439534834E+03
 0.30236454893455E+03 0.19290549362683E+04 0.18906378179804E+04 0.79853819443556E+03 0.17463595769744E+04
 0.94382869156664E+03 0.11097081037524E+04 0.10597956467038E+04 0.10763234928766E+04 0.19977437577613E+04
 0.95470850237483E+03 0.10572109948139E+04 0.93243599104018E+03 0.19956701081221E+04 0.11097081037524E+04
 0.10597956467038E+04 0.10763234928766E+04 0.19977437577613E+04 0.95470850237483E+03 0.10572109948139E+04
 0.93243599104018E+03 0.19956701081221E+04 0.20545843584307E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49195167648480E+03 0.14690725143339E+01
 0.14690725143339E+01 0.51514832322839E+00 0.37961818728898E+00 0.30125508840111E+03 0.33842826093541E+03
 0.32902051935324E+03 0.32817970425049E+03 0.22999999890284E+00 0.00000000000000E+00 0.21942871743091E+00
 0.00000000000000E+00 -.13628776639731E+02 0.99924379614916E-03 0.18565126893052E+00 0.80000000000000E+04
 0.30000000000000E+04 0.43091544949225E+02 0.16159329355959E+02 0.30236303284029E+03 0.35798635072714E+03
 0.30149903718627E+03 0.30458949263771E+03 0.30115000000009E+03 0.30115000000009E+03 0.30149355515547E+03
 0.30459057048835E+03 0.30115000000009E+03 0.30115000000009E+03 0.30149903718627E+03 0.30458949263771E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30149355515547E+03 0.30459057048835E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30396209912695E+03 0.30115000000011E+03 0.58285199464061E+02 0.56227468789628E+02
 0.55742762420441E+02 0.36137660778513E+03 0.30535513155259E+03 0.40737455521289E+02 0.52915256744603E+02
 0.40439663046135E+02 0.29273549234801E+03 0.40119379903151E+02 0.53011663100359E+02 0.39830046042466E+02
 0.29282270593520E+03 0.40737455521289E+02 0.52915256744603E+02 0.40439663046135E+02 0.29273549234801E+03
 0.40119379903151E+02 0.53011663100359E+02 0.39830046042466E+02 0.29282270593520E+03 0.16887971227152E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36187581585275E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17289342459138E+00 0.00000000000000E+00 0.00000000000000E+00 0.17289342459138E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22146237806975E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22146237806975E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23233948223856E+00 0.24165388909454E+00 0.33842826093541E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    150.12225248
 0.81617424915930E-01 0.30683619621639E+03 0.44657431943537E+03 0.44277263084213E+03 0.44110899605001E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19659639139895E+00 0.00000000000000E+00 -.22856232238594E+02
 0.42841172568792E-02 0.47362800477661E+00 0.18673625207513E+04 0.70026094528173E+03 0.16890893104544E+02
 0.63340849142039E+01 0.31801067741249E+03 0.30115000000011E+03 0.31566320302030E+03 0.32798273704153E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31336480630699E+03 0.32793230866193E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31566320302030E+03 0.32798273704153E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31336480630699E+03 0.32793230866193E+03 0.30115000000009E+03 0.30115000000009E+03 0.36172062105304E+03
 0.30263390635368E+03 0.19932348919438E+04 0.19492294720799E+04 0.82672139194587E+03 0.17553421920069E+04
 0.92448719310134E+03 0.11467008800769E+04 0.11115706387282E+04 0.11087233641082E+04 0.20511571946863E+04
 0.99224688378543E+03 0.11091934277712E+04 0.96623499484619E+03 0.20492810568767E+04 0.11467008800769E+04
 0.11115706387282E+04 0.11087233641082E+04 0.20511571946863E+04 0.99224688378543E+03 0.11091934277712E+04
 0.96623499484619E+03 0.20492810568767E+04 0.21030393259778E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49450514447948E+03 0.14690933306906E+01
 0.14690933306906E+01 0.56489680692222E+00 0.34631122912013E+00 0.30131110488492E+03 0.33943680563080E+03
 0.33063456677322E+03 0.32980225136055E+03 0.22999999890647E+00 0.00000000000000E+00 0.21817109499842E+00
 0.00000000000000E+00 -.16131111209055E+02 0.99903334299299E-03 0.20983984732190E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38124312908633E+02 0.14296617340738E+02 0.30263219000688E+03 0.36176404549823E+03
 0.30157292915296E+03 0.30492609981160E+03 0.30115000000009E+03 0.30115000000009E+03 0.30156646832707E+03
 0.30492724975370E+03 0.30115000000009E+03 0.30115000000009E+03 0.30157292915296E+03 0.30492609981160E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30156646832707E+03 0.30492724975370E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30425269589549E+03 0.30115000000011E+03 0.67045790863435E+02 0.64444675497417E+02
 0.64986350891000E+02 0.37855514254338E+03 0.31324385989792E+03 0.46878849149654E+02 0.61840801784238E+02
 0.46547168176877E+02 0.30611509680961E+03 0.46197711885472E+02 0.61936026391663E+02 0.45876358352381E+02
 0.30620051607993E+03 0.46878849149654E+02 0.61840801784238E+02 0.46547168176877E+02 0.30611509680961E+03
 0.46197711885472E+02 0.61936026391662E+02 0.45876358352381E+02 0.30620051607993E+03 0.18016552675393E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36323135987856E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19254483533953E+00 0.00000000000000E+00 0.00000000000000E+00 0.19254483533953E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22260297766629E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22260297766629E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23224056325785E+00 0.24082735050197E+00 0.33943680563080E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    160.14391063
 0.84730831530687E-01 0.30712047049014E+03 0.44828633048166E+03 0.44429929691472E+03 0.44254126607813E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19398811809818E+00 0.00000000000000E+00 -.24735889739872E+02
 0.41266974677457E-02 0.50869842793498E+00 0.19385961928462E+04 0.72697357231734E+03 0.15726409913385E+02
 0.58974037175194E+01 0.31934212171464E+03 0.30115000000011E+03 0.31674331175412E+03 0.32993531110065E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31435862288028E+03 0.32988584351039E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31674331175412E+03 0.32993531110065E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31435862288028E+03 0.32988584351039E+03 0.30115000000009E+03 0.30115000000009E+03 0.36516718623272E+03
 0.30293494456435E+03 0.20420326273950E+04 0.19925068955654E+04 0.84551972230380E+03 0.17494095677676E+04
 0.89966224685224E+03 0.11757590429767E+04 0.11517312647317E+04 0.11337996104660E+04 0.20872054946698E+04
 0.10217180561426E+04 0.11495393604769E+04 0.99237944480446E+03 0.20855037456495E+04 0.11757590429767E+04
 0.11517312647317E+04 0.11337996104660E+04 0.20872054946698E+04 0.10217180561426E+04 0.11495393604769E+04
 0.99237944480446E+03 0.20855037456495E+04 0.21328711769562E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49594283892812E+03 0.14691084739141E+01
 0.14691084739141E+01 0.61500509770083E+00 0.31501293890072E+00 0.30138876488323E+03 0.34024614943861E+03
 0.33208576350075E+03 0.33127652414349E+03 0.22999999828375E+00 0.00000000000000E+00 0.21683806038539E+00
 0.00000000000000E+00 -.17993306856322E+02 0.99875755309562E-03 0.23525660179408E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34005421905237E+02 0.12752033214464E+02 0.30293303846372E+03 0.36521134303242E+03
 0.30165425177871E+03 0.30526094085459E+03 0.30115000000009E+03 0.30115000000009E+03 0.30164679797334E+03
 0.30526214723072E+03 0.30115000000009E+03 0.30115000000009E+03 0.30165425177871E+03 0.30526094085459E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30164679797334E+03 0.30526214723072E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30454155231993E+03 0.30115000000011E+03 0.75844737716409E+02 0.72642793275092E+02
 0.74359485140026E+02 0.39369536170044E+03 0.31896407913472E+03 0.53150998120381E+02 0.70869797740579E+02
 0.52806846804868E+02 0.31805164355938E+03 0.52415605501706E+02 0.70961908641151E+02 0.52083617275013E+02
 0.31813347831700E+03 0.53150998120381E+02 0.70869797740579E+02 0.52806846804868E+02 0.31805164355938E+03
 0.52415605501706E+02 0.70961908641151E+02 0.52083617275013E+02 0.31813347831700E+03 0.19061507338873E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36421561110951E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20582562755760E+00 0.00000000000000E+00 0.00000000000000E+00 0.20582562755760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22405108383480E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22405108383480E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23216715344171E+00 0.24017413498582E+00 0.34024614943861E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    170.17425072
 0.89726104045480E-01 0.30743847519526E+03 0.44939258049500E+03 0.44514691755439E+03 0.44327109662289E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19149541173735E+00 0.00000000000000E+00 -.25958562907820E+02
 0.38969536787305E-02 0.54260504251705E+00 0.20528855766656E+04 0.76983209124962E+03 0.14743689006076E+02
 0.55288833772785E+01 0.32059732715216E+03 0.30115000000011E+03 0.31776919813431E+03 0.33175890271633E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31530238655545E+03 0.33171041519780E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31776919813431E+03 0.33175890271633E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31530238655545E+03 0.33171041519780E+03 0.30115000000009E+03 0.30115000000009E+03 0.36826869070344E+03
 0.30326134970770E+03 0.20785455179598E+04 0.20239614570663E+04 0.85632563602783E+03 0.17327249409382E+04
 0.87211767673022E+03 0.11981643287408E+04 0.11816287559349E+04 0.11528741432769E+04 0.21090362926854E+04
 0.10444926764165E+04 0.11796027759334E+04 0.10123650272924E+04 0.21074892377050E+04 0.11981643287408E+04
 0.11816287559349E+04 0.11528741432769E+04 0.21090362926854E+04 0.10444926764165E+04 0.11796027759334E+04
 0.10123650272924E+04 0.21074892377050E+04 0.21484991950891E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49668055643625E+03 0.14691183244374E+01
 0.14691183244374E+01 0.66515679814880E+00 0.28514339261374E+00 0.30149218269371E+03 0.34084513542131E+03
 0.33336431245457E+03 0.33259262769264E+03 0.22999999828481E+00 0.00000000000000E+00 0.21545481353597E+00
 0.00000000000000E+00 -.19232464139715E+02 0.99840274291344E-03 0.26150735247776E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30591874087672E+02 0.11471952782877E+02 0.30325924932993E+03 0.36831307902439E+03
 0.30174061489720E+03 0.30558693424886E+03 0.30115000000009E+03 0.30115000000009E+03 0.30173219716468E+03
 0.30558818163656E+03 0.30115000000009E+03 0.30115000000009E+03 0.30174061489720E+03 0.30558693424886E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30173219716468E+03 0.30558818163656E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30482183751374E+03 0.30115000000011E+03 0.84400670220983E+02 0.80569872631725E+02
 0.83528256765504E+02 0.40641326552035E+03 0.32246736747102E+03 0.59349963053293E+02 0.79680641799774E+02
 0.59026694167377E+02 0.32816842703495E+03 0.58572361917554E+02 0.79768325996615E+02 0.58262833750903E+02
 0.32824550574172E+03 0.59349963053293E+02 0.79680641799774E+02 0.59026694167377E+02 0.32816842703495E+03
 0.58572361917554E+02 0.79768325996615E+02 0.58262833750903E+02 0.32824550574172E+03 0.19996775029227E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36483753254563E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21409659827348E+00 0.00000000000000E+00 0.00000000000000E+00 0.21409659827348E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22552708444439E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22552708444439E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23211863588836E+00 0.23969902812283E+00 0.34084513542131E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    180.25025023
 0.94725715658116E-01 0.30779233607948E+03 0.45009918785700E+03 0.44560581506444E+03 0.44362286381394E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18912027981063E+00 0.00000000000000E+00 -.26682444640574E+02
 0.36912729074929E-02 0.57520128666192E+00 0.21672740543677E+04 0.81272777038791E+03 0.13908174730322E+02
 0.52155655238708E+01 0.32178725016374E+03 0.30115000000011E+03 0.31874793245748E+03 0.33346560054302E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31620273337006E+03 0.33341811441696E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31874793245748E+03 0.33346560054302E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31620273337006E+03 0.33341811441696E+03 0.30115000000009E+03 0.30115000000009E+03 0.37106431549641E+03
 0.30361475305982E+03 0.21063765291537E+04 0.20471961137279E+04 0.86139613052376E+03 0.17096961184152E+04
 0.84399300723881E+03 0.12157022964556E+04 0.12037320464173E+04 0.11675135956677E+04 0.21207580386775E+04
 0.10624076522887E+04 0.12018557442432E+04 0.10278749008207E+04 0.21193491881201E+04 0.12157022964556E+04
 0.12037320464173E+04 0.11675135956677E+04 0.21207580386775E+04 0.10624076522887E+04 0.12018557442432E+04
 0.10278749008207E+04 0.21193491881201E+04 0.21546639687008E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49704125516418E+03 0.14691241565027E+01
 0.14691241565027E+01 0.71553679569482E+00 0.25658028235571E+00 0.30162737228741E+03 0.34126354547013E+03
 0.33448363846565E+03 0.33376133894758E+03 0.22999999827987E+00 0.00000000000000E+00 0.21402663380797E+00
 0.00000000000000E+00 -.19989240728054E+02 0.99794780062709E-03 0.28853734720535E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27726046827160E+02 0.10397267560185E+02 0.30361247926303E+03 0.37110874450840E+03
 0.30183211358901E+03 0.30590384476813E+03 0.30115000000009E+03 0.30115000000009E+03 0.30182278064029E+03
 0.30590511625010E+03 0.30115000000009E+03 0.30115000000009E+03 0.30183211358901E+03 0.30590384476813E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30182278064029E+03 0.30590511625010E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30509392987021E+03 0.30115000000011E+03 0.92579742076963E+02 0.88108065326688E+02
 0.92377586913003E+02 0.41699726183250E+03 0.32415778698493E+03 0.65434828823912E+02 0.88157378823656E+02
 0.65175275737017E+02 0.33658751307404E+03 0.64628655313794E+02 0.88239064080989E+02 0.64383863749833E+02
 0.33665841650500E+03 0.65434828823912E+02 0.88157378823655E+02 0.65175275737017E+02 0.33658751307404E+03
 0.64628655313794E+02 0.88239064080989E+02 0.64383863749833E+02 0.33665841650500E+03 0.20826453490331E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36528235523509E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21887588736563E+00 0.00000000000000E+00 0.00000000000000E+00 0.21887588736563E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22697975472947E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22697975472947E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23208942852779E+00 0.23937322939114E+00 0.34126354547013E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    191.32588613
 0.99313992338504E-01 0.30820071345524E+03 0.45060455321453E+03 0.44589032193092E+03 0.44381595421337E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18665598886755E+00 0.00000000000000E+00 -.27131743075885E+02
 0.35207372830441E-02 0.60922076920490E+00 0.22722513373912E+04 0.85209425152169E+03 0.13131528674639E+02
 0.49243232529898E+01 0.32301793758527E+03 0.30115000000013E+03 0.31976518235899E+03 0.33520777698510E+03
 0.30115000000009E+03 0.30115000000009E+03 0.31713821505566E+03 0.33516136878204E+03 0.30115000000009E+03
 0.30115000000009E+03 0.31976518235899E+03 0.33520777698510E+03 0.30115000000009E+03 0.30115000000009E+03
 0.31713821505566E+03 0.33516136878204E+03 0.30115000000009E+03 0.30115000000009E+03 0.37383294593760E+03
 0.30402878370722E+03 0.21301843933757E+04 0.20664117849849E+04 0.86256534415207E+03 0.16809777326657E+04
 0.81409956179286E+03 0.12310364787774E+04 0.12216585798255E+04 0.11799737279737E+04 0.21262257907808E+04
 0.10781830593075E+04 0.12199293673003E+04 0.10412951866790E+04 0.21249517038192E+04 0.12310364787774E+04
 0.12216585798255E+04 0.11799737279737E+04 0.21262257907808E+04 0.10781830593075E+04 0.12199293673003E+04
 0.10412951866790E+04 0.21249517038192E+04 0.21549162690167E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49724275010593E+03 0.14691277763745E+01
 0.14691277763745E+01 0.77091497519289E+00 0.22693897075451E+00 0.30181870608231E+03 0.34155527079507E+03
 0.33554342073025E+03 0.33488478589399E+03 0.22999999818535E+00 0.00000000000000E+00 0.21242193550427E+00
 0.00000000000000E+00 -.20481567633004E+02 0.99731031696225E-03 0.31885767392632E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25089563947106E+02 0.94085864801649E+01 0.30402634097707E+03 0.37387738170723E+03
 0.30193603319370E+03 0.30623732015861E+03 0.30115000000009E+03 0.30115000000009E+03 0.30192578360731E+03
 0.30623859988945E+03 0.30115000000009E+03 0.30115000000009E+03 0.30193603319370E+03 0.30623732015861E+03
 0.30115000000009E+03 0.30115000000009E+03 0.30192578360731E+03 0.30623859988945E+03 0.30115000000009E+03
 0.30115000000009E+03 0.30537960320209E+03 0.30115000000011E+03 0.10096812069633E+03 0.95818871648361E+02
 0.10156555253631E+03 0.42647565182260E+03 0.32440227152361E+03 0.71883140306296E+02 0.96929944628358E+02
 0.71749790585941E+02 0.34412000779887E+03 0.71061685309836E+02 0.97003704253937E+02 0.70942682070373E+02
 0.34418295319362E+03 0.71883140306295E+02 0.96929944628358E+02 0.71749790585941E+02 0.34412000779887E+03
 0.71061685309836E+02 0.97003704253937E+02 0.70942682070373E+02 0.34418295319362E+03 0.21621616213725E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36562604776581E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22180081522638E+00 0.00000000000000E+00 0.00000000000000E+00 0.22180081522638E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22853154981884E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22853154981884E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23207098954133E+00 0.23914861458180E+00 0.34155527079507E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    200.01204161
 0.10205254794684E+00 0.30854240673034E+03 0.45090750709068E+03 0.44606460001385E+03 0.44393939767114E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18482408499373E+00 0.00000000000000E+00 -.27265041431843E+02
 0.34262591749554E-02 0.63456335460740E+00 0.23349080123526E+04 0.87559050463224E+03 0.12607094219851E+02
 0.47276603324440E+01 0.32394391786934E+03 0.30115000000018E+03 0.32053424747552E+03 0.33649567296696E+03
 0.30115000000010E+03 0.30115000000010E+03 0.31784638348555E+03 0.33645015056352E+03 0.30115000000010E+03
 0.30115000000010E+03 0.32053424747552E+03 0.33649567296696E+03 0.30115000000010E+03 0.30115000000010E+03
 0.31784638348555E+03 0.33645015056353E+03 0.30115000000010E+03 0.30115000000010E+03 0.37579125155493E+03
 0.30437889197666E+03 0.21456809066245E+04 0.20786076177215E+04 0.86197887781653E+03 0.16588013113924E+04
 0.79251253918681E+03 0.12411545657985E+04 0.12326590123611E+04 0.11880148165489E+04 0.21274086859468E+04
 0.10886685864130E+04 0.12310337223891E+04 0.10500997764381E+04 0.21262285376207E+04 0.12411545657985E+04
 0.12326590123612E+04 0.11880148165489E+04 0.21274086859468E+04 0.10886685864130E+04 0.12310337223891E+04
 0.10500997764381E+04 0.21262285376207E+04 0.21528502798496E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49737056186102E+03 0.14691288503264E+01
 0.14691288503264E+01 0.81434575257264E+00 0.20501753799215E+00 0.30200656186980E+03 0.34169168346760E+03
 0.33626758618433E+03 0.33566310799267E+03 0.22999999844368E+00 0.00000000000000E+00 0.21114379613108E+00
 0.00000000000000E+00 -.20641384314304E+02 0.99668839159494E-03 0.34298329478903E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23324751151279E+02 0.87467816817297E+01 0.30437629612279E+03 0.37583550600222E+03
 0.30202309602987E+03 0.30649150409917E+03 0.30115000000010E+03 0.30115000000010E+03 0.30201216414163E+03
 0.30649277206256E+03 0.30115000000010E+03 0.30115000000010E+03 0.30202309602987E+03 0.30649150409917E+03
 0.30115000000010E+03 0.30115000000010E+03 0.30201216414163E+03 0.30649277206256E+03 0.30115000000010E+03
 0.30115000000010E+03 0.30559796485114E+03 0.30115000000014E+03 0.10703508750904E+03 0.10137865789804E+03
 0.10835525658534E+03 0.43257686528266E+03 0.32367983241439E+03 0.76780220685956E+02 0.10338633872836E+03
 0.76780220685956E+02 0.34895462604068E+03 0.75958793933672E+02 0.10345257341256E+03 0.75958793933672E+02
 0.34901017850645E+03 0.76780220685956E+02 0.10338633872836E+03 0.76780220685956E+02 0.34895462604068E+03
 0.75958793933672E+02 0.10345257341256E+03 0.75958793933672E+02 0.34901017850645E+03 0.22166789068379E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36581987484135E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22264176485523E+00 0.00000000000000E+00 0.00000000000000E+00 0.22264176485523E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22949748096208E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22949748096208E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206556215717E+00 0.23904717198794E+00 0.34169168346760E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    210.00189238
 0.10439453462733E+00 0.30893163689596E+03 0.45121498422483E+03 0.44626378294829E+03 0.44409749981920E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18281751522668E+00 0.00000000000000E+00 -.27369183061911E+02
 0.33493944620308E-02 0.66231859356445E+00 0.23884914394793E+04 0.89568428980475E+03 0.12078779121911E+02
 0.45295421707168E+01 0.32496228400408E+03 0.30115000000024E+03 0.32138201432469E+03 0.33789913136485E+03
 0.30115000000011E+03 0.30115000000011E+03 0.31862697253621E+03 0.33785458097758E+03 0.30115000000011E+03
 0.30115000000011E+03 0.32138201432469E+03 0.33789913136485E+03 0.30115000000011E+03 0.30115000000011E+03
 0.31862697253621E+03 0.33785458097758E+03 0.30115000000011E+03 0.30115000000011E+03 0.37789336658956E+03
 0.30479882243951E+03 0.21612928319159E+04 0.20906290749686E+04 0.86004656542776E+03 0.16336765640484E+04
 0.76932976579348E+03 0.12514194419189E+04 0.12431650396108E+04 0.11959794642011E+04 0.21270282011570E+04
 0.10993733505295E+04 0.12416480198057E+04 0.10589553646519E+04 0.21259454095469E+04 0.12514194419189E+04
 0.12431650396108E+04 0.11959794642011E+04 0.21270282011570E+04 0.10993733505295E+04 0.12416480198056E+04
 0.10589553646519E+04 0.21259454095469E+04 0.21492158017835E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49753163797538E+03 0.14691296893711E+01
 0.14691296893711E+01 0.86429500642521E+00 0.18126708160486E+00 0.30227035704306E+03 0.34177565463322E+03
 0.33700164796515E+03 0.33646163670278E+03 0.22999999892582E+00 0.00000000000000E+00 0.20965773864520E+00
 0.00000000000000E+00 -.20773195098919E+02 0.99581727285910E-03 0.37101366027000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21562548382122E+02 0.80859556432957E+01 0.30479607198567E+03 0.37793739689989E+03
 0.30212352825208E+03 0.30677190937374E+03 0.30115000000011E+03 0.30115000000011E+03 0.30211191631204E+03
 0.30677315132646E+03 0.30115000000011E+03 0.30115000000011E+03 0.30212352825208E+03 0.30677190937374E+03
 0.30115000000011E+03 0.30115000000011E+03 0.30211191631204E+03 0.30677315132646E+03 0.30115000000011E+03
 0.30115000000011E+03 0.30583838999629E+03 0.30115000000016E+03 0.11345891662739E+03 0.10730502986705E+03
 0.11567373205969E+03 0.43846607011366E+03 0.32221396939367E+03 0.82186941011758E+02 0.11032696815667E+03
 0.82186941011758E+02 0.35360824613668E+03 0.81377010891462E+02 0.11038411742092E+03 0.81377010891462E+02
 0.35365497005632E+03 0.82186941011758E+02 0.11032696815667E+03 0.82186941011758E+02 0.35360824613668E+03
 0.81377010891462E+02 0.11038411742092E+03 0.81377010891462E+02 0.35365497005632E+03 0.22722077247943E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36599854749052E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22327693149875E+00 0.00000000000000E+00 0.00000000000000E+00 0.22327693149875E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23055672873484E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23055672873484E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206148466830E+00 0.23898393009075E+00 0.34177565463322E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    220.00336097
 0.10604136558047E+00 0.30931597487728E+03 0.45151814931407E+03 0.44649171172547E+03 0.44429822700119E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18090713702059E+00 0.00000000000000E+00 -.27425629462832E+02
 0.32973779978374E-02 0.68869208795409E+00 0.24261701282798E+04 0.90981379810491E+03 0.11616221733817E+02
 0.43560831501813E+01 0.32594036453127E+03 0.30115000000034E+03 0.32219824532720E+03 0.33923315800853E+03
 0.30115000000013E+03 0.30115000000013E+03 0.31937893878478E+03 0.33918955080492E+03 0.30115000000012E+03
 0.30115000000013E+03 0.32219824532720E+03 0.33923315800853E+03 0.30115000000013E+03 0.30115000000013E+03
 0.31937893878478E+03 0.33918955080492E+03 0.30115000000012E+03 0.30115000000013E+03 0.37985052997301E+03
 0.30524124026453E+03 0.21753295913444E+04 0.21012438912871E+04 0.85755363861930E+03 0.16099691251001E+04
 0.74812771828767E+03 0.12606823346283E+04 0.12521907538652E+04 0.12030075034161E+04 0.21258123455945E+04
 0.11090876688796E+04 0.12507713731345E+04 0.10668802338425E+04 0.21248166845886E+04 0.12606823346283E+04
 0.12521907538652E+04 0.12030075034161E+04 0.21258123455945E+04 0.11090876688796E+04 0.12507713731345E+04
 0.10668802338425E+04 0.21248166845886E+04 0.21452221122980E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49773359833706E+03 0.14691301441471E+01
 0.14691301441471E+01 0.91430234941894E+00 0.15906229398398E+00 0.30259035472141E+03 0.34180612730515E+03
 0.33764762680680E+03 0.33717236608144E+03 0.22999999954476E+00 0.00000000000000E+00 0.20815678446110E+00
 0.00000000000000E+00 -.20850589177905E+02 0.11024492244716E-02 0.39930530455496E+00 0.72565700282786E+04
 0.27212137606045E+04 0.20034795202424E+02 0.75130482009088E+01 0.30523835625061E+03 0.37989426147547E+03
 0.30222759622642E+03 0.30704164870677E+03 0.30115000000012E+03 0.30115000000012E+03 0.30221558734521E+03
 0.30704285005083E+03 0.30115000000012E+03 0.30115000000012E+03 0.30222759622642E+03 0.30704164870677E+03
 0.30115000000012E+03 0.30115000000012E+03 0.30221558734521E+03 0.30704285005083E+03 0.30115000000012E+03
 0.30115000000012E+03 0.30606957188449E+03 0.30115000000017E+03 0.11927896695955E+03 0.11272996641605E+03
 0.12250324707916E+03 0.44340983308920E+03 0.32029406977465E+03 0.87373938248385E+02 0.11678182703619E+03
 0.87970867081387E+02 0.35749751389782E+03 0.86586115664931E+02 0.11682929918158E+03 0.87207775497226E+02
 0.35753493840292E+03 0.87373938248385E+02 0.11678182703619E+03 0.87970867081387E+02 0.35749751389782E+03
 0.86586115664931E+02 0.11682929918158E+03 0.87207775497226E+02 0.35753493840292E+03 0.23214280509069E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36615035408438E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22358398586206E+00 0.00000000000000E+00 0.00000000000000E+00 0.22358398586206E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23147943133251E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23147943133251E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205959914919E+00 0.23896050000064E+00 0.34180612730515E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    230.08986188
 0.10718309658407E+00 0.30969572830383E+03 0.45184028803339E+03 0.44676179000859E+03 0.44455039636384E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17907175093249E+00 0.00000000000000E+00 -.27464938990485E+02
 0.32622536482289E-02 0.71394799242345E+00 0.24522924525943E+04 0.91960966972288E+03 0.11205297983743E+02
 0.42019867439037E+01 0.32689025912598E+03 0.30115000000052E+03 0.32299261031679E+03 0.34051755943329E+03
 0.30115000000013E+03 0.30115000000014E+03 0.32011124496966E+03 0.34047486900688E+03 0.30115000000013E+03
 0.30115000000014E+03 0.32299261031678E+03 0.34051755943329E+03 0.30115000000013E+03 0.30115000000014E+03
 0.32011124496966E+03 0.34047486900688E+03 0.30115000000013E+03 0.30115000000014E+03 0.38170171185772E+03
 0.30570995029206E+03 0.21883813979060E+04 0.21109797922456E+04 0.85483166937482E+03 0.15877499197336E+04
 0.72864409201195E+03 0.12693083637031E+04 0.12603236798104E+04 0.12094313678262E+04 0.21243617022581E+04
 0.11181738934938E+04 0.12589930733290E+04 0.10742061891950E+04 0.21234446765375E+04 0.12693083637031E+04
 0.12603236798104E+04 0.12094313678262E+04 0.21243617022581E+04 0.11181738934938E+04 0.12589930733290E+04
 0.10742061891950E+04 0.21234446765375E+04 0.21413274076783E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49798585149008E+03 0.14691304608553E+01
 0.14691304608553E+01 0.96473485394130E+00 0.13825960694757E+00 0.30297588648359E+03 0.34180361139785E+03
 0.33822474074097E+03 0.33781322832026E+03 0.22999999990950E+00 0.00000000000000E+00 0.20663305740822E+00
 0.00000000000000E+00 -.20904234149655E+02 0.12683245728184E-02 0.42800477988038E+00 0.63075337113611E+04
 0.23653251417604E+04 0.18691380040746E+02 0.70092675152798E+01 0.30570693645933E+03 0.38174493040475E+03
 0.30233534995715E+03 0.30730345704180E+03 0.30115000000013E+03 0.30115000000013E+03 0.30232295967236E+03
 0.30730460404273E+03 0.30115000000013E+03 0.30115000000013E+03 0.30233534995715E+03 0.30730345704180E+03
 0.30115000000013E+03 0.30115000000013E+03 0.30232295967236E+03 0.30730460404273E+03 0.30115000000013E+03
 0.30115000000013E+03 0.30629389013171E+03 0.30115000000019E+03 0.12454373745808E+03 0.11772586331813E+03
 0.12891786313898E+03 0.44767159339458E+03 0.31810914093991E+03 0.92396583713453E+02 0.12282452664400E+03
 0.93653908404600E+02 0.36083353975141E+03 0.91641999154239E+02 0.12286186621734E+03 0.92929748206447E+02
 0.36086132101249E+03 0.92396583713453E+02 0.12282452664400E+03 0.93653908404600E+02 0.36083353975141E+03
 0.91641999154239E+02 0.12286186621734E+03 0.92929748206447E+02 0.36086132101248E+03 0.23655243807447E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36629468480994E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22376154349326E+00 0.00000000000000E+00 0.00000000000000E+00 0.22376154349326E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23227973688936E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23227973688936E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205865417308E+00 0.23896115924429E+00 0.34180361139785E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    240.01654807
 0.10795282821994E+00 0.31006112269747E+03 0.45218201326982E+03 0.44706789590767E+03 0.44484478752685E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17734788648106E+00 0.00000000000000E+00 -.27499106428379E+02
 0.32389927156621E-02 0.73757251315018E+00 0.24699036713840E+04 0.92621387676901E+03 0.10846391178315E+02
 0.40673966918682E+01 0.32779423846326E+03 0.30115000000085E+03 0.32375000009326E+03 0.34173073848279E+03
 0.30115000000013E+03 0.30115000000014E+03 0.32081004332498E+03 0.34168891787021E+03 0.30115000000013E+03
 0.30115000000014E+03 0.32375000009326E+03 0.34173073848279E+03 0.30115000000013E+03 0.30115000000014E+03
 0.32081004332498E+03 0.34168891787021E+03 0.30115000000013E+03 0.30115000000014E+03 0.38342117122438E+03
 0.30619410524611E+03 0.22004303302339E+04 0.21198699180754E+04 0.85215791546805E+03 0.15675886531989E+04
 0.71116994815346E+03 0.12772796087421E+04 0.12676946287770E+04 0.12152768052254E+04 0.21230214107793E+04
 0.11265981958870E+04 0.12664429433455E+04 0.10809312207036E+04 0.21221737560939E+04 0.12772796087421E+04
 0.12676946287770E+04 0.12152768052254E+04 0.21230214107793E+04 0.11265981958870E+04 0.12664429433455E+04
 0.10809312207036E+04 0.21221737560939E+04 0.21378478286277E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49827967705794E+03 0.14691307361350E+01
 0.14691307361350E+01 0.10143682848896E+01 0.11933734089417E+00 0.30342261692548E+03 0.34178463475277E+03
 0.33873262062020E+03 0.33838098750158E+03 0.23000000000000E+00 0.00000000000000E+00 0.20512593400297E+00
 0.00000000000000E+00 -.20946542350786E+02 0.14694312284362E-02 0.45636528606143E+00 0.54442833697726E+04
 0.20416062636647E+04 0.17529817109979E+02 0.65736814162420E+01 0.30619096569508E+03 0.38346363983093E+03
 0.30244486045160E+03 0.30755222507369E+03 0.30115000000012E+03 0.30115000000012E+03 0.30243222972011E+03
 0.30755330637838E+03 0.30115000000012E+03 0.30115000000012E+03 0.30244486045160E+03 0.30755222507369E+03
 0.30115000000012E+03 0.30115000000012E+03 0.30243222972011E+03 0.30755330637838E+03 0.30115000000012E+03
 0.30115000000012E+03 0.30650704211686E+03 0.30115000000023E+03 0.12915372448571E+03 0.12221871205221E+03
 0.13480771448545E+03 0.45135156923576E+03 0.31586981617789E+03 0.97155314666022E+02 0.12835533680968E+03
 0.99367743953313E+02 0.36370039053812E+03 0.96442189178781E+02 0.12838249092116E+03 0.98690335519855E+02
 0.36371856491278E+03 0.97155314666022E+02 0.12835533680968E+03 0.99367743953313E+02 0.36370039053812E+03
 0.96442189178780E+02 0.12838249092116E+03 0.98690335519855E+02 0.36371856491278E+03 0.24045357384430E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36644148032669E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22389147061349E+00 0.00000000000000E+00 0.00000000000000E+00 0.22389147061349E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23295619354333E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23295619354333E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205808961538E+00 0.23897374568003E+00 0.34178463475277E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    250.01747564
 0.10849715646399E+00 0.31041962292042E+03 0.45255211210127E+03 0.44741178846218E+03 0.44518025028827E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17568805092832E+00 0.00000000000000E+00 -.27538051883225E+02
 0.32227424865028E-02 0.76021672705877E+00 0.24823578158990E+04 0.93088418096214E+03 0.10523314885416E+02
 0.39462430820311E+01 0.32867733197928E+03 0.30115000000142E+03 0.32449112203444E+03 0.34290907636777E+03
 0.30115000000014E+03 0.30115000000016E+03 0.32149437330966E+03 0.34286809545393E+03 0.30115000000013E+03
 0.30115000000016E+03 0.32449112203444E+03 0.34290907636777E+03 0.30115000000014E+03 0.30115000000016E+03
 0.32149437330966E+03 0.34286809545393E+03 0.30115000000013E+03 0.30115000000016E+03 0.38506819727599E+03
 0.30670477321135E+03 0.22119278233313E+04 0.21282661829689E+04 0.84951813945584E+03 0.15488010953112E+04
 0.69503536515810E+03 0.12848972610601E+04 0.12746598777133E+04 0.12207859314805E+04 0.21218739400060E+04
 0.11346693982721E+04 0.12734801771426E+04 0.10873147822377E+04 0.21210891065743E+04 0.12848972610601E+04
 0.12746598777133E+04 0.12207859314805E+04 0.21218739400060E+04 0.11346693982721E+04 0.12734801771426E+04
 0.10873147822377E+04 0.21210891065743E+04 0.21347273664001E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49861429890703E+03 0.14691310499103E+01
 0.14691310499103E+01 0.10643729227294E+01 0.10181409641694E+00 0.30394450667056E+03 0.34176099674908E+03
 0.33919416223775E+03 0.33889896913959E+03 0.23000000000000E+00 0.00000000000000E+00 0.20360155460339E+00
 0.00000000000000E+00 -.20988703839018E+02 0.17223349531975E-02 0.48501806930068E+00 0.46448572533164E+04
 0.17418214699937E+04 0.16494230846976E+02 0.61853365676160E+01 0.30670151216578E+03 0.38510968124465E+03
 0.30255900719216E+03 0.30779485442787E+03 0.30115000000012E+03 0.30115000000012E+03 0.30254626030753E+03
 0.30779585912748E+03 0.30115000000012E+03 0.30115000000012E+03 0.30255900719216E+03 0.30779485442787E+03
 0.30115000000012E+03 0.30115000000012E+03 0.30254626030753E+03 0.30779585912748E+03 0.30115000000012E+03
 0.30115000000012E+03 0.30671490764115E+03 0.30115000000029E+03 0.13325353629366E+03 0.12636461160690E+03
 0.14035863968157E+03 0.45470016049567E+03 0.31363972761569E+03 0.10178169695850E+03 0.13355291819267E+03
 0.10534041619216E+03 0.36629864134178E+03 0.10111781013065E+03 0.13356978277640E+03 0.10471738085488E+03
 0.36630718995457E+03 0.10178169695850E+03 0.13355291819266E+03 0.10534041619216E+03 0.36629864134178E+03
 0.10111781013065E+03 0.13356978277640E+03 0.10471738085488E+03 0.36630718995457E+03 0.24402717402952E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36660330699115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22403922997265E+00 0.00000000000000E+00 0.00000000000000E+00 0.22403922997265E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23354569201616E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23354569201616E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205746016566E+00 0.23898952662056E+00 0.34176099674908E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    260.00164734
 0.10889603445816E+00 0.31076805217137E+03 0.45294520557900E+03 0.44778436284678E+03 0.44554613885857E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17410327918557E+00 0.00000000000000E+00 -.27582213953281E+02
 0.32109375306561E-02 0.78173368682526E+00 0.24914841611277E+04 0.93430656042288E+03 0.10233664142694E+02
 0.38376240535104E+01 0.32953422957798E+03 0.30115000000240E+03 0.32521138730348E+03 0.34404704514847E+03
 0.30115000000017E+03 0.30115000000021E+03 0.32215999386413E+03 0.34400686607070E+03 0.30115000000015E+03
 0.30115000000021E+03 0.32521138730348E+03 0.34404704514847E+03 0.30115000000017E+03 0.30115000000021E+03
 0.32215999386413E+03 0.34400686607070E+03 0.30115000000015E+03 0.30115000000021E+03 0.38663818331676E+03
 0.30723783001689E+03 0.22228512261892E+04 0.21361610312456E+04 0.84696376943760E+03 0.15314107594780E+04
 0.68021217119323E+03 0.12921512333566E+04 0.12812475955707E+04 0.12259654022433E+04 0.21209536429694E+04
 0.11423712722787E+04 0.12801330646500E+04 0.10933529504892E+04 0.21202252756913E+04 0.12921512333566E+04
 0.12812475955707E+04 0.12259654022433E+04 0.21209536429694E+04 0.11423712722787E+04 0.12801330646500E+04
 0.10933529504892E+04 0.21202252756913E+04 0.21319735410436E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49897930569992E+03 0.14691314057149E+01
 0.14691314057149E+01 0.11142937812694E+01 0.85842143910642E-01 0.30454016386855E+03 0.34174161629887E+03
 0.33961264801007E+03 0.33936916668535E+03 0.23000000000000E+00 0.00000000000000E+00 0.20207482133947E+00
 0.00000000000000E+00 -.21014074278153E+02 0.20427954598794E-02 0.51367649589177E+00 0.39162021637116E+04
 0.14685758113919E+04 0.15574004385993E+02 0.58402516447474E+01 0.30723445576024E+03 0.38667848330395E+03
 0.30267756627035E+03 0.30803005975026E+03 0.30115000000012E+03 0.30115000000012E+03 0.30266482533686E+03
 0.30803097890354E+03 0.30115000000012E+03 0.30115000000012E+03 0.30267756627035E+03 0.30803005975026E+03
 0.30115000000012E+03 0.30115000000012E+03 0.30266482533686E+03 0.30803097890354E+03 0.30115000000012E+03
 0.30115000000012E+03 0.30691639586982E+03 0.30115000000041E+03 0.13682940893219E+03 0.13015750187612E+03
 0.14555913319621E+03 0.45780599072957E+03 0.31151906186738E+03 0.10624997404242E+03 0.13840956895995E+03
 0.11162896100594E+03 0.36870197196743E+03 0.10564142539259E+03 0.13841625559422E+03 0.11106621968607E+03
 0.36870107427225E+03 0.10624997404242E+03 0.13840956895995E+03 0.11162896100594E+03 0.36870197196743E+03
 0.10564142539259E+03 0.13841625559422E+03 0.11106621968607E+03 0.36870107427225E+03 0.24730432402277E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36678480844027E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22421121185345E+00 0.00000000000000E+00 0.00000000000000E+00 0.22421121185345E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23405826520171E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23405826520171E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205738052041E+00 0.23900293800949E+00 0.34174161629887E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    270.04938281
 0.10920408278588E+00 0.31110993370381E+03 0.45336096641839E+03 0.44818283523408E+03 0.44593873706936E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17257736047046E+00 0.00000000000000E+00 -.27626906478545E+02
 0.32018796412504E-02 0.80235033530056E+00 0.24985323923281E+04 0.93694964712303E+03 0.99707068695911E+01
 0.37390150760966E+01 0.33037404377107E+03 0.30115000000400E+03 0.32591834160019E+03 0.34515808364740E+03
 0.30115000000021E+03 0.30115000000029E+03 0.32281384629007E+03 0.34511867463768E+03 0.30115000000019E+03
 0.30115000000029E+03 0.32591834160019E+03 0.34515808364740E+03 0.30115000000021E+03 0.30115000000029E+03
 0.32281384629007E+03 0.34511867463768E+03 0.30115000000019E+03 0.30115000000029E+03 0.38815265312438E+03
 0.30779828369943E+03 0.22333435826101E+04 0.21436665202596E+04 0.84446672655891E+03 0.15151022182348E+04
 0.66641315804309E+03 0.12991394782308E+04 0.12875602191005E+04 0.12308966864701E+04 0.21202300819563E+04
 0.11498045040333E+04 0.12865052731297E+04 0.10991330450768E+04 0.21195529789626E+04 0.12991394782308E+04
 0.12875602191005E+04 0.12308966864701E+04 0.21202300819563E+04 0.11498045040333E+04 0.12865052731297E+04
 0.10991330450768E+04 0.21195529789626E+04 0.21295110593297E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49937108104814E+03 0.14691317657936E+01
 0.14691317657936E+01 0.11645324586112E+01 0.71287773981560E-01 0.30524572199801E+03 0.34173219741960E+03
 0.33999817101078E+03 0.33980186599498E+03 0.23000000000000E+00 0.00000000000000E+00 0.20053424483894E+00
 0.00000000000000E+00 -.21034672857692E+02 0.24598595207060E-02 0.54255047588400E+00 0.32522182395619E+04
 0.12195818398357E+04 0.14745171842242E+02 0.55294394408409E+01 0.30779480487191E+03 0.38819159389624E+03
 0.30277475346709E+03 0.30826058014445E+03 0.30115000000012E+03 0.30115000000013E+03 0.30276236408666E+03
 0.30826140559811E+03 0.30115000000012E+03 0.30115000000013E+03 0.30277475346709E+03 0.30826058014445E+03
 0.30115000000012E+03 0.30115000000013E+03 0.30276236408666E+03 0.30826140559811E+03 0.30115000000012E+03
 0.30115000000013E+03 0.30711384122477E+03 0.30115000000062E+03 0.13992534083691E+03 0.13373798777927E+03
 0.15048556430015E+03 0.46077955897561E+03 0.30954156685396E+03 0.11076223658631E+03 0.14299944806200E+03
 0.11076223658631E+03 0.37099896233593E+03 0.11021197713875E+03 0.14299610794544E+03 0.11021197713875E+03
 0.37098883077557E+03 0.11076223658631E+03 0.14299944806200E+03 0.11076223658631E+03 0.37099896233593E+03
 0.11021197713875E+03 0.14299610794544E+03 0.11021197713875E+03 0.37098883077557E+03 0.25026926439614E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36699141996613E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22437813360605E+00 0.00000000000000E+00 0.00000000000000E+00 0.22437813360605E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23448977527316E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23448977527316E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205737584121E+00 0.23900947201310E+00 0.34173219741960E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    280.02022507
 0.10945978224271E+00 0.31144091642251E+03 0.45378990404405E+03 0.44859607431487E+03 0.44634628794357E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17112815839688E+00 0.00000000000000E+00 -.27679127921293E+02
 0.31943996801197E-02 0.82183369437207E+00 0.25043829204554E+04 0.93914359517078E+03 0.97343295301520E+01
 0.36503735738070E+01 0.33118723272740E+03 0.30115000000655E+03 0.32660386353057E+03 0.34623038332524E+03
 0.30115000000030E+03 0.30115000000043E+03 0.32344841183657E+03 0.34619170357716E+03 0.30115000000025E+03
 0.30115000000043E+03 0.32660386353057E+03 0.34623038332524E+03 0.30115000000030E+03 0.30115000000043E+03
 0.32344841183657E+03 0.34619170357716E+03 0.30115000000025E+03 0.30115000000043E+03 0.38959697824608E+03
 0.30837857790437E+03 0.22432944924791E+04 0.21507053945721E+04 0.84205911383387E+03 0.14999626459046E+04
 0.65369323650152E+03 0.13057922909384E+04 0.12935414392639E+04 0.12355366529738E+04 0.21196767076285E+04
 0.11568924736989E+04 0.12925403143308E+04 0.11045993338947E+04 0.21190455775273E+04 0.13057922909384E+04
 0.12935414392639E+04 0.12355366529738E+04 0.21196767076285E+04 0.11568924736989E+04 0.12925403143308E+04
 0.11045993338947E+04 0.21190455775273E+04 0.21273182645307E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.49977796449107E+03 0.14691321865315E+01
 0.14691321865315E+01 0.12143866698764E+01 0.58318367110622E-01 0.30603295964772E+03 0.34173744962560E+03
 0.34034929792291E+03 0.34019434342679E+03 0.23000000000000E+00 0.00000000000000E+00 0.19900179115188E+00
 0.00000000000000E+00 -.21062561758666E+02 0.30069065580106E-02 0.57122228410817E+00 0.26605416050218E+04
 0.99770310188318E+03 0.14005055864531E+02 0.52518959491992E+01 0.30837500110333E+03 0.38963443994450E+03
 0.30287877889755E+03 0.30848409187212E+03 0.30115000000012E+03 0.30115000000014E+03 0.30286633855448E+03
 0.30848481785712E+03 0.30115000000012E+03 0.30115000000014E+03 0.30287877889755E+03 0.30848409187212E+03
 0.30115000000012E+03 0.30115000000014E+03 0.30286633855448E+03 0.30848481785712E+03 0.30115000000012E+03
 0.30115000000014E+03 0.30730527022049E+03 0.30115000000096E+03 0.14253189252908E+03 0.13701461642133E+03
 0.15510977315153E+03 0.46365988897543E+03 0.30777456695814E+03 0.11511388636029E+03 0.14729848030132E+03
 0.11511388636029E+03 0.37322504029806E+03 0.11462824815952E+03 0.14728544544880E+03 0.11462824815952E+03
 0.37320604809503E+03 0.11511388636029E+03 0.14729848030132E+03 0.11511388636029E+03 0.37322504029806E+03
 0.11462824815952E+03 0.14728544544880E+03 0.11462824815952E+03 0.37320604809503E+03 0.25311441299153E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36722302911646E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22458938431305E+00 0.00000000000000E+00 0.00000000000000E+00 0.22458938431305E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23488684945216E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23488684945216E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205695099142E+00 0.23900529525493E+00 0.34173744962560E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    290.01198890
 0.10968887038992E+00 0.31176530640001E+03 0.45423263543013E+03 0.44902360863387E+03 0.44676796172664E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16973788352423E+00 0.00000000000000E+00 -.27733381863640E+02
 0.31877277507194E-02 0.84043242322317E+00 0.25096246058638E+04 0.94110922719891E+03 0.95189092887670E+01
 0.35695909832876E+01 0.33198374360391E+03 0.30115000001051E+03 0.32727624900829E+03 0.34727786389506E+03
 0.30115000000044E+03 0.30115000000067E+03 0.32407133173361E+03 0.34723988108287E+03 0.30115000000036E+03
 0.30115000000066E+03 0.32727624900829E+03 0.34727786389506E+03 0.30115000000044E+03 0.30115000000067E+03
 0.32407133173361E+03 0.34723988108287E+03 0.30115000000036E+03 0.30115000000066E+03 0.39099202175023E+03
 0.30898294611703E+03 0.22528376092654E+04 0.21573793246798E+04 0.83969804330351E+03 0.14856886916656E+04
 0.64179215814559E+03 0.13122000455756E+04 0.12992714118159E+04 0.12399565230211E+04 0.21192444365309E+04
 0.11637298657807E+04 0.12983194848994E+04 0.11098314248752E+04 0.21186550295148E+04 0.13122000455756E+04
 0.12992714118159E+04 0.12399565230211E+04 0.21192444365309E+04 0.11637298657807E+04 0.12983194848994E+04
 0.11098314248752E+04 0.21186550295148E+04 0.21253152186707E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50019913794860E+03 0.14691326236451E+01
 0.14691326236451E+01 0.12643454890706E+01 0.46756085839816E-01 0.30688867617368E+03 0.34175962344773E+03
 0.34067267077836E+03 0.34055343535945E+03 0.23000000000000E+00 0.00000000000000E+00 0.19746293260100E+00
 0.00000000000000E+00 -.21092108577807E+02 0.37504819166319E-02 0.59996014689657E+00 0.21330592115438E+04
 0.79989720432891E+03 0.13334219016683E+02 0.50003321312560E+01 0.30897928540412E+03 0.39102790663857E+03
 0.30298598543151E+03 0.30870360816268E+03 0.30115000000012E+03 0.30115000000017E+03 0.30297371380063E+03
 0.30870422931782E+03 0.30115000000012E+03 0.30115000000017E+03 0.30298598543151E+03 0.30870360816268E+03
 0.30115000000012E+03 0.30115000000017E+03 0.30297371380063E+03 0.30870422931782E+03 0.30115000000012E+03
 0.30115000000017E+03 0.30749325351639E+03 0.30115000000149E+03 0.14470742197578E+03 0.13997394510345E+03
 0.15950767755127E+03 0.46651754229613E+03 0.30621232635710E+03 0.11938504974032E+03 0.15137983485115E+03
 0.11938504974032E+03 0.37543680459063E+03 0.11896591489613E+03 0.15135744550486E+03 0.11896591489613E+03
 0.37540933033945E+03 0.11938504974032E+03 0.15137983485115E+03 0.11938504974032E+03 0.37543680459063E+03
 0.11896591489613E+03 0.15135744550486E+03 0.11896591489613E+03 0.37540933033944E+03 0.25578984087580E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36748036103103E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22480941907857E+00 0.00000000000000E+00 0.00000000000000E+00 0.22480941907857E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23523903011100E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23523903011100E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205632825791E+00 0.23898907720458E+00 0.34175962344773E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    300.00764811
 0.10990107569333E+00 0.31208311195941E+03 0.45468537432689E+03 0.44946132698339E+03 0.44719957436228E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16840650482966E+00 0.00000000000000E+00 -.27787252569751E+02
 0.31815722980557E-02 0.85815501440137E+00 0.25144800276545E+04 0.94293001037045E+03 0.93223250645231E+01
 0.34958718991961E+01 0.33276357516974E+03 0.30115000001653E+03 0.32793542353471E+03 0.34830110839960E+03
 0.30115000000066E+03 0.30115000000105E+03 0.32468250328232E+03 0.34826378980051E+03 0.30115000000053E+03
 0.30115000000104E+03 0.32793542353471E+03 0.34830110839960E+03 0.30115000000066E+03 0.30115000000105E+03
 0.32468250328232E+03 0.34826378980051E+03 0.30115000000053E+03 0.30115000000104E+03 0.39234019681069E+03
 0.30960920490700E+03 0.22619849728298E+04 0.21637019701189E+04 0.83737094772130E+03 0.14721858392031E+04
 0.63062803674322E+03 0.13183703030288E+04 0.13047529867674E+04 0.12441661580299E+04 0.21188962967037E+04
 0.11703237373882E+04 0.13038460327707E+04 0.11148384206876E+04 0.21183447585185E+04 0.13183703030288E+04
 0.13047529867674E+04 0.12441661580299E+04 0.21188962967037E+04 0.11703237373882E+04 0.13038460327707E+04
 0.11148384206876E+04 0.21183447585185E+04 0.21234595150339E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50063041135206E+03 0.14691330576714E+01
 0.14691330576714E+01 0.13143237851025E+01 0.36591472100370E-01 0.30780052970283E+03 0.34179956525056E+03
 0.34097018207677E+03 0.34088104505404E+03 0.23000000000000E+00 0.00000000000000E+00 0.19592073633336E+00
 0.00000000000000E+00 -.21121486932274E+02 0.47923141972003E-02 0.62870411968784E+00 0.16693396281641E+04
 0.62600236056155E+03 0.12724586573366E+02 0.47717199650124E+01 0.30960547981142E+03 0.39237445218804E+03
 0.30309487176353E+03 0.30891935958376E+03 0.30115000000012E+03 0.30115000000021E+03 0.30308288927933E+03
 0.30891987202220E+03 0.30115000000012E+03 0.30115000000021E+03 0.30309487176353E+03 0.30891935958376E+03
 0.30115000000012E+03 0.30115000000021E+03 0.30308288927933E+03 0.30891987202220E+03 0.30115000000012E+03
 0.30115000000021E+03 0.30767797398803E+03 0.30115000000231E+03 0.14647022798919E+03 0.14258625063763E+03
 0.16369406171349E+03 0.46937245659127E+03 0.30485992456921E+03 0.12358331067593E+03 0.15525890488706E+03
 0.12358331067593E+03 0.37765113265922E+03 0.12323165053015E+03 0.15522758554373E+03 0.12323165053015E+03
 0.37761562747287E+03 0.12358331067593E+03 0.15525890488706E+03 0.12358331067593E+03 0.37765113265922E+03
 0.12323165053015E+03 0.15522758554373E+03 0.12323165053015E+03 0.37761562747287E+03 0.25830090030037E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36776197308859E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22502386974363E+00 0.00000000000000E+00 0.00000000000000E+00 0.22502386974363E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23554947035229E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23554947035229E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205557890132E+00 0.23896030858999E+00 0.34179956525056E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    310.01604397
 0.11010055381620E+00 0.31239533761789E+03 0.45514613137441E+03 0.44990715089098E+03 0.44763906565924E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16713059197886E+00 0.00000000000000E+00 -.27840800954091E+02
 0.31758076327359E-02 0.87505451086953E+00 0.25190442637447E+04 0.94464159890428E+03 0.91422875953756E+01
 0.34283578482659E+01 0.33352868430513E+03 0.30115000002553E+03 0.32858297611582E+03 0.34930304746628E+03
 0.30115000000102E+03 0.30115000000166E+03 0.32528338078634E+03 0.34926636240507E+03 0.30115000000080E+03
 0.30115000000165E+03 0.32858297611582E+03 0.34930304746628E+03 0.30115000000102E+03 0.30115000000166E+03
 0.32528338078634E+03 0.34926636240507E+03 0.30115000000080E+03 0.30115000000165E+03 0.39364651016048E+03
 0.31025649908478E+03 0.22707741400261E+04 0.21697065619119E+04 0.83506827225462E+03 0.14593535439200E+04
 0.62010993030410E+03 0.13243267485879E+04 0.13100047113129E+04 0.12481870171420E+04 0.21186042502050E+04
 0.11766984541252E+04 0.13091389709034E+04 0.11196431720837E+04 0.21180871655834E+04 0.13243267485879E+04
 0.13100047113129E+04 0.12481870171420E+04 0.21186042502050E+04 0.11766984541252E+04 0.13091389709034E+04
 0.11196431720837E+04 0.21180871655834E+04 0.21217185259530E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50106970226570E+03 0.14691334891012E+01
 0.14691334891012E+01 0.13643657643723E+01 0.27965769645131E-01 0.30875284660380E+03 0.34186165061280E+03
 0.34124437515537E+03 0.34117954593716E+03 0.23000000000000E+00 0.00000000000000E+00 0.19436949409151E+00
 0.00000000000000E+00 -.21151586550418E+02 0.62704446616160E-02 0.65754909333090E+00 0.12758265851498E+04
 0.47843496943116E+03 0.12166391956340E+02 0.45623969836277E+01 0.31025272960816E+03 0.39367910749219E+03
 0.30320518476849E+03 0.30913212466499E+03 0.30115000000012E+03 0.30115000000028E+03 0.30319358146154E+03
 0.30913252557992E+03 0.30115000000012E+03 0.30115000000028E+03 0.30320518476849E+03 0.30913212466499E+03
 0.30115000000012E+03 0.30115000000028E+03 0.30319358146154E+03 0.30913252557992E+03 0.30115000000012E+03
 0.30115000000028E+03 0.30786023109538E+03 0.30115000000355E+03 0.14785764815208E+03 0.14482929140268E+03
 0.16771645955311E+03 0.47236318271361E+03 0.30380814086274E+03 0.12772548784480E+03 0.15898432756308E+03
 0.12772548784480E+03 0.37991680564462E+03 0.12743923976834E+03 0.15894457247566E+03 0.12743923976834E+03
 0.37987378227119E+03 0.12772548784480E+03 0.15898432756308E+03 0.12772548784480E+03 0.37991680564462E+03
 0.12743923976834E+03 0.15894457247566E+03 0.12743923976834E+03 0.37987378227119E+03 0.26068498365918E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36818267703542E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22523381355029E+00 0.00000000000000E+00 0.00000000000000E+00 0.22523381355029E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23582734473815E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23582734473815E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205465371361E+00 0.23891588754591E+00 0.34186165061280E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    320.01724127
 0.11029239950420E+00 0.31270169248367E+03 0.45561208091193E+03 0.45035810436065E+03 0.44808343644129E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16591041725695E+00 0.00000000000000E+00 -.27894514539479E+02
 0.31702831893053E-02 0.89113425261713E+00 0.25234338771336E+04 0.94628770392508E+03 0.89773229751916E+01
 0.33664961156969E+01 0.33427868586711E+03 0.30115000003872E+03 0.32921851844388E+03 0.35028346966470E+03
 0.30115000000160E+03 0.30115000000263E+03 0.32587358329089E+03 0.35024738771898E+03 0.30115000000125E+03
 0.30115000000262E+03 0.32921851844388E+03 0.35028346966470E+03 0.30115000000160E+03 0.30115000000263E+03
 0.32587358329089E+03 0.35024738771898E+03 0.30115000000125E+03 0.30115000000262E+03 0.39491181555722E+03
 0.31092182270531E+03 0.22792139134405E+04 0.21754037319494E+04 0.83278894633476E+03 0.14471415469227E+04
 0.61018865585623E+03 0.13300740757541E+04 0.13150302208422E+04 0.12520255950774E+04 0.21183483227096E+04
 0.11828580940809E+04 0.13142022312361E+04 0.11242512445485E+04 0.21178625662758E+04 0.13300740757541E+04
 0.13150302208422E+04 0.12520255950774E+04 0.21183483227096E+04 0.11828580940809E+04 0.13142022312361E+04
 0.11242512445485E+04 0.21178625662758E+04 0.21200703993781E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50151400565014E+03 0.14691339218623E+01
 0.14691339218623E+01 0.14143717509074E+01 0.21320265576549E-01 0.30971198255440E+03 0.34195384453911E+03
 0.34149557449898E+03 0.34144861490877E+03 0.23000000000000E+00 0.00000000000000E+00 0.19280026871000E+00
 0.00000000000000E+00 -.21184099681326E+02 0.82249347872229E-02 0.68663747788540E+00 0.97265208867403E+03
 0.36474453325276E+03 0.11650980696010E+02 0.43691177610039E+01 0.31091802845433E+03 0.39494275195746E+03
 0.30331658445401E+03 0.30934260763299E+03 0.30115000000013E+03 0.30115000000041E+03 0.30330540517893E+03
 0.30934289585370E+03 0.30115000000013E+03 0.30115000000041E+03 0.30331658445401E+03 0.30934260763299E+03
 0.30115000000013E+03 0.30115000000041E+03 0.30330540517893E+03 0.30934289585371E+03 0.30115000000013E+03
 0.30115000000041E+03 0.30804058759553E+03 0.30115000000541E+03 0.14891365208974E+03 0.14665378647531E+03
 0.17163560300082E+03 0.47556341257725E+03 0.30306963156142E+03 0.13181416734814E+03 0.16261779590986E+03
 0.13181416734814E+03 0.38238974966687E+03 0.13158410840624E+03 0.16257023748943E+03 0.13158410840624E+03
 0.38233984555552E+03 0.13181416734814E+03 0.16261779590986E+03 0.13181416734814E+03 0.38238974966687E+03
 0.13158410840624E+03 0.16257023748943E+03 0.13158410840624E+03 0.38233984555552E+03 0.26297428931100E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36863228482236E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22544338116205E+00 0.00000000000000E+00 0.00000000000000E+00 0.22544338116205E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23608130577948E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23608130577948E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205345194234E+00 0.23885015995270E+00 0.34195384453911E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    330.01823316
 0.11047371254132E+00 0.31300330659998E+03 0.45608203373292E+03 0.45081322100890E+03 0.44853185677230E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16474286228235E+00 0.00000000000000E+00 -.27942992678993E+02
 0.31650796609734E-02 0.90644195285699E+00 0.25275825119484E+04 0.94784344198065E+03 0.88257168313812E+01
 0.33096438117679E+01 0.33501514635744E+03 0.30115000005771E+03 0.32984331871793E+03 0.35124463534335E+03
 0.30115000000246E+03 0.30115000000408E+03 0.32645426935014E+03 0.35120912809633E+03 0.30115000000192E+03
 0.30115000000407E+03 0.32984331871793E+03 0.35124463534335E+03 0.30115000000246E+03 0.30115000000408E+03
 0.32645426935014E+03 0.35120912809633E+03 0.30115000000192E+03 0.30115000000407E+03 0.39614011419986E+03
 0.31160305850851E+03 0.22873409157882E+04 0.21808288090577E+04 0.83052811713093E+03 0.14354779322032E+04
 0.60079717448665E+03 0.13356340760464E+04 0.13198478962523E+04 0.12557031548737E+04 0.21181137977304E+04
 0.11888251860616E+04 0.13190545656781E+04 0.11286852744199E+04 0.21176565904673E+04 0.13356340760464E+04
 0.13198478962523E+04 0.12557031548737E+04 0.21181137977304E+04 0.11888251860616E+04 0.13190545656781E+04
 0.11286852744199E+04 0.21176565904673E+04 0.21184988955131E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50196245299008E+03 0.14691343124425E+01
 0.14691343124425E+01 0.14643767103373E+01 0.16245173308127E-01 0.31064858471739E+03 0.34206409972701E+03
 0.34172386606975E+03 0.34168986852385E+03 0.23000000000000E+00 0.00000000000000E+00 0.19122204780511E+00
 0.00000000000000E+00 -.21212475267362E+02 0.10794454235003E-01 0.71582300130553E+00 0.74112130412841E+03
 0.27792048904815E+03 0.11175947106211E+02 0.41909801648292E+01 0.31159926168798E+03 0.39616940413052E+03
 0.30342896850178E+03 0.30955133111446E+03 0.30115000000015E+03 0.30115000000060E+03 0.30341822948179E+03
 0.30955150649994E+03 0.30115000000015E+03 0.30115000000060E+03 0.30342896850178E+03 0.30955133111446E+03
 0.30115000000015E+03 0.30115000000060E+03 0.30341822948179E+03 0.30955150649994E+03 0.30115000000015E+03
 0.30115000000060E+03 0.30821923017554E+03 0.30115000000812E+03 0.14961700864133E+03 0.14797469371107E+03
 0.17540122098537E+03 0.47876614315531E+03 0.30248791606502E+03 0.13582496310059E+03 0.16610597386492E+03
 0.13582496310059E+03 0.38492386232920E+03 0.13564243469044E+03 0.16605122786542E+03 0.13564243469044E+03
 0.38486769088996E+03 0.13582496310059E+03 0.16610597386492E+03 0.13582496310059E+03 0.38492386232920E+03
 0.13564243469044E+03 0.16605122786542E+03 0.13564243469044E+03 0.38486769088996E+03 0.26507872380094E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36903127544183E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22561784276256E+00 0.00000000000000E+00 0.00000000000000E+00 0.22561784276256E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23629266991990E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23629266991990E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205228457397E+00 0.23877190493352E+00 0.34206409972701E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    340.05745274
 0.11063575355305E+00 0.31330223930485E+03 0.45655674571383E+03 0.45127372229168E+03 0.44898577409857E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16362155058981E+00 0.00000000000000E+00 -.27984777003147E+02
 0.31604436134281E-02 0.92106661235246E+00 0.25312902169839E+04 0.94923383136898E+03 0.86855824461681E+01
 0.32570934173130E+01 0.33574176721301E+03 0.30115000008477E+03 0.33046046267467E+03 0.35219150980562E+03
 0.30115000000373E+03 0.30115000000626E+03 0.32702829494556E+03 0.35215655246213E+03 0.30115000000291E+03
 0.30115000000623E+03 0.33046046267467E+03 0.35219150980562E+03 0.30115000000373E+03 0.30115000000626E+03
 0.32702829494556E+03 0.35215655246213E+03 0.30115000000291E+03 0.30115000000623E+03 0.39733875354072E+03
 0.31230047183510E+03 0.22952196818211E+04 0.21860374086492E+04 0.82827682642226E+03 0.14242701043015E+04
 0.59185189374712E+03 0.13410463335492E+04 0.13244919171702E+04 0.12592526105202E+04 0.21178929159924E+04
 0.11946414439676E+04 0.13237305765327E+04 0.11329821130956E+04 0.21174618604537E+04 0.13410463335492E+04
 0.13244919171702E+04 0.12592526105202E+04 0.21178929159924E+04 0.11946414439676E+04 0.13237305765327E+04
 0.11329821130956E+04 0.21174618604537E+04 0.21169930486105E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50241643724839E+03 0.14691346490921E+01
 0.14691346490921E+01 0.15145728082246E+01 0.12359310074805E-01 0.31155360051874E+03 0.34218449786304E+03
 0.34193211335761E+03 0.34190752413769E+03 0.23000000000000E+00 0.00000000000000E+00 0.18963874845546E+00
 0.00000000000000E+00 -.21234314828845E+02 0.14188313645652E-01 0.74504858096766E+00 0.56384431580785E+03
 0.21144161842794E+03 0.10737554844557E+02 0.40265830667091E+01 0.31229669641475E+03 0.39736641964917E+03
 0.30354257807864E+03 0.30975894094420E+03 0.30115000000017E+03 0.30115000000087E+03 0.30353228133277E+03
 0.30975900369444E+03 0.30115000000017E+03 0.30115000000087E+03 0.30354257807864E+03 0.30975894094420E+03
 0.30115000000017E+03 0.30115000000087E+03 0.30353228133277E+03 0.30975900369444E+03 0.30115000000017E+03
 0.30115000000087E+03 0.30839668902016E+03 0.30115000001203E+03 0.14996180826048E+03 0.14878155574964E+03
 0.17899106409911E+03 0.48187359378489E+03 0.30198757436528E+03 0.13975536235398E+03 0.16942473081010E+03
 0.13975536235398E+03 0.38741443910183E+03 0.13961291078252E+03 0.16936335330261E+03 0.13961291078252E+03
 0.38735255218181E+03 0.13975536235398E+03 0.16942473081010E+03 0.13975536235398E+03 0.38741443910183E+03
 0.13961291078252E+03 0.16936335330261E+03 0.13961291078252E+03 0.38735255218181E+03 0.26696188246608E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36938845901722E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22574737698453E+00 0.00000000000000E+00 0.00000000000000E+00 0.22574737698453E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23645346456297E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23645346456297E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205128710886E+00 0.23868681521280E+00 0.34218449786304E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    351.14273572
 0.11079323191904E+00 0.31362346917948E+03 0.45708148198960E+03 0.45178342302829E+03 0.44948825159271E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16243955528230E+00 0.00000000000000E+00 -.28028907627111E+02
 0.31559510498830E-02 0.93640003713281E+00 0.25348935625274E+04 0.95058508594777E+03 0.85433572007274E+01
 0.32037589502728E+01 0.33652854504917E+03 0.30115000012851E+03 0.33112928621278E+03 0.35321708826771E+03
 0.30115000000587E+03 0.30115000000994E+03 0.32765058613573E+03 0.35318269996486E+03 0.30115000000457E+03
 0.30115000000989E+03 0.33112928621278E+03 0.35321708826771E+03 0.30115000000587E+03 0.30115000000994E+03
 0.32765058613573E+03 0.35318269996486E+03 0.30115000000457E+03 0.30115000000989E+03 0.39863247079578E+03
 0.31308478831726E+03 0.23036113468861E+04 0.21915068956894E+04 0.82568903740713E+03 0.14121938331972E+04
 0.58237635060300E+03 0.13468352131069E+04 0.13293905413022E+04 0.12629951780692E+04 0.21176250903164E+04
 0.12008724224272E+04 0.13286616156783E+04 0.11375402001571E+04 0.21172204050468E+04 0.13468352131069E+04
 0.13293905413022E+04 0.12629951780692E+04 0.21176250903164E+04 0.12008724224272E+04 0.13286616156783E+04
 0.11375402001571E+04 0.21172204050468E+04 0.21153202845575E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50291908581556E+03 0.14691350046456E+01
 0.14691350046456E+01 0.15699992231472E+01 0.91308185547634E-02 0.31252127060319E+03 0.34232298025975E+03
 0.34214157092408E+03 0.34212438719223E+03 0.23000000000000E+00 0.00000000000000E+00 0.18790140092892E+00
 0.00000000000000E+00 -.21256908262150E+02 0.19205042291488E-01 0.77706682357974E+00 0.41655727066770E+03
 0.15620897650039E+03 0.10295124894338E+02 0.38606718353768E+01 0.31308106218692E+03 0.39865843390291E+03
 0.30366745040668E+03 0.30998485834107E+03 0.30115000000018E+03 0.30115000000132E+03 0.30365761882114E+03
 0.30998479984521E+03 0.30115000000017E+03 0.30115000000132E+03 0.30366745040668E+03 0.30998485834107E+03
 0.30115000000018E+03 0.30115000000132E+03 0.30365761882114E+03 0.30998479984521E+03 0.30115000000017E+03
 0.30115000000132E+03 0.30858946220666E+03 0.30115000001845E+03 0.14992484871187E+03 0.14911751192501E+03
 0.18271908711880E+03 0.48512832437785E+03 0.30149564182346E+03 0.14398098095944E+03 0.17286170287085E+03
 0.14398098095944E+03 0.39004129324741E+03 0.14387517945792E+03 0.17279361274367E+03 0.14387517945792E+03
 0.38997367747781E+03 0.14398098095944E+03 0.17286170287085E+03 0.14398098095944E+03 0.39004129324741E+03
 0.14387517945792E+03 0.17279361274367E+03 0.14387517945792E+03 0.38997367747781E+03 0.26877390048819E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36974241094836E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22587725266280E+00 0.00000000000000E+00 0.00000000000000E+00 0.22587725266280E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23659133410321E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23659133410321E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205018044642E+00 0.23858906642821E+00 0.34232298025975E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    360.12183589
 0.11088138201748E+00 0.31388586437164E+03 0.45750895072361E+03 0.45220057528884E+03 0.44990032571621E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16152342487601E+00 0.00000000000000E+00 -.28038505192825E+02
 0.31534417580360E-02 0.94821756905628E+00 0.25369106563054E+04 0.95134149611452E+03 0.84368822737192E+01
 0.31638308526447E+01 0.33715693610242E+03 0.30115000017735E+03 0.33166413939337E+03 0.35403332367385E+03
 0.30115000000837E+03 0.30115000001426E+03 0.32814891485711E+03 0.35399938183267E+03 0.30115000000652E+03
 0.30115000001419E+03 0.33166413939337E+03 0.35403332367385E+03 0.30115000000837E+03 0.30115000001426E+03
 0.32814891485711E+03 0.35399938183267E+03 0.30115000000652E+03 0.30115000001419E+03 0.39964518635299E+03
 0.31372943998310E+03 0.23102282638962E+04 0.21958184465176E+04 0.82374407191818E+03 0.14030400492051E+04
 0.57517725692736E+03 0.13514108212774E+04 0.13332369275819E+04 0.12659569643427E+04 0.21174521543867E+04
 0.12058016277050E+04 0.13325322332910E+04 0.11411499782440E+04 0.21170669709177E+04 0.13514108212774E+04
 0.13332369275819E+04 0.12659569643427E+04 0.21174521543867E+04 0.12058016277050E+04 0.13325322332910E+04
 0.11411499782440E+04 0.21170669709177E+04 0.21141182099679E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50333116600835E+03 0.14691350819717E+01
 0.14691350819717E+01 0.16148947239950E+01 0.71420999733744E-02 0.31327623441572E+03 0.34243771179720E+03
 0.34229886233932E+03 0.34228600470622E+03 0.23000000000000E+00 0.00000000000000E+00 0.18650839235574E+00
 0.00000000000000E+00 -.21247236798685E+02 0.24552687470544E-01 0.80270382510057E+00 0.32582991208590E+03
 0.12218621703221E+03 0.99663160306950E+01 0.37373685115106E+01 0.31372576398402E+03 0.39966977533117E+03
 0.30377016778504E+03 0.31016641053233E+03 0.30115000000019E+03 0.30115000000185E+03 0.30376070817397E+03
 0.31016625355635E+03 0.30115000000019E+03 0.30115000000185E+03 0.30377016778504E+03 0.31016641053233E+03
 0.30115000000019E+03 0.30115000000185E+03 0.30376070817397E+03 0.31016625355635E+03 0.30115000000019E+03
 0.30115000000185E+03 0.30874433093988E+03 0.30115000002572E+03 0.14959413981093E+03 0.14899251541036E+03
 0.18555673131429E+03 0.48760020140296E+03 0.30111568643211E+03 0.14732107781420E+03 0.17546828307289E+03
 0.14732107781420E+03 0.39203933191392E+03 0.14723980971012E+03 0.17539513253689E+03 0.14723980971012E+03
 0.39196745331948E+03 0.14732107781420E+03 0.17546828307289E+03 0.14732107781420E+03 0.39203933191392E+03
 0.14723980971012E+03 0.17539513253689E+03 0.14723980971012E+03 0.39196745331948E+03 0.27003586036596E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37000266806196E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22581180774089E+00 0.00000000000000E+00 0.00000000000000E+00 0.22581180774089E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23662283187756E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23662283187756E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205033971577E+00 0.23850931518754E+00 0.34243771179720E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    370.74938760
 0.11094242009341E+00 0.31419497786446E+03 0.45801744615813E+03 0.45269877525937E+03 0.45039322080363E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16048417270572E+00 0.00000000000000E+00 -.28064598394810E+02
 0.31517064106828E-02 0.96154700822405E+00 0.25383074936434E+04 0.95186531011628E+03 0.83199260478963E+01
 0.31199722679611E+01 0.33789033962094E+03 0.30115000025517E+03 0.33228891120425E+03 0.35498491133548E+03
 0.30115000001253E+03 0.30115000002146E+03 0.32873141176821E+03 0.35495147397493E+03 0.30115000000977E+03
 0.30115000002136E+03 0.33228891120425E+03 0.35498491133548E+03 0.30115000001253E+03 0.30115000002146E+03
 0.32873141176821E+03 0.35495147397493E+03 0.30115000000977E+03 0.30115000002136E+03 0.40081925147710E+03
 0.31450134996516E+03 0.23178917036402E+04 0.22007993271211E+04 0.82144566708560E+03 0.13925609330042E+04
 0.56700803758319E+03 0.13567190668737E+04 0.13376641331551E+04 0.12693834891278E+04 0.21172890918881E+04
 0.12115255495409E+04 0.13369859811776E+04 0.11453348555488E+04 0.21169251098050E+04 0.13567190668737E+04
 0.13376641331551E+04 0.12693834891278E+04 0.21172890918881E+04 0.12115255495409E+04 0.13369859811776E+04
 0.11453348555488E+04 0.21169251098050E+04 0.21127810719816E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50382398370478E+03 0.14691352922006E+01
 0.14691352922006E+01 0.16680324825463E+01 0.53385073177106E-02 0.31415133586484E+03 0.34257573903683E+03
 0.34247457644727E+03 0.34246545877159E+03 0.23000000000000E+00 0.00000000000000E+00 0.18488169142812E+00
 0.00000000000000E+00 -.21250502697334E+02 0.32847709978084E-01 0.83260218264908E+00 0.24354818053793E+03
 0.91330567701725E+02 0.96084302524244E+01 0.36031613446591E+01 0.31449774734579E+03 0.40084223583141E+03
 0.30389190620973E+03 0.31037872450248E+03 0.30115000000023E+03 0.30115000000274E+03 0.30388286341798E+03
 0.31037845317947E+03 0.30115000000022E+03 0.30115000000274E+03 0.30389190620973E+03 0.31037872450248E+03
 0.30115000000023E+03 0.30115000000274E+03 0.30388286341798E+03 0.31037845317947E+03 0.30115000000022E+03
 0.30115000000274E+03 0.30892532223728E+03 0.30115000003742E+03 0.14888143972114E+03 0.14845719556055E+03
 0.18871160703961E+03 0.49035443883795E+03 0.30069927376314E+03 0.15117033225002E+03 0.17835730825594E+03
 0.15117033225002E+03 0.39426449735955E+03 0.15111303751427E+03 0.17827868359364E+03 0.15111303751427E+03
 0.39418806931007E+03 0.15117033225002E+03 0.17835730825594E+03 0.15117033225002E+03 0.39426449735955E+03
 0.15111303751427E+03 0.17827868359364E+03 0.15111303751427E+03 0.39418806931007E+03 0.27132542627317E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37028996879000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22582836592912E+00 0.00000000000000E+00 0.00000000000000E+00 0.22582836592912E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23663811323666E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23663811323666E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23204992446521E+00 0.23841278303994E+00 0.34257573903683E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    380.88460115
 0.11097160851798E+00 0.31448532003666E+03 0.45850318760236E+03 0.45317588946266E+03 0.45086564286667E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15953627566109E+00 0.00000000000000E+00 -.28079683655166E+02
 0.31508770526962E-02 0.97363249778034E+00 0.25389756141562E+04 0.95211585530857E+03 0.82166526058222E+01
 0.30812447271833E+01 0.33857953477081E+03 0.30115000035656E+03 0.33287653677139E+03 0.35587800176569E+03
 0.30115000001818E+03 0.30115000003129E+03 0.32927970667007E+03 0.35584502284392E+03 0.30115000001420E+03
 0.30115000003114E+03 0.33287653677139E+03 0.35587800176569E+03 0.30115000001818E+03 0.30115000003129E+03
 0.32927970667007E+03 0.35584502284392E+03 0.30115000001420E+03 0.30115000003114E+03 0.40191304479913E+03
 0.31524607108638E+03 0.23250204084290E+04 0.22053954521935E+04 0.81927366386398E+03 0.13829311776405E+04
 0.55956114545720E+03 0.13616696271193E+04 0.13417549458382E+04 0.12725510254603E+04 0.21171533248589E+04
 0.12168688917725E+04 0.13411001660793E+04 0.11492167186188E+04 0.21168078640384E+04 0.13616696271193E+04
 0.13417549458382E+04 0.12725510254603E+04 0.21171533248589E+04 0.12168688917725E+04 0.13411001660793E+04
 0.11492167186188E+04 0.21168078640384E+04 0.21115702387283E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50429632441858E+03 0.14691354137403E+01
 0.14691354137403E+01 0.17187085503195E+01 0.40425479337798E-02 0.31497514254334E+03 0.34270984677634E+03
 0.34263510086214E+03 0.34262853850822E+03 0.23000000000000E+00 0.00000000000000E+00 0.18335678412968E+00
 0.00000000000000E+00 -.21242931799253E+02 0.43378021887269E-01 0.86058925718395E+00 0.18442519165098E+03
 0.69159446869117E+02 0.92959561523901E+01 0.34859835571463E+01 0.31524255031535E+03 0.40193460274186E+03
 0.30400825703634E+03 0.31057860947099E+03 0.30115000000028E+03 0.30115000000397E+03 0.30399958660378E+03
 0.31057823127467E+03 0.30115000000028E+03 0.30115000000397E+03 0.30400825703634E+03 0.31057860947099E+03
 0.30115000000028E+03 0.30115000000397E+03 0.30399958660378E+03 0.31057823127467E+03 0.30115000000028E+03
 0.30115000000397E+03 0.30909563403930E+03 0.30115000005283E+03 0.14790110570646E+03 0.14760122474032E+03
 0.19153470012731E+03 0.49283143628637E+03 0.30033906265842E+03 0.15475933443503E+03 0.18093265404101E+03
 0.15475933443503E+03 0.39625880367289E+03 0.15472058752251E+03 0.18084919591869E+03 0.15472058752251E+03
 0.39617840517195E+03 0.15475933443503E+03 0.18093265404101E+03 0.15475933443503E+03 0.39625880367289E+03
 0.15472058752251E+03 0.18084919591869E+03 0.15472058752251E+03 0.39617840517195E+03 0.27237659076237E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37054729157576E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22578026525800E+00 0.00000000000000E+00 0.00000000000000E+00 0.22578026525800E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23660796363127E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23660796363127E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23204992078541E+00 0.23831950241528E+00 0.34270984677634E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    391.07272129
 0.11097080243433E+00 0.31477507643247E+03 0.45899218155069E+03 0.45365755225745E+03 0.45134307534343E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15862376413229E+00 0.00000000000000E+00 -.28091733317938E+02
 0.31508995588070E-02 0.98519685814367E+00 0.25389574788696E+04 0.95210905457610E+03 0.81202045397037E+01
 0.30450767023889E+01 0.33926311452768E+03 0.30115000049298E+03 0.33345987669559E+03 0.35676234831434E+03
 0.30115000002611E+03 0.30115000004510E+03 0.32982448550980E+03 0.35672981062054E+03 0.30115000002042E+03
 0.30115000004489E+03 0.33345987669559E+03 0.35676234831434E+03 0.30115000002611E+03 0.30115000004510E+03
 0.32982448550980E+03 0.35672981062054E+03 0.30115000002042E+03 0.30115000004489E+03 0.40298728348252E+03
 0.31600198265533E+03 0.23320319541910E+04 0.22098971049251E+04 0.81713691166005E+03 0.13736305216450E+04
 0.55240792542661E+03 0.13665491516263E+04 0.13457555726618E+04 0.12756593097488E+04 0.21170453362624E+04
 0.12221395955044E+04 0.13451225570106E+04 0.11530336176931E+04 0.21167169742454E+04 0.13665491516263E+04
 0.13457555726618E+04 0.12756593097488E+04 0.21170453362624E+04 0.12221395955044E+04 0.13451225570106E+04
 0.11530336176931E+04 0.21167169742454E+04 0.21104354541183E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50477362078461E+03 0.14691355108227E+01
 0.14691355108227E+01 0.17696491510112E+01 0.30556220920448E-02 0.31579455365077E+03 0.34284753024895E+03
 0.34279242113365E+03 0.34278771057427E+03 0.23000000000000E+00 0.00000000000000E+00 0.18185331521464E+00
 0.00000000000000E+00 -.21231846157105E+02 0.57388551520109E-01 0.88814163997020E+00 0.13940062587565E+03
 0.52275234703368E+02 0.90075722609610E+01 0.33778395978604E+01 0.31599855423319E+03 0.40300746527664E+03
 0.30412568699939E+03 0.31077716002653E+03 0.30115000000036E+03 0.30115000000570E+03 0.30411736662567E+03
 0.31077667632986E+03 0.30115000000036E+03 0.30115000000571E+03 0.30412568699939E+03 0.31077716002653E+03
 0.30115000000036E+03 0.30115000000570E+03 0.30411736662567E+03 0.31077667632986E+03 0.30115000000036E+03
 0.30115000000571E+03 0.30926477640866E+03 0.30115000007380E+03 0.14664842808883E+03 0.14643976207628E+03
 0.19420636438288E+03 0.49519722949905E+03 0.30001983329426E+03 0.15829482921547E+03 0.18336070298721E+03
 0.15829482921547E+03 0.39815407021215E+03 0.15827116071202E+03 0.18327273678632E+03 0.15827116071202E+03
 0.39807001454357E+03 0.15829482921547E+03 0.18336070298721E+03 0.15829482921547E+03 0.39815407021214E+03
 0.15827116071202E+03 0.18327273678632E+03 0.15827116071202E+03 0.39807001454357E+03 0.27328244328397E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37079500757747E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22571121742907E+00 0.00000000000000E+00 0.00000000000000E+00 0.22571121742907E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23655456581419E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655456581419E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205003221855E+00 0.23822393662559E+00 0.34284753024895E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    401.77647573
 0.11094497912091E+00 0.31507554919066E+03 0.45950568740666E+03 0.45416442118373E+03 0.45184584532183E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15770636644832E+00 0.00000000000000E+00 -.28102218463451E+02
 0.31516325532599E-02 0.99675094442109E+00 0.25383669780048E+04 0.95188761675181E+03 0.80260771708085E+01
 0.30097789390532E+01 0.33997138107840E+03 0.30115000068544E+03 0.33406473838451E+03 0.35767779650962E+03
 0.30115000003776E+03 0.30115000006548E+03 0.33038974665571E+03 0.35764570014854E+03 0.30115000002959E+03
 0.30115000006518E+03 0.33406473838451E+03 0.35767779650962E+03 0.30115000003776E+03 0.30115000006548E+03
 0.33038974665571E+03 0.35764570014854E+03 0.30115000002959E+03 0.30115000006518E+03 0.40409359159834E+03
 0.31680371804793E+03 0.23392383403708E+04 0.22144943590412E+04 0.81488772903404E+03 0.13641408275835E+04
 0.54517865990429E+03 0.13715760200930E+04 0.13498389022133E+04 0.12788392617856E+04 0.21169465582336E+04
 0.12275740546672E+04 0.13492270542550E+04 0.11569495632717E+04 0.21166346974946E+04 0.13715760200930E+04
 0.13498389022133E+04 0.12788392617856E+04 0.21169465582336E+04 0.12275740546672E+04 0.13492270542550E+04
 0.11569495632717E+04 0.21166346974946E+04 0.21092896977977E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50527623031918E+03 0.14691355952999E+01
 0.14691355952999E+01 0.18231679232107E+01 0.22761112509895E-02 0.31665114203642E+03 0.34299594611539E+03
 0.34295597031208E+03 0.34295264986328E+03 0.23000000000000E+00 0.00000000000000E+00 0.18030822716635E+00
 0.00000000000000E+00 -.21217264807800E+02 0.77042683593476E-01 0.91640991847170E+00 0.10383854282923E+03
 0.38939453560962E+02 0.87297178246845E+01 0.32736441842567E+01 0.31680039638213E+03 0.40411239781264E+03
 0.30424906242041E+03 0.31098301281375E+03 0.30115000000049E+03 0.30115000000827E+03 0.30424108117770E+03
 0.31098242093178E+03 0.30115000000049E+03 0.30115000000828E+03 0.30424906242041E+03 0.31098301281375E+03
 0.30115000000049E+03 0.30115000000827E+03 0.30424108117770E+03 0.31098242093178E+03 0.30115000000049E+03
 0.30115000000828E+03 0.30944011465475E+03 0.30115000010369E+03 0.14506846337424E+03 0.14493115314546E+03
 0.19685445541467E+03 0.49757897491170E+03 0.29974024721995E+03 0.16194343814469E+03 0.18575858676951E+03
 0.16194343814469E+03 0.40005035962750E+03 0.16193243066027E+03 0.18566622752846E+03 0.16193243066027E+03
 0.39996278255490E+03 0.16194343814469E+03 0.18575858676951E+03 0.16194343814469E+03 0.40005035962750E+03
 0.16193243066027E+03 0.18566622752846E+03 0.16193243066027E+03 0.39996278255490E+03 0.27409865174714E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37104681056368E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22562450800819E+00 0.00000000000000E+00 0.00000000000000E+00 0.22562450800819E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23646494897729E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23646494897729E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205024382132E+00 0.23812110749691E+00 0.34299594611539E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    411.20671516
 0.11089822261730E+00 0.31533947868974E+03 0.45995815528656E+03 0.45461217055594E+03 0.45229042613957E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15693140593036E+00 0.00000000000000E+00 -.28107347422141E+02
 0.31529609769526E-02 0.10064502327758E+01 0.25372974985983E+04 0.95148656197438E+03 0.79487288486544E+01
 0.29807733182454E+01 0.34058797503630E+03 0.30115000090684E+03 0.33459174203560E+03 0.35847324226464E+03
 0.30115000005168E+03 0.30115000008988E+03 0.33088271680941E+03 0.35844151966123E+03 0.30115000004056E+03
 0.30115000008947E+03 0.33459174203560E+03 0.35847324226464E+03 0.30115000005168E+03 0.30115000008988E+03
 0.33088271680941E+03 0.35844151966123E+03 0.30115000004056E+03 0.30115000008947E+03 0.40504690533648E+03
 0.31751512555638E+03 0.23454694617538E+04 0.22184618807804E+04 0.81296163990464E+03 0.13561085349597E+04
 0.53908208685553E+03 0.13759306843793E+04 0.13533521768672E+04 0.12815888978406E+04 0.21168863520570E+04
 0.12322844443679E+04 0.13527576670193E+04 0.11603388726786E+04 0.21165878806452E+04 0.13759306843793E+04
 0.13533521768672E+04 0.12815888978406E+04 0.21168863520570E+04 0.12322844443679E+04 0.13527576670193E+04
 0.11603388726786E+04 0.21165878806452E+04 0.21083574122804E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50572060291535E+03 0.14691356366232E+01
 0.14691356366232E+01 0.18703191203237E+01 0.17554080212236E-02 0.31740165621873E+03 0.34313036005803E+03
 0.34310025047597E+03 0.34309781286885E+03 0.23000000000000E+00 0.00000000000000E+00 0.17897792703267E+00
 0.00000000000000E+00 -.21199784164073E+02 0.99895699957599E-01 0.94070684525788E+00 0.80083527152776E+02
 0.30031322682291E+02 0.85042434211339E+01 0.31890912829252E+01 0.31751190747663E+03 0.40506454487913E+03
 0.30435835119869E+03 0.31116245571088E+03 0.30115000000066E+03 0.30115000001137E+03 0.30435064766789E+03
 0.31116177016094E+03 0.30115000000065E+03 0.30115000001137E+03 0.30435835119869E+03 0.31116245571088E+03
 0.30115000000066E+03 0.30115000001137E+03 0.30435064766789E+03 0.31116177016094E+03 0.30115000000065E+03
 0.30115000001137E+03 0.30959298266956E+03 0.30115000013841E+03 0.14347903452549E+03 0.14338749643961E+03
 0.19906714645870E+03 0.49960602089701E+03 0.29954353870602E+03 0.16510999598440E+03 0.18775505429322E+03
 0.16510999598440E+03 0.40165318299266E+03 0.16510789765213E+03 0.18765908270461E+03 0.16510789765213E+03
 0.40156274759188E+03 0.16510999598440E+03 0.18775505429322E+03 0.16510999598440E+03 0.40165318299266E+03
 0.16510789765213E+03 0.18765908270461E+03 0.16510789765213E+03 0.40156274759188E+03 0.27472051325433E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37126434905076E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22552120661340E+00 0.00000000000000E+00 0.00000000000000E+00 0.22552120661340E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23636466192440E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23636466192440E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205059519865E+00 0.23802823018786E+00 0.34313036005803E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    420.63695458
 0.11083842240937E+00 0.31560148607218E+03 0.46041035980629E+03 0.45506023076777E+03 0.45273551188634E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15618609399380E+00 0.00000000000000E+00 -.28111409872781E+02
 0.31546617247435E-02 0.10157229446187E+01 0.25359295854932E+04 0.95097359455994E+03 0.78761635172107E+01
 0.29535613189540E+01 0.34119789570604E+03 0.30115000118846E+03 0.33511341018504E+03 0.35925891992817E+03
 0.30115000007000E+03 0.30115000012206E+03 0.33137108519607E+03 0.35922755709599E+03 0.30115000005503E+03
 0.30115000012152E+03 0.33511341018504E+03 0.35925891992817E+03 0.30115000007000E+03 0.30115000012206E+03
 0.33137108519607E+03 0.35922755709599E+03 0.30115000005503E+03 0.30115000012152E+03 0.40598275907623E+03
 0.31823079139605E+03 0.23515930937074E+04 0.22223454651352E+04 0.81106553287270E+03 0.13483259180736E+04
 0.53320505753654E+03 0.13802191970240E+04 0.13567905241155E+04 0.12842862075533E+04 0.21168466826375E+04
 0.12369255960508E+04 0.13562122211201E+04 0.11636682863920E+04 0.21165606187421E+04 0.13802191970240E+04
 0.13567905241155E+04 0.12842862075533E+04 0.21168466826375E+04 0.12369255960508E+04 0.13562122211201E+04
 0.11636682863920E+04 0.21165606187421E+04 0.21074775568360E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50616546480985E+03 0.14691356693538E+01
 0.14691356693538E+01 0.19174703174367E+01 0.13535071738131E-02 0.31815048495159E+03 0.34326896249946E+03
 0.34324629713976E+03 0.34324450941628E+03 0.23000000000000E+00 0.00000000000000E+00 0.17767762668048E+00
 0.00000000000000E+00 -.21180888683357E+02 0.12955801822804E+00 0.96441390485995E+00 0.61748397431635E+02
 0.23155649036863E+02 0.82951935467601E+01 0.31106975800350E+01 0.31822768564752E+03 0.40599927729117E+03
 0.30446804866622E+03 0.31134015411484E+03 0.30115000000090E+03 0.30115000001546E+03 0.30446060323691E+03
 0.31133937661340E+03 0.30115000000088E+03 0.30115000001547E+03 0.30446804866622E+03 0.31134015411484E+03
 0.30115000000090E+03 0.30115000001546E+03 0.30446060323691E+03 0.31133937661340E+03 0.30115000000088E+03
 0.30115000001547E+03 0.30974439662640E+03 0.30115000018294E+03 0.14172506938740E+03 0.14167167168778E+03
 0.20118295919015E+03 0.50159059173363E+03 0.29940171774753E+03 0.16823964636429E+03 0.18965844409558E+03
 0.16823964636429E+03 0.40321262935673E+03 0.16824472275209E+03 0.18955908432995E+03 0.16824472275209E+03
 0.40311954549326E+03 0.16823964636429E+03 0.18965844409558E+03 0.16823964636429E+03 0.40321262935673E+03
 0.16824472275209E+03 0.18955908432995E+03 0.16824472275209E+03 0.40311954549325E+03 0.27527196506976E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37148012675687E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22541103838124E+00 0.00000000000000E+00 0.00000000000000E+00 0.22541103838124E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23624914953181E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23624914953181E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205098658977E+00 0.23793256672778E+00 0.34326896249946E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    430.06719400
 0.11076558263481E+00 0.31586177433270E+03 0.46086190061615E+03 0.45550822612618E+03 0.45318074888521E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15546908439991E+00 0.00000000000000E+00 -.28114737817029E+02
 0.31567358828375E-02 0.10245897495388E+01 0.25342633330506E+04 0.95034874989397E+03 0.78080031579478E+01
 0.29280011842304E+01 0.34180141345571E+03 0.30115000154368E+03 0.33562995401315E+03 0.36003524919625E+03
 0.30115000009388E+03 0.30115000016411E+03 0.33185503201336E+03 0.36000423278229E+03 0.30115000007394E+03
 0.30115000016339E+03 0.33562995401315E+03 0.36003524919625E+03 0.30115000009388E+03 0.30115000016411E+03
 0.33185503201336E+03 0.36000423278229E+03 0.30115000007394E+03 0.30115000016339E+03 0.40690227733620E+03
 0.31895022175813E+03 0.23576146124561E+04 0.22261505290293E+04 0.80919246319488E+03 0.13407669610634E+04
 0.52752853555252E+03 0.13844448315758E+04 0.13601557764590E+04 0.12869345989424E+04 0.21168216815719E+04
 0.12415009416156E+04 0.13595926400262E+04 0.11669415245727E+04 0.21165471270405E+04 0.13844448315758E+04
 0.13601557764590E+04 0.12869345989424E+04 0.21168216815719E+04 0.12415009416155E+04 0.13595926400262E+04
 0.11669415245727E+04 0.21165471270405E+04 0.21066418729040E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50661045404294E+03 0.14691356961665E+01
 0.14691356961665E+01 0.19646215145498E+01 0.10433900559570E-02 0.31889705033667E+03 0.34341186484175E+03
 0.34339481249929E+03 0.34339350260354E+03 0.23000000000000E+00 0.00000000000000E+00 0.17640804910464E+00
 0.00000000000000E+00 -.21160981299792E+02 0.16806533682656E+00 0.98751763600152E+00 0.47600535310003E+02
 0.17850200741251E+02 0.81011211429015E+01 0.30379204285881E+01 0.31894723565695E+03 0.40691772575198E+03
 0.30457815188412E+03 0.31151620789795E+03 0.30115000000121E+03 0.30115000002084E+03 0.30457094587281E+03
 0.31151534015818E+03 0.30115000000120E+03 0.30115000002084E+03 0.30457815188412E+03 0.31151620789795E+03
 0.30115000000121E+03 0.30115000002084E+03 0.30457094587281E+03 0.31151534015818E+03 0.30115000000120E+03
 0.30115000002084E+03 0.30989445291848E+03 0.30115000023957E+03 0.13982139534410E+03 0.13979025040607E+03
 0.20321260181783E+03 0.50354431104735E+03 0.29931564622043E+03 0.17133826739332E+03 0.19147915472462E+03
 0.17133826739332E+03 0.40473834298551E+03 0.17134908429820E+03 0.19137660854186E+03 0.17134908429820E+03
 0.40464279824829E+03 0.17133826739332E+03 0.19147915472462E+03 0.17133826739332E+03 0.40473834298551E+03
 0.17134908429820E+03 0.19137660854186E+03 0.17134908429820E+03 0.40464279824829E+03 0.27576163569056E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37169536452126E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22529625509817E+00 0.00000000000000E+00 0.00000000000000E+00 0.22529625509817E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23612142281187E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23612142281187E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205140314388E+00 0.23783403067054E+00 0.34341186484175E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    443.12758147
 0.11065414646666E+00 0.31621477483985E+03 0.46148423470856E+03 0.45612601201208E+03 0.45379473552975E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15452047633447E+00 0.00000000000000E+00 -.28128889512786E+02
 0.31599144366163E-02 0.10362379531163E+01 0.25317141208945E+04 0.94939279533544E+03 0.77202345039976E+01
 0.28950879389991E+01 0.34262523014604E+03 0.30115000220079E+03 0.33633540682684E+03 0.36109526716511E+03
 0.30115000013995E+03 0.30115000024546E+03 0.33251614024026E+03 0.36106470327651E+03 0.30115000011051E+03
 0.30115000024441E+03 0.33633540682684E+03 0.36109526716511E+03 0.30115000013995E+03 0.30115000024546E+03
 0.33251614024026E+03 0.36106470327650E+03 0.30115000011051E+03 0.30115000024441E+03 0.40815969685749E+03
 0.31995523724598E+03 0.23657817256667E+04 0.22312648795367E+04 0.80647576376670E+03 0.13303549154129E+04
 0.51984677282733E+03 0.13901917065195E+04 0.13646813141163E+04 0.12905017117269E+04 0.21167774222279E+04
 0.12477291001349E+04 0.13641376071533E+04 0.11713667652468E+04 0.21165175125233E+04 0.13901917065195E+04
 0.13646813141163E+04 0.12905017117269E+04 0.21167774222279E+04 0.12477291001349E+04 0.13641376071533E+04
 0.11713667652468E+04 0.21165175125233E+04 0.21054543378388E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50722415413813E+03 0.14691358101847E+01
 0.14691358101847E+01 0.20299234518859E+01 0.72754495269578E-03 0.31994019324267E+03 0.34361701475435E+03
 0.34360553078635E+03 0.34360468137065E+03 0.23000000000000E+00 0.00000000000000E+00 0.17470123326844E+00
 0.00000000000000E+00 -.21142368158256E+02 0.24102660913294E+00 0.10185048608762E+01 0.33191356044791E+02
 0.12446758516796E+02 0.78546507800831E+01 0.29454940425312E+01 0.31995242107964E+03 0.40817377721928E+03
 0.30472961309149E+03 0.31175655999901E+03 0.30115000000187E+03 0.30115000003129E+03 0.30472270626878E+03
 0.31175557135823E+03 0.30115000000184E+03 0.30115000003130E+03 0.30472961309149E+03 0.31175655999901E+03
 0.30115000000187E+03 0.30115000003129E+03 0.30472270626878E+03 0.31175557135823E+03 0.30115000000184E+03
 0.30115000003130E+03 0.31009935157540E+03 0.30115000034539E+03 0.13693986523860E+03 0.13693453931860E+03
 0.20590344561538E+03 0.50623029984960E+03 0.29929733700615E+03 0.17559366628075E+03 0.19388664279552E+03
 0.17559366628075E+03 0.40682322256710E+03 0.17561054205262E+03 0.19377997515521E+03 0.17561054205262E+03
 0.40672452797797E+03 0.17559366628075E+03 0.19388664279552E+03 0.17559366628075E+03 0.40682322256710E+03
 0.17561054205262E+03 0.19377997515521E+03 0.17561054205262E+03 0.40672452797797E+03 0.27636104556710E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37199354342131E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22520038676707E+00 0.00000000000000E+00 0.00000000000000E+00 0.22520038676707E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23591879822660E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23591879822660E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205160833449E+00 0.23769229031457E+00 0.34361701475435E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    450.01580259
 0.11059962168481E+00 0.31639870794171E+03 0.46181079934641E+03 0.45644995858049E+03 0.45411656254995E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15404015522318E+00 0.00000000000000E+00 -.28106026277125E+02
 0.31614719936698E-02 0.10420985029310E+01 0.25304668255858E+04 0.94892505959467E+03 0.76768174769461E+01
 0.28788065538548E+01 0.34305579876321E+03 0.30115000264170E+03 0.33670451230311E+03 0.36164694882038E+03
 0.30115000017196E+03 0.30115000030211E+03 0.33286266860733E+03 0.36161661848832E+03 0.30115000013598E+03
 0.30115000030083E+03 0.33670451230311E+03 0.36164694882038E+03 0.30115000017196E+03 0.30115000030211E+03
 0.33286266860733E+03 0.36161661848832E+03 0.30115000013598E+03 0.30115000030083E+03 0.40880144586259E+03
 0.32048536551893E+03 0.23699871335374E+04 0.22338656968705E+04 0.80518248075089E+03 0.13252452627831E+04
 0.51603686962850E+03 0.13931634927211E+04 0.13670068311965E+04 0.12923251061367E+04 0.21167522921010E+04
 0.12509500617855E+04 0.13664727179112E+04 0.11736332881225E+04 0.21164995124696E+04 0.13931634927211E+04
 0.13670068311965E+04 0.12923251061367E+04 0.21167522921010E+04 0.12509500617855E+04 0.13664727179112E+04
 0.11736332881225E+04 0.21164995124696E+04 0.21048931264640E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50754586364879E+03 0.14691356259789E+01
 0.14691356259789E+01 0.20643645574765E+01 0.60167710861991E-03 0.32049718026358E+03 0.34372924066861E+03
 0.34371992186932E+03 0.34371924666283E+03 0.23000000000000E+00 0.00000000000000E+00 0.17382513907112E+00
 0.00000000000000E+00 -.21102718585545E+02 0.29144816378860E+00 0.10343735939317E+01 0.27449135022868E+02
 0.10293425633575E+02 0.77341494861558E+01 0.29003060573084E+01 0.32048263866772E+03 0.40881485768187E+03
 0.30481120376938E+03 0.31188285410995E+03 0.30115000000234E+03 0.30115000003859E+03 0.30480444396619E+03
 0.31188180183795E+03 0.30115000000231E+03 0.30115000003861E+03 0.30481120376938E+03 0.31188285410995E+03
 0.30115000000234E+03 0.30115000003859E+03 0.30480444396619E+03 0.31188180183795E+03 0.30115000000231E+03
 0.30115000003861E+03 0.31020709364999E+03 0.30115000041698E+03 0.13534483043570E+03 0.13534813564172E+03
 0.20727709114494E+03 0.50764554919943E+03 0.29933207259876E+03 0.17783773695277E+03 0.19511117008135E+03
 0.17783773695277E+03 0.40791469364090E+03 0.17785707730692E+03 0.19500237611775E+03 0.17785707730692E+03
 0.40781438400508E+03 0.17783773695277E+03 0.19511117008135E+03 0.17783773695277E+03 0.40791469364090E+03
 0.17785707730692E+03 0.19500237611775E+03 0.17785707730692E+03 0.40781438400508E+03 0.27665317464949E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37215284053467E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22495363211882E+00 0.00000000000000E+00 0.00000000000000E+00 0.22495363211882E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23582193254684E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23582193254684E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205286844988E+00 0.23761606830309E+00 0.34372924066861E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    460.63121345
 0.11046776225762E+00 0.31669431064176E+03 0.46231665110959E+03 0.45695445974752E+03 0.45461914977769E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15332528989621E+00 0.00000000000000E+00 -.28116480398811E+02
 0.31652452687479E-02 0.10507600582893E+01 0.25274502671208E+04 0.94779385017032E+03 0.76135364462027E+01
 0.28550761673260E+01 0.34371525690448E+03 0.30115000343388E+03 0.33727023940629E+03 0.36249001332018E+03
 0.30115000023074E+03 0.30115000040626E+03 0.33339424548732E+03 0.36246003400704E+03 0.30115000018282E+03
 0.30115000040457E+03 0.33727023940629E+03 0.36249001332019E+03 0.30115000023074E+03 0.30115000040626E+03
 0.33339424548732E+03 0.36246003400704E+03 0.30115000018282E+03 0.30115000040457E+03 0.40977840585784E+03
 0.32130300463566E+03 0.23764636842669E+04 0.22379759581392E+04 0.80324885142648E+03 0.13176159335360E+04
 0.51035083785238E+03 0.13977325610021E+04 0.13705861594332E+04 0.12952091391711E+04 0.21168020743338E+04
 0.12559018566300E+04 0.13700659601484E+04 0.11771916391084E+04 0.21165595104850E+04 0.13977325610021E+04
 0.13705861594332E+04 0.12952091391711E+04 0.21168020743338E+04 0.12559018566300E+04 0.13700659601484E+04
 0.11771916391084E+04 0.21165595104850E+04 0.21041723499795E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50804791954430E+03 0.14691357102063E+01
 0.14691357102063E+01 0.21174416118142E+01 0.44874566106312E-03 0.32135567091257E+03 0.34390622929132E+03
 0.34389948298117E+03 0.34389900972250E+03 0.23000000000000E+00 0.00000000000000E+00 0.17250795978769E+00
 0.00000000000000E+00 -.21086211261617E+02 0.39077298521366E+00 0.10581841242769E+01 0.20472244251034E+02
 0.76770915941379E+01 0.75601209812771E+01 0.28350453679789E+01 0.32130046001902E+03 0.40979067900639E+03
 0.30493787338196E+03 0.31207660098970E+03 0.30115000000324E+03 0.30115000005207E+03 0.30493132756162E+03
 0.31207545207869E+03 0.30115000000319E+03 0.30115000005208E+03 0.30493787338196E+03 0.31207660098970E+03
 0.30115000000324E+03 0.30115000005207E+03 0.30493132756162E+03 0.31207545207869E+03 0.30115000000319E+03
 0.30115000005208E+03 0.31037248441062E+03 0.30115000054627E+03 0.13277991232524E+03 0.13279741437462E+03
 0.20931952078517E+03 0.50979778033745E+03 0.29943166194835E+03 0.18123531366763E+03 0.19692920871300E+03
 0.18123531366763E+03 0.40956581681238E+03 0.18125765656078E+03 0.19681746932134E+03 0.18125765656078E+03
 0.40946333803133E+03 0.18123531366763E+03 0.19692920871300E+03 0.18123531366763E+03 0.40956581681238E+03
 0.18125765656078E+03 0.19681746932134E+03 0.18125765656078E+03 0.40946333803133E+03 0.27705980633742E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37239958207000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22486821631399E+00 0.00000000000000E+00 0.00000000000000E+00 0.22486821631399E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23564758878171E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23564758878171E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205305962670E+00 0.23749401544348E+00 0.34390622929132E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    470.42036102
 0.11035967920142E+00 0.31695697323626E+03 0.46278033755583E+03 0.45741599765370E+03 0.45507836151677E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15269220612082E+00 0.00000000000000E+00 -.28122355671799E+02
 0.31683448501262E-02 0.10583791250169E+01 0.25249776708117E+04 0.94686662655440E+03 0.75587280690864E+01
 0.28345230259074E+01 0.34431669909447E+03 0.30115000434303E+03 0.33778645032826E+03 0.36325873924887E+03
 0.30115000030033E+03 0.30115000052976E+03 0.33387949481157E+03 0.36322906907764E+03 0.30115000023837E+03
 0.30115000052760E+03 0.33778645032826E+03 0.36325873924887E+03 0.30115000030033E+03 0.30115000052976E+03
 0.33387949481157E+03 0.36322906907764E+03 0.30115000023837E+03 0.30115000052760E+03 0.41066930587934E+03
 0.32206045334013E+03 0.23823138075167E+04 0.22416125548964E+04 0.80139564179669E+03 0.13106087587588E+04
 0.50520613875316E+03 0.14018758435035E+04 0.13737981537881E+04 0.12977665875337E+04 0.21168231727340E+04
 0.12603953508083E+04 0.13732899353252E+04 0.11803668486509E+04 0.21165893293292E+04 0.14018758435035E+04
 0.13737981537881E+04 0.12977665875337E+04 0.21168231727340E+04 0.12603953508083E+04 0.13732899353252E+04
 0.11803668486509E+04 0.21165893293292E+04 0.21034583008513E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50850682431037E+03 0.14691357575425E+01
 0.14691357575425E+01 0.21663873496352E+01 0.34232251460509E-03 0.32213402567143E+03 0.34407503464125E+03
 0.34407002737368E+03 0.34406968640466E+03 0.23000000000000E+00 0.00000000000000E+00 0.17132825114596E+00
 0.00000000000000E+00 -.21067492145070E+02 0.51225866705849E+00 0.10794543478755E+01 0.15617110093887E+02
 0.58564162852075E+01 0.74111517691741E+01 0.27791819134403E+01 0.32205805748245E+03 0.41068068373732E+03
 0.30505450489374E+03 0.31225358856346E+03 0.30115000000434E+03 0.30115000006810E+03 0.30504813989004E+03
 0.31225235277922E+03 0.30115000000428E+03 0.30115000006812E+03 0.30505450489374E+03 0.31225358856346E+03
 0.30115000000434E+03 0.30115000006810E+03 0.30504813989004E+03 0.31225235277922E+03 0.30115000000428E+03
 0.30115000006812E+03 0.31052362156464E+03 0.30115000069572E+03 0.13029809802982E+03 0.13032415999972E+03
 0.21115376708273E+03 0.51180411247749E+03 0.29959457655934E+03 0.18436592786243E+03 0.19855882117145E+03
 0.18436592786243E+03 0.41109934217598E+03 0.18439035885300E+03 0.19844447563815E+03 0.18439035885300E+03
 0.41099495509238E+03 0.18436592786243E+03 0.19855882117145E+03 0.18436592786243E+03 0.41109934217598E+03
 0.18439035885300E+03 0.19844447563814E+03 0.18439035885300E+03 0.41099495509238E+03 0.27741254193457E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37263007465681E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22476656009256E+00 0.00000000000000E+00 0.00000000000000E+00 0.22476656009256E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23548747265294E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23548747265294E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205335709908E+00 0.23737784757976E+00 0.34407503464125E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    480.35723496
 0.11025577851868E+00 0.31721896466728E+03 0.46324793397707E+03 0.45788108807789E+03 0.45554089853617E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15207427534846E+00 0.00000000000000E+00 -.28129774153881E+02
 0.31713302034064E-02 0.10657666667848E+01 0.25226007658890E+04 0.94597528720837E+03 0.75063334680322E+01
 0.28148750505121E+01 0.34492083480276E+03 0.30115000548094E+03 0.33830524655981E+03 0.36403048391730E+03
 0.30115000039006E+03 0.30115000068930E+03 0.33436741812111E+03 0.36400111470377E+03 0.30115000031015E+03
 0.30115000068654E+03 0.33830524655981E+03 0.36403048391730E+03 0.30115000039006E+03 0.30115000068930E+03
 0.33436741812111E+03 0.36400111470376E+03 0.30115000031015E+03 0.30115000068654E+03 0.41156231811918E+03
 0.32283204696184E+03 0.23881379628524E+04 0.22451938147259E+04 0.79946943270702E+03 0.13035609526363E+04
 0.50009417276576E+03 0.14060148932547E+04 0.13769728100811E+04 0.13002950838989E+04 0.21168184672703E+04
 0.12648871593459E+04 0.13764759872634E+04 0.11835165242904E+04 0.21165928429155E+04 0.14060148932547E+04
 0.13769728100811E+04 0.13002950838989E+04 0.21168184672703E+04 0.12648871593459E+04 0.13764759872634E+04
 0.11835165242904E+04 0.21165928429155E+04 0.21027047396523E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50896911350088E+03 0.14691358173121E+01
 0.14691358173121E+01 0.22160717193222E+01 0.25997144768741E-03 0.32292005931494E+03 0.34425147622155E+03
 0.34424777918199E+03 0.34424753500849E+03 0.23000000000000E+00 0.00000000000000E+00 0.17016472056881E+00
 0.00000000000000E+00 -.21050592316618E+02 0.67452664440125E+00 0.11003786273628E+01 0.11860169003555E+02
 0.44475633763333E+01 0.72702248126835E+01 0.27263343047563E+01 0.32282979909326E+03 0.41157285172700E+03
 0.30517298257975E+03 0.31243180221930E+03 0.30115000000583E+03 0.30115000008890E+03 0.30516678678644E+03
 0.31243048012691E+03 0.30115000000575E+03 0.30115000008892E+03 0.30517298257975E+03 0.31243180221930E+03
 0.30115000000583E+03 0.30115000008890E+03 0.30516678678644E+03 0.31243048012691E+03 0.30115000000575E+03
 0.30115000008892E+03 0.31067586663762E+03 0.30115000088406E+03 0.12767427815858E+03 0.12770658129868E+03
 0.21297136116090E+03 0.51386095021202E+03 0.29982473224531E+03 0.18753811076865E+03 0.20017094257459E+03
 0.18753811076865E+03 0.41266659857225E+03 0.18756412964340E+03 0.20005406606343E+03 0.18756412964340E+03
 0.41256037392660E+03 0.18753811076865E+03 0.20017094257459E+03 0.18753811076865E+03 0.41266659857225E+03
 0.18756412964340E+03 0.20005406606343E+03 0.18756412964340E+03 0.41256037392660E+03 0.27775173753135E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37286744738154E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22467418271748E+00 0.00000000000000E+00 0.00000000000000E+00 0.22467418271748E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23533818590448E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23533818590448E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205356356371E+00 0.23725643336359E+00 0.34425147622155E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    491.48578994
 0.11014419609463E+00 0.31751081796937E+03 0.46376856303558E+03 0.45839874911794E+03 0.45605564798769E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15141066511160E+00 0.00000000000000E+00 -.28127422227657E+02
 0.31745425332306E-02 0.10736412099925E+01 0.25200481380411E+04 0.94501805176541E+03 0.74512788122726E+01
 0.27942295546022E+01 0.34559066701591E+03 0.30115000705662E+03 0.33888087281340E+03 0.36488467877987E+03
 0.30115000051819E+03 0.30115000091745E+03 0.33490926153715E+03 0.36485563413806E+03 0.30115000041287E+03
 0.30115000091385E+03 0.33888087281340E+03 0.36488467877987E+03 0.30115000051819E+03 0.30115000091745E+03
 0.33490926153715E+03 0.36485563413806E+03 0.30115000041287E+03 0.30115000091385E+03 0.41254508651918E+03
 0.32369733740222E+03 0.23945377958827E+04 0.22491092327202E+04 0.79733479695510E+03 0.12958725615199E+04
 0.49455109057997E+03 0.14105783111634E+04 0.13804436651571E+04 0.13030742143446E+04 0.21168037687326E+04
 0.12698414971449E+04 0.13799587592985E+04 0.11869814536849E+04 0.21165866355118E+04 0.14105783111634E+04
 0.13804436651571E+04 0.13030742143446E+04 0.21168037687326E+04 0.12698414971449E+04 0.13799587592985E+04
 0.11869814536849E+04 0.21165866355118E+04 0.21018792201364E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50948359296386E+03 0.14691357983630E+01
 0.14691357983630E+01 0.22717144942425E+01 0.19096414727171E-03 0.32379595505329E+03 0.34445527490988E+03
 0.34445264478361E+03 0.34445247699504E+03 0.23000000000000E+00 0.00000000000000E+00 0.16890169333520E+00
 0.00000000000000E+00 -.21019972652594E+02 0.91827530434188E+00 0.11230258946549E+01 0.87119842624250E+01
 0.32669940984094E+01 0.71236113415338E+01 0.26713542530752E+01 0.32369526373464E+03 0.41255470530944E+03
 0.30530658428532E+03 0.31263021887949E+03 0.30115000000803E+03 0.30115000011876E+03 0.30530056343560E+03
 0.31262880176602E+03 0.30115000000792E+03 0.30115000011879E+03 0.30530658428532E+03 0.31263021887949E+03
 0.30115000000803E+03 0.30115000011876E+03 0.30530056343560E+03 0.31262880176602E+03 0.30115000000792E+03
 0.30115000011879E+03 0.31084547382834E+03 0.30115000114666E+03 0.12463503190637E+03 0.12467203452551E+03
 0.21496120631422E+03 0.51618789910291E+03 0.30015188675712E+03 0.19108322240199E+03 0.20193292517312E+03
 0.19108322240199E+03 0.41443337079843E+03 0.19111051195049E+03 0.20181334642300E+03 0.19111051195049E+03
 0.41432520738597E+03 0.19108322240199E+03 0.20193292517312E+03 0.19108322240199E+03 0.41443337079843E+03
 0.19111051195049E+03 0.20181334642299E+03 0.19111051195049E+03 0.41432520738597E+03 0.27811488739606E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37313545462159E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22450260953996E+00 0.00000000000000E+00 0.00000000000000E+00 0.22450260953996E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23513531096666E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23513531096666E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205423468009E+00 0.23711681685851E+00 0.34445527490988E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    500.01047921
 0.11019287227256E+00 0.31771572706616E+03 0.46416525867429E+03 0.45878602716400E+03 0.45643733745743E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15092196080392E+00 0.00000000000000E+00 -.28395472061972E+02
 0.31731399266924E-02 0.10793944785251E+01 0.25211620618127E+04 0.94543577317978E+03 0.74115628337581E+01
 0.27793360626593E+01 0.34609943318410E+03 0.30115000850609E+03 0.33931844624089E+03 0.36553193069533E+03
 0.30115000063920E+03 0.30115000113321E+03 0.33532161900701E+03 0.36550312743586E+03 0.30115000051005E+03
 0.30115000112883E+03 0.33931844624089E+03 0.36553193069533E+03 0.30115000063920E+03 0.30115000113321E+03
 0.33532161900701E+03 0.36550312743586E+03 0.30115000051005E+03 0.30115000112883E+03 0.41328287144075E+03
 0.32451293317410E+03 0.23992522441381E+04 0.22518115158764E+04 0.79575755059157E+03 0.12901945355993E+04
 0.49045819725478E+03 0.14139848794603E+04 0.13830421968031E+04 0.13050343888642E+04 0.21167894573559E+04
 0.12735391615931E+04 0.13825657897146E+04 0.11894556453371E+04 0.21165782781737E+04 0.14139848794603E+04
 0.13830421968031E+04 0.13050343888642E+04 0.21167894573559E+04 0.12735391615931E+04 0.13825657897146E+04
 0.11894556453371E+04 0.21165782781737E+04 0.21011975045856E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.50986584355304E+03 0.14691379580059E+01
 0.14691379580059E+01 0.23143379406025E+01 0.00000000000000E+00 0.34458305766512E+03 0.34458305766512E+03
 0.34458305766512E+03 0.34458305766512E+03 0.00000000000000E+00 0.00000000000000E+00 0.16796769616793E+00
 0.00000000000000E+00 -.21307372500610E+02 0.10000000000000E-02 0.11399406252574E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70179093741776E+01 0.26317160153166E+01 0.32451096620424E+03 0.41329181107231E+03
 0.31278111570760E+03 0.31278111570760E+03 0.30115000014716E+03 0.30115000014710E+03 0.31277969395078E+03
 0.31277969395078E+03 0.30115000014720E+03 0.30115000014714E+03 0.31278111570760E+03 0.31278111570760E+03
 0.30115000014716E+03 0.30115000014710E+03 0.31277969395078E+03 0.31277969395078E+03 0.30115000014720E+03
 0.30115000014714E+03 0.31097380762815E+03 0.30115000138964E+03 0.12178895385214E+03 0.14901023705802E+03
 0.21569877868761E+03 0.51700023863977E+03 0.30022296605872E+03 0.14446967564949E+03 0.20299479452407E+03
 0.14446967564949E+03 0.41537701939580E+03 0.14447505886549E+03 0.20290215625775E+03 0.14447505886549E+03
 0.41529583440804E+03 0.14446967564949E+03 0.20299479452407E+03 0.14446967564949E+03 0.41537701939580E+03
 0.14447505886549E+03 0.20290215625775E+03 0.14447505886549E+03 0.41529583440804E+03 0.28444587778768E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37337599345618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22612672528046E+00 0.00000000000000E+00 0.00000000000000E+00 0.22612672528046E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23616547200323E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23616547200323E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23204270158776E+00 0.23701643314646E+00 0.34458305766512E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    510.00278509
 0.11044269098478E+00 0.31795342396808E+03 0.46462152769958E+03 0.45922205434700E+03 0.45686309541511E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15037541183038E+00 0.00000000000000E+00 -.28301219545490E+02
 0.31659620408696E-02 0.10857927177459E+01 0.25268780537251E+04 0.94757927014690E+03 0.73678887961304E+01
 0.27629582985489E+01 0.34668945704666E+03 0.30115001050277E+03 0.33982641964468E+03 0.36628320152453E+03
 0.30115000080981E+03 0.30115000143773E+03 0.33580047634713E+03 0.36625467113294E+03 0.30115000064728E+03
 0.30115000143226E+03 0.33982641964468E+03 0.36628320152453E+03 0.30115000080981E+03 0.30115000143773E+03
 0.33580047634713E+03 0.36625467113294E+03 0.30115000064728E+03 0.30115000143226E+03 0.41413659432134E+03
 0.32545539260972E+03 0.24044273327570E+04 0.22546471403071E+04 0.79376848413425E+03 0.12834321190341E+04
 0.48569479247918E+03 0.14178028548433E+04 0.13858915936898E+04 0.13072038484338E+04 0.21165839463096E+04
 0.12776898431352E+04 0.13854243674291E+04 0.11922084636879E+04 0.21163790689788E+04 0.14178028548433E+04
 0.13858915936898E+04 0.13072038484338E+04 0.21165839463096E+04 0.12776898431351E+04 0.13854243674291E+04
 0.11922084636879E+04 0.21163790689788E+04 0.21001479744574E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51029291319827E+03 0.14691371986247E+01
 0.14691371986247E+01 0.23642994700050E+01 0.00000000000000E+00 0.34468444559002E+03 0.34468444559002E+03
 0.34468444559002E+03 0.34468444559002E+03 0.00000000000000E+00 0.00000000000000E+00 0.16689178657614E+00
 0.00000000000000E+00 -.21179549260386E+02 0.10000000000000E-02 0.11594224263870E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68999872849875E+01 0.25874952318703E+01 0.32545217733539E+03 0.41414473490639E+03
 0.31295255783673E+03 0.31295255783673E+03 0.30115000018729E+03 0.30115000018722E+03 0.31295112198369E+03
 0.31295112198369E+03 0.30115000018734E+03 0.30115000018727E+03 0.31295255783673E+03 0.31295255783673E+03
 0.30115000018729E+03 0.30115000018722E+03 0.31295112198369E+03 0.31295112198369E+03 0.30115000018734E+03
 0.30115000018727E+03 0.31111998069743E+03 0.30115000172606E+03 0.11709180471931E+03 0.14289201754067E+03
 0.21691017898673E+03 0.51771445203279E+03 0.29971972215113E+03 0.14740836026310E+03 0.20395899554422E+03
 0.14740836026310E+03 0.41574651079603E+03 0.14741377513940E+03 0.20386102185609E+03 0.14741377513940E+03
 0.41566009606678E+03 0.14740836026310E+03 0.20395899554422E+03 0.14740836026310E+03 0.41574651079603E+03
 0.14741377513940E+03 0.20386102185609E+03 0.14741377513940E+03 0.41566009606678E+03 0.28325913616893E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37352852388035E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22538882610828E+00 0.00000000000000E+00 0.00000000000000E+00 0.22538882610828E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23567892190921E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23567892190921E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23204742616095E+00 0.23695183895994E+00 0.34468444559002E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    520.26043134
 0.11042213849099E+00 0.31822513459285E+03 0.46508534018797E+03 0.45967980086762E+03 0.45731711147434E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14983992920553E+00 0.00000000000000E+00 -.28239798542561E+02
 0.31665509544480E-02 0.10920095610988E+01 0.25264081061960E+04 0.94740303982348E+03 0.73259431830892E+01
 0.27472286936585E+01 0.34728924624855E+03 0.30115001298055E+03 0.34034316017945E+03 0.36704588655181E+03
 0.30115000102740E+03 0.30115000182657E+03 0.33628773796865E+03 0.36701762406112E+03 0.30115000082264E+03
 0.30115000181975E+03 0.34034316017945E+03 0.36704588655181E+03 0.30115000102740E+03 0.30115000182657E+03
 0.33628773796865E+03 0.36701762406112E+03 0.30115000082264E+03 0.30115000181975E+03 0.41500427392454E+03
 0.32633607076505E+03 0.24098291535036E+04 0.22578808999958E+04 0.79164227280011E+03 0.12764821715928E+04
 0.48088168742865E+03 0.14217295411779E+04 0.13887242381844E+04 0.13095949891027E+04 0.21163168507052E+04
 0.12819665652669E+04 0.13882659057010E+04 0.11952033331334E+04 0.21161180331619E+04 0.14217295411779E+04
 0.13887242381844E+04 0.13095949891027E+04 0.21163168507052E+04 0.12819665652669E+04 0.13882659057010E+04
 0.11952033331334E+04 0.21161180331619E+04 0.20991647873753E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51074677006434E+03 0.14691367037635E+01
 0.14691367037635E+01 0.24155877012501E+01 0.00000000000000E+00 0.34482250935933E+03 0.34482250935933E+03
 0.34482250935933E+03 0.34482250935933E+03 0.00000000000000E+00 0.00000000000000E+00 0.16582391641379E+00
 0.00000000000000E+00 -.21086298446137E+02 0.10000000000000E-02 0.11786181073742E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67876099560554E+01 0.25453537335208E+01 0.32633354006721E+03 0.41501169138613E+03
 0.31312615651370E+03 0.31312615651370E+03 0.30115000024166E+03 0.30115000023865E+03 0.31312467258191E+03
 0.31312467258191E+03 0.30115000024172E+03 0.30115000023871E+03 0.31312615651370E+03 0.31312615651370E+03
 0.30115000024166E+03 0.30115000023865E+03 0.31312467258191E+03 0.31312467258191E+03 0.30115000024172E+03
 0.30115000023871E+03 0.31126848461200E+03 0.30115000214604E+03 0.11298140159560E+03 0.13753601142152E+03
 0.21828065917656E+03 0.51894776409910E+03 0.29957570162666E+03 0.15028728341039E+03 0.20509999346943E+03
 0.15028728341039E+03 0.41656698042941E+03 0.15029294485679E+03 0.20499789350565E+03 0.15029294485679E+03
 0.41647682107721E+03 0.15028728341039E+03 0.20509999346943E+03 0.15028728341039E+03 0.41656698042941E+03
 0.15029294485679E+03 0.20499789350565E+03 0.15029294485679E+03 0.41647682107721E+03 0.28251538113151E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37371806998231E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22485334015922E+00 0.00000000000000E+00 0.00000000000000E+00 0.22485334015922E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23528593161484E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23528593161484E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205070820080E+00 0.23686053378860E+00 0.34482250935933E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    530.67108518
 0.11025915698922E+00 0.31850312984626E+03 0.46555699265842E+03 0.46015231434319E+03 0.45778877676697E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14931816749887E+00 0.00000000000000E+00 -.28195991266904E+02
 0.31712312703540E-02 0.10980086089541E+01 0.25226794635848E+04 0.94600479884431E+03 0.72859173732896E+01
 0.27322190149836E+01 0.34789297173261E+03 0.30115001600767E+03 0.34086359009225E+03 0.36781149035049E+03
 0.30115000130051E+03 0.30115000231519E+03 0.33677892163043E+03 0.36778349004021E+03 0.30115000104317E+03
 0.30115000230670E+03 0.34086359009225E+03 0.36781149035049E+03 0.30115000130051E+03 0.30115000231519E+03
 0.33677892163043E+03 0.36778349004021E+03 0.30115000104317E+03 0.30115000230670E+03 0.41587122339110E+03
 0.32719924214321E+03 0.24153990628509E+04 0.22612957181967E+04 0.78958202607475E+03 0.12697119881776E+04
 0.47618205197250E+03 0.14257412562038E+04 0.13916148851473E+04 0.13120580454761E+04 0.21161474481244E+04
 0.12863349018469E+04 0.13911650730463E+04 0.11982800685843E+04 0.21159543765609E+04 0.14257412562038E+04
 0.13916148851473E+04 0.13120580454761E+04 0.21161474481244E+04 0.12863349018469E+04 0.13911650730463E+04
 0.11982800685843E+04 0.21159543765609E+04 0.20983504352206E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51121772610180E+03 0.14691363508141E+01
 0.14691363508141E+01 0.24676409704683E+01 0.00000000000000E+00 0.34498846471913E+03 0.34498846471913E+03
 0.34498846471913E+03 0.34498846471913E+03 0.00000000000000E+00 0.00000000000000E+00 0.16477648849662E+00
 0.00000000000000E+00 -.21011524985547E+02 0.10000000000000E-02 0.11973243879527E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66815643951585E+01 0.25055866481844E+01 0.32719737051006E+03 0.41587794926427E+03
 0.31330153357778E+03 0.31330153357778E+03 0.30115000030735E+03 0.30115000030352E+03 0.31329999067457E+03
 0.31329999067457E+03 0.30115000030743E+03 0.30115000030360E+03 0.31330153357778E+03 0.31330153357778E+03
 0.30115000030735E+03 0.30115000030352E+03 0.31329999067457E+03 0.31329999067457E+03 0.30115000030743E+03
 0.30115000030360E+03 0.31141872362899E+03 0.30115000266213E+03 0.10909904001146E+03 0.13249513119734E+03
 0.21977385452128E+03 0.52057667416209E+03 0.29970395036820E+03 0.15320394557415E+03 0.20636998500721E+03
 0.15320394557415E+03 0.41772224702694E+03 0.15320992223927E+03 0.20626426073999E+03 0.15320992223927E+03
 0.41762893437817E+03 0.15320394557415E+03 0.20636998500721E+03 0.15320394557415E+03 0.41772224702694E+03
 0.15320992223927E+03 0.20626426073999E+03 0.15320992223927E+03 0.41762893437817E+03 0.28204270987649E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37393525576475E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22442608522415E+00 0.00000000000000E+00 0.00000000000000E+00 0.22442608522415E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23494405170848E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23494405170848E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205319825772E+00 0.23674930820321E+00 0.34498846471913E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    540.56041829
 0.11005900666505E+00 0.31875864698184E+03 0.46600498398294E+03 0.46060305545447E+03 0.45823930916754E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14884042583536E+00 0.00000000000000E+00 -.28182802824057E+02
 0.31769980239676E-02 0.11034525074280E+01 0.25181004015889E+04 0.94428765059583E+03 0.72499721973960E+01
 0.27187395740235E+01 0.34846067389241E+03 0.30115001949162E+03 0.34135301727151E+03 0.36853158292435E+03
 0.30115000162392E+03 0.30115000289445E+03 0.33724089142257E+03 0.36850382047273E+03 0.30115000130485E+03
 0.30115000288403E+03 0.34135301727151E+03 0.36853158292435E+03 0.30115000162392E+03 0.30115000289445E+03
 0.33724089142257E+03 0.36850382047273E+03 0.30115000130485E+03 0.30115000288403E+03 0.41669125181571E+03
 0.32800802485906E+03 0.24207096630812E+04 0.22645323918069E+04 0.78757170239278E+03 0.12632799044700E+04
 0.47177034356520E+03 0.14295540818575E+04 0.13943599920776E+04 0.13143624212407E+04 0.21160544659347E+04
 0.12904866135538E+04 0.13939178032431E+04 0.12011692293961E+04 0.21158664961397E+04 0.14295540818575E+04
 0.13943599920776E+04 0.13143624212407E+04 0.21160544659347E+04 0.12904866135538E+04 0.13939178032431E+04
 0.12011692293961E+04 0.21158664961397E+04 0.20976208524995E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51166755896075E+03 0.14691362445567E+01
 0.14691362445567E+01 0.25170876360139E+01 0.00000000000000E+00 0.34516438837979E+03 0.34516438837979E+03
 0.34516438837979E+03 0.34516438837979E+03 0.00000000000000E+00 0.00000000000000E+00 0.16381427361771E+00
 0.00000000000000E+00 -.20970981990883E+02 0.10000000000000E-02 0.12144093371614E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65875646334368E+01 0.24703367375388E+01 0.32800651597463E+03 0.41669737085504E+03
 0.31346723551788E+03 0.31346723551788E+03 0.30115000039108E+03 0.30115000038074E+03 0.31346563259624E+03
 0.31346563259624E+03 0.30115000039119E+03 0.30115000038085E+03 0.31346723551788E+03 0.31346723551788E+03
 0.30115000039108E+03 0.30115000038074E+03 0.31346563259624E+03 0.31346563259624E+03 0.30115000039119E+03
 0.30115000038085E+03 0.31156072643092E+03 0.30115000325970E+03 0.10552618519545E+03 0.12788075017576E+03
 0.22126478484732E+03 0.52239691266733E+03 0.30002580389577E+03 0.15600473620124E+03 0.20765295207382E+03
 0.15600473620124E+03 0.41904963796856E+03 0.15601103780227E+03 0.20754403474165E+03 0.15601103780227E+03
 0.41895361332638E+03 0.15600473620124E+03 0.20765295207382E+03 0.15600473620124E+03 0.41904963796856E+03
 0.15601103780227E+03 0.20754403474165E+03 0.15601103780227E+03 0.41895361332638E+03 0.28177000751585E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37416019322474E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22420435975916E+00 0.00000000000000E+00 0.00000000000000E+00 0.22420435975916E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23468473455784E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23468473455784E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205432814164E+00 0.23662988852344E+00 0.34516438837979E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    550.07399820
 0.10987078446647E+00 0.31900330512404E+03 0.46643623283767E+03 0.46103670902964E+03 0.45867263363471E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14839639581609E+00 0.00000000000000E+00 -.28146370066351E+02
 0.31824402658404E-02 0.11084679157333E+01 0.25137942370420E+04 0.94267283889074E+03 0.72171687483690E+01
 0.27064382806384E+01 0.34900369129727E+03 0.30115002343064E+03 0.34182151224432E+03 0.36921737148922E+03
 0.30115000199857E+03 0.30115000356606E+03 0.33768386396717E+03 0.36918983308792E+03 0.30115000160852E+03
 0.30115000355344E+03 0.34182151224432E+03 0.36921737148922E+03 0.30115000199857E+03 0.30115000356606E+03
 0.33768386396717E+03 0.36918983308792E+03 0.30115000160852E+03 0.30115000355344E+03 0.41746040813252E+03
 0.32876969992859E+03 0.24257875936367E+04 0.22676239871024E+04 0.78584191782441E+03 0.12575338269203E+04
 0.46776269950681E+03 0.14332041479545E+04 0.13970091902010E+04 0.13165657166475E+04 0.21160443222902E+04
 0.12944570240477E+04 0.13965739639413E+04 0.12039237864266E+04 0.21158609441876E+04 0.14332041479545E+04
 0.13970091902010E+04 0.13165657166475E+04 0.21160443222902E+04 0.12944570240477E+04 0.13965739639413E+04
 0.12039237864266E+04 0.21158609441876E+04 0.20970641169344E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51210024102126E+03 0.14691359510230E+01
 0.14691359510230E+01 0.25646555355348E+01 0.00000000000000E+00 0.34534799329597E+03 0.34534799329597E+03
 0.34534799329597E+03 0.34534799329597E+03 0.00000000000000E+00 0.00000000000000E+00 0.16291749092326E+00
 0.00000000000000E+00 -.20907346583911E+02 0.10000000000000E-02 0.12302465229434E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65027617236093E+01 0.24385356463535E+01 0.32876841159233E+03 0.41746596482256E+03
 0.31362748361991E+03 0.31362748361991E+03 0.30115000047128E+03 0.30115000047060E+03 0.31362581912359E+03
 0.31362581912359E+03 0.30115000047141E+03 0.30115000047073E+03 0.31362748361991E+03 0.31362748361991E+03
 0.30115000047128E+03 0.30115000047060E+03 0.31362581912359E+03 0.31362581912359E+03 0.30115000047141E+03
 0.30115000047073E+03 0.31169821436726E+03 0.30115000393877E+03 0.10223299716296E+03 0.12364466199475E+03
 0.22274481310141E+03 0.52432961966735E+03 0.30047108250044E+03 0.15869236974029E+03 0.20893733903705E+03
 0.15869236974029E+03 0.42047879663888E+03 0.15869900948390E+03 0.20882568337633E+03 0.15869900948390E+03
 0.42038052812953E+03 0.15869236974029E+03 0.20893733903705E+03 0.15869236974029E+03 0.42047879663888E+03
 0.15869900948390E+03 0.20882568337633E+03 0.15869900948390E+03 0.42038052812953E+03 0.28164567436945E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37438764074398E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22383610883306E+00 0.00000000000000E+00 0.00000000000000E+00 0.22383610883306E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23440972339378E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23440972339378E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205633941035E+00 0.23650628224493E+00 0.34534799329597E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    560.92861790
 0.10962643596381E+00 0.31928709109572E+03 0.46692938406054E+03 0.46153421793545E+03 0.45917053201468E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14790713711964E+00 0.00000000000000E+00 -.28143610731709E+02
 0.31895332681780E-02 0.11139403045735E+01 0.25082039682157E+04 0.94057648808087E+03 0.71817133890880E+01
 0.26931425209080E+01 0.34961926440111E+03 0.30115002864326E+03 0.34235288040906E+03 0.36999345906161E+03
 0.30115000250541E+03 0.30115000447536E+03 0.33818664731179E+03 0.36996616849474E+03 0.30115000201999E+03
 0.30115000445980E+03 0.34235288040906E+03 0.36999345906161E+03 0.30115000250541E+03 0.30115000447536E+03
 0.33818664731179E+03 0.36996616849474E+03 0.30115000201999E+03 0.30115000445980E+03 0.41833050068281E+03
 0.32963044812379E+03 0.24315997413883E+04 0.22712262819098E+04 0.78390005782467E+03 0.12511194129511E+04
 0.46329985483732E+03 0.14373737638684E+04 0.14000482187027E+04 0.13191254645116E+04 0.21161214352197E+04
 0.12989905631270E+04 0.13996204815646E+04 0.12071064305023E+04 0.21159429240504E+04 0.14373737638684E+04
 0.14000482187027E+04 0.13191254645116E+04 0.21161214352197E+04 0.12989905631270E+04 0.13996204815646E+04
 0.12071064305022E+04 0.21159429240504E+04 0.20965377654353E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51259722021678E+03 0.14691359287915E+01
 0.14691359287915E+01 0.26189286340610E+01 0.00000000000000E+00 0.34556821633211E+03 0.34556821633211E+03
 0.34556821633211E+03 0.34556821633211E+03 0.00000000000000E+00 0.00000000000000E+00 0.16192810454684E+00
 0.00000000000000E+00 -.20875148240459E+02 0.10000000000000E-02 0.12476346655997E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64121334719046E+01 0.24045500519642E+01 0.32962938303494E+03 0.41833537453567E+03
 0.31381049397297E+03 0.31381049397297E+03 0.30115000059966E+03 0.30115000059265E+03 0.31380875785866E+03
 0.31380875785866E+03 0.30115000059982E+03 0.30115000059282E+03 0.31381049397297E+03 0.31381049397297E+03
 0.30115000059966E+03 0.30115000059265E+03 0.31380875785866E+03 0.31380875785866E+03 0.30115000059982E+03
 0.30115000059282E+03 0.31185531553283E+03 0.30115000484154E+03 0.98520201894488E+02 0.11889684891253E+03
 0.22445601799913E+03 0.52666914461541E+03 0.30109084652629E+03 0.16176650026606E+03 0.21042703748663E+03
 0.16176650026606E+03 0.42221927676445E+03 0.16177353572430E+03 0.21031242137317E+03 0.16177353572430E+03
 0.42211862394607E+03 0.16176650026606E+03 0.21042703748663E+03 0.16176650026606E+03 0.42221927676445E+03
 0.16177353572430E+03 0.21031242137317E+03 0.16177353572430E+03 0.42211862394607E+03 0.28158954313784E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37465895808605E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22366820880680E+00 0.00000000000000E+00 0.00000000000000E+00 0.22366820880680E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23414792039094E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23414792039094E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205703144294E+00 0.23635634198700E+00 0.34556821633211E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    571.33389420
 0.10942522147876E+00 0.31954881678081E+03 0.46739911744141E+03 0.46200626680958E+03 0.45964199092119E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14745493679953E+00 0.00000000000000E+00 -.28143020848927E+02
 0.31953979031637E-02 0.11189546683590E+01 0.25036005663267E+04 0.93885021237252E+03 0.71495300267457E+01
 0.26810737600297E+01 0.35020394816041E+03 0.30115003457473E+03 0.34285774592584E+03 0.37073050900649E+03
 0.30115000309708E+03 0.30115000553768E+03 0.33866451638932E+03 0.37070344660664E+03 0.30115000250121E+03
 0.30115000551876E+03 0.34285774592584E+03 0.37073050900649E+03 0.30115000309708E+03 0.30115000553768E+03
 0.33866451638932E+03 0.37070344660664E+03 0.30115000250121E+03 0.30115000551876E+03 0.41915900888655E+03
 0.33045098473954E+03 0.24370816909414E+04 0.22745472582216E+04 0.78196595895470E+03 0.12449369667366E+04
 0.45906117798708E+03 0.14413230409542E+04 0.14029073349903E+04 0.13214942560885E+04 0.21161855435414E+04
 0.13032856857297E+04 0.14024863397881E+04 0.12100682051576E+04 0.21160113569592E+04 0.14413230409542E+04
 0.14029073349903E+04 0.13214942560885E+04 0.21161855435414E+04 0.13032856857297E+04 0.14024863397881E+04
 0.12100682051576E+04 0.21160113569592E+04 0.20959799805160E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51306805628950E+03 0.14691359240388E+01
 0.14691359240388E+01 0.26709550155495E+01 0.00000000000000E+00 0.34578919327545E+03 0.34578919327545E+03
 0.34578919327545E+03 0.34578919327545E+03 0.00000000000000E+00 0.00000000000000E+00 0.16101206065319E+00
 0.00000000000000E+00 -.20846843714422E+02 0.10000000000000E-02 0.12636510287622E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63308617790122E+01 0.23740731671296E+01 0.33045004697185E+03 0.41916333570204E+03
 0.31398563882645E+03 0.31398563882645E+03 0.30115000075324E+03 0.30115000073580E+03 0.31398383326151E+03
 0.31398383326151E+03 0.30115000075344E+03 0.30115000073600E+03 0.31398563882645E+03 0.31398563882645E+03
 0.30115000075324E+03 0.30115000073580E+03 0.31398383326151E+03 0.31398383326151E+03 0.30115000075344E+03
 0.30115000073600E+03 0.31200570112941E+03 0.30115000587421E+03 0.94980130873540E+02 0.11439765850366E+03
 0.22612349605731E+03 0.52904258027287E+03 0.30178846673526E+03 0.16473569754866E+03 0.21188350074141E+03
 0.16473569754866E+03 0.42399488508646E+03 0.16474311831140E+03 0.21176616884376E+03 0.16474311831140E+03
 0.42389207608325E+03 0.16473569754866E+03 0.21188350074141E+03 0.16473569754866E+03 0.42399488508646E+03
 0.16474311831140E+03 0.21176616884376E+03 0.16474311831140E+03 0.42389207608325E+03 0.28161178575192E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37492720284067E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22352269641242E+00 0.00000000000000E+00 0.00000000000000E+00 0.22352269641242E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23390433367266E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23390433367266E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205757040712E+00 0.23620591283859E+00 0.34578919327545E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    580.38745338
 0.10928345572589E+00 0.31977001581772E+03 0.46780422532598E+03 0.46241166199908E+03 0.46004606349721E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14707478490806E+00 0.00000000000000E+00 -.28152365604624E+02
 0.31995427468569E-02 0.11231377648610E+01 0.25003572800703E+04 0.93763398002638E+03 0.71229017937888E+01
 0.26710881726708E+01 0.35070845098083E+03 0.30115004062736E+03 0.34329357522962E+03 0.37136606404285E+03
 0.30115000371551E+03 0.30115000664884E+03 0.33907726181610E+03 0.37133919347916E+03 0.30115000300510E+03
 0.30115000662647E+03 0.34329357522962E+03 0.37136606404285E+03 0.30115000371551E+03 0.30115000664884E+03
 0.33907726181610E+03 0.37133919347916E+03 0.30115000300510E+03 0.30115000662647E+03 0.41987238308408E+03
 0.33116034556086E+03 0.24417521179643E+04 0.22773150830200E+04 0.78024982990098E+03 0.12395767129989E+04
 0.45542563394845E+03 0.14447061813752E+04 0.14053319185228E+04 0.13234843318828E+04 0.21162057721684E+04
 0.13069664910712E+04 0.14049164669909E+04 0.12125686497986E+04 0.21160350947091E+04 0.14447061813752E+04
 0.14053319185228E+04 0.13234843318828E+04 0.21162057721684E+04 0.13069664910712E+04 0.14049164669909E+04
 0.12125686497986E+04 0.21160350947091E+04 0.20954420563261E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51347177900841E+03 0.14691359993282E+01
 0.14691359993282E+01 0.27162228114267E+01 0.00000000000000E+00 0.34598817284396E+03 0.34598817284396E+03
 0.34598817284396E+03 0.34598817284396E+03 0.00000000000000E+00 0.00000000000000E+00 0.16023986092369E+00
 0.00000000000000E+00 -.20835200814759E+02 0.10000000000000E-02 0.12770889887577E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62642463214578E+01 0.23490923705467E+01 0.33115949886074E+03 0.41987627558297E+03
 0.31413802080294E+03 0.31413802080294E+03 0.30115000090708E+03 0.30115000088608E+03 0.31413615409204E+03
 0.31413615409204E+03 0.30115000090733E+03 0.30115000088632E+03 0.31413802080294E+03 0.31413802080294E+03
 0.30115000090708E+03 0.30115000088608E+03 0.31413615409204E+03 0.31413615409204E+03 0.30115000090733E+03
 0.30115000088632E+03 0.31213657949612E+03 0.30115000693315E+03 0.91918220115354E+02 0.11052683959128E+03
 0.22759030661036E+03 0.53119820562852E+03 0.30246994748511E+03 0.16733030342810E+03 0.21316780123427E+03
 0.16733030342810E+03 0.42561505317009E+03 0.16733806492296E+03 0.21304821567513E+03 0.16733806492296E+03
 0.42551048420154E+03 0.16733030342810E+03 0.21316780123427E+03 0.16733030342810E+03 0.42561505317009E+03
 0.16733806492296E+03 0.21304821567513E+03 0.16733806492296E+03 0.42551048420154E+03 0.28168597852755E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37516962872826E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22346264052966E+00 0.00000000000000E+00 0.00000000000000E+00 0.22346264052966E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23377629400565E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23377629400565E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205751642254E+00 0.23607004183234E+00 0.34598817284396E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    590.20538026
 0.10912577061595E+00 0.32001872737458E+03 0.46824246964411E+03 0.46285079294453E+03 0.46048422964402E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14667635918955E+00 0.00000000000000E+00 -.28133035721719E+02
 0.32041656890227E-02 0.11274816904239E+01 0.24967497865068E+04 0.93628116994006E+03 0.70954589045187E+01
 0.26607970891945E+01 0.35125282497456E+03 0.30115004808524E+03 0.34376425882284E+03 0.37204922275883E+03
 0.30115000449243E+03 0.30115000804546E+03 0.33952365157224E+03 0.37202255592519E+03 0.30115000363900E+03
 0.30115000801883E+03 0.34376425882284E+03 0.37204922275883E+03 0.30115000449243E+03 0.30115000804546E+03
 0.33952365157225E+03 0.37202255592519E+03 0.30115000363900E+03 0.30115000801883E+03 0.42063037823899E+03
 0.33191931445254E+03 0.24467778333360E+04 0.22803575227557E+04 0.77852655084554E+03 0.12340757070065E+04
 0.45165652340677E+03 0.14483511478724E+04 0.14079484725288E+04 0.13256860189324E+04 0.21162658343234E+04
 0.13109300057184E+04 0.14075387359495E+04 0.12153114447838E+04 0.21160987135452E+04 0.14483511478724E+04
 0.14079484725288E+04 0.13256860189324E+04 0.21162658343234E+04 0.13109300057184E+04 0.14075387359495E+04
 0.12153114447838E+04 0.21160987135452E+04 0.20949667225705E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51390938426156E+03 0.14691358435901E+01
 0.14691358435901E+01 0.27653124458319E+01 0.00000000000000E+00 0.34620893891199E+03 0.34620893891199E+03
 0.34620893891199E+03 0.34620893891199E+03 0.00000000000000E+00 0.00000000000000E+00 0.15942794693510E+00
 0.00000000000000E+00 -.20788903517863E+02 0.10000000000000E-02 0.12911583002790E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61959869663321E+01 0.23234951123745E+01 0.33191860743129E+03 0.42063376526921E+03
 0.31430436872417E+03 0.31430436872417E+03 0.30115000109250E+03 0.30115000107554E+03 0.31430243467627E+03
 0.31430243467627E+03 0.30115000109279E+03 0.30115000107583E+03 0.31430436872417E+03 0.31430436872417E+03
 0.30115000109250E+03 0.30115000107554E+03 0.31430243467627E+03 0.31430243467627E+03 0.30115000109279E+03
 0.30115000107583E+03 0.31227958978529E+03 0.30115000824306E+03 0.88646095689997E+02 0.10640936015448E+03
 0.22917719464992E+03 0.53356153367759E+03 0.30323845305442E+03 0.17012373328317E+03 0.21455817984890E+03
 0.17012373328317E+03 0.42738928905930E+03 0.17013187270674E+03 0.21443631731114E+03 0.17013187270674E+03
 0.42728298661326E+03 0.17012373328317E+03 0.21455817984890E+03 0.17012373328317E+03 0.42738928905930E+03
 0.17013187270674E+03 0.21443631731114E+03 0.17013187270674E+03 0.42728298661326E+03 0.28179704494396E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37543061955266E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22320483084669E+00 0.00000000000000E+00 0.00000000000000E+00 0.22320483084669E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23351382338985E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23351382338985E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205875900373E+00 0.23592087883751E+00 0.34620893891199E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    601.38106045
 0.10896157814054E+00 0.32029332402420E+03 0.46873717316197E+03 0.46334561447286E+03 0.46097745168576E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14623940243389E+00 0.00000000000000E+00 -.28138159890995E+02
 0.32089936034654E-02 0.11322021943994E+01 0.24929934392393E+04 0.93487253971473E+03 0.70658757239412E+01
 0.26497033964780E+01 0.35186714180912E+03 0.30115005798744E+03 0.34429564236367E+03 0.37282024372369E+03
 0.30115000554829E+03 0.30115000994462E+03 0.34002769662474E+03 0.37279380008691E+03 0.30115000450197E+03
 0.30115000991230E+03 0.34429564236367E+03 0.37282024372369E+03 0.30115000554829E+03 0.30115000994462E+03
 0.34002769662474E+03 0.37279380008691E+03 0.30115000450197E+03 0.30115000991230E+03 0.42148939949988E+03
 0.33278116396662E+03 0.24524067010976E+04 0.22836987260701E+04 0.77644999104040E+03 0.12277059167090E+04
 0.44737367571343E+03 0.14524483833754E+04 0.14108532154559E+04 0.13281109812381E+04 0.21162917240944E+04
 0.13153882184761E+04 0.14104495852996E+04 0.12183501258715E+04 0.21161283411825E+04 0.14524483833754E+04
 0.14108532154559E+04 0.13281109812381E+04 0.21162917240944E+04 0.13153882184761E+04 0.14104495852996E+04
 0.12183501258715E+04 0.21161283411825E+04 0.20943413002198E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51440212111528E+03 0.14691358848748E+01
 0.14691358848748E+01 0.28211908468140E+01 0.00000000000000E+00 0.34646501938000E+03 0.34646501938000E+03
 0.34646501938000E+03 0.34646501938000E+03 0.00000000000000E+00 0.00000000000000E+00 0.15853499357764E+00
 0.00000000000000E+00 -.20767303067900E+02 0.10000000000000E-02 0.13065602367333E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61229477027416E+01 0.22961053885281E+01 0.33278061215209E+03 0.42149226905415E+03
 0.31449315453874E+03 0.31449315453874E+03 0.30115000135515E+03 0.30115000133411E+03 0.31449114402413E+03
 0.31449114402413E+03 0.30115000135551E+03 0.30115000133447E+03 0.31449315453874E+03 0.31449315453874E+03
 0.30115000135515E+03 0.30115000133411E+03 0.31449114402413E+03 0.31449114402413E+03 0.30115000135551E+03
 0.30115000133447E+03 0.31244191039881E+03 0.30115000999043E+03 0.84892094491162E+02 0.10171373901474E+03
 0.23098638920279E+03 0.53631670395919E+03 0.30417538281039E+03 0.17332053550725E+03 0.21614399950849E+03
 0.17332053550725E+03 0.42946186785950E+03 0.17332910515776E+03 0.21601959396278E+03 0.17332910515776E+03
 0.42935364078972E+03 0.17332053550725E+03 0.21614399950849E+03 0.17332053550725E+03 0.42946186785950E+03
 0.17332910515776E+03 0.21601959396278E+03 0.17332910515776E+03 0.42935364078972E+03 0.28195287942322E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37573621473292E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22309082945634E+00 0.00000000000000E+00 0.00000000000000E+00 0.22309082945634E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23332438028539E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23332438028539E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205894925528E+00 0.23574674771008E+00 0.34646501938000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    610.96021491
 0.10883127117120E+00 0.32052774185807E+03 0.46915808959674E+03 0.46376621303707E+03 0.46139653624700E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14587866596570E+00 0.00000000000000E+00 -.28133603152143E+02
 0.32128354960031E-02 0.11360616878387E+01 0.24900123302150E+04 0.93375462383061E+03 0.70418711286880E+01
 0.26407016732580E+01 0.35238969183560E+03 0.30115006780614E+03 0.34474792621608E+03 0.37347506045980E+03
 0.30115000661865E+03 0.30115001187079E+03 0.34045702318450E+03 0.37344880199213E+03 0.30115000537822E+03
 0.30115001183280E+03 0.34474792621608E+03 0.37347506045980E+03 0.30115000661865E+03 0.30115001187079E+03
 0.34045702318450E+03 0.37344880199213E+03 0.30115000537822E+03 0.30115001183280E+03 0.42221672329408E+03
 0.33351463415492E+03 0.24571582273039E+04 0.22865064539223E+04 0.77466987060940E+03 0.12223213337998E+04
 0.44377811383733E+03 0.14559186601625E+04 0.14132934171403E+04 0.13301610289763E+04 0.21162982451723E+04
 0.13191650038858E+04 0.14128947188866E+04 0.12209194387794E+04 0.21161378276800E+04 0.14559186601625E+04
 0.14132934171403E+04 0.13301610289763E+04 0.21162982451723E+04 0.13191650038858E+04 0.14128947188866E+04
 0.12209194387794E+04 0.21161378276800E+04 0.20937981380329E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51482080865651E+03 0.14691358481618E+01
 0.14691358481618E+01 0.28690866190843E+01 0.00000000000000E+00 0.34668810199634E+03 0.34668810199634E+03
 0.34668810199634E+03 0.34668810199634E+03 0.00000000000000E+00 0.00000000000000E+00 0.15779520943750E+00
 0.00000000000000E+00 -.20738887519513E+02 0.10000000000000E-02 0.13192611776210E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60640001659307E+01 0.22740000622240E+01 0.33351421823949E+03 0.42221917994627E+03
 0.31465513159256E+03 0.31465513159256E+03 0.30115000162248E+03 0.30115000159729E+03 0.31465305533463E+03
 0.31465305533463E+03 0.30115000162291E+03 0.30115000159772E+03 0.31465513159256E+03 0.31465513159256E+03
 0.30115000162248E+03 0.30115000159729E+03 0.31465305533463E+03 0.31465305533463E+03 0.30115000162291E+03
 0.30115000159772E+03 0.31258125123431E+03 0.30115001173069E+03 0.81676012695404E+02 0.97712620851429E+02
 0.23253411087414E+03 0.53870668859317E+03 0.30500990716465E+03 0.17605911278844E+03 0.21750083744251E+03
 0.17605911278844E+03 0.43125937313741E+03 0.17606805395030E+03 0.21737433268204E+03 0.17606805395030E+03
 0.43114957882747E+03 0.17605911278844E+03 0.21750083744251E+03 0.17605911278844E+03 0.43125937313741E+03
 0.17606805395030E+03 0.21737433268204E+03 0.17606805395030E+03 0.43114957882747E+03 0.28210527089074E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37599900880637E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22293575411531E+00 0.00000000000000E+00 0.00000000000000E+00 0.22293575411531E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23312913530703E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23312913530703E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205949011165E+00 0.23559566748286E+00 0.34668810199634E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    620.53936936
 0.10870359462934E+00 0.32076120389973E+03 0.46957632482318E+03 0.46418407862999E+03 0.46181288970017E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14553017938589E+00 0.00000000000000E+00 -.28132917107159E+02
 0.32166087645002E-02 0.11397554099409E+01 0.24870914014447E+04 0.93265927554176E+03 0.70190498156219E+01
 0.26321436808582E+01 0.35290863928040E+03 0.30115007900139E+03 0.34519734467428E+03 0.37412448110277E+03
 0.30115000786377E+03 0.30115001411235E+03 0.34088388886312E+03 0.37409840229265E+03 0.30115000639906E+03
 0.30115001406788E+03 0.34519734467428E+03 0.37412448110277E+03 0.30115000786377E+03 0.30115001411235E+03
 0.34088388886312E+03 0.37409840229265E+03 0.30115000639906E+03 0.30115001406788E+03 0.42293663018734E+03
 0.33424369153555E+03 0.24618529118515E+04 0.22892720011311E+04 0.77288342444605E+03 0.12169922987548E+04
 0.44024445718647E+03 0.14593555649515E+04 0.14156921963676E+04 0.13321874908196E+04 0.21162940761200E+04
 0.13229062247566E+04 0.14152981673729E+04 0.12234599373310E+04 0.21161364183699E+04 0.14593555649515E+04
 0.14156921963676E+04 0.13321874908196E+04 0.21162940761200E+04 0.13229062247565E+04 0.14152981673729E+04
 0.12234599373310E+04 0.21161364183699E+04 0.20932508438708E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51523675985095E+03 0.14691358426344E+01
 0.14691358426344E+01 0.29169823913547E+01 0.00000000000000E+00 0.34691358010678E+03 0.34691358010678E+03
 0.34691358010678E+03 0.34691358010678E+03 0.00000000000000E+00 0.00000000000000E+00 0.15707837478346E+00
 0.00000000000000E+00 -.20715127021140E+02 0.10000000000000E-02 0.13315163837389E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60081874302860E+01 0.22530702863572E+01 0.33424342195264E+03 0.42293868977315E+03
 0.31481713635487E+03 0.31481713635487E+03 0.30115000193459E+03 0.30115000190455E+03 0.31481499430250E+03
 0.31481499430250E+03 0.30115000193510E+03 0.30115000190506E+03 0.31481713635487E+03 0.31481713635487E+03
 0.30115000193459E+03 0.30115000190455E+03 0.31481499430250E+03 0.31481499430250E+03 0.30115000193510E+03
 0.30115000190506E+03 0.31272067745207E+03 0.30115001372278E+03 0.78453752318436E+02 0.93723487462628E+02
 0.23407539714197E+03 0.54111502021274E+03 0.30586924608507E+03 0.17879562617532E+03 0.21885166234193E+03
 0.17879562617532E+03 0.43307043205422E+03 0.17880494053972E+03 0.21872311854185E+03 0.17880494053972E+03
 0.43295913158790E+03 0.17879562617532E+03 0.21885166234193E+03 0.17879562617532E+03 0.43307043205422E+03
 0.17880494053972E+03 0.21872311854185E+03 0.17880494053972E+03 0.43295913158790E+03 0.28226901540240E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37626424454423E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22280701238839E+00 0.00000000000000E+00 0.00000000000000E+00 0.22280701238839E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23295319057116E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23295319057116E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23205984407189E+00 0.23544295533392E+00 0.34691358010678E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    630.38739689
 0.10857941831370E+00 0.32099810674532E+03 0.47000316702027E+03 0.46461020609678E+03 0.46223730280175E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14518425087313E+00 0.00000000000000E+00 -.28135225901391E+02
 0.32202870873926E-02 0.11433871643933E+01 0.24842505599330E+04 0.93159395997486E+03 0.69967551229638E+01
 0.26237831711114E+01 0.35343798757418E+03 0.30115009216680E+03 0.34565595849798E+03 0.37478665039923E+03
 0.30115000935865E+03 0.30115001680451E+03 0.34131961336823E+03 0.37476075012418E+03 0.30115000762654E+03
 0.30115001675239E+03 0.34565595849798E+03 0.37478665039923E+03 0.30115000935865E+03 0.30115001680451E+03
 0.34131961336823E+03 0.37476075012418E+03 0.30115000762654E+03 0.30115001675239E+03 0.42367217909415E+03
 0.33499084866950E+03 0.24666166839287E+04 0.22920550710627E+04 0.77098741229324E+03 0.12114789776130E+04
 0.43663662825830E+03 0.14628526512978E+04 0.14181095687114E+04 0.13342343508901E+04 0.21162689056354E+04
 0.13267145625184E+04 0.14177200687684E+04 0.12260317106250E+04 0.21161138765251E+04 0.14628526512978E+04
 0.14181095687114E+04 0.13342343508902E+04 0.21162689056354E+04 0.13267145625184E+04 0.14177200687684E+04
 0.12260317106250E+04 0.21161138765252E+04 0.20926511969243E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51566080317144E+03 0.14691358612361E+01
 0.14691358612361E+01 0.29662225289754E+01 0.00000000000000E+00 0.34714757678168E+03 0.34714757678168E+03
 0.34714757678168E+03 0.34714757678168E+03 0.00000000000000E+00 0.00000000000000E+00 0.15636461260698E+00
 0.00000000000000E+00 -.20693739305415E+02 0.10000000000000E-02 0.13436661761727E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59538597769777E+01 0.22326974163666E+01 0.33499073197040E+03 0.42367385670083E+03
 0.31498327081602E+03 0.31498327081602E+03 0.30115000233340E+03 0.30115000227483E+03 0.31498106138392E+03
 0.31498106138392E+03 0.30115000233402E+03 0.30115000227544E+03 0.31498327081602E+03 0.31498327081602E+03
 0.30115000233340E+03 0.30115000227483E+03 0.31498106138392E+03 0.31498106138392E+03 0.30115000233402E+03
 0.30115000227544E+03 0.31286369423677E+03 0.30115001607496E+03 0.75116761492432E+02 0.89613154341541E+02
 0.23565617858889E+03 0.54361237169084E+03 0.30677791220900E+03 0.18161639930059E+03 0.22023674647385E+03
 0.18161639930059E+03 0.43494831343113E+03 0.18162609680962E+03 0.22010612859850E+03 0.18162609680962E+03
 0.43483548506203E+03 0.18161639930059E+03 0.22023674647385E+03 0.18161639930059E+03 0.43494831343113E+03
 0.18162609680962E+03 0.22010612859850E+03 0.18162609680962E+03 0.43483548506204E+03 0.28244508087282E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37653797201136E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22269569091285E+00 0.00000000000000E+00 0.00000000000000E+00 0.22269569091285E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23276816502011E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23276816502011E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206008464509E+00 0.23528454736162E+00 0.34714757678168E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    640.77317055
 0.10846547318768E+00 0.32124329953003E+03 0.47044944194421E+03 0.46505487033106E+03 0.46267975744298E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14483263898151E+00 0.00000000000000E+00 -.28141962583305E+02
 0.32236697137785E-02 0.11470415931802E+01 0.24816438128902E+04 0.93061642983383E+03 0.69744637400810E+01
 0.26154239025304E+01 0.35399195989946E+03 0.30115010814429E+03 0.34613618200102E+03 0.37547883331767E+03
 0.30115001121298E+03 0.30115002014509E+03 0.34177614296885E+03 0.37545311527599E+03 0.30115000915164E+03
 0.30115002008366E+03 0.34613618200102E+03 0.37547883331767E+03 0.30115001121298E+03 0.30115002014509E+03
 0.34177614296885E+03 0.37545311527599E+03 0.30115000915164E+03 0.30115002008366E+03 0.42443924948321E+03
 0.33577390200139E+03 0.24715540882747E+04 0.22948952583048E+04 0.76897216871996E+03 0.12057114041387E+04
 0.43289437457515E+03 0.14664921453490E+04 0.14205989082371E+04 0.13363358739387E+04 0.21162106020709E+04
 0.13306792866561E+04 0.14202139149542E+04 0.12286810398120E+04 0.21160581374769E+04 0.14664921453490E+04
 0.14205989082371E+04 0.13363358739387E+04 0.21162106020710E+04 0.13306792866561E+04 0.14202139149543E+04
 0.12286810398120E+04 0.21160581374769E+04 0.20919846488132E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51610296826443E+03 0.14691359155126E+01
 0.14691359155126E+01 0.30181513972965E+01 0.00000000000000E+00 0.34739691432807E+03 0.34739691432807E+03
 0.34739691432807E+03 0.34739691432807E+03 0.00000000000000E+00 0.00000000000000E+00 0.15563650612103E+00
 0.00000000000000E+00 -.20677693122501E+02 0.10000000000000E-02 0.13560013477898E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58996991507710E+01 0.22123871815391E+01 0.33577394147530E+03 0.42444055792956E+03
 0.31515841933285E+03 0.31515841933285E+03 0.30115000280641E+03 0.30115000273597E+03 0.31515613876790E+03
 0.31515613876790E+03 0.30115000280715E+03 0.30115000273669E+03 0.31515841933285E+03 0.31515841933285E+03
 0.30115000280641E+03 0.30115000273597E+03 0.31515613876790E+03 0.31515613876790E+03 0.30115000280715E+03
 0.30115000273669E+03 0.31301452862215E+03 0.30115001894171E+03 0.71593608072765E+02 0.85294636478444E+02
 0.23731997671862E+03 0.54627312120199E+03 0.30776654459977E+03 0.18459124209860E+03 0.22169458369087E+03
 0.18459124209860E+03 0.43695046339808E+03 0.18460134563571E+03 0.22156183863371E+03 0.18460134563571E+03
 0.43683608525765E+03 0.18459124209860E+03 0.22169458369087E+03 0.18459124209860E+03 0.43695046339808E+03
 0.18460134563571E+03 0.22156183863371E+03 0.18460134563571E+03 0.43683608525765E+03 0.28264767548588E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37683088390080E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22260828749931E+00 0.00000000000000E+00 0.00000000000000E+00 0.22260828749931E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23263090764089E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23263090764089E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206007886961E+00 0.23511570774282E+00 0.34739691432807E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    651.15894421
 0.10836190196041E+00 0.32148919027478E+03 0.47089287173971E+03 0.46549631604523E+03 0.46311890288420E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14449428752568E+00 0.00000000000000E+00 -.28144788454161E+02
 0.32267505195549E-02 0.11505176241268E+01 0.24792744129173E+04 0.92972790484397E+03 0.69533919622235E+01
 0.26075219858338E+01 0.35454203048872E+03 0.30115012638833E+03 0.34661334956124E+03 0.37616486312212E+03
 0.30115001337492E+03 0.30115002404084E+03 0.34223012025909E+03 0.37613932175153E+03 0.30115001093250E+03
 0.30115002396875E+03 0.34661334956124E+03 0.37616486312212E+03 0.30115001337492E+03 0.30115002404084E+03
 0.34223012025909E+03 0.37613932175153E+03 0.30115001093250E+03 0.30115002396875E+03 0.42519647312267E+03
 0.33655080839465E+03 0.24764264052911E+04 0.22976992091368E+04 0.76698107577390E+03 0.12000554620660E+04
 0.42923948091318E+03 0.14700942902357E+04 0.14230488348974E+04 0.13384238713538E+04 0.21161497524360E+04
 0.13346034380437E+04 0.14226680841578E+04 0.12313089065362E+04 0.21159996461323E+04 0.14700942902357E+04
 0.14230488348974E+04 0.13384238713538E+04 0.21161497524361E+04 0.13346034380437E+04 0.14226680841578E+04
 0.12313089065362E+04 0.21159996461323E+04 0.20913325026609E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51654181978976E+03 0.14691359382802E+01
 0.14691359382802E+01 0.30700802656177E+01 0.00000000000000E+00 0.34764866930860E+03 0.34764866930860E+03
 0.34764866930860E+03 0.34764866930860E+03 0.00000000000000E+00 0.00000000000000E+00 0.15493288795218E+00
 0.00000000000000E+00 -.20657314752218E+02 0.10000000000000E-02 0.13678620926882E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58485428046903E+01 0.21932035517589E+01 0.33655102087233E+03 0.42519741096883E+03
 0.31533380186447E+03 0.31533380186447E+03 0.30115000335997E+03 0.30115000327563E+03 0.31533145013480E+03
 0.31533145013480E+03 0.30115000336085E+03 0.30115000327649E+03 0.31533380186447E+03 0.31533380186447E+03
 0.30115000335997E+03 0.30115000327563E+03 0.31533145013480E+03 0.31533145013480E+03 0.30115000336085E+03
 0.30115000327649E+03 0.31316564743647E+03 0.30115002222828E+03 0.68075293855354E+02 0.81002886874057E+02
 0.23897852842319E+03 0.54894863308627E+03 0.30877521202096E+03 0.18756003353753E+03 0.22314742718103E+03
 0.18756003353753E+03 0.43896279933356E+03 0.18757054502897E+03 0.22301261379411E+03 0.18757054502897E+03
 0.43884693091384E+03 0.18756003353753E+03 0.22314742718103E+03 0.18756003353753E+03 0.43896279933356E+03
 0.18757054502897E+03 0.22301261379411E+03 0.18757054502897E+03 0.43884693091385E+03 0.28286330038018E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37712447747366E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22249639956282E+00 0.00000000000000E+00 0.00000000000000E+00 0.22249639956282E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23247668871295E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23247668871295E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206023684200E+00 0.23494565240292E+00 0.34764866930860E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    661.54471788
 0.10825787289840E+00 0.32173520670813E+03 0.47133349677754E+03 0.46593509922016E+03 0.46355549548223E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14416870241071E+00 0.00000000000000E+00 -.28144399211317E+02
 0.32298508820225E-02 0.11538223599985E+01 0.24768945354500E+04 0.92883545079375E+03 0.69334763108685E+01
 0.26000536165757E+01 0.35508826458920E+03 0.30115014714807E+03 0.34708750170672E+03 0.37684502330341E+03
 0.30115001588554E+03 0.30115002856586E+03 0.34268152815625E+03 0.37681965309829E+03 0.30115001300372E+03
 0.30115002848160E+03 0.34708750170671E+03 0.37684502330341E+03 0.30115001588554E+03 0.30115002856586E+03
 0.34268152815625E+03 0.37681965309829E+03 0.30115001300372E+03 0.30115002848160E+03 0.42594541825572E+03
 0.33732234985641E+03 0.24812450934169E+04 0.23004738989810E+04 0.76499424394694E+03 0.11944741680360E+04
 0.42565495286935E+03 0.14736640372849E+04 0.14254599900093E+04 0.13404969484187E+04 0.21160841877894E+04
 0.13384927957769E+04 0.14250832326693E+04 0.12339157886791E+04 0.21159362474475E+04 0.14736640372849E+04
 0.14254599900093E+04 0.13404969484187E+04 0.21160841877894E+04 0.13384927957769E+04 0.14250832326694E+04
 0.12339157886791E+04 0.21159362474475E+04 0.20906880794044E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51697808295768E+03 0.14691359351441E+01
 0.14691359351441E+01 0.31220091339389E+01 0.00000000000000E+00 0.34790183572233E+03 0.34790183572233E+03
 0.34790183572233E+03 0.34790183572233E+03 0.00000000000000E+00 0.00000000000000E+00 0.15425305631806E+00
 0.00000000000000E+00 -.20633405945723E+02 0.10000000000000E-02 0.13792660102410E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58001864329291E+01 0.21750699123484E+01 0.33732273775469E+03 0.42594600323800E+03
 0.31550923339879E+03 0.31550923339879E+03 0.30115000400516E+03 0.30115000390463E+03 0.31550681058439E+03
 0.31550681058439E+03 0.30115000400621E+03 0.30115000390565E+03 0.31550923339879E+03 0.31550923339879E+03
 0.30115000400516E+03 0.30115000390463E+03 0.31550681058439E+03 0.31550681058439E+03 0.30115000400621E+03
 0.30115000390565E+03 0.31331688381597E+03 0.30115002598262E+03 0.64550481983600E+02 0.76723884303862E+02
 0.24062737593635E+03 0.55162623954746E+03 0.30979572673142E+03 0.19052293288911E+03 0.22459071571763E+03
 0.19052293288911E+03 0.44097480791107E+03 0.19053385351961E+03 0.22445387734425E+03 0.19053385351961E+03
 0.44085749224018E+03 0.19052293288911E+03 0.22459071571763E+03 0.19052293288911E+03 0.44097480791107E+03
 0.19053385351961E+03 0.22445387734425E+03 0.19053385351961E+03 0.44085749224019E+03 0.28308239421713E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37741809468621E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22236431855347E+00 0.00000000000000E+00 0.00000000000000E+00 0.22236431855347E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23230959085716E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23230959085716E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206052985768E+00 0.23477503550617E+00 0.34790183572233E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    671.93049154
 0.10814743725360E+00 0.32198095203319E+03 0.47177149901855E+03 0.46637167775812E+03 0.46399009589937E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14385525099440E+00 0.00000000000000E+00 -.28141160511111E+02
 0.32331487287729E-02 0.11569643897148E+01 0.24743680761746E+04 0.92788802856546E+03 0.69146467005539E+01
 0.25929925127077E+01 0.35563077403814E+03 0.30115017069163E+03 0.34755871632354E+03 0.37751949278557E+03
 0.30115001879006E+03 0.30115003380162E+03 0.34313042690363E+03 0.37749428841275E+03 0.30115001540345E+03
 0.30115003370353E+03 0.34755871632354E+03 0.37751949278557E+03 0.30115001879006E+03 0.30115003380162E+03
 0.34313042690363E+03 0.37749428841275E+03 0.30115001540345E+03 0.30115003370353E+03 0.42668659885929E+03
 0.33808863647667E+03 0.24860198313030E+04 0.23032254545399E+04 0.76301072793130E+03 0.11889626701442E+04
 0.42213688857329E+03 0.14772061337091E+04 0.14278366770421E+04 0.13425554147385E+04 0.21160177256229E+04
 0.13423524260356E+04 0.14274636825409E+04 0.12365028532550E+04 0.21158717755746E+04 0.14772061337092E+04
 0.14278366770421E+04 0.13425554147385E+04 0.21160177256230E+04 0.13423524260356E+04 0.14274636825409E+04
 0.12365028532550E+04 0.21158717755747E+04 0.20900548816503E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51741230374883E+03 0.14691359090504E+01
 0.14691359090504E+01 0.31739380022600E+01 0.00000000000000E+00 0.34815569555293E+03 0.34815569555293E+03
 0.34815569555293E+03 0.34815569555293E+03 0.00000000000000E+00 0.00000000000000E+00 0.15359632484782E+00
 0.00000000000000E+00 -.20606330513023E+02 0.10000000000000E-02 0.13902293659816E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57544461336790E+01 0.21579173001296E+01 0.33808919628122E+03 0.42668685416026E+03
 0.31568463019944E+03 0.31568463019944E+03 0.30115000475426E+03 0.30115000463493E+03 0.31568213640438E+03
 0.31568213640438E+03 0.30115000475550E+03 0.30115000463613E+03 0.31568463019944E+03 0.31568463019944E+03
 0.30115000475426E+03 0.30115000463493E+03 0.31568213640438E+03 0.31568213640438E+03 0.30115000475550E+03
 0.30115000463613E+03 0.31346816624272E+03 0.30115003025655E+03 0.61014929250559E+02 0.72452178852031E+02
 0.24226315711683E+03 0.55429603558706E+03 0.31082156268465E+03 0.19347828870246E+03 0.22602106348647E+03
 0.19347828870246E+03 0.44297828986468E+03 0.19348961953006E+03 0.22588223606392E+03 0.19348961953006E+03
 0.44285956230695E+03 0.19347828870246E+03 0.22602106348647E+03 0.19347828870246E+03 0.44297828986468E+03
 0.19348961953006E+03 0.22588223606392E+03 0.19348961953006E+03 0.44285956230695E+03 0.28329881111786E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37771118343248E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22221423522481E+00 0.00000000000000E+00 0.00000000000000E+00 0.22221423522481E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23213023980058E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23213023980058E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206094555084E+00 0.23460433084215E+00 0.34815569555293E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    682.32216074
 0.10803245031258E+00 0.32222224370305E+03 0.47220630708951E+03 0.46680525846435E+03 0.46442172862571E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14355312021755E+00 0.00000000000000E+00 -.28142878672726E+02
 0.32365896689527E-02 0.11599564480945E+01 0.24717374824312E+04 0.92690155591171E+03 0.68968106631431E+01
 0.25863039986787E+01 0.35616867684833E+03 0.30115019771140E+03 0.34802601909318E+03 0.37818865323833E+03
 0.30115002219768E+03 0.30115003994499E+03 0.34357554817190E+03 0.37816360837923E+03 0.30115001822347E+03
 0.30115003983100E+03 0.34802601909318E+03 0.37818865323833E+03 0.30115002219768E+03 0.30115003994499E+03
 0.34357554817190E+03 0.37816360837923E+03 0.30115001822347E+03 0.30115003983100E+03 0.42742763798085E+03
 0.33885548944456E+03 0.24907509522076E+04 0.23059291445842E+04 0.76090174606708E+03 0.11833007338090E+04
 0.41859447901155E+03 0.14807210840460E+04 0.14301676778018E+04 0.13445781254538E+04 0.21159307363997E+04
 0.13461848599428E+04 0.14297982067441E+04 0.12390539794895E+04 0.21157866002465E+04 0.14807210840460E+04
 0.14301676778018E+04 0.13445781254538E+04 0.21159307363997E+04 0.13461848599427E+04 0.14297982067441E+04
 0.12390539794895E+04 0.21157866002465E+04 0.20893620406004E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51784357847594E+03 0.14691359228934E+01
 0.14691359228934E+01 0.32258963482409E+01 0.00000000000000E+00 0.34840984323125E+03 0.34840984323125E+03
 0.34840984323125E+03 0.34840984323125E+03 0.00000000000000E+00 0.00000000000000E+00 0.15296169421100E+00
 0.00000000000000E+00 -.20583946564648E+02 0.10000000000000E-02 0.14007728111806E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57111331231920E+01 0.21416749211970E+01 0.33885620100937E+03 0.42742760667301E+03
 0.31585887582781E+03 0.31585887582781E+03 0.30115000570556E+03 0.30115000549513E+03 0.31585631171134E+03
 0.31585631171134E+03 0.30115000570704E+03 0.30115000549654E+03 0.31585887582781E+03 0.31585887582781E+03
 0.30115000570556E+03 0.30115000549513E+03 0.31585631171134E+03 0.31585631171134E+03 0.30115000570704E+03
 0.30115000549654E+03 0.31361846670683E+03 0.30115003518195E+03 0.57420056621404E+02 0.68130490695364E+02
 0.24389240965355E+03 0.55697152460235E+03 0.31185965290053E+03 0.19645200187686E+03 0.22744453898738E+03
 0.19645200187686E+03 0.44498514772344E+03 0.19646373958497E+03 0.22730368042701E+03 0.19646373958497E+03
 0.44486496125630E+03 0.19645200187687E+03 0.22744453898738E+03 0.19645200187687E+03 0.44498514772344E+03
 0.19646373958497E+03 0.22730368042701E+03 0.19646373958497E+03 0.44486496125630E+03 0.28350560561243E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37800329935464E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22209779517350E+00 0.00000000000000E+00 0.00000000000000E+00 0.22209779517350E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23193895665837E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23193895665837E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206117848795E+00 0.23443348576926E+00 0.34840984323125E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    696.18165002
 0.10789330260029E+00 0.32253864400558E+03 0.47278115052345E+03 0.46737776378373E+03 0.46499130000102E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14316731483199E+00 0.00000000000000E+00 -.28147653359957E+02
 0.32407633833250E-02 0.11637209732067E+01 0.24685541811424E+04 0.92570781792839E+03 0.68745001458170E+01
 0.25779375546814E+01 0.35688076113270E+03 0.30115023948163E+03 0.34864516549395E+03 0.37907248850710E+03
 0.30115002759410E+03 0.30115004967439E+03 0.34416590893716E+03 0.37904764924687E+03 0.30115002269742E+03
 0.30115004953576E+03 0.34864516549395E+03 0.37907248850710E+03 0.30115002759410E+03 0.30115004967439E+03
 0.34416590893716E+03 0.37904764924687E+03 0.30115002269742E+03 0.30115004953576E+03 0.42839939878631E+03
 0.33986673595063E+03 0.24969544587005E+04 0.23094202707046E+04 0.75815317867540E+03 0.11759543743852E+04
 0.41401042981644E+03 0.14853481326001E+04 0.14332055006231E+04 0.13472036057848E+04 0.21157860123657E+04
 0.13512301312344E+04 0.14328404362724E+04 0.12423741666961E+04 0.21156440767625E+04 0.14853481326001E+04
 0.14332055006231E+04 0.13472036057848E+04 0.21157860123657E+04 0.13512301312344E+04 0.14328404362724E+04
 0.12423741666961E+04 0.21156440767625E+04 0.20884388724235E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51841276305908E+03 0.14691359613623E+01
 0.14691359613623E+01 0.32951937946552E+01 0.00000000000000E+00 0.34875026492091E+03 0.34875026492091E+03
 0.34875026492091E+03 0.34875026492091E+03 0.00000000000000E+00 0.00000000000000E+00 0.15214891520677E+00
 0.00000000000000E+00 -.20559564860355E+02 0.10000000000000E-02 0.14141925592519E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56569382632248E+01 0.21213518487093E+01 0.33986765116717E+03 0.42839900044322E+03
 0.31609164322917E+03 0.31609164322917E+03 0.30115000712611E+03 0.30115000686327E+03 0.31608898482787E+03
 0.31608898482787E+03 0.30115000712793E+03 0.30115000686503E+03 0.31609164322917E+03 0.31609164322917E+03
 0.30115000712611E+03 0.30115000686327E+03 0.31608898482787E+03 0.31608898482787E+03 0.30115000712793E+03
 0.30115000686503E+03 0.31381937185195E+03 0.30115004283059E+03 0.52645300529905E+02 0.62419963423214E+02
 0.24604970987982E+03 0.56054013883370E+03 0.31326018040448E+03 0.20039579758276E+03 0.22932783239780E+03
 0.20039579758276E+03 0.44766087854768E+03 0.20040808412903E+03 0.22918435741552E+03 0.20040808412903E+03
 0.44753884389053E+03 0.20039579758276E+03 0.22932783239780E+03 0.20039579758276E+03 0.44766087854768E+03
 0.20040808412903E+03 0.22918435741552E+03 0.20040808412903E+03 0.44753884389054E+03 0.28379404394711E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37839703193497E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22196095124816E+00 0.00000000000000E+00 0.00000000000000E+00 0.22196095124816E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23176293824316E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23176293824316E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206127353404E+00 0.23420480314787E+00 0.34875026492091E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    703.11139466
 0.10782507190888E+00 0.32269861646895E+03 0.47306721918699E+03 0.46766271738669E+03 0.46527487435575E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14298150240013E+00 0.00000000000000E+00 -.28145938077863E+02
 0.32428138855769E-02 0.11655081974092E+01 0.24669932602613E+04 0.92512247259800E+03 0.68639585871496E+01
 0.25739844701811E+01 0.35723452794388E+03 0.30115026295188E+03 0.34895295745104E+03 0.37951072252902E+03
 0.30115003068425E+03 0.30115005524575E+03 0.34445961699705E+03 0.37948598298480E+03 0.30115002526296E+03
 0.30115005509324E+03 0.34895295745104E+03 0.37951072252902E+03 0.30115003068425E+03 0.30115005524575E+03
 0.34445961699705E+03 0.37948598298480E+03 0.30115002526296E+03 0.30115005509324E+03 0.42887927493171E+03
 0.34036801613432E+03 0.25000311555662E+04 0.23111681997647E+04 0.75680722957766E+03 0.11723665473255E+04
 0.41177528159995E+03 0.14876464543800E+04 0.14347114943298E+04 0.13485235262880E+04 0.21157254218090E+04
 0.13537356673846E+04 0.14343485085951E+04 0.12440363669351E+04 0.21155844937750E+04 0.14876464543800E+04
 0.14347114943298E+04 0.13485235262880E+04 0.21157254218090E+04 0.13537356673846E+04 0.14343485085952E+04
 0.12440363669351E+04 0.21155844937750E+04 0.20880049585566E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51869610355173E+03 0.14691359475425E+01
 0.14691359475425E+01 0.33298425178623E+01 0.00000000000000E+00 0.34892101925020E+03 0.34892101925020E+03
 0.34892101925020E+03 0.34892101925020E+03 0.00000000000000E+00 0.00000000000000E+00 0.15175642590453E+00
 0.00000000000000E+00 -.20542597425433E+02 0.10000000000000E-02 0.14206375868863E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56312743474104E+01 0.21117278802789E+01 0.34036903980651E+03 0.42887869067385E+03
 0.31620812938015E+03 0.31620812938015E+03 0.30115000794232E+03 0.30115000764938E+03 0.31620542383296E+03
 0.31620542383296E+03 0.30115000794433E+03 0.30115000765132E+03 0.31620812938015E+03 0.31620812938015E+03
 0.30115000794232E+03 0.30115000764938E+03 0.31620542383296E+03 0.31620542383296E+03 0.30115000794433E+03
 0.30115000765132E+03 0.31391997225255E+03 0.30115004714347E+03 0.50261845161835E+02 0.59582023072267E+02
 0.24712217178556E+03 0.56232030630083E+03 0.31396252365635E+03 0.20236015430113E+03 0.23026333249879E+03
 0.20236015430113E+03 0.44899408943406E+03 0.20237271651596E+03 0.23011857228635E+03 0.20237271651596E+03
 0.44887115378186E+03 0.20236015430113E+03 0.23026333249879E+03 0.20236015430113E+03 0.44899408943406E+03
 0.20237271651596E+03 0.23011857228636E+03 0.20237271651596E+03 0.44887115378186E+03 0.28394003360150E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37859303174656E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22186597575348E+00 0.00000000000000E+00 0.00000000000000E+00 0.22186597575348E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23165330706209E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23165330706209E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206150660165E+00 0.23409046275672E+00 0.34892101925020E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    710.04113930
 0.10775258643286E+00 0.32285946035742E+03 0.47335241550855E+03 0.46794708045607E+03 0.46555801160200E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14280021254707E+00 0.00000000000000E+00 -.28142241257458E+02
 0.32449951136696E-02 0.11672348835916E+01 0.24653349911992E+04 0.92450062169969E+03 0.68538047589737E+01
 0.25701767846151E+01 0.35758682213768E+03 0.30115028828894E+03 0.34925960080564E+03 0.37994662119019E+03
 0.30115003406229E+03 0.30115006133598E+03 0.34475235951500E+03 0.37992197933927E+03 0.30115002807015E+03
 0.30115006116850E+03 0.34925960080564E+03 0.37994662119019E+03 0.30115003406229E+03 0.30115006133598E+03
 0.34475235951500E+03 0.37992197933927E+03 0.30115002807015E+03 0.30115006116850E+03 0.42935584812985E+03
 0.34086672402183E+03 0.25030964852351E+04 0.23129205017023E+04 0.75547096530831E+03 0.11688190199886E+04
 0.40957069985376E+03 0.14899369085424E+04 0.14362092444420E+04 0.13498469755080E+04 0.21156719542815E+04
 0.13562324527583E+04 0.14358482586764E+04 0.12456996015364E+04 0.21155319760225E+04 0.14899369085424E+04
 0.14362092444420E+04 0.13498469755080E+04 0.21156719542815E+04 0.13562324527583E+04 0.14358482586764E+04
 0.12456996015364E+04 0.21155319760225E+04 0.20875865732773E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51897896510841E+03 0.14691359177578E+01
 0.14691359177578E+01 0.33644912410695E+01 0.00000000000000E+00 0.34909172421952E+03 0.34909172421952E+03
 0.34909172421952E+03 0.34909172421952E+03 0.00000000000000E+00 0.00000000000000E+00 0.15137296182268E+00
 0.00000000000000E+00 -.20523301244463E+02 0.10000000000000E-02 0.14269124405232E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56065107940799E+01 0.21024415477800E+01 0.34086785516682E+03 0.42935508393071E+03
 0.31632460985343E+03 0.31632460985343E+03 0.30115000883657E+03 0.30115000851065E+03 0.31632185719506E+03
 0.31632185719506E+03 0.30115000883880E+03 0.30115000851280E+03 0.31632460985343E+03 0.31632460985343E+03
 0.30115000883657E+03 0.30115000851065E+03 0.31632185719506E+03 0.31632185719506E+03 0.30115000883880E+03
 0.30115000851280E+03 0.31402060467199E+03 0.30115005181023E+03 0.47875937738926E+02 0.56749569402990E+02
 0.24818828376542E+03 0.56409224663888E+03 0.31466302145464E+03 0.20431939931143E+03 0.23119248877555E+03
 0.20431939931143E+03 0.45031964359011E+03 0.20433223777992E+03 0.23104645380746E+03 0.20433223777992E+03
 0.45019581717640E+03 0.20431939931143E+03 0.23119248877555E+03 0.20431939931143E+03 0.45031964359011E+03
 0.20433223777992E+03 0.23104645380746E+03 0.20433223777992E+03 0.45019581717641E+03 0.28408306975744E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37878827460123E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22175811907901E+00 0.00000000000000E+00 0.00000000000000E+00 0.22175811907901E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23153272717701E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23153272717701E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206183103066E+00 0.23397636480099E+00 0.34909172421952E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    723.90062859
 0.10758961396097E+00 0.32318140365505E+03 0.47392018740050E+03 0.46851421154979E+03 0.46612317130054E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14245039563349E+00 0.00000000000000E+00 -.28131932467574E+02
 0.32499100589617E-02 0.11705178606655E+01 0.24616065844468E+04 0.92310246916754E+03 0.68345817426929E+01
 0.25629681535098E+01 0.35828716133837E+03 0.30115034502034E+03 0.34986956732708E+03 0.38081170251616E+03
 0.30115004176848E+03 0.30115007522835E+03 0.34533504740396E+03 0.38078725019305E+03 0.30115003448300E+03
 0.30115007502731E+03 0.34986956732708E+03 0.38081170251616E+03 0.30115004176848E+03 0.30115007522835E+03
 0.34533504740396E+03 0.38078725019305E+03 0.30115003448300E+03 0.30115007502731E+03 0.43029984696891E+03
 0.34185662599942E+03 0.25091985159503E+04 0.23164248344306E+04 0.75281902907840E+03 0.11618276588380E+04
 0.40524453461418E+03 0.14944968490004E+04 0.14391796581267E+04 0.13524888198542E+04 0.21155829589980E+04
 0.13612028140277E+04 0.14388224552698E+04 0.12490152615127E+04 0.21154447234342E+04 0.14944968490004E+04
 0.14391796581267E+04 0.13524888198542E+04 0.21155829589980E+04 0.13612028140277E+04 0.14388224552698E+04
 0.12490152615127E+04 0.21154447234342E+04 0.20867875718798E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51954346607258E+03 0.14691358347013E+01
 0.14691358347013E+01 0.34337886874838E+01 0.00000000000000E+00 0.34943224409232E+03 0.34943224409232E+03
 0.34943224409232E+03 0.34943224409232E+03 0.00000000000000E+00 0.00000000000000E+00 0.15063239719757E+00
 0.00000000000000E+00 -.20481238360364E+02 0.10000000000000E-02 0.14389685979144E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55595375824009E+01 0.20848265934003E+01 0.34185796324492E+03 0.43029874803823E+03
 0.31655742249746E+03 0.31655742249746E+03 0.30115001088341E+03 0.30115001048200E+03 0.31655457570515E+03
 0.31655457570515E+03 0.30115001088613E+03 0.30115001048461E+03 0.31655742249746E+03 0.31655742249746E+03
 0.30115001088341E+03 0.30115001048200E+03 0.31655457570515E+03 0.31655457570515E+03 0.30115001088613E+03
 0.30115001048461E+03 0.31422185077662E+03 0.30115006229544E+03 0.43091212429888E+02 0.51094424170982E+02
 0.25029760435165E+03 0.56760235217400E+03 0.31605325980059E+03 0.20822106815234E+03 0.23302793823654E+03
 0.20822106815234E+03 0.45294044491858E+03 0.20823446099816E+03 0.23287937885095E+03 0.20823446099816E+03
 0.45281486148867E+03 0.20822106815234E+03 0.23302793823654E+03 0.20822106815234E+03 0.45294044491858E+03
 0.20823446099816E+03 0.23287937885095E+03 0.20823446099816E+03 0.45281486148867E+03 0.28435428978678E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37917655978552E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22152361351380E+00 0.00000000000000E+00 0.00000000000000E+00 0.22152361351380E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23127317032994E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23127317032994E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206261871467E+00 0.23374924656892E+00 0.34943224409232E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    730.83037323
 0.10749984021660E+00 0.32334197932135E+03 0.47420279875984E+03 0.46879696076498E+03 0.46640513936325E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14228145586586E+00 0.00000000000000E+00 -.28125951665444E+02
 0.32526238559016E-02 0.11720797964189E+01 0.24595527655264E+04 0.92233228707240E+03 0.68254738495133E+01
 0.25595526935675E+01 0.35863526036675E+03 0.30115037665452E+03 0.35017292038951E+03 0.38124098629410E+03
 0.30115004614277E+03 0.30115008311328E+03 0.34562501564822E+03 0.38121662590493E+03 0.30115003812797E+03
 0.30115008289351E+03 0.35017292038951E+03 0.38124098629410E+03 0.30115004614277E+03 0.30115008311328E+03
 0.34562501564822E+03 0.38121662590493E+03 0.30115003812797E+03 0.30115008289351E+03 0.43076748439782E+03
 0.34234784875173E+03 0.25122361250209E+04 0.23181734901941E+04 0.75150238855085E+03 0.11583806681002E+04
 0.40312076760658E+03 0.14967671069202E+04 0.14406532122239E+04 0.13538043416968E+04 0.21155476451141E+04
 0.13636771695641E+04 0.14402978007888E+04 0.12506650371001E+04 0.21154102099000E+04 0.14967671069202E+04
 0.14406532122239E+04 0.13538043416968E+04 0.21155476451141E+04 0.13636771695640E+04 0.14402978007888E+04
 0.12506650371001E+04 0.21154102099001E+04 0.20864053291670E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.51982506269390E+03 0.14691357865149E+01
 0.14691357865149E+01 0.34684374106909E+01 0.00000000000000E+00 0.34960192473435E+03 0.34960192473435E+03
 0.34960192473435E+03 0.34960192473435E+03 0.00000000000000E+00 0.00000000000000E+00 0.15027492494525E+00
 0.00000000000000E+00 -.20459210635605E+02 0.10000000000000E-02 0.14447577162480E+01 0.80000000000000E+04
 0.30000000000000E+04 0.55372606147249E+01 0.20764727305218E+01 0.34234928430243E+03 0.43076623021373E+03
 0.31667370876451E+03 0.31667370876451E+03 0.30115001204896E+03 0.30115001160455E+03 0.31667081494075E+03
 0.31667081494075E+03 0.30115001205194E+03 0.30115001160743E+03 0.31667370876451E+03 0.31667370876451E+03
 0.30115001204896E+03 0.30115001160455E+03 0.31667081494075E+03 0.31667081494075E+03 0.30115001205194E+03
 0.30115001160743E+03 0.31432242486268E+03 0.30115006816134E+03 0.40691996190270E+02 0.48271205410543E+02
 0.25134055309981E+03 0.56933955380322E+03 0.31674229793791E+03 0.21016337341358E+03 0.23393398890879E+03
 0.21016337341358E+03 0.45423503092472E+03 0.21017704443353E+03 0.23378417754121E+03 0.21017704443353E+03
 0.45410857899385E+03 0.21016337341358E+03 0.23393398890879E+03 0.21016337341358E+03 0.45423503092472E+03
 0.21017704443353E+03 0.23378417754121E+03 0.21017704443353E+03 0.45410857899385E+03 0.28448191725701E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37936964326004E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22140112906929E+00 0.00000000000000E+00 0.00000000000000E+00 0.22140112906929E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23113725458037E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23113725458037E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206305347994E+00 0.23363628400289E+00 0.34960192473435E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    744.68986251
 0.10730780317577E+00 0.32366219526985E+03 0.47476551854814E+03 0.46936066332362E+03 0.46696759745732E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14195458187682E+00 0.00000000000000E+00 -.28112816508908E+02
 0.32584442777493E-02 0.11750574020102E+01 0.24551593699573E+04 0.92068476373399E+03 0.68081780399105E+01
 0.25530667649665E+01 0.35932742144465E+03 0.30115044709435E+03 0.35077642599963E+03 0.38209324218569E+03
 0.30115005605819E+03 0.30115010098383E+03 0.34620224667182E+03 0.38206906024377E+03 0.30115004640123E+03
 0.30115010072235E+03 0.35077642599963E+03 0.38209324218569E+03 0.30115005605819E+03 0.30115010098383E+03
 0.34620224667182E+03 0.38206906024377E+03 0.30115004640123E+03 0.30115010072235E+03 0.43169441664766E+03
 0.34332286590707E+03 0.25182839582677E+04 0.23216615494044E+04 0.74888636533095E+03 0.11515783708588E+04
 0.39894757370119E+03 0.15012887275345E+04 0.14435785229743E+04 0.13564247088696E+04 0.21154954410412E+04
 0.13686046597609E+04 0.14432265125349E+04 0.12539484745503E+04 0.21153594802674E+04 0.15012887275345E+04
 0.14435785229743E+04 0.13564247088696E+04 0.21154954410412E+04 0.13686046597609E+04 0.14432265125349E+04
 0.12539484745503E+04 0.21153594802674E+04 0.20856728195629E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52038671223599E+03 0.14691356806868E+01
 0.14691356806868E+01 0.35377348571052E+01 0.00000000000000E+00 0.34994005715398E+03 0.34994005715398E+03
 0.34994005715398E+03 0.34994005715398E+03 0.00000000000000E+00 0.00000000000000E+00 0.14958464784959E+00
 0.00000000000000E+00 -.20413745482424E+02 0.10000000000000E-02 0.14558768735157E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54949701760710E+01 0.20606138160266E+01 0.34332448969227E+03 0.43169287239985E+03
 0.31690597080385E+03 0.31690597080385E+03 0.30115001469935E+03 0.30115001415720E+03 0.31690298296955E+03
 0.31690298296955E+03 0.30115001470295E+03 0.30115001416066E+03 0.31690597080385E+03 0.31690597080385E+03
 0.30115001469935E+03 0.30115001415720E+03 0.31690298296955E+03 0.31690298296955E+03 0.30115001470295E+03
 0.30115001416066E+03 0.31452341404856E+03 0.30115008126560E+03 0.35881216412919E+02 0.42634980176973E+02
 0.25340411072491E+03 0.57277948991489E+03 0.31810835863636E+03 0.21403131186933E+03 0.23572384838546E+03
 0.21403131186933E+03 0.45679391518968E+03 0.21404554130495E+03 0.23557154890800E+03 0.21404554130495E+03
 0.45666574182530E+03 0.21403131186933E+03 0.23572384838546E+03 0.21403131186933E+03 0.45679391518968E+03
 0.21404554130495E+03 0.23557154890800E+03 0.21404554130495E+03 0.45666574182530E+03 0.28472226657523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37975376907306E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22114891399619E+00 0.00000000000000E+00 0.00000000000000E+00 0.22114891399619E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23085581804366E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23085581804366E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206398228367E+00 0.23341157003474E+00 0.34994005715398E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    751.61960715
 0.10720688200109E+00 0.32382184056029E+03 0.47504561525183E+03 0.46964153879546E+03 0.46724798240340E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14179632349237E+00 0.00000000000000E+00 -.28105802439139E+02
 0.32615114532216E-02 0.11764776143389E+01 0.24528505003709E+04 0.91981893763909E+03 0.67999593893639E+01
 0.25499847710114E+01 0.35967152367599E+03 0.30115048616801E+03 0.35107660732110E+03 0.38251628660085E+03
 0.30115006165290E+03 0.30115011106544E+03 0.34648953259144E+03 0.38249219126617E+03 0.30115005107529E+03
 0.30115011078081E+03 0.35107660732110E+03 0.38251628660085E+03 0.30115006165290E+03 0.30115011106544E+03
 0.34648953259144E+03 0.38249219126617E+03 0.30115005107529E+03 0.30115011078081E+03 0.43215383444706E+03
 0.34380666943528E+03 0.25212934623251E+04 0.23234000503629E+04 0.74758613371817E+03 0.11482205279597E+04
 0.39689646357295E+03 0.15035399388855E+04 0.14450302335950E+04 0.13577295808644E+04 0.21154777464642E+04
 0.13710576206359E+04 0.14446798391798E+04 0.12555821200098E+04 0.21153424652530E+04 0.15035399388855E+04
 0.14450302335950E+04 0.13577295808644E+04 0.21154777464642E+04 0.13710576206359E+04 0.14446798391798E+04
 0.12555821200098E+04 0.21153424652530E+04 0.20853210429336E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52066666507934E+03 0.14691356241755E+01
 0.14691356241755E+01 0.35723835803124E+01 0.00000000000000E+00 0.35010850295190E+03 0.35010850295190E+03
 0.35010850295190E+03 0.35010850295190E+03 0.00000000000000E+00 0.00000000000000E+00 0.14925146097225E+00
 0.00000000000000E+00 -.20390484903417E+02 0.10000000000000E-02 0.14612143511808E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54748983224364E+01 0.20530868709136E+01 0.34380838356085E+03 0.43215215413152E+03
 0.31702192891688E+03 0.31702192891688E+03 0.30115001619935E+03 0.30115001560187E+03 0.31701889409529E+03
 0.31701889409529E+03 0.30115001620329E+03 0.30115001560566E+03 0.31702192891688E+03 0.31702192891688E+03
 0.30115001619935E+03 0.30115001560187E+03 0.31701889409529E+03 0.31701889409529E+03 0.30115001620329E+03
 0.30115001560566E+03 0.31462381353810E+03 0.30115008855733E+03 0.33470364556549E+02 0.39822797323488E+02
 0.25442523639503E+03 0.57448291912880E+03 0.31878555655180E+03 0.21595710903056E+03 0.23660817636343E+03
 0.21595710903056E+03 0.45805890050915E+03 0.21597161875269E+03 0.23645463925279E+03 0.21597161875269E+03
 0.45792987277524E+03 0.21595710903056E+03 0.23660817636343E+03 0.21595710903056E+03 0.45805890050915E+03
 0.21597161875269E+03 0.23645463925279E+03 0.21597161875269E+03 0.45792987277524E+03 0.28483555877323E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.37994484903274E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22102013438474E+00 0.00000000000000E+00 0.00000000000000E+00 0.22102013438474E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23071122920794E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23071122920794E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206446943280E+00 0.23329981332542E+00 0.35010850295190E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    765.47909644
 0.10699777345022E+00 0.32414013608559E+03 0.47560322042094E+03 0.47020114949301E+03 0.46780682203680E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14148945045749E+00 0.00000000000000E+00 -.28091122575131E+02
 0.32678850692487E-02 0.11791909602755E+01 0.24480665110536E+04 0.91802494164511E+03 0.67843125240133E+01
 0.25441171965050E+01 0.36035586710273E+03 0.30115057272897E+03 0.35167390118686E+03 0.38335637162792E+03
 0.30115007426023E+03 0.30115013377886E+03 0.34706150570629E+03 0.38333244450357E+03 0.30115006162135E+03
 0.30115013344295E+03 0.35167390118686E+03 0.38335637162792E+03 0.30115007426023E+03 0.30115013377886E+03
 0.34706150570629E+03 0.38333244450357E+03 0.30115006162135E+03 0.30115013344295E+03 0.43306482017976E+03
 0.34476688844949E+03 0.25272815910251E+04 0.23268628791605E+04 0.74499910868657E+03 0.11415854925852E+04
 0.39286138835515E+03 0.15080224750007E+04 0.14479109060110E+04 0.13603276926913E+04 0.21154562806681E+04
 0.13759412647744E+04 0.14475635891798E+04 0.12588322471470E+04 0.21153222553629E+04 0.15080224750007E+04
 0.14479109060110E+04 0.13603276926913E+04 0.21154562806681E+04 0.13759412647744E+04 0.14475635891798E+04
 0.12588322471470E+04 0.21153222553629E+04 0.20846421603278E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52122459537206E+03 0.14691355059020E+01
 0.14691355059020E+01 0.36416810267267E+01 0.00000000000000E+00 0.35044415586160E+03 0.35044415586160E+03
 0.35044415586160E+03 0.35044415586160E+03 0.00000000000000E+00 0.00000000000000E+00 0.14860804345320E+00
 0.00000000000000E+00 -.20343226899704E+02 0.10000000000000E-02 0.14714634242841E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54367644264703E+01 0.20387866599263E+01 0.34476877634991E+03 0.43306288348183E+03
 0.31725346904141E+03 0.31725346904141E+03 0.30115001958974E+03 0.30115001886720E+03 0.31725034025845E+03
 0.31725034025845E+03 0.30115001959444E+03 0.30115001887173E+03 0.31725346904141E+03 0.31725346904141E+03
 0.30115001958974E+03 0.30115001886720E+03 0.31725034025845E+03 0.31725034025845E+03 0.30115001959444E+03
 0.30115001887173E+03 0.31482439487063E+03 0.30115010476105E+03 0.28639598504922E+02 0.34212519905511E+02
 0.25644745341921E+03 0.57785843430423E+03 0.32012874361793E+03 0.21979272092473E+03 0.23835689964051E+03
 0.21979272092473E+03 0.46056163856912E+03 0.21980779350798E+03 0.23820089698544E+03 0.21980779350798E+03
 0.46043091185684E+03 0.21979272092473E+03 0.23835689964051E+03 0.21979272092473E+03 0.46056163856912E+03
 0.21980779350798E+03 0.23820089698544E+03 0.21980779350798E+03 0.46043091185684E+03 0.28504983337560E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38032512876045E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22075888396405E+00 0.00000000000000E+00 0.00000000000000E+00 0.22075888396405E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23041628594954E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23041628594954E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206547675533E+00 0.23307748091163E+00 0.35044415586160E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    772.40884108
 0.10691851617005E+00 0.32429379543032E+03 0.47588035221069E+03 0.47047788230326E+03 0.46808248019136E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14134079454952E+00 0.00000000000000E+00 -.28119381086613E+02
 0.32703072932041E-02 0.11804853194258E+01 0.24462532975493E+04 0.91734498658097E+03 0.67768737724678E+01
 0.25413276646754E+01 0.36069607066950E+03 0.30115062051345E+03 0.35197096860329E+03 0.38377345150375E+03
 0.30115008133450E+03 0.30115014652092E+03 0.34734614280877E+03 0.38374960604114E+03 0.30115006754618E+03
 0.30115014615672E+03 0.35197096860329E+03 0.38377345150375E+03 0.30115008133450E+03 0.30115014652092E+03
 0.34734614280877E+03 0.38374960604114E+03 0.30115006754618E+03 0.30115014615672E+03 0.43351642028954E+03
 0.34524338698203E+03 0.25302292014801E+04 0.23285180834241E+04 0.74370389587332E+03 0.11382940218230E+04
 0.39087160647026E+03 0.15102405929893E+04 0.14493312476380E+04 0.13615805839473E+04 0.21154412144493E+04
 0.13783576142765E+04 0.14489853816775E+04 0.12604076827610E+04 0.21153077542331E+04 0.15102405929893E+04
 0.14493312476380E+04 0.13615805839473E+04 0.21154412144493E+04 0.13783576142765E+04 0.14489853816775E+04
 0.12604076827610E+04 0.21153077542331E+04 0.20842830325063E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52149997332524E+03 0.14691357335767E+01
 0.14691357335767E+01 0.36763297499338E+01 0.00000000000000E+00 0.35061322278860E+03 0.35061322278860E+03
 0.35061322278860E+03 0.35061322278860E+03 0.00000000000000E+00 0.00000000000000E+00 0.14829722667999E+00
 0.00000000000000E+00 -.20360769169865E+02 0.10000000000000E-02 0.14763776438486E+01 0.80000000000000E+04
 0.30000000000000E+04 0.54186678004319E+01 0.20320004251620E+01 0.34524535827563E+03 0.43351436255691E+03
 0.31736909251813E+03 0.31736909251813E+03 0.30115002149766E+03 0.30115002070476E+03 0.31736591674929E+03
 0.31736591674929E+03 0.30115002150278E+03 0.30115002070969E+03 0.31736909251813E+03 0.31736909251813E+03
 0.30115002149766E+03 0.30115002070476E+03 0.31736591674929E+03 0.31736591674929E+03 0.30115002150278E+03
 0.30115002070969E+03 0.31492460832640E+03 0.30115011373259E+03 0.26234729375754E+02 0.31431799917346E+02
 0.25746371211121E+03 0.57957814359181E+03 0.32082711292004E+03 0.22171081891089E+03 0.23923642495013E+03
 0.22171081891089E+03 0.46184215930644E+03 0.22172617395249E+03 0.23907919180677E+03 0.22172617395249E+03
 0.46171058705795E+03 0.22171081891089E+03 0.23923642495013E+03 0.22171081891089E+03 0.46184215930644E+03
 0.22172617395249E+03 0.23907919180677E+03 0.22172617395249E+03 0.46171058705795E+03 0.28517924556431E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38052389129048E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22086518759530E+00 0.00000000000000E+00 0.00000000000000E+00 0.22086518759530E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23042687643435E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23042687643435E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206437231981E+00 0.23296394106045E+00 0.35061322278860E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    786.26833036
 0.10679988829118E+00 0.32459843542213E+03 0.47643015829335E+03 0.47102495461278E+03 0.46862660178669E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14105368375723E+00 0.00000000000000E+00 -.28119311200175E+02
 0.32739393599333E-02 0.11829455426360E+01 0.24435394552216E+04 0.91632729570809E+03 0.67627796138216E+01
 0.25360423551831E+01 0.36137233732554E+03 0.30115072587056E+03 0.35256180101189E+03 0.38460167293823E+03
 0.30115009718909E+03 0.30115017507026E+03 0.34791255299618E+03 0.38457798598613E+03 0.30115008084080E+03
 0.30115017464370E+03 0.35256180101189E+03 0.38460167293823E+03 0.30115009718909E+03 0.30115017507026E+03
 0.34791255299618E+03 0.38457798598613E+03 0.30115008084080E+03 0.30115017464370E+03 0.43441163196012E+03
 0.34618946308726E+03 0.25360192315185E+04 0.23317289921454E+04 0.74109129760492E+03 0.11317353127702E+04
 0.38693855867726E+03 0.15146214873167E+04 0.14521090287591E+04 0.13640414356387E+04 0.21153702364328E+04
 0.13831309093656E+04 0.14517658924201E+04 0.12635047902907E+04 0.21152377830545E+04 0.15146214873167E+04
 0.14521090287591E+04 0.13640414356387E+04 0.21153702364328E+04 0.13831309093656E+04 0.14517658924201E+04
 0.12635047902907E+04 0.21152377830545E+04 0.20835155673108E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52204369387482E+03 0.14691357330136E+01
 0.14691357330136E+01 0.37456271963481E+01 0.00000000000000E+00 0.35095338969687E+03 0.35095338969687E+03
 0.35095338969687E+03 0.35095338969687E+03 0.00000000000000E+00 0.00000000000000E+00 0.14769644057647E+00
 0.00000000000000E+00 -.20331679675305E+02 0.10000000000000E-02 0.14858072634687E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53842784301132E+01 0.20191044112924E+01 0.34619159360206E+03 0.43440934435728E+03
 0.31760028921867E+03 0.31760028921867E+03 0.30115002578599E+03 0.30115002483492E+03 0.31759701944295E+03
 0.31759701944295E+03 0.30115002579204E+03 0.30115002484075E+03 0.31760028921867E+03 0.31760028921867E+03
 0.30115002578599E+03 0.30115002483492E+03 0.31759701944295E+03 0.31759701944295E+03 0.30115002579204E+03
 0.30115002484075E+03 0.31512506587252E+03 0.30115013357190E+03 0.21442281433911E+02 0.25914826510530E+02
 0.25950320363274E+03 0.58302500029309E+03 0.32222428064218E+03 0.22554547969362E+03 0.24100203805574E+03
 0.22554547969362E+03 0.46440597288559E+03 0.22556140196879E+03 0.24084234722920E+03 0.22556140196879E+03
 0.46427271191408E+03 0.22554547969362E+03 0.24100203805574E+03 0.22554547969362E+03 0.46440597288559E+03
 0.22556140196879E+03 0.24084234722920E+03 0.22556140196879E+03 0.46427271191408E+03 0.28545135898196E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38091070779794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22070557938124E+00 0.00000000000000E+00 0.00000000000000E+00 0.22070557938124E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23021618517267E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23021618517267E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206466230726E+00 0.23273849490177E+00 0.35095338969687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    793.19807500
 0.10673945111581E+00 0.32475266077603E+03 0.47670292197594E+03 0.47129655948347E+03 0.46889689937536E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14091522058620E+00 0.00000000000000E+00 -.28118160565141E+02
 0.32757928935254E-02 0.11841111476437E+01 0.24421568334836E+04 0.91580881255634E+03 0.67561225277877E+01
 0.25335459479204E+01 0.36170845934033E+03 0.30115078377034E+03 0.35285564714099E+03 0.38501281583139E+03
 0.30115010603976E+03 0.30115019100311E+03 0.34819439840901E+03 0.38498920578710E+03 0.30115008827102E+03
 0.30115019054229E+03 0.35285564714099E+03 0.38501281583139E+03 0.30115010603976E+03 0.30115019100311E+03
 0.34819439840901E+03 0.38498920578710E+03 0.30115008827102E+03 0.30115019054229E+03 0.43485520170721E+03
 0.34665903919378E+03 0.25388804217347E+04 0.23333281721631E+04 0.73977755174964E+03 0.11284740093640E+04
 0.38499756985560E+03 0.15167915416850E+04 0.14534689978122E+04 0.13652735355558E+04 0.21153169313259E+04
 0.13854960536022E+04 0.14531271502305E+04 0.12650514397858E+04 0.21151849289666E+04 0.15167915416850E+04
 0.14534689978122E+04 0.13652735355558E+04 0.21153169313259E+04 0.13854960536022E+04 0.14531271502305E+04
 0.12650514397857E+04 0.21151849289666E+04 0.20831244727897E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52231373206776E+03 0.14691357237431E+01
 0.14691357237431E+01 0.37802759195553E+01 0.00000000000000E+00 0.35112330704785E+03 0.35112330704785E+03
 0.35112330704785E+03 0.35112330704785E+03 0.00000000000000E+00 0.00000000000000E+00 0.14740625270363E+00
 0.00000000000000E+00 -.20316098987475E+02 0.10000000000000E-02 0.14903315200589E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53679331694492E+01 0.20129749385434E+01 0.34666124578818E+03 0.43485280477067E+03
 0.31771583105546E+03 0.31771583105546E+03 0.30115002818652E+03 0.30115002714692E+03 0.31771251424722E+03
 0.31771251424722E+03 0.30115002819310E+03 0.30115002715325E+03 0.31771583105546E+03 0.31771583105546E+03
 0.30115002818652E+03 0.30115002714692E+03 0.31771251424722E+03 0.31771251424722E+03 0.30115002819310E+03
 0.30115002715325E+03 0.31522528748047E+03 0.30115014450564E+03 0.19045934509956E+02 0.23168601169889E+02
 0.26051744711525E+03 0.58474096931495E+03 0.32292093496412E+03 0.22745702468108E+03 0.24187915983915E+03
 0.22745702468108E+03 0.46568117629021E+03 0.22747323192374E+03 0.24171824153801E+03 0.22747323192374E+03
 0.46554707270705E+03 0.22745702468108E+03 0.24187915983915E+03 0.22745702468108E+03 0.46568117629021E+03
 0.22747323192374E+03 0.24171824153801E+03 0.22747323192374E+03 0.46554707270705E+03 0.28558655737344E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38110354911241E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22061898537701E+00 0.00000000000000E+00 0.00000000000000E+00 0.22061898537701E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23011076050441E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23011076050441E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206484872584E+00 0.23262608958215E+00 0.35112330704785E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    800.12781964
 0.10667473768478E+00 0.32490725930025E+03 0.47697452144422E+03 0.47156727634434E+03 0.46916645334703E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14077998746275E+00 0.00000000000000E+00 -.28116916340720E+02
 0.32777799184808E-02 0.11852353924233E+01 0.24406763721061E+04 0.91525363953979E+03 0.67497140662019E+01
 0.25311427748257E+01 0.36204330678562E+03 0.30115084537748E+03 0.35314850532896E+03 0.38542200769697E+03
 0.30115011555545E+03 0.30115020812955E+03 0.34847539340526E+03 0.38539847304394E+03 0.30115009626568E+03
 0.30115020763231E+03 0.35314850532896E+03 0.38542200769697E+03 0.30115011555545E+03 0.30115020812955E+03
 0.34847539340527E+03 0.38539847304394E+03 0.30115009626568E+03 0.30115020763231E+03 0.43529612960695E+03
 0.34712628104184E+03 0.25417269382655E+04 0.23349237231245E+04 0.73846418639796E+03 0.11252323426763E+04
 0.38307583534637E+03 0.15189519365021E+04 0.14548152997790E+04 0.13665031359855E+04 0.21152590745075E+04
 0.13878509466800E+04 0.14544746942044E+04 0.12665938895196E+04 0.21151274917436E+04 0.15189519365021E+04
 0.14548152997790E+04 0.13665031359855E+04 0.21152590745075E+04 0.13878509466800E+04 0.14544746942044E+04
 0.12665938895196E+04 0.21151274917437E+04 0.20827351514788E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52258299025070E+03 0.14691357137186E+01
 0.14691357137186E+01 0.38149246427624E+01 0.00000000000000E+00 0.35129289107996E+03 0.35129289107996E+03
 0.35129289107996E+03 0.35129289107996E+03 0.00000000000000E+00 0.00000000000000E+00 0.14712268931402E+00
 0.00000000000000E+00 -.20300466051315E+02 0.10000000000000E-02 0.14947331669780E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53521258353918E+01 0.20070471882719E+01 0.34712856166309E+03 0.43529362712898E+03
 0.31783130586163E+03 0.31783130586163E+03 0.30115003077215E+03 0.30115002963718E+03 0.31782794199537E+03
 0.31782794199537E+03 0.30115003077927E+03 0.30115002964404E+03 0.31783130586163E+03 0.31783130586163E+03
 0.30115003077215E+03 0.30115002963718E+03 0.31782794199537E+03 0.31782794199537E+03 0.30115003077927E+03
 0.30115002964404E+03 0.31532548108935E+03 0.30115015616125E+03 0.16648375315456E+02 0.20429386955831E+02
 0.26152657061636E+03 0.58644852182092E+03 0.32361431835148E+03 0.22936380148764E+03 0.24275107664564E+03
 0.22936380148764E+03 0.46694904464398E+03 0.22938029465528E+03 0.24258893142196E+03 0.22938029465528E+03
 0.46681409927998E+03 0.22936380148764E+03 0.24275107664564E+03 0.22936380148764E+03 0.46694904464398E+03
 0.22938029465528E+03 0.24258893142196E+03 0.22938029465528E+03 0.46681409927998E+03 0.28571921805594E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38129588052339E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22053203671822E+00 0.00000000000000E+00 0.00000000000000E+00 0.22053203671822E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.23000544275770E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23000544275770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206503841426E+00 0.23251401701473E+00 0.35129289107996E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    813.98730893
 0.10653687775835E+00 0.32521507736290E+03 0.47751471353105E+03 0.47210620429069E+03 0.46970328432518E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14051861362113E+00 0.00000000000000E+00 -.28114508337893E+02
 0.32820209837293E-02 0.11873668410400E+01 0.24375225020376E+04 0.91407093826412E+03 0.67375976180983E+01
 0.25265991067869E+01 0.36270929521591E+03 0.30115098042423E+03 0.35373132223583E+03 0.38623467972360E+03
 0.30115013673959E+03 0.30115024624418E+03 0.34903489390105E+03 0.38621129144430E+03 0.30115011408403E+03
 0.30115024566716E+03 0.35373132223583E+03 0.38623467972360E+03 0.30115013673959E+03 0.30115024624418E+03
 0.34903489390105E+03 0.38621129144430E+03 0.30115011408403E+03 0.30115024566716E+03 0.43617033156012E+03
 0.34805373027921E+03 0.25473822524401E+04 0.23380901775698E+04 0.73584624264387E+03 0.11188172002089E+04
 0.37929172635185E+03 0.15232482547097E+04 0.14574768110883E+04 0.13689425402657E+04 0.21151422925296E+04
 0.13925342618903E+04 0.14571385572037E+04 0.12696543956775E+04 0.21150114613188E+04 0.15232482547097E+04
 0.14574768110883E+04 0.13689425402657E+04 0.21151422925296E+04 0.13925342618903E+04 0.14571385572037E+04
 0.12696543956775E+04 0.21150114613188E+04 0.20819670754901E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52311918423091E+03 0.14691356943176E+01
 0.14691356943176E+01 0.38842220891767E+01 0.00000000000000E+00 0.35163111917672E+03 0.35163111917672E+03
 0.35163111917672E+03 0.35163111917672E+03 0.00000000000000E+00 0.00000000000000E+00 0.14657483915334E+00
 0.00000000000000E+00 -.20269417112105E+02 0.10000000000000E-02 0.15031799092655E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53220509073389E+01 0.19957690902521E+01 0.34805615324683E+03 0.43616763004457E+03
 0.31806200955855E+03 0.31806200955855E+03 0.30115003654405E+03 0.30115003519619E+03 0.31805855148267E+03
 0.31805855148267E+03 0.30115003655238E+03 0.30115003520421E+03 0.31806200955855E+03 0.31806200955855E+03
 0.30115003654405E+03 0.30115003519619E+03 0.31805855148267E+03 0.31805855148267E+03 0.30115003655238E+03
 0.30115003520421E+03 0.31552574669158E+03 0.30115018178214E+03 0.11851679892824E+02 0.14974903558551E+02
 0.26353073293430E+03 0.58984078106144E+03 0.32499239446246E+03 0.23316342033999E+03 0.24448062059396E+03
 0.23316342033999E+03 0.46946498472818E+03 0.23318048832662E+03 0.24431602181626E+03 0.23318048832662E+03
 0.46932835708393E+03 0.23316342033999E+03 0.24448062059395E+03 0.23316342033999E+03 0.46946498472818E+03
 0.23318048832662E+03 0.24431602181626E+03 0.23318048832662E+03 0.46932835708393E+03 0.28597886440994E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38167916830639E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22035949075022E+00 0.00000000000000E+00 0.00000000000000E+00 0.22035949075022E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22979531596916E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22979531596916E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206541286768E+00 0.23229081170403E+00 0.35163111917672E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    820.91705357
 0.10646604463863E+00 0.32536810956775E+03 0.47778338375187E+03 0.47237436662357E+03 0.46997045384301E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14039222399207E+00 0.00000000000000E+00 -.28113409373180E+02
 0.32842043426703E-02 0.11883772996844E+01 0.24359020223131E+04 0.91346325836741E+03 0.67318687441479E+01
 0.25244507790555E+01 0.36304046914686E+03 0.30115105423190E+03 0.35402129481535E+03 0.38663822887292E+03
 0.30115014849089E+03 0.30115026737985E+03 0.34931341121547E+03 0.38661491164221E+03 0.30115012397913E+03
 0.30115026675928E+03 0.35402129481535E+03 0.38663822887292E+03 0.30115014849089E+03 0.30115026737985E+03
 0.34931341121547E+03 0.38661491164221E+03 0.30115012397913E+03 0.30115026675928E+03 0.43660372315891E+03
 0.34851394338884E+03 0.25501909808435E+04 0.23396593602522E+04 0.73454228925062E+03 0.11156436690159E+04
 0.37742866831900E+03 0.15253845920115E+04 0.14587934934832E+04 0.13701519634790E+04 0.21150848879056E+04
 0.13948629952413E+04 0.14584563529584E+04 0.12711719375423E+04 0.21149543916732E+04 0.15253845920115E+04
 0.14587934934832E+04 0.13701519634790E+04 0.21150848879056E+04 0.13948629952413E+04 0.14584563529584E+04
 0.12711719375423E+04 0.21149543916732E+04 0.20815883987996E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52338602581641E+03 0.14691356854634E+01
 0.14691356854634E+01 0.39188708123839E+01 0.00000000000000E+00 0.35179980121271E+03 0.35179980121271E+03
 0.35179980121271E+03 0.35179980121271E+03 0.00000000000000E+00 0.00000000000000E+00 0.14631025213626E+00
 0.00000000000000E+00 -.20254077784381E+02 0.10000000000000E-02 0.15072306263591E+01 0.80000000000000E+04
 0.30000000000000E+04 0.53077477726982E+01 0.19904054147618E+01 0.34851643468926E+03 0.43660092824543E+03
 0.31817722942685E+03 0.31817722942685E+03 0.30115003975421E+03 0.30115003828795E+03 0.31817372419060E+03
 0.31817372419060E+03 0.30115003976320E+03 0.30115003829661E+03 0.31817722942685E+03 0.31817722942685E+03
 0.30115003975421E+03 0.30115003828795E+03 0.31817372419060E+03 0.31817372419060E+03 0.30115003976320E+03
 0.30115003829661E+03 0.31562581061229E+03 0.30115019582226E+03 0.94534544124949E+01 0.12261027507022E+02
 0.26452640379837E+03 0.59152662869843E+03 0.32567759288107E+03 0.23505647626691E+03 0.24533888559512E+03
 0.23505647626691E+03 0.47071408315769E+03 0.23507383319931E+03 0.24517305954548E+03 0.23507383319931E+03
 0.47057661445097E+03 0.23505647626691E+03 0.24533888559512E+03 0.23505647626691E+03 0.47071408315769E+03
 0.23507383319931E+03 0.24517305954548E+03 0.23507383319931E+03 0.47057661445097E+03 0.28610676619181E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38187018177512E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22027435788639E+00 0.00000000000000E+00 0.00000000000000E+00 0.22027435788639E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22969069631400E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22969069631400E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206559454312E+00 0.23217964913373E+00 0.35179980121271E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    834.77654285
 0.10632348076336E+00 0.32567280452484E+03 0.47831787340951E+03 0.47290795506112E+03 0.47050212084693E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14014755387268E+00 0.00000000000000E+00 -.28111374355837E+02
 0.32886075543693E-02 0.11902944575226E+01 0.24326405226951E+04 0.91224019601066E+03 0.67210260028015E+01
 0.25203847510506E+01 0.36369924555259E+03 0.30115121537485E+03 0.35459841487048E+03 0.38743989534216E+03
 0.30115017453152E+03 0.30115031419827E+03 0.34986801801977E+03 0.38741671608943E+03 0.30115014593052E+03
 0.30115031348272E+03 0.35459841487048E+03 0.38743989534216E+03 0.30115017453152E+03 0.30115031419827E+03
 0.34986801801977E+03 0.38741671608943E+03 0.30115014593052E+03 0.30115031348272E+03 0.43746328829712E+03
 0.34942738845217E+03 0.25557687703806E+04 0.23427704093017E+04 0.73194330227571E+03 0.11093610150652E+04
 0.37375799627809E+03 0.15296331351256E+04 0.14613985175270E+04 0.13725529754553E+04 0.21149706033969E+04
 0.13994940404557E+04 0.14610634874611E+04 0.12741840147601E+04 0.21148407026011E+04 0.15296331351256E+04
 0.14613985175270E+04 0.13725529754553E+04 0.21149706033969E+04 0.13994940404557E+04 0.14610634874611E+04
 0.12741840147601E+04 0.21148407026011E+04 0.20808398329150E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52391702372001E+03 0.14691356690676E+01
 0.14691356690676E+01 0.39881682587982E+01 0.00000000000000E+00 0.35213630498140E+03 0.35213630498140E+03
 0.35213630498140E+03 0.35213630498140E+03 0.00000000000000E+00 0.00000000000000E+00 0.14579901906216E+00
 0.00000000000000E+00 -.20223739934068E+02 0.10000000000000E-02 0.15150008713599E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52805250156848E+01 0.19801968808818E+01 0.34943001056917E+03 0.43746031823638E+03
 0.31840739600635E+03 0.31840739600635E+03 0.30115004688644E+03 0.30115004515712E+03 0.31840379631643E+03
 0.31840379631643E+03 0.30115004689687E+03 0.30115004516716E+03 0.31840739600635E+03 0.31840739600635E+03
 0.30115004688644E+03 0.30115004515712E+03 0.31840379631643E+03 0.31840379631643E+03 0.30115004689687E+03
 0.30115004516716E+03 0.31582579163541E+03 0.30115022655744E+03 0.46585177273081E+01 0.68626556776595E+01
 0.26650566352597E+03 0.59487865510673E+03 0.32704046326313E+03 0.23882921610861E+03 0.24704313753840E+03
 0.23882921610861E+03 0.47319539741676E+03 0.23884715414721E+03 0.24687485459215E+03 0.23884715414721E+03
 0.47305624555156E+03 0.23882921610861E+03 0.24704313753840E+03 0.23882921610861E+03 0.47319539741676E+03
 0.23884715414721E+03 0.24687485459215E+03 0.23884715414721E+03 0.47305624555156E+03 0.28635983558946E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38225096607585E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22010611473390E+00 0.00000000000000E+00 0.00000000000000E+00 0.22010611473390E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22948263343744E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22948263343744E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206594791889E+00 0.23195819943431E+00 0.35213630498140E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    841.70628749
 0.10625218608460E+00 0.32582452002622E+03 0.47858368556875E+03 0.47317335380763E+03 0.47076658389477E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14002910865980E+00 0.00000000000000E+00 -.28110410344912E+02
 0.32908139872244E-02 0.11912034838248E+01 0.24310094800428E+04 0.91162855501607E+03 0.67158970810874E+01
 0.25184614054078E+01 0.36402687685332E+03 0.30115130310935E+03 0.35488558622830E+03 0.38783806017249E+03
 0.30115018891373E+03 0.30115034004601E+03 0.35014412744374E+03 0.38781494790990E+03 0.30115015806706E+03
 0.30115033927883E+03 0.35488558622830E+03 0.38783806017248E+03 0.30115018891373E+03 0.30115034004601E+03
 0.35014412744374E+03 0.38781494790990E+03 0.30115015806706E+03 0.30115033927883E+03 0.43788951998332E+03
 0.34988064139135E+03 0.25585374671388E+04 0.23443121546880E+04 0.73064796671820E+03 0.11062507590373E+04
 0.37194955248554E+03 0.15317452110753E+04 0.14626866693710E+04 0.13737447881482E+04 0.21149130241456E+04
 0.14017962261741E+04 0.14623526396499E+04 0.12756787661005E+04 0.21147833864186E+04 0.15317452110753E+04
 0.14626866693710E+04 0.13737447881482E+04 0.21149130241456E+04 0.14017962261741E+04 0.14623526396499E+04
 0.12756787661005E+04 0.21147833864186E+04 0.20804691752662E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52418114649514E+03 0.14691356613007E+01
 0.14691356613007E+01 0.40228169820053E+01 0.00000000000000E+00 0.35230410986835E+03 0.35230410986835E+03
 0.35230410986835E+03 0.35230410986835E+03 0.00000000000000E+00 0.00000000000000E+00 0.14555208700280E+00
 0.00000000000000E+00 -.20208716127002E+02 0.10000000000000E-02 0.15187259623998E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52675730830064E+01 0.19753399061274E+01 0.34988332594827E+03 0.43788646779601E+03
 0.31852233966166E+03 0.31852233966166E+03 0.30115005083544E+03 0.30115004896047E+03 0.31851869266999E+03
 0.31851869266999E+03 0.30115005084665E+03 0.30115004897126E+03 0.31852233966166E+03 0.31852233966166E+03
 0.30115005083544E+03 0.30115004896047E+03 0.31851869266999E+03 0.31851869266999E+03 0.30115005084665E+03
 0.30115004897126E+03 0.31592570570102E+03 0.30115024333416E+03 0.22621092513778E+01 0.41791637197773E+01
 0.26748937725462E+03 0.59654481704326E+03 0.32771799290237E+03 0.24070887794147E+03 0.24788924831634E+03
 0.24070887794147E+03 0.47442763263667E+03 0.24072710819767E+03 0.24771973527914E+03 0.24072710819767E+03
 0.47428763827277E+03 0.24070887794147E+03 0.24788924831634E+03 0.24070887794147E+03 0.47442763263666E+03
 0.24072710819767E+03 0.24771973527914E+03 0.24072710819767E+03 0.47428763827277E+03 0.28648524585046E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38244072317962E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22002282873133E+00 0.00000000000000E+00 0.00000000000000E+00 0.22002282873133E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22937922907731E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22937922907731E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206612064153E+00 0.23184792309353E+00 0.35230410986835E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    859.06005038
 0.10608299919259E+00 0.32619704987487E+03 0.47924395951075E+03 0.47383206778031E+03 0.47142264923852E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13974336737220E+00 0.00000000000000E+00 -.28116654654223E+02
 0.32960618443257E-02 0.11933449786406E+01 0.24271389245236E+04 0.91017709669636E+03 0.67038451941309E+01
 0.25139419477991E+01 0.36484093461566E+03 0.30115154762488E+03 0.35559933113804E+03 0.38882734164530E+03
 0.30115022974349E+03 0.30115041338505E+03 0.35083044075530E+03 0.38880439102056E+03 0.30115019256851E+03
 0.30115041247425E+03 0.35559933113804E+03 0.38882734164530E+03 0.30115022974349E+03 0.30115041338505E+03
 0.35083044075530E+03 0.38880439102055E+03 0.30115019256851E+03 0.30115041247425E+03 0.43895353454128E+03
 0.35101153288311E+03 0.25653953235189E+04 0.23480822493868E+04 0.72727335717857E+03 0.10983205967263E+04
 0.36741087276184E+03 0.15369902785557E+04 0.14658486500989E+04 0.13766680560102E+04 0.21147359916306E+04
 0.14075158002333E+04 0.14655169383834E+04 0.12793578886829E+04 0.21146068865131E+04 0.15369902785557E+04
 0.14658486500989E+04 0.13766680560102E+04 0.21147359916306E+04 0.14075158002333E+04 0.14655169383834E+04
 0.12793578886829E+04 0.21146068865131E+04 0.20794632600901E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52483646342640E+03 0.14691357116102E+01
 0.14691357116102E+01 0.41095857964249E+01 0.00000000000000E+00 0.35272322066715E+03 0.35272322066715E+03
 0.35272322066715E+03 0.35272322066715E+03 0.00000000000000E+00 0.00000000000000E+00 0.14495799078912E+00
 0.00000000000000E+00 -.20180637069210E+02 0.10000000000000E-02 0.15276068948661E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52369493924687E+01 0.19638560221758E+01 0.35101434665129E+03 0.43895031264671E+03
 0.31880845425692E+03 0.31880845425692E+03 0.30115006264707E+03 0.30115005979282E+03 0.31880468919430E+03
 0.31880468919430E+03 0.30115006266057E+03 0.30115005980571E+03 0.31880845425692E+03 0.31880845425692E+03
 0.30115006264707E+03 0.30115005979282E+03 0.31880468919430E+03 0.31880468919430E+03 0.30115006266057E+03
 0.30115005980571E+03 0.31617445627541E+03 0.30115029024472E+03 -.37859177889154E+01 -.25460480332451E+01
 0.26994745777671E+03 0.60071711297645E+03 0.32941991791086E+03 0.24542878043344E+03 0.25000223157029E+03
 0.24542878043344E+03 0.47751397923424E+03 0.24544774226552E+03 0.24982956372379E+03 0.24544774226552E+03
 0.47737179836001E+03 0.24542878043344E+03 0.25000223157029E+03 0.24542878043344E+03 0.47751397923424E+03
 0.24544774226552E+03 0.24982956372379E+03 0.24544774226552E+03 0.47737179836001E+03 0.28679818765174E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38291478560147E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21987389707212E+00 0.00000000000000E+00 0.00000000000000E+00 0.21987389707212E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22913758359219E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22913758359219E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206618543251E+00 0.23157256692104E+00 0.35272322066715E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    869.48406862
 0.10600023540126E+00 0.32641595104163E+03 0.47963667207274E+03 0.47422286124019E+03 0.47181138781768E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13957915132027E+00 0.00000000000000E+00 -.28125065558162E+02
 0.32986350712188E-02 0.11945400637480E+01 0.24252455416488E+04 0.90946707811831E+03 0.66971382901125E+01
 0.25114268587922E+01 0.36532642803326E+03 0.30115171227545E+03 0.35602533721976E+03 0.38941630101495E+03
 0.30115025778088E+03 0.30115046371473E+03 0.35124039059120E+03 0.38939344372011E+03 0.30115021629452E+03
 0.30115046270746E+03 0.35602533721976E+03 0.38941630101495E+03 0.30115025778088E+03 0.30115046371473E+03
 0.35124039059120E+03 0.38939344372011E+03 0.30115021629452E+03 0.30115046270746E+03 0.43958374765242E+03
 0.35168278411967E+03 0.25694375789737E+04 0.23502542394725E+04 0.72526594398075E+03 0.10936381619992E+04
 0.36474588829850E+03 0.15400982700594E+04 0.14676974215503E+04 0.13783677675177E+04 0.21145982964142E+04
 0.14109054836054E+04 0.14673670087840E+04 0.12815054904801E+04 0.21144694555526E+04 0.15400982700594E+04
 0.14676974215503E+04 0.13783677675177E+04 0.21145982964142E+04 0.14109054836054E+04 0.14673670087840E+04
 0.12815054904801E+04 0.21144694555526E+04 0.20788338091600E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52522487125884E+03 0.14691357793756E+01
 0.14691357793756E+01 0.41617058876373E+01 0.00000000000000E+00 0.35297509697351E+03 0.35297509697351E+03
 0.35297509697351E+03 0.35297509697351E+03 0.00000000000000E+00 0.00000000000000E+00 0.14461710442534E+00
 0.00000000000000E+00 -.20170449569278E+02 0.10000000000000E-02 0.15326435941034E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52197392993248E+01 0.19574022372468E+01 0.35168567415083E+03 0.43958042620686E+03
 0.31898022090392E+03 0.31898022090392E+03 0.30115007046735E+03 0.30115006725681E+03 0.31897638444659E+03
 0.31897638444659E+03 0.30115007048233E+03 0.30115006727111E+03 0.31898022090392E+03 0.31898022090392E+03
 0.30115007046735E+03 0.30115006725681E+03 0.31897638444659E+03 0.31897638444659E+03 0.30115007048233E+03
 0.30115006727111E+03 0.31632387114444E+03 0.30115032194346E+03 -.73955785425777E+01 -.65238876790050E+01
 0.27142052487305E+03 0.60322517975724E+03 0.33044755225982E+03 0.24824976964762E+03 0.25126783407546E+03
 0.24824976964762E+03 0.47937017686521E+03 0.24826917644687E+03 0.25109328279266E+03 0.24826917644687E+03
 0.47922669886413E+03 0.24824976964762E+03 0.25126783407546E+03 0.24824976964762E+03 0.47937017686521E+03
 0.24826917644687E+03 0.25109328279266E+03 0.24826917644687E+03 0.47922669886412E+03 0.28700214579821E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38320151772775E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21981721555205E+00 0.00000000000000E+00 0.00000000000000E+00 0.21981721555205E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22904420812923E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22904420812923E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206596475119E+00 0.23140712437201E+00 0.35297509697351E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    879.90808686
 0.10592963732142E+00 0.32663563789990E+03 0.48002708876055E+03 0.47461085517459E+03 0.47219717101703E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13942053652949E+00 0.00000000000000E+00 -.28131874250431E+02
 0.33008331881378E-02 0.11956649661791E+01 0.24236305029741E+04 0.90886143861529E+03 0.66908375057312E+01
 0.25090640646492E+01 0.36580938969155E+03 0.30115189063202E+03 0.35644937305548E+03 0.39000128092348E+03
 0.30115028857585E+03 0.30115051896821E+03 0.35164868828571E+03 0.38997851425676E+03 0.30115024238064E+03
 0.30115051785664E+03 0.35644937305548E+03 0.39000128092348E+03 0.30115028857585E+03 0.30115051896821E+03
 0.35164868828571E+03 0.38997851425676E+03 0.30115024238064E+03 0.30115051785664E+03 0.44020730073143E+03
 0.35234811744014E+03 0.25734351942138E+04 0.23524041432955E+04 0.72328701614521E+03 0.10890365438411E+04
 0.36213309261515E+03 0.15431814266886E+04 0.14695218818227E+04 0.13800630607006E+04 0.21144560266369E+04
 0.14142678146393E+04 0.14691926952081E+04 0.12836425445713E+04 0.21143274046473E+04 0.15431814266886E+04
 0.14695218818227E+04 0.13800630607006E+04 0.21144560266369E+04 0.14142678146393E+04 0.14691926952081E+04
 0.12836425445713E+04 0.21143274046473E+04 0.20782137022769E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52561033786479E+03 0.14691358342323E+01
 0.14691358342323E+01 0.42138259788498E+01 0.00000000000000E+00 0.35322720880648E+03 0.35322720880648E+03
 0.35322720880648E+03 0.35322720880648E+03 0.00000000000000E+00 0.00000000000000E+00 0.14428766698667E+00
 0.00000000000000E+00 -.20158588212060E+02 0.10000000000000E-02 0.15374664980990E+01 0.80000000000000E+04
 0.30000000000000E+04 0.52033654130945E+01 0.19512620299104E+01 0.35235108803520E+03 0.44020387505961E+03
 0.31915204576486E+03 0.31915204576486E+03 0.30115007907762E+03 0.30115007547479E+03 0.31914813764744E+03
 0.31914813764744E+03 0.30115007909420E+03 0.30115007549061E+03 0.31915204576486E+03 0.31915204576486E+03
 0.30115007907762E+03 0.30115007547479E+03 0.31914813764744E+03 0.31914813764744E+03 0.30115007909420E+03
 0.30115007549061E+03 0.31647340591224E+03 0.30115035636557E+03 -.10986631409014E+02 -.10449038701097E+02
 0.27289175534513E+03 0.60573170282495E+03 0.33147548870310E+03 0.25106009797232E+03 0.25253132989676E+03
 0.25106009797232E+03 0.48122491015233E+03 0.25107995334406E+03 0.25235489849850E+03 0.25107995334406E+03
 0.48108014070917E+03 0.25106009797232E+03 0.25253132989676E+03 0.25106009797232E+03 0.48122491015233E+03
 0.25107995334406E+03 0.25235489849850E+03 0.25107995334406E+03 0.48108014070917E+03 0.28721794010225E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38348769152754E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21975091440778E+00 0.00000000000000E+00 0.00000000000000E+00 0.21975091440778E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22894461402440E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22894461402440E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206580965166E+00 0.23124183275025E+00 0.35322720880648E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    890.00000000
 0.10586655007014E+00 0.32684959538024E+03 0.48040305540381E+03 0.47498433038247E+03 0.47256850754954E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13927226521478E+00 0.00000000000000E+00 -.28137050926048E+02
 0.33027999109744E-02 0.11966875120370E+01 0.24221873003623E+04 0.90832023763586E+03 0.66851203171513E+01
 0.25069201189318E+01 0.36627467429731E+03 0.30115207688970E+03 0.35685815251205E+03 0.39056392291026E+03
 0.30115032115979E+03 0.30115057740387E+03 0.35204252876316E+03 0.39054124148818E+03 0.30115027000884E+03
 0.30115057618362E+03 0.35685815251205E+03 0.39056392291026E+03 0.30115032115979E+03 0.30115057740387E+03
 0.35204252876316E+03 0.39054124148818E+03 0.30115027000884E+03 0.30115057618362E+03 0.44080487240030E+03
 0.35298671890096E+03 0.25772708586499E+04 0.23544757916566E+04 0.72139928688365E+03 0.10846589448734E+04
 0.35965266155532E+03 0.15461461468164E+04 0.14712682735297E+04 0.13817047864265E+04 0.21143175190778E+04
 0.14175007257359E+04 0.14709402071396E+04 0.12857066316514E+04 0.21141890674212E+04 0.15461461468164E+04
 0.14712682735297E+04 0.13817047864265E+04 0.21143175190778E+04 0.14175007257359E+04 0.14709402071396E+04
 0.12857066316514E+04 0.21141890674213E+04 0.20776282317296E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52598134806499E+03 0.14691358759400E+01
 0.14691358759400E+01 0.42642855445455E+01 0.00000000000000E+00 0.35347110862413E+03 0.35347110862413E+03
 0.35347110862413E+03 0.35347110862413E+03 0.00000000000000E+00 0.00000000000000E+00 0.14397926887727E+00
 0.00000000000000E+00 -.20145669016183E+02 0.10000000000000E-02 0.15419407805789E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51882666965956E+01 0.19456000112233E+01 0.35298976623828E+03 0.44080134831577E+03
 0.31931849409692E+03 0.31931849409692E+03 0.30115008814049E+03 0.30115008419013E+03 0.31931451635750E+03
 0.31931451635750E+03 0.30115008815871E+03 0.30115008420753E+03 0.31931849409692E+03 0.31931849409692E+03
 0.30115008814049E+03 0.30115008419013E+03 0.31931451635750E+03 0.31931451635750E+03 0.30115008815871E+03
 0.30115008420753E+03 0.31661833095483E+03 0.30115039239651E+03 -.14448641796174E+02 -.14194427265788E+02
 0.27431114093401E+03 0.60814925955975E+03 0.33246656292108E+03 0.25376890317151E+03 0.25374932067701E+03
 0.25376890317151E+03 0.48301263830631E+03 0.25378919627301E+03 0.25357107266394E+03 0.25378919627301E+03
 0.48286662429640E+03 0.25376890317151E+03 0.25374932067701E+03 0.25376890317151E+03 0.48301263830630E+03
 0.25378919627301E+03 0.25357107266394E+03 0.25378919627301E+03 0.48286662429640E+03 0.28743542956125E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38376401495842E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21967808895115E+00 0.00000000000000E+00 0.00000000000000E+00 0.21967808895115E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22884448100678E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22884448100678E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206571676357E+00 0.23108220976153E+00 0.35347110862413E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    900.00000000
 0.10580723060076E+00 0.32706103761598E+03 0.48077355694595E+03 0.47535225828629E+03 0.47293429225278E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13913036037825E+00 0.00000000000000E+00 -.28144109503863E+02
 0.33046513016329E-02 0.11976380908530E+01 0.24208302994167E+04 0.90781136228128E+03 0.66798142620050E+01
 0.25049303482519E+01 0.36673342678309E+03 0.30115227552757E+03 0.35726142596669E+03 0.39111794976837E+03
 0.30115035635560E+03 0.30115064049325E+03 0.35243124013683E+03 0.39109535041744E+03 0.30115029987964E+03
 0.30115063915732E+03 0.35726142596669E+03 0.39111794976837E+03 0.30115035635560E+03 0.30115064049325E+03
 0.35243124013683E+03 0.39109535041744E+03 0.30115029987964E+03 0.30115063915732E+03 0.44139230468543E+03
 0.35361516217864E+03 0.25810391785600E+04 0.23565067375643E+04 0.71953380820255E+03 0.10803596602982E+04
 0.35722818305467E+03 0.15490646347704E+04 0.14729771358781E+04 0.13833197081605E+04 0.21141754413547E+04
 0.14206833870171E+04 0.14726501117519E+04 0.12877363797817E+04 0.21140471148307E+04 0.15490646347704E+04
 0.14729771358781E+04 0.13833197081605E+04 0.21141754413547E+04 0.14206833870171E+04 0.14726501117519E+04
 0.12877363797817E+04 0.21140471148307E+04 0.20770484513630E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52634681117275E+03 0.14691359328100E+01
 0.14691359328100E+01 0.43142855445455E+01 0.00000000000000E+00 0.35371232821073E+03 0.35371232821073E+03
 0.35371232821073E+03 0.35371232821073E+03 0.00000000000000E+00 0.00000000000000E+00 0.14368358132182E+00
 0.00000000000000E+00 -.20135135538747E+02 0.10000000000000E-02 0.15461926239575E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51739995884367E+01 0.19402498456638E+01 0.35361827928824E+03 0.44138869166087E+03
 0.31948333015820E+03 0.31948333015820E+03 0.30115009799680E+03 0.30115009362506E+03 0.31947928328371E+03
 0.31947928328371E+03 0.30115009801677E+03 0.30115009364414E+03 0.31948333015820E+03 0.31948333015820E+03
 0.30115009799680E+03 0.30115009362506E+03 0.31947928328371E+03 0.31947928328371E+03 0.30115009801677E+03
 0.30115009364414E+03 0.31676191009346E+03 0.30115043090955E+03 -.17875776763762E+02 -.17842761796284E+02
 0.27571207044038E+03 0.61053513171713E+03 0.33344450092455E+03 0.25644526409126E+03 0.25495038018596E+03
 0.25644526409126E+03 0.48477590193512E+03 0.25646599354898E+03 0.25477032450261E+03 0.25646599354898E+03
 0.48462864852079E+03 0.25644526409126E+03 0.25495038018596E+03 0.25644526409126E+03 0.48477590193512E+03
 0.25646599354898E+03 0.25477032450261E+03 0.25646599354898E+03 0.48462864852079E+03 0.28766126895182E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38403737931121E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21961953920511E+00 0.00000000000000E+00 0.00000000000000E+00 0.21961953920511E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22875204962112E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22875204962112E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206553820326E+00 0.23092446601564E+00 0.35371232821073E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    910.00000000
 0.10575574924025E+00 0.32727001961413E+03 0.48114184921297E+03 0.47571757233757E+03 0.47329727046903E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13899335739602E+00 0.00000000000000E+00 -.28155894804507E+02
 0.33062597101272E-02 0.11985280688201E+01 0.24196526290708E+04 0.90736973590154E+03 0.66748541048983E+01
 0.25030702893369E+01 0.36718990821352E+03 0.30115248900370E+03 0.35766292568255E+03 0.39166858401773E+03
 0.30115039465729E+03 0.30115070911610E+03 0.35281840831014E+03 0.39164606440407E+03 0.30115033241630E+03
 0.30115070765614E+03 0.35766292568255E+03 0.39166858401773E+03 0.30115039465729E+03 0.30115070911610E+03
 0.35281840831014E+03 0.39164606440408E+03 0.30115033241630E+03 0.30115070765614E+03 0.44197521071261E+03
 0.35423974521122E+03 0.25847677263222E+04 0.23584934167445E+04 0.71766769247180E+03 0.10760891319508E+04
 0.35483310101664E+03 0.15519605104025E+04 0.14746600114606E+04 0.13849071905499E+04 0.21140219456513E+04
 0.14238417424885E+04 0.14743339625693E+04 0.12897353778967E+04 0.21138937005912E+04 0.15519605104025E+04
 0.14746600114606E+04 0.13849071905499E+04 0.21140219456513E+04 0.14238417424884E+04 0.14743339625693E+04
 0.12897353778967E+04 0.21138937005912E+04 0.20764564725256E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52670951949300E+03 0.14691360277625E+01
 0.14691360277625E+01 0.43642855445455E+01 0.00000000000000E+00 0.35395309539113E+03 0.35395309539113E+03
 0.35395309539113E+03 0.35395309539113E+03 0.00000000000000E+00 0.00000000000000E+00 0.14339742197999E+00
 0.00000000000000E+00 -.20130243773251E+02 0.10000000000000E-02 0.15502698127696E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51603920389238E+01 0.19351470145964E+01 0.35424292438832E+03 0.44197151781020E+03
 0.31964803825612E+03 0.31964803825612E+03 0.30115010876726E+03 0.30115010391504E+03 0.31964392207001E+03
 0.31964392207001E+03 0.30115010878911E+03 0.30115010393591E+03 0.31964803825612E+03 0.31964803825612E+03
 0.30115010876726E+03 0.30115010391504E+03 0.31964392207001E+03 0.31964392207001E+03 0.30115010878911E+03
 0.30115010393591E+03 0.31690543268104E+03 0.30115047239176E+03 -.21302970318381E+02 -.21610655400731E+02
 0.27710811466019E+03 0.61291377317084E+03 0.33442011793734E+03 0.25911589885932E+03 0.25614621006472E+03
 0.25911589885932E+03 0.48653329226818E+03 0.25913706742328E+03 0.25596433578835E+03 0.25913706742328E+03
 0.48638479036722E+03 0.25911589885932E+03 0.25614621006472E+03 0.25911589885932E+03 0.48653329226818E+03
 0.25913706742328E+03 0.25596433578835E+03 0.25913706742328E+03 0.48638479036721E+03 0.28785491571716E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38431109351526E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21959342758965E+00 0.00000000000000E+00 0.00000000000000E+00 0.21959342758965E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22868241027766E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22868241027766E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206514153199E+00 0.23076700248331E+00 0.35395309539113E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    910.00025000
 0.10575574819867E+00 0.32727002526141E+03 0.48114185855724E+03 0.47571758160494E+03 0.47329727968470E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13899335403146E+00 0.00000000000000E+00 -.28155855249956E+02
 0.33062597426832E-02 0.11985280903856E+01 0.24196526052450E+04 0.90736972696687E+03 0.66748539847958E+01
 0.25030702442984E+01 0.36718992000038E+03 0.30115248900903E+03 0.35766293616537E+03 0.39166859753217E+03
 0.30115039465825E+03 0.30115070911781E+03 0.35281841858393E+03 0.39164607792045E+03 0.30115033241711E+03
 0.30115070765785E+03 0.35766293616537E+03 0.39166859753217E+03 0.30115039465825E+03 0.30115070911781E+03
 0.35281841858393E+03 0.39164607792045E+03 0.30115033241711E+03 0.30115070765785E+03 0.44197522142024E+03
 0.35423975773850E+03 0.25847680738481E+04 0.23584937212542E+04 0.71766788882227E+03 0.10760893119574E+04
 0.35483308369106E+03 0.15519607229880E+04 0.14746602538223E+04 0.13849073704074E+04 0.21140221454681E+04
 0.14238419592798E+04 0.14743342069017E+04 0.12897355646704E+04 0.21138939023573E+04 0.15519607229879E+04
 0.14746602538223E+04 0.13849073704074E+04 0.21140221454681E+04 0.14238419592798E+04 0.14743342069017E+04
 0.12897355646703E+04 0.21138939023573E+04 0.20764567089471E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52670952869725E+03 0.14691360274438E+01
 0.14691360274438E+01 0.43642867945455E+01 0.00000000000000E+00 0.35395310159842E+03 0.35395310159842E+03
 0.35395310159842E+03 0.35395310159842E+03 0.00000000000000E+00 0.00000000000000E+00 0.14339741494452E+00
 0.00000000000000E+00 -.20130055768701E+02 0.10000000000000E-02 0.15502699145584E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51603917000988E+01 0.19351468875370E+01 0.35424293691736E+03 0.44197152851556E+03
 0.31964804298864E+03 0.31964804298864E+03 0.30115010391574E+03 0.30115010391530E+03 0.31964392680042E+03
 0.31964392680042E+03 0.30115010393661E+03 0.30115010393617E+03 0.31964804298864E+03 0.31964804298864E+03
 0.30115010391574E+03 0.30115010391530E+03 0.31964392680042E+03 0.31964392680042E+03 0.30115010393661E+03
 0.30115010393617E+03 0.31690543684611E+03 0.30115047239280E+03 -.21301123823048E+02 -.21608817848955E+02
 0.27710897577596E+03 0.61291427644687E+03 0.33441975579203E+03 0.25911699653593E+03 0.25614747572340E+03
 0.25911699653593E+03 0.48653418920642E+03 0.25913816508862E+03 0.25596562501272E+03 0.25913816508862E+03
 0.48638571084271E+03 0.25911699653593E+03 0.25614747572340E+03 0.25911699653593E+03 0.48653418920642E+03
 0.25913816508862E+03 0.25596562501272E+03 0.25913816508862E+03 0.48638571084271E+03 0.28785574942178E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38431088012871E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21959316160657E+00 0.00000000000000E+00 0.00000000000000E+00 0.21959316160657E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22867809363332E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22867809363332E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206514884871E+00 0.23076700614055E+00 0.35395310159842E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    920.33732135
 0.10554889513977E+00 0.32752716908492E+03 0.48152765250931E+03 0.47610945888384E+03 0.47369150174848E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13885497356126E+00 0.00000000000000E+00 -.28090453645817E+02
 0.33127389855702E-02 0.11993922496444E+01 0.24149201113782E+04 0.90559504176681E+03 0.66700447683999E+01
 0.25012667881500E+01 0.36766368294614E+03 0.30115271808975E+03 0.35808052410788E+03 0.39223390613546E+03
 0.30115043601919E+03 0.30115078320317E+03 0.35322228855622E+03 0.39221146724105E+03 0.30115036756870E+03
 0.30115078161027E+03 0.35808052410788E+03 0.39223390613546E+03 0.30115043601919E+03 0.30115078320317E+03
 0.35322228855622E+03 0.39221146724105E+03 0.30115036756870E+03 0.30115078161027E+03 0.44255439168853E+03
 0.35485943395924E+03 0.25888270905162E+04 0.23610616641597E+04 0.71623271085471E+03 0.10724837271514E+04
 0.35266985274239E+03 0.15550465384446E+04 0.14765378923329E+04 0.13868710674732E+04 0.21140761049022E+04
 0.14271974478343E+04 0.14762129645323E+04 0.12921099683225E+04 0.21139480769847E+04 0.15550465384446E+04
 0.14765378923329E+04 0.13868710674732E+04 0.21140761049022E+04 0.14271974478343E+04 0.14762129645323E+04
 0.12921099683225E+04 0.21139480769847E+04 0.20763156192186E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52710217548740E+03 0.14691355005125E+01
 0.14691355005125E+01 0.44159721512711E+01 0.00000000000000E+00 0.35419433459709E+03 0.35419433459709E+03
 0.35419433459709E+03 0.35419433459709E+03 0.00000000000000E+00 0.00000000000000E+00 0.14311181818927E+00
 0.00000000000000E+00 -.20035391947675E+02 0.10000000000000E-02 0.15543308310868E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51469094223695E+01 0.19300910333886E+01 0.35486277423645E+03 0.44255051999846E+03
 0.31982163943820E+03 0.31982163943820E+03 0.30115011642698E+03 0.30115011503930E+03 0.31981744999499E+03
 0.31981744999499E+03 0.30115011645004E+03 0.30115011506209E+03 0.31982163943820E+03 0.31982163943820E+03
 0.30115011642698E+03 0.30115011503930E+03 0.31981744999499E+03 0.31981744999499E+03 0.30115011645004E+03
 0.30115011506209E+03 0.31705709926777E+03 0.30115051695727E+03 -.24702933990489E+02 -.25639866106743E+02
 0.27845835394559E+03 0.61513539257195E+03 0.33528474685663E+03 0.26172667742138E+03 0.25729111350670E+03
 0.26172667742138E+03 0.48814340393399E+03 0.26174831669024E+03 0.25710757693059E+03 0.26174831669024E+03
 0.48799383664717E+03 0.26172667742138E+03 0.25729111350670E+03 0.26172667742138E+03 0.48814340393399E+03
 0.26174831669024E+03 0.25710757693059E+03 0.26174831669024E+03 0.48799383664717E+03 0.28790399945743E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38457143505448E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21904339571066E+00 0.00000000000000E+00 0.00000000000000E+00 0.21904339571066E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22827776421206E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22827776421206E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206825277701E+00 0.23061313647365E+00 0.35419433459709E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    930.11365923
 0.10539348843461E+00 0.32773976104889E+03 0.48188978805713E+03 0.47647431836090E+03 0.47405662410437E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13872606893034E+00 0.00000000000000E+00 -.28084702793608E+02
 0.33176234588260E-02 0.12001885277629E+01 0.24113646709115E+04 0.90426175159180E+03 0.66656194547299E+01
 0.24996072955237E+01 0.36810853814155E+03 0.30115295067640E+03 0.35847231257965E+03 0.39276712921647E+03
 0.30115047851613E+03 0.30115085928625E+03 0.35360075501316E+03 0.39274476495055E+03 0.30115040371711E+03
 0.30115085755868E+03 0.35847231257965E+03 0.39276712921647E+03 0.30115047851613E+03 0.30115085928625E+03
 0.35360075501316E+03 0.39274476495055E+03 0.30115040371711E+03 0.30115085755868E+03 0.44311382080396E+03
 0.35545016046544E+03 0.25926089419800E+04 0.23632135958406E+04 0.71458759662815E+03 0.10686333083165E+04
 0.35047277370521E+03 0.15579402873816E+04 0.14782624478232E+04 0.13885157824675E+04 0.21140686806247E+04
 0.14303478376586E+04 0.14779384638843E+04 0.12941548970304E+04 0.21139407579996E+04 0.15579402873816E+04
 0.14782624478232E+04 0.13885157824675E+04 0.21140686806247E+04 0.14303478376586E+04 0.14779384638843E+04
 0.12941548970304E+04 0.21139407579996E+04 0.20759655304923E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52746645618117E+03 0.14691354541788E+01
 0.14691354541788E+01 0.44648538406870E+01 0.00000000000000E+00 0.35442418365775E+03 0.35442418365775E+03
 0.35442418365775E+03 0.35442418365775E+03 0.00000000000000E+00 0.00000000000000E+00 0.14285044482822E+00
 0.00000000000000E+00 -.20008692578956E+02 0.10000000000000E-02 0.15580016778280E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51347826602809E+01 0.19255434976053E+01 0.35545356224371E+03 0.44310991907137E+03
 0.31998313747927E+03 0.31998313747927E+03 0.30115012801825E+03 0.30115012649241E+03 0.31997887987573E+03
 0.31997887987573E+03 0.30115012804327E+03 0.30115012651714E+03 0.31998313747927E+03 0.31998313747927E+03
 0.30115012801825E+03 0.30115012649241E+03 0.31997887987573E+03 0.31997887987573E+03 0.30115012804327E+03
 0.30115012651714E+03 0.31719807670539E+03 0.30115056230007E+03 -.27977120435822E+02 -.29642247064418E+02
 0.27976583634086E+03 0.61732809533982E+03 0.33616342981726E+03 0.26424887686241E+03 0.25840394522648E+03
 0.26424887686241E+03 0.48974761445954E+03 0.26427095330254E+03 0.25821871270819E+03 0.26427095330254E+03
 0.48959690974632E+03 0.26424887686241E+03 0.25840394522648E+03 0.26424887686241E+03 0.48974761445954E+03
 0.26427095330254E+03 0.25821871270819E+03 0.26427095330254E+03 0.48959690974632E+03 0.28795080335837E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38482829599379E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21889968478597E+00 0.00000000000000E+00 0.00000000000000E+00 0.21889968478597E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22808547537754E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22808547537754E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206873470330E+00 0.23046411997043E+00 0.35442418365775E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    943.28686881
 0.10522303964851E+00 0.32801459891111E+03 0.48237295181275E+03 0.47695893345027E+03 0.47454046268799E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13855631226095E+00 0.00000000000000E+00 -.28089474604352E+02
 0.33229972364326E-02 0.12012196850702E+01 0.24074651378851E+04 0.90279942670692E+03 0.66598975186896E+01
 0.24974615695086E+01 0.36870369480440E+03 0.30115329111997E+03 0.35899647892660E+03 0.39348125862266E+03
 0.30115054160121E+03 0.30115097216057E+03 0.35410703900967E+03 0.39345899179518E+03 0.30115045743311E+03
 0.30115097023645E+03 0.35899647892660E+03 0.39348125862266E+03 0.30115054160121E+03 0.30115097216057E+03
 0.35410703900967E+03 0.39345899179518E+03 0.30115045743311E+03 0.30115097023645E+03 0.44386779832893E+03
 0.35624182966889E+03 0.25976098260792E+04 0.23659552507762E+04 0.71222469969120E+03 0.10632511036064E+04
 0.34746528041677E+03 0.15617910058794E+04 0.14805153055803E+04 0.13906344754835E+04 0.21139915764550E+04
 0.14345427751795E+04 0.14801925120938E+04 0.12968117622302E+04 0.21138637449035E+04 0.15617910058794E+04
 0.14805153055803E+04 0.13906344754835E+04 0.21139915764550E+04 0.14345427751795E+04 0.14801925120938E+04
 0.12968117622302E+04 0.21138637449035E+04 0.20753623159492E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52794946872346E+03 0.14691354926245E+01
 0.14691354926245E+01 0.45307198886109E+01 0.00000000000000E+00 0.35473631445050E+03 0.35473631445050E+03
 0.35473631445050E+03 0.35473631445050E+03 0.00000000000000E+00 0.00000000000000E+00 0.14251098781617E+00
 0.00000000000000E+00 -.19987769489240E+02 0.10000000000000E-02 0.15627019164469E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51193384456772E+01 0.19197519171290E+01 0.35624527506764E+03 0.44386387851310E+03
 0.32019932405833E+03 0.32019932405833E+03 0.30115014712975E+03 0.30115014353563E+03 0.32019497500561E+03
 0.32019497500561E+03 0.30115014715797E+03 0.30115014356317E+03 0.32019932405833E+03 0.32019932405833E+03
 0.30115014712975E+03 0.30115014353563E+03 0.32019497500561E+03 0.32019497500561E+03 0.30115014715797E+03
 0.30115014356317E+03 0.31738677996039E+03 0.30115062883632E+03 -.32373189846219E+02 -.35143591750623E+02
 0.28155463139834E+03 0.62035352792805E+03 0.33739112337272E+03 0.26766565374510E+03 0.25993028064020E+03
 0.26766565374510E+03 0.49197155772530E+03 0.26768831803654E+03 0.25974271424957E+03 0.26768831803654E+03
 0.49181927023827E+03 0.26766565374510E+03 0.25993028064020E+03 0.26766565374510E+03 0.49197155772530E+03
 0.26768831803655E+03 0.25974271424957E+03 0.26768831803655E+03 0.49181927023827E+03 0.28802357750173E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38517879513562E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21879223691356E+00 0.00000000000000E+00 0.00000000000000E+00 0.21879223691356E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22788996018456E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22788996018456E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206879231300E+00 0.23026144042391E+00 0.35473631445050E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    953.20129910
 0.10511780608564E+00 0.32822068445058E+03 0.48273358644144E+03 0.47731956735171E+03 0.47490008223904E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13843201340082E+00 0.00000000000000E+00 -.28089962536261E+02
 0.33263236139717E-02 0.12019564430482E+01 0.24050576337183E+04 0.90189661264434E+03 0.66558152304686E+01
 0.24959307114257E+01 0.36914915571671E+03 0.30115356804507E+03 0.35938901695134E+03 0.39401502902617E+03
 0.30115059360036E+03 0.30115106514439E+03 0.35448640939789E+03 0.39399283325085E+03 0.30115050175236E+03
 0.30115106306086E+03 0.35938901695134E+03 0.39401502902617E+03 0.30115059360036E+03 0.30115106514439E+03
 0.35448640939789E+03 0.39399283325085E+03 0.30115050175236E+03 0.30115106306086E+03 0.44442902631657E+03
 0.35682894164118E+03 0.26013092554684E+04 0.23679646554672E+04 0.71046093949769E+03 0.10592538113585E+04
 0.34524056716333E+03 0.15646552087069E+04 0.14821731525612E+04 0.13922079211195E+04 0.21139073328988E+04
 0.14376631963285E+04 0.14818512212054E+04 0.12987839041745E+04 0.21137795589103E+04 0.15646552087069E+04
 0.14821731525612E+04 0.13922079211195E+04 0.21139073328988E+04 0.14376631963285E+04 0.14818512212054E+04
 0.12987839041745E+04 0.21137795589103E+04 0.20748889065462E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52830854681090E+03 0.14691354965557E+01
 0.14691354965557E+01 0.45802920400629E+01 0.00000000000000E+00 0.35497336768974E+03 0.35497336768974E+03
 0.35497336768974E+03 0.35497336768974E+03 0.00000000000000E+00 0.00000000000000E+00 0.14226468054340E+00
 0.00000000000000E+00 -.19969482842026E+02 0.10000000000000E-02 0.15660606334196E+01 0.80000000000000E+04
 0.30000000000000E+04 0.51083590438840E+01 0.19156346414565E+01 0.35683243680937E+03 0.44442506303643E+03
 0.32036211521004E+03 0.32036211521004E+03 0.30115016156282E+03 0.30115015761613E+03 0.32035769712011E+03
 0.32035769712011E+03 0.30115016159337E+03 0.30115015764594E+03 0.32036211521004E+03 0.32036211521004E+03
 0.30115016156282E+03 0.30115015761613E+03 0.32035769712011E+03 0.32035769712011E+03 0.30115016159337E+03
 0.30115015764594E+03 0.31752892834512E+03 0.30115068308662E+03 -.35615198114844E+02 -.39271862352418E+02
 0.28291595984613E+03 0.62266686459345E+03 0.33833632494809E+03 0.27022274717356E+03 0.26109411965023E+03
 0.27022274717356E+03 0.49367700970070E+03 0.27024585737032E+03 0.26090482057082E+03 0.27024585737032E+03
 0.49352355722957E+03 0.27022274717356E+03 0.26109411965023E+03 0.27022274717356E+03 0.49367700970070E+03
 0.27024585737032E+03 0.26090482057082E+03 0.27024585737032E+03 0.49352355722957E+03 0.28809948915152E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38544437813410E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21869210833857E+00 0.00000000000000E+00 0.00000000000000E+00 0.21869210833857E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22775349686770E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22775349686770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206893040413E+00 0.23010784899621E+00 0.35497336768974E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    963.11572939
 0.10501684717956E+00 0.32842817728126E+03 0.48309208027984E+03 0.47767797512471E+03 0.47525750850793E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13831081929635E+00 0.00000000000000E+00 -.28087068529666E+02
 0.33295211284647E-02 0.12026567382130E+01 0.24027479302073E+04 0.90103047382773E+03 0.66519396148623E+01
 0.24944773555734E+01 0.36959258366137E+03 0.30115386328357E+03 0.35977996489120E+03 0.39454573869629E+03
 0.30115064965115E+03 0.30115116532203E+03 0.35486439918817E+03 0.39452361212981E+03 0.30115054956315E+03
 0.30115116306899E+03 0.35977996489120E+03 0.39454573869629E+03 0.30115064965115E+03 0.30115116532203E+03
 0.35486439918817E+03 0.39452361212981E+03 0.30115054956315E+03 0.30115116306899E+03 0.44498592688903E+03
 0.35740962416097E+03 0.26049749229133E+04 0.23699650290343E+04 0.70870171327209E+03 0.10552885845617E+04
 0.34304336272326E+03 0.15674999311333E+04 0.14838071309567E+04 0.13937826120213E+04 0.21138111572094E+04
 0.14407626903841E+04 0.14834860287619E+04 0.13007532302760E+04 0.21136834263299E+04 0.15674999311333E+04
 0.14838071309567E+04 0.13937826120213E+04 0.21138111572094E+04 0.14407626903841E+04 0.14834860287619E+04
 0.13007532302760E+04 0.21136834263298E+04 0.20744133815609E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52866540412259E+03 0.14691354732391E+01
 0.14691354732391E+01 0.46298641915149E+01 0.00000000000000E+00 0.35521137329237E+03 0.35521137329237E+03
 0.35521137329237E+03 0.35521137329237E+03 0.00000000000000E+00 0.00000000000000E+00 0.14202593489777E+00
 0.00000000000000E+00 -.19947503955281E+02 0.10000000000000E-02 0.15692757014656E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50978932462463E+01 0.19117099673424E+01 0.35741318090069E+03 0.44498190590250E+03
 0.32052491686151E+03 0.32052491686151E+03 0.30115017715017E+03 0.30115017282271E+03 0.32052042967812E+03
 0.32052042967812E+03 0.30115017718318E+03 0.30115017285491E+03 0.32052491686151E+03 0.32052491686151E+03
 0.30115017715017E+03 0.30115017282271E+03 0.32052042967812E+03 0.32052042967812E+03 0.30115017718318E+03
 0.30115017285491E+03 0.31767112482984E+03 0.30115074103817E+03 -.38816176949110E+02 -.43395311817571E+02
 0.28428370396548E+03 0.62499458401672E+03 0.33928946153141E+03 0.27276774142439E+03 0.26226423618568E+03
 0.27276774142439E+03 0.49539479044635E+03 0.27279129965881E+03 0.26207321383760E+03 0.27279129965881E+03
 0.49524018309495E+03 0.27276774142439E+03 0.26226423618568E+03 0.27276774142439E+03 0.49539479044635E+03
 0.27279129965881E+03 0.26207321383760E+03 0.27279129965881E+03 0.49524018309495E+03 0.28818370957953E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38570974016490E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21857010211760E+00 0.00000000000000E+00 0.00000000000000E+00 0.21857010211760E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22760509773434E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22760509773434E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23206921094532E+00 0.22995399557083E+00 0.35521137329237E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    973.03015968
 0.10490866621088E+00 0.32863729660726E+03 0.48344870249971E+03 0.47803501646424E+03 0.47561385205598E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13819258450224E+00 0.00000000000000E+00 -.28072669516708E+02
 0.33329542270558E-02 0.12033219951015E+01 0.24002729875672E+04 0.90010237033771E+03 0.66482620882578E+01
 0.24930982830967E+01 0.37003405792392E+03 0.30115417769068E+03 0.36016938856841E+03 0.39507346700166E+03
 0.30115070999132E+03 0.30115127310987E+03 0.35524106493891E+03 0.39505140787539E+03 0.30115060107340E+03
 0.30115127067678E+03 0.36016938856841E+03 0.39507346700165E+03 0.30115070999132E+03 0.30115127310987E+03
 0.35524106493891E+03 0.39505140787539E+03 0.30115060107340E+03 0.30115127067678E+03 0.44553876007528E+03
 0.35798418484100E+03 0.26086219141378E+04 0.23719727406746E+04 0.70694930516229E+03 0.10513580443467E+04
 0.34087399265863E+03 0.15703320519767E+04 0.14854228326684E+04 0.13953631515365E+04 0.21137097085518E+04
 0.14438486907000E+04 0.14851025322049E+04 0.13027255272650E+04 0.21135820111121E+04 0.15703320519767E+04
 0.14854228326684E+04 0.13953631515365E+04 0.21137097085518E+04 0.14438486907000E+04 0.14851025322049E+04
 0.13027255272650E+04 0.21135820111121E+04 0.20739460028731E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52902110016864E+03 0.14691353572284E+01
 0.14691353572284E+01 0.46794363429669E+01 0.00000000000000E+00 0.35544938444823E+03 0.35544938444823E+03
 0.35544938444823E+03 0.35544938444823E+03 0.00000000000000E+00 0.00000000000000E+00 0.14179449662756E+00
 0.00000000000000E+00 -.19912368706319E+02 0.10000000000000E-02 0.15723562573702E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50879054683067E+01 0.19079645506150E+01 0.35798780261895E+03 0.44553468298960E+03
 0.32068768312766E+03 0.32068768312766E+03 0.30115019396193E+03 0.30115018922379E+03 0.32068312680726E+03
 0.32068312680726E+03 0.30115019399752E+03 0.30115018925851E+03 0.32068768312766E+03 0.32068768312766E+03
 0.30115019396193E+03 0.30115018922379E+03 0.32068312680726E+03 0.32068312680726E+03 0.30115019399752E+03
 0.30115018925851E+03 0.31781332940178E+03 0.30115080287183E+03 -.41985860386654E+02 -.47517875805406E+02
 0.28565042093032E+03 0.62731645921162E+03 0.34023778617665E+03 0.27529756731546E+03 0.26343319337790E+03
 0.27529756731546E+03 0.49710710758276E+03 0.27532157569949E+03 0.26324045625544E+03 0.27532157569949E+03
 0.49695135380719E+03 0.27529756731546E+03 0.26343319337790E+03 0.27529756731546E+03 0.49710710758276E+03
 0.27532157569949E+03 0.26324045625544E+03 0.27532157569949E+03 0.49695135380719E+03 0.28826510183925E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38597216298076E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21837146874838E+00 0.00000000000000E+00 0.00000000000000E+00 0.21837146874838E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22740692046390E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22740692046390E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23207000530965E+00 0.22980088324622E+00 0.35544938444823E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    982.94458997
 0.10478502569004E+00 0.32884744828553E+03 0.48380392277034E+03 0.47839155005043E+03 0.47597011508891E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13807687038829E+00 0.00000000000000E+00 -.28068661266666E+02
 0.33368866508290E-02 0.12039566222039E+01 0.23974443357291E+04 0.89904162589841E+03 0.66447576702188E+01
 0.24917841263321E+01 0.37047371363837E+03 0.30115451214192E+03 0.36055738973139E+03 0.39559830648389E+03
 0.30115077486742E+03 0.30115138893912E+03 0.35561650799322E+03 0.39557631312269E+03 0.30115065649888E+03
 0.30115138631503E+03 0.36055738973139E+03 0.39559830648389E+03 0.30115077486742E+03 0.30115138893912E+03
 0.35561650799322E+03 0.39557631312269E+03 0.30115065649888E+03 0.30115138631503E+03 0.44608771895726E+03
 0.35855268700797E+03 0.26122666714943E+04 0.23739985952763E+04 0.70521193825428E+03 0.10474733852647E+04
 0.33873538731912E+03 0.15731598807759E+04 0.14870304325533E+04 0.13969503212509E+04 0.21136158233094E+04
 0.14469298394321E+04 0.14867109147662E+04 0.13047026120722E+04 0.21134881571901E+04 0.15731598807759E+04
 0.14870304325533E+04 0.13969503212509E+04 0.21136158233094E+04 0.14469298394321E+04 0.14867109147662E+04
 0.13047026120722E+04 0.21134881571901E+04 0.20735007427517E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52937661495337E+03 0.14691353249346E+01
 0.14691353249346E+01 0.47290084944189E+01 0.00000000000000E+00 0.35568660030345E+03 0.35568660030345E+03
 0.35568660030345E+03 0.35568660030345E+03 0.00000000000000E+00 0.00000000000000E+00 0.14157012051024E+00
 0.00000000000000E+00 -.19889107840865E+02 0.10000000000000E-02 0.15753101040622E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50783651925870E+01 0.19043869472201E+01 0.35855636307486E+03 0.44608359139982E+03
 0.32085032984918E+03 0.32085032984918E+03 0.30115021207096E+03 0.30115020689045E+03 0.32084570435494E+03
 0.32084570435494E+03 0.30115021210929E+03 0.30115020692784E+03 0.32085032984918E+03 0.32085032984918E+03
 0.30115021207096E+03 0.30115020689045E+03 0.32084570435494E+03 0.32084570435494E+03 0.30115021210929E+03
 0.30115020692784E+03 0.31795547610154E+03 0.30115086877321E+03 -.45130718905814E+02 -.51641765832990E+02
 0.28700989206324E+03 0.62962712800005E+03 0.34118218647650E+03 0.27780906417882E+03 0.26459489554911E+03
 0.27780906417882E+03 0.49881065402812E+03 0.27783352483420E+03 0.26440045246306E+03 0.27783352483420E+03
 0.49865376349442E+03 0.27780906417882E+03 0.26459489554911E+03 0.27780906417882E+03 0.49881065402812E+03
 0.27783352483420E+03 0.26440045246306E+03 0.27783352483420E+03 0.49865376349442E+03 0.28834085626449E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38623583732717E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21824241979903E+00 0.00000000000000E+00 0.00000000000000E+00 0.21824241979903E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22725165088745E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22725165088745E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23207033929235E+00 0.22964800681492E+00 0.35568660030345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
    992.85902026
 0.10466690525692E+00 0.32905461232403E+03 0.48415764128689E+03 0.47874625660772E+03 0.47632436894878E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13796350613956E+00 0.00000000000000E+00 -.28065670990314E+02
 0.33406521741217E-02 0.12045632882127E+01 0.23947419794171E+04 0.89802824228139E+03 0.66414111058208E+01
 0.24905291646828E+01 0.37091150961593E+03 0.30115486753287E+03 0.36094390515548E+03 0.39612032637866E+03
 0.30115084453483E+03 0.30115151325595E+03 0.35599066454125E+03 0.39609839716070E+03 0.30115071606335E+03
 0.30115151042948E+03 0.36094390515548E+03 0.39612032637866E+03 0.30115084453483E+03 0.30115151325595E+03
 0.35599066454125E+03 0.39609839716070E+03 0.30115071606335E+03 0.30115151042948E+03 0.44663290148672E+03
 0.35911530342130E+03 0.26158896377055E+04 0.23759912441261E+04 0.70348591756051E+03 0.10436277940331E+04
 0.33662444688481E+03 0.15759756063696E+04 0.14886268609988E+04 0.13985140869500E+04 0.21135254969788E+04
 0.14499973887334E+04 0.14883081011102E+04 0.13066537708191E+04 0.21133978538758E+04 0.15759756063696E+04
 0.14886268609988E+04 0.13985140869500E+04 0.21135254969788E+04 0.14499973887334E+04 0.14883081011102E+04
 0.13066537708191E+04 0.21133978538758E+04 0.20730575166988E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52973017618273E+03 0.14691353008424E+01
 0.14691353008424E+01 0.47785806458709E+01 0.00000000000000E+00 0.35592404545445E+03 0.35592404545445E+03
 0.35592404545445E+03 0.35592404545445E+03 0.00000000000000E+00 0.00000000000000E+00 0.14135248198983E+00
 0.00000000000000E+00 -.19867080703110E+02 0.10000000000000E-02 0.15781389231594E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50692622066402E+01 0.19009733274901E+01 0.35911903482635E+03 0.44662872949065E+03
 0.32101290371773E+03 0.32101290371773E+03 0.30115023155295E+03 0.30115022589652E+03 0.32100820901647E+03
 0.32100820901647E+03 0.30115023159415E+03 0.30115022593672E+03 0.32101290371773E+03 0.32101290371773E+03
 0.30115023155295E+03 0.30115022589652E+03 0.32100820901647E+03 0.32100820901647E+03 0.30115023159415E+03
 0.30115022593672E+03 0.31809759917042E+03 0.30115093893266E+03 -.48242805634570E+02 -.55751086173855E+02
 0.28837078261856E+03 0.63194195258566E+03 0.34212931605401E+03 0.28030718868538E+03 0.26575792892872E+03
 0.28030718868538E+03 0.50051806677833E+03 0.28033210360463E+03 0.26556178691588E+03 0.28033210360463E+03
 0.50036004707973E+03 0.28030718868538E+03 0.26575792892872E+03 0.28030718868538E+03 0.50051806677833E+03
 0.28033210360463E+03 0.26556178691588E+03 0.28033210360463E+03 0.50036004707973E+03 0.28842226560850E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38649958628611E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21812085607807E+00 0.00000000000000E+00 0.00000000000000E+00 0.21812085607807E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22709982985954E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22709982985954E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23207062537330E+00 0.22949513614874E+00 0.35592404545445E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
   1000.00000000
 0.10458486294036E+00 0.32920367622227E+03 0.48441147470586E+03 0.47900067926197E+03 0.47657842616120E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13788331709445E+00 0.00000000000000E+00 -.28056466046189E+02
 0.33432725744364E-02 0.12049829740093E+01 0.23928650212879E+04 0.89732438298295E+03 0.66390979561994E+01
 0.24896617335748E+01 0.37122583462552E+03 0.30115513679547E+03 0.36122154752938E+03 0.39649445109778E+03
 0.30115089778394E+03 0.30115160823179E+03 0.35625959014037E+03 0.39647256705995E+03 0.30115076161955E+03
 0.30115160525236E+03 0.36122154752938E+03 0.39649445109778E+03 0.30115089778394E+03 0.30115160823179E+03
 0.35625959014037E+03 0.39647256705995E+03 0.30115076161955E+03 0.30115160525236E+03 0.44702163594311E+03
 0.35951535145828E+03 0.26184838388388E+04 0.23774166219943E+04 0.70227896001476E+03 0.10409282676837E+04
 0.33513791286887E+03 0.15779950329331E+04 0.14897709063075E+04 0.13996358256239E+04 0.21134647812128E+04
 0.14521967690806E+04 0.14894526871768E+04 0.13080515263738E+04 0.21133371599487E+04 0.15779950329331E+04
 0.14897709063075E+04 0.13996358256238E+04 0.21134647812128E+04 0.14521967690806E+04 0.14894526871768E+04
 0.13080515263738E+04 0.21133371599487E+04 0.20727546737198E+03 0.00000000000000E+00 0.50000000000000E-02
 0.25025000000000E+06 0.25025000000000E+06 0.50050000000000E+08 0.52998373957803E+03 0.14691352266795E+01
 0.14691352266795E+01 0.48142855445460E+01 0.00000000000000E+00 0.35609517312777E+03 0.35609517312777E+03
 0.35609517312777E+03 0.35609517312777E+03 0.00000000000000E+00 0.00000000000000E+00 0.14119974560729E+00
 0.00000000000000E+00 -.19844183747193E+02 0.10000000000000E-02 0.15801021193642E+01 0.80000000000000E+04
 0.30000000000000E+04 0.50629639071803E+01 0.18986114651926E+01 0.35951912091905E+03 0.44701743507203E+03
 0.32113023617692E+03 0.32113023617692E+03 0.30115024051204E+03 0.30115024044563E+03 0.32112549148016E+03
 0.32112549148016E+03 0.30115024055436E+03 0.30115024048793E+03 0.32113023617692E+03 0.32113023617692E+03
 0.30115024051204E+03 0.30115024044563E+03 0.32112549148016E+03 0.32112549148016E+03 0.30115024055436E+03
 0.30115024048793E+03 0.31820021592839E+03 0.30115099217279E+03 -.50448555746412E+02 -.58678951164936E+02
 0.28934966217580E+03 0.63360600266604E+03 0.34280959217936E+03 0.28208983620786E+03 0.26659443631992E+03
 0.28208983620786E+03 0.50174537668600E+03 0.28211508070246E+03 0.26639709710731E+03 0.28211508070246E+03
 0.50158657142776E+03 0.28208983620786E+03 0.26659443631992E+03 0.28208983620786E+03 0.50174537668600E+03
 0.28211508070246E+03 0.26639709710731E+03 0.28211508070246E+03 0.50158657142776E+03 0.28848704873508E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.38668940286780E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.21798638414814E+00 0.00000000000000E+00 0.00000000000000E+00 0.21798638414814E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.22698944316428E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.22698944316428E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.23207110586161E+00 0.22938537509625E+00 0.35609517312777E+03
 0.00000000000000E+00 0.00000000000000E+00 0.30115000000000E+03
