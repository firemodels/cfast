#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-24 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL24 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.200000                                                                                   
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
#CONDINIT 1000.000000 10.000000 26.000000 0.230000 0.001000 101325.000000                           
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
#ROOM#LOC_1 #LLNL-24           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-24           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-24           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-24           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-24           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-24           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-24           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-24           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-24           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-24           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-24           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-24           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-24           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-24           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-24           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-24           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-24           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-24           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL24     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL24     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL24     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL24     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL24     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL24     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL24     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL24     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL24     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL24     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL24     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL24     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL24     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL24     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL24     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL24     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL24     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL24     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.81887566433057E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.49624695706700E-03 0.49624695706700E-03 0.70288424608146E-03 0.70639866731187E-03
 0.00000000000000E+00 0.47025385538425E-03 0.55341088693632E-03 0.47025385538425E-03 0.55341088693632E-03
 0.46787340275544E-03 0.55340426534305E-03 0.46787340275544E-03 0.55340426534305E-03 0.47025385538425E-03
 0.55341088699499E-03 0.47025385538425E-03 0.55341088699499E-03 0.46787340269676E-03 0.55340426534305E-03
 0.46787340269676E-03 0.55340426534305E-03 0.57021595444352E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.70032534202759E-07 0.99965591098861E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.57789407613414E-03 0.57789407613414E-03
 0.70978627299668E-03 0.71333520436166E-03 0.00000000000000E+00 0.50513708638344E-03 0.55722721601200E-03
 0.50513708638344E-03 0.55722721601200E-03 0.50074052517753E-03 0.55721956312448E-03 0.50074052517753E-03
 0.55721956312448E-03 0.50513708644211E-03 0.55722721601200E-03 0.50513708644211E-03 0.55722721601200E-03
 0.50074052517753E-03 0.55721956306581E-03 0.50074052517753E-03 0.55721956306581E-03 0.46090856589459E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.53737616405926E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.53737616405926E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19999969945323E+00 0.23536136600722E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     11.22967957
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18686230865731E+02
 0.99981558123991E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000167328E+03 0.29915000000000E+03 0.29915000230851E+03 0.29915000230851E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000229664E+03 0.29915000229664E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000230851E+03 0.29915000230851E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000229664E+03 0.29915000229664E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000814568E+03
 0.29915000672361E+03 0.51021647979818E-03 0.51021647979818E-03 0.67127991697644E-03 0.67463631656132E-03
 .00000000000000E+00 0.47791922921467E-03 0.56783802315756E-03 0.47791922921467E-03 0.56783802315756E-03
 0.47544835120090E-03 0.56789592155564E-03 0.47544835120090E-03 0.56789592155564E-03 0.47791922927334E-03
 0.56783802315756E-03 0.47791922927334E-03 0.56783802315756E-03 0.47544835108355E-03 0.56789592155564E-03
 0.47544835108355E-03 0.56789592155564E-03 0.57052319792826E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999934561E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20653265050395E+02 0.99947393743314E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915000669807E+03 0.29915000817677E+03
 0.29915000247907E+03 0.29915000247907E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000245737E+03
 0.29915000245737E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000247907E+03 0.29915000247907E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000245737E+03 0.29915000245737E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000238791E+03 0.29915000000000E+03 0.55579163111096E-03 0.55579163111096E-03
 0.72272989587485E-03 0.72634354535422E-03 .00000000000000E+00 0.51319865733507E-03 0.56388343669773E-03
 0.51319865733507E-03 0.56388343669773E-03 0.50869533819270E-03 0.56399422757735E-03 0.50869533819270E-03
 0.56399422757735E-03 0.51319865733507E-03 0.56388343675640E-03 0.51319865733507E-03 0.56388343675640E-03
 0.50869533813403E-03 0.56399422751868E-03 0.50869533813403E-03 0.56399422751868E-03 0.46113164125156E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23450199146621E+00 0.00000000000000E+00 0.00000000000000E+00 0.23450199146621E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186029906E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186029906E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19930988861639E+00 0.23450176554358E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     30.00000000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18686230803129E+02
 0.99981558124053E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000277435E+03 0.29915000000000E+03 0.29915000380367E+03 0.29915000380367E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000378392E+03 0.29915000378392E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000380367E+03 0.29915000380367E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000378392E+03 0.29915000378392E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001299210E+03
 0.29915001074588E+03 0.51821706269340E-03 0.51821706269340E-03 0.65283318996767E-03 0.65609735591750E-03
 .00000000000000E+00 0.48208624427917E-03 0.57595414116763E-03 0.48208624427917E-03 0.57595414116763E-03
 0.47956962490039E-03 0.57605491880605E-03 0.47956962490039E-03 0.57605491880605E-03 0.48208624463121E-03
 0.57595414163701E-03 0.48208624463121E-03 0.57595414163701E-03 0.47956962495907E-03 0.57605491874737E-03
 0.47956962495907E-03 0.57605491874737E-03 0.57048321845362E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999940880E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20653264985576E+02 0.99950701533893E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001072652E+03 0.29915001301550E+03
 0.29915000408433E+03 0.29915000408433E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000404844E+03
 0.29915000404844E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000408433E+03 0.29915000408433E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000404844E+03 0.29915000404844E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000393849E+03 0.29915000000000E+03 0.54295180032630E-03 0.54295180032630E-03
 0.72990094300516E-03 0.73355044772019E-03 .00000000000000E+00 0.51763413489364E-03 0.56754839539798E-03
 0.51763413489364E-03 0.56754839539798E-03 0.51307706330045E-03 0.56773662088703E-03 0.51307706330045E-03
 0.56773662088703E-03 0.51763413489364E-03 0.56754839533931E-03 0.51763413489364E-03 0.56754839533931E-03
 0.51307706324177E-03 0.56773662088703E-03 0.51307706324177E-03 0.56773662088703E-03 0.46110393140454E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23450199107340E+00 0.00000000000000E+00 0.00000000000000E+00 0.23450199107340E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186016644E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186016644E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19930988861855E+00 0.23450176554629E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     40.00000000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18686230802235E+02
 0.99981558124054E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000323504E+03 0.29915000000000E+03 0.29915000442461E+03 0.29915000442461E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000440157E+03 0.29915000440157E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000442461E+03 0.29915000442461E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000440157E+03 0.29915000440157E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001491807E+03
 0.29915001234984E+03 0.52137182357232E-03 0.52137182357232E-03 0.64564279968275E-03 0.64887101368117E-03
 .00000000000000E+00 0.48370286347909E-03 0.57911077811379E-03 0.48370286347909E-03 0.57911077811379E-03
 0.48117009020792E-03 0.57922946942208E-03 0.48117009020792E-03 0.57922946942208E-03 0.48370286347909E-03
 0.57911077817247E-03 0.48370286347909E-03 0.57911077817247E-03 0.48117009026660E-03 0.57922946942208E-03
 0.48117009026660E-03 0.57922946942208E-03 0.57047213345025E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999943973E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20653264984647E+02 0.99952321200762E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001233541E+03 0.29915001493543E+03
 0.29915000475101E+03 0.29915000475101E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000470923E+03
 0.29915000470923E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000475101E+03 0.29915000475101E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000470923E+03 0.29915000470923E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000458380E+03 0.29915000000000E+03 0.53796364051626E-03 0.53796364051626E-03
 0.73270014044562E-03 0.73636364114785E-03 .00000000000000E+00 0.51936531159989E-03 0.56897877864957E-03
 0.51936531159989E-03 0.56897877864957E-03 0.51478979059422E-03 0.56919917861847E-03 0.51478979059422E-03
 0.56919917861847E-03 0.51936531159989E-03 0.56897877870824E-03 0.51936531159989E-03 0.56897877870824E-03
 0.51478979047687E-03 0.56919917861847E-03 0.51478979047687E-03 0.56919917861847E-03 0.46109907855163E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23450199106783E+00 0.00000000000000E+00 0.00000000000000E+00 0.23450199106783E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186016382E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186016382E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19930988861859E+00 0.23450176554632E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     40.00025000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.18686230802229E+02
 0.99981558124054E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000323505E+03 0.29915000000000E+03 0.29915000442463E+03 0.29915000442463E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000440158E+03 0.29915000440158E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000442463E+03 0.29915000442463E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000440158E+03 0.29915000440158E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001491812E+03
 0.29915001234988E+03 0.52137801544498E-03 0.52137801544498E-03 0.64565138537281E-03 0.64887964229968E-03
 .00000000000000E+00 0.48370960294629E-03 0.57911570194688E-03 0.48370960294629E-03 0.57911570194688E-03
 0.48117688289148E-03 0.57923439348986E-03 0.48117688289148E-03 0.57923439348986E-03 0.48370960294629E-03
 0.57911570194688E-03 0.48370960294629E-03 0.57911570194688E-03 0.48117688277413E-03 0.57923439348986E-03
 0.48117688277413E-03 0.57923439348986E-03 0.57047974679926E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999943973E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.20653264984429E+02 0.99952321241230E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001233545E+03 0.29915001493547E+03
 0.29915000475103E+03 0.29915000475103E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000470925E+03
 0.29915000470925E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000475103E+03 0.29915000475103E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000470925E+03 0.29915000470925E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000458381E+03 0.29915000000000E+03 0.53797161780647E-03 0.53797161780647E-03
 0.73271650774376E-03 0.73638009028248E-03 .00000000000000E+00 0.51937655574069E-03 0.56898782249647E-03
 0.51937655574069E-03 0.56898782249647E-03 0.51480122131497E-03 0.56920822287609E-03 0.51480122131497E-03
 0.56920822287609E-03 0.51937655568202E-03 0.56898782255514E-03 0.51937655568202E-03 0.56898782255514E-03
 0.51480122131497E-03 0.56920822287609E-03 0.51480122131497E-03 0.56920822287609E-03 0.46110833652719E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23450199106774E+00 0.00000000000000E+00 0.00000000000000E+00 0.23450199106774E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186015172E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23450186015172E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19930988861859E+00 0.23450176554633E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     50.00593041
 0.22352739374519E+01 0.29917868243434E+03 0.33490630115567E+03 0.30828596283067E+03 0.30754182650759E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22594734468227E+00 0.00000000000000E+00 0.18478630130780E+02
 0.10000864818345E-02 0.72475973494499E-01 0.79993082051520E+04 0.29997405769320E+04 0.11038140799319E+03
 0.41393027997447E+02 0.30047980457910E+03 0.29915000000001E+03 0.30030121575385E+03 0.30072806240061E+03
 0.29915000000001E+03 0.29915000000001E+03 0.30002703436911E+03 0.30071603868069E+03 0.29915000000001E+03
 0.29915000000001E+03 0.30030121575385E+03 0.30072806240061E+03 0.29915000000001E+03 0.29915000000001E+03
 0.30002703436911E+03 0.30071603868069E+03 0.29915000000001E+03 0.29915000000001E+03 0.30345163117883E+03
 0.29915467303167E+03 0.61118603544323E+03 0.60862447504376E+03 0.28543945974075E+03 0.58834418146544E+03
 0.30147752442599E+03 0.39045984768310E+03 0.19161517366858E+03 0.38874193577740E+03 0.50476268699058E+03
 0.29384154563659E+03 0.18694699760568E+03 0.29265064550993E+03 0.50022250366549E+03 0.39045984768310E+03
 0.19161517366858E+03 0.38874193577740E+03 0.50476268699058E+03 0.29384154563659E+03 0.18694699760568E+03
 0.29265064550993E+03 0.50022250366549E+03 0.52080146921700E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.36007967023928E+03 0.12945684109367E+01
 0.12945684109367E+01 0.20046247819147E-01 0.13068573790493E+01 0.29916352326510E+03 0.30566287877111E+03
 0.30000039163638E+03 0.29998484586199E+03 0.22999999940882E+00 0.00000000000000E+00 0.22916154521438E+00
 0.00000000000000E+00 0.16577146826267E+02 0.99984547436240E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915466243487E+03 0.30346223236257E+03
 0.29915225738763E+03 0.29934144475698E+03 0.29915000000001E+03 0.29915000000001E+03 0.29915227397359E+03
 0.29934145546515E+03 0.29915000000001E+03 0.29915000000001E+03 0.29915225738763E+03 0.29934144475698E+03
 0.29915000000001E+03 0.29915000000001E+03 0.29915227397359E+03 0.29934145546515E+03 0.29915000000001E+03
 0.29915000000001E+03 0.29922962187518E+03 0.29915000000001E+03 0.66076549063986E+00 0.66258518634772E+00
 0.50409107320133E+00 0.38518593467600E+02 0.38011981939033E+02 0.77336942064154E+00 -.31791164898161E+00
 0.77812606989569E+00 0.53815712005624E+02 0.77842744855511E+00 -.31396692741351E+00 0.78317518064025E+00
 0.53819554984013E+02 0.77336942064154E+00 -.31791164898161E+00 0.77812606989569E+00 0.53815712005624E+02
 0.77842744855517E+00 -.31396692741345E+00 0.78317518064031E+00 0.53819554984013E+02 0.11686271626065E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31224459067742E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.22454644020955E+00 0.00000000000000E+00 0.00000000000000E+00 0.22454644020955E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21591591754108E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21591591754108E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.20055161374596E+00 0.23603881875409E+00 0.29916352326510E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     60.01304250
 0.16809355344911E+01 0.29922598309935E+03 0.35990119245395E+03 0.32590415396500E+03 0.32318231680798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22275559983982E+00 0.00000000000000E+00 0.91371188945624E+01
 0.99983622129652E-03 0.12573542016882E+00 0.80000000000000E+04 0.30000000000000E+04 0.63625667208640E+02
 0.23859625203240E+02 0.30161012493402E+03 0.29915000000003E+03 0.30150180748237E+03 0.30280318337243E+03
 0.29915000000002E+03 0.29915000000002E+03 0.30092217618614E+03 0.30277591928179E+03 0.29915000000002E+03
 0.29915000000002E+03 0.30150180748237E+03 0.30280318337243E+03 0.29915000000002E+03 0.29915000000002E+03
 0.30092217618614E+03 0.30277591928179E+03 0.29915000000002E+03 0.29915000000002E+03 0.30922251734447E+03
 0.29916354185660E+03 0.65308150545777E+03 0.64734226180241E+03 0.30427287689939E+03 0.82524646622020E+03
 0.51945222493631E+03 0.44547916021563E+03 0.25781075675650E+03 0.44105793668965E+03 0.74036814661070E+03
 0.33452949216633E+03 0.25234264587862E+03 0.33151890266318E+03 0.73517714309471E+03 0.44547916021563E+03
 0.25781075675650E+03 0.44105793668965E+03 0.74036814661070E+03 0.33452949216633E+03 0.25234264587861E+03
 0.33151890266318E+03 0.73517714309470E+03 0.68173992331441E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.38619749623310E+03 0.12946372075545E+01
 0.12946372075545E+01 0.60074696182996E-01 0.11131850535876E+01 0.29915704037424E+03 0.31052955728115E+03
 0.30208974671952E+03 0.30200927942634E+03 0.22999999943922E+00 0.00000000000000E+00 0.22843828638674E+00
 0.00000000000000E+00 0.88418555838289E+01 0.99979079651564E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29916349460926E+03 0.30926028538708E+03
 0.29915585575889E+03 0.29955636729379E+03 0.29915000000002E+03 0.29915000000002E+03 0.29915586222801E+03
 0.29955642540238E+03 0.29915000000002E+03 0.29915000000002E+03 0.29915585575889E+03 0.29955636729379E+03
 0.29915000000002E+03 0.29915000000002E+03 0.29915586222801E+03 0.29955642540238E+03 0.29915000000002E+03
 0.29915000000002E+03 0.29937888334633E+03 0.29915000000003E+03 0.12666669274829E+01 0.12642178984121E+01
 0.14433531877923E+00 0.75834710016236E+02 0.75689653020863E+02 0.12847032896404E+01 -.86345643717658E+00
 0.12849907054520E+01 0.80543819772408E+02 0.12823129035104E+01 -.84707280207902E+00 0.12825983619162E+01
 0.80559710641922E+02 0.12847032896404E+01 -.86345643717658E+00 0.12849907054519E+01 0.80543819772408E+02
 0.12823129035105E+01 -.84707280207902E+00 0.12825983619162E+01 0.80559710641922E+02 0.25021218551181E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32126229121331E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.16644117933154E+00 0.00000000000000E+00 0.00000000000000E+00 0.16644117933154E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20253302076055E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20253302076055E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.20029426716104E+00 0.23572304368274E+00 0.29915704037424E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     70.00870926
 0.12984184073556E+01 0.29931553822558E+03 0.37436895940411E+03 0.34188537820637E+03 0.33771773347002E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22050239151879E+00 0.00000000000000E+00 0.26507968030113E+01
 0.99947309095770E-03 0.15959000641450E+00 0.80000000000000E+04 0.30000000000000E+04 0.50128452148950E+02
 0.18798169555856E+02 0.30248490242000E+03 0.29915000000005E+03 0.30243668941556E+03 0.30476247158249E+03
 0.29915000000004E+03 0.29915000000004E+03 0.30163660709959E+03 0.30472607486426E+03 0.29915000000004E+03
 0.29915000000004E+03 0.30243668941556E+03 0.30476247158249E+03 0.29915000000004E+03 0.29915000000004E+03
 0.30163660709959E+03 0.30472607486426E+03 0.29915000000004E+03 0.29915000000004E+03 0.31427646876694E+03
 0.29917605443054E+03 0.70814627254019E+03 0.69976308116103E+03 0.33609657563086E+03 0.96471835477895E+03
 0.62694129626993E+03 0.49165829283746E+03 0.32706431546014E+03 0.48484625928998E+03 0.90085972740783E+03
 0.37340223088185E+03 0.32161719777783E+03 0.36877409783925E+03 0.89577837043924E+03 0.49165829283746E+03
 0.32706431546014E+03 0.48484625928998E+03 0.90085972740783E+03 0.37340223088185E+03 0.32161719777783E+03
 0.36877409783925E+03 0.89577837043923E+03 0.81921094616847E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39816335756113E+03 0.12946849820315E+01
 0.12946849820315E+01 0.10005736320994E+00 0.95014419730734E+00 0.29915287680811E+03 0.31424844340580E+03
 0.30468646673389E+03 0.30451507213728E+03 0.22999999951563E+00 0.00000000000000E+00 0.22777877739534E+00
 0.00000000000000E+00 0.36919720210670E+01 0.99975388293061E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29917597999286E+03 0.31431584354547E+03
 0.29916053275146E+03 0.29977416755323E+03 0.29915000000004E+03 0.29915000000004E+03 0.29916050342922E+03
 0.29977430097530E+03 0.29915000000004E+03 0.29915000000004E+03 0.29916053275146E+03 0.29977416755323E+03
 0.29915000000004E+03 0.29915000000004E+03 0.29916050342922E+03 0.29977430097530E+03 0.29915000000004E+03
 0.29915000000004E+03 0.29955432364197E+03 0.29915000000005E+03 0.20393562830810E+01 0.20272981841406E+01
 -.17303559715822E+00 0.10654774193455E+03 0.10672164270970E+03 0.19404090184070E+01 -.12701783292706E+01
 0.19373059682767E+01 0.10028986485986E+03 0.19299586106413E+01 -.12413012081872E+01 0.19268705494660E+01
 0.10031765403316E+03 0.19404090184070E+01 -.12701783292706E+01 0.19373059682767E+01 0.10028986485986E+03
 0.19299586106411E+01 -.12413012081872E+01 0.19268705494659E+01 0.10031765403316E+03 0.37455575653056E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32780059998848E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12019948257593E+00 0.00000000000000E+00 0.00000000000000E+00 0.12019948257593E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19787358507083E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19787358507083E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.20012275094919E+00 0.23143848221592E+00 0.30441886424218E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     80.00781102
 0.99652882775574E+00 0.29945665750319E+03 0.38458448952759E+03 0.35630704330870E+03 0.35140189700601E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21857409689800E+00 0.00000000000000E+00 -.17864474810867E+01
 0.99895834081866E-03 0.18738512889359E+00 0.80000000000000E+04 0.30000000000000E+04 0.42692822249214E+02
 0.16009808343455E+02 0.30340389846591E+03 0.29915000000007E+03 0.30330806486473E+03 0.30663334660230E+03
 0.29915000000005E+03 0.29915000000005E+03 0.30232492380262E+03 0.30659169120199E+03 0.29915000000005E+03
 0.29915000000005E+03 0.30330806486473E+03 0.30663334660230E+03 0.29915000000005E+03 0.29915000000005E+03
 0.30232492380262E+03 0.30659169120199E+03 0.29915000000005E+03 0.29915000000005E+03 0.31882283763453E+03
 0.29919198170981E+03 0.81050199462947E+03 0.79927777388948E+03 0.35800908483629E+03 0.10544952502501E+04
 0.69469611998968E+03 0.53600875892877E+03 0.38480284934079E+03 0.52685156143990E+03 0.10191648504847E+04
 0.41388274983912E+03 0.37990464549607E+03 0.40764447290140E+03 0.10146830439648E+04 0.53600875892877E+03
 0.38480284934079E+03 0.52685156143990E+03 0.10191648504847E+04 0.41388274983912E+03 0.37990464549607E+03
 0.40764447290140E+03 0.10146830439648E+04 0.94806176149117E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40876634252558E+03 0.18105474962089E+01
 0.18105474962089E+01 0.14005377025504E+00 0.83707869763909E+00 0.29915048592446E+03 0.31737734393105E+03
 0.30720580088958E+03 0.30694091518727E+03 0.22999999951308E+00 0.00000000000000E+00 0.22712500518206E+00
 0.00000000000000E+00 0.41223864962955E+00 0.99972950250466E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29919188340916E+03 0.31885608318718E+03
 0.29916627879947E+03 0.29999862143682E+03 0.29915000000005E+03 0.29915000000005E+03 0.29916619559839E+03
 0.29999884729232E+03 0.29915000000005E+03 0.29915000000005E+03 0.29916627879947E+03 0.29999862143682E+03
 0.29915000000005E+03 0.29915000000005E+03 0.29916619559839E+03 0.29999884729232E+03 0.29915000000005E+03
 0.29915000000005E+03 0.29974389727327E+03 0.29915000000007E+03 0.28726343727227E+01 0.28476362955555E+01
 -.52694559228465E+00 0.13319118205901E+03 0.13372076237926E+03 0.26289211651480E+01 -.16670126182427E+01
 0.26209663167255E+01 0.11815949016212E+03 0.26096500272006E+01 -.16258682709232E+01 0.26017481553518E+01
 0.11819880717709E+03 0.26289211651480E+01 -.16670126182427E+01 0.26209663167255E+01 0.11815949016212E+03
 0.26096500272006E+01 -.16258682709232E+01 0.26017481553517E+01 0.11819880717709E+03 0.48754994198065E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33291971330812E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.80792475087566E-01 0.00000000000000E+00 0.00000000000000E+00 0.80792475087566E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19544207350468E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19544207350468E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.20001344242767E+00 0.22845755897292E+00 0.30821250843745E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     90.00184986
 0.76003786925106E+00 0.29966753596227E+03 0.39287958112413E+03 0.36926468639301E+03 0.36418080547352E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21670874969845E+00 0.00000000000000E+00 -.47182274761126E+01
 0.99822648133876E-03 0.21364502475320E+00 0.80000000000000E+04 0.30000000000000E+04 0.37445290426217E+02
 0.14041983909831E+02 0.30438186419595E+03 0.29915000000008E+03 0.30416291384825E+03 0.30846646545276E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30302074157103E+03 0.30842188854909E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30416291384825E+03 0.30846646545276E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30302074157103E+03 0.30842188854909E+03 0.29915000000006E+03 0.29915000000006E+03 0.32309252577132E+03
 0.29922058791460E+03 0.91301500974012E+03 0.89880561824484E+03 0.38692672994347E+03 0.11331143812229E+04
 0.74425301762972E+03 0.58110309660877E+03 0.44425510710000E+03 0.56962971359759E+03 0.11247934989769E+04
 0.45650910961477E+03 0.43987851732167E+03 0.44866910914023E+03 0.11208607226348E+04 0.58110309660877E+03
 0.44425510710000E+03 0.56962971359759E+03 0.11247934989769E+04 0.45650910961477E+03 0.43987851732167E+03
 0.44866910914023E+03 0.11208607226348E+04 0.10700882863414E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41899756329461E+03 0.16266295741332E+01
 0.16266295741332E+01 0.18002992562580E+00 0.74092207095499E+00 0.29915000000000E+03 0.32013651696910E+03
 0.30977026789251E+03 0.30941457625254E+03 0.22999999951375E+00 0.00000000000000E+00 0.22644889306744E+00
 0.00000000000000E+00 -.15270719386538E+01 0.99971198554621E-03 0.15353384941197E-01 0.80000000000000E+04
 0.30000000000000E+04 0.52105773616957E+03 0.19539665106359E+03 0.29922045715944E+03 0.32312067111393E+03
 0.29917547828142E+03 0.30023304074725E+03 0.29915000000006E+03 0.29915000000006E+03 0.29917526540828E+03
 0.30023337102948E+03 0.29915000000006E+03 0.29915000000006E+03 0.29917547828142E+03 0.30023304074725E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29917526540828E+03 0.30023337102948E+03 0.29915000000006E+03
 0.29915000000006E+03 0.29994725982858E+03 0.29915000000008E+03 0.53770749093258E+01 0.53245534290181E+01
 0.12706962449086E+01 0.15940246401597E+03 0.15812541428984E+03 0.43691731386680E+01 0.10937967718035E+00
 0.43542358288869E+01 0.13616243852159E+03 0.43154538057562E+01 0.16196910853398E+00 0.43006743182061E+01
 0.13621236017435E+03 0.43691731386680E+01 0.10937967718042E+00 0.43542358288868E+01 0.13616243852159E+03
 0.43154538057563E+01 0.16196910853398E+00 0.43006743182062E+01 0.13621236017435E+03 0.60851151203238E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33741078534148E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45200756404896E-01 0.00000000000000E+00 0.00000000000000E+00 0.45200756404896E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19424614875129E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19424614875129E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19987281449228E+00 0.22723770280773E+00 0.30964325937610E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    100.11473763
 0.57113211246629E+00 0.29997785795549E+03 0.39998141882052E+03 0.38094300383019E+03 0.37611112477687E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21481101942580E+00 0.00000000000000E+00 -.63686174522879E+01
 0.99717758988573E-03 0.23998296796878E+00 0.80000000000000E+04 0.30000000000000E+04 0.33335699061114E+02
 0.12500887147918E+02 0.30541452099003E+03 0.29915000000008E+03 0.30502857433382E+03 0.31029358501951E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30374450687258E+03 0.31024738299053E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30502857433382E+03 0.31029358501951E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30374450687258E+03 0.31024738299053E+03 0.29915000000006E+03 0.29915000000006E+03 0.32723735088256E+03
 0.29927788029806E+03 0.10162281948070E+04 0.99906405150810E+03 0.42038598882414E+03 0.12026468743282E+04
 0.78015895555996E+03 0.62751893095423E+03 0.50542255574004E+03 0.61378863752606E+03 0.12211825409506E+04
 0.50149539429250E+03 0.50151240301157E+03 0.49210486562247E+03 0.12177298144667E+04 0.62751893095423E+03
 0.50542255574004E+03 0.61378863752606E+03 0.12211825409506E+04 0.50149539429250E+03 0.50151240301157E+03
 0.49210486562247E+03 0.12177298144667E+04 0.11855090160442E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42876234694724E+03 0.14740440880992E+01
 0.14740440880992E+01 0.22048147670505E+00 0.65473260560859E+00 0.29915025043908E+03 0.32260595129920E+03
 0.31236780987218E+03 0.31193042034383E+03 0.22999999958398E+00 0.00000000000000E+00 0.22572657116276E+00
 0.00000000000000E+00 -.23252819415651E+01 0.99970327034482E-03 0.37193560003553E-01 0.80000000000000E+04
 0.30000000000000E+04 0.21509099960412E+03 0.80659124851545E+02 0.29927759630579E+03 0.32726360061927E+03
 0.29919226915619E+03 0.30048451502767E+03 0.29915000000006E+03 0.29915000000006E+03 0.29919176054084E+03
 0.30048495643898E+03 0.29915000000006E+03 0.29915000000006E+03 0.29919226915619E+03 0.30048451502767E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29919176054084E+03 0.30048495643898E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30016786529869E+03 0.29915000000008E+03 0.94971740917311E+01 0.93815448000458E+01
 0.50940993772248E+01 0.18529159360348E+03 0.18017202372937E+03 0.71597923576308E+01 0.38884085263799E+01
 0.71309373682622E+01 0.15442002250954E+03 0.70507913872029E+01 0.39509281147294E+01 0.70223782518761E+01
 0.15447897389232E+03 0.71597923576308E+01 0.38884085263798E+01 0.71309373682622E+01 0.15442002250954E+03
 0.70507913872029E+01 0.39509281147294E+01 0.70223782518761E+01 0.15447897389232E+03 0.73758866870344E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34119420817212E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.15208653845901E-01 0.36505416669300E-02 0.00000000000000E+00 0.15208653845901E-01 0.36505416669300E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19396191568348E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19396191568348E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19984030483698E+00 0.22619331625572E+00 0.31101990592836E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    110.09834853
 0.41904931186144E+00 0.30041038317302E+03 0.40561475839392E+03 0.39091948471355E+03 0.38669850045390E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21296485195480E+00 0.00000000000000E+00 -.74639508564991E+01
 0.99573110760749E-03 0.26552582446263E+00 0.80000000000000E+04 0.30000000000000E+04 0.30128896186238E+02
 0.11298336069839E+02 0.30647302816119E+03 0.29915000000009E+03 0.30589099731037E+03 0.31206998549248E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30448230039610E+03 0.31202302895936E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30589099731037E+03 0.31206998549248E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30448230039610E+03 0.31202302895936E+03 0.29915000000006E+03 0.29915000000006E+03 0.33112699698948E+03
 0.29935820700318E+03 0.11150571490607E+04 0.10952360317629E+04 0.45299452713672E+03 0.12559801024645E+04
 0.80072060269208E+03 0.67248830916825E+03 0.56281700667696E+03 0.65669013454996E+03 0.13041918731981E+04
 0.54587816259369E+03 0.55930600923992E+03 0.53511009197224E+03 0.13011439310771E+04 0.67248830916825E+03
 0.56281700667696E+03 0.65669013454996E+03 0.13041918731981E+04 0.54587816259369E+03 0.55930600923992E+03
 0.53511009197224E+03 0.13011439310771E+04 0.12878347948121E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43611293887708E+03 0.13516246862447E+01
 0.13516246862447E+01 0.26041592029037E+00 0.57854170781748E+00 0.29915286310694E+03 0.32466460846813E+03
 0.31482486931435E+03 0.31432580088369E+03 0.22999999949609E+00 0.00000000000000E+00 0.22499067295950E+00
 0.00000000000000E+00 -.27162124286165E+01 0.99969068096013E-03 0.57080157157219E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14015378370394E+03 0.52557668888979E+02 0.29935774381454E+03 0.33115349442109E+03
 0.29921524308992E+03 0.30074099755321E+03 0.29915000000006E+03 0.29915000000006E+03 0.29921433081531E+03
 0.30074154906389E+03 0.29915000000006E+03 0.29915000000006E+03 0.29921524308992E+03 0.30074099755321E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29921433081531E+03 0.30074154906389E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30039344687003E+03 0.29915000000009E+03 0.14257650479956E+02 0.14039679071132E+02
 0.96630464770868E+01 0.20789464351308E+03 0.19818328180361E+03 0.10380135332789E+02 0.83728029554179E+01
 0.10331345111263E+02 0.17078150274353E+03 0.10213290163060E+02 0.84432907275699E+01 0.10165402432820E+02
 0.17084752570526E+03 0.10380135332789E+02 0.83728029554179E+01 0.10331345111263E+02 0.17078150274353E+03
 0.10213290163060E+02 0.84432907275699E+01 0.10165402432820E+02 0.17084752570526E+03 0.85930453211637E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34351537524874E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11278560315368E-02 0.22874340955893E-01 0.00000000000000E+00 0.11278560315368E-02 0.22874340955893E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19422071611430E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19422071611430E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19982294692524E+00 0.22326049666036E+00 0.31507697472341E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    120.06231948
 0.30234090254041E+00 0.30098568579559E+03 0.41031959028188E+03 0.39930088649500E+03 0.39582880049100E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21110455754779E+00 0.00000000000000E+00 -.88748087770126E+01
 0.11641649685818E-02 0.29122318802677E+00 0.68718783126981E+04 0.25769543672618E+04 0.27470340030975E+02
 0.10301377511616E+02 0.30755745046452E+03 0.29915000000009E+03 0.30675973467275E+03 0.31381131236746E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30524006376351E+03 0.31376413918205E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30675973467275E+03 0.31381131236746E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30524006376351E+03 0.31376413918205E+03 0.29915000000006E+03 0.29915000000006E+03 0.33482488949227E+03
 0.29945970848139E+03 0.12075568333785E+04 0.11855220181004E+04 0.48467486099897E+03 0.12996022121752E+04
 0.81250397687119E+03 0.71569283003263E+03 0.61718926184077E+03 0.69807917069010E+03 0.13780684114449E+04
 0.58915543186790E+03 0.61401462640844E+03 0.57725654407668E+03 0.13753575050766E+04 0.71569283003263E+03
 0.61718926184077E+03 0.69807917069010E+03 0.13780684114449E+04 0.58915543186791E+03 0.61401462640844E+03
 0.57725654407668E+03 0.13753575050766E+04 0.13786457321232E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44267092873011E+03 0.12947698835045E+01
 0.12947698835045E+01 0.30027180409130E+00 0.52574780835523E+00 0.29915758678286E+03 0.32638470592901E+03
 0.31684164044973E+03 0.31629496171470E+03 0.22999999951237E+00 0.00000000000000E+00 0.22423249191842E+00
 0.00000000000000E+00 -.35525986007311E+01 0.99966664105119E-03 0.76091274435498E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10513689065336E+03 0.39426333995011E+02 0.29945908420103E+03 0.33485259249087E+03
 0.29924396919390E+03 0.30100075960600E+03 0.29915000000006E+03 0.29915000000006E+03 0.29924256722349E+03
 0.30100141808961E+03 0.29915000000006E+03 0.29915000000006E+03 0.29924396919390E+03 0.30100075960600E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29924256722349E+03 0.30100141808961E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30062186158303E+03 0.29915000000009E+03 0.19315861440446E+02 0.18951041076198E+02
 0.14623255329417E+02 0.22777269366879E+03 0.21307632206273E+03 0.13823160063082E+02 0.13230794300369E+02
 0.13748043557155E+02 0.18567416978892E+03 0.13598002279023E+02 0.13307834780724E+02 0.13524427809599E+02
 0.18574586176966E+03 0.13823160063082E+02 0.13230794300369E+02 0.13748043557155E+02 0.18567416978892E+03
 0.13598002279023E+02 0.13307834780724E+02 0.13524427809599E+02 0.18574586176966E+03 0.96834172908815E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34553380418757E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.55024874749759E-01 0.00000000000000E+00 0.00000000000000E+00 0.55024874749759E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19455978626529E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19455978626529E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19979140774846E+00 0.21548974869051E+00 0.32638470592901E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    130.00263712
 0.21888085389622E+00 0.30168628016103E+03 0.41436816356278E+03 0.40614686127691E+03 0.40337570605853E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20920200028612E+00 0.00000000000000E+00 -.10740718807884E+02
 0.16080597426317E-02 0.31748034846761E+00 0.49749395423006E+04 0.18656023283627E+04 0.25198410038964E+02
 0.94494037646114E+01 0.30864373276501E+03 0.29915000000009E+03 0.30762486275974E+03 0.31550503810544E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30600625010618E+03 0.31545794531460E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30762486275974E+03 0.31550503810544E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30600625010618E+03 0.31545794531460E+03 0.29915000000006E+03 0.29915000000006E+03 0.33833522989388E+03
 0.29957785336311E+03 0.12917128500830E+04 0.12679810718576E+04 0.51531770410829E+03 0.13366488104280E+04
 0.81875451779916E+03 0.75648504753485E+03 0.66881844308321E+03 0.73734425227173E+03 0.14437856882635E+04
 0.63043607913082E+03 0.66592364453314E+03 0.61768909726218E+03 0.14413520033518E+04 0.75648504753485E+03
 0.66881844308320E+03 0.73734425227173E+03 0.14437856882635E+04 0.63043607913082E+03 0.66592364453314E+03
 0.61768909726218E+03 0.14413520033518E+04 0.14578205965427E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44954313135483E+03 0.12947836296948E+01
 0.12947836296948E+01 0.34003307466223E+00 0.49029191909469E+00 0.29916525579138E+03 0.32784845934653E+03
 0.31847303073530E+03 0.31788633771627E+03 0.22999999951710E+00 0.00000000000000E+00 0.22343981574759E+00
 0.00000000000000E+00 -.49681059945714E+01 0.99962704465335E-03 0.94914965679704E-01 0.80000000000000E+04
 0.30000000000000E+04 0.84285970528572E+02 0.31607238948215E+02 0.29957711236694E+03 0.33836463426060E+03
 0.29927707796477E+03 0.30126033599664E+03 0.29915000000006E+03 0.29915000000006E+03 0.29927511935173E+03
 0.30126109849553E+03 0.29915000000006E+03 0.29915000000006E+03 0.29927707796477E+03 0.30126033599664E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29927511935173E+03 0.30126109849553E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30084798654998E+03 0.29915000000009E+03 0.24519216158119E+02 0.23966301031767E+02
 0.19859784476749E+02 0.24554005193447E+03 0.22558096853534E+03 0.17401101268123E+02 0.18336612378503E+02
 0.17295464196238E+02 0.19939025203669E+03 0.17117437647275E+02 0.18419047920106E+02 0.17014141282548E+02
 0.19946646833391E+03 0.17401101268122E+02 0.18336612378503E+02 0.17295464196237E+02 0.19939025203669E+03
 0.17117437647275E+02 0.18419047920106E+02 0.17014141282548E+02 0.19946646833391E+03 0.10663780179422E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34730479084202E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.84491822264175E-01 0.00000000000000E+00 0.00000000000000E+00 0.84491822264175E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19495460141536E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19495460141536E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19974095223077E+00 0.21447047174114E+00 0.32784845934653E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    140.21003503
 0.16149505143547E+00 0.30245730635183E+03 0.41798442424668E+03 0.41176540496447E+03 0.40956313314600E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20719122461335E+00 0.00000000000000E+00 -.12875852271323E+02
 0.21794634109437E-02 0.34522533977612E+00 0.36706282655767E+04 0.13764855995913E+04 0.23173269972558E+02
 0.86899762397091E+01 0.30976703526108E+03 0.29915000000009E+03 0.30851827137203E+03 0.31721923016953E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30680841760817E+03 0.31717246030049E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30851827137203E+03 0.31721923016953E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30680841760817E+03 0.31717246030049E+03 0.29915000000006E+03 0.29915000000006E+03 0.34179686053827E+03
 0.29972071662026E+03 0.13689464235283E+04 0.13436494455288E+04 0.54513393860766E+03 0.13676694648318E+04
 0.81980985653111E+03 0.79551754700920E+03 0.71892745088685E+03 0.77485336841586E+03 0.15056302011096E+04
 0.67015415313630E+03 0.71627258132063E+03 0.65659134064768E+03 0.15034319887650E+04 0.79551754700920E+03
 0.71892745088685E+03 0.77485336841586E+03 0.15056302011096E+04 0.67015415313630E+03 0.71627258132063E+03
 0.65659134064767E+03 0.15034319887650E+04 0.15284642212055E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45545696289703E+03 0.12947993596934E+01
 0.12947993596934E+01 0.38086266631199E+00 0.45541260527127E+00 0.29917775083176E+03 0.32913013847897E+03
 0.32003634188396E+03 0.31942102793147E+03 0.22999999951491E+00 0.00000000000000E+00 0.22258329715668E+00
 0.00000000000000E+00 -.67530390648298E+01 0.99956768008598E-03 0.11439225977112E+00 0.80000000000000E+04
 0.30000000000000E+04 0.69934801672827E+02 0.26225550627310E+02 0.29971985846271E+03 0.34182834504552E+03
 0.29931674034734E+03 0.30152909377808E+03 0.29915000000006E+03 0.29915000000006E+03 0.29931414006806E+03
 0.30152995682533E+03 0.29915000000006E+03 0.29915000000006E+03 0.29931674034734E+03 0.30152909377808E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29931414006806E+03 0.30152995682533E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30108204154431E+03 0.29915000000009E+03 0.30155745610908E+02 0.29358245964681E+02
 0.25618144063413E+02 0.26206801187060E+03 0.23632177708687E+03 0.21298096376797E+02 0.23935518381576E+02
 0.21156889586099E+02 0.21211170948188E+03 0.20955142467045E+02 0.24022177849666E+02 0.20817276987018E+02
 0.21219131033317E+03 0.21298096376797E+02 0.23935518381576E+02 0.21156889586099E+02 0.21211170948188E+03
 0.20955142467045E+02 0.24022177849666E+02 0.20817276987018E+02 0.21219131033317E+03 0.11617744721645E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34889967498144E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11102642492028E+00 0.00000000000000E+00 0.00000000000000E+00 0.11102642492028E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19543314123914E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19543314123914E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19967880704758E+00 0.21356506170087E+00 0.32913013847897E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    151.26731008
 0.12444930361703E+00 0.30318293442937E+03 0.42130140924992E+03 0.41640148860468E+03 0.41460079790771E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20495968246055E+00 0.00000000000000E+00 -.15214415565498E+02
 0.28282347537635E-02 0.37608242516845E+00 0.28286195088136E+04 0.10607323158051E+04 0.21271932599394E+02
 0.79769747247728E+01 0.31095579015226E+03 0.29915000000009E+03 0.30946795395339E+03 0.31902140529603E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30766863211545E+03 0.31897509446956E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30946795395339E+03 0.31902140529603E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30766863211545E+03 0.31897509446956E+03 0.29915000000006E+03 0.29915000000006E+03 0.34533903639601E+03
 0.29989661928253E+03 0.14412579958981E+04 0.14138550634229E+04 0.57459895638856E+03 0.13929827842372E+04
 0.81551083306671E+03 0.83361890152140E+03 0.76890367427778E+03 0.81090261172995E+03 0.15652600156604E+04
 0.70900053758001E+03 0.76646803511286E+03 0.69416872846216E+03 0.15632756278080E+04 0.83361890152140E+03
 0.76890367427778E+03 0.81090261172995E+03 0.15652600156604E+04 0.70900053758001E+03 0.76646803511286E+03
 0.69416872846216E+03 0.15632756278080E+04 0.15924025494470E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46032188902557E+03 0.12948165889370E+01
 0.12948165889370E+01 0.42509176649134E+00 0.41949336618815E+00 0.29919858969462E+03 0.33029962530580E+03
 0.32160183989215E+03 0.32096896897018E+03 0.22999999948190E+00 0.00000000000000E+00 0.22160438488589E+00
 0.00000000000000E+00 -.88243233881474E+01 0.99947762114073E-03 0.13585434248729E+00 0.80000000000000E+04
 0.30000000000000E+04 0.58886597612795E+02 0.22082474104798E+02 0.29989564249594E+03 0.34537258383615E+03
 0.29936452870975E+03 0.30181546802857E+03 0.29915000000006E+03 0.29915000000006E+03 0.29936118813768E+03
 0.30181643072622E+03 0.29915000000006E+03 0.29915000000006E+03 0.29936452870975E+03 0.30181546802857E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29936118813768E+03 0.30181643072622E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30133072820225E+03 0.29915000000009E+03 0.36512386042941E+02 0.35397471226094E+02
 0.32215653812489E+02 0.27827289338681E+03 0.24589616130526E+03 0.25718998144067E+02 0.30330476947824E+02
 0.25539345665278E+02 0.22458165681745E+03 0.25314521451107E+02 0.30419944471519E+02 0.25139440211546E+02
 0.22466323763320E+03 0.25718998144067E+02 0.30330476947824E+02 0.25539345665278E+02 0.22458165681745E+03
 0.25314521451107E+02 0.30419944471519E+02 0.25139440211546E+02 0.22466323763320E+03 0.12594963000273E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35039447236699E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13544276383230E+00 0.00000000000000E+00 0.00000000000000E+00 0.13544276383230E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19609385951071E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19609385951071E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19960756098615E+00 0.21272861376450E+00 0.33029962530580E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    160.52871093
 0.10788776915661E+00 0.30362401240211E+03 0.42359039474603E+03 0.41927609295774E+03 0.41765577500779E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20307039869499E+00 0.00000000000000E+00 -.17011403935264E+02
 0.32623850778227E-02 0.40232450422607E+00 0.24521936586772E+04 0.91957262200396E+03 0.19884446301349E+02
 0.74566673630057E+01 0.31193318377574E+03 0.29915000000009E+03 0.31025169031701E+03 0.32049637739215E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30838390617060E+03 0.32045056296100E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31025169031701E+03 0.32049637739215E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30838390617060E+03 0.32045056296100E+03 0.29915000000006E+03 0.29915000000006E+03 0.34814531661597E+03
 0.30006567763801E+03 0.14924716500033E+04 0.14625589677097E+04 0.59590433525922E+03 0.14063697567885E+04
 0.80748589985300E+03 0.86162379750776E+03 0.80626589579797E+03 0.83662232689664E+03 0.16075559122171E+04
 0.73753735458785E+03 0.80398901656848E+03 0.72106531197447E+03 0.16057247619868E+04 0.86162379750776E+03
 0.80626589579797E+03 0.83662232689664E+03 0.16075559122171E+04 0.73753735458785E+03 0.80398901656848E+03
 0.72106531197447E+03 0.16057247619868E+04 0.16350205338300E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46330825913852E+03 0.12948298285338E+01
 0.12948298285338E+01 0.46213736989124E+00 0.39096411574981E+00 0.29922399090058E+03 0.33112209889303E+03
 0.32280808850279E+03 0.32217051385612E+03 0.22999999948071E+00 0.00000000000000E+00 0.22074263923565E+00
 0.00000000000000E+00 -.10472119145777E+02 0.99937651559811E-03 0.15423353937101E+00 0.80000000000000E+04
 0.30000000000000E+04 0.51869392562897E+02 0.19451022211086E+02 0.30006459721139E+03 0.34818030462910E+03
 0.29940966083458E+03 0.30205476213967E+03 0.29915000000006E+03 0.29915000000006E+03 0.29940566017937E+03
 0.30205579788980E+03 0.29915000000006E+03 0.29915000000006E+03 0.29940966083458E+03 0.30205476213967E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29940566017937E+03 0.30205579788980E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30153881386023E+03 0.29915000000009E+03 0.41968465170173E+02 0.40537550602319E+02
 0.37974797891784E+02 0.29053009633064E+03 0.25236542444940E+03 0.29586532590883E+02 0.35889948041921E+02
 0.29376483052747E+02 0.23402855141392E+03 0.29134134055910E+02 0.35979830809778E+02 0.28929800030979E+02
 0.23410994469140E+03 0.29586532590883E+02 0.35889948041921E+02 0.29376483052747E+02 0.23402855141392E+03
 0.29134134055910E+02 0.35979830809778E+02 0.28929800030979E+02 0.23410994469140E+03 0.13365572818207E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35146742203213E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15205997979359E+00 0.00000000000000E+00 0.00000000000000E+00 0.15205997979359E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19678179128277E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19678179128277E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19955124715091E+00 0.21213689961598E+00 0.33112209889303E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    170.49400281
 0.99778826858301E-01 0.30396271817513E+03 0.42556429316649E+03 0.42151987233424E+03 0.41997623732372E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20104562515471E+00 0.00000000000000E+00 -.18682363371257E+02
 0.35275136324752E-02 0.43062183654447E+00 0.22678863453141E+04 0.85045737949279E+03 0.18577785242374E+02
 0.69666694658904E+01 0.31295105875878E+03 0.29915000000009E+03 0.31107191958509E+03 0.32202943335071E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30913517403093E+03 0.32198420839284E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31107191958509E+03 0.32202943335071E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30913517403093E+03 0.32198420839284E+03 0.29915000000006E+03 0.29915000000006E+03 0.35097708851491E+03
 0.30026770118794E+03 0.15384323737958E+04 0.15052552187866E+04 0.61474040862605E+03 0.14127810182063E+04
 0.79496690753714E+03 0.88753553681536E+03 0.84117106192434E+03 0.85977529247844E+03 0.16444748130778E+04
 0.76390872689442E+03 0.83904601094650E+03 0.74530780664827E+03 0.16427889729604E+04 0.88753553681536E+03
 0.84117106192434E+03 0.85977529247844E+03 0.16444748130778E+04 0.76390872689442E+03 0.83904601094650E+03
 0.74530780664827E+03 0.16427889729604E+04 0.16700494459524E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46560207795357E+03 0.12948421398874E+01
 0.12948421398874E+01 0.50199853743995E+00 0.36188077259752E+00 0.29926172767250E+03 0.33186427273864E+03
 0.32399878327385E+03 0.32336527038614E+03 0.22999999947087E+00 0.00000000000000E+00 0.21977286880722E+00
 0.00000000000000E+00 -.12043090498750E+02 0.99923499506372E-03 0.17449667051188E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45846146958176E+02 0.17192305109316E+02 0.30026648070545E+03 0.35101323832124E+03
 0.29946252917215E+03 0.30230899978286E+03 0.29915000000006E+03 0.29915000000006E+03 0.29945779785029E+03
 0.30231010306920E+03 0.29915000000006E+03 0.29915000000006E+03 0.29946252917215E+03 0.30230899978286E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29945779785029E+03 0.30231010306920E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30175982680459E+03 0.29915000000009E+03 0.47887721843305E+02 0.46072955577165E+02
 0.44290355179540E+02 0.30242935615358E+03 0.25791754919815E+03 0.33826013850708E+02 0.41978552356802E+02
 0.33590421756345E+02 0.24323408675060E+03 0.33325926519730E+02 0.42067797049945E+02 0.33097334951322E+02
 0.24331428997503E+03 0.33826013850708E+02 0.41978552356802E+02 0.33590421756345E+02 0.24323408675060E+03
 0.33325926519730E+02 0.42067797049945E+02 0.33097334951323E+02 0.24331428997503E+03 0.14143060592473E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35245028834509E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16631098526351E+00 0.00000000000000E+00 0.00000000000000E+00 0.16631098526351E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19768414945623E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19768414945623E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19949779238102E+00 0.21160250006305E+00 0.33186427273864E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    180.55133445
 0.97817865783827E-01 0.30423151970709E+03 0.42708002998520E+03 0.42307443695516E+03 0.42153005329295E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19903771487860E+00 0.00000000000000E+00 -.20015702600453E+02
 0.35982282366557E-02 0.45890101246621E+00 0.22233164418262E+04 0.83374366568482E+03 0.17432953475101E+02
 0.65373575531627E+01 0.31392990186955E+03 0.29915000000010E+03 0.31186542307350E+03 0.32349991982159E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30986286731367E+03 0.32345530436580E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31186542307350E+03 0.32349991982159E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30986286731367E+03 0.32345530436580E+03 0.29915000000006E+03 0.29915000000006E+03 0.35362133676138E+03
 0.30049063181656E+03 0.15761000903719E+04 0.15394212311379E+04 0.62923486715318E+03 0.14115111282105E+04
 0.77913008672157E+03 0.90937503406877E+03 0.87059141017119E+03 0.87888856495455E+03 0.16727825272659E+04
 0.78611712573561E+03 0.86860398630861E+03 0.76533184196017E+03 0.16712275127025E+04 0.90937503406877E+03
 0.87059141017119E+03 0.87888856495455E+03 0.16727825272659E+04 0.78611712573561E+03 0.86860398630861E+03
 0.76533184196017E+03 0.16712275127025E+04 0.16951122150509E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46715674249572E+03 0.12948519639149E+01
 0.12948519639149E+01 0.54222786399568E+00 0.33424065159441E+00 0.29931308963572E+03 0.33247592628418E+03
 0.32508634753078E+03 0.32446537634736E+03 0.22999999951924E+00 0.00000000000000E+00 0.21875162824548E+00
 0.00000000000000E+00 -.13318841599489E+02 0.99905094216294E-03 0.19548254009253E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40924371026760E+02 0.15346639135035E+02 0.30048928856410E+03 0.35365815518894E+03
 0.29951911197121E+03 0.30255905037702E+03 0.29915000000006E+03 0.29915000000006E+03 0.29951364346112E+03
 0.30256021046551E+03 0.29915000000006E+03 0.29915000000006E+03 0.29951911197121E+03 0.30255905037702E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29951364346112E+03 0.30256021046551E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30197660984142E+03 0.29915000000009E+03 0.53829100375180E+02 0.51590628440988E+02
 0.50708769533201E+02 0.31313051308272E+03 0.26216819970185E+03 0.38137863932595E+02 0.48154970355168E+02
 0.37890478519477E+02 0.25154499254737E+03 0.37594641739050E+02 0.48242455336694E+02 0.37355521381193E+02
 0.25162297892912E+03 0.38137863932595E+02 0.48154970355168E+02 0.37890478519477E+02 0.25154499254737E+03
 0.37594641739050E+02 0.48242455336694E+02 0.37355521381193E+02 0.25162297892912E+03 0.14870713564744E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35326502493819E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17701155148746E+00 0.00000000000000E+00 0.00000000000000E+00 0.17701155148746E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19855983513982E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19855983513982E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19945454974369E+00 0.21116477446436E+00 0.33247592628418E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    190.37420424
 0.99144732697883E-01 0.30447809545084E+03 0.42816935907419E+03 0.42408157998452E+03 0.42249711240898E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19713107829806E+00 0.00000000000000E+00 -.20985159302742E+02
 0.35500717820089E-02 0.48596634643573E+00 0.22534755608443E+04 0.84505333531661E+03 0.16462045280862E+02
 0.61732669803232E+01 0.31484358092301E+03 0.29915000000010E+03 0.31261043196823E+03 0.32486381783921E+03
 0.29915000000006E+03 0.29915000000006E+03 0.31054687226730E+03 0.32481983232823E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31261043196823E+03 0.32486381783921E+03 0.29915000000006E+03 0.29915000000006E+03
 0.31054687226730E+03 0.32481983232823E+03 0.29915000000006E+03 0.29915000000006E+03 0.35599710625210E+03
 0.30073006787507E+03 0.16056701404710E+04 0.15656277125360E+04 0.63931298285198E+03 0.14040899516908E+04
 0.76158040392457E+03 0.92696478022386E+03 0.89395006706468E+03 0.89405343382353E+03 0.16924901483271E+04
 0.80400998004401E+03 0.89208465853325E+03 0.78124822461052E+03 0.16910500859259E+04 0.92696478022386E+03
 0.89395006706468E+03 0.89405343382353E+03 0.16924901483271E+04 0.80400998004401E+03 0.89208465853326E+03
 0.78124822461052E+03 0.16910500859259E+04 0.17111652530903E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46813853645660E+03 0.12948591069740E+01
 0.12948591069740E+01 0.58151934313833E+00 0.30889766617754E+00 0.29937873074874E+03 0.33295148310215E+03
 0.32603778653606E+03 0.32543603276739E+03 0.22999999946260E+00 0.00000000000000E+00 0.21771634805256E+00
 0.00000000000000E+00 -.14264845702105E+02 0.99882256261540E-03 0.21648857755182E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36953450803126E+02 0.13857544051172E+02 0.30072861583643E+03 0.35603424869550E+03
 0.29957814510771E+03 0.30279817482965E+03 0.29915000000006E+03 0.29915000000006E+03 0.29957195907541E+03
 0.30279937768314E+03 0.29915000000006E+03 0.29915000000006E+03 0.29957814510771E+03 0.30279817482965E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29957195907541E+03 0.30279937768314E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30218363368289E+03 0.29915000000009E+03 0.59510173145955E+02 0.56821155462377E+02
 0.56955658934331E+02 0.32235554343243E+03 0.26511510620342E+03 0.42368760279731E+02 0.54146143631459E+02
 0.42127327791426E+02 0.25874003017494E+03 0.41789279656235E+02 0.54230439274096E+02 0.41557249805502E+02
 0.25881449079493E+03 0.42368760279731E+02 0.54146143631459E+02 0.42127327791426E+02 0.25874003017494E+03
 0.41789279656235E+02 0.54230439274096E+02 0.41557249805502E+02 0.25881449079493E+03 0.15523519269781E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35391247349414E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18448036957807E+00 0.00000000000000E+00 0.00000000000000E+00 0.18448036957807E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.19952027066047E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.19952027066047E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19942264292349E+00 0.21082746524084E+00 0.33295148310215E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    200.15415503
 0.10190833822074E+00 0.30473732987555E+03 0.42895989540760E+03 0.42474012366664E+03 0.42310111166327E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19529812305506E+00 0.00000000000000E+00 -.21669500660864E+02
 0.34537985681427E-02 0.51216295095638E+00 0.23162902648089E+04 0.86860884930335E+03 0.15620028713637E+02
 0.58575107676140E+01 0.31571681943309E+03 0.29915000000011E+03 0.31332659925570E+03 0.32615436818431E+03
 0.29915000000006E+03 0.29915000000006E+03 0.31120506646606E+03 0.32611105106309E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31332659925570E+03 0.32615436818431E+03 0.29915000000006E+03 0.29915000000006E+03
 0.31120506646606E+03 0.32611105106309E+03 0.29915000000006E+03 0.29915000000006E+03 0.35816635811320E+03
 0.30099091823761E+03 0.16294271698932E+04 0.15862325535743E+04 0.64605643458251E+03 0.13924487830853E+04
 0.74316206632992E+03 0.94142835225507E+03 0.91264292569894E+03 0.90638275979572E+03 0.17057179598450E+04
 0.81874936969921E+03 0.91088855443936E+03 0.79424296977478E+03 0.17043814507485E+04 0.94142835225507E+03
 0.91264292569894E+03 0.90638275979572E+03 0.17057179598450E+04 0.81874936969921E+03 0.91088855443937E+03
 0.79424296977478E+03 0.17043814507485E+04 0.17207984078802E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46876303470923E+03 0.12948641493308E+01
 0.12948641493308E+01 0.62063914632696E+00 0.28513407812725E+00 0.29946250438987E+03 0.33331692767788E+03
 0.32688156116137E+03 0.32630472916402E+03 0.23000000000000E+00 0.00000000000000E+00 0.21665335722570E+00
 0.00000000000000E+00 -.14946339799498E+02 0.99853642603719E-03 0.23785577877583E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33633826519472E+02 0.12612684944802E+02 0.30098934254586E+03 0.35820353181958E+03
 0.29964111195424E+03 0.30303191524860E+03 0.29915000000006E+03 0.29915000000006E+03 0.29963421946441E+03
 0.30303314640309E+03 0.29915000000006E+03 0.29915000000006E+03 0.29964111195424E+03 0.30303191524860E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29963421946441E+03 0.30303314640309E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30238605986009E+03 0.29915000000010E+03 0.64965395404266E+02 0.61797359899305E+02
 0.63067200626494E+02 0.33030226519084E+03 0.26691972856121E+03 0.46557787062213E+02 0.59989413936273E+02
 0.46343872130510E+02 0.26502129037148E+03 0.45948479192990E+02 0.60069289187722E+02 0.45744890167319E+02
 0.26509111712866E+03 0.46557787062213E+02 0.59989413936273E+02 0.46343872130510E+02 0.26502129037148E+03
 0.45948479192990E+02 0.60069289187722E+02 0.45744890167319E+02 0.26509111712866E+03 0.16113294351983E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35433946483482E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18961256600275E+00 0.00000000000000E+00 0.00000000000000E+00 0.18961256600275E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20052833794454E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20052833794454E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19939983796870E+00 0.21057081692582E+00 0.33331692767788E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    211.37820021
 0.10554945105548E+00 0.30504839817797E+03 0.42960964345771E+03 0.42522718643702E+03 0.42352509899003E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19328162979056E+00 0.00000000000000E+00 -.22196020592793E+02
 0.33346535292250E-02 0.54114849163783E+00 0.23990498352790E+04 0.89964368822964E+03 0.14783372999502E+02
 0.55437648748133E+01 0.31666982584630E+03 0.29915000000014E+03 0.31411225216884E+03 0.32754930345000E+03
 0.29915000000006E+03 0.29915000000006E+03 0.31192695900967E+03 0.32750674566692E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31411225216884E+03 0.32754930345000E+03 0.29915000000006E+03 0.29915000000006E+03
 0.31192695900967E+03 0.32750674566692E+03 0.29915000000006E+03 0.29915000000006E+03 0.36045187168873E+03
 0.30131216867302E+03 0.16514182783307E+04 0.16048745748653E+04 0.65053074999944E+03 0.13754833074187E+04
 0.72169990366922E+03 0.95510058904028E+03 0.92961223921189E+03 0.91787901040182E+03 0.17150565138217E+04
 0.83273137693839E+03 0.92797394850433E+03 0.80645541056510E+03 0.17138275581834E+04 0.95510058904028E+03
 0.92961223921189E+03 0.91787901040182E+03 0.17150565138217E+04 0.83273137693839E+03 0.92797394850433E+03
 0.80645541056510E+03 0.17138275581834E+04 0.17262660423757E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46921152192803E+03 0.12948680288619E+01
 0.12948680288619E+01 0.66553532704246E+00 0.25903773748510E+00 0.29958554241295E+03 0.33360573192858E+03
 0.32773072331462E+03 0.32718939914139E+03 0.23000000000000E+00 0.00000000000000E+00 0.21540709010612E+00
 0.00000000000000E+00 -.15480947696523E+02 0.99812106411638E-03 0.26274466719864E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30447811121327E+02 0.11417929170498E+02 0.30131044861429E+03 0.36048879095694E+03
 0.29971604397736E+03 0.30329079159262E+03 0.29915000000006E+03 0.29915000000006E+03 0.29970837946074E+03
 0.30329204056256E+03 0.29915000000006E+03 0.29915000000006E+03 0.29971604397736E+03 0.30329079159262E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29970837946074E+03 0.30329204056256E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30260953390166E+03 0.29915000000010E+03 0.70874231871626E+02 0.67148232288992E+02
 0.69803679887544E+02 0.33786167337201E+03 0.26770897508503E+03 0.51241482151776E+02 0.66409852448883E+02
 0.51093876386479E+02 0.27102825723391E+03 0.50606836372643E+02 0.66483719672683E+02 0.50469994471392E+02
 0.27109195739338E+03 0.51241482151776E+02 0.66409852448883E+02 0.51093876386480E+02 0.27102825723391E+03
 0.50606836372644E+02 0.66483719672683E+02 0.50469994471393E+02 0.27109195739338E+03 0.16710998888957E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35463542487147E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19348805532571E+00 0.00000000000000E+00 0.00000000000000E+00 0.19348805532571E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20158701902101E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20158701902101E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19938226402863E+00 0.21036887174075E+00 0.33360573192858E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    220.26380957
 0.10830865198305E+00 0.30529833437193E+03 0.42999101430656E+03 0.42548924894793E+03 0.42374273144800E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19175218459160E+00 0.00000000000000E+00 -.22468580199770E+02
 0.32497019898405E-02 0.56322070195788E+00 0.24617641940739E+04 0.92316157277772E+03 0.14204023346781E+02
 0.53265087550428E+01 0.31739040654096E+03 0.29915000000019E+03 0.31470894922328E+03 0.32859277886221E+03
 0.29915000000006E+03 0.29915000000006E+03 0.31247529397825E+03 0.32855082329800E+03 0.29915000000006E+03
 0.29915000000006E+03 0.31470894922328E+03 0.32859277886221E+03 0.29915000000006E+03 0.29915000000006E+03
 0.31247529397825E+03 0.32855082329800E+03 0.29915000000006E+03 0.29915000000006E+03 0.36211533268189E+03
 0.30158487224754E+03 0.16658132412157E+04 0.16167809352037E+04 0.65231404185286E+03 0.13606816630238E+04
 0.70510605096166E+03 0.96420646513961E+03 0.94038755800669E+03 0.92539338786372E+03 0.17192398649275E+04
 0.84208487609017E+03 0.93883347105458E+03 0.81452144927373E+03 0.17180883687307E+04 0.96420646513961E+03
 0.94038755800669E+03 0.92539338786372E+03 0.17192398649275E+04 0.84208487609017E+03 0.93883347105458E+03
 0.81452144927373E+03 0.17180883687307E+04 0.17277118012246E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46944685670815E+03 0.12948700371606E+01
 0.12948700371606E+01 0.70107776446353E+00 0.23927201683153E+00 0.29970612071273E+03 0.33375349584438E+03
 0.32832243976734E+03 0.32781310545698E+03 0.23000000000000E+00 0.00000000000000E+00 0.21440450802906E+00
 0.00000000000000E+00 -.15766368271643E+02 0.99771668641855E-03 0.28267114785437E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28301438122441E+02 0.10613039295915E+02 0.30158306541729E+03 0.36215200552271E+03
 0.29977758806898E+03 0.30348864362956E+03 0.29915000000006E+03 0.29915000000006E+03 0.29976934762300E+03
 0.30348989444769E+03 0.29915000000006E+03 0.29915000000006E+03 0.29977758806898E+03 0.30348864362956E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29976934762300E+03 0.30348989444769E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30278002944211E+03 0.29915000000010E+03 0.75239453497755E+02 0.71070807243256E+02
 0.74908297723084E+02 0.34286705487072E+03 0.26758421565902E+03 0.54861660132343E+02 0.71253943455540E+02
 0.54861660132343E+02 0.27499948625827E+03 0.54214098390869E+02 0.71322041238217E+02 0.54214098390869E+02
 0.27505742647475E+03 0.54861660132343E+02 0.71253943455540E+02 0.54861660132343E+02 0.27499948625827E+03
 0.54214098390869E+02 0.71322041238217E+02 0.54214098390869E+02 0.27505742647475E+03 0.17130381673309E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35479991602981E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19546625454364E+00 0.00000000000000E+00 0.00000000000000E+00 0.19546625454364E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20244297673320E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20244297673320E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19937314771843E+00 0.21026552737532E+00 0.33375349584438E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    230.00683158
 0.11091781915181E+00 0.30558737128195E+03 0.43033265666451E+03 0.42572049832981E+03 0.42393431572704E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19014143196944E+00 0.00000000000000E+00 -.22629191148188E+02
 0.31732578078157E-02 0.58650601590012E+00 0.25210684049358E+04 0.94540065185093E+03 0.13640098793739E+02
 0.51150370476522E+01 0.31815631699504E+03 0.29915000000030E+03 0.31534578379550E+03 0.32968684949146E+03
 0.29915000000007E+03 0.29915000000008E+03 0.31306116872052E+03 0.32964558459787E+03 0.29915000000007E+03
 0.29915000000008E+03 0.31534578379550E+03 0.32968684949146E+03 0.29915000000007E+03 0.29915000000008E+03
 0.31306116872052E+03 0.32964558459787E+03 0.29915000000007E+03 0.29915000000008E+03 0.36380055335608E+03
 0.30190427395061E+03 0.16794354518999E+04 0.16278564103589E+04 0.65323690594899E+03 0.13443880360119E+04
 0.68788494553317E+03 0.97292480885074E+03 0.95023404209094E+03 0.93249797685299E+03 0.17216819537862E+04
 0.85108200781894E+03 0.94876517031783E+03 0.82222490573902E+03 0.17206080221819E+04 0.97292480885074E+03
 0.95023404209094E+03 0.93249797685299E+03 0.17216819537862E+04 0.85108200781894E+03 0.94876517031783E+03
 0.82222490573902E+03 0.17206080221819E+04 0.17276142122569E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46965438076208E+03 0.12948712205925E+01
 0.12948712205925E+01 0.74004985252016E+00 0.21853600882507E+00 0.29986684381229E+03 0.33385193670787E+03
 0.32890062566724E+03 0.32842901568482E+03 0.23000000000000E+00 0.00000000000000E+00 0.21329246975350E+00
 0.00000000000000E+00 -.15937308774052E+02 0.99718024553135E-03 0.30469402597603E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26255847893222E+02 0.98459429599583E+01 0.30190234919049E+03 0.36383670057648E+03
 0.29984887784020E+03 0.30370018619321E+03 0.29915000000007E+03 0.29915000000007E+03 0.29983999954533E+03
 0.30370142354680E+03 0.29915000000007E+03 0.29915000000007E+03 0.29984887784020E+03 0.30370018619321E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29983999954533E+03 0.30370142354680E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30296273614895E+03 0.29915000000012E+03 0.79680512039255E+02 0.75038017572519E+02
 0.80249440952826E+02 0.34748681057585E+03 0.26683612241826E+03 0.58734715052019E+02 0.76303770249153E+02
 0.58734715052019E+02 0.27865495291997E+03 0.58080392047378E+02 0.76364619375026E+02 0.58080392047378E+02
 0.27870578344299E+03 0.58734715052019E+02 0.76303770249153E+02 0.58734715052019E+02 0.27865495291997E+03
 0.58080392047378E+02 0.76364619375026E+02 0.58080392047378E+02 0.27870578344299E+03 0.17538411797066E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35492320971573E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19659857508191E+00 0.00000000000000E+00 0.00000000000000E+00 0.19659857508191E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20316218306981E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20316218306981E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936803672734E+00 0.21019778425130E+00 0.33385193670787E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    240.00338368
 0.11311943308983E+00 0.30588192767653E+03 0.43063257314265E+03 0.42592866571174E+03 0.42411052502640E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18855741185578E+00 0.00000000000000E+00 -.22744884257297E+02
 0.31114974588095E-02 0.60941975230315E+00 0.25711092828792E+04 0.96416598107970E+03 0.13127241067862E+02
 0.49227154004481E+01 0.31891073205026E+03 0.29915000000048E+03 0.31597476167101E+03 0.33075580261294E+03
 0.29915000000009E+03 0.29915000000010E+03 0.31363970880009E+03 0.33071521791885E+03 0.29915000000009E+03
 0.29915000000010E+03 0.31597476167101E+03 0.33075580261294E+03 0.29915000000009E+03 0.29915000000010E+03
 0.31363970880009E+03 0.33071521791885E+03 0.29915000000009E+03 0.29915000000010E+03 0.36542300381573E+03
 0.30224880953544E+03 0.16916854453601E+04 0.16376257198563E+04 0.65318146975103E+03 0.13275314350845E+04
 0.67108405798474E+03 0.98083356992631E+03 0.95874316783952E+03 0.93882624889403E+03 0.17226722957912E+04
 0.85928445018656E+03 0.95735458677615E+03 0.82916798865236E+03 0.17216711339958E+04 0.98083356992631E+03
 0.95874316783952E+03 0.93882624889403E+03 0.17226722957912E+04 0.85928445018656E+03 0.95735458677615E+03
 0.82916798865236E+03 0.17216711339958E+04 0.17262838733885E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46984345747309E+03 0.12948720730573E+01
 0.12948720730573E+01 0.78003606092805E+00 0.19829402255124E+00 0.30006640703737E+03 0.33389967652233E+03
 0.32942705312085E+03 0.32899584693829E+03 0.23000000000000E+00 0.00000000000000E+00 0.21214067500632E+00
 0.00000000000000E+00 -.16063641613992E+02 0.99651581422441E-03 0.32743241801502E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24432522743160E+02 0.91621960286850E+01 0.30224678762412E+03 0.36545859065180E+03
 0.29992269864634E+03 0.30390878958495E+03 0.29915000000008E+03 0.29915000000009E+03 0.29991336630653E+03
 0.30391000197922E+03 0.29915000000008E+03 0.29915000000009E+03 0.29992269864634E+03 0.30390878958495E+03
 0.29915000000008E+03 0.29915000000009E+03 0.29991336630653E+03 0.30391000197922E+03 0.29915000000008E+03
 0.29915000000009E+03 0.30314255994471E+03 0.29915000000015E+03 0.83859393868481E+02 0.78773999957733E+02
 0.85431375481399E+02 0.35147295034879E+03 0.26561441798999E+03 0.62578401637527E+02 0.81187150614524E+02
 0.62756631852747E+02 0.28179993471411E+03 0.61923430480086E+02 0.81240084589828E+02 0.62116172931251E+02
 0.28184308320379E+03 0.62578401637527E+02 0.81187150614524E+02 0.62756631852747E+02 0.28179993471411E+03
 0.61923430480086E+02 0.81240084589829E+02 0.62116172931251E+02 0.28184308320379E+03 0.17910658635803E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35501264430659E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19739531186636E+00 0.00000000000000E+00 0.00000000000000E+00 0.19739531186636E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20387339543758E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20387339543758E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936453348737E+00 0.21016377572827E+00 0.33389967652233E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    250.01113365
 0.11485597491448E+00 0.30617448089774E+03 0.43090918098548E+03 0.42613367245740E+03 0.42429136288417E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18703739174502E+00 0.00000000000000E+00 -.22815267354696E+02
 0.30644537108987E-02 0.63139408140506E+00 0.26105794881313E+04 0.97896730804924E+03 0.12670375341811E+02
 0.47513907531791E+01 0.31963913694019E+03 0.29915000000080E+03 0.31658363279674E+03 0.33177857924838E+03
 0.29915000000011E+03 0.29915000000013E+03 0.31419994953966E+03 0.33173866020786E+03 0.29915000000011E+03
 0.29915000000012E+03 0.31658363279674E+03 0.33177857924838E+03 0.29915000000011E+03 0.29915000000013E+03
 0.31419994953966E+03 0.33173866020786E+03 0.29915000000011E+03 0.29915000000012E+03 0.36694607781867E+03
 0.30261272743019E+03 0.17026701292873E+04 0.16462375743329E+04 0.65255213371149E+03 0.13111786378084E+04
 0.65536374342831E+03 0.98796696532821E+03 0.96608939580547E+03 0.94443344618011E+03 0.17227412927121E+04
 0.86671770226395E+03 0.96477462020192E+03 0.83539034520897E+03 0.17218066174177E+04 0.98796696532821E+03
 0.96608939580546E+03 0.94443344618011E+03 0.17227412927121E+04 0.86671770226395E+03 0.96477462020192E+03
 0.83539034520897E+03 0.17218066174177E+04 0.17243283312809E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47003392240840E+03 0.12948725916637E+01
 0.12948725916637E+01 0.82006706080422E+00 0.17908784244539E+00 0.30030388390668E+03 0.33390841686476E+03
 0.32989630799553E+03 0.32950616081310E+03 0.23000000000000E+00 0.00000000000000E+00 0.21097916150621E+00
 0.00000000000000E+00 -.16141879270576E+02 0.99572701170707E-03 0.35029964127739E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22837591185728E+02 0.85640966946479E+01 0.30261062004287E+03 0.36698101897853E+03
 0.29999905682861E+03 0.30411015121703E+03 0.29915000000009E+03 0.29915000000010E+03 0.29998923451331E+03
 0.30411132622216E+03 0.29915000000009E+03 0.29915000000010E+03 0.29999905682861E+03 0.30411015121703E+03
 0.29915000000009E+03 0.29915000000010E+03 0.29998923451331E+03 0.30411132622216E+03 0.29915000000009E+03
 0.29915000000010E+03 0.30331603781265E+03 0.29915000000019E+03 0.87650850170085E+02 0.82172219866206E+02
 0.90328962501657E+02 0.35484660908347E+03 0.26406600176930E+03 0.66304576882347E+02 0.85785040029319E+02
 0.66771684751174E+02 0.28444994832546E+03 0.65656447999551E+02 0.85829495305094E+02 0.66142426029101E+02
 0.28448495064012E+03 0.66304576882347E+02 0.85785040029319E+02 0.66771684751174E+02 0.28444994832546E+03
 0.65656447999551E+02 0.85829495305094E+02 0.66142426029101E+02 0.28448495064012E+03 0.18241748072309E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35507750642417E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19784953315798E+00 0.00000000000000E+00 0.00000000000000E+00 0.19784953315798E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20450139824019E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20450139824019E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936269785141E+00 0.21015617713762E+00 0.33390841686476E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    260.00573210
 0.11618316902402E+00 0.30646360212468E+03 0.43117978132137E+03 0.42634980767883E+03 0.42448967544994E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18558073250880E+00 0.00000000000000E+00 -.22859486208202E+02
 0.30294474452747E-02 0.65241758381305E+00 0.26407455961906E+04 0.99027959857146E+03 0.12262085202002E+02
 0.45982819507508E+01 0.32034298683207E+03 0.29915000000135E+03 0.31717328912000E+03 0.33275883885593E+03
 0.29915000000014E+03 0.29915000000016E+03 0.31474276657804E+03 0.33271956790450E+03 0.29915000000013E+03
 0.29915000000016E+03 0.31717328912000E+03 0.33275883885593E+03 0.29915000000014E+03 0.29915000000016E+03
 0.31474276657804E+03 0.33271956790450E+03 0.29915000000013E+03 0.29915000000016E+03 0.36838058429051E+03
 0.30299514520669E+03 0.17127083890787E+04 0.16539990474949E+04 0.65159409421162E+03 0.12956230782697E+04
 0.64077101358702E+03 0.99450843554474E+03 0.97258775138265E+03 0.94949530005192E+03 0.17223178280859E+04
 0.87356253209242E+03 0.97134071843286E+03 0.84106487702281E+03 0.17214438119640E+04 0.99450843554474E+03
 0.97258775138265E+03 0.94949530005192E+03 0.17223178280859E+04 0.87356253209242E+03 0.97134071843286E+03
 0.84106487702281E+03 0.17214438119640E+04 0.17221327495495E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47023916175543E+03 0.12948729174834E+01
 0.12948729174834E+01 0.86004545459940E+00 0.16096400075672E+00 0.30058291554264E+03 0.33389092338079E+03
 0.33031666324822E+03 0.32996725130756E+03 0.23000000000000E+00 0.00000000000000E+00 0.20981302180774E+00
 0.00000000000000E+00 -.16190609339119E+02 0.10950832211266E-02 0.37320210719507E+00 0.73053808565981E+04
 0.27395178212243E+04 0.21436106189557E+02 0.80385398210839E+01 0.30299295606917E+03 0.36841474741020E+03
 0.30007738753546E+03 0.30430433946020E+03 0.29915000000010E+03 0.29915000000011E+03 0.30006718475436E+03
 0.30430546543293E+03 0.29915000000010E+03 0.29915000000011E+03 0.30007738753546E+03 0.30430433946020E+03
 0.29915000000010E+03 0.29915000000011E+03 0.30006718475436E+03 0.30430546543293E+03 0.29915000000010E+03
 0.29915000000011E+03 0.30348329583213E+03 0.29915000000025E+03 0.91047670335837E+02 0.85243615162688E+02
 0.94941696987324E+02 0.35773494623177E+03 0.26231854075951E+03 0.69911562858664E+02 0.90099657317926E+02
 0.70814081991047E+02 0.28670728788133E+03 0.69276392562106E+02 0.90135230888953E+02 0.70201749188758E+02
 0.28673383235402E+03 0.69911562858664E+02 0.90099657317926E+02 0.70814081991047E+02 0.28670728788133E+03
 0.69276392562106E+02 0.90135230888953E+02 0.70201749188758E+02 0.28673383235402E+03 0.18537179270516E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35512999718939E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19810087035594E+00 0.00000000000000E+00 0.00000000000000E+00 0.19810087035594E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20504978139017E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20504978139017E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936187075087E+00 0.21016621475922E+00 0.33389092338079E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    270.00847574
 0.11717980430986E+00 0.30674873940484E+03 0.43145536903142E+03 0.42658433621283E+03 0.42471114594405E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18418043484932E+00 0.00000000000000E+00 -.22890998386217E+02
 0.30036812972851E-02 0.67257863849347E+00 0.26633984128845E+04 0.99877440483169E+03 0.11894519900185E+02
 0.44604449625694E+01 0.32102648184687E+03 0.29915000000225E+03 0.31774699349264E+03 0.33370401807122E+03
 0.29915000000018E+03 0.29915000000022E+03 0.31527119311004E+03 0.33366537764022E+03 0.29915000000016E+03
 0.29915000000022E+03 0.31774699349264E+03 0.33370401807122E+03 0.29915000000018E+03 0.29915000000022E+03
 0.31527119311004E+03 0.33366537764022E+03 0.29915000000016E+03 0.29915000000022E+03 0.36974229124799E+03
 0.30339686323056E+03 0.17220520579967E+04 0.16611387111902E+04 0.65045472044575E+03 0.12809264876603E+04
 0.62721949361234E+03 0.10006102377154E+04 0.97848168735833E+03 0.95414981962927E+03 0.17216699360951E+04
 0.87996990984245E+03 0.97729701780692E+03 0.84632934362028E+03 0.17208514322784E+04 0.10006102377154E+04
 0.97848168735833E+03 0.95414981962927E+03 0.17216699360951E+04 0.87996990984245E+03 0.97729701780692E+03
 0.84632934362028E+03 0.17208514322784E+04 0.17199105194420E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47046551110852E+03 0.12948731496760E+01
 0.12948731496760E+01 0.90005642915626E+00 0.14388205365977E+00 0.30090755818436E+03 0.33385751628341E+03
 0.33069691119055E+03 0.33038726883993E+03 0.23000000000000E+00 0.00000000000000E+00 0.20864139684629E+00
 0.00000000000000E+00 -.16223278937626E+02 0.12250933280977E-02 0.39616091842038E+00 0.65301147402558E+04
 0.24487930275959E+04 0.20193814250781E+02 0.75726803440430E+01 0.30339459604295E+03 0.36977554353008E+03
 0.30015814912290E+03 0.30449239258292E+03 0.29915000000011E+03 0.29915000000012E+03 0.30014764280085E+03
 0.30449345865361E+03 0.29915000000011E+03 0.29915000000012E+03 0.30015814912290E+03 0.30449239258292E+03
 0.29915000000011E+03 0.29915000000012E+03 0.30014764280085E+03 0.30449345865361E+03 0.29915000000011E+03
 0.29915000000012E+03 0.30364523512967E+03 0.29915000000036E+03 0.94070437309553E+02 0.88022677998039E+02
 0.99296775659787E+02 0.36026031354908E+03 0.26046705401100E+03 0.73416460382393E+02 0.94158839813216E+02
 0.74934321484453E+02 0.28867010074271E+03 0.72799976279260E+02 0.94185237483927E+02 0.74344508787939E+02
 0.28868797700923E+03 0.73416460382393E+02 0.94158839813216E+02 0.74934321484453E+02 0.28867010074271E+03
 0.72799976279260E+02 0.94185237483927E+02 0.74344508787939E+02 0.28868797700923E+03 0.18803354993118E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35517967275683E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19824791936188E+00 0.00000000000000E+00 0.00000000000000E+00 0.19824791936188E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20552841499230E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20552841499230E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936157575865E+00 0.21018686600425E+00 0.33385751628341E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    280.01035775
 0.11791949820741E+00 0.30702886761752E+03 0.43174111668534E+03 0.42683911474187E+03 0.42495629350947E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18283431870499E+00 0.00000000000000E+00 -.22917004518893E+02
 0.29848394065274E-02 0.69190192991335E+00 0.26802111974618E+04 0.10050791990482E+04 0.11562332252783E+02
 0.43358745947937E+01 0.32169118860530E+03 0.29915000000371E+03 0.31830587081727E+03 0.33461763974689E+03
 0.29915000000024E+03 0.29915000000031E+03 0.31578627916219E+03 0.33457961046994E+03 0.29915000000021E+03
 0.29915000000031E+03 0.31830587081727E+03 0.33461763974689E+03 0.29915000000024E+03 0.29915000000031E+03
 0.31578627916219E+03 0.33457961046994E+03 0.29915000000021E+03 0.29915000000031E+03 0.37104001005552E+03
 0.30381750681492E+03 0.17308435456449E+04 0.16677867728424E+04 0.64922756640836E+03 0.12671176974450E+04
 0.61464399320458E+03 0.10063604763560E+04 0.98392093179924E+03 0.95847911309321E+03 0.17209590149081E+04
 0.88602597358790E+03 0.98279368022306E+03 0.85126368171907E+03 0.17201913251359E+04 0.10063604763560E+04
 0.98392093179924E+03 0.95847911309321E+03 0.17209590149081E+04 0.88602597358790E+03 0.98279368022306E+03
 0.85126368171906E+03 0.17201913251359E+04 0.17177818056434E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47071406550415E+03 0.12948733412983E+01
 0.12948733412983E+01 0.94006395720737E+00 0.12785326684717E+00 0.30128046409193E+03 0.33381645476320E+03
 0.33104323296492E+03 0.33077177182747E+03 0.23000000000000E+00 0.00000000000000E+00 0.20746658456599E+00
 0.00000000000000E+00 -.16247299935803E+02 0.13786813426477E-02 0.41913264113201E+00 0.58026461608861E+04
 0.21759923103323E+04 0.19087036453170E+02 0.71576386699386E+01 0.30381516707270E+03 0.37107223718779E+03
 0.30024155230394E+03 0.30467473240120E+03 0.29915000000012E+03 0.29915000000015E+03 0.30023081913813E+03
 0.30467572867426E+03 0.29915000000012E+03 0.29915000000015E+03 0.30024155230394E+03 0.30467473240120E+03
 0.29915000000012E+03 0.29915000000015E+03 0.30023081913813E+03 0.30467572867426E+03 0.29915000000012E+03
 0.29915000000015E+03 0.30380223412143E+03 0.29915000000055E+03 0.96730471764941E+02 0.90533219731101E+02
 0.10340994472985E+03 0.36251714009628E+03 0.25859014564278E+03 0.76825800733214E+02 0.97979686013938E+02
 0.79176567676479E+02 0.29041483951890E+03 0.76233132415682E+02 0.97996732628568E+02 0.78614317524517E+02
 0.29042394737450E+03 0.76825800733214E+02 0.97979686013938E+02 0.79176567676479E+02 0.29041483951890E+03
 0.76233132415682E+02 0.97996732628568E+02 0.78614317524517E+02 0.29042394737450E+03 0.19045028653025E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35523354213667E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19834429721723E+00 0.00000000000000E+00 0.00000000000000E+00 0.19834429721723E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20594548044814E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20594548044814E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936154205170E+00 0.21021263490707E+00 0.33381645476320E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    290.01923380
 0.11846808984678E+00 0.30730387404821E+03 0.43203951676770E+03 0.42711378565811E+03 0.42522366569797E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18153824775786E+00 0.00000000000000E+00 -.22941760364590E+02
 0.29710172656613E-02 0.71044436851500E+00 0.26926804136964E+04 0.10097551551362E+04 0.11260557975457E+02
 0.42227092407963E+01 0.32233951045933E+03 0.29915000000601E+03 0.31885179807075E+03 0.33550414470223E+03
 0.29915000000033E+03 0.29915000000046E+03 0.31628976091838E+03 0.33546670705706E+03 0.29915000000029E+03
 0.29915000000046E+03 0.31885179807075E+03 0.33550414470223E+03 0.29915000000033E+03 0.29915000000046E+03
 0.31628976091838E+03 0.33546670705706E+03 0.29915000000029E+03 0.29915000000046E+03 0.37228295469586E+03
 0.30425732136313E+03 0.17391893631164E+04 0.16740374408107E+04 0.64796523933457E+03 0.12541513700186E+04
 0.60294630448736E+03 0.10118276722327E+04 0.98901342517013E+03 0.96254602765982E+03 0.17202729333477E+04
 0.89179815541134E+03 0.98793914009636E+03 0.85592983259636E+03 0.17195518498309E+04 0.10118276722327E+04
 0.98901342517013E+03 0.96254602765982E+03 0.17202729333477E+04 0.89179815541134E+03 0.98793914009636E+03
 0.85592983259636E+03 0.17195518498309E+04 0.17157991074080E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47098385176157E+03 0.12948735237081E+01
 0.12948735237081E+01 0.98009946137003E+00 0.11285719763538E+00 0.30170422406653E+03 0.33377414200908E+03
 0.33136126129749E+03 0.33112596084258E+03 0.23000000000000E+00 0.00000000000000E+00 0.20628860912571E+00
 0.00000000000000E+00 -.16267262490944E+02 0.15618754398789E-02 0.44211733382862E+00 0.51220473769793E+04
 0.19207677663672E+04 0.18094744059732E+02 0.67855290223996E+01 0.30425491559816E+03 0.37231405847602E+03
 0.30032803255636E+03 0.30485210449359E+03 0.29915000000014E+03 0.29915000000018E+03 0.30031714848030E+03
 0.30485302198359E+03 0.29915000000014E+03 0.29915000000018E+03 0.30032803255636E+03 0.30485210449359E+03
 0.29915000000014E+03 0.29915000000018E+03 0.30031714848030E+03 0.30485302198359E+03 0.29915000000014E+03
 0.29915000000018E+03 0.30395494318329E+03 0.29915000000083E+03 0.99045741200323E+02 0.92802505177128E+02
 0.10730505585387E+03 0.36458648318878E+03 0.25674490205564E+03 0.80151517090811E+02 0.10158670584076E+03
 0.83596544529920E+02 0.29200725699134E+03 0.79587231393268E+02 0.10159432098665E+03 0.83066346906931E+02
 0.29200758296470E+03 0.80151517090811E+02 0.10158670584076E+03 0.83596544529920E+02 0.29200725699134E+03
 0.79587231393268E+02 0.10159432098665E+03 0.83066346906931E+02 0.29200758296470E+03 0.19266665375640E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35529683594841E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19842236390295E+00 0.00000000000000E+00 0.00000000000000E+00 0.19842236390295E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20631003408421E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20631003408421E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936159972084E+00 0.21023930306713E+00 0.33377414200908E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    300.01423819
 0.11887794790964E+00 0.30757311725474E+03 0.43235023679848E+03 0.42740582082600E+03 0.42550991845072E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18029214504726E+00 0.00000000000000E+00 -.22967392888931E+02
 0.29607737968884E-02 0.72820764061470E+00 0.27019963525776E+04 0.10132486322166E+04 0.10985877590143E+02
 0.41197040963036E+01 0.32297171223264E+03 0.29915000000954E+03 0.31938488697304E+03 0.33636483766042E+03
 0.29915000000048E+03 0.29915000000069E+03 0.31678173996266E+03 0.33632797065263E+03 0.29915000000040E+03
 0.29915000000069E+03 0.31938488697304E+03 0.33636483766042E+03 0.29915000000048E+03 0.29915000000069E+03
 0.31678173996266E+03 0.33632797065263E+03 0.29915000000040E+03 0.29915000000069E+03 0.37347526665843E+03
 0.30471518114825E+03 0.17471385720192E+04 0.16799361368982E+04 0.64669846420367E+03 0.12419921389658E+04
 0.59206018244116E+03 0.10170445876410E+04 0.99381708399768E+03 0.96638333066599E+03 0.17196546251735E+04
 0.89731760817020E+03 0.99279161834256E+03 0.86035827932907E+03 0.17189762550736E+04 0.10170445876410E+04
 0.99381708399768E+03 0.96638333066599E+03 0.17196546251735E+04 0.89731760817019E+03 0.99279161834256E+03
 0.86035827932907E+03 0.17189762550736E+04 0.17139814115627E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47127188820197E+03 0.12948737125777E+01
 0.12948737125777E+01 0.10200794789507E+01 0.98912776658952E-01 0.30217928565917E+03 0.33373553549665E+03
 0.33165465796840E+03 0.33145307180204E+03 0.23000000000000E+00 0.00000000000000E+00 0.20511062813014E+00
 0.00000000000000E+00 -.16284333250232E+02 0.17820635990048E-02 0.46505338550429E+00 0.44891776053715E+04
 0.16834416020143E+04 0.17202326118592E+02 0.64508722944720E+01 0.30471271704953E+03 0.37350517195035E+03
 0.30041785610002E+03 0.30502472810350E+03 0.29915000000015E+03 0.29915000000022E+03 0.30040689592748E+03
 0.30502555898599E+03 0.29915000000015E+03 0.29915000000022E+03 0.30041785610002E+03 0.30502472810350E+03
 0.29915000000015E+03 0.29915000000022E+03 0.30040689592748E+03 0.30502555898599E+03 0.29915000000015E+03
 0.29915000000022E+03 0.30410355494535E+03 0.29915000000129E+03 0.10102873283794E+03 0.94848493379739E+02
 0.11099473113145E+03 0.36652771061117E+03 0.25497800582405E+03 0.83395214865529E+02 0.10499379299997E+03
 0.88241057704713E+02 0.29349595102567E+03 0.82863241248595E+02 0.10499200561731E+03 0.87746751976069E+02
 0.29348758122201E+03 0.83395214865529E+02 0.10499379299997E+03 0.88241057704713E+02 0.29349595102567E+03
 0.82863241248594E+02 0.10499200561731E+03 0.87746751976069E+02 0.29348758122201E+03 0.19471375838311E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35537305193681E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19849935501315E+00 0.00000000000000E+00 0.00000000000000E+00 0.19849935501315E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20662977396318E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20662977396318E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936169579057E+00 0.21026368942184E+00 0.33373553549665E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    310.02122094
 0.11919168552175E+00 0.30783739788057E+03 0.43267328706423E+03 0.42771348704910E+03 0.42581269819014E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17909039609227E+00 0.00000000000000E+00 -.22994723047689E+02
 0.29529801719701E-02 0.74527417348311E+00 0.27091275708305E+04 0.10159228390614E+04 0.10734304615188E+02
 0.40253642306954E+01 0.32359078979114E+03 0.29915000001487E+03 0.31990757757699E+03 0.33720455483876E+03
 0.29915000000069E+03 0.29915000000106E+03 0.31726446236285E+03 0.33716823899016E+03 0.29915000000057E+03
 0.29915000000105E+03 0.31990757757699E+03 0.33720455483876E+03 0.29915000000069E+03 0.29915000000106E+03
 0.31726446236285E+03 0.33716823899016E+03 0.29915000000057E+03 0.29915000000105E+03 0.37462547162356E+03
 0.30519196515450E+03 0.17547565863816E+04 0.16855370979891E+04 0.64543664971833E+03 0.12305363610312E+04
 0.58187252806425E+03 0.10220553926947E+04 0.99838912404047E+03 0.97002984043654E+03 0.17191159050018E+04
 0.90262861055021E+03 0.99740882862267E+03 0.86458865042368E+03 0.17184768322131E+04 0.10220553926947E+04
 0.99838912404047E+03 0.97002984043654E+03 0.17191159050018E+04 0.90262861055021E+03 0.99740882862267E+03
 0.86458865042368E+03 0.17184768322131E+04 0.17123175062895E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47157609397170E+03 0.12948739139561E+01
 0.12948739139561E+01 0.10601074099679E+01 0.85970305918798E-01 0.30270718305411E+03 0.33370412512980E+03
 0.33192758073460E+03 0.33175709105307E+03 0.23000000000000E+00 0.00000000000000E+00 0.20393010815034E+00
 0.00000000000000E+00 -.16286065168333E+02 0.20503455497651E-02 0.48798981983438E+00 0.39017813367686E+04
 0.14631680012882E+04 0.16393784613612E+02 0.61476692301044E+01 0.30518945109912E+03 0.37465412122106E+03
 0.30051178957684E+03 0.30519360105013E+03 0.29915000000016E+03 0.29915000000028E+03 0.30050082586255E+03
 0.30519433826021E+03 0.29915000000016E+03 0.29915000000028E+03 0.30051178957684E+03 0.30519360105013E+03
 0.29915000000016E+03 0.29915000000028E+03 0.30050082586255E+03 0.30519433826021E+03 0.29915000000016E+03
 0.29915000000028E+03 0.30424892943733E+03 0.29915000000198E+03 0.10270170836443E+03 0.96694464300421E+02
 0.11450802900723E+03 0.36839567504951E+03 0.25331510589724E+03 0.86572375616253E+02 0.10822978433407E+03
 0.93182395339506E+02 0.29492554847902E+03 0.86076164145042E+02 0.10821867715757E+03 0.92727343391357E+02
 0.29490861656101E+03 0.86572375616253E+02 0.10822978433407E+03 0.93182395339506E+02 0.29492554847902E+03
 0.86076164145042E+02 0.10821867715757E+03 0.92727343391357E+02 0.29490861656101E+03 0.19662546881400E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35546477090828E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19858238377458E+00 0.00000000000000E+00 0.00000000000000E+00 0.19858238377458E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20691216970429E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20691216970429E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936223946670E+00 0.21028405064989E+00 0.33370412512980E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    320.00468317
 0.11943860938206E+00 0.30809603877100E+03 0.43300618727351E+03 0.42803315579523E+03 0.42612798177756E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17793510813982E+00 0.00000000000000E+00 -.23023694542248E+02
 0.29468750111306E-02 0.76161758496287E+00 0.27147401806263E+04 0.10180275677349E+04 0.10503959149512E+02
 0.39389846810670E+01 0.32419572392386E+03 0.29915000002272E+03 0.32041893993128E+03 0.33802251652008E+03
 0.29915000000103E+03 0.29915000000163E+03 0.31773706020391E+03 0.33798673069097E+03 0.29915000000083E+03
 0.29915000000162E+03 0.32041893993128E+03 0.33802251652008E+03 0.29915000000103E+03 0.29915000000163E+03
 0.31773706020391E+03 0.33798673069097E+03 0.29915000000083E+03 0.29915000000162E+03 0.37573399283367E+03
 0.30568551671651E+03 0.17620491043238E+04 0.16908490635024E+04 0.64418826957582E+03 0.12197509168927E+04
 0.57234170596903E+03 0.10268649046852E+04 0.10027434367086E+04 0.97349425572983E+03 0.17186564581809E+04
 0.90773450351345E+03 0.10018048409640E+04 0.86862716374858E+03 0.17180534584127E+04 0.10268649046852E+04
 0.10027434367086E+04 0.97349425572983E+03 0.17186564581809E+04 0.90773450351345E+03 0.10018048409640E+04
 0.86862716374858E+03 0.17180534584127E+04 0.17107966701594E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47189262231364E+03 0.12948741274285E+01
 0.12948741274285E+01 0.11000412588559E+01 0.74058984168235E-01 0.30328499737497E+03 0.33368247628731E+03
 0.33218167201429E+03 0.33203938109214E+03 0.23000000000000E+00 0.00000000000000E+00 0.20275162584583E+00
 0.00000000000000E+00 -.16288852324099E+02 0.23801138159702E-02 0.51083735401288E+00 0.33611837998339E+04
 0.12604439249377E+04 0.15660561893440E+02 0.58727107100401E+01 0.30568296194262E+03 0.37576135390930E+03
 0.30061011347896E+03 0.30535863363191E+03 0.29915000000017E+03 0.29915000000035E+03 0.30059921602239E+03
 0.30535927140497E+03 0.29915000000017E+03 0.29915000000035E+03 0.30061011347896E+03 0.30535863363191E+03
 0.29915000000017E+03 0.29915000000035E+03 0.30059921602239E+03 0.30535927140497E+03 0.29915000000017E+03
 0.29915000000035E+03 0.30439098883540E+03 0.29915000000302E+03 0.10407552415616E+03 0.98346100567073E+02
 0.11785025123439E+03 0.37022083766900E+03 0.25178133517843E+03 0.89676932044477E+02 0.11130133762518E+03
 0.98467279745764E+02 0.29632144771487E+03 0.89219249827097E+02 0.11128109363257E+03 0.98054145512512E+02
 0.29629617718170E+03 0.89676932044477E+02 0.11130133762519E+03 0.98467279745764E+02 0.29632144771488E+03
 0.89219249827097E+02 0.11128109363257E+03 0.98054145512512E+02 0.29629617718170E+03 0.19841659330516E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35557300199745E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19867268698509E+00 0.00000000000000E+00 0.00000000000000E+00 0.19867268698509E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20716176199884E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20716176199884E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936267470374E+00 0.21029814690189E+00 0.33368247628731E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    330.08560555
 0.11963230330701E+00 0.30835300628280E+03 0.43335164073160E+03 0.42836701588182E+03 0.42645781393755E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17681085523083E+00 0.00000000000000E+00 -.23050350652983E+02
 0.29421035234893E-02 0.77745931378077E+00 0.27191429316233E+04 0.10196785993588E+04 0.10289927534724E+02
 0.38587228255213E+01 0.32479472499682E+03 0.29915000003426E+03 0.32092586158624E+03 0.33883025610337E+03
 0.29915000000153E+03 0.29915000000250E+03 0.31820589342396E+03 0.33879498577304E+03 0.29915000000122E+03
 0.29915000000248E+03 0.32092586158624E+03 0.33883025610337E+03 0.29915000000153E+03 0.29915000000250E+03
 0.31820589342396E+03 0.33879498577304E+03 0.29915000000122E+03 0.29915000000248E+03 0.37681761114854E+03
 0.30620257138132E+03 0.17691332645102E+04 0.16959659303526E+04 0.64293885252697E+03 0.12094445395220E+04
 0.56329099273236E+03 0.10315494363610E+04 0.10069529353329E+04 0.97683844508700E+03 0.17182635535143E+04
 0.91271516349011E+03 0.10060533482935E+04 0.87254260815465E+03 0.17176941079109E+04 0.10315494363610E+04
 0.10069529353329E+04 0.97683844508700E+03 0.17182635535143E+04 0.91271516349011E+03 0.10060533482935E+04
 0.87254260815465E+03 0.17176941079109E+04 0.17093875770748E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47222356700019E+03 0.12948743238404E+01
 0.12948743238404E+01 0.11403649483704E+01 0.63042671612599E-01 0.30397378945109E+03 0.33367175671680E+03
 0.33242359725154E+03 0.33230725602177E+03 0.23000000000000E+00 0.00000000000000E+00 0.20156131429850E+00
 0.00000000000000E+00 -.16287979673780E+02 0.27960234798773E-02 0.53386452315345E+00 0.28612063015834E+04
 0.10729523630938E+04 0.14985075151174E+02 0.56194031816901E+01 0.30619998270023E+03 0.37684365343972E+03
 0.30066813674060E+03 0.30552222574650E+03 0.29915000000018E+03 0.29915000000046E+03 0.30065746489385E+03
 0.30552275803246E+03 0.29915000000018E+03 0.29915000000046E+03 0.30066813674060E+03 0.30552222574650E+03
 0.29915000000018E+03 0.29915000000046E+03 0.30065746489385E+03 0.30552275803246E+03 0.29915000000018E+03
 0.29915000000046E+03 0.30453179827351E+03 0.29915000000457E+03 0.10516398726753E+03 0.99982819436843E+02
 0.12107348097777E+03 0.37204684019959E+03 0.25036799181694E+03 0.93003176879595E+02 0.11425774479084E+03
 0.93003176879595E+02 0.29771741111750E+03 0.92584672256451E+02 0.11422850424028E+03 0.92584672256451E+02
 0.29768398522562E+03 0.93003176879595E+02 0.11425774479084E+03 0.93003176879595E+02 0.29771741111750E+03
 0.92584672256451E+02 0.11422850424028E+03 0.92584672256451E+02 0.29768398522562E+03 0.20001524198091E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35570070197492E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19874019013625E+00 0.00000000000000E+00 0.00000000000000E+00 0.19874019013625E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20736111901008E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20736111901008E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936316627802E+00 0.21030542332710E+00 0.33367175671680E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    340.01376830
 0.11979867828076E+00 0.30860105007200E+03 0.43369944986614E+03 0.42870390888268E+03 0.42679069052976E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17574358361857E+00 0.00000000000000E+00 -.23082338884021E+02
 0.29380172932240E-02 0.79243845107325E+00 0.27229247487585E+04 0.10210967807844E+04 0.10095421277407E+02
 0.37857829790275E+01 0.32537381004185E+03 0.29915000005052E+03 0.32141645462459E+03 0.33960935203361E+03
 0.29915000000226E+03 0.29915000000378E+03 0.31865994859644E+03 0.33957457036303E+03 0.29915000000179E+03
 0.29915000000376E+03 0.32141645462459E+03 0.33960935203361E+03 0.29915000000226E+03 0.29915000000378E+03
 0.31865994859644E+03 0.33957457036303E+03 0.29915000000179E+03 0.29915000000376E+03 0.37785282665986E+03
 0.30672929207281E+03 0.17758464204800E+04 0.17007643872006E+04 0.64171484811920E+03 0.11998027404177E+04
 0.55487931805787E+03 0.10360042035851E+04 0.10109283195677E+04 0.97998510370584E+03 0.17179330512600E+04
 0.91745802465753E+03 0.10100643630508E+04 0.87624331973999E+03 0.17173940805017E+04 0.10360042035851E+04
 0.10109283195677E+04 0.97998510370584E+03 0.17179330512600E+04 0.91745802465753E+03 0.10100643630508E+04
 0.87624331973999E+03 0.17173940805017E+04 0.17080964658016E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47255760030273E+03 0.12948745595414E+01
 0.12948745595414E+01 0.11800775993710E+01 0.53155012336350E-01 0.30469686633336E+03 0.33367398472835E+03
 0.33264713200451E+03 0.33255325375873E+03 0.23000000000000E+00 0.00000000000000E+00 0.20038876360176E+00
 0.00000000000000E+00 -.16293918002673E+02 0.33161269851063E-02 0.55649765254792E+00 0.24124528511514E+04
 0.90466981918179E+03 0.14375622185237E+02 0.53908583194637E+01 0.30672667433159E+03 0.37787756952366E+03
 0.30074436993679E+03 0.30568077722138E+03 0.29915000000019E+03 0.29915000000061E+03 0.30073369194928E+03
 0.30568120135472E+03 0.29915000000019E+03 0.29915000000061E+03 0.30074436993679E+03 0.30568077722138E+03
 0.29915000000019E+03 0.29915000000061E+03 0.30073369194928E+03 0.30568120135472E+03 0.29915000000019E+03
 0.29915000000061E+03 0.30466825054445E+03 0.29915000000679E+03 0.10597232089032E+03 0.10139848392572E+03
 0.12411988855187E+03 0.37386528552682E+03 0.24912479753219E+03 0.96154065137436E+02 0.11704692011518E+03
 0.96154065137436E+02 0.29911059376397E+03 0.95778182893283E+02 0.11700903687540E+03 0.95778182893283E+02
 0.29906938262696E+03 0.96154065137436E+02 0.11704692011518E+03 0.96154065137436E+02 0.29911059376397E+03
 0.95778182893283E+02 0.11700903687540E+03 0.95778182893283E+02 0.29906938262697E+03 0.20163248294904E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35584498286145E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19884611932423E+00 0.00000000000000E+00 0.00000000000000E+00 0.19884611932423E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20756289306407E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20756289306407E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936334449523E+00 0.21030419473958E+00 0.33367398472835E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    350.00007078
 0.11994889054671E+00 0.30884669387699E+03 0.43405566840187E+03 0.42904944254162E+03 0.42713210460720E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17470843588831E+00 0.00000000000000E+00 -.23115007612780E+02
 0.29343377289665E-02 0.80690871497066E+00 0.27263392080017E+04 0.10223772030006E+04 0.99143804640788E+01
 0.37178926740295E+01 0.32594631708085E+03 0.29915000007346E+03 0.32190197912819E+03 0.34037794464603E+03
 0.29915000000336E+03 0.29915000000569E+03 0.31910964656646E+03 0.34034363673227E+03 0.29915000000264E+03
 0.29915000000566E+03 0.32190197912819E+03 0.34037794464603E+03 0.29915000000336E+03 0.29915000000569E+03
 0.31910964656646E+03 0.34034363673227E+03 0.29915000000264E+03 0.29915000000566E+03 0.37886437810479E+03
 0.30727499487625E+03 0.17823554949263E+04 0.17053730887989E+04 0.64049342255802E+03 0.11905709820918E+04
 0.54687509242101E+03 0.10403391731262E+04 0.10147702247235E+04 0.98301981611274E+03 0.17176471028263E+04
 0.92207924959929E+03 0.10139395608650E+04 0.87982665964079E+03 0.17171364396427E+04 0.10403391731262E+04
 0.10147702247235E+04 0.98301981611274E+03 0.17176471028263E+04 0.92207924959929E+03 0.10139395608650E+04
 0.87982665964079E+03 0.17171364396427E+04 0.17068834018523E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47290024510372E+03 0.12948748002567E+01
 0.12948748002567E+01 0.12200228093149E+01 0.44148608626059E-01 0.30545730024786E+03 0.33368924498222E+03
 0.33285831092967E+03 0.33278397430717E+03 0.23000000000000E+00 0.00000000000000E+00 0.19920937851584E+00
 0.00000000000000E+00 -.16300858208614E+02 0.39926230665411E-02 0.57921273986442E+00 0.20036952816913E+04
 0.75138573063422E+03 0.13811850895877E+02 0.51794440859540E+01 0.30727236181518E+03 0.37888781933689E+03
 0.30082451907353E+03 0.30583807822689E+03 0.29915000000020E+03 0.29915000000085E+03 0.30081396148884E+03
 0.30583839004794E+03 0.29915000000020E+03 0.29915000000085E+03 0.30082451907353E+03 0.30583807822689E+03
 0.29915000000020E+03 0.29915000000085E+03 0.30081396148884E+03 0.30583839004794E+03 0.29915000000020E+03
 0.29915000000085E+03 0.30480362125043E+03 0.29915000000997E+03 0.10653661306010E+03 0.10260266550017E+03
 0.12706453748732E+03 0.37571879895916E+03 0.24801893878440E+03 0.99283815133420E+02 0.11973934206825E+03
 0.99283815133420E+02 0.30053398564283E+03 0.98951742761889E+02 0.11969306847381E+03 0.98951742761889E+02
 0.30048527015893E+03 0.99283815133420E+02 0.11973934206825E+03 0.99283815133420E+02 0.30053398564283E+03
 0.98951742761889E+02 0.11969306847381E+03 0.98951742761889E+02 0.30048527015893E+03 0.20317351751728E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35600666845201E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19895386929469E+00 0.00000000000000E+00 0.00000000000000E+00 0.19895386929469E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20774295372382E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20774295372382E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936341380321E+00 0.21029463581931E+00 0.33368924498222E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    360.01644009
 0.12008714088494E+00 0.30908925726942E+03 0.43441801635327E+03 0.42940122557026E+03 0.42747964533518E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17370749723244E+00 0.00000000000000E+00 -.23147351630037E+02
 0.29309592892517E-02 0.82084439985750E+00 0.27294817875285E+04 0.10235556703232E+04 0.97460614964161E+01
 0.36547730611560E+01 0.32651107701962E+03 0.29915000010539E+03 0.32238140955773E+03 0.34113476528850E+03
 0.29915000000495E+03 0.29915000000848E+03 0.31955401748515E+03 0.34110091479122E+03 0.29915000000387E+03
 0.29915000000843E+03 0.32238140955773E+03 0.34113476528850E+03 0.29915000000495E+03 0.29915000000848E+03
 0.31955401748515E+03 0.34110091479122E+03 0.29915000000387E+03 0.29915000000843E+03 0.37985167598294E+03
 0.30783702499181E+03 0.17886546473433E+04 0.17097895692821E+04 0.63926899779275E+03 0.11817213919358E+04
 0.53925604915410E+03 0.10445500525777E+04 0.10184738751013E+04 0.98594074933876E+03 0.17173927623585E+04
 0.92657382634887E+03 0.10176742546456E+04 0.88328954562356E+03 0.17169083401952E+04 0.10445500525777E+04
 0.10184738751013E+04 0.98594074933876E+03 0.17173927623585E+04 0.92657382634887E+03 0.10176742546456E+04
 0.88328954562356E+03 0.17169083401952E+04 0.17057329172654E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47324909940714E+03 0.12948750385795E+01
 0.12948750385795E+01 0.12600882865472E+01 0.36042614144236E-01 0.30624616102422E+03 0.33371743680314E+03
 0.33305734574118E+03 0.33299968049887E+03 0.23000000000000E+00 0.00000000000000E+00 0.19802673451649E+00
 0.00000000000000E+00 -.16308054645086E+02 0.48905647194427E-02 0.60194106682823E+00 0.16358029100802E+04
 0.61342609128008E+03 0.13290337610879E+02 0.49838766040797E+01 0.30783439180628E+03 0.37987383533414E+03
 0.30090690222880E+03 0.30599394207148E+03 0.29915000000023E+03 0.29915000000119E+03 0.30089654185421E+03
 0.30599413865915E+03 0.29915000000023E+03 0.29915000000119E+03 0.30090690222880E+03 0.30599394207148E+03
 0.29915000000023E+03 0.29915000000119E+03 0.30089654185421E+03 0.30599413865915E+03 0.29915000000023E+03
 0.29915000000119E+03 0.30493773198274E+03 0.29915000001447E+03 0.10686542894925E+03 0.10357842049816E+03
 0.12990574902212E+03 0.37760558601799E+03 0.24705030825077E+03 0.10239172032821E+03 0.12233433115486E+03
 0.10239172032821E+03 0.30198655417306E+03 0.10210416644137E+03 0.12227999270215E+03 0.10210416644137E+03
 0.30193067897427E+03 0.10239172032821E+03 0.12233433115486E+03 0.10239172032821E+03 0.30198655417306E+03
 0.10210416644137E+03 0.12227999270215E+03 0.10210416644137E+03 0.30193067897427E+03 0.20463397085121E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35618457861524E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19905697039026E+00 0.00000000000000E+00 0.00000000000000E+00 0.19905697039026E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20790135468515E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20790135468515E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936340006055E+00 0.21027684108724E+00 0.33371743680314E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    370.00133388
 0.12021388330170E+00 0.30932769694797E+03 0.43478325014427E+03 0.42975608373377E+03 0.42783019165610E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17274559393766E+00 0.00000000000000E+00 -.23178518325443E+02
 0.29278688716122E-02 0.83418210172761E+00 0.27323628040743E+04 0.10246360515278E+04 0.95902321368821E+01
 0.35963370513308E+01 0.32706521405920E+03 0.29915000014895E+03 0.32285227223265E+03 0.34187610180588E+03
 0.29915000000721E+03 0.29915000001246E+03 0.31999076108381E+03 0.34184269034400E+03 0.29915000000563E+03
 0.29915000001240E+03 0.32285227223265E+03 0.34187610180588E+03 0.29915000000721E+03 0.29915000001246E+03
 0.31999076108381E+03 0.34184269034400E+03 0.29915000000563E+03 0.29915000001240E+03 0.38081053689020E+03
 0.30841058674944E+03 0.17947222422142E+04 0.17140036198825E+04 0.63804858487951E+03 0.11732707686536E+04
 0.53203194084968E+03 0.10486211927935E+04 0.10220262427572E+04 0.98874047523174E+03 0.17171632303073E+04
 0.93092449177255E+03 0.10212554226955E+04 0.88662148975071E+03 0.17167030082788E+04 0.10486211927935E+04
 0.10220262427572E+04 0.98874047523174E+03 0.17171632303073E+04 0.93092449177255E+03 0.10212554226955E+04
 0.88662148975071E+03 0.17167030082788E+04 0.17046423689535E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47360101800244E+03 0.12948752682274E+01
 0.12948752682274E+01 0.13000278617043E+01 0.28871997963725E-01 0.30705047813101E+03 0.33375804378614E+03
 0.33324397659867E+03 0.33320019646116E+03 0.23000000000000E+00 0.00000000000000E+00 0.19684825782641E+00
 0.00000000000000E+00 -.16314921845077E+02 0.61051792455351E-02 0.62454071014514E+00 0.13103628375613E+04
 0.49138606408550E+03 0.12809413173628E+02 0.48035299401104E+01 0.30840796677603E+03 0.38083144776969E+03
 0.30099051412539E+03 0.30614765717640E+03 0.29915000000025E+03 0.29915000000167E+03 0.30098040990565E+03
 0.30614773707765E+03 0.29915000000025E+03 0.29915000000167E+03 0.30099051412539E+03 0.30614765717640E+03
 0.29915000000025E+03 0.29915000000167E+03 0.30098040990565E+03 0.30614773707765E+03 0.29915000000025E+03
 0.29915000000167E+03 0.30506998010378E+03 0.29915000002069E+03 0.10696979040688E+03 0.10430807880844E+03
 0.13263386083047E+03 0.37952366758149E+03 0.24622663744687E+03 0.10546172504656E+03 0.12482389140995E+03
 0.10546172504656E+03 0.30345686416663E+03 0.10521851767350E+03 0.12476189230060E+03 0.10521851767350E+03
 0.30339424148042E+03 0.10546172504656E+03 0.12482389140995E+03 0.10546172504656E+03 0.30345686416663E+03
 0.10521851767350E+03 0.12476189230060E+03 0.10521851767350E+03 0.30339424148042E+03 0.20600544350327E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35639377363613E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19915035902455E+00 0.00000000000000E+00 0.00000000000000E+00 0.19915035902455E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20803784015710E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20803784015710E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936332471048E+00 0.21025116383775E+00 0.33375804378614E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    380.00680790
 0.12033146890868E+00 0.30956348916985E+03 0.43515242826694E+03 0.43011499442686E+03 0.42818470691771E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17181637146694E+00 0.00000000000000E+00 -.23209857628019E+02
 0.29250075267748E-02 0.84701357967854E+00 0.27350356970948E+04 0.10256383864105E+04 0.94449489263633E+01
 0.35418558473863E+01 0.32761216073101E+03 0.29915000020795E+03 0.32331745226273E+03 0.34260667202609E+03
 0.29915000001040E+03 0.29915000001810E+03 0.32042254371808E+03 0.34257368424651E+03 0.29915000000811E+03
 0.29915000001801E+03 0.32331745226273E+03 0.34260667202609E+03 0.29915000001040E+03 0.29915000001810E+03
 0.32042254371808E+03 0.34257368424651E+03 0.29915000000811E+03 0.29915000001801E+03 0.38174775931858E+03
 0.30899732624154E+03 0.18006057435436E+04 0.17180518466891E+04 0.63682504120644E+03 0.11651405252981E+04
 0.52513135888561E+03 0.10525833494316E+04 0.10254553605934E+04 0.99144246878858E+03 0.17169510850844E+04
 0.93516357482809E+03 0.10247114283041E+04 0.88984905210046E+03 0.17165133192211E+04 0.10525833494316E+04
 0.10254553605934E+04 0.99144246878858E+03 0.17169510850844E+04 0.93516357482809E+03 0.10247114283041E+04
 0.88984905210047E+03 0.17165133192211E+04 0.17035976763752E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47395696511059E+03 0.12948754991473E+01
 0.12948754991473E+01 0.13400497577933E+01 0.22925794696762E-01 0.30786209381903E+03 0.33381683366083E+03
 0.33342014496948E+03 0.33338725510408E+03 0.23000000000000E+00 0.00000000000000E+00 0.19565949152682E+00
 0.00000000000000E+00 -.16323416457044E+02 0.76886630297013E-02 0.64727365084331E+00 0.10404929919670E+04
 0.39018487198763E+03 0.12359532926417E+02 0.46348248474064E+01 0.30899473133358E+03 0.38176745401545E+03
 0.30107555792310E+03 0.30630039618317E+03 0.29915000000028E+03 0.29915000000236E+03 0.30106575059231E+03
 0.30630035818821E+03 0.29915000000028E+03 0.29915000000236E+03 0.30107555792310E+03 0.30630039618317E+03
 0.29915000000028E+03 0.29915000000236E+03 0.30106575059231E+03 0.30630035818821E+03 0.29915000000028E+03
 0.29915000000236E+03 0.30520151822067E+03 0.29915000002922E+03 0.10688031892784E+03 0.10479315242132E+03
 0.13530300855500E+03 0.38159958396771E+03 0.24562006036993E+03 0.10851672831464E+03 0.12726255192818E+03
 0.10851672831464E+03 0.30502199804553E+03 0.10831417317656E+03 0.12719332700906E+03 0.10831417317656E+03
 0.30495306920698E+03 0.10851672831464E+03 0.12726255192818E+03 0.10851672831464E+03 0.30502199804553E+03
 0.10831417317656E+03 0.12719332700906E+03 0.10831417317656E+03 0.30495306920698E+03 0.20733133171314E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35670191806893E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19924365147959E+00 0.00000000000000E+00 0.00000000000000E+00 0.19924365147959E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20816175019073E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20816175019073E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936310260421E+00 0.21021388380131E+00 0.33381683366083E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    390.01055402
 0.12044130108794E+00 0.30979629067613E+03 0.43552401584992E+03 0.43047641224900E+03 0.42854165846377E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17092077826666E+00 0.00000000000000E+00 -.23239888583927E+02
 0.29223398809749E-02 0.85932900516203E+00 0.27375323630498E+04 0.10265746361437E+04 0.93095891700893E+01
 0.34910959387835E+01 0.32815114414277E+03 0.29915000028680E+03 0.32377626563808E+03 0.34332554322390E+03
 0.29915000001483E+03 0.29915000002597E+03 0.32084872203621E+03 0.34329296347946E+03 0.29915000001156E+03
 0.29915000002585E+03 0.32377626563808E+03 0.34332554322390E+03 0.29915000001483E+03 0.29915000002597E+03
 0.32084872203621E+03 0.34329296347946E+03 0.29915000001156E+03 0.29915000002585E+03 0.38266276589646E+03
 0.30959442939318E+03 0.18063052327348E+04 0.17219374001483E+04 0.63560035128897E+03 0.11573197016335E+04
 0.51854134858812E+03 0.10564356579308E+04 0.10287616471395E+04 0.99404812681517E+03 0.17167518422869E+04
 0.93928967962415E+03 0.10280427834147E+04 0.89297269868682E+03 0.17163348836683E+04 0.10564356579308E+04
 0.10287616471395E+04 0.99404812681517E+03 0.17167518422869E+04 0.93928967962415E+03 0.10280427834147E+04
 0.89297269868682E+03 0.17163348836683E+04 0.17025941896101E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47431540159021E+03 0.12948757204269E+01
 0.12948757204269E+01 0.13800647422692E+01 0.18200250892587E-01 0.30866124410347E+03 0.33389140024264E+03
 0.33358527012811E+03 0.33356057498041E+03 0.23000000000000E+00 0.00000000000000E+00 0.19446314840644E+00
 0.00000000000000E+00 -.16331954243899E+02 0.96849598513551E-02 0.67009009364293E+00 0.82602304219988E+03
 0.30975864082495E+03 0.11938693133797E+02 0.44770099251738E+01 0.30959187061684E+03 0.38268128395201E+03
 0.30116170005139E+03 0.30645228699585E+03 0.29915000000033E+03 0.29915000000331E+03 0.30115220735732E+03
 0.30645213102003E+03 0.29915000000033E+03 0.29915000000331E+03 0.30116170005139E+03 0.30645228699585E+03
 0.29915000000033E+03 0.29915000000331E+03 0.30115220735732E+03 0.30645213102003E+03 0.29915000000033E+03
 0.29915000000331E+03 0.30533226635933E+03 0.29915000004078E+03 0.10660055701867E+03 0.10499706157665E+03
 0.13790437205649E+03 0.38374514594399E+03 0.24515125202721E+03 0.11154184954900E+03 0.12964056876654E+03
 0.11154184954900E+03 0.30668424969329E+03 0.11137460824671E+03 0.12956460083561E+03 0.11137460824671E+03
 0.30660949571935E+03 0.11154184954900E+03 0.12964056876654E+03 0.11154184954900E+03 0.30668424969329E+03
 0.11137460824671E+03 0.12956460083561E+03 0.11137460824671E+03 0.30660949571935E+03 0.20858397144418E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35698417433721E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19932651854721E+00 0.00000000000000E+00 0.00000000000000E+00 0.19932651854721E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20826770204945E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20826770204945E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936279583122E+00 0.21016659649093E+00 0.33389140024264E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    400.00292761
 0.12054035175498E+00 0.31002632396967E+03 0.43589707117620E+03 0.43083956979489E+03 0.42890036153194E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17005842161773E+00 0.00000000000000E+00 -.23267019069896E+02
 0.29199382400817E-02 0.87113710182755E+00 0.27397839756282E+04 0.10274189908606E+04 0.91833994708949E+01
 0.34437748015856E+01 0.32868209223300E+03 0.29915000039088E+03 0.32422862397645E+03 0.34403269168052E+03
 0.29915000002090E+03 0.29915000003681E+03 0.32126920507729E+03 0.34400050464910E+03 0.29915000001630E+03
 0.29915000003663E+03 0.32422862397645E+03 0.34403269168052E+03 0.29915000002090E+03 0.29915000003681E+03
 0.32126920507729E+03 0.34400050464910E+03 0.29915000001630E+03 0.29915000003663E+03 0.38355614444933E+03
 0.31019967255914E+03 0.18118310857183E+04 0.17256730300816E+04 0.63437576710611E+03 0.11497899392103E+04
 0.51224229326865E+03 0.10601833613650E+04 0.10319506453124E+04 0.99656442565804E+03 0.17165626071487E+04
 0.94330798184607E+03 0.10312551566438E+04 0.89599926603187E+03 0.17161649278782E+04 0.10601833613650E+04
 0.10319506453124E+04 0.99656442565804E+03 0.17165626071487E+04 0.94330798184607E+03 0.10312551566438E+04
 0.89599926603187E+03 0.17161649278782E+04 0.17016295292171E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47467560195827E+03 0.12948759203348E+01
 0.12948759203348E+01 0.14200342366237E+01 0.14448531033335E-01 0.30943765649151E+03 0.33397609789274E+03
 0.33373973493801E+03 0.33372118604237E+03 0.23000000000000E+00 0.00000000000000E+00 0.19326639105450E+00
 0.00000000000000E+00 -.16338397998572E+02 0.12199765446852E-01 0.69286504028957E+00 0.65575031215576E+03
 0.24590636705841E+03 0.11546260144193E+02 0.43298475540723E+01 0.31019716183557E+03 0.38357352949403E+03
 0.30124871010109E+03 0.30660320674327E+03 0.29915000000039E+03 0.29915000000463E+03 0.30123953776039E+03
 0.30660293342398E+03 0.29915000000039E+03 0.29915000000463E+03 0.30124871010109E+03 0.30660320674327E+03
 0.29915000000039E+03 0.29915000000463E+03 0.30123953776039E+03 0.30660293342398E+03 0.29915000000039E+03
 0.29915000000463E+03 0.30546206286967E+03 0.29915000005621E+03 0.10612657997133E+03 0.10490565462831E+03
 0.14041341928641E+03 0.38588680486396E+03 0.24477131848112E+03 0.11452306999769E+03 0.13193273809648E+03
 0.11452306999769E+03 0.30836955948107E+03 0.11438635576245E+03 0.13185049515772E+03 0.11438635576245E+03
 0.30828944156071E+03 0.11452306999769E+03 0.13193273809648E+03 0.11452306999769E+03 0.30836955948107E+03
 0.11438635576245E+03 0.13185049515772E+03 0.11438635576245E+03 0.30828944156071E+03 0.20973121914953E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35724324048690E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19938748589474E+00 0.00000000000000E+00 0.00000000000000E+00 0.19938748589474E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20834881995504E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20834881995504E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936249965440E+00 0.21011297189982E+00 0.33397609789274E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    410.53948672
 0.12063677729714E+00 0.31026317674733E+03 0.43629070492591E+03 0.43122285330918E+03 0.42927884641711E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16918270521301E+00 0.00000000000000E+00 -.23299788223323E+02
 0.29176040148180E-02 0.88307750412732E+00 0.27419759362029E+04 0.10282409760761E+04 0.90592274886515E+01
 0.33972103082443E+01 0.32923323157153E+03 0.29915000053835E+03 0.32469847719149E+03 0.34476706021326E+03
 0.29915000002985E+03 0.29915000005286E+03 0.32170605096342E+03 0.34473526604492E+03 0.29915000002329E+03
 0.29915000005261E+03 0.32469847719149E+03 0.34476706021326E+03 0.29915000002985E+03 0.29915000005286E+03
 0.32170605096342E+03 0.34473526604492E+03 0.29915000002329E+03 0.29915000005261E+03 0.38448262106843E+03
 0.31084670618476E+03 0.18174814527594E+04 0.17294449208522E+04 0.63300328393900E+03 0.11419743251922E+04
 0.50580602483350E+03 0.10640292504040E+04 0.10351832266825E+04 0.99911414707226E+03 0.17163460843742E+04
 0.94743696555564E+03 0.10345107346884E+04 0.89908190801271E+03 0.17159673020710E+04 0.10640292504040E+04
 0.10351832266825E+04 0.99911414707226E+03 0.17163460843742E+04 0.94743696555564E+03 0.10345107346884E+04
 0.89908190801271E+03 0.17159673020710E+04 0.17005907022747E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47505574264853E+03 0.12948761617906E+01
 0.12948761617906E+01 0.14621804730671E+01 0.11323233942937E-01 0.31023595538873E+03 0.33407252002883E+03
 0.33389258203029E+03 0.33387886905314E+03 0.23000000000000E+00 0.00000000000000E+00 0.19200805713671E+00
 0.00000000000000E+00 -.16350525306207E+02 0.15566990996011E-01 0.71676721611838E+00 0.51390792234993E+03
 0.19271547088122E+03 0.11161224760423E+02 0.41854592851586E+01 0.31084425757988E+03 0.38449888604944E+03
 0.30134043414488E+03 0.30676086799627E+03 0.29915000000046E+03 0.29915000000657E+03 0.30133158955353E+03
 0.30676047354693E+03 0.29915000000046E+03 0.29915000000657E+03 0.30134043414488E+03 0.30676086799627E+03
 0.29915000000046E+03 0.29915000000657E+03 0.30133158955353E+03 0.30676047354693E+03 0.29915000000046E+03
 0.29915000000657E+03 0.30559744472184E+03 0.29915000007836E+03 0.10541209778924E+03 0.10450553791635E+03
 0.14295043708993E+03 0.38810511074797E+03 0.24443992147259E+03 0.11761797350857E+03 0.13424743098364E+03
 0.11761797350857E+03 0.31013191922375E+03 0.11750891171924E+03 0.13415903926264E+03 0.11750891171924E+03
 0.31004659142613E+03 0.11761797350857E+03 0.13424743098364E+03 0.11761797350857E+03 0.31013191922375E+03
 0.11750891171924E+03 0.13415903926264E+03 0.11750891171924E+03 0.31004659142613E+03 0.21081989452228E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35749607759842E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19948261043543E+00 0.00000000000000E+00 0.00000000000000E+00 0.19948261043543E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20842823725649E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20842823725649E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936196303225E+00 0.21005173720806E+00 0.33407252002883E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    420.38375357
 0.12070541315923E+00 0.31048420525321E+03 0.43665964222505E+03 0.43158295614164E+03 0.42963474151736E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16839446522810E+00 0.00000000000000E+00 -.23314196575236E+02
 0.29159447114139E-02 0.89377691524689E+00 0.27435362435664E+04 0.10288260913374E+04 0.89507793986715E+01
 0.33565422745018E+01 0.32974170760039E+03 0.29915000071897E+03 0.32513235169449E+03 0.34544294636033E+03
 0.29915000004125E+03 0.29915000007337E+03 0.32210985247133E+03 0.34541150867565E+03 0.29915000003221E+03
 0.29915000007302E+03 0.32513235169449E+03 0.34544294636033E+03 0.29915000004125E+03 0.29915000007337E+03
 0.32210985247133E+03 0.34541150867565E+03 0.29915000003221E+03 0.29915000007302E+03 0.38532603896962E+03
 0.31145775996144E+03 0.18226228116289E+04 0.17328619979917E+04 0.63176905029025E+03 0.11350114420368E+04
 0.50008354649515E+03 0.10675381489283E+04 0.10381098053560E+04 0.10014334217758E+04 0.17161598561471E+04
 0.95120730545243E+03 0.10374573965234E+04 0.90189120301465E+03 0.17157974445855E+04 0.10675381489283E+04
 0.10381098053560E+04 0.10014334217758E+04 0.17161598561471E+04 0.95120730545243E+03 0.10374573965234E+04
 0.90189120301465E+03 0.17157974445855E+04 0.16996865674406E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47541306176304E+03 0.12948762679570E+01
 0.12948762679570E+01 0.15015575404740E+01 0.90137316155063E-02 0.31095921123779E+03 0.33416726904515E+03
 0.33402780824222E+03 0.33401746694105E+03 0.23000000000000E+00 0.00000000000000E+00 0.19083990740967E+00
 0.00000000000000E+00 -.16344603692098E+02 0.19555571543342E-01 0.73891984570727E+00 0.40909057463595E+03
 0.15340896548848E+03 0.10826614072522E+02 0.40599802771957E+01 0.31145537693112E+03 0.38534128573448E+03
 0.30142735059284E+03 0.30690738991930E+03 0.29915000000056E+03 0.29915000000905E+03 0.30141881060277E+03
 0.30690688261709E+03 0.29915000000056E+03 0.29915000000905E+03 0.30142735059284E+03 0.30690738991930E+03
 0.29915000000056E+03 0.29915000000905E+03 0.30141881060277E+03 0.30690688261709E+03 0.29915000000056E+03
 0.29915000000905E+03 0.30572320489218E+03 0.29915000010580E+03 0.10455760620247E+03 0.10386779945958E+03
 0.14521533542529E+03 0.39010924288352E+03 0.24416783078110E+03 0.12046363794209E+03 0.13630959507504E+03
 0.12046363794209E+03 0.31173134016801E+03 0.12037665726567E+03 0.13621583308062E+03 0.12037665726567E+03
 0.31164151563182E+03 0.12046363794209E+03 0.13630959507504E+03 0.12046363794209E+03 0.31173134016801E+03
 0.12037665726567E+03 0.13621583308062E+03 0.12037665726567E+03 0.31164151563182E+03 0.21171830817219E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35771441341535E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19945121056322E+00 0.00000000000000E+00 0.00000000000000E+00 0.19945121056322E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20843476603588E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20843476603588E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936200698476E+00 0.20999223819785E+00 0.33416726904515E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    434.39549455
 0.12077465588021E+00 0.31079238127143E+03 0.43718484901210E+03 0.43209651341302E+03 0.43014249690956E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16732017651793E+00 0.00000000000000E+00 -.23350441512544E+02
 0.29142725281171E-02 0.90828352968322E+00 0.27451104599228E+04 0.10294164224711E+04 0.88078223798577E+01
 0.33029333924466E+01 0.33045333556419E+03 0.29915000107655E+03 0.32573996602990E+03 0.34638913087943E+03
 0.29915000006495E+03 0.29915000011618E+03 0.32267551853013E+03 0.34635817183014E+03 0.29915000005080E+03
 0.29915000011563E+03 0.32573996602990E+03 0.34638913087943E+03 0.29915000006495E+03 0.29915000011618E+03
 0.32267551853013E+03 0.34635817183014E+03 0.29915000005080E+03 0.29915000011563E+03 0.38650575697421E+03
 0.31233832459775E+03 0.18297306007532E+04 0.17375380371978E+04 0.62991051099832E+03 0.11252700372780E+04
 0.49220997372468E+03 0.10724031639901E+04 0.10421164530316E+04 0.10046152862522E+04 0.17158909836390E+04
 0.95644155821231E+03 0.10414904892262E+04 0.90576337007765E+03 0.17155500287110E+04 0.10724031639901E+04
 0.10421164530316E+04 0.10046152862522E+04 0.17158909836390E+04 0.95644155821231E+03 0.10414904892262E+04
 0.90576337007765E+03 0.17155500287110E+04 0.16983940731617E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47592278924777E+03 0.12948765350240E+01
 0.12948765350240E+01 0.15576045043979E+01 0.65135156876783E-02 0.31196524808763E+03 0.33430789361896E+03
 0.33421087417084E+03 0.33420395805637E+03 0.23000000000000E+00 0.00000000000000E+00 0.18919610263229E+00
 0.00000000000000E+00 -.16353857135811E+02 0.27061985041930E-01 0.77003687819788E+00 0.29561763439026E+03
 0.11085661289635E+03 0.10389112815898E+02 0.38959173059619E+01 0.31233604393168E+03 0.38651964669791E+03
 0.30155076253726E+03 0.30711331998655E+03 0.29915000000079E+03 0.29915000001427E+03 0.30154262964912E+03
 0.30711265650393E+03 0.29915000000078E+03 0.29915000001427E+03 0.30155076253726E+03 0.30711331998655E+03
 0.29915000000079E+03 0.29915000001427E+03 0.30154262964912E+03 0.30711265650393E+03 0.29915000000078E+03
 0.29915000001427E+03 0.30589975967040E+03 0.29915000016093E+03 0.10303579385042E+03 0.10256900332593E+03
 0.14826728749573E+03 0.39285932996807E+03 0.24385070603487E+03 0.12443899136666E+03 0.13908206168895E+03
 0.12443899136666E+03 0.31393288215888E+03 0.12437802836980E+03 0.13898126034853E+03 0.12437802836980E+03
 0.31383722569356E+03 0.12443899136666E+03 0.13908206168895E+03 0.12443899136666E+03 0.31393288215888E+03
 0.12437802836980E+03 0.13898126034853E+03 0.12437802836980E+03 0.31383722569356E+03 0.21282434965442E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35800707166287E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19952251854638E+00 0.00000000000000E+00 0.00000000000000E+00 0.19952251854638E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20847076970903E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20847076970903E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936143901185E+00 0.20990328906003E+00 0.33430789361896E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    440.51322964
 0.12077558932338E+00 0.31093305038534E+03 0.43741665254363E+03 0.43232460868016E+03 0.43036865280595E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16686798705287E+00 0.00000000000000E+00 -.23340499798783E+02
 0.29142498209936E-02 0.91435705031308E+00 0.27451318491537E+04 0.10294244434326E+04 0.87493173451889E+01
 0.32809940044458E+01 0.33076238272336E+03 0.29915000126807E+03 0.32600425302859E+03 0.34679651480364E+03
 0.29915000007801E+03 0.29915000013983E+03 0.32292224185086E+03 0.34676576871781E+03 0.29915000006107E+03
 0.29915000013919E+03 0.32600425302859E+03 0.34679651480364E+03 0.29915000007801E+03 0.29915000013983E+03
 0.32292224185086E+03 0.34676576871781E+03 0.29915000006107E+03 0.29915000013919E+03 0.38699782003206E+03
 0.31272324899379E+03 0.18327860198221E+04 0.17395772574396E+04 0.62928955224155E+03 0.11215139668301E+04
 0.48907796682739E+03 0.10744957220319E+04 0.10438477010386E+04 0.10060091268117E+04 0.17158317863962E+04
 0.95869205403251E+03 0.10432325628984E+04 0.90744960509031E+03 0.17154994544173E+04 0.10744957220319E+04
 0.10438477010386E+04 0.10060091268117E+04 0.17158317863962E+04 0.95869205403251E+03 0.10432325628984E+04
 0.90744960509031E+03 0.17154994544173E+04 0.16979862802439E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47614950435354E+03 0.12948764617695E+01
 0.12948764617695E+01 0.15820754447635E+01 0.56518185974013E-02 0.31239044298637E+03 0.33437131605503E+03
 0.33428849478356E+03 0.33428269068862E+03 0.23000000000000E+00 0.00000000000000E+00 0.18848711204147E+00
 0.00000000000000E+00 -.16330134795349E+02 0.31187954333855E-01 0.78343804147019E+00 0.25650928927120E+03
 0.96190983476701E+02 0.10211400999864E+02 0.38292753749489E+01 0.31272102085523E+03 0.38701110247267E+03
 0.30160716965523E+03 0.30720370196021E+03 0.29915000000092E+03 0.29915000001716E+03 0.30159922080913E+03
 0.30720296813780E+03 0.29915000000091E+03 0.29915000001717E+03 0.30160716965523E+03 0.30720370196021E+03
 0.29915000000092E+03 0.29915000001716E+03 0.30159922080913E+03 0.30720296813780E+03 0.29915000000091E+03
 0.29915000001717E+03 0.30597732513094E+03 0.29915000019071E+03 0.10228806149446E+03 0.10188789852535E+03
 0.14953574309934E+03 0.39400169652563E+03 0.24371827471079E+03 0.12614423548218E+03 0.14023050142679E+03
 0.12614423548218E+03 0.31484485353282E+03 0.12609283294915E+03 0.14012682284248E+03 0.12609283294915E+03
 0.31474686304197E+03 0.12614423548218E+03 0.14023050142679E+03 0.12614423548218E+03 0.31484485353282E+03
 0.12609283294915E+03 0.14012682284248E+03 0.12609283294915E+03 0.31474686304197E+03 0.21324192074523E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35812666555448E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19936189720344E+00 0.00000000000000E+00 0.00000000000000E+00 0.19936189720344E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20841144861468E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20841144861468E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936210761496E+00 0.20986422828676E+00 0.33437131605503E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    451.11964294
 0.12077017406684E+00 0.31116925711819E+03 0.43781778626655E+03 0.43271933129637E+03 0.43075985975965E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16610698189403E+00 0.00000000000000E+00 -.23358060375722E+02
 0.29143801772746E-02 0.92454032484792E+00 0.27450090631213E+04 0.10293783986705E+04 0.86529486978472E+01
 0.32448557616927E+01 0.33129153651637E+03 0.29915000167496E+03 0.32645677970396E+03 0.34749645889063E+03
 0.29915000010660E+03 0.29915000019171E+03 0.32334438639122E+03 0.34746605996782E+03 0.29915000008358E+03
 0.29915000019084E+03 0.32645677970396E+03 0.34749645889063E+03 0.29915000010660E+03 0.29915000019171E+03
 0.32334438639122E+03 0.34746605996782E+03 0.29915000008358E+03 0.29915000019084E+03 0.38785465962042E+03
 0.31339618521834E+03 0.18379918992877E+04 0.17430081946589E+04 0.62800319137593E+03 0.11147521285874E+04
 0.48360892125463E+03 0.10780677681722E+04 0.10467655514952E+04 0.10083536574437E+04 0.17157016642518E+04
 0.96253856654813E+03 0.10461681917396E+04 0.91030275170455E+03 0.17153835076521E+04 0.10780677681722E+04
 0.10467655514952E+04 0.10083536574437E+04 0.17157016642518E+04 0.96253856654813E+03 0.10461681917396E+04
 0.91030275170455E+03 0.17153835076521E+04 0.16971775540746E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47654178636242E+03 0.12948765911628E+01
 0.12948765911628E+01 0.16245010979605E+01 0.44177624300321E-02 0.31312831332137E+03 0.33448391719405E+03
 0.33442102120441E+03 0.33441674513952E+03 0.23000000000000E+00 0.00000000000000E+00 0.18727239131804E+00
 0.00000000000000E+00 -.16325718221586E+02 0.39899984855087E-01 0.80636984983289E+00 0.20050132923747E+03
 0.75187998464052E+02 0.99210058531553E+01 0.37203771949332E+01 0.31339405479468E+03 0.38786694504430E+03
 0.30170303024138E+03 0.30735819195110E+03 0.29915000000121E+03 0.29915000002353E+03 0.30169537290856E+03
 0.30735734131043E+03 0.29915000000120E+03 0.29915000002353E+03 0.30170303024138E+03 0.30735819195110E+03
 0.29915000000121E+03 0.29915000002353E+03 0.30169537290856E+03 0.30735734131043E+03 0.29915000000120E+03
 0.29915000002353E+03 0.30610975469468E+03 0.29915000025450E+03 0.10084327606493E+03 0.10054465729418E+03
 0.15165287849965E+03 0.39594646369951E+03 0.24353532080736E+03 0.12906046261833E+03 0.14214556485842E+03
 0.12906046261833E+03 0.31639798506990E+03 0.12902350775055E+03 0.14203723687639E+03 0.12902350775055E+03
 0.31629624681047E+03 0.12906046261833E+03 0.14214556485842E+03 0.12906046261833E+03 0.31639798506990E+03
 0.12902350775055E+03 0.14203723687639E+03 0.12902350775055E+03 0.31629624681047E+03 0.21389955633045E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35833066205191E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19934073884510E+00 0.00000000000000E+00 0.00000000000000E+00 0.19934073884510E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20836063412818E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20836063412818E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936202488460E+00 0.20979350138607E+00 0.33448391719405E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    460.53519908
 0.12076106016843E+00 0.31137352901030E+03 0.43817298493695E+03 0.43306883936146E+03 0.43110613984181E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16545510822073E+00 0.00000000000000E+00 -.23373494536735E+02
 0.29145998471689E-02 0.93322380765484E+00 0.27448021750810E+04 0.10293008156554E+04 0.85724345375454E+01
 0.32146629515795E+01 0.33175564200691E+03 0.29915000213394E+03 0.32685389358002E+03 0.34811005140177E+03
 0.29915000013998E+03 0.29915000025246E+03 0.32371502929367E+03 0.34807994847340E+03 0.29915000010991E+03
 0.29915000025133E+03 0.32685389358002E+03 0.34811005140177E+03 0.29915000013998E+03 0.29915000025246E+03
 0.32371502929367E+03 0.34807994847340E+03 0.29915000010991E+03 0.29915000025133E+03 0.38860341801202E+03
 0.31399738913571E+03 0.18425057187294E+04 0.17459411296226E+04 0.62683238791449E+03 0.11088609348725E+04
 0.47889438501846E+03 0.10811743143079E+04 0.10492774168744E+04 0.10103614720677E+04 0.17155759883489E+04
 0.96588664196073E+03 0.10486948824956E+04 0.91275812989685E+03 0.17152695848821E+04 0.10811743143079E+04
 0.10492774168744E+04 0.10103614720677E+04 0.17155759883489E+04 0.96588664196073E+03 0.10486948824956E+04
 0.91275812989685E+03 0.17152695848821E+04 0.16964520866122E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47688909410927E+03 0.12948767048878E+01
 0.12948767048878E+01 0.16621633224945E+01 0.35488843492164E-02 0.31377907229046E+03 0.33458697265649E+03
 0.33453774276852E+03 0.33453448637955E+03 0.23000000000000E+00 0.00000000000000E+00 0.18621086115784E+00
 0.00000000000000E+00 -.16322776249097E+02 0.49668749773744E-01 0.82637891021769E+00 0.16106707006805E+03
 0.60400151275518E+02 0.96807891647339E+01 0.36302959367752E+01 0.31399534032235E+03 0.38861492925337E+03
 0.30178829515421E+03 0.30749408412284E+03 0.29915000000157E+03 0.29915000003101E+03 0.30178088059581E+03
 0.30749313192290E+03 0.29915000000156E+03 0.29915000003102E+03 0.30178829515421E+03 0.30749408412284E+03
 0.29915000000157E+03 0.29915000003101E+03 0.30178088059581E+03 0.30749313192290E+03 0.29915000000156E+03
 0.29915000003102E+03 0.30622618935701E+03 0.29915000032718E+03 0.99426543652600E+02 0.99199970055787E+02
 0.15345434341496E+03 0.39763479774004E+03 0.24341318260801E+03 0.13162217447853E+03 0.14377104536904E+03
 0.13162217447853E+03 0.31774331281367E+03 0.13159601989619E+03 0.14365881443857E+03 0.13159601989619E+03
 0.31763845505102E+03 0.13162217447853E+03 0.14377104536904E+03 0.13162217447853E+03 0.31774331281367E+03
 0.13159601989619E+03 0.14365881443857E+03 0.13159601989619E+03 0.31763845505102E+03 0.21441867304928E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35850864947609E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19932143001932E+00 0.00000000000000E+00 0.00000000000000E+00 0.19932143001932E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20834155237668E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20834155237668E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936190482240E+00 0.20972876309755E+00 0.33458697265649E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    470.00960244
 0.12073308736276E+00 0.31157959886876E+03 0.43853059263638E+03 0.43342153082927E+03 0.43145588431778E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16482080687073E+00 0.00000000000000E+00 -.23373880421521E+02
 0.29152748515047E-02 0.94163415904841E+00 0.27441666420820E+04 0.10290624907808E+04 0.84958685102127E+01
 0.31859506913298E+01 0.33221816326212E+03 0.29915000270226E+03 0.32724991368629E+03 0.34872037063918E+03
 0.29915000018258E+03 0.29915000033016E+03 0.32408496807030E+03 0.34869055741922E+03 0.29915000014358E+03
 0.29915000032870E+03 0.32724991368629E+03 0.34872037063918E+03 0.29915000018258E+03 0.29915000033016E+03
 0.32408496807030E+03 0.34869055741922E+03 0.29915000014358E+03 0.29915000032870E+03 0.38934296751703E+03
 0.31460454704195E+03 0.18469719372703E+04 0.17488430136347E+04 0.62568452027015E+03 0.11031381691238E+04
 0.47432522625230E+03 0.10842526992501E+04 0.10517526212920E+04 0.10123528913878E+04 0.17154676633419E+04
 0.96920586445028E+03 0.10511841636794E+04 0.91519405372419E+03 0.17151723331201E+04 0.10842526992501E+04
 0.10517526212920E+04 0.10123528913878E+04 0.17154676633419E+04 0.96920586445028E+03 0.10511841636794E+04
 0.91519405372419E+03 0.17151723331201E+04 0.16957717887518E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47723972844634E+03 0.12948767077312E+01
 0.12948767077312E+01 0.17000609359319E+01 0.28465821710322E-02 0.31442670939783E+03 0.33469348667165E+03
 0.33465502597375E+03 0.33465255194722E+03 0.23000000000000E+00 0.00000000000000E+00 0.18515995134769E+00
 0.00000000000000E+00 -.16303288016079E+02 0.61922907159517E-01 0.84615883434897E+00 0.12919290076920E+03
 0.48447337788451E+02 0.94544897190078E+01 0.35454336446279E+01 0.31460258758446E+03 0.38935371104966E+03
 0.30187483608874E+03 0.30762999332646E+03 0.29915000000206E+03 0.29915000004062E+03 0.30186765391582E+03
 0.30762894021647E+03 0.29915000000204E+03 0.29915000004063E+03 0.30187483608874E+03 0.30762999332646E+03
 0.29915000000206E+03 0.29915000004062E+03 0.30186765391582E+03 0.30762894021647E+03 0.29915000000204E+03
 0.29915000004063E+03 0.30634264234283E+03 0.29915000041795E+03 0.97889171058858E+02 0.97717425433112E+02
 0.15519638035658E+03 0.39929114645257E+03 0.24331878419420E+03 0.13417390518256E+03 0.14533904201562E+03
 0.13417390518256E+03 0.31905784519561E+03 0.13415693571982E+03 0.14522308876416E+03 0.13415693571982E+03
 0.31895004305989E+03 0.13417390518256E+03 0.14533904201562E+03 0.13417390518256E+03 0.31905784519561E+03
 0.13415693571982E+03 0.14522308876416E+03 0.13415693571982E+03 0.31895004305989E+03 0.21488123777450E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35868263556531E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19918982273334E+00 0.00000000000000E+00 0.00000000000000E+00 0.19918982273334E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20826716544649E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20826716544649E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936232537838E+00 0.20966250091888E+00 0.33469348667165E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    480.57340249
 0.12067239227500E+00 0.31181185146070E+03 0.43893068814143E+03 0.43381744343294E+03 0.43184901475657E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16413803842047E+00 0.00000000000000E+00 -.23384514617070E+02
 0.29167408400909E-02 0.95063895231288E+00 0.27427873913374E+04 0.10285452717515E+04 0.84153925951973E+01
 0.31557722231990E+01 0.33272959785916E+03 0.29915000347327E+03 0.32768816779562E+03 0.34939316957358E+03
 0.29915000024205E+03 0.29915000043883E+03 0.32449484011911E+03 0.34936367239637E+03 0.29915000019065E+03
 0.29915000043693E+03 0.32768816779562E+03 0.34939316957358E+03 0.29915000024205E+03 0.29915000043883E+03
 0.32449484011911E+03 0.34936367239637E+03 0.29915000019065E+03 0.29915000043693E+03 0.39015055170553E+03
 0.31528246849978E+03 0.18518864372188E+04 0.17520507367047E+04 0.62447747124199E+03 0.10970555673666E+04
 0.46945570876836E+03 0.10876424384062E+04 0.10544720601979E+04 0.10145578144699E+04 0.17153924928559E+04
 0.97286144023768E+03 0.10539183658833E+04 0.91788714067639E+03 0.17151086561844E+04 0.10876424384062E+04
 0.10544720601979E+04 0.10145578144699E+04 0.17153924928559E+04 0.97286144023768E+03 0.10539183658833E+04
 0.91788714067640E+03 0.17151086561844E+04 0.16951081100259E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47763360302009E+03 0.12948767860881E+01
 0.12948767860881E+01 0.17423161361623E+01 0.22265323749462E-02 0.31514106569603E+03 0.33481571016109E+03
 0.33478650600584E+03 0.33478468562955E+03 0.23000000000000E+00 0.00000000000000E+00 0.18400987329616E+00
 0.00000000000000E+00 -.16291301958934E+02 0.79167336512721E-01 0.86777016059983E+00 0.10105177655831E+03
 0.37894416209366E+02 0.92190309868112E+01 0.34571366200542E+01 0.31528062121080E+03 0.39016044321700E+03
 0.30197265772118E+03 0.30778085246305E+03 0.29915000000276E+03 0.29915000005411E+03 0.30196572240131E+03
 0.30777968794477E+03 0.29915000000273E+03 0.29915000005413E+03 0.30197265772118E+03 0.30778085246305E+03
 0.29915000000276E+03 0.29915000005411E+03 0.30196572240131E+03 0.30777968794477E+03 0.29915000000273E+03
 0.29915000005413E+03 0.30647194262582E+03 0.29915000054205E+03 0.96061795078954E+02 0.95935829536978E+02
 0.15705541524925E+03 0.40109039532233E+03 0.24324970299684E+03 0.13696877199577E+03 0.14700937390028E+03
 0.13696877199577E+03 0.32048012331817E+03 0.13696030032096E+03 0.14688962173489E+03 0.13696030032096E+03
 0.32036938076698E+03 0.13696877199577E+03 0.14700937390028E+03 0.13696877199577E+03 0.32048012331817E+03
 0.13696030032096E+03 0.14688962173489E+03 0.13696030032096E+03 0.32036938076698E+03 0.21533534127932E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35887423670672E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19911740154917E+00 0.00000000000000E+00 0.00000000000000E+00 0.19911740154917E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20816551001830E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20816551001830E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936245588903E+00 0.20958612624866E+00 0.33481571016109E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    491.63556560
 0.12060176366375E+00 0.31204992106824E+03 0.43934853969174E+03 0.43423106038575E+03 0.43225967413578E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16344941882715E+00 0.00000000000000E+00 -.23394427073151E+02
 0.29184486525399E-02 0.95967308062206E+00 0.27411823720241E+04 0.10279433895090E+04 0.83361721418865E+01
 0.31260645532074E+01 0.33325889311349E+03 0.29915000448405E+03 0.32814193743157E+03 0.35008941668690E+03
 0.29915000032259E+03 0.29915000058633E+03 0.32491937993954E+03 0.35006023596513E+03 0.29915000025453E+03
 0.29915000058384E+03 0.32814193743157E+03 0.35008941668690E+03 0.29915000032259E+03 0.29915000058633E+03
 0.32491937993954E+03 0.35006023596513E+03 0.29915000025453E+03 0.29915000058384E+03 0.39098653998073E+03
 0.31599572029416E+03 0.18569316511685E+04 0.17553086043770E+04 0.62315301088711E+03 0.10907383610053E+04
 0.46446958506375E+03 0.10911308452467E+04 0.10572434474979E+04 0.10168003787878E+04 0.17153088745227E+04
 0.97662633511504E+03 0.10567042381502E+04 0.92063704548250E+03 0.17150362466151E+04 0.10911308452467E+04
 0.10572434474979E+04 0.10168003787878E+04 0.17153088745227E+04 0.97662633511504E+03 0.10567042381502E+04
 0.92063704548250E+03 0.17150362466152E+04 0.16943977273484E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47804508192141E+03 0.12948768591271E+01
 0.12948768591271E+01 0.17865647885815E+01 0.17209701371499E-02 0.31588774517242E+03 0.33494795749506E+03
 0.33492608945758E+03 0.33492477157674E+03 0.23000000000000E+00 0.00000000000000E+00 0.18283092424819E+00
 0.00000000000000E+00 -.16277806395501E+02 0.10242399216104E+00 0.88988319103357E+00 0.78106699721505E+02
 0.29290012395564E+02 0.89899439393931E+01 0.33712289772724E+01 0.31599398724813E+03 0.39099562722446E+03
 0.30207495236262E+03 0.30793726482322E+03 0.29915000000376E+03 0.29915000007250E+03 0.30206825521434E+03
 0.30793598671451E+03 0.29915000000372E+03 0.29915000007252E+03 0.30207495236262E+03 0.30793726482322E+03
 0.29915000000376E+03 0.29915000007250E+03 0.30206825521434E+03 0.30793598671451E+03 0.29915000000372E+03
 0.29915000007252E+03 0.30660598633922E+03 0.29915000070621E+03 0.94016023566257E+02 0.93928604361642E+02
 0.15892809278001E+03 0.40295394591724E+03 0.24323121267333E+03 0.13987351056447E+03 0.14868819696035E+03
 0.13987351056447E+03 0.32194717203969E+03 0.13987232565007E+03 0.14856468447340E+03 0.13987232565007E+03
 0.32183354424080E+03 0.13987351056447E+03 0.14868819696035E+03 0.13987351056447E+03 0.32194717203969E+03
 0.13987232565007E+03 0.14856468447340E+03 0.13987232565007E+03 0.32183354424080E+03 0.21575967515701E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35907420020488E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19903317188931E+00 0.00000000000000E+00 0.00000000000000E+00 0.19903317188931E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20805994188185E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20805994188185E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936261046839E+00 0.20950356582397E+00 0.33494795749506E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    501.65945671
 0.12053243217087E+00 0.31226307266721E+03 0.43972617252771E+03 0.43460502671497E+03 0.43263098090798E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16284783175378E+00 0.00000000000000E+00 -.23402408609279E+02
 0.29201270733297E-02 0.96752372102665E+00 0.27396068044662E+04 0.10273525516748E+04 0.82685311234655E+01
 0.31006991712996E+01 0.33373351476528E+03 0.29915000561589E+03 0.32854907215549E+03 0.35071295110158E+03
 0.29915000041559E+03 0.29915000075700E+03 0.32530056145018E+03 0.35068404717843E+03 0.29915000032844E+03
 0.29915000075384E+03 0.32854907215549E+03 0.35071295110158E+03 0.29915000041559E+03 0.29915000075700E+03
 0.32530056145018E+03 0.35068404717843E+03 0.29915000032844E+03 0.29915000075384E+03 0.39173211763053E+03
 0.31664368123536E+03 0.18614169160921E+04 0.17581838354219E+04 0.62194932942874E+03 0.10851484072755E+04
 0.46008933119962E+03 0.10942396899467E+04 0.10596944652557E+04 0.10187846173006E+04 0.17152360943938E+04
 0.97998334307579E+03 0.10591675871868E+04 0.92307575573534E+03 0.17149729330088E+04 0.10942396899467E+04
 0.10596944652557E+04 0.10187846173006E+04 0.17152360943938E+04 0.97998334307579E+03 0.10591675871868E+04
 0.92307575573534E+03 0.17149729330088E+04 0.16937688938760E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47841712989584E+03 0.12948769179382E+01
 0.12948769179382E+01 0.18266603530393E+01 0.13625131698913E-02 0.31656116882868E+03 0.33507177860272E+03
 0.33505496463632E+03 0.33505398240105E+03 0.23000000000000E+00 0.00000000000000E+00 0.18178574774144E+00
 0.00000000000000E+00 -.16264778675550E+02 0.12937021844988E+00 0.90944990196029E+00 0.61838034254376E+02
 0.23189262845391E+02 0.87965263207531E+01 0.32986973702824E+01 0.31664205371239E+03 0.39174051869572E+03
 0.30216812096857E+03 0.30807796588877E+03 0.29915000000497E+03 0.29915000009387E+03 0.30216162459111E+03
 0.30807658683019E+03 0.29915000000491E+03 0.29915000009389E+03 0.30216812096857E+03 0.30807796588877E+03
 0.29915000000497E+03 0.29915000009387E+03 0.30216162459111E+03 0.30807658683019E+03 0.29915000000491E+03
 0.29915000009389E+03 0.30672658184437E+03 0.29915000089156E+03 0.92062709777456E+02 0.92002027719124E+02
 0.16056543957090E+03 0.40462953184356E+03 0.24326126507480E+03 0.14248457227579E+03 0.15015309866511E+03
 0.14248457227579E+03 0.32326072169032E+03 0.14248878037464E+03 0.15002637541426E+03 0.14248878037464E+03
 0.32314466073272E+03 0.14248457227579E+03 0.15015309866511E+03 0.14248457227579E+03 0.32326072169031E+03
 0.14248878037464E+03 0.15002637541426E+03 0.14248878037464E+03 0.32314466073272E+03 0.21610661461980E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35925580948698E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19894997914702E+00 0.00000000000000E+00 0.00000000000000E+00 0.19894997914702E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20796512034571E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20796512034571E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936276550660E+00 0.20942633650457E+00 0.33507177860272E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    510.01269931
 0.12046457374393E+00 0.31244044501011E+03 0.44004033571898E+03 0.43491658023765E+03 0.43294048290604E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16236215445278E+00 0.00000000000000E+00 -.23403766915067E+02
 0.29217717477147E-02 0.97383145338247E+00 0.27380646712931E+04 0.10267742517349E+04 0.82149739282020E+01
 0.30806152230757E+01 0.33412566197845E+03 0.29915000674018E+03 0.32888563943558E+03 0.35122742387416E+03
 0.29915000051037E+03 0.29915000093123E+03 0.32561588830497E+03 0.35119874407958E+03 0.29915000040389E+03
 0.29915000092739E+03 0.32888563943558E+03 0.35122742387416E+03 0.29915000051037E+03 0.29915000093123E+03
 0.32561588830497E+03 0.35119874407958E+03 0.29915000040389E+03 0.29915000092739E+03 0.39234484019628E+03
 0.31718436880174E+03 0.18651023872356E+04 0.17605437132530E+04 0.62095210294648E+03 0.10805970214837E+04
 0.45654015802253E+03 0.10967978724057E+04 0.10616998568654E+04 0.10204164365449E+04 0.17151836362282E+04
 0.98274673410664E+03 0.10611827182204E+04 0.92508224330487E+03 0.17149278956421E+04 0.10967978724057E+04
 0.10616998568654E+04 0.10204164365449E+04 0.17151836362282E+04 0.98274673410664E+03 0.10611827182204E+04
 0.92508224330487E+03 0.17149278956421E+04 0.16932673265992E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47872717051277E+03 0.12948769279468E+01
 0.12948769279468E+01 0.18600733234208E+01 0.11214405766764E-02 0.31711736733879E+03 0.33517765585716E+03
 0.33516415349691E+03 0.33516338512577E+03 0.23000000000000E+00 0.00000000000000E+00 0.18093192724306E+00
 0.00000000000000E+00 -.16247808286646E+02 0.15718052794093E+00 0.92540736375654E+00 0.50896889740734E+02
 0.19086333652775E+02 0.86448415187937E+01 0.32418155695476E+01 0.31718283405175E+03 0.39235268602773E+03
 0.30224619414386E+03 0.30819455949256E+03 0.29915000000624E+03 0.29915000011575E+03 0.30223985507427E+03
 0.30819309763174E+03 0.29915000000616E+03 0.29915000011579E+03 0.30224619414386E+03 0.30819455949256E+03
 0.29915000000624E+03 0.29915000011575E+03 0.30223985507427E+03 0.30819309763174E+03 0.29915000000616E+03
 0.29915000011579E+03 0.30682653819430E+03 0.29915000107693E+03 0.90369114422334E+02 0.90325615212208E+02
 0.16188901158922E+03 0.40601381342147E+03 0.24331535677430E+03 0.14464450726656E+03 0.15133525775102E+03
 0.14464450726656E+03 0.32434126705410E+03 0.14465246184247E+03 0.15120599872706E+03 0.14465246184247E+03
 0.32422330799929E+03 0.14464450726656E+03 0.15133525775102E+03 0.14464450726656E+03 0.32434126705410E+03
 0.14465246184247E+03 0.15120599872706E+03 0.14465246184247E+03 0.32422330799929E+03 0.21636939535052E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35940629749294E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19884159438681E+00 0.00000000000000E+00 0.00000000000000E+00 0.19884159438681E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20785475532186E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20785475532186E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936309158603E+00 0.20936055959199E+00 0.33517765585716E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    520.03659042
 0.12037819397264E+00 0.31265183375350E+03 0.44041656537085E+03 0.43528986948902E+03 0.43331135975936E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16179738271897E+00 0.00000000000000E+00 -.23412943796723E+02
 0.29238680226178E-02 0.98113080025612E+00 0.27361016085936E+04 0.10260381032226E+04 0.81538567517314E+01
 0.30576962818993E+01 0.33459231308292E+03 0.29915000834162E+03 0.32928635607061E+03 0.35183885630864E+03
 0.29915000064884E+03 0.29915000118613E+03 0.32599155901353E+03 0.35181043779892E+03 0.29915000051429E+03
 0.29915000118132E+03 0.32928635607061E+03 0.35183885630864E+03 0.29915000064884E+03 0.29915000118613E+03
 0.32599155901353E+03 0.35181043779891E+03 0.29915000051429E+03 0.29915000118132E+03 0.39307057215548E+03
 0.31783380154792E+03 0.18694616979407E+04 0.17633235015602E+04 0.61975752736081E+03 0.10752474831262E+04
 0.45239116812859E+03 0.10998292741816E+04 0.10640634475346E+04 0.10223426629891E+04 0.17151290361693E+04
 0.98602240287993E+03 0.10635573903019E+04 0.92745351312239E+03 0.17148816748895E+04 0.10998292741816E+04
 0.10640634475346E+04 0.10223426629891E+04 0.17151290361693E+04 0.98602240287993E+03 0.10635573903019E+04
 0.92745351312239E+03 0.17148816748895E+04 0.16926843922300E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47909867370669E+03 0.12948769955657E+01
 0.12948769955657E+01 0.19001688878786E+01 0.88770128810767E-03 0.31778795525515E+03 0.33530823713403E+03
 0.33529786861616E+03 0.33529729733609E+03 0.23000000000000E+00 0.00000000000000E+00 0.17992811726860E+00
 0.00000000000000E+00 -.16236091938054E+02 0.19856748970905E+00 0.94413442473360E+00 0.40288568948129E+02
 0.15108213355548E+02 0.84733696711221E+01 0.31775136266708E+01 0.31783238161829E+03 0.39307778038204E+03
 0.30234035749255E+03 0.30833371468851E+03 0.29915000000818E+03 0.29915000014789E+03 0.30233419573687E+03
 0.30833215505731E+03 0.29915000000807E+03 0.29915000014793E+03 0.30234035749255E+03 0.30833371468851E+03
 0.29915000000818E+03 0.29915000014789E+03 0.30233419573687E+03 0.30833215505731E+03 0.29915000000807E+03
 0.29915000014793E+03 0.30694586738161E+03 0.29915000134273E+03 0.88266264157802E+02 0.88239490384457E+02
 0.16343487311236E+03 0.40767520233091E+03 0.24342315485299E+03 0.14722084995724E+03 0.15271395627141E+03
 0.14722084995724E+03 0.32563423624918E+03 0.14723253756059E+03 0.15258180678940E+03 0.14723253756059E+03
 0.32551413988161E+03 0.14722084995724E+03 0.15271395627141E+03 0.14722084995724E+03 0.32563423624918E+03
 0.14723253756059E+03 0.15258180678940E+03 0.14723253756059E+03 0.32551413988161E+03 0.21666371936255E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35958980408951E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19876775790852E+00 0.00000000000000E+00 0.00000000000000E+00 0.19876775790852E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20775742386200E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20775742386200E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936318421108E+00 0.20927914838532E+00 0.33530823713403E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    534.90863933
 0.12024959442545E+00 0.31295843886885E+03 0.44097142670324E+03 0.43584025674715E+03 0.43385802446243E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16099424498349E+00 0.00000000000000E+00 -.23432871003369E+02
 0.29269944772652E-02 0.99144486695086E+00 0.27331790552180E+04 0.10249421457068E+04 0.80690316392515E+01
 0.30258868647193E+01 0.33527513570794E+03 0.29915001138452E+03 0.32987290622333E+03 0.35273414994707E+03
 0.29915000092197E+03 0.29915000169004E+03 0.32654150007181E+03 0.35270609799477E+03 0.29915000073262E+03
 0.29915000168335E+03 0.32987290622333E+03 0.35273414994707E+03 0.29915000092197E+03 0.29915000169004E+03
 0.32654150007181E+03 0.35270609799477E+03 0.29915000073262E+03 0.29915000168335E+03 0.39413838033871E+03
 0.31880224181850E+03 0.18757982085103E+04 0.17673227148762E+04 0.61784750201174E+03 0.10672558146404E+04
 0.44631907511859E+03 0.11042473727106E+04 0.10674694155890E+04 0.10251202441922E+04 0.17150343962494E+04
 0.99080067375583E+03 0.10669786588528E+04 0.93088653734894E+03 0.17147985376539E+04 0.11042473727106E+04
 0.10674694155890E+04 0.10251202441922E+04 0.17150343962494E+04 0.99080067375583E+03 0.10669786588528E+04
 0.93088653734894E+03 0.17147985376539E+04 0.16917662431257E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47964635828304E+03 0.12948771423974E+01
 0.12948771423974E+01 0.19596570835163E+01 0.62752537088295E-03 0.31878244548101E+03 0.33550884813025E+03
 0.33550185063557E+03 0.33550148364114E+03 0.23000000000000E+00 0.00000000000000E+00 0.17848082603874E+00
 0.00000000000000E+00 -.16224570697814E+02 0.28089479126122E+00 0.97106730485056E+00 0.28480414193798E+02
 0.10680155322674E+02 0.82383578975827E+01 0.30893842115935E+01 0.31880098556683E+03 0.39414473558306E+03
 0.30247907756380E+03 0.30853768575866E+03 0.29915000001221E+03 0.29915000021176E+03 0.30247315410264E+03
 0.30853598571496E+03 0.29915000001205E+03 0.29915000021182E+03 0.30247907756380E+03 0.30853768575866E+03
 0.29915000001221E+03 0.29915000021176E+03 0.30247315410264E+03 0.30853598571496E+03 0.29915000001205E+03
 0.29915000021182E+03 0.30712079861928E+03 0.29915000185268E+03 0.84988619302769E+02 0.84979647935819E+02
 0.16565746558187E+03 0.41015513050463E+03 0.24366937759486E+03 0.15102662892412E+03 0.15469364989597E+03
 0.15102662892412E+03 0.32755748973476E+03 0.15104260413433E+03 0.15455746087744E+03 0.15104260413433E+03
 0.32743443569543E+03 0.15102662892412E+03 0.15469364989597E+03 0.15102662892412E+03 0.32755748973476E+03
 0.15104260413433E+03 0.15455746087744E+03 0.15104260413433E+03 0.32743443569543E+03 0.21706267872219E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35986374421758E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19870673549727E+00 0.00000000000000E+00 0.00000000000000E+00 0.19870673549727E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20758709847200E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20758709847200E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936310789365E+00 0.20915395771378E+00 0.33550884813025E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    540.97295978
 0.12018459316791E+00 0.31308834752082E+03 0.44119841333835E+03 0.43606612795802E+03 0.43408273342169E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16067822488980E+00 0.00000000000000E+00 -.23422571647320E+02
 0.29285773456949E-02 0.99547682998516E+00 0.27317017977211E+04 0.10243881741454E+04 0.80363497763371E+01
 0.30136311661264E+01 0.33555283075645E+03 0.29915001283037E+03 0.33011181151612E+03 0.35309516662086E+03
 0.29915000105469E+03 0.29915000193518E+03 0.32676616521568E+03 0.35306726620656E+03 0.29915000083887E+03
 0.29915000192760E+03 0.33011181151612E+03 0.35309516662086E+03 0.29915000105469E+03 0.29915000193518E+03
 0.32676616521568E+03 0.35306726620655E+03 0.29915000083887E+03 0.29915000192760E+03 0.39455543359617E+03
 0.31919217783426E+03 0.18783521811048E+04 0.17689581860451E+04 0.61724596737837E+03 0.10643982693983E+04
 0.44406607218304E+03 0.11060300765559E+04 0.10688527603531E+04 0.10262617274814E+04 0.17150354357854E+04
 0.99272663935455E+03 0.10683679061391E+04 0.93228646590650E+03 0.17148039195591E+04 0.11060300765559E+04
 0.10688527603531E+04 0.10262617274814E+04 0.17150354357855E+04 0.99272663935455E+03 0.10683679061391E+04
 0.93228646590650E+03 0.17148039195591E+04 0.16915139249882E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47987129218157E+03 0.12948770665076E+01
 0.12948770665076E+01 0.19839143653238E+01 0.54474813124417E-03 0.31918256535960E+03 0.33559300675255E+03
 0.33558704704769E+03 0.33558674075263E+03 0.23000000000000E+00 0.00000000000000E+00 0.17790510345893E+00
 0.00000000000000E+00 -.16201032779621E+02 0.32357817218236E+00 0.98175750279278E+00 0.24723546542229E+02
 0.92713299533360E+01 0.81486517569182E+01 0.30557444088443E+01 0.31919099777624E+03 0.39456143238049E+03
 0.30253829595796E+03 0.30862164548688E+03 0.29915000001424E+03 0.29915000024293E+03 0.30253246624775E+03
 0.30861988735774E+03 0.29915000001405E+03 0.29915000024301E+03 0.30253829595796E+03 0.30862164548688E+03
 0.29915000001424E+03 0.29915000024293E+03 0.30253246624775E+03 0.30861988735774E+03 0.29915000001405E+03
 0.29915000024301E+03 0.30719289464564E+03 0.29915000209638E+03 0.83646695129779E+02 0.83643345610471E+02
 0.16653554480246E+03 0.41115222428026E+03 0.24378400175379E+03 0.15256186093131E+03 0.15547325275098E+03
 0.15256186093131E+03 0.32832535492420E+03 0.15257921250235E+03 0.15533551373780E+03 0.15257921250235E+03
 0.32820119841570E+03 0.15256186093131E+03 0.15547325275098E+03 0.15256186093131E+03 0.32832535492420E+03
 0.15257921250235E+03 0.15533551373780E+03 0.15257921250235E+03 0.32820119841570E+03 0.21721450126994E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35997637820273E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19854476001158E+00 0.00000000000000E+00 0.00000000000000E+00 0.19854476001158E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20750505368644E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20750505368644E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936370104523E+00 0.20910217767653E+00 0.33559300675255E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    551.01408165
 0.12008412543374E+00 0.31329687472355E+03 0.44157210325562E+03 0.43643749704459E+03 0.43445189372942E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16016893361124E+00 0.00000000000000E+00 -.23430193003092E+02
 0.29310272251462E-02 0.10019478578393E+01 0.27294185230916E+04 0.10235319461594E+04 0.79844474314777E+01
 0.29941677868041E+01 0.33600810885099E+03 0.29915001558915E+03 0.33050345148409E+03 0.35368894555474E+03
 0.29915000131329E+03 0.29915000241338E+03 0.32713415534412E+03 0.35366128388407E+03 0.29915000104619E+03
 0.29915000240407E+03 0.33050345148409E+03 0.35368894555474E+03 0.29915000131329E+03 0.29915000241338E+03
 0.32713415534412E+03 0.35366128388407E+03 0.29915000104619E+03 0.29915000240407E+03 0.39525208645257E+03
 0.31984230931916E+03 0.18825247480473E+04 0.17715919539531E+04 0.61606506166964E+03 0.10593950283747E+04
 0.44024964139667E+03 0.11089488046868E+04 0.10710891240474E+04 0.10281009411909E+04 0.17150042568009E+04
 0.99588346428602E+03 0.10706135796874E+04 0.93455601227313E+03 0.17147795843401E+04 0.11089488046868E+04
 0.10710891240474E+04 0.10281009411909E+04 0.17150042568009E+04 0.99588346428602E+03 0.10706135796874E+04
 0.93455601227313E+03 0.17147795843401E+04 0.16909917691556E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48024098404710E+03 0.12948771226648E+01
 0.12948771226648E+01 0.20240788527827E+01 0.43098724955870E-03 0.31985757354104E+03 0.33573527458894E+03
 0.33573071253113E+03 0.33573048613929E+03 0.23000000000000E+00 0.00000000000000E+00 0.17697015636536E+00
 0.00000000000000E+00 -.16187444060558E+02 0.40898796652966E+00 0.99908723156170E+00 0.19560477702759E+02
 0.73351791385345E+01 0.80073088187655E+01 0.30027408070371E+01 0.31984126422910E+03 0.39525750984897E+03
 0.30263449450085E+03 0.30875900226253E+03 0.29915000001830E+03 0.29915000030394E+03 0.30262880926931E+03
 0.30875715102746E+03 0.29915000001807E+03 0.29915000030403E+03 0.30263449450085E+03 0.30875900226253E+03
 0.29915000001830E+03 0.29915000030394E+03 0.30262880926931E+03 0.30875715102746E+03 0.29915000001807E+03
 0.29915000030403E+03 0.30731081399689E+03 0.29915000256384E+03 0.81346939050790E+02 0.81350756383805E+02
 0.16796696008644E+03 0.41282529420711E+03 0.24401849932024E+03 0.15509885646050E+03 0.15674442394562E+03
 0.15509885646050E+03 0.32961351317389E+03 0.15511811600164E+03 0.15660422981005E+03 0.15511811600164E+03
 0.32948761927350E+03 0.15509885646050E+03 0.15674442394562E+03 0.15509885646050E+03 0.32961351317389E+03
 0.15511811600164E+03 0.15660422981005E+03 0.15511811600164E+03 0.32948761927350E+03 0.21745616111230E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36016521101738E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19846065679967E+00 0.00000000000000E+00 0.00000000000000E+00 0.19846065679967E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20738807035510E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20738807035510E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936382701405E+00 0.20901373073130E+00 0.33573527458894E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    560.26776269
 0.11999100592471E+00 0.31348640151972E+03 0.44191484365488E+03 0.43677809100117E+03 0.43479041714624E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15971455521975E+00 0.00000000000000E+00 -.23438002274406E+02
 0.29333015869117E-02 0.10076906666288E+01 0.27273022438932E+04 0.10227383414600E+04 0.79389442265686E+01
 0.29771040849632E+01 0.33642385829346E+03 0.29915001858319E+03 0.33086121147042E+03 0.35423107588047E+03
 0.29915000160085E+03 0.29915000294575E+03 0.32747040618926E+03 0.35420362632147E+03 0.29915000127710E+03
 0.29915000293455E+03 0.33086121147042E+03 0.35423107588047E+03 0.29915000160085E+03 0.29915000294575E+03
 0.32747040618926E+03 0.35420362632147E+03 0.29915000127710E+03 0.29915000293455E+03 0.39588918711563E+03
 0.32044229746294E+03 0.18863144275354E+04 0.17739669304919E+04 0.61493258805180E+03 0.10547853305719E+04
 0.43677807957979E+03 0.11116051443687E+04 0.10731079114082E+04 0.10297626518075E+04 0.17149688395099E+04
 0.99875799705942E+03 0.10726404939768E+04 0.93661162661796E+03 0.17147501010206E+04 0.11116051443687E+04
 0.10731079114082E+04 0.10297626518075E+04 0.17149688395099E+04 0.99875799705942E+03 0.10726404939768E+04
 0.93661162661796E+03 0.17147501010206E+04 0.16904935500931E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48058002128429E+03 0.12948771802067E+01
 0.12948771802067E+01 0.20610935769375E+01 0.34726038283539E-03 0.32047506330616E+03 0.33586938295616E+03
 0.33586581906461E+03 0.33586564791127E+03 0.23000000000000E+00 0.00000000000000E+00 0.17612866834129E+00
 0.00000000000000E+00 -.16175399376772E+02 0.50759776255576E+00 0.10146513532931E+01 0.15760510762931E+02
 0.59101915360993E+01 0.78844816734691E+01 0.29566806275509E+01 0.32044136075854E+03 0.39589414956732E+03
 0.30272305964184E+03 0.30888474871034E+03 0.29915000002299E+03 0.29915000037210E+03 0.30271749867982E+03
 0.30888281341153E+03 0.29915000002269E+03 0.29915000037222E+03 0.30272305964184E+03 0.30888474871034E+03
 0.29915000002299E+03 0.29915000037210E+03 0.30271749867982E+03 0.30888281341153E+03 0.29915000002269E+03
 0.29915000037222E+03 0.30741879434237E+03 0.29915000307424E+03 0.79171589150721E+02 0.79181041125936E+02
 0.16926019158395E+03 0.41437633146297E+03 0.24426983892110E+03 0.15742996278215E+03 0.15789185115521E+03
 0.15742996278215E+03 0.33080478182131E+03 0.15745062639639E+03 0.15774949056047E+03 0.15745062639639E+03
 0.33067736970501E+03 0.15742996278215E+03 0.15789185115521E+03 0.15742996278215E+03 0.33080478182131E+03
 0.15745062639639E+03 0.15774949056047E+03 0.15745062639639E+03 0.33067736970501E+03 0.21766787738624E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36034024608653E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19838979972828E+00 0.00000000000000E+00 0.00000000000000E+00 0.19838979972828E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20726560305681E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20726560305681E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936391991333E+00 0.20893039641595E+00 0.33586938295616E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    572.17968218
 0.11988492628465E+00 0.31372557503746E+03 0.44235315570722E+03 0.43721298636497E+03 0.43522234855878E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15915009175346E+00 0.00000000000000E+00 -.23451545704374E+02
 0.29358967572899E-02 0.10147837096573E+01 0.27248914595296E+04 0.10218342973236E+04 0.78834533150928E+01
 0.29562949931598E+01 0.33695403900036E+03 0.29915002319855E+03 0.33131767882291E+03 0.35492169753582E+03
 0.29915000205636E+03 0.29915000379014E+03 0.32789970232136E+03 0.35489451196710E+03 0.29915000164359E+03
 0.29915000377601E+03 0.33131767882291E+03 0.35492169753582E+03 0.29915000205636E+03 0.29915000379014E+03
 0.32789970232136E+03 0.35489451196710E+03 0.29915000164359E+03 0.29915000377601E+03 0.39669853960970E+03
 0.32121398294414E+03 0.18910977826184E+04 0.17769243903813E+04 0.61345693357909E+03 0.10489338063158E+04
 0.43240958806881E+03 0.11149697809680E+04 0.10756422726921E+04 0.10318402347980E+04 0.17149052585748E+04
 0.10024006272284E+04 0.10751847242809E+04 0.93919081387305E+03 0.17146936590850E+04 0.11149697809680E+04
 0.10756422726921E+04 0.10318402347980E+04 0.17149052585748E+04 0.10024006272284E+04 0.10751847242809E+04
 0.93919081387305E+03 0.17146936590850E+04 0.16898339463573E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48101277108527E+03 0.12948772800002E+01
 0.12948772800002E+01 0.21087412549094E+01 0.26283291835627E-03 0.32126101754009E+03 0.33604675231646E+03
 0.33604416153125E+03 0.33604404231466E+03 0.23000000000000E+00 0.00000000000000E+00 0.17507349527625E+00
 0.00000000000000E+00 -.16165207469082E+02 0.67064881860315E+00 0.10341186890154E+01 0.11928746876290E+02
 0.44732800786088E+01 0.77360559140623E+01 0.29010209677733E+01 0.32121318030105E+03 0.39670295339980E+03
 0.30283763691621E+03 0.30904584417650E+03 0.29915000003072E+03 0.29915000048068E+03 0.30283222485098E+03
 0.30904380248799E+03 0.29915000003032E+03 0.29915000048083E+03 0.30283763691621E+03 0.30904584417650E+03
 0.29915000003072E+03 0.29915000048068E+03 0.30283222485098E+03 0.30904380248799E+03 0.29915000003032E+03
 0.29915000048083E+03 0.30755717816391E+03 0.29915000386634E+03 0.76313219645775E+02 0.76327851749608E+02
 0.17089639465933E+03 0.41640077670143E+03 0.24464990006881E+03 0.16042813139531E+03 0.15934203404811E+03
 0.16042813139531E+03 0.33235707750140E+03 0.16045019323312E+03 0.15919698361693E+03 0.16045019323312E+03
 0.33222779675925E+03 0.16042813139531E+03 0.15934203404811E+03 0.16042813139531E+03 0.33235707750141E+03
 0.16045019323312E+03 0.15919698361693E+03 0.16045019323312E+03 0.33222779675925E+03 0.21793744585451E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36057103258901E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19832623409849E+00 0.00000000000000E+00 0.00000000000000E+00 0.19832623409849E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20716387612017E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20716387612017E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936385066636E+00 0.20882006902121E+00 0.33604675231646E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    581.11362180
 0.11980834592267E+00 0.31390513190946E+03 0.44268052909771E+03 0.43753773998683E+03 0.43554488440407E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15874140962276E+00 0.00000000000000E+00 -.23456708513124E+02
 0.29377730949186E-02 0.10198874453355E+01 0.27231510880937E+04 0.10211816580352E+04 0.78440028226532E+01
 0.29415010584950E+01 0.33734840373060E+03 0.29915002727143E+03 0.33165741964207E+03 0.35543449523233E+03
 0.29915000246833E+03 0.29915000455462E+03 0.32821947521099E+03 0.35540750200547E+03 0.29915000197561E+03
 0.29915000453788E+03 0.33165741964207E+03 0.35543449523233E+03 0.29915000246833E+03 0.29915000455462E+03
 0.32821947521099E+03 0.35540750200547E+03 0.29915000197561E+03 0.29915000453788E+03 0.39729684378246E+03
 0.32179140701875E+03 0.18946310648307E+04 0.17791071259870E+04 0.61236304743401E+03 0.10446490734255E+04
 0.42922421075431E+03 0.11174611018014E+04 0.10775092640826E+04 0.10333808406447E+04 0.17148625529141E+04
 0.10050982808652E+04 0.10770587040339E+04 0.94110233314186E+03 0.17146559534043E+04 0.11174611018014E+04
 0.10775092640826E+04 0.10333808406447E+04 0.17148625529140E+04 0.10050982808652E+04 0.10770587040339E+04
 0.94110233314186E+03 0.17146559534043E+04 0.16893575177402E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48133591416035E+03 0.12948773180418E+01
 0.12948773180418E+01 0.21444770133883E+01 0.21325425444862E-03 0.32184560567450E+03 0.33618309546955E+03
 0.33618105711575E+03 0.33618096632521E+03 0.23000000000000E+00 0.00000000000000E+00 0.17430259379881E+00
 0.00000000000000E+00 -.16151861000463E+02 0.82656536515305E+00 0.10483052030118E+01 0.96786053919870E+01
 0.36294770219951E+01 0.76313653476261E+01 0.28617620053598E+01 0.32179071260393E+03 0.39730084890583E+03
 0.30292426946139E+03 0.30916632854923E+03 0.29915000003797E+03 0.29915000057936E+03 0.30291896211646E+03
 0.30916420820134E+03 0.29915000003747E+03 0.29915000057954E+03 0.30292426946139E+03 0.30916632854923E+03
 0.29915000003797E+03 0.29915000057936E+03 0.30291896211646E+03 0.30916420820134E+03 0.29915000003747E+03
 0.29915000057954E+03 0.30766072846608E+03 0.29915000456952E+03 0.74134613064090E+02 0.74151980663578E+02
 0.17210242196939E+03 0.41793076831074E+03 0.24496783423151E+03 0.16266806688962E+03 0.16041003023469E+03
 0.16266806688962E+03 0.33352719100492E+03 0.16269092264391E+03 0.16026305778518E+03 0.16269092264391E+03
 0.33339659565595E+03 0.16266806688962E+03 0.16041003023469E+03 0.16266806688962E+03 0.33352719100492E+03
 0.16269092264391E+03 0.16026305778518E+03 0.16269092264391E+03 0.33339659565595E+03 0.21813506094434E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36074505547459E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19824222157823E+00 0.00000000000000E+00 0.00000000000000E+00 0.19824222157823E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20705968526853E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20705968526853E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936398177369E+00 0.20873554427498E+00 0.33618309546955E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    590.04756142
 0.11972384448788E+00 0.31408500756147E+03 0.44300672455270E+03 0.43786172335398E+03 0.43586682521267E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15834482423474E+00 0.00000000000000E+00 -.23457665940115E+02
 0.29398463240952E-02 0.10248132960057E+01 0.27212306760498E+04 0.10204615035187E+04 0.78062999681803E+01
 0.29273624880676E+01 0.33774003037623E+03 0.29915003193809E+03 0.33199497026135E+03 0.35594306747568E+03
 0.29915000295022E+03 0.29915000544963E+03 0.32853737642574E+03 0.35591626167658E+03 0.29915000236457E+03
 0.29915000542987E+03 0.33199497026135E+03 0.35594306747568E+03 0.29915000295022E+03 0.29915000544963E+03
 0.32853737642574E+03 0.35591626167658E+03 0.29915000236457E+03 0.29915000542987E+03 0.39788873228892E+03
 0.32236774439435E+03 0.18981270745247E+04 0.17812696683483E+04 0.61127060835187E+03 0.10404318404322E+04
 0.42610487903853E+03 0.11199288975689E+04 0.10793487231140E+04 0.10349099474700E+04 0.17148230846800E+04
 0.10077710421816E+04 0.10789048214678E+04 0.94299872788247E+03 0.17146212059941E+04 0.11199288975689E+04
 0.10793487231140E+04 0.10349099474700E+04 0.17148230846800E+04 0.10077710421816E+04 0.10789048214678E+04
 0.94299872788246E+03 0.17146212059941E+04 0.16888950155003E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48165837401469E+03 0.12948773250965E+01
 0.12948773250965E+01 0.21802127718671E+01 0.17301797654135E-03 0.32242733028949E+03 0.33632155038480E+03
 0.33631994775157E+03 0.33631987869831E+03 0.23000000000000E+00 0.00000000000000E+00 0.17354913152960E+00
 0.00000000000000E+00 -.16133687127460E+02 0.10187876350520E+01 0.10621415839593E+01 0.78524706472237E+01
 0.29446764927089E+01 0.75319525389246E+01 0.28244822020967E+01 0.32236715958052E+03 0.39789234477256E+03
 0.30301137942896E+03 0.30928647050402E+03 0.29915000004671E+03 0.29915000069528E+03 0.30300617125378E+03
 0.30928427250113E+03 0.29915000004610E+03 0.29915000069549E+03 0.30301137942896E+03 0.30928647050402E+03
 0.29915000004671E+03 0.29915000069528E+03 0.30300617125378E+03 0.30928427250113E+03 0.29915000004610E+03
 0.29915000069549E+03 0.30776402770186E+03 0.29915000537922E+03 0.71923621823951E+02 0.71943089749484E+02
 0.17328877670131E+03 0.41946251648383E+03 0.24530729589901E+03 0.16490024932487E+03 0.16145947963587E+03
 0.16490024932487E+03 0.33469558569579E+03 0.16492371939436E+03 0.16131065483580E+03 0.16492371939436E+03
 0.33456373781677E+03 0.16490024932487E+03 0.16145947963587E+03 0.16490024932487E+03 0.33469558569579E+03
 0.16492371939436E+03 0.16131065483580E+03 0.16492371939436E+03 0.33456373781677E+03 0.21832432607997E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36091933889905E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19812733000802E+00 0.00000000000000E+00 0.00000000000000E+00 0.19812733000802E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20693222715554E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20693222715554E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936426951928E+00 0.20864995182100E+00 0.33632155038480E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    600.31868426
 0.11983591318498E+00 0.31427367895346E+03 0.44338041024391E+03 0.43822320256307E+03 0.43622203905026E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15790424121150E+00 0.00000000000000E+00 -.23573773954702E+02
 0.29370967492402E-02 0.10302497303168E+01 0.27237781670180E+04 0.10214168126317E+04 0.77651075895355E+01
 0.29119153460758E+01 0.33818761971700E+03 0.29915003800573E+03 0.33238109854235E+03 0.35652268667993E+03
 0.29915000358851E+03 0.29915000663596E+03 0.32890149528740E+03 0.35649609301997E+03 0.29915000288045E+03
 0.29915000661227E+03 0.33238109854235E+03 0.35652268667993E+03 0.29915000358851E+03 0.29915000663596E+03
 0.32890149528740E+03 0.35649609301997E+03 0.29915000288045E+03 0.29915000661227E+03 0.39855582714951E+03
 0.32318546131675E+03 0.19019610809839E+04 0.17834612724450E+04 0.61008449785092E+03 0.10357886069830E+04
 0.42265368664282E+03 0.11226838925093E+04 0.10814243214924E+04 0.10365108358634E+04 0.17147836206348E+04
 0.10107528891559E+04 0.10809875870082E+04 0.94501007932747E+03 0.17145867217317E+04 0.11226838925093E+04
 0.10814243214924E+04 0.10365108358634E+04 0.17147836206348E+04 0.10107528891559E+04 0.10809875870082E+04
 0.94501007932747E+03 0.17145867217317E+04 0.16883112815040E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48201613930849E+03 0.12948781806283E+01
 0.12948781806283E+01 0.22212972632296E+01 0.00000000000000E+00 0.33643458417345E+03 0.33643458417345E+03
 0.33643458417345E+03 0.33643458417345E+03 0.00000000000000E+00 0.00000000000000E+00 0.17270751027639E+00
 0.00000000000000E+00 -.16246598243969E+02 0.10000000000000E-02 0.10778214044306E+01 0.80000000000000E+04
 0.30000000000000E+04 0.74223799667682E+01 0.27833924875381E+01 0.32318444065969E+03 0.39855897666315E+03
 0.30942327263735E+03 0.30942327263735E+03 0.29915000085704E+03 0.29915000084939E+03 0.30942108434392E+03
 0.30942108434392E+03 0.29915000085731E+03 0.29915000084965E+03 0.30942327263735E+03 0.30942327263735E+03
 0.29915000085704E+03 0.29915000084939E+03 0.30942108434392E+03 0.30942108434392E+03 0.29915000085731E+03
 0.29915000084965E+03 0.30788068925347E+03 0.29915000643668E+03 0.68683591158773E+02 0.84995560919339E+02
 0.17389110954755E+03 0.42006886685031E+03 0.24530830175502E+03 0.12670347399164E+03 0.16232955060901E+03
 0.12670347399164E+03 0.33541019616125E+03 0.12671363983185E+03 0.16220322128580E+03 0.12671363983185E+03
 0.33530076684879E+03 0.12670347399164E+03 0.16232955060901E+03 0.12670347399164E+03 0.33541019616126E+03
 0.12671363983185E+03 0.16220322128580E+03 0.12671363983185E+03 0.33530076684879E+03 0.22182538999560E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36111157841254E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19885435922567E+00 0.00000000000000E+00 0.00000000000000E+00 0.19885435922567E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20740818558760E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20740818558760E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936023374041E+00 0.20857539566771E+00 0.33643458417345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    610.87089559
 0.11987688900738E+00 0.31448067204502E+03 0.44375869187063E+03 0.43859287625939E+03 0.43658709553964E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15746979512605E+00 0.00000000000000E+00 -.23540767267569E+02
 0.29360925158697E-02 0.10355801266662E+01 0.27247097823927E+04 0.10217661683972E+04 0.77251385904382E+01
 0.28969269714143E+01 0.33864259348010E+03 0.29915004528474E+03 0.33277379947643E+03 0.35711273730138E+03
 0.29915000437230E+03 0.29915000809396E+03 0.32927173293439E+03 0.35708635212404E+03 0.29915000351499E+03
 0.29915000806551E+03 0.33277379947643E+03 0.35711273730138E+03 0.29915000437230E+03 0.29915000809396E+03
 0.32927173293439E+03 0.35708635212404E+03 0.29915000351499E+03 0.29915000806551E+03 0.39923926869632E+03
 0.32393305643539E+03 0.19058363495315E+04 0.17857663217434E+04 0.60871845505763E+03 0.10308423721229E+04
 0.41908032479001E+03 0.11254667025084E+04 0.10834545336888E+04 0.10382008379814E+04 0.17146443056334E+04
 0.10137706413284E+04 0.10830247276771E+04 0.94711886977297E+03 0.17144521788363E+04 0.11254667025084E+04
 0.10834545336888E+04 0.10382008379814E+04 0.17146443056334E+04 0.10137706413284E+04 0.10830247276771E+04
 0.94711886977297E+03 0.17144521788363E+04 0.16876369244639E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48238288640363E+03 0.12948779374213E+01
 0.12948779374213E+01 0.22635061085675E+01 0.00000000000000E+00 0.33655626010974E+03 0.33655626010974E+03
 0.33655626010974E+03 0.33655626010974E+03 0.00000000000000E+00 0.00000000000000E+00 0.17185978556161E+00
 0.00000000000000E+00 -.16187845623153E+02 0.10000000000000E-02 0.10934559558144E+01 0.80000000000000E+04
 0.30000000000000E+04 0.73162526185533E+01 0.27435947319575E+01 0.32393197215618E+03 0.39924201852136E+03
 0.30956072404096E+03 0.30956072404096E+03 0.29915000105700E+03 0.29915000103951E+03 0.30955849118381E+03
 0.30955849118381E+03 0.29915000105732E+03 0.29915000103983E+03 0.30956072404096E+03 0.30956072404096E+03
 0.29915000105700E+03 0.29915000103951E+03 0.30955849118381E+03 0.30955849118381E+03 0.29915000105732E+03
 0.29915000103983E+03 0.30799862470832E+03 0.29915000771226E+03 0.65178227516717E+02 0.80529951085242E+02
 0.17501081121080E+03 0.42121057754096E+03 0.24532471227410E+03 0.12911225260628E+03 0.16326570567198E+03
 0.12911225260628E+03 0.33619993277510E+03 0.12912264762110E+03 0.16313562425382E+03 0.12912264762110E+03
 0.33608709036399E+03 0.12911225260629E+03 0.16326570567198E+03 0.12911225260629E+03 0.33619993277510E+03
 0.12912264762110E+03 0.16313562425382E+03 0.12912264762110E+03 0.33608709036399E+03 0.22126416561348E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36127228527305E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19847250107609E+00 0.00000000000000E+00 0.00000000000000E+00 0.19847250107609E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20711053263123E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20711053263123E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936191734701E+00 0.20850187069076E+00 0.33655626010974E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    621.57258755
 0.11979687465345E+00 0.31469713097472E+03 0.44414064977970E+03 0.43897167344737E+03 0.43696342013739E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15704548075995E+00 0.00000000000000E+00 -.23515931363383E+02
 0.29380532803036E-02 0.10407481522652E+01 0.27228913967051E+04 0.10210842737644E+04 0.76867779996415E+01
 0.28825417498655E+01 0.33910045441079E+03 0.29915005387928E+03 0.33316924538287E+03 0.35770522054886E+03
 0.29915000531935E+03 0.29915000985704E+03 0.32964482028069E+03 0.35767904017201E+03 0.29915000428300E+03
 0.29915000982295E+03 0.33316924538287E+03 0.35770522054886E+03 0.29915000531935E+03 0.29915000985704E+03
 0.32964482028069E+03 0.35767904017201E+03 0.29915000428300E+03 0.29915000982295E+03 0.39992343806730E+03
 0.32465810603669E+03 0.19097874306544E+04 0.17881891719238E+04 0.60734639810285E+03 0.10259343696140E+04
 0.41555123952065E+03 0.11282844273332E+04 0.10854845429566E+04 0.10399491329459E+04 0.17145127184229E+04
 0.10168277791011E+04 0.10850614121039E+04 0.94929092730549E+03 0.17143251467590E+04 0.11282844273332E+04
 0.10854845429566E+04 0.10399491329459E+04 0.17145127184229E+04 0.10168277791011E+04 0.10850614121039E+04
 0.94929092730549E+03 0.17143251467590E+04 0.16870230940286E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48275983078401E+03 0.12948777544200E+01
 0.12948777544200E+01 0.23063128764070E+01 0.00000000000000E+00 0.33669906670341E+03 0.33669906670341E+03
 0.33669906670341E+03 0.33669906670341E+03 0.00000000000000E+00 0.00000000000000E+00 0.17102506085990E+00
 0.00000000000000E+00 -.16137629239860E+02 0.10000000000000E-02 0.11087624200872E+01 0.80000000000000E+04
 0.30000000000000E+04 0.72152517573339E+01 0.27057194090002E+01 0.32465749734468E+03 0.39992580721094E+03
 0.30969943605201E+03 0.30969943605201E+03 0.29915000129168E+03 0.29915000127031E+03 0.30969714303466E+03
 0.30969714303466E+03 0.29915000129207E+03 0.29915000127070E+03 0.30969943605201E+03 0.30969943605201E+03
 0.29915000129168E+03 0.29915000127031E+03 0.30969714303466E+03 0.30969714303466E+03 0.29915000129207E+03
 0.29915000127070E+03 0.30811788837762E+03 0.29915000922650E+03 0.61900353577859E+02 0.76368521389866E+02
 0.17621160890086E+03 0.42263233329268E+03 0.24553966634731E+03 0.13151957918021E+03 0.16429061423623E+03
 0.13151957918021E+03 0.33722876435219E+03 0.13153029887681E+03 0.16415732432062E+03 0.13153029887681E+03
 0.33711317528379E+03 0.13151957918021E+03 0.16429061423623E+03 0.13151957918021E+03 0.33722876435219E+03
 0.13153029887681E+03 0.16415732432062E+03 0.13153029887681E+03 0.33711317528379E+03 0.22091496374126E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36145337843632E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19814638311383E+00 0.00000000000000E+00 0.00000000000000E+00 0.19814638311383E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20684847188824E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20684847188824E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936326679020E+00 0.20841495134777E+00 0.33669906670341E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    630.49066419
 0.11967042040825E+00 0.31487832566193E+03 0.44445894553231E+03 0.43928995644676E+03 0.43728062116410E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15670303053079E+00 0.00000000000000E+00 -.23503049628255E+02
 0.29411576282830E-02 0.10448890144526E+01 0.27200174254755E+04 0.10200065345533E+04 0.76563155410251E+01
 0.28711183278844E+01 0.33947950659124E+03 0.29915006206329E+03 0.33349676760494E+03 0.35819476018619E+03
 0.29915000623969E+03 0.29915001157145E+03 0.32995403446718E+03 0.35816874575989E+03 0.29915000503046E+03
 0.29915001153196E+03 0.33349676760494E+03 0.35819476018619E+03 0.29915000623969E+03 0.29915001157145E+03
 0.32995403446718E+03 0.35816874575989E+03 0.29915000503046E+03 0.29915001153196E+03 0.40048755261028E+03
 0.32524942722923E+03 0.19130986910703E+04 0.17902463030639E+04 0.60622727157636E+03 0.10219441820222E+04
 0.41268577408799E+03 0.11306348376363E+04 0.10871753079441E+04 0.10414156736002E+04 0.17144403361218E+04
 0.10193775850376E+04 0.10867574827507E+04 0.95110955636637E+03 0.17142563518644E+04 0.11306348376363E+04
 0.10871753079441E+04 0.10414156736002E+04 0.17144403361218E+04 0.10193775850376E+04 0.10867574827507E+04
 0.95110955636637E+03 0.17142563518644E+04 0.16865747367269E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48307706985023E+03 0.12948776595021E+01
 0.12948776595021E+01 0.23419851829399E+01 0.00000000000000E+00 0.33682896571803E+03 0.33682896571803E+03
 0.33682896571803E+03 0.33682896571803E+03 0.00000000000000E+00 0.00000000000000E+00 0.17034821420076E+00
 0.00000000000000E+00 -.16104092113550E+02 0.10000000000000E-02 0.11211162414900E+01 0.80000000000000E+04
 0.30000000000000E+04 0.71357453437371E+01 0.26759045039014E+01 0.32524911733249E+03 0.40048960963820E+03
 0.30981489791196E+03 0.30981489791196E+03 0.29915000152067E+03 0.29915000149551E+03 0.30981254986075E+03
 0.30981254986075E+03 0.29915000152113E+03 0.29915000149597E+03 0.30981489791196E+03 0.30981489791196E+03
 0.29915000152067E+03 0.29915000149551E+03 0.30981254986075E+03 0.30981254986075E+03 0.29915000152113E+03
 0.29915000149597E+03 0.30821726315585E+03 0.29915001067520E+03 0.59277614984186E+02 0.73053225934609E+02
 0.17724605205482E+03 0.42396087033106E+03 0.24582858801597E+03 0.13352022351745E+03 0.16518247883482E+03
 0.13352022351745E+03 0.33820773752230E+03 0.13353124440408E+03 0.16504676543390E+03 0.13353124440408E+03
 0.33809014911525E+03 0.13352022351745E+03 0.16518247883482E+03 0.13352022351745E+03 0.33820773752230E+03
 0.13353124440408E+03 0.16504676543390E+03 0.13353124440408E+03 0.33809014911526E+03 0.22073159375160E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36161476395958E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19793105849631E+00 0.00000000000000E+00 0.00000000000000E+00 0.19793105849631E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20665523823833E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20665523823833E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936408843363E+00 0.20833550315807E+00 0.33682896571803E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    641.19235615
 0.11949316887304E+00 0.31509303516172E+03 0.44484080112243E+03 0.43967281055282E+03 0.43766249068041E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15630439778441E+00 0.00000000000000E+00 -.23494838161514E+02
 0.29455201179659E-02 0.10496750076375E+01 0.27159889186310E+04 0.10184958444866E+04 0.76214065704068E+01
 0.28580274639026E+01 0.33993140021993E+03 0.29915007325287E+03 0.33388736454336E+03 0.35877745363801E+03
 0.29915000752350E+03 0.29915001396431E+03 0.33032303610905E+03 0.35875163287513E+03 0.29915000607465E+03
 0.29915001391740E+03 0.33388736454336E+03 0.35877745363801E+03 0.29915000752350E+03 0.29915001396431E+03
 0.33032303610905E+03 0.35875163287513E+03 0.29915000607465E+03 0.29915001391740E+03 0.40115800589425E+03
 0.32594860106735E+03 0.19170689796097E+04 0.17927095123243E+04 0.60490566886130E+03 0.10172563521650E+04
 0.40932615495940E+03 0.11334491880582E+04 0.10892019960732E+04 0.10431613906761E+04 0.17143951808600E+04
 0.10224298896811E+04 0.10887902384829E+04 0.95327546512251E+03 0.17142152587739E+04 0.11334491880582E+04
 0.10892019960732E+04 0.10431613906761E+04 0.17143951808600E+04 0.10224298896811E+04 0.10887902384829E+04
 0.95327546512251E+03 0.17142152587739E+04 0.16860866237943E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48345884047979E+03 0.12948775989967E+01
 0.12948775989967E+01 0.23847919507794E+01 0.00000000000000E+00 0.33699544502104E+03 0.33699544502104E+03
 0.33699544502104E+03 0.33699544502104E+03 0.00000000000000E+00 0.00000000000000E+00 0.16955777379195E+00
 0.00000000000000E+00 -.16071701828767E+02 0.10000000000000E-02 0.11354802518850E+01 0.80000000000000E+04
 0.30000000000000E+04 0.70454770012244E+01 0.26420538754592E+01 0.32594851856148E+03 0.40115970836228E+03
 0.30995349491527E+03 0.30995349491527E+03 0.29915000184138E+03 0.29915000181092E+03 0.30995107752722E+03
 0.30995107752722E+03 0.29915000184193E+03 0.29915000181147E+03 0.30995349491527E+03 0.30995349491527E+03
 0.29915000184138E+03 0.29915000181092E+03 0.30995107752722E+03 0.30995107752722E+03 0.29915000184193E+03
 0.29915000181147E+03 0.30833663058314E+03 0.29915001266501E+03 0.56214807557277E+02 0.69199685917715E+02
 0.17851816279445E+03 0.42569155998809E+03 0.24628080637967E+03 0.13592157721789E+03 0.16628653329732E+03
 0.13592157721789E+03 0.33949656401201E+03 0.13593298056185E+03 0.16614812122013E+03 0.13593298056185E+03
 0.33937681283795E+03 0.13592157721789E+03 0.16628653329733E+03 0.13592157721789E+03 0.33949656401201E+03
 0.13593298056185E+03 0.16614812122013E+03 0.13593298056185E+03 0.33937681283795E+03 0.22060849808011E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36181860386821E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19772603541258E+00 0.00000000000000E+00 0.00000000000000E+00 0.19772603541258E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20644775446717E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20644775446717E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936478754749E+00 0.20823338003494E+00 0.33699544502104E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    650.11043278
 0.11933984901503E+00 0.31526979069548E+03 0.44515854904392E+03 0.43999158077391E+03 0.43798047089377E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15598195118957E+00 0.00000000000000E+00 -.23491894336636E+02
 0.29493040674325E-02 0.10535193260369E+01 0.27125043118950E+04 0.10171891169606E+04 0.75935958670014E+01
 0.28475984501255E+01 0.34030552356072E+03 0.29915008383888E+03 0.33421085281918E+03 0.35925920148708E+03
 0.29915000876195E+03 0.29915001627380E+03 0.33062883077762E+03 0.35923353770348E+03 0.29915000708341E+03
 0.29915001621984E+03 0.33421085281918E+03 0.35925920148708E+03 0.29915000876195E+03 0.29915001627380E+03
 0.33062883077762E+03 0.35923353770348E+03 0.29915000708341E+03 0.29915001621984E+03 0.40171155918229E+03
 0.32652448386018E+03 0.19203616037719E+04 0.17947450506057E+04 0.60381508344511E+03 0.10134187736609E+04
 0.40658461479854E+03 0.11357837200489E+04 0.10908835592217E+04 0.10446013312614E+04 0.17143829587157E+04
 0.10249612598039E+04 0.10904766198123E+04 0.95506284498307E+03 0.17142062289843E+04 0.11357837200489E+04
 0.10908835592217E+04 0.10446013312614E+04 0.17143829587157E+04 0.10249612598039E+04 0.10904766198123E+04
 0.95506284498307E+03 0.17142062289843E+04 0.16857071967765E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48377672865136E+03 0.12948775773053E+01
 0.12948775773053E+01 0.24204642573123E+01 0.00000000000000E+00 0.33714120037181E+03 0.33714120037181E+03
 0.33714120037181E+03 0.33714120037181E+03 0.00000000000000E+00 0.00000000000000E+00 0.16891672744216E+00
 0.00000000000000E+00 -.16049007207285E+02 0.10000000000000E-02 0.11470815067657E+01 0.80000000000000E+04
 0.30000000000000E+04 0.69742210582371E+01 0.26153328968389E+01 0.32652452280471E+03 0.40171298306004E+03
 0.31006909836840E+03 0.31006909836840E+03 0.29915000215196E+03 0.29915000211636E+03 0.31006662147597E+03
 0.31006662147597E+03 0.29915000215261E+03 0.29915000211700E+03 0.31006909836839E+03 0.31006909836839E+03
 0.29915000215196E+03 0.29915000211636E+03 0.31006662147597E+03 0.31006662147597E+03 0.29915000215261E+03
 0.29915000211700E+03 0.30843624870839E+03 0.29915001455580E+03 0.53710459467526E+02 0.66063943260128E+02
 0.17959561756049E+03 0.42722026097604E+03 0.24672666532775E+03 0.13792336028081E+03 0.16722574567645E+03
 0.13792336028081E+03 0.34064212034882E+03 0.13793509370458E+03 0.16708522048358E+03 0.13793509370458E+03
 0.34052071699276E+03 0.13792336028081E+03 0.16722574567645E+03 0.13792336028081E+03 0.34064212034882E+03
 0.13793509370458E+03 0.16708522048358E+03 0.13793509370458E+03 0.34052071699276E+03 0.22056627671860E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36199503694774E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19758434252992E+00 0.00000000000000E+00 0.00000000000000E+00 0.19758434252992E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20628880700339E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20628880700339E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936521051124E+00 0.20814384328749E+00 0.33714120037181E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    663.50883142
 0.11913171253378E+00 0.31552600179811E+03 0.44563260273078E+03 0.44046599533710E+03 0.43845311485316E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15551358883092E+00 0.00000000000000E+00 -.23501946107418E+02
 0.29544564509250E-02 0.10590636881784E+01 0.27077738774911E+04 0.10154152040592E+04 0.75538422186492E+01
 0.28326908319935E+01 0.34086207042854E+03 0.29915010249163E+03 0.33469214115707E+03 0.35997641989392E+03
 0.29915001099959E+03 0.29915002044902E+03 0.33108379705280E+03 0.35995098225952E+03 0.29915000890948E+03
 0.29915002038257E+03 0.33469214115707E+03 0.35997641989392E+03 0.29915001099959E+03 0.29915002044902E+03
 0.33108379705280E+03 0.35995098225952E+03 0.29915000890948E+03 0.29915002038257E+03 0.40254086791965E+03
 0.32738510961937E+03 0.19252375318960E+04 0.17977002377986E+04 0.60207095104032E+03 0.10075459214143E+04
 0.40246461561877E+03 0.11392509588662E+04 0.10933599251686E+04 0.10466946535699E+04 0.17143504840433E+04
 0.10287225587308E+04 0.10929598178742E+04 0.95767648008875E+03 0.17141782394938E+04 0.11392509588662E+04
 0.10933599251686E+04 0.10466946535699E+04 0.17143504840432E+04 0.10287225587308E+04 0.10929598178742E+04
 0.95767648008875E+03 0.17141782394938E+04 0.16850734840385E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48424954622326E+03 0.12948776513709E+01
 0.12948776513709E+01 0.24740578518828E+01 0.00000000000000E+00 0.33736976792873E+03 0.33736976792873E+03
 0.33736976792873E+03 0.33736976792873E+03 0.00000000000000E+00 0.00000000000000E+00 0.16798281386675E+00
 0.00000000000000E+00 -.16031272032791E+02 0.10000000000000E-02 0.11639073079785E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68733995784380E+01 0.25775248419142E+01 0.32738525866671E+03 0.40254192532326E+03
 0.31024208770468E+03 0.31024208770468E+03 0.29915000275011E+03 0.29915000267101E+03 0.31023952015837E+03
 0.31023952015837E+03 0.29915000275095E+03 0.29915000267181E+03 0.31024208770468E+03 0.31024208770468E+03
 0.29915000275011E+03 0.29915000267101E+03 0.31023952015837E+03 0.31023952015837E+03 0.29915000275095E+03
 0.29915000267181E+03 0.30858533210200E+03 0.29915001790612E+03 0.49967720154031E+02 0.61405324485710E+02
 0.18124083008176E+03 0.42964835792298E+03 0.24750132369081E+03 0.14095188772487E+03 0.16866473444300E+03
 0.14095188772487E+03 0.34247167237153E+03 0.14096412563908E+03 0.16852113854819E+03 0.14096412563908E+03
 0.34234790245726E+03 0.14095188772487E+03 0.16866473444300E+03 0.14095188772487E+03 0.34247167237154E+03
 0.14096412563908E+03 0.16852113854819E+03 0.14096412563908E+03 0.34234790245726E+03 0.22057953309983E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36227039370765E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19748145675128E+00 0.00000000000000E+00 0.00000000000000E+00 0.19748145675128E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20610751650188E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20610751650188E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936527721763E+00 0.20800293214007E+00 0.33736976792873E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    672.44109718
 0.11900894720777E+00 0.31569671655471E+03 0.44594726055029E+03 0.44078026717891E+03 0.43876599801430E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15521190020781E+00 0.00000000000000E+00 -.23501612390270E+02
 0.29575039092960E-02 0.10626066391478E+01 0.27049837448581E+04 0.10143689043218E+04 0.75286561416705E+01
 0.28232460531264E+01 0.34123060087664E+03 0.29915011676367E+03 0.33501103310623E+03 0.36045015332845E+03
 0.29915001274921E+03 0.29915002371507E+03 0.33138557149331E+03 0.36042486265480E+03 0.29915001033961E+03
 0.29915002363903E+03 0.33501103310623E+03 0.36045015332845E+03 0.29915001274921E+03 0.29915002371507E+03
 0.33138557149331E+03 0.36042486265480E+03 0.29915001033961E+03 0.29915002363903E+03 0.40308474922278E+03
 0.32795118469990E+03 0.19284429493262E+04 0.17996382604745E+04 0.60095148492495E+03 0.10037647900653E+04
 0.39980854771576E+03 0.11415374978287E+04 0.10949912314436E+04 0.10480766266386E+04 0.17143406727541E+04
 0.10312022857978E+04 0.10945954390223E+04 0.95939907427477E+03 0.17141712187230E+04 0.11415374978287E+04
 0.10949912314436E+04 0.10480766266386E+04 0.17143406727541E+04 0.10312022857978E+04 0.10945954390223E+04
 0.95939907427477E+03 0.17141712187230E+04 0.16846795298283E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48456264233838E+03 0.12948776489120E+01
 0.12948776489120E+01 0.25097869149298E+01 0.00000000000000E+00 0.33752767257103E+03 0.33752767257103E+03
 0.33752767257103E+03 0.33752767257103E+03 0.00000000000000E+00 0.00000000000000E+00 0.16737905785269E+00
 0.00000000000000E+00 -.16011996960082E+02 0.10000000000000E-02 0.11747365688046E+01 0.80000000000000E+04
 0.30000000000000E+04 0.68100374266386E+01 0.25537640349895E+01 0.32795141141636E+03 0.40308555812531E+03
 0.31035793673716E+03 0.31035793673716E+03 0.29915000319857E+03 0.29915000310656E+03 0.31035530760553E+03
 0.31035530760553E+03 0.29915000319953E+03 0.29915000310750E+03 0.31035793673716E+03 0.31035793673716E+03
 0.29915000319857E+03 0.29915000310656E+03 0.31035530760553E+03 0.31035530760553E+03 0.29915000319953E+03
 0.29915000310750E+03 0.30868523777223E+03 0.29915002048189E+03 0.47517250581873E+02 0.58371139582072E+02
 0.18234511573426E+03 0.43132251218250E+03 0.24806567086956E+03 0.14296308284348E+03 0.16963306209991E+03
 0.14296308284348E+03 0.34373599998364E+03 0.14297566541162E+03 0.16948755957160E+03 0.14297566541162E+03
 0.34361080244755E+03 0.14296308284348E+03 0.16963306209992E+03 0.14296308284348E+03 0.34373599998364E+03
 0.14297566541162E+03 0.16948755957160E+03 0.14297566541162E+03 0.34361080244755E+03 0.22063225271426E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36245756784217E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19736145103665E+00 0.00000000000000E+00 0.00000000000000E+00 0.19736145103665E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20596834531959E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20596834531959E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936555861080E+00 0.20790595571793E+00 0.33752767257103E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    681.37336295
 0.11887221569612E+00 0.31586996707196E+03 0.44626090517529E+03 0.44109428526894E+03 0.43907898084136E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15491837135478E+00 0.00000000000000E+00 -.23487893319315E+02
 0.29609054941059E-02 0.10660313522178E+01 0.27018761713013E+04 0.10132035642380E+04 0.75044697169148E+01
 0.28141761438431E+01 0.34159705188293E+03 0.29915013266111E+03 0.33532827187277E+03 0.36092054515656E+03
 0.29915001473172E+03 0.29915002741694E+03 0.33168595895657E+03 0.36089539800575E+03 0.29915001196221E+03
 0.29915002733019E+03 0.33532827187277E+03 0.36092054515656E+03 0.29915001473172E+03 0.29915002741694E+03
 0.33168595895657E+03 0.36089539800575E+03 0.29915001196221E+03 0.29915002733019E+03 0.40362373376830E+03
 0.32851288563608E+03 0.19316332631909E+04 0.18015904368012E+04 0.59984056690623E+03 0.10000409903208E+04
 0.39720122058001E+03 0.11438126945328E+04 0.10966093152616E+04 0.10494695213619E+04 0.17143408823075E+04
 0.10336696503093E+04 0.10962176595151E+04 0.96112911229626E+03 0.17141740758243E+04 0.11438126945328E+04
 0.10966093152616E+04 0.10494695213619E+04 0.17143408823075E+04 0.10336696503093E+04 0.10962176595151E+04
 0.96112911229626E+03 0.17141740758243E+04 0.16843105806517E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48487565349569E+03 0.12948775478242E+01
 0.12948775478242E+01 0.25455159779769E+01 0.00000000000000E+00 0.33768768588896E+03 0.33768768588896E+03
 0.33768768588896E+03 0.33768768588896E+03 0.00000000000000E+00 0.00000000000000E+00 0.16679016807670E+00
 0.00000000000000E+00 -.15977266526825E+02 0.10000000000000E-02 0.11852678561766E+01 0.80000000000000E+04
 0.30000000000000E+04 0.67495291957095E+01 0.25310734483910E+01 0.32851319448318E+03 0.40362430022351E+03
 0.31047394761494E+03 0.31047394761494E+03 0.29915000370845E+03 0.29915000360177E+03 0.31047125651607E+03
 0.31047125651607E+03 0.29915000370956E+03 0.29915000360285E+03 0.31047394761494E+03 0.31047394761494E+03
 0.29915000370845E+03 0.29915000360177E+03 0.31047125651607E+03 0.31047125651607E+03 0.29915000370956E+03
 0.29915000360285E+03 0.30878532701326E+03 0.29915002336174E+03 0.45075733699499E+02 0.55361369152312E+02
 0.18344361311193E+03 0.43300442480999E+03 0.24864359363250E+03 0.14496690932151E+03 0.17059637666337E+03
 0.14496690932151E+03 0.34500437161131E+03 0.14497983993589E+03 0.17044904133401E+03 0.14497983993589E+03
 0.34487782312940E+03 0.14496690932151E+03 0.17059637666337E+03 0.14496690932151E+03 0.34500437161131E+03
 0.14497983993589E+03 0.17044904133401E+03 0.14497983993589E+03 0.34487782312940E+03 0.22069391621652E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36264292560459E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19714157575159E+00 0.00000000000000E+00 0.00000000000000E+00 0.19714157575159E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20575703312692E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20575703312692E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936635243361E+00 0.20780833817685E+00 0.33768768588896E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    690.90304147
 0.11872327625148E+00 0.31605877193027E+03 0.44659631295795E+03 0.44143036477641E+03 0.43941413404593E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15461372131130E+00 0.00000000000000E+00 -.23497299362619E+02
 0.29646197063224E-02 0.10695568920996E+01 0.26984911362962E+04 0.10119341761111E+04 0.74797330175633E+01
 0.28048998815862E+01 0.34198714440691E+03 0.29915015116276E+03 0.33566627665640E+03 0.36141886729766E+03
 0.29915001707062E+03 0.29915003178522E+03 0.33200652825660E+03 0.36139387169642E+03 0.29915001387845E+03
 0.29915003168597E+03 0.33566627665640E+03 0.36141886729766E+03 0.29915001707062E+03 0.29915003178522E+03
 0.33200652825660E+03 0.36139387169642E+03 0.29915001387845E+03 0.29915003168597E+03 0.40418707371283E+03
 0.32910230286111E+03 0.19350310869721E+04 0.18036984540521E+04 0.59878259060693E+03 0.99633873410333E+03
 0.39456223054336E+03 0.11462358828252E+04 0.10983509909153E+04 0.10509767752693E+04 0.17143968548418E+04
 0.10362947122592E+04 0.10979635737469E+04 0.96298840600468E+03 0.17142327180120E+04 0.11462358828252E+04
 0.10983509909153E+04 0.10509767752693E+04 0.17143968548418E+04 0.10362947122592E+04 0.10979635737469E+04
 0.96298840600468E+03 0.17142327180120E+04 0.16840208216989E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48521073275109E+03 0.12948776171318E+01
 0.12948776171318E+01 0.25836346920741E+01 0.00000000000000E+00 0.33786131850133E+03 0.33786131850133E+03
 0.33786131850133E+03 0.33786131850133E+03 0.00000000000000E+00 0.00000000000000E+00 0.16617777028189E+00
 0.00000000000000E+00 -.15968003295804E+02 0.10000000000000E-02 0.11961823162449E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66879437117195E+01 0.25079788918948E+01 0.32910273005623E+03 0.40418735577660E+03
 0.31059891041656E+03 0.31059891041656E+03 0.29915000425126E+03 0.29915000418757E+03 0.31059615243301E+03
 0.31059615243301E+03 0.29915000425254E+03 0.29915000418883E+03 0.31059891041656E+03 0.31059891041656E+03
 0.29915000425126E+03 0.29915000418757E+03 0.31059615243301E+03 0.31059615243301E+03 0.29915000425254E+03
 0.29915000418883E+03 0.30889326097794E+03 0.29915002672329E+03 0.42524472548125E+02 0.52228525912508E+02
 0.18460718900135E+03 0.43481733043531E+03 0.24928710548896E+03 0.14707757911164E+03 0.17161724968269E+03
 0.14707757911164E+03 0.34637300557738E+03 0.14709088758087E+03 0.17146811263710E+03 0.14709088758087E+03
 0.34624517679512E+03 0.14707757911164E+03 0.17161724968269E+03 0.14707757911164E+03 0.34637300557738E+03
 0.14709088758087E+03 0.17146811263710E+03 0.14709088758087E+03 0.34624517679512E+03 0.22078756774282E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36284960171028E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19708604694739E+00 0.00000000000000E+00 0.00000000000000E+00 0.19708604694739E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20566417616281E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20566417616281E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936626354366E+00 0.20770146836927E+00 0.33786131850133E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    700.11061605
 0.11860023101583E+00 0.31623413475371E+03 0.44691784096063E+03 0.44175146837858E+03 0.43973383213903E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15432759915455E+00 0.00000000000000E+00 -.23502514831514E+02
 0.29676951793941E-02 0.10728476413917E+01 0.26956946439605E+04 0.10108854914852E+04 0.74567904065316E+01
 0.27962964024494E+01 0.34236117651421E+03 0.29915017113465E+03 0.33599040353072E+03 0.36189735111627E+03
 0.29915001963936E+03 0.29915003658392E+03 0.33231386441501E+03 0.36187249705653E+03 0.29915001598576E+03
 0.29915003647114E+03 0.33599040353072E+03 0.36189735111627E+03 0.29915001963936E+03 0.29915003658392E+03
 0.33231386441501E+03 0.36187249705653E+03 0.29915001598576E+03 0.29915003647114E+03 0.40473249220577E+03
 0.32967193145249E+03 0.19382620923573E+04 0.18056511274953E+04 0.59766288059275E+03 0.99262800683529E+03
 0.39197681183958E+03 0.11485490329681E+04 0.10999931465020E+04 0.10523769085066E+04 0.17144187205359E+04
 0.10388024315634E+04 0.10996096184184E+04 0.96472913962357E+03 0.17142570043391E+04 0.11485490329681E+04
 0.10999931465020E+04 0.10523769085066E+04 0.17144187205359E+04 0.10388024315634E+04 0.10996096184184E+04
 0.96472913962357E+03 0.17142570043391E+04 0.16836656810192E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48553063039721E+03 0.12948776555615E+01
 0.12948776555615E+01 0.26204649903772E+01 0.00000000000000E+00 0.33803161413291E+03 0.33803161413291E+03
 0.33803161413291E+03 0.33803161413291E+03 0.00000000000000E+00 0.00000000000000E+00 0.16560122478250E+00
 0.00000000000000E+00 -.15954387906491E+02 0.10000000000000E-02 0.12064225627921E+01 0.80000000000000E+04
 0.30000000000000E+04 0.66311757146562E+01 0.24866908929961E+01 0.32967244049657E+03 0.40473256407477E+03
 0.31071905514845E+03 0.31071905514845E+03 0.29915000490664E+03 0.29915000483314E+03 0.31071623290801E+03
 0.31071623290801E+03 0.29915000490811E+03 0.29915000483458E+03 0.31071905514845E+03 0.31071905514845E+03
 0.29915000490664E+03 0.29915000483314E+03 0.31071623290801E+03 0.31071623290801E+03 0.29915000490811E+03
 0.29915000483458E+03 0.30899702173951E+03 0.29915003036553E+03 0.40037923163717E+02 0.49190088950705E+02
 0.18573594460848E+03 0.43660119477102E+03 0.24993657043950E+03 0.14913046854539E+03 0.17260798646750E+03
 0.14913046854539E+03 0.34772042259851E+03 0.14914414037588E+03 0.17245709973270E+03 0.14914414037588E+03
 0.34759134549400E+03 0.14913046854539E+03 0.17260798646750E+03 0.14913046854539E+03 0.34772042259851E+03
 0.14914414037588E+03 0.17245709973270E+03 0.14914414037588E+03 0.34759134549401E+03 0.22089054562023E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36304932236191E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19700522132130E+00 0.00000000000000E+00 0.00000000000000E+00 0.19700522132130E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20553946550024E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20553946550024E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936632828066E+00 0.20759692657380E+00 0.33803161413291E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    710.85278639
 0.11846285798383E+00 0.31643760971754E+03 0.44729062260450E+03 0.44212354864371E+03 0.44010421069491E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15400382951249E+00 0.00000000000000E+00 -.23507064223579E+02
 0.29711363087091E-02 0.10765442167063E+01 0.26925725274031E+04 0.10097146977762E+04 0.74311857105844E+01
 0.27866946414692E+01 0.34279472152054E+03 0.29915019718656E+03 0.33636626627100E+03 0.36245139025338E+03
 0.29915002304938E+03 0.29915004295550E+03 0.33267043957232E+03 0.36242669679026E+03 0.29915001878695E+03
 0.29915004282503E+03 0.33636626627100E+03 0.36245139025338E+03 0.29915002304938E+03 0.29915004295550E+03
 0.33267043957232E+03 0.36242669679026E+03 0.29915001878695E+03 0.29915004282503E+03 0.40536349881459E+03
 0.33033203566655E+03 0.19419852822199E+04 0.18078913454870E+04 0.59634044977082E+03 0.98832203824409E+03
 0.38899988622442E+03 0.11512208553402E+04 0.11018764210928E+04 0.10539893105481E+04 0.17144326763343E+04
 0.10416995572401E+04 0.11014972143362E+04 0.96673507293005E+03 0.17142736139257E+04 0.11512208553402E+04
 0.11018764210928E+04 0.10539893105481E+04 0.17144326763343E+04 0.10416995572401E+04 0.11014972143363E+04
 0.96673507293005E+03 0.17142736139257E+04 0.16832396377187E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48590127171620E+03 0.12948776890833E+01
 0.12948776890833E+01 0.26634336717308E+01 0.00000000000000E+00 0.33823238618628E+03 0.33823238618628E+03
 0.33823238618628E+03 0.33823238618628E+03 0.00000000000000E+00 0.00000000000000E+00 0.16494699486745E+00
 0.00000000000000E+00 -.15937033880545E+02 0.10000000000000E-02 0.12180022182167E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65681325373223E+01 0.24630497014959E+01 0.33033263947096E+03 0.40536334185057E+03
 0.31085927598841E+03 0.31085927598841E+03 0.29915000577966E+03 0.29915000569308E+03 0.31085637876162E+03
 0.31085637876162E+03 0.29915000578138E+03 0.29915000569477E+03 0.31085927598841E+03 0.31085927598841E+03
 0.29915000577966E+03 0.29915000569308E+03 0.31085637876162E+03 0.31085637876162E+03 0.29915000578138E+03
 0.29915000569477E+03 0.30911816993368E+03 0.29915003513447E+03 0.37139638412770E+02 0.45665331873263E+02
 0.18704676338207E+03 0.43869565664546E+03 0.25071365944648E+03 0.15152005581759E+03 0.17375834691269E+03
 0.15152005581759E+03 0.34930174591999E+03 0.15153415300613E+03 0.17360547765702E+03 0.15153415300613E+03
 0.34917127191490E+03 0.15152005581759E+03 0.17375834691269E+03 0.15152005581759E+03 0.34930174591999E+03
 0.15153415300613E+03 0.17360547765702E+03 0.15153415300613E+03 0.34917127191490E+03 0.22102150413988E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36328356167005E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19690060859010E+00 0.00000000000000E+00 0.00000000000000E+00 0.19690060859010E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20539148114115E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20539148114115E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936644865616E+00 0.20747385951260E+00 0.33823238618628E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    720.06036096
 0.11835486591405E+00 0.31661053613156E+03 0.44760821174793E+03 0.44244014097039E+03 0.44041919772011E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15373471115827E+00 0.00000000000000E+00 -.23511357952281E+02
 0.29738470486007E-02 0.10795935330281E+01 0.26901181766440E+04 0.10087943162415E+04 0.74101962963420E+01
 0.27788236111283E+01 0.34316395867699E+03 0.29915022208401E+03 0.33668652240240E+03 0.36292274647721E+03
 0.29915002636452E+03 0.29915004915077E+03 0.33297441722967E+03 0.36289818691959E+03 0.29915002151374E+03
 0.29915004900335E+03 0.33668652240240E+03 0.36292274647721E+03 0.29915002636452E+03 0.29915004915077E+03
 0.33297441722967E+03 0.36289818691959E+03 0.29915002151374E+03 0.29915004900335E+03 0.40589981187508E+03
 0.33089405399806E+03 0.19451355906244E+04 0.18097738096650E+04 0.59519529480529E+03 0.98465355027207E+03
 0.38648227899276E+03 0.11534877290342E+04 0.11034640933571E+04 0.10553501789071E+04 0.17144361639318E+04
 0.10441579847212E+04 0.11030884114680E+04 0.96842993776715E+03 0.17142792348593E+04 0.11534877290342E+04
 0.11034640933571E+04 0.10553501789071E+04 0.17144361639318E+04 0.10441579847212E+04 0.11030884114680E+04
 0.96842993776715E+03 0.17142792348593E+04 0.16828638862766E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48621655746528E+03 0.12948777207213E+01
 0.12948777207213E+01 0.27002639700339E+01 0.00000000000000E+00 0.33840626785947E+03 0.33840626785947E+03
 0.33840626785947E+03 0.33840626785947E+03 0.00000000000000E+00 0.00000000000000E+00 0.16440156800559E+00
 0.00000000000000E+00 -.15922861015656E+02 0.10000000000000E-02 0.12276213758261E+01 0.80000000000000E+04
 0.30000000000000E+04 0.65166672375809E+01 0.24437502140928E+01 0.33089474606266E+03 0.40589946316198E+03
 0.31097950907940E+03 0.31097950907940E+03 0.29915000663124E+03 0.29915000653190E+03 0.31097654762369E+03
 0.31097654762369E+03 0.29915000663320E+03 0.29915000653382E+03 0.31097950907940E+03 0.31097950907940E+03
 0.29915000663124E+03 0.29915000653190E+03 0.31097654762369E+03 0.31097654762369E+03 0.29915000663320E+03
 0.29915000653382E+03 0.30922208952095E+03 0.29915003970868E+03 0.34658773273600E+02 0.42662302409229E+02
 0.18816638849098E+03 0.44050419981544E+03 0.25139697938201E+03 0.15356417193220E+03 0.17474084168646E+03
 0.15356417193220E+03 0.35066696208655E+03 0.15357863455099E+03 0.17458631799423E+03 0.15357863455099E+03
 0.35053533579836E+03 0.15356417193220E+03 0.17474084168646E+03 0.15356417193220E+03 0.35066696208655E+03
 0.15357863455099E+03 0.17458631799423E+03 0.15357863455099E+03 0.35053533579837E+03 0.22114425400530E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36348579987758E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19681497117559E+00 0.00000000000000E+00 0.00000000000000E+00 0.19681497117559E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20527080533858E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20527080533858E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936652469109E+00 0.20736736231864E+00 0.33840626785947E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    733.30674338
 0.11821412090249E+00 0.31685469778502E+03 0.44806118207518E+03 0.44289102900949E+03 0.44086749845727E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15336079746342E+00 0.00000000000000E+00 -.23523099163112E+02
 0.29773873340538E-02 0.10837947751660E+01 0.26869194708059E+04 0.10075948015522E+04 0.73814712741852E+01
 0.27680517278195E+01 0.34369071281476E+03 0.29915026279475E+03 0.33714355375826E+03 0.36359520582555E+03
 0.29915003189724E+03 0.29915005949159E+03 0.33340829342391E+03 0.36357083210094E+03 0.29915002607168E+03
 0.29915005931639E+03 0.33714355375826E+03 0.36359520582555E+03 0.29915003189724E+03 0.29915005949159E+03
 0.33340829342391E+03 0.36357083210094E+03 0.29915002607168E+03 0.29915005931639E+03 0.40666770436622E+03
 0.33169946332762E+03 0.19495970133039E+04 0.18124050935842E+04 0.59346753195019E+03 0.97930120292547E+03
 0.38286633331553E+03 0.11567083734164E+04 0.11056938390741E+04 0.10572603788787E+04 0.17144087760823E+04
 0.10476526653279E+04 0.11053229412912E+04 0.97081774138567E+03 0.17142546962718E+04 0.11567083734164E+04
 0.11056938390741E+04 0.10572603788787E+04 0.17144087760823E+04 0.10476526653279E+04 0.11053229412912E+04
 0.97081774138568E+03 0.17142546962718E+04 0.16822649115624E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48666543495580E+03 0.12948778072353E+01
 0.12948778072353E+01 0.27532494997128E+01 0.00000000000000E+00 0.33865864541621E+03 0.33865864541621E+03
 0.33865864541621E+03 0.33865864541621E+03 0.00000000000000E+00 0.00000000000000E+00 0.16364103965249E+00
 0.00000000000000E+00 -.15908968971558E+02 0.10000000000000E-02 0.12409801939387E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64465170669718E+01 0.24174439001144E+01 0.33170027833883E+03 0.40666710615796E+03
 0.31115189982855E+03 0.31115189982855E+03 0.29915000813741E+03 0.29915000793743E+03 0.31114884640892E+03
 0.31114884640892E+03 0.29915000813980E+03 0.29915000793976E+03 0.31115189982855E+03 0.31115189982855E+03
 0.29915000813741E+03 0.29915000793743E+03 0.31114884640892E+03 0.31114884640892E+03 0.29915000813980E+03
 0.29915000793976E+03 0.30937111848096E+03 0.29915004722039E+03 0.31069888304527E+02 0.38341378105950E+02
 0.18977283385226E+03 0.44313014923398E+03 0.25240845121246E+03 0.15650990724256E+03 0.17615022571173E+03
 0.15650990724256E+03 0.35264951084879E+03 0.15652489429513E+03 0.17599334102863E+03 0.15652489429513E+03
 0.35251624363734E+03 0.15650990724256E+03 0.17615022571173E+03 0.15650990724256E+03 0.35264951084879E+03
 0.15652489429513E+03 0.17599334102863E+03 0.15652489429513E+03 0.35251624363734E+03 0.22133202775085E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36377905379109E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19673546161949E+00 0.00000000000000E+00 0.00000000000000E+00 0.19673546161949E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20512097367100E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20512097367100E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936641238858E+00 0.20721273826879E+00 0.33865864541621E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    740.09153731
 0.11815119051217E+00 0.31697900012671E+03 0.44829173957600E+03 0.44312015407755E+03 0.44109517013638E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15317523179493E+00 0.00000000000000E+00 -.23528651349823E+02
 0.29789729861867E-02 0.10858627046630E+01 0.26854892733487E+04 0.10070584775058E+04 0.73674139148951E+01
 0.27627802180857E+01 0.34395886616619E+03 0.29915028598941E+03 0.33737634097023E+03 0.36393698340107E+03
 0.29915003510339E+03 0.29915006548437E+03 0.33362944152007E+03 0.36391270242701E+03 0.29915002871638E+03
 0.29915006529332E+03 0.33737634097023E+03 0.36393698340107E+03 0.29915003510339E+03 0.29915006548437E+03
 0.33362944152007E+03 0.36391270242701E+03 0.29915002871638E+03 0.29915006529332E+03 0.40705644791140E+03
 0.33210844624848E+03 0.19518502008984E+04 0.18137250221525E+04 0.59259166782971E+03 0.97660681636077E+03
 0.38105219019191E+03 0.11583400249024E+04 0.11068172754373E+04 0.10582241949974E+04 0.17143896818919E+04
 0.10494231987405E+04 0.11064487113155E+04 0.97202304407946E+03 0.17142369694760E+04 0.11583400249024E+04
 0.11068172754373E+04 0.10582241949974E+04 0.17143896818919E+04 0.10494231987405E+04 0.11064487113155E+04
 0.97202304407946E+03 0.17142369694760E+04 0.16819581146726E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48689346521961E+03 0.12948778481462E+01
 0.12948778481462E+01 0.27803886754314E+01 0.00000000000000E+00 0.33878908825986E+03 0.33878908825986E+03
 0.33878908825986E+03 0.33878908825985E+03 0.00000000000000E+00 0.00000000000000E+00 0.16326221954192E+00
 0.00000000000000E+00 -.15901770732157E+02 0.10000000000000E-02 0.12476089729101E+01 0.80000000000000E+04
 0.30000000000000E+04 0.64122655204538E+01 0.24045995701702E+01 0.33210932932404E+03 0.40705572267792E+03
 0.31124036494733E+03 0.31124036494733E+03 0.29915000897518E+03 0.29915000875462E+03 0.31123726439434E+03
 0.31123726439434E+03 0.29915000897780E+03 0.29915000875717E+03 0.31124036494733E+03 0.31124036494733E+03
 0.29915000897518E+03 0.29915000875462E+03 0.31123726439434E+03 0.31123726439434E+03 0.29915000897780E+03
 0.29915000875717E+03 0.30944763119946E+03 0.29915005151531E+03 0.29241002207865E+02 0.36149318325505E+02
 0.19059249377901E+03 0.44448238054682E+03 0.25293692429892E+03 0.15801257208175E+03 0.17686928463381E+03
 0.15801257208175E+03 0.35367024199521E+03 0.15802782885076E+03 0.17671123050803E+03 0.15802782885076E+03
 0.35353617480936E+03 0.15801257208175E+03 0.17686928463381E+03 0.15801257208175E+03 0.35367024199521E+03
 0.15802782885076E+03 0.17671123050803E+03 0.15802782885076E+03 0.35353617480936E+03 0.22143659226562E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36393045435200E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19669214100804E+00 0.00000000000000E+00 0.00000000000000E+00 0.19669214100804E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20505385004036E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20505385004036E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936635528506E+00 0.20713291122687E+00 0.33878908825986E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    750.26872820
 0.11806103418129E+00 0.31716578952569E+03 0.44863599062460E+03 0.44346215465601E+03 0.44143498633747E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15290428087458E+00 0.00000000000000E+00 -.23534651970477E+02
 0.29812475833658E-02 0.10888599496933E+01 0.26834403303627E+04 0.10062901238860E+04 0.73471340389120E+01
 0.27551752645920E+01 0.34435903681194E+03 0.29915032394637E+03 0.33772389043545E+03 0.36444643463485E+03
 0.29915004042447E+03 0.29915007543054E+03 0.33395977518106E+03 0.36442228969042E+03 0.29915003311040E+03
 0.29915007521352E+03 0.33772389043545E+03 0.36444643463485E+03 0.29915004042447E+03 0.29915007543054E+03
 0.33395977518106E+03 0.36442228969042E+03 0.29915003311040E+03 0.29915007521352E+03 0.40763488460615E+03
 0.33271809053793E+03 0.19551990821889E+04 0.18156882507414E+04 0.59127990177565E+03 0.97260599840954E+03
 0.37836969712501E+03 0.11607692779908E+04 0.11084828874209E+04 0.10596630343634E+04 0.17143592018928E+04
 0.10520593521095E+04 0.11081176785711E+04 0.97382055901039E+03 0.17142084266743E+04 0.11607692779908E+04
 0.11084828874209E+04 0.10596630343634E+04 0.17143592018928E+04 0.10520593521095E+04 0.11081176785711E+04
 0.97382055901040E+03 0.17142084266743E+04 0.16815017854978E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48723381765919E+03 0.12948778923612E+01
 0.12948778923612E+01 0.28210974390092E+01 0.00000000000000E+00 0.33898577340965E+03 0.33898577340965E+03
 0.33898577340965E+03 0.33898577340965E+03 0.00000000000000E+00 0.00000000000000E+00 0.16270728509855E+00
 0.00000000000000E+00 -.15888419842441E+02 0.10000000000000E-02 0.12572890531307E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63628964080135E+01 0.23860861530051E+01 0.33271908291229E+03 0.40763396830328E+03
 0.31137316225008E+03 0.31137316225008E+03 0.29915001036940E+03 0.29915001011457E+03 0.31136999111406E+03
 0.31136999111406E+03 0.29915001037241E+03 0.29915001011750E+03 0.31137316225008E+03 0.31137316225008E+03
 0.29915001036940E+03 0.29915001011457E+03 0.31136999111406E+03 0.31136999111406E+03 0.29915001037241E+03
 0.29915001011750E+03 0.30956252924333E+03 0.29915005856427E+03 0.26502072054763E+02 0.32879078842587E+02
 0.19181639551625E+03 0.44651297061933E+03 0.25373749312550E+03 0.16025996105515E+03 0.17794253554135E+03
 0.16025996105515E+03 0.35520192532074E+03 0.16027562280144E+03 0.17778276128881E+03 0.16027562280144E+03
 0.35506669160396E+03 0.16025996105516E+03 0.17794253554135E+03 0.16025996105516E+03 0.35520192532074E+03
 0.16027562280144E+03 0.17778276128881E+03 0.16027562280144E+03 0.35506669160396E+03 0.22159844142107E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36415759968488E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19661051653296E+00 0.00000000000000E+00 0.00000000000000E+00 0.19661051653296E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20494205466611E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20494205466611E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936635313965E+00 0.20701275435010E+00 0.33898577340965E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    760.44591910
 0.11796918457594E+00 0.31735273890317E+03 0.44897838833304E+03 0.44380246482220E+03 0.44177320607532E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15264193533982E+00 0.00000000000000E+00 -.23538190816722E+02
 0.29835684828069E-02 0.10917356204942E+01 0.26813528987522E+04 0.10055073370321E+04 0.73277814242046E+01
 0.27479180340767E+01 0.34475679180046E+03 0.29915036598197E+03 0.33806952419132E+03 0.36495213974397E+03
 0.29915004641531E+03 0.29915008662857E+03 0.33428846836970E+03 0.36492812717385E+03 0.29915003806376E+03
 0.29915008638274E+03 0.33806952419132E+03 0.36495213974397E+03 0.29915004641531E+03 0.29915008662857E+03
 0.33428846836970E+03 0.36492812717385E+03 0.29915003806376E+03 0.29915008638274E+03 0.40820814904880E+03
 0.33332337395208E+03 0.19585167107618E+04 0.18176353399354E+04 0.58996699281812E+03 0.96864621263217E+03
 0.37572938484996E+03 0.11631792118325E+04 0.11101261160960E+04 0.10610933970577E+04 0.17143266737629E+04
 0.10546747761572E+04 0.11097640976484E+04 0.97560603643575E+03 0.17141777076749E+04 0.11631792118325E+04
 0.11101261160960E+04 0.10610933970577E+04 0.17143266737629E+04 0.10546747761573E+04 0.11097640976484E+04
 0.97560603643577E+03 0.17141777076749E+04 0.16810501615494E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48757252411270E+03 0.12948779184369E+01
 0.12948779184369E+01 0.28618062025871E+01 0.00000000000000E+00 0.33918313106389E+03 0.33918313106389E+03
 0.33918313106389E+03 0.33918313106389E+03 0.00000000000000E+00 0.00000000000000E+00 0.16216798586862E+00
 0.00000000000000E+00 -.15872359042950E+02 0.10000000000000E-02 0.12666619201810E+01 0.80000000000000E+04
 0.30000000000000E+04 0.63158131404604E+01 0.23684299276727E+01 0.33332447522906E+03 0.40820705226776E+03
 0.31150600579123E+03 0.31150600579123E+03 0.29915001194413E+03 0.29915001165059E+03 0.31150276424877E+03
 0.31150276424877E+03 0.29915001194757E+03 0.29915001165395E+03 0.31150600579123E+03 0.31150600579123E+03
 0.29915001194413E+03 0.29915001165059E+03 0.31150276424878E+03 0.31150276424878E+03 0.29915001194757E+03
 0.29915001165395E+03 0.30967751779891E+03 0.29915006639719E+03 0.23763997205981E+02 0.29624899182030E+02
 0.19303111088239E+03 0.44853905743083E+03 0.25454279099403E+03 0.16249872341732E+03 0.17900685763112E+03
 0.16249872341732E+03 0.35672835060607E+03 0.16251479040302E+03 0.17884539749818E+03 0.16251479040302E+03
 0.35659198353057E+03 0.16249872341732E+03 0.17900685763112E+03 0.16249872341732E+03 0.35672835060607E+03
 0.16251479040301E+03 0.17884539749818E+03 0.16251479040301E+03 0.35659198353057E+03 0.22176149071985E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36438440319403E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19651120469678E+00 0.00000000000000E+00 0.00000000000000E+00 0.19651120469678E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20481844481630E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20481844481630E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936644062108E+00 0.20689242516281E+00 0.33918313106389E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    770.62310999
 0.11787061699730E+00 0.31753970094093E+03 0.44931906088471E+03 0.44414142273001E+03 0.44211023994008E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15238782316837E+00 0.00000000000000E+00 -.23539440685913E+02
 0.29860631794983E-02 0.10944949667344E+01 0.26791127712656E+04 0.10046672892246E+04 0.73093072541662E+01
 0.27409902203123E+01 0.34515219981183E+03 0.29915041242767E+03 0.33841329150195E+03 0.36545419655993E+03
 0.29915005314260E+03 0.29915009920264E+03 0.33461556169840E+03 0.36543031280681E+03 0.29915004363296E+03
 0.29915009892494E+03 0.33841329150195E+03 0.36545419655993E+03 0.29915005314260E+03 0.29915009920264E+03
 0.33461556169839E+03 0.36543031280681E+03 0.29915004363296E+03 0.29915009892494E+03 0.40877649580674E+03
 0.33392437978992E+03 0.19618091522669E+04 0.18195709705201E+04 0.58865343752506E+03 0.96472649641725E+03
 0.37312979170457E+03 0.11655727476091E+04 0.11117496279458E+04 0.10625162379233E+04 0.17142946950135E+04
 0.10572725759859E+04 0.11113906461978E+04 0.97738089369366E+03 0.17141474200896E+04 0.11655727476091E+04
 0.11117496279458E+04 0.10625162379233E+04 0.17142946950135E+04 0.10572725759859E+04 0.11113906461978E+04
 0.97738089369368E+03 0.17141474200896E+04 0.16806063496214E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48790996014865E+03 0.12948779276465E+01
 0.12948779276465E+01 0.29025149661650E+01 0.00000000000000E+00 0.33938069033905E+03 0.33938069033905E+03
 0.33938069033905E+03 0.33938069033905E+03 0.00000000000000E+00 0.00000000000000E+00 0.16164396711890E+00
 0.00000000000000E+00 -.15853742867337E+02 0.10000000000000E-02 0.12757365165950E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62708873626606E+01 0.23515827609977E+01 0.33392558687345E+03 0.40877523109400E+03
 0.31163883855584E+03 0.31163883855584E+03 0.29915001371796E+03 0.29915001338084E+03 0.31163552680225E+03
 0.31163552680225E+03 0.29915001372189E+03 0.29915001338467E+03 0.31163883855584E+03 0.31163883855584E+03
 0.29915001371796E+03 0.29915001338084E+03 0.31163552680225E+03 0.31163552680225E+03 0.29915001372189E+03
 0.29915001338467E+03 0.30979254862874E+03 0.29915007508055E+03 0.21024047649851E+02 0.26383480508527E+02
 0.19423456253289E+03 0.45055439042897E+03 0.25534865508342E+03 0.16472787307954E+03 0.18006015598989E+03
 0.16472787307954E+03 0.35824435942391E+03 0.16474434546182E+03 0.17989703929214E+03 0.16474434546182E+03
 0.35810688701330E+03 0.16472787307954E+03 0.18006015598989E+03 0.16472787307954E+03 0.35824435942392E+03
 0.16474434546182E+03 0.17989703929214E+03 0.16474434546182E+03 0.35810688701330E+03 0.22192190016290E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36461048257633E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19639531049615E+00 0.00000000000000E+00 0.00000000000000E+00 0.19639531049615E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20468304755633E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20468304755633E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936661363605E+00 0.20677220701182E+00 0.33938069033905E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    785.63736044
 0.11772915319919E+00 0.31780665565331E+03 0.44981653902424E+03 0.44463606842985E+03 0.44260182003417E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15202728626976E+00 0.00000000000000E+00 -.23552262088054E+02
 0.29896508530130E-02 0.10983682582444E+01 0.26758977530562E+04 0.10034616573961E+04 0.72835316752386E+01
 0.27313243782145E+01 0.34572953354239E+03 0.29915049137032E+03 0.33891529045200E+03 0.36618811153709E+03
 0.29915006484901E+03 0.29915012108027E+03 0.33509310415875E+03 0.36616440954556E+03 0.29915005334179E+03
 0.29915012074836E+03 0.33891529045200E+03 0.36618811153709E+03 0.29915006484901E+03 0.29915012108027E+03
 0.33509310415875E+03 0.36616440954556E+03 0.29915005334179E+03 0.29915012074836E+03 0.40961506637871E+03
 0.33481051010412E+03 0.19665971599252E+04 0.18223310198215E+04 0.58655691340668E+03 0.95873828441660E+03
 0.36924858644289E+03 0.11690620762922E+04 0.11140803637761E+04 0.10645463422527E+04 0.17142050315795E+04
 0.10610627986024E+04 0.11137255627222E+04 0.97993060375248E+03 0.17140600323632E+04 0.11690620762922E+04
 0.11140803637761E+04 0.10645463422527E+04 0.17142050315795E+04 0.10610627986024E+04 0.11137255627222E+04
 0.97993060375249E+03 0.17140600323632E+04 0.16798525251919E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48840227074677E+03 0.12948780221199E+01
 0.12948780221199E+01 0.29625719679427E+01 0.00000000000000E+00 0.33967250279904E+03 0.33967250279904E+03
 0.33967250279904E+03 0.33967250279904E+03 0.00000000000000E+00 0.00000000000000E+00 0.16089801267927E+00
 0.00000000000000E+00 -.15838644469851E+02 0.10000000000000E-02 0.12885950713700E+01 0.80000000000000E+04
 0.30000000000000E+04 0.62083118100824E+01 0.23281169287809E+01 0.33481184207967E+03 0.40961360572254E+03
 0.31183306125900E+03 0.31183306125900E+03 0.29915001705555E+03 0.29915001640539E+03 0.31182964695154E+03
 0.31182964695154E+03 0.29915001706038E+03 0.29915001641003E+03 0.31183306125900E+03 0.31183306125900E+03
 0.29915001705555E+03 0.29915001640539E+03 0.31182964695154E+03 0.31182964695154E+03 0.29915001706038E+03
 0.29915001641003E+03 0.30996073897538E+03 0.29915008990973E+03 0.16923095575588E+02 0.21561130053291E+02
 0.19600309580348E+03 0.45354242132001E+03 0.25655931003751E+03 0.16803388486565E+03 0.18160692120426E+03
 0.16803388486565E+03 0.36049245599503E+03 0.16805095025398E+03 0.18144130228810E+03 0.16805095025398E+03
 0.36035328793025E+03 0.16803388486565E+03 0.18160692120427E+03 0.16803388486565E+03 0.36049245599503E+03
 0.16805095025398E+03 0.18144130228810E+03 0.16805095025398E+03 0.36035328793024E+03 0.22215588896033E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36494519499825E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19630897849598E+00 0.00000000000000E+00 0.00000000000000E+00 0.19630897849598E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20452132499533E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20452132499533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936645543213E+00 0.20659443598313E+00 0.33967250279904E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    793.14448566
 0.11766478568356E+00 0.31793991029064E+03 0.45006387548404E+03 0.44488176280132E+03 0.44284591585468E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15185323600072E+00 0.00000000000000E+00 -.23556705673019E+02
 0.29912861220718E-02 0.11002175510156E+01 0.26744348997478E+04 0.10029130874054E+04 0.72712892033174E+01
 0.27267334512440E+01 0.34601663857302E+03 0.29915053522798E+03 0.33916511720613E+03 0.36655212109563E+03
 0.29915007146734E+03 0.29915013344715E+03 0.33533101272747E+03 0.36652850774913E+03 0.29915005883818E+03
 0.29915013308509E+03 0.33916511720613E+03 0.36655212109563E+03 0.29915007146734E+03 0.29915013344715E+03
 0.33533101272747E+03 0.36652850774913E+03 0.29915005883818E+03 0.29915013308509E+03 0.41002735251169E+03
 0.33524788449959E+03 0.19689609707266E+04 0.18236880598334E+04 0.58555167065695E+03 0.95585255818181E+03
 0.36737312917158E+03 0.11707894317435E+04 0.11152301987115E+04 0.10655495514303E+04 0.17141612927220E+04
 0.10629385949699E+04 0.11148773867710E+04 0.98118933334467E+03 0.17140173532480E+04 0.11707894317435E+04
 0.11152301987115E+04 0.10655495514303E+04 0.17141612927220E+04 0.10629385949699E+04 0.11148773867710E+04
 0.98118933334468E+03 0.17140173532480E+04 0.16794954829470E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48864676274508E+03 0.12948780548621E+01
 0.12948780548621E+01 0.29926004688315E+01 0.00000000000000E+00 0.33981903260890E+03 0.33981903260890E+03
 0.33981903260890E+03 0.33981903260890E+03 0.00000000000000E+00 0.00000000000000E+00 0.16053674567172E+00
 0.00000000000000E+00 -.15829482756299E+02 0.10000000000000E-02 0.12947943816726E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61785872052255E+01 0.23169702019596E+01 0.33524928795645E+03 0.41002578555259E+03
 0.31193057495033E+03 0.31193057495033E+03 0.29915001883924E+03 0.29915001812108E+03 0.31192710919264E+03
 0.31192710919264E+03 0.29915001884455E+03 0.29915001812619E+03 0.31193057495033E+03 0.31193057495033E+03
 0.29915001883924E+03 0.29915001812108E+03 0.31192710919264E+03 0.31192710919264E+03 0.29915001884455E+03
 0.29915001812619E+03 0.31004523957668E+03 0.29915009817720E+03 0.14893601058083E+02 0.19186540266598E+02
 0.19687993569623E+03 0.45503155688330E+03 0.25716722150859E+03 0.16967159788543E+03 0.18237324761953E+03
 0.16967159788543E+03 0.36161160359941E+03 0.16968896221123E+03 0.18220643007618E+03 0.16968896221123E+03
 0.36147164188215E+03 0.16967159788543E+03 0.18237324761953E+03 0.16967159788543E+03 0.36161160359941E+03
 0.16968896221123E+03 0.18220643007618E+03 0.16968896221123E+03 0.36147164188215E+03 0.22227902898973E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36511325990347E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19625183492042E+00 0.00000000000000E+00 0.00000000000000E+00 0.19625183492042E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20444951011146E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20444951011146E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936642927600E+00 0.20650534414643E+00 0.33981903260890E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    800.65161088
 0.11760002942314E+00 0.31807434905360E+03 0.45031052444635E+03 0.44512686507402E+03 0.44308949260905E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15168320925658E+00 0.00000000000000E+00 -.23558631273676E+02
 0.29929330753030E-02 0.11020098757224E+01 0.26729632099074E+04 0.10023612037153E+04 0.72594630740088E+01
 0.27222986527533E+01 0.34630255314950E+03 0.29915058222317E+03 0.33941400281522E+03 0.36691418798968E+03
 0.29915007864162E+03 0.29915014685120E+03 0.33556813435101E+03 0.36689066157602E+03 0.29915006480163E+03
 0.29915014645683E+03 0.33941400281522E+03 0.36691418798967E+03 0.29915007864162E+03 0.29915014685120E+03
 0.33556813435101E+03 0.36689066157602E+03 0.29915006480163E+03 0.29915014645683E+03 0.41043658275496E+03
 0.33568262320583E+03 0.19713137566121E+04 0.18250494620576E+04 0.58455778474947E+03 0.95300683777829E+03
 0.36552626410507E+03 0.11725097452047E+04 0.11163746286389E+04 0.10665582362062E+04 0.17141249119812E+04
 0.10648064691060E+04 0.11160237352419E+04 0.98245106483382E+03 0.17139819790591E+04 0.11725097452047E+04
 0.11163746286389E+04 0.10665582362062E+04 0.17141249119813E+04 0.10648064691060E+04 0.11160237352419E+04
 0.98245106483382E+03 0.17139819790591E+04 0.16791534474702E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48889069425627E+03 0.12948780690507E+01
 0.12948780690507E+01 0.30226289697203E+01 0.00000000000000E+00 0.33996572806973E+03 0.33996572806973E+03
 0.33996572806973E+03 0.33996572806973E+03 0.00000000000000E+00 0.00000000000000E+00 0.16018308468589E+00
 0.00000000000000E+00 -.15817398995706E+02 0.10000000000000E-02 0.13008453706121E+01 0.80000000000000E+04
 0.30000000000000E+04 0.61498470000596E+01 0.23061926250223E+01 0.33568410044717E+03 0.41043490870360E+03
 0.31202814695949E+03 0.31202814695949E+03 0.29915002077706E+03 0.29915001998503E+03 0.31202462984587E+03
 0.31202462984587E+03 0.29915002078287E+03 0.29915001999062E+03 0.31202814695949E+03 0.31202814695949E+03
 0.29915002077706E+03 0.29915001998503E+03 0.31202462984587E+03 0.31202462984587E+03 0.29915002078287E+03
 0.29915001999062E+03 0.31012982291526E+03 0.29915010705655E+03 0.12867735416982E+02 0.16824370827732E+02
 0.19775192973431E+03 0.45651542851460E+03 0.25777473913162E+03 0.17130321624309E+03 0.18313482562610E+03
 0.17130321624309E+03 0.36272560841002E+03 0.17132087969174E+03 0.18296682417871E+03 0.17132087969174E+03
 0.36258486706992E+03 0.17130321624309E+03 0.18313482562610E+03 0.17130321624309E+03 0.36272560841003E+03
 0.17132087969173E+03 0.18296682417870E+03 0.17132087969173E+03 0.36258486706991E+03 0.22240234540435E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36528059815246E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19617607362167E+00 0.00000000000000E+00 0.00000000000000E+00 0.19617607362167E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20436245108840E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20436245108840E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936650076433E+00 0.20641633558431E+00 0.33996572806973E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    815.66586133
 0.11745504536630E+00 0.31834532221529E+03 0.45080170806618E+03 0.44561581779646E+03 0.44357579569291E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15135468524938E+00 0.00000000000000E+00 -.23557896477840E+02
 0.29966271024056E-02 0.11054317344783E+01 0.26696681724523E+04 0.10011255646696E+04 0.72369914400686E+01
 0.27138717900257E+01 0.34687096975481E+03 0.29915068629881E+03 0.33990909239863E+03 0.36763280175903E+03
 0.29915009480543E+03 0.29915017704408E+03 0.33604011771011E+03 0.36760944419974E+03 0.29915007825532E+03
 0.29915017657816E+03 0.33990909239863E+03 0.36763280175903E+03 0.29915009480543E+03 0.29915017704408E+03
 0.33604011771011E+03 0.36760944419974E+03 0.29915007825532E+03 0.29915017657816E+03 0.41124715944207E+03
 0.33654477024171E+03 0.19759955874720E+04 0.18277834852370E+04 0.58258825286747E+03 0.94740510481668E+03
 0.36190391068487E+03 0.11759329423551E+04 0.11186458486244E+04 0.10685833059244E+04 0.17140689837840E+04
 0.10685228843688E+04 0.11182985946163E+04 0.98497683344582E+03 0.17139279167237E+04 0.11759329423551E+04
 0.11186458486244E+04 0.10685833059244E+04 0.17140689837840E+04 0.10685228843688E+04 0.11182985946163E+04
 0.98497683344582E+03 0.17139279167237E+04 0.16785054112352E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48937749762996E+03 0.12948780636364E+01
 0.12948780636364E+01 0.30826859714980E+01 0.00000000000000E+00 0.34025852775206E+03 0.34025852775206E+03
 0.34025852775206E+03 0.34025852775206E+03 0.00000000000000E+00 0.00000000000000E+00 0.15949808626299E+00
 0.00000000000000E+00 -.15787869160885E+02 0.10000000000000E-02 0.13125165678236E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60951611553867E+01 0.22856854332700E+01 0.33654638959902E+03 0.41124528563785E+03
 0.31222329488453E+03 0.31222329488453E+03 0.29915002515735E+03 0.29915002419834E+03 0.31221967536896E+03
 0.31221967536896E+03 0.29915002516430E+03 0.29915002420503E+03 0.31222329488453E+03 0.31222329488453E+03
 0.29915002515735E+03 0.29915002419834E+03 0.31221967536896E+03 0.31221967536896E+03 0.29915002516430E+03
 0.29915002420503E+03 0.31029908557460E+03 0.29915012678784E+03 0.88163662390370E+01 0.12125360182849E+02
 0.19947581313030E+03 0.45945375816019E+03 0.25898056596423E+03 0.17454701695482E+03 0.18463811978265E+03
 0.17454701695482E+03 0.36492698669217E+03 0.17456527902607E+03 0.18446778398046E+03 0.17456527902607E+03
 0.36478471723517E+03 0.17454701695482E+03 0.18463811978265E+03 0.17454701695482E+03 0.36492698669217E+03
 0.17456527902607E+03 0.18446778398046E+03 0.17456527902607E+03 0.36478471723517E+03 0.22264001207514E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36561293398585E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19599060975764E+00 0.00000000000000E+00 0.00000000000000E+00 0.19599060975764E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20415894861236E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20415894861236E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936682529148E+00 0.20623910579745E+00 0.34025852775206E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    823.17298655
 0.11737232862020E+00 0.31848096653582E+03 0.45104624154646E+03 0.44585974320573E+03 0.44381860566324E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15119586010962E+00 0.00000000000000E+00 -.23556326339053E+02
 0.29987387459932E-02 0.11070660633557E+01 0.26677882528744E+04 0.10004205948279E+04 0.72263076837084E+01
 0.27098653813906E+01 0.34715353633808E+03 0.29915074372931E+03 0.34015534321489E+03 0.36798945770016E+03
 0.29915010387286E+03 0.29915019397722E+03 0.33627501668982E+03 0.36796618214122E+03 0.29915008581208E+03
 0.29915019347180E+03 0.34015534321489E+03 0.36798945770016E+03 0.29915010387286E+03 0.29915019397722E+03
 0.33627501668982E+03 0.36796618214122E+03 0.29915008581208E+03 0.29915019347180E+03 0.41164879221261E+03
 0.33697228308430E+03 0.19783267767324E+04 0.18291515434934E+04 0.58160995955642E+03 0.94464321260658E+03
 0.36012520325238E+03 0.11776368629621E+04 0.11197726426682E+04 0.10695943188034E+04 0.17140482959976E+04
 0.10703726041633E+04 0.11194271171553E+04 0.98623611891856E+03 0.17139080950067E+04 0.11776368629621E+04
 0.11197726426682E+04 0.10695943188034E+04 0.17140482959976E+04 0.10703726041633E+04 0.11194271171553E+04
 0.98623611891856E+03 0.17139080950067E+04 0.16781964588013E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48962045173560E+03 0.12948780520670E+01
 0.12948780520670E+01 0.31127144723868E+01 0.00000000000000E+00 0.34040434207851E+03 0.34040434207851E+03
 0.34040434207851E+03 0.34040434207851E+03 0.00000000000000E+00 0.00000000000000E+00 0.15916649189078E+00
 0.00000000000000E+00 -.15771683948202E+02 0.10000000000000E-02 0.13181430981488E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60691437911673E+01 0.22759289216878E+01 0.33697396916634E+03 0.41164682754946E+03
 0.31232081009856E+03 0.31232081009856E+03 0.29915002762226E+03 0.29915002656929E+03 0.31231713952642E+03
 0.31231713952642E+03 0.29915002762984E+03 0.29915002657658E+03 0.31232081009856E+03 0.31232081009856E+03
 0.29915002762226E+03 0.29915002656929E+03 0.31231713952642E+03 0.31231713952642E+03 0.29915002762984E+03
 0.29915002657658E+03 0.31038371205267E+03 0.29915013771126E+03 0.67887162751603E+01 0.97862796578775E+01
 0.20032644089107E+03 0.46090506606816E+03 0.25957699297264E+03 0.17615882760998E+03 0.18537857858848E+03
 0.17615882760998E+03 0.36601185940144E+03 0.17617738923988E+03 0.18520708917357E+03 0.17617738923988E+03
 0.36586883837573E+03 0.17615882760998E+03 0.18537857858848E+03 0.17615882760998E+03 0.36601185940144E+03
 0.17617738923988E+03 0.18520708917357E+03 0.17617738923988E+03 0.36586883837573E+03 0.22275231827159E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36577797339113E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19588903217315E+00 0.00000000000000E+00 0.00000000000000E+00 0.19588903217315E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20404856035721E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20404856035721E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936703681063E+00 0.20615101365083E+00 0.34040434207851E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    830.68011177
 0.11728312096760E+00 0.31861644995174E+03 0.45129009263817E+03 0.44610329967670E+03 0.44406117097878E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15104043444197E+00 0.00000000000000E+00 -.23554186208256E+02
 0.30010194470548E-02 0.11086525504220E+01 0.26657607993348E+04 0.99966029975055E+03 0.72159668030844E+01
 0.27059875511566E+01 0.34743503601466E+03 0.29915080499751E+03 0.34040074622622E+03 0.36834439689656E+03
 0.29915011365210E+03 0.29915021223625E+03 0.33650919868609E+03 0.36832120177074E+03 0.29915009396895E+03
 0.29915021168871E+03 0.34040074622622E+03 0.36834439689656E+03 0.29915011365210E+03 0.29915021223625E+03
 0.33650919868609E+03 0.36832120177074E+03 0.29915009396895E+03 0.29915021168871E+03 0.41204809269065E+03
 0.33739745538433E+03 0.19806521035097E+04 0.18305188726021E+04 0.58063574273485E+03 0.94190605999923E+03
 0.35836713855070E+03 0.11793361313883E+04 0.11208941071705E+04 0.10706030269383E+04 0.17140327206920E+04
 0.10722171385508E+04 0.11205502539427E+04 0.98749183309414E+03 0.17138933452616E+04 0.11793361313883E+04
 0.11208941071705E+04 0.10706030269383E+04 0.17140327206920E+04 0.10722171385508E+04 0.11205502539427E+04
 0.98749183309414E+03 0.17138933452616E+04 0.16778969821096E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48986309885412E+03 0.12948780362976E+01
 0.12948780362976E+01 0.31427429732756E+01 0.00000000000000E+00 0.34054969765058E+03 0.34054969765058E+03
 0.34054969765058E+03 0.34054969765058E+03 0.00000000000000E+00 0.00000000000000E+00 0.15884198620076E+00
 0.00000000000000E+00 -.15754822873835E+02 0.10000000000000E-02 0.13236341758701E+01 0.80000000000000E+04
 0.30000000000000E+04 0.60439660336973E+01 0.22664872626365E+01 0.33739920548006E+03 0.41204604249443E+03
 0.31241825811555E+03 0.31241825811555E+03 0.29915003028616E+03 0.29915002913164E+03 0.31241453657676E+03
 0.31241453657676E+03 0.29915003029441E+03 0.29915002913958E+03 0.31241825811555E+03 0.31241825811555E+03
 0.29915003028616E+03 0.29915002913164E+03 0.31241453657676E+03 0.31241453657676E+03 0.29915003029441E+03
 0.29915002913958E+03 0.31046831117799E+03 0.29915014938952E+03 0.47594650760997E+01 0.74540441656149E+01
 0.20116946691217E+03 0.46234407791787E+03 0.26016876367113E+03 0.17776394932761E+03 0.18611152491439E+03
 0.17776394932761E+03 0.36708593743235E+03 0.17778281071148E+03 0.18593888934556E+03 0.17778281071148E+03
 0.36694217162809E+03 0.17776394932761E+03 0.18611152491439E+03 0.17776394932761E+03 0.36708593743235E+03
 0.17778281071148E+03 0.18593888934556E+03 0.17778281071148E+03 0.36694217162808E+03 0.22286007863323E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36594226868477E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19578329487414E+00 0.00000000000000E+00 0.00000000000000E+00 0.19578329487414E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20393373182332E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20393373182332E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936727225360E+00 0.20606330063150E+00 0.34054969765058E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    845.69436222
 0.11708752193004E+00 0.31888690688037E+03 0.45177579944755E+03 0.44658925573997E+03 0.44454547889643E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15073915482097E+00 0.00000000000000E+00 -.23548235580874E+02
 0.30060323737172E-02 0.11116910071798E+01 0.26613153171426E+04 0.99799324392848E+03 0.71962442336339E+01
 0.26985915876127E+01 0.34799490195518E+03 0.29915093980820E+03 0.34088904768313E+03 0.36904925102677E+03
 0.29915013552115E+03 0.29915025305568E+03 0.33697544254318E+03 0.36902621221602E+03 0.29915011223287E+03
 0.29915025241549E+03 0.34088904768313E+03 0.36904925102677E+03 0.29915013552115E+03 0.29915025305568E+03
 0.33697544254318E+03 0.36902621221602E+03 0.29915011223287E+03 0.29915025241549E+03 0.41283992904188E+03
 0.33824085228323E+03 0.19852858244418E+04 0.18332514566528E+04 0.57869943472640E+03 0.93650357386367E+03
 0.35491064196364E+03 0.11827215100327E+04 0.11231224378479E+04 0.10726142800284E+04 0.17140176024672E+04
 0.10758914164109E+04 0.11227817728029E+04 0.98999326168346E+03 0.17138797673557E+04 0.11827215100327E+04
 0.11231224378479E+04 0.10726142800284E+04 0.17140176024672E+04 0.10758914164109E+04 0.11227817728029E+04
 0.98999326168346E+03 0.17138797673557E+04 0.16773260031390E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49034740228405E+03 0.12948779924509E+01
 0.12948779924509E+01 0.32027999750533E+01 0.00000000000000E+00 0.34083895280681E+03 0.34083895280681E+03
 0.34083895280681E+03 0.34083895280681E+03 0.00000000000000E+00 0.00000000000000E+00 0.15821368184589E+00
 0.00000000000000E+00 -.15719128364389E+02 0.10000000000000E-02 0.13342215365248E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59960057464201E+01 0.22485021549075E+01 0.33824272342380E+03 0.41283772102758E+03
 0.31261289796604E+03 0.31261289796604E+03 0.29915003626165E+03 0.29915003487934E+03 0.31260907474682E+03
 0.31260907474682E+03 0.29915003627139E+03 0.29915003488871E+03 0.31261289796604E+03 0.31261289796604E+03
 0.29915003626165E+03 0.29915003487934E+03 0.31260907474682E+03 0.31260907474682E+03 0.29915003627139E+03
 0.29915003488871E+03 0.31063738036432E+03 0.29915017516694E+03 0.69633264713919E+00 0.28112830916256E+01
 0.20283320873388E+03 0.46518542038861E+03 0.26133804561105E+03 0.18095445288354E+03 0.18755538056461E+03
 0.18095445288354E+03 0.36920205382695E+03 0.18097391443947E+03 0.18738047124054E+03 0.18097391443947E+03
 0.36905681512167E+03 0.18095445288354E+03 0.18755538056461E+03 0.18095445288354E+03 0.36920205382695E+03
 0.18097391443947E+03 0.18738047124054E+03 0.18097391443947E+03 0.36905681512167E+03 0.22306239633871E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36626858191795E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19555964660219E+00 0.00000000000000E+00 0.00000000000000E+00 0.19555964660219E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20369111310980E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20369111310980E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936781323130E+00 0.20588905493305E+00 0.34083895280681E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    853.20148744
 0.11698193281243E+00 0.31902192253264E+03 0.45201766504550E+03 0.44683163204051E+03 0.44478718692266E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15059301125766E+00 0.00000000000000E+00 -.23544427132833E+02
 0.30087454564624E-02 0.11131471737900E+01 0.26589155233511E+04 0.99709332125667E+03 0.71868304464734E+01
 0.26950614174275E+01 0.34827330282759E+03 0.29915101374525E+03 0.34113197022364E+03 0.36939922531043E+03
 0.29915014770306E+03 0.29915027578617E+03 0.33720752441604E+03 0.36937626245757E+03 0.29915012241883E+03
 0.29915027509519E+03 0.34113197022364E+03 0.36939922531043E+03 0.29915014770306E+03 0.29915027578617E+03
 0.33720752441604E+03 0.36937626245757E+03 0.29915012241883E+03 0.29915027509519E+03 0.41323256496132E+03
 0.33865911168803E+03 0.19875944184104E+04 0.18346170330027E+04 0.57773717120660E+03 0.93383694676679E+03
 0.35321108970415E+03 0.11844078839463E+04 0.11242296978329E+04 0.10736173434456E+04 0.17140179988346E+04
 0.10777214210490E+04 0.11238905544414E+04 0.99123948299052E+03 0.17138808834685E+04 0.11844078839463E+04
 0.11242296978329E+04 0.10736173434456E+04 0.17140179988346E+04 0.10777214210490E+04 0.11238905544414E+04
 0.99123948299053E+03 0.17138808834685E+04 0.16770541309639E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49058902933701E+03 0.12948779643887E+01
 0.12948779643887E+01 0.32328284759421E+01 0.00000000000000E+00 0.34098281042912E+03 0.34098281042912E+03
 0.34098281042912E+03 0.34098281042912E+03 0.00000000000000E+00 0.00000000000000E+00 0.15790960312307E+00
 0.00000000000000E+00 -.15700301907429E+02 0.10000000000000E-02 0.13393236059793E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59731643377930E+01 0.22399366266724E+01 0.33866104028480E+03 0.41323028366745E+03
 0.31271006858046E+03 0.31271006858046E+03 0.29915003959997E+03 0.29915003809040E+03 0.31270619463796E+03
 0.31270619463796E+03 0.29915003961052E+03 0.29915003810056E+03 0.31271006858046E+03 0.31271006858046E+03
 0.29915003959997E+03 0.29915003809040E+03 0.31270619463796E+03 0.31270619463796E+03 0.29915003961052E+03
 0.29915003810056E+03 0.31072183210192E+03 0.29915018934736E+03 -.13374785145940E+01 0.50148294858920E+00
 0.20365410112725E+03 0.46658769947528E+03 0.26191532784238E+03 0.18253993941296E+03 0.18826646686324E+03
 0.18253993941296E+03 0.37024412113387E+03 0.18255970143540E+03 0.18809042829750E+03 0.18255970143540E+03
 0.37009815272602E+03 0.18253993941296E+03 0.18826646686324E+03 0.18253993941296E+03 0.37024412113387E+03
 0.18255970143540E+03 0.18809042829750E+03 0.18255970143540E+03 0.37009815272602E+03 0.22315712391295E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36643056877285E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19544175008477E+00 0.00000000000000E+00 0.00000000000000E+00 0.19544175008477E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20356342410684E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20356342410684E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936811862070E+00 0.20580254567081E+00 0.34098281042912E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    860.70861266
 0.11690114436626E+00 0.31915261375044E+03 0.45225860437847E+03 0.44707185683633E+03 0.44502620682017E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15044985347060E+00 0.00000000000000E+00 -.23570471209024E+02
 0.30108245605604E-02 0.11145618855693E+01 0.26570794276074E+04 0.99640478535277E+03 0.71777082130473E+01
 0.26916405798927E+01 0.34855064554733E+03 0.29915109230917E+03 0.34137403207804E+03 0.36974758726040E+03
 0.29915016078107E+03 0.29915030018286E+03 0.33743886869332E+03 0.36972469892180E+03 0.29915013336280E+03
 0.29915029943796E+03 0.34137403207804E+03 0.36974758726039E+03 0.29915016078107E+03 0.29915030018286E+03
 0.33743886869332E+03 0.36972469892180E+03 0.29915013336280E+03 0.29915029943796E+03 0.41362303407363E+03
 0.33907516516166E+03 0.19898738241478E+04 0.18359285171093E+04 0.57677338947711E+03 0.93118371901057E+03
 0.35152646258608E+03 0.11860802714438E+04 0.11253268887665E+04 0.10745871355633E+04 0.17140167052494E+04
 0.10795360232846E+04 0.11249892078974E+04 0.99245074424004E+03 0.17138802650554E+04 0.11860802714438E+04
 0.11253268887665E+04 0.10745871355633E+04 0.17140167052494E+04 0.10795360232846E+04 0.11249892078973E+04
 0.99245074424005E+03 0.17138802650554E+04 0.16767673334343E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49082824696329E+03 0.12948781562923E+01
 0.12948781562923E+01 0.32628569768309E+01 0.00000000000000E+00 0.34112785729003E+03 0.34112785729003E+03
 0.34112785729003E+03 0.34112785729003E+03 0.00000000000000E+00 0.00000000000000E+00 0.15761185098312E+00
 0.00000000000000E+00 -.15715937795094E+02 0.10000000000000E-02 0.13442982675453E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59510602618034E+01 0.22316475981763E+01 0.33907714918270E+03 0.41362068271692E+03
 0.31280717273591E+03 0.31280717273591E+03 0.29915004319078E+03 0.29915004154433E+03 0.31280324813899E+03
 0.31280324813899E+03 0.29915004320221E+03 0.29915004155532E+03 0.31280717273591E+03 0.31280717273591E+03
 0.29915004319078E+03 0.29915004154433E+03 0.31280324813899E+03 0.31280324813899E+03 0.29915004320221E+03
 0.29915004155532E+03 0.31080625411446E+03 0.29915020444517E+03 -.33606952695119E+01 -.17863305336450E+01
 0.20448012284154E+03 0.46801943801437E+03 0.26251691455862E+03 0.18412582242610E+03 0.18898274328078E+03
 0.18412582242610E+03 0.37131275612798E+03 0.18414588506375E+03 0.18880557825811E+03 0.18414588506375E+03
 0.37116606232764E+03 0.18412582242610E+03 0.18898274328078E+03 0.18412582242610E+03 0.37131275612798E+03
 0.18414588506375E+03 0.18880557825811E+03 0.18414588506375E+03 0.37116606232764E+03 0.22327216937773E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36660102948580E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19554971419393E+00 0.00000000000000E+00 0.00000000000000E+00 0.19554971419393E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20359143078384E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20359143078384E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936726895698E+00 0.20571413037974E+00 0.34112785729003E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    875.72286310
 0.11678171662434E+00 0.31941079477280E+03 0.45273704442974E+03 0.44754702166106E+03 0.44549825930711E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15017285655986E+00 0.00000000000000E+00 -.23577872005506E+02
 0.30139032306012E-02 0.11172652753127E+01 0.26543652492798E+04 0.99538696847994E+03 0.71603406789500E+01
 0.26851277546063E+01 0.34910202728965E+03 0.29915126416847E+03 0.34185548919249E+03 0.37043948015564E+03
 0.29915018983037E+03 0.29915035435264E+03 0.33789924521745E+03 0.37041673655742E+03 0.29915015770081E+03
 0.29915035348986E+03 0.34185548919249E+03 0.37043948015564E+03 0.29915018983037E+03 0.29915035435264E+03
 0.33789924521745E+03 0.37041673655742E+03 0.29915015770081E+03 0.29915035348986E+03 0.41439735565606E+03
 0.33990091586564E+03 0.19943558015181E+04 0.18384712549973E+04 0.57483269857765E+03 0.92590449635035E+03
 0.34819763427981E+03 0.11893849781581E+04 0.11274801889939E+04 0.10764874656314E+04 0.17139918956275E+04
 0.10831219638625E+04 0.11271452703791E+04 0.99482760883608E+03 0.17138566856344E+04 0.11893849781581E+04
 0.11274801889939E+04 0.10764874656314E+04 0.17139918956275E+04 0.10831219638625E+04 0.11271452703791E+04
 0.99482760883609E+03 0.17138566856344E+04 0.16761614479074E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49130105000642E+03 0.12948782108244E+01
 0.12948782108244E+01 0.33229139786086E+01 0.00000000000000E+00 0.34141964089176E+03 0.34141964089176E+03
 0.34141964089176E+03 0.34141964089176E+03 0.00000000000000E+00 0.00000000000000E+00 0.15703489008497E+00
 0.00000000000000E+00 -.15696535064670E+02 0.10000000000000E-02 0.13538834555929E+01 0.80000000000000E+04
 0.30000000000000E+04 0.59089281037832E+01 0.22158480389187E+01 0.33990300440730E+03 0.41439487229323E+03
 0.31300135514107E+03 0.31300135514107E+03 0.29915005118971E+03 0.29915004923834E+03 0.31299732942382E+03
 0.31299732942382E+03 0.29915005120304E+03 0.29915004925116E+03 0.31300135514107E+03 0.31300135514107E+03
 0.29915005118971E+03 0.29915004923834E+03 0.31299732942382E+03 0.31299732942382E+03 0.29915005120304E+03
 0.29915004925116E+03 0.31097514075561E+03 0.29915023756925E+03 -.73893772852263E+01 -.63099805172367E+01
 0.20613423287579E+03 0.47088393579837E+03 0.26371903175819E+03 0.18729141723890E+03 0.19041719987727E+03
 0.18729141723890E+03 0.37344762819774E+03 0.18731208188127E+03 0.19023778999212E+03 0.18731208188127E+03
 0.37329948865659E+03 0.18729141723890E+03 0.19041719987727E+03 0.18729141723890E+03 0.37344762819774E+03
 0.18731208188127E+03 0.19023778999212E+03 0.18731208188127E+03 0.37329948865658E+03 0.22351426098186E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36693201387618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19543178130926E+00 0.00000000000000E+00 0.00000000000000E+00 0.19543178130926E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20342800514165E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20342800514165E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936726155589E+00 0.20553835498986E+00 0.34141964089176E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    883.22998833
 0.11672372937422E+00 0.31954107917933E+03 0.45297452023685E+03 0.44778290394902E+03 0.44573265660544E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15003902213615E+00 0.00000000000000E+00 -.23580858766395E+02
 0.30154003240798E-02 0.11185538633386E+01 0.26530474033962E+04 0.99489277627358E+03 0.71520918770262E+01
 0.26820344538848E+01 0.34937610246654E+03 0.29915135790336E+03 0.34209493842467E+03 0.37078301517810E+03
 0.29915020590905E+03 0.29915038432340E+03 0.33812832398482E+03 0.37076034185347E+03 0.29915017118716E+03
 0.29915038339639E+03 0.34209493842467E+03 0.37078301517810E+03 0.29915020590905E+03 0.29915038432340E+03
 0.33812832398482E+03 0.37076034185347E+03 0.29915017118716E+03 0.29915038339639E+03 0.41478117039968E+03
 0.34031065592114E+03 0.19965703232390E+04 0.18397344964354E+04 0.57385675930023E+03 0.92328027403804E+03
 0.34655423094132E+03 0.11910216043297E+04 0.11285363972052E+04 0.10774369178657E+04 0.17139676572914E+04
 0.10848982438537E+04 0.11282027870267E+04 0.99601248122958E+03 0.17138330103779E+04 0.11910216043297E+04
 0.11285363972052E+04 0.10774369178657E+04 0.17139676572914E+04 0.10848982438537E+04 0.11282027870267E+04
 0.99601248122959E+03 0.17138330103779E+04 0.16758530998825E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49153578167607E+03 0.12948782328321E+01
 0.12948782328321E+01 0.33529424794974E+01 0.00000000000000E+00 0.34156536282433E+03 0.34156536282433E+03
 0.34156536282433E+03 0.34156536282433E+03 0.00000000000000E+00 0.00000000000000E+00 0.15675550962512E+00
 0.00000000000000E+00 -.15686212724709E+02 0.10000000000000E-02 0.13585011683523E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58888429295229E+01 0.22083160985711E+01 0.34031279379084E+03 0.41477862463059E+03
 0.31309840703523E+03 0.31309840703523E+03 0.29915005562925E+03 0.29915005350864E+03 0.31309433083733E+03
 0.31309433083733E+03 0.29915005564362E+03 0.29915005352246E+03 0.31309840703523E+03 0.31309840703523E+03
 0.29915005562925E+03 0.29915005350864E+03 0.31309433083733E+03 0.31309433083733E+03 0.29915005564362E+03
 0.29915005352246E+03 0.31105958636724E+03 0.29915025568679E+03 -.94019405798872E+01 -.85522546036003E+01
 0.20695542752827E+03 0.47230843719779E+03 0.26431823253187E+03 0.18886742403043E+03 0.19112849542733E+03
 0.18886742403043E+03 0.37450807465626E+03 0.18888839023654E+03 0.19094796634435E+03 0.18888839023654E+03
 0.37435921505262E+03 0.18886742403043E+03 0.19112849542733E+03 0.18886742403043E+03 0.37450807465626E+03
 0.18888839023654E+03 0.19094796634435E+03 0.18888839023654E+03 0.37435921505262E+03 0.22363604306705E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36709703889616E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19536814637524E+00 0.00000000000000E+00 0.00000000000000E+00 0.19536814637524E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20334641859988E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20334641859988E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936727942679E+00 0.20545070557719E+00 0.34156536282433E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    890.73711355
 0.11666304970934E+00 0.31967169352156E+03 0.45321098521191E+03 0.44801795153703E+03 0.44596630854806E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14990818754770E+00 0.00000000000000E+00 -.23583747075978E+02
 0.30169685320575E-02 0.11198016343427E+01 0.26516683601417E+04 0.99437563505314E+03 0.71441224540596E+01
 0.26790459202723E+01 0.34964914839583E+03 0.29915145714411E+03 0.34233357987195E+03 0.37112496018633E+03
 0.29915022309865E+03 0.29915041635580E+03 0.33835670532395E+03 0.37110235577735E+03 0.29915018561620E+03
 0.29915041536085E+03 0.34233357987195E+03 0.37112496018633E+03 0.29915022309865E+03 0.29915041635580E+03
 0.33835670532395E+03 0.37110235577735E+03 0.29915018561620E+03 0.29915041536085E+03 0.41516277726537E+03
 0.34071831647009E+03 0.19987721155494E+04 0.18409935925107E+04 0.57287997392741E+03 0.92067057622011E+03
 0.34492620242306E+03 0.11926500577632E+04 0.11295818467457E+04 0.10783842525705E+04 0.17139393093791E+04
 0.10866658373253E+04 0.11292494999644E+04 0.99719379828799E+03 0.17138051939052E+04 0.11926500577632E+04
 0.11295818467457E+04 0.10783842525705E+04 0.17139393093791E+04 0.10866658373253E+04 0.11292494999644E+04
 0.99719379828800E+03 0.17138051939051E+04 0.16755451377973E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49176972147243E+03 0.12948782541144E+01
 0.12948782541144E+01 0.33829709803863E+01 0.00000000000000E+00 0.34171076285292E+03 0.34171076285292E+03
 0.34171076285292E+03 0.34171076285292E+03 0.00000000000000E+00 0.00000000000000E+00 0.15648205301814E+00
 0.00000000000000E+00 -.15675842882217E+02 0.10000000000000E-02 0.13630059932442E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58693799144335E+01 0.22010174679126E+01 0.34072050194363E+03 0.41516017147299E+03
 0.31319540715669E+03 0.31319540715669E+03 0.29915006038414E+03 0.29915005808227E+03 0.31319128052264E+03
 0.31319128052264E+03 0.29915006039960E+03 0.29915005809715E+03 0.31319540715669E+03 0.31319540715669E+03
 0.29915006038414E+03 0.29915005808227E+03 0.31319128052264E+03 0.31319128052264E+03 0.29915006039960E+03
 0.29915005809715E+03 0.31114401314168E+03 0.29915027490428E+03 -.11414560878860E+02 -.10781244401183E+02
 0.20777150953926E+03 0.47372472921239E+03 0.26491436212543E+03 0.19043826451237E+03 0.19183465257347E+03
 0.19043826451237E+03 0.37556134151245E+03 0.19045953271823E+03 0.19165300576936E+03 0.19045953271823E+03
 0.37541176306941E+03 0.19043826451237E+03 0.19183465257347E+03 0.19043826451237E+03 0.37556134151245E+03
 0.19045953271823E+03 0.19165300576936E+03 0.19045953271823E+03 0.37541176306941E+03 0.22375688939526E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36726160984071E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19530410652307E+00 0.00000000000000E+00 0.00000000000000E+00 0.19530410652307E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20326509071402E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20326509071402E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936729988790E+00 0.20536332714431E+00 0.34171076285292E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    905.75136399
 0.11653590006689E+00 0.31993198749891E+03 0.45368122416198E+03 0.44848569493605E+03 0.44643141004863E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14965508606449E+00 0.00000000000000E+00 -.23589534267170E+02
 0.30202599091464E-02 0.11221802013047E+01 0.26487786616553E+04 0.99329199812075E+03 0.71289798115299E+01
 0.26733674293237E+01 0.35019224226520E+03 0.29915167308384E+03 0.34280849619679E+03 0.37180417456772E+03
 0.29915026104746E+03 0.29915048704060E+03 0.33881142894137E+03 0.37178170403193E+03 0.29915021750649E+03
 0.29915048589802E+03 0.34280849619679E+03 0.37180417456772E+03 0.29915026104746E+03 0.29915048704060E+03
 0.33881142894137E+03 0.37178170403193E+03 0.29915021750649E+03 0.29915048589802E+03 0.41591953367109E+03
 0.34152749594774E+03 0.20031420267848E+04 0.18434893766245E+04 0.57092898464579E+03 0.91550130475160E+03
 0.34171767518259E+03 0.11958854326167E+04 0.11316465754883E+04 0.10802630586676E+04 0.17138782758815E+04
 0.10901778684715E+04 0.11313166271003E+04 0.99953681981998E+03 0.17137451340287E+04 0.11958854326167E+04
 0.11316465754883E+04 0.10802630586676E+04 0.17138782758815E+04 0.10901778684715E+04 0.11313166271003E+04
 0.99953681981998E+03 0.17137451340287E+04 0.16749336319599E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49223532427886E+03 0.12948782967569E+01
 0.12948782967569E+01 0.34430279821639E+01 0.00000000000000E+00 0.34200058013235E+03 0.34200058013235E+03
 0.34200058013235E+03 0.34200058013235E+03 0.00000000000000E+00 0.00000000000000E+00 0.15595243329149E+00
 0.00000000000000E+00 -.15655280314008E+02 0.10000000000000E-02 0.13716865840010E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58322360904522E+01 0.21870885339196E+01 0.34152977167599E+03 0.41591681537933E+03
 0.31338920816982E+03 0.31338920816982E+03 0.29915007090961E+03 0.29915006820651E+03 0.31338498077127E+03
 0.31338498077127E+03 0.29915007092747E+03 0.29915006822369E+03 0.31338920816982E+03 0.31338920816982E+03
 0.29915007090961E+03 0.29915006820651E+03 0.31338498077127E+03 0.31338498077127E+03 0.29915007092747E+03
 0.29915006822369E+03 0.31131277328175E+03 0.29915031683559E+03 -.15439829965584E+02 -.15189884881849E+02
 0.20938897932318E+03 0.47653363049456E+03 0.26609770627476E+03 0.19356498289412E+03 0.19323221514732E+03
 0.19356498289412E+03 0.37764727275689E+03 0.19358685654185E+03 0.19304833484322E+03 0.19358685654185E+03
 0.37749625802250E+03 0.19356498289412E+03 0.19323221514732E+03 0.19356498289412E+03 0.37764727275689E+03
 0.19358685654185E+03 0.19304833484322E+03 0.19358685654185E+03 0.37749625802250E+03 0.22399855900599E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36758946053090E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19517710875801E+00 0.00000000000000E+00 0.00000000000000E+00 0.19517710875801E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20310366685931E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20310366685931E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936733791983E+00 0.20518937946913E+00 0.34200058013235E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    913.25848922
 0.11647109018079E+00 0.32006142045329E+03 0.45391506745148E+03 0.44871837405463E+03 0.44666280430012E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14953262768373E+00 0.00000000000000E+00 -.23592539524375E+02
 0.30219403381050E-02 0.11233136347861E+01 0.26473057390063E+04 0.99273965212738E+03 0.71217866072849E+01
 0.26706699777318E+01 0.35046231558909E+03 0.29915179026713E+03 0.34304478300175E+03 0.37214149305647E+03
 0.29915028193027E+03 0.29915052591975E+03 0.33903778129323E+03 0.37211908753223E+03 0.29915023507433E+03
 0.29915052469718E+03 0.34304478300175E+03 0.37214149305647E+03 0.29915028193027E+03 0.29915052591975E+03
 0.33903778129323E+03 0.37211908753223E+03 0.29915023507433E+03 0.29915052469718E+03 0.41629476214613E+03
 0.34192911056988E+03 0.20053104892695E+04 0.18447244724379E+04 0.56995557079036E+03 0.91294224094515E+03
 0.34013689230084E+03 0.11974928154132E+04 0.11326670592155E+04 0.10811936068589E+04 0.17138469417716E+04
 0.10919227145280E+04 0.11323382489189E+04 0.10006975904595E+04 0.17137142446739E+04 0.11974928154132E+04
 0.11326670592156E+04 0.10811936068589E+04 0.17138469417716E+04 0.10919227145280E+04 0.11323382489189E+04
 0.10006975904595E+04 0.17137142446739E+04 0.16746303639739E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49246695196498E+03 0.12948783189009E+01
 0.12948783189009E+01 0.34730564830527E+01 0.00000000000000E+00 0.34214498392901E+03 0.34214498392901E+03
 0.34214498392901E+03 0.34214498392901E+03 0.00000000000000E+00 0.00000000000000E+00 0.15569602988507E+00
 0.00000000000000E+00 -.15645209245062E+02 0.10000000000000E-02 0.13758671832691E+01 0.80000000000000E+04
 0.30000000000000E+04 0.58145147273532E+01 0.21804430227575E+01 0.34193142882259E+03 0.41629199145843E+03
 0.31348599704688E+03 0.31348599704688E+03 0.29915007671664E+03 0.29915007379217E+03 0.31348171930567E+03
 0.31348171930567E+03 0.29915007673579E+03 0.29915007381059E+03 0.31348599704688E+03 0.31348599704688E+03
 0.29915007671664E+03 0.29915007379217E+03 0.31348171930567E+03 0.31348171930567E+03 0.29915007673579E+03
 0.29915007381059E+03 0.31139709622694E+03 0.29915033965081E+03 -.17452838200335E+02 -.17359812084821E+02
 0.21019061183200E+03 0.47792650054039E+03 0.26668493564923E+03 0.19512122100724E+03 0.19392385746709E+03
 0.19512122100724E+03 0.37868020318445E+03 0.19514339817378E+03 0.19373885993191E+03 0.19514339817378E+03
 0.37852846965804E+03 0.19512122100724E+03 0.19392385746709E+03 0.19512122100724E+03 0.37868020318445E+03
 0.19514339817378E+03 0.19373885993191E+03 0.19514339817378E+03 0.37852846965803E+03 0.22412185816150E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36775276230271E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19511497539646E+00 0.00000000000000E+00 0.00000000000000E+00 0.19511497539646E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20302398908406E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20302398908406E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936735144730E+00 0.20510281270098E+00 0.34214498392901E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    920.76561444
 0.11640628260776E+00 0.32019041000683E+03 0.45414806931796E+03 0.44895023160221E+03 0.44689338824156E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14941279009636E+00 0.00000000000000E+00 -.23595717857113E+02
 0.30236225789743E-02 0.11244113959756E+01 0.26458328680406E+04 0.99218732551524E+03 0.71148336175110E+01
 0.26680626065666E+01 0.35073141994370E+03 0.29915191392484E+03 0.34328029697869E+03 0.37247731523770E+03
 0.29915030417123E+03 0.29915056731414E+03 0.33926346704073E+03 0.37245497347595E+03 0.29915025379814E+03
 0.29915056600725E+03 0.34328029697869E+03 0.37247731523770E+03 0.29915030417123E+03 0.29915056731414E+03
 0.33926346704073E+03 0.37245497347595E+03 0.29915025379814E+03 0.29915056600725E+03 0.41666793177646E+03
 0.34232886954717E+03 0.20074677338019E+04 0.18459509797231E+04 0.56898355545046E+03 0.91039981061364E+03
 0.33857133738593E+03 0.11990933255420E+04 0.11336797754276E+04 0.10821185691534E+04 0.17138150412983E+04
 0.10936600915415E+04 0.11333520644246E+04 0.10018514684791E+04 0.17136827625121E+04 0.11990933255420E+04
 0.11336797754276E+04 0.10821185691534E+04 0.17138150412983E+04 0.10936600915415E+04 0.11333520644246E+04
 0.10018514684791E+04 0.17136827625121E+04 0.16743285242015E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49269776412863E+03 0.12948783423202E+01
 0.12948783423202E+01 0.35030849839416E+01 0.00000000000000E+00 0.34228899797664E+03 0.34228899797664E+03
 0.34228899797664E+03 0.34228899797664E+03 0.00000000000000E+00 0.00000000000000E+00 0.15544507281515E+00
 0.00000000000000E+00 -.15635395366279E+02 0.10000000000000E-02 0.13799446691918E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57973338921518E+01 0.21740002095569E+01 0.34233122832425E+03 0.41666511119482E+03
 0.31358270504128E+03 0.31358270504128E+03 0.29915008291190E+03 0.29915007975127E+03 0.31357837697241E+03
 0.31357837697241E+03 0.29915008293241E+03 0.29915007977099E+03 0.31358270504128E+03 0.31358270504128E+03
 0.29915008291190E+03 0.29915007975127E+03 0.31357837697241E+03 0.31357837697241E+03 0.29915008293241E+03
 0.29915007977099E+03 0.31148137549620E+03 0.29915036376877E+03 -.19467196803043E+02 -.19491727318718E+02
 0.21098735647219E+03 0.47931106745785E+03 0.26726877420330E+03 0.19667300013387E+03 0.19461057164777E+03
 0.19667300013387E+03 0.37970597071297E+03 0.19669548141346E+03 0.19442445515224E+03 0.19669548141346E+03
 0.37955351663750E+03 0.19667300013387E+03 0.19461057164777E+03 0.19667300013387E+03 0.37970597071297E+03
 0.19669548141346E+03 0.19442445515224E+03 0.19669548141346E+03 0.37955351663750E+03 0.22425013835823E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36791563241587E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19505451203504E+00 0.00000000000000E+00 0.00000000000000E+00 0.19505451203504E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20294556459524E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20294556459524E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936735751690E+00 0.20501654422675E+00 0.34228899797664E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    930.77020947
 0.11630549300267E+00 0.32037225348758E+03 0.45446012264506E+03 0.44926173740238E+03 0.44720375946637E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14925692389248E+00 0.00000000000000E+00 -.23594650200658E+02
 0.30262425848775E-02 0.11258153110876E+01 0.26435422064236E+04 0.99132832740885E+03 0.71059612719882E+01
 0.26647354769956E+01 0.35109044751667E+03 0.29915208507651E+03 0.34359491652606E+03 0.37292236545109E+03
 0.29915033515097E+03 0.29915062495993E+03 0.33956557570496E+03 0.37290010766688E+03 0.29915027989162E+03
 0.29915062353643E+03 0.34359491652606E+03 0.37292236545109E+03 0.29915033515097E+03 0.29915062495993E+03
 0.33956557570496E+03 0.37290010766688E+03 0.29915027989162E+03 0.29915062353643E+03 0.41715155197139E+03
 0.34284882759395E+03 0.20103637658600E+04 0.18476752950023E+04 0.56789638235599E+03 0.90738617244170E+03
 0.33665030817393E+03 0.12012359703622E+04 0.11350677707345E+04 0.10834177523777E+04 0.17138465188977E+04
 0.10959816111705E+04 0.11347415188119E+04 0.10034458722421E+04 0.17137148038470E+04 0.12012359703622E+04
 0.11350677707345E+04 0.10834177523777E+04 0.17138465188977E+04 0.10959816111705E+04 0.11347415188119E+04
 0.10034458722421E+04 0.17137148038470E+04 0.16740837182334E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49300812566529E+03 0.12948783344532E+01
 0.12948783344532E+01 0.35431033640734E+01 0.00000000000000E+00 0.34248047685894E+03 0.34248047685894E+03
 0.34248047685894E+03 0.34248047685894E+03 0.00000000000000E+00 0.00000000000000E+00 0.15511890363854E+00
 0.00000000000000E+00 -.15616540781206E+02 0.10000000000000E-02 0.13852215860741E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57752493033791E+01 0.21657184887672E+01 0.34285128559798E+03 0.41714861121822E+03
 0.31371351530523E+03 0.31371351530523E+03 0.29915008956956E+03 0.29915008806150E+03 0.31370911951907E+03
 0.31370911951907E+03 0.29915008959147E+03 0.29915008808303E+03 0.31371351530523E+03 0.31371351530523E+03
 0.29915008956956E+03 0.29915008806150E+03 0.31370911951907E+03 0.31370911951907E+03 0.29915008959147E+03
 0.29915008808303E+03 0.31159556628391E+03 0.29915039719026E+03 -.22068988428796E+02 -.22506288824813E+02
 0.21202782197332E+03 0.48111004002289E+03 0.26802207893970E+03 0.19868644406050E+03 0.19550529837728E+03
 0.19868644406050E+03 0.38103341910042E+03 0.19870933746814E+03 0.19531781139314E+03 0.19870933746814E+03
 0.38088012998058E+03 0.19868644406050E+03 0.19550529837728E+03 0.19868644406050E+03 0.38103341910042E+03
 0.19870933746814E+03 0.19531781139314E+03 0.19870933746814E+03 0.38088012998058E+03 0.22435653132329E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36813198559930E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19493284000456E+00 0.00000000000000E+00 0.00000000000000E+00 0.19493284000456E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20282904081776E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20282904081776E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936756020441E+00 0.20490216710707E+00 0.34248047685894E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    941.76778485
 0.11619920347367E+00 0.32056220504517E+03 0.45479977768441E+03 0.44960034467877E+03 0.44754078614324E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14909044163459E+00 0.00000000000000E+00 -.23593971649170E+02
 0.30290104773710E-02 0.11272970319145E+01 0.26411265526369E+04 0.99042245723885E+03 0.70966211863556E+01
 0.26612329448833E+01 0.35148224006123E+03 0.29915228796862E+03 0.34393817955924E+03 0.37340945274075E+03
 0.29915037234486E+03 0.29915069413636E+03 0.33989492812103E+03 0.37338728447911E+03 0.29915031124984E+03
 0.29915069257486E+03 0.34393817955924E+03 0.37340945274075E+03 0.29915037234486E+03 0.29915069413636E+03
 0.33989492812103E+03 0.37338728447911E+03 0.29915031124984E+03 0.29915069257486E+03 0.41768929483680E+03
 0.34342061293448E+03 0.20135058099459E+04 0.18494749872688E+04 0.56652566596590E+03 0.90380022537570E+03
 0.33444193107997E+03 0.12035678340450E+04 0.11365427822403E+04 0.10847729561847E+04 0.17138233460704E+04
 0.10985115572647E+04 0.11362180154840E+04 0.10051306218413E+04 0.17136921578233E+04 0.12035678340450E+04
 0.11365427822403E+04 0.10847729561847E+04 0.17138233460704E+04 0.10985115572647E+04 0.11362180154840E+04
 0.10051306218413E+04 0.17136921578233E+04 0.16736904121002E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49334533737508E+03 0.12948783294534E+01
 0.12948783294534E+01 0.35870936655710E+01 0.00000000000000E+00 0.34269179229214E+03 0.34269179229214E+03
 0.34269179229214E+03 0.34269179229214E+03 0.00000000000000E+00 0.00000000000000E+00 0.15477097688251E+00
 0.00000000000000E+00 -.15595829751862E+02 0.10000000000000E-02 0.13908151662502E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57520224068084E+01 0.21570084025531E+01 0.34342312868916E+03 0.41768631115439E+03
 0.31385559437763E+03 0.31385559437763E+03 0.29915009974125E+03 0.29915009806193E+03 0.31385112494187E+03
 0.31385112494187E+03 0.29915009976532E+03 0.29915009808560E+03 0.31385559437763E+03 0.31385559437763E+03
 0.29915009974125E+03 0.29915009806193E+03 0.31385112494187E+03 0.31385112494187E+03 0.29915009976532E+03
 0.29915009808560E+03 0.31171953185213E+03 0.29915043690530E+03 -.24952686960890E+02 -.26033618893955E+02
 0.21318388807052E+03 0.48312235420568E+03 0.26887254669481E+03 0.20092019604651E+03 0.19650076810070E+03
 0.20092019604651E+03 0.38252199505329E+03 0.20094353781623E+03 0.19631169923189E+03 0.20094353781623E+03
 0.38236770711844E+03 0.20092019604651E+03 0.19650076810070E+03 0.20092019604651E+03 0.38252199505329E+03
 0.20094353781623E+03 0.19631169923189E+03 0.20094353781623E+03 0.38236770711843E+03 0.22443938043862E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36836871435534E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19480446361198E+00 0.00000000000000E+00 0.00000000000000E+00 0.19480446361198E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20267711390622E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20267711390622E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936778120583E+00 0.20477608632471E+00 0.34269179229214E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    950.93243099
 0.11610987110504E+00 0.32071910608937E+03 0.45508116093151E+03 0.44988090730846E+03 0.44782004745239E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14895543968693E+00 0.00000000000000E+00 -.23599113624573E+02
 0.30313407101571E-02 0.11284821614457E+01 0.26390962827749E+04 0.98966110604060E+03 0.70891683300970E+01
 0.26584381237864E+01 0.35180712535200E+03 0.29915246891840E+03 0.34422291385042E+03 0.37381302791767E+03
 0.29915040589823E+03 0.29915075651417E+03 0.34016821581892E+03 0.37379093229966E+03 0.29915033956376E+03
 0.29915075482979E+03 0.34422291385042E+03 0.37381302791767E+03 0.29915040589823E+03 0.29915075651417E+03
 0.34016821581892E+03 0.37379093229966E+03 0.29915033956376E+03 0.29915075482979E+03 0.41813471391415E+03
 0.34389206774285E+03 0.20161032572267E+04 0.18509535000508E+04 0.56537312059360E+03 0.90081616422721E+03
 0.33261617803064E+03 0.12054984545194E+04 0.11377546224558E+04 0.10858876511362E+04 0.17137949967720E+04
 0.11006064834738E+04 0.11374310398328E+04 0.10065184257930E+04 0.17136642131298E+04 0.12054984545194E+04
 0.11377546224558E+04 0.10858876511362E+04 0.17137949967720E+04 0.11006064834739E+04 0.11374310398328E+04
 0.10065184257930E+04 0.17136642131298E+04 0.16733530520020E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49362474764226E+03 0.12948783673416E+01
 0.12948783673416E+01 0.36237522501524E+01 0.00000000000000E+00 0.34286887371797E+03 0.34286887371797E+03
 0.34286887371797E+03 0.34286887371797E+03 0.00000000000000E+00 0.00000000000000E+00 0.15448928284606E+00
 0.00000000000000E+00 -.15585303437573E+02 0.10000000000000E-02 0.13953142704064E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57334753680041E+01 0.21500532630016E+01 0.34389463058453E+03 0.41813169705949E+03
 0.31397384580727E+03 0.31397384580727E+03 0.29915010893660E+03 0.29915010710246E+03 0.31396931515207E+03
 0.31396931515207E+03 0.29915010896261E+03 0.29915010712803E+03 0.31397384580727E+03 0.31397384580727E+03
 0.29915010893660E+03 0.29915010710246E+03 0.31396931515207E+03 0.31396931515207E+03 0.29915010896261E+03
 0.29915010712803E+03 0.31182273779418E+03 0.29915047240208E+03 -.27327677591681E+02 -.29023873832651E+02
 0.21415127673290E+03 0.48481667850657E+03 0.26959464539001E+03 0.20277335656167E+03 0.19733449523279E+03
 0.20277335656167E+03 0.38377765908595E+03 0.20279707213677E+03 0.19714411864082E+03 0.20279707213677E+03
 0.38362254899230E+03 0.20277335656167E+03 0.19733449523279E+03 0.20277335656167E+03 0.38377765908595E+03
 0.20279707213677E+03 0.19714411864082E+03 0.20279707213677E+03 0.38362254899230E+03 0.22450495763629E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36856814898014E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19474127867181E+00 0.00000000000000E+00 0.00000000000000E+00 0.19474127867181E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20258330750621E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20258330750621E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936773843019E+00 0.20467030301583E+00 0.34286887371797E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    961.36631005
 0.11600690459331E+00 0.32089679113518E+03 0.45539976844083E+03 0.45019867708889E+03 0.44813637936499E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14880571940785E+00 0.00000000000000E+00 -.23600712090857E+02
 0.30340310461434E-02 0.11297786322575E+01 0.26367561433390E+04 0.98878355375213E+03 0.70810331967551E+01
 0.26553874487832E+01 0.35217520221141E+03 0.29915268892995E+03 0.34454560052665E+03 0.37426996932208E+03
 0.29915044715263E+03 0.29915083317395E+03 0.34047801415855E+03 0.37424795429385E+03 0.29915037440622E+03
 0.29915083134043E+03 0.34454560052665E+03 0.37426996932208E+03 0.29915044715263E+03 0.29915083317395E+03
 0.34047801415855E+03 0.37424795429385E+03 0.29915037440622E+03 0.29915083134043E+03 0.41863917673824E+03
 0.34442387403146E+03 0.20190398034732E+04 0.18526203493852E+04 0.56404670950154E+03 0.89741792481044E+03
 0.33055098176139E+03 0.12076838042593E+04 0.11391167151782E+04 0.10871458127541E+04 0.17137546003530E+04
 0.11029781752239E+04 0.11387944229631E+04 0.10080858732554E+04 0.17136242402590E+04 0.12076838042593E+04
 0.11391167151782E+04 0.10871458127541E+04 0.17137546003530E+04 0.11029781752239E+04 0.11387944229631E+04
 0.10080858732555E+04 0.17136242402590E+04 0.16729599603331E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49394122841894E+03 0.12948783791198E+01
 0.12948783791198E+01 0.36654877663834E+01 0.00000000000000E+00 0.34307145036803E+03 0.34307145036803E+03
 0.34307145036803E+03 0.34307145036803E+03 0.00000000000000E+00 0.00000000000000E+00 0.15417742299162E+00
 0.00000000000000E+00 -.15568545205214E+02 0.10000000000000E-02 0.14002630240568E+01 0.80000000000000E+04
 0.30000000000000E+04 0.57132123483649E+01 0.21424546306369E+01 0.34442649187309E+03 0.41863611608173E+03
 0.31410828860555E+03 0.31410828860555E+03 0.29915012048681E+03 0.29915011824067E+03 0.31410368844460E+03
 0.31410368844460E+03 0.29915012051520E+03 0.29915011826853E+03 0.31410828860555E+03 0.31410828860555E+03
 0.29915012048681E+03 0.29915011824067E+03 0.31410368844460E+03 0.31410368844460E+03 0.29915012051520E+03
 0.29915011826853E+03 0.31194009927345E+03 0.29915051565261E+03 -.30005814153617E+02 -.32462961914943E+02
 0.21525691847068E+03 0.48675927341192E+03 0.27042607034888E+03 0.20487588697006E+03 0.19828800178679E+03
 0.20487588697006E+03 0.38521848942244E+03 0.20490002812256E+03 0.19809614109764E+03 0.20490002812256E+03
 0.38506244680654E+03 0.20487588697006E+03 0.19828800178679E+03 0.20487588697006E+03 0.38521848942244E+03
 0.20490002812256E+03 0.19809614109764E+03 0.20490002812256E+03 0.38506244680654E+03 0.22457793647210E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36879466848406E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19463788009093E+00 0.00000000000000E+00 0.00000000000000E+00 0.19463788009093E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20245556609512E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20245556609512E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936784774303E+00 0.20454959540411E+00 0.34307145036803E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    972.11749733
 0.11589823108547E+00 0.32107957532485E+03 0.45572640065249E+03 0.45052462436024E+03 0.44846093244563E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14865568120826E+00 0.00000000000000E+00 -.23601986296010E+02
 0.30368756886604E-02 0.11310578523067E+01 0.26342862929397E+04 0.98785735985240E+03 0.70730245881627E+01
 0.26523842205610E+01 0.35255263974093E+03 0.29915293183508E+03 0.34487662870307E+03 0.37473798584458E+03
 0.29915049323838E+03 0.29915091876881E+03 0.34079595823225E+03 0.37471605162456E+03 0.29915041336452E+03
 0.29915091677096E+03 0.34487662870307E+03 0.37473798584458E+03 0.29915049323838E+03 0.29915091876881E+03
 0.34079595823225E+03 0.37471605162456E+03 0.29915041336452E+03 0.29915091677096E+03 0.41915502700194E+03
 0.34496559128263E+03 0.20220467885717E+04 0.18543280319097E+04 0.56268480123139E+03 0.89394968901171E+03
 0.32845146377416E+03 0.12099237039938E+04 0.11405059672990E+04 0.10884358685667E+04 0.17137115290965E+04
 0.11054090400717E+04 0.11401849493341E+04 0.10096921520401E+04 0.17135815719896E+04 0.12099237039938E+04
 0.11405059672990E+04 0.10884358685667E+04 0.17137115290965E+04 0.11054090400717E+04 0.11401849493341E+04
 0.10096921520401E+04 0.17135815719896E+04 0.16725598165217E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49426589007671E+03 0.12948783885086E+01
 0.12948783885086E+01 0.37084925155268E+01 0.00000000000000E+00 0.34328107609206E+03 0.34328107609206E+03
 0.34328107609206E+03 0.34328107609206E+03 0.00000000000000E+00 0.00000000000000E+00 0.15386563573334E+00
 0.00000000000000E+00 -.15551016744985E+02 0.10000000000000E-02 0.14051756071070E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56932385956163E+01 0.21349644733561E+01 0.34496826676537E+03 0.41915191667864E+03
 0.31424681251308E+03 0.31424681251308E+03 0.29915013319305E+03 0.29915013071003E+03 0.31424214087282E+03
 0.31424214087282E+03 0.29915013322400E+03 0.29915013074041E+03 0.31424681251308E+03 0.31424681251308E+03
 0.29915013319305E+03 0.29915013071003E+03 0.31424214087282E+03 0.31424214087282E+03 0.29915013322400E+03
 0.29915013074041E+03 0.31206106117217E+03 0.29915056350953E+03 -.32730664188265E+02 -.36020071521384E+02
 0.21639849079383E+03 0.48877180353593E+03 0.27129132028813E+03 0.20702939746895E+03 0.19927289462272E+03
 0.20702939746895E+03 0.38671224713841E+03 0.20705397777908E+03 0.19907952027283E+03 0.20705397777908E+03
 0.38655525861801E+03 0.20702939746895E+03 0.19927289462272E+03 0.20702939746895E+03 0.38671224713841E+03
 0.20705397777908E+03 0.19907952027283E+03 0.20705397777908E+03 0.38655525861801E+03 0.22465525120256E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36902858692982E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19452913274977E+00 0.00000000000000E+00 0.00000000000000E+00 0.19452913274977E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20232519681031E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20232519681031E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936796767757E+00 0.20442484487558E+00 0.34328107609206E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    980.71844716
 0.11581023398118E+00 0.32122540082289E+03 0.45598656178391E+03 0.45078432125642E+03 0.44871955534448E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14853861131470E+00 0.00000000000000E+00 -.23603185073602E+02
 0.30391830199195E-02 0.11320415861875E+01 0.26322863570789E+04 0.98710738390459E+03 0.70668781938853E+01
 0.26500793227070E+01 0.35285327540113E+03 0.29915313852848E+03 0.34514039496471E+03 0.37511038984261E+03
 0.29915053286880E+03 0.29915099234005E+03 0.34104939409967E+03 0.37508851869361E+03 0.29915044689310E+03
 0.29915099020263E+03 0.34514039496471E+03 0.37511038984261E+03 0.29915053286880E+03 0.29915099234005E+03
 0.34104939409966E+03 0.37508851869361E+03 0.29915044689310E+03 0.29915099020263E+03 0.41956500352620E+03
 0.34539464186543E+03 0.20244393981692E+04 0.18556863216699E+04 0.56159785641868E+03 0.89119717160230E+03
 0.32679132590153E+03 0.12117074914232E+04 0.11416080473549E+04 0.10894626908329E+04 0.17136772354595E+04
 0.11073448520782E+04 0.11412880094110E+04 0.10109702417418E+04 0.17135475768801E+04 0.12117074914232E+04
 0.11416080473549E+04 0.10894626908329E+04 0.17136772354595E+04 0.11073448520782E+04 0.11412880094110E+04
 0.10109702417418E+04 0.17135475768802E+04 0.16722431580601E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49452457890093E+03 0.12948783973417E+01
 0.12948783973417E+01 0.37428963148416E+01 0.00000000000000E+00 0.34344930976160E+03 0.34344930976160E+03
 0.34344930976160E+03 0.34344930976160E+03 0.00000000000000E+00 0.00000000000000E+00 0.15362297822236E+00
 0.00000000000000E+00 -.15537260614428E+02 0.10000000000000E-02 0.14089738562668E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56778910158039E+01 0.21292091309264E+01 0.34539736317630E+03 0.41956185303741E+03
 0.31435761993238E+03 0.31435761993238E+03 0.29915014414049E+03 0.29915014145339E+03 0.31435289122338E+03
 0.31435289122338E+03 0.29915014417361E+03 0.29915014148590E+03 0.31435761993238E+03 0.31435761993238E+03
 0.29915014414049E+03 0.29915014145339E+03 0.31435289122338E+03 0.31435289122338E+03 0.29915014417361E+03
 0.29915014148590E+03 0.31215784653078E+03 0.29915060431281E+03 -.34887517118652E+02 -.38870032940007E+02
 0.21731291226212E+03 0.49038817828541E+03 0.27198870146198E+03 0.20874314225533E+03 0.20006198045972E+03
 0.20874314225533E+03 0.38791253606545E+03 0.20876807426709E+03 0.19986740358664E+03 0.20876807426709E+03
 0.38775479867908E+03 0.20874314225533E+03 0.20006198045972E+03 0.20874314225533E+03 0.38791253606545E+03
 0.20876807426709E+03 0.19986740358664E+03 0.20876807426709E+03 0.38775479867908E+03 0.22471918721617E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36921601599147E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19444394094067E+00 0.00000000000000E+00 0.00000000000000E+00 0.19444394094067E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20222197540036E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20222197540036E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936805393278E+00 0.20432482647955E+00 0.34344930976160E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    990.14499388
 0.11571925721186E+00 0.32138060876438E+03 0.45626964370127E+03 0.45106655739163E+03 0.44900041877530E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14841324524050E+00 0.00000000000000E+00 -.23611586087686E+02
 0.30415721589763E-02 0.11330826077484E+01 0.26302187098835E+04 0.98633201620630E+03 0.70603854876013E+01
 0.26476445578505E+01 0.35318034120688E+03 0.29915338132689E+03 0.34542727014803E+03 0.37551647595057E+03
 0.29915057998483E+03 0.29915107975922E+03 0.34132483975018E+03 0.37549467191490E+03 0.29915048679187E+03
 0.29915107745824E+03 0.34542727014803E+03 0.37551647595057E+03 0.29915057998483E+03 0.29915107975922E+03
 0.34132483975018E+03 0.37549467191490E+03 0.29915048679187E+03 0.29915107745824E+03 0.42001800787073E+03
 0.34586718473611E+03 0.20270403934479E+04 0.18571355247704E+04 0.56029666339666E+03 0.88801251048237E+03
 0.32491436376873E+03 0.12136501859411E+04 0.11427922391069E+04 0.10905593809404E+04 0.17136186674619E+04
 0.11094549189339E+04 0.11424732011690E+04 0.10123444986155E+04 0.17134892809447E+04 0.12136501859411E+04
 0.11427922391069E+04 0.10905593809404E+04 0.17136186674619E+04 0.11094549189339E+04 0.11424732011690E+04
 0.10123444986155E+04 0.17134892809447E+04 0.16718328049607E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49480562530614E+03 0.12948784592440E+01
 0.12948784592440E+01 0.37806025017141E+01 0.00000000000000E+00 0.34363410545644E+03 0.34363410545644E+03
 0.34363410545644E+03 0.34363410545644E+03 0.00000000000000E+00 0.00000000000000E+00 0.15336373696695E+00
 0.00000000000000E+00 -.15529628457692E+02 0.10000000000000E-02 0.14130065612140E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56616863782477E+01 0.21231323918429E+01 0.34586993415877E+03 0.42001483472804E+03
 0.31447778163851E+03 0.31447778163851E+03 0.29915016026388E+03 0.29915015425406E+03 0.31447299104070E+03
 0.31447299104070E+03 0.29915016030023E+03 0.29915015428905E+03 0.31447778163851E+03 0.31447778163851E+03
 0.29915016026388E+03 0.29915015425406E+03 0.31447299104070E+03 0.31447299104070E+03 0.29915016030023E+03
 0.29915015428905E+03 0.31226275796084E+03 0.29915065235180E+03 -.37286336814435E+02 -.42069457102310E+02
 0.21832488435668E+03 0.49218638420895E+03 0.27276987543049E+03 0.21064421594458E+03 0.20093601917802E+03
 0.21064421594458E+03 0.38925084005676E+03 0.21066952922076E+03 0.20074004853153E+03 0.21066952922076E+03
 0.38909220176198E+03 0.21064421594458E+03 0.20093601917802E+03 0.21064421594458E+03 0.38925084005676E+03
 0.21066952922076E+03 0.20074004853153E+03 0.21066952922076E+03 0.38909220176198E+03 0.22478427792725E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36942180430139E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19440526776598E+00 0.00000000000000E+00 0.00000000000000E+00 0.19440526776598E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20211678113998E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20211678113998E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936789898083E+00 0.20421480363700E+00 0.34363410545644E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
   1000.00000000
 0.11563046062726E+00 0.32154491791084E+03 0.45656443502607E+03 0.45136031204016E+03 0.44929274582893E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14828551472354E+00 0.00000000000000E+00 -.23600107712279E+02
 0.30439076557074E-02 0.11341263730878E+01 0.26282006239578E+04 0.98557523398418E+03 0.70538876353072E+01
 0.26452078632402E+01 0.35352179387219E+03 0.29915365024022E+03 0.34572710243565E+03 0.37593830466084E+03
 0.29915063269660E+03 0.29915117751339E+03 0.34161323796928E+03 0.37591656930358E+03 0.29915053146408E+03
 0.29915117503163E+03 0.34572710243565E+03 0.37593830466084E+03 0.29915063269660E+03 0.29915117751339E+03
 0.34161323796928E+03 0.37591656930358E+03 0.29915053146408E+03 0.29915117503163E+03 0.42047806260946E+03
 0.34634657916814E+03 0.20297304473677E+04 0.18586386310776E+04 0.55909973427293E+03 0.88498113114500E+03
 0.32308589820071E+03 0.12156645235269E+04 0.11440245058898E+04 0.10917034148992E+04 0.17135676755143E+04
 0.11116405347340E+04 0.11437065281809E+04 0.10137708842680E+04 0.17134386035143E+04 0.12156645235269E+04
 0.11440245058898E+04 0.10917034148992E+04 0.17135676755143E+04 0.11116405347340E+04 0.11437065281809E+04
 0.10137708842680E+04 0.17134386035143E+04 0.16714773648007E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49509813217277E+03 0.12948783746665E+01
 0.12948783746665E+01 0.38200225261898E+01 0.00000000000000E+00 0.34382828410035E+03 0.34382828410035E+03
 0.34382828410035E+03 0.34382828410035E+03 0.00000000000000E+00 0.00000000000000E+00 0.15309997774094E+00
 0.00000000000000E+00 -.15501128436992E+02 0.10000000000000E-02 0.14170798939223E+01 0.80000000000000E+04
 0.30000000000000E+04 0.56454121142435E+01 0.21170295428413E+01 0.34634936175784E+03 0.42047486116626E+03
 0.31460514074572E+03 0.31460514074572E+03 0.29915016866722E+03 0.29915016860131E+03 0.31460028460178E+03
 0.31460028460178E+03 0.29915016870494E+03 0.29915016863902E+03 0.31460514074572E+03 0.31460514074572E+03
 0.29915016866722E+03 0.29915016860131E+03 0.31460028460178E+03 0.31460028460178E+03 0.29915016870494E+03
 0.29915016863902E+03 0.31237407080283E+03 0.29915070565822E+03 -.39679089318712E+02 -.45289624524040E+02
 0.21937479073419E+03 0.49404983264181E+03 0.27357816795395E+03 0.21257651198696E+03 0.20184274053284E+03
 0.21257651198696E+03 0.39063579128980E+03 0.21260223206174E+03 0.20164545528639E+03 0.21260223206174E+03
 0.39047636003690E+03 0.21257651198696E+03 0.20184274053284E+03 0.21257651198696E+03 0.39063579128980E+03
 0.21260223206174E+03 0.20164545528639E+03 0.21260223206174E+03 0.39047636003690E+03 0.22487405558486E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.36963698967112E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19421196652573E+00 0.00000000000000E+00 0.00000000000000E+00 0.19421196652573E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20199520283966E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20199520283966E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.19936842163220E+00 0.20410006485602E+00 0.34382828410035E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
