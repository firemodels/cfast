#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-49 0 MONOZONE(1=OUI,0=NON)                                                                     
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
3.000000 2.000000 0.300000 0.560000                                                                 
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
Plenum-LLNL49 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.176000                                                                                   
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
#CONDINIT 500.000000 10.000000 26.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-49           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-49           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-49           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-49           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-49           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-49           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-49           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-49           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-49           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-49           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-49           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-49           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-49           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-49           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-49           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-49           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-49           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-49           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL49     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL49     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL49     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL49     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL49     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL49     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL49     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL49     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL49     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL49     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL49     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL49     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL49     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL49     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL49     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL49     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL49     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL49     #HEAT_POWER#W#Total sprinkling power
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
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.72303047343164E-12
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
 0.00000000000000E+00 -.61628633304526E-07 0.99965591098870E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.57789407619281E-03 0.57789407619281E-03
 0.70978627299668E-03 0.71333520436166E-03 0.00000000000000E+00 0.50513708638344E-03 0.55722721601200E-03
 0.50513708638344E-03 0.55722721601200E-03 0.50074052511886E-03 0.55721956312448E-03 0.50074052511886E-03
 0.55721956312448E-03 0.50513708638344E-03 0.55722721601200E-03 0.50513708638344E-03 0.55722721601200E-03
 0.50074052523620E-03 0.55721956306581E-03 0.50074052523620E-03 0.55721956306581E-03 0.46090856590340E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47284631688618E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47284631688618E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17599973551909E+00 0.20711800208666E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     10.01794992
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.22999999999993E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14494455885932E+02
 0.99985695084248E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000158066E+03 0.29915000000000E+03 0.29915000218159E+03 0.29915000218159E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000217038E+03 0.29915000217038E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000218159E+03 0.29915000218159E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000217038E+03 0.29915000217038E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000771152E+03
 0.29915000636334E+03 0.50949230886572E-03 0.50949230886572E-03 0.67296747121646E-03 0.67633230857254E-03
 .00000000000000E+00 0.47753424971411E-03 0.56709196956556E-03 0.47753424971411E-03 0.56709196956556E-03
 0.47506800915081E-03 0.56714627119510E-03 0.47506800915081E-03 0.56714627119510E-03 0.47753424971411E-03
 0.56709196956556E-03 0.47753424971411E-03 0.56709196956556E-03 0.47506800909213E-03 0.56714627119510E-03
 0.47506800909213E-03 0.56714627119510E-03 0.57052656027141E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999929710E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16020173005958E+02 0.99951488412933E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915000633928E+03 0.29915000774081E+03
 0.29915000234279E+03 0.29915000234279E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000232229E+03
 0.29915000232229E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000234279E+03 0.29915000234279E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000232229E+03 0.29915000232229E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000225657E+03 0.29915000000000E+03 0.55697008349725E-03 0.55697008349725E-03
 0.72207141128405E-03 0.72568176834047E-03 .00000000000000E+00 0.51279090346364E-03 0.56354652776052E-03
 0.51279090346364E-03 0.56354652776052E-03 0.50829315015366E-03 0.56365076774778E-03 0.50829315015366E-03
 0.56365076774778E-03 0.51279090346364E-03 0.56354652776052E-03 0.51279090346364E-03 0.56354652776052E-03
 0.50829315015366E-03 0.56365076768910E-03 0.50829315015366E-03 0.56365076768910E-03 0.46113398508295E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20653166135458E+00 0.00000000000000E+00 0.00000000000000E+00 0.20653166135458E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653153009981E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653153009981E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17552905884716E+00 0.20653143529783E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     30.00000000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14494455811983E+02
 0.99985695084321E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000276206E+03 0.29915000000000E+03 0.29915000378627E+03 0.29915000378627E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000376660E+03 0.29915000376660E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000378627E+03 0.29915000378627E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000376660E+03 0.29915000376660E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001292363E+03
 0.29915001069067E+03 0.51809795515929E-03 0.51809795515929E-03 0.65311004515110E-03 0.65637559537686E-03
 .00000000000000E+00 0.48201937971716E-03 0.57582811216285E-03 0.48201937971716E-03 0.57582811216285E-03
 0.47950367240923E-03 0.57592841061937E-03 0.47950367240923E-03 0.57592841061937E-03 0.48201937977583E-03
 0.57582811216285E-03 0.48201937977583E-03 0.57582811216285E-03 0.47950367252658E-03 0.57592841061937E-03
 0.47950367252658E-03 0.57592841061937E-03 0.57048151022971E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999936069E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16020172929401E+02 0.99954593663387E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001067089E+03 0.29915001294755E+03
 0.29915000406564E+03 0.29915000406564E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000402992E+03
 0.29915000402992E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000406564E+03 0.29915000406564E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000402992E+03 0.29915000402992E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000392061E+03 0.29915000000000E+03 0.54314499066081E-03 0.54314499066081E-03
 0.72978690341918E-03 0.73343583793627E-03 .00000000000000E+00 0.51756307017058E-03 0.56748976558304E-03
 0.51756307017058E-03 0.56748976558304E-03 0.51300708426146E-03 0.56767709449089E-03 0.51300708426146E-03
 0.56767709449089E-03 0.51756307017058E-03 0.56748976552437E-03 0.51756307017058E-03 0.56748976552437E-03
 0.51300708432013E-03 0.56767709454957E-03 0.51300708432013E-03 0.56767709454957E-03 0.46110160160890E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20653166082780E+00 0.00000000000000E+00 0.00000000000000E+00 0.20653166082780E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152992146E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152992146E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17552905884941E+00 0.20653143530063E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     40.00000000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14494455811466E+02
 0.99985695084321E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000322604E+03 0.29915000000000E+03 0.29915000441188E+03 0.29915000441188E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000438890E+03 0.29915000438890E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000441188E+03 0.29915000441188E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000438890E+03 0.29915000438890E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001486962E+03
 0.29915001231089E+03 0.52128883234217E-03 0.52128883234217E-03 0.64583826469854E-03 0.64906745602203E-03
 .00000000000000E+00 0.48365755130664E-03 0.57902369057369E-03 0.48365755130664E-03 0.57902369057369E-03
 0.48112539492276E-03 0.57914202462244E-03 0.48112539492276E-03 0.57914202462244E-03 0.48365755124796E-03
 0.57902369057369E-03 0.48365755124796E-03 0.57902369057369E-03 0.48112539480541E-03 0.57914202456377E-03
 0.48112539480541E-03 0.57914202456377E-03 0.57047232243462E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999939035E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16020172928847E+02 0.99956040989058E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001229603E+03 0.29915001488750E+03
 0.29915000473734E+03 0.29915000473734E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000469568E+03
 0.29915000469568E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000473734E+03 0.29915000473734E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000469568E+03 0.29915000469568E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000457071E+03 0.29915000000000E+03 0.53809961147463E-03 0.53809961147463E-03
 0.73262369188225E-03 0.73628681034166E-03 .00000000000000E+00 0.51931796376980E-03 0.56893969267344E-03
 0.51931796376980E-03 0.56893969267344E-03 0.51474318626762E-03 0.56915942805396E-03 0.51474318626762E-03
 0.56915942805396E-03 0.51931796376980E-03 0.56893969273211E-03 0.51931796376980E-03 0.56893969273211E-03
 0.51474318620894E-03 0.56915942811264E-03 0.51474318620894E-03 0.56915942811264E-03 0.46109915706307E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20653166082407E+00 0.00000000000000E+00 0.00000000000000E+00 0.20653166082407E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152991948E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152991948E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17552905884943E+00 0.20653143530065E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     40.00025000
 0.30000000000000E+01 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000000000E+03
 0.22999999999995E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.14494455811461E+02
 0.99985695084321E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29915000322605E+03 0.29915000000000E+03 0.29915000441189E+03 0.29915000441189E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000438892E+03 0.29915000438892E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000441189E+03 0.29915000441189E+03 0.29915000000000E+03 0.29915000000000E+03
 0.29915000438892E+03 0.29915000438892E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915001486967E+03
 0.29915001231093E+03 0.52129507602303E-03 0.52129507602303E-03 0.64584692167184E-03 0.64907615628020E-03
 .00000000000000E+00 0.48366434651313E-03 0.57902865524314E-03 0.48366434651313E-03 0.57902865524314E-03
 0.48113224410836E-03 0.57914698987863E-03 0.48113224410836E-03 0.57914698987863E-03 0.48366434657180E-03
 0.57902865524314E-03 0.48366434657180E-03 0.57902865524314E-03 0.48113224410836E-03 0.57914698987863E-03
 0.48113224410836E-03 0.57914698987863E-03 0.57047999909407E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29915000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29915000000000E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000000000E+03 0.22999999939035E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.16020172928676E+02 0.99956041025207E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915001229607E+03 0.29915001488755E+03
 0.29915000473736E+03 0.29915000473736E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000469570E+03
 0.29915000469570E+03 0.29915000000000E+03 0.29915000000000E+03 0.29915000473736E+03 0.29915000473736E+03
 0.29915000000000E+03 0.29915000000000E+03 0.29915000469570E+03 0.29915000469570E+03 0.29915000000000E+03
 0.29915000000000E+03 0.29915000457072E+03 0.29915000000000E+03 0.53810766163663E-03 0.53810766163663E-03
 0.73264020857745E-03 0.73630340962033E-03 .00000000000000E+00 0.51932931052958E-03 0.56894881913184E-03
 0.51932931052958E-03 0.56894881913184E-03 0.51475472125019E-03 0.56916855504042E-03 0.51475472125019E-03
 0.56916855504042E-03 0.51932931052958E-03 0.56894881919052E-03 0.51932931052958E-03 0.56894881919052E-03
 0.51475472125019E-03 0.56916855509909E-03 0.51475472125019E-03 0.56916855509909E-03 0.46110849954702E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29915000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20653166082407E+00 0.00000000000000E+00 0.00000000000000E+00 0.20653166082407E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152990770E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20653152990770E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17552905884943E+00 0.20653143530066E+00 0.29915000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     50.00296065
 0.23581698191266E+01 0.29918091895582E+03 0.34150795155952E+03 0.30823650795311E+03 0.30733023519775E+03
 0.22999999999504E+00 0.00000000000000E+00 0.22514040546127E+00 0.00000000000000E+00 0.21454986365389E+02
 0.10001083770227E-02 0.85125000398251E-01 0.79991330777725E+04 0.29996749041647E+04 0.93979441557387E+02
 0.35242290584020E+02 0.30032961218733E+03 0.29914999999996E+03 0.30033803310776E+03 0.30105033859692E+03
 0.29915000000001E+03 0.29915000000001E+03 0.30004614784835E+03 0.30103820653546E+03 0.29915000000001E+03
 0.29915000000001E+03 0.30033803310776E+03 0.30105033859692E+03 0.29915000000001E+03 0.29915000000001E+03
 0.30004614784835E+03 0.30103820653546E+03 0.29915000000001E+03 0.29915000000001E+03 0.30419271290745E+03
 0.29915522475016E+03 0.54290093048947E+03 0.54073127487599E+03 0.31906765208165E+03 0.68837007792607E+03
 0.36770708758401E+03 0.39824074690050E+03 0.21293857263412E+03 0.39646000815968E+03 0.60296927672552E+03
 0.29816834963523E+03 0.20818340732158E+03 0.29695120553949E+03 0.59835017342749E+03 0.39824074690050E+03
 0.21293857263412E+03 0.39646000815968E+03 0.60296927672552E+03 0.29816834963523E+03 0.20818340732158E+03
 0.29695120553949E+03 0.59835017342749E+03 0.54035923900890E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.37175563110860E+03 0.12945464930890E+01
 0.12945464930890E+01 0.20027876938449E-01 0.13151898114967E+01 0.29916662193467E+03 0.30614476683187E+03
 0.30002637678391E+03 0.30000914594942E+03 0.22999999993379E+00 0.00000000000000E+00 0.22908282346630E+00
 0.00000000000000E+00 0.20044084443665E+02 0.99986641181925E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29915521294268E+03 0.30420518658669E+03
 0.29915254147779E+03 0.29935908214095E+03 0.29915000000001E+03 0.29915000000001E+03 0.29915256101699E+03
 0.29935909280333E+03 0.29915000000001E+03 0.29915000000001E+03 0.29915254147779E+03 0.29935908214095E+03
 0.29915000000001E+03 0.29915000000001E+03 0.29915256101699E+03 0.29935909280333E+03 0.29915000000001E+03
 0.29915000000001E+03 0.29923694898623E+03 0.29914999999996E+03 0.73898806909852E+00 0.74148389059758E+00
 0.58777165943959E+00 0.42054914880893E+02 0.41464204363156E+02 0.87083890811046E+00 -.32710415954649E+00
 0.87715226045483E+00 0.58920402594320E+02 0.87685193974220E+00 -.32316259560374E+00 0.88315411907182E+00
 0.58924240805212E+02 0.87083890811046E+00 -.32710415954649E+00 0.87715226045483E+00 0.58920402594320E+02
 0.87685193974208E+00 -.32316259560385E+00 0.88315411907171E+00 0.58924240805212E+02 0.12680366424306E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31319108643115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23866171141770E+00 0.00000000000000E+00 0.00000000000000E+00 0.23866171141770E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18740867431280E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18740867431280E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17658682637271E+00 0.20783847018978E+00 0.29916662193467E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     60.00896022
 0.18709340242032E+01 0.29922302004293E+03 0.36929069540340E+03 0.32559336279382E+03 0.32223306752613E+03
 0.22999999994485E+00 0.00000000000000E+00 0.22147866374774E+00 0.00000000000000E+00 0.10918584131773E+02
 0.99986369958067E-03 0.14408599998562E+00 0.80000000000000E+04 0.30000000000000E+04 0.55522396352169E+02
 0.20820898632063E+02 0.30136084558554E+03 0.29914999999999E+03 0.30153745488769E+03 0.30348275391797E+03
 0.29915000000002E+03 0.29915000000002E+03 0.30095055349046E+03 0.30345462612009E+03 0.29915000000002E+03
 0.29915000000002E+03 0.30153745488769E+03 0.30348275391797E+03 0.29915000000002E+03 0.29915000000002E+03
 0.30095055349046E+03 0.30345462612009E+03 0.29915000000002E+03 0.29915000000002E+03 0.31087282797918E+03
 0.29916491541759E+03 0.59252132360218E+03 0.58755795502581E+03 0.34208465307611E+03 0.95136187804295E+03
 0.60756680170145E+03 0.44911602439927E+03 0.29040339463119E+03 0.44463103535507E+03 0.86831870078795E+03
 0.33951398000568E+03 0.28468106572705E+03 0.33645515372560E+03 0.86289392169923E+03 0.44911602439927E+03
 0.29040339463119E+03 0.44463103535507E+03 0.86831870078795E+03 0.33951398000568E+03 0.28468106572705E+03
 0.33645515372560E+03 0.86289392169923E+03 0.71511245694783E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39722663687344E+03 0.12946240870668E+01
 0.12946240870668E+01 0.60051875185629E-01 0.11290963435828E+01 0.29915905432663E+03 0.31112764956255E+03
 0.30211851815008E+03 0.30203199981455E+03 0.22999999995475E+00 0.00000000000000E+00 0.22831666320207E+00
 0.00000000000000E+00 0.11029205621886E+02 0.99980273139378E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29916486305852E+03 0.31091694406778E+03
 0.29915652777518E+03 0.29958754766754E+03 0.29915000000002E+03 0.29915000000002E+03 0.29915653896900E+03
 0.29958760676165E+03 0.29915000000002E+03 0.29915000000002E+03 0.29915652777518E+03 0.29958754766754E+03
 0.29915000000002E+03 0.29915000000002E+03 0.29915653896900E+03 0.29958760676165E+03 0.29915000000002E+03
 0.29915000000002E+03 0.29939545381687E+03 0.29914999999999E+03 0.13831985817751E+01 0.13810517959435E+01
 0.21465887610501E+00 0.80597928102060E+02 0.80382195931574E+02 0.14227268209719E+01 -.90215963611613E+00
 0.14234662452993E+01 0.85902465460136E+02 0.14210881415651E+01 -.88544684207660E+00 0.14218234530976E+01
 0.85918669879689E+02 0.14227268209719E+01 -.90215963611620E+00 0.14234662452993E+01 0.85902465460136E+02
 0.14210881415652E+01 -.88544684207666E+00 0.14218234530977E+01 0.85918669879689E+02 0.26369041421583E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32225174864237E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17579639347406E+00 0.00000000000000E+00 0.00000000000000E+00 0.17579639347406E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17486484011480E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17486484011480E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17632302286263E+00 0.20751476253363E+00 0.29915905432663E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     70.02316633
 0.15105534376709E+01 0.29930269739566E+03 0.38351091915380E+03 0.34111057953483E+03 0.33592283400895E+03
 0.22999999995725E+00 0.00000000000000E+00 0.21909308535025E+00 0.00000000000000E+00 0.40346796321696E+01
 0.99952962173216E-03 0.17887264988515E+00 0.80000000000000E+04 0.30000000000000E+04 0.44724556857275E+02
 0.16771708821478E+02 0.30219518218737E+03 0.29915000000002E+03 0.30247004612864E+03 0.30567285761258E+03
 0.29915000000004E+03 0.29915000000004E+03 0.30167866984044E+03 0.30563480606753E+03 0.29915000000004E+03
 0.29915000000004E+03 0.30247004612864E+03 0.30567285761258E+03 0.29915000000004E+03 0.29915000000004E+03
 0.30167866984044E+03 0.30563480606753E+03 0.29915000000004E+03 0.29915000000004E+03 0.31645409946695E+03
 0.29917818852510E+03 0.66243635474429E+03 0.65501367162543E+03 0.37343202390073E+03 0.10832992840116E+04
 0.70800009999138E+03 0.49431644783978E+03 0.36549910393826E+03 0.48745209184054E+03 0.10259439910568E+04
 0.38003203102667E+03 0.35977491744429E+03 0.37531767462207E+03 0.10206139382341E+04 0.49431644783978E+03
 0.36549910393826E+03 0.48745209184054E+03 0.10259439910568E+04 0.38003203102667E+03 0.35977491744429E+03
 0.37531767462207E+03 0.10206139382341E+04 0.85589143742201E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41022256949544E+03 0.19545678902201E+01
 0.19545678902201E+01 0.10010869965484E+00 0.96725303642584E+00 0.29915450143239E+03 0.31474039138689E+03
 0.30469005846430E+03 0.30451013181083E+03 0.22999999994878E+00 0.00000000000000E+00 0.22763872550505E+00
 0.00000000000000E+00 0.54120467985825E+01 0.99976250760180E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29917810827804E+03 0.31649876852533E+03
 0.29916157224958E+03 0.29981029099888E+03 0.29915000000004E+03 0.29915000000004E+03 0.29916154858336E+03
 0.29981042654361E+03 0.29915000000004E+03 0.29915000000004E+03 0.29916157224958E+03 0.29981029099888E+03
 0.29915000000004E+03 0.29915000000004E+03 0.29916154858336E+03 0.29981042654361E+03 0.29915000000004E+03
 0.29915000000004E+03 0.29957570830292E+03 0.29915000000002E+03 0.21743867257573E+01 0.21619991681881E+01
 -.10620186041897E+00 0.11049150939603E+03 0.11059824226575E+03 0.21021406434162E+01 -.13102637457214E+01
 0.20993445181161E+01 0.10410808156281E+03 0.20923514958827E+01 -.12809396511561E+01 0.20895671722848E+01
 0.10413629267029E+03 0.21021406434162E+01 -.13102637457214E+01 0.20993445181161E+01 0.10410808156281E+03
 0.20923514958827E+01 -.12809396511561E+01 0.20895671722848E+01 0.10413629267029E+03 0.38536060171718E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32854355504323E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12962668901818E+00 0.00000000000000E+00 0.00000000000000E+00 0.12962668901818E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17140768270941E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17140768270941E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17615844706397E+00 0.20520721017154E+00 0.30222396665404E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     80.00366993
 0.12278595203622E+01 0.29942472014699E+03 0.39345452588696E+03 0.35496939516175E+03 0.34864331714564E+03
 0.22999999995134E+00 0.00000000000000E+00 0.21703413207803E+00 0.00000000000000E+00 -.68901915734757E+00
 0.99907571294084E-03 0.20782205101674E+00 0.80000000000000E+04 0.30000000000000E+04 0.38494471404075E+02
 0.14435426776528E+02 0.30308806473377E+03 0.29915000000003E+03 0.30335046433453E+03 0.30771752277388E+03
 0.29915000000005E+03 0.29915000000005E+03 0.30238502243773E+03 0.30767373398044E+03 0.29915000000005E+03
 0.29915000000005E+03 0.30335046433453E+03 0.30771752277388E+03 0.29915000000005E+03 0.29915000000005E+03
 0.30238502243773E+03 0.30767373398044E+03 0.29915000000005E+03 0.29915000000005E+03 0.32135772545464E+03
 0.29919466130399E+03 0.76325094081262E+03 0.75308746237012E+03 0.39930806092286E+03 0.11706475272730E+04
 0.76934292604550E+03 0.54131228592837E+03 0.43076285627075E+03 0.53208380745718E+03 0.11455546838324E+04
 0.42289258538509E+03 0.42553314531565E+03 0.41650772314258E+03 0.11407739132308E+04 0.54131228592837E+03
 0.43076285627075E+03 0.53208380745718E+03 0.11455546838324E+04 0.42289258538509E+03 0.42553314531565E+03
 0.41650772314258E+03 0.11407739132308E+04 0.98634732520462E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42121137609971E+03 0.17289841523296E+01
 0.17289841523296E+01 0.14003071405392E+00 0.85425629229189E+00 0.29915182330172E+03 0.31767951484003E+03
 0.30712791678787E+03 0.30685617121876E+03 0.22999999993583E+00 0.00000000000000E+00 0.22698053873803E+00
 0.00000000000000E+00 0.17790346801055E+01 0.99973560061832E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29919455824195E+03 0.32139390436008E+03
 0.29916763359556E+03 0.30003190224187E+03 0.29915000000005E+03 0.29915000000005E+03 0.29916755793111E+03
 0.30003212909962E+03 0.29915000000005E+03 0.29915000000005E+03 0.29916763359556E+03 0.30003190224187E+03
 0.29915000000005E+03 0.29915000000005E+03 0.29916755793111E+03 0.30003212909962E+03 0.29915000000005E+03
 0.29915000000005E+03 0.29976495349066E+03 0.29915000000003E+03 0.30097712539731E+01 0.29837596379907E+01
 -.43635429628585E+00 0.13553298300060E+03 0.13597151906837E+03 0.28089961257974E+01 -.16786177195378E+01
 0.28010631462309E+01 0.12022319435503E+03 0.27907890663483E+01 -.16376793339284E+01 0.27829041035185E+01
 0.12026229285977E+03 0.28089961257974E+01 -.16786177195378E+01 0.28010631462309E+01 0.12022319435503E+03
 0.27907890663484E+01 -.16376793339285E+01 0.27829041035186E+01 0.12026229285977E+03 0.49250036462836E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33345237453291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.90682941993216E-01 0.00000000000000E+00 0.00000000000000E+00 0.90682941993216E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16969136994215E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16969136994215E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17605192169192E+00 0.20106855084948E+00 0.30824716428093E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
     90.00552681
 0.10061524952426E+01 0.29959987852426E+03 0.40158626369083E+03 0.36738164505212E+03 0.36043611407681E+03
 0.22999999994781E+00 0.00000000000000E+00 0.21500882386359E+00 0.00000000000000E+00 -.38904548476276E+01
 0.99846006403168E-03 0.23572258320697E+00 0.80000000000000E+04 0.30000000000000E+04 0.33938199264412E+02
 0.12726824724154E+02 0.30403213751236E+03 0.29915000000005E+03 0.30422168802943E+03 0.30971317850400E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30310176698360E+03 0.30966597255820E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30422168802943E+03 0.30971317850400E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30310176698360E+03 0.30966597255820E+03 0.29915000000006E+03 0.29915000000006E+03 0.32595312711087E+03
 0.29922113658682E+03 0.86447466164158E+03 0.85138308393523E+03 0.43250861524977E+03 0.12486478540558E+04
 0.81397669572981E+03 0.58880735323966E+03 0.49766999005505E+03 0.57720686580912E+03 0.12529471353547E+04
 0.46746362679472E+03 0.49294500122907E+03 0.45938275145406E+03 0.12487020019922E+04 0.58880735323966E+03
 0.49766999005505E+03 0.57720686580912E+03 0.12529471353547E+04 0.46746362679472E+03 0.49294500122907E+03
 0.45938275145406E+03 0.12487020019922E+04 0.11096351006452E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43266463092816E+03 0.15660255227772E+01
 0.15660255227772E+01 0.18003814155032E+00 0.76092833112446E+00 0.29915060247131E+03 0.32026124451400E+03
 0.30955212076833E+03 0.30919263182168E+03 0.22999999993527E+00 0.00000000000000E+00 0.22629922663912E+00
 0.00000000000000E+00 -.48755229362800E+00 0.99971730957280E-03 0.12772802015301E-01 0.80000000000000E+04
 0.30000000000000E+04 0.62633085445279E+03 0.23487407041980E+03 0.29922100926639E+03 0.32598314131666E+03
 0.29917649631818E+03 0.30025993591512E+03 0.29915000000006E+03 0.29915000000006E+03 0.29917631038887E+03
 0.30026026382640E+03 0.29915000000006E+03 0.29915000000006E+03 0.29917649631818E+03 0.30025993591512E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29917631038887E+03 0.30026026382640E+03 0.29915000000006E+03
 0.29915000000006E+03 0.29996431968060E+03 0.29915000000005E+03 0.52092191972192E+01 0.51567478413443E+01
 0.10084852722464E+01 0.15977881222867E+03 0.15876528453007E+03 0.43781188271774E+01 -.25500692292745E+00
 0.43629535901886E+01 0.13644995578522E+03 0.43305290417775E+01 -.20343140046621E+00 0.43155016233871E+01
 0.13649888195595E+03 0.43781188271773E+01 -.25500692292740E+00 0.43629535901886E+01 0.13644995578522E+03
 0.43305290417776E+01 -.20343140046626E+00 0.43155016233873E+01 0.13649888195595E+03 0.60417776915730E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33775514329439E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.55548219334981E-01 0.00000000000000E+00 0.00000000000000E+00 0.55548219334981E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16895629096298E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16895629096298E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17598542813798E+00 0.20004143014043E+00 0.30970592074449E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    100.01064172
 0.82815146198997E+00 0.29984044060851E+03 0.40828620102581E+03 0.37834969601375E+03 0.37122277962634E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21298287859561E+00 0.00000000000000E+00 -.58055736974641E+01
 0.99764014196261E-03 0.26337539701782E+00 0.80000000000000E+04 0.30000000000000E+04 0.30374894886096E+02
 0.11390585582286E+02 0.30501970672975E+03 0.29915000000006E+03 0.30509607612198E+03 0.31166788100243E+03
 0.29915000000006E+03 0.29915000000006E+03 0.30383753998401E+03 0.31161868510458E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30509607612198E+03 0.31166788100243E+03 0.29915000000006E+03 0.29915000000006E+03
 0.30383753998401E+03 0.31161868510458E+03 0.29915000000006E+03 0.29915000000006E+03 0.33032272106906E+03
 0.29927572228904E+03 0.96490386616855E+03 0.94880694063910E+03 0.46864661352464E+03 0.13129227351134E+04
 0.84193288852110E+03 0.63621166447664E+03 0.56395791353854E+03 0.62226988017193E+03 0.13486375104044E+04
 0.51284607692625E+03 0.55969901772289E+03 0.50308062296851E+03 0.13448747497520E+04 0.63621166447664E+03
 0.56395791353854E+03 0.62226988017194E+03 0.13486375104044E+04 0.51284607692625E+03 0.55969901772289E+03
 0.50308062296851E+03 0.13448747497520E+04 0.12231388331331E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44198065423779E+03 0.14219273904234E+01
 0.14219273904234E+01 0.22005860120463E+00 0.67837685972469E+00 0.29915107627083E+03 0.32253492246436E+03
 0.31195954903166E+03 0.31152221349677E+03 0.22999999994018E+00 0.00000000000000E+00 0.22557865067165E+00
 0.00000000000000E+00 -.16096926765988E+01 0.99970465082653E-03 0.35044900027465E-01 0.80000000000000E+04
 0.30000000000000E+04 0.22827857958591E+03 0.85604467344717E+02 0.29927545391511E+03 0.33035064065509E+03
 0.29919280239809E+03 0.30050082884859E+03 0.29915000000006E+03 0.29915000000006E+03 0.29919233858902E+03
 0.30050126209126E+03 0.29915000000006E+03 0.29915000000006E+03 0.29919280239809E+03 0.30050082884859E+03
 0.29915000000006E+03 0.29915000000006E+03 0.29919233858902E+03 0.30050126209126E+03 0.29915000000006E+03
 0.29915000000006E+03 0.30017709409094E+03 0.29915000000006E+03 0.91759563232393E+01 0.90639065159925E+01
 0.47373602526184E+01 0.18382245567451E+03 0.17906140862063E+03 0.70873886218022E+01 0.34383289282655E+01
 0.70589874613820E+01 0.15307236212958E+03 0.69856932260942E+01 0.34990225341765E+01 0.69576917667638E+01
 0.15312955974260E+03 0.70873886218023E+01 0.34383289282655E+01 0.70589874613821E+01 0.15307236212958E+03
 0.69856932260942E+01 0.34990225341765E+01 0.69576917667638E+01 0.15312955974260E+03 0.72402278905727E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34155011589815E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.24104360435324E-01 0.13772154494124E-04 0.00000000000000E+00 0.24104360435324E-01 0.13772154494124E-04
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16900403738338E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16900403738338E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17588010136682E+00 0.19916017552506E+00 0.31088670160768E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    110.01223284
 0.68087512443455E+00 0.30015951021120E+03 0.41358513308340E+03 0.38784223805434E+03 0.38091621596174E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21098221017001E+00 0.00000000000000E+00 -.68612265696261E+01
 0.99656926690377E-03 0.29069544811919E+00 0.80000000000000E+04 0.30000000000000E+04 0.27520210762708E+02
 0.10320079036016E+02 0.30604313722017E+03 0.29915000000006E+03 0.30597490317869E+03 0.31357193445152E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30459099079330E+03 0.31352170270720E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30597490317869E+03 0.31357193445152E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30459099079330E+03 0.31352170270720E+03 0.29915000000007E+03 0.29915000000007E+03 0.33443048869586E+03
 0.29935350040994E+03 0.10620290226539E+04 0.10429712651813E+04 0.50356291159107E+03 0.13622961403465E+04
 0.85621541419745E+03 0.68201951096014E+03 0.62603006830985E+03 0.66583297643443E+03 0.14307465011189E+04
 0.55734502922328E+03 0.62218873627570E+03 0.54597061019284E+03 0.14274085055173E+04 0.68201951096014E+03
 0.62603006830985E+03 0.66583297643443E+03 0.14307465011189E+04 0.55734502922328E+03 0.62218873627569E+03
 0.54597061019284E+03 0.14274085055173E+04 0.13244017571646E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44981267237718E+03 0.13092402410868E+01
 0.13092402410868E+01 0.26006496569463E+00 0.60477216207978E+00 0.29915355633466E+03 0.32449039130045E+03
 0.31427504965877E+03 0.31377573889596E+03 0.23000000000000E+00 0.00000000000000E+00 0.22482272605843E+00
 0.00000000000000E+00 -.19990753241079E+01 0.99969251988024E-03 0.55665541986072E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14371547845527E+03 0.53893304420724E+02 0.29935305329706E+03 0.33445854464280E+03
 0.29921534240720E+03 0.30074902528737E+03 0.29915000000007E+03 0.29915000000007E+03 0.29921448557085E+03
 0.30074956359339E+03 0.29915000000007E+03 0.29915000000007E+03 0.29921534240720E+03 0.30074902528737E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29921448557085E+03 0.30074956359339E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30039708789583E+03 0.29915000000006E+03 0.13806037001116E+02 0.13595668576270E+02
 0.92412307080554E+01 0.20541103192842E+03 0.19612359506682E+03 0.10241397834197E+02 0.78726884755481E+01
 0.10193613427291E+02 0.16855152492792E+03 0.10081555240229E+02 0.79408317286495E+01 0.10034609783664E+02
 0.16861532257458E+03 0.10241397834197E+02 0.78726884755481E+01 0.10193613427291E+02 0.16855152492792E+03
 0.10081555240229E+02 0.79408317286508E+01 0.10034609783664E+02 0.16861532257458E+03 0.83969850491523E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34390392280057E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.63028893280020E-02 0.13323729712680E-01 0.00000000000000E+00 0.63028893280020E-02 0.13323729712680E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16958026967338E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16958026967338E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17586503536324E+00 0.19743142018513E+00 0.31358082828280E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    120.06637518
 0.56605724510688E+00 0.30056353037029E+03 0.41806826487922E+03 0.39589679611151E+03 0.38934752649541E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20892820461494E+00 0.00000000000000E+00 -.78868269136548E+01
 0.99521959538448E-03 0.31872549156466E+00 0.80000000000000E+04 0.30000000000000E+04 0.25099969132457E+02
 0.94124884246713E+01 0.30709822166608E+03 0.29915000000007E+03 0.30686306607932E+03 0.31544139574640E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30536482490363E+03 0.31539076819293E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30686306607932E+03 0.31544139574640E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30536482490363E+03 0.31539076819293E+03 0.29915000000007E+03 0.29915000000007E+03 0.33834318885997E+03
 0.29945338181823E+03 0.11539286123025E+04 0.11320375164843E+04 0.53782789028000E+03 0.14036916531542E+04
 0.86317462342283E+03 0.72609955311500E+03 0.68527008842825E+03 0.70781978043989E+03 0.15048903916273E+04
 0.60070881693584E+03 0.68179357029777E+03 0.58785143649367E+03 0.15019184705304E+04 0.72609955311500E+03
 0.68527008842825E+03 0.70781978043989E+03 0.15048903916273E+04 0.60070881693584E+03 0.68179357029777E+03
 0.58785143649367E+03 0.15019184705304E+04 0.14152603219371E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45724489990365E+03 0.12947626051691E+01
 0.12947626051691E+01 0.30028153502191E+00 0.54696773035952E+00 0.29915827820057E+03 0.32610817960476E+03
 0.31628102866845E+03 0.31573644682923E+03 0.23000000000000E+00 0.00000000000000E+00 0.22403952139130E+00
 0.00000000000000E+00 -.24757657068980E+01 0.99967203613593E-03 0.75494994614767E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10596729016039E+03 0.39737733810146E+02 0.29945277877019E+03 0.33837257733542E+03
 0.29924384703535E+03 0.30100125469499E+03 0.29915000000007E+03 0.29915000000007E+03 0.29924250465228E+03
 0.30100189478902E+03 0.29915000000007E+03 0.29915000000007E+03 0.29924384703535E+03 0.30100125469499E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29924250465228E+03 0.30100189478902E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30062017345388E+03 0.29915000000007E+03 0.18841065978684E+02 0.18487489293266E+02
 0.14251303818519E+02 0.22432665288051E+03 0.21000409254290E+03 0.13690062455051E+02 0.12780348253111E+02
 0.13616413689980E+02 0.18249572849548E+03 0.13471391020334E+02 0.12854236145910E+02 0.13399204864603E+02
 0.18256444132179E+03 0.13690062455051E+02 0.12780348253111E+02 0.13616413689980E+02 0.18249572849548E+03
 0.13471391020334E+02 0.12854236145910E+02 0.13399204864603E+02 0.18256444132179E+03 0.94545539911388E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34574811836143E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.38248403003800E-01 0.00000000000000E+00 0.00000000000000E+00 0.38248403003800E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17022680635028E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17022680635028E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17584820563801E+00 0.19149931411714E+00 0.32326221405389E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    130.29216896
 0.48347789255418E+00 0.30104466602182E+03 0.42205398193783E+03 0.40255220559166E+03 0.39637657213426E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20675752837608E+00 0.00000000000000E+00 -.94121377273993E+01
 0.99361405646754E-03 0.34829895126330E+00 0.80000000000000E+04 0.30000000000000E+04 0.22968774298583E+02
 0.86132903619687E+01 0.30817999804307E+03 0.29915000000007E+03 0.30776563364714E+03 0.31729523934423E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30616155782812E+03 0.31724460925611E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30776563364714E+03 0.31729523934423E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30616155782812E+03 0.31724460925611E+03 0.29915000000007E+03 0.29915000000007E+03 0.34213770389957E+03
 0.29957326718931E+03 0.12390590792261E+04 0.12144811255843E+04 0.57178402404577E+03 0.14403131681072E+04
 0.86567022394116E+03 0.76827345367239E+03 0.74285314154156E+03 0.74807143956907E+03 0.15730761078173E+04
 0.64258665399160E+03 0.73969138902653E+03 0.62838832671174E+03 0.15704160522570E+04 0.76827345367240E+03
 0.74285314154156E+03 0.74807143956908E+03 0.15730761078173E+04 0.64258665399160E+03 0.73969138902653E+03
 0.62838832671174E+03 0.15704160522570E+04 0.14960573332817E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46555285782996E+03 0.12947738419795E+01
 0.12947738419795E+01 0.34118471014351E+00 0.50990993606428E+00 0.29916599590366E+03 0.32748662938036E+03
 0.31785931444342E+03 0.31727650886286E+03 0.23000000000000E+00 0.00000000000000E+00 0.22320443373909E+00
 0.00000000000000E+00 -.35637398822500E+01 0.99963550957353E-03 0.95493055792975E-01 0.80000000000000E+04
 0.30000000000000E+04 0.83775725193502E+02 0.31415896947563E+02 0.29957253444881E+03 0.34216923246919E+03
 0.29927765584765E+03 0.30125819646793E+03 0.29915000000007E+03 0.29915000000007E+03 0.29927574553841E+03
 0.30125893613948E+03 0.29915000000007E+03 0.29915000000007E+03 0.29927765584765E+03 0.30125819646793E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29927574553841E+03 0.30125893613948E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30084547376071E+03 0.29915000000007E+03 0.24051647674793E+02 0.23508230408828E+02
 0.19560462861704E+02 0.24127593097159E+03 0.22161766579558E+03 0.17291746393516E+02 0.17967850655102E+02
 0.17187103459985E+02 0.19553121853376E+03 0.17012738104977E+02 0.18046766542633E+02 0.16910361865044E+02
 0.19560413458361E+03 0.17291746393516E+02 0.17967850655102E+02 0.17187103459985E+02 0.19553121853376E+03
 0.17012738104977E+02 0.18046766542633E+02 0.16910361865044E+02 0.19560413458360E+03 0.10401416344188E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34739965623647E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.68164091918074E-01 0.00000000000000E+00 0.00000000000000E+00 0.68164091918074E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17077508624286E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17077508624286E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17581365914538E+00 0.18898990180809E+00 0.32748662938036E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    141.32386051
 0.42686556318287E+00 0.30160913638584E+03 0.42580021439972E+03 0.40812924958016E+03 0.40223375792338E+03
 0.22999999988248E+00 0.00000000000000E+00 0.20431537405994E+00 0.00000000000000E+00 -.11346843434812E+02
 0.99173554017584E-03 0.38155325263923E+00 0.80000000000000E+04 0.30000000000000E+04 0.20966929110585E+02
 0.78625984164695E+01 0.30934239115068E+03 0.29915000000008E+03 0.30873398930112E+03 0.31925464104506E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30702576893651E+03 0.31920430997315E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30873398930112E+03 0.31925464104506E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30702576893651E+03 0.31920430997315E+03 0.29915000000007E+03 0.29915000000007E+03 0.34603921009832E+03
 0.29972455283807E+03 0.13200490735578E+04 0.12927328090882E+04 0.60654937366200E+03 0.14725740991659E+04
 0.86299197863557E+03 0.81005248861739E+03 0.80144945101241E+03 0.78797811194987E+03 0.16417541474428E+04
 0.68431464773738E+03 0.79857454320548E+03 0.66882088760726E+03 0.16393761198189E+04 0.81005248861739E+03
 0.80144945101241E+03 0.78797811194987E+03 0.16417541474428E+04 0.68431464773738E+03 0.79857454320549E+03
 0.66882088760726E+03 0.16393761198189E+04 0.15712779807663E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47247238571570E+03 0.12947880951013E+01
 0.12947880951013E+01 0.38531147635374E+00 0.47252464531064E+00 0.29917919533445E+03 0.32873183640969E+03
 0.31942226891498E+03 0.31881137385977E+03 0.23000000000000E+00 0.00000000000000E+00 0.22225099396136E+00
 0.00000000000000E+00 -.51530092140683E+01 0.99957572243349E-03 0.11731789484377E+00 0.80000000000000E+04
 0.30000000000000E+04 0.68190790592120E+02 0.25571546472045E+02 0.29972370757985E+03 0.34607328833938E+03
 0.29931960057593E+03 0.30153425442422E+03 0.29915000000007E+03 0.29915000000007E+03 0.29931700618997E+03
 0.30153509410106E+03 0.29915000000007E+03 0.29915000000007E+03 0.29931960057593E+03 0.30153425442422E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29931700618997E+03 0.30153509410106E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30108658930186E+03 0.29915000000008E+03 0.29969828671052E+02 0.29167612831030E+02
 0.25706530426828E+02 0.25772881732555E+03 0.23189375424659E+03 0.21409225344257E+02 0.23947700390492E+02
 0.21267212033006E+02 0.20817596829792E+03 0.21066031015526E+02 0.24030386966310E+02 0.20927339497117E+02
 0.20825182934292E+03 0.21409225344257E+02 0.23947700390492E+02 0.21267212033006E+02 0.20817596829792E+03
 0.21066031015526E+02 0.24030386966310E+02 0.20927339497117E+02 0.20825182934292E+03 0.11363752275142E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34895161085524E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.96283409847375E-01 0.00000000000000E+00 0.00000000000000E+00 0.96283409847375E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17142595560429E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17142595560429E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17576486095106E+00 0.18821881482678E+00 0.32873183640969E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    151.15823781
 0.39808515973181E+00 0.30211559979330E+03 0.42863206986843E+03 0.41184396013557E+03 0.40606750881436E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20206895240408E+00 0.00000000000000E+00 -.13104785349052E+02
 0.99005582593549E-03 0.41221004808731E+00 0.80000000000000E+04 0.30000000000000E+04 0.19407581249222E+02
 0.72778429684582E+01 0.31035981080428E+03 0.29915000000008E+03 0.30958469521753E+03 0.32095977554361E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30779095056469E+03 0.32090988410449E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30958469521753E+03 0.32095977554361E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30779095056469E+03 0.32090988410449E+03 0.29915000000007E+03 0.29915000000007E+03 0.34933717780147E+03
 0.29987969867066E+03 0.13819745969789E+04 0.13522695141851E+04 0.63457157690864E+03 0.14935879872849E+04
 0.85584355249173E+03 0.84331545946186E+03 0.84935882377819E+03 0.81968074064290E+03 0.16962599569259E+04
 0.71763891979978E+03 0.84670368146334E+03 0.70105633809333E+03 0.16940962476861E+04 0.84331545946186E+03
 0.84935882377819E+03 0.81968074064290E+03 0.16962599569259E+04 0.71763891979979E+03 0.84670368146334E+03
 0.70105633809333E+03 0.16940962476861E+04 0.16267429469286E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47699879130390E+03 0.12948010463214E+01
 0.12948010463214E+01 0.42464898556896E+00 0.44074247098115E+00 0.29919686675557E+03 0.32966100041318E+03
 0.32070977538348E+03 0.32008485869145E+03 0.23000000000000E+00 0.00000000000000E+00 0.22135170369068E+00
 0.00000000000000E+00 -.66982318266527E+01 0.99950143582436E-03 0.13718231746563E+00 0.80000000000000E+04
 0.30000000000000E+04 0.58316553822647E+02 0.21868707683492E+02 0.29987875579601E+03 0.34937331344466E+03
 0.29936157219259E+03 0.30177763027101E+03 0.29915000000007E+03 0.29915000000007E+03 0.29935831953081E+03
 0.30177855054976E+03 0.29915000000007E+03 0.29915000000007E+03 0.29936157219259E+03 0.30177763027101E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29935831953081E+03 0.30177855054976E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30129861672821E+03 0.29915000000008E+03 0.35453182063869E+02 0.34370473649673E+02
 0.31506866303322E+02 0.27100545211875E+03 0.23934105148391E+03 0.25271243724922E+02 0.29566409345698E+02
 0.25094994849252E+02 0.21837986568889E+03 0.24873121720824E+02 0.29650606260976E+02 0.24701276621694E+02
 0.21845657561208E+03 0.25271243724922E+02 0.29566409345698E+02 0.25094994849252E+02 0.21837986568889E+03
 0.24873121720824E+02 0.29650606260976E+02 0.24701276621694E+02 0.21845657561208E+03 0.12175902975320E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35014621631172E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11731428938511E+00 0.00000000000000E+00 0.00000000000000E+00 0.11731428938511E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17209354420396E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17209354420396E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17571807799628E+00 0.18763549259356E+00 0.32966100041318E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    160.28947610
 0.38413511012489E+00 0.30256899427452E+03 0.43080400187579E+03 0.41438414562020E+03 0.40862849394384E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19995525327336E+00 0.00000000000000E+00 -.14610282196536E+02
 0.98855755395001E-03 0.44118751744793E+00 0.80000000000000E+04 0.30000000000000E+04 0.18132879294220E+02
 0.67998297353326E+01 0.31129225372684E+03 0.29915000000009E+03 0.31036815177051E+03 0.32251387422448E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30850006790917E+03 0.32246454172070E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31036815177051E+03 0.32251387422448E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30850006790917E+03 0.32246454172070E+03 0.29915000000007E+03 0.29915000000007E+03 0.35224412884929E+03
 0.30004459534290E+03 0.14306392704173E+04 0.13986532635760E+04 0.65686028488565E+03 0.15051818863286E+04
 0.84503730001847E+03 0.87038347279085E+03 0.88896017349715E+03 0.84530444485362E+03 0.17390376376207E+04
 0.74480506758039E+03 0.88648536739384E+03 0.72718777279620E+03 0.17370478445015E+04 0.87038347279085E+03
 0.88896017349715E+03 0.84530444485362E+03 0.17390376376207E+04 0.74480506758039E+03 0.88648536739384E+03
 0.72718777279620E+03 0.17370478445015E+04 0.16676339356014E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48000720039867E+03 0.12948121379624E+01
 0.12948121379624E+01 0.46117393871305E+00 0.41259304613904E+00 0.29922029972975E+03 0.33038628331787E+03
 0.32181370458151E+03 0.32118443809887E+03 0.23000000000000E+00 0.00000000000000E+00 0.22047390692968E+00
 0.00000000000000E+00 -.80731623386683E+01 0.99940959417105E-03 0.15607660289893E+00 0.80000000000000E+04
 0.30000000000000E+04 0.51256881886266E+02 0.19221330707350E+02 0.30004353641393E+03 0.35228186283911E+03
 0.29940550569530E+03 0.30200379334352E+03 0.29915000000007E+03 0.29915000000007E+03 0.29940159852385E+03
 0.30200477793248E+03 0.29915000000007E+03 0.29915000000007E+03 0.29940550569530E+03 0.30200379334352E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29940159852385E+03 0.30200477793248E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30149613747615E+03 0.29915000000008E+03 0.40651917428474E+02 0.39260262721430E+02
 0.37088071672993E+02 0.28216974012031E+03 0.24489622808896E+03 0.28996298914320E+02 0.34956794081512E+02
 0.28789134118227E+02 0.22698358696685E+03 0.28549978900034E+02 0.35040835466356E+02 0.28348339573276E+02
 0.22705961792852E+03 0.28996298914320E+02 0.34956794081512E+02 0.28789134118227E+02 0.22698358696685E+03
 0.28549978900034E+02 0.35040835466356E+02 0.28348339573276E+02 0.22705961792852E+03 0.12887561079293E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35110145921277E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13322127518576E+00 0.00000000000000E+00 0.00000000000000E+00 0.13322127518576E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17286435523467E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17286435523467E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17567674915645E+00 0.18717700849776E+00 0.33038628331787E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    171.01348146
 0.37772511001055E+00 0.30306416783990E+03 0.43277669780853E+03 0.41644480459115E+03 0.41064723637377E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19748260154425E+00 0.00000000000000E+00 -.16093744002686E+02
 0.98692790793654E-03 0.47533831318710E+00 0.80000000000000E+04 0.30000000000000E+04 0.16830118208568E+02
 0.63112943282128E+01 0.31233690620224E+03 0.29915000000009E+03 0.31125230048934E+03 0.32425251164231E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30930219351911E+03 0.32420386924854E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31125230048934E+03 0.32425251164231E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30930219351911E+03 0.32420386924854E+03 0.29915000000007E+03 0.29915000000007E+03 0.35541343958812E+03
 0.30025744143759E+03 0.14773764308671E+04 0.14427269568699E+04 0.67758857494077E+03 0.15089937099423E+04
 0.82801719212681E+03 0.89721322253040E+03 0.92847391303792E+03 0.87047717907427E+03 0.17785652694402E+04
 0.77178280070228E+03 0.92618839950383E+03 0.75295721315569E+03 0.17767568855988E+04 0.89721322253040E+03
 0.92847391303791E+03 0.87047717907427E+03 0.17785652694402E+04 0.77178280070228E+03 0.92618839950383E+03
 0.75295721315570E+03 0.17767568855988E+04 0.17030200012050E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48234432631752E+03 0.12948230674876E+01
 0.12948230674876E+01 0.50406996015063E+00 0.38124340022169E+00 0.29925829983935E+03 0.33108693163070E+03
 0.32299729442501E+03 0.32237246558876E+03 0.23000000000000E+00 0.00000000000000E+00 0.21939072607415E+00
 0.00000000000000E+00 -.94635263456532E+01 0.99926897037383E-03 0.17889650486492E+00 0.80000000000000E+04
 0.30000000000000E+04 0.44718593054909E+02 0.16769472395591E+02 0.30025626022091E+03 0.35545237571779E+03
 0.29946024073582E+03 0.30226201454622E+03 0.29915000000007E+03 0.29915000000007E+03 0.29945555386739E+03
 0.30226306423760E+03 0.29915000000007E+03 0.29915000000007E+03 0.29946024073582E+03 0.30226201454622E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29945555386739E+03 0.30226306423760E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30172067916032E+03 0.29915000000009E+03 0.46795436149430E+02 0.44996987892793E+02
 0.43755731270459E+02 0.29393557147879E+03 0.24996106155198E+03 0.33423576933929E+02 0.41391259602237E+02
 0.33188776696584E+02 0.23608714350586E+03 0.32924252737038E+02 0.41474292619196E+02 0.32696333052123E+02
 0.23616164305465E+03 0.33423576933929E+02 0.41391259602237E+02 0.33188776696584E+02 0.23608714350586E+03
 0.32924252737038E+02 0.41474292619196E+02 0.32696333052123E+02 0.23616164305465E+03 0.13669087312637E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35203018682443E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14754420015888E+00 0.00000000000000E+00 0.00000000000000E+00 0.14754420015888E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17377173762512E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17377173762512E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17563517249269E+00 0.18673413520684E+00 0.33108693163070E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    180.20121454
 0.37707661462326E+00 0.30345848424882E+03 0.43401609233062E+03 0.41760601870769E+03 0.41174995877855E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19540470378048E+00 0.00000000000000E+00 -.17068965725635E+02
 0.98563599779096E-03 0.50428271619023E+00 0.80000000000000E+04 0.30000000000000E+04 0.15864116978743E+02
 0.59490438670286E+01 0.31320098207302E+03 0.29915000000009E+03 0.31198827288215E+03 0.32567870416906E+03
 0.29915000000007E+03 0.29915000000007E+03 0.30997202735680E+03 0.32563074526048E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31198827288215E+03 0.32567870416906E+03 0.29915000000007E+03 0.29915000000007E+03
 0.30997202735680E+03 0.32563074526048E+03 0.29915000000007E+03 0.29915000000007E+03 0.35791563287813E+03
 0.30046223601172E+03 0.15094283067953E+04 0.14724625551552E+04 0.69054179229930E+03 0.15045524089708E+04
 0.81055790771003E+03 0.91614720976019E+03 0.95605929827954E+03 0.88797650523734E+03 0.18030558571398E+04
 0.79087287710578E+03 0.95391996122205E+03 0.77097746060440E+03 0.18013858981972E+04 0.91614720976019E+03
 0.95605929827954E+03 0.88797650523734E+03 0.18030558571398E+04 0.79087287710578E+03 0.95391996122204E+03
 0.77097746060440E+03 0.18013858981972E+04 0.17234262786065E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48358634322820E+03 0.12948302526348E+01
 0.12948302526348E+01 0.54082089248261E+00 0.35587377083731E+00 0.29930243116740E+03 0.33156661515587E+03
 0.32391196394323E+03 0.32329827101767E+03 0.23000000000000E+00 0.00000000000000E+00 0.21842015616412E+00
 0.00000000000000E+00 -.10399821453042E+02 0.99911239444809E-03 0.19900378254382E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40200240908679E+02 0.15075090340755E+02 0.30046094633454E+03 0.35795512486957E+03
 0.29951168448826E+03 0.30248071960621E+03 0.29915000000007E+03 0.29915000000007E+03 0.29950630925305E+03
 0.30248181247592E+03 0.29915000000007E+03 0.29915000000007E+03 0.29951168448826E+03 0.30248071960621E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29950630925305E+03 0.30248181247592E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30191103013640E+03 0.29915000000009E+03 0.52014655768924E+02 0.49820865583016E+02
 0.49522952109159E+02 0.30286816459342E+03 0.25309759772372E+03 0.37283767537765E+02 0.46937160253074E+02
 0.37034723282333E+02 0.24303324588679E+03 0.36743743636670E+02 0.47017739125599E+02 0.36502764271386E+02
 0.24310495120698E+03 0.37283767537765E+02 0.46937160253074E+02 0.37034723282333E+02 0.24303324588679E+03
 0.36743743636670E+02 0.47017739125599E+02 0.36502764271385E+02 0.24310495120698E+03 0.14288486517743E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35267928108133E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15637895743850E+00 0.00000000000000E+00 0.00000000000000E+00 0.15637895743850E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17471288715217E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17471288715217E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17560732307740E+00 0.18643269351753E+00 0.33156661515587E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    191.27468798
 0.37913535993985E+00 0.30390342665616E+03 0.43506277092342E+03 0.41848705585734E+03 0.41256059968091E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19297838706153E+00 0.00000000000000E+00 -.17905436316251E+02
 0.98418481035845E-03 0.53836822026751E+00 0.80000000000000E+04 0.30000000000000E+04 0.14859718123824E+02
 0.55723942964340E+01 0.31419037608359E+03 0.29915000000010E+03 0.31283600493024E+03 0.32729878569370E+03
 0.29915000000007E+03 0.29915000000007E+03 0.31074430061394E+03 0.32725167054908E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31283600493024E+03 0.32729878569370E+03 0.29915000000007E+03 0.29915000000007E+03
 0.31074430061394E+03 0.32725167054908E+03 0.29915000000007E+03 0.29915000000007E+03 0.36067708734127E+03
 0.30073148193030E+03 0.15400280105849E+04 0.15003357477401E+04 0.70083718483355E+03 0.14917478851918E+04
 0.78740651443406E+03 0.93467124047890E+03 0.98237567362207E+03 0.90481492029201E+03 0.18228326260050E+04
 0.80962242612811E+03 0.98039588354948E+03 0.78845884498412E+03 0.18213126022539E+04 0.93467124047890E+03
 0.98237567362207E+03 0.90481492029201E+03 0.18228326260050E+04 0.80962242612811E+03 0.98039588354948E+03
 0.78845884498412E+03 0.18213126022539E+04 0.17383411745298E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48445816301809E+03 0.12948364155816E+01
 0.12948364155816E+01 0.58511478625107E+00 0.32710239460212E+00 0.29937230076085E+03 0.33201592216895E+03
 0.32489738434824E+03 0.32430454028890E+03 0.23000000000000E+00 0.00000000000000E+00 0.21720167068570E+00
 0.00000000000000E+00 -.11218396051805E+02 0.99887114138086E-03 0.22391764382060E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35727421312139E+02 0.13397782992052E+02 0.30073005481122E+03 0.36071675291570E+03
 0.29957691101865E+03 0.30273693534486E+03 0.29915000000007E+03 0.29915000000007E+03 0.29957071337631E+03
 0.30273806719356E+03 0.29915000000007E+03 0.29915000000007E+03 0.29957691101865E+03 0.30273693534486E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29957071337631E+03 0.30273806719356E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30213336729875E+03 0.29915000000009E+03 0.58157918697485E+02 0.55447952741561E+02
 0.56410555420467E+02 0.31231177190738E+03 0.25561916370981E+03 0.41913449890589E+02 0.53549725087565E+02
 0.41667102209949E+02 0.25041587978660E+03 0.41330107405037E+02 0.53626429230232E+02 0.41093142132676E+02
 0.25048341309040E+03 0.41913449890589E+02 0.53549725087565E+02 0.41667102209949E+02 0.25041587978660E+03
 0.41330107405037E+02 0.53626429230231E+02 0.41093142132676E+02 0.25048341309040E+03 0.14972197012338E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35329752647381E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16365271869107E+00 0.00000000000000E+00 0.00000000000000E+00 0.16365271869107E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17581362916001E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17581362916001E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17558315050987E+00 0.18615326731799E+00 0.33201592216895E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    200.76838512
 0.38180038047043E+00 0.30426527938592E+03 0.43567321570631E+03 0.41894934901166E+03 0.41297419030041E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19097861286500E+00 0.00000000000000E+00 -.18375166934233E+02
 0.98300979366029E-03 0.56666767596739E+00 0.80000000000000E+04 0.30000000000000E+04 0.14117621913660E+02
 0.52941082176225E+01 0.31499832676934E+03 0.29915000000011E+03 0.31353195534503E+03 0.32860635579554E+03
 0.29915000000007E+03 0.29915000000007E+03 0.31137903351120E+03 0.32855999666577E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31353195534503E+03 0.32860635579554E+03 0.29915000000007E+03 0.29915000000007E+03
 0.31137903351120E+03 0.32855999666577E+03 0.29915000000007E+03 0.29915000000007E+03 0.36283042149573E+03
 0.30098443368100E+03 0.15608118376805E+04 0.15188538723528E+04 0.70603515656147E+03 0.14765396651186E+04
 0.76697433277434E+03 0.94752109431535E+03 0.99989641256827E+03 0.91626162052152E+03 0.18330925097895E+04
 0.82269527750543E+03 0.99804072967832E+03 0.80046962544299E+03 0.18316880058752E+04 0.94752109431535E+03
 0.99989641256827E+03 0.91626162052152E+03 0.18330925097895E+04 0.82269527750544E+03 0.99804072967831E+03
 0.80046962544300E+03 0.18316880058752E+04 0.17449097424457E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48486937326270E+03 0.12948398764933E+01
 0.12948398764933E+01 0.62308957480315E+00 0.30398559994505E+00 0.29944959838655E+03 0.33230069798100E+03
 0.32564319050162E+03 0.32507350743293E+03 0.23000000000000E+00 0.00000000000000E+00 0.21611943411091E+00
 0.00000000000000E+00 -.11689170523818E+02 0.99860865858416E-03 0.24582129502060E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32543966540123E+02 0.12203987452546E+02 0.30098290285173E+03 0.36286999645636E+03
 0.29963619779406E+03 0.30295094225018E+03 0.29915000000007E+03 0.29915000000007E+03 0.29962930424869E+03
 0.30295209431318E+03 0.29915000000007E+03 0.29915000000007E+03 0.29963619779406E+03 0.30295094225018E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29962930424869E+03 0.30295209431318E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30231883047215E+03 0.29915000000009E+03 0.63214811204185E+02 0.60028790421460E+02
 0.62205418432295E+02 0.31931997775348E+03 0.25680353222902E+03 0.45855403123691E+02 0.59094457258956E+02
 0.45632593916830E+02 0.25592285972699E+03 0.45240862258268E+02 0.59166611847355E+02 0.45028369789252E+02
 0.25598570023422E+03 0.45855403123691E+02 0.59094457258956E+02 0.45632593916830E+02 0.25592285972699E+03
 0.45240862258268E+02 0.59166611847355E+02 0.45028369789252E+02 0.25598570023422E+03 0.15502683837451E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35371034079400E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16762258974344E+00 0.00000000000000E+00 0.00000000000000E+00 0.16762258974344E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17677966625045E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17677966625045E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17556942777240E+00 0.18597833639970E+00 0.33230069798100E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    210.26208226
 0.38450297789964E+00 0.30461538695160E+03 0.43610879847550E+03 0.41925559570713E+03 0.41324550717796E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18905701927049E+00 0.00000000000000E+00 -.18676731505211E+02
 0.98187705541339E-03 0.59399292757107E+00 0.80000000000000E+04 0.30000000000000E+04 0.13468173826098E+02
 0.50505651847868E+01 0.31577173117432E+03 0.29915000000014E+03 0.31420088152833E+03 0.32984367361067E+03
 0.29915000000007E+03 0.29915000000007E+03 0.31198956856083E+03 0.32979808558273E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31420088152833E+03 0.32984367361067E+03 0.29915000000007E+03 0.29915000000007E+03
 0.31198956856083E+03 0.32979808558273E+03 0.29915000000007E+03 0.29915000000007E+03 0.36480973576706E+03
 0.30125716081274E+03 0.15779068221667E+04 0.15337743990419E+04 0.70879424789528E+03 0.14591594318660E+04
 0.74682121273127E+03 0.95824767166087E+03 0.10138413232183E+04 0.92564024353104E+03 0.18389594004332E+04
 0.83366729369702E+03 0.10120992327747E+04 0.81042056215636E+03 0.18376597690529E+04 0.95824767166087E+03
 0.10138413232183E+04 0.92564024353104E+03 0.18389594004332E+04 0.83366729369702E+03 0.10120992327747E+04
 0.81042056215636E+03 0.18376597690529E+04 0.17476288531490E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48511225899682E+03 0.12948420983922E+01
 0.12948420983922E+01 0.66106436335522E+00 0.28221882396846E+00 0.29954595868389E+03 0.33250456979583E+03
 0.32630354281740E+03 0.32576085716923E+03 0.23000000000000E+00 0.00000000000000E+00 0.21500767082125E+00
 0.00000000000000E+00 -.11998120822557E+02 0.99828437302569E-03 0.26816319796388E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29832579790003E+02 0.11187217421251E+02 0.30125552337918E+03 0.36484902931896E+03
 0.29969820190801E+03 0.30315925818969E+03 0.29915000000007E+03 0.29915000000007E+03 0.29969063021933E+03
 0.30316041827925E+03 0.29915000000007E+03 0.29915000000007E+03 0.29969820190801E+03 0.30315925818969E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29969063021933E+03 0.30316041827925E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30249913152938E+03 0.29915000000010E+03 0.68014011861427E+02 0.64331758046979E+02
 0.67829641125543E+02 0.32534289104444E+03 0.25717410171327E+03 0.49733607207547E+02 0.64459891295108E+02
 0.49558186728411E+02 0.26071347780920E+03 0.49093560099541E+02 0.64526600117923E+02 0.48929020270926E+02
 0.26077083082408E+03 0.49733607207547E+02 0.64459891295108E+02 0.49558186728411E+02 0.26071347780920E+03
 0.49093560099541E+02 0.64526600117923E+02 0.48929020270926E+02 0.26077083082408E+03 0.15980894200433E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35396963230342E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17012091402195E+00 0.00000000000000E+00 0.00000000000000E+00 0.17012091402195E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17768372649755E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17768372649755E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17556062317094E+00 0.18585441783436E+00 0.33250456979583E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    220.45913212
 0.38706738568122E+00 0.30498204699389E+03 0.43645768827268E+03 0.41949437735583E+03 0.41346071910914E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18707910274149E+00 0.00000000000000E+00 -.18874000970855E+02
 0.98069469894086E-03 0.62220500305244E+00 0.80000000000000E+04 0.30000000000000E+04 0.12857498671263E+02
 0.48215620017237E+01 0.31656659659010E+03 0.29915000000019E+03 0.31489080314220E+03 0.33110101540156E+03
 0.29915000000007E+03 0.29915000000007E+03 0.31261957612854E+03 0.33105625420876E+03 0.29915000000007E+03
 0.29915000000007E+03 0.31489080314220E+03 0.33110101540156E+03 0.29915000000007E+03 0.29915000000007E+03
 0.31261957612854E+03 0.33105625420876E+03 0.29915000000007E+03 0.29915000000007E+03 0.36677028232178E+03
 0.30157099592201E+03 0.15933127338878E+04 0.15469578573636E+04 0.70986950884647E+03 0.14394500548614E+04
 0.72603119847070E+03 0.96801701687484E+03 0.10258758837129E+04 0.93403139139957E+03 0.18419889057978E+04
 0.84371867630693E+03 0.10242451654593E+04 0.81943026446321E+03 0.18407913440521E+04 0.96801701687484E+03
 0.10258758837129E+04 0.93403139139957E+03 0.18419889057978E+04 0.84371867630693E+03 0.10242451654593E+04
 0.81943026446322E+03 0.18407913440521E+04 0.17478440935917E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48528441226594E+03 0.12948435518598E+01
 0.12948435518598E+01 0.70185256278260E+00 0.25981037558140E+00 0.29967397973702E+03 0.33263598751914E+03
 0.32692673977128E+03 0.32641723975673E+03 0.23000000000000E+00 0.00000000000000E+00 0.21379228032486E+00
 0.00000000000000E+00 -.12204751110540E+02 0.99785586898997E-03 0.29247214163750E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27353032515197E+02 0.10257387193199E+02 0.30156925090700E+03 0.36680914236771E+03
 0.29976714754960E+03 0.30337572364081E+03 0.29915000000007E+03 0.29915000000007E+03 0.29975887924742E+03
 0.30337687920702E+03 0.29915000000007E+03 0.29915000000007E+03 0.29976714754960E+03 0.30337572364081E+03
 0.29915000000007E+03 0.29915000000007E+03 0.29975887924742E+03 0.30337687920702E+03 0.29915000000007E+03
 0.29915000000007E+03 0.30268598059784E+03 0.29915000000011E+03 0.72808815428915E+02 0.68585541574417E+02
 0.73595281015231E+02 0.33067566853879E+03 0.25671241111848E+03 0.53786342703821E+02 0.69938816941613E+02
 0.53689546618462E+02 0.26499472725420E+03 0.53126268201293E+02 0.69998816855007E+02 0.53040067188239E+02
 0.26504543573446E+03 0.53786342703821E+02 0.69938816941613E+02 0.53689546618462E+02 0.26499472725420E+03
 0.53126268201293E+02 0.69998816855007E+02 0.53040067188239E+02 0.26504543573446E+03 0.16433278466569E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35408933676628E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17171799336587E+00 0.00000000000000E+00 0.00000000000000E+00 0.17171799336587E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17854666824364E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17854666824364E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555503084564E+00 0.18577469361930E+00 0.33263598751914E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    230.01562599
 0.38900762991789E+00 0.30532046969011E+03 0.43672782689410E+03 0.41968833870087E+03 0.41364301605444E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18530282676998E+00 0.00000000000000E+00 -.18970636070944E+02
 0.97960674487034E-03 0.64756746268276E+00 0.80000000000000E+04 0.30000000000000E+04 0.12353925206275E+02
 0.46327219523531E+01 0.31728588428233E+03 0.29915000000029E+03 0.31551712547377E+03 0.33222201797255E+03
 0.29915000000008E+03 0.29915000000009E+03 0.31319228179875E+03 0.33217805853888E+03 0.29915000000008E+03
 0.29915000000009E+03 0.31551712547377E+03 0.33222201797255E+03 0.29915000000008E+03 0.29915000000009E+03
 0.31319228179875E+03 0.33217805853888E+03 0.29915000000008E+03 0.29915000000009E+03 0.36845942217179E+03
 0.30188673757537E+03 0.16057959204504E+04 0.15574460196151E+04 0.70993629782478E+03 0.14213820891385E+04
 0.70789610982456E+03 0.97598258887793E+03 0.10351911618813E+04 0.94075810458501E+03 0.18429303880432E+04
 0.85195967710277E+03 0.10336557736974E+04 0.82673551970367E+03 0.18418192449611E+04 0.97598258887793E+03
 0.10351911618813E+04 0.94075810458501E+03 0.18429303880432E+04 0.85195967710277E+03 0.10336557736974E+04
 0.82673551970367E+03 0.18418192449611E+04 0.17467060512447E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48542444620283E+03 0.12948442638618E+01
 0.12948442638618E+01 0.74007853829009E+00 0.23967932221339E+00 0.29982085915617E+03 0.33269425082537E+03
 0.32744153600261E+03 0.32696596698891E+03 0.23000000000000E+00 0.00000000000000E+00 0.21263805515819E+00
 0.00000000000000E+00 -.12309403207864E+02 0.99736599818917E-03 0.31547176028554E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25358846676987E+02 0.95095675038699E+01 0.30188489171330E+03 0.36849772180232E+03
 0.29983530442130E+03 0.30357270590379E+03 0.29915000000008E+03 0.29915000000008E+03 0.29982639456220E+03
 0.30357384203150E+03 0.29915000000008E+03 0.29915000000008E+03 0.29983530442130E+03 0.30357270590379E+03
 0.29915000000008E+03 0.29915000000008E+03 0.29982639456220E+03 0.30357384203150E+03 0.29915000000008E+03
 0.29915000000008E+03 0.30285610860620E+03 0.29915000000012E+03 0.76933764652717E+02 0.72203892112894E+02
 0.78728570365238E+02 0.33480482328129E+03 0.25568261006423E+03 0.57487670398543E+02 0.74793816040424E+02
 0.57487670398543E+02 0.26830234721450E+03 0.56816206544363E+02 0.74846503160672E+02 0.56816206544363E+02
 0.26834592726155E+03 0.57487670398543E+02 0.74793816040424E+02 0.57487670398543E+02 0.26830234721450E+03
 0.56816206544363E+02 0.74846503160672E+02 0.56816206544363E+02 0.26834592726155E+03 0.16805873119521E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35415249997347E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17245829943048E+00 0.00000000000000E+00 0.00000000000000E+00 0.17245829943048E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17928812331818E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17928812331818E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555253419212E+00 0.18573932624290E+00 0.33269425082537E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    240.10070506
 0.39059213325901E+00 0.30567182718614E+03 0.43698764414151E+03 0.41989066911643E+03 0.41384052835159E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18350450350511E+00 0.00000000000000E+00 -.19039849460810E+02
 0.97848005772865E-03 0.67323436684785E+00 0.80000000000000E+04 0.30000000000000E+04 0.11882934671705E+02
 0.44561005018895E+01 0.31801630307797E+03 0.29915000000046E+03 0.31615462684611E+03 0.33335119298848E+03
 0.29915000000010E+03 0.29915000000011E+03 0.31377531178133E+03 0.33330804961129E+03 0.29915000000010E+03
 0.29915000000011E+03 0.31615462684611E+03 0.33335119298848E+03 0.29915000000010E+03 0.29915000000011E+03
 0.31377531178133E+03 0.33330804961129E+03 0.29915000000010E+03 0.29915000000011E+03 0.37013624383971E+03
 0.30223788562078E+03 0.16175439334256E+04 0.15671924207213E+04 0.70917970550303E+03 0.14026516161387E+04
 0.68992601210814E+03 0.98349801713613E+03 0.10435818471528E+04 0.94703160782515E+03 0.18427887290254E+04
 0.85977343096665E+03 0.10421383598525E+04 0.83361573219812E+03 0.18417604554533E+04 0.98349801713613E+03
 0.10435818471528E+04 0.94703160782515E+03 0.18427887290254E+04 0.85977343096665E+03 0.10421383598525E+04
 0.83361573219812E+03 0.18417604554533E+04 0.17447020665750E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48558044185137E+03 0.12948447738229E+01
 0.12948447738229E+01 0.78041885454221E+00 0.21936408439248E+00 0.30000830206149E+03 0.33270516425918E+03
 0.32792348610017E+03 0.32748554051638E+03 0.23000000000000E+00 0.00000000000000E+00 0.21140758192303E+00
 0.00000000000000E+00 -.12383999885596E+02 0.99674211734347E-03 0.33991503325153E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23535293286309E+02 0.88257349823657E+01 0.30223594494721E+03 0.37017387061317E+03
 0.29990788801613E+03 0.30377269132922E+03 0.29915000000009E+03 0.29915000000009E+03 0.29989848008377E+03
 0.30377379580875E+03 0.29915000000009E+03 0.29915000000009E+03 0.29990788801613E+03 0.30377269132922E+03
 0.29915000000009E+03 0.29915000000009E+03 0.29989848008377E+03 0.30377379580875E+03 0.29915000000009E+03
 0.29915000000009E+03 0.30302853331267E+03 0.29915000000015E+03 0.80898290706863E+02 0.75664835056337E+02
 0.83834141524178E+02 0.33841670434327E+03 0.25416339211147E+03 0.61260952993140E+02 0.79606904296554E+02
 0.61372020790312E+02 0.27118640199146E+03 0.60583546660634E+02 0.79651464307490E+02 0.60707700734013E+02
 0.27122213881858E+03 0.61260952993140E+02 0.79606904296554E+02 0.61372020790312E+02 0.27118640199146E+03
 0.60583546660634E+02 0.79651464307490E+02 0.60707700734013E+02 0.27122213881858E+03 0.17152234516071E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35418410415543E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17295418853804E+00 0.00000000000000E+00 0.00000000000000E+00 0.17295418853804E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17996464746748E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17996464746748E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555104132940E+00 0.18573151736088E+00 0.33270516425918E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    250.00409947
 0.39174980291135E+00 0.30601064671458E+03 0.43723922068829E+03 0.42010296469144E+03 0.41405272824566E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18180958409015E+00 0.00000000000000E+00 -.19080505116879E+02
 0.97739627784616E-03 0.69738723727735E+00 0.80000000000000E+04 0.30000000000000E+04 0.11471388594997E+02
 0.43017707231240E+01 0.31870890255106E+03 0.29915000000075E+03 0.31676045272731E+03 0.33441236549093E+03
 0.29915000000012E+03 0.29915000000014E+03 0.31432969479261E+03 0.33437000407159E+03 0.29915000000011E+03
 0.29915000000014E+03 0.31676045272731E+03 0.33441236549092E+03 0.29915000000012E+03 0.29915000000014E+03
 0.31432969479261E+03 0.33437000407159E+03 0.29915000000011E+03 0.29915000000014E+03 0.37168360777150E+03
 0.30260231227279E+03 0.16280444466824E+04 0.15758107322063E+04 0.70803335707170E+03 0.13851695621389E+04
 0.67359603828184E+03 0.99021941893487E+03 0.10508105520474E+04 0.95258433354966E+03 0.18420720638507E+04
 0.86679110958414E+03 0.10494495148161E+04 0.83975718892760E+03 0.18411176455151E+04 0.99021941893487E+03
 0.10508105520474E+04 0.95258433354967E+03 0.18420720638507E+04 0.86679110958414E+03 0.10494495148161E+04
 0.83975718892761E+03 0.18411176455151E+04 0.17424520304673E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48575758087101E+03 0.12948450733720E+01
 0.12948450733720E+01 0.82003243220380E+00 0.20034988153781E+00 0.30022732479410E+03 0.33268006091821E+03
 0.32834545969285E+03 0.32794527539017E+03 0.23000000000000E+00 0.00000000000000E+00 0.21018980726341E+00
 0.00000000000000E+00 -.12426538054037E+02 0.99601455272272E-03 0.36404035037546E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21975585925431E+02 0.82408447220368E+01 0.30260029645688E+03 0.37172052591235E+03
 0.29998146155544E+03 0.30396189146734E+03 0.29915000000010E+03 0.29915000000010E+03 0.29997147586343E+03
 0.30396295287042E+03 0.29915000000010E+03 0.29915000000010E+03 0.29998146155544E+03 0.30396189146734E+03
 0.29915000000010E+03 0.29915000000010E+03 0.29997147586343E+03 0.30396295287042E+03 0.29915000000010E+03
 0.29915000000010E+03 0.30319154618866E+03 0.29915000000020E+03 0.84397170889473E+02 0.78707007994403E+02
 0.88553415236607E+02 0.34137638124256E+03 0.25238019892977E+03 0.64848059902284E+02 0.84037860505461E+02
 0.65195217035797E+02 0.27353801055130E+03 0.64172307398025E+02 0.84073900604519E+02 0.64537178745501E+02
 0.27356560066745E+03 0.64848059902284E+02 0.84037860505461E+02 0.65195217035797E+02 0.27353801055130E+03
 0.64172307398025E+02 0.84073900604519E+02 0.64537178745501E+02 0.27356560066745E+03 0.17452544735058E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35419475164485E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17319846596163E+00 0.00000000000000E+00 0.00000000000000E+00 0.17319846596163E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18053869128718E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18053869128718E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555052892835E+00 0.18574491209982E+00 0.33268006091821E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    260.01618108
 0.39259834125514E+00 0.30634689819257E+03 0.43750296530087E+03 0.42033908050345E+03 0.41429121815120E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18016280333391E+00 0.00000000000000E+00 -.19110253113581E+02
 0.97632318465554E-03 0.72079596204090E+00 0.80000000000000E+04 0.30000000000000E+04 0.11098841310582E+02
 0.41620654914681E+01 0.31938727515885E+03 0.29915000000126E+03 0.31735496649503E+03 0.33544379190073E+03
 0.29915000000015E+03 0.29915000000018E+03 0.31487406834439E+03 0.33540219895689E+03 0.29915000000014E+03
 0.29915000000018E+03 0.31735496649503E+03 0.33544379190073E+03 0.29915000000015E+03 0.29915000000018E+03
 0.31487406834439E+03 0.33540219895689E+03 0.29915000000014E+03 0.29915000000018E+03 0.37316375816956E+03
 0.30299044158116E+03 0.16378950287917E+04 0.15838285266880E+04 0.70666563564728E+03 0.13685430511784E+04
 0.65834408735285E+03 0.99652160442922E+03 0.10574029824644E+04 0.95774725538390E+03 0.18411177001000E+04
 0.87339363509785E+03 0.10561182102990E+04 0.84550688549824E+03 0.18402311558653E+04 0.99652160442923E+03
 0.10574029824644E+04 0.95774725538390E+03 0.18411177001000E+04 0.87339363509785E+03 0.10561182102990E+04
 0.84550688549824E+03 0.18402311558653E+04 0.17401676876903E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48596740409617E+03 0.12948452925542E+01
 0.12948452925542E+01 0.86008075864735E+00 0.18207387464785E+00 0.30048764990409E+03 0.33263114809211E+03
 0.32872948725223E+03 0.32836748350591E+03 0.23000000000000E+00 0.00000000000000E+00 0.20895141099774E+00
 0.00000000000000E+00 -.12454713559111E+02 0.99515138650415E-03 0.38851415554638E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20591270319995E+02 0.77217263699980E+01 0.30298835334970E+03 0.37319981147954E+03
 0.30005766263879E+03 0.30414647291161E+03 0.29915000000011E+03 0.29915000000012E+03 0.30004723193090E+03
 0.30414747949037E+03 0.29915000000011E+03 0.29915000000012E+03 0.30005766263879E+03 0.30414647291161E+03
 0.29915000000011E+03 0.29915000000012E+03 0.30004723193090E+03 0.30414747949037E+03 0.29915000000011E+03
 0.29915000000012E+03 0.30335049950280E+03 0.29915000000027E+03 0.87543931998550E+02 0.81445929933092E+02
 0.93039706022373E+02 0.34390965595631E+03 0.25040475140383E+03 0.68364116228565E+02 0.88233953904435E+02
 0.69079261661566E+02 0.27553848207519E+03 0.67695731609794E+02 0.88261006855223E+02 0.68432676803109E+02
 0.27555754934977E+03 0.68364116228565E+02 0.88233953904435E+02 0.69079261661566E+02 0.27553848207519E+03
 0.67695731609794E+02 0.88261006855223E+02 0.68432676803110E+02 0.27555754934977E+03 0.17722131143642E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35419659834735E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17333681416472E+00 0.00000000000000E+00 0.00000000000000E+00 0.17333681416472E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18103837892043E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18103837892043E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555046654306E+00 0.18577210786323E+00 0.33263114809211E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    270.01341710
 0.39320212663948E+00 0.30667616821824E+03 0.43778192268125E+03 0.42059823552474E+03 0.41455364206146E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17858107218812E+00 0.00000000000000E+00 -.19135337696930E+02
 0.97527469099137E-03 0.74321011060624E+00 0.80000000000000E+04 0.30000000000000E+04 0.10764116211329E+02
 0.40365435792482E+01 0.32004525256130E+03 0.29915000000210E+03 0.31793258911884E+03 0.33643754932157E+03
 0.29915000000020E+03 0.29915000000025E+03 0.31540333902297E+03 0.33639670028123E+03 0.29915000000018E+03
 0.29915000000025E+03 0.31793258911884E+03 0.33643754932157E+03 0.29915000000020E+03 0.29915000000025E+03
 0.31540333902297E+03 0.33639670028123E+03 0.29915000000018E+03 0.29915000000025E+03 0.37456912036989E+03
 0.30339776094554E+03 0.16471402909118E+04 0.15913013628982E+04 0.70521261176341E+03 0.13530183080421E+04
 0.64427963321984E+03 0.10024323528742E+04 0.10634655534343E+04 0.96255454345887E+03 0.18401342551699E+04
 0.87960339011879E+03 0.10622505241533E+04 0.85089095244436E+03 0.18393093821544E+04 0.10024323528742E+04
 0.10634655534343E+04 0.96255454345887E+03 0.18401342551699E+04 0.87960339011879E+03 0.10622505241532E+04
 0.85089095244436E+03 0.18393093821544E+04 0.17380195012453E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48620799987409E+03 0.12948454773765E+01
 0.12948454773765E+01 0.90006970270823E+00 0.16477460502389E+00 0.30078992536841E+03 0.33256885618493E+03
 0.32907794900268E+03 0.32875341043727E+03 0.23000000000000E+00 0.00000000000000E+00 0.20770937847263E+00
 0.00000000000000E+00 -.12474965767285E+02 0.10710743044199E-02 0.41300356114826E+00 0.74691363306794E+04
 0.28009261240048E+04 0.19370293025459E+02 0.72638598845472E+01 0.30339560486813E+03 0.37460416903699E+03
 0.30013588467935E+03 0.30432463675694E+03 0.29915000000012E+03 0.29915000000014E+03 0.30012508124677E+03
 0.30432557812248E+03 0.29915000000012E+03 0.29915000000014E+03 0.30013588467935E+03 0.30432463675694E+03
 0.29915000000012E+03 0.29915000000014E+03 0.30012508124677E+03 0.30432557812248E+03 0.29915000000012E+03
 0.29915000000014E+03 0.30350388088241E+03 0.29915000000040E+03 0.90304525585896E+02 0.83867775652582E+02
 0.97252052340413E+02 0.34609300554535E+03 0.24835469294324E+03 0.71772163280545E+02 0.92159382484458E+02
 0.73010824106998E+02 0.27725046528342E+03 0.71116757521927E+02 0.92177212694217E+02 0.72381128355326E+02
 0.27726085271799E+03 0.71772163280545E+02 0.92159382484458E+02 0.73010824106998E+02 0.27725046528342E+03
 0.71116757521927E+02 0.92177212694217E+02 0.72381128355326E+02 0.27726085271799E+03 0.17962783476733E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35419825499633E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17342382935350E+00 0.00000000000000E+00 0.00000000000000E+00 0.17342382935350E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18146575599479E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18146575599479E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555063090705E+00 0.18580704077396E+00 0.33256885618493E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    280.00393507
 0.39363044765677E+00 0.30699862705782E+03 0.43807829706090E+03 0.42087931400023E+03 0.41483783823196E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17705913649080E+00 0.00000000000000E+00 -.19159852929318E+02
 0.97425006646310E-03 0.76470125226659E+00 0.80000000000000E+04 0.30000000000000E+04 0.10461601803695E+02
 0.39231006763857E+01 0.32068552237492E+03 0.29915000000344E+03 0.31849552829004E+03 0.33739908057293E+03
 0.29915000000026E+03 0.29915000000036E+03 0.31591952675682E+03 0.33735895053532E+03 0.29915000000023E+03
 0.29915000000036E+03 0.31849552829004E+03 0.33739908057293E+03 0.29915000000026E+03 0.29915000000036E+03
 0.31591952675682E+03 0.33735895053532E+03 0.29915000000023E+03 0.29915000000036E+03 0.37591087525578E+03
 0.30382445994382E+03 0.16559024491645E+04 0.15983392549202E+04 0.70373703109078E+03 0.13385281412012E+04
 0.63127242495496E+03 0.10080320448593E+04 0.10691309653636E+04 0.96707872104292E+03 0.18392217171916E+04
 0.88549998321094E+03 0.10679798669180E+04 0.85598226778266E+03 0.18384530183809E+04 0.10080320448593E+04
 0.10691309653636E+04 0.96707872104292E+03 0.18392217171916E+04 0.88549998321093E+03 0.10679798669180E+04
 0.85598226778266E+03 0.18384530183809E+04 0.17360620410008E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48647664731757E+03 0.12948456580039E+01
 0.12948456580039E+01 0.94003177461736E+00 0.14843166827417E+00 0.30113736910779E+03 0.33250120109575E+03
 0.32939761049212E+03 0.32910932511563E+03 0.23000000000000E+00 0.00000000000000E+00 0.20646408773218E+00
 0.00000000000000E+00 -.12491789289193E+02 0.11890037922314E-02 0.43750375616288E+00 0.67283216859942E+04
 0.25231206322478E+04 0.18285557294785E+02 0.68570839855444E+01 0.30382224150735E+03 0.37594479359279E+03
 0.30021642597494E+03 0.30449714334912E+03 0.29915000000013E+03 0.29915000000016E+03 0.30020532148226E+03
 0.30449801006302E+03 0.29915000000013E+03 0.29915000000016E+03 0.30021642597494E+03 0.30449714334912E+03
 0.29915000000013E+03 0.29915000000016E+03 0.30020532148226E+03 0.30449801006302E+03 0.29915000000013E+03
 0.29915000000016E+03 0.30365236644629E+03 0.29915000000058E+03 0.92699882012505E+02 0.86006702102931E+02
 0.10121591644186E+03 0.34802462367858E+03 0.24630262765451E+03 0.75086788583342E+02 0.95840581419717E+02
 0.77036510808030E+02 0.27875394221449E+03 0.74449432074008E+02 0.95849060472420E+02 0.76428685609053E+02
 0.27875559147569E+03 0.75086788583342E+02 0.95840581419718E+02 0.77036510808030E+02 0.27875394221450E+03
 0.74449432074008E+02 0.95849060472420E+02 0.76428685609053E+02 0.27875559147569E+03 0.18179841056566E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35420633366298E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17349553749736E+00 0.00000000000000E+00 0.00000000000000E+00 0.17349553749736E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18183206003995E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18183206003995E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555087129274E+00 0.18584507114090E+00 0.33250120109575E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    290.00657834
 0.39393795372356E+00 0.30731484427655E+03 0.43839228627648E+03 0.42118015984956E+03 0.41514101862491E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17559094976200E+00 0.00000000000000E+00 -.19185666549158E+02
 0.97324734608125E-03 0.78535561169521E+00 0.80000000000000E+04 0.30000000000000E+04 0.10186468245553E+02
 0.38199255920823E+01 0.32131101698512E+03 0.29915000000556E+03 0.31904624643549E+03 0.33833389429143E+03
 0.29915000000036E+03 0.29915000000053E+03 0.31642488260691E+03 0.33829445916419E+03 0.29915000000031E+03
 0.29915000000053E+03 0.31904624643549E+03 0.33833389429143E+03 0.29915000000036E+03 0.29915000000053E+03
 0.31642488260691E+03 0.33829445916419E+03 0.29915000000031E+03 0.29915000000053E+03 0.37719933727001E+03
 0.30427124126795E+03 0.16642696312286E+04 0.16050185540614E+04 0.70226583859129E+03 0.13249609182976E+04
 0.61918375051336E+03 0.10133801429729E+04 0.10744883311923E+04 0.97137173619561E+03 0.18384183875334E+04
 0.89114286975110E+03 0.10733960350994E+04 0.86083400653710E+03 0.18377010331724E+04 0.10133801429729E+04
 0.10744883311923E+04 0.97137173619561E+03 0.18384183875334E+04 0.89114286975110E+03 0.10733960350993E+04
 0.86083400653710E+03 0.18377010331724E+04 0.17343004310037E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48676979628020E+03 0.12948458481979E+01
 0.12948458481979E+01 0.98004234767089E+00 0.13300908630610E+00 0.30153316771375E+03 0.33243424268414E+03
 0.32969416018568E+03 0.32944056101928E+03 0.23000000000000E+00 0.00000000000000E+00 0.20521420610487E+00
 0.00000000000000E+00 -.12507464566082E+02 0.13268701707430E-02 0.46204139845127E+00 0.60292259004665E+04
 0.22609597126750E+04 0.17314465818032E+02 0.64929246817618E+01 0.30426896685622E+03 0.37723202038222E+03
 0.30029975660698E+03 0.30466490604460E+03 0.29915000000014E+03 0.29915000000019E+03 0.30028842210048E+03
 0.30466568951693E+03 0.29915000000014E+03 0.29915000000019E+03 0.30029975660698E+03 0.30466490604460E+03
 0.29915000000014E+03 0.29915000000019E+03 0.30028842210048E+03 0.30466568951693E+03 0.29915000000014E+03
 0.29915000000019E+03 0.30379675546746E+03 0.29915000000088E+03 0.94752653063009E+02 0.87896168895774E+02
 0.10496047754667E+03 0.34978555274367E+03 0.24430027280927E+03 0.78325129863482E+02 0.99307156614495E+02
 0.81210761430222E+02 0.28011509320335E+03 0.77710410890875E+02 0.99306240203084E+02 0.80629330493291E+02
 0.28010802268895E+03 0.78325129863482E+02 0.99307156614495E+02 0.81210761430223E+02 0.28011509320335E+03
 0.77710410890875E+02 0.99306240203085E+02 0.80629330493291E+02 0.28010802268895E+03 0.18377996439405E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35422566621114E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17356926623359E+00 0.00000000000000E+00 0.00000000000000E+00 0.17356926623359E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18214778355225E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18214778355225E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555110741900E+00 0.18588272503207E+00 0.33243424268414E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    300.00375998
 0.39416427971005E+00 0.30762432812856E+03 0.43872175618060E+03 0.42149711508061E+03 0.41545917723225E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17417639527162E+00 0.00000000000000E+00 -.19213323359776E+02
 0.97226795022884E-03 0.80517807001353E+00 0.80000000000000E+04 0.30000000000000E+04 0.99356903745101E+01
 0.37258838904413E+01 0.32192207651644E+03 0.29915000000883E+03 0.31958495025817E+03 0.33924337563915E+03
 0.29915000000050E+03 0.29915000000080E+03 0.31691958636875E+03 0.33920460998225E+03 0.29915000000043E+03
 0.29915000000079E+03 0.31958495025817E+03 0.33924337563915E+03 0.29915000000050E+03 0.29915000000080E+03
 0.31691958636875E+03 0.33920460998225E+03 0.29915000000043E+03 0.29915000000079E+03 0.37843845101910E+03
 0.30473713738517E+03 0.16722742158720E+04 0.16113679470629E+04 0.70081231205750E+03 0.13122453824677E+04
 0.60792900884994E+03 0.10185002978009E+04 0.10795769430563E+04 0.97545485726427E+03 0.18377315825097E+04
 0.89655490586057E+03 0.10785387186970E+04 0.86546677457426E+03 0.18370611481527E+04 0.10185002978009E+04
 0.10795769430563E+04 0.97545485726428E+03 0.18377315825097E+04 0.89655490586056E+03 0.10785387186970E+04
 0.86546677457426E+03 0.18370611481527E+04 0.17327223785149E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48708268429651E+03 0.12948460519725E+01
 0.12948460519725E+01 0.10200310742271E+01 0.11852691458093E+00 0.30197852295205E+03 0.33237274714308E+03
 0.32997105806678E+03 0.32975018508245E+03 0.23000000000000E+00 0.00000000000000E+00 0.20396267440148E+00
 0.00000000000000E+00 -.12522962285552E+02 0.14889931442795E-02 0.48655850193022E+00 0.53727581155996E+04
 0.20147842933498E+04 0.16442010504931E+02 0.61657539393490E+01 0.30473481444504E+03 0.37846982108037E+03
 0.30038611800889E+03 0.30482819572828E+03 0.29915000000016E+03 0.29915000000024E+03 0.30037462432243E+03
 0.30482888852391E+03 0.29915000000016E+03 0.29915000000024E+03 0.30038611800889E+03 0.30482819572828E+03
 0.29915000000016E+03 0.29915000000024E+03 0.30037462432243E+03 0.30482888852391E+03 0.29915000000016E+03
 0.29915000000024E+03 0.30393728866192E+03 0.29915000000136E+03 0.96477061451447E+02 0.89559233919296E+02
 0.10850033580464E+03 0.35143437470141E+03 0.24239153721775E+03 0.81490957430743E+02 0.10257496263428E+03
 0.85575282440405E+02 0.28138224378708E+03 0.80902933669701E+02 0.10256471362201E+03 0.85024228456581E+02
 0.28136656948369E+03 0.81490957430743E+02 0.10257496263428E+03 0.85575282440405E+02 0.28138224378708E+03
 0.80902933669702E+02 0.10256471362201E+03 0.85024228456581E+02 0.28136656948369E+03 0.18560450274018E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35425961694098E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17365164404215E+00 0.00000000000000E+00 0.00000000000000E+00 0.17365164404215E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18242113114626E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18242113114626E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555129872545E+00 0.18591729117104E+00 0.33237274714308E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    310.00463058
 0.39433837508599E+00 0.30792753072593E+03 0.43906490680962E+03 0.42182740687700E+03 0.41578939872002E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17281178875282E+00 0.00000000000000E+00 -.19242801951974E+02
 0.97131031841466E-03 0.82422488895292E+00 0.80000000000000E+04 0.30000000000000E+04 0.97060888444695E+01
 0.36397833166760E+01 0.32252049380996E+03 0.29915000001375E+03 0.32011314289513E+03 0.34013088488501E+03
 0.29915000000072E+03 0.29915000000122E+03 0.31740500631078E+03 0.34009276411080E+03 0.29915000000060E+03
 0.29915000000122E+03 0.32011314289513E+03 0.34013088488501E+03 0.29915000000072E+03 0.29915000000122E+03
 0.31740500631078E+03 0.34009276411080E+03 0.29915000000060E+03 0.29915000000122E+03 0.37963442852137E+03
 0.30522227179771E+03 0.16799570594545E+04 0.16174216600192E+04 0.69937601602083E+03 0.13002752576032E+04
 0.59740236150225E+03 0.10234209571399E+04 0.10844327806192E+04 0.97935238891818E+03 0.18371493028902E+04
 0.90176471120739E+03 0.10834443991516E+04 0.86990578098339E+03 0.18365218504772E+04 0.10234209571399E+04
 0.10844327806192E+04 0.97935238891818E+03 0.18371493028902E+04 0.90176471120739E+03 0.10834443991516E+04
 0.86990578098339E+03 0.18365218504772E+04 0.17313008796251E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48741165832672E+03 0.12948462691701E+01
 0.12948462691701E+01 0.10600345566533E+01 0.10496191587549E+00 0.30247488243420E+03 0.33232004123306E+03
 0.33023163786830E+03 0.33004130861935E+03 0.23000000000000E+00 0.00000000000000E+00 0.20270888894950E+00
 0.00000000000000E+00 -.12538612271342E+02 0.16814264242749E-02 0.51106638878026E+00 0.47578650391735E+04
 0.17841993896901E+04 0.15653543601435E+02 0.58700788505383E+01 0.30521990849517E+03 0.37966443093587E+03
 0.30047604272734E+03 0.30498770074763E+03 0.29915000000017E+03 0.29915000000029E+03 0.30046445926557E+03
 0.30498829635768E+03 0.29915000000017E+03 0.29915000000029E+03 0.30047604272734E+03 0.30498770074763E+03
 0.29915000000017E+03 0.29915000000029E+03 0.30046445926557E+03 0.30498829635768E+03 0.29915000000017E+03
 0.29915000000029E+03 0.30407456232485E+03 0.29915000000207E+03 0.97892727931942E+02 0.91020413466667E+02
 0.11185901081079E+03 0.35302100895746E+03 0.24060270309262E+03 0.84595405264426E+02 0.10566783450412E+03
 0.90185156448584E+02 0.28259654503459E+03 0.84037683016535E+02 0.10564838528907E+03 0.89668003652102E+02
 0.28257244499253E+03 0.84595405264426E+02 0.10566783450412E+03 0.90185156448584E+02 0.28259654503459E+03
 0.84037683016535E+02 0.10564838528907E+03 0.89668003652102E+02 0.28257244499253E+03 0.18730269914973E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35431046968314E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17374393569280E+00 0.00000000000000E+00 0.00000000000000E+00 0.17374393569280E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18265985920397E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18265985920397E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555142848384E+00 0.18594688633490E+00 0.33232004123306E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    320.00555697
 0.39447986633352E+00 0.30822457926414E+03 0.43941944466040E+03 0.42216820033868E+03 0.41612882935130E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17149557415286E+00 0.00000000000000E+00 -.19273841102658E+02
 0.97037392992055E-03 0.84252291389669E+00 0.80000000000000E+04 0.30000000000000E+04 0.94952907132220E+01
 0.35607340174582E+01 0.32310709284085E+03 0.29915000002104E+03 0.32063148667220E+03 0.34099817848318E+03
 0.29915000000106E+03 0.29915000000188E+03 0.31788173853970E+03 0.34096067823585E+03 0.29915000000086E+03
 0.29915000000187E+03 0.32063148667220E+03 0.34099817848318E+03 0.29915000000106E+03 0.29915000000188E+03
 0.31788173853970E+03 0.34096067823585E+03 0.29915000000086E+03 0.29915000000187E+03 0.38079096354678E+03
 0.30572606926670E+03 0.16873407490814E+04 0.16231990621984E+04 0.69795408619007E+03 0.12889688089697E+04
 0.58752495234871E+03 0.10281581764318E+04 0.10890750019496E+04 0.98307843809200E+03 0.18366535968171E+04
 0.90678823922762E+03 0.10881326150180E+04 0.87416534268184E+03 0.18360655608889E+04 0.10281581764318E+04
 0.10890750019496E+04 0.98307843809200E+03 0.18366535968171E+04 0.90678823922762E+03 0.10881326150180E+04
 0.87416534268184E+03 0.18360655608889E+04 0.17300091112452E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48775322251201E+03 0.12948464978659E+01
 0.12948464978659E+01 0.11000382621938E+01 0.92308325647177E-01 0.30302220561979E+03 0.33227852365959E+03
 0.33047812250435E+03 0.33031595715513E+03 0.23000000000000E+00 0.00000000000000E+00 0.20145369504618E+00
 0.00000000000000E+00 -.12545977095081E+02 0.19119154725792E-02 0.53554791720558E+00 0.41842854010737E+04
 0.15691070254027E+04 0.14937972388620E+02 0.56017396457325E+01 0.30572367436081E+03 0.38081956653180E+03
 0.30057002171466E+03 0.30514385807222E+03 0.29915000000019E+03 0.29915000000037E+03 0.30055841618113E+03
 0.30514435100015E+03 0.29915000000019E+03 0.29915000000037E+03 0.30057002171466E+03 0.30514385807222E+03
 0.29915000000019E+03 0.29915000000037E+03 0.30055841618113E+03 0.30514435100015E+03 0.29915000000019E+03
 0.29915000000037E+03 0.30420895386245E+03 0.29915000000315E+03 0.99016293938046E+02 0.92297302524618E+02
 0.11505378455538E+03 0.35458198563396E+03 0.23895293215580E+03 0.87643911718566E+02 0.10860356309990E+03
 0.95092241070277E+02 0.28378837180931E+03 0.87119631307334E+02 0.10857511606955E+03 0.94612035750648E+02
 0.28375608611179E+03 0.87643911718566E+02 0.10860356309990E+03 0.95092241070277E+02 0.28378837180931E+03
 0.87119631307334E+02 0.10857511606955E+03 0.94612035750648E+02 0.28375608611178E+03 0.18889671390340E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35437960830223E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17384531737708E+00 0.00000000000000E+00 0.00000000000000E+00 0.17384531737708E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18287045616388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18287045616388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555173950444E+00 0.18597043600242E+00 0.33227852365959E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    330.00063760
 0.39460152160963E+00 0.30851560019222E+03 0.43978311561528E+03 0.42251699517399E+03 0.41647502998208E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17022663206722E+00 0.00000000000000E+00 -.19305861155659E+02
 0.96945827555204E-03 0.86009305509860E+00 0.80000000000000E+04 0.30000000000000E+04 0.93013191451510E+01
 0.34879946794316E+01 0.32368243987387E+03 0.29915000003162E+03 0.32114042506996E+03 0.34184650951703E+03
 0.29915000000157E+03 0.29915000000288E+03 0.31835017764271E+03 0.34180960575033E+03 0.29915000000126E+03
 0.29915000000286E+03 0.32114042506996E+03 0.34184650951703E+03 0.29915000000157E+03 0.29915000000288E+03
 0.31835017764271E+03 0.34180960575033E+03 0.29915000000126E+03 0.29915000000286E+03 0.38191086700435E+03
 0.30624768217244E+03 0.16944425524835E+04 0.16287155549222E+04 0.69654271465790E+03 0.12782560296854E+04
 0.57823060145423E+03 0.10327239062002E+04 0.10935165367490E+04 0.98664390405762E+03 0.18362253319171E+04
 0.91163733134744E+03 0.10926166285272E+04 0.87825644283031E+03 0.18356734716157E+04 0.10327239062002E+04
 0.10935165367490E+04 0.98664390405762E+03 0.18362253319171E+04 0.91163733134744E+03 0.10926166285272E+04
 0.87825644283031E+03 0.18356734716157E+04 0.17288225407741E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48810438788967E+03 0.12948467337891E+01
 0.12948467337891E+01 0.11400185847208E+01 0.80560394430396E-01 0.30361915008927E+03 0.33224974751080E+03
 0.33071208603009E+03 0.33057556682942E+03 0.23000000000000E+00 0.00000000000000E+00 0.20019812381949E+00
 0.00000000000000E+00 -.12548689850231E+02 0.21907253053388E-02 0.55998246172051E+00 0.36517586118643E+04
 0.13694094794491E+04 0.14286161704816E+02 0.53573106393059E+01 0.30624526485754E+03 0.38193805909697E+03
 0.30066858486636E+03 0.30529702945959E+03 0.29915000000020E+03 0.29915000000049E+03 0.30065702270275E+03
 0.30529741521641E+03 0.29915000000020E+03 0.29915000000049E+03 0.30066858486636E+03 0.30529702945959E+03
 0.29915000000020E+03 0.29915000000049E+03 0.30065702270275E+03 0.30529741521641E+03 0.29915000000020E+03
 0.29915000000049E+03 0.30434077435834E+03 0.29915000000474E+03 0.99863659127528E+02 0.93401982850236E+02
 0.11809948265008E+03 0.35614356835972E+03 0.23745358829639E+03 0.90639423249379E+02 0.11139740487375E+03
 0.10034947023540E+03 0.28497971834331E+03 0.90151258516761E+02 0.11136022373015E+03 0.99908783501030E+02
 0.28493954065357E+03 0.90639423249378E+02 0.11139740487375E+03 0.10034947023540E+03 0.28497971834331E+03
 0.90151258516761E+02 0.11136022373015E+03 0.99908783501030E+02 0.28493954065357E+03 0.19040266813501E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35446758920393E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17395205479312E+00 0.00000000000000E+00 0.00000000000000E+00 0.17395205479312E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18305689710679E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18305689710679E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555212162345E+00 0.18598694275132E+00 0.33224974751080E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    340.00270156
 0.39471131296162E+00 0.30880129901046E+03 0.44015459215079E+03 0.42287238188504E+03 0.41682668995828E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16900164971504E+00 0.00000000000000E+00 -.19338467872424E+02
 0.96856103401842E-03 0.87698631612176E+00 0.80000000000000E+04 0.30000000000000E+04 0.91221491748901E+01
 0.34208059405838E+01 0.32424805730658E+03 0.29915000004676E+03 0.32164125515069E+03 0.34267844018551E+03
 0.29915000000234E+03 0.29915000000437E+03 0.31881150478280E+03 0.34264211038884E+03 0.29915000000186E+03
 0.29915000000435E+03 0.32164125515069E+03 0.34267844018551E+03 0.29915000000234E+03 0.29915000000437E+03
 0.31881150478280E+03 0.34264211038884E+03 0.29915000000186E+03 0.29915000000435E+03 0.38299850821711E+03
 0.30678711432426E+03 0.17012907823830E+04 0.16339954130574E+04 0.69513580575738E+03 0.12680578074827E+04
 0.56944632269649E+03 0.10371366325372E+04 0.10977757579198E+04 0.99006487935759E+03 0.18358464642988E+04
 0.91633091714365E+03 0.10969151842221E+04 0.88219620473189E+03 0.18353278902833E+04 0.10371366325372E+04
 0.10977757579198E+04 0.99006487935759E+03 0.18358464642988E+04 0.91633091714365E+03 0.10969151842220E+04
 0.88219620473189E+03 0.18353278902833E+04 0.17277184978383E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48846339981697E+03 0.12948469740349E+01
 0.12948469740349E+01 0.11800268405602E+01 0.69689706307897E-01 0.30426435306497E+03 0.33223456315204E+03
 0.33093507266781E+03 0.33082164842910E+03 0.23000000000000E+00 0.00000000000000E+00 0.19894079144429E+00
 0.00000000000000E+00 -.12552098927267E+02 0.25324496845165E-02 0.58439654967096E+00 0.31589966224847E+04
 0.11846237334317E+04 0.13689334758229E+02 0.51335005343360E+01 0.30678468409722E+03 0.38302429246106E+03
 0.30077252177801E+03 0.30544781392314E+03 0.29915000000022E+03 0.29915000000066E+03 0.30076106587523E+03
 0.30544808877983E+03 0.29915000000022E+03 0.29915000000066E+03 0.30077252177801E+03 0.30544781392314E+03
 0.29915000000022E+03 0.29915000000066E+03 0.30076106587523E+03 0.30544808877983E+03 0.29915000000022E+03
 0.29915000000066E+03 0.30447053809407E+03 0.29915000000703E+03 0.10045113483941E+03 0.94342829726569E+02
 0.12101450515105E+03 0.35772693579284E+03 0.23610735811604E+03 0.93588460696761E+02 0.11406762496791E+03
 0.10602421677632E+03 0.28618826877574E+03 0.93138692820725E+02 0.11402200928787E+03 0.10562523214998E+03
 0.28614052297279E+03 0.93588460696761E+02 0.11406762496791E+03 0.10602421677632E+03 0.28618826877574E+03
 0.93138692820725E+02 0.11402200928787E+03 0.10562523214998E+03 0.28614052297279E+03 0.19183442066838E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35457461735611E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17406144094443E+00 0.00000000000000E+00 0.00000000000000E+00 0.17406144094443E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18322304837871E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18322304837871E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555241674933E+00 0.18599574947003E+00 0.33223456315204E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    350.12762976
 0.39480460128584E+00 0.30908542347421E+03 0.44053671950741E+03 0.42323752733445E+03 0.41718718339372E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16780568820985E+00 0.00000000000000E+00 -.19366556378067E+02
 0.96767042323104E-03 0.89341247032323E+00 0.80000000000000E+04 0.30000000000000E+04 0.89544306417681E+01
 0.33579114906630E+01 0.32481091934607E+03 0.29915000006843E+03 0.32214010999266E+03 0.34350455216789E+03
 0.29915000000347E+03 0.29915000000660E+03 0.31927134854886E+03 0.34346878029417E+03 0.29915000000274E+03
 0.29915000000657E+03 0.32214010999266E+03 0.34350455216789E+03 0.29915000000347E+03 0.29915000000660E+03
 0.31927134854886E+03 0.34346878029417E+03 0.29915000000274E+03 0.29915000000657E+03 0.38406888225578E+03
 0.30735122866604E+03 0.17079840996331E+04 0.16391196423672E+04 0.69370846919958E+03 0.12581987098444E+04
 0.56102169829887E+03 0.10414581758662E+04 0.11019101175905E+04 0.99339229528426E+03 0.18354981888269E+04
 0.92093420348698E+03 0.11010864261032E+04 0.88604183001480E+03 0.18350106316504E+04 0.10414581758662E+04
 0.11019101175905E+04 0.99339229528426E+03 0.18354981888269E+04 0.92093420348698E+03 0.11010864261032E+04
 0.88604183001480E+03 0.18350106316504E+04 0.17266696503707E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48883347150612E+03 0.12948471809907E+01
 0.12948471809907E+01 0.12205265533689E+01 0.59587104728672E-01 0.30502374504589E+03 0.33223298589461E+03
 0.33115210597191E+03 0.33105984463891E+03 0.23000000000000E+00 0.00000000000000E+00 0.19766744978234E+00
 0.00000000000000E+00 -.12550012079363E+02 0.29618095499662E-02 0.60906650628362E+00 0.27010514569012E+04
 0.10128942963379E+04 0.13134854597102E+02 0.49255704739131E+01 0.30734879402798E+03 0.38409326167905E+03
 0.30082747226986E+03 0.30559822759836E+03 0.29915000000023E+03 0.29915000000090E+03 0.30081597831174E+03
 0.30559838752058E+03 0.29915000000023E+03 0.29915000000090E+03 0.30082747226986E+03 0.30559822759836E+03
 0.29915000000023E+03 0.29915000000090E+03 0.30081597831174E+03 0.30559838752058E+03 0.29915000000023E+03
 0.29915000000090E+03 0.30459997023017E+03 0.29915000001036E+03 0.10077823623134E+03 0.95301590008443E+02
 0.12383963972306E+03 0.35935554467393E+03 0.23489670675226E+03 0.96832377222095E+02 0.11665260218321E+03
 0.96832377222095E+02 0.28743149117745E+03 0.96422696612788E+02 0.11659881100403E+03 0.96422696612788E+02
 0.28737646330248E+03 0.96832377222095E+02 0.11665260218321E+03 0.96832377222095E+02 0.28743149117745E+03
 0.96422696612787E+02 0.11659881100403E+03 0.96422696612787E+02 0.28737646330248E+03 0.19306741584075E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35470213543621E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17412872960611E+00 0.00000000000000E+00 0.00000000000000E+00 0.17412872960611E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18334018137682E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18334018137682E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555281488847E+00 0.18599705813272E+00 0.33223298589461E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    360.01126525
 0.39489265738438E+00 0.30935800755390E+03 0.44091467791772E+03 0.42359775686552E+03 0.41754194863961E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16667945778323E+00 0.00000000000000E+00 -.19400064513928E+02
 0.96681746169747E-03 0.90881747319298E+00 0.80000000000000E+04 0.30000000000000E+04 0.88026476558525E+01
 0.33009928709447E+01 0.32535177874906E+03 0.29915000009779E+03 0.32261989318633E+03 0.34429647739005E+03
 0.29915000000506E+03 0.29915000000977E+03 0.31971397368867E+03 0.34426123019234E+03 0.29915000000398E+03
 0.29915000000972E+03 0.32261989318633E+03 0.34429647739005E+03 0.29915000000506E+03 0.29915000000977E+03
 0.31971397368867E+03 0.34426123019234E+03 0.29915000000398E+03 0.29915000000972E+03 0.38508499725440E+03
 0.30791920451553E+03 0.17142980683433E+04 0.16439165429389E+04 0.69232803095049E+03 0.12490141556548E+04
 0.55322448454960E+03 0.10455450673068E+04 0.11057882528090E+04 0.99651610891819E+03 0.18351880747761E+04
 0.92529348446579E+03 0.11049979579750E+04 0.88966478535666E+03 0.18347284117771E+04 0.10455450673068E+04
 0.11057882528090E+04 0.99651610891819E+03 0.18351880747761E+04 0.92529348446579E+03 0.11049979579750E+04
 0.88966478535666E+03 0.18347284117771E+04 0.17257077503091E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48919929582191E+03 0.12948474278783E+01
 0.12948474278783E+01 0.12600610953173E+01 0.50574085363667E-01 0.30582151430806E+03 0.33224589065608E+03
 0.33135496487935E+03 0.33128079635434E+03 0.23000000000000E+00 0.00000000000000E+00 0.19642388206231E+00
 0.00000000000000E+00 -.12555673689506E+02 0.34896456849654E-02 0.63310435854380E+00 0.22924963512677E+04
 0.85968613172537E+03 0.12636147409253E+02 0.47385552784699E+01 0.30791676414506E+03 0.38510802793182E+03
 0.30089728301169E+03 0.30574329361426E+03 0.29915000000024E+03 0.29915000000124E+03 0.30088587807107E+03
 0.30574333895255E+03 0.29915000000024E+03 0.29915000000124E+03 0.30089728301169E+03 0.30574329361426E+03
 0.29915000000024E+03 0.29915000000124E+03 0.30088587807107E+03 0.30574333895255E+03 0.29915000000024E+03
 0.29915000000124E+03 0.30472480980756E+03 0.29915000001492E+03 0.10086275423719E+03 0.96103706230318E+02
 0.12649487233483E+03 0.36100025671827E+03 0.23387291002177E+03 0.99908569764495E+02 0.11907966282776E+03
 0.99908569764495E+02 0.28869077997376E+03 0.99539532187868E+02 0.11901820046764E+03 0.99539532187868E+02
 0.28862896768117E+03 0.99908569764495E+02 0.11907966282776E+03 0.99908569764495E+02 0.28869077997376E+03
 0.99539532187868E+02 0.11901820046764E+03 0.99539532187868E+02 0.28862896768117E+03 0.19439402709081E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35484703127625E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17424482008954E+00 0.00000000000000E+00 0.00000000000000E+00 0.17424482008954E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18347918268477E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18347918268477E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555291048273E+00 0.18598992470200E+00 0.33224589065608E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    370.00808612
 0.39498360182227E+00 0.30962915699943E+03 0.44130066917804E+03 0.42396463979881E+03 0.41790245528660E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16558029139976E+00 0.00000000000000E+00 -.19433580107534E+02
 0.96597047760848E-03 0.92379101075564E+00 0.80000000000000E+04 0.30000000000000E+04 0.86599673593448E+01
 0.32474877597543E+01 0.32589058941421E+03 0.29915000013836E+03 0.32309825650157E+03 0.34508391718914E+03
 0.29915000000737E+03 0.29915000001434E+03 0.32015561073672E+03 0.34504918025692E+03 0.29915000000578E+03
 0.29915000001428E+03 0.32309825650157E+03 0.34508391718914E+03 0.29915000000737E+03 0.29915000001434E+03
 0.32015561073672E+03 0.34504918025692E+03 0.29915000000578E+03 0.29915000001428E+03 0.38608697608050E+03
 0.30850854911760E+03 0.17204762323977E+04 0.16485745047978E+04 0.69092697604898E+03 0.12400934663283E+04
 0.54571185539906E+03 0.10495547549423E+04 0.11095592379895E+04 0.99955902509378E+03 0.18348904329588E+04
 0.92957624264107E+03 0.11088003281389E+04 0.89320627700748E+03 0.18344568137719E+04 0.10495547549423E+04
 0.11095592379895E+04 0.99955902509378E+03 0.18348904329588E+04 0.92957624264107E+03 0.11088003281389E+04
 0.89320627700748E+03 0.18344568137719E+04 0.17247751570792E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48957239753555E+03 0.12948476748211E+01
 0.12948476748211E+01 0.13000483787833E+01 0.42285169895513E-01 0.30664862387725E+03 0.33227311349013E+03
 0.33155075622562E+03 0.33149223334185E+03 0.23000000000000E+00 0.00000000000000E+00 0.19516568305367E+00
 0.00000000000000E+00 -.12561798416763E+02 0.41737002207163E-02 0.65737004347145E+00 0.19167644001578E+04
 0.71878665005919E+03 0.12169705753176E+02 0.45636396574409E+01 0.30850611779515E+03 0.38610867864954E+03
 0.30097431463444E+03 0.30588850311848E+03 0.29915000000027E+03 0.29915000000174E+03 0.30096307637089E+03
 0.30588843123890E+03 0.29915000000027E+03 0.29915000000174E+03 0.30097431463444E+03 0.30588850311848E+03
 0.29915000000027E+03 0.29915000000174E+03 0.30096307637089E+03 0.30588843123890E+03 0.29915000000027E+03
 0.29915000000174E+03 0.30484976182379E+03 0.29915000002129E+03 0.10073214204912E+03 0.96686220638929E+02
 0.12908391270448E+03 0.36271553860951E+03 0.23298620634151E+03 0.10297902686447E+03 0.12144514570347E+03
 0.10297902686447E+03 0.29000777043222E+03 0.10265211950525E+03 0.12137631827004E+03 0.10265211950525E+03
 0.28993949680315E+03 0.10297902686447E+03 0.12144514570347E+03 0.10297902686447E+03 0.29000777043222E+03
 0.10265211950525E+03 0.12137631827004E+03 0.10265211950525E+03 0.28993949680315E+03 0.19566797716468E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35501004450979E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17435854320299E+00 0.00000000000000E+00 0.00000000000000E+00 0.17435854320299E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18360303455838E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18360303455838E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555292922194E+00 0.18597469532151E+00 0.33227311349013E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    380.00160556
 0.39507348549335E+00 0.30989596309541E+03 0.44168925550002E+03 0.42433324370171E+03 0.41826399142901E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16452044492572E+00 0.00000000000000E+00 -.19465689584697E+02
 0.96513851578235E-03 0.93816929350130E+00 0.80000000000000E+04 0.30000000000000E+04 0.85272456212498E+01
 0.31977171079687E+01 0.32642142515317E+03 0.29915000019324E+03 0.32356991674318E+03 0.34585824352970E+03
 0.29915000001061E+03 0.29915000002080E+03 0.32059138441867E+03 0.34582399730709E+03 0.29915000000831E+03
 0.29915000002071E+03 0.32356991674318E+03 0.34585824352970E+03 0.29915000001061E+03 0.29915000002080E+03
 0.32059138441867E+03 0.34582399730709E+03 0.29915000000831E+03 0.29915000002071E+03 0.38706414210586E+03
 0.30911110976393E+03 0.17264575348818E+04 0.16530498860314E+04 0.68952219671669E+03 0.12315164160162E+04
 0.53854660831588E+03 0.10534466999261E+04 0.11131842666655E+04 0.10024916676005E+04 0.18345999987970E+04
 0.93373877946004E+03 0.11124545264219E+04 0.89663120570996E+03 0.18341904216641E+04 0.10534466999261E+04
 0.11131842666655E+04 0.10024916676005E+04 0.18345999987970E+04 0.93373877946004E+03 0.11124545264219E+04
 0.89663120570996E+03 0.18341904216641E+04 0.17238759576278E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48994775452332E+03 0.12948479114036E+01
 0.12948479114036E+01 0.13400224565613E+01 0.34816418933473E-01 0.30749009821888E+03 0.33231373551250E+03
 0.33173755540886E+03 0.33169220509505E+03 0.23000000000000E+00 0.00000000000000E+00 0.19390773503210E+00
 0.00000000000000E+00 -.12567355301817E+02 0.50690339181063E-02 0.68157684609161E+00 0.15782099960753E+04
 0.59182874852823E+03 0.11737487923592E+02 0.44015579713469E+01 0.30910870419763E+03 0.38708455961462E+03
 0.30105439644845E+03 0.30603236535326E+03 0.29915000000031E+03 0.29915000000245E+03 0.30104338321667E+03
 0.30603217563721E+03 0.29915000000031E+03 0.29915000000245E+03 0.30105439644845E+03 0.30603236535326E+03
 0.29915000000031E+03 0.29915000000245E+03 0.30104338321667E+03 0.30603217563721E+03 0.29915000000031E+03
 0.29915000000245E+03 0.30497354040389E+03 0.29915000002999E+03 0.10039622392578E+03 0.97034729879759E+02
 0.13158056570523E+03 0.36447740162194E+03 0.23223893308818E+03 0.10602478703465E+03 0.12372563139663E+03
 0.10602478703465E+03 0.29136415661528E+03 0.10574051053018E+03 0.12364986306524E+03 0.10574051053018E+03
 0.29128984763414E+03 0.10602478703465E+03 0.12372563139663E+03 0.10602478703465E+03 0.29136415661528E+03
 0.10574051053018E+03 0.12364986306524E+03 0.10574051053018E+03 0.29128984763414E+03 0.19687208601464E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35518835567748E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17446018488442E+00 0.00000000000000E+00 0.00000000000000E+00 0.17446018488442E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18370995727614E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18370995727614E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555290199150E+00 0.18595192273335E+00 0.33231373551250E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    390.01596321
 0.39515931609270E+00 0.31015943661740E+03 0.44208068673566E+03 0.42470404974405E+03 0.41862716135825E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16349615082973E+00 0.00000000000000E+00 -.19496276637831E+02
 0.96431836098914E-03 0.95200705496780E+00 0.80000000000000E+04 0.30000000000000E+04 0.84032990703735E+01
 0.31512371513901E+01 0.32694598977197E+03 0.29915000026679E+03 0.32403635883894E+03 0.34662201282232E+03
 0.29915000001513E+03 0.29915000002982E+03 0.32102265812974E+03 0.34658823979925E+03 0.29915000001184E+03
 0.29915000002968E+03 0.32403635883894E+03 0.34662201282232E+03 0.29915000001513E+03 0.29915000002982E+03
 0.32102265812974E+03 0.34658823979925E+03 0.29915000001184E+03 0.29915000002968E+03 0.38802033428053E+03
 0.30972691173076E+03 0.17322720228091E+04 0.16573691586917E+04 0.68811107666418E+03 0.12232354664299E+04
 0.53168383438237E+03 0.10572391603783E+04 0.11166816416909E+04 0.10053302132562E+04 0.18343125540262E+04
 0.93780006894969E+03 0.11159791059257E+04 0.89995710347770E+03 0.18339252444814E+04 0.10572391603783E+04
 0.11166816416909E+04 0.10053302132562E+04 0.18343125540262E+04 0.93780006894970E+03 0.11159791059257E+04
 0.89995710347770E+03 0.18339252444814E+04 0.17230047798262E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49032586492856E+03 0.12948481367691E+01
 0.12948481367691E+01 0.13800798871613E+01 0.28138941568994E-01 0.30834215037402E+03 0.33236705940440E+03
 0.33191636906346E+03 0.33188196194238E+03 0.23000000000000E+00 0.00000000000000E+00 0.19264718693280E+00
 0.00000000000000E+00 -.12572179152142E+02 0.62719343897644E-02 0.70578073913959E+00 0.12755235470983E+04
 0.47832133016186E+03 0.11334965034258E+02 0.42506118878467E+01 0.30972454516006E+03 0.38803950869043E+03
 0.30113674464266E+03 0.30617539825823E+03 0.29915000000035E+03 0.29915000000344E+03 0.30112600629592E+03
 0.30617509046959E+03 0.29915000000035E+03 0.29915000000344E+03 0.30113674464266E+03 0.30617539825823E+03
 0.29915000000035E+03 0.29915000000344E+03 0.30112600629592E+03 0.30617509046959E+03 0.29915000000035E+03
 0.29915000000344E+03 0.30509659702641E+03 0.29915000004177E+03 0.99865033029074E+02 0.97146042403356E+02
 0.13399582306389E+03 0.36629117616398E+03 0.23162537398477E+03 0.10905668869696E+03 0.12593165922523E+03
 0.10905668869696E+03 0.29275615047193E+03 0.10881509328624E+03 0.12584937635957E+03 0.10881509328624E+03
 0.29267623174085E+03 0.10905668869696E+03 0.12593165922523E+03 0.10905668869696E+03 0.29275615047193E+03
 0.10881509328624E+03 0.12584937635957E+03 0.10881509328624E+03 0.29267623174085E+03 0.19800800171293E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35539509126381E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17454849316106E+00 0.00000000000000E+00 0.00000000000000E+00 0.17454849316106E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18380023892375E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18380023892375E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555283788779E+00 0.18592201246508E+00 0.33236705940440E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    400.00459013
 0.39523937336255E+00 0.31041865631542E+03 0.44247259823986E+03 0.42507495915444E+03 0.41898999936553E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16251084531923E+00 0.00000000000000E+00 -.19526155724616E+02
 0.96351280847581E-03 0.96526144733760E+00 0.80000000000000E+04 0.30000000000000E+04 0.82879099979241E+01
 0.31079662492215E+01 0.32746225064719E+03 0.29915000036384E+03 0.32449574768908E+03 0.34737233097977E+03
 0.29915000002130E+03 0.29915000004219E+03 0.32144772557040E+03 0.34733901233348E+03 0.29915000001669E+03
 0.29915000004200E+03 0.32449574768908E+03 0.34737233097977E+03 0.29915000002130E+03 0.29915000004219E+03
 0.32144772557040E+03 0.34733901233348E+03 0.29915000001669E+03 0.29915000004200E+03 0.38895255792949E+03
 0.31035172248270E+03 0.17379071679900E+04 0.16615265230252E+04 0.68670120924985E+03 0.12152646659798E+04
 0.52512995068372E+03 0.10609229590956E+04 0.11200449987455E+04 0.10080699395843E+04 0.18340279057790E+04
 0.94174973477581E+03 0.11193677435090E+04 0.90317714991076E+03 0.18336611429027E+04 0.10609229590956E+04
 0.11200449987455E+04 0.10080699395843E+04 0.18340279057790E+04 0.94174973477581E+03 0.11193677435090E+04
 0.90317714991076E+03 0.18336611429027E+04 0.17221633598352E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49070456582209E+03 0.12948483569183E+01
 0.12948483569183E+01 0.14200343948305E+01 0.22572813378208E-01 0.30919131352456E+03 0.33243681091587E+03
 0.33208700006620E+03 0.33206112576734E+03 0.23000000000000E+00 0.00000000000000E+00 0.19138172275533E+00
 0.00000000000000E+00 -.12577678008251E+02 0.78185018711005E-02 0.73001292333718E+00 0.10232139266437E+04
 0.38370522249139E+03 0.10958710105335E+02 0.41095162895005E+01 0.31034940526312E+03 0.38897053939647E+03
 0.30122053387766E+03 0.30631718968244E+03 0.29915000000042E+03 0.29915000000480E+03 0.30121010395009E+03
 0.30631676483928E+03 0.29915000000041E+03 0.29915000000480E+03 0.30122053387766E+03 0.30631718968244E+03
 0.29915000000042E+03 0.29915000000480E+03 0.30121010395009E+03 0.30631676483928E+03 0.29915000000041E+03
 0.29915000000480E+03 0.30521871067787E+03 0.29915000005747E+03 0.99165394132879E+02 0.97021953657327E+02
 0.13635070942151E+03 0.36824075809180E+03 0.23120829512318E+03 0.11206523031306E+03 0.12808652769676E+03
 0.11206523031306E+03 0.29422529752884E+03 0.11186279188840E+03 0.12799823560656E+03 0.11186279188840E+03
 0.29414026425741E+03 0.11206523031306E+03 0.12808652769676E+03 0.11206523031306E+03 0.29422529752884E+03
 0.11186279188840E+03 0.12799823560656E+03 0.11186279188840E+03 0.29414026425741E+03 0.19909785103226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35569654654242E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17463126334490E+00 0.00000000000000E+00 0.00000000000000E+00 0.17463126334490E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18387984839512E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18387984839512E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555268215903E+00 0.18588282753616E+00 0.33243681091587E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    410.00166448
 0.39531386731377E+00 0.31067476068410E+03 0.44286586280501E+03 0.42544687087038E+03 0.41935344651927E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16155979968156E+00 0.00000000000000E+00 -.19554670780657E+02
 0.96271826685842E-03 0.97799965322256E+00 0.80000000000000E+04 0.30000000000000E+04 0.81799620006404E+01
 0.30674857502401E+01 0.32797235446614E+03 0.29915000049098E+03 0.32494996849754E+03 0.34811237878738E+03
 0.29915000002969E+03 0.29915000005904E+03 0.32186832113961E+03 0.34807949813327E+03 0.29915000002327E+03
 0.29915000005877E+03 0.32494996849754E+03 0.34811237878738E+03 0.29915000002969E+03 0.29915000005904E+03
 0.32186832113961E+03 0.34807949813327E+03 0.29915000002327E+03 0.29915000005877E+03 0.38986537358534E+03
 0.31098615845871E+03 0.17433950641929E+04 0.16655486199876E+04 0.68528851875103E+03 0.12075539998135E+04
 0.51883903846871E+03 0.10645183195614E+04 0.11232950790682E+04 0.10107277172817E+04 0.18337440542991E+04
 0.94560895582739E+03 0.11226414211971E+04 0.90631002622321E+03 0.18333963314205E+04 0.10645183195614E+04
 0.11232950790682E+04 0.10107277172817E+04 0.18337440542991E+04 0.94560895582739E+03 0.11226414211971E+04
 0.90631002622321E+03 0.18333963314205E+04 0.17213464295037E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49108473141395E+03 0.12948485670175E+01
 0.12948485670175E+01 0.14600226922212E+01 0.18099480267527E-01 0.31002557734952E+03 0.33252133954947E+03
 0.33224989848011E+03 0.33223045712833E+03 0.23000000000000E+00 0.00000000000000E+00 0.19010651861427E+00
 0.00000000000000E+00 -.12582983523423E+02 0.97508641523833E-02 0.75436739297547E+00 0.82044010407474E+03
 0.30766503902803E+03 0.10604912241031E+02 0.39768420903865E+01 0.31098389974982E+03 0.38988220957596E+03
 0.30130579151934E+03 0.30645861119159E+03 0.29915000000050E+03 0.29915000000665E+03 0.30129568492896E+03
 0.30645807062018E+03 0.29915000000050E+03 0.29915000000665E+03 0.30130579151934E+03 0.30645861119159E+03
 0.29915000000050E+03 0.29915000000665E+03 0.30129568492896E+03 0.30645807062018E+03 0.29915000000050E+03
 0.29915000000665E+03 0.30534046851507E+03 0.29915000007823E+03 0.98299939403202E+02 0.96637665788829E+02
 0.13865260755733E+03 0.37026434565776E+03 0.23091847506264E+03 0.11505379863341E+03 0.13019554810574E+03
 0.11505379863341E+03 0.29578901264878E+03 0.11488560599385E+03 0.13010174577176E+03 0.11488560599385E+03
 0.29569935226460E+03 0.11505379863341E+03 0.13019554810574E+03 0.11505379863341E+03 0.29578901264878E+03
 0.11488560599385E+03 0.13010174577176E+03 0.11488560599385E+03 0.29569935226460E+03 0.20012807089633E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35597589425735E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17470253347359E+00 0.00000000000000E+00 0.00000000000000E+00 0.17470253347359E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18394544087296E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18394544087296E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555246722956E+00 0.18583533791202E+00 0.33252133954947E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    421.30008516
 0.39539315187248E+00 0.31095957600983E+03 0.44331000687461E+03 0.42586652220417E+03 0.41976311515822E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16052568496704E+00 0.00000000000000E+00 -.19587260770960E+02
 0.96183618070964E-03 0.99178784121444E+00 0.80000000000000E+04 0.30000000000000E+04 0.80662412539803E+01
 0.30248404702426E+01 0.32854039015467E+03 0.29915000068317E+03 0.32545607603044E+03 0.34893626323258E+03
 0.29915000004287E+03 0.29915000008557E+03 0.32233714822031E+03 0.34890385355072E+03 0.29915000003364E+03
 0.29915000008519E+03 0.32545607603044E+03 0.34893626323258E+03 0.29915000004287E+03 0.29915000008557E+03
 0.32233714822031E+03 0.34890385355072E+03 0.29915000003364E+03 0.29915000008519E+03 0.39087883593367E+03
 0.31171304030425E+03 0.17494208258968E+04 0.16699334789715E+04 0.68361783402830E+03 0.11989971281626E+04
 0.51196120496416E+03 0.10684752122689E+04 0.11268236988017E+04 0.10136336006316E+04 0.18334002370317E+04
 0.94986213071954E+03 0.11261948310575E+04 0.90974750554304E+03 0.18330724038592E+04 0.10684752122689E+04
 0.11268236988017E+04 0.10136336006316E+04 0.18334002370317E+04 0.94986213071954E+03 0.11261948310575E+04
 0.90974750554304E+03 0.18330724038592E+04 0.17204030869300E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49151414563243E+03 0.12948488071409E+01
 0.12948488071409E+01 0.15052163749617E+01 0.14096407705471E-01 0.31094486791639E+03 0.33262867966152E+03
 0.33242490376087E+03 0.33241083616413E+03 0.23000000000000E+00 0.00000000000000E+00 0.18866210246682E+00
 0.00000000000000E+00 -.12590367740037E+02 0.12519896324031E-01 0.78189004503556E+00 0.63898292709060E+03
 0.23961859765898E+03 0.10231617668999E+02 0.38368566258745E+01 0.31171085829997E+03 0.39089445794236E+03
 0.30140280735811E+03 0.30661746699079E+03 0.29915000000062E+03 0.29915000000955E+03 0.30139306344486E+03
 0.30661679907078E+03 0.29915000000062E+03 0.29915000000955E+03 0.30140280735811E+03 0.30661746699079E+03
 0.29915000000062E+03 0.29915000000955E+03 0.30139306344486E+03 0.30661679907078E+03 0.29915000000062E+03
 0.29915000000955E+03 0.30547704685393E+03 0.29915000010990E+03 0.97113103974656E+02 0.95882454001973E+02
 0.14116590353398E+03 0.37256848717537E+03 0.23069675412372E+03 0.11839405915583E+03 0.13249853309580E+03
 0.11839405915583E+03 0.29759705631100E+03 0.11825928423545E+03 0.13239907497707E+03 0.11825928423545E+03
 0.29750271068176E+03 0.11839405915583E+03 0.13249853309580E+03 0.11839405915583E+03 0.29759705631100E+03
 0.11825928423545E+03 0.13239907497707E+03 0.11825928423545E+03 0.29750271068176E+03 0.20119075200574E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35626773862831E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17478698490038E+00 0.00000000000000E+00 0.00000000000000E+00 0.17478698490038E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18399860123282E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18399860123282E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555212737905E+00 0.18577499519487E+00 0.33262867966152E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    430.04620905
 0.39544243388890E+00 0.31117746537859E+03 0.44365405100643E+03 0.42619176318844E+03 0.42008047338858E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15975374639119E+00 0.00000000000000E+00 -.19606195087721E+02
 0.96116251438446E-03 0.10020341741086E+01 0.80000000000000E+04 0.30000000000000E+04 0.79837596428453E+01
 0.29939098660670E+01 0.32897497907884E+03 0.29915000087572E+03 0.32584354514585E+03 0.34956500179749E+03
 0.29915000005654E+03 0.29915000011317E+03 0.32269640232024E+03 0.34953294499345E+03 0.29915000004441E+03
 0.29915000011267E+03 0.32584354514585E+03 0.34956500179749E+03 0.29915000005654E+03 0.29915000011317E+03
 0.32269640232024E+03 0.34953294499345E+03 0.29915000004441E+03 0.29915000011267E+03 0.39164503920782E+03
 0.31228139939344E+03 0.17539734703867E+04 0.16732261694639E+04 0.68235607415287E+03 0.11926372826822E+04
 0.50686942815857E+03 0.10714696728759E+04 0.11294696795396E+04 0.10158199224210E+04 0.18331395711887E+04
 0.95308372034775E+03 0.11288587503633E+04 0.91234012643713E+03 0.18328260105495E+04 0.10714696728759E+04
 0.11294696795396E+04 0.10158199224210E+04 0.18331395711887E+04 0.95308372034775E+03 0.11288587503633E+04
 0.91234012643713E+03 0.18328260105495E+04 0.17197114378641E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49184744510063E+03 0.12948489466492E+01
 0.12948489466492E+01 0.15402008705366E+01 0.11612590696581E-01 0.31163526012640E+03 0.33271783558811E+03
 0.33255462004166E+03 0.33254366949178E+03 0.23000000000000E+00 0.00000000000000E+00 0.18754586658445E+00
 0.00000000000000E+00 -.12590114185508E+02 0.15197776313868E-01 0.80311934418174E+00 0.52639279818191E+03
 0.19739729931822E+03 0.99611596432791E+01 0.37354348662297E+01 0.31227928314687E+03 0.39165975900839E+03
 0.30147909439616E+03 0.30674004098527E+03 0.29915000000075E+03 0.29915000001257E+03 0.30146963242224E+03
 0.30673927593913E+03 0.29915000000074E+03 0.29915000001258E+03 0.30147909439616E+03 0.30674004098527E+03
 0.29915000000075E+03 0.29915000001257E+03 0.30146963242224E+03 0.30673927593913E+03 0.29915000000074E+03
 0.29915000001258E+03 0.30558237857268E+03 0.29915000014190E+03 0.96048198172880E+02 0.95073067912760E+02
 0.14303870908297E+03 0.37433029234057E+03 0.23057638971219E+03 0.12094994577003E+03 0.13421331010022E+03
 0.12094994577003E+03 0.29899149445499E+03 0.12083758347839E+03 0.13410983595237E+03 0.12083758347839E+03
 0.29889387232141E+03 0.12094994577003E+03 0.13421331010022E+03 0.12094994577003E+03 0.29899149445499E+03
 0.12083758347839E+03 0.13410983595237E+03 0.12083758347839E+03 0.29889387232141E+03 0.20192902172118E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35647872719451E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17479949484752E+00 0.00000000000000E+00 0.00000000000000E+00 0.17479949484752E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18401876169317E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18401876169317E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555200954809E+00 0.18572509025538E+00 0.33271783558811E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    442.83755412
 0.39550136133460E+00 0.31149146079327E+03 0.44415616601234E+03 0.42666647550727E+03 0.42054340324823E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15866780607985E+00 0.00000000000000E+00 -.19644052768073E+02
 0.96019326665034E-03 0.10163799571104E+01 0.80000000000000E+04 0.30000000000000E+04 0.78710721753545E+01
 0.29516520657579E+01 0.32960124041000E+03 0.29915000125189E+03 0.32640217026807E+03 0.35047133656334E+03
 0.29915000008439E+03 0.29915000016948E+03 0.32321446707448E+03 0.35043976920961E+03 0.29915000006641E+03
 0.29915000016875E+03 0.32640217026807E+03 0.35047133656334E+03 0.29915000008439E+03 0.29915000016948E+03
 0.32321446707448E+03 0.35043976920961E+03 0.29915000006641E+03 0.29915000016875E+03 0.39274966324555E+03
 0.31312175011342E+03 0.17604682835368E+04 0.16778987059930E+04 0.68040696291370E+03 0.11834292270864E+04
 0.49962022935812E+03 0.10757486641042E+04 0.11332003157108E+04 0.10189290925538E+04 0.18327406353003E+04
 0.95769301609122E+03 0.11326138112896E+04 0.91603806897764E+03 0.18324464114184E+04 0.10757486641042E+04
 0.11332003157108E+04 0.10189290925538E+04 0.18327406353003E+04 0.95769301609122E+03 0.11326138112896E+04
 0.91603806897764E+03 0.18324464114184E+04 0.17186735828666E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49233452790313E+03 0.12948492255852E+01
 0.12948492255852E+01 0.15913662507930E+01 0.87440780818993E-02 0.31262001241737E+03 0.33285521121381E+03
 0.33273725244163E+03 0.33272966460404E+03 0.23000000000000E+00 0.00000000000000E+00 0.18592226769013E+00
 0.00000000000000E+00 -.12601397392305E+02 0.20183436678882E-01 0.83394131331611E+00 0.39636460961926E+03
 0.14863672860722E+03 0.95930011767717E+01 0.35973754412894E+01 0.31311973671817E+03 0.39276313967169E+03
 0.30159064220951E+03 0.30691766699617E+03 0.29915000000102E+03 0.29915000001878E+03 0.30158157297317E+03
 0.30691676474016E+03 0.29915000000101E+03 0.29915000001878E+03 0.30159064220951E+03 0.30691766699617E+03
 0.29915000000102E+03 0.29915000001878E+03 0.30158157297317E+03 0.30691676474016E+03 0.29915000000101E+03
 0.29915000001878E+03 0.30573483569255E+03 0.29915000020503E+03 0.94258824228805E+02 0.93565005251207E+02
 0.14566091698382E+03 0.37686270237790E+03 0.23047348080916E+03 0.12463881736946E+03 0.13661219107813E+03
 0.12463881736946E+03 0.30100769122377E+03 0.12455444523080E+03 0.13650339133738E+03 0.12455444523080E+03
 0.30090578751508E+03 0.12463881736946E+03 0.13661219107813E+03 0.12463881736946E+03 0.30100769122377E+03
 0.12455444523080E+03 0.13650339133738E+03 0.12455444523080E+03 0.30090578751508E+03 0.20288430437603E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35677049577785E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17490431671621E+00 0.00000000000000E+00 0.00000000000000E+00 0.17490431671621E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18406255785765E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18406255785765E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555145893576E+00 0.18564783504432E+00 0.33285521121381E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    451.00246722
 0.39550632375911E+00 0.31169249498757E+03 0.44447929012500E+03 0.42697328439548E+03 0.42084292184680E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15800025384352E+00 0.00000000000000E+00 -.19644839502042E+02
 0.95957395753775E-03 0.10251494201377E+01 0.80000000000000E+04 0.30000000000000E+04 0.78037404527093E+01
 0.29264026697660E+01 0.32999868300323E+03 0.29915000154913E+03 0.32675703287208E+03 0.35104184208804E+03
 0.29915000010706E+03 0.29915000021537E+03 0.32354431179831E+03 0.35101058724608E+03 0.29915000008434E+03
 0.29915000021446E+03 0.32675703287208E+03 0.35104184208804E+03 0.29915000010706E+03 0.29915000021537E+03
 0.32354431179831E+03 0.35101058724608E+03 0.29915000008434E+03 0.29915000021446E+03 0.39342770562071E+03
 0.31365901417968E+03 0.17645541444239E+04 0.16808350708738E+04 0.67937629399108E+03 0.11781237550895E+04
 0.49535057962851E+03 0.10784401955766E+04 0.11355532860782E+04 0.10208825715080E+04 0.18325578267221E+04
 0.96059150134480E+03 0.11349813451322E+04 0.91835917587112E+03 0.18322749349943E+04 0.10784401955766E+04
 0.11355532860782E+04 0.10208825715080E+04 0.18325578267221E+04 0.96059150134480E+03 0.11349813451322E+04
 0.91835917587112E+03 0.18322749349943E+04 0.17181774397274E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49265017845456E+03 0.12948492313818E+01
 0.12948492313818E+01 0.16240259032064E+01 0.72939852128679E-02 0.31322665640743E+03 0.33294642102694E+03
 0.33285053057925E+03 0.33284452481267E+03 0.23000000000000E+00 0.00000000000000E+00 0.18489438091459E+00
 0.00000000000000E+00 -.12582981909679E+02 0.24196037624554E-01 0.85342161420738E+00 0.33063264837552E+03
 0.12398724314082E+03 0.93740302176786E+01 0.35152613316295E+01 0.31365707942396E+03 0.39344036453594E+03
 0.30166475240704E+03 0.30703159726830E+03 0.29915000000125E+03 0.29915000002385E+03 0.30165594193039E+03
 0.30703060692843E+03 0.29915000000124E+03 0.29915000002386E+03 0.30166475240704E+03 0.30703159726830E+03
 0.29915000000125E+03 0.29915000002385E+03 0.30165594193039E+03 0.30703060692843E+03 0.29915000000124E+03
 0.29915000002386E+03 0.30583270279944E+03 0.29915000025526E+03 0.93008002236636E+02 0.92440159567547E+02
 0.14725662950490E+03 0.37841611737510E+03 0.23042320472267E+03 0.12695422866018E+03 0.13806920901719E+03
 0.12695422866018E+03 0.30224519602001E+03 0.12688502855854E+03 0.13795733194035E+03 0.12688502855854E+03
 0.30214088460723E+03 0.12695422866018E+03 0.13806920901719E+03 0.12695422866018E+03 0.30224519602001E+03
 0.12688502855854E+03 0.13795733194035E+03 0.12688502855854E+03 0.30214088460723E+03 0.20341085444604E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35694341191488E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17477278005822E+00 0.00000000000000E+00 0.00000000000000E+00 0.17477278005822E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18399082146264E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18399082146264E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555184360575E+00 0.18559741772817E+00 0.33294642102694E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    460.58160183
 0.39551769346278E+00 0.31192491089681E+03 0.44485650899716E+03 0.42733090930750E+03 0.42119171915621E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15724137662416E+00 0.00000000000000E+00 -.19667504773012E+02
 0.95885876242974E-03 0.10350798930536E+01 0.80000000000000E+04 0.30000000000000E+04 0.77288719969226E+01
 0.28983269988460E+01 0.33045919696754E+03 0.29915000198186E+03 0.32716829286198E+03 0.35170451052715E+03
 0.29915000014108E+03 0.29915000028433E+03 0.32392645673155E+03 0.35167360352947E+03 0.29915000011130E+03
 0.29915000028315E+03 0.32716829286198E+03 0.35170451052715E+03 0.29915000014108E+03 0.29915000028433E+03
 0.32392645673155E+03 0.35167360352947E+03 0.29915000011130E+03 0.29915000028315E+03 0.39422143519742E+03
 0.31429432910409E+03 0.17692477469260E+04 0.16841897333038E+04 0.67801028468637E+03 0.11717533375880E+04
 0.49035300147823E+03 0.10815385753077E+04 0.11382203758585E+04 0.10231206553072E+04 0.18323006185108E+04
 0.96393262903461E+03 0.11376645555095E+04 0.92102728219010E+03 0.18320302438420E+04 0.10815385753077E+04
 0.11382203758585E+04 0.10231206553072E+04 0.18323006185108E+04 0.96393262903461E+03 0.11376645555095E+04
 0.92102728219010E+03 0.18320302438420E+04 0.17175109958725E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49301815498555E+03 0.12948493983800E+01
 0.12948493983800E+01 0.16623424416242E+01 0.58946847032448E-02 0.31393589314364E+03 0.33305677248050E+03
 0.33298163144387E+03 0.33297707390811E+03 0.23000000000000E+00 0.00000000000000E+00 0.18369915190002E+00
 0.00000000000000E+00 -.12586090599785E+02 0.29939775107820E-01 0.87604069745913E+00 0.26720307588117E+03
 0.10020115345544E+03 0.91319958344438E+01 0.34244984379164E+01 0.31429248486878E+03 0.39423323303494E+03
 0.30175072986493E+03 0.30716380253132E+03 0.29915000000162E+03 0.29915000003150E+03 0.30174220045044E+03
 0.30716271299663E+03 0.29915000000160E+03 0.29915000003151E+03 0.30175072986493E+03 0.30716380253132E+03
 0.29915000000162E+03 0.29915000003150E+03 0.30174220045044E+03 0.30716271299663E+03 0.29915000000160E+03
 0.29915000003151E+03 0.30594611188023E+03 0.29915000032890E+03 0.91410534564562E+02 0.90969376962074E+02
 0.14906410085563E+03 0.38022332944078E+03 0.23041390808088E+03 0.12964705257251E+03 0.13971806049098E+03
 0.12964705257251E+03 0.30368770179224E+03 0.12959338626855E+03 0.13960284740668E+03 0.12959338626855E+03
 0.30358080848483E+03 0.12964705257251E+03 0.13971806049098E+03 0.12964705257251E+03 0.30368770179224E+03
 0.12959338626855E+03 0.13960284740668E+03 0.12959338626855E+03 0.30358080848483E+03 0.20397457502744E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35714486259798E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17480296213253E+00 0.00000000000000E+00 0.00000000000000E+00 0.17480296213253E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18400191783980E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18400191783980E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555155345737E+00 0.18553561152849E+00 0.33305677248050E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    470.16073643
 0.39551415519907E+00 0.31215572251133E+03 0.44523351303276E+03 0.42768879640148E+03 0.42154080627986E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15650790660312E+00 0.00000000000000E+00 -.19675802739020E+02
 0.95814969249496E-03 0.10446329143293E+01 0.80000000000000E+04 0.30000000000000E+04 0.76581925480837E+01
 0.28718222055314E+01 0.33091535108905E+03 0.29915000251615E+03 0.32757587174201E+03 0.35235941862970E+03
 0.29915000018435E+03 0.29915000037216E+03 0.32430548766013E+03 0.35232884905645E+03 0.29915000014565E+03
 0.29915000037063E+03 0.32757587174201E+03 0.35235941862970E+03 0.29915000018435E+03 0.29915000037216E+03
 0.32430548766013E+03 0.35232884905645E+03 0.29915000014565E+03 0.29915000037063E+03 0.39500080077567E+03
 0.31493222541521E+03 0.17738620378376E+04 0.16874775454289E+04 0.67666842331524E+03 0.11655842080091E+04
 0.48553244257732E+03 0.10845873264914E+04 0.11408261056486E+04 0.10253166840230E+04 0.18320563181052E+04
 0.96722205282613E+03 0.11402854543048E+04 0.92364839462284E+03 0.18317976204746E+04 0.10845873264914E+04
 0.11408261056486E+04 0.10253166840230E+04 0.18320563181052E+04 0.96722205282612E+03 0.11402854543048E+04
 0.92364839462283E+03 0.18317976204746E+04 0.17168824209675E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49338686018424E+03 0.12948494595196E+01
 0.12948494595196E+01 0.17006589800420E+01 0.47629569254437E-02 0.31463438726050E+03 0.33317026076544E+03
 0.33311140372072E+03 0.33310794796626E+03 0.23000000000000E+00 0.00000000000000E+00 0.18251741901317E+00
 0.00000000000000E+00 -.12572871125461E+02 0.37053773754225E-01 0.89837055457117E+00 0.21590243555389E+03
 0.80963413332709E+02 0.89050113667391E+01 0.33393792625272E+01 0.31493047323549E+03 0.39501178812446E+03
 0.30183756869511E+03 0.30729540054832E+03 0.29915000000210E+03 0.29915000004128E+03 0.30182930920109E+03
 0.30729421404166E+03 0.29915000000208E+03 0.29915000004129E+03 0.30183756869511E+03 0.30729540054832E+03
 0.29915000000210E+03 0.29915000004128E+03 0.30182930920109E+03 0.30729421404166E+03 0.29915000000208E+03
 0.29915000004129E+03 0.30605897781533E+03 0.29915000042042E+03 0.89695626979129E+02 0.89351861400323E+02
 0.15080005430301E+03 0.38198404782600E+03 0.23042999325147E+03 0.13230804891924E+03 0.14129941201502E+03
 0.13230804891924E+03 0.30509099307755E+03 0.13226769518945E+03 0.14118114796978E+03 0.13226769518945E+03
 0.30498178557229E+03 0.13230804891924E+03 0.14129941201502E+03 0.13230804891924E+03 0.30509099307755E+03
 0.13226769518945E+03 0.14118114796978E+03 0.13226769518945E+03 0.30498178557229E+03 0.20447024592866E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35733633928940E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17471227093704E+00 0.00000000000000E+00 0.00000000000000E+00 0.17471227093704E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18392328660400E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18392328660400E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555173059499E+00 0.18547262361258E+00 0.33317026076544E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    481.89479181
 0.39547572797858E+00 0.31243759214997E+03 0.44569572385955E+03 0.42812893831058E+03 0.42197045919571E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15564232502272E+00 0.00000000000000E+00 -.19695259155776E+02
 0.95728510143059E-03 0.10558447159846E+01 0.80000000000000E+04 0.30000000000000E+04 0.75768717491186E+01
 0.28413269059195E+01 0.33146873544523E+03 0.29915000333193E+03 0.32807059091755E+03 0.35315182921763E+03
 0.29915000025251E+03 0.29915000051066E+03 0.32476597476081E+03 0.35312165985832E+03 0.29915000019986E+03
 0.29915000050860E+03 0.32807059091755E+03 0.35315182921763E+03 0.29915000025251E+03 0.29915000051066E+03
 0.32476597476081E+03 0.35312165985832E+03 0.29915000019986E+03 0.29915000050860E+03 0.39593771655153E+03
 0.31571602971660E+03 0.17794344126216E+04 0.16914472552504E+04 0.67506335753010E+03 0.11582933251640E+04
 0.47985465084626E+03 0.10882690578924E+04 0.11439538363184E+04 0.10279684826757E+04 0.18317920481366E+04
 0.97119589130554E+03 0.11434305732267E+04 0.92681415693272E+03 0.18315466019566E+04 0.10882690578924E+04
 0.11439538363184E+04 0.10279684826757E+04 0.18317920481366E+04 0.97119589130554E+03 0.11434305732267E+04
 0.92681415693272E+03 0.18315466019566E+04 0.17161829581464E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49384118193128E+03 0.12948496028749E+01
 0.12948496028749E+01 0.17475952015506E+01 0.36681557853493E-02 0.31547842263428E+03 0.33331272302129E+03
 0.33326911035986E+03 0.33326665126302E+03 0.23000000000000E+00 0.00000000000000E+00 0.18109090285686E+00
 0.00000000000000E+00 -.12567210819856E+02 0.48112875115735E-01 0.92528136247388E+00 0.16627565866218E+03
 0.62353371998317E+02 0.86460187403006E+01 0.32422570276127E+01 0.31571439990548E+03 0.39594773982716E+03
 0.30194507966465E+03 0.30745574707319E+03 0.29915000000290E+03 0.29915000005676E+03 0.30193713468330E+03
 0.30745444482368E+03 0.29915000000287E+03 0.29915000005678E+03 0.30194507966465E+03 0.30745574707319E+03
 0.29915000000290E+03 0.29915000005676E+03 0.30193713468330E+03 0.30745444482368E+03 0.29915000000287E+03
 0.29915000005678E+03 0.30619648993620E+03 0.29915000056114E+03 0.87446326349807E+02 0.87192774701768E+02
 0.15283188764411E+03 0.38408588141651E+03 0.23048983433418E+03 0.13552458888766E+03 0.14314698801831E+03
 0.13552458888766E+03 0.30676334440409E+03 0.13549790168149E+03 0.14302534154215E+03 0.13549790168149E+03
 0.30665163381043E+03 0.13552458888766E+03 0.14314698801831E+03 0.13552458888766E+03 0.30676334440409E+03
 0.13549790168149E+03 0.14302534154215E+03 0.13549790168149E+03 0.30665163381043E+03 0.20499687438235E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35756742773836E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17467879238510E+00 0.00000000000000E+00 0.00000000000000E+00 0.17467879238510E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18386521083482E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18386521083482E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555162384782E+00 0.18539324779588E+00 0.33331272302129E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    490.22492326
 0.39544062570593E+00 0.31263628636183E+03 0.44602344812606E+03 0.42844121388966E+03 0.42227526850951E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15504887919858E+00 0.00000000000000E+00 -.19704331422262E+02
 0.95667661866057E-03 0.10634923500439E+01 0.80000000000000E+04 0.30000000000000E+04 0.75223860328376E+01
 0.28208947623141E+01 0.33185786773189E+03 0.29915000404116E+03 0.32841862936615E+03 0.35370806158836E+03
 0.29915000031346E+03 0.29915000063466E+03 0.32509015438036E+03 0.35367816684625E+03 0.29915000024843E+03
 0.29915000063213E+03 0.32841862936615E+03 0.35370806158836E+03 0.29915000031346E+03 0.29915000063466E+03
 0.32509015438036E+03 0.35367816684625E+03 0.29915000024843E+03 0.29915000063213E+03 0.39659271333650E+03
 0.31627407253613E+03 0.17833278761411E+04 0.16942135363268E+04 0.67392569169954E+03 0.11532420299171E+04
 0.47594670975909E+03 0.10908442014526E+04 0.11461260600082E+04 0.10298190177300E+04 0.18316148768713E+04
 0.97397679715880E+03 0.11456143991809E+04 0.92902566174881E+03 0.18313781961017E+04 0.10908442014526E+04
 0.11461260600082E+04 0.10298190177300E+04 0.18316148768713E+04 0.97397679715879E+03 0.11456143991809E+04
 0.92902566174881E+03 0.18313781961017E+04 0.17157077918260E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49416379000830E+03 0.12948496697196E+01
 0.12948496697196E+01 0.17809157273529E+01 0.30468598480438E-02 0.31607283377538E+03 0.33341669152824E+03
 0.33338146199237E+03 0.33337953298451E+03 0.23000000000000E+00 0.00000000000000E+00 0.18009341969300E+00
 0.00000000000000E+00 -.12557767843891E+02 0.57923739541779E-01 0.94406736361750E+00 0.13811262986966E+03
 0.51792236201121E+02 0.84739715705725E+01 0.31777393389647E+01 0.31627253040503E+03 0.39660209860196E+03
 0.30202190809505E+03 0.30756891441024E+03 0.29915000000364E+03 0.29915000007067E+03 0.30201417375996E+03
 0.30756753224310E+03 0.29915000000360E+03 0.29915000007069E+03 0.30202190809505E+03 0.30756891441024E+03
 0.29915000000364E+03 0.29915000007067E+03 0.30201417375996E+03 0.30756753224310E+03 0.29915000000360E+03
 0.29915000007069E+03 0.30629352762225E+03 0.29915000068423E+03 0.85759230430451E+02 0.85556538009861E+02
 0.15422005465050E+03 0.38555230652767E+03 0.23056115160392E+03 0.13778553632998E+03 0.14440743669627E+03
 0.13778553632998E+03 0.30792698525556E+03 0.13776703262947E+03 0.14428359768038E+03 0.13776703262947E+03
 0.30781368915108E+03 0.13778553632998E+03 0.14440743669627E+03 0.13778553632998E+03 0.30792698525556E+03
 0.13776703262947E+03 0.14428359768038E+03 0.13776703262947E+03 0.30781368915108E+03 0.20532696640883E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35772751417143E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17461540755560E+00 0.00000000000000E+00 0.00000000000000E+00 0.17461540755560E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18378916553062E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18378916553062E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555169830531E+00 0.18533553296291E+00 0.33341669152824E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
    500.00000000
 0.39540122196696E+00 0.31286677982730E+03 0.44640639050985E+03 0.42880581542822E+03 0.42263094317330E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15437397715069E+00 0.00000000000000E+00 -.19715883652275E+02
 0.95597171219565E-03 0.10721514450714E+01 0.80000000000000E+04 0.30000000000000E+04 0.74616324370733E+01
 0.27981121639025E+01 0.33231034115557E+03 0.29915000504638E+03 0.32882347445295E+03 0.35435420616721E+03
 0.29915000040222E+03 0.29915000081535E+03 0.32546743321578E+03 0.35432462320475E+03 0.29915000031926E+03
 0.29915000081215E+03 0.32882347445295E+03 0.35435420616721E+03 0.29915000040222E+03 0.29915000081535E+03
 0.32546743321578E+03 0.35432462320475E+03 0.29915000031926E+03 0.29915000081215E+03 0.39735173129671E+03
 0.31693075891975E+03 0.17878165303829E+04 0.16973847294617E+04 0.67256667709803E+03 0.11473930353712E+04
 0.47146352488763E+03 0.10938183316535E+04 0.11486105714958E+04 0.10319450978348E+04 0.18313935818968E+04
 0.97719088736374E+03 0.11481117965428E+04 0.93157187407940E+03 0.18311665665362E+04 0.10938183316535E+04
 0.11486105714958E+04 0.10319450978348E+04 0.18313935818968E+04 0.97719088736374E+03 0.11481117965428E+04
 0.93157187407940E+03 0.18311665665362E+04 0.17151399128579E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49454054932520E+03 0.12948497548367E+01
 0.12948497548367E+01 0.18200160343200E+01 0.24500811239641E-02 0.31676692315814E+03 0.33354194394345E+03
 0.33351454383560E+03 0.33351309530392E+03 0.23000000000000E+00 0.00000000000000E+00 0.17894002192861E+00
 0.00000000000000E+00 -.12548444373297E+02 0.72032517116823E-01 0.96575555636369E+00 0.11106095302800E+03
 0.41647857385500E+02 0.82836696587302E+01 0.31063761220238E+01 0.31692931740518E+03 0.39736042034500E+03
 0.30211238652475E+03 0.30770091640913E+03 0.29915000000477E+03 0.29915000009101E+03 0.30210488505931E+03
 0.30769944292954E+03 0.29915000000472E+03 0.29915000009103E+03 0.30211238652475E+03 0.30770091640913E+03
 0.29915000000477E+03 0.29915000009101E+03 0.30210488505931E+03 0.30769944292954E+03 0.29915000000472E+03
 0.29915000009103E+03 0.30640668966211E+03 0.29915000085971E+03 0.83690299620450E+02 0.83536680005494E+02
 0.15579998604671E+03 0.38726266715038E+03 0.23068368117344E+03 0.14042382909215E+03 0.14584008942470E+03
 0.14042382909215E+03 0.30928130451045E+03 0.14041354750044E+03 0.14571385263392E+03 0.14041354750044E+03
 0.30916630439242E+03 0.14042382909215E+03 0.14584008942470E+03 0.14042382909215E+03 0.30928130451045E+03
 0.14041354750044E+03 0.14571385263392E+03 0.14041354750044E+03 0.30916630439242E+03 0.20567971227926E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35791533106881E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17454983169499E+00 0.00000000000000E+00 0.00000000000000E+00 0.17454983169499E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18372260456654E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18372260456654E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17555172445960E+00 0.18526598000680E+00 0.33354194394345E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29915000000000E+03
