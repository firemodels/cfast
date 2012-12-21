#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-27 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL27 0 MONOZONE(1=OUI,0=NON)                                                               
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
0.000000 0.096000                                                                                   
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
#CONDINIT 500.000000 10.000000 23.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-27           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-27           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-27           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-27           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-27           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-27           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-27           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-27           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-27           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-27           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-27           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-27           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-27           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-27           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-27           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-27           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-27           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-27           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL27     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL27     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL27     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL27     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL27     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL27     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL27     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL27     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL27     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL27     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL27     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL27     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL27     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL27     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL27     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL27     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL27     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL27     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.40054109154119E-12
 0.10000000000000E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.47663812341166E-03 0.47663812341166E-03 0.67511029172885E-03 0.67848584318749E-03
 0.00000000000000E+00 0.45167211999145E-03 0.53154326255225E-03 0.45167211999145E-03 0.53154326255225E-03
 0.44938572923703E-03 0.53153690258521E-03 0.44938572923703E-03 0.53153690258521E-03 0.45167211999145E-03
 0.53154326255225E-03 0.45167211999145E-03 0.53154326255225E-03 0.44938572923703E-03 0.53153690258521E-03
 0.44938572923703E-03 0.53153690258521E-03 0.54768429019760E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.33615544538701E-07 0.99965242536671E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.55505902004698E-03 0.55505902004698E-03
 0.68173959280659E-03 0.68514829077063E-03 0.00000000000000E+00 0.48517696880754E-03 0.53520879358666E-03
 0.48517696880754E-03 0.53520879358666E-03 0.48095413453083E-03 0.53520144310391E-03 0.48095413453083E-03
 0.53520144310391E-03 0.48517696880754E-03 0.53520879352857E-03 0.48517696880754E-03 0.53520879352857E-03
 0.48095413458892E-03 0.53520144304582E-03 0.48095413458892E-03 0.53520144304582E-03 0.44269610512877E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25921450501558E-05 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25921450501558E-05 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95999852800592E-01 0.11411747558035E+00 0.29615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     20.00000000
 0.30000000000000E+01 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.43733759037388E+01
 0.99995683813567E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29615000213176E+03 0.29615000000000E+03 0.29615000293085E+03 0.29615000293085E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000291570E+03 0.29615000291570E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000293085E+03 0.29615000293085E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000291570E+03 0.29615000291570E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615001016087E+03
 0.29615000839745E+03 0.49341130714826E-03 0.49341130714826E-03 0.63699917248756E-03 0.64018416835000E-03
 .00000000000000E+00 0.46078203001956E-03 0.54880809680449E-03 0.46078203001956E-03 0.54880809680449E-03
 0.45838947233739E-03 0.54888168184676E-03 0.45838947233739E-03 0.54888168184676E-03 0.46078203001956E-03
 0.54880809692066E-03 0.46078203001956E-03 0.54880809692066E-03 0.45838947193080E-03 0.54888168144017E-03
 0.45838947193080E-03 0.54888168144017E-03 0.54795791823881E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.22999999999994E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.48336633976209E+01 0.99962283912587E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615000837388E+03 0.29615001018947E+03
 0.29615000314722E+03 0.29615000314722E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000311961E+03
 0.29615000311961E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000314722E+03 0.29615000314722E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000311961E+03 0.29615000311961E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000303322E+03 0.29615000000000E+03 0.52842690327261E-03 0.52842690327261E-03
 0.69717674966639E-03 0.70066263341473E-03 .00000000000000E+00 0.49477761684313E-03 0.54313704607624E-03
 0.49477761684313E-03 0.54313704607624E-03 0.49042942508649E-03 0.54327591361429E-03 0.49042942508649E-03
 0.54327591361429E-03 0.49477761684313E-03 0.54313704607624E-03 0.49477761684313E-03 0.54313704607624E-03
 0.49042942473798E-03 0.54327591326578E-03 0.49042942473798E-03 0.54327591326578E-03 0.44289299082619E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11402025735285E+00 0.00000000000000E+00 0.00000000000000E+00 0.11402025735285E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013032801E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013032801E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95922462671187E-01 0.11402003853269E+00 0.29615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     30.00000000
 0.30000000000000E+01 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.43733759031121E+01
 0.99995683813567E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29615000264743E+03 0.29615000000000E+03 0.29615000363005E+03 0.29615000363005E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000361120E+03 0.29615000361120E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000363005E+03 0.29615000363005E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000361120E+03 0.29615000361120E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615001240669E+03
 0.29615001026109E+03 0.49700656558572E-03 0.49700656558572E-03 0.62874302022478E-03 0.63188673532590E-03
 .00000000000000E+00 0.46265037888293E-03 0.55244411026157E-03 0.46265037888293E-03 0.55244411026157E-03
 0.46023774079339E-03 0.55253716193185E-03 0.46023774079339E-03 0.55253716193185E-03 0.46265037888293E-03
 0.55244411031965E-03 0.46265037888293E-03 0.55244411031965E-03 0.46023774085147E-03 0.55253716198993E-03
 0.46023774085147E-03 0.55253716198993E-03 0.54794558641403E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.22999999999994E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.48336633970219E+01 0.99963150891034E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615001024227E+03 0.29615001242944E+03
 0.29615000389792E+03 0.29615000389792E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000386367E+03
 0.29615000386367E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000389792E+03 0.29615000389792E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000386367E+03 0.29615000386367E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000375871E+03 0.29615000000000E+03 0.52268699819770E-03 0.52268699819770E-03
 0.70039808640436E-03 0.70390007683638E-03 .00000000000000E+00 0.49677095580169E-03 0.54478398601317E-03
 0.49677095580169E-03 0.54478398601317E-03 0.49239937816250E-03 0.54495797576420E-03 0.49239937816250E-03
 0.54495797576420E-03 0.49677095574361E-03 0.54478398601317E-03 0.49677095574361E-03 0.54478398601317E-03
 0.49239937810442E-03 0.54495797570611E-03 0.49239937810442E-03 0.54495797570611E-03 0.44288759542327E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11402025734469E+00 0.00000000000000E+00 0.00000000000000E+00 0.11402025734469E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013033158E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013033158E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95922462671196E-01 0.11402003853270E+00 0.29615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     40.00000000
 0.30000000000000E+01 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.43733759025338E+01
 0.99995683813568E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29615000309360E+03 0.29615000000000E+03 0.29615000423235E+03 0.29615000423235E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000421032E+03 0.29615000421032E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000423235E+03 0.29615000423235E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000421032E+03 0.29615000421032E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615001429225E+03
 0.29615001182994E+03 0.50000776244250E-03 0.50000776244250E-03 0.62188402015158E-03 0.62499344025234E-03
 .00000000000000E+00 0.46419349449981E-03 0.55545612368004E-03 0.46419349449981E-03 0.55545612368004E-03
 0.46176509156827E-03 0.55556600830897E-03 0.46176509156827E-03 0.55556600830897E-03 0.46419349455789E-03
 0.55545612368004E-03 0.46419349455789E-03 0.55545612368004E-03 0.46176509162636E-03 0.55556600830897E-03
 0.46176509162636E-03 0.55556600830897E-03 0.54793356866050E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.22999999999994E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.48336633964467E+01 0.99963995365070E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615001181562E+03 0.29615001430949E+03
 0.29615000454458E+03 0.29615000454458E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000450462E+03
 0.29615000450462E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000454458E+03 0.29615000454458E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000450462E+03 0.29615000450462E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000438443E+03 0.29615000000000E+03 0.51792587012326E-03 0.51792587012326E-03
 0.70306555373424E-03 0.70658088150291E-03 .00000000000000E+00 0.49842092217565E-03 0.54614735349360E-03
 0.49842092217565E-03 0.54614735349360E-03 0.49403121344636E-03 0.54635161493358E-03 0.49403121344636E-03
 0.54635161493358E-03 0.49842092217565E-03 0.54614735349360E-03 0.49842092217565E-03 0.54614735349360E-03
 0.49403121280743E-03 0.54635161423656E-03 0.49403121280743E-03 0.54635161423656E-03 0.44288113195713E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11402025733710E+00 0.00000000000000E+00 0.00000000000000E+00 0.11402025733710E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013033335E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013033335E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95922462671206E-01 0.11402003853271E+00 0.29615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     40.00025000
 0.30000000000000E+01 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.43733759025319E+01
 0.99995683813568E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29615000309361E+03 0.29615000000000E+03 0.29615000423236E+03 0.29615000423236E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000421033E+03 0.29615000421033E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000423236E+03 0.29615000423236E+03 0.29615000000000E+03 0.29615000000000E+03
 0.29615000421033E+03 0.29615000421033E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615001429229E+03
 0.29615001182998E+03 0.50001360272713E-03 0.50001360272713E-03 0.62189211517183E-03 0.62500157574769E-03
 .00000000000000E+00 0.46419985103986E-03 0.55546076817818E-03 0.46419985103986E-03 0.55546076817818E-03
 0.46177149858379E-03 0.55557065344605E-03 0.46177149858379E-03 0.55557065344605E-03 0.46419985103986E-03
 0.55546076823627E-03 0.46419985103986E-03 0.55546076823627E-03 0.46177149846762E-03 0.55557065344605E-03
 0.46177149846762E-03 0.55557065344605E-03 0.54794074885448E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29615000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29615000000000E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000000000E+03 0.22999999999994E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.48336633963929E+01 0.99963995386180E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615001181566E+03 0.29615001430954E+03
 0.29615000454460E+03 0.29615000454460E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000450464E+03
 0.29615000450464E+03 0.29615000000000E+03 0.29615000000000E+03 0.29615000454460E+03 0.29615000454460E+03
 0.29615000000000E+03 0.29615000000000E+03 0.29615000450464E+03 0.29615000450464E+03 0.29615000000000E+03
 0.29615000000000E+03 0.29615000438445E+03 0.29615000000000E+03 0.51793341373712E-03 0.51793341373712E-03
 0.70308103537450E-03 0.70659644055137E-03 .00000000000000E+00 0.49843155768782E-03 0.54615590789467E-03
 0.49843155768782E-03 0.54615590789467E-03 0.49404202565171E-03 0.54636016997358E-03 0.49404202565171E-03
 0.54636016997358E-03 0.49843155768782E-03 0.54615590795276E-03 0.49843155768782E-03 0.54615590795276E-03
 0.49404202501278E-03 0.54636016921849E-03 0.49404202501278E-03 0.54636016921849E-03 0.44288988845291E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29615000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11402025733710E+00 0.00000000000000E+00 0.00000000000000E+00 0.11402025733710E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013032617E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11402013032617E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95922462671206E-01 0.11402003853272E+00 0.29615000000000E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     50.02130805
 0.22326036113215E+01 0.29619846092267E+03 0.33172828742502E+03 0.30528694777210E+03 0.30454211788041E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22597144078343E+00 0.00000000000000E+00 0.41928184678156E+02
 0.10002501216714E-02 0.72414421445895E-01 0.79979995269905E+04 0.29992498226215E+04 0.11047523187045E+03
 0.41428211951420E+02 0.29748056499781E+03 0.29615000000001E+03 0.29730232350802E+03 0.29772059156459E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29702743415866E+03 0.29770851171288E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29730232350802E+03 0.29772059156459E+03 0.29615000000002E+03 0.29615000000002E+03
 0.29702743415866E+03 0.29770851171288E+03 0.29615000000002E+03 0.29615000000002E+03 0.30044569625561E+03
 0.29615398066226E+03 0.61081461172745E+03 0.60828517782679E+03 0.28541253188517E+03 0.58773800495769E+03
 0.30089841041309E+03 0.39028614663442E+03 0.19100296096110E+03 0.38859540701144E+03 0.50315211038689E+03
 0.29355480384937E+03 0.18632739145683E+03 0.29239244329349E+03 0.49860541772467E+03 0.39028614663442E+03
 0.19100296096110E+03 0.38859540701144E+03 0.50315211038689E+03 0.29355480384937E+03 0.18632739145683E+03
 0.29239244329349E+03 0.49860541772467E+03 0.52032311218931E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.35706388515661E+03 0.12943957531437E+01
 0.12943957531437E+01 0.20131577152263E-01 0.13642473139409E+01 0.29618497286639E+03 0.30050497202559E+03
 0.29657594052615E+03 0.29657082205707E+03 0.23000000000000E+00 0.00000000000000E+00 0.22942601312416E+00
 0.00000000000000E+00 0.41881923202809E+02 0.99998292781226E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29615397127699E+03 0.30045632430139E+03
 0.29615206216987E+03 0.29628256530631E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615208384451E+03
 0.29628256805521E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615206216987E+03 0.29628256530631E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29615208384451E+03 0.29628256805521E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29619833266851E+03 0.29615000000001E+03 0.54656995184695E+00 0.55530568277830E+00
 0.62915593862982E+00 0.23218355166594E+02 0.22586053448271E+02 0.68641996141232E+00 -.89774717715620E-01
 0.70510216120366E+00 0.36329402891493E+02 0.69340847362820E+00 -.88897310693743E-01 0.71207488730028E+00
 0.36330254220743E+02 0.68641996141226E+00 -.89774717715620E-01 0.70510216120360E+00 0.36329402891493E+02
 0.69340847362820E+00 -.88897310693623E-01 0.71207488730028E+00 0.36330254220743E+02 0.68851984022794E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30491403069319E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.33647366462234E+00 0.00000000000000E+00 0.00000000000000E+00 0.33647366462234E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10697814624533E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10697814624533E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96667766158078E-01 0.11494538142967E+00 0.29618497286639E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     60.09224803
 0.16810304426503E+01 0.29624173384938E+03 0.35678282146584E+03 0.32285901769437E+03 0.32012414863264E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22277577679729E+00 0.00000000000000E+00 0.28229259329542E+02
 0.99996885609227E-03 0.12633281354907E+00 0.80000000000000E+04 0.30000000000000E+04 0.63324798801324E+02
 0.23746799550496E+02 0.29861234888470E+03 0.29615000000002E+03 0.29850408170683E+03 0.29980025062165E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29792257087041E+03 0.29977291902201E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29850408170683E+03 0.29980025062165E+03 0.29615000000002E+03 0.29615000000002E+03
 0.29792257087041E+03 0.29977291902201E+03 0.29615000000002E+03 0.29615000000002E+03 0.30623451681698E+03
 0.29616020375769E+03 0.65159835725137E+03 0.64586242090180E+03 0.30313185949238E+03 0.82461902412818E+03
 0.51997150533833E+03 0.44445110361618E+03 0.25540145417741E+03 0.44003814523629E+03 0.73856453004585E+03
 0.33344121645447E+03 0.24993952952998E+03 0.33044875038283E+03 0.73338143615064E+03 0.44445110361618E+03
 0.25540145417741E+03 0.44003814523629E+03 0.73856453004585E+03 0.33344121645447E+03 0.24993952952998E+03
 0.33044875038283E+03 0.73338143615064E+03 0.68040312051479E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.38312076919060E+03 0.12944966107925E+01
 0.12944966107925E+01 0.60415337081219E-01 0.12193897861485E+01 0.29617472083212E+03 0.30345063052449E+03
 0.29753585054862E+03 0.29750920022555E+03 0.23000000000000E+00 0.00000000000000E+00 0.22897243552438E+00
 0.00000000000000E+00 0.29738642941806E+02 0.99989770296100E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29616016710269E+03 0.30627222368832E+03
 0.29615489983450E+03 0.29641414961753E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615493001226E+03
 0.29641417103244E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615489983450E+03 0.29641414961753E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29615493001226E+03 0.29641417103244E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29628335457877E+03 0.29615000000002E+03 0.89103969352369E+00 0.89443431341059E+00
 0.47669088395397E+00 0.43313336951979E+02 0.42834262613605E+02 0.10103684955340E+01 -.40910929224736E+00
 0.10203076844455E+01 0.50386252000595E+02 0.10142796077216E+01 -.40267946358533E+00 0.10241994789496E+01
 0.50392506944371E+02 0.10103684955341E+01 -.40910929224741E+00 0.10203076844456E+01 0.50386252000595E+02
 0.10142796077215E+01 -.40267946358544E+00 0.10241994789495E+01 0.50392506944371E+02 0.13686995112581E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31040999151478E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.27416779743021E+00 0.00000000000000E+00 0.00000000000000E+00 0.27416779743021E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.98599238791037E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98599238791037E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96474586951330E-01 0.11470589975651E+00 0.29617472083212E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     70.00084913
 0.13049049924847E+01 0.29632598445131E+03 0.37112151051978E+03 0.33858782539235E+03 0.33440691312712E+03
 0.23000000000000E+00 0.00000000000000E+00 0.22055206400830E+00 0.00000000000000E+00 0.17961044878348E+02
 0.99958326844018E-03 0.16003617693390E+00 0.80000000000000E+04 0.30000000000000E+04 0.49988697263772E+02
 0.18745761473915E+02 0.29947259604569E+03 0.29615000000002E+03 0.29942412859573E+03 0.30173271436327E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29862431985752E+03 0.30169636473375E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29942412859573E+03 0.30173271436327E+03 0.29615000000002E+03 0.29615000000002E+03
 0.29862431985752E+03 0.30169636473375E+03 0.29615000000002E+03 0.29615000000002E+03 0.31123030847423E+03
 0.29616788689931E+03 0.70445770145365E+03 0.69609615400400E+03 0.33339509856707E+03 0.96197037408944E+03
 0.62690830002953E+03 0.48919056265948E+03 0.32202356323958E+03 0.48240727459899E+03 0.89621576183499E+03
 0.37095076916265E+03 0.31658336473994E+03 0.36635712666887E+03 0.89114237426016E+03 0.48919056265948E+03
 0.32202356323958E+03 0.48240727459899E+03 0.89621576183499E+03 0.37095076916265E+03 0.31658336473994E+03
 0.36635712666887E+03 0.89114237426016E+03 0.81486652834911E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39493083572105E+03 0.12945722225196E+01
 0.12945722225196E+01 0.10004974145948E+00 0.10904246688598E+01 0.29616726365458E+03 0.30559754816713E+03
 0.29874220492256E+03 0.29868395149254E+03 0.23000000000000E+00 0.00000000000000E+00 0.22857739621151E+00
 0.00000000000000E+00 0.20800276917919E+02 0.99983466654348E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29616783807434E+03 0.31126952406164E+03
 0.29615814847228E+03 0.29653815207299E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615817363983E+03
 0.29653820504836E+03 0.29615000000002E+03 0.29615000000002E+03 0.29615814847228E+03 0.29653815207299E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29615817363983E+03 0.29653820504836E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29637850451014E+03 0.29615000000002E+03 0.13142507770519E+01 0.13141313389997E+01
 0.38090887563467E+00 0.59160297117242E+02 0.58777483697229E+02 0.14179176034753E+01 -.60666374171240E+00
 0.14216934754752E+01 0.59944525383999E+02 0.14189608187281E+01 -.59482817597380E+00 0.14227234805132E+01
 0.59955953815036E+02 0.14179176034754E+01 -.60666374171235E+00 0.14216934754752E+01 0.59944525383999E+02
 0.14189608187282E+01 -.59482817597398E+00 0.14227234805132E+01 0.59955953815036E+02 0.19734736839486E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31444351753909E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.22582813753542E+00 0.00000000000000E+00 0.00000000000000E+00 0.22582813753542E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96329307502812E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96329307502812E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96332144902605E-01 0.11452931896888E+00 0.29616726365458E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     80.00775763
 0.10041741679304E+01 0.29646192866312E+03 0.38137319728035E+03 0.35295129644314E+03 0.34800942164479E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21863261085340E+00 0.00000000000000E+00 0.10150641249975E+02
 0.99904790257004E-03 0.18791253023803E+00 0.80000000000000E+04 0.30000000000000E+04 0.42572999202694E+02
 0.15964874701010E+02 0.30038004136830E+03 0.29615000000002E+03 0.30028797181713E+03 0.30359473067221E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29930493524751E+03 0.30355308814929E+03 0.29615000000002E+03
 0.29615000000002E+03 0.30028797181713E+03 0.30359473067221E+03 0.29615000000002E+03 0.29615000000002E+03
 0.29930493524751E+03 0.30355308814929E+03 0.29615000000002E+03 0.29615000000002E+03 0.31576877803867E+03
 0.29617729099821E+03 0.80387052620321E+03 0.79267820884721E+03 0.35443796131238E+03 0.10514877865738E+04
 0.69527763545488E+03 0.53244658563110E+03 0.37821381204748E+03 0.52332184680878E+03 0.10136207796437E+04
 0.41032771142330E+03 0.37331243768883E+03 0.40413011984133E+03 0.10091374493530E+04 0.53244658563110E+03
 0.37821381204748E+03 0.52332184680878E+03 0.10136207796437E+04 0.41032771142330E+03 0.37331243768883E+03
 0.40413011984133E+03 0.10091374493530E+04 0.94201222613118E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40552375456399E+03 0.18164705690005E+01
 0.18164705690005E+01 0.14007737544497E+00 0.97481454369738E+00 0.29616184303552E+03 0.30737532621199E+03
 0.30008794855471E+03 0.29999366255333E+03 0.23000000000000E+00 0.00000000000000E+00 0.22819314077906E+00
 0.00000000000000E+00 0.14169464354611E+02 0.99978752569059E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29617723194583E+03 0.31580186395899E+03
 0.29616201722398E+03 0.29666017379100E+03 0.29615000000002E+03 0.29615000000002E+03 0.29616202903856E+03
 0.29666026592710E+03 0.29615000000002E+03 0.29615000000002E+03 0.29616201722398E+03 0.29666017379100E+03
 0.29615000000002E+03 0.29615000000002E+03 0.29616202903856E+03 0.29666026592710E+03 0.29615000000002E+03
 0.29615000000002E+03 0.29648046725345E+03 0.29615000000002E+03 0.17649051301184E+01 0.17576253610580E+01
 0.27390741642647E+00 0.72847060889996E+02 0.72571783936487E+02 0.18444210132726E+01 -.76783860628623E+00
 0.18444210132726E+01 0.67884379076851E+02 0.18421418657122E+01 -.75083483431869E+00 0.18421418657122E+01
 0.67900696503709E+02 0.18444210132726E+01 -.76783860628635E+00 0.18444210132726E+01 0.67884379076851E+02
 0.18421418657121E+01 -.75083483431863E+00 0.18421418657121E+01 0.67900696503709E+02 0.25395603940526E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31792549520594E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.18513139082801E+00 0.00000000000000E+00 0.00000000000000E+00 0.18513139082801E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.95127750357034E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.95127750357034E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96226339701439E-01 0.11439813315854E+00 0.29616184303552E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
     90.07628038
 0.76660399753424E+00 0.29666792727826E+03 0.38975361140791E+03 0.36596699221891E+03 0.36082307871833E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21676726310411E+00 0.00000000000000E+00 0.40883362431533E+01
 0.99829446343229E-03 0.21434197801358E+00 0.80000000000000E+04 0.30000000000000E+04 0.37323533514714E+02
 0.13996325068018E+02 0.30135205238541E+03 0.29615000000002E+03 0.30113910553352E+03 0.30542918094223E+03
 0.29615000000003E+03 0.29615000000003E+03 0.29999587277611E+03 0.30538458537022E+03 0.29615000000003E+03
 0.29615000000003E+03 0.30113910553352E+03 0.30542918094223E+03 0.29615000000003E+03 0.29615000000003E+03
 0.29999587277611E+03 0.30538458537022E+03 0.29615000000003E+03 0.29615000000003E+03 0.32005324695981E+03
 0.29618836945103E+03 0.90493420612851E+03 0.89074665132398E+03 0.38241763765378E+03 0.11300507809674E+04
 0.74572105512537E+03 0.57673847533212E+03 0.43637282625367E+03 0.56529091201019E+03 0.11190175933403E+04
 0.45211167974938E+03 0.43199449099324E+03 0.44431197622697E+03 0.11150852825616E+04 0.57673847533212E+03
 0.43637282625367E+03 0.56529091201019E+03 0.11190175933403E+04 0.45211167974938E+03 0.43199449099324E+03
 0.44431197622697E+03 0.11150852825616E+04 0.10633820948273E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41570772054568E+03 0.16311705511189E+01
 0.16311705511189E+01 0.18035146644323E+00 0.88009579088650E+00 0.29615790188085E+03 0.30895315101451E+03
 0.30144578774392E+03 0.30131505800739E+03 0.23000000000000E+00 0.00000000000000E+00 0.22779917903770E+00
 0.00000000000000E+00 0.91413613019090E+01 0.99975120653190E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29618830093993E+03 0.32008116532062E+03
 0.29616653275388E+03 0.29678274788841E+03 0.29615000000003E+03 0.29615000000003E+03 0.29616652557761E+03
 0.29678288285899E+03 0.29615000000003E+03 0.29615000000003E+03 0.29616653275388E+03 0.29678274788841E+03
 0.29615000000003E+03 0.29615000000003E+03 0.29616652557761E+03 0.29678288285899E+03 0.29615000000003E+03
 0.29615000000003E+03 0.29658766170890E+03 0.29615000000002E+03 0.22479275595263E+01 0.22308791450097E+01
 0.18601071582997E+00 0.85331104005024E+02 0.85144163235615E+02 0.23074787191417E+01 -.89656756773461E+00
 0.23037951421806E+01 0.75439860180202E+02 0.23020940598319E+01 -.87516855246028E+00 0.22984143453345E+01
 0.75460267296442E+02 0.23074787191417E+01 -.89656756773466E+00 0.23037951421805E+01 0.75439860180202E+02
 0.23020940598318E+01 -.87516855246028E+00 0.22984143453344E+01 0.75460267296442E+02 0.30774118596924E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32100451995126E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.14879984564349E+00 0.00000000000000E+00 0.00000000000000E+00 0.14879984564349E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.94383490000632E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.94383490000632E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96146030425917E-01 0.11219247707357E+00 0.30171724978796E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    100.02568504
 0.58089582159351E+00 0.29696465977989E+03 0.39680200488505E+03 0.37747030601485E+03 0.37254988607386E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21491278505724E+00 0.00000000000000E+00 -.44424310083235E+00
 0.99725233904147E-03 0.24021983620804E+00 0.80000000000000E+04 0.30000000000000E+04 0.33302828468635E+02
 0.12488560675738E+02 0.30235664744648E+03 0.29615000000003E+03 0.30198205094173E+03 0.30721919729557E+03
 0.29615000000003E+03 0.29615000000003E+03 0.30069885282621E+03 0.30717300023584E+03 0.29615000000003E+03
 0.29615000000003E+03 0.30198205094173E+03 0.30721919729557E+03 0.29615000000003E+03 0.29615000000003E+03
 0.30069885282621E+03 0.30717300023584E+03 0.29615000000003E+03 0.29615000000003E+03 0.32412872884710E+03
 0.29620090547244E+03 0.10044290676208E+04 0.98733243581869E+03 0.41433555451886E+03 0.11983751023604E+04
 0.78196787006891E+03 0.62135145089388E+03 0.49509130907947E+03 0.60768897800262E+03 0.12130277272957E+04
 0.49528031491647E+03 0.49117053351566E+03 0.48596202116790E+03 0.12095664443817E+04 0.62135145089388E+03
 0.49509130907947E+03 0.60768897800262E+03 0.12130277272957E+04 0.49528031491647E+03 0.49117053351566E+03
 0.48596202116790E+03 0.12095664443817E+04 0.11757319847530E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42546999318089E+03 0.14817215537206E+01
 0.14817215537206E+01 0.22014908510205E+00 0.80650869001465E+00 0.29615529030733E+03 0.31038764457376E+03
 0.30273529964360E+03 0.30256956154078E+03 0.23000000000000E+00 0.00000000000000E+00 0.22738920906661E+00
 0.00000000000000E+00 0.54944663582233E+01 0.99972402991478E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29620082736791E+03 0.32415480216119E+03
 0.29617159526748E+03 0.29690619221986E+03 0.29615000000003E+03 0.29615000000003E+03 0.29617156550299E+03
 0.29690637127935E+03 0.29615000000003E+03 0.29615000000003E+03 0.29617159526748E+03 0.29690619221986E+03
 0.29615000000003E+03 0.29615000000003E+03 0.29617156550299E+03 0.29690637127935E+03 0.29615000000003E+03
 0.29615000000003E+03 0.29669755146361E+03 0.29615000000003E+03 0.27462662302420E+01 0.27180131714245E+01
 0.11093846557109E+00 0.96862574089842E+02 0.96751080931943E+02 0.27914287601683E+01 -.10103984077711E+01
 0.27830497544126E+01 0.82764883963174E+02 0.27831705831002E+01 -.98516801061214E+00 0.27748109171525E+01
 0.82788803474050E+02 0.27914287601684E+01 -.10103984077711E+01 0.27830497544127E+01 0.82764883963174E+02
 0.27831705831002E+01 -.98516801061219E+00 0.27748109171525E+01 0.82788803474050E+02 0.35828392727447E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32368298477058E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11673053589027E+00 0.00000000000000E+00 0.00000000000000E+00 0.11673053589027E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93917871537284E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93917871537284E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96087739570008E-01 0.11155170031093E+00 0.30325548591223E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    110.16305617
 0.42236868474048E+00 0.29740242064895E+03 0.40260269294405E+03 0.38779159272951E+03 0.38350361992714E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21305016808514E+00 0.00000000000000E+00 -.38039086378454E+01
 0.99575141786466E-03 0.26611208518374E+00 0.80000000000000E+04 0.30000000000000E+04 0.30062520439372E+02
 0.11273445164765E+02 0.30341937662225E+03 0.29615000000003E+03 0.30284795890316E+03 0.30901405284128E+03
 0.29615000000003E+03 0.29615000000003E+03 0.30143776852541E+03 0.30896709103760E+03 0.29615000000003E+03
 0.29615000000003E+03 0.30284795890316E+03 0.30901405284128E+03 0.29615000000003E+03 0.29615000000003E+03
 0.30143776852541E+03 0.30896709103760E+03 0.29615000000003E+03 0.29615000000003E+03 0.32807490828533E+03
 0.29621538720379E+03 0.11042068843266E+04 0.10844461511247E+04 0.44682989344827E+03 0.12530257854137E+04
 0.80396174249820E+03 0.66644011178326E+03 0.55239052229393E+03 0.65070578459446E+03 0.12970252844484E+04
 0.53970877143962E+03 0.54888007595396E+03 0.52902128030992E+03 0.12939800166741E+04 0.66644011178326E+03
 0.55239052229393E+03 0.65070578459446E+03 0.12970252844484E+04 0.53970877143962E+03 0.54888007595396E+03
 0.52902128030992E+03 0.12939800166741E+04 0.12798717878946E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43304337054569E+03 0.13539416473208E+01
 0.13539416473208E+01 0.26069856960394E+00 0.73988581955830E+00 0.29615381278041E+03 0.31174526687765E+03
 0.30405466968242E+03 0.30385469131871E+03 0.23000000000000E+00 0.00000000000000E+00 0.22694559966294E+00
 0.00000000000000E+00 0.29139237857435E+01 0.99970354904561E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29621529871949E+03 0.32810121818648E+03
 0.29617732145340E+03 0.29703343946936E+03 0.29615000000003E+03 0.29615000000003E+03 0.29617726527664E+03
 0.29703366489223E+03 0.29615000000003E+03 0.29615000000003E+03 0.29617732145340E+03 0.29703343946936E+03
 0.29615000000003E+03 0.29615000000003E+03 0.29617726527664E+03 0.29703366489223E+03 0.29615000000003E+03
 0.29615000000003E+03 0.29681266947315E+03 0.29615000000003E+03 0.32653863338461E+01 0.32212766855321E+01
 0.33986410555442E-01 0.10789036330539E+03 0.10785620696278E+03 0.32972313871745E+01 -.11193507026969E+01
 0.32836827124602E+01 0.89771565513679E+02 0.32860273216050E+01 -.10904454697582E+01 0.32725196111019E+01
 0.89798822496537E+02 0.32972313871745E+01 -.11193507026970E+01 0.32836827124602E+01 0.89771565513679E+02
 0.32860273216054E+01 -.10904454697581E+01 0.32725196111023E+01 0.89798822496538E+02 0.40801028362180E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32612355987873E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.88109884366956E-01 0.00000000000000E+00 0.00000000000000E+00 0.88109884366956E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93714947035607E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93714947035607E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.96018402301625E-01 0.11121383049797E+00 0.30394954352671E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    120.22532737
 0.29260073870811E+00 0.29804022946950E+03 0.40745601814919E+03 0.39678430461786E+03 0.39337086854325E+03
 0.23000000000000E+00 0.00000000000000E+00 0.21117615547028E+00 0.00000000000000E+00 -.61889394362975E+01
 0.12186884201834E-02 0.29210217829447E+00 0.65644342454620E+04 0.24616628420483E+04 0.27387676622991E+02
 0.10270378733622E+02 0.30450760938223E+03 0.29615000000003E+03 0.30371690801861E+03 0.31076080149264E+03
 0.29615000000004E+03 0.29615000000004E+03 0.30219450667651E+03 0.31071366484648E+03 0.29615000000004E+03
 0.29615000000004E+03 0.30371690801861E+03 0.31076080149264E+03 0.29615000000004E+03 0.29615000000004E+03
 0.30219450667651E+03 0.31071366484648E+03 0.29615000000004E+03 0.29615000000004E+03 0.33180584631509E+03
 0.29623183152105E+03 0.12005946510452E+04 0.11788757340763E+04 0.47895290032670E+03 0.12988929675845E+04
 0.81754530275616E+03 0.71072393444523E+03 0.60721839375593E+03 0.69332841664633E+03 0.13721252123064E+04
 0.58404311640596E+03 0.60406376123959E+03 0.57239208732972E+03 0.13694358394774E+04 0.71072393444523E+03
 0.60721839375593E+03 0.69332841664633E+03 0.13721252123064E+04 0.58404311640596E+03 0.60406376123959E+03
 0.57239208732972E+03 0.13694358394774E+04 0.13748029457894E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44001437096500E+03 0.12947500972826E+01
 0.12947500972826E+01 0.30094765442562E+00 0.68029593348843E+00 0.29615335603175E+03 0.31299888622296E+03
 0.30535892243194E+03 0.30512741571542E+03 0.23000000000000E+00 0.00000000000000E+00 0.22647719604859E+00
 0.00000000000000E+00 0.11927124972585E+01 0.99968810341823E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29623173278509E+03 0.33183337805499E+03
 0.29618352929397E+03 0.29716046716300E+03 0.29615000000004E+03 0.29615000000004E+03 0.29618344400332E+03
 0.29716073949842E+03 0.29615000000004E+03 0.29615000000004E+03 0.29618352929397E+03 0.29716046716300E+03
 0.29615000000004E+03 0.29615000000004E+03 0.29618344400332E+03 0.29716073949842E+03 0.29615000000004E+03
 0.29615000000004E+03 0.29692915105995E+03 0.29615000000003E+03 0.37882223527939E+01 0.37272576955434E+01
 -.33956658028965E-01 0.11817424606896E+03 0.11820837251028E+03 0.38150493886331E+01 -.12158637530281E+01
 0.37961481581058E+01 0.96296445207164E+02 0.38010467838469E+01 -.11836763566146E+01 0.37822131483435E+01
 0.96326640761665E+02 0.38150493886332E+01 -.12158637530281E+01 0.37961481581058E+01 0.96296445207164E+02
 0.38010467838469E+01 -.11836763566145E+01 0.37822131483435E+01 0.96326640761666E+02 0.45561000998907E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32832546704871E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.62713857744660E-01 0.00000000000000E+00 0.00000000000000E+00 0.62713857744660E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93729503281135E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93729503281135E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95989319478710E-01 0.11092508412552E+00 0.30464326810366E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    130.17991419
 0.18998650285322E+00 0.29899255726707E+03 0.41172512524314E+03 0.40458590312733E+03 0.40212338730630E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20924718920922E+00 0.00000000000000E+00 -.78096157434185E+01
 0.18769105263280E-02 0.31877453013301E+00 0.42623235832404E+04 0.15983713437152E+04 0.25096107887484E+02
 0.94110404578064E+01 0.30561625203719E+03 0.29615000000003E+03 0.30459190426619E+03 0.31246209625913E+03
 0.29615000000005E+03 0.29615000000005E+03 0.30297075842288E+03 0.31241518419917E+03 0.29615000000005E+03
 0.29615000000005E+03 0.30459190426619E+03 0.31246209625913E+03 0.29615000000005E+03 0.29615000000005E+03
 0.30297075842288E+03 0.31241518419917E+03 0.29615000000005E+03 0.29615000000005E+03 0.33536689211298E+03
 0.29625092577172E+03 0.12927846404401E+04 0.12704148537799E+04 0.51113355835042E+03 0.13404862793538E+04
 0.82679705321160E+03 0.75428663251819E+03 0.66059894236012E+03 0.73603830236114E+03 0.14401834253871E+04
 0.62821656358380E+03 0.65775025948775E+03 0.61641134261869E+03 0.14377954729374E+04 0.75428663251819E+03
 0.66059894236012E+03 0.73603830236113E+03 0.14401834253871E+04 0.62821656358380E+03 0.65775025948775E+03
 0.61641134261869E+03 0.14377954729374E+04 0.14616156983920E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44814046315274E+03 0.12947620363686E+01
 0.12947620363686E+01 0.34076600171717E+00 0.62779900091446E+00 0.29615379874633E+03 0.31416085828152E+03
 0.30662431562445E+03 0.30636447365276E+03 0.23000000000000E+00 0.00000000000000E+00 0.22598294931598E+00
 0.00000000000000E+00 0.13285622689851E+00 0.99967614880122E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29625081588187E+03 0.33539634004462E+03
 0.29619022067127E+03 0.29728692930427E+03 0.29615000000005E+03 0.29615000000005E+03 0.29619010445226E+03
 0.29728724788603E+03 0.29615000000005E+03 0.29615000000005E+03 0.29619022067127E+03 0.29728692930427E+03
 0.29615000000005E+03 0.29615000000005E+03 0.29619010445226E+03 0.29728724788603E+03 0.29615000000005E+03
 0.29615000000005E+03 0.29704654464502E+03 0.29615000000003E+03 0.43077216371196E+01 0.42266962881463E+01
 -.91038377521469E-01 0.12777378317606E+03 0.12786527674547E+03 0.43447713200010E+01 -.12990502763193E+01
 0.43203914487483E+01 0.10241873387611E+03 0.43281214508277E+01 -.12640283706841E+01 0.43038399594542E+01
 0.10245142227789E+03 0.43447713200011E+01 -.12990502763193E+01 0.43203914487483E+01 0.10241873387611E+03
 0.43281214508276E+01 -.12640283706848E+01 0.43038399594541E+01 0.10245142227789E+03 0.50089976548649E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33031873404850E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39994959448718E-01 0.00000000000000E+00 0.00000000000000E+00 0.39994959448718E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93820572171267E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93820572171267E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95970989722518E-01 0.11025664361941E+00 0.30642846190656E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    140.00071791
 0.11252861877431E+00 0.30042898519782E+03 0.41562334655175E+03 0.41130245909384E+03 0.40973044267723E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20723047975841E+00 0.00000000000000E+00 -.87898320924502E+01
 0.31688511379790E-02 0.34654994535911E+00 0.25245742547257E+04 0.94671534552212E+03 0.23084695603429E+02
 0.86567608512859E+01 0.30673970178501E+03 0.29615000000004E+03 0.30547575460061E+03 0.31413027602726E+03
 0.29615000000006E+03 0.29615000000006E+03 0.30376809177048E+03 0.31408385158868E+03 0.29615000000006E+03
 0.29615000000006E+03 0.30547575460061E+03 0.31413027602726E+03 0.29615000000006E+03 0.29615000000006E+03
 0.30376809177048E+03 0.31408385158868E+03 0.29615000000006E+03 0.29615000000006E+03 0.33879022407611E+03
 0.29627346982477E+03 0.13801945675996E+04 0.13592887872528E+04 0.54377493352834E+03 0.13795243818519E+04
 0.83303057365590E+03 0.79715412270787E+03 0.71329582221129E+03 0.77951252010483E+03 0.15069028977918E+04
 0.67208258223475E+03 0.71070977263666E+03 0.66159707456991E+03 0.15047714568874E+04 0.79715412270787E+03
 0.71329582221129E+03 0.77951252010483E+03 0.15069028977918E+04 0.67208258223475E+03 0.71070977263666E+03
 0.66159707456991E+03 0.15047714568874E+04 0.15435325854116E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45536496605692E+03 0.12947692574883E+01
 0.12947692574883E+01 0.38004921659827E+00 0.58617648366170E+00 0.29615512430586E+03 0.31524213409199E+03
 0.30778322990529E+03 0.30749756153229E+03 0.23000000000000E+00 0.00000000000000E+00 0.22546161645392E+00
 0.00000000000000E+00 -.38022759175708E+00 0.99966661052031E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29627334905993E+03 0.33882194368632E+03
 0.29619734382023E+03 0.29741268305327E+03 0.29615000000006E+03 0.29615000000006E+03 0.29619719508735E+03
 0.29741304657538E+03 0.29615000000006E+03 0.29615000000006E+03 0.29619734382023E+03 0.29741268305327E+03
 0.29615000000006E+03 0.29615000000006E+03 0.29619719508735E+03 0.29741304657538E+03 0.29615000000006E+03
 0.29615000000006E+03 0.29716400420527E+03 0.29615000000004E+03 0.48127045988265E+01 0.47072431309479E+01
 -.13454584946551E+00 0.13673255333288E+03 0.13686777191159E+03 0.48853263881815E+01 -.13705258484202E+01
 0.48555213418845E+01 0.10829811426849E+03 0.48661502491383E+01 -.13331403670797E+01 0.48364779250372E+01
 0.10833282866473E+03 0.48853263881816E+01 -.13705258484202E+01 0.48555213418846E+01 0.10829811426849E+03
 0.48661502491383E+01 -.13331403670797E+01 0.48364779250372E+01 0.10833282866473E+03 0.54309507157632E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33209347079000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.20113303646590E-01 0.15267318771867E-02 0.00000000000000E+00 0.20113303646590E-01 0.15267318771867E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93929646774779E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93929646774779E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95961498192888E-01 0.10831671524561E+00 0.31188409937714E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    150.00219307
 0.58415691233166E-01 0.30250821449234E+03 0.41938850407789E+03 0.41711262310866E+03 0.41625685013218E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20502366345098E+00 0.00000000000000E+00 -.94481245614579E+01
 0.61042752302242E-02 0.37681232151765E+00 0.13105568963191E+04 0.49145883611965E+03 0.21230728251611E+02
 0.79615230943542E+01 0.30790971993466E+03 0.29615000000014E+03 0.30642686113520E+03 0.31583325840675E+03
 0.29615000000017E+03 0.29615000000017E+03 0.30463596716621E+03 0.31578750776810E+03 0.29615000000017E+03
 0.29615000000017E+03 0.30642686113520E+03 0.31583325840675E+03 0.29615000000017E+03 0.29615000000017E+03
 0.30463596716621E+03 0.31578750776810E+03 0.29615000000017E+03 0.29615000000017E+03 0.34221235383241E+03
 0.29630128592903E+03 0.14639682820216E+04 0.14470509105064E+04 0.57813066362581E+03 0.14186980343245E+04
 0.83767671738056E+03 0.84002204877900E+03 0.76745173825937E+03 0.84002204877900E+03 0.15754531663396E+04
 0.71620076818424E+03 0.76509730024627E+03 0.71620076818424E+03 0.15735461891715E+04 0.84002204877900E+03
 0.76745173825937E+03 0.84002204877900E+03 0.15754531663396E+04 0.71620076818423E+03 0.76509730024627E+03
 0.71620076818423E+03 0.15735461891715E+04 0.16234320919610E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46224497158897E+03 0.12947741070937E+01
 0.12947741070937E+01 0.42005511722136E+00 0.55757055588558E+00 0.29615724269797E+03 0.31628435777922E+03
 0.30880283328307E+03 0.30849125154202E+03 0.23000000000000E+00 0.00000000000000E+00 0.22489151408266E+00
 0.00000000000000E+00 -.65189553639753E+00 0.99965677879166E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29630115358784E+03 0.34224656989880E+03
 0.29620508294600E+03 0.29754228096554E+03 0.29615000000017E+03 0.29615000000017E+03 0.29620489798291E+03
 0.29754269001720E+03 0.29615000000017E+03 0.29615000000017E+03 0.29620508294600E+03 0.29754228096554E+03
 0.29615000000017E+03 0.29615000000017E+03 0.29620489798291E+03 0.29754269001720E+03 0.29615000000017E+03
 0.29615000000017E+03 0.29728474575786E+03 0.29615000000014E+03 0.52986252977799E+01 0.51615586725052E+01
 -.18833492991384E+00 0.14532957468313E+03 0.14551885128769E+03 0.54327280467619E+01 -.14515175222570E+01
 0.53974575237283E+01 0.11421788997517E+03 0.54105441323499E+01 -.14116647618880E+01 0.53754459406872E+01
 0.11425472138256E+03 0.54327280467604E+01 -.14515175222570E+01 0.53974575237265E+01 0.11421788997517E+03
 0.54105441323499E+01 -.14116647618880E+01 0.53754459406872E+01 0.11425472138256E+03 0.58250020234546E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33369495447313E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.76585799336265E-02 0.12249959724747E-01 0.00000000000000E+00 0.76585799336265E-02 0.12249959724747E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.93989682717287E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.93989682717287E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95955839324405E-01 0.10680319023839E+00 0.31628435777922E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    160.01282435
 0.32999360881487E-01 0.30449494054895E+03 0.42314663533299E+03 0.42184149196786E+03 0.42134066125832E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20258795253226E+00 0.00000000000000E+00 -.10023730164614E+02
 0.10805808983563E-01 0.40997017579834E+00 0.74034253355477E+03 0.27762845008304E+03 0.19513614580430E+02
 0.73176054676612E+01 0.30908900370486E+03 0.29615000000028E+03 0.30741020282070E+03 0.31755924440806E+03
 0.29615000000028E+03 0.29615000000028E+03 0.30551609378458E+03 0.31751418348796E+03 0.29615000000028E+03
 0.29615000000028E+03 0.30741020282070E+03 0.31755924440806E+03 0.29615000000028E+03 0.29615000000028E+03
 0.30551609378458E+03 0.31751418348796E+03 0.29615000000028E+03 0.29615000000028E+03 0.34561483535645E+03
 0.29634991606811E+03 0.15347174123219E+04 0.15211403967286E+04 0.61482599151429E+03 0.14594306240544E+04
 0.84153050258256E+03 0.87791550554851E+03 0.82413606905197E+03 0.87791550554851E+03 0.16485249516867E+04
 0.75676343043415E+03 0.82168613446471E+03 0.75676343043415E+03 0.16465161700180E+04 0.87791550554851E+03
 0.82413606905197E+03 0.87791550554851E+03 0.16485249516867E+04 0.75676343043415E+03 0.82168613446472E+03
 0.75676343043415E+03 0.16465161700181E+04 0.16985694523542E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.19794444790993E+06 0.50050000000000E+08 0.46771808185582E+03 0.12863405392449E+01
 0.12863405392449E+01 0.46009764234623E+00 0.53233933209473E+00 0.29616018927692E+03 0.31725065429726E+03
 0.30976579825560E+03 0.30943041974001E+03 0.23000000000000E+00 0.00000000000000E+00 0.22427601078946E+00
 0.00000000000000E+00 -.94104758309710E+00 0.99964397922095E-03 0.16290240303944E-01 0.80000000000000E+04
 0.30000000000000E+04 0.49109158924214E+03 0.18415934596580E+03 0.29634974341692E+03 0.34565151966749E+03
 0.29621687435030E+03 0.29767992193700E+03 0.29615000000028E+03 0.29615000000028E+03 0.29621656905300E+03
 0.29768037503219E+03 0.29615000000028E+03 0.29615000000028E+03 0.29621687435030E+03 0.29767992193700E+03
 0.29615000000028E+03 0.29615000000028E+03 0.29621656905300E+03 0.29768037503219E+03 0.29615000000028E+03
 0.29615000000028E+03 0.29741075731504E+03 0.29615000000028E+03 0.78428877890066E+01 0.76449922898473E+01
 0.23721205551868E+01 0.15583945575568E+03 0.15345547459772E+03 0.72634987258898E+01 0.10730760748100E+01
 0.72193695702018E+01 0.12235362658595E+03 0.72098016705556E+01 0.11147218160698E+01 0.71659728772195E+01
 0.12239190707461E+03 0.72634987258898E+01 0.10730760748100E+01 0.72193695702018E+01 0.12235362658595E+03
 0.72098016705555E+01 0.11147218160697E+01 0.71659728772193E+01 0.12239190707461E+03 0.63684946199581E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33497505223263E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.19041333098472E-03 0.28695287092026E-01 0.00000000000000E+00 0.19041333098472E-03 0.28695287092026E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.94403364725071E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.94403364725071E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95950018838155E-01 0.10647112153029E+00 0.31725065429726E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    170.16442634
 0.24669923865962E-01 0.30514590363849E+03 0.42673860075824E+03 0.42573870656470E+03 0.42534484179739E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19998529174348E+00 0.00000000000000E+00 -.10555274396872E+02
 0.14454213189639E-01 0.44479964912958E+00 0.55347184208787E+03 0.20755194078295E+03 0.17985625698345E+02
 0.67446096368795E+01 0.30992046522334E+03 0.29615000000032E+03 0.30807910467590E+03 0.31934417272144E+03
 0.29615000000030E+03 0.29615000000030E+03 0.30622337454003E+03 0.31927734431555E+03 0.29615000000030E+03
 0.29615000000030E+03 0.30807910467590E+03 0.31934417272144E+03 0.29615000000030E+03 0.29615000000030E+03
 0.30622337454003E+03 0.31927734431555E+03 0.29615000000030E+03 0.29615000000030E+03 0.34898697875823E+03
 0.29642646684032E+03 0.14661860939440E+04 0.14519160180713E+04 0.64929721196188E+03 0.14944776479248E+04
 0.84193394990315E+03 0.83450380273459E+03 0.88730089078416E+03 0.83450380273459E+03 0.17260205083347E+04
 0.74733563859815E+03 0.87895418380186E+03 0.74733563859815E+03 0.17183284268292E+04 0.83450380273459E+03
 0.88730089078416E+03 0.83450380273459E+03 0.17260205083347E+04 0.74733563859814E+03 0.87895418380187E+03
 0.74733563859814E+03 0.17183284268292E+04 0.17363451019155E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.14641948722259E+06 0.50050000000000E+08 0.47104928271071E+03 0.11762495449529E+01
 0.11762495449529E+01 0.50070405032154E+00 0.50723136172549E+00 0.29616480832429E+03 0.31810142418739E+03
 0.31068346449680E+03 0.31032870521005E+03 0.23000000000000E+00 0.00000000000000E+00 0.22360763792520E+00
 0.00000000000000E+00 -.12548940083871E+01 0.99962529118983E-03 0.35641687176398E-01 0.80000000000000E+04
 0.30000000000000E+04 0.22445626550748E+03 0.84171099565304E+02 0.29642615205260E+03 0.34902582088277E+03
 0.29623416294676E+03 0.29782681496801E+03 0.29615000000030E+03 0.29615000000030E+03 0.29623362980698E+03
 0.29782730845750E+03 0.29615000000030E+03 0.29615000000030E+03 0.29623416294676E+03 0.29782681496801E+03
 0.29615000000030E+03 0.29615000000030E+03 0.29623362980698E+03 0.29782730845750E+03 0.29615000000030E+03
 0.29615000000030E+03 0.29754285620296E+03 0.29615000000032E+03 0.10940594743860E+02 0.10636937511357E+02
 0.57411268065682E+01 0.16608880265852E+03 0.16031897021792E+03 0.95264282965645E+01 0.43671084095921E+01
 0.94689048841189E+01 0.13040523715770E+03 0.94340335408661E+01 0.44095902926883E+01 0.93770690024338E+01
 0.13044403788357E+03 0.95264282965645E+01 0.43671084095921E+01 0.94689048841189E+01 0.13040523715770E+03
 0.94340335408665E+01 0.44095902926882E+01 0.93770690024342E+01 0.13044403788357E+03 0.69266563736195E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33604735398582E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.42915218297020E-01 0.00000000000000E+00 0.00000000000000E+00 0.42915218297020E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.94865499122521E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.94865499122521E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95944012439075E-01 0.10617938577415E+00 0.31810142418739E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    180.45770997
 0.25522431039387E-01 0.30472740833753E+03 0.42965747460430E+03 0.42859463493729E+03 0.42816410779415E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19731864715690E+00 0.00000000000000E+00 -.11420322758811E+02
 0.13971398881688E-01 0.48049828181208E+00 0.57259835380444E+03 0.21472438267666E+03 0.16649383156647E+02
 0.62435186837427E+01 0.31084101961003E+03 0.29615000000034E+03 0.30884027950051E+03 0.32111632730975E+03
 0.29615000000031E+03 0.29615000000031E+03 0.30698401668815E+03 0.32103850719191E+03 0.29615000000031E+03
 0.29615000000031E+03 0.30884027950051E+03 0.32111632730975E+03 0.29615000000031E+03 0.29615000000031E+03
 0.30698401668815E+03 0.32103850719191E+03 0.29615000000031E+03 0.29615000000031E+03 0.35227812550719E+03
 0.29652786724471E+03 0.15371792831377E+04 0.15173344497539E+04 0.68011755117242E+03 0.15177870556108E+04
 0.83426891668249E+03 0.87525877212563E+03 0.93518303781692E+03 0.87525877212563E+03 0.17821888491652E+04
 0.78457299820040E+03 0.92820958337526E+03 0.78457299820040E+03 0.17759771690542E+04 0.87525877212563E+03
 0.93518303781692E+03 0.87525877212563E+03 0.17821888491652E+04 0.78457299820039E+03 0.92820958337526E+03
 0.78457299820039E+03 0.17759771690542E+04 0.17914497767515E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.15169300072831E+06 0.50050000000000E+08 0.47394384987247E+03 0.11709236998126E+01
 0.11709236998126E+01 0.54187718480797E+00 0.48241283871416E+00 0.29617179675459E+03 0.31884304429939E+03
 0.31155177704254E+03 0.31118224427238E+03 0.23000000000000E+00 0.00000000000000E+00 0.22288365514275E+00
 0.00000000000000E+00 -.19729476850389E+01 0.99959461779747E-03 0.55518350815359E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14409649931076E+03 0.54036187241536E+02 0.29652743319007E+03 0.35231849292435E+03
 0.29625584109099E+03 0.29797914363570E+03 0.29615000000031E+03 0.29615000000031E+03 0.29625500346872E+03
 0.29797967293523E+03 0.29615000000031E+03 0.29615000000031E+03 0.29625584109099E+03 0.29797914363570E+03
 0.29615000000031E+03 0.29615000000031E+03 0.29625500346872E+03 0.29797967293523E+03 0.29615000000031E+03
 0.29615000000031E+03 0.29767881544815E+03 0.29615000000034E+03 0.14232923345165E+02 0.13775052126432E+02
 0.94513709589249E+01 0.17565566318641E+03 0.16615703537269E+03 0.11985361190413E+02 0.79797410099182E+01
 0.11911254800534E+02 0.13793645271880E+03 0.11851927532499E+02 0.80224097813591E+01 0.11778754589859E+02
 0.13797516136528E+03 0.11985361190413E+02 0.79797410099182E+01 0.11911254800534E+02 0.13793645271880E+03
 0.11851927532499E+02 0.80224097813592E+01 0.11778754589859E+02 0.13797516136528E+03 0.74683110364782E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33697092104542E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.63224292541862E-01 0.00000000000000E+00 0.00000000000000E+00 0.63224292541862E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.95433243969074E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.95433243969074E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95931722666511E-01 0.10591809535873E+00 0.31884304429939E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    190.39559380
 0.30741912600500E-01 0.30444759860384E+03 0.43138343965727E+03 0.43008268948009E+03 0.42954819806789E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19479395739591E+00 0.00000000000000E+00 -.12360282203562E+02
 0.11599276982118E-01 0.51531385894016E+00 0.68969816069858E+03 0.25863681026197E+03 0.15524519399602E+02
 0.58216947748506E+01 0.31195372743640E+03 0.29615000000035E+03 0.30978642742218E+03 0.32272601786207E+03
 0.29615000000032E+03 0.29615000000032E+03 0.30783651421639E+03 0.32265423989214E+03 0.29615000000032E+03
 0.29615000000032E+03 0.30978642742218E+03 0.32272601786207E+03 0.29615000000032E+03 0.29615000000032E+03
 0.30783651421639E+03 0.32265423989214E+03 0.29615000000032E+03 0.29615000000032E+03 0.35519856896989E+03
 0.29664839930738E+03 0.16679605008288E+04 0.16418755881601E+04 0.69987070366408E+03 0.15199372717567E+04
 0.81656721457426E+03 0.95252589587460E+03 0.96582453851916E+03 0.95252589587460E+03 0.18120940030388E+04
 0.84092709095284E+03 0.96270701917722E+03 0.84092709095284E+03 0.18096775996572E+04 0.95252589587460E+03
 0.96582453851916E+03 0.95252589587460E+03 0.18120940030388E+04 0.84092709095284E+03 0.96270701917722E+03
 0.84092709095284E+03 0.18096775996572E+04 0.18399580797991E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.18398012670614E+06 0.50050000000000E+08 0.47575969421665E+03 0.12327867459040E+01
 0.12327867459040E+01 0.58162872012553E+00 0.45916096526310E+00 0.29618201667648E+03 0.31944183931988E+03
 0.31232183757535E+03 0.31194295597887E+03 0.23000000000000E+00 0.00000000000000E+00 0.22214135138056E+00
 0.00000000000000E+00 -.28477604447574E+01 0.99955149313879E-03 0.75014635337160E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10664585602587E+03 0.39992196009702E+02 0.29664787967998E+03 0.35523939738181E+03
 0.29628039802122E+03 0.29812770603773E+03 0.29615000000032E+03 0.29615000000032E+03 0.29627920588499E+03
 0.29812826504170E+03 0.29615000000032E+03 0.29615000000032E+03 0.29628039802122E+03 0.29812770603773E+03
 0.29615000000032E+03 0.29615000000032E+03 0.29627920588499E+03 0.29812826504170E+03 0.29615000000032E+03
 0.29615000000032E+03 0.29781072202867E+03 0.29615000000035E+03 0.17497577004490E+02 0.16841372490706E+02
 0.13241805937824E+02 0.18396966534298E+03 0.17066165037546E+03 0.14474648180959E+02 0.11661985942966E+02
 0.14383283066071E+02 0.14451491070984E+03 0.14300588449115E+02 0.11704320142645E+02 0.14210622233339E+02
 0.14455305601855E+03 0.14474648180959E+02 0.11661985942966E+02 0.14383283066071E+02 0.14451491070984E+03
 0.14300588449115E+02 0.11704320142644E+02 0.14210622233339E+02 0.14455305601855E+03 0.79595683461937E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33765977795627E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.81168895135057E-01 0.00000000000000E+00 0.00000000000000E+00 0.81168895135057E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96154722345852E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96154722345852E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95917136007057E-01 0.10570256358651E+00 0.31944183931988E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    200.00542077
 0.36398901424014E-01 0.30456235620224E+03 0.43214994006579E+03 0.43060192410313E+03 0.42996453549830E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19244080493416E+00 0.00000000000000E+00 -.12848519628332E+02
 0.97965580628240E-02 0.54890630723749E+00 0.81661333998095E+03 0.30623000249285E+03 0.14574436282691E+02
 0.54654136060091E+01 0.31309874058958E+03 0.29615000000037E+03 0.31077297431031E+03 0.32416303650251E+03
 0.29615000000033E+03 0.29615000000033E+03 0.30869407798515E+03 0.32410222685054E+03 0.29615000000033E+03
 0.29615000000033E+03 0.31077297431031E+03 0.32416303650251E+03 0.29615000000033E+03 0.29615000000033E+03
 0.30869407798515E+03 0.32410222685054E+03 0.29615000000033E+03 0.29615000000033E+03 0.35769762235322E+03
 0.29678646837176E+03 0.17406873597530E+04 0.17097497342542E+04 0.70873445020889E+03 0.15051285001230E+04
 0.79285037766308E+03 0.99602651081550E+03 0.98610662118440E+03 0.99602651081550E+03 0.18248987581446E+04
 0.87435396717928E+03 0.98469595110412E+03 0.87435396717928E+03 0.18240802062071E+04 0.99602651081550E+03
 0.98610662118440E+03 0.99602651081550E+03 0.18248987581446E+04 0.87435396717927E+03 0.98469595110412E+03
 0.87435396717927E+03 0.18240802062071E+04 0.18599276546251E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47640722489018E+03 0.12947991583249E+01
 0.12947991583249E+01 0.62006802801926E+00 0.43742618183181E+00 0.29619684161863E+03 0.31990848264298E+03
 0.31299375424415E+03 0.31261057491273E+03 0.23000000000000E+00 0.00000000000000E+00 0.22138499383229E+00
 0.00000000000000E+00 -.33251327550572E+01 0.99949675388777E-03 0.94196476486573E-01 0.80000000000000E+04
 0.30000000000000E+04 0.84928866751617E+02 0.31848325031856E+02 0.29678588253788E+03 0.35773784421483E+03
 0.29630744345198E+03 0.29827150501573E+03 0.29615000000033E+03 0.29615000000033E+03 0.29630585752814E+03
 0.29827208756986E+03 0.29615000000033E+03 0.29615000000033E+03 0.29630744345198E+03 0.29827150501573E+03
 0.29615000000033E+03 0.29615000000033E+03 0.29630585752814E+03 0.29827208756986E+03 0.29615000000033E+03
 0.29615000000033E+03 0.29793792800657E+03 0.29615000000035E+03 0.20676804760289E+02 0.19779705332435E+02
 0.17043602379629E+02 0.19106258753508E+03 0.17393376714355E+03 0.16961669859935E+02 0.15345379478927E+02
 0.16854664375748E+02 0.15016247641230E+03 0.16748340384494E+02 0.15386742505587E+02 0.16643272649448E+02
 0.15019947250592E+03 0.16961669859935E+02 0.15345379478927E+02 0.16854664375748E+02 0.15016247641230E+03
 0.16748340384494E+02 0.15386742505587E+02 0.16643272649448E+02 0.15019947250592E+03 0.83984175190773E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33814632379466E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.89338777145954E-01 0.00000000000000E+00 0.00000000000000E+00 0.89338777145954E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.96988914335063E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.96988914335063E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95909105429147E-01 0.10553904326927E+00 0.31990848264298E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    210.00784196
 0.40130897178287E-01 0.30496607212111E+03 0.43257246896456E+03 0.43086548256756E+03 0.43016470832263E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19008254871201E+00 0.00000000000000E+00 -.13003220720059E+02
 0.88855215678586E-02 0.58296324958498E+00 0.90034107045986E+03 0.33762790142245E+03 0.13722991982248E+02
 0.51461219933430E+01 0.31412321533134E+03 0.29615000000041E+03 0.31165708494527E+03 0.32555427052712E+03
 0.29615000000040E+03 0.29615000000040E+03 0.30948665816199E+03 0.32549834441797E+03 0.29615000000040E+03
 0.29615000000040E+03 0.31165708494527E+03 0.32555427052712E+03 0.29615000000040E+03 0.29615000000040E+03
 0.30948665816199E+03 0.32549834441797E+03 0.29615000000040E+03 0.29615000000040E+03 0.36000782078556E+03
 0.29695147763289E+03 0.17624846823815E+04 0.17285553365020E+04 0.71272443123652E+03 0.14841259287809E+04
 0.76783787538816E+03 0.10093098710076E+04 0.10027755545995E+04 0.10093098710076E+04 0.18316166485625E+04
 0.88811290119678E+03 0.10014598141777E+04 0.88811290119678E+03 0.18308435840092E+04 0.10093098710076E+04
 0.10027755545995E+04 0.10093098710076E+04 0.18316166485625E+04 0.88811290119678E+03 0.10014598141777E+04
 0.88811290119678E+03 0.18308435840092E+04 0.18628413580160E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47661420973429E+03 0.12948002980595E+01
 0.12948002980595E+01 0.66007771279955E+00 0.41563379302339E+00 0.29621883576656E+03 0.32028341203317E+03
 0.31361537795904E+03 0.31323239425238E+03 0.23000000000000E+00 0.00000000000000E+00 0.22056093902560E+00
 0.00000000000000E+00 -.34808567221017E+01 0.99942100502025E-03 0.11450624781615E+00 0.80000000000000E+04
 0.30000000000000E+04 0.69865183364008E+02 0.26199443761503E+02 0.29695083124446E+03 0.36004684666799E+03
 0.29633839179702E+03 0.29841902418558E+03 0.29615000000040E+03 0.29615000000040E+03 0.29633635812797E+03
 0.29841962483326E+03 0.29615000000040E+03 0.29615000000040E+03 0.29633839179702E+03 0.29841902418558E+03
 0.29615000000040E+03 0.29615000000040E+03 0.29633635812797E+03 0.29841962483326E+03 0.29615000000040E+03
 0.29615000000040E+03 0.29806792016574E+03 0.29615000000037E+03 0.23948657674555E+02 0.22750271248896E+02
 0.21080084589373E+02 0.19750303000420E+03 0.17631754499188E+03 0.19599956220512E+02 0.19244007082392E+02
 0.19480637493933E+02 0.15531854446963E+03 0.19346702353916E+02 0.19283569411130E+02 0.19229944545054E+02
 0.15535360676914E+03 0.19599956220512E+02 0.19244007082392E+02 0.19480637493933E+02 0.15531854446963E+03
 0.19346702353916E+02 0.19283569411130E+02 0.19229944545054E+02 0.15535360676914E+03 0.88159296652017E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33852758575694E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.91723624136077E-01 0.00000000000000E+00 0.00000000000000E+00 0.91723624136077E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97845463685598E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97845463685598E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95906374902668E-01 0.10541233417019E+00 0.32028341203317E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    220.00212454
 0.42268174676200E-01 0.30542263235527E+03 0.43283020629272E+03 0.43103511109597E+03 0.43030114838152E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18779963973653E+00 0.00000000000000E+00 -.13082234553986E+02
 0.84362278010191E-02 0.61608738259113E+00 0.94829113066786E+03 0.35560917400045E+03 0.12985170977457E+02
 0.48694391165465E+01 0.31507511023312E+03 0.29615000000047E+03 0.31248017049342E+03 0.32685238071541E+03
 0.29615000000047E+03 0.29615000000047E+03 0.31023072140928E+03 0.32679977036102E+03 0.29615000000047E+03
 0.29615000000047E+03 0.31248017049342E+03 0.32685238071541E+03 0.29615000000047E+03 0.29615000000047E+03
 0.31023072140928E+03 0.32679977036102E+03 0.29615000000047E+03 0.29615000000047E+03 0.36208938716986E+03
 0.29713844427011E+03 0.17807098998241E+04 0.17443600908584E+04 0.71431954561934E+03 0.14620265097994E+04
 0.74413536645200E+03 0.10204405328611E+04 0.10156204012883E+04 0.10204405328611E+04 0.18340703256989E+04
 0.89971044664852E+03 0.10144011544754E+04 0.89971044664852E+03 0.18333598367495E+04 0.10204405328611E+04
 0.10156204012883E+04 0.10204405328611E+04 0.18340703256989E+04 0.89971044664852E+03 0.10144011544754E+04
 0.89971044664852E+03 0.18333598367495E+04 0.18625204922441E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47675380253092E+03 0.12948008801818E+01
 0.12948008801818E+01 0.70005484311836E+00 0.39471835935383E+00 0.29624886119579E+03 0.32055923158123E+03
 0.31416206523868E+03 0.31378342572069E+03 0.23000000000000E+00 0.00000000000000E+00 0.21970379576709E+00
 0.00000000000000E+00 -.35634897056815E+01 0.99931889637240E-03 0.13512837466717E+00 0.80000000000000E+04
 0.30000000000000E+04 0.59202961773976E+02 0.22201110665241E+02 0.29713775061341E+03 0.36212755787518E+03
 0.29637200295212E+03 0.29856350422030E+03 0.29615000000047E+03 0.29615000000047E+03 0.29636949169316E+03
 0.29856411465273E+03 0.29615000000047E+03 0.29615000000047E+03 0.29637200295212E+03 0.29856350422030E+03
 0.29615000000047E+03 0.29615000000047E+03 0.29636949169316E+03 0.29856411465273E+03 0.29615000000047E+03
 0.29615000000047E+03 0.29819480149936E+03 0.29615000000039E+03 0.27124979773694E+02 0.25572677014900E+02
 0.25145257356650E+02 0.20307362627838E+03 0.17780264263494E+03 0.22269458542361E+02 0.23153301309106E+02
 0.22144268729075E+02 0.15980071276060E+03 0.21978158260660E+02 0.23190032635311E+02 0.21856185373932E+02
 0.15983287673744E+03 0.22269458542361E+02 0.23153301309104E+02 0.22144268729075E+02 0.15980071276060E+03
 0.21978158260660E+02 0.23190032635311E+02 0.21856185373932E+02 0.15983287673744E+03 0.91941968547149E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33881386490241E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.92889772885009E-01 0.00000000000000E+00 0.00000000000000E+00 0.92889772885009E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.98648192298800E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.98648192298800E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904956260931E-01 0.10531999014721E+00 0.32055923158123E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    230.00041290
 0.43453082957192E-01 0.30589361697031E+03 0.43302511787523E+03 0.43118369932347E+03 0.43043399111554E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18557480215355E+00 0.00000000000000E+00 -.13123515192105E+02
 0.82061828219223E-02 0.64841721799444E+00 0.97487469796901E+03 0.36557801173838E+03 0.12337735300651E+02
 0.46266507377442E+01 0.31597709833969E+03 0.29615000000058E+03 0.31326194902125E+03 0.32807442793842E+03
 0.29615000000053E+03 0.29615000000053E+03 0.31094033961695E+03 0.32802442608325E+03 0.29615000000053E+03
 0.29615000000053E+03 0.31326194902125E+03 0.32807442793842E+03 0.29615000000053E+03 0.29615000000053E+03
 0.31094033961695E+03 0.32802442608325E+03 0.29615000000053E+03 0.29615000000053E+03 0.36399353701921E+03
 0.29734757388305E+03 0.17965546845447E+04 0.17580757814105E+04 0.71465983462222E+03 0.14404335316029E+04
 0.72220039780757E+03 0.10301384393317E+04 0.10261202630411E+04 0.10301384393317E+04 0.18344386128301E+04
 0.90987555183789E+03 0.10249933138390E+04 0.90987555183789E+03 0.18337936121199E+04 0.10301384393317E+04
 0.10261202630411E+04 0.10301384393317E+04 0.18344386128301E+04 0.90987555183789E+03 0.10249933138390E+04
 0.90987555183789E+03 0.18337936121199E+04 0.18607449169419E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47688725419443E+03 0.12948011843108E+01
 0.12948011843108E+01 0.74004799653568E+00 0.37465212129829E+00 0.29628879049685E+03 0.32075163116556E+03
 0.31464159439922E+03 0.31427077184140E+03 0.23000000000000E+00 0.00000000000000E+00 0.21881570035739E+00
 0.00000000000000E+00 -.36075684916378E+01 0.99918378852090E-03 0.15606992829175E+00 0.80000000000000E+04
 0.30000000000000E+04 0.51259073977691E+02 0.19222152741634E+02 0.29734683601279E+03 0.36403119186443E+03
 0.29640813598943E+03 0.29870467628207E+03 0.29615000000053E+03 0.29615000000053E+03 0.29640512495688E+03
 0.29870528687836E+03 0.29615000000053E+03 0.29615000000053E+03 0.29640813598943E+03 0.29870467628207E+03
 0.29615000000053E+03 0.29615000000053E+03 0.29640512495688E+03 0.29870528687836E+03 0.29615000000053E+03
 0.29615000000053E+03 0.29831842364530E+03 0.29615000000041E+03 0.30163724001878E+02 0.28205936306142E+02
 0.29196369793925E+02 0.20788125905315E+03 0.17853890741025E+03 0.24953510155030E+02 0.27031131232608E+02
 0.24831842204862E+02 0.16368800561527E+03 0.24626550457418E+02 0.27063982435179E+02 0.24508742545325E+02
 0.16371629687662E+03 0.24953510155030E+02 0.27031131232608E+02 0.24831842204862E+02 0.16368800561527E+03
 0.24626550457418E+02 0.27063982435179E+02 0.24508742545325E+02 0.16371629687662E+03 0.95355640338448E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33902701770090E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.93441883503779E-01 0.00000000000000E+00 0.00000000000000E+00 0.93441883503779E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99380885411403E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99380885411403E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904270061729E-01 0.10525601611055E+00 0.32075163116556E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    240.00096647
 0.44092489963161E-01 0.30636472071613E+03 0.43320959869363E+03 0.43134529652393E+03 0.43058936005673E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18339871668485E+00 0.00000000000000E+00 -.13148130540606E+02
 0.80871806170467E-02 0.68002128826295E+00 0.98921989984211E+03 0.37095746244079E+03 0.11764337584835E+02
 0.44116265943133E+01 0.31683932323941E+03 0.29615000000076E+03 0.31401107281207E+03 0.32923298764497E+03
 0.29615000000059E+03 0.29615000000059E+03 0.31162209284887E+03 0.32918518207247E+03 0.29615000000059E+03
 0.29615000000059E+03 0.31401107281207E+03 0.32923298764497E+03 0.29615000000059E+03 0.29615000000059E+03
 0.31162209284887E+03 0.32918518207247E+03 0.29615000000059E+03 0.29615000000059E+03 0.36575624817961E+03
 0.29757868597876E+03 0.18107404951893E+04 0.17703113167793E+04 0.71437187572637E+03 0.14200651986723E+04
 0.70212146356728E+03 0.10388407247613E+04 0.10351407155989E+04 0.10388407247613E+04 0.18339017632874E+04
 0.91904167548186E+03 0.10341002530106E+04 0.91904167548186E+03 0.18333206491477E+04 0.10388407247613E+04
 0.10351407155989E+04 0.10388407247613E+04 0.18339017632874E+04 0.91904167548186E+03 0.10341002530106E+04
 0.91904167548186E+03 0.18333206491477E+04 0.18584601808664E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47704181321933E+03 0.12948013656608E+01
 0.12948013656608E+01 0.78005021083055E+00 0.35542856721165E+00 0.29634065905820E+03 0.32087514870254E+03
 0.31506164303483E+03 0.31470144761422E+03 0.23000000000000E+00 0.00000000000000E+00 0.21789950347750E+00
 0.00000000000000E+00 -.36326826033526E+01 0.99900865348798E-03 0.17731145887782E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45118347401973E+02 0.16919380275740E+02 0.29757790513339E+03 0.36579352712862E+03
 0.29644659297581E+03 0.29884222716267E+03 0.29615000000059E+03 0.29615000000059E+03 0.29644306760368E+03
 0.29884282741157E+03 0.29615000000059E+03 0.29615000000059E+03 0.29644659297581E+03 0.29884222716267E+03
 0.29615000000059E+03 0.29615000000059E+03 0.29644306760368E+03 0.29884282741157E+03 0.29615000000059E+03
 0.29615000000059E+03 0.29843857715206E+03 0.29615000000044E+03 0.33027904183065E+02 0.30616518447684E+02
 0.33193686741309E+02 0.21202398950232E+03 0.17866433432730E+03 0.27635210383612E+02 0.30839575827447E+02
 0.27529312285324E+02 0.16705378786547E+03 0.27275339633105E+02 0.30867549606960E+02 0.27173859231998E+02
 0.16707728944521E+03 0.27635210383612E+02 0.30839575827447E+02 0.27529312285324E+02 0.16705378786547E+03
 0.27275339633105E+02 0.30867549606960E+02 0.27173859231998E+02 0.16707728944521E+03 0.98423706263747E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33918303277880E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.93705913830706E-01 0.00000000000000E+00 0.00000000000000E+00 0.93705913830706E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10003678664681E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10003678664681E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95903978836209E-01 0.10521515331797E+00 0.32087514870254E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    250.00325206
 0.44432372609237E-01 0.30682868399600E+03 0.43340810585370E+03 0.43153336450816E+03 0.43077603886371E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18126526142110E+00 0.00000000000000E+00 -.13166516874633E+02
 0.80253178431790E-02 0.71095182701717E+00 0.99684525352469E+03 0.37381697007176E+03 0.11252520488715E+02
 0.42196951832681E+01 0.31766833123499E+03 0.29615000000106E+03 0.31473308103477E+03 0.33033820924716E+03
 0.29615000000064E+03 0.29615000000065E+03 0.31228041905622E+03 0.33029232611184E+03 0.29615000000064E+03
 0.29615000000065E+03 0.31473308103477E+03 0.33033820924716E+03 0.29615000000064E+03 0.29615000000065E+03
 0.31228041905622E+03 0.33029232611184E+03 0.29615000000064E+03 0.29615000000065E+03 0.36740437334664E+03
 0.29783155859205E+03 0.18237018820420E+04 0.17814367134577E+04 0.71377936680558E+03 0.14011422332405E+04
 0.68379396960091E+03 0.10468119977030E+04 0.10431815523535E+04 0.10468119977030E+04 0.18330596100967E+04
 0.92747015527513E+03 0.10422213887872E+04 0.92747015527513E+03 0.18325390306078E+04 0.10468119977030E+04
 0.10431815523535E+04 0.10468119977030E+04 0.18330596100967E+04 0.92747015527513E+03 0.10422213887871E+04
 0.92747015527513E+03 0.18325390306078E+04 0.18561204439625E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47722702452221E+03 0.12948015011195E+01
 0.12948015011195E+01 0.82005935318765E+00 0.33703350201469E+00 0.29640667045030E+03 0.32094295457391E+03
 0.31542992139751E+03 0.31508255813391E+03 0.23000000000000E+00 0.00000000000000E+00 0.21695756789947E+00
 0.00000000000000E+00 -.36488047133804E+01 0.99878600979793E-03 0.19883772883082E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40233813004406E+02 0.15087679876652E+02 0.29783073592480E+03 0.36744124453958E+03
 0.29648718977495E+03 0.29897601494407E+03 0.29615000000063E+03 0.29615000000063E+03 0.29648314186205E+03
 0.29897659382914E+03 0.29615000000063E+03 0.29615000000063E+03 0.29648718977495E+03 0.29897601494407E+03
 0.29615000000063E+03 0.29615000000063E+03 0.29648314186205E+03 0.29897659382914E+03 0.29615000000063E+03
 0.29615000000063E+03 0.29855518959400E+03 0.29615000000048E+03 0.35689668426919E+02 0.32781759439376E+02
 0.37106642768656E+02 0.21559446451569E+03 0.17830228853319E+03 0.30301908388819E+02 0.34550292091998E+02
 0.30225985316515E+02 0.16996821776200E+03 0.29912075434078E+02 0.34572479785404E+02 0.29840910169360E+02
 0.16998610394171E+03 0.30301908388819E+02 0.34550292091998E+02 0.30225985316515E+02 0.16996821776200E+03
 0.29912075434078E+02 0.34572479785403E+02 0.29840910169360E+02 0.16998610394171E+03 0.10117247770592E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33929426902976E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.93843647851498E-01 0.00000000000000E+00 0.00000000000000E+00 0.93843647851498E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10061618447034E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10061618447034E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95903902181809E-01 0.10519282360662E+00 0.32094295457391E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    260.03166482
 0.44614155882363E-01 0.30728342685464E+03 0.43363079113971E+03 0.43175183080453E+03 0.43099536364133E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17916545076676E+00 0.00000000000000E+00 -.13183453797017E+02
 0.79926176830191E-02 0.74132064808614E+00 0.10009236419498E+04 0.37534636573118E+03 0.10791551564972E+02
 0.40468318368645E+01 0.31847072397390E+03 0.29615000000156E+03 0.31543359493507E+03 0.33140065159422E+03
 0.29615000000068E+03 0.29615000000070E+03 0.31292011833347E+03 0.33135649476854E+03 0.29615000000068E+03
 0.29615000000070E+03 0.31543359493507E+03 0.33140065159422E+03 0.29615000000068E+03 0.29615000000070E+03
 0.31292011833347E+03 0.33135649476854E+03 0.29615000000068E+03 0.29615000000070E+03 0.36896143530023E+03
 0.29810662050479E+03 0.18357329631409E+04 0.17917050594985E+04 0.71303843853145E+03 0.13836075921845E+04
 0.66700396146039E+03 0.10542316826641E+04 0.10505455019937E+04 0.10542316826641E+04 0.18321913100194E+04
 0.93533975565017E+03 0.10496597419834E+04 0.93533975565017E+03 0.18317274217987E+04 0.10542316826641E+04
 0.10505455019937E+04 0.10542316826641E+04 0.18321913100194E+04 0.93533975565017E+03 0.10496597419834E+04
 0.93533975565017E+03 0.18317274217987E+04 0.18539142014669E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47744465264696E+03 0.12948016258999E+01
 0.12948016258999E+01 0.86017300423227E+00 0.31940629630044E+00 0.29648939378967E+03 0.32096659959215E+03
 0.31575448382605E+03 0.31542166520787E+03 0.23000000000000E+00 0.00000000000000E+00 0.21598954071342E+00
 0.00000000000000E+00 -.36607076434167E+01 0.99850722173139E-03 0.22068876279801E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36250146580060E+02 0.13593804967522E+02 0.29810575753073E+03 0.36899776137501E+03
 0.29652986021529E+03 0.29910633135095E+03 0.29615000000067E+03 0.29615000000067E+03 0.29652528482560E+03
 0.29910687757657E+03 0.29615000000067E+03 0.29615000000067E+03 0.29652986021529E+03 0.29910633135095E+03
 0.29615000000067E+03 0.29615000000067E+03 0.29652528482560E+03 0.29910687757657E+03 0.29615000000067E+03
 0.29615000000067E+03 0.29866856133457E+03 0.29615000000054E+03 0.38134578205107E+02 0.34692742792584E+02
 0.40921595438673E+02 0.21868380928800E+03 0.17755760587214E+03 0.32950971603879E+02 0.38151547239106E+02
 0.32918429259339E+02 0.17250134264972E+03 0.32534145991794E+02 0.38167134010475E+02 0.32506120785643E+02
 0.17251288244396E+03 0.32950971603879E+02 0.38151547239106E+02 0.32918429259340E+02 0.17250134264972E+03
 0.32534145991794E+02 0.38167134010475E+02 0.32506120785643E+02 0.17251288244396E+03 0.10363459740656E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33937068930598E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.93930884433169E-01 0.00000000000000E+00 0.00000000000000E+00 0.93930884433169E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10112415410188E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10112415410188E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95903946731965E-01 0.10518511074545E+00 0.32096659959215E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    270.05494678
 0.44714988026203E-01 0.30772620874154E+03 0.43387905187089E+03 0.43199874424756E+03 0.43124401014626E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17710346637439E+00 0.00000000000000E+00 -.13200481918248E+02
 0.79745938044941E-02 0.77105974512126E+00 0.10031858921130E+04 0.37619470954237E+03 0.10375330900904E+02
 0.38907490878391E+01 0.31924731487974E+03 0.29615000000240E+03 0.31611318704464E+03 0.33242315207644E+03
 0.29615000000068E+03 0.29615000000072E+03 0.31354150441259E+03 0.33238056205574E+03 0.29615000000067E+03
 0.29615000000071E+03 0.31611318704464E+03 0.33242315207644E+03 0.29615000000068E+03 0.29615000000072E+03
 0.31354150441259E+03 0.33238056205574E+03 0.29615000000067E+03 0.29615000000071E+03 0.37043746188692E+03
 0.29840278984397E+03 0.18469634012484E+04 0.18012298532580E+04 0.71221846503043E+03 0.13673945678970E+04
 0.65161501054144E+03 0.10611780933083E+04 0.10573721001908E+04 0.10611780933083E+04 0.18314140531545E+04
 0.94272674501078E+03 0.10565549267113E+04 0.94272674501078E+03 0.18310027154318E+04 0.10611780933083E+04
 0.10573721001907E+04 0.10611780933083E+04 0.18314140531545E+04 0.94272674501078E+03 0.10565549267113E+04
 0.94272674501078E+03 0.18310027154318E+04 0.18519091009593E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47769160070892E+03 0.12948017513522E+01
 0.12948017513522E+01 0.90026613207746E+00 0.30258309631535E+00 0.29659106932218E+03 0.32095580161213E+03
 0.31604089752067E+03 0.31572384230910E+03 0.23000000000000E+00 0.00000000000000E+00 0.21500016373123E+00
 0.00000000000000E+00 -.36701498888777E+01 0.99816482655331E-03 0.24278440085314E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32951046162308E+02 0.12356642310866E+02 0.29840188917179E+03 0.37047306669941E+03
 0.29657425103674E+03 0.29923279462479E+03 0.29615000000065E+03 0.29615000000065E+03 0.29656913671983E+03
 0.29923329712371E+03 0.29615000000065E+03 0.29615000000065E+03 0.29657425103674E+03 0.29923279462479E+03
 0.29615000000065E+03 0.29615000000065E+03 0.29656913671983E+03 0.29923329712371E+03 0.29615000000065E+03
 0.29615000000065E+03 0.29877839188587E+03 0.29615000000060E+03 0.40340392258391E+02 0.36337116659829E+02
 0.44610362873236E+02 0.22135651224008E+03 0.17652309755248E+03 0.35568531116851E+02 0.41618136418835E+02
 0.35568531116851E+02 0.17470215586678E+03 0.35127831470076E+02 0.41626442434204E+02 0.35127831470076E+02
 0.17470675037630E+03 0.35568531116851E+02 0.41618136418835E+02 0.35568531116851E+02 0.17470215586678E+03
 0.35127831470076E+02 0.41626442434204E+02 0.35127831470076E+02 0.17470675037630E+03 0.10582699336271E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33941988846240E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.93995262055112E-01 0.00000000000000E+00 0.00000000000000E+00 0.93995262055112E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10156413770056E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10156413770056E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904069061059E-01 0.10518877387512E+00 0.32095580161213E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    280.00870312
 0.44773557000335E-01 0.30815538491981E+03 0.43415016357387E+03 0.43226975210593E+03 0.43151698070950E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17509053977378E+00 0.00000000000000E+00 -.13218024140160E+02
 0.79641615077263E-02 0.80000569196884E+00 0.10044999705542E+04 0.37668748895782E+03 0.99999288508957E+01
 0.37499733190859E+01 0.31999653338849E+03 0.29615000000376E+03 0.31677037605702E+03 0.33340472496726E+03
 0.29615000000071E+03 0.29615000000078E+03 0.31414311097082E+03 0.33336356400110E+03 0.29615000000069E+03
 0.29615000000077E+03 0.31677037605702E+03 0.33340472496726E+03 0.29615000000071E+03 0.29615000000078E+03
 0.31414311097082E+03 0.33336356400110E+03 0.29615000000069E+03 0.29615000000077E+03 0.37183416771999E+03
 0.29871754620119E+03 0.18574490114248E+04 0.18100684368780E+04 0.71137019362341E+03 0.13524795579709E+04
 0.63755251337941E+03 0.10676832414155E+04 0.10637260541686E+04 0.10676832414155E+04 0.18307729351221E+04
 0.94966068029880E+03 0.10629716598542E+04 0.94966068029880E+03 0.18304097902935E+04 0.10676832414155E+04
 0.10637260541686E+04 0.10676832414155E+04 0.18307729351221E+04 0.94966068029880E+03 0.10629716598542E+04
 0.94966068029880E+03 0.18304097902935E+04 0.18501334346521E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47796296952479E+03 0.12948018805921E+01
 0.12948018805921E+01 0.94008115740449E+00 0.28664123124427E+00 0.29671342155735E+03 0.32091934586700E+03
 0.31629373523531E+03 0.31599317374993E+03 0.23000000000000E+00 0.00000000000000E+00 0.21399759129291E+00
 0.00000000000000E+00 -.36778832614335E+01 0.99775314884541E-03 0.26496781738781E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30192345919094E+02 0.11322129719660E+02 0.29871661042268E+03 0.37186886920747E+03
 0.29662005273715E+03 0.29935482538569E+03 0.29615000000064E+03 0.29615000000064E+03 0.29661444579721E+03
 0.29935527362197E+03 0.29615000000064E+03 0.29615000000064E+03 0.29662005273715E+03 0.29935482538569E+03
 0.29615000000064E+03 0.29615000000064E+03 0.29661444579721E+03 0.29935527362197E+03 0.29615000000064E+03
 0.29615000000064E+03 0.29888422660902E+03 0.29615000000071E+03 0.42288085995673E+02 0.37709819693802E+02
 0.48140290428557E+02 0.22366528675241E+03 0.17528429487171E+03 0.38134723119345E+02 0.44921171291737E+02
 0.38229722829821E+02 0.17661072712460E+03 0.37673047722324E+02 0.44921689187868E+02 0.37775241894504E+02
 0.17660794481511E+03 0.38134723119345E+02 0.44921171291737E+02 0.38229722829821E+02 0.17661072712460E+03
 0.37673047722324E+02 0.44921689187868E+02 0.37775241894504E+02 0.17660794481511E+03 0.10776941148094E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33944853223585E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94047851648034E-01 0.00000000000000E+00 0.00000000000000E+00 0.94047851648034E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10194115747022E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10194115747022E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904242928009E-01 0.10520090578165E+00 0.32091934586700E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    290.00000298
 0.44812303904750E-01 0.30857557770370E+03 0.43444433289152E+03 0.43256417658833E+03 0.43181328710496E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17310373071526E+00 0.00000000000000E+00 -.13236514629996E+02
 0.79572746455635E-02 0.82849011308062E+00 0.10053693452017E+04 0.37701350445063E+03 0.96561200594817E+01
 0.36210450223056E+01 0.32072861141057E+03 0.29615000000590E+03 0.31741398255037E+03 0.33436011606701E+03
 0.29615000000079E+03 0.29615000000091E+03 0.31473289288622E+03 0.33432028041127E+03 0.29615000000075E+03
 0.29615000000090E+03 0.31741398255037E+03 0.33436011606701E+03 0.29615000000079E+03 0.29615000000091E+03
 0.31473289288622E+03 0.33432028041127E+03 0.29615000000075E+03 0.29615000000090E+03 0.37317641381805E+03
 0.29905373832771E+03 0.18673909698221E+04 0.18183943952243E+04 0.71048686447648E+03 0.13385353506424E+04
 0.62449605184350E+03 0.10738697800297E+04 0.10697407191338E+04 0.10738697800297E+04 0.18302587819031E+04
 0.95626931707267E+03 0.10690444105903E+04 0.95626931707267E+03 0.18299402312973E+04 0.10738697800297E+04
 0.10697407191338E+04 0.10738697800297E+04 0.18302587819030E+04 0.95626931707267E+03 0.10690444105903E+04
 0.95626931707267E+03 0.18299402312973E+04 0.18485423954241E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47825779538953E+03 0.12948020168183E+01
 0.12948020168183E+01 0.98004635686865E+00 0.27138179062091E+00 0.29685972288205E+03 0.32086393785194E+03
 0.31652106662462E+03 0.31623758073938E+03 0.23000000000000E+00 0.00000000000000E+00 0.21297251297263E+00
 0.00000000000000E+00 -.36845272073780E+01 0.99726136092143E-03 0.28746503890402E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27829471126299E+02 0.10436051672362E+02 0.29905277121108E+03 0.37321005443749E+03
 0.29666775415672E+03 0.29947387387794E+03 0.29615000000064E+03 0.29615000000065E+03 0.29666165566184E+03
 0.29947425742817E+03 0.29615000000064E+03 0.29615000000065E+03 0.29666775415672E+03 0.29947387387794E+03
 0.29615000000064E+03 0.29615000000065E+03 0.29666165566184E+03 0.29947425742817E+03 0.29615000000064E+03
 0.29615000000065E+03 0.29898734014054E+03 0.29615000000087E+03 0.43996069438384E+02 0.38832669263590E+02
 0.51543734625616E+02 0.22569411753169E+03 0.17389266423295E+03 0.40674699901008E+02 0.48092847895559E+02
 0.40912337482337E+02 0.17829363683196E+03 0.40194831673945E+02 0.48085115771021E+02 0.40441998976746E+02
 0.17828308992189E+03 0.40674699901008E+02 0.48092847895559E+02 0.40912337482337E+02 0.17829363683196E+03
 0.40194831673945E+02 0.48085115771020E+02 0.40441998976746E+02 0.17828308992189E+03 0.10950671532286E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33946242396858E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94096497661887E-01 0.00000000000000E+00 0.00000000000000E+00 0.94096497661887E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10226571149957E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10226571149957E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904451579214E-01 0.10521929429221E+00 0.32086393785194E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    300.00885872
 0.44840448504753E-01 0.30898691922862E+03 0.43475838280225E+03 0.43287849985700E+03 0.43212929901647E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17114643748875E+00 0.00000000000000E+00 -.13255623143257E+02
 0.79522794734804E-02 0.85646604310728E+00 0.10060008613478E+04 0.37725032300544E+03 0.93407089100413E+01
 0.35027658412655E+01 0.32144385268313E+03 0.29615000000922E+03 0.31804418076052E+03 0.33529046778106E+03
 0.29615000000092E+03 0.29615000000111E+03 0.31531094694230E+03 0.33525186502826E+03 0.29615000000085E+03
 0.29615000000110E+03 0.31804418076052E+03 0.33529046778106E+03 0.29615000000092E+03 0.29615000000111E+03
 0.31531094694230E+03 0.33525186502826E+03 0.29615000000085E+03 0.29615000000110E+03 0.37446769652378E+03
 0.29941041604594E+03 0.18768325779464E+04 0.18262512794094E+04 0.70957799453459E+03 0.13254799131739E+04
 0.61235402866661E+03 0.10797625200205E+04 0.10754469180249E+04 0.10797625200205E+04 0.18298613467645E+04
 0.96257720781922E+03 0.10748042450300E+04 0.96257720781922E+03 0.18295839133121E+04 0.10797625200205E+04
 0.10754469180249E+04 0.10797625200205E+04 0.18298613467645E+04 0.96257720781922E+03 0.10748042450300E+04
 0.96257720781922E+03 0.18295839133121E+04 0.18471193805500E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47857245444240E+03 0.12948021575977E+01
 0.12948021575977E+01 0.10200817798119E+01 0.25681701603602E+00 0.29703199120813E+03 0.32079536455132E+03
 0.31672680546269E+03 0.31646068334682E+03 0.23000000000000E+00 0.00000000000000E+00 0.21192806968941E+00
 0.00000000000000E+00 -.36900055767736E+01 0.99668292976590E-03 0.31022166638146E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25788011821724E+02 0.96705044331466E+01 0.29940942198619E+03 0.37450014890049E+03
 0.29671728297470E+03 0.29958986988101E+03 0.29615000000064E+03 0.29615000000067E+03 0.29671070922588E+03
 0.29959017895510E+03 0.29615000000064E+03 0.29615000000067E+03 0.29671728297470E+03 0.29958986988101E+03
 0.29615000000064E+03 0.29615000000067E+03 0.29671070922588E+03 0.29959017895510E+03 0.29615000000064E+03
 0.29615000000067E+03 0.29908769585102E+03 0.29615000000111E+03 0.45458863114951E+02 0.39711657119576E+02
 0.54810381434238E+02 0.22748669021962E+03 0.17240225687821E+03 0.43182953927239E+02 0.51125211975994E+02
 0.43631081927202E+02 0.17978479517839E+03 0.42687531860256E+02 0.51108893681082E+02 0.43147489620863E+02
 0.17976621606522E+03 0.43182953927242E+02 0.51125211975994E+02 0.43631081927206E+02 0.17978479517839E+03
 0.42687531860255E+02 0.51108893681082E+02 0.43147489620863E+02 0.17976621606522E+03 0.11105794048541E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33946598536393E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94139067477437E-01 0.00000000000000E+00 0.00000000000000E+00 0.94139067477437E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10254272305583E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10254272305583E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904687647119E-01 0.10524203935527E+00 0.32079536455132E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    310.00612385
 0.44862216433811E-01 0.30938913262762E+03 0.43508878165915E+03 0.43320906003898E+03 0.43246132382513E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16922367049399E+00 0.00000000000000E+00 -.13275145076806E+02
 0.79484201545478E-02 0.88386481955921E+00 0.10064893204498E+04 0.37743349516866E+03 0.90511578501220E+01
 0.33941841937957E+01 0.32214180737790E+03 0.29615000001424E+03 0.31866047055697E+03 0.33619581631584E+03
 0.29615000000111E+03 0.29615000000144E+03 0.31587675875505E+03 0.33615836211563E+03 0.29615000000100E+03
 0.29615000000143E+03 0.31866047055697E+03 0.33619581631585E+03 0.29615000000111E+03 0.29615000000144E+03
 0.31587675875505E+03 0.33615836211563E+03 0.29615000000100E+03 0.29615000000143E+03 0.37570979605057E+03
 0.29978604010381E+03 0.18858024678492E+04 0.18336696913128E+04 0.70865005190045E+03 0.13132470846775E+04
 0.60105378251758E+03 0.10853771424978E+04 0.10808631939693E+04 0.10853771424978E+04 0.18295660353603E+04
 0.96859949748342E+03 0.10802699220693E+04 0.96859949748342E+03 0.18293263700562E+04 0.10853771424978E+04
 0.10808631939693E+04 0.10853771424978E+04 0.18295660353602E+04 0.96859949748342E+03 0.10802699220693E+04
 0.96859949748342E+03 0.18293263700562E+04 0.18458466829795E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47890324927877E+03 0.12948023014230E+01
 0.12948023014230E+01 0.10600708403572E+01 0.24296496987814E+00 0.29723164818757E+03 0.32071865099676E+03
 0.31691430504338E+03 0.31666556523346E+03 0.23000000000000E+00 0.00000000000000E+00 0.21086840567188E+00
 0.00000000000000E+00 -.36943745001270E+01 0.99601339314606E-03 0.33316069670744E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24012436277936E+02 0.90046636042259E+01 0.29978502399869E+03 0.37574096791603E+03
 0.29676854332290E+03 0.29970268310061E+03 0.29615000000065E+03 0.29615000000070E+03 0.29676151370476E+03
 0.29970290883755E+03 0.29615000000065E+03 0.29615000000070E+03 0.29676854332290E+03 0.29970268310061E+03
 0.29615000000065E+03 0.29615000000070E+03 0.29676151370476E+03 0.29970290883755E+03 0.29615000000065E+03
 0.29615000000070E+03 0.29918520130811E+03 0.29615000000149E+03 0.46675007308923E+02 0.40357460672912E+02
 0.57931038297898E+02 0.22908121100669E+03 0.17086051751731E+03 0.45653076515998E+02 0.54011492244014E+02
 0.46391171286454E+02 0.18111400842246E+03 0.45144635519780E+02 0.53986382696517E+02 0.45896902219926E+02
 0.18108725300356E+03 0.45653076515998E+02 0.54011492244014E+02 0.46391171286454E+02 0.18111400842246E+03
 0.45144635519780E+02 0.53986382696517E+02 0.45896902219926E+02 0.18108725300356E+03 0.11244059991110E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33946304524777E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94175438566061E-01 0.00000000000000E+00 0.00000000000000E+00 0.94175438566061E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10277717139926E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10277717139926E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95904942709099E-01 0.10526748790231E+00 0.32071865099676E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    320.00924120
 0.44879624388787E-01 0.30978373124914E+03 0.43543371469772E+03 0.43355400667718E+03 0.43280752200778E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16733140142187E+00 0.00000000000000E+00 -.13294944940147E+02
 0.79453363498447E-02 0.91074717468163E+00 0.10068799667816E+04 0.37757998754309E+03 0.87839965057224E+01
 0.32939986896459E+01 0.32282513153291E+03 0.29615000002170E+03 0.31926509793817E+03 0.33708009145856E+03
 0.29615000000142E+03 0.29615000000196E+03 0.31643234402545E+03 0.33704371313381E+03 0.29615000000124E+03
 0.29615000000195E+03 0.31926509793817E+03 0.33708009145856E+03 0.29615000000142E+03 0.29615000000196E+03
 0.31643234402545E+03 0.33704371313381E+03 0.29615000000124E+03 0.29615000000195E+03 0.37690957797661E+03
 0.30018067776642E+03 0.18943646521813E+04 0.18407086028206E+04 0.70770429749022E+03 0.13017252067920E+04
 0.59048238781431E+03 0.10907516567216E+04 0.10860281087232E+04 0.10907516567216E+04 0.18293566848203E+04
 0.97437554532640E+03 0.10854804417790E+04 0.97437554532640E+03 0.18291517535471E+04 0.10907516567216E+04
 0.10860281087231E+04 0.10907516567216E+04 0.18293566848203E+04 0.97437554532640E+03 0.10854804417790E+04
 0.97437554532640E+03 0.18291517535471E+04 0.18447021809328E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47924832575574E+03 0.12948024472959E+01
 0.12948024472959E+01 0.11000833097328E+01 0.22975861503568E+00 0.29746066986247E+03 0.32063733108388E+03
 0.31708730602829E+03 0.31685583568082E+03 0.23000000000000E+00 0.00000000000000E+00 0.20979298683989E+00
 0.00000000000000E+00 -.36976600024281E+01 0.99524650769951E-03 0.35630663327786E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22452571052084E+02 0.84197141445314E+01 0.30017964491218E+03 0.37693940617203E+03
 0.29682169772808E+03 0.29981273852310E+03 0.29615000000066E+03 0.29615000000075E+03 0.29681423240845E+03
 0.29981287272262E+03 0.29615000000066E+03 0.29615000000075E+03 0.29682169772808E+03 0.29981273852310E+03
 0.29615000000066E+03 0.29615000000075E+03 0.29681423240845E+03 0.29981287272262E+03 0.29615000000066E+03
 0.29615000000075E+03 0.29928021885661E+03 0.29615000000204E+03 0.47651439927912E+02 0.40786163797757E+02
 0.60912746648595E+02 0.23050202294265E+03 0.16928471256081E+03 0.48090538037122E+02 0.56759752705647E+02
 0.49211752460962E+02 0.18231637666119E+03 0.47571456288322E+02 0.56725732565164E+02 0.48709251939298E+02
 0.18228138114578E+03 0.48090538037122E+02 0.56759752705647E+02 0.49211752460962E+02 0.18231637666119E+03
 0.47571456288322E+02 0.56725732565164E+02 0.48709251939298E+02 0.18228138114578E+03 0.11367489846777E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33942529034233E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94205041557195E-01 0.00000000000000E+00 0.00000000000000E+00 0.94205041557195E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10297412595229E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10297412595229E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95905212247801E-01 0.10529447831232E+00 0.32063733108388E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    330.01010599
 0.44892604784206E-01 0.31017131864217E+03 0.43579083644368E+03 0.43391104065507E+03 0.43316563261912E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16547056780535E+00 0.00000000000000E+00 -.13314594370073E+02
 0.79430382218001E-02 0.93710302911973E+00 0.10071712834068E+04 0.37768923127756E+03 0.85369481811565E+01
 0.32013555679337E+01 0.32349450681288E+03 0.29615000003258E+03 0.31985857361235E+03 0.33794454013793E+03
 0.29615000000188E+03 0.29615000000277E+03 0.31697813440142E+03 0.33790917212857E+03 0.29615000000160E+03
 0.29615000000276E+03 0.31985857361235E+03 0.33794454013793E+03 0.29615000000188E+03 0.29615000000277E+03
 0.31697813440142E+03 0.33790917212857E+03 0.29615000000160E+03 0.29615000000276E+03 0.37806998394530E+03
 0.30059334317947E+03 0.19025540248923E+04 0.18474034013702E+04 0.70674431859109E+03 0.12908482516807E+04
 0.58057021149663E+03 0.10959059871835E+04 0.10909619657569E+04 0.10959059871835E+04 0.18292200757023E+04
 0.97992555063401E+03 0.10904563894118E+04 0.97992555063401E+03 0.18290470498140E+04 0.10959059871835E+04
 0.10909619657569E+04 0.10959059871835E+04 0.18292200757023E+04 0.97992555063401E+03 0.10904563894118E+04
 0.97992555063401E+03 0.18290470498140E+04 0.18436709283233E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47960540749201E+03 0.12948025920606E+01
 0.12948025920606E+01 0.11400867689175E+01 0.21697594514648E+00 0.29772036161018E+03 0.32055108671396E+03
 0.31724860794211E+03 0.31703436632882E+03 0.23000000000000E+00 0.00000000000000E+00 0.20870731265020E+00
 0.00000000000000E+00 -.36995587578462E+01 0.99437836803509E-03 0.37956852785814E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21076563025768E+02 0.79037111346630E+01 0.30059229940240E+03 0.37809843343665E+03
 0.29687679351433E+03 0.29992006566600E+03 0.29615000000067E+03 0.29615000000082E+03 0.29686891561314E+03
 0.29992010098030E+03 0.29615000000067E+03 0.29615000000082E+03 0.29687679351433E+03 0.29992006566600E+03
 0.29615000000067E+03 0.29615000000082E+03 0.29686891561314E+03 0.29992010098030E+03 0.29615000000067E+03
 0.29615000000082E+03 0.29937272073310E+03 0.29615000000285E+03 0.48386497976761E+02 0.41006678977263E+02
 0.63745037744251E+02 0.23173728089849E+03 0.16767351796551E+03 0.50491073516989E+02 0.59360369862077E+02
 0.52102549144236E+02 0.18337542253650E+03 0.49964001628678E+02 0.59317396100054E+02 0.51594535410353E+02
 0.18333219290283E+03 0.50491073516989E+02 0.59360369862077E+02 0.52102549144236E+02 0.18337542253650E+03
 0.49964001628678E+02 0.59317396100054E+02 0.51594535410353E+02 0.18333219290283E+03 0.11476131066226E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33935244649435E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94223010127283E-01 0.00000000000000E+00 0.00000000000000E+00 0.94223010127283E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10313430559658E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10313430559658E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95905504196086E-01 0.10532312646684E+00 0.32055108671396E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    340.01016424
 0.44901112424074E-01 0.31055258767141E+03 0.43615841705240E+03 0.43427846989702E+03 0.43353399518837E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16364027647908E+00 0.00000000000000E+00 -.13334221995718E+02
 0.79415324064804E-02 0.96294827351154E+00 0.10073622558629E+04 0.37776084594857E+03 0.83078190387390E+01
 0.31154321395271E+01 0.32415112864379E+03 0.29615000004817E+03 0.32044187005306E+03 0.33879098582594E+03
 0.29615000000259E+03 0.29615000000399E+03 0.31751499234421E+03 0.33875656956372E+03 0.29615000000214E+03
 0.29615000000397E+03 0.32044187005306E+03 0.33879098582594E+03 0.29615000000259E+03 0.29615000000399E+03
 0.31751499234421E+03 0.33875656956372E+03 0.29615000000214E+03 0.29615000000397E+03 0.37919458034952E+03
 0.30102340187193E+03 0.19104087226832E+04 0.18537903328366E+04 0.70577301575972E+03 0.12805501599151E+04
 0.57124827907661E+03 0.11008623566997E+04 0.10956872802113E+04 0.11008623566997E+04 0.18291453107210E+04
 0.98527231072931E+03 0.10952205814116E+04 0.98527231072931E+03 0.18290015886096E+04 0.11008623566997E+04
 0.10956872802113E+04 0.11008623566997E+04 0.18291453107210E+04 0.98527231072931E+03 0.10952205814116E+04
 0.98527231072931E+03 0.18290015886096E+04 0.18427395749580E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47997282100509E+03 0.12948027366647E+01
 0.12948027366647E+01 0.11800870019151E+01 0.20459348704135E+00 0.29801207273659E+03 0.32046341004626E+03
 0.31740114512031E+03 0.31720393642837E+03 0.23000000000000E+00 0.00000000000000E+00 0.20761239597461E+00
 0.00000000000000E+00 -.37003655078160E+01 0.99340500615352E-03 0.40293143536831E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19854494580914E+02 0.74454354678426E+01 0.30102235348023E+03 0.37922163817692E+03
 0.29693395944270E+03 0.30002477715392E+03 0.29615000000068E+03 0.29615000000092E+03 0.29692569428742E+03
 0.30002470701035E+03 0.29615000000068E+03 0.29615000000092E+03 0.29693395944270E+03 0.30002477715392E+03
 0.29615000000068E+03 0.29615000000092E+03 0.29692569428742E+03 0.30002470701035E+03 0.29615000000068E+03
 0.29615000000092E+03 0.29946289672350E+03 0.29615000000404E+03 0.48890159973642E+02 0.41038624744258E+02
 0.66434685833113E+02 0.23283878754504E+03 0.16607192828276E+03 0.52858162805777E+02 0.61821925797935E+02
 0.55081912223942E+02 0.18431558695593E+03 0.52325650034182E+02 0.61770030151908E+02 0.54570995071103E+02
 0.18426419798043E+03 0.52858162805777E+02 0.61821925797935E+02 0.55081912223942E+02 0.18431558695593E+03
 0.52325650034182E+02 0.61770030151908E+02 0.54570995071103E+02 0.18426419798043E+03 0.11572349341006E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33928268622248E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94232777794845E-01 0.00000000000000E+00 0.00000000000000E+00 0.94232777794845E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10326355551122E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10326355551122E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95905809084379E-01 0.10535227625690E+00 0.32046341004626E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    350.00197706
 0.44905984548681E-01 0.31092764347953E+03 0.43653461209740E+03 0.43465444390008E+03 0.43391076939253E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16184125878934E+00 0.00000000000000E+00 -.13353921438158E+02
 0.79406699567835E-02 0.98827587307393E+00 0.10074716671943E+04 0.37780187519784E+03 0.80949057019037E+01
 0.30355896382139E+01 0.32479548154333E+03 0.29615000007018E+03 0.32101533951019E+03 0.33962027273270E+03
 0.29615000000363E+03 0.29615000000580E+03 0.31804321436536E+03 0.33958675485168E+03 0.29615000000295E+03
 0.29615000000577E+03 0.32101533951019E+03 0.33962027273270E+03 0.29615000000363E+03 0.29615000000580E+03
 0.31804321436536E+03 0.33958675485168E+03 0.29615000000295E+03 0.29615000000577E+03 0.38028550345448E+03
 0.30146984043265E+03 0.19179545657720E+04 0.18598938353360E+04 0.70479359510846E+03 0.12707819910247E+04
 0.56246442794072E+03 0.11056355817894E+04 0.11002194337652E+04 0.11056355817894E+04 0.18291227696429E+04
 0.99043076593803E+03 0.10997886419534E+04 0.99043076593803E+03 0.18290059365841E+04 0.11056355817894E+04
 0.11002194337652E+04 0.11056355817894E+04 0.18291227696429E+04 0.99043076593803E+03 0.10997886419534E+04
 0.99043076593803E+03 0.18290059365841E+04 0.18418965881482E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48034871865194E+03 0.12948028817979E+01
 0.12948028817979E+01 0.12200542531934E+01 0.19262029906491E+00 0.29833656147047E+03 0.32037754103007E+03
 0.31754718098044E+03 0.31736664000431E+03 0.23000000000000E+00 0.00000000000000E+00 0.20650971391420E+00
 0.00000000000000E+00 -.37003426219531E+01 0.99232451952395E-03 0.42636778076686E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18763143841712E+02 0.70361789406420E+01 0.30146879352650E+03 0.38031117532343E+03
 0.29699330524939E+03 0.30012699542661E+03 0.29615000000070E+03 0.29615000000106E+03 0.29698467988346E+03
 0.30012681417639E+03 0.29615000000070E+03 0.29615000000106E+03 0.29699330524939E+03 0.30012699542661E+03
 0.29615000000070E+03 0.29615000000106E+03 0.29698467988346E+03 0.30012681417639E+03 0.29615000000070E+03
 0.29615000000106E+03 0.29955087955715E+03 0.29615000000573E+03 0.49173811430982E+02 0.40902237707037E+02
 0.68988462634214E+02 0.23384199522100E+03 0.16450859027361E+03 0.55193916353754E+02 0.64152594147599E+02
 0.58166266103806E+02 0.18516674782143E+03 0.54658376407286E+02 0.64091890008950E+02 0.57654906895770E+02
 0.18510734898548E+03 0.55193916353754E+02 0.64152594147599E+02 0.58166266103807E+02 0.18516674782143E+03
 0.54658376407286E+02 0.64091890008950E+02 0.57654906895770E+02 0.18510734898548E+03 0.11658197369551E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33921872150712E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94237362707401E-01 0.00000000000000E+00 0.00000000000000E+00 0.94237362707401E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10336703409177E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10336703409177E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95906117883013E-01 0.10538085257265E+00 0.32037754103007E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    360.00653673
 0.44908288884434E-01 0.31129758091118E+03 0.43691882999170E+03 0.43503835154379E+03 0.43429534977419E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16006919790123E+00 0.00000000000000E+00 -.13373815159898E+02
 0.79402616654433E-02 0.10131497420634E+01 0.10075234717789E+04 0.37782130191707E+03 0.78961674349413E+01
 0.29610627881030E+01 0.32542975270329E+03 0.29615000010084E+03 0.32158086081246E+03 0.34043538344374E+03
 0.29615000000515E+03 0.29615000000845E+03 0.31856450768675E+03 0.34040271746823E+03 0.29615000000412E+03
 0.29615000000841E+03 0.32158086081246E+03 0.34043538344375E+03 0.29615000000515E+03 0.29615000000845E+03
 0.31856450768675E+03 0.34040271746823E+03 0.29615000000412E+03 0.29615000000841E+03 0.38134757755729E+03
 0.30193293133394E+03 0.19252338642208E+04 0.18657509081975E+04 0.70380571671555E+03 0.12614741936273E+04
 0.55414944832819E+03 0.11102511973570E+04 0.11045836790565E+04 0.11102511973570E+04 0.18291435821597E+04
 0.99542759677145E+03 0.11041861406619E+04 0.99542759677145E+03 0.18290514684357E+04 0.11102511973570E+04
 0.11045836790565E+04 0.11102511973570E+04 0.18291435821597E+04 0.99542759677145E+03 0.11041861406619E+04
 0.99542759677145E+03 0.18290514684357E+04 0.18411290294808E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48073248706305E+03 0.12948030283625E+01
 0.12948030283625E+01 0.12600724918556E+01 0.18103094837198E+00 0.29869522616814E+03 0.32029582609404E+03
 0.31768890803405E+03 0.31752457142633E+03 0.23000000000000E+00 0.00000000000000E+00 0.20539747646749E+00
 0.00000000000000E+00 -.36997087145629E+01 0.99113297078205E-03 0.44991864220092E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17780992494255E+02 0.66678721853457E+01 0.30193189179453E+03 0.38137187981020E+03
 0.29705513700656E+03 0.30022714966943E+03 0.29615000000072E+03 0.29615000000127E+03 0.29704617882152E+03
 0.30022685227608E+03 0.29615000000072E+03 0.29615000000127E+03 0.29705513700656E+03 0.30022714966943E+03
 0.29615000000072E+03 0.29615000000127E+03 0.29704617882152E+03 0.30022685227608E+03 0.29615000000072E+03
 0.29615000000127E+03 0.29963705635666E+03 0.29615000000810E+03 0.49249772818943E+02 0.40616249502672E+02
 0.71420635417942E+02 0.23477929224997E+03 0.16300155365494E+03 0.57507004832175E+02 0.66367176710105E+02
 0.61380793546156E+02 0.18595631200331E+03 0.56970716652057E+02 0.66297826226303E+02 0.60871311592106E+02
 0.18588909699813E+03 0.57507004832175E+02 0.66367176710105E+02 0.61380793546156E+02 0.18595631200331E+03
 0.56970716652057E+02 0.66297826226303E+02 0.60871311592106E+02 0.18588909699813E+03 0.11735694785054E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33916253283253E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94239226090236E-01 0.00000000000000E+00 0.00000000000000E+00 0.94239226090236E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10344954302744E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10344954302744E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95906424057194E-01 0.10540807485332E+00 0.32029582609404E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    370.00962952
 0.44909023134030E-01 0.31166214244463E+03 0.43730929983078E+03 0.43542840279818E+03 0.43468594781585E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15832613888019E+00 0.00000000000000E+00 -.13393921522309E+02
 0.79401309939722E-02 0.10375442209783E+01 0.10075400526859E+04 0.37782751975723E+03 0.77105147310801E+01
 0.28914430241550E+01 0.32605376465882E+03 0.29615000014294E+03 0.32213821583230E+03 0.34123621694510E+03
 0.29615000000732E+03 0.29615000001226E+03 0.31907865106462E+03 0.34120435948003E+03 0.29615000000581E+03
 0.29615000001219E+03 0.32213821583230E+03 0.34123621694510E+03 0.29615000000732E+03 0.29615000001226E+03
 0.31907865106462E+03 0.34120435948003E+03 0.29615000000581E+03 0.29615000001219E+03 0.38238148527150E+03
 0.30241135219443E+03 0.19322587556234E+04 0.18713737263094E+04 0.70281187083261E+03 0.12525969350577E+04
 0.54627100487095E+03 0.11147158420938E+04 0.11087870806393E+04 0.11147158420938E+04 0.18291996634579E+04
 0.10002691843983E+04 0.11084203120805E+04 0.10002691843983E+04 0.18291302360411E+04 0.11147158420938E+04
 0.11087870806393E+04 0.11147158420938E+04 0.18291996634579E+04 0.10002691843983E+04 0.11084203120805E+04
 0.10002691843983E+04 0.18291302360411E+04 0.18404278145559E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48112233367244E+03 0.12948031764937E+01
 0.12948031764937E+01 0.13000848630413E+01 0.16984126422745E+00 0.29908800707824E+03 0.32022036192214E+03
 0.31782759801362E+03 0.31767886741065E+03 0.23000000000000E+00 0.00000000000000E+00 0.20427768232973E+00
 0.00000000000000E+00 -.36986312282910E+01 0.10498117722905E-02 0.47354430680749E+00 0.76204136885848E+04
 0.28576551332193E+04 0.16893878534691E+02 0.63352044505090E+01 0.30241032573858E+03 0.38240444637694E+03
 0.29711956823437E+03 0.30032531453431E+03 0.29615000000074E+03 0.29615000000154E+03 0.29711030597388E+03
 0.30032489697393E+03 0.29615000000074E+03 0.29615000000154E+03 0.29711956823437E+03 0.30032531453431E+03
 0.29615000000074E+03 0.29615000000154E+03 0.29711030597388E+03 0.30032489697393E+03 0.29615000000074E+03
 0.29615000000154E+03 0.29972150290344E+03 0.29615000001140E+03 0.49128410081916E+02 0.40197584380554E+02
 0.73736423850362E+02 0.23567429697529E+03 0.16156919100567E+03 0.59796754997902E+02 0.68472084854099E+02
 0.64738456055576E+02 0.18670441644392E+03 0.59261889711592E+02 0.68394325695872E+02 0.64233045700236E+02
 0.18662964721413E+03 0.59796754997902E+02 0.68472084854099E+02 0.64738456055576E+02 0.18670441644392E+03
 0.59261889711592E+02 0.68394325695872E+02 0.64233045700236E+02 0.18662964721413E+03 0.11806278231933E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33911583487987E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94240139838001E-01 0.00000000000000E+00 0.00000000000000E+00 0.94240139838001E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10351480660998E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10351480660998E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95906721595755E-01 0.10543324387805E+00 0.32022036192214E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    380.00694546
 0.44909061779230E-01 0.31202142259857E+03 0.43770474775208E+03 0.43582330768077E+03 0.43508127613153E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15661231394204E+00 0.00000000000000E+00 -.13414271233935E+02
 0.79401233026296E-02 0.10614592644346E+01 0.10075410286577E+04 0.37782788574662E+03 0.75367941738782E+01
 0.28262978152043E+01 0.32666791538944E+03 0.29615000019997E+03 0.32268770322194E+03 0.34202338311139E+03
 0.29615000001038E+03 0.29615000001765E+03 0.31958590104604E+03 0.34199229426550E+03 0.29615000000818E+03
 0.29615000001756E+03 0.32268770322194E+03 0.34202338311139E+03 0.29615000001038E+03 0.29615000001765E+03
 0.31958590104604E+03 0.34199229426550E+03 0.29615000000818E+03 0.29615000001756E+03 0.38338879034556E+03
 0.30290419148486E+03 0.19390464087288E+04 0.18767782482374E+04 0.70181314518146E+03 0.12441146105231E+04
 0.53879239961573E+03 0.11190394602939E+04 0.11128397108206E+04 0.11190394602939E+04 0.18292836439956E+04
 0.10049655692601E+04 0.11125014197392E+04 0.10049655692601E+04 0.18292350238702E+04 0.11190394602939E+04
 0.11128397108206E+04 0.11190394602939E+04 0.18292836439955E+04 0.10049655692601E+04 0.11125014197392E+04
 0.10049655692601E+04 0.18292350238702E+04 0.18397838622501E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48151696659773E+03 0.12948033264178E+01
 0.12948033264178E+01 0.13400741267986E+01 0.15905367826746E+00 0.29951475234391E+03 0.32015268199382E+03
 0.31796432291206E+03 0.31783050166932E+03 0.23000000000000E+00 0.00000000000000E+00 0.20315116946889E+00
 0.00000000000000E+00 -.36972595795800E+01 0.11210136458793E-02 0.49722959166183E+00 0.71363984099631E+04
 0.26761494037362E+04 0.16089147014084E+02 0.60334301302814E+01 0.30290318366066E+03 0.38341044673391E+03
 0.29718678403577E+03 0.30042166729942E+03 0.29615000000076E+03 0.29615000000193E+03 0.29717724712874E+03
 0.30042112642962E+03 0.29615000000076E+03 0.29615000000193E+03 0.29718678403577E+03 0.30042166729942E+03
 0.29615000000076E+03 0.29615000000193E+03 0.29717724712874E+03 0.30042112642962E+03 0.29615000000076E+03
 0.29615000000193E+03 0.29980437943113E+03 0.29615000001592E+03 0.48822812421837E+02 0.39663733420756E+02
 0.75944028673847E+02 0.23654763909439E+03 0.16022389027717E+03 0.62066250963446E+02 0.70476165737967E+02
 0.68255894557680E+02 0.18742869544716E+03 0.61534854070391E+02 0.70390286116864E+02 0.67756609722943E+02
 0.18734667829282E+03 0.62066250963446E+02 0.70476165737965E+02 0.68255894557680E+02 0.18742869544715E+03
 0.61534854070391E+02 0.70390286116864E+02 0.67756609722943E+02 0.18734667829282E+03 0.11871321207633E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33907985725543E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94241655264779E-01 0.00000000000000E+00 0.00000000000000E+00 0.94241655264779E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10356626501746E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10356626501746E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95907005777155E-01 0.10545584625460E+00 0.32015268199382E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    390.01332888
 0.44909123191410E-01 0.31237617707251E+03 0.43810478965617E+03 0.43622266907243E+03 0.43548094153952E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15492473577531E+00 0.00000000000000E+00 -.13434908469038E+02
 0.79401115787111E-02 0.10849395830194E+01 0.10075425163356E+04 0.37782844362585E+03 0.73736824844528E+01
 0.27651309316698E+01 0.32727369459546E+03 0.29615000027646E+03 0.32323060567185E+03 0.34279886477100E+03
 0.29615000001464E+03 0.29615000002521E+03 0.32008742525576E+03 0.34276850910670E+03 0.29615000001150E+03
 0.29615000002508E+03 0.32323060567185E+03 0.34279886477100E+03 0.29615000001464E+03 0.29615000002521E+03
 0.32008742525576E+03 0.34276850910670E+03 0.29615000001150E+03 0.29615000002508E+03 0.38437273537272E+03
 0.30341149353702E+03 0.19456242766643E+04 0.18819883191027E+04 0.70080840253876E+03 0.12359800885639E+04
 0.53166764401245E+03 0.11232386789890E+04 0.11167577638724E+04 0.11232386789890E+04 0.18293891947560E+04
 0.10095341538958E+04 0.11164458830731E+04 0.10095341538958E+04 0.18293596817278E+04 0.11232386789890E+04
 0.11167577638724E+04 0.11232386789890E+04 0.18293891947559E+04 0.10095341538958E+04 0.11164458830731E+04
 0.10095341538958E+04 0.18293596817278E+04 0.18391879278563E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48191598726963E+03 0.12948034784603E+01
 0.12948034784603E+01 0.13800996604770E+01 0.14864984387318E+00 0.29997575560200E+03 0.32009381254090E+03
 0.31810011519224E+03 0.31798045246706E+03 0.23000000000000E+00 0.00000000000000E+00 0.20201653798054E+00
 0.00000000000000E+00 -.36957002402503E+01 0.11994720251345E-02 0.52100562996065E+00 0.66696011514755E+04
 0.25011004318033E+04 0.15354920446069E+02 0.57580951672760E+01 0.30341050966394E+03 0.38439312682083E+03
 0.29725711374751E+03 0.30051655957953E+03 0.29615000000079E+03 0.29615000000248E+03 0.29724733162971E+03
 0.30051589286580E+03 0.29615000000079E+03 0.29615000000248E+03 0.29725711374751E+03 0.30051655957953E+03
 0.29615000000079E+03 0.29615000000248E+03 0.29724733162971E+03 0.30051589286580E+03 0.29615000000079E+03
 0.29615000000248E+03 0.29988599338835E+03 0.29615000002205E+03 0.48343945637298E+02 0.39027805533191E+02
 0.78055225456958E+02 0.23741754189656E+03 0.15897204031231E+03 0.64321756831763E+02 0.72391369124092E+02
 0.71954705869253E+02 0.18814480874294E+03 0.63795773509314E+02 0.72297688037906E+02 0.71463490447752E+02
 0.18805587660986E+03 0.64321756831763E+02 0.72391369124092E+02 0.71954705869253E+02 0.18814480874294E+03
 0.63795773509314E+02 0.72297688037906E+02 0.71463490447752E+02 0.18805587660986E+03 0.11932054207186E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33905541489794E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94244807040460E-01 0.00000000000000E+00 0.00000000000000E+00 0.94244807040460E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10360683370559E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10360683370559E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95907273597092E-01 0.10547553713172E+00 0.32009381254090E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    400.00200345
 0.44909738555339E-01 0.31272567609475E+03 0.43850749398753E+03 0.43662455113534E+03 0.43588301232457E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15326745638725E+00 0.00000000000000E+00 -.13455775124834E+02
 0.79400019123668E-02 0.11079318549337E+01 0.10075564323907E+04 0.37783366214653E+03 0.72206606971137E+01
 0.27077477614176E+01 0.32787001032494E+03 0.29615000037761E+03 0.32376589721489E+03 0.34356132049986E+03
 0.29615000002050E+03 0.29615000003565E+03 0.32058226045272E+03 0.34353166358245E+03 0.29615000001606E+03
 0.29615000003547E+03 0.32376589721489E+03 0.34356132049986E+03 0.29615000002050E+03 0.29615000003565E+03
 0.32058226045272E+03 0.34353166358245E+03 0.29615000001606E+03 0.29615000003547E+03 0.38533228709104E+03
 0.30393117910999E+03 0.19519905158389E+04 0.18870044181297E+04 0.69980051081575E+03 0.12281838369117E+04
 0.52488432354186E+03 0.11273115119985E+04 0.11205400021861E+04 0.11273115119985E+04 0.18295104307055E+04
 0.10139721682588E+04 0.11202525637896E+04 0.10139721682588E+04 0.18294984122089E+04 0.11273115119985E+04
 0.11205400021861E+04 0.11273115119985E+04 0.18295104307054E+04 0.10139721682588E+04 0.11202525637896E+04
 0.10139721682588E+04 0.18294984122089E+04 0.18386342811796E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48231746145565E+03 0.12948036321930E+01
 0.12948036321930E+01 0.14200543587521E+01 0.13865417218586E+00 0.30046904538032E+03 0.32004469419972E+03
 0.31823519727835E+03 0.31812884570963E+03 0.23000000000000E+00 0.00000000000000E+00 0.20087705173570E+00
 0.00000000000000E+00 -.36940368669299E+01 0.12859426611023E-02 0.54480561908613E+00 0.62211171944030E+04
 0.23329189479011E+04 0.14684136359349E+02 0.55065511347557E+01 0.30393022422593E+03 0.38535146027362E+03
 0.29733059948609E+03 0.30060993660600E+03 0.29615000000082E+03 0.29615000000323E+03 0.29732060248126E+03
 0.30060914261272E+03 0.29615000000082E+03 0.29615000000323E+03 0.29733059948609E+03 0.30060993660600E+03
 0.29615000000082E+03 0.29615000000323E+03 0.29732060248126E+03 0.30060914261272E+03 0.29615000000082E+03
 0.29615000000323E+03 0.29996630069283E+03 0.29615000003024E+03 0.47704188501217E+02 0.38303300253396E+02
 0.80072508761196E+02 0.23829515413509E+03 0.15782228283009E+03 0.66559573466376E+02 0.74221057347962E+02
 0.75839728559286E+02 0.18886256472621E+03 0.66040834236066E+02 0.74119950151587E+02 0.75358391571470E+02
 0.18876709941049E+03 0.66559573466376E+02 0.74221057347962E+02 0.75839728559286E+02 0.18886256472621E+03
 0.66040834236066E+02 0.74119950151587E+02 0.75358391571470E+02 0.18876709941049E+03 0.11989244485847E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33904312075588E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94250250857409E-01 0.00000000000000E+00 0.00000000000000E+00 0.94250250857409E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10363873835290E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10363873835290E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95907521888807E-01 0.10549199965607E+00 0.32004469419972E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    410.00059517
 0.44911276162191E-01 0.31307109467947E+03 0.43891322000671E+03 0.43702930985891E+03 0.43628785112385E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15163545512740E+00 0.00000000000000E+00 -.13476913304040E+02
 0.79397292012906E-02 0.11305086830913E+01 0.10075910395911E+04 0.37784663984665E+03 0.70764604639075E+01
 0.26536726739653E+01 0.32845898488029E+03 0.29615000051034E+03 0.32429543947611E+03 0.34431350608543E+03
 0.29615000002847E+03 0.29615000004992E+03 0.32107211350365E+03 0.34428451802579E+03 0.29615000002230E+03
 0.29615000004967E+03 0.32429543947611E+03 0.34431350608543E+03 0.29615000002847E+03 0.29615000004992E+03
 0.32107211350365E+03 0.34428451802579E+03 0.29615000002230E+03 0.29615000004967E+03 0.38627153618061E+03
 0.30446395791965E+03 0.19581768477439E+04 0.18918534041111E+04 0.69878677698922E+03 0.12206764279848E+04
 0.51839571711062E+03 0.11312774968200E+04 0.11242051952225E+04 0.11312774968200E+04 0.18296429524888E+04
 0.10183003437634E+04 0.11239404539045E+04 0.10183003437634E+04 0.18296469918073E+04 0.11312774968200E+04
 0.11242051952225E+04 0.11312774968200E+04 0.18296429524888E+04 0.10183003437634E+04 0.11239404539045E+04
 0.10183003437634E+04 0.18296469918073E+04 0.18381150735019E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48272174610494E+03 0.12948037879262E+01
 0.12948037879262E+01 0.14600487256416E+01 0.12903534280238E+00 0.30099493497282E+03 0.32000570420888E+03
 0.31837033012534E+03 0.31827643579264E+03 0.23000000000000E+00 0.00000000000000E+00 0.19972974010493E+00
 0.00000000000000E+00 -.36923200617434E+01 0.13818020514653E-02 0.56869301077878E+00 0.57895412671565E+04
 0.21710779751837E+04 0.14067343625420E+02 0.52752538595327E+01 0.30446303690114E+03 0.38628953660314E+03
 0.29740767968308E+03 0.30070224193816E+03 0.29615000000088E+03 0.29615000000426E+03 0.29739749764508E+03
 0.30070131959965E+03 0.29615000000088E+03 0.29615000000426E+03 0.29740767968308E+03 0.30070224193816E+03
 0.29615000000088E+03 0.29615000000426E+03 0.29739749764508E+03 0.30070131959965E+03 0.29615000000088E+03
 0.29615000000426E+03 0.30004568441061E+03 0.29615000004109E+03 0.46912270392901E+02 0.37497838230897E+02
 0.82009019490611E+02 0.23919342778595E+03 0.15677436319788E+03 0.68787714602378E+02 0.75978085949601E+02
 0.79935265275500E+02 0.18959320183903E+03 0.68277976462270E+02 0.75869936321193E+02 0.79465537158475E+02
 0.18949159114925E+03 0.68787714602378E+02 0.75978085949601E+02 0.79935265275500E+02 0.18959320183903E+03
 0.68277976462270E+02 0.75869936321193E+02 0.79465537158475E+02 0.18949159114925E+03 0.12043792121321E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33904324055503E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94258368522853E-01 0.00000000000000E+00 0.00000000000000E+00 0.94258368522853E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10366394078093E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10366394078093E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95907749677237E-01 0.10550510533598E+00 0.32000570420888E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    420.00334463
 0.44913962019918E-01 0.31341241950559E+03 0.43932108365595E+03 0.43743606466941E+03 0.43669458409465E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15002934218035E+00 0.00000000000000E+00 -.13498287490444E+02
 0.79392535302311E-02 0.11526642550913E+01 0.10076514082259E+04 0.37786927808472E+03 0.69404425136495E+01
 0.26026659426186E+01 0.32904067884128E+03 0.29615000068274E+03 0.32481924944607E+03 0.34505552760884E+03
 0.29615000003922E+03 0.29615000006922E+03 0.32155698833521E+03 0.34502718034863E+03 0.29615000003070E+03
 0.29615000006888E+03 0.32481924944607E+03 0.34505552760884E+03 0.29615000003922E+03 0.29615000006922E+03
 0.32155698833521E+03 0.34502718034863E+03 0.29615000003070E+03 0.29615000006888E+03 0.38719117224346E+03
 0.30500882986447E+03 0.19641918738159E+04 0.18965437509558E+04 0.69776783649897E+03 0.12134382854035E+04
 0.51218160972199E+03 0.11351414606783E+04 0.11277584032467E+04 0.11351414606783E+04 0.18297827998758E+04
 0.10225234548252E+04 0.11275147364420E+04 0.10225234548252E+04 0.18298015635711E+04 0.11351414606783E+04
 0.11277584032467E+04 0.11351414606783E+04 0.18297827998758E+04 0.10225234548252E+04 0.11275147364420E+04
 0.10225234548252E+04 0.18298015635711E+04 0.18376253359062E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48312796398370E+03 0.12948039453982E+01
 0.12948039453982E+01 0.15000597234656E+01 0.11979628200444E+00 0.30155188560249E+03 0.31997718735706E+03
 0.31850566559372E+03 0.31842333579470E+03 0.23000000000000E+00 0.00000000000000E+00 0.19857538701673E+00
 0.00000000000000E+00 -.36905872635282E+01 0.14883707994104E-02 0.59265252094810E+00 0.53750046716647E+04
 0.20156267518743E+04 0.13498634895203E+02 0.50619880857011E+01 0.30500794730807E+03 0.38720804741526E+03
 0.29748855924462E+03 0.30079358920924E+03 0.29615000000094E+03 0.29615000000565E+03 0.29747822226269E+03
 0.30079253820897E+03 0.29615000000094E+03 0.29615000000565E+03 0.29748855924462E+03 0.30079358920924E+03
 0.29615000000094E+03 0.29615000000565E+03 0.29747822226269E+03 0.30079253820897E+03 0.29615000000094E+03
 0.29615000000565E+03 0.30012424282538E+03 0.29615000005532E+03 0.45978030262060E+02 0.36619165899182E+02
 0.83870323862737E+02 0.24011989762735E+03 0.15583022214530E+03 0.71006162044330E+02 0.77668288795165E+02
 0.84252092919587E+02 0.19034345776896E+03 0.70507093880484E+02 0.77553508490189E+02 0.83795603019692E+02
 0.19023611266454E+03 0.71006162044330E+02 0.77668288795165E+02 0.84252092919587E+02 0.19034345776896E+03
 0.70507093880484E+02 0.77553508490189E+02 0.83795603019692E+02 0.19023611266454E+03 0.12096231485231E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33905590966483E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94269309723793E-01 0.00000000000000E+00 0.00000000000000E+00 0.94269309723793E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10368388637553E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10368388637553E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95907955740256E-01 0.10551473661965E+00 0.31997718735706E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    430.00470275
 0.44917898502274E-01 0.31374964836738E+03 0.43973028886874E+03 0.43784402699431E+03 0.43710243046603E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14844964739035E+00 0.00000000000000E+00 -.13519851015178E+02
 0.79385568806896E-02 0.11743938970483E+01 0.10077398348634E+04 0.37790243807378E+03 0.68120245005593E+01
 0.25545091877097E+01 0.32961514252482E+03 0.29615000090449E+03 0.32533733626703E+03 0.34578747172638E+03
 0.29615000005353E+03 0.29615000009500E+03 0.32203688204700E+03 0.34575973886738E+03 0.29615000004192E+03
 0.29615000009454E+03 0.32533733626703E+03 0.34578747172638E+03 0.29615000005353E+03 0.29615000009500E+03
 0.32203688204700E+03 0.34575973886738E+03 0.29615000004192E+03 0.29615000009454E+03 0.38809183545949E+03
 0.30556477172197E+03 0.19700436258369E+04 0.19010834683829E+04 0.69674432317065E+03 0.12064516390122E+04
 0.50622359422565E+03 0.11389078961524E+04 0.11312044303695E+04 0.11389078961524E+04 0.18299267344828E+04
 0.10266459418246E+04 0.11309803307534E+04 0.10266459418246E+04 0.18299589867150E+04 0.11389078961524E+04
 0.11312044303695E+04 0.11389078961524E+04 0.18299267344828E+04 0.10266459418246E+04 0.11309803307534E+04
 0.10266459418246E+04 0.18299589867150E+04 0.18371608626098E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48353533099566E+03 0.12948041042652E+01
 0.12948041042652E+01 0.15400651559230E+01 0.11093870601801E+00 0.30213804071894E+03 0.31995928089068E+03
 0.31864123734116E+03 0.31856955668296E+03 0.23000000000000E+00 0.00000000000000E+00 0.19741474018082E+00
 0.00000000000000E+00 -.36888555495202E+01 0.16072052886311E-02 0.61666972179751E+00 0.49775844172427E+04
 0.18665941564660E+04 0.12972908701729E+02 0.48648407631485E+01 0.30556393190722E+03 0.38810763397025E+03
 0.29757343828312E+03 0.30088408153036E+03 0.29615000000101E+03 0.29615000000749E+03 0.29756297652717E+03
 0.30088290225438E+03 0.29615000000101E+03 0.29615000000749E+03 0.29757343828312E+03 0.30088408153036E+03
 0.29615000000101E+03 0.29615000000749E+03 0.29756297652717E+03 0.30088290225438E+03 0.29615000000101E+03
 0.29615000000749E+03 0.30020206446143E+03 0.29615000007381E+03 0.44911000301529E+02 0.35673270903044E+02
 0.85661597689101E+02 0.24107990421020E+03 0.15498999853266E+03 0.73214453261402E+02 0.79297003200371E+02
 0.88799722852437E+02 0.19111822386150E+03 0.72727635364777E+02 0.79176025432482E+02 0.88358000210842E+02
 0.19100557220344E+03 0.73214453261402E+02 0.79297003200371E+02 0.88799722852436E+02 0.19111822386150E+03
 0.72727635364777E+02 0.79176025432482E+02 0.88358000210843E+02 0.19100557220344E+03 0.12146957402921E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33908106509324E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94282967737297E-01 0.00000000000000E+00 0.00000000000000E+00 0.94282967737297E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10369965564670E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10369965564670E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908139524264E-01 0.10552084574229E+00 0.31995928089068E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    440.14171137
 0.44923566165817E-01 0.31408712988864E+03 0.44014589252582E+03 0.43825822280445E+03 0.43751641044265E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14687491327755E+00 0.00000000000000E+00 -.13542268915403E+02
 0.79375544476990E-02 0.11959942951793E+01 0.10078671022306E+04 0.37795016333648E+03 0.66889951166535E+01
 0.25083731687451E+01 0.33019037099524E+03 0.29615000119181E+03 0.32585686582545E+03 0.34651963643730E+03
 0.29615000007272E+03 0.29615000012969E+03 0.32251841452971E+03 0.34649250104978E+03 0.29615000005698E+03
 0.29615000012907E+03 0.32585686582545E+03 0.34651963643730E+03 0.29615000007272E+03 0.29615000012969E+03
 0.32251841452971E+03 0.34649250104978E+03 0.29615000005698E+03 0.29615000012907E+03 0.38898697249852E+03
 0.30613895522728E+03 0.19758189610781E+04 0.19055392789833E+04 0.69569616704982E+03 0.11995941378279E+04
 0.50041948994280E+03 0.11426324144099E+04 0.11345935812519E+04 0.11426324144099E+04 0.18300723301670E+04
 0.10307283758345E+04 0.11343878992440E+04 0.10307283758345E+04 0.18301170980435E+04 0.11426324144099E+04
 0.11345935812519E+04 0.11426324144099E+04 0.18300723301670E+04 0.10307283758345E+04 0.11343878992440E+04
 0.10307283758345E+04 0.18301170980435E+04 0.18367073807480E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48394886100816E+03 0.12948042694267E+01
 0.12948042694267E+01 0.15806131904035E+01 0.10234554800629E+00 0.30276020181577E+03 0.31995191736687E+03
 0.31877892033404E+03 0.31871709783698E+03 0.23000000000000E+00 0.00000000000000E+00 0.19623183946747E+00
 0.00000000000000E+00 -.36875212209833E+01 0.17421496660092E-02 0.64107447281769E+00 0.45920279733060E+04
 0.17220104899897E+04 0.12479049376023E+02 0.46796435160087E+01 0.30613816279603E+03 0.38900172932352E+03
 0.29766367347444E+03 0.30097507305260E+03 0.29615000000110E+03 0.29615000000996E+03 0.29765311553841E+03
 0.30097376484608E+03 0.29615000000110E+03 0.29615000000996E+03 0.29766367347444E+03 0.30097507305260E+03
 0.29615000000110E+03 0.29615000000996E+03 0.29765311553841E+03 0.30097376484608E+03 0.29615000000110E+03
 0.29615000000996E+03 0.30028030760750E+03 0.29615000009799E+03 0.43701939185933E+02 0.34648873544788E+02
 0.87411562269175E+02 0.24209134306930E+03 0.15424272298878E+03 0.75443004895303E+02 0.80890952953053E+02
 0.93657376747124E+02 0.19193241616563E+03 0.74970119790580E+02 0.80764152252924E+02 0.93232078970767E+02
 0.19181483465730E+03 0.75443004895303E+02 0.80890952953053E+02 0.93657376747124E+02 0.19193241616563E+03
 0.74970119790580E+02 0.80764152252919E+02 0.93232078970767E+02 0.19181483465728E+03 0.12196914503194E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33911911913080E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94305832780946E-01 0.00000000000000E+00 0.00000000000000E+00 0.94305832780946E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10371251951388E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10371251951388E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908296445637E-01 0.10552344829209E+00 0.31995191736687E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    450.07145111
 0.44930357726390E-01 0.31441409024997E+03 0.44055334605047E+03 0.43866418542165E+03 0.43792208622642E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14535773474473E+00 0.00000000000000E+00 -.13563543139422E+02
 0.79363537608731E-02 0.12167473198122E+01 0.10080195819194E+04 0.37800734321979E+03 0.65749066135068E+01
 0.24655899800650E+01 0.33074730068922E+03 0.29615000154875E+03 0.32636061568421E+03 0.34722766835396E+03
 0.29615000009737E+03 0.29615000017438E+03 0.32298562301626E+03 0.34720109533244E+03 0.29615000007636E+03
 0.29615000017356E+03 0.32636061568421E+03 0.34722766835397E+03 0.29615000009737E+03 0.29615000017438E+03
 0.32298562301626E+03 0.34720109533244E+03 0.29615000007636E+03 0.29615000017356E+03 0.38984679554504E+03
 0.30671111654915E+03 0.19813329590758E+04 0.19097727056551E+04 0.69466608377742E+03 0.11930921026379E+04
 0.49495268844158E+03 0.11461948874543E+04 0.11378180272724E+04 0.11461948874543E+04 0.18302130648396E+04
 0.10346386356877E+04 0.11376291137384E+04 0.10346386356877E+04 0.18302690545341E+04 0.11461948874543E+04
 0.11378180272724E+04 0.11461948874543E+04 0.18302130648395E+04 0.10346386356877E+04 0.11376291137384E+04
 0.10346386356877E+04 0.18302690545341E+04 0.18362810119056E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48435412410701E+03 0.12948044261624E+01
 0.12948044261624E+01 0.16203321493857E+01 0.94298900205239E-01 0.30339537994813E+03 0.31995504783112E+03
 0.31891400885170E+03 0.31886094194974E+03 0.23000000000000E+00 0.00000000000000E+00 0.19506689208876E+00
 0.00000000000000E+00 -.36818453320057E+01 0.18908094348197E-02 0.66503929121299E+00 0.42309922156501E+04
 0.15866220808688E+04 0.12029364438616E+02 0.45110116644810E+01 0.30671037351450E+03 0.38986058470784E+03
 0.29775656463304E+03 0.30106361872321E+03 0.29615000000120E+03 0.29615000001312E+03 0.29774594231714E+03
 0.30106218568453E+03 0.29615000000120E+03 0.29615000001312E+03 0.29775656463304E+03 0.30106361872321E+03
 0.29615000000120E+03 0.29615000001312E+03 0.29774594231714E+03 0.30106218568453E+03 0.29615000000120E+03
 0.29615000001312E+03 0.30035644518465E+03 0.29615000012829E+03 0.42400974951735E+02 0.33584785575384E+02
 0.89067370342612E+02 0.24312226188520E+03 0.15360955469088E+03 0.77617480316120E+02 0.82402127906163E+02
 0.98671219893700E+02 0.19276065114559E+03 0.77159668569477E+02 0.82270061587441E+02 0.98263370071989E+02
 0.19263866673351E+03 0.77617480316120E+02 0.82402127906163E+02 0.98671219893700E+02 0.19276065114559E+03
 0.77159668569477E+02 0.82270061587437E+02 0.98263370071989E+02 0.19263866673351E+03 0.12244658608367E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33916827245990E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94317899178545E-01 0.00000000000000E+00 0.00000000000000E+00 0.94317899178545E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10372196839670E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10372196839670E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908498418330E-01 0.10552264397504E+00 0.31995504783112E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    460.01125880
 0.44936998903132E-01 0.31473875798361E+03 0.44096154897815E+03 0.43907085783799E+03 0.43832843289842E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14386384220738E+00 0.00000000000000E+00 -.13585183284967E+02
 0.79351799926845E-02 0.12371247866841E+01 0.10081686877141E+04 0.37806325789279E+03 0.64666071572641E+01
 0.24249776839740E+01 0.33129885159390E+03 0.29615000199561E+03 0.32686024564436E+03 0.34792782575860E+03
 0.29615000012923E+03 0.29615000023229E+03 0.32344932785968E+03 0.34790179510251E+03 0.29615000010147E+03
 0.29615000023122E+03 0.32686024564436E+03 0.34792782575860E+03 0.29615000012923E+03 0.29615000023229E+03
 0.32344932785968E+03 0.34790179510251E+03 0.29615000010147E+03 0.29615000023122E+03 0.39069121543855E+03
 0.30729250346107E+03 0.19867238769518E+04 0.19138965042873E+04 0.69364116233411E+03 0.11867981443984E+04
 0.48968877625258E+03 0.11496833889698E+04 0.11409613061852E+04 0.11496833889698E+04 0.18303573128682E+04
 0.10384726654622E+04 0.11407880077503E+04 0.10384726654622E+04 0.18304235767230E+04 0.11496833889698E+04
 0.11409613061852E+04 0.11496833889698E+04 0.18303573128682E+04 0.10384726654622E+04 0.11407880077503E+04
 0.10384726654622E+04 0.18304235767230E+04 0.18358794817476E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48476007605564E+03 0.12948045855940E+01
 0.12948045855940E+01 0.16600913801219E+01 0.86608841664918E-01 0.30405403499358E+03 0.31996827160614E+03
 0.31904939587347E+03 0.31900421522716E+03 0.23000000000000E+00 0.00000000000000E+00 0.19389460535192E+00
 0.00000000000000E+00 -.36754116499119E+01 0.20586955685452E-02 0.68908727431099E+00 0.38859558072753E+04
 0.14572334277282E+04 0.11609559918225E+02 0.43535849693345E+01 0.30729181448188E+03 0.39070407011265E+03
 0.29785449507472E+03 0.30115182136478E+03 0.29615000000133E+03 0.29615000001723E+03 0.29784383825646E+03
 0.30115026513090E+03 0.29615000000133E+03 0.29615000001723E+03 0.29785449507472E+03 0.30115182136478E+03
 0.29615000000133E+03 0.29615000001723E+03 0.29784383825646E+03 0.30115026513090E+03 0.29615000000133E+03
 0.29615000001723E+03 0.30043229195131E+03 0.29615000016655E+03 0.40992117256278E+02 0.32461186825543E+02
 0.90670819994120E+02 0.24419499642022E+03 0.15307082232613E+03 0.79782058845835E+02 0.83869121709748E+02
 0.10394783075120E+03 0.19362150653068E+03 0.79340659964709E+02 0.83732241587284E+02 0.10355869414085E+03
 0.19349555809584E+03 0.79782058845835E+02 0.83869121709748E+02 0.10394783075120E+03 0.19362150653068E+03
 0.79340659964709E+02 0.83732241587284E+02 0.10355869414085E+03 0.19349555809584E+03 0.12291369193474E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33922898966137E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94335807497673E-01 0.00000000000000E+00 0.00000000000000E+00 0.94335807497673E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10372894918198E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10372894918198E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908691407333E-01 0.10551850192209E+00 0.31996827160614E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    470.00326090
 0.44944900788548E-01 0.31506142150418E+03 0.44137172389560E+03 0.43947938922575E+03 0.43873656656065E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14238674749882E+00 0.00000000000000E+00 -.13607132860559E+02
 0.79337840199276E-02 0.12572174596353E+01 0.10083460779757E+04 0.37812977924087E+03 0.63632587494613E+01
 0.23862220310480E+01 0.33184736006813E+03 0.29615000255441E+03 0.32735779809642E+03 0.34862333826170E+03
 0.29615000017035E+03 0.29615000030721E+03 0.32391138657000E+03 0.34859783271257E+03 0.29615000013394E+03
 0.29615000030582E+03 0.32735779809642E+03 0.34862333826170E+03 0.29615000017035E+03 0.29615000030721E+03
 0.32391138657000E+03 0.34859783271257E+03 0.29615000013394E+03 0.29615000030582E+03 0.39152534451765E+03
 0.30788513535813E+03 0.19920191236294E+04 0.19179264088009E+04 0.69260362523857E+03 0.11806473259221E+04
 0.48458068255739E+03 0.11531157053865E+04 0.11440379347688E+04 0.11531157053865E+04 0.18304998237256E+04
 0.10422497866791E+04 0.11438792454744E+04 0.10422497866791E+04 0.18305755332495E+04 0.11531157053865E+04
 0.11440379347688E+04 0.11531157053865E+04 0.18304998237255E+04 0.10422497866791E+04 0.11438792454744E+04
 0.10422497866791E+04 0.18305755332495E+04 0.18354878676178E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48516783919552E+03 0.12948047473053E+01
 0.12948047473053E+01 0.17000593885377E+01 0.79242054875146E-01 0.30473732794713E+03 0.31999137653576E+03
 0.31918553509887E+03 0.31914742930834E+03 0.23000000000000E+00 0.00000000000000E+00 0.19271004458722E+00
 0.00000000000000E+00 -.36696097895715E+01 0.22500833365545E-02 0.71332022781984E+00 0.35554238681001E+04
 0.13332839505375E+04 0.11215159318348E+02 0.42056847443806E+01 0.30788450322987E+03 0.39153730992406E+03
 0.29795784687988E+03 0.30124011137057E+03 0.29615000000151E+03 0.29615000002255E+03 0.29794718429215E+03
 0.30123843375994E+03 0.29615000000151E+03 0.29615000002255E+03 0.29795784687988E+03 0.30124011137057E+03
 0.29615000000151E+03 0.29615000002255E+03 0.29794718429215E+03 0.30123843375994E+03 0.29615000000151E+03
 0.29615000002255E+03 0.30050820665671E+03 0.29615000021479E+03 0.39473556953716E+02 0.31271848874735E+02
 0.92232229105345E+02 0.24531466873443E+03 0.15262127848356E+03 0.81946462958123E+02 0.85301498134890E+02
 0.10952942295577E+03 0.19451926892061E+03 0.81522801844932E+02 0.85160235681069E+02 0.10916026165027E+03
 0.19438977402403E+03 0.81946462958123E+02 0.85301498134890E+02 0.10952942295577E+03 0.19451926892060E+03
 0.81522801844932E+02 0.85160235681069E+02 0.10916026165027E+03 0.19438977402403E+03 0.12337253217454E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33930115431805E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94357784189955E-01 0.00000000000000E+00 0.00000000000000E+00 0.94357784189955E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10373373392932E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10373373392932E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908854007480E-01 0.10551106789762E+00 0.31999137653576E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    480.00440971
 0.44953728940277E-01 0.31538106995300E+03 0.44178177470392E+03 0.43988771369751E+03 0.43914443710271E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14093272239209E+00 0.00000000000000E+00 -.13629036413014E+02
 0.79322250944648E-02 0.12769418130834E+01 0.10085442488996E+04 0.37820409333737E+03 0.62649683157315E+01
 0.23493631183993E+01 0.33239065198375E+03 0.29615000324609E+03 0.32785129466928E+03 0.34931141057456E+03
 0.29615000022281E+03 0.29615000040303E+03 0.32436996230713E+03 0.34928641166703E+03 0.29615000017545E+03
 0.29615000040123E+03 0.32785129466928E+03 0.34931141057456E+03 0.29615000022281E+03 0.29615000040303E+03
 0.32436996230713E+03 0.34928641166703E+03 0.29615000017545E+03 0.29615000040123E+03 0.39234596946364E+03
 0.30848581638939E+03 0.19972014031380E+04 0.19218518901713E+04 0.69155999039568E+03 0.11746591356415E+04
 0.47964134529381E+03 0.11564801437527E+04 0.11470379661863E+04 0.11564801437527E+04 0.18306384211943E+04
 0.10459568748562E+04 0.11468928894022E+04 0.10459568748562E+04 0.18307227684648E+04 0.11564801437527E+04
 0.11470379661863E+04 0.11564801437527E+04 0.18306384211943E+04 0.10459568748562E+04 0.11468928894022E+04
 0.10459568748562E+04 0.18307227684648E+04 0.18351069785795E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48557536041922E+03 0.12948049086777E+01
 0.12948049086777E+01 0.17400639837722E+01 0.72230136235242E-01 0.30544033171815E+03 0.32002397585729E+03
 0.31932172345531E+03 0.31928988129680E+03 0.23000000000000E+00 0.00000000000000E+00 0.19151831118299E+00
 0.00000000000000E+00 -.36642347757274E+01 0.24685155758832E-02 0.73763425595707E+00 0.32408140658126E+04
 0.12153052746797E+04 0.10845483293913E+02 0.40670562352172E+01 0.30848524354114E+03 0.39235709122251E+03
 0.29806655621364E+03 0.30132819233904E+03 0.29615000000177E+03 0.29615000002935E+03 0.29805591669986E+03
 0.30132639606139E+03 0.29615000000177E+03 0.29615000002936E+03 0.29806655621364E+03 0.30132819233904E+03
 0.29615000000177E+03 0.29615000002935E+03 0.29805591669986E+03 0.30132639606139E+03 0.29615000000177E+03
 0.29615000002936E+03 0.30058393443489E+03 0.29615000027500E+03 0.37857090295343E+02 0.30020918973570E+02
 0.93748047740876E+02 0.24647619205356E+03 0.15225940407398E+03 0.84099672336764E+02 0.86696076644218E+02
 0.11540918452715E+03 0.19545007559395E+03 0.83694922861703E+02 0.86550880338150E+02 0.11506109659438E+03
 0.19531746296714E+03 0.84099672336764E+02 0.86696076644214E+02 0.11540918452715E+03 0.19545007559394E+03
 0.83694922861703E+02 0.86550880338150E+02 0.11506109659438E+03 0.19531746296714E+03 0.12382089781139E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33938406193355E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94379848642409E-01 0.00000000000000E+00 0.00000000000000E+00 0.94379848642409E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10373623078406E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10373623078406E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95908989726622E-01 0.10550047487711E+00 0.32002397585729E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    490.00456107
 0.44962978779354E-01 0.31569767635022E+03 0.44219106308950E+03 0.44029522326828E+03 0.43955145181500E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13950293004870E+00 0.00000000000000E+00 -.13650843449637E+02
 0.79305924066680E-02 0.12962840617569E+01 0.10087518800328E+04 0.37828195501229E+03 0.61714868183730E+01
 0.23143075568899E+01 0.33292839862805E+03 0.29615000409576E+03 0.32834041294695E+03 0.34999161913233E+03
 0.29615000028921E+03 0.29615000052454E+03 0.32482474942137E+03 0.34996710896728E+03 0.29615000022808E+03
 0.29615000052223E+03 0.32834041294695E+03 0.34999161913233E+03 0.29615000028921E+03 0.29615000052454E+03
 0.32482474942137E+03 0.34996710896728E+03 0.29615000022808E+03 0.29615000052223E+03 0.39315289568239E+03
 0.30909323048794E+03 0.20022726941234E+04 0.19256768695252E+04 0.69051227209542E+03 0.11688297403183E+04
 0.47486490686242E+03 0.11597774452589E+04 0.11499629709778E+04 0.11597774452589E+04 0.18307729183789E+04
 0.10495944197787E+04 0.11498305706285E+04 0.10495944197787E+04 0.18308651498253E+04 0.11597774452589E+04
 0.11499629709778E+04 0.11597774452589E+04 0.18307729183789E+04 0.10495944197787E+04 0.11498305706285E+04
 0.10495944197787E+04 0.18308651498253E+04 0.18347368175702E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48598204337693E+03 0.12948050693390E+01
 0.12948050693390E+01 0.17800645892153E+01 0.65576959541366E-01 0.30616014023271E+03 0.32006563376444E+03
 0.31945771377328E+03 0.31943136207036E+03 0.23000000000000E+00 0.00000000000000E+00 0.19032067236880E+00
 0.00000000000000E+00 -.36592791820808E+01 0.27189611585607E-02 0.76200453563009E+00 0.29423002144814E+04
 0.11033625804305E+04 0.10498625173386E+02 0.39369844400196E+01 0.30909271934421E+03 0.39316321586641E+03
 0.29818083018958E+03 0.30141604742872E+03 0.29615000000209E+03 0.29615000003800E+03 0.29817024216040E+03
 0.30141413565200E+03 0.29615000000209E+03 0.29615000003800E+03 0.29818083018958E+03 0.30141604742872E+03
 0.29615000000209E+03 0.29615000003800E+03 0.29817024216040E+03 0.30141413565200E+03 0.29615000000209E+03
 0.29615000003800E+03 0.30065946039471E+03 0.29615000034953E+03 0.36149982423358E+02 0.28708617206330E+02
 0.95219804209950E+02 0.24767710128097E+03 0.15198119804997E+03 0.86237314242941E+02 0.88054275177556E+02
 0.12159935624880E+03 0.19641214287623E+03 0.85852537554934E+02 0.87905591844677E+02 0.12127332544001E+03
 0.19627683673366E+03 0.86237314242942E+02 0.88054275177556E+02 0.12159935624880E+03 0.19641214287623E+03
 0.85852537554934E+02 0.87905591844677E+02 0.12127332544001E+03 0.19627683673366E+03 0.12425777897485E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33947713517313E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94401667936556E-01 0.00000000000000E+00 0.00000000000000E+00 0.94401667936556E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10373637466482E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10373637466482E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95909099414199E-01 0.10548686934282E+00 0.32006563376444E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
    500.00000000
 0.44972425541097E-01 0.31601116213040E+03 0.44259922851176E+03 0.44070157104852E+03 0.43995727057463E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13809755124030E+00 0.00000000000000E+00 -.13672604891614E+02
 0.79289256758918E-02 0.13152438418859E+01 0.10089639286599E+04 0.37836147324746E+03 0.60825223013622E+01
 0.22809458630108E+01 0.33346060082522E+03 0.29615000513268E+03 0.32882512306144E+03 0.35066399092467E+03
 0.29615000037260E+03 0.29615000067747E+03 0.32527571090736E+03 0.35063995240460E+03 0.29615000029432E+03
 0.29615000067453E+03 0.32882512306144E+03 0.35066399092467E+03 0.29615000037260E+03 0.29615000067747E+03
 0.32527571090736E+03 0.35063995240460E+03 0.29615000029432E+03 0.29615000067453E+03 0.39394655467049E+03
 0.30970646168701E+03 0.20072376850979E+04 0.19294057366800E+04 0.68946004949293E+03 0.11631487592290E+04
 0.47024140948860E+03 0.11630102259101E+04 0.11528159150826E+04 0.11630102259101E+04 0.18309027262949E+04
 0.10531649985201E+04 0.11526953204714E+04 0.10531649985201E+04 0.18310021452153E+04 0.11630102259101E+04
 0.11528159150826E+04 0.11630102259101E+04 0.18309027262949E+04 0.10531649985201E+04 0.11526953204714E+04
 0.10531649985201E+04 0.18310021452153E+04 0.18343756679938E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48638754643164E+03 0.12948052296644E+01
 0.12948052296644E+01 0.18200463449417E+01 0.59281249550799E-01 0.30689424370817E+03 0.32011589068089E+03
 0.31959336017845E+03 0.31957177490312E+03 0.23000000000000E+00 0.00000000000000E+00 0.18911763759201E+00
 0.00000000000000E+00 -.36548164118782E+01 0.30077165581251E-02 0.78642185283136E+00 0.26598251016668E+04
 0.99743441312505E+03 0.10172657297349E+02 0.38147464865060E+01 0.30970601440118E+03 0.39395611398992E+03
 0.29830089952128E+03 0.30150370335643E+03 0.29615000000250E+03 0.29615000004887E+03 0.29829039067077E+03
 0.30150167959906E+03 0.29615000000249E+03 0.29615000004888E+03 0.29830089952128E+03 0.30150370335643E+03
 0.29615000000250E+03 0.29615000004887E+03 0.29829039067077E+03 0.30150167959906E+03 0.29615000000249E+03
 0.29615000004888E+03 0.30073480514251E+03 0.29615000044118E+03 0.34358148972360E+02 0.27333934180098E+02
 0.96649799062655E+02 0.24891524178271E+03 0.15178219372474E+03 0.88356302061964E+02 0.89378149412198E+02
 0.12812013811022E+03 0.19740388162751E+03 0.87992446535068E+02 0.89226417946165E+02 0.12781703898347E+03
 0.19726629611024E+03 0.88356302061964E+02 0.89378149412198E+02 0.12812013811022E+03 0.19740388162751E+03
 0.87992446535068E+02 0.89226417946165E+02 0.12781703898347E+03 0.19726629611024E+03 0.12468222613111E+02
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33957983520185E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94424306836321E-01 0.00000000000000E+00 0.00000000000000E+00 0.94424306836321E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10373432340113E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10373432340113E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.95909182767360E-01 0.10547040463540E+00 0.32011589068089E+03
 0.00000000000000E+00 0.00000000000000E+00 0.29615000000000E+03
