#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-45 0 MONOZONE(1=OUI,0=NON)                                                                     
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
4000.000000 0.004000                                                                                
5000.000000 0.004000                                                                                
5001.000000 0.000000                                                                                
#FINDEBITPYROLYSE                                                                                   
#OPTIONSFOYER                                                                                       
0 0.000000                                                                                          
#PYROCABLE                                                                                          
1 0.000000 1 1 0                                                                                    
30.000000                                                                                           
#FINFOYER                                                                                           
#FINLOCAL                                                                                           
#LOCAL LOC_2                                                                                        
Plenum-LLNL45 0 MONOZONE(1=OUI,0=NON)                                                               
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
#OUVERTURE OUV_3                                                                                    
East                                                                                                
LOC_2 EXT                                                                                           
PAR_9                                                                                               
1.675000 0.275000                                                                                   
2.325000 0.925000                                                                                   
#FERMETURE                                                                                          
#FINFERMETURE                                                                                       
#FINOUVERTURE                                                                                       
#CONDINIT 500.000000 10.000000 30.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-45           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-45           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-45           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-45           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-45           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-45           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-45           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-45           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-45           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-45           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-45           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-45           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-45           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-45           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-45           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-45           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-45           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-45           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL45     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL45     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL45     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL45     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL45     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL45     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL45     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL45     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL45     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL45     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL45     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL45     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL45     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL45     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL45     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL45     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL45     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL45     #HEAT_POWER#W#Total sprinkling power
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
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to upper layer
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Upper layer to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to lower layer
#NOUVELLE LIGNE
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Lower layer to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Global mass flow rate to the outside
#OPENING#OUV_3 #East        #MASS_FLOW_RATE#kg/s#Outside to upper and lower layers
#OPENING#OUV_3 #East        #HEIGHT#m#Upper layer to the outside flow height
#OPENING#OUV_3 #East        #HEIGHT#m#Lower layer to the outside flow height
#NOUVELLE LIGNE
#OPENING#OUV_3 #East        #HEIGHT#m#Outside to inside flow height
#OPENING#OUV_3 #East        #AREA#m2#Area
#NOUVELLE LIGNE
#FIN ENTETE
#DEBUTRESULTAT
      0.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.73131879768489E-06
 0.10000000000072E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.52332576667151E-03 0.52332576667151E-03 0.74123867489846E-03 0.74494486827295E-03
 0.00000000000000E+00 0.49591429402529E-03 0.58360897238787E-03 0.49591429402529E-03 0.58360897238787E-03
 0.49340394679881E-03 0.58360198928660E-03 0.49340394679881E-03 0.58360198928660E-03 0.49591429408475E-03
 0.58360897238787E-03 0.49591429408475E-03 0.58360897238787E-03 0.49340394679881E-03 0.58360198946497E-03
 0.49340394679881E-03 0.58360198946497E-03 0.60133104547819E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.35921979381274E-06 0.99966045117442E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.60920757683842E-03 0.60920757683842E-03
 0.74804394976974E-03 0.75178416951859E-03 0.00000000000000E+00 0.53237686177453E-03 0.58737189630375E-03
 0.53237686177453E-03 0.58737189630375E-03 0.52773558284748E-03 0.58736383208734E-03 0.52773558284748E-03
 0.58736383208734E-03 0.53237686177453E-03 0.58737189630375E-03 0.53237686177453E-03 0.58737189630375E-03
 0.52773558266911E-03 0.58736383220625E-03 0.52773558266911E-03 0.58736383220625E-03 0.48579412417609E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.46084656062346E-04 0.46084656062346E-04 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.28340457592977E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.28340457592977E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.20824965444158E-01 0.20824965444158E-01 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     20.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.19272031430168E-02
 0.99999998097998E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000231740E+03 0.30315000000000E+03 0.30315000318220E+03 0.30315000318220E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000316571E+03 0.30315000316571E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000318220E+03 0.30315000318220E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000316571E+03 0.30315000316571E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001096064E+03
 0.30315000905936E+03 0.54269016817402E-03 0.54269016817402E-03 0.69722330211394E-03 0.70070941862451E-03
 .00000000000000E+00 0.50639816029656E-03 0.60351162891302E-03 0.50639816029656E-03 0.60351162891302E-03
 0.50376646727446E-03 0.60359790300751E-03 0.50376646727446E-03 0.60359790300751E-03 0.50639816023711E-03
 0.60351162891302E-03 0.50639816023711E-03 0.60351162891302E-03 0.50376646810686E-03 0.60359790383991E-03
 0.50376646810686E-03 0.60359790383991E-03 0.60162319077347E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.21300219359485E-02 0.99966081360295E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315000903520E+03 0.30315001098994E+03
 0.30315000341392E+03 0.30315000341392E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000338389E+03
 0.30315000338389E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000341392E+03 0.30315000341392E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000338389E+03 0.30315000338389E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000329096E+03 0.30315000000000E+03 0.57837325666023E-03 0.57837325666023E-03
 0.76552612568429E-03 0.76935375631271E-03 .00000000000000E+00 0.54322935507356E-03 0.59633573632352E-03
 0.54322935507356E-03 0.59633573632352E-03 0.53844568734210E-03 0.59649819463959E-03 0.53844568734210E-03
 0.59649819463959E-03 0.54322935424116E-03 0.59633573555057E-03 0.54322935424116E-03 0.59633573555057E-03
 0.53844568853125E-03 0.59649819576928E-03 0.53844568853125E-03 0.59649819576928E-03 0.48584924950423E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23657348112820E-02 0.00000000000000E+00 0.00000000000000E+00 0.23657348112820E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655984912476E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655984912476E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.40386860574672E-02
 0.64041867253835E-02 0.64041867253835E-02 0.40386860574672E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     30.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.19272031354269E-02
 0.99999998097998E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000289686E+03 0.30315000000000E+03 0.30315000396721E+03 0.30315000396721E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000394658E+03 0.30315000394658E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000396721E+03 0.30315000396721E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000394658E+03 0.30315000394658E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001347258E+03
 0.30315001114555E+03 0.54700144643919E-03 0.54700144643919E-03 0.68733714179619E-03 0.69077382750517E-03
 .00000000000000E+00 0.50863705100324E-03 0.60786724954723E-03 0.50863705100324E-03 0.60786724954723E-03
 0.50598150513131E-03 0.60797697390502E-03 0.50598150513131E-03 0.60797697390502E-03 0.50863705100324E-03
 0.60786724954723E-03 0.50863705100324E-03 0.60786724954723E-03 0.50598150548805E-03 0.60797697432122E-03
 0.50598150548805E-03 0.60797697432122E-03 0.60161096538213E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.21300219348658E-02 0.99966100487274E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001112490E+03 0.30315001349755E+03
 0.30315000425573E+03 0.30315000425573E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000421825E+03
 0.30315000421825E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000425573E+03 0.30315000425573E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000421825E+03 0.30315000421825E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000410455E+03 0.30315000000000E+03 0.57147915435798E-03 0.57147915435798E-03
 0.76932060635327E-03 0.77316720938504E-03 .00000000000000E+00 0.54557325801476E-03 0.59827256489752E-03
 0.54557325801476E-03 0.59827256489752E-03 0.54076200707191E-03 0.59847734135312E-03 0.54076200707191E-03
 0.59847734135312E-03 0.54557325807421E-03 0.59827256495697E-03 0.54557325807421E-03 0.59827256495697E-03
 0.54076200790432E-03 0.59847734200716E-03 0.54076200790432E-03 0.59847734200716E-03 0.48580975464490E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23657348023504E-02 0.00000000000000E+00 0.00000000000000E+00 0.23657348023504E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.40386860574672E-02
 0.64041867253835E-02 0.64041867253835E-02 0.40386860574672E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00000000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.19272031281321E-02
 0.99999998097998E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000339409E+03 0.30315000000000E+03 0.30315000463747E+03 0.30315000463747E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000461330E+03 0.30315000461330E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000463747E+03 0.30315000463747E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000461330E+03 0.30315000461330E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001555624E+03
 0.30315001288029E+03 0.55055285790343E-03 0.55055285790343E-03 0.67923201716082E-03 0.68262817724663E-03
 .00000000000000E+00 0.51045835888924E-03 0.61142443653156E-03 0.51045835888924E-03 0.61142443653156E-03
 0.50778448578784E-03 0.61155427237799E-03 0.50778448578784E-03 0.61155427237799E-03 0.51045835894869E-03
 0.61142443647210E-03 0.51045835894869E-03 0.61142443647210E-03 0.50778448584729E-03 0.61155427237799E-03
 0.50778448584729E-03 0.61155427237799E-03 0.60159673424306E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.21300219344926E-02 0.99966119584995E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001286412E+03 0.30315001557572E+03
 0.30315000497440E+03 0.30315000497440E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000493057E+03
 0.30315000493057E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000497440E+03 0.30315000497440E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000493057E+03 0.30315000493057E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000480005E+03 0.30315000000000E+03 0.56583537055048E-03 0.56583537055048E-03
 0.77241402164770E-03 0.77627609175594E-03 .00000000000000E+00 0.54748272402404E-03 0.59985063406464E-03
 0.54748272402404E-03 0.59985063406464E-03 0.54265054363617E-03 0.60009156642832E-03 0.54265054363617E-03
 0.60009156642832E-03 0.54748272396458E-03 0.59985063406464E-03 0.54748272396458E-03 0.59985063406464E-03
 0.54265054363617E-03 0.60009156642832E-03 0.54265054363617E-03 0.60009156642832E-03 0.48577170650886E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23657348023504E-02 0.00000000000000E+00 0.00000000000000E+00 0.23657348023504E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.40386860574672E-02
 0.64041867253835E-02 0.64041867253835E-02 0.40386860574672E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00025000
 0.30000000000000E+01 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.19272031283344E-02
 0.99999998097998E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.30315000339410E+03 0.30315000000000E+03 0.30315000463749E+03 0.30315000463749E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000461331E+03 0.30315000461331E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000463749E+03 0.30315000463749E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30315000461331E+03 0.30315000461331E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315001555629E+03
 0.30315001288034E+03 0.55055980241680E-03 0.55055980241680E-03 0.67924164530685E-03 0.68263785353338E-03
 .00000000000000E+00 0.51046591736043E-03 0.61142995888175E-03 0.51046591736043E-03 0.61142995888175E-03
 0.50779210383541E-03 0.61155979496601E-03 0.50779210383541E-03 0.61155979496601E-03 0.51046591736043E-03
 0.61142995894121E-03 0.51046591736043E-03 0.61142995894121E-03 0.50779210371650E-03 0.61155979496601E-03
 0.50779210371650E-03 0.61155979496601E-03 0.60160527258403E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.30315000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.30315000000000E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.21300219352993E-02 0.99966119585473E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315001286417E+03 0.30315001557577E+03
 0.30315000497442E+03 0.30315000497442E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000493059E+03
 0.30315000493059E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315000497442E+03 0.30315000497442E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315000493059E+03 0.30315000493059E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30315000480007E+03 0.30315000000000E+03 0.56584433673669E-03 0.56584433673669E-03
 0.77243241938574E-03 0.77629458148267E-03 .00000000000000E+00 0.54749536307644E-03 0.59986079998365E-03
 0.54749536307644E-03 0.59986079998365E-03 0.54266339245452E-03 0.60010173288244E-03 0.54266339245452E-03
 0.60010173288244E-03 0.54749536301698E-03 0.59986079998365E-03 0.54749536301698E-03 0.59986079998365E-03
 0.54266339251397E-03 0.60010173300136E-03 0.54266339251397E-03 0.60010173300136E-03 0.48578211281916E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30315000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23657348023504E-02 0.00000000000000E+00 0.00000000000000E+00 0.23657348023504E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.23655985757972E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.40386860574672E-02
 0.64041867253835E-02 0.64041867253835E-02 0.40386860574672E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     50.00079350
 0.25689525457623E+01 0.30316617735882E+03 0.36612734814573E+03 0.31221259482000E+03 0.31084669895721E+03
 0.22999999986251E+00 0.00000000000000E+00 0.22247596019673E+00 0.00000000000000E+00 0.45362846735483E+01
 0.99999140590225E-03 0.12271133213009E+00 0.80000000000000E+04 0.30000000000000E+04 0.65193652950641E+02
 0.24447619856491E+02 0.30407755971976E+03 0.30315000000000E+03 0.30434853806239E+03 0.30607630510671E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30405669060844E+03 0.30606444094461E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30434853806239E+03 0.30607630510671E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30405669060844E+03 0.30606444094461E+03 0.30315000000000E+03 0.30315000000000E+03 0.31036203892643E+03
 0.30315689691875E+03 0.43444497353911E+03 0.43286580078127E+03 0.40585682532066E+03 0.99199807890436E+03
 0.58411196945710E+03 0.39762788176851E+03 0.27271065737716E+03 0.39582114124656E+03 0.91945570088902E+03
 0.30151579904134E+03 0.26795061152158E+03 0.30026943756447E+03 0.91484472503072E+03 0.39762788176851E+03
 0.27271065737716E+03 0.39582114124656E+03 0.91945570088901E+03 0.30151579904134E+03 0.26795061152158E+03
 0.30026943756447E+03 0.91484472503072E+03 0.60480585730294E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41442499789819E+03 0.12946710941828E+01
 0.12946710941828E+01 0.20305769382112E-01 0.13245640582886E+01 0.30315050954797E+03 0.31448669412435E+03
 0.30447635902569E+03 0.30443397761757E+03 0.22999999996631E+00 0.00000000000000E+00 0.22846305250106E+00
 0.00000000000000E+00 0.54199043084409E+00 0.99966488619031E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.30315688272286E+03 0.31038006708822E+03
 0.30315328893133E+03 0.30348906475905E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315331150152E+03
 0.30348908405568E+03 0.30315000000000E+03 0.30315000000000E+03 0.30315328893133E+03 0.30348906475905E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30315331150152E+03 0.30348908405568E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30328566652462E+03 0.30315000000000E+03 0.10731258421588E+01 0.10707255667451E+01
 0.67909903835198E+00 0.77893043838120E+02 0.77210549304576E+02 0.12180633624226E+01 -.75879232895566E+00
 0.12172627895460E+01 0.11083325283069E+03 0.12243218710246E+01 -.74994640839282E+00 0.12235130904374E+01
 0.11084188148082E+03 0.12180633624226E+01 -.75879232895559E+00 0.12172627895460E+01 0.11083325283069E+03
 0.12243218710247E+01 -.74994640839252E+00 0.12235130904374E+01 0.11084188148083E+03 0.23163383797872E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32652736977805E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10954359945883E+00 0.00000000000000E+00 0.00000000000000E+00 0.10954359945883E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27357489663698E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.27357489663698E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32302417012337E+00 0.32302417012337E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     60.02465069
 0.21830513614739E+01 0.30321008694602E+03 0.39725821522865E+03 0.32882091706483E+03 0.32410479073840E+03
 0.22999999980410E+00 0.00000000000000E+00 0.21789374149899E+00 0.00000000000000E+00 0.17447130848672E+01
 0.99981904621652E-03 0.19024866125043E+00 0.80000000000000E+04 0.30000000000000E+04 0.42050230195678E+02
 0.15768836323379E+02 0.30498310479462E+03 0.30315000000000E+03 0.30554493891551E+03 0.30947439240155E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30499950468957E+03 0.30944522828114E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30554493891551E+03 0.30947439240155E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30499950468957E+03 0.30944522828114E+03 0.30315000000000E+03 0.30315000000000E+03 0.31937023006545E+03
 0.30318359409480E+03 0.50772719129951E+03 0.50389444671704E+03 0.44357349115180E+03 0.12779198389595E+04
 0.83212848035194E+03 0.44960737593015E+03 0.38659373642190E+03 0.44515306904316E+03 0.12159508825290E+04
 0.35189135750261E+03 0.38047531487563E+03 0.34874787969408E+03 0.12101651572987E+04 0.44960737593015E+03
 0.38659373642190E+03 0.44515306904316E+03 0.12159508825290E+04 0.35189135750261E+03 0.38047531487563E+03
 0.34874787969408E+03 0.12101651572987E+04 0.80345298523258E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43199286895362E+03 0.19369250428926E+01
 0.19369250428926E+01 0.60401198149066E-01 0.10893395098531E+01 0.30315069169663E+03 0.32291684572053E+03
 0.30856214402981E+03 0.30831748744894E+03 0.22999999995503E+00 0.00000000000000E+00 0.22706261405945E+00
 0.00000000000000E+00 0.37138260479252E+00 0.99966260177402E-03 0.29920670724624E-01 0.80000000000000E+04
 0.30000000000000E+04 0.26737368535714E+03 0.10026513200893E+03 0.30318349774042E+03 0.31943176946599E+03
 0.30316237835302E+03 0.30393414568658E+03 0.30315000000000E+03 0.30315000000000E+03 0.30316228581731E+03
 0.30393428036476E+03 0.30315000000000E+03 0.30315000000000E+03 0.30316237835302E+03 0.30393414568658E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30316228581731E+03 0.30393428036476E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30360318180915E+03 0.30315000000000E+03 0.40275808977664E+01 0.40089704380349E+01
 0.22463758990676E+01 0.15022331682395E+03 0.14796570904538E+03 0.33327359026917E+01 0.52824446037392E+00
 0.33276090322686E+01 0.15383240260181E+03 0.32894816917294E+01 0.56653478865819E+00 0.32844062726673E+01
 0.15386943725678E+03 0.33327359026917E+01 0.52824446037385E+00 0.33276090322687E+01 0.15383240260181E+03
 0.32894816917294E+01 0.56653478865866E+00 0.32844062726673E+01 0.15386943725678E+03 0.50372757807630E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33741250988950E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.86981823173678E-01 0.00000000000000E+00 0.00000000000000E+00 0.86981823173678E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21664801439782E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21664801439782E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.26765722363064E+00 0.26765722363064E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     70.48863310
 0.18684554160516E+01 0.30328801491147E+03 0.41063418241934E+03 0.34377700639519E+03 0.33646371502478E+03
 0.22999999980380E+00 0.00000000000000E+00 0.21505658469093E+00 0.00000000000000E+00 -.28649650720247E+00
 0.99954211158543E-03 0.22890579949311E+00 0.80000000000000E+04 0.30000000000000E+04 0.34948874243095E+02
 0.13105827841161E+02 0.30585357636225E+03 0.30315000000000E+03 0.30655067617903E+03 0.31229881260196E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30582211209785E+03 0.31225872649715E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30655067617903E+03 0.31229881260196E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30582211209785E+03 0.31225872649715E+03 0.30315000000000E+03 0.30315000000000E+03 0.32633729806114E+03
 0.30324575478032E+03 0.61526092197795E+03 0.60899213314202E+03 0.47554942120491E+03 0.13878559062882E+04
 0.90992873797722E+03 0.50830896661776E+03 0.48309955702340E+03 0.50133398848218E+03 0.13642558867152E+04
 0.40594376795389E+03 0.47703900497598E+03 0.40093348048459E+03 0.13586347109264E+04 0.50830896661776E+03
 0.48309955702340E+03 0.50133398848218E+03 0.13642558867152E+04 0.40594376795389E+03 0.47703900497598E+03
 0.40093348048459E+03 0.13586347109264E+04 0.95981448389881E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44715645731875E+03 0.16920804806249E+01
 0.16920804806249E+01 0.10225712778771E+00 0.90228379122174E+00 0.30315151710540E+03 0.32652667959631E+03
 0.31246599277449E+03 0.31205312112683E+03 0.23000000000000E+00 0.00000000000000E+00 0.22613963724793E+00
 0.00000000000000E+00 0.26167558122512E+00 0.99965879721332E-03 0.55659663815193E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14373065612761E+03 0.53898996047855E+02 0.30324545592143E+03 0.32639367448321E+03
 0.30318088434472E+03 0.30428583677388E+03 0.30315000000000E+03 0.30315000000000E+03 0.30318042000169E+03
 0.30428612848292E+03 0.30315000000000E+03 0.30315000000000E+03 0.30318088434472E+03 0.30428583677388E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30318042000169E+03 0.30428612848292E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30390407251070E+03 0.30315000000000E+03 0.88703905698845E+01 0.87940331123799E+01
 0.66061990604776E+01 0.18564572780426E+03 0.17900649774848E+03 0.66763600754066E+01 0.48721729779814E+01
 0.66592335447405E+01 0.16855755477602E+03 0.65563960969294E+01 0.49316037464491E+01 0.65396132192820E+01
 0.16861446292280E+03 0.66763600754066E+01 0.48721729779814E+01 0.66592335447405E+01 0.16855755477602E+03
 0.65563960969294E+01 0.49316037464492E+01 0.65396132192820E+01 0.16861446292280E+03 0.68011249513479E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34298024729055E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.73208057852346E-01 0.00000000000000E+00 0.00000000000000E+00 0.73208057852346E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18264541679416E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18264541679416E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.77243632536672E-02 0.00000000000000E+00
 0.21709361062954E+00 0.22481797388321E+00 0.00000000000000E+00 0.63864189561087E+00 0.31364189263064E+00
 0.32500000298023E+00 0.42250006586313E+00
     80.04420264
 0.16443914257195E+01 0.30339151277936E+03 0.41935675884694E+03 0.35579267340861E+03 0.34671573912151E+03
 0.22999999980002E+00 0.00000000000000E+00 0.21267911126964E+00 0.00000000000000E+00 -.18030398644232E+01
 0.99918617625084E-03 0.26046231739548E+00 0.80000000000000E+04 0.30000000000000E+04 0.30714615764756E+02
 0.11517980911783E+02 0.30669082153041E+03 0.30315000000000E+03 0.30744093784883E+03 0.31469127591676E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30656693069971E+03 0.31464482248507E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30744093784883E+03 0.31469127591676E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30656693069971E+03 0.31464482248507E+03 0.30315000000000E+03 0.30315000000000E+03 0.33190390289748E+03
 0.30332367058221E+03 0.71343112398276E+03 0.70467034830082E+03 0.51516138092673E+03 0.14683391637508E+04
 0.95060197591948E+03 0.56083787910433E+03 0.56970231929435E+03 0.55150458826512E+03 0.14843458969964E+04
 0.45511711785979E+03 0.56399018561256E+03 0.44832675157240E+03 0.14791335484559E+04 0.56083787910433E+03
 0.56970231929435E+03 0.55150458826512E+03 0.14843458969964E+04 0.45511711785979E+03 0.56399018561256E+03
 0.44832675157240E+03 0.14791335484559E+04 0.10912001188149E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45905727002241E+03 0.15412502155013E+01
 0.15412502155013E+01 0.14047940593649E+00 0.77952826425122E+00 0.30315333731466E+03 0.32833069255743E+03
 0.31524638587021E+03 0.31474603697410E+03 0.23000000000000E+00 0.00000000000000E+00 0.22543598482844E+00
 0.00000000000000E+00 0.17202589905916E+00 0.99965191024472E-03 0.72274770969219E-01 0.80000000000000E+04
 0.30000000000000E+04 0.11068869389302E+03 0.41508260209883E+02 0.30332318741601E+03 0.33194735592092E+03
 0.30320370102140E+03 0.30455767290946E+03 0.30315000000000E+03 0.30315000000000E+03 0.30320278533587E+03
 0.30455809993273E+03 0.30315000000000E+03 0.30315000000000E+03 0.30320370102140E+03 0.30455767290946E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30320278533587E+03 0.30455809993273E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30415180721973E+03 0.30315000000000E+03 0.13369953964962E+02 0.13201767642596E+02
 0.10819076101229E+02 0.20458762066313E+03 0.19371444918139E+03 0.98147845652165E+01 0.90607926252925E+01
 0.97798435214328E+01 0.17605642978023E+03 0.96328289514851E+01 0.91307085533006E+01 0.95986943533757E+01
 0.17612278878316E+03 0.98147845652165E+01 0.90607926252921E+01 0.97798435214328E+01 0.17605642978023E+03
 0.96328289514847E+01 0.91307085533007E+01 0.95986943533752E+01 0.17612278878316E+03 0.79155450304212E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34590776047670E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.62450668947706E-01 0.00000000000000E+00 0.00000000000000E+00 0.62450668947706E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15691861974320E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15691861974320E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.45888722497453E-01 0.00000000000000E+00
 0.14181323247240E+00 0.18770195496985E+00 0.00000000000000E+00 0.57726413212561E+00 0.25226412914538E+00
 0.32500000298023E+00 0.42250006586313E+00
     90.91225622
 0.14431495986283E+01 0.30355206706386E+03 0.42709605167436E+03 0.36766523440650E+03 0.35716805093786E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20999103892079E+00 0.00000000000000E+00 -.32035707196005E+01
 0.99864388440456E-03 0.29583752068516E+00 0.80000000000000E+04 0.30000000000000E+04 0.27041870758895E+02
 0.10140701534585E+02 0.30768641093197E+03 0.30315000000000E+03 0.30844538898936E+03 0.31728909805391E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30742393329111E+03 0.31723799913498E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30844538898936E+03 0.31728909805391E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30742393329111E+03 0.31723799913498E+03 0.30315000000000E+03 0.30315000000000E+03 0.33771371351818E+03
 0.30342512371961E+03 0.82399822356505E+03 0.81217300069330E+03 0.56494808067144E+03 0.15431426173533E+04
 0.97536979627846E+03 0.61938034528273E+03 0.66528809142493E+03 0.60731905831264E+03 0.15995730532973E+04
 0.51063536076897E+03 0.66006304578752E+03 0.50175405625089E+03 0.15948866217342E+04 0.61938034528274E+03
 0.66528809142494E+03 0.60731905831267E+03 0.15995730532973E+04 0.51063536076897E+03 0.66006304578756E+03
 0.50175405625089E+03 0.15948866217343E+04 0.12258540163325E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47334895463199E+03 0.13931038569876E+01
 0.13931038569876E+01 0.18395162025394E+00 0.68357437913024E+00 0.30315738171969E+03 0.32939278724526E+03
 0.31743688654968E+03 0.31689509935359E+03 0.23000000000000E+00 0.00000000000000E+00 0.22471745840198E+00
 0.00000000000000E+00 0.88769449264719E-01 0.99963775228873E-03 0.88061375628122E-01 0.80000000000000E+04
 0.30000000000000E+04 0.90845730525305E+02 0.34067148946989E+02 0.30342451539997E+03 0.33774885734212E+03
 0.30323358824397E+03 0.30482530743121E+03 0.30315000000000E+03 0.30315000000000E+03 0.30323211720861E+03
 0.30482586745886E+03 0.30315000000000E+03 0.30315000000000E+03 0.30323358824397E+03 0.30482530743121E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30323211720861E+03 0.30482586745886E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30439961655307E+03 0.30315000000000E+03 0.17964558345763E+02 0.17656964107648E+02
 0.15160778327823E+02 0.21616329038302E+03 0.20092670816356E+03 0.13086884279498E+02 0.13337583063061E+02
 0.13026261743733E+02 0.17988138121493E+03 0.12849206927920E+02 0.13413538658163E+02 0.12790069849245E+02
 0.17995280575577E+03 0.13086884279498E+02 0.13337583063061E+02 0.13026261743733E+02 0.17988138121493E+03
 0.12849206927920E+02 0.13413538658161E+02 0.12790069849245E+02 0.17995280575576E+03 0.87261447150593E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34744651341238E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.51761356490582E-01 0.00000000000000E+00 0.00000000000000E+00 0.51761356490582E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.13214041942761E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.13214041942761E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.69130035678109E-01 0.00000000000000E+00
 0.82908691774387E-01 0.15203872745250E+00 0.00000000000000E+00 0.52928718956512E+00 0.20428718658489E+00
 0.32500000298023E+00 0.42250006586313E+00
    100.04188332
 0.13020287029853E+01 0.30372731621293E+03 0.43184110282504E+03 0.37623849368601E+03 0.36501813497167E+03
 0.22999999885160E+00 0.00000000000000E+00 0.20779286498182E+00 0.00000000000000E+00 -.41487901605227E+01
 0.99805836099685E-03 0.32493880194785E+00 0.80000000000000E+04 0.30000000000000E+04 0.24620020607092E+02
 0.92325077276596E+01 0.30856058612137E+03 0.30315000000000E+03 0.30929232030836E+03 0.31937359508370E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30815777294361E+03 0.31932005869381E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30929232030836E+03 0.31937359508370E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30815777294361E+03 0.31932005869381E+03 0.30315000000000E+03 0.30315000000000E+03 0.34219803267498E+03
 0.30351594929859E+03 0.91317683157536E+03 0.89862564009298E+03 0.60386477384807E+03 0.15859323643625E+04
 0.97904826664518E+03 0.66572660322546E+03 0.73796427327474E+03 0.65135593553382E+03 0.16793183894275E+04
 0.55495855610137E+03 0.73315204618982E+03 0.54428915943360E+03 0.16750631741200E+04 0.66572660322547E+03
 0.73796427327474E+03 0.65135593553383E+03 0.16793183894275E+04 0.55495855610137E+03 0.73315204618985E+03
 0.54428915943360E+03 0.16750631741200E+04 0.13244672906878E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48285296912411E+03 0.12947350684322E+01
 0.12947350684322E+01 0.22047012863896E+00 0.62707325389827E+00 0.30316299677680E+03 0.32970556227934E+03
 0.31860947366835E+03 0.31806407821509E+03 0.23000000000000E+00 0.00000000000000E+00 0.22416304144047E+00
 0.00000000000000E+00 0.39469409875492E-01 0.99961875088700E-03 0.99861640588114E-01 0.80000000000000E+04
 0.30000000000000E+04 0.80110840888310E+02 0.30041565333116E+02 0.30351529239973E+03 0.34223057252664E+03
 0.30326076602642E+03 0.30501811190584E+03 0.30315000000000E+03 0.30315000000000E+03 0.30325882459287E+03
 0.30501876527790E+03 0.30315000000000E+03 0.30315000000000E+03 0.30326076602642E+03 0.30501811190584E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30325882459287E+03 0.30501876527790E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30458124975524E+03 0.30315000000000E+03 0.21197341729542E+02 0.20752517591335E+02
 0.18244120792675E+02 0.21974153054078E+03 0.20140618914414E+03 0.15476192965033E+02 0.16354273345988E+02
 0.15391633514701E+02 0.17977933948157E+03 0.15203711806018E+02 0.16431844509722E+02 0.15121283506495E+02
 0.17985172959832E+03 0.15476192965033E+02 0.16354273345987E+02 0.15391633514702E+02 0.17977933948157E+03
 0.15203711806018E+02 0.16431844509723E+02 0.15121283506495E+02 0.17985172959832E+03 0.91027239620653E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34766850186449E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44190164865545E-01 0.00000000000000E+00 0.00000000000000E+00 0.44190164865545E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.11497300818951E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11497300818951E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.79245692704196E-01 0.00000000000000E+00
 0.48272266744981E-01 0.12751795944918E+00 0.00000000000000E+00 0.50103662694914E+00 0.17603662396890E+00
 0.32500000298023E+00 0.42250006586313E+00
    110.31271380
 0.11714581725990E+01 0.30397371662957E+03 0.43623816800780E+03 0.38459074383735E+03 0.37288260469187E+03
 0.22999999864946E+00 0.00000000000000E+00 0.20525853301354E+00 0.00000000000000E+00 -.50193814516263E+01
 0.99724076833077E-03 0.35853063419552E+00 0.80000000000000E+04 0.30000000000000E+04 0.22313295537355E+02
 0.83674858265082E+01 0.30957465672161E+03 0.30315000000000E+03 0.31024702100797E+03 0.32163338293482E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30899510378030E+03 0.32157822985420E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31024702100797E+03 0.32163338293482E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30899510378030E+03 0.32157822985420E+03 0.30315000000000E+03 0.30315000000000E+03 0.34690037855267E+03
 0.30361839352978E+03 0.10095746524955E+04 0.99188116187428E+03 0.64646195639908E+03 0.16260508954835E+04
 0.97635662930238E+03 0.71562734126014E+03 0.81478982480537E+03 0.69869836944251E+03 0.17605780767428E+04
 0.60306242241302E+03 0.81042356644889E+03 0.59040173550389E+03 0.17567791217043E+04 0.71562734126014E+03
 0.81478982480537E+03 0.69869836944252E+03 0.17605780767428E+04 0.60306242241302E+03 0.81042356644890E+03
 0.59040173550389E+03 0.17567791217043E+04 0.14251560855183E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49315116517831E+03 0.12947414816298E+01
 0.12947414816298E+01 0.26155345057211E+00 0.58192288456256E+00 0.30317222124637E+03 0.32958147155331E+03
 0.31933604014148E+03 0.31880765478590E+03 0.23000000000000E+00 0.00000000000000E+00 0.22358650965570E+00
 0.00000000000000E+00 0.55000427710171E-02 0.99958800075986E-03 0.11199589551326E+00 0.80000000000000E+04
 0.30000000000000E+04 0.71431189181867E+02 0.26786695943200E+02 0.30361772483988E+03 0.34693280513789E+03
 0.30329193759391E+03 0.30520130337822E+03 0.30315000000000E+03 0.30315000000000E+03 0.30328949072960E+03
 0.30520204250960E+03 0.30315000000000E+03 0.30315000000000E+03 0.30329193759391E+03 0.30520130337822E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30328949072960E+03 0.30520204250960E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30475558060386E+03 0.30315000000000E+03 0.24047525734064E+02 0.23439278808107E+02
 0.21001968889858E+02 0.21857670927417E+03 0.19746973053986E+03 0.17696148097585E+02 0.19027220331606E+02
 0.17585232438122E+02 0.17657217633022E+03 0.17396101953245E+02 0.19104167210843E+02 0.17288062913411E+02
 0.17664338482675E+03 0.17696148097585E+02 0.19027220331607E+02 0.17585232438122E+02 0.17657217633022E+03
 0.17396101953245E+02 0.19104167210843E+02 0.17288062913411E+02 0.17664338482675E+03 0.92543257450841E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34708429635012E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36626603510692E-01 0.00000000000000E+00 0.00000000000000E+00 0.36626603510692E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.97972857372474E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.97972857372474E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.85823868189960E-01 0.00000000000000E+00
 0.17915610425842E-01 0.10373947861580E+00 0.00000000000000E+00 0.47846144228128E+00 0.15346143930105E+00
 0.32500000298023E+00 0.42250006586313E+00
    120.15271340
 0.10729849332035E+01 0.30426288402600E+03 0.44002198896109E+03 0.39146616424758E+03 0.37946502291352E+03
 0.22999999861908E+00 0.00000000000000E+00 0.20267587635222E+00 0.00000000000000E+00 -.56889884890639E+01
 0.99628641963782E-03 0.39265369470914E+00 0.80000000000000E+04 0.30000000000000E+04 0.20374187503637E+02
 0.76403203138639E+01 0.31057793606846E+03 0.30315000000000E+03 0.31117104274813E+03 0.32374294184034E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30981502377352E+03 0.32368709718099E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31117104274813E+03 0.32374294184034E+03 0.30315000000000E+03 0.30315000000000E+03
 0.30981502377352E+03 0.32368709718099E+03 0.30315000000000E+03 0.30315000000000E+03 0.35118952914627E+03
 0.30371371884378E+03 0.10982558032609E+04 0.10775148462911E+04 0.68771517203203E+03 0.16635659490742E+04
 0.97241220118198E+03 0.76200527280091E+03 0.88651576933091E+03 0.74268822659910E+03 0.18326289006449E+04
 0.64810408762522E+03 0.88254800267024E+03 0.63357742157440E+03 0.18292289411588E+04 0.76200527280091E+03
 0.88651576933091E+03 0.74268822659911E+03 0.18326289006449E+04 0.64810408762522E+03 0.88254800267024E+03
 0.63357742157440E+03 0.18292289411588E+04 0.15136486651674E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.50685525796750E+03 0.12947464143325E+01
 0.12947464143325E+01 0.30091344895725E+00 0.55308463337027E+00 0.30318414528314E+03 0.32909312939474E+03
 0.31953988874250E+03 0.31904026714195E+03 0.23000000000000E+00 0.00000000000000E+00 0.22307965193530E+00
 0.00000000000000E+00 -.42455514882839E-02 0.99954859056122E-03 0.12265986921253E+00 0.80000000000000E+04
 0.30000000000000E+04 0.65221005462992E+02 0.24457877048622E+02 0.30371306497797E+03 0.35122375655907E+03
 0.30332148252304E+03 0.30534304216067E+03 0.30315000000000E+03 0.30315000000000E+03 0.30331858956098E+03
 0.30534384377644E+03 0.30315000000000E+03 0.30315000000000E+03 0.30332148252304E+03 0.30534304216067E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30331858956098E+03 0.30534384377644E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30489258211634E+03 0.30315000000000E+03 0.25958707511797E+02 0.25194113687829E+02
 0.22932257586206E+02 0.21338600319211E+03 0.19033908431798E+03 0.19352726160992E+02 0.20863445689167E+02
 0.19219405554665E+02 0.17097731106652E+03 0.19036780511026E+02 0.20937421470301E+02 0.18907022790149E+02
 0.17104518707729E+03 0.19352726160992E+02 0.20863445689168E+02 0.19219405554665E+02 0.17097731106652E+03
 0.19036780511026E+02 0.20937421470303E+02 0.18907022790149E+02 0.17104518707729E+03 0.91756748163397E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34587918664027E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.30166731545031E-01 0.00000000000000E+00 0.00000000000000E+00 0.30166731545031E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.83342742065428E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.83342742065428E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.92262490496460E-01 0.85937324945233E-02
 0.00000000000000E+00 0.92262490496460E-01 0.85937324945233E-02 0.46404231668513E+00 0.13904231370490E+00
 0.32500000298023E+00 0.42250006586313E+00
    130.13322539
 0.99812021378238E+00 0.30461211926994E+03 0.44368681221133E+03 0.39741572479454E+03 0.38517768187316E+03
 0.22999999861487E+00 0.00000000000000E+00 0.19982532322813E+00 0.00000000000000E+00 -.62426221074145E+01
 0.99513874788814E-03 0.43010672757215E+00 0.80000000000000E+04 0.30000000000000E+04 0.18600034566207E+02
 0.69750129623275E+01 0.31161308931580E+03 0.30315000000000E+03 0.31211323179930E+03 0.32585408624753E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31065892400043E+03 0.32579811946395E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31211323179930E+03 0.32585408624753E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31065892400043E+03 0.32579811946395E+03 0.30315000000000E+03 0.30315000000000E+03 0.35539176919152E+03
 0.30380459107200E+03 0.11844863783084E+04 0.11607172663241E+04 0.73125309412721E+03 0.17022867877920E+04
 0.96737742819419E+03 0.80807360949231E+03 0.95942710616713E+03 0.78645872508719E+03 0.19093437327628E+04
 0.69310850690528E+03 0.95583400019231E+03 0.67678736549175E+03 0.19063153566046E+04 0.80807360949231E+03
 0.95942710616713E+03 0.78645872508719E+03 0.19093437327628E+04 0.69310850690528E+03 0.95583400019231E+03
 0.67678736549175E+03 0.19063153566046E+04 0.15993301561906E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.51920804564574E+03 0.12947504927441E+01
 0.12947504927441E+01 0.34083549693518E+00 0.53458149617586E+00 0.30319887674446E+03 0.32831777026281E+03
 0.31936570647660E+03 0.31890206382110E+03 0.23000000000000E+00 0.00000000000000E+00 0.22260983955503E+00
 0.00000000000000E+00 -.19773341655725E-01 0.99949986375380E-03 0.13261348798907E+00 0.80000000000000E+04
 0.30000000000000E+04 0.60325688746377E+02 0.22622133279891E+02 0.30380396380448E+03 0.35542906809636E+03
 0.30334988018536E+03 0.30545674046356E+03 0.30315000000000E+03 0.30315000000000E+03 0.30334658333011E+03
 0.30545758848399E+03 0.30315000000000E+03 0.30315000000000E+03 0.30334988018536E+03 0.30545674046356E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30334658333011E+03 0.30545758848399E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30500374483788E+03 0.30315000000000E+03 0.27112701243359E+02 0.26197955898572E+02
 0.24171210437945E+02 0.20510068641465E+03 0.18080861992451E+03 0.20541025458389E+02 0.22011144225555E+02
 0.20389666179823E+02 0.16339972414581E+03 0.20216769475539E+02 0.22081264126905E+02 0.20069602020243E+02
 0.16346352086932E+03 0.20541025458389E+02 0.22011144225555E+02 0.20389666179823E+02 0.16339972414581E+03
 0.20216769475539E+02 0.22081264126906E+02 0.20069602020243E+02 0.16346352086932E+03 0.89232724460589E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34426134140296E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23003087637710E-01 0.44777207086295E-03 0.00000000000000E+00 0.23003087637710E-01 0.44777207086295E-03
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.70635718070863E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.70635718070863E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.91457838122371E-01 0.24216036799565E-01
 0.00000000000000E+00 0.91457838122371E-01 0.24216036799565E-01 0.45479074808793E+00 0.12979074510770E+00
 0.32500000298023E+00 0.42250006586313E+00
    140.33884857
 0.94752340428948E+00 0.30502440348254E+03 0.44745350439002E+03 0.40246853553610E+03 0.38994452448987E+03
 0.22999999901520E+00 0.00000000000000E+00 0.19657211504133E+00 0.00000000000000E+00 -.66884784012632E+01
 0.99378930197801E-03 0.47251563279905E+00 0.80000000000000E+04 0.30000000000000E+04 0.16930656775545E+02
 0.63489962908292E+01 0.31267976104669E+03 0.30315000000000E+03 0.31307928016629E+03 0.32800815697833E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31153133835952E+03 0.32795252765811E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31307928016629E+03 0.32800815697833E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31153133835952E+03 0.32795252765811E+03 0.30315000000000E+03 0.30315000000000E+03 0.35958547971958E+03
 0.30389205540279E+03 0.12685003627444E+04 0.12417593135080E+04 0.77920528713100E+03 0.17471855784924E+04
 0.96408426492574E+03 0.85455485670730E+03 0.10363714318228E+04 0.83075717709530E+03 0.19942826337162E+04
 0.73871743701285E+03 0.10331328505951E+04 0.72069630223017E+03 0.19916029704690E+04 0.85455485670730E+03
 0.10363714318228E+04 0.83075717709530E+03 0.19942826337162E+04 0.73871743701285E+03 0.10331328505951E+04
 0.72069630223017E+03 0.19916029704691E+04 0.16846400179084E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.53193734154763E+03 0.12947537772231E+01
 0.12947537772231E+01 0.38165798965841E+00 0.52212755191072E+00 0.30321620756887E+03 0.32733639808558E+03
 0.31894052073484E+03 0.31851685486579E+03 0.23000000000000E+00 0.00000000000000E+00 0.22216689906423E+00
 0.00000000000000E+00 -.36298094970886E-01 0.99944255745150E-03 0.14208646846929E+00 0.80000000000000E+04
 0.30000000000000E+04 0.56303742968524E+02 0.21113903613197E+02 0.30389146207798E+03 0.35962622522103E+03
 0.30337679111231E+03 0.30554577384481E+03 0.30315000000000E+03 0.30315000000000E+03 0.30337313396888E+03
 0.30554665388767E+03 0.30315000000000E+03 0.30315000000000E+03 0.30337679111231E+03 0.30554577384481E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30337313396888E+03 0.30554665388767E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30509208970910E+03 0.30315000000000E+03 0.27663370664075E+02 0.26604611281508E+02
 0.24888833397556E+02 0.19470856057049E+03 0.16969528300595E+03 0.21375142744182E+02 0.22629215114140E+02
 0.21210758534246E+02 0.15442373314861E+03 0.21048860559766E+02 0.22694347170144E+02 0.20889224197548E+02
 0.15448244137329E+03 0.21375142744182E+02 0.22629215114140E+02 0.21210758534246E+02 0.15442373314861E+03
 0.21048860559766E+02 0.22694347170145E+02 0.20889224197549E+02 0.15448244137329E+03 0.85522610502979E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34234735755770E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.16650796107591E-01 0.37252886752677E-02 0.00000000000000E+00 0.16650796107591E-01 0.37252886752677E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.59440905656239E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.59440905656239E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.87230064239604E-01 0.33772934990074E-01
 0.00000000000000E+00 0.87230064239604E-01 0.33772934990074E-01 0.44856377595536E+00 0.12356377297513E+00
 0.32500000298023E+00 0.42250006586313E+00
    150.47078132
 0.92405437896181E+00 0.30548018537146E+03 0.45158163467805E+03 0.40657974000991E+03 0.39359855846613E+03
 0.22999999429278E+00 0.00000000000000E+00 0.19286620960888E+00 0.00000000000000E+00 -.70008241637647E+01
 0.99230349150286E-03 0.51992816887843E+00 0.80000000000000E+04 0.30000000000000E+04 0.15386740859333E+02
 0.57700278222500E+01 0.31363247068502E+03 0.30315000000000E+03 0.31390459126688E+03 0.33022789690038E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31232378305969E+03 0.33015679853445E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31390459126688E+03 0.33022789690038E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31232378305969E+03 0.33015679853445E+03 0.30315000000000E+03 0.30315000000000E+03 0.36379374849795E+03
 0.30397547022301E+03 0.12555251358012E+04 0.12264836974608E+04 0.83533488107026E+03 0.18017116268382E+04
 0.96220007136254E+03 0.82341759802510E+03 0.11288815234239E+04 0.79815835188336E+03 0.21020124256199E+04
 0.73481316873038E+03 0.11176522990875E+04 0.71554425404903E+03 0.20914975509988E+04 0.82341759802510E+03
 0.11288815234239E+04 0.79815835188336E+03 0.21020124256199E+04 0.73481316873039E+03 0.11176522990875E+04
 0.71554425404904E+03 0.20914975509989E+04 0.17391544617876E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.14879767951916E+06 0.50050000000000E+08 0.53771022851497E+03 0.11683981578317E+01
 0.11683981578317E+01 0.42218572063720E+00 0.51418879852957E+00 0.30323536156707E+03 0.32623232236882E+03
 0.31834913593917E+03 0.31796617998300E+03 0.23000000000000E+00 0.00000000000000E+00 0.22176279029289E+00
 0.00000000000000E+00 -.45880054799788E-01 0.99937931366971E-03 0.15083108718396E+00 0.80000000000000E+04
 0.30000000000000E+04 0.53039463875527E+02 0.19889798953323E+02 0.30397490004991E+03 0.36383823398978E+03
 0.30340153767013E+03 0.30560737133730E+03 0.30315000000000E+03 0.30315000000000E+03 0.30339757830583E+03
 0.30560826420619E+03 0.30315000000000E+03 0.30315000000000E+03 0.30340153767013E+03 0.30560737133730E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30339757830583E+03 0.30560826420619E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30515565256553E+03 0.30315000000000E+03 0.27687577089267E+02 0.26492436670111E+02
 0.25208483453686E+02 0.18374988326215E+03 0.15841535739120E+03 0.21934893541234E+02 0.22827086493016E+02
 0.21762713715225E+02 0.14522856068358E+03 0.21612890029749E+02 0.22885226257632E+02 0.21445911793972E+02
 0.14528030458543E+03 0.21934893541234E+02 0.22827086493016E+02 0.21762713715225E+02 0.14522856068358E+03
 0.21612890029750E+02 0.22885226257633E+02 0.21445911793972E+02 0.14528030458543E+03 0.81320822983902E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34074161863559E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.13180281961547E-01 0.65968349922407E-02 0.00000000000000E+00 0.13180281961547E-01 0.65968349922407E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.52130445143971E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.52130445143971E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.83680864303250E-01 0.38262914116625E-01
 0.00000000000000E+00 0.83680864303250E-01 0.38262914116625E-01 0.44459439926478E+00 0.11959439628455E+00
 0.32500000298023E+00 0.42250006586313E+00
    160.60271406
 0.91394221247284E+00 0.30595021075517E+03 0.45593530465340E+03 0.41024273513497E+03 0.39669093288620E+03
 0.22999999431162E+00 0.00000000000000E+00 0.18888399978610E+00 0.00000000000000E+00 -.72546497061610E+01
 0.99077655273020E-03 0.56884945301953E+00 0.80000000000000E+04 0.30000000000000E+04 0.14063474892232E+02
 0.52738030845870E+01 0.31425534306037E+03 0.30315000000000E+03 0.31433351352637E+03 0.33256063287300E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31286619723452E+03 0.33243810085428E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31433351352637E+03 0.33256063287300E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31286619723452E+03 0.33243810085428E+03 0.30315000000000E+03 0.30315000000000E+03 0.36807694820121E+03
 0.30406013016246E+03 0.12121153710677E+04 0.11823801656821E+04 0.89470987783580E+03 0.18595092307751E+04
 0.96032580355015E+03 0.77121187590101E+03 0.12246019518494E+04 0.74611273465119E+03 0.22135042613683E+04
 0.71519064705007E+03 0.12054594597051E+04 0.69565500772920E+03 0.21955926394047E+04 0.77121187590101E+03
 0.12246019518494E+04 0.74611273465119E+03 0.22135042613683E+04 0.71519064705007E+03 0.12054594597051E+04
 0.69565500772920E+03 0.21955926394047E+04 0.17832553902673E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.86244956346009E+05 0.50050000000000E+08 0.54193800101246E+03 0.12223283547493E+01
 0.12223283547493E+01 0.46271345161598E+00 0.50813159784919E+00 0.30325643261621E+03 0.32519594463158E+03
 0.31776383843398E+03 0.31741680091177E+03 0.23000000000000E+00 0.00000000000000E+00 0.22135354504201E+00
 0.00000000000000E+00 -.53094940916997E-01 0.99930978290408E-03 0.15963156210555E+00 0.80000000000000E+04
 0.30000000000000E+04 0.50115402583796E+02 0.18793275968923E+02 0.30405956810355E+03 0.36812528104327E+03
 0.30342492014472E+03 0.30565368806171E+03 0.30315000000000E+03 0.30315000000000E+03 0.30342070209384E+03
 0.30565457509303E+03 0.30315000000000E+03 0.30315000000000E+03 0.30342492014472E+03 0.30565368806171E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30342070209384E+03 0.30565457509303E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30520483769343E+03 0.30315000000000E+03 0.27559875140346E+02 0.26225887316981E+02
 0.25502282691529E+02 0.17374263928657E+03 0.14811284518159E+03 0.22439898340147E+02 0.22984638411136E+02
 0.22264289706986E+02 0.13692221243666E+03 0.22123315472757E+02 0.23034778269557E+02 0.21953279348054E+02
 0.13696610996906E+03 0.22439898340147E+02 0.22984638411136E+02 0.22264289706986E+02 0.13692221243666E+03
 0.22123315472757E+02 0.23034778269558E+02 0.21953279348054E+02 0.13696610996906E+03 0.77407687930087E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33924259305106E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10700926146359E-01 0.91984391808559E-02 0.00000000000000E+00 0.10700926146359E-01 0.91984391808559E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.46068408482293E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.46068408482293E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.80326002268137E-01 0.41419244230470E-01
 0.00000000000000E+00 0.80326002268137E-01 0.41419244230470E-01 0.44156579892459E+00 0.11656579594436E+00
 0.32500000298023E+00 0.42250006586313E+00
    170.73464681
 0.91006398639072E+00 0.30642299463919E+03 0.45952567600377E+03 0.41308126382718E+03 0.39904290119996E+03
 0.22999999433681E+00 0.00000000000000E+00 0.18476552402709E+00 0.00000000000000E+00 -.74618913875519E+01
 0.98924584768305E-03 0.61919217766055E+00 0.80000000000000E+04 0.30000000000000E+04 0.12920059859648E+02
 0.48450224473680E+01 0.31490723011587E+03 0.30315000000000E+03 0.31481106199785E+03 0.33488354718384E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31343672258613E+03 0.33472431840820E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31481106199785E+03 0.33488354718384E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31343672258613E+03 0.33472431840820E+03 0.30315000000000E+03 0.30315000000000E+03 0.37225462531109E+03
 0.30414815054712E+03 0.12277433132437E+04 0.11971860972325E+04 0.94344675853577E+03 0.18978474488700E+04
 0.94968345654158E+03 0.77146302975146E+03 0.13032272201983E+04 0.74635882570524E+03 0.23010326879560E+04
 0.72706001703899E+03 0.12830222371242E+04 0.70717448758719E+03 0.22824252087155E+04 0.77146302975146E+03
 0.13032272201983E+04 0.74635882570524E+03 0.23010326879560E+04 0.72706001703899E+03 0.12830222371242E+04
 0.70717448758720E+03 0.22824252087155E+04 0.18344932296396E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.62254686529865E+05 0.50050000000000E+08 0.54555247901539E+03 0.12421291314039E+01
 0.12421291314039E+01 0.50324118259477E+00 0.50440214465296E+00 0.30327922297071E+03 0.32411361834090E+03
 0.31710767586939E+03 0.31679544261989E+03 0.23000000000000E+00 0.00000000000000E+00 0.22097074224966E+00
 0.00000000000000E+00 -.60813760887704E-01 0.99923459064982E-03 0.16791101748750E+00 0.80000000000000E+04
 0.30000000000000E+04 0.47644282785645E+02 0.17866606044617E+02 0.30414758352323E+03 0.37230583675693E+03
 0.30344676056403E+03 0.30568286074863E+03 0.30315000000000E+03 0.30315000000000E+03 0.30344232201464E+03
 0.30568372714260E+03 0.30315000000000E+03 0.30315000000000E+03 0.30344676056403E+03 0.30568286074863E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30344232201464E+03 0.30568372714260E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30523838097621E+03 0.30315000000000E+03 0.27048872624258E+02 0.25568650256692E+02
 0.25499817768773E+02 0.16276811918990E+03 0.13714080233228E+03 0.22726649261491E+02 0.22850821423596E+02
 0.22552067706926E+02 0.12793997734259E+03 0.22417830068255E+02 0.22892718826745E+02 0.22249108811821E+02
 0.12797590883557E+03 0.22726649261491E+02 0.22850821423596E+02 0.22552067706926E+02 0.12793997734259E+03
 0.22417830068255E+02 0.22892718826745E+02 0.22249108811821E+02 0.12797590883557E+03 0.72941877157888E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33710872924027E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.83932405949917E-02 0.12066108930209E-01 0.00000000000000E+00 0.83932405949917E-02 0.12066108930209E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.38697905925686E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.38697905925686E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.75561122056370E-01 0.45155556714122E-01
 0.00000000000000E+00 0.75561122056370E-01 0.45155556714122E-01 0.43970107232648E+00 0.11470106934625E+00
 0.32500000298023E+00 0.42250006586313E+00
    180.86657955
 0.90894773012366E+00 0.30690234466107E+03 0.46206667650376E+03 0.41505458742891E+03 0.40068822543294E+03
 0.22999999436703E+00 0.00000000000000E+00 0.18059472319255E+00 0.00000000000000E+00 -.76070401789636E+01
 0.98769933201922E-03 0.67083994572913E+00 0.80000000000000E+04 0.30000000000000E+04 0.11925348290500E+02
 0.44720056089375E+01 0.31561197167923E+03 0.30315000000001E+03 0.31536793628049E+03 0.33711762706873E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31405165102043E+03 0.33693703285813E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31536793628049E+03 0.33711762706873E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31405165102043E+03 0.33693703285813E+03 0.30315000000000E+03 0.30315000000000E+03 0.37615197418301E+03
 0.30424042573119E+03 0.12632575292330E+04 0.12316517301818E+04 0.97813661843831E+03 0.19126634799773E+04
 0.92963617844685E+03 0.79098952931118E+03 0.13637785929949E+04 0.76560294647582E+03 0.23619514272248E+04
 0.74919300835511E+03 0.13450069314868E+04 0.72882152208818E+03 0.23449873400990E+04 0.79098952931118E+03
 0.13637785929950E+04 0.76560294647583E+03 0.23619514272248E+04 0.74919300835511E+03 0.13450069314868E+04
 0.72882152208818E+03 0.23449873400990E+04 0.18760946263526E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.55349650968272E+05 0.50050000000000E+08 0.54810207168907E+03 0.12476748149035E+01
 0.12476748149035E+01 0.54376891357356E+00 0.50276781702569E+00 0.30330364373178E+03 0.32296488763881E+03
 0.31637486051938E+03 0.31609689526479E+03 0.23000000000000E+00 0.00000000000000E+00 0.22062936881056E+00
 0.00000000000000E+00 -.65509563086745E-01 0.99915406756734E-03 0.17539943633835E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45610180779418E+02 0.17103817792282E+02 0.30423986216630E+03 0.37620422632597E+03
 0.30346661273211E+03 0.30569493052144E+03 0.30315000000000E+03 0.30315000000000E+03 0.30346199289188E+03
 0.30569576406431E+03 0.30315000000000E+03 0.30315000000000E+03 0.30346661273211E+03 0.30569493052144E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30346199289188E+03 0.30569576406431E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30525632860396E+03 0.30315000000000E+03 0.26111906792053E+02 0.24475639741096E+02
 0.25149955006933E+02 0.15116481856445E+03 0.12588911378249E+03 0.22766216145918E+02 0.22377779651334E+02
 0.22597570206183E+02 0.11855020838885E+03 0.22467605536300E+02 0.22411395217915E+02 0.22305013991572E+02
 0.11857821583315E+03 0.22766216145918E+02 0.22377779651334E+02 0.22597570206183E+02 0.11855020838885E+03
 0.22467605536300E+02 0.22411395217915E+02 0.22305013991572E+02 0.11857821583315E+03 0.68060860188631E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33481967236034E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.68854994543378E-02 0.14240811691976E-01 0.00000000000000E+00 0.68854994543378E-02 0.14240811691976E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.32385828415456E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.63367753499375E-07 0.32385828415456E-01 0.63367753499375E-07 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.71015465447966E-01 0.47681442099287E-01
 0.00000000000000E+00 0.71015465447966E-01 0.47681442099287E-01 0.43888390851285E+00 0.11388390553261E+00
 0.32500000298023E+00 0.42250006586313E+00
    190.99851230
 0.90894542041078E+00 0.30739128133996E+03 0.46371440186731E+03 0.41635134036475E+03 0.40180414149028E+03
 0.22999999439791E+00 0.00000000000000E+00 0.17641905544576E+00 0.00000000000000E+00 -.77105301207689E+01
 0.98612728985053E-03 0.72335758636691E+00 0.80000000000000E+04 0.30000000000000E+04 0.11059537012918E+02
 0.41473263798443E+01 0.31634399004536E+03 0.30315000000001E+03 0.31597417184892E+03 0.33922406946151E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31468904173356E+03 0.33903267524476E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31597417184892E+03 0.33922406946151E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31468904173356E+03 0.33903267524476E+03 0.30315000000000E+03 0.30315000000000E+03 0.37969599851108E+03
 0.30433922899128E+03 0.13009306553148E+04 0.12681842093344E+04 0.10002779229842E+04 0.19086608587299E+04
 0.90338154613071E+03 0.81466117015568E+03 0.14085490797857E+04 0.78883910555540E+03 0.24003962631402E+04
 0.77202725025069E+03 0.13918913725168E+04 0.75112218376526E+03 0.23856481638767E+04 0.81466117015568E+03
 0.14085490797857E+04 0.78883910555540E+03 0.24003962631402E+04 0.77202725025069E+03 0.13918913725168E+04
 0.75112218376527E+03 0.23856481638767E+04 0.19045073382891E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.55335363351899E+05 0.50050000000000E+08 0.54973797930668E+03 0.12475818446688E+01
 0.12475818446688E+01 0.58429664455234E+00 0.50240506193340E+00 0.30332982130624E+03 0.32181187935771E+03
 0.31562155967771E+03 0.31537572821194E+03 0.23000000000000E+00 0.00000000000000E+00 0.22032028871873E+00
 0.00000000000000E+00 -.66062578906149E-01 0.99906781189996E-03 0.18225731339337E+00 0.80000000000000E+04
 0.30000000000000E+04 0.43893986205829E+02 0.16460244827186E+02 0.30433868205165E+03 0.37974785253162E+03
 0.30348445253104E+03 0.30569320621438E+03 0.30315000000000E+03 0.30315000000000E+03 0.30347968626017E+03
 0.30569399633552E+03 0.30315000000000E+03 0.30315000000000E+03 0.30348445253104E+03 0.30569320621438E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30347968626017E+03 0.30569399633552E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30526158996802E+03 0.30315000000001E+03 0.24879925022239E+02 0.23072413893077E+02
 0.24598563511427E+02 0.14007726770807E+03 0.11535571137909E+03 0.22647649199459E+02 0.21709229387394E+02
 0.22489879070573E+02 0.10962504702679E+03 0.22360139457278E+02 0.21734557557559E+02 0.22208523540533E+02
 0.10964516988171E+03 0.22647649199460E+02 0.21709229387394E+02 0.22489879070574E+02 0.10962504702679E+03
 0.22360139457278E+02 0.21734557557559E+02 0.22208523540533E+02 0.10964516988171E+03 0.63278305402847E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33291837922989E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.57763124273024E-02 0.16015612585988E-01 0.00000000000000E+00 0.57763124273024E-02 0.16015612585988E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28438326628721E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.65447807403370E-04 0.28438326628721E-01 0.65447807403370E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.67727236353646E-01 0.48239966915407E-01
 0.00000000000000E+00 0.67727236353646E-01 0.48239966915407E-01 0.43870253096670E+00 0.11370252798647E+00
 0.32500000298023E+00 0.42250006586313E+00
    201.31087586
 0.90939603569151E+00 0.30789919460120E+03 0.46474032845355E+03 0.41719676000065E+03 0.40257725772789E+03
 0.22999999444269E+00 0.00000000000000E+00 0.17219139057752E+00 0.00000000000000E+00 -.77840530184742E+01
 0.98449985107793E-03 0.77723401193734E+00 0.80000000000000E+04 0.30000000000000E+04 0.10292910342484E+02
 0.38598413784314E+01 0.31709374979188E+03 0.30315000000002E+03 0.31661397708509E+03 0.34122440390693E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31534129498141E+03 0.34102892944399E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31661397708509E+03 0.34122440390693E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31534129498141E+03 0.34102892944399E+03 0.30315000000000E+03 0.30315000000000E+03 0.38293955610832E+03
 0.30445059583880E+03 0.13356600092960E+04 0.13017725169607E+04 0.10129605096716E+04 0.18917106216891E+04
 0.87368530946918E+03 0.83766100094366E+03 0.14414247826314E+04 0.81134697260698E+03 0.24224979073124E+04
 0.79291208544319E+03 0.14269533421865E+04 0.77148807362683E+03 0.24099695892527E+04 0.83766100094366E+03
 0.14414247826314E+04 0.81134697260699E+03 0.24224979073124E+04 0.79291208544319E+03 0.14269533421865E+04
 0.77148807362684E+03 0.24099695892527E+04 0.19219047009893E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.58122818747787E+05 0.50050000000000E+08 0.55074440848942E+03 0.12452107501098E+01
 0.12452107501098E+01 0.62554609879685E+00 0.50234634179094E+00 0.30335864377927E+03 0.32071884298046E+03
 0.31490495460614E+03 0.31468783000644E+03 0.23000000000000E+00 0.00000000000000E+00 0.22001473050360E+00
 0.00000000000000E+00 -.64193498670307E-01 0.99897288563512E-03 0.18902315361409E+00 0.80000000000000E+04
 0.30000000000000E+04 0.42322857528516E+02 0.15871071573194E+02 0.30445007099932E+03 0.38299033323071E+03
 0.30350096896020E+03 0.30568325323611E+03 0.30315000000000E+03 0.30315000000000E+03 0.30349607786095E+03
 0.30568398999983E+03 0.30315000000000E+03 0.30315000000000E+03 0.30350096896020E+03 0.30568325323611E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30349607786095E+03 0.30568398999983E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30525917547913E+03 0.30315000000001E+03 0.23509704676573E+02 0.21502545127679E+02
 0.24038612250868E+02 0.13015796630417E+03 0.10599916099205E+03 0.22496133422841E+02 0.21032926821339E+02
 0.22354144476047E+02 0.10164831027387E+03 0.22218574549295E+02 0.21049869116295E+02 0.22082754268682E+02
 0.10166048554428E+03 0.22496133422842E+02 0.21032926821340E+02 0.22354144476049E+02 0.10164831027387E+03
 0.22218574549295E+02 0.21049869116295E+02 0.22082754268682E+02 0.10166048554428E+03 0.58916454189039E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33149009620285E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.50040573937466E-02 0.17356783799094E-01 0.00000000000000E+00 0.50040573937466E-02 0.17356783799094E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26339995705353E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20798089534960E-03 0.26339995705353E-01 0.20798089534960E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.65524622896101E-01 0.47476661179702E-01
 0.00000000000000E+00 0.65524622896101E-01 0.47476661179702E-01 0.43867317089547E+00 0.11367316791524E+00
 0.32500000298023E+00 0.42250006586313E+00
    210.02479937
 0.90983013128680E+00 0.30833622606378E+03 0.46528147647130E+03 0.41768363721026E+03 0.40306098372917E+03
 0.22999999447448E+00 0.00000000000000E+00 0.16864711034814E+00 0.00000000000000E+00 -.78261159210383E+01
 0.98310402664024E-03 0.82279969019195E+00 0.80000000000000E+04 0.30000000000000E+04 0.97229010843863E+01
 0.36460879066449E+01 0.31772029871843E+03 0.30315000000004E+03 0.31715848715479E+03 0.34280793159564E+03
 0.30315000000000E+03 0.30315000000000E+03 0.31588650962839E+03 0.34261246624903E+03 0.30315000000000E+03
 0.30315000000000E+03 0.31715848715479E+03 0.34280793159564E+03 0.30315000000000E+03 0.30315000000000E+03
 0.31588650962839E+03 0.34261246624903E+03 0.30315000000000E+03 0.30315000000000E+03 0.38541872806536E+03
 0.30455584497935E+03 0.13603230442605E+04 0.13255392042865E+04 0.10183484670821E+04 0.18716807006255E+04
 0.84824049120802E+03 0.85433129561659E+03 0.14619710894435E+04 0.82761663541320E+03 0.24321439607573E+04
 0.80777520635465E+03 0.14491578631186E+04 0.78596329941600E+03 0.24212673984884E+04 0.85433129561659E+03
 0.14619710894435E+04 0.82761663541320E+03 0.24321439607573E+04 0.80777520635465E+03 0.14491578631186E+04
 0.78596329941600E+03 0.24212673984884E+04 0.19297991863880E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60808085227302E+05 0.50050000000000E+08 0.55127146128958E+03 0.12429554727659E+01
 0.12429554727659E+01 0.66040179284441E+00 0.50213168064036E+00 0.30338512059712E+03 0.31988905764413E+03
 0.31436429121307E+03 0.31416793183512E+03 0.23000000000000E+00 0.00000000000000E+00 0.21975066380225E+00
 0.00000000000000E+00 -.61942344005886E-01 0.99888570849000E-03 0.19480934547818E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41065791686550E+02 0.15399671882456E+02 0.30455533819427E+03 0.38546850699624E+03
 0.30351406937351E+03 0.30567091998127E+03 0.30315000000000E+03 0.30315000000000E+03 0.30350908396418E+03
 0.30567160410446E+03 0.30315000000000E+03 0.30315000000000E+03 0.30351406937351E+03 0.30567091998127E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30350908396418E+03 0.30567160410446E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30525330020247E+03 0.30315000000001E+03 0.22337562128257E+02 0.20135157008914E+02
 0.23666522731695E+02 0.12299455418225E+03 0.99209698836896E+02 0.22412628716655E+02 0.20558458331582E+02
 0.22287360070875E+02 0.95884582434127E+02 0.22141709587230E+02 0.20568119896664E+02 0.22022555822735E+02
 0.95889874701924E+02 0.22412628716655E+02 0.20558458331582E+02 0.22287360070875E+02 0.95884582434127E+02
 0.22141709587231E+02 0.20568119896665E+02 0.22022555822736E+02 0.95889874701926E+02 0.55715191175710E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33058798347732E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46549559812023E-02 0.18000964523702E-01 0.00000000000000E+00 0.46549559812023E-02 0.18000964523702E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25439035017416E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.31111960763363E-03 0.25439035017416E-01 0.31111960763363E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.64271029815278E-01 0.46338780988577E-01
 0.00000000000000E+00 0.64271029815278E-01 0.46338780988577E-01 0.43856584032018E+00 0.11356583733995E+00
 0.32500000298023E+00 0.42250006586313E+00
    220.50077899
 0.91023838850655E+00 0.30887042316401E+03 0.46571916840255E+03 0.41812925203415E+03 0.40354244404519E+03
 0.22999999451879E+00 0.00000000000000E+00 0.16442354201663E+00 0.00000000000000E+00 -.78619562361999E+01
 0.98140338299110E-03 0.87737169486171E+00 0.80000000000000E+04 0.30000000000000E+04 0.91181423413265E+01
 0.34193033779974E+01 0.31845481828685E+03 0.30315000000009E+03 0.31780407823104E+03 0.34459612482601E+03
 0.30315000000000E+03 0.30315000000001E+03 0.31652642686380E+03 0.34440304879998E+03 0.30315000000000E+03
 0.30315000000001E+03 0.31780407823104E+03 0.34459612482601E+03 0.30315000000000E+03 0.30315000000001E+03
 0.31652642686380E+03 0.34440304879998E+03 0.30315000000000E+03 0.30315000000001E+03 0.38813247400596E+03
 0.30469887362273E+03 0.13841088980561E+04 0.13483786784488E+04 0.10208463592474E+04 0.18447164527567E+04
 0.81876586171311E+03 0.87045196214866E+03 0.14808654540463E+04 0.84332513825203E+03 0.24372438241386E+04
 0.82223976491986E+03 0.14697481877334E+04 0.80005209122458E+03 0.24280320100791E+04 0.87045196214866E+03
 0.14808654540462E+04 0.84332513825204E+03 0.24372438241386E+04 0.82223976491986E+03 0.14697481877333E+04
 0.80005209122458E+03 0.24280320100791E+04 0.19338287880432E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.63333518429451E+05 0.50050000000000E+08 0.55169867701664E+03 0.12408481743511E+01
 0.12408481743511E+01 0.70230571133526E+00 0.50146273315663E+00 0.30342013954000E+03 0.31901626077766E+03
 0.31380234505600E+03 0.31362695305489E+03 0.23000000000000E+00 0.00000000000000E+00 0.21941591809237E+00
 0.00000000000000E+00 -.59235606710155E-01 0.99877042975194E-03 0.20204032643016E+00 0.80000000000000E+04
 0.30000000000000E+04 0.39596055606085E+02 0.14848520852282E+02 0.30469838969551E+03 0.38818115209268E+03
 0.30352925581509E+03 0.30565442510744E+03 0.30315000000000E+03 0.30315000000000E+03 0.30352416114042E+03
 0.30565503718662E+03 0.30315000000000E+03 0.30315000000000E+03 0.30352925581509E+03 0.30565442510744E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30352416114042E+03 0.30565503718662E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30524432183842E+03 0.30315000000002E+03 0.20946639777833E+02 0.18469410697946E+02
 0.23386069518965E+02 0.11580141664424E+03 0.92298416777676E+02 0.22401834262551E+02 0.20148488130006E+02
 0.22300462029778E+02 0.90091345522791E+02 0.22136123423796E+02 0.20149202418708E+02 0.22040713578490E+02
 0.90088202606074E+02 0.22401834262551E+02 0.20148488130006E+02 0.22300462029778E+02 0.90091345522791E+02
 0.22136123423797E+02 0.20149202418708E+02 0.22040713578491E+02 0.90088202606074E+02 0.52442810476036E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32977551477826E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44845591682427E-02 0.18331748771160E-01 0.00000000000000E+00 0.44845591682427E-02 0.18331748771160E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24953327262844E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.38003996388005E-03 0.24953327262844E-01 0.38003996388005E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.63256676210500E-01 0.44776959673888E-01
 0.00000000000000E+00 0.63256676210500E-01 0.44776959673888E-01 0.43823136657832E+00 0.11323136359808E+00
 0.32500000298023E+00 0.42250006586313E+00
    231.07311566
 0.91048023361186E+00 0.30941763293514E+03 0.46606164523402E+03 0.41852121959676E+03 0.40399057426216E+03
 0.22999999456203E+00 0.00000000000000E+00 0.16020029527266E+00 0.00000000000000E+00 -.78899609226462E+01
 0.97966748554557E-03 0.93204475545085E+00 0.80000000000000E+04 0.30000000000000E+04 0.85832788106084E+01
 0.32187295539782E+01 0.31917048339222E+03 0.30315000000020E+03 0.31843795571141E+03 0.34629103123838E+03
 0.30315000000001E+03 0.30315000000002E+03 0.31715113242064E+03 0.34610187084549E+03 0.30315000000000E+03
 0.30315000000002E+03 0.31843795571141E+03 0.34629103123838E+03 0.30315000000001E+03 0.30315000000002E+03
 0.31715113242064E+03 0.34610187084549E+03 0.30315000000000E+03 0.30315000000002E+03 0.39062923343638E+03
 0.30486430164196E+03 0.14027692961251E+04 0.13662478191302E+04 0.10209531272980E+04 0.18172259340642E+04
 0.79116804112976E+03 0.88297497610586E+03 0.14959217814640E+04 0.85553342131721E+03 0.24386429400084E+04
 0.83375942473990E+03 0.14862185465375E+04 0.81130207111278E+03 0.24307993259902E+04 0.88297497610586E+03
 0.14959217814640E+04 0.85553342131722E+03 0.24386429400084E+04 0.83375942473990E+03 0.14862185465375E+04
 0.81130207111278E+03 0.24307993259902E+04 0.19344085873616E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64829545030668E+05 0.50050000000000E+08 0.55203699059064E+03 0.12396131954111E+01
 0.12396131954111E+01 0.74459505799501E+00 0.50028062929093E+00 0.30345977829776E+03 0.31826931390748E+03
 0.31333003137792E+03 0.31317195058328E+03 0.23000000000000E+00 0.00000000000000E+00 0.21905212323392E+00
 0.00000000000000E+00 -.56886013333248E-01 0.99863997188496E-03 0.20977537524886E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38136029982115E+02 0.14301011243293E+02 0.30486384333277E+03 0.39067699510054E+03
 0.30354439695047E+03 0.30563808267606E+03 0.30315000000000E+03 0.30315000000000E+03 0.30353918821295E+03
 0.30563861228140E+03 0.30315000000000E+03 0.30315000000000E+03 0.30354439695047E+03 0.30563808267606E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30353918821295E+03 0.30563861228140E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30523505388396E+03 0.30315000000005E+03 0.19571623829078E+02 0.16764539952369E+02
 0.23312368061848E+02 0.10996914258650E+03 0.86540212684345E+02 0.22519644087576E+02 0.19930870073193E+02
 0.22446155949391E+02 0.85385728553702E+02 0.22256092285321E+02 0.19922130964282E+02 0.22188280579346E+02
 0.85373689932846E+02 0.22519644087576E+02 0.19930870073193E+02 0.22446155949391E+02 0.85385728553702E+02
 0.22256092285322E+02 0.19922130964282E+02 0.22188280579347E+02 0.85373689932846E+02 0.49723028953893E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32917980966891E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44752557729043E-02 0.18364113552004E-01 0.00000000000000E+00 0.44752557729043E-02 0.18364113552004E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24840990882428E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.39659932947720E-03 0.24840990882428E-01 0.39659932947720E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.62622137589933E-01 0.43205709935359E-01
 0.00000000000000E+00 0.62622137589933E-01 0.43205709935359E-01 0.43764031464547E+00 0.11264031166523E+00
 0.32500000298023E+00 0.42250006586313E+00
    241.64545232
 0.91058148033714E+00 0.30997108989963E+03 0.46639149248353E+03 0.41891365190360E+03 0.40444345168715E+03
 0.22999999460387E+00 0.00000000000000E+00 0.15601175774655E+00 0.00000000000000E+00 -.79158554612795E+01
 0.97791802807611E-03 0.98621918128092E+00 0.80000000000000E+04 0.30000000000000E+04 0.81117870670589E+01
 0.30419201501471E+01 0.31986079879311E+03 0.30315000000039E+03 0.31905237592146E+03 0.34789541489215E+03
 0.30315000000001E+03 0.30315000000003E+03 0.31775512446365E+03 0.34771103442933E+03 0.30315000000001E+03
 0.30315000000003E+03 0.31905237592146E+03 0.34789541489215E+03 0.30315000000001E+03 0.30315000000003E+03
 0.31775512446365E+03 0.34771103442933E+03 0.30315000000001E+03 0.30315000000003E+03 0.39293240670567E+03
 0.30505256827606E+03 0.14177079345834E+04 0.13805506383915E+04 0.10199795855722E+04 0.17911617096965E+04
 0.76608222619648E+03 0.89282853437768E+03 0.15086473157998E+04 0.86517383990023E+03 0.24385450142487E+04
 0.84313798552061E+03 0.15001244743463E+04 0.82051267947143E+03 0.24318281734650E+04 0.89282853437769E+03
 0.15086473157998E+04 0.86517383990023E+03 0.24385450142487E+04 0.84313798552061E+03 0.15001244743464E+04
 0.82051267947142E+03 0.24318281734650E+04 0.19334252881104E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65455845884570E+05 0.50050000000000E+08 0.55236681335913E+03 0.12391105792901E+01
 0.12391105792901E+01 0.78688440465476E+00 0.49857331463968E+00 0.30350474573956E+03 0.31764449670112E+03
 0.31294469503106E+03 0.31280074553448E+03 0.23000000000000E+00 0.00000000000000E+00 0.21865869990627E+00
 0.00000000000000E+00 -.55160197881640E-01 0.99849201218342E-03 0.21802331709674E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36693323019438E+02 0.13759996132289E+02 0.30505214377855E+03 0.39297932142453E+03
 0.30355975895588E+03 0.30562328331974E+03 0.30315000000000E+03 0.30315000000000E+03 0.30355442576584E+03
 0.30562371984806E+03 0.30315000000000E+03 0.30315000000000E+03 0.30355975895588E+03 0.30562328331974E+03
 0.30315000000000E+03 0.30315000000000E+03 0.30355442576584E+03 0.30562371984806E+03 0.30315000000000E+03
 0.30315000000000E+03 0.30522671321566E+03 0.30315000000010E+03 0.18216494174360E+02 0.15021579141742E+02
 0.23443528879588E+02 0.10537325266534E+03 0.81812506141351E+02 0.22771292184162E+02 0.19903852130276E+02
 0.22728613900441E+02 0.81668698708643E+02 0.22506840595305E+02 0.19885221714176E+02 0.22469312310561E+02
 0.81647375105465E+02 0.22771292184162E+02 0.19903852130276E+02 0.22728613900441E+02 0.81668698708642E+02
 0.22506840595305E+02 0.19885221714175E+02 0.22469312310562E+02 0.81647375105463E+02 0.47506127766425E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32874167480416E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45305689294641E-02 0.18276753549852E-01 0.00000000000000E+00 0.45305689294641E-02 0.18276753549852E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24904545165330E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.38520640031163E-03 0.24904545165330E-01 0.38520640031163E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.31574518714509E-03 0.62240131328056E-01 0.41481998614073E-01
 0.00000000000000E+00 0.62240131328056E-01 0.41797743801218E-01 0.43678665731984E+00 0.11178665433961E+00
 0.32500000298023E+00 0.42250006586313E+00
    250.17772901
 0.91059987313334E+00 0.31042085024810E+03 0.46667509146405E+03 0.41924672738812E+03 0.40482324086006E+03
 0.22999999463918E+00 0.00000000000000E+00 0.15265375726894E+00 0.00000000000000E+00 -.79376660255189E+01
 0.97650093902863E-03 0.10295562958622E+01 0.80000000000000E+04 0.30000000000000E+04 0.77703376028607E+01
 0.29138766010728E+01 0.32039914984659E+03 0.30315000000067E+03 0.31953304150859E+03 0.34913413129372E+03
 0.30315000000002E+03 0.30315000000006E+03 0.31822707130631E+03 0.34895384231725E+03 0.30315000000002E+03
 0.30315000000006E+03 0.31953304150859E+03 0.34913413129371E+03 0.30315000000002E+03 0.30315000000006E+03
 0.31822707130631E+03 0.34895384231725E+03 0.30315000000002E+03 0.30315000000006E+03 0.39468565704444E+03
 0.30522353354336E+03 0.14280714918236E+04 0.13905094996116E+04 0.10187397862099E+04 0.17713322389862E+04
 0.74749875384522E+03 0.89956356615966E+03 0.15178623250513E+04 0.87180939506760E+03 0.24381497821877E+04
 0.84972593423251E+03 0.15101652683467E+04 0.82703546624084E+03 0.24322137087147E+04 0.89956356615967E+03
 0.15178623250514E+04 0.87180939506761E+03 0.24381497821877E+04 0.84972593423251E+03 0.15101652683467E+04
 0.82703546624082E+03 0.24322137087147E+04 0.19322160976909E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65569621660931E+05 0.50050000000000E+08 0.55265190275505E+03 0.12390327042613E+01
 0.12390327042613E+01 0.82101351141770E+00 0.49683359958824E+00 0.30354562597205E+03 0.31721763818336E+03
 0.31268916148966E+03 0.31255475782790E+03 0.23000000000000E+00 0.00000000000000E+00 0.21831902638223E+00
 0.00000000000000E+00 -.54303324955956E-01 0.99835753458513E-03 0.22506948988056E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35544577829031E+02 0.13329216685887E+02 0.30522314205651E+03 0.39473188880422E+03
 0.30357244969082E+03 0.30561373006863E+03 0.30315000000000E+03 0.30315000000001E+03 0.30356700451807E+03
 0.30561408521114E+03 0.30315000000000E+03 0.30315000000001E+03 0.30357244969082E+03 0.30561373006863E+03
 0.30315000000000E+03 0.30315000000001E+03 0.30356700451807E+03 0.30561408521114E+03 0.30315000000000E+03
 0.30315000000001E+03 0.30522176436319E+03 0.30315000000016E+03 0.17114021760026E+02 0.13556584490780E+02
 0.23678889586086E+02 0.10240723968695E+03 0.78609955652934E+02 0.23065634271708E+02 0.20002423812275E+02
 0.23048333280519E+02 0.79262184365930E+02 0.22798079512410E+02 0.19975602508491E+02 0.22785060940523E+02
 0.79233187188942E+02 0.23065634271708E+02 0.20002423812275E+02 0.23048333280519E+02 0.79262184365930E+02
 0.22798079512411E+02 0.19975602508490E+02 0.22785060940524E+02 0.79233187188939E+02 0.46011395793097E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32846350727144E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45726496014750E-02 0.18212036501972E-01 0.00000000000000E+00 0.45726496014750E-02 0.18212036501972E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24985169048933E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.37157642557554E-03 0.24985169048933E-01 0.37157642557554E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.54561720250492E-03 0.62035566215839E-01 0.40301622815325E-01
 0.00000000000000E+00 0.62035566215839E-01 0.40847240017830E-01 0.43591679979412E+00 0.11091679681389E+00
 0.32500000298023E+00 0.42250006586313E+00
    263.16216244
 0.91057736927328E+00 0.31110787799344E+03 0.46714926452576E+03 0.41978667944364E+03 0.40542759128852E+03
 0.22999999464423E+00 0.00000000000000E+00 0.14757815287507E+00 0.00000000000000E+00 -.79719873854535E+01
 0.97434417623760E-03 0.10948534866438E+01 0.80000000000000E+04 0.30000000000000E+04 0.73069137538424E+01
 0.27400926576909E+01 0.32119302510618E+03 0.30315000000145E+03 0.32024427157696E+03 0.35093836672466E+03
 0.30315000000005E+03 0.30315000000015E+03 0.31892462935294E+03 0.35076477914877E+03 0.30315000000004E+03
 0.30315000000014E+03 0.32024427157696E+03 0.35093836672466E+03 0.30315000000005E+03 0.30315000000015E+03
 0.31892462935294E+03 0.35076477914877E+03 0.30315000000004E+03 0.30315000000014E+03 0.39718891878895E+03
 0.30551679545511E+03 0.14421426522431E+04 0.14041042050234E+04 0.10166150718479E+04 0.17436597135786E+04
 0.72196156637153E+03 0.90861706659868E+03 0.15306109913648E+04 0.88080210502968E+03 0.24374912009267E+04
 0.85875165354864E+03 0.15240045523595E+04 0.83604997029534E+03 0.24325734933937E+04 0.90861706659868E+03
 0.15306109913648E+04 0.88080210502969E+03 0.24374912009267E+04 0.85875165354863E+03 0.15240045523596E+04
 0.83604997029533E+03 0.24325734933937E+04 0.19303788526347E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65430415317961E+05 0.50050000000000E+08 0.55312917175763E+03 0.12391724338675E+01
 0.12391724338675E+01 0.87295124514997E+00 0.49373883952194E+00 0.30361703400725E+03 0.31668014114899E+03
 0.31238029890852E+03 0.31225792608814E+03 0.23000000000000E+00 0.00000000000000E+00 0.21776372689123E+00
 0.00000000000000E+00 -.53695283836207E-01 0.99812271654297E-03 0.23647620968020E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33830041553943E+02 0.12686265582729E+02 0.30551646381792E+03 0.39723398902868E+03
 0.30359262534541E+03 0.30560288467250E+03 0.30315000000000E+03 0.30315000000002E+03 0.30358697779396E+03
 0.30560310205069E+03 0.30315000000000E+03 0.30315000000002E+03 0.30359262534541E+03 0.30560288467251E+03
 0.30315000000000E+03 0.30315000000002E+03 0.30358697779396E+03 0.30560310205069E+03 0.30315000000000E+03
 0.30315000000002E+03 0.30521702310373E+03 0.30315000000036E+03 0.15406734373285E+02 0.11209628754342E+02
 0.24263576896572E+02 0.99001746965725E+02 0.74616852184671E+02 0.23690965385152E+02 0.20352376705340E+02
 0.23706357748399E+02 0.76480231377589E+02 0.23415625982866E+02 0.20312180582789E+02 0.23435610448734E+02
 0.76438715414484E+02 0.23690965385152E+02 0.20352376705340E+02 0.23706357748398E+02 0.76480231377588E+02
 0.23415625982867E+02 0.20312180582788E+02 0.23435610448735E+02 0.76438715414483E+02 0.44169461012842E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32815938698607E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46462325379999E-02 0.18098532048839E-01 0.00000000000000E+00 0.46462325379999E-02 0.18098532048839E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25179672358770E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.34088333285620E-03 0.25179672358770E-01 0.34088333285620E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82210460822924E-03 0.61916678204847E-01 0.38799046660161E-01
 0.00000000000000E+00 0.61916678204847E-01 0.39621151268390E-01 0.43436941976097E+00 0.10936941678074E+00
 0.32500000298023E+00 0.42250006586313E+00
    272.90048752
 0.91054517981979E+00 0.31162387677004E+03 0.46754330016484E+03 0.42021940702737E+03 0.40590210601606E+03
 0.22999999465923E+00 0.00000000000000E+00 0.14379701777178E+00 0.00000000000000E+00 -.79997607735494E+01
 0.97273055316211E-03 0.11433098915240E+01 0.80000000000000E+04 0.30000000000000E+04 0.69972280125521E+01
 0.26239605047071E+01 0.32177228096557E+03 0.30315000000251E+03 0.32076491477681E+03 0.35223810008288E+03
 0.30315000000010E+03 0.30315000000027E+03 0.31943479431835E+03 0.35206976554249E+03 0.30315000000008E+03
 0.30315000000026E+03 0.32076491477681E+03 0.35223810008288E+03 0.30315000000010E+03 0.30315000000027E+03
 0.31943479431836E+03 0.35206976554249E+03 0.30315000000008E+03 0.30315000000026E+03 0.39895765462927E+03
 0.30576209235846E+03 0.14519346757285E+04 0.14136232802971E+04 0.10149872179478E+04 0.17247920311768E+04
 0.70472987713924E+03 0.91488577006295E+03 0.15394621608398E+04 0.88707875590487E+03 0.24371815521410E+04
 0.86506432636009E+03 0.15335676327715E+04 0.84240609657881E+03 0.24329200204645E+04 0.91488577006296E+03
 0.15394621608398E+04 0.88707875590488E+03 0.24371815521410E+04 0.86506432636009E+03 0.15335676327715E+04
 0.84240609657880E+03 0.24329200204646E+04 0.19292744199690E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65231294987424E+05 0.50050000000000E+08 0.55352547151454E+03 0.12393527256225E+01
 0.12393527256225E+01 0.91190454544918E+00 0.49117135158660E+00 0.30367916169553E+03 0.31635364281189E+03
 0.31220341479817E+03 0.31208848881902E+03 0.23000000000000E+00 0.00000000000000E+00 0.21731670099166E+00
 0.00000000000000E+00 -.53815758363594E-01 0.99791850250403E-03 0.24558013945500E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32575924167785E+02 0.12215971562919E+02 0.30576181974696E+03 0.39900161043547E+03
 0.30360872085501E+03 0.30559772948872E+03 0.30315000000000E+03 0.30315000000003E+03 0.30360292165549E+03
 0.30559783179174E+03 0.30315000000000E+03 0.30315000000003E+03 0.30360872085501E+03 0.30559772948872E+03
 0.30315000000000E+03 0.30315000000003E+03 0.30360292165549E+03 0.30559783179174E+03 0.30315000000000E+03
 0.30315000000003E+03 0.30521579240463E+03 0.30315000000062E+03 0.14090412930079E+02 0.93461777528845E+01
 0.24852148692698E+02 0.97177192012252E+02 0.72200782576091E+02 0.24278947288298E+02 0.20749988560997E+02
 0.24337432234511E+02 0.74976540776729E+02 0.23995179204603E+02 0.20699310728997E+02 0.24059814189296E+02
 0.74925245371007E+02 0.24278947288299E+02 0.20749988560997E+02 0.24337432234511E+02 0.74976540776728E+02
 0.23995179204603E+02 0.20699310728996E+02 0.24059814189296E+02 0.74925245371007E+02 0.43069026595062E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32799469135115E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46778867627315E-02 0.18058171504097E-01 0.00000000000000E+00 0.46778867627315E-02 0.18058171504097E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25317389518996E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.32007752672652E-03 0.25317389518996E-01 0.32007752672652E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98804255163111E-03 0.61909776280817E-01 0.37920252112699E-01
 0.00000000000000E+00 0.61909776280817E-01 0.38908294664330E-01 0.43308567579330E+00 0.10808567281307E+00
 0.32500000298023E+00 0.42250006586313E+00
    282.63881259
 0.91051537571925E+00 0.31213935247681E+03 0.46796445939967E+03 0.42067074080755E+03 0.40639032714705E+03
 0.22999999469738E+00 0.00000000000000E+00 0.14003707166512E+00 0.00000000000000E+00 -.80289463136978E+01
 0.97112387887500E-03 0.11913374711918E+01 0.80000000000000E+04 0.30000000000000E+04 0.67151417574372E+01
 0.25181781590389E+01 0.32233960359408E+03 0.30315000000420E+03 0.32127620947139E+03 0.35349826147958E+03
 0.30315000000018E+03 0.30315000000047E+03 0.31993532197382E+03 0.35333524897395E+03 0.30315000000014E+03
 0.30315000000047E+03 0.32127620947139E+03 0.35349826147958E+03 0.30315000000018E+03 0.30315000000047E+03
 0.31993532197382E+03 0.35333524897395E+03 0.30315000000014E+03 0.30315000000047E+03 0.40064892051621E+03
 0.30602947703152E+03 0.14613622956710E+04 0.14228350890623E+04 0.10132837604592E+04 0.17072087303699E+04
 0.68885855110841E+03 0.92092579976737E+03 0.15477771391850E+04 0.89316159580915E+03 0.24369853689078E+04
 0.87114953401154E+03 0.15425200842132E+04 0.84856901263726E+03 0.24333054190759E+04 0.92092579976737E+03
 0.15477771391850E+04 0.89316159580916E+03 0.24369853689078E+04 0.87114953401154E+03 0.15425200842132E+04
 0.84856901263725E+03 0.24333054190760E+04 0.19284033669499E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65046930188200E+05 0.50050000000000E+08 0.55394850471874E+03 0.12395176314574E+01
 0.12395176314574E+01 0.95085784574838E+00 0.48847950995762E+00 0.30374949344997E+03 0.31608165485318E+03
 0.31206564941388E+03 0.31195713261227E+03 0.23000000000000E+00 0.00000000000000E+00 0.21684413585162E+00
 0.00000000000000E+00 -.54361551612186E-01 0.99768742079640E-03 0.25514653713853E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31354530967656E+02 0.11757949112871E+02 0.30602927083117E+03 0.40069161072418E+03
 0.30362579885845E+03 0.30559542787084E+03 0.30315000000000E+03 0.30315000000006E+03 0.30361982460501E+03
 0.30559540557324E+03 0.30315000000000E+03 0.30315000000006E+03 0.30362579885845E+03 0.30559542787084E+03
 0.30315000000000E+03 0.30315000000006E+03 0.30361982460501E+03 0.30559540557324E+03 0.30315000000000E+03
 0.30315000000006E+03 0.30521683789417E+03 0.30315000000103E+03 0.12728307497386E+02 0.73776098328091E+01
 0.25551914896206E+02 0.95866719602542E+02 0.70187045131855E+02 0.24962610540106E+02 0.21246497091100E+02
 0.25082021269574E+02 0.73882776075705E+02 0.24668480544363E+02 0.21184980489752E+02 0.24795254537650E+02
 0.73821393405918E+02 0.24962610540106E+02 0.21246497091100E+02 0.25082021269574E+02 0.73882776075704E+02
 0.24668480544364E+02 0.21184980489752E+02 0.24795254537651E+02 0.73821393405918E+02 0.42159664568471E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32787385458321E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46907276717593E-02 0.18053092957310E-01 0.00000000000000E+00 0.46907276717593E-02 0.18053092957310E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25446018533264E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.30127723964117E-03 0.25446018533264E-01 0.30127723964117E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11285861502079E-02 0.61958877910801E-01 0.37213349030864E-01
 0.00000000000000E+00 0.61958877910801E-01 0.38341935181072E-01 0.43173975497881E+00 0.10673975199858E+00
 0.32500000298023E+00 0.42250006586313E+00
    292.37713767
 0.91049388383297E+00 0.31265347319634E+03 0.46840690370448E+03 0.42113605508326E+03 0.40688843858288E+03
 0.22999999473890E+00 0.00000000000000E+00 0.13629816629335E+00 0.00000000000000E+00 -.80592010978776E+01
 0.96952669396254E-03 0.12389467216197E+01 0.80000000000000E+04 0.30000000000000E+04 0.64570976785359E+01
 0.24214116294510E+01 0.32289652495421E+03 0.30315000000684E+03 0.32177945256600E+03 0.35472315053674E+03
 0.30315000000031E+03 0.30315000000081E+03 0.32042745100035E+03 0.35456546117434E+03 0.30315000000024E+03
 0.30315000000080E+03 0.32177945256600E+03 0.35472315053674E+03 0.30315000000031E+03 0.30315000000081E+03
 0.32042745100035E+03 0.35456546117434E+03 0.30315000000024E+03 0.30315000000080E+03 0.40227196284950E+03
 0.30631889358651E+03 0.14705410508575E+04 0.14318433319213E+04 0.10114726681738E+04 0.16906793527877E+04
 0.67414932127298E+03 0.92682451710041E+03 0.15555857109487E+04 0.89912943206831E+03 0.24368355351684E+04
 0.87707398631549E+03 0.15509013622515E+04 0.85459745391223E+03 0.24336728549293E+04 0.92682451710042E+03
 0.15555857109487E+04 0.89912943206831E+03 0.24368355351684E+04 0.87707398631549E+03 0.15509013622515E+04
 0.85459745391223E+03 0.24336728549293E+04 0.19277200023542E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64913983811300E+05 0.50050000000000E+08 0.55439234191898E+03 0.12396369468784E+01
 0.12396369468784E+01 0.98981114604758E+00 0.48572480252270E+00 0.30382880302271E+03 0.31585582721422E+03
 0.31196127791398E+03 0.31185835324428E+03 0.23000000000000E+00 0.00000000000000E+00 0.21634667669821E+00
 0.00000000000000E+00 -.55281735511236E-01 0.99742696909551E-03 0.26516630339236E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30169745920403E+02 0.11313654720151E+02 0.30631875942901E+03 0.40231326021862E+03
 0.30364397270934E+03 0.30559603679049E+03 0.30315000000000E+03 0.30315000000010E+03 0.30363780555766E+03
 0.30559588060386E+03 0.30315000000000E+03 0.30315000000010E+03 0.30364397270934E+03 0.30559603679049E+03
 0.30315000000000E+03 0.30315000000010E+03 0.30363780555766E+03 0.30559588060386E+03 0.30315000000000E+03
 0.30315000000010E+03 0.30522023103681E+03 0.30315000000168E+03 0.11310049192306E+02 0.52935747568515E+01
 0.26349090030285E+02 0.94989428455215E+02 0.68508592974779E+02 0.25736133630563E+02 0.21828918045051E+02
 0.25935194416230E+02 0.73134057518113E+02 0.25429805596550E+02 0.21756239324601E+02 0.25637351734290E+02
 0.73062312886930E+02 0.25736133630564E+02 0.21828918045050E+02 0.25935194416231E+02 0.73134057518112E+02
 0.25429805596550E+02 0.21756239324601E+02 0.25637351734290E+02 0.73062312886931E+02 0.41405611264212E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32779043078554E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46878554620390E-02 0.18077338134385E-01 0.00000000000000E+00 0.46878554620390E-02 0.18077338134385E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25568386200334E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.28395878921174E-03 0.25568386200334E-01 0.28395878921174E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.12494970818663E-02 0.62056052300237E-01 0.36646200454818E-01
 0.00000000000000E+00 0.62056052300237E-01 0.37895697536685E-01 0.43036240126135E+00 0.10536239828112E+00
 0.32500000298023E+00 0.42250006586313E+00
    302.11546274
 0.91048176286270E+00 0.31316557582251E+03 0.46886592697207E+03 0.42161181690774E+03 0.40739358518242E+03
 0.22999999477578E+00 0.00000000000000E+00 0.13258040715911E+00 0.00000000000000E+00 -.80902571235874E+01
 0.96794098306085E-03 0.12861443585187E+01 0.80000000000000E+04 0.30000000000000E+04 0.62201415782079E+01
 0.23325530918280E+01 0.32344424719934E+03 0.30315000001085E+03 0.32227564021951E+03 0.35591605060069E+03
 0.30315000000051E+03 0.30315000000135E+03 0.32091215620294E+03 0.35576363182967E+03 0.30315000000040E+03
 0.30315000000134E+03 0.32227564021951E+03 0.35591605060069E+03 0.30315000000051E+03 0.30315000000135E+03
 0.32091215620294E+03 0.35576363182967E+03 0.30315000000040E+03 0.30315000000134E+03 0.40383330889853E+03
 0.30663006130202E+03 0.14795146399132E+04 0.14406823256895E+04 0.10095476235704E+04 0.16750498511994E+04
 0.66045448951120E+03 0.93261244275924E+03 0.15629204894057E+04 0.90500610442994E+03 0.24366884090006E+04
 0.88286424928813E+03 0.15587511675549E+04 0.86051166585123E+03 0.24339862498847E+04 0.93261244275924E+03
 0.15629204894057E+04 0.90500610442995E+03 0.24366884090006E+04 0.88286424928812E+03 0.15587511675550E+04
 0.86051166585122E+03 0.24339862498847E+04 0.19271824000413E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64839004864397E+05 0.50050000000000E+08 0.55485229856279E+03 0.12397056462368E+01
 0.12397056462368E+01 0.10287644463468E+01 0.48295443489283E+00 0.30391785143597E+03 0.31566941074678E+03
 0.31188576562274E+03 0.31178778832982E+03 0.23000000000000E+00 0.00000000000000E+00 0.21582488890598E+00
 0.00000000000000E+00 -.56538759819796E-01 0.99713469691367E-03 0.27563114769785E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29024295936139E+02 0.10884110976052E+02 0.30663000411673E+03 0.40387310570268E+03
 0.30366333775005E+03 0.30559951156633E+03 0.30315000000000E+03 0.30315000000016E+03 0.30365695703862E+03
 0.30559921239778E+03 0.30315000000000E+03 0.30315000000016E+03 0.30366333775005E+03 0.30559951156633E+03
 0.30315000000000E+03 0.30315000000016E+03 0.30365695703862E+03 0.30559921239778E+03 0.30315000000000E+03
 0.30315000000016E+03 0.30522595567379E+03 0.30315000000267E+03 0.98281261290022E+01 0.30876319623816E+01
 0.27232018964066E+02 0.94480660967114E+02 0.67112481908228E+02 0.26594205160865E+02 0.22486611186113E+02
 0.26893597643326E+02 0.72678785222444E+02 0.26273980025325E+02 0.22402485553928E+02 0.26582969780258E+02
 0.72596442159471E+02 0.26594205160865E+02 0.22486611186113E+02 0.26893597643326E+02 0.72678785222443E+02
 0.26273980025325E+02 0.22402485553928E+02 0.26582969780258E+02 0.72596442159470E+02 0.40778892730978E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32773951646802E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46719957829746E-02 0.18125826057729E-01 0.00000000000000E+00 0.46719957829746E-02 0.18125826057729E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25687550303867E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.26764510848723E-03 0.25687550303867E-01 0.26764510848723E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.13545119614780E-02 0.62195434624476E-01 0.36193424335203E-01
 0.00000000000000E+00 0.62195434624476E-01 0.37547936296681E-01 0.42897721744641E+00 0.10397721446618E+00
 0.32500000298023E+00 0.42250006586313E+00
    311.85378782
 0.91047862512177E+00 0.31367512124419E+03 0.46933764169113E+03 0.42209517582463E+03 0.40790348522632E+03
 0.22999999481445E+00 0.00000000000000E+00 0.12888400932337E+00 0.00000000000000E+00 -.81218950203951E+01
 0.96636832234700E-03 0.13329353171974E+01 0.80000000000000E+04 0.30000000000000E+04 0.60017916074281E+01
 0.22506718527856E+01 0.32398373155857E+03 0.30315000001683E+03 0.32276554428584E+03 0.35707975394511E+03
 0.30315000000083E+03 0.30315000000220E+03 0.32139021595553E+03 0.35693251180616E+03 0.30315000000066E+03
 0.30315000000219E+03 0.32276554428584E+03 0.35707975394511E+03 0.30315000000083E+03 0.30315000000220E+03
 0.32139021595553E+03 0.35693251180616E+03 0.30315000000066E+03 0.30315000000219E+03 0.40533837942737E+03
 0.30696253924299E+03 0.14883023501785E+04 0.14493637870659E+04 0.10075075051818E+04 0.16601971428020E+04
 0.64765210009431E+03 0.93830131662095E+03 0.15698112749142E+04 0.91079807429004E+03 0.24365101034341E+04
 0.88853298806885E+03 0.15661052434386E+04 0.86631923166318E+03 0.24342180198117E+04 0.93830131662096E+03
 0.15698112749142E+04 0.91079807429005E+03 0.24365101034341E+04 0.88853298806885E+03 0.15661052434386E+04
 0.86631923166318E+03 0.24342180198117E+04 0.19267521356471E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64819595163215E+05 0.50050000000000E+08 0.55532453722460E+03 0.12397261213457E+01
 0.12397261213457E+01 0.10677177466460E+01 0.48020502556580E+00 0.30401734372321E+03 0.31551684673326E+03
 0.31183543397531E+03 0.31174189567984E+03 0.23000000000000E+00 0.00000000000000E+00 0.21527927755373E+00
 0.00000000000000E+00 -.58103704021115E-01 0.99680834887560E-03 0.28653341501768E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27919954814019E+02 0.10469983055257E+02 0.30696256338767E+03 0.40537659254345E+03
 0.30368398141315E+03 0.30560577857437E+03 0.30315000000001E+03 0.30315000000026E+03 0.30367736604361E+03
 0.30560532759638E+03 0.30315000000001E+03 0.30315000000026E+03 0.30368398141315E+03 0.30560577857437E+03
 0.30315000000001E+03 0.30315000000026E+03 0.30367736604361E+03 0.30560532759638E+03 0.30315000000001E+03
 0.30315000000026E+03 0.30523396712404E+03 0.30315000000415E+03 0.82771852118802E+01 0.75628574146828E+00
 0.28190466660108E+02 0.94287268116821E+02 0.65955849123413E+02 0.27531943296986E+02 0.23210463871330E+02
 0.27954682872699E+02 0.72474668918840E+02 0.27196235664055E+02 0.23114649797253E+02 0.27629711378207E+02
 0.72381533472895E+02 0.27531943296986E+02 0.23210463871329E+02 0.27954682872699E+02 0.72474668918839E+02
 0.27196235664055E+02 0.23114649797253E+02 0.27629711378207E+02 0.72381533472895E+02 0.40257026833783E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32771728410658E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46455317902090E-02 0.18194205829623E-01 0.00000000000000E+00 0.46455317902090E-02 0.18194205829623E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25806579864178E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25192783528933E-03 0.25806579864178E-01 0.25192783528933E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14460575190891E-02 0.62372631472065E-01 0.35834527554574E-01
 0.00000000000000E+00 0.62372631472065E-01 0.37280585073663E-01 0.42760251278290E+00 0.10260250980267E+00
 0.32500000298023E+00 0.42250006586313E+00
    321.59211289
 0.91048352604432E+00 0.31418167634874E+03 0.46981906474360E+03 0.42258397202024E+03 0.40841641554350E+03
 0.22999999485683E+00 0.00000000000000E+00 0.12520922797883E+00 0.00000000000000E+00 -.81539545912644E+01
 0.96480994148022E-03 0.13793229127798E+01 0.80000000000000E+04 0.30000000000000E+04 0.57999471522423E+01
 0.21749801820909E+01 0.32451575249663E+03 0.30315000002557E+03 0.32324976594991E+03 0.35821662859545E+03
 0.30315000000132E+03 0.30315000000349E+03 0.32186225733491E+03 0.35807443723651E+03 0.30315000000105E+03
 0.30315000000348E+03 0.32324976594991E+03 0.35821662859545E+03 0.30315000000132E+03 0.30315000000349E+03
 0.32186225733491E+03 0.35807443723651E+03 0.30315000000105E+03 0.30315000000348E+03 0.40679175689841E+03
 0.30731575411870E+03 0.14969155718024E+04 0.14578929997722E+04 0.10053591625962E+04 0.16460288796032E+04
 0.63564292119401E+03 0.94389651020166E+03 0.15762902078839E+04 0.91650655406018E+03 0.24362815779559E+04
 0.89408828037105E+03 0.15730009388921E+04 0.87202414773334E+03 0.24343546036558E+04 0.94389651020168E+03
 0.15762902078840E+04 0.91650655406021E+03 0.24362815779559E+04 0.89408828037105E+03 0.15730009388921E+04
 0.87202414773333E+03 0.24343546036559E+04 0.19264020178972E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64849911730242E+05 0.50050000000000E+08 0.55580612965514E+03 0.12397035532654E+01
 0.12397035532654E+01 0.11066710469452E+01 0.47750460309254E+00 0.30412792425286E+03 0.31539361312296E+03
 0.31180733426131E+03 0.31171783398636E+03 0.23000000000000E+00 0.00000000000000E+00 0.21471026987324E+00
 0.00000000000000E+00 -.59955598415531E-01 0.99644588099693E-03 0.29786645699909E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26857673336560E+02 0.10071627501210E+02 0.30731586339695E+03 0.40682832938563E+03
 0.30370598233722E+03 0.30561474291283E+03 0.30315000000001E+03 0.30315000000041E+03 0.30369911083108E+03
 0.30561413161451E+03 0.30315000000001E+03 0.30315000000041E+03 0.30370598233722E+03 0.30561474291283E+03
 0.30315000000001E+03 0.30315000000041E+03 0.30369911083108E+03 0.30561413161451E+03 0.30315000000001E+03
 0.30315000000041E+03 0.30524419958598E+03 0.30315000000629E+03 0.66537881631237E+01 -.17013324318280E+01
 0.29215526762192E+02 0.94365875212716E+02 0.65004270816714E+02 0.28544910240621E+02 0.23992774097459E+02
 0.29116532363839E+02 0.72487353641742E+02 0.28192238695523E+02 0.23885076962472E+02 0.28775784836110E+02
 0.72383277728123E+02 0.28544910240621E+02 0.23992774097459E+02 0.29116532363839E+02 0.72487353641742E+02
 0.28192238695523E+02 0.23885076962471E+02 0.28775784836110E+02 0.72383277728122E+02 0.39822212947468E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32772057184179E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46101600252717E-02 0.18279442547027E-01 0.00000000000000E+00 0.46101600252717E-02 0.18279442547027E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25927694883243E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.23655580562767E-03 0.25927694883243E-01 0.23655580562767E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15257340705179E-02 0.62583810846832E-01 0.35553753704381E-01
 0.00000000000000E+00 0.62583810846832E-01 0.37079487774898E-01 0.42625230154627E+00 0.10125229856604E+00
 0.32500000298023E+00 0.42250006586313E+00
    331.33043797
 0.91049550442269E+00 0.31468489065181E+03 0.47030784214469E+03 0.42307650957160E+03 0.40893102431063E+03
 0.22999999489112E+00 0.00000000000000E+00 0.12155633864428E+00 0.00000000000000E+00 -.81863161551459E+01
 0.96326680106268E-03 0.14253094587377E+01 0.80000000000000E+04 0.30000000000000E+04 0.56128161859568E+01
 0.21048060697338E+01 0.32504095979158E+03 0.30315000003813E+03 0.32372880001743E+03 0.35932871456783E+03
 0.30315000000206E+03 0.30315000000544E+03 0.32232880605905E+03 0.35919142493522E+03 0.30315000000164E+03
 0.30315000000541E+03 0.32372880001743E+03 0.35932871456783E+03 0.30315000000206E+03 0.30315000000544E+03
 0.32232880605905E+03 0.35919142493521E+03 0.30315000000164E+03 0.30315000000541E+03 0.40819737538567E+03
 0.30768902641625E+03 0.15053639885395E+04 0.14662745840074E+04 0.10031133442421E+04 0.16324733352108E+04
 0.62434442424757E+03 0.94940221496982E+03 0.15823889691036E+04 0.92213234696308E+03 0.24359923816497E+04
 0.89953706147166E+03 0.15794746111517E+04 0.87762999012504E+03 0.24343904166250E+04 0.94940221496983E+03
 0.15823889691036E+04 0.92213234696309E+03 0.24359923816497E+04 0.89953706147166E+03 0.15794746111517E+04
 0.87762999012504E+03 0.24343904166250E+04 0.19261130577041E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64924008639848E+05 0.50050000000000E+08 0.55629477070876E+03 0.12396431396341E+01
 0.12396431396341E+01 0.11456243472444E+01 0.47487459986743E+00 0.30425015916342E+03 0.31529600884041E+03
 0.31179907987670E+03 0.31171330152004E+03 0.23000000000000E+00 0.00000000000000E+00 0.21411822941675E+00
 0.00000000000000E+00 -.62078637310795E-01 0.99604551900401E-03 0.30962444933040E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25837752856084E+02 0.96891573210315E+01 0.30768922410491E+03 0.40823227513984E+03
 0.30372941307919E+03 0.30562629955639E+03 0.30315000000002E+03 0.30315000000064E+03 0.30372226353411E+03
 0.30562551980157E+03 0.30315000000002E+03 0.30315000000064E+03 0.30372941307919E+03 0.30562629955639E+03
 0.30315000000002E+03 0.30315000000064E+03 0.30372226353411E+03 0.30562551980157E+03 0.30315000000002E+03
 0.30315000000064E+03 0.30525657578214E+03 0.30315000000936E+03 0.49560355755730E+01 -.42839793379076E+01
 0.30299330812518E+02 0.94680740273709E+02 0.64229912807128E+02 0.29629004015076E+02 0.24826933092741E+02
 0.30377627467747E+02 0.72688673955297E+02 0.29257983460076E+02 0.24707208156558E+02 0.30019783044231E+02
 0.72573557981020E+02 0.29629004015076E+02 0.24826933092741E+02 0.30377627467746E+02 0.72688673955297E+02
 0.29257983460076E+02 0.24707208156559E+02 0.30019783044231E+02 0.72573557981023E+02 0.39460301394102E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32774673702216E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45670862152371E-02 0.18379433865247E-01 0.00000000000000E+00 0.45670862152371E-02 0.18379433865247E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26052462993773E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22139066941606E-03 0.26052462993773E-01 0.22139066941606E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15945988550158E-02 0.62825739051398E-01 0.35338765550424E-01
 0.00000000000000E+00 0.62825739051398E-01 0.36933364405440E-01 0.42493729993372E+00 0.99937296953484E-01
 0.32500000298023E+00 0.42250006586313E+00
    344.96242614
 0.91052529038755E+00 0.31538309449808E+03 0.47099790046532E+03 0.42376749500134E+03 0.40965042770442E+03
 0.22999999496566E+00 0.00000000000000E+00 0.11648032069469E+00 0.00000000000000E+00 -.82320299768591E+01
 0.96113386066611E-03 0.14890204867803E+01 0.80000000000000E+04 0.30000000000000E+04 0.53726594570222E+01
 0.20147472963833E+01 0.32576276200341E+03 0.30315000006564E+03 0.32438858665895E+03 0.36084609018505E+03
 0.30315000000377E+03 0.30315000000995E+03 0.32297063606117E+03 0.36071526609895E+03 0.30315000000301E+03
 0.30315000000991E+03 0.32438858665894E+03 0.36084609018504E+03 0.30315000000377E+03 0.30315000000995E+03
 0.32297063606117E+03 0.36071526609895E+03 0.30315000000301E+03 0.30315000000991E+03 0.41010165609141E+03
 0.30824889688391E+03 0.15169463975181E+04 0.14777989781776E+04 0.99962348023744E+03 0.16140543145373E+04
 0.60943271689866E+03 0.95698223717340E+03 0.15902962083205E+04 0.92989755050309E+03 0.24354125162527E+04
 0.90700414743167E+03 0.15878458176481E+04 0.88533463726844E+03 0.24342089367501E+04 0.95698223717343E+03
 0.15902962083206E+04 0.92989755050313E+03 0.24354125162528E+04 0.90700414743167E+03 0.15878458176481E+04
 0.88533463726844E+03 0.24342089367501E+04 0.19256831690532E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65108261285859E+05 0.50050000000000E+08 0.55698416691866E+03 0.12394897372528E+01
 0.12394897372528E+01 0.12001522999383E+01 0.47134529548051E+00 0.30444131022433E+03 0.31519602784497E+03
 0.31181657080850E+03 0.31173560039023E+03 0.23000000000000E+00 0.00000000000000E+00 0.21325174515414E+00
 0.00000000000000E+00 -.65490257094002E-01 0.99542007842665E-03 0.32678109093070E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24481220676555E+02 0.91804577537082E+01 0.30824921319913E+03 0.41013424833421E+03
 0.30376421892211E+03 0.30564665928919E+03 0.30315000000005E+03 0.30315000000116E+03 0.30375664784413E+03
 0.30564563484846E+03 0.30315000000005E+03 0.30315000000116E+03 0.30376421892211E+03 0.30564665928919E+03
 0.30315000000005E+03 0.30315000000116E+03 0.30375664784413E+03 0.30564563484846E+03 0.30315000000005E+03
 0.30315000000116E+03 0.30527733018246E+03 0.30315000001606E+03 0.24182336210097E+01 -.81592022567162E+01
 0.31902846684593E+02 0.95456184873581E+02 0.63393823955566E+02 0.31263530972450E+02 0.26070415760825E+02
 0.32312211954613E+02 0.73237280415404E+02 0.30864153871409E+02 0.25933635414495E+02 0.31927883337434E+02
 0.73106540570521E+02 0.31263530972450E+02 0.26070415760825E+02 0.32312211954613E+02 0.73237280415404E+02
 0.30864153871410E+02 0.25933635414496E+02 0.31927883337435E+02 0.73106540570522E+02 0.39039241244056E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32781281758859E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44907410944041E-02 0.18550136266613E-01 0.00000000000000E+00 0.44907410944041E-02 0.18550136266613E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26222845047051E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20162578002755E-03 0.26222845047051E-01 0.20162578002755E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16755203403736E-02 0.63198125042870E-01 0.35138383483321E-01
 0.00000000000000E+00 0.63198125042870E-01 0.36813903823694E-01 0.42317264774026E+00 0.98172644760023E-01
 0.32500000298023E+00 0.42250006586313E+00
    350.15536605
 0.91053910345733E+00 0.31564701430984E+03 0.47126219898306E+03 0.42403096207083E+03 0.40992400499378E+03
 0.22999999492131E+00 0.00000000000000E+00 0.11455835181637E+00 0.00000000000000E+00 -.82493543176387E+01
 0.96033007009000E-03 0.15130860831122E+01 0.80000000000000E+04 0.30000000000000E+04 0.52872074426493E+01
 0.19827027909935E+01 0.32603509525373E+03 0.30315000008012E+03 0.32463799934377E+03 0.36141296356217E+03
 0.30315000000471E+03 0.30315000001242E+03 0.32321307360464E+03 0.36128453782268E+03 0.30315000000376E+03
 0.30315000001237E+03 0.32463799934377E+03 0.36141296356217E+03 0.30315000000471E+03 0.30315000001242E+03
 0.32321307360464E+03 0.36128453782268E+03 0.30315000000376E+03 0.30315000001237E+03 0.41080282084614E+03
 0.30847137883972E+03 0.15212687869243E+04 0.14821019869406E+04 0.99828975384966E+03 0.16073615241116E+04
 0.60408032149266E+03 0.95981829751171E+03 0.15931343160792E+04 0.93280337271852E+03 0.24351443943272E+04
 0.90979220967748E+03 0.15908431417668E+04 0.88821285422658E+03 0.24340758021683E+04 0.95981829751173E+03
 0.15931343160793E+04 0.93280337271856E+03 0.24351443943274E+04 0.90979220967747E+03 0.15908431417668E+04
 0.88821285422656E+03 0.24340758021683E+04 0.19255430944875E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65193707367728E+05 0.50050000000000E+08 0.55724811060772E+03 0.12394181240580E+01
 0.12394181240580E+01 0.12209240595687E+01 0.47005267633309E+00 0.30452051263483E+03 0.31516810008541E+03
 0.31183148210032E+03 0.31175224433130E+03 0.23000000000000E+00 0.00000000000000E+00 0.21291017123882E+00
 0.00000000000000E+00 -.66902531579579E-01 0.99516116121824E-03 0.33352933109808E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23985896453729E+02 0.89947111701483E+01 0.30847174291725E+03 0.41083453057242E+03
 0.30377836587952E+03 0.30565562876936E+03 0.30315000000006E+03 0.30315000000144E+03 0.30377062129434E+03
 0.30565450646948E+03 0.30315000000006E+03 0.30315000000144E+03 0.30377836587952E+03 0.30565562876936E+03
 0.30315000000006E+03 0.30315000000144E+03 0.30377062129434E+03 0.30565450646948E+03 0.30315000000006E+03
 0.30315000000144E+03 0.30528624931852E+03 0.30315000001957E+03 0.14155957131992E+01 -.96930120518367E+01
 0.32538775682630E+02 0.95847170413205E+02 0.63145700852161E+02 0.31921249939323E+02 0.26566183190685E+02
 0.33101131839211E+02 0.73522621083283E+02 0.31510549940374E+02 0.26422851456234E+02 0.32706238978356E+02
 0.73385902144569E+02 0.31921249939322E+02 0.26566183190683E+02 0.33101131839210E+02 0.73522621083278E+02
 0.31510549940374E+02 0.26422851456231E+02 0.32706238978356E+02 0.73385902144563E+02 0.38907623791513E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32785030982102E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44623259537220E-02 0.18614322194179E-01 0.00000000000000E+00 0.44623259537220E-02 0.18614322194179E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26299517458852E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19327789643611E-03 0.26299517458852E-01 0.19327789643611E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17006193381669E-02 0.63360903622893E-01 0.35078906057078E-01
 0.00000000000000E+00 0.63360903622893E-01 0.36779525395245E-01 0.42252633816654E+00 0.97526335186312E-01
 0.32500000298023E+00 0.42250006586313E+00
    360.54124586
 0.91056902336754E+00 0.31617133870147E+03 0.47179448745558E+03 0.42455928126411E+03 0.41047112878705E+03
 0.22999999502203E+00 0.00000000000000E+00 0.11073389438886E+00 0.00000000000000E+00 -.82842052989776E+01
 0.95873717193616E-03 0.15608732451600E+01 0.80000000000000E+04 0.30000000000000E+04 0.51253361058027E+01
 0.19220010396760E+01 0.32657551967011E+03 0.30315000011766E+03 0.32513362919944E+03 0.36252940666940E+03
 0.30315000000722E+03 0.30315000001905E+03 0.32369459369834E+03 0.36240563689971E+03 0.30315000000578E+03
 0.30315000001898E+03 0.32513362919943E+03 0.36252940666940E+03 0.30315000000722E+03 0.30315000001905E+03
 0.32369459369834E+03 0.36240563689971E+03 0.30315000000578E+03 0.30315000001898E+03 0.41217033113437E+03
 0.30893065595976E+03 0.15297899704784E+04 0.14905877586238E+04 0.99563251508374E+03 0.15944759185386E+04
 0.59386524087947E+03 0.96541634111392E+03 0.15985935665223E+04 0.93853922178487E+03 0.24346003974696E+04
 0.91529087200173E+03 0.15965947881242E+04 0.89389061886844E+03 0.24337772965964E+04 0.96541634111393E+03
 0.15985935665223E+04 0.93853922178489E+03 0.24346003974697E+04 0.91529087200174E+03 0.15965947881242E+04
 0.89389061886845E+03 0.24337772965964E+04 0.19253320924954E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65378788592167E+05 0.50050000000000E+08 0.55777954833357E+03 0.12392622105345E+01
 0.12392622105345E+01 0.12624675788296E+01 0.46755779216731E+00 0.30469009835731E+03 0.31512791634124E+03
 0.31187439425350E+03 0.31179848988714E+03 0.23000000000000E+00 0.00000000000000E+00 0.21220786866277E+00
 0.00000000000000E+00 -.69945150069364E-01 0.99460722927971E-03 0.34737986326311E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23029544444091E+02 0.86360791665340E+01 0.30893112355001E+03 0.41220026545825E+03
 0.30380811723119E+03 0.30567546083158E+03 0.30315000000009E+03 0.30315000000220E+03 0.30380000643092E+03
 0.30567413649192E+03 0.30315000000009E+03 0.30315000000220E+03 0.30380811723119E+03 0.30567546083158E+03
 0.30315000000009E+03 0.30315000000220E+03 0.30380000643092E+03 0.30567413649192E+03 0.30315000000009E+03
 0.30315000000220E+03 0.30530567845593E+03 0.30315000002862E+03 -.64760266861749E+00 -.12848742661942E+02
 0.33844126319177E+02 0.96767951463472E+02 0.62754604512700E+02 0.33286728795453E+02 0.27589398177838E+02
 0.34759810027172E+02 0.74206066659611E+02 0.32852444808506E+02 0.27433006907887E+02 0.34342930372377E+02
 0.74057469828629E+02 0.33286728795454E+02 0.27589398177839E+02 0.34759810027174E+02 0.74206066659612E+02
 0.32852444808506E+02 0.27433006907889E+02 0.34342930372377E+02 0.74057469828634E+02 0.38685882540485E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32793902567078E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43984504803315E-02 0.18756636034651E-01 0.00000000000000E+00 0.43984504803315E-02 0.18756636034651E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26453712337020E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.17718011257639E-03 0.26453712337020E-01 0.17718011257639E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17431625411537E-02 0.63703456975011E-01 0.35000829607662E-01
 0.00000000000000E+00 0.63703456975011E-01 0.36743992148816E-01 0.42127889608365E+00 0.96278893103422E-01
 0.32500000298023E+00 0.42250006586313E+00
    370.92712568
 0.91060488372790E+00 0.31669085256141E+03 0.47232924700234E+03 0.42508755297784E+03 0.41101669341231E+03
 0.22999999509526E+00 0.00000000000000E+00 0.10693564761989E+00 0.00000000000000E+00 -.83191603956498E+01
 0.95716408632483E-03 0.16082072359154E+01 0.80000000000000E+04 0.30000000000000E+04 0.49744832763709E+01
 0.18654312286391E+01 0.32711052180814E+03 0.30315000016976E+03 0.32562512934718E+03 0.36362456947449E+03
 0.30315000001087E+03 0.30315000002865E+03 0.32417178586399E+03 0.36350525821401E+03 0.30315000000872E+03
 0.30315000002854E+03 0.32562512934718E+03 0.36362456947449E+03 0.30315000001087E+03 0.30315000002865E+03
 0.32417178586399E+03 0.36350525821401E+03 0.30315000000872E+03 0.30315000002854E+03 0.41349648013001E+03
 0.30940850636117E+03 0.15381864680646E+04 0.14989515061868E+04 0.99293581746353E+03 0.15821102611339E+04
 0.58420976458303E+03 0.97094832500284E+03 0.16037538243541E+04 0.94420574233788E+03 0.24339968047698E+04
 0.92070884296234E+03 0.16020163808755E+04 0.89948510133218E+03 0.24333902714289E+04 0.97094832500285E+03
 0.16037538243541E+04 0.94420574233790E+03 0.24339968047698E+04 0.92070884296234E+03 0.16020163808755E+04
 0.89948510133218E+03 0.24333902714289E+04 0.19251714938296E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65600616764770E+05 0.50050000000000E+08 0.55831322412834E+03 0.12390746450983E+01
 0.12390746450983E+01 0.13040110980904E+01 0.46519244314643E+00 0.30487465362219E+03 0.31510654677711E+03
 0.31193334719394E+03 0.31186063562245E+03 0.23000000000000E+00 0.00000000000000E+00 0.21148037470753E+00
 0.00000000000000E+00 -.73261075120067E-01 0.99400510250534E-03 0.36169613008143E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22118013809545E+02 0.82942551785793E+01 0.30940908113239E+03 0.41352467305357E+03
 0.30383980673577E+03 0.30569775344294E+03 0.30315000000014E+03 0.30315000000329E+03 0.30383130430421E+03
 0.30569621973834E+03 0.30315000000014E+03 0.30315000000330E+03 0.30383980673577E+03 0.30569775344294E+03
 0.30315000000014E+03 0.30315000000329E+03 0.30383130430421E+03 0.30569621973834E+03 0.30315000000014E+03
 0.30315000000330E+03 0.30532716407561E+03 0.30315000004109E+03 -.27892361751487E+01 -.16120655714036E+02
 0.35188788238589E+02 0.97853947010888E+02 0.62489214831106E+02 0.34716528083157E+02 0.28650352708777E+02
 0.36525934309728E+02 0.75024044853092E+02 0.34257458617271E+02 0.28480997382810E+02 0.36085977915760E+02
 0.74863709342355E+02 0.34716528083158E+02 0.28650352708778E+02 0.36525934309729E+02 0.75024044853094E+02
 0.34257458617271E+02 0.28480997382812E+02 0.36085977915760E+02 0.74863709342359E+02 0.38511750884100E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32804553796901E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43274092470019E-02 0.18913571120037E-01 0.00000000000000E+00 0.43274092470019E-02 0.18913571120037E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26613297835062E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16152600132339E-03 0.26613297835062E-01 0.16152600132339E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17755362240056E-02 0.64069255811373E-01 0.34968240136901E-01
 0.00000000000000E+00 0.64069255811373E-01 0.36743776360907E-01 0.42009622157321E+00 0.95096218592981E-01
 0.32500000298023E+00 0.42250006586313E+00
    381.31300550
 0.91064689058882E+00 0.31720543109404E+03 0.47286450463122E+03 0.42561435419503E+03 0.41155958816939E+03
 0.22999999511934E+00 0.00000000000000E+00 0.10316408782002E+00 0.00000000000000E+00 -.83541110932898E+01
 0.95561102132287E-03 0.16550899620599E+01 0.80000000000000E+04 0.30000000000000E+04 0.48335741158405E+01
 0.18125902934402E+01 0.32764047569298E+03 0.30315000024099E+03 0.32611279080550E+03 0.36469973630446E+03
 0.30315000001605E+03 0.30315000004231E+03 0.32464493345708E+03 0.36458468400971E+03 0.30315000001291E+03
 0.30315000004215E+03 0.32611279080550E+03 0.36469973630446E+03 0.30315000001605E+03 0.30315000004231E+03
 0.32464493345708E+03 0.36458468400971E+03 0.30315000001291E+03 0.30315000004215E+03 0.41478412061482E+03
 0.30990393185336E+03 0.15464656947852E+04 0.15071981373853E+04 0.99019106294621E+03 0.15701945825221E+04
 0.57505256426116E+03 0.97641976053984E+03 0.16086226972408E+04 0.94980684400286E+03 0.24333106384230E+04
 0.92605092184346E+03 0.16071189563137E+04 0.90499961409154E+03 0.24328951702213E+04 0.97641976053985E+03
 0.16086226972409E+04 0.94980684400288E+03 0.24333106384230E+04 0.92605092184346E+03 0.16071189563137E+04
 0.90499961409154E+03 0.24328951702214E+04 0.19250372823686E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65860466483880E+05 0.50050000000000E+08 0.55884718296893E+03 0.12388545232274E+01
 0.12388545232274E+01 0.13455546173513E+01 0.46296325757737E+00 0.30507418147870E+03 0.31510211331376E+03
 0.31200707065434E+03 0.31193744141797E+03 0.23000000000000E+00 0.00000000000000E+00 0.21072794106633E+00
 0.00000000000000E+00 -.76835893828654E-01 0.99335494719568E-03 0.37647328278628E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21249847906316E+02 0.79686929648685E+01 0.30990461412985E+03 0.41481062787275E+03
 0.30387347774933E+03 0.30572240314645E+03 0.30315000000022E+03 0.30315000000484E+03 0.30386455843926E+03
 0.30572065363638E+03 0.30315000000022E+03 0.30315000000484E+03 0.30387347774933E+03 0.30572240314645E+03
 0.30315000000022E+03 0.30315000000484E+03 0.30386455843926E+03 0.30572065363638E+03 0.30315000000022E+03
 0.30315000000484E+03 0.30535061674048E+03 0.30315000005802E+03 -.50074199211017E+01 -.19502469181808E+02
 0.36566956477016E+02 0.99086319555897E+02 0.62336528296496E+02 0.36207083308826E+02 0.29744637563503E+02
 0.38398082124184E+02 0.75962037582142E+02 0.35722096502883E+02 0.29562473610969E+02 0.37934033972599E+02
 0.75790158351589E+02 0.36207083308825E+02 0.29744637563504E+02 0.38398082124183E+02 0.75962037582143E+02
 0.35722096502883E+02 0.29562473610969E+02 0.37934033972599E+02 0.75790158351589E+02 0.38379142541461E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32816898718180E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42505727974128E-02 0.19082791496408E-01 0.00000000000000E+00 0.42505727974128E-02 0.19082791496408E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26780781862711E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14619040274274E-03 0.26780781862711E-01 0.14619040274274E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17978397556444E-02 0.64457833120194E-01 0.34974252413581E-01
 0.00000000000000E+00 0.64457833120194E-01 0.36772092169226E-01 0.41898162878868E+00 0.93981625808450E-01
 0.32500000298023E+00 0.42250006586313E+00
    391.69888531
 0.91069456316635E+00 0.31771499137084E+03 0.47339916765631E+03 0.42613892335157E+03 0.41209922692010E+03
 0.22999999515753E+00 0.00000000000000E+00 0.99419707537731E-01 0.00000000000000E+00 -.83890112188699E+01
 0.95407805674184E-03 0.17015207163008E+01 0.80000000000000E+04 0.30000000000000E+04 0.47016765199264E+01
 0.17631286949724E+01 0.32816569445369E+03 0.30315000033703E+03 0.32659684706794E+03 0.36575594065570E+03
 0.30315000002332E+03 0.30315000006145E+03 0.32511428185332E+03 0.36564494849951E+03 0.30315000001879E+03
 0.30315000006122E+03 0.32659684706794E+03 0.36575594065570E+03 0.30315000002332E+03 0.30315000006145E+03
 0.32511428185332E+03 0.36564494849951E+03 0.30315000001879E+03 0.30315000006122E+03 0.41603567900926E+03
 0.31041586822260E+03 0.15546329253444E+04 0.15153309426114E+04 0.98740458888266E+03 0.15586897977161E+04
 0.56634818588900E+03 0.98183300471851E+03 0.16132203595441E+04 0.95534367978627E+03 0.24325416764511E+04
 0.93132085683049E+03 0.16119256432735E+04 0.91043672760645E+03 0.24322946953787E+04 0.98183300471851E+03
 0.16132203595442E+04 0.95534367978629E+03 0.24325416764511E+04 0.93132085683050E+03 0.16119256432735E+04
 0.91043672760647E+03 0.24322946953787E+04 0.19249217438050E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66155363692108E+05 0.50050000000000E+08 0.55938035777269E+03 0.12386044162510E+01
 0.12386044162510E+01 0.13870981366121E+01 0.46087370014088E+00 0.30528862184362E+03 0.31511321933388E+03
 0.31209462026873E+03 0.31202798355350E+03 0.23000000000000E+00 0.00000000000000E+00 0.20995068407654E+00
 0.00000000000000E+00 -.80659815277606E-01 0.99265714862634E-03 0.39170870995550E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20423339580345E+02 0.76587523426294E+01 0.31041665812682E+03 0.41606055929903E+03
 0.30390918130732E+03 0.30574931524778E+03 0.30315000000033E+03 0.30315000000699E+03 0.30389982018112E+03
 0.30574734435660E+03 0.30315000000032E+03 0.30315000000699E+03 0.30390918130732E+03 0.30574931524778E+03
 0.30315000000033E+03 0.30315000000699E+03 0.30389982018112E+03 0.30574734435660E+03 0.30315000000032E+03
 0.30315000000699E+03 0.30537595555708E+03 0.30315000008068E+03 -.72989195789207E+01 -.22986286287932E+02
 0.37973846553924E+02 0.10045060460291E+03 0.62286888816218E+02 0.37755183778440E+02 0.30868861833606E+02
 0.40374912443045E+02 0.77009130918271E+02 0.37243210600743E+02 0.30674104320283E+02 0.39885824787295E+02
 0.76825958477733E+02 0.37755183778441E+02 0.30868861833610E+02 0.40374912443047E+02 0.77009130918278E+02
 0.37243210600743E+02 0.30674104320287E+02 0.39885824787295E+02 0.76825958477741E+02 0.38284166351950E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32830842316150E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41684631440710E-02 0.19263579163414E-01 0.00000000000000E+00 0.41684631440710E-02 0.19263579163414E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26956942590986E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13122320003515E-03 0.26956942590986E-01 0.13122320003515E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18102270232154E-02 0.64867945603567E-01 0.35014791026633E-01
 0.00000000000000E+00 0.64867945603567E-01 0.36825018049848E-01 0.41793685007044E+00 0.92936847090209E-01
 0.32500000298023E+00 0.42250006586313E+00
    402.08476513
 0.91074781268579E+00 0.31821947562467E+03 0.47393220316705E+03 0.42666052782820E+03 0.41263504403836E+03
 0.22999999518368E+00 0.00000000000000E+00 0.95703014147917E-01 0.00000000000000E+00 -.84238151553422E+01
 0.95256519684907E-03 0.17474987921428E+01 0.80000000000000E+04 0.30000000000000E+04 0.45779716907215E+01
 0.17167393840206E+01 0.32868647121341E+03 0.30315000046487E+03 0.32707751905106E+03 0.36679416568937E+03
 0.30315000003336E+03 0.30315000008789E+03 0.32558006201283E+03 0.36668703903182E+03 0.30315000002694E+03
 0.30315000008756E+03 0.32707751905106E+03 0.36679416568936E+03 0.30315000003336E+03 0.30315000008789E+03
 0.32558006201283E+03 0.36668703903182E+03 0.30315000002694E+03 0.30315000008756E+03 0.41725339873757E+03
 0.31094325997875E+03 0.15626960866717E+04 0.15233560968067E+04 0.98457892261183E+03 0.15475564423799E+04
 0.55805462515501E+03 0.98719304045569E+03 0.16175620145976E+04 0.96082018491964E+03 0.24316858724197E+04
 0.93652372260198E+03 0.16164543005934E+04 0.91580049729775E+03 0.24315873947105E+04 0.98719304045570E+03
 0.16175620145976E+04 0.96082018491965E+03 0.24316858724198E+04 0.93652372260200E+03 0.16164543005935E+04
 0.91580049729778E+03 0.24315873947106E+04 0.19248164051837E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66484759231818E+05 0.50050000000000E+08 0.55991173121202E+03 0.12383248543431E+01
 0.12383248543431E+01 0.14286416558730E+01 0.45892579176548E+00 0.30551777420289E+03 0.31513868520433E+03
 0.31219516240510E+03 0.31213144275987E+03 0.23000000000000E+00 0.00000000000000E+00 0.20914869264238E+00
 0.00000000000000E+00 -.84722049038993E-01 0.99191256039438E-03 0.40740017090251E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19636712430134E+02 0.73637671613002E+01 0.31094415782724E+03 0.41727671201736E+03
 0.30394696506449E+03 0.30577840682667E+03 0.30315000000048E+03 0.30315000000994E+03 0.30393713742161E+03
 0.30577620984118E+03 0.30315000000047E+03 0.30315000000994E+03 0.30394696506449E+03 0.30577840682667E+03
 0.30315000000048E+03 0.30315000000994E+03 0.30393713742161E+03 0.30577620984117E+03 0.30315000000047E+03
 0.30315000000994E+03 0.30540310726306E+03 0.30315000011061E+03 -.96601834071855E+01 -.26564270653636E+02
 0.39405140412970E+02 0.10193444308066E+03 0.62332276965627E+02 0.39357797696512E+02 0.32020018735475E+02
 0.42454796650781E+02 0.78156053423523E+02 0.38817827109868E+02 0.31812939895257E+02 0.41939783520147E+02
 0.77961891367187E+02 0.39357797696517E+02 0.32020018735485E+02 0.42454796650788E+02 0.78156053423542E+02
 0.38817827109868E+02 0.31812939895271E+02 0.41939783520147E+02 0.77961891367214E+02 0.38223732709830E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32846308479661E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.40816199930058E-02 0.19455177009750E-01 0.00000000000000E+00 0.40816199930058E-02 0.19455177009750E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27142634479057E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11668076965279E-03 0.27142634479057E-01 0.11668076965279E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18128265865538E-02 0.65298671800566E-01 0.35086284584672E-01
 0.00000000000000E+00 0.65298671800566E-01 0.36899111171226E-01 0.41696289588274E+00 0.91962892902509E-01
 0.32500000298023E+00 0.42250006586313E+00
    412.47064494
 0.91080642460174E+00 0.31871884837907E+03 0.47446278688143E+03 0.42717859362119E+03 0.41316660199397E+03
 0.22999999521939E+00 0.00000000000000E+00 0.92014549596421E-01 0.00000000000000E+00 -.84584897492855E+01
 0.95107238006840E-03 0.17930226457804E+01 0.80000000000000E+04 0.30000000000000E+04 0.44617395205948E+01
 0.16731523202231E+01 0.32920307270438E+03 0.30315000063306E+03 0.32755501154823E+03 0.36781526004264E+03
 0.30315000004705E+03 0.30315000012393E+03 0.32604248510989E+03 0.36771181025740E+03 0.30315000003807E+03
 0.30315000012347E+03 0.32755501154823E+03 0.36781526004264E+03 0.30315000004705E+03 0.30315000012393E+03
 0.32604248510989E+03 0.36771181025740E+03 0.30315000003807E+03 0.30315000012347E+03 0.41843919737644E+03
 0.31148505603560E+03 0.15706608487201E+04 0.15312777663733E+04 0.98171897102157E+03 0.15367641905061E+04
 0.55013662462941E+03 0.99250307808688E+03 0.16216627641977E+04 0.96623867751883E+03 0.24307422842989E+04
 0.94166329513091E+03 0.16207223724734E+04 0.92109384375423E+03 0.24307745922838E+04 0.99250307808688E+03
 0.16216627641978E+04 0.96623867751884E+03 0.24307422842990E+04 0.94166329513092E+03 0.16207223724734E+04
 0.92109384375424E+03 0.24307745922838E+04 0.19247155248892E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66847325955446E+05 0.50050000000000E+08 0.56044049575401E+03 0.12380170000840E+01
 0.12380170000840E+01 0.14701851751338E+01 0.45712010138179E+00 0.30576132734370E+03 0.31517755555747E+03
 0.31230799076033E+03 0.31224712169195E+03 0.23000000000000E+00 0.00000000000000E+00 0.20832200409042E+00
 0.00000000000000E+00 -.89011957051297E-01 0.99112240406633E-03 0.42354622602290E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18888139023502E+02 0.70830521338132E+01 0.31148606226746E+03 0.41846100423762E+03
 0.30398687381426E+03 0.30580960247033E+03 0.30315000000070E+03 0.30315000001392E+03 0.30397655520020E+03
 0.30580717553519E+03 0.30315000000069E+03 0.30315000001393E+03 0.30398687381426E+03 0.30580960247033E+03
 0.30315000000070E+03 0.30315000001392E+03 0.30397655520020E+03 0.30580717553519E+03 0.30315000000069E+03
 0.30315000001393E+03 0.30543200425860E+03 0.30315000014966E+03 -.12087225360503E+02 -.30228457739593E+02
 0.40857079531084E+02 0.10352768193289E+03 0.62466317004152E+02 0.41012097044431E+02 0.33195598287871E+02
 0.44635865408920E+02 0.79395300543562E+02 0.40443170333377E+02 0.32976524959617E+02 0.44094096300853E+02
 0.79190502996529E+02 0.41012097044435E+02 0.33195598287879E+02 0.44635865408926E+02 0.79395300543578E+02
 0.40443170333377E+02 0.32976524959629E+02 0.44094096300853E+02 0.79190502996553E+02 0.38195658043343E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32863231685860E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39904523639595E-02 0.19657081345837E-01 0.00000000000000E+00 0.39904523639595E-02 0.19657081345837E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27338455276315E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.10264637694565E-03 0.27338455276315E-01 0.10264637694565E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.18057809047752E-02 0.65749124025731E-01 0.35185979325387E-01
 0.00000000000000E+00 0.65749124025731E-01 0.36991760230163E-01 0.41606005069089E+00 0.91060047710661E-01
 0.32500000298023E+00 0.42250006586313E+00
    423.81898046
 0.91087680025861E+00 0.31925862434848E+03 0.47503850382306E+03 0.42773974443624E+03 0.41374187500374E+03
 0.22999999521514E+00 0.00000000000000E+00 0.88017215989044E-01 0.00000000000000E+00 -.84962613483676E+01
 0.94946403190701E-03 0.18422450861508E+01 0.80000000000000E+04 0.30000000000000E+04 0.43425275280367E+01
 0.16284478230138E+01 0.32976217155179E+03 0.30315000087699E+03 0.32807246559658E+03 0.36891219616550E+03
 0.30315000006765E+03 0.30315000017819E+03 0.32654329123219E+03 0.36881253811637E+03 0.30315000005488E+03
 0.30315000017754E+03 0.32807246559658E+03 0.36891219616549E+03 0.30315000006765E+03 0.30315000017819E+03
 0.32654329123219E+03 0.36881253811637E+03 0.30315000005488E+03 0.30315000017754E+03 0.41970357879402E+03
 0.31209408217477E+03 0.15792622806608E+04 0.15398291602652E+04 0.97850392563779E+03 0.15252327091819E+04
 0.54183626391591E+03 0.99825547856813E+03 0.16258765955195E+04 0.97210287589344E+03 0.24296002299710E+04
 0.94721316026253E+03 0.16250991984477E+04 0.92680583817592E+03 0.24297579279534E+04 0.99825547856813E+03
 0.16258765955195E+04 0.97210287589345E+03 0.24296002299711E+04 0.94721316026254E+03 0.16250991984477E+04
 0.92680583817593E+03 0.24297579279534E+04 0.19245804357753E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.67282661844446E+05 0.50050000000000E+08 0.56101405757273E+03 0.12376473072022E+01
 0.12376473072022E+01 0.15155785171951E+01 0.45530963553957E+00 0.30604332664671E+03 0.31523434013715E+03
 0.31244450213544E+03 0.31238667724913E+03 0.23000000000000E+00 0.00000000000000E+00 0.20739055346741E+00
 0.00000000000000E+00 -.93954280694076E-01 0.99020908954847E-03 0.44170572800900E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18111605742720E+02 0.67918521535199E+01 0.31209520371721E+03 0.41972382086161E+03
 0.30403265116303E+03 0.30584585993921E+03 0.30315000000103E+03 0.30315000001988E+03 0.30402177155624E+03
 0.30584317969434E+03 0.30315000000101E+03 0.30315000001989E+03 0.30403265116303E+03 0.30584585993921E+03
 0.30315000000103E+03 0.30315000001988E+03 0.30402177155624E+03 0.30584317969434E+03 0.30315000000101E+03
 0.30315000001989E+03 0.30546536122853E+03 0.30315000020578E+03 -.14822278412755E+02 -.34342431459605E+02
 0.42464189683463E+02 0.10538400970179E+03 0.62707499069915E+02 0.42877553404105E+02 0.34506195515964E+02
 0.47134647958670E+02 0.80847979432605E+02 0.42275736586277E+02 0.34274424973541E+02 0.46562534719035E+02
 0.80631987849935E+02 0.42877553404108E+02 0.34506195515970E+02 0.47134647958674E+02 0.80847979432616E+02
 0.42275736586276E+02 0.34274424973548E+02 0.46562534719034E+02 0.80631987849950E+02 0.38195481034996E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32883122240065E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.38843730012999E-02 0.19892872816214E-01 0.00000000000000E+00 0.38843730012999E-02 0.19892872816214E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27558508606161E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.88332026103999E-04 0.27558508606161E-01 0.88332026103999E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17878689677004E-02 0.66256897451265E-01 0.35329234883764E-01
 0.00000000000000E+00 0.66256897451265E-01 0.37117103851465E-01 0.41515481776979E+00 0.90154814789555E-01
 0.32500000298023E+00 0.42250006586313E+00
    436.12977167
 0.91096010908459E+00 0.31983718742131E+03 0.47565722239865E+03 0.42834194371181E+03 0.41435883517613E+03
 0.22999999528779E+00 0.00000000000000E+00 0.83720706550379E-01 0.00000000000000E+00 -.85368359148199E+01
 0.94774613842287E-03 0.18950242586088E+01 0.80000000000000E+04 0.30000000000000E+04 0.42215818418458E+01
 0.15830931906922E+01 0.33036381070273E+03 0.30315000123223E+03 0.32863014026554E+03 0.37008092466572E+03
 0.30315000009884E+03 0.30315000026037E+03 0.32708266643280E+03 0.36998515484872E+03 0.30315000008039E+03
 0.30315000025941E+03 0.32863014026554E+03 0.37008092466572E+03 0.30315000009884E+03 0.30315000026037E+03
 0.32708266643280E+03 0.36998515484872E+03 0.30315000008039E+03 0.30315000025941E+03 0.42103486715617E+03
 0.31277123515246E+03 0.15884659719936E+04 0.15489674798144E+04 0.97499569572696E+03 0.15131408985766E+04
 0.53327022437097E+03 0.10044316123038E+04 0.16301463092438E+04 0.97838680416136E+03 0.24282248858416E+04
 0.95315249216945E+03 0.16295247021302E+04 0.93290894417470E+03 0.24284999014420E+04 0.10044316123038E+04
 0.16301463092438E+04 0.97838680416137E+03 0.24282248858416E+04 0.95315249216945E+03 0.16295247021302E+04
 0.93290894417470E+03 0.24284999014420E+04 0.19244236910916E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.67798000884067E+05 0.50050000000000E+08 0.56163026919805E+03 0.12372096412753E+01
 0.12372096412753E+01 0.15648216820569E+01 0.45353555449425E+00 0.30636717674465E+03 0.31531192462674E+03
 0.31260741716640E+03 0.31255281243346E+03 0.23000000000000E+00 0.00000000000000E+00 0.20634682971271E+00
 0.00000000000000E+00 -.99571130919609E-01 0.98916230800942E-03 0.46201512955797E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17315450270328E+02 0.64932938513728E+01 0.31277248216077E+03 0.42105349372607E+03
 0.30408536003049E+03 0.30588790067621E+03 0.30315000000154E+03 0.30315000002882E+03 0.30407383747803E+03
 0.30588494209750E+03 0.30315000000151E+03 0.30315000002883E+03 0.30408536003049E+03 0.30588790067621E+03
 0.30315000000154E+03 0.30315000002882E+03 0.30407383747803E+03 0.30588494209750E+03 0.30315000000151E+03
 0.30315000002883E+03 0.30550377460274E+03 0.30315000028665E+03 -.17864225279148E+02 -.38899042456531E+02
 0.44228932563362E+02 0.10752645726109E+03 0.63076380034906E+02 0.44967294691545E+02 0.35956515283001E+02
 0.49978301502898E+02 0.82534249786512E+02 0.44328733719589E+02 0.35711510205346E+02 0.49372389829582E+02
 0.82306684315596E+02 0.44967294691549E+02 0.35956515283007E+02 0.49978301502903E+02 0.82534249786523E+02
 0.44328733719589E+02 0.35711510205354E+02 0.49372389829582E+02 0.82306684315613E+02 0.38236763144784E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32906808855355E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37677970909498E-02 0.20154783552338E-01 0.00000000000000E+00 0.37677970909498E-02 0.20154783552338E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27821788227149E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.73220283536526E-04 0.27821788227149E-01 0.73220283536526E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17548748193349E-02 0.66842232510127E-01 0.35507859517068E-01
 0.00000000000000E+00 0.66842232510127E-01 0.37262734336403E-01 0.41426777724713E+00 0.89267774266895E-01
 0.32500000298023E+00 0.42250006586313E+00
    442.28516728
 0.91100393152600E+00 0.32012374755178E+03 0.47596469743138E+03 0.42864079142036E+03 0.41466478603609E+03
 0.22999999533407E+00 0.00000000000000E+00 0.81588284970320E-01 0.00000000000000E+00 -.85570487654507E+01
 0.94689757053933E-03 0.19211690072236E+01 0.80000000000000E+04 0.30000000000000E+04 0.41641313023060E+01
 0.15615492383648E+01 0.33066288537874E+03 0.30315000145234E+03 0.32890767834692E+03 0.37065727202519E+03
 0.30315000011872E+03 0.30315000031275E+03 0.32735096934550E+03 0.37056336213255E+03 0.30315000009668E+03
 0.30315000031160E+03 0.32890767834692E+03 0.37065727202519E+03 0.30315000011872E+03 0.30315000031275E+03
 0.32735096934550E+03 0.37056336213255E+03 0.30315000009668E+03 0.30315000031160E+03 0.42168555727506E+03
 0.31311548782718E+03 0.15930223871365E+04 0.15534861336506E+04 0.97324819239697E+03 0.15072666113977E+04
 0.52915217803878E+03 0.10074958998190E+04 0.16321832144391E+04 0.98149927201239E+03 0.24275041119752E+04
 0.95609336450829E+03 0.16316320892165E+04 0.93592653075802E+03 0.24278312388347E+04 0.10074958998190E+04
 0.16321832144391E+04 0.98149927201240E+03 0.24275041119753E+04 0.95609336450829E+03 0.16316320892165E+04
 0.93592653075802E+03 0.24278312388347E+04 0.19243532207760E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.68069081586751E+05 0.50050000000000E+08 0.56193643473401E+03 0.12369793192785E+01
 0.12369793192785E+01 0.15894432644878E+01 0.45272090223667E+00 0.30653595930759E+03 0.31535679523703E+03
 0.31269454403673E+03 0.31264151753454E+03 0.23000000000000E+00 0.00000000000000E+00 0.20581184613784E+00
 0.00000000000000E+00 -.10248520869342E+00 0.98861762890570E-03 0.47240969331772E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16934453532941E+02 0.63504200748530E+01 0.31311680026250E+03 0.42170340046577E+03
 0.30411294147038E+03 0.30590995753095E+03 0.30315000000187E+03 0.30315000003448E+03 0.30410108457327E+03
 0.30590685889979E+03 0.30315000000184E+03 0.30315000003449E+03 0.30411294147038E+03 0.30590995753095E+03
 0.30315000000187E+03 0.30315000003448E+03 0.30410108457327E+03 0.30590685889979E+03 0.30315000000184E+03
 0.30315000003449E+03 0.30552383457900E+03 0.30315000033636E+03 -.19410676632070E+02 -.41206962746362E+02
 0.45117239163751E+02 0.10864383896779E+03 0.63301013608221E+02 0.46034667675575E+02 0.36691583300821E+02
 0.51448876441615E+02 0.83417910145125E+02 0.45377317145097E+02 0.36440222610776E+02 0.50825720203018E+02
 0.83184828476598E+02 0.46034667675578E+02 0.36691583300825E+02 0.51448876441618E+02 0.83417910145133E+02
 0.45377317145098E+02 0.36440222610784E+02 0.50825720203019E+02 0.83184828476614E+02 0.38273515987177E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32919322244359E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37073071953036E-02 0.20291535939698E-01 0.00000000000000E+00 0.37073071953036E-02 0.20291535939698E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27958442528521E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.66143984034677E-04 0.27958442528521E-01 0.66143984034677E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.17336870796195E-02 0.67143077913532E-01 0.35610326393827E-01
 0.00000000000000E+00 0.67143077913532E-01 0.37344013473446E-01 0.41386045111833E+00 0.88860448138102E-01
 0.32500000298023E+00 0.42250006586313E+00
    454.59595849
 0.91109690095181E+00 0.32069145658240E+03 0.47657518538419E+03 0.42923345797748E+03 0.41527122286996E+03
 0.22999999538478E+00 0.00000000000000E+00 0.77355704683153E-01 0.00000000000000E+00 -.85972908881974E+01
 0.94522093497606E-03 0.19729669559039E+01 0.80000000000000E+04 0.30000000000000E+04 0.40548068866845E+01
 0.15205525825067E+01 0.33125761231296E+03 0.30315000199622E+03 0.32946017729611E+03 0.37179472693976E+03
 0.30315000016924E+03 0.30315000044593E+03 0.32788483394491E+03 0.37170437395237E+03 0.30315000013818E+03
 0.30315000044430E+03 0.32946017729611E+03 0.37179472693975E+03 0.30315000016924E+03 0.30315000044593E+03
 0.32788483394491E+03 0.37170437395237E+03 0.30315000013818E+03 0.30315000044430E+03 0.42295967749495E+03
 0.31381472532939E+03 0.16020619994622E+04 0.15624399081783E+04 0.96974505981360E+03 0.14957988674183E+04
 0.52120508230559E+03 0.10135905451696E+04 0.16360642546858E+04 0.98767893762901E+03 0.24259854245663E+04
 0.96192760032099E+03 0.16356409000069E+04 0.94190393297830E+03 0.24264052461119E+04 0.10135905451696E+04
 0.16360642546858E+04 0.98767893762901E+03 0.24259854245663E+04 0.96192760032099E+03 0.16356409000069E+04
 0.94190393297830E+03 0.24264052461120E+04 0.19242149644414E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.68644180003335E+05 0.50050000000000E+08 0.56254417938121E+03 0.12364905878061E+01
 0.12364905878061E+01 0.16386864293496E+01 0.45123454993993E+00 0.30688655678909E+03 0.31545807692735E+03
 0.31287956623942E+03 0.31282963156005E+03 0.23000000000000E+00 0.00000000000000E+00 0.20471560061436E+00
 0.00000000000000E+00 -.10850928659117E+00 0.98748812964006E-03 0.49367807499414E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16204892226771E+02 0.60768345850392E+01 0.31381616952631E+03 0.42297601553463E+03
 0.30417052049661E+03 0.30595607507760E+03 0.30315000000272E+03 0.30315000004875E+03 0.30415797002555E+03
 0.30595269579646E+03 0.30315000000268E+03 0.30315000004877E+03 0.30417052049661E+03 0.30595607507760E+03
 0.30315000000272E+03 0.30315000004875E+03 0.30415797002555E+03 0.30595269579646E+03 0.30315000000268E+03
 0.30315000004877E+03 0.30556559167571E+03 0.30315000045815E+03 -.22552406758779E+02 -.45878974992375E+02
 0.46903907579811E+02 0.11096541880940E+03 0.63826991691694E+02 0.48213198378641E+02 0.38180366232839E+02
 0.54485627072508E+02 0.85262205314539E+02 0.47517445485431E+02 0.37916842946707E+02 0.53827302607300E+02
 0.85018651647622E+02 0.48213198378643E+02 0.38180366232842E+02 0.54485627072511E+02 0.85262205314546E+02
 0.47517445485431E+02 0.37916842946713E+02 0.53827302607301E+02 0.85018651647633E+02 0.38377717870913E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32945650409314E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35824448780861E-02 0.20575965267738E-01 0.00000000000000E+00 0.35824448780861E-02 0.20575965267738E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28242468961831E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.52995353363280E-04 0.28242468961831E-01 0.52995353363280E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16820783451501E-02 0.67761468039199E-01 0.35838698953640E-01
 0.00000000000000E+00 0.67761468039199E-01 0.37520777298790E-01 0.41311727496996E+00 0.88117271989732E-01
 0.32500000298023E+00 0.42250006586313E+00
    460.75135410
 0.91114622109531E+00 0.32097261665198E+03 0.47687786475513E+03 0.42952703886910E+03 0.41557152870623E+03
 0.22999999540737E+00 0.00000000000000E+00 0.75255829141801E-01 0.00000000000000E+00 -.86173019421721E+01
 0.94439277007746E-03 0.19986191761917E+01 0.80000000000000E+04 0.30000000000000E+04 0.40027635556083E+01
 0.15010363333531E+01 0.33155332805679E+03 0.30315000232856E+03 0.32973518906119E+03 0.37235608217474E+03
 0.30315000020093E+03 0.30315000052951E+03 0.32815044246787E+03 0.37226742953320E+03 0.30315000016426E+03
 0.30315000052758E+03 0.32973518906119E+03 0.37235608217474E+03 0.30315000020093E+03 0.30315000052951E+03
 0.32815044246787E+03 0.37226742953320E+03 0.30315000016426E+03 0.30315000052758E+03 0.42358369825868E+03
 0.31416938164729E+03 0.16065471311964E+04 0.15668767076973E+04 0.96798535062180E+03 0.14901898991349E+04
 0.51736462175999E+03 0.10166226063539E+04 0.16379079871141E+04 0.99074770555558E+03 0.24251816898800E+04
 0.96482211370240E+03 0.16375424826259E+04 0.94486479603773E+03 0.24256426195001E+04 0.10166226063539E+04
 0.16379079871142E+04 0.99074770555559E+03 0.24251816898800E+04 0.96482211370241E+03 0.16375424826259E+04
 0.94486479603773E+03 0.24256426195001E+04 0.19241417207188E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.68949268865357E+05 0.50050000000000E+08 0.56284542640086E+03 0.12362312948547E+01
 0.12362312948547E+01 0.16633080117804E+01 0.45056193614075E+00 0.30706805124687E+03 0.31551427252642E+03
 0.31297724198456E+03 0.31292881966122E+03 0.23000000000000E+00 0.00000000000000E+00 0.20415432204584E+00
 0.00000000000000E+00 -.11161270213170E+00 0.98690443325614E-03 0.50455171646429E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15855659071108E+02 0.59458721516654E+01 0.31417089123555E+03 0.42359931622080E+03
 0.30420051774710E+03 0.30598011608508E+03 0.30315000000327E+03 0.30315000005764E+03 0.30418760813487E+03
 0.30597659656885E+03 0.30315000000321E+03 0.30315000005766E+03 0.30420051774710E+03 0.30598011608508E+03
 0.30315000000327E+03 0.30315000005764E+03 0.30418760813487E+03 0.30597659656885E+03 0.30315000000321E+03
 0.30315000005766E+03 0.30558726836678E+03 0.30315000053195E+03 -.24146253339052E+02 -.48241156016789E+02
 0.47801464134143E+02 0.11216756810732E+03 0.64127096652509E+02 0.49323585190704E+02 0.38933574530010E+02
 0.56050489537887E+02 0.86221349649123E+02 0.48608230353759E+02 0.38664257208892E+02 0.55374252607601E+02
 0.85972851304938E+02 0.49323585190706E+02 0.38933574530012E+02 0.56050489537890E+02 0.86221349649127E+02
 0.48608230353760E+02 0.38664257208898E+02 0.55374252607602E+02 0.85972851304948E+02 0.38444737947772E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32959461233960E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35183617795880E-02 0.20723195213537E-01 0.00000000000000E+00 0.35183617795880E-02 0.20723195213537E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28390441174675E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.46928996059199E-04 0.28390441174675E-01 0.46928996059199E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16517336173311E-02 0.68079139556517E-01 0.35963554479178E-01
 0.00000000000000E+00 0.68079139556517E-01 0.37615288096509E-01 0.41278096807038E+00 0.87780965090145E-01
 0.32500000298023E+00 0.42250006586313E+00
    473.06214532
 0.91125030252410E+00 0.32152958351416E+03 0.47747771439104E+03 0.43010845391117E+03 0.41616614729193E+03
 0.22999999546171E+00 0.00000000000000E+00 0.71089650266542E-01 0.00000000000000E+00 -.86570909339712E+01
 0.94275648356084E-03 0.20494255285182E+01 0.80000000000000E+04 0.30000000000000E+04 0.39035329113834E+01
 0.14638248417688E+01 0.33214160722295E+03 0.30315000313858E+03 0.33028285359452E+03 0.37346449716622E+03
 0.30315000028025E+03 0.30315000073881E+03 0.32867912239814E+03 0.37337909692933E+03 0.30315000022969E+03
 0.30315000073611E+03 0.33028285359452E+03 0.37346449716622E+03 0.30315000028025E+03 0.30315000073881E+03
 0.32867912239814E+03 0.37337909692933E+03 0.30315000022969E+03 0.30315000073611E+03 0.42480678529063E+03
 0.31488790193647E+03 0.16154484640798E+04 0.15756700250810E+04 0.96445160658544E+03 0.14792046176978E+04
 0.50993075307940E+03 0.10226559643042E+04 0.16414100080081E+04 0.99684260522844E+03 0.24234855637354E+04
 0.97056649293636E+03 0.16411493642662E+04 0.95073120099246E+03 0.24240193757890E+04 0.10226559643042E+04
 0.16414100080081E+04 0.99684260522844E+03 0.24234855637354E+04 0.97056649293636E+03 0.16411493642662E+04
 0.95073120099246E+03 0.24240193757890E+04 0.19239831644979E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.69593104882932E+05 0.50050000000000E+08 0.56344230426159E+03 0.12356840476998E+01
 0.12356840476998E+01 0.17125511766422E+01 0.44935458325160E+00 0.30744265724968E+03 0.31563737718437E+03
 0.31318248721029E+03 0.31313701847022E+03 0.23000000000000E+00 0.00000000000000E+00 0.20300533151239E+00
 0.00000000000000E+00 -.11798893228683E+00 0.98570185924250E-03 0.52677880967773E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15186639730049E+02 0.56949898987686E+01 0.31488954090185E+03 0.42482102592702E+03
 0.30426293890652E+03 0.30603012616797E+03 0.30315000000465E+03 0.30315000007974E+03 0.30424928660222E+03
 0.30602632722667E+03 0.30315000000457E+03 0.30315000007977E+03 0.30426293890652E+03 0.30603012616797E+03
 0.30315000000465E+03 0.30315000007974E+03 0.30424928660222E+03 0.30602632722667E+03 0.30315000000457E+03
 0.30315000007977E+03 0.30563218008601E+03 0.30315000071027E+03 -.27375558301691E+02 -.53012058869435E+02
 0.49603427300409E+02 0.11465072325801E+03 0.64799278821104E+02 0.51584708444132E+02 0.40456769703253E+02
 0.59269745322267E+02 0.88210962215341E+02 0.50829375048051E+02 0.40176470411824E+02 0.58557054108832E+02
 0.87953184334091E+02 0.51584708444134E+02 0.40456769703255E+02 0.59269745322271E+02 0.88210962215345E+02
 0.50829375048052E+02 0.40176470411830E+02 0.58557054108833E+02 0.87953184334102E+02 0.38608307689692E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32988350398761E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.33872075323173E-02 0.21027414368489E-01 0.00000000000000E+00 0.33872075323173E-02 0.21027414368489E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28698794935883E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.35866469228140E-04 0.28698794935883E-01 0.35866469228140E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15822475519375E-02 0.68731041121258E-01 0.36233172977089E-01
 0.00000000000000E+00 0.68731041121258E-01 0.37815420529027E-01 0.41217729162580E+00 0.87177288645568E-01
 0.32500000298023E+00 0.42250006586313E+00
    485.37293653
 0.91136155324969E+00 0.32207946702978E+03 0.47806988073660E+03 0.43068199216063E+03 0.41675263065715E+03
 0.22999999551318E+00 0.00000000000000E+00 0.66969227435007E-01 0.00000000000000E+00 -.86965628707367E+01
 0.94114655682231E-03 0.20995610427641E+01 0.80000000000000E+04 0.30000000000000E+04 0.38103202703113E+01
 0.14288701013668E+01 0.33272585492882E+03 0.30315000418043E+03 0.33082750870020E+03 0.37455443296698E+03
 0.30315000038575E+03 0.30315000101736E+03 0.32920455389469E+03 0.37447209779149E+03 0.30315000031694E+03
 0.30315000101363E+03 0.33082750870020E+03 0.37455443296697E+03 0.30315000038575E+03 0.30315000101736E+03
 0.32920455389469E+03 0.37447209779148E+03 0.30315000031694E+03 0.30315000101363E+03 0.42599810711354E+03
 0.31561759513232E+03 0.16242626571387E+04 0.15843604957458E+04 0.96090093250244E+03 0.14685082844588E+04
 0.50280284729383E+03 0.10286511415027E+04 0.16446762469587E+04 0.10028832393310E+04 0.24216737021185E+04
 0.97625441237601E+03 0.16445074815255E+04 0.95652670811379E+03 0.24222693111904E+04 0.10286511415027E+04
 0.16446762469587E+04 0.10028832393310E+04 0.24216737021185E+04 0.97625441237601E+03 0.16445074815255E+04
 0.95652670811379E+03 0.24222693111904E+04 0.19238062286769E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.70281289362839E+05 0.50050000000000E+08 0.56403136824820E+03 0.12350990078487E+01
 0.12350990078487E+01 0.17617943415040E+01 0.44832628510010E+00 0.30783169727863E+03 0.31577429601215E+03
 0.31340037882266E+03 0.31335776325342E+03 0.23000000000000E+00 0.00000000000000E+00 0.20182090734429E+00
 0.00000000000000E+00 -.12457381837029E+00 0.98445604779303E-03 0.54964727698875E+00 0.80000000000000E+04
 0.30000000000000E+04 0.14554788743480E+02 0.54580457788048E+01 0.31561936171190E+03 0.42601105022698E+03
 0.30432860621412E+03 0.30608267072018E+03 0.30315000000652E+03 0.30315000010885E+03 0.30431417904217E+03
 0.30607859486603E+03 0.30315000000642E+03 0.30315000010888E+03 0.30432860621412E+03 0.30608267072018E+03
 0.30315000000652E+03 0.30315000010885E+03 0.30431417904217E+03 0.30607859486603E+03 0.30315000000642E+03
 0.30315000010888E+03 0.30567912912519E+03 0.30315000093699E+03 -.30654634743166E+02 -.57837789151602E+02
 0.51412763022868E+02 0.11723454175287E+03 0.65564714914883E+02 0.53897288194942E+02 0.42001427248032E+02
 0.62603559717515E+02 0.90292576039099E+02 0.53100976329641E+02 0.41710990159182E+02 0.61853608425600E+02
 0.90026361943724E+02 0.53897288194944E+02 0.42001427248034E+02 0.62603559717517E+02 0.90292576039102E+02
 0.53100976329641E+02 0.41710990159187E+02 0.61853608425600E+02 0.90026361943733E+02 0.38810955211459E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33018885764503E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.32523288329581E-02 0.21344639263618E-01 0.00000000000000E+00 0.32523288329581E-02 0.21344639263618E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29023885122511E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.26298500382359E-04 0.29023885122511E-01 0.26298500382359E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15014769510445E-02 0.69404332144423E-01 0.36527715121794E-01
 0.00000000000000E+00 0.69404332144423E-01 0.38029192072838E-01 0.41166314255005E+00 0.86663139569819E-01
 0.32500000298023E+00 0.42250006586313E+00
    491.52833214
 0.91141988433487E+00 0.32235177339878E+03 0.47836295416164E+03 0.43096572338637E+03 0.41704276377090E+03
 0.22999999553789E+00 0.00000000000000E+00 0.64926557431472E-01 0.00000000000000E+00 -.87161770113564E+01
 0.94035134207063E-03 0.21243746253676E+01 0.80000000000000E+04 0.30000000000000E+04 0.37658141386506E+01
 0.14121803019940E+01 0.33301653032192E+03 0.30315000480444E+03 0.33109875904303E+03 0.37509268146525E+03
 0.30315000045046E+03 0.30315000118830E+03 0.32946610131321E+03 0.37501181244838E+03 0.30315000037056E+03
 0.30315000118395E+03 0.33109875904303E+03 0.37509268146524E+03 0.30315000045046E+03 0.30315000118830E+03
 0.32946610131321E+03 0.37501181244838E+03 0.30315000037056E+03 0.30315000118395E+03 0.42658238240777E+03
 0.31598623413046E+03 0.16286390664612E+04 0.15886689456033E+04 0.95911948569416E+03 0.14632604365808E+04
 0.49934535345813E+03 0.10316357243460E+04 0.16462243781983E+04 0.10058844220209E+04 0.24207245016635E+04
 0.97907844366521E+03 0.16460971835092E+04 0.95939904418789E+03 0.24213473111290E+04 0.10316357243460E+04
 0.16462243781983E+04 0.10058844220209E+04 0.24207245016635E+04 0.97907844366521E+03 0.16460971835092E+04
 0.95939904418789E+03 0.24213473111291E+04 0.19237097235565E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.70642118896616E+05 0.50050000000000E+08 0.56432284511040E+03 0.12347922163335E+01
 0.12347922163335E+01 0.17864159239349E+01 0.44787741437824E+00 0.30803122275873E+03 0.31584776784730E+03
 0.31351386517821E+03 0.31347263646812E+03 0.23000000000000E+00 0.00000000000000E+00 0.20121533600497E+00
 0.00000000000000E+00 -.12793791922831E+00 0.98381833439342E-03 0.56132263071714E+00 0.80000000000000E+04
 0.30000000000000E+04 0.14252053208294E+02 0.53445199531101E+01 0.31598806387022E+03 0.42659470540491E+03
 0.30436265888222E+03 0.30610988193697E+03 0.30315000000768E+03 0.30315000012658E+03 0.30434783232452E+03
 0.30610566896792E+03 0.30315000000756E+03 0.30315000012661E+03 0.30436265888222E+03 0.30610988193697E+03
 0.30315000000768E+03 0.30315000012658E+03 0.30434783232452E+03 0.30610566896792E+03 0.30315000000756E+03
 0.30315000012661E+03 0.30570335416816E+03 0.30315000107162E+03 -.32310761441145E+02 -.60268728522485E+02
 0.52319617396382E+02 0.11856266682791E+03 0.65981451344543E+02 0.55072027598006E+02 0.42781504631042E+02
 0.64311516020557E+02 0.91366813853464E+02 0.54254863293297E+02 0.42486326856497E+02 0.63542644349344E+02
 0.91096707442964E+02 0.55072027598007E+02 0.42781504631043E+02 0.64311516020558E+02 0.91366813853466E+02
 0.54254863293298E+02 0.42486326856501E+02 0.63542644349346E+02 0.91096707442972E+02 0.38926814356726E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33034756454618E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.31836012284479E-02 0.21508107910816E-01 0.00000000000000E+00 0.31836012284479E-02 0.21508107910816E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29192834704949E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22090459682392E-04 0.29192834704949E-01 0.22090459682392E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14570399419399E-02 0.69748781846466E-01 0.36683693046580E-01
 0.00000000000000E+00 0.69748781846466E-01 0.38140732988520E-01 0.41143870718912E+00 0.86438704208886E-01
 0.32500000298023E+00 0.42250006586313E+00
    500.00000000
 0.91150260338499E+00 0.32272371388561E+03 0.47876355693827E+03 0.43135331588017E+03 0.41743901391512E+03
 0.22999999557020E+00 0.00000000000000E+00 0.62134727947098E-01 0.00000000000000E+00 -.87427910062239E+01
 0.93926733631612E-03 0.21582429750747E+01 0.80000000000000E+04 0.30000000000000E+04 0.37067188877207E+01
 0.13900195828953E+01 0.33341691026567E+03 0.30315000578271E+03 0.33147282045630E+03 0.37582587689379E+03
 0.30315000055369E+03 0.30315000146109E+03 0.32982663832020E+03 0.37574697368310E+03 0.30315000045622E+03
 0.30315000145574E+03 0.33147282045630E+03 0.37582587689379E+03 0.30315000055369E+03 0.30315000146109E+03
 0.32982663832020E+03 0.37574697368310E+03 0.30315000045622E+03 0.30315000145574E+03 0.42736740405854E+03
 0.31649335497314E+03 0.16346219527603E+04 0.15945423799524E+04 0.95678871470833E+03 0.14563569705275E+04
 0.49478431224565E+03 0.10357237215739E+04 0.16482839044942E+04 0.10099814180208E+04 0.24193991966358E+04
 0.98294050197033E+03 0.16482093913551E+04 0.96331513704034E+03 0.24200554497736E+04 0.10357237215739E+04
 0.16482839044942E+04 0.10099814180208E+04 0.24193991966358E+04 0.98294050197033E+03 0.16482093913551E+04
 0.96331513704034E+03 0.24200554497737E+04 0.19236244989130E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.71153809638537E+05 0.50050000000000E+08 0.56472120854840E+03 0.12343570111843E+01
 0.12343570111843E+01 0.18203025953703E+01 0.44732805172198E+00 0.30831105731240E+03 0.31595429918268E+03
 0.31367494151957E+03 0.31363557710269E+03 0.23000000000000E+00 0.00000000000000E+00 0.20036710827022E+00
 0.00000000000000E+00 -.13260595506755E+00 0.98292533244424E-03 0.57765753418581E+00 0.80000000000000E+04
 0.30000000000000E+04 0.13849036023179E+02 0.51933885086921E+01 0.31649527503148E+03 0.42737889290364E+03
 0.30441188160604E+03 0.30614897731942E+03 0.30315000000955E+03 0.30315000015470E+03 0.30439648263923E+03
 0.30614457531855E+03 0.30315000000940E+03 0.30315000015475E+03 0.30441188160604E+03 0.30614897731942E+03
 0.30315000000955E+03 0.30315000015470E+03 0.30439648263923E+03 0.30614457531855E+03 0.30315000000940E+03
 0.30315000015475E+03 0.30573804216795E+03 0.30315000128132E+03 -.34574180285059E+02 -.63580239985974E+02
 0.53568732250761E+02 0.12042886144489E+03 0.66592285532874E+02 0.56706241102167E+02 0.43861731854358E+02
 0.66700244029852E+02 0.92879527878205E+02 0.55860609641107E+02 0.43560409382013E+02 0.65905672867769E+02
 0.92604453852792E+02 0.56706241102168E+02 0.43861731854358E+02 0.66700244029853E+02 0.92879527878205E+02
 0.55860609641108E+02 0.43560409382016E+02 0.65905672867771E+02 0.92604453852798E+02 0.39113782171318E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33058109262518E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.30938521885757E-02 0.21724695295870E-01 0.00000000000000E+00 0.30938521885757E-02 0.21724695295870E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29460369515268E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16443545425908E-04 0.29460369515268E-01 0.16443545425908E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.13899180016978E-02 0.70253501127147E-01 0.36889110776573E-01
 0.00000000000000E+00 0.70253501127147E-01 0.38279028778271E-01 0.41116402586099E+00 0.86164022880757E-01
 0.32500000298023E+00 0.42250006586313E+00
