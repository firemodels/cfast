#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-47 0 MONOZONE(1=OUI,0=NON)                                                                     
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
3.000000 2.000000 0.600000 0.560000                                                                 
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
2000.000000 0.004000                                                                                
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
Plenum-LLNL47 0 MONOZONE(1=OUI,0=NON)                                                               
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
#CONDINIT 500.000000 10.000000 19.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-47           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-47           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-47           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-47           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-47           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-47           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-47           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-47           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-47           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-47           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-47           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-47           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-47           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-47           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-47           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-47           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-47           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-47           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL47     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL47     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL47     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL47     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL47     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL47     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL47     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL47     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL47     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL47     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL47     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL47     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL47     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL47     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL47     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL47     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL47     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL47     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.75885538859780E-06
 0.10000000000075E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.45140398709315E-03 0.45140398709315E-03 0.63936865799117E-03 0.64256550128112E-03
 0.00000000000000E+00 0.42775973175097E-03 0.50340234270621E-03 0.42775973175097E-03 0.50340234270621E-03
 0.42559438695707E-03 0.50339631950060E-03 0.42559438695707E-03 0.50339631950060E-03 0.42775973175097E-03
 0.50340234276351E-03 0.42775973175097E-03 0.50340234276351E-03 0.42559438684247E-03 0.50339631944330E-03
 0.42559438684247E-03 0.50339631944330E-03 0.51868883349612E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.37286472792578E-06 0.99964766651900E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.52548288844977E-03 0.52548288844977E-03
 0.64523867808258E-03 0.64846487147299E-03 0.00000000000000E+00 0.45921118148946E-03 0.50664812476676E-03
 0.45921118148946E-03 0.50664812476676E-03 0.45520776334017E-03 0.50664116888882E-03 0.45520776334017E-03
 0.50664116888882E-03 0.45921118148946E-03 0.50664812482406E-03 0.45921118148946E-03 0.50664812482406E-03
 0.45520776328287E-03 0.50664116894612E-03 0.45520776328287E-03 0.50664116894612E-03 0.41903040664695E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47819439850300E-04 0.47819439850300E-04 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29945843494270E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29945843494270E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.22011911537327E-01 0.22011911537327E-01 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     20.00000000
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20750829083021E-02
 0.99999997952052E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000198513E+03 0.29215000000000E+03 0.29215000272945E+03 0.29215000272945E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000271534E+03 0.29215000271534E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000272945E+03 0.29215000272945E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000271534E+03 0.29215000271534E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000946493E+03
 0.29215000781637E+03 0.46641678658779E-03 0.46641678658779E-03 0.60530126566349E-03 0.60832777199181E-03
 .00000000000000E+00 0.43592072399714E-03 0.51885304002666E-03 0.43592072399714E-03 0.51885304002666E-03
 0.43366047525228E-03 0.51891846736044E-03 0.43366047525228E-03 0.51891846736044E-03 0.43592072439824E-03
 0.51885304042776E-03 0.43592072439824E-03 0.51885304042776E-03 0.43366047525228E-03 0.51891846741774E-03
 0.43366047525228E-03 0.51891846741774E-03 0.51894866883082E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22934392957650E-02 0.99964804916171E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000779586E+03 0.29215000948983E+03
 0.29215000292834E+03 0.29215000292834E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000290260E+03
 0.29215000290260E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000292834E+03 0.29215000292834E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000290260E+03 0.29215000290260E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000282223E+03 0.29215000000000E+03 0.50160770568400E-03 0.50160770568400E-03
 0.65882851581349E-03 0.66212265839256E-03 .00000000000000E+00 0.46765211782620E-03 0.51361966377590E-03
 0.46765211782620E-03 0.51361966377590E-03 0.46353731160053E-03 0.51374353380091E-03 0.46353731160053E-03
 0.51374353380091E-03 0.46765211822730E-03 0.51361966406240E-03 0.46765211822730E-03 0.51361966406240E-03
 0.46353731165783E-03 0.51374353385821E-03 0.46353731165783E-03 0.51374353385821E-03 0.41909714601869E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25005946902235E-02 0.00000000000000E+00 0.00000000000000E+00 0.25005946902235E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004726389297E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004726389297E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42688856197784E-02
 0.67692708118493E-02 0.67692708118493E-02 0.42688856197784E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     30.00000000
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20750829010212E-02
 0.99999997952053E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000248502E+03 0.29215000000000E+03 0.29215000340853E+03 0.29215000340853E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000339085E+03 0.29215000339085E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000340853E+03 0.29215000340853E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000339085E+03 0.29215000339085E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001167136E+03
 0.29215000964566E+03 0.46981767084836E-03 0.46981767084836E-03 0.59748073215937E-03 0.60046813582017E-03
 .00000000000000E+00 0.43769745740068E-03 0.52230399046127E-03 0.43769745740068E-03 0.52230399046127E-03
 0.43541772074904E-03 0.52238751961447E-03 0.43541772074904E-03 0.52238751961447E-03 0.43769745780178E-03
 0.52230399080507E-03 0.43769745780178E-03 0.52230399080507E-03 0.43541772074904E-03 0.52238751955717E-03
 0.43541772074904E-03 0.52238751955717E-03 0.51893993131666E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22934392959767E-02 0.99964825132275E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215000962788E+03 0.29215001169289E+03
 0.29215000365661E+03 0.29215000365661E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000362443E+03
 0.29215000362443E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000365661E+03 0.29215000365661E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000362443E+03 0.29215000362443E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000352572E+03 0.29215000000000E+03 0.49615180883854E-03 0.49615180883854E-03
 0.66183411383239E-03 0.66514328440155E-03 .00000000000000E+00 0.46950987281972E-03 0.51515483424287E-03
 0.46950987281972E-03 0.51515483424287E-03 0.46537239630177E-03 0.51531144778807E-03 0.46537239630177E-03
 0.51531144778807E-03 0.46950987327812E-03 0.51515483470127E-03 0.46950987327812E-03 0.51515483470127E-03
 0.46537239630177E-03 0.51531144778807E-03 0.46537239630177E-03 0.51531144778807E-03 0.41906728708264E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25005946814581E-02 0.00000000000000E+00 0.00000000000000E+00 0.25005946814581E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004727219711E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004727219711E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42688856197784E-02
 0.67692708118493E-02 0.67692708118493E-02 0.42688856197784E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00000000
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20750828950257E-02
 0.99999997952052E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000291253E+03 0.29215000000000E+03 0.29215000398660E+03 0.29215000398660E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000396586E+03 0.29215000396586E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000398660E+03 0.29215000398660E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000396586E+03 0.29215000396586E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001349960E+03
 0.29215001116443E+03 0.47261723563672E-03 0.47261723563672E-03 0.59106852832837E-03 0.59402387097001E-03
 .00000000000000E+00 0.43914298136490E-03 0.52512258630339E-03 0.43914298136490E-03 0.52512258630339E-03
 0.43684815160299E-03 0.52522159614417E-03 0.43684815160299E-03 0.52522159614417E-03 0.43914298170871E-03
 0.52512258670449E-03 0.43914298170871E-03 0.52512258670449E-03 0.43684815148839E-03 0.52522159614417E-03
 0.43684815148839E-03 0.52522159614417E-03 0.51892889530983E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22934393014338E-02 0.99964845316877E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215001115049E+03 0.29215001351641E+03
 0.29215000427647E+03 0.29215000427647E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000423881E+03
 0.29215000423881E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000427647E+03 0.29215000427647E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000423881E+03 0.29215000423881E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000412526E+03 0.29215000000000E+03 0.49168452678893E-03 0.49168452678893E-03
 0.66428336457441E-03 0.66760478139728E-03 .00000000000000E+00 0.47102271083529E-03 0.51640518839904E-03
 0.47102271083529E-03 0.51640518839904E-03 0.46686784823239E-03 0.51658970195153E-03 0.46686784823239E-03
 0.51658970195153E-03 0.47102271083529E-03 0.51640518851364E-03 0.47102271083529E-03 0.51640518851364E-03
 0.46686784846159E-03 0.51658970229533E-03 0.46686784846159E-03 0.51658970229533E-03 0.41903766371668E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25005946814581E-02 0.00000000000000E+00 0.00000000000000E+00 0.25005946814581E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004728050125E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004728050125E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42688857009720E-02
 0.67692707171822E-02 0.67692707171822E-02 0.42688857009720E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00025000
 0.30000000000000E+01 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20750828965523E-02
 0.99999997952052E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29215000291254E+03 0.29215000000000E+03 0.29215000398661E+03 0.29215000398661E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000396588E+03 0.29215000396588E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000398661E+03 0.29215000398661E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29215000396588E+03 0.29215000396588E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215001349965E+03
 0.29215001116447E+03 0.47262261960336E-03 0.47262261960336E-03 0.59107598839113E-03 0.59403136833309E-03
 .00000000000000E+00 0.43914884086437E-03 0.52512686827616E-03 0.43914884086437E-03 0.52512686827616E-03
 0.43685405763007E-03 0.52522587857534E-03 0.43685405763007E-03 0.52522587857534E-03 0.43914884120817E-03
 0.52512686856266E-03 0.43914884120817E-03 0.52512686856266E-03 0.43685405768737E-03 0.52522587857534E-03
 0.43685405768737E-03 0.52522587857534E-03 0.51893551371590E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29215000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29215000000000E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22934392944437E-02 0.99964845317382E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215001115053E+03 0.29215001351646E+03
 0.29215000427649E+03 0.29215000427649E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000423883E+03
 0.29215000423883E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215000427649E+03 0.29215000427649E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215000423883E+03 0.29215000423883E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29215000412528E+03 0.29215000000000E+03 0.49169151183258E-03 0.49169151183258E-03
 0.66429770206427E-03 0.66761919057459E-03 .00000000000000E+00 0.47103256019206E-03 0.51641311069903E-03
 0.47103256019206E-03 0.51641311069903E-03 0.46687786118070E-03 0.51659762453801E-03 0.46687786118070E-03
 0.51659762453801E-03 0.47103256024936E-03 0.51641311069903E-03 0.47103256024936E-03 0.51641311069903E-03
 0.46687786152450E-03 0.51659762493911E-03 0.46687786152450E-03 0.51659762493911E-03 0.41904577277976E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29215000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25005946814581E-02 0.00000000000000E+00 0.00000000000000E+00 0.25005946814581E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004727219711E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25004727219711E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42688856197784E-02
 0.67692708118493E-02 0.67692708118493E-02 0.42688856197784E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     50.11286408
 0.24710988044398E+01 0.29216675324300E+03 0.34335646855098E+03 0.30119152045192E+03 0.30005334288921E+03
 0.22999999958193E+00 0.00000000000000E+00 0.22402859356305E+00 0.00000000000000E+00 0.50056145538593E+01
 0.99999205736817E-03 0.10338130311883E+00 0.80000000000000E+04 0.30000000000000E+04 0.77383431613401E+02
 0.29018786855026E+02 0.29319820874189E+03 0.29215000000000E+03 0.29335837158900E+03 0.29449733410247E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29305857182650E+03 0.29448522117001E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29335837158900E+03 0.29449733410247E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29305857182650E+03 0.29448522117001E+03 0.29215000000000E+03 0.29215000000000E+03 0.29816751437466E+03
 0.29215575570035E+03 0.48206507830866E+03 0.48015395174627E+03 0.35859870738872E+03 0.82402854361837E+03
 0.46363684269270E+03 0.39925722906717E+03 0.23701929469591E+03 0.39738622425023E+03 0.73855623934845E+03
 0.29940252373359E+03 0.23224580449112E+03 0.29812255834138E+03 0.73392779047192E+03 0.39925722906717E+03
 0.23701929469591E+03 0.39738622425023E+03 0.73855623934845E+03 0.29940252373359E+03 0.23224580449112E+03
 0.29812255834138E+03 0.73392779047191E+03 0.56770551374891E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.38070661342264E+03 0.12946676373181E+01
 0.12946676373181E+01 0.20784997371089E-01 0.13269828689158E+01 0.29215053240388E+03 0.30214944410591E+03
 0.29330385441498E+03 0.29326996091090E+03 0.22999999994539E+00 0.00000000000000E+00 0.22867956428768E+00
 0.00000000000000E+00 0.59794701735694E+00 0.99965255583802E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29215574301224E+03 0.29818289784404E+03
 0.29215277960896E+03 0.29244946221865E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215280087909E+03
 0.29244947632435E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215277960896E+03 0.29244946221865E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215280087909E+03 0.29244947632435E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29226751742349E+03 0.29215000000000E+03 0.86639771299820E+00 0.86450909706447E+00
 0.59363338705091E+00 0.67604323274229E+02 0.67007721720243E+02 0.99240619438599E+00 -.57965782041001E+00
 0.99178554869675E+00 0.96908378759878E+02 0.99808447121409E+00 -.57295097549722E+00 0.99745640985425E+00
 0.96914928394199E+02 0.99240619438605E+00 -.57965782040996E+00 0.99178554869681E+00 0.96908378759878E+02
 0.99808447121421E+00 -.57295097549654E+00 0.99745640985437E+00 0.96914928394201E+02 0.20049790856638E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31255029590749E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11969031103365E+00 0.00000000000000E+00 0.00000000000000E+00 0.11969031103365E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29965745391452E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29965745391452E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.34559781859781E+00 0.34559781859781E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     60.13456955
 0.20508018209352E+01 0.29221150914989E+03 0.37322225076629E+03 0.31784325862555E+03 0.31375954388796E+03
 0.22999999965395E+00 0.00000000000000E+00 0.21992101319967E+00 0.00000000000000E+00 0.20304037832448E+01
 0.99980953900725E-03 0.16829376793840E+00 0.80000000000000E+04 0.30000000000000E+04 0.47535925411856E+02
 0.17825972029446E+02 0.29413821720638E+03 0.29215000000000E+03 0.29453842829337E+03 0.29736173357106E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29396232788080E+03 0.29733316781816E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29453842829337E+03 0.29736173357106E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29396232788080E+03 0.29733316781816E+03 0.29215000000000E+03 0.29215000000000E+03 0.30593452871553E+03
 0.29216960421932E+03 0.53697558675673E+03 0.53258199028936E+03 0.38368968237349E+03 0.11022638278048E+04
 0.71665569701945E+03 0.44568589535379E+03 0.32268751373168E+03 0.44113329229657E+03 0.10220056843255E+04
 0.34068460468621E+03 0.31679321668882E+03 0.33754816280628E+03 0.10164293826149E+04 0.44568589535379E+03
 0.32268751373168E+03 0.44113329229657E+03 0.10220056843255E+04 0.34068460468621E+03 0.31679321668882E+03
 0.33754816280628E+03 0.10164293826149E+04 0.74989752806831E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40384544521824E+03 0.12946895517101E+01
 0.12946895517101E+01 0.60871819270051E-01 0.10935785901312E+01 0.29215063645469E+03 0.31001609229941E+03
 0.29699123895626E+03 0.29678464262142E+03 0.22999999995004E+00 0.00000000000000E+00 0.22744591151384E+00
 0.00000000000000E+00 0.42838337740079E+00 0.99965052634639E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29216953710368E+03 0.30598598691860E+03
 0.29215809008737E+03 0.29284507995369E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215808115979E+03
 0.29284518399210E+03 0.29215000000000E+03 0.29215000000000E+03 0.29215809008737E+03 0.29284507995369E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29215808115979E+03 0.29284518399210E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29254804517106E+03 0.29215000000000E+03 0.19208255819188E+01 0.19113713077563E+01
 -.97469972871702E-01 0.13314458920535E+03 0.13324254652809E+03 0.18414370940737E+01 -.15331365744202E+01
 0.18384966458946E+01 0.13698483294929E+03 0.18330291990610E+01 -.15031797625020E+01 0.18300932104237E+01
 0.13701382173972E+03 0.18414370940737E+01 -.15331365744202E+01 0.18384966458945E+01 0.13698483294929E+03
 0.18330291990611E+01 -.15031797625013E+01 0.18300932104238E+01 0.13701382173972E+03 0.43950459772556E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32354858471383E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.96639674542903E-01 0.00000000000000E+00 0.00000000000000E+00 0.96639674542903E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24068708185263E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24068708185263E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29276294299079E+00 0.29276294299079E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     70.05857494
 0.17254008883432E+01 0.29228588898320E+03 0.38672900446424E+03 0.33241159268128E+03 0.32612335374636E+03
 0.22999999964664E+00 0.00000000000000E+00 0.21745622563132E+00 0.00000000000000E+00 -.78138811769358E-01
 0.99953431114646E-03 0.20377599485711E+00 0.80000000000000E+04 0.30000000000000E+04 0.39258794960660E+02
 0.14722048110248E+02 0.29492411289403E+03 0.29215000000000E+03 0.29544669220026E+03 0.29974807296554E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29468483562141E+03 0.29970916348269E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29544669220026E+03 0.29974807296555E+03 0.29215000000000E+03 0.29215000000000E+03
 0.29468483562141E+03 0.29970916348269E+03 0.29215000000000E+03 0.29215000000000E+03 0.31197578477301E+03
 0.29220479709194E+03 0.61519390591629E+03 0.60851747777435E+03 0.41040831932426E+03 0.12177307506832E+04
 0.80527038976227E+03 0.49003919132640E+03 0.39811706801053E+03 0.48316172033185E+03 0.11661461430566E+04
 0.38149794487406E+03 0.39220027012253E+03 0.37670427871651E+03 0.11606496308000E+04 0.49003919132639E+03
 0.39811706801053E+03 0.48316172033185E+03 0.11661461430566E+04 0.38149794487406E+03 0.39220027012253E+03
 0.37670427871651E+03 0.11606496308000E+04 0.88704786560711E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41716173392948E+03 0.18449967115615E+01
 0.18449967115615E+01 0.10056784080034E+00 0.91244244910294E+00 0.29215106592244E+03 0.31376125948155E+03
 0.30061588752378E+03 0.30025141151063E+03 0.22999999995092E+00 0.00000000000000E+00 0.22662625953442E+00
 0.00000000000000E+00 0.32076079426798E+00 0.99964799469136E-03 0.30722033282546E-01 0.80000000000000E+04
 0.30000000000000E+04 0.26039943145772E+03 0.97649786796644E+02 0.29220464453381E+03 0.31202591415135E+03
 0.29216896781105E+03 0.29315851403704E+03 0.29215000000000E+03 0.29215000000000E+03 0.29216876942733E+03
 0.29315874077777E+03 0.29215000000000E+03 0.29215000000000E+03 0.29216896781105E+03 0.29315851403704E+03
 0.29215000000000E+03 0.29215000000000E+03 0.29216876942733E+03 0.29315874077777E+03 0.29215000000000E+03
 0.29215000000000E+03 0.29281577943886E+03 0.29215000000000E+03 0.53627358428574E+01 0.53256711893638E+01
 0.27476677735277E+01 0.16883657868945E+03 0.16607517257705E+03 0.42066223729936E+01 0.13276474994801E+01
 0.41973684581005E+01 0.15406220360317E+03 0.41433958868319E+01 0.13754854823143E+01 0.41342719430285E+01
 0.15410806861127E+03 0.42066223729935E+01 0.13276474994801E+01 0.41973684581005E+01 0.15406220360317E+03
 0.41433958868319E+01 0.13754854823150E+01 0.41342719430285E+01 0.15410806861127E+03 0.60666543720477E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32935218254279E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.83157914105278E-01 0.00000000000000E+00 0.00000000000000E+00 0.83157914105278E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.20712199939719E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.20712199939719E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.47741737847980E-02 0.00000000000000E+00
 0.24867475042343E+00 0.25344892420823E+00 0.00000000000000E+00 0.64372122455147E+00 0.31872122157124E+00
 0.32500000298023E+00 0.42250006586313E+00
     80.03352769
 0.14666902178057E+01 0.29239432618242E+03 0.39634236942325E+03 0.34552251002946E+03 0.33765579895975E+03
 0.22999999967010E+00 0.00000000000000E+00 0.21527356199867E+00 0.00000000000000E+00 -.18436916292474E+01
 0.99914621430937E-03 0.23414835458508E+00 0.80000000000000E+04 0.30000000000000E+04 0.34166372914199E+02
 0.12812389842825E+02 0.29576236351434E+03 0.29215000000001E+03 0.29631541871629E+03 0.30195496510683E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29539069593272E+03 0.30190974933167E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29631541871629E+03 0.30195496510683E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29539069593272E+03 0.30190974933167E+03 0.29215000000001E+03 0.29215000000001E+03 0.31723612824827E+03
 0.29226350360625E+03 0.70956704726873E+03 0.70032742344580E+03 0.43838274777811E+03 0.13002234351231E+04
 0.85964877360611E+03 0.53700244204308E+03 0.46778984088969E+03 0.52778250941638E+03 0.12827495512043E+04
 0.42451072601351E+03 0.46228635226302E+03 0.41802437105243E+03 0.12777271038394E+04 0.53700244204308E+03
 0.46778984088969E+03 0.52778250941638E+03 0.12827495512043E+04 0.42451072601351E+03 0.46228635226302E+03
 0.41802437105243E+03 0.12777271038394E+04 0.10148268093449E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42865116940719E+03 0.16520134548573E+01
 0.16520134548573E+01 0.14046765180641E+00 0.77913707284996E+00 0.29215215762487E+03 0.31605002433014E+03
 0.30363688172207E+03 0.30316879977733E+03 0.23000000000000E+00 0.00000000000000E+00 0.22592987291747E+00
 0.00000000000000E+00 0.22006331351783E+00 0.99964326544662E-03 0.51509326091754E-01 0.80000000000000E+04
 0.30000000000000E+04 0.15531168056343E+03 0.58241880211286E+02 0.29226318339154E+03 0.31727503741084E+03
 0.29218611110078E+03 0.29343653524243E+03 0.29215000000001E+03 0.29215000000001E+03 0.29218558009424E+03
 0.29343688661668E+03 0.29215000000001E+03 0.29215000000001E+03 0.29218611110078E+03 0.29343653524243E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29218558009424E+03 0.29343688661668E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29306615772514E+03 0.29215000000001E+03 0.93106101294011E+01 0.92126942954088E+01
 0.63210041411342E+01 0.19207129631933E+03 0.18571868715749E+03 0.69016007629501E+01 0.49085090720580E+01
 0.68801025878751E+01 0.16513153694354E+03 0.67818868926080E+01 0.49680659248644E+01 0.67608150372141E+01
 0.16518814200479E+03 0.69016007629501E+01 0.49085090720580E+01 0.68801025878751E+01 0.16513153694354E+03
 0.67818868926081E+01 0.49680659248648E+01 0.67608150372142E+01 0.16518814200479E+03 0.73183536013932E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33301453307177E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.71682741601964E-01 0.00000000000000E+00 0.00000000000000E+00 0.71682741601964E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.17946791165101E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.17946791165101E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.51605899741960E-01 0.00000000000000E+00
 0.16309430271766E+00 0.21470020245962E+00 0.00000000000000E+00 0.57706853642498E+00 0.25206853344475E+00
 0.32500000298023E+00 0.42250006586313E+00
     90.76320893
 0.12497401117071E+01 0.29255558341813E+03 0.40483800194777E+03 0.35806338785578E+03 0.34903353346890E+03
 0.22999999975403E+00 0.00000000000000E+00 0.21293446036011E+00 0.00000000000000E+00 -.34327613612933E+01
 0.99857982171632E-03 0.26606244059314E+00 0.80000000000000E+04 0.30000000000000E+04 0.30068129805039E+02
 0.11275548676890E+02 0.29670816803880E+03 0.29215000000001E+03 0.29723751971751E+03 0.30424744543585E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29615687609569E+03 0.30419803968011E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29723751971751E+03 0.30424744543585E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29615687609569E+03 0.30419803968011E+03 0.29215000000001E+03 0.29215000000001E+03 0.32248391340130E+03
 0.29234316957621E+03 0.81154942843224E+03 0.79934463362381E+03 0.47677315192596E+03 0.13806279059561E+04
 0.90147088827046E+03 0.58777639058368E+03 0.54434786753179E+03 0.57601881505724E+03 0.13961502146020E+04
 0.47208583010734E+03 0.53934789674361E+03 0.46373985802882E+03 0.13916688315541E+04 0.58777639058368E+03
 0.54434786753179E+03 0.57601881505724E+03 0.13961502146020E+04 0.47208583010734E+03 0.53934789674361E+03
 0.46373985802882E+03 0.13916688315541E+04 0.11440402687807E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44192099593251E+03 0.14980324024869E+01
 0.14980324024869E+01 0.18338637679759E+00 0.68056875891252E+00 0.29215464584760E+03 0.31755617441768E+03
 0.30603118323540E+03 0.30550455269606E+03 0.23000000000000E+00 0.00000000000000E+00 0.22525103980625E+00
 0.00000000000000E+00 0.12541680840018E+00 0.99963381761044E-03 0.69124088408295E-01 0.80000000000000E+04
 0.30000000000000E+04 0.11573389514733E+03 0.43400210680247E+02 0.29234270774638E+03 0.32251536716109E+03
 0.29220922180649E+03 0.29370408711581E+03 0.29215000000001E+03 0.29215000000001E+03 0.29220825913881E+03
 0.29370456078074E+03 0.29215000000001E+03 0.29215000000001E+03 0.29220922180649E+03 0.29370408711581E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29220825913881E+03 0.29370456078074E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29331239894253E+03 0.29215000000001E+03 0.13451213882620E+02 0.13253517495503E+02
 0.10145122817605E+02 0.20781862254650E+03 0.19762277411480E+03 0.97660373125286E+01 0.87085424475663E+01
 0.97257322844168E+01 0.17257359485246E+03 0.95939785075954E+01 0.87755165863696E+01 0.95545751022721E+01
 0.17263668447091E+03 0.97660373125282E+01 0.87085424475662E+01 0.97257322844163E+01 0.17257359485246E+03
 0.95939785075954E+01 0.87755165863695E+01 0.95545751022720E+01 0.17263668447091E+03 0.82658375591785E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33526878665088E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.60561010390126E-01 0.00000000000000E+00 0.00000000000000E+00 0.60561010390126E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.15347729869635E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.15347729869635E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.78758000904713E-01 0.00000000000000E+00
 0.99363979667226E-01 0.17812198057194E+00 0.00000000000000E+00 0.52778437945626E+00 0.20278437647603E+00
 0.32500000298023E+00 0.42250006586313E+00
    100.03374626
 0.10955494196782E+01 0.29273857859621E+03 0.41063912606633E+03 0.36758383394612E+03 0.35798721943369E+03
 0.22999999924075E+00 0.00000000000000E+00 0.21094250347802E+00 0.00000000000000E+00 -.45627271222255E+01
 0.99794446535392E-03 0.29314345525375E+00 0.80000000000000E+04 0.30000000000000E+04 0.27290392661419E+02
 0.10233897248032E+02 0.29756394931809E+03 0.29215000000001E+03 0.29803749722648E+03 0.30617160218584E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29683447589929E+03 0.30611999663262E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29803749722648E+03 0.30617160218584E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29683447589929E+03 0.30611999663262E+03 0.29215000000001E+03 0.29215000000001E+03 0.32675297410307E+03
 0.29242077974775E+03 0.89830116083108E+03 0.88340380874752E+03 0.51082896053524E+03 0.14330805069617E+04
 0.91969740162376E+03 0.63065554385148E+03 0.60777683062218E+03 0.61670111341545E+03 0.14804428526846E+04
 0.51284509867706E+03 0.60319828149747E+03 0.50287270181479E+03 0.14764006714815E+04 0.63065554385148E+03
 0.60777683062218E+03 0.61670111341546E+03 0.14804428526846E+04 0.51284509867706E+03 0.60319828149747E+03
 0.50287270181479E+03 0.14764006714815E+04 0.12443753723705E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45055177939633E+03 0.13758798026941E+01
 0.13758798026941E+01 0.22046852611476E+00 0.62063562891443E+00 0.29215842905665E+03 0.31827624062142E+03
 0.30746981101717E+03 0.30692367054388E+03 0.23000000000000E+00 0.00000000000000E+00 0.22470530534063E+00
 0.00000000000000E+00 0.63595926646986E-01 0.99962026307503E-03 0.82304668077393E-01 0.80000000000000E+04
 0.30000000000000E+04 0.97199833094247E+02 0.36449937410343E+02 0.29242024614187E+03 0.32678237671691E+03
 0.29223190927436E+03 0.29390982029103E+03 0.29215000000001E+03 0.29215000000001E+03 0.29223054720911E+03
 0.29391038661773E+03 0.29215000000001E+03 0.29215000000001E+03 0.29223190927436E+03 0.29390982029103E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29223054720911E+03 0.29391038661773E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29350436130719E+03 0.29215000000001E+03 0.16682990641304E+02 0.16375684720191E+02
 0.13168815143053E+02 0.21542771640935E+03 0.20219305719058E+03 0.12058899435539E+02 0.11691702845075E+02
 0.11999055630135E+02 0.17568012914332E+03 0.11850169595391E+02 0.11761946571889E+02 0.11791732613635E+02
 0.17574580479100E+03 0.12058899435538E+02 0.11691702845075E+02 0.11999055630134E+02 0.17568012914332E+03
 0.11850169595391E+02 0.11761946571889E+02 0.11791732613635E+02 0.17574580479100E+03 0.88013920275748E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33614885439046E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.52242005545630E-01 0.00000000000000E+00 0.00000000000000E+00 0.52242005545630E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.13445783763298E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.13445783763298E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.90663672540165E-01 0.00000000000000E+00
 0.60747521864064E-01 0.15141119440423E+00 0.00000000000000E+00 0.49781781445721E+00 0.17281781147698E+00
 0.32500000298023E+00 0.42250006586313E+00
    110.46310076
 0.94982914438915E+00 0.29300120947942E+03 0.41586964802714E+03 0.37696830674107E+03 0.36712676530114E+03
 0.22999999832312E+00 0.00000000000000E+00 0.20870820814738E+00 0.00000000000000E+00 -.56144237455546E+01
 0.99703961110498E-03 0.32356369494749E+00 0.80000000000000E+04 0.30000000000000E+04 0.24724652749742E+02
 0.92717447811532E+01 0.29856116602591E+03 0.29215000000001E+03 0.29894079920616E+03 0.30826744892159E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29761137853904E+03 0.30821444043495E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29894079920616E+03 0.30826744892159E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29761137853904E+03 0.30821444043495E+03 0.29215000000001E+03 0.29215000000001E+03 0.33124968749639E+03
 0.29251257843286E+03 0.99298534920169E+03 0.97500930815125E+03 0.54801534360502E+03 0.14795820119739E+04
 0.92882659165081E+03 0.67715527294176E+03 0.67475092470495E+03 0.66078395470561E+03 0.15627556257348E+04
 0.55753966064009E+03 0.67061390457613E+03 0.54577205549550E+03 0.15591645484169E+04 0.67715527294176E+03
 0.67475092470495E+03 0.66078395470561E+03 0.15627556257348E+04 0.55753966064009E+03 0.67061390457613E+03
 0.54577205549550E+03 0.15591645484169E+04 0.13460831780892E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45958206823675E+03 0.12947458650443E+01
 0.12947458650443E+01 0.26218594409658E+00 0.57206291193094E+00 0.29216504127275E+03 0.31856568012089E+03
 0.30849712923002E+03 0.30795304523997E+03 0.23000000000000E+00 0.00000000000000E+00 0.22413451421321E+00
 0.00000000000000E+00 0.16826549154681E-01 0.99959717833069E-03 0.95621128481491E-01 0.80000000000000E+04
 0.30000000000000E+04 0.83663517959302E+02 0.31373819234738E+02 0.29251200515054E+03 0.33127925312296E+03
 0.29225902159781E+03 0.29411234733491E+03 0.29215000000001E+03 0.29215000000001E+03 0.29225720901764E+03
 0.29411300313807E+03 0.29215000000001E+03 0.29215000000001E+03 0.29225902159781E+03 0.29411234733491E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29225720901764E+03 0.29411300313807E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29369451421012E+03 0.29215000000001E+03 0.19755459258701E+02 0.19308169215014E+02
 0.16076692215824E+02 0.21826685648384E+03 0.20210978080693E+03 0.14312248325928E+02 0.14538621664172E+02
 0.14229175509073E+02 0.17581288005045E+03 0.14071600592301E+02 0.14610274905373E+02 0.13990559517006E+02
 0.17587933305293E+03 0.14312248325927E+02 0.14538621664172E+02 0.14229175509072E+02 0.17581288005045E+03
 0.14071600592300E+02 0.14610274905372E+02 0.13990559517005E+02 0.17587933305293E+03 0.91268674142463E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33592115485128E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44066981719202E-01 0.00000000000000E+00 0.00000000000000E+00 0.44066981719202E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.11595626441785E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.11595626441785E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.97716382264419E-01 0.00000000000000E+00
 0.27981766006118E-01 0.12569814827054E+00 0.00000000000000E+00 0.47353145596547E+00 0.14853145298524E+00
 0.32500000298023E+00 0.42250006586313E+00
    120.89245526
 0.83220396957660E+00 0.29333217302270E+03 0.42036656200510E+03 0.38512705441047E+03 0.37528199894576E+03
 0.22999999835019E+00 0.00000000000000E+00 0.20638944192717E+00 0.00000000000000E+00 -.64721064121065E+01
 0.99590623145441E-03 0.35508074266839E+00 0.80000000000000E+04 0.30000000000000E+04 0.22530086931443E+02
 0.84487825992910E+01 0.29958857204659E+03 0.29215000000001E+03 0.29984939164616E+03 0.31030827158654E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29840400484129E+03 0.31025467035281E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29984939164616E+03 0.31030827158654E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29840400484129E+03 0.31025467035281E+03 0.29215000000001E+03 0.29215000000001E+03 0.33550376789610E+03
 0.29260498870394E+03 0.10843944241002E+04 0.10633807121271E+04 0.58531616303311E+03 0.15209031436945E+04
 0.93266039984623E+03 0.72236838574676E+03 0.73939262916469E+03 0.70369943626235E+03 0.16378654041896E+04
 0.60145589012300E+03 0.73565749500088E+03 0.58798551668538E+03 0.16346777009367E+04 0.72236838574676E+03
 0.73939262916469E+03 0.70369943626235E+03 0.16378654041896E+04 0.60145589012300E+03 0.73565749500089E+03
 0.58798551668538E+03 0.16346777009368E+04 0.14392074126673E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46942618460743E+03 0.12947521832781E+01
 0.12947521832781E+01 0.30390336207840E+00 0.53839677174323E+00 0.29217454500557E+03 0.31840739313156E+03
 0.30899160596845E+03 0.30846656968630E+03 0.23000000000000E+00 0.00000000000000E+00 0.22361014389891E+00
 0.00000000000000E+00 -.23258355668519E-02 0.99956447473125E-03 0.10766046921599E+00 0.80000000000000E+04
 0.30000000000000E+04 0.74307682831574E+02 0.27865381061840E+02 0.29260440812851E+03 0.33553502300319E+03
 0.29228656036815E+03 0.29428277100012E+03 0.29215000000001E+03 0.29215000000001E+03 0.29228431706715E+03
 0.29428350050681E+03 0.29215000000001E+03 0.29215000000001E+03 0.29228656036815E+03 0.29428277100012E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29228431706715E+03 0.29428350050681E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29385578502571E+03 0.29215000000001E+03 0.22127415367359E+02 0.21532276922362E+02
 0.18368532290375E+02 0.21637123297398E+03 0.19791085802215E+03 0.16157159037645E+02 0.16758214580351E+02
 0.16052137773661E+02 0.17273060722091E+03 0.15893960776873E+02 0.16829304182066E+02 0.15791606192386E+02
 0.17279601280347E+03 0.16157159037645E+02 0.16758214580351E+02 0.16052137773660E+02 0.17273060722091E+03
 0.15893960776873E+02 0.16829304182066E+02 0.15791606192386E+02 0.17279601280347E+03 0.92038798099543E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33520799646628E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36886522404101E-01 0.00000000000000E+00 0.00000000000000E+00 0.36886522404101E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.99352596837289E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.99352596837289E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10439887786761E+00 0.24217720718258E-02
 0.85234431760969E-03 0.10525122218521E+00 0.24217720718258E-02 0.45669838587162E+00 0.13169838289138E+00
 0.32500000298023E+00 0.42250006586313E+00
    130.60504607
 0.74661185398301E+00 0.29370888039582E+03 0.42423180659601E+03 0.39174848529014E+03 0.38198537026204E+03
 0.22999999798616E+00 0.00000000000000E+00 0.20409026284516E+00 0.00000000000000E+00 -.71352639600423E+01
 0.99462238433501E-03 0.38618546374717E+00 0.80000000000000E+04 0.30000000000000E+04 0.20715435330931E+02
 0.77682882490990E+01 0.30057336592757E+03 0.29215000000001E+03 0.30070673089716E+03 0.31217745632131E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29916137090130E+03 0.31212384101199E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30070673089716E+03 0.31217745632131E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29916137090130E+03 0.31212384101199E+03 0.29215000000001E+03 0.29215000000001E+03 0.33933578742810E+03
 0.29268954575694E+03 0.11664779899537E+04 0.11427280154246E+04 0.62106748052991E+03 0.15584463297570E+04
 0.93427351182448E+03 0.76374447418987E+03 0.79932931497403E+03 0.74308387956333E+03 0.17057098805622E+04
 0.64197457593903E+03 0.79593205403485E+03 0.62703861494788E+03 0.17028562586938E+04 0.76374447418987E+03
 0.79932931497403E+03 0.74308387956333E+03 0.17057098805622E+04 0.64197457593903E+03 0.79593205403485E+03
 0.62703861494788E+03 0.17028562586939E+04 0.15205204874931E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47982583077763E+03 0.12947570685681E+01
 0.12947570685681E+01 0.34275372534181E+00 0.51876102962892E+00 0.29218592989539E+03 0.31796282655089E+03
 0.30904812691779E+03 0.30854888710602E+03 0.23000000000000E+00 0.00000000000000E+00 0.22315747841521E+00
 0.00000000000000E+00 -.16861395594846E-01 0.99952537730277E-03 0.11797861316208E+00 0.80000000000000E+04
 0.30000000000000E+04 0.67808900152180E+02 0.25428337557067E+02 0.29268897289317E+03 0.33936956014398E+03
 0.29231189231318E+03 0.29441288401527E+03 0.29215000000001E+03 0.29215000000001E+03 0.29230927483501E+03
 0.29441366700085E+03 0.29215000000001E+03 0.29215000000001E+03 0.29231189231318E+03 0.29441288401527E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29230927483502E+03 0.29441366700085E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29398070649027E+03 0.29215000000001E+03 0.23684726961342E+02 0.22950856403357E+02
 0.19943721999165E+02 0.21142529161736E+03 0.19138185100820E+03 0.17498723801642E+02 0.18263303039607E+02
 0.17375962472384E+02 0.16776012332342E+03 0.17221547682722E+02 0.18332361442344E+02 0.17102020472450E+02
 0.16782318196052E+03 0.17498723801642E+02 0.18263303039607E+02 0.17375962472384E+02 0.16776012332342E+03
 0.17221547682720E+02 0.18332361442344E+02 0.17102020472449E+02 0.16782318196052E+03 0.90969205310061E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33420756804079E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.30434553946885E-01 0.00000000000000E+00 0.00000000000000E+00 0.30434553946885E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.85534461335393E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.85534461335393E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10555994821585E+00 0.21014473540272E-01
 0.00000000000000E+00 0.10555994821585E+00 0.21014473540272E-01 0.44688051481446E+00 0.12188051183423E+00
 0.32500000298023E+00 0.42250006586313E+00
    140.57895722
 0.68129230196488E+00 0.29416503077124E+03 0.42801878310259E+03 0.39762093941843E+03 0.38793148696785E+03
 0.22999999795542E+00 0.00000000000000E+00 0.20153605516620E+00 0.00000000000000E+00 -.76935464229074E+01
 0.99307458966140E-03 0.42055926982403E+00 0.80000000000000E+04 0.30000000000000E+04 0.19022289066051E+02
 0.71333583997691E+01 0.30159591102845E+03 0.29215000000001E+03 0.30159069903444E+03 0.31408211271820E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29995007693309E+03 0.31402884597999E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30159069903444E+03 0.31408211271820E+03 0.29215000000001E+03 0.29215000000001E+03
 0.29995007693309E+03 0.31402884597999E+03 0.29215000000001E+03 0.29215000000001E+03 0.34315867822911E+03
 0.29277334080274E+03 0.12472516463387E+04 0.12208912186431E+04 0.65943143998031E+03 0.15972480520904E+04
 0.93451945491022E+03 0.80555907013912E+03 0.86150495781921E+03 0.78307476025585E+03 0.17779001846056E+04
 0.68315574997514E+03 0.85842297801158E+03 0.66689952050370E+03 0.17753559324215E+04 0.80555907013912E+03
 0.86150495781921E+03 0.78307476025585E+03 0.17779001846056E+04 0.68315574997514E+03 0.85842297801158E+03
 0.66689952050371E+03 0.17753559324215E+04 0.16007362611660E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.48908800892545E+03 0.12947611813082E+01
 0.12947611813082E+01 0.38264936995577E+00 0.50614430659863E+00 0.29219974886553E+03 0.31728466626556E+03
 0.30882027418322E+03 0.30835236789263E+03 0.23000000000000E+00 0.00000000000000E+00 0.22272346768120E+00
 0.00000000000000E+00 -.35674470133992E-01 0.99947790676834E-03 0.12785575281364E+00 0.80000000000000E+04
 0.30000000000000E+04 0.62570512659378E+02 0.23463942247267E+02 0.29277278959678E+03 0.34319534541589E+03
 0.29233657057928E+03 0.29452081511355E+03 0.29215000000001E+03 0.29215000000001E+03 0.29233360362042E+03
 0.29452164068673E+03 0.29215000000001E+03 0.29215000000001E+03 0.29233657057928E+03 0.29452081511355E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29233360362042E+03 0.29452164068673E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29408533136916E+03 0.29215000000001E+03 0.24714608145077E+02 0.23841583406318E+02
 0.21074316244751E+02 0.20420029573977E+03 0.18302060791379E+03 0.18542498415469E+02 0.19315739795745E+02
 0.18405504841961E+02 0.16116613159631E+03 0.18256657159615E+02 0.19381639309927E+02 0.18123429556639E+02
 0.16122581402120E+03 0.18542498415469E+02 0.19315739795744E+02 0.18405504841962E+02 0.16116613159631E+03
 0.18256657159614E+02 0.19381639309927E+02 0.18123429556639E+02 0.16122581402120E+03 0.88621718379767E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33312212374472E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.23185245643978E-01 0.69520824005186E-03 0.00000000000000E+00 0.23185245643978E-01 0.69520824005186E-03
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.73603407674255E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.73603407674255E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10140324489821E+00 0.31785209626749E-01
 0.00000000000000E+00 0.10140324489821E+00 0.31785209626749E-01 0.44057215329932E+00 0.11557215031908E+00
 0.32500000298023E+00 0.42250006586313E+00
    150.55286838
 0.64028785600907E+00 0.29468691504597E+03 0.43188880651920E+03 0.40260590487527E+03 0.39285135058693E+03
 0.22999999620576E+00 0.00000000000000E+00 0.19869110716418E+00 0.00000000000000E+00 -.81409722802304E+01
 0.99131149789594E-03 0.45848768229703E+00 0.80000000000000E+04 0.30000000000000E+04 0.17448669416635E+02
 0.65432510312381E+01 0.30263315607482E+03 0.29215000000001E+03 0.30248604305321E+03 0.31600457199086E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30075639110799E+03 0.31595196106371E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30248604305321E+03 0.31600457199086E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30075639110799E+03 0.31595196106371E+03 0.29215000000001E+03 0.29215000000001E+03 0.34694693539678E+03
 0.29285576328939E+03 0.13245065601983E+04 0.12957254009995E+04 0.70150845672247E+03 0.16417725384605E+04
 0.93675653945446E+03 0.84729882884026E+03 0.92699616588331E+03 0.82321906432002E+03 0.18570226441496E+04
 0.72440567779985E+03 0.92420034048642E+03 0.70701664765894E+03 0.18547571005445E+04 0.84729882884026E+03
 0.92699616588331E+03 0.82321906432002E+03 0.18570226441496E+04 0.72440567779985E+03 0.92420034048642E+03
 0.70701664765894E+03 0.18547571005445E+04 0.16804467942687E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.49852628762881E+03 0.12947644774159E+01
 0.12947644774159E+01 0.42254501456973E+00 0.49853744715799E+00 0.29221557782838E+03 0.31646262656714E+03
 0.30840391871425E+03 0.30796945890186E+03 0.23000000000000E+00 0.00000000000000E+00 0.22231348070025E+00
 0.00000000000000E+00 -.51636587328806E-01 0.99942358970103E-03 0.13719182246137E+00 0.80000000000000E+04
 0.30000000000000E+04 0.58312513504606E+02 0.21867192564227E+02 0.29285522811617E+03 0.34698662763063E+03
 0.29235999277254E+03 0.29460519367610E+03 0.29215000000001E+03 0.29215000000001E+03 0.29235671114579E+03
 0.29460604764898E+03 0.29215000000001E+03 0.29215000000001E+03 0.29235999277254E+03 0.29460519367610E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29235671114579E+03 0.29460604764898E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29416860631342E+03 0.29215000000001E+03 0.25277572102918E+02 0.24266765421729E+02
 0.21835216416181E+02 0.19533230021219E+03 0.17338790771392E+03 0.19326600209359E+02 0.19989986243555E+02
 0.19179458573102E+02 0.15366872092153E+03 0.19036681155714E+02 0.20051551654228E+02 0.18893777822641E+02
 0.15372396904093E+03 0.19326600209359E+02 0.19989986243555E+02 0.19179458573103E+02 0.15366872092153E+03
 0.19036681155714E+02 0.20051551654228E+02 0.18893777822641E+02 0.15372396904093E+03 0.85387837833982E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33148652984458E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.17018753598013E-01 0.40633965912712E-02 0.00000000000000E+00 0.17018753598013E-01 0.40633965912712E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.62861928508119E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.62861928508119E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.95864366088335E-01 0.39269934888514E-01
 0.00000000000000E+00 0.95864366088335E-01 0.39269934888514E-01 0.43676872357900E+00 0.11176872059876E+00
 0.32500000298023E+00 0.42250006586313E+00
    160.52677953
 0.62127554339423E+00 0.29525386017738E+03 0.43605076246173E+03 0.40689287180342E+03 0.39685883917110E+03
 0.22999999628430E+00 0.00000000000000E+00 0.19551252971852E+00 0.00000000000000E+00 -.84541318097932E+01
 0.98940492787235E-03 0.49988640568761E+00 0.80000000000000E+04 0.30000000000000E+04 0.16003635844019E+02
 0.60013634415070E+01 0.30343466193627E+03 0.29215000000001E+03 0.30312445386632E+03 0.31801286359000E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30141267159984E+03 0.31793645480758E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30312445386632E+03 0.31801286359000E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30141267159984E+03 0.31793645480758E+03 0.29215000000001E+03 0.29215000000001E+03 0.35077445697316E+03
 0.29293835440408E+03 0.12535431553184E+04 0.12236679328098E+04 0.74897755402840E+03 0.16922051029437E+04
 0.93948266114516E+03 0.78057939935882E+03 0.10090420252358E+04 0.75616874549705E+03 0.19567423014508E+04
 0.69777792125361E+03 0.99670240896249E+03 0.68002194427542E+03 0.19451733340283E+04 0.78057939935882E+03
 0.10090420252358E+04 0.75616874549705E+03 0.19567423014508E+04 0.69777792125361E+03 0.99670240896249E+03
 0.68002194427542E+03 0.19451733340283E+04 0.17186913092952E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.13160811675949E+06 0.50050000000000E+08 0.50308856575665E+03 0.11848371599422E+01
 0.11848371599422E+01 0.46244065918369E+00 0.49376183896904E+00 0.29223326218912E+03 0.31556136889933E+03
 0.30788234965340E+03 0.30748166701632E+03 0.23000000000000E+00 0.00000000000000E+00 0.22192244203261E+00
 0.00000000000000E+00 -.57681545538094E-01 0.99936302874732E-03 0.14611936824220E+00 0.80000000000000E+04
 0.30000000000000E+04 0.54749757655258E+02 0.20531159120722E+02 0.29293782896534E+03 0.35081732859100E+03
 0.29238207470201E+03 0.29466950192610E+03 0.29215000000001E+03 0.29215000000001E+03 0.29237851582784E+03
 0.29467036913475E+03 0.29215000000001E+03 0.29215000000001E+03 0.29238207470201E+03 0.29466950192610E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29237851582784E+03 0.29467036913475E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29423319863905E+03 0.29215000000001E+03 0.25491686800159E+02 0.24341933146531E+02
 0.22358272628004E+02 0.18664461313116E+03 0.16417454914002E+03 0.19942642725848E+02 0.20406202031721E+02
 0.19789511507845E+02 0.14650136832401E+03 0.19652493491672E+02 0.20462045302417E+02 0.19504003633615E+02
 0.14655088252703E+03 0.19942642725848E+02 0.20406202031721E+02 0.19789511507845E+02 0.14650136832401E+03
 0.19652493491672E+02 0.20462045302416E+02 0.19504003633614E+02 0.14655088252703E+03 0.82066793730708E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33046589020334E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.14297331394012E-01 0.63195677443525E-02 0.00000000000000E+00 0.14297331394012E-01 0.63195677443525E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.57590776155249E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.57590776155249E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.92990343288394E-01 0.41647461578432E-01
 0.00000000000000E+00 0.92990343288394E-01 0.41647461578432E-01 0.43438091948452E+00 0.10938091650429E+00
 0.32500000298023E+00 0.42250006586313E+00
    170.50069068
 0.61278948987977E+00 0.29581871756293E+03 0.44030631906705E+03 0.41079282452716E+03 0.40036260104519E+03
 0.22999999631640E+00 0.00000000000000E+00 0.19214821007249E+00 0.00000000000000E+00 -.87352123779410E+01
 0.98751294779758E-03 0.54207308234429E+00 0.80000000000000E+04 0.30000000000000E+04 0.14758157636979E+02
 0.55343091138671E+01 0.30395704988829E+03 0.29215000000001E+03 0.30346518956881E+03 0.32010382790807E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30188441238217E+03 0.31998283115156E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30346518956881E+03 0.32010382790808E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30188441238217E+03 0.31998283115156E+03 0.29215000000001E+03 0.29215000000001E+03 0.35465607112917E+03
 0.29302525795762E+03 0.12064975470874E+04 0.11768675505099E+04 0.79833707633394E+03 0.17436979772298E+04
 0.94136921551414E+03 0.73493364956933E+03 0.10886971222946E+04 0.71141495383726E+03 0.20537315889616E+04
 0.68202086684882E+03 0.10707028440531E+04 0.66460675549619E+03 0.20369580037204E+04 0.73493364956933E+03
 0.10886971222946E+04 0.71141495383726E+03 0.20537315889616E+04 0.68202086684882E+03 0.10707028440531E+04
 0.66460675549619E+03 0.20369580037204E+04 0.17631651227384E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.79114344881170E+05 0.50050000000000E+08 0.50725028725321E+03 0.12290599598881E+01
 0.12290599598881E+01 0.50233630379765E+00 0.49021903954547E+00 0.29225286517062E+03 0.31471278808625E+03
 0.30737260019294E+03 0.30700215870631E+03 0.23000000000000E+00 0.00000000000000E+00 0.22152343020220E+00
 0.00000000000000E+00 -.64760359043578E-01 0.99929590399169E-03 0.15513916088804E+00 0.80000000000000E+04
 0.30000000000000E+04 0.51566606098723E+02 0.19337477287021E+02 0.29302472950281E+03 0.35470200561924E+03
 0.29240335431449E+03 0.29472157170037E+03 0.29215000000001E+03 0.29215000000001E+03 0.29239954546050E+03
 0.29472243754231E+03 0.29215000000001E+03 0.29215000000001E+03 0.29240335431449E+03 0.29472157170037E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29239954546050E+03 0.29472243754231E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29428613151356E+03 0.29215000000001E+03 0.25576438359131E+02 0.24278090125048E+02
 0.22860132447429E+02 0.17830120631009E+03 0.15532677320042E+03 0.20517306410598E+02 0.20789494859726E+02
 0.20361775204291E+02 0.13966082310379E+03 0.20227621298221E+02 0.20838883454383E+02 0.20077077778887E+02
 0.13970398580760E+03 0.20517306410598E+02 0.20789494859726E+02 0.20361775204291E+02 0.13966082310379E+03
 0.20227621298221E+02 0.20838883454383E+02 0.20077077778887E+02 0.13970398580760E+03 0.78813066801819E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32918297998732E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.11595014031871E-01 0.90497456698485E-02 0.00000000000000E+00 0.11595014031871E-01 0.90497456698485E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.51341715972208E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.51341715972208E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.89154371048037E-01 0.44678878631457E-01
 0.00000000000000E+00 0.89154371048037E-01 0.44678878631457E-01 0.43260951977274E+00 0.10760951679250E+00
 0.32500000298023E+00 0.42250006586313E+00
    180.47460184
 0.60947846000908E+00 0.29637139839102E+03 0.44387781580900E+03 0.41391048776588E+03 0.40311700184505E+03
 0.22999999633889E+00 0.00000000000000E+00 0.18868772305186E+00 0.00000000000000E+00 -.89684637405630E+01
 0.98566913962479E-03 0.58533543665002E+00 0.80000000000000E+04 0.30000000000000E+04 0.13667376856227E+02
 0.51252663210851E+01 0.30453063889039E+03 0.29215000000001E+03 0.30387274152220E+03 0.32218354609065E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30239533126832E+03 0.32203125329746E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30387274152220E+03 0.32218354609065E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30239533126832E+03 0.32203125329746E+03 0.29215000000001E+03 0.29215000000001E+03 0.35845246182071E+03
 0.29311756516269E+03 0.12183296516461E+04 0.11886370402253E+04 0.84056551676228E+03 0.17800803788915E+04
 0.93531203454541E+03 0.73534478851183E+03 0.11554477725788E+04 0.71241520829141E+03 0.21316462169194E+04
 0.69336888773834E+03 0.11368110791977E+04 0.67611836742041E+03 0.21145448465389E+04 0.73534478851183E+03
 0.11554477725788E+04 0.71241520829141E+03 0.21316462169194E+04 0.69336888773834E+03 0.11368110791977E+04
 0.67611836742041E+03 0.21145448465389E+04 0.18134414978777E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.58632686778265E+05 0.50050000000000E+08 0.51082386518129E+03 0.12456879175213E+01
 0.12456879175213E+01 0.54223194841161E+00 0.48854539374466E+00 0.29227428777716E+03 0.31380042493663E+03
 0.30678942816705E+03 0.30644939941308E+03 0.23000000000000E+00 0.00000000000000E+00 0.22114705893675E+00
 0.00000000000000E+00 -.72158448389061E-01 0.99922256297498E-03 0.16367545683879E+00 0.80000000000000E+04
 0.30000000000000E+04 0.48877211981022E+02 0.18328954492883E+02 0.29311702696381E+03 0.35850062660853E+03
 0.29242352880339E+03 0.29475846092636E+03 0.29215000000001E+03 0.29215000000001E+03 0.29241949672768E+03
 0.29475931330314E+03 0.29215000000001E+03 0.29215000000001E+03 0.29242352880339E+03 0.29475846092636E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29241949672768E+03 0.29475931330314E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29432519700757E+03 0.29215000000001E+03 0.25316952204726E+02 0.23857401239436E+02
 0.23098589380990E+02 0.16889950062290E+03 0.14568541829501E+03 0.20907008843164E+02 0.20910414471127E+02
 0.20753023010927E+02 0.13204364118950E+03 0.20619968742577E+02 0.20953038960984E+02 0.20471252244602E+02
 0.13208024710711E+03 0.20907008843164E+02 0.20910414471127E+02 0.20753023010927E+02 0.13204364118950E+03
 0.20619968742577E+02 0.20953038960985E+02 0.20471252244602E+02 0.13208024710711E+03 0.74992594421303E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32736376988068E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.91154364411379E-02 0.12040991262732E-01 0.00000000000000E+00 0.91154364411379E-02 0.12040991262732E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.43989371611620E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.43989371611620E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.84140252812357E-01 0.48220477695238E-01
 0.00000000000000E+00 0.84140252812357E-01 0.48220477695238E-01 0.43177269687233E+00 0.10677269389210E+00
 0.32500000298023E+00 0.42250006586313E+00
    190.44851299
 0.60856589893953E+00 0.29691722393399E+03 0.44655871614509E+03 0.41620314640306E+03 0.40513900600177E+03
 0.22999999646292E+00 0.00000000000000E+00 0.18518723165165E+00 0.00000000000000E+00 -.91400633182142E+01
 0.98385550901547E-03 0.62962231850604E+00 0.80000000000000E+04 0.30000000000000E+04 0.12706029892622E+02
 0.47647612097334E+01 0.30517159234414E+03 0.29215000000001E+03 0.30436474889770E+03 0.32419534684016E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30295420567008E+03 0.32402444437147E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30436474889769E+03 0.32419534684016E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30295420567008E+03 0.32402444437150E+03 0.29215000000001E+03 0.29215000000001E+03 0.36203337178831E+03
 0.29321583780758E+03 0.12526964970628E+04 0.12225816966821E+04 0.87272412946269E+03 0.17980019988680E+04
 0.92091424875803E+03 0.75423104760470E+03 0.12088061787884E+04 0.73153436578417E+03 0.21888076257510E+04
 0.71460984108612E+03 0.11914567973005E+04 0.69732559799429E+03 0.21731781924575E+04 0.75423104760477E+03
 0.12088061787885E+04 0.73153436578428E+03 0.21888076257511E+04 0.71460984108615E+03 0.11914567973003E+04
 0.69732559799434E+03 0.21731781924570E+04 0.18561996961498E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.52987686742907E+05 0.50050000000000E+08 0.51350276763420E+03 0.12501291962750E+01
 0.12501291962750E+01 0.58212759302557E+00 0.48860655024880E+00 0.29229752419719E+03 0.31280565380421E+03
 0.30612538283131E+03 0.30581641726884E+03 0.23000000000000E+00 0.00000000000000E+00 0.22080608046437E+00
 0.00000000000000E+00 -.76885860404540E-01 0.99914305816978E-03 0.17149714638673E+00 0.80000000000000E+04
 0.30000000000000E+04 0.46648006503616E+02 0.17493002438856E+02 0.29321530026850E+03 0.36208235760643E+03
 0.29244220597454E+03 0.29477972540399E+03 0.29215000000001E+03 0.29215000000001E+03 0.29243797983700E+03
 0.29478055379999E+03 0.29215000000001E+03 0.29215000000001E+03 0.29244220597454E+03 0.29477972540399E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29243797983700E+03 0.29478055379999E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29435001863073E+03 0.29215000000001E+03 0.24672535057598E+02 0.23037353068879E+02
 0.23027648322906E+02 0.15864353027277E+03 0.13550074370824E+03 0.21085240594114E+02 0.20725191592161E+02
 0.20937218459169E+02 0.12382050427180E+03 0.20803229904020E+02 0.20760864534288E+02 0.20660681321464E+02
 0.12385044279276E+03 0.21085240594115E+02 0.20725191592159E+02 0.20937218459170E+02 0.12382050427179E+03
 0.20803229904022E+02 0.20760864534287E+02 0.20660681321466E+02 0.12385044279276E+03 0.70687286753070E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32533406490917E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.73996129644841E-02 0.14459518046137E-01 0.00000000000000E+00 0.73996129644841E-02 0.14459518046137E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.37418257304844E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.37418257304844E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.79267924248629E-01 0.50858717390502E-01
 0.00000000000000E+00 0.79267924248629E-01 0.50858717390502E-01 0.43180327512440E+00 0.10680327214417E+00
 0.32500000298023E+00 0.42250006586313E+00
    200.19184167
 0.60861253088308E+00 0.29744929045690E+03 0.44841068274338E+03 0.41778501773501E+03 0.40655172604125E+03
 0.22999999664111E+00 0.00000000000000E+00 0.18176076375221E+00 0.00000000000000E+00 -.92569450942984E+01
 0.98209449091494E-03 0.67360765695545E+00 0.80000000000000E+04 0.30000000000000E+04 0.11876349559561E+02
 0.44536310848355E+01 0.30583740974754E+03 0.29215000000002E+03 0.30489951250344E+03 0.32606448667092E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30352698514935E+03 0.32588396038831E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30489951250344E+03 0.32606448667092E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30352698514935E+03 0.32588396038832E+03 0.29215000000001E+03 0.29215000000001E+03 0.36526181121397E+03
 0.29331905101729E+03 0.12902171048445E+04 0.12594906843445E+04 0.89482328062555E+03 0.18003132337516E+04
 0.90101583672289E+03 0.77702325335212E+03 0.12493051716091E+04 0.75433842206509E+03 0.22271084503318E+04
 0.73649375300739E+03 0.12337337321357E+04 0.71907532802320E+03 0.22133493880738E+04 0.77702325335214E+03
 0.12493051716091E+04 0.75433842206512E+03 0.22271084503318E+04 0.73649375300740E+03 0.12337337321356E+04
 0.71907532802323E+03 0.22133493880736E+04 0.18869995668811E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.53276146738547E+05 0.50050000000000E+08 0.51534367231497E+03 0.12497770073766E+01
 0.12497770073766E+01 0.62110090772776E+00 0.48984453375805E+00 0.29232209256808E+03 0.31179005390936E+03
 0.30543253761174E+03 0.30515347648550E+03 0.23000000000000E+00 0.00000000000000E+00 0.22050555128129E+00
 0.00000000000000E+00 -.78290542371990E-01 0.99905904710028E-03 0.17848135681242E+00 0.80000000000000E+04
 0.30000000000000E+04 0.44822608606723E+02 0.16808478227521E+02 0.29331852707195E+03 0.36531051000972E+03
 0.29245887337943E+03 0.29478726645239E+03 0.29215000000001E+03 0.29215000000001E+03 0.29245448467111E+03
 0.29478806248147E+03 0.29215000000001E+03 0.29215000000001E+03 0.29245887337943E+03 0.29478726645239E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29245448467110E+03 0.29478806248147E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29436214594211E+03 0.29215000000001E+03 0.23735129266293E+02 0.21910665631047E+02
 0.22729941185277E+02 0.14844234478532E+03 0.12559875389412E+03 0.21098161352469E+02 0.20318418859743E+02
 0.20960405657713E+02 0.11569085332043E+03 0.20822564056752E+02 0.20347170626622E+02 0.20690399747863E+02
 0.11571420026921E+03 0.21098161352471E+02 0.20318418859740E+02 0.20960405657715E+02 0.11569085332042E+03
 0.20822564056759E+02 0.20347170626618E+02 0.20690399747871E+02 0.11571420026920E+03 0.66302645952762E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32342632338396E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.63626464276473E-02 0.16097580887954E-01 0.00000000000000E+00 0.63626464276473E-02 0.16097580887954E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.32480474320580E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.32613534124808E-05 0.32480474320580E-01 0.32613534124808E-05 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.75256440830728E-01 0.52128648615781E-01
 0.00000000000000E+00 0.75256440830728E-01 0.52128648615781E-01 0.43242226687902E+00 0.10742226389879E+00
 0.32500000298023E+00 0.42250006586313E+00
    210.15425853
 0.60901034339969E+00 0.29799494739609E+03 0.44971239236187E+03 0.41891322794242E+03 0.40758643749810E+03
 0.22999999670846E+00 0.00000000000000E+00 0.17826771039390E+00 0.00000000000000E+00 -.93461205135214E+01
 0.98029532017530E-03 0.71901940557296E+00 0.80000000000000E+04 0.30000000000000E+04 0.11126264378950E+02
 0.41723491421061E+01 0.30653450045284E+03 0.29215000000005E+03 0.30547483194067E+03 0.32786839051326E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30412224351699E+03 0.32768369104762E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30547483194067E+03 0.32786839051326E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30412224351699E+03 0.32768369104763E+03 0.29215000000001E+03 0.29215000000001E+03 0.36827690944309E+03
 0.29343407805367E+03 0.13257959570922E+04 0.12943706392353E+04 0.90968204014087E+03 0.17915597021326E+04
 0.87732925179107E+03 0.79942874870057E+03 0.12810089846811E+04 0.77665108296451E+03 0.22523638807421E+04
 0.75688811271347E+03 0.12672723253298E+04 0.73930383394413E+03 0.22404759810623E+04 0.79942874870058E+03
 0.12810089846811E+04 0.77665108296454E+03 0.22523638807421E+04 0.75688811271348E+03 0.12672723253297E+04
 0.73930383394415E+03 0.22404759810621E+04 0.19082032423369E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.55736970210519E+05 0.50050000000000E+08 0.51663178356890E+03 0.12476815424023E+01
 0.12476815424023E+01 0.66095057515841E+00 0.49162029120507E+00 0.29234942808503E+03 0.31077046041633E+03
 0.30473302489699E+03 0.30448246035689E+03 0.23000000000000E+00 0.00000000000000E+00 0.22021863257616E+00
 0.00000000000000E+00 -.76602963446884E-01 0.99896562532043E-03 0.18519242200033E+00 0.80000000000000E+04
 0.30000000000000E+04 0.43198311861733E+02 0.16199366948150E+02 0.29343357666433E+03 0.36832475508767E+03
 0.29247448335518E+03 0.29478460558708E+03 0.29215000000001E+03 0.29215000000001E+03 0.29246995149482E+03
 0.29478535955507E+03 0.29215000000001E+03 0.29215000000001E+03 0.29247448335518E+03 0.29478460558708E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29246995149482E+03 0.29478535955507E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29436497923434E+03 0.29215000000001E+03 0.22587176560977E+02 0.20545737779945E+02
 0.22334868759091E+02 0.13873888499281E+03 0.11629234188993E+03 0.21035015909742E+02 0.19810765248968E+02
 0.20912173426699E+02 0.10797409707204E+03 0.20766226593683E+02 0.19832217908743E+02 0.20649009363013E+02
 0.10799051899061E+03 0.21035015909743E+02 0.19810765248967E+02 0.20912173426701E+02 0.10797409707203E+03
 0.20766226593686E+02 0.19832217908742E+02 0.20649009363016E+02 0.10799051899061E+03 0.62051231554862E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32186695471811E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.55359771743053E-02 0.17503870410033E-01 0.00000000000000E+00 0.55359771743053E-02 0.17503870410033E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29442889161899E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.68003334692678E-04 0.29442889161899E-01 0.68003334692678E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.72362828347487E-01 0.51908679914798E-01
 0.00000000000000E+00 0.72362828347487E-01 0.51908679914798E-01 0.43331014560254E+00 0.10831014262230E+00
 0.32500000298023E+00 0.42250006586313E+00
    220.69541401
 0.60954813690740E+00 0.29857431533794E+03 0.45063835488687E+03 0.41974157088765E+03 0.40837887919776E+03
 0.22999999669066E+00 0.00000000000000E+00 0.17459434623463E+00 0.00000000000000E+00 -.94138470895729E+01
 0.97839245407110E-03 0.76726423021235E+00 0.80000000000000E+04 0.30000000000000E+04 0.10426655752981E+02
 0.39099959073678E+01 0.30727001821530E+03 0.29215000000010E+03 0.30609325957275E+03 0.32965599241540E+03
 0.29215000000001E+03 0.29215000000001E+03 0.30474757540355E+03 0.32947073867488E+03 0.29215000000001E+03
 0.29215000000001E+03 0.30609325957275E+03 0.32965599241540E+03 0.29215000000001E+03 0.29215000000001E+03
 0.30474757540355E+03 0.32947073867489E+03 0.29215000000001E+03 0.29215000000001E+03 0.37117399932551E+03
 0.29357039400800E+03 0.13584837092884E+04 0.13263363540510E+04 0.91900211119564E+03 0.17744010995493E+04
 0.85080397779767E+03 0.82044541854417E+03 0.13063362382521E+04 0.79753975896308E+03 0.22680855340582E+04
 0.77552251431936E+03 0.12943766848096E+04 0.75778734217779E+03 0.22579739457013E+04 0.82044541854418E+03
 0.13063362382521E+04 0.79753975896310E+03 0.22680855340582E+04 0.77552251431936E+03 0.12943766848095E+04
 0.75778734217780E+03 0.22579739457012E+04 0.19218824396645E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.59063700319178E+05 0.50050000000000E+08 0.51754503337479E+03 0.12448972080381E+01
 0.12448972080381E+01 0.70311519709569E+00 0.49339351281507E+00 0.29238126165400E+03 0.30977129979631E+03
 0.30405121179163E+03 0.30382728411779E+03 0.23000000000000E+00 0.00000000000000E+00 0.21991950066845E+00
 0.00000000000000E+00 -.73387326809241E-01 0.99885686918381E-03 0.19215849632398E+00 0.80000000000000E+04
 0.30000000000000E+04 0.41632299133481E+02 0.15612112175055E+02 0.29356991710733E+03 0.37122081976893E+03
 0.29248979885660E+03 0.29477595655351E+03 0.29215000000001E+03 0.29215000000001E+03 0.29248513078936E+03
 0.29477665804224E+03 0.29215000000001E+03 0.29215000000001E+03 0.29248979885660E+03 0.29477595655351E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29248513078936E+03 0.29477665804224E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29436237954710E+03 0.29215000000002E+03 0.21282179365673E+02 0.18974558461470E+02
 0.21947238671950E+02 0.12969427782686E+03 0.10763730296155E+03 0.20969994365468E+02 0.19301922480697E+02
 0.20867446541479E+02 0.10077868824428E+03 0.20707034970536E+02 0.19315542421502E+02 0.20610050186939E+02
 0.10078770138121E+03 0.20969994365469E+02 0.19301922480696E+02 0.20867446541480E+02 0.10077868824428E+03
 0.20707034970539E+02 0.19315542421501E+02 0.20610050186941E+02 0.10078770138121E+03 0.58019155530296E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32060271240716E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.49491492990250E-02 0.18569569996216E-01 0.00000000000000E+00 0.49491492990250E-02 0.18569569996216E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27652349232720E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18081096197003E-03 0.27652349232720E-01 0.18081096197003E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.70221207685192E-01 0.50790369108345E-01
 0.00000000000000E+00 0.70221207685192E-01 0.50790369108345E-01 0.43419675640753E+00 0.10919675342730E+00
 0.32500000298023E+00 0.42250006586313E+00
    231.23656949
 0.61001809095104E+00 0.29915623518106E+03 0.45127869358540E+03 0.42034620969654E+03 0.40898954490425E+03
 0.22999999672381E+00 0.00000000000000E+00 0.17094941663200E+00 0.00000000000000E+00 -.94612242512481E+01
 0.97648882481432E-03 0.81546455733455E+00 0.80000000000000E+04 0.30000000000000E+04 0.98103589273690E+01
 0.36788845977634E+01 0.30799617622628E+03 0.29215000000021E+03 0.30671184445730E+03 0.33133489823128E+03
 0.29215000000001E+03 0.29215000000002E+03 0.30536394536755E+03 0.33115171883057E+03 0.29215000000001E+03
 0.29215000000002E+03 0.30671184445730E+03 0.33133489823128E+03 0.29215000000001E+03 0.29215000000002E+03
 0.30536394536755E+03 0.33115171883058E+03 0.29215000000001E+03 0.29215000000002E+03 0.37380178167655E+03
 0.29372336318716E+03 0.13852116437349E+04 0.13523986265087E+04 0.92393055242601E+03 0.17530886697467E+04
 0.82453846455853E+03 0.83778097187550E+03 0.13256414302175E+04 0.81475668226634E+03 0.22763704943072E+04
 0.79085906867506E+03 0.13152074948926E+04 0.77301578112353E+03 0.22677573997875E+04 0.83778097187551E+03
 0.13256414302175E+04 0.81475668226636E+03 0.22763704943072E+04 0.79085906867507E+03 0.13152074948926E+04
 0.77301578112354E+03 0.22677573997874E+04 0.19291332466268E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.61970783151326E+05 0.50050000000000E+08 0.51817680625150E+03 0.12424785151275E+01
 0.12424785151275E+01 0.74527981903297E+00 0.49470767563090E+00 0.29241680903646E+03 0.30888324225525E+03
 0.30345252831952E+03 0.30325131182490E+03 0.23000000000000E+00 0.00000000000000E+00 0.21961224819785E+00
 0.00000000000000E+00 -.69871274098687E-01 0.99873545671507E-03 0.19922817754012E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40154962509703E+02 0.15058110941139E+02 0.29372291310285E+03 0.37384760983983E+03
 0.29250441128340E+03 0.29476274051810E+03 0.29215000000001E+03 0.29215000000001E+03 0.29249961579329E+03
 0.29476337908339E+03 0.29215000000001E+03 0.29215000000001E+03 0.29250441128340E+03 0.29476274051810E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29249961579329E+03 0.29476337908339E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29435538548468E+03 0.29215000000005E+03 0.19957681376726E+02 0.17341559172893E+02
 0.21688547907993E+02 0.12205758457265E+03 0.10026059392512E+03 0.20971842888354E+02 0.18912547586288E+02
 0.20893739154736E+02 0.94695263649058E+02 0.20712886687553E+02 0.18917875726144E+02 0.20640159401878E+02
 0.94696454595126E+02 0.20971842888355E+02 0.18912547586287E+02 0.20893739154737E+02 0.94695263649057E+02
 0.20712886687555E+02 0.18917875726143E+02 0.20640159401882E+02 0.94696454595124E+02 0.54552397417432E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31965267281763E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46453092887412E-02 0.19153691960029E-01 0.00000000000000E+00 0.46453092887412E-02 0.19153691960029E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26739577791668E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27280470808778E-03 0.26739577791668E-01 0.27280470808778E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.68749629305417E-01 0.49261353444716E-01
 0.00000000000000E+00 0.68749629305417E-01 0.49261353444716E-01 0.43485383781545E+00 0.10985383483522E+00
 0.32500000298023E+00 0.42250006586313E+00
    240.84946241
 0.61032283733409E+00 0.29968888443477E+03 0.45172655662040E+03 0.42079586879706E+03 0.40946586101515E+03
 0.22999999674013E+00 0.00000000000000E+00 0.16765051056161E+00 0.00000000000000E+00 -.94941277932572E+01
 0.97475295500432E-03 0.85923791133099E+00 0.80000000000000E+04 0.30000000000000E+04 0.93105761448627E+01
 0.34914660543235E+01 0.30864098802304E+03 0.29215000000039E+03 0.30726549268006E+03 0.33278037771184E+03
 0.29215000000002E+03 0.29215000000003E+03 0.30591139867343E+03 0.33260027446437E+03 0.29215000000002E+03
 0.29215000000003E+03 0.30726549268006E+03 0.33278037771184E+03 0.29215000000002E+03 0.29215000000003E+03
 0.30591139867343E+03 0.33260027446437E+03 0.29215000000002E+03 0.29215000000003E+03 0.37600512735109E+03
 0.29388019196880E+03 0.14047667385857E+04 0.13714408993777E+04 0.92597930349860E+03 0.17322306908136E+04
 0.80162149079755E+03 0.85046040731638E+03 0.13397186166327E+04 0.82737677225000E+03 0.22800811703893E+04
 0.80224960898025E+03 0.13304607653460E+04 0.78436938449919E+03 0.22726078948766E+04 0.85046040731639E+03
 0.13397186166327E+04 0.82737677225001E+03 0.22800811703893E+04 0.80224960898025E+03 0.13304607653460E+04
 0.78436938449919E+03 0.22726078948765E+04 0.19320775813242E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.63855909984165E+05 0.50050000000000E+08 0.51862049744787E+03 0.12409188251470E+01
 0.12409188251470E+01 0.78373139068696E+00 0.49540075566004E+00 0.29245310090854E+03 0.30817558431066E+03
 0.30298296420515E+03 0.30279926400050E+03 0.23000000000000E+00 0.00000000000000E+00 0.21931805657350E+00
 0.00000000000000E+00 -.66983533073358E-01 0.99861152867203E-03 0.20590155737542E+00 0.80000000000000E+04
 0.30000000000000E+04 0.38853518652185E+02 0.14570069494570E+02 0.29387976851460E+03 0.37605012774460E+03
 0.29251744805021E+03 0.29474991937342E+03 0.29215000000001E+03 0.29215000000001E+03 0.29251253639503E+03
 0.29475049278655E+03 0.29215000000001E+03 0.29215000000001E+03 0.29251744805021E+03 0.29474991937342E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29251253639503E+03 0.29475049278655E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29434800655954E+03 0.29215000000008E+03 0.18749209169498E+02 0.15806723964474E+02
 0.21581270540000E+02 0.11623345773919E+03 0.94544280846488E+02 0.21048341090859E+02 0.18679488334226E+02
 0.20995504796376E+02 0.90048160556355E+02 0.20790834587762E+02 0.18677046457518E+02 0.20743043801936E+02
 0.90042043103440E+02 0.21048341090860E+02 0.18679488334225E+02 0.20995504796376E+02 0.90048160556352E+02
 0.20790834587765E+02 0.18677046457517E+02 0.20743043801939E+02 0.90042043103438E+02 0.51851847010540E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31898618263133E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45227160420669E-02 0.19403552440994E-01 0.00000000000000E+00 0.45227160420669E-02 0.19403552440994E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26329351009362E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.32334418632497E-03 0.26329351009362E-01 0.32334418632497E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.67790625786605E-01 0.47804544727851E-01
 0.00000000000000E+00 0.67790625786605E-01 0.47804544727851E-01 0.43520037783002E+00 0.11020037484979E+00
 0.32500000298023E+00 0.42250006586313E+00
    251.19128610
 0.61051980663506E+00 0.30026285462160E+03 0.45214134261131E+03 0.42123306757148E+03 0.40994289074566E+03
 0.22999999652711E+00 0.00000000000000E+00 0.16412617207841E+00 0.00000000000000E+00 -.95232911038875E+01
 0.97288937720317E-03 0.90604625527114E+00 0.80000000000000E+04 0.30000000000000E+04 0.88295712867396E+01
 0.33110892325274E+01 0.30931405742547E+03 0.29215000000075E+03 0.30784660098899E+03 0.33425683870537E+03
 0.29215000000003E+03 0.29215000000006E+03 0.30648361133047E+03 0.33408083419248E+03 0.29215000000003E+03
 0.29215000000006E+03 0.30784660098899E+03 0.33425683870537E+03 0.29215000000003E+03 0.29215000000006E+03
 0.30648361133047E+03 0.33408083419248E+03 0.29215000000003E+03 0.29215000000006E+03 0.37820154011096E+03
 0.29406929959173E+03 0.14216927107330E+04 0.13879210100400E+04 0.92667605832525E+03 0.17098786547378E+04
 0.77856921612092E+03 0.86137249462997E+03 0.13523838297954E+04 0.83828566012490E+03 0.22818214143805E+04
 0.81231182201991E+03 0.13441981119683E+04 0.79445738942320E+03 0.22753738829346E+04 0.86137249462998E+03
 0.13523838297954E+04 0.83828566012492E+03 0.22818214143805E+04 0.81231182201994E+03 0.13441981119683E+04
 0.79445738942324E+03 0.22753738829346E+04 0.19329301921800E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65074339852142E+05 0.50050000000000E+08 0.51903368444210E+03 0.12399204656057E+01
 0.12399204656057E+01 0.82509868547015E+00 0.49560923453811E+00 0.29249698468541E+03 0.30751820131568E+03
 0.30255509886504E+03 0.30238727987590E+03 0.23000000000000E+00 0.00000000000000E+00 0.21898223353122E+00
 0.00000000000000E+00 -.64364977351290E-01 0.99846171234172E-03 0.21341209786869E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37486159781449E+02 0.14057309918043E+02 0.29406890698528E+03 0.37824574039710E+03
 0.29253146883008E+03 0.29473657647811E+03 0.29215000000001E+03 0.29215000000001E+03 0.29252642586330E+03
 0.29473707107950E+03 0.29215000000001E+03 0.29215000000001E+03 0.29253146883008E+03 0.29473657647811E+03
 0.29215000000001E+03 0.29215000000001E+03 0.29252642586330E+03 0.29473707107950E+03 0.29215000000001E+03
 0.29215000000001E+03 0.29434015158584E+03 0.29215000000016E+03 0.17453902081110E+02 0.14105992963268E+02
 0.21624264034823E+02 0.11108397789964E+03 0.89351592544642E+02 0.21236158012160E+02 0.18573370086524E+02
 0.21212108012644E+02 0.85928702102105E+02 0.20978044553548E+02 0.18562060648010E+02 0.20958331668161E+02
 0.85914253569335E+02 0.21236158012160E+02 0.18573370086522E+02 0.21212108012645E+02 0.85928702102102E+02
 0.20978044553551E+02 0.18562060648009E+02 0.20958331668165E+02 0.85914253569333E+02 0.49398152436193E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31844120902987E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45022414219408E-02 0.19461611641167E-01 0.00000000000000E+00 0.45022414219408E-02 0.19461611641167E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26181813387346E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.34221259871884E-03 0.26181813387346E-01 0.34221259871884E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.67088062554875E-01 0.46270191507485E-01
 0.00000000000000E+00 0.67088062554875E-01 0.46270191507485E-01 0.43530461726905E+00 0.11030461428882E+00
 0.32500000298023E+00 0.42250006586313E+00
    261.53310980
 0.61060852230071E+00 0.30083675939442E+03 0.45253983419608E+03 0.42166277075171E+03 0.41041587007545E+03
 0.22999999652594E+00 0.00000000000000E+00 0.16062465388410E+00 0.00000000000000E+00 -.95504729256561E+01
 0.97103314061742E-03 0.95250194955968E+00 0.80000000000000E+04 0.30000000000000E+04 0.83989329404504E+01
 0.31495998526689E+01 0.30996686615432E+03 0.29215000000137E+03 0.30841255385079E+03 0.33566519501592E+03
 0.29215000000005E+03 0.29215000000011E+03 0.30703978884482E+03 0.33549379259105E+03 0.29215000000004E+03
 0.29215000000011E+03 0.30841255385079E+03 0.33566519501592E+03 0.29215000000005E+03 0.29215000000011E+03
 0.30703978884482E+03 0.33549379259105E+03 0.29215000000004E+03 0.29215000000011E+03 0.38024802983883E+03
 0.29427982933670E+03 0.14355456838595E+04 0.14014277718215E+04 0.92659332684454E+03 0.16886155464569E+04
 0.75738925297815E+03 0.87021909521471E+03 0.13634287622758E+04 0.84718816634004E+03 0.22825107656549E+04
 0.82074175165935E+03 0.13561559327667E+04 0.80297207265447E+03 0.22769253132546E+04 0.87021909521472E+03
 0.13634287622758E+04 0.84718816634005E+03 0.22825107656548E+04 0.82074175165936E+03 0.13561559327667E+04
 0.80297207265449E+03 0.22769253132546E+04 0.19326052490219E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65623124974670E+05 0.50050000000000E+08 0.51943245685093E+03 0.12394816786735E+01
 0.12394816786735E+01 0.86646598025334E+00 0.49530707483894E+00 0.29254686994881E+03 0.30696132063870E+03
 0.30220160103429E+03 0.30204703394954E+03 0.23000000000000E+00 0.00000000000000E+00 0.21862333464068E+00
 0.00000000000000E+00 -.62424051471020E-01 0.99829145547926E-03 0.22132906200611E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36145275850756E+02 0.13554478444034E+02 0.29427947722551E+03 0.38029136279347E+03
 0.29254578073224E+03 0.29472436253468E+03 0.29215000000001E+03 0.29215000000002E+03 0.29254059054646E+03
 0.29472476825797E+03 0.29215000000001E+03 0.29215000000002E+03 0.29254578073224E+03 0.29472436253468E+03
 0.29215000000001E+03 0.29215000000002E+03 0.29254059054646E+03 0.29472476825797E+03 0.29215000000001E+03
 0.29215000000002E+03 0.29433300698283E+03 0.29215000000029E+03 0.16161362470554E+02 0.12350379348010E+02
 0.21824176616406E+02 0.10695822217673E+03 0.85024924677244E+02 0.21529380760933E+02 0.18612288329198E+02
 0.21529380760933E+02 0.82619418692974E+02 0.21268354827664E+02 0.18591709423234E+02 0.21268354827664E+02
 0.82596286698296E+02 0.21529380760932E+02 0.18612288329198E+02 0.21529380760932E+02 0.82619418692973E+02
 0.21268354827666E+02 0.18591709423232E+02 0.21268354827666E+02 0.82596286698294E+02 0.47356022092417E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31802557726465E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45271637557481E-02 0.19433403956557E-01 0.00000000000000E+00 0.45271637557481E-02 0.19433403956557E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26181217803142E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.34080492477492E-03 0.26181217803142E-01 0.34080492477492E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.66610190339375E-01 0.44895019075712E-01
 0.00000000000000E+00 0.66610190339375E-01 0.44895019075712E-01 0.43515353741947E+00 0.11015353443924E+00
 0.32500000298023E+00 0.42250006586313E+00
    271.87493349
 0.61063048247545E+00 0.30140912919311E+03 0.45294747268107E+03 0.42210282874854E+03 0.41089827822481E+03
 0.22999999655955E+00 0.00000000000000E+00 0.15714342249667E+00 0.00000000000000E+00 -.95777374505860E+01
 0.96918890773941E-03 0.99858475126038E+00 0.80000000000000E+04 0.30000000000000E+04 0.80113380360582E+01
 0.30042517635218E+01 0.31060057896529E+03 0.29215000000244E+03 0.30896373388912E+03 0.33701587019050E+03
 0.29215000000010E+03 0.29215000000021E+03 0.30758092150947E+03 0.33684933387776E+03 0.29215000000008E+03
 0.29215000000021E+03 0.30896373388912E+03 0.33701587019050E+03 0.29215000000010E+03 0.29215000000021E+03
 0.30758092150947E+03 0.33684933387776E+03 0.29215000000008E+03 0.29215000000021E+03 0.38217347219430E+03
 0.29451284649668E+03 0.14474331471142E+04 0.14130559311008E+04 0.92609559173621E+03 0.16687087495440E+04
 0.73798267984915E+03 0.87774515337142E+03 0.13734020117812E+04 0.85482178804741E+03 0.22828135075111E+04
 0.82811761121494E+03 0.13669209604099E+04 0.81048226566051E+03 0.22779669911302E+04 0.87774515337143E+03
 0.13734020117812E+04 0.85482178804743E+03 0.22828135075111E+04 0.82811761121494E+03 0.13669209604099E+04
 0.81048226566052E+03 0.22779669911301E+04 0.19318241587396E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65758968153677E+05 0.50050000000000E+08 0.51984129002500E+03 0.12393860965570E+01
 0.12393860965570E+01 0.90783327503653E+00 0.49457259205446E+00 0.29260370999212E+03 0.30649308368371E+03
 0.30191354798460E+03 0.30177008520176E+03 0.23000000000000E+00 0.00000000000000E+00 0.21824010586638E+00
 0.00000000000000E+00 -.61164094427469E-01 0.99809752705250E-03 0.22968221621401E+00 0.80000000000000E+04
 0.30000000000000E+04 0.34830733227277E+02 0.13061524960229E+02 0.29451254294876E+03 0.38221585462476E+03
 0.29256057012859E+03 0.29471431607472E+03 0.29215000000001E+03 0.29215000000003E+03 0.29255523002414E+03
 0.29471462323362E+03 0.29215000000001E+03 0.29215000000003E+03 0.29256057012859E+03 0.29471431607472E+03
 0.29215000000001E+03 0.29215000000003E+03 0.29255523002414E+03 0.29471462323362E+03 0.29215000000001E+03
 0.29215000000003E+03 0.29432750440213E+03 0.29215000000052E+03 0.14859896739095E+02 0.10524581925973E+02
 0.22169492452404E+02 0.10370737622777E+03 0.81427036313104E+02 0.21928072708604E+02 0.18784126336286E+02
 0.21959971316056E+02 0.80002182129597E+02 0.21661815928879E+02 0.18753880412116E+02 0.21698772639660E+02
 0.79970017566733E+02 0.21928072708604E+02 0.18784126336286E+02 0.21959971316056E+02 0.80002182129597E+02
 0.21661815928881E+02 0.18753880412115E+02 0.21698772639663E+02 0.79970017566731E+02 0.45662185598493E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31770756731133E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45629021976897E-02 0.19385346243385E-01 0.00000000000000E+00 0.45629021976897E-02 0.19385346243385E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26250451176141E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.32963665860316E-03 0.26250451176141E-01 0.32963665860316E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.98243123270159E-04 0.66294619831834E-01 0.43614880704095E-01
 0.00000000000000E+00 0.66294619831834E-01 0.43713123827365E-01 0.43478629602723E+00 0.10978629304700E+00
 0.32500000298023E+00 0.42250006586313E+00
    282.21675719
 0.61061911443902E+00 0.30197855475670E+03 0.45337401047801E+03 0.42255902411046E+03 0.41139388699039E+03
 0.22999999659989E+00 0.00000000000000E+00 0.15368071510742E+00 0.00000000000000E+00 -.96059679428740E+01
 0.96736108817580E-03 0.10442917111064E+01 0.80000000000000E+04 0.30000000000000E+04 0.76606947224779E+01
 0.28727605209292E+01 0.31121752124800E+03 0.29215000000418E+03 0.30950187667548E+03 0.33831718091425E+03
 0.29215000000017E+03 0.29215000000038E+03 0.30810886636632E+03 0.33815565333754E+03 0.29215000000013E+03
 0.29215000000038E+03 0.30950187667547E+03 0.33831718091425E+03 0.29215000000017E+03 0.29215000000038E+03
 0.30810886636632E+03 0.33815565333755E+03 0.29215000000013E+03 0.29215000000038E+03 0.38399803716666E+03
 0.29476891176726E+03 0.14581423140597E+04 0.14235742614525E+04 0.92536398846883E+03 0.16501719439056E+04
 0.72018113549447E+03 0.88448939335007E+03 0.13826041757740E+04 0.86171266940817E+03 0.22830284364522E+04
 0.83484773190448E+03 0.13768206354741E+04 0.81738390410014E+03 0.22788257315064E+04 0.88448939335008E+03
 0.13826041757739E+04 0.86171266940819E+03 0.22830284364522E+04 0.83484773190449E+03 0.13768206354741E+04
 0.81738390410015E+03 0.22788257315063E+04 0.19309708478126E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65688646771286E+05 0.50050000000000E+08 0.52026931060182E+03 0.12394600755666E+01
 0.12394600755666E+01 0.94920056981972E+00 0.49343249499419E+00 0.29266837146860E+03 0.30610065859963E+03
 0.30168204063127E+03 0.30154797901989E+03 0.23000000000000E+00 0.00000000000000E+00 0.21783296640255E+00
 0.00000000000000E+00 -.60558921726102E-01 0.99787699992341E-03 0.23846898834123E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33547339029897E+02 0.12580252136211E+02 0.29476866414149E+03 0.38403935905806E+03
 0.29257610052636E+03 0.29470691739654E+03 0.29215000000001E+03 0.29215000000005E+03 0.29257060052456E+03
 0.29470711634848E+03 0.29215000000001E+03 0.29215000000005E+03 0.29257610052636E+03 0.29470691739654E+03
 0.29215000000001E+03 0.29215000000005E+03 0.29257060052456E+03 0.29470711634848E+03 0.29215000000001E+03
 0.29215000000005E+03 0.29432409276733E+03 0.29215000000090E+03 0.13535577466869E+02 0.86123391174088E+01
 0.22644890488856E+02 0.10119030587173E+03 0.78432190930432E+02 0.22427625141930E+02 0.19073513216771E+02
 0.22511058402093E+02 0.77964012704603E+02 0.22154013589203E+02 0.19033208397735E+02 0.22243801422533E+02
 0.77922473075343E+02 0.22427625141930E+02 0.19073513216771E+02 0.22511058402093E+02 0.77964012704603E+02
 0.22154013589206E+02 0.19033208397735E+02 0.22243801422536E+02 0.77922473075342E+02 0.44255485831981E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31746473968508E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45913448535434E-02 0.19351914465088E-01 0.00000000000000E+00 0.45913448535434E-02 0.19351914465088E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26348266092963E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.31480811276147E-03 0.26348266092963E-01 0.31480811276147E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.33468717372159E-03 0.66103854988723E-01 0.42394467887772E-01
 0.00000000000000E+00 0.66103854988723E-01 0.42729155061494E-01 0.43421624749710E+00 0.10921624451686E+00
 0.32500000298023E+00 0.42250006586313E+00
    292.55858089
 0.61059650322084E+00 0.30254383822587E+03 0.45382082298617E+03 0.42303109034869E+03 0.41190178744206E+03
 0.22999999663866E+00 0.00000000000000E+00 0.15023551816384E+00 0.00000000000000E+00 -.96353627008774E+01
 0.96555335689093E-03 0.10896274514239E+01 0.80000000000000E+04 0.30000000000000E+04 0.73419589324277E+01
 0.27532345996604E+01 0.31182006398733E+03 0.29215000000696E+03 0.31002889915344E+03 0.33957561555247E+03
 0.29215000000030E+03 0.29215000000066E+03 0.30862548318798E+03 0.33941915976261E+03 0.29215000000023E+03
 0.29215000000066E+03 0.31002889915344E+03 0.33957561555247E+03 0.29215000000030E+03 0.29215000000066E+03
 0.30862548318798E+03 0.33941915976261E+03 0.29215000000023E+03 0.29215000000066E+03 0.38573644039495E+03
 0.29504825499254E+03 0.14681660634180E+04 0.14334578907374E+04 0.92447038577345E+03 0.16328852750949E+04
 0.70379253739257E+03 0.89079221908105E+03 0.13911918469674E+04 0.86818877730748E+03 0.22832553984762E+04
 0.84118768261758E+03 0.13860293814984E+04 0.82392098363740E+03 0.22796199543338E+04 0.89079221908105E+03
 0.13911918469674E+04 0.86818877730749E+03 0.22832553984762E+04 0.84118768261758E+03 0.13860293814984E+04
 0.82392098363740E+03 0.22796199543338E+04 0.19302113485061E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65548776331450E+05 0.50050000000000E+08 0.52071754202550E+03 0.12395893833096E+01
 0.12395893833096E+01 0.99056786460291E+00 0.49197557435914E+00 0.29274171948569E+03 0.30577320934820E+03
 0.30149909287495E+03 0.30137307254427E+03 0.23000000000000E+00 0.00000000000000E+00 0.21740207621472E+00
 0.00000000000000E+00 -.60545019132743E-01 0.99762696170252E-03 0.24769038541756E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32298387305238E+02 0.12111895239464E+02 0.29504807042734E+03 0.38577657058586E+03
 0.29259253202340E+03 0.29470240171314E+03 0.29215000000001E+03 0.29215000000009E+03 0.29258685635549E+03
 0.29470248285593E+03 0.29215000000001E+03 0.29215000000009E+03 0.29259253202340E+03 0.29470240171314E+03
 0.29215000000001E+03 0.29215000000009E+03 0.29258685635549E+03 0.29470248285593E+03 0.29215000000001E+03
 0.29215000000009E+03 0.29432300714468E+03 0.29215000000151E+03 0.12176327384437E+02 0.66004516046531E+01
 0.23236313130649E+02 0.99293545482510E+02 0.75941050786208E+02 0.23022893327769E+02 0.19466863043542E+02
 0.23176517447778E+02 0.76415308962304E+02 0.22739957225886E+02 0.19416128957918E+02 0.22901052112834E+02
 0.76364074036430E+02 0.23022893327770E+02 0.19466863043541E+02 0.23176517447780E+02 0.76415308962304E+02
 0.22739957225891E+02 0.19416128957917E+02 0.22901052112840E+02 0.76364074036428E+02 0.43085741521053E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31728180916195E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46054781636352E-02 0.19346164635580E-01 0.00000000000000E+00 0.46054781636352E-02 0.19346164635580E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26455223345824E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29916982641569E-03 0.26455223345824E-01 0.29916982641569E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.52243670969449E-03 0.66011010125231E-01 0.41404770233843E-01
 0.00000000000000E+00 0.66011010125231E-01 0.41927206943537E-01 0.43348778717957E+00 0.10848778419934E+00
 0.32500000298023E+00 0.42250006586313E+00
    300.31494866
 0.61057990341717E+00 0.30296450595157E+03 0.45416788714080E+03 0.42339397184651E+03 0.41228920851554E+03
 0.22999999667698E+00 0.00000000000000E+00 0.14766282768791E+00 0.00000000000000E+00 -.96581076373812E+01
 0.96421246483775E-03 0.11233896604949E+01 0.80000000000000E+04 0.30000000000000E+04 0.71213046383883E+01
 0.26704892393956E+01 0.31226377491898E+03 0.29215000001003E+03 0.31041790089172E+03 0.34049439453768E+03
 0.29215000000044E+03 0.29215000000100E+03 0.30900648867331E+03 0.34034174750837E+03 0.29215000000035E+03
 0.29215000000099E+03 0.31041790089172E+03 0.34049439453768E+03 0.29215000000044E+03 0.29215000000100E+03
 0.30900648867331E+03 0.34034174750837E+03 0.29215000000035E+03 0.29215000000099E+03 0.38699035825078E+03
 0.29527302601454E+03 0.14754060639789E+04 0.14406169488832E+04 0.92370753314506E+03 0.16206439252986E+04
 0.69231785448777E+03 0.89534885050182E+03 0.13972802429507E+04 0.87288644543770E+03 0.22834396726486E+04
 0.84577633074359E+03 0.13925406913540E+04 0.82866842738377E+03 0.22801870376568E+04 0.89534885050183E+03
 0.13972802429507E+04 0.87288644543771E+03 0.22834396726486E+04 0.84577633074359E+03 0.13925406913540E+04
 0.82866842738378E+03 0.22801870376568E+04 0.19297336791770E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65446091825093E+05 0.50050000000000E+08 0.52106552332691E+03 0.12396828667784E+01
 0.12396828667784E+01 0.10215933356903E+01 0.49073148199668E+00 0.29280293143280E+03 0.30556458157935E+03
 0.30138955258659E+03 0.30126883860426E+03 0.23000000000000E+00 0.00000000000000E+00 0.21706344335042E+00
 0.00000000000000E+00 -.60881544084516E-01 0.99741838867967E-03 0.25489137057714E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31385919350215E+02 0.11769719756331E+02 0.29527289311618E+03 0.38702950727321E+03
 0.29260552954348E+03 0.29470097474477E+03 0.29215000000001E+03 0.29215000000013E+03 0.29259971033513E+03
 0.29470096129042E+03 0.29215000000001E+03 0.29215000000013E+03 0.29260552954347E+03 0.29470097474477E+03
 0.29215000000001E+03 0.29215000000013E+03 0.29259971033513E+03 0.29470096129042E+03 0.29215000000001E+03
 0.29215000000013E+03 0.29432379336970E+03 0.29215000000218E+03 0.11127797796924E+02 0.50198996893554E+01
 0.23748121030379E+02 0.98220638785201E+02 0.74353777149670E+02 0.23528889459410E+02 0.19822637683747E+02
 0.23748376478131E+02 0.75529797686952E+02 0.23237744406438E+02 0.19763857286947E+02 0.23465507426733E+02
 0.75471100033275E+02 0.23528889459411E+02 0.19822637683746E+02 0.23748376478133E+02 0.75529797686951E+02
 0.23237744406442E+02 0.19763857286946E+02 0.23465507426737E+02 0.75471100033273E+02 0.42338669470620E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31717746085258E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46058948459475E-02 0.19361388454171E-01 0.00000000000000E+00 0.46058948459475E-02 0.19361388454171E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26536956494303E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.28754306079229E-03 0.26536956494303E-01 0.28754306079229E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.63879871608973E-03 0.65994384887030E-01 0.40792058970664E-01
 0.00000000000000E+00 0.65994384887030E-01 0.41430857686754E-01 0.43286574099834E+00 0.10786573801811E+00
 0.32500000298023E+00 0.42250006586313E+00
    312.26183052
 0.61056266598145E+00 0.30360612896865E+03 0.45471803421862E+03 0.42396360497497E+03 0.41289314409414E+03
 0.22999999671871E+00 0.00000000000000E+00 0.14371888850034E+00 0.00000000000000E+00 -.96943893374269E+01
 0.96217441145414E-03 0.11749992833800E+01 0.80000000000000E+04 0.30000000000000E+04 0.68085147907386E+01
 0.25531930465270E+01 0.31293371931424E+03 0.29215000001723E+03 0.31100659730368E+03 0.34187123804372E+03
 0.29215000000080E+03 0.29215000000182E+03 0.30958244680273E+03 0.34172432914764E+03 0.29215000000063E+03
 0.29215000000181E+03 0.31100659730368E+03 0.34187123804372E+03 0.29215000000080E+03 0.29215000000182E+03
 0.30958244680273E+03 0.34172432914764E+03 0.29215000000063E+03 0.29215000000181E+03 0.38885331631997E+03
 0.29564647464534E+03 0.14862666567025E+04 0.14513880699878E+04 0.92229333942142E+03 0.16026607951793E+04
 0.67575598906079E+03 0.90220202775486E+03 0.14061131445109E+04 0.87997205162934E+03 0.22837140236515E+04
 0.85265656060086E+03 0.14019630079483E+04 0.83580874213332E+03 0.22809907457313E+04 0.90220202775486E+03
 0.14061131445109E+04 0.87997205162935E+03 0.22837140236515E+04 0.85265656060087E+03 0.14019630079483E+04
 0.83580874213333E+03 0.22809907457312E+04 0.19291079280800E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65339462999565E+05 0.50050000000000E+08 0.52161675954475E+03 0.12397815868640E+01
 0.12397815868640E+01 0.10693808631493E+01 0.48864522467472E+00 0.29290838610761E+03 0.30529627261680E+03
 0.30126075155911E+03 0.30114724141175E+03 0.23000000000000E+00 0.00000000000000E+00 0.21651635869567E+00
 0.00000000000000E+00 -.61949603443774E-01 0.99705926567130E-03 0.26645547156521E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30023778280875E+02 0.11258916855328E+02 0.29564642554031E+03 0.38889083670624E+03
 0.29262664346039E+03 0.29470233215571E+03 0.29215000000001E+03 0.29215000000024E+03 0.29262058339172E+03
 0.29470216457876E+03 0.29215000000001E+03 0.29215000000024E+03 0.29262664346038E+03 0.29470233215571E+03
 0.29215000000001E+03 0.29215000000024E+03 0.29262058339172E+03 0.29470216457876E+03 0.29215000000001E+03
 0.29215000000024E+03 0.29432792709989E+03 0.29215000000376E+03 0.94426114838994E+01 0.24381301404636E+01
 0.24633837352344E+02 0.97056984339083E+02 0.72299977799978E+02 0.24399522005758E+02 0.20457476676448E+02
 0.24745214622194E+02 0.74552017197325E+02 0.24093560052339E+02 0.20386022495358E+02 0.24448773810333E+02
 0.74481588574779E+02 0.24399522005761E+02 0.20457476676447E+02 0.24745214622197E+02 0.74552017197323E+02
 0.24093560052345E+02 0.20386022495355E+02 0.24448773810341E+02 0.74481588574773E+02 0.41359574770490E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31705712155484E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45817557430758E-02 0.19431897034689E-01 0.00000000000000E+00 0.45817557430758E-02 0.19431897034689E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26643553689865E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.27243526654912E-03 0.26643553689865E-01 0.27243526654912E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.78732737973751E-03 0.66024739297996E-01 0.40049011330481E-01
 0.00000000000000E+00 0.66024739297996E-01 0.40836338710219E-01 0.43182261233736E+00 0.10682260935713E+00
 0.32500000298023E+00 0.42250006586313E+00
    320.64285871
 0.61055966606576E+00 0.30405116614591E+03 0.45511073330679E+03 0.42436710701286E+03 0.41331870241774E+03
 0.22999999654436E+00 0.00000000000000E+00 0.14096581976188E+00 0.00000000000000E+00 -.97198549794977E+01
 0.96076584328530E-03 0.12109279990575E+01 0.80000000000000E+04 0.30000000000000E+04 0.66065034471303E+01
 0.24774387926739E+01 0.31339560771928E+03 0.29215000002490E+03 0.31141347373426E+03 0.34281187603116E+03
 0.29215000000121E+03 0.29215000000275E+03 0.30998002888406E+03 0.34266894103626E+03 0.29215000000094E+03
 0.29215000000274E+03 0.31141347373426E+03 0.34281187603116E+03 0.29215000000121E+03 0.29215000000275E+03
 0.30998002888406E+03 0.34266894103626E+03 0.29215000000094E+03 0.29215000000274E+03 0.39011026358600E+03
 0.29592770260933E+03 0.14937165168126E+04 0.14587897852085E+04 0.92115613521121E+03 0.15906482273555E+04
 0.66488631146827E+03 0.90692237758603E+03 0.14119036492228E+04 0.88485868600705E+03 0.22838220257093E+04
 0.85736878556887E+03 0.14081265688803E+04 0.84070751093680E+03 0.22814304751563E+04 0.90692237758602E+03
 0.14119036492228E+04 0.88485868600704E+03 0.22838220257093E+04 0.85736878556886E+03 0.14081265688803E+04
 0.84070751093679E+03 0.22814304751563E+04 0.19287145090108E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65320905809214E+05 0.50050000000000E+08 0.52200998191672E+03 0.12398027435957E+01
 0.12398027435957E+01 0.11029049758924E+01 0.48710690131817E+00 0.29299079735366E+03 0.30514083608789E+03
 0.30119525760874E+03 0.30108623983776E+03 0.23000000000000E+00 0.00000000000000E+00 0.21611458979709E+00
 0.00000000000000E+00 -.62991546254290E-01 0.99677879603076E-03 0.27490285982702E+00 0.80000000000000E+04
 0.30000000000000E+04 0.29101188707291E+02 0.10912945765234E+02 0.29592771284188E+03 0.39014658463777E+03
 0.29264240685355E+03 0.29470571238888E+03 0.29215000000002E+03 0.29215000000036E+03 0.29263616090192E+03
 0.29470542942044E+03 0.29215000000002E+03 0.29215000000036E+03 0.29264240685355E+03 0.29470571238888E+03
 0.29215000000002E+03 0.29215000000036E+03 0.29263616090192E+03 0.29470542942044E+03 0.29215000000002E+03
 0.29215000000036E+03 0.29433283031667E+03 0.29215000000546E+03 0.82083748939397E+01 0.51991735322857E+00
 0.25325954389118E+02 0.96557614862830E+02 0.71105030701766E+02 0.25082176319329E+02 0.20963116429748E+02
 0.25535763548250E+02 0.74114700626200E+02 0.24764585301634E+02 0.20882430179895E+02 0.25228593217503E+02
 0.74035741137106E+02 0.25082176319330E+02 0.20963116429748E+02 0.25535763548251E+02 0.74114700626198E+02
 0.24764585301638E+02 0.20882430179894E+02 0.25228593217508E+02 0.74035741137105E+02 0.40782022633075E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31700838540325E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45654733213145E-02 0.19480497030264E-01 0.00000000000000E+00 0.45654733213145E-02 0.19480497030264E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26742642701279E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25929215778336E-03 0.26742642701279E-01 0.25929215778336E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.87238682025496E-03 0.66113246844033E-01 0.39613002648721E-01
 0.00000000000000E+00 0.66113246844033E-01 0.40485389468975E-01 0.43105345065909E+00 0.10605344767885E+00
 0.32500000298023E+00 0.42250006586313E+00
    333.21440099
 0.61056107436358E+00 0.30471053955929E+03 0.45571292334448E+03 0.42498086411936E+03 0.41396212424912E+03
 0.22999999625273E+00 0.00000000000000E+00 0.13685761690951E+00 0.00000000000000E+00 -.97588684113840E+01
 0.95868644057706E-03 0.12643815893462E+01 0.80000000000000E+04 0.30000000000000E+04 0.63272038025617E+01
 0.23727014259606E+01 0.31407869494589E+03 0.29215000004201E+03 0.31201668332750E+03 0.34418743540780E+03
 0.29215000000217E+03 0.29215000000494E+03 0.31056884869215E+03 0.34405037048830E+03 0.29215000000169E+03
 0.29215000000491E+03 0.31201668332750E+03 0.34418743540780E+03 0.29215000000217E+03 0.29215000000494E+03
 0.31056884869215E+03 0.34405037048830E+03 0.29215000000169E+03 0.29215000000491E+03 0.39192055454460E+03
 0.29637573300407E+03 0.15046147894167E+04 0.14696230009032E+04 0.91940940434866E+03 0.15737378339553E+04
 0.64973138258487E+03 0.91384307781310E+03 0.14201197968333E+04 0.89202336939462E+03 0.22839965839329E+04
 0.86426397312471E+03 0.14168446431551E+04 0.84787864237687E+03 0.22820462844358E+04 0.91384307781309E+03
 0.14201197968334E+04 0.89202336939460E+03 0.22839965839329E+04 0.86426397312469E+03 0.14168446431551E+04
 0.84787864237685E+03 0.22820462844358E+04 0.19282985645758E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65329617298217E+05 0.50050000000000E+08 0.52261267597091E+03 0.12398018039636E+01
 0.12398018039636E+01 0.11531911450072E+01 0.48473639455908E+00 0.29312899177958E+03 0.30495310234349E+03
 0.30113205119441E+03 0.30102907057494E+03 0.23000000000000E+00 0.00000000000000E+00 0.21548383488432E+00
 0.00000000000000E+00 -.65055104858559E-01 0.99630883317969E-03 0.28809910767189E+00 0.80000000000000E+04
 0.30000000000000E+04 0.27768222069993E+02 0.10413083276247E+02 0.29637584615342E+03 0.39195493108390E+03
 0.29266780265827E+03 0.29471408908893E+03 0.29215000000003E+03 0.29215000000063E+03 0.29266125184111E+03
 0.29471362072876E+03 0.29215000000003E+03 0.29215000000063E+03 0.29266780265827E+03 0.29471408908893E+03
 0.29215000000003E+03 0.29215000000063E+03 0.29266125184111E+03 0.29471362072876E+03 0.29215000000003E+03
 0.29215000000063E+03 0.29434295857925E+03 0.29215000000924E+03 0.62855431178025E+01 -.24987666380400E+01
 0.26454262981806E+02 0.96230516433264E+02 0.69643982136549E+02 0.26197893648176E+02 0.21802253933835E+02
 0.26847632767410E+02 0.73793794867252E+02 0.25861027103898E+02 0.21707475146501E+02 0.26522605608702E+02
 0.73701863871210E+02 0.26197893648177E+02 0.21802253933833E+02 0.26847632767411E+02 0.73793794867250E+02
 0.25861027103900E+02 0.21707475146499E+02 0.26522605608706E+02 0.73701863871208E+02 0.40062384805093E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31697422607738E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45243984350007E-02 0.19585512735495E-01 0.00000000000000E+00 0.45243984350007E-02 0.19585512735495E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26889164217957E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24052861928503E-03 0.26889164217957E-01 0.24052861928503E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97654344326518E-03 0.66306195663373E-01 0.39109380889574E-01
 0.00000000000000E+00 0.66306195663373E-01 0.40085924332839E-01 0.42986819727954E+00 0.10486819429931E+00
 0.32500000298023E+00 0.42250006586313E+00
    341.59542917
 0.61056931036963E+00 0.30514449402304E+03 0.45612006015206E+03 0.42539304438737E+03 0.41439217353333E+03
 0.22999999638970E+00 0.00000000000000E+00 0.13413322513768E+00 0.00000000000000E+00 -.97851722659514E+01
 0.95732281744560E-03 0.12997299621424E+01 0.80000000000000E+04 0.30000000000000E+04 0.61551247051449E+01
 0.23081717644294E+01 0.31452798276536E+03 0.29215000005851E+03 0.31241431478126E+03 0.34508344401031E+03
 0.29215000000313E+03 0.29215000000715E+03 0.31095659524776E+03 0.34495019492158E+03 0.29215000000245E+03
 0.29215000000712E+03 0.31241431478126E+03 0.34508344401031E+03 0.29215000000313E+03 0.29215000000715E+03
 0.31095659524776E+03 0.34495019492158E+03 0.29215000000245E+03 0.29215000000712E+03 0.39308495552254E+03
 0.29669172263150E+03 0.15117578024488E+04 0.14767245879640E+04 0.91815539529853E+03 0.15630037354328E+04
 0.64025756315775E+03 0.91839494658043E+03 0.14252811044252E+04 0.89673274922777E+03 0.22840646664109E+04
 0.86877713012665E+03 0.14223066674510E+04 0.85257173222435E+03 0.22823758402638E+04 0.91839494658043E+03
 0.14252811044252E+04 0.89673274922777E+03 0.22840646664110E+04 0.86877713012665E+03 0.14223066674510E+04
 0.85257173222435E+03 0.22823758402638E+04 0.19280812434983E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65380564343543E+05 0.50050000000000E+08 0.52301994039795E+03 0.12397624093693E+01
 0.12397624093693E+01 0.11867152577504E+01 0.48314773245309E+00 0.29323120955500E+03 0.30485406972491E+03
 0.30111036403444E+03 0.30101103835423E+03 0.23000000000000E+00 0.00000000000000E+00 0.21504494886030E+00
 0.00000000000000E+00 -.66730209961344E-01 0.99596150245075E-03 0.29724137174873E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26914153816928E+02 0.10092807681348E+02 0.29669190872795E+03 0.39311799799280E+03
 0.29268590997362E+03 0.29472194596636E+03 0.29215000000003E+03 0.29215000000091E+03 0.29267913812643E+03
 0.29472134675977E+03 0.29215000000003E+03 0.29215000000091E+03 0.29268590997362E+03 0.29472194596636E+03
 0.29215000000003E+03 0.29215000000091E+03 0.29267913812643E+03 0.29472134675977E+03 0.29215000000003E+03
 0.29215000000091E+03 0.29435161469784E+03 0.29215000001289E+03 0.49513156248826E+01 -.46091476817265E+01
 0.27259434561642E+02 0.96253850680344E+02 0.68858118945894E+02 0.27000309294411E+02 0.22408857409078E+02
 0.27805842196176E+02 0.73771358529758E+02 0.26649428301729E+02 0.22304547160925E+02 0.27467802954937E+02
 0.73670684499816E+02 0.27000309294412E+02 0.22408857409078E+02 0.27805842196177E+02 0.73771358529756E+02
 0.26649428301731E+02 0.22304547160924E+02 0.27467802954940E+02 0.73670684499814E+02 0.39663253800393E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31697525184718E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44891096106068E-02 0.19670982306628E-01 0.00000000000000E+00 0.44891096106068E-02 0.19670982306628E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26990649744603E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22808811024840E-03 0.26990649744603E-01 0.22808811024840E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10326881715182E-02 0.66472955690372E-01 0.38853725455401E-01
 0.00000000000000E+00 0.66472955690372E-01 0.39886413626919E-01 0.42907386622655E+00 0.10407386324631E+00
 0.32500000298023E+00 0.42250006586313E+00
    354.16697145
 0.61059111425268E+00 0.30578683927104E+03 0.45673691985147E+03 0.42601399388543E+03 0.41503740315344E+03
 0.22999999642319E+00 0.00000000000000E+00 0.13006867346603E+00 0.00000000000000E+00 -.98249421440887E+01
 0.95531146071978E-03 0.13523223242073E+01 0.80000000000000E+04 0.30000000000000E+04 0.59157494162418E+01
 0.22184060310907E+01 0.31519359646283E+03 0.29215000009393E+03 0.31300463209584E+03 0.34639821406612E+03
 0.29215000000531E+03 0.29215000001216E+03 0.31153165647808E+03 0.34627050869048E+03 0.29215000000415E+03
 0.29215000001210E+03 0.31300463209584E+03 0.34639821406612E+03 0.29215000000531E+03 0.29215000001216E+03
 0.31153165647808E+03 0.34627050869049E+03 0.29215000000415E+03 0.29215000001210E+03 0.39477298311798E+03
 0.29719060984668E+03 0.15222987326449E+04 0.14872014139555E+04 0.91616020820593E+03 0.15476320841015E+04
 0.62689107485458E+03 0.92513343540243E+03 0.14325945758777E+04 0.90369685566208E+03 0.22840947211096E+04
 0.87543014796027E+03 0.14300257864322E+04 0.85948583619791E+03 0.22827547755859E+04 0.92513343540243E+03
 0.14325945758778E+04 0.90369685566207E+03 0.22840947211097E+04 0.87543014796026E+03 0.14300257864322E+04
 0.85948583619791E+03 0.22827547755859E+04 0.19278294153741E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65515440719574E+05 0.50050000000000E+08 0.52363672200225E+03 0.12396530970875E+01
 0.12396530970875E+01 0.12370014268651E+01 0.48079269090579E+00 0.29340035954416E+03 0.30473959033212E+03
 0.30110504414323E+03 0.30101076891999E+03 0.23000000000000E+00 0.00000000000000E+00 0.21435932422261E+00
 0.00000000000000E+00 -.69663118476141E-01 0.99538727152983E-03 0.31146803612192E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25684818575953E+02 0.96318069659824E+01 0.29719090788065E+03 0.39480401804489E+03
 0.29271493425912E+03 0.29473701345727E+03 0.29215000000006E+03 0.29215000000154E+03 0.29270780403896E+03
 0.29473620779436E+03 0.29215000000006E+03 0.29215000000154E+03 0.29271493425912E+03 0.29473701345727E+03
 0.29215000000006E+03 0.29215000000154E+03 0.29270780403896E+03 0.29473620779436E+03 0.29215000000006E+03
 0.29215000000154E+03 0.29436736036290E+03 0.29215000002072E+03 0.28697524105748E+01 -.79180451326918E+01
 0.28536952383631E+02 0.96601413317111E+02 0.67921776171562E+02 0.28286654944346E+02 0.23381476348695E+02
 0.29367217422783E+02 0.73986379844512E+02 0.27913117162213E+02 0.23262730491431E+02 0.29008123301645E+02
 0.73872511532279E+02 0.28286654944347E+02 0.23381476348694E+02 0.29367217422785E+02 0.73986379844511E+02
 0.27913117162217E+02 0.23262730491430E+02 0.29008123301649E+02 0.73872511532278E+02 0.39166312464682E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31700838950042E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44256520398908E-02 0.19820112979741E-01 0.00000000000000E+00 0.44256520398908E-02 0.19820112979741E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27149723003881E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20950311560445E-03 0.27149723003881E-01 0.20950311560445E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10996917294008E-02 0.66773478155350E-01 0.38570984546384E-01
 0.00000000000000E+00 0.66773478155350E-01 0.39670676275785E-01 0.42789634545290E+00 0.10289634247266E+00
 0.32500000298023E+00 0.42250006586313E+00
    362.54799964
 0.61061216024454E+00 0.30620930183373E+03 0.45715062464226E+03 0.42642842224550E+03 0.41546656174009E+03
 0.22999999647196E+00 0.00000000000000E+00 0.12737392626957E+00 0.00000000000000E+00 -.98515617363734E+01
 0.95399321183270E-03 0.13870986497506E+01 0.80000000000000E+04 0.30000000000000E+04 0.57674340620536E+01
 0.21627877732701E+01 0.31563223996558E+03 0.29215000012694E+03 0.31339442161348E+03 0.34725666257161E+03
 0.29215000000742E+03 0.29215000001703E+03 0.31191100159882E+03 0.34713252303120E+03 0.29215000000582E+03
 0.29215000001696E+03 0.31339442161348E+03 0.34725666257161E+03 0.29215000000742E+03 0.29215000001703E+03
 0.31191100159882E+03 0.34713252303120E+03 0.29215000000582E+03 0.29215000001696E+03 0.39586239835946E+03
 0.29753915116773E+03 0.15292246810118E+04 0.14940808891290E+04 0.91474985689482E+03 0.15378045680781E+04
 0.61848096189876E+03 0.92957570776178E+03 0.14371971966199E+04 0.90828105641050E+03 0.22840500125567E+04
 0.87979616884676E+03 0.14348715313514E+04 0.86401871086637E+03 0.22829168187352E+04 0.92957570776178E+03
 0.14371971966199E+04 0.90828105641050E+03 0.22840500125567E+04 0.87979616884676E+03 0.14348715313514E+04
 0.86401871086637E+03 0.22829168187352E+04 0.19276914851244E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65645628870374E+05 0.50050000000000E+08 0.52405020742868E+03 0.12395458165688E+01
 0.12395458165688E+01 0.12705255396083E+01 0.47926075487688E+00 0.29352394170047E+03 0.30468334078329E+03
 0.30111783276436E+03 0.30102669496223E+03 0.23000000000000E+00 0.00000000000000E+00 0.21388423385800E+00
 0.00000000000000E+00 -.71881099873084E-01 0.99496815330260E-03 0.32129174488986E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24899488166876E+02 0.93373080625784E+01 0.29753952504851E+03 0.39589210466255E+03
 0.29273557623511E+03 0.29474918565823E+03 0.29215000000008E+03 0.29215000000214E+03 0.29272818898704E+03
 0.29474823600612E+03 0.29215000000008E+03 0.29215000000215E+03 0.29273557623511E+03 0.29474918565823E+03
 0.29215000000008E+03 0.29215000000214E+03 0.29272818898704E+03 0.29474823600612E+03 0.29215000000008E+03
 0.29215000000215E+03 0.29437964935114E+03 0.29215000002800E+03 0.14279770323567E+01 -.10217037376246E+02
 0.29429695940979E+02 0.97015861982087E+02 0.67439017561403E+02 0.29196428357636E+02 0.24067173918488E+02
 0.30489927807146E+02 0.74275489088483E+02 0.28806759393509E+02 0.23938751580624E+02 0.30115842372628E+02
 0.74152809048333E+02 0.29196428357638E+02 0.24067173918488E+02 0.30489927807147E+02 0.74275489088483E+02
 0.28806759393512E+02 0.23938751580623E+02 0.30115842372631E+02 0.74152809048332E+02 0.38893063697522E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31704974881119E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43775186322155E-02 0.19931370962852E-01 0.00000000000000E+00 0.43775186322155E-02 0.19931370962852E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27262080118556E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19706013038694E-03 0.27262080118556E-01 0.19706013038694E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11340174768143E-02 0.67004685757400E-01 0.38439038573735E-01
 0.00000000000000E+00 0.67004685757400E-01 0.39573056050549E-01 0.42713037743844E+00 0.10213037445821E+00
 0.32500000298023E+00 0.42250006586313E+00
    370.92902782
 0.61063778246194E+00 0.30662715566708E+03 0.45756549436861E+03 0.42684261022421E+03 0.41589444052846E+03
 0.22999999648130E+00 0.00000000000000E+00 0.12469137517310E+00 0.00000000000000E+00 -.98782261002236E+01
 0.95269291410352E-03 0.14216466065160E+01 0.80000000000000E+04 0.30000000000000E+04 0.56272775268712E+01
 0.21102290725767E+01 0.31606710745636E+03 0.29215000016972E+03 0.31378143314955E+03 0.34810152355317E+03
 0.29215000001026E+03 0.29215000002358E+03 0.31228736080685E+03 0.34798084020105E+03 0.29215000000805E+03
 0.29215000002348E+03 0.31378143314955E+03 0.34810152355317E+03 0.29215000001026E+03 0.29215000002358E+03
 0.31228736080685E+03 0.34798084020105E+03 0.29215000000805E+03 0.29215000002348E+03 0.39692487647103E+03
 0.29789986720615E+03 0.15360710960697E+04 0.15008763712885E+04 0.91328354621458E+03 0.15282866916855E+04
 0.61043672773980E+03 0.93397779598923E+03 0.14415977514924E+04 0.91281748889900E+03 0.22839536428636E+04
 0.88410875477739E+03 0.14394954022252E+04 0.86849161465130E+03 0.22830086092316E+04 0.93397779598925E+03
 0.14415977514924E+04 0.91281748889903E+03 0.22839536428635E+04 0.88410875477740E+03 0.14394954022252E+04
 0.86849161465132E+03 0.22830086092316E+04 0.19275718062330E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.65804125026103E+05 0.50050000000000E+08 0.52446474107414E+03 0.12394143380903E+01
 0.12394143380903E+01 0.13040496523515E+01 0.47777093225656E+00 0.29365634963741E+03 0.30464147581092E+03
 0.30114255982900E+03 0.30105441190260E+03 0.23000000000000E+00 0.00000000000000E+00 0.21339482770437E+00
 0.00000000000000E+00 -.74299480598445E-01 0.99451949511469E-03 0.33138549865518E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24141068430772E+02 0.90529006615394E+01 0.29790031788316E+03 0.39695326882259E+03
 0.29275729336598E+03 0.29476299464266E+03 0.29215000000011E+03 0.29215000000296E+03 0.29274963456264E+03
 0.29476189625718E+03 0.29215000000011E+03 0.29215000000296E+03 0.29275729336598E+03 0.29476299464266E+03
 0.29215000000011E+03 0.29215000000296E+03 0.29274963456264E+03 0.29476189625718E+03 0.29215000000011E+03
 0.29215000000296E+03 0.29439331923092E+03 0.29215000003742E+03 -.56620086065171E-01 -.12587259422880E+02
 0.30351598920860E+02 0.97560173869181E+02 0.67056816953717E+02 0.30145792820441E+02 0.24779783358185E+02
 0.31677444342656E+02 0.74668601537559E+02 0.29739217535731E+02 0.24641674938042E+02 0.31287654228635E+02
 0.74537128483630E+02 0.30145792820442E+02 0.24779783358184E+02 0.31677444342657E+02 0.74668601537558E+02
 0.29739217535733E+02 0.24641674938042E+02 0.31287654228637E+02 0.74537128483628E+02 0.38660444800940E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31710522149482E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43251639259395E-02 0.20051395025112E-01 0.00000000000000E+00 0.43251639259395E-02 0.20051395025112E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27379963809658E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.18459913973029E-03 0.27379963809658E-01 0.18459913973029E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11607394024589E-02 0.67258353048195E-01 0.38346300230132E-01
 0.00000000000000E+00 0.67258353048195E-01 0.39507039632591E-01 0.42638546612828E+00 0.10138546314805E+00
 0.32500000298023E+00 0.42250006586313E+00
    383.50057010
 0.61068451470754E+00 0.30724534347956E+03 0.45818853320209E+03 0.42746231034739E+03 0.41653294917372E+03
 0.22999999656031E+00 0.00000000000000E+00 0.12069085695311E+00 0.00000000000000E+00 -.99182309016567E+01
 0.95077568900235E-03 0.14730403097449E+01 0.80000000000000E+04 0.30000000000000E+04 0.54309443856192E+01
 0.20366041446072E+01 0.31671282804825E+03 0.29215000025754E+03 0.31435710909524E+03 0.34934488570894E+03
 0.29215000001633E+03 0.29215000003763E+03 0.31284669214204E+03 0.34922917629523E+03 0.29215000001285E+03
 0.29215000003747E+03 0.31435710909525E+03 0.34934488570894E+03 0.29215000001633E+03 0.29215000003763E+03
 0.31284669214204E+03 0.34922917629523E+03 0.29215000001285E+03 0.29215000003747E+03 0.39847142371285E+03
 0.29846265019774E+03 0.15462043472347E+04 0.15109226428587E+04 0.91098600455649E+03 0.15145344558388E+04
 0.59899352125953E+03 0.94051308462457E+03 0.14478428434925E+04 0.91953895902054E+03 0.22837064300765E+04
 0.89048563782976E+03 0.14460418717568E+04 0.87509562108072E+03 0.22830123395519E+04 0.94051308462455E+03
 0.14478428434925E+04 0.91953895902051E+03 0.22837064300765E+04 0.89048563782975E+03 0.14460418717568E+04
 0.87509562108070E+03 0.22830123395519E+04 0.19274144309649E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66093205460385E+05 0.50050000000000E+08 0.52508707599235E+03 0.12391733258840E+01
 0.12391733258840E+01 0.13543358214662E+01 0.47563235419782E+00 0.29387168041300E+03 0.30460298299128E+03
 0.30120021318533E+03 0.30111632669586E+03 0.23000000000000E+00 0.00000000000000E+00 0.21263402576667E+00
 0.00000000000000E+00 -.78285305404752E-01 0.99379072108789E-03 0.34703004447420E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23052759054684E+02 0.86447846455066E+01 0.29846321775766E+03 0.39849788404987E+03
 0.29279195519713E+03 0.29478666878362E+03 0.29215000000018E+03 0.29215000000469E+03 0.29278386176601E+03
 0.29478533909489E+03 0.29215000000018E+03 0.29215000000469E+03 0.29279195519713E+03 0.29478666878362E+03
 0.29215000000018E+03 0.29215000000469E+03 0.29278386176601E+03 0.29478533909489E+03 0.29215000000018E+03
 0.29215000000469E+03 0.29441632259015E+03 0.29215000005671E+03 -.23623722380454E+01 -.16269552674584E+02
 0.31782767030070E+02 0.98593872033347E+02 0.66652191168127E+02 0.31640236869520E+02 0.25894205599512E+02
 0.33578963085173E+02 0.75433040611070E+02 0.31206929049038E+02 0.25741610679146E+02 0.33164365397781E+02
 0.75288470671236E+02 0.31640236869520E+02 0.25894205599512E+02 0.33578963085173E+02 0.75433040611070E+02
 0.31206929049039E+02 0.25741610679144E+02 0.33164365397783E+02 0.75288470671233E+02 0.38378695036698E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31721289518689E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42395475274688E-02 0.20246538205173E-01 0.00000000000000E+00 0.42395475274688E-02 0.20246538205173E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27568222481597E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16592326262304E-03 0.27568222481597E-01 0.16592326262304E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11875124124652E-02 0.67677595319421E-01 0.38271036970838E-01
 0.00000000000000E+00 0.67677595319421E-01 0.39458549383303E-01 0.42531617709891E+00 0.10031617411868E+00
 0.32500000298023E+00 0.42250006586313E+00
    391.88159829
 0.61072080890188E+00 0.30765179618544E+03 0.45860363368612E+03 0.42787382425157E+03 0.41695597307128E+03
 0.22999999658221E+00 0.00000000000000E+00 0.11803969560008E+00 0.00000000000000E+00 -.99448722531010E+01
 0.94951932656253E-03 0.15070166300261E+01 0.80000000000000E+04 0.30000000000000E+04 0.53085014727818E+01
 0.19906880522932E+01 0.31713921932735E+03 0.29215000033619E+03 0.31473788727703E+03 0.35015871226067E+03
 0.29215000002198E+03 0.29215000005072E+03 0.31321634200033E+03 0.35004617755785E+03 0.29215000001732E+03
 0.29215000005051E+03 0.31473788727703E+03 0.35015871226067E+03 0.29215000002198E+03 0.29215000005072E+03
 0.31321634200033E+03 0.35004617755785E+03 0.29215000001732E+03 0.29215000005051E+03 0.39947287485035E+03
 0.29885154306061E+03 0.15528738343966E+04 0.15175259045084E+04 0.90939655023263E+03 0.15056891413356E+04
 0.59174560835184E+03 0.94482707783497E+03 0.14517851125191E+04 0.92396628966526E+03 0.22834734984056E+04
 0.89467919144398E+03 0.14501647750797E+04 0.87943114279466E+03 0.22829279451509E+04 0.94482707783499E+03
 0.14517851125191E+04 0.92396628966528E+03 0.22834734984056E+04 0.89467919144399E+03 0.14501647750797E+04
 0.87943114279468E+03 0.22829279451509E+04 0.19273196800031E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66317717268027E+05 0.50050000000000E+08 0.52550158787045E+03 0.12389855417633E+01
 0.12389855417633E+01 0.13878599342094E+01 0.47427942909779E+00 0.29402641941813E+03 0.30459205417169E+03
 0.30125134535904E+03 0.30117017843953E+03 0.23000000000000E+00 0.00000000000000E+00 0.21210909367871E+00
 0.00000000000000E+00 -.81171249384247E-01 0.99326767610874E-03 0.35779457578353E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22359198661637E+02 0.83846994981139E+01 0.29885218957127E+03 0.39949807910996E+03
 0.29281649839172E+03 0.29480435145426E+03 0.29215000000024E+03 0.29215000000629E+03 0.29280809703961E+03
 0.29480286256923E+03 0.29215000000024E+03 0.29215000000629E+03 0.29281649839172E+03 0.29480435145426E+03
 0.29215000000024E+03 0.29215000000629E+03 0.29280809703960E+03 0.29480286256924E+03 0.29215000000024E+03
 0.29215000000629E+03 0.29443326137349E+03 0.29215000007393E+03 -.39506257942958E+01 -.18804102874103E+02
 0.32765323259310E+02 0.99413224690908E+02 0.66484074815301E+02 0.32681069905641E+02 0.26664659953701E+02
 0.34925921002344E+02 0.76048025284969E+02 0.32229074127336E+02 0.26502475885412E+02 0.34494004142017E+02
 0.75894823810844E+02 0.32681069905642E+02 0.26664659953701E+02 0.34925921002345E+02 0.76048025284969E+02
 0.32229074127337E+02 0.26502475885410E+02 0.34494004142019E+02 0.75894823810841E+02 0.38230963898196E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31729983429409E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41781205871454E-02 0.20386174192079E-01 0.00000000000000E+00 0.41781205871454E-02 0.20386174192079E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27701757723784E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.15355220353833E-03 0.27701757723784E-01 0.15355220353833E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11969911838714E-02 0.67980881821394E-01 0.38258341035333E-01
 0.00000000000000E+00 0.67980881821394E-01 0.39455332219205E-01 0.42463971454889E+00 0.99639711568662E-01
 0.32500000298023E+00 0.42250006586313E+00
    400.26262647
 0.61076107086384E+00 0.30805376727742E+03 0.45901799492109E+03 0.42828363714183E+03 0.41737656111740E+03
 0.22999999661940E+00 0.00000000000000E+00 0.11540147066514E+00 0.00000000000000E+00 -.99714673711638E+01
 0.94828007537166E-03 0.15407633859914E+01 0.80000000000000E+04 0.30000000000000E+04 0.51922313787670E+01
 0.19470867670376E+01 0.31756255209865E+03 0.29215000043509E+03 0.31511641862980E+03 0.35096111040438E+03
 0.29215000002930E+03 0.29215000006771E+03 0.31358356899437E+03 0.35085163807085E+03 0.29215000002313E+03
 0.29215000006743E+03 0.31511641862980E+03 0.35096111040438E+03 0.29215000002930E+03 0.29215000006771E+03
 0.31358356899437E+03 0.35085163807086E+03 0.29215000002313E+03 0.29215000006743E+03 0.40045206060641E+03
 0.29925078330389E+03 0.15594800856100E+04 0.15240585743610E+04 0.90776557048519E+03 0.14970814162959E+04
 0.58477701795827E+03 0.94910999378849E+03 0.14555617095559E+04 0.92835358843330E+03 0.22831859573556E+04
 0.89883018227550E+03 0.14541073291676E+04 0.88371622262970E+03 0.22827754661621E+04 0.94910999378849E+03
 0.14555617095560E+04 0.92835358843329E+03 0.22831859573556E+04 0.89883018227549E+03 0.14541073291676E+04
 0.88371622262968E+03 0.22827754661620E+04 0.19272299163240E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66566773244119E+05 0.50050000000000E+08 0.52591527326154E+03 0.12387768519563E+01
 0.12387768519563E+01 0.14213840469526E+01 0.47299008846207E+00 0.29419007426736E+03 0.30459191083748E+03
 0.30131193377117E+03 0.30123340641778E+03 0.23000000000000E+00 0.00000000000000E+00 0.21157001216350E+00
 0.00000000000000E+00 -.84232403408538E-01 0.99271509328736E-03 0.36882629707408E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21690427346055E+02 0.81339102547707E+01 0.29925150948013E+03 0.40047603821203E+03
 0.29284222289823E+03 0.29482349975626E+03 0.29215000000033E+03 0.29215000000836E+03 0.29283349913549E+03
 0.29482184807769E+03 0.29215000000033E+03 0.29215000000836E+03 0.29284222289823E+03 0.29482349975626E+03
 0.29215000000033E+03 0.29215000000836E+03 0.29283349913549E+03 0.29482184807769E+03 0.29215000000033E+03
 0.29215000000836E+03 0.29445143590359E+03 0.29215000009552E+03 -.55783151613637E+01 -.21398291813735E+02
 0.33767895985862E+02 0.10032671947562E+03 0.66389984009828E+02 0.33755732786538E+02 0.27455101629497E+02
 0.36335518973811E+02 0.76739669038998E+02 0.33284392219475E+02 0.27283414572296E+02 0.35885697115678E+02
 0.76577945324959E+02 0.33755732786538E+02 0.27455101629497E+02 0.36335518973812E+02 0.76739669038998E+02
 0.33284392219477E+02 0.27283414572294E+02 0.35885697115681E+02 0.76577945324955E+02 0.38112262596821E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31739810576772E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41134707233738E-02 0.20533103155772E-01 0.00000000000000E+00 0.41134707233738E-02 0.20533103155772E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27841984761810E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14130880480061E-03 0.27841984761810E-01 0.14130880480061E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.12001086187100E-02 0.68301790060388E-01 0.38272210313348E-01
 0.00000000000000E+00 0.68301790060388E-01 0.39472318932058E-01 0.42399504423104E+00 0.98995041250804E-01
 0.32500000298023E+00 0.42250006586313E+00
    412.83416875
 0.61082862050953E+00 0.30864844576119E+03 0.45963730580138E+03 0.42889453343783E+03 0.41800238919946E+03
 0.22999999670152E+00 0.00000000000000E+00 0.11146889577388E+00 0.00000000000000E+00 -.10011238374057E+02
 0.94645263566787E-03 0.15909513481231E+01 0.80000000000000E+04 0.30000000000000E+04 0.50284378648272E+01
 0.18856641993102E+01 0.31819217589914E+03 0.29215000063100E+03 0.31568027256962E+03 0.35214432012680E+03
 0.29215000004434E+03 0.29215000010272E+03 0.31413014886948E+03 0.35203923417769E+03 0.29215000003510E+03
 0.29215000010229E+03 0.31568027256962E+03 0.35214432012680E+03 0.29215000004434E+03 0.29215000010272E+03
 0.31413014886948E+03 0.35203923417769E+03 0.29215000003510E+03 0.29215000010229E+03 0.40188139885866E+03
 0.29986789248859E+03 0.15692793327765E+04 0.15337325133998E+04 0.90524990560198E+03 0.14845825995265E+04
 0.57480644439650E+03 0.95548084543560E+03 0.14609351636942E+04 0.93486378112595E+03 0.22826531328572E+04
 0.90498243974211E+03 0.14597047978668E+04 0.89005441960866E+03 0.22824225131826E+04 0.95548084543561E+03
 0.14609351636942E+04 0.93486378112597E+03 0.22826531328572E+04 0.90498243974212E+03 0.14597047978668E+04
 0.89005441960867E+03 0.22824225131826E+04 0.19271000982929E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.66984627773777E+05 0.50050000000000E+08 0.52653342131281E+03 0.12384261368524E+01
 0.12384261368524E+01 0.14716702160674E+01 0.47118243767857E+00 0.29445209045669E+03 0.30461036401734E+03
 0.30141943061807E+03 0.30134473604557E+03 0.23000000000000E+00 0.00000000000000E+00 0.21073488810748E+00
 0.00000000000000E+00 -.89138649800260E-01 0.99183167212966E-03 0.38587410646034E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20732150372527E+02 0.77745563896974E+01 0.29986873926737E+03 0.40190359688519E+03
 0.29288308160763E+03 0.29485487501556E+03 0.29215000000052E+03 0.29215000001260E+03 0.29287384719401E+03
 0.29485297317290E+03 0.29215000000051E+03 0.29215000001260E+03 0.29288308160763E+03 0.29485487501556E+03
 0.29215000000052E+03 0.29215000001260E+03 0.29287384719401E+03 0.29485297317290E+03 0.29215000000051E+03
 0.29215000001260E+03 0.29448093124679E+03 0.29215000013808E+03 -.80906399020993E+01 -.25393472287291E+02
 0.35304716686159E+02 0.10185775650737E+03 0.66376516237781E+02 0.35428011826790E+02 0.28674973192357E+02
 0.38565700725592E+02 0.77909017508011E+02 0.34926484132037E+02 0.28489250061075E+02 0.38087988720727E+02
 0.77734768628518E+02 0.35428011826790E+02 0.28674973192357E+02 0.38565700725592E+02 0.77909017508011E+02
 0.34926484132039E+02 0.28489250061074E+02 0.38087988720730E+02 0.77734768628516E+02 0.37984208698491E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31756552329810E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.40108660670491E-02 0.20766675789000E-01 0.00000000000000E+00 0.40108660670491E-02 0.20766675789000E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28065300218905E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.12331329346962E-03 0.28065300218905E-01 0.12331329346962E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11933904565235E-02 0.68813907429934E-01 0.38337613199665E-01
 0.00000000000000E+00 0.68813907429934E-01 0.39531003656189E-01 0.42309121883929E+00 0.98091215859053E-01
 0.32500000298023E+00 0.42250006586313E+00
    421.21519694
 0.61087831488642E+00 0.30903946983204E+03 0.46004819400345E+03 0.42929887568512E+03 0.41841595405761E+03
 0.22999999671828E+00 0.00000000000000E+00 0.10886403720095E+00 0.00000000000000E+00 -.10037651480915E+02
 0.94525485365852E-03 0.16241205082556E+01 0.80000000000000E+04 0.30000000000000E+04 0.49257428616502E+01
 0.18471535731188E+01 0.31860855752405E+03 0.29215000080089E+03 0.31605370897208E+03 0.35292016263101E+03
 0.29215000005785E+03 0.29215000013419E+03 0.31449186475107E+03 0.35281786611360E+03 0.29215000004586E+03
 0.29215000013364E+03 0.31605370897208E+03 0.35292016263100E+03 0.29215000005785E+03 0.29215000013419E+03
 0.31449186475107E+03 0.35281786611360E+03 0.29215000004586E+03 0.29215000013364E+03 0.40280941708592E+03
 0.30029070620922E+03 0.15757446177624E+04 0.15401036905042E+04 0.90353149488316E+03 0.14765049654464E+04
 0.56845581308884E+03 0.95969585423073E+03 0.14643346021231E+04 0.93915999110568E+03 0.22822307332245E+04
 0.90903819153052E+03 0.14632385408560E+04 0.89422381166957E+03 0.22821064112817E+04 0.95969585423073E+03
 0.14643346021231E+04 0.93915999110567E+03 0.22822307332245E+04 0.90903819153051E+03 0.14632385408560E+04
 0.89422381166956E+03 0.22821064112817E+04 0.19270141543934E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.67292031601293E+05 0.50050000000000E+08 0.52694344306243E+03 0.12381678049867E+01
 0.12381678049867E+01 0.15051943288105E+01 0.47006497852827E+00 0.29463759943189E+03 0.30463422746608E+03
 0.30150151763792E+03 0.30142930538789E+03 0.23000000000000E+00 0.00000000000000E+00 0.21016047224834E+00
 0.00000000000000E+00 -.92609849772495E-01 0.99120715559983E-03 0.39757252673545E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20122114738887E+02 0.75457930270827E+01 0.30029163395967E+03 0.40283047147673E+03
 0.29291187018094E+03 0.29487750258247E+03 0.29215000000069E+03 0.29215000001639E+03 0.29290227741767E+03
 0.29487543050869E+03 0.29215000000068E+03 0.29215000001639E+03 0.29291187018094E+03 0.29487750258247E+03
 0.29215000000069E+03 0.29215000001639E+03 0.29290227741767E+03 0.29487543050869E+03 0.29215000000068E+03
 0.29215000001639E+03 0.29450203229038E+03 0.29215000017482E+03 -.98104266637820E+01 -.28120923598981E+02
 0.36348484058562E+02 0.10297662855667E+03 0.66446402077811E+02 0.36581085317731E+02 0.29509102221803E+02
 0.40128428248865E+02 0.78769752521263E+02 0.36058690570064E+02 0.29314203736417E+02 0.39631474418556E+02
 0.78587359589989E+02 0.36581085317731E+02 0.29509102221803E+02 0.40128428248865E+02 0.78769752521262E+02
 0.36058690570065E+02 0.29314203736415E+02 0.39631474418558E+02 0.78587359589986E+02 0.37929792016608E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31768976122358E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39389545810724E-02 0.20930911831557E-01 0.00000000000000E+00 0.39389545810724E-02 0.20930911831557E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28223063561906E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11165342484548E-03 0.28223063561906E-01 0.11165342484548E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11816357891883E-02 0.69174502274967E-01 0.38407987602373E-01
 0.00000000000000E+00 0.69174502274967E-01 0.39589623391561E-01 0.42253248926414E+00 0.97532486283904E-01
 0.32500000298023E+00 0.42250006586313E+00
    434.86112368
 0.61096972731713E+00 0.30966674297485E+03 0.46070945884073E+03 0.42994861653213E+03 0.41908001508786E+03
 0.22999999691808E+00 0.00000000000000E+00 0.10465253761428E+00 0.00000000000000E+00 -.10080475353548E+02
 0.94333970949722E-03 0.16776386309953E+01 0.80000000000000E+04 0.30000000000000E+04 0.47686074057879E+01
 0.17882277771704E+01 0.31927721356729E+03 0.29215000117704E+03 0.31665404261302E+03 0.35416148138101E+03
 0.29215000008904E+03 0.29215000020709E+03 0.31507281998853E+03 0.35406342577964E+03 0.29215000007080E+03
 0.29215000020625E+03 0.31665404261302E+03 0.35416148138100E+03 0.29215000008904E+03 0.29215000020709E+03
 0.31507281998853E+03 0.35406342577964E+03 0.29215000007080E+03 0.29215000020625E+03 0.40429172555216E+03
 0.30100491815926E+03 0.15861817232707E+04 0.15503864379007E+04 0.90045266205416E+03 0.14633601910772E+04
 0.55840526571273E+03 0.96652343593817E+03 0.14695182728606E+04 0.94611267864842E+03 0.22813482830487E+04
 0.91557331951789E+03 0.14686188980237E+04 0.90093812191225E+03 0.22813781172795E+04 0.96652343593818E+03
 0.14695182728606E+04 0.94611267864845E+03 0.22813482830488E+04 0.91557331951788E+03 0.14686188980238E+04
 0.90093812191224E+03 0.22813781172795E+04 0.19267481332206E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.67857498668728E+05 0.50050000000000E+08 0.52760314975039E+03 0.12376925918555E+01
 0.12376925918555E+01 0.15597780357744E+01 0.46840491057150E+00 0.29495688537886E+03 0.30469064801858E+03
 0.30165108653939E+03 0.30158280573875E+03 0.23000000000000E+00 0.00000000000000E+00 0.20919556212242E+00
 0.00000000000000E+00 -.98587042556867E-01 0.99013411908631E-03 0.41717846344420E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19176445336973E+02 0.71911670013649E+01 0.30100596112449E+03 0.40431105040353E+03
 0.29296030557455E+03 0.29491663924944E+03 0.29215000000109E+03 0.29215000002508E+03 0.29295010904780E+03
 0.29491429015171E+03 0.29215000000107E+03 0.29215000002509E+03 0.29296030557455E+03 0.29491663924944E+03
 0.29215000000109E+03 0.29215000002508E+03 0.29295010904780E+03 0.29491429015171E+03 0.29215000000107E+03
 0.29215000002509E+03 0.29453825972774E+03 0.29215000025561E+03 -.12731978139088E+02 -.32747842136139E+02
 0.38081794821953E+02 0.10495261985240E+03 0.66680416056336E+02 0.38531849762096E+02 0.30902842184102E+02
 0.42812634907089E+02 0.80298862665934E+02 0.37973597508999E+02 0.30693220385660E+02 0.42282608072885E+02
 0.80103430945675E+02 0.38531849762095E+02 0.30902842184102E+02 0.42812634907088E+02 0.80298862665934E+02
 0.37973597508999E+02 0.30693220385661E+02 0.42282608072885E+02 0.80103430945677E+02 0.37871068933489E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31790701552793E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.38122359781407E-02 0.21220718676339E-01 0.00000000000000E+00 0.38122359781407E-02 0.21220718676339E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28480613550776E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.94266100069159E-04 0.28480613550776E-01 0.94266100069159E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11517271978954E-02 0.69778729634733E-01 0.38571538697085E-01
 0.00000000000000E+00 0.69778729634733E-01 0.39723265894981E-01 0.42170245528575E+00 0.96702452305520E-01
 0.32500000298023E+00 0.42250006586313E+00
    441.68408705
 0.61101775282801E+00 0.30997610188628E+03 0.46103776609097E+03 0.43027064655736E+03 0.41940875290116E+03
 0.22999999646392E+00 0.00000000000000E+00 0.10256091648931E+00 0.00000000000000E+00 -.10101610257903E+02
 0.94239805147347E-03 0.17041638922653E+01 0.80000000000000E+04 0.30000000000000E+04 0.46943841706244E+01
 0.17603940639842E+01 0.31961009207284E+03 0.29215000141636E+03 0.31695338897194E+03 0.35477259650100E+03
 0.29215000010954E+03 0.29215000025510E+03 0.31536229303153E+03 0.35467658828196E+03 0.29215000008723E+03
 0.29215000025407E+03 0.31695338897195E+03 0.35477259650099E+03 0.29215000010954E+03 0.29215000025510E+03
 0.31536229303152E+03 0.35467658828196E+03 0.29215000008723E+03 0.29215000025407E+03 0.40501029625102E+03
 0.30136814845050E+03 0.15913270757822E+04 0.15554403240499E+04 0.89895098677276E+03 0.14570826768444E+04
 0.55363693513773E+03 0.96989762071218E+03 0.14719810218107E+04 0.94953602802254E+03 0.22808553305898E+04
 0.91879598228338E+03 0.14711700072520E+04 0.90423849279763E+03 0.22809530465479E+04 0.96989762071211E+03
 0.14719810218107E+04 0.94953602802243E+03 0.22808553305899E+04 0.91879598228343E+03 0.14711700072520E+04
 0.90423849279770E+03 0.22809530465479E+04 0.19266312403388E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.68154578939717E+05 0.50050000000000E+08 0.52793063412131E+03 0.12374426704080E+01
 0.12374426704080E+01 0.15870698892564E+01 0.46764689022260E+00 0.29512464102152E+03 0.30472698826755E+03
 0.30173331638192E+03 0.30166695277804E+03 0.23000000000000E+00 0.00000000000000E+00 0.20869903623088E+00
 0.00000000000000E+00 -.10170094000179E+00 0.98957126740368E-03 0.42724600385169E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18724575368473E+02 0.70217157631775E+01 0.30136925645663E+03 0.40502876919740E+03
 0.29298611050226E+03 0.29493761537059E+03 0.29215000000136E+03 0.29215000003077E+03 0.29297559398167E+03
 0.29493512397762E+03 0.29215000000134E+03 0.29215000003077E+03 0.29298611050226E+03 0.29493761537059E+03
 0.29215000000136E+03 0.29215000003077E+03 0.29297559398167E+03 0.29493512397761E+03 0.29215000000134E+03
 0.29215000003077E+03 0.29455756108648E+03 0.29215000030672E+03 -.14212892533865E+02 -.35085294541640E+02
 0.38959939624649E+02 0.10600882649580E+03 0.66854087173032E+02 0.39534376338372E+02 0.31613693113993E+02
 0.44211514623962E+02 0.81120588611199E+02 0.38957874419079E+02 0.31396919917705E+02 0.43664717591721E+02
 0.80918874601695E+02 0.39534376338373E+02 0.31613693113995E+02 0.44211514623964E+02 0.81120588611202E+02
 0.38957874419083E+02 0.31396919917706E+02 0.43664717591727E+02 0.80918874601696E+02 0.37868625676797E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31802865379435E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37506529349206E-02 0.21363433172146E-01 0.00000000000000E+00 0.37506529349206E-02 0.21363433172146E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28629680032989E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.85243155006199E-04 0.28629680032989E-01 0.85243155006199E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11307025302412E-02 0.70105336103225E-01 0.38660931593175E-01
 0.00000000000000E+00 0.70105336103225E-01 0.39791634123416E-01 0.42132344511130E+00 0.96323442131065E-01
 0.32500000298023E+00 0.42250006586313E+00
    455.33001379
 0.61111844085442E+00 0.31058661641097E+03 0.46169136596686E+03 0.43091039964876E+03 0.42006085649795E+03
 0.22999999585362E+00 0.00000000000000E+00 0.98406461804658E-01 0.00000000000000E+00 -.10143844980350E+02
 0.94054520449338E-03 0.17567368270192E+01 0.80000000000000E+04 0.30000000000000E+04 0.45538978160856E+01
 0.17077116810321E+01 0.32027174419515E+03 0.29215000202292E+03 0.31754918003450E+03 0.35597647972752E+03
 0.29215000016325E+03 0.29215000038113E+03 0.31593806776136E+03 0.35588438519264E+03 0.29215000013038E+03
 0.29215000037960E+03 0.31754918003452E+03 0.35597647972752E+03 0.29215000016325E+03 0.29215000038113E+03
 0.31593806776135E+03 0.35588438519264E+03 0.29215000013038E+03 0.29215000037960E+03 0.40641183978995E+03
 0.30210776458021E+03 0.16015243896544E+04 0.15654338476233E+04 0.89597204684239E+03 0.14449675006948E+04
 0.54451559361825E+03 0.97659781700419E+03 0.14767087313939E+04 0.95631401828635E+03 0.22798324813729E+04
 0.92518100663113E+03 0.14760569731727E+04 0.91076066320571E+03 0.22800503870843E+04 0.97659781700410E+03
 0.14767087313940E+04 0.95631401828621E+03 0.22798324813729E+04 0.92518100663119E+03 0.14760569731727E+04
 0.91076066320580E+03 0.22800503870843E+04 0.19264450689477E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.68777423556097E+05 0.50050000000000E+08 0.52858248987763E+03 0.12369180191855E+01
 0.12369180191855E+01 0.16416535962203E+01 0.46627266286838E+00 0.29547622838759E+03 0.30481534884539E+03
 0.30191229780225E+03 0.30184967871535E+03 0.23000000000000E+00 0.00000000000000E+00 0.20767735598413E+00
 0.00000000000000E+00 -.10822107338934E+00 0.98839370009509E-03 0.44791838657500E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17860396536012E+02 0.66976487010044E+01 0.30210901097302E+03 0.40642865884881E+03
 0.29304049063026E+03 0.29498209899290E+03 0.29215000000209E+03 0.29215000004557E+03 0.29302930557088E+03
 0.29497931963013E+03 0.29215000000206E+03 0.29215000004558E+03 0.29304049063026E+03 0.29498209899290E+03
 0.29215000000209E+03 0.29215000004557E+03 0.29302930557088E+03 0.29497931963013E+03 0.29215000000206E+03
 0.29215000004558E+03 0.29459828485399E+03 0.29215000043544E+03 -.17224537088336E+02 -.39820407575554E+02
 0.40734887131726E+02 0.10824592362931E+03 0.67307362061921E+02 0.41589054448796E+02 0.33061496810873E+02
 0.47120564240042E+02 0.82870846503157E+02 0.40975092201379E+02 0.32830926500650E+02 0.46539427422263E+02
 0.82657102401957E+02 0.41589054448800E+02 0.33061496810874E+02 0.47120564240048E+02 0.82870846503160E+02
 0.40975092201385E+02 0.32830926500650E+02 0.46539427422271E+02 0.82657102401956E+02 0.37910148235458E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31828798563137E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36202263961052E-02 0.21667219138014E-01 0.00000000000000E+00 0.36202263961052E-02 0.21667219138014E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28936148690827E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.68612333550720E-04 0.28936148690827E-01 0.68612333550720E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10787513268067E-02 0.70779630241138E-01 0.38878441306282E-01
 0.00000000000000E+00 0.70779630241138E-01 0.39957192633089E-01 0.42063633143419E+00 0.95636328453959E-01
 0.32500000298023E+00 0.42250006586313E+00
    462.15297716
 0.61117257661530E+00 0.31088784915692E+03 0.46201586171191E+03 0.43122742943459E+03 0.42038363599915E+03
 0.22999999607846E+00 0.00000000000000E+00 0.96343891830425E-01 0.00000000000000E+00 -.10164881548989E+02
 0.93963367427141E-03 0.17827852482354E+01 0.80000000000000E+04 0.30000000000000E+04 0.44873604422734E+01
 0.16827601658525E+01 0.32060057449582E+03 0.29215000240203E+03 0.31784565051735E+03 0.35656980761973E+03
 0.29215000019784E+03 0.29215000046249E+03 0.31622439318173E+03 0.35647958169878E+03 0.29215000015823E+03
 0.29215000046064E+03 0.31784565051736E+03 0.35656980761973E+03 0.29215000019784E+03 0.29215000046249E+03
 0.31622439318172E+03 0.35647958169878E+03 0.29215000015823E+03 0.29215000046064E+03 0.40709654235938E+03
 0.30248392479174E+03 0.16065966317083E+04 0.15703938637540E+04 0.89447213132752E+03 0.14390763047710E+04
 0.54013181278689E+03 0.97993919579362E+03 0.14789655567190E+04 0.95968426504115E+03 0.22792819186413E+04
 0.92835324368088E+03 0.14783855722558E+04 0.91399258624444E+03 0.22795529800187E+04 0.97993919579356E+03
 0.14789655567190E+04 0.95968426504106E+03 0.22792819186414E+04 0.92835324368092E+03 0.14783855722558E+04
 0.91399258624449E+03 0.22795529800187E+04 0.19263593734449E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.69112301346597E+05 0.50050000000000E+08 0.52890605412513E+03 0.12366357474494E+01
 0.12366357474494E+01 0.16689454497023E+01 0.46565754310404E+00 0.29565965304093E+03 0.30486683091497E+03
 0.30200856969647E+03 0.30194777983634E+03 0.23000000000000E+00 0.00000000000000E+00 0.20715225183883E+00
 0.00000000000000E+00 -.11161467116383E+00 0.98778047028487E-03 0.45852196581779E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17447364785963E+02 0.65427617947363E+01 0.30248524015853E+03 0.40711257269632E+03
 0.29306903568838E+03 0.29500556757895E+03 0.29215000000257E+03 0.29215000005506E+03 0.29305750234133E+03
 0.29500264314587E+03 0.29215000000254E+03 0.29215000005507E+03 0.29306903568838E+03 0.29500556757895E+03
 0.29215000000257E+03 0.29215000005506E+03 0.29305750234133E+03 0.29500264314587E+03 0.29215000000254E+03
 0.29215000005507E+03 0.29461966606536E+03 0.29215000051538E+03 -.18755304709326E+02 -.42218249071101E+02
 0.41630409620571E+02 0.10942216461160E+03 0.67583602942921E+02 0.42640610292559E+02 0.33797411670952E+02
 0.48629686259985E+02 0.83795728886122E+02 0.42007443970558E+02 0.33560205783613E+02 0.48030986618369E+02
 0.83576243893576E+02 0.42640610292560E+02 0.33797411670952E+02 0.48629686259986E+02 0.83795728886123E+02
 0.42007443970560E+02 0.33560205783613E+02 0.48030986618372E+02 0.83576243893576E+02 0.37952126861511E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31842570208395E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35522571824374E-02 0.21826695406261E-01 0.00000000000000E+00 0.35522571824374E-02 0.21826695406261E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29095463763845E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.60937972456383E-04 0.29095463763845E-01 0.60937972456383E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10480159563225E-02 0.71127687472298E-01 0.39003409458722E-01
 0.00000000000000E+00 0.71127687472298E-01 0.40051425415044E-01 0.42032877155202E+00 0.95328768571786E-01
 0.32500000298023E+00 0.42250006586313E+00
    475.79890390
 0.61128873252763E+00 0.31148243896603E+03 0.46265884315694E+03 0.43185469898829E+03 0.42102177525031E+03
 0.22999999633219E+00 0.00000000000000E+00 0.92248927007661E-01 0.00000000000000E+00 -.10206696966408E+02
 0.93783961750933E-03 0.18344036613087E+01 0.80000000000000E+04 0.30000000000000E+04 0.43610902925764E+01
 0.16354088597161E+01 0.32125448868770E+03 0.29215000334642E+03 0.31843594058708E+03 0.35773998265869E+03
 0.29215000028669E+03 0.29215000067188E+03 0.31679408495315E+03 0.35765332596100E+03 0.29215000022995E+03
 0.29215000066920E+03 0.31843594058709E+03 0.35773998265869E+03 0.29215000028669E+03 0.29215000067188E+03
 0.31679408495315E+03 0.35765332596100E+03 0.29215000022995E+03 0.29215000066920E+03 0.40843562435354E+03
 0.30324793947364E+03 0.16166862587022E+04 0.15802380047150E+04 0.89143519350747E+03 0.14275705035419E+04
 0.53167813406690E+03 0.98660422472614E+03 0.14832583542485E+04 0.96638695967934E+03 0.22780758548590E+04
 0.93465611539359E+03 0.14828079238023E+04 0.92039721497897E+03 0.22784409679619E+04 0.98660422472611E+03
 0.14832583542485E+04 0.96638695967929E+03 0.22780758548591E+04 0.93465611539361E+03 0.14828079238023E+04
 0.92039721497900E+03 0.22784409679618E+04 0.19261761574189E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.69830828822677E+05 0.50050000000000E+08 0.52954706847305E+03 0.12360298488649E+01
 0.12360298488649E+01 0.17235291566662E+01 0.46457106655040E+00 0.29604073902021E+03 0.30498343529115E+03
 0.30221375666153E+03 0.30215653886044E+03 0.23000000000000E+00 0.00000000000000E+00 0.20607353485782E+00
 0.00000000000000E+00 -.11864179346298E+00 0.98650884467597E-03 0.48026243372653E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16657559363794E+02 0.62465847614228E+01 0.30324938947306E+03 0.40845015699114E+03
 0.29312883006189E+03 0.29505488055206E+03 0.29215000000384E+03 0.29215000007927E+03 0.29311657237956E+03
 0.29505166489513E+03 0.29215000000378E+03 0.29215000007929E+03 0.29312883006189E+03 0.29505488055206E+03
 0.29215000000384E+03 0.29215000007927E+03 0.29311657237956E+03 0.29505166489513E+03 0.29215000000378E+03
 0.29215000007929E+03 0.29466438927709E+03 0.29215000071315E+03 -.21863502752366E+02 -.47069857471355E+02
 0.43434482102478E+02 0.11188107396245E+03 0.68229419449454E+02 0.44789692218153E+02 0.35291117182057E+02
 0.51752960197295E+02 0.85738290266661E+02 0.44117205802786E+02 0.35041205984106E+02 0.51118385937276E+02
 0.85507906126302E+02 0.44789692218153E+02 0.35291117182058E+02 0.51752960197295E+02 0.85738290266663E+02
 0.44117205802788E+02 0.35041205984107E+02 0.51118385937280E+02 0.85507906126303E+02 0.38075790730740E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31871722463879E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34122625452693E-02 0.22158257254810E-01 0.00000000000000E+00 0.34122625452693E-02 0.22158257254810E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29429602717018E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.46834188079426E-04 0.29429602717018E-01 0.46834188079426E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.97750265942566E-03 0.71846345931837E-01 0.39280005668980E-01
 0.00000000000000E+00 0.71846345931837E-01 0.40257508328406E-01 0.41978553327520E+00 0.94785530294968E-01
 0.32500000298023E+00 0.42250006586313E+00
    482.62186727
 0.61135040089991E+00 0.31177587872081E+03 0.46297718046304E+03 0.43216485498414E+03 0.42133709070264E+03
 0.22999999632908E+00 0.00000000000000E+00 0.90216885968019E-01 0.00000000000000E+00 -.10227474533667E+02
 0.93695674184192E-03 0.18599711110439E+01 0.80000000000000E+04 0.30000000000000E+04 0.43011420728520E+01
 0.16129282773195E+01 0.32157964051967E+03 0.29215000392757E+03 0.31872981269266E+03 0.35831702131869E+03
 0.29215000034292E+03 0.29215000080469E+03 0.31707750090795E+03 0.35823206822799E+03 0.29215000027545E+03
 0.29215000080149E+03 0.31872981269267E+03 0.35831702131869E+03 0.29215000034292E+03 0.29215000080469E+03
 0.31707750090795E+03 0.35823206822799E+03 0.29215000027545E+03 0.29215000080149E+03 0.40909054971931E+03
 0.30363535389315E+03 0.16217003910326E+04 0.15851189408064E+04 0.88990065304015E+03 0.14219493510326E+04
 0.52759919472721E+03 0.98992523684281E+03 0.14852994431665E+04 0.96971691125522E+03 0.22774221217772E+04
 0.93778543915557E+03 0.14849073846356E+04 0.92356877033532E+03 0.22778286888863E+04 0.98992523684277E+03
 0.14852994431665E+04 0.96971691125517E+03 0.22774221217772E+04 0.93778543915559E+03 0.14849073846356E+04
 0.92356877033536E+03 0.22778286888863E+04 0.19260771191182E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.70212302431656E+05 0.50050000000000E+08 0.52986437851746E+03 0.12357080342643E+01
 0.12357080342643E+01 0.17508210101481E+01 0.46409849335692E+00 0.29623803041149E+03 0.30504831815661E+03
 0.30232242397758E+03 0.30226694767545E+03 0.23000000000000E+00 0.00000000000000E+00 0.20551985079894E+00
 0.00000000000000E+00 -.12226779126880E+00 0.98585179926785E-03 0.49140012823265E+00 0.80000000000000E+04
 0.30000000000000E+04 0.16280012031687E+02 0.61050045118825E+01 0.30363686980363E+03 0.40910437046894E+03
 0.29316008578325E+03 0.29508070295595E+03 0.29215000000465E+03 0.29215000009451E+03 0.29314745224282E+03
 0.29507734156987E+03 0.29215000000458E+03 0.29215000009454E+03 0.29316008578325E+03 0.29508070295595E+03
 0.29215000000465E+03 0.29215000009451E+03 0.29314745224282E+03 0.29507734156987E+03 0.29215000000458E+03
 0.29215000009454E+03 0.29468770932386E+03 0.29215000083403E+03 -.23439025690410E+02 -.49520840243477E+02
 0.44342166391386E+02 0.11316137456746E+03 0.68597497344119E+02 0.45886333882220E+02 0.36048421791388E+02
 0.53365511360559E+02 0.86754303886741E+02 0.45193744707094E+02 0.35792456973366E+02 0.52712638771889E+02
 0.86518775394720E+02 0.45886333882221E+02 0.36048421791389E+02 0.53365511360561E+02 0.86754303886743E+02
 0.45193744707096E+02 0.35792456973366E+02 0.52712638771893E+02 0.86518775394721E+02 0.38157123222138E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31887077912898E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.33403336519786E-02 0.22330357189549E-01 0.00000000000000E+00 0.33403336519786E-02 0.22330357189549E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29604398865602E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.40449041330006E-04 0.29604398865602E-01 0.40449041330006E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.93793909355724E-03 0.72216655373702E-01 0.39430591918199E-01
 0.00000000000000E+00 0.72216655373702E-01 0.40368531011756E-01 0.41954924667846E+00 0.94549243698226E-01
 0.32500000298023E+00 0.42250006586313E+00
    496.26779401
 0.61148081556726E+00 0.31235524458209E+03 0.46360723464195E+03 0.43277800456262E+03 0.42196008811145E+03
 0.22999999623404E+00 0.00000000000000E+00 0.86184546232883E-01 0.00000000000000E+00 -.10268760294472E+02
 0.93521846530455E-03 0.19106163122771E+01 0.80000000000000E+04 0.30000000000000E+04 0.41871305863947E+01
 0.15701739698980E+01 0.32222648833849E+03 0.29215000535336E+03 0.31931512472037E+03 0.35945554776957E+03
 0.29215000048485E+03 0.29215000114062E+03 0.31764158965841E+03 0.35937384784071E+03 0.29215000039055E+03
 0.29215000113610E+03 0.31931512472038E+03 0.35945554776957E+03 0.29215000048485E+03 0.29215000114062E+03
 0.31764158965840E+03 0.35937384784071E+03 0.29215000039055E+03 0.29215000113610E+03 0.41037270525950E+03
 0.30441996522538E+03 0.16316719862044E+04 0.15948034139230E+04 0.88680124310348E+03 0.14109486126357E+04
 0.51971336331671E+03 0.99654695017048E+03 0.14891817443604E+04 0.97633708832082E+03 0.22760155206009E+04
 0.94400286442877E+03 0.14888949929661E+04 0.92985374355572E+03 0.22764951377511E+04 0.99654695017046E+03
 0.14891817443604E+04 0.97633708832079E+03 0.22760155206010E+04 0.94400286442879E+03 0.14888949929660E+04
 0.92985374355576E+03 0.22764951377510E+04 0.19258618652813E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.71019032855769E+05 0.50050000000000E+08 0.53049230101415E+03 0.12350271823651E+01
 0.12350271823651E+01 0.18054047171120E+01 0.46329114674987E+00 0.29664514353723E+03 0.30519068665162E+03
 0.30255130367224E+03 0.30249921738928E+03 0.23000000000000E+00 0.00000000000000E+00 0.20438364787516E+00
 0.00000000000000E+00 -.12972633050916E+00 0.98449874155410E-03 0.51421249727646E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15557770459435E+02 0.58341639222881E+01 0.30442161087509E+03 0.41038517076480E+03
 0.29322532452584E+03 0.29513463801441E+03 0.29215000000674E+03 0.29215000013274E+03 0.29321191226096E+03
 0.29513098596576E+03 0.29215000000664E+03 0.29215000013277E+03 0.29322532452584E+03 0.29513463801441E+03
 0.29215000000674E+03 0.29215000013274E+03 0.29321191226096E+03 0.29513098596576E+03 0.29215000000664E+03
 0.29215000013277E+03 0.29473622159516E+03 0.29215000112842E+03 -.26628386045918E+02 -.54467020124217E+02
 0.46166917119962E+02 0.11581834617157E+03 0.69420594466013E+02 0.48121792974173E+02 0.37582846462836E+02
 0.56688216203821E+02 0.88872016792542E+02 0.47388143782115E+02 0.37315407658194E+02 0.55998056255512E+02
 0.88626839801679E+02 0.48121792974174E+02 0.37582846462836E+02 0.56688216203823E+02 0.88872016792543E+02
 0.47388143782118E+02 0.37315407658194E+02 0.55998056255516E+02 0.88626839801679E+02 0.38357915540750E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31919305850650E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.31929740464172E-02 0.22686987261865E-01 0.00000000000000E+00 0.31929740464172E-02 0.22686987261865E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.29970002310250E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.29077956272724E-04 0.29970002310250E-01 0.29077956272724E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.85079287668172E-03 0.72978120114818E-01 0.39754213122928E-01
 0.00000000000000E+00 0.72978120114818E-01 0.40605005999610E-01 0.41914557337493E+00 0.94145570394700E-01
 0.32500000298023E+00 0.42250006586313E+00
    500.00000000
 0.61151746836503E+00 0.31251205966670E+03 0.46377871103327E+03 0.43294464446936E+03 0.42212924259768E+03
 0.22999999617093E+00 0.00000000000000E+00 0.85089195438400E-01 0.00000000000000E+00 -.10279760513547E+02
 0.93474908155946E-03 0.19243502055379E+01 0.80000000000000E+04 0.30000000000000E+04 0.41572474578575E+01
 0.15589677966966E+01 0.32240458445714E+03 0.29215000579911E+03 0.31947660167868E+03 0.35976324220608E+03
 0.29215000053002E+03 0.29215000124767E+03 0.31779711006893E+03 0.35968241911943E+03 0.29215000042724E+03
 0.29215000124273E+03 0.31947660167868E+03 0.35976324220608E+03 0.29215000053002E+03 0.29215000124767E+03
 0.31779711006893E+03 0.35968241911944E+03 0.29215000042724E+03 0.29215000124273E+03 0.41071037503630E+03
 0.30463271840370E+03 0.16343759865410E+04 0.15974147350107E+04 0.88605721774610E+03 0.14081907999834E+04
 0.51770329614856E+03 0.99834634904517E+03 0.14902147487378E+04 0.97812458122581E+03 0.22756320175945E+04
 0.94569008790151E+03 0.14899541195288E+04 0.93154937106870E+03 0.22761290860087E+04 0.99834634904516E+03
 0.14902147487379E+04 0.97812458122578E+03 0.22756320175945E+04 0.94569008790153E+03 0.14899541195287E+04
 0.93154937106873E+03 0.22761290860086E+04 0.19258513011903E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.71245762917730E+05 0.50050000000000E+08 0.53066317945749E+03 0.12348356589983E+01
 0.12348356589983E+01 0.18203335410577E+01 0.46310040869291E+00 0.29675943329775E+03 0.30523260774853E+03
 0.30261665404780E+03 0.30256547342654E+03 0.23000000000000E+00 0.00000000000000E+00 0.20406599465183E+00
 0.00000000000000E+00 -.13178047515857E+00 0.98411956260488E-03 0.52058000848065E+00 0.80000000000000E+04
 0.30000000000000E+04 0.15367474489365E+02 0.57628029335120E+01 0.30463440594079E+03 0.41072246773636E+03
 0.29324471448585E+03 0.29515047458091E+03 0.29215000000742E+03 0.29215000014485E+03 0.29323107426226E+03
 0.29514674091395E+03 0.29215000000731E+03 0.29215000014489E+03 0.29324471448585E+03 0.29515047458091E+03
 0.29215000000742E+03 0.29215000014485E+03 0.29323107426226E+03 0.29514674091395E+03 0.29215000000731E+03
 0.29215000014489E+03 0.29475040482759E+03 0.29215000122001E+03 -.27481185965452E+02 -.55781709071453E+02
 0.46666345488364E+02 0.11656646111553E+03 0.69666783899728E+02 0.48739557983346E+02 0.38005340388421E+02
 0.57613460360100E+02 0.89469606186345E+02 0.47994979634169E+02 0.37734949679897E+02 0.56913467082287E+02
 0.89221993361494E+02 0.48739557983347E+02 0.38005340388421E+02 0.57613460360102E+02 0.89469606186346E+02
 0.47994979634171E+02 0.37734949679897E+02 0.56913467082291E+02 0.89221993361494E+02 0.38432230171386E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31929226706186E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.31577813388357E-02 0.22774192157810E-01 0.00000000000000E+00 0.31577813388357E-02 0.22774192157810E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30099190615487E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.25714812901722E-04 0.30099190615487E-01 0.25714812901722E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.82416718227287E-03 0.73211615370421E-01 0.39829902868502E-01
 0.00000000000000E+00 0.73211615370421E-01 0.40654070050775E-01 0.41905020434645E+00 0.94050201366222E-01
 0.32500000298023E+00 0.42250006586313E+00
