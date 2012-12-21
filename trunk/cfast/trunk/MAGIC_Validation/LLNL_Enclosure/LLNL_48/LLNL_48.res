#FORMAT_CAS                                                                                         
413                                                                                                 
#TITRE                                                                                              
""                                                                                                  
#LOCAL LOC_1                                                                                        
LLNL-48 0 MONOZONE(1=OUI,0=NON)                                                                     
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
Plenum-LLNL48 0 MONOZONE(1=OUI,0=NON)                                                               
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
#CONDINIT 500.000000 10.000000 21.000000 0.230000 0.001000 101325.000000                            
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
#ROOM#LOC_1 #LLNL-48           #HEIGHT#m#Layer interface height
#ROOM#LOC_1 #LLNL-48           #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_1 #LLNL-48           #TEMPERATURE#K#Upper layer temperature
#ROOM#LOC_1 #LLNL-48           #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_1 #LLNL-48           #TEMPERATURE#K#Enthalpy average temperature
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-48           #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_1 #LLNL-48           #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-48           #CONCENTRATION#g/g#Upper layer oxygen concentration
#ROOM#LOC_1 #LLNL-48           #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_1 #LLNL-48           #PRESSURE#Pa#Pressure at the room floor
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-48           #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_1 #LLNL-48           #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_1 #LLNL-48           #VISIBILITY#m#Lower layer light-emitting sign
#ROOM#LOC_1 #LLNL-48           #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_1 #LLNL-48           #VISIBILITY#m#Upper layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_1 #LLNL-48           #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_1 #LLNL-48           #HEAT_POWER#kW#Absorbed Heat Flux on walls
#ROOM#LOC_1 #LLNL-48           #HEAT_POWER#W#Total sprinkling power
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
#ROOM#LOC_2 #Plenum-LLNL48     #HEIGHT#m#Layer interface height
#ROOM#LOC_2 #Plenum-LLNL48     #TEMPERATURE#K#Lower layer temperature
#ROOM#LOC_2 #Plenum-LLNL48     #TEMPERATURE#K#Upper layer temperature
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL48     #TEMPERATURE#K#Geometrical average temperature
#ROOM#LOC_2 #Plenum-LLNL48     #TEMPERATURE#K#Enthalpy average temperature
#ROOM#LOC_2 #Plenum-LLNL48     #CONCENTRATION#g/g#Lower layer oxygen concentration
#ROOM#LOC_2 #Plenum-LLNL48     #CONCENTRATION#g/g#Lower layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL48     #CONCENTRATION#g/g#Upper layer oxygen concentration
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL48     #CONCENTRATION#g/g#Upper layer unburnt combustible concentration
#ROOM#LOC_2 #Plenum-LLNL48     #PRESSURE#Pa#Pressure at the room floor
#ROOM#LOC_2 #Plenum-LLNL48     #GAS_EXTINCTION#m-1#Lower layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL48     #GAS_EXTINCTION#m-1#Upper layer extinction coefficient
#ROOM#LOC_2 #Plenum-LLNL48     #VISIBILITY#m#Lower layer light-emitting sign
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL48     #VISIBILITY#m#Lower layer light-reflecting sign
#ROOM#LOC_2 #Plenum-LLNL48     #VISIBILITY#m#Upper layer light-emitting sign
#ROOM#LOC_2 #Plenum-LLNL48     #VISIBILITY#m#Upper layer light-reflecting sign
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
#ROOM#LOC_2 #Plenum-LLNL48     #HEAT_POWER#kW#Absorbed Heat Flux on walls
#NOUVELLE LIGNE
#ROOM#LOC_2 #Plenum-LLNL48     #HEAT_POWER#W#Total sprinkling power
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
 0.30000000000000E+01 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 0.75369014233134E-06
 0.10000000000074E-02 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.46389238024936E-03 0.46389238024936E-03 0.65705721967824E-03 0.66034250577664E-03
 0.00000000000000E+00 0.43959399083025E-03 0.51732930530814E-03 0.43959399083025E-03 0.51732930530814E-03
 0.43736874021818E-03 0.51732311538818E-03 0.43736874021818E-03 0.51732311538818E-03 0.43959399077256E-03
 0.51732930536583E-03 0.43959399077256E-03 0.51732930536583E-03 0.43736874021818E-03 0.51732311538818E-03
 0.43736874021818E-03 0.51732311538818E-03 0.53303870698711E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 0.37030052155921E-06 0.99965006212315E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.54002072153512E-03 0.54002072153512E-03
 0.66308963599115E-03 0.66640508417111E-03 0.00000000000000E+00 0.47191556375433E-03 0.52066488311202E-03
 0.47191556375433E-03 0.52066488311202E-03 0.46780138838034E-03 0.52065773475023E-03 0.46780138838034E-03
 0.52065773475023E-03 0.47191556381203E-03 0.52066488311202E-03 0.47191556381203E-03 0.52066488311202E-03
 0.46780138838034E-03 0.52065773475023E-03 0.46780138838034E-03 0.52065773475023E-03 0.43062316145611E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47494157812538E-04 0.47494157812538E-04 0.00000000000000E+00
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29641791906072E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.29641791906072E-04 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.21787834550974E-01 0.21787834550974E-01 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     20.00000000
 0.30000000000000E+01 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20469566659607E-02
 0.99999997979811E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29415000204258E+03 0.29415000000000E+03 0.29415000280779E+03 0.29415000280779E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000279327E+03 0.29415000279327E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000280779E+03 0.29415000280779E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000279327E+03 0.29415000279327E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000972492E+03
 0.29415000803231E+03 0.47962754189653E-03 0.47962754189653E-03 0.62133842075797E-03 0.62444511286176E-03
 .00000000000000E+00 0.44814126172560E-03 0.53351970755337E-03 0.44814126172560E-03 0.53351970755337E-03
 0.44581668929807E-03 0.53358856749952E-03 0.44581668929807E-03 0.53358856749952E-03 0.44814126086021E-03
 0.53351970686106E-03 0.44814126086021E-03 0.53351970686106E-03 0.44581668900961E-03 0.53358856715337E-03
 0.44581668900961E-03 0.53358856715337E-03 0.53330429660436E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22623581275695E-02 0.99965044095206E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415000801117E+03 0.29415000975059E+03
 0.29415000301237E+03 0.29415000301237E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000298589E+03
 0.29415000298589E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000301237E+03 0.29415000301237E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000298589E+03 0.29415000298589E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000290333E+03 0.29415000000000E+03 0.51499011890720E-03 0.51499011890720E-03
 0.67732632452856E-03 0.68071295615120E-03 .00000000000000E+00 0.48075734871316E-03 0.52796757936382E-03
 0.48075734871316E-03 0.52796757936382E-03 0.47652660089777E-03 0.52809781258681E-03 0.47652660089777E-03
 0.52809781258681E-03 0.48075734755931E-03 0.52796757832536E-03 0.48075734755931E-03 0.52796757832536E-03
 0.47652660089777E-03 0.52809781258681E-03 0.47652660089777E-03 0.52809781258681E-03 0.43068825278627E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24751350685985E-02 0.00000000000000E+00 0.00000000000000E+00 0.24751350685985E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42254273077984E-02
 0.67003484757506E-02 0.67003484757506E-02 0.42254273077984E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     30.00000000
 0.30000000000000E+01 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20469566665898E-02
 0.99999997979811E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29415000255627E+03 0.29415000000000E+03 0.29415000350529E+03 0.29415000350529E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000348710E+03 0.29415000348710E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000350529E+03 0.29415000350529E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000348710E+03 0.29415000348710E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415001198510E+03
 0.29415000990672E+03 0.48318121693692E-03 0.48318121693692E-03 0.61317056843480E-03 0.61623642127697E-03
 .00000000000000E+00 0.44999587596631E-03 0.53712293633808E-03 0.44999587596631E-03 0.53712293633808E-03
 0.44765106283742E-03 0.53721078356315E-03 0.44765106283742E-03 0.53721078356315E-03 0.44999587446631E-03
 0.53712293495346E-03 0.44999587446631E-03 0.53712293495346E-03 0.44765106243357E-03 0.53721078327468E-03
 0.44765106243357E-03 0.53721078327468E-03 0.53329500328172E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22623581338429E-02 0.99965064105708E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415000988844E+03 0.29415001200722E+03
 0.29415000376037E+03 0.29415000376037E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000372728E+03
 0.29415000372728E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000376037E+03 0.29415000376037E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000372728E+03 0.29415000372728E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000362595E+03 0.29415000000000E+03 0.50929232706010E-03 0.50929232706010E-03
 0.68046469505184E-03 0.68386701852710E-03 .00000000000000E+00 0.48269695896883E-03 0.52957038059651E-03
 0.48269695896883E-03 0.52957038059651E-03 0.47844268816527E-03 0.52973494430224E-03 0.47844268816527E-03
 0.52973494430224E-03 0.48269695752652E-03 0.52957037926958E-03 0.48269695752652E-03 0.52957037926958E-03
 0.47844268828065E-03 0.52973494435993E-03 0.47844268828065E-03 0.52973494435993E-03 0.43065681801227E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24751350685985E-02 0.00000000000000E+00 0.00000000000000E+00 0.24751350685985E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42254273077984E-02
 0.67003484757506E-02 0.67003484757506E-02 0.42254273077984E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00000000
 0.30000000000000E+01 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20469566627660E-02
 0.99999997979811E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29415000299586E+03 0.29415000000000E+03 0.29415000409937E+03 0.29415000409937E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000407803E+03 0.29415000407803E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000409937E+03 0.29415000409937E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000407803E+03 0.29415000407803E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415001385824E+03
 0.29415001146339E+03 0.48610692485069E-03 0.48610692485069E-03 0.60647366250187E-03 0.60950603081438E-03
 .00000000000000E+00 0.45150473253468E-03 0.54006587203166E-03 0.45150473253468E-03 0.54006587203166E-03
 0.44914426541367E-03 0.54016996649856E-03 0.44914426541367E-03 0.54016996649856E-03 0.45150473132315E-03
 0.54006587110858E-03 0.45150473132315E-03 0.54006587110858E-03 0.44914426535597E-03 0.54016996661394E-03
 0.44914426535597E-03 0.54016996661394E-03 0.53328343654638E-04 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 .00000000000000E+00 0.15000000000000E+01 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22623581301745E-02 0.99965084085135E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415001144907E+03 0.29415001387552E+03
 0.29415000439740E+03 0.29415000439740E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000435866E+03
 0.29415000435866E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000439740E+03 0.29415000439740E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000435866E+03 0.29415000435866E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000424215E+03 0.29415000000000E+03 0.50462712084775E-03 0.50462712084775E-03
 0.68302233590152E-03 0.68643744758103E-03 .00000000000000E+00 0.48427656311702E-03 0.53087590234403E-03
 0.48427656311702E-03 0.53087590234403E-03 0.48000428707251E-03 0.53106973472735E-03 0.48000428707251E-03
 0.53106973472735E-03 0.48427656167472E-03 0.53087590101711E-03 0.48427656167472E-03 0.53087590101711E-03
 0.48000428718790E-03 0.53106973472735E-03 0.48000428718790E-03 0.53106973472735E-03 0.43062579210979E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24751350685985E-02 0.00000000000000E+00 0.00000000000000E+00 0.24751350685985E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750105741350E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42254273077984E-02
 0.67003484757506E-02 0.67003484757506E-02 0.42254273077984E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     40.00025000
 0.30000000000000E+01 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000000000E+03
 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00 0.00000000000000E+00 -.20469566587626E-02
 0.99999997979811E-03 0.10000000000000E-02 0.80000000000000E+04 0.30000000000000E+04 0.80000000000000E+04
 0.30000000000000E+04 0.29415000299587E+03 0.29415000000000E+03 0.29415000409938E+03 0.29415000409938E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000407805E+03 0.29415000407805E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000409938E+03 0.29415000409938E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29415000407805E+03 0.29415000407805E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415001385828E+03
 0.29415001146343E+03 0.48611256830940E-03 0.48611256830940E-03 0.60648148224535E-03 0.60951388965658E-03
 .00000000000000E+00 0.45151087433929E-03 0.54007036020248E-03 0.45151087433929E-03 0.54007036020248E-03
 0.44915045562209E-03 0.54017445507322E-03 0.44915045562209E-03 0.54017445507322E-03 0.45151087278160E-03
 0.54007035893325E-03 0.45151087278160E-03 0.54007035893325E-03 0.44915045573748E-03 0.54017445501553E-03
 0.44915045573748E-03 0.54017445501553E-03 0.53329037375674E-04 0.00000000000000E+00 0.10000000000048E-06
 0.00000000000000E+00 0.00000000000000E+00 0.50050000000000E+08 0.29415000000000E+03 0.00000000000000E+00
 0.00000000000000E+00 0.25000000000239E-10 0.15000000000000E+01 0.29415000000000E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000000000E+03 0.23000000000000E+00 0.00000000000000E+00 0.23000000000000E+00
 0.00000000000000E+00 -.22623581317535E-02 0.99965084085634E-03 0.10000000000000E-02 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415001144911E+03 0.29415001387557E+03
 0.29415000439741E+03 0.29415000439741E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000435868E+03
 0.29415000435868E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415000439741E+03 0.29415000439741E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415000435868E+03 0.29415000435868E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29415000424217E+03 0.29415000000000E+03 0.50463443565176E-03 0.50463443565176E-03
 0.68303734972865E-03 0.68645253647729E-03 .00000000000000E+00 0.48428687711184E-03 0.53088419832063E-03
 0.48428687711184E-03 0.53088419832063E-03 0.48001477275955E-03 0.53107803151164E-03 0.48001477275955E-03
 0.53107803151164E-03 0.48428687566953E-03 0.53088419699370E-03 0.48428687566953E-03 0.53088419699370E-03
 0.48001477287493E-03 0.53107803151164E-03 0.48001477287493E-03 0.53107803151164E-03 0.43063428379936E-04
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.29415000000000E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24751350598032E-02 0.00000000000000E+00 0.00000000000000E+00 0.24751350598032E-02
 0.20000004768372E+00 0.10000002384186E+00 0.10000002384186E+00 0.52000010490417E-01 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750106574196E-02 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.24750106574196E-02 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.42254273077984E-02
 0.67003484757506E-02 0.67003484757506E-02 0.42254273077984E-02 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     50.17775528
 0.23487077326817E+01 0.29416771958042E+03 0.33685750366785E+03 0.30343556167029E+03 0.30249002120581E+03
 0.22999999969610E+00 0.00000000000000E+00 0.22510068430715E+00 0.00000000000000E+00 0.52098734100045E+01
 0.99999117803726E-03 0.86671675819847E-01 0.80000000000000E+04 0.30000000000000E+04 0.92302357423301E+02
 0.34613384033738E+02 0.29534663700317E+03 0.29415000000000E+03 0.29535824685058E+03 0.29607310839708E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29506044378741E+03 0.29606066343189E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29535824685058E+03 0.29607310839708E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29506044378741E+03 0.29606066343189E+03 0.29415000000000E+03 0.29415000000000E+03 0.29926346368885E+03
 0.29415510417081E+03 0.54255517353470E+03 0.54028241261130E+03 0.31911894046565E+03 0.69402113994022E+03
 0.37330660477224E+03 0.39839437417455E+03 0.21249494669647E+03 0.39652678431054E+03 0.60634006182394E+03
 0.29804206192566E+03 0.20771130679870E+03 0.29676095709088E+03 0.60169644853097E+03 0.39839437417455E+03
 0.21249494669647E+03 0.39652678431053E+03 0.60634006182394E+03 0.29804206192566E+03 0.20771130679870E+03
 0.29676095709088E+03 0.60169644853097E+03 0.54249126670146E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.36713068993481E+03 0.12946661328498E+01
 0.12946661328498E+01 0.21011085933825E-01 0.13270374070286E+01 0.29415054626554E+03 0.30288494478757E+03
 0.29515769574315E+03 0.29513191546066E+03 0.22999999997289E+00 0.00000000000000E+00 0.22886820574338E+00
 0.00000000000000E+00 0.62115414407791E+00 0.99965513740668E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29415509296439E+03 0.29927645657226E+03
 0.29415247391000E+03 0.29440739077292E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415249319619E+03
 0.29440740322607E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415247391000E+03 0.29440739077292E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415249319619E+03 0.29440740322607E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29424993381014E+03 0.29415000000000E+03 0.76334891022065E+00 0.76175966681343E+00
 0.54056070153898E+00 0.56877933603347E+02 0.56334670098300E+02 0.87866337467622E+00 -.50269634260437E+00
 0.87815308408236E+00 0.82702390987919E+02 0.88387529363856E+00 -.49688307872900E+00 0.88335856670676E+00
 0.82708068997665E+02 0.87866337467616E+00 -.50269634260541E+00 0.87815308408230E+00 0.82702390987916E+02
 0.88387529363896E+00 -.49688307872894E+00 0.88335856670719E+00 0.82708068997665E+02 0.16928109977491E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31191523113459E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.12400336807968E+00 0.00000000000000E+00 0.00000000000000E+00 0.12400336807968E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.31130465872962E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.31130465872962E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.35100461761664E+00 0.35100461761664E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     60.18555475
 0.18710846255857E+01 0.29421606448369E+03 0.36417657500558E+03 0.32054256312703E+03 0.31714244326649E+03
 0.22999999975115E+00 0.00000000000000E+00 0.22149842605182E+00 0.00000000000000E+00 0.21893976782851E+01
 0.99979705871545E-03 0.14553005194886E+00 0.80000000000000E+04 0.30000000000000E+04 0.54971463920119E+02
 0.20614298970045E+02 0.29636110242157E+03 0.29415000000000E+03 0.29653831147710E+03 0.29848306317851E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29594869249778E+03 0.29845487235321E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29653831147710E+03 0.29848306317851E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29594869249778E+03 0.29845487235321E+03 0.29415000000000E+03 0.29415000000000E+03 0.30588943197396E+03
 0.29416703894790E+03 0.58972049476489E+03 0.58467567960748E+03 0.34010453529543E+03 0.95177407592318E+03
 0.60996901795127E+03 0.44723694253799E+03 0.28588010551308E+03 0.44268672967393E+03 0.86597538725399E+03
 0.33753054209036E+03 0.28017139874788E+03 0.33442920098140E+03 0.86056679135631E+03 0.44723694253798E+03
 0.28588010551308E+03 0.44268672967393E+03 0.86597538725400E+03 0.33753054209036E+03 0.28017139874789E+03
 0.33442920098140E+03 0.86056679135632E+03 0.71323820623491E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.39215090947789E+03 0.12946883805923E+01
 0.12946883805923E+01 0.61042283815607E-01 0.10931748931068E+01 0.29415062144746E+03 0.31002298137726E+03
 0.29845547113085E+03 0.29829258778289E+03 0.22999999991923E+00 0.00000000000000E+00 0.22778918710715E+00
 0.00000000000000E+00 0.45808138064181E+00 0.99965327250401E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29416698065762E+03 0.30593293188100E+03
 0.29415704208636E+03 0.29474977406473E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415703465347E+03
 0.29474986620988E+03 0.29415000000000E+03 0.29415000000000E+03 0.29415704208636E+03 0.29474977406473E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29415703465347E+03 0.29474986620988E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29448939013144E+03 0.29415000000000E+03 0.16736084329497E+01 0.16657330368685E+01
 -.78244204449382E-01 0.11487133213024E+03 0.11494996755571E+03 0.16044820667748E+01 -.13655016602966E+01
 0.16020539168586E+01 0.11927340792753E+03 0.15970920887526E+01 -.13388113776530E+01 0.15946674935985E+01
 0.11929926096731E+03 0.16044820667748E+01 -.13655016602969E+01 0.16020539168586E+01 0.11927340792752E+03
 0.15970920887529E+01 -.13388113776531E+01 0.15946674935988E+01 0.11929926096731E+03 0.38024039490953E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32244492080691E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10185978505471E+00 0.00000000000000E+00 0.00000000000000E+00 0.10185978505471E+00 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25384778280038E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.25384778280038E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.30164207866243E+00 0.30164207866243E+00 0.00000000000000E+00 0.65000000596046E+00 0.32500000298023E+00
 0.32500000298023E+00 0.42250006586313E+00
     70.29975400
 0.15127981774397E+01 0.29429944174027E+03 0.37846027230512E+03 0.33602082194168E+03 0.33076264569393E+03
 0.22999999434614E+00 0.00000000000000E+00 0.21911499911231E+00 0.00000000000000E+00 -.69249859813713E-01
 0.99949152885193E-03 0.18072954730918E+00 0.80000000000000E+04 0.30000000000000E+04 0.44265036454244E+02
 0.16599388670341E+02 0.29718747663310E+03 0.29415000000000E+03 0.29746486356014E+03 0.30067249959282E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29666986904204E+03 0.30063438496524E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29746486356014E+03 0.30067249959282E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29666986904204E+03 0.30063438496524E+03 0.29415000000000E+03 0.29415000000000E+03 0.31148866771127E+03
 0.29418387585146E+03 0.65704467170423E+03 0.64954948587480E+03 0.36964645125941E+03 0.10828346738967E+04
 0.71133999038095E+03 0.49097516612768E+03 0.35804343919220E+03 0.48404862691398E+03 0.10219958222676E+04
 0.37660675829143E+03 0.35233682153947E+03 0.37186080786173E+03 0.10166866871474E+04 0.49097516612768E+03
 0.35804343919220E+03 0.48404862691398E+03 0.10219958222676E+04 0.37660675829143E+03 0.35233682153947E+03
 0.37186080786173E+03 0.10166866871474E+04 0.85199952167814E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.40516090253754E+03 0.19562957534474E+01
 0.19562957534474E+01 0.10149908083571E+00 0.90641309540752E+00 0.29415093555343E+03 0.31386796023283E+03
 0.30195344731825E+03 0.30164971121326E+03 0.23000000000000E+00 0.00000000000000E+00 0.22702098191531E+00
 0.00000000000000E+00 0.34705093930220E+00 0.99965110925214E-03 0.00000000000000E+00 0.80000000000000E+04
 0.30000000000000E+04 0.80000000000000E+04 0.30000000000000E+04 0.29418377686831E+03 0.31153261359690E+03
 0.29416301713241E+03 0.29503250446699E+03 0.29415000000000E+03 0.29415000000000E+03 0.29416295013406E+03
 0.29503270974737E+03 0.29415000000000E+03 0.29415000000000E+03 0.29416301713241E+03 0.29503250446699E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29416295013405E+03 0.29503270974737E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29472916981778E+03 0.29415000000000E+03 0.26840371833780E+01 0.26652182892852E+01
 -.51783658564898E+00 0.14777707877773E+03 0.14829750454630E+03 0.24257934115957E+01 -.18402408902768E+01
 0.24201795966650E+01 0.13510984894922E+03 0.24071326101203E+01 -.17968466567478E+01 0.24015581602451E+01
 0.13515149483238E+03 0.24257934115957E+01 -.18402408902772E+01 0.24201795966650E+01 0.13510984894922E+03
 0.24071326101214E+01 -.17968466567485E+01 0.24015581602463E+01 0.13515149483238E+03 0.52585402534523E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32857425322947E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.87707023775383E-01 0.00000000000000E+00 0.00000000000000E+00 0.87707023775383E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.21811461369392E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.21811461369392E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.73586857652742E-02 0.00000000000000E+00
 0.25525352029867E+00 0.26261220606395E+00 0.00000000000000E+00 0.64070654770376E+00 0.31570654472353E+00
 0.32500000298023E+00 0.42250006586313E+00
     80.49214468
 0.12285440287500E+01 0.29442482580091E+03 0.38857945831072E+03 0.35002175446103E+03 0.34358390947115E+03
 0.22999999434606E+00 0.00000000000000E+00 0.21703766508528E+00 0.00000000000000E+00 -.19661267027519E+01
 0.99904718111197E-03 0.21024881481091E+00 0.80000000000000E+04 0.30000000000000E+04 0.38050155037473E+02
 0.14268808139052E+02 0.29808008921995E+03 0.29415000000001E+03 0.29834728706169E+03 0.30273688392589E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29737552519956E+03 0.30269297084862E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29834728706169E+03 0.30273688392589E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29737552519956E+03 0.30269297084862E+03 0.29415000000000E+03 0.29415000000000E+03 0.31646037370780E+03
 0.29421618546541E+03 0.75636111893474E+03 0.74610770311283E+03 0.39418711921257E+03 0.11703127107196E+04
 0.77415465591101E+03 0.53705472509752E+03 0.42145116750809E+03 0.52774608755952E+03 0.11414119441913E+04
 0.41851775177029E+03 0.41624458757523E+03 0.41209818188394E+03 0.11366587162798E+04 0.53705472509752E+03
 0.42145116750809E+03 0.52774608755952E+03 0.11414119441913E+04 0.41851775177029E+03 0.41624458757523E+03
 0.41209818188394E+03 0.11366587162798E+04 0.98196506683828E+02 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.41630469487942E+03 0.17288816866716E+01
 0.17288816866716E+01 0.14226864354552E+00 0.76950893411772E+00 0.29415155987298E+03 0.31640217539673E+03
 0.30498747710663E+03 0.30458269181829E+03 0.23000000000000E+00 0.00000000000000E+00 0.22635689928271E+00
 0.00000000000000E+00 0.24105316645028E+00 0.99964794143894E-03 0.18527735731140E-01 0.80000000000000E+04
 0.30000000000000E+04 0.43178508783208E+03 0.16191940793703E+03 0.29421603598839E+03 0.31649603948623E+03
 0.29417315810605E+03 0.29528867230262E+03 0.29415000000000E+03 0.29415000000000E+03 0.29417293652877E+03
 0.29528899755290E+03 0.29415000000000E+03 0.29415000000000E+03 0.29417315810605E+03 0.29528867230262E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29417293652877E+03 0.29528899755290E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29496114003611E+03 0.29415000000001E+03 0.55499678788727E+01 0.55027515302019E+01
 0.16463255023676E+01 0.17174655524298E+03 0.17009199811310E+03 0.43982513351575E+01 0.35984750557715E+00
 0.43861447871490E+01 0.14746375791505E+03 0.43390413307519E+01 0.41538541506235E+00 0.43270918309111E+01
 0.14751661311900E+03 0.43982513351571E+01 0.35984750557640E+00 0.43861447871486E+01 0.14746375791505E+03
 0.43390413307524E+01 0.41538541506235E+00 0.43270918309116E+01 0.14751661311900E+03 0.64758514163395E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33255672126989E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.75725806302980E-01 0.00000000000000E+00 0.00000000000000E+00 0.75725806302980E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.18886662801225E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.18886662801225E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.56862925445952E-01 0.00000000000000E+00
 0.16677066739830E+00 0.22363359284425E+00 0.00000000000000E+00 0.57225446705886E+00 0.24725446407863E+00
 0.32500000298023E+00 0.42250006586313E+00
     90.43143889
 0.10111573418764E+01 0.29459703919413E+03 0.39665711236610E+03 0.36225751493268E+03 0.35518299541352E+03
 0.22999999441187E+00 0.00000000000000E+00 0.21505072052085E+00 0.00000000000000E+00 -.35284014328621E+01
 0.99844777032556E-03 0.23787903183649E+00 0.80000000000000E+04 0.30000000000000E+04 0.33630538758452E+02
 0.12611452034419E+02 0.29899639839310E+03 0.29415000000001E+03 0.29919448508873E+03 0.30469593540291E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29807006574624E+03 0.30464867021250E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29919448508873E+03 0.30469593540291E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29807006574624E+03 0.30464867021250E+03 0.29415000000000E+03 0.29415000000000E+03 0.32098753833313E+03
 0.29427106951997E+03 0.85339829467759E+03 0.84028276115401E+03 0.42540955791714E+03 0.12465374070673E+04
 0.81900080136060E+03 0.58239675839778E+03 0.48496774347373E+03 0.57076909423813E+03 0.12459402493555E+04
 0.46097790312255E+03 0.48025717190962E+03 0.45291035631208E+03 0.12417137187329E+04 0.58239675839778E+03
 0.48496774347373E+03 0.57076909423812E+03 0.12459402493555E+04 0.46097790312255E+03 0.48025717190962E+03
 0.45291035631208E+03 0.12417137187329E+04 0.11016537652779E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.42752806205050E+03 0.15687713240746E+01
 0.15687713240746E+01 0.18202582038218E+00 0.67678097677861E+00 0.29415288975834E+03 0.31803015230909E+03
 0.30725703426116E+03 0.30679406130368E+03 0.23000000000000E+00 0.00000000000000E+00 0.22576388574041E+00
 0.00000000000000E+00 0.14910923412879E+00 0.99964251455428E-03 0.37667292557746E-01 0.80000000000000E+04
 0.30000000000000E+04 0.21238585140505E+03 0.79644694276895E+02 0.29427077481675E+03 0.32101728967501E+03
 0.29418928581600E+03 0.29552223470462E+03 0.29415000000000E+03 0.29415000000000E+03 0.29418877926348E+03
 0.29552267120636E+03 0.29415000000000E+03 0.29415000000000E+03 0.29418928581600E+03 0.29552223470462E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29418877926348E+03 0.29552267120636E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29517594300316E+03 0.29415000000001E+03 0.91094876437416E+01 0.90039299794687E+01
 0.48777842717557E+01 0.18827552468963E+03 0.18337335149652E+03 0.68378430782889E+01 0.35960045807724E+01
 0.68137853497172E+01 0.15637558910472E+03 0.67314781678970E+01 0.36592816376107E+01 0.67078440063824E+01
 0.15643533543329E+03 0.68378430782889E+01 0.35960045807724E+01 0.68137853497172E+01 0.15637558910472E+03
 0.67314781678971E+01 0.36592816376106E+01 0.67078440063825E+01 0.15643533543329E+03 0.74013827943458E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33496121764820E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.65220246011481E-01 0.00000000000000E+00 0.00000000000000E+00 0.65220246011481E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.16393104208473E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.16393104208473E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.82902993790356E-01 0.00000000000000E+00
 0.10680342942751E+00 0.18970642321787E+00 0.00000000000000E+00 0.52589048838931E+00 0.20089048540907E+00
 0.32500000298023E+00 0.42250006586313E+00
    100.56209584
 0.83307207812843E+00 0.29483582864515E+03 0.40349251645172E+03 0.37331956554719E+03 0.36603334409918E+03
 0.22999999615563E+00 0.00000000000000E+00 0.21302336194126E+00 0.00000000000000E+00 -.48558177900369E+01
 0.99762605088962E-03 0.26577961211379E+00 0.80000000000000E+04 0.30000000000000E+04 0.30100126704131E+02
 0.11287547514049E+02 0.29997391867303E+03 0.29415000000001E+03 0.30006125217692E+03 0.30665355799262E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29879696326287E+03 0.30660430742872E+03 0.29415000000000E+03
 0.29415000000000E+03 0.30006125217692E+03 0.30665355799262E+03 0.29415000000000E+03 0.29415000000000E+03
 0.29879696326287E+03 0.30660430742872E+03 0.29415000000000E+03 0.29415000000000E+03 0.32538892720253E+03
 0.29434043454532E+03 0.95163430064478E+03 0.93551946959455E+03 0.46040617709504E+03 0.13110302117947E+04
 0.84832200381417E+03 0.62862717241435E+03 0.54948030136511E+03 0.61466516094038E+03 0.13411181001891E+04
 0.50516050242529E+03 0.54523609500989E+03 0.49542341921524E+03 0.13373743211632E+04 0.62862717241435E+03
 0.54948030136510E+03 0.61466516094037E+03 0.13411181001891E+04 0.50516050242528E+03 0.54523609500989E+03
 0.49542341921524E+03 0.13373743211632E+04 0.12142092945038E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.43705359017874E+03 0.14248871960261E+01
 0.14248871960261E+01 0.22254844820650E+00 0.61013421648264E+00 0.29415550952276E+03 0.31905372837105E+03
 0.30892622487184E+03 0.30843460862978E+03 0.23000000000000E+00 0.00000000000000E+00 0.22519837048876E+00
 0.00000000000000E+00 0.75621798338201E-01 0.99963288643393E-03 0.53791911914289E-01 0.80000000000000E+04
 0.30000000000000E+04 0.14872124293978E+03 0.55770466102416E+02 0.29434001211942E+03 0.32541667036188E+03
 0.29420956993147E+03 0.29573923561952E+03 0.29415000000000E+03 0.29415000000000E+03 0.29420871468456E+03
 0.29573977441560E+03 0.29415000000000E+03 0.29415000000000E+03 0.29420956993147E+03 0.29573923561952E+03
 0.29415000000000E+03 0.29415000000000E+03 0.29420871468456E+03 0.29573977441560E+03 0.29415000000000E+03
 0.29415000000000E+03 0.29537761502151E+03 0.29415000000001E+03 0.12574455436540E+02 0.12382650109075E+02
 0.80988923419527E+01 0.19888699216906E+03 0.19074760536540E+03 0.92592003281775E+01 0.67939704730906E+01
 0.92186547534751E+01 0.16183357849893E+03 0.91111967962034E+01 0.68619397804065E+01 0.90714629088649E+01
 0.16189725830729E+03 0.92592003281775E+01 0.67939704730903E+01 0.92186547534751E+01 0.16183357849893E+03
 0.91111967962030E+01 0.68619397804065E+01 0.90714629088644E+01 0.16189725830729E+03 0.80629019087975E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33627962347663E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.55716896516632E-01 0.00000000000000E+00 0.00000000000000E+00 0.55716896516632E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.14179837567106E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.14179837567106E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.95789141128741E-01 0.00000000000000E+00
 0.63812959924016E-01 0.15960210105276E+00 0.00000000000000E+00 0.49256710824132E+00 0.16756710526109E+00
 0.32500000298023E+00 0.42250006586313E+00
    110.30291606
 0.68885476806456E+00 0.29513955812917E+03 0.40872843197209E+03 0.38264635285683E+03 0.37554110564251E+03
 0.22999999675634E+00 0.00000000000000E+00 0.21109891445260E+00 0.00000000000000E+00 -.59122277457976E+01
 0.99658899830854E-03 0.29225903037626E+00 0.80000000000000E+04 0.30000000000000E+04 0.27372977969922E+02
 0.10264866738721E+02 0.30094129319141E+03 0.29415000000001E+03 0.30089385959734E+03 0.30848057614422E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29950787349863E+03 0.30843030240368E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30089385959734E+03 0.30848057614422E+03 0.29415000000001E+03 0.29415000000001E+03
 0.29950787349863E+03 0.30843030240368E+03 0.29415000000001E+03 0.29415000000001E+03 0.32935685614517E+03
 0.29441373264120E+03 0.10438139593576E+04 0.10248871091290E+04 0.49333038237378E+03 0.13593724800061E+04
 0.86357544572048E+03 0.67187894453225E+03 0.60796616411633E+03 0.65578142339342E+03 0.14200956697169E+04
 0.54709371155881E+03 0.60412766442683E+03 0.53583887196197E+03 0.14167648543544E+04 0.67187894453225E+03
 0.60796616411633E+03 0.65578142339341E+03 0.14200956697169E+04 0.54709371155881E+03 0.60412766442683E+03
 0.53583887196197E+03 0.14167648543544E+04 0.13117537209274E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.44473771481678E+03 0.13142010665878E+01
 0.13142010665878E+01 0.26151172909081E+00 0.56382896303743E+00 0.29415969379891E+03 0.31952514109559E+03
 0.30999062519808E+03 0.30949361122060E+03 0.23000000000000E+00 0.00000000000000E+00 0.22469276483168E+00
 0.00000000000000E+00 0.26238179425661E-01 0.99961817978779E-03 0.67253106742248E-01 0.80000000000000E+04
 0.30000000000000E+04 0.11895361251727E+03 0.44607604693977E+02 0.29441324496000E+03 0.32938474613344E+03
 0.29423105189338E+03 0.29592510414525E+03 0.29415000000001E+03 0.29415000000001E+03 0.29422984135339E+03
 0.29592573036124E+03 0.29415000000001E+03 0.29415000000001E+03 0.29423105189338E+03 0.29592510414525E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29422984135339E+03 0.29592573036124E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29555024848838E+03 0.29415000000001E+03 0.15525757717205E+02 0.15232721147304E+02
 0.10882663298907E+02 0.20363162241677E+03 0.19269454580137E+03 0.11364203781321E+02 0.95354943901266E+01
 0.11305652102868E+02 0.16377319720138E+03 0.11183236293085E+02 0.96061307435881E+01 0.11125941680686E+02
 0.16383891836727E+03 0.11364203781321E+02 0.95354943901247E+01 0.11305652102867E+02 0.16377319720138E+03
 0.11183236293087E+02 0.96061307435857E+01 0.11125941680688E+02 0.16383891836726E+03 0.84463094070723E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33630616449356E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.47755372867414E-01 0.00000000000000E+00 0.00000000000000E+00 0.47755372867414E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.12343133407487E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.12343133407487E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10156780068626E+00 0.00000000000000E+00
 0.33121000657489E-01 0.13468880134375E+00 0.00000000000000E+00 0.46941448151871E+00 0.14441447853848E+00
 0.32500000298023E+00 0.42250006586313E+00
    120.14884940
 0.56867489236075E+00 0.29553814376280E+03 0.41324177074773E+03 0.39093007161236E+03 0.38423396638633E+03
 0.22999999675231E+00 0.00000000000000E+00 0.20910994298855E+00 0.00000000000000E+00 -.67971363252558E+01
 0.99523622883973E-03 0.31958706190964E+00 0.80000000000000E+04 0.30000000000000E+04 0.25032302472438E+02
 0.93871134271643E+01 0.30195348419421E+03 0.29415000000001E+03 0.30174526382483E+03 0.31029251873491E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30024701054846E+03 0.31024187888984E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30174526382483E+03 0.31029251873491E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30024701054846E+03 0.31024187888984E+03 0.29415000000001E+03 0.29415000000001E+03 0.33318301952311E+03
 0.29449194274543E+03 0.11341768860422E+04 0.11125635261040E+04 0.52639411158631E+03 0.14012027096146E+04
 0.87217662747031E+03 0.71467987297568E+03 0.66492295722724E+03 0.69659107938700E+03 0.14926516903404E+04
 0.58910322247658E+03 0.66145066631184E+03 0.57645803860784E+03 0.14896880355865E+04 0.71467987297568E+03
 0.66492295722723E+03 0.69659107938699E+03 0.14926516903404E+04 0.58910322247658E+03 0.66145066631184E+03
 0.57645803860784E+03 0.14896880355865E+04 0.14023131941476E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.45224930017177E+03 0.12947545776739E+01
 0.12947545776739E+01 0.30089546244122E+00 0.53014638758328E+00 0.29416599690147E+03 0.31958625169039E+03
 0.31060194752520E+03 0.31011484400220E+03 0.23000000000000E+00 0.00000000000000E+00 0.22421943102175E+00
 0.00000000000000E+00 -.54672394532906E-03 0.99959649661045E-03 0.79371913247475E-01 0.80000000000000E+04
 0.30000000000000E+04 0.10079132116995E+03 0.37796745438733E+02 0.29449142602168E+03 0.33321223668915E+03
 0.29425411161940E+03 0.29608800880318E+03 0.29415000000001E+03 0.29415000000001E+03 0.29425253871752E+03
 0.29608871065362E+03 0.29415000000001E+03 0.29415000000001E+03 0.29425411161940E+03 0.29608800880318E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29425253871752E+03 0.29608871065362E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29570302360883E+03 0.29415000000001E+03 0.18010127011568E+02 0.17601415431107E+02
 0.13284732883738E+02 0.20419936263346E+03 0.19084820608531E+03 0.13219142554968E+02 0.11879690700359E+02
 0.13141707061680E+02 0.16279157179797E+03 0.13012498285512E+02 0.11950877019832E+02 0.12936812225981E+02
 0.16285733155341E+03 0.13219142554968E+02 0.11879690700358E+02 0.13141707061680E+02 0.16279157179797E+03
 0.13012498285512E+02 0.11950877019831E+02 0.12936812225981E+02 0.16285733155341E+03 0.86198803482784E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33592256549072E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.40659484638660E-01 0.00000000000000E+00 0.00000000000000E+00 0.40659484638660E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.10683483044599E+00
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.10683483044599E+00 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10593071106039E+00 0.00000000000000E+00
 0.63907396093620E-02 0.11232145066975E+00 0.00000000000000E+00 0.45257319379164E+00 0.12757319081141E+00
 0.32500000298023E+00 0.42250006586313E+00
    130.19118668
 0.47133252413502E+00 0.29605933295131E+03 0.41739081058588E+03 0.39832832004903E+03 0.39214181865988E+03
 0.22999999674422E+00 0.00000000000000E+00 0.20698013466427E+00 0.00000000000000E+00 -.75419733521310E+01
 0.99347689011161E-03 0.34874306229234E+00 0.80000000000000E+04 0.30000000000000E+04 0.22939524437890E+02
 0.86023216642089E+01 0.30302104780241E+03 0.29415000000001E+03 0.30262955973509E+03 0.31211604918676E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30102654168702E+03 0.31206553310043E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30262955973509E+03 0.31211604918676E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30102654168702E+03 0.31206553310043E+03 0.29415000000001E+03 0.29415000000001E+03 0.33696034847367E+03
 0.29457316163209E+03 0.12230681538557E+04 0.11990038642216E+04 0.56061894241129E+03 0.14410808992437E+04
 0.87765886212035E+03 0.75766076189298E+03 0.72224369333319E+03 0.73781227447334E+03 0.15612770324675E+04
 0.63170620619266E+03 0.71910182597553E+03 0.61788167140245E+03 0.15586392339746E+04 0.75766076189298E+03
 0.72224369333319E+03 0.73781227447334E+03 0.15612770324675E+04 0.63170620619266E+03 0.71910182597553E+03
 0.61788167140245E+03 0.15586392339746E+04 0.14877056333980E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46157944944649E+03 0.12947600647007E+01
 0.12947600647007E+01 0.34106481156787E+00 0.50811682311360E+00 0.29417477530423E+03 0.31931798276739E+03
 0.31080085830134E+03 0.31033303468804E+03 0.23000000000000E+00 0.00000000000000E+00 0.22377124344073E+00
 0.00000000000000E+00 -.12695764575412E-01 0.99956654388212E-03 0.90546790649306E-01 0.80000000000000E+04
 0.30000000000000E+04 0.88352109916126E+02 0.33132041218547E+02 0.29457263607547E+03 0.33699177101459E+03
 0.29427817886809E+03 0.29622730184819E+03 0.29415000000001E+03 0.29415000000001E+03 0.29427624735717E+03
 0.29622806600318E+03 0.29415000000001E+03 0.29415000000001E+03 0.29427817886809E+03 0.29622730184819E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29427624735717E+03 0.29622806600318E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29583548506936E+03 0.29415000000001E+03 0.19953529781109E+02 0.19419267443438E+02
 0.15247887018499E+02 0.20131001307229E+03 0.18598588661869E+03 0.14784630340239E+02 0.13775253107276E+02
 0.14688983971480E+02 0.15944917250582E+03 0.14558875401735E+02 0.13845100293930E+02 0.14465490436828E+02
 0.15951320493647E+03 0.14784630340238E+02 0.13775253107275E+02 0.14688983971480E+02 0.15944917250582E+03
 0.14558875401735E+02 0.13845100293928E+02 0.14465490436828E+02 0.15951320493646E+03 0.86094757802515E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33514222870661E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34130026666651E-01 0.00000000000000E+00 0.00000000000000E+00 0.34130026666651E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.91608223654722E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.91608223654722E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10913417871346E+00 0.16987808282811E-01
 0.00000000000000E+00 0.10913417871346E+00 0.16987808282811E-01 0.44155841155680E+00 0.11655840857657E+00
 0.32500000298023E+00 0.42250006586313E+00
    140.45005347
 0.39667643336023E+00 0.29672381954663E+03 0.42135154532209E+03 0.40487258473596E+03 0.39918240778977E+03
 0.22999999654196E+00 0.00000000000000E+00 0.20464946838734E+00 0.00000000000000E+00 -.81787960131191E+01
 0.99124585656804E-03 0.38049506882056E+00 0.80000000000000E+04 0.30000000000000E+04 0.21025239630038E+02
 0.78844648612642E+01 0.30412090414000E+03 0.29415000000001E+03 0.30353565330432E+03 0.31395184506672E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30183466030537E+03 0.31390174085519E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30353565330432E+03 0.31395184506672E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30183466030537E+03 0.31390174085519E+03 0.29415000000001E+03 0.29415000000001E+03 0.34068928137779E+03
 0.29465435822328E+03 0.13099531243740E+04 0.12839092248731E+04 0.59691634731230E+03 0.14809860798015E+04
 0.88108515075262E+03 0.80090031375779E+03 0.78102746704549E+03 0.77970944198513E+03 0.16324548506954E+04
 0.67485815768288E+03 0.77818500950910E+03 0.66023120296100E+03 0.16301104093933E+04 0.80090031375779E+03
 0.78102746704548E+03 0.77970944198513E+03 0.16324548506954E+04 0.67485815768288E+03 0.77818500950909E+03
 0.66023120296100E+03 0.16301104093933E+04 0.15711151787998E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.46992203679362E+03 0.12947647560576E+01
 0.12947647560576E+01 0.38210027872053E+00 0.49437004915650E+00 0.29418590505624E+03 0.31877243581402E+03
 0.31066920620114E+03 0.31022735117350E+03 0.23000000000000E+00 0.00000000000000E+00 0.22334687659250E+00
 0.00000000000000E+00 -.34030826441395E-01 0.99952850392650E-03 0.10095702685510E+00 0.80000000000000E+04
 0.30000000000000E+04 0.79241636260564E+02 0.29715613597711E+02 0.29465384688831E+03 0.34072329425858E+03
 0.29430183011767E+03 0.29634443996285E+03 0.29415000000001E+03 0.29415000000001E+03 0.29429955779301E+03
 0.29634525590857E+03 0.29415000000001E+03 0.29415000000001E+03 0.29430183011767E+03 0.29634443996285E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29429955779301E+03 0.29634525590857E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29594743591762E+03 0.29415000000001E+03 0.21340815318448E+02 0.20677573688534E+02
 0.16721862072988E+02 0.19548827785503E+03 0.17868280647167E+03 0.16016764700302E+02 0.15180982762583E+02
 0.15905602401681E+02 0.15403469803084E+03 0.15777169451562E+02 0.15248756291100E+02 0.15668765407809E+02
 0.15409636955853E+03 0.16016764700302E+02 0.15180982762583E+02 0.15905602401681E+02 0.15403469803084E+03
 0.15777169451562E+02 0.15248756291099E+02 0.15668765407809E+02 0.15409636955853E+03 0.84428430910005E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33401965240383E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.27098917501251E-01 0.00000000000000E+00 0.00000000000000E+00 0.27098917501251E-01 0.00000000000000E+00
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.77744079592123E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.77744079592123E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.10427143094236E+00 0.29566231904164E-01
 0.00000000000000E+00 0.10427143094236E+00 0.29566231904164E-01 0.43468502457825E+00 0.10968502159802E+00
 0.32500000298023E+00 0.42250006586313E+00
    150.40934183
 0.34788496541028E+00 0.29749625682222E+03 0.42510476702741E+03 0.41030707297448E+03 0.40496166061986E+03
 0.23000000000000E+00 0.00000000000000E+00 0.20217684959554E+00 0.00000000000000E+00 -.86683131524582E+01
 0.10314885089618E-02 0.41395609958814E+00 0.77557819893237E+04 0.29084182459964E+04 0.19325720790102E+02
 0.72471452962882E+01 0.30520742230766E+03 0.29415000000001E+03 0.30442979292279E+03 0.31574292535236E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30264101814349E+03 0.31569348165908E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30442979292279E+03 0.31574292535236E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30264101814349E+03 0.31569348165908E+03 0.29415000000001E+03 0.29415000000001E+03 0.34425399008316E+03
 0.29473270739956E+03 0.13900088406960E+04 0.13625351515485E+04 0.63425331204606E+03 0.15218729316891E+04
 0.88444835308286E+03 0.84233080867024E+03 0.83964369952909E+03 0.82032568054204E+03 0.17044563321410E+04
 0.71634339366058E+03 0.83705963592615E+03 0.70135978416224E+03 0.17023628905683E+04 0.84233080867024E+03
 0.83964369952909E+03 0.82032568054204E+03 0.17044563321410E+04 0.71634339366058E+03 0.83705963592615E+03
 0.70135978416224E+03 0.17023628905683E+04 0.16496615074624E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.20020000000000E+06 0.50050000000000E+08 0.47814296494282E+03 0.12947683622696E+01
 0.12947683622696E+01 0.42193743215692E+00 0.48663400099420E+00 0.29419870462352E+03 0.31806229781459E+03
 0.31032040725948E+03 0.30990703966545E+03 0.23000000000000E+00 0.00000000000000E+00 0.22296127455349E+00
 0.00000000000000E+00 -.50544908974194E-01 0.99948483654968E-03 0.11031127138596E+00 0.80000000000000E+04
 0.30000000000000E+04 0.72522054178935E+02 0.27195770317101E+02 0.29473221313044E+03 0.34429063052934E+03
 0.29432396580998E+03 0.29643406237608E+03 0.29415000000001E+03 0.29415000000001E+03 0.29432138925226E+03
 0.29643491456708E+03 0.29415000000001E+03 0.29415000000001E+03 0.29432396580998E+03 0.29643406237608E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29432138925226E+03 0.29643491456708E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29603471141413E+03 0.29415000000001E+03 0.22194972774717E+02 0.21405070317637E+02
 0.17765505152422E+02 0.18817482320771E+03 0.17032049052953E+03 0.16945291755652E+02 0.16147849292703E+02
 0.16822274822024E+02 0.14766755184750E+03 0.16697420468961E+02 0.16212120609826E+02 0.16577606849086E+02
 0.14772555739308E+03 0.16945291755652E+02 0.16147849292702E+02 0.16822274822024E+02 0.14766755184749E+03
 0.16697420468961E+02 0.16212120609824E+02 0.16577606849086E+02 0.14772555739307E+03 0.81859865233757E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33283029269108E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.20137874782152E-01 0.18487701707636E-02 0.00000000000000E+00 0.20137874782152E-01 0.18487701707636E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.66712654709344E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.66712654709344E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.98507545197805E-01 0.37129749655257E-01
 0.00000000000000E+00 0.98507545197805E-01 0.37129749655257E-01 0.43081700049710E+00 0.10581699751687E+00
 0.32500000298023E+00 0.42250006586313E+00
    160.36863019
 0.32405602377193E+00 0.29835650420442E+03 0.42907898588904E+03 0.41495851667827E+03 0.40968940801706E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19940938507655E+00 0.00000000000000E+00 -.90253686159028E+01
 0.10867278888197E-02 0.45078870508247E+00 0.73615484449272E+04 0.27605806668477E+04 0.17746673574122E+02
 0.66550025902959E+01 0.30614936348197E+03 0.29415000000001E+03 0.30518580042045E+03 0.31759129957378E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30337569572775E+03 0.31752945286059E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30518580042045E+03 0.31759129957378E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30337569572775E+03 0.31752945286059E+03 0.29415000000001E+03 0.29415000000001E+03 0.34783364429074E+03
 0.29481146764424E+03 0.13442340116805E+04 0.13164288167561E+04 0.67501216459577E+03 0.15669433560866E+04
 0.88855613066780E+03 0.80189154677920E+03 0.91111683834250E+03 0.78014445675146E+03 0.17935602412743E+04
 0.70824671709886E+03 0.90174385002467E+03 0.69361348285820E+03 0.17848016688051E+04 0.80189154677919E+03
 0.91111683834249E+03 0.78014445675146E+03 0.17935602412743E+04 0.70824671709886E+03 0.90174385002467E+03
 0.69361348285820E+03 0.17848016688051E+04 0.16983465017290E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.14880785782188E+06 0.50050000000000E+08 0.48296381851840E+03 0.11720382731847E+01
 0.11720382731847E+01 0.46177458559330E+00 0.48228381810763E+00 0.29421335312373E+03 0.31723974101078E+03
 0.30983623816585E+03 0.30945275802684E+03 0.23000000000000E+00 0.00000000000000E+00 0.22259709403959E+00
 0.00000000000000E+00 -.58585413424234E-01 0.99943497272488E-03 0.11910068963481E+00 0.80000000000000E+04
 0.30000000000000E+04 0.67170056063739E+02 0.25188771023902E+02 0.29481098609732E+03 0.34787305947479E+03
 0.29434504661516E+03 0.29650351681977E+03 0.29415000000001E+03 0.29415000000001E+03 0.29434219535015E+03
 0.29650439102178E+03 0.29415000000001E+03 0.29415000000001E+03 0.29434504661516E+03 0.29650351681977E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29434219535015E+03 0.29650439102178E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29610332257225E+03 0.29415000000001E+03 0.22658676051494E+02 0.21739782362216E+02
 0.18525174075539E+02 0.18025614073023E+03 0.16163834078432E+03 0.17675235345700E+02 0.16812894311509E+02
 0.17543920814743E+02 0.14114385881547E+03 0.17423238155652E+02 0.16872318056329E+02 0.17295522752822E+02
 0.14119694474842E+03 0.17675235345700E+02 0.16812894311508E+02 0.17543920814743E+02 0.14114385881546E+03
 0.17423238155652E+02 0.16872318056328E+02 0.17295522752822E+02 0.14119694474842E+03 0.78894867427239E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33172709433665E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.16409081500235E-01 0.42397711219393E-02 0.00000000000000E+00 0.16409081500235E-01 0.42397711219393E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.59996108478868E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.59996108478868E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.94616535446432E-01 0.40490815951443E-01
 0.00000000000000E+00 0.94616535446432E-01 0.40490815951443E-01 0.42864190905381E+00 0.10364190607358E+00
 0.32500000298023E+00 0.42250006586313E+00
    170.32791855
 0.31324821463517E+00 0.29920408496457E+03 0.43329201202100E+03 0.41929107743614E+03 0.41392293190481E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19646040620189E+00 0.00000000000000E+00 -.93283947686487E+01
 0.11242049064757E-02 0.48839230175610E+00 0.71161404419409E+04 0.26685526657278E+04 0.16380274568691E+02
 0.61426029632592E+01 0.30663412247404E+03 0.29415000000001E+03 0.30550115655740E+03 0.31953525138106E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30384652713756E+03 0.31942788864418E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30550115655740E+03 0.31953525138106E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30384652713756E+03 0.31942788864418E+03 0.29415000000001E+03 0.29415000000001E+03 0.35145660162985E+03
 0.29489407433637E+03 0.12514875141022E+04 0.12254405614813E+04 0.71935351676165E+03 0.16165651596103E+04
 0.89361487526487E+03 0.73250908203182E+03 0.98706296200164E+03 0.71285670301131E+03 0.18879086123792E+04
 0.68002532823999E+03 0.97015979430452E+03 0.66674434384041E+03 0.18720729959719E+04 0.73250908203182E+03
 0.98706296200164E+03 0.71285670301131E+03 0.18879086123792E+04 0.68002532823999E+03 0.97015979430452E+03
 0.66674434384041E+03 0.18720729959719E+04 0.17366506791599E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.81951965899049E+05 0.50050000000000E+08 0.48699776307592E+03 0.12280456097683E+01
 0.12280456097683E+01 0.50161173902968E+00 0.47901084042094E+00 0.29422992837595E+03 0.31647256651268E+03
 0.30936958998798E+03 0.30901272003029E+03 0.23000000000000E+00 0.00000000000000E+00 0.22222260802143E+00
 0.00000000000000E+00 -.64255294267738E-01 0.99937859234593E-03 0.12798438302030E+00 0.80000000000000E+04
 0.30000000000000E+04 0.62507626408849E+02 0.23440359903318E+02 0.29489359549344E+03 0.35149873772034E+03
 0.29436550513222E+03 0.29656154982664E+03 0.29415000000001E+03 0.29415000000001E+03 0.29436240257196E+03
 0.29656243141700E+03 0.29415000000001E+03 0.29415000000001E+03 0.29436550513222E+03 0.29656154982664E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29436240257196E+03 0.29656243141700E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29616084445468E+03 0.29415000000001E+03 0.23003758873754E+02 0.21947140964608E+02
 0.19275981075024E+02 0.17315538935972E+03 0.15378302837932E+03 0.18375394074976E+02 0.17451290829696E+02
 0.18238969676746E+02 0.13534070549329E+03 0.18120271864638E+02 0.17504929106097E+02 0.17987799194478E+02
 0.13538803324118E+03 0.18375394074976E+02 0.17451290829695E+02 0.18238969676746E+02 0.13534070549328E+03
 0.18120271864637E+02 0.17504929106096E+02 0.17987799194478E+02 0.13538803324118E+03 0.76202459366878E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.33076857798845E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.13751028334239E-01 0.64975105411609E-02 0.00000000000000E+00 0.13751028334239E-01 0.64975105411609E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.54637241722401E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.54637241722401E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.91336302395352E-01 0.42807690983395E-01
 0.00000000000000E+00 0.91336302395352E-01 0.42807690983395E-01 0.42700542021047E+00 0.10200541723024E+00
 0.32500000298023E+00 0.42250006586313E+00
    180.28720691
 0.30878755286709E+00 0.29999028679239E+03 0.43703133976087E+03 0.42292581596472E+03 0.41740498450605E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19341260489025E+00 0.00000000000000E+00 -.95952631080658E+01
 0.11404436699734E-02 0.52681748447486E+00 0.70148138050400E+04 0.26305551768900E+04 0.15185524846379E+02
 0.56945718173922E+01 0.30715032347305E+03 0.29415000000001E+03 0.30585780967906E+03 0.32148814231230E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30433942575226E+03 0.32134598327210E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30585780967906E+03 0.32148814231230E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30433942575226E+03 0.32134598327210E+03 0.29415000000001E+03 0.29415000000001E+03 0.35505336352635E+03
 0.29498211375076E+03 0.12435655570252E+04 0.12188141027647E+04 0.76042326306446E+03 0.16567202470523E+04
 0.89249486767249E+03 0.72160281097073E+03 0.10519717524053E+04 0.70364517291357E+03 0.19667408161978E+04
 0.68542029959807E+03 0.10334796145538E+04 0.67319785010021E+03 0.19496622411040E+04 0.72160281097073E+03
 0.10519717524053E+04 0.70364517291356E+03 0.19667408161978E+04 0.68542029959807E+03 0.10334796145538E+04
 0.67319785010021E+03 0.19496622411040E+04 0.17877379848754E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.54358813827485E+05 0.50050000000000E+08 0.49071682111866E+03 0.12501359350398E+01
 0.12501359350398E+01 0.54144889246606E+00 0.47739418068371E+00 0.29424838428861E+03 0.31565827390138E+03
 0.30884430276123E+03 0.30851395203673E+03 0.23000000000000E+00 0.00000000000000E+00 0.22186246000485E+00
 0.00000000000000E+00 -.71797968568071E-01 0.99931581171362E-03 0.13648433088507E+00 0.80000000000000E+04
 0.30000000000000E+04 0.58614787119676E+02 0.21980545169879E+02 0.29498162639324E+03 0.35509782160241E+03
 0.29438521842305E+03 0.29660587426235E+03 0.29415000000001E+03 0.29415000000001E+03 0.29438188545120E+03
 0.29660675090102E+03 0.29415000000001E+03 0.29415000000001E+03 0.29438521842305E+03 0.29660587426235E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29438188545120E+03 0.29660675090102E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29620581741895E+03 0.29415000000001E+03 0.23051866198637E+02 0.21845137707557E+02
 0.19814846482165E+02 0.16504415471992E+03 0.14513023400535E+03 0.18923132105868E+02 0.17875443437672E+02
 0.18785013767740E+02 0.12877154569929E+03 0.18666840795081E+02 0.17922851142206E+02 0.18532979962573E+02
 0.12881278177229E+03 0.18923132105868E+02 0.17875443437671E+02 0.18785013767740E+02 0.12877154569929E+03
 0.18666840795081E+02 0.17922851142205E+02 0.18532979962573E+02 0.12881278177229E+03 0.72977255734944E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32919861329213E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.10883057239962E-01 0.94474105874136E-02 0.00000000000000E+00 0.10883057239962E-01 0.94474105874136E-02
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.47252736549252E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.47252736549252E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.86340234980761E-01 0.46347389079199E-01
 0.00000000000000E+00 0.86340234980761E-01 0.46347389079199E-01 0.42619709034186E+00 0.10119708736163E+00
 0.32500000298023E+00 0.42250006586313E+00
    190.24649527
 0.30735417392003E+00 0.30072133325988E+03 0.43998620976659E+03 0.42571832940832E+03 0.42005641430430E+03
 0.23000000000000E+00 0.00000000000000E+00 0.19031736616916E+00 0.00000000000000E+00 -.98011776629489E+01
 0.11457613802114E-02 0.56616011909556E+00 0.69822566357788E+04 0.26183462384171E+04 0.14130278220197E+02
 0.52988543325738E+01 0.30775304105592E+03 0.29415000000001E+03 0.30630837360611E+03 0.32339513029283E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30488498979108E+03 0.32323112778750E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30630837360611E+03 0.32339513029283E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30488498979108E+03 0.32323112778750E+03 0.29415000000001E+03 0.29415000000001E+03 0.35849854061055E+03
 0.29507580419795E+03 0.12723770899472E+04 0.12482543653052E+04 0.79386890317282E+03 0.16812813368727E+04
 0.88344308918398E+03 0.73639078600712E+03 0.11053543700394E+04 0.71953814603903E+03 0.20274548326275E+04
 0.70485991334236E+03 0.10877111939123E+04 0.69330843922494E+03 0.20114407426478E+04 0.73639078600712E+03
 0.11053543700394E+04 0.71953814603903E+03 0.20274548326275E+04 0.70485991334236E+03 0.10877111939123E+04
 0.69330843922494E+03 0.20114407426478E+04 0.18350681709537E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.45492092852276E+05 0.50050000000000E+08 0.49366982069575E+03 0.12570610760034E+01
 0.12570610760034E+01 0.58128604590245E+00 0.47755003811405E+00 0.29426869809008E+03 0.31475145051231E+03
 0.30823042437903E+03 0.30792773856677E+03 0.23000000000000E+00 0.00000000000000E+00 0.22153421675695E+00
 0.00000000000000E+00 -.77485965517476E-01 0.99924674741888E-03 0.14427970558113E+00 0.80000000000000E+04
 0.30000000000000E+04 0.55447853651889E+02 0.20792945119458E+02 0.29507531393216E+03 0.35854422925975E+03
 0.29440375105799E+03 0.29663510571061E+03 0.29415000000001E+03 0.29415000000001E+03 0.29440021216755E+03
 0.29663596645005E+03 0.29415000000001E+03 0.29415000000001E+03 0.29440375105799E+03 0.29663510571061E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29440021216755E+03 0.29663596645005E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29623707445606E+03 0.29415000000001E+03 0.22718138403189E+02 0.21347904938630E+02
 0.20048233898497E+02 0.15583986350018E+03 0.13569138843219E+03 0.19264247882791E+02 0.17995045543174E+02
 0.19128420739808E+02 0.12139966135826E+03 0.19009376013260E+02 0.18035902978853E+02 0.18878051518217E+02
 0.12143457326740E+03 0.19264247882791E+02 0.17995045543174E+02 0.19128420739808E+02 0.12139966135826E+03
 0.19009376013260E+02 0.18035902978852E+02 0.18878051518217E+02 0.12143457326740E+03 0.69170326409637E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32729198783031E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.87199523163843E-02 0.12111631439933E-01 0.00000000000000E+00 0.87199523163843E-02 0.12111631439933E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.40080731640590E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.40080731640590E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.81115859390098E-01 0.49426607356401E-01
 0.00000000000000E+00 0.81115859390098E-01 0.49426607356401E-01 0.42627501905702E+00 0.10127501607679E+00
 0.32500000298023E+00 0.42250006586313E+00
    200.20578363
 0.30721983636361E+00 0.30141276832733E+03 0.44217184525120E+03 0.42775718505812E+03 0.42199070732374E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18720587581929E+00 0.00000000000000E+00 -.99485321317749E+01
 0.11462617630147E-02 0.60624205704415E+00 0.69792086398833E+04 0.26172032399562E+04 0.13196049180431E+02
 0.49485184426614E+01 0.30842013751354E+03 0.29415000000002E+03 0.30683047649434E+03 0.32522337679081E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30546763793429E+03 0.32504712050519E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30683047649434E+03 0.32522337679081E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30546763793429E+03 0.32504712050519E+03 0.29415000000001E+03 0.29415000000001E+03 0.36171629187513E+03
 0.29517594441720E+03 0.13122441790949E+04 0.12882707037879E+04 0.81893180777909E+03 0.16909430423916E+04
 0.86791657557364E+03 0.75952621832670E+03 0.11479501086607E+04 0.74334426500856E+03 0.20713091282608E+04
 0.72778975098522E+03 0.11319543398310E+04 0.71663585588680E+03 0.20570608484542E+04 0.75952621832671E+03
 0.11479501086607E+04 0.74334426500856E+03 0.20713091282608E+04 0.72778975098522E+03 0.11319543398310E+04
 0.71663585588680E+03 0.20570608484542E+04 0.18724755193778E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.44661095835228E+05 0.50050000000000E+08 0.49584883158571E+03 0.12576133229550E+01
 0.12576133229550E+01 0.62112319933883E+00 0.47911486474552E+00 0.29429093140405E+03 0.31377167545173E+03
 0.30754933275203E+03 0.30727480805064E+03 0.23000000000000E+00 0.00000000000000E+00 0.22124137608406E+00
 0.00000000000000E+00 -.80112450757760E-01 0.99917120537966E-03 0.15132090590225E+00 0.80000000000000E+04
 0.30000000000000E+04 0.52867777603498E+02 0.19825416601312E+02 0.29517546371304E+03 0.36176211687730E+03
 0.29442085228431E+03 0.29664990967578E+03 0.29415000000001E+03 0.29415000000001E+03 0.29441713334020E+03
 0.29665074449296E+03 0.29415000000001E+03 0.29415000000001E+03 0.29442085228431E+03 0.29664990967578E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29441713334020E+03 0.29665074449296E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29625519728956E+03 0.29415000000001E+03 0.22030433410872E+02 0.20481094220554E+02
 0.20007615626600E+02 0.14602622694770E+03 0.12591857324296E+03 0.19417780036439E+02 0.17841488397939E+02
 0.19288556846210E+02 0.11359908325189E+03 0.19166517275979E+02 0.17875518088793E+02 0.19041971090882E+02
 0.11362746069253E+03 0.19417780036439E+02 0.17841488397939E+02 0.19288556846210E+02 0.11359908325189E+03
 0.19166517275979E+02 0.17875518088792E+02 0.19041971090882E+02 0.11362746069252E+03 0.64997233211881E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32530782930303E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.73262491348339E-02 0.14081930335283E-01 0.00000000000000E+00 0.73262491348339E-02 0.14081930335283E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.34106883729975E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.00000000000000E+00 0.34106883729975E-01 0.00000000000000E+00 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.76423531551145E-01 0.51394943428432E-01
 0.00000000000000E+00 0.76423531551145E-01 0.51394943428432E-01 0.42705743237276E+00 0.10205742939253E+00
 0.32500000298023E+00 0.42250006586313E+00
    210.16507199
 0.30756412887535E+00 0.30207752829868E+03 0.44373442211635E+03 0.42921156240095E+03 0.42337976366595E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18409557943142E+00 0.00000000000000E+00 -.10056859804565E+02
 0.11449781720061E-02 0.64682238146525E+00 0.69870327623655E+04 0.26201372858870E+04 0.12368155817177E+02
 0.46380584314415E+01 0.30912067252731E+03 0.29415000000005E+03 0.30739343764459E+03 0.32695772684677E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30606707270202E+03 0.32677552159940E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30739343764459E+03 0.32695772684677E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30606707270202E+03 0.32677552159940E+03 0.29415000000001E+03 0.29415000000001E+03 0.36467924387241E+03
 0.29528410812568E+03 0.13516241840211E+04 0.13275248668556E+04 0.83659189476164E+03 0.16889914081191E+04
 0.84821655388365E+03 0.78314805786985E+03 0.11813410156621E+04 0.76737517035380E+03 0.21013687408544E+04
 0.74952824293251E+03 0.11671119387033E+04 0.73861776842933E+03 0.20889416191187E+04 0.78314805786985E+03
 0.11813410156621E+04 0.76737517035380E+03 0.21013687408544E+04 0.74952824293250E+03 0.11671119387033E+04
 0.73861776842932E+03 0.20889416191188E+04 0.18993431185307E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.46790850595319E+05 0.50050000000000E+08 0.49740266916622E+03 0.12558347908370E+01
 0.12558347908370E+01 0.66096035277521E+00 0.48150346208287E+00 0.29431527310680E+03 0.31277033113528E+03
 0.30684621491284E+03 0.30659896302560E+03 0.23000000000000E+00 0.00000000000000E+00 0.22097709688875E+00
 0.00000000000000E+00 -.79358309865327E-01 0.99908855127047E-03 0.15774949229766E+00 0.80000000000000E+04
 0.30000000000000E+04 0.50713316939901E+02 0.19017493852463E+02 0.29528364706226E+03 0.36472452201288E+03
 0.29443652275481E+03 0.29665275106883E+03 0.29415000000001E+03 0.29415000000001E+03 0.29443264757996E+03
 0.29665355050147E+03 0.29415000000001E+03 0.29415000000001E+03 0.29443652275481E+03 0.29665275106883E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29443264757996E+03 0.29665355050147E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29626237436620E+03 0.29415000000001E+03 0.21080759733028E+02 0.19332773789205E+02
 0.19794780837743E+02 0.13642750949903E+03 0.11653375475709E+03 0.19446495173337E+02 0.17514827730517E+02
 0.19328235768272E+02 0.10599709587423E+03 0.19200014298595E+02 0.17541757287365E+02 0.19086529883124E+02
 0.10601871322763E+03 0.19446495173337E+02 0.17514827730517E+02 0.19328235768272E+02 0.10599709587423E+03
 0.19200014298595E+02 0.17541757287363E+02 0.19086529883124E+02 0.10601871322763E+03 0.60827776310613E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32357346373269E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.62934673240497E-02 0.15687902643118E-01 0.00000000000000E+00 0.62934673240497E-02 0.15687902643118E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.30047994754954E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.44501849750880E-04 0.30047994754954E-01 0.44501849750880E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.72833529509964E-01 0.51866306574476E-01
 0.00000000000000E+00 0.72833529509964E-01 0.51866306574476E-01 0.42825173104144E+00 0.10325172806120E+00
 0.32500000298023E+00 0.42250006586313E+00
    221.36716307
 0.30818358555501E+00 0.30279966369090E+03 0.44494654589554E+03 0.43034410061778E+03 0.42447627363087E+03
 0.23000000000000E+00 0.00000000000000E+00 0.18061107686949E+00 0.00000000000000E+00 -.10146962027363E+02
 0.11426763814415E-02 0.69279432641703E+00 0.70011073388143E+04 0.26254152520554E+04 0.11547438676893E+02
 0.43302895038349E+01 0.30991928028205E+03 0.29415000000011E+03 0.30804672558460E+03 0.32878708886389E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30674148224097E+03 0.32860286737028E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30804672558460E+03 0.32878708886389E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30674148224097E+03 0.32860286737028E+03 0.29415000000001E+03 0.29415000000001E+03 0.36771565834991E+03
 0.29541952141944E+03 0.13918356640627E+04 0.13674285334465E+04 0.84938863439964E+03 0.16770997690433E+04
 0.82346419147165E+03 0.80776096970652E+03 0.12100485448406E+04 0.79227246414987E+03 0.21227260663307E+04
 0.77129564424638E+03 0.11977032878056E+04 0.76056782917264E+03 0.21121968088856E+04 0.80776096970652E+03
 0.12100485448406E+04 0.79227246414987E+03 0.21227260663307E+04 0.77129564424637E+03 0.11977032878057E+04
 0.76056782917264E+03 0.21121968088856E+04 0.19192685460004E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.50622739954408E+05 0.50050000000000E+08 0.49860551417041E+03 0.12527064656540E+01
 0.12527064656540E+01 0.70576871711536E+00 0.48431070366657E+00 0.29434556981188E+03 0.31169649041803E+03
 0.30609433270601E+03 0.30587489972948E+03 0.23000000000000E+00 0.00000000000000E+00 0.22069462534465E+00
 0.00000000000000E+00 -.76122442433830E-01 0.99898572239274E-03 0.16462794187318E+00 0.80000000000000E+04
 0.30000000000000E+04 0.48594423941489E+02 0.18222908978058E+02 0.29541908515611E+03 0.36776003185488E+03
 0.29445270596662E+03 0.29664807835378E+03 0.29415000000001E+03 0.29415000000001E+03 0.29444867472196E+03
 0.29664882912840E+03 0.29415000000001E+03 0.29415000000001E+03 0.29445270596662E+03 0.29664807835378E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29444867472196E+03 0.29664882912840E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29626304503631E+03 0.29415000000002E+03 0.19844561877224E+02 0.17839327443847E+02
 0.19501972069379E+02 0.12667915860850E+03 0.10707967667877E+03 0.19428343018678E+02 0.17093586066004E+02
 0.19327382771983E+02 0.98278583507361E+02 0.19186623633897E+02 0.17112455201853E+02 0.19090455459847E+02
 0.98292553929398E+02 0.19428343018678E+02 0.17093586066004E+02 0.19327382771983E+02 0.98278583507360E+02
 0.19186623633897E+02 0.17112455201851E+02 0.19090455459847E+02 0.98292553929395E+02 0.56512584842066E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32206204057946E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.53801209519138E-02 0.17225176593201E-01 0.00000000000000E+00 0.53801209519138E-02 0.17225176593201E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27448330280921E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19523699461034E-03 0.27448330280921E-01 0.19523699461034E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.69998126548720E-01 0.51098477309261E-01
 0.00000000000000E+00 0.69998126548720E-01 0.51098477309261E-01 0.42965535183328E+00 0.10465534885305E+00
 0.32500000298023E+00 0.42250006586313E+00
    230.40148401
 0.30869049892330E+00 0.30336656356415E+03 0.44563412569729E+03 0.43099524411880E+03 0.42512005811429E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17781715462588E+00 0.00000000000000E+00 -.10197921434812E+02
 0.11407997310699E-02 0.72995264517593E+00 0.70126243740409E+04 0.26297341402653E+04 0.10959615055675E+02
 0.41098556458782E+01 0.31056446720250E+03 0.29415000000022E+03 0.30858117933548E+03 0.33017787434904E+03
 0.29415000000001E+03 0.29415000000001E+03 0.30728215493949E+03 0.32999460783131E+03 0.29415000000001E+03
 0.29415000000001E+03 0.30858117933548E+03 0.33017787434904E+03 0.29415000000001E+03 0.29415000000001E+03
 0.30728215493949E+03 0.32999460783131E+03 0.29415000000001E+03 0.29415000000001E+03 0.36994981707980E+03
 0.29554085802456E+03 0.14196538635154E+04 0.13949216030474E+04 0.85569257022742E+03 0.16629012560781E+04
 0.80293022299953E+03 0.82499639356069E+03 0.12279384720381E+04 0.80964836493131E+03 0.21329883407913E+04
 0.78631891697672E+03 0.12169420244686E+04 0.77569469443013E+03 0.21237934409126E+04 0.82499639356069E+03
 0.12279384720381E+04 0.80964836493131E+03 0.21329883407913E+04 0.78631891697671E+03 0.12169420244687E+04
 0.77569469443013E+03 0.21237934409126E+04 0.19291517954349E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.53758449045438E+05 0.50050000000000E+08 0.49928761342895E+03 0.12501587514998E+01
 0.12501587514998E+01 0.74190600086584E+00 0.48628451108933E+00 0.29437271761788E+03 0.31090367320845E+03
 0.30554450810365E+03 0.30534475801639E+03 0.23000000000000E+00 0.00000000000000E+00 0.22046732976099E+00
 0.00000000000000E+00 -.72893194040776E-01 0.99889360583135E-03 0.17011720431387E+00 0.80000000000000E+04
 0.30000000000000E+04 0.47026401781445E+02 0.17634900668042E+02 0.29554044178031E+03 0.36999344897950E+03
 0.29446503616973E+03 0.29663912232978E+03 0.29415000000001E+03 0.29415000000001E+03 0.29446088938562E+03
 0.29663982499264E+03 0.29415000000001E+03 0.29415000000001E+03 0.29446503616973E+03 0.29663912232978E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29446088938562E+03 0.29663982499264E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29625874292591E+03 0.29415000000004E+03 0.18791030024106E+02 0.16548072457550E+02
 0.19327095567562E+02 0.11984347170169E+03 0.10041974065629E+03 0.19442386736983E+02 0.16805623505388E+02
 0.19358855814578E+02 0.92858227590976E+02 0.19203710766388E+02 0.16817481003838E+02 0.19124903650973E+02
 0.92865558321176E+02 0.19442386736983E+02 0.16805623505389E+02 0.19358855814577E+02 0.92858227590978E+02
 0.19203710766388E+02 0.16817481003838E+02 0.19124903650972E+02 0.92865558321175E+02 0.53434610778479E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32111712410834E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.49228605493100E-02 0.18049621976956E-01 0.00000000000000E+00 0.49228605493100E-02 0.18049621976956E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26269769145231E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.32577765813311E-03 0.26269769145231E-01 0.32577765813311E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.68357234799170E-01 0.49966001277950E-01
 0.00000000000000E+00 0.68357234799170E-01 0.49966001277950E-01 0.43064225554466E+00 0.10564225256443E+00
 0.32500000298023E+00 0.42250006586313E+00
    241.69438518
 0.30919942597369E+00 0.30406194793409E+03 0.44627165709228E+03 0.43161460361242E+03 0.42574879050594E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17434755630440E+00 0.00000000000000E+00 -.10243195510932E+02
 0.11389218133685E-02 0.77633132083255E+00 0.70241871795738E+04 0.26340701923402E+04 0.10304878581249E+02
 0.38643294679683E+01 0.31135894501388E+03 0.29415000000045E+03 0.30924487531122E+03 0.33181990898353E+03
 0.29415000000002E+03 0.29415000000003E+03 0.30794570043146E+03 0.33163968554687E+03 0.29415000000001E+03
 0.29415000000003E+03 0.30924487531122E+03 0.33181990898353E+03 0.29415000000002E+03 0.29415000000003E+03
 0.30794570043146E+03 0.33163968554686E+03 0.29415000000001E+03 0.29415000000003E+03 0.37251070970361E+03
 0.29570992390567E+03 0.14480535924859E+04 0.14229078819757E+04 0.86015882497172E+03 0.16422328893190E+04
 0.77777327022242E+03 0.84270688534892E+03 0.12456343235605E+04 0.82749906263134E+03 0.21401827421260E+04
 0.80185099683398E+03 0.12360777243236E+04 0.79135155207867E+03 0.21323915008425E+04 0.84270688534892E+03
 0.12456343235605E+04 0.82749906263134E+03 0.21401827421260E+04 0.80185099683397E+03 0.12360777243236E+04
 0.79135155207866E+03 0.21323915008426E+04 0.19359959050669E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.56906614547525E+05 0.50050000000000E+08 0.49992128411529E+03 0.12476087434366E+01
 0.12476087434366E+01 0.78707760555395E+00 0.48816469291860E+00 0.29441078718177E+03 0.31002202690677E+03
 0.30494145620915E+03 0.30476281396429E+03 0.23000000000000E+00 0.00000000000000E+00 0.22017440022696E+00
 0.00000000000000E+00 -.68970779946411E-01 0.99876445730406E-03 0.17709285454316E+00 0.80000000000000E+04
 0.30000000000000E+04 0.45174041723124E+02 0.16940265646172E+02 0.29570953708192E+03 0.37255340363079E+03
 0.29447995150788E+03 0.29662486541958E+03 0.29415000000001E+03 0.29415000000001E+03 0.29447566516130E+03
 0.29662549723005E+03 0.29415000000001E+03 0.29415000000001E+03 0.29447995150788E+03 0.29662486541958E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29447566516130E+03 0.29662549723005E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29625036196690E+03 0.29415000000008E+03 0.17445343349813E+02 0.14861139479942E+02
 0.19223489351455E+02 0.11258330041568E+03 0.93263693617464E+02 0.19523076618805E+02 0.16551753321127E+02
 0.19465164878750E+02 0.87090255162998E+02 0.19286365200855E+02 0.16554462032643E+02 0.19232952534118E+02
 0.87088949488789E+02 0.19523076618805E+02 0.16551753321123E+02 0.19465164878750E+02 0.87090255162990E+02
 0.19286365200854E+02 0.16554462032641E+02 0.19232952534116E+02 0.87088949488786E+02 0.50102131517793E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.32019850264172E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.46152703890039E-02 0.18634455172258E-01 0.00000000000000E+00 0.46152703890039E-02 0.18634455172258E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25465192051020E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.44614761550870E-03 0.25465192051020E-01 0.44614761550870E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.66857071035592E-01 0.48316621674374E-01
 0.00000000000000E+00 0.66857071035592E-01 0.48316621674374E-01 0.43158234645930E+00 0.10658234347907E+00
 0.32500000298023E+00 0.42250006586313E+00
    250.72870612
 0.30947670953224E+00 0.30461045579554E+03 0.44668130330456E+03 0.43202543050204E+03 0.42617646524901E+03
 0.23000000000000E+00 0.00000000000000E+00 0.17158953820302E+00 0.00000000000000E+00 -.10270312173705E+02
 0.11379012137575E-02 0.81329225763122E+00 0.70304872718988E+04 0.26364327269620E+04 0.98365623488174E+01
 0.36887108808065E+01 0.31197891746632E+03 0.29415000000080E+03 0.30976590053173E+03 0.33306483529953E+03
 0.29415000000003E+03 0.29415000000005E+03 0.30846316597127E+03 0.33288790163356E+03 0.29415000000002E+03
 0.29415000000005E+03 0.30976590053173E+03 0.33306483529953E+03 0.29415000000003E+03 0.29415000000005E+03
 0.30846316597127E+03 0.33288790163356E+03 0.29415000000002E+03 0.29415000000005E+03 0.37440135326079E+03
 0.29586061375183E+03 0.14661407641892E+04 0.14406991605498E+04 0.86196240958440E+03 0.16248875160921E+04
 0.75861529445977E+03 0.85402099529776E+03 0.12571974757086E+04 0.83893182921942E+03 0.21431689991347E+04
 0.81198444452933E+03 0.12486124223294E+04 0.80160287757771E+03 0.21363122300720E+04 0.85402099529775E+03
 0.12571974757086E+04 0.83893182921941E+03 0.21431689991347E+04 0.81198444452932E+03 0.12486124223294E+04
 0.80160287757770E+03 0.21363122300720E+04 0.19385159372771E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.58621859458654E+05 0.50050000000000E+08 0.50032978163434E+03 0.12462266319469E+01
 0.12462266319469E+01 0.82321488930443E+00 0.48915528457159E+00 0.29444511763062E+03 0.30940534797370E+03
 0.30452676415322E+03 0.30436244653530E+03 0.23000000000000E+00 0.00000000000000E+00 0.21992843526492E+00
 0.00000000000000E+00 -.66211901686620E-01 0.99864801795973E-03 0.18285701590055E+00 0.80000000000000E+04
 0.30000000000000E+04 0.43750030375377E+02 0.16406261390766E+02 0.29586025327535E+03 0.37444332103358E+03
 0.29449174178080E+03 0.29661273497402E+03 0.29415000000001E+03 0.29415000000001E+03 0.29448734243252E+03
 0.29661330174302E+03 0.29415000000001E+03 0.29415000000001E+03 0.29449174178080E+03 0.29661273497402E+03
 0.29415000000001E+03 0.29415000000001E+03 0.29448734243252E+03 0.29661330174302E+03 0.29415000000001E+03
 0.29415000000001E+03 0.29624279445476E+03 0.29415000000013E+03 0.16358089835427E+02 0.13461035163575E+02
 0.19249316007896E+02 0.10773631186583E+03 0.88390749277893E+02 0.19655040599357E+02 0.16448632968751E+02
 0.19619858961708E+02 0.83230056962444E+02 0.19418236884760E+02 0.16443693092088E+02 0.19387186391710E+02
 0.83221550460158E+02 0.19655040599358E+02 0.16448632968743E+02 0.19619858961709E+02 0.83230056962428E+02
 0.19418236884757E+02 0.16443693092085E+02 0.19387186391706E+02 0.83221550460152E+02 0.47823534533242E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31962858312346E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45114446706993E-02 0.18845728216825E-01 0.00000000000000E+00 0.45114446706993E-02 0.18845728216825E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25152338940535E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.50054514092772E-03 0.25152338940535E-01 0.50054514092772E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.65985002022447E-01 0.46969258342111E-01
 0.00000000000000E+00 0.65985002022447E-01 0.46969258342111E-01 0.43207764228579E+00 0.10707763930556E+00
 0.32500000298023E+00 0.42250006586313E+00
    261.59056959
 0.30967368786073E+00 0.30526199722043E+03 0.44711954991387E+03 0.43247636608272E+03 0.42665331756849E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16829232896119E+00 0.00000000000000E+00 -.10297559739846E+02
 0.11371772382613E-02 0.85749819281867E+00 0.70349631797340E+04 0.26381111924003E+04 0.93294657259898E+01
 0.34985496472462E+01 0.31270151843996E+03 0.29415000000151E+03 0.31037568975086E+03 0.33449094056649E+03
 0.29415000000005E+03 0.29415000000010E+03 0.30906688701155E+03 0.33431844774719E+03 0.29415000000004E+03
 0.29415000000010E+03 0.31037568975086E+03 0.33449094056649E+03 0.29415000000005E+03 0.29415000000010E+03
 0.30906688701155E+03 0.33431844774719E+03 0.29415000000004E+03 0.29415000000010E+03 0.37652318857175E+03
 0.29606229219489E+03 0.14836174116508E+04 0.14578875043452E+04 0.86285104921884E+03 0.16041766683405E+04
 0.73701136387557E+03 0.86495955386381E+03 0.12690715470593E+04 0.85004401837114E+03 0.21449829626837E+04
 0.82207671583125E+03 0.12614827065240E+04 0.81187156187455E+03 0.21390735398581E+04 0.86495955386381E+03
 0.12690715470593E+04 0.85004401837113E+03 0.21449829626837E+04 0.82207671583125E+03 0.12614827065240E+04
 0.81187156187454E+03 0.21390735398581E+04 0.19394659328792E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.59840345247400E+05 0.50050000000000E+08 0.50076811317199E+03 0.12452550403493E+01
 0.12452550403493E+01 0.86666234318125E+00 0.48976845855306E+00 0.29449168820872E+03 0.30876225905179E+03
 0.30410274206213E+03 0.30395304228510E+03 0.23000000000000E+00 0.00000000000000E+00 0.21961545496451E+00
 0.00000000000000E+00 -.63519656361498E-01 0.99849010042817E-03 0.19007770352852E+00 0.80000000000000E+04
 0.30000000000000E+04 0.42088050578744E+02 0.15783018967029E+02 0.29606196687508E+03 0.37656430975639E+03
 0.29450598425647E+03 0.29659916116479E+03 0.29415000000001E+03 0.29415000000002E+03 0.29450144120002E+03
 0.29659964074658E+03 0.29415000000001E+03 0.29415000000002E+03 0.29450598425647E+03 0.29659916116480E+03
 0.29415000000001E+03 0.29415000000002E+03 0.29450144120002E+03 0.29659964074658E+03 0.29415000000001E+03
 0.29415000000002E+03 0.29623433170649E+03 0.29415000000025E+03 0.15035075272399E+02 0.11707675005185E+02
 0.19411323018177E+02 0.10291123001429E+03 0.83402850381022E+02 0.19903152094985E+02 0.16444185717917E+02
 0.19895174780593E+02 0.79375693197390E+02 0.19664100828032E+02 0.16429671377154E+02 0.19659149062544E+02
 0.79358191377539E+02 0.19903152094984E+02 0.16444185717914E+02 0.19895174780593E+02 0.79375693197383E+02
 0.19664100828031E+02 0.16429671377153E+02 0.19659149062543E+02 0.79358191377536E+02 0.45484246492700E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31908963272772E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44782263912061E-02 0.18928969932515E-01 0.00000000000000E+00 0.44782263912061E-02 0.18928969932515E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24995503564913E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.52868351710483E-03 0.24995503564913E-01 0.52868351710483E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.00000000000000E+00 0.65207913138570E-01 0.45445820550483E-01
 0.00000000000000E+00 0.65207913138570E-01 0.45445820550483E-01 0.43238422927653E+00 0.10738422629630E+00
 0.32500000298023E+00 0.42250006586313E+00
    272.45243306
 0.30975803743247E+00 0.30590516079069E+03 0.44754229102944E+03 0.43291787786600E+03 0.42712282304904E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16501305408691E+00 0.00000000000000E+00 -.10322419735926E+02
 0.11368674016960E-02 0.90140976641071E+00 0.70368804559487E+04 0.26388301709808E+04 0.88749870459635E+01
 0.33281201422363E+01 0.31340170995573E+03 0.29415000000276E+03 0.31096880053370E+03 0.33585222977312E+03
 0.29415000000010E+03 0.29415000000019E+03 0.30965314302257E+03 0.33568456949186E+03 0.29415000000008E+03
 0.29415000000018E+03 0.31096880053370E+03 0.33585222977312E+03 0.29415000000010E+03 0.29415000000019E+03
 0.30965314302257E+03 0.33568456949186E+03 0.29415000000008E+03 0.29415000000018E+03 0.37850224909347E+03
 0.29628624173872E+03 0.14977384520257E+04 0.14717841341081E+04 0.86302537973903E+03 0.15845118694588E+04
 0.71717136282111E+03 0.87379047709443E+03 0.12794881356318E+04 0.85907780785254E+03 0.21458725007976E+04
 0.83052882745873E+03 0.12727476248432E+04 0.82052951409596E+03 0.21407592761717E+04 0.87379047709443E+03
 0.12794881356318E+04 0.85907780785254E+03 0.21458725007975E+04 0.83052882745872E+03 0.12727476248432E+04
 0.82052951409595E+03 0.21407592761717E+04 0.19392598251797E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60362122212613E+05 0.50050000000000E+08 0.50119181456921E+03 0.12448516352500E+01
 0.12448516352500E+01 0.91010979705807E+00 0.48974928668243E+00 0.29454511684261E+03 0.30821706837994E+03
 0.30375318270463E+03 0.30361571609676E+03 0.23000000000000E+00 0.00000000000000E+00 0.21928189712487E+00
 0.00000000000000E+00 -.61543974592114E-01 0.99830898236997E-03 0.19765491408820E+00 0.80000000000000E+04
 0.30000000000000E+04 0.40474581858513E+02 0.15177968196942E+02 0.29628596018075E+03 0.37854245255265E+03
 0.29452056014840E+03 0.29658687078265E+03 0.29415000000001E+03 0.29415000000003E+03 0.29451586916608E+03
 0.29658725171217E+03 0.29415000000001E+03 0.29415000000003E+03 0.29452056014840E+03 0.29658687078265E+03
 0.29415000000001E+03 0.29415000000003E+03 0.29451586916608E+03 0.29658725171217E+03 0.29415000000001E+03
 0.29415000000003E+03 0.29622676891799E+03 0.29415000000047E+03 0.13695641713147E+02 0.98787080707338E+01
 0.19718716709118E+02 0.99062072513148E+02 0.79244762220484E+02 0.20255171553402E+02 0.16570612747779E+02
 0.20270873509389E+02 0.76287277502551E+02 0.20011729208435E+02 0.16546002817880E+02 0.20031284673645E+02
 0.76260315363380E+02 0.20255171553402E+02 0.16570612747776E+02 0.20270873509389E+02 0.76287277502546E+02
 0.20011729208434E+02 0.16546002817878E+02 0.20031284673643E+02 0.76260315363376E+02 0.43533605652725E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31867831041167E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44984776902177E-02 0.18912140017712E-01 0.00000000000000E+00 0.44984776902177E-02 0.18912140017712E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.24983646426132E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.52914606606397E-03 0.24983646426132E-01 0.52914606606397E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.36049897934407E-03 0.64677728168737E-01 0.43711738483930E-01
 0.00000000000000E+00 0.64677728168737E-01 0.44072237463274E-01 0.43237464334121E+00 0.10737464036098E+00
 0.32500000298023E+00 0.42250006586313E+00
    280.59883066
 0.30977518236531E+00 0.30638156138290E+03 0.44786352377820E+03 0.43325432354406E+03 0.42747998419132E+03
 0.23000000000000E+00 0.00000000000000E+00 0.16256399346469E+00 0.00000000000000E+00 -.10340972248572E+02
 0.11368043479997E-02 0.93414006561998E+00 0.70372707617427E+04 0.26389765356535E+04 0.85640262038118E+01
 0.32115098264294E+01 0.31391284767812E+03 0.29415000000422E+03 0.31140295110251E+03 0.33683664731491E+03
 0.29415000000016E+03 0.29415000000030E+03 0.31008200818248E+03 0.33667273550495E+03 0.29415000000012E+03
 0.29415000000029E+03 0.31140295110251E+03 0.33683664731492E+03 0.29415000000016E+03 0.29415000000030E+03
 0.31008200818248E+03 0.33667273550495E+03 0.29415000000012E+03 0.29415000000029E+03 0.37990978405022E+03
 0.29646942092517E+03 0.15068949256056E+04 0.14808089164301E+04 0.86288265259435E+03 0.15705826374541E+04
 0.70338557159676E+03 0.87951175220961E+03 0.12866401203088E+04 0.86496765558982E+03 0.21462926928900E+04
 0.83615682808591E+03 0.12804611773148E+04 0.82632727134636E+03 0.21417010980824E+04 0.87951175220961E+03
 0.12866401203088E+04 0.86496765558982E+03 0.21462926928900E+04 0.83615682808591E+03 0.12804611773148E+04
 0.82632727134635E+03 0.21417010980824E+04 0.19387792688961E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60468178839160E+05 0.50050000000000E+08 0.50151400511392E+03 0.12447806099561E+01
 0.12447806099561E+01 0.94269538746569E+00 0.48934581992873E+00 0.29459027980243E+03 0.30786510448022E+03
 0.30353445116931E+03 0.30340487140071E+03 0.23000000000000E+00 0.00000000000000E+00 0.21901782182013E+00
 0.00000000000000E+00 -.60558210217817E-01 0.99815593114310E-03 0.20358283957786E+00 0.80000000000000E+04
 0.30000000000000E+04 0.39296042911026E+02 0.14736016091635E+02 0.29646917780052E+03 0.37994923322129E+03
 0.29453188848029E+03 0.29657909030070E+03 0.29415000000001E+03 0.29415000000004E+03 0.29452707491049E+03
 0.29657938998666E+03 0.29415000000001E+03 0.29415000000004E+03 0.29453188848029E+03 0.29657909030070E+03
 0.29415000000001E+03 0.29415000000004E+03 0.29452707491049E+03 0.29657938998666E+03 0.29415000000001E+03
 0.29415000000004E+03 0.29622222590641E+03 0.29415000000073E+03 0.12673333761632E+02 0.84483774238134E+01
 0.20036656538509E+02 0.96724232565791E+02 0.76587392744590E+02 0.20585468851831E+02 0.16743721312880E+02
 0.20632192449711E+02 0.74401590096600E+02 0.20337379152935E+02 0.16711232521907E+02 0.20389020251425E+02
 0.74367261506730E+02 0.20585468851831E+02 0.16743721312878E+02 0.20632192449711E+02 0.74401590096597E+02
 0.20337379152935E+02 0.16711232521906E+02 0.20389020251424E+02 0.74367261506728E+02 0.42287242260239E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31843368610074E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45226688442963E-02 0.18883012258189E-01 0.00000000000000E+00 0.45226688442963E-02 0.18883012258189E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25020687922923E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.52049972803961E-03 0.25020687922923E-01 0.52049972803961E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.58079475016574E-03 0.64393837186977E-01 0.42594503312628E-01
 0.00000000000000E+00 0.64393837186977E-01 0.43175298062794E-01 0.43217290996437E+00 0.10717290698413E+00
 0.32500000298023E+00 0.42250006586313E+00
    291.46069413
 0.30976609207748E+00 0.30700789089308E+03 0.44830529933228E+03 0.43371558398799E+03 0.42796731615784E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15931117330946E+00 0.00000000000000E+00 -.10366385341770E+02
 0.11368375292193E-02 0.97750679320637E+00 0.70370653627999E+04 0.26388995110500E+04 0.81840863466113E+01
 0.30690323799792E+01 0.31457782946487E+03 0.29415000000724E+03 0.31196916802220E+03 0.33810696540757E+03
 0.29415000000028E+03 0.29415000000054E+03 0.31064106312537E+03 0.33794813433130E+03 0.29415000000022E+03
 0.29415000000054E+03 0.31196916802220E+03 0.33810696540757E+03 0.29415000000028E+03 0.29415000000054E+03
 0.31064106312537E+03 0.33794813433130E+03 0.29415000000022E+03 0.29415000000054E+03 0.38169978643552E+03
 0.29673432182198E+03 0.15179336355591E+04 0.14917068473201E+04 0.86246840114141E+03 0.15531191169994E+04
 0.68633837385226E+03 0.88640826865877E+03 0.12955199564381E+04 0.87210149306356E+03 0.21467494867514E+04
 0.84306296489419E+03 0.12900111643055E+04 0.83347130201581E+03 0.21427744562581E+04 0.88640826865876E+03
 0.12955199564381E+04 0.87210149306356E+03 0.21467494867514E+04 0.84306296489418E+03 0.12900111643055E+04
 0.83347130201580E+03 0.21427744562581E+04 0.19380265711209E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60411947340914E+05 0.50050000000000E+08 0.50195710407186E+03 0.12448447493610E+01
 0.12448447493610E+01 0.98614284134251E+00 0.48839650216125E+00 0.29465799968330E+03 0.30746279605532E+03
 0.30329358421600E+03 0.30317310127507E+03 0.23000000000000E+00 0.00000000000000E+00 0.21864655444702E+00
 0.00000000000000E+00 -.59846685786625E-01 0.99792652086443E-03 0.21182803216885E+00 0.80000000000000E+04
 0.30000000000000E+04 0.37766484058272E+02 0.14162431521852E+02 0.29673413668160E+03 0.38173812884128E+03
 0.29454767213975E+03 0.29657102136695E+03 0.29415000000001E+03 0.29415000000008E+03 0.29454268966296E+03
 0.29657120308535E+03 0.29415000000001E+03 0.29415000000008E+03 0.29454767213975E+03 0.29657102136695E+03
 0.29415000000001E+03 0.29415000000008E+03 0.29454268966296E+03 0.29657120308535E+03 0.29415000000001E+03
 0.29415000000008E+03 0.29621803499379E+03 0.29415000000126E+03 0.11278397823076E+02 0.64533337351461E+01
 0.20568200715542E+02 0.94240732763871E+02 0.73569691044752E+02 0.21112090680535E+02 0.17070197220548E+02
 0.21216669006996E+02 0.72384857302051E+02 0.20855999474614E+02 0.17026809349595E+02 0.20966605903210E+02
 0.72340361467367E+02 0.21112090680534E+02 0.17070197220548E+02 0.21216669006995E+02 0.72384857302050E+02
 0.20855999474615E+02 0.17026809349595E+02 0.20966605903211E+02 0.72340361467367E+02 0.40870588946356E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31817521610270E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45512535883651E-02 0.18851659934365E-01 0.00000000000000E+00 0.45512535883651E-02 0.18851659934365E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25098723599500E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.50357999729428E-03 0.25098723599500E-01 0.50357999729428E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.81743200861448E-03 0.64134794544235E-01 0.41335925057178E-01
 0.00000000000000E+00 0.64134794544235E-01 0.42153357065793E-01 0.43169825108062E+00 0.10669824810039E+00
 0.32500000298023E+00 0.42250006586313E+00
    302.32255760
 0.30974297886537E+00 0.30762316014937E+03 0.44876444072867E+03 0.43419193383283E+03 0.42846738655939E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15607190789247E+00 0.00000000000000E+00 -.10392856138099E+02
 0.11369221789930E-02 0.10205635142591E+01 0.70365414166571E+04 0.26387030312464E+04 0.78388065889342E+01
 0.29395524708503E+01 0.31522643531551E+03 0.29415000001205E+03 0.31252291304958E+03 0.33933489051385E+03
 0.29415000000049E+03 0.29415000000095E+03 0.31118743463514E+03 0.33918117200047E+03 0.29415000000038E+03
 0.29415000000095E+03 0.31252291304958E+03 0.33933489051385E+03 0.29415000000049E+03 0.29415000000095E+03
 0.31118743463514E+03 0.33918117200047E+03 0.29415000000038E+03 0.29415000000095E+03 0.38340397483307E+03
 0.29702297016455E+03 0.15281806842347E+04 0.15018370133657E+04 0.86187040456937E+03 0.15368323106458E+04
 0.67065255405353E+03 0.89281630411116E+03 0.13037917909927E+04 0.87875043194728E+03 0.21471856999621E+04
 0.84954314884432E+03 0.12988793506701E+04 0.84019291226441E+03 0.21437536223542E+04 0.89281630411117E+03
 0.13037917909927E+04 0.87875043194728E+03 0.21471856999621E+04 0.84954314884432E+03 0.12988793506701E+04
 0.84019291226440E+03 0.21437536223542E+04 0.19373201867066E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60268971609959E+05 0.50050000000000E+08 0.50241745690275E+03 0.12449766907480E+01
 0.12449766907480E+01 0.10295902952193E+01 0.48708779435289E+00 0.29473510834000E+03 0.30712733461768E+03
 0.30310326650787E+03 0.30299055352338E+03 0.23000000000000E+00 0.00000000000000E+00 0.21825315433003E+00
 0.00000000000000E+00 -.59756160306025E-01 0.99766542915870E-03 0.22047040506407E+00 0.80000000000000E+04
 0.30000000000000E+04 0.36286049357396E+02 0.13607268509024E+02 0.29702285023174E+03 0.38344108520066E+03
 0.29456440178815E+03 0.29656583000958E+03 0.29415000000001E+03 0.29415000000013E+03 0.29455923344288E+03
 0.29656588279255E+03 0.29415000000001E+03 0.29415000000013E+03 0.29456440178815E+03 0.29656583000958E+03
 0.29415000000001E+03 0.29415000000013E+03 0.29455923344288E+03 0.29656588279255E+03 0.29415000000001E+03
 0.29415000000013E+03 0.29621621076000E+03 0.29415000000211E+03 0.98380185446418E+01 0.43480723100310E+01
 0.21211691217631E+02 0.92381387071940E+02 0.71063637398221E+02 0.21733237090510E+02 0.17495466578731E+02
 0.21915391784867E+02 0.70858581443573E+02 0.21467228425159E+02 0.17440755717664E+02 0.21656428766840E+02
 0.70803550658822E+02 0.21733237090508E+02 0.17495466578730E+02 0.21915391784864E+02 0.70858581443572E+02
 0.21467228425159E+02 0.17440755717664E+02 0.21656428766841E+02 0.70803550658823E+02 0.39690168298548E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31797985118959E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45676375312729E-02 0.18843674103920E-01 0.00000000000000E+00 0.45676375312729E-02 0.18843674103920E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25190764016580E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.48432958578337E-03 0.25190764016580E-01 0.48432958578337E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.10050167557737E-02 0.63984802594811E-01 0.40310737355224E-01
 0.00000000000000E+00 0.63984802594811E-01 0.41315754110998E-01 0.43104389717645E+00 0.10604389419621E+00
 0.32500000298023E+00 0.42250006586313E+00
    310.46895520
 0.30972541485214E+00 0.30807692010031E+03 0.44911934827667E+03 0.43455787341711E+03 0.42884947277197E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15365109090453E+00 0.00000000000000E+00 -.10413386284196E+02
 0.11369865144540E-02 0.10526555692574E+01 0.70361432596603E+04 0.26385537223726E+04 0.75998267939092E+01
 0.28499350477160E+01 0.31570361976243E+03 0.29415000001734E+03 0.31293121594491E+03 0.34023113432883E+03
 0.29415000000074E+03 0.29415000000143E+03 0.31158999500848E+03 0.34008123917821E+03 0.29415000000057E+03
 0.29415000000142E+03 0.31293121594491E+03 0.34023113432883E+03 0.29415000000074E+03 0.29415000000143E+03
 0.31158999500848E+03 0.34008123917821E+03 0.29415000000057E+03 0.29415000000142E+03 0.38463254930299E+03
 0.29725497991678E+03 0.15355636268044E+04 0.15091391541536E+04 0.86131988917559E+03 0.15253033960602E+04
 0.65967690743870E+03 0.89744083717236E+03 0.13096501467110E+04 0.88355208567036E+03 0.21475128251005E+04
 0.85422697702316E+03 0.13051437787600E+04 0.84505495164565E+03 0.21444471757789E+04 0.89744083717236E+03
 0.13096501467110E+04 0.88355208567035E+03 0.21475128251005E+04 0.85422697702316E+03 0.13051437787600E+04
 0.84505495164565E+03 0.21444471757789E+04 0.19368640758952E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60160322599309E+05 0.50050000000000E+08 0.50277314747562E+03 0.12450749648309E+01
 0.12450749648309E+01 0.10621758856269E+01 0.48593216665474E+00 0.29479962700895E+03 0.30691380103758E+03
 0.30298935648227E+03 0.30288176508840E+03 0.23000000000000E+00 0.00000000000000E+00 0.21794361575231E+00
 0.00000000000000E+00 -.60053041583491E-01 0.99744707050851E-03 0.22721382020426E+00 0.80000000000000E+04
 0.30000000000000E+04 0.35209125892114E+02 0.13203422209543E+02 0.29725491323813E+03 0.38466865580843E+03
 0.29457765745147E+03 0.29656390733047E+03 0.29415000000001E+03 0.29415000000019E+03 0.29457233776990E+03
 0.29656385628704E+03 0.29415000000001E+03 0.29415000000019E+03 0.29457765745147E+03 0.29656390733047E+03
 0.29415000000001E+03 0.29415000000019E+03 0.29457233776990E+03 0.29656385628704E+03 0.29415000000001E+03
 0.29415000000019E+03 0.29621648012660E+03 0.29415000000306E+03 0.87226820822887E+01 0.26917641322539E+01
 0.21760558667580E+02 0.91338922231113E+02 0.69469560770196E+02 0.22258171338288E+02 0.17872513811916E+02
 0.22512733859941E+02 0.69990649037574E+02 0.21983533967517E+02 0.17809053935416E+02 0.22245896927595E+02
 0.69927497338803E+02 0.22258171338286E+02 0.17872513811916E+02 0.22512733859940E+02 0.69990649037573E+02
 0.21983533967517E+02 0.17809053935416E+02 0.22245896927595E+02 0.69927497338804E+02 0.38934853819975E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31786790793947E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45703335597239E-02 0.18855854940953E-01 0.00000000000000E+00 0.45703335597239E-02 0.18855854940953E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25263593810481E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.46945544556779E-03 0.25263593810481E-01 0.46945544556779E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.11212996000238E-02 0.63931514434054E-01 0.39673486620749E-01
 0.00000000000000E+00 0.63931514434054E-01 0.40794786220773E-01 0.43046608332737E+00 0.10546608034714E+00
 0.32500000298023E+00 0.42250006586313E+00
    321.33081867
 0.30970770869166E+00 0.30867137543000E+03 0.44960433152395E+03 0.43505499055362E+03 0.42936600399986E+03
 0.23000000000000E+00 0.00000000000000E+00 0.15043477199149E+00 0.00000000000000E+00 -.10441520688658E+02
 0.11370513324462E-02 0.10951814486809E+01 0.70357421619559E+04 0.26384033107335E+04 0.73047256321185E+01
 0.27392721120445E+01 0.31632912712817E+03 0.29415000002756E+03 0.31346757111485E+03 0.34139652583020E+03
 0.29415000000123E+03 0.29415000000241E+03 0.31211833207840E+03 0.34125167894309E+03 0.29415000000095E+03
 0.29415000000239E+03 0.31346757111485E+03 0.34139652583020E+03 0.29415000000123E+03 0.29415000000241E+03
 0.31211833207840E+03 0.34125167894309E+03 0.29415000000095E+03 0.29415000000239E+03 0.38621149786109E+03
 0.29758476182488E+03 0.15451584544107E+04 0.15186265160640E+04 0.86045799656185E+03 0.15107302533849E+04
 0.64596996684021E+03 0.90346361169620E+03 0.13170437028017E+04 0.88980053266563E+03 0.21479372121811E+04
 0.86031182537567E+03 0.13130305616323E+04 0.85136862414488E+03 0.21453127712809E+04 0.90346361169619E+03
 0.13170437028016E+04 0.88980053266562E+03 0.21479372121811E+04 0.86031182537567E+03 0.13130305616323E+04
 0.85136862414487E+03 0.21453127712809E+04 0.19363599726136E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60050794281717E+05 0.50050000000000E+08 0.50325898566977E+03 0.12451750067026E+01
 0.12451750067026E+01 0.11056233395038E+01 0.48422741568932E+00 0.29489523365422E+03 0.30667309925842E+03
 0.30287098897585E+03 0.30276946580554E+03 0.23000000000000E+00 0.00000000000000E+00 0.21751174414436E+00
 0.00000000000000E+00 -.60885708952184E-01 0.99712367088905E-03 0.23655272609121E+00 0.80000000000000E+04
 0.30000000000000E+04 0.33819098736217E+02 0.12682162026081E+02 0.29758477131118E+03 0.38624617324564E+03
 0.29459637766151E+03 0.29656400367266E+03 0.29415000000002E+03 0.29415000000032E+03 0.29459083939234E+03
 0.29656380484432E+03 0.29415000000001E+03 0.29415000000032E+03 0.29459637766151E+03 0.29656400367266E+03
 0.29415000000002E+03 0.29415000000032E+03 0.29459083939234E+03 0.29656380484432E+03 0.29415000000001E+03
 0.29415000000032E+03 0.29621906546044E+03 0.29415000000490E+03 0.71832534642558E+01 0.37572570854577E+00
 0.22571721198879E+02 0.90353278309230E+02 0.67668698504357E+02 0.23032808853005E+02 0.18444494522207E+02
 0.23404986757002E+02 0.69151433325331E+02 0.22745168457269E+02 0.18369060120548E+02 0.23126174338304E+02
 0.69077193942620E+02 0.23032808853004E+02 0.18444494522207E+02 0.23404986757000E+02 0.69151433325331E+02
 0.22745168457269E+02 0.18369060120549E+02 0.23126174338304E+02 0.69077193942623E+02 0.38073220726387E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31775812375819E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45616172724564E-02 0.18895345420646E-01 0.00000000000000E+00 0.45616172724564E-02 0.18895345420646E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25363840546983E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.44945140043331E-03 0.25363840546983E-01 0.44945140043331E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.12510331968825E-02 0.63927829925798E-01 0.38972625601370E-01
 0.00000000000000E+00 0.63927829925798E-01 0.40223658798252E-01 0.42961370784466E+00 0.10461370486443E+00
 0.32500000298023E+00 0.42250006586313E+00
    331.83768293
 0.30970146733403E+00 0.30923466347970E+03 0.45008222423601E+03 0.43554199215712E+03 0.42986972448774E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14733613477305E+00 0.00000000000000E+00 -.10469518595532E+02
 0.11370740693510E-02 0.11360372583837E+01 0.70356014754311E+04 0.26383505532867E+04 0.70420225577651E+01
 0.26407584591619E+01 0.31692227618601E+03 0.29415000004248E+03 0.31397722816940E+03 0.34249391892008E+03
 0.29415000000199E+03 0.29415000000392E+03 0.31261975482786E+03 0.34235379699867E+03 0.29415000000154E+03
 0.29415000000390E+03 0.31397722816940E+03 0.34249391892008E+03 0.29415000000199E+03 0.29415000000392E+03
 0.31261975482786E+03 0.34235379699867E+03 0.29415000000154E+03 0.29415000000390E+03 0.38768610450904E+03
 0.29792776996197E+03 0.15542784352062E+04 0.15276427243926E+04 0.85939862513019E+03 0.14972156115503E+04
 0.63351999329443E+03 0.90920400612542E+03 0.13237567438818E+04 0.89574854976153E+03 0.21482906289887E+04
 0.86607539034654E+03 0.13201747099690E+04 0.85734493510835E+03 0.21460488555180E+04 0.90920400612543E+03
 0.13237567438818E+04 0.89574854976154E+03 0.21482906289886E+04 0.86607539034654E+03 0.13201747099690E+04
 0.85734493510835E+03 0.21460488555180E+04 0.19359235783976E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60012185945288E+05 0.50050000000000E+08 0.50373749886047E+03 0.12452149644612E+01
 0.12452149644612E+01 0.11476507965570E+01 0.48246243990857E+00 0.29499863681315E+03 0.30648147585736E+03
 0.30278811682913E+03 0.30269179906119E+03 0.23000000000000E+00 0.00000000000000E+00 0.21707353240606E+00
 0.00000000000000E+00 -.62133824187506E-01 0.99677413337256E-03 0.24595938293530E+00 0.80000000000000E+04
 0.30000000000000E+04 0.32525695521461E+02 0.12197135820548E+02 0.29792785411515E+03 0.38771933759767E+03
 0.29461553590718E+03 0.29656719209482E+03 0.29415000000002E+03 0.29415000000052E+03 0.29460976841259E+03
 0.29656684237690E+03 0.29415000000002E+03 0.29415000000052E+03 0.29461553590718E+03 0.29656719209482E+03
 0.29415000000002E+03 0.29415000000052E+03 0.29460976841259E+03 0.29656684237690E+03 0.29415000000002E+03
 0.29415000000052E+03 0.29622415587109E+03 0.29415000000761E+03 0.56188967834060E+01 -.20062930547124E+01
 0.23432245940186E+02 0.89772049769167E+02 0.66222642599281E+02 0.23859609376921E+02 0.19063510133383E+02
 0.24370979361084E+02 0.68632009368345E+02 0.23557628196222E+02 0.18976197104742E+02 0.24078861748871E+02
 0.68546790373407E+02 0.23859609376918E+02 0.19063510133383E+02 0.24370979361080E+02 0.68632009368345E+02
 0.23557628196224E+02 0.18976197104742E+02 0.24078861748872E+02 0.68546790373407E+02 0.37364315571610E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31768554163464E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45357751909113E-02 0.18966215717121E-01 0.00000000000000E+00 0.45357751909113E-02 0.18966215717121E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25453345173423E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.43179137287088E-03 0.25453345173423E-01 0.43179137287088E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.13553784778092E-02 0.63977202811505E-01 0.38440158811066E-01
 0.00000000000000E+00 0.63977202811505E-01 0.39795537288876E-01 0.42873121995428E+00 0.10373121697405E+00
 0.32500000298023E+00 0.42250006586313E+00
    343.52478053
 0.30970393417327E+00 0.30984780361018E+03 0.45062189018520E+03 0.43608912737122E+03 0.43043331624861E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14390427565547E+00 0.00000000000000E+00 -.10500851630121E+02
 0.11370648151389E-02 0.11811581972540E+01 0.70356587359731E+04 0.26383720259899E+04 0.67730131481105E+01
 0.25398799305414E+01 0.31757224278080E+03 0.29415000006724E+03 0.31453704386668E+03 0.34368355209779E+03
 0.29415000000333E+03 0.29415000000657E+03 0.31316986416852E+03 0.34354860985784E+03 0.29415000000258E+03
 0.29415000000654E+03 0.31453704386668E+03 0.34368355209779E+03 0.29415000000333E+03 0.29415000000657E+03
 0.31316986416852E+03 0.34354860985784E+03 0.29415000000258E+03 0.29415000000654E+03 0.38925946057775E+03
 0.29833330827330E+03 0.15642079787181E+04 0.15374394859355E+04 0.85812789930011E+03 0.14830627948352E+04
 0.62064425603857E+03 0.91547199633178E+03 0.13307656595487E+04 0.90222310002631E+03 0.21486126067773E+04
 0.87233918182542E+03 0.13276138695346E+04 0.86382452061419E+03 0.21467483012975E+04 0.91547199633178E+03
 0.13307656595487E+04 0.90222310002631E+03 0.21486126067773E+04 0.87233918182542E+03 0.13276138695346E+04
 0.86382452061418E+03 0.21467483012975E+04 0.19355331467160E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60027445535425E+05 0.50050000000000E+08 0.50427765174926E+03 0.12452099579025E+01
 0.12452099579025E+01 0.11943991869331E+01 0.48042477743791E+00 0.29512717119794E+03 0.30630968489544E+03
 0.30272811379256E+03 0.30263697713502E+03 0.23000000000000E+00 0.00000000000000E+00 0.21656261841716E+00
 0.00000000000000E+00 -.63944324482796E-01 0.99633998495766E-03 0.25685255332135E+00 0.80000000000000E+04
 0.30000000000000E+04 0.31146273986971E+02 0.11679852745114E+02 0.29833348237803E+03 0.38929101244924E+03
 0.29463848706783E+03 0.29657383292850E+03 0.29415000000003E+03 0.29415000000086E+03 0.29463243971630E+03
 0.29657330271521E+03 0.29415000000003E+03 0.29415000000086E+03 0.29463848706783E+03 0.29657383292850E+03
 0.29415000000003E+03 0.29415000000086E+03 0.29463243971630E+03 0.29657330271521E+03 0.29415000000003E+03
 0.29415000000086E+03 0.29623244205758E+03 0.29415000001211E+03 0.38077253603886E+01 -.47899882705824E+01
 0.24473079307710E+02 0.89510224912713E+02 0.64914780208464E+02 0.24867150135203E+02 0.19823913666533E+02
 0.25565699127927E+02 0.68356870242140E+02 0.24547715087517E+02 0.19723022950197E+02 0.25257350698066E+02
 0.68259143426663E+02 0.24867150135201E+02 0.19823913666533E+02 0.25565699127925E+02 0.68356870242140E+02
 0.24547715087519E+02 0.19723022950198E+02 0.25257350698068E+02 0.68259143426666E+02 0.36708507849748E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31764768660074E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.45044314707606E-02 0.19050422045984E-01 0.00000000000000E+00 0.45044314707606E-02 0.19050422045984E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25575962592137E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.40884662718948E-03 0.25575962592137E-01 0.40884662718948E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14493382279890E-02 0.64113085630507E-01 0.37962148741865E-01
 0.00000000000000E+00 0.64113085630507E-01 0.39411486969854E-01 0.42771238871896E+00 0.10271238573872E+00
 0.32500000298023E+00 0.42250006586313E+00
    351.31617892
 0.30971042589925E+00 0.31024905151664E+03 0.45098608315012E+03 0.43645684114779E+03 0.43081082491234E+03
 0.23000000000000E+00 0.00000000000000E+00 0.14162516344468E+00 0.00000000000000E+00 -.10522036853246E+02
 0.11370408504183E-02 0.12110480824127E+01 0.70358070222868E+04 0.26384276333576E+04 0.66058483690110E+01
 0.24771931383791E+01 0.31800016420802E+03 0.29415000009010E+03 0.31490630705847E+03 0.34446010972111E+03
 0.29415000000462E+03 0.29415000000914E+03 0.31353237897396E+03 0.34432853990610E+03 0.29415000000358E+03
 0.29415000000910E+03 0.31490630705847E+03 0.34446010972111E+03 0.29415000000462E+03 0.29415000000914E+03
 0.31353237897396E+03 0.34432853990610E+03 0.29415000000358E+03 0.29415000000910E+03 0.39027453889578E+03
 0.29861724339179E+03 0.15707236218333E+04 0.15438564229919E+04 0.85722080585912E+03 0.14740627071163E+04
 0.61255579722792E+03 0.91959397988607E+03 0.13352050376617E+04 0.90646918364217E+03 0.21488009774270E+04
 0.87644288092946E+03 0.13323139969485E+04 0.86806038643331E+03 0.21471632124643E+04 0.91959397988607E+03
 0.13352050376617E+04 0.90646918364217E+03 0.21488009774270E+04 0.87644288092945E+03 0.13323139969485E+04
 0.86806038643330E+03 0.21471632124643E+04 0.19353244225343E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60067602622328E+05 0.50050000000000E+08 0.50464205945194E+03 0.12451810709406E+01
 0.12451810709406E+01 0.12255647805171E+01 0.47904786500646E+00 0.29522124035272E+03 0.30621668422792E+03
 0.30270512161911E+03 0.30261714963423E+03 0.23000000000000E+00 0.00000000000000E+00 0.21620828594660E+00
 0.00000000000000E+00 -.65401834815887E-01 0.99602248861187E-03 0.26436674026353E+00 0.80000000000000E+04
 0.30000000000000E+04 0.30260992710450E+02 0.11347872266419E+02 0.29861748255007E+03 0.39030493492322E+03
 0.29465476450179E+03 0.29658005979898E+03 0.29415000000004E+03 0.29415000000119E+03 0.29464851652335E+03
 0.29657940266015E+03 0.29415000000004E+03 0.29415000000119E+03 0.29465476450179E+03 0.29658005979898E+03
 0.29415000000004E+03 0.29415000000119E+03 0.29464851652335E+03 0.29657940266015E+03 0.29415000000004E+03
 0.29415000000119E+03 0.29623949664253E+03 0.29415000001628E+03 0.25569816675259E+01 -.67232625692080E+01
 0.25208672110492E+02 0.89530227055991E+02 0.64195511584947E+02 0.25584904059916E+02 0.20367550403448E+02
 0.26429203433361E+02 0.68327301643305E+02 0.25252931126121E+02 0.20257479193764E+02 0.26109181238283E+02
 0.68221144214498E+02 0.25584904059914E+02 0.20367550403448E+02 0.26429203433358E+02 0.68327301643305E+02
 0.25252931126122E+02 0.20257479193765E+02 0.26109181238285E+02 0.68221144214501E+02 0.36336688062527E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31764206608661E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44768266238513E-02 0.19119423826320E-01 0.00000000000000E+00 0.44768266238513E-02 0.19119423826320E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25660569786911E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.39350005883398E-03 0.25660569786911E-01 0.39350005883398E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15014896963983E-02 0.64236961413729E-01 0.37710570017634E-01
 0.00000000000000E+00 0.64236961413729E-01 0.39212059714032E-01 0.42702393250323E+00 0.10202392952300E+00
 0.32500000298023E+00 0.42250006586313E+00
    363.00327652
 0.30972845706434E+00 0.31084000164024E+03 0.45153675754755E+03 0.43701082784050E+03 0.43137794382933E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13821994643027E+00 0.00000000000000E+00 -.10554097011100E+02
 0.11369744607763E-02 0.12555989877068E+01 0.70362178535988E+04 0.26385816950996E+04 0.63714610144845E+01
 0.23892978804317E+01 0.31863465466943E+03 0.29415000013718E+03 0.31545478265983E+03 0.34560219968265E+03
 0.29415000000738E+03 0.29415000001469E+03 0.31407031618802E+03 0.34547554583840E+03 0.29415000000573E+03
 0.29415000001462E+03 0.31545478265984E+03 0.34560219968264E+03 0.29415000000738E+03 0.29415000001469E+03
 0.31407031618802E+03 0.34547554583841E+03 0.29415000000573E+03 0.29415000001462E+03 0.39175127274781E+03
 0.29906283151754E+03 0.15803765701157E+04 0.15533444411587E+04 0.85575896172779E+03 0.14611120197876E+04
 0.60107426325119E+03 0.92571474207213E+03 0.13415307533950E+04 0.91275535209682E+03 0.21490199981815E+04
 0.88250878131682E+03 0.13389957559655E+04 0.87430675776193E+03 0.21476886025670E+04 0.92571474207211E+03
 0.13415307533950E+04 0.91275535209678E+03 0.21490199981815E+04 0.88250878131684E+03 0.13389957559654E+04
 0.87430675776196E+03 0.21476886025668E+04 0.19350610461023E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60179141381876E+05 0.50050000000000E+08 0.50519290357661E+03 0.12450945222729E+01
 0.12450945222729E+01 0.12723131708931E+01 0.47698916007708E+00 0.29537530435471E+03 0.30610539910553E+03
 0.30269330651704E+03 0.30260973638926E+03 0.23000000000000E+00 0.00000000000000E+00 0.21565642165160E+00
 0.00000000000000E+00 -.67935306295290E-01 0.99550293836800E-03 0.27601321098243E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28984119896019E+02 0.10869044961007E+02 0.29906317097062E+03 0.39177993266464E+03
 0.29468070176461E+03 0.29659205051621E+03 0.29415000000006E+03 0.29415000000189E+03 0.29467413140391E+03
 0.29659119377386E+03 0.29415000000006E+03 0.29415000000189E+03 0.29468070176461E+03 0.29659205051621E+03
 0.29415000000006E+03 0.29415000000189E+03 0.29467413140391E+03 0.29659119377386E+03 0.29415000000006E+03
 0.29415000000189E+03 0.29625233477798E+03 0.29415000002487E+03 0.61434016414772E+00 -.97376822029326E+01
 0.26367558873270E+02 0.89813731268024E+02 0.63314334600388E+02 0.26727069640000E+02 0.21232079170954E+02
 0.27824075320370E+02 0.68483748257659E+02 0.26375012905325E+02 0.21108091170273E+02 0.27485347302955E+02
 0.68364850785444E+02 0.26727069639999E+02 0.21232079170957E+02 0.27824075320368E+02 0.68483748257666E+02
 0.26375012905325E+02 0.21108091170272E+02 0.27485347302956E+02 0.68364850785442E+02 0.35861875364612E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31766012930500E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.44272733330391E-02 0.19238772917184E-01 0.00000000000000E+00 0.44272733330391E-02 0.19238772917184E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25794027052568E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.37014530333035E-03 0.25794027052568E-01 0.37014530333035E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15658932979984E-02 0.64467970885724E-01 0.37417367321520E-01
 0.00000000000000E+00 0.64467970885724E-01 0.38983260619518E-01 0.42599458003854E+00 0.10099457705831E+00
 0.32500000298023E+00 0.42250006586313E+00
    370.79467491
 0.30974564961631E+00 0.31122694526154E+03 0.45190592757012E+03 0.43738102664927E+03 0.43175596096080E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13595894291188E+00 0.00000000000000E+00 -.10575606218034E+02
 0.11369112230126E-02 0.12851104892619E+01 0.70366092251262E+04 0.26387284594223E+04 0.62251456717895E+01
 0.23344296269211E+01 0.31905308065956E+03 0.29415000017948E+03 0.31581708779506E+03 0.34634935510430E+03
 0.29415000000997E+03 0.29415000001990E+03 0.31442532815696E+03 0.34622587551396E+03 0.29415000000775E+03
 0.29415000001981E+03 0.31581708779506E+03 0.34634935510430E+03 0.29415000000997E+03 0.29415000001990E+03
 0.31442532815696E+03 0.34622587551396E+03 0.29415000000775E+03 0.29415000001981E+03 0.39270721253380E+03
 0.29937251109919E+03 0.15867366781423E+04 0.15595828766199E+04 0.85472129851620E+03 0.14528107023247E+04
 0.59381579731595E+03 0.92975658501465E+03 0.13455399179653E+04 0.91689372142758E+03 0.21491207332441E+04
 0.88649704168439E+03 0.13432208606761E+04 0.87840356616146E+03 0.21479733151874E+04 0.92975658501464E+03
 0.13455399179653E+04 0.91689372142757E+03 0.21491207332441E+04 0.88649704168439E+03 0.13432208606761E+04
 0.87840356616147E+03 0.21479733151873E+04 0.19349107872779E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60285492574932E+05 0.50050000000000E+08 0.50556209546216E+03 0.12450099128002E+01
 0.12450099128002E+01 0.13034787644772E+01 0.47563678991359E+00 0.29548685158815E+03 0.30604809736766E+03
 0.30269921934096E+03 0.30261839453490E+03 0.23000000000000E+00 0.00000000000000E+00 0.21527501702212E+00
 0.00000000000000E+00 -.69843650545202E-01 0.99512710585537E-03 0.28402666589267E+00 0.80000000000000E+04
 0.30000000000000E+04 0.28166369431746E+02 0.10562388536905E+02 0.29937291846760E+03 0.39273472516873E+03
 0.29469904669937E+03 0.29660176074763E+03 0.29415000000008E+03 0.29415000000255E+03 0.29469224703411E+03
 0.29660076511339E+03 0.29415000000008E+03 0.29415000000255E+03 0.29469904669937E+03 0.29660176074763E+03
 0.29415000000008E+03 0.29415000000255E+03 0.29469224703411E+03 0.29660076511339E+03 0.29415000000008E+03
 0.29415000000255E+03 0.29626235688350E+03 0.29415000003260E+03 -.72500352399443E+00 -.11821123517186E+02
 0.27173519185465E+02 0.90153533223173E+02 0.62844146441781E+02 0.27530139224741E+02 0.21838225016622E+02
 0.28819825244894E+02 0.68707813348552E+02 0.27163885964315E+02 0.21704891841119E+02 0.28467878088750E+02
 0.68580388510633E+02 0.27530139224739E+02 0.21838225016626E+02 0.28819825244891E+02 0.68707813348560E+02
 0.27163885964315E+02 0.21704891841118E+02 0.28467878088751E+02 0.68580388510631E+02 0.35593849143929E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31768832162515E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43894005648555E-02 0.19327889536852E-01 0.00000000000000E+00 0.43894005648555E-02 0.19327889536852E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.25887915810027E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.35434031085308E-03 0.25887915810027E-01 0.35434031085308E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16004207953326E-02 0.64649414737329E-01 0.37270870497614E-01
 0.00000000000000E+00 0.64649414737329E-01 0.38871291292947E-01 0.42531839495679E+00 0.10031839197656E+00
 0.32500000298023E+00 0.42250006586313E+00
    382.48177251
 0.30977871027825E+00 0.31179727592870E+03 0.45246144582926E+03 0.43793652411788E+03 0.43232194221713E+03
 0.23000000000000E+00 0.00000000000000E+00 0.13258146649507E+00 0.00000000000000E+00 -.10607995969686E+02
 0.11367896950725E-02 0.13290940994484E+01 0.70373614703552E+04 0.26390105513832E+04 0.60191373984133E+01
 0.22571765244050E+01 0.31967441403122E+03 0.29415000026435E+03 0.31635593295096E+03 0.34745016156858E+03
 0.29415000001537E+03 0.29415000003081E+03 0.31495285181327E+03 0.34733128090563E+03 0.29415000001198E+03
 0.29415000003066E+03 0.31635593295096E+03 0.34745016156858E+03 0.29415000001537E+03 0.29415000003081E+03
 0.31495285181327E+03 0.34733128090564E+03 0.29415000001198E+03 0.29415000003066E+03 0.39410137929446E+03
 0.29985504400438E+03 0.15961722422812E+04 0.15688179001034E+04 0.85307708832507E+03 0.14408068054979E+04
 0.58346433173116E+03 0.93576591913973E+03 0.13512629365843E+04 0.92302757392538E+03 0.21491989180141E+04
 0.89240218520037E+03 0.13492387271230E+04 0.88445420129288E+03 0.21483001844883E+04 0.93576591913972E+03
 0.13512629365843E+04 0.92302757392536E+03 0.21491989180141E+04 0.89240218520037E+03 0.13492387271230E+04
 0.88445420129289E+03 0.21483001844883E+04 0.19347115645072E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60490002111882E+05 0.50050000000000E+08 0.50611752530592E+03 0.12448452318928E+01
 0.12448452318928E+01 0.13502271548532E+01 0.47366007372017E+00 0.29566765285597E+03 0.30598467021490E+03
 0.30272683074637E+03 0.30264989629635E+03 0.23000000000000E+00 0.00000000000000E+00 0.21468278234391E+00
 0.00000000000000E+00 -.73016799249622E-01 0.99451854080970E-03 0.29641917727030E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26988807113194E+02 0.10120802667448E+02 0.29985555457487E+03 0.39412719781369E+03
 0.29472820799777E+03 0.29661881031567E+03 0.29415000000013E+03 0.29415000000392E+03 0.29472104268261E+03
 0.29661759817315E+03 0.29415000000013E+03 0.29415000000393E+03 0.29472820799778E+03 0.29661881031567E+03
 0.29415000000013E+03 0.29415000000392E+03 0.29472104268261E+03 0.29661759817315E+03 0.29415000000013E+03
 0.29415000000393E+03 0.29627950981339E+03 0.29415000004811E+03 -.27994679491189E+01 -.15051836193647E+02
 0.28426788876206E+02 0.90862720236246E+02 0.62293797415659E+02 0.28793779981104E+02 0.22787629434527E+02
 0.30411133252525E+02 0.69202949786362E+02 0.28405092578313E+02 0.22640230120350E+02 0.30038312269101E+02
 0.69062733166821E+02 0.28793779981102E+02 0.22787629434531E+02 0.30411133252521E+02 0.69202949786370E+02
 0.28405092578313E+02 0.22640230120349E+02 0.30038312269101E+02 0.69062733166818E+02 0.35255030143945E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31775264781897E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.43262196245280E-02 0.19474406747070E-01 0.00000000000000E+00 0.43262196245280E-02 0.19474406747070E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26037002677775E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.33029284428914E-03 0.26037002677775E-01 0.33029284428914E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16406170606055E-02 0.64958793409527E-01 0.37114333577548E-01
 0.00000000000000E+00 0.64958793409527E-01 0.38754950638154E-01 0.42433003686009E+00 0.99330033879854E-01
 0.32500000298023E+00 0.42250006586313E+00
    391.71088968
 0.30981143122630E+00 0.31223934004033E+03 0.45290016471601E+03 0.43837405424593E+03 0.43276683111447E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12992649054234E+00 0.00000000000000E+00 -.10633822730613E+02
 0.11366694815753E-02 0.13635881232117E+01 0.70381057375739E+04 0.26392896515902E+04 0.58668742150358E+01
 0.22000778306384E+01 0.32015880510647E+03 0.29415000035563E+03 0.31677658939214E+03 0.34830318542390E+03
 0.29415000002142E+03 0.29415000004308E+03 0.31536426382493E+03 0.34818775467953E+03 0.29415000001673E+03
 0.29415000004288E+03 0.31677658939214E+03 0.34830318542390E+03 0.29415000002142E+03 0.29415000004308E+03
 0.31536426382493E+03 0.34818775467954E+03 0.29415000001673E+03 0.29415000004288E+03 0.39517505206479E+03
 0.30025275354003E+03 0.16035545621631E+04 0.15760318707887E+04 0.85164527506340E+03 0.14315510373487E+04
 0.57564753590994E+03 0.94047790742798E+03 0.13555437851093E+04 0.92782507208214E+03 0.21491873527352E+04
 0.89700960024659E+03 0.13537303744267E+04 0.88916572939498E+03 0.21484648723582E+04 0.94047790742797E+03
 0.13555437851093E+04 0.92782507208213E+03 0.21491873527352E+04 0.89700960024660E+03 0.13537303744267E+04
 0.88916572939499E+03 0.21484648723581E+04 0.19345402531719E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60692410216873E+05 0.50050000000000E+08 0.50655608135676E+03 0.12446810577411E+01
 0.12446810577411E+01 0.13871436235392E+01 0.47215737777329E+00 0.29582183103295E+03 0.30595167649865E+03
 0.30276308898376E+03 0.30268906879227E+03 0.23000000000000E+00 0.00000000000000E+00 0.21419819283048E+00
 0.00000000000000E+00 -.75789775729848E-01 0.99400017531797E-03 0.30651862383084E+00 0.80000000000000E+04
 0.30000000000000E+04 0.26099556040076E+02 0.97873335150286E+01 0.30025334290635E+03 0.39519957668690E+03
 0.29475239844909E+03 0.29663425487515E+03 0.29415000000018E+03 0.29415000000546E+03 0.29474492897154E+03
 0.29663286732619E+03 0.29415000000018E+03 0.29415000000546E+03 0.29475239844909E+03 0.29663425487515E+03
 0.29415000000018E+03 0.29415000000546E+03 0.29474492897154E+03 0.29663286732619E+03 0.29415000000018E+03
 0.29415000000546E+03 0.29629473068959E+03 0.29415000006478E+03 -.45059344959682E+01 -.17710910843756E+02
 0.29449613521632E+02 0.91570834110083E+02 0.61973972520843E+02 0.29839717099795E+02 0.23567924304482E+02
 0.31750586533786E+02 0.69712503855511E+02 0.29432159047862E+02 0.23409408121819E+02 0.31360204850173E+02
 0.69562202851402E+02 0.29839717099793E+02 0.23567924304487E+02 0.31750586533784E+02 0.69712503855520E+02
 0.29432159047863E+02 0.23409408121816E+02 0.31360204850173E+02 0.69562202851397E+02 0.35028228283378E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31781660052603E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42656654858982E-02 0.19611228142432E-01 0.00000000000000E+00 0.42656654858982E-02 0.19611228142432E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26149273085376E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.31277204094560E-03 0.26149273085376E-01 0.31277204094560E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16643335143386E-02 0.65219901096611E-01 0.37048090657385E-01
 0.00000000000000E+00 0.65219901096611E-01 0.38712424171724E-01 0.42357868888664E+00 0.98578685906411E-01
 0.32500000298023E+00 0.42250006586313E+00
    402.37772562
 0.30985573739265E+00 0.31274129137220E+03 0.45340575996012E+03 0.43887719574704E+03 0.43327761621042E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12687168758699E+00 0.00000000000000E+00 -.10663268559898E+02
 0.11365067774968E-02 0.14031932712053E+01 0.70391133237416E+04 0.26396674964031E+04 0.57012816154174E+01
 0.21379806057815E+01 0.32071386091978E+03 0.29415000049567E+03 0.31725936147317E+03 0.34927247693971E+03
 0.29415000003109E+03 0.29415000006274E+03 0.31583599822370E+03 0.34916089885266E+03 0.29415000002432E+03
 0.29415000006246E+03 0.31725936147317E+03 0.34927247693971E+03 0.29415000003109E+03 0.29415000006274E+03
 0.31583599822370E+03 0.34916089885266E+03 0.29415000002432E+03 0.29415000006246E+03 0.39638067611415E+03
 0.30072746295640E+03 0.16119879974974E+04 0.15842508123172E+04 0.84993404463303E+03 0.14212323320524E+04
 0.56704861719620E+03 0.94587395463144E+03 0.13602322446208E+04 0.93329965728446E+03 0.21490704976980E+04
 0.90226378786790E+03 0.13586400335362E+04 0.89452294403545E+03 0.21485307501307E+04 0.94587395463143E+03
 0.13602322446208E+04 0.93329965728445E+03 0.21490704976980E+04 0.90226378786790E+03 0.13586400335362E+04
 0.89452294403545E+03 0.21485307501307E+04 0.19343381643442E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.60966483179419E+05 0.50050000000000E+08 0.50706140305733E+03 0.12444578578040E+01
 0.12444578578040E+01 0.14298109673272E+01 0.47049792585580E+00 0.29601246572715E+03 0.30593035923281E+03
 0.30281946035063E+03 0.30274866724494E+03 0.23000000000000E+00 0.00000000000000E+00 0.21361967316789E+00
 0.00000000000000E+00 -.79218873628853E-01 0.99335998625081E-03 0.31853332553919E+00 0.80000000000000E+04
 0.30000000000000E+04 0.25115111539611E+02 0.94181668273540E+01 0.30072814328949E+03 0.39640374750137E+03
 0.29478207698330E+03 0.29665428094435E+03 0.29415000000027E+03 0.29415000000790E+03 0.29477423349476E+03
 0.29665268295957E+03 0.29415000000027E+03 0.29415000000790E+03 0.29478207698330E+03 0.29665428094435E+03
 0.29415000000027E+03 0.29415000000790E+03 0.29477423349476E+03 0.29665268295957E+03 0.29415000000027E+03
 0.29415000000790E+03 0.29631417884194E+03 0.29415000009032E+03 -.65344014785000E+01 -.20871335162334E+02
 0.30668159724741E+02 0.92542831318041E+02 0.61721330794677E+02 0.31103328095822E+02 0.24502546284429E+02
 0.33392210654148E+02 0.70423912452162E+02 0.30673156663288E+02 0.24331147493366E+02 0.32980829377767E+02
 0.70261969235313E+02 0.31103328095820E+02 0.24502546284432E+02 0.33392210654145E+02 0.70423912452168E+02
 0.30673156663289E+02 0.24331147493365E+02 0.32980829377768E+02 0.70261969235310E+02 0.34814843042450E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31791365321709E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.42004278295440E-02 0.19761181922635E-01 0.00000000000000E+00 0.42004278295440E-02 0.19761181922635E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26307969037088E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.28964870095281E-03 0.26307969037088E-01 0.28964870095281E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16804024932877E-02 0.65568678770686E-01 0.36998270727049E-01
 0.00000000000000E+00 0.65568678770686E-01 0.38678673220336E-01 0.42274896292790E+00 0.97748959947667E-01
 0.32500000298023E+00 0.42250006586313E+00
    413.04456157
 0.30990348796112E+00 0.31323444678567E+03 0.45391139630733E+03 0.43937930386317E+03 0.43378645497532E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12383193977203E+00 0.00000000000000E+00 -.10692807173367E+02
 0.11363314916846E-02 0.14425094860957E+01 0.70401991483489E+04 0.26400746806308E+04 0.55458907390985E+01
 0.20797090271619E+01 0.32126416172019E+03 0.29415000068137E+03 0.31773875429268E+03 0.35022476882987E+03
 0.29415000004441E+03 0.29415000008995E+03 0.31630404050507E+03 0.35011689063715E+03 0.29415000003482E+03
 0.29415000008954E+03 0.31773875429268E+03 0.35022476882987E+03 0.29415000004441E+03 0.29415000008995E+03
 0.31630404050507E+03 0.35011689063715E+03 0.29415000003482E+03 0.29415000008954E+03 0.39755106122550E+03
 0.30121657055639E+03 0.16202978918566E+04 0.15923268950649E+04 0.84820665550811E+03 0.14113478405517E+04
 0.55890015176602E+03 0.95120124037640E+03 0.13647038774495E+04 0.93868580167851E+03 0.21489091418851E+04
 0.90743990199642E+03 0.13633107799809E+04 0.89978525571111E+03 0.21485316730506E+04 0.95120124037640E+03
 0.13647038774495E+04 0.93868580167852E+03 0.21489091418851E+04 0.90743990199643E+03 0.13633107799809E+04
 0.89978525571111E+03 0.21485316730506E+04 0.19341712674127E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.61261862826156E+05 0.50050000000000E+08 0.50756668917968E+03 0.12442163691360E+01
 0.12442163691360E+01 0.14724783111152E+01 0.46892756666753E+00 0.29621680437857E+03 0.30592610670869E+03
 0.30289080036489E+03 0.30282310619790E+03 0.23000000000000E+00 0.00000000000000E+00 0.21302106908259E+00
 0.00000000000000E+00 -.82925795338665E-01 0.99267469227174E-03 0.33092068382974E+00 0.80000000000000E+04
 0.30000000000000E+04 0.24174977240516E+02 0.90656164651934E+01 0.30121734980765E+03 0.39757270040375E+03
 0.29481366268938E+03 0.29667651924994E+03 0.29415000000040E+03 0.29415000001126E+03 0.29480542279467E+03
 0.29667470363035E+03 0.29415000000039E+03 0.29415000001126E+03 0.29481366268938E+03 0.29667651924994E+03
 0.29415000000040E+03 0.29415000001126E+03 0.29480542279467E+03 0.29667470363035E+03 0.29415000000039E+03
 0.29415000001126E+03 0.29633552393288E+03 0.29415000012412E+03 -.86172953566321E+01 -.24110304583003E+02
 0.31916536853977E+02 0.93659611231835E+02 0.61583491693588E+02 0.32415305807719E+02 0.25467020140992E+02
 0.35126222595023E+02 0.71253201296116E+02 0.31961605454450E+02 0.25282874801936E+02 0.34693035629229E+02
 0.71079796763084E+02 0.32415305807717E+02 0.25467020140995E+02 0.35126222595020E+02 0.71253201296121E+02
 0.31961605454451E+02 0.25282874801935E+02 0.34693035629230E+02 0.71079796763081E+02 0.34649052666396E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31802767737204E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.41288897841034E-02 0.19924466430439E-01 0.00000000000000E+00 0.41288897841034E-02 0.19924466430439E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26473642795482E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.26686249888229E-03 0.26473642795482E-01 0.26686249888229E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16867892749761E-02 0.65944019807580E-01 0.36995231898188E-01
 0.00000000000000E+00 0.65944019807580E-01 0.38682021173164E-01 0.42196378333377E+00 0.96963780353533E-01
 0.32500000298023E+00 0.42250006586313E+00
    423.71139752
 0.30995668766121E+00 0.31371923751533E+03 0.45441610097737E+03 0.43987945638970E+03 0.43429251533892E+03
 0.23000000000000E+00 0.00000000000000E+00 0.12080753552575E+00 0.00000000000000E+00 -.10722364696987E+02
 0.11361362877055E-02 0.14815372026623E+01 0.70414087522517E+04 0.26405282820944E+04 0.53997969039346E+01
 0.20249238389755E+01 0.32180982805612E+03 0.29415000092471E+03 0.31821479467984E+03 0.35116130735604E+03
 0.29415000006253E+03 0.29415000012710E+03 0.31676844725343E+03 0.35105697072696E+03 0.29415000004914E+03
 0.29415000012653E+03 0.31821479467984E+03 0.35116130735604E+03 0.29415000006253E+03 0.29415000012710E+03
 0.31676844725343E+03 0.35105697072696E+03 0.29415000004914E+03 0.29415000012653E+03 0.39869040933571E+03
 0.30171953996576E+03 0.16285322444515E+04 0.16003095080831E+04 0.84643776025414E+03 0.14018073253017E+04
 0.55113737624628E+03 0.95649065761381E+03 0.13689665878453E+04 0.94401595960741E+03 0.21486901759239E+04
 0.91256051789572E+03 0.13677529808031E+04 0.90497660252989E+03 0.21484570282319E+04 0.95649065761381E+03
 0.13689665878453E+04 0.94401595960741E+03 0.21486901759239E+04 0.91256051789572E+03 0.13677529808031E+04
 0.90497660252989E+03 0.21484570282319E+04 0.19340245646308E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.61590950188303E+05 0.50050000000000E+08 0.50807096602792E+03 0.12439466037421E+01
 0.12439466037421E+01 0.15151456549032E+01 0.46745543738499E+00 0.29643460327371E+03 0.30593731946760E+03
 0.30297592189776E+03 0.30291122014816E+03 0.23000000000000E+00 0.00000000000000E+00 0.21240243690407E+00
 0.00000000000000E+00 -.86893186523987E-01 0.99194529737838E-03 0.34367984539837E+00 0.80000000000000E+04
 0.30000000000000E+04 0.23277477882728E+02 0.87290542060232E+01 0.30172042001528E+03 0.39871066525539E+03
 0.29484713063120E+03 0.29670089317957E+03 0.29415000000057E+03 0.29415000001581E+03 0.29483847261556E+03
 0.29669885399611E+03 0.29415000000056E+03 0.29415000001581E+03 0.29484713063120E+03 0.29670089317957E+03
 0.29415000000057E+03 0.29415000001581E+03 0.29483847261556E+03 0.29669885399611E+03 0.29415000000056E+03
 0.29415000001581E+03 0.29635868908013E+03 0.29415000016830E+03 -.10754469467984E+02 -.27425546250632E+02
 0.33191337732111E+02 0.94906540781155E+02 0.61549246360383E+02 0.33774099495635E+02 0.26458682340331E+02
 0.36952303977986E+02 0.72189020966140E+02 0.33295980117154E+02 0.26261958247771E+02 0.36496532786576E+02
 0.72004361220878E+02 0.33774099495634E+02 0.26458682340334E+02 0.36952303977983E+02 0.72189020966145E+02
 0.33295980117154E+02 0.26261958247769E+02 0.36496532786576E+02 0.72004361220875E+02 0.34525528215823E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31815761364778E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.40520148944982E-02 0.20099485439994E-01 0.00000000000000E+00 0.40520148944982E-02 0.20099485439994E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26647541733369E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.24439894406154E-03 0.26647541733369E-01 0.24439894406154E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16838598315199E-02 0.66344294629281E-01 0.37032593110774E-01
 0.00000000000000E+00 0.66344294629281E-01 0.38716452942294E-01 0.42122771869249E+00 0.96227715712263E-01
 0.32500000298023E+00 0.42250006586313E+00
    434.37823346
 0.31001564051590E+00 0.31419600307577E+03 0.45491887655337E+03 0.44037677930124E+03 0.43479503165048E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11779887532053E+00 0.00000000000000E+00 -.10751875244017E+02
 0.11359200723264E-02 0.15202758366367E+01 0.70427490409740E+04 0.26410308903652E+04 0.52622029550232E+01
 0.19733261081337E+01 0.32235117297108E+03 0.29415000124010E+03 0.31868771803011E+03 0.35208290547065E+03
 0.29415000008688E+03 0.29415000017720E+03 0.31722944451099E+03 0.35198195479332E+03 0.29415000006842E+03
 0.29415000017641E+03 0.31868771803012E+03 0.35208290547065E+03 0.29415000008688E+03 0.29415000017720E+03
 0.31722944451100E+03 0.35198195479331E+03 0.29415000006842E+03 0.29415000017641E+03 0.39980069078935E+03
 0.30223547971995E+03 0.16367007950308E+04 0.16082084558193E+04 0.84462420122075E+03 0.13925698350486E+04
 0.54372251282175E+03 0.96174862407147E+03 0.13730270422233E+04 0.94929700265704E+03 0.21484032875792E+04
 0.91763089267804E+03 0.13719752814658E+04 0.91010273188553E+03 0.21482984402995E+04 0.96174862407140E+03
 0.13730270422233E+04 0.94929700265693E+03 0.21484032875792E+04 0.91763089267803E+03 0.13719752814658E+04
 0.91010273188550E+03 0.21482984402995E+04 0.19338857918024E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.61955625917854E+05 0.50050000000000E+08 0.50857324468969E+03 0.12436471232239E+01
 0.12436471232239E+01 0.15578129986911E+01 0.46608809887234E+00 0.29666549962929E+03 0.30596266839034E+03
 0.30307380191518E+03 0.30301200101447E+03 0.23000000000000E+00 0.00000000000000E+00 0.21176381208864E+00
 0.00000000000000E+00 -.91104298796560E-01 0.99117321020836E-03 0.35681019209160E+00 0.80000000000000E+04
 0.30000000000000E+04 0.22420884204861E+02 0.84078315768228E+01 0.30223645996912E+03 0.39981962189960E+03
 0.29488250905884E+03 0.29672732564174E+03 0.29415000000082E+03 0.29415000002190E+03 0.29487341140963E+03
 0.29672505776303E+03 0.29415000000081E+03 0.29415000002190E+03 0.29488250905884E+03 0.29672732564174E+03
 0.29415000000082E+03 0.29415000002190E+03 0.29487341140963E+03 0.29672505776303E+03 0.29415000000081E+03
 0.29415000002190E+03 0.29638360284786E+03 0.29415000022538E+03 -.12943495292421E+02 -.30811504338115E+02
 0.34488855912398E+02 0.96270717200181E+02 0.61609417008221E+02 0.35177189550187E+02 0.27474854766993E+02
 0.38868571074480E+02 0.73221609306327E+02 0.34673805935628E+02 0.27265765493947E+02 0.38389484432087E+02
 0.73025942482241E+02 0.35177189550185E+02 0.27474854766996E+02 0.38868571074478E+02 0.73221609306333E+02
 0.34673805935630E+02 0.27265765493945E+02 0.38389484432089E+02 0.73025942482238E+02 0.34440337515566E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31830258341642E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.39706492368068E-02 0.20284838100453E-01 0.00000000000000E+00 0.39706492368068E-02 0.20284838100453E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.26830793647140E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.22228618111847E-03 0.26830793647140E-01 0.22228618111847E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16719390330708E-02 0.66768055231625E-01 0.37105140096060E-01
 0.00000000000000E+00 0.66768055231625E-01 0.38777079129131E-01 0.42054404943617E+00 0.95544046455940E-01
 0.32500000298023E+00 0.42250006586313E+00
    445.04506941
 0.31007997967480E+00 0.31466511954775E+03 0.45541916210950E+03 0.44087082522393E+03 0.43529364395392E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11480636699300E+00 0.00000000000000E+00 -.10781309639283E+02
 0.11356842132303E-02 0.15587235455137E+01 0.70442116803269E+04 0.26415793801226E+04 0.51324046672839E+01
 0.19246517502315E+01 0.32288844752845E+03 0.29415000164472E+03 0.31915771127961E+03 0.35299021999342E+03
 0.29415000011922E+03 0.29415000024396E+03 0.31768722544789E+03 0.35289250325801E+03 0.29415000009410E+03
 0.29415000024288E+03 0.31915771127961E+03 0.35299021999341E+03 0.29415000011922E+03 0.29415000024396E+03
 0.31768722544790E+03 0.35289250325800E+03 0.29415000009410E+03 0.29415000024288E+03 0.40088356492108E+03
 0.30276346550471E+03 0.16448048328198E+04 0.16160255461915E+04 0.84277025339216E+03 0.13836109902837E+04
 0.53662688562463E+03 0.96697558387277E+03 0.13768975919458E+04 0.95453010802125E+03 0.21480491677538E+04
 0.92265284969391E+03 0.13759917530661E+04 0.91516615200853E+03 0.21480582715666E+04 0.96697558387273E+03
 0.13768975919459E+04 0.95453010802119E+03 0.21480491677538E+04 0.92265284969390E+03 0.13759917530661E+04
 0.91516615200852E+03 0.21480582715666E+04 0.19337504773575E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.62353620719550E+05 0.50050000000000E+08 0.50907297374654E+03 0.12433198273434E+01
 0.12433198273434E+01 0.16004803424791E+01 0.46482960510061E+00 0.29690909600699E+03 0.30600110865070E+03
 0.30318361755287E+03 0.30312463576961E+03 0.23000000000000E+00 0.00000000000000E+00 0.21110517020526E+00
 0.00000000000000E+00 -.95545474254268E-01 0.99035995834182E-03 0.37031208214438E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21603399904410E+02 0.81012749641539E+01 0.30276454528376E+03 0.40090122883359E+03
 0.29491983163799E+03 0.29675574816599E+03 0.29415000000115E+03 0.29415000002995E+03 0.29491027311795E+03
 0.29675324724301E+03 0.29415000000113E+03 0.29415000002996E+03 0.29491983163799E+03 0.29675574816599E+03
 0.29415000000115E+03 0.29415000002995E+03 0.29491027311794E+03 0.29675324724301E+03 0.29415000000113E+03
 0.29415000002996E+03 0.29641020195783E+03 0.29415000029835E+03 -.15181259707234E+02 -.34261880628599E+02
 0.35805878271198E+02 0.97741761968579E+02 0.61756854306025E+02 0.36622207220061E+02 0.28513343162262E+02
 0.40872992270224E+02 0.74343242622667E+02 0.36092756167231E+02 0.28292147507972E+02 0.40369902937049E+02
 0.74136858395285E+02 0.36622207220058E+02 0.28513343162266E+02 0.40872992270221E+02 0.74343242622676E+02
 0.36092756167232E+02 0.28292147507970E+02 0.40369902937050E+02 0.74136858395281E+02 0.34390752164948E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31846177665291E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.38852347496951E-02 0.20479919156227E-01 0.00000000000000E+00 0.38852347496951E-02 0.20479919156227E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27023849518215E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.20064087022604E-03 0.27023849518215E-01 0.20064087022604E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16513760065814E-02 0.67213533751734E-01 0.37209367249719E-01
 0.00000000000000E+00 0.67213533751734E-01 0.38860743256301E-01 0.41991480255030E+00 0.94914799570070E-01
 0.32500000298023E+00 0.42250006586313E+00
    450.37848739
 0.31011414549656E+00 0.31489691439779E+03 0.45566820325514E+03 0.44111648060366E+03 0.43554137142407E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11331629997692E+00 0.00000000000000E+00 -.10795990768890E+02
 0.11355590113872E-02 0.15778377136252E+01 0.70449883447513E+04 0.26418706292817E+04 0.50702299298066E+01
 0.19013362236775E+01 0.32315563123395E+03 0.29415000188671E+03 0.31939166333776E+03 0.35343872062323E+03
 0.29415000013904E+03 0.29415000028499E+03 0.31791496634997E+03 0.35334256538502E+03 0.29415000010986E+03
 0.29415000028373E+03 0.31939166333776E+03 0.35343872062323E+03 0.29415000013904E+03 0.29415000028499E+03
 0.31791496634997E+03 0.35334256538502E+03 0.29415000010986E+03 0.29415000028373E+03 0.40141521733093E+03
 0.30303169436566E+03 0.16488343291861E+04 0.16199051932704E+04 0.84182886003898E+03 0.13792283071062E+04
 0.53319030276705E+03 0.96957843491453E+03 0.13787648023001E+04 0.95712987735577E+03 0.21478465381321E+04
 0.92514673785087E+03 0.13779264481959E+04 0.91767559263782E+03 0.21479077133780E+04 0.96957843491451E+03
 0.13787648023001E+04 0.95712987735575E+03 0.21478465381321E+04 0.92514673785087E+03 0.13779264481959E+04
 0.91767559263781E+03 0.21479077133780E+04 0.19336826848236E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.62564966650806E+05 0.50050000000000E+08 0.50932171667156E+03 0.12431458814664E+01
 0.12431458814664E+01 0.16218140143731E+01 0.46424205112159E+00 0.29703550730020E+03 0.30602495063170E+03
 0.30324276555793E+03 0.30318516475013E+03 0.23000000000000E+00 0.00000000000000E+00 0.21076833104183E+00
 0.00000000000000E+00 -.97847783763283E-01 0.98993845744579E-03 0.37720251993834E+00 0.80000000000000E+04
 0.30000000000000E+04 0.21208766053068E+02 0.79532872699004E+01 0.30303282371462E+03 0.40143226882175E+03
 0.29493923126053E+03 0.29677068651790E+03 0.29415000000136E+03 0.29415000003487E+03 0.29492943440904E+03
 0.29676806768198E+03 0.29415000000134E+03 0.29415000003488E+03 0.29493923126053E+03 0.29677068651790E+03
 0.29415000000136E+03 0.29415000003487E+03 0.29492943440904E+03 0.29676806768198E+03 0.29415000000134E+03
 0.29415000003488E+03 0.29642411532250E+03 0.29415000034187E+03 -.16317424569175E+02 -.36009361081793E+02
 0.36470779597389E+02 0.98514468013898E+02 0.61861334518522E+02 0.37359738467133E+02 0.29040332419278E+02
 0.41907546019404E+02 0.74935306078721E+02 0.36816964741520E+02 0.28813194090451E+02 0.41392210411511E+02
 0.74723684103136E+02 0.37359738467130E+02 0.29040332419281E+02 0.41907546019399E+02 0.74935306078727E+02
 0.36816964741520E+02 0.28813194090448E+02 0.41392210411511E+02 0.74723684103130E+02 0.34378568104304E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31854644638525E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.38411027688709E-02 0.20581006201647E-01 0.00000000000000E+00 0.38411027688709E-02 0.20581006201647E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27124065252633E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.19004293749132E-03 0.27124065252633E-01 0.19004293749132E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16379494169227E-02 0.67444039097498E-01 0.37272286452588E-01
 0.00000000000000E+00 0.67444039097498E-01 0.38910235869511E-01 0.41962102556079E+00 0.94621022580561E-01
 0.32500000298023E+00 0.42250006586313E+00
    461.04532333
 0.31018636219229E+00 0.31535517100363E+03 0.45616379575654E+03 0.44160482406408E+03 0.43603348022877E+03
 0.23000000000000E+00 0.00000000000000E+00 0.11034881434134E+00 0.00000000000000E+00 -.10825263935249E+02
 0.11352944724146E-02 0.16158452391601E+01 0.70466299223541E+04 0.26424862208828E+04 0.49509691931626E+01
 0.18566134474360E+01 0.32368722979962E+03 0.29415000246436E+03 0.31985758318234E+03 0.35432576393064E+03
 0.29415000018754E+03 0.29415000038567E+03 0.31836825624597E+03 0.35423262421756E+03 0.29415000014852E+03
 0.29415000038397E+03 0.31985758318234E+03 0.35432576393064E+03 0.29415000018754E+03 0.29415000038567E+03
 0.31836825624597E+03 0.35423262421756E+03 0.29415000014852E+03 0.29415000038397E+03 0.40245981618348E+03
 0.30357607028363E+03 0.16568501035465E+04 0.16276087305341E+04 0.83991926255082E+03 0.13706436730928E+04
 0.52652481422927E+03 0.97476390222611E+03 0.13823691114472E+04 0.96229720620664E+03 0.21473900171426E+04
 0.93010175232907E+03 0.13816556718562E+04 0.92265166922767E+03 0.21475463838984E+04 0.97476390222609E+03
 0.13823691114472E+04 0.96229720620661E+03 0.21473900171427E+04 0.93010175232907E+03 0.13816556718562E+04
 0.92265166922767E+03 0.21475463838984E+04 0.19335445089823E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.63011691009437E+05 0.50050000000000E+08 0.50981667746089E+03 0.12427779558609E+01
 0.12427779558609E+01 0.16644813581611E+01 0.46315132258814E+00 0.29729722163597E+03 0.30608138480007E+03
 0.30336912027521E+03 0.30331422669339E+03 0.23000000000000E+00 0.00000000000000E+00 0.21007957775229E+00
 0.00000000000000E+00 -.10260735280097E+00 0.98906694715710E-03 0.39126293060312E+00 0.80000000000000E+04
 0.30000000000000E+04 0.20446608595576E+02 0.76674782233410E+01 0.30357729844926E+03 0.40247568432464E+03
 0.29497952394932E+03 0.29680198193417E+03 0.29415000000188E+03 0.29415000004687E+03 0.29496923477508E+03
 0.29679912496287E+03 0.29415000000186E+03 0.29415000004689E+03 0.29497952394932E+03 0.29680198193417E+03
 0.29415000000188E+03 0.29415000004687E+03 0.29496923477508E+03 0.29679912496287E+03 0.29415000000186E+03
 0.29415000004689E+03 0.29645313565816E+03 0.29415000044541E+03 -.18622229293055E+02 -.39545253650975E+02
 0.37811691106633E+02 0.10012931134019E+03 0.62128561778024E+02 0.38863515879986E+02 0.30108704796666E+02
 0.44039770620120E+02 0.76178302205014E+02 0.38293541631619E+02 0.29869927910426E+02 0.43499476224948E+02
 0.75956470154254E+02 0.38863515879983E+02 0.30108704796668E+02 0.44039770620115E+02 0.76178302205019E+02
 0.38293541631619E+02 0.29869927910423E+02 0.43499476224948E+02 0.75956470154249E+02 0.34378244278427E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31872558673121E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.37502783401581E-02 0.20789863274433E-01 0.00000000000000E+00 0.37502783401581E-02 0.20789863274433E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27332257634587E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.16935937635200E-03 0.27332257634587E-01 0.16935937635200E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.16050245256407E-02 0.67919645507418E-01 0.37418120607661E-01
 0.00000000000000E+00 0.67919645507418E-01 0.39023145133302E-01 0.41907566129407E+00 0.94075658313836E-01
 0.32500000298023E+00 0.42250006586313E+00
    471.71215928
 0.31026359578409E+00 0.31580656003362E+03 0.45665576059179E+03 0.44208896744896E+03 0.43652093452890E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10739856367147E+00 0.00000000000000E+00 -.10854405599295E+02
 0.11350117055470E-02 0.16535562062764E+01 0.70483854579673E+04 0.26431445467378E+04 0.48380574967060E+01
 0.18142715612647E+01 0.32421530224980E+03 0.29415000318877E+03 0.32032098286458E+03 0.35519996100818E+03
 0.29415000025030E+03 0.29415000051642E+03 0.31881875297522E+03 0.35510969838731E+03 0.29415000019868E+03
 0.29415000051416E+03 0.32032098286459E+03 0.35519996100818E+03 0.29415000025030E+03 0.29415000051642E+03
 0.31881875297522E+03 0.35510969838731E+03 0.29415000019868E+03 0.29415000051416E+03 0.40348053360369E+03
 0.30413028593508E+03 0.16648102596616E+04 0.16352402296639E+04 0.83797691267653E+03 0.13622854228215E+04
 0.52011862558158E+03 0.97992345230389E+03 0.13858078611982E+04 0.96742308219724E+03 0.21468659623041E+04
 0.93501474950648E+03 0.13852070197317E+04 0.92757273986137E+03 0.21471066206743E+04 0.97992345230387E+03
 0.13858078611982E+04 0.96742308219721E+03 0.21468659623042E+04 0.93501474950648E+03 0.13852070197317E+04
 0.92757273986137E+03 0.21471066206743E+04 0.19334007519440E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.63489449322983E+05 0.50050000000000E+08 0.51030796932694E+03 0.12423841594083E+01
 0.12423841594083E+01 0.17071487019491E+01 0.46217358535768E+00 0.29757030782637E+03 0.30614891382491E+03
 0.30350571043044E+03 0.30345345270144E+03 0.23000000000000E+00 0.00000000000000E+00 0.20937066288296E+00
 0.00000000000000E+00 -.10756207603571E+00 0.98815920359278E-03 0.40569690918459E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19719154420178E+02 0.73946829075669E+01 0.30413161249686E+03 0.40349527257340E+03
 0.29502182714940E+03 0.29683512630226E+03 0.29415000000257E+03 0.29415000006234E+03 0.29501102495219E+03
 0.29683202874217E+03 0.29415000000254E+03 0.29415000006236E+03 0.29502182714940E+03 0.29683512630226E+03
 0.29415000000257E+03 0.29415000006234E+03 0.29501102495219E+03 0.29683202874217E+03 0.29415000000254E+03
 0.29415000006236E+03 0.29648370587687E+03 0.29415000057467E+03 -.20967490545623E+02 -.43131045511070E+02
 0.39165399507734E+02 0.10183092432348E+03 0.62469697818212E+02 0.40403908881523E+02 0.31194977225023E+02
 0.46253902644784E+02 0.77495557272802E+02 0.39806021835601E+02 0.30944923176470E+02 0.45688056720488E+02
 0.77263898357600E+02 0.40403908881520E+02 0.31194977225026E+02 0.46253902644779E+02 0.77495557272806E+02
 0.39806021835601E+02 0.30944923176467E+02 0.45688056720488E+02 0.77263898357595E+02 0.34408669327954E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31891727150015E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.36562510330074E-02 0.21007435418616E-01 0.00000000000000E+00 0.36562510330074E-02 0.21007435418616E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27550850625229E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.14948190741977E-03 0.27550850625229E-01 0.14948190741977E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15642842914947E-02 0.68413750534771E-01 0.37588595596163E-01
 0.00000000000000E+00 0.68413750534771E-01 0.39152879887658E-01 0.41858679267884E+00 0.93586789698608E-01
 0.32500000298023E+00 0.42250006586313E+00
    482.37899523
 0.31034571715936E+00 0.31625134945306E+03 0.45714378111691E+03 0.44256866020123E+03 0.43700353696352E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10446600036146E+00 0.00000000000000E+00 -.10883403631456E+02
 0.11347112101997E-02 0.16909678441103E+01 0.70502520183896E+04 0.26438445068961E+04 0.47310184092880E+01
 0.17741319034830E+01 0.32474003106987E+03 0.29415000408987E+03 0.32078200088702E+03 0.35606179170532E+03
 0.29415000033079E+03 0.29415000068467E+03 0.31926659939489E+03 0.35597427377881E+03 0.29415000026316E+03
 0.29415000068170E+03 0.32078200088702E+03 0.35606179170532E+03 0.29415000033079E+03 0.29415000068467E+03
 0.31926659939489E+03 0.35597427377881E+03 0.29415000026316E+03 0.29415000068170E+03 0.40447855574164E+03
 0.30469350898476E+03 0.16727180402894E+04 0.16428032529035E+04 0.83600506820572E+03 0.13541375148925E+04
 0.51395242134570E+03 0.98505891956471E+03 0.13890898895639E+04 0.97250980321103E+03 0.21462755307260E+04
 0.93988802485344E+03 0.13885905276310E+04 0.93244153055691E+03 0.21465907115293E+04 0.98505891956470E+03
 0.13890898895639E+04 0.97250980321101E+03 0.21462755307260E+04 0.93988802485344E+03 0.13885905276310E+04
 0.93244153055691E+03 0.21465907115293E+04 0.19332494862569E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.63997442915401E+05 0.50050000000000E+08 0.51079528175504E+03 0.12419651569364E+01
 0.12419651569364E+01 0.17498160457371E+01 0.46130862233389E+00 0.29785414564425E+03 0.30622693197728E+03
 0.30365197295836E+03 0.30360228014398E+03 0.23000000000000E+00 0.00000000000000E+00 0.20864150240270E+00
 0.00000000000000E+00 -.11269833429345E+00 0.98721748620773E-03 0.42050559430592E+00 0.80000000000000E+04
 0.30000000000000E+04 0.19024717169827E+02 0.71342689386851E+01 0.30469493344518E+03 0.40449221841847E+03
 0.29506615975921E+03 0.29687007358806E+03 0.29415000000348E+03 0.29415000008207E+03 0.29505482404973E+03
 0.29686673372970E+03 0.29415000000344E+03 0.29415000008210E+03 0.29506615975921E+03 0.29687007358806E+03
 0.29415000000348E+03 0.29415000008207E+03 0.29505482404973E+03 0.29686673372970E+03 0.29415000000344E+03
 0.29415000008210E+03 0.29651578022405E+03 0.29415000073467E+03 -.23349787905136E+02 -.46761431903085E+02
 0.40529715313667E+02 0.10361315193301E+03 0.62880788042778E+02 0.41979027469058E+02 0.32297771701522E+02
 0.48547062712679E+02 0.78882608722795E+02 0.41352546310096E+02 0.32036838096336E+02 0.47955104360067E+02
 0.78641538721930E+02 0.41979027469055E+02 0.32297771701525E+02 0.48547062712675E+02 0.78882608722800E+02
 0.41352546310096E+02 0.32036838096334E+02 0.47955104360067E+02 0.78641538721925E+02 0.34468507810612E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31912091802433E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.35592410491837E-02 0.21233559782442E-01 0.00000000000000E+00 0.35592410491837E-02 0.21233559782442E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.27779832768102E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.13055239045097E-03 0.27779832768102E-01 0.13055239045097E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.15160778493135E-02 0.68925301355742E-01 0.37781551822904E-01
 0.00000000000000E+00 0.68925301355742E-01 0.39297629672217E-01 0.41815431116695E+00 0.93154308186714E-01
 0.32500000298023E+00 0.42250006586313E+00
    493.04583117
 0.31043264652888E+00 0.31668978033462E+03 0.45762758634546E+03 0.44304368764015E+03 0.43748111897939E+03
 0.23000000000000E+00 0.00000000000000E+00 0.10155159217141E+00 0.00000000000000E+00 -.10912251237738E+02
 0.11343933062211E-02 0.17280771049318E+01 0.70522277909499E+04 0.26445854216062E+04 0.46294230605618E+01
 0.17360336477107E+01 0.32526158495023E+03 0.29415000520223E+03 0.32124076620471E+03 0.35691169456904E+03
 0.29415000043311E+03 0.29415000089931E+03 0.31971192732042E+03 0.35682679509462E+03 0.29415000034534E+03
 0.29415000089542E+03 0.32124076620471E+03 0.35691169456904E+03 0.29415000043311E+03 0.29415000089931E+03
 0.31971192732042E+03 0.35682679509462E+03 0.29415000034534E+03 0.29415000089542E+03 0.40545497012909E+03
 0.30526494097378E+03 0.16805770337327E+04 0.16503016436503E+04 0.83400662312436E+03 0.13461854226255E+04
 0.50800876638552E+03 0.99017239284216E+03 0.13922232255833E+04 0.97755984730460E+03 0.21456198938014E+04
 0.94472394612729E+03 0.13918153050567E+04 0.93726077360988E+03 0.21460008410884E+04 0.99017239284214E+03
 0.13922232255833E+04 0.97755984730457E+03 0.21456198938014E+04 0.94472394612729E+03 0.13918153050567E+04
 0.93726077360988E+03 0.21460008410884E+04 0.19330891927022E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64535178219568E+05 0.50050000000000E+08 0.51127834924263E+03 0.12415213565528E+01
 0.12415213565528E+01 0.17924833895250E+01 0.46055542659929E+00 0.29814807895664E+03 0.30631491384864E+03
 0.30380739376354E+03 0.30376019409192E+03 0.23000000000000E+00 0.00000000000000E+00 0.20789199718034E+00
 0.00000000000000E+00 -.11800250069385E+00 0.98624416445506E-03 0.43569033029447E+00 0.80000000000000E+04
 0.30000000000000E+04 0.18361665255672E+02 0.68856244708769E+01 0.30526646269391E+03 0.40546760803356E+03
 0.29511253588760E+03 0.29690678308255E+03 0.29415000000467E+03 0.29415000010704E+03 0.29510064638217E+03
 0.29690319994907E+03 0.29415000000461E+03 0.29415000010707E+03 0.29511253588760E+03 0.29690678308254E+03
 0.29415000000467E+03 0.29415000010704E+03 0.29510064638217E+03 0.29690319994907E+03 0.29415000000461E+03
 0.29415000010707E+03 0.29654931730544E+03 0.29415000093116E+03 -.25765705601971E+02 -.50431452234966E+02
 0.41902700549180E+02 0.10547063099717E+03 0.63358416945248E+02 0.43587108957020E+02 0.33415898570883E+02
 0.50916187563099E+02 0.80335594852558E+02 0.42931380529956E+02 0.33144516236368E+02 0.50297584661922E+02
 0.80085559111038E+02 0.43587108957017E+02 0.33415898570885E+02 0.50916187563094E+02 0.80335594852564E+02
 0.42931380529956E+02 0.33144516236365E+02 0.50297584661923E+02 0.80085559111032E+02 0.34556643886380E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31933592211248E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34593765317982E-02 0.21468274306458E-01 0.00000000000000E+00 0.34593765317982E-02 0.21468274306458E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28018890914263E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.11271554679978E-03 0.28018890914263E-01 0.11271554679978E-03 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14607612698087E-02 0.69453461168273E-01 0.37994986555073E-01
 0.00000000000000E+00 0.69453461168273E-01 0.39455747824882E-01 0.41777771329964E+00 0.92777710319412E-01
 0.32500000298023E+00 0.42250006586313E+00
    500.00000000
 0.31049136343157E+00 0.31697240907050E+03 0.45794100712381E+03 0.44335116305361E+03 0.43779007672085E+03
 0.23000000000000E+00 0.00000000000000E+00 0.99661573214154E-01 0.00000000000000E+00 -.10930562285677E+02
 0.11341786817257E-02 0.17521043082503E+01 0.70535623080372E+04 0.26450858655140E+04 0.45659382048943E+01
 0.17122268268354E+01 0.32560142707213E+03 0.29415000605252E+03 0.32154012612803E+03 0.35745938575485E+03
 0.29415000051307E+03 0.29415000106747E+03 0.32000232796412E+03 0.35737614549436E+03 0.29415000040969E+03
 0.29415000106287E+03 0.32154012612804E+03 0.35745938575485E+03 0.29415000051307E+03 0.29415000106747E+03
 0.32000232796412E+03 0.35737614549434E+03 0.29415000040969E+03 0.29415000106287E+03 0.40607521914823E+03
 0.30563872290455E+03 0.16856628773000E+04 0.16551376838393E+04 0.83276830405828E+03 0.13412422622151E+04
 0.50431011663651E+03 0.99348745459650E+03 0.13941981648528E+04 0.98082114573093E+03 0.21451715060953E+04
 0.94785252444169E+03 0.13938447400085E+04 0.94036809509244E+03 0.21455907106953E+04 0.99348745459644E+03
 0.13941981648528E+04 0.98082114573083E+03 0.21451715060953E+04 0.94785252444171E+03 0.13938447400087E+04
 0.94036809509247E+03 0.21455907106957E+04 0.19330153150535E+03 0.00000000000000E+00 0.40000000000000E-02
 0.20020000000000E+06 0.64898394376584E+05 0.50050000000000E+08 0.51159127587805E+03 0.12412213950676E+01
 0.12412213950676E+01 0.18203000648318E+01 0.46012309669714E+00 0.29834492392292E+03 0.30637750555076E+03
 0.30391352132871E+03 0.30386790786522E+03 0.23000000000000E+00 0.00000000000000E+00 0.20739226085482E+00
 0.00000000000000E+00 -.12150543640488E+00 0.98559341105943E-03 0.44579487333423E+00 0.80000000000000E+04
 0.30000000000000E+04 0.17945473307411E+02 0.67295524902790E+01 0.30564030909413E+03 0.40608721152870E+03
 0.29514454309566E+03 0.29693204234913E+03 0.29415000000561E+03 0.29415000012648E+03 0.29513227525695E+03
 0.29692829815745E+03 0.29415000000554E+03 0.29415000012651E+03 0.29514454309566E+03 0.29693204234913E+03
 0.29415000000561E+03 0.29415000012648E+03 0.29513227525695E+03 0.29692829815745E+03 0.29415000000554E+03
 0.29415000012651E+03 0.29657230062526E+03 0.29415000108074E+03 -.27335961153509E+02 -.52808405712166E+02
 0.42803139668244E+02 0.10672465238692E+03 0.63707497020337E+02 0.44654111009640E+02 0.34153042206934E+02
 0.52499977862255E+02 0.81319480570585E+02 0.43979411544899E+02 0.33875072420022E+02 0.51864184582596E+02
 0.81063845320112E+02 0.44654111009637E+02 0.34153042206935E+02 0.52499977862251E+02 0.81319480570587E+02
 0.43979411544900E+02 0.33875072420023E+02 0.51864184582598E+02 0.81063845320115E+02 0.34638969817605E+01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.31949174789464E+03 0.00000000000000E+00 0.00000000000000E+00 .00000000000000E+00 0.00000000000000E+00
 0.34031664633081E-02 0.21603689677497E-01 0.00000000000000E+00 0.34031664633081E-02 0.21603689677497E-01
 0.10000002384186E+00 0.00000000000000E+00 0.10000002384186E+00 0.52000010490417E-01 0.28213207364299E-01
 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00 0.00000000000000E+00
 0.00000000000000E+00 0.99804732763451E-04 0.28213207364299E-01 0.99804732763451E-04 0.20000004768372E+00
 0.20000004768372E+00 0.16000007629395E+00 0.14189923338440E-02 0.69831278859851E-01 0.38123189061901E-01
 0.00000000000000E+00 0.69831278859851E-01 0.39542181395745E-01 0.41756154834857E+00 0.92561545368338E-01
 0.32500000298023E+00 0.42250006586313E+00
