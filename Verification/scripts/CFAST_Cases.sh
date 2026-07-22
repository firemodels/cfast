#!/bin/bash

$RUNCFAST -d Energy_Balance sealed_test.in
$RUNCFAST -d Energy_Balance sealed_test_2_layers.in

$RUNCFAST -d Fires Ignition_Test.in

$RUNCFAST -d Mass_Balance species_mass_1.in
$RUNCFAST -d Mass_Balance species_mass_2.in
$RUNCFAST -d Mass_Balance species_mass_3.in
$RUNCFAST -d Mass_Balance species_mass_4.in

$RUNCFAST -d NRC_Users_Guide/A_Cabinet_Fire_in_MCR Cabinet_fire_in_MCR.in
$RUNCFAST -d NRC_Users_Guide/A_Cabinet_Fire_in_MCR Cabinet_fire_in_MCR_no_ventilation.in
$RUNCFAST -d NRC_Users_Guide/B_Cabinet_Fire_in_Switchgear Cabinet_fire_in_switchgear.in
$RUNCFAST -d NRC_Users_Guide/B_Cabinet_Fire_in_Switchgear Initial_fire_only.in
$RUNCFAST -d NRC_Users_Guide/D_MCC_Fire_in_Switchgear MCC_in_switchgear.in
$RUNCFAST -d NRC_Users_Guide/D_MCC_Fire_in_Switchgear MCC_in_switchgear_one_compartment.in
$RUNCFAST -d NRC_Users_Guide/E_Trash_Fire_in_Cable_Spreading_Room Trash_fire_in_cable_spreading_room.in
$RUNCFAST -d NRC_Users_Guide/G_Transient_Fire_in_Corridor "Transient in Corridor.in"

$RUNCFAST -d Radiation radiation_1.in
$RUNCFAST -d Radiation radiation_2.in
$RUNCFAST -d Radiation radiation_3.in
$RUNCFAST -d Radiation radiation_4.in
$RUNCFAST -d Radiation radiation_5.in

$RUNCFAST -d Species Trace_Species_1.in
$RUNCFAST -d Species Trace_Species_2.in
$RUNCFAST -d Species Trace_Species_3.in
$RUNCFAST -d Species gas_tenability.in
$RUNCFAST -d Species heat_tenability.in
$RUNCFAST -d Species methane_flame_simple.in
$RUNCFAST -d Species species_test.in

$RUNCFAST -d Sprinkler sprinkler_1.in

$RUNCFAST -d Target target_1.in
$RUNCFAST -d Target target_2.in

$RUNCFAST -d Thermal_Equilibrium basic_tempequilib.in
$RUNCFAST -d Thermal_Equilibrium basic_tempequilib_window.in
$RUNCFAST -d Thermal_Equilibrium basic_tempequilib_window_elevation.in

$RUNCFAST -d Units units_basic.in
$RUNCFAST -d Units units_fire.in
$RUNCFAST -d Units units_vents_devices.in

$RUNCFAST -d Ventilation Leakage_1.in
$RUNCFAST -d Ventilation Leakage_2.in
$RUNCFAST -d Ventilation VVent_Tests.in
$RUNCFAST -d Ventilation surface_opened_fraction_1.in
$RUNCFAST -d Ventilation ventilation_1.in
$RUNCFAST -d Ventilation ventilation_2.in
$RUNCFAST -d Ventilation ventilation_3.in
$RUNCFAST -d Ventilation ventilation_4.in
