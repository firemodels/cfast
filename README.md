# cfast_sockets


Modifying door opening while executing CFAST using sockets.

The application is used to modify the door opening during a CFAST fire simulation (https://github.com/firemodels/cfast). We use it in the AAMKS fire risk assessment software (https://github.com/aamks/aamks), where we conduct parallel simulations of fire development and evacuation. Moving people (evacuating from the building) pass through the door, open and close it, which affects the development of fire in the building. CFAST does not have the functionality to change the door opening during program execution, so we added such an option using inter-process communication, socket communication - the AAMKS process communicates with the CFAST process to control the opening or closing of the door in the CFAST simulation. In this repository, for easy representation of the communication mechanism CFAST communicates with the process created by the main.py program in Python.


## Requirements

To modify the opening level of a door using a socket while a program is running in CFAST, we must make some assumptions:

1. Doors defined in cfast.in file (&VENT TYPE = 'WALL') should have the CRITERION parameter = 'TIME' set, and the T-List should be set from 0 to the value of (&TIME SIMULATION - &TIME SPREADSHEET) in the &TIME SPREADSHEET step.
For example:

`&HEAD VERSION = 7724, TITLE = 'P_ID_3_S_ID_11' /
&TIME SIMULATION = 100, PRINT = 10, SMOKEVIEW = 10, SPREADSHEET = 10 /
&INIT EXTERIOR_TEMPERATURE = 4.26, INTERIOR_TEMPERATURE = 21.78, PRESSURE = 101580, RELATIVE_HUMIDITY = 36 /`

.
.
.

`!! SECTION DOORS AND HOLES
&VENT TYPE = 'WALL' ID = 'd2' COMP_IDS = 'r1', 'c2' WIDTH = 0.9 TOP = 2.0 BOTTOM = 0.0 OFFSET = 3.39 FACE = 'REAR' CRITERION = 'TIME', T = 0,10,20,30,40,50,60,70,80,90, F=0,0,0,0,0,0,0,0,0,0/
`

The doors opening is changed every step equal to &TIME SPREADSHEET (in our example 10 seconds).

2. Then in main.py program we have to set the value of loop_repetition_number variable to the value of &TIME SIMULATION/&TIME SMOKEVIEW (in our example 100/10 = 10)

### GFortran Linux

Files changed or added that differ from the original CFAST code:

1. cfast4aamks/GFortran Linux/Build/CFAST/gnu_linux_64/main.py  (for testing)
2. cfast4aamks/GFortran Linux/Build/CFAST/gnu_linux_64/cfast.in (cfast.in file for testing)
3. cfast4aamks/GFortran Linux/Build/CFAST/gnu_linux_64/c_socket_handler.c  (c code for fortran socket communication)
4. cfast4aamks/GFortran Linux/Build/CFAST/gnu_linux_64/c_socket_handler.so (complied c socket library)
5. cfast4aamks/GFortran Linux/Build/CFAST/makefile
6. cfast4aamks/GFortran Linux/Source/CFAST/solve.f90
7. cfast4aamks/GFortran Linux/Source/CFAST/cfast_parameters.f90
8. cfast4aamks/GFortran Linux/Source/CFAST/outputspreadsheet.f90

How to use instruction:

1. Compile CFAST using ./make_cfast.sh script in Build/CFAST/gnu_linux_64 folder.

2. All files (main.py, c_socket_handler.so, cfast.in, cfast7_linux_64) must be placed in one folder (for example cfast4aamks/GFortran Linux/Build/CFAST/gnu_linux_64/) because main.py creates file cfast_evac_socket_port.txt in working directory and saves the port number in this file. Then CFAST process looks for cfast_evac_socket_port.txt file in its working directory to get process number.

3. First run the main.py program with the command `python3 main.py`, then run CFAST with the command `./cfast7_linux_64 cfast.in`. In the console of the main.py process enter the degrees of opening of the door in the following format `d2=1,d4=0,d5=0.4,d7=1`. IDs entered in the main.py process console input must match the IDs presented in the cfast.in input file.

Example usage:

![331514539-43df6f3d-907b-4880-b198-3c230b3b9ea9](https://github.com/user-attachments/assets/536fb5ea-866a-4db0-b094-ee13964af956)

### Intel Fortran Compiler Windows

In aamks project we use Intel Fortran Compiler Windows CFAST using Visual Studio only for debugging purposes because quite accurate debugging is possible there (which we haven't been able to do on the gfort compiler so far)

Files changed or added that differ from the original CFAST code:

1. cfast4aamks/Intel Fortran Compiler Windows/Source/CFAST/solve.f90
2. cfast4aamks/Intel Fortran Compiler Windows/Source/CFAST/cfast_parameters.f90
3. cfast4aamks/Intel Fortran Compiler Windows/Build/CFAST/gnu_linux_64/main.py  (for testing)
4. cfast4aamks/Intel Fortran Compiler Windows/Build/CFAST/gnu_linux_64/cfast.in (cfast.in file for testing)

Intel Fortran Windows socket communication was made using:
https://github.com/lassytouton/LittleDemos/tree/master/Intel%20Visual%20FORTRAN%20TCP%20IP%20Sockets%20Based%20Client%20Server

How to use instruction:

1. Run the batch file restore_vs_config.bat from the folder Utilities\Visual_Studio within the cfast repository to create the Visual Studio solution.
2. Open the solution, cfast.sln, in the root of the repository.
3. Set CFAST (IFORT) project as Startup project.
4. Move cfast.in and main.py files to Source\CFAST folder.
5. In Visual Studio right click on CFAST (IFORT) project, click "properties", open "Debugging" option in "Configuration Properties" and write cfast.in in Command Arguments.
6. Make sure the solve.f90 and cfast_parameters.f90 files are changed to cfast4aamks/Intel Fortran Compiler Windows/Source/CFAST/solve.f90 file.
7. Run main.py for example using PyCharm IDE.
8. Run CFAST in Visual Studio (first rebuild CFAST (IFORT) project, then run with or without debuging).
9. In the console of the main.py process, we enter the degrees of opening of the door in the following format: `d2=1,d4=0,d5=0.4,d7=1`. IDs entered in the main.py process console input must match the IDs presented in the cfast.in input file.


This application is operational as of December 17, 2024. Changes to the https://github.com/firemodels/cfast project may require modifications to our repository.
