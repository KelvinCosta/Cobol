       IDENTIFICATION DIVISION.
       PROGRAM-ID. VEHICLES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VEHICLES-FILE ASSIGN TO 'vehicles.txt'
           ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD VEHICLES-FILE.
       01 VEHICLE-STRUCT.
           02 FMODELO PIC X(25).
           02 FDETALHES.
              03 FPLACA PIC X(7).
              03 FESTADO PIC A(2).
       WORKING-STORAGE SECTION.
       01 VEHICLE-RECORD.
           02 MODELO PIC X(25).
           02 DETALHES.
              03 PLACA PIC X(7).
              03 ESTADO PIC A(2).
       77 CONTINUAR PIC X VALUE 'S'.
       PROCEDURE DIVISION.
           DISPLAY "CADASTRO DE VEICULOS"
           OPEN OUTPUT VEHICLES-FILE
           PERFORM UNTIL CONTINUAR = 'N' OR 'n'
              DISPLAY "INSIRA MODELO :-> "
              ACCEPT MODELO 
   
              DISPLAY "INSIRA PLACA :-> "
              ACCEPT PLACA 
   
              DISPLAY "INSIRA SIGLA ESTADO :-> "
              ACCEPT ESTADO 

              DISPLAY 'INSERINDO VEICULO: '
              DISPLAY VEHICLE-RECORD
              
              MOVE VEHICLE-RECORD TO VEHICLE-STRUCT
              WRITE VEHICLE-STRUCT
   
              DISPLAY "INSERIR MAIS VEICULOS??(S/N):-> \"
              ACCEPT CONTINUAR

           END-PERFORM.
           CLOSE VEHICLES-FILE.
           STOP RUN.

