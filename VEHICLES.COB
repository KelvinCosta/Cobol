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
           02 FPLACA PIC X(25).
           02 FPROPRIETARIO PIC X(30).
           02 FMARCA PIC 9(01).
           02 FFANOFABRICACAO PIC 9(4).
           02 FSITUACAO PIC X(1).
       WORKING-STORAGE SECTION.
       01 VEHICLE-RECORD.
           02 PLACA PIC X(25).
           02 PROPRIETARIO PIC X(30).
           02 MARCA PIC 9(01).
           02 ANOFABRICACAO PIC 9(4).
           02 SITUACAO PIC X(1).
       77 OPCAO PIC 9.
       77 CONTINUAR PIC X VALUE 'S'.
       
       PROCEDURE DIVISION.
           OPEN OUTPUT VEHICLES-FILE
           DISPLAY "CADASTRO DE VEICULOS"
           PERFORM UNTIL OPCAO = '5'

              DISPLAY "OPCOES"
              DISPLAY "1 - INSERIR"
              DISPLAY "2 - ALTERAR"
              DISPLAY "3 - EXCLUIR"
              DISPLAY "4 - CONSULTAR"
              DISPLAY "5 - SAIR"
              DISPLAY "INSIRA OPCAO"
              ACCEPT OPCAO
              
              EVALUATE OPCAO
                 WHEN 1
                    DISPLAY "INSERIR VEICULOS"
                    PERFORM UNTIL CONTINUAR = 'N' OR 'n'
                       
                       DISPLAY "INSIRA PLACA (XXX9999):-> "
                       ACCEPT PLACA 
               
                       DISPLAY "INSIRA PROPRIETARIO :-> "
                       ACCEPT PROPRIETARIO 
                         
                       DISPLAY "CODIGO MARCAS"
                       DISPLAY "1 - FORD"
                       DISPLAY "2 - GM"
                       DISPLAY "3 - FIAT"
                       DISPLAY "4 - VW"
                       DISPLAY "INSIRA CODIGO MARCA :-> "
                       ACCEPT MARCA 
              
                       DISPLAY "INSIRA ANO FABRICACAO (XXXX) :-> "
                       ACCEPT ANOFABRICACAO
              
                       DISPLAY "CODIGO SITUACAO"
                       DISPLAY "M - MANUTENCAO"
                       DISPLAY "B - BATIDO"
                       DISPLAY "S - SUCATA"
                       DISPLAY "O - ORIGINAL"
                       DISPLAY "N - NORMAL"
                       DISPLAY "INSIRA CODIGO SITUACAO :-> "
                       ACCEPT SITUACAO 
              
                       DISPLAY 'INSERINDO VEICULO: '
                       DISPLAY VEHICLE-RECORD
                      
                       MOVE VEHICLE-RECORD TO VEHICLE-STRUCT
                       WRITE VEHICLE-STRUCT
  
                       DISPLAY "INSERIR MAIS VEICULOS??(S/N):-> \"
                       ACCEPT CONTINUAR

                    END-PERFORM
                 WHEN 2
                    DISPLAY "ALTERAR VEICULOS" 
                 WHEN 3
                    DISPLAY "EXCLUIR VEICULOS"
                 WHEN 4
                    DISPLAY "CONSULTAR VEICULOS"
              END-EVALUATE
           END-PERFORM.
           CLOSE VEHICLES-FILE.
           STOP RUN.

