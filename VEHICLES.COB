       IDENTIFICATION DIVISION.
       PROGRAM-ID. VEHICLES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT VEHICLES-FILE ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS FPLACA
              ALTERNATE RECORD KEY IS FPROPRIETARIO WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD VEHICLES-FILE
           LABEL RECORDS IS STANDARD
           VALUE OF FILE-ID IS "VEHICLES.DAT".
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
       77 EOF-SWITCH PIC X VALUE 'N'.
       77 ALTER-VEHICLE PIC X(9).

       01 TXTPLACA PIC X(25).
       01 TXTPROPRIETARIO PIC X(30).
       01 TXTMARCA PIC 9(01).
       01 TXTANOFABRICACAO PIC 9(4).
       01 TXTSITUACAO PIC X(1).

       PROCEDURE DIVISION.

       INICIO.
           OPEN OUTPUT  VEHICLES-FILE
           CLOSE VEHICLES-FILE
           GO TO MENU-SCREEN.

       INSERT-VEHICLE-REG.
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
           WRITE VEHICLE-STRUCT.

       READ-VEHICLE-REG.
           OPEN INPUT VEHICLES-FILE
                    DISPLAY 'VEICULOS CADASTRADOS: '
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ VEHICLES-FILE
                          AT END
                             MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             DISPLAY VEHICLE-STRUCT IN VEHICLES-FILE
                       END-READ
                    END-PERFORM
                    CLOSE VEHICLES-FILE.

       MENU-SCREEN.
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
                    OPEN I-O VEHICLES-FILE
                    DISPLAY "INSERIR VEICULOS"
                    PERFORM UNTIL CONTINUAR = 'N' OR 'n'
                       PERFORM INSERT-VEHICLE-REG
                       DISPLAY "INSERIR MAIS VEICULOS??(S/N):-> "
                       ACCEPT CONTINUAR
                    END-PERFORM
                    CLOSE VEHICLES-FILE
                 WHEN 2
                    DISPLAY 'INSIRA PLACA DO CARRO A SER ALTERADO:'
                    ACCEPT ALTER-VEHICLE

                    OPEN INPUT VEHICLES-FILE
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ VEHICLES-FILE
                          AT END
                             MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             IF PLACA = ALTER-VEHICLE
                                DISPLAY VEHICLE-STRUCT IN VEHICLES-FILE
                                MOVE "Y" TO EOF-SWITCH
                             END-IF
                    END-PERFORM
                    CLOSE VEHICLES-FILE
                 WHEN 3
                    DISPLAY "EXCLUIR VEICULOS"
                 WHEN 4
                    PERFORM READ-VEHICLE-REG
              END-EVALUATE
           END-PERFORM.
           STOP RUN.
