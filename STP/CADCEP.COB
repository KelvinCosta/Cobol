       IDENTIFICATION DIVISION.
       PROGRAM-ID. VEHICLES.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CEP-FILE ASSIGN TO 'vehicles.txt'
              ORGANIZATION IS SEQUENTIAL.
       DATA DIVISION.
       FILE SECTION.
       FD CEP-FILE.
       01 CEP-STRUCT.
           02 FCEP PIC X(08).
           02 FLOGRADOURO PIC X(35).
           02 FBAIRRO PIC X(20).
           02 FCIDADE PIC X(20).
           02 FUF PIC X(02).
           02 FPONTOREFERENCIA PIC X(35).
           02 FSTATUS PIC X(01).
           02 FOBSERVACAO PIC X(40).
           02 FDATA PIC 9(08).
       WORKING-STORAGE SECTION.
       01 CEP-RECORD.
           02 RCEP PIC X(08).
           02 RLOGRADOURO PIC X(35).
           02 RBAIRRO PIC X(20).
           02 RCIDADE PIC X(20).
           02 RUF PIC X(02).
           02 RPONTOREFERENCIA PIC X(35).
           02 RSTATUS PIC X(01).
           02 ROBSERVACAO PIC X(40).
           02 RDATA PIC 9(08).

       77 OPCAO PIC 9.
       77 CONTINUAR PIC X VALUE 'S'.
       77 EOF-SWITCH PIC X VALUE 'N'.
       77 ALTER-CEP PIC X(9).

       01 TXTPLACA PIC X(25).
       01 TXTPROPRIETARIO PIC X(30).
       01 TXTMARCA PIC 9(01).
       01 TXTANOFABRICACAO PIC 9(4).
       01 TXTSITUACAO PIC X(1).

       PROCEDURE DIVISION.
       OPEN OUTPUT CEP-FILE
       CLOSE CEP-FILE
       GO TO MENU-SCREEN.

       INSERT-VEHICLE-REG.
           DISPLAY "INSIRA RCEP: "
           ACCEPT RCEP

           DISPLAY "INSIRA RLOGRADOURO: "
           ACCEPT RLOGRADOURO

               DISPLAY "INSIRA RBAIRRO: "
           ACCEPT RBAIRRO

           DISPLAY "INSIRA RCIDADE: "
           ACCEPT RCIDADE

           DISPLAY "INSIRA RUF: "
           ACCEPT RUF

           DISPLAY "INSIRA RPONTOREFERENCIA: "
           ACCEPT RPONTOREFERENCIA

           DISPLAY "INSIRA RSTATUS: "
           ACCEPT RSTATUS

           DISPLAY "INSIRA ROBSERVACAO: "
           ACCEPT ROBSERVACAO

           DISPLAY 'INSERINDO CEP: '
           DISPLAY CEP-RECORD

           MOVE CEP-RECORD TO CEP-STRUCT
           WRITE CEP-STRUCT.

       READ-CEP-REG.
           OPEN INPUT CEP-FILE
           DISPLAY 'CEPS CADASTRADOS: '
           PERFORM UNTIL EOF-SWITCH = "Y"
               READ CEP-FILE INTO CEP-STRUCT
                   AT END MOVE "Y" TO EOF-SWITCH
                   NOT AT END DISPLAY CEP-STRUCT
               END-READ
           END-PERFORM
           CLOSE CEP-FILE.

       MENU-SCREEN.
           DISPLAY "CADASTRO DE CEPS"
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
                    OPEN OUTPUT CEP-FILE
                    DISPLAY "INSERIR CEPS"
                    PERFORM UNTIL CONTINUAR = 'N' OR 'n'
                       PERFORM INSERT-VEHICLE-REG
                       DISPLAY "INSERIR MAIS VEICULOS??(S/N):-> "
                       ACCEPT CONTINUAR
                    END-PERFORM
                    CLOSE CEP-FILE
                 WHEN 2
                    DISPLAY 'INSIRA CEP A SER ALTERADO:'
                    ACCEPT ALTER-CEP
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ CEP-FILE INTO CEP-STRUCT
                          AT END MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             IF RCEP = ALTER-CEP
                                DISPLAY CEP-STRUCT
                                MOVE "Y" TO EOF-SWITCH
                             END-IF
                    END-PERFORM
                 WHEN 3
                    DISPLAY "EXCLUIR VEICULOS"
                 WHEN 4
                    PERFORM READ-CEP-REG
              END-EVALUATE
           END-PERFORM.
           STOP RUN.
