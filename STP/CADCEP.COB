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

       77  OPCAO PIC 9.
       77  CONTINUAR PIC X VALUE 'S'.
       77  EOF-SWITCH PIC X VALUE 'N'.
       77  ALTER-CEP PIC X(9).
       77  WS-WAIT PIC X.
       77  LN PIC 9.

       01 TXTPLACA PIC X(25).
       01 TXTPROPRIETARIO PIC X(30).
       01 TXTMARCA PIC 9(01).
       01 TXTANOFABRICACAO PIC 9(4).
       01 TXTSITUACAO PIC X(1).


       SCREEN SECTION.
       01  DATA-ENTRY-SCREEN.
           05 CEP-SECTION.
              10 VALUE "CADASTRO CEP" BLANK SCREEN LINE 1 COL 35.
              10 VALUE "CEP:" LINE 3 COL 3.
              10 CEP-INPUT LINE 3 COL 35 PIC X(08) TO RCEP.
           05 LOGRADOURO-SECTION.
              10 VALUE "INSIRA LOGRADOURO:" LINE 5 COL 3.
              10 LOGRADOURO-INPUT LINE 5 COL 35
                 PIC X(35) TO RLOGRADOURO.
           05 BAIRRO-SECTION.
              10 VALUE "INSIRA BAIRRO:" LINE 7 COL 3.
              10 BAIRRO-INPUT LINE 7 COL 35
                 PIC X(35) TO RBAIRRO.
           05 CIDADE-SECTION.
              10 VALUE "INSIRA CIDADE:" LINE 9 COL 3.
              10 CIDADE-INPUT LINE 9 COL 35
                 PIC X(35) TO RCIDADE.
           05 UF-SECTION.
              10 VALUE "INSIRA UF:" LINE 11 COL 3.
              10 UF-INPUT LINE 11 COL 35
                 PIC X(35) TO RUF.
           05 PR-SECTION.
              10 VALUE "INSIRA PONTO REFERENCIA:" LINE 13 COL 3.
              10 PR-INPUT LINE 13 COL 35
                 PIC X(35) TO RPONTOREFERENCIA.
           05 STATUS-SECTION.
              10 VALUE "INSIRA STATUS:" LINE 15 COL 3.
              10 STATUS-INPUT LINE 15 COL 35
                 PIC X(35) TO RSTATUS.
           05 OBS-SECTION.
              10 VALUE "INSIRA OBSERVACAO:" LINE 17 COL 3.
              10 OBS-INPUT LINE 17 COL 35
                 PIC X(35) TO ROBSERVACAO.

       01  DATA-SHOW-SCREEN.
           05 CEP-SHOW-SECTION.
              10 VALUE "CEP:" BLANK SCREEN LINE LN COL 3.
              10 CEP-OUTPUT LINE LN COL 35 PIC X(08) FROM FCEP.
           05 LOGRADOURO-SHOW-SECTION.
              10 VALUE "LOGRADOURO:" LINE LN COL 3.
              10 LOGRADOURO-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FLOGRADOURO.
           05 BAIRRO-SHOW-SECTION.
              10 VALUE "BAIRRO:" LINE LN COL 3.
              10 BAIRRO-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FBAIRRO.
           05 CIDADE-SHOW-SECTION.
              10 VALUE "CIDADE:" LINE LN COL 3.
              10 CIDADE-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FCIDADE.
           05 UF-SHOW-SECTION.
              10 VALUE "UF:" LINE LN COL 3.
              10 UF-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FUF.
           05 PR-SHOW-SECTION.
              10 VALUE "PONTO REFERENCIA:" LINE LN COL 3.
              10 PR-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FPONTOREFERENCIA.
           05 STATUS-SHOW-SECTION.
              10 VALUE "STATUS:" LINE LN COL 3.
              10 STATUS-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FSTATUS.
           05 OBS-SHOW-SECTION.
              10 VALUE "OBSERVACAO:" LINE LN COL 3.
              10 OBS-OUTPUT LINE LN COL 35
                 PIC X(35) FROM FOBSERVACAO.

       01  MENU-SCREEN.
           05 VALUE "CADASTRO DE CEPS" BLANK SCREEN LINE 1 COL 35.
           05 VALUE "OPCOES" LINE 3 COL 3.
           05 VALUE "1 - INSERIR" LINE 4 COL 5.
           05 VALUE "2 - ALTERAR" LINE 5 COL 5.
           05 VALUE "3 - EXCLUIR" LINE 6 COL 5.
           05 VALUE "4 - CONSULTAR" LINE 7 COL 5.
           05 VALUE "5 - SAIR" LINE 8 COL 5.
           05 VALUE "INSIRA OPCAO" LINE 10 COL 3.
           05 OPCAO-INPUT LINE 10 COL 35  PIC 9(1) TO OPCAO.

       PROCEDURE DIVISION.
           OPEN OUTPUT CEP-FILE
           CLOSE CEP-FILE
           GO TO INICIO.

       INSERT-CEP-REG.
           DISPLAY CEP-SECTION
           ACCEPT CEP-INPUT

           DISPLAY LOGRADOURO-SECTION
           ACCEPT LOGRADOURO-INPUT

           DISPLAY BAIRRO-SECTION
           ACCEPT BAIRRO-INPUT

           DISPLAY CIDADE-SECTION
           ACCEPT CIDADE-INPUT

           DISPLAY UF-SECTION
           ACCEPT UF-INPUT

           DISPLAY PR-SECTION
           ACCEPT PR-INPUT

           DISPLAY STATUS-SECTION
           ACCEPT STATUS-INPUT

           DISPLAY OBS-SECTION
           ACCEPT OBS-INPUT

           DISPLAY 'INSERINDO CEP: '
           DISPLAY CEP-RECORD

           MOVE CEP-RECORD TO CEP-STRUCT
           WRITE CEP-STRUCT.

       READ-CEP-REG.
           MOVE 3 TO LN
           OPEN INPUT CEP-FILE
           DISPLAY 'CEPS CADASTRADOS: '
           PERFORM UNTIL EOF-SWITCH = "Y"
               READ CEP-FILE INTO CEP-STRUCT
                   AT END MOVE "Y" TO EOF-SWITCH
                   NOT AT END
                       DISPLAY CEP-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY LOGRADOURO-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY BAIRRO-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY CIDADE-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY UF-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY PR-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY STATUS-SHOW-SECTION
                       ADD 1 TO LN
                       DISPLAY OBS-SHOW-SECTION
                       ADD 2 TO LN
               END-READ
           END-PERFORM
           CLOSE CEP-FILE.

       INICIO.

           DISPLAY MENU-SCREEN
           ACCEPT OPCAO-INPUT

           EVALUATE OPCAO
               WHEN 1
                  OPEN OUTPUT CEP-FILE
                  DISPLAY "INSERIR CEPS"
                  PERFORM UNTIL CONTINUAR = 'N' OR 'n'
                     PERFORM INSERT-CEP-REG
                     DISPLAY "INSERIR MAIS CEPS??(S/N):-> "
                     ACCEPT CONTINUAR
                  END-PERFORM
                  CLOSE CEP-FILE
                  GO TO INICIO
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
                  GO TO INICIO
               WHEN 3
                  DISPLAY "EXCLUIR VEICULOS"
                  GO TO INICIO
               WHEN 4
                  PERFORM READ-CEP-REG
                  ACCEPT WS-WAIT
                  GO TO INICIO
           END-EVALUATE.
           STOP RUN.
