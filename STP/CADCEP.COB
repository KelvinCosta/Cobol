       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCEP.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT REGCEP ASSIGN TO DISK
              ORGANIZATION IS INDEXED
              ACCESS MODE  IS DYNAMIC
              RECORD KEY   IS FCEP
              ALTERNATE RECORD KEY IS FLOGRADOURO WITH DUPLICATES.
       DATA DIVISION.
       FILE SECTION.
       FD REGCEP
           LABEL RECORDS IS STANDARD
           VALUE OF FILE-ID IS "REGCEP.DAT".
       01 CEP-STRUCT.
           02 FCEP PIC 9(08).
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
           02 RCEP PIC 9(08).
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

       01 TXTCEP PIC 9(08).
       01 TXTLOGRADOURO PIC X(35).
       01 TXTBAIRRO PIC X(20).
       01 TXTCIDADE PIC X(20).
       01 TXTUF PIC X(02).
       01 TXTPONTOREFERENCIA PIC X(35).
       01 TXTSTATUS PIC X(01).
       01 TXTOBSERVACAO PIC X(40).
       01 TXTDATA PIC 9(08).

       PROCEDURE DIVISION.

       INICIO.
           OPEN OUTPUT  REGCEP
           CLOSE REGCEP 
           GO TO MENU-SCREEN.
      
       INSERT-CEP-REG.
           DISPLAY "INSIRA CEP (99999.999):-> "
           ACCEPT RCEP 
               
           DISPLAY "INSIRA LOGRADOURO :-> "
           ACCEPT RLOGRADOURO  

           DISPLAY "INSIRA BAIRRO :-> "
           ACCEPT RBAIRRO

           DISPLAY "INSIRA CIDADE :-> "
           ACCEPT RCIDADE 

           DISPLAY "INSIRA UNIDADE FEDERATIVA :-> "
           ACCEPT RUF

           DISPLAY "INSIRA PONTO REFERENCIA :-> "
           ACCEPT RPONTOREFERENCIA 
             
           DISPLAY "CODIGO STATUS"
           DISPLAY "A - ATIVO"
           DISPLAY "I - INATIVO"
           DISPLAY "C - CANCELADO"
           DISPLAY "E - ESPECIFICO"
           DISPLAY "INSIRA STATUS :-> "
           ACCEPT RSTATUS 

           DISPLAY "INSIRA OBSERVACAO (ACEITA EM BRANCO) :-> "
           ACCEPT ROBSERVACAO
           
           DISPLAY 'INSERINDO CEP: '
           DISPLAY CEP-RECORD
             
           MOVE CEP-RECORD TO CEP-STRUCT
           WRITE CEP-STRUCT.

       READ-REGCEP.
           OPEN INPUT REGCEP 
                    DISPLAY 'CEPS CADASTRADOS: '
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ REGCEP 
                          AT END
                             MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             DISPLAY CEP-STRUCT IN REGCEP
                       END-READ 
                    END-PERFORM
                    CLOSE REGCEP.

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
                    OPEN I-O REGCEP 
                    DISPLAY "INSERIR CEPS"
                    PERFORM UNTIL CONTINUAR = 'N' OR 'n'
                       PERFORM INSERT-CEP-REG   
                       DISPLAY "INSERIR MAIS CEPS??(S/N):-> "
                       ACCEPT CONTINUAR
                    END-PERFORM
                    CLOSE REGCEP  
                 WHEN 2
                    DISPLAY 'INSIRA O CEP A SER ALTERADO:'
                    ACCEPT ALTER-CEP 

                    OPEN INPUT REGCEP  
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ REGCEP  
                          AT END   
                             MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             IF RCEP = ALTER-CEP
                                DISPLAY "CEP A SER ALTERADO: "
                                DISPLAY CEP-STRUCT IN REGCEP
                                MOVE "Y" TO EOF-SWITCH
                             END-IF
                    END-PERFORM 
                    CLOSE REGCEP 
                 WHEN 3
                    DISPLAY "EXCLUIR CEP"
                    
                    DISPLAY 'INSIRA O CEP A SER ALTERADO:'
                    ACCEPT ALTER-CEP 

                    OPEN INPUT REGCEP  
                    PERFORM UNTIL EOF-SWITCH = "Y"
                       READ REGCEP  
                          AT END   
                             MOVE "Y" TO EOF-SWITCH
                          NOT AT END
                             IF RCEP = ALTER-CEP
                                DISPLAY CEP-STRUCT IN REGCEP
                                MOVE "Y" TO EOF-SWITCH
                             END-IF
                    END-PERFORM 
                    CLOSE REGCEP 

                 WHEN 4
                    PERFORM READ-REGCEP
              END-EVALUATE
           END-PERFORM.
           STOP RUN.

