      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. YOUR-PROGRAM-NAME.
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FACT ASSIGN TO "..\fact.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT FACTURAS ASSIGN TO "..\facturas.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS fac-llave.
       DATA DIVISION.
       FILE SECTION.
       FD  FACTURAS.
       01  fac-reg.
           03 fac-llave.
              05 fac-facturas pic 9(12).
              05 fac-art pic x(8).
           03 fac-precio pic 9(10).
           03 fac-cantid pic 9(4).
       FD  FACT.
       01  fac-tex-reg.
           03 fac-tex-fac pic 9(12).
           03 fac-tex-art pic x(8).
           03 fac-tex-prec pic 9(10).
           03 fac-tex-cant pic 9(4).
       WORKING-STORAGE SECTION.
       77  w-flag-fac pic 9.
           88 fin-archivo value 1.
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO.
           PERFORM 200-LEER-ARCH-FACT.
           PERFORM UNTIL fin-archivo
            PERFORM 300-PROCESO
            PERFORM 200-LEER-ARCH-FACT
           END-PERFORM.
           PERFORM 400-FIN.
            STOP RUN.
       100-INICIO.
           OPEN INPUT FACT.
           OPEN OUTPUT FACTURAS.
       200-LEER-ARCH-FACT.
           READ FACT AT END MOVE 1 TO w-flag-fac.
       300-PROCESO.
           MOVE fac-tex-fac to fac-facturas.
           MOVE fac-tex-art to fac-art.
           MOVE fac-tex-prec to fac-precio.
           MOVE fac-tex-cant to fac-cantid.
           WRITE fac-reg.
           DISPLAY fac-reg.
       400-FIN.
           CLOSE FACT.
           CLOSE FACTURAS.
       END PROGRAM YOUR-PROGRAM-NAME.
