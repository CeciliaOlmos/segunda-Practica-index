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
           SELECT ART ASSIGN TO "..\art.txt"
           ORGANIZATION LINE SEQUENTIAL.
           SELECT ARTICULO ASSIGN TO "..\articulo.dat"
                   ORGANIZATION IS INDEXED
                   ACCESS MODE IS DYNAMIC
                   RECORD KEY IS art-codigo.
       DATA DIVISION.
       FILE SECTION.
       FD  ART.
       01  art-tex-reg.
           03 art-text-cod pic x(8).
           03 art-text-des pic x(30).
           03 art-text-cto pic 9(10).
       FD  ARTICULO.
       01  art-reg.
           03 art-codigo pic x(8).
           03 art-descrip pic x(30).
           03 art-costo pic 9(10).
       WORKING-STORAGE SECTION.
       77  w-flag-art pic 9.
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
           OPEN INPUT ART.
           OPEN OUTPUT ARTICULO.
       200-LEER-ARCH-FACT.
           READ ART AT END MOVE 1 TO w-flag-art.
       300-PROCESO.
           MOVE art-text-cod to art-codigo.
           MOVE art-text-des to art-descrip.
           MOVE art-text-cto to art-costo.
           WRITE art-reg.
           DISPLAY art-reg.
       400-FIN.
           CLOSE ART.
           CLOSE ARTICULO.
       END PROGRAM YOUR-PROGRAM-NAME.
