      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CECILIA-OLMOS.
        ENVIRONMENT DIVISION.
       CONFIGURATION SECTION. SPECIAL-NAMES. DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENVIP ASSIGN TO "..\clienvip.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is cli-codcli
           ALTERNATE RECORD KEY IS cli-vend WITH DUPLICATES.

           SELECT FACTURAS ASSIGN TO "..\facturas.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is DYNAMIC
           record KEY is fac-llave.

           SELECT ARTICULO ASSIGN TO "..\articulo.dat"
           ORGANIZATION INDEXED
           ACCESS MODE is RANDOM
           record KEY is art-codigo.

           SELECT MOVART ASSIGN TO "..\movart.txt"
           ORGANIZATION LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD  CLIENVIP.
       01  cli-reg.
           03 cli-codcli pic 9(6).
           03 cli-ganancia pic 9(10).
           03 cli-nombre pic x(30).
           03 cli-vend pic 9(3).
       FD  FACTURAS.
       01  fac-reg.
           03 fac-llave.
              05 fac-facturas pic 9(12).
              05 fac-art pic x(8).
           03 fac-precio pic 9(10).
           03 fac-cantid pic 9(4).
       FD  ARTICULO.
       01  art-reg.
           03 art-codigo pic x(8).
           03 art-descrip pic x(30).
           03 art-costo pic 9(10).
       FD  MOVART.
       01  mov-reg.
           03 mov-vend pic 9(3).
           03 mov-cliente pic 9(6).
           03 mov-factura pic 9(12).
       WORKING-STORAGE SECTION.
       01  w-flag-cli pic 9.
           88 fin-cli value 1.
       01  w-flag-fact pic 9.
           88 fin-fact value 1.
       01  w-flag-art pic 9.
           88 fin-art value 1.
       01  w-flag-mov pic 9.
           88 fin-mov value 1.
       01  w-ven-ant pic 9(3).
       01  w-cli-ant pic 9(6).
       01  w-fac-ant pic 9(12).

       01  w-costo-venta pic 9(10).
       01  w-suma-fact pic 9(10).
       01  w-ganancia-cliente pic 9(10).
       01  w-menor pic 9(10).
       01  w-es-cliente-vip pic x value "n".
       01  w-cli-eliminar pic 9(6).
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM 100-INICIO-ACTUALIZAR.
           PERFORM 200-LEER-ARCH-MOVART.
           PERFORM UNTIL fin-mov
               PERFORM 300-INICIO-VENDEDOR
               PERFORM UNTIL fin-mov OR
                             mov-vend is not EQUAL w-ven-ant
                   PERFORM 400-INICIO-CLIENTE
                   PERFORM UNTIL fin-mov OR
                                 mov-vend is not EQUAL w-ven-ant
                                 OR mov-cliente is not EQUAL w-cli-ant
                            PERFORM 500-PROCESO-ACTUALIZAR
                            PERFORM 200-LEER-ARCH-MOVART
                   END-PERFORM
                   PERFORM 800-FIN-CLIENTE
               END-PERFORM
           PERFORM 1000-FIN-VENDEDOR
           END-PERFORM.
           PERFORM 1100-FIN-ACTUALIZAR.
           STOP RUN.

       100-INICIO-ACTUALIZAR.
           PERFORM 120-ABRIR-ARCHIVOS.

       120-ABRIR-ARCHIVOS.
           OPEN INPUT MOVART FACTURAS ARTICULO.
           OPEN I-O CLIENVIP.

       200-LEER-ARCH-MOVART.
           READ MOVART AT END MOVE 1 TO w-flag-mov.

       300-INICIO-VENDEDOR.
           MOVE mov-vend TO w-ven-ant.

       400-INICIO-CLIENTE.
           MOVE mov-cliente TO w-cli-ant.
           move zero to w-ganancia-cliente.

       500-PROCESO-ACTUALIZAR.
           PERFORM 510-BUSCO-NROFAC-EN-ARCHFAC.

       510-BUSCO-NROFAC-EN-ARCHFAC.
           PERFORM 520-ARMO-CLAVE-FACTURA.
           PERFORM 530-POSICIONO-FACTURA.

       520-ARMO-CLAVE-FACTURA.
           MOVE mov-factura TO fac-facturas.
           MOVE ZERO TO fac-art.

       530-POSICIONO-FACTURA.
           START FACTURAS KEY IS > fac-llave
               INVALID KEY
                   DISPLAY "NO ENCONTRE FACTURA"
                   MOVE ZERO TO w-flag-fact
                   NOT INVALID KEY
                   PERFORM 540-PROCESO-FACTURA.

       540-PROCESO-FACTURA.
           IF w-flag-fact=1 then
               PERFORM 550-LEER-ARCH-FACTURA.
               PERFORM 560-INICIO-FACTURA
               PERFORM UNTIL fin-fact
               or mov-factura is not EQUAL fac-facturas
               OR fac-facturas IS NOT EQUAL w-fac-ant
                   PERFORM 600-BUSCO-ART-EN-ARCHART
                   PERFORM 550-LEER-ARCH-FACTURA
               END-PERFORM.
           PERFORM 620-FIN-FACTURA.

       550-LEER-ARCH-FACTURA.
           READ FACTURAS NEXT AT END MOVE 1 TO w-flag-fact.

       560-INICIO-FACTURA.
           MOVE fac-facturas TO w-fac-ant.
           move zero to w-costo-venta.
           move ZERO to w-suma-fact.

       600-BUSCO-ART-EN-ARCHART.
           MOVE fac-art TO art-codigo.
           READ ARTICULO INVALID KEY
               MOVE ZERO TO w-flag-art
               NOT INVALID KEY
                   MOVE 1 TO w-flag-art
                   PERFORM 610-PROCESO-ARTICULO.

       610-PROCESO-ARTICULO.
           COMPUTE w-costo-venta=fac-precio-(fac-cantid*art-costo).
           COMPUTE w-suma-fact=w-suma-fact+w-costo-venta.

       620-FIN-FACTURA.
           add w-suma-fact to w-ganancia-cliente.

       800-FIN-CLIENTE.
           DISPLAY w-ganancia-cliente.
           PERFORM 900-PROCESO-CLIENVIP.

       900-PROCESO-CLIENVIP.
           MOVE w-ven-ant TO cli-vend
           START CLIENVIP KEY IS = cli-vend INVALID KEY
                       DISPLAY "NO ENCONTRE EL VENDEDOR"
                       NOT INVALID KEY
                       PERFORM 910-ACTUALIZO-CLIENVIP.

       910-ACTUALIZO-CLIENVIP.
           PERFORM 930-LEER-ARCH-CLIENVIP.
           PERFORM 920-INICIO-CLIENVIP.
           PERFORM UNTIL fin-cli OR w-ven-ant NOT = cli-vend
               IF cli-ganancia < w-menor
                   COMPUTE w-menor=cli-ganancia
                   COMPUTE w-cli-eliminar=cli-codcli
               END-IF
               PERFORM 930-LEER-ARCH-CLIENVIP
           END-PERFORM.
           PERFORM 940-FIN-CLIENVIP.

       930-LEER-ARCH-CLIENVIP.
           READ CLIENVIP NEXT AT END MOVE 1 TO w-flag-cli.

       920-INICIO-CLIENVIP.
           MOVE 999999 TO  w-menor.

       940-FIN-CLIENVIP.
           IF w-ganancia-cliente >w-menor
              PERFORM 950-BUSCO-CLIENTEVIP
           END-IF.

       950-BUSCO-CLIENTEVIP.
           MOVE w-cli-ant TO cli-codcli.
           READ CLIENVIP INVALID KEY
                      PERFORM 960-ELIMINO-CLIENTE-MENOR
                      PERFORM 970-INGRESO-CLIENTEVIP
                      NOT INVALID KEY
                      PERFORM 980-ACTUALIZO-GANANCIA.

       960-ELIMINO-CLIENTE-MENOR.
           DISPLAY w-cli-eliminar," cliente menor se elimina".
           move w-cli-eliminar to cli-codcli.
           DELETE CLIENVIP.

       970-INGRESO-CLIENTEVIP.
           DISPLAY w-cli-ant," cliente para ingresar".
           MOVE w-cli-ant TO cli-codcli.
           MOVE w-ganancia-cliente TO cli-ganancia.
           MOVE "----CLIENTE VIP NUEVO--------" TO cli-nombre.
           move w-ven-ant to cli-vend.
           WRITE cli-reg.

       980-ACTUALIZO-GANANCIA.
           DISPLAY cli-codcli," cliente acualizado".
           MOVE w-ganancia-cliente TO cli-ganancia.
           REWRITE cli-reg.

       1000-FIN-VENDEDOR.

       1100-FIN-ACTUALIZAR.
           PERFORM 1200-CERRAR-ARCHIVOS.

       1200-CERRAR-ARCHIVOS.
           CLOSE MOVART FACTURAS ARTICULO CLIENVIP.
       END PROGRAM CECILIA-OLMOS.
