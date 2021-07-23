      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEGPERFM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  NUM-DE-TIEMPOS     PIC 9 VALUE 5.

       PROCEDURE DIVISION.
       BEGIN.
           DISPLAY 'Comenzando a ejecutar el programa'
           PERFORM 3 TIMES
              DISPLAY '>>>>Esta en una linea perform'
           END-PERFORM
           DISPLAY 'Finaliza en la linea perform'
           PERFORM FUERA-DE-LINEA NUM-DE-TIEMPOS TIMES
           DISPLAY 'Vuelve a comenzar'
           STOP RUN.

           FUERA-DE-LINEA.
           DISPLAY '>>>> Esta es una interpretacion fuera de linea'.
       END PROGRAM SEGPERFM.
