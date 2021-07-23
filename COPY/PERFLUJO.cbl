      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PERFORM1.
      * Ilustra c�mo el primer formato de PERFORM
      * puede usarse para cambiar el flujo de control a trav�s de un
      * programa.
      * Utilice la salida de este programa para comprender c�mo
      * funciona este formato de PERFORM.
       PROCEDURE DIVISION.
       BEGIN.
           NIVEL-SUPERIOR.
           DISPLAY 'En nivel superior. Comenzado a ejecutar el programa'
           PERFORM PRIMER-NIVEL-INFERIOR
           DISPLAY 'De nuevo en el nivel superior.'.
           STOP RUN.

           SEGUNDO-NIVEL-INFERIOR.
           DISPLAY '>>>>>>>> Ahora en Segundo nivel inferior.'
           PERFORM TERCER-NIVEL-INFERIOR.
           DISPLAY '>>>>>>>> De nuevo en el Segundo nivel inferior.'.

           PRIMER-NIVEL-INFERIOR.
           DISPLAY '>>>> Ahora en Primer nivel inferior'
           PERFORM SEGUNDO-NIVEL-INFERIOR
           DISPLAY '>>>>De nuevo en Primer nivel inferior'.

           TERCER-NIVEL-INFERIOR.
           DISPLAY '>>>>>>>>>>>> Ahora en Tercer nivel inferior'.

       END PROGRAM PERFORM1.
