---
language: COBOL
contributors:
    - ["Hyphz", "http://github.com/hyphz/"]
filename: learn.COB
---
COBOL est un langage orienté business, ayant connu de nombreuses révisions depuis sa versio originale en 1960. Il est toujours utilisé dans 80% des grandes organisations (banque, services publics, etc.).

```cobol
      *COBOL. Coding like it's 1985. 
      *Compiles with GnuCOBOL in OpenCobolIDE 4.7.6.
       
      *COBOL connait de nombreuses différences entre la version d'origine (COBOl-85)
      *et les versions plus récentes (COBOL-2002 et COBOL-2014.
      *La version d'origine requiert que les colonnes 1-6 soient vides (pour leur utilisation 
      *comme index sur les cartes perforées d'origine...)
      *Un '*' sur la colonne 7 indique un commentaire.
      *Dans le COBOL original, un ne peut être que sur une seule ligne.
      *Le COBOL récent ne requiert plus de colonnes fixes et utilise *> pour 
      *les commentaires, qui peuvent maintenant être placés au milieu d'une ligne.
      *Le COBOL d'origine impose également une longueur avec une limite maximale.
      *Les mots-clés doivent être en lettres capitales dans le COBOL d'origine, 
      *mais sont insensibles à la casse dans les versions récentes.
      *Bien que les versions récentes autorise des caractères à la casse mixte, 
      *il est toujours d'usage d'écrire en lettres capitales quand il s'agit de COBOL.
      *Les déclarations, en COBOL, se terminent toujours par un point.
      
      *Le code COBOL est divisé en quatre parties distinctes, à savoir :
      *IDENTIFICATION DIVISION.
      *ENVIRONMENT DIVISION.
      *DATA DIVISION.
      *PROCEDURE DIVISION.

      *Premièrement, il faut donner un ID au programme.
      *La division Identification peut également inclure d'autres valeurs,
      *mais elles ne constitueront que des commentaires. Program-id est le seul paramètre obligatoire.
       IDENTIFICATION DIVISION.
           PROGRAM-ID.    LEARN.
           AUTHOR.        JOHN DOE.
           DATE-WRITTEN.  06/01/2021.

      *Let's declare some variables.
      *We do this in the WORKING-STORAGE section within the DATA DIVISION.
      *Each data item (aka variable) with start with a level number, 
      *then the name of the item, followed by a picture clause 
      *describing the type of data that the variable will contain.
      *Almost every COBOL programmer will abbreviate PICTURE as PIC.
      *A is for alphabetic, X is for alphanumeric, and 9 is for numeric.
       
      *exemple:
      01  MYNAME PIC xxxxxxxxxx.    *> A 10 character string.
       
      *But counting all those x's can lead to errors, 
      *so the above code can, and should
      *be re-written as:
      01 MYNAME PIC X(10).
       
      *Voici quelques exemples:
      01  AGE             PIC      9(3).   *> A number up to 3 digits.
      01  LAST_NAME       PIC      X(10).  *> A string up to 10 characters.
       
      *In COBOL, multiple spaces are the same as a single space, so it is common
      *to use multiple spaces to line up your code so that it is easier for other
      *coders to read.
      01  inyear picture s9(7). *> S makes number signed.
                                 *> Brackets indicate 7 repeats of 9,
                                 *> ie a 6 digit number (not an array).

      *Now let's write some code. Here is a simple, Hello World program.
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 THE-MESSAGE      PIC X(20).
      PROCEDURE DIVISION.
          DISPLAY "STARTING PROGRAM".
          MOVE "HELLO WORLD" TO THE-MESSAGE.
          DISPLAY THE-MESSAGE.
          STOP RUN.
      
      *Le code ci-dessus aura comme résultat:
      *STARTING PROGRAM
      *HELLO WORLD
      

      
      ********COBOL can perform math***************
      ADD 1 TO AGE GIVING NEW-AGE.
      SUBTRACT 1 FROM COUNT.
      DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
      COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.
      
      
      *********PERFORM********************
      *The PERFORM keyword allows you to jump to another specified section of the code,
      *and then to return to the next executable
      *statement once the specified section of code is completed. 
      *You must write the full word, PERFORM, you cannot abbreviate it.

      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLOCOBOL.

      PROCEDURE DIVISION.
         FIRST-PARA.
             DISPLAY 'THIS IS IN FIRST-PARA'.
         PERFORM THIRD-PARA THRU FOURTH-PARA. *>skip second-para and perfrom 3rd & 4th
         *> then after performing third and fourth,
         *> return here and continue the program until STOP RUN.
   
         SECOND-PARA.
             DISPLAY 'THIS IS IN SECOND-PARA'.
         STOP RUN.
   
         THIRD-PARA.
             DISPLAY 'THIS IS IN THIRD-PARA'.
   
         FOURTH-PARA.
             DISPLAY 'THIS IS IN FOURTH-PARA'.
   
   
      *Quand on compile et exécute le programme ci-dessus, nous aurons le résultat suivant : 
          THIS IS IN FIRST-PARA
          THIS IS IN THIRD-PARA
          THIS IS IN FOURTH-PARA
          THIS IS IN SECOND-PARA
          
          
      **********Combining variables together using STRING ***********
      
      *Now it is time to learn about two related COBOL verbs: string and unstring.

      *The string verb is used to concatenate, or put together, two or more stings.
      *Unstring is used, not surprisingly, to separate a         
      *string into two or more smaller strings. 
      *It is important that you remember to use ‘delimited by’ when you
      *are using string or unstring in your program. 

      IDENTIFICATION DIVISION.
      PROGRAM-ID. LEARNING.
      ENVIRONMENT DIVISION.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 FULL-NAME PIC X(20).
      01 FIRST-NAME PIC X(13) VALUE "BOB GIBBERISH".
      01 LAST-NAME PIC X(5) VALUE "COBB".
      PROCEDURE DIVISION.
          STRING FIRST-NAME DELIMITED BY SPACE
            " "
            LAST-NAME DELIMITED BY SIZE
            INTO FULL-NAME
          END-STRING.
          DISPLAY "THE FULL NAME IS: "FULL-NAME.
      STOP RUN.


      *Le code ci-dessous aura comme résultat:
      THE FULL NAME IS: BOB COBB


      *Regardons ensemble pourquoi...

      *First, we declared all of our variables, including the one that we are creating
      *by the string command, in the DATA DIVISION.

      *The action takes place down in the PROCEDURE DIVISION. 
      *We start with the STRING keyword and end with END-STRING. In between we         
      *list what we want to combine together into the larger, master variable. 
      *Here, we are combining FIRST-NAME, a space, and LAST-NAME. 

      *The DELIMITED BY phrase that follows FIRST-NAME and 
      *LAST-NAME tells the program how much of each variable we want to capture. 
      *DELIMITED BY SPACE tells the program to start at the beginning, 
      *and capture the variable until it runs into a space. 
      *DELIMITED BY SIZE tells the program to capture the full size of the variable. 
      *Since we have DELIMITED BY SPACE after FIRST-NAME, the GIBBERISH part is ignored. 

      *To make this clearer, change line 10 in the above code to:

      STRING FIRST-NAME DELIMITED BY SIZE

      *and then re-run the program. This time the output is:

      THE FULL NAME IS: BOB GIBBERISH COBB






```

##Ready For More?

* [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

