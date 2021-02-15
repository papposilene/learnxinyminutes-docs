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

      *Déclarons quelques variables.
      *Nous le faisons dans la section WORKING-STORAGE dans la DATA DIVISION.
      *Chaque élément de donnée (aka variable) commence avec un niveau numérique, 
      *puis par le nom de cet élément, suivi par une clause picture 
      *décrivant le type de donnée que la variable contiendra.
      *Presque tout programmeur COBOL utilise l'abbréviation PIC pour PICTURE.
      *A est pour les données alphabétiques, X pour celles alphanumériques et 9 pour celles numériques.
       
      *exemple:
      01  MYNAME PIC xxxxxxxxxx.    *> Une chaîne de 10 caractères.
       
      *Mais compter tous ces x peut mener à des erreurs, 
      *nous écrirons donc le code ci-dessus par le code ci-dessous
      01 MYNAME PIC X(10).
       
      *Voici quelques exemples:
      01  AGE             PIC      9(3).   *> Un nombre jusqu'à 3 chiffres.
      01  LAST_NAME       PIC      X(10).  *> une chaîne jusqu'à dix caractères.
       
      *En COBOL, qu'il y ait un ou plusieurs espace n'a pas d'importance, il est donc
      *commun d'utiliser de nombreuses lignes pour écrire son code et le rendre plus agréable à lire
      01  inyear picture s9(7). *> S indique que le nombre est signé (+ ou -).
                                 *> Les parenthèses indiquent 7 répétitions du 9,
                                 *> ie un nombre à 6 chiffres (et non un tableau).

      *Maintenant, écrivons un peu de COBOL. Ci=dessous un simple Hello World en COBOL.
      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO.
      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 LE-MESSAGE      PIC X(20).
      PROCEDURE DIVISION.
          DISPLAY "DEBUT DU PROGRAMME".
          MOVE "HELLO WORLD" TO LE-MESSAGE.
          DISPLAY LE-MESSAGE.
          STOP RUN.
      
      *Le code ci-dessus aura comme résultat:
      *STARTING PROGRAM
      *HELLO WORLD
      

      
      ********COBOL peut faire des mathématiques***************
      ADD 1 TO AGE GIVING NOUVEL-AGE.
      SUBTRACT 1 FROM COUNT.
      DIVIDE VAR-1 INTO VAR-2 GIVING VAR-3.
      COMPUTE TOTAL-COUNT = COUNT1 PLUS COUNT2.
      
      
      *********PERFORM********************
      *Le mot-clé PERFORM vous permet de sauter à une autre section spécifiée dans le programme
      *and alors de retourner à la prochaine directive une fois la section à laquelle vous avez sauté
      *est achevée et a rendu son résultat. 
      *Vous devez écrire en entier le mot, PERFORM. Il n'est pas possible d'utiliser une abbréviation.

      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLOCOBOL.

      PROCEDURE DIVISION.
         FIRST-PARA.
             DISPLAY 'CECI EST LE PREMIER PARA'.
         PERFORM THIRD-PARA THRU FOURTH-PARA. *>sauter le second-para et exécuter les 3 et 4e.
         *> Une fois seulement que le 3 et 4e aient été exécutés, ,
         *> retourner au 1er para, et terminer l'exécution jusqu'au STOP RUN.
   
         SECOND-PARA.
             DISPLAY 'CECI EST LE SECOND-PARA'.
         STOP RUN.
   
         THIRD-PARA.
             DISPLAY 'CECI EST LE THIRD-PARA'.
   
         FOURTH-PARA.
             DISPLAY 'CECI EST LE FOURTH-PARA'.
   
   
      *Quand on compile et exécute le programme ci-dessus, nous aurons le résultat suivant : 
          CECI EST LE FIRST-PARA
          CECI EST LE THIRD-PARA
          CECI EST LE FOURTH-PARA
          CECI EST LE SECOND-PARA
          
          
      **********Combiner des variables ensemble en utilisant STRING***********
      
      *Now it is time to learn about two related COBOL verbs: string and unstring.

      *Le verbe STRING est utilisé pour concaténer deux ou plusieurs chaînes.
      *UNSTRING est utilisé pour séparer une chaîne en deux ou plusieurs chaînes
      *Il est important de se souvenir d'utiliser ‘delimited by’ quand
      *vous faites usage de string or unstring. 

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

      *D'abord, nous déclarons toutes nos variables, dont celle que nous utiliserons 
      *pour la commande STRING dans DATA DIVISION.

      *Cette action prend place dans PROCEDURE DIVISION. 
      *Nous commençons avec le mot-clé STRING et termineons avec END-STRING. 
      *Dans l'entre-deux, nous écrirons ce que nous voulons combiner ensemble, dans une plus grande variable. 
      *Ici, nous combinons FIRST-NAME, une espace, et LAST-NAME. 

      *L'élément DELIMITED BY qui suit FIRST-NAME et LAST-NAME
      *dit au programme combien de chaque variable nous voulons conserver. 
      *DELIMITED BY SPACE dit ensuite où commencer, 
      *et conserve le contenu de la variable jusqu'à la première espace. 
      *DELIMITED BY SIZE dit au programme de conserver l'intégralité de la variable. 
      *Avec DELIMITED BY SPACE après FIRST-NAME, la partie GIBBERISH est ignorée. 

      *Pour rendre cela plus clair, changez la ligne 10 du code ci-dessus par :

      STRING FIRST-NAME DELIMITED BY SIZE

      *and relancez leprogramme. Le résultat sera alors de :

      THE FULL NAME IS: BOB GIBBERISH COBB






```

##Ready For More?

* [GnuCOBOL](https://sourceforge.net/projects/open-cobol/)

