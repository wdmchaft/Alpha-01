GMRYED0 ;HIRMFO/YH-INTAKE, OUTPUT AND IV ENTRY POINTS ;5/2/96
 ;;4.0;Intake/Output;;Apr 25, 1997
EN1 ;ENTER/EDIT IV AND IV MAINTENANCE
 S GMRYOPT="LIST^GMRYED3" D EDIT
 Q
EN2 ;ENTER/EDIT OUTPUT
 S GMRYOPT="OUTPUT^GMRYED1" D EDIT
 Q
EN3 ;ENTER/EDIT INPUT
 S GMRYOPT="INPUT^GMRYED1" D EDIT
 Q
EDIT K ^TMP("GMRPT",$J) S (GMRVIDT,GDR,GMROUT)=0,GSITE="" D MASPT^GMRYRP5 G:GMROUT QUIT
 G:'$D(^TMP("GMRPT",$J)) QUIT
 I $G(GMRVHLOC)>0 S GMRHLOC=+$G(GMRVHLOC)
 E  I $G(GMRWARD)>0 S GMRHLOC=+$G(^DIC(42,+GMRWARD,44))
 I +$G(GMRHLOC)=0 D HOSP^GMRYRP5 S GMRHLOC=+$G(GMRVHLOC)
 I '$D(^SC(GMRHLOC)) W !,GMRWARD(1)_" NOT DEFINED IN HOSPITAL LOCATION FILE",! G QUIT
 S GMRRM="" F  S GMRRM=$O(^TMP("GMRPT",$J,GMRRM)) Q:GMRRM=""!GMROUT  S GMRNM="" F  S GMRNM=$O(^TMP("GMRPT",$J,GMRRM,GMRNM)) Q:GMRNM=""!GMROUT  S DFN=0 F  S DFN=$O(^TMP("GMRPT",$J,GMRRM,GMRNM,DFN)) Q:DFN'>0!GMROUT  S GMRNAM=^(DFN) D
 . I "P"[GMREDB S DA=$$SEARCH^GMRYUT12(DFN) D:DA>0 @GMRYOPT Q
 . S GMROUT(1)=0 D ASKOK Q:GMROUT  I 'GMROUT(1) S DA=$$SEARCH^GMRYUT12(DFN) D:DA>0 @GMRYOPT S GMROUT=0 W !
 . Q
QUIT ; 
 K DFN,GPORT,^TMP("GMRPT",$J),GMREDB,GMRHLOC,GMRI,GMRLEN,GMRNAM,GMRNM,GMROUT,GMRRM,GMRVIDT,GMRYOPT,GDR,GMRVHLOC,GMRVWLOC,GSITE,GMRWARD Q
ASKOK ;LOOPING THROUGH PATIENT GLOBAL
 W !,GMRNAM,?$X+10,$S(GMRRM="  BLANK":"    ",1:GMRRM)_"-"_$S(GMRNM="BLANK":"   ",1:GMRNM_"  ") S %=1 D YN^DICN I %=1!(%=-1) S:%=-1 GMROUT=1 Q
 I '% W $C(7),!,?4,"ANSWER 'YES' or 'NO'" G ASKOK
ASL W !!,"Do you wish to stop looping through names?" S %=1 D YN^DICN W ! I %=1!(%=-1) S GMROUT=1 Q
 I '% W $C(7),!,?4,"ANSWER 'YES' or 'NO'" G ASL
 S GMROUT(1)=1
 Q
