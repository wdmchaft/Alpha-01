PXQUTL2 ;ISL/JVS - Utility routine  ;5/1/97  08:46
 ;;1.0;PCE PATIENT CARE ENCOUNTER;**4,29**;Aug 12, 1996
 ;
 ;
 Q
EN1 ;--ENTRY POINT
 S PXQ=PXQFORM
 I FLENUM=9000010,PXQFORM[.01 S PXQ=".01;.22"
 I FLENUM=409.68,PXQFORM[.01 S PXQ=".01;.04;.08"
 D GETS^DIQ(FLENUM,VISIT_",",PXQ,"EN","PXQDATA","PXQDATA")
 D PRINT^PXQUTL1
 K PXQDATA
 Q
DATE ;--DATE RANGE
 W !
 N X,Y,DIR
 I $D(^DISV(DUZ,"PXQUTL2-START")) S DIR("B")=$G(^DISV(DUZ,"PXQUTL2-START"))
 S DIR("A")="Enter Starting Date (eg. T-4) : "
 S DIR(0)="DAO" D ^DIR
 I X["^" Q
 S PXQSTART=$G(Y)
 S ^DISV(DUZ,"PXQUTL2-START")=$G(Y(0))
 I $D(^DISV(DUZ,"PXQUTL2-END")) S DIR("B")=$G(^DISV(DUZ,"PXQUTL2-END"))
 S DIR("A")="Enter Ending Date : "
 S DIR(0)="DAO" D ^DIR
 I X["^" Q
 S PXQEND=$G(Y)
 S ^DISV(DUZ,"PXQUTL2-END")=$G(Y(0))
 I PXQSTART>PXQEND W !,"Starting Date cannot be before Ending date" G DATE
 Q
MENU ;---MENU
 N Y,X,DIR
 S DIR("A")="OPTION"
 S DIR(0)="SOM^D:Dependent Entry Count;EX:Expanded D.E.C.;I:Internal View of Visits;S:Source;V:Visit Ien;E:Encounter Ien;P:Parameter Setups;M:Maximum Global Lengths;PA:Patient/IHS & Location files;U:User's Visit Review;C:Cross-references repair"
 D ^DIR
 I Y="D" N PXQRECI S PXQRECI=0 D DEPEN^PXQMAIN G MENU
 I Y="EX" N PXQRECI S PXQRECI=0 D EXPAND^PXQMAIN G MENU
 I Y="I" N PXQRECI S PXQRECI=0 D INTER^PXQMAIN G MENU
 I Y="S" N PXQRECI S PXQRECI=0 D SOURCE^PXQMAIN G MENU
 I Y="V" N PXQRECI S PXQRECI=0 D ASK^PXQMAIN1 G MENU
 I Y="E" N PXQRECI S PXQRECI=0 D ASKENC^PXQMAIN3 G MENU
 I Y="P" N PXQRECI S PXQRECI=0 D SETUP^PXQMAIN G MENU
 I Y="M" N PXQRECI S PXQRECI=0 D MAX^PXQMAIN G MENU
 I Y="PA" N PXQRECI S PXQRECI=0 D MENU^PXQMAIN2 G MENU
 I Y="U" K DIR,Y,X D REPT^PXQMAIN4 K DIR,X,Y G MENU
 I Y="C" K DIR,Y,X D T^PXQUTL3 K Y G MENU
 E  K DIR Q
