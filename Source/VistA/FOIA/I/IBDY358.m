IBDY358 ;ALB/JEH - PRE INSTALL FOR PATCH IBD*3*58 ; 10-AUG-04
 ;;3.0;AUTOMATED INFO COLLECTION SYS;**58**;APR 24, 1997
 ;
 ;This routine will loop through the SELECTION file looking for incorrect Vitamin B12 dosage and correct
EN N ITEM,CNT,SUBCOL
 S ITEM=0
 S CNT=0
 W !,"STARTING CORRECTION OF Vitamin B12 INJECTION DOSAGE TO 1000mcg"
 F  S ITEM=$O(^IBE(357.3,ITEM)) Q:'ITEM  D
 . S CNT=CNT+1
 . S SUBCOL=0
 . F  S SUBCOL=$O(^IBE(357.3,ITEM,1,SUBCOL)) Q:'SUBCOL  D
 . . I $P(^IBE(357.3,ITEM,1,SUBCOL,0),"^",2)["Vitamin B12, per 1000mg" D
 . . . S $P(^IBE(357.3,ITEM,1,SUBCOL,0),"^",2)="Vitamin B12, per 1000mcg"
 . . . W !,"ITEM NUMBER "_ITEM_" IN SELECTION FILE MODIFIED"
 . . Q
 W !,CNT_" SELECTION FILE ENTRIES READ"
 S ITEM=0
 S CNT=0
 F  S ITEM=$O(^IBE(357.1,ITEM)) Q:'ITEM  D
 . S CNT=CNT+1
 . S SUBCOL=0
 . F  S SUBCOL=$O(^IBE(357.1,ITEM,"S",SUBCOL)) Q:'SUBCOL  D
 . . ;W !,ITEM_" "_SUBCOL
 . . I $P(^IBE(357.1,ITEM,"S",SUBCOL,0),"^",5)["J3420 Vitamin B12, per 1000mg" D
 . . . S $P(^IBE(357.1,ITEM,"S",SUBCOL,0),"^",5)="    J3420 Vitamin B12,per 1000mcg"
 . . . W !,"ITEM NUMBER "_ITEM_" "_SUBCOL_" ENCOUNTER FORM BLOCK MODIFIED"
 . . Q
 W !,CNT_" ENCOUNTER FORM BLOCK ENTRIES READ"
        W !,"UPDATE OF Vitamin B12 INJECTION DOSAGE COMPLETE"
 Q
 ;
