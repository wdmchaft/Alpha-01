MCARAM0C ;WASH ISC/JKL- MUSE AUTO INSTRUMENT REINIT-NO DIAG ;1/31/95  11:27
 ;;2.3;Medicine;;09/13/1996
 ;
 ;
 ;Called from ^MCARAM0
 ;Deletes EKG records without diagnoses
 S MCIEN=0
 F  S MCIEN=$O(^MCAR(691.5,MCIEN)) Q:MCIEN="B"  I '$D(^MCAR(691.5,MCIEN,9)) D DEL
 Q
 ;
DEL ;
 S MCCNT=MCCNT+1
 S DA=MCIEN,DIK="^MCAR(691.5," D ^DIK
 W:MCCNT#100=0 "."
 Q
