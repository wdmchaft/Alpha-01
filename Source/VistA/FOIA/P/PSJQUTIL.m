PSJQUTIL ;BIR/RMS-UTILITY FOR PATIENTS ON SPECIFIC DRUGS ; 7/6/09 2:19pm
 ;;5.0; INPATIENT MEDICATIONS ;**214**;16 DEC 97;Build 8
 ;
 ;Reference to ^DIC(42 is supported by DBIA# 1377
 ;
CNTDIV()   ;COUNT THE NUMBER OF DIVISIONS PRESENT FOR WARD LOCATIONS
 N WARD,DIV,DIVCT,DIVARR
 S WARD=0 F  S WARD=$O(^DIC(42,WARD)) Q:'+WARD  D
 . S DIV=$P($G(^DIC(42,WARD,0)),U,11)
 . I +DIV S DIVARR(DIV)=""
 S (DIV,DIVCT)=0 F  S DIV=$O(DIVARR(DIV)) Q:'+DIV  D
 . S DIVCT=DIVCT+1
 Q DIVCT
