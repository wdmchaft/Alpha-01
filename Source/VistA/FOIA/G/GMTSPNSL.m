GMTSPNSL ; SLC/KER - Progress Note - Selected          ; 08/27/2002
 ;;2.7;Health Summary;**12,30,35,45,56**;Oct 20, 1995
 ;                          
 ; External References
 ;    DBIA  3062  MAIN^TIULAPIS
 ;    DBIA  2056  $$GET1^DIQ (file #8925.1)
 ;                     
MAIN ; Control branching
 N ADATE,ASUB,ATDATE,ATTYPE,ATYPE,CHILD,COSGEDBY,CURIEN,GMTSA,GMTSAI
 N GMTSAII,GMTSCNT,GMTSD,GMTSDIC,GMTSEXSG,GMTSI,GMTSIEN,GMTSII,GMTSIQ
 N GMTSJ,GMTSK,GMTSODIC,GMTSPDIC,GMTSTDIC,GMTSPR,GMTSREC,GMTST,GMTSX
 N GMTSXTRA,GMTDOC,I,PARIEN,PDATE,PN,PSUB,PTYPE,REASON,SIGNEDBY,TYPE
 N TIUDOC,TIUSTAT,GMTSTIUC,X,Y K ^TMP("TIU",$J) S GMTSX=1,GMTSI=0,GMTSTIUC="P"
 F  S GMTSI=$O(GMTSEG(+GMTSEGN,8925.1,GMTSI)) Q:+GMTSI'>0  D
 . S TIUDOC(GMTSI)=+$G(GMTSEG(+GMTSEGN,8925.1,GMTSI))
 . S GMTDOC(GMTSI)=$$GET1^DIQ(8925.1,(+(TIUDOC(GMTSI))_","),.01)
 . S:$L($G(GMTDOC(GMTSI))) GMTDOC("B",GMTDOC(GMTSI),GMTSI)=""
 D MAIN^TIULAPIS(DFN,.TIUDOC,"ALL",GMTS1,GMTS2,GMTSNDM,GMTSX)
 Q:'$D(^TMP("TIU",$J))  D SNOTE^GMTSPN K ^TMP("TIU",$J),PN Q
