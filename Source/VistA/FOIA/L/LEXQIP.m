LEXQIP ;ISL/KER - Query - ICD Procedure - Extract ;01/03/2011
 ;;2.0;LEXICON UTILITY;**62,73**;Sep 23, 1996;Build 10
 ;               
 ; Global Variables
 ;    ^ICD0(              ICR   4485
 ;    ^TMP("LEXQIP")      SACC 2.3.2.5.1
 ;    ^TMP("LEXQIPA")     SACC 2.3.2.5.1
 ;    ^TMP("LEXQIPO")     SACC 2.3.2.5.1
 ;               
 ; External References
 ;    GETS^DIQ            ICR   2056
 ;    HIST^ICDAPIU        ICR   3991
 ;    $$ICDD^ICDCODE      ICR   3990
 ;    $$ICDOP^ICDCODE     ICR   3990
 ;    $$DT^XLFDT          ICR  10103
 ;    $$UP^XLFSTR         ICR  10104
 ;               
EN ; Main Entry Point
 N LEXENV S LEXENV=$$EV^LEXQM Q:+LEXENV'>0
 N LEXAD,LEXEDT,LEXCDT,LEXEXIT,LEXTEST S LEXEXIT=0,LEXCDT="" K ^TMP("LEXQIP",$J),^TMP("LEXQIPO",$J),^TMP("LEXQIPA",$J)
 F  S LEXCDT=$$AD^LEXQM,LEXAD=LEXCDT Q:'$L(LEXCDT)  S LEXEDT=$P(LEXCDT,"^",1),LEXCDT=$P(LEXCDT,"^",2) Q:LEXCDT'?7N  D LOOK Q:LEXCDT'?7N  Q:+LEXEXIT>0
 K ^TMP("LEXQIP",$J),^TMP("LEXQIPO",$J),^TMP("LEXQIPA",$J)
 Q
LOOK ; ICD Lookup Loop
 N LEXGET,LEXDG,LEXST,LEXSD,LEXLD,LEXMOR,LEXWN,LEXCC,LEXMC,LEXICP,LEXICPC S LEXCDT=$G(LEXCDT),LEXEDT=$$ED^LEXQM(LEXCDT) I LEXCDT'?7N S LEXCDT="" Q
 S LEXLEN=62 F  S LEXICP=$$ICP^LEXQIPA S:LEXICP="^^" LEXEXIT=1 Q:LEXICP="^"!(LEXICP="^^")  D  Q:LEXICP="^"!(LEXICP="^^")
 . K LEXGET,LEXST,LEXSD,LEXLD,LEXWN,LEXCC,LEXMOR,LEXMC,^TMP("LEXQIP",$J) N LEXIEN,LEXLDT,LEXELDT,LEXINC,LEXINOT,LEXIREQ,LEXINCC
 . S LEXIEN=+($G(LEXICP)),LEXLDT=+($G(LEXCDT)) Q:+LEXIEN'>0  Q:LEXLDT'?7N  S LEXELDT=$$SD^LEXQM(LEXLDT) Q:'$L(LEXELDT)
 . D CSV,EN^LEXQIP3
 Q
CSV ; Code Set Versioning Display
 N LEXEDT,LEXIEN,LEXIENS,LEXLTXT,LEXSO,LEXSTAT,LEXNAM
 S LEXCDT=$G(LEXCDT),LEXEDT=$$ED^LEXQM(LEXCDT) I LEXCDT'?7N S (LEXICP,LEXCDT)="" Q
 S LEXIEN=+($G(LEXICP)),LEXSO=$P($G(LEXICP),"^",2),LEXLTXT=$P($G(LEXICP),"^",3) Q:+LEXIEN'>0  Q:'$L(LEXSO)
 S LEXNAM=$P($$ICDOP^ICDCODE(LEXSO,LEXCDT),"^",5) I '$L(LEXNAM) D
 . S LEXNAM=$O(^ICD0(+LEXIEN,67,"B"," "),-1)
 . S LEXNAM=$O(^ICD0(+LEXIEN,67,"B",+LEXNAM," "),-1)
 . S LEXNAM=$P($G(^ICD0(+LEXIEN,67,+LEXNAM,0)),"^",2)
 Q:'$L($G(LEXNAM))
 ;
 ; Get the "Unversioned" Fields
 ; 
 ;   ICD Code             Field .01
 ;   Sex                  Field 9.5
 ;   Major O.R. Proc      Field 20
 S LEXIENS=LEXIEN_"," D GETS^DIQ(80.1,LEXIENS,".01;9.5;20","IE","LEXGET","LEXMSG")
 S LEXGET(80.1,(+LEXIEN_","),"B")=$G(LEXNAM)
 S LEXTMP=$G(LEXGET(80.1,(+LEXIEN_","),20,"E")) D:$L(LEXTMP) OR^LEXQIP2(LEXTMP,.LEXMOR)
 ;            
 ; Get the "Versioned" Fields
 ;            
 ;   Date/Status          80.166  (66)
 S LEXST=$$EF(+($G(LEXIEN)),+LEXCDT),LEXSTAT=+($P(LEXST,"^",2))
 ;   Procedure Name       80.167  (67)
 D SDS(+($G(LEXIEN)),+LEXCDT,.LEXSD,62,LEXSTAT)
 ;   Description          80.168  (68)
 D LDS(+($G(LEXIEN)),+LEXCDT,.LEXLD,62,LEXSTAT)
 ;   Warning Message
 D WN(+LEXCDT,.LEXWN,62)
 ;   MDC/DRG Groups       80.171  (71)
 D MDCDRG^LEXQIP2(+LEXIEN,+LEXCDT,.LEXDG,LEXLEN)
 Q
 ;      
EF(X,LEXCDT) ; Effective Dates
 N LEX,LEXAD,LEXBRD,LEXBRW,LEXEE,LEXEF,LEXES,LEXFA,LEXH,LEXI,LEXID,LEXIEN,LEXLS,LEXSO,LEXST S LEXIEN=+($G(X)),LEXCDT=+($G(LEXCDT))
 Q:+LEXIEN'>0 "^^"  Q:'$L(^ICD0(+LEXIEN,0)) "^^"  Q:LEXCDT'?7N "^^"  S LEXSO=$P($G(^ICD0(+LEXIEN,0)),"^",1)
 S LEX=$$ICDOP^ICDCODE(LEXSO,LEXCDT) S LEXFA=$$FA(+LEXIEN),(LEXLS,LEXST)=$P(LEX,"^",10),LEXID=$P(LEX,"^",12),LEXAD=$P(LEX,"^",13),LEXBRD=2781001,LEXBRW=""
 S:LEXCDT<LEXBRD&(+LEXFA=LEXBRD) LEXBRW="Warning:  The 'Based on Date' provided precedes the initial Code Set Business Rule date of "_$$SD^LEXQM(LEXBRD)_", the Effective date may be inaccurate."
 S LEXES=$S(+LEXST>0:"Active",1:"Inactive")
 S:+LEXST'>0&(+LEXAD'>0) LEXES="Not Applicable",LEXLS=-1 S:LEXST>0 LEXEF=LEXAD S:LEXST'>0 LEXEF=LEXID
 S:LEXST'>0&(+LEXID'>0) LEXEF=LEXFA S LEXEE=$$SD^LEXQM(LEXEF)
 I LEXST'>0,+LEXID'>0,$L(LEXEE),+LEXEF>LEXCDT S LEXEE="(future activation of "_LEXEE_")",LEXEF=""
 S X=LEXLS_"^"_LEXST_"^"_LEXEF_"^"_LEXES_"^"_LEXEE S:$L(LEXBRW) $P(X,"^",6)=LEXBRW
 Q X
 ; 
SDS(X,LEXVDT,LEX,LEXLEN,LEXSTA) ; Diagnosis (short description)
 ; 
 ; LEX=# of Lines
 ; LEX(0)=External Date of Diagnosis Name
 ; LEX(#)=Diagnosis Name
 ; 
 N LEXD,LEXBRD,LEXBRW,LEXDDT,LEXE,LEXEE,LEXEFF,LEXFA,LEXHIS,LEXI,LEXIA,LEXIEN,LEXL,LEXLAST,LEXLEF,LEXLHI,LEXM,LEXR,LEXSDT,LEXSO,LEXLSD,LEXT
 S LEXIEN=$G(X) Q:+LEXIEN'>0  Q:'$D(^ICD0(+LEXIEN,67))  S LEXVDT=+($G(LEXVDT)) S:LEXVDT'?7N LEXVDT=$$DT^XLFDT S LEXSTA=+($G(LEXSTA))
 S LEXSO=$P($G(^ICD0(+LEXIEN,0)),"^",1),LEXLAST=$$ICDOP^ICDCODE(LEXSO),LEXLSD=$P(LEXLAST,"^",5),LEXBRD=2781001,LEXBRW=""
 S:$D(LEXGET)&($L(LEXLSD)) LEXGET(80.1,(+LEXIEN_","),"B")=LEXLSD
 S LEXLEN=+($G(LEXLEN)) S:+LEXLEN'>0 LEXLEN=62 S LEXFA=$$FA(+LEXIEN),LEXM=""
 S LEXM="" S:+LEXVDT<LEXFA&(LEXFA'=LEXBRD) LEXM="Diagnosis Short Name is not available.  The date provided precedes the initial activation of the code" I $L(LEXM) D  Q
 . K LEX N LEXT,LEXI S LEXT(1)=LEXM D PR^LEXQM(.LEXT,(LEXLEN-7))
 . S LEXI=0 F  S LEXI=$O(LEXT(LEXI)) Q:+LEXI'>0  S LEXT=$G(LEXT(LEXI)) S LEX(LEXI)=LEXT
 . S:$D(LEX(1)) LEX(0)="--/--/----" S LEX=+($O(LEX(" "),-1))
 S LEXM="" S LEXEFF=$O(^ICD0(LEXIEN,67,"B",(LEXVDT+.001)),-1),LEXHIS=$O(^ICD0(LEXIEN,67,"B",+LEXEFF," "),-1),LEXSDT=$P($G(^ICD0(+LEXIEN,67,+LEXHIS,0)),"^",2)
 S LEXLEF=$O(^ICD0(LEXIEN,67,"B",(9999999+.001)),-1),LEXLHI=$O(^ICD0(LEXIEN,67,"B",+LEXLEF," "),-1),LEXDDT=$P($G(^ICD0(+LEXIEN,67,+LEXLHI,0)),"^",2)
 S (LEXD,LEXE,LEXR)="" S:$L(LEXSDT)&(LEXEFF?7N) LEXD=LEXSDT,LEXE=LEXEFF
 S:$L(LEXDDT)&(LEXLEF?7N)&('$L(LEXD))&('$L(LEXE)) LEXD=LEXDDT,LEXE=LEXLEF,LEXR="No Text Available for Date Provided"
 K LEX S LEX(1)=LEXD S:$L(LEXD) LEXGET(80.1,(+LEXIEN_","),"B")=LEXD
 S LEXEE=$$SD^LEXQM(LEXE) S:$D(LEXTEST)&(+LEXSTA'>0) LEXEE="--/--/----" S:$L(LEX(1)) LEX(0)=LEXEE
 S LEX=+($O(LEX(" "),-1))
 Q
LDS(X,LEXVDT,LEX,LEXLEN,LEXSTA) ; Long Description
 ; 
 ; LEX=# of Lines
 ; LEX(0)=External Date of Description
 ; LEX(#)=Description
 ; 
 N LEXC,LEXBRD,LEXBRW,LEXDDT,LEXEVDT,LEXFA,LEXI,LEXIEN,LEXL,LEXLN,LEXM,LEXT,LEXSO,LEXTL,LEXTMP S LEXIEN=$G(X) Q:+LEXIEN'>0  Q:'$D(^ICD0(+LEXIEN,68))
 S LEXVDT=+($G(LEXVDT)) S:LEXVDT'?7N LEXVDT=$$DT^XLFDT S LEXEVDT=$$SD^LEXQM(LEXVDT),LEXLEN=+($G(LEXLEN)) S:+LEXLEN'>0 LEXLEN=62
 S LEXSO=$P($G(^ICD0(+LEXIEN,0)),"^",1) S LEXFA=$$FA(+LEXIEN),LEXM="" S LEXSTA=+($G(LEXSTA)),LEXBRD=2781001,LEXBRW=""
 S LEXM="" S:+LEXVDT<LEXFA&(LEXFA'=LEXBRD) LEXM="Description is not available.  The date provided precedes the initial activation of the code" I $L(LEXM) D  Q
 . K LEX N LEXT,LEXI S LEXT(1)=LEXM D PR^LEXQM(.LEXT,(LEXLEN-7)) S LEXI=0 F  S LEXI=$O(LEXT(LEXI)) Q:+LEXI'>0  S LEXT=$G(LEXT(LEXI)) S LEX(LEXI)=LEXT
 . S:$D(LEX(1)) LEX(0)="--/--/----" S LEX=+($O(LEX(" "),-1))
 K LEXTMP S LEXTL=$$ICDD^ICDCODE(LEXSO,"LEXTMP",LEXVDT) S LEXL=+($O(LEXTMP(" "),-1)),LEXLN=$G(LEXTMP(+LEXL))
 S LEXM="" K:LEXL>0&(LEXLN["CODE TEXT MAY BE INACCURATE") LEXTMP(+LEXL)
 F LEXI=1:1:2 S LEXL=+($O(LEXTMP(" "),-1)),LEXLN=$$TM^LEXQM($G(LEXTMP(+LEXL))) K:LEXL>0&('$L(LEXLN)) LEXTMP(+LEXL)
 S LEXDDT=$O(^ICD0(+LEXIEN,68,"B",(LEXVDT+.999999)),-1) S:LEXDDT'?7N LEXDDT=$O(^ICD0(+LEXIEN,68,"B",0)) S:LEXDDT?7N LEXEVDT=$$SD^LEXQM(LEXDDT)
 D PR^LEXQM(.LEXTMP,LEXLEN) K LEX F LEXI=1:1:13 D
 . Q:'$D(LEXTMP(LEXI))  S LEXT=$$TM^LEXQM($G(LEXTMP(LEXI))),LEX(LEXI)=$$UP^XLFSTR(LEXT)
 I $L(LEXM) D
 . N LEXT,LEXI,LEXL,LEXC S LEXL=+($O(LEX(" "),-1)),LEXC=0 S LEXT(1)=LEXM D PR^LEXQM(.LEXT,(LEXLEN-7))
 . S LEXI=0 F  S LEXI=$O(LEXT(LEXI)) Q:+LEXI'>0  D
 . . S LEXT=$G(LEXT(LEXI)) S:$L(LEXT) LEXC=LEXC+1 S LEXL=LEXL+1,LEX(LEXL)=LEXT
 S:$D(LEXTEST)&(+LEXSTA'>0) LEXEVDT="--/--/----" S:$D(LEX(1)) LEX(0)=LEXEVDT S LEX=+($O(LEX(" "),-1))
 Q
WN(X,LEX,LEXLEN) ; Warning
 ;            
 ; LEX=# of Lines
 ; LEX(0)=External Date
 ; LEX(#)=Warning
 ;            
 N LEXVDT,LEXREF,LEXIA,LEXTMP K LEX S LEXVDT=$G(X) Q:LEXVDT'?7N  S LEXIA=$$IA(LEXVDT) Q:+LEXIA'>0  S LEXLEN=+$G(LEXLEN) S:+LEXLEN>62 LEXLEN=62
 S LEXTMP(1)="Warning:  The 'Based on Date' provided precedes Code Set Versioning.  The Operation/Procedure (Short Name) and Description may be inaccurate for "_$$SD^LEXQM(LEXVDT)
 D PR^LEXQM(.LEXTMP,LEXLEN) K LEX S LEXI=0 F  S LEXI=$O(LEXTMP(LEXI)) Q:+LEXI'>0  S LEX(LEXI)=$G(LEXTMP(LEXI))
 S LEX=$O(LEX(" "),-1),LEX(0)=$$SD^LEXQM(LEXVDT)
 Q
 ; Miscellaneous
FA(X) ;   First Activation
 N LEXFA,LEXH,LEXI,LEXIEN,LEXSO
 S LEXIEN=+($G(X)) S X="",LEXSO=$P($G(^ICD0(+LEXIEN,0)),"^",1) D HIST^ICDAPIU(LEXSO,.LEXH) S LEXFA="",LEXI=0
 F  S LEXI=$O(LEXH(LEXI)) Q:+LEXI'>0!($L(LEXFA))  S:+($G(LEXH(LEXI)))>0&(LEXI?7N) LEXFA=LEXI Q:$L(LEXFA)
 S X=LEXFA
 Q X
IA(X) ;   Inaccurate
 N LEXBRD,LEXVDT,LEXSYS S LEXVDT=+($G(X)),LEXSYS=1,LEXVDT=$S($G(LEXVDT)="":$$DT^XLFDT,1:$$DBR(LEXVDT)),LEXBRD=3021001,X=$S(LEXVDT<LEXBRD:1,1:0)
 Q X
DBR(X) ;   Date Business Rules
 N LEXVDT S LEXVDT=$G(X) Q:'$G(LEXVDT)!($P(LEXVDT,".")'?7N) $$DT^XLFDT
 S:LEXVDT#10000=0 LEXVDT=LEXVDT+101 S:LEXVDT#100=0 LEXVDT=LEXVDT+1 S X=$S(LEXVDT<2781001:2781001,1:LEXVDT)
 Q X
