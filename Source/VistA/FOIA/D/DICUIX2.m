DICUIX2 ;SEA/TOAD,SF/TKW-FileMan: Build index data in DINDEX array (cont). ;11:19 AM  7 Nov 2000
 ;;22.0;VA FileMan;**4,28,67**;Mar 30, 1999
 ;Per VHA Directive 10-93-142, this routine should not be modified.
 ;
COMMON1 ; Put info about data subscripts into DINDEX array
 N DIFR,DIPRT
 S DIFR=$G(DIFROM(DISUB)),DIPRT=$G(DIPART(DISUB))
 I DINDEX(DISUB,"FILE")=DIFILE S DINDEX("FLIST")=DINDEX("FLIST")_"^"_DINDEX(DISUB,"FIELD")
 I DIFLAGS["q" D C3 Q
 S DINDEX(DISUB,"USE")=0 D
 . I DIFROM("IEN") S DINDEX(DISUB,"USE")=1 Q
 . S:$G(DIFROM(DISUB+1))]"" DINDEX(DISUB,"USE")=1 Q
C1 S DINDEX(DISUB,"WAY")=$S(DIFLAGS[4:1,DIWAY=DINDEX("WAY"):1,1:-1)
 I $G(DINDEX("WAY","REVERSE")) S DITO(DISUB)=DIFR,DIFR=""
C2 I DIFLAGS[4 S DINDEX(DISUB,"LENGTH")=DILENGTH
 I DIFLAGS[3 D
 . S DIFR=$E(DIFR,1,DILENGTH)
 . S DIPRT=$E(DIPRT,1,DILENGTH)
 . I $D(DITO(DISUB)) S DITO(DISUB)=$E(DITO(DISUB),1,DILENGTH)
 . Q
C3 I 'DINDEX(DISUB,"FILE")!('DINDEX(DISUB,"FIELD")) S DINODE="",DICODE="DINDEX(DISUB)"
 E  D GET^DICUIX1(DIFILE,DINDEX(DISUB,"FILE"),DINDEX(DISUB,"FIELD"),.DINODE,.DICODE)
 I $G(DIERR) D
 . S DINODE="",DICODE="DINDEX(DISUB)"
 . D BLD^DIALOG(8099,DINDEX) Q
 S DINDEX(DISUB,"GET")="DIVAL="_DICODE
C4 S DITYPE=$P(DINODE,U,2)
 N % S %="F" D  S DINDEX(DISUB,"TYPE")=%
 . Q:DIFLAGS["Q"
 . I DITYPE["P" S %="P" Q
 . I DITYPE["D" S %="D" Q
 . I DITYPE["S" S %="S" Q
 . I DITYPE["V" S %="V" Q
 . I DITYPE["N" S %="N"
 . Q
 Q:DIFLAGS["q"
 I DISUB=1 D
 . S DITEMP=$S($D(DIFILE(DIFILE,"NO B")):DIFILE(DIFILE,"NO B"),1:DIFILE(DIFILE,"O")_"DINDEX")
 . I "VP"[DINDEX(DISUB,"TYPE") D
 . . S DINDEX(1,"NODE")=DINODE Q:DIFLAGS[4
 . . I DIFLAGS'["Q",$$CHKP^DICUIX1(.DIFILE,.DINDEX,+$G(DINUMBER),DIFR_DIPRT,.DISCREEN) D  Q
 . . . D TMPIDX^DICUIX1(1,.DITEMP,.DITEMP2,.DINDEX) Q
 . . S DINDEX("AT")=2 Q
 . Q
 I DISUB>1 D
 . I DIFLAGS[4,"VP"[DINDEX(DISUB,"TYPE") S DINDEX(DISUB,"GET")="DIVAL=$G(DINDEX(DISUB,""EXT""))"
 . I DIFLAGS[3,"VP"[DINDEX(DISUB,"TYPE"),DIFLAGS'["Q",'$D(DINDEX("ROOTCNG")) D TMPIDX^DICUIX1(DISUB,.DITEMP,.DITEMP2,.DINDEX) Q
 . S DITEMP=DITEMP_"DINDEX("_(DISUB-1)_")"
 . Q
 S DINDEX(DISUB,"ROOT")=DITEMP_")",DITEMP=DITEMP_","
 I $D(DITEMP2) D
 . S:DISUB>1 DITEMP2=DITEMP2_"DIX("_(DISUB-1)_")"
 . S DINDEX(DISUB,"IXROOT")=DITEMP2_")",DITEMP2=DITEMP2_","
 . Q
C5 S DINDEX(DISUB,"MORE?")=0
 I +$P(DIPRT,"E")=DIPRT,DITYPE'["D" D
 . I DINDEX(DISUB,"WAY")=-1 S DINDEX(DISUB,"MORE?")=1 Q
 . I +$P(DIFR,"E")=DIFR!(DIFR="") S DINDEX(DISUB,"MORE?")=1
 . Q
C6 I DIPRT]"" D
 . I DIFLAGS[4,"VP"[DINDEX(DISUB,"TYPE") Q:DIFLAGS'["l"  Q:DISUB>1
 . I DITYPE["D",DIFLAGS[3 D  Q
 . . N I S I=$S(DINDEX(DISUB,"WAY")=1:"0000000",1:9999999)
 . . D DAT(.DIFR,DIPRT,I,DINDEX(DISUB,"WAY"),.DIOUT) Q
 . Q:$E(DIFR,1,$L(DIPRT))=DIPRT
 . I DINDEX(DISUB,"WAY")=1 D  Q
 . . I DIFR]](DIPRT_$S(+$P(DIPRT,"E")=DIPRT:" ",1:"")) S DIOUT=1 Q
 . . I +$P(DIPRT,"E")=DIPRT,DIPRT<0 S DIFR=$S(DIPRT[".":$P(DIPRT,".")-1,1:"")  Q
 . . I +$P(DIPRT,"E")=DIPRT,+$P(DIFR,"E")=DIFR,DIFR>DIPRT Q
 . . S DINDEX(DISUB,"USE")=1
 . . S DIFR=DIPRT_$S(+$P(DIPRT,"E")'=DIPRT:"",DIFR]]DIPRT:" ",1:"")
 . . Q
 . I DIFR'="",DIPRT]]DIFR S DIOUT=1 Q
 . I +$P(DIPRT,"E")=DIPRT,DIFR?.1"-"1.N.E Q
 . S DINDEX(DISUB,"USE")=1
 . S DIFR=DIPRT_"{{{{{{{{{{"
 . Q
 S DINDEX(DISUB)=$G(DIFR) I DIFR]"" S DINDEX(DISUB,"FROM")=DIFR
 I DIPRT]"" S DINDEX(DISUB,"PART")=DIPRT
 I $D(DITO(DISUB)) S DINDEX(DISUB,"TO")=DITO(DISUB)
C7 I $G(DIDENT(-5)) D
 . I $D(DINDEX(DISUB,"TRANOUT")) S DINDEX(DISUB,"GETEXT")=DIGET Q
 . N T S T=DITYPE I T'["D",T'["S",T'["P",T'["V",T'["O" Q
 . I DIFLAGS[3,"PV"[DINDEX(DISUB,"TYPE"),(DISUB>1!($D(DINDEX("ROOTCNG",1)))) D
 . . I DINDEX(DISUB,"FILE")'=DIFILE S DIGET=0 Q
 . . S DIGET=2 Q
 . S DINDEX(DISUB,"GETEXT")=DIGET Q
 Q
 ;
COMMON2 ; Put data about IEN subscript into DINDEX array.
 N DIEN S DIEN=DINDEX("#")+1
 S:DINDEX'="#" DINDEX(DIEN,"ROOT")=DITEMP_"DINDEX("_(DIEN-1)_"))"
 I $D(DITEMP2) S DINDEX(DIEN,"IXROOT")=DITEMP2_"DIX("_(DIEN-1)_"))"
 I $G(DINDEX("WAY","REVERSE")),DIFROM("IEN") S DINDEX(DIEN,"TO")=DIFROM("IEN"),DIFROM("IEN")=""
 S DINDEX(DIEN)=DIFROM("IEN")
 I DINDEX(DIEN)=0,DINDEX("WAY")=-1 S DINDEX(DIEN)=""
 I DIFROM("IEN") S DINDEX(DIEN,"FROM")=DIFROM("IEN")
 S DINDEX(DIEN,"WAY")=DINDEX("WAY")
 Q
 ;
DAT(DIFR,DIPRT,DIAPP,DIWAY,DIOUT) ; Process FROM and PART for dates
 N L,P,DIPART S L=$L(DIFR),P=$L(DIPRT),DIPART=DIPRT
 I L<P S DIFR=DIFR_$E(DIPART,L+1,P)
 I $L(DIFR)<7 S DIFR=$E(DIFR_DIAPP,1,7)
 Q:$E(DIFR,1,P)=DIPART
 I P<7 S DIPART=$E(DIPART_DIAPP,1,7)
 I DIWAY=1,DIFR]]DIPART S DIOUT=1 Q
 I DIWAY=-1,DIPART]]DIFR S DIOUT=1 Q
 S $E(DIFR,1,P)=DIPRT
 S DINDEX(DISUB,"USE")=1
 Q
 ;
